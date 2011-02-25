#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "parser.h"
#include "clang.tab.h"

#define QUILT	"quilt"

typedef int walkfn(node_t *, void *);
static void walk_tree(node_t *tree, walkfn *fn, void *data);

static void
walk_tree_rec(node_t *tree, walkfn *fn, void *data)
{
	node_t *item = tree;
	if (item->seen)
		return;
	do {
		if (fn(item, data))
			break;
		int i;
		for (i = 0; i < item->nchild; ++i)
			if (item->child[i])
				walk_tree_rec(item->child[i], fn, data);
		item->seen = 1;
		item = list_entry(item->list.next, node_t, list);
	} while (item != tree);
}

static void
reset_seen(node_t *tree)
{
	node_t *item = tree;
	do {
		item->seen = 0;
		int i;
		for (i = 0; i < item->nchild; ++i)
			if (item->child[i])
				reset_seen(item->child[i]);

		item = list_entry(item->list.next, node_t, list);
	} while (item != tree);
}

static void
walk_tree(node_t *tree, walkfn *fn, void *data)
{
	if (!tree)
		return;
	reset_seen(tree);
	walk_tree_rec(tree, fn, data);
}

/************************************************************
 * Interface to quilt
 *
 */

static int
list_count(struct list_head *list)
{
	struct list_head *iter;
	int ret = 0;
	list_for_each(iter, list) {
		++ret;
	}
	return ret;
}

static int
writeout(const char *name, struct list_head *rawlist)
{
	FILE *f;
	int res;
	if (! (f = fopen(name, "w")) ) {
		fprintf(stderr, "Cannot open %s for writing: %s\n",
			name, strerror(errno));
		return -1;
	}
	if ( (res = dump_contents(rawlist, f)) ) {
		fprintf(stderr, "Write to %s failed: %s\n",
			name, strerror(errno));
		fclose(f);
		return res;
	}
	if (fclose(f)) {
		fprintf(stderr, "Cannot close %s: %s\n",
			name, strerror(errno));
	}

	return 0;
}

static int
writeout_files(struct list_head *filelist)
{
	struct parsed_file *pf;
	int res;
	list_for_each_entry(pf, filelist, list)
		if ( (res = writeout(pf->name, &pf->raw)) )
			return res;
	return 0;
}

static int
run_command(const char *name, const char *const argv[])
{
	pid_t pid;
	if ( !(pid = vfork()) ) {
		execvp(name, (char *const*) argv);
		_exit(-1);
	} else if (pid > 0){
		int status;
		if (waitpid(pid, &status, 0) == -1) {
			perror("Cannot get child status");
			return -1;
		}

		if (WIFEXITED(status))
			return WEXITSTATUS(status);
		else
			return -1;
	} else {
		perror("Cannot fork");
		return -1;
	}
}

static const char *quilt_refresh_argv[] =
{ QUILT, "refresh", "-p", "ab", "--no-timestamp", NULL };

static int
quilt_new(const char *name, struct list_head *filelist)
{
	int n = list_count(filelist);
	const char **argv = calloc(sizeof(char*), n + 4);
	struct parsed_file *pf;
	int i, res;

	i = 0;
	argv[i++] = QUILT;
	argv[i++] = "new";
	argv[i++] = name;
	argv[i] = NULL;
	if ( (res = run_command(QUILT, argv)) )
		return res;

	i = 0;
	argv[i++] = QUILT;
	argv[i++] = "add";
	list_for_each_entry(pf, filelist, list)
		argv[i++] = pf->name;
	argv[i] = NULL;
	if ( (res = run_command(QUILT, argv)) )
		return res;

	if ( (res = writeout_files(filelist)) )
		return res;

	return run_command(QUILT, quilt_refresh_argv);
}

static const char *basedir;

static int
quilt_import(const char *name)
{
	static const char *quilt_push_argv[] =
		{ QUILT, "push", NULL };
	char *path = malloc(strlen(basedir) + strlen(name) + 1);
	const char *quilt_import_argv[] =
		{ QUILT, "import", path, NULL };
	int res;

	strcpy(stpcpy(path, basedir), name);

	if ( (res = run_command(QUILT, quilt_import_argv)) )
		return res;

	if ( (res = run_command(QUILT, quilt_push_argv)) )
		return res;
	
	return run_command(QUILT, quilt_refresh_argv);
}

/************************************************************
 * Useful helper functions
 *
 */

static void
replace_text(node_t *node, const char *text)
{
	struct dynstr *ds = newdynstr(text, strlen(text));
	replace_text_list(node->first_text, node->last_text, ds, ds);

	node->first_text = node->last_text = ds;
}

/* Remove text nodes from @remove up to, but not including @keep. */
static void
remove_text_list(struct dynstr *remove, struct dynstr *keep)
{
	struct list_head *it, *next;

	it = &remove->list;
	while (it != &keep->list) {
		next = it->next;
		list_del(it);
		free(list_entry(it, struct dynstr, list)); 
		it = next;
	}
}

/* Remove text nodes from @remove backwards up to, but not including @keep. */
static void
remove_text_list_rev(struct dynstr *remove, struct dynstr *keep)
{
	struct list_head *it, *next;

	it = &remove->list;
	while (it != &keep->list) {
		next = it->prev;
		list_del(it);
		free(list_entry(it, struct dynstr, list)); 
		it = next;
	}
}

/* Returns non-zero if @node is an ID */
static int
is_id(node_t *node)
{
	return node->type == nt_expr && node->e.op == ID;
}

/* Returns non-zero if @node is a direct call to a function
 * with the ID @name.
 */
static int
is_direct_call(node_t *node, const char *name)
{
	if (node->type != nt_expr || node->e.op != FUNC)
		return 0;

	node_t *fn = node->child[che_arg1];
	return is_id(fn) && !strcmp(fn->e.str, name);
}

/* Get the @pos-th element from @list */
static node_t *
nth_element(node_t *list, int pos)
{
	node_t *elem = list;
	int i;
	for (i = 1; i < pos; ++i) {
		elem = list_entry(elem->list.next, node_t, list);
		if (elem == list)
			return NULL;
	}
	return elem;
}

/************************************************************
 * Use target types
 *
 */

/* Convert a basic type into its target equivallent */
static void
btype_to_target(node_t *item)
{
	static const struct {
		unsigned long old;
		char *new;
	} subst[] = {
		{ TYPE_UNSIGNED|TYPE_LONG|TYPE_LONGLONG, "tulonglong" },
		{ TYPE_LONG|TYPE_LONGLONG|TYPE_INT, "tlonglong" },
		{ TYPE_LONG|TYPE_LONGLONG, "tlonglong" },
		{ TYPE_UNSIGNED|TYPE_LONG, "tulong" },
		{ TYPE_LONG, "tlong" },
		{ TYPE_UNSIGNED|TYPE_INT, "tuint" },
		{ TYPE_INT, "tint" },
		{ TYPE_UNSIGNED|TYPE_SHORT|TYPE_INT, "tushort" },
		{ TYPE_UNSIGNED|TYPE_SHORT, "tushort" },
		{ TYPE_SHORT, "tshort" },
	};
	int i;

	for (i = 0; i < sizeof(subst)/sizeof(subst[0]); ++i) {
		if (item->t.btype == subst[i].old) {
			replace_text(item, subst[i].new);
			item->t.category = type_typedef;
			item->t.name = subst[i].new;
			break;
		}
	}
}

/* Convert a typedef into its target equivallent */
static void
typedef_to_target(node_t *item)
{
	static const struct {
		const char *old;
		char *new;
	} subst[] = {
		{ "ushort", "tushort" },
		{ "uint", "tuint" },
		{ "ulong", "tulong" },
		{ "longlong", "tlonglong" },
		{ "ulonglong", "tulonglong" },
	};
	int i;

	for (i = 0; i < sizeof(subst)/sizeof(subst[0]); ++i) {
		if (!strcmp(item->t.name, subst[i].old)) {
			replace_text(item, subst[i].new);
			item->t.name = subst[i].new;
			break;
		}
	}
}

/* Replace types with target types */
static int target_types(node_t *item, void *data)
{
	/* Convert types to their target equivallents */
	if (item->type == nt_type) {
		if (item->t.category == type_basic)
			btype_to_target(item);
		else if (item->t.category == type_typedef)
			typedef_to_target(item);
	}

	return 0;
}

/************************************************************
 * Translate calls to mkstring()
 *
 */

static int
mkstring_typecast(node_t *node, void *data)
{
	const char **typecast = data;

	if (!is_id(node))
		return 0;

	if (!strcmp(node->e.str, "LONG_DEC") ||
	    !strcmp(node->e.str, "LONG_HEX")) {
		*typecast = "ulong";
		return 1;
	} else if (!strcmp(node->e.str, "INT_DEC") ||
		   !strcmp(node->e.str, "INT_HEX")) {
		*typecast = "uint";
		return 1;
	} else if (!strcmp(node->e.str, "LONGLONG_HEX")) {
		*typecast = "ulonglong";
		return 1;
	} else
		return 0;
}

static int
mkstring_variadic(node_t *node, void *data)
{
	if (!is_direct_call(node, "mkstring"))
		return 0;

	/* Get the right typecast if necessary */
	const char *typecast = NULL;
	node_t *flags = nth_element(node->child[che_arg2], 3);
	walk_tree(flags, mkstring_typecast, &typecast);

	/* Remove MKSTRING if necessary */
	node_t *opt = nth_element(node->child[che_arg2], 4);
	if (is_direct_call(opt, "MKSTR")) {
		remove_text_list(opt->first_text,
				 opt->child[che_arg2]->first_text);
		remove_text_list_rev(opt->last_text,
				     opt->child[che_arg2]->last_text);
		list_add(&opt->child[che_arg2]->list, &opt->list);
		list_del(&opt->list);
		free(opt->child[che_arg1]);
		node_t *oldopt = opt;
		opt = opt->child[che_arg2];
		free(oldopt);
	}

	/* Ensure correct typecast if necessary */
	if (typecast) {
		YYLTYPE loc;
		struct dynstr *typestr = newdynstr(typecast, strlen(typecast));
		struct dynstr *lparen = newdynstr("(", 1);
		struct dynstr *rparen = newdynstr(")", 1);
		list_add_tail(&lparen->list, &opt->first_text->list);
		list_add(&typestr->list, &lparen->list);
		list_add(&rparen->list, &typestr->list);

		loc.first_text = loc.last_text = typestr;
		node_t *type = newtype_name(&loc, typecast);
		type->t.category = type_typedef;
		loc.first_text = lparen;
		loc.last_text = opt->last_text;
		node_t *cast = newexpr2(&loc, TYPECAST, type, opt);
		list_add(&cast->list, &opt->list);
		list_del_init(&opt->list);
	}

	return 0;
}

static char *
get_read_fn(node_t *type)
{
	if (type->t.category == type_basic) {
		switch (type->t.btype & ~(TYPE_INT | TYPE_UNSIGNED)) {
		case TYPE_SHORT: return "readshort";
		case 0:		 return "readint";
		case TYPE_LONG:	 return "readlong";
		case TYPE_LONG | TYPE_LONGLONG:	return "readlonglong";
		}
	} else if (type->t.category == type_typedef) {
		if (!strcmp(type->t.name, "ushort"))
			return "readshort";
		else if (!strcmp(type->t.name, "uint"))
			return "readint";
		else if (!strcmp(type->t.name, "ulong"))
			return "readlong";
		else if (!strcmp(type->t.name, "longlong") ||
			 !strcmp(type->t.name, "ulonglong") ||
			 !strcmp(type->t.name, "uint64_t"))
			return "readlonglong";
	} else if (type->t.category == type_pointer)
		return "readptr";
	return NULL;
}

static int
convert_readmem(node_t *node, void *data)
{
	if (!is_direct_call(node, "readmem"))
		return 0;

	node_t *arg = nth_element(node->child[che_arg2], 4);
	if (arg->type != nt_expr) {
		fputs("Huh?! Argument to call not an expression?\n", stderr);
		return 0;
	}

	node_t *mult = NULL;
	node_t *size = arg;
	if (arg->e.op == '*') {
		if (arg->child[che_arg1]->e.op == SIZEOF_TYPE) {
			size = arg->child[che_arg1];
			mult = arg->child[che_arg2];
		} else if (arg->child[che_arg2]->e.op == SIZEOF_TYPE) {
			mult = arg->child[che_arg1];
			size = arg->child[che_arg2];
		} else
			/* not a recognized format */
			return 0;
	} else if (arg->e.op != SIZEOF_TYPE)
		return 0;

	/* Replace the function name */
	char *newfn = get_read_fn(size->child[che_arg1]);
	if (!newfn)
		return 0;
	node->child[che_arg1]->e.str = newfn;
	replace_text(node->child[che_arg1], newfn);

	/* Replace the 4th argument */
	if (mult) {
		remove_text_list(arg->first_text, mult->first_text);
		remove_text_list_rev(arg->last_text, mult->last_text);
		list_add(&mult->list, &arg->list);
		list_del(&arg->list);
	} else {
		replace_text(arg, "1");
		YYLTYPE loc;
		loc.first_text = loc.last_text = arg->first_text;
		node_t *one = newexprnum(&loc, strdup("1"));
		list_add(&one->list, &arg->list);
		list_del(&arg->list);
	}
	return 0;
}

static int import(const char *patchname, struct list_head *flist, void *arg)
{
	return quilt_import(patchname);
}

/* Helper for a simple transformation */
static int simple(const char *patchname, struct list_head *filelist,
		  void *xform_fn)
{
	struct parsed_file *pf;

	list_for_each_entry(pf, filelist, list) {
		walk_tree(pf->parsed, xform_fn, NULL);
	}
	return quilt_new(patchname, filelist);
}

/************************************************************
 * Main entry point for the transformations
 *
 */

typedef int xformfn(const char *, struct list_head *, void *);

struct xform_desc {
	const char *name;
	xformfn *fn;
	void *arg;
};

static struct xform_desc xforms[] = {
/************************************************************
 * Upstreamed patches first...
 */

// Configure GDB_CONF_FLAGS from configure
{ "configure-gdb-conf-flags.patch", import },

{ "remove-VOID_PTR.patch", import },

{ "gdb-does-not-need-syment.patch", import },

// mkstring() fixes
{ "mkstring-optimize.patch", import },

/************************************************************
 * Things that could theoretically go upstream, but are not
 * accepted there
 */

// convert mkstring() to a variadic function
{ "variadic-mkstring.patch", import },

/* convert mkstring() to a variadic function */
{ "variadic-mkstring-use.patch", simple, mkstring_variadic },

/************************************************************
 * Endianity-related issues
 */

// Find and convert calls to readmem
{ "replace-readmem.patch", simple, convert_readmem },

// Add readlong, readint, etc. functions
{ "readtype.patch", import },

// Modify the "facilitator" macros
{ "facilitators.patch", import },

/************************************************************
 * Target types next...
 */

// Introduce target types
{ "target-types.patch", import },

// Use target types
{ "target-types-use.patch", simple, target_types },

// Introduce target timeval
{ "target-timeval.patch", import },

// Use target timeval
// TBD

// Introduce target off_t
{ "target-off_t.patch", import },

// Use target off_t
// TBD

// Provide platform-independent struct pt_regs
{ "arch-pt-regs.patch", import },

// Use platform-independent pt_regs
// TBD

/************************************************************
 * Print formatters
 */

// Introduce target print formatters
{ "target-printf-spec.patch", import },

// Replace all occurences in the source code
// TBD

/************************************************************
 * Build-time changes
 */

// Allow configuring for any target
{ "configure-cross-gdb-conf-flags.patch", import },
{ "configure-any-target.patch", import },

/************************************************************
 * Final manual tweaks
 */
{ "manual.patch", import },

};

static int
run_xform(struct xform_desc *desc, struct list_head *filelist)
{
	return desc->fn(desc->name, filelist, desc->arg);
}

static struct xform_desc *
find_xform(const char *name)
{
	int i;
	for (i = 0; i < sizeof(xforms)/sizeof(xforms[0]); ++i)
		if (!strcmp(xforms[i].name, name))
			return &xforms[i];
	return NULL;
}

int
xform_files(struct arguments *args, struct list_head *filelist)
{
	int ret = 0;

	basedir = args->basedir;
	if (list_empty(&args->xform_names)) {
		int i;
		for (i = 0; i < sizeof(xforms)/sizeof(xforms[0]); ++i)
			if ( (ret = run_xform(&xforms[i], filelist)) )
				break;
	} else {
		struct dynstr *ds;
		list_for_each_entry(ds, &args->xform_names, list) {
			struct xform_desc *desc = find_xform(ds->text);
			if (desc) {
				if ( (ret = run_xform(desc, filelist)) )
					break;
			} else
				fprintf(stderr, "WARNING: "
					"Cannot find transform %s\n",
					ds->text);
		}
	}

	return ret;
}
