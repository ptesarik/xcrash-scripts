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

static int uptodate;

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

	/* All files must be re-read to reflect the change */
	uptodate = 0;

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
}

/* Remove text nodes from @remove up to, but not including @keep. */
static void
remove_text_list(struct dynstr *remove, struct dynstr *keep)
{
	struct list_head *it, *next;
	struct dynstr *prec =
		list_entry(remove->list.prev, struct dynstr, list);

	it = &remove->list;
	while (it != &keep->list) {
		node_t *node, *nnode;
		struct dynstr *ds = list_entry(it, struct dynstr, list);

		list_for_each_entry_safe(node, nnode,
					 &ds->node_first, first_list)
			set_node_first(node, keep);
		list_for_each_entry_safe(node, nnode,
					 &ds->node_last, last_list)
			set_node_last(node, prec);

		next = it->next;
		list_del(it);
		free(ds);
		it = next;
	}
}

/* Remove text nodes from @remove backwards up to, but not including @keep. */
static void
remove_text_list_rev(struct dynstr *remove, struct dynstr *keep)
{
	struct list_head *it, *next;
	struct dynstr *follow =
		list_entry(keep->list.next, struct dynstr, list);

	it = &remove->list;
	while (it != &keep->list) {
		node_t *node, *nnode;
		struct dynstr *ds = list_entry(it, struct dynstr, list);

		list_for_each_entry_safe(node, nnode,
					 &ds->node_first, first_list)
			set_node_first(node, follow);
		list_for_each_entry_safe(node, nnode,
					 &ds->node_last, last_list)
			set_node_last(node, keep);

		next = it->prev;
		list_del(it);
		free(ds);
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

	node_t *fn = first_node(&node->child[che_arg1]);
	return is_id(fn) && !strcmp(fn->e.str, name);
}

/* Get the @pos-th element from @list */
static node_t *
nth_element(struct list_head *list, int pos)
{
	node_t *elem;
	int i = 0;
	list_for_each_entry(elem, list, list) {
		if (++i == pos)
			return elem;
	}
	return NULL;
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

/* Replace sizeof pointers with target pointers */
static int target_ptr(node_t *node, void *data)
{
	if (! (node->type == nt_expr && node->e.op == SIZEOF_TYPE) )
		return 0;

	node_t *arg = nth_element(&node->child[che_arg1], 1);
	if (arg->type == nt_type && arg->t.category == type_pointer) {
		replace_text(arg, "tptr");
		reparse_node(arg, START_TYPE_NAME);
	}

	return 0;
}

/* Replace types with target types */
static int target_off_t(node_t *item, void *data)
{
	/* Convert types to their target equivallents */
	if (item->type == nt_type && item->t.category == type_typedef &&
	    !strcmp(item->t.name, "off_t")) {
		replace_text(item, "toff_t");
		item->t.name = "toff_t";
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
		*typecast = "(ulong)";
		return 1;
	} else if (!strcmp(node->e.str, "INT_DEC") ||
		   !strcmp(node->e.str, "INT_HEX")) {
		*typecast = "(uint)";
		return 1;
	} else if (!strcmp(node->e.str, "LONGLONG_HEX")) {
		*typecast = "(ulonglong)";
		return 1;
	} else
		return 0;
}

static int
mkstring_variadic(node_t *node, void *data)
{
	if (!is_direct_call(node, "mkstring"))
		return 0;

	/* Remove MKSTRING if necessary */
	node_t *opt = nth_element(&node->child[che_arg2], 4);
	if (is_direct_call(opt, "MKSTR")) {
		node_t *arg = nth_element(&opt->child[che_arg2], 1);
		remove_text_list(opt->first_text, arg->first_text);
		remove_text_list_rev(opt->last_text, arg->last_text);
	}

	/* Get the right typecast if necessary */
	const char *typecast = NULL;
	node_t *flags = nth_element(&node->child[che_arg2], 3);
	walk_tree_single(flags, mkstring_typecast, &typecast);
	if (typecast) {
		struct dynstr *ds = newdynstr(typecast, strlen(typecast));
		list_add_tail(&ds->list, &opt->first_text->list);
	}

	reparse_node(node, START_EXPR);
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

	node_t *arg = nth_element(&node->child[che_arg2], 4);
	if (arg->type != nt_expr) {
		fputs("Huh?! Argument to call not an expression?\n", stderr);
		return 0;
	}

	node_t *mult = NULL;
	node_t *size = arg;
	if (arg->e.op == '*') {
		node_t *arg1 = first_node(&arg->child[che_arg1]);
		node_t *arg2 = first_node(&arg->child[che_arg2]);
		if (arg1->e.op == SIZEOF_TYPE) {
			size = arg1;
			mult = arg2;
		} else if (arg2->e.op == SIZEOF_TYPE) {
			mult = arg1;
			size = arg2;
		} else
			/* not a recognized format */
			return 0;
	} else if (arg->e.op != SIZEOF_TYPE)
		return 0;

	/* Replace the function name */
	char *newfn = get_read_fn(first_node(&size->child[che_arg1]));
	if (!newfn)
		return 0;
	replace_text(first_node(&node->child[che_arg1]), newfn);

	/* Replace the 4th argument */
	if (mult) {
		remove_text_list(arg->first_text, mult->first_text);
		remove_text_list_rev(arg->last_text, mult->last_text);
	} else {
		struct dynstr *ds = newdynstr("1", 1);
		replace_text_list(arg->first_text, arg->last_text, ds, ds);
	}

	reparse_node(node, START_EXPR);
	return 0;
}

static int
printf_spec_one(node_t *node, void *data)
{
	if (node->type != nt_expr || node->e.op != STRING_CONST)
		return 0;

	char *start = node->first_text->text;
	char *p = start;
	LIST_HEAD(ds);
	while ( (p = strchr(p, '%')) ) {
		char *spec;
		do {
			++p;
		} while (*p && strchr("-0123456789.*", *p));
		spec = p;
		while (*p == 'l')
			++p;
		if (strchr("dioux", *p)) {
			/* Re-create prefix string */
			size_t len = spec - start;
			char *pfx = calloc(len + 2, sizeof(char));
			memcpy(pfx, start, len);
			pfx[len] = '\"';
			struct dynstr *dspfx = newdynstr(pfx, len + 1);
			list_add_tail(&dspfx->list, &ds);

			/* Create the PRI identifier */
			len = p - spec + 1 + 3;
			char *pri = calloc(len + 1, sizeof(char));
			memcpy(stpcpy(pri, "PRI"), spec, len - 3);
			struct dynstr *dspri = newdynstr(pri, len);
			list_add_tail(&dspri->list, &ds);

			/* Re-open the tail string */
			*p = '\"';
			start = p;
		} else
			++p;
	}

	if (!list_empty(&ds)) {
		if (strcmp(start, "\"\"")) {
			struct dynstr *dslast =
				newdynstr(start, strlen(start));
			list_add_tail(&dslast->list, &ds);
		}
		replace_text_list(node->first_text, node->last_text,
				  list_entry(ds.next, struct dynstr, list),
				  list_entry(ds.prev, struct dynstr, list));
	}

	reparse_node(node, START_EXPR);
	return 0;
}

static int
printf_spec(node_t *node, void *data)
{
	int pos;

	if (is_direct_call(node, "printf"))
		pos = 1;
	else if (is_direct_call(node, "fprintf") ||
		 is_direct_call(node, "sprintf"))
		pos = 2;
	else if (is_direct_call(node, "snprintf") ||
		 is_direct_call(node, "vsnprintf"))
		pos = 3;
	else
		return 0;

	node_t *spec = nth_element(&node->child[che_arg2], pos);
	walk_tree_single(spec, printf_spec_one, NULL);

	return 0;
}

static int
use_ia64_fpreg_t(node_t *node, void *data)
{
	if (! (node->type == nt_type && node->t.category == type_struct &&
	       node->t.name && !strcmp(node->t.name, "ia64_fpreg")) )
		return 0;

	replace_text(node, "ia64_fpreg_t");
	reparse_node(node, START_TYPE_NAME);
	return 0;
}

static int update_parsed_files(struct list_head *filelist)
{
	struct parsed_file *pf;

	if (uptodate)
		return 0;

	init_predef_types();
	list_for_each_entry(pf, filelist, list) {
		int res = parse_file(pf);
		if (res)
			return res;
	}

	uptodate = 1;
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
	int res;

	if ( (res = update_parsed_files(filelist)) )
		return res;

	list_for_each_entry(pf, filelist, list) {
		walk_tree(&pf->parsed, xform_fn, NULL);
	}
	return quilt_new(patchname, filelist);
}

/* Rename a struct */

struct rename_data {
	const char *oldname;
	const char *newname;
	size_t newlen;
};

#define TO	"\0"

static int
rename_struct_fn(node_t *node, void *data)
{
	struct rename_data *rd = data;

	if (node->type == nt_type && node->t.category == type_struct &&
	    node->t.name && !strcmp(node->t.name, rd->oldname)) {
		struct dynstr *oldds = text_dynstr(node->t.name);
		struct dynstr *newds = newdynstr(rd->newname, rd->newlen);
		replace_text_list(oldds, oldds, newds, newds);
		node->t.name = newds->text;
	}
	return 0;
}

static int
rename_struct(const char *patchname, struct list_head *filelist,
	       void *arg)
{
	struct parsed_file *pf;
	struct rename_data rd;
	int res;

	rd.oldname = arg;
	rd.newname = arg + strlen(arg) + 1;
	rd.newlen = strlen(rd.newname);

	if ( (res = update_parsed_files(filelist)) )
		return res;

	list_for_each_entry(pf, filelist, list) {
		walk_tree(&pf->parsed, rename_struct_fn, &rd);
	}
	return quilt_new(patchname, filelist);
}

/* Remove the definition of a named struct */
static int
remove_struct_fn(node_t *node, void *data)
{
	const char *name = data;
	if (node->type != nt_decl)
		return 0;

	node_t *type = nth_element(&node->child[chd_type], 1);
	if (type && type->type == nt_type &&
	    type->t.category == type_struct &&
	    !strcmp(type->t.name, name)) {
		remove_text_list(node->first_text,
				 next_dynstr(node->last_text));
		freenode(node);
	}
	return 0;
}

static int
remove_struct(const char *patchname, struct list_head *filelist,
	      void *arg)
{
	struct parsed_file *pf;
	int res;

	if ( (res = update_parsed_files(filelist)) )
		return res;

	list_for_each_entry(pf, filelist, list) {
		walk_tree(&pf->parsed, remove_struct_fn, arg);
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

// Target pointer types are trickier, but let's change the size
// where they are read and find out later where the size of the
// variable needs adjustment
{ "sizeof-target-ptr.patch", simple, target_ptr },

// Introduce target timeval
{ "target-timeval.patch", import },

// Use target timeval
{ "target-timeval-use.patch", rename_struct,
		"timeval" TO "ttimeval" },

// Introduce target off_t
{ "target-off_t.patch", import },

// Use target off_t
{ "target-off_t-use.patch", simple, target_off_t },

// Provide platform-independent struct pt_regs
{ "arch-pt-regs.patch", import },

// Remove struct ppc64_pt_regs
{ "remove-ppc64_pt_regs.patch", remove_struct,  "ppc64_pt_regs" },

// Replace remaining ppc64_pt_regs with pt_regs_ppc64
{ "pt-regs-ppc64.patch", rename_struct,
		"ppc64_pt_regs" TO "pt_regs_ppc64" },

// Replace system struct ia64_fpreg with our ia64_fpreg_t
{ "use-ia64_fpreg_t.patch", simple, use_ia64_fpreg_t },

// Use platform-independent pt_regs
// TBD

/************************************************************
 * Print formatters
 */

// Introduce target print formatters
{ "target-printf-spec.patch", import },

// Replace all occurences in the source code
{ "target-printf-spec-use.patch", simple, printf_spec },

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
