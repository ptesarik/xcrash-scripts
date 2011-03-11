#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "tools.h"
#include "clang.tab.h"

static int uptodate;
static const char *basedir;

/************************************************************
 * Useful helper functions
 *
 */

static struct dynstr *
newdynstr_token(const char *text, int token)
{
	struct dynstr *ret = newdynstr(text, strlen(text));
	ret->token = token;
	return ret;
}

static struct dynstr *
replace_text(node_t *node, const char *text)
{
	nullify_str(node);
	struct dynstr *ds = newdynstr_token(text, node->first_text->token);
	replace_text_list(node->first_text, node->last_text, ds, ds);
	return ds;
}

/* Remove text nodes from @ds up to, but not including @keep. */
static void
remove_text_list(struct dynstr *ds, struct dynstr *keep)
{
	while (ds != keep) {
		node_t *node, *nnode;
		
		list_for_each_entry_safe(node, nnode,
					 &ds->node_first, first_list)
			set_node_first(node, &dummydynstr);
		list_for_each_entry_safe(node, nnode,
					 &ds->node_last, last_list)
			set_node_last(node, &dummydynstr);

		ds = dynstr_del(ds);
	}
}

/* Remove text nodes from @ds backwards up to, but not including @keep. */
static void
remove_text_list_rev(struct dynstr *ds, struct dynstr *keep)
{
	while (ds != keep) {
		node_t *node, *nnode;

		list_for_each_entry_safe(node, nnode,
					 &ds->node_first, first_list)
			set_node_first(node, &dummydynstr);
		list_for_each_entry_safe(node, nnode,
					 &ds->node_last, last_list)
			set_node_last(node, &dummydynstr);

		ds = dynstr_del_rev(ds);
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
	return is_id(fn) && !strcmp(fn->str->text, name);
}

/* Returns non-zero if @node is type struct @name. */
static int
is_struct(node_t *node, const char *name)
{
	if (!node)
		return 0;
	return (node->type == nt_type && node->t.category == type_struct &&
		node->str && !strcmp(node->str->text, name));
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

static LIST_HEAD(splitlist);

/* Change the name in @node->t.name */
static void
replace_type_name(node_t *node, const char *newname)
{
	struct dynstr *oldds = node->str;
	struct split_node *split = split_search(&splitlist, oldds, newname);
	struct dynstr *newds = split
		? split->newds
		: newdynstr_token(newname, oldds->token);
	set_node_str(node, newds);
	if (!oldds->refcount) {
		if (split) {
			list_splice(&split->list, &node->parent->list);
			split_remove(split);
		}
		replace_text_list(oldds, oldds, newds, newds);
	} else {
		node_t *parent = node->parent;
		if (parent->type != nt_var) {
			fputs("Don't know how to split non-vars\n", stderr);
			exit(1);
		}
		if (!split)
			split_add(&splitlist, parent, oldds, newds);
		else
			split_addnode(split, parent);
	}
}

/************************************************************
 * Use target types
 *
 */

/* Convert a basic type into its target equivallent */
static int
btype_to_target(node_t *item)
{
	static const struct {
		unsigned long old;
		char *new;
	} subst[] = {
		{ TYPE_UNSIGNED|TYPE_LONG|TYPE_LONGLONG, "tulonglong" },
		{ TYPE_LONG|TYPE_LONGLONG, "tlonglong" },
		{ TYPE_UNSIGNED|TYPE_LONG, "tulong" },
		{ TYPE_LONG, "tlong" },
		{ TYPE_UNSIGNED, "tuint" },
		{ 0, "tint" },
		{ TYPE_UNSIGNED|TYPE_SHORT, "tushort" },
		{ TYPE_SHORT, "tshort" },
	};
	int i;

	for (i = 0; i < sizeof(subst)/sizeof(subst[0]); ++i) {
		if ((item->t.btype & ~TYPE_INT) == subst[i].old) {
			set_node_str(item, replace_text(item, subst[i].new));
			item->t.category = type_typedef;
			return 1;
		}
	}
	return 0;
}

/* Convert a typedef into its target equivallent */
static int
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
		if (!strcmp(item->str->text, subst[i].old)) {
			set_node_str(item, replace_text(item, subst[i].new));
			return 1;
		}
	}
	return 0;
}

/* Convert a target typedef into a GDB variant */
static int
ttype_to_gdb(node_t *item)
{
	static const struct {
		const char *old;
		char *new;
	} subst[] = {
		{ "tulonglong", "bfd_vma" },
		{ "tlonglong", "bfd_signed_vma" },
		{ "tulong", "bfd_vma" },
		{ "tlong", "bfd_signed_vma" },
		/* The following types may need changing: */
		{ "tuint", "unsigned int" },
		{ "tint", "int" },
		{ "tushort", "short" },
		{ "tshort", "unsigned short" },
	};
	int i;

	for (i = 0; i < sizeof(subst)/sizeof(subst[0]); ++i) {
		if (!strcmp(item->str->text, subst[i].old)) {
			replace_text(item, subst[i].new);
			reparse_node(item, START_TYPE_NAME);
			return 1;
		}
	}
	return 0;
}

/* Replace types with target types */
static int target_types(node_t *item, void *data)
{
	struct parsed_file *pf = data;

	/* Convert types to their target equivallents */
	if (item->type == nt_type) {
		int modified = 0;
		if (item->t.category == type_basic)
			modified = btype_to_target(item);
		else if (item->t.category == type_typedef)
			modified = typedef_to_target(item);
		if (modified) {
			if (!strcmp(pf->name, "defs.h") &&
			    check_cpp_cond(item->first_text->cpp_cond,
					   "GDB_COMMON", NULL, NULL) >= 0)
				ttype_to_gdb(item);
			pf->clean = 0;
		}
	}

	return 0;
}

/* Replace sizeof pointers with target pointers */
static int target_ptr(node_t *node, void *data)
{
	struct parsed_file *pf = data;

	if (! (node->type == nt_expr && node->e.op == SIZEOF_TYPE) )
		return 0;

	node_t *arg = nth_element(&node->child[che_arg1], 1);
	if (arg->type == nt_type && arg->t.category == type_pointer) {
		replace_text(arg, "tptr");
		reparse_node(arg, START_TYPE_NAME);
		pf->clean = 0;
	}

	return 0;
}

/* Replace struct timeval with ttimeval */

struct varsearch {
	const char *varname;
	node_t *found;
};

static int
checkvar(node_t *node, void *data)
{
	struct varsearch *vs = data;

	if (!is_id(node) || strcmp(node->str->text, vs->varname))
		return 0;
	vs->found = node;
	return 1;
}

static int
checkselect(node_t *node, void *data)
{
	struct varsearch *vs = data;

	if (!is_direct_call(node, "select"))
		return 0;

	node_t *arg = nth_element(&node->child[che_arg2], 5);
	walk_tree_single(arg, checkvar, vs);
	return !!vs->found;
}

static int
target_timeval(node_t *node, void *data)
{
	struct parsed_file *pf = data;
	struct list_head *scope;

	if (!is_struct(node, "timeval"))
		return 0;

	if (node->parent->type == nt_var) {
		struct varsearch vs;
		vs.varname = node->parent->str->text;
		vs.found = NULL;
		scope = find_scope(&pf->parsed, node);
		walk_tree(scope, checkselect, &vs);
		if (vs.found)
			return 0;
	}

	replace_type_name(node, "ttimeval");
	pf->clean = 0;
	return 0;
}

/* Replace types with target types */
static int target_off_t(node_t *node, void *data)
{
	struct parsed_file *pf = data;

	/* Convert types to their target equivallents */
	if (node->type == nt_type && node->t.category == type_typedef &&
	    !strcmp(node->str->text, "off_t")) {
		replace_type_name(node, "toff_t");
		pf->clean = 0;
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

	const char *id = node->str->text;
	if (!strcmp(id, "LONG_DEC") ||
	    !strcmp(id, "LONG_HEX")) {
		*typecast = "(ulong)";
		return 1;
	} else if (!strcmp(id, "INT_DEC") ||
		   !strcmp(id, "INT_HEX")) {
		*typecast = "(uint)";
		return 1;
	} else if (!strcmp(id, "LONGLONG_HEX")) {
		*typecast = "(ulonglong)";
		return 1;
	} else
		return 0;
}

static int
mkstring_variadic(node_t *node, void *data)
{
	struct parsed_file *pf = data;

	if (!is_direct_call(node, "mkstring"))
		return 0;

	/* Remove MKSTRING if necessary */
	node_t *opt = nth_element(&node->child[che_arg2], 4);
	if (is_direct_call(opt, "MKSTR")) {
		node_t *arg = nth_element(&opt->child[che_arg2], 1);
		nullify_str(opt);
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
	pf->clean = 0;
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
		const char *name = type->str->text;
		if (!strcmp(name, "ushort"))
			return "readshort";
		else if (!strcmp(name, "uint"))
			return "readint";
		else if (!strcmp(name, "ulong"))
			return "readlong";
		else if (!strcmp(name, "longlong") ||
			 !strcmp(name, "ulonglong") ||
			 !strcmp(name, "uint64_t"))
			return "readlonglong";
	} else if (type->t.category == type_pointer)
		return "readptr";
	return NULL;
}

static int
convert_readmem(node_t *node, void *data)
{
	struct parsed_file *pf = data;

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
		struct dynstr *ds = newdynstr_token("1", INT_CONST);
		replace_text_list(arg->first_text, arg->last_text, ds, ds);
	}

	reparse_node(node, START_EXPR);
	pf->clean = 0;
	return 0;
}

/************************************************************
 * Convert printf specifiers
 *
 */

static int
printf_spec_one(node_t *node, void *data)
{
	struct parsed_file *pf = data;

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
			dspfx->token = STRING_CONST;
			list_add_tail(&dspfx->list, &ds);

			/* Create the PRI identifier */
			len = p - spec + 1 + 3;
			char *pri = calloc(len + 1, sizeof(char));
			memcpy(stpcpy(pri, "PRI"), spec, len - 3);
			struct dynstr *dspri = newdynstr(pri, len);
			dspri->token = ID;
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
				newdynstr_token(start, STRING_CONST);
			list_add_tail(&dslast->list, &ds);
		}
		replace_text_list(node->first_text, node->last_text,
				  list_entry(ds.next, struct dynstr, list),
				  list_entry(ds.prev, struct dynstr, list));
		reparse_node(node, START_EXPR);
		pf->clean = 0;
	}

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
	walk_tree_single(spec, printf_spec_one, data);

	return 0;
}

/************************************************************
 * Target pt_regs
 *
 */

/* Remove declarations of struct @oldname and rename all other
 * struct @oldname to struct @newname
 * Returns non-zero if @node was modified
 */
static int
replace_struct(node_t *node, const char *oldname, const char *newname)
{
	if (node->type == nt_decl) {
		node_t *type = nth_element(&node->child[chd_type], 1);
		if (!is_struct(type, oldname))
			return 0;
		remove_text_list(node->first_text,
				 next_dynstr(node->last_text));
		freenode(node);
		return 1;
	}

	/* No change if part of struct pt_regs declaration */
	if (node->parent->type == nt_decl)
		return 0;

	if (!is_struct(node, oldname))
		return 0;

	replace_type_name(node, newname);
	return 1;
}

static int
use_pt_regs_x86_64(node_t *node, void *data)
{
	struct parsed_file *pf = data;
	int cond = check_cpp_cond(node->first_text->cpp_cond,
			      "X86_64", NULL, NULL);
	if (!cond && strcmp(pf->name, "unwind_x86_64.h"))
		return 0;
	else if (cond < 0)
		return 0;

	if (replace_struct(node, "pt_regs", "pt_regs_x86_64"))
		pf->clean = 0;

	return 0;
}

static int
use_pt_regs_ppc64(node_t *node, void *data)
{
	struct parsed_file *pf = data;

	if (replace_struct(node, "ppc64_pt_regs", "pt_regs_ppc64"))
		pf->clean = 0;

	return 0;
}

static int
use_ia64_fpreg_t(node_t *node, void *data)
{
	struct parsed_file *pf = data;

	if (!is_struct(node, "ia64_fpreg"))
		return 0;

	replace_text(node, "ia64_fpreg_t");
	reparse_node(node, START_TYPE_NAME);
	pf->clean = 0;
	return 0;
}

/************************************************************
 * Transformation functions
 *
 */

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
	/* All files must be re-read to reflect the change */
	uptodate = 0;

	return quilt_import(patchname, basedir);
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
		walk_tree(&pf->parsed, xform_fn, pf);
	}
	return quilt_new(patchname, filelist);
}

static struct dynstr *
remove_comma(struct list_head *list, struct dynstr *ds)
{
	ds = dynstr_delspace(list, ds);
	if (&ds->list != list && ds->token == ',')
		ds = dynstr_delspace(list, dynstr_del(ds));
	else {
		ds = dynstr_delspace_rev(list, prev_dynstr(ds));
		if (&ds->list != list && ds->token == ',')
			ds = dynstr_delspace_rev(list, dynstr_del_rev(ds));
	}
	return ds;
}

static void
type_split(struct list_head *raw, struct split_node *split)
{
	node_t *firstvar = first_node(&split->nodes);
	node_t *olddecl = firstvar->parent;
	node_t *newdecl;
	struct dynstr *ds, *point = olddecl->first_text;
	YYLTYPE loc;

	loc.first_text = NULL;
	for (ds = olddecl->first_text; ds != split->oldds;
	     ds = next_dynstr(ds)) {
		struct dynstr *newds = newdynstr(ds->text, ds->len);
		insert_text_list(point, newds, newds);
		if (!loc.first_text)
			loc.first_text = newds;
	}
	insert_text_list(point, split->newds, split->newds);
	if (!loc.first_text)
		loc.first_text = split->newds;

	loc.last_text = split->newds;
	newdecl = newnode(&loc, nt_decl, chd_max);

	ds = newdynstr(" ", 1);
	node_t *var, *nvar;
	list_for_each_entry_safe(var, nvar, &split->nodes, list) {
		if (var != firstvar)
			ds = newdynstr(", ", 2);
		insert_text_list(point, ds, ds);

		struct dynstr *nextds = next_dynstr(var->last_text);
		detach_text(var->first_text, var->last_text);
		insert_text_list(point, var->first_text, var->last_text);
		remove_comma(raw, nextds);

		freenode(var);
	}

	ds = newdynstr(";", 1);
	insert_text_list(point, ds, ds);
	set_node_last(newdecl, ds);
	reparse_node(newdecl, START_DECL);
}

/* Helper for substituting types */
static int
type_subst(const char *patchname, struct list_head *filelist, void *xform_fn)
{
	struct parsed_file *pf;
	int res;

	if ( (res = update_parsed_files(filelist)) )
		return res;

	list_for_each_entry(pf, filelist, list) {
		walk_tree(&pf->parsed, xform_fn, pf);

		struct split_node *split, *nsplit;
		list_for_each_entry_safe(split, nsplit, &splitlist, list) {
			type_split(&pf->raw, split);
			split_remove(split);
		}
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
{ "target-timeval-use.patch", type_subst, target_timeval },

// Introduce target off_t
{ "target-off_t.patch", import },

// Use target off_t
{ "target-off_t-use.patch", type_subst, target_off_t },

// Provide platform-independent struct pt_regs
{ "arch-pt-regs.patch", import },

// Use platform-independent pt_regs_x86_64
{ "pt-regs-x86_64.patch", type_subst, use_pt_regs_x86_64 },

// Replace ppc64_pt_regs with pt_regs_ppc64
{ "pt-regs-ppc64.patch", type_subst, use_pt_regs_ppc64 },

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
