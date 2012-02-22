#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "tools.h"
#include "varscope.h"
#include "clang.tab.h"

static int uptodate;
static const char *basedir;

static int update_parsed_files(struct list_head *filelist);

/************************************************************
 * Useful helper functions
 *
 */

static struct dynstr *
replace_text(node_t *node, const char *text)
{
	nullify_str(node);
	struct dynstr *ds = newdynstr(text, strlen(text));
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

/************************************************************
 * Type replacements with split declarations
 *
 */

static LIST_HEAD(splitlist);

/* Replace all nodes in @nodelist with a copy of @newnode */
static void
replace_nodes(struct list_head *nodelist, node_t *newnode)
{
	node_t *node, *nnode;
	list_for_each_entry_safe(node, nnode, nodelist, split_list) {
		list_add(&dupnode(newnode)->list, &node->list);
		freenode(node);
	}
}

/* Change the type definition of @node */
static node_t *
replace_type(node_t *node, const char *newtext)
{
	struct dynstr *oldds = node->str;
	struct split_node *split = split_search(&splitlist, oldds, newtext);
	struct dynstr *newds = split
		? split->newds
		: newdynstr(newtext, strlen(newtext));
	set_node_str(node, NULL);
	if (!oldds->refcount) {
		replace_text_list(node->first_text, node->last_text,
				  newds, newds);

		node = reparse_node(node, START_TYPE_NAME);
		newds = node->str;

		if (split) {
			replace_nodes(&split->nodes, node);
			split_remove(split);
		}
	} else if (!split)
		split_add(&splitlist, node, oldds, newds);
	else
		split_addnode(split, node);
	return node;
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
	struct dynstr *ds, *point;
	node_t *newdecl;
	YYLTYPE loc;

	loc.first_text = loc.last_text = split->newds;
	newdecl = newnode(&loc, nt_decl, chd_max);

	node_t *firstvar = list_entry(split->nodes.next, node_t, split_list);
	node_t *olddecl = typed_parent(firstvar, nt_decl);
	point = olddecl->first_text;
	struct dynstr *indent = dynstr_dup_indent(raw, point, 0);

	insert_text_list(point, split->newds, split->newds);
	ds = newdynstr(" ", 1);
	node_t *type, *ntype;
	list_for_each_entry_safe(type, ntype, &split->nodes, split_list) {
		if (!ds)
			ds = newdynstr(", ", 2);
		insert_text_list(point, ds, ds);
		ds = NULL;

		node_t *var = typed_parent(type, nt_var);
		struct dynstr *nextds = next_dynstr(var->last_text);
		detach_text(var->first_text, var->last_text);
		insert_text_list(point, var->first_text, var->last_text);
		remove_comma(raw, nextds);

		freenode(var);
	}

	ds = newdynstr(";", 1);
	insert_text_list(point, ds, ds);
	set_node_last(newdecl, ds);

	insert_text_list(point, indent, indent);
	reparse_node(newdecl, START_DECL);
}

/************************************************************
 * Use target types
 *
 */

/* Convert a basic type into its target equivallent */
static const char *
btype_to_target(node_t *item)
{
	static const struct {
		unsigned long old;
		const char *new;
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
		if ((item->t.btype & ~TYPE_INT) == subst[i].old)
			return subst[i].new;
	}
	return NULL;
}

/* Convert a typedef into its target equivallent */
static const char *
typedef_to_target(node_t *item)
{
	static const struct {
		const char *old;
		const char *new;
	} subst[] = {
		{ "ushort", "tushort" },
		{ "uint", "tuint" },
		{ "ulong", "tulong" },
		{ "longlong", "tlonglong" },
		{ "ulonglong", "tulonglong" },
		{ "u_long", "tulong" },
	};
	int i;

	for (i = 0; i < sizeof(subst)/sizeof(subst[0]); ++i) {
		if (!strcmp(item->str->text, subst[i].old))
			return subst[i].new;
	}
	return NULL;
}

/* Convert a target typedef into a GDB variant */
static const char *
ttype_to_gdb(const char *text)
{
	static const struct {
		const char *old;
		const char *new;
	} subst[] = {
		{ "tulonglong", "bfd_vma" },
		{ "tlonglong", "bfd_signed_vma" },
		{ "tulong", "bfd_vma" },
		{ "tlong", "bfd_signed_vma" },
		{ "tptr", "bfd_vma" },
		/* The following types may need changing: */
		{ "tuint", "unsigned int" },
		{ "tint", "int" },
		{ "tushort", "short" },
		{ "tshort", "unsigned short" },
	};
	int i;

	for (i = 0; i < sizeof(subst)/sizeof(subst[0]); ++i) {
		if (!strcmp(text, subst[i].old))
			return subst[i].new;
	}
	return text;
}

/* Substitute a host @type with its corresponding target type.
 * Use GDB-specific types if @gdb is nonzero.
 */
static const char *
subst_target_type(node_t *type, int gdb)
{
	const char *modified = NULL;

	if (type->t.category == type_basic)
		modified = btype_to_target(type);
	else if (type->t.category == type_typedef)
		modified = typedef_to_target(type);
	else if (type->t.category == type_pointer)
		modified = "tptr";

	if (modified && gdb)
		modified = ttype_to_gdb(modified);

	return modified;
}

/* Array of function names seen inside a GDB_COMMON block */
static const char **gdb_common_decls;
static unsigned num_gdb_common_decls;

static int
find_gdb_common_decl(const char *name)
{
	unsigned i;
	for (i = 0; i < num_gdb_common_decls; ++i)
		if (!strcmp(gdb_common_decls[i], name))
			return 1;
	return 0;
}

static void
add_gdb_common_decl(const char *name)
{
	if (find_gdb_common_decl(name))
		return;

	++num_gdb_common_decls;
	gdb_common_decls =
		realloc(gdb_common_decls,
			num_gdb_common_decls * sizeof(const char*));
	gdb_common_decls[num_gdb_common_decls-1] = name;
}

/* Walk the raw list and mark all referencing nodes as GDB_COMMON */
static void
mark_raw_gdb_common(struct list_head *raw)
{
	struct dynstr *ds;
	node_t *cond = NULL;
	int is_gdb = 1;

	list_for_each_entry(ds, raw, list) {
		if (ds->cpp_cond != cond) {
			cond = ds->cpp_cond;
			is_gdb = check_cpp_cond(cond,
						"GDB_COMMON", NULL, NULL) >= 0
				? 1 : 0;
		}

		if (!is_gdb)
			continue;

		node_t *node;
		list_for_each_entry(node, &ds->node_first, first_list)
			node->user_data = (void*)1;
		list_for_each_entry(node, &ds->node_last, last_list)
			node->user_data = (void*)1;
	}
}

static enum walk_action
mark_node_gdb_common(node_t *item, void *data)
{
	item->user_data = (void*)1;
	return walk_continue;
}

static enum walk_action
find_gdb_print_options(node_t *node, void *data)
{
	if (is_direct_call(node, "gdb_user_print_option_address")) {
		node_t *expr = node;
		while (expr->type == nt_expr && expr->e.op != '=')
			expr = expr->parent;
		if (expr->e.op != '=')
			return walk_continue;

		node_t *left = nth_element(&expr->child[che_arg1], 1);
		if (left->type == nt_expr && left->e.op == ID) {
			add_gdb_common_decl(left->str->text);
			walk_tree(&expr->child[che_arg2],
				  mark_node_gdb_common, NULL);
		} else
			fputs("Unknown left hand side type\n", stderr);
	} else if (node->type == nt_decl) {
		/* mark func declaration too */
		node_t *var;
		list_for_each_entry(var, &node->child[chd_var], list)
			if (var->str &&
			    !strcmp(var->str->text,
				    "gdb_user_print_option_address"))
				walk_tree_single(var, mark_node_gdb_common,
						 NULL);
	}

	return walk_continue;
}

/* Replace types with target types */
static enum walk_action
target_types_fn(node_t *item, void *data)
{
	/* Convert types to their target equivallents */
	if (item->type == nt_type) {
		const char *modified = NULL;
		if (item->t.category == type_basic)
			modified = btype_to_target(item);
		else if (item->t.category == type_typedef)
			modified = typedef_to_target(item);
		if (modified) {
			if (item->user_data)
				modified = ttype_to_gdb(modified);
			replace_type(item, modified);
		}
	} else if (item->type == nt_decl) {
		node_t *var;
		list_for_each_entry(var, &item->child[chd_var], list)  {
			if (!var->str)
				continue;

			node_t *type = nth_element(&var->child[chv_type], 1);

			if (item->user_data && type &&
			    type->t.category == type_func)
				add_gdb_common_decl(var->str->text);
			else if (find_gdb_common_decl(var->str->text))
				walk_tree(&var->child[chv_type],
					  mark_node_gdb_common, NULL);
		}
	}

	return walk_continue;
}

/* Specialized handler to replace target types */
static int
target_types(const char *patchname, struct list_head *filelist, void *data)
{
	struct parsed_file *pf;
	int res;

	if ( (res = update_parsed_files(filelist)) )
		return res;

	list_for_each_entry(pf, filelist, list) {
		if (!strcmp(pf->name, "defs.h"))
			mark_raw_gdb_common(&pf->raw);
		walk_tree(&pf->parsed, find_gdb_print_options, pf);
	}
	list_for_each_entry(pf, filelist, list)
		walk_tree(&pf->parsed, target_types_fn, pf);
	return quilt_new(patchname, filelist);
}

/* Replace struct timeval with ttimeval */

struct varsearch {
	const char *varname;
	node_t *found;
};

static enum walk_action
checkvar(node_t *node, void *data)
{
	struct varsearch *vs = data;

	if (!is_id(node) || strcmp(node->str->text, vs->varname))
		return walk_continue;
	vs->found = node;
	return walk_terminate;
}

static enum walk_action
checkselect(node_t *node, void *data)
{
	struct varsearch *vs = data;

	if (!is_direct_call(node, "select"))
		return walk_continue;

	node_t *arg = nth_element(&node->child[che_arg2], 5);
	walk_tree_single(arg, checkvar, vs);
	return vs->found ? walk_terminate : walk_continue;
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

	replace_type(node, "struct ttimeval");
	return 0;
}

/* Replace types with target types */
static int target_off_t(node_t *node, void *data)
{
	/* Convert types to their target equivallents */
	if (node->type == nt_type && node->t.category == type_typedef &&
	    !strcmp(node->str->text, "off_t"))
		replace_type(node, "toff_t");

	return 0;
}

/************************************************************
 * Replace target pointer types
 *
 */

static enum walk_action
target_ptr_var(node_t *node, void *data)
{
	struct parsed_file *pf = data;

	if (node->type != nt_expr)
		return walk_skip_children;

	node_t *var = (node->e.op == '&')
		? varscope_expr(&pf->parsed,
				first_node(&node->child[che_arg1]))
		: varscope_expr(&pf->parsed, node);
	if (!var || list_empty(&var->child[chv_type]))
		return walk_continue;

	node_t *type = first_node(&var->child[chv_type]);
	if (type->split_list.next)
		return walk_terminate;

	if (node->e.op == '&') {
		const char *newtype = subst_target_type(type, 0);
		if (newtype) {
			replace_type(type, newtype);
			return walk_terminate;
		}
	} else if (type->t.category == type_pointer) {
		struct dynstr *dsfirst = type->str;
		node_t *prevtype = type;
		type = first_node(&type->child[cht_type]);
		list_move(&type->list, &prevtype->list);
		type->parent = prevtype->parent;
		freenode(prevtype);

		remove_text_list(dsfirst, var->str);
		if (var->first_text == &dummydynstr)
			set_node_first(var, var->str);

		while (type->t.category == type_pointer)
			type = first_node(&type->child[cht_type]);

		replace_type(type, "tptr");
		return walk_terminate;
	}

	return walk_skip_children;
}

static enum walk_action
find_sizeof(node_t *node, void *data)
{
	if (node->type == nt_expr && node->e.op == SIZEOF_TYPE) {
		*(node_t**)data = node;
		return walk_terminate;
	}

	return walk_continue;
}

static enum walk_action
target_types_symbol_data_fn(node_t *node, void *data)
{
	struct parsed_file *pf = data;
	node_t *arg;

	if (!is_direct_call(node, "get_symbol_data") &&
	    !is_direct_call(node, "try_get_symbol_data"))
		return walk_continue;

	/* Process the 2nd argument */
	arg = nth_element(&node->child[che_arg2], 2);
	node_t *typesize = NULL;
	walk_tree_single(arg, find_sizeof, &typesize);
	if (!typesize)
		return walk_continue;

	node_t *type = nth_element(&typesize->child[che_arg1], 1);
	const char *newtype = subst_target_type(type, 0);
	if (newtype) {
		replace_text(type, newtype);
		reparse_node(type, START_TYPE_NAME);
	} else
		return walk_continue;

	/* Process the 3rd argument */
	arg = nth_element(&node->child[che_arg2], 3);
	walk_tree_single(arg, target_ptr_var, pf);

	return walk_continue;
}

static int
target_types_symbol_data(const char *patchname, struct list_head *filelist,
			 void *data)
{
	struct parsed_file *pf = data;
	int res;

	if ( (res = update_parsed_files(filelist)) )
		return res;

	fill_varscope(filelist);
	list_for_each_entry(pf, filelist, list)
		walk_tree(&pf->parsed, target_types_symbol_data_fn, pf);

	list_for_each_entry(pf, filelist, list) {
		struct split_node *split, *nsplit;
		list_for_each_entry_safe(split, nsplit, &splitlist, list) {
			type_split(&pf->raw, split);
			split_remove(split);
		}
	}

	return quilt_new(patchname, filelist);
}

/************************************************************
 * Translate calls to mkstring()
 *
 */

static enum walk_action
mkstring_typecast(node_t *node, void *data)
{
	const char **typecast = data;

	if (!is_id(node))
		return walk_continue;

	const char *id = node->str->text;
	if (!strcmp(id, "LONG_DEC") ||
	    !strcmp(id, "LONG_HEX")) {
		*typecast = "(ulong)";
		return walk_terminate;
	} else if (!strcmp(id, "INT_DEC") ||
		   !strcmp(id, "INT_HEX")) {
		*typecast = "(uint)";
		return walk_terminate;
	} else if (!strcmp(id, "LONGLONG_HEX")) {
		*typecast = "(ulonglong)";
		return walk_terminate;
	} else
		return walk_continue;
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
	nullify_str(arg);
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

/************************************************************
 * Convert printf specifiers
 *
 */

static enum walk_action
printf_spec_one(node_t *node, void *data)
{
	if (node->type != nt_expr || node->e.op != STRING_CONST)
		return walk_continue;

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
				newdynstr(start, strlen(start));
			list_add_tail(&dslast->list, &ds);
		}
		nullify_str(node);
		replace_text_list(node->first_text, node->last_text,
				  list_entry(ds.next, struct dynstr, list),
				  list_entry(ds.prev, struct dynstr, list));
		reparse_node(node, START_EXPR);
	}

	return walk_continue;
}

static enum walk_action
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
		return walk_continue;

	node_t *spec = nth_element(&node->child[che_arg2], pos);
	walk_tree_single(spec, printf_spec_one, data);

	return walk_continue;
}

/************************************************************
 * Target pt_regs
 *
 */

/* Remove declarations of struct @oldname and rename all other
 * struct @oldname to @newname
 * Returns non-zero if @node was modified
 */
static int
replace_struct(node_t *node, const char *oldname, const char *newname)
{
	if (node->type == nt_decl) {
		node_t *type = nth_element(&node->child[chd_type], 1);
		if (!is_struct(type, oldname))
			return 0;
		node->pf->clean = 0;
		nullify_str(node);
		remove_text_list(node->first_text,
				 next_dynstr(node->last_text));
		freenode(node);
		return 1;
	}

	if (!is_struct(node, oldname))
		return 0;

	replace_type(node, newname);
	return 1;
}

#define DEF_PT_REGS_X86_64	"#define pt_regs pt_regs_x86_64"

static enum walk_action
use_pt_regs_x86_64(node_t *node, void *data)
{
	struct parsed_file *pf = data;
	int cond = check_cpp_cond(node->first_text->cpp_cond,
			      "X86_64", NULL, NULL);
	if (!cond && strcmp(pf->name, "unwind_x86_64.h"))
		return walk_continue;
	else if (cond < 0)
		return walk_continue;

	if (node->type == nt_decl) {
		node_t *type = nth_element(&node->child[chd_type], 1);
		if (is_struct(type, "pt_regs")) {
			struct dynstr *ds =
				newdynstr(DEF_PT_REGS_X86_64,
					  strlen(DEF_PT_REGS_X86_64));
			insert_text_list(node->first_text, ds, ds);
		}
	}

	replace_struct(node, "pt_regs", "struct pt_regs_x86_64");
	return walk_continue;
}

static enum walk_action
use_pt_regs_ppc64(node_t *node, void *data)
{
	replace_struct(node, "ppc64_pt_regs", "struct pt_regs_ppc64");
	return walk_continue;
}

static enum walk_action
use_ia64_fpreg_t(node_t *node, void *data)
{
	replace_struct(node, "ia64_fpreg", "ia64_fpreg_t");
	return walk_continue;
}

/************************************************************
 * Transformation functions
 *
 */

static int
update_parsed_files(struct list_head *filelist)
{
	struct parsed_file *pf;

	if (uptodate) {
		list_for_each_entry(pf, filelist, list)
			reset_user_data(&pf->parsed);
		return 0;
	}

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

{ "remove-VOID_PTR.patch", import },

{ "gdb-does-not-need-syment.patch", import },

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

// Move or add #include "defs.h", so that target types are
// available in all files
{ "move-include-defs.patch", import },

// Fix up some constructs which are no longer valid after moving
// the #include directive
{ "include-defs-fixups.patch", import },

// Target types in calls to (try_)get_symbol_data
{ "target-types-symbol_data.patch", target_types_symbol_data },

// Use target types
{ "target-types-use.patch", target_types },

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
