#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "tools.h"
#include "varscope.h"
#include "clang.tab.h"

#define MAXIND	32		/* Arbitrarily chosen constant... */

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
	struct dynstr *prev = prev_dynstr(ds);
	node_t *node, *nnode;

	while (ds != keep) {
		list_for_each_entry_safe(node, nnode,
					 &ds->node_first, first_list)
			set_node_first(node, keep);
		list_for_each_entry_safe(node, nnode,
					 &ds->node_last, last_list)
			set_node_last(node, prev);

		ds = dynstr_del(ds);
	}

	list_for_each_entry_safe(node, nnode, &ds->node_first, first_list)
		if (node->last_text == prev) {
			set_node_first(node, &dummydynstr);
			set_node_last(node, &dummydynstr);
		}
}

/* Remove text nodes from @ds backwards up to, but not including @keep. */
static void
remove_text_list_rev(struct dynstr *ds, struct dynstr *keep)
{
	struct dynstr *next = next_dynstr(ds);
	node_t *node, *nnode;

	while (ds != keep) {
		list_for_each_entry_safe(node, nnode,
					 &ds->node_first, first_list)
			set_node_first(node, next);
		list_for_each_entry_safe(node, nnode,
					 &ds->node_last, last_list)
			set_node_last(node, keep);

		ds = dynstr_del_rev(ds);
	}

	list_for_each_entry_safe(node, nnode, &ds->node_first, first_list)
		if (node->first_text == next) {
			set_node_first(node, &dummydynstr);
			set_node_last(node, &dummydynstr);
		}
}

static void
replace_node_str(node_t *node, const char *newtext)
{
	struct dynstr *oldds = node->str;
	struct dynstr *newds = newdynstr(newtext, strlen(newtext));

	list_add(&newds->list, &oldds->list);
	list_add(&newds->cpp_list, &oldds->cpp_list);
	list_add(&newds->node_first, &oldds->node_first);
	list_add(&newds->node_last, &oldds->node_last);
	newds->cpp_cond = oldds->cpp_cond;
	newds->token = oldds->token;

	set_node_str(node, newds);
	dynstr_del(oldds);
	node->pf->clean = 0;
}

/* Delete the node (and all its children). Also removes the acoompanying
 * dynstr objects.
 */
static void
delete_node(node_t *node)
{
	nullify_str(node);
	remove_text_list(node->first_text, next_dynstr(node->last_text));
	freenode(node);
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

/* Returns non-zero if @node is a function type. */
static int
is_function(node_t *node)
{
	return node && node->type == nt_type &&
		node->t.category == type_func;
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

/* Get the base type of @type */
static node_t *
base_type(node_t *type)
{
	while (type->t.category == type_pointer)
		type = first_node(&type->child[cht_type]);
	return type;
}

/* "Flatten" @type, i.e. remove all nodes in the hierarchy above
 * @base up to and including @type.
*/
static node_t *
flatten_type(node_t *type, node_t *base)
{
	while (type != base) {
		node_t *child = first_node(&type->child[cht_type]);
		assert(!list_empty(&type->child[cht_type]));
		list_splice_init(&type->child[cht_type], &type->list);

		child->parent = type->parent;
		if (type->first_text == child->first_text) {
			struct dynstr *newfirst =
				next_dynstr(child->last_text);
			list_move(&type->first_list, &newfirst->node_first);
			type->first_text = newfirst;
		}
		delete_node(type);
		type = child;
	}
	return base;
}

/************************************************************
 * Type replacements with split declarations
 *
 */

static LIST_HEAD(splitlist);
static LIST_HEAD(replacedlist);

/* Replace all nodes in @nodelist with a copy of @newnode */
static void
replace_nodes(struct list_head *nodelist, node_t *newnode)
{
	node_t *node, *nnode;
	list_for_each_entry_safe(node, nnode, nodelist, user_list) {
		node_t *dup = dupnode(newnode);
		dup->parent = node->parent;
		list_add(&dup->list, &node->list);
		list_add_tail(&dup->user_list, &replacedlist);
		freenode(node);
	}
}

static int
check_split(node_t *node, struct split_node *split, const char *newtext)
{
	if (!node->user_list.next)
		return 0;	/* First request for this var */
	if (split)
		return 1;	/* Matching split already exists */

	/* If the variable is already on the split list, but no matching
	 * split was found, there is a split for a different @newtext.
	 */
	node_t *var = typed_parent(node, nt_var);
	node_t *base = base_type(node);
	list_for_each_entry(split, &splitlist, list)
		if (split->oldds == base->str)
			break;
	fprintf(stderr, "Conflicting type change for '%s %s':"
		" first '%s', now '%s'\n",
		base->str->text, var ? var->str->text : "<unknown>",
		&split->list != &splitlist ? split->newds->text : "<unknown>",
		newtext);
	abort();
}

/* This does the real job if there is only a single reference
 * to @type.
 * Returns: the new type node.
 */
static node_t *
replace_single_type(node_t *type, struct dynstr *newds)
{
	set_node_str(type, NULL);
	replace_text_list(type->first_text, type->last_text,
			  newds, newds);

	type = reparse_node(type, START_TYPE_NAME);
	list_add_tail(&type->user_list, &replacedlist);
	return type;
}

/* Change the type definition of @node */
static node_t *
replace_type(node_t *node, const char *newtext)
{
	node_t *base = base_type(node);
	struct dynstr *newds, *oldds = base->str;
	struct split_node *split = split_search(&splitlist, oldds, newtext);
	if (check_split(node, split, newtext))
		return node;
	newds = split
		? split->newds
		: newdynstr(newtext, strlen(newtext));
	node = flatten_type(node, base);
	if (split)
		split_addnode(split, node);
	else if (node->str->refcount > 1)
		split_add(&splitlist, node, oldds, newds);
	else
		replace_single_type(node, newds);
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
	node_t *type, *ntype, *first;

	/* First, remove references via a split ->str to get the number
	 * of references for each dynstr.
	 */
	list_for_each_entry(type, &split->nodes, user_list)
		set_node_str(type, NULL);

	/* Check if the split is unnecessary (all vars are converted) */
	first = list_entry(split->nodes.next, node_t, user_list);
	if (!split->oldds->refcount) {
		list_del(&first->user_list);
		first = replace_single_type(first, split->newds);
		replace_nodes(&split->nodes, first);
		return;
	}

	/* Get the insertion point */
	node_t *olddecl = typed_parent(first, nt_decl);
	point = olddecl->first_text;

	/* Create a new dynstr chain */
	insert_text_list(point, split->newds, split->newds);
	ds = newdynstr(" ", 1);
	list_for_each_entry_safe(type, ntype, &split->nodes, user_list) {
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

	/* Create a new parse tree node */
	node_t *newdecl = dupnode_nochild(olddecl);
	list_add_tail(&newdecl->list, &olddecl->list);
	list_del_init(&newdecl->dup_list);
	set_node_first(newdecl, split->newds);
	set_node_last(newdecl, ds);
	newdecl = reparse_node(newdecl, START_DECL);

	/* Restore indentation */
	ds = dynstr_dup_indent(raw, newdecl->first_text, 0);
	insert_text_list(point, ds, ds);

	/* Add it to the list of tracked variables */
	node_t *var;
	list_for_each_entry(var, &newdecl->child[chd_var], list) {
		node_t *type = first_node(&var->child[chv_type]);
		list_add_tail(&type->user_list, &replacedlist);
	}
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
 * Use GDB-specific types if the definition of @type is also
 * used when compiling GDB.
 */
static const char *
target_type_name(node_t *type)
{
	const char *modified = NULL;

	if (type->t.category == type_basic)
		modified = btype_to_target(type);
	else if (type->t.category == type_typedef)
		modified = typedef_to_target(type);
	else if (type->t.category == type_pointer)
		modified = "tptr";

	if (modified && type->pf->name && !strcmp(type->pf->name, "defs.h") &&
	    check_cpp_cond(type->str->cpp_cond,
			   "GDB_COMMON", NULL, NULL) >= 0)
		modified = ttype_to_gdb(modified);

	return modified;
}

static inline node_t *
next_dup(node_t *node)
{
	return list_entry(node->dup_list.next, node_t, dup_list);
}

static inline int
ind_is_pointer(ind_t ind)
{
	return (ind == ind_pointer || ind == ind_implicit);
}

static inline int
ind_is_func(ind_t ind)
{
	return (ind == ind_return || ind > 0);
}

/* Return the base type of @type according to the instructions in @ind */
static node_t *
ind_base_type(node_t *type, const ind_t *ind)
{
	while (*ind != ind_stop) {
		if (ind_is_pointer(*ind)) {
			if (type->t.category != type_pointer &&
			    type->t.category != type_array) {
				ind_warn("pointer/array not found", ind);
				return NULL;
			}
			type = first_node(&type->child[cht_type]);
		} else if (*ind == ind_return) {
			if (type->t.category != type_func) {
				ind_warn("func not found", ind);
				return NULL;
			}
			type = first_node(&type->child[cht_type]);
		} else {
			assert(ind > 0);

			node_t *decl;
			decl = nth_element(&type->child[cht_param], *ind);
			if (!decl) {
				ind_warn("func arg not found", ind);
				return NULL;
			}
			assert(decl->type == nt_decl);

			type = first_node(&decl->child[chd_type]);
			if (&type->list == &decl->child[chd_type]) {
				type = first_node(&decl->child[chd_var]);
				assert(&type->list != &decl->child[chd_var]);
				type = first_node(&type->child[chv_type]);
				assert(&type->list != &type->child[chv_type]);
			}
		}
		--ind;
	}

	return type;
}

/* Substitute @type with a target type (if applicable)
 * The target type is at @ind indirection level.
 * Returns 1 if the type was substituted, zero otherwise.
 */
static int
subst_target_type(node_t *type, const ind_t *ind)
{
	if (! (type = ind_base_type(type, ind)) )
		return 0;

	if (type->pf->name) {
		const char *newtype = target_type_name(type);
		if (newtype) {
			replace_type(type, newtype);
			return 1;
		}
	}

	if (!type->user_list.next)
		list_add_tail(&type->user_list, &replacedlist);

	return 0;
}

/* Substitute the type for @firstvar and all its duplicates.
 * The target type is at @ind indirection level.
 */
static int
subst_target_var(node_t *firstvar, const ind_t *ind)
{
	int ret = 0;
	do {
		node_t *var = firstvar;
		do {
			node_t *type = nth_element(&var->child[chv_type], 1);
			if (type) {
				if (type->t.category == type_func &&
				    ind_is_pointer(*ind))
					--ind;
				ret += subst_target_type(type, ind);
			}
		} while ((var = next_dup(var)) != firstvar);

		/* Find other declarations, too */
		firstvar = varscope_find_next(firstvar);
	} while (firstvar);
	return ret;
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
		if (pf->name && !strcmp(pf->name, "defs.h"))
			mark_raw_gdb_common(&pf->raw);
		walk_tree(&pf->parsed, find_gdb_print_options, pf);
	}
	list_for_each_entry(pf, filelist, list)
		if (pf->name)
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
	struct list_head *scope;

	if (!is_struct(node, "timeval"))
		return 0;

	if (node->parent->type == nt_var) {
		struct varsearch vs;
		vs.varname = node->parent->str->text;
		vs.found = NULL;
		scope = find_scope(node, NULL);
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
 * Split machspec usage by architecture
 *
 */
static enum walk_action
arch_machspec(node_t *node, void *data)
{
	static const struct {
		const char *const cond;	   /* CPP condition */
		const char *const newname; /* Resulting type name */
	} tbl[] = {
		{ "ARM", "arm_machine_specific" },
		{ "X86", "x86_machine_specific" },
		{ "X86_64", "x86_64_machine_specific" },
		{ "PPC64", "ppc64_machine_specific" },
		{ "IA64", "ia64_machine_specific" },
		{ NULL, NULL }
	}, *tp;

	if (!is_struct(node, "machine_specific"))
		return walk_continue;

	node_t *cpp_cond = node->str->cpp_cond;
	for (tp = tbl; tp->cond; ++tp)
		if (check_cpp_cond(cpp_cond, tp->cond, NULL, NULL) > 0) {
			replace_node_str(node, tp->newname);
			break;
		}

	return walk_continue;
}

/************************************************************
 * Modify variables to target types
 *
 */

static enum walk_action
target_var(node_t *node, void *data)
{
	if (node->type != nt_expr)
		return walk_skip_children;

	node_t *var = (node->e.op == '&')
		? varscope_expr(first_node(&node->child[che_arg1]))
		: varscope_expr(node);
	if (!var || list_empty(&var->child[chv_type]))
		return walk_continue;

	node_t *type = first_node(&var->child[chv_type]);
	if (node->e.op == '&' || type->t.category == type_pointer) {
		const char *newtype;

		/* If we're passing the value of a pointer, change the
		 * pointer's base type */
		if (node->e.op != '&')
			type = first_node(&type->child[cht_type]);
		newtype = target_type_name(type);
		if (newtype) {
			replace_type(type, newtype);
			return walk_terminate;
		}
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

static void
target_sizeof(node_t *node)
{
	node_t *typesize = NULL;
	walk_tree_single(node, find_sizeof, &typesize);
	if (!typesize)
		return;

	node_t *type = nth_element(&typesize->child[che_arg1], 1);
	const char *newtype = target_type_name(type);
	if (newtype) {
		replace_text(type, newtype);
		reparse_node(type, START_TYPE_NAME);
	}
}

static enum walk_action
target_types_symbol_data(node_t *node, void *data)
{
	struct parsed_file *pf = data;
	node_t *arg;

	if (!is_direct_call(node, "get_symbol_data") &&
	    !is_direct_call(node, "try_get_symbol_data"))
		return walk_continue;

	/* Process the 2nd argument */
	arg = nth_element(&node->child[che_arg2], 2);
	target_sizeof(arg);

	/* Process the 3rd argument */
	arg = nth_element(&node->child[che_arg2], 3);
	walk_tree_single(arg, target_var, pf);

	return walk_continue;
}

static enum walk_action
target_types_readmem(node_t *node, void *data)
{
	struct parsed_file *pf = data;
	int has_size;
	node_t *arg;

	if (is_direct_call(node, "readmem"))
		has_size = 1;
	else if (is_direct_call(node, "readshort") ||
		 is_direct_call(node, "readint") ||
		 is_direct_call(node, "readlong") ||
		 is_direct_call(node, "readulonglong") ||
		 is_direct_call(node, "readptr"))
		has_size = 0;
	else
		return walk_continue;

	/* Process the 3rd argument (@buffer) */
	arg = nth_element(&node->child[che_arg2], 3);
	walk_tree_single(arg, target_var, pf);

	if (has_size) {
		/* Process the 4th argument (@size) */
		arg = nth_element(&node->child[che_arg2], 4);
		target_sizeof(arg);
	}

	return walk_continue;
}

static enum walk_action
target_facilitators(node_t *node, void *data)
{
	static const char * const namelist[] = {
		"INT", "UINT", "LONG", "ULONG",
		"ULONGLONG", "ULONG_PTR", "USHORT", "SHORT",
		NULL
	};
	const char *const *name;
	struct parsed_file *pf = node->pf;

	/* Ignore everything except the built-in file */
	if (pf->name)
		return walk_terminate;

	for (name = namelist; *name; ++name) {
		node_t *type = varscope_type(NULL, *name);
		if (type)
			list_add_tail(&type->user_list, &replacedlist);
		else
			fprintf(stderr, "ERROR: Cannot find %s\n", *name);
	}

	return walk_terminate;
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
	if (!cond && pf->name && strcmp(pf->name, "unwind_x86_64.h"))
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
 * Variable tracking
 *
 */

static enum walk_action
build_scopes(node_t *node, void *data)
{
	node_t *var = varscope_expr(node);
	if (var) {
		node->user_data = var;
		if (!var->user_list.next)
			INIT_LIST_HEAD(&var->user_list);
		list_add_tail(&node->user_list, &var->user_list);
	}
	return walk_continue;
}

/* Check whether @expr refers to host-specific data */
static int
is_host_type(node_t *expr, ind_t *ind)
{
	node_t *child, *var, *type;

	switch (expr->e.op) {
	case INT_CONST:
	case FLOAT_CONST:
	case CHAR_CONST:
		return 0;

	case STRING_CONST:
		return 1;

	case '&':
	case '*':
		child = first_node(&expr->child[che_arg1]);
		if (list_empty(&expr->child[che_arg2])) {
			if (expr->e.op == '*')
				*++ind = ind_pointer;
			else if (!ind_is_pointer(*ind))
				return 1;
			else
				--ind;
		}
		return is_host_type(child, ind);

	case '/':
	case '|':
	case '^':
	case SHL_OP:
	case SHR_OP:
	case ARRAY:
		child = first_node(&expr->child[che_arg1]);
		return is_host_type(child, ind);

	case '+':
	case '-':
		child = nth_element(&expr->child[che_arg1], 1);
		if (child && is_host_type(child, ind))
			return 1;

		child = nth_element(&expr->child[che_arg2], 1);
		if (child && is_host_type(child, ind))
			return 1;
		return 0;

	case '?':
		child = nth_element(&expr->child[che_arg3], 1);
		if (child && is_host_type(child, ind))
			return 1;

		child = nth_element(&expr->child[che_arg2], 1);
		if (child && is_host_type(child, ind))
			return 1;
		return 0;

	case SIZEOF_TYPE:
	case SIZEOF:
		return 0;

	case ID:
	case '.':
	case PTR_OP:
		if (! (var = varscope_expr(expr)) )
			return 1;
		type = first_node(&var->child[chv_type]);
		break;

	case TYPECAST:
		type = first_node(&expr->child[che_arg1]);
		break;

	case FUNC:
		child = first_node(&expr->child[che_arg1]);
		if (! (var = varscope_expr(child)) )
			/* Standard function prototypes are not part of
			 * the parse tree, but they cannot be changed
			 * anyway, so treat them as host-specific. */
			return 1;

		type = first_node(&var->child[chv_type]);
		assert(type->t.category == type_func);

		/* Macros have no return type => be pessimistic */
		if (! (type = nth_element(&var->child[chv_type], 1)) )
			return 1;

		break;

	default:
		fprintf(stderr, "%s: Operator %d not yet handled\n",
			__FUNCTION__, expr->e.op);
		return 1;	/* be pessimistic */
	}

	if (! (type = ind_base_type(type, ind)) )
		return 1;	/* not found - e.g. NULL */

	if (type->t.category == type_pointer)
		return 1;

	return 0;
}

/* Check whether the result of an arithmetic operation on @expr (which
 * is known to be a target type) is still a target type.
 */
static int
is_target_arith(node_t *expr, node_t *parent)
{
	ind_t ind[MAXIND];
	node_t *sibling;

	if (is_child(expr, parent, che_arg1))
		sibling = first_node(&parent->child[che_arg2]);
	else
		sibling = first_node(&parent->child[che_arg1]);

	ind[0] = ind_stop;
	return !is_host_type(sibling, ind);
}

static void
track_assign(node_t *expr, ind_t *ind)
{
	node_t *target = first_node(&expr->child[che_arg1]);
	while (target->type == nt_expr &&
	       (target->e.op == '*' || target->e.op == ARRAY)) {
		target = first_node(&target->child[che_arg1]);
		*(++ind) = ind_pointer;
	}
	target = varscope_expr(target);
	if (target)
		subst_target_var(target, ind);
}

static void
track_assign2(node_t *expr, ind_t *ind)
{
	node_t *target = first_node(&expr->child[che_arg2]);
	while (target->type == nt_expr && target->e.op == '*') {
		target = first_node(&target->child[che_arg1]);
		*(++ind) = ind_pointer;
	}
	target = varscope_expr(target);
	if (target)
		subst_target_var(target, ind);
}

static void
track_return(node_t *node, ind_t *ind)
{
	node_t *fn;
	while ((fn = node->parent) &&
	       ! (fn->type == nt_decl &&
		  is_child(node, fn, chd_body)))
		node = fn;
	if (fn) {
		node_t *var = first_node(&fn->child[chd_var]);
		*(++ind) = ind_return;
		subst_target_var(var, ind);
	}
}

static int
try_track_args(node_t *arg, node_t *fn, ind_t *ind)
{
	int pos = child_order(arg, fn, che_arg2);
	if (!pos)
		return 0;

	node_t *var = varscope_expr(first_node(&fn->child[che_arg1]));
	for ( ; var; var = varscope_find_next(var)) {
		node_t *type = nth_element(&var->child[chv_type], 1);
		if (!type)
			continue;

		if (type->t.category == type_pointer)
			type = first_node(&type->child[cht_type]);

		node_t *argdecl = nth_element(&type->child[cht_param], pos);
		if (!argdecl)
			continue;

		node_t *node = first_node(&argdecl->child[chd_type]);
		if (&node->list != &argdecl->child[chd_type])
			subst_target_type(node, ind);

		node = first_node(&argdecl->child[chd_var]);
		if (&node->list != &argdecl->child[chd_var])
			subst_target_var(node, ind);
	}

	return 1;
}

/* Track uses of @expr with indirection level @ind. */
static void
track_expr(node_t *expr, ind_t *ind)
{
	ind_t saveind[2];
	int saveidx = 0;
	node_t *parent = expr->parent;

	assert(expr->type == nt_expr);

	switch(parent->type) {
	case nt_var:
		if (is_child(expr, parent, chv_init))
			subst_target_var(parent, ind);
		break;

	case nt_expr:
		if (expr->e.op == ID && ind_is_func(*ind))
			*++ind = ind_implicit;

		switch (parent->e.op) {

		case '&':
			if (!list_empty(&parent->child[che_arg2]))
				/* This is the binary '&' operator */
				break;

			if (*ind != ind_implicit)
				*++ind = ind_pointer;
			/* fall through */
		case INC_OP:
		case DEC_OP:
			track_expr(parent, ind);
			break;

		case ADD_ASSIGN:
		case SUB_ASSIGN:
			if (!is_child(expr, parent, che_arg1))
				break;
			/* else fall through */
		case '+':
		case '-':
			if (is_target_arith(expr, parent))
				track_expr(parent, ind);
			break;

		case FUNC:
			if (try_track_args(expr, parent, ind))
				break;

			if (ind_is_pointer(*ind))
				saveind[saveidx++] = *ind--;
			else
				ind_warn("non-pointer call", ind);

			if (*ind == ind_return &&
			    is_child(expr, parent, che_arg1)) {
				saveind[saveidx++] = *ind--;
				track_expr(parent, ind);
			}
			while (saveidx)
				*++ind = saveind[--saveidx];
			break;

		case '*':
			if (!list_empty(&parent->child[che_arg2]))
				/* This is the binary '*' operator */
				break;

			if (ind_is_pointer(*ind)) {
				saveind[saveidx++] = *ind--;
				track_expr(parent, ind);
				*++ind = saveind[--saveidx];
			} else
				ind_warn("expected pointer", ind);
			break;

		case '=':
			if (is_child(expr, parent, che_arg2))
				track_assign(parent, ind);
			else if (ind_is_pointer(*ind) && ind_is_func(ind[-1]))
				track_assign2(parent, ind);
			track_expr(parent, ind);
			break;

		case RETURN:
			track_return(parent, ind);
			break;
		}
		break;

	default:		/* Avoid compiler warnings */
		break;
	}
}

/* Track all uses of variable @var */
static void
track_var_usage(node_t *var, ind_t *ind)
{
	assert(var->type == nt_var);

	node_t *node;
	list_for_each_entry(node, &var->user_list, user_list)
		track_expr(node, ind);
}

/* Check whether @node is a function argument.
 * If so, add the argument index to @ind and return the function type.
 * If not, return NULL.
 */
static inline node_t *
check_func_arg(node_t *node, ind_t *ind)
{
	if (!node || node->type != nt_decl)
		return NULL;

	node_t *parent = node->parent;
	if (!is_function(parent))
		return NULL;

	int argidx = child_order(node, parent, cht_param);
	if (!argidx)
		return NULL;

	*ind = argidx;
	return parent;
}

/* Track one instance of the @type object. All containing variables
 * are tracked appropriately.
 */
static void
track_type(node_t *type)
{
	ind_t ind[MAXIND];
	int idx = 0;

	ind[idx] = ind_stop;
	if (type->t.category == type_func)
		ind[++idx] = ind_return;

	do {
		node_t *parent;
		while ((parent = type->parent)->type == nt_type) {
			if (parent->t.category == type_pointer ||
			    parent->t.category == type_array)
				ind[++idx] = ind_pointer;
			else if (parent->t.category == type_func)
				ind[++idx] = ind_return;
			else
				assert(0);

			type = parent;
		}

		if (parent->type == nt_var && parent->user_list.next) {
			track_var_usage(parent, ind + idx);
			type = parent->parent;
		} else
			type = parent;
	} while ((type = check_func_arg(type, &ind[++idx])) != NULL);
}

static void
track_vars(struct list_head *filelist)
{
	struct parsed_file *pf;

	list_for_each_entry(pf, filelist, list)
		walk_tree(&pf->parsed, build_scopes, NULL);

	/* This initializes @split to an invalid address, but
	 * it can be used to "continue" the walk at the beginning
	 * of @splitlist
	 */
	struct split_node *split = list_entry(&splitlist,
					      struct split_node, list);
	do {
		node_t *type;
		list_for_each_entry(type, &replacedlist, user_list)
			track_type(type);
		INIT_LIST_HEAD(&replacedlist);

		list_for_each_entry_continue(split, &splitlist, list) {
			list_for_each_entry(type, &split->nodes, user_list)
				track_type(type);
		}
		split = list_entry(split->list.prev, struct split_node, list);
	} while (!list_empty(&replacedlist));
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

	fill_varscope(filelist);
	list_for_each_entry(pf, filelist, list)
		walk_tree(&pf->parsed, xform_fn, pf);

	track_vars(filelist);

	list_for_each_entry(pf, filelist, list) {
		struct split_node *split, *nsplit;
		list_for_each_entry_safe(split, nsplit, &splitlist, list) {
			type_split(&pf->raw, split);
			split_remove(split);
		}
	}
	INIT_LIST_HEAD(&replacedlist);

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

{ "fix-bt-g.patch", import },

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

// Prepare for arch-specific machspec
{ "arch-machspec-split-decl.patch", import },

// Give each architecture its own name for machspec
{ "arch-machspec-split-use.patch", simple, arch_machspec },

// Target types in calls to (try_)get_symbol_data
{ "target-types-symbol_data.patch", type_subst, target_types_symbol_data },

// Target types in calls to readmem
{ "target-types-readmem.patch", type_subst, target_types_readmem },

// Target facilitator macros
{ "target-facilitators.patch", type_subst, target_facilitators },

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
