#include <string.h>
#include <stdlib.h>
#include "parser.h"
#include "clang.tab.h"

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

/* Simple (linear) transformations go here */
static int simple_xform(node_t *item, void *data)
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

/************************************************************
 * Main entry point for the transformations
 *
 */
void
xform_files(struct list_head *filelist)
{
	struct parsed_file *pf;

	list_for_each_entry(pf, filelist, list) {
		node_t *tree = pf->parsed;

		/* convert mkstring() to a variadic function */
		walk_tree(tree, mkstring_variadic, NULL);

		/* Do the simple transformations */
		walk_tree(tree, simple_xform, NULL);
	}
}
