#include <string.h>
#include "parser.h"

typedef int walkfn(node_t *, void *);
static void walk_tree(node_t *tree, walkfn *fn, void *data);

static void
walk_tree_rec(node_t *tree, walkfn *fn, void *data)
{
	node_t *item = tree;
	do {
		if (fn(item, data))
			break;
		int i;
		for (i = 0; i < item->nchild; ++i)
			if (item->child[i])
				walk_tree_rec(item->child[i], fn, data);
		item = list_entry(item->list.next, node_t, list);
	} while (item != tree);
}

static void
walk_tree(node_t *tree, walkfn *fn, void *data)
{
	if (!tree)
		return;
	walk_tree_rec(tree, fn, data);
}

static void
replace_text(node_t *node, const char *text)
{
	struct dynstr *ds = newdynstr(text, strlen(text));
	replace_text_list(node->first_text, node->last_text, ds, ds);

	node->first_text = node->last_text = ds;
}

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
 * Main entry point for the transformations
 *
 */
void
xform_tree(node_t *tree)
{
	/* Do the simple transformations */
	walk_tree(tree, simple_xform, NULL);
}
