#include "parser.h"

/* CPP conditions */
int vcheck_cpp_cond(node_t *node, const char **set, const char **unset);
int check_cpp_cond(node_t *node, ...);

/* Related to raw contents */
int dynstr_isspace(struct dynstr *ds);
struct dynstr *dynstr_delspace(struct list_head *list, struct dynstr *ds);
struct dynstr *dynstr_delspace_rev(struct list_head *list, struct dynstr *ds);
void dynstr_bol(struct list_head *list, struct dynstr **ds, size_t *pos);

static inline struct dynstr *
dynstr_del(struct dynstr *ds)
{
	struct dynstr *ret = next_dynstr(ds);
	list_del_init(&ds->list);
	freedynstr(ds);
	return ret;
}

static inline struct dynstr *
dynstr_del_rev(struct dynstr *ds)
{
	struct dynstr *ret = prev_dynstr(ds);
	list_del_init(&ds->list);
	freedynstr(ds);
	return ret;
}

/* Related to the parsed tree */
struct list_head *find_scope(struct list_head *tree, node_t *node);
void nullify_str(node_t *node);

/* Split nodes */
struct split_node {
	struct list_head list;
	struct dynstr *oldds, *newds;
	struct list_head nodes;
};

struct split_node *split_add(struct list_head *splitlist, node_t *node,
			     struct dynstr *oldds, struct dynstr *newds);
void split_remove(struct split_node *split);
struct split_node *split_search(struct list_head *splitlist,
				struct dynstr *ds, const char *newtext);

/* Add @node to the list of nodes for @split */
static inline void
split_addnode(struct split_node *split, node_t *node)
{
	list_move_tail(&node->list, &split->nodes);
}


/* Quilt interface */
int quilt_new(const char *name, struct list_head *filelist);
int quilt_import(const char *name, const char *basedir);
