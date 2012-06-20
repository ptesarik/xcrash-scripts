#ifndef TOOLS_H
#define TOOLS_H

#include <stdlib.h>

#include "parser.h"

/* CPP conditions */
int vcheck_cpp_cond(node_t *node, const char **set, const char **unset);
int check_cpp_cond(node_t *node, ...);

struct truth_table {
	unsigned n, tblsize;
	const char **names;
	unsigned long tbl[];
};

static inline void
set_minterm(struct truth_table *tbl, unsigned long n)
{
	size_t idx = n/(8*sizeof(long));
	unsigned bit = n%(8*sizeof(long));
	tbl->tbl[idx] |= 1<<bit;
}

static inline void
clear_minterm(struct truth_table *tbl, unsigned long n)
{
	size_t idx = n/(8*sizeof(long));
	unsigned bit = n%(8*sizeof(long));
	tbl->tbl[idx] &= ~(1<<bit);
}

static inline int
test_minterm(const struct truth_table *tbl, unsigned long n)
{
	size_t idx = n/(8*sizeof(long));
	unsigned bit = n%(8*sizeof(long));
	return tbl->tbl[idx] & (1<<bit);
}

struct truth_table *cpp_truth_table(node_t *node);
void dump_truth_table(const struct truth_table *tbl);

static inline void
free_truth_table(struct truth_table *tbl)
{
	free(tbl);
}

static inline int
is_always_false(const struct truth_table *tbl)
{
	int i;
	for (i = 0; i < tbl->tblsize; ++i)
		if (tbl->tbl[i])
			return 0;
	return 1;
}

int cond_is_disjunct(node_t *a, node_t *b);

/* Related to raw contents */
int dynstr_isspace(struct dynstr *ds);
struct dynstr *dynstr_delspace(struct list_head *list, struct dynstr *ds);
struct dynstr *dynstr_delspace_rev(struct list_head *list, struct dynstr *ds);
void dynstr_bol(struct list_head *list, struct dynstr **ds, size_t *pos);
struct dynstr *dynstr_dup_indent(struct list_head *list,
				 struct dynstr *startds, size_t startoff);

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

void detach_text_list(struct dynstr *first, struct dynstr *last);
void unflag_text_list(struct dynstr *first, struct dynstr *last);
void remove_text_list(struct dynstr *first, struct dynstr *last);
void trim_text_list(struct dynstr *oldfirst, struct dynstr *oldlast,
		    struct dynstr *newfirst, struct dynstr *newlast);

void insert_text_list(struct dynstr *where,
		      struct dynstr *first, struct dynstr *last);
void replace_text_list(struct dynstr *oldfirst, struct dynstr *oldlast,
		       struct dynstr *newfirst, struct dynstr *newlast);


/* Related to the parsed tree */
void reset_user_data(struct list_head *tree);
struct list_head *node_scope(node_t *node);
struct list_head *find_scope(node_t *node, node_t **pparent);
void nullify_str(node_t *node);

/* Return the parent of @node with a given @type (or NULL if not found) */
static inline node_t *
typed_parent(node_t *node, enum node_type type)
{
	while (node && node->type != type)
		node = node->parent;
	return node;
}

/* Get the @pos-th node from @list */
node_t *nth_node(struct list_head *list, int pos);

/* Return the order of @child among @parent's children of @idx.
 * Returns zero if @child is not found.
 */
int child_order(node_t *child, node_t *parent, int idx);

/* Return non-zero if @child is an @idx-type child of @parent */
static inline int is_child(node_t *child, node_t *parent, int idx)
{
	return child_order(child, parent, idx) > 0;
}

/* Return the next duplicate of @node */
static inline node_t *
next_dup(node_t *node)
{
	return list_entry(node->dup_list.next, node_t, dup_list);
}

/* CPP conditions */
#define CPP_STACK_SIZE	32

struct cpp_cond_state {
	node_t *current;
	node_t *precond;
	unsigned stackptr;
	struct {
		node_t *node;
		node_t *precond;
	} stack[CPP_STACK_SIZE];
};

node_t *get_cpp_cond(struct cpp_cond_state *state, struct list_head *tree);

/* Remember the current last pointer on @list */
static inline node_t *
checkpoint_user_list(struct list_head *list)
{
	return list_entry(list->prev, node_t, user_list);
}

/* Remove all nodes from @list, starting at @checkpoint */
void rollback_user_list(struct list_head *list, node_t *checkpoint);

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
	list_add_tail(&node->user_list, &split->nodes);
}

/* Make a fake struct split pointer such that its newds field overlaps
   @node->str */
static inline struct split_node *
fake_split(node_t *node)
{
	return (struct split_node *)
		((char*)&node->str - offsetof(struct split_node, newds));
}

struct parsed_file *find_file(const struct file_array *files,
			      const char *name);

/* Quilt interface */
int quilt_new(const char *name);
int quilt_refresh(const struct file_array *files);
int quilt_import(const char *name, const char *basedir);
FILE *quilt_header(void);

#endif	/* TOOLS_H */
