#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "tools.h"
#include "varscope.h"
#include "clang.tab.h"

#define HASH_SIZE	4096

struct varscope {
	struct varscope *next;
	node_t *node;		/* var or type */
	struct list_head *scope;
};

static struct varscope *vshash[HASH_SIZE];

static unsigned
mkhash(const char *name)
{
	const char *p;
	unsigned ret = 0;
	for (p = name; *p; ++p)
		ret = ret * 13 + *p;
	return ret % HASH_SIZE;
}

void
varscope_add(struct list_head *tree, node_t *node)
{
	struct list_head *scope = find_scope(tree, node);
	struct varscope *vs = malloc(sizeof(struct varscope));
	unsigned idx = mkhash(node->str->text);

	vs->next = vshash[idx];
	vs->node = node;
	vs->scope = scope;
	vshash[idx] = vs;
}

static node_t *
do_find_one(struct list_head *scope,
	    enum node_type type, const char *idname)
{
	struct varscope *vs;
	unsigned idx = mkhash(idname);
	for (vs = vshash[idx]; vs; vs = vs->next)
		if (vs->scope == scope &&
		    vs->node->type == type &&
		    !strcmp(vs->node->str->text, idname))
			return vs->node;
	return NULL;
}

static node_t *
do_find(struct list_head *tree, node_t *scopenode,
	enum node_type type, const char *idname)
{
	node_t *ret;
	struct list_head *scope;
	do {
		scope = find_scope(tree, scopenode);
		scopenode = first_node(scope)->parent;
		if ( (ret = do_find_one(scope, type, idname)) )
			return ret;
	} while (scope != tree);
	return NULL;
	
}

node_t *
varscope_find(struct list_head *tree, node_t *node)
{
	return do_find(tree, node, node->type, node->str->text);
}

void
free_varscope(void)
{
	unsigned idx;
	for (idx = 0; idx < HASH_SIZE; ++idx) {
		struct varscope *vs, *vsnext;
		for (vs = vshash[idx]; vs; vs = vsnext) {
			vsnext = vs->next;
			free(vs);
		}
		vshash[idx] = NULL;
	}
}


static enum walk_action
fill_varscope_fn(node_t *node, void *data)
{
	struct parsed_file *pf = data;

	if (node->type == nt_var && node->str)
		varscope_add(&pf->parsed, node);
	else if (node->type == nt_type && node->str &&
		 (node->t.category == type_struct ||
		  node->t.category == type_union ||
		  node->t.category == type_enum) &&
		 !list_empty(&node->child[cht_body]))
		varscope_add(&pf->parsed, node);

	return walk_continue;
}

void
fill_varscope(struct list_head *filelist)
{
	struct parsed_file *pf;
	free_varscope();
	list_for_each_entry(pf, filelist, list)
		walk_tree(&pf->parsed, fill_varscope_fn, pf);
}

node_t *
resolve_typedef(struct list_head *tree, node_t *type)
{
	while (type->type == nt_type && type->t.category == type_typedef) {
		node_t *var = do_find(tree, type, nt_var, type->str->text);
		if (!var)
			return NULL;
		type = first_node(&var->child[chv_type]);
	}
	return type;
}

node_t *
varscope_expr(struct list_head *tree, node_t *expr)
{
	node_t *left, *right;

	if (expr->type != nt_expr)
		return NULL;

	switch (expr->e.op) {
	case ID:
		return do_find(tree, expr, nt_var, expr->str->text);

	case '.':
	case PTR_OP:
		/* Find the left-side variable */
		left = first_node(&expr->child[che_arg1]);
		if (! (left = varscope_expr(tree, left)) )
			return NULL;
		assert(left->type == nt_var);

		/* Convert it to a type */
		node_t *type = first_node(&left->child[chv_type]);
		assert(type->type == nt_type);

		/* Take the base type of a pointer for "->" */
		if (expr->e.op == PTR_OP) {
			assert(type->t.category == type_pointer);
			type = first_node(&type->child[che_arg1]);
		}

		/* If type is a typedef, find its struct type */
		if (! (type = resolve_typedef(tree, type)) )
			return NULL;
		assert (type->t.category == type_struct ||
			type->t.category == type_union ||
			type->t.category == type_enum);

		/* If we only got the struct name, get its declaration */
		if (list_empty(&type->child[cht_body]) &&
		    ! (type = varscope_find(tree, type)) )
			return NULL;

		/* Now, find the member in the struct */
		right = first_node(&expr->child[che_arg2]);
		return do_find_one(&type->child[cht_body],
				   nt_var, right->str->text);

	default:
		return NULL;
	}
}