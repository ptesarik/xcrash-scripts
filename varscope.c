#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdint.h>

#include "tools.h"
#include "varscope.h"
#include "clang.tab.h"

#define HASH_SIZE	8192

struct varscope {
	struct varscope *next;
	node_t *node;		/* var or type */
	struct list_head *scope;
};

static struct varscope *vshash[HASH_SIZE];

static unsigned
mkhash(const char *name, void *scope)
{
	const char *p;
	unsigned ret = (uintptr_t)scope;
	for (p = name; *p; ++p)
		ret = ret * 13 + *p;
	return ret % HASH_SIZE;
}

/* Returns non-zero iff:
 *   a. the file name of @pf ends with a ".h", or
 *   b. it is the built-in pseudo-file
 */
static int
is_header_file(struct parsed_file *pf)
{
	if (pf->name) {
		char *suffix = strrchr(pf->name, '.');
		return suffix && !strcmp(suffix, ".h");
	} else
		return 1;
}

static struct list_head *
find_var_scope(node_t *node)
{
	struct parsed_file *pf = node->pf;
	struct list_head *tree = &pf->parsed;
	struct list_head *scope = find_scope(node, NULL);
	if (scope == tree && is_header_file(pf))
		return NULL; /* global scope */
	return scope;
}

void
varscope_add(node_t *node)
{
	struct list_head *scope = find_var_scope(node);
	struct varscope *vs = malloc(sizeof(struct varscope));
	unsigned idx = mkhash(node->str->text, scope);

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
	unsigned idx = mkhash(idname, scope);
	for (vs = vshash[idx]; vs; vs = vs->next)
		if (vs->scope == scope &&
		    vs->node->type == type &&
		    !strcmp(vs->node->str->text, idname))
			return vs->node;
	return NULL;
}

/* do_find - perform a node search
 *
 * @tree	tree corresopnding to the file where @idname is used
 * @scopenode	determines the initial scope - use any node inside the
 *		desired initial scope, e.g. the variable itself
 * @type	type of node to search for
 * @idname	name of the identifier
 */
static node_t *
do_find(struct list_head *tree, node_t *scopenode,
	enum node_type type, const char *idname)
{
	node_t *ret;
	struct list_head *scope;
	do {
		if (scopenode)
			scope = find_scope(scopenode, &scopenode);
		else {
			scope = tree;
			tree = NULL;
		}
		if ( (ret = do_find_one(scope, type, idname)) )
			return ret;
	} while (scope);
	return NULL;
	
}

node_t *
varscope_find(node_t *node, enum node_type type)
{
	return do_find(&node->pf->parsed, node, type, node->str->text);
}

node_t *
varscope_find_next(node_t *node)
{
	struct list_head *scope = find_var_scope(node);
	const char *idname = node->str->text;
	unsigned idx = mkhash(idname, scope);
	struct varscope *vs;
	for (vs = vshash[idx]; vs; vs = vs->next)
		if (vs->node == node)
			break;
	if (!vs)
		return NULL;

	while ( (vs = vs->next) )
		if (vs->scope == scope &&
		    vs->node->type == node->type &&
		    !strcmp(vs->node->str->text, idname))
			return vs->node;

	if (scope == &node->pf->parsed)
		return do_find_one(NULL, node->type, idname);

	return NULL;
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
	if (node->type == nt_var && node->str)
		varscope_add(node);
	else if (node->type == nt_type && node->str &&
		 (node->t.category == type_struct ||
		  node->t.category == type_union ||
		  node->t.category == type_enum) &&
		 !list_empty(&node->child[cht_body]))
		varscope_add(node);

	return walk_continue;
}

void
fill_varscope(struct list_head *filelist)
{
	struct parsed_file *pf;
	free_varscope();
	list_for_each_entry(pf, filelist, list)
		walk_tree(&pf->parsed, fill_varscope_fn, NULL);
}

static node_t *
resolve_typedef(struct list_head *tree, node_t *type)
{
	while (type->type == nt_type && type->t.category == type_typedef) {
		node_t *var = do_find(tree, type, nt_var, type->str->text);
		if (!var)
			return NULL;
		type = first_node(&var->child[chv_type]);
		if (! (type->t.flags & TF_TYPEDEF))
			return NULL;
	}
	return type;
}

/* expr_type - get the type declaration for @expr
 *
 * Note that the resulting node may be in a different file than @expr.
 */
static node_t *
expr_type(node_t *expr)
{
	node_t *var, *type;

	assert(expr->type == nt_expr);
	if (expr->e.op == ARRAY) {
		/* Find the array/pointer variable */
		var = first_node(&expr->child[che_arg1]);
		if (! (var = varscope_expr(var)) )
			return NULL;
		assert(var->type == nt_var);

		/* Convert it to a type */
		if (list_empty(&var->child[chv_type]))
			return NULL; /* unspecified type */
		type = first_node(&var->child[chv_type]);
		assert(type->type == nt_type);

		/* Get the base type */
		assert(type->t.category == type_array ||
		       type->t.category == type_pointer);
		return !list_empty(&type->child[cht_type])
			? first_node(&type->child[cht_type])
			: NULL;	/* unspecified type */
	} else {
		/* Find the variable */
		if (! (var = varscope_expr(expr)) )
			return NULL;
		assert(var->type == nt_var);

		/* Convert it to a type */
		return !list_empty(&var->child[chv_type])
			? first_node(&var->child[chv_type])
			: NULL;	/* unspecified type */
	}
}

node_t *
varscope_expr(node_t *expr)
{
	struct list_head *tree = &expr->pf->parsed;
	node_t *left, *right, *type;
	node_t *ret;

	if (expr->type != nt_expr)
		return NULL;

	switch (expr->e.op) {
	case ID:
		ret = do_find(tree, expr, nt_var, expr->str->text);
		break;

	case '.':
	case PTR_OP:
		/* Find the type of the left side */
		if (list_empty(&expr->child[che_arg1]))
			return NULL; /* named struct member initializers */
		left = first_node(&expr->child[che_arg1]);
		if (! (type = expr_type(left)) )
			return NULL;
		assert(type->type == nt_type);

		/* Take the base type of a pointer for "->" */
		if (expr->e.op == PTR_OP) {
			if (! (type = resolve_typedef(tree, type)) )
				return NULL;
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
		    ! (type = do_find(tree, type, nt_type, type->str->text)) )
			return NULL;

		/* Now, find the member in the struct */
		right = first_node(&expr->child[che_arg2]);
		ret = do_find_one(&type->child[cht_body],
				  nt_var, right->str->text);
		break;

	default:
		return NULL;
	}

	node_t *expr_cond = expr->first_text->cpp_cond;
	while (ret) {
		node_t *ret_cond = ret->first_text->cpp_cond;
		if (expr_cond && ret_cond &&
		    expr_cond != ret_cond) {
			node_t *op = dupnode_nochild(expr_cond);
			op->type = nt_expr;
			op->e.op = AND_OP;
			set_node_child(op, che_arg1, expr_cond);
			set_node_child(op, che_arg2, ret_cond);

			struct truth_table *ttbl = cpp_truth_table(op);
			int cont = is_always_false(ttbl);

			free(ttbl);
			list_del_init(&expr_cond->list);
			list_del_init(&ret_cond->list);
			freenode(op);
			if (!cont)
				break;

			ret = varscope_find_next(ret);
		} else
			break;
	}

	return ret;
}

node_t *
varscope_type(node_t *scope, const char *name)
{
	struct list_head *tree = scope ? &scope->pf->parsed : NULL;;
	char localname[strlen(name) + 1];
	char *spec, *dot;
	node_t *node;

	spec = localname;
	strcpy(spec, name);

	if ( (dot = strchr(spec, '.')) )
		*dot = 0;

	if (! (node = do_find(tree, scope, nt_type, spec)) ) {
		if (! (node = do_find(tree, scope, nt_var, spec)) )
			return NULL;
		if (list_empty(&node->child[chv_type]))
			return NULL; /* unspecified type */
		node = first_node(&node->child[chv_type]);
	}

	if (! (node = resolve_typedef(tree, node)) )
		return NULL;

	while (dot && dot[1]) {
		spec = dot + 1;
		if ( (dot = strchr(spec, '.')) )
			*dot = 0;

		scope = node;
		assert(scope->type == nt_type);
		if (scope->t.category != type_struct &&
		    scope->t.category != type_union &&
		    scope->t.category != type_enum)
			return NULL;

		node = do_find_one(&scope->child[cht_body], nt_var, spec);
		if (!node)
			return NULL;
		if (list_empty(&node->child[chv_type]))
			return NULL; /* unspecified type */
		node = first_node(&node->child[chv_type]);
	}

	return node;
}
