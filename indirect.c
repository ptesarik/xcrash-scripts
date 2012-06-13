#define _GNU_SOURCE
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "indirect.h"
#include "varscope.h"
#include "tools.h"
#include "clang.tab.h"

node_t *
build_ind(node_t *type, ind_t **indp)
{
	assert(type->type == nt_type);

	node_t *parent;
	while ((parent = type->parent)->type == nt_type) {
		ind_t ind;
		if (parent->t.category == type_pointer ||
		    parent->t.category == type_array)
			ind = ind_pointer;
		else if (parent->t.category == type_func)
			ind = ind_return;
		else
			assert(0);

		if (indp)
			*(--*indp) = ind;
		type = parent;
	}
	return parent;
}

static node_t *
array_base_type(node_t *type)
{
	return (type->t.category == type_pointer ||
		type->t.category == type_array)
		? first_node(&type->child[cht_type])
		: NULL;
}

static node_t *
func_return_type(node_t *type)
{
	return (type->t.category == type_func)
		? first_node(&type->child[cht_type])
		: NULL;
}

static node_t *
kr_param_type(node_t *fntype, const char *name)
{
	node_t *fnvar, *fndecl;
	if (! (fnvar = fntype->parent) )
		return NULL;
	if (! (fndecl = fnvar->parent) )
		return NULL;

	node_t *var = varscope_find(&fndecl->child[chd_body], nt_var, name);
	if (!var)
		return NULL;
	return nth_node(&var->child[chv_type], 1);
}

static node_t *
func_arg_type(node_t *type, int pos)
{
	node_t *decl = nth_node(&type->child[cht_param], pos);
	if (!decl)
		return NULL;
	assert(decl->type == nt_decl);

	type = first_node(&decl->child[chd_type]);
	if (&type->list != &decl->child[chd_type])
		return type;

	node_t *var = first_node(&decl->child[chd_var]);
	assert(&var->list != &decl->child[chd_var]);
	type = first_node(&var->child[chv_type]);
	if (&type->list != &var->child[chv_type])
		return type;

	if ( (type = kr_param_type(decl->parent, var->str->text)) )
		return type;

	return NULL;
}

/* Return the base type of @type according to the instructions in @ind */
node_t *
ind_base_type(node_t *type, const ind_t *ind)
{
	while (*ind != ind_stop) {
		if (ind_is_pointer(*ind))
			type = array_base_type(type);
		else if (*ind == ind_return)
			type = func_return_type(type);
		else {
			assert(ind > 0);
			type = func_arg_type(type, *ind);
		}
		if (!type)
			break;

		++ind;
	}

	return type;
}

/* Check whether @expr refers to host-specific data */
int
is_host_type(node_t *expr, ind_t *ind)
{
	node_t *child, *var, *type;

	switch (expr->e.op) {
	case INT_CONST:
	case FLOAT_CONST:
	case CHAR_CONST:
		return 0;

	case '!':
	case STRING_CONST:
		return 1;

	case ADDR_OF:
		child = first_node(&expr->child[che_arg1]);
		if (!ind_is_pointer(*ind))
			return 1;
		++ind;
		return is_host_type(child, ind);

	case DEREF_OP:
	case ARRAY:
		child = first_node(&expr->child[che_arg1]);
		*--ind = ind_pointer;
		return is_host_type(child, ind);

	case '~':
	case '/':
	case '%':
	case SHL_OP:
	case SHR_OP:
		child = first_node(&expr->child[che_arg1]);
		return is_host_type(child, ind);

	case '&':
	case '|':
	case '^':
	case '*':
	case '+':
	case '-':
		child = nth_node(&expr->child[che_arg1], 1);
		if (child && is_host_type(child, ind))
			return 1;

		child = nth_node(&expr->child[che_arg2], 1);
		if (child && is_host_type(child, ind))
			return 1;
		return 0;

	case '?':
		child = nth_node(&expr->child[che_arg3], 1);
		if (child && is_host_type(child, ind))
			return 1;

		child = nth_node(&expr->child[che_arg2], 1);
		if (child && is_host_type(child, ind))
			return 1;
		return 0;

	case SIZEOF_TYPE:
	case SIZEOF:
		return 0;

	case ID:
	case '.':
	case PTR_OP:
		if (! (var = varscope_find_expr(expr)) )
			return 1;
		type = first_node(&var->child[chv_type]);
		break;

	case TYPECAST:
		type = first_node(&expr->child[che_arg1]);
		break;

	case FUNC:
		child = first_node(&expr->child[che_arg1]);
		if (! (var = varscope_find_expr(child)) )
			/* Standard function prototypes are not part of
			 * the parse tree, but they cannot be changed
			 * anyway, so treat them as host-specific. */
			return 1;

		type = first_node(&var->child[chv_type]);
		assert(type->t.category == type_func);
		if (! (type = nth_node(&type->child[cht_type], 1)) )
			return 1;

		break;

	default:
		fprintf(stderr, "%s: Operator %d not yet handled\n",
			__FUNCTION__, expr->e.op);
		return 1;	/* be pessimistic */
	}

	if (! (type = ind_base_type(type, ind)) )
		return 1;	/* not found - e.g. NULL */

	if (type->t.category == type_pointer ||
	    type->t.category == type_array)
		return 1;

	return 0;
}
