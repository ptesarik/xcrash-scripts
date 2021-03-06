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

struct list_head *
find_var_scope(node_t *node)
{
	struct parsed_file *pf = node->loc.first.pf;
	struct list_head *tree = &pf->parsed;
	struct list_head *scope = find_scope(node, NULL);
	if (scope == tree && is_header_file(pf))
		return NULL; /* global scope */
	return scope;
}

static void
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

static enum walk_action
fill_varscope(node_t *node, void *data)
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
init_varscope(const struct file_array *files)
{
	struct parsed_file *pf;
	free_varscope();
	files_for_each(pf, files)
		walk_tree(&pf->parsed, fill_varscope, NULL);
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

/* varscope_find - find an identifier in a given scope
 *
 * @scope	the variable scope
 * @type	type of node to search for
 * @idname	name of the identifier
 */
node_t *
varscope_find(struct list_head *scope,
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

/* varscope_find_next - find the next identifier in a given scope
 *
 * @scope	the variable scope
 * @start	the starting point
 */
node_t *
varscope_find_next(struct list_head *scope, const node_t *start)
{
	const char *idname = start->str->text;
	unsigned idx = mkhash(idname, scope);
	struct varscope *vs;
	for (vs = vshash[idx]; vs; vs = vs->next)
		if (vs->node == start)
			break;
	if (!vs)
		return NULL;

	while ( (vs = vs->next) )
		if (vs->scope == scope &&
		    vs->node->type == start->type &&
		    !strcmp(vs->node->str->text, idname))
			return vs->node;

	return NULL;
}

/* varscope_traverse - traverse the scopes to find an identifier
 *
 * @tree	tree corresponding to the file where @idname is used
 * @scopenode	determines the initial scope - use any node inside the
 *		desired initial scope, e.g. the variable itself
 * @type	type of node to search for
 * @idname	name of the identifier
 */
node_t *
varscope_traverse(struct list_head *tree, node_t *scopenode,
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
		if ( (ret = varscope_find(scope, type, idname)) )
			return ret;
	} while (scope);
	return NULL;
	
}

void
varscope_remove(node_t *node)
{
	struct list_head *scope = find_var_scope(node);
	unsigned idx = mkhash(node->str->text, scope);
	struct varscope *vs, **prev = &vshash[idx];
	while (*prev) {
		vs = *prev;
		if (vs->node == node) {
			*prev = vs->next;
			break;
		}
		prev = &vs->next;
	}
}

node_t *
varscope_find_first_var(node_t *var)
{
	struct list_head *scope = find_var_scope(var);
	return varscope_find(scope, var->type, var->str->text);
}

node_t *
varscope_find_next_var(node_t *var)
{
	struct list_head *scope = find_var_scope(var);
	node_t *ret = varscope_find_next(scope, var);
	if (!ret && scope == &var->loc.first.pf->parsed)
		ret = varscope_find(NULL, var->type, var->str->text);
	return ret;
}

node_t *
resolve_typedef(struct list_head *tree, node_t *type)
{
	while (type->type == nt_type && type->t.category == type_typedef) {
		node_t *var = varscope_traverse(tree, type,
						nt_var, type->str->text);
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
		if (! (var = varscope_find_expr(var)) )
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
		if (! (var = varscope_find_expr(expr)) )
			return NULL;
		assert(var->type == nt_var);

		/* Convert it to a type */
		return !list_empty(&var->child[chv_type])
			? first_node(&var->child[chv_type])
			: NULL;	/* unspecified type */
	}
}

node_t *
varscope_find_expr(node_t *expr)
{
	struct list_head *tree = &expr->loc.first.pf->parsed;
	node_t *left, *right, *type;
	node_t *ret;

	if (expr->type != nt_expr)
		return NULL;

	switch (expr->e.op) {
	case ID:
		ret = varscope_traverse(tree, expr, nt_var, expr->str->text);
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
		    ! (type = varscope_traverse(tree, type,
						nt_type, type->str->text)) )
			return NULL;

		/* Now, find the member in the struct */
		right = first_node(&expr->child[che_arg2]);
		ret = varscope_find(&type->child[cht_body],
				    nt_var, right->str->text);
		break;

	default:
		return NULL;
	}

	node_t *expr_cond = expr->loc.first.text->cpp_cond;
	while (ret) {
		node_t *ret_cond = ret->loc.first.text->cpp_cond;
		if (!cond_is_disjunct(expr_cond, ret_cond))
			break;

		ret = varscope_find_next_var(ret);
	}

	return ret;
}

static node_t *
find_global(const struct file_array *files, struct list_head *scope,
	    enum node_type type, const char *idname)
{
	node_t *ret = varscope_find(scope, type, idname);
	if (!ret && scope)
		ret = varscope_find(NULL, type, idname);
	if (ret)
		return ret;

	struct parsed_file *pf;
	files_for_each(pf, files)
		if ( (ret = varscope_find(&pf->parsed, type, idname)) )
			break;
	return ret;
}

static size_t
check_prefix(const char *spec, enum node_type *nt, int *extra)
{
#define PREFIX(name,nt,cat)	{ name " ", sizeof(name), (nt), (cat) }
	static const struct {
		const char *prefix;
		size_t prefix_len;
		enum node_type nt;
		int extra;
	} prefixes[] = {
		PREFIX("var", nt_var, -1),
		PREFIX("typedef", nt_var, TF_TYPEDEF),
		PREFIX("type", nt_type, -1),
		PREFIX("struct", nt_type, type_struct),
		PREFIX("union", nt_type, type_union),
		PREFIX("enum", nt_type, type_enum),
		{ NULL }
	}, *p;

	*nt = *extra = -1;
	for (p = prefixes; p->prefix; ++p)
		if (!strncmp(spec, p->prefix, p->prefix_len)) {
			*nt = p->nt;
			*extra = p->extra;
			return p->prefix_len;
		}

	return 0;
}

static node_t *
find_symbol(const struct file_array *files, struct list_head *scope,
	    const char *spec)
{
	enum node_type nodetype;
	int extra;
	spec += check_prefix(spec, &nodetype, &extra);

	node_t *node = (nodetype != -1)
		? find_global(files, scope, nodetype, spec)
		: (find_global(files, scope, nt_var, spec)
		   ?: find_global(files, scope, nt_type, spec));

	while (node) {
		if (extra == -1)
			return node;

		if (node->type == nt_type && node->t.category == extra)
			return node;

		if (node->type == nt_var) {
			node_t *type = first_node(&node->child[chv_type]);
			assert(&type->list != &node->child[chv_type]);
			assert(type->type == nt_type);
			if (type->t.flags & extra)
				return node;
		}
		node = varscope_find_next(scope, node);
	}
	return NULL;
}

/* varscope_symbol - find a symbol in the varscope database
 *
 * @files	array of all parsed_file structures
 * @name	name of the symbol
 *
 * The @name is in fact a "path" to the symbol. The general syntax is:
 *
 * [<file_name>:][<tag> ]name[.[<tag> ]name]...
 *
 * where <tag> specifies the type of the symbol and can be one of:
 *   var, typedef, type, struct, union, enum
 */
node_t *
varscope_symbol(const struct file_array *files, const char *name)
{
	struct list_head *tree;
	char localname[strlen(name) + 1];
	char *spec, *sep;

	spec = localname;
	strcpy(spec, name);

	if ( (sep = strchr(spec, ':')) ) {
		struct parsed_file *pf;

		*sep = 0;
		if (! (pf = find_file(files, spec)) )
			return NULL;
		tree = &pf->parsed;
	} else {
		sep = spec - 1;
		tree = NULL;
	}

	struct list_head *scope = tree;
	node_t *node = NULL;
	do {
		spec = sep + 1;
		if ( (sep = strchr(spec, '.')) )
			*sep = 0;

		if (node)
			scope = node_scope(node);

		if (! (node = find_symbol(files, scope, spec)) )
			return NULL;
		if (! (node = resolve_typedef(tree, node)) )
			return NULL;
	} while (sep && sep[1]);

	return node;
}
