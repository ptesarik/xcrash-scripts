#ifndef VARSCOPE_H
#define VARSCOPE_H

#include "parser.h"

void init_varscope(struct list_head *filelist);
void free_varscope(void);

node_t *varscope_find(struct list_head *scope,
		      enum node_type type, const char *idname);
node_t *varscope_find_next(struct list_head *scope, const node_t *start);
node_t *varscope_traverse(struct list_head *tree, node_t *scopenode,
			  enum node_type type, const char *idname);
void varscope_remove(node_t *node);

node_t *varscope_find_first_var(node_t *var);
node_t *varscope_find_next_var(node_t *var);
node_t *varscope_find_expr(node_t *expr);

struct list_head *find_var_scope(node_t *node);
node_t *resolve_typedef(struct list_head *tree, node_t *type);

node_t *varscope_symbol(struct list_head *filelist, const char *name);

#endif	/* VARSCOPE_H */
