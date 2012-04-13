#ifndef VARSCOPE_H
#define VARSCOPE_H

#include "parser.h"

void varscope_add(node_t *var);
node_t *varscope_find(node_t *idexpr, enum node_type type);
node_t *varscope_find_first(node_t *node);
node_t *varscope_find_next(node_t *node);
void fill_varscope(struct list_head *filelist);
void free_varscope(void);

node_t *varscope_expr(node_t *expr);

node_t *varscope_type(node_t *scope, const char *name);

#endif	/* VARSCOPE_H */
