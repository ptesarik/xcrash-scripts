#ifndef VARSCOPE_H
#define VARSCOPE_H

#include "parser.h"

void varscope_add(struct list_head *tree, node_t *var);
node_t *varscope_find(struct list_head *tree, node_t *idexpr);
void fill_varscope(struct list_head *filelist);
void free_varscope(void);

node_t *varscope_expr(struct list_head *tree, node_t *expr);

#endif	/* VARSCOPE_H */