/* Data dumping facility */

#ifndef DUMP_H
#define DUMP_H

#include "parser.h"
#include "indirect.h"

extern FILE *fdump;

void dump_text(struct dynstr *first, struct dynstr *last);
void dump_tree(struct list_head *tree);

void shortdump_type(node_t *type);
void shortdump_var(node_t *var);

void dump_ind(const ind_t *ind);

#endif	/* DUMP_H */
