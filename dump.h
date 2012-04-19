/* Data dumping facility */

#ifndef DUMP_H
#define DUMP_H

extern FILE *fdump;

void dump_text(struct dynstr *first, struct dynstr *last);
void dump_tree(struct list_head *tree);

#endif	/* DUMP_H */
