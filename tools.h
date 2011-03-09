#include "parser.h"

/* CPP conditions */
int vcheck_cpp_cond(node_t *node, const char **set, const char **unset);
int check_cpp_cond(node_t *node, ...);

/* Related to the parsed tree */
struct list_head *find_scope(struct list_head *tree, node_t *node);

/* Quilt interface */
int quilt_new(const char *name, struct list_head *filelist);
int quilt_import(const char *name, const char *basedir);
