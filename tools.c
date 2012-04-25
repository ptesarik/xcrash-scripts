#define _GNU_SOURCE
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <assert.h>

#include "tools.h"
#include "clang.tab.h"

/************************************************************
 * Useful helper functions
 *
 */

static int
list_count(struct list_head *list)
{
	struct list_head *iter;
	int ret = 0;
	list_for_each(iter, list)
		++ret;
	return ret;
}

/************************************************************
 * CPP conditions
 *
 */

/* Check whether a CPP condition is met. The condition is met if:
 * - at least one of the identifiers in @set is set
 * - none of the identifiers in @unset is set
 *
 * Returns:
 *   >0  if condition is met
 *   <0  if condition cannot be met
 *    0  if indeterminate (e.g. no condition at all)
 */
int
vcheck_cpp_cond(node_t *node, const char **set, const char **unset)
{
	const char **p;
	int left, right;

	if (!node)
		return 0;

	if (node->type != nt_expr) {
		fprintf(stderr, "%s: Cannot handle node type %d\n",
			__func__, node->type);
		exit(1);
	}

	switch (node->e.op) {
	case ID:
		for (p = unset; *p; ++p)
			if (!strcmp(*p, node->str->text))
				return -1;
		for (p = set; *p; ++p)
			if (!strcmp(*p, node->str->text))
				return 1;
		return 0;

	case '!':
		return -vcheck_cpp_cond(first_node(&node->child[che_arg1]),
					set, unset);

	case AND_OP:
		left = vcheck_cpp_cond(first_node(&node->child[che_arg1]),
				       set, unset);
		if (left < 0)
			return -1;
		right = vcheck_cpp_cond(first_node(&node->child[che_arg2]),
					set, unset);
		if (right < 0)
			return -1;
		return left + right;

	case OR_OP:
		left = vcheck_cpp_cond(first_node(&node->child[che_arg1]),
				       set, unset);
		if (left > 0)
			return 1;
		right = vcheck_cpp_cond(first_node(&node->child[che_arg2]),
					set, unset);
		if (right > 0)
			return 1;
		return left + right;

	case EQ_OP:
	case NE_OP:
		/* No idea about the value of preprocessor defines */
		return 0;

	default:
		fprintf(stderr, "%s: Operator %d not supported\n",
			__func__, node->e.op);
		exit(1);
	}
}

int
check_cpp_cond(node_t *node, ...)
{
	const char *name;
	const char **set, **unset;
	va_list va;
	int i, n;

	/* Count the arguments */
	va_start(va, node);
	n = 2;
	while (va_arg(va, const char *))
		++n;
	while (va_arg(va, const char *))
		++n;
	va_end(va);

	va_start(va, node);
	set = alloca(n * sizeof(const char *));
	for (i = 0; (name = va_arg(va, const char *)); ++i)
		set[i] = name;
	set[i++] = NULL;
	unset = &set[i];
	for (i = 0; (name = va_arg(va, const char *)); ++i)
		unset[i] = name;
	unset[i++] = NULL;
	va_end(va);

	return vcheck_cpp_cond(node, set, unset);
}

static struct truth_table *
alloc_truth_table(int n)
{
	int tblsize;
	struct truth_table *ret;

	tblsize = (1 << n) / (8*sizeof(unsigned long));
	if (tblsize <= 0)
		tblsize = 1;
	ret = calloc(1, sizeof(struct truth_table) +
		     tblsize * sizeof(unsigned long) + n * sizeof(char*));
	if (!ret)
		return NULL;
	ret->n = n;
	ret->tblsize = tblsize;
	ret->names = (const char**)
		((char*)(ret + 1) + tblsize * sizeof(unsigned long));
	return ret;
}

/* Translate a minterm index @n (@nbits wide) according to the
 * @xlat table which maps source bit numbers to target bit values.
 */
static unsigned long
xlat_minterm(unsigned long *xlat, unsigned long n, int nbits)
{
	unsigned long ret = 0;
	int i;
	for (i = 0; i < nbits; ++i)
		if (n & (1 << i))
			ret |= xlat[i];
	return ret;
}

static struct truth_table *
binop_truth_table(struct truth_table *left, struct truth_table *right, int op)
{
	struct truth_table *ret;
	unsigned long rxlat[right->n]; /* bit translation for @right */
	unsigned long copy = 0;	       /* target copy positions */
	unsigned ncopy = 0;	       /* bits set in @copy */
	unsigned i, j, n;

	/* Count the number of variables in the result */
	n = left->n + right->n;
	for (i = 0; i < left->n; ++i) {
		const char *lname = left->names[i];
		for (j = 0; j < right->n; ++j)
			if (right->names[j] &&
			    !strcmp(lname, right->names[j])) {
				right->names[j] = NULL;
				rxlat[j] = 1 << i;
				--n;
				break;
			}
		if (j >= right->n) {
			copy |= 1 << i;
			++ncopy;
		}
	}

	/* Allocate the result */
	if (! (ret = alloc_truth_table(n)) )
		return NULL;

	/* Copy the names */
	n = left->n;
	for (i = 0; i < left->n; ++i)
		ret->names[i] = left->names[i];
	for (i = 0; i < right->n; ++i)
		if (right->names[i]) {
			ret->names[n] = right->names[i];
			rxlat[i] = 1 << n;
			++n;
		}

	/* Copy the bits from @right to @ret */
	for (i = 0; i < 1 << right->n; ++i) {
		if (!test_minterm(right, i))
			continue;

		unsigned long base = xlat_minterm(rxlat, i, right->n);
		for (j = 0; j < (1 << ncopy); ++j) {
			unsigned long off = 0;
			unsigned long mask = 1;
			unsigned tmp = j;
			while (tmp) {
				while (!(copy & mask))
					mask <<= 1;
				if (tmp & 1)
					off |= mask;
				tmp >>= 1;
				mask <<= 1;
			}
			set_minterm(ret, base + off);
		}
	}

	/* Do the bitop */
	for (i = 0; i < left->tblsize; ++i) {
		unsigned long val = left->tbl[i];
		for (j = 1 << left->n; j < 8*sizeof(long); j <<= 1)
			val |= val << j;
		for (j = 0; j < ret->tblsize; j += left->tblsize)
			switch (op) {
			case AND_OP:
				ret->tbl[i+j] &= val;
				break;
			case OR_OP:
				ret->tbl[i+j] |= val;
				break;
			case EQ_OP:
				val = ~val;
				/* fall through */
			case NE_OP:
				ret->tbl[i+j] ^= val;
				break;
			}
	}

	return ret;
}

struct truth_table *
cpp_truth_table(node_t *node)
{
	struct truth_table *left, *right, *ret;
	int i;

	if (!node)
		return NULL;
	assert(node->type == nt_expr);

	switch (node->e.op) {
	case ID:
		if (! (ret = alloc_truth_table(1)) )
			return NULL;
		set_minterm(ret, 1);
		ret->names[0] = node->str->text;
		return ret;

	case '!':
		ret = cpp_truth_table(first_node(&node->child[che_arg1]));
		if (!ret)
			return NULL;
		for (i = 0; i < ret->tblsize; ++i)
			ret->tbl[i] = ~ret->tbl[i];
		if ((1 << ret->n) < 8*sizeof(long))
			ret->tbl[0] &= (1 << (1 << ret->n)) - 1;
		return ret;

	case AND_OP:
	case OR_OP:
	case EQ_OP:
	case NE_OP:
		left = cpp_truth_table(first_node(&node->child[che_arg1]));
		if (!left)
			return NULL;
		right = cpp_truth_table(first_node(&node->child[che_arg2]));
		if (!right) {
			free_truth_table(left);
			return NULL;
		}

		ret = binop_truth_table(left, right, node->e.op);
		free_truth_table(right);
		free_truth_table(left);
		return ret;

	default:
		fprintf(stderr, "%s: Operator %d not supported\n",
			__func__, node->e.op);
		exit(1);
	}
}

void
dump_truth_table(const struct truth_table *tbl)
{
	unsigned i, j;

	for (i = tbl->n; i; --i)
		printf("%s ", tbl->names[i-1]);
	putchar('\n');

	for (i = 0; i < 1 << tbl->n; ++i) {
		for (j = tbl->n; j; --j) {
			int len = strlen(tbl->names[j-1]);
			printf("%*d ", len, !!((i<<1) & (1<<j)));
		}
		printf("%d\n", !!test_minterm(tbl, i));
	}
}

/************************************************************
 * Related to raw contents
 *
 */

/* Return non-zero if @ds is all whitespace */
int
dynstr_isspace(struct dynstr *ds)
{
	int i;
	for (i = 0; i < ds->len; ++i)
		if (!isspace(ds->text[i]))
			return 0;
	return 1;
}

/* Remove all whitespace at @ds.
 * Returns the following dynstr.
 */
struct dynstr *
dynstr_delspace(struct list_head *list, struct dynstr *ds)
{
	while (&ds->list != list && dynstr_isspace(ds))
		ds = dynstr_del(ds);
	return ds;
}

/* Remove all whitespace at @ds going backwards.
 * Returns the preceding dynstr.
 */
struct dynstr *
dynstr_delspace_rev(struct list_head *list, struct dynstr *ds)
{
	while (&ds->list != list && dynstr_isspace(ds))
		ds = dynstr_del_rev(ds);
	return ds;
}

/* Find the beginning of the line starting from @ds:@pos */
void
dynstr_bol(struct list_head *list, struct dynstr **ds, size_t *pos)
{
	struct dynstr *iter = *ds;
	size_t off = *pos;
	char *p = NULL;
	while (&iter->list != list &&
	       !(p = memrchr(iter->text, '\n', off)) ) {
		iter = prev_dynstr(iter);
		off = (&iter->list != list ? iter->len : 0);
	}
	if (p && (off = p - iter->text + 1) < iter->len) {
		*ds = iter;
		*pos = off;
	} else {
		*ds = next_dynstr(iter);
		*pos = 0;
	}
}

/* Create a new dynstr that contains a newline and the indentation
 * of @startds:@startoff.
 * This is useful to re-create the indentation of a split decl.
 */
struct dynstr *
dynstr_dup_indent(struct list_head *list,
		  struct dynstr *startds, size_t startoff)
{
	struct dynstr *bol = startds;
	size_t boloff = startoff;
	dynstr_bol(list, &bol, &boloff);

	size_t len = 0;
	struct dynstr *ds = bol;
	size_t off = boloff;
	while (ds != startds && dynstr_isspace(ds)) {
		len += ds->len - off;
		ds = next_dynstr(ds);
		off = 0;
	}

	int i = 0;
	while (i < startoff && isspace(ds->text[i]))
		++i, ++len;

	struct dynstr *newds = newdynstr(NULL, len + 1);
	char *p = newds->text;
	*p++ = '\n';

	ds = bol;
	off = boloff;
	while (len) {
		size_t curlen = ds->len - off;
		if (curlen > len)
			curlen = len;
		memcpy(p, ds->text + off, curlen);
		p += curlen;
		len -= curlen;
		ds = next_dynstr(ds);
		off = 0;
	}

	return newds;
}

/************************************************************
 * Related to the parsed tree
 *
 */

static enum walk_action
reset_user_data_fn(node_t *node, void *data)
{
	node->user_list.next = NULL;
	node->user_list.prev = NULL;
	node->user_data = NULL;
	return walk_continue;
}

void
reset_user_data(struct list_head *tree)
{
	walk_tree(tree, reset_user_data_fn, NULL);
}

/* Get the @pos-th node from @list */
node_t *
nth_node(struct list_head *list, int pos)
{
	node_t *elem;
	int i = 0;
	list_for_each_entry(elem, list, list) {
		if (++i == pos)
			return elem;
	}
	return NULL;
}

int
child_order(node_t *child, node_t *parent, int idx)
{
	return list_pos(&child->list, &parent->child[idx]) + 1;
}

struct list_head *
node_scope(node_t *node)
{
	switch (node->type) {
	case nt_type:
		if (node->t.category == type_struct ||
		    node->t.category == type_union ||
		    node->t.category == type_enum)
			return &node->child[cht_body];
		else if (node->t.category == type_func) {
			node = typed_parent(node, nt_decl);
			return &node->child[chd_body];
		}
		break;

	case nt_var:
		node = node->parent;
		assert(node->type == nt_decl);
		/* fall through */
	case nt_decl:
		return &node->child[chd_body];

	default:
		break;
	}
	return NULL;
}

struct list_head *
find_scope(node_t *node, node_t **pparent)
{
	struct list_head *ret = &node->pf->parsed;
	node_t *parent;
	while ( (parent = node->parent) ) {
		if (parent->type == nt_type &&
		    (parent->t.category == type_struct ||
		     parent->t.category == type_union) &&
		    is_child(node, parent, cht_body)) {
			ret = &parent->child[cht_body];
			break;
		} else if (parent->type == nt_type &&
			   parent->t.category == type_func &&
			   is_child(node, parent, cht_param)) {
			parent = typed_parent(parent, nt_decl);
			ret = &parent->child[chd_body];
			break;
		} else if (parent->type == nt_decl &&
			   (is_child(node, parent, chd_body) ||
			    is_child(node, parent, chd_decl)) ) {
			ret = &parent->child[chd_body];
			break;
		}

		node = parent;
	}
	if (pparent)
		*pparent = parent;
	return ret;
}

static enum walk_action
nullify_str_fn(node_t *node, void *data)
{
	set_node_str(node, NULL);
	return walk_continue;
}

/* Remove all str references (recursively), so any dynstr can
 * be modified freely.
 */
void
nullify_str(node_t *node)
{
	walk_tree_single(node, nullify_str_fn, NULL);
}

void
rollback_user_list(struct list_head *list, node_t *ptr)
{
	node_t *next;
	list_for_each_entry_safe_continue(ptr, next, list, user_list) {
		list_del(&ptr->user_list);
		ptr->user_list.next = ptr->user_list.prev = NULL;
	}
}

/************************************************************
 * Split nodes
 *
 */

/* Add a new split_node to @splitlist */
struct split_node *
split_add(struct list_head *splitlist, node_t *node,
	  struct dynstr *oldds, struct dynstr *newds)
{
	struct split_node *split = malloc(sizeof(struct split_node));
	split->oldds = oldds;
	split->newds = newds;
	INIT_LIST_HEAD(&split->nodes);
	split_addnode(split, node);
	list_add_tail(&split->list, splitlist);
	return split;
}

/* Remove @split from its list and free it */
void
split_remove(struct split_node *split)
{
	list_del(&split->list);
	free(split);
}

/* Search @splitlist for a split @ds with text changed to @newtext */
struct split_node *
split_search(struct list_head *splitlist, struct dynstr *ds,
	     const char *newtext)
{
	struct split_node *split;
	list_for_each_entry(split, splitlist, list)
		if (split->oldds == ds &&
		    !strcmp(split->newds->text, newtext))
			return split;
	return NULL;
}

/************************************************************
 * Parsed file
 *
 */

struct parsed_file *
find_file(struct list_head *filelist, const char *name)
{
	struct parsed_file *pf;
	list_for_each_entry(pf, filelist, list)
		if (pf->name == NULL) {
			if (name == NULL)
				return pf;
		} else if (!strcmp(pf->name, name))
			return pf;
	return NULL;
}

/************************************************************
 * System interfaces
 *
 */

static int
run_command(const char *name, const char *const argv[])
{
	pid_t pid;
	if ( !(pid = vfork()) ) {
		execvp(name, (char *const*) argv);
		_exit(-1);
	} else if (pid > 0){
		int status;
		if (waitpid(pid, &status, 0) == -1) {
			perror("Cannot get child status");
			return -1;
		}

		if (WIFEXITED(status))
			return WEXITSTATUS(status);
		else
			return -1;
	} else {
		perror("Cannot fork");
		return -1;
	}
}

/************************************************************
 * Interface to quilt
 *
 */

#define QUILT	"quilt"

static int
writeout(const char *name, struct list_head *rawlist)
{
	FILE *f;
	int res;
	if (! (f = fopen(name, "w")) ) {
		fprintf(stderr, "Cannot open %s for writing: %s\n",
			name, strerror(errno));
		return -1;
	}
	if ( (res = dump_contents(rawlist, f)) ) {
		fprintf(stderr, "Write to %s failed: %s\n",
			name, strerror(errno));
		fclose(f);
		return res;
	}
	if (fclose(f)) {
		fprintf(stderr, "Cannot close %s: %s\n",
			name, strerror(errno));
	}

	return 0;
}

static int
writeout_files(struct list_head *filelist)
{
	struct parsed_file *pf;
	int res;
	list_for_each_entry(pf, filelist, list) {
		if (pf->clean || !pf->name)
			continue;
		if ( (res = writeout(pf->name, &pf->raw)) )
			return res;
		pf->clean = 1;
	}
	return 0;
}

int
quilt_new(const char *name)
{
	const char *argv[4];
	int i = 0;
	argv[i++] = QUILT;
	argv[i++] = "new";
	argv[i++] = name;
	argv[i] = NULL;
	return run_command(QUILT, argv);
}

static const char *quilt_refresh_argv[] =
{ QUILT, "refresh", "-p", "ab", "--no-timestamp", NULL };

int
quilt_refresh(struct list_head *filelist)
{
	int n = list_count(filelist);
	const char **argv = alloca((n+3) * sizeof(char*));
	struct parsed_file *pf;
	int i, res;

	i = 0;
	argv[i++] = QUILT;
	argv[i++] = "add";
	list_for_each_entry(pf, filelist, list) {
		if (pf->clean || !pf->name)
			continue;
		argv[i++] = pf->name;
	}
	argv[i] = NULL;
	if ( (res = run_command(QUILT, argv)) )
		return res;

	if ( (res = writeout_files(filelist)) )
		return res;

	return run_command(QUILT, quilt_refresh_argv);
}

int
quilt_import(const char *name, const char *basedir)
{
	static const char *quilt_push_argv[] =
		{ QUILT, "push", NULL };
	char *path = malloc(strlen(basedir) + strlen(name) + 1);
	const char *quilt_import_argv[] =
		{ QUILT, "import", path, NULL };
	int res;

	strcpy(stpcpy(path, basedir), name);

	if ( (res = run_command(QUILT, quilt_import_argv)) )
		return res;

	if ( (res = run_command(QUILT, quilt_push_argv)) )
		return res;

	return run_command(QUILT, quilt_refresh_argv);
}

FILE *
quilt_header(void)
{
	return popen(QUILT " header -r", "w");
}
