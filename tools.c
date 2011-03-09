#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

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
			if (!strcmp(*p, node->e.str))
				return -1;
		for (p = set; *p; ++p)
			if (!strcmp(*p, node->e.str))
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

/************************************************************
 * Related to the parsed tree
 *
 */

struct list_head *
find_scope(struct list_head *tree, node_t *node)
{
	for (; node; node = node->parent) {
		if (node->type != nt_decl)
			continue;
		if (list_empty(&node->child[chd_var]))
			continue;
		node_t *var = first_node(&node->child[chd_var]);
		if (list_empty(&var->child[chv_type]))
			continue;
		node_t *type = first_node(&var->child[chv_type]);
		if (type->t.category == type_func)
			return &node->child[chd_body];
	}
	return tree;
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
		if (pf->clean)
			continue;
		if ( (res = writeout(pf->name, &pf->raw)) )
			return res;
	}
	return 0;
}

static const char *quilt_refresh_argv[] =
{ QUILT, "refresh", "-p", "ab", "--no-timestamp", NULL };

int
quilt_new(const char *name, struct list_head *filelist)
{
	int n = list_count(filelist);
	const char **argv = alloca((n+4) * sizeof(char*));
	struct parsed_file *pf;
	int i, res;

	i = 0;
	argv[i++] = QUILT;
	argv[i++] = "new";
	argv[i++] = name;
	argv[i] = NULL;
	if ( (res = run_command(QUILT, argv)) )
		return res;

	i = 0;
	argv[i++] = QUILT;
	argv[i++] = "add";
	list_for_each_entry(pf, filelist, list) {
		if (pf->clean)
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
