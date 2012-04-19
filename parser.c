
/* Check the output for

void (*signal(int sig, void (*func)(int handler_sig)))(int oldhandler_sig);

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <argp.h>
#include "parser.h"
#include "clang.tab.h"

/* Swap these two lines if you want to enable debugging */
#define DEBUG	1
#undef DEBUG

#if DEBUG
# include "dump.h"
#endif

static const char *predef_types[] = {
	"__time_t",		/* from types.h */
	"Bytef",		/* from zlib.h */
	"Elf32_Ehdr",
	"Elf32_Nhdr",
	"Elf32_Off",
	"Elf32_Phdr",
	"Elf32_Shdr",
	"Elf32_Sym",
	"Elf64_Addr",
	"Elf64_Ehdr",
	"Elf64_Nhdr",
	"Elf64_Off",
	"Elf64_Phdr",
	"Elf64_Shdr",
	"Elf64_Sym",
	"FILE",
	"asection",
	"asymbol",
	"bfd",
	"bfd_byte",
	"bfd_signed_vma",
	"bfd_size_type",
	"bfd_vma",
	"bool",			/* from stdbool.h */
	"dev_t",		/* from stat.h - must be changed! */
	"fd_set",
	"file_ptr",		/* from bfd.h */
	"iconv_t",
	"in_addr_t",
	"int8_t",
	"int16_t",
	"int32_t",
	"int64_t",
	"jmp_buf",
	"kltype_t",		/* in a !REDHAT #define */
	"loff_t",
	"off_t",
	"pid_t",
	"regex_t",
	"rwlock_t",		/* in a !REDHAT #define */
	"size_t",
	"socklen_t",
	"ssize_t",
	"stab_type_t",		/* in a !REDHAT #define */
	"time_t",
	"u_long",		/* BSD type used in vas_* */
	"uLong",		/* from zlib.h */
	"uLongf",		/* from zlib.h */
#if 0
	"u8",			/* sucks: see unwind.c and unwind_i.h */
	"u64",			/* sucks: see unwind.c and unwind_i.h */
#endif
	"uInt",			/* from zlib.h */
	"uint",
	"uint8_t",
	"uint16_t",
	"uint32_t",
	"uint64_t",
	"ulong",
	"ushort",
	"va_list",
	"vm_offset_t",		/* in a !MCLX #define */
	"z_stream",		/* from zlib.h */
	NULL,
};

static const char builtin_file[] =
	/* symbol_info: */
"typedef unsigned long symvalue;\n"
"typedef struct _symbol_info\n"
"{\n"
"  symvalue value;\n"
"  char type;\n"
"  const char *name;\n"
"  unsigned char stab_type;\n"
"  char stab_other;\n"
"  short stab_desc;\n"
"  const char *stab_name;\n"
"} symbol_info;\n"
;

static LIST_HEAD(files);

struct dynstr dummydynstr = {
	.list = LIST_HEAD_INIT(dummydynstr.list),
	.node_first = LIST_HEAD_INIT(dummydynstr.node_first),
	.node_last = LIST_HEAD_INIT(dummydynstr.node_last),
	.text = "",
};
YYLTYPE dummyloc = {
	.first_text = &dummydynstr,
	.last_text = &dummydynstr,
};

void init_predef_types(void)
{
	const char **p;
	cleartypedefs();
	for (p = predef_types; *p; ++p)
		addtypedef(*p);
}

static node_t *
check_current_node(node_t *node,
		   struct list_head *prev, struct list_head *next)
{
	/* No change */
	if (next->prev == &node->list)
		return node;

	/* Deleted node */
	if (next->prev == prev)
		return NULL;

	/* Replaced node */
	return list_entry(prev->next, node_t, list);
}

static inline enum walk_action
walk_children(node_t *node, walkfn *fn, void *data)
{
	int i;
	for (i = 0; i < node->nchild; ++i)
		if (walk_tree(&node->child[i], fn, data) == walk_terminate)
			return walk_terminate;
	return walk_continue;
}

enum walk_action
walk_tree(struct list_head *tree, walkfn *fn, void *data)
{
	node_t *item, *next;
	struct list_head *prev = tree;
	list_for_each_entry_safe(item, next, tree, list) {
		enum walk_action act = fn(item, data);
		if (act == walk_terminate)
			return walk_terminate;

		if (! (item = check_current_node(item, prev, &next->list)) )
			continue;

		if (act != walk_skip_children &&
		    walk_children(item, fn, data) == walk_terminate)
			return walk_terminate;

		prev = &item->list;
	}
	return walk_continue;
}

enum walk_action
walk_tree_single(node_t *tree, walkfn *fn, void *data)
{
	struct list_head *prev = tree->list.prev,
		*next = tree->list.next;

	switch (fn(tree, data)) {
	case walk_terminate:     return walk_terminate;
	case walk_skip_children: return walk_continue;
	case walk_continue:      break;
	}

	if (! (tree = check_current_node(tree, prev, next)) )
		return walk_continue;

	return walk_children(tree, fn, data);
}

/* Completely detach a dynstr list from its surrounding */
void
detach_text(struct dynstr *first, struct dynstr *last)
{
	first->list.prev->next = last->list.next;
	last->list.next->prev = first->list.prev;
	first->list.prev = &last->list;
	last->list.next = &first->list;
}

static void
implant_text_list(struct list_head *prev, struct list_head *next,
		  struct dynstr *first, struct dynstr *last)
{
	first->list.prev = prev;
	prev->next = &first->list;
	last->list.next = next;
	next->prev = &last->list;	
}

void insert_text_list(struct dynstr *where,
		      struct dynstr *first, struct dynstr *last)
{
	implant_text_list(where->list.prev, &where->list, first, last);
}

void replace_text_list(struct dynstr *oldfirst, struct dynstr *oldlast,
		       struct dynstr *newfirst, struct dynstr *newlast)
{
	struct list_head *it, *next, *follow;
	node_t *node, *nnode;

	implant_text_list(oldfirst->list.prev, oldlast->list.next,
			  newfirst, newlast);

	if (oldfirst->cpp_cond != oldlast->cpp_cond) {
		fputs("Replacing CPP conditionals not supported\n", stderr);
		exit(1);
	}
	for (it = &newfirst->list; it != newlast->list.next; it = it->next) {
		struct dynstr *ds = list_entry(it, struct dynstr, list);
		ds->cpp_cond = oldfirst->cpp_cond;
	}

	list_for_each_entry_safe(node, nnode,
				 &oldfirst->node_first, first_list)
		set_node_first(node, newfirst);
	list_for_each_entry_safe(node, nnode,
				 &oldlast->node_last, last_list)
		set_node_last(node, newlast);

	it = &oldfirst->list;
	follow = oldlast->list.next;
	while (it != follow) {
		struct dynstr *ds = list_entry(it, struct dynstr, list);
		list_del(&ds->node_first);
		list_del(&ds->node_last);

		next = it->next;
		freedynstr(ds);
		it = next;
	}
}

int dump_contents(struct list_head *contents, FILE *f)
{
	struct dynstr *ds;
	list_for_each_entry(ds, contents, list)
		if (fwrite(ds->text, 1, ds->len, f) != ds->len)
			return -1;
	return 0;
}

/* Re-parse a node from the current raw contents */
node_t *reparse_node(node_t *node, int type)
{
	node_t *newnode;
	int res;

	parsed_file = node->pf;
	INIT_LIST_HEAD(&parsed_tree);
	INIT_LIST_HEAD(&raw_contents);
	lex_input_first = node->first_text;
	lex_input_last = node->last_text;
	lex_cpp_mode = 0;
	start_symbol = type;
	res = yyparse();
	yylex_destroy();

	if (res != 0) {
		/* This is fatal (for now) */
		fprintf(stderr, "Reparsing failed with %d\n", res);
		exit(1);
	}

	parsed_file->clean = 0;
	newnode = first_node(&parsed_tree);
	newnode->parent = node->parent;
	list_add(&newnode->list, &node->list);

	struct dynstr *oldfirst = node->first_text,
		*oldlast = node->last_text;
	freenode(node);
	replace_text_list(oldfirst, oldlast,
			  newnode->first_text, newnode->last_text);

	return newnode;
}

static void
discard_parsing(void)
{
	node_t *node, *nnode;
	list_for_each_entry_safe(node, nnode, &parsed_tree, list)
		freenode(node);

	struct dynstr *ds, *dsnext;
	list_for_each_entry_safe(ds, dsnext, &raw_contents, list)
		freedynstr(ds);
}

/* Check whether @tree is a CPP conditional */
static int
is_cpp_cond(struct list_head *tree)
{
	node_t *dir = first_node(tree);
	if (! (dir->type == nt_expr) )
		return 0;

	int op = dir->e.op;
	return (op == CPP_IF || op == CPP_IFDEF ||
		op == CPP_IFNDEF || op == CPP_ELIF ||
		op == CPP_ELSE || op == CPP_ENDIF);
}

/* Check whether a parsed tree is a "defined()" CPP operator */
static int
is_define(struct list_head *tree)
{
	if (list_empty(tree))
		return 0;

	node_t *node = first_node(tree);
	return (node->type == nt_decl);
}

/* Remove unnecessary defined() calls */
static enum walk_action
remove_defined(node_t *node, void *data)
{
	if (! (node->type == nt_expr && node->e.op == CPP_DEFINED) )
		return walk_continue;
	node_t *arg = first_node(&node->child[che_arg1]);
	list_move(&arg->list, &node->list);
	arg->parent = node->parent;
	freenode(node);

	return walk_continue;
}

/* Adapted from configure.c */
static const char *const all_archs[] = {
	"X86",
	"ALPHA",
	"PPC",
	"IA64",
	"S390",
	"S390X",
	"PPC64",
	"X86_64",
	"ARM",
	NULL,
};

/* Make the architecture macros exclusive */
static enum walk_action
make_arch_exclusive(node_t *node, void *data)
{
	if (! (node->type == nt_expr && node->e.op == ID) )
		return walk_continue;
	
	const char *const *arch;
	for (arch = all_archs; *arch; ++arch)
		if (!strcmp(*arch, node->str->text))
			break;
	if (!*arch)
		return walk_continue;

	const char *const *p;
	for (p = all_archs; *p; ++p) {
		if (p == arch)
			continue;

		node_t *other = dupnode_nochild(node);
		struct dynstr *ds = newdynstr(*p, strlen(*p));
		set_node_str(other, ds);

		node_t *op_not = dupnode_nochild(other);
		set_node_str(op_not, NULL);
		op_not->e.op = '!';
		set_node_child(op_not, che_arg1, other);

		node_t *op_and = dupnode_nochild(op_not);
		op_and->e.op = AND_OP;

		list_add_tail(&op_and->list, &node->list);
		list_del_init(&node->list);
		set_node_child(op_and, che_arg1, op_not);
		set_node_child(op_and, che_arg2, node);
	}
	return walk_skip_children;
}

#define CPP_STACK_SIZE	32

struct cpp_cond_state {
	node_t *current;
	node_t *precond;
	unsigned stackptr;
	struct {
		node_t *node;
		node_t *precond;
	} stack[CPP_STACK_SIZE];
};

static node_t *
get_cpp_cond(struct cpp_cond_state *state, struct list_head *tree)
{
	node_t *dir = first_node(tree);
	int op = dir->e.op;

	walk_tree(&dir->child[che_arg1], remove_defined, NULL);
	walk_tree(&dir->child[che_arg1], make_arch_exclusive, NULL);

	node_t *realroot = first_node(&dir->child[che_arg1]);
	node_t *root = realroot;

	if (op == CPP_ELSE || op == CPP_ENDIF) {
		/* Do nothing for directives without arguments,
		 * because their @root is an invalid pointer.
		 */
	} else {
		detach_text(root->first_text, root->last_text);
		list_del_init(&root->list);
	}

	/* Use the latest condition as base on else-blocks */
	if (op == CPP_ELSE || op == CPP_ELIF) {
		node_t *lastcond = state->current;
		if (state->precond)
			lastcond = first_node(&lastcond->child[che_arg2]);
		root = dupnode(lastcond);
	}

	/* Convert negative conditions into positive */
	if (op == CPP_IFNDEF || op == CPP_ELSE || op == CPP_ELIF)
		root = newexpr1(&dummyloc, '!', root);

	/* Include the real condition for #elif */
	if (op == CPP_ELIF) {
		state->precond = state->precond
			? newexpr2(&dummyloc, AND_OP,
				   dupnode(state->precond), root)
			: root;

		root = realroot;
	}

	/* Push new state when starting a conditional block */
	if (op == CPP_IF || op == CPP_IFDEF || op == CPP_IFNDEF) {
		state->stack[state->stackptr].node = state->current;
		state->stack[state->stackptr].precond = state->precond;
		state->stackptr++;

		state->precond = state->current;
	}

	/* Add the new condition */
	if (op == CPP_IF || op == CPP_IFDEF || op == CPP_IFNDEF ||
	    op == CPP_ELSE || op == CPP_ELIF)
		state->current = state->precond
			? newexpr2(&dummyloc, AND_OP,
				   dupnode(state->precond), root)
			: root;

	/* Pop the previous state on #endif */
	if (op == CPP_ENDIF) {
		--state->stackptr;
		state->current = state->stack[state->stackptr].node;
		state->precond = state->stack[state->stackptr].precond;
	}

	return state->current;
}

/* Parse macro bodies saved during the first stage */
static void parse_macros(void)
{
	struct dynstr *ds, *next;
	struct cpp_cond_state cond_state;
	struct dynstr *cond_ds = NULL;
	node_t *lastcond;

	memset(&cond_state, 0, sizeof cond_state);

	list_for_each_entry_safe(ds, next, &raw_cpp, cpp_list) {
		struct list_head savedparsed;
		struct list_head savedraw;
		int ret;

		/* Save the original parsed_tree and start a new one */
		list_add(&savedparsed, &parsed_tree);
		list_del_init(&parsed_tree);

		/* Save the original raw list and start a new one */
		list_add(&savedraw, &raw_contents);
		list_del_init(&raw_contents);

		lex_input_first = lex_input_last = ds;
		lex_cpp_mode = 1;
		start_symbol = START_DIRECTIVE;
		ret = yyparse();
		yylex_destroy();

		if (!ret && is_define(&parsed_tree)) {
			struct dynstr *first, *last;
			first = list_entry(raw_contents.next,
					   struct dynstr, list);
			last = list_entry(raw_contents.prev,
					  struct dynstr, list);
			replace_text_list(ds, ds, first, last);
		} else {
			if (!ret && is_cpp_cond(&parsed_tree)) {
				if (!cond_ds)
					cond_ds = ds;
				while (cond_ds != ds) {
					cond_ds->cpp_cond = lastcond;
					cond_ds = next_dynstr(cond_ds);
				}
				lastcond = get_cpp_cond(&cond_state,
							&parsed_tree);
			}
			discard_parsing();
		}

		list_splice(&savedparsed, &parsed_tree);

		list_add(&raw_contents, &savedraw);
		list_del(&savedraw);
	}

	if (cond_ds)
		while (cond_ds != ds) {
			cond_ds->cpp_cond = lastcond;
			cond_ds = next_dynstr(cond_ds);
		}
}

/* Parse an external file */
int parse_file(struct parsed_file *pf)
{
	node_t *node, *nextnode;
	struct dynstr *ds, *nextds;
	struct list_head oldraw;
	int ret;

	INIT_LIST_HEAD(&oldraw);
	if (!pf->name) {
		/* The built-in file exists only in memory,
		 * so don't clean it up! */
		list_splice_tail(&pf->raw, &oldraw);
		INIT_LIST_HEAD(&pf->raw);
	}

	/* Clean up any respective old contents first */
	list_for_each_entry_safe(node, nextnode, &pf->parsed, list)
		freenode(node);
	list_for_each_entry_safe(ds, nextds, &pf->raw, list)
		freedynstr(ds);

	if (!pf->name)
		yyin = NULL;
	else if (!strcmp(pf->name, "-"))
		yyin = stdin;
	else {
		yyin = fopen(pf->name, "r");
		if (!yyin) {
			fprintf(stderr, "Cannot open %s: %s\n",
				pf->name, strerror(errno));
			return errno == ENOENT ? 0 : -1;
		}
	}

	if (pf->name)
		fprintf(stderr, "Parsing file %s\n", pf->name);

	parsed_file = pf;
	INIT_LIST_HEAD(&parsed_tree);
	INIT_LIST_HEAD(&raw_contents);
	INIT_LIST_HEAD(&raw_cpp);
	if (yyin)
		lex_input_first = lex_input_last = NULL;
	else {
		lex_input_first = list_entry(oldraw.next, struct dynstr, list);
		lex_input_last = list_entry(oldraw.prev, struct dynstr, list);
	}
	lex_cpp_mode = 0;
	start_symbol = 0;
	ret = yyparse();
	if (yyin && yyin != stdin && fclose(yyin)) {
		fprintf(stderr, "Cannot close %s: %s\n",
			pf->name, strerror(errno));
		ret = -1;
	}
	yylex_destroy();

	if (ret) {
		fprintf(stderr, "Parser failed with %d\n", ret);
	} else {
		parse_macros();

		list_add_tail(&pf->parsed, &parsed_tree);
		list_del_init(&parsed_tree);
		list_add_tail(&pf->raw, &raw_contents);
		list_del_init(&raw_contents);
		pf->clean = 1;
	}

	/* Clean up old contents if necessary */
	list_for_each_entry_safe(ds, nextds, &oldraw, list)
		freedynstr(ds);

	return ret;
}

static void
add_builtin_file(void)
{
	static struct parsed_file builtin_pf = {
		.parsed = LIST_HEAD_INIT(builtin_pf.parsed),
		.raw = LIST_HEAD_INIT(builtin_pf.raw),
	};
	struct dynstr *ds = newdynstr(builtin_file, sizeof builtin_file - 1);

	list_add(&ds->list, &builtin_pf.raw);
	list_add(&builtin_pf.list, &files);
}

static int add_file(const char *name)
{
	struct parsed_file *pf;

	if (! (pf = malloc(sizeof(struct parsed_file))) ) {
		perror("Cannot allocate parsed file");
		return -1;
	}

	pf->name = name;
	INIT_LIST_HEAD(&pf->parsed);
	INIT_LIST_HEAD(&pf->raw);
	pf->clean = 1;
	list_add_tail(&pf->list, &files);
	return 0;
}

static char *
get_basedir(const char *path)
{
	char *p, *ret;
	if ( (p = strrchr(path, '/')) ) {
		ret = malloc(p - path + 2);
		memcpy(ret, path, p - path + 1);
		ret[p - path + 1] = 0;
	} else
		ret = "./";
	return ret;
}

const char *argp_program_version =
	"xcrashify 1.0";
const char *argp_program_bug_address =
	"<ptesarik@suse.cz>";
static char doc[] =
	"A script to add cross-platform functionality to the crash utility";

/* Non-option arguments are input files */
static char args_doc[] = "[FILE...]";

/* The options we understand. */
static struct argp_option options[] = {
	{"basedir",  'd', "DIR",   0,  "Where to look for imported patches" },
	{"xform",    'x', "NAME",  0,  "Do only the specified transform(s)" },
	{"patch",    'p', "NAME",  OPTION_ALIAS },
	{ 0 }
};

/* Parse a single option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
	struct arguments *arguments = state->input;
	struct dynstr *ds;

	switch (key) {
	case 'd':
		free(arguments->basedir);
		arguments->basedir = strdup(arg);
		break;

	case 'x':
	case 'p':
		ds = newdynstr(arg, strlen(arg) + 1);
		list_add_tail(&ds->list, &arguments->xform_names);
		break;

         default:
           return ARGP_ERR_UNKNOWN;
         }
       return 0;
     }

/* An argp parser. */
static struct argp argp = { options, parse_opt, args_doc, doc };

int main(int argc, char **argv)
{
	struct arguments arguments;
	int i;
	int ret;

	/* Default values. */
	INIT_LIST_HEAD(&arguments.xform_names);
	arguments.basedir = get_basedir(argv[0]);

	/* Parse arguments */
	argp_parse(&argp, argc, argv, 0, &i, &arguments);

	add_builtin_file();
	init_predef_types();
	if (i >= argc)
		ret = add_file("-");
	else {
		while(i < argc)
			if ( (ret = add_file(argv[i++])) )
				break;
	}

#if DEBUG
	fdump = stdout;
#endif

	if (!ret) {
#if DEBUG
		struct parsed_file *pf;
		list_for_each_entry(pf, &files, list) {
			parse_file(pf);
			printf("File %s original\n", pf->name);
			dump_tree(&pf->parsed);
		}
#endif
		ret = xform_files(&arguments, &files);

#if DEBUG
		list_for_each_entry(pf, &files, list) {
			printf("File %s transformed\n", pf->name);
			dump_tree(&pf->parsed);
		}
#endif
	}

	return ret;
}
