
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

static const char *predef_types[] = {
	"__time_t",		/* from types.h */
	"Bytef",		/* from zlib.h */
	"Elf32_Ehdr",
	"Elf32_Nhdr",
	"Elf32_Off",
	"Elf32_Phdr",
	"Elf32_Shdr",
	"Elf32_Sym",
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
	"symbol_info",
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

#if DEBUG

static void dump_basic_type(type_t *type);
static void dump_type(type_t *type, int showflags);
static void dump_expr(expr_t *expr);
static void dump_var(var_t *var);
static void dump_tree(struct list_head *tree);

static int indent = 2;
static int depth = -1;

#endif	/* DEBUG */

void init_predef_types(void)
{
	const char **p;
	cleartypedefs();
	for (p = predef_types; *p; ++p)
		addtypedef(*p);
}

void
walk_tree(struct list_head *tree, walkfn *fn, void *data)
{
	node_t *item, *next;
	list_for_each_entry_safe(item, next, tree, list) {
		int i;
		for (i = 0; i < item->nchild; ++i)
			walk_tree(&item->child[i], fn, data);
		if (fn(item, data))
			break;
	}
}

void
walk_tree_single(node_t *tree, walkfn *fn, void *data)
{
	int i;
	for (i = 0; i < tree->nchild; ++i)
		walk_tree(&tree->child[i], fn, data);
	fn(tree, data);
}

#if DEBUG

static void dump_chunk(struct dynstr *first, struct dynstr *last)
{
	struct dynstr *cur = first;
	fputs(">>>", stdout);
	for (;;) {
		fwrite(cur->text, 1, cur->len, stdout);

		if (cur == last)
			break;
		cur = list_entry(cur->list.next, struct dynstr, list);
	}
	fputs("<<<\n", stdout);
}

static void dump_basic_type(type_t *type)
{
	static const char *types[] = {
		[bt_char] = "char",
		[bt_double] = "double",
		[bt_float] = "float",
		[bt_int] = "int",
		[bt_long] = "long",
		[bt_longlong] = "long",
		[bt_short] = "short",
		[bt_signed] = "signed",
		[bt_unsigned] = "unsigned",
		[bt_void] = "void",
	};
	int i, n;
	for (i = n = 0; i < bt_max; ++i) {
		if (! (type->btype & (1UL << i)) )
			continue;
		if (n++)
			putchar(' ');
		fputs(types[i], stdout);
	}
}

static void dump_type(type_t *type, int showflags)
{
	if (showflags) {
		putchar('[');
		if (type->flags & TF_CONST)
			fputs(" const", stdout);
		if (type->flags & TF_VOLATILE)
			fputs(" volatile", stdout);
		fputs(" ] ", stdout);

		putchar('[');
		if (type->flags & TF_AUTO)
			fputs(" auto", stdout);
		if (type->flags & TF_REGISTER)
			fputs(" register", stdout);
		if (type->flags & TF_STATIC)
			fputs(" static", stdout);
		if (type->flags & TF_EXTERN)
			fputs(" extern", stdout);
		if (type->flags & TF_INLINE)
			fputs(" inline", stdout);
		if (type->flags & TF_TYPEDEF)
			fputs(" typedef", stdout);
		fputs(" ] ", stdout);
	}

	switch(type->category) {
	case type_none:
		fputs("none", stdout);
		break;

	case type_basic:
		dump_basic_type(type);
		break;

	case type_typedef:
		fputs(type->name, stdout);
		break;

	case type_struct:
		printf("struct %s", type->name);
		break;

	case type_union:
		printf("union %s", type->name);
		break;

	case type_enum:
		printf("enum %s", type->name);
		break;

	case type_pointer:
		fputs("ptr to ", stdout);
		break;

	case type_array:
		fputs("array", stdout);
		break;

	case type_func:
		fputs("func", stdout);
		break;

	case type_typeof:
		fputs("typeof\n", stdout);
		break;

	default:
		fputs("UNKNOWN!", stdout);
	}
}

static void dump_op(int op)
{
	switch(op) {
	case SHL_ASSIGN:	fputs("<<=", stdout); break;
	case SHR_ASSIGN:	fputs(">>=", stdout); break;
	case ADD_ASSIGN:	fputs("+=", stdout); break;
	case SUB_ASSIGN:	fputs("-=", stdout); break;
	case MUL_ASSIGN:	fputs("*=", stdout); break;
	case DIV_ASSIGN:	fputs("/=", stdout); break;
	case MOD_ASSIGN:	fputs("%=", stdout); break;
	case AND_ASSIGN:	fputs("&=", stdout); break;
	case XOR_ASSIGN:	fputs("^=", stdout); break;
	case OR_ASSIGN:		fputs("|=", stdout); break;
	case OR_OP:	fputs("||", stdout); break;
	case AND_OP:	fputs("&&", stdout); break;
	case EQ_OP:	fputs("==", stdout); break;
	case NE_OP:	fputs("!=", stdout); break;
	case LE_OP:	fputs("<=", stdout); break;
	case GE_OP:	fputs(">=", stdout); break;
	case SHL_OP:	fputs("<<", stdout); break;
	case SHR_OP:	fputs(">>", stdout); break;
	case INC_OP:	fputs("++", stdout); break;
	case DEC_OP:	fputs("--", stdout); break;
	case '?':	fputs("?:", stdout); break;
	case RANGE:	fputs("...", stdout); break;
	case PTR_OP:	fputs("=>", stdout); break;
	case SIZEOF_TYPE:
	case SIZEOF:	fputs("sizeof", stdout); break;
	case FUNC:	fputs("call", stdout); break;
	case ARRAY:	fputs("idx", stdout); break;
	case '{':	fputs("block", stdout); break;
	case LABEL:	fputs("label", stdout); break;
	case CASE:	fputs("case", stdout); break;
	case DEFAULT:	fputs("default", stdout); break;
	case IF:	fputs("if", stdout); break;
	case SWITCH:	fputs("switch", stdout); break;
	case WHILE:	fputs("while", stdout); break;
	case DO:	fputs("dowhile", stdout); break;
	case FOR:	fputs("for", stdout); break;
	case GOTO:	fputs("goto", stdout); break;
	case CONTINUE:	fputs("continue", stdout); break;
	case BREAK:	fputs("break", stdout); break;
	case RETURN:	fputs("return", stdout); break;
	case ELLIPSIS:	fputs("...", stdout); break;
	case TYPECAST:	fputs("typecast", stdout); break;
	case CONCAT:	fputs("concat", stdout); break;

	case OFFSETOF:	fputs("offsetof", stdout); break;
	case FOR_CPU_INDEXES:	fputs("for_cpu_indexes", stdout); break;
	case FRAME_REG:	fputs ("FRAME_REG", stdout); break;

	case CPP_IF:	fputs("#if", stdout); break;
	case CPP_IFDEF:	fputs("#ifdef", stdout); break;
	case CPP_IFNDEF:	fputs("#ifndef", stdout); break;
	case CPP_ELIF:	fputs("#elif", stdout); break;
	case CPP_ELSE:	fputs("#else", stdout); break;
	case CPP_ENDIF:	fputs("#endif", stdout); break;

	default:
		if (op <= 255)
			printf("%c", op);
		else
			printf("op%d", op);
	}
}

static void dump_expr(expr_t *expr)
{
	switch (expr->op) {
	case INT_CONST:
		printf("%ld", expr->num);
		break;
	case FLOAT_CONST:
		printf("%f", expr->f);
		break;
	case ID:
	case STRING_CONST:
	case CHAR_CONST:
		fputs(expr->str, stdout);
		break;
	default:
		dump_op(expr->op);
	}
}

static void dump_var(var_t *var)
{
	printf("name: %s", var->name);
}

static void dump_child_pos(node_t *parent, int pos)
{
	static const char *const chv_names[] =
		{ "chv_type", "chv_bitsize", "chv_init", "chv_attr" };
	static const char *const chd_names[] =
		{ "chd_type", "chd_var", "chd_decl", "chd_body" };

	switch (parent->type) {
	case nt_type:
		switch (pos) {
		case 0:
			if (parent->t.category == type_struct ||
			    parent->t.category == type_union ||
			    parent->t.category == type_enum)
				fputs("cht_body", stdout);
			else if (parent->t.category == type_typeof)
				fputs("cht_expr", stdout);
			else
				fputs("cht_type", stdout);
			break;

		case 1:
			if (parent->t.category == type_array)
				fputs("cht_size", stdout);
			else
				fputs("cht_param", stdout);
			break;

		case cht_attr:
			fputs("cht_attr", stdout);
			break;

		default:
			printf("cht_%d", pos);
		}
		break;

	case nt_expr:
		printf("che_arg%d", pos + 1);
		break;

	case nt_var:
		if (pos < chv_max)
			fputs(chv_names[pos], stdout);
		else
			printf("chv_%d", pos);
		break;

	case nt_decl:
		if (pos < chd_max)
			fputs(chd_names[pos], stdout);
		else
			printf("chd_%d", pos);
		break;
	}
}

static void dump_node(node_t *node)
{
	int i;

	++depth;

	printf("%*s(", depth*indent, "");
	dump_chunk(node->first_text, node->last_text);
	printf("%*s", depth*indent, "");
	switch (node->type) {
	case nt_type: dump_type(&node->t, 1); break;
	case nt_expr: dump_expr(&node->e); break;
	case nt_var:  dump_var (&node->v); break;
	case nt_decl: fputs("decl", stdout); break;
	}
	putchar('\n');

	for (i = 0; i < node->nchild; ++i) {
		if (!list_empty(&node->child[i])) {
			printf("%*s[", depth*indent, "");
			dump_child_pos(node, i);
			printf("]:\n");
		}
		dump_tree(&node->child[i]);
	}
	printf("%*s)\n", depth*indent, "");

	--depth;
}

static void dump_tree(struct list_head *tree)
{
	node_t *item;
	list_for_each_entry(item, tree, list)
		dump_node(item);
}

#endif	/* DEBUG */

void replace_text_list(struct dynstr *oldfirst, struct dynstr *oldlast,
		       struct dynstr *newfirst, struct dynstr *newlast)
{
	struct list_head *it, *next;
	node_t *node, *nnode;

	newfirst->list.prev = oldfirst->list.prev;
	newfirst->list.prev->next = &newfirst->list;
	newlast->list.next = oldlast->list.next;
	newlast->list.next->prev = &newlast->list;

	list_for_each_entry_safe(node, nnode,
				 &oldfirst->node_first, first_list)
		set_node_first(node, newfirst);
	list_for_each_entry_safe(node, nnode,
				 &oldlast->node_last, last_list)
		set_node_last(node, newlast);

	it = &oldfirst->list;
	while (it != &oldlast->list) {
		struct dynstr *ds = list_entry(it, struct dynstr, list);
		list_del(&ds->node_first);
		list_del(&ds->node_last);

		next = it->next;
		free(ds);
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

	newnode = first_node(&parsed_tree);
	replace_text_list(node->first_text, node->last_text,
			  newnode->first_text, newnode->last_text);
	list_add(&newnode->list, &node->list);
	freenode(node);

	return newnode;
}

static void
discard_parsing(void)
{
	node_t *node, *nnode;
	list_for_each_entry_safe(node, nnode, &parsed_tree, list)
		freenode(node);

	struct dynstr *it, *itnext;
	list_for_each_entry_safe(it, itnext, &raw_contents, list)
		free(it);
}

/* Completely detach a dynstr list from its surrounding */
static void
detach_text(struct dynstr *first, struct dynstr *last)
{
	first->list.prev->next = last->list.next;
	last->list.next->prev = first->list.prev;
	first->list.prev = &last->list;
	last->list.next = &first->list;
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
static int
remove_defined(node_t *node, void *data)
{
	if (! (node->type == nt_expr && node->e.op == CPP_DEFINED) )
		return 0;
	node_t *arg = first_node(&node->child[che_arg1]);
	list_move(&arg->list, &node->list);
	arg->parent = node->parent;
	freenode(node);

	return 0;
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

	node_t *realroot = first_node(&dir->child[che_arg1]);
	node_t *root = realroot;
	detach_text(root->first_text, root->last_text);
	list_del_init(&root->list);

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
	int ret;

	/* Clean up any respective old contents first */
	list_for_each_entry_safe(node, nextnode, &pf->parsed, list)
		freenode(node);
	list_for_each_entry_safe(ds, nextds, &pf->raw, list)
		free(ds);

	if (!strcmp(pf->name, "-"))
		yyin = stdin;
	else
		yyin = fopen(pf->name, "r");

	fprintf(stderr, "Parsing file %s\n", pf->name);

	INIT_LIST_HEAD(&parsed_tree);
	INIT_LIST_HEAD(&raw_contents);
	INIT_LIST_HEAD(&raw_cpp);
	lex_input_first = lex_input_last = NULL;
	lex_cpp_mode = 0;
	start_symbol = 0;
	ret = yyparse();
	if (yyin != stdin)
		fclose(yyin);
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

	return ret;
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

	init_predef_types();
	if (i >= argc)
		ret = add_file("-");
	else {
		while(i < argc)
			if ( (ret = add_file(argv[i++])) )
				break;
	}

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
