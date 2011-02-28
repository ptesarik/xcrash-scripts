
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

const char *predef_types[] = {
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

#if DEBUG

static void dump_basic_type(type_t *type);
static void dump_type(type_t *type, int showflags);
static void dump_expr(expr_t *expr);
static void dump_var(var_t *var);
static void dump_tree(node_t *tree);

static int indent = 2;
static int depth = -1;

#endif	/* DEBUG */

static void init_types(const char **types)
{
	while (*types) {
		addtypedef(*types);
		++types;
	}
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

static void dump_tree(node_t *tree)
{
	node_t *item = tree;
	int i;

	if (!tree)
		return;

	++depth;
	do {
		printf("%*s(", depth*indent, "");
		dump_chunk(item->first_text, item->last_text);
		printf("%*s", depth*indent, "");
		switch (item->type) {
		case nt_type: dump_type(&item->t, 1); break;
		case nt_expr: dump_expr(&item->e); break;
		case nt_var:  dump_var (&item->v); break;
		case nt_decl: fputs("decl", stdout); break;
		}
		putchar('\n');

		for (i = 0; i < item->nchild; ++i) {
			if (!item->child[i])
				continue;
			dump_tree(item->child[i]);
		}
		printf("%*s)\n", depth*indent, "");

		item = list_entry(item->list.next, node_t, list);
	} while (item != tree);
	--depth;
}

static void dump_first_and_last(struct dynstr *ds)
{
	node_t *node;

	printf("Used as first_text:\n");
	list_for_each_entry(node, &ds->node_first, first_list)
		dump_tree(node);
	printf("Used as last_text:\n");
	list_for_each_entry(node, &ds->node_last, last_list)
		dump_tree(node);
}

#endif	/* DEBUG */

void replace_text_list(struct dynstr *oldfirst, struct dynstr *oldlast,
		       struct dynstr *newfirst, struct dynstr *newlast)
{
	struct list_head *it, *next;
	struct dynstr *ds;
	node_t *node, *nnode;

	newfirst->list.prev = oldfirst->list.prev;
	newfirst->list.prev->next = &newfirst->list;
	newlast->list.next = oldlast->list.next;
	newlast->list.next->prev = &newlast->list;

	it = &oldfirst->list;
	ds = list_entry(it, struct dynstr, list);
	list_for_each_entry_safe(node, nnode, &ds->node_first, first_list) {
		node->first_text = ds;
		list_move(&node->first_list, &newfirst->node_first);
	}

	while (it != &oldlast->list) {
		ds = list_entry(it, struct dynstr, list);
		next = it->next;
		if (!list_empty(&ds->node_first) ||
		    !list_empty(&ds->node_last)) {
			/* This is fatal (for now) */
			fprintf(stderr, "String >>>%s<<< still in use"
				" while replacing!\n", ds->text);
#if DEBUG
			dump_first_and_last(ds);
#endif
			exit(1);
		}
		free(ds);
		it = next;
	}
	
	ds = list_entry(it, struct dynstr, list);
	list_for_each_entry_safe(node, nnode, &ds->node_last, last_list) {
		node->last_text = ds;
		list_move(&node->last_list, &newlast->node_last);
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

static void parse_macros(void)
{
	struct dynstr *ds, *next;

	list_for_each_entry_safe(ds, next, &raw_cpp, cpp_list) {
		struct list_head savedraw;
		int ret;

		/* Save the original raw list and start a new one */
		list_add(&savedraw, &raw_contents);
		list_del_init(&raw_contents);

		cpp_input = ds;
		first_token = CPP_START;
		ret = yyparse();
		if (ret) {
			struct dynstr *it, *itnext;
			list_for_each_entry_safe(it, itnext,
						 &raw_contents, list)
				free(it);
		} else {
			struct dynstr *first, *last;
			first = list_entry(raw_contents.next,
					   struct dynstr, list);
			last = list_entry(raw_contents.prev,
					  struct dynstr, list);
			replace_text_list(ds, ds, first, last);
		}
		list_add(&raw_contents, &savedraw);
		list_del(&savedraw);

		yylex_destroy();
	}
}

static int parse_file(const char *name)
{
	int ret;

	if (!strcmp(name, "-"))
		yyin = stdin;
	else
		yyin = fopen(name, "r");

	fprintf(stderr, "Parsing file %s\n", name);

	parsed_tree = NULL;
	INIT_LIST_HEAD(&raw_contents);
	INIT_LIST_HEAD(&raw_cpp);
	cpp_input = NULL;
	first_token = 0;
	ret = yyparse();
	if (yyin != stdin)
		fclose(yyin);
	yylex_destroy();

	if (ret) {
		fprintf(stderr, "Parser failed with %d\n", ret);
	} else {
		struct parsed_file *pf;
		
		parse_macros();
		if (! (pf = malloc(sizeof(struct parsed_file))) ) {
			perror("Cannot allocate parsed file");
			return -1;
		}
		pf->name = name;
		pf->parsed = parsed_tree;
		pf->raw = raw_contents;
		pf->raw.prev->next = &pf->raw;
		pf->raw.next->prev = &pf->raw;

		list_add_tail(&pf->list, &files);
	}

	return ret;
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

	init_types(predef_types);
	if (i >= argc)
		ret = parse_file("-");
	else {
		while(i < argc)
			if ( (ret = parse_file(argv[i++])) )
				break;
	}

	if (!ret) {
#if DEBUG
		struct parsed_file *pf;
		list_for_each_entry(pf, &files, list) {
			printf("File %s original\n", pf->name);
			dump_tree(pf->parsed);
		}
#endif
		ret = xform_files(&arguments, &files);

#if DEBUG
		list_for_each_entry(pf, &files, list) {
			printf("File %s transformed\n", pf->name);
			dump_tree(pf->parsed);
		}
#endif
	}

	return ret;
}
