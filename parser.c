
/* Check the output for

void (*signal(int sig, void (*func)(int handler_sig)))(int oldhandler_sig);

*/

#include <stdio.h>
#include <string.h>
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

static void dump_basic_type(type_t *type);
static void dump_type(type_t *type, int showflags);
static void dump_expr(expr_t *expr);
static void dump_exprlist(expr_t *list);
static void dump_var(var_t *var);
static void dump_varlist(var_t *list);
static void dump_tree(node_t *tree);

static int indent = 2;
static int depth = -1;

static void init_types(const char **types)
{
	while (*types) {
		addtypedef(*types);
		++types;
	}
}

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
	int i;
	for (i = 0; i < type->b.count; ++i) {
		if (i)
			putchar(' ');
		switch(type->b.list[i]) {
		case VOID:	fputs("void", stdout); break;
		case CHAR:	fputs("char", stdout); break;
		case SHORT:	fputs("short", stdout); break;
		case INT:	fputs("int", stdout); break;
		case LONG:	fputs("long", stdout); break;
		case FLOAT:	fputs("float", stdout); break;
		case DOUBLE:	fputs("double", stdout); break;
		case SIGNED:	fputs("signed", stdout); break;
		case UNSIGNED:	fputs("unsigned", stdout); break;
		default:	fputs("UNKNOWN!", stdout);
		}
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
		printf("struct %s", type->s.name);
		if (type->s.body) {
			fputs(" {\n", stdout);
			dump_tree(type->s.body);
			putchar('}');
		}
		break;

	case type_union:
		printf("union %s", type->s.name);
		if (type->s.body) {
			fputs(" {\n", stdout);
			dump_tree(type->s.body);
			putchar('}');
		}
		break;

	case type_enum:
		printf("enum %s", type->e.name);
		if (type->e.body) {
			fputs(" {\n", stdout);
			dump_varlist(type->e.body);
			putchar('}');
		}
		break;

	case type_pointer:
		fputs("ptr to ", stdout);
		dump_type(type->t, 0);
		break;

	case type_array:
		fputs("array", stdout);
		if (type->a.size) {
			fputs("[\n", stdout);
			dump_exprlist(type->a.size);
			putchar(']');
		}
		fputs(" of ", stdout);
		dump_type(type->a.type, 0);
		break;

	case type_func:
		fputs("func", stdout);
		if (type->f.param) {
			fputs(" (\n", stdout);
			dump_tree(type->f.param);
			putchar(')');
		}
		fputs(" returning ", stdout);
		dump_type(type->f.type, 0);
		break;

	case type_typeof:
		fputs("typeof(\n", stdout);
		dump_exprlist(type->expr);
		putchar(')');
		break;

	default:
		fputs("UNKNOWN!", stdout);
	}

	if (!list_empty(&type->attr)) {
		node_t *attr;
		fputs(" attr(", stdout);
		list_for_each_entry(attr, &type->attr, list) {
			if (&attr->list != type->attr.next)
				putchar(',');
			dump_expr(&attr->e);
		}
		putchar(')');
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
	case SIZEOF_TYPE:
		fputs("sizeof(", stdout);
		dump_type(expr->type, 1);
		putchar(')');
		break;
	case TYPECAST:
	case OFFSETOF:
		dump_op(expr->op);
		putchar('(');
		dump_type(expr->typecast.type, 1);
		putchar(',');
		dump_exprlist(expr->typecast.expr);
		putchar(')');
		break;
	case FRAME_REG:
		dump_op(expr->op);
		putchar('(');
		dump_exprlist(expr->typecast.expr);
		putchar(',');
		dump_type(expr->typecast.type, 1);
		putchar(')');
		break;
	case DECL:
		dump_tree(expr->decl);
		break;

	case ELLIPSIS:
	case CONTINUE:
	case BREAK:
		dump_op(expr->op);
		break;

	case SIZEOF:
	case LABEL:
	case DEFAULT:
	case GOTO:
	case RETURN:
		dump_op(expr->op);
		if (expr->expr) {
			putchar('(');
			dump_exprlist(expr->expr);
			putchar(')');
		}
		break;

	case '~':
	case '!':
	case '=':
	case SHR_ASSIGN:
	case SHL_ASSIGN:
	case ADD_ASSIGN:
	case SUB_ASSIGN:
	case MUL_ASSIGN:
	case DIV_ASSIGN:
	case MOD_ASSIGN:
	case AND_ASSIGN:
	case XOR_ASSIGN:
	case OR_ASSIGN:
	case OR_OP:
	case AND_OP:
	case '|':
	case '^':
	case '&':
	case EQ_OP:
	case NE_OP:
	case LE_OP:
	case GE_OP:
	case '<':
	case '>':
	case SHL_OP:
	case SHR_OP:
	case INC_OP:
	case DEC_OP:
	case '+':
	case '-':
	case '*':
	case '/':
	case '%':
	case ',':
	case '.':
	case PTR_OP:
	case FUNC:
	case ARRAY:
	case SWITCH:
	case WHILE:
	case DO:
	case CASE:
	case RANGE:
		dump_op(expr->op);
		putchar('(');
		if (expr->binary.right) {
			dump_exprlist(expr->binary.left);
			putchar(',');
			dump_exprlist(expr->binary.right);
		} else {
			/* Some operators are both binary and unary */
			dump_exprlist(expr->expr);
		}
		putchar(')');
		break;

	case '?':
	case IF:
	case FOR_CPU_INDEXES:
		dump_op(expr->op);
		putchar('(');
		dump_exprlist(expr->ternary.cond);
		putchar(',');
		dump_exprlist(expr->ternary.ontrue);
		if (expr->ternary.onfalse) {
			putchar(',');
			dump_exprlist(expr->ternary.onfalse);
		}
		putchar(')');
		break;

	case FOR:
		dump_op(expr->op);
		putchar('(');
		if (expr->forloop.init)
			dump_exprlist(expr->forloop.init);
		putchar(',');
		if (expr->forloop.cond)
			dump_exprlist(expr->forloop.cond);
		putchar(',');
		if (expr->forloop.iter)
			dump_exprlist(expr->forloop.iter);
		putchar(',');
		if (expr->forloop.body)
			dump_exprlist(expr->forloop.body);
		putchar(',');
		putchar(')');
		break;

	default:
		fputs("UNKNOWN!", stdout);
	}
}

static void dump_exprlist(expr_t *list)
{
	dump_tree(expr_node(list));
}

static void dump_var(var_t *var)
{
	printf("%*sname: %s\n", indent*depth, "", var->name);
	if (var->type) {
		printf("%*stype: ", indent*depth, "");
		dump_type(var->type, 1);
		putchar('\n');
	}
	if (!list_empty(&var->attr)) {
		node_t *attr;
		list_for_each_entry(attr, &var->attr, list) {
			printf("%*sattribute: ", indent*depth, "");
			dump_expr(&attr->e);
			putchar('\n');
		}
	}
	if (var->init) {
		printf("%*sinit: ", indent*depth, "");
		dump_exprlist(var->init);
		putchar('\n');
	}
}

static void dump_varlist(var_t *list)
{
	dump_tree(var_node(list));
}

static void dump_tree(node_t *tree)
{
	node_t *item = tree;
	int i;

	if (!tree)
		return;

	++depth;
	do {
		dump_chunk(item->first_text, item->last_text);
		switch (item->type) {
		case nt_type: dump_type(&item->t, 1); break;
		case nt_expr: dump_expr(&item->e); break;
		case nt_var:  dump_var (&item->v); break;
		case nt_decl: /* nothing special */ break;
		}

		for (i = 0; i < item->nchild; ++i) {
			if (!item->child[i])
				continue;
			printf("%*schild[%d]: ", indent*depth, "", i);
			dump_tree(item->child[i]);
		}

		item = list_entry(item->list.next, node_t, list);
	} while (item != tree);
	--depth;
}

static void dump_contents(struct list_head *contents)
{
	struct dynstr *ds;
	list_for_each_entry(ds, contents, list)
		fwrite(ds->text, 1, ds->len, stdout);
}

static int parse_file(const char *name)
{
	int ret;

	if (!strcmp(name, "-"))
		yyin = stdin;
	else
		yyin = fopen(name, "r");

	printf("Parsing file %s:\n", name);
	ret = yyparse();
	if (ret) {
		fprintf(stderr, "Parser failed with %d\n", ret);
	} else {
		dump_contents(&raw_contents);
		dump_tree(parsed_tree);
	}

	if (yyin != stdin)
		fclose(yyin);
	yylex_destroy();

	return ret;
}

int main(int argc, char **argv)
{
	int i;
	int ret;
#if DEBUG
	extern int yydebug;
	yydebug = 1;
#endif

	init_types(predef_types);

	if (argc <= 1)
		ret = parse_file("-");
	else {
		for (i = 1; i < argc; ++i)
			if ( (ret = parse_file(argv[i])) )
				break;
	}

	return ret;
}
