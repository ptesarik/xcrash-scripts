#include <stdio.h>

#include "parser.h"
#include "clang.tab.h"

static int indent = 2;
static int depth = -1;

static void dump_basic_type(type_t *type);
static void dump_type(node_t *node, int showflags);
static void dump_expr(node_t *node);
static void dump_var(node_t *node);

void
dump_text(struct dynstr *first, struct dynstr *last)
{
	struct dynstr *cur = first;
	for (;;) {
		fwrite(cur->text, 1, cur->len, stdout);

		if (cur == last)
			break;
		cur = list_entry(cur->list.next, struct dynstr, list);
	}
}

static void
dump_basic_type(type_t *type)
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

static const char *
name_string(node_t *node)
{
	return node->str ? node->str->text : "<anonymous>";
}

static void
dump_type(node_t *node, int showflags)
{
	type_t *type = &node->t;
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
		fputs(node->str->text, stdout);
		break;

	case type_struct:
		printf("struct %s", name_string(node));
		break;

	case type_union:
		printf("union %s", name_string(node));
		break;

	case type_enum:
		printf("enum %s", name_string(node));
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

static void
dump_op(int op)
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
	case ADDR_OF:   fputs("&", stdout); break;
	case DEREF_OP:  fputs("*", stdout); break;
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

static void
dump_expr(node_t *node)
{
	expr_t *expr = &node->e;

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
		fputs(node->str->text, stdout);
		break;
	default:
		dump_op(expr->op);
	}
}

static void
dump_var(node_t *node)
{
	printf("name: %s", name_string(node));
}

static void
dump_child_pos(node_t *parent, int pos)
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

static void
dump_node(node_t *node)
{
	int i;

	++depth;

	printf("%*s(", depth*indent, "");
	fputs(">>>", stdout);
	dump_text(node->first_text, node->last_text);
	fputs("<<<\n", stdout);
	printf("%*s", depth*indent, "");
	switch (node->type) {
	case nt_type: dump_type(node, 1); break;
	case nt_expr: dump_expr(node); break;
	case nt_var:  dump_var (node); break;
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

void
dump_tree(struct list_head *tree)
{
	node_t *item;
	list_for_each_entry(item, tree, list)
		dump_node(item);
}
