#include <stdio.h>
#include <assert.h>

#include "dump.h"
#include "tools.h"
#include "clang.tab.h"

FILE *fdump;

static int indent = 2;
static int depth = -1;

static void dump_basic_type(type_t *type);
static void dump_type(node_t *node, int showflags);
static void dump_expr(node_t *node);
static void dump_var(node_t *node);

/* Mark fake text with reverse video */
#define FAKE_START	"\e[7m"
#define FAKE_END	"\e[27m"

/* Mark expanded macros with underline */
#define EXPANDED_START	"\e[4m"
#define EXPANDED_END	"\e[24m"

void
dump_text(struct dynstr *first, struct dynstr *last, int markup)
{
	struct dynstr *cur = first;
	for (;;) {
		if (markup) {
			if (cur->flags.fake)
				fputs(FAKE_START, fdump);
			if (cur->flags.expanded)
				fputs(EXPANDED_START, fdump);
		}

		fwrite(cur->text, 1, cur->len, fdump);

		if (markup) {
			if (cur->flags.fake)
				fputs(FAKE_END, fdump);
			if (cur->flags.expanded)
				fputs(EXPANDED_END, fdump);
		}

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
			putc(' ', fdump);
		fputs(types[i], fdump);
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
		putc('[', fdump);
		if (type->flags & TF_CONST)
			fputs(" const", fdump);
		if (type->flags & TF_VOLATILE)
			fputs(" volatile", fdump);
		fputs(" ] ", fdump);

		putc('[', fdump);
		if (type->flags & TF_AUTO)
			fputs(" auto", fdump);
		if (type->flags & TF_REGISTER)
			fputs(" register", fdump);
		if (type->flags & TF_STATIC)
			fputs(" static", fdump);
		if (type->flags & TF_EXTERN)
			fputs(" extern", fdump);
		if (type->flags & TF_INLINE)
			fputs(" inline", fdump);
		if (type->flags & TF_TYPEDEF)
			fputs(" typedef", fdump);
		fputs(" ] ", fdump);
	}

	switch(type->category) {
	case type_basic:
		dump_basic_type(type);
		break;

	case type_typedef:
		fputs(node->str->text, fdump);
		break;

	case type_struct:
		fprintf(fdump, "struct %s", name_string(node));
		break;

	case type_union:
		fprintf(fdump, "union %s", name_string(node));
		break;

	case type_enum:
		fprintf(fdump, "enum %s", name_string(node));
		break;

	case type_pointer:
		fputs("ptr to ", fdump);
		break;

	case type_array:
		fputs("array", fdump);
		break;

	case type_func:
		fputs("func", fdump);
		break;

	case type_typeof:
		fputs("typeof ", fdump);
		break;

	default:
		fputs("UNKNOWN!", fdump);
	}
}

static void
dump_op(int op)
{
	switch(op) {
	case SHL_ASSIGN:	fputs("<<=", fdump); break;
	case SHR_ASSIGN:	fputs(">>=", fdump); break;
	case ADD_ASSIGN:	fputs("+=", fdump); break;
	case SUB_ASSIGN:	fputs("-=", fdump); break;
	case MUL_ASSIGN:	fputs("*=", fdump); break;
	case DIV_ASSIGN:	fputs("/=", fdump); break;
	case MOD_ASSIGN:	fputs("%=", fdump); break;
	case AND_ASSIGN:	fputs("&=", fdump); break;
	case XOR_ASSIGN:	fputs("^=", fdump); break;
	case OR_ASSIGN:		fputs("|=", fdump); break;
	case OR_OP:	fputs("||", fdump); break;
	case AND_OP:	fputs("&&", fdump); break;
	case EQ_OP:	fputs("==", fdump); break;
	case NE_OP:	fputs("!=", fdump); break;
	case LE_OP:	fputs("<=", fdump); break;
	case GE_OP:	fputs(">=", fdump); break;
	case SHL_OP:	fputs("<<", fdump); break;
	case SHR_OP:	fputs(">>", fdump); break;
	case INC_OP:	fputs("++", fdump); break;
	case DEC_OP:	fputs("--", fdump); break;
	case '?':	fputs("?:", fdump); break;
	case RANGE:	fputs("...", fdump); break;
	case PTR_OP:	fputs("=>", fdump); break;
	case ADDR_OF:   fputs("&", fdump); break;
	case DEREF_OP:  fputs("*", fdump); break;
	case SIZEOF_TYPE:
	case SIZEOF:	fputs("sizeof", fdump); break;
	case FUNC:	fputs("call", fdump); break;
	case ARRAY:	fputs("idx", fdump); break;
	case '{':	fputs("block", fdump); break;
	case LABEL:	fputs("label", fdump); break;
	case CASE:	fputs("case", fdump); break;
	case DEFAULT:	fputs("default", fdump); break;
	case IF:	fputs("if", fdump); break;
	case SWITCH:	fputs("switch", fdump); break;
	case WHILE:	fputs("while", fdump); break;
	case DO:	fputs("dowhile", fdump); break;
	case FOR:	fputs("for", fdump); break;
	case GOTO:	fputs("goto", fdump); break;
	case CONTINUE:	fputs("continue", fdump); break;
	case BREAK:	fputs("break", fdump); break;
	case RETURN:	fputs("return", fdump); break;
	case ELLIPSIS:	fputs("...", fdump); break;
	case TYPECAST:	fputs("typecast", fdump); break;
	case CONCAT:	fputs("concat", fdump); break;
	case BLOCK:	fputs("block", fdump); break;

	case CPP_IF:	fputs("#if", fdump); break;
	case CPP_IFDEF:	fputs("#ifdef", fdump); break;
	case CPP_IFNDEF:	fputs("#ifndef", fdump); break;
	case CPP_ELIF:	fputs("#elif", fdump); break;
	case CPP_ELSE:	fputs("#else", fdump); break;
	case CPP_ENDIF:	fputs("#endif", fdump); break;

	default:
		if (op <= 255)
			fprintf(fdump, "%c", op);
		else
			fprintf(fdump, "op%d", op);
	}
}

static void
dump_expr(node_t *node)
{
	expr_t *expr = &node->e;

	switch (expr->op) {
	case INT_CONST:
		fprintf(fdump, "%ld", expr->num);
		break;
	case FLOAT_CONST:
		fprintf(fdump, "%f", expr->f);
		break;
	case ID:
	case STRING_CONST:
	case CHAR_CONST:
		fputs(node->str->text, fdump);
		break;
	default:
		dump_op(expr->op);
	}
}

static void
dump_var(node_t *node)
{
	fprintf(fdump, "name: %s", name_string(node));
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
				fputs("cht_body", fdump);
			else if (parent->t.category == type_typeof)
				fputs("cht_expr", fdump);
			else
				fputs("cht_type", fdump);
			break;

		case 1:
			if (parent->t.category == type_array)
				fputs("cht_size", fdump);
			else
				fputs("cht_param", fdump);
			break;

		case cht_attr:
			fputs("cht_attr", fdump);
			break;

		default:
			fprintf(fdump, "cht_%d", pos);
		}
		break;

	case nt_expr:
		fprintf(fdump, "che_arg%d", pos + 1);
		break;

	case nt_var:
		if (pos < chv_max)
			fputs(chv_names[pos], fdump);
		else
			fprintf(fdump, "chv_%d", pos);
		break;

	case nt_decl:
		if (pos < chd_max)
			fputs(chd_names[pos], fdump);
		else
			fprintf(fdump, "chd_%d", pos);
		break;
	}
}

static void
dump_node(node_t *node)
{
	int i;

	++depth;

	fprintf(fdump, "%*s(>>>", depth*indent, "");
	dump_node_text(node, 1);
	fprintf(fdump, "<<<\n%*s", depth*indent, "");
	switch (node->type) {
	case nt_type: dump_type(node, 1); break;
	case nt_expr: dump_expr(node); break;
	case nt_var:  dump_var (node); break;
	case nt_decl: fputs("decl", fdump); break;
	}
	putc('\n', fdump);

	for (i = 0; i < node->nchild; ++i) {
		if (!list_empty(&node->child[i])) {
			fprintf(fdump, "%*s[", depth*indent, "");
			dump_child_pos(node, i);
			fprintf(fdump, "]:\n");
		}
		dump_tree(&node->child[i]);
	}
	fprintf(fdump, "%*s)\n", depth*indent, "");

	--depth;
}

void
dump_tree(struct list_head *tree)
{
	node_t *item;
	list_for_each_entry(item, tree, list)
		dump_node(item);
}

void
shortdump_type(node_t *type)
{
	assert(type->type == nt_type);
	dump_type(type, 0);

	if (type->t.category == type_pointer ||
	    type->t.category == type_typeof ||
	    type->t.category == type_array ||
	    type->t.category == type_func) {
		node_t *child = first_node(&type->child[cht_type]);
		if (&child->list == &type->child[cht_type])
			return;

		if (type->t.category == type_array) {
			node_t *size = first_node(&type->child[cht_size]);
			if (&size->list != &type->child[cht_size]) {
				putc('[', fdump);
				dump_node_text(size, 0);
				putc(']', fdump);
			}
			fputs(" of ", fdump);
		} else if (type->t.category == type_func)
			fputs(" returning ", fdump);
		shortdump_type(child);
	}
}

void
shortdump_scope(node_t *node)
{
	node_t *parent;
	struct list_head *scope = find_scope(node, &parent);
	if (!scope) {
		fputs("<global>", fdump);
		return;
	}

	if (scope == &node->pf->parsed) {
		fputs("<file>", fdump);
		return;
	}
	if (parent->type == nt_decl) {
		node_t *var = first_node(&parent->child[chd_var]);
		if (&var->list == &parent->child[chd_var])
			fputs("<novar>", fdump);
		else
			fputs(var->str->text, fdump);
	} else if (parent->type == nt_type)
		shortdump_type(parent);
	else
		fputs("<unknown>", fdump);
}

void
shortdump_var(node_t *var)
{
	assert(var->type == nt_var);
	fprintf(fdump, "%s:", node_file_name(var));
	shortdump_scope(var);
	fprintf(fdump, ":%s", var->str->text);
}

void
dump_ind(const ind_t *ind)
{
	for (;;)
		switch(*ind++) {
		case ind_stop:
			fputs("base type", fdump);
			return;
		case ind_implicit:
			fputs("implicit ", fdump);
			/* fall through */
		case ind_pointer:
			fputs("pointer to ", fdump);
			break;
		case ind_return:
			fputs("function returning ", fdump);
			break;
		default:
			fprintf(fdump, "function with arg #%d ", ind[-1]);
		}
}

void
shortdump_varind(node_t *var, ind_t *ind)
{
	shortdump_var(var);
	fputs(" as ", fdump);
	dump_ind(ind);
}
