/* ANSI C grammar, Lex specification          -*- mode: c; -*-
 *
 * In 1985, Jeff Lee published this Lex specification together with
 * a Yacc grammar for the April 30, 1985 ANSI C draft.  Tom Stockfisch
 * reposted both to net.sources in 1987; that original, as mentioned
 * in the answer to question 17.25 of the comp.lang.c FAQ, can be
 * ftp'ed from ftp.uu.net, file usenet/net.sources/ansi.c.grammar.Z.
 *
 * I intend to keep this version as close to the current C Standard
 * grammar as possible; please let me know if you discover
 * discrepancies.
 *
 * Jutta Degener, 1995
 *
 * Adapted 2011 by Petr Tesarik to work with crash sources.
 */

%{
#include <stdlib.h>

#include "parser.h"
#include "clang.tab.h"
#include "tools.h"

int start_symbol;

struct list_head raw_contents;
struct list_head raw_cpp;
struct dynstr *lex_input_first, *lex_input_last;

static int token_at_eof;
static struct dynstr *lex_input;
static char *lex_input_ptr;

static struct cpp_cond_state cond_state;
static node_t *cpp_cond;

static void store_yytext(YYLTYPE *loc, const char *s);
#define YY_USER_ACTION	store_yytext(loc, yytext);

static void init_vars(void);
#define YY_USER_INIT	init_vars()

static int store_token(YYSTYPE *, YYLTYPE *, int);
static int store_btype(YYSTYPE *, YYLTYPE *, int);

#define STORE_TOKEN(tok)	(store_token(val, loc, (tok)))
#define STORE_BTYPE(btype)	(store_btype(val, loc, (btype)))

static size_t dynstr_read(void *, size_t);
static int parse_cpp(void);

#define YY_INPUT(buf,result,max_size) \
	if (lex_input) { \
		result = dynstr_read(buf, max_size); \
	} else { \
		errno=0; \
		while ( (result = fread(buf, 1, max_size, yyin))==0 && ferror(yyin)) { \
			if(errno != EINTR) { \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
			} \
			errno=0; \
			clearerr(yyin); \
		} \
	}

#define YY_DECL	int yylex(YYSTYPE *val, YYLTYPE *loc)

%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
WS			[ \t\v\f]
CPP			("#"|"%:"){WS}*

%s cpp
%s cpp_condition
%x cpp_dir
%x cpp_def
%x cpp_ign
%x cpp_recover

%%
%{
	if (start_symbol) {
		int ret = start_symbol;
		start_symbol = 0;
		return ret;
	}
%}

<*>"/*"([^*]+|\*+[^*/])*\*+\/	/* ignore comments */
<*>"//".*		/* ignore line comments */

  /* CPP directive */
^{WS}*{CPP}	{
			BEGIN(cpp_dir);
			if (parse_cpp() && YY_START != INITIAL) {
				loc->last_text->reuse = 1;
				BEGIN(cpp_recover);
			}
		}
<cpp_recover>\n	{
			loc->last_text->reuse = 0;
			BEGIN(INITIAL);
		}
  /* skip whitespace except newline */
<cpp,cpp_condition,cpp_dir,cpp_def>{WS}+
  /* A newline terminates the directive */
<cpp,cpp_condition,cpp_dir,cpp_def,cpp_ign>\n	{
			/* Simulate a terminating semicolon at the end
			 * of each macro. This is often needed to make
			 * the macro body a valid statement, because
			 * a semicolon is assumed after the macro when
			 * used. And in the worst case we'll add an
			 * empty expression at the end.
			 */
			if (token_at_eof) {
				struct dynstr *ds = loc->last_text;
				int ret = token_at_eof;
				token_at_eof = 0;

				yyless(0);
				ds->text[--ds->len] = 0;
				return STORE_TOKEN(ret);
			}

			loc->last_text->reuse = 0;
			BEGIN(INITIAL);
			yyterminate();
		}

  /* CPP directive (start) */
<cpp_dir>{
"define"		{
			token_at_eof = ';';
			BEGIN(cpp_def);
			return STORE_TOKEN(CPP_DEFINE);
			}
"if"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return STORE_TOKEN(CPP_IF);
			}
"ifdef"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return STORE_TOKEN(CPP_IFDEF);
			}
"ifndef"		{
			BEGIN(cpp); BEGIN(cpp_condition);
			return STORE_TOKEN(CPP_IFNDEF);
			}
"elif"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return STORE_TOKEN(CPP_ELIF);
			}
"else"			{
			BEGIN(cpp_ign);
			return STORE_TOKEN(CPP_ELSE);
			}
"endif"			{
			BEGIN(cpp_ign);
			return STORE_TOKEN(CPP_ENDIF);
			}
(?s:.)			{
			loc->last_text->reuse = 1;
			BEGIN(cpp_ign);
			}
}

  /* CPP define name */
<cpp_def>{L}({L}|{D})*/\(	{
				BEGIN(cpp);
				return STORE_TOKEN(CPP_IDARG);
				}
<cpp_def>{L}({L}|{D})*		{
				BEGIN(cpp);
				return STORE_TOKEN(ID);
				}

  /* CPP directive (during define) */
<cpp>"#"		return STORE_TOKEN('#');
<cpp>"##"		return STORE_TOKEN(CPP_CONCAT);

<cpp_condition>"defined"	return STORE_TOKEN(CPP_DEFINED);
<cpp_condition>{L}({L}|{D})*	return STORE_TOKEN(ID);

<cpp_ign,cpp_recover>.	/* eat up an ignored directive */

"..."			return STORE_TOKEN(ELLIPSIS);
">>="			return STORE_TOKEN(SHR_ASSIGN);
"<<="			return STORE_TOKEN(SHL_ASSIGN);
"+="			return STORE_TOKEN(ADD_ASSIGN);
"-="			return STORE_TOKEN(SUB_ASSIGN);
"*="			return STORE_TOKEN(MUL_ASSIGN);
"/="			return STORE_TOKEN(DIV_ASSIGN);
"%="			return STORE_TOKEN(MOD_ASSIGN);
"&="			return STORE_TOKEN(AND_ASSIGN);
"^="			return STORE_TOKEN(XOR_ASSIGN);
"|="			return STORE_TOKEN(OR_ASSIGN);
">>"			return STORE_TOKEN(SHR_OP);
"<<"			return STORE_TOKEN(SHL_OP);
"++"			return STORE_TOKEN(INC_OP);
"--"			return STORE_TOKEN(DEC_OP);
"->"			return STORE_TOKEN(PTR_OP);
"&&"			return STORE_TOKEN(AND_OP);
"||"			return STORE_TOKEN(OR_OP);
"<="			return STORE_TOKEN(LE_OP);
">="			return STORE_TOKEN(GE_OP);
"=="			return STORE_TOKEN(EQ_OP);
"!="			return STORE_TOKEN(NE_OP);
";"			return STORE_TOKEN(';');
("{"|"<%")		return STORE_TOKEN('{');
("}"|"%>")		return STORE_TOKEN('}');
","			return STORE_TOKEN(',');
":"			return STORE_TOKEN(':');
"="			return STORE_TOKEN('=');
"("			return STORE_TOKEN('(');
")"			return STORE_TOKEN(')');
("["|"<:")		return STORE_TOKEN('[');
("]"|":>")		return STORE_TOKEN(']');
"."			return STORE_TOKEN('.');
"&"			return STORE_TOKEN('&');
"!"			return STORE_TOKEN('!');
"~"			return STORE_TOKEN('~');
"-"			return STORE_TOKEN('-');
"+"			return STORE_TOKEN('+');
"*"			return STORE_TOKEN('*');
"/"			return STORE_TOKEN('/');
"%"			return STORE_TOKEN('%');
"<"			return STORE_TOKEN('<');
">"			return STORE_TOKEN('>');
"^"			return STORE_TOKEN('^');
"|"			return STORE_TOKEN('|');
"?"			return STORE_TOKEN('?');

"__attribute__"		return STORE_TOKEN(ATTRIBUTE);
"auto"			return STORE_TOKEN(AUTO);
"break"			return STORE_TOKEN(BREAK);
"case"			return STORE_TOKEN(CASE);
"const"			return STORE_TOKEN(CONST);
"continue"		return STORE_TOKEN(CONTINUE);
"default"		return STORE_TOKEN(DEFAULT);
"do"			return STORE_TOKEN(DO);
"else"			return STORE_TOKEN(ELSE);
"enum"			return STORE_TOKEN(ENUM);
"extern"		return STORE_TOKEN(EXTERN);
"for"			return STORE_TOKEN(FOR);
"goto"			return STORE_TOKEN(GOTO);
"if"			return STORE_TOKEN(IF);
"inline"		return STORE_TOKEN(INLINE);
"__inline__"		return STORE_TOKEN(INLINE);
"register"		return STORE_TOKEN(REGISTER);
"return"		return STORE_TOKEN(RETURN);
"sizeof"		return STORE_TOKEN(SIZEOF);
"static"		return STORE_TOKEN(STATIC);
"struct"		return STORE_TOKEN(STRUCT);
"switch"		return STORE_TOKEN(SWITCH);
"typedef"		return STORE_TOKEN(TYPEDEF);
"__typeof__"		return STORE_TOKEN(TYPEOF);
"union"			return STORE_TOKEN(UNION);
"volatile"		return STORE_TOKEN(VOLATILE);
"while"			return STORE_TOKEN(WHILE);

"char"			return STORE_BTYPE(TYPE_CHAR);
"double"		return STORE_BTYPE(TYPE_DOUBLE);
"int"			return STORE_BTYPE(TYPE_INT);
"float"			return STORE_BTYPE(TYPE_FLOAT);
"long"			return STORE_BTYPE(TYPE_LONG);
"short"			return STORE_BTYPE(TYPE_SHORT);
"signed"		return STORE_BTYPE(TYPE_SIGNED);
"unsigned"		return STORE_BTYPE(TYPE_UNSIGNED);
"void"			return STORE_BTYPE(TYPE_VOID);

	/* HACKs */
"FRAME_REG"		return STORE_TOKEN(FRAME_REG);
"for_cpu_indexes"	return STORE_TOKEN(FOR_CPU_INDEXES);
"offsetof"		return STORE_TOKEN(OFFSETOF);
"struct_offset"		return STORE_TOKEN(OFFSETOF);
"ATTRIBUTE_UNUSED"	return STORE_TOKEN(ATTRIBUTE_UNUSED);

{L}({L}|{D})*		|
3COM({L}|{D})*		|	/* HACK */
3DFX({L}|{D})*		|	/* HACK */
3DLABS({L}|{D})*	{	/* HACK */
				if (istypedef(yytext))
					return STORE_TOKEN(TYPEID);
				return STORE_TOKEN(ID);
			}

0[xX]{H}+{IS}?		|
0{D}+{IS}?		|
{D}+{IS}?		return STORE_TOKEN(INT_CONST);

{D}+{E}{FS}?		|
{D}*"."{D}+({E})?{FS}?	|
{D}+"."{D}*({E})?{FS}?	return STORE_TOKEN(FLOAT_CONST);

L?\'(?s:\\.|[^\\'])*\'	/* (hilight) ' */ return STORE_TOKEN(CHAR_CONST);

L?\"(?s:\\.|[^\\"])*\"	/* (hilight) " */ return STORE_TOKEN(STRING_CONST);

<*>\\\n?		/* continuation lines */
<INITIAL>[ \t\v\n\f]+	/* skip whitespace */
.			/* ignore bad characters */

%%

int yywrap(void)
{
	return 1;
}

int tabsize = 8;

static struct dynstr *curds;
static size_t curdsoff;

static void init_vars(void)
{
	token_at_eof = 0;
	if (lex_input_first) {
		lex_input = lex_input_first;
		lex_input_ptr = lex_input->text;
		curds = lex_input;
		curdsoff = 0;
	} else {
		lex_input = NULL;
		curds = NULL;
	}

	memset(&cond_state, 0, sizeof cond_state);
	cpp_cond = NULL;
}

struct dynstr *newdynstr(const char *s, size_t len)
{
	struct dynstr *ds = malloc(sizeof(struct dynstr) + len + 1);
	INIT_LIST_HEAD(&ds->list);
	INIT_LIST_HEAD(&ds->cpp_list);
	INIT_LIST_HEAD(&ds->node_first);
	INIT_LIST_HEAD(&ds->node_last);
	ds->cpp_cond = NULL;
	ds->token = 0;
	ds->refcount = 0;
	ds->len = len;
	ds->alloc = len + 1;
	ds->reuse = 0;
	if (s)
		memcpy(ds->text, s, len);
	ds->text[ds->len] = '\0';
	return ds;
}

void freedynstr(struct dynstr *ds)
{
	if (ds->refcount) {
		fputs("Attempt to free an in-use dynstr!\n", stderr);
		abort();
	}
	free(ds);
}

#define ALLOC_INC	256

/* Try to reuse existing dynstr */
static struct dynstr *
reuse_last_content(const char *s, size_t len)
{
	struct dynstr *ds;

	if (list_empty(&raw_contents))
		return NULL;

	ds = list_entry(raw_contents.prev, struct dynstr, list);
	if (!ds->reuse)
		return NULL;

	if (ds->len + len + 1 > ds->alloc) {
		struct dynstr tmp;
		list_add_tail(&tmp.list, &ds->list);
		list_del(&ds->list);
		list_add_tail(&tmp.cpp_list, &ds->cpp_list);
		list_del(&ds->cpp_list);
		list_add_tail(&tmp.node_first, &ds->node_first);
		list_del(&ds->node_first);
		list_add_tail(&tmp.node_last, &ds->node_last);
		list_del(&ds->node_last);

		size_t newalloc = ds->len + len + 1 + ALLOC_INC;
		newalloc -= newalloc % ALLOC_INC;
		ds = realloc(ds, sizeof(struct dynstr) + newalloc);
		ds->alloc = newalloc;

		/* Fix up list pointers */
		list_add(&ds->list, &tmp.list);
		list_del(&tmp.list);
		list_add(&ds->cpp_list, &tmp.cpp_list);
		list_del(&tmp.cpp_list);
		list_add(&ds->node_first, &tmp.node_first);
		list_del(&tmp.node_first);
		list_add(&ds->node_last, &tmp.node_last);
		list_del(&tmp.node_last);
	}
	memcpy(ds->text + ds->len, s, len);
	ds->len += len;
	ds->text[ds->len] = '\0';
	return ds;
}

static void store_yytext(YYLTYPE *loc, const char *s)
{
	struct dynstr *ds;
	int i;
	loc->first_line = loc->last_line;
	loc->first_column = loc->last_column;
	for (i = 0; i < yyleng; ++i) {
		if (yytext[i] == '\n') {
			loc->last_line++;
			loc->last_column = 0;
		} else if (yytext[i] == '\t')
			loc->last_column += tabsize -
				(loc->last_column % tabsize);
		else
			loc->last_column++;
	}

	if (curds) {
		curdsoff += yyleng;
		while(curdsoff > curds->len || !curds->len) {
			curdsoff -= curds->len;
			curds = list_entry(curds->list.next,
					   struct dynstr, list);
		}
	}

	if (! (ds = reuse_last_content(s, yyleng)) ) {
		ds = newdynstr(s, yyleng);
		list_add_tail(&ds->list, &raw_contents);
	}
	ds->cpp_cond = cpp_cond;
	loc->first_text = loc->last_text = ds;
}

static int
store_token(YYSTYPE *val, YYLTYPE *loc, int token)
{
	if (token == ID || token == TYPEID || token == CPP_IDARG ||
	    token == INT_CONST || token == FLOAT_CONST ||
	    token == CHAR_CONST || token == STRING_CONST)
		val->str = loc->first_text;
	else if (token != BASIC_TYPE)
		val->token = token;
	loc->last_text->token = token;
	return token;
}

static int
store_btype(YYSTYPE *val, YYLTYPE *loc, int btype)
{
	val->btype = btype;
	return store_token(val, loc, BASIC_TYPE);
}

static size_t
dynstr_read(void *buf, size_t max_size)
{
	size_t n;

	while (! (n = lex_input->len - (lex_input_ptr - lex_input->text)) &&
	       lex_input != lex_input_last) {
		lex_input = list_entry(lex_input->list.next,
				       struct dynstr, list);
		lex_input_ptr = lex_input->text;
	}

	if (n > max_size)
		n = max_size;
	memcpy(buf, lex_input_ptr, n);
	lex_input_ptr += n;
	return n;
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

/* Discard the result of parsing */
static void
discard_parsing(void)
{
	node_t *node, *nnode;
	list_for_each_entry_safe(node, nnode, &parsed_tree, list)
		freenode(node);
}

static int
parse_cpp(void)
{
	struct list_head savedparsed;
	struct dynstr *ds;
	int ret;

	/* Save the original parsed_tree and start a new one */
	list_add(&savedparsed, &parsed_tree);
	list_del_init(&parsed_tree);

	ds = newdynstr(NULL, 0);
	ds->reuse = 1;
	list_add_tail(&ds->list, &raw_contents);

	start_symbol = START_DIRECTIVE;
	ret = yyparse();

	if (!ret && is_cpp_cond(&parsed_tree))
		cpp_cond = get_cpp_cond(&cond_state, &parsed_tree);
	if (ret || !is_define(&parsed_tree))
		discard_parsing();

	list_splice(&savedparsed, &parsed_tree);

	return ret;
}
