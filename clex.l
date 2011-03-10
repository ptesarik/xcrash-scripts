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

int start_symbol;
int lex_cpp_mode;

struct list_head raw_contents;
struct list_head raw_cpp;
struct dynstr *lex_input_first, *lex_input_last;

static int token_at_eof;
static struct dynstr *lex_input;
static char *lex_input_ptr;

static void store_yytext(const char *s);
#define YY_USER_ACTION	store_yytext(yytext);

static void init_vars(void);
#define YY_USER_INIT	init_vars()

static size_t dynstr_read(void *, size_t);
static int lex_input_type(void);

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

%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
CPP			("#"|"%:")[ \t\v\f]*

%s cpp
%s cpp_condition
%x cpp_dir
%x cpp_def
%x cpp_ign

%%
%{
	if (start_symbol) {
		int ret = start_symbol;
		start_symbol = 0;
		return ret;
	}
%}

<*>"/*"([^*]+|\*+[^*/])*\*+\/	/* ignore comments */
<*>"//".*\n?		/* ignore line comments */

  /* CPP directive (start) */
<cpp_dir>{
{CPP}			/* skip whitespace except newline */
"define"		{
			token_at_eof = ';';
			BEGIN(cpp_def);
			return yylval.token = CPP_DEFINE;
			}
"if"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return yylval.token = CPP_IF;
			}
"ifdef"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return yylval.token = CPP_IFDEF;
			}
"ifndef"		{
			BEGIN(cpp); BEGIN(cpp_condition);
			return yylval.token = CPP_IFNDEF;
			}
"elif"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return yylval.token = CPP_ELIF;
			}
"else"			{
			BEGIN(cpp_ign);
			return yylval.token = CPP_ELSE;
			}
"endif"			{
			BEGIN(cpp_ign);
			return yylval.token = CPP_ENDIF;
			}
(?s:.)			{
			yylloc.last_text->reuse = 1;
			BEGIN(cpp_ign);
			}
}

  /* CPP define name */
<cpp_def>[ \t\v\f]+		/* skip whitespace except newline */
<cpp_def>{L}({L}|{D})*/\(	{
				BEGIN(cpp);
				yylval.str = yylloc.first_text;
				return CPP_IDARG;
				}
<cpp_def>{L}({L}|{D})*		{
				BEGIN(cpp);
				yylval.str = yylloc.first_text;
				return ID;
				}

  /* CPP directive (during define) */
<cpp>"##"		return yylval.token = CPP_CONCAT;
<cpp><<EOF>>		{
			/* Simulate a terminating semicolon at the end
			 * of each macro. This is often needed to make
			 * the macro body a valid statement, because
			 * a semicolon is assumed after the macro when
			 * used. And in the worst case we'll add an
			 * empty expression at the end.
			 */
				if (!token_at_eof)
					yyterminate();
				else {
					yylval.token = token_at_eof;
					token_at_eof = 0;
					return yylval.token;
				}
			}

<cpp_condition>"defined"	return yylval.token = CPP_DEFINED;
<cpp_condition>{L}({L}|{D})*	{
				yylval.str = yylloc.first_text;
				return ID;
				}

  /* CPP ignored directive */
{CPP}	{
	if (!lex_input)
		list_add_tail(&yylloc.last_text->cpp_list, &raw_cpp);
	BEGIN(cpp_ign);
	yylloc.last_text->reuse = 1;
	}
<cpp_ign>(.|\\\n)	/* and eat up the directive */
<cpp_ign>\n		{
	yylloc.last_text->reuse = 0;
	BEGIN(INITIAL);
	}

"..."			return yylval.token = ELLIPSIS;
">>="			return yylval.token = SHR_ASSIGN;
"<<="			return yylval.token = SHL_ASSIGN;
"+="			return yylval.token = ADD_ASSIGN;
"-="			return yylval.token = SUB_ASSIGN;
"*="			return yylval.token = MUL_ASSIGN;
"/="			return yylval.token = DIV_ASSIGN;
"%="			return yylval.token = MOD_ASSIGN;
"&="			return yylval.token = AND_ASSIGN;
"^="			return yylval.token = XOR_ASSIGN;
"|="			return yylval.token = OR_ASSIGN;
">>"			return yylval.token = SHR_OP;
"<<"			return yylval.token = SHL_OP;
"++"			return yylval.token = INC_OP;
"--"			return yylval.token = DEC_OP;
"->"			return yylval.token = PTR_OP;
"&&"			return yylval.token = AND_OP;
"||"			return yylval.token = OR_OP;
"<="			return yylval.token = LE_OP;
">="			return yylval.token = GE_OP;
"=="			return yylval.token = EQ_OP;
"!="			return yylval.token = NE_OP;
";"			return yylval.token = ';';
("{"|"<%")		return yylval.token = '{';
("}"|"%>")		return yylval.token = '}';
","			return yylval.token = ',';
":"			return yylval.token = ':';
"="			return yylval.token = '=';
"("			return yylval.token = '(';
")"			return yylval.token = ')';
("["|"<:")		return yylval.token = '[';
("]"|":>")		return yylval.token = ']';
"."			return yylval.token = '.';
"&"			return yylval.token = '&';
"!"			return yylval.token = '!';
"~"			return yylval.token = '~';
"-"			return yylval.token = '-';
"+"			return yylval.token = '+';
"*"			return yylval.token = '*';
"/"			return yylval.token = '/';
"%"			return yylval.token = '%';
"<"			return yylval.token = '<';
">"			return yylval.token = '>';
"^"			return yylval.token = '^';
"|"			return yylval.token = '|';
"?"			return yylval.token = '?';

"__attribute__"		return yylval.token = ATTRIBUTE;
"auto"			return yylval.token = AUTO;
"break"			return yylval.token = BREAK;
"case"			return yylval.token = CASE;
"const"			return yylval.token = CONST;
"continue"		return yylval.token = CONTINUE;
"default"		return yylval.token = DEFAULT;
"do"			return yylval.token = DO;
"else"			return yylval.token = ELSE;
"enum"			return yylval.token = ENUM;
"extern"		return yylval.token = EXTERN;
"for"			return yylval.token = FOR;
"goto"			return yylval.token = GOTO;
"if"			return yylval.token = IF;
"inline"		return yylval.token = INLINE;
"__inline__"		return yylval.token = INLINE;
"register"		return yylval.token = REGISTER;
"return"		return yylval.token = RETURN;
"sizeof"		return yylval.token = SIZEOF;
"static"		return yylval.token = STATIC;
"struct"		return yylval.token = STRUCT;
"switch"		return yylval.token = SWITCH;
"typedef"		return yylval.token = TYPEDEF;
"__typeof__"		return yylval.token = TYPEOF;
"union"			return yylval.token = UNION;
"volatile"		return yylval.token = VOLATILE;
"while"			return yylval.token = WHILE;

"char"			yylval.btype = TYPE_CHAR;	return BASIC_TYPE;
"double"		yylval.btype = TYPE_DOUBLE;	return BASIC_TYPE;
"int"			yylval.btype = TYPE_INT;	return BASIC_TYPE;
"float"			yylval.btype = TYPE_FLOAT;	return BASIC_TYPE;
"long"			yylval.btype = TYPE_LONG;	return BASIC_TYPE;
"short"			yylval.btype = TYPE_SHORT;	return BASIC_TYPE;
"signed"		yylval.btype = TYPE_SIGNED;	return BASIC_TYPE;
"unsigned"		yylval.btype = TYPE_UNSIGNED;	return BASIC_TYPE;
"void"			yylval.btype = TYPE_VOID;	return BASIC_TYPE;

	/* HACKs */
"FRAME_REG"		return yylval.token = FRAME_REG;
"for_cpu_indexes"	return yylval.token = FOR_CPU_INDEXES;
"offsetof"		return yylval.token = OFFSETOF;
"struct_offset"		return yylval.token = OFFSETOF;

{L}({L}|{D})*		|
3COM({L}|{D})*		|	/* HACK */
3DFX({L}|{D})*		|	/* HACK */
3DLABS({L}|{D})*	{	/* HACK */
				int type;
				yylval.str = yylloc.first_text;
				if( (type = lex_input_type()) >= 0)
					return type;
				else if (istypedef(yytext))
					return TYPEID;
				return ID;
			}

0[xX]{H}+{IS}?		|
0{D}+{IS}?		|
{D}+{IS}?		{
				yylval.str = yylloc.first_text;
				return INT_CONST;
			}

{D}+{E}{FS}?		|
{D}*"."{D}+({E})?{FS}?	|
{D}+"."{D}*({E})?{FS}?	{
				yylval.str = yylloc.first_text;
				return FLOAT_CONST;
			}

L?\'(?s:\\.|[^\\'])*\'	{ /* fix highlight: ' */
				yylval.str = yylloc.first_text;
				return CHAR_CONST;
			}
L?\"(?s:\\.|[^\\"])*\"	{ /* fix highlight: " */
				yylval.str = yylloc.first_text;
				return STRING_CONST;
			}
<*>\\\n?		/* continuation lines */
[ \t\v\n\f]+		/* skip whitespace */
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
	if (lex_cpp_mode)
		BEGIN(cpp_dir);
}

struct dynstr *newdynstr(const char *s, size_t len)
{
	struct dynstr *ds = malloc(sizeof(struct dynstr) + len + 1);
	INIT_LIST_HEAD(&ds->cpp_list);
	INIT_LIST_HEAD(&ds->node_first);
	INIT_LIST_HEAD(&ds->node_last);
	ds->cpp_cond = NULL;
	ds->len = len;
	ds->alloc = len + 1;
	ds->refcount = 0;
	ds->reuse = 0;
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

static void store_yytext(const char *s)
{
	struct dynstr *ds;
	int i;
	yylloc.first_line = yylloc.last_line;
	yylloc.first_column = yylloc.last_column;
	for (i = 0; i < yyleng; ++i) {
		if (yytext[i] == '\n') {
			yylloc.last_line++;
			yylloc.last_column = 0;
		} else if (yytext[i] == '\t')
			yylloc.last_column += tabsize -
				(yylloc.last_column % tabsize);
		else
			yylloc.last_column++;
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
	yylloc.first_text = yylloc.last_text = ds;
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

/* Return the original identifier type if available */
static int
lex_input_type(void)
{
	node_t *node;

	if (!lex_input_first)
		return -1;

	list_for_each_entry(node, &curds->node_first, first_list) {
		if (node->type == nt_expr && node->e.op == ID)
			return ID;
		if (node->type == nt_type && node->t.category == type_typedef)
			return TYPEID;
	}

	return -1;
}
