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

static int store_token(int);
static int store_btype(int);

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
			return store_token(CPP_DEFINE);
			}
"if"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return store_token(CPP_IF);
			}
"ifdef"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return store_token(CPP_IFDEF);
			}
"ifndef"		{
			BEGIN(cpp); BEGIN(cpp_condition);
			return store_token(CPP_IFNDEF);
			}
"elif"			{
			BEGIN(cpp); BEGIN(cpp_condition);
			return store_token(CPP_ELIF);
			}
"else"			{
			BEGIN(cpp_ign);
			return store_token(CPP_ELSE);
			}
"endif"			{
			BEGIN(cpp_ign);
			return store_token(CPP_ENDIF);
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
				return store_token(CPP_IDARG);
				}
<cpp_def>{L}({L}|{D})*		{
				BEGIN(cpp);
				return store_token(ID);
				}

  /* CPP directive (during define) */
<cpp>"##"		return store_token(CPP_CONCAT);
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
					int ret = token_at_eof;
					token_at_eof = 0;
					return store_token(ret);
				}
			}

<cpp_condition>"defined"	return store_token(CPP_DEFINED);
<cpp_condition>{L}({L}|{D})*	return store_token(ID);

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

"..."			return store_token(ELLIPSIS);
">>="			return store_token(SHR_ASSIGN);
"<<="			return store_token(SHL_ASSIGN);
"+="			return store_token(ADD_ASSIGN);
"-="			return store_token(SUB_ASSIGN);
"*="			return store_token(MUL_ASSIGN);
"/="			return store_token(DIV_ASSIGN);
"%="			return store_token(MOD_ASSIGN);
"&="			return store_token(AND_ASSIGN);
"^="			return store_token(XOR_ASSIGN);
"|="			return store_token(OR_ASSIGN);
">>"			return store_token(SHR_OP);
"<<"			return store_token(SHL_OP);
"++"			return store_token(INC_OP);
"--"			return store_token(DEC_OP);
"->"			return store_token(PTR_OP);
"&&"			return store_token(AND_OP);
"||"			return store_token(OR_OP);
"<="			return store_token(LE_OP);
">="			return store_token(GE_OP);
"=="			return store_token(EQ_OP);
"!="			return store_token(NE_OP);
";"			return store_token(';');
("{"|"<%")		return store_token('{');
("}"|"%>")		return store_token('}');
","			return store_token(',');
":"			return store_token(':');
"="			return store_token('=');
"("			return store_token('(');
")"			return store_token(')');
("["|"<:")		return store_token('[');
("]"|":>")		return store_token(']');
"."			return store_token('.');
"&"			return store_token('&');
"!"			return store_token('!');
"~"			return store_token('~');
"-"			return store_token('-');
"+"			return store_token('+');
"*"			return store_token('*');
"/"			return store_token('/');
"%"			return store_token('%');
"<"			return store_token('<');
">"			return store_token('>');
"^"			return store_token('^');
"|"			return store_token('|');
"?"			return store_token('?');

"__attribute__"		return store_token(ATTRIBUTE);
"auto"			return store_token(AUTO);
"break"			return store_token(BREAK);
"case"			return store_token(CASE);
"const"			return store_token(CONST);
"continue"		return store_token(CONTINUE);
"default"		return store_token(DEFAULT);
"do"			return store_token(DO);
"else"			return store_token(ELSE);
"enum"			return store_token(ENUM);
"extern"		return store_token(EXTERN);
"for"			return store_token(FOR);
"goto"			return store_token(GOTO);
"if"			return store_token(IF);
"inline"		return store_token(INLINE);
"__inline__"		return store_token(INLINE);
"register"		return store_token(REGISTER);
"return"		return store_token(RETURN);
"sizeof"		return store_token(SIZEOF);
"static"		return store_token(STATIC);
"struct"		return store_token(STRUCT);
"switch"		return store_token(SWITCH);
"typedef"		return store_token(TYPEDEF);
"__typeof__"		return store_token(TYPEOF);
"union"			return store_token(UNION);
"volatile"		return store_token(VOLATILE);
"while"			return store_token(WHILE);

"char"			return store_btype(TYPE_CHAR);
"double"		return store_btype(TYPE_DOUBLE);
"int"			return store_btype(TYPE_INT);
"float"			return store_btype(TYPE_FLOAT);
"long"			return store_btype(TYPE_LONG);
"short"			return store_btype(TYPE_SHORT);
"signed"		return store_btype(TYPE_SIGNED);
"unsigned"		return store_btype(TYPE_UNSIGNED);
"void"			return store_btype(TYPE_VOID);

	/* HACKs */
"FRAME_REG"		return store_token(FRAME_REG);
"for_cpu_indexes"	return store_token(FOR_CPU_INDEXES);
"offsetof"		return store_token(OFFSETOF);
"struct_offset"		return store_token(OFFSETOF);

{L}({L}|{D})*		|
3COM({L}|{D})*		|	/* HACK */
3DFX({L}|{D})*		|	/* HACK */
3DLABS({L}|{D})*	{	/* HACK */
				int oldtype = lex_input_type();
				if(oldtype>= 0)
					return store_token(oldtype);
				else if (istypedef(yytext))
					return store_token(TYPEID);
				return store_token(ID);
			}

0[xX]{H}+{IS}?		|
0{D}+{IS}?		|
{D}+{IS}?		return store_token(INT_CONST);

{D}+{E}{FS}?		|
{D}*"."{D}+({E})?{FS}?	|
{D}+"."{D}*({E})?{FS}?	return store_token(FLOAT_CONST);

L?\'(?s:\\.|[^\\'])*\'	/* (hilight) ' */ return store_token(CHAR_CONST);
				
L?\"(?s:\\.|[^\\"])*\"	/* (hilight) " */ return store_token(STRING_CONST);

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

static int
store_token(int token)
{
	if (token == ID || token == TYPEID || token == CPP_IDARG ||
	    token == INT_CONST || token == FLOAT_CONST ||
	    token == CHAR_CONST || token == STRING_CONST)
		yylval.str = yylloc.first_text;
	else if (token != BASIC_TYPE)
		yylval.token = token;
	yylloc.last_text->token = token;
	return token;
}

static int
store_btype(int btype)
{
	yylval.btype = btype;
	return store_token(BASIC_TYPE);
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
	if (lex_input_first) {
		if (curds->token == ID || curds->token == TYPEID)
			return curds->token;
		else if (curds->token == BASIC_TYPE)
			return TYPEID;
	}

	return -1;
}
