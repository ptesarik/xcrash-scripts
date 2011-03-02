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
#include "parser.h"
#include "clang.tab.h"

int start_symbol;

struct list_head raw_contents;
struct list_head raw_cpp;
struct dynstr *cpp_input;

static int eof_seen;
static char *cpp_ptr;

static void store_yytext(const char *s);
#define YY_USER_ACTION	store_yytext(yytext);

static void init_vars(void);
#define YY_USER_INIT	init_vars()

#define YY_INPUT(buf,result,max_size) \
	if (cpp_input) { \
		int n = cpp_input->len - (cpp_ptr - cpp_input->text); \
		if (n > max_size) \
			n = max_size; \
		memcpy(buf, cpp_ptr, n); \
		cpp_ptr += n; \
		result = n; \
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

%s cpp
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
[ \t\v\f]+		/* skip whitespace except newline */
"define"		{
			BEGIN(cpp_def);
			return yylval.token = CPP_DEFINE;
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
				yylval.str = yylloc.first_text->text;
				return CPP_IDARG;
				}
<cpp_def>{L}({L}|{D})*		{
				BEGIN(cpp);
				yylval.str = yylloc.first_text->text;
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
				if (eof_seen)
					yyterminate();
				else {
					eof_seen = 1;
					return yylval.token = ';';
				}
			}

  /* CPP ignored directive */
("#"|"%:")[ \t\v\f]*	BEGIN(cpp_ign);
<cpp_ign>(.|\\\n)	{
	if (!cpp_input && !yylloc.last_text->reuse)
		list_add_tail(&yylloc.last_text->cpp_list, &raw_cpp);
	yylloc.last_text->reuse = 1;
	/* and eat up the directive */
	}
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
				yylval.str = yylloc.first_text->text;
				if (typedef_ign)
					--typedef_ign;
				else if (istypedef(yytext))
					return TYPEID;
				return ID;
			}

0[xX]{H}+{IS}?		|
0{D}+{IS}?		|
{D}+{IS}?		{
				yylval.str = yylloc.first_text->text;
				return INT_CONST;
			}

{D}+{E}{FS}?		|
{D}*"."{D}+({E})?{FS}?	|
{D}+"."{D}*({E})?{FS}?	{
				yylval.str = yylloc.first_text->text;
				return FLOAT_CONST;
			}

L?\'(?s:\\.|[^\\'])*\'	{ /* fix highlight: ' */
				yylval.str = yylloc.first_text->text;
				return CHAR_CONST;
			}
L?\"(?s:\\.|[^\\"])*\"	{ /* fix highlight: " */
				yylval.str = yylloc.first_text->text;
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

int typedef_ign;

static void init_vars(void)
{
	typedef_ign = 0;
	eof_seen = 0;
	if (cpp_input) {
		cpp_ptr = cpp_input->text;
		BEGIN(cpp_dir);
	}
}

struct dynstr *newdynstr(const char *s, size_t len)
{
	struct dynstr *ds = malloc(sizeof(struct dynstr) + len + 1);
	ds->cpp_list.next = NULL;
	INIT_LIST_HEAD(&ds->node_first);
	INIT_LIST_HEAD(&ds->node_last);
	ds->len = len;
	ds->alloc = len + 1;
	ds->reuse = 0;
	memcpy(ds->text, s, len);
	ds->text[ds->len] = '\0';
	return ds;
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
		size_t newalloc = ds->len + len + 1 + ALLOC_INC;
		newalloc -= newalloc % ALLOC_INC;
		ds = realloc(ds, sizeof(struct dynstr) + newalloc);
		ds->alloc = newalloc;

		/* Fix up list pointers */
		ds->list.prev->next = &ds->list;
		ds->list.next->prev = &ds->list;
		if (ds->cpp_list.next) {
			ds->cpp_list.prev->next = &ds->cpp_list;
			ds->cpp_list.next->prev = &ds->cpp_list;
		}
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

	if (! (ds = reuse_last_content(s, yyleng)) ) {
		ds = newdynstr(s, yyleng);
		list_add_tail(&ds->list, &raw_contents);
	}
	yylloc.first_text = yylloc.last_text = ds;
}
