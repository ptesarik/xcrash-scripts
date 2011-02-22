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

struct list_head raw_contents;
struct list_head raw_cpp;
static void store_yytext(const char *s);
#define YY_USER_ACTION	store_yytext(yytext);

static void init_vars(void);
#define YY_USER_INIT	init_vars()

%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[Ee][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*

%x cpp_ign

%%

<*>"/*"([^*]+|\*+[^*/])*\*+\/	/* ignore comments */
<*>"//".*\n?		/* ignore line comments */

\\\n?			/* continuation lines */

  /* CPP ignored directive */
("#"|"%:")		BEGIN(cpp_ign);
<cpp_ign>(.|\\\n)+	{
	if (!cpp_input)
		list_add_tail(&yylloc.last_text->cpp_list, &raw_cpp);
	/* and eat up the directive */
	}
<cpp_ign>\n		BEGIN(INITIAL);

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
"char"			return yylval.token = CHAR;
"const"			return yylval.token = CONST;
"continue"		return yylval.token = CONTINUE;
"default"		return yylval.token = DEFAULT;
"do"			return yylval.token = DO;
"double"		return yylval.token = DOUBLE;
"else"			return yylval.token = ELSE;
"enum"			return yylval.token = ENUM;
"extern"		return yylval.token = EXTERN;
"float"			return yylval.token = FLOAT;
"for"			return yylval.token = FOR;
"goto"			return yylval.token = GOTO;
"if"			return yylval.token = IF;
"inline"		return yylval.token = INLINE;
"__inline__"		return yylval.token = INLINE;
"int"			return yylval.token = INT;
"long"			return yylval.token = LONG;
"register"		return yylval.token = REGISTER;
"return"		return yylval.token = RETURN;
"short"			return yylval.token = SHORT;
"signed"		return yylval.token = SIGNED;
"sizeof"		return yylval.token = SIZEOF;
"static"		return yylval.token = STATIC;
"struct"		return yylval.token = STRUCT;
"switch"		return yylval.token = SWITCH;
"typedef"		return yylval.token = TYPEDEF;
"__typeof__"		return yylval.token = TYPEOF;
"union"			return yylval.token = UNION;
"unsigned"		return yylval.token = UNSIGNED;
"void"			return yylval.token = VOID;
"volatile"		return yylval.token = VOLATILE;
"while"			return yylval.token = WHILE;

	/* HACKs */
"FRAME_REG"		return yylval.token = FRAME_REG;
"for_cpu_indexes"	return yylval.token = FOR_CPU_INDEXES;
"offsetof"		return yylval.token = OFFSETOF;
"struct_offset"		return yylval.token = OFFSETOF;

{L}({L}|{D})*		|
3COM({L}|{D})*		|	/* HACK */
3DFX({L}|{D})*		|	/* HACK */
3DLABS({L}|{D})*	{	/* HACK */
				yylval.str = strdup(yytext);
				if (typedef_ign)
					--typedef_ign;
				else if (istypedef(yytext))
					return TYPEID;
				return ID;
			}

0[xX]{H}+{IS}?		yylval.str = strdup(yytext); return INT_CONST;
0{D}+{IS}?		yylval.str = strdup(yytext); return INT_CONST;
{D}+{IS}?		yylval.str = strdup(yytext); return INT_CONST;

{D}+{E}{FS}?		yylval.str = strdup(yytext); return FLOAT_CONST;
{D}*"."{D}+({E})?{FS}?	yylval.str = strdup(yytext); return FLOAT_CONST;
{D}+"."{D}*({E})?{FS}?	yylval.str = strdup(yytext); return FLOAT_CONST;

L?\'(?s:\\.|[^\\'])*\'	{ /* fix highlight: ' */
			yylval.str = strdup(yytext); return CHAR_CONST;
			}
L?\"(?s:\\.|[^\\"])*\"	{ /* fix highlight: " */
			yylval.str = strdup(yytext); return STRING_CONST;
			}
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
}

struct dynstr *newdynstr(const char *s, size_t len)
{
	struct dynstr *ds = malloc(sizeof(struct dynstr) + len);
	ds->len = ds->alloc = len;
	memcpy(ds->text, s, len);
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

	ds = newdynstr(s, yyleng);
	list_add_tail(&ds->list, &raw_contents);
	yylloc.first_text = yylloc.last_text = ds;
}
