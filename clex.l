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

int filenum;

int start_symbol;

struct list_head raw_contents;
struct dynstr *lex_input_first, *lex_input_last;
struct dynstr *macrods;

dynstr_flags_t lex_dynstr_flags;

static char *lex_input_ptr;

static int input_off;		/* absolute byte offset */
static int output_off;		/* output offset */

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
static int parse_cpp(YYLTYPE *loc);

struct lexer_state {
	struct list_head list;
	YY_BUFFER_STATE buffer;
	struct dynstr *lex_input_first, *lex_input_last;
	struct dynstr *macrods;
	int input_off, output_off;
	struct list_head stored_meta;
};
static LIST_HEAD(state_stack);

struct stored_meta {
	struct list_head list;
	int off;
	dynstr_flags_t flags;
	struct dynstr *ds;
};
static LIST_HEAD(stored_meta);

#define YY_INPUT(buf,result,max_size) \
	if (lex_input_first) { \
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
		input_off += result; \
	}

#define YY_DECL	int yylex(YYSTYPE *val, YYLTYPE *loc)

%}

D			[0-9]
L			[a-zA-Z_]
H			[a-fA-F0-9]
E			[EePp][+-]?{D}+
FS			(f|F|l|L)
IS			(u|U|l|L)*
WS			[ \t\v\f]
CPP			("#"|"%:"){WS}*

%x cpp
%x cpp_def
%x cpp_undef
%x cpp_ign
%x cpp_recover
%s cpp_macro
%s cpp_condition
%s cpp_arg

%%
%{
	if (start_symbol) {
		int ret = start_symbol;
		start_symbol = 0;
		return ret;
	}

	while (macrods) {
		if (!macrods->flags.fake)
			update_pos(&loc->last, macrods->text, macrods->len);
		if (&macrods->list == &raw_contents) {
			macrods = NULL;
		} else if (!macrods->token)
			macrods = next_dynstr(macrods);
		else
			break;
	}

	if (macrods) {
		int ret = macrods->token;
		loc->first.text = loc->last.text = macrods;
		macrods = next_dynstr(macrods);
		return STORE_TOKEN(ret);
	}
%}

<*>"/*"([^*]+|\*+[^*/])*\*+\/	/* ignore comments */
<*>"//".*		/* ignore line comments */

  /* CPP directive */
^{WS}*{CPP}	{
			BEGIN(cpp);
			if (parse_cpp(loc) && YY_START != INITIAL) {
				loc->last.text->flags.reuse = 1;
				BEGIN(cpp_recover);
			}
		}
<cpp_recover>\n	{
			loc->last.text->flags.reuse = 0;
			BEGIN(INITIAL);
		}
  /* skip whitespace except newline */
<cpp_macro,cpp_condition,cpp,cpp_def,cpp_undef>{WS}+
  /* A newline terminates the directive */
<cpp_macro,cpp_condition,cpp,cpp_def,cpp_undef,cpp_ign>\n	{
			loc->last.text->flags.reuse = 0;
			BEGIN(INITIAL);
			yyterminate();
		}

  /* CPP directive (start) */
<cpp>{
"define"		{
			BEGIN(cpp_def);
			}
"undef"			{
			BEGIN(cpp_undef);
			}
"if"			{
			BEGIN(cpp_condition);
			return STORE_TOKEN(CPP_IF);
			}
"ifdef"			{
			BEGIN(cpp_condition);
			return STORE_TOKEN(CPP_IFDEF);
			}
"ifndef"		{
			BEGIN(cpp_condition);
			return STORE_TOKEN(CPP_IFNDEF);
			}
"elif"			{
			BEGIN(cpp_condition);
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
			loc->last.text->flags.reuse = 1;
			BEGIN(cpp_ign);
			}
}

  /* CPP define name */
<cpp_def>{L}({L}|{D})*/\(	{
		BEGIN(cpp_macro);
		if (yyparse_macro(loc, yytext, 1, cpp_cond))
			YY_FATAL_ERROR("yyparse_macro failed");
		yyterminate();
	}
<cpp_def>{L}({L}|{D})*		{
		BEGIN(cpp_macro);
		if (yyparse_macro(loc, yytext, 0, cpp_cond))
			YY_FATAL_ERROR("yyparse_macro failed");
		yyterminate();
	}

<cpp_undef>{L}({L}|{D})*	{
		undefmacro(yytext);
		BEGIN(cpp_ign);
	}

  /* CPP directive (during define) */
<cpp_macro>"#"		return STORE_TOKEN('#');
<cpp_macro>"##"		return STORE_TOKEN(CPP_CONCAT);

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
"typeof"|"__typeof__"	return STORE_TOKEN(TYPEOF);
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

{L}({L}|{D})*		{
		if (YY_START == INITIAL) {
			struct hashed_macro *hm = findmacro(loc->last.text);
			if (hm) {
				macrods = expand_macro(loc, hm);
				if (macrods)
					return yylex(val, loc);
			}
		}
		return STORE_TOKEN(ID);
	}

<cpp_macro,cpp_arg>\.?{D}({L}|{D}|[_.]|{E})*	{
		return STORE_TOKEN(FLOAT_CONST);
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
<INITIAL,cpp_arg>[ \t\v\n\f]+	/* skip whitespace */
.			/* ignore bad characters */

%%

int yywrap(void)
{
	return 1;
}

int
yylex_cpp_arg(union YYSTYPE *val, YYLTYPE *loc)
{
	int saved = YY_START;
	int res;

	BEGIN(cpp_arg);
	res = yylex(val, loc);
	BEGIN(saved);
	return res;
}

void
lex_push_state(void)
{
	struct lexer_state *state = malloc(sizeof(struct lexer_state));
	state->buffer = YY_CURRENT_BUFFER;
	state->lex_input_first = lex_input_first;
	state->lex_input_last = lex_input_last;
	state->macrods = macrods;
	state->input_off = input_off;
	state->output_off = output_off;
	list_add(&state->stored_meta, &stored_meta);
	list_del_init(&stored_meta);
	list_add(&state->list, &state_stack);

	yy_switch_to_buffer(yy_create_buffer(NULL, YY_BUF_SIZE));
	lex_input_first = lex_input_last = NULL;
	macrods = NULL;
	input_off = output_off = 0;
}

void
lex_pop_state(void)
{
        if (!YY_CURRENT_BUFFER)
                return;
	yy_delete_buffer(YY_CURRENT_BUFFER);

	if (!list_empty(&state_stack)) {
		struct lexer_state *state =
			list_entry(state_stack.next, struct lexer_state, list);
		yy_switch_to_buffer(state->buffer);
		lex_input_first = state->lex_input_first;
		lex_input_last = state->lex_input_last;
		macrods = state->macrods;
		input_off = state->input_off;
		output_off = state->output_off;
		list_add(&stored_meta, &state->stored_meta);
		list_del_init(&state->stored_meta);

		list_del(&state->list);
		free(state);
	}
}

static struct stored_meta *
add_stored_meta(int off, dynstr_flags_t flags, struct dynstr *ds)
{
	struct stored_meta *ret = malloc(sizeof(struct stored_meta));
	ret->off = off;
	ret->flags = flags;
	ret->ds = ds;
	list_add_tail(&ret->list, &stored_meta);
	return ret;
}

static struct stored_meta *
next_stored_meta(void)
{
	return list_empty(&stored_meta)
		? NULL
		: list_entry(stored_meta.next, struct stored_meta, list);
}

static struct stored_meta *
last_stored_meta(void)
{
	return list_empty(&stored_meta)
		? NULL
		: list_entry(stored_meta.prev, struct stored_meta, list);
}

static void init_vars(void)
{
	struct stored_meta *sm;

	lex_input_ptr = NULL;
	macrods = NULL;

	memset(&lex_dynstr_flags, 0, sizeof lex_dynstr_flags);
	memset(&cond_state, 0, sizeof cond_state);
	cpp_cond = NULL;

	input_off = output_off = 0;

	while ( (sm = next_stored_meta()) ) {
		list_del(&sm->list);
		free(sm);
	}	
}

struct dynstr *newdynstr(const char *s, size_t len)
{
	struct dynstr *ds = malloc(sizeof(struct dynstr) + len + 1);
	INIT_LIST_HEAD(&ds->list);
	INIT_LIST_HEAD(&ds->node_first);
	INIT_LIST_HEAD(&ds->node_last);
	ds->cpp_cond = NULL;
	ds->exp = NULL;
	ds->token = 0;
	ds->refcount = 0;
	ds->flags = lex_dynstr_flags;
	ds->len = len;
	ds->alloc = len + 1;
	if (s)
		memcpy(ds->text, s, len);
	ds->text[ds->len] = '\0';
	return ds;
}

struct dynstr *
dupdynstr(struct dynstr *ds)
{
	struct dynstr *ret = malloc(sizeof(struct dynstr) + ds->len + 1);
	INIT_LIST_HEAD(&ret->list);
	INIT_LIST_HEAD(&ret->node_first);
	INIT_LIST_HEAD(&ret->node_last);
	ret->cpp_cond = cpp_cond;
	ret->exp = NULL;
	ret->token = ds->token;
	ret->refcount = 0;
	ret->flags = lex_dynstr_flags;
	ret->len = ds->len;
	ret->alloc = ds->len + 1;
	memcpy(ret->text, ds->text, ds->len + 1);
	return ret;
}

void freedynstr(struct dynstr *ds)
{
	if (ds->refcount) {
		fputs("Attempt to free an in-use dynstr!\n", stderr);
		abort();
	}
	put_macro_exp(ds->exp);
	free(ds);
}

static struct dynstr*
realloc_dynstr(struct dynstr *ds, size_t newalloc)
{
	struct dynstr tmp, *newds;

	list_add_tail(&tmp.list, &ds->list);
	list_del(&ds->list);
	list_add_tail(&tmp.node_first, &ds->node_first);
	list_del(&ds->node_first);
	list_add_tail(&tmp.node_last, &ds->node_last);
	list_del(&ds->node_last);

	newds = realloc(ds, sizeof(struct dynstr) + newalloc);
	if (newds) {
		ds = newds;
		ds->alloc = newalloc;
	}

	/* Fix up list pointers */
	list_add(&ds->list, &tmp.list);
	list_del(&tmp.list);
	list_add(&ds->node_first, &tmp.node_first);
	list_del(&tmp.node_first);
	list_add(&ds->node_last, &tmp.node_last);
	list_del(&tmp.node_last);

	return newds;
}

#define ALLOC_INC	256

/* Try to reuse existing dynstr */
static struct dynstr *
reuse_last_content(const char *s, size_t len)
{
	struct dynstr *ds;

	if (list_empty(&raw_contents))
		return NULL;

	ds = last_dynstr(&raw_contents);
	if (!ds->flags.reuse)
		return NULL;

	if (!dynstr_flags_equal(ds->flags, lex_dynstr_flags))
		return NULL;

	if (ds->len + len + 1 > ds->alloc) {
		size_t newalloc = ds->len + len + 1 + ALLOC_INC;
		newalloc -= newalloc % ALLOC_INC;
		ds = realloc_dynstr(ds, newalloc);
	}
	memcpy(ds->text + ds->len, s, len);
	ds->len += len;
	ds->text[ds->len] = '\0';
	return ds;
}

int tabsize = 8;

void
init_loc(YYLTYPE *loc, YYLTYPE *parent)
{
	static const pos_t begpos = {
		.line = 1, .column = 0, .vcolumn = 0
	};

	loc->first = loc->last = begpos;
	loc->parent = parent;
}

void
update_pos(pos_t *pos, const char *p, size_t len)
{
	while (len) {
		if (*p == '\n') {
			pos->line++;
			pos->column = pos->vcolumn = 0;
		} else {
			pos->column++;
			if (*p == '\t')
				pos->vcolumn += tabsize -
					(pos->vcolumn % tabsize);
			else
				pos->vcolumn++;
		}
		++p;
		--len;
	}
}

static int
store_segment(YYLTYPE *loc, const char *s, int len)
{
	struct stored_meta *sm;
	struct dynstr *ds;

	sm = next_stored_meta();
	if (sm && len > sm->off - output_off)
		len = sm->off - output_off;

	if (len) {
		if ( ! (ds = reuse_last_content(s, len)) ) {
			ds = newdynstr(s, len);
			list_add_tail(&ds->list, &raw_contents);
		}

		loc->last.text = ds;
		if (!loc->first.text)
			loc->first.text = ds;
		ds->cpp_cond = cpp_cond;
	}

	if (sm) {
		lex_dynstr_flags = sm->flags;
		if (sm->ds)
			list_add_tail(&sm->ds->list, &raw_contents);

		list_del(&sm->list);
		free(sm);
	}

	return len;
}

static void
store_yytext(YYLTYPE *loc, const char *s)
{
	int len;

	loc->first = loc->last;
	update_pos(&loc->last, yytext, yyleng);

	loc->first.text = NULL;
	len = yyleng;
	while (len) {
		int seglen = store_segment(loc, s, len);
		s += seglen;
		len -= seglen;
		output_off += seglen;
	}
}

static int
store_token(YYSTYPE *val, YYLTYPE *loc, int token)
{
	if (token == ID ||
	    token == INT_CONST || token == FLOAT_CONST ||
	    token == CHAR_CONST || token == STRING_CONST)
		val->str = loc->first.text;
	else if (token != BASIC_TYPE)
		val->token = token;
	loc->last.text->token = token;

	if (token == ID && istypedef(val->str->text))
		return TYPEID;
	return token;
}

static int
store_btype(YYSTYPE *val, YYLTYPE *loc, int btype)
{
	val->btype = btype;
	return store_token(val, loc, BASIC_TYPE);
}

static inline size_t
dynstr_remain(struct dynstr *ds, char *ptr)
{
	return ds->len - (ptr - ds->text);
}

static size_t
dynstr_read(void *buf, size_t max_size)
{
	size_t n;
	struct stored_meta *sm;

	if (!lex_input_ptr)
		lex_input_ptr = lex_input_first->text;

	while ((lex_input_first->flags.macro ||
		! (n = dynstr_remain(lex_input_first, lex_input_ptr))) &&
	       lex_input_first != lex_input_last) {
		if (lex_input_first->flags.macro) {
			struct dynstr *ds = dupdynstr(lex_input_first);
			ds->flags = lex_input_first->flags;
			add_stored_meta(input_off, ds->flags, ds);
		}
		lex_input_first = next_dynstr(lex_input_first);
		lex_input_ptr = lex_input_first->text;
	}

	sm = last_stored_meta();
	if (!sm || !dynstr_flags_equal(sm->flags, lex_input_first->flags))
		add_stored_meta(input_off, lex_input_first->flags, NULL);

	if (n > max_size)
		n = max_size;
	memcpy(buf, lex_input_ptr, n);
	lex_input_ptr += n;
	input_off += n;
	if (!n) {
		lex_input_first = lex_input_last = NULL;
		lex_input_ptr = NULL;
	}
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

int
parse_cpp(YYLTYPE *loc)
{
	struct list_head savedparsed;
	int ret;

	/* Save the original parsed_tree and start a new one */
	list_add(&savedparsed, &parsed_tree);
	list_del_init(&parsed_tree);

	start_symbol = START_DIRECTIVE;
	ret = yyparse(loc);

	if (!ret && is_cpp_cond(&parsed_tree))
		cpp_cond = get_cpp_cond(&cond_state, &parsed_tree);
	if (ret || !is_define(&parsed_tree))
		discard_parsing();

	list_splice(&savedparsed, &parsed_tree);

	return ret;
}
