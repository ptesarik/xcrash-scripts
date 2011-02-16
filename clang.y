/* ANSI C Yacc grammar
 *
 * In 1985, Jeff Lee published his Yacc grammar (which is accompanied
 * by a matching Lex specification) for the April 30, 1985 draft
 * version of the ANSI C standard.  Tom Stockfisch reposted it to
 * net.sources in 1987; that original, as mentioned in the answer to
 * question 17.25 of the comp.lang.c FAQ, can be ftp'ed from
 * ftp.uu.net, file usenet/net.sources/ansi.c.grammar.Z.
 *
 * Jutta Degener, 1995
 */

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "parser.h"

static void addvartypedefs(var_t *);
static void hidetypedef(const char *);
static void unhidetypedefs(void);
static void hidefnparams(declarator_t *);
static void hidedecls(decl_t *);

%}

%union {
	int token;
	unsigned tflags;	/* type flags */
	char *str;
	type_t *type;
	abstract_t abstract;
	declarator_t *declarator;
	expr_t *expr;
	var_t *var;
	decl_t *decl;
}

/* operators */
%token <token> ELLIPSIS		"..."
%token <token> SHR_ASSIGN	">>="
%token <token> SHL_ASSIGN	"<<="
%token <token> ADD_ASSIGN	"+="
%token <token> SUB_ASSIGN	"-="
%token <token> MUL_ASSIGN	"*="
%token <token> DIV_ASSIGN	"/="
%token <token> MOD_ASSIGN	"%="
%token <token> AND_ASSIGN	"&="
%token <token> XOR_ASSIGN	"^="
%token <token> OR_ASSIGN	"|="
%token <token> SHR_OP		">>"
%token <token> SHL_OP		"<<"
%token <token> INC_OP		"++"
%token <token> DEC_OP		"--"
%token <token> PTR_OP		"->"
%token <token> AND_OP		"&&"
%token <token> OR_OP		"||"
%token <token> LE_OP		"<="
%token <token> GE_OP		">="
%token <token> EQ_OP		"=="
%token <token> NE_OP		"!="

/* reserved words */
%token <token> ATTRIBUTE AUTO BREAK CASE CHAR CONST CONTINUE DEFAULT
%token <token> DO DOUBLE ELSE ENUM EXTERN FLOAT FOR GOTO IF INLINE INT
%token <token> LONG REGISTER RETURN SHORT SIGNED SIZEOF STATIC STRUCT
%token <token> SWITCH TYPEDEF TYPEOF UNION UNSIGNED VOID VOLATILE WHILE

/* HACK kludges */
%token <token> OFFSETOF FOR_CPU_INDEXES FRAME_REG

/* constants */
%token <str> INT_CONST FLOAT_CONST CHAR_CONST STRING_CONST

/* identifiers */
%token <str> ID TYPEID

/* pseudo-tokens */
%token ARRAY DECL FUNC LABEL RANGE SIZEOF_TYPE TYPECAST

/* precedence */
%left TYPEID ID
%left IF THEN ELSE

%type <token> assign_op eq_op rel_op shift_op add_op mul_op
%type <token> unary_op unary_lval_op
%type <token> '=' '&' '!' '~' '-' '+' '*' '/' '%' '<' '>' '^' '|' '.'
%type <token> basic_type struct_or_union

%type <tflags> storage_class_spec type_qualifier type_qualifier_list

%type <type> type_decl opt_notype_decl notype_decl
%type <type> type_name _type_name typedef_name
%type <type> type_spec basic_type_list spec_qualifier_list
%type <type> struct_or_union_spec struct_desc enum_spec enum_desc

%type <abstract> pointer array_declarator direct_suffix_declarator
%type <abstract> param_declarator abstract_param_declarator

%type <declarator> declarator declarator_list direct_declarator 
%type <declarator> init_declarator init_declarator_list
%type <declarator> struct_declarator struct_declarator_list
%type <declarator> abstract_declarator direct_abstract_declarator

%type <expr> expr opt_expr assign_expr cond_expr const_expr logical_or_expr
%type <expr> logical_and_expr or_expr xor_expr and_expr eq_expr rel_expr
%type <expr> shift_expr add_expr mul_expr cast_expr unary_expr primary_expr
%type <expr> string_const
%type <expr> initializer initializer_list array_size postfix_expr
%type <expr> compound_stat compound_body stat argument_expr_list
%type <expr> opt_attr attr_spec attr_list attribute attr_param_list

%type <var> enum_body enumerator_list enumerator
%type <var> id_list

%type <decl> translation_unit external_decl func_def decl_list decl
%type <decl> param_decl _param_decl param_list param_type_list
%type <decl> param_type_or_idlist
%type <decl> struct_body struct_decl_list struct_decl

%error-verbose
%debug
%glr-parser
%start translation_unit
%%

translation_unit	: /* empty */
			{ parsed_tree = $$ = NULL; }
			| translation_unit external_decl
			{
				if ($1) {
					list_add_tail(&$2->list, &$1->list);
					parsed_tree = $$ = $1;
				} else
					parsed_tree = $$ = $2;
			}
			;

external_decl		: func_def
			| decl
			;

func_def		: type_decl declarator_list decl_list compound_stat
			{
				unhidetypedefs();
				$$ = newdecl($1, $2);
				$$->decl = $3;
				$$->body = $4;
			}
			| type_decl declarator_list           compound_stat
			{
				unhidetypedefs();
				$$ = newdecl($1, $2);
				$$->body = $3;
			}
			|           declarator_list decl_list compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(newtype_int(), $1);
				$$->decl = $2;
				$$->body = $3;
			}
			|           declarator_list           compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(newtype_int(), $1);
				$$->body = $2;
			}
			;

/* HACK to cope with multiple #ifdef'd functions in unwind.c */
declarator_list		: declarator
			{
				hidefnparams($1);
				$$ = $1;
			}
			| declarator_list declarator
			{
				hidefnparams($2);
				list_add_tail(&$2->list, &$1->list);
				$$ = $1;
			}
			;

decl_list		: decl
			| decl_list decl
			{
				list_add_tail(&$2->list, &$1->list);
				$$ = $1;
			}
			;

decl			: type_decl init_declarator_list ';'
			{
				$$ = newdecl($1, $2);
				if ($1->flags & TF_TYPEDEF)
					addvartypedefs($2->var);
			}
			| type_decl                      ';'
			{ typedef_ign = 0; $$ = newdecl($1, NULL); }
			;

type_decl		: notype_decl
			| opt_notype_decl type_spec opt_notype_decl
			{
				$$ = $2;
				if ($1)
					type_merge($$, $1);
				if ($3)
					type_merge($$, $3);
				if (yychar == TYPEID)
					yychar = ID;
				else if (yychar != ID)
					typedef_ign = 1;
			}
			;

opt_notype_decl		: /* empty */
			{ $$ = NULL; }
			| notype_decl
			;

notype_decl		: attr_spec
			{
				$$ = newtype();
				list_add_tail(&$$->attr, &$1->list);
			}
			| storage_class_spec
			{ $$ = newtype(); $$->flags = $1; }
			| type_qualifier
			{ $$ = newtype(); $$->flags = $1; }
			| notype_decl attr_spec
			{
				struct list_head *last = $2->list.prev;
				$$ = $1;
				list_splice(&$$->attr, last);
				list_add(&$$->attr, last);
			}
			| notype_decl storage_class_spec
			{ $$ = $1; $$->flags |= $2; }
			| notype_decl type_qualifier
			{ $$ = $1; $$->flags |= $2; }
			;

opt_attr		: /* empty */
			{ $$ = NULL; }
			| attr_spec
			;

attr_spec		: ATTRIBUTE '(' '(' attr_list ')' ')'
			{ $$ = $4; }
			;

attr_list		: attribute
			| attr_list ',' attribute
			{
				list_add_tail(&$3->list, &$1->list);
				$$ = $1;
			}
			;

attribute		: /* empty */
			{ $$ = newexpr(ATTRIBUTE); }
			| ID '(' attr_param_list ')'
			{ $$ = newexpr2(FUNC, newexprid($1), $3); }
			| ID
			{ $$ = newexprid($1); }
			/* HACK: should change context in lexer... */
			| CONST
			{ $$ = newexprid(strdup("const")); }
			;

attr_param_list		: /* empty */
			{ $$ = NULL; }
			| const_expr
			| attr_param_list ',' const_expr
			{
				list_add_tail(&$3->list, &$1->list);
				$$ = $1;
			}
			;

storage_class_spec	: AUTO		{ $$ = TF_AUTO; }
			| REGISTER	{ $$ = TF_REGISTER; }
			| STATIC	{ $$ = TF_STATIC; }
			| EXTERN	{ $$ = TF_EXTERN; }
			| INLINE	{ $$ = TF_INLINE; }
			| TYPEDEF	{ $$ = TF_TYPEDEF; }
			;

type_spec		: basic_type_list
			| struct_or_union_spec
			| enum_spec
			| typedef_name
			;

basic_type_list		: basic_type
			{
				$$ = newtype();
				$$->category = type_basic;
				$$->b.list[$$->b.count++] = $1;
			}
			| basic_type_list basic_type
			{ $$->b.list[$$->b.count++] = $2; }
			;

basic_type		: VOID
			| CHAR
			| SHORT
			| INT
			| LONG
			| FLOAT
			| DOUBLE
			| SIGNED
			| UNSIGNED
			;

struct_or_union_spec	: struct_or_union { typedef_ign = 1; }
				opt_attr struct_desc
			{
				$$ = $4;
				$$->category = $1;
				if ($3)
					list_add_tail(&$3->list, &$$->attr);
			}
			;

struct_or_union		: STRUCT	{ $$ = type_struct; }
			| UNION		{ $$ = type_union; }
			;

struct_desc		: ID     struct_body
			{ $$ = newtype_name($1); $$->s.body = $2; }
			|        struct_body
			{ $$ = newtype(); $$->s.body = $1; }
			| ID
			{ $$ = newtype_name($1); }
			;

struct_body		: '{' { typedef_ign = 0; } struct_decl_list '}'
			{ $$ = $3; }
			;

struct_decl_list	: struct_decl
			| struct_decl_list struct_decl
			{
				list_add_tail(&$2->list, &$1->list);
				$$ = $1;
			}
			;

struct_decl		: spec_qualifier_list struct_declarator_list ';'
			{ $$ = newdecl($1, $2); }
			;


struct_declarator_list	: struct_declarator
			| struct_declarator_list ',' struct_declarator
			{
				list_add_tail(&$3->var->list, &$1->var->list);
				$$ = $1;
			}
			;

struct_declarator	: declarator ':' const_expr
			{
				$$ = $1;
				$$->var->bitsize = $3;
			}
			|            ':' const_expr
			{
				$$ = newdeclarator();
				$$->var = newvar(NULL);
				$$->var->bitsize = $2;
			}
			| declarator
			;

enum_spec		: ENUM { typedef_ign = 1; } opt_attr enum_desc
			{
				$$ = $4;
				$$->category = type_enum;
				if ($3)
					list_add_tail(&$3->list, &$$->attr);
			}
			;

enum_desc		: ID enum_body
			{ $$ = newtype_name($1); $$->e.body = $2; }
			|    enum_body
			{ $$ = newtype(); $$->e.body = $1; }
			| ID
			{ $$ = newtype_name($1); }
			;

enum_body		: '{' enumerator_list ',' '}'
			{ $$ = $2; }
			| '{' enumerator_list     '}'
			{ $$ = $2; }
			;

enumerator_list		: enumerator
			| enumerator_list ',' enumerator
			{
				list_add_tail(&$3->list, &$1->list);
				$$ = $1;
			}
			;

enumerator		: ID
			{ $$ = newvar($1); }
			| ID '=' const_expr
			{ $$ = newvar($1); $$->init = $3; }
			;

type_qualifier_list	: type_qualifier
			| type_qualifier_list type_qualifier
			{ $$ = $1 | $2; }
			;

type_qualifier		: CONST		{ $$ = TF_CONST; }
			| VOLATILE	{ $$ = TF_VOLATILE; }
			;

init_declarator_list	: init_declarator
			{
				$$ = $1;
			}
			| init_declarator_list ',' init_declarator
			{
				list_add_tail(&$3->list, &$1->list);
				$$ = $1;
			}
			;

init_declarator		: declarator '=' initializer
			{
				$$ = $1;
				$$->var->init = $3;
			}
			| declarator
			;

/* FIXME: This should be _type_decl sans storage specifiers */
spec_qualifier_list	: type_decl
			;

declarator		: pointer direct_declarator opt_attr
			{
				$$ = $2;
				link_abstract($$, &$1);
				if ($3)
					list_add_tail(&$3->list,
						      &$$->var->attr);
			}
			|         direct_declarator opt_attr
			{
				$$ = $1;
				if ($2)
					list_add_tail(&$2->list,
						      &$$->var->attr);
			}
			;

direct_declarator	: ID
			{
				var_t *var = newvar($1);
				$$ = newdeclarator();
				$$->var = var;
			}
			| '(' declarator ')'
			{ $$ = $2; }
			| direct_declarator direct_suffix_declarator
			{
				$$ = $1;
				link_abstract($$, &$2);
			}
			;

direct_suffix_declarator: array_declarator
			| param_declarator


param_declarator	: '(' param_type_or_idlist ')'
			{
				type_t *type = newtype();
				type->category = type_func;
				type->f.param = $2;
				$$.tree = type;
				$$.stub = &type->f.type;
			}
			;

param_type_or_idlist	: param_type_list
			| id_list
			{
				$$ = newdecl(NULL, NULL);
				$$->var = $1;
			}
			;

pointer			: '*'
			{
				type_t *ptr = newtype();
				ptr->category = type_pointer;
				$$.tree = ptr;
				$$.stub = &ptr->t;
			}
			| pointer '*'
			{
				type_t *ptr = newtype();
				ptr->category = type_pointer;
				ptr->t = $1.tree;
				$$ = $1;
				$$.tree = ptr;
			}
			| pointer type_qualifier_list
			{ $$ = $1; $$.tree->flags |= $2; }
			;

param_type_list		: param_list ',' "..."
			| param_list
			| /* empty */
			{ $$ = NULL; }

param_list		: param_decl
			| param_list ',' param_decl
			{
				list_add_tail(&$3->list, &$1->list);
				$$ = $1;
			}
			;

param_decl		: _param_decl
			{ typedef_ign = 0; $$ = $1; }
			;
_param_decl		: type_decl declarator
			{ $$ = newdecl($1, $2); }
			| type_decl abstract_declarator
			{ $$ = newdecl($1, $2); }
			| type_decl
			{ $$ = newdecl($1, NULL); }
			;

id_list			: ID
			{ $$ = newvar($1); }
			| id_list ',' ID
			{
				var_t *var = newvar($3);
				list_add_tail(&var->list, &$1->list);
				$$ = $1;
			}
			;

initializer		: assign_expr
			| '.' ID '=' assign_expr
			{
				$$ = newexpr2($3,
					      newexpr2($1, NULL,
						       newexprid($2)),
					      $4);
			}
			| '{' initializer_list ',' '}'
			{ $$ = newexpr1(ARRAY, $2); }
			| '{' initializer_list     '}'
			{ $$ = newexpr1(ARRAY, $2); }
			;

initializer_list	: initializer
			| initializer_list ',' initializer
			{
				list_add_tail(&$3->list, &$1->list);
				$$ = $1;
			}
			;

type_name		: { typedef_ign = 0; } _type_name
			{ $$ = $2; typedef_ign = 0; }
			;
_type_name		: spec_qualifier_list
			| spec_qualifier_list abstract_declarator
			| TYPEOF '(' expr ')'
			{
				$$ = newtype();
				$$->category = type_typeof;
				$$->expr = $3;
			}
			;

abstract_declarator	: pointer
			{ $$ = newdeclarator(); $$->abstract = $1; }
			| direct_abstract_declarator
			| pointer direct_abstract_declarator
			{ $$ = $2; link_abstract($$, &$1); }
			;

direct_abstract_declarator
			: '(' abstract_declarator ')'
			{ $$ = $2; }
			|                            array_declarator
			{ $$ = newdeclarator(); $$->abstract = $1; }
			| direct_abstract_declarator array_declarator
			{ $$ = $1; link_abstract($$, &$2); }
			|                            abstract_param_declarator
			{ $$ = newdeclarator(); $$->abstract = $1; }
			| direct_abstract_declarator abstract_param_declarator
			{ $$ = $1; link_abstract($$, &$2); }
			;

array_declarator	: '[' array_size ']'
			{
				type_t *type = newtype();
				type->category = type_array;
				type->a.size = $2;
				$$.tree = type;
				$$.stub = &type->a.type;
			}
			;

array_size		: /* empty */
			{ $$ = NULL; }
			| const_expr
			;

abstract_param_declarator:
			'(' param_type_list ')'
			{
				type_t *type = newtype();
				type->category = type_func;
				type->f.param = $2;
				$$.tree = type;
				$$.stub = &type->f.type;
			}
			;

typedef_name		: TYPEID
			{
				$$ = newtype_name($1);
				$$->category = type_typedef;
			}

stat			: ID ':' stat
			{ $$ = newexpr1(LABEL, $3); }
			| CASE const_expr ':' stat
			{ $$ = newexpr2($1, $2, $4); }
			| CASE const_expr "..." const_expr ':' stat
			{ $$ = newexpr2($1, newexpr2(RANGE, $2, $4), $6); }
			| DEFAULT ':' stat
			{ $$ = newexpr1($1, $3); }
			| IF '(' expr ')' stat ELSE stat
			{ $$ = newexpr3($1, $3, $5, $7); }
			| IF '(' expr ')' stat ELSE stat ELSE stat
			{ /* HACK to cope with #ifdef'd else clauses */
				list_add_tail(&$9->list, &$7->list);
				$$ = newexpr3($1, $3, $5, $7);
			}
			| IF '(' expr ')' stat
			{ $$ = newexpr2($1, $3, $5); }
			| SWITCH '(' expr ')' stat
			{ $$ = newexpr2($1, $3, $5); }
			| WHILE '(' expr ')' stat
			{ $$ = newexpr2($1, $3, $5); }
			| DO stat WHILE '(' expr ')' ';'
			{ $$ = newexpr2($1, $2, $5); }
			| FOR '(' opt_expr ';' opt_expr ';' opt_expr ')' stat
			{ $$ = newexpr4($1, $3, $5, $7, $9); }
			| GOTO ID ';'
			{ $$ = newexpr1($1, newexprid($2)); }
			| CONTINUE ';'
			{ $$ = newexpr($1); }
			| BREAK ';'
			{ $$ = newexpr($1); }
			| RETURN opt_expr ';'
			{ $$ = newexpr1($1, $2); }
			/* HACK */
			| FOR_CPU_INDEXES '(' opt_expr ',' opt_expr ')' stat
			{ $$ = newexpr3($1, $3, $5, $7); }
			| compound_stat
			| opt_expr ';'
			;

opt_expr		: expr
			| /* empty */
			{ $$ = NULL; }
			;

compound_stat		: '{' compound_body '}'
			{ $$ = $2; }
			;

compound_body		: /* empty */
			{ $$ = NULL; }
			| compound_body decl
			{
				expr_t *expr = newexprdecl($2);
				if ($1) {
					list_add_tail(&expr->list, &$1->list);
					$$ = $1;
				} else
					$$ = expr;
				hidedecls($2);
			}
			| compound_body stat
			{
				if (!$2) {
					$$ = $1;
				} else if ($1) {
					list_add_tail(&$2->list, &$1->list);
					$$ = $1;
				} else
					$$ = $2;
			}
			;

expr			: assign_expr
			| expr ',' assign_expr
			{ $$ = newexpr2(',', $1, $3); }
			;

argument_expr_list	: assign_expr
			| argument_expr_list ',' assign_expr
			{ $$ = newexpr2(',', $1, $3); }
			;

assign_expr		: cond_expr
			| unary_expr assign_op assign_expr
			{ $$ = newexpr2($2, $1, $3); }
			;
assign_op		: '=' | ">>=" | "<<=" | "+=" | "-="
			| "*=" | "/=" | "%=" | "&=" | "^=" | "|="
			;

cond_expr		: logical_or_expr
			| logical_or_expr '?' expr ':' cond_expr
			{ $$ = newexpr3('?', $1, $3, $5); }
			;

const_expr		: cond_expr
			;

logical_or_expr		: logical_and_expr
			| logical_or_expr "||" logical_and_expr
			{ $$ = newexpr2(OR_OP, $1, $3); }
			;

logical_and_expr	: or_expr
			| logical_and_expr "&&" or_expr
			{ $$ = newexpr2(AND_OP, $1, $3); }
			;

or_expr			: xor_expr
			| or_expr '|' xor_expr
			{ $$ = newexpr2('|', $1, $3); }
			;

xor_expr		: and_expr
			| xor_expr '^' and_expr
			{ $$ = newexpr2('^', $1, $3); }
			;

and_expr		: eq_expr
			| and_expr '&' eq_expr
			{ $$ = newexpr2('&', $1, $3); }
			;

eq_expr			: rel_expr
			| eq_expr eq_op rel_expr
			{ $$ = newexpr2($2, $1, $3); }
			;
eq_op			: "==" | "!="
			;

rel_expr		: shift_expr
			| rel_expr rel_op shift_expr
			{ $$ = newexpr2($2, $1, $3); }
			;
rel_op			: "<=" | ">=" | '<' | '>'
			;

shift_expr		: add_expr
			| shift_expr shift_op add_expr
			{ $$ = newexpr2($2, $1, $3); }
			;
shift_op		: "<<" | ">>"
			;

add_expr		: mul_expr
			| add_expr add_op mul_expr
			{ $$ = newexpr2($2, $1, $3); }
			;
add_op			: '+' | '-'
			;

mul_expr		: cast_expr
			| mul_expr mul_op cast_expr
			{ $$ = newexpr2($2, $1, $3); }
			;
mul_op			: '*' | '/' | '%'
			;

cast_expr		: unary_expr
			| '(' type_name ')' cast_expr
			{ $$ = newexprtypecast(TYPECAST, $2, $4); }
			;

unary_expr		: postfix_expr
			| unary_lval_op unary_expr
			{ $$ = newexpr1($1, $2); }
			| unary_op cast_expr
			{ $$ = newexpr1($1, $2); }
			| SIZEOF '(' type_name  ')'
			{ $$ = newexprtype(SIZEOF_TYPE, $3); }
			/* HACK: defined as a preprocessor macro */
			| OFFSETOF '(' type_name ',' unary_expr ')'
			{ $$ = newexprtypecast($1, $3, $5); }
			/* HACK */
			| FRAME_REG '(' opt_expr ',' type_name ')'
			{ $$ = newexprtypecast($1, $5, $3); }
			;
unary_op		: '&' | '*' | '+' | '-' | '~' | '!'
			;
unary_lval_op		: "++" | "--" | SIZEOF
			;

postfix_expr		: primary_expr
			| postfix_expr '[' expr ']'
			{ $$ = newexpr2(ARRAY, $1, $3); }
			| postfix_expr '(' argument_expr_list ')'
			{ $$ = newexpr2(FUNC, $1, $3); }
			| postfix_expr '('                    ')'
			{ $$ = newexpr2(FUNC, $1, NULL); }
			| postfix_expr '.' { typedef_ign = 1; } ID
			{
				typedef_ign = 0;
				$$ = newexpr2($2, $1, newexprid($4));
			}
			| postfix_expr "->" { typedef_ign = 1; } ID
			{
				typedef_ign = 0;
				$$ = newexpr2($2, $1, newexprid($4));
			}
			| postfix_expr "++"
			{ $$ = newexpr1($2, $1); }
			| postfix_expr "--"
			{ $$ = newexpr1($2, $1); }
			;

primary_expr		: ID
			{ $$ = newexprid($1); }
			| INT_CONST
			{ $$ = newexprnum($1); }
			| CHAR_CONST
			{ $$ = newexprchar($1); }
			| FLOAT_CONST
			{ $$ = newexprfloat($1); }
			| string_const
			| '(' expr ')'
			{ $$ = $2; }
			;

string_const		: STRING_CONST
			{ $$ = newexprstr($1); }
			| string_const STRING_CONST 
			{
				expr_t *expr = newexprstr($2);
				list_add_tail(&expr->list, &$1->list);
				$$ = $1;
			}
			/* HACK for concatenation with macros */
			| string_const ID
			{
				expr_t *expr = newexprid($2);
				list_add_tail(&expr->list, &$1->list);
				$$ = $1;
			}
			;

%%

void
yyerror(const char *s)
{
	fflush(stdout);
	fprintf(stderr, "%s\n%*s\n%*s at line %d\n",
		linestart, colnum, "^", colnum, s, linenum);
}

decl_t *parsed_tree;

type_t *
newtype_name(const char *name)
{
	int len = name ? strlen(name) + 1 : 0;
	type_t *ret = calloc(sizeof(type_t) + len, 1);

	INIT_LIST_HEAD(&ret->attr);
	if (name) {
		ret->name = (char*)(ret + 1);
		strcpy(ret->name, name);
	}
	return ret;
}

type_t *
newtype(void)
{
	return newtype_name(NULL);
}

type_t *
newtype_int(void)
{
	type_t *ret = newtype();
	ret->category = type_basic;
	ret->b.list[ret->b.count++] = INT;
	return ret;
}

/* Merge the flags and attributes from @other into @merger */
void
type_merge(type_t *merger, type_t *other)
{
	merger->flags |= other->flags;
	list_splice(&other->attr, &merger->attr);
	free(other);
}

var_t *
newvar(const char *name)
{
	size_t extra = name ? strlen(name) + 1 : 0;
	var_t *ret = calloc(sizeof(var_t) + extra, 1);

	INIT_LIST_HEAD(&ret->list);
	INIT_LIST_HEAD(&ret->attr);
	if (name) {
		ret->name = (char*)(ret + 1);
		strcpy(ret->name, name);
	}
	return ret;
}

declarator_t *
newdeclarator(void)
{
	declarator_t *ret = calloc(sizeof(declarator_t), 1);
	INIT_LIST_HEAD(&ret->list);
	return ret;
}

void
link_abstract(declarator_t *declarator, const abstract_t *abstract)
{
	if (declarator->abstract.stub) {
		*declarator->abstract.stub = abstract->tree;
		declarator->abstract.stub = abstract->stub;
	} else
		declarator->abstract = *abstract;
}

decl_t *
newdecl(type_t *type, declarator_t *declarator)
{
	decl_t *ret = calloc(sizeof(decl_t), 1);
	var_t *var, *lastvar;
	declarator_t *d, *nextd;

	INIT_LIST_HEAD(&ret->list);

	ret->type = type;
	if (!declarator)
		return ret;

	ret->var = declarator->var;

	lastvar = list_entry(declarator->list.prev, declarator_t, list)->var;
	nextd = declarator;
	do {
		d = nextd;
		var = d->var;
		if (var) {
			lastvar->list.next = &var->list;
			var->list.prev = &lastvar->list;

			if (d->abstract.stub) {
				*d->abstract.stub = type;
				var->type = d->abstract.tree;
				var->type->flags = type->flags;
			} else
				var->type = type;
		} else if (d->abstract.stub) {
			*d->abstract.stub = type;
			ret->type = d->abstract.tree;
			ret->type->flags = type->flags;
		}

		lastvar = var;
		nextd = list_entry(d->list.next, declarator_t, list);
		free(d);
	} while(nextd != declarator);

	return ret;
}

expr_t *
newexpr(int op)
{
	expr_t *ret = calloc(sizeof(expr_t), 1);
	ret->op = op;
	INIT_LIST_HEAD(&ret->list);
	return ret;
}

expr_t *
newexprnum(char *str)
{
	expr_t *ret = newexpr(INT_CONST);
	ret->num = strtol(str, (char**)NULL, 0);
	free(str);
	return ret;
}

expr_t *
newexprfloat(char *str)
{
	expr_t *ret = newexpr(FLOAT_CONST);
	ret->f = strtod(str, (char**)NULL);
	free(str);
	return ret;
}

expr_t *
newexprstr(char *str)
{
	size_t len = strlen(str) + 1;
	expr_t *ret = malloc(sizeof(expr_t) + len);

	ret->op = STRING_CONST;
	INIT_LIST_HEAD(&ret->list);
	ret->str = (char*)(ret + 1);
	strcpy(ret->str, str);
	free(str);
	return ret;
}

expr_t *
newexprchar(char *str)
{
	expr_t *ret = newexprstr(str);
	ret->op = CHAR_CONST;
	return ret;
}

expr_t *
newexprid(char *id)
{
	expr_t *ret = newexprstr(id);
	ret->op = ID;
	return ret;
}

expr_t *
newexprtype(int op, type_t *type)
{
	expr_t *ret = newexpr(op);
	ret->type = type;
	return ret;
}

expr_t *
newexprtypecast(int op, type_t *type, expr_t *expr)
{
	expr_t *ret = newexpr(op);
	ret->typecast.type = type;
	ret->typecast.expr = expr;
	return ret;
}

expr_t *
newexprdecl(decl_t *decl)
{
	expr_t *ret = newexpr(DECL);
	ret->decl = decl;
	return ret;
}

expr_t *
newexpr1(int op, expr_t *expr)
{
	expr_t *ret = newexpr(op);
	ret->expr = expr;
	return ret;
}

expr_t *
newexpr2(int op, expr_t *left, expr_t *right)
{
	expr_t *ret = newexpr(op);
	ret->binary.left = left;
	ret->binary.right = right;
	return ret;
}

expr_t *
newexpr3(int op, expr_t *cond, expr_t *ontrue, expr_t *onfalse)
{
	expr_t * ret = newexpr(op);
	ret->ternary.cond = cond;
	ret->ternary.ontrue = ontrue;
	ret->ternary.onfalse = onfalse;
	return ret;
}

expr_t *
newexpr4(int op, expr_t *init, expr_t *cond, expr_t *iter, expr_t *body)
{
	expr_t * ret = newexpr(op);
	ret->forloop.init = init;
	ret->forloop.cond = cond;
	ret->forloop.iter = iter;
	ret->forloop.body = body;
	return ret;
}

#define HASH_SIZE	512

struct hashed_type {
	char *name;
	int hidden;
	struct hashed_type *next;
};

static struct hashed_type *typedefs[HASH_SIZE];

static unsigned
mkhash(const char *s)
{
	unsigned ret = 0;
	while (*s) {
		ret *= 13;
		ret += *s++;
	}
	return ret % HASH_SIZE;
}

int
istypedef(const char *name)
{
	unsigned hash = mkhash(name);
	struct hashed_type *ht;
	for (ht = typedefs[hash]; ht; ht = ht->next) {
		if (!strcmp(name, ht->name))
			return !ht->hidden;
	}
	return 0;
}

void
addtypedef(const char *name)
{
	unsigned hash = mkhash(name);
	struct hashed_type *ht;
	for (ht = typedefs[hash]; ht; ht = ht->next) {
		if (!strcmp(name, ht->name))
			return;
	}

	ht = malloc(sizeof(struct hashed_type) + strlen(name) + 1);
	ht->name = (char*)(ht + 1);
	ht->hidden = 0;
	ht->next = typedefs[hash];
	strcpy(ht->name, name);
	typedefs[hash] = ht;
}

static void
addvartypedefs(var_t *varlist)
{
	var_t *var = varlist;
	do {
		addtypedef(var->name);
		var = list_entry(var->list.next, var_t, list);
	} while (var != varlist);
}

static void
hidetypedef(const char *name)
{
	unsigned hash = mkhash(name);
	struct hashed_type *ht;
	for (ht = typedefs[hash]; ht; ht = ht->next) {
		if (!strcmp(name, ht->name)) {
			++ht->hidden;
			break;
		}
	}
}

static void
unhidetypedefs(void)
{
	unsigned hash;
	struct hashed_type *ht;
	for (hash = 0; hash < HASH_SIZE; ++hash)
		for (ht = typedefs[hash]; ht; ht = ht->next)
			if (ht->hidden)
				--ht->hidden;
}

static void
hidevars(var_t *first)
{
	var_t *var = first;
	do {
		if (var->name)
			hidetypedef(var->name);
		var = list_entry(var->list.next, var_t, list);
	} while (var != first);
}

static void
hidedecls(decl_t *first)
{
	decl_t *decl = first;
	do {
		if (decl->var)
			hidevars(decl->var);
		decl = list_entry(decl->list.next, decl_t, list);
	} while (decl != first);
}

static void
hidefnparams(declarator_t *first)
{
	declarator_t *decl = first;
	do {
		type_t *type = decl->abstract.tree;
		if (type->category != type_func) {
			fprintf(stderr, "Ouch! %s is not a function!\n",
				decl->var->name);
			continue;
		}
		if (type->f.param)
			hidedecls(type->f.param);

		decl = list_entry(decl->list.next, declarator_t, list);
	} while (decl != first);
}