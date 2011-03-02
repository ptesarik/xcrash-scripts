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

static void yyerror(const char *);

static void type_add_attr(node_t *, node_t *);
static void type_merge(node_t *, node_t *);

static declarator_t *newdeclarator(void);
static void link_abstract(declarator_t *, const abstract_t *);

static void addtypedeflist(node_t *);
static void hidetypedef(const char *);
static void unhidetypedefs(void);
static void hidefnparams(declarator_t *);
static void hidedecls(node_t *);

# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (N)								\
	{								\
	  (Current).first_line	 = YYRHSLOC(Rhs, 1).first_line;		\
	  (Current).first_column = YYRHSLOC(Rhs, 1).first_column;	\
	  (Current).first_text   = YYRHSLOC(Rhs, 1).first_text;		\
	  (Current).last_line	 = YYRHSLOC(Rhs, N).last_line;		\
	  (Current).last_column	 = YYRHSLOC(Rhs, N).last_column;	\
	  (Current).last_text    = YYRHSLOC(Rhs, N).last_text;		\
	}								\
      else								\
	{								\
	  struct dynstr *ds = newdynstr(NULL, 0);			\
	  (Current).first_line	 = (Current).last_line	 =		\
	    YYRHSLOC(Rhs, 0).last_line;					\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC(Rhs, 0).last_column;				\
	  (Current).first_text   = (Current).last_text   = ds;		\
	  list_add_tail(&ds->list,					\
			(yychar != YYEMPTY && yychar != YYEOF)		\
			? &yylloc.first_text->list			\
			: &raw_contents);				\
	}								\
    while (0)

%}

%union {
	int token;
	unsigned tflags;	/* type flags */
	unsigned long btype;
	char *str;
	type_t *type;
	abstract_t abstract;
	declarator_t *declarator;
	expr_t *expr;
	var_t *var;
	node_t *node;
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
%token <token> ATTRIBUTE AUTO BREAK CASE CONST CONTINUE DEFAULT
%token <token> DO ELSE ENUM EXTERN FOR GOTO IF INLINE
%token <token> REGISTER RETURN SIZEOF STATIC STRUCT
%token <token> SWITCH TYPEDEF TYPEOF UNION VOLATILE WHILE

/* basic types */
%token <btype> BASIC_TYPE

/* HACK kludges */
%token <token> OFFSETOF FOR_CPU_INDEXES FRAME_REG

/* constants */
%token <str> INT_CONST FLOAT_CONST CHAR_CONST STRING_CONST

/* identifiers */
%token <str> ID TYPEID

/* CPP tokens */
%token <token> CPP_DEFINE
%token <token> CPP_CONCAT	"##"
%token <str> CPP_IDARG

/* start symbol pseudo-tokens */
%token START_DIRECTIVE

/* other pseudo-tokens */
%token ARRAY CONCAT FUNC LABEL RANGE SIZEOF_TYPE TYPECAST

/* precedence */
%left TYPEID ID
%left IF ELSE

%type <token> assign_op eq_op rel_op shift_op add_op mul_op
%type <token> unary_op unary_lval_op
%type <token> '=' '&' '!' '~' '-' '+' '*' '/' '%' '<' '>' '^' '|' '.'
%type <token> struct_or_union

%type <tflags> storage_class_spec type_qualifier type_qualifier_list

/* type type */
%type <node> type_decl opt_notype_decl notype_decl
%type <node> type_name _type_name typedef_name
%type <node> type_spec basic_type_list spec_qualifier_list
%type <node> struct_or_union_spec struct_desc enum_spec enum_desc

%type <abstract> pointer array_declarator direct_suffix_declarator
%type <abstract> param_declarator abstract_param_declarator

%type <declarator> declarator declarator_list direct_declarator 
%type <declarator> init_declarator init_declarator_list
%type <declarator> struct_declarator struct_declarator_list
%type <declarator> abstract_declarator direct_abstract_declarator

/* expr type */
%type <node> expr opt_expr assign_expr cond_expr const_expr logical_or_expr
%type <node> logical_and_expr or_expr xor_expr and_expr eq_expr rel_expr
%type <node> shift_expr add_expr mul_expr cast_expr unary_expr primary_expr
%type <node> string_const
%type <node> initializer initializer_list array_size postfix_expr
%type <node> compound_stat compound_body stat argument_expr_list
%type <node> opt_attr attr_spec attr_list attribute attr_param_list

/* var type */
%type <node> enum_body enumerator_list enumerator
%type <node> id_list

/* decl type */
%type <node> external_decl func_def decl_list decl
%type <node> param_decl _param_decl param_list param_type_list
%type <node> param_type_or_idlist
%type <node> struct_body struct_decl_list struct_decl

/* CPP types */
%type <node> directive macro_def macro_declarator macro_param

%type <node> translation_unit

%error-verbose
%locations
%glr-parser
%start translation_unit
%initial-action
{
	struct dynstr *ds = newdynstr(NULL, 0);
	list_add_tail(&ds->list, &raw_contents);
	@$.first_line   = @$.last_line   = 1;
	@$.first_column = @$.last_column = 0;
	@$.first_text   = @$.last_text   = ds;
}
%%

translation_unit	: /* empty */
			{ $$ = parsed_tree; }
			| translation_unit external_decl
			{
				if ( ($$ = parsed_tree) )
					list_add_tail(&$2->list, &$$->list);
				else
					parsed_tree = $$ = $2;
			}
			| START_DIRECTIVE directive
			{
				if ($2) {
					if ( ($$ = parsed_tree) )
						list_add_tail(&$2->list,
							      &$$->list);
					else
						parsed_tree = $$ = $2;
				}
			}
			;

directive		: CPP_DEFINE macro_def
			{ $$ = $2; }
			| /* empty */
			{ $$ = NULL; }
			;

macro_def		: macro_declarator compound_body
			{
				$$ = newdecl(&@$, NULL, NULL);
				$$->child[chd_var] = $1;
				$$->child[chd_body] = $2;
			}
			;

macro_declarator	: CPP_IDARG macro_param
			{
				node_t *type = newtype(&@$);
				type->t.category = type_func;
				type->child[cht_param] = $2;
				$$ = newvar(&@1, $1);
				$$->child[chv_type] = type;
			}
			| ID
			{
				node_t *type = newtype(&@$);
				type->t.category = type_func;
				$$ = newvar(&@1, $1);
				$$->child[chv_type] = type;
			}
			;

macro_param		: '(' id_list ')'
			{
				$$ = newdecl(&@$, NULL, NULL);
				$$->child[chd_var] = $2;
			}
			| '(' ')'
			{ $$ = NULL; }
			;

external_decl		: func_def
			| decl
			;

func_def		: type_decl declarator_list decl_list compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, $1, $2);
				$$->child[chd_decl] = $3;
				$$->child[chd_body] = $4;
			}
			| type_decl declarator_list           compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, $1, $2);
				$$->child[chd_body] = $3;
			}
			|           declarator_list decl_list compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, newtype_int(&@$), $1);
				$$->child[chd_decl] = $2;
				$$->child[chd_body] = $3;
			}
			|           declarator_list           compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, newtype_int(&@$), $1);
				$$->child[chd_body] = $2;
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
				if ($1->t.flags & TF_TYPEDEF)
					addtypedeflist($2->var);
				$$ = newdecl(&@$, $1, $2);
			}
			| type_decl                      ';'
			{ typedef_ign = 0; $$ = newdecl(&@$, $1, NULL); }
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
				$$ = newtype(&@$);
				$$->child[cht_attr] = $1;
			}
			| storage_class_spec
			{ $$ = newtype(&@$); $$->t.flags = $1; }
			| type_qualifier
			{ $$ = newtype(&@$); $$->t.flags = $1; }
			| notype_decl attr_spec
			{ $$ = $1; type_add_attr($$, $2); }
			| notype_decl storage_class_spec
			{ $$ = $1; $$->t.flags |= $2; }
			| notype_decl type_qualifier
			{ $$ = $1; $$->t.flags |= $2; }
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
			{ $$ = newexpr(&@$, ATTRIBUTE); }
			| ID '(' attr_param_list ')'
			{ $$ = newexpr2(&@$, FUNC, newexprid(&@$, $1), $3); }
			| ID
			{ $$ = newexprid(&@$, $1); }
			/* HACK: should change context in lexer... */
			| CONST
			{ $$ = newexprid(&@$, "const"); }
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
			| typedef_name
			| struct_or_union_spec
			| enum_spec
			;

basic_type_list		: BASIC_TYPE
			{
				$$ = newtype(&@$);
				$$->t.category = type_basic;
				$$->t.btype = $1;
			}
			| basic_type_list BASIC_TYPE
			{
				/* "long" can be repeated */
				if ($2 == TYPE_LONG &&
				    $$->t.btype & TYPE_LONG)
					$$->t.btype |= TYPE_LONGLONG;
				$$->t.btype |= $2;
				set_node_last($$, @2.last_text);
			}
			;

typedef_name		: TYPEID
			{
				$$ = newtype_name(&@$, $1);
				$$->t.category = type_typedef;
			}
			;

struct_or_union_spec	: struct_or_union { typedef_ign = 1; }
				opt_attr struct_desc
			{
				$$ = $4;
				$$->t.category = $1;
				type_add_attr($$, $3);
				set_node_first($$, @$.first_text);
			}
			;

struct_or_union		: STRUCT	{ $$ = type_struct; }
			| UNION		{ $$ = type_union; }
			;

struct_desc		: ID     struct_body
			{
				$$ = newtype_name(&@$, $1);
				$$->child[cht_body] = $2;
			}
			|        struct_body
			{
				$$ = newtype(&@$);
				$$->child[cht_body] = $1;
			}
			| ID
			{ $$ = newtype_name(&@$, $1); }
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
			{ $$ = newdecl(&@$, $1, $2); }
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
				$$->var->child[chv_bitsize] = $3;
			}
			|            ':' const_expr
			{
				$$ = newdeclarator();
				$$->var = newvar(&@$, NULL);
				$$->var->child[chv_bitsize] = $2;
			}
			| declarator
			;

enum_spec		: ENUM { typedef_ign = 1; } opt_attr enum_desc
			{
				$$ = $4;
				$$->t.category = type_enum;
				type_add_attr($$, $3);
				set_node_first($$, @$.first_text);
			}
			;

enum_desc		: ID enum_body
			{
				$$ = newtype_name(&@$, $1);
				$$->child[cht_body] = $2; }
			|    enum_body
			{
				$$ = newtype(&@$);
				$$->child[cht_body] = $1;
			}
			| ID
			{ $$ = newtype_name(&@$, $1); }
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
			{ $$ = newvar(&@$, $1); }
			| ID '=' const_expr
			{
				$$ = newvar(&@$, $1);
				$$->child[chv_init] = $3;
			}
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
				$$->var->child[chv_init] = $3;
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
					$$->var->child[chv_attr] = $3;
			}
			|         direct_declarator opt_attr
			{
				$$ = $1;
				if ($2)
					$$->var->child[chv_attr] = $2;
			}
			;

direct_declarator	: ID
			{
				$$ = newdeclarator();
				$$->var = newvar(&@$, $1);
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
				node_t *type = newtype(&@$);
				type->t.category = type_func;
				type->child[cht_param] = $2;
				$$.tree = type;
				$$.stub = &type->child[cht_type];
			}
			;

param_type_or_idlist	: param_type_list
			| id_list
			{
				$$ = newdecl(&@$, NULL, NULL);
				$$->child[chd_var] = $1;
			}
			;

pointer			: '*'
			{
				node_t *ptr = newtype(&@$);
				ptr->t.category = type_pointer;
				$$.tree = ptr;
				$$.stub = &ptr->child[cht_type];
			}
			| pointer '*'
			{
				node_t *ptr = newtype(&@$);
				ptr->t.category = type_pointer;
				ptr->child[cht_type] = $1.tree;
				$$ = $1;
				$$.tree = ptr;
			}
			| pointer type_qualifier_list
			{ $$ = $1; $$.tree->t.flags |= $2; }
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
			{ $$ = newdecl(&@$, $1, $2); }
			| type_decl abstract_declarator
			{ $$ = newdecl(&@$, $1, $2); }
			| type_decl
			{ $$ = newdecl(&@$, $1, NULL); }
			;

id_list			: ID
			{ $$ = newvar(&@$, $1); }
			| id_list ',' ID
			{
				node_t *var = newvar(&@$, $3);
				list_add_tail(&var->list, &$1->list);
				$$ = $1;
			}
			;

initializer		: assign_expr
			| '.' ID '=' assign_expr
			{
				$$ = newexpr2(&@$, $3,
					      newexpr2(&@$, $1, NULL,
						       newexprid(&@$, $2)),
					      $4);
			}
			| '{' initializer_list ',' '}'
			{ $$ = newexpr1(&@$, ARRAY, $2); }
			| '{' initializer_list     '}'
			{ $$ = newexpr1(&@$, ARRAY, $2); }
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
			{
				*$2->abstract.stub = $1;
				$$ = $2->abstract.tree;
				$$->t.flags = $1->t.flags;
				free($2);
				set_node_first($$, @$.first_text);
				set_node_last($$, @$.last_text);
			}
			| TYPEOF '(' expr ')'
			{
				$$ = newtype(&@$);
				$$->t.category = type_typeof;
				$$->child[cht_expr] = $3;
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
				node_t *type = newtype(&@$);
				type->t.category = type_array;
				type->child[cht_size] = $2;
				$$.tree = type;
				$$.stub = &type->child[cht_type];
			}
			;

array_size		: /* empty */
			{ $$ = NULL; }
			| const_expr
			;

abstract_param_declarator:
			'(' param_type_list ')'
			{
				node_t *type = newtype(&@$);
				type->t.category = type_func;
				type->child[cht_param] = $2;
				$$.tree = type;
				$$.stub = &type->child[cht_type];
			}
			;

stat			: ID ':' stat
			{ $$ = newexpr1(&@$, LABEL, $3); }
			| CASE const_expr ':' stat
			{ $$ = newexpr2(&@$, $1, $2, $4); }
			| CASE const_expr "..." const_expr ':' stat
			{
				$$ = newexpr2(&@$, $1,
					      newexpr2(&@$, RANGE, $2, $4),
					      $6);
			}
			| DEFAULT ':' stat
			{ $$ = newexpr1(&@$, $1, $3); }
			| IF '(' expr ')' stat ELSE stat
			{ $$ = newexpr3(&@$, $1, $3, $5, $7); }
			| IF '(' expr ')' stat
			{ $$ = newexpr2(&@$, $1, $3, $5); }
			| SWITCH '(' expr ')' stat
			{ $$ = newexpr2(&@$, $1, $3, $5); }
			| WHILE '(' expr ')' stat
			{ $$ = newexpr2(&@$, $1, $3, $5); }
			| DO stat WHILE '(' expr ')' ';'
			{ $$ = newexpr2(&@$, $1, $2, $5); }
			| FOR '(' opt_expr ';' opt_expr ';' opt_expr ')' stat
			{ $$ = newexpr4(&@$, $1, $3, $5, $7, $9); }
			| GOTO ID ';'
			{ $$ = newexpr1(&@$, $1, newexprid(&@$, $2)); }
			| CONTINUE ';'
			{ $$ = newexpr(&@$, $1); }
			| BREAK ';'
			{ $$ = newexpr(&@$, $1); }
			| RETURN opt_expr ';'
			{ $$ = newexpr1(&@$, $1, $2); }
			/* HACK */
			| FOR_CPU_INDEXES '(' opt_expr ',' opt_expr ')' stat
			{ $$ = newexpr3(&@$, $1, $3, $5, $7); }
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
				if ($1) {
					list_add_tail(&$2->list, &$1->list);
					$$ = $1;
				} else
					$$ = $2;
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
			{ $$ = newexpr2(&@$, ',', $1, $3); }
			;

argument_expr_list	: assign_expr
			| argument_expr_list ',' assign_expr
			{ list_add_tail(&$3->list, &$1->list); }
			;

assign_expr		: cond_expr
			| unary_expr assign_op assign_expr
			{ $$ = newexpr2(&@$, $2, $1, $3); }
			;
assign_op		: '=' | ">>=" | "<<=" | "+=" | "-="
			| "*=" | "/=" | "%=" | "&=" | "^=" | "|="
			;

cond_expr		: logical_or_expr
			| logical_or_expr '?' expr ':' cond_expr
			{ $$ = newexpr3(&@$, '?', $1, $3, $5); }
			;

const_expr		: cond_expr
			;

logical_or_expr		: logical_and_expr
			| logical_or_expr "||" logical_and_expr
			{ $$ = newexpr2(&@$, OR_OP, $1, $3); }
			;

logical_and_expr	: or_expr
			| logical_and_expr "&&" or_expr
			{ $$ = newexpr2(&@$, AND_OP, $1, $3); }
			;

or_expr			: xor_expr
			| or_expr '|' xor_expr
			{ $$ = newexpr2(&@$, '|', $1, $3); }
			;

xor_expr		: and_expr
			| xor_expr '^' and_expr
			{ $$ = newexpr2(&@$, '^', $1, $3); }
			;

and_expr		: eq_expr
			| and_expr '&' eq_expr
			{ $$ = newexpr2(&@$, '&', $1, $3); }
			;

eq_expr			: rel_expr
			| eq_expr eq_op rel_expr
			{ $$ = newexpr2(&@$, $2, $1, $3); }
			;
eq_op			: "==" | "!="
			;

rel_expr		: shift_expr
			| rel_expr rel_op shift_expr
			{ $$ = newexpr2(&@$, $2, $1, $3); }
			;
rel_op			: "<=" | ">=" | '<' | '>'
			;

shift_expr		: add_expr
			| shift_expr shift_op add_expr
			{ $$ = newexpr2(&@$, $2, $1, $3); }
			;
shift_op		: "<<" | ">>"
			;

add_expr		: mul_expr
			| add_expr add_op mul_expr
			{ $$ = newexpr2(&@$, $2, $1, $3); }
			;
add_op			: '+' | '-'
			;

mul_expr		: cast_expr
			| mul_expr mul_op cast_expr
			{ $$ = newexpr2(&@$, $2, $1, $3); }
			;
mul_op			: '*' | '/' | '%'
			;

cast_expr		: unary_expr
			| '(' type_name ')' cast_expr
			{ $$ = newexpr2(&@$, TYPECAST, $2, $4); }
			;

unary_expr		: postfix_expr
			| unary_lval_op unary_expr
			{ $$ = newexpr1(&@$, $1, $2); }
			| unary_op cast_expr
			{ $$ = newexpr1(&@$, $1, $2); }
			| SIZEOF '(' type_name  ')'
			{ $$ = newexpr1(&@$, SIZEOF_TYPE, $3); }
			/* HACK: defined as a preprocessor macro */
			| OFFSETOF '(' type_name ',' unary_expr ')'
			{ $$ = newexpr2(&@$, $1, $3, $5); }
			/* HACK */
			| FRAME_REG '(' opt_expr ',' type_name ')'
			{ $$ = newexpr2(&@$, $1, $5, $3); }
			;
unary_op		: '&' | '*' | '+' | '-' | '~' | '!'
			;
unary_lval_op		: "++" | "--" | SIZEOF
			;

postfix_expr		: primary_expr
			| postfix_expr '[' expr ']'
			{ $$ = newexpr2(&@$, ARRAY, $1, $3); }
			| postfix_expr '(' argument_expr_list ')'
			{ $$ = newexpr2(&@$, FUNC, $1, $3); }
			| postfix_expr '('                    ')'
			{ $$ = newexpr2(&@$, FUNC, $1, NULL); }
			| postfix_expr '.' { typedef_ign = 1; } ID
			{
				typedef_ign = 0;
				$$ = newexpr2(&@$, $2, $1, newexprid(&@$, $4));
			}
			| postfix_expr "->" { typedef_ign = 1; } ID
			{
				typedef_ign = 0;
				$$ = newexpr2(&@$, $2, $1, newexprid(&@$, $4));
			}
			| postfix_expr "++"
			{ $$ = newexpr1(&@$, $2, $1); }
			| postfix_expr "--"
			{ $$ = newexpr1(&@$, $2, $1); }
			;

primary_expr		: ID
			{ $$ = newexprid(&@$, $1); }
			| INT_CONST
			{ $$ = newexprnum(&@$, $1); }
			| CHAR_CONST
			{ $$ = newexprchar(&@$, $1); }
			| FLOAT_CONST
			{ $$ = newexprfloat(&@$, $1); }
			| string_const
			| '(' expr ')'
			{ $$ = $2; }
			;

string_const		: STRING_CONST
			{ $$ = newexprstr(&@$, $1); }
			| string_const STRING_CONST 
			{
				$$ = newexpr2(&@$, CONCAT, $1,
					      newexprstr(&@2, $2));
			}
			/* HACK for concatenation with macros */
			| string_const ID
			{
				$$ = newexpr2(&@$, CONCAT, $1,
					      newexprid(&@2, $2));
			}
			;

%%

static void
print_last_line(const YYLTYPE *loc, FILE *f)
{
	int column = loc->last_column;
	struct dynstr *ds;

	for (ds = loc->last_text; column > ds->len;
	     ds = list_entry(ds->list.prev, struct dynstr, list))
		column -= ds->len;
	for (;;) {
		fwrite(ds->text + ds->len - column, sizeof(char), column, f);
		if (ds == loc->last_text)
			break;
		ds = list_entry(ds->list.next, struct dynstr, list);
		column = ds->len;
		
	}
	putc('\n', f);
}

void
yyerror(const char *s)
{
	int first_column;
	int i;

	first_column = (yylloc.first_line == yylloc.last_line)
		? yylloc.first_column
		: 0;

	fflush(stdout);
	print_last_line(&yylloc, stderr);
	fprintf(stderr, "%*s", first_column + 1, "^");
	for (i = 1; i < yylloc.last_column - first_column; ++i)
		putc('^', stderr);
	fprintf(stderr, "\n%*s on line %d\n",
		first_column + 1, s, yylloc.last_line);
}

node_t *parsed_tree;

/* Initialize a new node of type @type, room for @nchild children
 * and @extra bytes.
 */
node_t *
newnode(const YYLTYPE *loc, enum node_type type, int nchild)
{
	size_t allocextra = nchild * sizeof(node_t *);
	node_t *ret = calloc(sizeof(node_t) + allocextra, 1);

	INIT_LIST_HEAD(&ret->list);
	ret->type = type;
	ret->nchild = nchild;

	ret->first_text = loc->first_text;
	list_add(&ret->first_list, &loc->first_text->node_first);
	ret->last_text = loc->last_text;
	list_add(&ret->last_list, &loc->last_text->node_last);

	return ret;
}

void
freenode(node_t *node)
{
	int i;
	for (i = 0; i < node->nchild; ++i)
		if (node->child[i])
			freenode(node->child[i]);
	list_del(&node->first_list);
	list_del(&node->last_list);
	free(node);
}

void
set_node_first(node_t *node, struct dynstr *ds)
{
	node->first_text = ds;
	list_move(&node->first_list, &ds->node_first);
}

void
set_node_last(node_t *node, struct dynstr *ds)
{
	node->last_text = ds;
	list_move(&node->last_list, &ds->node_last);
}

node_t *
newtype(const YYLTYPE *loc)
{
	return newnode(loc, nt_type, cht_max);
}

node_t *
newtype_name(const YYLTYPE *loc, const char *name)
{
	node_t *node = newtype(loc);
	node->t.name = name;
	return node;
}

node_t *
newtype_int(const YYLTYPE *loc)
{
	node_t *ret = newtype(loc);
	ret->t.category = type_basic;
	ret->t.btype = TYPE_INT;
	return ret;
}

/* Append @attr (if any) to @merger */
static void
type_add_attr(node_t *merger, node_t *attr)
{
	if (!merger->child[cht_attr]) {
		merger->child[cht_attr] = attr;
	} else if (attr) {
		struct list_head *last = merger->child[cht_attr]->list.prev;
		list_splice(&attr->list, last);
		list_add(&attr->list, last);
	}
}

/* Merge the flags and attributes from @other into @merger */
static void
type_merge(node_t *merger, node_t *other)
{
	merger->t.flags |= other->t.flags;
	type_add_attr(merger, other->child[cht_attr]);
	if (merger->t.category == type_basic &&
	    other->t.category == type_basic)
		merger->t.btype |= other->t.btype;
	freenode(other);
}

node_t *
newvar(const YYLTYPE *loc, const char *name)
{
	node_t *node = newnode(loc, nt_var, chv_max);
	node->v.name = name;
	return node;
}

static declarator_t *
newdeclarator(void)
{
	declarator_t *ret = calloc(sizeof(declarator_t), 1);
	INIT_LIST_HEAD(&ret->list);
	return ret;
}

static void
link_abstract(declarator_t *declarator, const abstract_t *abstract)
{
	if (declarator->abstract.stub) {
		*declarator->abstract.stub = abstract->tree;
		declarator->abstract.stub = abstract->stub;
	} else
		declarator->abstract = *abstract;
}

node_t *
newdecl(const YYLTYPE *loc, node_t *type, declarator_t *declarator)
{
	node_t *node = newnode(loc, nt_decl, chd_max);
	node_t *var, *lastvar;
	declarator_t *d, *nextd;

	node->child[chd_type] = type;
	if (!declarator)
		return node;

	node->child[chd_var] = declarator->var;

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
				var->child[chv_type] = d->abstract.tree;
				var->child[chv_type]->t.flags = type->t.flags;
			} else
				var->child[chv_type] = type;
		} else if (d->abstract.stub) {
			*d->abstract.stub = type;
			node->child[chd_type] = d->abstract.tree;
			node->child[chd_type]->t.flags = type->t.flags;
		}

		lastvar = var;
		nextd = list_entry(d->list.next, declarator_t, list);
		free(d);
	} while(nextd != declarator);

	return node;
}

node_t *
newexpr(const YYLTYPE *loc, int op)
{
	node_t *node = newnode(loc, nt_expr, che_max);
	node->e.op = op;
	return node;
}

node_t *
newexprnum(const YYLTYPE *loc, const char *str)
{
	node_t *ret = newexpr(loc, INT_CONST);
	ret->e.num = strtol(str, (char**)NULL, 0);
	return ret;
}

node_t *
newexprfloat(const YYLTYPE *loc, const char *str)
{
	node_t *ret = newexpr(loc, FLOAT_CONST);
	ret->e.f = strtod(str, (char**)NULL);
	return ret;
}

node_t *
newexprstr(const YYLTYPE *loc, const char *str)
{
	node_t *ret = newexpr(loc, STRING_CONST);
	ret->e.str = str;
	return ret;
}

node_t *
newexprchar(const YYLTYPE *loc, const char *str)
{
	node_t *ret = newexpr(loc, CHAR_CONST);
	ret->e.str = str;
	return ret;
}

node_t *
newexprid(const YYLTYPE *loc, const char *id)
{
	node_t *ret = newexpr(loc, ID);
	ret->e.str = id;
	return ret;
}

node_t *
newexpr1(const YYLTYPE *loc, int op, node_t *arg1)
{
	node_t *ret = newexpr(loc, op);
	ret->child[che_arg1] = arg1;
	return ret;
}

node_t *
newexpr2(const YYLTYPE *loc, int op, node_t *arg1, node_t *arg2)
{
	node_t *ret = newexpr(loc, op);
	ret->child[che_arg1] = arg1;
	ret->child[che_arg2] = arg2;
	return ret;
}

node_t *
newexpr3(const YYLTYPE *loc, int op, node_t *arg1, node_t *arg2, node_t *arg3)
{
	node_t * ret = newexpr(loc, op);
	ret->child[che_arg1] = arg1;
	ret->child[che_arg2] = arg2;
	ret->child[che_arg3] = arg3;
	return ret;
}

node_t *
newexpr4(const YYLTYPE *loc, int op,
	 node_t *arg1, node_t *arg2, node_t *arg3, node_t *arg4)
{
	node_t * ret = newexpr(loc, op);
	ret->child[che_arg1] = arg1;
	ret->child[che_arg2] = arg2;
	ret->child[che_arg3] = arg3;
	ret->child[che_arg4] = arg4;
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
addtypedeflist(node_t *first)
{
	node_t *node = first;
	do {
		if (node->type == nt_var)
			addtypedef(node->v.name);
		node = list_entry(node->list.next, node_t, list);
	} while (node != first);
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
hidevars(node_t *first)
{
	node_t *node = first;
	do {
		if (node->type == nt_var && node->v.name)
			hidetypedef(node->v.name);
		node = list_entry(node->list.next, node_t, list);
	} while (node != first);
}

static void
hidedecls(node_t *first)
{
	node_t *node = first;
	do {
		if (node->type == nt_decl && node->child[chd_var])
			hidevars(node->child[chd_var]);
		node = list_entry(node->list.next, node_t, list);
	} while (node != first);
}

static void
hidefnparams(declarator_t *first)
{
	declarator_t *decl = first;
	do {
		node_t *tree = decl->abstract.tree;
		if (!tree || tree->type != nt_type ||
		    tree->t.category != type_func) {
			fprintf(stderr, "Ouch! %s is not a function!\n",
				decl->var->v.name);
		} else if (tree->child[cht_param])
			hidedecls(tree->child[cht_param]);

		decl = list_entry(decl->list.next, declarator_t, list);
	} while (decl != first);
}
