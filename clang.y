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

static void type_merge(node_t *, node_t *);

static declarator_t *newdeclarator(void);
static void link_abstract(abstract_t *, const abstract_t *);

static void addtypedeflist(node_t *);
static void hidetypedef(const char *);
static void unhidetypedefs(void);
static void hidefnparams(declarator_t *);
static void hidevars(struct list_head *);
static void hidedecls(struct list_head *);

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
%token <token> CPP_IF CPP_IFDEF CPP_IFNDEF CPP_ELIF CPP_ELSE CPP_ENDIF
%token <token> CPP_CONCAT	"##"
%token <str> CPP_IDARG

/* start symbol pseudo-tokens */
%token START_TYPE_NAME
%token START_EXPR
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

%type <str> id_or_typeid

/* type type */
%type <node> type_decl notype_decl type_name typedef_name
%type <node> type_spec basic_type_list spec_qualifier_list
%type <node> struct_or_union_spec struct_desc enum_spec enum_desc

%type <abstract> pointer array_declarator direct_suffix_declarator
%type <abstract> param_declarator abstract_param_declarator

%type <declarator> declarator direct_declarator
%type <declarator> func_declarators func_declarator direct_func_declarator
%type <declarator> init_declarator init_declarator_list
%type <declarator> struct_declarator struct_declarator_list
%type <declarator> abstract_declarator direct_abstract_declarator

/* expr type */
%type <node> expr opt_expr assign_expr cond_expr const_expr logical_or_expr
%type <node> logical_and_expr or_expr xor_expr and_expr eq_expr rel_expr
%type <node> shift_expr add_expr mul_expr cast_expr unary_expr primary_expr
%type <node> string_const
%type <node> initializer initializer_list array_size postfix_expr
%type <node> compound_stat compound_body stat
%type <node> argument_expr argument_expr_list
%type <node> opt_attr attr_spec attr_list attribute attr_param_list

/* var type */
%type <node> enum_body enumerator_list enumerator
%type <node> id_list

/* decl type */
%type <node> external_decl func_def decl_list decl
%type <node> param_decl param_list param_type_list
%type <node> param_type_or_idlist
%type <node> struct_body struct_decl_list struct_decl

/* CPP types */
%type <node> directive macro_def macro_declarator macro_param
%type <token> cpp_cond

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
			| translation_unit external_decl
			{ list_add_tail(&$2->list, &parsed_tree); }
			| START_TYPE_NAME type_name
			{ list_add_tail(&$2->list, &parsed_tree); }
			| START_EXPR expr
			{ list_add_tail(&$2->list, &parsed_tree); }
			| START_DIRECTIVE directive
			{
				if($2)
					list_add_tail(&$2->list,
						      &parsed_tree);
			}
			;

directive		: CPP_DEFINE macro_def
			{ $$ = $2; }
			| cpp_cond expr
			{ $$ = newexpr1(&@$, $1, $2); }
			| CPP_ELSE
			{ $$ = newexpr(&@$, $1); }
			| CPP_ENDIF
			{ $$ = newexpr(&@$, $1); }
			| /* empty */
			{ $$ = NULL; }
			;

cpp_cond		: CPP_IF
			| CPP_IFDEF
			| CPP_IFNDEF
			| CPP_ELIF
			;

macro_def		: macro_declarator compound_body
			{
				$$ = newdecl(&@$, NULL, NULL);
				set_node_child($$, chd_var, $1);
				set_node_child($$, chd_body, $2);
			}
			;

macro_declarator	: CPP_IDARG macro_param
			{
				node_t *type = newtype(&@$);
				type->t.category = type_func;
				set_node_child(type, cht_param, $2);
				$$ = newvar(&@1, $1);
				set_node_child($$, chv_type, type);
			}
			| ID
			{
				node_t *type = newtype(&@$);
				type->t.category = type_func;
				$$ = newvar(&@1, $1);
				set_node_child($$, chv_type, type);
			}
			;

macro_param		: '(' id_list ')'
			{
				$$ = newdecl(&@$, NULL, NULL);
				set_node_child($$, chd_var, $2);
			}
			| '(' ')'
			{ $$ = NULL; }
			;

external_decl		: func_def
			| decl
			;

func_def		: type_decl func_declarators decl_list compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, $1, $2);
				set_node_child($$, chd_decl, $3);
				set_node_child($$, chd_body, $4);
			}
			| type_decl func_declarators           compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, $1, $2);
				set_node_child($$, chd_body, $3);
			}
			|           func_declarators decl_list compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, newtype_int(&@$), $1);
				set_node_child($$, chd_decl, $2);
				set_node_child($$, chd_body, $3);
			}
			|           func_declarators           compound_stat
			{
				unhidetypedefs();
				$$ = newdecl(&@$, newtype_int(&@$), $1);
				set_node_child($$, chd_body, $2);
			}
			;

/* HACK to cope with multiple #ifdef'd functions in unwind.c */
func_declarators	: func_declarator
			{
				hidefnparams($1);
				$$ = $1;
			}
			| func_declarators func_declarator
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
			{ $$ = newdecl(&@$, $1, NULL); }
			;

type_decl		: notype_decl type_spec notype_decl
			{
				$$ = $2;
				type_merge($$, $1);
				type_merge($$, $3);
			}
			| notype_decl type_spec
			{
				$$ = $2;
				type_merge($$, $1);
			}
			|             type_spec notype_decl
			{
				$$ = $1;
				type_merge($$, $2);
			}
			|             type_spec
			| notype_decl
			;

notype_decl		: attr_spec
			{
				$$ = newtype(&@$);
				set_node_child($$, cht_attr, $1);
			}
			| storage_class_spec
			{ $$ = newtype(&@$); $$->t.flags = $1; }
			| type_qualifier
			{ $$ = newtype(&@$); $$->t.flags = $1; }
			| notype_decl attr_spec
			{
				$$ = $1;
				set_node_child($$, cht_attr, $2);
			}
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

struct_or_union_spec	: struct_or_union opt_attr struct_desc
			{
				$$ = $3;
				$$->t.category = $1;
				set_node_child($$, cht_attr, $2);
				set_node_first($$, @$.first_text);
			}
			;

struct_or_union		: STRUCT	{ $$ = type_struct; }
			| UNION		{ $$ = type_union; }
			;

struct_desc		: id_or_typeid struct_body
			{
				$$ = newtype_name(&@$, $1);
				set_node_child($$, cht_body, $2);
			}
			|              struct_body
			{
				$$ = newtype(&@$);
				set_node_child($$, cht_body, $1);
			}
			| id_or_typeid
			{ $$ = newtype_name(&@$, $1); }
			;

struct_body		: '{' struct_decl_list '}'
			{ $$ = $2; }
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
				set_node_child($$->var, chv_bitsize, $3);
			}
			|            ':' const_expr
			{
				$$ = newdeclarator();
				$$->var = newvar(&@$, NULL);
				set_node_child($$->var, chv_bitsize, $2);
			}
			| declarator
			;

enum_spec		: ENUM opt_attr enum_desc
			{
				$$ = $3;
				$$->t.category = type_enum;
				set_node_child($$, cht_attr, $2);
				set_node_first($$, @$.first_text);
			}
			;

enum_desc		: id_or_typeid enum_body
			{
				$$ = newtype_name(&@$, $1);
				set_node_child($$, cht_body, $2); }
			|              enum_body
			{
				$$ = newtype(&@$);
				set_node_child($$, cht_body, $1);
			}
			| id_or_typeid
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
				set_node_child($$, chv_init, $3);
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
				set_node_child($$->var, chv_init, $3);
			}
			| declarator
			;

/* FIXME: This should be _type_decl sans storage specifiers */
spec_qualifier_list	: type_decl
			;

declarator		: pointer direct_declarator opt_attr
			{
				$$ = $2;
				link_abstract(&$$->abstract, &$1);
				if ($3)
					set_node_child($$->var, chv_attr, $3);
			}
			|         direct_declarator opt_attr
			{
				$$ = $1;
				if ($2)
					set_node_child($$->var, chv_attr, $2);
			}
			;

direct_declarator	: id_or_typeid
			{
				$$ = newdeclarator();
				$$->var = newvar(&@$, $1);
			}
			| '(' declarator ')'
			{ $$ = $2; }
			| direct_declarator direct_suffix_declarator
			{
				$$ = $1;
				link_abstract(&$$->abstract, &$2);
			}
			;

id_or_typeid		: ID
			| TYPEID
			;

direct_suffix_declarator: array_declarator
			| param_declarator
			;

func_declarator		: pointer direct_func_declarator
			{
				$$ = $2;
				link_abstract(&$$->abstract, &$1);
			}
			|         direct_func_declarator
			;

direct_func_declarator	: ID param_declarator
			{
				$$ = newdeclarator();
				$$->var = newvar(&@1, $1);
				link_abstract(&$$->abstract, &$2);
			}
			;

param_declarator	: '(' param_type_or_idlist ')'
			{
				node_t *type = newtype(&@$);
				type->t.category = type_func;
				set_node_child(type, cht_param, $2);
				$$.tree = $$.stub = type;
			}
			;

param_type_or_idlist	: param_type_list
			| id_list
			{
				$$ = newdecl(&@$, NULL, NULL);
				set_node_child($$, chd_var, $1);
			}
			;

pointer			: '*'
			{
				node_t *ptr = newtype(&@$);
				ptr->t.category = type_pointer;
				$$.tree = $$.stub = ptr;
			}
			| pointer '*'
			{
				node_t *ptr = newtype(&@$);
				ptr->t.category = type_pointer;
				set_node_child(ptr, cht_type, $1.tree);
				$$.tree = ptr;
				$$.stub = $1.stub;
			}
			| pointer type_qualifier_list
			{ $$ = $1; $$.tree->t.flags |= $2; }
			;

param_type_list		: param_list ',' "..."
			| param_list
			| /* empty */
			{ $$ = NULL; }
			;

param_list		: param_decl
			| param_list ',' param_decl
			{
				list_add_tail(&$3->list, &$1->list);
				$$ = $1;
			}
			;

param_decl		: type_decl declarator
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

type_name		: spec_qualifier_list
			| spec_qualifier_list abstract_declarator
			{
				set_node_child($2->abstract.stub,
					       cht_type, $1);
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
				set_node_child($$, cht_expr, $3);
			}
			;

abstract_declarator	: pointer
			{ $$ = newdeclarator(); $$->abstract = $1; }
			| direct_abstract_declarator
			| pointer direct_abstract_declarator
			{ $$ = $2; link_abstract(&$$->abstract, &$1); }
			;

direct_abstract_declarator
			: '(' abstract_declarator ')'
			{ $$ = $2; }
			|                            array_declarator
			{ $$ = newdeclarator(); $$->abstract = $1; }
			| direct_abstract_declarator array_declarator
			{ $$ = $1; link_abstract(&$$->abstract, &$2); }
			|                            abstract_param_declarator
			{ $$ = newdeclarator(); $$->abstract = $1; }
			| direct_abstract_declarator abstract_param_declarator
			{ $$ = $1; link_abstract(&$$->abstract, &$2); }
			;

array_declarator	: '[' array_size ']'
			{
				node_t *type = newtype(&@$);
				type->t.category = type_array;
				set_node_child(type, cht_size, $2);
				$$.tree = $$.stub = type;
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
				set_node_child(type, cht_param, $2);
				$$.tree = $$.stub = type;
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
				hidevars(&$2->child[chd_var]);
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

argument_expr_list	: argument_expr
			| argument_expr_list ',' argument_expr
			{ list_add_tail(&$3->list, &$1->list); }
			;

argument_expr		: assign_expr
			| type_name
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
			| postfix_expr '.' id_or_typeid
			{ $$ = newexpr2(&@$, $2, $1, newexprid(&@$, $3)); }
			| postfix_expr "->" id_or_typeid
			{ $$ = newexpr2(&@$, $2, $1, newexprid(&@$, $3)); }
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

struct list_head parsed_tree;

/* Initialize a new node of type @type, room for @nchild children
 * and @extra bytes.
 */
node_t *
newnode(const YYLTYPE *loc, enum node_type type, int nchild)
{
	size_t allocextra = nchild * sizeof(struct list_head);
	node_t *ret = calloc(sizeof(node_t) + allocextra, 1);
	int i;

	INIT_LIST_HEAD(&ret->list);
	ret->type = type;
	ret->nchild = nchild;
	for (i = 0; i < nchild; ++i)
		INIT_LIST_HEAD(&ret->child[i]);

	ret->first_text = loc->first_text;
	list_add(&ret->first_list, &loc->first_text->node_first);
	ret->last_text = loc->last_text;
	list_add(&ret->last_list, &loc->last_text->node_last);

	return ret;
}

node_t *
dupnode(node_t *node)
{
	size_t allocextra = node->nchild * sizeof(struct list_head);
	node_t *ret = malloc(sizeof(node_t) + allocextra);
	int i;

	memcpy(ret, node, sizeof(node_t));

	INIT_LIST_HEAD(&ret->list);
	for (i = 0; i < ret->nchild; ++i) {
		node_t *child;
		INIT_LIST_HEAD(&ret->child[i]);
		list_for_each_entry(child, &node->child[i], list)
			set_node_child(ret, i, dupnode(child));
	}

	ret->first_text = node->first_text;
	list_add(&ret->first_list, &ret->first_text->node_first);
	ret->last_text = node->last_text;
	list_add(&ret->last_list, &ret->last_text->node_last);

	return ret;
}

void
freenode(node_t *node)
{
	int i;
	for (i = 0; i < node->nchild; ++i) {
		node_t *child, *next;
		list_for_each_entry_safe(child, next, &node->child[i], list)
			freenode(child);
	}
	list_del(&node->list);
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

/* Merge the flags and attributes from @other into @merger */
static void
type_merge(node_t *merger, node_t *other)
{
	node_t *attr;
	list_for_each_entry(attr, &other->child[cht_attr], list)
		attr->parent = merger;
	merger->t.flags |= other->t.flags;
	list_splice(&other->child[cht_attr], merger->child[cht_attr].prev);
	INIT_LIST_HEAD(&other->child[cht_attr]);
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
link_abstract(abstract_t *parent, const abstract_t *child)
{
	if (parent->stub)
		set_node_child(parent->stub, cht_type, child->tree);
	else
		parent->tree = child->tree;
	parent->stub = child->stub;
}

node_t *
newdecl(const YYLTYPE *loc, node_t *type, declarator_t *declarator)
{
	node_t *node = newnode(loc, nt_decl, chd_max);
	declarator_t *d, *nextd;
	node_t *typedup;

	if (!declarator) {
		set_node_child(node, chd_type, type);
		return node;
	}

	nextd = declarator;
	do {
		d = nextd;

		if (d->abstract.stub) {
			set_node_child(d->abstract.stub, cht_type, type);
			typedup = dupnode(d->abstract.tree);
			typedup->t.flags = type->t.flags;
			list_del_init(&type->list);
			freenode(d->abstract.tree);
		} else
			typedup = dupnode(type);

		if (d->var) {
			set_node_child(d->var, chv_type, typedup);
			set_node_child(node, chd_var, d->var);
		} else
			set_node_child(node, chd_type, typedup);

		nextd = list_entry(d->list.next, declarator_t, list);
		free(d);
	} while(nextd != declarator);

	freenode(type);

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
	set_node_child(ret, che_arg1, arg1);
	return ret;
}

node_t *
newexpr2(const YYLTYPE *loc, int op, node_t *arg1, node_t *arg2)
{
	node_t *ret = newexpr(loc, op);
	set_node_child(ret, che_arg1, arg1);
	set_node_child(ret, che_arg2, arg2);
	return ret;
}

node_t *
newexpr3(const YYLTYPE *loc, int op, node_t *arg1, node_t *arg2, node_t *arg3)
{
	node_t * ret = newexpr(loc, op);
	set_node_child(ret, che_arg1, arg1);
	set_node_child(ret, che_arg2, arg2);
	set_node_child(ret, che_arg3, arg3);
	return ret;
}

node_t *
newexpr4(const YYLTYPE *loc, int op,
	 node_t *arg1, node_t *arg2, node_t *arg3, node_t *arg4)
{
	node_t * ret = newexpr(loc, op);
	set_node_child(ret, che_arg1, arg1);
	set_node_child(ret, che_arg2, arg2);
	set_node_child(ret, che_arg3, arg3);
	set_node_child(ret, che_arg4, arg4);
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

void
cleartypedefs(void)
{
	unsigned hash;
	for (hash = 0; hash < HASH_SIZE; ++hash) {
		struct hashed_type *ht, *next;
		next = typedefs[hash];
		typedefs[hash] = NULL;
		while ( (ht = next) ) {
			next = ht->next;
			free(ht);
		}
	}
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
hidevars(struct list_head *vars)
{
	node_t *node;
	list_for_each_entry(node, vars, list) {
		if (node->type == nt_var && node->v.name)
			hidetypedef(node->v.name);
	}
}

static void
hidedecls(struct list_head *decls)
{
	node_t *node;
	list_for_each_entry(node, decls, list) {
		if (node->type == nt_decl)
			hidevars(&node->child[chd_var]);
	}
}

static void
hidefnparams(declarator_t *first)
{
	declarator_t *decl = first;
	do {
		node_t *tree = decl->abstract.tree;
		if (tree && tree->type == nt_type &&
		    tree->t.category == type_func)
			hidedecls(&tree->child[cht_param]);
		else
			fprintf(stderr, "Ouch! %s is not a function!\n",
				decl->var->v.name);

		decl = list_entry(decl->list.next, declarator_t, list);
	} while (decl != first);
}
