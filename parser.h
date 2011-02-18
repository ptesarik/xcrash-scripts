/* Common definitions for the C lexer and parser */

#include "lists.h"

/* Stored file contents */
struct dynstr {
	struct list_head list;
	size_t len, alloc;
	char text[];
};

struct dynstr *newdynstr(const char *, size_t);

extern struct list_head raw_contents;

typedef struct {
	int first_line;
	int first_column;
	int last_line;
	int last_column;
	struct dynstr *first_text, *last_text;
} loc_t;
#define YYLTYPE	loc_t

/* Parsed types */

enum type_category {
	type_none = 0,
	type_basic,
	type_typedef,
	type_struct,
	type_union,
	type_enum,
	type_pointer,
	type_array,
	type_func,
	type_typeof
};

#define MAX_BASIC_TYPES 8

typedef struct basic_type {
	int list[MAX_BASIC_TYPES];
	int count;
} basic_type_t;

typedef struct struct_type {
	char *name;
	struct node *body;
} struct_type_t;

typedef struct enum_type {
	char *name;
	struct var *body;
} enum_type_t;

typedef struct array_type {
	struct node *type;	/* type of elements */
	struct expr *size;
} array_type_t;

typedef struct func_type {
	struct node *type;	/* return type */
	struct node *param;
} func_type_t;

typedef struct type {
	enum type_category category;
	unsigned flags;		/* see TF_xxx macros below */
	union {
		basic_type_t b;
		char *name;
		struct_type_t s;
		enum_type_t e;
		struct node *t;
		array_type_t a;
		func_type_t f;
		struct expr *expr;
	};
	struct list_head attr;
} type_t;

/* storage type flag bitmask: */
#define TF_AUTO		(1U << 0)
#define TF_REGISTER	(1U << 1)
#define TF_STATIC	(1U << 2)
#define TF_EXTERN	(1U << 3)
#define TF_INLINE	(1U << 4)
#define TF_TYPEDEF	(1U << 5)
/* qualifier type flag bitmask: */
#define TF_CONST	(1U << 6)
#define TF_VOLATILE	(1U << 7)

typedef struct expr {
	int op;
	union {
		long num;
		double f;
		char *str;
		type_t *type;
		struct expr *expr;
		struct node *decl;
		struct {
			type_t *type;
			struct expr *expr;
		} typecast;
		struct {
			struct expr *left;
			struct expr *right;
		} binary;
		struct {
			struct expr *cond;
			struct expr *ontrue;
			struct expr *onfalse;
		} ternary;	/* for ternary ?: */
		struct {
			struct expr *init;
			struct expr *cond;
			struct expr *iter;
			struct expr *body;
		} forloop;
	};
} expr_t;

typedef struct var {
	char *name;
} var_t;

enum {
	cht_expr = 0,
	cht_body = 0,		/* struct_type and enum_type */
	cht_type = 0,		/* array_type and func_type */
	cht_size,		/* array_type */
	cht_param = 1,		/* func_type */
	cht_attr,
	cht_max,

	che_type = 0,		/* type and typecast */
	che_expr,		/* expr and typecast */
	che_decl = 0,
	che_left = 0,
	che_right,
	che_cond = 0,
	che_ontrue,
	che_onfalse,
	che_forinit = 0,
	che_forcond,
	che_foriter,
	che_forbody,
	che_max,

	chv_type = 0,
	chv_bitsize,
	chv_init,
	chv_attr,
	chv_max,

	chd_type = 0,
	chd_var,
	chd_decl,
	chd_body,
	chd_max,
};

enum node_type {
	nt_type,
	nt_expr,
	nt_var,
	nt_decl,
};

typedef struct node {
	struct list_head list;
	enum node_type type;
	union {
		type_t t;
		expr_t e;
		var_t v;
	};
	struct dynstr *first_text, *last_text;
	size_t nchild;
	struct node *child[];
} node_t;

/* Macros to get the address of the containing node_t */
#define type_node(ptr) ({	\
        const type_t *__ptr = (ptr);	\
        (node_t *)( (char *)__ptr - offsetof(node_t,t) );})
#define expr_node(ptr) ({	\
        const expr_t *__ptr = (ptr);	\
        (node_t *)( (char *)__ptr - offsetof(node_t,e) );})
#define var_node(ptr) ({	\
        const var_t *__ptr = (ptr);	\
        (node_t *)( (char *)__ptr - offsetof(node_t,v) );})

/* This type is used only temporarily during parsing */
typedef struct abstract {
	node_t *tree;
	node_t **stub;	
} abstract_t;

/* This type is used only temporarily during parsing */
typedef struct declarator {
	struct list_head list;
	node_t *var;
	abstract_t abstract;
} declarator_t;

/* When yyparse() succeeds, the resulting tree is here: */
extern node_t *parsed_tree;

/* Parser/lexer interface */
extern FILE *yyin;
extern YYLTYPE yylloc;

void yyerror(const char *);
int yyparse(void);
int yylex(void);
int yylex_destroy(void);

/* Location handling */
extern int tabsize;
extern const char *linestart;

/* Typedef hash */
void addtypedef(const char *name);
int istypedef(const char *name);

extern int typedef_ign;		/* treat typedefs as regular identifiers */

/* Parse tree */
node_t *newnode_extra(YYLTYPE *, enum node_type, int, size_t);
static inline node_t *
newnode(YYLTYPE *loc, enum node_type type, int nchild)
{
	return newnode_extra(loc, type, nchild, 0);
}

node_t *newtype(YYLTYPE *);
node_t *newtype_name(YYLTYPE *, const char *);
node_t *newtype_int(YYLTYPE *);
void type_merge(node_t *, node_t *);

node_t *newvar(YYLTYPE *, const char *);

node_t *newdecl(YYLTYPE *, node_t *, declarator_t *);

expr_t *newexpr(YYLTYPE *, int);
expr_t *newexprnum(YYLTYPE *, char *);
expr_t *newexprfloat(YYLTYPE *, char *);
expr_t *newexprstr(YYLTYPE *, char *);
expr_t *newexprchar(YYLTYPE *, char *);
expr_t *newexprid(YYLTYPE *, char *);
expr_t *newexprtype(YYLTYPE *, int, node_t *);
expr_t *newexprtypecast(YYLTYPE *, int, node_t *, expr_t *);
expr_t *newexprdecl(YYLTYPE *, node_t *);
expr_t *newexpr1(YYLTYPE *, int, expr_t *);
expr_t *newexpr2(YYLTYPE *, int, expr_t *, expr_t *);
expr_t *newexpr3(YYLTYPE *, int, expr_t *, expr_t *, expr_t *);
expr_t *newexpr4(YYLTYPE *, int, expr_t *, expr_t *, expr_t *, expr_t *);
