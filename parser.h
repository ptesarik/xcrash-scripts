/* Common definitions for the C lexer and parser */

#include "lists.h"

/* Stored file contents */
struct dynstr {
	struct list_head list;
	size_t len, alloc;
	char text[];
};

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
	struct decl *body;
} struct_type_t;

typedef struct enum_type {
	char *name;
	struct var *body;
} enum_type_t;

typedef struct array_type {
	struct type *type;	/* type of elements */
	struct expr *size;
} array_type_t;

typedef struct func_type {
	struct type *type;	/* return type */
	struct decl *param;
} func_type_t;

typedef struct type {
	enum type_category category;
	unsigned flags;		/* see TF_xxx macros below */
	union {
		basic_type_t b;
		char *name;
		struct_type_t s;
		enum_type_t e;
		struct type *t;
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
	struct list_head list;
	int op;
	union {
		long num;
		double f;
		char *str;
		type_t *type;
		struct expr *expr;
		struct decl *decl;
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
	struct list_head list;
	char *name;
	type_t *type;
	expr_t *bitsize;	/* used for struct members */
	struct list_head attr;
	expr_t *init;
} var_t;

typedef struct abstract {
	type_t *tree;
	type_t **stub;	
} abstract_t;

typedef struct declarator {
	struct list_head list;
	var_t *var;
	abstract_t abstract;
} declarator_t;

typedef struct decl {
	struct list_head list;
	type_t *type;
	var_t *var;
	struct decl *decl;
	expr_t *body;
} decl_t;

typedef struct{
	union {
		long num;
		char *str;
		type_t *type;
		var_t *var;
		expr_t *expr;
		decl_t *decl;
	};
} unit_t;

/* When yyparse() succeeds, the resulting tree is here: */
extern decl_t *parsed_tree;

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
type_t *newtype(void);
type_t *newtype_name(const char *);
type_t *newtype_int(void);
void type_merge(type_t *, type_t *);

var_t *newvar(const char *);

declarator_t *newdeclarator(void);
void link_abstract(declarator_t *, const abstract_t *);

decl_t *newdecl(type_t *, declarator_t *);

expr_t *newexpr(int);
expr_t *newexprnum(char *);
expr_t *newexprfloat(char *);
expr_t *newexprstr(char *);
expr_t *newexprchar(char *);
expr_t *newexprid(char *);
expr_t *newexprtype(int, type_t *);
expr_t *newexprtypecast(int, type_t *, expr_t *);
expr_t *newexprdecl(decl_t *);
expr_t *newexpr1(int, expr_t *);
expr_t *newexpr2(int, expr_t *, expr_t *);
expr_t *newexpr3(int, expr_t *, expr_t *, expr_t *);
expr_t *newexpr4(int, expr_t *, expr_t *, expr_t *, expr_t *);
