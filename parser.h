/* Common definitions for the C lexer and parser */

#ifndef PARSER_H
#define PARSER_H

#include <stdio.h>		/* for FILE */
#include <string.h>		/* for strrchr */
#include <stdlib.h>		/* for free */
#include "lists.h"

typedef union {
	struct {
		int fake:1;	/* not found in input file */
		int macro:1;	/* verbatim macro text (before expansion) */
	};
	unsigned val;
} dynstr_flags_t;

#define null_dynstr_flags	((dynstr_flags_t) {})
#define fake_dynstr_flags	((dynstr_flags_t) {{.fake=1}})
#define macro_dynstr_flags	((dynstr_flags_t) {{.macro=1}})

/* Stored file contents */
struct macro_exp;
struct dynstr {
	struct list_head list;
	struct list_head node_first, node_last;
	struct list_head dup_list;
	struct node *cpp_cond;
	struct macro_exp *exp;

	int token;
	int refcount;		/* external references (with node->str) */

	dynstr_flags_t flags;

	size_t len, alloc;
	char text[];
};

struct dynstr *newdynstr(const char *, size_t, dynstr_flags_t);
struct dynstr *dupdynstr(struct dynstr *);
void freedynstr(struct dynstr *ds);

#define next_dynstr(ds)	\
	(list_entry((ds)->list.next, struct dynstr, list))
#define prev_dynstr(ds)	\
	(list_entry((ds)->list.prev, struct dynstr, list))
#define text_dynstr(ptr) ({	\
        const char *__mptr = (ptr);	\
        (struct dynstr *)(__mptr - offsetof(struct dynstr,text) );})
#define first_dynstr(listptr) \
	(list_entry((listptr)->next, struct dynstr, list))
#define last_dynstr(listptr) \
	(list_entry((listptr)->prev, struct dynstr, list))

extern struct list_head raw_contents;

/* If non-zero, this is the first token returned by the lexer
 * It is used to override the parser's start symbol by forcing
 * the lexer to return a special START_xxx pseudo-token.
 */
extern int start_symbol;

/* If @lex_input_first is non-NULL, then we're parsing dynstr
 * strings instead of an input stream.
 */
extern struct dynstr *lex_input_first, *lex_input_last;

/* If @macrods is non-NULL, then we're reading in the result of
 * macro expansion. This is different from setting @lex_input_first:
 *   1. we don't create any new dynstr objects, and
 *   2. there's no tokenization - the existing tokens are returned
 */
extern struct dynstr *macrods;

/* Default flags for newly created dynstr objects. */
extern dynstr_flags_t lex_dynstr_flags;

struct parsed_file;

typedef struct {
	struct parsed_file *pf;
	int line;
	unsigned short column, vcolumn;
	struct dynstr *text;
} pos_t;

/* Update @loc with @len characters starting at @p */
void update_pos(pos_t *pos, const char *p, size_t len);

typedef struct loc {
	pos_t first, last;
	struct loc *parent;
} loc_t;
#define YYLTYPE	loc_t

/* Parsed types */

enum type_category {
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

enum basic_type {
	bt_void = 0,
	bt_signed,
	bt_unsigned,
	bt_char,
	bt_short,
	bt_int,
	bt_long,
	bt_longlong,
	bt_float,
	bt_double,

	bt_max
};

#define TYPE_VOID	(1UL << bt_void)
#define TYPE_SIGNED	(1UL << bt_signed)
#define TYPE_UNSIGNED	(1UL << bt_unsigned)
#define TYPE_CHAR	(1UL << bt_char)
#define TYPE_SHORT	(1UL << bt_short)
#define TYPE_INT	(1UL << bt_int)
#define TYPE_LONG	(1UL << bt_long)
#define TYPE_LONGLONG	(1UL << bt_longlong)
#define TYPE_FLOAT	(1UL << bt_float)
#define TYPE_DOUBLE	(1UL << bt_double)

typedef struct type {
	enum type_category category;
	unsigned flags;		/* see TF_xxx macros below */
	unsigned long btype;
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
	};
} expr_t;

enum {
	cht_expr = 0,
	cht_body = 0,		/* struct_type and enum_type */
	cht_type = 0,		/* array_type and func_type */
	cht_size,		/* array_type */
	cht_param = 1,		/* func_type */
	cht_attr,
	cht_max,

	che_arg1 = 0,
	che_arg2,
	che_arg3,
	che_arg4,
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
	};

	struct dynstr *str;
	struct node *parent;
	struct list_head dup_list;

	loc_t loc;
	struct list_head first_list, last_list;

	struct list_head user_list;
	void *user_data;	/* user-specific data */

	int nchild;
	struct list_head child[];
} node_t;

/* Macros to get the address of the containing node_t */
#define type_node(ptr) ({	\
        const type_t *__ptr = (ptr);	\
        (node_t *)( (char *)__ptr - offsetof(node_t,t) );})
#define expr_node(ptr) ({	\
        const expr_t *__ptr = (ptr);	\
        (node_t *)( (char *)__ptr - offsetof(node_t,e) );})

/* This type is used only temporarily during parsing */
typedef struct abstract {
	node_t *tree;
	node_t *stub;
} abstract_t;

/* This type is used only temporarily during parsing */
typedef struct declarator {
	struct list_head list;
	node_t *var;
	abstract_t abstract;
} declarator_t;

/* When yyparse() succeeds, the resulting tree is here: */
extern struct list_head parsed_tree;

/* Location handling */
extern int tabsize;

/* Typedef hash */
void cleartypedefs(void);
void addtypedef(const char *name);
int istypedef(const char *name);
void init_predef_types(void);

/* Macro hash */
struct hashed_macro {
	struct hashed_macro *next;
	char *name;

	struct list_head params;
	int nparams;		/* number of parameters */

	struct node *cpp_cond;
	YYLTYPE loc;

	int undef:1;		/* #undefined macro */
	int hidden:1;		/* macro should be ignored in searches */
	int hasparam:1;		/* a macro that has parameters */
	int isparam:1;		/* is a macro parameter? */
	int noexpand:1;		/* should not expand (prevent recursion) */
	int variadic:1;		/* this a variadic parameter */
};

void clearmacros(void);
struct hashed_macro *findmacro(const struct dynstr *ds, YYLTYPE *loc);
void undefmacro(const char *name, const YYLTYPE *loc);

/* Macro expansion */
struct macro_exp {
	int refcount;			     /* Reference count */
	struct hashed_macro *hm;
	struct dynstr *first, *last;	     /* Unexpanded macro text */
	struct dynstr *exp_first, *exp_last; /* Macro expansion */
	struct {
		struct dynstr *first, *last;
	} params[];
};

static inline struct macro_exp *
get_macro_exp(struct macro_exp *exp)
{
	++exp->refcount;
	return exp;
}

static inline void
put_macro_exp(struct macro_exp *exp)
{
	if (exp && !--exp->refcount)
		free(exp);
}

/* Parser/lexer interface */
extern FILE *yyin;
union YYSTYPE;

int yyparse(YYLTYPE *);
int yyparse_macro(YYLTYPE *, const char *, int, node_t *);

int yylex(union YYSTYPE *val, YYLTYPE *loc);
int yylex_cpp_arg(union YYSTYPE *val, YYLTYPE *loc);
int yylex_destroy(void);

void lex_push_state(void);
void lex_pop_state(void);

void yyerror(YYLTYPE *loc, YYLTYPE *fileloc, const char *);

struct dynstr *expand_macro(YYLTYPE *loc, struct hashed_macro *hm);

/* Parse tree */
node_t *newnode(const YYLTYPE *, enum node_type, int);
void freenode(node_t *);
node_t *dupnode(node_t *);
node_t *dupnode_nochild(node_t *);

void set_node_first(node_t *, struct dynstr *);
void set_node_last(node_t *, struct dynstr *);

/* Set @node's string reference */
static inline void
set_node_str(node_t *node, struct dynstr *str)
{
	if (node->str)
		--node->str->refcount;
	if ( (node->str = str) )
		++node->str->refcount;
}

/* Add @child (possibly a linked list) to the @parent node */
static inline void
set_node_child(node_t *parent, int pos, node_t *child)
{
	if (child) {
		struct list_head childlist;
		list_add_tail(&childlist, &child->list);
		list_for_each_entry(child, &childlist, list)
			child->parent = parent;
		list_splice(&childlist, parent->child[pos].prev);
	}
}

static inline node_t *
first_node(const struct list_head *nodelist)
{
	return list_entry(nodelist->next, node_t, list);
}

static inline node_t *
last_node(const struct list_head *nodelist)
{
	return list_entry(nodelist->prev, node_t, list);
}

node_t *newtype(const YYLTYPE *);
node_t *newtype_name(const YYLTYPE *, struct dynstr *);
node_t *newtype_int(const YYLTYPE *);
node_t *newtype_macro(const YYLTYPE *);

node_t *newvar(const YYLTYPE *, struct dynstr *);

node_t *newdecl(const YYLTYPE *, node_t *, declarator_t *);

node_t *newexpr(const YYLTYPE *, int);
node_t *newexprnum(const YYLTYPE *, struct dynstr *);
node_t *newexprfloat(const YYLTYPE *, struct dynstr *);
node_t *newexprstr(const YYLTYPE *, struct dynstr *);
node_t *newexprchar(const YYLTYPE *, struct dynstr *);
node_t *newexprid(const YYLTYPE *, struct dynstr *);
node_t *newexpr1(const YYLTYPE *, int, node_t *);
node_t *newexpr2(const YYLTYPE *, int, node_t *, node_t *);
node_t *newexpr3(const YYLTYPE *, int, node_t *, node_t *, node_t *);
node_t *newexpr4(const YYLTYPE *, int, node_t *, node_t *, node_t *, node_t *);

/* A dummy (empty) dynstr and location */
extern struct dynstr dummydynstr;
extern YYLTYPE dummyloc;

/* Linked string */
struct linkedstr {
	struct list_head list;
	char text[];
};

/* Program arguments */
struct arguments {
	char *basedir;
	struct list_head xform_names;
};

/* Tree walking */
enum walk_action {
	walk_continue,		/* Continue the walk */
	walk_terminate,		/* Terminate the walk */
	walk_skip_children,	/* Skip children */
};

typedef enum walk_action walkfn(node_t *, void *);
enum walk_action walk_tree(struct list_head *tree, walkfn *fn, void *data);
enum walk_action walk_tree_single(node_t *tree, walkfn *fn, void *data);

/* Tree transformation functions */
int dump_contents(struct list_head *contents, FILE *f);
void dump_tree(struct list_head *tree);

struct parsed_file {
	const char *name;
	struct list_head parsed;
	struct list_head raw;
	YYLTYPE loc;
	int clean;
};

struct file_array {
	int n;			/* number of files in the array */
	struct parsed_file *array;
};

/* Iterate over all files in a struct file_array @fa */
#define files_for_each(pos, fa)	\
	for ((pos) = (fa)->array; (pos) != &(fa)->array[(fa)->n]; ++(pos))

int file_name(int);
int parse_file(struct parsed_file *);
node_t *reparse_node(node_t *, int);
int xform_files(struct arguments *, const struct file_array *);

/* Returns non-zero iff:
 *   a. the file name of @pf ends with a ".h", or
 *   b. it is the built-in pseudo-file
 */
static inline int
is_header_file(struct parsed_file *pf)
{
	if (pf->name) {
		char *suffix = strrchr(pf->name, '.');
		return suffix && suffix[1] == 'h' && !suffix[2];
	} else
		return 1;
}

#endif	/* PARSER_H */
