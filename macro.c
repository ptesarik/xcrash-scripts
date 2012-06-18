/* CPP macro handling */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "parser.h"
#include "clang.tab.h"
#include "tools.h"

/************************************************************
 * Macro hash
 *
 */

#define HASH_SIZE	512

static struct hashed_macro *macros[HASH_SIZE];

static struct dynstr *do_expand(YYLTYPE *, struct hashed_macro *,
				struct macro_exp *exp);

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
clearmacros(void)
{
	unsigned hash;
	for (hash = 0; hash < HASH_SIZE; ++hash) {
		struct hashed_macro *hm, *next;
		next = macros[hash];
		macros[hash] = NULL;
		while ( (hm = next) ) {
			next = hm->next;
			free(hm);
		}
	}
}

struct hashed_macro *
findmacro(const struct dynstr *ds)
{
	unsigned hash = mkhash(ds->text);
	struct hashed_macro *hm;
	for (hm = macros[hash]; hm; hm = hm->next) {
		if (!hm->hidden && !strcmp(ds->text, hm->name) &&
		    !cond_is_disjunct(ds->cpp_cond, hm->cpp_cond))
			return hm->undef ? NULL : hm;
	}
	return NULL;
}

static struct hashed_macro *
findhiddenmacro(const char *name)
{
	unsigned hash = mkhash(name);
	struct hashed_macro *hm;
	for (hm = macros[hash]; hm; hm = hm->next) {
		if (hm->hidden && !strcmp(name, hm->name))
			return hm;
	}
	return NULL;
}

static struct hashed_macro *
addmacro(const char *name)
{
	unsigned hash = mkhash(name);
	struct hashed_macro *hm;

	hm = malloc(sizeof(struct hashed_macro) + strlen(name) + 1);
	hm->next = macros[hash];
	hm->name = (char*)(hm + 1);
	strcpy(hm->name, name);
	INIT_LIST_HEAD(&hm->params);
	hm->nparams = 0;
	hm->cpp_cond = NULL;
	hm->first = hm->last = NULL;
	hm->undef = 0;
	hm->hidden = 0;
	hm->hasparam = 0;
	hm->isparam = 0;
	hm->noexpand = 0;
	hm->variadic = 0;
	macros[hash] = hm;
	return hm;
}

void
undefmacro(const char *name)
{
	struct hashed_macro *hm = addmacro(name);
	hm->undef = 1;
}

static void
delmacro_text(struct hashed_macro *hm)
{
	struct dynstr *ds = hm->first;

	detach_text(hm->first, hm->last);
	do {
		struct dynstr *next = next_dynstr(ds);
		freedynstr(ds);
		ds = next;
	} while (ds != hm->first);
}		

static void
delmacro(const char *name)
{
	unsigned hash = mkhash(name);
	struct hashed_macro *hm, **pprev;
	pprev = &macros[hash];
	while ( (hm = *pprev) ) {
		if (!strcmp(name, hm->name)) {
			*pprev = hm->next;
			if (hm->first && hm->first->flags.fake)
				delmacro_text(hm);
			free(hm);
			break;
		}
		pprev = &hm->next;
	}
}

/************************************************************
 * Macro parser
 *
 */

int
yyparse_macro(YYLTYPE *loc, const char *name, int hasparam, node_t *cpp_cond)
{
	struct hashed_macro *hm;
	YYSTYPE val;
	int token, ntoken;

	if (! (hm = addmacro(name)) )
		return -1;
	hm->cpp_cond = cpp_cond;

	if (hasparam) {
		hm->hasparam = 1;

		token = yylex(&val, loc);
		if (token != '(') {
			yyerror(loc, NULL, "expecting '('");
			return 1;
		}

		do {
			node_t *var;

			token = yylex(&val, loc);
			if (token == ')')
				break;
			if (token != ID && token != ELLIPSIS) {
				yyerror(loc, NULL,
					"expecting ')', ID or '...'");
				return 1;
			}

			if (token == ELLIPSIS) {
				struct dynstr *ds;
				hm->variadic = 1;
				ds = newdynstr("__VA_ARGS__", 11);
				ds->flags.fake = 1;
				list_add_tail(&ds->list, &raw_contents);
				var = newvar(loc, ds);
			} else
				var = newvar(loc, val.str);
			list_add_tail(&var->list, &hm->params);
			++hm->nparams;

			ntoken = yylex(&val, loc);
			if (token == ID && ntoken == ELLIPSIS) {
				hm->variadic = 1;
				ntoken = yylex(&val, loc);
			}
			token = ntoken;
		} while (token == ',');
		if (token != ',' && token != ')') {
			yyerror(loc, NULL, "expecting ',' or ')'");
			return 1;
		}
	}

	while ( (token = yylex(&val, loc)) ) {
		if (!hm->first)
			hm->first = loc->first_text;
		hm->last = loc->last_text;
	}

	return 0;
}

static int
do_parse_macro_args(YYLTYPE *loc, struct hashed_macro *hm)
{
	node_t *param;
	YYSTYPE val;
	int token;

	token = yylex_cpp_arg(&val, loc);
	if (token != '(') {
		yyerror(loc, NULL, "expecting '('");
		return 1;
	}

	list_for_each_entry(param, &hm->params, list) {
		int paren = 0;
		struct hashed_macro *arg = addmacro(param->str->text);
		arg->hidden = 1;

		while ((token = yylex_cpp_arg(&val, loc)) &&
		       (paren || ((token != ',' || hm->variadic) &&
				  token != ')')) ) {
			if (token == '(')
				++paren;
			else if (token == ')')
				--paren;
			if (!arg->first)
				arg->first = loc->first_text;
			arg->last = loc->last_text;
		}
	}

	if (list_empty(&hm->params))
		token = yylex_cpp_arg(&val, loc);

	if (token != ')') {
		yyerror(loc, NULL, "expecting ')'");
		return 1;
	}

	return 0;
}

static int
parse_macro_args(YYLTYPE *loc, struct hashed_macro *hm)
{
	YYLTYPE lloc = *loc;
	int ret = do_parse_macro_args(&lloc, hm);

	loc->last_line    = lloc.last_line;
	loc->last_column  = lloc.last_column;
	loc->last_vcolumn = lloc.last_vcolumn;
	loc->last_text    = lloc.last_text;
	return ret;
}

static struct dynstr *
dupconcat(struct dynstr *a, struct dynstr *b)
{
	struct dynstr *ret = newdynstr(NULL, a->len + b->len);
	char *p = ret->text;
	memcpy(p, a->text, a->len);
	memcpy(p + a->len, b->text, b->len);
	return ret;
}

static struct dynstr *
dupmerge(struct dynstr *first, struct dynstr *last)
{
	struct dynstr *endmark = next_dynstr(last);
	struct dynstr *ds, *ret;

	size_t len = 0;
	for (ds = first, len = 0; ds != endmark; ds = next_dynstr(ds))
		len += ds->len;
	ret = newdynstr(NULL, len);

	char *p = ret->text;
	for (ds = first; ds != endmark; ds = next_dynstr(ds)) {
		memcpy(p, ds->text, ds->len);
		p += ds->len;
	}
	return ret;
}

static struct dynstr *
duplist(struct dynstr *first, struct dynstr *last)
{
	if (!first)
		return NULL;
	
	struct dynstr *ret = dupdynstr(first);
	while (first != last) {
		first = next_dynstr(first);
		struct dynstr *ds = dupdynstr(first);
		list_add_tail(&ds->list, &ret->list);
	}
	return ret;
}

static void
remove_macros(struct dynstr *first, struct dynstr *last)
{
	while (&first->list != last->list.next) {
		struct dynstr *next = next_dynstr(first);
		if (first->flags.macro) {
			list_del(&first->list);
			freedynstr(first);
		}
		first = next;
	}
}

static void
cpp_stringify(struct hashed_macro *hm, struct list_head *point)
{
	struct dynstr *ds;

	ds = newdynstr("\"", 1);
	list_add_tail(&ds->list, point);

	ds = dupmerge(hm->first, hm->last);
	ds->token = STRING_CONST;
	list_add_tail(&ds->list, point);

	ds = newdynstr("\"", 1);
	list_add_tail(&ds->list, point);
}

static void
cpp_concat(struct list_head *point, struct dynstr *ds, struct dynstr *prevtok)
{
	struct hashed_macro *nested;
	struct dynstr *dupds, *merged;

	if (ds->token == ID &&
	    (nested = findmacro(ds)) &&
	    nested->isparam) {
		nested = nested->next;
		dupds = duplist(nested->first, nested->last);
		insert_text_list(list_entry(point, struct dynstr, list),
				 dupds, prev_dynstr(dupds));
	} else {
		dupds = dupdynstr(ds);
		list_add_tail(&dupds->list, point);
	}

	struct dynstr *first = last_dynstr(&raw_contents);

	merged = dupconcat(prevtok, dupds);
	lex_push_state();
	lex_input_first = lex_input_last = merged;
	YYSTYPE val; YYLTYPE loc;
	while (yylex(&val, &loc));
	lex_pop_state();
	freedynstr(merged);

	first = next_dynstr(first);
	if (&first->list != &raw_contents) {
		struct dynstr *last = last_dynstr(&raw_contents);

		detach_text(first, last);
		replace_text_list(prevtok, dupds, first, last);
		remove_macros(first, last);
	}
}

static struct dynstr *
expand_body(YYLTYPE *loc, struct hashed_macro *hm, struct list_head *point)
{
	struct dynstr *ds, *prevtok, *ret;
	enum {
		normal,		/* Initial state */
		stringify,	/* After the '#' token was seen */
		concat,		/* After the '##' token was seen */
	} state;
	YYLTYPE lloc;

	if (!hm->first)
		return NULL;

	init_loc(&lloc, loc);

	state = normal;
	prevtok = NULL;
	ret = last_dynstr(point);
	for (ds = hm->first; ; ds = next_dynstr(ds)) {
		struct hashed_macro *nested;

		lloc.first_line    = lloc.last_line;
		lloc.first_column  = lloc.last_column;
		lloc.first_vcolumn = lloc.last_vcolumn;
		update_loc(&lloc, ds->text, ds->len);
		lloc.first_text = lloc.last_text = ds;

		if (state == stringify) {
			if (ds->token)
				state = normal;
			if (ds->token == ID &&
			    (nested = findmacro(ds)) &&
			    nested->isparam)
				cpp_stringify(nested->next, point);
			else
				yyerror(&lloc, NULL, "Invalid use of '#'");
		} else if (state == concat) {
			if (ds->token) {
				state = normal;
				cpp_concat(point, ds, prevtok);
				prevtok = last_dynstr(point);
			}
		} else if (ds->token == '#')
			state = stringify;
		else if (ds->token == CPP_CONCAT) {
			if (prevtok)
				state = concat;
		} else if (ds->token == ID &&
			   (nested = findmacro(ds)) &&
			   !nested->noexpand) {
			struct dynstr *newfirst, *newlast;
			struct dynstr *oldmacrods = macrods;

			macrods = next_dynstr(ds);
			newfirst = do_expand(&lloc, nested, NULL);
			ds = macrods ? prev_dynstr(macrods) : hm->last;
			macrods = oldmacrods;

			if (newfirst) {
				struct dynstr *pointds = 
					list_entry(point, struct dynstr, list);
				newlast = last_dynstr(&raw_contents);
				detach_text(newfirst, newlast);
				insert_text_list(pointds, newfirst, newlast);
				prevtok = newlast;
			} else
				prevtok = NULL;
		} else {
			struct dynstr *dupds = dupdynstr(ds);
			list_add_tail(&dupds->list, point);
			if (ds->token)
				prevtok = dupds;
		}

		if (ds == hm->last)
			break;
	}
	ret = next_dynstr(ret);
	return &ret->list != point ? ret : NULL;
}

static void
expand_params(YYLTYPE *loc, struct hashed_macro *hm, struct macro_exp *exp)
{
	node_t *param;
	int nparam;

	nparam = 0;
	list_for_each_entry(param, &hm->params, list) {
		struct hashed_macro *arg = findhiddenmacro(param->str->text);
		struct hashed_macro *newarg = addmacro(arg->name);
		newarg->isparam = 1;
		newarg->hidden = 1;

		newarg->first = expand_body(loc, arg, &arg->first->list);
		newarg->last = newarg->first ? prev_dynstr(arg->first) : NULL;

		if (exp) {
			exp->params[nparam].first = arg->first;
			exp->params[nparam].last = arg->last;
			++nparam;
		}
	}

	list_for_each_entry(param, &hm->params, list) {
		struct hashed_macro *arg = findhiddenmacro(param->str->text);
		arg->hidden = 0;
		assert(arg->next);
		assert(arg->next->hidden);
		assert(!strcmp(arg->next->name, arg->name));
		arg->next->hidden = 0;
	}
}

static void
delete_params(struct hashed_macro *hm)
{
	node_t *param;
	list_for_each_entry(param, &hm->params, list) {
		delmacro(param->str->text);
		delmacro(param->str->text);
	}
}

static struct dynstr *
do_expand(YYLTYPE *loc, struct hashed_macro *hm, struct macro_exp *exp)
{
	struct dynstr *ret;

	if (hm->hasparam) {
		lex_dynstr_flags.macro = 1;
		if (parse_macro_args(loc, hm))
			return NULL;
		lex_dynstr_flags.macro = 0;

		lex_dynstr_flags.fake = 1;
		expand_params(loc, hm, exp);
	}
	lex_dynstr_flags.fake = 1;

	if (hm->isparam) {
		ret = duplist(hm->first, hm->last);
		if (ret) {
			struct dynstr *pointds =
				list_entry(&raw_contents, struct dynstr, list);
			insert_text_list(pointds, ret, prev_dynstr(ret));
		}
	} else {
		hm->noexpand = 1;
		ret = expand_body(loc, hm, &raw_contents);
		hm->noexpand = 0;
	}

	delete_params(hm);

	return ret;
}

struct dynstr *
expand_macro(YYLTYPE *loc, struct hashed_macro *hm)
{
	dynstr_flags_t saved_dynstr_flags = lex_dynstr_flags;
	struct dynstr *ds, *ret;
	struct macro_exp *exp;

	ds = loc->first_text;
	do {
		ds->flags.macro = 1;
		ds = next_dynstr(ds);
	} while (&ds->list != loc->last_text->list.next);

	exp = malloc(sizeof(struct macro_exp) +
		     hm->nparams * sizeof(exp->params[0]));
	exp->hm = hm;

	ret = do_expand(loc, hm, exp);

	exp->first = loc->first_text;
	exp->last = loc->last_text;
	exp->exp_first = next_dynstr(exp->last);
	exp->exp_last = last_dynstr(&raw_contents);
	exp->refcount = 0;

	ds = exp->first;
	while (&ds->list != &raw_contents) {
		put_macro_exp(ds->exp);
		ds->exp = get_macro_exp(exp);
		ds = next_dynstr(ds);
	}

	if (!exp->refcount)
		free(exp);

	lex_dynstr_flags = saved_dynstr_flags;
	return ret;
}
