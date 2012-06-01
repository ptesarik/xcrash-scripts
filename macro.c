/* CPP macro handling */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "parser.h"
#include "clang.tab.h"

/************************************************************
 * Macro hash
 *
 */

#define HASH_SIZE	512

struct hashed_macro {
	struct hashed_macro *next;
	char *name;
	struct list_head params;
	struct dynstr *first, *last;
	int hidden:1;		/* macro should be ignored in searches */
	int hasparam:1;		/* a macro that has parameters */
	int isparam:1;		/* is a macro parameter? */
	int noexpand:1;		/* should not expand (prevent recursion) */
};

static struct hashed_macro *macros[HASH_SIZE];

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
findmacro(const char *name)
{
	unsigned hash = mkhash(name);
	struct hashed_macro *hm;
	for (hm = macros[hash]; hm; hm = hm->next) {
		if (!hm->hidden && !strcmp(name, hm->name))
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
	hm->first = hm->last = NULL;
	hm->hidden = 0;
	hm->hasparam = 0;
	hm->isparam = 0;
	hm->noexpand = 0;
	macros[hash] = hm;
	return hm;
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
			if (hm->first && hm->first->fake)
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
yyparse_macro(YYLTYPE *loc, const char *name, int hasparam)
{
	struct hashed_macro *hm;
	YYSTYPE val;
	int token, ntoken;

	if (! (hm = addmacro(name)) )
		return -1;

	if (hasparam) {
		hm->hasparam = 1;

		token = yylex(&val, loc);
		if (token != '(') {
			yyerror(loc, "expecting '('");
			return 1;
		}

		do {
			node_t *var;

			token = yylex(&val, loc);
			if (token == ')')
				break;
			if (token != ID && token != ELLIPSIS) {
				yyerror(loc, "expecting ')', ID or '...'");
				return 1;
			}

			var = newvar(loc, val.str);
			list_add_tail(&var->list, &hm->params);

			ntoken = yylex(&val, loc);
			if (token == ID && ntoken == ELLIPSIS)
				ntoken = yylex(&val, loc);
			token = ntoken;
		} while (token == ',');
		if (token != ',' && token != ')') {
			yyerror(loc, "expecting ',' or ')'");
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
parse_macro_args(YYLTYPE *loc, struct hashed_macro *hm)
{
	node_t *param;
	YYSTYPE val;
	int token;

	token = yylex_cpp_arg(&val, loc);
	if (token != '(') {
		yyerror(loc, "expecting '('");
		return 1;
	}

	list_for_each_entry(param, &hm->params, list) {
		int paren = 0;
		struct hashed_macro *arg = addmacro(param->str->text);
		param->user_data = arg;

		while ((token = yylex_cpp_arg(&val, loc)) &&
		       (paren || (token != ',' && token != ')')) ) {
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
		yyerror(loc, "expecting ')'");
		return 1;
	}

	return 0;
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
	ret->fake = 1;
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

static struct dynstr *
expand_body(YYLTYPE *loc, struct hashed_macro *hm, struct list_head *point)
{
	struct dynstr *ds, *ret;
	enum {
		normal,		/* Initial state */
		stringify,	/* After the '#' token was seen */
	} state;

	if (!hm->first)
		return NULL;

	state = normal;
	ret = last_dynstr(point);
	for (ds = hm->first; ; ds = next_dynstr(ds)) {
		struct hashed_macro *nested;
		if (state == stringify) {
			struct dynstr *dupds;
			if (ds->token)
				state = normal;
			if (ds->token == ID &&
			    (nested = findmacro(ds->text)) &&
			    nested->isparam) {
				nested = nested->next;
				dupds = dupmerge(nested->first, nested->last);
				dupds->token = STRING_CONST;
				list_add_tail(&dupds->list, point);
			} else
				yyerror(loc, "Invalid use of '#'");
		} else if (ds->token == '#')
			state = stringify;
		else if (ds->token == ID &&
			 (nested = findmacro(ds->text)) &&
			 !nested->noexpand) {
			struct dynstr *newfirst, *newlast;
			struct dynstr *oldmacrods = macrods;

			macrods = next_dynstr(ds);
			newfirst = expand_macro(loc, nested);
			ds = macrods ? prev_dynstr(macrods) : hm->last;
			macrods = oldmacrods;

			if (newfirst) {
				struct dynstr *pointds = 
					list_entry(point, struct dynstr, list);
				newlast = last_dynstr(&raw_contents);
				detach_text(newfirst, newlast);
				insert_text_list(pointds, newfirst, newlast);
			}
		} else {
			struct dynstr *dupds = dupdynstr(ds);
			list_add_tail(&dupds->list, point);
		}

		if (ds == hm->last)
			break;
	}
	ret = next_dynstr(ret);
	return &ret->list != point ? ret : NULL;
}

static void
expand_params(YYLTYPE *loc, struct hashed_macro *hm)
{
	node_t *param;

	list_for_each_entry(param, &hm->params, list)
		((struct hashed_macro *)param->user_data)->hidden = 1;

	list_for_each_entry(param, &hm->params, list) {
		struct hashed_macro *arg = param->user_data;
		struct hashed_macro *newarg = addmacro(arg->name);
		newarg->isparam = 1;
		newarg->hidden = 1;

		newarg->first = expand_body(loc, arg, &arg->first->list);
		newarg->last = newarg->first ? prev_dynstr(arg->first) : NULL;

		param->user_data = newarg;
	}

	list_for_each_entry(param, &hm->params, list) {
		struct hashed_macro *arg = param->user_data;
		arg->hidden = 0;
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

struct dynstr *
expand_macro(YYLTYPE *loc, struct hashed_macro *hm)
{
	struct dynstr *ret;

	if (hm->hasparam) {
		if (parse_macro_args(loc, hm))
			return NULL;

		expand_params(loc, hm);
	}

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
