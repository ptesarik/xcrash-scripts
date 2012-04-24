#ifndef INDIRECT_H
#define INDIRECT_H

#include "parser.h"

/* Type indirection
 *
 * Positive numbers denote the position of a function argument.
 * Negative numbers have special meanings.
 */
typedef char ind_t;
enum ind {
	ind_stop = 0,		/* End-of-array marker */
	ind_pointer = -1,	/* Pointer to a type */
	ind_return = -2,	/* Function return type */
	ind_implicit = -3	/* Implicit pointer */
};

#define MAXIND	32		/* Arbitrarily chosen constant... */

/* Build the indirection array from a base type
 * Returns: the first non-type parent node
 */
node_t *build_ind(node_t *type, ind_t **indp);

/* Get the base type of @type
 * Returns NULL if @ind cannot be (fully) followed.
 */
node_t *ind_base_type(node_t *type, const ind_t *ind);

/* Check whether @ind refers to a pointer */
static inline int
ind_is_pointer(ind_t ind)
{
	return (ind == ind_pointer || ind == ind_implicit);
}

/* Check whether @ind refers to a function */
static inline int
ind_is_func(ind_t ind)
{
	return (ind == ind_return || ind > 0);
}

/* Check whether @expr refers to host-specific data */
int is_host_type(node_t *expr, ind_t *ind);

#endif	/* INDIRECT_H */
