/* Array things
   Copyright (C) 2000-2008 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of G95.

G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include <string.h>

#include "g95.h"


static match match_array_cons_element(g95_constructor **);

typedef struct cons_stack {
    g95_iterator *iterator;
    struct cons_stack *previous;
} cons_stack;

static cons_stack *stack_base;

static try check_constructor(g95_constructor *, try (*)(g95_expr *));

typedef struct iterator_stack {
    g95_symbol *variable;
    bignum value;

    struct iterator_stack *prev;
} iterator_stack;

static iterator_stack *iter_stack = NULL;

typedef struct {
    bignum extract_n;
    g95_expr *extracted;
    bignum *count;
    enum { C_NONE=0, C_EXPANDING, C_COUNTING, C_SIZING } context;

    g95_constructor *head, *tail;

    try (*expand_work_function)(g95_expr *);
} expand_info;

static expand_info current_expand;

static g95_typespec constructor_ts, target_ts;

static enum { CONS_START, CONS_GOOD, CONS_BAD } cons_state;

int g95_constructor_string_length;

static try expand_constructor(g95_constructor *);


/* g95_free_array_ref()-- Free an array reference structure and
 * everything it points to. */

void g95_free_array_ref(g95_array_ref *ar) {
int i;

    for(i=0; i<G95_MAX_DIMENSIONS; i++) {
	g95_free_expr(ar->start[i]);
	g95_free_expr(ar->end[i]);
	g95_free_expr(ar->stride[i]);
    }

    g95_free(ar);
}



/* g95_free_array_spec()-- Free all of the expressions associated with
 * array bounds specifications */

void g95_free_array_spec(g95_array_spec *as) {
int i;

    if (as == NULL)
	return; 

    for(i=0; i<as->rank; i++) {
	g95_free_expr(as->lower[i]);
	g95_free_expr(as->upper[i]);
    }

    g95_free(as);
}



/* initial_dimen_type()-- Hazard a guess at the type of a dimension. */

static int initial_dimen_type(g95_expr *e) {
int t, u;

    switch(e->type) {
    case EXPR_VARIABLE:
	switch(e->rank) {
	case -1:   t = DIMEN_UNKNOWN; break;
	case  0:   t = DIMEN_ELEMENT; break;
	default:   t = DIMEN_VECTOR;  break;
	}

	break;

    case EXPR_CONSTANT:
	t = DIMEN_ELEMENT;
	break;

    case EXPR_OP:
	t = initial_dimen_type(e->value.op.op1);
	if (e->value.op.op2 == NULL)
	    break;

	u = initial_dimen_type(e->value.op.op2);

	if (t == DIMEN_VECTOR || u == DIMEN_VECTOR)
	    t = DIMEN_VECTOR;

	else if (t == DIMEN_UNKNOWN || u == DIMEN_UNKNOWN)
	    t = DIMEN_UNKNOWN;

	else
	    t = DIMEN_ELEMENT;

	break;

    case EXPR_FUNCTION:
	t = DIMEN_UNKNOWN;
	break;

    case EXPR_ARRAY:
	t = DIMEN_VECTOR;
	break;

    default:
	g95_internal_error("initial_dimen_type(): Bad type");
    }

    return t;
}



/* match_subscript()-- Match a single dimension of an array reference.
 * This can be a single element or an array section.  Any modifications
 * we've made to the ar structure are cleaned up by the caller.  */

static match match_subscript(g95_array_ref *ar, int init) {
match m;
int i;

    i = ar->dimen;

    g95_gobble_whitespace();
    ar->where[i] = g95_current_locus;
    ar->start[i] = ar->end[i] = ar->stride[i] = NULL;

    if (g95_match_char(':') == MATCH_YES)
	goto end_element;

    /* Get start element */

    if (init)
	m = g95_match_init_expr(&ar->start[i]);
    else
	m = g95_match_expr(&ar->start[i]);

    if (m == MATCH_NO)
	g95_error("Expected array subscript at %C");

    if (m != MATCH_YES)
	return MATCH_ERROR;

    if (g95_match_char(':') == MATCH_NO) {
	ar->dimen_type[i] = initial_dimen_type(ar->start[i]);

	return MATCH_YES;
    }

/* Get an optional end element.  Because we've seen the colon, we
 * definitely have a range along this dimension. */

end_element:
    ar->dimen_type[i] = DIMEN_RANGE;

    if (init)
	m = g95_match_init_expr(&ar->end[i]);
    else
	m = g95_match_expr(&ar->end[i]);

    if (m == MATCH_ERROR)
	return MATCH_ERROR;

/* See if we have an optional stride */

    if (g95_match_char(':') == MATCH_YES) {
	m = init
	    ? g95_match_init_expr(&ar->stride[i])
	    : g95_match_expr(&ar->stride[i]);

	if (m == MATCH_NO)
	    g95_error("Expected array subscript stride at %C");

	if (m != MATCH_YES)
	    return MATCH_ERROR;
    }

    return MATCH_YES;
}


/* g95_copy_coarray_spec()-- Copy a coarray specification. */

g95_coarray_spec *g95_copy_coarray_spec(g95_coarray_spec *src) {
g95_coarray_spec *dest;
int i;

    if (src == NULL)
	return NULL;

    dest = g95_get_coarray_spec();
    *dest = *src;

    for(i=0; i<dest->corank; i++) {
	dest->lower[i] = g95_copy_expr(dest->lower[i]);
	dest->upper[i] = g95_copy_expr(dest->upper[i]);
    }

    return dest;
}



/* g95_match_coarray_ref()-- Match a coarray reference. */

match g95_match_coarray_ref(g95_coarray_ref *car) {
match m;
int i;

    if (g95_match_char('[') == MATCH_NO)
	return MATCH_NO;

    memset(car, '\0', sizeof(g95_coarray_ref));

    car->dimen = 0;

    for(;;) {
	m = g95_match_expr(&car->element[car->dimen]);

	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	if (m == MATCH_NO || g95_match_char(',') != MATCH_YES) {
	    car->dimen++;
	    break;
	}

	if (++car->dimen >= G95_MAX_DIMENSIONS) {
	    g95_error("Too many dimensions in coarray reference at %C");
	    goto cleanup;
	}
    }

    if (g95_match_char(']') != MATCH_YES) {
	g95_error("Syntax error in coarray reference at %C");
	goto cleanup;
    }

    return MATCH_YES;

cleanup:
    for(i=0; i<car->dimen; i++) {
	g95_free_expr(car->element[i]);
	car->element[i] = NULL;
    }

    return MATCH_ERROR;
}



/* g95_match_array_ref()-- Match an array reference, whether it is the
 * whole array or a particular elements or a section. */

match g95_match_array_ref(g95_array_ref *ar, int init) {
match m;
int i;

    memset(ar, '\0', sizeof(g95_array_ref));

    if (g95_match_char('(') != MATCH_YES) {
	ar->type = AR_FULL;
	ar->dimen = 0;
	return MATCH_YES;
    }

    ar->type = AR_UNKNOWN;

    for(ar->dimen=0; ar->dimen<G95_MAX_DIMENSIONS; ar->dimen++) {
	m = match_subscript(ar, init);
	if (m == MATCH_ERROR)
	    goto error;

	if (g95_match_char(')') == MATCH_YES)
	    goto matched;

	if (g95_match_char(',') != MATCH_YES) {
	    g95_error("Invalid form of array reference at %C");
	    goto error;
	}
    }

    g95_error("Array reference at %C cannot have more than "
	    stringize(G95_MAX_DIMENSIONS) " dimensions");

error:
    return MATCH_ERROR;

matched:
    ar->dimen++;
    ar->type = AR_ELEMENT;

    for(i=0; i<ar->dimen; i++)
	switch(ar->dimen_type[i]) {
	case DIMEN_ELEMENT:
	    break;

	case DIMEN_RANGE:
	case DIMEN_VECTOR:
	    ar->type = AR_SECTION;
	    goto done;

	case DIMEN_UNKNOWN:
	    if (ar->type == AR_ELEMENT)
		ar->type = AR_UNKNOWN;

	    break;
	}

done:
    return MATCH_YES;
}



/* g95_free_coarray_spec()-- Free all of the expressions associated
 * with coarray bounds specification. */

void g95_free_coarray_spec(g95_coarray_spec *cas) {
int i;

    if (cas == NULL)
	return;

    for(i=0; i<cas->corank; i++) {
	g95_free_expr(cas->lower[i]);
	g95_free_expr(cas->upper[i]);
    }

    g95_free(cas);
}



/* g95_simplify_array_spec()-- Simplify things in an array
 * specification. */

try g95_simplify_array_spec(g95_array_spec *as) {
int i;

    for(i=0; i<as->rank; i++)
	if (g95_simplify_init_expr(as->lower[i]) == FAILURE ||
	    g95_simplify_init_expr(as->upper[i]) == FAILURE)
	    return FAILURE;

    return SUCCESS;
}



/* g95_constant_array_spec()-- Return nonzero if an array
 * specification is constant.  Generates an error if necessary. */

int g95_constant_array_spec(g95_array_spec *as, int flag) {
g95_expr *e;
int i, m;

    if (as == NULL)
	return 1;

    e = NULL;
    m = 1;

    for(i=0; i<as->rank; i++) {
	e = as->lower[i];
	if (!g95_is_constant_expr(e)) {
	    m = 0;
	    break;
	}

	e = as->upper[i];
	if (e != NULL && !g95_is_constant_expr(e)) {
	    m = 0;
	    break;
	}
    }

    if (flag && !m)
	g95_error("Array bound at %L must be constant", &e->where);

    return m;
}



/* resolve_array_bound()-- Resolve an expression associated with an
 * array bound. */

static try resolve_array_bound(g95_expr *e) {

    if (e == NULL)
	return SUCCESS;

    if (g95_resolve_expr(e) == FAILURE ||
	g95_specification_expr(e) == FAILURE ||
	g95_simplify_spec_expr(e) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* g95_resolve_array_spec()-- Takes an array specification, resolves
 * the expressions that make up the shape and make sure everything is
 * integral. */

try g95_resolve_array_spec(g95_array_spec *as) {
int i;

    if (as == NULL)
	return SUCCESS;

    for(i=0; i<as->rank; i++)
	if (resolve_array_bound(as->lower[i]) == FAILURE ||
	    resolve_array_bound(as->upper[i]) == FAILURE)
	    return FAILURE;

    return SUCCESS;
}



/* match_deferred_coarray_spec()-- Match a deferred coarray
 * specification.  The first colon has already been seen.  This
 * subroutine amounts to a colon-counter. */

static match match_deferred_coarray_spec(g95_coarray_spec *cas) {

    cas->type = CAS_DEFERRED;

    for(;;) {
	if (g95_match_char(']') == MATCH_YES)
	    break;

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;

	if (++cas->corank >= G95_MAX_DIMENSIONS) {
	    g95_error("Too many dimensions in coarray specification at %C");
	    return MATCH_ERROR;
	}

	if (g95_match_char(':') != MATCH_YES)
	    goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in deferred coarray specification at %C");
    return MATCH_ERROR;
}


/* match_assumed_coarray_spec()-- Match an assumed-size coarray
 * specification.  The * signals the final dimension, and we eat the
 * trailing right square brace. */

static match match_assumed_coarray_spec(g95_coarray_spec *cas) {
match m;
int i;

    cas->type   = CAS_ASSUMED;
    cas->corank = 0;

    for(i=0; i<G95_MAX_DIMENSIONS; i++)
	cas->lower[i] = cas->upper[i] = NULL;

    for(;;) {
	if (g95_match_char('*') == MATCH_YES) {
	    cas->lower[cas->corank++] = g95_int_expr(1);
	    break;
	}

	m = g95_match_expr(&cas->lower[cas->corank]);

	if (m == MATCH_ERROR)
	    return m;

	if (m == MATCH_NO)
	    goto syntax;

	if (g95_match_char(':') != MATCH_YES) {
	    cas->upper[cas->corank] = cas->lower[cas->corank];
	    cas->lower[cas->corank] = g95_int_expr(1);

	} else {
	    if (g95_match_char('*') == MATCH_YES) {
		cas->corank++;
		break;
	    }

	    m = g95_match_expr(&cas->upper[cas->corank]);
	    if (m == MATCH_ERROR)
		return m;

	    if (m == MATCH_NO)
		goto syntax;
	}

	cas->corank++;

	if (g95_match_char(',') == MATCH_NO)
	    break;

	if (cas->corank >= G95_MAX_DIMENSIONS) {
	    g95_error("Too many dimensions in coarray specification at %C");
	    return MATCH_ERROR;
	}
    }

    if (g95_match_char(']') == MATCH_NO)
	goto syntax;

    if (cas->corank == 0 || cas->upper[cas->corank - 1] != NULL) {
	g95_error("Coarray specification at %C must be assumed-size");
	return MATCH_ERROR;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in coarray specification at %C");
    return MATCH_ERROR;
}



/* g95_match_coarray_spec()-- Match a coarray specification. */

match g95_match_coarray_spec(g95_coarray_spec **cas_p) {
g95_coarray_spec *cas;
match m;
int i;

    if (g95_match_char('[') != MATCH_YES) {
	*cas_p = NULL;
	return MATCH_NO;
    }

    cas = g95_get_coarray_spec();

    for(i=0; i<G95_MAX_DIMENSIONS; i++) {
	cas->lower[i] = NULL;
	cas->upper[i] = NULL;
    }

    cas->corank = 1;
    cas->artificial = 0;

    m = (g95_match_char(':') == MATCH_YES)
	? match_deferred_coarray_spec(cas)
	: match_assumed_coarray_spec(cas);

    if (m == MATCH_YES)
	*cas_p = cas;

    else
	g95_free_coarray_spec(cas);

    return m;
}



/* match_array_element_spec()-- Match a single array element
 * specification.  The return values as well as the upper and lower
 * bounds of the array spec are filled in according to what we see on
 * the input.  The caller makes sure individual specifications make
 * sense as a whole.
 *
 *       Parsed       Lower   Upper  Returned
 *       ------------------------------------
 *         :          NULL    NULL   AS_DEFERRED
 *         x           1       x     AS_EXPLICIT
 *         x:          x      NULL   AS_ASSUMED_SHAPE
 *         x:y         x       y     AS_EXPLICIT
 *         x:*         x      NULL   AS_ASSUMED_SIZE
 *         *           1      NULL   AS_ASSUMED_SIZE
 * Anything else is AS_UNKNOWN */

static array_type match_array_element_spec(g95_array_spec *as) {
g95_expr **upper, **lower;
match m;

    lower = &as->lower[as->rank - 1];
    upper = &as->upper[as->rank - 1];

    if (g95_match_char('*') == MATCH_YES) {
	*lower = g95_int_expr(1);
	return AS_ASSUMED_SIZE;
    }

    if (g95_match_char(':') == MATCH_YES)
	return AS_DEFERRED;

    m = g95_match_expr(upper);
    if (m == MATCH_NO)
	g95_error("Expected expression in array specification at %C");

    if (m != MATCH_YES)
	return AS_UNKNOWN;

    if (g95_match_char(':') == MATCH_NO) {
	*lower = g95_int_expr(1);
	return AS_EXPLICIT;
    }

    *lower = *upper;
    *upper = NULL;

    if (g95_match_char('*') == MATCH_YES)
	return AS_ASSUMED_SIZE;

    m = g95_match_expr(upper);
    if (m == MATCH_ERROR)
	return AS_UNKNOWN;

    if (m == MATCH_NO)
	return AS_ASSUMED_SHAPE;

    return AS_EXPLICIT;
}



/* g95_match_array_spec()-- Matches an array specification,
 * incidentally figuring out what sort it is.  */

match g95_match_array_spec(g95_array_spec **asp) {
array_type current_type;
g95_array_spec *as;
match m;
int i;

    if (g95_match_char('(') != MATCH_YES) {
	*asp = NULL;
	return MATCH_NO;
    }

    as = g95_get_array_spec();

    for(i=0; i<G95_MAX_DIMENSIONS; i++) {
	as->lower[i] = NULL;
	as->upper[i] = NULL;
    }

    as->rank = 1;

    for(;;) {
	current_type = match_array_element_spec(as);

	if (as->rank == 1) {
	    if (current_type == AS_UNKNOWN)
		goto cleanup;

	    as->type = current_type;
	} else
	    switch(as->type) {
		/* See how current spec meshes with the existing */
	    case AS_UNKNOWN:
		goto cleanup;

	    case AS_EXPLICIT:
		if (current_type == AS_ASSUMED_SIZE) {
		    as->type = AS_ASSUMED_SIZE;
		    break;
		}

		if (current_type == AS_EXPLICIT)
		    break;

		g95_error("Bad array specification for an explicitly shaped "
			  "array at %C");

		goto cleanup;
	
	    case AS_ASSUMED_SHAPE:
		if ((current_type == AS_ASSUMED_SHAPE) ||
		    (current_type == AS_DEFERRED))
		    break;

		g95_error("Bad array specification for assumed shape array "
			  "at %C");
		goto cleanup;

	    case AS_DEFERRED:
		if (current_type == AS_DEFERRED)
		    break;

		if (current_type == AS_ASSUMED_SHAPE) {
		    as->type = AS_ASSUMED_SHAPE;
		    break;
		}

		g95_error("Bad specification for deferred shape array at %C");
		goto cleanup;
	  
	    case AS_ASSUMED_SIZE:
		g95_error("Bad specification for assumed size array at %C");
		goto cleanup;
	    }

	if (g95_match_char(')') == MATCH_YES)
	    break;

	if (g95_match_char(',') != MATCH_YES) {
	    g95_error("Expected another dimension in array declaration at %C");
	    goto cleanup;
	}

	if (as->rank >= G95_MAX_DIMENSIONS) {
	    g95_error("Array specification at %C has more than "
		      stringize(G95_MAX_DIMENSIONS) " dimensions");
	    goto cleanup;
	}

	as->rank++;
    }

/* If a lower bounds of an assumed shape array is blank, put in one. */

    if (as->type == AS_ASSUMED_SHAPE) {
	for(i=0; i<as->rank; i++) {
	    if (as->lower[i] == NULL)
		as->lower[i] = g95_int_expr(1);
	}
    }

    *asp = as;

    m = MATCH_YES;

    for(i=0; i<as->rank; i++)
	if (g95_simplify_expr(as->lower[i]) == FAILURE ||
	    g95_simplify_expr(as->upper[i]) == FAILURE)
	    m = MATCH_ERROR;

    return m;

/* Something went wrong */

 cleanup:
    g95_free_array_spec(as);
    return MATCH_ERROR;
}



/* copy_array_spec()-- Copy an array specification. */

g95_array_spec *g95_copy_array_spec(g95_array_spec *src) {
g95_array_spec *dest;
int i;

    if (src == NULL)
	return NULL;

    dest = g95_get_array_spec();

    *dest = *src;

    for(i=0; i<dest->rank; i++) {
	dest->lower[i] = g95_copy_expr(dest->lower[i]);
	dest->upper[i] = g95_copy_expr(dest->upper[i]);
    }

    return dest;
}



/* g95_set_array_spec()-- Given a symbol and an array specification,
 * modify the symbol to have that array specification.  The error
 * locus is needed in case something goes wrong.  On failure, the
 * caller must free the spec. */

try g95_set_array_spec(g95_symbol *sym, g95_array_spec *as,
		       g95_locus *error_loc) {

    if (as == NULL)
	return SUCCESS;

    if (g95_add_dimension(&sym->attr, sym->name, error_loc) == FAILURE)
	return FAILURE;

    sym->as = as;
    return SUCCESS;
}



/* g95_compare_array_spec()-- Compares two array specifications.  */

int g95_compare_array_spec(g95_array_spec *as1, g95_array_spec *as2) {
int i, a1, a2;

    if (as1 == NULL && as2 == NULL)
	return 1;

    if (as1 == NULL || as2 == NULL || as1->rank != as2->rank)
	return 0;

    if (as1->rank == 0)
	return 1;

    if (as1->type != as2->type)
	return 0;

    if (as1->type == AS_EXPLICIT)
	for(i=0; i<as1->rank; i++) {
	    if (g95_extract_int(as1->lower[i], &a1) != NULL ||
		g95_extract_int(as2->lower[i], &a2) != NULL)
		goto error;

	    if (a1 != a2)
		return 0;

	    if (g95_extract_int(as1->upper[i], &a1) != NULL ||
		g95_extract_int(as2->upper[i], &a2) != NULL)
		goto error;

	    if (a1 != a2)
		return 0;
	}

    return 1;

error:
    g95_internal_error("g95_compare_array_spec(): Array spec clobbered");
    return 0;        /* Keep the compiler happy */
}


/* g95_append_constructor()-- Given an array constructor expression,
 * append the new expression node onto the constructor. */

void g95_append_constructor(g95_expr *base, g95_expr *new) {
g95_constructor *c;

    if (base->value.constructor.c == NULL)
	base->value.constructor.c = c = g95_get_constructor();

    else {
	c = base->value.constructor.c;
	while(c->next)
	    c=c->next;

	c->next = g95_get_constructor();
	c = c->next;
    }

    c->expr = new;

    if (new->ts.type != base->ts.type || new->ts.kind != base->ts.kind)
	g95_internal_error("g95_append_constructor(): "
			   "New node has wrong kind");
}



/* g95_free_constructor()-- Free chains of g95_constructor structures */

void g95_free_constructor(g95_constructor *p) {
g95_constructor *next;

    if (p == NULL)
	return;

    for(; p; p=next) {
	next = p->next;

	g95_free_expr(p->expr);
	if (p->iterator != NULL)
	    g95_free_iterator(p->iterator, 1);

	g95_free(p);
    }
}



/* g95_start_constructor()-- Start an array constructor.  The
 * constructor starts with zero elements and should be appended to by
 * g95_append_constructor(). */

g95_expr *g95_start_constructor(g95_typespec *ts, g95_locus *where) {
g95_expr *result;

    result = g95_get_expr();

    result->type = EXPR_ARRAY;
    result->rank = 1;

    result->ts = *ts;
    result->where = *where;

    return result;
}


/* element_string_length()-- Return the length of the string
 * expression, or -1 if this is not possible. */

static int element_string_length(g95_expr *e) {
g95_charlen *cl;

    if (e->ts.type != BT_CHARACTER)
	return -1;

    switch(e->type) {
    case EXPR_CONSTANT:
	return e->value.character.length;

    case EXPR_VARIABLE:
	if (e->ref != NULL)
	    return -1;

	cl = e->symbol->ts.cl;
	if (cl->length == NULL)
	    return -1;

	e = cl->length;
	if (e->type != EXPR_CONSTANT)
	    return -1;

	return bi_to_int(e->value.integer);

    default:
	break;
    }

    return -1;
}



/* check_element_type()-- Given an expression, compare its type with
 * the type of the current constructor.  Returns nonzero if an error
 * was issued.  The cons_state variable keeps track of whether the
 * type of the constructor being read or resolved is known to be good,
 * bad or just starting out. */

static int check_element_type(g95_expr *expr) {
int m, rc;

    rc = 0;

    switch(cons_state) { 
    case CONS_BAD:
	break;        /* Errors are suppressed */

    case CONS_START:
	if (expr->ts.type == BT_UNKNOWN)
	    cons_state = CONS_BAD;

	else {
	    cons_state = CONS_GOOD;
	    constructor_ts = expr->ts;
	}

	g95_constructor_string_length = element_string_length(expr);
	break;

    case CONS_GOOD:
	if (expr->ts.type == BT_CHARACTER) {
	    m = element_string_length(expr);

	    if (g95_constructor_string_length == -1 && m != -1) {
		g95_constructor_string_length = m;

		if (constructor_ts.cl == NULL) {
		    constructor_ts.cl = g95_get_charlen(NULL);
		    constructor_ts.cl->length = g95_int_expr(m);
		}

	    } else if (target_ts.type == BT_UNKNOWN &&
		       m != g95_constructor_string_length && m != -1) {
		g95_error("Element in character array constructor at %L has "
			  "length %d instead of %d", &expr->where, m,
			  g95_constructor_string_length);

		rc = 1;
		break;
	    }
	}

	if (target_ts.type != BT_UNKNOWN) {
	    rc = (g95_convert_type(expr, &target_ts, 0) == FAILURE);
	    break;
	}

	if (!g95_compare_types(&constructor_ts, &expr->ts)) {
	    g95_error("Element in %s array constructor at %L is %s",
		      g95_typename(&constructor_ts), &expr->where,
		      g95_typename(&expr->ts));
	    rc = 1;
	}

	break;
    }

    if (rc)
	cons_state = CONS_BAD;

    return rc;
}



/* check_constructor_type()-- Recursive work function for
 * g95_check_constructor_type(). */

static try check_constructor_type(g95_constructor *d) {
g95_constructor *c;
g95_expr *e;

    for(c=d; c; c=c->next) {
	e = c->expr;

	if (e->type == EXPR_ARRAY) {
	    if (check_constructor_type(e->value.constructor.c) == FAILURE)
		return FAILURE;

	    continue;
	}

	if (check_element_type(e))
	    return FAILURE;
    }

    return SUCCESS;
}



/* g95_check_constructor_type()-- Check that all elements of an array
 * constructor are the same type.  On FAILURE, an error has been
 * generated. */

try g95_check_constructor_type(g95_expr *e) {
try t;

    g95_constructor_string_length = -1;

    if (e->ts.type == BT_UNKNOWN) {
	g95_clear_ts(&constructor_ts);
	g95_clear_ts(&target_ts);
	cons_state = CONS_START;

    } else {
	target_ts = constructor_ts = e->ts;
	cons_state = CONS_GOOD;
    }

    t = check_constructor_type(e->value.constructor.c);
    if (t == SUCCESS && e->ts.type == BT_UNKNOWN)
	e->ts = constructor_ts;

    return t;
}



/* g95_check_iter_variable()-- Check an EXPR_VARIABLE expression in a
 * constructor to make sure that that variable is an iteration
 * variables. */

try g95_check_iter_variable(g95_expr *expr) {
iterator_stack *p;
g95_symbol *sym;
cons_stack *c;

    sym = expr->symbol;

    for(c=stack_base; c; c=c->previous)
	if (sym == c->iterator->var->symbol)
	    return SUCCESS;

    for(p=iter_stack; p; p=p->prev)
	if (sym == p->variable)
	    return SUCCESS;

    return FAILURE;
}



/* check_constructor()-- Recursive work function for
 * g95_check_constructor().  This amounts to calling the check
 * function for each expression in the constructor, giving variables
 * with the names of iterators a pass.  */

static try check_constructor(g95_constructor *c,
			     try (*check_function)(g95_expr *)) {
cons_stack element;
g95_expr *e;
try t;

    for(; c; c=c->next) {
	e = c->expr;

	if (e->type != EXPR_ARRAY) {
	    if ((*check_function)(e) == FAILURE)
		return FAILURE;

	    continue;
	}

	if (c->iterator == NULL)
	    t = check_constructor(e->value.constructor.c, check_function);

	else {
	    element.previous = stack_base;
	    element.iterator = c->iterator;
	    stack_base = &element;

	    t = check_constructor(e->value.constructor.c, check_function);
	    stack_base = element.previous;
	}

	if (t == FAILURE)
	    return FAILURE;
    }

/* Nothing went wrong, so all OK */

    return SUCCESS;
}



/* g95_check_constructor()-- Checks a constructor to see if it is a
 * particular kind of expression-- specification, restricted,
 * or initialization as determined by the check_function.  */

try g95_check_constructor(g95_expr *expr,
			  try (*check_function)(g95_expr *)) {

    return check_constructor(expr->value.constructor.c, check_function);
}



/* count_elements()-- Work function that counts the number of elements
 * present in a constructor. */

static try count_elements(g95_expr *e) {
bignum b;

    if (e->rank == 0)
	*current_expand.count = bi_int_add(*current_expand.count, 1);

    else {
	b = g95_array_size(e);
	if (b == NULL) {
	    g95_free_expr(e);
	    return FAILURE;
	}

	*current_expand.count = bi_add(*current_expand.count, b);
    }

    g95_free_expr(e);
    return SUCCESS;
}



/* extract_element()-- Work function that extracts a particular
 * element from an array constructor, freeing the rest. */

static try extract_element(g95_expr *e) {

    if (e->rank != 0) {  /* Something unextractable */
	g95_free_expr(e);
	return FAILURE;
    }

    if (bi_is_zero(big_copy(current_expand.extract_n)))
	current_expand.extracted = e;

    else
	g95_free_expr(e);

    current_expand.extract_n = bi_int_subtract(current_expand.extract_n, 1);

    return SUCCESS;
}



/* expand_ac_element()-- Work function for expanding a constructor */

static try expand_ac_element(g95_expr *e) {
g95_constructor *c;

    if (e->rank != 0) {
	g95_free_expr(e);
	return FAILURE;
    }

    c = g95_get_constructor();

    if (current_expand.head == NULL)
	current_expand.head = c;
    else
	current_expand.tail->next = c;

    current_expand.tail = c;
    g95_simplify_expr(e);
    c->expr = e;

    return SUCCESS;
}



#ifndef IN_GCC
try g95_expand_ac_element(g95_expr *e) {
    return SUCCESS;
}
#endif



/* expand_expr()-- Expand an expression with that is inside of a
 * constructor, recursing into other constructors if present. */

static try expand_expr(g95_expr *e) {

    if (e->type == EXPR_ARRAY)
	return expand_constructor(e->value.constructor.c);

    e = g95_copy_expr(e);

    if (g95_simplify_expr(e) == FAILURE) {
	g95_free_expr(e);
	return FAILURE;
    }

    return current_expand.expand_work_function(e);
}



/* g95_simplify_iteration_var()-- Given an initialization expression
 * that is a variable reference, substitute the current value of the
 * iteration variable. */

try g95_simplify_iterator_var(g95_expr *e) {
iterator_stack *p;
int kind;

    for(p=iter_stack; p; p=p->prev)
	if (e->symbol == p->variable)
	    break;

    if (p == NULL)
	return FAILURE;   /* Variable not found */

    kind = e->ts.kind;
    g95_replace_expr(e, g95_int_expr(0));

    big_depermanent(e->value.integer);
    big_free(e->value.integer);

    e->value.integer = big_clone(big_copy(p->value));
    big_permanent(e->value.integer);
    e->ts.kind = kind;

    return SUCCESS;
}



/* g95_expand_iterator()-- Given an iterator, a function to call and
 * something to expand, we effectively assign the correct values to
 * the loop variable and call the expansion function with the thing to
 * expand.  If the expansion function fails, we terminate the loop. */

try g95_expand_iterator(g95_iterator *iterator, try (*expand)(void *),
			void *x) {
g95_expr *start, *end, *step;
iterator_stack frame;
bignum trip;
try t;

    start = end = step = NULL;
    trip = frame.value = bi_0;
    frame.prev = iter_stack;

    t = FAILURE;

    start = g95_copy_expr(iterator->start);
    if (g95_simplify_expr(start) == FAILURE)
	goto cleanup;

    if (start->type != EXPR_CONSTANT || start->ts.type != BT_INTEGER)
	goto cleanup;

    end = g95_copy_expr(iterator->end);
    if (g95_simplify_expr(end) == FAILURE)
	goto cleanup;

    if (end->type != EXPR_CONSTANT || end->ts.type != BT_INTEGER)
	goto cleanup;

    step = g95_copy_expr(iterator->step);
    if (g95_simplify_expr(step) == FAILURE)
	goto cleanup;

    if (step->type != EXPR_CONSTANT || step->ts.type != BT_INTEGER)
	goto cleanup;

    if (bi_is_zero(step->value.integer)) {
	g95_error("Iterator step at %L cannot be zero", &step->where);
	goto cleanup;
    }

    /* Calculate the trip count of the loop */

    trip = bi_subtract(end->value.integer, start->value.integer);
    trip = bi_add(trip, step->value.integer);
    trip = bi_divide(trip, step->value.integer);

    frame.value = start->value.integer;

    frame.variable = iterator->var->symbol;
    iter_stack = &frame;

    while(bi_compare(big_copy(trip), bi_0) > 0) {
	if (expand(x) == FAILURE)
	    goto cleanup;

	frame.value = bi_add(frame.value, step->value.integer);
	trip = bi_int_add(trip, -1);
    }

    t = SUCCESS;

cleanup:
    big_free(trip);
    big_free(frame.value);

    g95_free_expr(start);
    g95_free_expr(end);
    g95_free_expr(step);

    iter_stack = frame.prev;
    return t;
}



/* expand_constructor()-- Expand a constructor into constant
 * constructors without any iterators, calling the work function for
 * each of the expanded expressions.  The work function needs to
 * either save or free the passed expression. */

static try expand_constructor(g95_constructor *c) {
g95_expr *e;

    for(; c; c=c->next) {
	if (c->iterator != NULL) {
	    if (g95_expand_iterator(c->iterator, (void *) expand_expr,
				    c->expr) == FAILURE)
		return FAILURE;

	    continue;
	}

	e = g95_copy_expr(c->expr);

	if (g95_simplify_expr(e) == FAILURE) {
	    g95_free_expr(e);
	    return FAILURE;
	}

	if (e->type == EXPR_ARRAY) {
	    if (expand_constructor(e->value.constructor.c) == FAILURE)
		return FAILURE;

	    continue;
	}

	if (current_expand.expand_work_function(e) == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



/* g95_expand_constructor()-- Top level subroutine for expanding a
 * constructor into a series of elements. */

try g95_expand_constructor(g95_expr *e) {
expand_info expand_save;
try rc;

    expand_save = current_expand;

    current_expand.expand_work_function = expand_ac_element;
    current_expand.head = NULL;
    current_expand.tail = NULL;

    rc = expand_constructor(e->value.constructor.c);

    if (rc != SUCCESS)
	g95_free_constructor(current_expand.head);

    else {
	g95_free_constructor(e->value.constructor.c);
	e->value.constructor.c = current_expand.head;
    }

    current_expand = expand_save;
    return rc;
}



/* g95_expand_data_constructor()-- Top level subroutine for expanding
 * constructors in a DATA statement. */

try g95_expand_data_constructor(g95_expr *e) {
expand_info expand_save;
try rc;

    expand_save = current_expand;

    current_expand.expand_work_function = g95_expand_ac_element;
    rc = expand_constructor(e->value.constructor.c);
    current_expand = expand_save;

    return rc;
}



/* check_duplicate_iterator()-- Given an expression node that might be an 
 * array constructor and a symbol, make sure that no iterators in this or
 * child constructors use the symbol as an implied-DO iterator. 
 * Returns nonzero if a duplicate was found. */

static int check_duplicate_iterator(g95_constructor *c, g95_symbol *master) {
g95_expr *e;

    for(; c; c=c->next) {
	e = c->expr;

	if (e->type == EXPR_ARRAY &&
	    check_duplicate_iterator(e->value.constructor.c, master))
	    return 1;

	if (c->iterator == NULL)
	    continue;

	if (c->iterator->var->symbol == master) {
	    g95_error("DO-iterator '%s' at %L is inside iterator of the same "
		      "name", master->name, &c->where);

	    return 1;
	}
    }

    return 0;
}



/* match_array_list()-- Match a list of array elements. */

static match match_array_list(g95_constructor **result) {
g95_constructor *p, *head, *tail, *new;
g95_iterator iter;
g95_locus old_loc;
g95_symbol *sym;
g95_expr *e;
match m;
int n;

    old_loc = g95_current_locus;

    if (g95_match_char('(') == MATCH_NO)
	return MATCH_NO;

    memset(&iter, '\0', sizeof(g95_iterator));
    head = NULL;

    m = match_array_cons_element(&head);
    if (m != MATCH_YES)
	goto cleanup;

    tail = head;

    if (g95_match_char(',') != MATCH_YES) {
	m = MATCH_NO;
	goto cleanup;
    }

    for(n=1;; n++) {
	m = g95_match_iterator(&iter);
	if (m == MATCH_YES) break;
	if (m == MATCH_ERROR) goto cleanup;

	m = match_array_cons_element(&new);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) {
	    if (n > 2) goto syntax;
	    m = MATCH_NO;
	    goto cleanup;    /* Could be a complex constant */
	}

	tail->next = new;
	tail = new;

	if (g95_match_char(',') != MATCH_YES) {
	    if (n > 2)
		goto syntax;

	    m = MATCH_NO;
	    goto cleanup;
	}
    }

    if (g95_match_char(')') != MATCH_YES)
	goto syntax;

    if (check_duplicate_iterator(head, iter.var->symbol)) {
	m = MATCH_ERROR;
	goto cleanup;
    }

    /* Peek at type of the iterator variable */

    sym = iter.var->symbol;

    if (!g95_iterator_variable(iter.var->symbol)) {
	g95_error("Iterator variable '%s' at %L must be of type INTEGER",
		  sym->name, &iter.var->where);
	m = MATCH_ERROR;
	goto cleanup;    
    }

    if (sym->attr.flavor != FL_VARIABLE && sym->attr.flavor != FL_UNKNOWN) {
	g95_error("Iterator variable '%s' at %L must be a VARIABLE",
		  sym->name, &iter.var->where);
	m = MATCH_ERROR;
	goto cleanup;
    }

    e = g95_get_expr();
    e->type = EXPR_ARRAY;
    e->rank = 1;
    e->where = old_loc;
    e->value.constructor.c = head;

    p = g95_get_constructor();
    p->where = g95_current_locus;
    p->iterator = g95_get_iterator();
    *p->iterator = iter;
    p->iterator->var->symbol->attr.st_construct = 1;

    p->expr = e;
    *result = p;

    return MATCH_YES;

syntax:
    g95_error("Syntax error in array constructor at %C");
    m = MATCH_ERROR;

cleanup:
    g95_free_constructor(head);
    g95_free_iterator(&iter, 0);
    g95_current_locus = old_loc;
    return m;
}



/* match_array_cons_element()-- match a single element of an array
 * constructor, which can be a single expression or a list of
 * elements. */

static match match_array_cons_element(g95_constructor **result) {
g95_constructor *p;
g95_expr *expr;
match m;

    m = match_array_list(result);
    if (m != MATCH_NO)
	return m;

    m = g95_match_expr(&expr);
    if (m != MATCH_YES)
	return m;

    p = g95_get_constructor();
    p->where = g95_current_locus;
    p->expr = expr;

    *result = p;
    return MATCH_YES;  
}



/* g95_match_array_constructor()-- Match an array constructor */

match g95_match_array_constructor(g95_expr **result) {
g95_constructor *head, *tail, *new;
g95_locus where;
g95_typespec ts;
g95_expr *expr;
int flag;
match m;

    flag = 0;
    if (g95_match(" (/") == MATCH_NO) {
	if (g95_match_char('[') == MATCH_NO)
	    return MATCH_NO;

	flag = 1;
    }

    where = g95_current_locus;
    head = tail = NULL;
    g95_clear_ts(&ts);

    if (G95_STRICT_F95())
	m = MATCH_NO;

    else {
	m = g95_match_type_spec(&ts, 1);
	if (m == MATCH_ERROR)
	    m = MATCH_NO;

	else if (m == MATCH_YES && ts.type == BT_CHARACTER &&
		 ts.cl->length == NULL) {
	    g95_error("Assumed-length CHARACTER not allowed at %C");
	    goto cleanup;
	}
    }

    if (m == MATCH_NO || g95_match(" ::") != MATCH_YES) {
	g95_current_locus = where;
	g95_clear_ts(&ts);
    }

    if ((flag && g95_match_char(']') == MATCH_YES) ||
	(!flag && g95_match(" /)") == MATCH_YES)) {

	if (ts.type != BT_UNKNOWN)
	    goto done;

	g95_error("Empty array constructor at %C is not allowed");
	goto cleanup;
    }

    for(;;) {
	m = match_array_cons_element(&new);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	if (head == NULL)
	    head = new;
	else
	    tail->next = new;

	tail = new;

	if (g95_match_char(',') == MATCH_NO)
	    break;
    }

    if ((flag && g95_match_char(']') == MATCH_NO) ||
	(!flag && g95_match(" /)") == MATCH_NO))
	goto syntax;  /* Special case */

    if (flag && G95_STRICT_F95())
	g95_warning(114, "Square brackets for array constructor at %C is "
		    "nonstandard");

done:
    expr = g95_get_expr();

    expr->type = EXPR_ARRAY;
    expr->value.constructor.c = head;

    if (ts.type != BT_UNKNOWN)
	expr->ts = ts;

    /* Size must be calculated at resolution time */

    g95_fixup_ac(expr);

    expr->where = where;
    expr->rank = 1;

    *result = expr;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in array constructor at %C");

cleanup:
    g95_free_constructor(head);
    return MATCH_ERROR;
}



/* g95_constant_constructor()-- Given an array constructor, determine
 * if the constructor is constant or by making sure that all elements
 * are constants or iterator variables. */

int g95_constant_constructor(g95_expr *e) {
g95_constructor *c;
iterator_stack s;
try rc;

    rc = 1;

    for(c=e->value.constructor.c; c; c=c->next) {
	if (c->iterator != NULL) {
	    s.prev = iter_stack;
	    s.value = NULL;
	    s.variable = c->iterator->var->symbol;

	    iter_stack = &s;
	}

	rc = g95_is_constant_expr(c->expr);

	if (c->iterator != NULL)
	    iter_stack = s.prev;

	if (rc == 0)
	    break;
    }

    return rc;
}



/* g95_expanded_ac()-- Returns nonzero if an array constructor has
 * been completely expanded (no iterators) and zero if iterators are
 * present. */

int g95_expanded_ac(g95_expr *e) {
g95_constructor *p;

    if (e->type == EXPR_ARRAY)
	for(p=e->value.constructor.c; p; p=p->next)
	    if (p->iterator != NULL || !g95_expanded_ac(p->expr))
		return 0;

    return 1;
}



/* g95_resolve_array_constructor()-- Resolve all of the expressions in
 * an array list. */

try g95_resolve_array_constructor(g95_expr *expr) {
g95_constructor *p;
try t;

    t = SUCCESS;

    for(p=expr->value.constructor.c; p; p=p->next) {
	if (p->iterator != NULL &&
	    g95_resolve_iterator(p->iterator) == FAILURE)
	    t = FAILURE;

	if (g95_resolve_expr(p->expr) == FAILURE)
	    t = FAILURE;
    }

    if (t == SUCCESS)
	t = g95_check_constructor_type(expr);

    return t;
}



/* copy_iterator()-- Copy an iterator structure */

static g95_iterator *copy_iterator(g95_iterator *src) {
g95_iterator *dest;

    if (src == NULL)
	return NULL;

    dest = g95_get_iterator();
  
    dest->var   = g95_copy_expr(src->var);
    dest->start = g95_copy_expr(src->start);
    dest->end   = g95_copy_expr(src->end);
    dest->step  = g95_copy_expr(src->step);

    return dest;
}



/* g95_copy_constructor()-- Copy a constructor structure. */

g95_constructor *g95_copy_constructor(g95_constructor *src) {
g95_constructor *dest;

    if (src == NULL)
	return NULL;

    dest = g95_get_constructor();

    dest->where    = src->where;
    dest->expr     = g95_copy_expr(src->expr);
    dest->iterator = copy_iterator(src->iterator);
    dest->next     = g95_copy_constructor(src->next);

    return dest;
}



/* get_fast_element()-- Fast version of g95_get_array_element().  This
 * only works on arrays that have been completely expanded.  Returns
 * NULL if this is not so, or no element is found. */

static g95_expr *get_fast_element(g95_expr *array, bignum element) {
g95_constructor *c;

    c = array->value.constructor.c; 

    while(bi_compare(big_copy(element), bi_0) > 0) {
	element = bi_subtract(element, bi_1);

	if (c == NULL || c->iterator != NULL || c->expr->type == EXPR_ARRAY) {
	    big_free(element);
	    return NULL;
	}

	c = c->next;
    }

    big_free(element);

    if (c == NULL || c->iterator != NULL || c->expr->type == EXPR_ARRAY)
	return NULL;

    return g95_copy_expr(c->expr);
}



/* g95_get_array_element()-- Given an array expression and an element
 * number (starting at zero), return a pointer to the array element.
 * NULL is returned if the size of the array has been exceeded.  The
 * expression node returned remains a part of the array and should not
 * be freed.  Access is not efficient at all, but this is another
 * place where things do not have to be particularly fast. */

g95_expr *g95_get_array_element(g95_expr *array, bignum element) {
expand_info expand_save;
g95_expr *e;
try rc;

    switch(array->type) {
    case EXPR_ARRAY:
	break;

    case EXPR_VARIABLE:
	if (array->symbol->attr.flavor != FL_PARAMETER)
	    return NULL;

	array = array->symbol->value;
	if (array->type != EXPR_ARRAY)
	    return NULL;

	break;

    default:
	return NULL;
    }

    e = get_fast_element(array, big_copy(element));
    if (e != NULL)
	big_free(element);

    else {
	expand_save = current_expand; 
	current_expand.extract_n = element;
	current_expand.expand_work_function = extract_element;
	current_expand.extracted = NULL;

	rc = expand_constructor(array->value.constructor.c);
	e = current_expand.extracted;
	big_free(current_expand.extract_n);

	current_expand = expand_save; 

	if (rc == FAILURE) {
	    g95_free_expr(e);
	    e = NULL;
	}
    }

    return e;
}



/* g95_spec_dimen_size()-- Get the size of single dimension of an array
 * specification.  */

bignum g95_spec_dimen_size(g95_array_spec *as, int dimen) {
bignum b;

    if (as == NULL || as->type != AS_EXPLICIT ||
	as->lower[dimen]->type != EXPR_CONSTANT ||
	as->upper[dimen]->type != EXPR_CONSTANT) {

	return NULL;
    }

    b = bi_subtract(as->upper[dimen]->value.integer,
		    as->lower[dimen]->value.integer);
    b = bi_add(b, bi_1);

    if (bi_is_negative(big_copy(b))) {
	big_free(b);
	b = bi_0;
    }

    return b;
}



/* g95_array_spec_size()-- Given an array specification, figure out
 * how big it is. */

bignum g95_array_spec_size(g95_array_spec *as) {
bignum size, b;
int d;

    size = bi_1;

    for(d=0; d<as->rank; d++) {
	b = g95_spec_dimen_size(as, d);
	if (b == NULL) {
	    big_free(size);
	    return NULL;
	}

	size = bi_multiply(size, b);
    }

    return size;
}



/* ref_dimen_size()-- Get the number of elements in an array section */

static bignum ref_dimen_size(g95_array_ref *ar, g95_array_spec *as,
			     int dimen) {
bignum upper, lower, stride, size;

    switch(ar->dimen_type[dimen]) {
    case DIMEN_ELEMENT:
	size = bi_1;
	break;

    case DIMEN_VECTOR:
	size = g95_array_size(ar->start[dimen]);    /* Recurse! */
	break;

    case DIMEN_RANGE:
	lower  = bi_0;
	upper  = bi_0;
	stride = bi_0;

	if (ar->start[dimen] == NULL) {
	    if (as->lower[dimen] == NULL ||
		as->lower[dimen]->type != EXPR_CONSTANT)
		goto cleanup;

	    lower = as->lower[dimen]->value.integer;

	} else {
	    if (ar->start[dimen]->type != EXPR_CONSTANT)
		goto cleanup;

	    lower = ar->start[dimen]->value.integer;
	}

	if (ar->end[dimen] == NULL) {
	    if (as->upper[dimen] == NULL ||
		as->upper[dimen]->type != EXPR_CONSTANT)
		goto cleanup;

	    upper = as->upper[dimen]->value.integer;
	} else {
	    if (ar->end[dimen]->type != EXPR_CONSTANT)
		goto cleanup;

	    upper = ar->end[dimen]->value.integer;
	}

	if (ar->stride[dimen] == NULL)
	    stride = bi_1;

	else {
	    if (ar->stride[dimen]->type != EXPR_CONSTANT)
		goto cleanup;

	    stride = ar->stride[dimen]->value.integer;
	}

	size = bi_add(bi_subtract(upper, lower), big_copy(stride));
	size = bi_divide(size, stride);    /* Zero stride caught earlier */

	if (bi_is_negative(big_copy(size))) {
	    big_free(size);
	    size = bi_0;
	}

	break;

    cleanup:
	size = NULL;

	big_free(upper);
	big_free(lower);
	big_free(stride);
	break;

    default:
	g95_internal_error("ref_dimen_size(): Bad dimen type");
    }

    return size;
}



static bignum ref_size(g95_array_ref *ar, g95_array_spec *as) {
bignum b, size;
int d;

    size = bi_1;

    for(d=0; d<ar->dimen; d++) {
	b = ref_dimen_size(ar, as, d);

	if (b == NULL) {
	    big_free(size);
	    size = NULL;
	    break;
	}

	size = bi_multiply(size, b);
    }

    return size;
}



/* elemental_array_function()-- Get the array size from the actual
 * arguments of an elemental function. */

static bignum elemental_array_function(g95_actual_arglist *b) {
g95_actual_arglist *a;
bignum size;

    for(a=b; a; a=a->next) {
	if (a->u.expr == NULL || a->u.expr->rank == 0)
	    continue;

	size = g95_array_size(a->u.expr);
	if (size != NULL)
	    return size;
    }

    return NULL;
}



/* array_function_size()-- Deduce the size of an array-value intrinsic
 * for a couple common inquiry functions. */

static bignum array_function_size(g95_expr *e) {
g95_intrinsic_sym *isym;

    isym = e->value.function.isym;

    if ((isym != NULL) ? isym->elemental : e->symbol->attr.elemental)
	return elemental_array_function(e->value.function.actual);

    isym = e->value.function.isym;
    if (isym == NULL)
	return g95_array_spec_size(e->symbol->as);

    switch(isym->id) {
    case G95_ISYM_MINLOC:
    case G95_ISYM_MAXLOC:
    case G95_ISYM_SHAPE:
    case G95_ISYM_LBOUND:
    case G95_ISYM_UBOUND:
	/* Special cases where the size of the array is equal to the
	 * rank of the first argument.  The second argument (DIM) must
	 * not be present. */

	if (e->value.function.actual->next != NULL)
	    break;

	return int_to_bi(e->value.function.actual->u.expr->rank);

    default:
	break;
    }

    return NULL;
}



/* g95_array_size()-- Given an array expression, figure out how many
 * elements are in the array.  Returns NULL if it wasn't possible to
 * determine the size of the array. */

bignum g95_array_size(g95_expr *array) {
expand_info expand_save;
g95_array_spec *as;
g95_array_ref *ar;
int i, flag;
bignum b;
try t;

    switch(array->type) {
    case EXPR_ARRAY:
	flag = g95_suppress_error;
	g95_suppress_error = 1;

	expand_save = current_expand;

	current_expand.count = &b;
	b = bi_0;

	current_expand.expand_work_function = count_elements;

	t = expand_constructor(array->value.constructor.c);
	g95_suppress_error = flag;

	if (t == FAILURE) {
	    big_free(b);
	    b = NULL;
	}

	current_expand = expand_save;
	break;

    case EXPR_VARIABLE:
	g95_find_array_ref(array, &ar, &as, 1);

	switch(ar->type) {
	case AR_FULL:
	    b = g95_array_spec_size(as);
	    break;

	case AR_SECTION:
	    b = ref_size(ar, as);
	    break;

	default:
	    g95_internal_error("g95_array_size(): Bad ref");
	}

	break;

    case EXPR_FUNCTION:
	if (array->value.function.pointer != NULL) {
	    as = array->value.function.pointer->ts.interface->as;
	    b = g95_array_spec_size(as);
	    break;
	}

	b = array_function_size(array);
	if (b != NULL)
	    break;

	/* Fall through */

    default:
	b = NULL;
	if (array->rank == 0 || array->shape == NULL)
	    break;

	b = bi_1;
	for(i=0; i<array->rank; i++)
	    if (array->shape[i] != NULL)
		b = bi_multiply(b, array->shape[i]);

	    else {
		if (b != NULL) {
		    big_free(b);

		    b = NULL;
		}

		break;
	    }

	break;
    }

    return b;
}



/* g95_zero_size_ac()-- Return nonzero if the expression is a zero
 * size array constructor. */

int g95_zero_size_ac(g95_expr *e) {

    return (e->type != EXPR_ARRAY)
	? 0
	: (e->value.constructor.c == NULL);
}



/* g95_zero_size_array()-- Return nonzero if the array has zero size */

int g95_zero_size_array(g95_expr *array) {
g95_array_spec *as;
g95_ref *ref;
int i;

    if (array->type == EXPR_ARRAY)
	return g95_zero_size_ac(array);

    if (array->type != EXPR_VARIABLE)
	return 0;

    as = array->symbol->as;
    if (as == NULL)
	return 0;

    for(ref=array->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type == AR_FULL)
		goto full;

	    if (ref->u.ar.type == AR_SECTION)
		return 0;

	    break;

	case REF_COMPONENT:
	    if (ref->u.c.component->as != NULL)
		as = ref->u.c.component->as;

	    break;

	case REF_COARRAY:
	case REF_SUBSTRING:
	    break;
	}

 full:
    for(i=0; i<as->rank; i++)
	if (as->lower[i] != NULL && as->lower[i]->type == EXPR_CONSTANT &&
	    as->upper[i] != NULL && as->upper[i]->type == EXPR_CONSTANT &&
	    bi_compare(as->lower[i]->value.integer,
		       as->upper[i]->value.integer) > 0)
	    return 1;

    return 0;
}



/* g95_array_ref_shape()-- Given an array reference, return the shape
 * of the reference in an array of integers. */

try g95_array_ref_shape(g95_array_ref *ar, g95_array_spec *as, bignum *shape,
			int *rank_p) {
int d, rank;

    d = 0;
    rank = 0;

    if (as->type != AS_EXPLICIT)
	return FAILURE;

    switch(ar->type) {
    case AR_SECTION:
	for(; d<ar->dimen; d++) {
	    if (ar->dimen_type[d] != DIMEN_RANGE &&
		ar->dimen_type[d] != DIMEN_VECTOR)
		continue;

	    shape[rank] = ref_dimen_size(ar, as, d);
	    if (shape[rank] == NULL)
		goto cleanup;

	    rank++;
	}

	*rank_p = rank;
	return SUCCESS;

    case AR_FULL:
	for(; d<as->rank; d++) {
	    shape[rank] = g95_spec_dimen_size(as, d);
	    if (shape[rank] == NULL)
		goto cleanup;

	    rank++;
	}

	*rank_p = rank;
	return SUCCESS;

    default:
	break;
    }

cleanup:
    for(rank--; rank>=0; rank--)
	big_free(shape[rank]);

    return FAILURE;
}



/* g95_array_dimen_size()-- Given an array expression and a dimension,
 * figure out how many elements it has along that dimension.  Returns
 * a null pointer on failure. */

bignum g95_array_dimen_size(g95_expr *array, int dimen) {
g95_array_spec *as;
g95_array_ref *ar;
bignum b;
int i;

    if (dimen > array->rank - 1)
	g95_internal_error("g95_array_dimen_size(): Bad dimension");

    b = NULL;

    switch(array->type) {
    case EXPR_VARIABLE:
	if (array->symbol == NULL)
	    return NULL;

	g95_find_array_ref(array, &ar, &as, 1);

	switch(ar->type) {
	case AR_SECTION:
	    for(i=0; dimen>=0; i++)
		if (ar->dimen_type[i] != DIMEN_ELEMENT)
		    dimen--;

	    b = ref_dimen_size(ar, as, i-1);
	    break;

	case AR_FULL:
	    b = g95_spec_dimen_size(as, dimen);
	    break;

	default:
	    g95_internal_error("g95_array_dimen_size(): Bad array ref");
	}

	break;

    case EXPR_FUNCTION:
	if (array->symbol != NULL && array->value.function.isym == NULL)
	    b = g95_spec_dimen_size(array->symbol->result->as, dimen);

	break;

    case EXPR_ARRAY:
	if (array->shape != NULL)
	    b = big_copy(array->shape[dimen]);

	else if (array->rank == 1 && dimen == 0)
	    b = g95_array_size(array);

	else
	    b = NULL;

	break;

    default:
	break;
    }

    if (b == NULL && array->shape != NULL && array->shape[dimen] != NULL)
	b = big_clone(array->shape[dimen]);

    return b;
}



/* g95_find_array_ref()-- Given an array expression, find the array
 * reference and/or specification that characterizes the reference. */

void g95_find_array_ref(g95_expr *e, g95_array_ref **ar_p,
			g95_array_spec **as_p, int flag) {
g95_array_spec *as;
g95_ref *ref;

    if (ar_p != NULL)
	*ar_p = NULL;

    if (as_p != NULL)
	*as_p = NULL;

    as = e->symbol->as;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type == AR_FULL ||
		ref->u.ar.type == AR_SECTION)
		goto done;

	    as = NULL;
	    break;

	case REF_COMPONENT:
	    as = ref->u.c.component->as;
	    break;

	case REF_COARRAY:
	    break;

	case REF_SUBSTRING:
	    goto error;      
	}

error:
    if (flag)
	g95_internal_error("g95_find_array_ref(): No ref found");

    return;

done:
    if (ar_p != NULL)
	*ar_p = &ref->u.ar;

    if (as_p != NULL)
	*as_p = as;
}


/* g95_check_conformance()-- Given two expressions, make sure that
 * the arrays are conformable. */

try g95_check_conformance(const char *optype, g95_expr *op1, g95_expr *op2) {
bignum o1, o2;
int d;
try t;

    if (op1->rank == 0 || op2->rank == 0)
	return SUCCESS;

    if (op1->rank != op2->rank) {
	g95_error("Incompatible ranks in %s at %L", optype, &op1->where);
	return FAILURE;
    }

    t = SUCCESS;

    for(d=0; d<op1->rank; d++) {
	o1 = g95_array_dimen_size(op1, d);
	o2 = g95_array_dimen_size(op2, d);

	if (o1 != NULL && o2 != NULL &&
	    bi_compare(big_copy(o1), big_copy(o2)) != 0) {
	    g95_error("%s at %L has different shape on dimension %d (%s/%s)",
		      optype, &op2->where, d+1,
		      bi_to_string(big_copy(o1)), bi_to_string(big_copy(o2)));

	    t = FAILURE;
	}

	if (o1 != NULL) big_free(o1);
	if (o2 != NULL) big_free(o2);

	if (t == FAILURE)
	    return FAILURE;
    }

    o1 = g95_array_size(op1);
    o2 = g95_array_size(op2);

    if (o1 != NULL && o2 != NULL &&
	bi_compare(big_copy(o1), big_copy(o2)) != 0) {
	g95_error("%s at %L has different sizes (%s/%s)", optype, &op2->where,
		  bi_to_string(big_copy(o1)), bi_to_string(big_copy(o2)));
	t = FAILURE;
    }

    if (o1 != NULL) big_free(o1);
    if (o2 != NULL) big_free(o2);

    return t;
}

