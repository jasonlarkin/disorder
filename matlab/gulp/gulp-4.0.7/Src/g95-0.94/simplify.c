/* Simplify intrinsic functions at compile-time
   Copyright (C) 2000-2008 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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

#include "g95.h"
#include "intrinsic.h"

#include <string.h>


g95_expr g95_bad_expr;


/* Note that 'simplification' is not just transforming expressions.
 * For functions that are not simplified at compile time,
 * range checking is done if possible.
 *
 * The return convention is that each simplification function returns:
 *
 *   A new expression node corresponding to the simplified arguments.
 *   The original arguments are destroyed by the caller, and must not
 *   be a part of the new expression.
 *
 *   NULL pointer indicating that no simplification was possible and
 *   the original expression should remain intact.  If the
 *   simplification function sets the type and/or the function name
 *   via the pointer g95_simple_expression, then this type is
 *   retained.
 *
 *   An expression pointer to g95_bad_expr (a static placeholder)
 *   indicating that some error has prevented simplification.  For
 *   example, sqrt(-1.0).  The error is generated within the function
 *   and should be propagated upwards
 *
 * By the time a simplification function gets control, it has been
 * decided that the function call is really supposed to be the
 * intrinsic.  No type checking is strictly necessary, since only
 * valid types will be passed on.  On the other hand, a simplification
 * subroutine may have to look at the type of an argument as part of
 * its processing.
 *
 * Array arguments are never passed to these subroutines.  */

/* Static table for converting non-ascii character sets to ascii.
 * The xascii_table[] is the inverse table. */

static int ascii_table[256] = {
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
    ' ',  '!',  '"',  '#',  '$',  '%',  '&',  '\'',
    '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
    '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
    '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
    '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
    'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
    'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
    'X',  'Y',  'Z',  '[', '\\',  ']',  '^',  '_',
    '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
    'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
    'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
    'x',  'y',  'z',  '{',  '|',  '}',  '~',  '\?'
};

static int xascii_table[256];



/* range_check()-- Range checks an expression node.  If all goes well,
 * returns the node, otherwise returns &g95_bad_expr and frees the node. */

static g95_expr *range_check(g95_expr *result, char *name) {

    if (!g95_range_check(result))
	return result;

    g95_error_now("Result of %s overflows its kind at %L", name,
		  &result->where);
    g95_free_expr(result);

    return &g95_bad_expr;
}



/* get_kind()-- A helper function that gets an optional and possibly
 * missing kind parameter.  Returns the kind, -1 if something went
 * wrong. */

static int get_kind(bt type, g95_expr *k, char *name, int default_kind) {
int kind;

    if (k == NULL)
	return default_kind;

    if (k->type != EXPR_CONSTANT) {
	g95_error("KIND parameter of %s at %L must be an initialization "
		  "expression", name, &k->where);

	return -1;
    }

    if (g95_extract_int(k, &kind) != NULL ||
	g95_validate_kind(type, kind) < 0) {

	g95_error("Invalid KIND parameter of %s at %L", name, &k->where);
	return -1;
    }

    return kind;
}



/* string_compare()-- Compare a constant string expression with a C
 * string.  The comparison is case insensitive.  Returns nonzero if
 * equal. */

static int string_compare(g95_expr *e, char *p) {
int p_len, q_len, m;
char *q;

    q = e->value.character.string;

    q_len = e->value.character.length;
    p_len = strlen(p);
  
    if (p_len > q_len)
	return 0;

    if (strncasecmp(p, q, p_len) != 0)
	return 0;

    for(m=p_len; m<q_len; m++)
	if (q[m] != ' ')
	    return 0;

    return 1;
}



/* simplify_bound0()-- Simplify one dimension of a LBOUND/UBOUND. */

static g95_expr *simplify_bound0(g95_expr *array, int dim, int upper) {
bignum start, end, stride, m;
g95_array_spec *as;
g95_array_ref *ar;
g95_ref *ref;
g95_expr *v;
int i, d;

    as = array->symbol->as;

    for(ref=array->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    switch(ref->u.ar.type) {
	    case AR_ELEMENT:
		as = NULL;
		break;

	    case AR_FULL:
		goto full;

	    case AR_UNKNOWN:
		return NULL;

	    case AR_SECTION:
		goto section;
	    }

	    break;

	case REF_COMPONENT:
	    as = ref->u.c.component->as;
	    break;

	case REF_COARRAY:
	case REF_SUBSTRING:
	    break;
	}

    g95_error("Cannot determine bound of scalar expression at %L",
	      &array->where);
    return &g95_bad_expr;

full:
    if (as->type == AS_DEFERRED || as->type == AS_ASSUMED_SHAPE)
	return NULL;

    if (dim < 1 || dim > as->rank)
	goto bad_dim;

    if (upper && as->type == AS_ASSUMED_SIZE && dim == as->rank)
	goto bad_ubound;

    v = upper ? as->upper[dim-1] : as->lower[dim-1];
    if (v->type != EXPR_CONSTANT)
	return NULL;

    return g95_copy_expr(v);

section:
    ar = &ref->u.ar;
    d = dim;

    for(i=0; i<ar->dimen; i++) {
	if (ar->dimen_type[i] == DIMEN_ELEMENT)
	    continue;

	if (d == 1)
	    break;

	d--;
    }

    if (i >= as->rank)
	goto bad_dim;

    if (!upper)
	return g95_int_expr(1);

    if (as->type == AS_ASSUMED_SHAPE || as->type == AS_DEFERRED)
	return NULL;

    start = end = stride = NULL;

    if (ar->start[i] == NULL) {
	if (as->lower[i]->type != EXPR_CONSTANT)
	    return NULL;

	start = as->lower[i]->value.integer;

    } else {
	if (ar->start[i]->type != EXPR_CONSTANT)
	    return NULL;

	start = ar->start[i]->value.integer;
    }

    if (ar->end[i] == NULL) {
	if (as->upper[i]->type != EXPR_CONSTANT)
	    return NULL;

	end = as->upper[i]->value.integer;

    } else {
	if (ar->end[i]->type != EXPR_CONSTANT)
	    return NULL;

	end = ar->end[i]->value.integer;
    }

    if (ar->stride[i] == NULL)
	stride = bi_1;

    else if (ar->stride[i]->type != EXPR_CONSTANT)
	return NULL;

    else
	stride = ar->stride[i]->value.integer;

    m = bi_divide(bi_add(bi_subtract(end, start), stride), stride);

    if (bi_compare(big_copy(m), bi_0) < 0) {
	big_free(m);
	m = bi_0;
    }

    /* start, step and stride are all permanents and don't have to be freed */

    v = g95_constant_result(BT_INTEGER, -1, &array->where);

    m = big_clone(m);

    big_permanent(m);
    v->value.integer = m;

    return v;

bad_dim:
    g95_error("Bad DIM argument of %s at %L", upper ? "UBOUND" : "LBOUND",
	      &array->where);
    return &g95_bad_expr;

bad_ubound:
    g95_error("Can't determine UBOUND for the final dimension of "
	      "assumed-size array at %L", &array->where);
    return &g95_bad_expr;
}



/* simplify_bound()-- Simplify an LBOUND or UBOUND expression. */

static g95_expr *simplify_bound(g95_expr *array, g95_expr *dim, g95_expr *kind,
				int upper) {
g95_expr *result, *e;
g95_typespec ts;
int i;

    if (array->type != EXPR_VARIABLE)
	return NULL;

    if (g95_zero_size_array(array))
	return g95_int_expr(upper ? 0 : 1);

    if (dim != NULL)
	return (dim->type == EXPR_CONSTANT)
	    ? simplify_bound0(array, bi_to_int(dim->value.integer), upper)
	    : NULL;

    g95_clear_ts(&ts);
    ts.type = BT_INTEGER;

    if (kind == NULL)
	ts.kind = g95_default_integer_kind(0);

    else
	g95_extract_int(kind, &ts.kind);

    result = g95_start_constructor(&ts, &array->where);

    for(i=1; i<=array->rank; i++) {
	e = simplify_bound0(array, i, upper);
	if (e == NULL || e == &g95_bad_expr) {
	    g95_free_expr(result);
	    return e;
	}

	g95_append_constructor(result, e);
    }

    return result;
}



g95_expr *g95_simplify_abs(g95_expr *e) {
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    switch(e->ts.type) {
    case BT_INTEGER:
	result = g95_constant_result(BT_INTEGER, e->ts.kind, &e->where);
	result->value.integer = big_clone(bi_abs(e->value.integer));
	big_permanent(result->value.integer);

	result = range_check(result, "IABS");
	break;

    case BT_REAL:
	g95_arithmetic_locus = &e->where;
	result = g95_constant_result(BT_REAL, e->ts.kind, &e->where);
	result->value.real = big_clone(bg_abs(e->value.real));
	big_permanent(result->value.real);
	g95_arithmetic_locus = NULL;
	break;

    case BT_COMPLEX:
	g95_arithmetic_locus = &e->where;
	result = g95_constant_result(BT_REAL, e->ts.kind, &e->where);

	result->value.real =
	    big_clone(bg_hypot(e->value.complex.r, e->value.complex.i));

	big_permanent(result->value.real);

	g95_arithmetic_locus = NULL;
	break;

    default:
	g95_internal_error("g95_simplify_abs(): Bad type");
    }

    return result;
}



g95_expr *g95_simplify_achar(g95_expr *e) {
g95_expr *result;
int index;

    if (e->type != EXPR_CONSTANT)
	return NULL;

/* We cannot assume that the native character set is ASCII in this function */

    if (g95_extract_int(e, &index) != NULL)
	return &g95_bad_expr;

    result = g95_constant_result(BT_CHARACTER, -1, &e->where);

    result->value.character.string = g95_getmem(2);
    result->ts.cl = &g95_unity_charlen;

    result->value.character.length = 1;
    result->value.character.string[0] = ascii_table[index];
    result->value.character.string[1] = '\0';   /* For debugger */

    return result;
}



g95_expr *g95_simplify_acos(g95_expr *x) {
g95_expr *result;
bignum t;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    if (bg_compare_int(x->value.real,  1) > 0 ||
	bg_compare_int(x->value.real, -1) < 0) {
	g95_error("Argument of ACOS at %L must be between -1 and 1",
		  &x->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);

    if (bg_compare_int(x->value.real, 1) == 0) {
	result->value.real = bg_from_int(0, x->value.real->ff);
	goto done;
    }

    if (bg_compare_int(x->value.real, -1) == 0) {
	result->value.real = bg_convert(bg_pi, x->value.real->ff);
	goto done;
    }

    t = bg_negate(bg_multiply(x->value.real, x->value.real));
    t = bg_divide(x->value.real, bg_sqrt(bg_add_int(t, 1)));
    t = bg_negate(bg_arctangent(t));

    result->value.real = bg_add(t, bg_convert(bg_half_pi, t->ff));

done:
    result->value.real = big_clone(result->value.real);
    big_permanent(result->value.real);

    return result;
}



g95_expr *g95_simplify_adjustl(g95_expr *e) {
g95_expr *result;
int count, i, len;
char ch;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    len = e->value.character.length;

    result = g95_constant_result(BT_CHARACTER, e->ts.kind, &e->where);

    result->value.character.length = len;
    result->value.character.string = g95_getmem(len+1);

    for (count=0, i=0; i<len; ++i) {
	ch = e->value.character.string[i];
	if (ch != ' ')
	    break;

	++count;
    }

    for(i=0; i<len-count; ++i)
	result->value.character.string[i] = e->value.character.string[count+i];

    for(i=len-count; i<len; ++i)
	result->value.character.string[i] = ' ';

    result->value.character.string[len] = '\0';   /* For debugger */
    return result;
}



g95_expr *g95_simplify_adjustr(g95_expr *e) {
g95_expr *result;
int count, i, len;
char ch;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    len = e->value.character.length;

    result = g95_constant_result(BT_CHARACTER, e->ts.kind, &e->where);

    result->value.character.length = len;
    result->value.character.string = g95_getmem(len+1);

    for (count=0, i=len-1; i>=0; --i) {
	ch = e->value.character.string[i];
	if (ch != ' ')
	    break;
	++count;
    }

    for(i=0; i<count; ++i)
	result->value.character.string[i] = ' ';

    for(i=count; i<len; ++i)
	result->value.character.string[i] = e->value.character.string[i-count];

    result->value.character.string[len] = '\0';   /* For debugger */
    return result;
}



g95_expr *g95_simplify_aimag(g95_expr *e) {
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_REAL, e->ts.kind, &e->where);
    result->value.real = big_clone(e->value.complex.i);
    big_permanent(result->value.real);

    return result;
}



g95_expr *g95_simplify_aint(g95_expr *e, g95_expr *k) {
g95_expr *result;
g95_ff *ff;
int kind;

    kind = get_kind(BT_REAL, k, "AINT", e->ts.kind);
    if (kind == -1)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &e->where;
    result = g95_constant_result(BT_REAL, kind, &e->where);

    ff = g95_get_ff(kind);
    result->value.real = big_clone(bg_convert(bg_trunc(e->value.real), ff));

    big_permanent(result->value.real);
    g95_arithmetic_locus = NULL;

    return result;
}



g95_expr *g95_simplify_dint(g95_expr *e) {
g95_expr *result;
g95_ff *ff;
int kind;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &e->where;
    kind = g95_default_double_kind();

    result = g95_constant_result(BT_REAL, kind, &e->where);

    ff = g95_get_ff(kind);
    result->value.real = big_clone(bg_convert(bg_trunc(e->value.real), ff));

    big_permanent(result->value.real);
    g95_arithmetic_locus = NULL;

    return result;
}



g95_expr *g95_simplify_asin(g95_expr *x) {
g95_expr *result;
bignum a, t;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    if (bg_compare_int(x->value.real,  1) > 0 ||
	bg_compare_int(x->value.real, -1) < 0) {
	g95_error("Argument of ASIN at %L must be between -1 and 1",
		  &x->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);

    if (bg_compare_int(x->value.real, 1) == 0) {
	result->value.real = bg_half_pi;
	goto done;
    }

    if (bg_compare_int(x->value.real, -1) == 0) {
	result->value.real = bg_negate(bg_half_pi);
	goto done;
    }

    a = bg_convert(x->value.real, bg_pi->ff);
    t = bg_sqrt(bg_subtract(bg_from_int(1, a->ff),
			    bg_multiply(big_copy(a), big_copy(a))));

    result->value.real = bg_arctangent(bg_divide(a, t));

done:
    result->value.real = bg_convert(result->value.real, x->value.real->ff);
    result->value.real = big_clone(result->value.real);

    big_permanent(result->value.real);
    return result;
}



g95_expr *g95_simplify_atan(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &x->where;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_arctangent(x->value.real));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;

    return result;
}



g95_expr *g95_simplify_atan2(g95_expr *y, g95_expr *x) {
g95_expr *result;
int flag;
bignum u;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &y->where;

    if (bg_compare_int(y->value.real, 0) == 0) {
	if (bg_compare_int(x->value.real, 0) == 0) {
	    g95_error("If first argument of ATAN2 %L is zero, the second "
		      "argument must not be zero", &x->where);
	    g95_arithmetic_locus = NULL;
	    return &g95_bad_expr;
	}

	result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);

	if (bg_compare_int(big_copy(x->value.real), 0) < 0)
	    result->value.real = bg_pi;
	else
	    result->value.real = bg_from_int(0, x->value.real->ff);

	goto done;
    }

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);

    if (bg_compare_int(x->value.real, 0) == 0) {
	if (bg_compare_int(y->value.real, 0) < 0)
	    result->value.real = bg_negate(bg_half_pi);
	else
	    result->value.real = bg_half_pi;

	goto done;
    }

    u = bg_convert(y->value.real, bg_pi->ff);

    if (bg_compare_int(big_copy(u), 0) > 0)
	flag = 0;

    else {
	flag = 1;
	u = bg_negate(u);
    }

    u = bg_divide(u, bg_convert(x->value.real, bg_pi->ff));

    if (bg_compare_int(x->value.real, 0) > 0)
	result->value.real = bg_arctangent(u);

    else {
	result->value.real = bg_arctangent(bg_negate(u));
	result->value.real =
	    bg_add(bg_convert(bg_pi, u->ff), bg_negate(result->value.real));
    }

    if (flag)
	result->value.real = bg_negate(result->value.real);

done:
    result->value.real = bg_convert(result->value.real, x->value.real->ff);
    result->value.real = big_clone(result->value.real);
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_dnint(g95_expr *e) {
g95_expr *result;
g95_ff *ff;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_REAL, g95_default_double_kind(),
				 &e->where);

    g95_arithmetic_locus = &e->where;

    result->value.real = bg_round(e->value.real);

    ff = g95_get_ff(g95_default_double_kind());
    result->value.real = big_clone(bg_convert(result->value.real, ff));

    big_permanent(result->value.real);
    g95_arithmetic_locus = NULL;

    return result;
}



g95_expr *g95_simplify_anint(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

    kind = get_kind(BT_REAL, k, "ANINT", e->ts.kind);
    if (kind == -1)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_REAL, kind, &e->where);

    g95_arithmetic_locus = &e->where;

    result->value.real = bg_round(e->value.real);
    result->value.real = bg_convert(result->value.real, g95_get_ff(kind));
    result->value.real = big_clone(result->value.real);

    big_permanent(result->value.real);
    g95_arithmetic_locus = NULL;

    return result;
}



g95_expr *g95_simplify_bit_size(g95_expr *e) {
g95_expr *result;

    result = g95_constant_result(BT_INTEGER, e->ts.kind, &e->where);
    result->value.integer = big_clone(int_to_bi(g95_bit_size(e->ts.kind)));
    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_btest(g95_expr *e, g95_expr *pos) {
int b, m;

    if (g95_compare_expr_int(pos, 0) == CMP_LT ||
	g95_compare_expr_int(pos, g95_bit_size(e->ts.kind)-1) == CMP_GT) {
	g95_error("POS argument of BTEST() intrinsic out of range at %L",
		  &pos->where);
	return &g95_bad_expr;
    }

    if (e->type != EXPR_CONSTANT || pos->type != EXPR_CONSTANT)
	return NULL;

    if (g95_extract_int(pos, &b) != NULL || b < 0)
	return g95_logical_expr(0, &e->where);

    m = bi_getbit(e->value.integer, e->ts.kind, b);
    return g95_logical_expr(m, &e->where);
}



g95_expr *g95_simplify_ceiling(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

    kind = get_kind(BT_INTEGER, k, "CEILING", g95_default_real_kind(1));
    if (kind == -1)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &e->where;
    result = g95_constant_result(BT_INTEGER, kind, &e->where);
    result->value.integer = big_clone(bg_to_bi(bg_ceiling(e->value.real)));
    big_permanent(result->value.integer);

    g95_arithmetic_locus = NULL;
    return range_check(result, "CEILING");
}



g95_expr *g95_simplify_char(g95_expr *e, g95_expr *k) {
g95_expr *result;
int c, kind;

    kind = get_kind(BT_CHARACTER, k, "CHAR", g95_default_character_kind());
    if (kind == -1)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    if (g95_extract_int(e, &c) != NULL || c < 0 || c > 255) {
	g95_error("Bad character in CHAR function at %L", &e->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(BT_CHARACTER, kind, &e->where);
    result->ts.cl = &g95_unity_charlen;

    result->value.character.length = 1;
    result->value.character.string = g95_getmem(2);

    result->value.character.string[0] = c;
    result->value.character.string[1] = '\0';   /* For debugger */

    return result;
}



/* simplify_cmplx()-- Common subroutine for simplifying CMPLX and DCMPLX */

static g95_expr *simplify_cmplx(g95_expr *x, g95_expr *y, int kind) {
g95_expr *result;
g95_ff *ff;

    result = g95_constant_result(BT_COMPLEX, kind, &x->where);
    ff = g95_get_ff(kind);

    switch(x->ts.type) {
    case BT_INTEGER:
	g95_arithmetic_locus = &x->where;
	result->value.complex.r = big_clone(bg_from_bi(x->value.integer, ff));
	big_permanent(result->value.complex.r);

	g95_arithmetic_locus = NULL;
	break;

    case BT_REAL:
	result->value.complex.r = big_clone(bg_convert(x->value.real, ff));
	big_permanent(result->value.complex.r);
	break;

    case BT_COMPLEX:
	g95_arithmetic_locus = &x->where;
	result->value.complex.r = big_clone(bg_convert(x->value.complex.r,ff));
	result->value.complex.i = big_clone(bg_convert(x->value.complex.i,ff));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);

	g95_arithmetic_locus = NULL;
	break;

    default:
	g95_internal_error("simplify_cmplx(): Bad type (x)");
    }

    if (y == NULL) {
	if (x->ts.type == BT_INTEGER || x->ts.type == BT_REAL) {
	    result->value.complex.i = big_clone(bg_from_int(0, ff));
	    big_permanent(result->value.complex.i);
	}

    } else
	switch(y->ts.type) {
	case BT_INTEGER:
	    g95_arithmetic_locus = &y->where;
	    result->value.complex.i =
		big_clone(bg_from_bi(y->value.integer, ff));
	    big_permanent(result->value.complex.i);
      
	    g95_arithmetic_locus = NULL;
	    break;

	case BT_REAL:
	    g95_arithmetic_locus = &y->where;
	    result->value.complex.i = big_clone(bg_convert(y->value.real, ff));
	    big_permanent(result->value.complex.i);

	    g95_arithmetic_locus = NULL;
	    break;

	default:
	    g95_internal_error("simplify_cmplx(): Bad type (y)");
	}

    return result;
}



g95_expr *g95_simplify_cmplx(g95_expr *x, g95_expr *y, g95_expr *k) {
g95_expr *result;
int kind;

    kind = get_kind(BT_REAL, k, "CMPLX", g95_default_real_kind(1));
    if (kind == -1)
	return &g95_bad_expr;

    if (y != NULL && y->type != EXPR_CONSTANT)
	return NULL;

    if (x->type != EXPR_CONSTANT) {
	if (x->ts.type != BT_COMPLEX || x->ts.kind != kind || y != NULL)
	    return NULL;

	result = g95_get_expr();

	result->where   = x->where;
	result->ts.type = BT_COMPLEX;
	result->ts.kind = kind;
	result->type    = EXPR_OP;
	result->rank    = x->rank;

	result->value.op.operator = INTRINSIC_PAREN;
	result->value.op.op1      = g95_copy_expr(x);

	return result;
    }

    return simplify_cmplx(x, y, kind);
}



/* simplify_cobound_array()-- Simplify the co_lbound() and co_ubound()
 * intrinsics for returning an array. */

static g95_expr *simplify_cobound_array(g95_expr *coarray, int kind,
					int upper) {
g95_coarray_spec *cas;
g95_expr *result;
g95_typespec ts;
int i;

    if (upper)
	return NULL;    /* Must defer to runtime */

    cas = coarray->symbol->cas;
    if (cas->type != CAS_ASSUMED)
	return NULL;

    g95_clear_ts(&ts);
    ts.type = BT_INTEGER;
    ts.kind = kind;

    result = g95_start_constructor(&ts, &coarray->where);

    for(i=0; i<cas->corank; i++)
	g95_append_constructor(result, g95_copy_expr(cas->lower[i]));

    return result;
}



/* simplify_cobound_scalar()-- Simplify the co_lbound() and
 * co_ubound() intrinsics for returning a scalar. */

static g95_expr *simplify_cobound_scalar(g95_expr *coarray, g95_expr *dim,
					 int kind, int upper) {
g95_coarray_spec *cas;
g95_expr *result;
g95_ref *ref;
int d;

    if (dim->type != EXPR_CONSTANT || g95_extract_int(dim, &d) != NULL)
	return NULL;

    cas = coarray->symbol->cas;

    for(ref=coarray->ref; ref; ref=ref->next)
	if (ref->type == REF_COMPONENT)
	    cas = ref->u.c.component->cas;

    if (upper && d == cas->corank)
	return NULL;

    if (d < 1 || d > cas->corank) {
	g95_error("Bad DIM argument of %s at %L",
		  upper ? "CO_UBOUND" : "CO_LBOUND", &dim->where);
	return &g95_bad_expr;
    }

    d--;
    result = g95_copy_expr(upper ? cas->upper[d] : cas->lower[d]);

    if (result != NULL)
	result->ts.kind = kind;

    return result;
}



/* g95_simplify_co_lbound()-- Simplify the co_lbound intrinsic. */

g95_expr *g95_simplify_co_lbound(g95_expr *coarray, g95_expr *dim,
				 g95_expr *kind) {
int k;

    k = get_kind(BT_INTEGER, kind, "co_lbound", g95_default_integer_kind(0));

    return (dim == NULL)
	? simplify_cobound_array(coarray, k, 0)
	: simplify_cobound_scalar(coarray, dim, k, 0);
}



/* g95_simplify_co_ubound()-- Simplify the co_ubound intrinsic. */

g95_expr *g95_simplify_co_ubound(g95_expr *coarray, g95_expr *dim,
				 g95_expr *kind) {
int k;

    k = get_kind(BT_INTEGER, kind, "co_ubound", g95_default_integer_kind(0));

    return (dim == NULL)
	? simplify_cobound_array(coarray, k, 1)
	: simplify_cobound_scalar(coarray, dim, k, 1);
}



g95_expr *g95_simplify_conjg(g95_expr *e) {
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_COMPLEX, e->ts.kind, &e->where);

    result->value.complex.r = big_clone(e->value.complex.r);
    result->value.complex.i = big_clone(bg_negate(e->value.complex.i));

    big_permanent(result->value.complex.r);
    big_permanent(result->value.complex.i);

    return result;
}



g95_expr *g95_simplify_cos(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);

    g95_arithmetic_locus = &x->where;

    switch(x->ts.type) {
    case BT_REAL:
	result->value.real = big_clone(bg_cosine(x->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	result->value.complex.r =
	    big_clone(bg_multiply(bg_cosine(x->value.complex.r),
				  bg_hyperbolic_cosine(x->value.complex.i)));

	result->value.complex.i =
	    bg_multiply(bg_sine(x->value.complex.r),
			bg_hyperbolic_sine(x->value.complex.i));

	result->value.complex.i =
	    big_clone(bg_negate(result->value.complex.i));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);
	break;

    default:
	g95_internal_error("g95_simplify_cos(): Bad type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_cosh(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &x->where;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);
    result->value.real = bg_hyperbolic_cosine(x->value.real);
    result->value.real = big_clone(result->value.real);
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_dcmplx(g95_expr *x, g95_expr *y) {
g95_expr *result;

    if (y != NULL && y->type != EXPR_CONSTANT)
	return NULL;

    if (x->type != EXPR_CONSTANT) {
	if (x->ts.type != BT_COMPLEX ||
	    x->ts.kind != g95_default_double_kind() || y != NULL)
	    return NULL;

	result = g95_get_expr();
	result->where = x->where;
	result->ts.type = BT_COMPLEX;
	result->ts.kind = g95_default_double_kind();
	result->type = EXPR_OP;
	result->value.op.operator = INTRINSIC_PAREN;
	result->value.op.op1 = g95_copy_expr(x);
	result->rank = x->rank;

	return result;
    }

    return simplify_cmplx(x, y, g95_default_double_kind());
}



g95_expr *g95_simplify_dble(g95_expr *e) {
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    switch (e->ts.type) {
    case BT_INTEGER:
	result = g95_int2real(e, g95_default_double_kind());
	break;

    case BT_REAL:
	result = g95_real2real(e, g95_default_double_kind());
	break;

    case BT_COMPLEX:
	result = g95_complex2real(e, g95_default_double_kind());
	break;

    default:
	g95_internal_error("g95_simplify_dble(): bad type at %L", &e->where);
    }

    return result;
}



g95_expr *g95_simplify_digits(g95_expr *x) {
g95_expr *result;
int i, digits;

    i = g95_validate_kind(x->ts.type, x->ts.kind);
    if (i < 0)
	goto bad;

    switch(x->ts.type) {
    case BT_INTEGER:
	digits = g95_integer_kinds[i].digits;
	break;

    case BT_REAL:
    case BT_COMPLEX:
	digits = g95_real_kinds[i].digits;
	break;

    default:
    bad:
	g95_internal_error("g95_simplify_digits(): Bad type");
    }

    result = g95_int_expr(digits);
    result->ts.kind = g95_default_integer_kind(1);
    result->where = x->where;

    return result;
}



g95_expr *g95_simplify_dim(g95_expr *x, g95_expr *y) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);
    g95_arithmetic_locus = &x->where;

    switch (x->ts.type) {
    case BT_INTEGER:
	if (bi_compare(x->value.integer, y->value.integer) > 0)
	    result->value.integer =
		bi_subtract(x->value.integer, y->value.integer);

	else
	    result->value.integer = int_to_bi(0);

	result->value.integer = big_clone(result->value.integer);
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	if (bg_compare_gt(x->value.real, y->value.real))
	    result->value.real = bg_subtract(x->value.real, y->value.real);

	else
	    result->value.real = bg_from_int(0, g95_get_ff(x->ts.kind));

	result->value.real = big_clone(result->value.real);
	big_permanent(result->value.real);
	break;

    default:
	g95_internal_error("g95_simplify_dim(): Bad type");
    }

    g95_arithmetic_locus = NULL;
    return range_check(result, "DIM");
}



g95_expr *g95_simplify_dprod(g95_expr *x, g95_expr *y) {
g95_expr *result;
g95_ff *ff;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT)
	return NULL;

    ff = g95_get_ff(g95_default_double_kind());
    result = g95_constant_result(BT_REAL, g95_default_double_kind(),
				 &x->where);

    g95_arithmetic_locus = &x->where;
    result->value.real = bg_multiply(bg_convert(x->value.real, ff),
				     bg_convert(y->value.real, ff));

    result->value.real = big_clone(result->value.real);
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_epsilon(g95_expr *e) {
g95_expr *result;
int i;

    i = g95_validate_kind(e->ts.type, e->ts.kind);
    if (i == -1)
	g95_internal_error("g95_simplify_epsilon(): Bad kind");

    result = g95_constant_result(BT_REAL, e->ts.kind, &e->where);
    result->value.real = big_clone(bg_epsilon(g95_get_ff(e->ts.kind)));
    big_permanent(result->value.real);

    return result;
}



g95_expr *g95_simplify_exp(g95_expr *x) {
g95_expr *result;
bignum f;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &x->where;
    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);

    switch (x->ts.type) {
    case BT_REAL:
	result->value.real = big_clone(bg_exponential(x->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX: /* Euler's formula */
	g95_arithmetic_locus = &x->where;
	f = bg_exponential(x->value.complex.r);

	result->value.complex.r =
	    big_clone(bg_multiply(big_copy(f), bg_cosine(x->value.complex.i)));

	result->value.complex.i =
	    big_clone(bg_multiply(f, bg_sine(x->value.complex.i)));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);
	break;

    default:
	g95_internal_error("in g95_simplify_exp(): Bad type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_exponent(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    result = g95_int_expr(bg_exponent(x->value.real));
    result->ts.kind = g95_default_integer_kind(1);
    result->where = x->where;

    return result;
}



g95_expr *g95_simplify_float(g95_expr *a) {
g95_expr *result;
g95_ff *ff;
int kind;

    if (a->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &a->where;

    kind = g95_default_real_kind(1);
    ff = g95_get_ff(kind);

    result = g95_constant_result(BT_REAL, kind, &a->where);
    result->value.real = big_clone(bg_from_bi(a->value.integer, ff));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_floor(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

    kind = get_kind(BT_INTEGER, k, "FLOOR", g95_default_real_kind(1));
    if (kind == -1)
	g95_internal_error("g95_simplify_floor(): Bad kind");

    if (e->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &e->where;
    result = g95_constant_result(BT_INTEGER, kind, &e->where);
    result->value.integer = big_clone(bg_to_bi(bg_floor(e->value.real)));
    big_permanent(result->value.integer);
    g95_arithmetic_locus = NULL;

    return range_check(result, "FLOOR");
}



g95_expr *g95_simplify_fraction(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_fraction(x->value.real));
    big_permanent(result->value.real);

    return result;
}



g95_expr *g95_simplify_huge(g95_expr *e) {
g95_expr *result;

    result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);

    switch(e->ts.type) {
    case BT_INTEGER:
	result->value.integer = big_clone(bi_huge(e->ts.kind));
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	result->value.real = big_clone(bg_huge(g95_get_ff(e->ts.kind)));
	big_permanent(result->value.real);
	break;

    default:
	g95_internal_error("g95_simplify_huge(): Bad type");
    }

    return result;
}



g95_expr *g95_simplify_iachar(g95_expr *e) {
g95_expr *result, *len;
int index;

    len = e->ts.cl->length;

    if (e->ts.cl != &g95_unknown_charlen && len != NULL &&
	len->type == EXPR_CONSTANT &&
	bi_compare(len->value.integer, bi_1) != 0) {
	g95_error("Argument of IACHAR at %L must be of length one", &e->where);
	return &g95_bad_expr;
    }

    if (e->type != EXPR_CONSTANT)
	return NULL;

    index = xascii_table[(int) e->value.character.string[0] & 0xFF];

    result = g95_int_expr(index);
    result->where = e->where;
    result->ts.kind = g95_default_integer_kind(1);

    return range_check(result, "IACHAR");
}



g95_expr *g95_simplify_iand(g95_expr *x, g95_expr *y) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_INTEGER, x->ts.kind, &x->where);
    result->value.integer =
	big_clone(bi_and(x->value.integer, y->value.integer, result->ts.kind));
    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_ibclr(g95_expr *i, g95_expr *pos) {
g95_expr *result;
int n;

    if (g95_compare_expr_int(pos, 0) == CMP_LT ||
	g95_compare_expr_int(pos, g95_bit_size(i->ts.kind)-1) == CMP_GT) {
	g95_error("POS argument of IBCLR is out of range at %L", &pos->where);
	return &g95_bad_expr;
    }

    if (i->type != EXPR_CONSTANT || pos->type != EXPR_CONSTANT)
	return NULL;

    if (g95_extract_int(pos, &n) != NULL) {
	g95_error("Invalid second argument of IBCLR at %L", &pos->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(BT_INTEGER, i->ts.kind, &i->where);
    result->value.integer =
	big_clone(bi_setbit(i->value.integer, i->ts.kind, n, 0));

    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_ibits(g95_expr *x, g95_expr *pos, g95_expr *len) {
g95_expr *result;
int pos_v, len_v, i;

    if (pos->type == EXPR_CONSTANT &&
	(g95_extract_int(pos, &pos_v) != NULL || pos_v < 0)) {
	g95_error("Invalid second argument of IBITS at %L", &pos->where);
	return &g95_bad_expr;
    }

    if (len->type == EXPR_CONSTANT &&
	(g95_extract_int(len, &len_v) != NULL || len_v < 0)) {
	g95_error("Invalid third argument of IBITS at %L", &len->where);
	return &g95_bad_expr;
    }

    if (pos->type != EXPR_CONSTANT || len->type != EXPR_CONSTANT)
	return NULL;

    if (pos_v+len_v > g95_bit_size(x->ts.kind)) {
	g95_error("Size of bit-field in IBITS intrinsic exceeds BIT_SIZE "
		  "at %L", &pos->where);
	return &g95_bad_expr;
    }

    if (x->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);
    result->value.integer = bi_0;

    for(i=0; i<len_v; i++)
	if (bi_getbit(x->value.integer, x->ts.kind, i+pos_v))
	    result->value.integer =
		bi_setbit(result->value.integer, result->ts.kind, i, 1);

    result->value.integer = big_clone(result->value.integer);
    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_ibset(g95_expr *i, g95_expr *pos) {
g95_expr *result;
int n;

    if (g95_compare_expr_int(pos, 0) == CMP_LT ||
	g95_compare_expr_int(pos, g95_bit_size(i->ts.kind)-1) == CMP_GT) {
	g95_error("POS argument of IBSET is out of range at %L", &pos->where);
	return &g95_bad_expr;
    }

    if (i->type != EXPR_CONSTANT || pos->type != EXPR_CONSTANT)
	return NULL;

    if (g95_extract_int(pos, &n) != NULL) {
	g95_error("Invalid second argument of IBSET at %L", &pos->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(BT_INTEGER, i->ts.kind, &i->where);
    result->value.integer =
	big_clone(bi_setbit(i->value.integer, i->ts.kind, n, 1));

    big_permanent(result->value.integer);
    return result;
}



g95_expr *g95_simplify_ichar(g95_expr *e) {
g95_expr *result, *len;
int index;

    len = e->ts.cl->length;

    if (e->ts.cl != &g95_unknown_charlen && len != NULL &&
	len->type == EXPR_CONSTANT &&
	bi_compare(len->value.integer, bi_1) != 0) {
	g95_error("Argument of ICHAR at %L must be of length one", &e->where);
	return &g95_bad_expr;
    }

    if (e->type != EXPR_CONSTANT)
	return NULL;

    index = (unsigned char) e->value.character.string[0];

    result = g95_int_expr(index);
    result->where = e->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}



g95_expr *g95_simplify_ieor(g95_expr *x, g95_expr *y) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_INTEGER, x->ts.kind, &x->where);
    result->value.integer =
	big_clone(bi_xor(x->value.integer, y->value.integer, result->ts.kind));

    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_index(g95_expr *x, g95_expr *y, g95_expr *b) {
g95_expr *result;
int back, len, lensub;
int i, j, k, count, index=0, start;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT ||
	(b != NULL && b->type != EXPR_CONSTANT))
	return NULL;

    if (b != NULL && b->value.logical != 0)
	back = 1;
    else
	back = 0;

    result = g95_constant_result(BT_INTEGER, -1, &x->where);

    len    = x->value.character.length;
    lensub = y->value.character.length;

    if (len < lensub) {
	index = 0;
	goto done;
    }

    if (back == 0) {
	if (lensub == 0) {
	    index = 1;
	    goto done;
	}

	if (lensub == 1) {
	    for(i=0; i<len; i++)
		for(j=0; j<lensub; j++)
		    if (y->value.character.string[j] ==
			x->value.character.string[i]) {
			index = i+1;
			goto done;
		    }

	} else {
	    for(i=0; i<len; i++) {
		for(j=0; j<lensub; j++) {
		    if (y->value.character.string[j] ==
			x->value.character.string[i]) {
			start = i;
			count = 0;

			for(k=0; k<lensub; k++)
			    if (y->value.character.string[k] ==
				x->value.character.string[k+start])
				count++;

			if (count == lensub) {
			    index = start+1;
			    goto done;
			}
		    }
		}
	    }
	}

    } else {
	if (lensub == 0) {
	    index = len+1;
	    goto done;

	} else if (lensub == 1) {
	    for(i=0; i<len; i++) {
		for(j=0; j<lensub; j++) {
		    if (y->value.character.string[j] ==
			x->value.character.string[len-i]) {
			index = len-i+1;
			goto done;
		    }
		}
	    }

	} else {
	    for(i=0; i<len; i++) {
		for(j=0; j<lensub; j++) {
		    if (y->value.character.string[j] == 
			x->value.character.string[len-i]) {
			start = len-i;
			if (start <= len-lensub) {
			    count = 0;
			    for(k=0; k<lensub; k++)
				if (y->value.character.string[k] ==
				    x->value.character.string[k+start])
				    count++;

			    if (count == lensub) {
				index = start+1;
				goto done;
			    }

			} else
			    continue;
		    }
		}
	    }
	}
    }

done:
  result->value.integer = big_clone(int_to_bi(index));
  big_permanent(result->value.integer);

  return result;
}



g95_expr *g95_simplify_int(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

    kind = get_kind(BT_INTEGER, k, "INT", g95_default_integer_kind(1));
    if (kind == -1)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT) {
	if (e->ts.type != BT_INTEGER || e->ts.kind != kind)
	    return NULL;

	result = g95_get_expr();
	result->where = e->where;
	result->ts.type = BT_INTEGER;
	result->ts.kind = kind;
	result->type = EXPR_OP;
	result->value.op.operator = INTRINSIC_PAREN;
	result->value.op.op1 = g95_copy_expr(e);
	result->rank = e->rank;

	return result;
    }

    g95_arithmetic_locus = &e->where;
    result = g95_constant_result(BT_INTEGER, kind, &e->where);

    switch(e->ts.type) {
    case BT_INTEGER:
	result->value.integer = e->value.integer;
	break;

    case BT_REAL:
	result->value.integer = bg_to_bi(bg_trunc(e->value.real));
	break;

    case BT_COMPLEX:
	result->value.integer = bg_to_bi(bg_trunc(e->value.complex.r));
	break;

    default:
	g95_internal_error("g95_simplify_int(): Bad type");
    }

    result->value.integer = big_clone(result->value.integer);
    big_permanent(result->value.integer);

    g95_arithmetic_locus = NULL;
    return range_check(result, "INT");
}



g95_expr *g95_simplify_ifix(g95_expr *e) {
g95_expr *result;
int kind;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    kind = g95_default_integer_kind(1);
    result = g95_constant_result(BT_INTEGER, kind, &e->where);

    g95_arithmetic_locus = &e->where;
    result->value.integer = big_clone(bg_to_bi(bg_trunc(e->value.real)));
    big_permanent(result->value.integer);
    g95_arithmetic_locus = NULL;

    return range_check(result, "IFIX");
}



g95_expr *g95_simplify_idint(g95_expr *e) {
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(1),
				 &e->where);

    g95_arithmetic_locus = &e->where;
    result->value.integer = big_clone(bg_to_bi(bg_trunc(e->value.real)));
    big_permanent(result->value.integer);
    g95_arithmetic_locus = NULL;

    return range_check(result, "IDINT");
}



g95_expr *g95_simplify_ior(g95_expr *x, g95_expr *y) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_INTEGER, x->ts.kind, &x->where);
    result->value.integer =
	big_clone(bi_or(x->value.integer, y->value.integer, result->ts.kind));

    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_ishft(g95_expr *e, g95_expr *s) {
g95_expr *result;
int k, shift;

    if (g95_compare_expr_int(s, -g95_bit_size(e->ts.kind)) == CMP_LT ||
	g95_compare_expr_int(s,  g95_bit_size(e->ts.kind)) == CMP_GT) {
	g95_error("SHIFT argument of ISHFT() intrinsic is out of range at %L",
		  &s->where);
	return &g95_bad_expr;
    }

    if (e->type != EXPR_CONSTANT || s->type != EXPR_CONSTANT)
	return NULL;

    if (g95_extract_int(s, &shift) != NULL) {
	g95_error("Invalid second argument of ISHFT at %L", &s->where);
	return &g95_bad_expr;
    }

    k = g95_validate_kind(BT_INTEGER, e->ts.kind);
    if (k == -1)
	g95_internal_error("In g95_simplify_ishft: bad kind");

    result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);
    result->value.integer =
	big_clone(bi_shift(e->value.integer, e->ts.kind, shift, 0));

    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_ishftc(g95_expr *e, g95_expr *s, g95_expr *size) {
int shift, isize, k;
g95_expr *result;

    if (size != NULL &&
	(g95_compare_expr_int(size, 1) == CMP_LT ||
	 g95_compare_expr_int(size, g95_bit_size(e->ts.kind)) == CMP_GT)) {
	g95_error("SIZE argument of ISHFTC() intrinsic is out of range at %L",
		  &size->where);
	return &g95_bad_expr;
    }

    if (s->type != EXPR_CONSTANT ||
	(size != NULL && size->type != EXPR_CONSTANT))
	return NULL;

    if (g95_extract_int(s, &shift) != NULL) {
	g95_error("SHIFT argument of ISHFTC at %L is out of range", &s->where);
	return &g95_bad_expr;
    }

    k = g95_validate_kind(e->ts.type, e->ts.kind);
    if (k == -1)
	g95_internal_error("In g95_simplify_ishftc: bad kind");

    if (size == NULL)
	isize = g95_integer_kinds[k].bit_size;

    else if (g95_extract_int(size, &isize) != NULL || isize <= 0) {
	g95_error("Invalid SIZE argument of ISHFTC at %L", &size->where);
	return &g95_bad_expr;
    }
    

    if (shift < -isize || shift > isize) {
	g95_error("SHIFT argument of ISHFTC intrinsic is out of range at %L",
		  &s->where);
	return &g95_bad_expr;
    }

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);
    result->value.integer =
	big_clone(bi_shift(e->value.integer, e->ts.kind, shift, isize));

    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_kind(g95_expr *e) {
g95_expr *result;

    if (e->ts.type == BT_DERIVED) {
	g95_error("Argument of KIND at %L is a DERIVED type", &e->where);
	return &g95_bad_expr;
    }

    result = g95_int_expr(e->ts.kind);
    result->ts.kind = g95_default_integer_kind(1);
    result->where = e->where;

    return result;
}



g95_expr *g95_simplify_lbound(g95_expr *array, g95_expr *dim, g95_expr *kind) {

    if (array->rank == 0) {
	g95_error("Argument of LBOUND at %L must be an array", &array->where);
	return &g95_bad_expr;
    }

    return simplify_bound(array, dim, kind, 0);
}



/* simplify_substring()-- Simplify a substring reference. */

static g95_expr *simplify_substring(g95_ref *ref, g95_locus *where) {
bignum s, e, len;
g95_expr *result;
int kind;

    if (ref->u.ss.start == NULL)
	s = bi_1;

    else if (ref->u.ss.start->type != EXPR_CONSTANT)
	return NULL;

    else
	s = ref->u.ss.start->value.integer;

    if (ref->u.ss.end == NULL) {
	if (ref->u.ss.length == NULL || ref->u.ss.length->length == NULL ||
	    ref->u.ss.length->length->type != EXPR_CONSTANT)
	    return NULL;
	e = ref->u.ss.length->length->value.integer;

    } else if (ref->u.ss.end->type != EXPR_CONSTANT)
	return NULL;

    else
	e = ref->u.ss.end->value.integer;

    len = bi_add(bi_subtract(e, s), bi_1);

    if (bi_compare(big_copy(len), bi_1) < 0) {
	big_free(len);
	len = big_clone(bi_0);
    }

    kind   = g95_default_integer_kind(0);
    result = g95_constant_result(BT_INTEGER, kind, where);

    result->value.integer = len;
    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_len(g95_expr *e) {
g95_typespec *ts;
g95_expr *result;
g95_ref *ref;

    result = NULL;

    switch(e->type) {
    case EXPR_CONSTANT:
	result = g95_constant_result(BT_INTEGER, -1, &e->where);
	result->value.integer= big_clone(int_to_bi(e->value.character.length));
	big_permanent(result->value.integer);
	break;

    case EXPR_VARIABLE:
	ts = &e->symbol->ts;

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
	    case REF_COARRAY:
		break;

	    case REF_COMPONENT:
		ts = &ref->u.c.component->ts;
		break;

	    case REF_SUBSTRING:
		result = simplify_substring(ref, &e->where);
		ts = NULL;
		break;
	    }

	if (ts != NULL)
	    result = (ts->cl != NULL && ts->cl->length != NULL &&
		      ts->cl->length->type == EXPR_CONSTANT)
		? g95_copy_expr(ts->cl->length)
		: NULL;

	break;

    default:
	break;
    }

    return result;
}



g95_expr *g95_simplify_len_trim(g95_expr *e) {
int count, len, i;
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_INTEGER, -1, &e->where);
    len = e->value.character.length;

    for(count=0, i=1; i<=len; i++)
	if (e->value.character.string[len-i] == ' ')
	    count++;
	else
	    break;

    result->value.integer = big_clone(int_to_bi(len - count));
    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_lge(g95_expr *a, g95_expr *b) {

    return (a->type != EXPR_CONSTANT || b->type != EXPR_CONSTANT)
	? NULL
	: g95_logical_expr(g95_compare_string(a, b, xascii_table) >= 0,
			   &a->where);
}



g95_expr *g95_simplify_lgt(g95_expr *a, g95_expr *b) {

    return (a->type != EXPR_CONSTANT || b->type != EXPR_CONSTANT)
	? NULL
	: g95_logical_expr(g95_compare_string(a, b, xascii_table) > 0,
			   &a->where);
}



g95_expr *g95_simplify_lle(g95_expr *a, g95_expr *b) {

    return (a->type != EXPR_CONSTANT || b->type != EXPR_CONSTANT)
	? NULL
	: g95_logical_expr(g95_compare_string(a, b, xascii_table) <= 0,
			   &a->where);
}



g95_expr *g95_simplify_llt(g95_expr *a, g95_expr *b) {

    return (a->type != EXPR_CONSTANT || b->type != EXPR_CONSTANT)
	? NULL
	: g95_logical_expr(g95_compare_string(a, b, xascii_table) < 0,
			   &a->where);
}



g95_expr *g95_simplify_log(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT || x->ts.type == BT_COMPLEX)
	return NULL;

    g95_arithmetic_locus = &x->where;
    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_ln(x->value.real));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_log10(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT || x->ts.type == BT_COMPLEX)
	return NULL;

    g95_arithmetic_locus = &x->where;
    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_log(x->value.real));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_logical(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

    kind = get_kind(BT_LOGICAL, k, "LOGICAL", g95_default_logical_kind());
    if (kind < 0)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_LOGICAL, kind, &e->where);
    result->value.logical = e->value.logical;

    return result;
}



/* simplify_min_max()-- This function is special since MAX() can take
 * any number of arguments.  The simplified expression is a rewritten
 * version of the argument list containing at most one constant
 * element.  Other constant elements are deleted.  Because the
 * argument list has already been checked, this function always
 * succeeds.  sign is 1 for MAX(), -1 for MIN(). */

static g95_expr *simplify_min_max(g95_expr *expr, int sign) {
g95_actual_arglist *arg, *last, *extremum;
int m, length;
g95_expr *r;
char *p;

    last     = NULL;
    extremum = NULL;

    arg = expr->value.function.actual;

    for(; arg; last=arg, arg=arg->next) {
	if (arg->u.expr->type != EXPR_CONSTANT)
	    continue;

	if (extremum == NULL) {
	    extremum = arg;
	    continue;
	}

	switch(arg->u.expr->ts.type) {
	case BT_INTEGER:
	    if (bi_compare(arg->u.expr->value.integer,
			   extremum->u.expr->value.integer)*sign > 0) {

		big_depermanent(extremum->u.expr->value.integer);
		big_free(extremum->u.expr->value.integer);

		extremum->u.expr->value.integer =
		    big_clone(arg->u.expr->value.integer);
		big_permanent(extremum->u.expr->value.integer);
	    }

	    break;

	case BT_REAL:
	    m = (sign < 0)
		? bg_compare_lt(arg->u.expr->value.real,
				extremum->u.expr->value.real)
		: bg_compare_gt(arg->u.expr->value.real,
				extremum->u.expr->value.real);

	    if (m) {
		big_depermanent(extremum->u.expr->value.real);
		big_free(extremum->u.expr->value.real);

		extremum->u.expr->value.real =
		    big_clone(arg->u.expr->value.real);
		big_permanent(extremum->u.expr->value.real);
	    }

	    break;

	case BT_CHARACTER:
	    if (g95_compare_string(arg->u.expr,
				   extremum->u.expr, NULL)*sign > 0) {
		p = arg->u.expr->value.character.string;
		length = arg->u.expr->value.character.length;

		arg->u.expr->value.character.string =
		    extremum->u.expr->value.character.string;

		arg->u.expr->value.character.length =
		    extremum->u.expr->value.character.length;

		extremum->u.expr->value.character.string = p;
		extremum->u.expr->value.character.length = length;
	    }

	    break;

	default:
	    g95_internal_error("g95_simplify_max(): Bad type in arglist");
	}

	/* Delete the extra constant argument */

	if (last == NULL)
	    expr->value.function.actual = arg->next;
	else
	    last->next = arg->next;

	arg->next = NULL;
	g95_free_actual_arglist(arg);
	arg = last;
    }

    /* If there is one value left, replace the function call with the
     * expression */

    if (expr->value.function.actual->next != NULL)
	return NULL;

    r = g95_copy_expr(expr->value.function.actual->u.expr);
    if (g95_convert_type(r, &expr->ts, 0) == SUCCESS)
	return r;

    g95_free_expr(r);
    return &g95_bad_expr;
}



g95_expr *g95_simplify_min(g95_expr *e) {

    return simplify_min_max(e, -1);
}



g95_expr *g95_simplify_max(g95_expr *e) {

    return simplify_min_max(e, 1);
}



g95_expr *g95_simplify_maxexponent(g95_expr *x) {
g95_expr *result;
int i;

    i = g95_validate_kind(BT_REAL, x->ts.kind);
    if (i < 0)
	g95_internal_error("g95_simplify_maxexponent(): Bad kind");

    result = g95_int_expr(g95_real_kinds[i].max_exponent);
    result->where = x->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}



g95_expr *g95_simplify_minexponent(g95_expr *x) {
g95_expr *result;
int i;

    i = g95_validate_kind(BT_REAL, x->ts.kind);
    if (i < 0)
	g95_internal_error("g95_simplify_minexponent(): Bad kind");

    result = g95_int_expr(g95_real_kinds[i].min_exponent);
    result->where = x->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}


/* g95_simplify_merge()-- The problem with simplifying array
 * expressions is that bounds information is incorrectly discarded. */

g95_expr *g95_simplify_merge(g95_expr *tsource, g95_expr *fsource,
			     g95_expr *mask) {

    if (g95_simplify_mode == SIMP_INIT && G95_STRICT_F95()) {
	g95_error("MERGE intrinsic not permitted in initialization expression "
		  "at %L", &tsource->where);
	return &g95_bad_expr;
    }

    return (mask->type != EXPR_CONSTANT || tsource->rank > 0 ||
	    fsource->rank > 0 || mask->rank > 0)
	? NULL
	: g95_copy_expr(mask->value.logical ? tsource : fsource);
}



g95_expr *g95_simplify_mod(g95_expr *a, g95_expr *p) {
g95_expr *result;
bignum t;

    if (a->type != EXPR_CONSTANT || p->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &a->where;

    switch (a->ts.type) {
    case BT_INTEGER:
	if (bi_compare(p->value.integer, bi_0) == 0) {
	    g95_error("Second argument MOD at %L is zero", &a->where);
	    return &g95_bad_expr;
	}

	result = g95_constant_result(a->ts.type, a->ts.kind, &a->where);

	t = bi_divide(a->value.integer, p->value.integer);
	t = bi_multiply(t, p->value.integer);

	result->value.integer = big_clone(bi_subtract(a->value.integer, t));
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	if (bg_compare_int(p->value.real, 0) == 0) {
	    g95_error("Second argument of MOD at %L is zero", &p->where);
	    return &g95_bad_expr;
	}

	result = g95_constant_result(a->ts.type, a->ts.kind, &a->where);

	t = bg_trunc(bg_divide(a->value.real, p->value.real));
	t = bg_subtract(a->value.real, bg_multiply(t, p->value.real));

	result->value.real = big_clone(t);
	big_permanent(result->value.real);
	break;

    default:
	g95_internal_error("g95_simplify_mod(): Bad arguments");
    }

    g95_arithmetic_locus = NULL;
    return range_check(result, "MOD");
}



g95_expr *g95_simplify_modulo(g95_expr *a, g95_expr *p) {
g95_expr *result;
bignum t;

    if (a->type != EXPR_CONSTANT || p->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &a->where;

    switch (a->ts.type) {
    case BT_INTEGER:
	if (bi_is_zero(p->value.integer)) {
	    g95_error("Second argument of MODULO at %L is zero", &a->where);
	    return &g95_bad_expr;
	}

	result = g95_constant_result(a->ts.type, a->ts.kind, &a->where);
	t = bi_rem(a->value.integer, p->value.integer);

	if (bi_is_negative(p->value.integer)) {
	    if (!bi_is_negative(big_copy(t)) && !bi_is_zero(big_copy(t)))
		t = bi_add(t, p->value.integer);

	} else if (bi_is_negative(big_copy(t)))
	    t = bi_add(t, p->value.integer);

	result->value.integer = big_clone(t);
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	if (bg_compare_int(p->value.real, 0) == 0) {
	    g95_error("Second argument of MODULO at %L is zero", &p->where);
	    return &g95_bad_expr;
	}

	result = g95_constant_result(a->ts.type, a->ts.kind, &a->where);
	t = bg_floor(bg_divide(a->value.real, p->value.real));
	t = bg_subtract(a->value.real, bg_multiply(t, p->value.real));

	result->value.real = big_clone(t);
	big_permanent(result->value.real);
	break;

    default:
	g95_internal_error("g95_simplify_modulo(): Bad arguments");
	result = NULL;
    }

    g95_arithmetic_locus = NULL;
    return range_check(result, "MODULO");
}



g95_expr *g95_simplify_nearest(g95_expr *x, g95_expr *s) {
g95_expr *result;
int k;

    if (x->type != EXPR_CONSTANT || s->type != EXPR_CONSTANT)
	return NULL;

    k = g95_validate_kind(x->ts.type, x->ts.kind);
    if (k == -1)
	g95_internal_error("g95_simplify_nearest(): Bad kind");

    if (bg_real_type(s->value.real) == FF_ZERO) {
	g95_error("S argument of NEAREST at %L cannot be zero", &s->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);
    result->value.real =
	big_clone(bg_nearest(x->value.real, bg_compare_int(s->value.real, 0)));
    big_permanent(result->value.real);

    return result;
}



/* g95_simplify_newline()-- F2003 newline() function.  This function
 * is somewhat unique in that it always simplifies. */

g95_expr *g95_simplify_newline(g95_expr *a) {
g95_expr *result;

    result = g95_constant_result(BT_CHARACTER, a->ts.kind, &a->where);
    result->ts.cl = &g95_unity_charlen;

    result->value.character.length = 1;
    result->value.character.string = g95_getmem(2);

    result->value.character.string[0] = '\n';
    result->value.character.string[1] = '\0';

    return result;
}



static g95_expr *simplify_nint(char *name, g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

    kind = get_kind(BT_INTEGER, k, name, g95_default_integer_kind(1));
    if (kind == -1)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_INTEGER, kind, &e->where);
    result->value.real = big_clone(bg_to_bi(bg_round(e->value.real)));

    big_permanent(result->value.integer);

    return range_check(result, name);
}



g95_expr *g95_simplify_nint(g95_expr *e, g95_expr *k) {

    return simplify_nint("NINT", e, k);
}



g95_expr *g95_simplify_idnint(g95_expr *e) {

    return simplify_nint("IDNINT", e, NULL);
}



g95_expr *g95_simplify_not(g95_expr *e) {
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);
    result->value.integer = big_clone(bi_not(e->value.integer, e->ts.kind));
    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_null(g95_expr *mold) {
g95_expr *result;

    result = g95_null_expr(NULL);
    result->value.op.op1 = g95_copy_expr(mold);

    if (mold != NULL) {
	result->where = mold->where;
	result->ts = mold->ts;
	result->rank = mold->rank;
    }

    return result;
}



g95_expr *g95_simplify_precision(g95_expr *e) {
g95_expr *result;
int i;

    i = g95_validate_kind(e->ts.type, e->ts.kind);
    if (i == -1)
	g95_internal_error("g95_simplify_precision(): Bad kind");

    result = g95_int_expr(g95_real_kinds[i].precision);
    result->where = e->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}



g95_expr *g95_simplify_radix(g95_expr *e) {
g95_expr *result;
int i;

    i = g95_validate_kind(e->ts.type, e->ts.kind);
    if (i < 0)
	goto bad;

    switch(e->ts.type) {
    case BT_INTEGER:
	i = g95_integer_kinds[i].radix;
	break;

    case BT_REAL:
	i = g95_real_kinds[i].radix;
	break;

    default:
    bad:
	g95_internal_error("g95_simplify_radix(): Bad type");
    }

    result = g95_int_expr(i);
    result->where = e->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}



g95_expr *g95_simplify_range(g95_expr *e) {
g95_expr *result;
int i, j;

    i = g95_validate_kind(e->ts.type, e->ts.kind);
    if (i < 0)
	goto bad_type;

    switch(e->ts.type) {
    case BT_INTEGER:
	j = g95_integer_kinds[i].range;
	break;

    case BT_REAL:
    case BT_COMPLEX:
	j = g95_real_kinds[i].range;
	break;

    bad_type:
    default:
	g95_internal_error("g95_simplify_range(): Bad kind");
    }

    result = g95_int_expr(j);
    result->where = e->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}



g95_expr *g95_simplify_real(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

    kind = (e->ts.type == BT_COMPLEX)
	? e->ts.kind
	: g95_default_real_kind(1);

    kind = get_kind(BT_REAL, k, "REAL", kind);
    if (kind == -1)
	return &g95_bad_expr;

    if (e->type != EXPR_CONSTANT) {
	if (e->ts.type != BT_REAL || e->ts.kind != kind)
	    return NULL;

	result = g95_get_expr();
	result->where = e->where;
	result->ts.type = BT_REAL;
	result->ts.kind = kind;
	result->type = EXPR_OP;
	result->value.op.operator = INTRINSIC_PAREN;
	result->value.op.op1 = g95_copy_expr(e);
	result->rank = e->rank;

	return result;
    }

    switch (e->ts.type) {
    case BT_INTEGER:
	result = g95_int2real(e, kind);
	break;

    case BT_REAL:
	result = g95_real2real(e, kind);
	break;

    case BT_COMPLEX:
	result = g95_complex2real(e, kind);
	break;

    default:
	g95_internal_error("bad type in REAL");
	return &g95_bad_expr;
    }

    return range_check(result, "REAL");
}


g95_expr *g95_simplify_repeat(g95_expr *e, g95_expr *n) {
g95_expr *result;
int i, j, len, ncopies, nlen;

    if (n != NULL && n->type == EXPR_CONSTANT &&
	(g95_extract_int(n, &ncopies) != NULL || ncopies < 0)) {
	g95_error("Invalid second argument of REPEAT at %L", &n->where);
	return &g95_bad_expr;
    }

    if (e->type != EXPR_CONSTANT || n->type != EXPR_CONSTANT)
	return NULL;

    len    = e->value.character.length;
    nlen   = ncopies*len;

    result = g95_constant_result(BT_CHARACTER, e->ts.kind, &e->where);

    if (ncopies == 0) {
	result->value.character.string=g95_getmem(1);
	result->value.character.length=0;
	result->value.character.string='\0';
	return result;
    }

    result->value.character.length=nlen;
    result->value.character.string=g95_getmem(nlen+1);

    for(i=0; i<ncopies; i++)
	for(j=0; j<len; j++)
	    result->value.character.string[j+i*len] =
		e->value.character.string[j];

    result->value.character.string[nlen] = '\0';  /* For debugger */
    return result;
}



/* g95_simplify_reshape()-- This one is a bear, but mainly has to do
 * with shuffling elements. */

g95_expr *g95_simplify_reshape(g95_expr *source, g95_expr *shape_exp,
			       g95_expr *pad, g95_expr *order_exp) {

bignum index, nsource, shape[G95_MAX_DIMENSIONS];
int i, rank, npad, source_copy, x[G95_MAX_DIMENSIONS];
int order[G95_MAX_DIMENSIONS];
g95_constructor *head, *tail;
g95_expr *e;

/* Unpack the shape array */

    if ((source->type != EXPR_CONSTANT && source->type != EXPR_ARRAY) ||
	!g95_is_constant_expr(source))
	return NULL;

    if (shape_exp->type != EXPR_ARRAY || !g95_is_constant_expr(shape_exp))
	return NULL;

    if (pad != NULL && (pad->type != EXPR_ARRAY || !g95_is_constant_expr(pad)))
	return NULL;

    if (order_exp != NULL &&
	(order_exp->type != EXPR_ARRAY || !g95_is_constant_expr(order_exp)))
	return NULL;

    source_copy = !g95_expanded_ac(source);
    if (source_copy) {
	source = g95_copy_expr(source);
	g95_expand_constructor(source);
    }

    index = bi_0;
    rank = 0;
    head = tail = NULL;

    for(i=0; i<G95_MAX_DIMENSIONS; i++)
	shape[i] = NULL;

    for(;;) {
	e = g95_get_array_element(shape_exp, int_to_bi(rank));
	if (e == NULL)
	    break;

	shape[rank] = big_clone(e->value.integer);
	g95_free_expr(e);

	if (rank >= G95_MAX_DIMENSIONS) {
	    g95_error("Too many dimensions in shape specification for RESHAPE "
		      "at %L", &e->where);

	    goto bad_reshape;
	}

	if (bi_compare(big_copy(shape[rank]), bi_0) < 0) {
	    g95_error("Shape specification at %L cannot be negative",
		      &e->where);
	    goto bad_reshape;
	}

	rank++;
    }

    if (rank == 0) {
	g95_error("Shape specification at %L cannot be the null array",
		  &shape_exp->where);
	goto bad_reshape;
    }

    if (source->type == EXPR_CONSTANT) {  /* Simple case */
	e = g95_copy_expr(source);
	goto cleanup;
    }

    /* Now unpack the order array if present */

    if (order_exp == NULL) {
	for(i=0; i<rank; i++)
	    order[i] = i;

    } else {
	for(i=0; i<rank; i++)
	    x[i] = 0;

	for(i=0; i<rank; i++) {
	    e = g95_get_array_element(order_exp, int_to_bi(i));
	    if (e == NULL) {
		g95_error("ORDER parameter of RESHAPE at %L is not the same "
			  "size as SHAPE parameter", &e->where);
		goto bad_reshape;
	    }

	    if (g95_extract_int(e, &order[i]) != NULL) {
		g95_error("Error in ORDER parameter of RESHAPE at %L",
			  &e->where);
		g95_free_expr(e);
		goto bad_reshape;
	    }

	    g95_free_expr(e);

	    if (order[i] < 1 || order[i] > rank) {
		g95_error("ORDER parameter of RESHAPE at %L is out of range",
			  &e->where);
		goto bad_reshape;
	    }

	    order[i]--;

	    if (x[order[i]]) {
		g95_error("Invalid permutation in ORDER parameter at %L",
			  &e->where);
		goto bad_reshape;
	    }

	    x[order[i]] = 1;
	}
    }

    /* Count the elements in the source and padding arrays */

    npad = 0;
    if (pad != NULL)
	npad = bi_to_int(g95_array_size(pad));

    nsource = g95_array_size(source);

    /* If it weren't for that pesky permutation we could just loop
     * through the source and round out any shortage with pad
     * elements.  But no, someone just had to have the compiler do
     * something the user should be doing. */

    for(i=0; i<rank; i++)
	x[i] = 0;

    for(;;) {     /* Figure out which element to extract */
	index = bi_0;

	for(i=rank-1; i>=0; i--) {
	    index = bi_int_add(index, x[order[i]]);
	    if (i != 0)
		index = bi_multiply(index, big_copy(shape[order[i-1]]));
	}

	if (bi_compare(big_copy(index), bi_maxint) > 0) {
	    g95_internal_error("Reshaped array too large at %L", &e->where);
	    goto bad_reshape;
	}

	if (bi_compare(big_copy(index), big_copy(nsource)) < 0)
	    e = g95_get_array_element(source, index);

	else {
	    index = bi_subtract(index, big_copy(nsource));

	    if (npad == 0) {
		g95_error("PAD parameter required for short SOURCE parameter "
			  "at %L", &source->where);
		goto bad_reshape;
	    }

	    index = bi_mod(index, int_to_bi(npad));
	    e = g95_get_array_element(pad, index);
	}

	if (head == NULL)
	    head = tail = g95_get_constructor();

	else {
	    tail->next = g95_get_constructor();
	    tail = tail->next;
	}

	if (e == NULL)
	    goto bad_reshape;

	tail->where = e->where;
	tail->expr = e;

	/* Calculate the next element */

	i = 0;
    inc:
	if (bi_compare(int_to_bi(++x[i]), big_copy(shape[i])) < 0)
	    continue;

	x[i++] = 0;
	if (i < rank)
	    goto inc;

	break;
    }

    big_free(nsource);

    e = g95_get_expr();
    e->type = EXPR_ARRAY;
    e->value.constructor.c = head;
    e->ts = head->expr->ts;
    e->where = source->where;

cleanup:
    if (source_copy)
	g95_free_expr(source);

    e->shape = g95_get_shape(rank);

    for(i=0; i<rank; i++) {
	e->shape[i] = big_clone(shape[i]);
	big_permanent(e->shape[i]);
    }

    e->rank = rank;

    return e;

bad_reshape:
    if (source_copy)
	g95_free_expr(source);

    g95_free_constructor(head);
    big_free(index);

    for(i=0; i<G95_MAX_DIMENSIONS; i++)
	if (shape[i] != NULL)
	    big_free(shape[i]);

    return &g95_bad_expr;
}



g95_expr *g95_simplify_rrspacing(g95_expr *x) {
g95_expr *result;
int i;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    i = g95_validate_kind(x->ts.type, x->ts.kind);
    if (i < 0)
	g95_internal_error("g95_simplify_rrspacing(): bad kind");

    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_rrspacing(x->value.real));
    big_permanent(result->value.real);

    return result;
}



g95_expr *g95_simplify_scale(g95_expr *x, g95_expr *i) {
g95_expr *result;
char *msg;
int e;

    if (x->type != EXPR_CONSTANT || i->type != EXPR_CONSTANT)
	return NULL;

    msg = g95_extract_int(i, &e);
    if (msg != NULL) {
	g95_error("EXPONENT argument too large at %L", &i->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_scale(x->value.real, e));
    big_permanent(result->value.real);

    return result;
}



static int contains_char(char c, char *p, int len) {

    while(len > 0) {
	if (*p++ == c)
	    return 1;

	len--;
    }

    return 0;
}



g95_expr *g95_simplify_scan(g95_expr *e, g95_expr *c, g95_expr *b) {
int index, len, lenc;
g95_expr *result;

    if (e->type != EXPR_CONSTANT || c->type != EXPR_CONSTANT ||
	(b != NULL && b->type != EXPR_CONSTANT))
	return NULL;

    len  = e->value.character.length;
    lenc = c->value.character.length;

    if (b != NULL && b->value.logical) {
	index = e->value.character.length - 1;

	while(index >= 0) {
	    if (contains_char(e->value.character.string[index],
			      c->value.character.string,
			      c->value.character.length)) {
		index++;
		goto done;
	    }

	    index--;
	}

    } else {
	index = 0;
	while(index < e->value.character.length) {
	    if (contains_char(e->value.character.string[index],
			      c->value.character.string,
			      c->value.character.length)) {
		index++;
		goto done;
	    }

	    index++;
	}
    }

    index = 0;

done:
    result = g95_constant_result(BT_INTEGER, -1, &e->where);
    result->value.integer = big_clone(int_to_bi(index));
    big_permanent(result->value.integer);

    return result;
}



g95_expr *g95_simplify_selected_char_kind(g95_expr *name) {

    if (name->type != EXPR_CONSTANT)
	return NULL;

    if (string_compare(name, "default") ||
	string_compare(name, "ascii"))
	return g95_int_expr(g95_default_character_kind());

    return g95_int_expr(-1);
}



g95_expr *g95_simplify_selected_int_kind(g95_expr *e) {
int i, kind, range;
g95_expr *result;

    if (e->type != EXPR_CONSTANT || g95_extract_int(e, &range) != NULL)
	return NULL;

    kind = INT_MAX;

    for(i=0; g95_integer_kinds[i].kind!=0; i++)
	if (g95_integer_kinds[i].range >= range &&
	    g95_integer_kinds[i].kind < kind) kind = g95_integer_kinds[i].kind;

    if (kind == INT_MAX)
	kind = -1;

    result = g95_int_expr(kind);
    result->where = e->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}



g95_expr *g95_simplify_selected_real_kind(g95_expr *p, g95_expr *q) {
int range, precision, i, kind, found_precision, found_range;
g95_expr *result;

    if (p == NULL)
	precision = 0;

    else if (p->type != EXPR_CONSTANT ||
	     g95_extract_int(p, &precision) != NULL)
	return NULL;

    if (q == NULL)
	range = 0;

    else if (q->type != EXPR_CONSTANT || g95_extract_int(q, &range) != NULL)
	return NULL;

    kind = INT_MAX;
    found_precision = 0;
    found_range = 0;

    for(i=0; g95_real_kinds[i].kind!=0; i++) {
	if (g95_real_kinds[i].precision >= precision)
	    found_precision = 1;

	if (g95_real_kinds[i].range >= range)
	    found_range = 1;

	if (g95_real_kinds[i].precision >= precision &&
	    g95_real_kinds[i].range >= range &&
	    g95_real_kinds[i].kind < kind)
	    kind = g95_real_kinds[i].kind;
    }

    if (kind == INT_MAX) {
	kind = 0;

	if (!found_precision)
	    kind = -1;

	if (!found_range)
	    kind -= 2;
    }

    result = g95_int_expr(kind);
    result->where = (p != NULL) ? p->where : q->where;
    result->ts.kind = g95_default_integer_kind(1);

    return result;
}



g95_expr *g95_simplify_set_exponent(g95_expr *x, g95_expr *i) {
g95_expr *result;
int e;

    if (x->type != EXPR_CONSTANT || i->type != EXPR_CONSTANT)
	return NULL;

    if (g95_extract_int(i, &e) != NULL) {
	g95_error("EXPONENT argument is too large at %L", &i->where);
	return &g95_bad_expr;
    }

    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_set_exponent(x->value.real, e));
    big_permanent(result->value.real);

    return result;
}



g95_expr *g95_simplify_shape(g95_expr *source, g95_expr *kind) {
bignum shape[G95_MAX_DIMENSIONS];
g95_expr *result, *e;
g95_array_spec *as;
g95_array_ref *ar;
g95_typespec ts;
int n, rank;

    g95_clear_ts(&ts);    
    ts.type = BT_INTEGER;

    if (kind == NULL)
	ts.kind = g95_default_integer_kind(0);

    else
	g95_extract_int(kind, &ts.kind);

    if (source->rank == 0)
	return g95_start_constructor(&ts, &source->where);

    if (source->type != EXPR_VARIABLE || source->rank < 0)
	return NULL;

    g95_find_array_ref(source, &ar, &as, 1);

    if (g95_array_ref_shape(ar, as, shape, &rank) == FAILURE)
	return NULL;

    result = g95_start_constructor(&ts, &source->where);

    for(n=0; n<rank; n++) {
	e = g95_constant_result(BT_INTEGER, ts.kind, &source->where);

	e->value.integer = big_clone(shape[n]);
	big_permanent(e->value.integer);

	g95_append_constructor(result, e);
    }

    return result;
}



g95_expr *g95_simplify_size(g95_expr *array, g95_expr *dim, g95_expr *kind) {
bignum s, shape[G95_MAX_DIMENSIONS];
g95_array_spec *as;
g95_array_ref *ar;
g95_expr *result;
int k, n, d;

    if (array->type == EXPR_VARIABLE && array->rank == -1)
	g95_variable_rank(array);

    if (array->rank == 0) {
	g95_error("Argument of SIZE intrinsic at %L must be an array",
		  &array->where);
	return &g95_bad_expr;
    }

    if (dim == NULL) {
	s = g95_array_size(array);
	if (s == NULL)
	    return NULL;

    } else if (dim->type != EXPR_CONSTANT)
	return NULL;

    else {
	d = bi_to_int(dim->value.integer);
	if (d < 1 || d > array->rank) {
	    g95_error("DIM parameter of SIZE at %L is out of range",
		      &dim->where);
	    return &g95_bad_expr;
	}

	d--;
	if (array->shape != NULL)
	    s = array->shape[d];

	else {
	    if (array->type != EXPR_VARIABLE)
		return NULL;

	    g95_find_array_ref(array, &ar, &as, 1);

	    if (g95_array_ref_shape(ar, as, shape, &n) == FAILURE)
		return NULL;

	    s = big_copy(shape[d]);

	    for(d=0; d<as->rank; d++)
		big_free(shape[d]);
	}
    }

    if (kind == NULL)
	k = -1;

    else
	g95_extract_int(kind, &k);

    result = g95_constant_result(BT_INTEGER, k, &array->where);

    result->value.integer = big_clone(s);
    big_permanent(result->value.integer);
    big_free(s);

    return result;
}



g95_expr *g95_simplify_sign(g95_expr *x, g95_expr *y) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);

    switch(x->ts.type) {
    case BT_INTEGER:
	result->value.integer = bi_abs(x->value.integer);
	if (bi_is_negative(y->value.integer))
	    result->value.integer = bi_negate(result->value.integer);

	result->value.integer = big_clone(result->value.integer);
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	result->value.real = big_clone(x->value.real);
	result->value.real->real_sign = y->value.real->real_sign;
	big_permanent(result->value.real);
	break;

    default:
	g95_internal_error("Bad type in g95_simplify_sign");
    }

    return result;
}



g95_expr *g95_simplify_sin(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);
    g95_arithmetic_locus = &x->where;

    switch(x->ts.type) {
    case BT_REAL: 
	result->value.real = big_clone(bg_sine(x->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	result->value.complex.r =
	    big_clone(bg_multiply(bg_sine(x->value.complex.r),
				  bg_hyperbolic_cosine(x->value.complex.i)));

	result->value.complex.i =
	    big_clone(bg_multiply(bg_cosine(x->value.complex.r),
				  bg_hyperbolic_sine(x->value.complex.i)));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);

	break;

    default:
	g95_internal_error("in g95_simplify_sin(): Bad type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_sinh(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &x->where;

    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_hyperbolic_sine(x->value.real));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;

    return result;
}



/* simplify_sngl()-- The argument is always a double precision real
 * that is converted to single precision. */

g95_expr *g95_simplify_sngl(g95_expr *a) {
g95_expr *result;
int kind;

    if (a->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &a->where;

    kind = g95_default_real_kind(1);
    result = g95_constant_result(BT_REAL, kind, &a->where);
    result->value.real = big_clone(bg_convert(a->value.real,
					      g95_get_ff(kind)));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_spacing(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(BT_REAL, x->ts.kind, &x->where);
    result->value.real = big_clone(bg_spacing(x->value.real));
    big_permanent(result->value.real);

    return result;
}



g95_expr *g95_simplify_sqrt(g95_expr *e) {
g95_expr *result;
bignum w, t;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);
    g95_arithmetic_locus = &e->where;

    switch(e->ts.type) {
    case BT_REAL:
	result->value.real = big_clone(bg_sqrt(e->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	/* Formula taken from Numerical Recipes to avoid over- and underflow */

	if (bg_compare_int(e->value.complex.r, 0) == 0 &&
	    bg_compare_int(e->value.complex.i, 0) == 0) {
	    result->value.complex.r = bg_from_int(0, e->value.complex.r->ff);
	    result->value.complex.i = bg_from_int(0, e->value.complex.r->ff);

	} else {
	    if (bg_compare_ge(bg_abs(e->value.complex.r),
			      bg_abs(e->value.complex.i))) {
		t = bg_divide(e->value.complex.i, e->value.complex.r);
		w = bg_multiply(big_copy(t), big_copy(t));
		big_free(t);

		t = bg_divide_int(bg_add_int(bg_sqrt(bg_add_int(w, 1)), 1), 2);
		w = bg_multiply(bg_sqrt(bg_abs(e->value.complex.r)),
				bg_sqrt(t));

	    } else {
		t = bg_divide(e->value.complex.r, e->value.complex.i);
		w = bg_sqrt(bg_add_int(bg_multiply(big_copy(t),
						   big_copy(t)), 1));

		t = bg_sqrt(bg_divide_int(bg_add(w, bg_abs(t)), 2));
		w = bg_multiply(bg_sqrt(bg_abs(e->value.complex.i)), t);
	    }

	    if (bg_compare_int(e->value.complex.r, 0) >= 0) {
		result->value.complex.r = w;
		result->value.complex.i =
		    bg_divide(e->value.complex.i,
			      bg_multiply_int(big_copy(w), 2));
	    } else {
		result->value.complex.r =
		    bg_divide(bg_abs(e->value.complex.i),
			      bg_multiply_int(big_copy(w), 2));

		if (bg_compare_int(e->value.complex.i, 0) >= 0)
		    result->value.complex.i = w;

		else
		    result->value.complex.i = bg_negate(w);
	    }
	}

	result->value.complex.r = big_clone(result->value.complex.r);
	result->value.complex.i = big_clone(result->value.complex.i);

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);
	break;

    default:
	g95_internal_error("invalid argument of SQRT at %L", &e->where);
    }

    g95_arithmetic_locus = NULL;
    return result;
}



g95_expr *g95_simplify_tan(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    g95_arithmetic_locus = &x->where;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);
    result->value.real =
	big_clone(bg_divide(bg_sine(x->value.real), bg_cosine(x->value.real)));

    big_permanent(result->value.real);
    g95_arithmetic_locus = NULL;

    return result;
}



g95_expr *g95_simplify_tanh(g95_expr *x) {
g95_expr *result;

    if (x->type != EXPR_CONSTANT)
	return NULL;

    result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);

    g95_arithmetic_locus = &x->where;

    result->value.real =
	big_clone(bg_divide(bg_hyperbolic_sine(x->value.real),
			    bg_hyperbolic_cosine(x->value.real)));

    big_permanent(result->value.real);
    g95_arithmetic_locus = NULL;

    return result;
}



g95_expr *g95_simplify_tiny(g95_expr *e) {
g95_expr *result;

    result = g95_constant_result(BT_REAL, e->ts.kind, &e->where);
    result->value.real = big_clone(bg_tiny(g95_get_ff(e->ts.kind)));

    big_permanent(result->value.real);
    return result;
}


/* transfer_pack()-- Pack a scalar value.  Return nonzero if something
 * goes wrong. */

static int transfer_pack(g95_expr *source, char *mem) {
g95_constructor *cons;
bignum e, elements;
g95_component *c;
g95_expr *value;
int rc, size;

    if (source->type == EXPR_NULL) {
	memset(mem, '\0', sizeof(void *));   /* Assume host == target */
	return 0;
    }
  
    rc = 0;

    switch(source->ts.type) {
    case BT_INTEGER:
	g95_pack_int(source->value.integer, source->ts.kind, mem);
	break;

    case BT_REAL:
	g95_pack_real(source->value.real, mem);
	break;

    case BT_COMPLEX:
	g95_pack_real(source->value.complex.r, mem);
	g95_pack_real(source->value.complex.i, mem+source->ts.kind);
	break;

    case BT_LOGICAL:
	g95_pack_logical(source->value.logical, source->ts.kind, mem);
	break;

    case BT_CHARACTER:
	memcpy(mem, source->value.character.string,
	       source->value.character.length);
	break;

    case BT_DERIVED:
	if (source->ts.type != EXPR_STRUCTURE)
	    return 1;

	cons = source->value.constructor.c;

	for(c=source->ts.derived->components; c; c=c->next, cons=cons->next) {
	    size = g95_front_ts_size(&c->ts);

	    if (c->as == NULL) {
		rc = transfer_pack(cons->expr, mem);
		mem += size;

	    } else {
		elements = g95_array_spec_size(c->as);
		e = bi_0;

		while(bi_compare(big_copy(e), big_copy(elements)) < 0) {

		    switch(cons->expr->type) {
		    case EXPR_ARRAY:
			value = g95_get_array_element(cons->expr, big_copy(e));
			break;

		    case EXPR_CONSTANT:
			value = g95_copy_expr(cons->expr);
			break;

		    default:
			value = NULL;
			break;
		    }

		    if (value == NULL) {
			rc = 1;
			break;
		    }

		    rc = transfer_pack(value, mem);
		    g95_free_expr(value);
		    e = bi_add(e, bi_1);
		    mem += size;
		}

		big_free(e);
		big_free(elements);
	    }

	    if (rc != 0)
		break;
	}

	break;

    default:
	g95_internal_error("transfer_pack(): Bad type");
	break;
    }

    return rc;
}


/* transfer_unpack()-- Unpack a scalar value. */

static g95_expr *transfer_unpack(char *mem, g95_typespec *ts,
				 g95_locus *where) {
g95_constructor *tail, *ac_head, *ac_tail;
g95_expr *expr, *result;
bignum e, elements;
g95_component *c;
int i, size, len;

    result = g95_constant_result(ts->type, ts->kind, where);
    result->ts = *ts;

    switch(ts->type) {
    case BT_INTEGER:
	result->value.integer = big_clone(g95_unpack_int(mem, ts->kind));
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	result->value.real =
	    big_clone(g95_unpack_real(mem, g95_get_ff(ts->kind)));

	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	result->value.complex.r =
	    big_clone(g95_unpack_real(mem, g95_get_ff(ts->kind)));

	result->value.complex.i =
	    big_clone(g95_unpack_real(mem + ts->kind, g95_get_ff(ts->kind)));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);
	break;

    case BT_LOGICAL:
	result->value.logical = g95_unpack_logical(mem, ts->kind);
	break;

    case BT_CHARACTER:
	if (ts->cl->length == NULL || ts->cl->length->type != EXPR_CONSTANT) {
	    g95_free(result);
	    result = NULL;

	} else {
	    len = bi_to_int(ts->cl->length->value.integer);

	    result->value.character.string = g95_getmem(len+1);
	    result->value.character.length = len;

	    memmove(result->value.character.string, mem, len);
	}

	break;

    case BT_DERIVED:
	result->type = EXPR_STRUCTURE;
	result->symbol = ts->derived;
	tail = NULL;

	for(c=ts->derived->components; c; c=c->next) {
	    size = g95_front_ts_size(&c->ts);

	    if (c->as == NULL) {
		expr = transfer_unpack(mem, &c->ts, where);
		mem += size;

	    } else {
		elements = g95_array_spec_size(c->as);
		ac_head = ac_tail = NULL;
		e = bi_0;

		while(bi_compare(big_copy(e), big_copy(elements)) < 0) {
		    expr = transfer_unpack(mem, &c->ts, where);
		    if (expr == NULL)
			return NULL;

		    if (ac_head == NULL)
			ac_head = ac_tail = g95_get_constructor();

		    else {
			ac_tail->next = g95_get_constructor();
			ac_tail = ac_tail->next;
		    }

		    ac_tail->expr = expr;
		    e = bi_add(e, bi_1);
		    mem += size;
		}

		expr = g95_get_expr();
		expr->type = EXPR_ARRAY;

		expr->rank = c->as->rank;
		expr->where = *where;

		expr->value.constructor.c = ac_head;
		expr->shape = g95_get_shape(expr->rank);

		for(i=0; i<c->as->rank; i++) {
		    expr->shape[i] = big_clone(c->as->lower[i]->value.integer);
		    big_permanent(expr->shape[i]);
		}

		big_free(e);
		big_free(elements);
	    }

	    if (tail == NULL)
		result->value.constructor.c = tail = g95_get_constructor();

	    else {
		tail->next = g95_get_constructor();
		tail = tail->next;
	    }

	    tail->expr = expr;
	}

	break;

    default:
	result = NULL;
	g95_internal_error("transfer_unpack(): Bad type");
    }

    return result;
}



g95_expr *g95_simplify_transfer(g95_expr *source, g95_expr *mold,
				g95_expr *size) {
int source_size, source_elements, source_element_size, 
    mold_size, mold_elements, mold_element_size, i, j, n;
g95_expr *result, *e;
char *mem, *p;
bignum b;

    if (!g95_is_constant_expr(source) ||
	(size != NULL && size->type != EXPR_CONSTANT))
	return NULL;

    if (source->type == EXPR_STRUCTURE && source->ts.type == BT_DERIVED &&
	g95_pointer_array_component(source->ts.derived))
	return NULL;

    if (mold->ts.type == BT_CHARACTER && mold->ts.cl->length == NULL)
	return NULL;

    if (source->rank == 0)
	source_elements = 1;

    else {
	b = g95_array_size(source);
	if (b == NULL)
	    return NULL;

	source_elements = bi_to_int(b);
    }

    result = NULL;

    source_element_size = (source->type == EXPR_CONSTANT &&
			   source->ts.type == BT_CHARACTER)
	? source->value.character.length
	: g95_front_ts_size(&source->ts);

    source_size = source_elements * source_element_size;

    if (size != NULL)
	mold_elements = bi_to_int(size->value.integer);

    else if (mold->rank == 0)
	mold_elements = 1;

    else {
	b = g95_array_size(mold);
	if (b == NULL)
	    return NULL;

	mold_elements = bi_to_int(b);
    }

    mold_element_size = g95_front_ts_size(&mold->ts);
    mold_size = mold_elements * mold_element_size;

    mem = g95_getmem((mold_size > source_size) ? mold_size : source_size);

    if (source->rank == 0)
	j = transfer_pack(source, mem);

    else {
	j = 0;
	p = mem;

	for(i=0; i<source_elements; i++) {
	    result = g95_get_array_element(source, int_to_bi(i));
	    if (result == NULL)
		goto done;

	    j |= transfer_pack(result, p);
	    p += source_element_size;
	    g95_free_expr(result);
	}
    }

    if (j != 0)
	goto done;

    if (mold_size > source_size)
	memset(mem+source_size, '\0', mold_size - source_size);

    if (mold->rank == 0 && size == NULL)
	result = transfer_unpack(mem, &mold->ts, &source->where);

    else {
	result = g95_start_constructor(&mold->ts, &mold->where);

	j = source_size / mold_element_size;  /* Elements physically present */
	
	n = (size == NULL)
	    ? j
	    : bi_to_int(size->value.integer);

	p = mem;

	if (size != NULL && j < n)
	    g95_warning(148, "Not enough data to produce the requested array "
			"size in TRANSFER function at %L", &size->where);

	result->shape = g95_get_shape(1);
	result->shape[0] = int_to_bi(n);
	big_permanent(result->shape[0]);

	for(i=0; i<n; i++) {
	    e = transfer_unpack(p, &mold->ts, &source->where);
	    g95_append_constructor(result, e);

	    if (i < j)
		p += mold_element_size;
	}
    }

done:
    g95_free(mem);
    return result;
}


g95_expr *g95_simplify_transpose(g95_expr *e) {
g95_expr *result, *c;
int i, j, ext0, ext1;
bignum n;

    if (g95_simplify_mode != SIMP_REGULAR && g95_option.fmode != 0)
	return NULL;

    if (e->type == EXPR_CONSTANT)
	return g95_copy_expr(e);

    if (e->type != EXPR_ARRAY || e->shape == NULL)
	return NULL;

    result = g95_start_constructor(&e->ts, &e->where);
    result->rank = 2;
    result->shape = g95_get_shape(2);

    result->shape[0] = big_clone(e->shape[1]);
    big_permanent(result->shape[0]);

    result->shape[1] = big_clone(e->shape[0]);
    big_permanent(result->shape[1]);

    ext0 = bi_to_int(e->shape[0]);
    ext1 = bi_to_int(e->shape[1]);

    for(j=0; j<ext0; j++)
	for(i=0; i<ext1; i++) {
	    n = bi_int_multiply(e->shape[0], i);
	    n = bi_add(n, int_to_bi(j));

	    c = g95_get_array_element(e, n);
	    if (c == NULL) {
		g95_free_expr(result);
		return NULL;
	    }

	    g95_append_constructor(result, c);
	}

    return result;
}



g95_expr *g95_simplify_trim(g95_expr *e) {
int count, i, len, lentrim;
g95_expr *result;

    if (e->type != EXPR_CONSTANT)
	return NULL;

    len = e->value.character.length;

    result = g95_constant_result(BT_CHARACTER, e->ts.kind, &e->where);

    for (count=0, i=1; i<=len; ++i) {
	if (e->value.character.string[len-i] != ' ')
	    break;

	count++;
    }

    lentrim = len-count;

    result->value.character.length = lentrim;
    result->value.character.string = g95_getmem(lentrim+1);

    for(i=0; i<lentrim; i++)
	result->value.character.string[i] = e->value.character.string[i];

    result->value.character.string[lentrim] = '\0';   /* For debugger */

    return result;
}



g95_expr *g95_simplify_ubound(g95_expr *array, g95_expr *dim, g95_expr *kind) {

    if (array->rank == 0) {
	g95_error("Argument of UBOUND at %L must be an array", &array->where);
	return &g95_bad_expr;
    }

    return simplify_bound(array, dim, kind, 1);
}



g95_expr *g95_simplify_verify(g95_expr *s, g95_expr *set, g95_expr *b) {
size_t index, len, lenset, m;
g95_expr *result;
int back;
char c;

    if (s->type != EXPR_CONSTANT || set->type != EXPR_CONSTANT ||
	(b != NULL && b->type != EXPR_CONSTANT))
	return NULL;

    back = (b != NULL) && b->value.logical;

    len    = s->value.character.length;
    lenset = set->value.character.length;

    if (len == 0) {
	index = 0;
	goto done;
    }

    if (lenset == 0) {
	index = back ? s->value.character.length : 1;
	goto done;
    }

    if (back == 0) {
	if (lenset == 0) {
	    index = len;
	    goto done;
	}

	index = strspn(s->value.character.string,
		       set->value.character.string) + 1;

	if (index > len)
	    index = 0;

    } else {
	if (lenset == 0) {
	    index = 1;
	    goto done;
	}

	index = s->value.character.length - 1;
	while(index >= 0) {
	    c = s->value.character.string[index];

	    for(m=0; m<set->value.character.length; m++)
		if (c == set->value.character.string[m])
		    break;

	    if (m == set->value.character.length) {
		index++;
		goto done;
	    }

	    index--;
	}

	index = 0;
    }
    
done:
    result = g95_constant_result(BT_INTEGER, -1, &s->where);
    result->value.integer = big_clone(int_to_bi(index));
    big_permanent(result->value.integer);

    return result;
}



/* invert_table()-- Given a collating table, create the inverse table */

static void invert_table(int *table, int *xtable) {
int i;

    for(i=0; i<256; i++)
	if (table[i] == 0)
	    table[i] = i;

    for(i=0; i<256; i++)
	xtable[i] = 0;

    for(i=0; i<256; i++)
	xtable[table[i]] = i;
}



void g95_simplify_init_1(void) {

    invert_table(ascii_table, xascii_table);
}



void g95_simplify_done_1(void) {

}

