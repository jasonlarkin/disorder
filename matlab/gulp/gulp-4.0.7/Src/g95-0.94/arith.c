/* Compiler arithmetic
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

/* arith.c-- Since target arithmetic must be done on the host, there
 * has to be some way of evaluating arithmetic expressions as the host
 * would evaluate them.  This file is the interface between the upper
 * level compiler functions and low level target arithmetic. */

#include "g95.h"
#include <string.h>



/* g95_arith_init_1()-- Get things ready to do math. */

void g95_arith_init_1(void) {
g95_integer_info *p;

/* Precalculate the minimum/maximum values for each kind. */

    for(p=g95_integer_kinds; p->kind != 0; p++) {
	p->min = bi_negate(bi_power(bi_2, int_to_bi(p->bit_size-1)));
	big_permanent(p->min);

	p->max = bi_int_add(bi_negate(p->min), -1);
	big_permanent(p->max);

	p->umax = bi_int_add(bi_power(bi_2, int_to_bi(p->bit_size)), -1);
	big_permanent(p->umax);

	p->range = (int) (0.301029999 * (double) p->digits);
    }
}



/* g95_arith_done_1()-- Get rid of numeric constants. */

void g95_arith_done_1(void) {
g95_integer_info *ip;

    for(ip=g95_integer_kinds; ip->kind; ip++) {
	big_depermanent(ip->min);
	big_free(ip->min);

	big_depermanent(ip->max);
	big_free(ip->max);

	big_depermanent(ip->umax);
	big_free(ip->umax);
    }
}



static int validate_real(int kind) {
int i;

    for(i=0;; i++) {
	if (g95_real_kinds[i].kind == 0) {
	    i = -1;
	    break;
	}

	if (g95_real_kinds[i].kind == kind)
	    break;
    }

    return i;
}



static int validate_logical(int kind) {
int i;

    if (g95_option.l1 == 1 && kind == 1)
	return 0;

    for(i=0;; i++) {
	if (g95_logical_kinds[i].kind == 0) {
	    i = -1;
	    break;
	}

	if (g95_logical_kinds[i].kind == kind)
	    break;
    }

    return i;
}


/* validate_integer()-- Make sure that a valid kind is present.
 * Returns an index into the g95_integer_kinds array, -1 if the kind
 * is not present. */

static int validate_integer(int kind) {
int i;

    for(i=0;; i++) {
	if (g95_integer_kinds[i].kind == 0) {
	    i = -1;
	    break;
	}

	if (g95_integer_kinds[i].kind == kind)
	    break;
    }

    return i;
}



static int validate_character(int kind) {

    return (kind == g95_default_character_kind())
	? 0 : -1;
}



/* g95_validate_kind()-- Validate a kind given a basic type.  The
 * return value is the same for the child functions, with -1
 * indicating nonexistence of the type */

int g95_validate_kind(bt type, int kind) {
int rc;

    switch(type) {
    case BT_REAL:     /* Fall through */
    case BT_COMPLEX:    rc = validate_real(kind);       break;
    case BT_INTEGER:    rc = validate_integer(kind);    break;
    case BT_LOGICAL:    rc = validate_logical(kind);    break;
    case BT_CHARACTER:  rc = validate_character(kind);  break;

    default:
	g95_internal_error("g95_validate_kind(): Got bad type");
    }

    return rc;
}



int g95_default_integer_kind(int flag) {
int k;

    k = g95_integer_kinds[0].kind;

    if (flag && g95_option.default_integer != 0)
	k = g95_option.default_integer;

    return k;
}



int g95_pointer_integer_kind(void) {

    return sizeof(void *);
}



int g95_default_real_kind(int flag) {
int k;

    k = g95_real_kinds[0].kind;

    if (flag && g95_option.r_value)
	k = g95_option.r_value;

    return k;
}



int g95_default_double_kind(void) {
int k;

    k = g95_default_real_kind(0); 
    if (validate_real(2*k) > 0)
	k = 2*k;

    return k;
}



int g95_default_logical_kind(void) {
int k;

    k = g95_logical_kinds[0].kind;

    if (g95_option.default_integer != 0)
	k = g95_option.default_integer;

    return k;
}



int g95_default_complex_kind(void) {

    return g95_default_real_kind(1);
}



int g95_extended_kind(void) {

    return 10;
}



int g95_quad_kind(void) {

    return 16;
}



int g95_default_character_kind(void) {

    return 1;
}



/* g95_constant_result()-- Function to return a constant expression node
 * of a given type and kind. */

g95_expr *g95_constant_result(bt type, int kind, g95_locus *where) {
g95_expr *result;

    if (where == NULL)
	g95_internal_error("g95_constant_result(): bad where");

    result = g95_get_expr();

    if (kind == -1)
	switch(type) {
	case BT_INTEGER:    kind = g95_default_integer_kind(0);   break;
	case BT_LOGICAL:    kind = g95_default_logical_kind();    break;
	case BT_COMPLEX:
	case BT_REAL:       kind = g95_default_real_kind(1);      break;
	case BT_CHARACTER:  kind = g95_default_character_kind();  break;
	default:           break;
	}

    result->type    = EXPR_CONSTANT;
    result->ts.type = type;
    result->ts.kind = kind;
    result->where   = *where;

    return result;
}



/* g95_convert_complex()-- Convert a pair of real, constant expression
 * nodes to a single complex expression node. */

g95_expr *g95_convert_complex(g95_expr *real, g95_expr *imag, int kind) {
g95_expr *e;

    e = g95_constant_result(BT_COMPLEX, kind, &real->where);

    e->value.complex.r = big_clone(real->value.real);
    e->value.complex.i = big_clone(imag->value.real);

    big_permanent(e->value.complex.r);
    big_permanent(e->value.complex.i);

    return e;
}



/* g95_convert_real()-- Convert a real string to an expression node. */

g95_expr *g95_convert_real(char *buffer, int kind, g95_locus *where) {
g95_expr *e;

    e = g95_constant_result(BT_REAL, kind, where);
    g95_arithmetic_locus = where;
    e->value.real = big_clone(bg_from_string(buffer, g95_get_ff(kind)));
    big_permanent(e->value.real);

    g95_arithmetic_locus = NULL;
    return e;
}



/* g95_convert_integer()-- Convert an integer string to an expression
 * node */

g95_expr *g95_convert_integer(char *buffer, int kind, int radix,
			      g95_locus *where) {
g95_expr *e;

    e = g95_constant_result(BT_INTEGER, kind, where);
    if (*buffer == '+')
	buffer++;

    e->value.integer = big_clone(bi_from_string(buffer, radix));
    big_permanent(e->value.integer);

    return e;
}



/* g95_range_check()-- Make sure a constant integer expression is
 * within the range for its kind.  Returns nonzero if there is a range
 * problem, zero if OK. */

int g95_range_check(g95_expr *e) {
bignum min, max;
int i;

    if (e->type != EXPR_CONSTANT || e->ts.type != BT_INTEGER)
	return 0;

    i = validate_integer(e->ts.kind);
    if (i == -1)
	g95_internal_error("check_integer_range(): Bad kind");

    if (e->value.integer->typeless) {
	min = bi_0;
	max = g95_integer_kinds[i].umax;
    } else {
	min = g95_integer_kinds[i].min;
	max = g95_integer_kinds[i].max;
    }

    return (bi_compare(e->value.integer, min) < 0 ||
	    bi_compare(e->value.integer, max) > 0);
}



/* check_integer_overflow()-- Check an integer expression for
 * overflow. */

static void check_integer_overflow(g95_expr *e) {
int flag;

    flag = g95_range_check(e);

    if (flag)
	g95_warning(131, "Integer overflow at %L", &e->where);

    return;
}



/* g95_int2int()-- Convert integers to integers */

g95_expr *g95_int2int(g95_expr *src, int kind) {
g95_expr *result;

    check_integer_overflow(src);

    result = g95_constant_result(BT_INTEGER, kind, &src->where);
    result->value.integer = big_clone(src->value.integer);
    big_permanent(result->value.integer);

    check_integer_overflow(result);

    return result;
}



/* g95_int2real()-- Convert integers to reals */

g95_expr *g95_int2real(g95_expr *src, int kind) {
g95_expr *result;
g95_ff *ff;

    check_integer_overflow(src);

    result = g95_constant_result(BT_REAL, kind, &src->where);

    g95_arithmetic_locus = &src->where;

    ff = g95_get_ff(kind);
    result->value.real = big_clone(bg_from_bi(src->value.integer, ff));
    big_permanent(result->value.real);
    g95_arithmetic_locus = NULL;

    return result;
}



/* g95_int2complex()-- Convert default integer to default complex */

g95_expr *g95_int2complex(g95_expr *src, int kind) {
g95_expr *result;
g95_ff *ff;

    check_integer_overflow(src);

    result = g95_constant_result(BT_COMPLEX, kind, &src->where);

    g95_arithmetic_locus = &src->where;  

    ff = g95_get_ff(kind);
    result->value.complex.r = big_clone(bg_from_bi(src->value.integer, ff));
    big_permanent(result->value.complex.r);

    result->value.complex.i = big_clone(bg_from_int(0, ff));
    big_permanent(result->value.complex.i);

    g95_arithmetic_locus = NULL;
    return result;
}



/* g95_real2int()-- Convert default real to default integer */

g95_expr *g95_real2int(g95_expr *src, int kind) {
g95_expr *result;

    result = g95_constant_result(BT_INTEGER, kind, &src->where);
    result->value.integer = big_clone(bg_to_bi(bg_trunc(src->value.real)));
    big_permanent(result->value.integer);

    check_integer_overflow(result);

    return result;
}



/* g95_real2real()-- Convert real to real */

g95_expr *g95_real2real(g95_expr *src, int kind) {
g95_expr *result;
g95_ff *ff;

    g95_arithmetic_locus = &src->where;
    ff = g95_get_ff(kind);

    result = g95_constant_result(BT_REAL, kind, &src->where);
    result->value.real = big_clone(bg_convert(src->value.real, ff));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



/* g95_real2complex()-- Convert real to complex. */

g95_expr *g95_real2complex(g95_expr *src, int kind) {
g95_expr *result;
g95_ff *ff;

    g95_arithmetic_locus = &src->where;
    ff = g95_get_ff(kind);

    result = g95_constant_result(BT_COMPLEX, kind, &src->where);
    result->value.complex.r = big_clone(bg_convert(src->value.real, ff));
    result->value.complex.i = big_clone(bg_from_int(0, ff));

    big_permanent(result->value.complex.r);
    big_permanent(result->value.complex.i);

    g95_arithmetic_locus = NULL;
    return result;
}



/* g95_complex2int()-- Convert complex to integer */

g95_expr *g95_complex2int(g95_expr *src, int kind) {
g95_expr *result;

    result = g95_constant_result(BT_INTEGER, kind, &src->where);
    result->value.integer =
	big_clone(bg_to_bi(bg_trunc(src->value.complex.r)));
    big_permanent(result->value.integer);

    check_integer_overflow(result);

    return result;
}



/* g95_complex2real()-- Convert complex to real */

g95_expr *g95_complex2real(g95_expr *src, int kind) {
g95_expr *result;
g95_ff *ff;

    g95_arithmetic_locus = &src->where;
    ff = g95_get_ff(kind);
    result = g95_constant_result(BT_REAL, kind, &src->where);
    result->value.real = big_clone(bg_convert(src->value.complex.r, ff));
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}



/* g95_complex2complex()-- Convert complex to complex */

g95_expr *g95_complex2complex(g95_expr *src, int kind) {
g95_expr *result;
g95_ff *ff;

    result = g95_constant_result(BT_COMPLEX, kind, &src->where);

    g95_arithmetic_locus = &src->where;

    ff = g95_get_ff(kind);
    result->value.complex.r = big_clone(bg_convert(src->value.complex.r, ff));
    result->value.complex.i = big_clone(bg_convert(src->value.complex.i, ff));

    big_permanent(result->value.complex.r);
    big_permanent(result->value.complex.i);

    g95_arithmetic_locus = NULL;
    return result;
}



/* g95_log2log()-- Logical kind conversion. */

g95_expr *g95_log2log(g95_expr *src, int kind) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, kind, &src->where);
    result->value.logical = src->value.logical;

    return result;
}



/* Low-level arithmetic functions.  All of these subroutines assume
 * that all operands are of the same type and return an operand of the
 * same type. */

static g95_expr *arith_eqv(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2), 
				 &op1->where);
    result->value.logical = op1->value.logical == op2->value.logical;

    return result;
}



static g95_expr *arith_neqv(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2), 
				 &op1->where);
    result->value.logical = op1->value.logical != op2->value.logical;

    return result;
}



static g95_expr *arith_not(g95_expr *op1) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, op1->ts.kind, &op1->where);
    result->value.logical = !op1->value.logical;

    return result;
}



static g95_expr *arith_and(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2), 
				 &op1->where);
    result->value.logical = op1->value.logical && op2->value.logical;

    return result;
}



static g95_expr *arith_or(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2), 
				 &op1->where);
    result->value.logical = op1->value.logical || op2->value.logical;

    return result;
}



/* g95_arith_uplus()-- It may seem silly to have a subroutine that
 * actually computes the unary plus of a constant, but it prevents us
 * from making exceptions in the code elsewhere. */

static g95_expr *arith_uplus(g95_expr *op1) {

    check_integer_overflow(op1);

    return g95_copy_expr(op1);
}



static g95_expr *arith_uminus(g95_expr *op1) {
g95_expr *result;

    result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);
    g95_arithmetic_locus = &op1->where;

    switch(op1->ts.type) {
    case BT_INTEGER:
	/* No bounds checking of op1 here */
	result->value.integer = big_clone(bi_negate(op1->value.integer));
	big_permanent(result->value.integer);

	check_integer_overflow(result);
	break;

    case BT_REAL:
	result->value.real = big_clone(bg_negate(op1->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	result->value.complex.r = big_clone(bg_negate(op1->value.complex.r));
	result->value.complex.i = big_clone(bg_negate(op1->value.complex.i));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);
	break;

    default:
	g95_internal_error("arith_uminus(): Bad basic type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



static g95_expr *arith_plus(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);
    g95_arithmetic_locus = &op1->where;

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	result->value.integer =
	    big_clone(bi_add(op1->value.integer, op2->value.integer));
	big_permanent(result->value.integer);

	check_integer_overflow(result);
	break;

    case BT_REAL:
	result->value.real =
	    big_clone(bg_add(op1->value.real, op2->value.real));
	big_permanent(result->value.real);
	break;

  case BT_COMPLEX:
      result->value.complex.r =
	  big_clone(bg_add(op1->value.complex.r, op2->value.complex.r));

      result->value.complex.i =
	  big_clone(bg_add(op1->value.complex.i, op2->value.complex.i));

      big_permanent(result->value.complex.i);
      big_permanent(result->value.complex.r);
      break;

    default:
	g95_internal_error("arith_plus(): Bad basic type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



static g95_expr *arith_minus(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);
    g95_arithmetic_locus = &op1->where;

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	result->value.integer =
	    big_clone(bi_subtract(op1->value.integer, op2->value.integer));
	big_permanent(result->value.integer);

	check_integer_overflow(result);
	break;

    case BT_REAL:
	result->value.real =
	    big_clone(bg_subtract(op1->value.real, op2->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	result->value.complex.r =
	    big_clone(bg_subtract(op1->value.complex.r, op2->value.complex.r));

	result->value.complex.i =
	    big_clone(bg_subtract(op1->value.complex.i, op2->value.complex.i));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);
	break;

    default:
	g95_internal_error("arith_minus(): Bad basic type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



static g95_expr *arith_times(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);
    g95_arithmetic_locus = &op1->where;

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	result->value.integer =
	    big_clone(bi_multiply(op1->value.integer, op2->value.integer));
	big_permanent(result->value.integer);

	check_integer_overflow(result);
	break;

    case BT_REAL:
	result->value.real =
	    big_clone(bg_multiply(op1->value.real, op2->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	result->value.complex.r =
	    big_clone(bg_subtract(bg_multiply(op1->value.complex.r,
					      op2->value.complex.r),
				  bg_multiply(op1->value.complex.i,
					      op2->value.complex.i)));

	result->value.complex.i =
	    big_clone(bg_add(bg_multiply(op1->value.complex.r,
					 op2->value.complex.i),
			     bg_multiply(op1->value.complex.i,
					 op2->value.complex.r)));

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);
	break;

    default:
	g95_internal_error("arith_times(): Bad basic type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



static g95_expr *arith_divide(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
bignum t;

    result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);
    g95_arithmetic_locus = &op1->where;

    switch(op1->ts.type) {
    case BT_INTEGER:
	if (bi_compare(big_copy(op2->value.integer), bi_0) == 0) {
	    g95_zero_division(&op1->where);

	    result->value.integer = big_clone(bi_huge(op1->ts.kind));
	    result->value.integer->sign = op1->value.integer->sign;
	} else
	    result->value.integer =
		big_clone(bi_divide(op1->value.integer, op2->value.integer));

	big_permanent(result->value.integer);
	check_integer_overflow(result);
	break;

    case BT_REAL:
	result->value.real =
	    big_clone(bg_divide(op1->value.real, op2->value.real));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
    /* Trickery from Numerical Recipes to prevent overflow/precision loss */

	if (bg_compare_gt(bg_abs(op2->value.complex.r),
			  bg_abs(op2->value.complex.i))) {

	    t = bg_divide(op2->value.complex.i, op2->value.complex.r);

	    result->value.complex.r =
		bg_add(op1->value.complex.r,
		       bg_multiply(big_copy(t), op1->value.complex.i));

	    result->value.complex.i =
		bg_subtract(op1->value.complex.i,
			    bg_multiply(big_copy(t), op1->value.complex.r));

	    t = bg_add(op2->value.complex.r,
		       bg_multiply(t, op2->value.complex.i));

	    result->value.complex.r = bg_divide(result->value.complex.r,
						big_copy(t));
	    result->value.complex.i = bg_divide(result->value.complex.i, t);

	} else {
	    t = bg_divide(op2->value.complex.r, op2->value.complex.i);

	    result->value.complex.r =
		bg_add(bg_multiply(op1->value.complex.r, big_copy(t)),
		       op1->value.complex.i);

	    result->value.complex.i =
		bg_subtract(bg_multiply(op1->value.complex.i, big_copy(t)),
			    op1->value.complex.r);

	    t = bg_add(bg_multiply(op2->value.complex.r, t),
		       op2->value.complex.i);

	    result->value.complex.r = bg_divide(result->value.complex.r,
						big_copy(t));
	    result->value.complex.i = bg_divide(result->value.complex.i, t);
	}

	result->value.complex.r = big_clone(result->value.complex.r);
	result->value.complex.i = big_clone(result->value.complex.i);

	big_permanent(result->value.complex.r);
	big_permanent(result->value.complex.i);

	break;

    default:
	g95_internal_error("arith_divide(): Bad basic type");
    }

    g95_arithmetic_locus = NULL;
    return result;
}



/* complex_reciprocal()-- Compute the reciprocal of a complex number
 * (guaranteed nonzero) */

static void complex_reciprocal(g95_expr *op) {
bignum n, t;

    if (bg_compare_gt(bg_abs(big_copy(op->value.complex.r)),
		      bg_abs(big_copy(op->value.complex.i)))) {

	t = bg_divide(big_copy(op->value.complex.i),
		      big_copy(op->value.complex.r));

	n = bg_add(op->value.complex.r,
		   bg_multiply(op->value.complex.i, big_copy(t)));

	op->value.complex.r = bg_divide(bg_from_int(1, t->ff), big_copy(n));
	op->value.complex.i = bg_divide(bg_negate(t), n);

    } else {
	t = bg_divide(big_copy(op->value.complex.r),
		      big_copy(op->value.complex.i));

	n = bg_add(bg_multiply(op->value.complex.r, big_copy(t)),
		   op->value.complex.i);

	op->value.complex.r = bg_divide(t, big_copy(n));
	op->value.complex.i = bg_divide(bg_from_int(-1, t->ff), n);
    }

    op->value.complex.r = big_clone(op->value.complex.r);
    op->value.complex.i = big_clone(op->value.complex.i);
}



/* complex_pow_ui()-- Raise a complex number to positive power */

static void complex_pow_ui(bignum sr, bignum si, bignum p, g95_expr *result) {
bignum nr, ni, tr, ti;
g95_ff *ff;

    ff = g95_get_ff(result->ts.kind);

    nr = bg_from_int(1, ff);
    ni = bg_from_int(0, ff);

    while(!bi_is_zero(big_copy(p))) {
	if (bi_is_odd(big_copy(p))) {
	    tr = bg_subtract(bg_multiply(big_copy(nr), big_copy(sr)),
			     bg_multiply(big_copy(ni), big_copy(si)));
	    ti = bg_add(bg_multiply(ni, big_copy(sr)),
			bg_multiply(nr, big_copy(si)));

	    nr = tr;
	    ni = ti;
	}

	tr = bg_subtract(bg_multiply(big_copy(sr), big_copy(sr)),
			 bg_multiply(big_copy(si), big_copy(si)));
	ti = bg_multiply_int(bg_multiply(sr, si), 2);

	sr = tr;
	si = ti;

	p = bi_half(p);
    }

    big_free(sr);
    big_free(si);
    big_free(p);

    result->value.complex.r = big_clone(nr);
    result->value.complex.i = big_clone(ni);
}



/* int_power()-- Raise a number to an integer power */

static g95_expr *int_power(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
g95_ff *ff;

    result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);
    g95_arithmetic_locus = &op1->where;

    if (bi_is_zero(op2->value.integer)) { /* Something to the zeroth power */
	switch(op1->ts.type) {
	case BT_INTEGER:
	    check_integer_overflow(op1);
	    check_integer_overflow(op2);

	    result->value.integer = big_clone(int_to_bi(1));
	    big_permanent(result->value.integer);
	    break;

	case BT_REAL:
	    result->value.real =
		big_clone(bg_from_int(1, g95_get_ff(op1->ts.kind)));
	    big_permanent(result->value.real);
	    break;

	case BT_COMPLEX:
	    ff = g95_get_ff(op1->ts.kind);
	    result->value.complex.r = big_clone(bg_from_int(1, ff));
	    result->value.complex.i = big_clone(bg_from_int(0, ff));

	    big_permanent(result->value.complex.r);
	    big_permanent(result->value.complex.i);
	    break;

	default:
	    g95_internal_error("int_power(): Bad base");
	}

    } else {
	switch(op1->ts.type) {
	case BT_INTEGER:
	    if (bi_compare(op1->value.integer, bi_0) == 0) {
		if (bi_compare(op2->value.integer, bi_0) >= 0) {
		    result->value.integer = big_clone(bi_0);
		    big_permanent(result->value.integer);
		    break;

		} else {
		    result->value.integer = big_clone(bi_huge(op1->ts.kind));
		    big_permanent(result->value.integer);
		    g95_zero_division(&op1->where);
		    return result;
		}
	    }

	    result->value.integer =
		bi_power(op1->value.integer, bi_abs(op2->value.integer));

	    if (bi_is_negative(op2->value.integer))
		result->value.integer =
		    bi_divide(int_to_bi(1), result->value.integer);

	    result->value.integer = big_clone(result->value.integer);
	    big_permanent(result->value.integer);

	    check_integer_overflow(result);
	    break;

	case BT_REAL:
	    result->value.real =
		big_clone(bg_pow(op1->value.real, op2->value.integer));

	    big_permanent(result->value.real);
	    break;

	case BT_COMPLEX:
	    complex_pow_ui(op1->value.complex.r, op1->value.complex.i,
			   bi_abs(op2->value.integer), result);

	    if (bi_is_negative(op2->value.integer))
		complex_reciprocal(result);

	    big_permanent(result->value.complex.r);
	    big_permanent(result->value.complex.i);
	    break;

	default:
	    break;
	}
    }

    g95_arithmetic_locus = NULL;
    return result;
}



/* arith_power()-- Compute an exponential. */

static g95_expr *arith_power(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    if (op2->ts.type == BT_INTEGER)
	return int_power(op1, op2);

    result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);
    g95_arithmetic_locus = &op1->where;

    result->value.real = bg_power(op1->value.real, op2->value.real);
    big_permanent(result->value.real);

    g95_arithmetic_locus = NULL;
    return result;
}


static g95_expr *derived_ne(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);
    result->value.logical =
	op1->value.constructor.ivalue != op2->value.constructor.ivalue;

    return result;
}


static g95_expr *arith_ne(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
int rc;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	rc = (bi_compare(op1->value.integer, op2->value.integer) != 0);
	break;

    case BT_REAL:
	rc = bg_compare_ne(op1->value.real, op2->value.real);
	break;

    case BT_COMPLEX:
	rc = bg_compare_ne(op1->value.complex.r, op2->value.complex.r) &&
	     bg_compare_ne(op1->value.complex.i, op2->value.complex.i);
	break;

    case BT_CHARACTER:
	rc = (g95_compare_string(op1, op2, NULL) != 0);
	break;

    default:
	rc = 0;
	g95_internal_error("arith_ne(): Bad type");
    }

    result->value.logical = rc;
    return result;
}


static g95_expr *derived_eq(g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);
    result->value.logical =
	op1->value.constructor.ivalue == op2->value.constructor.ivalue;

    return result;
}


static g95_expr *arith_eq(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
int rc;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	rc = (bi_compare(op1->value.integer, op2->value.integer) == 0);
	break;

    case BT_REAL:
	rc = bg_compare_eq(op1->value.real, op2->value.real);
	break;

    case BT_COMPLEX:
	rc = bg_compare_eq(op1->value.complex.r, op2->value.complex.r) &&
	     bg_compare_eq(op1->value.complex.i, op2->value.complex.i);
	break;

    case BT_CHARACTER:
	rc = (g95_compare_string(op1, op2, NULL) == 0);
	break;

    default:
	rc = 0;
	g95_internal_error("arith_eq(): Bad type");
    }

    result->value.logical = rc;
    return result;
}



static g95_expr *arith_ge(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
int rc;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	rc = (bi_compare(op1->value.integer, op2->value.integer) >= 0);
	break;

    case BT_REAL:
	rc = bg_compare_ge(op1->value.real, op2->value.real);
	break;

    case BT_CHARACTER:
	rc = (g95_compare_string(op1, op2, NULL) >= 0);
	break;

    default:
	rc = 0;
	g95_internal_error("arith_eq(): Bad type");
    }

    result->value.logical = rc;
    return result;
}



static g95_expr *arith_gt(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
int rc;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	rc = (bi_compare(op1->value.integer, op2->value.integer) > 0);
	break;

    case BT_REAL:
	rc = bg_compare_gt(op1->value.real, op2->value.real);
	break;

    case BT_CHARACTER:
	rc = (g95_compare_string(op1, op2, NULL) > 0);
	break;

    default:
	rc = 0;
	g95_internal_error("arith_gt(): Bad type");
    }

    result->value.logical = rc;
    return result;
}



static g95_expr *arith_lt(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
int rc;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	rc = (bi_compare(op1->value.integer, op2->value.integer) < 0);
	break;

    case BT_REAL:
	rc = bg_compare_lt(op1->value.real, op2->value.real);
	break;

    case BT_CHARACTER:
	rc = (g95_compare_string(op1, op2, NULL) < 0);
	break;

    default:
	rc = 0;
	g95_internal_error("arith_lt(): Bad type");
    }

    result->value.logical = rc;
    return result;
}



static g95_expr *arith_le(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
int rc;

    result = g95_constant_result(BT_LOGICAL, -1, &op1->where);

    switch(op1->ts.type) {
    case BT_INTEGER:
	check_integer_overflow(op1);
	check_integer_overflow(op2);

	rc = (bi_compare(op1->value.integer, op2->value.integer) <= 0);
	break;

    case BT_REAL:
	rc = bg_compare_le(op1->value.real, op2->value.real);
	break;

    case BT_CHARACTER:
	rc = (g95_compare_string(op1, op2, NULL) <= 0);
	break;

    default:
	rc = 0;
	g95_internal_error("arith_le(): Bad type");
    }

    result->value.logical = rc;
    return result;
}



/* arith_concat()-- Concatenate two string constants */

static g95_expr *arith_concat(g95_expr *op1, g95_expr *op2) {
g95_expr *result;
int len;

    result = g95_constant_result(BT_CHARACTER, -1, &op1->where);

    len = op1->value.character.length + op2->value.character.length;

    result->value.character.string = g95_getmem(len+1);
    result->value.character.length = len;

    memcpy(result->value.character.string, op1->value.character.string,
	   op1->value.character.length);

    memcpy(result->value.character.string + op1->value.character.length,
	   op2->value.character.string, op2->value.character.length);

    result->value.character.string[len] = '\0';
    return result;
}



/* g95_compare_string()-- Given two constant strings and the inverse
 * collating sequence, compare the strings.  We return -1 for a<b, 0
 * for a==b and 1 for a>b.  If the xcoll_table is NULL, we use the
 * processor's default collating sequence. */

int g95_compare_string(g95_expr *a, g95_expr *b, int *xcoll_table) {
int len, alen, blen, i, ac, bc;

    alen = a->value.character.length;
    blen = b->value.character.length;

    len = (alen > blen) ? alen : blen;

    for(i=0; i<len; i++) {
	ac = (i < alen) ? ((unsigned char) a->value.character.string[i]) : ' ';
	bc = (i < blen) ? ((unsigned char) b->value.character.string[i]) : ' ';

	if (xcoll_table != NULL) {
	    ac = xcoll_table[ac];
	    bc = xcoll_table[bc];
	}

	if (ac < bc) return -1;
	if (ac > bc) return 1;
    }

    return 0;   /* Strings are equal */
}



static g95_expr *reduce_unary(g95_expr *(*eval)(), g95_expr *op) {
g95_constructor *c, *head;
g95_expr *r;

    if (op->type == EXPR_CONSTANT)
	return eval(op);

    head = g95_copy_constructor(op->value.constructor.c);

    for(c=head; c; c=c->next)
	g95_replace_expr(c->expr, eval(c->expr));

    r = g95_get_expr();
    r->type = EXPR_ARRAY;
    r->value.constructor.c = head;
    r->shape = op->shape;

    r->ts = head->expr->ts;
    r->where = op->where;
    r->rank = op->rank;

    return r;
}



static g95_expr *reduce_binary_ac(g95_expr *(*eval)(), g95_expr *op1,
				  g95_expr *op2) {
g95_constructor *c, *head;
g95_expr *r;

    head = g95_copy_constructor(op1->value.constructor.c);

    for(c=head; c; c=c->next)
	g95_replace_expr(c->expr, eval(c->expr, op2));

    r = g95_get_expr();
    r->type = EXPR_ARRAY;
    r->value.constructor.c = head;
    r->shape = op1->shape;

    r->ts    = head->expr->ts;
    r->where = op1->where;
    r->rank  = op1->rank;

    return r;
}



static g95_expr *reduce_binary_ca(g95_expr *(*eval)(), g95_expr *op1,
				  g95_expr *op2) {
g95_constructor *c, *head;
g95_expr *r;

    head = g95_copy_constructor(op2->value.constructor.c);

    for(c=head; c; c=c->next)
	g95_replace_expr(c->expr, eval(op1, c->expr));

    r = g95_get_expr();
    r->type = EXPR_ARRAY;
    r->value.constructor.c = head;
    r->shape = op2->shape;

    r->ts    = head->expr->ts;
    r->where = op2->where;
    r->rank  = op2->rank;

    return r;
}



static g95_expr *reduce_binary_aa(g95_expr *(*eval)(), g95_expr *op1,
				  g95_expr *op2) {
g95_constructor *c, *d, *head;
g95_locus *where;
g95_expr *r;

    head = g95_copy_constructor(op1->value.constructor.c);

    if (g95_check_conformance("Elemental binary operation",
			      op1, op2) != FAILURE) {
	d = op2->value.constructor.c;
	for(c=head; c; c=c->next, d=d->next) {
	    if (d == NULL) {
		where = &c->expr->where;
		goto incommensurate;
	    }

	    g95_replace_expr(c->expr, eval(c->expr, d->expr));
	}

	if (d != NULL) {
	    where = &d->expr->where;
	incommensurate:
	    g95_error("Incommensurate arrays at %L", where);
	}
    }

    r = g95_get_expr();
    r->type = EXPR_ARRAY;
    r->value.constructor.c = head;
    r->shape = op1->shape;

    r->ts    = head->expr->ts;
    r->where = op1->where;
    r->rank  = op1->rank;

    return r;
}



/* charlen_expr()-- Given a character expression, return its length. */

static g95_expr *charlen_expr(g95_expr *e) {
g95_constructor *c;
g95_expr *f;

    if (e->ts.cl != NULL && e->ts.cl->length != NULL)
	return g95_copy_expr(e->ts.cl->length);

    switch(e->type) {
    case EXPR_CONSTANT:
	f = g95_int_expr(e->value.character.length);
	break;

    case EXPR_VARIABLE:
	f = g95_len_expr(g95_copy_expr(e));
	break;

    case EXPR_FUNCTION:
	f = g95_copy_expr(e->ts.cl->length);
	break;

    case EXPR_OP:
	f = g95_get_expr();
	f->ts.type = BT_INTEGER;
	f->ts.kind = g95_default_integer_kind(0);

	f->type = EXPR_OP;
	f->value.op.operator = INTRINSIC_PLUS;

	f->value.op.op1 = charlen_expr(e->value.op.op1);
	f->value.op.op2 = charlen_expr(e->value.op.op2);

	if (f->value.op.op1 != NULL && f->value.op.op2 != NULL)
	    g95_simplify_expr(f);

	else {
	    g95_free_expr(f);
	    f = NULL;
	}

	break;

    case EXPR_ARRAY:
	f = NULL;
	for(c=e->value.constructor.c; c; c=c->next) {
	    f = charlen_expr(c->expr);
	    if (f != NULL)
		break;
	}

	break;

    default:
	f = NULL;
	break;
    }

    return f;
}



/* concat_charlen()-- Create a charlen for the results of a
 * concatenation. */

static g95_charlen *concat_charlen(g95_expr *op1, g95_expr *op2) {
g95_charlen *cl;
g95_expr *e;

    e = g95_get_expr();
    e->ts.type = BT_INTEGER;
    e->ts.kind = g95_default_integer_kind(0);
    e->where = op1->where;

    e->type = EXPR_OP;
    e->value.op.operator = INTRINSIC_PLUS;

    e->value.op.op1 = charlen_expr(op1);
    e->value.op.op2 = charlen_expr(op2);

    if (e->value.op.op1 == NULL || e->value.op.op2 == NULL) {
	g95_free_expr(e);
	return NULL;
    }

    if (g95_simplify_expr(e) == FAILURE)
	return NULL;

    cl = g95_get_charlen(NULL);
    cl->length = e;

    return cl;
}



static g95_expr *reduce_binary(g95_expr *(*eval)(), g95_expr *op1,
			       g95_expr *op2) {
int c1, c2;

    c1 = (op1->type == EXPR_CONSTANT) || (op1->type == EXPR_STRUCTURE);
    c2 = (op2->type == EXPR_CONSTANT) || (op2->type == EXPR_STRUCTURE);

    if (c1 && c2)
	return eval(op1, op2);

    if (c1 && op2->type == EXPR_ARRAY)
	return reduce_binary_ca(eval, op1, op2);

    if (op1->type == EXPR_ARRAY && c2)
	return reduce_binary_ac(eval, op1, op2);

    return reduce_binary_aa(eval, op1, op2);
}



/* High level arithmetic subroutines.  These subroutines go into
 * eval_intrinsic(), which can do one of several things to its
 * operands.  If the operands are incompatible with the intrinsic
 * operation, we return a node pointing to the operands and hope that
 * an operator interface is found during resolution.
 *
 * If the operands are compatible and are constants, then we try doing
 * the arithmetic.  We also handle the cases where either or both
 * operands are array constructors. */

static g95_expr *eval_intrinsic(g95_intrinsic_op operator,
				g95_expr *(*eval)(),
				g95_expr *op1, g95_expr *op2) {
g95_expr temp, *result;
int unary;

    g95_clear_ts(&temp.ts);

    switch(operator) {
    case INTRINSIC_NOT:    /* Logical unary */
	if (op1->ts.type != BT_LOGICAL)
	    goto runtime;

	temp.ts.type = BT_LOGICAL;
	temp.ts.kind = g95_default_logical_kind();

	unary = 1;
	break;

	/* Logical binary operators */
    case INTRINSIC_OR:     case INTRINSIC_AND:
    case INTRINSIC_NEQV:   case INTRINSIC_EQV:
	if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
	    goto runtime;

	temp.ts.type = BT_LOGICAL;
	temp.ts.kind = g95_default_logical_kind();

	unary = 0;
	break;

    case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  /* Numeric unary */
	if (!g95_numeric_ts(&op1->ts)) 
	    goto runtime;

	temp.ts = op1->ts;

	unary = 1;
	break;

    case INTRINSIC_GE:  case INTRINSIC_LT:  /* Additional restrictions */
    case INTRINSIC_LE:  case INTRINSIC_GT:  /* for ordering relations */
	if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX) {
	    temp.ts.type = BT_LOGICAL;
	    temp.ts.kind = g95_default_logical_kind();
	    goto runtime;
	}

	/* Fall through */

    case INTRINSIC_EQ:      case INTRINSIC_NE:
	if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {
	    unary = 0;
	    temp.ts.type = BT_LOGICAL;
	    temp.ts.kind = g95_default_logical_kind();
	    break;
	}

	if (g95_ieee_class_compare(operator) &&
	    g95_internal_derived(&op1->ts, ITYPE_IEEE_CLASS) &&
	    g95_internal_derived(&op2->ts, ITYPE_IEEE_CLASS) &&

	    (op1->type == EXPR_STRUCTURE ||
	     (op1->type == EXPR_ARRAY && g95_is_constant_expr(op1) &&
	      g95_expanded_ac(op1))) &&

	    (op2->type == EXPR_STRUCTURE ||
	     (op2->type == EXPR_ARRAY && g95_is_constant_expr(op2) &&
	      g95_expanded_ac(op2)))) {

	    if (eval == arith_eq)
		eval = derived_eq;

	    else if (eval == arith_ne)
		eval = derived_ne;

	    unary = 0;
	    goto eval;
	}

	/* Fall through */

    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   /* Numeric binary */
	if (!g95_numeric_ts(&op1->ts) || !g95_numeric_ts(&op2->ts))
	    goto runtime;

	/* Insert any type conversions to make the operands compatible */

	temp.type = EXPR_OP;
	g95_clear_ts(&temp.ts);
	temp.value.op.operator = operator;

	temp.value.op.op1 = op1;
	temp.value.op.op2 = op2;

	g95_type_convert_binary(&temp);

	if (operator == INTRINSIC_EQ || operator == INTRINSIC_NE ||
	    operator == INTRINSIC_GE || operator == INTRINSIC_GT ||
	    operator == INTRINSIC_LE || operator == INTRINSIC_LT) {

	    temp.ts.type = BT_LOGICAL;
	    temp.ts.kind = g95_default_logical_kind();
	}

	unary = 0;
	break;

    case INTRINSIC_CONCAT:   /* Character binary */
	if (op1->ts.type != BT_CHARACTER || op2->ts.type != BT_CHARACTER)
	    goto runtime;

	temp.ts.type = BT_CHARACTER;
	temp.ts.kind = g95_default_character_kind();
	temp.ts.cl   = concat_charlen(op1, op2);

	unary = 0;
	break;

    case INTRINSIC_USER:
	goto runtime;

    default:
	g95_internal_error("eval_intrinsic(): Bad operator");
    }

    /* Try to combine the operators */

    if (operator == INTRINSIC_POWER && op2->ts.type != BT_INTEGER &&
	op2->ts.type != BT_REAL)
	goto runtime;

    if (op1->type != EXPR_CONSTANT &&
	(op1->type != EXPR_ARRAY || !g95_is_constant_expr(op1) ||
	 !g95_expanded_ac(op1)))
	goto runtime;

    if (op2 != NULL && op2->type != EXPR_CONSTANT &&
	(op2->type != EXPR_ARRAY || !g95_is_constant_expr(op2) ||
	 !g95_expanded_ac(op2)))
	goto runtime;

 eval:
    result = unary
	? reduce_unary(eval, op1)
	: reduce_binary(eval, op1, op2);

    g95_free_expr(op1);
    g95_free_expr(op2);
    return result;

    /* Create a run-time expression */

runtime:
    result = g95_get_expr();
    result->ts = temp.ts;

    result->type = EXPR_OP;
    result->value.op.operator = operator;

    result->value.op.op1 = op1;
    result->value.op.op2 = op2;

    if (op2 == NULL)
	result->rank = op1->rank;

    else {
	if (op1->rank != 0 && op2->rank == 0)
	    result->rank = op1->rank;

	if (op1->rank == 0 && op2->rank != 0)
	    result->rank = op2->rank;
    }

    result->where = op1->where;
    return result;
}



/* eval_type_intrinsic0() -- Modify type of expression for zero size array */

static g95_expr *eval_type_intrinsic0(g95_intrinsic_op operator, g95_expr *op){

    if (op == NULL)
	g95_internal_error("eval_type_intrinsic0(): op NULL");

    switch(operator) {
    case INTRINSIC_GE:  case INTRINSIC_LT:
    case INTRINSIC_LE:  case INTRINSIC_GT:
    case INTRINSIC_EQ:  case INTRINSIC_NE:
	op->ts.type = BT_LOGICAL;
	op->ts.kind = g95_default_logical_kind();
	break;

    default:
	break;
    }

    return op;
}



/* reduce_binary0()-- Reduce a binary expression where at least one of
 * the operands involves a zero-length array.  Returns NULL if neither
 * of the operands is a zero-length array. */

static g95_expr *reduce_binary0(g95_expr *op1, g95_expr *op2) {

    if (g95_zero_size_ac(op1)) {
	g95_free_expr(op2);
	return op1;
    }

    if (g95_zero_size_ac(op2)) {
	g95_free_expr(op1);
	return op2;
    }

    return NULL;
}



static g95_expr *eval_intrinsic_f2(g95_intrinsic_op operator,
				   g95_expr *(*eval)(g95_expr *),
				   g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    if (op2 == NULL) {
	if (g95_zero_size_ac(op1))
	    return eval_type_intrinsic0(operator, op1);

    } else {
	result = reduce_binary0(op1, op2);
	return eval_type_intrinsic0(operator, result);
    }

    return eval_intrinsic(operator, eval, op1, op2);
}



static g95_expr *eval_intrinsic_f3(g95_intrinsic_op operator,
				   g95_expr *(*eval)(g95_expr *, g95_expr *),
				   g95_expr *op1, g95_expr *op2) {
g95_expr *result;

    result = reduce_binary0(op1, op2);

    return (result != NULL)
	? eval_type_intrinsic0(operator, result)
	: eval_intrinsic(operator, eval, op1, op2);
}



g95_expr *g95_uplus(g95_expr *op) {
    return eval_intrinsic_f2(INTRINSIC_UPLUS, arith_uplus, op, NULL);
}


g95_expr *g95_uminus(g95_expr *op) {
    return eval_intrinsic_f2(INTRINSIC_UMINUS, arith_uminus, op, NULL);
}


g95_expr *g95_add(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_PLUS, arith_plus, op1, op2);
}


g95_expr *g95_subtract(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_MINUS, arith_minus, op1, op2);
}


g95_expr *g95_multiply(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_TIMES, arith_times, op1, op2);
}


g95_expr *g95_divide(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_DIVIDE, arith_divide, op1, op2);
}


g95_expr *g95_power(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_POWER, arith_power, op1, op2);
}


g95_expr *g95_concat(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_CONCAT, arith_concat, op1, op2);
}


g95_expr *g95_gt(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_GT, arith_gt, op1, op2);
}


g95_expr *g95_ge(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_GE, arith_ge, op1, op2);
}


g95_expr *g95_lt(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_LT, arith_lt, op1, op2);
}


g95_expr *g95_le(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_LE, arith_le, op1, op2);
}


g95_expr *g95_eq(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_EQ, arith_eq, op1, op2);
}


g95_expr *g95_ne(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_NE, arith_ne, op1, op2);
}


g95_expr *g95_and(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_AND, arith_and, op1, op2);
}


g95_expr *g95_or(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_OR, arith_or, op1, op2);
}


g95_expr *g95_not(g95_expr *op1) {
    return eval_intrinsic_f2(INTRINSIC_NOT, arith_not, op1, NULL);
}


g95_expr *g95_eqv(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_EQV, arith_eqv, op1, op2);
}


g95_expr *g95_neqv(g95_expr *op1, g95_expr *op2) {
    return eval_intrinsic_f3(INTRINSIC_NEQV, arith_neqv, op1, op2);
}

