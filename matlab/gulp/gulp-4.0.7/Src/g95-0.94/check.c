/* Check functions
 * Copyright (C) 2002-2008 Free Software Foundation, Inc.
 * Contributed by Andy Vaught & Katherine Holcomb

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


/* check.c-- These functions check to see if an argument list is
 * compatible with a particular intrinsic function or subroutine.
 * Presence of required arguments has already been established, the
 * argument list has been sorted into the right order and has NULL
 * arguments in the correct places for missing optional arguments.  */


#include <stdlib.h>
#include <stdarg.h>

#include "g95.h"
#include "intrinsic.h"




/* must_be()-- The fundamental complaint function of this source file.
 * This function can be called in all kinds of ways. */

static void must_be(g95_expr *e, int n, char *thing) {

    g95_error("'%s' argument of '%s' intrinsic at %L must be %s",
	      g95_current_intrinsic_arg[n], g95_current_intrinsic, &e->where,
	      thing);
}



/* cannot_be()-- Poor bastard stepchild of must_be(). */

static void cannot_be(g95_expr *e, int n, char *thing) {

    g95_error("'%s' argument of '%s' intrinsic at %L cannot be %s",
	      g95_current_intrinsic_arg[n], g95_current_intrinsic, &e->where,
	      thing);
}



/* numeric_check()-- Check that the expression is a numeric type */

static try numeric_check(g95_expr *e, int n) {

    if (g95_numeric_ts(&e->ts))
	return SUCCESS;

    must_be(e, n, "a numeric type");

    return FAILURE;
}



/* type_check()-- Check the type of an expression */

static try type_check(g95_expr *e, int n, bt type) {

    if (e->ts.type == type)
	return SUCCESS;

    must_be(e, n, g95_basic_typename(type));

    return FAILURE;
}



/* check_internal_type()-- Check that an expression is of an internal
 * derived type. */

static try check_internal_type(g95_expr *e, int n, internal_type type) {
char *p;

    if (e->ts.type == BT_DERIVED &&
	e->ts.derived->attr.itype == type)
	return SUCCESS;

    switch(type) {
    case ITYPE_C_PTR:          p = "intrinsic TYPE(C_PTR)";          break;
    case ITYPE_C_FUNPTR:       p = "intrinsic TYPE(C_FUNPTR)";       break;
    case ITYPE_IEEE_CLASS:     p = "intrinsic TYPE(IEEE_CLASS)";     break;
    case ITYPE_IEEE_FLAG:      p = "intrinsic TYPE(IEEE_FLAG)";      break;
    case ITYPE_IEEE_STATUS:    p = "intrinsic TYPE(IEEE_STATUS)";    break;
    case ITYPE_IEEE_ROUND:     p = "intrinsic TYPE(IEEE_ROUND)";     break;
    case ITYPE_IEEE_FEATURES:  p = "intrinsic TYPE(IEEE_FEATURES)";  break;
    default:
	g95_internal_error("check_internal_type(): Unknown type!");
	break;
    }

    must_be(e, n, p);

    return FAILURE;
}



/* nonprocedure()-- Make sure that the argument is not a procedure */

static try nonprocedure(g95_expr *e, int n) {

    if (e->ts.type == BT_PROCEDURE) {
	cannot_be(e, n, "a PROCEDURE");
	return FAILURE;
    }

    return SUCCESS;
}



/* int_or_real_check()-- Check that an expression is integer or real.
 * Normally this would be checking that the argument is "real" in the
 * mathematical sense, but fortran uses "real" in another context.  */

static try int_or_real_check(g95_expr *e, int n) {

    if (e->ts.type != BT_INTEGER && e->ts.type != BT_REAL) {
	must_be(e, n, "INTEGER or REAL");
	return FAILURE;
    }

    return SUCCESS;
}



/* scalar_check()-- Make sure an expression is a scalar */

static try scalar_check(g95_expr *e, int n) {

    if (e->rank == 0)
	return SUCCESS;

    must_be(e, n, "a scalar");

    return FAILURE;
}



/* kind_check()-- Verifiies that the expression is an optional
 * constant integer and that it specifies a valid kind for that type. */

static try kind_check(g95_expr *k, int n, bt type) {
int kind;

    if (k == NULL)
	return SUCCESS;

    if (type_check(k, n, BT_INTEGER) == FAILURE ||
	scalar_check(k, n) == FAILURE)
	return FAILURE;

    if (k->type != EXPR_CONSTANT) {
	must_be(k, n, "a constant");
	return FAILURE;
    }

    if (g95_extract_int(k, &kind) != NULL ||
	g95_validate_kind(type, kind) == -1) {
	g95_error("Invalid kind for %s at %L", g95_basic_typename(type),
		  &k->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* restricted_kind()-- Makes sure that a kind value is a kind=4 or
 * kind=8 integer. */

static try restricted_kind(g95_expr *kind, int n) {
int value;

    if (kind == NULL)
	return SUCCESS;

    if (scalar_check(kind, n) == FAILURE)
	return FAILURE;

    if (kind->type != EXPR_CONSTANT) {
	must_be(kind, n, "a constant");
	return FAILURE;
    }

    if (g95_extract_int(kind, &value) != NULL || (value != 4 && value != 8)) {
	g95_error("Invalid kind for at %L", &kind->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* double_check()-- Make sure the expression is a double precision real */

static try double_check(g95_expr *d, int n) {

    if (type_check(d, n, BT_REAL) == FAILURE)
	return FAILURE;

    if (d->ts.kind != g95_default_double_kind()) {
	must_be(d, n, "double precision");
	return FAILURE;
    }

    return SUCCESS;
}



/* logical_array_check()-- Make sure the expression is a logical array */

static try logical_array_check(g95_expr *array, int n) {

    if (array->ts.type != BT_LOGICAL || array->rank == 0) {
	must_be(array, n, "a logical array");
	return FAILURE;
    }

    return SUCCESS;
}



/* mask_array_check()-- Make the parameter is a logical array
 * conformable with the given rank. */

static try mask_array_check(g95_expr *array, int n, int rank) {

    if (array->ts.type != BT_LOGICAL) {
	must_be(array, n, "a logical array");
	return FAILURE;
    }

    if (array->rank != rank && array->rank != 0) {
	must_be(array, n, "conformable with ARRAY parameter");
	return FAILURE;
    }

    return SUCCESS;
}



/* assumed_size_check()-- Complain if the expression is an
 * assumed-size array. */

static try assumed_size_check(g95_expr *e, int n) {
g95_array_spec *as;
g95_array_ref *ar;

    if (e->rank == 0 || e->type != EXPR_VARIABLE)
	return SUCCESS;

    g95_find_array_ref(e, &ar, &as, 0);

    if (as == NULL || as->type != AS_ASSUMED_SIZE || ar->type != AR_FULL)
	return SUCCESS;

    cannot_be(e, n, "an assumed-size array");
    return FAILURE;
}



/* array_check()-- Make sure an expression is an array */

static try array_check(g95_expr *e, int n) {

    if (e->rank != 0)
	return SUCCESS;

    must_be(e, n, "an array");

    return FAILURE;
}



/* coarray_check()-- Make sure the expression is a coarray. */

static try coarray_check(g95_expr *e, int n) {

    if (g95_coarray_expr(e))
	return SUCCESS;

    must_be(e, n, "a coarray");
    return FAILURE;
}



/* same_type_check()-- Make sure two expression have the same type and
 * kind. */

static try same_type_check(g95_expr *e, int n, g95_expr *f, int m) {
char message[100];

    if (g95_compare_types(&e->ts, &f->ts))
	return SUCCESS;

    sprintf(message, "the same type and kind as '%s'",
	    g95_current_intrinsic_arg[n]);

    must_be(f, m, message);

    return FAILURE;
}



/* nonoptional_check()-- Make sure a variable expression is not an
 * optional dummy argument */

static try nonoptional_check(g95_expr *e, int n) {

    if (e->type == EXPR_VARIABLE && e->symbol->attr.optional) {
	g95_error("'%s' argument of '%s' intrinsic at %L must not be OPTIONAL",
		  g95_current_intrinsic_arg[n], g95_current_intrinsic,
		  &e->where);
	return FAILURE;
    }

    /* TODO: Recursive check on nonoptional variables? */

    return SUCCESS;
}



/* rank_check()-- Make sure that an expression has a certain (nonzero) rank */

static try rank_check(g95_expr *e, int n, int rank) {
char message[100];

    if (e->rank == rank)
	return SUCCESS;

    sprintf(message, "of rank %d", rank);

    must_be(e, n, message);

    return FAILURE;
}



/* variable_check()-- Make sure an expression is a variable (can be
 * assigned to). */

static try variable_check(g95_expr *e, int n) {

    if (e->type == EXPR_VARIABLE &&
	e->symbol->attr.flavor != FL_PARAMETER)
	return SUCCESS;

    if (e->type == EXPR_VARIABLE &&
	e->symbol->attr.intent == INTENT_IN) {
	g95_error("'%s' argument of '%s' intrinsic at %L cannot be INTENT(IN)",
		  g95_current_intrinsic_arg[n], g95_current_intrinsic,
		  &e->where);
	return FAILURE;
    }

    must_be(e, n, "a variable");

    return FAILURE;
}



/* allocatable_array_check()-- Make sure an expression is an
 * allocatable array. */

static try allocatable_array_check(g95_expr *e, int n) {

    if (variable_check(e, n) == FAILURE)
	return FAILURE;

    if (!g95_allocatable_expr(e)) {
	must_be(e, n, "ALLOCATABLE");
	return FAILURE;
    }

    if (g95_coarray_expr(e))
	return SUCCESS;

    if (!array_check(e, n) == FAILURE)
	return FAILURE;

    return SUCCESS;
}


/* kind_value_check()-- Check that an expression has a particular kind */

static try kind_value_check(g95_expr *e, int n, int k) {
char message[100];

    if (e->ts.kind == k)
	return SUCCESS;

    sprintf(message, "of kind %d", k);

    must_be(e, n, message);
    return FAILURE;
}



/* coarray_dim()-- Check the DIM parameter of a coarray. */

static try coarray_dim(g95_expr *coarray, g95_expr *dim, int n) {
int corank;

    if (type_check(dim, n, BT_INTEGER) == FAILURE ||
	scalar_check(dim, n) == FAILURE)
	return FAILURE;

    corank = g95_expr_corank(coarray);

    if (dim->type != EXPR_CONSTANT || coarray->type != EXPR_VARIABLE ||
	corank == 0)
	return SUCCESS;

    if (bi_compare(dim->value.integer, bi_1) < 0 ||
	bi_compare(dim->value.integer, int_to_bi(corank)) > 0) {

	g95_error("DIM parameter at %L is out of range", &dim->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* dim_check()-- Check the common DIM parameter for correctness */

static try dim_check(g95_expr *dim, g95_expr *array, int d, int n,
		     int optional) {

    if (optional) {
	if (dim == NULL)
	    return SUCCESS;

	if (nonoptional_check(dim, n) == FAILURE)
	    return FAILURE;

    } else {
	if (dim == NULL) {
	    g95_error("Missing DIM parameter in intrinsic '%s' at %L",
		      g95_current_intrinsic, g95_current_intrinsic_where);
	    return FAILURE;
	}
    }

    if (type_check(dim, n, BT_INTEGER) == FAILURE ||
	scalar_check(dim, n) == FAILURE)
	return FAILURE;

    if (dim->type != EXPR_CONSTANT || array->type != EXPR_VARIABLE ||
	array->symbol->as == NULL)
	return SUCCESS;

    if (bi_compare(dim->value.integer, bi_1) < 0 ||
	bi_compare(dim->value.integer, int_to_bi(array->rank + d)) > 0) {
	g95_error("DIM parameter at %L is out of range", &dim->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* dim_rank_check()-- If a DIM parameter is a constant, make sure that
 * it is greater than zero and less than the rank of the given
 * array. */

static try dim_rank_check(g95_expr *dim, g95_expr *array) {
g95_array_spec *as;
int rank;

    if (dim->type != EXPR_CONSTANT ||
	array->type != EXPR_VARIABLE)
	return SUCCESS;

    g95_find_array_ref(array, NULL, &as, 1);

    rank = array->rank;
    if (as->type == AS_ASSUMED_SIZE)
	rank--;

    if (bi_compare(dim->value.integer, bi_1) < 0 ||
	bi_compare(dim->value.integer, int_to_bi(G95_MAX_DIMENSIONS)) > 0) {
	g95_error("DIM argument of '%s' intrinsic at %L is not a valid "
		  "dimension index", g95_current_intrinsic, &dim->where);

	return FAILURE;
    }

    return SUCCESS;
}



/* check_a_kind()-- Check subroutine suitable for intrinsics 
 * taking a real argument and a kind argument for the result. */

static try check_a_kind(g95_expr *a, g95_expr *kind, bt type) {

    if (type_check(a, 0, BT_REAL) == FAILURE ||
	kind_check(kind, 1, type) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* g95_check_a_ikind()-- Check subroutine suitable for ceiling,
 * floor and nint. */

try g95_check_a_ikind(g95_expr *a, g95_expr *kind) {

    return check_a_kind(a, kind, BT_INTEGER);
}



/* g95_check_a_xkind()-- Check subroutine suitable for aint,
 * anint. */

try g95_check_a_xkind(g95_expr *a, g95_expr *kind) {

    return check_a_kind(a, kind, BT_REAL);
}



/* assumed_size()-- Check to see that we're not inquiring about
 * the size of an assumed size array. */

static try assumed_size(g95_expr *array, g95_expr *dim, char *p) {
g95_ref *ref;
bignum d;

    if (array->type != EXPR_VARIABLE || array->symbol->as == NULL ||
	array->symbol->as->type != AS_ASSUMED_SIZE)
	return SUCCESS;

    ref = array->ref;
    if (ref->type != REF_ARRAY || ref->u.ar.type != AR_FULL)
	return SUCCESS;

    if (dim == NULL)
	goto bad;

    if (dim->type != EXPR_CONSTANT || ref == NULL || ref->type != REF_ARRAY ||
	ref->u.ar.type != AR_FULL || ref->next != NULL)
	return SUCCESS;

    d = int_to_bi(array->symbol->as->rank);
    if (bi_compare(dim->value.integer, d) == 0) {
    bad:
	g95_error("Cannot determine %s of assumed-size array at %L",
		  p, &array->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* elemental_conformance()-- Check conformance between
 * arguments of elemental intrinsics. */

static try elemental_conformance(g95_expr *a, g95_expr *b) {
char message[1000];

    sprintf(message, "Argument to ELEMENTAL intrinsic '%s'",
	    g95_current_intrinsic);

    return g95_check_conformance(message, a, b);
}



try g95_check_abs(g95_expr *a) {

    return numeric_check(a, 0);
}


try g95_check_achar(g95_expr *i) {

    return type_check(i, 0, BT_INTEGER);
}



try g95_check_all_any(g95_expr *mask, g95_expr *dim) {

    if (logical_array_check(mask, 0) == FAILURE ||
	assumed_size_check(mask, 0) == FAILURE ||
	dim_check(dim, mask, 0, 1, 1) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_allocated(g95_expr *array) {

    return allocatable_array_check(array, 0);
}



/* Common check function where the first argument must be real or
 * integer and the second argument must be the same as the first. */

try g95_check_a_p(g95_expr *a, g95_expr *p) {

    if (int_or_real_check(a, 0) == FAILURE)
	return FAILURE;

    if (same_type_check(a, 0, p, 1) == FAILURE)
	return FAILURE;

  return SUCCESS;
}



try g95_check_associated(g95_expr *pointer, g95_expr *target) {

    if (pointer->type != EXPR_FUNCTION &&
	variable_check(pointer, 0) == FAILURE)
	return FAILURE;

    if (!g95_pointer_expr(pointer)) {
	must_be(pointer, 0, "a POINTER");
	return FAILURE;
    }

    if (target == NULL)
	return SUCCESS;

    /* Target argument is optional */

    if (!g95_pointer_expr(target) && !g95_target_expr(target)) {
	must_be(target, 1, "a POINTER or a TARGET");
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_atan2(g95_expr *y, g95_expr *x) {

    if (type_check(y, 0, BT_REAL) == FAILURE ||
	same_type_check(y, 0, x, 1) == FAILURE ||
	elemental_conformance(y, x) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_btest(g95_expr *i, g95_expr *pos) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE ||
	type_check(pos, 1, BT_INTEGER) == FAILURE ||
	elemental_conformance(i, pos) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_char(g95_expr *i, g95_expr *kind) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE)
	return FAILURE;

    if (kind_check(kind, 1, BT_CHARACTER) == FAILURE)
	return FAILURE;

  return SUCCESS;
}



try g95_check_cmplx(g95_expr *x, g95_expr *y, g95_expr *kind) {

    if (numeric_check(x, 0) == FAILURE)
	return FAILURE;

    if (y != NULL) {
	if (numeric_check(y, 1) == FAILURE)
	    return FAILURE;

	if (y->ts.type == BT_COMPLEX) {
	    g95_error("Argument 'y' of CMPLX at %L cannot be COMPLEX",
		      &y->where);
	    return FAILURE;
	}

	if (x->ts.type == BT_COMPLEX) {
	    must_be(y, 1, "not be present if 'x' is COMPLEX");
	    return FAILURE;
	}

	if (elemental_conformance(x, y) == FAILURE)
	    return FAILURE;
    }

    if (kind_check(kind, 2, BT_COMPLEX) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_count(g95_expr *mask, g95_expr *dim) {

    if (logical_array_check(mask, 0)  == FAILURE ||
	assumed_size_check(mask, 0)   == FAILURE ||
	dim_check(dim, mask, 0, 1, 1) == FAILURE)
	return FAILURE;

  return SUCCESS;
}



try g95_check_cshift(g95_expr *array, g95_expr *shift, g95_expr *dim) {

    if (array_check(array, 0) == FAILURE ||
	assumed_size_check(array, 0) == FAILURE)
	return FAILURE;

    if (array->rank == 1) {
	if (scalar_check(shift, 1) == FAILURE)
	    return FAILURE;

    } else if (shift->rank > 0 &&
	       (rank_check(shift, 1, array->rank-1) == FAILURE ||
		assumed_size_check(shift, 1) == FAILURE))
	return FAILURE;

    if (dim_check(dim, array, 0, 2, 1) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_dcmplx(g95_expr *x, g95_expr *y) {

    if (numeric_check(x, 0) == FAILURE)
	return FAILURE;

    if (y != NULL) {
	if (numeric_check(y, 1) == FAILURE)
	    return FAILURE;

	if (x->ts.type == BT_COMPLEX) {
	    must_be(y, 1, "not be present if 'x' is COMPLEX");
	    return FAILURE;
	}

	if (elemental_conformance(x, y) == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



try g95_check_dble(g95_expr *x) {

    return numeric_check(x, 0);
}



try g95_check_digits(g95_expr *x) {

    if (int_or_real_check(x, 0) == FAILURE)
	return FAILURE;

    g95_intrinsic_extension = 0;
    return SUCCESS;
}



try g95_check_dot_product(g95_expr *vector_a, g95_expr *vector_b) {

    switch(vector_a->ts.type) {
    case BT_LOGICAL:
	if (type_check(vector_b, 1, BT_LOGICAL) == FAILURE)
	    return FAILURE;
	break;

    case BT_INTEGER:
    case BT_REAL:
    case BT_COMPLEX:
	if (numeric_check(vector_b, 1) == FAILURE)
	    return FAILURE;
	break;

    default:
	must_be(vector_a, 0, "numeric or LOGICAL");
	return FAILURE;
    }

    if (rank_check(vector_a, 0, 1) == FAILURE ||
	assumed_size_check(vector_a, 0) == FAILURE)
	return FAILURE;

    if (rank_check(vector_b, 1, 1) == FAILURE ||
	assumed_size_check(vector_b, 0) == FAILURE)
	return FAILURE;

    if (g95_check_conformance("DOT_PRODUCT vector_a/vector_b",
			      vector_a, vector_b) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_ishft(g95_expr *i, g95_expr *shift) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE ||
	type_check(shift, 1, BT_INTEGER) == FAILURE ||
	elemental_conformance(i, shift) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_ishftc(g95_expr *i, g95_expr *shift, g95_expr *size) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE ||
	type_check(shift, 1, BT_INTEGER) == FAILURE ||
	elemental_conformance(i, shift) == FAILURE)
	return FAILURE;

    if (size != NULL &&
	(type_check(size, 2, BT_INTEGER) == FAILURE ||
	 elemental_conformance(i, size) == FAILURE ||
	 elemental_conformance(shift, size) == FAILURE))
	return FAILURE;

    return SUCCESS;
}



try g95_check_isnan(g95_expr *x) {

    return type_check(x, 0, BT_REAL);
}



try g95_check_kind(g95_expr *x) {

    if (x->ts.type == BT_DERIVED) {
	must_be(x, 0, "a non-derived type");
	return FAILURE;
    }

    g95_intrinsic_extension = 0;

    return SUCCESS;
}



try g95_check_lbound(g95_expr *array, g95_expr *dim, g95_expr *kind) {

    if (array_check(array, 0) == FAILURE ||
	dim_check(dim, array, 0, 1, 1) == FAILURE ||
	restricted_kind(kind, 2) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_loc(g95_expr *x) {

    return SUCCESS;
}



try g95_check_logical(g95_expr *a, g95_expr *kind) {

    if (type_check(a, 0, BT_LOGICAL) == FAILURE ||
	kind_check(kind, 1, BT_LOGICAL) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_eoshift(g95_expr *array, g95_expr *shift, g95_expr *boundary,
		      g95_expr *dim) {

    if (array_check(array, 0) == FAILURE ||
	assumed_size_check(array, 0) == FAILURE ||
	type_check(shift, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

    if (array->rank == 1) {
	if (scalar_check(shift, 1) == FAILURE)
	    return FAILURE;

	if (boundary != NULL && scalar_check(boundary, 2) == FAILURE)
	    return FAILURE;

    } else {
	if (shift->rank > 0 &&
	    (rank_check(shift, 1, array->rank-1) == FAILURE ||
	     assumed_size_check(shift, 1) == FAILURE))
	    return FAILURE;

	if (boundary != NULL && boundary->rank != 0 &&
	    (rank_check(boundary, 2, array->rank-1) == FAILURE ||
	     assumed_size_check(boundary, 2) == FAILURE))
	    return FAILURE;
    }

    if (boundary != NULL) {
	if (same_type_check(array, 0, boundary, 2) == FAILURE ||
	    assumed_size_check(boundary, 2) == FAILURE)
	    return FAILURE;

    } else {
	if (array->ts.type == BT_DERIVED) {
	    g95_error("Derived-type EOSHIFT requires the BOUNDARY parameter "
		      "at %L", &array->where);
	    return FAILURE;
	}
    }

    if (dim_check(dim, array, 0, 3, 1) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_exit(g95_expr *code) {

    if (code == NULL)
	return SUCCESS;

    if (type_check(code, 0, BT_INTEGER) == FAILURE ||
	scalar_check(code, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_etime(g95_expr *t) {

    if (t == NULL)
	return SUCCESS;

    if (type_check(t, 0, BT_REAL) == FAILURE ||
	array_check(t, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_getarg(g95_expr *n, g95_expr *args) {

    if (type_check(n, 0, BT_INTEGER) == FAILURE ||
	type_check(args, 1, BT_CHARACTER) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_huge(g95_expr *x) {

    if (int_or_real_check(x, 0) == FAILURE)
	return FAILURE;

    g95_intrinsic_extension = 0;

    return SUCCESS;
}



/* g95_check_i()-- Check that the single argument is an integer */

try g95_check_i(g95_expr *i) {

    return type_check(i, 0, BT_INTEGER);
}



try g95_check_iand(g95_expr *i, g95_expr *j) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE ||
	type_check(j, 1, BT_INTEGER) == FAILURE ||
	same_type_check(i, 0, j, 1) == FAILURE ||
	elemental_conformance(i, j) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_ibclr(g95_expr *i, g95_expr *pos) {

    if (type_check(i,   0, BT_INTEGER) == FAILURE ||
	type_check(pos, 1, BT_INTEGER) == FAILURE ||
	elemental_conformance(i, pos)  == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_ibits(g95_expr *i, g95_expr *pos, g95_expr *len) {

    if (type_check(i,   0, BT_INTEGER)  == FAILURE ||
	type_check(pos, 1, BT_INTEGER)  == FAILURE ||
	type_check(len, 2, BT_INTEGER)  == FAILURE ||
	elemental_conformance(i,   pos) == FAILURE ||
	elemental_conformance(i,   len) == FAILURE ||
	elemental_conformance(len, pos) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_ibset(g95_expr *i, g95_expr *pos) {

    if (type_check(i,   0, BT_INTEGER) == FAILURE ||
	type_check(pos, 1, BT_INTEGER) == FAILURE ||
	elemental_conformance(i, pos)  == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_idnint(g95_expr *a) {

    return double_check(a, 0);
}



try g95_check_ieor(g95_expr *i, g95_expr *j) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE ||
	type_check(j, 1, BT_INTEGER) == FAILURE ||
	same_type_check(i, 0, j, 1) == FAILURE ||
	elemental_conformance(i, j) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_index(g95_expr *string, g95_expr *substring, g95_expr *back) {

    if (type_check(string,    0, BT_CHARACTER) == FAILURE ||
	type_check(substring, 1, BT_CHARACTER) == FAILURE ||
	elemental_conformance(string, substring) == FAILURE)
	return FAILURE;

    if (back != NULL &&
	(type_check(back, 2, BT_LOGICAL) == FAILURE ||
	 elemental_conformance(string, back) == FAILURE ||
	 elemental_conformance(substring, back) == FAILURE))
	return FAILURE;

    if (string->ts.kind != substring->ts.kind) {
	must_be(substring, 1, "the same kind as 'string'");
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_int(g95_expr *x, g95_expr *kind) {

    if (numeric_check(x, 0) == FAILURE ||
	kind_check(kind, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_ior(g95_expr *i, g95_expr *j) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE ||
	type_check(j, 1, BT_INTEGER) == FAILURE ||
	same_type_check(i, 0, j, 1)  == FAILURE ||
	elemental_conformance(i, j)  == FAILURE)
	return FAILURE;

    return SUCCESS;
}



static try min_max_args(g95_actual_arglist *arg) {

    if (arg == NULL || arg->next == NULL) {
	g95_error("Intrinsic '%s' at %L must have at least two arguments",
		  g95_current_intrinsic, g95_current_intrinsic_where);
	return FAILURE;
    }

    return SUCCESS;
}



static try min_max_conformance(g95_actual_arglist *arg) {
g95_actual_arglist *a1, *a2;

    for(a1=arg; a1; a1=a1->next)
	for(a2=a1->next; a2; a2=a2->next)
	    if (elemental_conformance(a1->u.expr, a2->u.expr) == FAILURE)
		return FAILURE;

    return SUCCESS;
}



static try check_rest(bt type, int kind, g95_actual_arglist *arg) {
g95_expr *x;
int n;

    if (min_max_args(arg) == FAILURE || min_max_conformance(arg) == FAILURE)
	return FAILURE; 

    n = 1;

    for(; arg; arg=arg->next, n++) {
	x = arg->u.expr;
	if (x->ts.type != type || x->ts.kind != kind) {
	    g95_error("'a%d' argument of '%s' intrinsic at %L must be %s(%d)",
		      n, g95_current_intrinsic, &x->where,
		      g95_basic_typename(type), kind);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



try g95_check_min_max(g95_actual_arglist *arg) {
g95_actual_arglist *a;
g95_typespec ts;
g95_expr *x;
int n;

    if (min_max_args(arg) == FAILURE)
	return FAILURE; 

    x = arg->u.expr;

    switch(x->ts.type) {
    case BT_INTEGER:
    case BT_REAL:
	break;

    case BT_CHARACTER:
	if (g95_option.fmode == 0 || g95_option.fmode == 2003)
	    break;

	/* Fall through */

    default:
	g95_error("'a1' argument of '%s' intrinsic at %L cannot be %s",
		  g95_current_intrinsic, &x->where,
		  g95_basic_typename(x->ts.type));
	return FAILURE;
    }

    /* Character min/max is a special case */

    if (x->ts.type == BT_CHARACTER) {
	n = 2;
	for(a=arg->next; a; a=a->next, n++)
	    if (a->u.expr->ts.type != BT_CHARACTER) {
		g95_error("'a%d' argument of '%s' intrinsic at %L must be of "
			  "type CHARACTER", n, g95_current_intrinsic,
			  &a->u.expr->where);
		return FAILURE;
	    }

	return SUCCESS;
    }

    if (g95_option.fmode != 0)
	return check_rest(x->ts.type, x->ts.kind, arg);

    if (min_max_conformance(arg) == FAILURE)
	return FAILURE;

    /* Sloppy case, arguments are converted like binary operands */

    n = 1;
    for(a=arg; a; a=a->next, n++) {
	x = a->u.expr;
	if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) {
	    g95_error("'a%d' argument of '%s' intrinsic at %L cannot be of "
		      "type %s", n, g95_current_intrinsic, &a->u.expr->where,
		      g95_basic_typename(x->ts.type));
	    return FAILURE;
	}
    }

    x = arg->u.expr;
    g95_clear_ts(&ts);

    ts.type = x->ts.type;
    ts.kind = x->ts.kind;

    for(a=arg->next; a; a=a->next)
	g95_binary_result_type(&ts, &a->u.expr->ts, &ts);

    for(a=arg; a; a=a->next) {
	x = a->u.expr;
	if (x->ts.type != ts.type || x->ts.kind != ts.kind)
	    g95_convert_type(x, &ts, 1);
    }

    return SUCCESS;
}



try g95_check_min_max_integer(g95_actual_arglist *arg) {

    return check_rest(BT_INTEGER, g95_default_integer_kind(1), arg);
}



try g95_check_min_max_real(g95_actual_arglist *arg) {

    return check_rest(BT_REAL, g95_default_real_kind(1), arg);
}



try g95_check_min_max_double(g95_actual_arglist *arg) {

    return check_rest(BT_REAL, g95_default_double_kind(), arg);
}



try g95_check_matmul(g95_expr *matrix_a, g95_expr *matrix_b) {

    if ((matrix_a->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_b->ts)) {
	must_be(matrix_a, 0, "numeric or LOGICAL");
	return FAILURE;
    }

    if ((matrix_b->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_a->ts)) {
	must_be(matrix_b, 0, "numeric or LOGICAL");
	return FAILURE;
    }

    if (assumed_size_check(matrix_a, 0) == FAILURE ||
	assumed_size_check(matrix_b, 1) == FAILURE)
	return FAILURE;

    switch(matrix_a->rank) {
    case 1:
	if (rank_check(matrix_b, 1, 2) == FAILURE)
	    return FAILURE;

	break;

    case 2:
	if (matrix_b->rank == 2)
	    break;

	if (rank_check(matrix_b, 1, 1) == FAILURE)
	    return FAILURE;

	break;

    default:
	must_be(matrix_a, 0, "of rank 1 or 2");
	return FAILURE;
    }

    if (matrix_a->rank == 2 && matrix_b->rank == 2 &&
	matrix_a->shape != NULL && matrix_b->shape != NULL &&
	bi_compare(matrix_a->shape[1], matrix_b->shape[0]) != 0) {
	g95_error("Matrix size mismatch in matrix multiplication at %L", 
		  &matrix_a->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* g95_check_minloc_maxloc()-- Whoever came up with this interface was
 * probably on something.  If the (array, mask) form is given,
 * sort_actual() will have 'mask' in the 'dim' position. */

try g95_check_minloc_maxloc(g95_actual_arglist *a) {
g95_expr *array, *mask, *dim;
char msg[500];

    array = a->u.expr;
    if (int_or_real_check(array, 0) == FAILURE ||
	array_check(array, 0) == FAILURE)
	return FAILURE;

    dim  = a->next->u.expr;
    mask = a->next->next->u.expr;

    if (mask == NULL && dim != NULL && dim->ts.type == BT_LOGICAL &&
	a->next->name == NULL) {
	mask = dim;
	dim  = NULL;

	a->next->u.expr = NULL;
	a->next->type   = ARG_EXPR;

	a->next->next->type   = ARG_ARRAY_DESC;
	a->next->next->u.expr = mask;
    }

    if (dim_check(dim, array, 0, 1, 1) == FAILURE)
	return FAILURE;

    if (assumed_size_check(array, 0) == FAILURE)
	return FAILURE;

    if (mask != NULL) {
	if (mask_array_check(mask, 2, array->rank) == FAILURE ||
	    assumed_size_check(mask, 2) == FAILURE)
	    return FAILURE;

	sprintf(msg, "%s array/mask", g95_current_intrinsic);
	if (g95_check_conformance(msg, array, mask) == FAILURE)
	    return FAILURE;
    }

    a = a->next->next;
    a->next = g95_get_actual_arglist();

    if (a->u.expr != NULL && a->u.expr->rank == 0) {
	a->next->u.expr = a->u.expr;
	a->u.expr = NULL;
    }
    
    return SUCCESS;
}



try g95_check_minval_maxval(g95_expr *array, g95_expr *dim, g95_expr *mask) {
char msg[500];

    if (array_check(array, 0) == FAILURE ||
	int_or_real_check(array, 0) == FAILURE ||
	assumed_size_check(array, 0) == FAILURE ||
	dim_check(dim, array, 0, 1, 1) == FAILURE)
	return FAILURE;

    if (mask != NULL) {
	sprintf(msg, "%s array/mask", g95_current_intrinsic);

	if (mask_array_check(mask, 2, array->rank) == FAILURE ||
	    assumed_size_check(mask, 2) == FAILURE ||
	    g95_check_conformance(msg, array, mask) == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



try g95_check_merge(g95_expr *tsource, g95_expr *fsource, g95_expr *mask) {

    if (same_type_check(tsource, 0, fsource, 1) == FAILURE ||
	type_check(mask, 2, BT_LOGICAL) == FAILURE)
	return FAILURE;

    if (tsource->ts.type == BT_CHARACTER &&
	tsource->ts.cl != NULL &&
	tsource->ts.cl->length != NULL &&
	tsource->ts.cl->length->type == EXPR_CONSTANT &&
	fsource->ts.cl != NULL &&
	fsource->ts.cl->length != NULL &&
	fsource->ts.cl->length->type == EXPR_CONSTANT &&
	bi_compare(tsource->ts.cl->length->value.integer,
		   fsource->ts.cl->length->value.integer) != 0) {

	g95_error("MERGE intrinsic at %L has different character lengths",
		  &tsource->where);
	return FAILURE;
    }

    if ((tsource->rank > 0 || fsource->rank > 0) &&
	g95_check_conformance("MERGE tsource/fsource",
			      tsource, fsource) == FAILURE)
	return FAILURE;

    if ((tsource->rank > 0 || mask->rank > 0) &&
	g95_check_conformance("MERGE tsource/mask",
			      tsource, mask) == FAILURE)
	return FAILURE;

    if ((fsource->rank > 0 || mask->rank > 0) &&
	g95_check_conformance("MERGE fsource/mask",
			      fsource, mask) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_nearest(g95_expr *x, g95_expr *s) {

    if (type_check(x, 0, BT_REAL) == FAILURE ||
	type_check(s, 1, BT_REAL) == FAILURE ||
	elemental_conformance(x, s) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_null(g95_expr *mold) {

    if (mold == NULL)
	return SUCCESS;

    if (variable_check(mold, 0) == FAILURE)
	return FAILURE;

    if (!g95_pointer_expr(mold)) {
	must_be(mold, 0, "a POINTER");
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_product(g95_expr *array, g95_expr *dim, g95_expr *mask) {

    if (array_check(array, 0) == FAILURE ||
	numeric_check(array, 0) == FAILURE ||
	assumed_size_check(array, 0) == FAILURE ||
	dim_check(dim, array, 0, 1, 1) == FAILURE)
	return FAILURE;

    if (mask != NULL &&
	(mask_array_check(mask, 2, array->rank) == FAILURE ||
	 assumed_size_check(mask, 2) == FAILURE ||
	 g95_check_conformance("PRODUCT array/mask", array, mask) == FAILURE))
	return FAILURE;

    return SUCCESS;
}



try g95_check_present(g95_expr *a) {

    if (variable_check(a, 0) == FAILURE)
	return FAILURE;

    if (!a->symbol->attr.dummy) {
	must_be(a, 0, "a dummy variable");
	return FAILURE;
    }

    if (a->ref != NULL &&
	(a->ref->type != REF_ARRAY || a->ref->u.ar.type != AR_FULL ||
	 a->ref->next != NULL)) {
	must_be(a, 0, "a variable name");
	return FAILURE;
    }

    if (!a->symbol->attr.optional) {
	must_be(a, 0, "an OPTIONAL dummy variable");
	return FAILURE;
    }

    g95_intrinsic_extension = 0;

    return SUCCESS;
}



try g95_check_precision(g95_expr *x) {

    if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
	must_be(x, 0, "of type REAL or COMPLEX");
	return FAILURE;
    }

    g95_intrinsic_extension = 0;

    return SUCCESS;
}



try g95_check_pack(g95_expr *array, g95_expr *mask, g95_expr *vector) {

    if (array_check(array, 0) == FAILURE ||
	assumed_size_check(array, 0) == FAILURE ||
	mask_array_check(mask, 1, array->rank) == FAILURE ||
	assumed_size_check(mask, 1) == FAILURE ||
	g95_check_conformance("PACK array/mask", array, mask) == FAILURE)
	return FAILURE;

    if (vector != NULL) {
	if (same_type_check(array, 0, vector, 2) == FAILURE ||
	    assumed_size_check(vector, 2) == FAILURE ||
	    rank_check(vector, 2, 1) == FAILURE)
	    return FAILURE;

	/* TODO: More constraints here */
    }

    return SUCCESS;
}



try g95_check_radix(g95_expr *x) {

    if (int_or_real_check(x, 0) == FAILURE)
	return FAILURE;

    g95_intrinsic_extension = 0;

    return SUCCESS;
}



try g95_check_range(g95_expr *x) {

    if (numeric_check(x, 0) == FAILURE)
	return FAILURE;

    g95_intrinsic_extension = 0;

    return SUCCESS;
}



try g95_check_real(g95_expr *a, g95_expr *kind) {

    if (numeric_check(a, 0) == FAILURE ||
	kind_check(kind, 1, BT_REAL) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_repeat(g95_expr *x, g95_expr *y) {
g95_typespec ts;

    if (scalar_check(x, 0) == FAILURE ||
	scalar_check(y, 0) == FAILURE ||
	type_check(x, 0, BT_CHARACTER) == FAILURE ||
	type_check(y, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

    if (y->ts.kind != g95_default_integer_kind(0)) {
	ts.type = BT_INTEGER;
	ts.kind = g95_default_integer_kind(0);
	g95_convert_type(y, &ts, 0);
    }

    return SUCCESS;
}



try g95_check_reshape(g95_expr *source, g95_expr *shape,
		      g95_expr *pad, g95_expr *order) {
bignum size;

    if (array_check(source, 0) == FAILURE ||
	rank_check(shape, 1, 1) == FAILURE ||
	assumed_size_check(source, 0) == FAILURE ||
	assumed_size_check(shape, 1) == FAILURE ||
	type_check(shape, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

    size = g95_array_size(shape);
    if (size == NULL) {
	g95_error("'shape' argument of 'reshape' intrinsic at %L must be an "
		  "array of constant size", &shape->where);
	return FAILURE;
    }

    if (bi_compare(size, int_to_bi(G95_MAX_DIMENSIONS)) > 0) {
	g95_error("'shape' argument of 'reshape' intrinsic at %L has more "
		  "than " stringize(G95_MAX_DIMENSIONS) " elements",
		  &shape->where);
	return FAILURE;
    }

    if (pad != NULL && 
	(same_type_check(source, 0, pad, 2) == FAILURE ||
	 assumed_size_check(pad, 2) == FAILURE ||
	 array_check(pad, 2) == FAILURE))
	return FAILURE;

    if (order != NULL) {
	if (array_check(order, 3) == FAILURE ||
	    type_check(order, 3, BT_INTEGER) == FAILURE ||
	    rank_check(order, 3, 1) == FAILURE ||
	    assumed_size_check(order, 3) == FAILURE)
	    return FAILURE;

	size = g95_array_size(order);

	if (size != NULL && bi_compare(size, g95_array_size(shape)) != 0) {
	    g95_error("'order' argument of 'reshape' intrinsic at %L must "
		      "have the same size as the 'shape' argument",
		      &order->where);
	    return FAILURE;
	}
    }

    g95_intrinsic_extension = 0;
    return SUCCESS;
}



try g95_check_scale(g95_expr *x, g95_expr *i) {
g95_typespec ts;

    if (type_check(x, 0, BT_REAL) == FAILURE ||
	type_check(i, 1, BT_INTEGER) == FAILURE ||
	elemental_conformance(x, i) == FAILURE)
	return FAILURE;

    if (i->ts.kind != g95_default_integer_kind(0)) {
	ts.type = BT_INTEGER;
	ts.kind = g95_default_integer_kind(0);
	g95_convert_type(i, &ts, 0);
    }

    return SUCCESS;
}



try g95_check_scan(g95_expr *x, g95_expr *y, g95_expr *z) {

    if (type_check(x, 0, BT_CHARACTER) == FAILURE ||
	type_check(y, 1, BT_CHARACTER) == FAILURE ||
	elemental_conformance(x, y)    == FAILURE)
	return FAILURE;

    if (z != NULL &&
	(type_check(z, 2, BT_LOGICAL) == FAILURE ||
	 elemental_conformance(x, z)  == FAILURE ||
	 elemental_conformance(y, z)  == FAILURE))
	return FAILURE;

    if (same_type_check(x, 0, y, 1) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_selected_int_kind(g95_expr *r) {

    if (type_check(r, 0, BT_INTEGER) == FAILURE ||
	scalar_check(r, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_selected_real_kind(g95_expr *p, g95_expr *r) {

    if (p == NULL && r == NULL) {
	g95_error("Missing arguments to %s intrinsic at %L",
		  g95_current_intrinsic, g95_current_intrinsic_where);

	return FAILURE;
    }

    if (p != NULL &&
	(scalar_check(p, 0) == FAILURE ||
	 type_check(p, 0, BT_INTEGER) == FAILURE))
	return FAILURE;

    if (r != NULL &&
	(scalar_check(r, 0) == FAILURE ||
	 type_check(r, 1, BT_INTEGER) == FAILURE))
	return FAILURE;

    g95_intrinsic_extension = 0;
    return SUCCESS;
}



try g95_check_set_exponent(g95_expr *x, g95_expr *i) {

    if (type_check(x, 0, BT_REAL) == FAILURE ||
	type_check(i, 1, BT_INTEGER) == FAILURE ||
	elemental_conformance(x, i) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_shape(g95_expr *source, g95_expr *kind) {
g95_array_spec *as;

    if (restricted_kind(kind, 1) == FAILURE)
	return FAILURE;

    if (source->type != EXPR_VARIABLE)
	return SUCCESS;

    g95_find_array_ref(source, NULL, &as, 0);

    if (as != NULL && as->type == AS_ASSUMED_SIZE) {
	g95_error("'source' argument of 'shape' intrinsic at %L must not be "
		  "an assumed size array", &source->where);
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_size(g95_expr *array, g95_expr *dim, g95_expr *kind) {

    if (array_check(array, 0) == FAILURE ||
	dim_check(dim, array, 0, 1, 1) == FAILURE ||
	assumed_size(array, dim, "SIZE") == FAILURE ||
	restricted_kind(kind, 2) == FAILURE)
	return FAILURE;

    if (dim != NULL) {
	if (type_check(dim, 1, BT_INTEGER) == FAILURE ||
	    kind_value_check(dim, 1, g95_default_integer_kind(1)) == FAILURE ||
	    dim_rank_check(dim, array) == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



try g95_check_sizeof(g95_expr *object) {

    return SUCCESS;
}



try g95_check_sign(g95_expr *a, g95_expr *b) {

    if (int_or_real_check(a, 0) == FAILURE ||
	same_type_check(a, 0, b, 1) == FAILURE ||
	elemental_conformance(a, b) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_spread(g95_expr *source, g95_expr *dim, g95_expr *ncopies) {

    if (source->rank >= G95_MAX_DIMENSIONS) {
	must_be(source, 0, "less than rank " stringize(G95_MAX_DIMENSIONS));
	return FAILURE;
    }

    if (source->rank > 0 && assumed_size_check(source, 0) == FAILURE)
	return FAILURE;

    if (dim_check(dim, source, 1, 1, 0) == FAILURE ||
	type_check(ncopies, 2, BT_INTEGER) == FAILURE ||
	scalar_check(ncopies, 2) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_sum(g95_expr *array, g95_expr *dim, g95_expr *mask) {

    if (array_check(array, 0) == FAILURE ||
	numeric_check(array, 0) == FAILURE ||
	assumed_size_check(array, 0) == FAILURE ||
	dim_check(dim, array, 0, 1, 1) == FAILURE)
	return FAILURE;

    if (mask != NULL &&
	(mask_array_check(mask, 2, array->rank) == FAILURE ||
	 assumed_size_check(mask, 2) == FAILURE ||
	 g95_check_conformance("SUM array/mask", array, mask) == FAILURE))
	return FAILURE;

    return SUCCESS;
}



try g95_check_signal(g95_expr *sig, g95_expr *handler, g95_expr *flag) {

    if (scalar_check(sig, 0) == FAILURE ||
	type_check(sig, 0, BT_INTEGER) == FAILURE ||
	scalar_check(handler, 0) == FAILURE ||
	(flag != NULL &&
	 (scalar_check(flag, 0) == FAILURE ||
	  type_check(flag, 0, BT_INTEGER) == FAILURE)))
	return FAILURE;

    if (handler->ts.type != BT_PROCEDURE && handler->ts.type != BT_INTEGER) {
	g95_error("'handler' argument of 'signal' intrinsic at %L must be "
		  "a procedure or an integer", &handler->where);
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_transfer(g95_expr *source, g95_expr *mold, g95_expr *size) {

    if (nonprocedure(source, 0) == FAILURE ||
	nonprocedure(mold, 1) == FAILURE)
	return FAILURE;

    if (size != NULL) {
	if (type_check(size, 2, BT_INTEGER) == FAILURE ||
	    scalar_check(size, 2) == FAILURE ||
	    nonoptional_check(size, 2) == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



try g95_check_transpose(g95_expr *matrix) {

    if (rank_check(matrix, 0, 2) == FAILURE ||
	assumed_size_check(matrix, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_trim(g95_expr *string) {

    if (type_check(string, 0, BT_CHARACTER) == FAILURE ||
	scalar_check(string, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_ubound(g95_expr *array, g95_expr *dim, g95_expr *kind) {

    if (array_check(array, 0) == FAILURE ||
	dim_check(dim, array, 0, 1, 1) == FAILURE ||
	assumed_size(array, dim, "UBOUND") == FAILURE ||
	restricted_kind(kind, 2) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_unpack(g95_expr *vector, g95_expr *mask, g95_expr *field) {

    if (rank_check(vector, 0, 1) == FAILURE ||
	logical_array_check(mask, 1) == FAILURE ||
	assumed_size_check(vector, 0) == FAILURE ||
	assumed_size_check(mask, 1) == FAILURE ||
	assumed_size_check(field, 2) == FAILURE ||
	same_type_check(vector, 0, field, 2) == FAILURE ||
	g95_check_conformance("UNPACK mask/field", mask, field) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_verify(g95_expr *x, g95_expr *y, g95_expr *z) {

    if (type_check(x, 0, BT_CHARACTER) == FAILURE ||
	same_type_check(x, 0, y, 1) == FAILURE ||
	elemental_conformance(x, y) == FAILURE)
	return FAILURE;

    if (z != NULL &&
	(type_check(z, 2, BT_LOGICAL) == FAILURE ||
	 elemental_conformance(x, z) == FAILURE ||
	 elemental_conformance(y, z) == FAILURE))
	return FAILURE;

    return SUCCESS;
}



try g95_check_besn(g95_expr *n, g95_expr *x) {

    if (type_check(x, 1, BT_REAL) == FAILURE ||
	kind_value_check(x, 1, g95_default_real_kind(1)) == FAILURE ||
	type_check(n, 0, BT_INTEGER) == FAILURE ||
	elemental_conformance(n, x) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_besn2(g95_expr *n, g95_expr *x) {

    if (type_check(x, 1, BT_REAL) == FAILURE ||
	kind_value_check(x, 1, g95_default_double_kind()) == FAILURE ||
	type_check(n, 0, BT_INTEGER) == FAILURE ||
	elemental_conformance(n, x) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* g95_check_x()-- Common check function for the half a dozen
 * intrinsics that have a single real argument */

try g95_check_x(g95_expr *x) {

    if (type_check(x, 0, BT_REAL) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* g95_check_x_ni()-- Common check functions for numeric inquiry
 * functions that have a single real argument. */

try g95_check_x_ni(g95_expr *x) {

    if (type_check(x, 0, BT_REAL) == FAILURE)
	return FAILURE;

    g95_intrinsic_extension = 0;

    return SUCCESS;
}



/* g95_check_iostat()-- Check the is_iostat_end() and is_iostat_eor()
 * intrinsics. */

try g95_check_iostat(g95_expr *i) {

    if (type_check(i, 0, BT_INTEGER) == FAILURE ||
	scalar_check(i, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_cpu_time(g95_expr *time) {

    if (scalar_check(time, 0) == FAILURE ||
	type_check(time, 0, BT_REAL) == FAILURE ||
	variable_check(time, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_secnds(g95_expr *t) {

    if (scalar_check(t, 0) == FAILURE ||
	type_check(t, 0, BT_REAL) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_date_and_time(g95_expr *date, g95_expr *time,
			    g95_expr *zone, g95_expr *values) {

    if (date != NULL &&
	(type_check(date, 0, BT_CHARACTER) == FAILURE ||
	 scalar_check(date, 0) == FAILURE ||
	 variable_check(date, 0) == FAILURE))
	return FAILURE;

    if (time != NULL &&
	(type_check(time, 1, BT_CHARACTER) == FAILURE ||
	 scalar_check(time, 1) == FAILURE ||
	 variable_check(time, 1) == FAILURE))
	return FAILURE;

    if (zone != NULL &&
	(type_check(zone, 2, BT_CHARACTER) == FAILURE ||
	 scalar_check(zone, 2) == FAILURE ||
	 variable_check(zone, 2) == FAILURE))
	return FAILURE;

    if (values != NULL &&
	(type_check(values, 3, BT_INTEGER) == FAILURE ||
	 array_check(values, 3) == FAILURE ||
	 rank_check(values, 3, 1) == FAILURE ||
	 variable_check(values, 3) == FAILURE))
	return FAILURE;

    return SUCCESS;
}



try g95_check_move_alloc(g95_expr *from, g95_expr *to) {

    if (allocatable_array_check(from, 0) == FAILURE ||
	allocatable_array_check(to, 1) == FAILURE ||
	same_type_check(from, 0, to, 1) == FAILURE)
	return FAILURE;

    if (from->rank != to->rank) {
	must_be(from, 1, "same rank as FROM parameter");
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_mvbits(g95_expr *from, g95_expr *frompos, g95_expr *len,
		     g95_expr *to, g95_expr *topos) {
int len_v, frompos_v, topos_v;

    if (type_check(from, 0, BT_INTEGER) == FAILURE ||
	type_check(frompos, 1, BT_INTEGER) == FAILURE ||
	type_check(len, 2, BT_INTEGER) == FAILURE ||
	same_type_check(from, 0, to, 3) == FAILURE ||
	variable_check(to, 3) == FAILURE ||
	type_check(topos, 4, BT_INTEGER) == FAILURE)
	return FAILURE;

    if (g95_compare_expr_int(frompos, 0) == CMP_LT) {
	g95_error("FROMPOS argument of MVBIT intrinsic at %L must be "
		  "nonnegative", &frompos->where);
	return FAILURE;
    }
 
    frompos_v = (frompos->type == EXPR_CONSTANT)
	? bi_to_int(frompos->value.integer)
	: -1;

    if (g95_compare_expr_int(topos, 0) == CMP_LT) {
	g95_error("TOPOS argument of MVBIT intrinsic at %L must be "
		  "nonnegative", &topos->where);
	return FAILURE;
    }

    topos_v = (topos->type == EXPR_CONSTANT)
	? bi_to_int(topos->value.integer)
	: -1;

    if (g95_compare_expr_int(len, 0) == CMP_LT) {
	g95_error("LEN argument of MVBIT intrinsic at %L must be nonnegative",
		  &len->where);
	return FAILURE;
    }

    len_v = (len->type == EXPR_CONSTANT)
	? bi_to_int(len->value.integer)
	: -1;

    if (frompos_v != -1 && len_v != -1 &&
	frompos_v + len_v > g95_bit_size(from->ts.kind)) {
	g95_error("length of from-field in MVBITS at %L exceeds BIT_SIZE",
		  &len->where);
	return FAILURE;
    }

    if (topos_v != -1 && len_v != -1 &&
	topos_v + len_v > g95_bit_size(to->ts.kind)) {
	g95_error("length of to-field in MVBITS at %L exceeds BIT_SIZE",
		  &len->where);
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_random_number(g95_expr *harvest) {

    if (type_check(harvest, 0, BT_REAL) == FAILURE ||
	variable_check(harvest, 0) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_random_seed(g95_expr *size, g95_expr *put, g95_expr *get) {

    if (size != NULL && 
	(scalar_check(size, 0) == FAILURE ||
	 type_check(size, 0, BT_INTEGER) == FAILURE ||
	 variable_check(size, 0) == FAILURE ||
	 kind_value_check(size, 0, g95_default_integer_kind(1)) == FAILURE))
	return FAILURE;

    if (put != NULL &&
	(array_check(put, 1) == FAILURE ||
	 rank_check(put, 1, 1) == FAILURE ||
	 type_check(put, 1, BT_INTEGER) == FAILURE ||
	 kind_value_check(put, 1, g95_default_integer_kind(1)) == FAILURE))
	return FAILURE;

    if (get != NULL &&
	(array_check(get, 2) == FAILURE ||
	 rank_check(get, 2, 1) == FAILURE ||
	 type_check(get, 2, BT_INTEGER) == FAILURE ||
	 variable_check(get, 2) == FAILURE ||
	 kind_value_check(get, 2, g95_default_integer_kind(1)) == FAILURE))
	return FAILURE;

    return SUCCESS;
}



try g95_check_system_clock(g95_expr *count, g95_expr *count_rate,
			   g95_expr *count_max) {

    if (count != NULL &&
	(scalar_check(count, 0) == FAILURE ||
	 type_check(count, 0, BT_INTEGER) == FAILURE ||
	 (G95_STRICT_F95() &&
	  kind_value_check(count, 0, g95_default_integer_kind(0)) == FAILURE)))
	return FAILURE;

    if (count_rate != NULL) {
	if (scalar_check(count_rate, 1) == FAILURE)
	    return FAILURE;

	if (G95_STRICT_F95()) {
	    if (type_check(count_rate, 1, BT_INTEGER) == FAILURE ||
		kind_value_check(count_rate, 1,
				 g95_default_integer_kind(0)) == FAILURE)
		return FAILURE;

	} else if (count_rate->ts.type != BT_INTEGER &&
		   count_rate->ts.type != BT_REAL) {
	    must_be(count_rate, 1, "REAL or INTEGER");
	    return FAILURE;
	}
    }

    if (count_max != NULL &&
	(scalar_check(count_max, 2) == FAILURE ||
	 type_check(count_max, 2, BT_INTEGER) == FAILURE ||
	 (G95_STRICT_F95() && 
	  kind_value_check(count_max, 2,
			   g95_default_integer_kind(0)) == FAILURE)))
	return FAILURE;

    return SUCCESS;
}



try g95_check_this_image(g95_expr *coarray, g95_expr *dim) {

    if (coarray == NULL && dim == NULL)
	return SUCCESS;

    if (coarray == NULL) {
	g95_error("Missing coarray argument at %L", &dim->where);
	return FAILURE;
    }

    if (dim != NULL && coarray_dim(coarray, dim, 1) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_image_index(g95_expr *coarray, g95_expr *sub) {

    if (coarray_check(coarray, 0) == FAILURE)
	return FAILURE;

    if (rank_check(sub, 1, 1) == FAILURE ||
	type_check(sub, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_c_loc(g95_expr *x) {
g95_symbol *sym;

    if (variable_check(x, 0) == FAILURE)
	return FAILURE;

    sym = x->symbol;

    if (!sym->attr.pointer && !sym->attr.target) {
	must_be(x, 0, "a TARGET or a POINTER");
	return FAILURE;
    }

    if (sym->ts.type == BT_CHARACTER && x->ref == NULL &&
	sym->ts.cl->length != NULL &&
	sym->ts.cl->length->type == EXPR_CONSTANT && 
	bi_compare(sym->ts.cl->length->value.integer, bi_1) != 0) {
	g95_error("C_LOC of character type at %L must be of length one.",
		  &x->where);
	return FAILURE;
    }

    sym->attr.targetted = 1;
    return SUCCESS;
}



try g95_check_c_funloc(g95_expr *x) {

    if (type_check(x, 0, BT_PROCEDURE) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



try g95_check_c_associated(g95_expr *c_ptr1, g95_expr *c_ptr2) {

    if (!g95_c_ptr(&c_ptr1->ts)) {
	must_be(c_ptr1, 0, "of type C_PTR or C_FUNPTR");
	return FAILURE;      
    }

    if (c_ptr2 != NULL && !g95_c_ptr(&c_ptr2->ts)) {
	must_be(c_ptr2, 1, "of type C_PTR or C_FUNPTR");
	return FAILURE;      
    }

    return SUCCESS;
}



try g95_check_c_f_pointer(g95_expr *cptr, g95_expr *fptr, g95_expr *shape) {
bignum size;

    if (check_internal_type(cptr, 0, ITYPE_C_PTR) == FAILURE)
	return FAILURE;

    if (scalar_check(cptr, 0) == FAILURE)
	return FAILURE;

    if (variable_check(fptr, 1) == FAILURE)
	return FAILURE;

    if (!g95_pointer_expr(fptr)) {
	must_be(fptr, 1, "a POINTER");
	return FAILURE;
    }

    if (fptr->rank != 0 && shape == NULL) {
	g95_error("SHAPE argument of C_F_POINTER is missing for array F_PTR "
		  "at %L", &fptr->where);
	return FAILURE;
    }

    if (fptr->rank == 0 && shape != NULL) {
	g95_error("SHAPE argument of C_F_POINTER must not be present for "
		  "scalar F_PTR at %L", &shape->where);
	return FAILURE;
    }

    if (shape != NULL) {
	if (shape->rank != 1) {
	    g95_error("SHAPE argument of C_F_POINTER at %L must be of rank "
		      "one", &shape->where);
	    return FAILURE;
	}

	if (type_check(shape, 2, BT_INTEGER) == FAILURE)
	    return FAILURE;

	size = g95_array_size(shape);
	if (size != NULL && bi_compare(size, int_to_bi(fptr->rank)) != 0) {
	    g95_error("Size of SHAPE argument of C_F_POINTER at %L must be "
		      "the same as the rank of FPTR", &shape->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



try g95_check_c_f_procpointer(g95_expr *cptr, g95_expr *fptr) {

    if (check_internal_type(cptr, 0, ITYPE_C_FUNPTR) == FAILURE)
	return FAILURE;

    if (scalar_check(cptr, 0) == FAILURE)
	return FAILURE;

    if (variable_check(fptr, 1) == FAILURE)
	return FAILURE;

    if (!g95_proc_pointer_expr(fptr)) {
	must_be(fptr, 1, "a PROCEDURE POINTER");
	return FAILURE;
    }

    return SUCCESS;
}



try g95_check_ieee_class(g95_expr *x) {

    return type_check(x, 0, BT_REAL);
}



try g95_check_ieee_is_nan(g95_expr *x) {

    return type_check(x, 0, BT_REAL);
}



try g95_check_ieee_is_negative(g95_expr *x) {

    return type_check(x, 0, BT_REAL);
}



try g95_check_ieee_is_finite(g95_expr *x) {

    return type_check(x, 0, BT_REAL);
}



try g95_check_ieee_is_normal(g95_expr *x) {

    return type_check(x, 0, BT_REAL);
}



/* g95_check_ieee_support()-- This checks a whole family of support
 * functions. */

try g95_check_ieee_support(g95_expr *x) {

    return (x == NULL)
	? SUCCESS
	: type_check(x, 0, BT_REAL);
}



try g95_check_ieee_support_rounding(g95_expr *round, g95_expr *x) {

    if (check_internal_type(round, 0, ITYPE_IEEE_ROUND) == FAILURE)
	return FAILURE;

    return (x == NULL)
	? SUCCESS
	: type_check(x, 1, BT_REAL);
}



try g95_check_ieee_value(g95_expr *x, g95_expr *class) {

    if (type_check(x, 0, BT_REAL) == FAILURE)
	return FAILURE;

    if (class->ts.type != BT_DERIVED ||
	class->ts.derived->attr.itype != ITYPE_IEEE_CLASS) {
	must_be(class, 1, "of type IEEE_CLASS");
	return FAILURE;
    }

    return SUCCESS;
}



/* g95_check_co_bound()-- Does double-duty for co_lbound() and
 * co_ubound(). */

try g95_check_co_bound(g95_expr *coarray, g95_expr *dim, g95_expr *kind) {

    if (coarray_check(coarray, 0) == FAILURE)
	return FAILURE;

    if (dim != NULL && type_check(dim, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

    if (kind != NULL && type_check(kind, 1, BT_INTEGER) == FAILURE)
	return FAILURE;

    return SUCCESS;
}


