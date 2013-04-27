/* Translation of intrinsics
   Copyright (C) 2000-2008 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org> and
                  Steven Bosscher <s.bosscher@student.tudelft.nl>

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

#include "trans.h"
#include "intrinsic.h"



/* convert_args()-- Evaluate the arguments to an intrinsic function.
 * This function converts things by value, not reference. */

static tree convert_args(g95_se *se, g95_expr *expr, ...) {
g95_actual_arglist *actual;
g95_se argse, *dest;
va_list ap;
tree args;
int flag;

    va_start(ap, expr);

    args = NULL_TREE;
    dest = NULL;
    flag = 1;

    for(actual=expr->value.function.actual; actual; actual=actual->next) {
	g95_init_se(&argse, se);

	if (actual->u.expr == NULL)
	    argse.expr = null_pointer_node;

	else if (actual->u.expr->rank > 0)
	    g95_conv_descriptor(&argse, actual->u.expr, 0);

	else if (actual->u.expr->ts.type != BT_CHARACTER)
	    g95_conv_expr(&argse, actual->u.expr);

	else {
	    argse.reflevel = 1;
	    g95_conv_expr(&argse, actual->u.expr);

	    args = g95_chainon_list(args, argse.expr);
	    args = g95_chainon_list(args, argse.string_length);
	}

	if (flag) {
	    dest = va_arg(ap, g95_se *);
	    if (dest == NULL)
		flag = 0;
	}

	if (dest != NULL) {
	    g95_init_se(dest, NULL);

	    dest->expr = (argse.expr == null_pointer_node) ? NULL : argse.expr;
	    dest->string_length = argse.string_length;
	}

	args = g95_chainon_list(args, argse.expr);
    }

    va_end(ap);
    return args;
}



/* access_arg()-- Given an expression that is a function call
 * expression, return a particular argument number (zero based). */

static g95_expr *access_arg(g95_expr *e, int narg) {
g95_actual_arglist *a;

    a = e->value.function.actual;

    for(; narg>0; narg--)
	a = a->next;

    return a->u.expr;
}



/* intrinsic_conversion()-- Convert one type to another. */

static void intrinsic_conversion(g95_se *se, g95_expr *expr) {
g95_se arg;
tree type;

    /* Evaluate the argument */
    type = g95_get_typenode(&expr->ts, 0);
    assert(expr->value.function.actual->u.expr);

    convert_args(se, expr, &arg, NULL);

    /* Conversion from complex to non-complex involves taking the real
     * component of the value.  */

    if (TREE_CODE(TREE_TYPE(arg.expr)) == COMPLEX_TYPE
	&& expr->ts.type != BT_COMPLEX) {
	tree artype;

	artype = TREE_TYPE(TREE_TYPE(arg.expr));
	arg.expr = build1(REALPART_EXPR, artype, arg.expr);
    }

    se->expr = convert(type, arg.expr);
}


/* intrinsic_size_name()-- Return the name of the default intrinsic
 * size function. */

static char *intrinsic_size_name(void) {
static char name[20];

    sprintf(name, PREFIX "size_%d", g95_default_integer_kind(0));
    return name;
}



/* simple_libcall()-- Generate a call to a library function.  The
 * function is declared with zero or more arguments, since the
 * argument list has already been vetted. */

static void simple_libcall(g95_se *se, g95_expr *expr) {
tree args, name, type, var, tmp, rtype, decl, len;

    args = g95_trans_arglist(expr->value.function.actual, se);
    name = get_identifier(expr->value.function.iname);
    var = NULL_TREE;

    if (expr->rank > 0)
	rtype = g95_get_array_pdesc(expr->rank, 0);

    else
	switch(expr->ts.type) {
	case BT_LOGICAL: case BT_INTEGER: case BT_REAL:
	    rtype = g95_get_typenode(&expr->ts, 0);
	    break;

	case BT_CHARACTER:
	    len = g95_build_int(expr->value.function.isym->len, 0);
	    var = g95_temp_string(se, len);
	    TREE_ADDRESSABLE(var) = 1;

	    tmp = tree_cons(NULL_TREE, len, NULL_TREE);
	    args = chainon(tmp, args);

	    tmp = tree_cons(NULL_TREE, var, NULL_TREE);

	    args = chainon(tmp, args);
	    rtype = void_type_node;
	    se->string_length = len;

	    break;

	case BT_COMPLEX:
	    type = g95_get_complex_type(expr->ts.kind);
	    var = g95_create_var(type);
	    TREE_ADDRESSABLE(var) = 1;

	    tmp = g95_addr_expr(var);
	    tmp = tree_cons(NULL_TREE, tmp, NULL_TREE);
	    args = chainon(tmp, args);

	    rtype = void_type_node;
	    break;

	default:
	    g95_internal_error("simple_libcall(): Bad function type");
	}

    decl = build_function_type(rtype, NULL_TREE);
    decl = build_decl(FUNCTION_DECL, name, decl);

    DECL_EXTERNAL(decl) = 1;
    TREE_PUBLIC(decl) = 1;

    pushdecl(decl);
    rest_of_decl_compilation(decl, 1, 0);

    tmp = g95_build_function_call(decl, args);

    if (var == NULL_TREE)
	se->expr = tmp;

    else {
	g95_add_expr_to_block(&se->pre, tmp);
	se->expr = var;
    }
}



/* intrinsic_builtin()-- Handle a function call for which there are
 * gcc built-in functions. */

static void intrinsic_builtin(g95_se *se, g95_expr *expr, int r4, int r8) {
tree decl, tmp;

    if (expr->ts.type != BT_REAL)
	goto library;

    switch(expr->ts.kind) {
    case 4:  decl = built_in_decls[r4];  break;
    case 8:  decl = built_in_decls[r8];  break;
    default:
    library:
	simple_libcall(se, expr);
	return;
    }

    if (decl == NULL)
	goto library;

    tmp = convert_args(se, expr, NULL);
    se->expr = g95_build_function_call(decl, tmp);
}



/* g95_dim()-- Generate code for DIM(x, y). */

void g95_dim(g95_se *se, tree x, tree y) {
tree type, val, tmp, zero;

    type = TREE_TYPE(x);

    val = g95_build_minus(type, x, y);
    val = save_expr(val);

    zero = g95_build_const(type, integer_zero_node);
    tmp = g95_build_le(val, zero);
    se->expr = g95_build_cond(type, tmp, zero, val);
}



/* intrinsic_abs()-- Absolute value */

static void intrinsic_abs(g95_se *se, g95_expr *expr) {
tree args;
g95_se a;

    switch(access_arg(expr, 0)->ts.type) {
    case BT_INTEGER:
    case BT_REAL:
	args = convert_args(se, expr, &a, NULL);
	se->expr = g95_build_abs(TREE_TYPE(a.expr), a.expr);
	break;

    case BT_COMPLEX:
	simple_libcall(se, expr);
	break;

    default:
	abort();
    }
}



/* intrinsic_adjust()-- Left or right justification of strings. */

static void intrinsic_adjust(g95_se *se, g95_expr *e, int right_flag) {
tree tmp, result;
g95_se string;
char *name;

    convert_args(se, e, &string);

    result = g95_temp_string(se, string.string_length);

    name = right_flag ? PREFIX "adjustr" : PREFIX "adjustl";
    tmp = g95_call_library(void_type_node, name,
			   result, string.expr, string.string_length,
			   NULL_TREE);

    g95_add_expr_to_block(&se->pre, tmp);

    se->expr = result;
    se->string_length = string.string_length;
}



/* intrinsic_aimag()-- Get the imaginary component of a value. */

static void intrinsic_aimag(g95_se *se, g95_expr *expr) {
tree type;
g95_se z;

    convert_args(se, expr, &z);
    type = g95_get_real_type(expr->ts.kind);
    se->expr = build1(IMAGPART_EXPR, type, z.expr);
}



/* intrinsic_allocated()-- Test for allocation of an allocatable array. */

static void intrinsic_allocated(g95_se *se, g95_expr *expr) {
g95_expr *arg;
g95_se se0;
tree tmp;

    arg = access_arg(expr, 0);

    g95_init_se(&se0, se);
    se0.reflevel = 1;

    g95_conv_expr(&se0, arg);

    tmp = (!g95_coarray_expr(arg) || arg->rank > 0)
	? g95_base_value(se0.expr)
	: se0.expr;

    se->expr = g95_build_ne(tmp, null_pointer_node);
    se->expr = convert(g95_get_logical_type(expr->ts.kind), se->expr);
}



/* intrinsic_associated()-- Test for pointer association */

static void intrinsic_associated(g95_se *se, g95_expr *expr) {
g95_expr *pointer_expr, *target_expr;
tree pointer, target, t1, t2, p;
g95_se se0;

    pointer_expr = access_arg(expr, 0);
    target_expr = access_arg(expr, 1);

    if (pointer_expr->rank > 0 && target_expr != NULL &&
	target_expr->rank > 0) {
	g95_init_se(&se0, se);

	se0.reflevel = 1;
	g95_conv_descriptor(&se0, pointer_expr, 1);
	pointer = se0.expr;

	se0.reflevel = 1;
	g95_conv_descriptor(&se0, target_expr, 1);
	target = se0.expr;

	se->expr = g95_call_library(g95_default_integer,
				    PREFIX "compare_section",
				    pointer, target, NULL_TREE);
	se->expr = convert(boolean_type_node, se->expr);

    } else {
	g95_init_se(&se0, se);
	p = boolean_true_node;

	if (pointer_expr->rank == 0) {
	    se0.reflevel = 1;
	    g95_conv_expr(&se0, pointer_expr);

	    if (pointer_expr->ts.type == BT_CHARACTER)
		p = g95_build_ne(se0.string_length, integer_zero_node);

	} else {
	    se0.reflevel = 0;
	    g95_conv_descriptor(&se0, pointer_expr, 1);
	    se0.expr = g95_base_ref(se0.expr);
	}

	pointer = se0.expr;

	if (target_expr == NULL) { /* See if the pointer is associated */
	    t1 = g95_build_ne(pointer, null_pointer_node);
	    se->expr = g95_build_andif(p, t1);
	    se->expr = convert(g95_get_logical_type(expr->ts.kind), se->expr);
	    return;
	}

	/* See if the pointer is associated with a particular target */

	g95_init_se(&se0, se);

	se0.reflevel = (target_expr->rank > 0) ? 0 : 1;
	g95_conv_expr(&se0, target_expr);

	target = se0.expr;
	if (target_expr->rank > 0)
	    target = g95_base_value(target);

	t1 = g95_build_eq(pointer, target);
	t2 = g95_build_ne(pointer, null_pointer_node);

	p = g95_build_andif(p, t1);
	se->expr = g95_build_andif(p, t2);
	se->expr = convert(g95_get_logical_type(expr->ts.kind), se->expr);
    }
}



/* intrinsic_bitop()-- Generate code to perform logical and, or and
 * exclusive-or. */

static void intrinsic_bitop(g95_se *se, g95_expr *expr, int op) {
g95_se i, j;

    convert_args(se, expr, &i, &j);
    se->expr = fold(build2(op, TREE_TYPE(i.expr), i.expr, j.expr));
}



/* intrinsic_bound()-- Build an expression for LBOUND or UBOUND.  If
 * the DIM parameter is not present, we call a library subroutine. */

static void intrinsic_bound(g95_se *se, g95_expr *expr, int upper) {
g95_se array, dim;

    if (access_arg(expr, 1) == NULL)
	simple_libcall(se, expr);

    else {
	convert_args(se, expr, &array, &dim, NULL);
	se->expr = g95_desc_info(array.expr, dim.expr, upper ? -1 : -2);
	se->expr = convert(g95_default_integer, se->expr);
    }
}



/* intrinsic_btest()-- Bit test.  BTEST(i, pos) = (i & (1 << pos)) != 0  */

static void intrinsic_btest(g95_se *se, g95_expr *expr) {
tree tmp, type;
g95_se i, pos;

    convert_args(se, expr, &i, &pos);
    type = TREE_TYPE(i.expr);

    tmp = convert(type, integer_one_node);

    tmp = g95_build_lshift(type, tmp, pos.expr);
    tmp = g95_build_and(type, i.expr, tmp);
    se->expr = g95_build_ne(tmp, integer_zero_node);
}



/* intrinsic_char()-- Generate a string of length one. */

static void intrinsic_char(g95_se *se, g95_expr *e) {
tree var, tmp;
g95_se i;

    convert_args(se, e, &i, NULL);

    tmp = build_index_type(integer_one_node);
    tmp = build_array_type(g95_character1_type_node, tmp);
    var = g95_create_var(tmp);

    tmp = g95_build_array_ref(g95_character1_type_node, var,integer_zero_node);

    g95_add_modify_expr(&se->pre, tmp,
			convert(g95_character1_type_node, i.expr));

    se->expr = g95_addr_expr(var);
    se->string_length = convert(g95_default_integer, integer_one_node);
}



/* intrinsic_ceiling_floor()-- Handle the CEILING() and FLOOR()
 * intrinsics */

static void intrinsic_ceiling_floor(g95_se *se, g95_expr *expr) {
tree type;

    simple_libcall(se, expr);

    if (expr->ts.kind != g95_default_integer_kind(0)) {
	type = g95_get_int_type(expr->ts.kind);
	se->expr = convert(type, se->expr);
    }
}



/* intrinsic_cmplx()-- Create a complex value from one or two real
 * components. */

static void intrinsic_cmplx(g95_se *se, g95_expr *expr) {
tree tmp, type;
g95_se x, y;
g95_expr *e;

    convert_args(se, expr, &x, &y, NULL);

    if (access_arg(expr, 0)->ts.type == BT_COMPLEX) {
	type = g95_get_complex_type(expr->ts.kind);
	se->expr = convert(type, x.expr);

    } else {
	type = g95_get_real_type(expr->ts.kind);
	x.expr = convert(type, x.expr);

	e = expr->value.function.actual->next->u.expr;

	if (y.expr == NULL_TREE)
	    y.expr = integer_zero_node;

	else if (e != NULL && e->type == EXPR_VARIABLE &&
		 e->symbol->attr.optional &&
		 !G95_ARRAY(e->symbol->backend_decl)) {

	    tmp = g95_build_eq(e->symbol->backend_decl, null_pointer_node);
	    y.expr = g95_build_cond(TREE_TYPE(y.expr), tmp,
				    convert(type, integer_zero_node), y.expr);
	}

	y.expr = convert(type, y.expr);

	type = g95_get_complex_type(expr->ts.kind);
	se->expr = fold(build2(COMPLEX_EXPR, type, x.expr, y.expr));
    }
}



/* intrinsic_conjg()-- Get the complex conjugate. */

static void intrinsic_conjg(g95_se *se, g95_expr *expr) {
g95_se z;

    convert_args(se, expr, &z);
    se->expr = build1(CONJ_EXPR, TREE_TYPE(z.expr), z.expr);
}



/* intrinsic_dim()-- Calculate a positive difference,
 * DIM(x, y) = ((x - y) < 0) ? 0 : x - y.  */

static void intrinsic_dim(g95_se *se, g95_expr *expr) {
g95_se x, y;

    convert_args(se, expr, &x, &y);
    g95_dim(se, x.expr, y.expr);
}



/* intrinsic_dprod()-- Double precision product of two single
 * precision values. */

static void intrinsic_dprod(g95_se *se, g95_expr *expr) {
g95_se x, y;
tree type;

    convert_args(se, expr, &x, &y);

    type = g95_get_typenode(&expr->ts, 0);
    x.expr = convert(type, x.expr);
    y.expr = convert(type, y.expr);

    se->expr = g95_build_mult(type, x.expr, y.expr);
}



/* intrinsic_ibits()-- Extract a sequence of bits.
 * IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN).  */

static void intrinsic_ibits(g95_se *se, g95_expr *expr) {
tree type, tmp, mask, p;
g95_se i, pos, len;
int m;

    convert_args(se, expr, &i, &pos, &len);
    type = TREE_TYPE(i.expr);

    tmp = g95_create_var(type);
    g95_add_modify_expr(&se->pre, tmp, i.expr);
    i.expr = tmp;

    len.expr = save_expr(len.expr);

    mask = g95_build_int(-1, ~(unsigned HOST_WIDE_INT) 0);
    mask = convert(type, mask);
    mask = g95_build_lshift(type, mask, len.expr);
    mask = fold(build1(BIT_NOT_EXPR, type, mask));

    tmp = g95_build_rshift(type, i.expr, pos.expr);
    tmp = g95_build_and(type, tmp, mask);

    /* If len is equal to the bit size, then pos has to be zero, and the
     * result is simply i.  The logical expression above doesn't work in
     * this case on x86 machines with a 32 bit word size and a 5 bit
     * barrel shifter. */

    m = access_arg(expr, 0)->ts.kind;
    m = g95_validate_kind(BT_LOGICAL, m);
    m = g95_integer_kinds[m].bit_size;

    p = g95_build_int(m, 0);
    p = g95_build_eq(len.expr, p);

    se->expr = g95_build_cond(type, p, i.expr, tmp);
}



/* intrinsic_ichar()-- Return the integer of the first character in a
 * string. */

static void intrinsic_ichar(g95_se *se, g95_expr *expr) {
tree tmp;
g95_se c;

    g95_init_se(&c, se);
    g95_conv_expr(&c, access_arg(expr, 0));

    if (TREE_CODE(TREE_TYPE(c.expr)) == ARRAY_TYPE)
	c.expr = g95_build_array_ref(TREE_TYPE(TREE_TYPE(c.expr)), c.expr,
				     integer_zero_node);

    tmp = convert(unsigned_char_type_node, c.expr);
    se->expr = convert(g95_default_integer, tmp);
}




static void intrinsic_index(g95_se *se, g95_expr *expr) {

    expr->value.function.iname = g95_get_string(PREFIX "index3");
    simple_libcall(se, expr);
}



/* intrinsic_ishft()-- Care must be taken if the number of shifts is
 * greater than the word size.  The SHIFT_EXPRs are undefined for more
 * shifts than bits but the fortran standard is not. */

static void intrinsic_ishft(g95_se *se, g95_expr *expr) {
tree i, shifts, type, tmp, lshift, rshift, result, word_size, utype;
g95_se se0;
int m;

    g95_init_se(&se0, se);
    g95_conv_expr(&se0, access_arg(expr, 0));

    type = TREE_TYPE(se0.expr);
    utype = g95_unsigned_type(type);

    g95_save_expr(&se0);
    i = se0.expr;

    g95_conv_expr(&se0, access_arg(expr, 1));
    shifts = save_expr(se0.expr);

    /* Right shifts if nonpositive */
    tmp = fold(build1(NEGATE_EXPR, g95_default_integer, shifts));
    rshift = g95_build_rshift(utype, i, tmp);

    /* Left shifts if positive */
    lshift = g95_build_lshift(utype, i, shifts);

    tmp = g95_build_ge(shifts, integer_zero_node);
    result = g95_build_cond(type, tmp, lshift, rshift);

    /* Too many shifts mean a zero result */

    m = g95_validate_kind(BT_INTEGER, access_arg(expr, 0)->ts.kind);
    m = g95_integer_kinds[m].bit_size;
    word_size = g95_build_int(m, 0);

    tmp = g95_build_abs(TREE_TYPE(shifts), shifts);
    tmp = g95_build_ge(tmp, word_size);

    se->expr = g95_build_cond(type, tmp, integer_zero_node, result);
}



/* intrinsic_ishftc()-- Circular shift. */

static void intrinsic_ishftc(g95_se *se, g95_expr *expr) {
tree type, t, tmp, lrot, rrot, m, bitsize, p, q;
g95_se i, shift, size;
int n;

    convert_args(se, expr, &i, &shift, &size);
    type = g95_unsigned_type(TREE_TYPE(i.expr));

    tmp = g95_create_var(type);
    g95_add_modify_expr(&se->pre, tmp, i.expr);
    i.expr = tmp;

    if (size.expr == NULL_TREE) { /* Special case */
	lrot = fold(build2(LROTATE_EXPR, type, i.expr, shift.expr));

	tmp = fold(build1(NEGATE_EXPR, TREE_TYPE(shift.expr), shift.expr));
	rrot = fold(build2(RROTATE_EXPR, type, i.expr, tmp));

	tmp = g95_build_gt(shift.expr, integer_zero_node);
	t = g95_build_cond(type, tmp, lrot, rrot);

	/* Do nothing if shift == 0.  */
	tmp = g95_build_eq(shift.expr, integer_zero_node);
	se->expr = g95_build_cond(type, tmp, i.expr, t);

    } else {
	/* General case.  Let M = (~0) >> (bitsize(I) - size)
	 * If S>0:
	 *   result = (I & ~M) | ((I << S) & M) | (((I & M) >> (size-S)) & M)
	 * If S<0:
	 *   result = (I & ~M) | (((I & M) >> -S) & M) | ((I << (size+S)) & M)
	 * If S==0 || abs(S) == BIT_SIZE:
	 *   result = I
	 */

	shift.expr = save_expr(shift.expr);

	tmp = g95_create_var(g95_default_integer);
	g95_add_modify_expr(&se->pre, tmp, size.expr);
	size.expr = tmp;

	/* Calculate bitsize and the mask */

	n = g95_validate_kind(BT_INTEGER, access_arg(expr, 0)->ts.kind);
	bitsize = g95_build_int(g95_integer_kinds[n].bit_size, 0);

	tmp = g95_build_minus(g95_default_integer, bitsize, size.expr);

	t = convert(type, integer_zero_node);
	t = fold(build1(BIT_NOT_EXPR, type, t));
	t = g95_build_rshift(type, t, tmp);

	m = g95_create_var(type);
	g95_add_modify_expr(&se->pre, m, t);

	/* Left shift */

	tmp = g95_build_and(type, i.expr, m);
	t = g95_build_minus(g95_default_integer, size.expr, shift.expr);
	tmp = g95_build_rshift(type, tmp, t);
	tmp = g95_build_and(type, m, tmp);

	t = g95_build_lshift(type, i.expr, shift.expr);
	t = g95_build_and(type, t, m);
	lrot = g95_build_ior(type, tmp, t);

	/* Right shift */

	tmp = g95_build_plus(g95_default_integer, size.expr, shift.expr);
	tmp = g95_build_lshift(type, i.expr, tmp);
	tmp = g95_build_and(type, m, tmp);

	p = g95_build_and(type, i.expr, m);
	t = fold(build1(NEGATE_EXPR, g95_default_integer, shift.expr));
	t = g95_build_rshift(type, p, t);
	t = g95_build_and(type, m, t);

	rrot = g95_build_ior(type, tmp, t);

	tmp = g95_build_gt(shift.expr, integer_zero_node);
	tmp = g95_build_cond(type, tmp, lrot, rrot);

	/* Final term */

	t = fold(build1(BIT_NOT_EXPR, type, m));
	t = g95_build_and(type, t, i.expr);
	t = g95_build_ior(type, t, tmp);

	/* Zero shift condition */

	p = g95_build_eq(shift.expr, integer_zero_node);

	q = g95_build_abs(g95_default_integer, shift.expr);
	q = g95_build_eq(q, bitsize);

	tmp = fold(build2(TRUTH_OR_EXPR, boolean_type_node, p, q));

	se->expr = g95_build_cond(type, tmp, i.expr, t);
    }
}



/* intrinsic_int()-- Convert to an integer using the specified
 * rounding mode. */

static void intrinsic_int(g95_se *se, g95_expr *expr) {
tree type, tmp;
g95_se a;

    type = g95_get_typenode(&expr->ts, 0);
    convert_args(se, expr, &a, NULL);

    switch(access_arg(expr, 0)->ts.type) {
    case BT_INTEGER:
	se->expr = convert(type, a.expr);   /* Convert to a different kind */
	break;

    case BT_COMPLEX:
	tmp = g95_get_real_type(access_arg(expr, 0)->ts.kind);
	a.expr = build1(REALPART_EXPR, tmp, a.expr);

	/* Fall through */

    case BT_REAL:
	se->expr = build1(FIX_TRUNC_EXPR, type, a.expr);
	break;

    default:
	abort();
    }
}



/* intrinsic_len()-- The length of a character string.  */

static void intrinsic_len(g95_se *se, g95_expr *expr) {

    g95_conv_charlen_expr(se, access_arg(expr, 0));
}



/* intrinsic_loc()-- Implement the LOC() extension. */

static void intrinsic_loc(g95_se *se, g95_expr *expr) {
g95_expr *e;
g95_se se0;

    g95_init_se(&se0, se);
    se0.reflevel = 1;
    e = access_arg(expr, 0);

    if (e->rank == 0)
	g95_conv_expr(&se0, access_arg(expr, 0));

    else
	g95_conv_nondesc(&se0, INTENT_IN, e, null_pointer_node);

    se->expr = convert(g95_pointer_integer, se0.expr);
}



/* intrinsic_merge()-- The MERGE() intrinsic is just like the ternary
 * conditional operator in C. */

static void intrinsic_merge(g95_se *se, g95_expr *expr) {
g95_se t, f, mask;

    convert_args(se, expr, &t, &f, &mask);

    se->expr = g95_build_cond(TREE_TYPE(t.expr), mask.expr, t.expr, f.expr);
    se->string_length = t.string_length;
}



/* numeric_minmax()-- Get the minimum or maximum value of all the
 * parameters. */

static void numeric_minmax(g95_se *se, g95_expr *expr, int op) {
tree extremum, arg, tmp, p, type;
g95_actual_arglist *a;
g95_typespec ts;
stmtblock_t b;
g95_expr *e;

    arg = convert_args(se, expr, NULL);
    type = g95_get_typenode(&expr->ts, 0);

    extremum = g95_create_var(type);

    g95_add_modify_expr(&se->pre, extremum, convert(type, TREE_VALUE(arg)));
    a = expr->value.function.actual->next;

    for(arg=TREE_CHAIN(arg); arg!=NULL_TREE; arg=TREE_CHAIN(arg), a=a->next) {
	e = a->u.expr;

	tmp = build2(op, type, extremum, convert(type, TREE_VALUE(arg)));

	if (e->type != EXPR_VARIABLE || !e->symbol->attr.optional)
	    g95_add_modify_expr(&se->pre, extremum, tmp);

	else {
	    g95_init_block(&b);
	    g95_add_modify_expr(&b, extremum, tmp);

	    p = e->symbol->backend_decl;
	    if (G95_ARRAY(p))
		p = G95_ARRAY_DUMMY(p);

	    p = g95_build_ne(p, null_pointer_node);
	    tmp = g95_build_cond(void_type_node, p, g95_finish_block(&b),
				 empty_stmt_node);
	    g95_add_expr_to_block(&se->pre, tmp);
	}
    }

    se->expr = extremum;

    ts = access_arg(expr, 0)->ts;
    if (expr->ts.type != ts.type || expr->ts.kind != ts.kind)
	se->expr = convert(g95_get_typenode(&expr->ts, 0), se->expr);
}



/* character_minmax()-- Get the minimum or maximum value of all the
 * parameters.  This is a little different than the numeric case. */

static void character_minmax(g95_se *se, g95_expr *expr, int op) {
tree tmp, extremum, extremum_length;
g95_actual_arglist *a;
g95_se se0;

    extremum = g95_create_var(pchar_type_node);
    extremum_length = g95_create_var(g95_default_integer);

    a = expr->value.function.actual;

    g95_init_se(&se0, se);
    se0.reflevel = 1;
    g95_conv_expr(&se0, a->u.expr);

    g95_add_modify_expr(&se->pre, extremum, se0.expr);
    g95_add_modify_expr(&se->pre, extremum_length, se0.string_length);

    a = a->next;
    for(; a; a=a->next) {
	g95_init_se(&se0, se);
	se0.reflevel = 1;
	g95_conv_expr(&se0, a->u.expr);

	tmp = (op == MAX_EXPR)
	    ? integer_one_node
	    : integer_zero_node;

	tmp = g95_call_library(void_type_node, PREFIX "string_minmax", tmp,
			       g95_addr_expr(extremum),
			       g95_addr_expr(extremum_length),
			       se0.expr, se0.string_length, NULL_TREE);

	g95_add_expr_to_block(&se->pre, tmp);
    }

    se->expr = extremum;
    se->string_length = extremum_length;
}



/* intrinsic_minmax()-- Get the minimum or maximum value of all the
 * parameters. */

static void intrinsic_minmax(g95_se *se, g95_expr *expr, int op) {

    if (expr->ts.type == BT_CHARACTER)
	character_minmax(se, expr, op);
    else
	numeric_minmax(se, expr, op);
}



/* intrinsic_mod()-- Translate the MOD() intrinsic. */

static void intrinsic_mod(g95_se *se, g95_expr *expr) {
g95_se a, p;

    switch(expr->ts.type) {
    case BT_INTEGER:
	convert_args(se, expr, &a, &p);
	se->expr = build2(TRUNC_MOD_EXPR, TREE_TYPE(a.expr), a.expr, p.expr);
	break;

    case BT_REAL:
	simple_libcall(se, expr);
	break;

    default:
	abort();
    }
}



/* intrinsic_modulo()-- Translate the MODULO intrinsics. */

static void intrinsic_modulo(g95_se *se, g95_expr *expr) {
g95_se a, p;

    switch(expr->ts.type) {
    case BT_INTEGER:
	convert_args(se, expr, &a, &p);
	se->expr = build2(FLOOR_MOD_EXPR, TREE_TYPE(a.expr), a.expr, p.expr);
	break;

    case BT_REAL:
	simple_libcall(se, expr);
	break;

    default:
	abort();
    }
}



/* intrinsic_mvbits()-- Call the mvbits intrinsic.  In order to deal
 * with a multitude of combinations of integer kinds, the kind
 * parameter is passed behind each pointer. */

static tree intrinsic_mvbits(g95_actual_arglist *a) {
g95_expr *from, *frompos, *len, *to, *topos;
g95_se se1, se2, se3, se4, se5;
stmtblock_t block;
tree tmp;

    from    = a->u.expr;
    frompos = a->next->u.expr;
    len     = a->next->next->u.expr;
    to      = a->next->next->next->u.expr;
    topos   = a->next->next->next->next->u.expr;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);
    se1.reflevel = 1;
    g95_conv_expr(&se1, from);
    g95_add_block_to_block(&block, &se1.pre);

    g95_init_se(&se2, NULL);
    se2.reflevel = 1;
    g95_conv_expr(&se2, frompos);
    g95_add_block_to_block(&block, &se2.pre);

    g95_init_se(&se3, NULL);
    se3.reflevel = 1;
    g95_conv_expr(&se3, len);
    g95_add_block_to_block(&block, &se3.pre);

    g95_init_se(&se4, NULL);
    se4.reflevel = 1;
    g95_conv_expr(&se4, to);
    g95_add_block_to_block(&block, &se4.pre);

    g95_init_se(&se5, NULL);
    se5.reflevel = 1;
    g95_conv_expr(&se5, topos);
    g95_add_block_to_block(&block, &se5.pre);

    tmp = g95_call_library(void_type_node, PREFIX "mvbits",
			   se1.expr, g95_build_int(from->ts.kind, 0),
			   se2.expr, g95_build_int(frompos->ts.kind, 0),
			   se3.expr, g95_build_int(len->ts.kind, 0),
			   se4.expr, g95_build_int(to->ts.kind, 0),
			   se5.expr, g95_build_int(topos->ts.kind, 0), NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);
    g95_add_block_to_block(&block, &se3.post);
    g95_add_block_to_block(&block, &se4.post);
    g95_add_block_to_block(&block, &se5.post);

    return g95_finish_block(&block);
}



/* intrinsic_move_alloc()-- Handle a MOVE_ALLOC call. */

static tree intrinsic_move_alloc(g95_actual_arglist *a) {
g95_expr *from, *to;
stmtblock_t block;
g95_se se1, se2;
tree tmp, t;

    from = a->u.expr;
    to = a->next->u.expr;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);
    se1.reflevel = 1;
    g95_conv_descriptor(&se1, from, 0);

    g95_init_se(&se2, NULL);
    se2.reflevel = 1;
    g95_conv_descriptor(&se2, to, 0);

    g95_add_block_to_block(&block, &se1.pre);
    g95_add_block_to_block(&block, &se2.pre);

    if (from->ts.type != BT_DERIVED)
	t = null_pointer_node;

    else {
	t = G95_DTYPE_ALLOCS(from->ts.derived->backend_decl);
	if (t == NULL)
	    t = null_pointer_node;
    }

    tmp = g95_call_library(void_type_node, PREFIX "move_alloc",
			   se1.expr, se2.expr, t, NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);

    return g95_finish_block(&block);
}



/* intrinsic_nint()-- Nearest integer.  Real argument, integer result */

static void intrinsic_nint(g95_se *se, g95_expr *expr) {
tree type;

    simple_libcall(se, expr);

    if (expr->ts.kind != g95_default_integer_kind(0)) {
	type = g95_get_int_type(expr->ts.kind);
	se->expr = convert(type, se->expr);
    }
}



/* intrinsic_not()-- Logical not. */

static void intrinsic_not(g95_se *se, g95_expr *expr) {
g95_se i;

    convert_args(se, expr, &i);
    se->expr = build1(BIT_NOT_EXPR, TREE_TYPE(i.expr), i.expr);
}



/* intrinsic_pack()-- PACK() intrinsic. */

static void intrinsic_pack(g95_se *se, g95_expr *expr) {
tree array, mask, vector, alloc;
g95_expr *mask_expr;
g95_typespec *ts;
char *name;
int save;

    save = se->reflevel;

    se->reflevel = 1; 
    g95_conv_descriptor(se, access_arg(expr, 0), 0);
    array = se->expr;

    se->reflevel = 1;

    mask_expr = access_arg(expr, 1);

    if (mask_expr->rank == 0) 
	g95_conv_expr(se, mask_expr);

    else
	g95_conv_descriptor(se, mask_expr, 0);

    mask = se->expr;

    if (access_arg(expr, 2) == NULL)
	vector = null_pointer_node;

    else {
	se->reflevel = 1; 
	g95_conv_descriptor(se, access_arg(expr, 2), 0);
	vector = se->expr;
    }

    ts = &access_arg(expr, 0)->ts;

    alloc = (ts->type == BT_DERIVED)
	? G95_DTYPE_ALLOCS(ts->derived->backend_decl)
	: null_pointer_node;

    if (alloc == NULL)
	alloc = null_pointer_node;

    name = (mask_expr->rank == 0)
	? PREFIX "pack_s"
	: PREFIX "pack";

    se->reflevel = save;

    se->expr = g95_call_library(g95_get_array_pdesc(1, 0), name,
				array, mask, vector, alloc, NULL_TREE);
}



/* intrinsic_present()-- Test for the presence of an argument */

static void intrinsic_present(g95_se *se, g95_expr *expr) {
g95_expr *e;
tree decl;

    e = access_arg(expr, 0);
    decl = e->symbol->backend_decl;

    if (e->rank > 0)
	decl = G95_ARRAY_DUMMY(decl);

    se->expr = g95_build_ne(decl, null_pointer_node);
    se->expr = convert(g95_get_logical_type(expr->ts.kind), se->expr);
}



/* intrinsic_repeat()-- Repeat a given string. */

static void intrinsic_repeat(g95_se *se, g95_expr *e) {
g95_se string, copies;
tree length, var, tmp;

    convert_args(se, e, &string, &copies);

    copies.expr = save_expr(g95_build_max(g95_default_integer,
					  copies.expr, integer_zero_node));

    length = save_expr(g95_build_mult(g95_default_integer, copies.expr,
				      string.string_length));

    var = g95_temp_string(se, length);
    tmp = g95_call_library(void_type_node, PREFIX "repeat",
			   var, string.expr, string.string_length, copies.expr,
			   NULL);

    g95_add_expr_to_block(&se->pre, tmp);

    se->expr = var;
    se->string_length = length;
}



/* intrinsic_selected_int_kind()-- Figure out a integer kind value
 * given a range.  We implement this as a nested conditional
 * statement generated from the g95_integer_kinds[] table. */

static void intrinsic_selected_int_kind(g95_se *se, g95_expr *expr) {
int i, j, kind_index;
tree tmp, rest, k;
g95_se r;

    convert_args(se, expr, &r);

    r.expr = save_expr(r.expr);
    rest = integer_minus_one_node;

    /* Find the largest kind */

    kind_index = 0; 

    for(i=1; g95_integer_kinds[i].kind != 0; i++)
	if (g95_integer_kinds[i].kind > g95_integer_kinds[kind_index].kind)
	    kind_index = i;

    /* Loop over integer kinds from largest kind to smallest */

    do {
	tmp = g95_build_int(g95_integer_kinds[kind_index].range, 0);
	tmp = g95_build_le(r.expr, tmp);

	k = g95_build_int(g95_integer_kinds[kind_index].kind, 0);
	rest = g95_build_cond(g95_default_integer, tmp, k, rest);

	/* Find the next smaller kind */

	j = -1;
	for(i=0; g95_integer_kinds[i].kind != 0; i++)
	    if ((j == -1 ||
		 g95_integer_kinds[i].kind > g95_integer_kinds[j].kind) &&
		g95_integer_kinds[i].kind < g95_integer_kinds[kind_index].kind)
		j = i;

	kind_index = j;
    } while(kind_index != -1);

    se->expr = rest;
}



/* intrinsic_selected_real_kind()-- Figure out a integer kind value
 * given a range and/or precision.  We implement this as a nested
 * conditional statements generated from the g95_real_kinds[] table. */

static void intrinsic_selected_real_kind(g95_se *se, g95_expr *expr) {
tree tmp, p_rest, r_rest, r1, r2, t1, t2, k, cond, p, r;
int i, j, kind_index;
g95_se p0, r0;

    convert_args(se, expr, &p0, &r0);
    p = p0.expr;
    r = r0.expr;
    p_rest = r_rest = NULL;

    if (p != NULL_TREE) {
	p = save_expr(convert(g95_default_integer, p));
	p_rest = integer_minus_one_node;
    }

    if (r != NULL_TREE) {
	r = save_expr(convert(g95_default_integer, r));
	r_rest = g95_build_int(-2, -1);
    }

    assert(p != NULL_TREE || r != NULL_TREE);
    kind_index = 0;

    /* Largest kind */

    for(i=1; g95_real_kinds[i].kind != 0; i++)
	if (g95_real_kinds[i].kind > g95_real_kinds[kind_index].kind)
	    kind_index = i;

    /* Generate tree for p and r branches */

    do {
	k = g95_build_int(g95_real_kinds[kind_index].kind, 0);

	if (p != NULL) {
	    tmp = g95_build_int(g95_real_kinds[kind_index].precision, 0);
	    tmp = g95_build_le(p, tmp);
		    
	    p_rest = g95_build_cond(g95_default_integer, tmp, k, p_rest);
	}

	if (r != NULL) {
	    tmp = g95_build_int(g95_real_kinds[kind_index].range, 0);
	    tmp = g95_build_le(r, tmp);

	    r_rest = g95_build_cond(g95_default_integer, tmp, k, r_rest);
	}

	/* Find the next highest real kind */

	j = -1;
	for(i=0; g95_real_kinds[i].kind != 0; i++)
	    if ((j == -1 || g95_real_kinds[i].kind > g95_real_kinds[j].kind) &&
		g95_real_kinds[i].kind < g95_real_kinds[kind_index].kind)
		j = i;

	kind_index = j;
    } while(kind_index != -1);

    /* If we've only got one parameter, then we already have the answer. */

    if (p == NULL_TREE) {
	se->expr = r_rest;
	return;
    }

    if (r == NULL_TREE) {
	se->expr = p_rest;
	return;
    }

    /* We have two parameters.  If p and r are nonnegative, the result
     * is the larger of the two.  If p and r are negative, the result
     * is -3, otherwise the result is the smaller of the two and is a
     * negative number. */

    p = save_expr(p_rest);
    r = save_expr(r_rest);

    t1 = g95_build_lt(p, integer_zero_node);
    t2 = g95_build_lt(r, integer_zero_node);
    cond = g95_build_and(boolean_type_node, t1, t2);

    r1 = g95_build_int(-3, -1);
    r2 = build2(MIN_EXPR, g95_default_integer, p, r);

    r2 = g95_build_cond(g95_default_integer, cond, r1, r2);

    /* First part */

    t1 = g95_build_ge(p, integer_zero_node);
    t2 = g95_build_ge(r, integer_zero_node);
    cond = g95_build_and(boolean_type_node, t1, t2);

    r1 = g95_build_max(g95_default_integer, p, r);
    se->expr = g95_build_cond(g95_default_integer, cond, r1, r2);
}



/* intrinsic_sign()-- For integers, SIGN(A, B) is absolute value of A
 * times sign of B.  The real value versions use library functions to
 * ensure the correct handling of negative zero.  The integer case is
 * implemented as: SIGN(A, B) = (b >= 0) ? |a| : -|a| */

static void intrinsic_sign(g95_se *se, g95_expr *expr) {
tree arg, tmp, type, var;
g95_se a, b;

    if (expr->ts.type == BT_REAL)
	simple_libcall(se, expr);

    else {
	arg = convert_args(se, expr, &a, &b);
	type = TREE_TYPE(a.expr);

	var = g95_create_var(type);
	g95_add_modify_expr(&se->pre, var, g95_build_abs(type, a.expr));

	tmp = g95_build_ge(b.expr, integer_zero_node);
	se->expr = g95_build_cond(type, tmp, var,
				  build1(NEGATE_EXPR, type, var));
    }
}



/* intrinsic_singlebitop()-- Set or clear a single bit.  */

static void intrinsic_singlebitop(g95_se *se, g95_expr *expr, int set) {
tree type, mask;
g95_se i, pos;
int op;

    convert_args(se, expr, &i, &pos);
    type = TREE_TYPE(i.expr);

    mask = convert(type, integer_one_node);
    mask = g95_build_lshift(type, mask, pos.expr);

    if (set)
	op = BIT_IOR_EXPR;
    else {
	op = BIT_AND_EXPR;
	mask = fold(build1(BIT_NOT_EXPR, type, mask));
    }

    se->expr = fold(build2(op, type, i.expr, mask));
}



/* intrinsic_size()-- implement the intrinsic SIZE function.  If the
 * DIM parameter is not present, we call a library subroutine. */

static void intrinsic_size(g95_se *se, g95_expr *expr) {
g95_se array, dim, kind;
tree ubound, lbound;

    if (access_arg(expr, 1) == NULL)
	simple_libcall(se, expr);

    else {
	convert_args(se, expr, &array, &dim, &kind);
	dim.expr = save_expr(dim.expr);

	ubound = g95_desc_info(array.expr, dim.expr, -1);
	lbound = g95_desc_info(array.expr, dim.expr, -2);

	ubound = g95_build_plus(g95_pointer_integer, ubound, integer_one_node);
	ubound = fold(ubound);

	g95_dim(se, ubound, lbound);
	se->expr = convert(g95_default_integer, se->expr);
    }
}


/* intrinsic_sizeof()-- SIZEOF extension, return the size of the
 * object in bytes. */

static void intrinsic_sizeof(g95_se *se, g95_expr *expr) {
g95_expr *e;
char *name;
g95_se se0;
tree tmp;

    e = access_arg(expr, 0);
    g95_init_se(&se0, se);

    if (e->rank == 0) {
	g95_conv_expr(&se0, e);

	se->expr = (e->ts.type == BT_CHARACTER)
	    ? se0.string_length
	    : size_in_bytes(TREE_TYPE(se0.expr));

    } else {
	se0.reflevel = 1;
	g95_conv_descriptor(&se0, e, 0);

	name = intrinsic_size_name();
	tmp = g95_call_library(g95_default_integer, name, se0.expr, NULL_TREE);

	se->expr =
	    g95_build_mult(g95_default_integer, g95_esize_ref(se0.expr), tmp);
    }

    g95_raise_chains(&se0);
}


/* intrinsic_spread()-- Implement SPREAD().  For the version for a
 * scalar source variable, we have to add the length of the source to
 * the argument list. */

static void intrinsic_spread(g95_se *se, g95_expr *expr) {
tree arglist, size, decl, name, type;

    if (access_arg(expr, 0)->rank != 0)
	simple_libcall(se, expr);

    else {
	arglist = g95_trans_arglist(expr->value.function.actual, se);

	size = g95_ts_size(&access_arg(expr, 0)->ts);
	g95_chainon_list(arglist, size);

	name = get_identifier(expr->value.function.iname);

	type = g95_get_array_pdesc(expr->rank, 0);
	decl = build_function_type(type, NULL);
	decl = build_decl(FUNCTION_DECL, name, decl);

	DECL_EXTERNAL(decl) = 1;
	TREE_PUBLIC(decl) = 1;

	pushdecl(decl);
	rest_of_decl_compilation(decl, 1, 0);

	se->expr = g95_build_function_call(decl, arglist);
    }
}



/* intrinsic_strcmp()-- Intrinsic string comparison functions. */

static void intrinsic_strcmp(g95_se *se, g95_expr *expr, int op) {
g95_se a, b;

    convert_args(se, expr, &a, &b); 

    se->expr = g95_call_library(g95_default_integer, PREFIX "compare_string",
				a.expr, a.string_length, b.expr,
				b.string_length, NULL_TREE);

    se->expr = build2(op, boolean_type_node, se->expr, integer_zero_node);
    se->expr = convert(g95_get_logical_type(expr->ts.kind), se->expr);
}



/* intrinsic_system_clock()-- Intrinsic system_clock subroutine. */

static tree intrinsic_system_clock(g95_actual_arglist *a) {
int count_kind, count_rate_kind, count_max_kind;
g95_expr *count, *count_rate, *count_max;
g95_se se1, se2, se3;
stmtblock_t block;
tree tmp;

    count      = a->u.expr;
    count_rate = a->next->u.expr;
    count_max  = a->next->next->u.expr;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);

    if (count == NULL) {
	se1.expr = null_pointer_node;
	count_kind = 0;

    } else {
	se1.reflevel = 1;
	g95_conv_expr(&se1, count);
	g95_add_block_to_block(&block, &se1.pre);
	count_kind = count->ts.kind; 
    }

    g95_init_se(&se2, NULL);

    if (count_rate == NULL) {
	se2.expr = null_pointer_node;
	count_rate_kind = 0;

    } else {
	se2.reflevel = 1;
	g95_conv_expr(&se2, count_rate);
	g95_add_block_to_block(&block, &se2.pre);
	count_rate_kind = count_rate->ts.kind;
    }

    g95_init_se(&se3, NULL);

    if (count_max == NULL) {
	se3.expr = null_pointer_node;
	count_max_kind = 0;

    } else {
	se3.reflevel = 1;
	g95_conv_expr(&se3, count_max);
	g95_add_block_to_block(&block, &se3.pre);
	count_max_kind = count_max->ts.kind;
    }

    tmp = (count_rate == NULL || count_rate->ts.type == BT_INTEGER)
	? g95_call_library(void_type_node, PREFIX "system_clock",
			   se1.expr, g95_build_int(count_kind, 0),
			   se2.expr, g95_build_int(count_rate_kind, 0),
			   null_pointer_node, integer_zero_node,
			   se3.expr, g95_build_int(count_max_kind, 0), NULL)

	: g95_call_library(void_type_node, PREFIX "system_clock",
			   se1.expr, g95_build_int(count_kind, 0),
			   null_pointer_node, integer_zero_node,
			   se2.expr, g95_build_int(count_rate_kind, 0),
			   se3.expr, g95_build_int(count_max_kind, 0), NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);
    g95_add_block_to_block(&block, &se3.post);

    return g95_finish_block(&block);
}



/* mold_size()-- Calculate the size of the mold. */

static tree mold_size(g95_expr *mold, g95_se *parent) {
tree tmp, start_minus_one, end;
g95_se se;

    if (mold->ts.type == BT_CHARACTER && mold->ref != NULL &&
	mold->ref->type == REF_SUBSTRING) {

	if (mold->ref->u.ss.start == NULL)
	    start_minus_one = integer_zero_node;

	else {
	    g95_init_se(&se, parent);
	    g95_conv_expr(&se, mold->ref->u.ss.start);
	    start_minus_one =
		g95_build_minus(g95_default_integer, se.expr,
				integer_one_node);
	    g95_raise_chains(&se); 
	}

	if (mold->ref->u.ss.end == NULL) {
	    tmp = g95_get_typenode(&mold->ts, 0);
	    end = size_in_bytes(tmp);

	} else {
	    g95_init_se(&se, parent);
	    g95_conv_expr(&se, mold->ref->u.ss.end);
	    end = se.expr;

	    g95_raise_chains(&se);
	}

	g95_init_se(&se, parent);

	g95_dim(&se, end, start_minus_one);
	g95_raise_chains(&se);

	return se.expr;
    }

    tmp = g95_get_typenode(&mold->ts, 0);
    return size_in_bytes(tmp);
}



/* intrinsic_tranfer()-- Implement the TRANSFER function. */

static void intrinsic_transfer(g95_se *se, g95_expr *expr) {
g95_expr *src_expr, *mold_expr, *size_expr;
tree s, src, tmp, var, pvar;
g95_se source, mold, size;
char *name;

    g95_init_se(&source, se);

    src_expr  = access_arg(expr, 0);
    mold_expr = access_arg(expr, 1);
    size_expr = access_arg(expr, 2);

    if (src_expr->rank == 0) {
	source.reflevel = 1;
	g95_conv_expr(&source, src_expr);
	src = source.expr;

    } else {
	g95_conv_descriptor(&source, src_expr, 0);

	if (g95_contiguous(src_expr))
	    src = g95_base_ref(source.expr);

	else {
	    var = g95_create_var(pchar_type_node);
	    TREE_ADDRESSABLE(var) = 1;

	    pvar = g95_addr_expr(var);
	    g95_set_section_info(&se->pre, 0, integer_zero_node);

	    src = g95_call_library(pvoid_type_node, PREFIX "contiguous_array",
				   g95_addr_expr(source.expr), pvar,
				   null_pointer_node, NULL_TREE);
	
	    tmp = g95_call_library(void_type_node,
				   PREFIX "contiguous_array_done", var,
				   integer_zero_node, NULL_TREE);

	    g95_add_expr_to_block(&se->post, tmp);
	}
    }

    g95_raise_chains(&source);

    g95_init_se(&mold, se);

    if (mold_expr->rank == 0)
	g95_conv_expr(&mold, mold_expr);

    else
	g95_conv_descriptor(&mold, mold_expr, 0);

    g95_raise_chains(&mold);

    g95_init_se(&size, se);
    if (access_arg(expr, 2) != NULL)
	g95_conv_expr(&size, access_arg(expr, 2));

    g95_raise_chains(&size);

    /* src points to some bytes.  Figure out what to transmogrify it into */

    if (access_arg(expr, 1)->rank == 0 &&
	access_arg(expr, 2) == NULL) { /* Scalar result */

	tmp = TREE_TYPE(mold.expr);
	if (!POINTER_TYPE_P(tmp) &&
	    (!g95_c_ptr(&src_expr->ts) || g95_pointer_expr(src_expr)))
	    tmp = build_pointer_type(tmp);

	se->expr = convert(tmp, src);

    } else { /* Rank 1 array result.  Figure out the size. */

	if (access_arg(expr, 2) != NULL)
	    s = size.expr;

	else {
	    if (src_expr->rank == 0)
		s = (src_expr->ts.type == BT_CHARACTER)
		    ? source.string_length
		    : size_in_bytes(TREE_TYPE(TREE_TYPE(src)));

	    else {
		name = intrinsic_size_name();
		tmp = g95_call_library(g95_default_integer, name,
				       g95_desc_addr(source.expr), NULL_TREE);

		s = g95_esize_ref(source.expr);
		s = g95_build_mult(g95_default_integer, s, tmp);
	    }

	    /* Convert size in bytes to size in elements */

	    tmp = mold_size(mold_expr, se);
	    s = fold(build2(CEIL_DIV_EXPR, g95_default_integer, s, tmp));
	}

	/* Initialize the descriptor */

	tmp = mold_size(mold_expr, se);
	se->expr = g95_transfer_result(&se->pre, tmp, src, s);
    }

    se->string_length = mold.string_length;
}



/* intrinsic_trim()-- Trim the trailing blanks from a string.  The
 * string that we return is the original string with a new length.
 * Since trim() must appear on the right of an assignment, there
 * doesn't appear to be a need to copy the string. */

static void intrinsic_trim(g95_se *se, g95_expr *expr) {
g95_se string;

    convert_args(se, expr, &string);

    se->expr = string.expr;
    se->string_length =
	g95_call_library(g95_default_integer, PREFIX "len_trim_1",
			 string.expr, string.string_length, NULL_TREE);
}



/* intrinsic_usize()-- If UBOUND() is applied to an array section
 * instead of a full array, then we are effectively calling the SIZE()
 * intrinsic. */

static void intrinsic_ubound(g95_se *se, g95_expr *expr) {
g95_expr *array;
g95_ref *ref;
int i;

    array = access_arg(expr, 0);

    if (array->type != EXPR_VARIABLE)
	goto size;

    for(ref=array->ref; ref; ref=ref->next) {
	if (ref->type != REF_ARRAY)
	    continue;

	switch(ref->u.ar.type) {
	case AR_FULL:
	    if (ref->next == NULL) {
		intrinsic_bound(se, expr, 1);
		return;
	    }

	    /* Fall through */

	case AR_SECTION:
	    for(i=0; i<ref->u.ar.dimen; i++)
		if (ref->u.ar.start[i] != NULL || ref->u.ar.end[i] != NULL)
		    goto size;

	    intrinsic_bound(se, expr, 1);
	    return;

	default:
	    break;
	}
    }

    g95_internal_error("intrinsic_ubound(): Can't find array section");

/* At this point, we have the ubound of an intermediate array, and the
 * ubound is the extent. */

size:
    if (access_arg(expr, 1) != NULL)
	intrinsic_size(se, expr);

    else {
	expr->value.function.isym = g95_find_function("shape");
	expr->value.function.iname = expr->value.function.isym->lib_name;
	simple_libcall(se, expr);
    }
}



/* intrinsic_unpack()-- UNPACK() intrinsic. */

static void intrinsic_unpack(g95_se *se, g95_expr *expr) {
g95_expr *field_expr, *mask_expr, *vector_expr;
tree vector, mask, field, alloc;
g95_typespec *ts;
char *name;
int save;

    save = se->reflevel;

    se->reflevel = 1;
    vector_expr = access_arg(expr, 0);
    g95_conv_descriptor(se, vector_expr, 0);
    vector = se->expr;

    se->reflevel = 1; 
    mask_expr = access_arg(expr, 1);
    g95_conv_descriptor(se, mask_expr, 0);
    mask = se->expr;

    se->reflevel = 1;
    field_expr = access_arg(expr, 2);

    if (field_expr->rank == 0)
	g95_conv_expr(se, field_expr);

    else
	g95_conv_descriptor(se, field_expr, 0);

    field = se->expr;

    ts = &vector_expr->ts;

    alloc = (ts->type == BT_DERIVED)
	? G95_DTYPE_ALLOCS(ts->derived->backend_decl)
	: null_pointer_node;

    name = (field_expr->rank == 0)
	? PREFIX "unpack_s"
	: PREFIX "unpack";

    se->reflevel = save;

    se->expr = g95_call_library(g95_get_array_pdesc(mask_expr->rank, 0),
				name, vector, mask, field, alloc, NULL_TREE);
}



/* g95_conv_intrinsic_function()-- Generate code for an intrinsic
 * function.  Some map directly to library calls, others get special
 * handling.  In some cases the name of the function used depends on
 * the type specifiers.  */

void g95_conv_intrinsic_function(g95_se *se, g95_expr *expr) {
g95_intrinsic_sym *isym;

    isym = expr->value.function.isym;

    switch(expr->value.function.isym->id) {
    case G95_ISYM_ATAN:
	intrinsic_builtin(se, expr, BUILT_IN_ATANF, BUILT_IN_ATAN);
	break;

    case G95_ISYM_SELECTED_INT_KIND:
	intrinsic_selected_int_kind(se, expr);
	break;

    case G95_ISYM_SELECTED_REAL_KIND:
	intrinsic_selected_real_kind(se, expr);
	break;

    case G95_ISYM_ALLOCATED:
	intrinsic_allocated(se, expr);
	break;

    case G95_ISYM_REPEAT:
	intrinsic_repeat(se, expr);
	break;

    case G95_ISYM_ADJUSTL:
	intrinsic_adjust(se, expr, 0);
	break;

    case G95_ISYM_ADJUSTR:
	intrinsic_adjust(se, expr, 1);
	break;

    case G95_ISYM_PRESENT:
	intrinsic_present(se, expr);
	break;

    case G95_ISYM_ASSOCIATED:
	intrinsic_associated(se, expr);
	break;

    case G95_ISYM_ABS:
	intrinsic_abs(se, expr);
	break;

    case G95_ISYM_AIMAG:
	intrinsic_aimag(se, expr);
	break;

    case G95_ISYM_BTEST:
	intrinsic_btest(se, expr);
	break;

    case G95_ISYM_ACHAR:
    case G95_ISYM_CHAR:
	intrinsic_char(se, expr);
	break;

    case G95_ISYM_CEILING:
    case G95_ISYM_FLOOR:
	intrinsic_ceiling_floor(se, expr);
	break;

    case G95_ISYM_EXP:
	intrinsic_builtin(se, expr, BUILT_IN_EXPF, BUILT_IN_EXP);
	break;

    case G95_ISYM_TRIM:
	intrinsic_trim(se, expr);
	break;

    case G95_ISYM_CONVERSION:
    case G95_ISYM_REAL:
    case G95_ISYM_LOGICAL:
    case G95_ISYM_DBLE:
	intrinsic_conversion(se, expr);
	break;

	/* Integer conversions are handled separately to make sure we get
	 * the correct rounding mode. */

    case G95_ISYM_INT:
	intrinsic_int(se, expr);
	break;

    case G95_ISYM_NINT:
	intrinsic_nint(se, expr);
	break;

    case G95_ISYM_MOD:
	intrinsic_mod(se, expr);
	break;

    case G95_ISYM_MODULO:
	intrinsic_modulo(se, expr);
	break;

    case G95_ISYM_CMPLX:
	intrinsic_cmplx(se, expr);
	break;

    case G95_ISYM_CONJG:
	intrinsic_conjg(se, expr);
	break;

    case G95_ISYM_COS:
	intrinsic_builtin(se, expr, BUILT_IN_COSF, BUILT_IN_COS);
	break;

    case G95_ISYM_DIM:
	intrinsic_dim(se, expr);
	break;

    case G95_ISYM_DPROD:
	intrinsic_dprod(se, expr);
	break;

    case G95_ISYM_IAND:
	intrinsic_bitop(se, expr, BIT_AND_EXPR);
	break;

    case G95_ISYM_IBCLR:
	intrinsic_singlebitop(se, expr, 0);
	break;

    case G95_ISYM_IBITS:
	intrinsic_ibits(se, expr);
	break;

    case G95_ISYM_IBSET:
	intrinsic_singlebitop(se, expr, 1);
	break;

    case G95_ISYM_IACHAR:
    case G95_ISYM_ICHAR:
	intrinsic_ichar(se, expr);
	break;

    case G95_ISYM_IEOR:
	intrinsic_bitop(se, expr, BIT_XOR_EXPR);
	break;

    case G95_ISYM_INDEX:
	intrinsic_index(se, expr);
	break;

    case G95_ISYM_IOR:
	intrinsic_bitop(se, expr, BIT_IOR_EXPR);
	break;

    case G95_ISYM_ISHFT:
	intrinsic_ishft(se, expr);
	break;

    case G95_ISYM_ISHFTC:
	intrinsic_ishftc(se, expr);
	break;

    case G95_ISYM_LBOUND:
	intrinsic_bound(se, expr, 0);
	break;

    case G95_ISYM_LEN:
	intrinsic_len(se, expr);
	break;

    case G95_ISYM_LGE:
	intrinsic_strcmp(se, expr, GE_EXPR);
	break;

    case G95_ISYM_LGT:
	intrinsic_strcmp(se, expr, GT_EXPR);
	break;

    case G95_ISYM_LLE:
	intrinsic_strcmp(se, expr, LE_EXPR);
	break;

    case G95_ISYM_LLT:
	intrinsic_strcmp(se, expr, LT_EXPR);
	break;

    case G95_ISYM_LOC:
	intrinsic_loc(se, expr);
	break;

    case G95_ISYM_LOG:
	intrinsic_builtin(se, expr, BUILT_IN_LOGF, BUILT_IN_LOG);
	break;

    case G95_ISYM_LOG10:
	intrinsic_builtin(se, expr, BUILT_IN_LOG10F, BUILT_IN_LOG10);
	break;

    case G95_ISYM_MAX:
	intrinsic_minmax(se, expr, MAX_EXPR);
	break;

    case G95_ISYM_MERGE:
	intrinsic_merge(se, expr);
	break;

    case G95_ISYM_MIN:
	intrinsic_minmax(se, expr, MIN_EXPR);
	break;

    case G95_ISYM_NOT:
	intrinsic_not(se, expr);
	break;

    case G95_ISYM_PACK:
	intrinsic_pack(se, expr);
	break;

    case G95_ISYM_SIGN:
	intrinsic_sign(se, expr);
	break;

    case G95_ISYM_SIN:
	intrinsic_builtin(se, expr, BUILT_IN_SINF, BUILT_IN_SIN);
	break;

    case G95_ISYM_SIZE:
	intrinsic_size(se, expr);
	break;

    case G95_ISYM_SIZEOF:
	intrinsic_sizeof(se, expr);
	break;

    case G95_ISYM_SQRT:
	intrinsic_builtin(se, expr, BUILT_IN_SQRTF, BUILT_IN_SQRT);
	break;

    case G95_ISYM_TAN:
	intrinsic_builtin(se, expr, BUILT_IN_TANF, BUILT_IN_TAN);
	break;

    case G95_ISYM_TRANSFER:
	intrinsic_transfer(se, expr);
	break;

    case G95_ISYM_UBOUND:
	intrinsic_ubound(se, expr);
	break;

    case G95_ISYM_UNPACK:
	intrinsic_unpack(se, expr);
	break;

    case G95_ISYM_SPREAD:
	intrinsic_spread(se, expr);
	break;

    default:
	simple_libcall(se, expr);
	break;
    }
}



/* g95_conv_intrinsic_subroutine()-- Convert an intrinsic subroutine.
 * If we return NULL_TREE, the intrinsic is just a library call. */

tree g95_conv_intrinsic_subroutine(g95_code *code) {
tree decl;

    switch(code->ext.sub.isym->id) {
    case G95_ISYM_MVBITS:
	decl = intrinsic_mvbits(code->ext.sub.actual);
	break;

    case G95_ISYM_SYSTEM_CLOCK:
	decl = intrinsic_system_clock(code->ext.sub.actual);
	break;

    case G95_ISYM_MOVE_ALLOC:
	decl = intrinsic_move_alloc(code->ext.sub.actual);
	break;

    default:
	decl = NULL_TREE;
	break;
    }

    return decl;
}



/* intrinsic_c_loc()-- Implement the C_LOC() intrinsic. */

static void intrinsic_c_loc(g95_se *se, g95_expr *expr) {
g95_expr *e;
g95_se se0;

    e = access_arg(expr, 0);
    if (e->rank == 0)
	g95_conv_expr(se, e);

    else {
	g95_init_se(&se0, se);
	g95_conv_descriptor(&se0, e, 1);
	g95_raise_chains(&se0);

	se->expr = g95_base_ref(se0.expr);
    }
}



/* intrinsic_c_funloc()-- Implement the C_FUNLOC() intrinsic. */

static void intrinsic_c_funloc(g95_se *se, g95_expr *expr) {
g95_expr *e;

    e = access_arg(expr, 0);

    if (e->type == EXPR_VARIABLE)
	se->expr = e->symbol->backend_decl;

    else
	g95_conv_expr(se, access_arg(expr, 0));
}



/* intrinsic_c_associated()-- Check c_ptr association.  This is a
 * special case of the main associated() intrinsic because all the
 * arguments are guaranteed scalar. */

static tree intrinsic_c_associated(g95_se *se, g95_expr *expr) {
g95_expr *c_ptr_1, *c_ptr_2;
g95_se se0, se1;
tree t1, t2;

    c_ptr_1 = access_arg(expr, 0);
    c_ptr_2 = access_arg(expr, 1);

    g95_init_se(&se0, se);
    se0.reflevel = 1;

    g95_conv_expr(&se0, c_ptr_1);

    if (c_ptr_2 == NULL)
	se->expr = g95_build_ne(se0.expr, null_pointer_node);

    else {
	g95_init_se(&se1, se);
	se1.reflevel = 1;
	g95_conv_expr(&se1, c_ptr_2);

	t1 = g95_build_ne(se0.expr, null_pointer_node);
	t2 = g95_build_eq(se0.expr, se1.expr);

	se->expr = g95_build_andif(t1, t2);
    }

    return NULL;
}



/* intrinsic_ieee_class()-- Implement the IEEE_CLASS function. */

static void intrinsic_ieee_class(g95_se *se, g95_expr *expr) {
g95_se se0;

    g95_init_se(&se0, se);
    se0.reflevel = 1;
    g95_conv_expr(&se0, access_arg(expr, 0));

    se->expr = g95_call_library(g95_default_integer,
				expr->value.function.iname,
				se0.expr, NULL_TREE);
}



void g95_conv_modproc_function(g95_se *se, g95_expr *expr) {

    switch(expr->symbol->attr.iproc) {
    case IPROC_C_LOC:
	intrinsic_c_loc(se, expr);
	break;

    case IPROC_C_FUNLOC:
	intrinsic_c_funloc(se, expr);
	break;

    case IPROC_C_ASSOCIATED:
	intrinsic_c_associated(se, expr);
	break;

    case IPROC_IEEE_CLASS:
	intrinsic_ieee_class(se, expr);
	break;

    case IPROC_IEEE_SELECTED_REAL_KIND:
	intrinsic_selected_real_kind(se, expr);
	break;

    case IPROC_IEEE_IS_NAN:
    case IPROC_IEEE_VALUE:
    case IPROC_IEEE_IS_FINITE:
    case IPROC_IEEE_IS_NORMAL:
    case IPROC_IEEE_IS_NEGATIVE:
	simple_libcall(se, expr);
	break;

    default:
	g95_internal_error("g95_conv_modproc_function(): Bad procedure");
    }
}



/* intrinsic_c_f_pointer()-- Convert a C pointer to a fortran
 * pointer. */

static tree intrinsic_c_f_pointer(g95_actual_arglist *a) {
g95_expr *cptr, *fptr, *shape;
g95_se se1, se2, se3;
stmtblock_t block;
tree tmp;

    cptr = a->u.expr;
    fptr = a->next->u.expr;

    shape = (a->next->next == NULL)
	? NULL
	: a->next->next->u.expr;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);
    se1.reflevel = 1;
    g95_conv_expr(&se1, cptr);
    g95_add_block_to_block(&block, &se1.pre);

    g95_init_se(&se2, NULL);
    se2.reflevel = 1;
    g95_conv_expr(&se2, fptr);

    if (shape == NULL) {
	g95_add_block_to_block(&block, &se2.pre);
	g95_add_modify_expr(&block, se2.expr, se1.expr);

    } else {
	se2.expr = G95_ARRAY_DESC(se2.expr);
	g95_reflevel(&se2, 1);
	g95_add_block_to_block(&block, &se2.pre);

	g95_init_se(&se3, NULL);
	se3.reflevel = 1;
	g95_conv_descriptor(&se3, shape, 0);
	g95_add_block_to_block(&block, &se3.pre);

	tmp = g95_call_library(void_type_node, PREFIX "c_f_pointer",
			       se1.expr, se2.expr,
			       convert(g95_default_integer,
				       g95_ts_size(&fptr->ts)),
			       se3.expr, NULL_TREE);

	g95_add_expr_to_block(&block, tmp);
	g95_add_block_to_block(&block, &se3.post);
    }

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);

    return g95_finish_block(&block);
}



tree g95_conv_modproc_subroutine(g95_code *code) {
tree tmp;

    switch(code->sym->attr.iproc) {
    case IPROC_C_F_POINTER:
    case IPROC_C_F_PROCPOINTER:
	tmp = intrinsic_c_f_pointer(code->ext.sub.actual);
	break;

    default:
	g95_internal_error("g95_conv_modpoc_subroutine(): Bad proc");
	tmp = NULL_TREE;
    }

    return tmp;
}

