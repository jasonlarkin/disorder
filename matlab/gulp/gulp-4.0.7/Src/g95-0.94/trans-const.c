/* Translation of constants
   Copyright (C) 2000-2008 Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* trans_code.c -- convert constant values */

#include "trans.h"
#include "stdlib.h"

tree null_string_node, empty_stmt_node;



/* g95_build_const()-- Build a constant with given type from an
 * integer constant node.  */

tree g95_build_const(tree type, tree intval) {
tree val, zero;

    switch(TREE_CODE(type)) {
    case INTEGER_TYPE:
	val = convert(type, intval);
	break;

    case REAL_TYPE:
	val = build_real_from_int_cst(type, intval);
	break;

    case COMPLEX_TYPE:
	val = build_real_from_int_cst(TREE_TYPE (type), intval);
	zero = build_real_from_int_cst(TREE_TYPE (type), integer_zero_node);
	val = build_complex(type, val, zero);
	break;

    default:
	abort();
    }

    return val;
}



/* g95_build_string_const()-- Build a string constant */

tree g95_build_string_const(int length, char *s) {
tree str, len;

    if (length == 0)
	return null_string_node;

    str = build_string(length, s);
    len = g95_build_int(length-1, 0);

    TREE_TYPE(str) = build_array_type(g95_character1_type_node,
				      build_index_type(len));

    return str;
}



/* g95_resize_string_constant()-- Given a constant string, resize it
 * to the desired size, truncating or padding with spaces. */

tree g95_resize_string_constant(tree string, tree new_size) {
int current_len, len;
tree decl;
char *p;

    current_len = (string == null_string_node)
	? 0
	: int_size_in_bytes(TREE_TYPE(string));

    len = TREE_INT_CST_LOW(new_size);
    if (len == current_len)
	return string;

    if (len < current_len) {
	p = (char *) TREE_STRING_POINTER(string);
	return g95_build_string_const(len, p);
    }

    p = g95_getmem(len);

    if (string != null_string_node)
	memcpy(p, TREE_STRING_POINTER(string), current_len);

    memset(p+current_len, ' ', len-current_len);

    decl = g95_build_string_const(len, p);
    g95_free(p);

    return decl;
}



/* g95_build_int()-- Build a default integer, working around a broken
 * build_int_cst(). */

tree g95_build_int(HOST_WIDE_INT low, HOST_WIDE_INT high) {
tree t;

    t = make_node(INTEGER_CST);

    TREE_INT_CST_LOW(t) = low;
    TREE_INT_CST_HIGH(t) = high;
    TREE_TYPE(t) = g95_default_integer;

    return t;
}



static HOST_WIDE_INT bi_to_hwi(bignum b) {
unsigned HOST_WIDE_INT m;
int i, bits;
bignum p;

    bits = 8*sizeof(HOST_WIDE_INT);
    m = 0;

    p = bi_power(bi_2, int_to_bi(bits-1));

    for(i=0; i<bits; i++) {
	m <<= 1;

	if (bi_compare(big_copy(b), big_copy(p)) >= 0) {
	    m |= 1;
	    b = bi_subtract(b, big_copy(p));
	}

	p = bi_half(p);
    }

    big_free(p);
    big_free(b);

    return m;
}



tree bi_to_tree(bignum b, int kind) {
tree decl;
bignum p, q;
int sign;

    sign = 0;

    if (bi_is_negative(big_copy(b))) {
	b = bi_negate(b);
	sign = 1;
    }

    p = bi_power(bi_2, int_to_bi(8*sizeof(HOST_WIDE_INT)));

    decl = make_node(INTEGER_CST);

    q = bi_rem(big_copy(b), big_copy(p));
    TREE_INT_CST_LOW(decl)  = bi_to_hwi(q);

    q = bi_divide(b, p);
    TREE_INT_CST_HIGH(decl) = bi_to_hwi(q);

    TREE_TYPE(decl) = (kind == -1)
	? g95_default_integer
	: g95_get_int_type(kind);

    if (sign)
	decl = fold(build1(NEGATE_EXPR, TREE_TYPE(decl), decl));

    return decl;
}



/* significand_string()-- Convert the significand to hexadecimal.  The
 * weird thing about this conversion is that it has to start at the
 * left of the number.  The least significant bits do not necessarily
 * convert to a full hex digit (which can end up with trailing zeros). */

static void significand_string(bignum b, char *p) {
bignum d;
int m, n;

    n = (b->ff->digits + 3) / 4; 
    d = bi_power(bi_2, int_to_bi(b->ff->digits - 4));

    for(;;) {
	m = bi_to_int(bi_divide(big_copy(b), big_copy(d)));

	if (m >= 10)
	    m += 'A' - 10;
	else
	    m += '0';

	*p++ = m;

	if (--n == 0)
	    break;

	b = bi_int_multiply(bi_rem(b, big_copy(d)), 16);
    }

    *p = '\0';
    big_free(b);
    big_free(d);
}


/* bg_to_tree()-- Convert a real constant into backend form. */

tree bg_to_tree(bignum b, int kind) {
char *p, buffer[200];
REAL_VALUE_TYPE *r;
tree type, decl;
int m, e;

    type = g95_get_real_type(kind);
    decl = build_real(type, REAL_VALUE_ATOF("0", TYPE_MODE(type)));
    r = TREE_REAL_CST_PTR(decl);
    m = 0;

    switch(bg_real_type(b)) {
    case FF_INFINITY:
	real_inf(r);

	if (b->real_sign < 0)
	    r->sign = 1;

	big_free(b);
	break;

    case FF_NAN:
	p = buffer;
	if (b->real_sign < 0)
	    *p++ = '-';

	*p++ = '0';
	*p++ = 'x';

	significand_string(b, p);
	real_nan(r, buffer, 0, TYPE_MODE(type));
	break;

    case FF_ZERO:
	p = buffer;
	if (b->real_sign < 0)
	    *p++ = '-';

	strcpy(p, "0.0");
	real_from_string(r, buffer);
	big_free(b);
	break;

    case FF_DENORMAL:
	m = 1;  /* Danger- works for x86 denormal */

	/* Fall through */

    case FF_NORMAL:
	p = buffer;
	if (b->real_sign < 0)
	    *p++ = '-';

	strcpy(p, "0x0.");
	e = b->real_exp - b->ff->exp_bias + 1 + m;

	significand_string(b, strchr(buffer, '\0'));
	sprintf(strchr(buffer, '\0'), "P%d", e);

	real_from_string(r, buffer);
	break;
    }

    return decl;
}



/* g95_conv_constant()-- Translate a scalar constant.  Constants never
 * have pre or post chains, but strings have a length which needs to
 * be returned.  If the string_length is not a NULL_TREE, then we want
 * a string of the prescribed length.  Ignored if we are not dealing
 * with a character constant. */

void g95_conv_constant(g95_se *se, g95_expr *expr) {
tree real, imag, type;
int len;

    if (expr->type == EXPR_NULL) {
	se->expr = null_pointer_node;
	return;
    }

    if (expr->type == EXPR_STRUCTURE) {
	g95_conv_expr(se, expr);
	assert(se->pre.head == NULL && se->post.head == NULL);
	return;
    }

    assert(expr->type == EXPR_CONSTANT);

    switch(expr->ts.type) {
    case BT_COMPLEX:
      real = bg_to_tree(expr->value.complex.r, expr->ts.kind);
      imag = bg_to_tree(expr->value.complex.i, expr->ts.kind);
      type = g95_get_complex_type(expr->ts.kind);

      se->expr = build_complex(type, real, imag);
      break;

    case BT_CHARACTER:
	len = expr->value.character.length;
	se->expr = g95_build_string_const(len, expr->value.character.string);
	se->string_length = convert(g95_default_integer, g95_build_int(len,0));
	break;

    case BT_INTEGER:
	se->expr = bi_to_tree(expr->value.integer, expr->ts.kind);
	break;

    case BT_REAL:
	se->expr = bg_to_tree(expr->value.real, expr->ts.kind);
	break;

    case BT_LOGICAL:
	se->expr = convert(g95_get_int_type(expr->ts.kind),
			   g95_build_int(expr->value.logical, 0));
	break;

    default:
	g95_internal_error("g95_conv_constant(): invalid constant: type %d",
			   expr->ts.type);
	break;
    }
}

