

/* Floating point emulation.  We use the bigint library to emulate
 * IEEE-754 floating point arithmetic. */

#include <string.h>
#include "g95.h"


static g95_ff super;
g95_locus *g95_arithmetic_locus = NULL;

bignum bg_pi, bg_half_pi, bg_two_pi, bg_e, bg_ln2, bg_ln10;

typedef enum { AWARN_OVERFLOW=1, AWARN_ZERODIV, AWARN_INVALID } awarn;



/* g95_zero_division()-- Issue a warning about division by zero. */

void g95_zero_division(g95_locus *where) {

    g95_warning(133, "Integer division by zero at %L", where);
}




static void arithmetic_warning(awarn w) {

    if (g95_arithmetic_locus == NULL)
	return;

    switch(w) {
    case AWARN_OVERFLOW:
	g95_warning(128, "Arithmetic overflow at %L", g95_arithmetic_locus);
	break;

    case AWARN_ZERODIV:
	g95_zero_division(g95_arithmetic_locus);
	break;

    case AWARN_INVALID:
	g95_warning(130, "Invalid arithmetic operation at %L",
		    g95_arithmetic_locus);
	break;
    }

    g95_arithmetic_locus = NULL;
}



/* int_power()-- Compute nonnegative integer powers of an integer base */

static bignum int_power(int base, int e) {
bignum p, s;

    p = bi_1;
    s = int_to_bi(base);

    while(e > 0) {
	if (e & 1)
	    p = bi_multiply(p, big_copy(s));

	s = bi_square(s);
	e = e >> 1;
    }

    big_free(s);
    return p;
}



/* min_mantissa()-- Return the smallest mantissa for a real type. */

static bignum min_mantissa(g95_ff *ff) {

    return bi_2n(ff->digits-1);
}



/* max_mantissa()-- Return the largest mantissa for a real type. */

static bignum max_mantissa(g95_ff *ff) {

    return bi_int_subtract(bi_2n(ff->digits), 1);
}



/* get_infinity()-- Return a signed infinity. */

static bignum get_infinity(int sign, g95_ff *ff) {
bignum b;

    b = (ff->msb == MSB_IMPLICIT) 
	? big_clone(bi_0)
	: bi_2n(ff->digits-1);

    b->real_exp  = ff->exp_nan;
    b->real_sign = sign;
    b->ff        = ff;

    return b;
}



/* get_nan()-- Return a not-a-number. */

static bignum get_nan(g95_ff *ff) {
bignum b;

    b = big_clone(bi_1);

    b->real_exp  = ff->exp_nan;
    b->real_sign = 1;
    b->ff        = ff;

    return b;
}



/* get_zero()-- Return a signed zero for a real type. */

static bignum get_zero(int sign, g95_ff *ff) {
bignum b;

    b = big_clone(bi_0);

    b->real_exp  = 0;
    b->real_sign = sign;
    b->ff        = ff;

    return b;
}



/* bg_get_exceptional()-- Get an exceptional value. */

bignum bg_get_exceptional(ff_class type, int kind, int sign) {
g95_ff *ff; 
bignum r;

    ff = g95_get_ff(kind); 

    switch(type) {
    case FF_ZERO:      r = get_zero(sign, ff);       break;
    case FF_NAN:       r = get_nan(ff);              break;
    case FF_INFINITY:  r = get_infinity(sign, ff);   break;

    default:
	g95_internal_error("g95_get_exceptional(): Bad type");
	r = NULL;
    }

    return r;
}



/* bg_real_type()-- Given a big_float, figure out what kind of number it
 * is.  Does not consume the number.  */

ff_class bg_real_type(bignum b) {

    if (b->ff->msb == MSB_IMPLICIT) {
	if (b->real_exp == b->ff->exp_nan)
	    return (bi_is_zero(big_copy(b))) ? FF_INFINITY : FF_NAN;

	if (b->real_exp == 0)
	    return (bi_is_zero(big_copy(b))) ? FF_ZERO : FF_DENORMAL;

    } else {
	if (b->real_exp == b->ff->exp_nan)
	    return (bi_compare(bi_2n(b->ff->digits-1),
			       big_copy(b))) ? FF_NAN : FF_INFINITY;

	if (b->real_exp == 0)
	    return (bi_is_zero(big_copy(b))) ? FF_ZERO : FF_DENORMAL;
    }

    return FF_NORMAL;
}



/* clamp_range()-- Given a big_float that has an arbitrary exponent
 * and mantissa, clamp the number to the proper range which may
 * involve changing it to infinity or denormalizing it. */

static bignum clamp_range(bignum b) {
int n, z, sign, exp, bottom_bit;
g95_ff *ff;

    /* Worry about a too-large mantissa */

    sign = b->real_sign;
    exp  = b->real_exp;
    ff   = b->ff;

    if (bi_is_zero(big_copy(b))) {  /* Special case */
	big_free(b);
	return get_zero(sign, ff);
    }

    bottom_bit = 0;
    z = 0;
    n = bi_bits(big_copy(b)) - ff->digits;

    while(n>0) {
	z |= bottom_bit;

	bottom_bit = bi_is_odd(big_copy(b));
	b = bi_half(b);
	exp++;
	n--;
    }

    switch(g95_option.round) {
    case ROUND_NEAREST:
	if (bottom_bit && (z || bi_is_odd(big_copy(b))))
	    b = bi_add(b, bi_1);

	break;

    case ROUND_PLUS:
	if (sign > 0 && (z || bottom_bit))
	    b = bi_add(b, bi_1);

	break;

    case ROUND_MINUS:
	if (sign < 0 && (z || bottom_bit))
	    b = bi_add(b, bi_1);

	break;

    case ROUND_ZERO:
	break;
    }

    if (bi_bits(big_copy(b)) > ff->digits) {
	b = bi_half(b);
	exp++;
    }

    /* Worry about a too-small mantissa */

    n = ff->digits - bi_bits(big_copy(b));

    while(n>0) {
	b = bi_double(b);
	exp--;
	n--;
    }

    /* Worry about the exponent now */

    if (exp >= ff->exp_nan) {  /* Overflow! */
	arithmetic_warning(AWARN_OVERFLOW);
	big_free(b);
	return get_infinity(sign, ff);
    }

    if (exp > 0)
	goto done;       /* Zero, normal or already denormal */

    if (exp < -ff->digits) {   /* Will underflow to zero */
	big_free(b);
	return get_zero(sign, ff);
    }

    /* Denormalize the number */

    bottom_bit = bi_is_odd(big_copy(b));
    b = bi_half(b);
    z = 0;

    while(exp < 0) {
	z |= bottom_bit;

	bottom_bit = bi_is_odd(big_copy(b));
	exp++;
	b = bi_half(b);
    }

    if (bottom_bit && (z || bi_is_odd(big_copy(b)))) {
	b = bi_add(b, bi_1);   /* Round up */

	if (bi_bits(big_copy(b)) > ff->digits) {
	    b = bi_half(b);
	    exp++;
	}
    }

done:
    b->real_sign = sign;
    b->real_exp  = exp;
    b->ff        = ff;

    return b;
}



/* next_float()-- Generate the next larger floating point after the
 * current one.  This takes no account of the sign of the number */

static bignum next_float(bignum b) {
g95_ff *ff;
int exp, sign;
bignum r;

    ff = b->ff;

    switch(bg_real_type(b)) {
    case FF_ZERO:   /* Special case, denormalized TINY() */
	r = big_clone(bi_1);
	r->real_exp  = 0;
	r->real_sign = b->real_sign;
	r->ff        = ff;

	big_free(b);
	break;

    case FF_NORMAL:
    case FF_DENORMAL:
	exp  = b->real_exp;
	sign = b->real_sign;

	r = bi_int_add(b, 1);

	r->real_exp  = exp;
	r->real_sign = sign;
	r->ff        = ff;

	if (b->real_exp != 0)
	    r = clamp_range(r);

	else if (bi_compare(big_copy(r), min_mantissa(ff)) == 0)
	    r->real_exp++;

	break;

    default:
	r = b;
	break;
    }

    return r;
}



/* prev_float()-- Generate the previous floating point number after
 * the current one.  Does not take account of the sign. */

static bignum prev_float(bignum b) {
int exp, sign;
g95_ff *ff;
bignum r;

    exp  = b->real_exp;
    sign = b->real_sign;
    ff   = b->ff;

    switch(bg_real_type(b)) {
    case FF_NORMAL:
	if (bi_compare(big_copy(b), min_mantissa(ff)) != 0) {
	    r = bi_int_subtract(b, 1);
	    r->real_exp = exp;

	} else {
	    r = (b->real_exp == 1)
		? bi_half(big_clone(max_mantissa(ff)))
		: big_clone(max_mantissa(ff));

	    r->real_exp = b->real_exp - 1;
	}

	big_free(b);

	r->real_sign = sign;
	break;

    case FF_DENORMAL:
	r = bi_int_subtract(b, 1);
	r->real_exp  = exp;
	r->real_sign = sign;
	break;

  case FF_ZERO:
      r = next_float(b);
      r->real_sign = -r->real_sign;
      break;

  default:
      r = b;
      break;
    }

    r->ff = ff;
    return r;
}



/* bg_nearest()-- Return the next number, up or down. */

bignum bg_nearest(bignum b, int flag) {

    return ((b->real_sign < 0) ^ (flag > 0)) ? next_float(b) : prev_float(b);
}



bignum bg_rrspacing(bignum b) {
g95_ff *ff;

    ff = b->ff;

    switch(bg_real_type(b)) {
    case FF_NORMAL:
    case FF_DENORMAL:
	b = big_clone(b);

	while(bi_bits(big_copy(b)) < ff->digits)
	    b = bi_double(b);

	b->real_exp  = b->ff->exp_bias + b->ff->digits - 1;
	b->real_sign = 1;

	b = clamp_range(b);
	break;

    case FF_ZERO:
	b = big_clone(b);
	b->real_sign = 1;
	break;

    case FF_INFINITY:
	big_free(b);
	b = get_zero(1, ff);
	break;

    default:
	break;
    }

    return b;
}



bignum bg_scale(bignum b, int e) {

    switch(bg_real_type(b)) {
    case FF_NORMAL:
    case FF_DENORMAL:
	b = big_clone(b);
	b->real_exp += e;
	b = clamp_range(b);
	break;

    default:
	break;
    }

    return b;
}



bignum bg_set_exponent(bignum b, int e) {

    switch(bg_real_type(b)) {
    case FF_NORMAL:
    case FF_DENORMAL:
	b = big_clone(b);
	b->real_exp = e - b->real_exp + 2*b->ff->exp_bias;
	b = clamp_range(b);
	break;

    default:
	break;
    }

    return b;
}



bignum bg_spacing(bignum b) {
g95_ff *ff;
int e;

    ff = b->ff;
    switch(bg_real_type(b)) {
    case FF_NORMAL:
	e = b->real_exp;
	big_free(b);

	b = big_clone(bi_1);
	b->real_sign = 1;
	b->real_exp  = e;
	b->ff        = ff;

	b = clamp_range(b);

	if (bg_real_type(b) != FF_DENORMAL)
	    break;

	/* Fall through */

    case FF_ZERO:
    case FF_DENORMAL:
	big_free(b);
	b = bg_tiny(ff);
	break;

    default:
	break;
    }

    return b;
}



/* bg_abs()-- Compute an absolute value. */

bignum bg_abs(bignum b) {
bignum a;

    if (bg_real_type(b) == FF_NAN || b->real_sign == 1)
	return b;

    a = big_clone(b);
    a->real_sign = 1;

    return a;
}



/* bg_compare()-- Compare two big_floats.  Returns an integer equal to
 * zero, greater than zero or less than zero.  This subroutine doesn't
 * handle not-a-numbers and should not be used in general cases. */

static int bg_compare(bignum a, bignum b) {
ff_class ac, bc;
int r;

    ac = bg_real_type(a);
    bc = bg_real_type(b);

    if (a->ff != b->ff)
	g95_internal_error("bg_compare(): Different formats");

    if (ac == FF_INFINITY && bc == FF_INFINITY) {
	if (a->real_sign == b->real_sign)
	    r = 0;
	else
	    r = (a->real_sign > b->real_sign) ? 1 : -1;

	goto done;
    }

    if (ac == FF_INFINITY) {
	r = (a->real_sign > 0) ? 1 : -1;
	goto done;
    }

    if (bc == FF_INFINITY) {
	r = (b->real_sign > 0) ? -1 : 1;
	goto done;
    }

    /* At this point everything is finite */

    if (ac == FF_ZERO && bc == FF_ZERO) {
	r = 0;
	goto done;
    }

    if (ac == FF_ZERO) {
	r = (b->real_sign > 0) ? -1 : 1;
	goto done;
    }

    if (bc == FF_ZERO) {
	r = (a->real_sign > 0) ? 1 : -1;
	goto done;
    }

    /* Both are nonzero at this point */

    if (a->real_sign != b->real_sign) {
	r = (a->real_sign < b->real_sign) ? -1 : 1;
	goto done;
    }

    r = (a->real_sign < 0) ? -1 : 1;

    if (a->real_exp < b->real_exp) {
	r = -r;
	goto done;
    }

    if (a->real_exp > b->real_exp)
	goto done;

    r = r * bi_compare(big_copy(a), big_copy(b));

done:
    big_free(a);
    big_free(b);

    return r;
}



/* bg_compare_int()-- Compare with an integer */

int bg_compare_int(bignum a, int b) {

    if (bg_real_type(a) == FF_NAN)
	return 0;

    return bg_compare(a, bg_from_int(b, a->ff));
}



int bg_compare_ge(bignum a, bignum b) {
int m;

    if (bg_real_type(a) != FF_NAN && bg_real_type(b) != FF_NAN)
	m = (bg_compare(a, b) >= 0);

    else {
	m = 0;
	big_free(a);
	big_free(b);
    }

    return m;
}



int bg_compare_le(bignum a, bignum b) {
int m;

    if (bg_real_type(a) != FF_NAN && bg_real_type(b) != FF_NAN)
	m = (bg_compare(a, b) <= 0);

    else {
	m = 0;
	big_free(a);
	big_free(b);
    }

    return m;
}



int bg_compare_lt(bignum a, bignum b) {
int m;

    if (bg_real_type(a) != FF_NAN && bg_real_type(b) != FF_NAN)
	m = (bg_compare(a, b) < 0);

    else {
	m = 0;
	big_free(a);
	big_free(b);
    }

    return m;
}



int bg_compare_gt(bignum a, bignum b) {
int m;

    if (bg_real_type(a) != FF_NAN && bg_real_type(b) != FF_NAN)
	m = (bg_compare(a, b) > 0);

    else {
	m = 0;
	big_free(a);
	big_free(b);
    }

    return m;
}



int bg_compare_eq(bignum a, bignum b) {
int m;

    if (bg_real_type(a) != FF_NAN && bg_real_type(b) != FF_NAN)
	m = (bg_compare(a, b) == 0);

    else {
	m = 0;
	big_free(a);
	big_free(b);
    }

    return m;
}



int bg_compare_ne(bignum a, bignum b) {
int m;

    if (bg_real_type(a) != FF_NAN && bg_real_type(b) != FF_NAN)
	m = (bg_compare(a, b) != 0);

    else {
	m = 0;
	big_free(a);
	big_free(b);
    }

    return m;
}



/* nan_result()-- Compute the not-a-number result of a binary operation. */

static bignum nan_result(bignum a, bignum b) {
ff_class ac, bc;
bignum r;

    ac = bg_real_type(a);
    bc = bg_real_type(b);

    if (ac == FF_NAN && bc == FF_NAN) {
	if (bi_compare(big_copy(a), big_copy(b)) < 0) {
	    r = b;
	    big_free(a);
	} else {
	    r = big_copy(a);
	    big_free(b);
	}

	r->real_exp = r->ff->exp_nan;
	return r;
    }

    if (ac == FF_NAN) {
	big_free(b);
	return a;
    }

    if (bc == FF_NAN) {
	big_free(a);
	return b;
    }

    g95_internal_error("nan_result(): Bad float class");
}



static bignum sum_diff(bignum a, bignum b, int add_flag) {
int sa, sb, ea, eb;
g95_ff *ff;
bignum r;

    if (a->ff != b->ff)
	g95_internal_error("sum_diff(): Different formats");

    sa = a->real_sign;
    ea = a->real_exp;

    sb = b->real_sign;
    eb = b->real_exp;

    ff = a->ff;

    if (ea == 0)
	ea++;

    if (eb == 0)
	eb++;

    while(ea > eb) {
	a = bi_double(a);
	ea--;
    }

    while(ea < eb) {
	b = bi_double(b);
	eb--;
    }

    if (sa < 0)
	a = bi_negate(a);

    if (sb < 0)
	b = bi_negate(b);

    r = add_flag
	? bi_add(a, b)
	: bi_subtract(a, b);

    if (bi_compare(big_copy(r), bi_0) >= 0)
	r->real_sign = 1;

    else {
	r = bi_negate(r);
	r->real_sign = -1;
    }

    r->real_exp = ea;
    r->ff = ff;

    return clamp_range(r);
}



/* bg_add()-- Add two numbers. */

bignum bg_add(bignum a, bignum b) {
ff_class ac, bc;
bignum result;
int sign;

    ac = bg_real_type(a);
    bc = bg_real_type(b);

    if (ac == FF_NAN || bc == FF_NAN)
	return nan_result(a, b);

    if (ac == FF_INFINITY && bc == FF_INFINITY) {
	if (a->sign == b->sign) {
	    result = a;
	    big_free(b);

	} else {
	    big_free(a);
	    big_free(b);
	    result = get_nan(a->ff);
	}

	return result;
    }

    if (ac == FF_INFINITY) {
	big_free(b);
	return a;
    }

    if (bc == FF_INFINITY) {
	big_free(a);
	return b;
    }

    if (ac == FF_ZERO && bc == FF_ZERO) {
	sign = (a->real_sign == -1 && b->real_sign == -1) ? -1 : 1;

	big_free(a);
	b = big_clone(b);
	b->real_sign = sign;

	return b;
    }

    if (ac == FF_ZERO) {
	big_free(a);
	return b;
    }

    if (bc == FF_ZERO) {
	big_free(b);
	return a;
    }

    /* finite_result */

    return sum_diff(a, b, 1);
}



/* bg_add_int()-- Add an integer to a real. */

bignum bg_add_int(bignum a, int b) {

    return bg_add(a, bg_from_int(b, a->ff));
}



/* bg_subtract()-- Subtraction. */

bignum bg_subtract(bignum a, bignum b) {
ff_class ac, bc;
bignum result;
int sign;

    ac = bg_real_type(a);
    bc = bg_real_type(b);

    if (ac == FF_NAN || bc == FF_NAN)
	return nan_result(a, b);

    if (ac == FF_INFINITY && bc == FF_INFINITY) {
	if (a->sign != b->sign) {
	    big_free(b);
	    return a;
	} else {
	    result = get_nan(a->ff);
	    big_free(a);
	    big_free(b);
	    return result;
	}
    }

    if (ac == FF_INFINITY) {
	big_free(b);
	return a;
    }

    if (bc == FF_INFINITY) {
	big_free(a);
	b->real_sign = -b->real_sign;
	return b;
    }

    if (ac == FF_ZERO && bc == FF_ZERO) {
	sign = (a->real_sign == 1 && b->real_sign == -1) ? -1 : 1;
	big_free(a);

	b = big_clone(b);
	b->real_sign = sign;
	return b;
    }

    if (ac == FF_ZERO) {
	big_free(a);
	return bg_negate(b);
    }

    if (bc == FF_ZERO) {
	big_free(b);
	return a;
    }

    /* finite_result */

    return sum_diff(a, b, 0);
}



/* bg_multiply()-- Multiply two numbers. */

bignum bg_multiply(bignum a, bignum b) {
ff_class ac, bc;
bignum result;
int sign, exp;
g95_ff *ff;

    if (a->ff != b->ff)
	g95_internal_error("bg_multiply(): Different formats");

    ac = bg_real_type(a);
    bc = bg_real_type(b);

    if (ac == FF_NAN || bc == FF_NAN)
	return nan_result(a, b);

    if (ac == FF_INFINITY || bc == FF_INFINITY) {
	result = (ac == FF_ZERO || bc == FF_ZERO)
	    ? get_nan(a->ff)
	    : get_infinity(a->real_sign * b->real_sign, a->ff);

	big_free(a);
	big_free(b);

	return result;
    }

    if (ac == FF_ZERO || bc == FF_ZERO) {
	result = get_zero(a->real_sign * b->real_sign, a->ff);
	big_free(a);
	big_free(b);

	return result;
    }

    if (a->real_exp == 0)
	a->real_exp++;

    if (b->real_exp == 0)
	b->real_exp++;

    ff   = a->ff;
    exp  = a->real_exp + b->real_exp - ff->exp_bias - ff->digits + 1;
    sign = a->real_sign * b->real_sign;

    result = bi_multiply(a, b);

    result->real_exp  = exp;
    result->real_sign = sign;
    result->ff        = ff;

    return clamp_range(result);
}



/* bg_multiply_int()-- Multiply by a machine integer. */

bignum bg_multiply_int(bignum a, int b) {

    return bg_multiply(a, bg_from_int(b, a->ff));
}



/* bg_negate()-- Negate a number */

bignum bg_negate(bignum x) {

    if (bg_real_type(x) == FF_NAN)
	return x;

    x = big_clone(x);
    x->real_sign = -x->real_sign;

    return x;
}



/* bg_divide()-- Compute a quotient */

bignum bg_divide(bignum a, bignum b) {
ff_class ac, bc;
bignum result;
int sign, exp;
g95_ff *ff;

    ac = bg_real_type(a);
    bc = bg_real_type(b);
    ff = a->ff;
    sign = a->real_sign * b->real_sign;

    if (ac == FF_NAN || bc == FF_NAN)
	return nan_result(a, b);

    if (ac == FF_INFINITY && bc == FF_INFINITY) {
	arithmetic_warning(AWARN_INVALID);
	big_free(a);
	big_free(b);
	return get_nan(ff);
    }

    switch(bc) {
    case FF_ZERO:
	big_free(a);
	big_free(b);

	if (ac == FF_ZERO) {
	    result = get_nan(ff);
	    arithmetic_warning(AWARN_INVALID);
	} else {
	    result = get_infinity(sign, ff);
	    arithmetic_warning(AWARN_ZERODIV);
	}

	return result;

    case FF_INFINITY:
	result = get_zero(sign, ff);

	big_free(a);
	big_free(b);
	return result;

    default:
	break;
    }

    switch(ac) {
    case FF_INFINITY:
	result = big_clone(a);
	result->real_sign = sign;

	big_free(b);
	return result;

    case FF_ZERO:
	result = get_zero(sign, ff);

	big_free(a);
	big_free(b);
	return result;

    default:
	break;
    }

    /* Finite result */

    if (a->real_exp == 0)
	a->real_exp++;

    if (b->real_exp == 0)
	b->real_exp++;

    exp = a->real_exp - b->real_exp + ff->exp_bias - 1 - ff->digits;

    result = bi_divide(bi_multiply(a, bi_2n(2*ff->digits)), b);

    result->real_sign = sign;
    result->real_exp  = exp;
    result->ff        = ff;

    return clamp_range(result);
}



/* bg_divide_int()-- Divide by an integer. */

bignum bg_divide_int(bignum a, int b) {

    return bg_divide(a, bg_from_int(b, a->ff));
}



/* bg_from_bi()-- Get a real from an integer. */

bignum bg_from_bi(bignum b, g95_ff *ff) {
int sign;

    sign = 1;

    if (bi_is_negative(big_copy(b))) {
	b = bi_negate(b);
	sign = -1;
    }

    b->real_sign = sign;
    b->real_exp  = ff->exp_bias + ff->digits - 1;
    b->ff        = ff;

    return clamp_range(b);
}



/* bg_from_int()-- Get a real from a machine integer */

bignum bg_from_int(int i, g95_ff *ff) {

    return bg_from_bi(int_to_bi(i), ff);
}



/* bg_halves()-- Return a half-integral number. */

bignum bg_halves(int i, g95_ff *ff) {

    return bg_divide_int(bg_from_int(i, ff), 2);
}



bignum bg_to_bi(bignum b) {
int s, e, f, m;
bignum x;

    x = big_clone(b);

    e = x->real_exp;
    f = x->ff->exp_bias + x->ff->digits - 1;
    s = x->real_sign;

    m = 0;

    while(e < f) {
	m = bi_is_odd(big_copy(x));
	x = bi_half(x);
	e++;
    }

    while(e > f) {
	x = bi_double(x);
	e--;
    }

    x->ff        = NULL;
    x->real_exp  = -1;
    x->real_sign = 0;

    x = bi_int_add(x, m);
    if (s < 0)
	x = bi_negate(x);

    return x;
}


int bg_to_int(bignum b) {

    return bi_to_int(bg_to_bi(b));
}




/* bg_trunc()-- Truncate a real towards zero. */

bignum bg_trunc(bignum a) {
g95_ff *ff;
int m, e, s;
bignum g, p;

    ff = a->ff;
    s = a->real_sign;

    switch(bg_real_type(a)) {
    case FF_NAN:
	big_free(a);
	return get_nan(ff);

    case FF_ZERO:
	big_free(a);
	return get_zero(s, ff);

    case FF_INFINITY:
	big_free(a);
	return get_infinity(s, ff);

    default:
	break;
    }

    m = ff->digits;
    e = m - (a->real_exp - ff->exp_bias) - 1;

    if (e < 0)
	return a;

    if (e > m) {
	big_free(a);
	return get_zero(s, ff);
    }

    g = big_clone(a);
    p = bi_2n(e);

    e = g->real_exp;

    g = bi_divide(g, big_copy(p));
    g = bi_multiply(g, p);

    g->real_exp  = e;
    g->real_sign = s;
    g->ff        = ff;

    return g;
}



/* bg_round()-- Round to the nearest integer.  Ties are broken with a
 * F2003's bizarre round-to-infinity. */

bignum bg_round(bignum a) {
bignum p, b, r;
int s, e, e0;
g95_ff *ff;

    ff = a->ff; 
    s = a->real_sign;
    e = e0 = a->real_exp;

    switch(bg_real_type(a)) {
    case FF_NAN:
	big_free(a);
	return get_nan(ff);

    case FF_ZERO:
	big_free(a);
	return get_zero(s, ff);

    case FF_INFINITY:
	big_free(a);
	return get_infinity(s, ff);

    default:
	break;
    }

    e = ff->digits - (a->real_exp - ff->exp_bias) - 1;

    if (e <= 0)
	return a;

    if (e > ff->digits) {
	big_free(a);
	return get_zero(s, ff);
    }

    p = bi_2n(e);
    r = bi_rem(big_copy(a), big_copy(p));

    a = bi_divide(a, big_copy(p));
    a = bi_multiply(a, big_copy(p));

    if (bi_compare(r, bi_half(big_copy(p))) >= 0)
	b = big_copy(p);

    else
	b = bi_0;

    big_free(p);

    a = bi_add(a, b);
    a->real_exp  = e0;
    a->real_sign = s;
    a->ff        = ff;

    return clamp_range(a);
}



/* floor_ceil()-- Common code for computing floor and ceiling. */

static bignum floor_ceil(bignum a, int ceiling) {
g95_ff *ff;
int z, m, e, s;
bignum g, p;

    ff = a->ff;
    s = a->real_sign;

    switch(bg_real_type(a)) {
    case FF_NAN:
	big_free(a);
	return get_nan(ff);

    case FF_ZERO:
	big_free(a);
	return get_zero(s, ff);

    case FF_INFINITY:
	big_free(a);
	return get_infinity(s, ff);

    default:
	break;
    }

    m = ff->digits;
    e = m - (a->real_exp - ff->exp_bias) - 1;

    if (e < 0)
	return a;   /* spacing(a) > 1 */

    if (e > m) {  /* 0 < |a| < 1 */
	big_free(a);

	if (ceiling && s > 0)
	    return bg_from_bi(bi_1, ff);

	if (!ceiling && s < 0)
	    return bg_from_bi(bi_m1, ff);

	return get_zero(s, ff);
    }

    g = big_clone(a);
    p = bi_2n(e);
    e = g->real_exp;

    z = bi_compare(bi_0, bi_rem(big_copy(g), big_copy(p))) != 0;

    if (z) {
	g = bi_divide(g, big_copy(p));
	g = bi_multiply(g, big_copy(p));

	z = ((ceiling && s > 0) || (!ceiling && s < 0));
	if (z)
	    g = bi_add(g, big_copy(p));
    }

    big_free(p);

    g->real_exp  = e;
    g->real_sign = s;
    g->ff        = ff;

    if (z)
	g = clamp_range(g);

    return g;
}



/* bg_floor()-- Round a number to the next lowest integer. */

bignum bg_floor(bignum a) {

    return floor_ceil(a, 0);
}



/* bg_ceiling()-- Round a number to the next highest integer. */

bignum bg_ceiling(bignum a) {

    return floor_ceil(a, 1);
}



/* newton_terminate()-- Given two iterations of a Newton iteration,
 * see if we should terminate it. */

static int newton_terminate(bignum x, bignum y) {
int ex, ey, sx, sy, m;

    ex = x->real_exp;
    ey = y->real_exp;
    m = ex - ey;

    if (m < 0)
	m = -m;

    if (m > 1) {
	big_free(x);
	big_free(y);
	return 0;
    }

    x = big_clone(x);
    y = big_clone(y);

    sx = x->real_sign;
    sy = y->real_sign;

    if (ex < ey)
	y = bi_double(y);
    else if (ex > ey)
	x = bi_double(x);

    if (sx < 0)
	x = bi_negate(x);

    if (sy < 0)
	y = bi_negate(y);

    return bi_compare(bi_abs(bi_subtract(x, y)), bi_10) <= 0;
}



/* bg_sqrt()-- Square roots using a Newton iteration. */

bignum bg_sqrt(bignum a) {
bignum g, h, q;
g95_ff *ff;
int s;

    ff = a->ff;
    s = a->real_sign;

    switch(bg_real_type(a)) {
    case FF_NAN:
	big_free(a);
	return get_nan(ff);

    case FF_ZERO:
	big_free(a);
	return get_zero(s, ff);

    case FF_INFINITY:
	big_free(a);
	return get_infinity(s, ff);

    default:
	break;
    }

    if (s < 0) {
	arithmetic_warning(AWARN_INVALID);
	big_free(a);
	return get_nan(ff);
    }

    /* Newton's method with a good initial guess */

    g = big_clone(big_copy(a));

    g->real_exp  = (a->real_exp - ((int) a->ff->exp_bias))/2 + a->ff->exp_bias;
    g->real_sign = 1;

    for(;;) {
	q = bg_divide(big_copy(a), big_copy(g));

	h = bg_add(big_copy(g), q);
	h->real_exp--;

	if (newton_terminate(big_copy(g), big_copy(h)))
	    break;

	big_free(g);
	g = h;
    }

    big_free(g);
    big_free(a);

    return h;
}



/* bg_pow_int()-- Raise a number to an integer power. */

bignum bg_pow_int(bignum b, int e) {
bignum r;

    r = bg_from_int(1, b->ff);

    if (e < 0) {
	b = bg_divide(bg_from_int(1, r->ff), b);
	e = -e;
    }

    while(e!=0) {
	if (e & 1)
	    r = bg_multiply(r, big_copy(b));

	b = bg_multiply(b, big_copy(b));
	e = e >> 1;
    }

    big_free(b);

    return r;
}



/* bg_pow()-- Raise a number to an integer power. */

bignum bg_pow(bignum b, bignum e) {
bignum r;

    if (bi_compare(big_copy(b), bi_0) == 0) {
	r = (bi_compare(big_copy(e), bi_0) < 0)
	    ? get_infinity(b->real_sign, b->ff)
	    : bg_from_int(0, b->ff);

	goto done;
    }

    if (bi_compare(big_copy(e), bi_0) < 0) {
	e = bi_negate(e);
	b = bg_divide(bg_from_int(1, b->ff), b);
    }

    if (bi_is_zero(big_copy(e)))
	r = bg_from_int(0, b->ff);

    else {
	r = bg_from_int(1, b->ff);

	for(;;) {
	    if (bi_is_odd(big_copy(e)))
		r = bg_multiply(r, big_copy(b));

	    e = bi_half(e);
	    if (bi_is_zero(big_copy(e)))
		break;

	    b = bg_multiply(b, big_copy(b));
	}
    }

done:
    big_free(b);
    big_free(e);

    return r;
}



/* bg_convert()-- Convert float types. */

bignum bg_convert(bignum b, g95_ff *ff) {
int sign;
bignum r;

    if (b->ff == ff) 
	return b;

    switch(bg_real_type(b)) {
    case FF_INFINITY:
	sign = b->real_sign;
	big_free(b);

	return get_infinity(sign, ff);

    case FF_NAN:
	big_free(b);
	return get_nan(ff);

    default:
	break;
    }

    r = big_clone(b);
    r->real_exp = r->real_exp - r->ff->exp_bias - r->ff->digits
	+ ff->exp_bias + ff->digits;

    r->ff = ff;

    return clamp_range(r);
}



/* bg_from_string()-- Convert a string to a bignum. */

bignum bg_from_string(char *p, g95_ff *f) {
int nonzero, sign, exp, seen_dp, dp_count, exp_sign, k, n;
bignum b, u, v, x, r, vmr, low, high;

    sign = 1;

    if (*p == '+')
	p++;
    else if (*p == '-') {
	sign = -1;
	p++;
    }

    b = bi_0;
    seen_dp = 0;
    dp_count = 0;
    exp = 0;
    n = 0;
    exp_sign = 1;
    nonzero = 0;

    for(;;)
	switch(*p) {
	case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    nonzero = 1;
	    /* Fall through */

	case '0':
	    b = bi_int_multiply(b, 10);
	    b = bi_int_add(b, *p++ - '0');

	    if (seen_dp)
		dp_count++;

	    break;

	case 'e': case 'd': case 'q':
	case 'E': case 'D': case 'Q':
	    p++;
	    goto parse_exponent;

	case '.':
	    seen_dp = 1;
	    p++;
	    break;

	case '\0':
	    goto done;

	default:
	    p++;
	    break;
	}

parse_exponent:
    if (*p == '+')
	p++;
    else if (*p == '-') {
	exp_sign = -1;
	p++;
    }

    for(;;) {
	if (*p == '\0')
	    break;

	exp = 10*exp + *p++ - '0';

	if (exp > 10000000) {
	    if (exp_sign > 0) {  /* Overflow */
		x = bi_0;
		k = 999999999;
	    } else {	/* Underflow to zero */
		x = bi_0;
		k = 0;
	    }

	    goto fine;
	}
    }

done:
    exp = exp_sign*exp - dp_count;

    /* At this point, we have an integer multiplied by a power of ten.
     * Find the best floating point approximation.  This is Algorithm
     * M from William Clinger's "How to Read Floating Point Numbers
     * Accurately" from Proceedings of the ACM SIGPLAN Conference
     * 1990, pages 92-101. */

    k = 0;

    if (bi_is_zero(big_copy(b))) {
	big_free(b);
	x = bi_0;
	k = 0;
	goto fine;
    }

    if (exp < 0) {
	u = b;
	v = int_power(10, -exp);

	n = f->digits - bi_bits(big_copy(u)) + bi_bits(big_copy(v));
	if (n > 0) {
	    u = bi_multiply(u, bi_2n(n));
	    k = k - n;
	}

    } else {
	u = bi_multiply(b, int_power(10, exp));

	n = bi_bits(big_copy(u)) - f->digits;
	if (n < 0)
	    n = 0;

	k = k + n;
	v = bi_2n(n);
    }

    low  = bi_2n(f->digits-1);
    high = bi_2n(f->digits);

    for(;;) {
	x = bi_divide(big_copy(u), big_copy(v));

	if (bi_compare(big_copy(x), big_copy(low)) < 0) {
	    u = bi_double(u);
	    big_free(x);
	    k--;
	    continue;
	}

	if (bi_compare(big_copy(high), big_copy(x)) <= 0) {
	    v = bi_double(v);
	    big_free(x);
	    k++;
	    continue;
	}

	break;
    }

/* See if we have to round up */

    r = bi_subtract(u, bi_multiply(big_copy(x), big_copy(v)));
    vmr = bi_subtract(v, big_copy(r));

    n = bi_compare(r, vmr);
    n = (n > 0 || (n == 0 && bi_is_odd(big_copy(x))));

    big_free(low);
    big_free(high);

fine:
    x->real_sign = sign;
    x->real_exp = k + f->exp_bias + f->digits - 1;
    x->ff = f;

    x = clamp_range(x);

    if (n && bg_real_type(x) == FF_NORMAL)
	x = next_float(x);

    if (nonzero && g95_option.prec_loss && bg_real_type(x) == FF_ZERO &&
	g95_arithmetic_locus != NULL) {
	g95_warning(164, "Real number at %L underflows to zero",
		    g95_arithmetic_locus);
	g95_arithmetic_locus = NULL;
    }

    return x;
}



/* bg_to_string()-- Convert a floating point number to a string.  Use
 * Steele and White's algorithm FPP2 from "Proceedings of ACM SIGPLAN
 * conference on programming language design and implementation",
 * 25(6):112-126 June 1990. */

char *bg_to_string(bignum f) {
int low, high, m, k, d, u, h;
static char buffer[300];
bignum r, s, mp, mm;
char *p, *q;
g95_ff *ff;

    p = buffer;

    switch(bg_real_type(f)) {
    case FF_NAN:
	strcpy(p, "NaN");
	big_free(f);
	return buffer;

    case FF_ZERO:
	if (f->real_sign < 0)
	    *p++ = '-';

	strcpy(p, "0.0");
	big_free(f);
	return buffer;

    case FF_INFINITY:
	if (f->real_sign < 0) 
	    *p++ = '-';

	strcpy(p, "Inf");
	big_free(f);
	return buffer;

    case FF_NORMAL:
    case FF_DENORMAL:
	break;
    }

    if (f->real_sign < 0)
	*p++ = '-';

    q = p;
    ff = f->ff;

    d = f->real_exp - ff->exp_bias - ff->digits + 1;

    m = (d > 0) ? d : 0;
    mm = bi_2n(m);
    mp = big_copy(mm);

    r = bi_multiply(big_copy(f), big_copy(mm));

    m = (d > 0) ? 0 : -d;
    s = bi_2n(m);

    if (bi_compare(big_copy(f), bi_2n(ff->digits-1)) == 0) {
	mp = bi_double(mp);
	r  = bi_double(r);
	s  = bi_double(s);
    }

    k = 0;

    while(bi_compare(big_copy(r),
		     bi_int_divide(bi_int_add(big_copy(s), 9), 10)) < 0) {
	k--;
	r  = bi_int_multiply(r,  10);
	mm = bi_int_multiply(mm, 10);
	mp = bi_int_multiply(mp, 10);
    }

    while(bi_compare(bi_add(big_copy(mp), bi_double(big_copy(r))),
		     bi_double(big_copy(s))) >= 0) {
	s = bi_int_multiply(s, 10);
	k++;
    }

    h = k-1;

    for(;;) {
	k--;
	u = bi_to_int(bi_divide(bi_int_multiply(big_copy(r), 10),big_copy(s)));
	r = bi_mod(bi_int_multiply(r, 10), big_copy(s));

	mm = bi_int_multiply(mm, 10);
	mp = bi_int_multiply(mp, 10);

	low = bi_compare(bi_double(big_copy(r)), big_copy(mm)) < 0;

	high = bi_compare(bi_double(big_copy(r)),
			  bi_subtract(bi_double(big_copy(s)),
				      big_copy(mp))) > 0;
	if (high || low)
	    break;

	*p++ = u + '0';
    }

    if (low && !high)
	*p++ = u + '0';
    else if (!low && high)
	*p++ = u + '1';
    else
	*p++ = u +
	    (bi_compare(bi_double(big_copy(r)),
			big_copy(s)) < 0)
	    ? '0' : '1';

    big_free(r);
    big_free(s);
    big_free(mm);
    big_free(mp);
    big_free(f);

    *p++ = '\0';
    memmove(q+2, q+1, strlen(q)+1);
    q[1] = '.';

    sprintf(p, "E%+d", h);
    return buffer;
}



/* calculate_pi()-- High precision calculation of pi.  We use the
 * Bailey, Borwein and Plouffe formula:
 *
 * pi = \sum_{n=0}^\infty (1/16)^n [4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6)]
 *
 * which gives about four bits per iteration. */

static void calculate_pi(void) {
bignum d, t, u, pi;
int i, limit;

    pi = bg_from_int(0, &super);
    d = bg_from_int(1, &super);

    limit = (super.man_len / 4) + 10;

    for(i=0; i<limit; i++) {
	t = bg_divide_int(bg_from_int(4, &super), 8*i+1);

	u = bg_from_int(2, &super);
	u = bg_divide_int(u, 8*i+4);

	t = bg_subtract(t, u);
	t = bg_subtract(t, bg_divide_int(bg_from_int(1, &super), 8*i+5));
	t = bg_subtract(t, bg_divide_int(bg_from_int(1, &super), 8*i+6));

	pi = bg_add(pi, bg_divide(t, big_copy(d)));
	d = bg_multiply(d, bg_from_int(16, &super));
    }

    big_free(d);

    bg_pi = pi;
    big_permanent(bg_pi);

    bg_half_pi = bg_divide_int(bg_pi, 2);
    big_permanent(bg_half_pi);

    bg_two_pi = bg_multiply_int(bg_pi, 2);
    big_permanent(bg_two_pi);
}



/* calculate_e()-- Calculate a high precision value for e. */

static void calculate_e(void) {
bignum e, f, a;
int i;

    e = bg_from_int(0, &super);
    a = bg_from_int(1, &super);

    i = 1;
    for(;;) {
	f = big_copy(e);
	e = bg_add(e, big_copy(a));

	if (newton_terminate(big_copy(e), f))
	    break;

	a = bg_divide_int(a, i++);
    }

    big_free(a);

    bg_e = e;
    big_permanent(bg_e);
}



/* calculate_ln()-- Calculate the natural logarithm of two. 
 * We use the series:
 * ln 2 = 2 [ (1/3) + (1/3)^3/3 + (1/3)^5/5 + (1/3)^5 + ... ] */

static void calculate_ln2(void) {
bignum m, r, s, t, ninth;
int i;

    s = get_zero(0, &super);

    r = bg_divide_int(bg_from_int(1, &super), 3);
    ninth = bg_divide_int(bg_from_int(1, &super), 9);
    i = 1;

    for(;;) {
	t = big_copy(s);

	m = bg_divide_int(big_copy(r), i);

	s = bg_add(s, m);

	if (newton_terminate(big_copy(s), t))
	    break;

	r = bg_multiply(r, big_copy(ninth));
	i += 2;
    }

    big_free(r);
    big_free(ninth);

    bg_ln2 = bg_multiply_int(s, 2);
    big_permanent(bg_ln2);
}



int bg_exponent(bignum x) {
int e;

    if (x->real_exp != 0)
	e = x->real_exp - x->ff->exp_bias + 1;

    else
	e = -x->ff->exp_bias + 2 - x->ff->digits + bi_bits(big_copy(x));

    big_free(x);

    return e;
}


bignum bg_fraction(bignum x) {
g95_ff *ff;

    ff = x->ff;
    switch(bg_real_type(x)) {
    case FF_NAN:
    case FF_INFINITY:
	return x;

    case FF_ZERO:
	big_free(x);
	return get_zero(0, ff);

    default:
	break;
    }

    x = big_clone(x);
    x->real_exp = x->ff->exp_bias - 1;

    return x;
}



bignum bg_sine(bignum x) {
bignum sq, num, den, y;
g95_ff *ff;
int i, m;

    switch(bg_real_type(x)) {
    case FF_INFINITY:
	big_free(x);
	arithmetic_warning(AWARN_INVALID);
	break;

    case FF_NAN:
    case FF_ZERO:
	return x;

    default:
	break;
    }

    ff = x->ff;
    x = bg_convert(x, &super);

    y = bg_trunc(bg_divide(big_copy(x), bg_two_pi));
    x = bg_subtract(x, bg_multiply(bg_two_pi, y));

    sq = bg_multiply(big_copy(x), big_copy(x));

    num = big_copy(x);
    den = bg_from_int(1, bg_pi->ff);

    i = 1;
    m = 0;
    for(;;) {
	num = bg_multiply(num, big_copy(sq));
	den = bg_multiply_int(den, (i+1)*(i+2));

	i += 2;
	y = big_copy(x);

	if (m)
	    x = bg_add(x, bg_divide(big_copy(num), big_copy(den)));
	else
	    x = bg_subtract(x, bg_divide(big_copy(num), big_copy(den)));

	m = !m;
	if (bg_compare(big_copy(x), y) == 0 && i > 20)
	    break;
    }

    big_free(sq);
    big_free(num);
    big_free(den);

    return bg_convert(x, ff);
}



bignum bg_cosine(bignum x) {
bignum sq, num, den, y;
g95_ff *ff;
int i, m;

    switch(bg_real_type(x)) {
    case FF_INFINITY:
	big_free(x);
	arithmetic_warning(AWARN_INVALID);
	break;

    case FF_NAN:
	return x;

    default:
	break;
    }

    ff = x->ff;
    x = bg_convert(x, bg_pi->ff);

    y = bg_trunc(bg_divide(big_copy(x), bg_two_pi));
    x = bg_subtract(x, bg_multiply(bg_two_pi, y));
    sq = bg_multiply(x, big_copy(x));

    x   = bg_from_int(1, bg_pi->ff);
    num = bg_from_int(1, bg_pi->ff);
    den = bg_from_int(1, bg_pi->ff);

    i = 0;
    m = 0;
    for(;;) {
	num = bg_multiply(num, big_copy(sq));
	den = bg_multiply_int(den, (i+1)*(i+2));

	i += 2;
	y = big_copy(x);

	if (m)
	    x = bg_add(x, bg_divide(big_copy(num), big_copy(den)));
	else
	    x = bg_subtract(x, bg_divide(big_copy(num), big_copy(den)));

	m = !m;

	if (bg_compare(big_copy(x), y) == 0 && i > 20)
	    break;
    }

    big_free(sq);
    big_free(num);
    big_free(den);

    return bg_convert(x, ff);
}



bignum bg_hyperbolic_sine(bignum x) {
bignum y, n, d, t;
g95_ff *ff;
int i;

    switch(bg_real_type(x)) { 
    case FF_INFINITY:
    case FF_NAN:
	return x;

    default:
	break;
    }

    ff = x->ff;
    x = bg_convert(x, &super);

    if (bg_compare(bg_abs(big_copy(x)), bg_from_int(1, &super)) > 0) {
	y = bg_subtract(bg_exponential(big_copy(x)),
			bg_exponential(bg_negate(big_copy(x))));
	y = bg_divide_int(y, 2);
    } else {  /* The usual formula has cancellation problems if |x| << 1 */
	n = big_copy(x);
	y = big_copy(x);

	x = bg_multiply(big_copy(x), x);
	d = bg_from_int(1, &super);
	i = 2;

	do {
	    t = big_copy(y);
	    n = bg_multiply(n, big_copy(x));
	    d = bg_multiply_int(d, i*(i+1));
	    i += 2;

	    y = bg_add(y, bg_divide(big_copy(n), big_copy(d)));
	} while(!newton_terminate(big_copy(y), t));

	big_free(n);
	big_free(d);
    }

    big_free(x);
    return bg_convert(y, ff);
}



bignum bg_hyperbolic_cosine(bignum x) {
g95_ff *ff;
bignum y;

    ff = x->ff;

    switch(bg_real_type(x)) {
    case FF_NAN:
	return x;

    case FF_INFINITY:
	return get_infinity(0, ff);

    default:
	break;
    }

    x = bg_convert(x, &super);

    y = bg_add(bg_exponential(big_copy(x)),
	       bg_exponential(bg_negate(big_copy(x))));
    big_free(x);

    return bg_convert(bg_divide_int(y, 2), ff);
}



bignum bg_exponential(bignum x) {
bignum num, den, e, f, r;
g95_ff *ff;
int i, n;

    ff = x->ff;
    n = 0;

    switch(bg_real_type(x)) {
    case FF_INFINITY:
	if (x->real_sign > 0)
	    return x;
    
	big_free(x);
	return get_zero(0, ff);

    case FF_NAN:
	return x;

	/* Compute N and r such that x = N ln2 + r, where -ln2 < r < ln2.
	 * Then exp(x) = 2^N exp(r), where exp(r) comes from the
	 * MacLaurin series. */

    case FF_ZERO:
	n = 0;
	x = bg_convert(x, &super);
	break;

    case FF_NORMAL:
    case FF_DENORMAL:
	x = bg_convert(x, &super);
	e = bg_trunc(bg_divide(big_copy(x), bg_ln2));

	if (bg_compare_int(big_copy(e), 10000) > 0) {
	    big_free(e);
	    big_free(x);
	    return get_infinity(0, ff);
	}

	if (bg_compare_int(big_copy(e), -10000) < 0) {
	    big_free(e);
	    big_free(x);
	    return get_zero(0, ff);
	}

	n = bg_to_int(e);
	break;
    }

    /* Compute exp(r) */

    r = bg_subtract(x, bg_multiply_int(bg_ln2, n));
    e = bg_from_int(1, &super);

    num = big_copy(r);
    den = bg_from_int(1, &super);
    i = 2;

    for(;;) {
	f = big_copy(e);
	e = bg_add(e, bg_divide(big_copy(num), big_copy(den)));

	if (newton_terminate(big_copy(e), f))
	    break;

	num = bg_multiply(num, big_copy(r));
	den = bg_multiply_int(den, i++);
    }

    big_free(num);
    big_free(den);
    big_free(r);

    e = bg_multiply(e, bg_pow_int(bg_from_int(2, &super), n));
    return bg_convert(e, ff);
}



/* bg_ln()-- Natural logarithm */

bignum bg_ln(bignum x) {
bignum a, r, y, num;
int i, m, n;
g95_ff *ff;

    ff = x->ff;

    switch(bg_real_type(x)) { 
    case FF_NAN:
	return x;

    case FF_INFINITY:
	if (x->real_sign > 0)
	    return x;

	/* Fall through */

    case FF_ZERO:
    invalid:
	big_free(x);
	arithmetic_warning(AWARN_INVALID);
	return get_nan(ff);

    default:
	break;
    }

    if (x->real_sign < 0)
	goto invalid;

    /* Decompose x into: x = m * 2^N where 3/4 < m < 1.  Then
     * ln(x) = ln(m) + N ln2. */

    n = bg_exponent(big_copy(x));
    x = bg_fraction(bg_convert(x, &super));

    a = bg_divide_int(bg_from_int(3, &super), 4);

    while(bg_compare(big_copy(x), big_copy(a)) < 0) {
	x = bg_multiply_int(x, 2);
	n--;
    }

    big_free(a);

    x = bg_add_int(x, -1);
    r = get_zero(0, &super);

    i = 1;
    m = 1;
    num = bg_from_int(1, &super);

    do {
	y = big_copy(r);
	num = bg_multiply(num, big_copy(x));

	if (m)
	    r = bg_add(r, bg_divide_int(big_copy(num), i++));
	else
	    r = bg_subtract(r, bg_divide_int(big_copy(num), i++));

	m = !m;
    } while(!newton_terminate(big_copy(r), y));

    big_free(num);
    big_free(x);

    return bg_convert(bg_add(r, bg_multiply_int(bg_ln2, n)), ff);
}



bignum bg_log(bignum x) {
g95_ff *ff;

    ff = x->ff;
    x = bg_convert(x, &super);

    return bg_convert(bg_divide(bg_ln(x), bg_ln10), ff);
}



bignum bg_power(bignum b, bignum e) {
ff_class b1, e1;
g95_ff *ff;
int m;

    b1 = bg_real_type(b); 
    e1 = bg_real_type(e);
    ff = e->ff;

    if (b1 == FF_NAN || e1 == FF_NAN) {
	big_free(b);
	big_free(e);

	return get_nan(ff);
    }

    if (b1 == FF_INFINITY) {
	switch(e1) {
	case FF_INFINITY:
	default:
	    if (b->real_sign > 0 && e->real_sign > 0) {
		big_free(e);
		return b;
	    }

	    goto return_nan;

	case FF_ZERO:
	    goto return_nan;
	}
    }

    m = bg_compare_int(big_copy(b), 0);
    ff = b->ff;

    if (m == 0) {  /* 0.0^e */
	m = (e1 == FF_ZERO);
	big_free(b);

	return bg_from_int(m, ff);
    }

    if (m > 0)     /* b > 0, compute: b^e = exp(e ln b) */
	return bg_exponential(bg_multiply(e, bg_ln(b)));

    /* If b < 0, then e has to be an integer to avoid a not-a-number. */

    if (bg_compare(bg_trunc(big_copy(e)), big_copy(e)) != 0)
	goto return_nan;

    return bg_pow_int(b, bg_to_int(e));

return_nan:
    big_free(b);
    big_free(e);
    return get_nan(ff);
}



bignum bg_arctangent(bignum x) {
bignum num, term, sq, s, t;
int i, m, sign;
g95_ff *ff;

    sign = x->real_sign; 
    ff = x->ff;

    switch(bg_real_type(x)) {
    case FF_ZERO:
    case FF_NAN:
	return x;

    case FF_INFINITY:
	big_free(x);
	return (sign > 0)
	    ? bg_half_pi
	    : bg_negate(bg_half_pi);

    case FF_NORMAL:
    case FF_DENORMAL:
	break;
    }

    if (bg_compare_int(big_copy(x), 1) == 0) {
	big_free(x);
	s = bg_divide_int(bg_pi, 4);
	goto done;
    }

    if (bg_compare_int(big_copy(x), -1) == 0) {
	big_free(x);
	s = bg_negate(bg_divide_int(bg_pi, 4));
	goto done;
    }

    x = bg_convert(x, &super);

    sq = bg_multiply(big_copy(x), big_copy(x));
    x = bg_abs(x);
    i = 1;

    if (bg_compare_lt(bg_abs(big_copy(x)), bg_halves(1, &super))) {
	m = 1;
	s = get_zero(0, &super);
	num = big_copy(x);

	do {
	    t = big_copy(s);
	    term = bg_divide_int(big_copy(num), i);

	    if (m) 
		s = bg_add(s, term);
	    else
		s = bg_subtract(s, term);

	    m = !m;
	    i += 2;
	    num = bg_multiply(num, big_copy(sq));
	} while(!newton_terminate(big_copy(s), t));

    } else if (bg_compare_lt(bg_abs(big_copy(x)), bg_halves(3, &super))) {
	s = bg_divide(bg_add_int(big_copy(x), -1), bg_add_int(big_copy(x), 1));
	big_free(x);
	x = s;

	s = bg_divide_int(bg_pi, 4);
	num = big_copy(x);

	big_free(sq);
	sq = bg_multiply(big_copy(x), big_copy(x));

	m = 1;

	do {
	    t = big_copy(s);
	    term = bg_divide_int(big_copy(num), i);

	    if (m) 
		s = bg_add(s, term);
	    else
		s = bg_subtract(s, term);

	    m = !m;
	    i += 2;
	    num = bg_multiply(num, big_copy(sq));
	} while(!newton_terminate(big_copy(s), t));

    } else {
	s = bg_half_pi;
	num = bg_divide(bg_from_int(1, &super), big_copy(x));
	m = 0;

	do {
	    t = big_copy(s);
	    term = bg_divide_int(big_copy(num), i);

	    if (m)
		s = bg_add(s, term);
	    else
		s = bg_subtract(s, term);

	    m = !m;
	    i += 2;
	    num = bg_divide(num, big_copy(sq));
	} while(!newton_terminate(big_copy(s), t));
    }

    s->real_sign *= sign;

    big_free(num);
    big_free(sq);
    big_free(x);

done:
    return bg_convert(s, ff);
}



bignum bg_hypot(bignum a, bignum b) {
g95_ff *ff;
bignum t, x;

    ff = a->ff;
    a = bg_abs(bg_convert(a, &super));
    b = bg_abs(bg_convert(b, &super));

    if (bg_compare_lt(big_copy(a), big_copy(b))) {
	t = a;
	a = b;
	b = t;
    }

    if (bg_real_type(a) == FF_ZERO) {
	big_free(a);
	big_free(b);
	return get_zero(0, ff);
    }

    x = bg_divide(b, big_copy(a));
    t = bg_multiply(big_copy(x), big_copy(x));
    big_free(x);

    return bg_convert(bg_multiply(a, bg_sqrt(bg_add_int(t, 1))), ff);
}



bignum bi_huge(int kind) {

    return bi_int_add(bi_2n(g95_bit_size(kind)-1), -1);
}



/* bg_huge()-- Return the HUGE() value */

bignum bg_huge(g95_ff *ff) {
bignum b;

    b = max_mantissa(ff);
    b->real_sign = 1;
    b->real_exp = ff->max_exponent + ff->exp_bias - 1;
    b->ff = ff;

    return b;
}



/* bg_tiny()-- Return the TINY() value */

bignum bg_tiny(g95_ff *ff) {
bignum b;

    b = min_mantissa(ff);
    b->real_sign = 1;
    b->real_exp = ff->min_exponent + ff->exp_bias - 1;
    b->ff = ff;

    return b;
}



/* bg_epsilon()-- Return the EPSILON() value */

bignum bg_epsilon(g95_ff *ff) {

    return bg_pow_int(bg_from_int(ff->radix, ff), 1 - ff->digits);
}



/* set_bit()-- Set a bit in a machine real */

static void set_bit(char *mem, g95_ff *ff, int bit, int value) {
int n, mask;

    n = bit >> 3;

    if (ff->endian == END_LITTLE)
	n = (ff->totalsize >> 3) - n - 1;

    mask = 1 << (7 - (bit & 0x07));

    if (value)
	mem[n] |= mask;
    else
	mem[n] &= ~mask;
}



/* read_bit()-- Read a bit from memory */

static int read_bit(char *mem, g95_ff *ff, int bit) {
int n, mask;

    n = bit >> 3;

    if (ff->endian == END_LITTLE)
	n = (ff->totalsize >> 3) - n - 1;

    mask = 1 << (7 - (bit & 0x07));

    return !!(mem[n] & mask);
}



/* g95_pack_real()-- Pack a bignum into a real number */

void g95_pack_real(bignum f, char *mem) {
g95_ff *ff;
int i;

    ff = f->ff;
    set_bit(mem, ff, ff->sign_start, f->real_sign < 0);

    for(i=0; i<ff->exp_len; i++)
	set_bit(mem, ff, ff->exp_start+ff->exp_len-i-1,
		f->real_exp & (1 << i));

    for(i=0; i<ff->man_len; i++) {
	set_bit(mem, ff, ff->man_start+ff->man_len-i-1,bi_is_odd(big_copy(f)));
	f = bi_half(f);
    }

    big_free(f);
}



/* g95_unpack_real()-- Unpack a real number into a bignum */

bignum g95_unpack_real(char *mem, g95_ff *ff) {
int exp, i, sign;
bignum r;

    sign = read_bit(mem, ff, ff->sign_start) ? -1 : 1;
    exp = 0;

    for(i=0; i<ff->exp_len; i++)
	exp = 2*exp + read_bit(mem, ff, ff->exp_start+i);

    r = bi_0;

    for(i=0; i<ff->man_len; i++)
	r = bi_int_add(bi_double(r), read_bit(mem, ff, ff->man_start+i));

    if (ff->msb == MSB_IMPLICIT && (exp != 0 && exp != ff->exp_nan))
	r = bi_add(r, min_mantissa(ff));

    r->real_sign = sign;
    r->real_exp  = exp;
    r->ff        = ff;

    return r;
}



/* g95_get_ff()-- Get a float format from a kind. */

g95_ff *g95_get_ff(int kind) {
int i;

    i = g95_validate_kind(BT_REAL, kind);
    if (i < 0)
	g95_internal_error("g95_get_ff(): Bad kind");

    return &g95_real_kinds[i];
}



static void init_floatformat(g95_ff *ff) {
double logb;
int a, b;

    ff->digits = ff->man_len + (ff->msb == MSB_IMPLICIT);

    ff->min_exponent = 2 - ff->exp_bias;
    ff->max_exponent = ff->exp_nan - ff->exp_bias;

    logb = 0.301029995664;

    a = ff->max_exponent * logb;
    b = -ff->min_exponent * logb;
    ff->range = (a < b) ? a : b;

    ff->precision = (ff->digits-1) * logb;
    if (ff->radix == 10 || ff->radix == 100)
	ff->precision++;
}



/* init_super()-- Initialize the extended precision float format */

static void init_super(void) {
g95_ff *f, *g;

    g = NULL;
    f = g95_real_kinds;

    while(f->kind > 0) {
	if (g == NULL || f->kind > g->kind)
	    g = f;

	f++;
    }

    super.kind = -1;
    super.radix = 2;

    super.exp_len = g->exp_len + 4;
    super.man_len = g->man_len + 6;

    super.exp_nan  = (1 << super.exp_len) - 1;
    super.exp_bias = super.exp_nan / 2;

    super.msb          = MSB_EXPLICIT;
    super.digits       = super.man_len;
    super.min_exponent = 2 - super.exp_bias;
    super.max_exponent = super.exp_nan - super.exp_bias;
}



void bg_init(void) {
g95_ff *ff;

    init_super();

    calculate_ln2();
    calculate_pi();
    calculate_e();

    bg_ln10 = bg_ln(bg_from_int(10, &super));
    big_permanent(bg_ln10);

    ff = g95_real_kinds;
    while(ff->kind > 0)
	init_floatformat(ff++);
}


void bg_done(void) {

    big_depermanent(bg_pi);
    big_free(bg_pi);

    big_depermanent(bg_two_pi);
    big_free(bg_two_pi);

    big_depermanent(bg_half_pi);
    big_free(bg_half_pi);

    big_depermanent(bg_e);
    big_free(bg_e);

    big_depermanent(bg_ln2);
    big_free(bg_ln2);

    big_depermanent(bg_ln10);
    big_free(bg_ln10);
}
