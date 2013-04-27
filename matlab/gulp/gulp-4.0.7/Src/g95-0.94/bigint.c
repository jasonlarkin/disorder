
/* bigint - internal portion of large integer package
**
** Copyright © 2000 by Jef Poskanzer <jef@acme.com>.
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
** OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
** LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
** OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
** SUCH DAMAGE.
*/

/* Modified for g95 by Andy Vaught */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#include "g95.h"

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))

/* MAXINT and MININT extracted from <values.h>, which gives a warning
** message if included.
*/
#define BITSPERBYTE 8
#define BITS(type)  (BITSPERBYTE * (int)sizeof(type))
#define INTBITS     BITS(int)
#define MININT      ((1 << (INTBITS - 1)) + 1)
#define MAXINT      ((~MININT)+1)


/* The package represents arbitrary-precision integers as a sign and a sum
** of components multiplied by successive powers of the basic radix, i.e.:
**
**   sign * ( comp0 + comp1 * radix + comp2 * radix^2 + comp3 * radix^3 )
**
** To make good use of the computer's word size, the radix is chosen
** to be a power of two.  It could be chosen to be the full word size,
** however this would require a lot of finagling in the middle of the
** algorithms to get the inter-word overflows right.  That would slow things
** down.  Instead, the radix is chosen to be *half* the actual word size.
** With just a little care, this means the words can hold all intermediate
** values, and the overflows can be handled all at once at the end, in a
** normalization step.  This simplifies the coding enormously, and is probably
** somewhat faster to run.  The cost is that numbers use twice as much
** storage as they would with the most efficient representation, but storage
** is cheap.
**
** A few more notes on the representation:
**
**  - The sign is always 1 or -1, never 0.  The number 0 is represented
**    with a sign of 1.
**  - The components are signed numbers, to allow for negative intermediate
**    values.  After normalization, all components are >= 0 and the sign is
**    updated.
*/

#undef DUMP

#define PERMANENT 123456789

static comp bi_radix, bi_radix_o2;
static int bi_radix_sqrt, bi_comp_bits;

static bignum active_list, free_list;
static int active_count, free_count;
static int check_level;



#ifdef DUMP
static void dump(char *, bignum);
#endif /* DUMP */
static int csqrt0(comp c);
static int cbits(comp c);

bignum bi_0, bi_1, bi_2, bi_10, bi_m1, bi_maxint, bi_minint;



void big_initialize(void) {

/* Set the radix.  This does not actually have to be a power of
** two, that's just the most efficient value.  It does have to
** be even for bi_half() to work.
*/

    bi_radix = 1;
    bi_radix <<= BITS(comp) / 2 - 1;

/* Halve the radix.  Only used by bi_half(). */
    bi_radix_o2 = bi_radix >> 1;

/* Take the square root of the radix.  Only used by bi_divide(). */
    bi_radix_sqrt = csqrt0(bi_radix);

/* Figure out how many bits in a component.  Only used by bi_bits(). */
    bi_comp_bits = cbits(bi_radix - 1);

/* Init various globals. */
    active_list  = (bignum) 0;
    active_count = 0;
    free_list    = (bignum) 0;
    free_count   = 0;

/* This can be 0 through 3. */
    check_level = 0;

/* Set up some convenient bigints. */

    bi_0 = int_to_bi(0);             big_permanent(bi_0);
    bi_1 = int_to_bi(1);             big_permanent(bi_1);
    bi_2 = int_to_bi(2);             big_permanent(bi_2);
    bi_10 = int_to_bi(10);           big_permanent(bi_10);
    bi_m1 = int_to_bi(-1);           big_permanent(bi_m1);
    bi_maxint = int_to_bi(MAXINT);   big_permanent(bi_maxint);

    bi_minint = bi_subtract(bi_negate(bi_maxint), bi_1);
    big_permanent(bi_minint);
}



static void triple_check(void) {
bignum p;
int c;

    for(p=active_list, c=0; p!=(void *) 0; p=(bignum) p->next, c++)
	if (p->refs == 0)
	    g95_internal_error("triple_check: found a zero ref on the "
			       "active list");

    if (c != active_count)
	g95_internal_error("triple_check: "
			   "active_count is %d but active_list has %d items",
			   free_count, c);
}



static void double_check(void) {
bignum p;
int c;

    for(p=free_list, c=0; p!=(void *) 0; p=(bignum) p->next, c++)
	if (p->refs != 0)
	    g95_internal_error("double_check: non-zero ref on the free list");

    if (c != free_count)
	g95_internal_error("double_check: "
			   "free_count is %d but the free list has %d items",
			   free_count, c);
}



static void check(bignum bi) {

    if (check_level == 0)
	return;

    if (bi->refs == 0)
	g95_internal_error("check: zero refs in bigint");

    if (bi->refs < 0)
	g95_internal_error("check: negative refs in bigint");

    if (check_level < 3) {
	/* At check levels less than 3, active bigints have a zero next. */
	if (bi->next != (void *) 0)
	    g95_internal_error("check: attempt to use a bigint from the free "
			       "list");

    } else {
	/* At check levels 3 or higher, active bigints must be on the active
	** list. */
	bignum p;

	for(p=active_list; p != (void *) 0; p=(bignum) p->next)
	    if (p == bi)
		break;

	if (p == (bignum) 0)
	    g95_internal_error("check: using a bigint not on the active list");
    }

    if (check_level >= 2)
	double_check();

    if (check_level >= 3)
	triple_check();
}



void big_terminate(void) {
bignum p, pn;

    big_depermanent(bi_0);        big_free(bi_0);
    big_depermanent(bi_1);        big_free(bi_1);
    big_depermanent(bi_2);        big_free(bi_2);
    big_depermanent(bi_10);       big_free(bi_10);
    big_depermanent(bi_m1);       big_free(bi_m1);
    big_depermanent(bi_maxint);   big_free(bi_maxint);
    big_depermanent(bi_minint);   big_free(bi_minint);

    if (active_count != 0 && check_level != 0)
	g95_internal_error("big_terminate: there were %d un-freed bigints",
			   active_count);

    if (check_level >= 2)
	double_check();

    if (check_level >= 3) {
	triple_check();
	for(p=active_list; p!=(bignum) 0; p=pn) {
	    pn = (bignum) p->next;
	    g95_free(p->comps);
	    g95_free(p);
	}
    }

    for(p=free_list; p!=(bignum) 0; p=pn) {
	pn = (bignum) p->next;
	g95_free(p->comps);
	g95_free(p);
    }
}



bignum big_copy(bignum bi) {

    check(bi);

    if (bi->refs != PERMANENT)
	bi->refs++;

    return bi;
}



void big_permanent(bignum bi) {

    check(bi);

    if (check_level >= 1 && bi->refs != 1)
	g95_internal_error("big_permanent: refs was not 1");

    bi->refs = PERMANENT;
}



void big_depermanent(bignum bi) {

    check(bi);

    if (check_level >= 1 && bi->refs != PERMANENT)
	g95_internal_error("big_depermanent: bignum was not permanent");

    bi->refs = 1;
}



/* big_clone_perm()-- Call big_clone() and big_permanent() on a
 * number.  Numbers stored in various structures are permanent, but we
 * need to be careful to duplicate numbers so that everything can be
 * freed correctly.  The regular code should not call
 * big_permanent(). */

bignum big_clone_perm(bignum bi) {

    bi = big_clone(bi);
    big_permanent(bi);

    return bi;
}



void big_free(bignum bi) {

    check(bi);
    if (bi->refs == PERMANENT)
	return;

    bi->refs--;

    if (bi->refs > 0)
	return;

    if (check_level >= 3) {
	/* The active list only gets maintained at check levels 3 or higher. */
	bignum *nextP;

	for(nextP=&active_list; *nextP!=(bignum) 0;
	    nextP=(bignum *) &((*nextP)->next))

	    if (*nextP == bi) {
		*nextP = (bignum) bi->next;
		break;
	    }
    }

    active_count--;

    bi->next = (void *) free_list;
    free_list = bi;
    free_count++;

    if (check_level >= 1 && active_count < 0)
	g95_internal_error("big_free: active_count went negative "
			   "- double-freed bigint?");
}



int bi_compare(bignum bia, bignum bib) {
int r, c;

    check(bia);
    check(bib);

/* First check for pointer equality. */

    if (bia == bib)
	r = 0;

    else { /* Compare signs. */
	if (bia->sign > bib->sign)
	    r = 1;
	else if (bia->sign < bib->sign)
	    r = -1;  /* Signs are the same.  Check the number of components. */
	else if (bia->num_comps > bib->num_comps)
	    r = bia->sign;
	else if (bia->num_comps < bib->num_comps)
	    r = -bia->sign;
	else {
	    /* Same number of components.  Compare starting from the high end
	    ** and working down.
	    */

	    r = 0;	/* if we complete the loop, the numbers are equal */
	    for(c=bia->num_comps-1; c>=0; c--) {
		if (bia->comps[c] > bib->comps[c]) {
		    r = bia->sign;
		    break;
		} else if (bia->comps[c] < bib->comps[c]) {
		    r = -bia->sign;
		    break;
		}
	    }
	}
    }

    big_free(bia);
    big_free(bib);

    return r;
}



/* Allocate and zero more components.  Does not consume bi, of course. */

static void more_comps(bignum bi, int n) {

    if (n > bi->max_comps) {
	bi->max_comps = max(bi->max_comps * 2, n);
	bi->comps =
	    (comp*) realloc((void*) bi->comps, bi->max_comps * sizeof(comp));

	if (bi->comps == (comp*) 0)
	    g95_internal_error("out of memory");
    }

    for(; bi->num_comps<n; bi->num_comps++)
	bi->comps[bi->num_comps] = 0;
}



/* Put bi into normal form.  Does not consume bi, of course.
**
** Normal form is:
**  - All components >= 0 and < bi_radix.
**  - Leading 0 components removed.
**  - Sign either 1 or -1.
**  - The number zero represented by a single 0 component and a sign of 1.
*/

static void normalize(bignum bi) {
int c;

  /* Borrow for negative components.  Got to be careful with the math here:
  **   -9 / 10 == 0    -9 % 10 == -9
  **   -10 / 10 == -1  -10 % 10 == 0
  **   -11 / 10 == -1  -11 % 10 == -1
  */

    for(c=0; c<bi->num_comps-1; c++)
	if (bi->comps[c] < 0) {
	    bi->comps[c+1] += bi->comps[c] / bi_radix - 1;
	    bi->comps[c] = bi->comps[c] % bi_radix;

	    if (bi->comps[c] != 0)
		bi->comps[c] += bi_radix;
	    else
		bi->comps[c+1] += 1;
	}

    /* Is the top component negative? */
    if (bi->comps[bi->num_comps - 1] < 0) {
	/* Switch the sign of the number, and fix up the components. */
	bi->sign = -bi->sign;

	for(c=0; c<bi->num_comps-1; c++) {
	    bi->comps[c] = bi_radix - bi->comps[c];
	    bi->comps[c + 1] += 1;
	}

	bi->comps[bi->num_comps-1] = -bi->comps[bi->num_comps-1];
    }

    /* Carry for components larger than the radix. */
    for(c=0; c<bi->num_comps; c++)
	if (bi->comps[c] >= bi_radix) {
	    if (c + 1 >= bi->num_comps)
		more_comps(bi, bi->num_comps + 1);

	    bi->comps[c+1] += bi->comps[c] / bi_radix;
	    bi->comps[c] = bi->comps[c] % bi_radix;
	}

    /* Trim off any leading zero components. */
    for(; bi->num_comps > 1 && bi->comps[bi->num_comps-1]==0; bi->num_comps--)
	;

    /* Check for -0. */
    if (bi->num_comps == 1 && bi->comps[0] == 0 && bi->sign == -1)
	bi->sign = 1;
}



/* Make a new empty bigint.  Fills in everything except sign and the
** components.
*/

static bignum alloc(int num_comps) {
static int serial=0;
bignum biR;

  /* Can we recycle an old bigint? */

    if (free_list != (bignum) 0) {
	biR = free_list;
	free_list = (bignum) biR->next;
	free_count--;

	if (check_level >= 1 && biR->refs != 0)
	    g95_internal_error("alloc: refs was not 0");

	more_comps(biR, num_comps);

    } else {    /* No free bigints available - create a new one. */
	biR = (bignum) g95_getmem(sizeof(_bignum));
	biR->real_exp = -1;

	biR->comps = (comp*) g95_getmem(num_comps * sizeof(comp));
	biR->max_comps = num_comps;
    }

    biR->num_comps = num_comps;
    biR->refs = 1;
    biR->typeless = 0;
    
    if (check_level >= 3) {
	/* The active list only gets maintained at check levels 3 or higher. */
	biR->next = (void *) active_list;
	active_list = biR;
    } else
	biR->next = (void *) 0;

    active_count++;

    biR->serial = serial++;
    return biR;
}



/* Make a modifiable copy of bi.  DOES consume bi. */

bignum big_clone(bignum bi) {
bignum biR;
int c;

    /* Very clever optimization. */
    if (bi->refs != PERMANENT && bi->refs == 1)
	return bi;

    biR = alloc(bi->num_comps);

    biR->sign      = bi->sign;
    biR->real_exp  = bi->real_exp;
    biR->real_sign = bi->real_sign;
    biR->typeless  = bi->typeless;
    biR->ff        = bi->ff;

    for(c=0; c<bi->num_comps; c++)
	biR->comps[c] = bi->comps[c];

    big_free(bi);

    return biR;
}



bignum int_to_bi(int i) {
bignum biR;

    biR = alloc(1);

    biR->sign     = 1;
    biR->comps[0] = i;

    normalize(biR);
    check(biR);

    return biR;
}



int bi_to_int(bignum bi) {
comp v, m;
int c, r;

    check(bi);
    if (bi_compare(big_copy(bi), bi_maxint) > 0 ||
	bi_compare(big_copy(bi), bi_minint) < 0)
	g95_internal_error("bi_to_int: overflow");

    v = 0;
    m = 1;
    for(c=0; c<bi->num_comps; c++) {
	v += bi->comps[c] * m;
	m *= bi_radix;
    }

    r = (int) (bi->sign * v);
    big_free(bi);

    return r;
}



bignum bi_int_add(bignum bi, int i) {
bignum biR;

    check(bi);

    biR = big_clone(bi);

    if (biR->sign == 1)
	biR->comps[0] += i;
    else
	biR->comps[0] -= i;

    normalize(biR);
    check(biR);

    return biR;
}



bignum bi_int_subtract(bignum bi, int i) {
bignum biR;

    check(bi);

    biR = big_clone(bi);

    if (biR->sign == 1)
	biR->comps[0] -= i;
    else
	biR->comps[0] += i;

    normalize(biR);
    check(biR);

    return biR;
}



bignum bi_int_multiply(bignum bi, int i) {
bignum biR;
int c;

    check(bi);
    biR = big_clone(bi);

    if (i < 0) {
	i = -i;
	biR->sign = -biR->sign;
    }

    for(c=0; c<biR->num_comps; c++)
	biR->comps[c] *= i;

    normalize(biR);
    check(biR);

    return biR;
}



bignum bi_int_divide(bignum binumer, int denom) {
bignum biR;
comp r;
int c;

    check(binumer);

    if (denom == 0)
	g95_internal_error("bi_int_divide: divide by zero");

    biR = big_clone(binumer);
    if (denom < 0) {
	denom = -denom;
	biR->sign = -biR->sign;
    }

    r = 0;
    for(c=biR->num_comps-1; c>=0; c--) {
	r = r * bi_radix + biR->comps[c];
	biR->comps[c] = r / denom;
	r = r % denom;
    }

    normalize(biR);
    check(biR);
  
    return biR;
}



int bi_int_rem(bignum bi, int m) {
comp rad_r, r;
int c;

    check(bi);

    if (m == 0)
	g95_internal_error("bi_int_rem: divide by zero");

    if (m < 0)
	m = -m;

    rad_r = 1;
    r = 0;

    for(c=0; c<bi->num_comps; c++) {
	r = (r + bi->comps[c] * rad_r) % m;
	rad_r = (rad_r * bi_radix) % m;
    }

    if (bi->sign < 1)
	r = -r;

    big_free(bi);

    return (int) r;
}



bignum bi_add(bignum bia, bignum bib) {
bignum biR;
int c;

    check(bia);
    check(bib);

    biR = big_clone(bia);

    more_comps(biR, max(biR->num_comps, bib->num_comps));
    for(c=0; c<bib->num_comps; c++)
	if (biR->sign == bib->sign)
	    biR->comps[c] += bib->comps[c];
	else
	    biR->comps[c] -= bib->comps[c];


    big_free(bib);
    normalize(biR);

    check(biR);

    return biR;
}



bignum bi_subtract(bignum bia, bignum bib) {
bignum biR;
int c;

    check(bia);
    check(bib);

    biR = big_clone(bia);
    more_comps(biR, max(biR->num_comps, bib->num_comps));

    for(c=0; c<bib->num_comps; c++)
	if (biR->sign == bib->sign)
	    biR->comps[c] -= bib->comps[c];
	else
	    biR->comps[c] += bib->comps[c];

    big_free(bib);
    normalize(biR);

    check(biR);

    return biR;
}



/* Regular O(n^2) multiplication. */

static bignum regular_multiply(bignum bia, bignum bib) {
int new_comps, c1, c2;
bignum biR;

    check(bia);
    check(bib);

    biR = big_clone(bi_0);
    new_comps = bia->num_comps + bib->num_comps;
    more_comps(biR, new_comps);

    for(c1=0; c1<bia->num_comps; c1++) {
	for(c2=0; c2<bib->num_comps; c2++)
	    biR->comps[c1 + c2] += bia->comps[c1] * bib->comps[c2];

	/* Normalize after each inner loop to avoid overflowing any
	** components.  But be sure to reset biR's components count,
	** in case a previous normalization lowered it.	*/

	biR->num_comps = new_comps;
	normalize(biR);
    }

    check(biR);
    if (!bi_is_zero(big_copy(biR)))
	biR->sign = bia->sign * bib->sign;

    big_free(bia);
    big_free(bib);

    return biR;
}



/* Karatsuba multiplication.  This is supposedly O(n^1.59), better than
** regular multiplication for large n.  The define below sets the crossover
** point - below that we use regular multiplication, above it we
** use Karatsuba.  Note that Karatsuba is a recursive algorithm, so
** all Karatsuba calls involve regular multiplications as the base
** steps.
*/
#define KARATSUBA_THRESH 12

bignum bi_multiply(bignum bia, bignum bib) {
bignum bi_i, bi_j, bi_k, bi_l;
bignum bi_ik, bi_mid, bi_jl;
int n, c;

    check(bia);
    check(bib);

    if (min(bia->num_comps, bib->num_comps) < KARATSUBA_THRESH)
	return regular_multiply(bia, bib);

  /* The factors are large enough that Karatsuba multiplication
  ** is a win.  The basic idea here is you break each factor up
  ** into two parts, like so:
  **     i * r^n + j        k * r^n + l
  ** r is the radix we're representing numbers with, so this
  ** breaking up just means shuffling components around, no
  ** math required.  With regular multiplication the product
  ** would be:
  **     ik * r^(n*2) + ( il + jk ) * r^n + jl
  ** That's four sub-multiplies and one addition, not counting the
  ** radix-shifting.  With Karatsuba, you instead do:
  **     ik * r^(n*2) + ( (i+j)(k+l) - ik - jl ) * r^n  + jl
  ** This is only three sub-multiplies.  The number of adds
  ** (and subtracts) increases to four, but those run in linear time
  ** so they are cheap.  The sub-multiplies are accomplished by
  ** recursive calls, eventually reducing to regular multiplication.
  */

    n = (max(bia->num_comps, bib->num_comps) + 1) / 2;
    bi_i = alloc(n);
    bi_j = alloc(n);
    bi_k = alloc(n);
    bi_l = alloc(n);

    for(c=0; c<n; c++) {
	if (c + n < bia->num_comps)
	    bi_i->comps[c] = bia->comps[c + n];
	else
	    bi_i->comps[c] = 0;

	if (c < bia->num_comps)
	    bi_j->comps[c] = bia->comps[c];
	else
	    bi_j->comps[c] = 0;

	if (c + n < bib->num_comps)
	    bi_k->comps[c] = bib->comps[c + n];
	else
	    bi_k->comps[c] = 0;

	if (c < bib->num_comps)
	    bi_l->comps[c] = bib->comps[c];
	else
	    bi_l->comps[c] = 0;
    }

    bi_i->sign = bi_j->sign = bi_k->sign = bi_l->sign = 1;

    normalize(bi_i);
    normalize(bi_j);
    normalize(bi_k);
    normalize(bi_l);

    bi_ik = bi_multiply(big_copy(bi_i), big_copy(bi_k));
    bi_jl = bi_multiply(big_copy(bi_j), big_copy(bi_l));
    bi_mid = bi_subtract(
			 bi_subtract(
				     bi_multiply(bi_add(bi_i, bi_j),
						 bi_add(bi_k, bi_l)),
				     big_copy(bi_ik)),
			 big_copy(bi_jl));

    more_comps(bi_jl, max(bi_mid->num_comps + n, bi_ik->num_comps + n*2));

    for (c=0; c<bi_mid->num_comps; c++)
	bi_jl->comps[c + n] += bi_mid->comps[c];

    for(c=0; c<bi_ik->num_comps; c++)
	bi_jl->comps[c + n*2] += bi_ik->comps[c];

    big_free(bi_ik);
    big_free(bi_mid);
    bi_jl->sign = bia->sign * bib->sign;
    big_free(bia);
    big_free(bib);

    normalize(bi_jl);
    check(bi_jl);

    return bi_jl;
}



/* Divide two multi-precision positive conditioned numbers. */

static bignum multi_divide2(bignum binumer, bignum bidenom) {
bignum biapprox, birem, biquotient;
int c, o;

  /* Figure out the approximate quotient.   Since we're dividing by only
  ** the top component of the denominator, which is less than or equal to
  ** the full denominator, the result is guaranteed to be greater than or
  ** equal to the correct quotient.
  */

    o = bidenom->num_comps - 1;
    biapprox = bi_int_divide(big_copy(binumer), bidenom->comps[o]);
 
    /* And downshift the result to get the approximate quotient. */
 
    for(c=o; c<biapprox->num_comps; c++)
	biapprox->comps[c - o] = biapprox->comps[c];

    biapprox->num_comps -= o;

    /* Find the remainder from the approximate quotient. */
    birem = bi_subtract(bi_multiply(big_copy(biapprox), big_copy(bidenom)),
			binumer);

    /* If the remainder is negative, zero, or in fact any value less
    ** than bidenom, then we have the correct quotient and we're done. */

    if (bi_compare(big_copy(birem), big_copy(bidenom)) < 0) {
	biquotient = biapprox;
	big_free(birem);
	big_free(bidenom);
    } else {
	/* The real quotient is now biapprox - birem / bidenom.  We still
	** have to do a divide.  However, birem is smaller than binumer,
	** so the next divide will go faster.  We do the divide by
	** recursion.  Since this is tail-recursion or close to it, we
	** could probably re-arrange things and make it a non-recursive
	** loop, but the overhead of recursion is small and the bookkeeping
	** is simpler this way.
	**
	** Note that since the sub-divide uses the same denominator, it
	** doesn't have to adjust the values again - the high-order component
	** will still be good.
	*/

	biquotient = bi_subtract(biapprox, multi_divide2(birem, bidenom));
    }

    return biquotient;
}



/* Divide two multi-precision positive numbers. */

static bignum multi_divide(bignum binumer, bignum bidenom) {

    /* We use a successive approximation method that is kind of like a
    ** continued fraction.  The basic approximation is to do an int divide
    ** by the high-order component of the denominator.  Then we correct
    ** based on the remainder from that.
    **
    ** However, if the high-order component is too small, this doesn't
    ** work well.  In particular, if the high-order component is 1 it
    ** doesn't work at all.  Easily fixed, though - if the component
    ** is too small, increase it!
    */

    if (bidenom->comps[bidenom->num_comps-1] < bi_radix_sqrt) {
	/* We use the square root of the radix as the threshold here
	** because that's the largest value guaranteed to not make the
	** high-order component overflow and become too small again.
	**
	** We increase binumer along with bidenom to keep the end result
	** the same.
	*/

	binumer = bi_int_multiply(binumer, bi_radix_sqrt);
	bidenom = bi_int_multiply(bidenom, bi_radix_sqrt);
    }

    /* Now start the recursion. */
    return multi_divide2(binumer, bidenom);
}



/* The following three routines implement a multi-precision divide method
** that I haven't seen used anywhere else.  It is not quite as fast as
** the standard divide method, but it is a lot simpler.  In fact it's
** about as simple as the binary shift-and-subtract method, which goes
** about five times slower than this.
**
** The method assumes you already have multi-precision multiply and subtract
** routines, and also a multi-by-single precision divide routine.  The latter
** is used to generate approximations, which are then checked and corrected
** using the former.  The result converges to the correct value by about
** 16 bits per loop.

** AEV: This routine has problems and is currently not called.  For
** some divisors and dividents, the quotient is off by one for some
** reason.
*/

/* Public routine to divide two arbitrary numbers. */

bignum bi_bad_divide(bignum binumer, bignum bidenom) {
bignum biquotient;
int sign;

/* Check signs and trivial cases. */

    sign = 1;
    switch(bi_compare(big_copy(bidenom), bi_0)) {
    case 0:
	g95_internal_error("bi_divide: divide by zero");

    case -1:
	sign *= -1;
	bidenom = bi_negate(bidenom);
	break;
    }
  
    switch(bi_compare(big_copy(binumer), bi_0)) {
    case 0:
	big_free(binumer);
	big_free(bidenom);
	return bi_0;

    case -1:
	sign *= -1;
	binumer = bi_negate(binumer);
	break;
    }

    switch(bi_compare(big_copy(binumer), big_copy(bidenom))) {
    case -1:
	big_free(binumer);
	big_free(bidenom);
	return bi_0;

    case 0:
	big_free(binumer);
	big_free(bidenom);

	return (sign == 1)
	    ? bi_1
	    : bi_m1;
    }

    /* Is the denominator small enough to do an int divide? */

    if (bidenom->num_comps == 1) {
	/* Win! */
	biquotient = bi_int_divide(binumer, bidenom->comps[0]);
	big_free(bidenom);

    } else {
	/* No, we have to do a full multi-by-multi divide. */
	biquotient = multi_divide(binumer, bidenom);
    }

    if (sign == -1)
	biquotient = bi_negate(biquotient);

    return biquotient;
}



/* Binary division - about five times slower than the above. */

bignum bi_divide(bignum binumer, bignum bidenom) {
bignum biquotient;
int sign;

    /* Check signs and trivial cases. */
    sign = 1;

    switch (bi_compare(big_copy(bidenom), bi_0)) {
    case 0:
	g95_internal_error("bi_divide: divide by zero");

    case -1:
	sign *= -1;
	bidenom = bi_negate(bidenom);
	break;
    }

    switch(bi_compare(big_copy(binumer), bi_0)) {
    case 0:
	big_free(binumer);
	big_free(bidenom);
	return bi_0;

    case -1:
	sign *= -1;
	binumer = bi_negate(binumer);
	break;
    }

    switch(bi_compare(big_copy(binumer), big_copy(bidenom))) {
    case -1:
	big_free(binumer);
	big_free(bidenom);
	return bi_0;

    case 0:
	big_free(binumer);
	big_free(bidenom);

	return (sign == 1)
	    ? bi_1
	    : bi_m1;
    }

    /* Is the denominator small enough to do an int divide? */
    if (bidenom->num_comps == 1) {
	/* Win! */
	biquotient = bi_int_divide(binumer, bidenom->comps[0]);
	big_free(bidenom);
    } else {
	/* No, we have to do a full multi-by-multi divide. */
	int num_bits, den_bits, i;

	num_bits = bi_bits(big_copy(binumer));
	den_bits = bi_bits(big_copy(bidenom));
	bidenom = bi_multiply(bidenom,
			      bi_power(bi_2, int_to_bi(num_bits - den_bits)));

	biquotient = bi_0;
	for(i=den_bits; i<=num_bits; i++) {
	    biquotient = bi_double(biquotient);
	    if (bi_compare(big_copy(binumer), big_copy(bidenom)) >= 0) {
		biquotient = bi_int_add(biquotient, 1);
		binumer = bi_subtract(binumer, big_copy(bidenom));
	    }

	    bidenom = bi_half(bidenom);
	}

	big_free(binumer);
	big_free(bidenom);
    }

    if (sign == -1)
	biquotient = bi_negate(biquotient);

    return biquotient;
}



bignum bi_negate(bignum bi) {
bignum biR;

    check(bi);
    biR = big_clone(bi);

    if (!bi_is_zero(big_copy(biR)))
	biR->sign = -biR->sign;

    check(biR);
    return biR;
}



bignum bi_abs(bignum bi) {
bignum biR;

    check(bi);

    biR = big_clone(bi);
    biR->sign = 1;

    check(biR);
    return biR;
}



bignum bi_half(bignum bi) {
bignum biR;
int c;

    check(bi);
    /* This depends on the radix being even. */

    biR = big_clone(bi);
    for(c=0; c<biR->num_comps; c++) {
	if (biR->comps[c] & 1)
	    if (c > 0)
		biR->comps[c - 1] += bi_radix_o2;

	biR->comps[c] = biR->comps[c] >> 1;
    }

    /* Avoid normalization. */
    if (biR->num_comps > 1 && biR->comps[biR->num_comps-1] == 0)
	biR->num_comps--;

    check(biR);
    return biR;
}



bignum bi_double(bignum bi) {
bignum biR;
int c;

    check(bi);
    biR = big_clone(bi);

    for(c=biR->num_comps-1; c>=0; c--) {
	biR->comps[c] = biR->comps[c] << 1;

	if (biR->comps[c] >= bi_radix) {
	    if (c + 1 >= biR->num_comps)
		more_comps(biR, biR->num_comps + 1);

	    biR->comps[c] -= bi_radix;
	    biR->comps[c + 1] += 1;
	}
    }

    check(biR);
    return biR;
}



/* bi_2n()-- Compute 2^n for a nonnegative integer n. */

bignum bi_2n(int n) {
bignum r;
int i, m;

    m = (n / bi_comp_bits) + 1;
    r = alloc(m);

    for(i=0; i<m-1; i++)
	r->comps[i] = 0;

    r->comps[m-1] = 1 << (n % bi_comp_bits);
    r->sign = 1;

    return r;
}



/* Find integer square root by Newton's method. */

bignum bi_sqrt(bignum bi) {
bignum biR, biR2, bidiff;

    switch(bi_compare(big_copy(bi), bi_0)) {
    case -1:
	g95_internal_error("bi_sqrt: imaginary result");

    case 0:
	return bi;
    }
  
    if (bi_is_one(big_copy(bi)))
	return bi;

    /* Newton's method converges reasonably fast, but it helps to have
    ** a good initial guess.  We can make a *very* good initial guess
    ** by taking the square root of the top component times the square
    ** root of the radix part.  Both of those are easy to compute. */

    biR = bi_int_multiply(bi_power(int_to_bi(bi_radix_sqrt),
				   int_to_bi(bi->num_comps - 1)),
			  csqrt0(bi->comps[bi->num_comps - 1]));

    /* Now do the Newton loop until we have the answer. */

    for(;;) {
	biR2 = bi_divide(big_copy(bi), big_copy(biR));
	bidiff = bi_subtract(big_copy(biR), big_copy(biR2));
	if (bi_is_zero(big_copy(bidiff)) ||
	    bi_compare(big_copy(bidiff), bi_m1) == 0) {
	    big_free(bi);
	    big_free(bidiff);
	    big_free(biR2);
	    return biR;
	}

	if (bi_is_one(big_copy(bidiff))) {
	    big_free(bi);
	    big_free(bidiff);
	    big_free(biR);
	    return biR2;
	}

	big_free(bidiff);
	biR = bi_half(bi_add(biR, biR2));
    }
}



int bi_is_odd(bignum bi) {
int r;

    check(bi);
    r = bi->comps[0] & 1;

    big_free(bi);
    return r;
}



int bi_is_zero(bignum bi) {
int r;

    check(bi);
    r = (bi->sign == 1 && bi->num_comps == 1 && bi->comps[0] == 0);

    big_free(bi);
    return r;
}




int bi_is_one(bignum bi) {
int r;

    check(bi);
    r = (bi->sign == 1 && bi->num_comps == 1 && bi->comps[0] == 1);

    big_free(bi);
    return r;
}



int bi_is_negative(bignum bi) {
int r;

    check(bi);
    r = (bi->sign == -1);

    big_free(bi);
    return r;
}



int bi_bits(bignum bi) {
int bits;

    bits = bi_comp_bits*(bi->num_comps-1) + cbits(bi->comps[bi->num_comps-1]);
    big_free(bi);

    return bits;
}



#ifdef DUMP
/* Debug routine to dump out a complete bigint.  Does not consume bi. */

static void dump(char *str, bigint obi) {
real_bigint bi = (real_bigint) obi;
int c;

    (void) fprintf(stdout, "dump %s at 0x%08x:\n", str, (unsigned int) bi);
    (void) fprintf(stdout, "  refs: %d\n", bi->refs);
    (void) fprintf(stdout, "  next: 0x%08x\n", (unsigned int) bi->next);
    (void) fprintf(stdout, "  num_comps: %d\n", bi->num_comps);
    (void) fprintf(stdout, "  max_comps: %d\n", bi->max_comps);
    (void) fprintf(stdout, "  sign: %d\n", bi->sign);

    for(c=bi->num_comps-1; c>=0; c--)
	(void) fprintf(stdout, "    comps[%d]: %11lld (0x%016llx)\n", c,
		       (long long) bi->comps[c], (long long) bi->comps[c]);

    (void) fprintf(stdout, "  print: ");
    bi_print(stdout, big_copy(bi));

    (void) fprintf(stdout, "\n");
}
#endif /* DUMP */



/* Trivial square-root routine so that we don't have to link in the math lib.*/

static int csqrt0(comp c) {
comp r, r2, diff;

    if (c < 0)
	g95_internal_error("csqrt0: imaginary result");

    r = c / 2;

    for(;;) {
	r2 = c / r;
	diff = r - r2;

	if (diff == 0 || diff == -1)
	    return (int) r;

	if (diff == 1)
	    return (int) r2;

	r = (r + r2) / 2;
    }
}


/* Figure out how many bits are in a number. */

static int cbits(comp c) {
int b;

    for(b=0; c!=0; b++)
	c >>= 1;

    return b;
}



bignum bi_from_string(char *str, int base) {
int d, m, i, sign;
bignum biR;
comp t;

    sign = 1;
    if (*str == '-') {
	sign = -1;
	++str;
    }

    t = 1;
    while(t <= bi_radix)
	t = t * base;

    t = t / base;

    biR = bi_0;
    m = 1;
    i = 0;

    for(;;) {
	d = *str++;

	switch(base) {
	case 2:
	    if ('0' <= d && d <= '1')
		d = d - '0';
	    else
		d = -1;
	    break;

	case 8:
	    if ('0' <= d && d <= '7')
		d = d - '0';
	    else
		d = -1;
	    break;

	case 10:
	    if ('0' <= d && d <= '9')
		d = d - '0';
	    else
		d = -1;
	    break;

	case 16:
	    if ('0' <= d && d <= '9')
		d = d - '0';
	    else if ('a' <= d && d <= 'f')
		d = d - 'a' + 10;
	    else if ('A' <= d && d <= 'F')
		d = d - 'A' + 10;
	    else
		d = -1;

	    break;

	default:
	    d = -1;
	}

	if (d < 0 || m == t) {
	    biR = bi_int_add(bi_int_multiply(biR, m), i);
	    if (d < 0)
		break;

	    m = 1;
	    i = 0;
	}

	i = base*i + d;
	m = base*m;
    }

    if (sign == -1)
	biR = bi_negate(biR);

    return biR;
}



/* bi_to_string()-- Convert a bignum to a string.  Returns a pointer
 * to one of two alternating static buffers, so that it is possible to
 * print two numbers at once. */

char *bi_to_string(bignum b) {
static char b1[100], b2[100], *buffer = b1;
int n, i, cmp, sign;
char *p;

    buffer = (buffer == b1) ? b2 : b1;

    p = buffer + sizeof(b1) - 1;
    *p-- = '\0';

    cmp = bi_compare(big_copy(b), bi_0);
    if (cmp == 0) {
	*p = '0';
	big_free(b);
	return p;
    }

    sign = 0;
    if (cmp < 0) {
	b = bi_negate(b);
	sign = 1;
    }

    do {
	i = bi_int_rem(big_copy(b), 10000);
	b = bi_int_divide(b, 10000);

	cmp = bi_compare(big_copy(b), bi_0);

	for(n=0; n<4; n++) {
	    if (i == 0 && cmp == 0)
		break;

	    *p-- = (i % 10) + '0';
	    if (p < buffer)
		g95_internal_error("bi_to_string(): Number too large");

	    i = i / 10;
	}
    } while(cmp != 0);

    if (sign)
	*p-- = '-';

    big_free(b);
    return p+1;
}



static void print_pos(FILE *f, bignum bi) {

    if (bi_compare(big_copy(bi), bi_10) >= 0)
	print_pos(f, bi_int_divide(big_copy(bi), 10));

    putc(bi_int_mod(bi, 10) + '0', f);
}



void bi_print(FILE *f, bignum bi) {

    if (bi_is_negative(big_copy(bi))) {
	putc('-', f);
	bi = bi_negate(bi);
    }
  
    print_pos(f, bi);
}



int bi_int_mod(bignum bi, int m) {
int r;

    if (m <= 0)
	g95_internal_error("bi_int_mod: zero or negative modulus");

    r = bi_int_rem(bi, m);
    if (r < 0)
	r += m;

    return r;
}



bignum bi_rem(bignum bia, bignum bim) {
bignum r;

    r = bi_subtract(big_copy(bia),
		    bi_multiply(bi_divide(big_copy(bia), big_copy(bim)), bim));

    big_free(bia);
    return r;
}



bignum bi_mod(bignum bia, bignum bim) {
bignum biR;

    if (bi_compare(big_copy(bim), bi_0) <= 0)
	g95_internal_error("bi_mod: zero or negative modulus");

    biR = bi_rem(bia, big_copy(bim));
    if (bi_is_negative(big_copy(biR)))
	biR = bi_add(biR, bim);
    else
	big_free(bim);

    return biR;
}



bignum bi_square(bignum bi) {
bignum biR;

    biR = bi_multiply(big_copy(bi), big_copy(bi));
    big_free(bi);

    return biR;
}



bignum bi_power(bignum bi, bignum biexp) {
bignum biR;

    if (bi_is_negative(big_copy(biexp)))
	g95_internal_error("bi_power: negative exponent");

    biR = bi_1;
    for(;;) {
	if (bi_is_odd(big_copy(biexp)))
	    biR = bi_multiply(biR, big_copy(bi));

	biexp = bi_half(biexp);
	if (bi_compare(big_copy(biexp), bi_0) <= 0)
	    break;

	bi = bi_multiply(big_copy(bi), bi);
    }

    big_free(bi);
    big_free(biexp);
    return biR;
}



/* g95_bit_size()-- Given a (valid) integer kind, return the bit size. */

int g95_bit_size(int kind) {
g95_integer_info *info;

    info = g95_int_info(kind);
    return info->bit_size;
}




/* g95_int_info()-- Get the integer format from the kind */

g95_integer_info *g95_int_info(int kind) {
int i;

    i = g95_validate_kind(BT_INTEGER, kind);
    if (i < 0)
	g95_internal_error("g95_int_info(): Bad kind");

    return &g95_integer_kinds[i];
}



/* set_bit()-- Set a bit in memory. */

static void set_bit(char *mem, int kind, int bit, int value) {
int n, mask;

    n = bit >> 3;

    if (G95_INTEGER_ENDIAN == END_BIG)
	n = (g95_bit_size(kind) >> 3) - n - 1;

    mask = 1 << (bit & 0x07);

    if (value)
	mem[n] |= mask;
    else
	mem[n] &= ~mask;
}



/* read_bit()-- Read a bit from memory */

static int read_bit(char *mem, int kind, int bit) {
int n;

    n = bit >> 3;

    if (G95_INTEGER_ENDIAN == END_BIG)
	n = (g95_bit_size(kind) >> 3) - n - 1;

    return !!(mem[n] & (1 << (bit & 0x07)));
}



/* g95_unpack_int()-- Convert a target representation to internal */

bignum g95_unpack_int(char *mem, int kind) {
int i, sign, bit_size;
bignum b;

    bit_size = g95_bit_size(kind);
    sign = read_bit(mem, kind, bit_size-1);

    b = bi_0;

    for(i=bit_size-2; i>=0; i--)
	b = bi_int_add(bi_double(b), sign ^ read_bit(mem, kind, i));

    if (sign)
	b = bi_negate(bi_int_add(b, 1));

    return b;
}



int g95_unpack_logical(char *mem, int kind) {

    return !bi_is_zero(g95_unpack_int(mem, kind));
}



/* g95_pack_int()-- Convert a bignum to target representation */

void g95_pack_int(bignum b, int kind, char *buffer) {
int i, sign, bit_size;

    bit_size = g95_bit_size(kind);

    if (b->typeless) {
	for(i=0; i<bit_size; i++) {
	    set_bit(buffer, kind, i, bi_is_odd(big_copy(b)));
	    b = bi_half(b);
	}

    } else {
	sign = 0;
	if (bi_compare(big_copy(b), bi_0) < 0) {
	    b = bi_int_add(bi_negate(b), -1);
	    sign = 1;
	}

	set_bit(buffer, kind, bit_size-1, sign);

	for(i=0; i<bit_size-1; i++) {
	    set_bit(buffer, kind, i, sign ^ bi_is_odd(big_copy(b)));
	    b = bi_half(b);
	}
    }

    big_free(b);
}



void g95_pack_logical(int value, int kind, char *buffer) {

    g95_pack_int(int_to_bi(value), kind, buffer);
}



/* bi_getbit()-- Extract a bit from a bigint */

int bi_getbit(bignum b, int kind, int bit) {
char mem[32];

    g95_pack_int(b, kind, mem);
    return read_bit(mem, kind, bit);
}



bignum bi_setbit(bignum b, int kind, int bit, int value) {
char mem[32];

    g95_pack_int(b, kind, mem);
    set_bit(mem, kind, bit, value);
    return g95_unpack_int(mem, kind);
}



bignum bi_and(bignum a, bignum b, int kind) {
char mem1[32], mem2[32];
int i, bytes;

    g95_pack_int(a, kind, mem1); 
    g95_pack_int(b, kind, mem2);

    bytes = g95_bit_size(kind) / 8;

    for(i=0; i<bytes; i++)
	mem1[i] &= mem2[i];

    return g95_unpack_int(mem1, kind);
}



bignum bi_or(bignum a, bignum b, int kind) {
char mem1[32], mem2[32];
int i, bytes;

    g95_pack_int(a, kind, mem1); 
    g95_pack_int(b, kind, mem2);

    bytes = g95_bit_size(kind) / 8;

    for(i=0; i<bytes; i++)
	mem1[i] |= mem2[i];

    return g95_unpack_int(mem1, kind);
}



bignum bi_xor(bignum a, bignum b, int kind) {
char mem1[32], mem2[32];
int i, bytes;

    g95_pack_int(a, kind, mem1); 
    g95_pack_int(b, kind, mem2);

    bytes = g95_bit_size(kind) / 8;

    for(i=0; i<bytes; i++)
	mem1[i] ^= mem2[i];

    return g95_unpack_int(mem1, kind);
}



bignum bi_not(bignum a, int kind) {
char mem1[32];
int i, bytes;

    g95_pack_int(a, kind, mem1);

    bytes = g95_bit_size(kind) / 8;
    for(i=0; i<bytes; i++)
	mem1[i] = ~mem1[i];

    return g95_unpack_int(mem1, kind);
}



bignum bi_shift(bignum a, int kind, int size, int circular) {
char mem1[32], mem2[32];
int i, j, bits;

    g95_pack_int(a, kind, mem1);
    bits = g95_bit_size(kind);

    for(i=0; i<bits; i++) {
	j = i - size;

	if (circular == 0) {
	    j = (j < 0 || j >= bits)
		? 0 : read_bit(mem1, kind, j);
	} else {
	    if (i >= circular)
		j = i;
	    else {
		while(j < 0)
		    j += circular;

		j = j % circular;
	    }

	    j = read_bit(mem1, kind, j);
	}

	set_bit(mem2, kind, i, j);
    }

    return g95_unpack_int(mem2, kind);
}

