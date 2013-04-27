/* Intrinsic function resolution
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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


/* iresolve.c-- assign name and types to intrinsic procedures.  For
 * functions, the first argument to a resolution function is an
 * expression pointer to the original function node and the rest are
 * pointers to the arguments of the function call.  For subroutines,
 * a pointer to the code node is passed.
 *
 * The result type and library subroutine name are generally set
 * according to the function arguments. */

#include <string.h>
#include <stdarg.h>

#include "g95.h"
#include "intrinsic.h"


/* String pool subroutines.  This are used to provide static locations
 * for the string constants that represent library function names. */

typedef struct string_node {
    BBT_HEADER(string_node);
    char string[1];
} string_node;

static string_node *string_root = NULL;



/* compare_str() -- Compare two string nodes */

static int compare_str(string_node *a, string_node *b) {

    return strcmp(a->string, b->string);
}



/* g95_get_string()-- Intern a string in the string table, so that it
 * may be safely re-used. */

char *g95_get_string(char *name) {
string_node *p;
int h;

    /* Search */
    p = string_root;

    for(;;) {
	if (p == NULL)
	    break;

	h = strcmp(name, p->string);
	if (h == 0)
	    return p->string;

	p = (h < 0) ? p->left : p->right;
    }

    /* Add */

    p = g95_getmem(sizeof(string_node) + strlen(name));
    strcpy(p->string, name);
    g95_insert_bbt(&string_root, p, compare_str);

    return p->string;
}



/* g95_get_fstring()-- Given printf-like arguments, return a static
 * address of the resulting string.  If the name is not in the table,
 * it is added. */

char *g95_get_fstring(char *format, ...) {
char name[2*G95_MAX_SYMBOL_LEN];
va_list ap;

    va_start(ap, format); 
    vsprintf(name, format, ap);
    va_end(ap);

    return g95_get_string(name);
}



static void free_strings(string_node *p) {

    if (p == NULL)
	return;

    free_strings(p->left);
    free_strings(p->right);

    g95_free(p);
}



static g95_charlen *get_charlen(g95_expr *len) {
g95_charlen *cl;

    cl = g95_get_charlen(NULL);
    cl->length = len;

    return cl;
}


/* default_int()-- Make sure the kind of an argument is a default
 * integer.  If not, convert it to that. */

static void default_int(g95_expr *e) {
g95_typespec ts;
int kind;

    kind = g95_default_integer_kind(0);

    if (e == NULL || e->ts.kind == kind)
	return;

    g95_clear_ts(&ts);
    ts.type = BT_INTEGER;
    ts.kind = kind;

    g95_convert_type(e, &ts, 0);
}



/* default_logical()-- Make sure the kind of an argument is a default
 * logical.  If not, convert it to that. */

static void default_logical(g95_expr *e) {
g95_typespec ts;
int kind;

    kind = g95_default_logical_kind();

    if (e == NULL || e->ts.kind == kind)
	return;

    g95_clear_ts(&ts);
    ts.type = BT_LOGICAL;
    ts.kind = kind;

    g95_convert_type(e, &ts, 1);
}



/* charcat()-- Concatenate a character to a string */

static void charcat(char *string, char c) {
char *p;

    p = strchr(string, '\0');
    *p++ = c;
    *p = '\0';
}



void g95_resolve_abs(g95_expr *f, g95_expr *a) {

    f->ts = a->ts;

    if (f->ts.type == BT_COMPLEX)
	f->ts.type = BT_REAL;

    f->value.function.iname =
	g95_get_fstring(PREFIX "abs_%c%d", g95_type_letter(a->ts.type),
			a->ts.kind);
}



void g95_resolve_achar(g95_expr *f, g95_expr *i) {

    i = i;
    f->ts.type = BT_CHARACTER;
    f->ts.kind = g95_default_character_kind();
    f->ts.cl = get_charlen(g95_int_expr(1));

    f->value.function.iname = g95_get_fstring(PREFIX "char_%d", f->ts.kind);
}



void g95_resolve_acos(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "acos_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_adjust(g95_expr *f, g95_expr *a) {

    f->ts = a->ts;
}



void g95_resolve_aimag(g95_expr *f, g95_expr *x) {

    f->ts.type = BT_REAL;
    f->ts.kind = x->ts.kind;
    f->value.function.iname = g95_get_fstring(PREFIX "aimag_%d", x->ts.kind);
}



void g95_resolve_aint(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = a->ts.type;

    f->ts.kind = (kind == NULL)
	? a->ts.kind
	: bi_to_int(kind->value.integer);

    f->value.function.iname =
	g95_get_fstring(PREFIX "aint%d_%d", f->ts.kind, a->ts.kind);
}



void g95_resolve_all(g95_expr *f, g95_expr *mask, g95_expr *dim) {
char suffix[10];

    f->ts = mask->ts;
    suffix[0] = '\0';

    if (dim == NULL || mask->rank == 1)
	f->rank = 0;

    else {
	f->rank = mask->rank - 1;
	charcat(suffix, 'd');
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "all%s_%d", suffix, mask->ts.kind);

    default_int(dim);
}



void g95_resolve_anint(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = a->ts.type;

    f->ts.kind = (kind == NULL)
	? a->ts.kind
	: bi_to_int(kind->value.integer);

    f->value.function.iname =
	g95_get_fstring(PREFIX "anint%d_%d", f->ts.kind, a->ts.kind);
}



void g95_resolve_any(g95_expr *f, g95_expr *mask, g95_expr *dim) {
char suffix[10];

    f->ts = mask->ts;
    suffix[0] = '\0';

    if (dim == NULL || mask->rank == 1)
	f->rank = 0;

    else {
	f->rank = mask->rank - 1;
	charcat(suffix, 'd');
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "any%s_%d", suffix, mask->ts.kind);

    default_int(dim);
}



void g95_resolve_asin(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "asin_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_atan(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "atan_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_atan2(g95_expr *f, g95_expr *x, g95_expr *y) {

    y = y;
    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "atan2_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_btest(g95_expr *f, g95_expr *i, g95_expr *pos) {

    f->ts.type = BT_LOGICAL;
    f->ts.kind = g95_default_logical_kind();

    f->value.function.iname =
	g95_get_fstring(PREFIX "btest_%d_%d", i->ts.kind, pos->ts.kind);
}



void g95_resolve_ceiling(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = (kind == NULL)
	? g95_default_integer_kind(1)
	: bi_to_int(kind->value.integer);

    f->value.function.iname =
	g95_get_fstring(PREFIX "ceiling_%d_r%d", f->ts.kind, a->ts.kind);
}



void g95_resolve_char(g95_expr *f, g95_expr *i, g95_expr *kind) {

    i = i;
    f->ts.type = BT_CHARACTER;
    f->ts.kind = (kind == NULL)
	? g95_default_character_kind()
	: bi_to_int(kind->value.integer);

    f->ts.cl = get_charlen(g95_int_expr(1));

    f->value.function.iname = g95_get_fstring(PREFIX "char_%d", f->ts.kind);
}



void g95_resolve_cmplx(g95_expr *f, g95_expr *x, g95_expr *y, g95_expr *kind) {

    x = x;
    y = y;

    f->ts.type = BT_COMPLEX;
    f->ts.kind = (kind == NULL)
	? g95_default_real_kind(1)
	: bi_to_int(kind->value.integer);

    f->value.function.iname = g95_get_string(PREFIX "cmplx");
}



void g95_resolve_co_lbound(g95_expr *f, g95_expr *coarray, g95_expr *dim,
			   g95_expr *kind) {
int k;

    if (kind == NULL)
	k = g95_default_integer_kind(0);

    else {
	g95_extract_int(kind, &k);
	default_int(kind);
    }

    f->ts.type = BT_INTEGER;
    f->ts.kind = k;

    if (dim != NULL)
	default_int(dim);

    if (dim != NULL)
	f->value.function.iname = g95_get_fstring(PREFIX "co_lbound_%d", k);

    else {
	f->value.function.iname = PREFIX "co_lbound_v";
	f->rank = 1;

	f->shape = g95_get_shape(1);
	f->shape[0] = int_to_bi(coarray->symbol->cas->corank);
	big_permanent(f->shape[0]);
    }
}



void g95_resolve_co_ubound(g95_expr *f, g95_expr *coarray, g95_expr *dim,
			   g95_expr *kind) {
int k;

    if (kind == NULL)
	k = g95_default_integer_kind(0);

    else {
	g95_extract_int(kind, &k);
	default_int(kind);
    }

    f->ts.type = BT_INTEGER;
    f->ts.kind = k;

    if (dim != NULL)
	default_int(dim);

    if (dim != NULL)
	f->value.function.iname = g95_get_fstring(PREFIX "co_ubound_%d", k);

    else {
	f->value.function.iname = PREFIX "co_ubound_v";
	f->rank = 1;

	f->shape = g95_get_shape(1);
	f->shape[0] = int_to_bi(coarray->symbol->cas->corank);
	big_permanent(f->shape[0]);
    }
}



void g95_resolve_conjg(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "conjg_%d", x->ts.kind);
}



void g95_resolve_cos(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "cos_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_cosh(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "cosh_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_count(g95_expr *f, g95_expr *mask, g95_expr *dim) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);

    if (dim == NULL || mask->rank == 1)
	f->value.function.iname =
	    g95_get_fstring(PREFIX "count_%d", mask->ts.kind);

    else {
	f->rank = mask->rank - 1;
	f->value.function.iname =
	    g95_get_fstring(PREFIX "countd_%d", mask->ts.kind);
    }

    default_int(dim);
}



void g95_resolve_cshift(g95_expr *f, g95_expr *array, g95_expr *shift,
			g95_expr *dim) {

    f->ts = array->ts;
    f->rank = array->rank;

    if (shift->rank == 0)
	default_int(shift);

    f->value.function.iname =
	g95_get_fstring(PREFIX "cshift%d", (shift->rank == 0) ? 1 : 2);

    default_int(dim);
}



void g95_resolve_dble(g95_expr *f, g95_expr *a) {

    f->ts.type = BT_REAL;
    f->ts.kind = g95_default_double_kind();
    f->value.function.iname =
	g95_get_fstring(PREFIX "dble_%c%d", g95_type_letter(a->ts.type),
			a->ts.kind);
}



void g95_resolve_dim(g95_expr *f, g95_expr *x, g95_expr *y) {

    y = y;
    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "dim_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_dot_product(g95_expr *f, g95_expr *a, g95_expr *b) {

    if (a->ts.type != BT_LOGICAL || b->ts.type != BT_LOGICAL)
	g95_binary_result_type(&a->ts, &b->ts, &f->ts);

    else {
	f->ts.type = BT_LOGICAL;
	f->ts.kind = g95_default_logical_kind();
    }

    f->rank = 0;
    f->value.function.iname =
	g95_get_fstring(PREFIX "dot_product_%c%d_%c%d",
			g95_type_letter(a->ts.type), a->ts.kind,
			g95_type_letter(b->ts.type), b->ts.kind);
}



void g95_resolve_dtime(g95_expr *f, g95_expr *t) {

    f->ts.type = BT_REAL;
    f->ts.kind = g95_default_real_kind(0);

    f->value.function.iname = PREFIX "dtime_f";
}



void g95_resolve_eoshift(g95_expr *f, g95_expr *array, g95_expr *shift,
      		                   g95_expr *boundary, g95_expr *dim) {

    f->ts = array->ts;
    f->rank = array->rank;

    if (shift->rank == 0)
	default_int(shift);

    if (boundary == NULL || boundary->rank == 0)
	f->value.function.iname =
	    g95_get_fstring(PREFIX "eoshift%d_%c%d",
			    (shift->rank == 0) ? 1 : 2,
			    g95_type_letter(array->ts.type), array->ts.kind);
    else
	f->value.function.iname =
	    g95_get_fstring(PREFIX "eoshift%d", (shift->rank == 0) ? 3 : 4);

    default_int(dim);
}



void g95_resolve_erf(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "erf_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_erfc(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "erfc_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_etime(g95_expr *f, g95_expr *t) {

    f->ts.type = BT_REAL;
    f->ts.kind = g95_default_real_kind(0);

    f->value.function.iname = PREFIX "etime_f";
}



void g95_resolve_exit(g95_code *code) {
g95_expr *arg;
char *name;

    arg = code->ext.sub.actual->u.expr;

    name = (arg == NULL)
	? PREFIX "exit"
	: g95_get_fstring(PREFIX "exit_%d", arg->ts.kind);

    code->ext.sub.sub_name = name;
}



void g95_resolve_exp(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "exp_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_exponent(g95_expr *f, g95_expr *x) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);

    f->value.function.iname =
	g95_get_fstring(PREFIX "exponent_%d", x->ts.kind);
}



void g95_resolve_floor(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = (kind == NULL)
	? g95_default_integer_kind(1)
	: bi_to_int(kind->value.integer);

    f->value.function.iname =
	g95_get_fstring(PREFIX "floor_%d_r%d", f->ts.kind, a->ts.kind);
}



void g95_resolve_fraction(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "fraction_%d", x->ts.kind);
}



void g95_resolve_getarg(g95_code *c) {
int kind;

    kind = c->ext.sub.actual->u.expr->ts.kind;
    c->ext.sub.sub_name = g95_get_fstring(PREFIX "getarg_%d", kind);
}



void g95_resolve_gamma(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "gamma_%d", x->ts.kind);
}



void g95_resolve_iand(g95_expr *f, g95_expr *i, g95_expr *j) {

    j = j;
    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "iand_%d", i->ts.kind);
}



void g95_resolve_ibclr(g95_expr *f, g95_expr *i, g95_expr *pos) {

    pos = pos;
    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "ibclr_%d", i->ts.kind);
}



void g95_resolve_ibits(g95_expr *f, g95_expr *i, g95_expr *pos,
		       g95_expr *len) {

    pos = pos;
    len = len;
    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "ibits_%d", i->ts.kind);
}



void g95_resolve_ibset(g95_expr *f, g95_expr *i, g95_expr *pos) {

    pos = pos;
    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "ibset_%d", i->ts.kind);
}



void g95_resolve_ichar(g95_expr *f, g95_expr *c) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);

    f->value.function.iname = g95_get_fstring(PREFIX "ichar_%d", c->ts.kind);
}



void g95_resolve_idnint(g95_expr *f, g95_expr *a) {
    g95_resolve_nint(f, a, NULL);
}



void g95_resolve_ieor(g95_expr *f, g95_expr *i, g95_expr *j) {

    j = j;
    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "ieor_%d", i->ts.kind);
}



void g95_resolve_ior(g95_expr *f, g95_expr *i, g95_expr *j) {

    j = j;
    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "ior_%d", i->ts.kind);
}



void g95_resolve_int(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = (kind == NULL)
	? g95_default_integer_kind(1)
	: bi_to_int(kind->value.integer);

    f->value.function.iname =
	g95_get_fstring(PREFIX "int_%d_%c%d", f->ts.kind,
			g95_type_letter(a->ts.type), a->ts.kind);
}



void g95_resolve_ishft(g95_expr *f, g95_expr *i, g95_expr *shift) {

    f->ts = i->ts;

    default_int(shift);

    f->value.function.iname = g95_get_fstring(PREFIX "ishft");
}



void g95_resolve_ishftc(g95_expr *f, g95_expr *i, g95_expr *shift,
			g95_expr *size) {
int s_kind;

    s_kind = (size == NULL)
	? g95_default_integer_kind(1)
	: shift->ts.kind;

    default_int(shift);
    if (size != NULL)
	default_int(size);

    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "ishftc");
}



void g95_resolve_isnan(g95_expr *f, g95_expr *x) {

    f->ts.type = BT_LOGICAL;
    f->ts.kind = g95_default_logical_kind();

    f->value.function.iname = g95_get_fstring(PREFIX "isnan_%d", x->ts.kind);
}



void g95_resolve_lbound(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *kind) {
int k;

    array = array;

    if (kind == NULL)
	k = g95_default_integer_kind(0);

    else
	g95_extract_int(kind, &k);

    f->ts.type = BT_INTEGER;
    f->ts.kind = k;
    f->value.function.iname = g95_get_fstring(PREFIX "lbound_%d", k);

    if (dim != NULL)
	default_int(dim);

    else {
	f->rank = 1;
	f->shape = g95_get_shape(1);
	f->shape[0] = int_to_bi(array->rank);
	big_permanent(f->shape[0]);
    }
}



void g95_resolve_lgamma(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "log_gamma_%d", x->ts.kind);
}



void g95_resolve_len(g95_expr *f, g95_expr *string) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);
    f->value.function.iname =
	g95_get_fstring(PREFIX "len_%d", string->ts.kind);
}



void g95_resolve_len_trim(g95_expr *f, g95_expr *string) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);
    f->value.function.iname =
	g95_get_fstring(PREFIX "len_trim_%d", string->ts.kind);
}



void g95_resolve_log(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "log_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_log10(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "log10_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_matmul(g95_expr *f, g95_expr *a, g95_expr *b) {

    if (a->ts.type != BT_LOGICAL || b->ts.type != BT_LOGICAL)
	g95_binary_result_type(&a->ts, &b->ts, &f->ts);

    else {
	f->ts.type = BT_LOGICAL;
	f->ts.kind = (a->ts.kind > b->ts.kind) ? a->ts.kind : b->ts.kind;
    }

    f->rank = (a->rank == 2 && b->rank == 2) ? 2 : 1;

    f->value.function.iname =
	g95_get_fstring(PREFIX "matmul%d%d_%c%d%c%d", a->rank, b->rank,
			g95_type_letter(a->ts.type), a->ts.kind,
			g95_type_letter(b->ts.type), b->ts.kind);
}



void g95_resolve_logical(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = BT_LOGICAL;
    f->ts.kind = (kind == NULL)
	? g95_default_logical_kind()
	: bi_to_int(kind->value.integer);

    f->rank = a->rank;

    f->value.function.iname =
	g95_get_fstring(PREFIX "logical_%d_%c%d", f->ts.kind,
			g95_type_letter(a->ts.type), a->ts.kind);
}



void g95_resolve_nearest(g95_expr *f, g95_expr *x, g95_expr *s) {

    f->ts = x->ts;
    f->rank = x->rank;

    f->value.function.iname =
	g95_get_fstring(PREFIX "nearest_%d_%d", x->ts.kind, s->ts.kind);
}



void g95_resolve_maxloc(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask, g95_expr *mask_s) {
char *suffix;
int i, j, k;

    mask = mask;

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);

    if (dim == NULL) {
	f->rank = 1;
	suffix = "";

    } else {
	f->rank = array->rank - 1;
	suffix = (array->rank == 1) ? "1" : "d";
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "maxloc%s_%c%d", suffix,
			g95_type_letter(array->ts.type), array->ts.kind);

    if (f->rank > 0) {
	if (dim == NULL) {
	    f->shape = g95_get_shape(1);
	    f->shape[0] = int_to_bi(array->rank);
	    big_permanent(f->shape[0]);

	} else if (array->shape != NULL && dim->type == EXPR_CONSTANT) {
	    i = bi_to_int(dim->value.integer) - 1;
	    f->shape = g95_get_shape(array->rank-1);

	    for(j=0, k=0; j<array->rank; j++)
		if (j != i) {
		    f->shape[k] = big_clone(array->shape[j]);
		    big_permanent(f->shape[k]);
		}
	}
    }

    default_int(dim);
    default_logical(mask_s);
}



void g95_resolve_maxval(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask) {
char suffix[10];

    f->ts = array->ts;
    suffix[0] = '\0';

    if (mask != NULL && mask->rank == 0) {
	charcat(suffix, '1');
	default_logical(mask);
    }

    if (dim == NULL || array->rank == 1)
	f->rank = 0;

    else {
	f->rank = array->rank - 1;
	charcat(suffix, 'd');
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "maxval%s_%c%d", suffix,
			g95_type_letter(array->ts.type), array->ts.kind);

    default_int(dim);
}



void g95_resolve_merge(g95_expr *f, g95_expr *tsource, g95_expr *fsource,
		       g95_expr *mask) {

    mask = mask;
    fsource = fsource;

    f->ts = tsource->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "merge_%c%d", g95_type_letter(tsource->ts.type),
			tsource->ts.kind);
}



void g95_resolve_min_max(g95_expr *f, g95_intrinsic_sym *s, g95_expr *a1) {

    f->ts = a1->ts;
    f->value.function.iname = s->name;

    if (s->ts.type != BT_UNKNOWN &&
	(s->ts.type != a1->ts.type || s->ts.kind != a1->ts.kind))
	f->ts = s->ts;
}



void g95_resolve_minloc(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask, g95_expr *mask_s) {
char *suffix;
int i, j, k;

    mask = mask;

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);

    if (dim == NULL) {
	f->rank = 1;
	suffix = "";

    } else {
	f->rank = array->rank - 1;
	suffix = (array->rank == 1) ? "1" : "d";
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "minloc%s_%c%d", suffix,
			g95_type_letter(array->ts.type), array->ts.kind);

    if (f->rank > 0) {
	if (dim == NULL) {
	    f->shape = g95_get_shape(1);
	    f->shape[0] = int_to_bi(array->rank);
	    big_permanent(f->shape[0]);

	} else if (array->shape != NULL && dim->type == EXPR_CONSTANT) {
	    i = bi_to_int(dim->value.integer) - 1;
	    f->shape = g95_get_shape(array->rank-1);

	    for(j=0, k=0; j<array->rank; j++)
		if (j != i) {
		    f->shape[k] = big_clone(array->shape[j]);
		    big_permanent(f->shape[k++]);
		}
	}
    }

    default_int(dim);
    default_logical(mask_s);
}



void g95_resolve_minval(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask) {
char suffix[10];

    f->ts = array->ts;
    suffix[0] = '\0';

    if (mask != NULL && mask->rank == 0) {
	charcat(suffix, '1');
	default_logical(mask);
    }

    if (dim == NULL || array->rank == 1)
	f->rank = 0;

    else {
	f->rank = array->rank - 1;
	charcat(suffix, 'd');
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "minval%s_%c%d", suffix,
			g95_type_letter(array->ts.type), array->ts.kind);

    default_int(dim);
}



void g95_resolve_mod(g95_expr *f, g95_expr *a, g95_expr *p) {

    p = p;
    f->ts = a->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "mod_%c%d", g95_type_letter(a->ts.type),
			a->ts.kind);
}



void g95_resolve_modulo(g95_expr *f, g95_expr *a, g95_expr *p) {

    p = p;
    f->ts = a->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "modulo_%c%d", g95_type_letter(a->ts.type),
			a->ts.kind);
}



void g95_resolve_nint(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = BT_INTEGER;
    f->ts.kind = (kind == NULL)
	? g95_default_integer_kind(1)
	: bi_to_int(kind->value.integer);

    f->value.function.iname =
	g95_get_fstring(PREFIX "nint_%d_r%d", f->ts.kind, a->ts.kind);
}



void g95_resolve_not(g95_expr *f, g95_expr *i) {

    f->ts = i->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "not_%d", i->ts.kind);
}



void g95_resolve_pack(g95_expr *f, g95_expr *array, g95_expr *mask,
		      g95_expr *vector) {

    vector = vector;
    f->ts = array->ts;
    f->rank = 1;

    f->value.function.iname =
	g95_get_fstring(mask->rank != 0 ? PREFIX "pack" : PREFIX "pack_s");
}



void g95_resolve_product(g95_expr *f, g95_expr *array, g95_expr *dim,
			 g95_expr *mask) {
char suffix[10];

    f->ts = array->ts;
    suffix[0] = '\0';

    if (mask != NULL && mask->rank == 0) {
	charcat(suffix, '1');
	default_logical(mask);
    }

    if (dim == NULL || array->rank == 1)
	f->rank = 0;

    else {
	f->rank = array->rank - 1;
	charcat(suffix, 'd');
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "product%s_%c%d", suffix,
			g95_type_letter(array->ts.type), array->ts.kind);

    default_int(dim);
}



void g95_resolve_real(g95_expr *f, g95_expr *a, g95_expr *kind) {

    f->ts.type = BT_REAL;

    if (kind != NULL)
	f->ts.kind = bi_to_int(kind->value.integer);

    else
	f->ts.kind = (a->ts.type == BT_COMPLEX) ?
	    a->ts.kind : g95_default_real_kind(1);

    f->value.function.iname =
	g95_get_fstring(PREFIX "real_%d_%c%d", f->ts.kind,
			g95_type_letter(a->ts.type), a->ts.kind);
}



void g95_resolve_repeat(g95_expr *f, g95_expr *string, g95_expr *ncopies) {

    ncopies = ncopies;
    f->ts.type = BT_CHARACTER;
    f->ts.kind = string->ts.kind;
    f->ts.cl = &g95_unknown_charlen;
    f->value.function.iname =
	g95_get_fstring(PREFIX "repeat_%d", string->ts.kind);
}



void g95_resolve_reshape(g95_expr *f, g95_expr *source, g95_expr *shape,
                         g95_expr *pad, g95_expr *order) {
    pad = pad; 
    order = order;
    f->ts = source->ts;
    f->rank = bi_to_int(g95_array_size(shape));
    f->value.function.iname = g95_get_string(PREFIX "reshape");
}



void g95_resolve_rrspacing(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "rrspacing_%d", x->ts.kind);
}



void g95_resolve_scale(g95_expr *f, g95_expr *x, g95_expr *i) {

    default_int(i);

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "scale_%d", x->ts.kind);
}



void g95_resolve_scan(g95_expr *f, g95_expr *string, g95_expr *set,
		      g95_expr *back) {

    set = set;
    back = back;

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);
    f->value.function.iname =
	g95_get_fstring(PREFIX "scan_%d", string->ts.kind);
}



void g95_resolve_set_exponent(g95_expr *f, g95_expr *x, g95_expr *i) {

    default_int(i);

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "set_exponent_%d", x->ts.kind);
}



void g95_resolve_shape(g95_expr *f, g95_expr *array, g95_expr *kind) {
int k;

    array = array;

    if (kind == NULL)
	k = g95_default_integer_kind(0);

    else
	g95_extract_int(kind, &k);

    f->ts.type = BT_INTEGER;
    f->ts.kind = k;

    f->rank = 1;
    f->value.function.iname = g95_get_fstring(PREFIX "shape_%d", k);

    f->shape = g95_get_shape(1);
    f->shape[0] = int_to_bi(array->rank);
    big_permanent(f->shape[0]);
}



void g95_resolve_sign(g95_expr *f, g95_expr *a, g95_expr *b) {

    b = b;
    f->ts = a->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "sign_%c%d", g95_type_letter(a->ts.type),
			a->ts.kind);
}



void g95_resolve_sin(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "sin_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_sinh(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "sinh_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}


void g95_resolve_size(g95_expr *f, g95_expr *array, g95_expr *dim,
		      g95_expr *kind) {
int k;

    if (kind == NULL)
	k = g95_default_integer_kind(0);

    else {
	g95_extract_int(kind, &k);
	default_int(kind);
    }

    f->ts.type = BT_INTEGER;
    f->ts.kind = k;
    f->value.function.iname = g95_get_fstring(PREFIX "size_%d", k);

    default_int(dim);
}



void g95_resolve_spacing(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "spacing_%d", x->ts.kind);
}



void g95_resolve_spread(g95_expr *f, g95_expr *source, g95_expr *dim,
			g95_expr *ncopies) {
int i, j, d;

    dim = dim;
    ncopies = ncopies;

    f->ts = source->ts;

    if (source->shape != NULL && dim->type == EXPR_CONSTANT &&
	ncopies->type == EXPR_CONSTANT) {
	f->shape = g95_get_shape(source->rank+1);

	d = bi_to_int(dim->value.integer) - 1;

	for(i=0, j=0; i<source->rank; i++) {
	    if (i == d) {
		f->shape[j] = big_clone(ncopies->value.integer);
		big_permanent(f->shape[j++]);
	    }

	    f->shape[j] = big_clone(source->shape[i]);
	    big_permanent(f->shape[j++]);
	}

	if (i == d) {
	    f->shape[j] = big_clone(ncopies->value.integer);
	    big_permanent(f->shape[j++]);
	}
    }

    f->rank = source->rank + 1;
    f->value.function.iname =
	g95_get_fstring(source->rank > 0 ? PREFIX "spread" : PREFIX"spread_s");
}



void g95_resolve_sqrt(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "sqrt_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_sum(g95_expr *f, g95_expr *array, g95_expr *dim,
		     g95_expr *mask) {
char suffix[10];

    f->ts = array->ts;
    suffix[0] = '\0';

    if (mask != NULL && mask->rank == 0) {
	charcat(suffix, '1');
	default_logical(mask);
    }

    if (dim == NULL || array->rank == 1)
	f->rank = 0;

    else {
	f->rank = array->rank - 1;
	charcat(suffix, 'd');
    }

    f->value.function.iname =
	g95_get_fstring(PREFIX "sum%s_%c%d", suffix,
			g95_type_letter(array->ts.type), array->ts.kind);

    default_int(dim);
}



void g95_resolve_tan(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "tan_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_tanh(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname =
	g95_get_fstring(PREFIX "tanh_%c%d", g95_type_letter(x->ts.type),
			x->ts.kind);
}



void g95_resolve_transfer(g95_expr *f, g95_expr *source, g95_expr *mold,
			  g95_expr *size) {

    source = source;
    f->ts = mold->ts;

    if (size != NULL || mold->rank != 0)
	f->rank = 1;

    if (size != NULL && size->type == EXPR_CONSTANT) {
	f->shape = g95_get_shape(1);
	f->shape[0] = big_clone(size->value.integer);
	big_permanent(f->shape[0]);
    }

    f->value.function.iname = PREFIX "transfer";
}



void g95_resolve_transpose(g95_expr *f, g95_expr *matrix) {

    f->ts = matrix->ts;
    f->rank = 2;

    if (matrix->shape != NULL) {
	f->shape = g95_get_shape(2);

	f->shape[0] = big_clone(matrix->shape[1]);
	big_permanent(f->shape[0]);

	f->shape[1] = big_clone(matrix->shape[0]);
	big_permanent(f->shape[1]);
    }

    f->value.function.iname = g95_get_string(PREFIX "transpose");
}



void g95_resolve_trim(g95_expr *f, g95_expr *string) {

    f->ts.type = BT_CHARACTER;
    f->ts.kind = string->ts.kind;
    f->ts.cl = &g95_unknown_charlen;
    f->value.function.iname =
	g95_get_fstring(PREFIX "trim_%d", string->ts.kind);
}



void g95_resolve_ubound(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *kind) {
int k;

    array = array; 

    if (kind == NULL)
	k = g95_default_integer_kind(0);

    else {
	g95_extract_int(kind, &k);
	default_int(kind);
    }

    f->ts.type = BT_INTEGER;
    f->ts.kind = k;
    f->value.function.iname = g95_get_fstring(PREFIX "ubound_%d", k);

    if (dim != NULL)
	default_int(dim);

    else {
	f->rank = 1;
	f->shape = g95_get_shape(1);
	f->shape[0] = int_to_bi(array->rank);
	big_permanent(f->shape[0]);
    }
}



void g95_resolve_unpack(g95_expr *f, g95_expr *vector, g95_expr *mask,
			g95_expr *field) {

    f->ts = vector->ts;
    f->rank = mask->rank;
    f->value.function.iname =
	g95_get_fstring(field->rank != 0 ? PREFIX "unpack" : PREFIX"unpack_s");
}



void g95_resolve_verify(g95_expr *f, g95_expr *string, g95_expr *set, 
			g95_expr *back) {

    set = set;
    back = back;

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);
    f->value.function.iname =
	g95_get_fstring(PREFIX "verify_%d", string->ts.kind);
}



void g95_resolve_cpu_time(g95_code *c) {

    c->ext.sub.sub_name =
	g95_get_fstring(PREFIX "cpu_time_%d",
			c->ext.sub.actual->u.expr->ts.kind);
}


void g95_resolve_random_number(g95_code *c) {
int kind;

    kind = c->ext.sub.actual->u.expr->ts.kind;
    c->ext.sub.sub_name = g95_get_fstring(PREFIX "random_%d", kind);
}


void g95_resolve_besj0(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "besj0_%d", x->ts.kind);
}


void g95_resolve_besj1(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "besj1_%d", x->ts.kind);
}


void g95_resolve_besjn(g95_expr *f, g95_expr *n, g95_expr *x) {

    f->ts = x->ts;
    default_int(n);
    f->value.function.iname = g95_get_fstring(PREFIX "besjn_%d", x->ts.kind);
}


void g95_resolve_besy0(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "besy0_%d", x->ts.kind);
}


void g95_resolve_besy1(g95_expr *f, g95_expr *x) {

    f->ts = x->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "besy1_%d", x->ts.kind);
}


void g95_resolve_besyn(g95_expr *f, g95_expr *n, g95_expr *x) {

    f->ts = x->ts;
    default_int(n);
    f->value.function.iname = g95_get_fstring(PREFIX "besyn_%d", x->ts.kind);
}


void g95_resolve_is_iostat_end(g95_expr *f, g95_expr *i) {

    f->ts.type = BT_LOGICAL;
    f->ts.kind = g95_default_logical_kind();

    f->value.function.iname =
	g95_get_fstring(PREFIX "is_iostat_end_%d", i->ts.kind);
}


void g95_resolve_is_iostat_eor(g95_expr *f, g95_expr *i) {

    f->ts.type = BT_LOGICAL;
    f->ts.kind = g95_default_logical_kind();

    f->value.function.iname =
	g95_get_fstring(PREFIX "is_iostat_eor_%d", i->ts.kind);
}


void g95_resolve_secnds(g95_expr *f, g95_expr *t) {

    f->ts = t->ts;
    f->value.function.iname = g95_get_fstring(PREFIX "secnds_%d", t->ts.kind);
}


void g95_resolve_this_image(g95_expr *f, g95_expr *coarray, g95_expr *dim) {
int corank;

    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);

    if (coarray == NULL) {
	f->value.function.iname = g95_get_fstring(PREFIX "this_image");
	return;
    }

    if (dim != NULL) {
	f->value.function.iname = g95_get_fstring(PREFIX "this_image1");
	return;
    }

    f->value.function.iname = g95_get_fstring(PREFIX "this_image_v");
    f->rank = 1;

    corank = g95_expr_corank(coarray);

    if (corank != 0) {
	f->shape = g95_get_shape(1);
	f->shape[0] = int_to_bi(corank);
	big_permanent(f->shape[0]);
    }
}



void g95_iresolve_init_1(void) {

}



void g95_iresolve_done_1(void) {

    free_strings(string_root);
}

