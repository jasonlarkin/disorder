/* Set up intrinsic functions
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


/* intrinsic.c-- Build up a list of intrinsic subroutines and
 * functions for the name-resolution stage. */

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#include "g95.h"
#include "intrinsic.h"


int g95_intrinsic_extension;
simp_t g95_simplify_mode = SIMP_REGULAR;


/* Pointers to a intrinsic function and its argument names being
 * checked. */

char *g95_current_intrinsic, *g95_current_intrinsic_arg[MAX_INTRINSIC_ARGS];
g95_locus *g95_current_intrinsic_where;

static g95_intrinsic_sym *functions, *subroutines, *conversion, *next_sym;
static g95_intrinsic_arg *arguments, *next_arg;
static char lib_name;

static int nfunc, nsub, nargs, nconv, simplified, extension_flag;

static enum { SZ_NOTHING=0, SZ_SUBS, SZ_FUNCS, SZ_CONVS, SZ_MODPROC } sizing;

static try do_simplify(g95_intrinsic_sym *, g95_expr *);

g95_charlen g95_unity_charlen;


static struct {
    g95_intrinsic_arg *args;
} modproc[IPROC_LAST];


#define IARG_NONE         0
#define IARG_OPTIONAL     (1 << 0)
#define IARG_ARRAYDESC    (1 << 1)
#define IARG_INTENT_OUT   (1 << 2)
#define IARG_INTENT_INOUT (1 << 3)

/* g95_type_letter()-- Return a letter based on the passed type.  Used
 * to construct the name of a type-dependent subroutine. */

char g95_type_letter(bt type) {
char c;

    switch(type) {
    case BT_LOGICAL:    c = 'l';  break;
    case BT_CHARACTER:  c = 'c';  break;
    case BT_INTEGER:    c = 'i';  break;
    case BT_REAL:       c = 'r';  break;
    case BT_COMPLEX:    c = 'z';  break;
    case BT_DERIVED:    c = 'd';  break;
    default:            c = 'u';  break;
    }

    return c;
}



/* conv_name()-- Return a pointer to the name of a conversion function
 * given two typespecs. */

static char *conv_name(g95_typespec *from, g95_typespec *to) {
char name[30];

    sprintf(name, "__convert_%c%d_%c%d",
	    g95_type_letter(from->type), from->kind,
	    g95_type_letter(to->type), to->kind);

    return g95_get_string(name);
}



/* find_conv()-- Given a pair of typespecs, find the g95_intrinsic_sym
 * node that corresponds to the conversion.  Returns NULL if the
 * conversion isn't found. */

static g95_intrinsic_sym *find_conv(g95_typespec *from, g95_typespec *to) {
g95_intrinsic_sym *sym;
char *target;
int i;

    target = conv_name(from, to);
    sym = conversion;

    for(i=0; i<nconv; i++, sym++)
	if (strcmp(target, sym->name) == 0)
	    return sym;

    return NULL;
}



/* convert_constant()-- Master function to convert one constant to
 * another.  While this is used as a simplification function, it
 * requires the destination type and kind information which is
 * supplied by a special case in do_simplify(). */

static g95_expr *convert_constant(g95_expr *e, bt type, int kind) {
g95_expr *result, *(*f)(g95_expr *, int);

    switch(e->ts.type) {
    case BT_INTEGER:
	switch(type) {
	case BT_INTEGER:  f = g95_int2int;          break;
	case BT_REAL:     f = g95_int2real;         break;
	case BT_COMPLEX:  f = g95_int2complex;      break;
	default: goto oops;
	}
	break;

    case BT_REAL:
	switch(type) {
	case BT_INTEGER:  f = g95_real2int;         break;
	case BT_REAL:     f = g95_real2real;        break;
	case BT_COMPLEX:  f = g95_real2complex;     break;
	default: goto oops;
	}
	break;

    case BT_COMPLEX:
	switch(type) {
	case BT_INTEGER:  f = g95_complex2int;      break;
	case BT_REAL:     f = g95_complex2real;     break;
	case BT_COMPLEX:  f = g95_complex2complex;  break;

	default: goto oops;
	}
	break;

    case BT_LOGICAL:
	if (type != BT_LOGICAL)
	    goto oops;

	f = g95_log2log;
	break;

    default:
    oops:
	g95_internal_error("convert_constant(): Unexpected type");
    }

    result = NULL;

    /* Convert a constant */

    if (e->type == EXPR_CONSTANT) {
	result = f(e, kind);
	if (result == NULL)
	    return &g95_bad_expr;
    }

    return result;
}



/* do_check()-- Interface to the check functions.  We break apart an
 * argument list and call the proper check function rather than
 * forcing each function to manipulate the argument list */

static try do_check(g95_intrinsic_sym *specific, g95_actual_arglist *arg) {
g95_expr *a1, *a2, *a3, *a4, *a5;

    a1 = arg->u.expr;
    arg = arg->next;

    if (arg == NULL) 
	return (*specific->check)(a1);

    a2 = arg->u.expr;
    arg = arg->next;

    if (arg == NULL) 
	return (*specific->check)(a1, a2);

    a3 = arg->u.expr;
    arg = arg->next;

    if (arg == NULL)
	return (*specific->check)(a1, a2, a3);

    a4 = arg->u.expr;
    arg = arg->next;

    if (arg == NULL)
	return (*specific->check)(a1, a2, a3, a4);

    a5 = arg->u.expr;
    arg = arg->next;

    if (arg == NULL)
	return (*specific->check)(a1, a2, a3, a4, a5);

    g95_internal_error("do_check(): too many args");
    return FAILURE;
}



/* include_symbol()-- Check to see if we should add this symbol or
 * not.  Returns nonzero if so. */

static int include_symbol(char *name) {
char *p, *q;
int m, n;

    if (!extension_flag || g95_option.fmode == 0 ||
	(g95_option.intrinsic_extensions && g95_option.intrinsics == NULL))
	return 1;

    /* See if the name is in the comma-separated list. */

    m = strlen(name);
    p = g95_option.intrinsics;

    if (*p == '\0')
	return 0;

    for(;;) {
	q = strchr(p, ',');

	n = (q == NULL)
	    ? strchr(p, '\0') - p
	    : q - p;

	if (n == m && strncasecmp(name, p, m) == 0)
	    return 1;

	if (q == NULL)
	    break;

	p = q + 1;
    }

    return 0;
}



/* hide_symbol()-- Hide the last intrinsic symbol added */

static void hide_symbol(void) {

    if (sizing == SZ_NOTHING)
	next_sym[-1].hidden = 1;
}


/*********** Subroutines to build the intrinsic list ****************/

/* add_sym()-- Add a single intrinsic symbol to the current list.
 * Returns nonzero if the symbol was added.
 *
 * Argument list:
 *    char *     name of function
 *    int        whether function is elemental
 *    enum       Id of this name
 *    bt         return type of function
 *    int        kind of return type of function
 *    char *     Name of to use when passed as an actual argument
 *    check      pointer to check function
 *    simplify   pointer to simplification function
 *    resolve    pointer to resolution function
 * Optional arguments come in multiples of four:
 *    char *    name of argument
 *    bt        type of argument
 *    int       kind of argument
 *    int       argument flags
 *
 * the sequence is terminated by a NULL name. */

static int add_sym(char *name, int elemental, g95_isym_id id, bt type,
		   int kind, char *actual, try (*check)(),
		   g95_expr *(*simplify)(), void (*resolve)(), ...) {

int flags, first_flag;
char buffer[100];
va_list argp;

    if (!include_symbol(name)) 
	return 0;

    switch(sizing) {
    case SZ_SUBS:
	nsub++;
	break;

    case SZ_FUNCS:
	nfunc++;
	break;

    case SZ_NOTHING:
	next_sym->name = g95_get_string(name);

	strcpy(buffer, PREFIX);
	strcat(buffer, name);
	next_sym->lib_name = g95_get_string(buffer);

	next_sym->elemental = elemental;
	next_sym->ts.type = type;
	next_sym->ts.kind = kind;
	next_sym->simplify = simplify;
	next_sym->check = check;
	next_sym->resolve = resolve;
	next_sym->specific = 0;
	next_sym->generic = 0;
	next_sym->hidden = 0;
	next_sym->alias = 0;
	next_sym->id = id;

	if (actual == &lib_name)
	    next_sym->actual = next_sym->lib_name;

	else
	    next_sym->actual = actual;

	break;

    default:
	g95_internal_error("add_sym(): Bad sizing mode");
    }

    va_start(argp, resolve);

    first_flag = 1;

    for(;;) {
	name = va_arg(argp, char *);
	if (name == NULL)
	    break;

	type  = (bt) va_arg(argp, int);
	kind  = va_arg(argp, int);
	flags = va_arg(argp, int);

	if (sizing != SZ_NOTHING) {
	    nargs++;
	    continue;
	}

	if (first_flag)
	    next_sym->formal = next_arg;

	else
	    (next_arg-1)->next = next_arg;

	first_flag = 0;

	next_arg->name = g95_get_string(name);
	next_arg->ts.type = type;
	next_arg->ts.kind = kind;

	next_arg->optional   = !!(flags & IARG_OPTIONAL);
	next_arg->array_desc = !!(flags & IARG_ARRAYDESC);

	next_arg->intent = (flags & IARG_INTENT_INOUT)
	    ? INTENT_INOUT
	    : ((flags & IARG_INTENT_OUT)
	       ? INTENT_OUT
	       : INTENT_IN);

	next_arg++;
    }

    va_end(argp);

    next_sym++;
    return 1;
}



/* library_name()-- Set the library name of an intrinsic. */

static void library_name(char *name) {

    if (sizing == SZ_NOTHING)
	next_sym[-1].lib_name = name;
}



/* result_length()-- Set the return length of a character intrinsic. */

static void result_length(int len) {

    if (sizing == SZ_NOTHING)
	next_sym[-1].len = len;
}



/* find_sym()-- Locate an intrinsic symbol given a base pointer,
 * number of elements in the table and a pointer to a name.  Returns
 * the NULL pointer if a name is not found. */

static g95_intrinsic_sym *find_sym(g95_intrinsic_sym *start, int n,
				   char *name) {

    while(n > 0) {
	if (start->name == NULL)
	    break;

	if (strcmp(name, start->name) == 0 && !start->hidden)
	    return start;

	start++;
	n--;
    }

    return NULL;
}



/* g95_find_function()-- Given a name, find a function in the intrinsic
 * function table.  Returns NULL if not found. */

g95_intrinsic_sym *g95_find_function(char *name) {
g95_intrinsic_sym *p;

    p = find_sym(functions, nfunc, name);
    if (p != NULL)
	return p;

    return find_sym(conversion, nconv, name);
}



/* g95_find_subroutine()-- Given a name, find a function in the
 * intrinsic subroutine table.  Returns NULL if not found. */

g95_intrinsic_sym *g95_find_subroutine(char *name) {

    return find_sym(subroutines, nsub, name);
}



/* g95_find_id()-- Given a name, find the G95_ISYM_* number.  Returns
 * G95_ISYM_NONE if the name isn't found. */

int g95_find_id(char *name) {
g95_intrinsic_sym *sym;

    sym = g95_find_function(name);
    if (sym == NULL)
	sym = g95_find_subroutine(name);

    return (sym == NULL) ? G95_ISYM_NONE : sym->id;
}



/* g95_generic_intrinsic()-- Given a string, figure out if it is
 * the name of a generic intrinsic function or not. */

int g95_generic_intrinsic(char *name, int sub) {
g95_intrinsic_sym *sym;
int n;

    if (sub) {
	sym = g95_find_subroutine(name);
	n = (sym != NULL);

    } else {
	sym = g95_find_function(name);
	n = (sym == NULL) ? 0 : sym->generic;
    }

    return n;
}



/* g95_specific_intrinsic()-- Given a string, figure out if it is the
 * name of a specific intrinsic function or not. */

int g95_specific_intrinsic(char *name) {
g95_intrinsic_sym *sym;

    sym = g95_find_function(name);
    return (sym == NULL) ? 0 : sym->specific;
}



/* g95_intrinsic_name()-- Given a string, figure out if it is the name
 * of an intrinsic subroutine or function.  There are no generic
 * intrinsic subroutines, they are all specific. */

int g95_intrinsic_name(char *name, int subroutine_flag) {

    return (subroutine_flag
	    ? g95_find_subroutine(name)
	    : g95_find_function(name)) != NULL;
}



/* make_generic()-- Collect a set of intrinsic functions into a
 * generic collection.  The first argument is the name of the generic
 * function, which is also the name of a specific function.  The rest
 * of the specifics currently in the table are placed into the list of
 * specific functions associated with that generic.  */

static void make_generic(char *name) {
g95_intrinsic_sym *g, *base;

    if (sizing != SZ_NOTHING)
	return;

    g = base = g95_find_function(name);
    if (g == NULL)
	g95_internal_error("make_generic(): Can't find generic symbol '%s'",
			   name);

    g->generic = 1;
    g->specific = 1;

    if ((g+1)->name != NULL)
	g->specific_head = g + 1;

    g++;
  
    while(g->name != NULL) {
	g->next = g + 1;
	g->specific = 1;

	if (g->alias) {
	    g->generic = 1;
	    g->specific_head = base + 1;
	}

	g++;
    }

    g--;
    g->next = NULL;
}



/* make_alias()-- Create a duplicate intrinsic function entry for the
 * current function, the only difference being the alternate name.
 * Note that we use argument lists more than once, but all argument
 * lists are freed as a single block.  */

static void make_alias(char *name) {

    switch(sizing) {
    case SZ_FUNCS:
	nfunc++;
	break;

    case SZ_SUBS:
	nsub++;
	break;

    case SZ_NOTHING:
	next_sym[0] = next_sym[-1];
	next_sym->name = g95_get_string(name);
	next_sym->alias = 1;
	next_sym++;
	break;

    default:
	break;
    }
}



/* add_f2003_functions()-- Add F2003 functions to the intrinsic
 * function table if required. */

static void add_f2003_functions(void) {
int dc, di0, di1, dr0, dr1, dd, dl;

    di0 = g95_default_integer_kind(0);
    di1 = g95_default_integer_kind(1);
    dc  = g95_default_character_kind();
    dl  = g95_default_logical_kind();
    dr0 = g95_default_real_kind(0);
    dr1 = g95_default_real_kind(1);
    dd  = g95_default_double_kind();

    add_sym("command_argument_count", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0,
	    NULL,
	    NULL, NULL, NULL,
	    NULL);

    make_generic("command_argument_count");

    add_sym("selected_char_kind", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, g95_simplify_selected_char_kind, NULL,
	    "name", BT_CHARACTER, dc, IARG_NONE, NULL);

    add_sym("new_line", 1, G95_ISYM_EXTENSION, BT_CHARACTER, dc, NULL,
	    NULL, g95_simplify_newline, NULL,
	    "a", BT_CHARACTER, dc, IARG_NONE, NULL);

    make_generic("new_line");

    add_sym("is_iostat_end", 1, G95_ISYM_EXTENSION, BT_LOGICAL, dl, NULL,
	    g95_check_iostat, NULL, g95_resolve_is_iostat_end,
	    "i", BT_INTEGER, di0, IARG_NONE, NULL);

    make_generic("is_iostat_end");

    add_sym("is_iostat_eor", 1, G95_ISYM_EXTENSION, BT_LOGICAL, dl, NULL,
	    g95_check_iostat, NULL, g95_resolve_is_iostat_eor,
	    "i", BT_INTEGER, di0, IARG_NONE, NULL);

    make_generic("is_iostat_eor");
}



/* add_f2008_functions()-- Add F2008 functions to the intrinsic
 * function table. */

static void add_f2008_functions(void) {
static char coarray[] = "coarray";
static char kind[]    = "kind";
static char dim[]     = "dim";
static char sub[]     = "sub";
int di0;

    di0 = g95_default_integer_kind(0);

    add_sym("lcobound", 0, G95_ISYM_EXTENSION, BT_INTEGER, 0, NULL,
	    g95_check_co_bound, g95_simplify_co_lbound, g95_resolve_co_lbound,
	    coarray, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dim,     BT_UNKNOWN, 0, IARG_OPTIONAL,
	    kind,    BT_UNKNOWN, 0, IARG_OPTIONAL, NULL);

    make_alias("co_lbound");

    add_sym("ucobound", 0, G95_ISYM_EXTENSION, BT_INTEGER, 0, NULL,
	    g95_check_co_bound, g95_simplify_co_ubound, g95_resolve_co_ubound,
	    coarray, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dim,     BT_UNKNOWN, 0, IARG_OPTIONAL,
	    kind,    BT_UNKNOWN, 0, IARG_OPTIONAL, NULL);

    make_alias("co_ubound");

    add_sym("image_index", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    g95_check_image_index, NULL, NULL,
	    coarray, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    sub,     BT_UNKNOWN, 0, IARG_ARRAYDESC, NULL);

    add_sym("num_images", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL, NULL);

    add_sym("this_image", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    g95_check_this_image, NULL, g95_resolve_this_image,
	    coarray, BT_UNKNOWN, 0,   IARG_ARRAYDESC | IARG_OPTIONAL,
	    dim,     BT_INTEGER, di0, IARG_OPTIONAL, NULL);
}



/* add_function_extensions()-- Add some semi-standard intrinsic
 * function extensions. */

static void add_function_extensions(void) {
int m, dc, di0, di1, dr0, dr1, dd, dl;

    di0 = g95_default_integer_kind(0);
    di1 = g95_default_integer_kind(1);
    dc  = g95_default_character_kind();
    dl  = g95_default_logical_kind();
    dr0 = g95_default_real_kind(0);
    dr1 = g95_default_real_kind(1);
    dd  = g95_default_double_kind();

    extension_flag = 1;

    if (add_sym("access", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"filename", BT_CHARACTER, dc, IARG_NONE,
		"mode", BT_CHARACTER, dc, IARG_NONE, NULL))
	make_generic("access");

    m = add_sym("algama", 1, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		NULL, NULL, g95_resolve_lgamma,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dlgama", 1, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_lgamma,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("algama");

    /* Bessel functions */

    m = add_sym("besj0", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		NULL, NULL, g95_resolve_besj0,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dbesj0", 0, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_besj0,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("besj0");

    m = add_sym("besj1", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		NULL, NULL, g95_resolve_besj1,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dbesj1", 0, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_besj1,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("besj1");

    m = add_sym("besjn", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		g95_check_besn, NULL, g95_resolve_besjn,
		"n", BT_INTEGER, di0, IARG_NONE,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dbesjn", 0, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    g95_check_besn2, NULL, g95_resolve_besjn,
	    "n", BT_INTEGER, di0, IARG_NONE,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("besjn");

    m = add_sym("besy0", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		NULL, NULL, g95_resolve_besy0,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dbesy0", 0, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_besy0,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("besy0");

    m = add_sym("besy1", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		NULL, NULL, g95_resolve_besy1,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dbesy1", 0, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_besy1,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("besy1");

    m = add_sym("besyn", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		g95_check_besn, NULL, g95_resolve_besyn,
		"n", BT_INTEGER, di0, IARG_NONE,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dbesyn", 0, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    g95_check_besn2, NULL, g95_resolve_besyn,
	    "n", BT_INTEGER, di0, IARG_NONE,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("besyn");

    if (add_sym("chdir", 1, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"dir", BT_CHARACTER, dc, IARG_NONE, NULL))
	make_generic("chdir");

    add_sym("chmod", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL,
	    "file", BT_CHARACTER, dc, IARG_NONE,
	    "mode", BT_INTEGER, di0, IARG_NONE, NULL);

    if (add_sym("dtime", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, NULL,
		NULL, NULL, g95_resolve_dtime,
		"tarray", BT_REAL, dr0, IARG_OPTIONAL | IARG_INTENT_OUT, NULL))
	make_generic("dtime");

    if (add_sym("etime", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, NULL,
		g95_check_etime, NULL, g95_resolve_etime,
		"tarray", BT_REAL, dr0, IARG_OPTIONAL | IARG_INTENT_OUT, NULL))
	make_generic("etime");

    m = add_sym("erfc", 1, G95_ISYM_EXTENSION, BT_REAL, dr1, &lib_name,
		NULL, NULL, g95_resolve_erfc,
		"x", BT_REAL, dr1, IARG_NONE, NULL);

    add_sym("derfc", 1, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_erfc,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("erfc");

    m = add_sym("erf", 1, G95_ISYM_EXTENSION, BT_REAL, dr1, &lib_name,
		NULL, NULL, g95_resolve_erf,
		"x", BT_REAL, dr1, IARG_NONE, NULL);

    add_sym("derf", 1, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_erf,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("erf");

    if (add_sym("fdate", 0, G95_ISYM_EXTENSION, BT_CHARACTER, dc, NULL,
		NULL, NULL, NULL, NULL))
	result_length(24);

    add_sym("fnum", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL,
	    "unit", BT_INTEGER, di1, IARG_NONE, NULL);

    if (add_sym("fstat", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"unit", BT_INTEGER, di1, IARG_NONE,
		"sarray", BT_INTEGER, di1, IARG_INTENT_OUT, NULL))
	library_name(PREFIX "fstat_f");

    add_sym("ftell", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL,
	    "unit", BT_INTEGER, di1, IARG_NONE, NULL);

    m = add_sym("gamma", 1, G95_ISYM_EXTENSION, BT_REAL, dr0, &lib_name,
		NULL, NULL, g95_resolve_gamma,
		"x", BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dgamma", 1, G95_ISYM_EXTENSION, BT_REAL, dd, &lib_name,
	    NULL, NULL, g95_resolve_gamma,
	    "x", BT_REAL, dd, IARG_NONE, NULL);

    if (m)
	make_generic("gamma");

    if (add_sym("getcwd", 1, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"name", BT_CHARACTER, dc, IARG_INTENT_OUT, NULL))
	make_generic("getcwd");

    if (add_sym("getuid", 1, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		NULL))
	make_generic("getuid");

    if (add_sym("getgid", 1, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		NULL))
	make_generic("getgid");

    if (add_sym("getpid", 1, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		NULL))
	make_generic("getpid");

    add_sym("hostnm", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL,
	    "name", BT_CHARACTER, dc, IARG_INTENT_OUT, NULL);

    add_sym("iargc", 1, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL,
	    NULL);

    add_sym("ierrno", 1, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL,
	    NULL);

    if (add_sym("isatty", 1, G95_ISYM_EXTENSION, BT_LOGICAL, dl, NULL,
		NULL, NULL, NULL,
		"unit", BT_INTEGER, di0, IARG_NONE, NULL))
	make_generic("isatty");

    if (add_sym("isnan", 1, G95_ISYM_EXTENSION, BT_LOGICAL, dl, NULL,
		g95_check_isnan, NULL, g95_resolve_isnan,
		"x", BT_REAL, dr1, IARG_NONE, NULL))
	make_generic("isnan");

    if (add_sym("link", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"path1", BT_CHARACTER, dc, IARG_NONE,
		"path2", BT_CHARACTER, dc, IARG_NONE, NULL))
	make_generic("link");

    add_sym("loc", 0, G95_ISYM_LOC, BT_INTEGER, SIZEOF_VOID_P, NULL,
	    g95_check_loc, NULL, NULL,
	    "x", BT_UNKNOWN, 0, IARG_NONE, NULL);

    if (add_sym("lstat", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"file", BT_CHARACTER, dc, IARG_NONE,
		"sarray", BT_INTEGER, di1, IARG_INTENT_OUT, NULL))
	library_name(PREFIX "lstat_f");

    if (add_sym("rand", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, NULL,
		NULL, NULL, NULL,
		"x", BT_INTEGER, di0, IARG_OPTIONAL, NULL))
	make_generic("rand");

    if (add_sym("rename", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"path1", BT_CHARACTER, dc, IARG_NONE,
		"path2", BT_CHARACTER, dc, IARG_NONE, NULL))
	library_name(PREFIX "rename_f");

    m = add_sym("secnds", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		g95_check_secnds, NULL, g95_resolve_secnds,
		"t", BT_REAL, dr0, IARG_NONE, NULL);
 
    if (m)
	make_generic("secnds");

    if (add_sym("second", 0, G95_ISYM_EXTENSION, BT_REAL, dr0, NULL,
		NULL, NULL, NULL,
		NULL))
	library_name(PREFIX "second_f");

    if (add_sym("signal", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		g95_check_signal, NULL, NULL,
		"signal", BT_INTEGER, di0, IARG_NONE,
		"handler", BT_PROCEDURE, 0, IARG_NONE,
		"flag", BT_INTEGER, di0, IARG_NONE, NULL)) {
	library_name(PREFIX "signal_f");
	make_generic("signal");
    }

    if (add_sym("stat", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"file", BT_CHARACTER, dc, IARG_NONE,
		"sarray", BT_INTEGER, di1, IARG_INTENT_OUT, NULL))
	library_name(PREFIX "stat_f");

    if (add_sym("system", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL,
		"cmd", BT_CHARACTER, dc, IARG_NONE, NULL)) {
	make_generic("system");
	library_name(PREFIX "system_f");
    }

    if (add_sym("time", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
		NULL, NULL, NULL, NULL))
	make_generic("time");

    add_sym("unlink", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    NULL, NULL, NULL,
	    "file", BT_CHARACTER, dc, IARG_NONE, NULL);

    extension_flag = 0;
}



/* add_functions()-- Add intrinsic functions */

static void add_functions(void) {

/* Argument names as in the standard (to be used as argument keywords) */

char a[] = "a",  f[] = "field",      pt[] = "pointer",   tg[] = "target",
     b[] = "b",  m[] = "matrix",     ma[] = "matrix_a",  mb[] = "matrix_b",
     c[] = "c",  n[] = "ncopies",   pos[] = "pos",      bck[] = "back",
     i[] = "i",  v[] = "vector",     va[] = "vector_a",  vb[] = "vector_b",
     j[] = "j", a1[] = "a1",         fs[] = "fsource",   ts[] = "tsource",
     l[] = "l", a2[] = "a2",         mo[] = "mold",     ord[] = "order",
     p[] = "p", ar[] = "array",     shp[] = "shape",    src[] = "source",
     r[] = "r", bd[] = "boundary",  pad[] = "pad",      set[] = "set",
     s[] = "s", dm[] = "dim",      kind[] = "kind",     msk[] = "mask",
     x[] = "x", sh[] = "shift",     stg[] = "string",   ssg[] = "substring",
     y[] = "y", sz[] = "size",      sta[] = "string_a", stb[] = "string_b",
     z[] = "z", ln[] = "len";

int di0, di1, dr0, dr1, dd, dl, dc, dx, dq;

    di0 = g95_default_integer_kind(0);
    di1 = g95_default_integer_kind(1);

    dr0 = g95_default_real_kind(0);
    dr1 = g95_default_real_kind(1);

    dx = g95_extended_kind();
    dq = g95_quad_kind();

    dd = g95_default_double_kind();
    dl = g95_default_logical_kind();
    dc = g95_default_character_kind();

    add_sym("abs", 1, G95_ISYM_ABS, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "abs_r%d", dr1),
	    g95_check_abs, g95_simplify_abs, g95_resolve_abs,
	    a, BT_REAL, dr1, IARG_NONE, NULL);

    add_sym("iabs", 1, G95_ISYM_ABS, BT_INTEGER, di1,
	    g95_get_fstring(PREFIX "abs_i%d", di1),
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_INTEGER, di1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("dabs", 1, G95_ISYM_ABS, BT_REAL, dd,
	    g95_get_fstring(PREFIX "abs_r%d", dd),
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("cabs", 1, G95_ISYM_ABS, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "abs_z%d", dr1),
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_COMPLEX, dr1, IARG_NONE, NULL);
    
    if (G95_STRICT_F())
	hide_symbol();

    add_sym("zabs", 1, G95_ISYM_ABS, BT_REAL, dd,
	    g95_get_fstring(PREFIX "abs_z%d", dd),
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_COMPLEX, dd, IARG_NONE, NULL);

    if (g95_option.fmode != 0)
	hide_symbol();

    if (g95_option.fmode == 0)
	make_alias("cdabs");

    add_sym("", 1, G95_ISYM_ABS, BT_REAL, dx, NULL,
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_ABS, BT_REAL, dq, NULL,
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_ABS, BT_REAL, dx, NULL,
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_COMPLEX, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_ABS, BT_REAL, dq, NULL,
	    NULL, g95_simplify_abs, g95_resolve_abs,
	    a, BT_COMPLEX, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("abs");

    if (!G95_STRICT_F()) {
	add_sym("achar", 1, G95_ISYM_ACHAR, BT_CHARACTER, dc, NULL,
		g95_check_achar, g95_simplify_achar, g95_resolve_achar,
		i, BT_INTEGER, di1, IARG_NONE, NULL);

	make_generic("achar");
    }

    add_sym("acos", 1, G95_ISYM_ACOS, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "acos_r%d", dr0),
	    NULL, g95_simplify_acos, g95_resolve_acos,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dacos", 1, G95_ISYM_ACOS, BT_REAL, dd,
	    g95_get_fstring(PREFIX "acos_r%d", dd),
	    NULL, g95_simplify_acos, g95_resolve_acos,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_ACOS, BT_REAL, dx, NULL,
	    NULL, g95_simplify_acos, g95_resolve_acos,
	    x, BT_REAL, dx, IARG_NONE, NULL);
    
    hide_symbol();

    add_sym("", 1, G95_ISYM_ACOS, BT_REAL, dq, NULL,
	    NULL, g95_simplify_acos, g95_resolve_acos,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("acos");

    add_sym("adjustl", 1, G95_ISYM_ADJUSTL, BT_CHARACTER, dc, NULL,
	    NULL, g95_simplify_adjustl, g95_resolve_adjust,
	    stg, BT_CHARACTER, dc, IARG_NONE, NULL);

    make_generic("adjustl");

    add_sym("adjustr", 1, G95_ISYM_ADJUSTR, BT_CHARACTER, dc, NULL,
	    NULL, g95_simplify_adjustr, g95_resolve_adjust,
	    stg, BT_CHARACTER, dc, IARG_NONE, NULL);

    make_generic("adjustr");

    add_sym("aimag", 1, G95_ISYM_AIMAG, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "aimag_%d", dr1),
	    NULL, g95_simplify_aimag, g95_resolve_aimag,
	    z, BT_COMPLEX, dr0, IARG_NONE, NULL);

    if (g95_option.fmode == 0)
	make_alias("imag");

    add_sym("dimag", 1, G95_ISYM_AIMAG, BT_REAL, dd,
	    g95_get_fstring(PREFIX "aimag_%d", dd),
	    NULL, g95_simplify_aimag, g95_resolve_aimag,
	    z, BT_COMPLEX, dd, IARG_NONE, NULL);    /* Extension */

    if (g95_option.fmode != 0)
	hide_symbol();

    add_sym("", 1, G95_ISYM_AIMAG, BT_REAL, dx, NULL,
	    NULL, g95_simplify_aimag, g95_resolve_aimag,
	    z, BT_COMPLEX, dx, IARG_NONE, NULL);
    
    hide_symbol();

    add_sym("", 1, G95_ISYM_AIMAG, BT_REAL, dq, NULL,
	    NULL, g95_simplify_aimag, g95_resolve_aimag,
	    z, BT_COMPLEX, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("aimag");

    add_sym("aint", 1, G95_ISYM_AINT, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "aint%d_%d", dr1, dr1),
	    g95_check_a_xkind, g95_simplify_aint, g95_resolve_aint,
	    a, BT_REAL, dr0, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    add_sym("dint", 1, G95_ISYM_AINT, BT_REAL, dd,
	    g95_get_fstring(PREFIX "aint%d_%d", dd, dd),
	    NULL, g95_simplify_dint, NULL,
	    a, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    make_generic("aint");

    add_sym("all", 0, G95_ISYM_ALL, BT_UNKNOWN, 0, NULL,
	    g95_check_all_any, NULL, g95_resolve_all,
	    msk, BT_LOGICAL, dl, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("all");

    add_sym("allocated", 0, G95_ISYM_ALLOCATED, BT_LOGICAL, dl, NULL,
	    g95_check_allocated, NULL, NULL,
	    ar, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("allocated");

    add_sym("anint", 1, G95_ISYM_ANINT, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "anint%d_%d", dr1, dr1),
	    g95_check_a_xkind, g95_simplify_anint, g95_resolve_anint,
	    a, BT_REAL, dr0, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    add_sym("dnint", 1, G95_ISYM_ANINT, BT_REAL, dd,
	  g95_get_fstring(PREFIX "anint%d_%d", dd, dd),
	    NULL, g95_simplify_dnint, NULL,
	    a, BT_REAL, dd, IARG_NONE, NULL);

    make_generic("anint");

    add_sym("any", 0, G95_ISYM_ANY, BT_UNKNOWN, 0, NULL,
	    g95_check_all_any, NULL, g95_resolve_any,
	    msk, BT_LOGICAL, dl, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("any");

    add_sym("asin", 1, G95_ISYM_ASIN, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "asin_r%d", dr0),
	    NULL, g95_simplify_asin, g95_resolve_asin,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dasin", 1, G95_ISYM_ASIN, BT_REAL, dd,
	    g95_get_fstring(PREFIX "asin_r%d", dd),
	    NULL, g95_simplify_asin, g95_resolve_asin,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_ASIN, BT_REAL, dx, NULL,
	    NULL, g95_simplify_asin, g95_resolve_asin,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_ASIN, BT_REAL, dq, NULL,
	    NULL, g95_simplify_asin, g95_resolve_asin,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("asin");

    add_sym("associated", 0, G95_ISYM_ASSOCIATED, BT_LOGICAL, dl, NULL,
	    g95_check_associated, NULL, NULL,
	    pt, BT_UNKNOWN, 0, IARG_NONE,
	    tg, BT_UNKNOWN, 0, IARG_OPTIONAL, NULL);

    make_generic("associated");

    add_sym("atan", 1, G95_ISYM_ATAN, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "atan_r%d", dr1),
	    NULL, g95_simplify_atan, g95_resolve_atan,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("datan", 1, G95_ISYM_ATAN, BT_REAL, dd,
	    g95_get_fstring(PREFIX "atan_r%d", dd),
	    NULL, g95_simplify_atan, g95_resolve_atan,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_ATAN, BT_REAL, dx, NULL,
	    NULL, g95_simplify_atan, g95_resolve_atan,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_ATAN, BT_REAL, dq, NULL,
	    NULL, g95_simplify_atan, g95_resolve_atan,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("atan");

    add_sym("atan2", 1, G95_ISYM_ATAN2, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "atan2_r%d", dr1),
	    g95_check_atan2, g95_simplify_atan2, g95_resolve_atan2,
	    y, BT_REAL, dr0, IARG_NONE,   x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("datan2", 1, G95_ISYM_ATAN2, BT_REAL, dd,
	    g95_get_fstring(PREFIX "atan2_r%d", dd),
	    NULL, g95_simplify_atan2, g95_resolve_atan2,
	    y, BT_REAL, dd, IARG_NONE,   x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_ATAN2, BT_REAL, dx, NULL,
	    NULL, g95_simplify_atan2, g95_resolve_atan2,
	    y, BT_REAL, dx, IARG_NONE,   x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_ATAN2, BT_REAL, dq, NULL,
	    NULL, g95_simplify_atan2, g95_resolve_atan2,
	    y, BT_REAL, dq, IARG_NONE,   x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("atan2");

    add_sym("bit_size", 0, G95_ISYM_BIT_SIZE, BT_INTEGER, di1, NULL,
	    g95_check_i, g95_simplify_bit_size, NULL,
	    i, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("bit_size");

    add_sym("btest", 1, G95_ISYM_BTEST, BT_LOGICAL, dl, NULL,
	    g95_check_btest, g95_simplify_btest, g95_resolve_btest,
	    i, BT_INTEGER, di1, IARG_NONE,
	    pos, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("btest");

    add_sym("ceiling", 1, G95_ISYM_CEILING, BT_UNKNOWN, 0, NULL,
	    g95_check_a_ikind, g95_simplify_ceiling, g95_resolve_ceiling,
	    a, BT_REAL, dr0, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("ceiling");

    add_sym("char", 1, G95_ISYM_CHAR, BT_CHARACTER, dc, NULL,
	    g95_check_char, g95_simplify_char, g95_resolve_char,
	    i, BT_INTEGER, di1, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("char");

    add_sym("cmplx", 1, G95_ISYM_CMPLX, BT_UNKNOWN, 0, NULL,
	    g95_check_cmplx, g95_simplify_cmplx, g95_resolve_cmplx,
	    x, BT_UNKNOWN, dr0, IARG_NONE,
	    y, BT_UNKNOWN, dr0, IARG_OPTIONAL,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("cmplx");

    /* Making dcmplx a specific of cmplx causes cmplx to return a double
     * complex instead of the default complex.  */

    if (g95_option.fmode == 0) {
	add_sym("dcmplx", 1, G95_ISYM_CMPLX, BT_COMPLEX, dd, NULL,
		g95_check_dcmplx, g95_simplify_dcmplx, NULL,
		x, BT_REAL, dd, IARG_NONE,
		y, BT_REAL, dd, IARG_OPTIONAL, NULL);  /* Extension */

	make_generic("dcmplx");
    }

    add_sym("conjg", 1, G95_ISYM_CONJG, BT_COMPLEX, dr0,
	    g95_get_fstring(PREFIX "conjg_%d", dr1),
	    NULL, g95_simplify_conjg, g95_resolve_conjg,
	    z, BT_COMPLEX, dr0, IARG_NONE, NULL);

    add_sym("dconjg", 1, G95_ISYM_CONJG, BT_COMPLEX, dd,
	    g95_get_fstring(PREFIX "conjg_%d", dd),
	    NULL, g95_simplify_conjg, g95_resolve_conjg,
	    z, BT_COMPLEX, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_CONJG, BT_COMPLEX, dx, NULL,
	    NULL, g95_simplify_conjg, g95_resolve_conjg,
	    z, BT_COMPLEX, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_CONJG, BT_COMPLEX, dq, NULL,
	    NULL, g95_simplify_conjg, g95_resolve_conjg,
	    z, BT_COMPLEX, dq, IARG_NONE, NULL);

    hide_symbol();

    if (g95_option.fmode != 0)
	hide_symbol();

    make_generic("conjg");

    add_sym("cos", 1, G95_ISYM_COS, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "cos_r%d", dr1),
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dcos", 1, G95_ISYM_COS, BT_REAL, dd,
	    g95_get_fstring(PREFIX "cos_r%d", dd),
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("ccos", 1, G95_ISYM_COS, BT_COMPLEX, dr0,
	    g95_get_fstring(PREFIX "cos_z%d", dr1),
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_COMPLEX, dr1, IARG_NONE, NULL);
    
    if (G95_STRICT_F())
	hide_symbol();

    add_sym("zcos", 1, G95_ISYM_COS, BT_COMPLEX, dd,
	    g95_get_fstring(PREFIX "cos_z%d", dd),
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_COMPLEX, dd, IARG_NONE, NULL);

    if (g95_option.fmode != 0)
	hide_symbol();

    make_alias("cdcos");

    add_sym("", 1, G95_ISYM_COS, BT_REAL, dx, NULL,
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_COS, BT_REAL, dq, NULL,
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_COS, BT_COMPLEX, dx, NULL,
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_COMPLEX, dx, IARG_NONE, NULL);
    
    hide_symbol();

    add_sym("", 1, G95_ISYM_COS, BT_COMPLEX, dq, NULL,
	    NULL, g95_simplify_cos, g95_resolve_cos,
	    x, BT_COMPLEX, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("cos");

    add_sym("cosh", 1, G95_ISYM_COSH, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "cosh_r%d", dr1),
	    NULL, g95_simplify_cosh, g95_resolve_cosh,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dcosh", 1, G95_ISYM_COSH, BT_REAL, dd,
	    g95_get_fstring(PREFIX "cosh_r%d", dd),
	    NULL, g95_simplify_cosh, g95_resolve_cosh,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_COSH, BT_REAL, dx, NULL,
	    NULL, g95_simplify_cosh, g95_resolve_cosh,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_COSH, BT_REAL, dq, NULL,
	    NULL, g95_simplify_cosh, g95_resolve_cosh,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("cosh");

    add_sym("count", 0, G95_ISYM_COUNT, BT_INTEGER, di0, NULL,
	    g95_check_count, NULL, g95_resolve_count,
	    msk, BT_LOGICAL, dl, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("count");

    add_sym("cshift", 0, G95_ISYM_CSHIFT, BT_UNKNOWN, 0, NULL,
	    g95_check_cshift, NULL, g95_resolve_cshift,
	    ar, BT_REAL, dr0, IARG_ARRAYDESC,
	    sh, BT_INTEGER, di1, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("cshift");

    if (!G95_STRICT_F()) {
	add_sym("dble", 1, G95_ISYM_DBLE, BT_REAL, dd, NULL,
		g95_check_dble, g95_simplify_dble, g95_resolve_dble,
		a, BT_REAL, dr0, IARG_NONE, NULL);

	if (g95_option.fmode == 0) {
	    make_alias("dfloat");
	    make_alias("dreal");
	}

	make_generic("dble");
    }

    add_sym("digits", 0, G95_ISYM_DIGITS, BT_INTEGER, di1, NULL,
	    g95_check_digits, g95_simplify_digits, NULL,
	    x, BT_UNKNOWN, dr0, IARG_NONE, NULL);

    make_generic("digits");

    if (!G95_STRICT_F()) {
	add_sym("dim", 1, G95_ISYM_DIM, BT_UNKNOWN, 0,
		g95_get_fstring(PREFIX "dim_r%d", dr1),
		g95_check_a_p, g95_simplify_dim, g95_resolve_dim,
		x, BT_UNKNOWN, dr0, IARG_NONE,  y, BT_UNKNOWN, dr0, IARG_NONE,
		NULL);

	add_sym("idim", 1, G95_ISYM_DIM, BT_INTEGER, di1,
		g95_get_fstring(PREFIX "dim_i%d", di1),
		NULL, g95_simplify_dim, g95_resolve_dim,
		x, BT_INTEGER, di1, IARG_NONE,
		y, BT_INTEGER, di1, IARG_NONE, NULL);

	add_sym("ddim", 1, G95_ISYM_DIM, BT_REAL, dd,
		g95_get_fstring(PREFIX "dim_r%d", dd),
		NULL, g95_simplify_dim, g95_resolve_dim,
		x, BT_REAL, dd, IARG_NONE,   y, BT_REAL, dd, IARG_NONE, NULL);

	make_generic("dim");
    }

    add_sym("dot_product", 0, G95_ISYM_DOT_PRODUCT, BT_UNKNOWN, 0, NULL,
	    g95_check_dot_product, NULL, g95_resolve_dot_product,
	    va, BT_REAL, dr1, IARG_ARRAYDESC,
	    vb, BT_REAL, dr1, IARG_ARRAYDESC, NULL);

    make_generic("dot_product");

    if (!G95_STRICT_F()) {
	add_sym("dprod", 1, G95_ISYM_DPROD, BT_REAL, dd, &lib_name,
		NULL, g95_simplify_dprod, NULL,
		x, BT_REAL, dr1, IARG_NONE,   y, BT_REAL, dr1, IARG_NONE,
		NULL);

	make_generic("dprod");
    }

    add_sym("eoshift", 0, G95_ISYM_EOSHIFT, BT_UNKNOWN, dr0, NULL,
	    g95_check_eoshift, NULL, g95_resolve_eoshift,
	    ar, BT_REAL, dr0, IARG_ARRAYDESC,
	    sh, BT_INTEGER, di1, IARG_ARRAYDESC,
	    bd, BT_REAL, dr0, IARG_OPTIONAL | IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("eoshift");

    add_sym("epsilon", 0, G95_ISYM_EPSILON, BT_UNKNOWN, 0, NULL,
	    g95_check_x_ni, g95_simplify_epsilon, NULL,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    make_generic("epsilon");

    add_sym("exp", 1, G95_ISYM_EXP, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "exp_r%d", dr1),
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dexp", 1, G95_ISYM_EXP, BT_REAL, dd,
	    g95_get_fstring(PREFIX "exp_r%d", dd),
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("cexp", 1, G95_ISYM_EXP, BT_COMPLEX, dr0,
	    g95_get_fstring(PREFIX "exp_z%d", dr1),
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_COMPLEX, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("zexp", 1, G95_ISYM_EXP, BT_COMPLEX, dd,
	    g95_get_fstring(PREFIX "exp_z%d", dd),
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_COMPLEX, dd, IARG_NONE, NULL);

    if (g95_option.fmode != 0)
	hide_symbol();

    make_alias("cdexp");

    add_sym("", 1, G95_ISYM_EXP, BT_REAL, dx, NULL,
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    add_sym("", 1, G95_ISYM_EXP, BT_REAL, dq, NULL,
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    add_sym("", 1, G95_ISYM_EXP, BT_COMPLEX, dx, NULL,
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_COMPLEX, dx, IARG_NONE, NULL);

    add_sym("", 1, G95_ISYM_EXP, BT_COMPLEX, dq, NULL,
	    NULL, g95_simplify_exp, g95_resolve_exp,
	    x, BT_COMPLEX, dq, IARG_NONE, NULL);

    make_generic("exp");

    add_sym("exponent", 1, G95_ISYM_EXPONENT, BT_INTEGER, di0, NULL,
	    g95_check_x, g95_simplify_exponent, g95_resolve_exponent,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    make_generic("exponent");

    add_sym("floor", 1, G95_ISYM_FLOOR, BT_UNKNOWN, 0, NULL,
	    g95_check_a_ikind, g95_simplify_floor, g95_resolve_floor,
	    a, BT_REAL, dr0, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("floor");

    add_sym("fraction", 1, G95_ISYM_FRACTION, BT_UNKNOWN, 0, NULL,
	    g95_check_x, g95_simplify_fraction, g95_resolve_fraction,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    make_generic("fraction");

    add_sym("huge", 0, G95_ISYM_HUGE, BT_UNKNOWN, 0, NULL,
	    g95_check_huge, g95_simplify_huge, NULL,
	    x, BT_UNKNOWN, dr0, IARG_NONE,  NULL);
    
    make_generic("huge");

    if (!G95_STRICT_F()) {
	add_sym("iachar", 1, G95_ISYM_IACHAR, BT_INTEGER, di1, NULL,
		NULL, g95_simplify_iachar, g95_resolve_ichar,
		c, BT_CHARACTER, dc, IARG_NONE, NULL);

	make_generic("iachar");
    }

    add_sym("iand", 1, G95_ISYM_IAND, BT_INTEGER, di1, NULL,
	    g95_check_iand, g95_simplify_iand, g95_resolve_iand,
	    i, BT_INTEGER, di1, 0,   j, BT_INTEGER, di1, 0, NULL);

    make_generic("iand");

    add_sym("ibclr", 1, G95_ISYM_IBCLR, BT_INTEGER, di1, NULL,
	    g95_check_ibclr, g95_simplify_ibclr, g95_resolve_ibclr,
	    i, BT_INTEGER, di1, IARG_NONE,
	    pos, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("ibclr");

    add_sym("ibits", 1, G95_ISYM_IBITS, BT_INTEGER, di1, NULL,
	    g95_check_ibits, g95_simplify_ibits, g95_resolve_ibits,
	    i, BT_INTEGER, di1, IARG_NONE,   pos, BT_INTEGER, di1, IARG_NONE,
	    ln, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("ibits");

    add_sym("ibset", 1, G95_ISYM_IBSET, BT_INTEGER, di1, NULL,
	    g95_check_ibset, g95_simplify_ibset, g95_resolve_ibset,
	    i, BT_INTEGER, di1, IARG_NONE, pos,
	    BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("ibset");

    add_sym("ichar", 1, G95_ISYM_ICHAR, BT_INTEGER, di1, NULL,
	    NULL, g95_simplify_ichar, g95_resolve_ichar,
	    c, BT_CHARACTER, dc, IARG_NONE, NULL);

    make_generic("ichar");

    add_sym("ieor", 1, G95_ISYM_IEOR, BT_INTEGER, di1, NULL,
	    g95_check_ieor, g95_simplify_ieor, g95_resolve_ieor,
	    i, BT_INTEGER, di1, IARG_NONE,
	    j, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("ieor");

    add_sym("index", 1, G95_ISYM_INDEX, BT_INTEGER, di0, PREFIX "index2",
	    g95_check_index, g95_simplify_index, NULL,
	    stg, BT_CHARACTER, dc, IARG_NONE,
	    ssg, BT_CHARACTER, dc, IARG_NONE,
	    bck, BT_LOGICAL, dl, IARG_OPTIONAL, NULL);

    make_generic("index");

    add_sym("int", 1, G95_ISYM_INT, BT_UNKNOWN, 0, NULL,
	    g95_check_int, g95_simplify_int, g95_resolve_int,
	    a, BT_REAL, dr0, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    add_sym("ifix", 1, G95_ISYM_INT, BT_INTEGER, di1, NULL,
	    NULL, g95_simplify_ifix, NULL,
	    a, BT_REAL, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("idint", 1, G95_ISYM_INT, BT_INTEGER, di1, NULL,
	    NULL, g95_simplify_idint, NULL,
	    a, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    make_generic("int");

    add_sym("ior", 1, G95_ISYM_IOR, BT_INTEGER, di1, NULL,
	    g95_check_ior, g95_simplify_ior, g95_resolve_ior,
	    i, BT_INTEGER, di1, IARG_NONE,
	    j, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("ior");

    add_sym("ishft", 1, G95_ISYM_ISHFT, BT_UNKNOWN, 0, NULL,
	    g95_check_ishft, g95_simplify_ishft, g95_resolve_ishft,
	    i, BT_INTEGER, di1, IARG_NONE,
	    sh, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("ishft");

    add_sym("ishftc", 1, G95_ISYM_ISHFTC, BT_UNKNOWN, 0, NULL,
	    g95_check_ishftc, g95_simplify_ishftc, g95_resolve_ishftc,
	    i, BT_INTEGER, di1, IARG_NONE,   sh, BT_INTEGER, di1, IARG_NONE,
	    sz, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("ishftc");

    add_sym("kind", 0, G95_ISYM_KIND, BT_INTEGER, di1, NULL,
	    g95_check_kind, g95_simplify_kind, NULL,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    make_generic("kind");

    add_sym("lbound", 0, G95_ISYM_LBOUND, BT_INTEGER, di0, NULL,
	    g95_check_lbound, g95_simplify_lbound, g95_resolve_lbound,
	    ar, BT_REAL, dr0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    kind, BT_INTEGER, di0, IARG_OPTIONAL, NULL);

    make_generic("lbound");

    add_sym("len", 0, G95_ISYM_LEN, BT_INTEGER, di0, PREFIX "len",
	    NULL, g95_simplify_len, g95_resolve_len,
	    stg, BT_CHARACTER, dc, IARG_NONE, NULL);

    make_generic("len");

    add_sym("len_trim", 1, G95_ISYM_LEN_TRIM, BT_INTEGER, di0, NULL,
	    NULL, g95_simplify_len_trim, g95_resolve_len_trim,
	    stg, BT_CHARACTER, dc, IARG_NONE, NULL);

    if (g95_option.fmode == 0)
	make_alias("lnblnk");

    make_generic("len_trim");

    if (!G95_STRICT_F()) {
	add_sym("lge", 1, G95_ISYM_LGE, BT_LOGICAL, dl, NULL,
		NULL, g95_simplify_lge, NULL,
		sta, BT_CHARACTER, dc, IARG_NONE,
		stb, BT_CHARACTER, dc, IARG_NONE, NULL);

	make_generic("lge");

	add_sym("lgt", 1, G95_ISYM_LGT, BT_LOGICAL, dl, NULL,
		NULL, g95_simplify_lgt, NULL,
		sta, BT_CHARACTER, dc, IARG_NONE,
		stb, BT_CHARACTER, dc, IARG_NONE, NULL);

	make_generic("lgt");

	add_sym("lle", 1, G95_ISYM_LLE, BT_LOGICAL, dl, NULL,
		NULL, g95_simplify_lle, NULL,
		sta, BT_CHARACTER, dc, IARG_NONE,
		stb, BT_CHARACTER, dc, IARG_NONE, NULL);

	make_generic("lle");

	add_sym("llt", 1, G95_ISYM_LLT, BT_LOGICAL, dl, NULL,
		NULL, g95_simplify_llt, NULL,
		sta, BT_CHARACTER, dc, IARG_NONE,
		stb, BT_CHARACTER, dc, IARG_NONE, NULL);

	make_generic("llt");
    }

    add_sym("log", 1, G95_ISYM_LOG, BT_REAL, dr1, NULL,
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_REAL, dr0, IARG_NONE, NULL);
    
    add_sym("alog", 1, G95_ISYM_LOG, BT_REAL, dr0,
	    g95_get_fstring(PREFIX "log_r%d", dr1),
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_REAL, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("dlog", 1, G95_ISYM_LOG, BT_REAL, dd,
	    g95_get_fstring(PREFIX "log_r%d", dd),
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("clog", 1, G95_ISYM_LOG, BT_COMPLEX, dr0,
	    g95_get_fstring(PREFIX "log_z%d", dr1),
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_COMPLEX, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("zlog", 1, G95_ISYM_LOG, BT_COMPLEX, dd,
	    g95_get_fstring(PREFIX "log_z%d", dd),
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_COMPLEX, dd, IARG_NONE, NULL);

    if (g95_option.fmode != 0)
	hide_symbol();

    make_alias("cdlog");

    add_sym("", 1, G95_ISYM_LOG, BT_REAL, dx, NULL,
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_LOG, BT_REAL, dq, NULL,
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_LOG, BT_COMPLEX, dx, NULL,
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_COMPLEX, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_LOG, BT_COMPLEX, dq, NULL,
	    NULL, g95_simplify_log, g95_resolve_log,
	    x, BT_COMPLEX, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("log");

    add_sym("log10", 1, G95_ISYM_LOG10, BT_REAL, dr1, NULL,
	    NULL, g95_simplify_log10, g95_resolve_log10,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("alog10", 1, G95_ISYM_LOG10, BT_REAL, dr0,
	    g95_get_fstring(PREFIX "log10_r%d", dr1),
	    NULL, g95_simplify_log10, g95_resolve_log10,
	    x, BT_REAL, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("dlog10", 1, G95_ISYM_LOG10, BT_REAL, dd,
	    g95_get_fstring(PREFIX "log10_r%d", dd),
	    NULL, g95_simplify_log10, g95_resolve_log10,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_LOG10, BT_REAL, dx, NULL,
	    NULL, g95_simplify_log10, g95_resolve_log10,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_LOG10, BT_REAL, dq, NULL,
	    NULL, g95_simplify_log10, g95_resolve_log10,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("log10");

    add_sym("logical", 1, G95_ISYM_LOGICAL, BT_UNKNOWN, 0, NULL,
	    g95_check_logical, g95_simplify_logical, g95_resolve_logical,
	    l, BT_LOGICAL, dl, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    make_generic("logical");

    add_sym("matmul", 0, G95_ISYM_MATMUL, BT_UNKNOWN, 0, NULL,
	    g95_check_matmul, NULL, g95_resolve_matmul,
	    ma, BT_REAL, 0, IARG_ARRAYDESC,
	    mb, BT_REAL, 0, IARG_ARRAYDESC, NULL);

    make_generic("matmul");

/* Note: amax0 is equivalent to real(max), max1 is equivalent to
 * int(max).  The max function must have at least two arguments. */

    add_sym("max", 1, G95_ISYM_MAX, BT_UNKNOWN, 0, NULL,
	    g95_check_min_max, g95_simplify_max, g95_resolve_min_max,
	    a1, BT_UNKNOWN, 0, IARG_NONE,
	    a2, BT_UNKNOWN, 0, IARG_NONE, NULL);

    add_sym("max0", 1, G95_ISYM_MAX, BT_INTEGER, di1, NULL,
	    g95_check_min_max_integer, g95_simplify_max, g95_resolve_min_max,
	    a1, BT_INTEGER, di1, IARG_NONE,
	    a2, BT_INTEGER, di1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("amax0", 1, G95_ISYM_MAX, BT_REAL, dr1, NULL,
	    g95_check_min_max_integer, g95_simplify_max, g95_resolve_min_max,
	    a1, BT_INTEGER, di1, IARG_NONE,
	    a2, BT_INTEGER, di1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("amax1", 1, G95_ISYM_MAX, BT_REAL, dr1, NULL,
	    g95_check_min_max_real, g95_simplify_max, g95_resolve_min_max,
	    a1, BT_REAL, dr1, IARG_NONE,
	    a2, BT_REAL, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("max1", 1, G95_ISYM_MAX, BT_INTEGER, di1, NULL,
	    g95_check_min_max_real, g95_simplify_max, g95_resolve_min_max,
	    a1, BT_REAL, dr1, IARG_NONE,
	    a2, BT_REAL, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("dmax1", 1, G95_ISYM_MAX, BT_REAL, dd, NULL,
	    g95_check_min_max_double, g95_simplify_max, g95_resolve_min_max,
	    a1, BT_REAL, dd, IARG_NONE,
	    a2, BT_REAL, dd, IARG_NONE, NULL);

    make_generic("max");

    add_sym("maxexponent", 0, G95_ISYM_MAXEXPONENT, BT_INTEGER, di1, NULL,
	    g95_check_x_ni, g95_simplify_maxexponent, NULL,
	    x, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("maxexponent");

    add_sym("maxloc", 0, G95_ISYM_MAXLOC, BT_INTEGER, di1, NULL,
	    g95_check_minloc_maxloc, NULL, g95_resolve_maxloc,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    msk, BT_LOGICAL, dl, IARG_OPTIONAL | IARG_ARRAYDESC, NULL);

    make_generic("maxloc");

    add_sym("maxval", 0, G95_ISYM_MAXVAL, BT_UNKNOWN, 0, NULL,
	    g95_check_minval_maxval, NULL, g95_resolve_maxval,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    msk, BT_LOGICAL, dl, IARG_OPTIONAL | IARG_ARRAYDESC, NULL);

    make_generic("maxval");

    add_sym("merge", 1, G95_ISYM_MERGE, BT_UNKNOWN, 0, NULL,
	    g95_check_merge, g95_simplify_merge, g95_resolve_merge,
	    ts, BT_UNKNOWN, 0, IARG_NONE,
	    fs, BT_UNKNOWN, 0, IARG_NONE,
	    msk, BT_LOGICAL, dl, IARG_NONE, NULL);

    make_generic("merge");

/* Note: amin0 is equivalent to real(min), min1 is equivalent to int(min). */

    add_sym("min", 1, G95_ISYM_MIN, BT_UNKNOWN, 0, NULL,
	    g95_check_min_max, g95_simplify_min, g95_resolve_min_max,
	    a1, BT_REAL, dr0, IARG_NONE,
	    a2, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("min0", 1, G95_ISYM_MIN, BT_INTEGER, di1, NULL,
	    g95_check_min_max_integer, g95_simplify_min, g95_resolve_min_max,
	    a1, BT_INTEGER, di1, IARG_NONE,
	    a2, BT_INTEGER, di1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("amin0", 1, G95_ISYM_MIN, BT_REAL, dr1, NULL,
	    g95_check_min_max_integer, g95_simplify_min, g95_resolve_min_max,
	    a1, BT_INTEGER, di1, IARG_NONE,
	    a2, BT_INTEGER, di1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("amin1", 1, G95_ISYM_MIN, BT_REAL, dr1, NULL,
	    g95_check_min_max_real, g95_simplify_min, g95_resolve_min_max,
	    a1, BT_REAL, dr1, IARG_NONE,
	    a2, BT_REAL, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("min1", 1, G95_ISYM_MIN, BT_INTEGER, di1, NULL,
	    g95_check_min_max_real, g95_simplify_min, g95_resolve_min_max,
	    a1, BT_REAL, dr1, IARG_NONE,
	    a2, BT_REAL, dr1, IARG_NONE, NULL);
    
    if (G95_STRICT_F())
	hide_symbol();

    add_sym("dmin1", 1, G95_ISYM_MIN, BT_REAL, dd, NULL,
	    g95_check_min_max_double, g95_simplify_min, g95_resolve_min_max,
	    a1, BT_REAL, dd, IARG_NONE,
	    a2, BT_REAL, dd, IARG_NONE, NULL);

    make_generic("min");

    add_sym("minexponent", 0, G95_ISYM_MINEXPONENT, BT_INTEGER, di1, NULL,
	    g95_check_x_ni, g95_simplify_minexponent, NULL,
	    x, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("minexponent");

    add_sym("minloc", 0, G95_ISYM_MINLOC, BT_INTEGER, di0, NULL,
	    g95_check_minloc_maxloc, NULL, g95_resolve_minloc,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    msk, BT_LOGICAL, dl, IARG_OPTIONAL | IARG_ARRAYDESC, NULL);

    make_generic("minloc");

    add_sym("minval", 0, G95_ISYM_MINVAL, BT_UNKNOWN, 0, NULL,
	    g95_check_minval_maxval, NULL, g95_resolve_minval,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    msk, BT_LOGICAL, dl, IARG_OPTIONAL | IARG_ARRAYDESC, NULL);

    make_generic("minval");

    if (!G95_STRICT_F()) {
	add_sym("mod", 1, G95_ISYM_MOD, BT_UNKNOWN, 0,
		g95_get_fstring(PREFIX "mod_i%d", di1),
		g95_check_a_p, g95_simplify_mod, g95_resolve_mod,
		a, BT_INTEGER, di1, IARG_NONE,
		p, BT_INTEGER, di1, IARG_NONE, NULL);

	add_sym("amod", 1, G95_ISYM_MOD, BT_REAL, dr0,
		g95_get_fstring(PREFIX "mod_r%d", dr0),
		NULL, g95_simplify_mod, g95_resolve_mod,
		a, BT_REAL, dr0, IARG_NONE,
		p, BT_REAL, dr0, IARG_NONE, NULL);

	add_sym("dmod", 1, G95_ISYM_MOD, BT_REAL, dd,
		g95_get_fstring(PREFIX "mod_r%d", dd),
		NULL, g95_simplify_mod, g95_resolve_mod,
		a, BT_REAL, dd, IARG_NONE,   p, BT_REAL, dd, IARG_NONE, NULL);

	add_sym("", 1, G95_ISYM_MOD, BT_REAL, dx, NULL,
		NULL, g95_simplify_mod, g95_resolve_mod,
		a, BT_REAL, dx, IARG_NONE,   p, BT_REAL, dx, IARG_NONE, NULL);

	hide_symbol();

	add_sym("", 1, G95_ISYM_MOD, BT_REAL, dq, NULL,
		NULL, g95_simplify_mod, g95_resolve_mod,
		a, BT_REAL, dq, IARG_NONE,   p, BT_REAL, dq, IARG_NONE, NULL);

	hide_symbol();

	make_generic("mod");
    }

    add_sym("modulo", 1, G95_ISYM_MODULO, BT_UNKNOWN, 0, NULL,
	    g95_check_a_p, g95_simplify_modulo, g95_resolve_modulo,
	    a, BT_REAL, di1, IARG_NONE,   p, BT_REAL, di1, IARG_NONE, NULL);

    make_generic("modulo");

    add_sym("nearest", 1, G95_ISYM_NEAREST, BT_UNKNOWN, 0, NULL,
	    g95_check_nearest, g95_simplify_nearest, g95_resolve_nearest,
	    x, BT_UNKNOWN, 0, IARG_NONE,   s, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("nearest");

    add_sym("nint", 1, G95_ISYM_NINT, BT_UNKNOWN, 0,
	    g95_get_fstring(PREFIX "nint_%d_r%d", di1, dr1),
	    g95_check_a_ikind, g95_simplify_nint, g95_resolve_nint,
	    a, BT_REAL, dr1, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    add_sym("idnint", 1, G95_ISYM_NINT, BT_INTEGER, di1,
	    g95_get_fstring(PREFIX "nint_%d_r%d", di1, dd),
	    g95_check_idnint, g95_simplify_idnint, g95_resolve_idnint,
	    a, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    make_generic("nint");

    add_sym("not", 1, G95_ISYM_NOT, BT_INTEGER, di1, NULL,
	    g95_check_i, g95_simplify_not, g95_resolve_not,
	    i, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("not");

    add_sym("null", 0, G95_ISYM_NULL, BT_UNKNOWN, 0, NULL,
	    g95_check_null, g95_simplify_null, NULL,
	    mo, BT_INTEGER, di1, 1, NULL);

    make_generic("null");

    add_sym("pack", 0, G95_ISYM_PACK, BT_UNKNOWN, 0, NULL,
	    g95_check_pack, NULL, g95_resolve_pack,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    msk, BT_LOGICAL, dl, IARG_ARRAYDESC,
	    v, BT_UNKNOWN, 0, IARG_OPTIONAL | IARG_ARRAYDESC, NULL);

    make_generic("pack");

    add_sym("precision", 0, G95_ISYM_PRECISION, BT_INTEGER, di1, NULL,
	    g95_check_precision, g95_simplify_precision, NULL,
	    x, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("precision");

    add_sym("present", 0, G95_ISYM_PRESENT, BT_LOGICAL, dl, NULL,
	    g95_check_present, NULL, NULL,
	    a, BT_UNKNOWN, 0, 0, NULL);

    make_generic("present");

    add_sym("product", 0, G95_ISYM_PRODUCT, BT_UNKNOWN, 0, NULL,
	    g95_check_product, NULL, g95_resolve_product,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    msk, BT_LOGICAL, dl, IARG_ARRAYDESC | IARG_OPTIONAL, NULL);

    make_generic("product");

    add_sym("radix", 0, G95_ISYM_RADIX, BT_INTEGER, di1, NULL,
	    g95_check_radix, g95_simplify_radix, NULL,
	    x, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("radix");

    add_sym("range", 0, G95_ISYM_RANGE, BT_INTEGER, di1, NULL,
	    g95_check_range, g95_simplify_range, NULL,
	    x, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("range");

    add_sym("real", 1, G95_ISYM_REAL, BT_UNKNOWN, 0, NULL,
	    g95_check_real, g95_simplify_real, g95_resolve_real,
	    a, BT_UNKNOWN, 0, IARG_NONE,
	    kind, BT_INTEGER, di1, IARG_OPTIONAL, NULL);

    add_sym("float", 1, G95_ISYM_REAL, BT_REAL, dr1, NULL,
	    NULL, g95_simplify_float, NULL,
	    a, BT_INTEGER, di1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("sngl", 1, G95_ISYM_REAL, BT_REAL, dr1, NULL,
	    NULL, g95_simplify_sngl, NULL,
	    a, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    make_generic("real");

    add_sym("repeat", 0, G95_ISYM_REPEAT, BT_CHARACTER, dc, NULL,
	    g95_check_repeat, g95_simplify_repeat, g95_resolve_repeat,
	    stg, BT_CHARACTER, dc, IARG_NONE,
	    n, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("repeat");

    add_sym("reshape", 0, G95_ISYM_RESHAPE, BT_UNKNOWN, 0, NULL,
	    g95_check_reshape, g95_simplify_reshape, g95_resolve_reshape,
	    src, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    shp, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    pad, BT_UNKNOWN, 0, IARG_ARRAYDESC | IARG_OPTIONAL,
	    ord, BT_UNKNOWN, 0, IARG_ARRAYDESC | IARG_OPTIONAL, NULL);

    make_generic("reshape");

    add_sym("rrspacing", 1, G95_ISYM_RRSPACING, BT_UNKNOWN, 0, NULL,
	    g95_check_x, g95_simplify_rrspacing, g95_resolve_rrspacing,
	    x, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("rrspacing");

    add_sym("scale", 1, G95_ISYM_SCALE, BT_UNKNOWN, 0, NULL,
	    g95_check_scale, g95_simplify_scale, g95_resolve_scale,
	    x, BT_UNKNOWN, 0, IARG_NONE,
	    i, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("scale");

    add_sym("scan", 1, G95_ISYM_SCAN, BT_INTEGER, di1, NULL,
	    g95_check_scan, g95_simplify_scan, g95_resolve_scan,
	    stg, BT_CHARACTER, dc, IARG_NONE,
	    set, BT_CHARACTER, dc, IARG_NONE,
	    bck, BT_LOGICAL, dl, IARG_OPTIONAL, NULL);

    make_generic("scan");

    add_sym("selected_int_kind", 0, G95_ISYM_SELECTED_INT_KIND,
	    BT_INTEGER, di0, NULL,
	    g95_check_selected_int_kind, g95_simplify_selected_int_kind, NULL,
	    r, BT_INTEGER, 0, IARG_NONE, NULL);

    make_generic("selected_int_kind");

    add_sym("selected_real_kind", 0, G95_ISYM_SELECTED_REAL_KIND,
	    BT_INTEGER, di0, NULL,
	    g95_check_selected_real_kind, g95_simplify_selected_real_kind,NULL,
	    p, BT_INTEGER, di0, IARG_OPTIONAL,
	    r, BT_INTEGER, di0, IARG_OPTIONAL, NULL);

    make_generic("selected_real_kind");

    add_sym("set_exponent", 1, G95_ISYM_SET_EXPONENT, BT_REAL, dr1, NULL,
	    g95_check_set_exponent, g95_simplify_set_exponent,
	    g95_resolve_set_exponent,
	    x, BT_REAL, dr1, IARG_NONE,
	    i, BT_INTEGER, di0, IARG_NONE, NULL);

    make_generic("set_exponent");

    add_sym("shape", 0, G95_ISYM_SHAPE, BT_INTEGER, di0, NULL,
	    g95_check_shape, g95_simplify_shape, g95_resolve_shape,
	    src, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    kind, BT_INTEGER, di0, IARG_OPTIONAL, NULL);

    make_generic("shape");

    add_sym("sign", 1, G95_ISYM_SIGN, BT_UNKNOWN, 0,
	    g95_get_fstring(PREFIX "sign_r%d", dr1),
	    g95_check_sign, g95_simplify_sign, g95_resolve_sign,
	    a, BT_UNKNOWN, 0, IARG_NONE,   b, BT_UNKNOWN, 0, IARG_NONE, NULL);

    add_sym("isign", 1, G95_ISYM_SIGN, BT_INTEGER, di1,
	    g95_get_fstring(PREFIX "sign_i%d", di1),
	    NULL, g95_simplify_sign, g95_resolve_sign,
	    a, BT_INTEGER, di1, IARG_NONE,
	    b, BT_INTEGER, di1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("dsign", 1, G95_ISYM_SIGN, BT_REAL, dd,
	    g95_get_fstring(PREFIX "sign_r%d", dd),
	    NULL, g95_simplify_sign, g95_resolve_sign,
	    a, BT_REAL, dd, IARG_NONE,   b, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    make_generic("sign");

    add_sym("sin", 1, G95_ISYM_SIN, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "sin_r%d", dr1),
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dsin", 1, G95_ISYM_SIN, BT_REAL, dd,
	    g95_get_fstring(PREFIX "sin_r%d", dd),
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("csin", 1, G95_ISYM_SIN, BT_COMPLEX, dr0,
	    g95_get_fstring(PREFIX "sin_z%d", dr1),
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_COMPLEX, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("zsin", 1, G95_ISYM_SIN, BT_COMPLEX, dd,
	    g95_get_fstring(PREFIX "sin_z%d", dd),
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_COMPLEX, dd, IARG_NONE, NULL);

    if (g95_option.fmode != 0)
	hide_symbol();

    make_alias("cdsin");

    add_sym("", 1, G95_ISYM_SIN, BT_REAL, dx, NULL,
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_SIN, BT_REAL, dq, NULL,
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_SIN, BT_COMPLEX, dx, NULL,
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_COMPLEX, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_SIN, BT_COMPLEX, dq, NULL,
	    NULL, g95_simplify_sin, g95_resolve_sin,
	    x, BT_COMPLEX, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("sin");

    add_sym("sinh", 1, G95_ISYM_SINH, BT_UNKNOWN, 0,
	    g95_get_fstring(PREFIX "sinh_r%d", dr1),
	    NULL, g95_simplify_sinh, g95_resolve_sinh,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dsinh", 1, G95_ISYM_SINH, BT_REAL, dd,
	    g95_get_fstring(PREFIX "sinh_r%d", dd),
	    NULL, g95_simplify_sinh, g95_resolve_sinh,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_SINH, BT_REAL, dx, NULL,
	    NULL, g95_simplify_sinh, g95_resolve_sinh,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_SINH, BT_REAL, dq, NULL,
	    NULL, g95_simplify_sinh, g95_resolve_sinh,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("sinh");

    add_sym("size", 0, G95_ISYM_SIZE, BT_INTEGER, di0, NULL,
	    g95_check_size, g95_simplify_size, g95_resolve_size,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    kind, BT_INTEGER, di0, IARG_OPTIONAL, NULL);

    make_generic("size");

    if (g95_option.fmode == 0)
	add_sym("sizeof", 0, G95_ISYM_SIZEOF, BT_INTEGER, di0, NULL,
		g95_check_sizeof, NULL, NULL,
		"object", BT_UNKNOWN, 0, IARG_NONE, NULL);

    add_sym("spacing", 1, G95_ISYM_SPACING, BT_UNKNOWN, 0, NULL,
	    g95_check_x, g95_simplify_spacing, g95_resolve_spacing,
	    x, BT_UNKNOWN, 0, IARG_NONE, NULL);

    make_generic("spacing");

    add_sym("spread", 0, G95_ISYM_SPREAD, BT_UNKNOWN, 0, NULL,
	    g95_check_spread, NULL, g95_resolve_spread,
	    src, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_NONE,
	    n, BT_INTEGER, di1, IARG_NONE, NULL);

    make_generic("spread");

    add_sym("sqrt", 1, G95_ISYM_SQRT, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "sqrt_r%d", dr1),
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dsqrt", 1, G95_ISYM_SQRT, BT_REAL, dd,
	    g95_get_fstring(PREFIX "sqrt_r%d", dd),
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("csqrt", 1, G95_ISYM_SQRT, BT_COMPLEX, dr0,
	    g95_get_fstring(PREFIX "sqrt_z%d", dr1),
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_COMPLEX, dr1, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("zsqrt", 1, G95_ISYM_SQRT, BT_COMPLEX, dd,
	    g95_get_fstring(PREFIX "sqrt_z%d", dd),
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_COMPLEX, dd, IARG_NONE, NULL);

    if (g95_option.fmode != 0)
	hide_symbol();

    make_alias("cdsqrt");

    add_sym("", 1, G95_ISYM_SQRT, BT_REAL, dx, NULL,
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_SQRT, BT_REAL, dq, NULL,
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_SQRT, BT_COMPLEX, dx, NULL,
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_COMPLEX, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_SQRT, BT_COMPLEX, dq, NULL,
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt,
	    x, BT_COMPLEX, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("sqrt");

    add_sym("sum", 0, G95_ISYM_SUM, BT_UNKNOWN, 0, NULL,
	    g95_check_sum, NULL, g95_resolve_sum,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di1, IARG_OPTIONAL,
	    msk, BT_LOGICAL, dl, IARG_OPTIONAL | IARG_ARRAYDESC, NULL);

    make_generic("sum");

    add_sym("tan", 1, G95_ISYM_TAN, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "tan_r%d", dr1),
	    NULL, g95_simplify_tan, g95_resolve_tan,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dtan", 1, G95_ISYM_TAN, BT_REAL, dd,
	    g95_get_fstring(PREFIX "tan_r%d", dd),
	    NULL, g95_simplify_tan, g95_resolve_tan,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_TAN, BT_REAL, dx, NULL,
	    NULL, g95_simplify_tan, g95_resolve_tan,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_TAN, BT_REAL, dq, NULL,
	    NULL, g95_simplify_tan, g95_resolve_tan,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("tan");

    add_sym("tanh", 1, G95_ISYM_TANH, BT_REAL, dr1,
	    g95_get_fstring(PREFIX "tanh_r%d", dr1),
	    NULL, g95_simplify_tanh, g95_resolve_tanh,
	    x, BT_REAL, dr0, IARG_NONE, NULL);

    add_sym("dtanh", 1, G95_ISYM_TANH, BT_REAL, dd,
	    g95_get_fstring(PREFIX "tanh_r%d", dd),
	    NULL, g95_simplify_tanh, g95_resolve_tanh,
	    x, BT_REAL, dd, IARG_NONE, NULL);

    if (G95_STRICT_F())
	hide_symbol();

    add_sym("", 1, G95_ISYM_TANH, BT_REAL, dx, NULL,
	    NULL, g95_simplify_tanh, g95_resolve_tanh,
	    x, BT_REAL, dx, IARG_NONE, NULL);

    hide_symbol();

    add_sym("", 1, G95_ISYM_TANH, BT_REAL, dq, NULL,
	    NULL, g95_simplify_tanh, g95_resolve_tanh,
	    x, BT_REAL, dq, IARG_NONE, NULL);

    hide_symbol();

    make_generic("tanh");

    add_sym("tiny", 0, G95_ISYM_TINY, BT_UNKNOWN, 0, NULL,
	    g95_check_x_ni, g95_simplify_tiny, NULL,
	    x, BT_REAL, 0, IARG_NONE, NULL);

    make_generic("tiny");

    if (!G95_STRICT_F()) {
	add_sym("transfer", 0, G95_ISYM_TRANSFER, BT_UNKNOWN, 0, NULL,
		g95_check_transfer, g95_simplify_transfer,g95_resolve_transfer,
		src, BT_UNKNOWN, 0, IARG_NONE,
		mo, BT_UNKNOWN, 0, IARG_NONE,
		sz, BT_INTEGER, di1, 1,  NULL);

	make_generic("transfer");
    }

    add_sym("transpose", 0, G95_ISYM_TRANSPOSE, BT_UNKNOWN, 0, NULL,
	    g95_check_transpose, g95_simplify_transpose, g95_resolve_transpose,
	    m, BT_UNKNOWN, 0, IARG_ARRAYDESC, NULL);

    make_generic("transpose");

    add_sym("trim", 0, G95_ISYM_TRIM, BT_CHARACTER, dc, NULL,
	    g95_check_trim, g95_simplify_trim, g95_resolve_trim,
	    stg, BT_CHARACTER, dc, IARG_NONE, NULL);

    make_generic("trim");

    add_sym("ubound", 0, G95_ISYM_UBOUND, BT_INTEGER, di0, NULL,
	    g95_check_ubound, g95_simplify_ubound, g95_resolve_ubound,
	    ar, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    dm, BT_INTEGER, di0, IARG_OPTIONAL,
	    kind, BT_INTEGER, di0, IARG_OPTIONAL, NULL);

    make_generic("ubound");

    add_sym("unpack", 0, G95_ISYM_UNPACK, BT_UNKNOWN, 0, NULL,
	    g95_check_unpack, NULL, g95_resolve_unpack,
	    v, BT_UNKNOWN, 0, IARG_ARRAYDESC,
	    msk, BT_LOGICAL, dl, IARG_ARRAYDESC,
	    f, BT_UNKNOWN, 0, IARG_ARRAYDESC, NULL);

    make_generic("unpack");

    add_sym("verify", 1, G95_ISYM_VERIFY, BT_INTEGER, di0, NULL,
	    g95_check_verify, g95_simplify_verify, g95_resolve_verify,
	    stg, BT_CHARACTER, dc, IARG_NONE,
	    set, BT_CHARACTER, dc, IARG_NONE,
	    bck, BT_LOGICAL, dl, IARG_OPTIONAL, NULL);

    make_generic("verify");

    if (g95_option.fmode == 0 || g95_option.fmode == 2003)
	add_f2003_functions();

    if (g95_option.fmode == 0 || g95_option.intrinsic_extensions)
	add_function_extensions();

    add_f2008_functions();
}



/* add_f2003_subroutines()-- Add F2003 subroutines to the symbol table. */

static void add_f2003_subroutines(void) {
int di0, di1, dr, dc, dl;

    di0 = g95_default_integer_kind(0);
    di1 = g95_default_integer_kind(1);
    dr  = g95_default_real_kind(1);
    dc  = g95_default_character_kind();
    dl  = g95_default_logical_kind();

    add_sym("get_command", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "command", BT_CHARACTER, dc, IARG_OPTIONAL,
	    "length", BT_INTEGER, di0, IARG_OPTIONAL,
	    "status", BT_INTEGER, di0, IARG_OPTIONAL, NULL);

    add_sym("get_command_argument", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0,NULL,
	    NULL, NULL, NULL,
	    "number", BT_INTEGER, di0, IARG_NONE,
	    "value", BT_CHARACTER, dc, IARG_OPTIONAL | IARG_INTENT_OUT,
	    "length", BT_INTEGER, di0, IARG_OPTIONAL | IARG_INTENT_OUT,
	    "status", BT_INTEGER, di0, IARG_OPTIONAL | IARG_INTENT_OUT, NULL);

    add_sym("get_environment_variable", 0, G95_ISYM_EXTENSION, BT_UNKNOWN,0,
	    NULL,
	    NULL, NULL, NULL,
	    "name", BT_CHARACTER, dc, IARG_NONE,
	    "value", BT_CHARACTER, dc, IARG_OPTIONAL | IARG_INTENT_OUT,
	    "length", BT_INTEGER, di1, IARG_OPTIONAL | IARG_INTENT_OUT,
	    "status", BT_INTEGER, di1, IARG_OPTIONAL | IARG_INTENT_OUT,
	    "trim_name", BT_LOGICAL, dl, IARG_OPTIONAL, NULL);

    add_sym("move_alloc", 0, G95_ISYM_MOVE_ALLOC, BT_UNKNOWN, 0, NULL,
	    g95_check_move_alloc, NULL, NULL,
	    "from", BT_UNKNOWN, 0, IARG_INTENT_INOUT | IARG_ARRAYDESC,
	    "to",   BT_UNKNOWN, 0, IARG_INTENT_OUT | IARG_ARRAYDESC, NULL);
}



/* add_subroutine_extensions()-- Add some semi-standard intrinsic
 * subroutine extensions. */

static void add_subroutine_extensions(void) {
int di0, di1, dr, dc;

    di0 = g95_default_integer_kind(0);
    di1 = g95_default_integer_kind(1);
    dc = g95_default_character_kind();
    dr = g95_default_real_kind(1);
    extension_flag = 1;

    add_sym("abort", 1, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL, NULL);

    add_sym("chdir", 1, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "dir", BT_CHARACTER, dc, IARG_NONE, NULL);

    add_sym("dtime", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "tarray", BT_REAL, dr, IARG_INTENT_OUT,
	    "result", BT_REAL, dr, IARG_INTENT_OUT | IARG_OPTIONAL, NULL);

    add_sym("etime", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "tarray", BT_REAL, dr, IARG_INTENT_OUT,
	    "result", BT_REAL, dr, IARG_INTENT_OUT | IARG_OPTIONAL, NULL);

    add_sym("exit", 0, G95_ISYM_EXTENSION, BT_INTEGER, di0, NULL,
	    g95_check_exit, NULL, g95_resolve_exit,
	    "code", BT_INTEGER, di0, IARG_OPTIONAL, NULL);

    add_sym("fdate", 1, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "date", BT_CHARACTER, dc, IARG_INTENT_OUT, NULL);

    add_sym("flush", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "unit", BT_INTEGER, di0, IARG_NONE, NULL);

    if (add_sym("fstat", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		NULL, NULL, NULL,
		"unit", BT_INTEGER, di1, IARG_NONE,
		"sarray", BT_INTEGER, di1, IARG_INTENT_OUT,
		"status", BT_INTEGER, di1, IARG_INTENT_OUT, NULL))
	library_name(PREFIX "fstat_s");

    add_sym("gerror", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "message", BT_CHARACTER, dc, IARG_INTENT_OUT, NULL);

    add_sym("getarg", 1, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    g95_check_getarg, NULL, g95_resolve_getarg,
	    "n", BT_INTEGER, di1, IARG_NONE,
	    "arg", BT_CHARACTER, dc, IARG_INTENT_OUT, NULL);

    add_sym("getenv", 1, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "variable", BT_CHARACTER, dc, IARG_NONE,
	    "value", BT_CHARACTER, dc, IARG_INTENT_OUT, NULL);

    add_sym("getlog", 1, G95_ISYM_EXTENSION, BT_INTEGER, dr, NULL,
	    NULL, NULL, NULL,
	    "name", BT_CHARACTER, dc, IARG_INTENT_OUT, NULL);

    add_sym("idate", 1, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "m", BT_INTEGER, di0, IARG_INTENT_OUT,
	    "d", BT_INTEGER, di0, IARG_INTENT_OUT,
	    "y", BT_INTEGER, di0, IARG_INTENT_OUT, NULL);

    add_sym("itime", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "tarray", BT_INTEGER, di0, IARG_INTENT_OUT, NULL);

    if (add_sym("lstat", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		NULL, NULL, NULL,
		"file", BT_CHARACTER, dc, IARG_NONE,
		"sarray", BT_INTEGER, di1, IARG_INTENT_OUT,
		"status", BT_INTEGER, di1, IARG_INTENT_OUT, NULL))
	library_name(PREFIX "lstat_s");

    add_sym("ltime", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "stime", BT_INTEGER, di0, IARG_NONE,
	    "tarray", BT_INTEGER, di0, IARG_INTENT_OUT, NULL);

    if (add_sym("rename", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		NULL, NULL, NULL,
		"path1", BT_CHARACTER, dc, IARG_NONE,
		"path2", BT_CHARACTER, dc, IARG_NONE,
		"status", BT_INTEGER, di0, IARG_INTENT_OUT|IARG_OPTIONAL,NULL))
	library_name(PREFIX "rename_s");

    if (add_sym("second", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		NULL, NULL, NULL,
		"time", BT_REAL, dr, IARG_INTENT_OUT,
		NULL))
	library_name(PREFIX "second_s");

    if (add_sym("signal", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		g95_check_signal, NULL, NULL,
		"signal", BT_INTEGER, di0, IARG_NONE,
		"handler", BT_PROCEDURE, 0, IARG_NONE,
		"status", BT_INTEGER, di0, IARG_INTENT_OUT | IARG_OPTIONAL,
		NULL))
	library_name(PREFIX "signal_s");

    add_sym("srand", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
	    NULL, NULL, NULL,
	    "seed", BT_INTEGER, di0, IARG_NONE, NULL);

    if (add_sym("system", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		NULL, NULL, NULL,
		"cmd", BT_CHARACTER, dc, IARG_NONE,
		"result", BT_INTEGER, di0, IARG_OPTIONAL | IARG_INTENT_OUT,
		NULL))
	library_name(PREFIX "system_s");

    if (add_sym("stat", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		NULL, NULL, NULL,
		"file", BT_CHARACTER, dc, IARG_NONE,
		"sarray", BT_INTEGER, di1, IARG_INTENT_OUT,
		"status", BT_INTEGER, di1, IARG_INTENT_OUT, NULL))
	library_name(PREFIX "stat_s");

    if (add_sym("unlink", 0, G95_ISYM_EXTENSION, BT_UNKNOWN, 0, NULL,
		NULL, NULL, NULL,
		"file", BT_CHARACTER, dc, IARG_NONE,
		"status", BT_INTEGER, di0, IARG_INTENT_OUT | IARG_OPTIONAL,
		NULL))
	library_name(PREFIX "unlink_s");

    extension_flag = 0;
}



/* add_subroutines()-- Add intrinsic subroutines */

static void add_subroutines(void) {

/* Argument names as in the standard (to be used as argument keywords) */
char   h[] = "harvest", dt[] = "date", vl[] = "values",  pt[] = "put",
       c[] = "count",   tm[] = "time", tp[] = "topos",   gt[] = "get",
       t[] = "to",      zn[] = "zone", fp[] = "frompos", cm[] = "count_max",
       f[] = "from",    sz[] = "size", ln[] = "len",     cr[] = "count_rate";

int di0, di1, dr, dc, dl;

    di0 = g95_default_integer_kind(0);
    di1 = g95_default_integer_kind(1);
    dr  = g95_default_real_kind(1);
    dc  = g95_default_character_kind();
    dl  = g95_default_logical_kind();

    add_sym("cpu_time", 0, G95_ISYM_CPU_TIME, BT_UNKNOWN, 0, NULL,
	    g95_check_cpu_time, NULL, g95_resolve_cpu_time,
	    tm, BT_REAL, dr, IARG_INTENT_OUT, NULL);

    add_sym("date_and_time", 0, G95_ISYM_DATE_AND_TIME, BT_UNKNOWN, 0, NULL,
	    g95_check_date_and_time, NULL, NULL,
	    dt, BT_CHARACTER, dc, IARG_OPTIONAL | IARG_INTENT_OUT,
	    tm, BT_CHARACTER, dc, IARG_OPTIONAL | IARG_INTENT_OUT,
	    zn, BT_CHARACTER, dc, IARG_OPTIONAL | IARG_INTENT_OUT,
	    vl, BT_INTEGER,   di0,
	      IARG_OPTIONAL | IARG_ARRAYDESC | IARG_INTENT_OUT, NULL);

    add_sym("mvbits", 1, G95_ISYM_MVBITS, BT_UNKNOWN, 0, NULL,
	    g95_check_mvbits, NULL, NULL,
	    f,  BT_INTEGER, di0, IARG_NONE,
	    fp, BT_INTEGER, di0, IARG_NONE,
	    ln, BT_INTEGER, di0, IARG_NONE,
	    t,  BT_INTEGER, di0, IARG_INTENT_INOUT,
	    tp, BT_INTEGER, di0, IARG_NONE, NULL);

    add_sym("random_number", 0, G95_ISYM_RANDOM_NUMBER, BT_UNKNOWN, 0, NULL,
	    g95_check_random_number, NULL, g95_resolve_random_number,
	    h, BT_REAL, dr, IARG_INTENT_OUT, NULL);

    add_sym("random_seed", 0, G95_ISYM_RANDOM_SEED, BT_UNKNOWN, 0, NULL,
	    g95_check_random_seed, NULL, NULL,
	    sz, BT_INTEGER, di0, IARG_OPTIONAL | IARG_INTENT_OUT,
	    pt, BT_INTEGER, di0, IARG_OPTIONAL | IARG_ARRAYDESC,
	    gt, BT_INTEGER, di0,
	      IARG_OPTIONAL | IARG_INTENT_OUT | IARG_ARRAYDESC, NULL);

    add_sym("system_clock", 0, G95_ISYM_SYSTEM_CLOCK, BT_UNKNOWN, 0, NULL,
	    g95_check_system_clock, NULL, NULL,
	    c,  BT_INTEGER, di0, IARG_INTENT_OUT | IARG_OPTIONAL,
	    cr, BT_INTEGER, di0, IARG_INTENT_OUT | IARG_OPTIONAL,
	    cm, BT_INTEGER, di0, IARG_INTENT_OUT | IARG_OPTIONAL, NULL);

    if (g95_option.fmode == 0 || g95_option.fmode == 2003)
	add_f2003_subroutines();

    if (g95_option.fmode == 0 || g95_option.intrinsic_extensions)
	add_subroutine_extensions();
}


/* add_conv()-- Add a function to the list of conversion symbols */

static void add_conv(bt from_type, int from_kind, bt to_type, int to_kind,
		     g95_expr *(*simplify)()) {

g95_typespec from, to;
g95_intrinsic_sym *sym;

    if (sizing == SZ_CONVS) {
	nconv++;
	return;
    }

    g95_clear_ts(&from);
    from.type = from_type;
    from.kind = from_kind;

    g95_clear_ts(&to);
    to.type = to_type;
    to.kind = to_kind;

    sym = conversion + nconv;

    sym->name = conv_name(&from, &to);
    sym->lib_name = sym->name;
    sym->simplify = simplify;
    sym->elemental = 1;
    sym->ts = to;
    sym->id = G95_ISYM_CONVERSION;

    nconv++;
}



/* add_conversions()-- Create g95_intrinsic_sym nodes for all intrinsic
 * conversion functions by looping over the kind tables. */

static void add_conversions(void) {
int i, j;

  /* Integer-Integer conversions */

    for(i=0; g95_integer_kinds[i].kind != 0; i++)
	for(j=0; g95_integer_kinds[j].kind != 0; j++) {
	    if (i != j)
		add_conv(BT_INTEGER, g95_integer_kinds[i].kind,
			 BT_INTEGER, g95_integer_kinds[j].kind,
			 convert_constant);
	}

    /* Integer-Real/Complex conversions */

    for(i=0; g95_integer_kinds[i].kind != 0; i++)
	for(j=0; g95_real_kinds[j].kind != 0; j++) {
	    add_conv(BT_INTEGER, g95_integer_kinds[i].kind,
		     BT_REAL,    g95_real_kinds[j].kind, convert_constant);

	    add_conv(BT_REAL,    g95_real_kinds[j].kind,
		     BT_INTEGER, g95_integer_kinds[i].kind, convert_constant);

	    add_conv(BT_INTEGER, g95_integer_kinds[i].kind,
		     BT_COMPLEX, g95_real_kinds[j].kind, convert_constant);

	    add_conv(BT_COMPLEX, g95_real_kinds[j].kind,
		     BT_INTEGER, g95_integer_kinds[i].kind, convert_constant);
	}

    /* Real/Complex - Real/Complex conversions */

    for(i=0; g95_real_kinds[i].kind != 0; i++)
	for(j=0; g95_real_kinds[j].kind != 0; j++) {
	    if (i != j) {
		add_conv(BT_REAL, g95_real_kinds[i].kind,
			 BT_REAL, g95_real_kinds[j].kind, convert_constant);

		add_conv(BT_COMPLEX, g95_real_kinds[i].kind,
			 BT_COMPLEX, g95_real_kinds[j].kind, convert_constant);
	    }

	    add_conv(BT_REAL,    g95_real_kinds[i].kind,
		     BT_COMPLEX, g95_real_kinds[j].kind, convert_constant);

	    add_conv(BT_COMPLEX, g95_real_kinds[i].kind,
		     BT_REAL,    g95_real_kinds[j].kind, convert_constant);
	}

    /* Logical/Logical kind conversion */

    for(i=0; g95_logical_kinds[i].kind; i++)
	for(j=0; g95_logical_kinds[j].kind; j++)
	    if (i != j)
		add_conv(BT_LOGICAL, g95_logical_kinds[i].kind,
			 BT_LOGICAL, g95_logical_kinds[j].kind,
			 convert_constant);
}



/* add_modproc()-- Add an intrinsic module procedure */

static void add_modproc(internal_proc proc, ...) {
int first_flag, flags;
va_list argp;
char *name;

    va_start(argp, proc);
    first_flag = 1;

    for(;;) {
	name = va_arg(argp, char *);
	if (name == NULL)
	    break;

	flags = va_arg(argp, int);

	if (sizing != SZ_NOTHING) {
	    nargs++;
	    continue;
	}

	if (first_flag)
	    modproc[proc].args = next_arg;

	else
	    (next_arg-1)->next = next_arg;

	first_flag = 0;
	next_arg->name = g95_get_string(name);
	next_arg->optional = !!(flags & IARG_OPTIONAL);

	next_arg->intent = (flags & IARG_INTENT_INOUT)
	    ? INTENT_INOUT
	    : ((flags & IARG_INTENT_OUT)
	       ? INTENT_OUT
	       : INTENT_IN);

	next_arg++;
    }

    va_end(argp);
    next_sym++;
}



/* add_modprocs()-- Add intrinsic module procedures. */

static void add_modprocs(void) {
static char x[] = "x";

    add_modproc(IPROC_C_LOC, x, 0, NULL);
    add_modproc(IPROC_C_FUNLOC, x, 0, NULL);
    add_modproc(IPROC_C_ASSOCIATED, "c_ptr1", 0,
		"c_ptr2", IARG_OPTIONAL, NULL);
    add_modproc(IPROC_C_F_POINTER, "cptr", 0, "fptr", IARG_INTENT_OUT,
		"shape", IARG_OPTIONAL, NULL);
    add_modproc(IPROC_C_F_PROCPOINTER, "cptr", 0,
		"fptr",IARG_INTENT_OUT, NULL);
    add_modproc(IPROC_IEEE_CLASS, x, 0, NULL);
    add_modproc(IPROC_IEEE_IS_NAN, x, 0, NULL);
    add_modproc(IPROC_IEEE_IS_NEGATIVE, x, 0, NULL);
    add_modproc(IPROC_IEEE_IS_FINITE, x, 0, NULL);
    add_modproc(IPROC_IEEE_IS_NORMAL, x, 0, NULL);
    add_modproc(IPROC_IEEE_VALUE, x, 0, "class", 0, NULL);

    add_modproc(IPROC_IEEE_SELECTED_REAL_KIND,
		"p", IARG_OPTIONAL, "q", IARG_OPTIONAL, NULL);

    add_modproc(IPROC_IEEE_SUPPORT_DATATYPE, x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_DENORMAL, x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_DIVIDE,   x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_INF,      x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_NAN,      x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_ROUNDING,
		"round", 0, x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_SQRT, x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_STANDARD, x, IARG_OPTIONAL, NULL);
    add_modproc(IPROC_IEEE_SUPPORT_UNDERFLOW_CONTROL,
		x, IARG_OPTIONAL, NULL);
}



/* g95_intrinsic_init_1()-- Initialize the table of intrinsics */

void g95_intrinsic_init_1(void) {
g95_intrinsic_sym *sym;
int i;

    nargs = nfunc = nsub = nconv = 0;

    sizing = SZ_FUNCS;
    add_functions();

    sizing = SZ_SUBS;
    add_subroutines();

    sizing = SZ_CONVS;
    add_conversions();

    sizing = SZ_MODPROC;
    add_modprocs();

    functions = g95_getmem(sizeof(g95_intrinsic_sym)*(nfunc+nsub));

    next_sym = functions;
    subroutines = functions + nfunc;

    conversion = g95_getmem(sizeof(g95_intrinsic_sym)*nconv);
    arguments = next_arg = g95_getmem(sizeof(g95_intrinsic_arg)*nargs);

    sizing = SZ_NOTHING;
    nconv = 0;

    add_functions();
    add_subroutines();
    add_conversions();
    add_modprocs();

    /* Set the pure flag.  All intrinsic functions are pure, and
     * intrinsic subroutines are pure if they are elemental. */

    for(i=0; i<nfunc; i++)
	functions[i].pure = 1;

    for(i=0; i<nsub; i++)
	subroutines[i].pure = subroutines[i].elemental;

    g95_unity_charlen.length = g95_int_expr(1);

    sym = g95_find_function("char");
    sym->ts.cl = &g95_unity_charlen;

    if (!G95_STRICT_F()) {
	sym = g95_find_function("achar");
	sym->ts.cl = &g95_unity_charlen;
    }

    sym = g95_find_function("repeat");
    sym->ts.cl = &g95_unknown_charlen;

    sym = g95_find_function("trim");
    sym->ts.cl = &g95_unknown_charlen;

    sym = g95_find_function("adjustl");
    sym->ts.cl = &g95_unknown_charlen;

    sym = g95_find_function("adjustr");
    sym->ts.cl = &g95_unknown_charlen;
}



void g95_intrinsic_done_1(void) {

    g95_free(functions);
    g95_free(conversion);
    g95_free(arguments);

    g95_free_expr(g95_unity_charlen.length);
}



/* g95_intrinsic_symbol()-- Given a symbol that we have decided is
 * intrinsic, mark it as such by placing it into a special module that
 * is otherwise impossible to read or write.  Returns nonzero if
 * something goes wrong. */

int g95_intrinsic_symbol(char *name, int function, g95_locus *where) {
g95_symbol *sym;

    g95_find_symbol(name, NULL, 1, &sym);
    if (sym == NULL || sym->attr.use_assoc)
	return 0;

    sym->module = g95_get_string("(intrinsic)");

    if (sym->attr.proc != PROC_INTRINSIC &&
	(g95_add_procedure(&sym->attr, PROC_INTRINSIC,
			   sym->name, where) == FAILURE))
	return 1;

    if (function) {
	if (!sym->attr.function &&
	    g95_add_function(&sym->attr, sym->name, where) == FAILURE)
	    return 1;

	if (sym->result == NULL)
	    sym->result = sym;

    } else {
	if (!sym->attr.subroutine &&
	    g95_add_subroutine(&sym->attr, sym->name, where) == FAILURE)
	    return 1;

	if (sym->ts.type == BT_UNKNOWN)
	    sym->ts.type = BT_PROCEDURE;
    }

    return 0;
}



/* g95_is_transformational()-- Given an intrinsic symbol, return
 * nonzero if the symbol is a transformational function. */

int g95_is_transformational(g95_intrinsic_sym *isym) {
int m;

    switch(isym->id) {
    case G95_ISYM_ALL:
    case G95_ISYM_ANY:
    case G95_ISYM_COUNT:
    case G95_ISYM_CSHIFT:
    case G95_ISYM_DOT_PRODUCT:
    case G95_ISYM_EOSHIFT:
    case G95_ISYM_MATMUL:
    case G95_ISYM_MAXLOC:
    case G95_ISYM_MAXVAL:
    case G95_ISYM_MINLOC:
    case G95_ISYM_MINVAL:
    case G95_ISYM_NULL:
    case G95_ISYM_PACK:
    case G95_ISYM_PRODUCT:
    case G95_ISYM_REPEAT:
    case G95_ISYM_RESHAPE:
    case G95_ISYM_SELECTED_INT_KIND:
    case G95_ISYM_SELECTED_REAL_KIND:
    case G95_ISYM_SPREAD:
    case G95_ISYM_SUM:
    case G95_ISYM_TRANSFER:
    case G95_ISYM_TRANSPOSE:
    case G95_ISYM_TRIM:
    case G95_ISYM_UNPACK:
	m = 1;
	break;

    default:
	m = 0;
    }

    return m;
}



/******** Subroutines to check intrinsic interfaces ***********/

/* remove_nullargs()-- Given an actual argument list, remove any
 * NULL arguments that may have been left behind by a sort against
 * some formal argument list. */

static void remove_nullargs(g95_actual_arglist **ap) {
g95_actual_arglist *head, *tail, *next;

    tail = NULL;

    for(head=*ap; head; head=next) {
	next = head->next;

	if (head->type == ARG_EXPR && head->u.expr == NULL) {
	    head->next = NULL;
	    g95_free_actual_arglist(head);

	} else {
	    if (tail == NULL)
		*ap = head;
	    else
		tail->next = head;

	    tail = head;
	    tail->next = NULL;
	}
    }

    if (tail == NULL)
	*ap = NULL;
}



/* sort_actual()-- Given an actual arglist and a formal arglist, sort
 * the actual arglist so that its arguments are in a one-to-one
 * correspondence with the format arglist.  Arguments that are not
 * present are given a blank g95_actual_arglist structure.  If
 * something is obviously wrong (say, a missing required argument) we
 * abort sorting and return FAILURE. */

static try sort_actual(char *name, g95_actual_arglist **ap,
		       g95_intrinsic_arg *formal, g95_locus *where) {

g95_actual_arglist *actual, *a;
g95_intrinsic_arg *f;
int i;

    remove_nullargs(ap);
    actual = *ap;

    for(f=formal; f; f=f->next)
	f->actual = NULL;

    f = formal;
    a = actual;

    if (f == NULL && a == NULL) /* No arguments */
	return SUCCESS;

    for(;;) {     /* Put the nonkeyword arguments in a 1:1 correspondence */
	if (f == NULL)
	    break;

	if (a == NULL)
	    goto optional;

	if (a->name != NULL)
	    goto keywords;

	f->actual = a;

	f = f->next;
	a = a->next;
    }

    if (a == NULL)
	goto do_sort;

    g95_error("Too many arguments in call to '%s' at %L", name, where);
    return FAILURE;

/* Associate the remaining actual arguments, all of which have to be
 * keyword arguments. */

keywords:
    for(; a; a=a->next) {
	for(f=formal, i=0; f; f=f->next, i++)
	    if (strcmp(a->name, f->name) == 0)
		break;

	if (f == NULL) {
	    g95_error("Can't find keyword named '%s' in call to '%s' at %L",
		      a->name, name, where);
	    return FAILURE;
	}

	if (f->actual != NULL) {
	    g95_error("Argument '%s' appears twice in call to '%s' at %L",
		      f->name, name, where);
	    return FAILURE;
	}

	f->actual = a;
    }

/* At this point, all unmatched formal args must be optional */

optional:
    for(f=formal; f; f=f->next) {
	if (f->actual == NULL && f->optional == 0) {
	    g95_error("Missing actual argument '%s' in call to '%s' at %L",
		      f->name, name, where);
	    return FAILURE;
	}
    }

/* Using the formal argument list, string the actual argument list
 * together in a way that corresponds with the formal list. */

do_sort:
    actual = NULL;

    for(f=formal; f; f=f->next) {
	a = (f->actual == NULL) ? g95_get_actual_arglist() : f->actual;

	if (a->type != ARG_ALT_RETURN && a->u.expr == NULL)
	    a->missing_arg_type = f->ts.type;

	if (actual == NULL)
	    *ap = a;

	else
	    actual->next = a;

	actual = a;
    }

    if (actual != NULL)
	actual->next = NULL;  /* End the sorted argument list. */

    return SUCCESS;
}



/* check_arglist()-- Compare an actual argument list with an
 * intrinsic's formal argument list.  The lists are checked for
 * agreement of type.  We don't check for arrayness here. */

static try check_arglist(g95_actual_arglist **ap, g95_intrinsic_sym *sym,
			 int error_flag) {
g95_actual_arglist *actual;
g95_intrinsic_arg *formal;
int i;

    formal = sym->formal;
    actual = *ap;

    i = 0;
    for(; formal; formal=formal->next, actual=actual->next, i++) {
	if (actual->u.expr == NULL)
	    continue;

	if (!g95_compare_types(&formal->ts, &actual->u.expr->ts)) {
	    if (error_flag)
		g95_error("Type of argument '%s' in call to '%s' at %L "
			  "should be %s, not %s", g95_current_intrinsic_arg[i],
			  g95_current_intrinsic, &actual->u.expr->where,
			  g95_typename(&formal->ts),
			  g95_typename(&actual->u.expr->ts));
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* resolve_intrinsic()-- Given a pointer to an intrinsic symbol and an
 * expression node that represent the function call to that
 * subroutine, figure out the type of the result.  This may involve
 * calling a resolution subroutine */

static void resolve_intrinsic(g95_intrinsic_sym *specific, g95_expr *e) {
g95_expr *a1, *a2, *a3, *a4, *a5;
g95_actual_arglist *arg;

    if (specific->resolve == NULL) {
	if (e->value.function.iname == NULL)
	    e->value.function.iname = specific->lib_name;

	if (e->ts.type == BT_UNKNOWN)
	    e->ts = specific->ts;

	goto done;
    }

    arg = e->value.function.actual;

/* At present only the iargc extension intrinsic takes no arguments,
 * and it doesn't need a resolution function, but this is here for
 * generality */

    if (arg == NULL) {
	(*specific->resolve)(e);
	goto done;
    }

    a1  = arg->u.expr;
    arg = arg->next;

    if (specific->resolve == g95_resolve_min_max) {
	g95_resolve_min_max(e, specific, a1);
	if (e->ts.type == BT_UNKNOWN)
	    e->ts = specific->ts;

	goto done;
    }

    if (arg == NULL) {
	(*specific->resolve)(e, a1);
	goto done;
    }

    a2  = arg->u.expr;
    arg = arg->next;

    if (arg == NULL) {
	(*specific->resolve)(e, a1, a2);
	goto done;
    }

    a3  = arg->u.expr;
    arg = arg->next;
      
    if (arg == NULL) {
	(*specific->resolve)(e, a1, a2, a3);
	goto done;
    }

    a4  = arg->u.expr;
    arg = arg->next;

    if (arg == NULL) {
	(*specific->resolve)(e, a1, a2, a3, a4);
	goto done;
    }

    a5  = arg->u.expr;
    arg = arg->next;

    if (arg == NULL) {
	(*specific->resolve)(e, a1, a2, a3, a4, a5);
	goto done;
    }

    g95_internal_error("resolve_intrinsic(): Too many args for intrinsic");

done:
    if (e->rank == -1)
	e->rank = 0;
}



/* examine_arglist()-- Examine the argument list.  Returns 0 if all
 * constructor arguments have no elements left, 1 if all constructor
 * arguments have at least one element left, and 2 otherwise (which is
 * an error). */

static int examine_arglist(g95_actual_arglist *a) {
int i, rc, seen_cons;

    seen_cons = 0;
    rc = 0;

    for(; a; a=a->next) {
	if (a->u.expr == NULL || a->u.expr->type != EXPR_ARRAY)
	    continue;

	i = (a->u.expr->value.constructor.c == NULL) ? 0 : 1;

	if (!seen_cons) {
	    seen_cons = 1;
	    rc = i;

	} else if (rc != i) {
	    g95_error("Arguments of elemental intrinsic at %L have "
		      "differing numbers of elements", &a->u.expr->where);
	    return 2;
	}
    }

    return rc;
}



/* simplify_elemental_intrinsic()-- Simplify an elemental intrinsic
 * that has an array constructor(s) as actual arguments.  Constructors
 * are expanded, and a new constructor is built the elements of which
 * are the function calls.  Returns nonzero if the caller does not
 * have anything more to do. */

static int simplify_elemental_intrinsic(g95_intrinsic_sym *sym, g95_expr *e,
					try *status) {
g95_actual_arglist *arg, *n, *tail_a;
g95_constructor *c, *head, *tail;
g95_expr *a, *f;
bignum *shape;
int rank;

    if (!sym->elemental)
	return 0;

    rank = 0;
    shape = NULL;

    for(arg=e->value.function.actual; arg; arg=arg->next) {
	a = arg->u.expr;
	if (a == NULL || a->rank == 0)
	    continue;

	switch(a->type) {
	case EXPR_VARIABLE:
	    if (a->symbol->attr.flavor != FL_PARAMETER)
		return 0;

	    g95_simplify_expr(a);

	    if (a->type != EXPR_ARRAY)
		return 0;

	    /* Fall through */

	case EXPR_ARRAY:
	    rank = a->rank;
	    if (shape == NULL)
		shape = a->shape;

	    break;

	default:
	    return 0;
	}
    }

    if (rank == 0)
	return 0;

    /* Expand the individual constructors */

    for(arg=e->value.function.actual; arg; arg=arg->next) {
	a = arg->u.expr;
	if (a == NULL)
	    continue;

	if (a->type == EXPR_ARRAY &&
	    g95_expand_constructor(a) == FAILURE)
	    return 0;
    }

    /* Build a new constructor */

    switch(examine_arglist(e->value.function.actual)) {
    case 0:
	head = NULL;
	goto done;

    case 2:
	*status = FAILURE;
	return 1;

    case 1:
	break;
    }

    head = tail = NULL;

    for(;;) {
	f = g95_get_expr();
	f->type = EXPR_FUNCTION;
	f->where = e->where;

	f->value.function.isym = sym;
	f->value.function.iname = sym->name;
	f->ts = (sym->ts.type != BT_UNKNOWN) ? sym->ts : e->ts;
	f->rank = 0;
	
	tail_a = NULL;

	for(arg=e->value.function.actual; arg; arg=arg->next) {
	    n = g95_get_actual_arglist();

	    if (f->value.function.actual == NULL)
		f->value.function.actual = n;

	    else
		tail_a->next = n;

	    tail_a = n;

	    if (arg->u.expr == NULL)
		n->u.expr = NULL;

	    else if (arg->u.expr->type != EXPR_ARRAY)
		n->u.expr = g95_copy_expr(arg->u.expr);

	    else {
		c = arg->u.expr->value.constructor.c;
		n->u.expr = c->expr;
		arg->u.expr->value.constructor.c = c->next;

		g95_free(c);
	    }
	}

	if (head == NULL)
	    head = tail = g95_get_constructor();

	else {
	    tail->next = g95_get_constructor();
	    tail = tail->next;
	}

	tail->expr = f;

	switch(examine_arglist(e->value.function.actual)) {
	case 0:
	    goto done;

	case 1:
	    break;

	case 2:
	    *status = FAILURE;
	    return 1;
	}
    }
    
done:
    f = g95_get_expr();
    f->type = EXPR_ARRAY;
    f->ts = (sym->ts.type != BT_UNKNOWN) ? sym->ts : e->ts;
    f->where = e->where;
    f->value.constructor.c = head;
    f->rank = rank;
    f->shape = shape;

    *status = SUCCESS;

    while(head != NULL) {
	if (do_simplify(sym, head->expr) == FAILURE) {
	    *status = FAILURE;
	    break;
	}

	head = head->next;
    }

    g95_replace_expr(e, f);
    return 1;
}



/* massage_sum_product()-- Massage an (array, dim, mask) argument list
 * that consists of (array, x, null) into (array, null, x) if x is a
 * logical (mask).  It affects the SUM, PRODUCT, MINVAL and MAXVAL
 * intrinsics. */

static void massage_sum_product(g95_actual_arglist *a) {
g95_actual_arglist *a2, *a3;

    a2 = a->next;
    if (a2 == NULL || a2->name != NULL || a2->u.expr == NULL ||
	a2->u.expr->ts.type != BT_LOGICAL)
	return;

    a3 = a2->next;
    if (a3 == NULL || a3->name != NULL || a3->u.expr != NULL)
	return;

    a3->u.expr = a2->u.expr;

    a2->u.expr = NULL;
    a2->type = ARG_EXPR;
}



/* do_simplify()-- Given an intrinsic symbol node and an expression
 * node, call the simplification function (if there is one), perhaps
 * replacing the expression with something simpler.  We return FAILURE
 * on an error of the simplification, SUCCESS if the simplification
 * worked, even if nothing has changed in the expression itself */

static try do_simplify(g95_intrinsic_sym *specific, g95_expr *e) {
g95_expr *result, *a1, *a2, *a3, *a4, *a5;
g95_actual_arglist *arg;
try t;

    simplified = 0;

    if (simplify_elemental_intrinsic(specific, e, &t))
	return t;

/* Max and min require special handling due to the variable number of args */

    if (specific->simplify == g95_simplify_min) {
	result = g95_simplify_min(e);
	goto finish;
    }

    if (specific->simplify == g95_simplify_max) {
	result = g95_simplify_max(e);
	goto finish;
    }

    if (specific->simplify == NULL) {
	result = NULL;
	goto finish;
    }

    arg = e->value.function.actual;

    a1  = arg->u.expr;
    arg = arg->next;

    if (specific->simplify == convert_constant) {
	result = convert_constant(a1, specific->ts.type, specific->ts.kind);
	goto finish;
    }

    if (arg == NULL)
	result = (*specific->simplify)(a1);

    else {
	a2 = arg->u.expr;
	arg = arg->next;

	if (arg == NULL)
	    result = (*specific->simplify)(a1, a2);

	else {
	    a3 = arg->u.expr;
	    arg = arg->next;
      
	    if (arg == NULL)
		result = (*specific->simplify)(a1, a2, a3);

	    else {
		a4 = arg->u.expr;
		arg = arg->next;

		if (arg == NULL)
		    result = (*specific->simplify)(a1, a2, a3, a4);

		else {
		    a5 = arg->u.expr;
		    arg = arg->next;

		    if (arg == NULL)
			result = (*specific->simplify)(a1, a2, a3, a4, a5);

		    else 
			g95_internal_error("do_simplify(): "
					   "Too many args for intrinsic");
		}
	    }
	}
    }

 finish:
    t = SUCCESS;

    if (result == &g95_bad_expr)
	t = FAILURE;

    else if (result != NULL) {
	result->where = e->where;
	g95_replace_expr(e, result);
	simplified = 1;

    } else
	resolve_intrinsic(specific, e);         /* Must call at run-time */

    return t;
}



/* init_arglist()-- Initialize the g95_current_intrinsic_arg[] array
 * for the benefit of error messages.  This subroutine returns FAILURE
 * if a subroutine has more than MAX_INTRINSIC_ARGS, in which case the
 * actual argument list cannot match any intrinsic. */

static void init_arglist(g95_intrinsic_sym *isym) {
g95_intrinsic_arg *formal;
int i;

    g95_current_intrinsic = isym->name;

    i = 0; 
    for(formal=isym->formal; formal; formal=formal->next) {
	if (i >= MAX_INTRINSIC_ARGS)
	    g95_internal_error("init_arglist(): too many arguments");

	g95_current_intrinsic_arg[i++] = formal->name;
    }
}



/* fix_array_args()-- Fix the argument type of array arguments for
 * intrinsics. */

static void fix_array_args(g95_intrinsic_arg *f, g95_actual_arglist *a) {

    while(f) {
	if (a->u.expr != NULL) {
	    if (g95_coarray_expr(a->u.expr) && f->array_desc)
		a->type = ARG_ARRAY_DESC;

	    else if (a->u.expr->rank > 0)
		a->type = f->array_desc ? ARG_ARRAY_DESC : ARG_ARRAY;
	}

        a->intent = f->intent;

	f = f->next;
	a = a->next;
    }
}



/* check_elemental_conformance()-- Check elemental conformance between
 * arguments. */

static try check_elemental_conformance(g95_intrinsic_sym *sym,
				       g95_actual_arglist *actual) {
g95_actual_arglist *a1, *a2;
char message[200];

    sprintf(message, "Argument to ELEMENTAL intrinsic '%s'", sym->name);

    for(a1=actual; a1; a1=a1->next) {
	if (a1->u.expr == NULL)
	    continue;

	for(a2=actual->next; a2; a2=a2->next) {
	    if (a2->u.expr == NULL)
		continue;

	    if (g95_check_conformance(message, a1->u.expr,a2->u.expr)==FAILURE)
		return FAILURE;
	}
    }

    return SUCCESS;
}



/* check_specific()-- Given a pointer to an intrinsic symbol and an
 * expression consisting of a function call, see if the function call
 * is consistent with the intrinsic's formal argument list.  Return
 * SUCCESS if the expression and intrinsic match, FAILURE otherwise.  */

static try check_specific(g95_intrinsic_sym *specific, g95_expr *expr,
			  int error_flag) {
g95_actual_arglist **ap;
try t;

    ap = &expr->value.function.actual;

    init_arglist(specific);

/* Don't attempt to sort the argument list for min or max */

    if (specific->check == g95_check_min_max ||
	specific->check == g95_check_min_max_integer ||
	specific->check == g95_check_min_max_real ||
	specific->check == g95_check_min_max_double)
	return (*specific->check)(*ap);

    if (sort_actual(specific->name, ap, specific->formal,
		    &expr->where) == FAILURE)
	return FAILURE;

    if (specific->check == g95_check_sum ||
	specific->check == g95_check_product ||
	specific->check == g95_check_minval_maxval)
	massage_sum_product(*ap);
    
    /* sort_actual() can get minloc() and maxloc() slightly wrong, so
     * this is fixed in g95_check_minloc_maxloc() */

    if (specific->check == g95_check_minloc_maxloc)
	t = g95_check_minloc_maxloc(*ap);

    else {
	if (specific->check == NULL) {
	    t = check_arglist(ap, specific, error_flag);
	    if (t == SUCCESS)
		expr->ts = specific->ts;

	} else
	    t = do_check(specific, *ap);
    }

    if (t == FAILURE)
	remove_nullargs(ap);

    else
	fix_array_args(specific->formal, expr->value.function.actual);

    return t;
}



/* arg_usage()-- Mark actual arguments as used or set according to the
 * intent of the formal argument.  By default, intrinsic arguments are
 * INTENT(IN). */

static void arg_usage(g95_intrinsic_arg *f, g95_actual_arglist *a) {
g95_expr *e;

    for(; f; f=f->next, a=a->next) {
	e = a->u.expr;
	if (e == NULL)
	    continue;

	if (e->type == EXPR_OP && e->value.op.operator == INTRINSIC_PAREN) {
	    e = e->value.op.op1;
	    g95_free(a->u.expr);
	    a->u.expr = e;
	}

	if (f->intent == INTENT_OUT || f->intent == INTENT_INOUT) {
	    if (e->type != EXPR_VARIABLE)
		g95_error("Argument '%s' at %L must be a VARIABLE", f->name,
			  &e->where);

	    else if (e->symbol->attr.intent == INTENT_IN)
		g95_error("Argument '%s' at %L is INTENT(IN)",
			  f->name, &e->where);
	}

	g95_arg_usage(f->intent, e);
    }
}



/* g95_has_alt_return()-- Checks an actual argument list to see if
 * there is an alternate return buried someplace in it.  Returns
 * nonzero if so, zero if an alternate return is present. */

int g95_has_alt_return(g95_actual_arglist *a) {

    while(a != NULL) {
	if (a->type == ARG_ALT_RETURN)
	    return 1;

	a = a->next;
    }

    return 0;
}



/* g95_intrinsic_func_interface()-- see if a function call corresponds
 * to an intrinsic function call.  We return:
 *  MATCH_YES    if the call corresponds to an intrinsic, simplification
 *               is done if possible.
 *
 *  MATCH_NO     if the call does not correspond to an intrinsic
 *
 *  MATCH_ERROR  if the call corresponds to an intrinsic but there was an
 *               error during the simplification process.
 *
 * The error_flag parameter enables error reporting.
 */

match g95_intrinsic_func_interface(g95_expr *expr, int error_flag) {
g95_intrinsic_sym *isym, *specific;

    if (g95_has_alt_return(expr->value.function.actual))
	return MATCH_NO; 

    isym = expr->value.function.isym;

    if (isym != NULL) {
	if (0 && check_specific(isym, expr, error_flag) == FAILURE) {
	    g95_suppress_error = 0;
	    return MATCH_NO;
	}

	if (do_simplify(expr->value.function.isym, expr) == FAILURE)
	    return MATCH_ERROR;

	if (expr->type == EXPR_FUNCTION)
	    fix_array_args(expr->value.function.isym->formal,
			   expr->value.function.actual);

	return MATCH_YES;
    }

    g95_suppress_error = !error_flag;
    g95_intrinsic_extension = 1;

    isym = specific = g95_find_function(expr->value.function.name);
    if (isym == NULL) {
	g95_suppress_error = 0;
	return MATCH_NO;
    }

    g95_current_intrinsic_where = &expr->where;

/* Bypass the generic list for min and max */

    if (isym->resolve == g95_resolve_min_max) {
	init_arglist(isym);

	if (g95_check_min_max(expr->value.function.actual) == SUCCESS)
	    goto got_specific;

	g95_suppress_error = 0;
	return MATCH_NO;
    }

/* If the function is generic, check all of its specific incarnations.
 * If the generic name is also a specific, we check that name last, so
 * that any error message will correspond to the specific */

    g95_suppress_error = 1;

    if (isym->generic) {
	for(specific=isym->specific_head; specific; specific=specific->next) {
	    if (specific == isym)
		continue;

	    if (check_specific(specific, expr, 0) == SUCCESS)
		goto got_specific;
	}
    }

    g95_suppress_error = !error_flag;

    if (check_specific(isym, expr, error_flag) == FAILURE) {
	g95_suppress_error = 0;
	return MATCH_NO;
    }

    specific = isym;

got_specific:
    g95_suppress_error = !error_flag;
    expr->value.function.isym = specific;

    if (g95_intrinsic_symbol(expr->value.function.name, 1, &expr->where)) {
	g95_suppress_error = 0;
	return MATCH_ERROR;
    }

    resolve_intrinsic(specific, expr);

    if (specific->elemental &&
	check_elemental_conformance(specific,
				    expr->value.function.actual) == FAILURE) {
	g95_suppress_error = 0;
	return MATCH_ERROR;
    }

    if (do_simplify(specific, expr) == FAILURE) {
	g95_suppress_error = 0;
	return MATCH_ERROR;
    }

    if (!simplified && expr->type == EXPR_FUNCTION)
	arg_usage(specific->formal, expr->value.function.actual);

    g95_suppress_error = 0;
    return MATCH_YES;
}



/* g95_intrinsic_sub_interface()-- see if a CALL statement corresponds
 * to an intrinsic subroutine.  Returns MATCH_YES if the subroutine
 * corresponds to an intrinsic, MATCH_NO if not, and MATCH_ERROR if
 * there was an error (but did correspond). */

match g95_intrinsic_sub_interface(g95_code *c, int error_flag) {
g95_intrinsic_sym *isym;
char *name;
try t;

    if (g95_has_alt_return(c->ext.sub.actual))
	return MATCH_NO;

    name = c->ext.sub.name;

    isym = g95_find_subroutine(name);
    if (isym == NULL)
	return MATCH_NO;

    g95_suppress_error = !error_flag;

    init_arglist(isym);

    t = sort_actual(name, &c->ext.sub.actual, isym->formal, &c->where);
    if (t == FAILURE)
	goto fail;

    if ((isym->check != NULL &&
	 do_check(isym, c->ext.sub.actual) == FAILURE) ||
	(isym->check == NULL &&
	 check_arglist(&c->ext.sub.actual, isym, 1) == FAILURE))
	goto fail;

    /* The subroutine corresponds to an intrinsic.  Allow errors to be
     * seen at this point. */

    g95_suppress_error = 0;

    c->ext.sub.isym = isym;

    if (isym->resolve != NULL)
	isym->resolve(c);

    else
	c->ext.sub.sub_name = isym->lib_name;

    if (g95_pure(NULL, 0) && !isym->elemental) {
	g95_error("Subroutine call to intrinsic '%s' at %L is not PURE",
		  name, &c->where);
	return MATCH_ERROR;
    }

    if (g95_intrinsic_symbol(name, 0, &c->where))
	return MATCH_ERROR;

    fix_array_args(isym->formal, c->ext.sub.actual);
    arg_usage(isym->formal, c->ext.sub.actual);

    return MATCH_YES;

fail:
    remove_nullargs(&c->ext.sub.actual);

    g95_suppress_error = 0;
    return MATCH_NO;
}



/* intrinsic_type()-- Find an intrinsic type node */

static g95_symbol *intrinsic_type(internal_type itype) {
g95_namespace *ns;

    for(ns=g95_current_ns; ns; ns=ns->parent)
	if (ns->itypes[itype] != NULL)
	    return ns->itypes[itype];

    g95_internal_error("intrinsic_type(): Can't find intrinsic type");
    return NULL;
}



/* g95_check_intrinsic_modfunc()-- Return nonzero if the intrinsic
 * module function is being used correctly.  If not, an error is
 * assumed to be issued. */

try g95_check_intrinsic_modfunc(g95_symbol *sym, g95_expr *expr) {
g95_expr *e, *a[MAX_INTRINSIC_ARGS];
g95_actual_arglist *actual;
g95_intrinsic_arg *b;
int m;
try t;

    b = modproc[sym->attr.iproc].args;

    if (sort_actual(sym->name, &expr->value.function.actual,
		    b, &expr->where) == FAILURE)
	return FAILURE;

    m = 0;
    actual = expr->value.function.actual;

    for(m=0; m<MAX_INTRINSIC_ARGS; m++) {
	a[m] = (actual == NULL) ? NULL : actual->u.expr;
	g95_current_intrinsic_arg[m] = (b == NULL) ? NULL : b->name;

	if (actual != NULL)
	    actual = actual->next;

	if (b != NULL)
	    b = b->next;
    }

    g95_current_intrinsic = sym->name;
    expr->value.function.iproc = sym->attr.iproc;

    switch(sym->attr.iproc) {
    case IPROC_C_LOC:
	t = g95_check_c_loc(a[0]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_DERIVED;
	    expr->ts.derived = intrinsic_type(ITYPE_C_PTR);
	    expr->rank = 0;
	}

	break;

    case IPROC_C_FUNLOC:
	t = g95_check_c_funloc(a[0]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_DERIVED;
	    expr->ts.derived = intrinsic_type(ITYPE_C_FUNPTR);
	    expr->rank = 0;
	}

	break;

    case IPROC_C_ASSOCIATED:
	t = g95_check_c_associated(a[0], a[1]);
	break;

    case IPROC_C_F_POINTER:
	t = g95_check_c_f_pointer(a[0], a[1], a[2]);
	break;

    case IPROC_C_F_PROCPOINTER:
	t = g95_check_c_f_procpointer(a[0], a[1]);
	break;

    case IPROC_IEEE_CLASS:
	t = g95_check_ieee_class(a[0]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_DERIVED;
	    expr->ts.derived = intrinsic_type(ITYPE_IEEE_CLASS);
	    expr->rank = a[0]->rank;
	    expr->value.function.iname =
		g95_get_fstring(PREFIX "class_%d", a[0]->ts.kind);
	}

	break;

    case IPROC_IEEE_IS_NAN:
	t = g95_check_ieee_is_nan(a[0]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_LOGICAL;
	    expr->ts.kind = g95_default_logical_kind();
	    expr->rank = 0;
	    expr->value.function.iname =
		g95_get_fstring(PREFIX "isnan_%d", a[0]->ts.kind);
	}

	break;

    case IPROC_IEEE_VALUE:
	t = g95_check_ieee_value(a[0], a[1]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_REAL;
	    expr->ts.kind = a[0]->ts.kind;
	    expr->rank = a[1]->rank;
	    expr->value.function.iname =
		g95_get_fstring(PREFIX "value_%d", a[0]->ts.kind);
	}

	break;

    case IPROC_IEEE_IS_NEGATIVE:
	t = g95_check_ieee_is_negative(a[0]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_LOGICAL;
	    expr->ts.kind = g95_default_logical_kind();
	    expr->rank = a[0]->rank;
	    expr->value.function.iname =
		g95_get_fstring(PREFIX "is_negative_%d", a[0]->ts.kind);
	}

	break;

    case IPROC_IEEE_IS_FINITE:
	t = g95_check_ieee_is_finite(a[0]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_LOGICAL;
	    expr->ts.kind = g95_default_logical_kind();
	    expr->rank    = a[0]->rank;
	    expr->value.function.iname =
		g95_get_fstring(PREFIX "is_finite_%d", a[0]->ts.kind);
	}

	break;

    case IPROC_IEEE_IS_NORMAL:
	t = g95_check_ieee_is_normal(a[0]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_LOGICAL;
	    expr->ts.kind = g95_default_logical_kind();
	    expr->rank    = a[0]->rank;
	    expr->value.function.iname =
		g95_get_fstring(PREFIX "is_normal_%d", a[0]->ts.kind);
	}

	break;

    case IPROC_IEEE_SUPPORT_DATATYPE:
    case IPROC_IEEE_SUPPORT_DENORMAL:
    case IPROC_IEEE_SUPPORT_DIVIDE:
    case IPROC_IEEE_SUPPORT_INF: 
    case IPROC_IEEE_SUPPORT_NAN:
    case IPROC_IEEE_SUPPORT_SQRT:
    case IPROC_IEEE_SUPPORT_STANDARD:
    case IPROC_IEEE_SUPPORT_UNDERFLOW_CONTROL:
	t = g95_check_ieee_support(a[0]);
	if (t == SUCCESS) {
	    e = g95_logical_expr(1, &expr->where);
	    g95_replace_expr(expr, e);
	}

	break;

    case IPROC_IEEE_SUPPORT_ROUNDING:
	t = g95_check_ieee_support_rounding(a[0], a[1]);
	if (t == SUCCESS) {
	    e = g95_logical_expr(1, &expr->where);
	    g95_replace_expr(expr, e);
	}

	break;

    case IPROC_IEEE_SELECTED_REAL_KIND:
	t = g95_check_selected_real_kind(a[0], a[1]);
	if (t == SUCCESS) {
	    expr->ts.type = BT_INTEGER;
	    expr->ts.kind = g95_default_integer_kind(0);
	    expr->rank = 0;
	}

	break;

    default:
	g95_internal_error("g95_check_intrinsic_modproc(): Bad proc");
	t = FAILURE;
	break;
    }

    if (t == SUCCESS && expr->type == EXPR_FUNCTION) {
	arg_usage(modproc[sym->attr.iproc].args, expr->value.function.actual);
	expr->symbol = sym;

	if (sym->attr.function && expr->ts.type == BT_UNKNOWN) {
	    expr->ts = sym->ts;
	    expr->rank = 0;
	    expr->value.function.iname = 0;
	}
    }

    return t;
}



/* g95_check_intrinsic_modsub()-- Return nonzero if the intrinsic
 * module subroutine is being used correctly.  If not, an error is
 * assumed to be issued. */

try g95_check_intrinsic_modsub(g95_symbol *sym, g95_code *c) {
g95_expr *a[MAX_INTRINSIC_ARGS];
g95_actual_arglist *actual;
g95_intrinsic_arg *b;
int m;
try t;

    b = modproc[sym->attr.iproc].args;

    if (sort_actual(sym->name, &c->ext.sub.actual, b, &c->where) == FAILURE)
	return FAILURE;

    m = 0;
    actual = c->ext.sub.actual;

    for(m=0; m<MAX_INTRINSIC_ARGS; m++) {
	a[m] = (actual == NULL) ? NULL : actual->u.expr;
	g95_current_intrinsic_arg[m] = (b == NULL) ? NULL : b->name;

	if (actual != NULL)
	    actual = actual->next;

	if (b != NULL)
	    b = b->next;
    }

    g95_current_intrinsic = sym->name;

    switch(sym->attr.iproc) {
    case IPROC_C_F_POINTER:
	t = g95_check_c_f_pointer(a[0], a[1], a[2]);
	break;

    case IPROC_C_F_PROCPOINTER:
	t = g95_check_c_f_procpointer(a[0], a[1]);
	break;

    default:
	g95_internal_error("g95_check_intrinsic_modsub(): Bad procedure");
	t = FAILURE;
	break;
    }

    if (t == SUCCESS) {
	c->sym = sym;
	arg_usage(modproc[sym->attr.iproc].args, c->ext.sub.actual);
    }

    return t;
}



/* g95_convert_hollerith()-- Convert a hollerith constant to something
 * else.  Returns NULL if something goes wrong. */

#define MEM_SIZE 100

g95_expr *g95_convert_hollerith(g95_expr *e, g95_typespec *ts) {
g95_expr *result;
char mem[MEM_SIZE];
g95_ff *ff;
int len;

    memset(mem, ' ', MEM_SIZE);
    len = e->value.character.length;

    if (len > MEM_SIZE)
	len = MEM_SIZE;

    memmove(mem, e->value.character.string, len);
    result = g95_constant_result(ts->type, ts->kind, &e->where);

    switch(ts->type) {
    case BT_INTEGER:
	result->value.integer = g95_unpack_int(mem, ts->kind);
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	result->value.real = g95_unpack_real(mem, g95_get_ff(ts->kind));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	ff = g95_get_ff(ts->kind);
	result->value.complex.r = g95_unpack_real(mem, ff);
	big_permanent(result->value.complex.r);

	result->value.complex.i = g95_unpack_real(mem + ts->kind, ff);
	big_permanent(result->value.complex.i);
	break;

    case BT_LOGICAL:
	result->value.logical = g95_unpack_logical(mem, ts->kind);
	break;

    default:
	g95_free_expr(result);
	result = NULL;

	g95_error("Can't convert to type %s at %L",
		  g95_typename(ts), &e->where);
	break;
    }

    return result;
}



/* check_prec_loss()-- Check for precision loss during an implicit
 * constant conversion.  */

static int check_prec_loss(g95_expr *orig, g95_expr *expr) {
bignum m, n;

    if (orig->type != EXPR_CONSTANT || expr->type != EXPR_CONSTANT)
	return 0;

    switch(orig->ts.type) {
    case BT_INTEGER:
	m = NULL;

	switch(expr->ts.type) {
	case BT_REAL:
	    m = bg_to_bi(expr->value.real);
	    break;

	case BT_COMPLEX:
	    m = bg_to_bi(expr->value.complex.r);
	    break;

	default:
	    break;
	}

	return (m != NULL && bi_compare(m, orig->value.integer) != 0);

    case BT_REAL:
	n = orig->value.real;
	break;

    case BT_COMPLEX:
	if (expr->ts.type != BT_COMPLEX) {
	    if (bg_real_type(orig->value.complex.i) != FF_ZERO)
		return 1;

	} else {
	    m = bg_convert(orig->value.complex.i, g95_get_ff(expr->ts.kind));
	    m = bg_convert(m, g95_get_ff(orig->ts.kind));

	    if (bg_compare_ne(m, orig->value.complex.i))
		return 1;      
	}

	n = orig->value.complex.r;
	break;

    default:
	return 0;
    }

    /* Common code for real and complex cases. */

    switch(expr->ts.type) {
    case BT_INTEGER:
	m = bg_from_bi(expr->value.integer, n->ff);
	break;

    case BT_REAL:
	m = big_copy(expr->value.real);
	break;

    case BT_COMPLEX:
	m = big_copy(expr->value.complex.r);
	break;

    default:
	m = NULL;
	break;
    }

    if (m->ff != n->ff)
	m = bg_convert(m, n->ff);

    return (m != NULL && bg_compare_ne(m, n));
}



/* convert_length()-- Do a character length conversion */

static void convert_length(g95_expr *expr, g95_typespec *ts) {
char *p;
int len;

    expr->ts = *ts;

    if (expr->ts.cl == NULL || expr->ts.cl->length == NULL ||
	expr->ts.cl->length->type != EXPR_CONSTANT ||
	ts->cl->length->type != EXPR_CONSTANT)
	return;

    if (g95_extract_int(ts->cl->length, &len) != NULL)
	return;

    if (len > expr->value.character.length) {
	p = g95_getmem(len+1);

	memset(p, ' ', len);
	p[len] = '\0';

	memcpy(p, expr->value.character.string, expr->value.character.length);

	g95_free(expr->value.character.string);
	expr->value.character.string = p;
    }

    expr->value.character.length = len;
}



/* g95_convert_type()-- Tries to convert an expression (in place) from
 * one type to another. */

try g95_convert_type(g95_expr *expr, g95_typespec *ts, int fatal) {
int rank, constant, level;
g95_intrinsic_sym *sym;
g95_typespec from_ts;
g95_expr *new, *orig;
g95_locus old_where;
try rc;

    level = 0;
    orig = NULL;
    rc = SUCCESS;

    from_ts = expr->ts;        /* expr->ts gets clobbered */
    if (ts->type == BT_UNKNOWN) {
	level = 3;
	goto done;
    }

    if (expr->type == EXPR_NULL ||
	g95_zero_size_ac(expr)) { /* Sometimes the RHS acquires the type */
	expr->ts = *ts;
	goto done;
    }

    if (expr->ts.type == BT_UNKNOWN) {
	level = 3;
	goto done;
    }

    if (expr->ts.type == BT_CHARACTER && ts->type == BT_CHARACTER)
	convert_length(expr, ts);

    if (g95_compare_types(&expr->ts, ts))
	goto done;

    switch(expr->type) {
    case EXPR_CONSTANT:
	constant = 1;
	break;

    case EXPR_ARRAY:
	constant = g95_is_constant_expr(expr);
	break;

    default:
	constant = 0;
	break;
    }

    if (constant && expr->ts.type == BT_CHARACTER &&
	ts->type != BT_CHARACTER &&
	expr->value.character.hollerith) {

	new = g95_convert_hollerith(expr, ts);
	if (new != NULL)
	    g95_replace_expr(expr, new);
	else
	    level = 3;

	goto done;
    }

    sym = find_conv(&expr->ts, ts);
    if (sym == NULL) {
	level = 3;
	goto done;
    }

    if (constant && g95_option.prec_loss)
	orig = g95_copy_expr(expr);

    if (!constant && g95_option.prec_loss) {
	switch(from_ts.type) {
	case BT_INTEGER:
	    if (ts->kind < from_ts.kind)
		level = 1;

	    break;

	case BT_REAL:
	    if (ts->type == BT_INTEGER || ts->kind < from_ts.kind)
		level = 1;

	    break;

	case BT_COMPLEX:
	    if (ts->type == BT_INTEGER || ts->type == BT_REAL ||
		ts->kind < from_ts.kind)
		level = 1;
	    break;

	default:
	    break;
	}
    }

/* Insert a pre-resolved function call to the right function */

    old_where = expr->where;
    rank = expr->rank;
    new = g95_get_expr();
    *new = *expr;

    new = g95_build_funcall(NULL, new, NULL);
    new->value.function.iname = sym->lib_name;
    new->value.function.isym = sym;
    new->where = old_where;
    new->rank = rank;

    *expr = *new;

    g95_free(new);
    expr->ts = *ts;

    if (!constant)
	goto done;

    if (do_simplify(sym, expr) == FAILURE) {
	if (fatal)
	    level = 3;
	else {
	    rc = FAILURE;
	    level = 0;
	}

	goto done;
    }

    if (g95_option.prec_loss && check_prec_loss(orig, expr))
	level = 2;

done:
    switch(level) {
    case 0:
	break;

    case 1:
	g95_warning(140, "Implicit conversion at %L may cause precision loss",
		    &expr->where);
	break;

    case 2:
	g95_warning(141, "Implicit conversion at %L causes precision loss",
		    &expr->where);
	break;

    case 3:
	if (fatal)
	    g95_internal_error("Can't convert %s to %s at %L",
			       g95_typename(&from_ts), g95_typename(ts),
			       &expr->where);

	g95_error("Can't convert %s to %s at %L",
		  g95_typename(&from_ts), g95_typename(ts), &expr->where);
	rc = FAILURE;
    }

    if (orig != NULL)
	g95_free_expr(orig);

    return rc;
}

