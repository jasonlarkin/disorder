/* Expression subroutines
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

/* expr.c-- Manipulate expression nodes */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "g95.h"

static try check_init_expr(g95_expr *);
static try check_restricted(g95_expr *);


typedef struct {
    bignum lower, upper, stride, value, trip, count;
} array_range;


static g95_shapes *current_shapes;

static int next_shape, elemental_flag=0;
static g95_symbol *procedure;


/* g95_get_expr()-- Get a new expr node */

g95_expr *g95_get_expr(void) {
static int serial = 0;
g95_expr *e;

    e = g95_getmem(sizeof(g95_expr));

    g95_clear_ts(&e->ts);
    e->serial = serial++;

    return e;
}



/* g95_get_charlen()-- Get a new charlen node */

g95_charlen *g95_get_charlen(g95_namespace *ns) {
g95_charlen *cl;

    cl = g95_getmem(sizeof(g95_charlen));

    if (ns == NULL)
	ns = g95_current_ns;

    cl->next = ns->cl_list;
    ns->cl_list = cl;

    return cl;
}



/* g95_get_shape()-- Allocate a new set of shapes. */

bignum *g95_get_shape(int n) {
g95_shapes *s;
bignum *p;

    if (current_shapes == NULL || next_shape + n >= G95_SHAPES) {
	s = g95_getmem(sizeof(g95_shapes));
	s->next = current_shapes;
	current_shapes = s;
	next_shape = 0;
    }

    p = current_shapes->buffer + next_shape;
    next_shape += n;

    return p;
}



/* g95_free_expr0()-- Workhorse function for g95_free_expr() that
 * frees everything beneath an expression node, but not the node
 * itself.  This is useful when we want to simplify a node and replace
 * it with something else or the expression node belongs to another
 * structure.  */

void g95_free_expr0(g95_expr *e) {

    switch(e->type) {
    case EXPR_CONSTANT:
	switch(e->ts.type) {
	case BT_INTEGER:
	    big_depermanent(e->value.integer);
	    big_free(e->value.integer);
	    break;

	case BT_REAL:
	    big_depermanent(e->value.real);
	    big_free(e->value.real);
	    break;

	case BT_CHARACTER:
	    g95_free(e->value.character.string); 
	    break;

	case BT_COMPLEX:
	    big_depermanent(e->value.complex.r);
	    big_free(e->value.complex.r);

	    big_depermanent(e->value.complex.i);
	    big_free(e->value.complex.i);
	    break;

	default:
	    break;
	}

	break;

    case EXPR_OP:
	if (e->value.op.op1 != NULL)
	    g95_free_expr(e->value.op.op1);

	if (e->value.op.op2 != NULL)
	    g95_free_expr(e->value.op.op2);

	break;

    case EXPR_FUNCTION:
	g95_free_actual_arglist(e->value.function.actual);
	break;

    case EXPR_NULL:
    case EXPR_VARIABLE:
    case EXPR_UNKNOWN:
    case EXPR_PROCEDURE:
	break;

    case EXPR_ARRAY:
    case EXPR_STRUCTURE:
	g95_free_constructor(e->value.constructor.c);
	break;

    case EXPR_SUBSTRING:
	g95_free(e->value.character.string);
	break;

    default:
	g95_internal_error("g95_free_expr0(): Bad expr type");
    }

    g95_free_ref_list(e->ref);

    /* Shapes aren't freed here */

    memset(e, '\0', sizeof(g95_expr));
}



/* g95_free_expr()-- Free an expression node and everything beneath it. */

void g95_free_expr(g95_expr *e) {

    if (e == NULL)
	return;

    g95_free_expr0(e);
    g95_free(e);
}



/* g95_replace_expr()-- Grafts the *src expression onto the *dest
 * subexpression. */

void g95_replace_expr(g95_expr *dest, g95_expr *src) {

    g95_free_expr0(dest);
    *dest = *src;

    memset(src, '\0', sizeof(g95_expr));
    g95_free(src);
}



/* g95_extract_int()-- Tries to extract an integer constant from the
 * passed expression node.  Returns an error message or NULL if the
 * result is set.  It is tempting to generate an error and return
 * SUCCESS or FAILURE, but failure is OK for some callers. */

char *g95_extract_int(g95_expr *expr, int *result) {

    if (expr->type != EXPR_CONSTANT)
	return "Constant expression required at %C";

    if (expr->ts.type != BT_INTEGER)
	return "Integer expression required at %C";

    if (bi_compare(expr->value.integer, bi_maxint) > 0 ||
	bi_compare(expr->value.integer, bi_minint) < 0)
	return "Integer value too large in expression at %C";

    *result = bi_to_int(expr->value.integer);
    return NULL;
}



/* g95_copy_formal_arglist()-- Copy a formal argument list. */

g95_formal_arglist *g95_copy_formal_arglist(g95_formal_arglist *f) {
g95_formal_arglist *head, *tail;

    head = tail = NULL;

    for(; f; f=f->next) {

	if (head == NULL)
	    head = tail = g95_get_formal_arglist();

	else {
	    tail->next = g95_get_formal_arglist();
	    tail = tail->next;
	}

	tail->where = f->where;
	tail->sym   = f->sym;
    }

    return head;
}



/* copy_array_ref()-- Copy an array reference structure */

static void copy_array_ref(g95_array_ref *src, g95_array_ref *dest) {
int i;

    if (src == NULL)
	return;

    *dest = *src;

    for(i=0; i<G95_MAX_DIMENSIONS; i++) {
	dest->start[i]  = g95_copy_expr(src->start[i]);
	dest->end[i]    = g95_copy_expr(src->end[i]);
	dest->stride[i] = g95_copy_expr(src->stride[i]);
    }
}


/* copy_coarray_ref()-- Copy a coarray reference */

static void copy_coarray_ref(g95_coarray_ref *src, g95_coarray_ref *dest) {
int i;

    if (src == NULL)
	return;

    *dest = *src;

    for(i=0; i<G95_MAX_DIMENSIONS; i++)
	dest->element[i] = g95_copy_expr(src->element[i]);
}



/* copy_ref()-- Recursively copy a list of reference structures */

static g95_ref *copy_ref(g95_ref *src) {
g95_ref *dest;

    if (src == NULL)
	return NULL; 

    dest = g95_get_ref();

    dest->type  = src->type;
    dest->where = src->where;

    switch(src->type) {
    case REF_ARRAY:
	copy_array_ref(&src->u.ar, &dest->u.ar);
	break;

    case REF_COARRAY:
	copy_coarray_ref(&src->u.car, &dest->u.car);
	break;

    case REF_COMPONENT:
	dest->u.c = src->u.c;
	break;

    case REF_SUBSTRING:
	dest->u.ss       = src->u.ss;
	dest->u.ss.start = g95_copy_expr(src->u.ss.start);
	dest->u.ss.end   = g95_copy_expr(src->u.ss.end);
	break;
    }

    dest->next = copy_ref(src->next);

    return dest;
}



/* g95_full_ref()-- Get a full array reference node */

g95_ref *g95_full_ref(g95_locus *where) {
g95_ref *ref;

    ref = g95_get_ref();

    ref->type      = REF_ARRAY;
    ref->u.ar.type = AR_FULL;
    ref->where     = (where == NULL) ? g95_current_locus : *where;

    return ref;
}



/* g95_copy_expr()-- Given an expression pointer, return a copy of the
 * expression.  This subroutine is recursive. */

g95_expr *g95_copy_expr(g95_expr *p) {
int i, length, serial;
g95_expr *q;
char *s;

    if (p == NULL)
	return NULL;

    q = g95_get_expr();
    serial = q->serial;
    *q = *p;
    q->serial = serial;

    if (q->shape != NULL) {
	q->shape = g95_getmem(p->rank * sizeof(bignum));

	for(i=0; i<p->rank; i++) {
	    q->shape[i] = big_clone(p->shape[i]);
	    big_permanent(q->shape[i]);
	}
    }

    switch(q->type) {
    case EXPR_SUBSTRING:
	s = g95_getmem(p->value.character.length+1);
	q->value.character.string = s;

	memcpy(s, p->value.character.string, p->value.character.length+1);
	break;

    case EXPR_CONSTANT:
	switch(q->ts.type) {
	case BT_INTEGER:
	    q->value.integer = big_clone(p->value.integer);
	    big_permanent(q->value.integer);
	    break;

	case BT_REAL:
	    q->value.real = big_clone(p->value.real);
	    big_permanent(q->value.real);
	    break;

	case BT_COMPLEX:
	    q->value.complex.r = big_clone(p->value.complex.r);
	    q->value.complex.i = big_clone(p->value.complex.i);

	    big_permanent(q->value.complex.r);
	    big_permanent(q->value.complex.i);
	    break;

	case BT_CHARACTER:
	    length = p->value.character.length;

	    s = g95_getmem(length+1);
	    q->value.character.string = s;

	    if (length != 0)
		memcpy(s, p->value.character.string, length+1);

	    break;

	case BT_LOGICAL:
	case BT_DERIVED:
	    break; /* Already done */

	case BT_PROCEDURE:
	case BT_UNKNOWN:
	    g95_internal_error("g95_copy_expr(): Bad expr node");
	    break;
	}

	break;

    case EXPR_OP:
	switch(q->value.op.operator) {
	case INTRINSIC_NOT:
	case INTRINSIC_UPLUS:
	case INTRINSIC_UMINUS:
	case INTRINSIC_PAREN:
	    q->value.op.op1 = g95_copy_expr(p->value.op.op1);
	    break;

	default:               /* Binary operators */
	    q->value.op.op1 = g95_copy_expr(p->value.op.op1);
	    q->value.op.op2 = g95_copy_expr(p->value.op.op2);
	    break;
	}

	break;

    case EXPR_FUNCTION:
	q->value.function.actual =
	    g95_copy_actual_arglist(p->value.function.actual);
	break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
	q->value.constructor.c =
	    g95_copy_constructor(p->value.constructor.c);
	break;

    case EXPR_VARIABLE:
	break;

    case EXPR_UNKNOWN:
    case EXPR_NULL:
    case EXPR_PROCEDURE:
	break;
    }

    q->ref = copy_ref(p->ref);

    return q;
}



/* g95_get_actual_arglist()-- Get an actual argument list */

g95_actual_arglist *g95_get_actual_arglist(void) {
g95_actual_arglist *a;

    a = g95_getmem(sizeof(g95_actual_arglist));
    memset(a, '\0', sizeof(g95_actual_arglist));

    return a;
}



/* g95_free_actual_arglist()-- Free an argument list and everything
 * below it. */

void g95_free_actual_arglist(g95_actual_arglist *a1) {
g95_actual_arglist *a2;

    g95_delete_global_ref(g95_gsym_root, a1);

    while(a1) {
	a2 = a1->next;
	if (a1->type != ARG_ALT_RETURN)
	    g95_free_expr(a1->u.expr);

	g95_free(a1);
	a1 = a2;
    }
}



/* g95_copy_arglist()-- Copy an arglist structure and all of the arguments. */

g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *old) {
g95_actual_arglist *p, *head, *tail, *new;

    head = tail = NULL;

    for(p=old; p; p=p->next) {
	new = g95_get_actual_arglist();
	*new = *p;

	if (p->type != ARG_ALT_RETURN)
	    new->u.expr = g95_copy_expr(p->u.expr);

	new->next = NULL;

	if (head == NULL)
	    head = new;
	else
	    tail->next = new;

	tail = new;
    }

    g95_copy_global_ref(g95_gsym_root, old, head);

    return head;
}



/* g95_free_ref_list()-- Free a list of reference structures */

void g95_free_ref_list(g95_ref *p) {
g95_ref *q;
int i;

    for(; p; p=q) {
	q = p->next;

	switch(p->type) {
	case REF_ARRAY:
	    for(i=0; i<G95_MAX_DIMENSIONS; i++) {
		g95_free_expr(p->u.ar.start[i]);
		g95_free_expr(p->u.ar.end[i]);
		g95_free_expr(p->u.ar.stride[i]);
	    }

	    break;

	case REF_COARRAY:
	    for(i=0; i<G95_MAX_DIMENSIONS; i++)
		g95_free_expr(p->u.car.element[i]);

	    break;

	case REF_SUBSTRING:
	    g95_free_expr(p->u.ss.start);
	    g95_free_expr(p->u.ss.end);
	    break;

	case REF_COMPONENT:
	    break;
	}

	g95_free(p);
    }
}



/* g95_vector_subscripts()-- Return nonzero if the expression contains
 * vector subscripts. */

int g95_vector_subscripts(g95_expr *e) {
g95_ref *ref;
int i;

    if (e->type != EXPR_VARIABLE)
	return 0;

    for(ref=e->ref; ref; ref=ref->next) {
	if (ref->type != REF_ARRAY || ref->u.ar.type != AR_SECTION)
	    continue;

	for(i=0; i<ref->u.ar.dimen; i++)
	    if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
		return 1;
    }

    return 0;
}



/* g95_coindexed_expr()-- Return nonzero if the expression has a
 * coarray reference at the top level. */

int g95_coindexed_expr(g95_expr *e) {
g95_ref *ref;

    if (e->type != EXPR_VARIABLE)
	return 0;

    for(ref=e->ref; ref; ref=ref->next)
	if (ref->type == REF_COARRAY)
	    return 1;

    return 0;
}



/* g95_coarray_expr()-- Return nonzero if the expression is a full coarray. */

int g95_coarray_expr(g95_expr *e) {
g95_ref *ref;
int flag;

    if (e->type != EXPR_VARIABLE)
	return 0;

    flag = (e->symbol->cas != NULL);

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type == AR_FULL)
		break;

	    /* Fall Through */

	case REF_COARRAY:
	case REF_SUBSTRING:
	    flag = 0;
	    break;

	case REF_COMPONENT:
	    flag = ref->u.c.component->cas != NULL;
	    break;
	}

    return flag;
}



/* g95_kind_max()-- Return the maximum kind of two expressions.
 * Higher kind numbers mean more precision for numeric types. */

int g95_kind_max(g95_expr *e1, g95_expr *e2) {

    return (e1->ts.kind > e2->ts.kind)
	? e1->ts.kind
	: e2->ts.kind;
}



/* numeric_type()-- Returns nonzero if the type is numeric, zero
 * otherwise */

static int numeric_type(bt type) {

    return type == BT_COMPLEX || type == BT_REAL || type == BT_INTEGER;
}



/* g95_numeric_ts()-- Returns nonzero if the typespec is a numeric
 * type, zero otherwise. */

int g95_numeric_ts(g95_typespec *ts) {

    return numeric_type(ts->type);
}



/* g95_int_expr()-- Returns an expression node that is an integer
 * constant. */

g95_expr *g95_int_expr(int i) {
g95_expr *p;

    p = g95_get_expr();

    p->type    = EXPR_CONSTANT;
    p->ts.type = BT_INTEGER;
    p->ts.kind = g95_default_integer_kind(0);

    p->where = g95_current_locus;
    p->value.integer = big_clone(int_to_bi(i));
    big_permanent(p->value.integer);

    return p;
}



/* g95_logical_expr()-- Returns an expression node that is a logical
 * constant. */

g95_expr *g95_logical_expr(int i, g95_locus *where) {
g95_expr *p;

    p = g95_get_expr();

    p->type    = EXPR_CONSTANT;
    p->ts.type = BT_LOGICAL;
    p->ts.kind = g95_default_logical_kind();

    if (where == NULL)
	where = &g95_current_locus;

    p->where = *where;
    p->value.logical = i;

    return p;
}



/* g95_char_expr()-- Return an expression node that is a character constant. */

g95_expr *g95_char_expr(int length, int kind, g95_locus *where) {
g95_charlen *cl;
g95_expr *e;

    cl = g95_get_charlen(NULL);
    cl->length = g95_int_expr(length);

    e = g95_get_expr();

    e->type    = EXPR_CONSTANT;
    e->ts.type = BT_CHARACTER;
    e->ts.kind = kind;
    e->ts.cl   = cl;

    if (where == NULL)
	where = &g95_current_locus;

    e->where = *where;

    e->value.character.string = g95_getmem(length+1);
    e->value.character.length = length;

    memset(e->value.character.string, ' ', length);
    e->value.character.string[length] = '\0';

    return e;
}



/* g95_get_temporary()-- Creates a temporary symbol which is a
 * variable of a particular type and rank.  If the variable has
 * nonzero rank, it is marked as allocatable with a deferred array
 * specification. */

g95_symbol *g95_get_temporary(g95_typespec *ts, int rank, int corank) {
char name[G95_MAX_SYMBOL_LEN+1];
static int serial = 0;
g95_coarray_spec *cas;
g95_array_spec *as;
g95_symtree *st;
g95_symbol *sym;

    sprintf(name, "SC%d", serial++);
    sym = g95_new_symbol(name, g95_current_ns);

    sym->attr.flavor     = FL_VARIABLE;
    sym->attr.used       = 1;
    sym->attr.artificial = 1;
    sym->refs            = 1;

    sym->ts = *ts;
    if (ts->type == BT_CHARACTER && ts->cl == NULL)
	sym->ts.cl = &g95_unknown_charlen;

    if (rank > 0) {
	as = sym->as = g95_get_array_spec();
	as->rank = rank;
	as->type = AS_DEFERRED;

	sym->attr.allocatable = 1;
	sym->attr.dimension   = 1;
    }

    if (corank > 0) {
	cas = sym->cas = g95_get_coarray_spec();
	cas->corank = corank;
	cas->artificial = 1;
	cas->type = CAS_DEFERRED;
    }

    st = g95_new_symtree(&g95_current_ns->sym_root, name);
    st->n.sym = sym;

    return sym;
}



/* g95_null_expr()-- Return an expression that is the NULL() function. */

g95_expr *g95_null_expr(g95_locus *where) {
g95_expr *e;

    e = g95_get_expr(); 

    e->type    = EXPR_NULL;
    e->ts.type = BT_UNKNOWN;
    e->where   = (where == NULL) ? g95_current_locus : *where;
    e->rank    = -1;

    return e;
}



/* g95_get_temporary_int()-- Allocate an integer loop variable */

g95_symbol *g95_get_temporary_int(void) {
g95_typespec ts;

    g95_clear_ts(&ts); 
    ts.type = BT_INTEGER;
    ts.kind = g95_default_integer_kind(0);

    return g95_get_temporary(&ts, 0, 0);
}



/* g95_get_array_int()-- Allocate a temporary array index variable */

g95_symbol *g95_get_array_int(void) {
g95_typespec ts;

    g95_clear_ts(&ts); 
    ts.type = BT_INTEGER;
    ts.kind = sizeof(void *);

    return g95_get_temporary(&ts, 0, 0);
}



/* g95_len_expr()-- Return an expression that is a LEN() call */

g95_expr *g95_len_expr(g95_expr *arg) {
g95_actual_arglist *a;
g95_expr *f;

    a = g95_get_actual_arglist();

    a->type   = ARG_EXPR;
    a->u.expr = arg;

    f = g95_get_expr();

    f->type    = EXPR_FUNCTION;
    f->ts.type = BT_INTEGER;
    f->ts.kind = g95_default_integer_kind(0);
    f->where   = arg->where;
    f->rank    = 0;

    f->value.function.actual = a;
    f->value.function.isym   = g95_find_function("len");
    f->value.function.iname  = f->value.function.isym->name;

    return f;
}



/* g95_get_variable_expr()-- Given a symbol, create an expression node
 * with that symbol as a variable. */

g95_expr *g95_get_variable_expr(g95_symbol *var, g95_locus *where) {
g95_expr *e;

    e = g95_get_expr();

    e->type   = EXPR_VARIABLE;
    e->symbol = var;
    e->ts     = var->ts;

    if (where != NULL)
	e->where = *where;

    if (var->as != NULL) {
	e->rank = var->as->rank;
	e->ref = g95_full_ref(NULL);
    }

    return e;
}



/* g95_build_funcall()-- Return an expression node with an optional
 * argument list attached.  A variable number of g95_expr pointers are
 * strung together in an argument list with a NULL pointer terminating
 * the list. */

g95_expr *g95_build_funcall(g95_symbol *func, ...) {
g95_actual_arglist *tail;
g95_expr *p, *q;
va_list argp;

    p = g95_get_expr();

    p->type                  = EXPR_FUNCTION;
    p->symbol                = func;
    p->value.function.actual = NULL;
    p->rank                  = -1;

    tail = NULL;

    va_start(argp, func);
    for(;;) {
	q = va_arg(argp, g95_expr *);
	if (q == NULL)
	    break;

	if (tail == NULL) 
	    p->value.function.actual = tail = g95_get_actual_arglist();

	else {
	    tail->next = g95_get_actual_arglist();
	    tail = tail->next;
	}

	tail->type = ARG_EXPR;
	tail->u.expr = q;
    }

    va_end(argp);

    return p;
}



/* g95_type_convert_binary()-- Given an expression node with some sort of
 * numeric binary expression, insert type conversions required to make
 * the operands have the same type.
 *
 * The exception is that the operands of an exponential don't have to
 * have the same type.  If possible, the base is promoted to the type
 * of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
 * 1.0**2 stays as it is. */

void g95_type_convert_binary(g95_expr *e) {
g95_expr *op1, *op2;

    op1 = e->value.op.op1;
    op2 = e->value.op.op2;

    if (op1->ts.type == BT_UNKNOWN || op2->ts.type == BT_UNKNOWN) {
	g95_clear_ts(&e->ts);
	return;
    }

/* Kind conversions */

    if (op1->ts.type == op2->ts.type) {
	if (op1->ts.kind == op2->ts.kind) {  /* No type conversions */
	    e->ts = op1->ts;
	    goto done;
	}

	if (op1->ts.kind > op2->ts.kind)
	    g95_convert_type(op2, &op1->ts, 1);
	else
	    g95_convert_type(op1, &op2->ts, 1);

	e->ts = op1->ts;
	goto done;
    }

/* integer combined with complex */

    if (op1->ts.type == BT_COMPLEX && op2->ts.type == BT_INTEGER) {
	e->ts.type = BT_COMPLEX;
	e->ts.kind = op1->ts.kind;

	if (e->value.op.operator != INTRINSIC_POWER)
	    g95_convert_type(e->value.op.op2, &e->ts, 1);

	goto done;
    }

    if (op1->ts.type == BT_INTEGER && op2->ts.type == BT_COMPLEX) {
	e->ts.type = BT_COMPLEX;
	e->ts.kind = op2->ts.kind;

	g95_convert_type(e->value.op.op1, &e->ts, 1);
	goto done;
    }

/* Real combined with complex */

    if (op1->ts.type == BT_COMPLEX && op2->ts.type == BT_REAL) {
	e->ts.type = BT_COMPLEX;

	if (op1->ts.kind >= op2->ts.kind) {
	    e->ts.kind = op1->ts.kind;
	    g95_convert_type(e->value.op.op2, &e->ts, 1);

	} else {
	    e->ts.kind = op2->ts.kind;

	    g95_convert_type(e->value.op.op1, &e->ts, 1);
	    g95_convert_type(e->value.op.op2, &e->ts, 1);
	}

	goto done;
    }

    if (op1->ts.type == BT_REAL && op2->ts.type == BT_COMPLEX) {
	e->ts.type = BT_COMPLEX;

	if (op2->ts.kind >= op1->ts.kind) {
	    e->ts.kind = op2->ts.kind;
	    g95_convert_type(e->value.op.op1, &e->ts, 1);

	} else {
	    e->ts.kind = op1->ts.kind;

	    g95_convert_type(e->value.op.op1, &e->ts, 1);
	    g95_convert_type(e->value.op.op2, &e->ts, 1);
	}

	goto done;
    }

/* Integer combined with real */

    if (op1->ts.type == BT_REAL && op2->ts.type == BT_INTEGER) {
	e->ts.type = BT_REAL;
	e->ts.kind = op1->ts.kind;

	if (e->value.op.operator != INTRINSIC_POWER)
	    g95_convert_type(e->value.op.op2, &e->ts, 1);

	goto done;
    }

    if (op1->ts.type == BT_INTEGER && op2->ts.type == BT_REAL) {
	e->ts.type = BT_REAL;
	e->ts.kind = op2->ts.kind;

	g95_convert_type(e->value.op.op1, &e->ts, 1);
	goto done;
    }

done:
    return;
}



/* g95_is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified. */

int g95_is_constant_expr(g95_expr *e) {
g95_actual_arglist *arg;
int rv;

    if (e == NULL)
	return 1; 

    switch(e->type) {
    case EXPR_OP:
	rv = g95_is_constant_expr(e->value.op.op1) &&
	    (e->value.op.op2 == NULL || g95_is_constant_expr(e->value.op.op2));

	break;

    case EXPR_VARIABLE:
	rv = (g95_check_iter_variable(e) == SUCCESS);
	break;

    case EXPR_FUNCTION:
	rv = 0;
	/* call to intrinsic with at least one argument */
	if (e->value.function.isym && e->value.function.actual) {
	    for(arg = e->value.function.actual; arg; arg = arg->next)
		if (!g95_is_constant_expr(arg->u.expr))
		    break;

	    if (arg == NULL)
		rv = 1;
	}

	break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
	rv = 1;
	break;

    case EXPR_SUBSTRING:
	rv = g95_is_constant_expr(e->ref->u.ss.start) &&
	     g95_is_constant_expr(e->ref->u.ss.end);
	break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
	rv = g95_constant_constructor(e);
	break;

    default:
	g95_internal_error("g95_is_constant_expr(): Unknown expression type");
    }

    return rv;
}



/* simplify_intrinsic_op()-- Try to collapse intrinsic expressions */

static try simplify_intrinsic_op(g95_expr *e) {
g95_expr *op1, *op2, *result;

    if (e->value.op.operator == INTRINSIC_USER)
	return SUCCESS;

    op1 = e->value.op.op1;
    op2 = e->value.op.op2;

    if (g95_simplify_expr(op1) == FAILURE ||
	g95_simplify_expr(op2) == FAILURE)
	return FAILURE;

/* See if it is now possible to determine a type from the operands. */

    if (e->ts.type == BT_UNKNOWN) {
	if (op2 == NULL) {
	    if (op1->ts.type != BT_UNKNOWN)
		e->ts = op1->ts;

	} else if (op1->ts.type != BT_UNKNOWN && op2->ts.type != BT_UNKNOWN)
	    g95_type_convert_binary(e);
    }

    if (e->rank == -1) {
	if (op2 == NULL)
	    e->rank = op1->rank;

	else if (op1->rank != -1 && op2->rank != -1)
	    e->rank = (op1->rank > op2->rank)
		? op1->rank
		: op2->rank;
    }

    if (!g95_is_constant_expr(op1) ||
	(op2 != NULL && !g95_is_constant_expr(op2)))
	return SUCCESS;

/* Rip e apart */

    e->value.op.op1 = NULL;
    e->value.op.op2 = NULL;

    switch(e->value.op.operator) {
    case INTRINSIC_UPLUS:
	result = g95_uplus(op1);
	break;

    case INTRINSIC_UMINUS:
	result = g95_uminus(op1);
	break;

    case INTRINSIC_PLUS:
	result = g95_add(op1, op2);
	break;

    case INTRINSIC_MINUS:
	result = g95_subtract(op1, op2);
	break;

    case INTRINSIC_TIMES:
	result = g95_multiply(op1, op2);
	break;

    case INTRINSIC_DIVIDE:
	result = g95_divide(op1, op2);
	break;

    case INTRINSIC_POWER:
	result = g95_power(op1, op2);
	break;

    case INTRINSIC_CONCAT:
	result = g95_concat(op1, op2);
	break;

    case INTRINSIC_EQ:      
	result = g95_eq(op1, op2);
	break;

    case INTRINSIC_NE:
	result = g95_ne(op1, op2);
	break;

    case INTRINSIC_GT:
	result = g95_gt(op1, op2);
	break;

    case INTRINSIC_GE:
	result = g95_ge(op1, op2);
	break;

    case INTRINSIC_LT:
	result = g95_lt(op1, op2);
	break;

    case INTRINSIC_LE:
	result = g95_le(op1, op2);
	break;

    case INTRINSIC_NOT:
	result = g95_not(op1);
	break;

    case INTRINSIC_AND:
	result = g95_and(op1, op2);
	break;

    case INTRINSIC_OR:
	result = g95_or(op1, op2);
	break;

    case INTRINSIC_EQV:
	result = g95_eqv(op1, op2);
	break;

    case INTRINSIC_NEQV:
	result = g95_neqv(op1, op2);
	break;

    case INTRINSIC_PAREN:
	if (g95_simplify_mode != SIMP_REGULAR) {
	    result = op1;
	    break;
	}

	e->value.op.op1 = op1;
	return SUCCESS;

    default:
	g95_internal_error("simplify_intrinsic_op(): Bad operator"); 
    }

    if (result == NULL) {
	g95_free_expr(op1);
	g95_free_expr(op2);
	return FAILURE;
    }

    result->rank = e->rank;
    g95_replace_expr(e, result);

    return SUCCESS;
}



/* simplify_ref()-- Simplify a reference structure. */

static try simplify_ref(g95_ref *ref) {
try t;
int i;

    t = SUCCESS; 

    switch(ref->type) {
    case REF_ARRAY:
	for(i=0; i<ref->u.ar.dimen; i++) {
	    if (g95_simplify_expr(ref->u.ar.start[i])  == FAILURE) t = FAILURE;
	    if (g95_simplify_expr(ref->u.ar.end[i])    == FAILURE) t = FAILURE;
	    if (g95_simplify_expr(ref->u.ar.stride[i]) == FAILURE) t = FAILURE;
	}

	break;

    case REF_COARRAY:
	for(i=0; i<ref->u.car.dimen; i++)
	    if (g95_simplify_expr(ref->u.car.element[i]) == FAILURE)
		t = FAILURE;

	break;

    case REF_COMPONENT:
	break;

    case REF_SUBSTRING:
	if (g95_simplify_expr(ref->u.ss.start) == FAILURE) t = FAILURE;
	if (g95_simplify_expr(ref->u.ss.end)   == FAILURE) t = FAILURE;
	break;
    }

    return t;
}



/* simplify_constructor()-- Subroutine to simplify constructor
 * expressions.  Mutually recursive with g95_simplify_expr(). */

static try simplify_constructor(g95_constructor *c) {

    for(;c; c=c->next) {
	if (c->iterator &&
	    (g95_simplify_expr(c->iterator->start) == FAILURE ||
	     g95_simplify_expr(c->iterator->end)   == FAILURE ||
	     g95_simplify_expr(c->iterator->step)  == FAILURE))
	    return FAILURE;

	if (c->expr && g95_simplify_expr(c->expr) == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



/* check_inquiry()-- Certain inquiry functions are specifically
 * allowed to have variable arguments, which is an exception to the
 * normal requirement that an initialization function have
 * initialization arguments.  We head off this problem here.  Returns
 * nonzero if we decide that the function is an intrinsic inquiry
 * function. */

static int check_inquiry(g95_expr *e) {
g95_actual_arglist *a, *m;
g95_intrinsic_sym *isym;
g95_symbol *sym;
g95_ref *ref;
g95_expr *f;

    if (e->symbol != NULL || e->value.function.pointer != NULL)
	return 0;

    isym = (e->value.function.isym != NULL)
	? e->value.function.isym
	: g95_find_function(e->value.function.name);

    if (isym == NULL)
	return 0;

    switch(isym->id) {
    case G95_ISYM_BIT_SIZE:
    case G95_ISYM_DIGITS:
    case G95_ISYM_EPSILON:
    case G95_ISYM_HUGE:
    case G95_ISYM_KIND:
    case G95_ISYM_LEN:
    case G95_ISYM_MAXEXPONENT:
    case G95_ISYM_MINEXPONENT:
    case G95_ISYM_PRECISION:
    case G95_ISYM_RADIX:
    case G95_ISYM_RANGE:
    case G95_ISYM_SHAPE:
    case G95_ISYM_TINY:
	if (e->value.function.actual == NULL ||
	    e->value.function.actual->next != NULL)  /* Doesn't have one arg */
	    return 0;
	break;

    case G95_ISYM_SIZE:
    case G95_ISYM_LBOUND:
    case G95_ISYM_UBOUND:
	if (e->value.function.actual == NULL)
	    return 0;

	m = e->value.function.actual;

	if (m->next == NULL) {
	    a = g95_get_actual_arglist();
	    a->type = ARG_EXPR;
	    m->next = a;

	    a = g95_get_actual_arglist();
	    a->type = ARG_EXPR;
	    m->next->next = a;
	    break;
	}

	if (m->next->next == NULL) {
	    a = g95_get_actual_arglist();
	    a->type = ARG_EXPR;
	    m->next->next = a;
	    break;
	}

	if (m->next->next->next != NULL)
	    return 0;

	break;

    default:
	return 0;
    }

    f = e->value.function.actual->u.expr;
    if (f == NULL || (f->type != EXPR_VARIABLE && f->type != EXPR_UNKNOWN))
	return 0;

    if (e->value.function.isym != NULL ||
	g95_generic_name(e->value.function.name))
	return 0;

    /* At this point we have an inquiry function with a variable
     * argument.  The type of the variable might be undefined, but we
     * need it now, because the arguments of these functions are
     * allowed to be undefined. */

    e->value.function.isym = isym;

    if (f->type == EXPR_UNKNOWN)
	f->type = EXPR_VARIABLE;

    sym = f->symbol;

    if (f->ts.type == BT_UNKNOWN) {
	if (isym->id != G95_ISYM_LEN && sym->ts.type == BT_UNKNOWN &&
	    g95_set_default_type(sym, 0, g95_current_ns) == FAILURE)
	    return 0;

	f->ts = sym->ts;
    }

    if (sym->as != NULL && f->ref == NULL) {
	ref = g95_extend_ref(f, REF_ARRAY, &f->where);
	ref->u.ar.type = AR_FULL;
    }

    g95_intrinsic_symbol(isym->name, 1, &e->where);

    if (e->ts.type == BT_UNKNOWN) {
	e->ts.type = BT_INTEGER;
	e->ts.kind = g95_default_integer_kind(1);
    }

    return 1;
}



/* bad_init_function()-- Make sure that the function is allowed in an
 * initialization expression.  Returns nonzero if bad. */

static int bad_init_function(g95_expr *e) {
int id;

    id = (e->value.function.isym != NULL)
	? e->value.function.isym->id
	: g95_find_id(e->value.function.name);

    switch(id) {
    case G95_ISYM_ANY:         case G95_ISYM_ALL:      case G95_ISYM_ALLOCATED:
    case G95_ISYM_ASSOCIATED:  case G95_ISYM_COUNT:    case G95_ISYM_CSHIFT:
    case G95_ISYM_DOT_PRODUCT: case G95_ISYM_EOSHIFT:  case G95_ISYM_MATMUL:
    case G95_ISYM_MAXLOC:      case G95_ISYM_MAXVAL:   case G95_ISYM_MINVAL:
    case G95_ISYM_MINLOC:      case G95_ISYM_PACK:     case G95_ISYM_PRODUCT:
    case G95_ISYM_SPREAD:      case G95_ISYM_SUM:      case G95_ISYM_TRANSPOSE:
    case G95_ISYM_UNPACK:
	return 1;

    default:
	break;
    }

    if (g95_option.fmode == 0)
	return 0;

    switch(id) {
    case G95_ISYM_TRANSPOSE:
	return 1;

    default:
	break;
    }

    return 0;
}



/* g95_check_parameter()-- At this point, we're trying to simplify an
 * expression involving a parameter.  Make sure the parameter has a
 * type, defaulting it if necessary and possibly converting the value
 * to this type. */

try g95_check_parameter(g95_symbol *sym) {
g95_expr *e;
int i;

    if (sym->as != NULL)
	for(i=0; i<sym->as->rank; i++)
	    if (!g95_is_constant_expr(sym->as->lower[i]) ||
		!g95_is_constant_expr(sym->as->upper[i])) {
		g95_error("Parameter array '%s' at %L must have "
			  "constant bounds", sym->name, &sym->declared_at);
		return FAILURE;
	    }


    if (sym->ts.type == BT_UNKNOWN &&
	g95_set_default_type(sym, 1, sym->ns) == FAILURE)
	return FAILURE;

    if (sym->value == NULL || sym->attr.current)
	return SUCCESS;

    e = g95_assign_boz(&sym->ts, sym->value);
    if (e != NULL) {
	g95_free_expr(sym->value);
	sym->value = e;
    }

    if (g95_compare_types(&sym->ts, &sym->value->ts))
	return SUCCESS;

    return g95_convert_type(sym->value, &sym->ts, 0);
}



/* simplify_range()-- Simplify a start or end variable in a range.
 * Returns NULL if simplification was not possible. */

static try simplify_range(g95_expr *e, int d, int *result) {
try r;

    if (e == NULL) {
	*result = d;
	return SUCCESS;
    }

    e = g95_copy_expr(e);

    if (e->type == EXPR_CONSTANT) {
	*result = bi_to_int(e->value.integer);
	r = SUCCESS;

    } else if (g95_simplify_expr(e) == FAILURE || e->type != EXPR_CONSTANT)
	r = FAILURE;

    else {
	*result = bi_to_int(e->value.integer);
	r = SUCCESS;
    }
  
    g95_free_expr(e);
    return r;
}



/* simplify_substring()-- Simplify a substring of a constant string.
 * Returns NULL if the substring cannot be calculated. */

static g95_expr *simplify_substring(g95_expr *e, g95_ref *ref) {
int n, start, end;
g95_expr *f;
char *p;

    if (simplify_range(ref->u.ss.start, 1, &start) == FAILURE)
	return NULL;

    if (simplify_range(ref->u.ss.end,
		       e->value.character.length, &end) == FAILURE) 
	return NULL;

    if (start < 0)
	start = 0;

    if (end > e->value.character.length)
	end = e->value.character.length;

    n = end - start + 1;
    if (n < 0)
	n = 0;

    p = e->value.character.string;
    f = g95_char_expr(n, e->ts.kind, &ref->where);

    if (n > 0)
	memcpy(f->value.character.string, p+start-1, n);

    return f;
}



/* simplify_array_substring()-- Simplify substrings of an array
 * constructor. */

static g95_expr *simplify_array_substring(g95_expr *array, g95_ref *ref) {
g95_expr *e, *result;
g95_constructor *c;

    result = g95_start_constructor(&array->ts, &array->where);

    for(c=array->value.constructor.c; c; c=c->next) {
	e = simplify_substring(c->expr, ref);
	if (e == NULL) {
	    g95_free_expr(result);
	    return NULL;
	}

	g95_append_constructor(result, e);
    }

    return result;
}



/* simplify_array_element()-- Extract an element from an array */

static g95_expr *simplify_array_element(g95_expr *array, g95_array_spec *as,
					g95_array_ref *ar, g95_locus *where) {
bignum offset, mult, t;
g95_expr *result;
int i;

    if (ar->dimen != as->rank) {
	g95_error("Wrong number of dimensions in array reference at %L",
		  where);
	return NULL;
    }

    if (array->type == EXPR_CONSTANT || array->type == EXPR_STRUCTURE) {
	result = g95_copy_expr(array);
	result->rank = 0;
	return result;
    }

    for(i=0; i<ar->dimen; i++) {
	if (ar->start[i]->type != EXPR_CONSTANT)
	    return NULL;

	if (bi_compare(ar->start[i]->value.integer,
		       as->lower[i]->value.integer) < 0 ||
	    bi_compare(ar->start[i]->value.integer,
		       as->upper[i]->value.integer) > 0) {
	    g95_error("Array reference out of bounds at %L", where);
	    return NULL;
	}
    }

    offset = bi_0;
    mult = bi_1;
    t = bi_0;

    for(i=0; i<ar->dimen; i++) {
	t = bi_subtract(ar->start[i]->value.integer,
			as->lower[i]->value.integer);
	t = bi_multiply(t, big_copy(mult));

	offset = bi_add(offset, t);

	if (i != ar->dimen-1) {
	    t = bi_subtract(as->upper[i]->value.integer,
			    as->lower[i]->value.integer);
	    t = bi_int_add(t, 1);

	    if (bi_is_negative(big_copy(t))) {
		big_free(t);
		t = bi_0;
	    }

	    mult = bi_multiply(mult, t);
	}
    }

    result = g95_get_array_element(array, offset);
    big_free(mult);

    return result;
}



/* vector_subscript()-- Evaluate a vector subscript, which amounts to
 * returning an index number. */

static bignum vector_subscript(g95_expr *v, bignum n) {
g95_array_spec *as;
g95_array_ref ar;
int i, k, rank;
g95_ref *ref;
g95_expr *e;
bignum t;

    switch(v->type) {
    case EXPR_VARIABLE:
	ref = v->ref;
	if (ref->type != REF_ARRAY)
	    g95_internal_error("vector_subscript(): Bad ref");

	as = v->symbol->as;
	rank = as->rank;

	k = g95_default_integer_kind(0);
	memset(&ar, '\0', sizeof(ar));
	ar.type = AR_ELEMENT;
	ar.dimen = rank;

	switch(ref->u.ar.type) {
	case AR_FULL:
	    ar.dimen_type[0] = DIMEN_ELEMENT;
	    ar.start[0] = g95_constant_result(BT_INTEGER, k, &v->where);

	    t = bi_add(as->lower[0]->value.integer, n);
	    ar.start[0]->value.integer = bi_int_subtract(t, -1);
	    big_permanent(ar.start[0]->value.integer);
	    break;

	case AR_SECTION:
	    for(i=0; i<rank; i++) {
		ar.dimen_type[i] = DIMEN_ELEMENT;

		switch(ref->u.ar.dimen_type[i]) {
		case DIMEN_ELEMENT:
		    ar.start[i] = g95_copy_expr(ref->u.ar.start[i]);
		    break;

		case DIMEN_VECTOR:
		    ar.start[i] =
			g95_constant_result(BT_INTEGER, k, &v->where);

		    ar.start[i]->value.integer =
			vector_subscript(ref->u.ar.start[i], n);

		    big_permanent(ar.start[i]->value.integer);
		    break;

		default:
		    g95_internal_error("vector_subscript(): Bad dimen");
		}
	    }
	
	    break;

	default:
	    g95_internal_error("vector_subscript(): Bad array ref");
	    break;
	}

	e = simplify_array_element(v, as, &ar, &v->where);

	for(i=0; i<rank; i++)
	    g95_free_expr(ar.start[i]);

	if (e == NULL || e->type != EXPR_CONSTANT)
	    t = NULL;

	else
	    t = big_clone(e->value.integer);

	g95_free_expr(e);
	break;

    case EXPR_ARRAY:
	e = g95_get_array_element(v, n);
	t = big_clone(e->value.integer);
	g95_free_expr(e);
	break;

    default:
	t = NULL;
	g95_internal_error("vector_subscript(): Bad expr");
    }

    return t;
}



/* section_ref0()-- Extract a section prior to looping over it. */

static try section_ref0(int i, g95_array_ref *ar, g95_array_spec *as,
			array_range *r) {
g95_ref *ref;
g95_expr *e;
int j;

    if (ar->type == AR_FULL) {
	if (as->lower[i]->type != EXPR_CONSTANT ||
	    as->upper[i]->type != EXPR_CONSTANT)
	    return FAILURE;

	r->lower = as->lower[i]->value.integer;
	r->upper = as->upper[i]->value.integer;
	r->stride = bi_1;

    } else {  /* Section reference */
	switch(ar->dimen_type[i]) {
	case DIMEN_ELEMENT:
	    if (ar->start[i]->type != EXPR_CONSTANT)
		return FAILURE;

	    r->lower = ar->start[i]->value.integer;
	    r->upper = ar->start[i]->value.integer;
	    r->stride = bi_1;
	    break;

	case DIMEN_RANGE:
	    if (ar->start[i] == NULL) {
		if (as->lower[i]->type != EXPR_CONSTANT)
		    return FAILURE;

		r->lower = as->lower[i]->value.integer;

	    } else {
		if (ar->start[i]->type != EXPR_CONSTANT)
		    return FAILURE;

		r->lower = ar->start[i]->value.integer;
	    }

	    if (ar->end[i] == NULL) {
		if (as->upper[i]->type != EXPR_CONSTANT)
		    return FAILURE;

		r->upper = as->upper[i]->value.integer;

	    } else {
		if (ar->end[i]->type != EXPR_CONSTANT)
		    return FAILURE;

		r->upper = ar->end[i]->value.integer;
	    }

	    if (ar->stride[i] == NULL)
		r->stride = bi_1;

	    else {
		if (ar->stride[i]->type != EXPR_CONSTANT)
		    return FAILURE;

		r->stride = ar->stride[i]->value.integer;
	    }

	    break;

	case DIMEN_VECTOR:
	    e = ar->start[i];

	    switch(e->type) {
	    case EXPR_VARIABLE:
		as = e->symbol->as;

		for(ref=e->ref; ref; ref=ref->next)
		    switch(ref->type) {
		    case REF_ARRAY:
			switch(ref->u.ar.type) {
			case AR_FULL:
			    return section_ref0(0, &ref->u.ar, as, r);

			case AR_SECTION:
			    for(j=0; j<ar->dimen; j++)
				if (ref->u.ar.dimen_type[j] == DIMEN_RANGE ||
				    ref->u.ar.dimen_type[j] == DIMEN_VECTOR)
				    return section_ref0(j, &ref->u.ar, as, r);

			    g95_internal_error("section_ref0(): "
					       "Section not found");

			default:
			    g95_internal_error("section_ref0(): Bad ref (2)");
			}

		    case REF_COMPONENT:
			as = ref->u.c.component->as;
			break;

		    default:
			break;
		    }

		break;

	    case EXPR_ARRAY:
		r->lower = bi_1;
		r->upper = g95_array_size(e);
		r->stride = bi_1;

		if (r->upper == NULL)
		    return FAILURE;

		break;

	    default:
		g95_internal_error("section_ref0(): Bad type");
	    }

	    break;

	default:
	    g95_internal_error("section_ref0(): Bad dimen");
	}
    }

    return SUCCESS;
}



/* simplify_array_section()-- Expand an array section. */

static g95_expr *simplify_array_section(g95_expr *array, g95_array_spec *as,
					g95_array_ref *ar, g95_locus *where) {
g95_expr *e, *m, ar_expr[G95_MAX_DIMENSIONS];
array_range v[G95_MAX_DIMENSIONS];
g95_array_ref a;
int rank, i;
bignum t, u;

    e = g95_start_constructor(&array->ts, where);

    memset(&a, '\0', sizeof(a));
    memset(&v, '\0', sizeof(v));
    memset(&ar_expr, '\0', sizeof(ar_expr));

    a.type = AR_ELEMENT;
    a.dimen = rank = as->rank;

    switch(ar->type) {
    case AR_FULL:
	e->rank = as->rank;
	break;

    case AR_SECTION:
	e->rank = 0;
	for(i=0; i<rank; i++)
	    if (ar->dimen_type[i] == DIMEN_RANGE ||
		ar->dimen_type[i] == DIMEN_VECTOR)
		e->rank++;

	break;

    default:
	g95_internal_error("simplify_array_section(): Bad type");
    }

    for(i=0; i<rank; i++) {
	ar_expr[i].type    = EXPR_CONSTANT;
	ar_expr[i].ts.type = BT_INTEGER;
	ar_expr[i].ts.kind = g95_default_integer_kind(1);

	a.start[i] = &ar_expr[i];
	a.dimen_type[i] = DIMEN_ELEMENT;

	if (section_ref0(i, ar, as, &v[i]) == FAILURE) {
	    g95_free_expr(e);
	    e = NULL;
	    goto done;
	}

	v[i].count = bi_0;

	t = bi_subtract(big_copy(v[i].upper), big_copy(v[i].lower));
	t = bi_add(t, big_copy(v[i].stride));

	v[i].trip = bi_divide(t, big_copy(v[i].stride));

	if (bi_compare(big_copy(v[i].trip), bi_0) <= 0)
	    goto done;

	v[i].value = big_copy(v[i].lower);
    }

    for(;;) {
	for(i=0; i<rank; i++) {
	    if (ar->dimen_type[i] != DIMEN_VECTOR)
		ar_expr[i].value.integer = big_clone(big_copy(v[i].value));

	    else {
		u = big_copy(v[i].value);
		if (ar->start[i]->type == EXPR_ARRAY)
		    u = bi_subtract(u, v[i].lower);

		t = vector_subscript(ar->start[i], u);
		if (t == NULL) {
		    g95_free_expr(e);
		    e = NULL;
		    goto done;
		}

		ar_expr[i].value.integer = t;
	    }

	    big_permanent(ar_expr[i].value.integer);
	}

	m = simplify_array_element(array, as, &a, where);

	for(i=0; i<rank; i++) {
	    big_depermanent(ar_expr[i].value.integer);
	    big_free(ar_expr[i].value.integer);
	    ar_expr[i].value.integer = NULL;
	}

	if (m == NULL) {
	    g95_free_expr(e);
	    e = NULL;
	    goto done;
	}

	g95_append_constructor(e, m);

	i = 0;
	for(;;) {
	    v[i].value = bi_add(v[i].value, big_copy(v[i].stride));
	    v[i].count = bi_add(v[i].count, bi_1);

	    if (bi_compare(big_copy(v[i].count), big_copy(v[i].trip)) != 0)
		break;

	    big_free(v[i].value);
	    v[i].value = big_copy(v[i].lower);

	    big_free(v[i].count);
	    v[i].count = bi_0;

	    if (++i >= rank)
		goto done;
	}
    }

done:
    for(i=0; i<rank; i++) {
	if (v[i].lower != NULL)
	    big_free(v[i].lower);

	if (v[i].upper != NULL)
	    big_free(v[i].upper);

	if (v[i].stride != NULL)
	    big_free(v[i].stride);

	if (v[i].count != NULL)
	    big_free(v[i].count);

	if (v[i].trip != NULL)
	    big_free(v[i].trip);

	if (v[i].value != NULL)
	    big_free(v[i].value);

	if (ar_expr[i].value.integer != NULL) {
	    big_depermanent(ar_expr[i].value.integer);
	    big_free(ar_expr[i].value.integer);
	}
    }

    return e;
}



/* simplify_array_ref()-- Simplify an array references. */

static g95_expr *simplify_array_ref(g95_expr *array, g95_array_spec *as,
				    g95_array_ref *ar, g95_locus *where) {
g95_expr *e;
int i;

    if (array->type != EXPR_ARRAY && array->type != EXPR_CONSTANT &&
	array->type != EXPR_STRUCTURE && array->type != EXPR_FUNCTION) {
	g95_error("Can't evaluate array reference to non-array at %L", where);
	return NULL;
    }

    if (as->type != AS_EXPLICIT || array->type == EXPR_FUNCTION)
	return NULL;

    for(i=0; i<as->rank; i++)
	if (as->lower[i]->type != EXPR_CONSTANT ||
	    as->upper[i]->type != EXPR_CONSTANT)
	    return NULL;

    switch(ar->type) {
    case AR_UNKNOWN:
	return NULL;

    case AR_SECTION:
    case AR_FULL:
	if (g95_resolve_array_ref(ar, as, &array->where) == FAILURE)
	    return NULL;

	e = simplify_array_section(array, as, ar, where);
	break;

    case AR_ELEMENT:
	if (ar->dimen != as->rank) {
	    g95_error("Wrong number of dimensions in array reference at %L",
		      where);
	    return NULL;
	}

	e = simplify_array_element(array, as, ar, where);
	break;

    default:
	g95_internal_error("simplify_array_ref(): Bad ref type");
	e = NULL;
    }

    return e;
}



/* simplify_component0()-- Simplify a component reference given a
 * structure constructor. */

static g95_expr *simplify_component0(g95_expr *e, g95_array_spec **as_p,
				     g95_ref *ref) {
g95_constructor *cons;
g95_component *c;

    cons = e->value.constructor.c;

    for(c=e->ts.derived->components; c; c=c->next, cons=cons->next)
	if (strcmp(c->name, ref->u.c.name) == 0)
	    break;

    if (c == NULL) {
	g95_error("Component '%s' at not found %L",
		  ref->u.c.name, &ref->where);
	return NULL;
    }

    if (as_p != NULL)
	*as_p = c->as;

    return g95_copy_expr(cons->expr);
}



/* simplify_component()-- */

static g95_expr *simplify_component(g95_expr *e, g95_array_spec **as_p,
				    g95_ref *ref) {
g95_component *c;
g95_expr *b, *f;
bignum n, m;

    switch(e->type) {
    case EXPR_STRUCTURE:
	return simplify_component0(e, as_p, ref);

    case EXPR_ARRAY:
	break;

    default:
	g95_error("Component reference for non-derived type at %L",
		  &ref->where);
	return NULL;
    }

    for(c=e->ts.derived->components; c; c=c->next)
	if (strcmp(c->name, ref->u.c.name) == 0)
	    break;

    if (c == NULL) {
	g95_error("Component '%s' at not found %L",
		  ref->u.c.name, &ref->where);
	return NULL;
    }

    *as_p = c->as;
    f = g95_start_constructor(&c->ts, &e->where);

    n = g95_array_size(e);
    m = bi_0;

    while(bi_compare(big_copy(m), big_copy(n)) < 0) {
	b = g95_get_array_element(e, big_copy(m));

	g95_append_constructor(f, simplify_component0(b, NULL, ref));
	g95_free_expr(b);
	m = bi_add(m, bi_1);
    }

    big_free(m);
    big_free(n);

    return f;
}



/* expand_ref()-- Expand a sequence of ref structures. */

static g95_expr *expand_ref(g95_expr *e, g95_ref *r, g95_array_spec *as) {
g95_ref *ref;
g95_expr *f;
int m;

    m = 1;

    for(ref=r; ref; ref=ref->next) {
	switch(ref->type) {
	case REF_ARRAY:
	    f = simplify_array_ref(e, as, &ref->u.ar, &ref->where);
	    as = NULL;
	    break;

	case REF_COMPONENT:
	    f = simplify_component(e, &as, ref);
	    if (f == NULL)
		goto error;

	    break;

	case REF_SUBSTRING:
	    switch(e->type) {
	    case EXPR_CONSTANT:
		f = simplify_substring(e, ref);
		break;

	    case EXPR_ARRAY:
		f = simplify_array_substring(e, ref);
		break;

	    default:
		f = NULL;
		break;
	    }

	    break;

	case REF_COARRAY:
	    f = NULL;
	    break;

	default:
	    g95_internal_error("expand_ref(): Bad ref");
	    f = NULL;
	    break;
	}

	if (!m)
	    g95_free_expr(e);

	m = 0;
	e = f;
	if (e == NULL)
	    break;
    }

    if (m)
	e = g95_copy_expr(e);

    return e;

error:
    if (!m)
	g95_free_expr(e);

    return NULL;
}



/* simplify_variable()-- Simplify a variable reference. */

static try simplify_variable(g95_expr *e) {
g95_symbol *sym;
g95_expr *rhs;
g95_annot *a;
g95_ref *ref;

    for(ref=e->ref; ref; ref=ref->next)
	if (simplify_ref(ref) == FAILURE)
	    return FAILURE;

    sym = e->symbol;

    if (sym->attr.flavor == FL_PARAMETER && sym->value == NULL) {
	g95_error("PARAMETER value is used before being defined at %L",
		  &e->where);
	return FAILURE;
    }

    if (sym->attr.flavor != FL_PARAMETER || sym->ts.type == BT_UNKNOWN)
	return SUCCESS;

    g95_set_usage(sym, &e->where, 0, 1);

    if (e->rank == -1 && sym->as != NULL && e->ref == NULL)
	e->rank = sym->as->rank;

    /* Don't reduce vector valued parameters outside of initialization
     * expressions.  Prevents extra temporaries from being generated. */

    if (e->rank > 0 && g95_simplify_mode == SIMP_REGULAR)
	return SUCCESS;

    if (sym->value->rank == 0 && e->ts.type == BT_DERIVED &&
	e->ts.derived->attr.itype == ITYPE_NONE && e->ref == NULL &&
	g95_simplify_mode == SIMP_REGULAR)
	return SUCCESS;

    a = g95_annotate(ANNOT_PARAMETER, &e->where);
    a->u.sym = sym;

    rhs = expand_ref(sym->value, e->ref, sym->as);
    if (rhs != NULL)
	g95_replace_expr(e, rhs);

    return SUCCESS;
}



/* g95_simplify_expr()-- Given an expression, simplify it by collapsing
 * constant expressions.  Most simplification takes place when the
 * expression tree is being constructed.  If an intrinsic function is
 * simplified at some point, we get called again to collapse the
 * result against other constants.
 *
 * We work by recursively simplifying expression nodes, simplifying
 * intrinsic functions where possible, which can lead to further
 * constant collapsing.  If an operator has constant operand(s), we
 * rip the expression apart, and rebuild it, hoping that it becomes
 * something simpler. */

try g95_simplify_expr(g95_expr *e) {
g95_actual_arglist *ap;
g95_locus where;
g95_ref *ref;
g95_expr *f;
try rc;

    if (e == NULL)
	return SUCCESS;

    where = e->where;

/* Replace a parameter variable with its value */

    switch(e->type) {
    case EXPR_CONSTANT:
    case EXPR_NULL:
	rc = SUCCESS;
	break;

    case EXPR_OP:
	rc = simplify_intrinsic_op(e);
	break;

    case EXPR_FUNCTION:
	rc = SUCCESS;

	if (!check_inquiry(e))
	    for(ap=e->value.function.actual; ap; ap=ap->next) {
		rc = g95_simplify_expr(ap->u.expr);
		if (rc == FAILURE)
		    break;
	    }

	if (e->value.function.isym != NULL &&
	    g95_intrinsic_func_interface(e, 1) == MATCH_ERROR)
	    rc = FAILURE;

	break;

    case EXPR_PROCEDURE:
	rc = SUCCESS;
	break;

    case EXPR_VARIABLE:
    case EXPR_UNKNOWN:
	rc = SUCCESS;
	g95_simplify_iterator_var(e);

	if (e->type == EXPR_VARIABLE)
	    rc = simplify_variable(e);

	break;

    case EXPR_STRUCTURE:
	rc = simplify_constructor(e->value.constructor.c);
	break;

    case EXPR_ARRAY:
	if (g95_simplify_mode != SIMP_INIT)
	    rc = simplify_constructor(e->value.constructor.c);

	else {
	    rc = g95_expand_constructor(e);
	    if (rc == SUCCESS && !g95_expanded_ac(e))
		rc = FAILURE;
	}

	break;

    case EXPR_SUBSTRING:
	rc = SUCCESS;
	for(ref=e->ref; ref; ref=ref->next) {
	    rc = simplify_ref(ref);
	    if (rc == FAILURE)
		break;
	}

	if (rc == SUCCESS) {
	    f = simplify_substring(e, e->ref);
	    if (f != NULL)
		g95_replace_expr(e, f);
	}

	break;

    default:
	rc = FAILURE;
	g95_internal_error("g95_simplify_expr(): Bad expression");
    }

    e->where = where;
    return rc;
}



/* g95_simplify_spec_expr()-- Simplify a specification expression */

try g95_simplify_spec_expr(g95_expr *e) {
simp_t save;
try t;

    save = g95_simplify_mode;
    g95_simplify_mode = SIMP_SPEC;

    t = g95_simplify_expr(e);

    g95_simplify_mode = save;
    return t;
}



/* g95_simplify_init_expr()-- Simplify an initialization expression */

try g95_simplify_init_expr(g95_expr *e) {
simp_t save;
try t;

    if (g95_resolve_expr(e) == FAILURE)
	return FAILURE;

    save = g95_simplify_mode;
    g95_simplify_mode = SIMP_INIT;

    t = g95_simplify_expr(e);

    g95_simplify_mode = save;
    return t;
}



/* et0()-- Returns the type of an expression with the exception that
 * iterator variables are automatically integers no matter what else
 * they may be declared as. */

static bt et0(g95_expr *e) {

    if (e->type == EXPR_VARIABLE && g95_check_iter_variable(e) == SUCCESS)
	return BT_INTEGER;

    return e->ts.type;
}



/* check_intrinsic_op()-- Check an intrinsic arithmetic operation to
 * see if it is consistent with some type of expression. */

static try check_intrinsic_op(g95_expr *e, try (*check_function)(g95_expr *)) {
g95_expr *op1, *op2;
int relational;
 
    if ((*check_function)(e->value.op.op1) == FAILURE)
	return FAILURE;

    relational = 0;
    op1 = e->value.op.op1;
    op2 = e->value.op.op2;

    switch(e->value.op.operator) {
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
	if (!numeric_type(et0(op1)))
	    goto not_numeric;

	break;

    case INTRINSIC_EQ:  case INTRINSIC_NE:  case INTRINSIC_GT:
    case INTRINSIC_GE:  case INTRINSIC_LT:  case INTRINSIC_LE:
	relational = 1;
	/* Fall through */

    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:
	if ((*check_function)(op2) == FAILURE)
	    return FAILURE;

	if ((!relational || et0(op1) != BT_CHARACTER ||
	     et0(op2) != BT_CHARACTER) && 
	    (!numeric_type(et0(op1)) || !numeric_type(et0(op2))))
	    goto not_numeric;

	if (e->value.op.operator != INTRINSIC_POWER)
	    break;

	if (check_function == check_init_expr && et0(op2) != BT_INTEGER) {
	    g95_error("Exponent at %L must be INTEGER for an initialization "
		      "expression", &op2->where);
	    return FAILURE;
	}

	break;

    case INTRINSIC_CONCAT:
	if ((*check_function)(op2) == FAILURE)
	    return FAILURE;

	if (et0(op1) != BT_CHARACTER || et0(op2) != BT_CHARACTER) {
	    g95_error("Concatenation operator in expression at %L "
		      "must have two CHARACTER operands", &op1->where);
	    return FAILURE;
	}

	if (op1->ts.kind != op2->ts.kind) {
	    g95_error("Concat operator at %L must concatenate strings of the "
		      "same kind", &e->where);
	    return FAILURE;
	}

	break;

    case INTRINSIC_NOT:
	if (et0(op1) != BT_LOGICAL) {
	    g95_error(".NOT. operator in expression at %L must have a LOGICAL "
		      "operand", &op1->where);
	    return FAILURE;
	}

	break;

    case INTRINSIC_AND:    case INTRINSIC_OR:
    case INTRINSIC_EQV:    case INTRINSIC_NEQV:
	if ((*check_function)(op2) == FAILURE)
	    return FAILURE;

	if (et0(op1) != BT_LOGICAL || et0(op2) != BT_LOGICAL) {
	    g95_error("LOGICAL operands are required in expression at %L",
		      &e->where);
	    return FAILURE;
	}

	break;

    case INTRINSIC_PAREN:
	break;

    default:
	g95_error("Only intrinsic operators can be used in expression at %L",
		  &e->where);
	return FAILURE;
    }

    return SUCCESS;

not_numeric:
    g95_error("Numeric operands are required in expression at %L", &e->where);

    return FAILURE;
}



/* check_init_expr()-- Verify that an expression is an
 * initialization expression.  A side effect is that the expression
 * tree is reduced to a single constant node if all goes well.  This
 * would normally happen when the expression is constructed but
 * function references are assumed to be intrinsics in the context of
 * initialization expressions.  If FAILURE is returned an error
 * message has been generated. */

try check_init_expr(g95_expr *e) {
g95_actual_arglist *ap;
g95_isym_id id;
g95_ref *ref;
match m;
int i;
try t;

    if (e == NULL)
	return SUCCESS;

    switch(e->type) {
    case EXPR_OP:
	t = check_intrinsic_op(e, check_init_expr);
	if (t == SUCCESS)
	    t = g95_simplify_expr(e);

	break;

    case EXPR_VARIABLE:
	t = SUCCESS;

	if (g95_check_iter_variable(e) == SUCCESS)
	    break;

	if (g95_simplify_expr(e) == FAILURE) {
	    t = FAILURE;
	    break;
	}

	if (e->type != EXPR_VARIABLE)
	    break;

	if (e->symbol->attr.flavor != FL_PARAMETER) {
	    g95_error("Variable '%s' at %L cannot appear in an initialization "
		      "expression", e->symbol->name, &e->where);
	    t = FAILURE;
	    break;
	}

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (check_init_expr(ref->u.ar.start[i])  == FAILURE ||
			check_init_expr(ref->u.ar.end[i])    == FAILURE ||
			check_init_expr(ref->u.ar.stride[i]) == FAILURE)
			return FAILURE;

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (check_init_expr(ref->u.car.element[i]) == FAILURE)
			return FAILURE;

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		if (check_init_expr(ref->u.ss.start) == FAILURE ||
		    check_init_expr(ref->u.ss.end)   == FAILURE)
		    return FAILURE;

		break;
	    }

	t = g95_simplify_expr(e);
	break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
	t = SUCCESS;
	break;

    case EXPR_FUNCTION:
	t = SUCCESS;

	if (check_inquiry(e)) {
	    id = (e->value.function.isym != NULL)
		? e->value.function.isym->id
		: g95_find_id(e->value.function.name);

	    switch(id) {
	    case G95_ISYM_SIZE:
	    case G95_ISYM_LBOUND:
	    case G95_ISYM_UBOUND:
		if (e->value.function.actual->next != NULL &&
		    check_init_expr(e->value.function.actual->next->u.expr)
		         == FAILURE)
		    t = FAILURE;
		break;

	    default:
		break;
	    }

	} else if (bad_init_function(e)) {
	    g95_error("Function '%s' cannot appear in an initialization "
		      "expression at %L", e->value.function.name, &e->where);

	    t = FAILURE;

	} else {
	    t = SUCCESS;
	    for(ap=e->value.function.actual; ap; ap=ap->next)
		if (check_init_expr(ap->u.expr) == FAILURE) {
		    t = FAILURE;
		    break;
		}
	}

	if (t == SUCCESS) {
	    m = g95_intrinsic_func_interface(e, 0);

	    if (m == MATCH_NO)
		g95_error("Function '%s' in initialization expression at %L "
			  "must be an intrinsic function",
			  e->value.function.name, &e->where);

	    if (m != MATCH_YES)
		t = FAILURE;
	}

	break;

    case EXPR_SUBSTRING:
	ref = e->ref;

	t = check_init_expr(ref->u.ss.start);
	if (t == FAILURE)
	    break;

	t = check_init_expr(ref->u.ss.end);
	if (t == SUCCESS)
	    t = g95_simplify_expr(e);

	break;

    case EXPR_STRUCTURE:
	t = g95_check_constructor(e, check_init_expr);
	break;

    case EXPR_ARRAY:
	t = g95_check_constructor(e, check_init_expr);
	if (t == FAILURE)
	    break;

	t = g95_check_constructor_type(e);
	break;

    default:
	g95_internal_error("check_init_expr(): Unknown expression type");
    }

    return t;
}



/* g95_check_init_expr()-- Top level function for validating an
 * initialization expression. */

try g95_check_init_expr(g95_expr *e) {
simp_t save;
try t;

    save = g95_simplify_mode;
    g95_simplify_mode = SIMP_INIT;

    t = g95_resolve_expr(e);
    if (t == FAILURE)
	goto done;

    t = g95_simplify_expr(e);
    if (t == FAILURE)
	goto done;

    t = check_init_expr(e);
    if (t == FAILURE)
	goto done;

    if (!g95_is_constant_expr(e)) {
	g95_error("Expression at %C is not an initialization expression");
	t = FAILURE;
    }

done:
    g95_simplify_mode = save;
    return t;
}



/* g95_match_init_expr()-- Match an initialization expression.  We work
 * by first matching an expression, then reducing it to a constant */

match g95_match_init_expr(g95_expr **result) {
match m;
try t;

    m = g95_match_expr(result);
    if (m != MATCH_YES)
	return m;

    t = g95_simplify_expr(*result);
    if (t == SUCCESS)
	t = g95_check_init_expr(*result);

    if (t == FAILURE) {
	g95_free_expr(*result);
	*result = NULL;
    }

    return (t == SUCCESS) ? MATCH_YES : MATCH_ERROR;
}



/* restricted_args()-- Given an actual argument list, test to see that
 * each argument is a restricted expression and optionally if the
 * expression type is integer or character */

static try restricted_args(g95_actual_arglist *a, int check_type) {
bt type;

    for(; a; a=a->next) {
	if (check_restricted(a->u.expr) == FAILURE)
	    return FAILURE;

	if (!check_type || a->u.expr == NULL)
	    continue;

	type = a->u.expr->ts.type;
	if (type != BT_CHARACTER && type != BT_INTEGER) {
	    g95_error("Function argument at %L must be of type "
		      "INTEGER or CHARACTER", &a->u.expr->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* external_spec_function()-- Make sure a non-intrinsic function is a
 * specification function. */

static try external_spec_function(g95_expr *e) {
g95_symbol *f;

    f = e->symbol;

    if (f->attr.proc == PROC_ST_FUNCTION) {
	g95_error("Specification function '%s' at %L cannot be a statement "
		  "function", f->name, &e->where);
	return FAILURE;
    }

    if (f->attr.proc == PROC_INTERNAL) {
	g95_error("Specification function '%s' at %L cannot be an internal "
		  "function", f->name, &e->where);
	return FAILURE;
    }

    if (!f->attr.pure && !f->attr.elemental) {
	g95_error("Specification function '%s' at %L must be PURE", f->name,
		  &e->where);
	return FAILURE;
    }

    if (f->attr.recursive) {
	g95_error("Specification function '%s' at %L cannot be RECURSIVE",
		  f->name, &e->where);
	return FAILURE;
    }

    return restricted_args(e->value.function.actual, 0);
}



/* restricted_null()-- Check the NULL() function as part of a
 * restricted expression.  NULL is always OK. */

static try restricted_null(g95_expr *e) {

    e = NULL;
    return SUCCESS;
}



/* restricted_len()-- Check the LEN() function as part of a restricted
 * expression.  The argument of LEN can be a character variable that
 * isn't a restricted expression, but in which the length is a
 * restricted expression.  If the variable already exists, then the
 * length was already a specification expression. */

static try restricted_len(g95_expr *e) {
g95_expr *arg;

    arg = e->value.function.actual->u.expr;

    if (arg->type == EXPR_VARIABLE && arg->ts.type == BT_CHARACTER)
	return SUCCESS;

    return restricted_args(e->value.function.actual, 1);
}



/* restricted_array_inquiry()-- Check an array inquiry function as
 * part of a restricted expression.  The array argument does not have
 * to be a restricted variable, but in that case, the array must not
 * be allocatable or a dummy pointer array. */

static try restricted_array_inquiry(g95_expr *e) {
g95_actual_arglist *a;
g95_expr *array;
g95_symbol *sym;

    a = e->value.function.actual; 
    array = a->u.expr;

    if (array->type == EXPR_VARIABLE) {
	sym = array->symbol;
	if (!sym->attr.allocatable && (!sym->attr.pointer || sym->attr.dummy))
	    a = a->next;  /* skip array arg */
    }

    return restricted_args(a, 0);
}



/* restricted_intrinsic()-- Check to see that a function reference to
 * an intrinsic is a restricted expression.  Some functions required
 * by the standard are omitted because references to them have already
 * been simplified.  Strictly speaking, a lot of these checks are
 * redundant with other checks.  If a function is indeed a particular
 * intrinsic, then the type of its argument have already been checked
 * and passed. */

static try restricted_intrinsic(g95_expr *e) {
try t;

    switch(g95_find_id(e->value.function.isym->name)) {
    case G95_ISYM_NULL:
	t = restricted_null(e);
	break;

    case G95_ISYM_SHAPE:   case G95_ISYM_SIZE:
    case G95_ISYM_LBOUND:  case G95_ISYM_UBOUND:
	t = restricted_array_inquiry(e);
	break;

    case G95_ISYM_LEN:
	elemental_flag = 1;
	t = restricted_len(e);
	elemental_flag = 0;
	break;

    case G95_ISYM_BIT_SIZE:     case G95_ISYM_KIND:   case G95_ISYM_DIGITS:
    case G95_ISYM_EPSILON:      case G95_ISYM_HUGE:   case G95_ISYM_PRECISION:
    case G95_ISYM_RADIX:        case G95_ISYM_RANGE:  case G95_ISYM_TINY:
    case G95_ISYM_MAXEXPONENT:  case G95_ISYM_MINEXPONENT:
	elemental_flag = 1;
	t = restricted_args(e->value.function.actual, 0);
	elemental_flag = 0;
	break;

    default:
	t = restricted_args(e->value.function.actual, 0);
	break;
    }

    return t;
}



/* common_variable()-- Return nonzero if the given symbol is in a
 * common block or is equivalenced to a symbol in a common block. */

static int common_variable(g95_symbol *sym) {
int seen_target, seen_common;
g95_equiv *e, *f;

    if (sym->attr.in_common)
	return 1;

    for(e=sym->ns->equiv; e; e=e->next) {
	seen_common = 0;
	seen_target = 0;

	for(f=e; f; f=f->eq) {
	    seen_common |= f->expr->symbol->attr.in_common;
	    seen_target |= (f->expr->symbol == sym);
	}

	if (seen_common && seen_target)
	    return 1;
    }

    return 0;
}



/* find_procedure()-- Given a dummy variable, find the procedure that
 * contains it.  Returns NULL if not found. */

static g95_symbol *find_procedure(g95_symtree *st, g95_symbol *target) {
g95_formal_arglist *f;
g95_symbol *sym;

    if (st == NULL)
	return NULL;

    sym = st->n.sym;

    if (sym->attr.proc != PROC_ST_FUNCTION)
	for(f=sym->formal; f; f=f->next)
	    if (f->sym == target)
		return sym;

    sym = find_procedure(st->left, target);
    if (sym != NULL)
	return sym;

    return find_procedure(st->right, target);
}



/* check_restricted()-- Verify that an expression is a restricted
 * expression.  Like its cousin check_init_expr(), an error message is
 * generated if we return FAILURE. */

static try check_restricted(g95_expr *e) {
g95_symbol *sym, *p;
g95_ref *ref;
try t;
int i;

    if (e == NULL)
	return SUCCESS;

    t = SUCCESS;

    switch(e->type) {
    case EXPR_VARIABLE:
	sym = e->symbol;
	t = FAILURE;

	if (sym->attr.st_construct) {
	    t = SUCCESS;
	    break;
	}

	if (sym->attr.flavor != FL_PARAMETER &&
	    g95_current_ns->state == COMP_PROGRAM) {
	    g95_error("Variable '%s' at %L cannot appear in a specification "
		      "expression in a PROGRAM unit", sym->name, &e->where);
	    break;
	}

	if (sym->attr.optional) {
	    g95_error("Dummy argument '%s' in specification expression at %L "
		      "cannot be OPTIONAL", sym->name, &e->where);
	    break;
	}

	if (sym->attr.intent == INTENT_OUT) {
	    g95_error("Dummy argument '%s' in specification expression at %L "
		      "cannot be INTENT(OUT)", sym->name, &e->where);
	    break;
	}

	if (sym->attr.dummy && sym->ns->proc_name != NULL &&
	    sym->ns->proc_name->attr.elemental &&
	    !elemental_flag) {
	    g95_error("Dummy variable '%s' at %L of an ELEMENTAL procedure "
		      "cannot appear in a specification expression",
		      sym->name, &e->where);
	    break;
	}

	if (sym->attr.dummy) {
	    p = find_procedure(sym->ns->sym_root, sym);

	    if (procedure == NULL)
		procedure = p;

	    else if (procedure != p && (procedure->attr.entry || p->attr.entry)) {
		g95_error("Specification expression at %L is defined by dummy "
			  "variables of different entry points", &e->where);
		break;
	    }
	}

	if (!common_variable(sym) && !sym->attr.use_assoc &&
	    !sym->attr.dummy && sym->ns == g95_current_ns &&
	    sym->attr.flavor != FL_PARAMETER) {
	    g95_error("Variable '%s' cannot appear in restricted expression "
		      "at %L", sym->name, &e->where);
	    break;
	}

	t = SUCCESS;

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (check_restricted(ref->u.ar.start[i])  == FAILURE ||
			check_restricted(ref->u.ar.end[i])    == FAILURE ||
			check_restricted(ref->u.ar.stride[i]) == FAILURE) {
			t = FAILURE;
			break;
		    }

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    if (check_restricted(ref->u.car.element[i]) == FAILURE) {
			t = FAILURE;
			break;
		    }

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		if (check_restricted(ref->u.ss.start) == FAILURE ||
		    check_restricted(ref->u.ss.end)   == FAILURE)
		    t = FAILURE;

		break;
	    }

	break;

    case EXPR_OP:
	t = check_intrinsic_op(e, check_restricted);
	if (t == SUCCESS)
	    t = g95_simplify_expr(e);

	break;

    case EXPR_FUNCTION:
	t = (e->value.function.isym != NULL)
	    ? restricted_intrinsic(e)
	    : external_spec_function(e);

	break;

    case EXPR_SUBSTRING:
	ref = e->ref;

	t = g95_specification_expr(ref->u.ss.start);
	if (t == FAILURE)
	    break;

	t = g95_specification_expr(ref->u.ss.end);
	if (t == SUCCESS)
	    t = g95_simplify_expr(e);

	break;

    case EXPR_NULL:
    case EXPR_CONSTANT:
	t = SUCCESS;
	break;

    case EXPR_ARRAY:
	t = g95_check_constructor(e, check_restricted);
	break;

    case EXPR_STRUCTURE:
	t = g95_check_constructor(e, check_restricted);
	break;

    default:
	g95_internal_error("check_restricted(): Unknown expression type");
    }

    return t;
}



/* g95_check_spec_expr()-- Check to see that an expression is a
 * specification expression.  If we return FAILURE, an error has been
 * generated. */

try g95_specification_expr(g95_expr *e) {
g95_typespec ts;
int k;

    if (e == NULL)
	return SUCCESS;

    if (g95_simplify_expr(e) == FAILURE)
	return FAILURE;

    if (e->ts.type != BT_INTEGER) {
	g95_error("Expression at %L must be of INTEGER type", &e->where);
	return FAILURE;
    }

    if (e->rank != 0) {
	g95_error("Expression at %L must be scalar", &e->where);
	return FAILURE;
    }

    procedure = NULL;

    if (check_restricted(e) == FAILURE)
	return FAILURE;

    k = g95_default_integer_kind(0);
    if (e->ts.kind != k) {
	g95_clear_ts(&ts);
	ts.type = BT_INTEGER;
	ts.kind = k;

	if (g95_convert_type(e, &ts, 0) == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



/* g95_binary_result_type()-- Given a pair of typespecs, figure out
 * the return type according to the rules for intrinsic binary operators. */

void g95_binary_result_type(g95_typespec *a, g95_typespec *b,
			    g95_typespec *result) {

    if (a->type == b->type) {
	result->type = a->type;
	result->kind = (a->kind > b->kind) ? a->kind : b->kind;

    } else if (a->type == BT_COMPLEX)
	*result = *a;

    else if (b->type == BT_COMPLEX)
	*result = *b;

    else if (a->type == BT_REAL)
	*result = *a;

    else if (b->type == BT_REAL)
	*result = *b;

    else
	g95_internal_error("g95_binary_result_type: Bad result kind");
}



/* g95_compare_expr()-- Compare two integer expressions. */

comparison g95_compare_expr(g95_expr *a, g95_expr *b) {
int i;

    if (a == NULL || a->type != EXPR_CONSTANT ||
	b == NULL || b->type != EXPR_CONSTANT)
	return CMP_UNKNOWN;

    if (a->ts.type != BT_INTEGER || b->ts.type != BT_INTEGER)
	g95_internal_error("g95_compare_expr(): Bad expression");

    i = bi_compare(a->value.integer, b->value.integer);

    if (i < 0) return CMP_LT;
    if (i > 0) return CMP_GT;
    return CMP_EQ;
}



/* g95_compare_expr_int()-- Compare an integer expression with an integer. */

comparison g95_compare_expr_int(g95_expr *a, int b) {
int i;

    if (a == NULL || a->type != EXPR_CONSTANT)
	return CMP_UNKNOWN;
 
    if (a->ts.type != BT_INTEGER)
	g95_internal_error("g95_compare_expr_int(): Bad expression");

    i = bi_compare(a->value.integer, int_to_bi(b));

    if (i < 0) return CMP_LT;
    if (i > 0) return CMP_GT;

    return CMP_EQ;
}



/* g95_c_ptr()-- Return nonzero if a typespec is a C_PTR or C_FUNPTR. */

int g95_c_ptr(g95_typespec *ts) {
internal_type t;

    if (ts->type != BT_DERIVED)
	return 0;

    t = ts->derived->attr.itype;

    return t == ITYPE_C_PTR || t == ITYPE_C_FUNPTR;
}



/* g95_expr_corank()-- Compute the corank of an expression. */

int g95_expr_corank(g95_expr *e) {
g95_coarray_spec *cas;
g95_ref *ref;

    if (e->type != EXPR_VARIABLE)
	return 0;

    cas = e->symbol->cas;

    for(ref=e->ref; ref; ref=ref->next)
	if (ref->type == REF_COMPONENT)
	    cas = ref->u.c.component->cas;

    return (cas == NULL) ? 0 : cas->corank;
}



/* g95_expr_done_1()-- Clean up */

void g95_expr_done_1(void) {
g95_shapes *s, *t;
int i;

    for(s=current_shapes; s; s=t) {
	t = s->next;

	for(i=0; i<G95_SHAPES; i++)
	    if (s->buffer[i] != NULL) {
		big_depermanent(s->buffer[i]);
		big_free(s->buffer[i]);
	    }

	g95_free(s);
    }
}
