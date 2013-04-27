
/* Array Scalarization
   Copyright (C) 2000 - 2008 Free Software Foundation, Inc.
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


#include "g95.h"
#include <string.h>
#include <assert.h>

/* Scalarization is the process of converting a single vector
 * expression to multiple scalar expressions.  The process works by
 * traversing all code nodes and examining expressions within these
 * nodes.  A single code node can be expanded to multiple code nodes
 * depending on the expressions it contains.  New nodes are inserted
 * before and after the original node.
 *
 * Vector subexpressions within scalar expressions can only happen as
 * actual arguments.  The back end handles cases of passing whole
 * arrays or sections.  We transform a general array expression into
 * an assignment to a temporary then pass the full array. */


static void scalarize_pass1(g95_expr *, int);
static void traverse_code(g95_code **);
static void scalarize_structure(g95_code *);
static void scalarize_structure0(g95_expr *, g95_constructor *);
static void scalarize_code(g95_code **);
static void traverse_va_rhs(g95_expr *);
static void section_from_section(g95_alloc *, g95_array_ref *,
				 g95_array_spec *, g95_expr *);


typedef struct loop_info {
    g95_symbol *var[G95_MAX_DIMENSIONS];
    g95_expr *start[G95_MAX_DIMENSIONS], *end[G95_MAX_DIMENSIONS],
	     *step[G95_MAX_DIMENSIONS];

    struct loop_info *next;
} loop_info;

static g95_intrinsic_sym forall_copy_array;
static int forall_sc_flag=0;


#define DISABLE_SCALARIZE_ASSIGNMENT 0


#if DISABLE_SCALARIZE_ASSIGNMENT && IN_GCC
#error Scalarizer is disabled!
#endif


/* When nodes are inserted prior to the current node, the current node
 * has to be moved.  info.current_code points to the node being
 * scalarized and can change during the process. */

typedef struct {
    g95_code *current_code, **prev_link;
    loop_info *loop_head;
    int rank, ac_assign;
} assignment_info;

static assignment_info info;

#define section_ref(ref) (ref->type == REF_ARRAY && \
	  (ref->u.ar.type == AR_FULL || ref->u.ar.type == AR_SECTION))


static void va_variable_vector(g95_expr *, g95_array_ref *, loop_info *, int);



/* free_info_chain()-- Free the linked list of loop_info structures.
 * The expression nodes are grafted elsewhere and are not freed. */

static void free_info_chain(void) {
loop_info *p, *q;

    for(p=info.loop_head; p; p=q) { 
	q = p->next;
	g95_free(p);
    }

    info.loop_head = NULL;
}



/* g95_elemental_function()-- Returns nonzero if a function reference is
 * to an elemental function. */

int g95_elemental_function(g95_expr *e) {
g95_intrinsic_sym *isym;
g95_symbol *sym;

    if (e->value.function.pointer != NULL) {
	sym = e->value.function.pointer->ts.interface;
	return (sym == NULL) ? 0 : sym->attr.elemental;
    }

    isym = e->value.function.isym;

    return (isym != NULL)
	? isym->elemental
	: e->symbol->attr.elemental;
}



/* pure_function()-- Return nonzero if a function reference is to a
 * pure function. */

static int pure_function(g95_expr *e) {
g95_intrinsic_sym *isym;
g95_symbol *sym;

    if (e->value.function.pointer != NULL) {
	sym = e->value.function.pointer->ts.interface;
	return (sym == NULL) ? 0 : sym->attr.pure;
    }

    isym = e->value.function.isym;

    return (isym != NULL)
	? 1
	: e->symbol->attr.pure;
}



/* elemental_expr()-- Determine if an expression is elemental or not.
 * The only way an expression can be nonelemental is to have a
 * nonelemental function returning an array. */

static int elemental_expr(g95_expr *e) {
g95_actual_arglist *arg;
int rc;

    if (e == NULL)
	return 1;

    switch(e->type) {
    case EXPR_OP:
	rc = elemental_expr(e->value.op.op1) ||
             elemental_expr(e->value.op.op2);
	break;

    case EXPR_FUNCTION:
	rc = g95_elemental_function(e);

	for(arg=e->value.function.actual; arg; arg=arg->next)
	    rc = rc && elemental_expr(arg->u.expr);

	break;

    default:
	rc = 1;
	break;
    }

    return rc;
}



/* advance_prev_link()-- Advance prev_link until it points to the
 * given node */

static void advance_prev_link(void) {

    while(*info.prev_link != info.current_code)
	info.prev_link = &((*info.prev_link)->next);
}



/* insert_code_node()-- Given a code node, insert it into the current
 * list.  If pre is nonzero, the node is inserted before the current
 * node, otherwise it is inserted after the current node.  Succeeding
 * insertions show up earlier in the execution list, so expansions
 * must be done "outer first" instead of "inner first".  A pointer to
 * a pointer to the just inserted node is returned. */

static g95_code **insert_code_node(g95_code *new, int pre) {
g95_code **link, *tail;

    tail = new;
    while(tail->next != NULL)
	tail = tail->next;

    if (pre) {
	new->here = info.current_code->here;
	info.current_code->here = NULL;

	tail->next = *info.prev_link;
	*info.prev_link = new;

	link = info.prev_link;

    } else {
	tail->next = info.current_code->next;
	info.current_code->next = new;
	link = &info.current_code->next;
    }

    return link;
}



/* insert_assignment()-- Given an symbol and an expression, generate
 * an assignment statement that assigns the expression to the symbol
 * and inserts it in front of the current node. */

static g95_code **insert_assignment(g95_symbol *sym, g95_expr *e, int pointer){
g95_code *c;

    c = g95_get_code(pointer ? EXEC_POINTER_ASSIGN : EXEC_ASSIGN, NULL);
    c->expr = g95_get_variable_expr(sym, &e->where);
    c->expr2 = e;

    return insert_code_node(c, 1);
}



/* variable_rank()-- Get the rank of a full variable expression,
 * suitable for making a temporary.  This is different than the rank
 * of a section. */

static int variable_rank(g95_expr *e) {
g95_array_spec *as;
g95_ref *ref;
int rank;

    if (e->type != EXPR_VARIABLE)
	return e->rank;

    as = e->symbol->as;
    rank = (as == NULL) ? 0 : as->rank;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (section_ref(ref))
		goto done;

	    rank = 0;
	    break;

	case REF_COARRAY:
	    break;

	case REF_COMPONENT:
	    as = ref->u.c.component->as;
	    rank = (as == NULL) ? 0 : as->rank;
	    break;

	case REF_SUBSTRING:
	    break;
	}

done:
    return rank;
}



/* variable_corank()-- Get the corank of a full variable expression,
 * also suitable for making a temporary. */

static int variable_corank(g95_expr *e) {
g95_coarray_spec *cas;
g95_ref *ref;

    if (e->type != EXPR_VARIABLE)
	return 0;

    cas = e->symbol->cas;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type != AR_FULL)
		cas = NULL;

	    break;

	case REF_COMPONENT:
	    cas = ref->u.c.component->cas;
	    break;

	default:
	    cas = NULL;
	}

    return (cas == NULL)
	? 0
	: cas->corank;
}



/* stable_expression()-- Given an expression, create a "stable"
 * version appropriate for using inside a loop.  For a constant or
 * simple variable, this is the original expression.  Otherwise we
 * create a temporary variable and do the assignment outside the loop.
 * Analogous to GCC's save_expr(). */

static g95_expr *stable_expression(g95_expr *e) {
g95_symbol *var;

    if (e == NULL)
	return NULL;

    switch(e->type) {
    case EXPR_CONSTANT:
	break;

    case EXPR_VARIABLE:
	if (e->ref == NULL ||
	    (e->ref->type == REF_ARRAY && e->ref->u.ar.type == AR_FULL))
	    break;

	/* Fall through */

    default:
	var = g95_get_temporary(&e->ts, e->rank, variable_corank(e));
	insert_assignment(var, e, 0);
	advance_prev_link();

	e = g95_get_variable_expr(var, &e->where);
	break;
    }

    return e;
}



/* stable_refs()-- Run stable_expression() on all of the ref's of an
 * expression. */

static void stable_refs(g95_expr *e) {
g95_ref *ref;
int i;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    for(i=0; i<ref->u.ar.dimen; i++) {
		ref->u.ar.start[i]  = stable_expression(ref->u.ar.start[i]);
		ref->u.ar.end[i]    = stable_expression(ref->u.ar.end[i]);
		ref->u.ar.stride[i] = stable_expression(ref->u.ar.stride[i]);
	    }

	    break;

	case REF_COARRAY:
	    for(i=0; i<ref->u.car.dimen; i++)
		ref->u.car.element[i] =
		    stable_expression(ref->u.car.element[i]);

	    break;

	case REF_SUBSTRING:
	    ref->u.ss.start = stable_expression(ref->u.ss.start);
	    ref->u.ss.end   = stable_expression(ref->u.ss.end);
	    break;

	case REF_COMPONENT:
	    break;
	}
}



/* full_array()-- Given an expression, return the full array part of
 * the reference.  The expression node is a variable node with a set
 * of reference structures.  We keep everything up to the array
 * section reference.  The type of the expression can change when
 * trailing component references are removed. */

static g95_expr *full_array(g95_expr *e, int flag) {
g95_ref *ref, *r, **link;
g95_typespec *ts;
int i;

    link = &e->ref;

    for(;;) {
	ref = *link;
	if (flag || ref == NULL)
	    break;

	if (ref->type != REF_COARRAY)
	    link = &ref->next;

	else {
	    *link = ref->next;
	    ref->next = NULL;
	    g95_free_ref_list(ref);
	}
    }

    e = g95_copy_expr(e);
    ts = &e->ts;

    if (section_ref(e->ref)) {
	ref = e->ref;
	e->ref = r = g95_full_ref(&e->where);

    } else {
	r = e->ref;

	while(!section_ref(r)) {
	    if (r->type == REF_COMPONENT)
		ts = &r->u.c.component->ts;

	    r = r->next;
	}

	ref = r->next;
	r->next = NULL;
    }

    g95_free_ref_list(ref);
    e->ts = *ts;

    if (r->u.ar.type != AR_FULL) {
	for(i=0; i<G95_MAX_DIMENSIONS; i++) {
	    r->u.ar.dimen_type[i] = DIMEN_UNKNOWN;

	    g95_free_expr(r->u.ar.start[i]);
	    g95_free_expr(r->u.ar.end[i]);
	    g95_free_expr(r->u.ar.stride[i]);

	    r->u.ar.start[i]  = NULL;
	    r->u.ar.end[i]    = NULL;
	    r->u.ar.stride[i] = NULL;
	}

	r->u.ar.type = AR_FULL;
	r->u.ar.dimen = 0;
    }

    return e;
}



/* bound_expr()-- Given an array, a dimension and a upper/lower flag,
 * generate an expression that is the upper or lower bound intrinsic
 * for that dimension. */

static g95_expr *bound_expr(g95_expr *array, int dimension, int upper_flag) {
g95_actual_arglist *a;
g95_expr *e;

    e = g95_get_expr();
    e->type = EXPR_FUNCTION;
    e->value.function.isym =
	g95_find_function(upper_flag ? "ubound" : "lbound");

    e->value.function.iname = e->value.function.isym->name;
    e->where = array->where;
    e->rank = 0;

    e->ts.type = BT_INTEGER;
    e->ts.kind = g95_pointer_integer_kind();

    e->value.function.actual = a = g95_get_actual_arglist();

    a->type   = ARG_ARRAY;
    a->u.expr = full_array(array, 1);
    /* a->pointer = ??? */

    a->next = g95_get_actual_arglist();
    a = a->next;

    a->type          = ARG_EXPR;
    a->u.expr        = g95_int_expr(dimension+1);
    a->u.expr->where = array->where;

    a->next   = g95_get_actual_arglist();
    a = a->next;

    a->type   = ARG_EXPR;
    a->u.expr = NULL;

    g95_simplify_expr(e);
    return e;
}



/* get_extent()-- Get the extent of a temporary array.  A NULL stride
 * is assumed to be one.  extent = (end - start + stride) / stride. */

static g95_expr *get_extent(g95_expr *start, g95_expr *end, g95_expr *stride) {
g95_expr *e, *f;

    e = g95_get_expr();
    e->type              = EXPR_OP;
    e->value.op.operator = INTRINSIC_MINUS;
    e->value.op.op1      = end;
    e->value.op.op2      = start;

    e->ts.type = BT_INTEGER;
    e->ts.kind = g95_pointer_integer_kind();

    f = g95_get_expr();
    f->type              = EXPR_OP;
    f->value.op.operator = INTRINSIC_PLUS;
    f->ts.type           = BT_INTEGER;
    f->ts.kind           = g95_pointer_integer_kind();
    f->value.op.op1      = e;

    if (stride == NULL) {
	f->value.op.op2 = g95_int_expr(1);
	e = f;

    } else {
	f->value.op.op2 = stride;

	e = g95_get_expr();
	e->type              = EXPR_OP;
	e->value.op.operator = INTRINSIC_DIVIDE;
	e->value.op.op1      = f;
	e->value.op.op2      = g95_copy_expr(stride);

	e->ts.type = BT_INTEGER;
	e->ts.kind = g95_pointer_integer_kind();
    }

    g95_simplify_expr(e);
    return e;
}



/* section_from_bounds()-- Build an allocate specification by a series
 * of LBOUND()/UBOUND() expressions. */

static void section_from_bounds(g95_alloc *alloc, g95_expr *e,
				int copy_bounds) {
int i, rank;

    rank = alloc->rank = e->rank;

    for(i=0; i<rank; i++) {
	if (copy_bounds) {
	    alloc->lower[i] = bound_expr(e, i, 0);
	    alloc->upper[i] = bound_expr(e, i, 1);

	} else {
	    alloc->lower[i] = g95_int_expr(1);
	    alloc->upper[i] = get_extent(bound_expr(e, i, 0),
					 bound_expr(e, i, 1), NULL);
	}
    }
}



/* section_from_spec()-- Build an allocate specification (an array
 * reference) from an array specification. */

static void section_from_spec(g95_alloc *alloc, g95_array_spec *as) {
int i, rank; 

    rank = alloc->rank = as->rank;

    for(i=0; i<rank; i++) {
	alloc->lower[i] = g95_int_expr(1);
	alloc->upper[i] = get_extent(g95_copy_expr(as->lower[i]),
				     g95_copy_expr(as->upper[i]), NULL);
    }
}



/* section_range()-- Get a range from a section. */

static void section_range(g95_alloc *alloc, g95_array_ref *src,
			  g95_array_spec *as, g95_expr *array,
			  int i, int rank) {
g95_expr *start, *end, *step;

    alloc->lower[rank] = g95_int_expr(1);

    if (src->start[i] == NULL) {
	start = (as->lower[i] != NULL)
	    ? g95_copy_expr(as->lower[i])
	    : bound_expr(array, i, 0);

    } else {
	src->start[i] = stable_expression(src->start[i]);
	start = g95_copy_expr(src->start[i]);
    }

    if (src->end[i] == NULL) {
	end = (as->upper[i] != NULL)
	    ? g95_copy_expr(as->upper[i])
	    : bound_expr(array, i, 1);

    } else {
	src->end[i] = stable_expression(src->end[i]);
	end = g95_copy_expr(src->end[i]);
    }

    if (src->stride[i] == NULL)
	step = NULL;

    else {
	src->stride[i] = stable_expression(src->stride[i]);
	step = g95_copy_expr(src->stride[i]);
    }

    alloc->upper[rank] = get_extent(start, end, step);
}



/* section_vector_variable()-- Traverse a vector subscript expression
 * looking for a clue to the size.  Returns nonzero if we are done
 * with the search. */

static int section_vector(g95_expr *e, g95_alloc *alloc, int i) {
g95_actual_arglist *a;
g95_array_spec *as;
g95_alloc alloc0;
g95_ref *ref;

    if (e == NULL)
	return 0;

    switch(e->type) {
    case EXPR_OP:
	if (section_vector(e->value.op.op1, alloc, i) ||
	    section_vector(e->value.op.op2, alloc, i))
	    return 1;

	break;

    case EXPR_FUNCTION:
	for(a=e->value.function.actual; a; a=a->next)
	    if (section_vector(a->u.expr, alloc, i))
		return 1;

	break;

    case EXPR_VARIABLE:
	if (e->rank == 0)
	    break;

	as = e->symbol->as;

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		switch(ref->u.ar.type) {
		case AR_FULL:
		    section_range(alloc, &ref->u.ar, as, e, 0, i);
		    return 1;

		case AR_SECTION:
		    memset(&alloc0, '\0', sizeof(alloc0));

		    section_from_section(&alloc0, &ref->u.ar, as, e);

		    alloc->lower[i] = alloc0.lower[0];
		    alloc->upper[i] = alloc0.upper[0];
		    return 1;

		default:
		    break;
		}

		break;

	    case REF_COMPONENT:
		as = ref->u.c.component->as;
		break;

	    case REF_COARRAY:
	    case REF_SUBSTRING:
		break;
	    }

	g95_internal_error("section_vector(): Array not found");

    default:
	break;
    }

    return 0;
}



/* section_from_section()-- Build an allocate specification (an array
 * reference) from an array section.  The section always starts at one
 * up to the extent of the section.  The section variables are
 * replaced with stable versions. */

static void section_from_section(g95_alloc *alloc, g95_array_ref *src,
				 g95_array_spec *as, g95_expr *array) {
int i, rank;

    rank = 0;

    for(i=0; i<src->dimen; i++) {
	switch(src->dimen_type[i]) {
	case DIMEN_ELEMENT:
	    break;

	case DIMEN_RANGE:
	    section_range(alloc, src, as, array, i, rank++);
	    break;

	case DIMEN_VECTOR:
	    section_vector(src->start[i], alloc, rank++);
	    break;

	default:
	    g95_internal_error("section_from_section(): Bad dimen type");
	}
    }

    alloc->rank = rank;
}



/* extract_section()-- Extract the bounds of an array section given a
 * right-hand side expression.  A vector expression has already been
 * located by find_section_spec(). */

static int extract_section(g95_expr *e, g95_alloc *alloc, int copy_bounds) {
g95_array_spec *as;
g95_ref *ref;

    as = e->symbol->as;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_COMPONENT:
	    as = ref->u.c.component->as;
	    break;

	case REF_ARRAY:
	    switch(ref->u.ar.type) {
	    case AR_FULL:
		section_from_bounds(alloc, e, copy_bounds);
		return 1;

	    case AR_SECTION:
		section_from_section(alloc, &ref->u.ar, as, e);
		return 1;

	    default:
		break;
	    }

	case REF_COARRAY:
	case REF_SUBSTRING:
	    break;
	}

    return 0;
}



/* find_section_spec()-- Locate the vector part of a right-hand side
 * expression in order to determine the bounds of a temporary from the
 * full extent or a section.  Returns nonzero if the vector part has
 * been located. */

static int find_section_spec(g95_expr *e, g95_alloc *alloc, int copy_bounds) {
g95_actual_arglist *arg;
int rc;

    if (e == NULL)
	return 0;

    switch(e->type) { 
    case EXPR_OP:
	rc = find_section_spec(e->value.op.op1, alloc, copy_bounds) ||
	     find_section_spec(e->value.op.op2, alloc, copy_bounds);
	break;

    case EXPR_VARIABLE:
	rc = extract_section(e, alloc, copy_bounds);
	break;

    case EXPR_FUNCTION:
	if (!g95_elemental_function(e)) {
	    section_from_spec(alloc, e->symbol->as);
	    rc = 1;
	} else {

	    rc = 0;
	    for(arg=e->value.function.actual; arg; arg=arg->next)
		if (find_section_spec(arg->u.expr, alloc, copy_bounds)) {
		    rc = 1;
		    break;
		}

	    break;
	}

    case EXPR_ARRAY:
	g95_internal_error("find_section_spec(): Unexpected constructor");
	break;

    default:
	rc = 0;
    }

    return rc;
}



/* generate_deallocate()-- Given a symbol, generate a deallocate
 * statement and append it to the current node. */

static void generate_deallocate(g95_symbol *var, int rank, g95_locus *where) {
g95_code *c;
g95_expr *e;

    c = g95_get_code(EXEC_DEALLOCATE, NULL);

    c->ext.alloc_list       = g95_get_alloc();
    c->ext.alloc_list->expr = e = g95_get_expr();

    e->type   = EXPR_VARIABLE;
    e->ts     = var->ts;
    e->symbol = var;
    e->ref    = g95_full_ref(where);
    e->rank   = rank;
    e->where  = *where;

    insert_code_node(c, 0);
}



/* insert_rhs_start()-- For a given dimension, build a chain of
 * assignments that initialize loop indexes on the right. */

static g95_code *insert_rhs_start(int dimension, g95_locus *where) {
g95_code *head, *tail, *c;
loop_info *r;

    head = tail = NULL;

    for(r=info.loop_head; r; r=r->next) {
	if (r->var[dimension] == NULL)
	    continue;

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_get_variable_expr(r->var[dimension], &c->where);
	c->expr2 = r->start[dimension];

	g95_free_expr(r->end[dimension]);  /* Unneeded */
	r->end[dimension] = NULL;

	if (head == NULL)
	    head = c;
	else
	    tail->next = c;

	tail = c;
    }

    return head;
}



/* insert_rhs_tail()-- For a given dimension, insert the RHS variable
 * increments. */

static void insert_rhs_tail(g95_code *block, int dimension) {
g95_code *c, *tail;
loop_info *r;
g95_expr *e;

    tail = block;
    while(tail->next != NULL)
	tail = tail->next;

    for(r=info.loop_head; r; r=r->next) {
	if (r->var[dimension] == NULL)
	    continue;

	e = g95_get_expr();

	e->type = EXPR_OP;
	e->value.op.operator = INTRINSIC_PLUS;
	e->value.op.op1 = g95_get_variable_expr(r->var[dimension], NULL);
	e->value.op.op2 = r->step[dimension];
	e->ts.type = BT_INTEGER;
	e->ts.kind = g95_pointer_integer_kind();

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_get_variable_expr(r->var[dimension], NULL);
	c->expr2 = e;

	tail->next = c;
	tail = c;
    }
}



/* build_loop()-- Build a single loop associated with a wide range of
 * scalarization loops. */

static g95_code *build_loop(loop_info *control, int dim, g95_code *inner) {
g95_iterator *iter;
g95_code *head, *tail, *c;

    iter = g95_get_iterator(); 

    iter->var   = g95_get_variable_expr(control->var[dim], NULL);
    iter->start = control->start[dim];
    iter->end   = control->end[dim];
    iter->step  = control->step[dim];

    head = insert_rhs_start(dim, &inner->where);

    c = g95_get_code(EXEC_DO, NULL);

    if (head == NULL)
	head = c;

    else {
	tail = head;
	while(tail->next != NULL)
	    tail = tail->next;

	tail->next = c;
    }

    insert_rhs_tail(inner, dim);

    c->ext.iterator = iter;
    c->block = inner;

    return head;
}



/* elemental_assumed_length()-- Scalarize an elemental assumed length
 * character function call.  The main difficulty is that we don't know
 * the length of the returned strings until the function actually
 * returns.  We treat the situation like an indefinite array
 * constructor assignment.  We get an initial array, loop over the
 * array components and use array constructor assignment to assign the
 * array.  Copy out isn't a factor here. */

static g95_expr *elemental_assumed_length(g95_expr *rhs) {
assignment_info save;
g95_alloc alloc;
g95_symbol *var;
g95_typespec ts;
loop_info *r;
g95_code *c;
int rank, i;

    g95_clear_ts(&ts);

    ts.type = BT_CHARACTER;
    ts.kind = g95_default_character_kind();

    ts.cl = &g95_unknown_charlen;   /* Old version, might still be needed */
    ts.cl = rhs->ts.cl;

    rank = rhs->rank;
    var  = g95_get_temporary(&ts, rank, variable_corank(rhs));

    c = g95_get_code(EXEC_AC_START, NULL);
    c->sym = var;  

    if (find_section_spec(rhs, &alloc, 0)) {
	c->ext.shape = g95_getmem(alloc.rank * sizeof(g95_expr *));

	for(i=0; i<alloc.rank; i++)
	    c->ext.shape[i] =
		get_extent(alloc.lower[i], alloc.upper[i], NULL);
    }

    insert_code_node(c, 1);
    advance_prev_link();

    save = info;
    memset(&info, '\0', sizeof(info));

    traverse_va_rhs(rhs);

    /* Use the first loop_info structure as the loop control */

    r = info.loop_head;
    info.loop_head = r->next;

    c = g95_get_code(EXEC_AC_ASSIGN, NULL);
    c->sym = var;
    c->expr = rhs;

    for(i=0; i<rank; i++)
	c = build_loop(r, i, c);

    info = save;

    g95_free(r);
    free_info_chain();

    insert_code_node(c, 1);
    generate_deallocate(var, rank, &rhs->where);

    advance_prev_link();

    return g95_get_variable_expr(var, &rhs->where);
}



/* temp_array()-- Generate a temporary array with a particular
 * extent, generate appropriate ALLOCATE and DEALLOCATE statements for
 * it, as well as an assignment statement that is subsequently
 * scalarized recursively.  Returns an expression for the full
 * temporary array. */

static g95_expr *temp_array(g95_expr *rhs, int copy_out) {
g95_code *c, **link;
g95_symbol *var;
g95_typespec ts;
g95_expr *e;

    ts = rhs->ts;

    if (ts.type == BT_CHARACTER &&
	(ts.cl == &g95_unknown_charlen || ts.cl == NULL ||
	 ts.cl->length == NULL || ts.cl->length->type != EXPR_CONSTANT))

	return elemental_assumed_length(rhs);

    /* Build the ALLOCATE node */ 

    c = g95_get_code(EXEC_ALLOCATE, NULL);

    c->ext.alloc_list       = g95_get_alloc();
    c->ext.alloc_list->expr = e = g95_get_expr();

    var = g95_get_temporary(&ts, rhs->rank, variable_corank(rhs));

    e->type   = EXPR_VARIABLE;
    e->ts     = var->ts;
    e->symbol = var;
    e->rank   = rhs->rank;
    e->where  = rhs->where;

    /* The extent of each dimension of the array is determined from
     * the extent of the reference on the right hand side.  Recurse
     * into the expression to find either a full reference that we can
     * transform into bounds, or a section specification that we can
     * do the same with. */

    if (!find_section_spec(rhs, c->ext.alloc_list, 0))
	g95_internal_error("temp_array(): "
			   "Can't find array part of expression");

    insert_code_node(c, 1);
    generate_deallocate(var, rhs->rank, &rhs->where);

    /* Copy out if necessary */

    if (copy_out) {
	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_copy_expr(rhs);
	c->expr2 = e = g95_get_variable_expr(var, &rhs->where);

	link = insert_code_node(c, 0);
	g95_scalarize_assignment(link);
    }

    /* Now build the assignment */

    c = g95_get_code(EXEC_ASSIGN, NULL);
    c->expr = e = g95_get_variable_expr(var, &rhs->where);
    c->expr2 = rhs;

    advance_prev_link();

    link = insert_code_node(c, 1);
    g95_scalarize_assignment(link);

    /* Build the full array expression */

    return g95_get_variable_expr(var, &rhs->where);
}



/* get_loop_info()-- Get an loop_info structure.  It's linked into the
 * main list. */

static loop_info *get_loop_info(void) {
loop_info *r;

    r = g95_getmem(sizeof(loop_info));
    r->next = info.loop_head;
    info.loop_head = r;

    return r;
}



/* va_variable_full()-- Transform a full array reference into a scalar
 * reference. */

static void va_variable_full(g95_expr *e, g95_ref *ref) {
g95_symbol *var;
loop_info *r;
int i, rank;

    rank = e->rank;
    r = get_loop_info();

    for(i=0; i<rank; i++) {
	var = g95_get_array_int();

	r->var[i]   = var;
	r->start[i] = bound_expr(e, i, 0);
	r->end[i]   = bound_expr(e, i, 1);
	r->step[i]  = g95_int_expr(1);

	ref->u.ar.dimen_type[i] = DIMEN_ELEMENT;
	ref->u.ar.start[i] = g95_get_variable_expr(var, &e->where);
    }

    ref->u.ar.dimen = e->rank;
}



/* va_expand_vss()-- Expand a vector subscript on the right side. */

static void va_expand_vss(g95_expr *e, loop_info *r, int j) {
g95_actual_arglist *a;
g95_ref *ref;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	va_expand_vss(e->value.op.op1, r, j);
	va_expand_vss(e->value.op.op2, r, j);
	break;

    case EXPR_FUNCTION:   /* Should be elemental */
	for(a=e->value.function.actual; a; a=a->next) {
	    va_expand_vss(a->u.expr, r, j);

	    if (a->u.expr != NULL && a->u.expr->rank == 0 &&
		a->type != ARG_EXPR)
		a->type = ARG_EXPR;
	}

	break;

    case EXPR_ARRAY:
	g95_replace_expr(e, temp_array(e, 0));

	/* Fall through */

    case EXPR_VARIABLE:
	if (e->rank == 0)
	    break;

	/* Find the section reference.  We're guaranteed that it is
	 * one dimensional */

	for(ref=e->ref;; ref=ref->next)
	    if (section_ref(ref))
		break;

	va_variable_vector(e, &ref->u.ar, r, j);
	break;

    default:
	break;
    }

    e->rank = 0;
}



/* va_variable_vector()-- Expand a vector subscript on the right side. */

static void va_variable_vector(g95_expr *e, g95_array_ref *ar, loop_info *r,
			       int j) {
g95_symbol *var;
int m;

    if (r->start[j] != NULL) {
	r = g95_getmem(sizeof(loop_info));

	/* Make sure the new structure is not first */

	r->next = info.loop_head->next;
	info.loop_head->next = r;
    }

    if (ar->type == AR_FULL) {   /* 1-D full array */
	r->var[j]   = var = g95_get_array_int();
	r->start[j] = bound_expr(e, 0, 0);
	r->end[j]   = bound_expr(e, 0, 1);
	r->step[j]  = g95_int_expr(1);

	ar->type          = AR_ELEMENT;
	ar->dimen_type[0] = DIMEN_ELEMENT;
	ar->start[0]      = g95_get_variable_expr(var, &e->where);
	ar->dimen         = 1;
	return;
    }

    /* Section reference, also guaranteed to be one dimensional, so
     * find the section reference. */

    for(m=0; m<ar->dimen; m++)
	switch(ar->dimen_type[m]) {
	case DIMEN_ELEMENT:
	    break;

	case DIMEN_RANGE:
	    r->var[j] = var = g95_get_array_int();

	    r->start[j] = (ar->start[m] == NULL)
		? bound_expr(e, m, 0)
		: ar->start[m];

	    r->end[j] = (ar->end[m] == NULL)
		? bound_expr(e, m, 1)
		: ar->end[m];

	    ar->start[m] = g95_get_variable_expr(var, &e->where);
	    ar->end[m] = NULL;

	    if (ar->stride[m] == NULL)
		r->step[j] = g95_int_expr(1);

	    else {
		r->step[j] = ar->stride[m];
		ar->stride[m] = NULL;
	    }

	    ar->type = AR_ELEMENT;
	    ar->dimen_type[m] = DIMEN_ELEMENT;
	    return;

	case DIMEN_VECTOR:
	    va_expand_vss(ar->start[m], r, j);
	    ar->type = AR_ELEMENT;
	    ar->dimen_type[m] = DIMEN_ELEMENT;
	    return;

	default:
	    break;
	}

    g95_internal_error("va_variable_vector(): Section not found");
}



/* va_variable_section()-- Transform a section reference into a purely
 * scalar reference. */

static void va_variable_section(g95_expr *e, g95_ref *ref) {
g95_expr *f, *base;
g95_symbol *var;
int i, j, rank;
loop_info *r;

    j = 0;
    rank = ref->u.ar.dimen;
    base = g95_copy_expr(e);

    r = get_loop_info();

    for(i=0; i<rank; i++) {
	switch(ref->u.ar.dimen_type[i]) {
	case DIMEN_ELEMENT:
	    break;

	case DIMEN_RANGE:
	    r->var[j] = var = g95_get_array_int();

	    if (ref->u.ar.start[i] != NULL)
		r->start[j] = ref->u.ar.start[i];

	    else {
		f = g95_copy_expr(base);
		r->start[j] = bound_expr(f, i, 0);
		g95_free_expr(f);
	    }

	    if (ref->u.ar.end[i] != NULL)
		r->end[j] = ref->u.ar.end[i];

	    else {
		f = g95_copy_expr(base);
		r->end[j] = bound_expr(f, i, 1);
		g95_free_expr(f);
	    }

	    r->step[j] = (ref->u.ar.stride[i] == NULL)
		? g95_int_expr(1)
		: ref->u.ar.stride[i];

	    ref->u.ar.stride[i] = NULL;
	    ref->u.ar.start[i] = g95_get_variable_expr(var, &ref->where);
	    ref->u.ar.end[i] = NULL;

	    ref->u.ar.dimen_type[i] = DIMEN_ELEMENT;
	    j++;
	    break;

	case DIMEN_VECTOR:
	    va_expand_vss(ref->u.ar.start[i], r, j++);
	    break;

	default:
	    g95_internal_error("va_variable_section(): Bad dimen type");
	}
    }

    g95_free_expr(base);
    ref->u.ar.type = AR_ELEMENT;
}



/* va_variable()-- Given a nonscalar variable on the right, build the
 * necessary variables for iterating over it. */

static void va_variable(g95_expr *e) {
g95_ref *ref;

    for(ref=e->ref; ref; ref=ref->next)
	if (section_ref(ref))
	    break;

    if (ref == NULL)
	g95_internal_error("va_variable(): Section not found");

    if (ref->u.ar.type == AR_FULL)
	va_variable_full(e, ref);
    else
	va_variable_section(e, ref);

    ref->u.ar.type = AR_ELEMENT;
}



/* traverse_va_rhs()-- Traverse the right side of an expression,
 * replacing section references with scalar references. */

static void traverse_va_rhs(g95_expr *e) {
g95_actual_arglist *arg;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	traverse_va_rhs(e->value.op.op1);
	traverse_va_rhs(e->value.op.op2);

	e->rank = 0;
	break;

    case EXPR_STRUCTURE:  case EXPR_NULL:
    case EXPR_CONSTANT:   case EXPR_SUBSTRING:
	break;   /* Leave these nodes as they are */

    case EXPR_VARIABLE:
	if (e->rank > 0)
	    va_variable(e);

	e->rank = 0;
	break;

    case EXPR_FUNCTION:
	if (!g95_elemental_function(e))
	    break;  /* Possible when expanding a WHERE */

	for(arg=e->value.function.actual; arg; arg=arg->next) {
	    traverse_va_rhs(arg->u.expr);

	    if (arg->u.expr != NULL && arg->u.expr->rank == 0 &&
		arg->type != ARG_EXPR)
		arg->type = ARG_EXPR;
	}

	e->rank = 0;
	break;

    default:
	g95_internal_error("traverse_va_rhs(): Bad expression");
    }
}



/* init_vars()-- Initialize any internal loop values with stable
 * versions. */

static void init_vars(void) {
loop_info *r;
int i;

    for(r=info.loop_head; r; r=r->next)
	for(i=0; i<G95_MAX_DIMENSIONS; i++) {
	    r->start[i] = stable_expression(r->start[i]);
	    r->step[i]  = stable_expression(r->step[i]);
	}
}



/* check_variable_dependency()-- Classify a variable expression. */

static int check_variable_dependency(g95_expr *target, g95_expr *e) {
g95_ref *r1, *r2;
int i, r;

    r = 0;

    if (e->symbol->attr.equivalenced)
	r |= SEEN_EQUIV;

    if (e->symbol->attr.pointer)
	r |= SEEN_POINTER;

    if (e->symbol != target->symbol) {
	if (g95_find_variable(e, target->symbol))
	    r |= SEEN_SCALAR;

	goto done;
    }

    if (e->rank == 0)
	r |= SEEN_SCALAR;

    r1 = e->ref;
    r2 = target->ref;

    for(;r1 && r2; r1=r1->next, r2=r2->next) {
	if (r1->type != r2->type)
	    break;

	switch(r1->type) {
	case REF_ARRAY:
	    switch(r1->u.ar.type) {
	    case AR_FULL:	r |= SEEN_FULL;     goto done;

	    case AR_SECTION:
		r |= SEEN_SECTION;
		/* Fall through */

	    case AR_ELEMENT:
		for(i=0; i<r1->u.ar.dimen; i++)
		    if (g95_find_variable(r1->u.ar.start[i], target->symbol) ||
			g95_find_variable(r1->u.ar.end[i],   target->symbol) ||
			g95_find_variable(r1->u.ar.stride[i],target->symbol)) {
			r |= SEEN_SCALAR;
			break;
		    }

		goto done;

	    default:
		g95_internal_error("check_variable_dependency(): Bad ref");
		break;
	    }

	    break;

	case REF_COARRAY:
	    for(i=0; i<r1->u.car.dimen; i++)
		if (g95_find_variable(r1->u.car.element[i], target->symbol)) {
		    r |= SEEN_SCALAR;
		    break;
		}

	    break;

	case REF_COMPONENT:
	    if (r1->u.c.component != r2->u.c.component)
		goto done;

	    break;

	case REF_SUBSTRING:
	    break;
	}
    }

    r |= SEEN_SCALAR;

done:
    return r;
}



/* g95_check_dependency()-- Check the right hand side of an array
 * expression to see if we need a temporary for the assignment.  What
 * we need are the character of the references.  We return an integer
 * bitfield with the SEEN_* fields set. */

int g95_check_dependency(g95_expr *target, g95_expr *e) {
g95_intrinsic_sym *isym;
g95_actual_arglist *arg;
int rc;

    if (e == NULL)
	return 0;

    rc = 0;

    switch(e->type) {
    case EXPR_SUBSTRING:
    case EXPR_OP:
	rc = g95_check_dependency(target, e->value.op.op1) |
	     g95_check_dependency(target, e->value.op.op2);
	break;

    case EXPR_FUNCTION:
	isym = e->value.function.isym;
	if ((isym != NULL) ? isym->elemental : e->symbol->attr.elemental)
	    for(arg=e->value.function.actual; arg; arg=arg->next)
		rc |= g95_check_dependency(target, arg->u.expr);

	break;

    case EXPR_VARIABLE:
	rc = check_variable_dependency(target, e);
	break;

    default:
	break;
    }

    return rc;
}



/* split_ref()-- Split variable reference in two for compound
 * references.  We assign the references before and including the
 * array to a pointer, and the leave the rest where it is.  This has
 * the effect of creating an array that can be evaluated multiple
 * times. */

static void split_ref(g95_expr *e) {
g95_ref *break_ref, *ref;
g95_typespec *ts;
g95_symbol *sym;
g95_code *c;
int flag;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	split_ref(e->value.op.op1);
	split_ref(e->value.op.op2);
	return;

    default:
	return;

    case EXPR_VARIABLE:
	break;
    }

    break_ref = NULL;
    flag      = 0;
    ts        = &e->symbol->ts;

    for(ref=e->ref; ref; ref=ref->next) {
	switch(ref->type) {
	case REF_ARRAY:
	    if (section_ref(ref))
		break_ref = ref;

	    else
		flag = 1;

	    break;

	case REF_COMPONENT:
	    if (break_ref == NULL)
		ts = &ref->u.c.component->ts;

	    flag = 1;
	    break;

	case REF_COARRAY:
	    return;

	case REF_SUBSTRING:
	    flag = 1;
	    break;
	}
    }

    if (flag && break_ref != NULL && e->ref != break_ref) {
	sym = g95_get_temporary(ts, variable_rank(e), variable_corank(e));
	sym->attr.pointer     = 1;
	sym->attr.allocatable = 0;

	c = g95_get_code(EXEC_POINTER_ASSIGN, NULL);
	c->expr      = g95_get_variable_expr(sym, &e->ref->where);
	c->expr2     = g95_copy_expr(e);
	c->expr2->ts = *ts;

	g95_free_ref_list(c->expr2->ref);
	c->expr2->ref = e->ref;

	e->symbol = sym;
	e->ref = ref = g95_get_ref();
	*ref = *break_ref;

	memset(break_ref, '\0', sizeof(*break_ref));
	break_ref->type = REF_ARRAY;
	break_ref->u.ar.type = AR_FULL;

	insert_code_node(c, 1);
    }
}



/* g95_scalarize_assignment()-- Scalarize an assignment statement.  Like
 * just about every other subroutine this file, this subroutine is
 * recursive. */

void g95_scalarize_assignment(g95_code **prev_link) {
assignment_info save;
g95_expr *base, *rhs;
g95_code *c, *d, *n;
int rank, i, j, m;
g95_symbol *var;
loop_info *head;
g95_ref *ref;

#if DISABLE_SCALARIZE_ASSIGNMENT
    return; 
#endif

    n = *prev_link;
    rhs = n->expr2;

    /* Easy out if the operation is something the back end knows how to do */

    switch(rhs->type) {
    case EXPR_ARRAY:   /* Assignment of a constructor */
	return;

	/* Assign a nonelemental array function to an array pointer */
    case EXPR_FUNCTION:
	if (g95_pointer_expr(n->expr) && rhs->rank > 0 &&
	    !g95_elemental_function(rhs))
	    return;

	break;

    default:
	break;
    }

    save = info;

    memset(&info, '\0', sizeof(info));

    info.prev_link = prev_link;
    info.current_code = n;
    info.ac_assign = (n->expr->rank > 1 && n->expr2->rank == 1);

    /* See if a temporary variable is necessary for the assignment, ie
     * the variable on the left also appears on the right.  The only
     * case that we avoid a temporary is if the left is a full array
     * and all occurrances on the right that are not inside functions
     * are also full arrays.  There are cases where the temporary
     * could be avoided, but they are somewhat esoteric (TODO). */

    m = 0;
    rank = n->expr->rank;

    if (n->expr2->rank > 0) {
	i = g95_check_dependency(info.current_code->expr, n->expr2);

	ref = n->expr->ref;
	while(!section_ref(ref))
	    ref = ref->next;

	for(j=0; j<ref->u.ar.dimen; j++)
	    if (ref->u.ar.dimen_type[j] == DIMEN_VECTOR &&
		g95_check_dependency(info.current_code->expr,
				     ref->u.ar.start[j]) &
		(SEEN_SECTION | SEEN_SCALAR | SEEN_FULL))
		ref->u.ar.start[j] = temp_array(ref->u.ar.start[j], 0);

	if ((i & (SEEN_SECTION | SEEN_SCALAR)) ||
	    ((i & SEEN_FULL) && ref->u.ar.type == AR_SECTION))
	    m = 1;

	if (n->expr->symbol->attr.equivalenced && (i & SEEN_EQUIV))
	    m = 1;

	if (n->expr->type == EXPR_VARIABLE && (i & SEEN_POINTER) &&
	    (n->expr->symbol->attr.pointer || n->expr->symbol->attr.target))
	    m = 1;

	if (n->expr2->type == EXPR_VARIABLE &&
	    n->expr2->symbol->attr.aliasable) {
	    m = 1;
	    n->expr2->symbol->attr.aliasable = 0;
	}
    }

    /* Coindexed expressions cannot be split */

    if (g95_coindexed_expr(rhs))
	stable_refs(rhs);

    else
	split_ref(rhs);

    if (g95_coindexed_expr(n->expr))
	stable_refs(n->expr);

    else
	split_ref(n->expr);

    advance_prev_link();

    /* Find the section specification on the left. */

    if (m)
	info.current_code->expr2 = temp_array(n->expr2, 0);

    if (info.current_code->expr2->rank == 0 &&
	info.current_code->expr2->type != EXPR_CONSTANT &&
	info.current_code->expr2->type != EXPR_NULL) { /* Scalar on rhs */

	var = g95_get_temporary(&n->expr2->ts, 0, 0);
	c = *insert_assignment(var, n->expr2, 0);
	n->expr2 = g95_get_variable_expr(var, &rhs->where); 
	advance_prev_link();

	if (c->expr2->type == EXPR_STRUCTURE)
	    scalarize_structure(c);

	else if (n->expr2->ts.type == BT_CHARACTER &&
		 n->expr2->ts.cl == &g95_unknown_charlen)
	    generate_deallocate(var, 0, &n->where);
    }

    advance_prev_link();

    traverse_va_rhs(info.current_code->expr2);
    traverse_va_rhs(info.current_code->expr);

    c = g95_get_code(EXEC_ASSIGN, NULL);
    c->expr = info.current_code->expr;
    c->expr2 = info.current_code->expr2;

    base = g95_copy_expr(c->expr);

    info.rank = c->expr->rank;

    info.current_code->expr->rank  = 0;
    info.current_code->expr2->rank = 0;

    info.current_code->type  = EXEC_NOP;
    info.current_code->expr  = NULL;
    info.current_code->expr2 = NULL;

    init_vars();

    head = info.loop_head;
    info.loop_head = head->next;

    for(i=0; i<rank; i++)
	c = build_loop(head, i, c);

    /* Now graft the top loop where the original expression is */

    d = c;
    while(d->next != NULL)
	d = d->next;

    d->next = info.current_code->next;
    c->here = info.current_code->here;

    *info.current_code = *c;

    g95_free(c);
    g95_free_expr(base);

    g95_free(head);
    free_info_chain();
    info = save;
}



/* structure_arg()-- For a structure constructor, create a temporary,
 * assign to the temporary. */

static void structure_arg(g95_expr *e) {
g95_symbol *var;
g95_code *c;

    if (e->ts.derived->attr.itype != ITYPE_NONE)
	return;

    var = g95_get_temporary(&e->ts, 0, 0);

    c = g95_get_code(EXEC_ASSIGN, NULL);
    c->expr = g95_get_variable_expr(var, &e->where);
    c->expr2 = g95_get_expr();

    *(c->expr2) = *e;

    e->type = EXPR_VARIABLE;
    e->symbol = var;

    insert_code_node(c, 1);
    scalarize_code(info.prev_link);

    advance_prev_link();
}



/* contains_ac()-- Returns nonzero if the expression contains an array
 * constructor, which makes unrolling a little difficult. */

static int contains_ac(g95_expr *e) {
g95_actual_arglist *a;
g95_constructor *c;
g95_ref *ref;
int i;

    if (e == NULL)
	return 0;

    switch(e->type) {
    case EXPR_OP:
	return contains_ac(e->value.op.op1) ||
	       contains_ac(e->value.op.op2);

    case EXPR_FUNCTION:
	if (!g95_elemental_function(e))
	    break;

	for(a=e->value.function.actual; a; a=a->next)
	    if (contains_ac(a->u.expr))
		return 1;

	break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
	break;

    case EXPR_VARIABLE:
    case EXPR_SUBSTRING:
	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (contains_ac(ref->u.ar.start[i]) ||
			contains_ac(ref->u.ar.end[i]) ||
			contains_ac(ref->u.ar.stride[i]))
			return 1;

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    if (contains_ac(ref->u.car.element[i]))
			return 1;

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		if (contains_ac(ref->u.ss.start) ||
		    contains_ac(ref->u.ss.end))
		    return 1;

		break;
	    }

	break;

    case EXPR_STRUCTURE:
	for(c=e->value.constructor.c; c; c=c->next)
	    if (contains_ac(c->expr))
		return 1;

	break;

    case EXPR_ARRAY:
	return 1;

    default:
	g95_internal_error("contains_ac(): Bad expr");
    }

    return 0;
}


/* unroll_sum()-- Transform the SUM, PRODUCT, COUNT, MINVAL and MAXVAL
 * intrinsics into loops.  The SUM is the archetype of all the rest. */

static int unroll_sum(g95_expr *e) {
g95_symbol *result, *temp;
g95_expr *base, *initial;
g95_code *b, *c, *d;
g95_typespec ts;
g95_isym_id id;
loop_info *head;
g95_expr *f;
int rank, i;

    if (e->value.function.actual->next->u.expr != NULL)
	return 0;   /* DIM present */

    id = e->value.function.isym->id;

    if ((id == G95_ISYM_MINVAL || id == G95_ISYM_MAXVAL ||
	 id == G95_ISYM_PRODUCT || id == G95_ISYM_SUM) &&
	e->value.function.actual->next->next->u.expr != NULL)
	return 0;

    base = e->value.function.actual->u.expr;

    if (base->type == EXPR_FUNCTION || contains_ac(base))
	return 0;

    scalarize_pass1(base, 1);
    advance_prev_link();

    ts = e->ts;
    result = g95_get_temporary(&ts, 0, 0); 
    rank = base->rank;

    switch(id) {
    case G95_ISYM_COUNT:
	ts.type = BT_INTEGER;
	ts.kind = g95_default_integer_kind(0);

	initial = g95_int_expr(0);

	f = g95_get_expr();
	f->type  = EXPR_OP;
	f->ts    = ts;
	f->where = e->where;
	f->value.op.operator = INTRINSIC_PLUS;

	f->value.op.op1 = g95_get_variable_expr(result, &e->where);
	f->value.op.op2 = g95_int_expr(1);

	d = g95_get_code(EXEC_ASSIGN, NULL);
	d->expr  = g95_get_variable_expr(result, &e->where);
	d->expr2 = f;

	c = g95_get_code(EXEC_IF, NULL);
	c->block = d;
	c->expr  = base;

	break;

    case G95_ISYM_MINVAL:
	initial = g95_constant_result(ts.type, ts.kind, &e->where);

	if (ts.type == BT_INTEGER) {
	    initial->value.integer = big_clone(bi_huge(e->ts.kind));
	    big_permanent(initial->value.integer);

	} else {
	    initial->value.real = big_clone(bg_huge(g95_get_ff(e->ts.kind)));
	    big_permanent(initial->value.real);
	}

	temp = g95_get_temporary(&ts, 0, 0);

	d = g95_get_code(EXEC_ASSIGN, NULL);
	d->expr = g95_get_variable_expr(result, &e->where);
	d->expr2 = g95_get_variable_expr(temp, &e->where);

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_get_variable_expr(temp, &e->where);
	c->expr2 = base;

	b = c->next = g95_get_code(EXEC_IF, NULL);
	b->block = d;
	b->expr = f = g95_get_expr();

	f->type = EXPR_OP;
	f->ts.type = BT_LOGICAL;
	f->ts.kind = g95_default_logical_kind();

	f->value.op.operator = INTRINSIC_LT;
	f->value.op.op1 = g95_get_variable_expr(temp, &e->where);
	f->value.op.op2 = g95_get_variable_expr(result, &e->where);

	break;

    case G95_ISYM_MAXVAL:
	initial = g95_constant_result(ts.type, ts.kind, &e->where);

	if (ts.type == BT_INTEGER) {
	    initial->value.integer = big_clone(bi_huge(e->ts.kind));
	    initial->value.integer = bi_negate(initial->value.integer);
	    big_permanent(initial->value.integer);

	} else {
	    initial->value.real = big_clone(bg_huge(g95_get_ff(e->ts.kind)));
	    initial->value.real = bg_negate(initial->value.real);
	    big_permanent(initial->value.real);
	}

	temp = g95_get_temporary(&ts, 0, 0);

	d = g95_get_code(EXEC_ASSIGN, NULL);
	d->expr = g95_get_variable_expr(result, &e->where);
	d->expr2 = g95_get_variable_expr(temp, &e->where);

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_get_variable_expr(temp, &e->where);
	c->expr2 = base;

	b = c->next = g95_get_code(EXEC_IF, NULL);
	b->block = d;
	b->expr = f = g95_get_expr();

	f->type = EXPR_OP;
	f->ts.type = BT_LOGICAL;
	f->ts.kind = g95_default_logical_kind();

	f->value.op.operator = INTRINSIC_GT;
	f->value.op.op1 = g95_get_variable_expr(temp, &e->where);
	f->value.op.op2 = g95_get_variable_expr(result, &e->where);

	break;

    case G95_ISYM_SUM:
	initial = g95_constant_result(ts.type, ts.kind, &e->where);

	switch(ts.type) {
	case BT_INTEGER:
	    initial->value.integer = int_to_bi(0);
	    big_permanent(initial->value.integer);
	    break;

	case BT_REAL:
	    initial->value.real = bg_from_int(0, g95_get_ff(ts.kind));
	    big_permanent(initial->value.real);
	    break;

	case BT_COMPLEX:
	    initial->value.complex.r = bg_from_int(0, g95_get_ff(ts.kind));
	    initial->value.complex.i = bg_from_int(0, g95_get_ff(ts.kind));
	    big_permanent(initial->value.complex.r);
	    big_permanent(initial->value.complex.i);
	    break;

	default:
	    g95_internal_error("unroll_sum(): Bad type in SUM");
	}

	f = g95_get_expr();
	f->type = EXPR_OP;
	f->ts = ts;
	f->value.op.operator = INTRINSIC_PLUS;

	f->value.op.op1 = g95_get_variable_expr(result, &e->where);
	f->value.op.op2 = base;

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_get_variable_expr(result, &e->where);
	c->expr2 = f;

	break;

    case G95_ISYM_PRODUCT:
	initial = g95_constant_result(ts.type, ts.kind, &e->where);

	switch(ts.type) {
	case BT_INTEGER:
	    initial->value.integer = int_to_bi(1);
	    big_permanent(initial->value.integer);
	    break;

	case BT_REAL:
	    initial->value.real = bg_from_int(1, g95_get_ff(ts.kind));
	    big_permanent(initial->value.real);
	    break;

	case BT_COMPLEX:
	    initial->value.complex.r = bg_from_int(1, g95_get_ff(ts.kind));
	    initial->value.complex.i = bg_from_int(0, g95_get_ff(ts.kind));
	    big_permanent(initial->value.complex.r);
	    big_permanent(initial->value.complex.i);
	    break;

	default:
	    g95_internal_error("unroll_sum(): Bad type in PRODUCT");
	}

	f = g95_get_expr();
	f->type = EXPR_OP;
	f->ts = ts;
	f->value.op.operator = INTRINSIC_TIMES;

	f->value.op.op1 = g95_get_variable_expr(result, &e->where);
	f->value.op.op2 = base;

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_get_variable_expr(result, &e->where);
	c->expr2 = f;

	break;

    default:
	g95_internal_error("unroll_sum(): Bad function");
	initial = NULL;
    }

    /* We have an initial value, a code node to be placed in the inner
     * loop and a place for the scalarized expression to go to.  Put
     * it all together. */

    traverse_va_rhs(base);

    head = info.loop_head;
    info.loop_head = head->next;

    for(i=0; i<rank; i++)
	c = build_loop(head, i, c);

    g95_free(head);
    free_info_chain();

    d = g95_get_code(EXEC_ASSIGN, NULL);
    d->expr = g95_get_variable_expr(result, &e->where);
    d->expr2 = initial;
    d->next = c;

    insert_code_node(d, 1);
    advance_prev_link();

    g95_free(e->value.function.actual->next);
    g95_free(e->value.function.actual);

    e->type = EXPR_VARIABLE;
    e->symbol = result;

    return 1;
}



/* unroll_any_all()-- Unroll the ANY and ALL intrinsics into loops.
 * These are a lot like SUM(), except that we do an early out if
 * possible. */

static int unroll_any_all(g95_expr *e) {
g95_code *c1, *c2, *c3;
g95_symbol *result;
g95_typespec ts;
loop_info *head;
g95_expr *base;
g95_isym_id id;
g95_expr *f;
int rank, i;

    if (e->value.function.actual->next->u.expr != NULL)
	return 0;  /* DIM present */

    id = e->value.function.isym->id;

    base = e->value.function.actual->u.expr;
    if (base->type == EXPR_FUNCTION || contains_ac(base))
	return 0;

    rank = base->rank;
    scalarize_pass1(base, 1);

    g95_clear_ts(&ts);
    ts.type = BT_LOGICAL;
    ts.kind = g95_default_logical_kind();

    result = g95_get_temporary(&ts, 0, 0);

    c1 = g95_get_code(EXEC_IF, NULL);

    if (id == G95_ISYM_ANY)
	c1->expr = base;

    else {
	c1->expr = f = g95_get_expr();

	f->type = EXPR_OP;
	f->ts = ts;
	f->where = e->where;

	f->value.op.operator = INTRINSIC_NOT;
	f->value.op.op1 = base;
    }

    c1->block = g95_get_code(EXEC_ASSIGN, NULL);
    c1->block->expr = g95_get_variable_expr(result, &e->where);
    c1->block->expr2 = g95_logical_expr(id == G95_ISYM_ANY, &e->where);

    c1->block->next = g95_get_code(EXEC_GOTO, NULL);
    c1->block->next->label = g95_get_st_label(-1);

    c2 = g95_get_code(EXEC_ASSIGN, NULL);
    c2->expr = g95_get_variable_expr(result, &e->where);
    c2->expr2 = g95_logical_expr(id == G95_ISYM_ALL, &e->where);

    c3 = g95_get_code(EXEC_NOP, NULL);
    c3->here = c1->block->next->label;

    traverse_va_rhs(base);
    head = info.loop_head;
    info.loop_head = head->next;

    for(i=0; i<rank; i++)
	c1 = build_loop(head, i, c1);

    c2->next = c3;

    insert_code_node(c2, 1);
    insert_code_node(c1, 1);
    advance_prev_link();

    g95_free(head);
    free_info_chain();
    g95_free(e->value.function.actual->next);
    g95_free(e->value.function.actual);

    e->type = EXPR_VARIABLE;
    e->symbol = result;

    return 1;
}



/* unroll_dot_product()-- Transform the DOT_PRODUCT intrinsic into
 * loops.  This is unique in that it has a pair of arguments that are
 * looped over in tandem and always unrolls. */

static int unroll_dot_product(g95_expr *e) {
g95_expr *f, *g, *v1, *v2;
g95_symbol *result;
g95_code *c1, *c2;
loop_info *head;

    v1 = e->value.function.actual->u.expr;
    v2 = e->value.function.actual->next->u.expr;

    if (v1->type == EXPR_FUNCTION || v2->type == EXPR_FUNCTION ||
	contains_ac(v1) || contains_ac(v2))
	return 0;

    scalarize_pass1(v1, 1);
    scalarize_pass1(v2, 1);

    result = g95_get_temporary(&e->ts, 0, 0);

    c1 = g95_get_code(EXEC_ASSIGN, NULL);
    c1->expr = g95_get_variable_expr(result, &e->where);

    if (e->ts.type == BT_LOGICAL)
	c1->expr2 = g95_logical_expr(0, &e->where);

    else {
	c1->expr2 = g95_int_expr(0);
	g95_convert_type(c1->expr2, &e->ts, 1);
    }

    f = g95_get_expr();
    f->type = EXPR_OP;
    f->ts = e->ts;
    f->where = e->where;

    f->value.op.operator = (e->ts.type == BT_LOGICAL)
	? INTRINSIC_AND
	: INTRINSIC_TIMES;

    if (v1->ts.type != BT_COMPLEX)
	f->value.op.op1 = v1;

    else {
	g = g95_get_expr();
	g->type = EXPR_FUNCTION;
	g->ts = e->ts;
	g->where = e->where;

	g->value.function.isym = g95_find_function("conjg");
	g->value.function.actual = g95_get_actual_arglist();

	g->value.function.actual->type = ARG_EXPR;
	g->value.function.actual->u.expr = v1;

	f->value.op.op1 = g;
    }

    f->value.op.op2 = v2;

    if (v1->ts.type != v2->ts.type || v1->ts.kind != v2->ts.kind)
	g95_type_convert_binary(f);

    g = g95_get_expr();
    g->type = EXPR_OP;
    g->ts = e->ts;
    g->where = e->where;

    g->value.op.operator = (e->ts.type == BT_LOGICAL)
	? INTRINSIC_OR
	: INTRINSIC_PLUS;

    g->value.op.op1 = g95_get_variable_expr(result, &e->where);
    g->value.op.op2 = f;

    c2 = g95_get_code(EXEC_ASSIGN, NULL);
    c2->expr = g95_get_variable_expr(result, &e->where);
    c2->expr2 = g;

    traverse_va_rhs(v1);
    traverse_va_rhs(v2);

    head = info.loop_head;
    info.loop_head = head->next;

    c2 = build_loop(head, 0, c2);   /* v1 & v2 are rank 1 */

    c1->next = c2;

    insert_code_node(c1, 1);
    advance_prev_link();

    g95_free(head);
    free_info_chain();
    g95_free(e->value.function.actual->next);
    g95_free(e->value.function.actual);

    e->type = EXPR_VARIABLE;
    e->symbol = result;

    return 1;
}



/* unroll_function()-- Unroll selected intrinsic functions, avoiding
 * temporary arrays or function call overhead.  Returns nonzero if the
 * expression has been unrolled. */

static int unroll_function(g95_expr *e) {

    if (e->value.function.isym == NULL)
	return 0;

    switch(e->value.function.isym->id) {
    case G95_ISYM_ANY:
    case G95_ISYM_ALL:
	return unroll_any_all(e);

    case G95_ISYM_COUNT:
    case G95_ISYM_MINVAL:
    case G95_ISYM_MAXVAL:
    case G95_ISYM_SUM:
    case G95_ISYM_PRODUCT:
	return unroll_sum(e);

    case G95_ISYM_DOT_PRODUCT:
	return unroll_dot_product(e);

    default:
	break;
    }

    return 0;
}



/* scalarize_actual_arg()-- Given an actual argument, see if it is an
 * array expression that can be passed directly or not.  If not,
 * generate the assignment to a temporary, pass the temporary and
 * generate any cleanup code.  The temporary assignment is scalarized
 * separately. */

static void scalarize_actual_arg(int pure, int elemental, int expand,
				 g95_actual_arglist *arg) {
int i, need_temp, copy_out;
g95_ref *ref;
g95_expr *e;

    e = arg->u.expr;
    if (e == NULL || (e->type == EXPR_ARRAY && e->value.constructor.c == NULL))
	return;

    scalarize_pass1(e, expand | (e->rank > 0));

    if (e->rank == 0) {
	if (e->type == EXPR_STRUCTURE)
	    structure_arg(e);

	else if (e->type == EXPR_OP &&
		 e->value.op.operator == INTRINSIC_PAREN &&
		 e->value.op.op1->type == EXPR_STRUCTURE)
	    structure_arg(e->value.op.op1);

	return;
    }

    copy_out = 0;
    need_temp = 0;

    switch(e->type) {
    case EXPR_VARIABLE:
	if (arg->type != ARG_ARRAY_DESC && g95_coindexed_expr(e)) {
	    need_temp = 1;
	    break;
	}

	if (elemental && elemental_expr(e))
	    break;

	ref = e->ref;
	while(!section_ref(ref))
	    ref = ref->next;

	for(i=0; i<ref->u.ar.dimen; i++)
	    if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR) {
		need_temp = 1;
		break;
	    }

	copy_out = !pure;
	break;

    case EXPR_FUNCTION:
	need_temp = g95_elemental_function(e) && !elemental;
	break;

    case EXPR_ARRAY:
	break;

    case EXPR_NULL:
	break;

    default:
	need_temp = !elemental || !elemental_expr(e);
	break;
    }

    if (arg->intent == INTENT_IN)
	copy_out = 0;

    if (need_temp)
	arg->u.expr = temp_array(e, copy_out);
}



/* scalarize_function()-- Given a function call, go through it and
 * replace vector expressions (besides full array references) with a
 * precalculated temporary. */

static void scalarize_function(g95_expr *e, int expand) {
g95_actual_arglist *actual;
g95_intrinsic_sym *isym;
int pure, elemental;
g95_typespec ts;
g95_code **link;
g95_symbol *var;
g95_expr *f;
int m;

    if (unroll_function(e))
	return;

    pure = pure_function(e);
    elemental = g95_elemental_function(e);

    for(actual=e->value.function.actual; actual; actual=actual->next)
	scalarize_actual_arg(pure, elemental, expand, actual);

    /* alt-returns not possible here */

    if (!expand || (e->rank != 0 && elemental))
	return;

    /* Expand the function */

    ts = e->ts;
    if (ts.type == BT_CHARACTER && ts.cl != &g95_unknown_charlen &&
	!g95_is_constant_expr(ts.cl->length))
	ts.cl = &g95_unknown_charlen;

    var = g95_get_temporary(&ts, e->rank, 0);

    isym = e->value.function.isym;
    if (isym != NULL && isym->id == G95_ISYM_TRANSFER)
	var->attr.aliasable = 1;

    f = g95_get_expr();
    *f = *e;

    e->value.function.actual = NULL;
    e->value.function.iname  = NULL;
    e->value.function.isym   = NULL;
    e->shape = NULL;

    e->type = EXPR_VARIABLE;
    e->symbol = var;

    if (e->rank == 0) {
	advance_prev_link();
	if ((f->type == EXPR_VARIABLE || f->type == EXPR_FUNCTION) &&
	    f->symbol != NULL)
	    var->attr.pointer = f->symbol->attr.pointer;

	link = insert_assignment(var, f, var->attr.pointer);

	if (e->ts.type == BT_CHARACTER &&
	    (e->ts.cl == &g95_unknown_charlen ||
	     e->ts.cl->length == NULL ||
	     e->ts.cl->length->type != EXPR_CONSTANT)) {

	    var->ts.cl = &g95_unknown_charlen;
	    generate_deallocate(var, 0, &e->where);
	}

    } else {
	var->attr.pointer     = 1;
	var->attr.allocatable = 0;
	var->attr.noinit      = 1;

	e->ref = g95_full_ref(&e->where);

	if (f->type == EXPR_FUNCTION) {
	    if (f->value.function.isym != NULL)
		m = f->value.function.isym->id != G95_ISYM_TRANSFER;

	    else if (f->symbol != NULL)
		m = !f->symbol->result->attr.pointer;

	    else
		m = !f->value.function.pointer->ts.interface->attr.pointer;

	    if (m)
		generate_deallocate(var, e->rank, &e->where);
	}

	advance_prev_link();
	link = insert_assignment(var, f, 1);
    }

    advance_prev_link();

    if (e->rank > 0)
	g95_scalarize_assignment(link);
}



/* scalarize_call()-- Scalarize a subroutine call. */

static void scalarize_call(g95_code *code) {
int i, pure, elemental, rank;
g95_actual_arglist *actual;
g95_intrinsic_sym *isym;
g95_st_label *here;
g95_code *c, *d;
loop_info *r;

    isym = code->ext.sub.isym;
    pure = (isym != NULL)
	? isym->pure
	: ((code->sym == NULL) ? 0 : code->sym->attr.pure);

    rank = 0;

    for(actual=code->ext.sub.actual; actual; actual=actual->next) {
	if (actual->type != ARG_ALT_RETURN)
	    scalarize_actual_arg(pure, 0, 0, actual);

	if (actual->u.expr != NULL && actual->u.expr->rank > 0)
	    rank = actual->u.expr->rank;
    }

    /* The RANDOM_NUMBER() intrinsic isn't really elemental, but we
     * act as if it were. */

    elemental = (isym != NULL)
	? (isym->elemental || isym->id == G95_ISYM_RANDOM_NUMBER)
	: ((code->sym == NULL)
	   ? 0
	   : code->sym->attr.elemental);

    if (rank == 0 || !elemental)
	return;

    here = code->here;
    code->here = NULL;

    /* Scalarize an elemental subroutine that has a vector argument */

    for(actual=code->ext.sub.actual; actual; actual=actual->next) {
	if (actual->u.expr == NULL)
	    continue;

	if (actual->u.expr->rank == 0)
	    actual->u.expr = stable_expression(actual->u.expr);

	else {
	    traverse_va_rhs(actual->u.expr);

	    if (actual->u.expr != NULL && actual->u.expr->rank == 0)
		actual->type = ARG_EXPR;
	}
    }

    /* Use the first loop_info structure as the loop control */

    r = info.loop_head;
    info.loop_head = r->next;

    c = g95_get_code(EXEC_CALL, NULL);

    c->ext.sub.actual = code->ext.sub.actual;
    c->ext.sub.isym = code->ext.sub.isym;
    c->ext.sub.sub_name = code->ext.sub.sub_name;
    c->sym = code->sym;

    for(i=0; i<rank; i++)
	c = build_loop(r, i, c);

    g95_free(r);
    free_info_chain();

    /* Graft the new chain into the old node */

    d = c;
    while(d->next != NULL)
	d = d->next;

    d->next = code->next;
    d->here = here;
    *code = *c;

    g95_free(c);
}



/* scalarize_ref()-- Scalarize a variable reference. */

static void scalarize_ref(g95_expr *e) {
g95_ref *ref;
int i;

    for(ref=e->ref; ref; ref=ref->next) {
	switch(ref->type) {
	case REF_ARRAY:
	    for(i=0; i<ref->u.ar.dimen; i++) {
		scalarize_pass1(ref->u.ar.start[i],
				ref->u.ar.dimen_type[i] == DIMEN_VECTOR);

		scalarize_pass1(ref->u.ar.end[i],    0);
		scalarize_pass1(ref->u.ar.stride[i], 0);
	    }

	    break;

	case REF_COARRAY:
	    for(i=0; i<ref->u.car.dimen; i++)
		scalarize_pass1(ref->u.car.element[i], 0);

	    break;

	case REF_COMPONENT:
	    break;

	case REF_SUBSTRING:
	    scalarize_pass1(ref->u.ss.start, 0);
	    scalarize_pass1(ref->u.ss.end,   0);
	    break;
	}
    }
}



/* transform_ac_range()-- Transform a range in a vector section into a
 * loop. */

static g95_expr *transform_ac_range(g95_expr *inner, g95_array_ref *ar,
				    int d, g95_expr *base) {
g95_constructor *c;
g95_iterator *iter;
g95_symbol *var;
g95_expr *e;

    c = g95_get_constructor();
    c->expr = inner;

    var = g95_get_array_int();
    iter = c->iterator = g95_get_iterator();
    iter->var = g95_get_variable_expr(var, &base->where);

    if (ar->start[d] == NULL)
	iter->start = bound_expr(base, d, 0);

    else {
	iter->start = ar->start[d];
	ar->start[d] = NULL;
    }

    if (ar->end[d] == NULL)
	iter->end = bound_expr(base, d, 1);

    else {
	iter->end = ar->end[d];
	ar->end[d] = NULL;
    }

    if (ar->stride[d] == NULL)
	iter->step = g95_int_expr(1);

    else {
	iter->step = ar->stride[d];
	ar->stride[d] = NULL;
    }

    ar->dimen_type[d] = DIMEN_ELEMENT;
    ar->type = AR_ELEMENT;
    ar->start[d] = g95_get_variable_expr(var, &inner->where);

    inner->rank = 0;

    e = g95_get_expr();
    e->where = inner->where;
    e->ts = inner->ts;
    e->type = EXPR_ARRAY;
    e->rank = 1;
    e->value.constructor.c = c;

    return e;
}



/* transform_ac_vector()-- Transform a vector subscript within an
 * array subscript into an appropriate loop.  We just search for the
 * range part within nested subscripts and use transform_ac_range(). */

static g95_expr *transform_ac_vector(g95_expr *inner, g95_array_ref *ar,
				     int d) {
g95_ref *ref;
g95_expr *e;
int m;

    e = ar->start[d];

    ar->dimen_type[d] = DIMEN_ELEMENT;
    ar->type          = AR_ELEMENT;

    inner->rank       = 0;

    for(ref=e->ref;; ref=ref->next)
	if (section_ref(ref))
	    break;

    ar = &ref->u.ar;

    if (ar->type == AR_FULL) {
	ar->dimen = 1;
	m = 0;
	goto found;
    }

    /* Find the section reference */

    for(m=0; m<ar->dimen; m++)
	switch(ar->dimen_type[m]) {
	case DIMEN_ELEMENT:
	    break;

	case DIMEN_RANGE:
	    goto found;

	case DIMEN_VECTOR:
	    return transform_ac_vector(inner, ar, m);

	default:
	    goto fatal;
	}

fatal:
    g95_internal_error("transform_ac_vector(): Dimension not found");

found:
    return transform_ac_range(inner, ar, m, e);
}


/* ac_temp()-- See if a temporary array is necessary for an expression
 * in an array constructor.  Returns nonzero if so. */

static int ac_temp(g95_expr *e) {
g95_ref *ref;
int i;

    if (e->rank == 0)
	return 0;

    switch(e->type) {
    case EXPR_OP:
    case EXPR_FUNCTION:
    case EXPR_CONSTANT:
	return 1;

    case EXPR_VARIABLE:
	break;

    default:
	g95_internal_error("ac_temp(): Bad type");
    }

    for(ref=e->ref; ref; ref=ref->next) {
	if (ref->type != REF_ARRAY)
	    continue;

	for(i=0; i<ref->u.ar.dimen; i++) {
	    if (ref->u.ar.type != AR_SECTION)
		continue;

	    if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
		return 1;
	}
    }

    return 0;
}



/* scalarize_ac0()-- Traverse an array constructor.  We replace
 * non-constructor array expressions with an equivalent array
 * variable. */

static void scalarize_ac0(g95_symbol *var, g95_constructor *c) {
assignment_info save;
g95_iterator *iter;
g95_code *code, *d;
g95_symbol *temp;
g95_locus where;
g95_expr *e;

    for(; c; c=c->next) {
	e = c->expr;

	iter = c->iterator;
	if (iter == NULL) {
	    scalarize_pass1(e, e->rank != 0);
	    advance_prev_link();

	    switch(e->type) {
	    case EXPR_ARRAY:
		scalarize_ac0(var, e->value.constructor.c);
		break;

	    case EXPR_STRUCTURE:
		temp = g95_get_temporary(&e->ts, 0, 0);

		code = g95_get_code(EXEC_ASSIGN, NULL);
		code->expr = g95_get_variable_expr(temp, &e->where);
		code->expr2 = e;

		insert_code_node(code, 1);
		scalarize_structure(code);
		advance_prev_link();

		e = g95_get_variable_expr(temp, &code->where);
		goto insert;

	    default:
		if (ac_temp(e)) {
		    e = temp_array(e, 0);
		    advance_prev_link();
		}

	    insert:
		code = g95_get_code(EXEC_AC_ASSIGN, NULL);
		code->sym = var;
		code->expr = e;
		c->expr = NULL;

		insert_code_node(code, 1);
	    }

	} else {    /* Iterator */
	    scalarize_pass1(iter->start, 0);
	    scalarize_pass1(iter->end,   0);
	    scalarize_pass1(iter->step,  0);

	    where = iter->var->where;

	    /* Build DO-loop */

	    code = g95_get_code(EXEC_DO, NULL);
	    code->ext.iterator = iter;

	    c->iterator = NULL;
	    code->block = d = g95_get_code(EXEC_NOP, NULL);

	    insert_code_node(code, 1);

	    save = info;
	    memset(&info, '\0', sizeof(info));

	    info.current_code = d;
	    info.prev_link = &code->block;

	    assert(c->expr->type == EXPR_ARRAY);
	    scalarize_ac0(var, c->expr->value.constructor.c);

	    info = save;
	}

	advance_prev_link();
    }
}



/* scalarize_ac()-- Scalarize an array constructor.  This just
 * generates an assignment to a temporary array.  TODO: Make sure
 * something like "x = (/ x(5:1:-1) /)" works. */

static void scalarize_ac(g95_expr *e) {
g95_symbol *var;
g95_expr *t;
g95_code *c;
int i;

    if (e->value.constructor.c == NULL)
	return;

    var = g95_get_temporary(&e->ts, e->rank, 0);
    c = g95_get_code(EXEC_AC_START, NULL);

    c->sym = var;

    if (e->shape == NULL)
	c->ext.shape = NULL;

    else {  /* Convert bignum shape array to expr shape array */
	c->ext.shape = g95_getmem(e->rank * sizeof(g95_expr *));

	for(i=0; i<e->rank; i++) {
	    t = g95_constant_result(BT_INTEGER, g95_default_integer_kind(0),
				    &e->where);

	    t->value.integer = e->shape[i];
	    c->ext.shape[i] = t;
	}

	e->shape = NULL; 	/* Shapes aren't freed */
    }

    insert_code_node(c, 1);
    advance_prev_link();

    scalarize_ac0(var, e->value.constructor.c);
    advance_prev_link();

    generate_deallocate(var, 1, &e->where);

    g95_free_constructor(e->value.constructor.c);
    e->value.constructor.c = NULL;

    e->type = EXPR_VARIABLE;
    e->symbol = var;
    e->ref = g95_full_ref(&e->where);
}



/* scalarize_pass1()-- First pass for scalarizing an expression.  For
 * expressions that are ultimately scalar, this is the only
 * manipulation.  The thing we accomplish here is to move functions
 * that return arrays into temporaries.  For an expression that is
 * ultimately scalar, the expansion flag doesn't matter.  For an array
 * expression (not just the subexpression!), scalar and vector
 * functions must usually be replaced by a temporary. */

static void scalarize_pass1(g95_expr *e, int expand) {
g95_constructor *c;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	scalarize_pass1(e->value.op.op1, expand);
	scalarize_pass1(e->value.op.op2, expand);
	break;

    case EXPR_FUNCTION:
	scalarize_function(e, expand);
	break;

    case EXPR_SUBSTRING:
    case EXPR_VARIABLE:
	scalarize_ref(e);
	break;

    case EXPR_ARRAY:
	scalarize_ac(e);
	break;

    case EXPR_STRUCTURE:
	for(c=e->value.constructor.c; c; c=c->next)
	    if (c->expr != NULL)
		scalarize_pass1(c->expr, c->expr->rank > 0);

	break;

    default:
	break;
    }
}



/* scalarize_transfer()-- Scalarize an expression that is part of an
 * I/O transfer. */

static void scalarize_transfer(g95_expr *e, int copy_out) {
g95_ref *r, *ref;
g95_code **link;
g95_symbol *var;
g95_expr *f;
int i;

    switch(e->type) {
    case EXPR_VARIABLE:
	if (g95_coindexed_expr(e))
	    goto need_temp;

	for(ref=e->ref;; ref=ref->next)
	    if (section_ref(ref))
		break;

	if (ref->u.ar.type == AR_FULL)
	    break;

	/* Make sure we don't have a vector subscript anyplace */

	for(i=0; i<ref->u.ar.dimen; i++)
	    if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
		goto need_temp;

	for(ref=ref->next; ref; ref=ref->next)
	    if (ref->type == REF_COMPONENT && ref->u.c.component->allocatable)
		goto need_temp;

	break;

    case EXPR_FUNCTION:
	advance_prev_link();

	if (e->value.function.isym != NULL &&
	    e->value.function.isym->id == G95_ISYM_TRANSFER)
	    break;

	if (g95_elemental_function(e))
	    goto need_temp;

	/* Transfer an array result directly without a temporary */

	var = g95_get_temporary(&e->ts, e->rank, 0);

	f = g95_get_expr();
	*f = *e;

	e->value.function.actual = NULL;
	e->value.function.iname  = NULL;
	e->value.function.isym   = NULL;

	e->type = EXPR_VARIABLE;
	e->symbol = var;

	var->attr.pointer = 1;
	var->attr.allocatable = 0;

	e->ref = g95_full_ref(&e->where);

	link = insert_assignment(var, f, 0);
	g95_scalarize_assignment(link);

	generate_deallocate(var, e->rank, &e->where);
	break;

    case EXPR_ARRAY:
	if (e->value.constructor.c != NULL)
	    goto need_temp;

	break;

    default:
    need_temp:
	r = NULL;
	for(ref=e->ref; ref!=NULL; r=ref, ref=ref->next)
	    if (ref->type == REF_SUBSTRING)
		break;

	if (ref != NULL) {
	    r->next = NULL;
	    e->ts.type = BT_UNKNOWN;
	    g95_resolve_expr(e);
	}

	info.current_code->expr = temp_array(e, copy_out);

	if (ref != NULL) {
	    r = info.current_code->expr->ref;
	    while(r->next != NULL)
		r=r->next;

	    r->next = ref;
	}

	break;
    }
}



/* append_node()-- Append a code node to the current list.  The
 * current pointer is updated. */

static void append_node(g95_code **current, g95_code *new) {
g95_code *c;

    c = *current;
    c->next = new;

    while(c->next != NULL)
	c = c->next;

    *current = c;
}



/* where_sub()-- Call one of the WHILE helpers */

static g95_code *where_sub(char *name, g95_locus *where, g95_expr *arg) {
g95_actual_arglist *a, *b;
g95_code *c;

    c = g95_get_code(EXEC_CALL, NULL);
    c->ext.sub.isym = &forall_copy_array;
    c->ext.sub.sub_name = name;

    if (arg != NULL) {
	c->ext.sub.actual = a = g95_get_actual_arglist();
	a->type = ARG_EXPR;
	a->u.expr = arg;

	a->next = b = g95_get_actual_arglist();
	b->type = ARG_EXPR;
	b->cb = CB_VALUE;
	b->u.expr = g95_int_expr(arg->ts.kind);
    }

    return c;
}



/* where_loops()-- Given an array expression, generate loops over all
 * of the elements given some inner code to execute. */

static g95_code *where_loops(g95_code *inner, g95_expr *e, g95_expr *f) {
loop_info *head;
g95_code *c;
int i, rank;

    rank = e->rank;
    c = inner;

    traverse_va_rhs(e);
    traverse_va_rhs(f);

    head = info.loop_head;
    info.loop_head = head->next;

    for(i=0; i<rank; i++)
	c = build_loop(head, i, c);

    g95_free(head);
    free_info_chain();

    return c;
}



/* append0()-- Append nodes together to form a single string.  The pre
 * and post nodes can be NULL. */

static g95_code *append0(g95_code *pre, g95_code *body, g95_code *post) {
g95_code *p;

    if (pre != NULL) {
	p = pre;
	while(p->next != NULL)
	    p = p->next;

	p->next = body;
    }

    if (post != NULL) {
	p = body;
	while(p->next != NULL)
	    p = p->next;

	p->next = post;
    }

    return (pre == NULL) ? body : pre;
}


/* expand_vector_where()-- Expand a vector where assignment. */

static g95_code *expand_vector_where(g95_code *c, g95_code *pre,
				     g95_code *post) {
g95_code *i, *m;
g95_expr *e;

    i = g95_get_code(EXEC_IF, &c->where);
    i->expr = e = g95_get_expr();

    e->type = EXPR_FUNCTION;
    e->ts.type = BT_LOGICAL;
    e->ts.kind = g95_default_integer_kind(0);

    e->value.function.isym = &forall_copy_array;
    e->value.function.iname = PREFIX "where_read";
    e->where = c->where;

    i->block = c;

    m = where_loops(i, c->expr, c->expr2);
    m = append0(pre, m, post);
    m = g95_forall_loops(m);

    return m;
}



/* expand_scalar_where()-- Expand a scalar WHERE assignment.  This
 * amounts to a single loop over the assignment. */

static g95_code *expand_scalar_where(g95_code *c, g95_code *pre,
				     g95_code *post) {
g95_iterator *iter;
g95_code *loop, *p;
g95_expr *e;

    loop = g95_get_code(EXEC_DO, &c->where);
    loop->block = c;

    loop->ext.iterator = iter = g95_get_iterator();

    iter->var = g95_get_variable_expr(g95_get_array_int(), 0);
    iter->start = g95_int_expr(1);
    iter->step = g95_int_expr(1);

    iter->end = e = g95_get_expr();

    e->ts.type = BT_INTEGER;
    e->ts.kind = g95_pointer_integer_kind();
    e->type = EXPR_FUNCTION;

    e->value.function.isym = &forall_copy_array;
    e->value.function.iname = PREFIX "where_bits";
    e->where = c->where;

    p = append0(pre, loop, post);
    return g95_forall_loops(p);
}



/* expand_where_assignment()-- Given an assignment node, perform the
 * masked assignment. */

static g95_code *expand_where_assignment(g95_code *c) {
g95_code *head, *m, *pre, node;
assignment_info save;

    memset(&node, '\0', sizeof(node));
    node.type = EXEC_NOP;
    node.where = c->where;

    save = info;
    memset(&info, '\0', sizeof(info));

    info.current_code = head = &node;
    info.prev_link = &head;

    scalarize_pass1(c->expr, 1);
    scalarize_pass1(c->expr2, 1);

    if (head == &node)
	pre = NULL;

    else {
	pre = head;
	m = pre;
	while(m->next != &node)
	    m = m->next;

	m->next = NULL;
    }

    if (c->expr->rank == 0)
	m = expand_scalar_where(c, pre, node.next);

    else {
	if (c->type == EXEC_CALL) {
	    c->ext.sub.actual->type = ARG_EXPR;
	    c->ext.sub.actual->next->type = ARG_EXPR;
	}

	m = expand_vector_where(c, pre, node.next);

	if (c->type == EXEC_CALL) {
	    c->expr = NULL;
	    c->expr2 = NULL;
	}
    }

    info = save;
    return m;
}



/* expand_where_assignment()-- Expand a WHERE assignment where a
 * temporary is required.  This means allocating a temporary array,
 * performing the masked assign to the temporary from the RHS, then
 * masked assign from the temporary to the LHS, then deallocating the
 * array. */

static g95_code *expand_where_assignment_t(g95_code *c) {
g95_code *d, *alloc, *dealloc, *a1, *a2, *p;
g95_typespec *ts;
g95_symbol *var;
g95_expr *e;
int rank;

    alloc = g95_get_code(EXEC_ALLOCATE, NULL);
    alloc->ext.alloc_list = g95_get_alloc();
    alloc->ext.alloc_list->expr = e = g95_get_expr();

    rank = c->expr->rank;
    ts   = &c->expr->ts;
    var  = g95_get_temporary(ts, rank, variable_corank(c->expr));

    e->type   = EXPR_VARIABLE;
    e->ts     = *ts;
    e->symbol = var;
    e->rank   = rank;
    e->where  = c->where;

    if (!find_section_spec(c->expr, alloc->ext.alloc_list, 0))
	g95_internal_error("expand_where_assignment_t(): "
			   "Can't find array part");

    d = g95_get_code(EXEC_ASSIGN, &c->where);

    d->expr = g95_get_variable_expr(var, &c->where);
    d->expr2 = c->expr2;

    c->expr2 = g95_get_variable_expr(var, &c->where);

    a1 = expand_where_assignment(d);
    a2 = expand_where_assignment(c);

    dealloc = g95_get_code(EXEC_DEALLOCATE, NULL);
    dealloc->ext.alloc_list = g95_get_alloc();
    dealloc->ext.alloc_list->expr = g95_get_variable_expr(var, &c->where);
    dealloc->ext.alloc_list->expr->where = g95_current_locus;

    /* Link the list */

    alloc->next = a1;

    p = a1;
    while(p->next != NULL)
	p = p->next;

    p->next = a2;

    while(p->next != NULL)
	p = p->next;

    p->next = dealloc;

    return alloc;
}


/* expand_where_body()-- Expand a single node of a WHERE statement.
 * The type is zero for the initial WHERE, one for the ELSEWHERE with
 * expressions and two for blank ELSEWHEREs. */

static g95_code *expand_where_body(g95_code *c) {
g95_ref *ref;
g95_code *m;
int i;

    if (c->type != EXEC_WHERE)
	c->next = c->block = NULL;

    switch(c->type) {
    case EXEC_ASSIGN:
	if (c->expr->rank == 0) {
	    m = expand_where_assignment(c);
	    break;
	}

	if (g95_expanding_forall()) {
	    c->type = EXEC_WHERE_ASSIGN;
	    m = expand_where_assignment(c);
	    break;
	}

	ref = c->expr->ref;
	while(!section_ref(ref))
	    ref = ref->next;

	i = g95_check_dependency(c->expr, c->expr2);

	if ((i & (SEEN_SECTION | SEEN_SCALAR | SEEN_EQUIV)) ||
	    ((i & SEEN_FULL) && ref->u.ar.type == AR_SECTION))
	    m = expand_where_assignment_t(c);
	else
	    m = expand_where_assignment(c);

	break;

    case EXEC_CALL:  /* Total Hack */
	c->expr  = c->ext.sub.actual->u.expr;
	c->expr2 = c->ext.sub.actual->next->u.expr;

	m = expand_where_assignment(c);
	break;

    case EXEC_WHERE:
	m = g95_expand_where(c);
	break;

    case EXEC_NOP:
	m = c;
	break;

    default:
	g95_internal_error("expand_where_body(): Bad statement");
    }

    return m;
}



/* expand_where_mask()-- Given a WHERE or ELSEWHERE mask, expand it
 * into the appropriate loops. */

static g95_code *expand_where_mask(g95_expr *e) {
g95_code node, *head, *m, *p;
assignment_info save;
g95_typespec ts;
int need_temp;

    memset(&node, '\0', sizeof(node)); 
    node.type = EXEC_NOP;

    save = info;
    memset(&info, '\0', sizeof(info));

    info.current_code = head = &node;
    info.prev_link = &head;

    scalarize_pass1(e, 1);

    if (e->ts.kind != g95_default_logical_kind()) {
	g95_clear_ts(&ts);
	ts.type = BT_LOGICAL;
	ts.kind = g95_default_logical_kind();

	g95_convert_type(e, &ts, 1);
    }

    switch(e->type) {
    case EXPR_VARIABLE:
	need_temp = 0;
	break;

    case EXPR_FUNCTION:
	need_temp = !g95_elemental_function(e);
	break;

    case EXPR_ARRAY:
	need_temp = 1;
	break;

    default:
	need_temp = !elemental_expr(e);
	break;
    }

    if (need_temp)
	e = temp_array(e, 0);

    /* Build the loops */

    m = where_sub(PREFIX "where_write", &e->where, e);
    m = where_loops(m, e, NULL);
    m = g95_forall_loops(m);

    /* Now string everything together */

    if (head == &node) {
	head = m;
	p = m;

    } else {
	p = head;
	while(p->next != &node)
	    p = p->next;

	p->next = m;
    }

    while(p->next != NULL)
	p = p->next;

    p->next = node.next;

    info = save;
    return head;
}



/* g95_expand_where()-- Expand a single WHERE node.  The language in
 * the standard is pretty confusing.  We transform something like:
 *
 *       WHERE(p1)        into    while_start()
 *         c1                     loops over p1
 *       ELSEWHERE (p2)               while_write1(p1(x))
 *         c2                     loops over c1
 *       ELSEWHERE (p3)               if (while_read1()) c1
 *         c3                     loops over p2
 *       ELSEWHERE                    while_writen(p2(x))
 *         c4                     loops over c2
 *                                    if (while_readn()) c2
 *                                loops over p3
 *                                    while_writen(p3(x))
 *                                loops over c3
 *                                    if (while_readn()) c3
 *                                loops over c4
 *				      if (!while_read1()) c4
 *                                while_end()
 *
 * The "control mask" and "pending control mask" are stored in the
 * runtime library. */

g95_code *g95_expand_where(g95_code *c) {
g95_code node, *d, *f, *g, *m, *current;
int flag;

    current = &node;
    memset(&node, '\0', sizeof(node));

    d = c->block;

    append_node(&current, where_sub(PREFIX "where_start", &c->where, NULL));

    m = expand_where_mask(d->expr);
    append_node(&current, m);

    for(f=d->next; f; f=g) {
	g = f->next;
	m = expand_where_body(f);
	append_node(&current, m);
    }

    d->next = NULL;
    flag = 1;

    for(d=d->block; d; d=d->block) {
	if (d->expr == NULL) {    /* Final ELSEWHERE clause */
	    m = where_sub(PREFIX "where_elsewhere2", &c->where, NULL);
	    append_node(&current, m);

	    for(f=d->next; f; f=g) {
		g = f->next;
		m = expand_where_body(f);
		append_node(&current, m);
	    }

	} else {
	    if (flag) {
		m = where_sub(PREFIX "where_elsewhere1", &c->where, NULL);
		append_node(&current, m);
		flag = 0;
	    }

	    m = expand_where_mask(d->expr);
	    append_node(&current, m);

	    for(f=d->next; f; f=g) {
		g = f->next;
		m = expand_where_body(f);
		append_node(&current, m);
	    }
	}

	d->next = NULL;
    }

    m = where_sub(PREFIX "where_done", &c->where, NULL);
    append_node(&current, m);

    c->type = EXEC_NOP;
    c->block = NULL;

    return node.next;
}



/* scalarize_scalar()-- Scalarize an expression that is ultimately
 * scalar.  If the original node involves a control transfer of some
 * sort, then we have to do cleanup prior to the node instead of after
 * it.  So if any nodes are added after the current one, we create a
 * temporary for the expression, and relink things so that the
 * reference to the temporary is after the dust settles.  The location
 * of the current node does not change, we just do some fancy
 * relinking.  */

static void scalarize_scalar(g95_code ***prev_link_p, g95_expr **e) {
g95_code **prev_link, *c, *n, *tail;
g95_symbol *var;
g95_locus where;

    prev_link = *prev_link_p;
    c = *prev_link;
    tail = c->next;

    scalarize_pass1(*e, 0);
    if (c->next == tail)
	return;

    /* Deal with cleanup code */

    var = g95_get_temporary(&(*e)->ts, 0, 0);

    n = g95_get_code(EXEC_ASSIGN, NULL);
    n->type  = EXEC_ASSIGN;
    n->expr  = g95_get_variable_expr(var, NULL);
    n->expr2 = *e;

    where = (*e)->where;
    *e = g95_get_variable_expr(var, &where);

    while(*prev_link != c)
	prev_link = &(*prev_link)->next;

    n->next = c->next;
    *prev_link = n;

    n = c;
    while(n->next != tail)
	n = n->next;

    c->next = n->next;
    n->next = c;

    *prev_link_p = &n->next;
}



/* g95_scalarize_spec_expr()-- Scalarize a specification expression.
 * Returns a list of g95_code nodes if something needs scalarization
 * or NULL if the expression has only scalar subexpressions.  Similar
 * to scalarize_scalar(), but made simpler by the face that no control
 * transfers are possible within the node. */

g95_code *g95_scalarize_spec_expr(g95_expr *e) {
g95_code *c, *link;
g95_locus where;
g95_symbol *var;
g95_expr *f;

    where = g95_current_locus;

    g95_current_locus = e->where;

    c = g95_get_code(EXEC_NOP, NULL);
    link = c;

    info.prev_link = &link;
    info.current_code = c;

    scalarize_pass1(e, 0);
    g95_current_locus = where;

    if (link->type == EXEC_NOP && link->next == NULL) {
	g95_free(link);
	link = NULL;

    } else {
	var = g95_get_temporary_int();

	c->type  = EXEC_ASSIGN;
	c->expr  = g95_get_variable_expr(var, &e->where);
	c->expr2 = g95_get_expr();

	*(c->expr2) = *e;
	f = g95_get_variable_expr(var, &e->where);

	*e = *f;
	g95_free(f);
    }

    return link;
}



/* allocatable_sc()-- Scalarize an assignment to an allocatable
 * structure component from a structure constructor component, which
 * is some array expression. */

static void allocatable_sc(g95_expr *component, g95_expr *rhs) {
g95_code *c, **link;
g95_locus *where;
g95_ref *ref;
g95_expr *e;

    if (rhs == NULL) {   /* Allocatable arrays default to NULL() */
	c = g95_get_code(EXEC_POINTER_ASSIGN, NULL);

	c->expr = component;
	c->expr2 = g95_null_expr(&component->where);

	insert_code_node(c, 1);
	advance_prev_link();

	return;
    }

    where = &rhs->where;

    /* Deallocate the old */

    c = g95_get_code(EXEC_DEALLOCATE, NULL);

    c->expr = g95_get_variable_expr(g95_get_array_int(), where);
    c->ext.alloc_list = g95_get_alloc();
    c->ext.alloc_list->expr = e = g95_copy_expr(component);

    ref = g95_extend_ref(e, REF_ARRAY, where);
    ref->u.ar.type = AR_FULL;

    insert_code_node(c, 1);
    advance_prev_link();

    if (rhs->type == EXPR_NULL)
	return;

    /* Allocate the new */

    c = g95_get_code(EXEC_ALLOCATE, NULL);

    c->ext.alloc_list       = g95_get_alloc();
    c->ext.alloc_list->expr = g95_copy_expr(component);

    if (!find_section_spec(rhs, c->ext.alloc_list, 1))
	g95_internal_error("scalarize_ac(): "
			   "Can't find array part of expression");

    insert_code_node(c, 1);
    advance_prev_link();

    /* Copy the new in */

    c = g95_get_code(EXEC_ASSIGN, NULL);

    c->expr  = component;
    c->expr2 = rhs;

    ref = g95_extend_ref(component, REF_ARRAY, where);
    ref->u.ar.type = AR_FULL;

    link = insert_code_node(c, 1);
    g95_scalarize_assignment(link);
}


/* scalarize_structure0()-- Given a base expression, create the scalar
 * assignments from each element of the constructor to copies of the
 * base with a structure reference added. */

static void scalarize_structure0(g95_expr *base, g95_constructor *cons) {
g95_code *n, **link;
g95_constructor *d;
g95_component *c;
g95_symbol *var;
g95_expr *e, *f;
g95_ref *ref;

    for(c=base->ts.derived->components; c; c=c->next, cons=d) {
	e = g95_copy_expr(base);

	ref = g95_extend_ref(e, REF_COMPONENT, &base->where);

	ref->u.c.name      = c->name;
	ref->u.c.component = c;

	e->ts = c->ts;
	if (c->as != NULL)
	    e->rank = c->as->rank;

	if (c->allocatable) {
	    allocatable_sc(e, cons->expr);

	} else if (cons->expr->type == EXPR_STRUCTURE &&
		   e->ts.type == BT_DERIVED && !c->pointer) {

	    if (c->as == NULL) {
		scalarize_structure0(e, cons->expr->value.constructor.c);
		g95_free_expr(e);

	    } else {
		var = g95_get_temporary(&e->ts, 0, 0);
		f = g95_get_variable_expr(var, &e->where);

		scalarize_structure0(f, cons->expr->value.constructor.c);

		n = g95_get_code(EXEC_ASSIGN, NULL);
		n->expr  = e;
		n->expr2 = f;
		goto insert;
	    }

	} else {

	    if (cons->expr->type == EXPR_ARRAY) {
		n = g95_get_code(EXEC_ASSIGN, NULL);
		n->type = EXEC_ASSIGN;
		n->expr = e;
		n->expr2 = cons->expr;

		cons->expr = NULL;
		scalarize_ac(n->expr2);

	    } else {
		n = g95_get_code((c->pointer || c->allocatable)
				 ? EXEC_POINTER_ASSIGN 
				 : EXEC_ASSIGN, NULL);
		n->expr  = e;
		n->expr2 = cons->expr;
	    }

	insert:
	    link = info.prev_link;
	    insert_code_node(n, 1);

	    advance_prev_link();

	    if (c->as != NULL) {
		ref = g95_extend_ref(n->expr, REF_ARRAY, &base->where);
		ref->u.ar.type = AR_FULL;

		n->expr->rank = c->as->rank;

		if (!c->pointer && !c->allocatable)
		    g95_scalarize_assignment(link);
	    }
	}

	d = cons->next;
	g95_free(cons);
    }
}



/* scalarize_structure()-- Scalarize a scalar assignment with a
 * structure constructor on the right.  For scalars, this replaces the
 * original assignment with multiple scalar assignments through a
 * pointer to the assignee. */

static void scalarize_structure(g95_code *c) {
g95_symbol *var;
g95_code *n;

    if (c->expr2->ts.derived->attr.itype != ITYPE_NONE)
	return;

    if (c->expr->rank > 0)
	g95_internal_error("scalarize_structure(): Vector assignment");

    var = g95_get_temporary(&c->expr->ts, 0, 0);
    var->attr.pointer = 1;

    n = g95_get_code(EXEC_POINTER_ASSIGN, NULL);
    n->expr = g95_get_variable_expr(var, &c->expr->where);
    n->expr2 = c->expr;

    insert_code_node(n, 1);
    advance_prev_link();

    scalarize_structure0(n->expr, c->expr2->value.constructor.c);

    c->type = EXEC_NOP;
    c->expr = NULL;
    c->expr2 = NULL;
}



/* scalarize_structure_transfer()-- Build a temporary for transferring
 * a raw structure. */

static void scalarize_structure_transfer(g95_typespec *ts) {
g95_symbol *var;
g95_code *c, *d;

    var = g95_get_temporary(ts, 0, 0);

    d = *info.prev_link;

    c = g95_get_code(EXEC_ASSIGN, NULL);

    c->expr  = g95_get_variable_expr(var, &d->where);
    c->expr2 = d->expr;
    c->next  = d;

    d->expr = g95_get_variable_expr(var, &d->where);

    scalarize_structure(c);
}



/* scalarize_ac_elements()-- Run scalarize_pass1() on all of the
 * elements of an array constructor without getting rid of the
 * constructor itself. */

static void scalarize_ac_elements(g95_constructor *c) {
g95_iterator *iter;

    for(; c; c=c->next) {
	if (c->expr->type == EXPR_ARRAY || c->expr->type == EXPR_STRUCTURE)
	    scalarize_ac_elements(c->expr->value.constructor.c);
	else
	    scalarize_pass1(c->expr, c->expr->rank != 0);

	iter = c->iterator;
	if (iter != NULL) {
	    scalarize_pass1(iter->start, 0);
	    scalarize_pass1(iter->end,   0);
	    scalarize_pass1(iter->step,  0);
	}
    }
}



/* simple_ac_assign()-- Do a 'simple' assignment from an array
 * constructor to an explicit-shaped array.  Returns nonzero if the
 * simple assignment was possible. */

static int simple_ac_assign(g95_code *code) {
bignum size, index[G95_MAX_DIMENSIONS];
g95_constructor *cons, *cons0;
g95_expr *template, *rhs;
g95_array_spec *as;
int rank, n, i, m;
g95_symbol *var;
g95_ref *ref;
g95_code *c;

    if (code->expr->rank == 0)
	return 0;

    template = code->expr;
    rhs = code->expr2;

    n = 0;
    switch(code->expr2->type) {
    case EXPR_ARRAY:
	/* Dispose of a truly bizarre case-- assignment of a zero-sized
	 * array.  Conformance has been checked earlier.  We transform
	 * this case to a no-op. */

	if (code->expr2->value.constructor.c == NULL) {
	    g95_free_expr(code->expr);
	    g95_free_expr(code->expr2);

	    code->expr = code->expr2 = NULL;
	    code->type = EXEC_NOP;
	    return 1;
	}

	n = !g95_expanded_ac(code->expr2);

	for(cons=code->expr2->value.constructor.c; cons; cons=cons->next)
	    if (cons->expr->rank > 0)
		return 0;

	break;

    case EXPR_VARIABLE:
    case EXPR_FUNCTION:
    case EXPR_OP:
	n  = (code->expr2->rank != 0);
	break;

    case EXPR_CONSTANT:
    case EXPR_STRUCTURE:
    case EXPR_NULL:
    case EXPR_SUBSTRING:
	break;

    default:
	g95_internal_error("simple_ac_assign(): Bad type");
    }

    if (n)
	return 0;

    as = template->symbol->as;
    m = 0;

    for(ref=template->ref; ref; ref=ref->next, m++)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type == AR_FULL && ref->next == NULL)
		goto found;

	    return 0;

	case REF_COMPONENT:
	    as = ref->u.c.component->as;
	    break;

	case REF_COARRAY:
	case REF_SUBSTRING:
	    break;
	}

    return 0;

found:
    if (as->type != AS_EXPLICIT)
	return 0;

    for(i=0; i<as->rank; i++)
	if (as->lower[i]->type != EXPR_CONSTANT ||
	    as->upper[i]->type != EXPR_CONSTANT)
	    return 0;

    size = g95_array_spec_size(as);
    if (size == NULL || bi_compare(big_copy(size), bi_1) < 0 ||
	bi_compare(size, int_to_bi(200)) > 0)
	return 0;

    if (code->expr2->type == EXPR_ARRAY || code->expr2->type == EXPR_STRUCTURE)
	scalarize_ac_elements(code->expr2->value.constructor.c);
    else
	scalarize_pass1(code->expr2, code->expr2->rank != 0);

    /* Expand the assignment */

    code->type = EXEC_NOP;
    code->expr = code->expr2 = NULL;

    if (rhs->type == EXPR_ARRAY) {
	var = NULL;
	cons = rhs->value.constructor.c;

    } else {
	var = g95_get_temporary(&rhs->ts, 0, 0);

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->expr = g95_get_variable_expr(var, &code->where);
	c->expr2 = rhs;

	advance_prev_link();
	scalarize_code(insert_code_node(c, 1));
	advance_prev_link();

	if (var->ts.type == BT_CHARACTER && var->as == NULL &&
	    var->ts.cl == &g95_unknown_charlen)
	    generate_deallocate(var, 0, &code->where);

	cons = NULL;
    }

    template->rank = 0;
    rank = as->rank;

    for(i=0; i<rank; i++)
	index[i] = big_copy(as->lower[i]->value.integer);

    for(;;) {
	if (cons == NULL && var == NULL)
	    break;

	c = g95_get_code(EXEC_ASSIGN, NULL);
	c->type = EXEC_ASSIGN;

	if (var != NULL)
	    c->expr2 = g95_get_variable_expr(var, &code->where);

	else {
	    c->expr2 = cons->expr;
	    cons0 = cons;
	    cons = cons->next;
	    g95_free(cons0);
	}

	c->expr = g95_copy_expr(template);
	ref = c->expr->ref;
	for(i=m; i>0; i--)
	    ref = ref->next;

	ref->u.ar.type = AR_ELEMENT;
	ref->u.ar.dimen = rank;

	for(i=0; i<rank; i++) {
	    ref->u.ar.start[i] =
		g95_constant_result(BT_INTEGER, -1, &code->where);
	    ref->u.ar.start[i]->value.integer = big_clone(big_copy(index[i]));
	    big_permanent(ref->u.ar.start[i]->value.integer);
	}

	advance_prev_link();
	scalarize_code(insert_code_node(c, 1));
	advance_prev_link();

	i = 0;
	for(;;) {
	    index[i] = bi_add(index[i], bi_1);
	    if (bi_compare(big_copy(index[i]),
			   as->upper[i]->value.integer) <= 0)
		break;

	    big_free(index[i]);
	    index[i] = big_copy(as->lower[i]->value.integer);

	    if (++i >= rank) {
		cons = NULL;
		var = NULL;
		break;
	    }
	}
    }

    g95_free_expr(template);
    for(i=0; i<rank; i++)
	big_free(index[i]);

    return 1;
}



/* scalarize_forall_body()-- Process a vector assignment that needs a
 * temporary by calling a helper subroutines. */

static void scalarize_forall_body(g95_code **prev_link) {
g95_actual_arglist *a, *b;
g95_code *code;

    code = *prev_link;

    a = g95_get_actual_arglist();
    a->next = b = g95_get_actual_arglist();

    a->type = ARG_ARRAY_DESC;
    a->u.expr = code->expr;

    b->type = ARG_ARRAY_DESC;
    b->u.expr = code->expr2;

    code->type = EXEC_CALL;
    code->expr = code->expr2 = NULL;
    code->ext.sub.actual = a;

    code->ext.sub.isym = &forall_copy_array;
    code->ext.sub.sub_name = PREFIX "forall_copy_array";

    scalarize_code(prev_link);
}



/* scalarize_code()-- Scalarize a single code node. */

static void scalarize_code(g95_code **prev_link) {
g95_code *code, *old_next;
assignment_info save;
g95_locus where;
int flag_save;
g95_alloc *a;
g95_expr *e;
int i;

    code = *prev_link;
    if (code == NULL)
	return;

    save = info;

    info.prev_link = prev_link;
    info.current_code = code;

    code = info.current_code;

    where = g95_current_locus;
    g95_current_locus = code->where;

    switch(code->type) {
    case EXEC_CALL:
	scalarize_call(code);
	break;

    case EXEC_DO:
	scalarize_scalar(&prev_link, &code->ext.iterator->start);
	scalarize_scalar(&prev_link, &code->ext.iterator->end);
	scalarize_scalar(&prev_link, &code->ext.iterator->step);

	traverse_code(&code->block);
	break;

    case EXEC_POINTER_ASSIGN:
	scalarize_pass1(code->expr, code->expr->rank != 0);
	scalarize_pass1(code->expr2, 0);
	break;

    case EXEC_ASSIGN:
	if (forall_sc_flag && code->expr2->rank > 0 &&
	    g95_find_variable(code->expr2, code->expr->symbol)) {
	    scalarize_forall_body(prev_link);
	    break;
	}

	if (simple_ac_assign(code))
	    break;

	scalarize_pass1(code->expr,  code->expr->rank  != 0);
	scalarize_pass1(code->expr2, code->expr2->rank != 0);

	if (code->expr->rank == 0) {  /* Scalar on the left */
	    if (code->expr2->rank)
		g95_internal_error("scalarize_code(): "
				   "scalar <- array assignment");

	    if (code->expr2->type == EXPR_STRUCTURE)
		scalarize_structure(code);

	    /* scalar <- scalar assignment */
	    break;
	}

	advance_prev_link();

	/* Array expression on the left */

	g95_scalarize_assignment(info.prev_link);
	break;

    case EXEC_IF:
	scalarize_scalar(&prev_link, &code->expr);

	traverse_code(&code->block);
	traverse_code(&code->ext.block);
	break;

    case EXEC_DO_WHILE:
	traverse_code(&code->block);
	break;

    case EXEC_SELECT:
	scalarize_scalar(&prev_link, &code->expr);

	if (code->expr2 != NULL)
	    scalarize_pass1(code->expr2, code->expr2->rank != 0);

	for(code=code->block; code; code=code->block)
	    traverse_code(&code->next);

	break;

    case EXEC_WHERE:
	flag_save = forall_sc_flag;
	forall_sc_flag = 0;

	insert_code_node(g95_expand_where(code), 0);

	forall_sc_flag = flag_save;
	break;

    case EXEC_FORALL:
	old_next = code->next;
	g95_expand_forall(code);
	flag_save = forall_sc_flag;
	forall_sc_flag = 1;

	while(code->next != old_next) {
	    scalarize_code(&code->next);
	    code = code->next;
	}

	forall_sc_flag = flag_save;
	break;

    case EXEC_ALLOCATE:
	for(a=code->ext.alloc_list; a; a=a->next) {
	    scalarize_ref(a->expr);

	    for(i=0; i<a->rank; i++) {
		if (a->lower[i] != NULL)
		    scalarize_scalar(&prev_link, &a->lower[i]);

		if (a->upper[i] != NULL)
		    scalarize_scalar(&prev_link, &a->upper[i]);
	    }
	}

	break;

    case EXEC_DEALLOCATE:
	for(a=code->ext.alloc_list; a; a=a->next)
	    scalarize_ref(a->expr);

	break;

    case EXEC_OPEN:
	scalarize_scalar(&prev_link, &code->ext.open->unit);
	scalarize_scalar(&prev_link, &code->ext.open->file);
	scalarize_scalar(&prev_link, &code->ext.open->status);
	scalarize_scalar(&prev_link, &code->ext.open->access);
	scalarize_scalar(&prev_link, &code->ext.open->form);
	scalarize_scalar(&prev_link, &code->ext.open->recl);
	scalarize_scalar(&prev_link, &code->ext.open->blank);
	scalarize_scalar(&prev_link, &code->ext.open->position);
	scalarize_scalar(&prev_link, &code->ext.open->action);
	scalarize_scalar(&prev_link, &code->ext.open->delim);
	scalarize_scalar(&prev_link, &code->ext.open->pad);
	scalarize_scalar(&prev_link, &code->ext.open->iostat);
	break;

    case EXEC_CLOSE:
	scalarize_scalar(&prev_link, &code->ext.close->unit);
	scalarize_scalar(&prev_link, &code->ext.close->status);
	scalarize_scalar(&prev_link, &code->ext.close->iostat);
	break;

    case EXEC_BACKSPACE:
    case EXEC_ENDFILE:
    case EXEC_REWIND:
	scalarize_scalar(&prev_link, &code->ext.filepos->unit);
	scalarize_scalar(&prev_link, &code->ext.filepos->iostat);
	break;

    case EXEC_READ:
    case EXEC_WRITE:
	scalarize_scalar(&prev_link, &code->ext.dt->io_unit);
	scalarize_scalar(&prev_link, &code->ext.dt->rec);
	scalarize_scalar(&prev_link, &code->ext.dt->advance);
	scalarize_scalar(&prev_link, &code->ext.dt->iostat);
	scalarize_scalar(&prev_link, &code->ext.dt->size);

	e = code->ext.dt->format_expr;
	if (e != NULL) {
	    if (e->rank == 0)
		scalarize_scalar(&prev_link, &code->ext.dt->format_expr);

	    else if (e->type == EXPR_OP)
		code->ext.dt->format_expr = temp_array(e, 0);

	    else
		scalarize_pass1(e, 0);
	}

	break;

    case EXEC_INQUIRE:
	scalarize_scalar(&prev_link, &code->ext.inquire->unit);
	scalarize_scalar(&prev_link, &code->ext.inquire->file);
	scalarize_scalar(&prev_link, &code->ext.inquire->iostat);
	scalarize_scalar(&prev_link, &code->ext.inquire->exist);
	scalarize_scalar(&prev_link, &code->ext.inquire->opened);
	scalarize_scalar(&prev_link, &code->ext.inquire->number);
	scalarize_scalar(&prev_link, &code->ext.inquire->named);
	scalarize_scalar(&prev_link, &code->ext.inquire->name);
	scalarize_scalar(&prev_link, &code->ext.inquire->access);
	scalarize_scalar(&prev_link, &code->ext.inquire->sequential);
	scalarize_scalar(&prev_link, &code->ext.inquire->direct);
	scalarize_scalar(&prev_link, &code->ext.inquire->form);
	scalarize_scalar(&prev_link, &code->ext.inquire->formatted);
	scalarize_scalar(&prev_link, &code->ext.inquire->unformatted);
	scalarize_scalar(&prev_link, &code->ext.inquire->recl);
	scalarize_scalar(&prev_link, &code->ext.inquire->nextrec);
	scalarize_scalar(&prev_link, &code->ext.inquire->blank);
	scalarize_scalar(&prev_link, &code->ext.inquire->position);
	scalarize_scalar(&prev_link, &code->ext.inquire->action);
	scalarize_scalar(&prev_link, &code->ext.inquire->read);
	scalarize_scalar(&prev_link, &code->ext.inquire->write);
	scalarize_scalar(&prev_link, &code->ext.inquire->readwrite);
	scalarize_scalar(&prev_link, &code->ext.inquire->delim);
	scalarize_scalar(&prev_link, &code->ext.inquire->pad);
	scalarize_scalar(&prev_link, &code->ext.inquire->iolength);
	break;

    case EXEC_TRANSFER:
	e = code->expr;
	scalarize_pass1(e, e->rank != 0);

	if (e->rank != 0)
	    scalarize_transfer(e, code->ext.transfer == M_READ);

	else if (e->type == EXPR_STRUCTURE)
	    scalarize_structure_transfer(&e->ts);

	break;

    case EXEC_RETURN:
    case EXEC_STOP:
    case EXEC_ARITHMETIC_IF:
    case EXEC_IOLENGTH:
	scalarize_scalar(&prev_link, &code->expr);
	break;

    case EXEC_SYNC_ALL:
    case EXEC_SYNC_TEAM:
    case EXEC_SYNC_MEMORY:
    case EXEC_SYNC_IMAGES:
    case EXEC_NOTIFY:
    case EXEC_QUERY:
	scalarize_scalar(&prev_link, &code->ext.sync.stat);
	scalarize_scalar(&prev_link, &code->ext.sync.errmsg);

	if (code->expr != NULL) {
	    if (code->expr->rank == 0)
		scalarize_scalar(&prev_link, &code->expr);

	    else {
		scalarize_pass1(code->expr, 1);

		if (code->expr->type == EXPR_ARRAY)
		    scalarize_ac(code->expr);
	    } 
	}

	break;

    default:
	break;
    }

    info = save;
    g95_current_locus = where;
}



/* traverse_code()-- Traverse the code tree, scalarizing as we go.
 * The 'next' member can change if scalarize_code() generates new
 * nodes. */

static void traverse_code(g95_code **prev_link) {
g95_code *code, *next;

    for(;;) {
	code = *prev_link;
	if (code == NULL)
	    break;

	next = code->next;
	scalarize_code(prev_link);

	while(code->next != next)
	    code = code->next;

	prev_link = &code->next;
    }
}



/* g95_scalarize()-- Scalarize a namespace, it's child and sibling
 * namespaces */

void g95_scalarize(g95_namespace *ns) {
g95_namespace *save;

    if (ns == NULL)
	return;

    save = g95_current_ns;
    g95_current_ns = ns;
    traverse_code(&ns->code);

    for(ns=ns->contained; ns; ns=ns->sibling)
	g95_scalarize(ns);

    g95_current_ns = save;
}
