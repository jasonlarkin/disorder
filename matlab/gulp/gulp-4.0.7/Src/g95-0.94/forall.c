
/* FORALL handling
   Copyright (C) 2003-2008 Free Software Foundation, Inc.
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


/* This module is responsible for expanding the masked assignment
 * statements WHERE and FORALL into sets of simpler statements. */

#include "g95.h"


static g95_intrinsic_sym forall_copy_array_done;
static g95_code *current_node;
static g95_intrinsic_sym forall_get, forall_save, forall_start, forall_done;
static int copy_done_flag, level=0;

static g95_forall_iterator *current_iterator;
static g95_expr *current_mask;



typedef struct forall_rename {
    g95_symbol *old, *new;
    struct forall_rename *next;
} forall_rename;

static forall_rename *rename_base;



/* insert_post()-- Insert a given node after the current node.  The
 * tail of the chain becomes the new current node. */

static void insert_post(g95_code *c) {
g95_code *tail;

    tail = c;
    while(tail->next != NULL)
	tail = tail->next;

    tail->next = current_node->next;
    current_node->next = c;

    current_node = tail;
}



/* find_mask_constructor()-- See if we can find a mask symbol in a
 * constructor */

static int find_mask_constructor(g95_constructor *c, g95_symbol *target) {
g95_iterator *iter;

    if (c == NULL)
	return 0;

    for(; c; c=c->next) {
	if (g95_find_variable(c->expr, target))
	    return 1;

	if (c->iterator != NULL) {
	    iter = c->iterator;
	    if (g95_find_variable(iter->start, target) ||
		g95_find_variable(iter->end,   target) ||
		g95_find_variable(iter->step,  target))
		return 1;
	}
    }

    return 0;
}



/* find_mask_ref()-- See if we can find a mask symbol in a g95_ref
 * structure. */

static int find_mask_ref(g95_ref *ref, g95_symbol *target) {
int i, rc;

    rc = 0;
    switch(ref->type) {
    case REF_ARRAY:
	for(i=0; i<G95_MAX_DIMENSIONS; i++)
	    if (g95_find_variable(ref->u.ar.start[i],  target) ||
		g95_find_variable(ref->u.ar.end[i],    target) ||
		g95_find_variable(ref->u.ar.stride[i], target))
		return 1;

	break;

    case REF_COARRAY:
	for(i=0; i<ref->u.car.dimen; i++)
	    if (g95_find_variable(ref->u.car.element[i], target))
		return 1;

	break;

    case REF_COMPONENT:
	break;

    case REF_SUBSTRING:
	if (g95_find_variable(ref->u.ss.start, target) ||
	    g95_find_variable(ref->u.ss.end, target))
	    return 1;

	break;
    }

    return 0;
}



/* g95_find_variable()-- Search through an expression tree to find the
 * target symbol.  If found, we return nonzero. */

int g95_find_variable(g95_expr *e, g95_symbol *target) {
g95_actual_arglist *arg;
g95_ref *ref;
int rc;

    if (e == NULL)
	return 0;

    rc = 0;

    switch(e->type) {
    case EXPR_OP:
	rc = g95_find_variable(e->value.op.op1, target) ||
	     g95_find_variable(e->value.op.op2, target);
	break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
	break;

    case EXPR_FUNCTION:
	for(arg=e->value.function.actual; arg; arg=arg->next)
	    if (g95_find_variable(arg->u.expr, target)) {
		rc = 1;
		break;
	    }

	break;

    case EXPR_VARIABLE:
	if (e->symbol == target) {
	    rc = 1;
	    break;
	}

	for(ref=e->ref; ref; ref=ref->next)
	    rc |= find_mask_ref(ref, target);

	break;

    case EXPR_SUBSTRING:
	rc = find_mask_ref(e->ref, target);
	break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
	rc = find_mask_constructor(e->value.constructor.c, target);
	break;

    default:
	g95_internal_error("g95_find_variable(): Bad expression");
    }

    return rc;
}



/* find_iter()-- Given an expression, see if it depends on any of the
 * iterator variables.  Returns nonzero if so. */

static int find_iter(g95_expr *e, g95_forall_iterator *f) {
g95_forall_iterator *g;

    if (e == NULL)
	return 0;

    for(g=f; g; g=g->next)
	if (g95_find_variable(e, g->var->symbol))
	    return 1;

    return 0;
}



/* find_expr_iter()-- Traverse an expression looking for the target
 * variable.  If we find it, search its subexpressions for any of the
 * iterator variables.  Return nonzero if we find what we're looking
 * for. */

static int find_expr_iter(g95_expr *e, g95_symbol *target,
			  g95_forall_iterator *f) {
g95_actual_arglist *a;
g95_constructor *c;
g95_ref *ref;
int i;

    if (e == NULL)
	return 0;

    switch(e->type) {
    case EXPR_OP:
	return find_expr_iter(e->value.op.op1, target, f) ||
   	       find_expr_iter(e->value.op.op1, target, f);

    case EXPR_FUNCTION:
	for(a=e->value.function.actual; a; a=a->next)
	    if (find_expr_iter(a->u.expr, target, f))
		return 1;

	break;

    case EXPR_VARIABLE:
	if (e->symbol == target)
	    for(ref=e->ref; ref; ref=ref->next)
		switch(ref->type) {
		case REF_ARRAY:
		    for(i=0; i<ref->u.ar.dimen; i++)
			if (find_iter(ref->u.ar.start[i], f) ||
			    find_iter(ref->u.ar.end[i], f) ||
			    find_iter(ref->u.ar.stride[i], f))
			    return 1;

		    break;

		case REF_COARRAY:
		    for(i=0; i<ref->u.car.dimen; i++)
			if (find_iter(ref->u.car.element[i], f))
			    return 1;

		    break;

		case REF_COMPONENT:
		    break;

		case REF_SUBSTRING:
		    if (find_iter(ref->u.ss.start, f) ||
			find_iter(ref->u.ss.end, f))
			return 1;

		    break;
		}

	/* Normal recursion now */

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (find_expr_iter(ref->u.ar.start[i], target, f) ||
			find_expr_iter(ref->u.ar.end[i], target, f) ||
			find_expr_iter(ref->u.ar.stride[i], target, f))
			return 1;

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    if (find_expr_iter(ref->u.car.element[i], target, f))
			return 1;

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		if (find_expr_iter(ref->u.ss.start, target, f) ||
		    find_expr_iter(ref->u.ss.end, target, f))
		    return 1;

		break;
	    }
  
	break;

    case EXPR_ARRAY:
    case EXPR_STRUCTURE:
	for(c=e->value.constructor.c; c; c=c->next) {

	    if (c->iterator != NULL &&
		(find_expr_iter(c->iterator->start, target, f) ||
		 find_expr_iter(c->iterator->end, target, f) ||
		 find_expr_iter(c->iterator->step, target, f)))
		return 1;

	    if (find_expr_iter(c->expr, target, f))
		return 1;
	}

	break;

    case EXPR_SUBSTRING:
	ref = e->ref;
    
	return find_expr_iter(ref->u.ss.start, target, f) ||
	       find_expr_iter(ref->u.ss.end, target, f);

    default:
	break;
    }

    return 0;
}



/* need_forall_temp()-- See if we need a forall temporary for an
 * assignment statement.  If the variable being assigned is 'A', then
 * we see if the reference to 'A' depends on any of the iterator
 * variables, or if any of the 'A' variables in the right depend on
 * any of the iterator variables.  We return nonzero if either is so. */

static int need_forall_temp(g95_forall_iterator *f, g95_expr *lhs,
			    g95_expr *rhs) {

    return find_expr_iter(rhs, lhs->symbol, f) ||
	(find_iter(lhs, f) && g95_find_variable(rhs, lhs->symbol));
}



/* rename_variable()-- Rewrite the variables in an expression from an
 * old symbol to an new one. */

static void rename_variable(g95_expr *e, g95_symbol *old, g95_symbol *new) {
g95_actual_arglist *a;
g95_constructor *c;
g95_ref *ref;
int i;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	rename_variable(e->value.op.op1, old, new);
	rename_variable(e->value.op.op2, old, new);
	break;

    case EXPR_FUNCTION:
	for(a=e->value.function.actual; a; a=a->next)
	    rename_variable(a->u.expr, old, new);

	break;

    case EXPR_VARIABLE:
	if (e->symbol == old)
	    e->symbol = new;

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++) {
		    rename_variable(ref->u.ar.start[i],  old, new);
		    rename_variable(ref->u.ar.end[i],    old, new);
		    rename_variable(ref->u.ar.stride[i], old, new);
		}

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    rename_variable(ref->u.car.element[i], old, new);

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		rename_variable(ref->u.ss.start, old, new);
		rename_variable(ref->u.ss.end,   old, new);
		break;
	    }

    case EXPR_ARRAY:
    case EXPR_STRUCTURE:
	for(c=e->value.constructor.c; c; c=c->next) {

	    if (c->iterator != NULL) {
		rename_variable(c->iterator->start, old, new);
		rename_variable(c->iterator->end,   old, new);
		rename_variable(c->iterator->step,  old, new);
	    }

	    rename_variable(c->expr, old, new);
	}

	break;

    case EXPR_SUBSTRING:
	ref = e->ref;

	rename_variable(ref->u.ss.start, old, new);
	rename_variable(ref->u.ss.end,   old, new);
	break;

    default:
	break;
    }
}



/* build_loops()-- Recursive function for building FORALL loops. */

static g95_code *build_loops(g95_forall_iterator *f, g95_expr *mask,
			     g95_code *base) {
g95_iterator *iter;
g95_code *c, *d;
g95_expr *e;

    if (f != NULL) {
	d = build_loops(f->next, mask, base);

	c = g95_get_code(EXEC_DO, &current_node->where);
	c->block = d;
	c->ext.iterator = iter = g95_get_iterator();

	iter->var   = g95_copy_expr(f->var);
	iter->start = g95_copy_expr(f->start);
	iter->end   = g95_copy_expr(f->end);
	iter->step  = g95_copy_expr(f->stride);

	if (f->next == NULL && mask != NULL)
	    c->block->block->ext.block = c;  /* Point CYCLE to its DO-loop */

    } else if (mask == NULL)  /* Bottom level */
	c = base;

    else {
	e = g95_get_expr();

	e->type = EXPR_OP;
	e->where = current_node->where;
	e->value.op.operator = INTRINSIC_NOT;
	e->ts.type = BT_LOGICAL;
	e->ts.kind = g95_default_logical_kind();
	e->value.op.op1 = mask;

	c = g95_get_code(EXEC_IF, &current_node->where);
	c->expr = e;

	c->block = d = g95_get_code(EXEC_CYCLE, &current_node->where);
	c->next = base;
    }

    return c;
}



/* g95_forall_loops()-- Given a code node, enclose it in the current
 * FORALL loops.  Returns the node itself if no foralls are active. */

g95_code *g95_forall_loops(g95_code *body) {

    return (current_iterator == NULL)
	? body
	: build_loops(current_iterator, g95_copy_expr(current_mask), body);
}



/* g95_expanding_forall()-- Return nonzero if we are in the process of
 * expanding a FORALL. */

int g95_expanding_forall(void) {

    return current_iterator != NULL;
}



/* forall_temp()-- Create a temporary for a forall loop specification */

static void forall_temp(g95_expr **e) {
g95_symbol *sym;
g95_expr *f;
g95_code *c;

    f = *e;
    if (f->type == EXPR_CONSTANT)
	return;

    sym = g95_get_temporary_int();

    c = g95_get_code(EXEC_ASSIGN, &current_node->where);
    c->expr  = g95_get_variable_expr(sym, &current_node->where);
    c->expr2 = f;

    insert_post(c);

    *e = g95_get_variable_expr(sym, &current_node->where);
}



static void forall_preamble(g95_code *c) {
g95_actual_arglist *arg;
g95_forall_iterator *f;
g95_code *d;

  /* Save loop indeces */

    for(f=c->ext.forall_iterator; f; f=f->next) {
	f->save = g95_get_temporary_int();

	d = g95_get_code(EXEC_ASSIGN, &current_node->where);
	d->expr  = g95_get_variable_expr(f->save, &current_node->where);
	d->expr2 = g95_copy_expr(f->var);
	insert_post(d);
    }

    for(f=c->ext.forall_iterator; f; f=f->next) {
	forall_temp(&f->start);
	forall_temp(&f->end);
	forall_temp(&f->stride);
    }

    if (c->expr != NULL) {
	d = g95_get_code(EXEC_CALL, &current_node->where);
	d->ext.sub.isym = &forall_start;
	d->ext.sub.sub_name = PREFIX "forall_start";
	insert_post(d);

	d = g95_get_code(EXEC_CALL, &current_node->where);
	d->ext.sub.isym = &forall_save;
	d->ext.sub.sub_name = PREFIX "forall_save";
	d->ext.sub.actual = arg = g95_get_actual_arglist();

	arg->type = ARG_EXPR;
	arg->u.expr = c->expr;     /* The mask */

	d = build_loops(c->ext.forall_iterator, NULL, d);
	insert_post(d);
    }
}



/* add_forall_temp()-- Given a variable, add it to list of variables
 * that are going to need a temporary. */

static void add_forall_temp(g95_symbol *sym) {
forall_rename *p;

    for(p=rename_base; p; p=p->next)
	if (p->old == sym)
	    return;

    p = g95_getmem(sizeof(forall_rename));
    p->old = sym;
    p->next = rename_base;

    rename_base = p;
}



/* find_where_temp()-- Traverse a WHERE tree to find any temporaries
 * required by a parent FORALL.  We examine each assignment and
 * determine if the LHS depends on a forall iterator.  The condition
 * for needing a temporary is the same as for a FORALL assignment
 * except that we consider the union of all expressions within a
 * WHERE.  The condition clauses for WHERE and ELSEWHERE clauses are
 * like right-hand-side expressions. */

static void find_where_temp(g95_code *c, g95_forall_iterator *f) {
g95_code *a, *b, *x, *y;

    for(a=c->block; a; a=a->block)
	for(b=a->next; b; b=b->next)
	    if (b->type == EXEC_WHERE)
		find_where_temp(b, f);

    /* Loop over all LHS expressions against all where-clauses */

    for(a=c->block; a; a=a->block)
	for(x=c->block; x; x=x->block)
	    for(y=x->next; y; y=y->next)
		if (y->type == EXEC_ASSIGN &&
		    need_forall_temp(f, y->expr, x->expr))
		    add_forall_temp(y->expr->symbol);

    /* Loop over all lhs vs all rhs expressions */

    for(a=c->block; a; a=a->block)
	for(b=a->next; b; b=b->next)
	    if (b->type == EXEC_ASSIGN)

		for(x=c->block; x; x=x->block)
		    for(y=x->next; y; y=y->next)
			if (y->type == EXEC_ASSIGN &&
			    need_forall_temp(f, y->expr, x->expr))
			    add_forall_temp(y->expr->symbol);
}



/* find_where_mask_temps()-- Find forall mask temporaries inside WHEREs. */

static void find_where_mask_temps(g95_expr *mask, g95_code *c) {
g95_code *d;

    for(; c; c=c->block)
	switch(c->type) {
	case EXEC_ASSIGN:
	    if (g95_find_variable(mask, c->expr->symbol))
		add_forall_temp(c->expr->symbol);

	    break;

	case EXEC_WHERE:
	    for(d=c->next; d; d=d->next)
		switch(d->type) {
		case EXEC_ASSIGN:
		    if (g95_find_variable(mask, d->expr->symbol))
			add_forall_temp(d->expr->symbol);

		case EXEC_WHERE:
		    find_where_mask_temps(mask, d->block);
		    break;

		default:
		    break;
		}

	    break;

	default:
	    break;
    }
}



/* find_forall_mask_temps()-- Find forall mask temporaries. */

static void find_forall_mask_temps(g95_expr *mask, g95_code *c) {

    for(; c; c=c->next)
	switch(c->type) {
	case EXEC_ASSIGN:
	    if (g95_find_variable(mask, c->expr->symbol))
		add_forall_temp(c->expr->symbol);

	    break;

	case EXEC_FORALL:
	    find_forall_mask_temps(mask, c->block);
	    break;

	case EXEC_WHERE:
	    find_where_mask_temps(mask, c->block);
	    break;

	default:
	    break;
	}
}


/* find_forall_temps()-- Recursive function for identifying
 * forall temporaries.  Some code nodes can be traversed more than
 * once, but this is OK because we're making a list of unique
 * variables. */

static void find_forall_temps(g95_code *c, g95_forall_iterator *f) {
g95_expr *mask, *lhs, *rhs;
g95_code *a;

    switch(c->type) {
    case EXEC_FORALL:
	mask = c->expr;
	find_forall_mask_temps(mask, c->block);

	for(a=c->block; a; a=a->next)
	    find_forall_temps(a, f);

	return;

    case EXEC_WHERE:
	find_where_temp(c, f);
	return;

    case EXEC_CALL:
	lhs = c->ext.sub.actual->u.expr;
	rhs = c->ext.sub.actual->next->u.expr;
	break;

    case EXEC_ASSIGN:
    case EXEC_POINTER_ASSIGN:
	lhs = c->expr;
	rhs = c->expr2;

	/* Vector assignment requiring a temporary is expanded elsewhere */

	if (c->type == EXEC_ASSIGN && rhs->rank > 0 &&
	    g95_find_variable(rhs, lhs->symbol)) {
	    copy_done_flag = 1;
	    return;
	}

	break;

    default:
	g95_internal_error("find_forall_temps(): Bad type");
    }

    if (need_forall_temp(f, lhs, rhs))
	add_forall_temp(lhs->symbol);
}



/* rename_forall_temps()-- Recursive function for renaming forall
 * temporaries.  Some code nodes can be traversed more than once, but
 * this is OK as well. */

static void rename_forall_temps(forall_rename *head, g95_code *c,
				g95_forall_iterator *f) {
g95_expr *lhs, *rhs;
forall_rename *p;
g95_code *a, *b;
g95_ref *ref;
int i;

    switch(c->type) {
    case EXEC_WHERE:
	for(a=c->block; a; a=a->block) {
	    if (a == c->block)
		for(p=head; p; p=p->next)
		    rename_variable(a->expr, p->old, p->new);

	    for(b=a->next; b; b=b->next)
		rename_forall_temps(head, b, f);
	}

	return;

    case EXEC_FORALL:
	if (c->expr != NULL)
	    for(p=head; p; p=p->next)
		rename_variable(c->expr, p->old, p->new);

	for(a=c->block; a; a=a->next)
	    rename_forall_temps(head, a, f);

	return;

    case EXEC_CALL:
	lhs = c->ext.sub.actual->u.expr;
	rhs = c->ext.sub.actual->next->u.expr;
	break;

    case EXEC_ASSIGN:
    case EXEC_POINTER_ASSIGN:
	lhs = c->expr;
	rhs = c->expr2;

	break;

    default:
	g95_internal_error("rename_forall_temps(): Bad type");
    }

    for(p=head; p; p=p->next) {
	rename_variable(rhs, p->old, p->new);

	if (lhs == NULL)
	    continue;

	for(ref=lhs->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++) {
		    rename_variable(ref->u.ar.start[i],  p->old, p->new);
		    rename_variable(ref->u.ar.end[i],    p->old, p->new);
		    rename_variable(ref->u.ar.stride[i], p->old, p->new);
		}

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    rename_variable(ref->u.car.element[i], p->old, p->new);

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		rename_variable(ref->u.ss.start, p->old, p->new);
		rename_variable(ref->u.ss.end,   p->old, p->new);
		break;
	    }
    }
}



/* array_temp()-- Create an array temporary for the named symbol,
 * and copy the existing value. */

static void array_temp(forall_rename *p) {
int i, k, rank, corank;
g95_actual_arglist *a;
g95_code *c, *d, *end;
g95_ref *ref;
g95_expr *e;

    rank = p->old->as->rank;

    corank = (p->old->cas == NULL)
	? 0
	: p->old->cas->corank;

    p->new = g95_get_temporary(&p->old->ts, rank, corank);

    /* Allocate the temporary */

    c = g95_get_code(EXEC_ALLOCATE, &current_node->where);
    c->ext.alloc_list = g95_get_alloc();
    c->ext.alloc_list->expr = e = g95_get_expr();

    e->type   = EXPR_VARIABLE;
    e->ts     = p->old->ts;
    e->symbol = p->new;
    e->rank   = rank;
    e->where  = current_node->where;

    ref = g95_extend_ref(e, REF_ARRAY, NULL);
    ref->u.ar.type = AR_FULL;
    c->ext.alloc_list->rank = rank;

    /* Allocate it with the (possibly current) bounds of the old variable */

    for(i=0; i<rank; i++)
	for(k=0; k<2; k++) {
	    e = g95_get_expr();
	    e->type = EXPR_FUNCTION;
	    e->value.function.isym =
		g95_find_function(k == 0 ? "lbound" : "ubound");
	    e->value.function.iname = e->value.function.isym->name;
	    e->where = g95_current_locus;

	    e->ts.type = BT_INTEGER;
	    e->ts.kind = g95_pointer_integer_kind();

	    e->value.function.actual = a = g95_get_actual_arglist();
	    a->type = ARG_ARRAY;
	    a->u.expr = g95_get_variable_expr(p->old, NULL);

	    a->next = g95_get_actual_arglist();
	    a       = a->next;

	    a->type          = ARG_EXPR;
	    a->u.expr        = g95_int_expr(i+1);
	    a->u.expr->where = g95_current_locus;

	    if (k == 0)
		c->ext.alloc_list->lower[i] = e;

	    else
		c->ext.alloc_list->upper[i] = e;
	}

    insert_post(c);

    /* Assign the current value to the temporary */

    d = g95_get_code(EXEC_ASSIGN, &current_node->where);

    d->expr  = g95_get_variable_expr(p->new, &current_node->where);
    d->expr2 = g95_get_variable_expr(p->old, &current_node->where);

    insert_post(d);
    end = current_node->next;

    g95_scalarize_assignment(&c->next);

    while(current_node->next != end)
	current_node = current_node->next;
}



/* scalar_temp()-- Create scalar temporary, which pretty much has to
 * be a character variable. */

static void scalar_temp(forall_rename *p) {
g95_code *c;

    p->new = g95_get_temporary(&p->old->ts, 0, 0);

    c = g95_get_code(EXEC_ASSIGN, &current_node->where);
    c->expr  = g95_get_variable_expr(p->new, &current_node->where);
    c->expr2 = g95_get_variable_expr(p->old, &current_node->where);

    insert_post(c);
}



/* forall_body()-- Process a single code node in the body of a FORALL
 * statement. */

static void forall_body(g95_forall_iterator *f, int mask, g95_code *c) {
forall_rename *p, *q, *head;
g95_expr *mask_expr;
g95_code *d;

    if (!mask)
	mask_expr = NULL;
    else {
	mask_expr = g95_build_funcall(NULL, NULL);
	mask_expr->value.function.isym = &forall_get;
	mask_expr->value.function.iname = PREFIX "forall_get";
	mask_expr->ts.type = BT_LOGICAL;
	mask_expr->ts.kind = g95_default_logical_kind();
	mask_expr->where = c->where;
    }

    find_forall_temps(c, f);

    head = rename_base;
    rename_base = NULL;

    for(p=head; p; p=p->next)
	if (p->old->as != NULL)
	    array_temp(p);
	else
	    scalar_temp(p);

    rename_forall_temps(head, c, f);

    switch(c->type) {
    case EXEC_FORALL:
	g95_expand_forall(c);
	d = build_loops(f, mask_expr, c);
	insert_post(d);
	break;

    case EXEC_WHERE:
	current_iterator = f;
	current_mask = mask_expr;

	d = g95_expand_where(c);
	insert_post(d);

	current_iterator = NULL;
	current_mask = NULL;
	break;

    case EXEC_CALL:
	d = build_loops(f, mask_expr, c);
	insert_post(d);
	break;

    case EXEC_ASSIGN:
    case EXEC_POINTER_ASSIGN:
	d = build_loops(f, mask_expr, c);
	insert_post(d);
	break;

    default:
	g95_internal_error("g95_expand_forall(): Bad code node");
    }

    /* Deallocate array temporaries */

    for(p=head; p; p=q) {
	q = p->next;

	if (p->old->as != NULL) {
	    c = g95_get_code(EXEC_DEALLOCATE, NULL);
	    c->ext.alloc_list = g95_get_alloc();
	    c->ext.alloc_list->expr =
		g95_get_variable_expr(p->new, &current_node->where);

	    insert_post(c);
	}

	g95_free(p);
    }
}



/* g95_expand_forall()-- Process a FORALL node */

void g95_expand_forall(g95_code *c) {
g95_code *save, *new, *n, *m, *end;
g95_forall_iterator *f, *g;
g95_locus where;

    if (level++ == 0)
	copy_done_flag = 0;

    save = current_node;
    current_node = c;
    end = current_node->next;

    where = g95_current_locus;
    g95_current_locus = c->where;

    forall_preamble(c);

    for(n=c->block; n; n=m) {
	m = n->next;
	n->next = NULL;

	forall_body(c->ext.forall_iterator, c->expr != NULL, n);
    }

    while(current_node->next != end)
	current_node = current_node->next;

    /* Restore loop indeces.  Free the forall_iterator without getting
     * rid of the expression nodes which are now used elsewhere. */

    for(f=c->ext.forall_iterator; f; f=g) {
	new = g95_get_code(EXEC_ASSIGN, &current_node->where);
	new->expr  = g95_copy_expr(f->var);
	new->expr2 = g95_get_variable_expr(f->save, &current_node->where);

	new->next = current_node->next;
	current_node->next = new;

	g95_free_expr(f->start);
	g95_free_expr(f->end);
	g95_free_expr(f->stride);

	g = f->next;
	g95_free(f);
    }

    if (c->expr != NULL) {
	n = g95_get_code(EXEC_CALL, &current_node->where);
	n->ext.sub.isym = &forall_done;
	n->ext.sub.sub_name = PREFIX "forall_done";

	n->next = current_node->next;
	current_node->next = n;
    }

    if (--level == 0 && copy_done_flag) {
	n = g95_get_code(EXEC_CALL, NULL);
	n->ext.sub.isym = &forall_copy_array_done;
	n->ext.sub.sub_name = PREFIX "forall_copy_array_done";
	insert_post(n);
    }

    c->type = EXEC_NOP;
    c->block = NULL;
    c->expr = NULL;
    c->where = current_node->where;
    c->ext.forall_iterator = NULL;

    current_node = save;
    g95_current_locus = where;
}



/* g95_resolve_forall_iterators()-- Resolve a list of FORALL iterators */

try g95_resolve_forall_iterators(g95_forall_iterator *f) {
g95_forall_iterator *iter, *iter2;
g95_typespec ts;
g95_symbol *sym;
g95_expr *e;

    for(iter=f; iter; iter=iter->next) {
	if (g95_resolve_variable(iter->var, 0) == FAILURE)
	    return FAILURE;

	if (iter->var->ts.type != BT_INTEGER || iter->var->rank != 0) {
	    g95_error("FORALL Iteration variable at %L must be a scalar "
		      "INTEGER", &iter->var->where);
	    return FAILURE;
	}

	sym = iter->var->symbol;

	if (G95_STRICT_F() &&
	    (sym->ns != g95_current_ns || sym->attr.use_assoc ||
	     sym->attr.dummy)) {
	    g95_error("FORALL-variable '%s' at %L must be in the same scope "
		      "as the FORALL under F", iter->var->symbol->name,
		      &iter->var->where);

	    return FAILURE;
	}

	sym->attr.st_construct = 1;

	if (g95_resolve_expr(iter->start) == FAILURE)
	    return FAILURE;

	if (iter->start->ts.type != BT_INTEGER || iter->start->rank != 0) {
	    g95_error("FORALL start expression at %L must be a scalar INTEGER",
		      &iter->start->where);
	    return FAILURE;
	}

	if (g95_resolve_expr(iter->end) == FAILURE)
	    return FAILURE;

	if (iter->end->ts.type != BT_INTEGER || iter->end->rank != 0) {
	    g95_error("FORALL end expression at %L must be a scalar INTEGER",
		      &iter->end->where);
	    return FAILURE;
	}

	if (g95_resolve_expr(iter->stride) == FAILURE)
	    return FAILURE;

	if (iter->stride->ts.type != BT_INTEGER || iter->stride->rank != 0) {
	    g95_error("FORALL stride expression at %L must be a scalar "
		      "INTEGER", &iter->stride->where);
	    return FAILURE;
	}

	if (iter->stride->type == EXPR_CONSTANT &&
	    bi_compare(iter->stride->value.integer, bi_0) == 0) {
	    g95_error("FORALL stride at %L cannot be zero",
		      &iter->stride->where);
	    return FAILURE;
	}

	g95_clear_ts(&ts);
	ts.type = BT_INTEGER;
	ts.kind = iter->var->ts.kind;

	if (iter->start->ts.type != ts.kind)
	    g95_convert_type(iter->start, &ts, 1);

	if (iter->end->ts.type != ts.kind)
	    g95_convert_type(iter->end, &ts, 1);

	if (iter->stride->ts.type != ts.kind)
	    g95_convert_type(iter->stride, &ts, 1);
    }

    /* Check bad combinations of variables */

    for(iter=f; iter; iter=iter->next)
	for(iter2=f; iter2; iter2=iter2->next) {
	    e = iter->start;
	    sym = iter2->var->symbol;

	    if (e != NULL && g95_find_variable(e, sym))
		goto bad;

	    e = iter->end;
	    if (e != NULL && g95_find_variable(e, sym))
		goto bad;

	    e = iter->stride;
	    if (e != NULL && g95_find_variable(e, sym))
		goto bad;
	}

    return SUCCESS;

bad:
    g95_error("Forall iterator '%s' cannot appear in subscript/stride "
	      "expression at %L", iter2->var->symbol->name, &e->where);
    return FAILURE;
}

