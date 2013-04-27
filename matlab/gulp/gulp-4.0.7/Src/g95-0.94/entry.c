/* Handle the ENTRY construct
   Copyright (C) 2004-2008 Free Software Foundation, Inc.
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

static g95_symbol *entries;


static void move_constructor(g95_constructor *, g95_symbol *, g95_symbol *);



/* g95_find_entries()-- Recursive function for finding and linking
 * entry symbols within a namespace. */

g95_symbol *g95_find_entries(g95_symtree *st, int flag) {
g95_symbol *sym;

    if (flag)
	entries = NULL;

    if (st == NULL)
	return NULL;

    sym = st->n.sym;

    if (sym->attr.entry) {
	sym->tlink = entries;
	entries = sym;
    }

    g95_find_entries(st->left, 0);
    g95_find_entries(st->right, 0);

    return entries;
}



/* augment_arglist()-- We search through an existing argument list for
 * the symbol and add it if it is not there. */

static void augment_arglist(g95_formal_arglist **fp, g95_symbol *sym) {
g95_formal_arglist *f;

    while(*fp != NULL) {
	if ((*fp)->sym == sym)
	    return;

	fp = &((*fp)->next);
    }

    f = g95_get_formal_arglist();
    *fp = f;

    f->sym = sym;
}



/* move_expr()-- Given the right hand side of the statement
 * function, replace the old symbol which is a formal argument with a
 * new symbol in the right namespace.  Recursively traverses the
 * expression tree. */

static void move_expr(g95_expr *e, g95_symbol *old, g95_symbol *new) {
g95_actual_arglist *a;
g95_ref *ref;
int i;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	move_expr(e->value.op.op1, old, new);
	move_expr(e->value.op.op2, old, new);
	break;

    case EXPR_UNKNOWN:
    case EXPR_VARIABLE:
    case EXPR_PROCEDURE:
	if (e->symbol == old)
	    e->symbol = new;

	/* Fall through */

    case EXPR_SUBSTRING:
	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<G95_MAX_DIMENSIONS; i++) {
		    move_expr(ref->u.ar.start[i], old, new);
		    move_expr(ref->u.ar.end[i], old, new);
		    move_expr(ref->u.ar.stride[i], old, new);
		}

		break;

	    case REF_SUBSTRING:
		move_expr(ref->u.ss.start, old, new);
		move_expr(ref->u.ss.end, old, new);
		break;

	    case REF_COMPONENT:
	    case REF_COARRAY:
		break;
	    }

	break;

    case EXPR_FUNCTION:
	for(a=e->value.function.actual; a; a=a->next)
	    move_expr(a->u.expr, old, new);

	break;

    case EXPR_CONSTANT:
    case EXPR_NULL:
	break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
	move_constructor(e->value.constructor.c, old, new);
	break;
    }
}



/* move_constructor()-- Mutually recursive with move_expr() to fix
 * symbol references within the right side of a statement function. */

static void move_constructor(g95_constructor *c, g95_symbol *old,
			     g95_symbol *new) {

    for(; c; c=c->next) {
	move_expr(c->expr, old, new);

	if (c->iterator == NULL)
	    continue;

	move_expr(c->iterator->start, old, new);
	move_expr(c->iterator->end,   old, new);
	move_expr(c->iterator->step,  old, new);
    }
}



/* add_entry()-- Populate a new namespace for an entry point, adding a
 * code node that calls the master function. */

static void add_entry(g95_namespace *master, g95_namespace *new, int n) {
g95_symbol *original, *sym, *result, *s, *t;
g95_actual_arglist *head, *tail;
g95_formal_arglist *f, *g;
g95_charlen *cl;
g95_symtree *st;
g95_code *c;
int i;

/* Build the formal argument list.  The symbols need to live in the
 * new namespace. */

    original = new->proc_name;

    if (new->parent == NULL) {
	g95_get_symbol(original->name, new, &sym);

    } else {
	st = g95_new_symtree(&new->sym_root, original->name);
	st->n.sym = original;
	original->refs++;
	sym = original;
    }

    sym->module = original->module;
    sym->attr   = original->attr;

    sym->attr.entry = 0;
    sym->bind       = original->bind;

    if (original->attr.function) {
	if (original->result == original)
	    original->attr.entry_result = 1;
	else
	    original->result->attr.entry_result = 1;
    }

    original->attr.entry = 1;

    if (original->attr.function) {
	sym->ts = original->result->ts;
	sym->result = sym;
    }

    sym->formal = g95_copy_formal_arglist(original->formal);

    if (original->attr.function && original->result->as != NULL) {
	if (sym != original)
	    sym->as = g95_copy_array_spec(original->as);

	sym->as->type = AS_DEFERRED;
	sym->attr.pointer = 1;
    }

    if (original->ns->state != COMP_MODULE)
	new->proc_name = sym;

    if (sym->attr.function) {
	result = sym->result;

	if (result->ts.type == BT_CHARACTER) {
	    cl = g95_get_charlen(new);
	    cl->length = g95_copy_expr(result->ts.cl->length);

	    result->ts.cl = cl;
	}

    } else {
	g95_current_ns = new;
	result = g95_get_temporary_int();
	g95_current_ns = master;
    }

    for(f=sym->formal; f; f=f->next) {
	if (f->sym == NULL)
	    continue;

	g95_get_symbol(f->sym->name, new, &s);

	if (f->sym->result == NULL)
	    s->ts = f->sym->ts;

	else {
	    s->result = s;
	    s->ts = f->sym->result->ts;
	}

	if (s->ts.type == BT_CHARACTER) {
	    cl = g95_get_charlen(new);

	    cl->length = g95_copy_expr(s->ts.cl->length);
	    s->ts.cl = cl;
	}

	s->attr = f->sym->attr;

	/* The public dummy variables are scalars to avoid any
	 * initialization.  The array spec is not copied. */

	s->attr.dimension = 0;
    }

    /* Fix any array specification expressions */

    for(f=sym->formal; f; f=f->next) {
	if (f->sym == NULL)
	    continue;

	g95_find_symbol(f->sym->name, new, 0, &s);

	for(g=sym->formal; g; g=g->next) {
	    if (g->sym == NULL)
		continue;

	    if (sym->ts.type == BT_CHARACTER)
		move_expr(sym->ts.cl->length, f->sym, s);

	    g95_find_symbol(g->sym->name, new, 0, &t);

	    if (t->ts.type == BT_CHARACTER)
		move_expr(t->ts.cl->length, f->sym, s);

	    if (t->as == NULL)
		continue;

	    for(i=0; i<G95_MAX_DIMENSIONS; i++) {
		move_expr(t->as->lower[i], f->sym, s);
		move_expr(t->as->upper[i], f->sym, s);
	    }
	}
    }

    for(f=new->proc_name->formal; f; f=f->next) {
	if (f->sym == NULL)
	    continue;

	g95_find_symbol(f->sym->name, new, 0, &s);
	f->sym = s;
    }

    head = g95_get_actual_arglist();
    head->type = ARG_EXPR;
    head->u.expr = g95_int_expr(n);

    head->next = tail = g95_get_actual_arglist();

    tail->type = (original->attr.subroutine || original->result->as == NULL)
	? ARG_EXPR
	: ARG_ARRAY_DESC;

    tail->u.expr = g95_get_variable_expr(result, NULL);
    tail->pointer = new->proc_name->result != NULL &&
	new->proc_name->result->attr.pointer;

    tail->next = g95_get_actual_arglist();
    tail = tail->next;
    tail->type = ARG_EXPR;
    tail->u.expr = (original->ts.type != BT_CHARACTER)
	? g95_null_expr(NULL)
	: g95_len_expr(g95_get_variable_expr(new->proc_name->result, NULL));

    g95_commit_symbols();

    for(f=master->proc_name->formal; f; f=f->next) {
	if (f->sym == NULL)
	    continue;

	for(g=new->proc_name->formal; g; g=g->next)
	    if (g->sym != NULL && strcmp(f->sym->name, g->sym->name) == 0)
		break;

	tail->next = g95_get_actual_arglist();
	tail = tail->next;

	if (g != NULL)
	    tail->u.expr = g95_get_variable_expr(g->sym, NULL);

	else {
	    tail->u.expr = NULL;
	    tail->missing_arg_type = f->sym->ts.type;
	}

	tail->type = ARG_EXPR;
	tail->pointer = f->sym->attr.pointer;
    }

    c = new->code = g95_get_code(EXEC_CALL, NULL);
    c->sym = master->proc_name;

    c->ext.sub.sub_name = master->proc_name->name;
    c->ext.sub.actual = head;
}



/* find_entry()-- Search a code tree for a particular entry node. */

static g95_code *find_entry(g95_code *base, g95_symbol *sym) {
g95_code *c;

    if (base == NULL)
	return NULL;

    for(; base; base=base->next) {
	if (base->type == EXEC_ENTRY && base->sym == sym)
	    break;

	c = find_entry(base->block, sym);
	if (c != NULL) {
	    base = c;
	    break;
	}
    }

    return base;
}



/* allocate_array_return()-- Allocate the array return value */

static g95_code *allocate_array_return(g95_symbol *sym, g95_symbol *var) {
g95_ref *ref;
g95_code *c;
g95_expr *e;
int i;

    c = g95_get_code(EXEC_ALLOCATE, NULL);
    c->ext.alloc_list       = g95_get_alloc();
    c->ext.alloc_list->expr = e = g95_get_expr();

    e->type   = EXPR_VARIABLE;
    e->ts     = sym->ts;
    e->symbol = var;
    e->rank   = sym->as->rank;
    e->where  = sym->declared_at;

    ref = g95_extend_ref(e, REF_ARRAY, NULL);
    ref->u.ar.type = AR_FULL;
    c->ext.alloc_list->rank = e->rank;

    for(i=0; i<e->rank; i++) {
	c->ext.alloc_list->lower[i] = g95_copy_expr(sym->as->lower[i]);
	c->ext.alloc_list->upper[i] = g95_copy_expr(sym->as->upper[i]);
    }

    return c;
}



/* entry_branch()-- Build a select statement for the master function.
 * The select variable is inserted as the first argument of the
 * procedure.  The select variable is zero for starting at the head of
 * the function, one for the first entry, two for the second entry and
 * so on. */

static void entry_branch(g95_namespace *master, g95_typespec *ts, int flag,
			 g95_symbol *primary) {
g95_code *head, *tail, *b, *c, *d;
g95_symbol *var, *sym, *result;
g95_formal_arglist *f;
g95_namespace *save;
g95_charlen *cl;
g95_case *cp;
g95_expr *e;
int rank, n;

    save = g95_current_ns;
    g95_current_ns = master;

    var = g95_get_temporary_int();   /* Character result length */
    var->ts.kind = g95_default_integer_kind(0);
    var->attr.dummy = 1;

    f = g95_get_formal_arglist();
    f->sym = var;
    f->next = master->proc_name->formal;

    master->proc_name->formal = f;

    result = g95_get_temporary(ts, 0, 0);  /* Result variable */
    result->attr.dummy = 1;

    rank = (primary->attr.subroutine || primary->result->as == NULL)
	? 0
	: primary->result->as->rank;

    if (rank == 0) {
	result->attr.pointer = flag;

	if (result->ts.type == BT_CHARACTER) {
	    result->ts.cl = cl = g95_get_charlen(master);
	    cl->length = e = g95_get_expr();

	    e->ts.type = BT_INTEGER;
	    e->ts.kind = g95_default_integer_kind(0);
	    e->type    = EXPR_VARIABLE;
	    e->symbol  = var;
	    e->rank    = 0;
	}

    } else {
	result->attr.pointer   = 1;
	result->attr.dimension = 1;
	result->as = g95_get_array_spec();
	result->as->rank = rank;
	result->as->type = AS_DEFERRED;
    }

    f = g95_get_formal_arglist();
    f->sym = result;
    f->next = master->proc_name->formal;

    master->proc_name->formal = f;

    var = g95_get_temporary_int();    /* Branch selector */
    var->ts.kind = g95_default_integer_kind(0);
    var->attr.dummy = 1;

    f = g95_get_formal_arglist();
    f->sym = var;
    f->next = master->proc_name->formal;

    master->proc_name->formal = f;

    /* Build the select statement. */

    head = tail = NULL;

    n = 1;
    for(sym=entries; sym; sym=sym->tlink) {
	if (head == NULL)
	    head = tail = g95_get_code(EXEC_SELECT, NULL);

	else {
	    tail->block = g95_get_code(EXEC_SELECT, NULL);
	    tail = tail->block;
	}

	cp = g95_get_case();
	cp->low = cp->high = g95_int_expr(n++);

	tail->ext.case_list = cp;
	tail->next = c = g95_get_code(EXEC_GOTO, NULL);

	if (rank > 0) {
	    if (!sym->attr.pointer) { /* Allocate the array return value */
		b = allocate_array_return(sym, result);
		b->next = c;
		tail->next = b;
	    }
	}

	d = find_entry(master->code, sym);
	if (d->here == NULL)
	    d->here = g95_get_st_label(-1);

	c->label = d->here;
    }

    if (rank > 0 && !primary->attr.pointer) {
	tail->block = g95_get_code(EXEC_SELECT, NULL);
	tail = tail->block;

	cp = g95_get_case();
	cp->low = cp->high = g95_int_expr(0);

	tail->ext.case_list = cp;
	tail->next = allocate_array_return(primary, result);
    }

    c = g95_get_code(EXEC_SELECT, NULL);
    c->block = head;
    c->expr  = g95_get_variable_expr(var, &c->where);

    c->next = master->code;
    master->code = c;

    g95_current_ns = save;
}



/* process_entry()-- Process the ENTRY statements within a namespace */

static void process_entry(g95_namespace *ns) {
g95_namespace *master, *new, temp;
char name[G95_MAX_SYMBOL_LEN+1];
g95_typespec integer, *ts;
g95_formal_arglist *f, *g;
g95_symbol *sym, *s;
static int serial=0;
g95_namespace *save;
g95_symtree *st;
int n;

    g95_find_entries(ns->sym_root, 1);
    if (entries == NULL)
	return;

    save = g95_current_ns;
    master = g95_get_namespace(ns->parent, 0);

    /* There are two reasons for switching the contents of the master
     * and ns nodes-- the symbols that live in the master namespace
     * must point to the master node, and we also want the master node
     * to be compiled first. */

    temp = *master;
    *master = *ns;
    *ns = temp;

    new = master;
    master = ns;
    ns = new;

    master->sibling = ns;

    master->sym_root = ns->sym_root;
    ns->sym_root = NULL;

    master->code = ns->code;
    ns->code = NULL;

    master->common_root = ns->common_root;
    ns->common_root = NULL;

    master->blank_common = ns->blank_common;
    ns->blank_common = NULL;

    master->equiv = ns->equiv;
    ns->equiv = NULL;

    master->st_labels = ns->st_labels;
    ns->st_labels = NULL;

    master->data = ns->data;
    ns->data = NULL;

    master->contained = ns->contained;
    ns->contained = NULL;

    master->format_label = ns->format_label;
    ns->format_label = 0;

    master->goto_label = ns->goto_label;
    ns->goto_label = 0;

    master->save_all = ns->save_all;
    ns->save_all = 0;

    f = g95_copy_formal_arglist(ns->proc_name->formal);

    for(sym=entries; sym!=NULL; sym=sym->tlink)
	for(g=sym->formal; g; g=g->next)
	    augment_arglist(&f, g->sym);

    /* Create the master function. */

    s = ns->proc_name;

    sprintf(name, "__g95_master_%d", serial++);
    g95_get_symbol(name, master, &sym);
    sym->formal = f;
    master->proc_name = sym;
    master->state = COMP_SUBROUTINE;

    sym->attr = s->attr;
    sym->attr.function   = 0;
    sym->attr.subroutine = 1;
    sym->attr.artificial = 1;
    sym->attr.bind       = 0;

    if (master->parent != NULL && master->parent->state == COMP_MODULE) {
	st = g95_new_symtree(&master->parent->sym_root, sym->name);
	st->n.sym = sym;
	sym->refs++;
	sym->ns = master->parent;

	sym->module = s->module;
    }

    g95_commit_symbols();

    add_entry(master, ns, 0);

    /* Add public procedures for the entry points. */

    n = 1;
    for(sym=entries; sym; sym=sym->tlink, n++) {
	new = g95_get_namespace(ns->parent, 0);
	new->proc_name = sym;
	new->state = sym->attr.function ? COMP_FUNCTION : COMP_SUBROUTINE;

	add_entry(master, new, n);

	new->sibling = master->sibling;
	master->sibling = new;
    }

    if (ns->proc_name->result != NULL) {
	ts = &ns->proc_name->result->ts;
	n = ns->proc_name->result->attr.pointer;

    } else {
	g95_clear_ts(&integer);
	integer.type = BT_INTEGER;
	integer.kind = g95_default_integer_kind(0);
	ts = &integer;
	n = 0;
    }

    entry_branch(master, ts, n, s);
    g95_current_ns = save;
}



/* g95_process_entry()-- Resolve entry statements in multiple or single
 * namespaces. */

void g95_process_entry(g95_namespace *ns) {

    if (ns->proc_name == NULL)
	return;

    if (ns->state != COMP_MODULE)
	process_entry(ns);

    else
	for(ns=ns->contained; ns; ns=ns->sibling)
	    if (ns->proc_name != NULL && !ns->proc_name->attr.entry)
		process_entry(ns);
}



/* entry_return_type()-- Make sure a function or its entries return
 * the allowed types.  Returns nonzero if an error was generated. */

static int entry_return_type(g95_symbol *sym) {
g95_symbol *r;

    r = sym->result;

    if (r->attr.pointer) {
	g95_error("ENTRY '%s' at %L cannot be a POINTER",
		  sym->name, &sym->declared_at);
	return 1;
    }

    if (sym->as != NULL) {
	g95_error("ENTRY '%s' at %L cannot be an array",
		  sym->name, &sym->declared_at);
	return 1;
    }

    switch(r->ts.type) {
    case BT_LOGICAL:
	if (r->ts.kind != g95_default_logical_kind())
	    goto type_error;

	break;

    case BT_INTEGER:
	if (r->ts.kind != g95_default_integer_kind(1))
	    goto type_error;

	break;

    case BT_REAL:
    case BT_COMPLEX:
	if (r->ts.kind != g95_default_real_kind(1) &&
	    r->ts.kind != g95_default_double_kind())
	    goto type_error;

	break;

    default:
	goto type_error;
    }

    return 0;

type_error:
    g95_error("ENTRY '%s' at %L cannot be of type %s",
	      sym->name, &sym->declared_at, g95_typename(&r->ts));
    return 1;
}



/* compare_entries()-- Compare the results of entry functions.  Return
 * nonzero if the characteristics are all the same. */

static int compare_entries(g95_symbol *s1, g95_symbol *s2) {
int i, rank, r1, r2;
bignum d1, d2;

    if (!g95_compare_types(&s1->ts, &s2->ts))
	return 0;

    if (s1->attr.pointer != s2->attr.pointer)
	return 0;

    r1 = (s1->as == NULL) ? 0 : s1->as->rank;
    r2 = (s2->as == NULL) ? 0 : s2->as->rank;
  
    if (r1 != r2)
	return 0;

    if (r1 == 0)
	return 1;

    rank = s1->as->rank;

    for(i=0; i<rank; i++) {
	d1 = g95_spec_dimen_size(s1->as, i);
	d2 = g95_spec_dimen_size(s2->as, i);

	if (d1 != NULL && d2 != NULL &&
	    bi_compare(big_copy(d1), big_copy(d2)) != 0) {

	    big_free(d1);
	    big_free(d2);
	    return 0;
	}

	if (d1 != NULL)
	    big_free(d1);

	if (d2 != NULL)
	    big_free(d2);
    }

    return 1;
}



/* g95_resolve_entry()-- Resolve ENTRY points */

try g95_resolve_entry(g95_namespace *ns) {
g95_symbol *sym;

    g95_find_entries(ns->sym_root, 1);

    if (entries == NULL || !ns->proc_name->attr.function)
	return SUCCESS;

/* If all the return variables have the same characteristics, then all
 * is well.  Otherwise, the return types can only have certain
 * characteristics. */

    for(sym=entries; sym; sym=sym->tlink) {
	if (!compare_entries(ns->proc_name->result, sym->result))
	    break;

	if (sym->attr.function && sym->value != NULL) {
	    g95_error("ENTRY '%s' at %L cannot have an initial value",
		      sym->name, &sym->value->where);
	    return FAILURE;
	}

	if (ns->proc_name->attr.recursive && sym->ts.type == BT_CHARACTER &&
	    sym->ts.cl->length == NULL) {
	    g95_error("RECURSIVE ENTRY '%s' at %L cannot return an "
		      "assumed-length CHARACTER", sym->name,
		      &sym->declared_at);
	    return FAILURE;
	}
    }

    if (sym == NULL)
	return SUCCESS;

    if (entry_return_type(ns->proc_name))
	return FAILURE;

    for(sym=entries; sym; sym=sym->tlink)
	if (!entry_return_type(sym))
	    return FAILURE;

    return SUCCESS;
}

