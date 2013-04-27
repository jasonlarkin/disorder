/* Expression translation
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

/* trans-expr.c-- generate SIMPLE trees for expressions.  */

#include "trans.h"


/* Tree for computing the addition chains used for the computation of
 * integer powers.  See the discussion in Knuth's "The Art of Computer
 * Programming", Vol 2, section 4.6.3.  This table has been typed in
 * from figure 14.  If we start at node i, this contains the index of
 * the previous node and so on to one.  So for example if we are
 * interested in the addition chain for 10, the chain (in reverse
 * order) is 10, 5, 3, 2, 1. 
 *
 * The addition chain is a series of numbers that satisfies
 *  a_i = a_j + a_k, where i > j >= k.
 *
 * We start by computing x and x^2.  Following the addition chain
 * leads us to the desired power by a series of multiplications.  This
 * table is special in that it gives the minimal number of
 * multiplications. */

static int power_tree[] = {  0,
  /*  1-10  */   0,  1,  2,  2,  3,  3,  5,  4,  8,  5,
  /* 11-20  */  10,  6,  9,  7, 12,  8,  9, 16, 14, 10,
  /* 21-30  */  11, 11, 20, 12, 24, 13, 15, 14, 28, 15,
  /* 31-40  */  21, 16, 32, 17, 26, 18, 36, 19, 27, 20,
  /* 41-50  */  40, 21, 34, 22, 30, 23, 46, 24, 33, 25,
  /* 51-60  */  48, 26, 51, 27, 54, 28, 56, 29, 56, 30,
  /* 61-70  */  52, 31, 60, 32, 64, 33, 66, 34, 68, 35,
  /* 71-80  */  70, 36, 72, 37, 60, 38, 43, 39, 78, 40,
  /* 81-90  */  65, 41, 80, 42, 80, 43, 86, 44, 88, 45,
  /* 91-100 */  90, 46, 92, 47, 85, 48, 96, 49, 96, 50, 0 };

#define MAX_CHAIN_LEN 50

tree g95_space;



/* g95_init_se()-- Initialize a simple expression holder.  Care must
 * be taken when multiple se are created with the same parent.  The
 * child se must be kept in sync.  The easiest way is to delay
 * creation of a child se until after after the previous se has been
 * translated. */

void g95_init_se(g95_se *se, g95_se *parent) {

    memset(se, '\0', sizeof(g95_se));

    g95_init_block(&se->pre);
    g95_init_block(&se->post);

    se->parent = parent;
}



/* g95_reflevel()-- Given something that is an object, a pointer to an
 * object or a pointer to a pointer to an object and a reference
 * level, generate code to bring the object to the desired level.
 * Level zero is a reference to some object, one is a pointer to an
 * object and two is a pointer to a pointer to the object. */

void g95_reflevel(g95_se *se, int level) {
tree tmp, type, new_type, object;

    object = se->expr;
    if (object == null_string_node)
	return;

    type = TREE_TYPE(object);

    if (level < 0 || level > 2)
	g95_internal_error("g95_reflevel(): Bad level");

    if (!POINTER_TYPE_P(type)) {
	/* Object is not a pointer */
	switch(level) {
	case 0:
	    break;

	case 1:
	    if (G95_CONSTANT_P(object) || TREE_CODE(object) == CALL_EXPR ||
		TREE_CODE(object) == NOP_EXPR ||
		(IS_EXPR_CODE_CLASS(TREE_CODE_CLASS(TREE_CODE(object))) &&
		 TREE_CODE(object) != COMPONENT_REF &&
		 TREE_CODE(object) != ARRAY_REF)) {
		tmp = g95_create_var(type);
		g95_add_modify_expr(&se->pre, tmp, object);
		object = tmp;
	    }

	    se->expr = g95_addr_expr(object);
	    break;

	case 2:
	    se->expr = g95_addr_expr(g95_addr_expr(object));
	    break;

	    /* Can't think of a case where this is needed */
	    g95_internal_error("g95_reflevel(): Bad case");
	}

	return;
    }

    new_type = TREE_TYPE(type);
    if (!POINTER_TYPE_P(new_type)) {
	/* Pointer to an object */
	switch(level) {
	case 0:
	    se->expr = g95_build_indirect(new_type, object);
	    break;

	case 1:
	    break;

	case 2:
	    se->expr = g95_addr_expr(object);
	    break;
	}

	return;
    }

    new_type = TREE_TYPE(new_type);
    if (!POINTER_TYPE_P(new_type)) {
	/* Pointer to a pointer to an object */
	switch(level) {
	case 0:
	    type = build_pointer_type(new_type);
	    object = g95_build_indirect(type, object);

	    se->expr = g95_build_indirect(new_type, object);
	    break;

	case 1:
	    se->expr = g95_build_indirect(build_pointer_type(new_type),object);
	    break;

	case 2:
	    break;
	}

	return;
    }

    g95_internal_error("g95_reflevel(): Bad type");
}



/* conv_substring()-- Converts a string into a substring.  The
 * standard specifies that the start and end indexes be within the
 * range of the string.  This implementation doesn't check for
 * violations of this condition, allowing access to memory outside of
 * the string.  To fix this, start and end would need to be clamped to
 * the legal ranges. */

static void conv_substring(g95_se *se, g95_ref *ref) {
tree start_minus_one, type, tmp, t, check;
g95_se start, end, length;
stmtblock_t block;

    g95_reflevel(se, 1);
    g95_init_se(&start, se);

    if (ref->u.ss.start == NULL)
	start.expr = integer_one_node;

    else
	g95_conv_expr_type(&start, ref->u.ss.start, g95_default_integer);

    start_minus_one = save_expr(g95_build_minus(g95_default_integer,
						start.expr, integer_one_node));

    /* Deal with the end specification */

    g95_init_se(&end, se);
    type = TREE_TYPE(se->expr);

    if (ref->u.ss.end == NULL)
	end.expr = se->string_length;

    else
	g95_conv_expr_type(&end, ref->u.ss.end, g95_default_integer);

    check = NULL_TREE;

    if (g95_option.bounds_check) {
	end.expr = save_expr(end.expr);
	se->string_length = save_expr(se->string_length);

	tmp = g95_build_lt(start_minus_one, integer_zero_node);

	t = g95_build_lt(end.expr, integer_one_node);
	tmp = g95_build_orif(t, tmp);

	t = g95_build_ge(start_minus_one, se->string_length);
	tmp = g95_build_orif(t, tmp);

	t = g95_build_gt(end.expr, se->string_length);
	tmp = g95_build_orif(t, tmp);

	t = g95_build_plus(g95_default_integer, start_minus_one,
			   integer_one_node);
	t = g95_build_le(t, end.expr);
	tmp = g95_build_andif(t, tmp);

	g95_init_block(&block);
	g95_set_locus(&block, &ref->where);

	t = g95_call_library(void_type_node, PREFIX "substring_oob",
			     se->string_length, start_minus_one, end.expr,
			     NULL_TREE);

	g95_add_expr_to_block(&block, t);

	t = g95_finish_block(&block);

	check = g95_build_cond(void_type_node, tmp, t, empty_stmt_node);
    }

    start_minus_one = convert(g95_pointer_integer, start_minus_one);
    end.expr = convert(g95_default_integer, end.expr);

    /* string length = DIM(end, start-1).  We choose this form because
     * 'start' is likely to be one, in which case the subtraction
     * collapses to DIM(end, 0). */

    g95_init_se(&length, se);
    g95_dim(&length, end.expr, start_minus_one);

    se->string_length = length.expr;
    se->expr = g95_build_plus(type, se->expr, start_minus_one);

    if (check != NULL_TREE)
	se->expr = build2(COMPOUND_EXPR, TREE_TYPE(se->expr), check, se->expr);
}



/* conv_constant_substring()-- This subroutine converts a substring of
 * a constant string. */

static void conv_constant_substring(g95_se *se, g95_expr *expr) {
int len;

    len = expr->value.character.length;
    se->expr = g95_build_string_const(len, expr->value.character.string);
    se->string_length = convert(g95_default_integer, g95_build_int(len, 0));

    conv_substring(se, expr->ref);
}



/* conv_component_ref()-- Convert a derived type component reference. */

static void conv_component_ref(g95_se *se, g95_ref *ref) {
tree tmp, decl, field;
g95_component *c;

    c = ref->u.c.component;
    g95_reflevel(se, 0);

    if (c->dimension || c->allocatable) {
	decl = c->backend_decl;

	G95_ARRAY_DESC(decl)    = NULL_TREE;
	G95_ARRAY_STORAGE(decl) = NULL_TREE;

	field = G95_ARRAY_FIELD(decl);
	tmp = g95_build_component(TREE_TYPE(field), se->expr, field);

	if (c->pointer || c->allocatable)
	    G95_ARRAY_DESC(decl) = tmp;

	else
	    G95_ARRAY_STORAGE(decl) = tmp;

	/* For allocate expressions */

	if (ref->u.c.alloc && c->cas != NULL && ref->next != NULL)
	    decl = g95_base_ref(G95_ARRAY_DESC(decl));

    } else {
	field = c->backend_decl;
	assert(TREE_CODE(field) == FIELD_DECL);

	decl = g95_build_component(TREE_TYPE(field), se->expr, field);
    }

    if (c->ts.type == BT_CHARACTER)
	se->string_length = c->ts.cl->backend_decl;

    se->expr = decl;
}



/* array_offset()-- Given an explicit array reference, convert it to a
 * byte offset into the array. */

static tree array_offset(g95_se *se, g95_typespec *ts, g95_array_spec *as,
			 g95_array_ref *ar) {
tree mult, sub, t, offset, extent;
g95_se se0;
bignum xt;
int i;

    mult   = g95_ts_size(ts);
    offset = NULL;
    sub    = NULL;

    for(i=0;; i++) {
	g95_init_se(&se0, se);
	g95_conv_expr(&se0, ar->start[i]);
	g95_raise_chains(&se0);

	t = g95_build_mult(g95_pointer_integer, se0.expr, mult);

	offset = (offset == NULL)
	    ? t
	    : g95_build_plus(g95_pointer_integer, offset, t);

	t = bi_to_tree(as->lower[i]->value.integer,
		       g95_pointer_integer_kind());
	t = g95_build_mult(g95_pointer_integer, t, mult);

	sub = (sub == NULL)
	    ? t
	    : g95_build_plus(g95_pointer_integer, sub, t);

	if (i >= as->rank - 1)
	    break;

	xt = bi_subtract(as->upper[i]->value.integer,
			 as->lower[i]->value.integer);
	xt = bi_add(xt, bi_1);

	extent = bi_to_tree(xt, g95_pointer_integer_kind());

	mult = g95_build_mult(g95_pointer_integer, mult, extent);
    }

    return (sub == NULL)
	? convert(g95_pointer_integer, offset)
	: g95_build_minus(g95_pointer_integer, offset, sub);
}



/* conv_static_coref()-- Convert an image reference to an image
 * number.  This handles that case of a static coarray with constant
 * bounds. */

static void conv_static_coref(g95_se *se, g95_coarray_spec *cas,
			      g95_ref *ref) {
bignum t, prod, extent, offset;
tree m, acc;
g95_se se0;
int i;

    prod   = bi_1;
    offset = bi_1;
    extent = bi_1;
    i = 0;
    acc = integer_zero_node;

    for(;;) {
	t = bi_multiply(cas->lower[i]->value.integer, big_copy(prod));
	offset = bi_subtract(offset, t);

	g95_init_se(&se0, se);
	g95_conv_expr(&se0, ref->u.car.element[i]);
	g95_raise_chains(&se0);

	m = bi_to_tree(big_copy(prod), g95_default_integer_kind(0));
	m = g95_build_mult(g95_default_integer, se0.expr, m);

	acc = g95_build_plus(g95_default_integer, acc, m);

	if (i >= ref->u.car.dimen - 1)
	    break;

	extent = bi_subtract(cas->upper[i]->value.integer,
			     cas->lower[i]->value.integer);
	extent = bi_add(extent, bi_1);

	prod = bi_multiply(prod, extent);
	i++;
    }

    big_free(prod);

    m = bi_to_tree(offset, g95_default_integer_kind(0));
    se->expr = g95_build_plus(g95_default_integer, acc, m);
}



/* conv_dynamic_coref()-- Convert an image reference to an image
 * number for a dummy or allocatable coarray. */

static void conv_dynamic_coref(g95_se *se, g95_ref *ref, int rank) {
tree index, t, desc;
g95_se se0;
int i;

    desc = se->expr;
    if (G95_ARRAY(desc) && G95_ARRAY_DESC(desc) != NULL)
	desc = G95_ARRAY_DESC(desc);

    index = NULL;
    i = ref->u.car.dimen - 1;

    for(;;) {
	g95_init_se(&se0, se);
	g95_conv_expr(&se0, ref->u.car.element[i]);
	g95_raise_chains(&se0);

	se0.expr = convert(g95_pointer_integer, se0.expr);
	t = g95_lbound_ref(desc, rank + i);

	t = g95_build_minus(g95_pointer_integer, se0.expr, t);

	index = (index == NULL)
	    ? t 
	    : g95_build_plus(g95_pointer_integer, index, t);

	if (i == 0)
	    break;

	i--;
	t = g95_multiplier_ref(desc, rank + i);

	index = g95_build_mult(g95_pointer_integer, index, t);
    }

    t = convert(g95_pointer_integer, integer_one_node);
    se->expr = g95_build_plus(g95_pointer_integer, index, t);
}



/* conv_coarray()-- Convert a coindexed variable.  This involves
 * converting the rest of the reference. */

static void conv_coarray(g95_se *se, g95_ref *ref, g95_typespec *ts,
			 g95_array_spec *as, g95_coarray_spec *cas,
			 g95_expr *e) {
tree arglist, reflist, typelist, offset, var, decl, t, field, base, image;
int rank, i, flag;
g95_component *c;
g95_se se0;
char *name;

    arglist = NULL;
    reflist = NULL;
    offset  = NULL;

    rank = (as == NULL)
	? 0 :
	as->rank;

    if (se->active_var == NULL) {
	if (e->rank != 0) {
	    t = g95_get_array_desc(e->rank, 0);
	    se->active_var = g95_create_var(t);

	} else {
	    t = g95_get_typenode(&e->ts, 0);

	    if (e->ts.type == BT_PROCEDURE)
		t = build_pointer_type(t);

	    se->active_var = g95_create_var(t);
	}
    }

    var = se->active_var;
    if (!POINTER_TYPE_P(TREE_TYPE(var)) || ts->type == BT_PROCEDURE)
	var = g95_addr_expr(var);

    g95_init_se(&se0, se);
    se0.expr = se->expr;

    /* See if we can use the simple or complicated version of coarray
     * references. */

    if (cas->type == CAS_DEFERRED)
	flag = 1;

    else {
	flag = 0;

	for(i=0; i<ref->u.car.dimen; i++) {
	    if (cas->lower[i]->type != EXPR_CONSTANT) {
		flag = 1;
		break;
	    }

	    if (i < ref->u.car.dimen - 1 &&
		cas->upper[i]->type != EXPR_CONSTANT) {
		flag = 1;
		break;
	    }
	}
    }

    if (flag)
	conv_dynamic_coref(&se0, ref, rank);

    else
	conv_static_coref(&se0, cas, ref);

    image = se0.expr;

    for(ref=ref->next; ref; ref=ref->next) {
	switch(ref->type) {
	case REF_ARRAY:
	    for(i=0; i<as->rank; i++)
		if (as->type != AS_EXPLICIT ||
		    as->lower[i]->type != EXPR_CONSTANT ||
		    (i < as->rank-1 && as->upper[i]->type != EXPR_CONSTANT))
		    break;

	    if (i == as->rank) {
		/* Figure out array offset at compile time */
		t = array_offset(se, ts, as, &ref->u.ar);

		offset = (offset == NULL)
		    ? t
		    : g95_build_plus(g95_default_integer, offset, t);

	    } else {
		/* Pointer array, cannot be assumed size. */

		if (offset != NULL) {
		    reflist = g95_chainon_list(reflist, offset);
		    offset = NULL;
		}

		t = g95_build_int(-as->rank, -1);
		reflist = g95_chainon_list(reflist, t);

		for(i=0; i<as->rank; i++) {
		    g95_init_se(&se0, se);
		    g95_conv_expr(&se0, ref->u.ar.start[i]);
		    se0.expr = save_expr(se0.expr);
		    g95_raise_chains(&se0);

		    t = convert(g95_pointer_integer, se0.expr);
		    reflist = g95_chainon_list(reflist, t);
		}
	    }

	    as = NULL;
	    break;

	case REF_COMPONENT:
	    c = ref->u.c.component;

	    field = c->backend_decl;
	    if (c->dimension)
		field = G95_ARRAY_FIELD(field);

	    /* Even if the offset is zero, we have to include it to
	     * get the referencer in the right state for array
	     * deferences. */

	    t = byte_position(field);
	    offset = (offset == NULL)
		? t
		: g95_build_plus(g95_default_integer, offset, t);

	    as = c->as;
	    ts = &c->ts;

	    if (c->pointer && as == NULL) {  /* Deference non-array pointers */
		if (offset != NULL) {
		    reflist = g95_chainon_list(reflist, offset);
		    offset = NULL;
		}

		t = g95_build_int(-G95_MAX_DIMENSIONS - 1, -1);
		reflist = g95_chainon_list(reflist, t);
	    }

	    break;

	case REF_SUBSTRING:
	    if (ref->u.ss.start != NULL) {
		g95_init_se(&se0, se);
		g95_conv_expr(&se0, ref->u.ss.start);
		se0.expr = save_expr(se0.expr);
		g95_raise_chains(&se0);

		offset = (offset == NULL)
		    ? se0.expr
		    : g95_build_plus(g95_default_integer, offset, se0.expr);
	    }

	    if (ref->u.ss.end != NULL) {
		g95_init_se(&se0, se);
		g95_conv_expr(&se0, ref->u.ss.end);
		se0.expr = save_expr(se0.expr);
		g95_raise_chains(&se0);

		reflist = g95_chainon_list(reflist, se0.expr);
	    }

	    as = NULL;
	    break;

	case REF_COARRAY:
	    break;

	default:
	    g95_internal_error("conv_coarray(): Bad ref");
	}
    }

    if (offset != NULL)
	reflist = g95_chainon_list(reflist, offset);

    arglist = g95_chainon_list(arglist, var);

    /* Calculate the base descriptor */

    t = se->expr;
    if (G95_ARRAY(t))
	t = G95_ARRAY_DESC(t);

    base = g95_addr_expr(t);

    arglist = g95_chainon_list(arglist, base);
    arglist = g95_chainon_list(arglist, image);

    t = g95_build_int(-9999, -1);
    reflist = g95_chainon_list(reflist, t);

    if (as != NULL && as->type == AS_DEFERRED)
	t = g95_build_int(-as->rank, -1);

    else if (g95_pointer_expr(e))
	t = size_in_bytes(pvoid_type_node);

    else
	t = g95_ts_size(ts);

    arglist = g95_chainon_list(arglist, t);
    arglist = chainon(arglist, reflist);  /* Combining lists here */

    typelist = NULL;

    for(t=arglist; t; t=TREE_CHAIN(t))
	typelist = g95_chainon_list(typelist, TREE_TYPE(TREE_VALUE(t)));

    if (se->load_pre || !se->store_post) {
	name = PREFIX "load_image";

	decl = build_function_type(void_type_node, typelist);
	decl = build_decl(FUNCTION_DECL, get_identifier(name), decl);

	DECL_EXTERNAL(decl) = 1;
	TREE_PUBLIC(decl)   = 1;

	pushdecl(decl);
	rest_of_decl_compilation(decl, 1, 0);

	decl = g95_build_function_call(decl, arglist);
	g95_add_expr_to_block(&se->pre, decl);
    }

    if (se->store_post) {
	name = PREFIX "store_image";

	decl = build_function_type(void_type_node, typelist);
	decl = build_decl(FUNCTION_DECL, get_identifier(name), decl);

	DECL_EXTERNAL(decl) = 1;
	TREE_PUBLIC(decl)   = 1;

	pushdecl(decl);
	rest_of_decl_compilation(decl, 1, 0);

	decl = g95_build_function_call(decl, arglist);
	g95_add_expr_to_block(&se->post, decl);
    }

    se->expr = var;
}



/* deref_coarray()-- Dereference a scalar coarray in the reference
 * sequence. */

static void deref_coarray(g95_se *se, g95_typespec *ts) {
tree field, type, desc;

    if (G95_COARRAY(se->expr)) {
	desc = G95_ARRAY_DESC(se->expr);
	field = g95_find_field(TREE_TYPE(desc), "base");

	type = build_pointer_type(g95_get_typenode(ts, 0));
	se->expr = g95_build_component(type, desc, field);
    }
}



/* conv_variable()-- Return the contents of a variable.  Also handles
 * reference/pointer variables (all Fortran pointer references are
 * implicit).  When processing a variable reference the rule is to do
 * as little as possible.  If an array reference is computed, a
 * pointer to the reference is computed. */

static void conv_variable(g95_se *se, g95_expr *expr) {
g95_ref *ref, *next_ref;
g95_coarray_spec *cas;
g95_array_spec *as;
g95_typespec *ts;
g95_symbol *sym;

    sym = expr->symbol;
    if (sym->backend_decl == NULL_TREE)
	g95_get_symbol_decl(sym);

    se->expr = sym->backend_decl;

    if (TREE_CODE(sym->backend_decl) == VAR_DECL ||
	TREE_CODE(sym->backend_decl) == PARM_DECL)
	TREE_USED(sym->backend_decl) = 1;

    if (sym->ts.type == BT_CHARACTER && !sym->attr.external &&
	!sym->attr.intrinsic && expr->ts.type != BT_PROCEDURE)

	se->string_length = (sym->ts.cl->backend_decl != NULL)
	    ? sym->ts.cl->backend_decl
	    : g95_esize_value(sym->backend_decl);

    ts  = &sym->ts;
    as  = sym->as;
    cas = sym->cas;

    for(ref=expr->ref; ref; ref=ref->next) {
	switch(ref->type) {
	case REF_ARRAY:
	    next_ref = ref->next;

	    if (next_ref == NULL || next_ref->type != REF_COARRAY)
		g95_array_element_ref(se, &ref->u.ar, ts, as,
				      cas != NULL, &ref->where);

	    else {
		/* Coarray references really should precede array
		 * references, but we're stuck with the notation.
		 * Temporarily reverse the list. */

		ref->next = next_ref->next;
		next_ref->next = ref;

		conv_coarray(se, next_ref, ts, as, cas, expr);

		next_ref->next = ref->next;
		ref->next = next_ref;

		goto done;
	    }

	    break;

	case REF_COMPONENT:
	    deref_coarray(se, ts);

	    conv_component_ref(se, ref);
	    ts  = &ref->u.c.component->ts;
	    as  = ref->u.c.component->as;
	    cas = ref->u.c.component->cas;

	    if (ts->cl != NULL)
		se->string_length = ts->cl->backend_decl;

	    break;

	case REF_SUBSTRING:
	    conv_substring(se, ref);
	    break;

	case REF_COARRAY:
	    conv_coarray(se, ref, ts, as, cas, expr);
	    goto done;

	default:
	    g95_internal_error("conv_variable(): Bad ref");
	    break;
	}	
    }

    if (expr->rank == 0)
	deref_coarray(se, ts);

done:
    return;
}



/* conv_unary_op()-- Convert unary operators .not. and minus */

static void conv_unary_op(enum tree_code code, g95_se *se, g95_expr *expr) {
g95_se operand;

    g95_init_se(&operand, se);
    g95_conv_expr(&operand, expr->value.op.op1);

    /* TRUTH_NOT_EXPR is not a unary operator in GCC.  We must convert it
     * to a comparison with zero (e.g. EQ_EXPR (op1, 0)).  All other
     * unary operators have an equivalent SIMPLE unary operator  */

    if (code == TRUTH_NOT_EXPR)
	se->expr = build2(EQ_EXPR, TREE_TYPE(operand.expr), operand.expr,
			  integer_zero_node);
    else
	se->expr = build1(code, TREE_TYPE(operand.expr), operand.expr);

    se->expr = fold(se->expr);
}



/* reciprocal()-- Given a tree, return the reciprocal. */

static tree reciprocal(tree x) {
tree tmp;

    if (TREE_TYPE(x) == TREE_TYPE(integer_one_node))
	tmp = build2(TRUNC_DIV_EXPR, TREE_TYPE(x), integer_one_node, x);

    else {
	tmp = g95_build_const(TREE_TYPE(x), integer_one_node);
	tmp = build2(RDIV_EXPR, TREE_TYPE(x), tmp, x);
    }

    return tmp;
}



static void chain_multiply(g95_se *se, tree base, int power) {
int i, j, k, chain_length, negative, addition_chain[MAX_CHAIN_LEN];
tree tmp, intermediate[MAX_CHAIN_LEN];

    if (power > 0)
	negative = 0;

    else {
	negative = 1;
	power = -power;
    }

    chain_length = 0;
    for(i=power; i!=0; i=power_tree[i])
	chain_length++;

    if (chain_length > MAX_CHAIN_LEN)
	g95_internal_error("chain_multiply(): Table too small");

    j = 0;
    for(i=power; i!=0; i=power_tree[i], j++)
	addition_chain[chain_length-j-1] = i;

    /* Now we have the chain.  Compute x and x^2 and store them to
     * temporaries. */

    intermediate[0] = save_expr(base);
    intermediate[1] = g95_build_mult(TREE_TYPE(base), intermediate[0],
				     intermediate[0]);

    tmp = NULL_TREE;

    /* Now traverse the chain.  If we're not at the end of the chain,
     * store the result to a temporary so that it can be re-used without
     * being recomputed.  The last element is the result of the whole
     * expression. */

    for(i=2; i<chain_length; i++) {
	for(j=0; j<i; j++)
	    for(k=0; k<i; k++)
		if (addition_chain[j] + addition_chain[k] == addition_chain[i])
		    goto found;

	g95_internal_error("chain_multiply(): Corrupt addition chain for n=%d",
			   power);
    found:
	tmp = g95_build_mult(TREE_TYPE(base), intermediate[j],intermediate[k]);

	if (i != chain_length-1)
	    intermediate[i] = save_expr(tmp);
    }

    if (negative)
	tmp = reciprocal(tmp);

    se->expr = tmp;
}



/* integer_exponent()-- Handle the case of constant integer exponent.
 * Returns nonzero if the exponent was such that we want to call the
 * library function, zero if we've taken care of everything. */

static int integer_exponent(g95_se *se, g95_expr *b, bignum exponent) {
int i, *ip, power;
g95_se se0;
tree base;

    if (bi_compare(big_copy(exponent), bi_maxint) > 0 ||
	bi_compare(big_copy(exponent), bi_minint) < 0)
	return 1;

    power = bi_to_int(exponent);

    /* Special cases of the special case */

    if (-2 <= power && power <= 2) {
	g95_init_se(&se0, se);
	g95_conv_expr(&se0, b);
	base = se0.expr;

	switch(power) {
	case -2:
	    base = save_expr(base);
	    se->expr = reciprocal(g95_build_mult(TREE_TYPE(base), base, base));
	    return 0;

	case -1:
	    se->expr = reciprocal(base);
	    return 0;

	case 0:
	    se->expr = convert(TREE_TYPE(base), integer_one_node);
	    return 0;

	case 1:
	    se->expr = base;
	    return 0;

	case 2:
	    base = save_expr(base);
	    se->expr = g95_build_mult(TREE_TYPE(base), base, base);
	    return 0;
	}
    }

    /* See if we have the information on how to do this with the minimum
     * multiplications. */

    if (power == -power)
	return 1;

    ip = power_tree + 2;
    for(i=2; i<abs(power); i++, ip++)
	if (*ip == 0)
	    return 1;   /* Hit the end of the table */

    g95_init_se(&se0, se);
    g95_conv_expr(&se0, b);
    base = se0.expr;

    chain_multiply(se, base, power); 
    return 0;
}



/* intrinsic_power()-- Try to use builtin functions for exponentiation. */

static tree intrinsic_power(g95_typespec *ts, tree base, tree exp) {
tree decl, args;

    if (ts->type != BT_REAL)
	return NULL_TREE;

    decl = NULL_TREE;

    if (g95_default_real_kind(0) == ts->kind)
	decl = built_in_decls[BUILT_IN_POWF];

    if (g95_default_double_kind() == ts->kind)
	decl = built_in_decls[BUILT_IN_POW];

    if (decl == NULL_TREE)
	return NULL_TREE;

    args = g95_chainon_list(NULL_TREE, base);
    args = g95_chainon_list(args, exp);

    return g95_build_function_call(decl, args);
}



/* library_power()-- Call a library subroutine to evaluate the
 * exponentiation operator.  There are two main cases to worry about:
 * an integer exponent, and a base and exponent with the same types */

static void library_power(g95_se *se, g95_expr *expr) {
tree tmp, base, exp, type;
g95_typespec *base_ts;
char name[100];
int kind, flag;
g95_se se0;

    base_ts = &expr->value.op.op1->ts;

    g95_init_se(&se0, se);

    kind = expr->value.op.op1->ts.kind;
    flag = (kind != g95_default_real_kind(0) &&
	    kind != g95_default_double_kind());

    se0.reflevel = (base_ts->type == BT_COMPLEX || flag);
    g95_conv_expr(&se0, expr->value.op.op1);

    base = se0.expr;

    type = TREE_TYPE(base);
    if (se0.reflevel)
	type = TREE_TYPE(type);

    se0.reflevel = (expr->value.op.op2->ts.type == BT_COMPLEX || flag);
    g95_conv_expr(&se0, expr->value.op.op2);

    exp = se0.expr;

    if (expr->value.op.op2->ts.type == BT_INTEGER)
	sprintf(name, PREFIX "power_%c%d_i%d", g95_type_letter(base_ts->type),
		base_ts->kind, expr->value.op.op2->ts.kind);

    else {
	se->expr = intrinsic_power(base_ts, base, exp);
	if (se->expr != NULL)
	    return;

	sprintf(name, PREFIX "power_%c%d", 
		g95_type_letter(base_ts->type), base_ts->kind);
    }

    if (base_ts->type != BT_COMPLEX)
	se->expr = g95_call_library(type, name, base, exp, NULL_TREE);

    else {
	se->expr = g95_create_var(g95_get_typenode(base_ts, 0));
	TREE_ADDRESSABLE(se->expr) = 1;

	tmp = g95_addr_expr(se->expr);
	tmp = g95_call_library(void_type_node, name, tmp, base, exp,NULL_TREE);
	g95_add_expr_to_block(&se->pre, tmp);
    }
}



/* conv_power_op()-- Handle the exponentiation operator. */

static void conv_power_op(g95_se *se, g95_expr *expr) {
int use_library;
ff_class class;
bignum p1, p2;
g95_ff *ff;

    use_library = 1;

    if (expr->value.op.op2->type == EXPR_CONSTANT) {
	if (expr->value.op.op2->ts.type == BT_INTEGER)
	    use_library =
		integer_exponent(se, expr->value.op.op1,
				 expr->value.op.op2->value.integer);

	else if (expr->value.op.op2->ts.type == BT_REAL) {
	    class = bg_real_type(expr->value.op.op2->value.real);
	    if (class != FF_INFINITY && class != FF_NAN) {
		ff = expr->value.op.op2->value.real->ff;

		p1 = bg_to_bi(expr->value.op.op2->value.real);
		p2 = bg_from_bi(big_copy(p1), ff);

		if (bg_compare_eq(expr->value.op.op2->value.real, p2))
		    use_library = integer_exponent(se, expr->value.op.op1, p1);

		else
		    big_free(p1);
	    }
	}
    }

    if (use_library)
	library_power(se, expr);
}



/* g95_stack_variable()-- Returns nonzero if a variable of the given
 * type should go on the stack.  This is used mainly for arrays.  When
 * the current frame size is exceeded, we start spilling things to the
 * heap. */

int g95_stack_variable(tree type) {
int s;

    s = int_size_in_bytes(type);
    if (s == -1)
	return 0;

    if (g95_context->frame_size + s > g95_option.max_frame_size)
	return 0;

    g95_context->frame_size += s;

    return 1;
}



/* g95_temp_string()-- Generate code to allocate a string temporary.
 * Returns a pointer to the storage area or a character pointer
 * variable. */

tree g95_temp_string(g95_se *se, tree length) {
tree var, type, tmp;

    tmp = g95_build_minus(g95_default_integer, length, integer_one_node);
    type = build_index_type(tmp);
    type = build_array_type(g95_character1_type_node, type);

    if (g95_stack_variable(type)) {
	var = g95_create_var(type);
	TREE_ADDRESSABLE(var) = 1;
	var = g95_addr_expr(var);

    } else {
	var = g95_create_var(pchar_type_node);

	g95_call_temp_alloc(&se->pre, var, length);
	g95_call_temp_free(&se->post, var);
    }

    return var;
}



/* conv_concat_op()-- Handle a string concatenation operation.  A
 * temporary is allocated to hold the result */

static void conv_concat_op(g95_se *se, g95_expr *expr) {
g95_se left, right;
tree var, tmp;

    g95_init_se(&left, se);
    left.reflevel = 1;
    g95_conv_expr(&left, expr->value.op.op1);

    g95_init_se(&right, se);
    right.reflevel = 1;
    g95_conv_expr(&right, expr->value.op.op2);

    se->string_length = g95_build_plus(g95_default_integer, left.string_length,
				       right.string_length);

    se->string_length = save_expr(se->string_length);

    var = g95_temp_string(se, se->string_length);

    tmp = g95_call_library(void_type_node, PREFIX "concat_string", var,
			   left.expr,  left.string_length,
			   right.expr, right.string_length, NULL_TREE);

    g95_add_expr_to_block(&se->pre, tmp);

    se->expr = var;
}



/* unit_length()-- Return nonzero if the length of a string is of unit
 * length. */

static int unit_length(g95_expr *e) {
g95_expr *len;

    if (e->ts.cl == NULL)
	return 0;

    len = e->ts.cl->length;
    if (len == NULL || len->type != EXPR_CONSTANT)
	return 0;

    return (bi_compare(len->value.integer, bi_1) == 0);
}



/* conv_char()-- Convert a single character expression. */

static void conv_char(g95_se *se, g95_expr *expr) {
unsigned char *p;

    g95_conv_expr(se, expr);

    switch(TREE_CODE(TREE_TYPE(se->expr))) {
    case ARRAY_TYPE:
	se->expr = g95_build_array_ref(TREE_TYPE(TREE_TYPE(se->expr)),
				       se->expr, integer_zero_node);
	break;

    case STRING_CST:
	p = (unsigned char *) TREE_STRING_POINTER(se->expr);
	se->expr = g95_build_int(*p, 0);
	break;

    default:
	break;
    }

    se->expr = convert(unsigned_char_type_node, se->expr);
}



/* unit_compare()-- Compare length-1 strings. */

static void unit_compare(g95_se *se, enum tree_code code, g95_expr *expr) {
g95_expr *op1, *op2;
g95_se left, right;
tree cond, type;

    op1 = expr->value.op.op1;
    op2 = expr->value.op.op2;

    g95_init_se(&left, se);
    g95_init_se(&right, se);

    conv_char(&left, op1);
    conv_char(&right, op2);

    if (code != EQ_EXPR && code != NE_EXPR) {
	left.expr = save_expr(left.expr);
	right.expr = save_expr(right.expr);

	cond = g95_build_eq(left.expr, g95_space);

	left.expr = g95_build_cond(g95_default_integer, cond,
				   integer_minus_one_node,
				   convert(g95_default_integer, left.expr));

	cond = g95_build_eq(right.expr, g95_space);

	right.expr = g95_build_cond(g95_default_integer, cond,
				    integer_minus_one_node,
				    convert(g95_default_integer, right.expr));
    }

    type = g95_get_typenode(&expr->ts, 0);
    se->expr = fold(build2(code, type, left.expr, right.expr));
}



/* binary_string()-- Translate most binary string operations. */

static void binary_string(g95_se *se, g95_expr *expr) {
tree tmp, type, left_len, right_len;
g95_expr *op1, *op2;
enum tree_code code;
g95_se left, right;

    switch(expr->value.op.operator) {
    case INTRINSIC_EQ:
	code = EQ_EXPR;
	break;

    case INTRINSIC_NE:
	code = NE_EXPR;
	break;

    case INTRINSIC_GT:
	code = GT_EXPR;
	break;

    case INTRINSIC_GE:
	code = GE_EXPR;
	break;

    case INTRINSIC_LT:
	code = LT_EXPR;
	break;

    case INTRINSIC_LE:
	code = LE_EXPR;
	break;

    default:
	g95_internal_error("binary_string(): Unknown intrinsic op");
    }

    op1 = expr->value.op.op1;
    op2 = expr->value.op.op2;

    if (unit_length(op1) && unit_length(op2)) {
	unit_compare(se, code, expr);
	return;
    }

    g95_init_se(&left,  se);
    g95_init_se(&right, se);

    left.reflevel  = 1;
    right.reflevel = 1;

    g95_conv_expr(&left,  op1);
    g95_conv_expr(&right, op2);

    left_len = (op1->ts.type == BT_CHARACTER)
	? left.string_length
	: g95_ts_size(&op1->ts);

    right_len = (op2->ts.type == BT_CHARACTER)
	? right.string_length
	: g95_ts_size(&op2->ts);

    tmp = g95_call_library(g95_default_integer, PREFIX "compare_string",
			   left.expr,  left_len,
			   right.expr, right_len, NULL_TREE);

    left.expr = tmp;
    right.expr = integer_zero_node;

    type = g95_get_typenode(&expr->ts, 0);
    se->expr = fold(build2(code, type, left.expr, right.expr));
}



/* binary_nonstring()-- Translate a nonstring operator expression. */

static void binary_nonstring(g95_se *se, g95_expr *expr) {
g95_expr *op1, *op2;
enum tree_code code;
g95_se left, right;
tree type;

    switch(expr->value.op.operator) {
    case INTRINSIC_PLUS:
	code = PLUS_EXPR;
	break;

    case INTRINSIC_MINUS:
	code = MINUS_EXPR;
	break;

    case INTRINSIC_TIMES:
	code = MULT_EXPR;
	break;

    case INTRINSIC_DIVIDE:
	/* If the expression is real or complex, use an RDIV_EXPR. If
	 * integer, we must round towards zero, so we use a TRUNC_DIV_EXPR. */

	code = (expr->ts.type == BT_INTEGER) ? TRUNC_DIV_EXPR : RDIV_EXPR;
	break;

    case INTRINSIC_AND:
	code = g95_option.short_circuit ? TRUTH_ANDIF_EXPR : TRUTH_AND_EXPR;
	break;

    case INTRINSIC_OR:
	code = g95_option.short_circuit ? TRUTH_ORIF_EXPR : TRUTH_OR_EXPR;
	break;

    case INTRINSIC_EQ:
	code = EQ_EXPR;
	break;

    case INTRINSIC_EQV:   /* Gets inverted at the end */
    case INTRINSIC_NEQV:
	code = TRUTH_XOR_EXPR;
	break;

    case INTRINSIC_NE:
	code = NE_EXPR;
	break;

    case INTRINSIC_GT:
	code = GT_EXPR;
	break;

    case INTRINSIC_GE:
	code = GE_EXPR;
	break;

    case INTRINSIC_LT:
	code = LT_EXPR;
	break;

    case INTRINSIC_LE:
	code = LE_EXPR;
	break;

    default:
	g95_internal_error("binary_nonstring(): Unknown intrinsic op");
	return;
    }

    op1 = expr->value.op.op1;
    op2 = expr->value.op.op2;

    g95_init_se(&left,  se);
    g95_init_se(&right, se);

    g95_conv_expr(&left,  op1);
    g95_conv_expr(&right, op2);

    if (op1->ts.kind > op2->ts.kind)
	right.expr = convert(TREE_TYPE(left.expr), right.expr);

    if (op1->ts.kind < op2->ts.kind)
	left.expr = convert(TREE_TYPE(right.expr), left.expr);

    type = g95_get_typenode(&expr->ts, 0);
    se->expr = fold(build2(code, type, left.expr, right.expr));

    if (expr->value.op.operator == INTRINSIC_EQV)
	se->expr = fold(build1(TRUTH_NOT_EXPR, type, se->expr));
}



/* conv_expr_op()-- Translate an intrinsic operator expression.  Most
 * binary operators are handled by this function, others are passed
 * on.  Recursion is used in either case.  We are guaranteed that
 * typespecs are the same for the operands of binary operators except
 * exponentiation.  Character strings get special handling.  */

static void conv_expr_op(g95_se *se, g95_expr *expr) {

    switch(expr->value.op.operator) {
    case INTRINSIC_UPLUS:
    case INTRINSIC_PAREN:
	g95_conv_expr(se, expr->value.op.op1);
	return;

    case INTRINSIC_UMINUS:
	conv_unary_op(NEGATE_EXPR, se, expr);
	return;

    case INTRINSIC_NOT:
	conv_unary_op(TRUTH_NOT_EXPR, se, expr);
	return;

    case INTRINSIC_POWER:
	conv_power_op(se, expr);
	return;

    case INTRINSIC_CONCAT:
	conv_concat_op(se, expr);
	return;

    case INTRINSIC_USER:
    case INTRINSIC_ASSIGN:
	g95_internal_error("conv_expr_op(): User operator still exists");
	return;

    default:
	if (expr->value.op.op1->ts.type == BT_CHARACTER ||
	    expr->value.op.op2->ts.type == BT_CHARACTER)
	    binary_string(se, expr);
	else
	    binary_nonstring(se, expr);
    }
}



/* conv_scalar_parameter()-- Convert a scalar parameter argument.  The
 * unusual thing is that INTRINSIC_PAREN expressions must be a copy of
 * the original. */

static void conv_scalar_parameter(g95_se *se, g95_expr *e, int dealloc) {
tree tmp, type, var, size, call;
g95_expr *f;
int ref;

    f = e;
    while(f->type == EXPR_OP && f->value.op.operator == INTRINSIC_PAREN)
	f = f->value.op.op1;

    if (e->type != EXPR_OP || e->value.op.operator != INTRINSIC_PAREN) {
	g95_conv_expr(se, e);

	if ((f->type == EXPR_FUNCTION || dealloc) &&
	    e->ts.type == BT_DERIVED &&
	    e->ts.derived->attr.itype == ITYPE_NONE) {
	    tmp = G95_DTYPE_ALLOCS(e->ts.derived->backend_decl);

	    if (tmp != NULL) {
		tmp = g95_call_library(void_type_node, PREFIX "deep_dealloc",
				       se->expr, tmp, NULL_TREE);

		g95_add_expr_to_block(dealloc ? &se->pre : &se->post, tmp);
	    }
	}

	return;
    }

    ref = se->reflevel;
    if (se->reflevel > 0)
	se->reflevel--;

    g95_conv_expr(se, e);

    if (e->ts.type == BT_CHARACTER) {  /* Characters are easy this time */
	type = g95_get_typenode(&e->ts, 0);
	var = g95_create_var(type);

	tmp = g95_call_library(void_type_node, PREFIX "copy_string",
			       g95_addr_expr(var),
			       se->string_length,
			       g95_addr_expr(se->expr),
			       se->string_length, NULL_TREE);
	g95_add_expr_to_block(&se->pre, tmp);

    } else {
	var = g95_create_var(TREE_TYPE(se->expr));

	if (e->ts.type != BT_DERIVED ||
	    e->ts.derived->attr.itype != ITYPE_NONE ||
	    !g95_allocatable_component(e->ts.derived))

	    g95_add_modify_expr(&se->pre, var, se->expr);

	else {
	    tmp = G95_DTYPE_ALLOCS(e->ts.derived->backend_decl);
	    if (tmp == NULL)
		g95_add_modify_expr(&se->pre, var, se->expr);

	    else {  /* Nasty case of allocatable components */
		if (f->type == EXPR_FUNCTION)
		    g95_add_modify_expr(&se->pre, var, se->expr);

		else {
		    size = g95_ts_size(&e->ts);

		    call = g95_call_library(void_type_node, PREFIX "deep_copy",
					    g95_addr_expr(var),
					    g95_addr_expr(se->expr),
					    size, tmp, NULL_TREE);
		    g95_add_expr_to_block(&se->pre, call);
		}

		call = g95_call_library(void_type_node, PREFIX "deep_dealloc",
					g95_addr_expr(var), tmp, NULL_TREE);

		g95_add_expr_to_block(&se->post, call);
	    }
	}
    }

    if (ref > 0)
	se->expr = g95_addr_expr(var);

    se->reflevel = ref;
}



/* g95_trans_arglist()-- Translate an actual argument list into tree
 * form.  Alternate return elements are ignored. */

tree g95_trans_arglist(g95_actual_arglist *arg, g95_se *se) {
tree arglist, arglist_tail;
g95_se parmse;

    arglist      = NULL_TREE;
    arglist_tail = NULL_TREE;

    for(; arg!=NULL; arg=arg->next) {
	g95_init_se(&parmse, se);

	switch(arg->type) {
	case ARG_ALT_RETURN:
	    continue;

	case ARG_EXPR:
	    if (arg->u.expr == NULL)
		parmse.expr = null_pointer_node;

	    else {
		if (g95_c_ptr(&arg->u.expr->ts))
		    parmse.reflevel = (arg->cb == CB_VALUE) ? 1 : 2;

		else if (arg->pointer)
		    parmse.reflevel = 2;

		else if (arg->cb == CB_VALUE)
		    parmse.reflevel = 0;

		else
		    parmse.reflevel = 1;

		if (arg->intent != INTENT_OUT)
		    parmse.load_pre = 1;

		if (arg->intent != INTENT_IN)
		    parmse.store_post = 1;

		conv_scalar_parameter(&parmse, arg->u.expr, arg->dealloc);
	    }

	    break;

	case ARG_ARRAY_DESC:
	case ARG_ARRAY_ELEMENT:
	case ARG_ARRAY:
	    if (arg->u.expr == NULL)
		parmse.expr = null_pointer_node;

	    else
		g95_array_argument(&parmse, arg);

	    break;
	}

	arglist = g95_chainon_list(arglist, parmse.expr);

	if (arg->u.expr == NULL) {
	    if (arg->missing_arg_type == BT_CHARACTER)
		arglist_tail =
		    g95_chainon_list(arglist_tail, integer_zero_node);

	} else if (arg->u.expr->ts.type == BT_CHARACTER)
	    arglist_tail =
		g95_chainon_list(arglist_tail, parmse.string_length);

	g95_raise_chains(&parmse);
    }

    if (arglist_tail != NULL_TREE)
	arglist = chainon(arglist, arglist_tail);

    return arglist;
}



/* conv_function_call()-- Generate code for a function call.  For
 * functions that return simple types, this is just generating the
 * call and adding it to the current expression.  For more complicated
 * types we create a temporary return value and pass a pointer to it
 * in the argument list.  The function call is then added to the
 * pre-chain and the result is the temporary. */

static void conv_function_call(g95_se *se, g95_expr *expr) {
tree arglist, tmp, t, var, length, pre_args, type, result_type;
int char_flag, result_pointer;
g95_typespec *result_ts;
internal_type itype;
g95_symbol *sym;
g95_se se0;

    arglist = g95_trans_arglist(expr->value.function.actual, se);

    char_flag  = 0;
    pre_args   = NULL_TREE;
    length     = NULL_TREE;
    var        = NULL_TREE;

    if (expr->value.function.pointer == NULL) {
	sym = expr->symbol;

	result_ts = &sym->result->ts;
	result_pointer = sym->result->attr.pointer;
	result_type = g95_result_type(sym);

    } else {
	sym = expr->value.function.pointer->ts.interface;

	if (sym != NULL && sym->result != NULL) {
	    result_ts = &sym->result->ts;
	    result_pointer = sym->result->attr.pointer;
	    result_type = g95_result_type(sym);

	} else {
	    result_ts = &expr->ts;
	    result_pointer = 0;

	    result_type = g95_result_type(expr->value.function.pointer->symbol);
	}
    }

    itype = ITYPE_NONE;

    if (!result_pointer && expr->rank == 0 &&
	result_ts->type == BT_CHARACTER) {

	if (sym->attr.bind && result_ts->cl->length != NULL &&
	    g95_compare_expr_int(result_ts->cl->length, 1) == CMP_EQ)

	    result_type = g95_int1_type_node;

	else if (result_ts->cl != &g95_unknown_charlen &&
		 result_ts->cl->length != NULL &&
		 result_ts->cl->length->type == EXPR_CONSTANT) {

	    /* Fortran 77 f2c compatible calling convention */
	    char_flag = 1;

	    type = g95_get_typenode(result_ts, 1);
	    var = g95_create_var(type);
	    tmp = g95_addr_expr(var);

	    t = build1(DECL_EXPR, TREE_TYPE(var), var);
	    g95_add_expr_to_block(&g95_context->pre, t);

	    pre_args = g95_chainon_list(pre_args, tmp);
	    pre_args = g95_chainon_list(pre_args, result_ts->cl->backend_decl);

	} else {
	    /* Variable length string return.  Update: variable length
	     * strings are required to have an explicit interface. */
	    char_flag = 2;

	    pre_args = g95_chainon_list(pre_args, null_pointer_node);
	    pre_args = g95_chainon_list(pre_args, integer_zero_node);
	}
    }

    /* Character returns for pointer valued or array valued functions
     * cannot be assumed length, so the length is calculated locally. */

    if (result_ts->type == BT_CHARACTER &&
	(result_pointer || expr->rank > 0)) {

	if (expr->rank > 0 || result_ts->cl->length == NULL ||
	    result_ts->cl->length->type == EXPR_CONSTANT)
	    se->string_length = result_ts->cl->backend_decl;

	else
	    char_flag = 3;  /* Scalar variable length pointer */
    }

    if (pre_args != NULL_TREE)
	arglist = chainon(pre_args, arglist);

    if (expr->value.function.pointer == NULL) {
	tmp = sym->backend_decl;
	if (!sym->attr.dummy && sym->attr.flavor != FL_VARIABLE)
	    tmp = g95_addr_expr(tmp);

    } else {
	g95_init_se(&se0, se);
	se0.reflevel = 1;

	g95_conv_expr(&se0, expr->value.function.pointer);
	tmp = se0.expr;
    }

    tmp = build3(CALL_EXPR, result_type, tmp, arglist, NULL);
    TREE_SIDE_EFFECTS(tmp) = 1;

    switch(char_flag) {
    case 0:
	if (var == NULL)
	    se->expr = tmp;

	else {
	    g95_add_expr_to_block(&se->pre, tmp);
	    se->expr = var;
	}

	break;

    case 1:
	g95_add_expr_to_block(&se->pre, tmp);

	se->expr = var;
	se->string_length = result_ts->cl->backend_decl;
	break;

    case 2:
    case 3:
	se->expr = g95_create_var(pchar_type_node);
	g95_add_modify_expr(&se->pre, se->expr, tmp);

	se->string_length = g95_create_var(g95_default_integer);
	g95_add_modify_expr(&se->pre, se->string_length, g95_string_len);

	if (char_flag == 2)
	    g95_call_temp_free(&se->post, se->expr);

	break;
    }
}



/* conv_function_expr()-- Translate a function call. */

static void conv_function_expr(g95_se *se, g95_expr *expr) {

    if (g95_option.trace == TRACE_FRAME)
	g95_set_locus(&se->pre, &expr->where);

    if (expr->value.function.isym)
	g95_conv_intrinsic_function(se, expr);

    else if (expr->value.function.pointer)
	conv_function_call(se, expr);

    else if (expr->symbol->attr.iproc != IPROC_NONE)
	g95_conv_modproc_function(se, expr);

    else
	conv_function_call(se, expr);
}



/* intrinsic_structure()-- Convert an intrinsic structure constant to
 * its internal encoding. */

static void intrinsic_structure(g95_se *se, internal_value value) {
int m;

    switch(value) {
    case IVALUE_C_NULL_FUNPTR:
    case IVALUE_C_NULL_PTR:
	se->expr     = null_pointer_node;
	se->reflevel = 1;
	return;

    case IVALUE_OTHER_VALUE:        m = CLASS_OTHER_VALUE;        break;
    case IVALUE_NEGATIVE_INF:       m = CLASS_NEGATIVE_INF;       break; 
    case IVALUE_NEGATIVE_DENORMAL:  m = CLASS_NEGATIVE_DENORMAL;  break;
    case IVALUE_NEGATIVE_ZERO:      m = CLASS_NEGATIVE_ZERO;      break;
    case IVALUE_NEGATIVE_NORMAL:    m = CLASS_NEGATIVE_NORMAL;    break;
    case IVALUE_POSITIVE_INF:       m = CLASS_POSITIVE_INF;       break;
    case IVALUE_POSITIVE_DENORMAL:  m = CLASS_POSITIVE_DENORMAL;  break;
    case IVALUE_POSITIVE_ZERO:      m = CLASS_POSITIVE_ZERO;      break;
    case IVALUE_POSITIVE_NORMAL:    m = CLASS_POSITIVE_NORMAL;    break;
    case IVALUE_SIGNALING_NAN:      m = CLASS_SIGNALING_NAN;      break;
    case IVALUE_QUIET_NAN:          m = CLASS_QUIET_NAN;          break;

    default:
	g95_internal_error("intrinsic_structure(): bad value");
    }

    se->expr = g95_build_int(m, 0);
}



/* conv_structure()-- Create a constructor for a derived type. */

static void conv_structure(g95_se *se, g95_expr *e) {
tree field, tmp, decl, type;
g95_constructor *cons;
g95_component *comp;
variable_info vinfo;
g95_expr *f;
g95_se se0;

    if (e->value.constructor.ivalue != IVALUE_NONE)
	return intrinsic_structure(se, e->value.constructor.ivalue);

    decl = NULL_TREE;

    cons = e->value.constructor.c;
    comp = e->symbol->components;

    while(cons != NULL) {
	g95_init_se(&se0, se);

	f = cons->expr;

	if (comp->dimension) {
	    if (comp->pointer) {
		if (f->type == EXPR_NULL)
		    se0.expr = g95_null_array(comp->as->rank, 0);

		else if (f->type == EXPR_VARIABLE && f->symbol->attr.target)
		    g95_conv_descriptor(&se0, f, 0);

		else
		    g95_conv_expr(&se0, f); /* Array pointer in constructor */

	    } else {
		g95_component_vinfo(comp, &vinfo);
		vinfo.value = f;

		se0.expr = g95_conv_array_initializer(&vinfo, &se0);
	    }

	} else {
	    if (comp->pointer)
		se0.reflevel = 1;

	    g95_conv_expr(&se0, f);

	    if (f->ts.type == BT_CHARACTER && f->type != EXPR_NULL) {
		if (f->rank == 0 && f->type == EXPR_CONSTANT)
		    se0.expr =
			g95_resize_string_constant(se0.expr,
						   comp->ts.cl->backend_decl);
		else {
		    tmp = se0.expr;
		    type = build_range_type(g95_default_integer,
					    integer_one_node,
					    comp->ts.cl->backend_decl);
		    type = build_array_type(g95_character1_type_node, type);

		    se0.expr = g95_create_var(type);
		    tmp = g95_call_library(void_type_node,
					   PREFIX "copy_string",
					   g95_addr_expr(se0.expr),
					   comp->ts.cl->backend_decl,
					   g95_addr_expr(tmp),
					   se0.string_length, NULL_TREE);
		    g95_add_expr_to_block(&se->pre, tmp);
		}
	    }
	}

	g95_raise_chains(&se0);

	field = comp->backend_decl;
	if (comp->dimension)
	    field = G95_ARRAY_FIELD(field);

	decl = tree_cons(field, se0.expr, decl);

	cons = cons->next;
	comp = comp->next;
    }

    if (e->symbol->backend_decl == NULL)
	g95_get_typenode(&e->ts, 0);

    se->expr = g95_build_constructor(G95_DTYPE_TYPE(e->symbol->backend_decl),
				     nreverse(decl));
}



/* g95_conv_expr()-- Translate an expression. */

void g95_conv_expr(g95_se *se, g95_expr *expr) {

    switch(expr->type) {
    case EXPR_OP:
	conv_expr_op(se, expr);
	break;

    case EXPR_SUBSTRING:
	conv_constant_substring(se, expr);
	break;

    case EXPR_STRUCTURE:
	conv_structure(se, expr);
	break;

    case EXPR_ARRAY:
	g95_internal_error("g95_conv_expr(): Array constructors");
	break;

    case EXPR_FUNCTION:
	conv_function_expr(se, expr);
	break;

    case EXPR_CONSTANT:
	g95_conv_constant(se, expr);
	break;

    case EXPR_VARIABLE:
	conv_variable(se, expr);
	break;

    case EXPR_NULL:
	se->expr = null_pointer_node;
	break;

    case EXPR_PROCEDURE:
	se->expr = expr->symbol->backend_decl;
	break;

    default:
	g95_internal_error("g95_conv_expr(): Bad type");
	break;
    }

    if (!G95_ARRAY(se->expr))
	g95_reflevel(se, se->reflevel);

    g95_raise_chains(se);
}



/* g95_conv_expr0()-- Convert an expression without having to worry
 * about the parent se. */

void g95_conv_expr0(g95_se *se, g95_expr *expr) {
g95_se se0;

    g95_init_se(&se0, se);
    g95_conv_expr(&se0, expr);

    se->expr = se0.expr;
}



/* g95_conv_spec_expr()-- Convert a specification expression.  This
 * can involve computing scalarized expressions. */

void g95_conv_spec_expr(g95_se *se, g95_expr *expr) {
g95_code *c;

    c = g95_scalarize_spec_expr(expr);

    if (c != NULL) {
	g95_add_expr_to_block(&se->pre, g95_trans_code(c));
	g95_free_statements(c);
    }

    g95_conv_expr(se, expr);
}



/* conv_charlen_expr()-- Convert a character length expression. */

static void conv_charlen_expr(g95_se *se, g95_expr *e) {
g95_typespec *ts;
g95_ref *ref;
g95_se se0;
tree t;

    g95_init_se(&se0, se);

    switch(e->type) {
    case EXPR_CONSTANT:
	se->expr = g95_build_int(e->value.character.length, 0);
	break;

    case EXPR_OP:  /* Has to be concat operator */
	conv_charlen_expr(&se0, e->value.op.op1);
	t = se0.expr;

	conv_charlen_expr(&se0, e->value.op.op2);
	se->expr = g95_build_plus(g95_default_integer, t, se0.expr);
	break;

    case EXPR_SUBSTRING:
    case EXPR_FUNCTION:
    evaluate:
	g95_conv_expr(&se0, e);
	se->expr = se0.string_length;
	break;

    case EXPR_ARRAY:
	g95_conv_char_length(&se0, &e->ts);
	se->expr = se0.expr;
	break;

    case EXPR_VARIABLE:
	ts = &e->symbol->ts;

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
	    case REF_COARRAY:
		break;

	    case REF_COMPONENT:
		ts = &ref->u.c.component->ts;
		break;

	    case REF_SUBSTRING:
		if (ref->type == REF_SUBSTRING)
		    goto evaluate;    

		break;
	    }

	se->expr = ts->cl->backend_decl;
	if (se->expr == NULL_TREE) {
	    if (ts->cl->length != NULL)
		g95_conv_expr(se, ts->cl->length);

	    else {
		g95_conv_expr(&se0, e);
		se->expr = se0.string_length;
	    }
	}

	break;

    default:
	g95_internal_error("conv_charlen_expr(): Bad type");
    }

    g95_raise_chains(&se0);
}



/* g95_conv_charlen_expr()-- Calculate the length of a character
 * expression. */

void g95_conv_charlen_expr(g95_se *se, g95_expr *e) {

    conv_charlen_expr(se, e);

    se->expr = g95_build_max(g95_default_integer, se->expr, integer_zero_node);
}



/* g95_conv_char_length()-- Convert a character length parameter.
 * This is special in that negative lengths must be clamped to
 * zero.  The length must not be assumed. */

tree g95_conv_char_length(g95_se *se, g95_typespec *ts) {
tree length, tmp;
g95_se se0;

    g95_init_se(&se0, se);

    g95_conv_spec_expr(&se0, ts->cl->length);

    length = save_expr(se0.expr);

    tmp = g95_build_lt(length, integer_zero_node);
    return g95_build_cond(g95_default_integer, tmp, integer_zero_node, length);
}



/* g95_conv_expr_type()-- Convert an expression to a particular
 * destination type. */

void g95_conv_expr_type(g95_se *se, g95_expr *expr, tree type) {

    g95_conv_expr(se, expr);
    se->expr = convert(type, se->expr);
}



/* g95_trans_pointer_assign()-- Translate a pointer assignment */

tree g95_trans_pointer_assign(g95_code *code) {
g95_se left, right;
stmtblock_t block;

    g95_init_block(&block);
    g95_init_se(&left, NULL);

    if (code->expr->rank > 0) {
	if (code->expr2->type != EXPR_NULL)
	    g95_pointer_array_assignment(code, &block);

	else {
	    left.reflevel = 1;
	    g95_conv_expr(&left, code->expr);

	    g95_add_block_to_block(&block, &left.pre);
	    g95_nullify_array_pointer(&block, left.expr);
	    g95_add_block_to_block(&block, &left.post);
	}

    } else {
	left.reflevel = 1;
	g95_conv_expr(&left, code->expr);

	g95_init_se(&right, NULL);
	right.reflevel = 1;
	g95_conv_expr(&right, code->expr2);

	g95_add_block_to_block(&block, &left.pre);
	g95_add_block_to_block(&block, &right.pre);

	g95_add_modify_expr(&block, left.expr, right.expr);

	g95_add_block_to_block(&block, &left.post);
	g95_add_block_to_block(&block, &right.post);
    }

    return g95_finish_block(&block);
}



/* g95_trans_assignment()-- Translate an assignment statement */

tree g95_trans_assignment(g95_expr *expr1, g95_expr *expr2) {
stmtblock_t block;
g95_charlen *cl;
g95_symbol *sym;
g95_se lse, rse;
tree var, tmp;

    if ((expr1->rank > 0 || expr2->rank > 0) && expr2->type != EXPR_FUNCTION)
	g95_internal_error("Unexpected array assignment statement at %L",
			   &expr1->where);

    g95_init_block(&block);

    g95_init_se(&lse, NULL);
    g95_init_se(&rse, NULL);

    if (expr1->ts.type == BT_CHARACTER || expr1->rank > 0) {
	lse.reflevel = 1;
	rse.reflevel = 1;
    }

    if (g95_coindexed_expr(expr1) && g95_coindexed_expr(expr2)) {
	g95_conv_expr(&rse, expr2);

	lse.store_post = 1;
	lse.active_var = rse.active_var;

	g95_conv_expr(&lse, expr1);

	g95_add_block_to_block(&block, &rse.pre);
	g95_add_block_to_block(&block, &lse.pre);

	g95_add_block_to_block(&block, &rse.post);
	g95_add_block_to_block(&block, &lse.post);

    } else if (g95_coindexed_expr(expr1)) {
	g95_conv_expr(&rse, expr2);

	lse.active_var = rse.expr;
	lse.store_post = 1;

	g95_conv_expr(&lse, expr1);

	g95_add_block_to_block(&block, &rse.pre);
	g95_add_block_to_block(&block, &lse.pre);

	g95_add_block_to_block(&block, &rse.post);
	g95_add_block_to_block(&block, &lse.post);

    } else if (g95_coindexed_expr(expr2)) {
	lse.reflevel = 1;
       	g95_conv_expr(&lse, expr1);

	rse.active_var = lse.expr;
	rse.load_pre = 1;

	g95_conv_expr(&rse, expr2);

	g95_add_block_to_block(&block, &lse.pre);
	g95_add_block_to_block(&block, &rse.pre);

	g95_add_block_to_block(&block, &lse.post);
	g95_add_block_to_block(&block, &rse.post);

    } else if (expr1->ts.type == BT_DERIVED &&
	       expr1->ts.derived->attr.itype != ITYPE_NONE) {

	if (g95_c_ptr(&expr1->ts)) {
	    lse.reflevel = 1;
	    rse.reflevel = 1;
	}

	g95_conv_expr(&lse, expr1);
	g95_add_block_to_block(&block, &lse.pre);

	g95_conv_expr(&rse, expr2);
	g95_add_block_to_block(&block, &rse.pre);

	g95_add_modify_expr(&block, lse.expr, rse.expr);

    } else if (expr1->ts.type == BT_DERIVED &&
	       G95_DTYPE_ALLOCS(expr1->ts.derived->backend_decl) != NULL_TREE){

	if (expr2->type == EXPR_FUNCTION) {
	    /* For functions on the RHS, just copy the results.
	     * Otherwise we'd need to do a deep copy followed by a
	     * deep dealloc of the original results. */

	    lse.reflevel = 1;
	    g95_conv_expr(&lse, expr1);
	    g95_add_block_to_block(&block, &lse.pre);
	    lse.expr = save_expr(lse.expr);

	    g95_conv_expr(&rse, expr2);
	    g95_add_block_to_block(&block, &rse.pre);

	    var = g95_create_var(TREE_TYPE(rse.expr));
	    g95_add_modify_expr(&block, var, rse.expr);

	    tmp = G95_DTYPE_ALLOCS(expr1->ts.derived->backend_decl);
	    tmp = g95_call_library(void_type_node, PREFIX "deep_dealloc",
				   lse.expr, tmp, NULL_TREE);
	    g95_add_expr_to_block(&block, tmp);

	    lse.expr = g95_build_indirect(TREE_TYPE(TREE_TYPE(lse.expr)), lse.expr);
	    g95_add_modify_expr(&block, lse.expr, var);

	} else {
	    lse.reflevel = 1;
	    g95_conv_expr(&lse, expr1);
	    g95_add_block_to_block(&block, &lse.pre);
	    lse.expr = save_expr(lse.expr);

	    rse.reflevel = 1;
	    g95_conv_expr(&rse, expr2);
	    g95_add_block_to_block(&block, &rse.pre);

	    var = G95_DTYPE_ALLOCS(expr1->ts.derived->backend_decl);
	    tmp = g95_call_library(void_type_node, PREFIX "deep_dealloc",
				   lse.expr, var, NULL_TREE);
	    g95_add_expr_to_block(&block, tmp);


	    tmp = g95_ts_size(&expr1->ts);
	    tmp = g95_call_library(void_type_node, PREFIX "deep_copy",
				   lse.expr, rse.expr, tmp, var, NULL_TREE);

	    g95_add_expr_to_block(&block, tmp);
	}

    } else if (expr1->ts.type != BT_CHARACTER || expr1->rank > 0) {
	g95_conv_expr(&lse, expr1);
	g95_add_block_to_block(&block, &lse.pre);

	g95_conv_expr(&rse, expr2);
	g95_add_block_to_block(&block, &rse.pre);

	g95_add_modify_expr(&block, lse.expr, rse.expr);

    } else {
	g95_conv_expr(&rse, expr2);
	g95_add_block_to_block(&block, &rse.pre);

	sym = expr1->symbol;

	if (sym->attr.artificial && expr1->ref == NULL &&
	    sym->ts.cl == &g95_unknown_charlen) {

	    var = g95_create_var(g95_default_integer);
	    g95_add_modify_expr(&block, var, rse.string_length);
	    rse.string_length = var;

	    cl = g95_get_charlen(NULL);
	    cl->backend_decl = var;
	    sym->ts.cl = cl;

	    g95_call_temp_alloc(&block, sym->backend_decl, rse.string_length);
	    lse.expr = sym->backend_decl;
	    lse.string_length = var;

	} else
	    g95_conv_expr(&lse, expr1);

	g95_add_block_to_block(&block, &lse.pre);

	tmp = g95_call_library(void_type_node, PREFIX "copy_string",
			       lse.expr, lse.string_length,
			       rse.expr, rse.string_length, NULL_TREE);

	g95_add_expr_to_block(&block, tmp);
    }

    g95_add_block_to_block(&block, &lse.post);
    g95_add_block_to_block(&block, &rse.post);

    return g95_finish_block(&block);
}

