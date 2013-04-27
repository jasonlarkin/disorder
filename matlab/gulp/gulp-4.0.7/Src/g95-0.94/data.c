
/* DATA statement translation
   Copyright (C) 2003-2008 Free Software Foundation, Inc.
   Contributed by Andy Vaught.

This file is part of g95.

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

#include "trans.h"
#include <string.h>

/* Variables pointing to the current data constant and repeat count. */


typedef struct {
    bignum start, end, step, extent, value, trip, count;
    g95_expr *vector;
    g95_array_ref *vector_ref;
    int vector_index;
} dimen;

static try expand_data_var(g95_data_variable *);

typedef struct init_tree {
    BBT_HEADER(init_tree)

    int start, end, hard;
    tree head, tail;
} init_tree;


typedef struct data_t {
    init_tree *root;

    tree ac_string_length, array_list;
    int ac_count, initial_offset, repeat_count, error_flag;
    g95_data_value *current_value;

    g95_locus current_data;
    g95_symbol *current_symbol;

    bignum current_offset, string_length;

    g95_array_spec *as_save;
    g95_array_ref *ar_save;

    struct data_t *previous;
} data_t;

static data_t dinfo;
static int last_char_flag, last_right;
static tree init_type, init_value;
static g95_se *saved_se;


#define get_init_tree() g95_getmem(sizeof(init_tree))

typedef enum { OVERLAP_NONE, OVERLAP_FOUND, OVERLAP_OK } overlap_t;




/* push_data()-- Push a data node */

static void push_data(void) {
data_t *p;

    p = g95_getmem(sizeof(dinfo)); 
    *p = dinfo;

    memset(&dinfo, '\0', sizeof(dinfo));
    dinfo.previous = p;
}



/* pop_data()-- Pop a data node */

static void pop_data(void) {
data_t *p;

    p = dinfo.previous;
    dinfo = *p;

    g95_free(p);
}



/* compare_init_tree()-- Compare two init_tree structures.  The
 * structures are assumed not to overlap. */

static int compare_init_tree(void *a, void *b) {
init_tree *p, *q;

    p = (init_tree *) a;
    q = (init_tree *) b;

    if (p->start < q->start) return -1;
    if (p->start > q->start) return 1;

    return 0;
}



/* check_overlap()-- Make sure that this init_tree structure does not
 * overlap with any other structure in the tree.  Returns one if there
 * is a conflict, two if there is no conflict and the node should be
 * inserted or three if there is no conflict but the node should not
 * be inserted. */

static overlap_t check_overlap(init_tree *new, init_tree *current) {
overlap_t m;

    if (current == NULL)
	return OVERLAP_NONE;

    if (new->start == current->start) {
	if (new->hard == 0 && current->hard == 0 &&
	    new->end == current->end &&
	    simple_cst_equal(new->head, current->head) == 1)
	    return OVERLAP_OK;

	return OVERLAP_FOUND;
    }

    if (new->end < current->start)
	return check_overlap(new, current->left);

    if (new->start > current->end)
	return check_overlap(new, current->right);

    if ((new->start <= current->start && current->start < new->end) ||
	(new->start <  current->end   && current->end   < new->end) ||
	(current->start <= new->start && new->start < current->end) ||
	(current->start <  new->end   && new->end   < current->end)) {

	return new->hard
	    ? OVERLAP_FOUND
	    : (current->hard ? OVERLAP_OK : OVERLAP_FOUND);
    }

    m = check_overlap(new, current->left);
    if (m != OVERLAP_NONE)
	return m;

    return check_overlap(new, current->right);
}



/* free_init_tree()-- Recursively free the entire tree */

static void free_init_tree(init_tree *p) {

    if (p == NULL)
	return;

    free_init_tree(p->left);
    free_init_tree(p->right);

    g95_free(p);
}



/* find_start()-- Recursively find a node with a particular start
 * value.  Returns NULL if no such node exists. */

static init_tree *find_start(init_tree *r, int start) {

    if (r == NULL)
	return NULL;

    if (r->start == start)
	return r;

    return find_start(r->start < start ? r->right : r->left, start);
}



/* find_end()-- Recursively find a node with a particular end value.
 * Returns NULL if no such node exists. */

static init_tree *find_end(init_tree *r, int end) {

    if (r == NULL)
	return NULL;

    if (r->end == end)
	return r;

    return find_end(r->end < end ? r->right : r->left, end);
}



/* init_tree_type()-- Return the type of an init_tree, examining the
 * innards of a list node if necessary. */

static tree init_tree_type(tree decl) {

    if (TREE_CODE(decl) == TREE_LIST)
	decl = TREE_VALUE(decl);

    return TREE_TYPE(decl);
}



/* join_nodes()-- Given a position in the tree, find the nodes on the
 * left and right and concatenate them into a single constructor list
 * if they are of the same type.  Returns nonzero if the tree was
 * modified. */

static int join_nodes(int n) {
init_tree *left, *right;

    left  = find_end(dinfo.root, n);
    right = find_start(dinfo.root, n);

    if (left == NULL || right == NULL ||
	init_tree_type(left->head) != init_tree_type(right->head))
	return 0;

    /* Join the nodes */

    if (TREE_CODE(left->head) != TREE_LIST) {
	left->head = g95_chainon_list(NULL_TREE, left->head);
	left->tail = left->head;
    }

    if (TREE_CODE(right->head) != TREE_LIST) {
	right->head = g95_chainon_list(NULL_TREE, right->head);
	right->tail = right->head;
    }

    TREE_CHAIN(left->tail) = right->head;
    left->tail = right->tail;

    left->end = right->end;

    g95_delete_bbt(&dinfo.root, right, compare_init_tree);
    right->left = right->right = NULL;

    free_init_tree(right);

    return 1;
}



/* find_soft_join()-- Search the tree for a soft node, change it to a
 * hard node and try to join it.  Returns nonzero if we found a soft
 * node, zero otherwise. */

static int find_soft_join(init_tree *p) {

    if (p == NULL)
	return 0;

    if (p->hard == 0) {
	p->hard = 1;
	if (join_nodes(p->end))
	    return 1;

	/* Join failed, keep going */
    }

    return find_soft_join(p->left) || find_soft_join(p->right);
}



/* next_value()-- Return the next value in the value list, taking the
 * repeat count into account.  Returns NULL if we are out of data
 * items. */

static tree next_value(g95_expr **last_expr, g95_typespec *ts) {
static g95_typespec *last_ts = NULL;
static tree value;
g95_expr *e;
g95_se se;

    if (dinfo.current_value == NULL) {
	g95_error("Not enough values in DATA statement at %L",
		  &dinfo.current_data);
	return NULL;
    }

    *last_expr = dinfo.current_value->expr;

    if (dinfo.repeat_count == 0 || ts != last_ts) {
	g95_init_se(&se, NULL);

	e = g95_assign_boz(ts, dinfo.current_value->expr);
	if (e == NULL)
	    g95_conv_constant(&se, dinfo.current_value->expr);

	else {
	    g95_conv_constant(&se, e);
	    g95_free_expr(e);
	}

	value = se.expr;
	last_ts = ts;
    }

    if (++dinfo.repeat_count >= dinfo.current_value->repeat) {
	dinfo.current_value = dinfo.current_value->next;
	dinfo.repeat_count = 0;
    }

    return value;
}



/* type_check()-- Given a value and a type, make sure the value can be
 * assigned correctly.  Converts types and in the case of strings,
 * lengths.  Return NULL if something goes wrong. */

static tree type_check(tree decl, g95_expr *e, g95_typespec *ts) {
g95_typespec value_ts;
tree type, len;
g95_expr *f;
g95_se se;

    value_ts = e->ts;
    type = g95_get_typenode(ts, 0);

    if (e->ts.type == BT_CHARACTER && e->value.character.hollerith &&
	ts->type != BT_CHARACTER) {
	f = g95_convert_hollerith(e, ts);

	if (f != NULL) {
	    g95_init_se(&se, NULL);
	    g95_conv_constant(&se, f);
	    decl = se.expr;
	    value_ts = *ts;
	    g95_free_expr(f);
	}
    }

    switch(ts->type) {
    case BT_CHARACTER:
	if (value_ts.type != BT_CHARACTER) {
	    g95_error("Character constant required at %L", &e->where);
	    return NULL_TREE;
	}

	len = bi_to_tree(big_copy(dinfo.string_length), -1);
	decl = g95_resize_string_constant(decl, len);
	break;

    case BT_DERIVED:
	if (value_ts.type != BT_DERIVED || value_ts.derived != ts->derived) {
	    g95_error("Structure constructor '%s' required at %L",
		      ts->derived->name, &e->where);
	    return NULL_TREE;
	}

	break;

    case BT_LOGICAL:
	if (value_ts.type != BT_LOGICAL) {
	    g95_error("Logical constant required at %L", &e->where);
	    return NULL_TREE;
	}

	decl = fold(convert(type, decl));
	break;

    case BT_REAL:
    case BT_INTEGER:
    case BT_COMPLEX:
	if (value_ts.type != BT_REAL && value_ts.type != BT_INTEGER &&
	    value_ts.type != BT_COMPLEX) {
	    g95_error("Numeric constant required at %L", &e->where);
	    return NULL_TREE;
	}

	decl = fold(convert(type, decl));
	break;

    default:
	g95_internal_error("type_check(): Bad type");
    }

    return decl;
}



/* add_value()-- Given an initialization value, insert it at the
 * current offset. */

static try add_value(tree value, int hard) {
int start, end;
init_tree *p;
try t;

    p = get_init_tree();
    p->head = value;

    p->start = start = bi_to_int(big_copy(dinfo.current_offset));
    p->end = end = start + int_size_in_bytes(TREE_TYPE(value));

    p->hard = hard;
    t = SUCCESS;

    switch(check_overlap(p, dinfo.root)) {
    case OVERLAP_FOUND:
	g95_error("Memory is initialized more than once (offset %s) at %L",
		  bi_to_string(big_copy(dinfo.current_offset)),
		  &dinfo.current_data);

	free_init_tree(p);
	t = FAILURE;
	break;

    case OVERLAP_NONE:
	g95_insert_bbt(&dinfo.root, p, compare_init_tree);

	if (hard) {
	    join_nodes(start);
	    join_nodes(end);
	}

	break;

    case OVERLAP_OK:
	free_init_tree(p);
	break;
    }

    return t;
}



/* add_element()-- Given a tree node and a starting offset, insert the
 * node into the tree. */

static try add_element(g95_symbol *base, g95_typespec *ts, int pointer) {
g95_expr *e;
tree value;

    value = next_value(&e, ts);
    if (value == NULL_TREE)
	return FAILURE;

    if (base != dinfo.current_symbol)
	return SUCCESS;

    if (pointer) {
	if (value != null_pointer_node) {
	    g95_error("NULL() required for pointer initialization at %L",
		      &e->where);
	    value = NULL_TREE;
	}

    } else
	value = type_check(value, e, ts);

    if (value == NULL_TREE)
	return FAILURE;

    return add_value(value, 1);
}



/* init_dimen()-- Initialize a dimen structure. */

static void init_dimen(dimen *d) {

    d->start  = bi_0;
    d->end    = bi_0;
    d->step   = bi_0;
    d->extent = bi_0;
    d->value  = bi_0;
    d->trip   = bi_0;
    d->count  = bi_0;

    d->vector = NULL;
    d->vector_ref = NULL;
    d->vector_index = 0;
}



/* clear_dimen()-- Clear a dimen structure. */

static void clear_dimen(dimen *d) {

    big_free(d->start);
    big_free(d->end);
    big_free(d->step);
    big_free(d->extent);
    big_free(d->value);
    big_free(d->trip);
    big_free(d->count);

    if (d->vector != NULL)
	g95_free_expr(d->vector);
}


/* vss_dimen()-- For a vector subscript in an array reference, drill
 * down until we find the true array range, full or partial. */

static try vss_dimen(g95_expr *e, dimen *d) {
g95_array_spec *as;
g95_ref *ref;
int i;

    if (e->type != EXPR_VARIABLE) {
	g95_error("vss_dimen(): Variable expr not found!");
	return FAILURE;
    }

    as = e->symbol->as;
    e->rank = 0;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type == AR_FULL || ref->u.ar.type == AR_SECTION)
		goto section;

	    as = NULL;
	    break;

	case REF_COMPONENT:
	    if (ref->u.c.component->dimension)
		as = ref->u.c.component->as;

	    break;
      
	case REF_SUBSTRING:
	case REF_COARRAY:
	    g95_internal_error("vss_dimen(): Unexpected ref");
	    break;
	}

    g95_internal_error("vss_dimen(): Section not found");

section:
    d->vector_ref = &ref->u.ar;

    if (ref->u.ar.type == AR_FULL) {
	ref->u.ar.dimen = 1;
	ref->u.ar.type = AR_ELEMENT;
	d->vector_index = 0;

	d->start  = as->lower[0]->value.integer;
	d->end    = as->upper[0]->value.integer;
	d->step   = bi_1;

	return SUCCESS;
    }

    ref->u.ar.type = AR_ELEMENT;

    for(i=0; i<ref->u.ar.dimen; i++)
	switch(ref->u.ar.dimen_type[i]) {
	case DIMEN_ELEMENT:
	    break;

	case DIMEN_RANGE:
	    goto found_range;

	case DIMEN_VECTOR:
	    return vss_dimen(ref->u.ar.start[i], d);

	default:
	    g95_internal_error("vss_dimen(): Bad dimen");
	}

    g95_internal_error("vss_dimen(): Missing section");

found_range:
    d->vector_index = i;
    if (ref->u.ar.start[i] == NULL)
	d->start = as->lower[i]->value.integer;

    else {
	if (ref->u.ar.start[i]->type != EXPR_CONSTANT) {
	    g95_error("Array range start in DATA is not constant at %L",
		      &ref->u.ar.start[i]->where);
	    return FAILURE;
	}

	d->start = ref->u.ar.start[i]->value.integer;
    }

    if (ref->u.ar.end[i] == NULL)
	d->end = as->upper[i]->value.integer;

    else {
	if (ref->u.ar.end[i]->type != EXPR_CONSTANT) {
	    g95_error("Array range end in DATA is not constant at %L",
		      &ref->u.ar.end[i]->where);
	    return FAILURE;
	}

	d->end = ref->u.ar.end[i]->value.integer;
    }

    if (ref->u.ar.stride[i] == NULL)
	d->step = bi_1;

    else {
	if (ref->u.ar.stride[i]->type != EXPR_CONSTANT) {
	    g95_error("Array range stride in DATA is not constant at %L",
		      &ref->u.ar.stride[i]->where);
	    return FAILURE;
	}

	d->step = ref->u.ar.stride[i]->value.integer;
    }

    return SUCCESS;
}



/* evaluate_vss()-- Evaluate a vector subscript.  Returns NULL on
 * error, or an array index number if successful. */

static bignum evaluate_vss(dimen *d) {
g95_array_ref *ar;
g95_expr *c, *v;
bignum value;

    v = g95_constant_result(BT_INTEGER, -1, &d->vector->where);
    v->value.integer = big_clone(big_copy(d->value));
    big_permanent(v->value.integer);

    ar = d->vector_ref;
    ar->start[d->vector_index] = v;
    ar->dimen_type[d->vector_index] = DIMEN_ELEMENT;

    c = g95_copy_expr(d->vector);

    if (g95_simplify_init_expr(c) == FAILURE)
	return NULL;

    if (c->type != EXPR_CONSTANT)
	g95_internal_error("Vector subscript didn't reduce at %L",
			   &d->vector->where);

    value = big_clone(c->value.integer);
    g95_free_expr(c);

    g95_free_expr(ar->start[d->vector_index]);
    ar->start[d->vector_index] = NULL;

    return value;
}



/* init_array_loop()-- Initialize each dimension of an array section.
 * Dimensions that are single elements are replaced with an equivalent
 * single-trip loop. */

static try init_array_loop(int n, dimen *d) {

    if (dinfo.ar_save->type == AR_FULL) {
	d->start = dinfo.as_save->lower[n]->value.integer;
	d->end   = dinfo.as_save->upper[n]->value.integer;
	d->step  = bi_1;

	return SUCCESS;
    }

    switch(dinfo.ar_save->dimen_type[n]) {
    case DIMEN_ELEMENT:
	if (dinfo.ar_save->start[n]->type != EXPR_CONSTANT) {
	    g95_error("Array element in DATA not constant at %L",
		      &dinfo.ar_save->start[n]->where);
	    return FAILURE;
	}

	d->start = dinfo.ar_save->start[n]->value.integer;
	d->end   = d->start;
	d->step  = bi_1;

	break;

    case DIMEN_RANGE:
	if (dinfo.ar_save->start[n] == NULL)
	    d->start = dinfo.as_save->lower[n]->value.integer;
	else {
	    if (dinfo.ar_save->start[n]->type != EXPR_CONSTANT) {
		g95_error("Array range start in DATA not constant at %L",
			  &dinfo.ar_save->start[n]->where);
		return FAILURE;
	    }

	    d->start = dinfo.ar_save->start[n]->value.integer;
	}

	if (dinfo.ar_save->end[n] == NULL)
	    d->end = dinfo.as_save->upper[n]->value.integer;
	else {
	    if (dinfo.ar_save->end[n]->type != EXPR_CONSTANT) {
		g95_error("Array range end in DATA not constant at %L",
			  &dinfo.ar_save->end[n]->where);
		return FAILURE;
	    }

	    d->end = dinfo.ar_save->end[n]->value.integer;
	}

	if (dinfo.ar_save->stride[n] == NULL) {
	    d->step = bi_1;
	} else {
	    if (dinfo.ar_save->stride[n]->type != EXPR_CONSTANT) {
		g95_error("Array range step in DATA not constant at %L",
			  &dinfo.ar_save->stride[n]->where);
		return FAILURE;
	    }

	    d->step = dinfo.ar_save->stride[n]->value.integer;
	}

	break;

    case DIMEN_VECTOR:
	d->vector = g95_copy_expr(dinfo.ar_save->start[n]);
	return vss_dimen(d->vector, d);

    case DIMEN_UNKNOWN:
	g95_internal_error("init_dimen(): bad dimension type");
    }

    return SUCCESS;
}


/* expand_section()-- Expand an array section. */

static try expand_section(g95_symbol *base, g95_typespec *ts) {
dimen *d, index[G95_MAX_DIMENSIONS];
int n, rank, element_size;
bignum m, offset0;
try t; 

    rank = dinfo.as_save->rank;
    t = FAILURE;
    offset0 = bi_0;

    element_size = g95_int_ts_size(ts);

    for(n=0; n<rank; n++)
	init_dimen(&index[n]);

    for(n=0; n<rank; n++) {
	d = &index[n];
	if (init_array_loop(n, d) == FAILURE)
	    goto cleanup;

	/* Calculate the derived quantities */

	d->extent = bi_subtract(dinfo.as_save->upper[n]->value.integer,
				dinfo.as_save->lower[n]->value.integer);

	d->extent = bi_int_add(d->extent, 1);

	if (bi_is_negative(big_copy(d->extent))) {
	    big_free(d->extent);
	    d->extent = bi_0;
	}

	d->trip = bi_subtract(big_copy(d->end), big_copy(d->start));
	d->trip = bi_add(d->trip, big_copy(d->step));
	d->trip = bi_divide(d->trip, big_copy(d->step));
		     
	d->value = big_copy(d->start);
    }

/* See if there is nothing to do */

    for(n=0; n<rank; n++)
	if (bi_compare(big_copy(index[n].trip), bi_0) <= 0) {
	    t = SUCCESS;
	    goto cleanup;
	}

/* Now start looping over the whole mess */

    for(;;) {
	big_free(offset0);
	offset0 = bi_0;      /* Calculate the offset of the current entry */

	for(n=rank-1; n>=0; n--) {
	    offset0 = bi_multiply(offset0, big_copy(index[n].extent));

	    m = (index[n].vector == NULL)
		? big_copy(index[n].value)
		: evaluate_vss(&index[n]);

	    if (m == NULL)
		return FAILURE;

	    offset0 = bi_add(offset0, m);
	    offset0 = bi_subtract(offset0,
				  dinfo.as_save->lower[n]->value.integer);
	}

	offset0 = bi_multiply(offset0, int_to_bi(element_size));

	dinfo.current_offset = bi_add(dinfo.current_offset, big_copy(offset0));

	if (add_element(base, ts, 0) == FAILURE)
	    goto cleanup;

	dinfo.current_offset =
	    bi_subtract(dinfo.current_offset, big_copy(offset0));

	/* Calculate the next loop index */

	n = 0;
	for(;;) {
	    index[n].value = bi_add(index[n].value, big_copy(index[n].step));
	    index[n].count = bi_int_add(index[n].count, 1);

	    if (bi_compare(big_copy(index[n].count),
			   big_copy(index[n].trip)) < 0)
		break;

	    big_free(index[n].count);
	    index[n].count = bi_0;

	    big_free(index[n].value);
	    index[n].value = big_copy(index[n].start);

	    if (++n >= rank) {
		t = SUCCESS;
		goto cleanup;
	    }
	}
    }

cleanup:
    for(n=0; n<rank; n++)
	clear_dimen(&index[n]);

    big_free(offset0);
    return t;
}



/* expand_array_ref()-- Accumulate the offset of an array element into
 * the current offset.  Saves array sections to be expanded later.
 * Returns FAILURE if something goes wrong. */

static try expand_array_ref(g95_ref *ref, g95_array_spec *as,
			    g95_typespec *ts) {
int i, section, element_size;
bignum acc;

    element_size = g95_int_ts_size(ts);

    if (as == NULL) {
	g95_error("Array reference follows scalar object at %L", &ref->where);
	return FAILURE;
    }

    section = (ref->u.ar.type == AR_FULL || ref->u.ar.type == AR_SECTION);

    if (!section)
	for(i=0; i<as->rank; i++)
	    if (ref->u.ar.dimen_type[i] == DIMEN_RANGE)
		section = 1;

    /* For an element, calculate the offset.  Save a section for later
     * expansion. */

    if (section) {
	if (dinfo.as_save != NULL) {
	    g95_error("Multiple array references in expression at %L",
		      &ref->where);
	    return FAILURE;
	}

	dinfo.ar_save = &ref->u.ar;
	dinfo.as_save = as;

    } else {
	i = g95_element_number(&ref->u.ar, as);
	if (i < 0) {
	    g95_error("Array reference at %L is outside of array",
		      &ref->where);
	    return FAILURE;
	}

	acc = bi_multiply(int_to_bi(element_size), int_to_bi(i));

	dinfo.current_offset = bi_add(dinfo.current_offset, acc);
    }

    return SUCCESS;
}



/* expand_substring_ref()-- Accumulate offset from a substring reference.
 * Also sets string_length to the correct value */

static try expand_substring_ref(g95_ref *ref, g95_typespec *ts) {
bignum start, end;
g95_expr *e;
try t;

    t = FAILURE;

    start = bi_0;
    end = bi_0;

    e = ref->u.ss.start;
    if (e == NULL)
	start = bi_1;

    else {
	if (e->type != EXPR_CONSTANT) {
	    g95_error("Substring start value at %L must be constant",
		      &e->where);
	    goto done;
	}

	start = e->value.integer;
    }

    e = ref->u.ss.end;
    if (e == NULL)
	e = ts->cl->length;

    if (e->type != EXPR_CONSTANT) {
	g95_error("Substring end value at %L must be constant", &e->where);
	goto done;
    }

    end = e->value.integer;

    dinfo.current_offset =
	bi_int_add(bi_add(dinfo.current_offset, big_copy(start)), -1);

    big_free(dinfo.string_length);
    dinfo.string_length =
	bi_int_add(bi_subtract(big_copy(end), big_copy(start)), 1);

    if (bi_is_negative(big_copy(dinfo.string_length))) {
	big_free(dinfo.string_length);
	dinfo.string_length = bi_0;
    }

    t = SUCCESS;

done:
    big_free(start);
    big_free(end);

    return t;
}



/* expand_data_var0()-- At this point we have an expression node that is
 * to accept the next data value.  This can be a scalar variable or a
 * an array variable (full or section).  If it is an array variable,
 * we have to loop over the relevant section. */

static try expand_data_var0(g95_expr *e) {
int substring, pointer;
g95_array_spec *as;
g95_typespec *ts;
g95_component *c;
g95_ref *ref;
g95_expr *s;
tree field;
try t;

    t = FAILURE;
    pointer = g95_pointer_expr(e);

    s = g95_copy_expr(e);
    if (g95_simplify_expr(s) == FAILURE)
	return FAILURE;

    big_free(dinfo.current_offset);
    dinfo.current_offset = int_to_bi(dinfo.initial_offset);

    dinfo.as_save = NULL;
    dinfo.ar_save = NULL;
    dinfo.current_data = e->where;

    as = s->symbol->as;
    ts = &s->symbol->ts;
    substring = 0;

    for(ref=s->ref; ref; ref=ref->next) {
	substring = 0;

	switch(ref->type) {
	case REF_ARRAY:
	    if (expand_array_ref(ref, as, ts) == FAILURE)
		goto done;

	    as = NULL;
	    break;

	case REF_COMPONENT:
	    c = ref->u.c.component;
	    field = c->backend_decl;
	    if (c->dimension)
		field = G95_ARRAY_FIELD(field);

	    dinfo.current_offset =
		bi_int_add(dinfo.current_offset, int_byte_position(field));

	    ts = &ref->u.c.component->ts;
	    as = ref->u.c.component->as;
	    break;

	case REF_SUBSTRING:
	    if (expand_substring_ref(ref, ts) == FAILURE)
		goto done;

	    substring = 1;
	    as = NULL;
	    break;

	default:
	    g95_internal_error("expand_data_var0(): Bad ref");
	}
    }

    if (ts->type == BT_CHARACTER && !substring) {
	e = ts->cl->length;
	if (e->type != EXPR_CONSTANT) {
	    g95_error("String length at %L must be constant", &e->where);
	    goto done;
	}

	big_free(dinfo.string_length);
	dinfo.string_length = e->value.integer;
    }

    t = (dinfo.as_save == NULL) 
	? add_element(s->symbol, ts, pointer)
	: expand_section(s->symbol, ts);

done:
    g95_free_expr(s);
    return t;
}



/* expand_list()-- Expand a list of data items for one set of
 * iteration values.  Mutually recursive with expand_data_var(). */

static try expand_list(g95_data_variable *list) {

    for(; list!=NULL; list=list->next)
	if (expand_data_var(list) == FAILURE)
	    return FAILURE;

    return SUCCESS;
}



/* expand_data_var()-- Expand a single data variable node.  The two
 * cases here are data list with implied DO loops or without the DO
 * loops.  In either case, we pass the expansion onwards. */

static try expand_data_var(g95_data_variable *var) {
g95_iterator iterator;
try t;

    if (var->expr != NULL)
	return expand_data_var0(var->expr);

    /* Translate a list with loops.  First copy the iterator to a
     * local copy, and simplify it with any current values. */

    iterator = var->iter;
    iterator.start = g95_copy_expr(iterator.start);
    iterator.end   = g95_copy_expr(iterator.end);
    iterator.step  = g95_copy_expr(iterator.step);

    t = FAILURE;

    if (g95_simplify_expr(iterator.start) == FAILURE ||
	g95_simplify_expr(iterator.end)   == FAILURE ||
	g95_simplify_expr(iterator.step)  == FAILURE)
	goto cleanup;

    t = g95_expand_iterator(&iterator, (void *) expand_list, var->list);

cleanup:
    g95_free_expr(iterator.start);
    g95_free_expr(iterator.end);
    g95_free_expr(iterator.step);

    return t;
}



/* expand_data()-- Translate a single DATA statement.  We traverse the
 * variable and constant lists, matching the two as we go.  The
 * implicit loops within data statements are used as are the repeat
 * counts of the constants. */

static try expand_data(g95_data *d) {
g95_data_variable *var;
try t;

    dinfo.repeat_count = 0;
    dinfo.current_value = d->value;
    dinfo.current_data = d->where;

    dinfo.current_offset = bi_0;
    dinfo.string_length = bi_0;

    t = FAILURE;

    for(var=d->var; var; var=var->next)
	if (expand_data_var(var) == FAILURE)
	    goto done;

    /* We're done.  Make sure that the list of data items is also empty */

    if (dinfo.current_value != NULL) {
	g95_error("DATA statement at %L has more values than variables",
		  &dinfo.current_value->expr->where);
	goto done;
    }

    t = SUCCESS;

done:
    big_free(dinfo.current_offset);
    big_free(dinfo.string_length);

    dinfo.current_offset = NULL;
    dinfo.string_length = NULL;

    return t;
}



/* has_current_symbol()-- Checks to see if the data node involves the
 * current symbol.  Returns nonzero if this is so. */

static int has_current_symbol(g95_data_variable *var) {

    if (var == NULL)
	return 0;

    return (var->expr != NULL &&
	    var->expr->symbol == dinfo.current_symbol) ||
	has_current_symbol(var->list) ||
	has_current_symbol(var->next);
}



/* generate_data()-- Process the DATA statements in a program
 * unit for a particular symbol.  This subroutine can be called
 * multiple times when initializing a COMMON block. */

static void generate_data(g95_symbol *sym, int offset) {
g95_data *d;

    if (dinfo.error_flag)
	return;

    dinfo.current_symbol = sym;
    dinfo.initial_offset = offset;

    for(d=sym->ns->data; d; d=d->next) {
	if (!has_current_symbol(d->var))
	    continue;

	if (expand_data(d) == FAILURE) {
	    dinfo.error_flag = 1;
	    break;
	}
    }
}



/* same_type0()-- Recursive function for same_type() */

static int same_type0(init_tree *p, tree type) {

    if (p == NULL)
	return 1;

    return init_tree_type(p->head) == type
	&& same_type0(p->left,  type)
	&& same_type0(p->right, type);
}



/* same_type()-- Checks to see if all elements of the tree are the
 * same type as the root node.  Return nonzero if this is so. */

static int same_type(void) {
tree type;

    type = init_tree_type(dinfo.root->head);

    return same_type0(dinfo.root, type);
}



/* element_number()-- Given an offset, figure out which element number
 * this is, zero based. */

static int element_number(int offset) {
int size;

    size = int_size_in_bytes(init_tree_type(dinfo.root->head));

    return offset / size;
}



/* build_array_init0()-- Add the current init_tree to the list of
 * constructors. */

static void build_array_init0(init_tree *p) {
tree decl;
int n;

    n = element_number(p->start);

    decl = p->head;
    if (TREE_CODE(decl) != TREE_LIST) {  /* Single element */
	decl = tree_cons(NULL_TREE, decl, NULL_TREE);
	TREE_PURPOSE(decl) = g95_build_int(n, 0);

	TREE_CHAIN(decl) = dinfo.array_list;
	dinfo.array_list = decl;

    } else {   /* List of elements */
	for(;;) {
	    TREE_PURPOSE(decl) = g95_build_int(n, 0);
	    if (TREE_CHAIN(decl) == NULL)
		break;

	    n++;
	    decl = TREE_CHAIN(decl);
	}

	TREE_CHAIN(decl) = dinfo.array_list;
	dinfo.array_list = p->head;
    }
}



/* build_array_init1()-- Recursive function for building the elements
 * of an array constructor.  Traversal is right to left so that the
 * final list is in order of ascending elements. */

static void build_array_init1(init_tree *p) {

    if (p != NULL) {
	build_array_init1(p->right);
	build_array_init0(p);
	build_array_init1(p->left);
    }
}



/* build_array_init()-- At this point, all elements in the tree are of
 * the same type and we are building an array constructor. */

static tree build_array_init(int total_size) {
init_tree *n;
tree decl;
int m;

    dinfo.array_list = NULL;
    build_array_init1(dinfo.root);

    /* Find the maximum array element */

    if (total_size == 0) {
	n = dinfo.root;
	while(n->right != NULL)
	    n = n->right;

	decl = g95_build_int(element_number(n->end)-1, 0);

    } else {
	m = int_size_in_bytes(init_tree_type(dinfo.root->head));
	m = (total_size + m - 1) / m;

	decl = g95_build_int(m-1, 0);
    }

    decl = build_index_type(decl);
    decl = build_array_type(init_tree_type(dinfo.root->head), decl);
    decl = g95_build_constructor(decl, dinfo.array_list);

    return decl;
}



/* add_struture_field()-- Given a value, add it to the structure being
 * built. */

static void add_structure_field(tree value) {
tree field;

    field = build_decl(FIELD_DECL, NULL_TREE, TREE_TYPE(value));
    DECL_CONTEXT(field) = init_type;
    DECL_PACKED(field) = 1;

    TYPE_FIELDS(init_type) = chainon(TYPE_FIELDS(init_type), field);

    init_value = chainon(init_value, tree_cons(field, value, NULL_TREE));
}



/* add_structure()-- Add the current init_tree to the current
 * structure constructor.  We also consider any holes between this
 * init_tree and the previous offset.  We initialize with a character
 * array of spaces if this or the last node is a character string.
 * Otherwise, we initialize with a character array of zeroes. */

static void add_structure(init_tree *t) {
int n, char_flag;
tree tmp, type;
char *p;

    n = t->start - last_right;

    if (t->head == NULL)
	type = NULL;

    else {
	type = (TREE_CODE(t->head) == TREE_LIST)
	    ? TREE_TYPE(TREE_VALUE(t->head))
	    : TREE_TYPE(t->head);

	if (TREE_CODE(type) == ARRAY_TYPE)
	    type = TREE_TYPE(type);
    }

    char_flag = (type == g95_character1_type_node);

    if (n > 0) {    /* Build some padding */
	tmp = g95_build_int(n, 0);
	tmp = build_range_type(g95_default_integer, integer_one_node, tmp);
	type = build_array_type(g95_character1_type_node, tmp);

	p = g95_getmem(n);
	memset(p, (char_flag || last_char_flag) ? ' ' : '\0', n);

	tmp = g95_build_string_const(n, p);
	add_structure_field(tmp);
    }

    last_right = t->end;
    last_char_flag = char_flag;

    if (t->head == NULL)
	return;

    if (TREE_CODE(t->head) != TREE_LIST)
	add_structure_field(t->head);

    else {
	n = list_length(t->head);
	type = TREE_TYPE(TREE_VALUE(t->head));

	tmp  = g95_build_int(n, 0);
	tmp  = build_range_type(g95_default_integer, integer_one_node, tmp);
	type = build_array_type(type, tmp);

	tmp = g95_build_constructor(type, t->head);
	add_structure_field(tmp);
    }
}



/* structure_traverse()-- Traverse the init_tree building a structure
 * constructor. */

static void structure_traverse(init_tree *t) {

    if (t != NULL) {
	structure_traverse(t->left);
	add_structure(t);
	structure_traverse(t->right);
    }
}



/* build_structure_init()-- Build a constructor for a heterogeneous
 * init_tree.  The tree is traversed from low offsets to high offsets. */

static tree build_structure_init(int total_length) {
init_tree t;
int m;

    last_right = 0;
    last_char_flag = 0;

    init_type = make_node(RECORD_TYPE);
    init_value = NULL_TREE;

    structure_traverse(dinfo.root);

    m = total_length - last_right;
    if (m > 0) {
	t.start = total_length;
	t.end = total_length;
	t.head = t.tail = NULL;

	add_structure(&t);
    }

    g95_finish_type(init_type);

    return g95_build_constructor(init_type, init_value);
}



/* hole_check()-- Recursive function for checking the initialization
 * for holes at the start and within the structure itself, but not at
 * the end.  Returns -1 if a hole is found. */

static int hole_check(init_tree *p, int end) {

    if (p->left != NULL) {
	end = hole_check(p->left, end);
	if (end == -1)
	    return -1;
    }

    if (p->start != end)
	return -1;

    return (p->right == NULL)
	? p->end
	: hole_check(p->right, p->end);
}



/* build_initializer()-- Given a tree created by calls to
 * g95_generate_data(), build a backend initializer.  For scalar
 * variables, this is just a constant value.  For arrays, we build an
 * array constructor if possible, or failing that a structure and the
 * corresponding constructor.  Returns NULL if something went wrong
 * with the DATA statements that built the tree. */

static tree build_initializer(int scalar, int total_length) {

    if (dinfo.error_flag) {
	dinfo.error_flag = 0;
	return NULL;
    }

    if (dinfo.root == NULL)
	return NULL_TREE;

    if (hole_check(dinfo.root, 0) == -1)
	scalar |= 1;

    return (scalar || !same_type())
	? build_structure_init(total_length)
	: build_array_init(total_length);
}



/* add_array_element()-- Add a new array element to the tree.
 * Elements are always added in sequence. */

static void add_array_element(tree value) {

    add_value(value, 1);
    dinfo.current_offset = bi_int_add(dinfo.current_offset,
				      int_size_in_bytes(TREE_TYPE(value)));
    dinfo.ac_count++;
}



/* constant_initializer()-- Generate an array constructor that comes
 * from a single constant. */

static tree constant_initializer(variable_info *vinfo, tree decl) {
bignum size;
int s;

    size = g95_array_spec_size(vinfo->as);
    if (size == NULL)
	g95_internal_error("constant_initializer(): Can't get array size");

    big_free(dinfo.current_offset);
    dinfo.current_offset = int_to_bi(dinfo.initial_offset);

    s = int_size_in_bytes(TREE_TYPE(decl));

    while(bi_compare(big_copy(size), bi_0) > 0) {
	add_value(decl, 1);

	size = bi_int_add(size, -1);
	dinfo.current_offset = bi_int_add(dinfo.current_offset, s);
    }

    big_free(size);
    return NULL_TREE;
}



/* g95_expand_ac_element()-- Convert a constant element of a
 * constructor, adding it to the current list. */

try g95_expand_ac_element(g95_expr *e) {
g95_se se;
int flag;

    flag = 0;
    if (saved_se == NULL) {
	flag = 1;
	saved_se = &se;
	g95_init_se(&se, NULL);

	/* we should have a constant at this point, but can also have
	 * constant array constructors. */
    }

    g95_conv_expr(saved_se, e);

    if (STRING_P(saved_se->expr))
	saved_se->expr =
	    g95_resize_string_constant(saved_se->expr, dinfo.ac_string_length);

    add_array_element(saved_se->expr);

    g95_free_expr(e);

    if (flag)
	saved_se = NULL;

    return SUCCESS;
}



/* array_initializer()-- Take a single array initialization
 * and add it to the tree.  Create an array constructor from an
 * initialization expression.  This only handles constant arrays. */

static void array_initializer(variable_info *vinfo, int offset, g95_se *se) {
g95_se se0, *se1;

    dinfo.ac_count = 0;
    dinfo.ac_string_length = (vinfo->ts.type == BT_CHARACTER)
	? vinfo->ts.cl->backend_decl
	: NULL_TREE;

    dinfo.initial_offset = offset;
    dinfo.current_offset = int_to_bi(offset);

    if (vinfo->value != NULL)
	dinfo.current_data = vinfo->value->where;

    if (vinfo->value == NULL && vinfo->ts.type == BT_DERIVED &&
	G95_DTYPE_INITIAL(vinfo->ts.derived->backend_decl) != NULL_TREE)
	constant_initializer(vinfo,
			  G95_DTYPE_INITIAL(vinfo->ts.derived->backend_decl));


    else {
	switch(vinfo->value->type) {
	case EXPR_CONSTANT:
	case EXPR_STRUCTURE:
	    g95_init_se(&se0, NULL);
	    g95_conv_constant(&se0, vinfo->value);

	    if (STRING_P(se0.expr))
		se0.expr =
		    g95_resize_string_constant(se0.expr,
					       dinfo.ac_string_length);

	    constant_initializer(vinfo, se0.expr);
	    break;

	case EXPR_ARRAY:
	    se1 = saved_se;

	    saved_se = se;
	    g95_expand_data_constructor(vinfo->value);

	    saved_se = se1;
	    break;

	default:
	    g95_init_se(&se0, NULL);
	    g95_conv_expr(&se0, vinfo->value);

	    g95_add_block_to_block(&se->pre, &se0.pre);
	    g95_add_block_to_block(&se->post, &se0.post);

	    if (!G95_ARRAY(se0.expr))
		se0.expr = save_expr(se0.expr);

	    constant_initializer(vinfo, se0.expr);
	    break;
	}
    }

    big_free(dinfo.current_offset);
    dinfo.current_offset = NULL;
}


/* scalar_initializer()-- Place the initialization value of a scalar
 * symbol into a common block. */

static void scalar_initializer(g95_symbol *sym, int offset) {
g95_se se;
tree len;

    if (sym->value == NULL)
	se.expr = g95_default_scalar_value(sym);

    else {
	dinfo.current_data = sym->declared_at;

	g95_init_se(&se, NULL);
	g95_conv_constant(&se, sym->value);

	if (STRING_P(se.expr)) {
	    len = sym->ts.cl->backend_decl;
	    se.expr = g95_resize_string_constant(se.expr, len);
	}
    }

    dinfo.current_offset = int_to_bi(offset);
    add_value(se.expr, 1);

    big_free(dinfo.current_offset);
    dinfo.current_offset = NULL;
}



/* g95_init_common_var()-- Set the initial value of a variable within
 * a common block, be it in a DATA statement or with an initial value. */

void g95_init_common_var(g95_symbol *sym, int offset) {
variable_info vinfo;

    if (sym->value == NULL)
	generate_data(sym, offset);

    else if (sym->as == NULL)
	scalar_initializer(sym, offset);

    else {
	g95_symbol_vinfo(sym, &vinfo);
	array_initializer(&vinfo, offset, NULL);
    }
}



/* default_structure_init()-- Recursive work function for
 * g95_default_structure_init(). */

static void default_structure_init(g95_symbol *sym, bignum offset) {
variable_info vinfo;
bignum count, save;
g95_component *c;
tree field;
g95_se se;

    save = dinfo.current_offset;

    for(c=sym->components; c; c=c->next) {
	count = (c->as == NULL ||
		 (c->initializer != NULL && c->initializer->rank > 0))
	    ? bi_1
	    : g95_array_spec_size(c->as);

	field = c->dimension
	    ? G95_ARRAY_FIELD(c->backend_decl)
	    : c->backend_decl;

	dinfo.current_offset = bi_int_add(big_copy(offset),
					  int_byte_position(field));

	if (c->initializer != NULL) {
	    g95_init_se(&se, NULL);

	    if (c->initializer->type != EXPR_ARRAY)     
		g95_conv_constant(&se, c->initializer);

	    else {
		g95_component_vinfo(c, &vinfo);
		g95_conv_array_initializer(&vinfo, &se);
	    }

	    while(bi_compare(big_copy(count), bi_0) > 0) {
		add_value(se.expr, 0);
		count = bi_subtract(count, bi_1);
		dinfo.current_offset =
		    bi_int_add(dinfo.current_offset, g95_int_ts_size(&c->ts));
	    }

	} else if (c->ts.type == BT_DERIVED) {
	    while(bi_compare(big_copy(count), bi_0) > 0) {
		default_structure_init(c->ts.derived,
				       big_copy(dinfo.current_offset));
		count = bi_subtract(count, bi_1);
		dinfo.current_offset =
		    bi_int_add(dinfo.current_offset, g95_int_ts_size(&c->ts));
	    }
	}

	big_free(count);
	big_free(dinfo.current_offset);
    }

    big_free(offset);
    dinfo.current_offset = save;
}



/* g95_default_structure_init()-- Given a derived type symbol in a
 * common block, create any default initializations that are
 * needed. */

void g95_default_structure_init(g95_symbol *sym, int offset0) {
bignum count, offset;
g95_locus save;

    if (sym->ts.type != BT_DERIVED)
	return;

    save = dinfo.current_data;
    dinfo.current_data = sym->declared_at;

    count = (sym->as == NULL)
	? bi_1
	: g95_array_spec_size(sym->as);

    offset = int_to_bi(offset0);
 
    while(bi_compare(big_copy(count), bi_0) > 0) {
	default_structure_init(sym->ts.derived, big_copy(offset));

	count = bi_subtract(count, bi_1);
	offset = bi_int_add(offset, g95_int_ts_size(&sym->ts));
    }

    dinfo.current_data = save;
}



/* g95_start_common()-- Get ready to build a common block. */

void g95_start_common(void) {

    push_data();
}



/* g95_data_initializer()-- Return a constructor for the current
 * object, which can be a wide variety of things. */

tree g95_data_initializer(int scalar, int total_length) {
tree decl;

    while(find_soft_join(dinfo.root));  /* Join soft nodes together */

    decl = build_initializer(scalar, total_length);

    free_init_tree(dinfo.root);
    dinfo.root = NULL;
    dinfo.error_flag = 0;

    pop_data();
    return decl;
}



/* g95_generate_data()-- Given a symbol not in a common, generate an
 * initializer from the DATA statements that mention it. */

tree g95_generate_data(g95_symbol *sym) {

    push_data();
    generate_data(sym, 0);

    return g95_data_initializer(sym->as == NULL, 0);
}



/* g95_conv_array_initializer()-- Main entry point for converting a
 * single array initializer to an initialized block.  This subroutine
 * doesn't have anything to do with DATA statements, but laying out
 * the constructor uses the same machinery. */

tree g95_conv_array_initializer(variable_info *vinfo, g95_se *se) {

    if (vinfo->value != NULL && vinfo->value->type == EXPR_NULL)
	return g95_null_array(vinfo->as->rank, 0);

    push_data();
    array_initializer(vinfo, 0, se);

    return g95_data_initializer(0, 0);
}



/* g95_simple_array_init()-- Create a simple array constructor from a
 * (constant) array specification and an initial value. */

tree g95_simple_array_init(variable_info *vinfo, tree value) {

    push_data();

    dinfo.current_offset = bi_0;

    constant_initializer(vinfo, value);

    big_free(dinfo.current_offset);
    dinfo.current_offset = NULL;

    return g95_data_initializer(0, 0);
}

