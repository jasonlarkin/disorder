
/* Transform scalar (and some vector) array expressions

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
Boston, MA 02111-1307, USA.

*/


/* An array descriptor looks like:

struct {
    char *offset;
    void *base;
    int rank, corank, element_size;

    struct {
        long mult, lbound, ubound;
    } dimen[G95_MAX_DIMENSIONS];
} g95_array_descriptor;

To make things easier for the back end, the array of structures at the
end is really a one dimensional array of default integers named info[],
which is only as large as it has to be.  The multipliers are at index
3*d, lbounds at 3*d+1 and ubounds at 3*d+2 where d is the (zero based)
dimension we are interested in.  The address of an array element given
by (x_1, x_2, ..., x_n)

is determined by calculating

   address = offset + mult_1*x_1 + mult_2*x_2 + ... + mult_n*x_n.

The offset pointer is first within the descriptor for ease of access.
The base pointer points to a block of memory occupied by the array and
is typically only used if the array is ALLOCATABLE or temporary and
will usually be NULL.  Storage for the array is managed separately
from the descriptor.  The multipliers are calculated by

   mult_1 = element size in bytes
   mult_2 = dim(ubound_1+1, lbound_1) * mult_1
   mult_3 = dim(ubound_2+1, lbound_2) * mult_2
   ...
   mult_n = dim(ubound_{n-1}+1, lbound_{n-1}) * mult_{n-1}

For an n dimensional array, mult_{n+1} is the size of the entire array
in bytes.  This formula correctly calculates the size of an array if
lbound_n>ubound_n for any n, which means a zero-sized array.  mult_1
is usually equal to the element size in bytes, but can be different if
an array section is passed between program units.

The offset pointer is calculated using:

   offset = base - (mult_1*lbound_1 + mult_2*lbound_2 + ... + mult_n*lbound_n)

which is the condition that (lbound_1, lbound_2, ..., lbound_n) points
to the first element of the array.  Note that the base can also be
recovered from the offset through

   base = offset + mult_1*lbound_1 + mult_2*lbound_2 + ... + mult_n*lbound_n.

If the array specification is such that the inner product overflows a
single word, things still work because the calculation of the offset
underflows in the same way: (a+(b-a)) mod N = b mod N.  If a
multiplier overflows, then the array is too big to hold in memory
anyhow.

The rank of the array is stored in the descriptor but is never used by
compiled code, which implicitly knows how many dimensions are in the
array.  This is for use by library and external user subroutines in
other languages.  It also allows us to avoid recompiles of the library
if G95_MAX_DIMENSIONS changes.

Assumed shape arrays are passed to procedures by passing a pointer to
the descriptor.  Passing an array section causes a new descriptor to
be created that accesses the same memory in a different way.  The new
descriptor is passed to the procedure.  Array expressions including
vector subscripts are transformed to full temporary array earlier in
the translation process and are not issues at this level.

Deferred shape arrays use the original descriptor and is therefore the
same shape as the original.  Allocatable arrays are created as a
descriptor that is initialized by the ALLOCATE statement.  Assumed
size arrays create a new descriptor that points into an existing block
of memory, though if the block is not contiguous (through being passed
an array section), the elements of the array must be copied and
repacked into a new block of memory.  A library subroutine takes care
of this mess.

Descriptors live almost exclusively on the stack.  The exception is
arrays that are module variables or are saved.  Common blocks require
storage association and end up being created by subroutines that use
the common block (though they are always explicitly shaped with
constant specifications).

Arrays that live in derived types are treated similarly, mainly
because the derived types can also be in the common blocks and must
storage-associate correctly.  For an allocatable array in a structure
(f2k) it would make sense for the descriptor to live in the structure.
Descriptors for arrays in structures are created as needed. 

The ultimate story of variable reference is as follows:  We
start with the base variable.  If the variable is a derived type and
we are referencing a component, a component reference is built.  If
the entity is then an array, we have to build a descriptor for the
array.  If the array has an array reference associated with it, we use
the descriptor to build a calculation that returns the address of the
element.  And so on through the reference list.

A wicked case in which an intermediate reference is an array section
is handled by recognizing that the ultimate result is an array
section.  The initial descriptor is calculated, then modified by
succeeding references, which can only modify the offset by a constant.
Intermediate references that are vector subscripts are expanded into
temporary arrays at a higher level.

*/

#include "trans.h"


static tree section_info;
tree g95_empty_array, null_array;


/* The zeroth elements of array_cache and coarray_cache are unused. */

static tree array_cache[G95_MAX_DIMENSIONS+1];
static tree coarray_cache[2*G95_MAX_DIMENSIONS+1];



/* g95_init_array_types()-- Initialize array types.  Zero dimensional
 * arrays are not allowed. */

void g95_init_array_types(void) {
tree tmp;

    /* Build the section_info[] array. */

    tmp = g95_build_int(4*G95_MAX_DIMENSIONS, 0);
    tmp = build_range_type(g95_pointer_integer, integer_zero_node, tmp);
    tmp = build_array_type(g95_pointer_integer, tmp);

    section_info =
	build_decl(VAR_DECL, get_identifier(PREFIX "section_info"), tmp);

    DECL_EXTERNAL(section_info) = 1;
    TREE_PUBLIC(section_info) = 1;

    null_array = g95_array_node();

    /* Empty array constructor */

    g95_empty_array = build_decl(VAR_DECL,
				 get_identifier(PREFIX "empty_array"),
				 g95_get_array_desc(1, 0));

    DECL_EXTERNAL(g95_empty_array) = 1;
    TREE_PUBLIC(g95_empty_array)   = 1;

    g95_empty_array = g95_addr_expr(g95_empty_array);
}



/* g95_get_array_desc()-- Get an array descriptor type of a particular
 * dimension.  We cache arrays and coarrays of particular types.  For
 * a coarray, the only important part is the sum of rank and corank.
 * In particular, a scalar coarray has rank=0. */

tree g95_get_array_desc(int rank, int corank) {
tree type, tmp;
int t;

    t = rank + corank;

    if (corank == 0) {
	if (array_cache[rank] != NULL)
	    return array_cache[rank];

    } else if (coarray_cache[t] != NULL)
	return coarray_cache[t];

    type = make_node(RECORD_TYPE);

    g95_add_field(type, "offset",  pchar_type_node);
    g95_add_field(type, "base",    pvoid_type_node);
    g95_add_field(type, "rank",    g95_default_integer);
    g95_add_field(type, "corank",  g95_default_integer);
    g95_add_field(type, "esize",   g95_default_integer);

    tmp = g95_build_int(3*t-1, 0);
    tmp = build_range_type(g95_pointer_integer, integer_zero_node, tmp);
    tmp = build_array_type(g95_pointer_integer, tmp);

    g95_add_field(type, "info", tmp);
    g95_finish_type(type);

    if (corank == 0)
	array_cache[rank] = type;

    else
	coarray_cache[t] = type;

    G95_DESCRIPTOR_P(type) = 1;
    return type;
}



/* g95_get_array_pdesc()-- Get a type node that is a pointer to a
 * descriptor. */

tree g95_get_array_pdesc(int rank, int corank) {

    return build_pointer_type(g95_get_array_desc(rank, corank));
}



/* conditional_indirect()-- Given a variable which might be a pointer
 * to a descriptor, insert the indirection to return a reference to
 * the descriptor. */

static tree conditional_indirect(tree decl) {

    if (G95_DESCRIPTOR_P(TREE_TYPE(decl)))
	return decl;

    if (POINTER_TYPE_P(TREE_TYPE(decl)) &&
	G95_DESCRIPTOR_P(TREE_TYPE(TREE_TYPE(decl))))
	return g95_build_indirect(TREE_TYPE(TREE_TYPE(decl)), decl);

    g95_internal_error("conditional_indirect(): Not a descriptor");
}



/* g95_desc_addr()-- Given a variable which might be a descriptor,
 * return a pointer to the descriptor. */

tree g95_desc_addr(tree node) {

    if (G95_DESCRIPTOR_P(TREE_TYPE(node)))
	return g95_addr_expr(node);

    if (POINTER_TYPE_P(TREE_TYPE(node)) &&
	G95_DESCRIPTOR_P(TREE_TYPE(TREE_TYPE(node))))
	return node;

    g95_internal_error("g95_desc_addr(): Not a descriptor");
}



/* get_dimension()-- Given a descriptor variable, figure out what
 * dimension it represents.  This forms an index into the component
 * arrays to get the right component of the right type. */

static int get_dimension(tree desc) {
tree type;
int i;

    type = TREE_TYPE(desc);

    if (POINTER_TYPE_P(type))
	type = TREE_TYPE(type);

    for(i=1; i<=G95_MAX_DIMENSIONS; i++)
	if (array_cache[i] == type)
	    return i;

    g95_internal_error("get_dimension(): Bad array type");
}



/* g95_offset_ref()-- Given a descriptor node, return a reference to
 * the offset member. */

tree g95_offset_ref(tree desc) {
tree field;

    desc = conditional_indirect(desc);
    field = g95_find_field(TREE_TYPE(desc), "offset");

    return g95_build_component(TREE_TYPE(field), desc, field);
}



/* g95_esize_ref()-- Given a descriptor node, return a reference to
 * the element size member. */

tree g95_esize_ref(tree desc) {
tree field;

    desc = conditional_indirect(desc);
    field = g95_find_field(TREE_TYPE(desc), "esize");

    return g95_build_component(TREE_TYPE(field), desc, field);
}



/* g95_esize_value()-- Return the value of the array element size. */

tree g95_esize_value(tree array) {

    return (G95_ARRAY_ESIZE(array) == NULL_TREE)
	? g95_esize_ref(G95_ARRAY_DESC(array))
	: G95_ARRAY_ESIZE(array);
}



/* g95_rank_ref()-- Generate a reference to the rank element of a descriptor. */

tree g95_rank_ref(tree desc) {
tree field;

    desc = conditional_indirect(desc);
    field = g95_find_field(TREE_TYPE(desc), "rank");

    return g95_build_component(TREE_TYPE(field), desc, field);
}


/* g95_corank_ref()-- Generate a reference to the coarray element of a
 * descriptor. */

tree g95_corank_ref(tree desc) {
tree field;

    desc = conditional_indirect(desc);
    field = g95_find_field(TREE_TYPE(desc), "corank");

    return g95_build_component(TREE_TYPE(field), desc, field);
}



/* g95_base_ref()-- Generate a reference to the descriptor base */

tree g95_base_ref(tree desc) {
tree field;

    desc = conditional_indirect(desc);
    field = g95_find_field(TREE_TYPE(desc), "base");

    return g95_build_component(TREE_TYPE(field), desc, field);
}



/* g95_base_value()-- Given an array node, return the base value. */

tree g95_base_value(tree array) {

    return (G95_ARRAY_BASE(array) == NULL_TREE)
	? g95_base_ref(G95_ARRAY_DESC(array))
	: G95_ARRAY_BASE(array);
}



/* g95_desc_info()-- Reference the info[] array within a descriptor.
 * The dimension indeces are one-based within this subroutine, and m
 * must be nonpositive. */

tree g95_desc_info(tree desc, tree dim, int m) {
tree tmp, index, field;

    desc = conditional_indirect(desc);
    field = g95_find_field(TREE_TYPE(desc), "info");

    tmp = g95_build_component(TREE_TYPE(field), desc, field);

    index = g95_build_mult(g95_default_integer, dim, g95_build_int(3, 0));

    if (m != 0)
	index = g95_build_plus(g95_default_integer, index,
			       g95_build_int(m, -1));

    return g95_build_array_ref(g95_pointer_integer, tmp, index);
}



/* g95_multiplier_ref()-- Given a descriptor and a dimension, return a
 * reference to the multiplier. */

tree g95_multiplier_ref(tree desc, int dim) {

    return g95_desc_info(desc, g95_build_int(dim+1, 0), -3);
}



/* g95_lbound_ref()-- Given a descriptor and a dimension, return a
 * reference to the lower bound. */

tree g95_lbound_ref(tree desc, int dim) {

    return g95_desc_info(desc, g95_build_int(dim+1, 0), -2);
}



/* g95_ubound_ref()-- Given a descriptor and a dimension, return
 * a reference to the ubound member. */

tree g95_ubound_ref(tree desc, int dim) {

    return g95_desc_info(desc, g95_build_int(dim+1, 0), -1);
}



/* multiplier_value()-- Given an array and a dimension, return the
 * value of the multiplier. */

static tree multiplier_value(tree array, int dimension) {
tree mult;

    mult = G95_ARRAY_MULT(array, dimension);

    return (mult != NULL_TREE)
	? mult
	: g95_multiplier_ref(G95_ARRAY_DESC(array), dimension);
}



/* lbound_value()-- Given an array and a dimension, return the
 * value of the lower bound. */

static tree lbound_value(tree array, int dimension) {

    return (G95_ARRAY_LBOUND(array, dimension) != NULL_TREE &&
	    G95_CONSTANT_P(G95_ARRAY_LBOUND(array, dimension)))
	? G95_ARRAY_LBOUND(array, dimension)
	: g95_lbound_ref(G95_ARRAY_DESC(array), dimension);
}



/* ubound_value()-- Given an array and a dimension, return the
 * value of the upper bound. */

static tree ubound_value(tree array, int dimension) {

    return (G95_ARRAY_UBOUND(array, dimension) != NULL_TREE &&
	    G95_CONSTANT_P(G95_ARRAY_UBOUND(array, dimension)))
	? G95_ARRAY_UBOUND(array, dimension)
	: g95_ubound_ref(G95_ARRAY_DESC(array), dimension);
}



/* g95_character_array_len()-- Return the length of a character array,
 * taking into account the possibility of a nonpresent optional
 * argument. */

tree g95_character_array_len(g95_symbol *sym) {
tree size, tmp;

    size = g95_esize_ref(sym->backend_decl);

    if (sym->attr.optional) {
	tmp = g95_desc_addr(sym->backend_decl);
	tmp = g95_build_eq(tmp, null_pointer_node);

	size = g95_build_cond(g95_pointer_integer, tmp, integer_zero_node,
			      size);
    }

    return size;
}



/* g95_transfer_result()-- Generate a one dimensional array descriptor
 * as the results of a TRANSFER() intrinsic. */

tree g95_transfer_result(stmtblock_t *block, tree esize, tree src, tree size) {
tree desc, tmp;

    desc = g95_create_var(g95_get_array_desc(1, 0));

    tmp = g95_rank_ref(desc);
    g95_add_modify_expr(block, tmp, integer_one_node);

    tmp = g95_corank_ref(desc);
    g95_add_modify_expr(block, tmp, integer_zero_node);

    tmp = g95_esize_ref(desc);
    g95_add_modify_expr(block, tmp, esize);

    tmp = g95_multiplier_ref(desc, 0);
    g95_add_modify_expr(block, tmp, esize);

    tmp = g95_lbound_ref(desc, 0);
    g95_add_modify_expr(block, tmp, integer_zero_node);

    tmp = g95_ubound_ref(desc, 0);
    size = g95_build_minus(g95_pointer_integer, size, integer_one_node);
    g95_add_modify_expr(block, tmp, size);

    tmp = g95_offset_ref(desc);
    g95_add_modify_expr(block, tmp, src);

    tmp = g95_base_ref(desc);
    g95_add_modify_expr(block, tmp, g95_offset_ref(desc));

    return desc;
}



/* g95_nullify_array_pointer()-- Given an array pointer, generate code
 * to nullify the pointer. */

void g95_nullify_array_pointer(stmtblock_t *block, tree array) {
tree tmp;
int n, d;

    tmp = g95_offset_ref(G95_ARRAY_DESC(array));
    g95_add_modify_expr(block, tmp, null_pointer_node);

    tmp = g95_base_value(array);
    g95_add_modify_expr(block, tmp, null_pointer_node);

    d = get_dimension(G95_ARRAY_DESC(array));

    for(n=0; n<d; n++) {
	tmp = g95_multiplier_ref(G95_ARRAY_DESC(array), n);
	g95_add_modify_expr(block, tmp, integer_zero_node);
    }
}



/* convert_bounds()-- Given an array spec, convert the expressions
 * forming the bounds into the array pairs. */

static void convert_bounds(g95_array_spec *as, g95_coarray_spec *cas,
			   tree array) {
int i, j, corank;
tree desc, tmp;
g95_se se;

    G95_ARRAY_RANK(array) = g95_build_int(as->rank, 0);
    g95_init_se(&se, NULL);
    desc = G95_ARRAY_DESC(array);

    for(i=0; i<as->rank; i++) {
	if (as->lower[i] == NULL && as->type == AS_ASSUMED_SHAPE) {
	    G95_ARRAY_LBOUND(array, i) =
		convert(g95_pointer_integer, integer_one_node);

	    tmp = g95_lbound_ref(desc, i);
	    g95_add_modify_expr(&se.pre, tmp, integer_one_node);
	}

	if (as->lower[i] != NULL) {
	    g95_conv_spec_expr(&se, as->lower[i]);
	    G95_ARRAY_LBOUND(array, i) =
		convert(g95_pointer_integer, se.expr);

	    if (desc != NULL_TREE) {
		tmp = g95_lbound_ref(desc, i);
		g95_add_modify_expr(&se.pre, tmp, G95_ARRAY_LBOUND(array, i));
	    }
	}

	if (as->upper[i] != NULL) {
	    g95_conv_spec_expr(&se, as->upper[i]);
	    G95_ARRAY_UBOUND(array, i) =
		convert(g95_pointer_integer, se.expr);

	    if (desc != NULL_TREE) {
		tmp = g95_ubound_ref(desc, i);
		g95_add_modify_expr(&se.pre, tmp, G95_ARRAY_UBOUND(array, i));
	    }
	}
    }

    corank = (cas == NULL) ? 0 : cas->corank;
    G95_ARRAY_CORANK(array) = g95_build_int(corank, 0);

    for(j=0; j<corank; j++, i++) {
	g95_conv_spec_expr(&se, cas->lower[j]);
	G95_ARRAY_LBOUND(array, i) =
	    convert(g95_pointer_integer, se.expr);

	G95_ARRAY_MULT(array, i) = 
	    convert(g95_pointer_integer, integer_zero_node);

	if (desc != NULL_TREE) {
	    tmp = g95_lbound_ref(desc, i);
	    g95_add_modify_expr(&se.pre, tmp, se.expr);
	}

	if (cas->upper[j] != NULL) {
	    g95_conv_spec_expr(&se, cas->upper[j]);
	    G95_ARRAY_UBOUND(array, i) =
		convert(g95_pointer_integer, se.expr);

	    if (desc != NULL_TREE) {
		tmp = g95_ubound_ref(desc, i);
		g95_add_modify_expr(&se.pre, tmp, se.expr);
	    }
	}
    }

    g95_add_block_to_block(&se.pre, &se.post);
    G95_ARRAY_INIT(array) = g95_finish_block(&se.pre);
}



/* get_rank()-- Extract the rank of an array. */

static int get_rank(tree array) {

    return TREE_INT_CST_LOW(G95_ARRAY_RANK(array));
}



/* get_multipliers()-- Calculate the multipliers and size of an
 * explicit array.  Returns a tree representing the total size of the
 * array is returned if the expression is constant, NULL_TREE
 * otherwise. */

static tree get_multipliers(tree array, g95_array_spec *as) {
tree tmp, desc, extent;
stmtblock_t block;
int rank, d;

    rank = as->rank;

    g95_init_block(&block);
    g95_add_expr_to_block(&block, G95_ARRAY_INIT(array));

    G95_ARRAY_INIT(array) = NULL;
    desc = G95_ARRAY_DESC(array);

    for(d=0; d<rank; d++) {    /* Calculate the multipliers */
	if (d == 0) {
	    G95_ARRAY_MULT(array, 0) =
		convert(g95_pointer_integer, g95_esize_value(array));

	    if (desc != NULL_TREE) {
		tmp = g95_multiplier_ref(desc, 0);
		g95_add_modify_expr(&block, tmp, G95_ARRAY_MULT(array, 0));
	    }

	} else {
	    extent = g95_build_minus(g95_pointer_integer,
				     ubound_value(array, d-1),
				     lbound_value(array, d-1));

	    extent = g95_build_plus(g95_pointer_integer, extent,
				    integer_one_node);
	    extent = save_expr(extent);

	    /* Make sure the extent is nonnegative */

	    extent = g95_build_max(g95_pointer_integer, extent,
				   convert(g95_pointer_integer,
					   integer_zero_node));

	    tmp = multiplier_value(array, d-1);
	    tmp = g95_build_mult(g95_pointer_integer, extent, tmp);
	    G95_ARRAY_MULT(array, d) = tmp;

	    if (desc != NULL_TREE) {
		tmp = g95_multiplier_ref(desc, d);
		g95_add_modify_expr(&block, tmp, G95_ARRAY_MULT(array, d));
	    }
	}

	if (d == as->rank - 1 && as->type == AS_ASSUMED_SIZE) {
	    tmp = NULL_TREE;
	    goto done;
	}
    }

    /* Build one more 'multiplier', which is the total size of the array */

    d = rank - 1;

    extent = g95_build_minus(g95_pointer_integer, ubound_value(array, d),
			     lbound_value(array, d));

    extent = g95_build_plus(g95_pointer_integer, extent, integer_one_node);
    extent = save_expr(extent);

    extent = g95_build_max(g95_pointer_integer, extent,
			   convert(g95_pointer_integer, integer_zero_node));

    tmp = multiplier_value(array, d);
    tmp = g95_build_mult(g95_pointer_integer, extent, tmp);

    G95_ARRAY_SIZE(array) = tmp;
    tmp = (TREE_CODE(tmp) == INTEGER_CST) ? tmp : NULL_TREE;

done:
    G95_ARRAY_INIT(array) = g95_finish_block(&block);
    return tmp;
}



/* g95_get_array_storage()-- Get a declaration for storage associated
 * with an array, or NULL_TREE if the storage is obtained at runtime. */

tree g95_get_array_storage(variable_info *vinfo, tree array) {
tree tmp, etype, element_size;

    etype = g95_get_typenode(&vinfo->ts, 1);

    if (vinfo->ts.type != BT_CHARACTER)
	element_size = size_in_bytes(etype);

    else {
	element_size = vinfo->ts.cl->backend_decl;
	if (element_size == NULL_TREE)
	    return NULL_TREE;

	if (INTEGER_CST_P(element_size)) {
	    tmp = build_range_type(g95_pointer_integer, integer_one_node,
				   element_size);
	    etype = build_array_type(char_type_node, tmp);
	}
    }

    G95_ARRAY_ESIZE(array) = save_expr(element_size);

    if (vinfo->as->type == AS_DEFERRED)
	return NULL_TREE;

    convert_bounds(vinfo->as, vinfo->cas, array);
    tmp = get_multipliers(array, vinfo->as);
    if (tmp == NULL_TREE)
	return NULL_TREE;

    if (vinfo->cas != NULL)
	G95_ARRAY_MULT(array, vinfo->cas->corank) = tmp;

    /* An array can be an array of zero length characters, so head off the
     * division by zero when calculating the total amount of storage needed. */

    if (TREE_INT_CST_LOW(element_size)  == 0 &&
	TREE_INT_CST_HIGH(element_size) == 0)
	tmp = integer_zero_node;

    else {
	tmp = fold(build2(TRUNC_DIV_EXPR, g95_pointer_integer, tmp,
			  element_size));
	tmp = g95_build_minus(g95_pointer_integer, tmp, integer_one_node);
    }

    return build_array_type(etype, build_index_type(tmp));
}



/* init_offset()-- Return the initial value for the offset member. */

static tree init_offset(tree array, tree base, tree offset) {
tree tmp, t;
int i, rank;

    rank = get_rank(array);
    t = integer_zero_node;

    for(i=0; i<rank; i++) {
	tmp = g95_build_mult(g95_pointer_integer, lbound_value(array, i),
			     multiplier_value(array, i));

	t = g95_build_minus(g95_pointer_integer, t, tmp);
    }

    if (offset != NULL_TREE)
	t = g95_build_plus(g95_pointer_integer, t, offset);

    t = convert(g95_pointer_integer, t);
    return g95_build_plus(pchar_type_node, base, t);
}



/* desc_constructor()-- Build a constructor for the descriptor. */

static tree desc_constructor(int rank, int corank, tree array,
			     tree desc_type) {
tree node, info_node, t, tmp, storage, field;
int flag, i;

    if (desc_type == NULL)
	desc_type = TREE_TYPE(G95_ARRAY_DESC(array));

    storage = G95_ARRAY_STORAGE(array);

    if (storage == NULL_TREE)
	storage = null_pointer_node;

    else if (!POINTER_TYPE_P(TREE_TYPE(storage)))
	storage = g95_addr_expr(storage);

    /* offset */

    flag = (storage != null_pointer_node);

    for(i=0; i<rank; i++)
	if (G95_ARRAY_LBOUND(array, i) == NULL_TREE ||
	    G95_ARRAY_MULT(array, i) == NULL_TREE)
	    flag = 0;

    t = flag
	? init_offset(array, storage, NULL_TREE)
	: null_pointer_node;

    field = g95_find_field(desc_type, "offset");
    node = tree_cons(field, t, NULL_TREE);

    /* base */

    field = g95_find_field(desc_type, "base");
    node = tree_cons(field, storage, node);

    /* rank */

    tmp = G95_ARRAY_RANK(array);
    if (tmp == NULL_TREE)
	tmp = convert(g95_default_integer, integer_zero_node);

    field = g95_find_field(desc_type, "rank");
    node = tree_cons(field, tmp, node);

    /* corank */

    tmp = G95_ARRAY_CORANK(array);
    if (tmp == NULL_TREE)
	tmp = convert(g95_default_integer, integer_zero_node);

    field = g95_find_field(desc_type, "corank");
    node = tree_cons(field, tmp, node);

    /* esize */

    tmp = G95_ARRAY_ESIZE(array);
    if (tmp == NULL_TREE)
	tmp = convert(g95_default_integer, integer_zero_node);

    field = g95_find_field(desc_type, "esize");
    node = tree_cons(field, tmp, node);

    /* info */

    info_node = NULL_TREE;

    for(i=0; i<rank+corank; i++) {
	tmp = G95_ARRAY_MULT(array, i);
	if (tmp != NULL_TREE)
	    info_node = tree_cons(g95_build_int(3*i, 0), tmp, info_node);

	tmp = G95_ARRAY_LBOUND(array, i);
	if (tmp != NULL_TREE)
	    info_node = tree_cons(g95_build_int(3*i+1, 0), tmp, info_node);

	tmp = G95_ARRAY_UBOUND(array, i);
	if (tmp != NULL_TREE)
	    info_node = tree_cons(g95_build_int(3*i+2, 0), tmp, info_node);
    }

    tmp = g95_build_int(3*rank, 0);
    tmp = build_range_type(g95_pointer_integer, integer_zero_node, tmp);
    tmp = build_array_type(g95_pointer_integer, tmp);

    info_node = g95_build_constructor(tmp, nreverse(info_node));

    field = g95_find_field(desc_type, "info");
    node = tree_cons(field, info_node, node);

    return g95_build_constructor(desc_type, nreverse(node));
}



/* g95_null_array()-- Return a null array constructor. */

tree g95_null_array(int rank, int corank) {
tree type;

    type = g95_get_array_desc(rank, corank);

    return desc_constructor(rank, corank, null_array, type);
}



/* deferred_descriptor()-- Initialize a descriptor for a deferred array. */

static tree deferred_descriptor(variable_info *vinfo, tree array) {
stmtblock_t block;
tree tmp, desc;
int corank;

    g95_init_block(&block);

    desc = G95_ARRAY_DESC(array);

    tmp = g95_rank_ref(desc);
    g95_add_modify_expr(&block, tmp, g95_build_int(vinfo->as->rank, 0));

    tmp = g95_corank_ref(desc);
    corank = (vinfo->cas == NULL) ? 0 : vinfo->cas->corank;
    g95_add_modify_expr(&block, tmp, g95_build_int(corank, 0));

    tmp = g95_offset_ref(desc);
    g95_add_modify_expr(&block, tmp, null_pointer_node);

    tmp = g95_base_ref(desc);
    g95_add_modify_expr(&block, tmp, null_pointer_node);

    tmp = g95_esize_ref(desc);
    g95_add_modify_expr(&block, tmp, g95_ts_size(&vinfo->ts));

    return g95_finish_block(&block);
}



/* make_pointer()-- Given an object, return a pointer to the object if
 * it isn't a pointer already. */

static tree make_pointer(tree object) {

    if (!POINTER_TYPE_P(TREE_TYPE(object)))
	object = g95_addr_expr(object);

    return object;
}



/* g95_init_descriptor()-- Initialize a descriptor. */

void g95_init_descriptor(g95_se *se, tree array, tree desc, tree offset,
			 tree string_length) {
tree tmp, t, save;
int i, rank;

    save = G95_ARRAY_DESC(array);
    G95_ARRAY_DESC(array) = desc;

    tmp = g95_rank_ref(desc);
    g95_add_modify_expr(&se->pre, tmp, G95_ARRAY_RANK(array));

    tmp = g95_corank_ref(desc);
    g95_add_modify_expr(&se->pre, tmp, G95_ARRAY_CORANK(array));

    tmp = g95_esize_ref(desc);
    t = (string_length == NULL)
	? G95_ARRAY_ESIZE(array)
	: string_length;

    g95_add_modify_expr(&se->pre, tmp, t);

    tmp = g95_base_ref(desc);

    t = G95_ARRAY_STORAGE(array);
    if (TREE_CODE(TREE_TYPE(t)) == ARRAY_TYPE)
	t = g95_addr_expr(t);

    g95_add_modify_expr(&se->pre, tmp, t);

    rank = get_rank(array);

    for(i=0; i<rank; i++) {
	tmp = g95_lbound_ref(desc, i);
	g95_add_modify_expr(&se->pre, tmp, G95_ARRAY_LBOUND(array, i));

	if (G95_ARRAY_UBOUND(array, i) != NULL_TREE) {
	    tmp = g95_ubound_ref(desc, i);
	    g95_add_modify_expr(&se->pre, tmp, G95_ARRAY_UBOUND(array, i));
	}

	if (G95_ARRAY_MULT(array, i) != NULL_TREE) {
	    tmp = g95_multiplier_ref(desc, i);
	    g95_add_modify_expr(&se->pre, tmp, G95_ARRAY_MULT(array, i));
	}
    }

    tmp = g95_offset_ref(desc);
    g95_add_modify_expr(&se->pre, tmp,
			init_offset(array, g95_base_ref(desc), offset));

    G95_ARRAY_DESC(array) = save;
}



/* explicit_descriptor()-- Initialize a descriptor for an explicit array. */

static tree explicit_descriptor(tree array, g95_typespec *ts) {
tree tmp, node, storage, desc, initial;
stmtblock_t block;

    g95_init_block(&block);

    if (G95_ARRAY_INIT(array) != NULL_TREE) {
	g95_add_expr_to_block(&block, G95_ARRAY_INIT(array));
	G95_ARRAY_INIT(array) = NULL_TREE;
    }

    desc = G95_ARRAY_DESC(array);

    g95_add_modify_expr(&block, g95_rank_ref(desc),   G95_ARRAY_RANK(array));
    g95_add_modify_expr(&block, g95_corank_ref(desc), G95_ARRAY_CORANK(array));
    g95_add_modify_expr(&block, g95_esize_ref(desc),  G95_ARRAY_ESIZE(array));

    /* Calculate the offset */

    storage = G95_ARRAY_STORAGE(array);

    if (storage != NULL_TREE)
	node = make_pointer(storage);

    else {  /* Allocate the array on the heap */
	tmp = g95_base_ref(G95_ARRAY_DESC(array));
	g95_call_temp_alloc(&block, tmp, G95_ARRAY_SIZE(array));

	g95_call_temp_free(&g95_context->post, tmp);
	node = g95_base_value(array);

	if (ts->type == BT_DERIVED) {
	    initial = g95_derived_type_init(ts);

	    if (initial != null_pointer_node) {
		tmp = g95_call_library(void_type_node, PREFIX "array_init",
				       g95_addr_expr(desc), initial,
				       NULL_TREE);
		g95_add_expr_to_block(&block, tmp);
	    }
	}
    }

    if (G95_ARRAY_STORAGE(array) != NULL_TREE) {
	tmp = g95_addr_expr(G95_ARRAY_STORAGE(array));
	g95_add_modify_expr(&block, g95_base_ref(desc), tmp);
    }

    node = init_offset(array, node, NULL_TREE);
    g95_add_modify_expr(&block, g95_offset_ref(desc), node);

    return g95_finish_block(&block);
}



/* g95_init_array_desc()-- Initialize an array descriptor with its
 * initial value. */

void g95_init_array_desc(variable_info *vinfo, tree array) {
int rank, corank;
tree block;

    if (vinfo->static_storage) {
	rank   = (vinfo->as  == NULL) ? 0 : vinfo->as->rank;
	corank = (vinfo->cas == NULL) ? 0 : vinfo->cas->corank;

	DECL_INITIAL(G95_ARRAY_DESC(array)) =
	    desc_constructor(rank, corank, array, NULL);

    } else if (!vinfo->noinit) {
	switch(vinfo->as->type) {
	case AS_EXPLICIT:
	    block = explicit_descriptor(array, &vinfo->ts);
	    break;

	case AS_DEFERRED:
	    block = deferred_descriptor(vinfo, array);
	    break;

	default:
	    g95_internal_error("g95_init_array_base(): Bad type");
	    block = NULL_TREE;
	}

	if (block != NULL_TREE)
	    g95_add_expr_to_block(&g95_context->pre, block);
    }
}



/* g95_fix_dummy_array()-- At this point, sym is a dummy array
 * argument.  The backend_decl is a PARM_DECL, and we have to generate
 * setup code here. */

tree g95_fix_dummy_array(g95_symbol *sym) {
tree parm, tmp, desc, var, decl, array;
stmtblock_t block;

    parm = sym->backend_decl;

    array = sym->backend_decl = g95_array_node();
    G95_ARRAY_DUMMY(array) = parm;

    if (sym->attr.allocatable || sym->attr.pointer ||
	sym->as->type == AS_DEFERRED) {
	G95_ARRAY_DESC(array) = parm;
	return NULL_TREE;
    }

    desc = NULL_TREE;
    if (!g95_constant_array_spec(sym->as, 0) || sym->attr.desc ||
	sym->as->type == AS_ASSUMED_SHAPE) {
	desc = g95_create_var(g95_get_array_desc(sym->as->rank, 0));
	G95_ARRAY_DESC(array) = desc;
    }

    /* Convert bounds for the descriptor */

    g95_init_block(&block);

    convert_bounds(sym->as, sym->cas, array);

    if (G95_ARRAY_INIT(array) != NULL_TREE) {
	g95_add_expr_to_block(&block, G95_ARRAY_INIT(array));
	G95_ARRAY_INIT(array) = NULL_TREE;
    }

    G95_ARRAY_ESIZE(array) = (sym->ts.type != BT_CHARACTER)
	? g95_ts_size(&sym->ts)
	: sym->ts.cl->backend_decl;

    tmp = empty_stmt_node;

    switch(sym->as->type) {
    case AS_ASSUMED_SHAPE:
	if (sym->ts.type != BT_DERIVED || sym->attr.pointer ||
	    sym->attr.intent != INTENT_OUT ||
	    G95_DTYPE_INITIAL(sym->ts.derived->backend_decl) == NULL)
	    decl = null_pointer_node;

	else { /* Initialize an intent(out) derived type array */
	    decl = G95_DTYPE_INITIAL(sym->ts.derived->backend_decl);
	    var = g95_create_var(TREE_TYPE(decl));
	    DECL_INITIAL(var) = decl;
	    TREE_STATIC(var) = 1;

	    TREE_ADDRESSABLE(var) = 1;
	    decl = g95_addr_expr(var);
	}

	g95_add_modify_expr(&block, g95_rank_ref(desc),
			    G95_ARRAY_RANK(array));

	g95_add_modify_expr(&block, g95_corank_ref(desc),
			    G95_ARRAY_CORANK(array));

	g95_add_modify_expr(&block, g95_esize_ref(desc),
			    G95_ARRAY_ESIZE(array));

	tmp = g95_call_library(void_type_node, PREFIX "init_assumed_shape",
			       parm, g95_addr_expr(G95_ARRAY_DESC(array)),
			       decl, NULL_TREE);
	break;

    case AS_EXPLICIT:
    case AS_ASSUMED_SIZE:
	G95_ARRAY_STORAGE(array) = parm;
	if (desc == NULL)
	    break;

	g95_add_modify_expr(&block, g95_base_ref(desc), parm);
	get_multipliers(array, sym->as);

	if (G95_ARRAY_INIT(array) != NULL_TREE) {
	    g95_add_expr_to_block(&block, G95_ARRAY_INIT(array));
	    G95_ARRAY_INIT(array) = NULL_TREE;
	}

	g95_add_modify_expr(&block, g95_rank_ref(desc),
			    G95_ARRAY_RANK(array));

	g95_add_modify_expr(&block, g95_esize_ref(desc),
			    G95_ARRAY_ESIZE(array));

	tmp = init_offset(array, g95_base_ref(desc), NULL_TREE);
	g95_add_modify_expr(&block, g95_offset_ref(desc), tmp);

	break;

    default:
	g95_internal_error("g95_fix_dummy_array(): Bad array spec");
    }

    g95_add_expr_to_block(&block, tmp);
    return g95_finish_block(&block);
}



/* semifull_ref()-- Check an array reference to see if it is
 * "semi-full", ie of the form 'array(:,x)', where the first element
 * is a section with a unity stride and scalar elements for the rest
 * of the elements.  Return nonzero if this is the case. */

static int semifull_ref(g95_array_ref *ar) {
int i;

    if (ar->type != AR_SECTION)
	return 0;

    for(i=1; i<ar->dimen; i++)
	if (ar->dimen_type[i] != DIMEN_ELEMENT)
	    return 0;

    return (ar->stride[0] == NULL ||
	    g95_compare_expr_int(ar->stride[0], 1) == CMP_EQ);
}



/* single_dim_ref()-- Reference a single dimension of an array element
 * reference.  Inserts bounds checking if required. */

static void single_dim_ref(g95_se *se, g95_array_ref *ar, int dim, tree array,
			   g95_array_spec *as, g95_locus *where) {
tree d, t1, t2, tmp;
stmtblock_t block;
g95_se se0;

    g95_init_se(&se0, se);
    g95_conv_expr(&se0, ar->start[dim]);
    se0.expr = convert(g95_pointer_integer, se0.expr);

    if (!g95_option.bounds_check) {
	se->expr = se0.expr;
	return;
    }

    se->expr = save_expr(se0.expr);

    t1 = lbound_value(array, dim);
    t1 = g95_build_lt(se->expr, t1);
    t2 = NULL_TREE;

    if (as->type != AS_ASSUMED_SIZE || dim != as->rank - 1) {
	t2 = ubound_value(array, dim);
	t2 = g95_build_gt(se->expr, t2);
	t1 = g95_build_orif(t1, t2);
    }

    g95_init_block(&block);
    g95_set_locus(&block, where);

    d = g95_build_int(dim+1, 0);

    tmp = (t2 == NULL_TREE)
	? g95_call_library(void_type_node, PREFIX "array_oob1",
			   convert(g95_pointer_integer, se->expr), d,
			   convert(g95_pointer_integer,
				   lbound_value(array, dim)), NULL_TREE)

	: g95_call_library(void_type_node, PREFIX "array_oob2",
			   convert(g95_pointer_integer, se->expr), d,
			   convert(g95_pointer_integer,
				   lbound_value(array, dim)),
			   convert(g95_pointer_integer,
				   ubound_value(array, dim)), NULL_TREE);

    g95_add_expr_to_block(&block, tmp);

    tmp = g95_finish_block(&block);
    tmp = g95_build_cond(void_type_node, t1, tmp, empty_stmt_node);

    /* We have to build a compound expression instead of inserting into
     * se->pre.  Otherwise the check can happen prematurely. */

    se->expr = build2(COMPOUND_EXPR, TREE_TYPE(se0.expr), tmp, se0.expr);
}



/* g95_desc_element_ref()-- Convert an array reference to an array element */

tree g95_desc_element_ref(g95_se *se, g95_array_ref *ar, g95_array_spec *as,
			  g95_locus *where) {
tree array, tmp, pointer, sum;
int i, rank;

    array   = se->expr;
    pointer = g95_offset_ref(G95_ARRAY_DESC(array));
    rank    = ar->dimen;

    sum = convert(g95_pointer_integer, integer_zero_node);

    for(i=0; i<rank; i++) {
	if (ar->dimen_type[i] == DIMEN_ELEMENT || ar->start[i] != NULL)
	    single_dim_ref(se, ar, i, array, as, where);

	else
	    se->expr = lbound_value(array, i);

	tmp = multiplier_value(array, i);
	tmp = convert(g95_pointer_integer, tmp);

	se->expr = convert(g95_pointer_integer, se->expr);

	if (!integer_onep(se->expr))
	    tmp = g95_build_mult(g95_pointer_integer, tmp, se->expr);

	sum = g95_build_plus(g95_pointer_integer, sum, tmp);
    }

    return g95_build_plus(TREE_TYPE(pointer), sum, pointer);
}



/* nondesc_element_ref()-- Reference an array element without the
 * benefit of a descriptor.  This is possible when all array bounds
 * are known in advance.  It's faster than the general case because it
 * avoids references to multipliers held in memory. */

static tree nondesc_element_ref(g95_se *se, g95_array_ref *ar,
				g95_array_spec *as, g95_locus *where) {
tree array, t, type, tmp, storage;
bignum offset, multiplier, e;
int i, rank;

    array = se->expr;
    storage = G95_ARRAY_STORAGE(array);
    type = TREE_TYPE(storage);

    multiplier = bi_1;
    offset = bi_0;
    rank = as->rank;
    tmp = NULL_TREE;

    for(i=0;; i++) {
	if (ar->dimen_type[i] == DIMEN_ELEMENT || ar->start[0] != NULL)
	    single_dim_ref(se, ar, i, array, as, where);

	else
	    se->expr = lbound_value(array, i);

	if (i == 0)
	    tmp = se->expr;

	else {
	    t = bi_to_tree(big_copy(multiplier), -1);
	    t = convert(g95_pointer_integer, t);

	    t = g95_build_mult(g95_pointer_integer, t, se->expr);
	    tmp = g95_build_plus(g95_pointer_integer, t, tmp);
	}

	e = bi_multiply(big_copy(multiplier), as->lower[i]->value.integer);
	offset = bi_add(offset, e);

	if (i == rank-1)
	    break;

	e = bi_subtract(as->upper[i]->value.integer,
			as->lower[i]->value.integer);
	e = bi_add(e, bi_1);

	multiplier = bi_multiply(multiplier, e);
    }

    big_free(multiplier);

    if (bi_compare(big_copy(offset), bi_0) == 0)
	big_free(offset);

    else
	tmp = g95_build_minus(g95_pointer_integer, tmp, bi_to_tree(offset,-1));

    switch(TREE_CODE(type)) {
    case POINTER_TYPE:
	tmp = g95_build_mult(g95_pointer_integer, tmp,
			     convert(g95_pointer_integer,
				     g95_esize_value(array)));

	tmp = convert(g95_pointer_integer, tmp);
	tmp = g95_build_plus(type, storage, tmp);
	break;

    case ARRAY_TYPE:
	type = TREE_TYPE(type);
	/* Fall through */

    default:
	tmp = g95_build_array_ref(type, storage, tmp);
	tmp = g95_addr_expr(tmp);
	break;
    }

    return tmp;
}



/* g95_array_element_ref()-- Reference an array element. */

void g95_array_element_ref(g95_se *se, g95_array_ref *ar, g95_typespec *ts,
			   g95_array_spec *as, int coarray, g95_locus *where) {
tree type, pointer;
int i, flag;

    if (ar->type == AR_FULL) 
	return;

    if (ar->type != AR_ELEMENT && !semifull_ref(ar))
	g95_internal_error("g95_array_element_ref(): Not an element");

    if ((as->type != AS_ASSUMED_SIZE && as->type != AS_EXPLICIT) || coarray)
	flag = 0;

    else {
	flag = 1;

	for(i=0; i<as->rank; i++) {
	    if (as->lower[i]->type != EXPR_CONSTANT)
		flag = 0;

	    if (as->type == AS_ASSUMED_SIZE && i == as->rank - 1)
		continue;

	    if (as->upper[i]->type != EXPR_CONSTANT)
		flag = 0;
	}
    }

    if (G95_ARRAY_STORAGE(se->expr) == NULL_TREE)
	flag = 0;

    if (G95_ARRAY_DUMMY(se->expr) != NULL)
	TREE_USED(G95_ARRAY_DUMMY(se->expr)) = 1;

    pointer = (flag)
	? nondesc_element_ref(se, ar, as, where)
	: g95_desc_element_ref(se, ar, as, where);

    /* Now that we have a pointer to the array element, what happens
     * next depends on the type of the element.  If the element is a
     * simple scalar or derived type, then the pointer is left as is.
     * We can't have arrays within arrays, so only the basic type is
     * important. */

    type = (ts->type == BT_CHARACTER)
	? g95_character1_type_node
	: g95_get_typenode(ts, 0);

    se->expr = convert(build_pointer_type(type), pointer);

    TYPE_REF_CAN_ALIAS_ALL(TREE_TYPE(se->expr)) =
	TYPE_REF_CAN_ALIAS_ALL(TREE_TYPE(pointer));
}



/* g95_set_section_info()-- Generate an instruction that modifies the
 * section_info[] array.  The format of this array is a series of
 * records for each dimension of the original array.  Records start
 * with a word flag that is nonzero for a dimension with an element or
 * zero for a section.  For a dimension with an element, a word
 * follows that gives the element number.  For a dimension with a
 * range, three words follow giving the start, end and stride. */

void g95_set_section_info(stmtblock_t *block, int n, tree value) {
tree tmp;

    tmp = g95_build_array_ref(g95_pointer_integer, section_info,
			      g95_build_int(n, 0));

    value = convert(g95_pointer_integer, value);
    g95_add_modify_expr(block, tmp, value);
}



/* scalar_function()-- Return nonzero if the (scalar numeric) expression
 * contains a function references. */

static int scalar_function(g95_expr *e) {
g95_ref *ref;
int i;

    if (e == NULL)
	return 0;

    switch(e->type) {
    case EXPR_CONSTANT:
	break;

    case EXPR_OP:
	return scalar_function(e->value.op.op1) ||
	       scalar_function(e->value.op.op2);

    case EXPR_FUNCTION:
	return 1;

    case EXPR_VARIABLE:
	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (scalar_function(ref->u.ar.start[i]) ||
			scalar_function(ref->u.ar.end[i]) ||
			scalar_function(ref->u.ar.stride[i]))
			return 1;

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    if (scalar_function(ref->u.car.element[i]))
			return 1;

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		return scalar_function(ref->u.ss.start) ||
		    scalar_function(ref->u.ss.end);
	    }

	break;

    default:
	g95_internal_error("scalar_function(): Bad type");
    }

    return 0;
}



tree g95_build_section_info(g95_expr *e, g95_se *se, stmtblock_t *pre,
			    stmtblock_t *post) {
tree var;

    g95_add_block_to_block(pre,  &se->pre);
    g95_add_block_to_block(post, &se->post);

    if (!scalar_function(e))
	return se->expr;

    var = g95_create_var(g95_pointer_integer);
    g95_add_modify_expr(pre, var, se->expr);

    return var;
}



/* section_ref()-- Convert a section reference.  This amounts to
 * copying the section information into the section_info[] global.
 * Vector subscripts are handled at a higher level and it is an error
 * to see them here. */

static tree section_ref(tree array, g95_array_ref *ref) {
tree info[4*G95_MAX_DIMENSIONS];
stmtblock_t pre, post;
int i, n, d;
g95_se se;

    g95_init_block(&pre);
    g95_init_block(&post);

    n = 0;
    d = 0;

    for(i=0; i<ref->dimen; i++)
	switch(ref->dimen_type[i]) {
	case DIMEN_ELEMENT:
	    info[n++] = integer_one_node;

	    g95_init_se(&se, NULL);
	    g95_conv_expr0(&se, ref->start[i]);

	    info[n++] = g95_build_section_info(ref->start[i], &se, &pre,&post);
	    break;

	case DIMEN_RANGE:
	    d++;
	    info[n++] = integer_zero_node;

	    if (ref->start[i] == NULL)
		info[n++] = lbound_value(array, i);

	    else {
		g95_init_se(&se, NULL);
		g95_conv_expr0(&se, ref->start[i]);

		info[n++] =
		    g95_build_section_info(ref->start[i], &se, &pre, &post);
	    }

	    if (ref->end[i] == NULL)
		info[n++] = ubound_value(array, i);

	    else {
		g95_init_se(&se, NULL);
		g95_conv_expr0(&se, ref->end[i]);

		info[n++] =
		    g95_build_section_info(ref->end[i], &se, &pre, &post);
	    }

	    if (ref->stride[i] == NULL)
		info[n++] = integer_one_node;

	    else {
		g95_init_se(&se, NULL);
		g95_conv_expr0(&se, ref->stride[i]);

		info[n++] =
		    g95_build_section_info(ref->stride[i], &se, &pre, &post);
	    }

	    break;

	default:
	    g95_internal_error("section_ref(): Bad dimension type");
	}

    for(i=0; i<n; i++)
	g95_set_section_info(&pre, i, info[i]);

    g95_add_expr_to_block(&pre, g95_finish_block(&post));

    return g95_finish_block(&pre);
}



/* simple_array_ref()-- Compute an array reference in the form of an
 * integer offset in bytes.  The array is guaranteed to be explicit. */

static tree simple_array_ref(g95_array_spec *as, g95_array_ref *ar,
			     g95_typespec *ts, g95_se *se) {
tree offset, upper, lower, mult, tmp;
g95_se se0;
int i;

    offset = integer_zero_node;
    mult = g95_ts_size(ts);

    for(i=0; i<as->rank; i++) {
	g95_init_se(&se0, se);
	g95_conv_constant(&se0, as->upper[i]);
	upper = se0.expr;

	g95_init_se(&se0, se);
	g95_conv_constant(&se0, as->lower[i]);
	lower = se0.expr;

	g95_init_se(&se0, se);
	g95_conv_expr(&se0, ar->start[i]);

	tmp = g95_build_minus(g95_pointer_integer, se0.expr, lower);
	tmp = g95_build_mult(g95_pointer_integer, tmp, mult);

	offset = g95_build_plus(g95_pointer_integer, offset, tmp);

	if (i != as->rank - 1) {
	    tmp = g95_build_minus(g95_pointer_integer, upper, lower);
	    tmp = g95_build_plus(g95_pointer_integer, tmp, integer_one_node);
	    tmp = g95_build_max(g95_pointer_integer, integer_zero_node, tmp);

	    mult = g95_build_mult(g95_pointer_integer, tmp, mult);
	}
    }

    return offset;
}



/* post_refs()-- Account for part-refs that follow an array section.
 * Returns the value of the total offset.  For character substrings,
 * this can also involve changing the string length. */

static tree post_refs(g95_se *se, tree array, g95_expr *e, g95_ref *resume,
		      tree *esize) {
tree offset, tmp, start, end, size, field;
g95_array_spec *as;
g95_typespec *ts;
g95_component *c;
g95_ref *ref;
g95_se se0;
int flag;

    offset = integer_zero_node;
    g95_init_se(&se0, se);

    ts = &e->symbol->ts;
    as = e->symbol->as;
    size = NULL_TREE;
    flag = 0;

    for(ref=e->ref; ref!=NULL; ref=ref->next) {
	if (ref == resume) {
	    flag = 1;
	    continue;
	}

	if (!flag)
	    continue;

	switch(ref->type) {
	case REF_ARRAY:
	    tmp = simple_array_ref(as, &ref->u.ar, ts, &se0);
	    offset = g95_build_plus(g95_pointer_integer, offset, tmp);
	    break;

	case REF_COMPONENT:
	    c = ref->u.c.component;

	    field = c->backend_decl;
	    if (c->dimension)
		field = G95_ARRAY_FIELD(field);

	    offset = g95_build_plus(TREE_TYPE(offset), offset,
				    byte_position(field));

	    ts = &c->ts;
	    as = c->as;
	    size = g95_ts_size(ts);
	    break;

	case REF_SUBSTRING:
	    if (ref->u.ss.start == NULL)
		start = integer_one_node;

	    else {
		g95_conv_expr(&se0, ref->u.ss.start);
		start = se0.expr;
	    }

	    start = save_expr(start);

	    if (ref->u.ss.end == NULL)
		end = (ts == NULL)
		    ? g95_esize_value(array)
		    : ts->cl->backend_decl;

	    else {
		g95_conv_expr(&se0, ref->u.ss.end);
		end = se0.expr;
	    }

	    offset = g95_build_plus(g95_pointer_integer, offset, start);
	    offset = g95_build_minus(g95_pointer_integer, offset,
				     integer_one_node);

	    tmp  = g95_build_minus(g95_pointer_integer, end, start);
	    tmp  = g95_build_plus(g95_pointer_integer, tmp, integer_one_node);
	    size = g95_build_max(g95_pointer_integer, tmp, integer_zero_node);
	    break;

	case REF_COARRAY:
	    break;

	default:
	    g95_internal_error("post_ref(): Bad ref");
	}
    }

    if (size != NULL)
	*esize = save_expr(size);

    if (ts != NULL && ts->type == BT_CHARACTER)
	se->string_length = (size != NULL)
	    ? size
	    : ts->cl->backend_decl;

    return offset;
}



/* array_section()-- Convert all of an array section expressions
 * except for the section itself.  We return the array node, an offset
 * and set a pointer to the section reference. */

static tree array_section(g95_se *se, g95_expr *e, g95_ref **section,
			  g95_array_spec **as, tree *offset, int *coref,
			  tree *size) {
g95_ref *ref, *prev;
g95_se se0;
tree array;

    prev   = NULL;
    ref    = e->ref;

    *as    = e->symbol->as;
    *coref = (e->symbol->cas != NULL);

    while(ref->type != REF_ARRAY ||
	  (ref->u.ar.type != AR_FULL && ref->u.ar.type != AR_SECTION)) {

	if (ref->type == REF_COMPONENT)
	    *as = ref->u.c.component->as;

	prev = ref;
	ref = ref->next;
    }

    *section = ref;

    if (prev == NULL)
	e->ref = NULL;

    else
	prev->next = NULL;

    g95_init_se(&se0, se);
    g95_conv_expr(&se0, e);
    array = se0.expr;

    if (prev == NULL)
	e->ref = ref;

    else
	prev->next = ref;

    *size = NULL_TREE;
    *offset = (ref->next == NULL)
	? integer_zero_node
	: post_refs(&se0, array, e, ref, size);

    g95_raise_chains(&se0);

    se->expr          = se0.expr;
    se->string_length = se0.string_length;

    return array;
}



/* full_section()-- Return nonzero if the section reference is really
 * a full reference. */

static int full_section(g95_array_ref *ar) {
int i; 

    for(i=0; i<ar->dimen; i++)
	if (ar->start[i] != NULL || ar->end[i] != NULL ||
	    (ar->stride[i] != NULL &&
	     (ar->stride[i]->type != EXPR_CONSTANT ||
	      bi_compare(ar->stride[i]->value.integer, bi_1) != 0)))
	    return 0;

    return 1;
}



/* slice_array()-- Slice an array from an old to a new.  In the
 * general case, we have to call section_array().  In the special case
 * consisting of one or more full sections followed by zero or more
 * elements, the array descriptor can be mostly copied. */

static void slice_array(stmtblock_t *block, tree array, g95_array_ref *ar,
			tree src, tree dest, int assumed_size,
			int section_flag) {
tree tmp, offset;
int i, rank;
g95_se se;

    if (section_flag || ar->dimen_type[0] != DIMEN_RANGE ||
	ar->start[0] != NULL || ar->end[0] != NULL || ar->stride[0] != NULL)
	goto general;

    rank = 1;

    while(rank < ar->dimen && ar->dimen_type[rank] == DIMEN_RANGE &&
	  ar->start[rank] == NULL && ar->end[rank] == NULL &&
	  ar->stride[rank] == NULL)
	rank++;

    i = rank;
    while(i < ar->dimen && ar->dimen_type[i] == DIMEN_ELEMENT)
	i++;

    if (i != ar->dimen)
	goto general;

    /* Special case: copy the descriptor.  The offset is the old
     * offset plus the trailing elements times the corresponding
     * multipliers. */

    g95_add_modify_expr(block, g95_rank_ref(dest),  g95_build_int(rank, 0));
    g95_add_modify_expr(block, g95_esize_ref(dest), g95_esize_ref(src));
    g95_add_modify_expr(block, g95_base_ref(dest),  g95_base_ref(src));

    for(i=0; i<rank; i++) {
	g95_add_modify_expr(block, g95_multiplier_ref(dest, i),
			    g95_multiplier_ref(src, i));

	g95_add_modify_expr(block, g95_lbound_ref(dest, i),
			    g95_lbound_ref(src, i));

	g95_add_modify_expr(block, g95_ubound_ref(dest, i),
			    g95_ubound_ref(src, i));
    }

    offset = g95_offset_ref(src);

    g95_init_se(&se, NULL);

    for(; i<ar->dimen; i++) {
	g95_conv_expr(&se, ar->start[i]);

	tmp = g95_multiplier_ref(src, i);
	tmp = g95_build_mult(g95_pointer_integer, se.expr, tmp);

	offset = g95_build_plus(TREE_TYPE(offset), tmp, offset);
    }

    g95_add_block_to_block(block, &se.pre);
    g95_add_modify_expr(block, g95_offset_ref(dest), offset);
    g95_add_block_to_block(block, &se.post);

    return;

general:
    tmp = section_ref(array, ar);
    g95_add_expr_to_block(block, tmp);

    tmp = g95_call_library(void_type_node, PREFIX "section_array",
			   g95_desc_addr(src),
			   g95_desc_addr(dest),
			   assumed_size ? integer_one_node : integer_zero_node,
			   NULL_TREE);

    g95_add_expr_to_block(block, tmp);
}



/* g95_conv_descriptor()-- Convert an expression that ultimately boils
 * down to an array descriptor.  May be passed a descriptor where the
 * result should be stored.  If section_flag is set, a full explicit
 * array section (a(:)) is always passed through section_array().  If
 * not set, we can optimize by passing the original array. */

void g95_conv_descriptor(g95_se *se, g95_expr *e, int section_flag) {
tree array, offset, desc, tmp, t, size, src;
int assumed_size, rank, i, coref;
g95_array_spec *as;
g95_ref *section;

    if (e->type == EXPR_NULL ||
	(e->type == EXPR_ARRAY && e->value.constructor.c == NULL)) {
	se->expr = g95_empty_array;
	return;
    }

    desc = NULL;

    if (e->rank == 0 && g95_coarray_expr(e)) {  /* Scalar coarray */
	i = e->rank;
	e->rank = 1;

	g95_conv_expr(se, e);
	se->expr = G95_ARRAY_DESC(se->expr);

	e->rank = i;
	goto done;
    }

    array = array_section(se, e, &section, &as, &offset, &coref, &size);

    if (G95_ARRAY(array) && G95_ARRAY_DUMMY(array) != NULL)
	TREE_USED(G95_ARRAY_DUMMY(array)) = 1;

    /* Special do-nothing case */

    if (!g95_coindexed_expr(e) && section->u.ar.type == AR_FULL &&
	section->next == NULL  && G95_ARRAY_DESC(array) != NULL_TREE) {
	se->expr = desc = G95_ARRAY_DESC(array);
	goto done;
    }

    if (g95_coindexed_expr(e)) {
	desc = src = se->expr;

    } else if (G95_ARRAY_DESC(array) != NULL)
	desc = src = G95_ARRAY_DESC(array);

    else {
	rank = (as == NULL) ? e->rank : as->rank;
	desc = src = g95_create_var(g95_get_array_desc(rank, 0));

	rank = get_rank(array);
	for(i=0; i<rank; i++)
	    if (G95_ARRAY_MULT(array, i) == NULL) {
		get_multipliers(array, as);
		break;
	    }

	g95_init_descriptor(se, array, desc, offset, se->string_length);
	offset = integer_zero_node;
    }

    if (section->u.ar.type == AR_SECTION &&
	(section_flag || !full_section(&section->u.ar))) {

	desc = g95_create_var(g95_get_array_desc(e->rank, 0));

	assumed_size = (e->type == EXPR_VARIABLE &&
			e->symbol->as != NULL &&
			e->symbol->as->type == AS_ASSUMED_SIZE);

	slice_array(&se->pre, array, &section->u.ar, src, desc, assumed_size, 
		    section_flag);
    }

    if (!integer_zerop(offset)) {
	if (src == desc) {
	    desc = g95_create_var(g95_get_array_desc(e->rank, 0));

	    se->expr = src;
	    g95_reflevel(se, 0);

	    g95_add_modify_expr(&se->pre, desc, se->expr);
	}

	tmp = g95_offset_ref(desc);
	t = g95_build_plus(pchar_type_node, tmp, offset);

	g95_add_modify_expr(&se->pre, tmp, t);
    }

    if (size != NULL_TREE)
	g95_add_modify_expr(&se->pre, g95_esize_ref(desc), size);

    se->expr = desc;

done:
    if (e->ts.type == BT_CHARACTER && se->string_length == NULL_TREE)
	se->string_length = g95_esize_value(desc);

    g95_reflevel(se, se->reflevel);
    g95_raise_chains(se);
}



/* g95_contiguous()-- Return nonzero if an array expression is guaranteed
 * to be contiguous. */

int g95_contiguous(g95_expr *e) {
symbol_attribute *attr;
g95_component *c;
g95_ref *ref;
int flag;

    if (e->type != EXPR_VARIABLE)
	return 0;

    attr = &e->symbol->attr;

    if (attr->pointer)
	flag = 0;

    else if (attr->allocatable || e->symbol->cas != NULL)
	flag = 1;

    else if (attr->dimension && attr->dummy &&
	     e->symbol->as->type == AS_ASSUMED_SHAPE)
	flag = 0;

    else
	flag = 1;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type == AR_ELEMENT)
		break;

	    if (ref->next != NULL)
		return 0;

	    if (ref->u.ar.type == AR_FULL)
		break;

	    return semifull_ref(&ref->u.ar);

	case REF_COMPONENT:
	    c = ref->u.c.component;
	    if (c->dimension)
		flag = !c->pointer;

	    break;

	case REF_COARRAY:
	    flag = 1;
	    break;

	case REF_SUBSTRING:
	    break;
	}

    return flag;
}


/* contiguous_array()-- Call the non-section version
 * contiguous_array() subroutine to create a contiguous array. */

static void contiguous_array(g95_se *se, g95_intent intent, tree desc,
			     tree length) {
tree tmp, var1, var2;

    g95_set_section_info(&se->pre, 0, integer_zero_node);

    var1 = g95_create_var(pchar_type_node);
    TREE_ADDRESSABLE(var1) = 1;
    tmp = g95_addr_expr(var1);

    if (length != null_pointer_node)
	length = g95_addr_expr(length);

    tmp = g95_call_library(pvoid_type_node, PREFIX "contiguous_array",
			   g95_desc_addr(desc), tmp, length, NULL_TREE);

    var2 = g95_create_var(pvoid_type_node);
    g95_add_modify_expr(&se->pre, var2, tmp);

    se->expr = var2;

    tmp = (intent == INTENT_IN)
	? integer_zero_node
	: integer_one_node;

    tmp = g95_call_library(void_type_node, PREFIX "contiguous_array_done",
			   var1, tmp, NULL_TREE);

    g95_add_se_expr(se, NULL, tmp);
}



/* g95_conv_nondesc()-- Convert an expression that boils down to a
 * pointer to a contiguous array. */

void g95_conv_nondesc(g95_se *se, g95_intent intent, g95_expr *e, tree length){
tree desc, dest, array, offset, tmp, type, size, a;
int i, cflag, flag, coref, assumed_size;
g95_array_spec *as;
g95_expr *stride;
g95_ref *section;

    if (e->type == EXPR_ARRAY && e->value.constructor.c == NULL) {
	se->expr = g95_empty_array;
	return;
    }

    array = array_section(se, e, &section, &as, &offset, &coref, &size);
    desc = NULL_TREE;
    flag = 1;
    cflag = g95_contiguous(e);

    if (section->u.ar.type == AR_FULL && section->next == NULL) {
	/* Easy cases*/
	if (G95_ARRAY_STORAGE(array) != NULL_TREE &&
	    G95_ARRAY_DESC(array) == NULL_TREE && !G95_COARRAY(array)) {

	    se->expr = G95_ARRAY_STORAGE(array);
	    goto done;
	}

	if (cflag) {
	    se->expr = g95_base_value(array);
	    goto done;
	}
    }

    if (cflag && semifull_ref(&section->u.ar) && section->next == NULL) {
	g95_array_element_ref(se, &section->u.ar, &e->ts, as, coref,
			      &section->where);
	goto done;
    }

    type = g95_get_array_desc((as == NULL) ? e->rank : as->rank, 0);

    if (G95_ARRAY_DESC(array) == NULL_TREE) {
	desc = g95_create_var(type);
	g95_init_descriptor(se, array, desc, offset, se->string_length);

    } else {
	if (integer_zerop(offset))
	    desc = G95_ARRAY_DESC(array);

	else {
	    desc = g95_create_var(type);

	    a = G95_ARRAY_DESC(array);
	    if (POINTER_TYPE_P(TREE_TYPE(a)))
		a = g95_build_indirect(TREE_TYPE(TREE_TYPE(a)), a);

	    g95_add_modify_expr(&se->pre, desc, a);

	    tmp = g95_offset_ref(desc);
	    g95_add_modify_expr(&se->pre, tmp,
				g95_build_plus(pchar_type_node, tmp, offset));
	}
    }

    if (section->u.ar.type == AR_SECTION) {
	dest = (desc == G95_ARRAY_DESC(array))
	    ? g95_create_var(type)
	    : desc;

	assumed_size = (e->type == EXPR_VARIABLE &&
			e->symbol->as != NULL &&
			e->symbol->as->type == AS_ASSUMED_SIZE);

	slice_array(&se->pre, array, &section->u.ar, desc, dest,
		    assumed_size, 0);
	desc = dest;

	/* If any of the strides are non-unity, the array can't be
	 * contiguous any longer. */

	/* Doing the below uncovers some other bugs, section_array()
	 * needs to be able to handle simple stride-1 sections by
	 * itself. */

	cflag = 0;  /* hack, get rid of me */

	for(i=0; i<section->u.ar.dimen; i++) {
	    if (section->u.ar.dimen_type[i] == DIMEN_VECTOR) {
		cflag = 0;
		break;
	    }

	    stride = section->u.ar.stride[i];
	    if (stride == NULL || g95_compare_expr_int(stride, 1) == CMP_EQ)
		continue;

	    cflag = 0;
	    break;
	}
    }

    if (size != NULL_TREE)
	g95_add_modify_expr(&se->pre, g95_esize_ref(desc), size);

    if (cflag)
	se->expr = g95_base_ref(desc);

    else
	contiguous_array(se, intent, desc, length);

    flag = 0;

done:
    if (e->ts.type == BT_CHARACTER && se->string_length == NULL_TREE)
	se->string_length = g95_esize_ref(desc);

    if (flag && length != null_pointer_node) {
	tmp = (G95_ARRAY_ESIZE(array) == NULL_TREE ||
	       G95_ARRAY_SIZE(array) == NULL_TREE)
	    ? g95_build_int(1000000, 0)
	    : g95_build_mult(g95_pointer_integer, G95_ARRAY_ESIZE(array),
			     G95_ARRAY_SIZE(array));

	g95_add_modify_expr(&se->pre, length, tmp);
    }

    g95_reflevel(se, se->reflevel);
    g95_raise_chains(se);
}



/* pass_array_element()-- This is a special case of passing an array
 * base pointer, but without the possibility of passing a section,
 * which simplifies things immensely. */

static void pass_array_element(g95_se *se, g95_actual_arglist *arg) {
tree array, p, q, multiplier, extent, tmp;
int reflevel, i, rank;
g95_ref *ref, *prev;
g95_array_spec *as;
g95_typespec *ts;
g95_expr *e;

    e = arg->u.expr; 
    reflevel = (arg->cb == CB_VALUE) ? 0 : 1;

    if (g95_contiguous(e)) {
	se->reflevel = reflevel;
	g95_conv_expr(se, e);
	return;
    }

    /* Pain-in-the-butt case: Evaluate the full array, make it
     * contiguous in a possibly different block of memory, then compute
     * the final array reference against the new block. */

    ref  = e->ref;
    as   = e->symbol->as;
    ts   = &e->symbol->ts;
    prev = NULL;

    while(ref->next != NULL) {
	if (ref->type == REF_COMPONENT) {
	    as = ref->u.c.component->as;
	    ts = &ref->u.c.component->ts;
	}

	prev = ref;
	ref  = ref->next;
    }

    if (prev == NULL)
	e->ref = NULL;
    else
	prev->next = NULL;

    se->reflevel = 0;
    g95_conv_expr(se, e);

    if (prev == NULL)
	e->ref = ref;
    else
	prev->next = ref;

    array = se->expr;
    contiguous_array(se, arg->intent, G95_ARRAY_DESC(array),
		     null_pointer_node);

    /* Reference the element.  We always have an array descriptor
     * because of the non-contiguous array.  The multipliers aren't
     * necessarily correct for the new array, so we are on our own. */

    p = integer_zero_node;
    q = se->expr;
    multiplier = g95_esize_value(array);
    rank = ref->u.ar.dimen;

    for(i=0; i<rank; i++) {
	single_dim_ref(se, &ref->u.ar, i, array, as, &e->where);

	tmp = g95_build_minus(g95_pointer_integer, se->expr,
			      lbound_value(array, i));
	tmp = g95_build_mult(g95_pointer_integer, multiplier, tmp);
	p = g95_build_plus(g95_pointer_integer, p, tmp);

	if (i == rank-1)
	    break;

	extent = g95_build_minus(g95_pointer_integer, ubound_value(array, i),
				 lbound_value(array, i));
	extent = g95_build_plus(g95_pointer_integer, extent, integer_one_node);

	multiplier = g95_build_mult(g95_pointer_integer, multiplier, extent);
	multiplier = save_expr(multiplier);
    }

    se->expr = g95_build_plus(pvoid_type_node, q, p);

    g95_reflevel(se, reflevel);
}



/* g95_pointer_array_assignment()-- Array pointer assignment */

void g95_pointer_array_assignment(g95_code *code, stmtblock_t *block) {
g95_se left, right;

    g95_init_se(&left, NULL); 
    g95_init_se(&right, NULL);

    left.reflevel = 0;
    right.reflevel = 0;

    g95_conv_descriptor(&left, code->expr, 0);

    if (code->expr2->type == EXPR_VARIABLE)
	g95_conv_descriptor(&right, code->expr2, 1);
    else
	g95_conv_expr(&right, code->expr2);

    g95_add_modify_expr(&right.pre, left.expr, right.expr);

    g95_add_block_to_block(block, &left.pre);
    g95_add_block_to_block(block, &right.pre);

    g95_add_block_to_block(block, &left.post);
    g95_add_block_to_block(block, &right.post);
}



/* desc_array_argument()-- Pass an assumed-shape array */

static void desc_array_argument(g95_se *se, g95_expr *expr) {
g95_symbol *sym;
tree decl, tmp;

    if (expr->type == EXPR_OP && expr->value.op.operator == INTRINSIC_PAREN)
	expr = expr->value.op.op1;

    se->reflevel = 1; 
    g95_conv_descriptor(se, expr, 0);

    if (se->expr == g95_empty_array)
	return;

    sym = expr->symbol;

    /* If the argument is itself an assumed shape array, then we have
     * to see if it is present before passing the local descriptor. */

    decl = G95_ARRAY_DUMMY(sym->backend_decl);

    if (expr->type == EXPR_VARIABLE && sym->attr.dummy &&
	sym->attr.optional && expr->ref != NULL &&
	expr->ref->type == REF_ARRAY && expr->ref->next == NULL) {

	tmp = g95_build_eq(decl, null_pointer_node);
	se->expr = g95_build_cond(pvoid_type_node, tmp, null_pointer_node,
				  se->expr);
    }
}



/* g95_array_argument()-- Convert an actual argument that is an array
 * or array pointer. */

void g95_array_argument(g95_se *se, g95_actual_arglist *arg) {

    if (arg->pointer) {
	se->reflevel = 1;
	g95_conv_descriptor(se, arg->u.expr, 0);

    } else if (arg->type == ARG_ARRAY_ELEMENT)
	pass_array_element(se, arg);

    else if (arg->type == ARG_ARRAY_DESC)
	desc_array_argument(se, arg->u.expr);

    else {
	se->reflevel = 1;
	g95_conv_nondesc(se, arg->intent, arg->u.expr, null_pointer_node);
    }
}



