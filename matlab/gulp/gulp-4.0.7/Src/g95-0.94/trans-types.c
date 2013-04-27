/* Backend support for Fortran 95 basic types and derived types.
   Copyright (C) 2002-2008 Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* trans-types.c -- g95 backend types */

#include "trans.h"

tree g95_type_nodes[NUM_F95_TYPES];

tree pvoid_type_node, ppvoid_type_node, pchar_type_node, pchar_type_node_a;
tree g95_default_integer, g95_pointer_integer, integer_zero_unsigned;
tree x87_type_node, quad_type_node;
tree x87_complex_type_node, quad_complex_type_node;
tree pfunc_type_node;


static struct {
    int count;
    tree map, type, offset_field, count_field, size_field, pointer_field,
         coarray_field;
} alloc;



/* g95_init_types()-- Create the backend type nodes. We map them to
 * their equivalent C type, at least for now.  We also give names to
 * the types here, and we push them in the global binding level
 * context.*/

void g95_init_types(void) {
unsigned HOST_WIDE_INT n;

    /* Name the types.  */
#define PUSH_TYPE(name, node) \
    pushdecl(build_decl(TYPE_DECL, get_identifier(name), node))

    g95_int1_type_node = signed_char_type_node;
    PUSH_TYPE("int1", g95_int1_type_node);

    g95_int2_type_node = short_integer_type_node;
    PUSH_TYPE("int2", g95_int2_type_node);

    g95_int4_type_node = g95_type_for_size(32, 0 /*unsigned*/);
    PUSH_TYPE("int4", g95_int4_type_node);

    g95_int8_type_node = g95_type_for_size(64, 0 /*unsigned*/);
    PUSH_TYPE ("int8", g95_int8_type_node);

#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    g95_int16_type_node = g95_type_for_size (128, 0 /*unsigned*/);
    PUSH_TYPE ("int16", g95_int16_type_node);
#endif

    switch(g95_default_integer_kind(0)) {
    case 4: g95_default_integer = g95_int4_type_node;  break;
    case 8: g95_default_integer = g95_int8_type_node;  break;
    default:
	g95_internal_error("g95_init_types(): Bad default integer");
    }

    g95_real4_type_node = float_type_node;
    PUSH_TYPE("real4", g95_real4_type_node);

    g95_real8_type_node = double_type_node;
    PUSH_TYPE("real8", g95_real8_type_node);

#if defined(FPU_387)
    x87_type_node = make_node(REAL_TYPE);
    TYPE_PRECISION(x87_type_node) = 10;
    layout_type(x87_type_node);
    TYPE_MODE(x87_type_node) = XFmode;
    TYPE_SIZE(x87_type_node) = convert(bitsizetype, g95_build_int(96, 0));
    TYPE_SIZE_UNIT(x87_type_node) = convert(sizetype, g95_build_int(12, 0));
    REAL_MODE_FORMAT(XFmode) = &ieee_extended_intel_96_format;

    g95_real10_type_node = x87_type_node;
    PUSH_TYPE("real10", g95_real10_type_node);

#elif defined(FPU_SSE)
    x87_type_node = make_node(REAL_TYPE);
    TYPE_PRECISION(x87_type_node) = 10;
    layout_type(x87_type_node);
    TYPE_MODE(x87_type_node) = XFmode;
    TYPE_SIZE(x87_type_node) = convert(bitsizetype, g95_build_int(128, 0));
    TYPE_SIZE_UNIT(x87_type_node) = convert(sizetype, g95_build_int(16, 0));
    REAL_MODE_FORMAT(XFmode) = &ieee_extended_intel_96_format;

    g95_real10_type_node = x87_type_node;
    PUSH_TYPE("real10", g95_real10_type_node);
#endif

    quad_type_node = make_node(REAL_TYPE);
    TYPE_PRECISION(quad_type_node) = 16;
    layout_type(quad_type_node);
    TYPE_MODE(quad_type_node) = TFmode;
    TYPE_SIZE(quad_type_node) = convert(bitsizetype, g95_build_int(128, 0));
    TYPE_SIZE_UNIT(quad_type_node) = convert(sizetype, g95_build_int(16, 0));

    REAL_MODE_FORMAT(TFmode) = &ieee_quad_format;

    g95_real16_type_node = quad_type_node;
    PUSH_TYPE("real16", g95_real16_type_node);

    g95_complex4_type_node = complex_float_type_node;
    PUSH_TYPE("complex4", g95_complex4_type_node);

    g95_complex8_type_node = complex_double_type_node;
    PUSH_TYPE("complex8", g95_complex8_type_node);

#if defined(FPU_387)
    x87_complex_type_node = make_node(COMPLEX_TYPE);
    TREE_TYPE(x87_complex_type_node) = x87_type_node;
    TYPE_SIZE(x87_complex_type_node) =
	convert(bitsizetype, g95_build_int(160, 0));
    TYPE_SIZE_UNIT(x87_complex_type_node) =
	convert(sizetype, g95_build_int(24, 0));

    TYPE_MODE(x87_complex_type_node) = XCmode;

    layout_type(x87_complex_type_node);
    PUSH_TYPE("complex10", x87_complex_type_node);

#elif defined(FPU_SSE)
    x87_complex_type_node = make_node(COMPLEX_TYPE);
    TREE_TYPE(x87_complex_type_node) = x87_type_node;
    TYPE_SIZE(x87_complex_type_node) =
	convert(bitsizetype, g95_build_int(192, 0));
    TYPE_SIZE_UNIT(x87_complex_type_node) =
	convert(sizetype, g95_build_int(32, 0));

    TYPE_MODE(x87_complex_type_node) = XCmode;

    layout_type(x87_complex_type_node);
    PUSH_TYPE("complex10", x87_complex_type_node);
#endif

    quad_complex_type_node = make_node(COMPLEX_TYPE);
    TREE_TYPE(quad_complex_type_node) = quad_type_node;
    layout_type(quad_complex_type_node);

    quad_complex_type_node->type.mode = TCmode;   /* Work around gcc bug */
    PUSH_TYPE("complex16", quad_complex_type_node);

    g95_logical1_type_node = make_node(BOOLEAN_TYPE);
    TYPE_PRECISION(g95_logical1_type_node) = 8;
    fixup_unsigned_type(g95_logical1_type_node);
    PUSH_TYPE("logical1", g95_logical1_type_node);

    g95_logical2_type_node = make_node(BOOLEAN_TYPE);
    TYPE_PRECISION(g95_logical2_type_node) = 16;
    fixup_unsigned_type(g95_logical2_type_node);
    PUSH_TYPE("logical2", g95_logical2_type_node);

    g95_logical4_type_node = make_node(BOOLEAN_TYPE);
    TYPE_PRECISION(g95_logical4_type_node) = 32;
    fixup_unsigned_type(g95_logical4_type_node);
    PUSH_TYPE("logical4", g95_logical4_type_node);

    g95_logical8_type_node = make_node(BOOLEAN_TYPE);
    TYPE_PRECISION(g95_logical8_type_node) = 64;
    fixup_unsigned_type(g95_logical8_type_node);
    PUSH_TYPE("logical8", g95_logical8_type_node);

#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    g95_logical16_type_node = make_node(BOOLEAN_TYPE);
    TYPE_PRECISION (g95_logical16_type_node) = 128;
    fixup_unsigned_type (g95_logical16_type_node);
    PUSH_TYPE ("logical16", g95_logical16_type_node);
#endif

    g95_character1_type_node =
	build_type_variant (signed_char_type_node, 0, 0);
    PUSH_TYPE("char", g95_character1_type_node);

    PUSH_TYPE("byte", unsigned_char_type_node);

    PUSH_TYPE("void", void_type_node);

  /* DBX debugging output gets upset if these aren't set.  */
    if (!TYPE_NAME(integer_type_node))
	PUSH_TYPE("c_integer", integer_type_node);

    if (!TYPE_NAME(char_type_node))
	PUSH_TYPE("c_char", char_type_node);
#undef PUSH_TYPE

    pvoid_type_node  = build_pointer_type(void_type_node);
    ppvoid_type_node = build_pointer_type(pvoid_type_node);
    pchar_type_node  = build_pointer_type(g95_character1_type_node);

    pchar_type_node_a = build_pointer_type(g95_character1_type_node);
    TYPE_REF_CAN_ALIAS_ALL(pchar_type_node_a) = 1;

    pfunc_type_node =
	build_pointer_type(build_function_type(void_type_node, NULL_TREE));

    n = TREE_INT_CST_LOW(TYPE_SIZE(g95_default_integer));
    if (n > sizeof(HOST_WIDE_INT)*8)
	n = sizeof(HOST_WIDE_INT)*8;

    n += G95_DTYPE_SIZE_SHIFT;
    size_type_node = g95_default_integer;

    /* Get the internal boolean_type_node from the corresponding integer
     * kind, to avoid the effects of -i8. */

    boolean_type_node  = g95_get_logical_type(g95_default_integer_kind(0));
    boolean_false_node = TYPE_MIN_VALUE(boolean_type_node);
    boolean_true_node  = TYPE_MAX_VALUE(boolean_type_node);
    TREE_INT_CST_LOW(boolean_true_node) = 1;

    integer_one_node = convert(g95_default_integer, integer_one_node);
    integer_zero_node = convert(g95_default_integer, integer_zero_node);
    integer_minus_one_node =
	convert(g95_default_integer, integer_minus_one_node);

    integer_zero_unsigned = convert(g95_unsigned_type(g95_default_integer),
				    integer_zero_node);

    switch(int_size_in_bytes(pvoid_type_node)) {
    case 4: g95_pointer_integer = g95_int4_type_node; break;
    case 8: g95_pointer_integer = g95_int8_type_node; break;
    default:
	g95_internal_error("g95_init_types(): Bad pointer size");
    }

    alloc.type = make_node(RECORD_TYPE);

    alloc.offset_field =
	g95_add_field(alloc.type, "offset",  g95_default_integer);

    alloc.count_field =
	g95_add_field(alloc.type, "count",   g95_default_integer);

    alloc.size_field =
	g95_add_field(alloc.type, "size",    g95_default_integer);

    alloc.coarray_field =
	g95_add_field(alloc.type, "coarray", g95_default_integer);

    alloc.pointer_field =
	g95_add_field(alloc.type, "pointer", pvoid_type_node);

    g95_finish_type(alloc.type);

    g95_space = g95_build_int(' ', 0);
}



/* alloc_constructor()-- Build an allocation constructor */

static void alloc_constructor(tree offset, tree count, tree size, tree decl,
			      tree coarray) {
tree node, cons;

    node = tree_cons(alloc.offset_field,   offset,   NULL_TREE);
    node = tree_cons(alloc.count_field,    count,    node);
    node = tree_cons(alloc.size_field,     size,     node);
    node = tree_cons(alloc.coarray_field,  coarray,  node);
    node = tree_cons(alloc.pointer_field,  decl,     node);

    cons = g95_build_constructor(alloc.type, nreverse(node));
    alloc.map = tree_cons(g95_build_int(alloc.count++, 0), cons, alloc.map);
}



/* build_alloc_map0()-- Build a map for a single derived type. */

static void build_alloc_map0(g95_component *c) {
tree decl, count, size, offset, coarray;

    for(; c; c=c->next) {
	decl = c->backend_decl;
	if (c->as != NULL || c->cas != NULL)
	    decl = G95_ARRAY_FIELD(decl);

	offset = byte_position(decl);

	if (c->allocatable && (c->as != NULL || c->cas != NULL)) {
	    decl = (c->ts.type == BT_DERIVED &&
		    G95_DTYPE_ALLOCS(c->ts.derived->backend_decl))
		? G95_DTYPE_ALLOCS(c->ts.derived->backend_decl)
		: null_pointer_node;

	    count = integer_minus_one_node;
	    size = integer_zero_node;

	    coarray = (c->cas != NULL)
		? integer_one_node
		: integer_zero_node;

	    alloc_constructor(offset, count, size, decl, coarray);
	}

	if (c->ts.type == BT_DERIVED && !c->pointer && !c->allocatable &&
	    g95_allocatable_component(c->ts.derived)) {

	    decl = G95_DTYPE_ALLOCS(c->ts.derived->backend_decl);
	    count = (c->as == NULL)
		? integer_one_node
		: bi_to_tree(g95_array_spec_size(c->as), -1);

	    size = size_in_bytes(G95_DTYPE_TYPE(c->ts.derived->backend_decl));
	    coarray = integer_zero_node;

	    alloc_constructor(offset, count, size, decl, coarray);
	}
    }
}



/* build_alloc_map()-- Find all of the allocatable arrays within a
 * and any sub-types. */

static void build_alloc_map(g95_symbol *derived) {
tree var, tmp;

    if (!g95_allocatable_component(derived))
	return;

    alloc.count = 0;
    alloc.map   = NULL_TREE;

    build_alloc_map0(derived->components);

    alloc_constructor(integer_minus_one_node, integer_zero_node,
		      integer_zero_node, null_pointer_node, integer_zero_node);

    tmp = build_index_type(g95_build_int(alloc.count-1, 0));
    tmp = build_array_type(alloc.type, tmp);
    tmp = g95_build_constructor(tmp, nreverse(alloc.map));

    var = g95_create_var(TREE_TYPE(tmp));
    TREE_STATIC(var) = 1;
    DECL_INITIAL(var) = tmp;

    G95_DTYPE_ALLOCS(derived->backend_decl) = g95_addr_expr(var);
}



/* get_derived_type()-- Build a tree node for a derived type.  */

static tree get_derived_type(g95_symbol *derived) {
tree type, field, field_type, fieldlist, decl, tmp, len;
int rank, corank, init_flag;
variable_info vinfo;
g95_component *c;
g95_se se;

    assert(derived && derived->attr.flavor == FL_DERIVED);

    switch(derived->attr.itype) {
    case ITYPE_NONE:
	break;

    case ITYPE_C_PTR:
	return pvoid_type_node;

    case ITYPE_C_FUNPTR:
	return pfunc_type_node;

    case ITYPE_IEEE_CLASS:
    case ITYPE_IEEE_FLAG:
    case ITYPE_IEEE_STATUS:
    case ITYPE_IEEE_ROUND:
    case ITYPE_IEEE_FEATURES:
	return g95_int4_type_node;

    default:
	g95_internal_error("get_derived_type(): Bad type");
    }

    if (derived->backend_decl)
	return G95_DTYPE_TYPE(derived->backend_decl);

    /* Build the type node. */

    type = make_node(RECORD_TYPE);
    TYPE_NAME(type)   = get_identifier(derived->name);
    TYPE_PACKED(type) = g95_option.pack_derived;

    derived->backend_decl = g95_dtype_node();
    G95_DTYPE_TYPE(derived->backend_decl) = type;

    /* Build the type member list. Install the newly created
     * RECORD_TYPE node as DECL_CONTEXT of each FIELD_DECL. */

    fieldlist = NULL_TREE;
    for(c=derived->components; c; c=c->next) {

	if (c->ts.type == BT_DERIVED && c->pointer && c->as == NULL) {
	    field_type = pvoid_type_node;
	    field = build_decl(FIELD_DECL, get_identifier(c->name),field_type);
	    decl = field;

	} else {
	    g95_component_vinfo(c, &vinfo);
	    field_type = g95_get_descriptor(&vinfo);

	    if (!c->dimension && !c->allocatable) {
		field = build_decl(FIELD_DECL, get_identifier(c->name),
				   field_type);
		decl = field;

	    } else {
		decl = g95_array_node(); 

		if (!c->pointer && !c->allocatable)
		    field_type = g95_get_array_storage(&vinfo, decl);

		field = build_decl(FIELD_DECL, get_identifier(c->name), field_type);
		G95_ARRAY_FIELD(decl) = field;
	    }
	}

	DECL_CONTEXT(field) = type;
	DECL_PACKED(field) |= TYPE_PACKED(type);
	DECL_INITIAL(field) = 0;

	DECL_ALIGN(field) = 0;
	DECL_USER_ALIGN(field) = 0;

	TREE_CHAIN(field) = NULL_TREE;

	fieldlist = chainon(fieldlist, field);

	c->backend_decl = decl;
    }

    /* Now we have the final fieldlist.  Record it, then lay out the
     * derived type, including the fields.  */

    TYPE_FIELDS(type) = fieldlist;
    g95_finish_type(type);

    field = derived->components->backend_decl;
    if (derived->components->dimension || derived->components->allocatable)
	field = G95_ARRAY_FIELD(field);

    TYPE_ALIGN(type) = TYPE_ALIGN(TREE_TYPE(field));

    /* See if this structure has any initializers.  If it does, we
     * build a constructor for the whole thing that becomes the
     * initial value if an explicit initialization is not present. */

    init_flag = 0;
    for(c=derived->components; c; c=c->next)
	if (c->initializer || c->allocatable ||
	    (c->ts.type == BT_DERIVED &&
	     c->ts.derived->backend_decl != NULL_TREE &&
	     G95_DTYPE_INITIAL(c->ts.derived->backend_decl) != NULL_TREE)) {
	    init_flag = 1;
	    break;
	}

    if (init_flag) {
	decl = NULL_TREE;

	for(c=derived->components; c; c=c->next) {
	    tmp = NULL_TREE;

	    field = c->backend_decl;
	    if (c->dimension || c->allocatable)
		field = G95_ARRAY_FIELD(field);

	    if (c->initializer == NULL) {
		if (c->ts.type == BT_DERIVED && !c->pointer &&
		    c->ts.derived->attr.itype == ITYPE_NONE)
		    tmp = G95_DTYPE_INITIAL(c->ts.derived->backend_decl);

		if (c->as == NULL && c->cas == NULL) {
		    if (c->pointer)
			tmp = null_pointer_node;

		} else {
		    if (c->pointer || c->allocatable) {
			rank   = (c->as == NULL)  ? 0 : c->as->rank;
			corank = (c->cas == NULL) ? 0 : c->cas->corank;

			tmp = g95_null_array(rank, corank);

		    } else if (tmp != NULL) {
			g95_component_vinfo(c, &vinfo);
			tmp = g95_simple_array_init(&vinfo, tmp);
		    }
		}

	    } else {
		g95_init_se(&se, NULL);
		if (c->as == NULL) {
		    g95_conv_constant(&se, c->initializer);
		    tmp = se.expr;

		    if (STRING_P(tmp)) {
			len = c->ts.cl->backend_decl;
			tmp = g95_resize_string_constant(tmp, len);
		    }

		} else {
		    g95_component_vinfo(c, &vinfo);
		    tmp = g95_conv_array_initializer(&vinfo, NULL);
		}
	    }

	    if (tmp != NULL_TREE) {
		tmp = tree_cons(field, tmp, NULL_TREE);
		decl = chainon(decl, tmp);
	    }
	}

	G95_DTYPE_INITIAL(derived->backend_decl) =
	    g95_build_constructor(type, decl);
    }

    build_alloc_map(derived);
    return type;
}



/* g95_get_int_type()-- Get a type node for an integer kind */

tree g95_get_int_type(int kind) {

    switch(kind) {
    case 1:    return g95_int1_type_node;
    case 2:    return g95_int2_type_node;
    case 4:    return g95_int4_type_node;
    case 8:    return g95_int8_type_node;
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:   return g95_int16_type_node;
#endif
    default:
	g95_internal_error("Integer kind=%d not available", kind);
  }
}



/* g95_get_real_type()-- Get a type node for a real kind */

tree g95_get_real_type(int kind) {

    switch (kind) {
    case 4:      return g95_real4_type_node;
    case 8:      return g95_real8_type_node;
    case 10:     return x87_type_node;
    case 16:     return quad_type_node;
    default:
	g95_internal_error("Real kind=%d not available", kind);
    }
}



/* g95_get_complex_type()-- Get a type node for a complex kind */

tree g95_get_complex_type(int kind) {

    switch(kind) {
    case 4:   return g95_complex4_type_node;
    case 8:   return g95_complex8_type_node;
    case 10:  return x87_complex_type_node;
    case 16:  return quad_complex_type_node;
    default:
	g95_internal_error("Complex kind=%d not available", kind);
    }
}



/* g95_get_logical_type()-- Get a type node for a logical kind */

tree g95_get_logical_type(int kind) {

    switch (kind) {
    case 1:    return g95_logical1_type_node;
    case 2:    return g95_logical2_type_node;
    case 4:    return g95_logical4_type_node;
    case 8:    return g95_logical8_type_node;

#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:   return g95_logical16_type_node;
#endif
    default:
	fatal_error ("Logical kind=%d not available", kind);
    }
}



/* g95_init_character_len()-- Initialize a character length */

void g95_init_character_len(stmtblock_t *block, g95_charlen *cl, tree desc) {
g95_se se;
tree var;

    if (cl->backend_decl != NULL_TREE)
	return;

    g95_init_se(&se, NULL);
    g95_conv_spec_expr(&se, cl->length);

    se.expr = g95_build_max(g95_default_integer, se.expr, integer_zero_node);

    g95_add_block_to_block(block, &se.pre);

    if (G95_CONSTANT_P(se.expr)) {
	cl->backend_decl = se.expr;
	if (desc != NULL)
	    g95_add_modify_expr(block, g95_esize_ref(desc), se.expr);

    } else {
	var = (desc == NULL)
	    ? g95_create_var(g95_default_integer)
	    : g95_esize_ref(desc);

	g95_add_modify_expr(block, var, se.expr);
	cl->backend_decl = var;
    }

    g95_add_block_to_block(block, &se.post);
}



/* get_character_type()-- Get a type node for a character kind.  */

static tree get_character_type(int kind, g95_charlen *cl, int flag) {
tree base, type, len, bounds;

    switch(kind) {
    case 1:
	base = g95_character1_type_node;
	break;

    default:
	g95_internal_error("Character kind=%d not available", kind);
    }

    if (cl == &g95_unknown_charlen)
	return pchar_type_node;

    if (cl->backend_decl != NULL)
	len = cl->backend_decl;

    else if (cl->length == NULL ||
	     (!flag && cl->length->type != EXPR_CONSTANT))
	len = NULL_TREE;

    else {
	g95_init_character_len(&g95_context->pre, cl, NULL_TREE);
	len = cl->backend_decl;
    }

    if (len != NULL_TREE)
	len = g95_build_minus(g95_default_integer, len, integer_one_node);

    bounds = build_range_type(g95_default_integer, integer_zero_node, len);

    type = build_array_type(base, bounds);

    TYPE_STRING_FLAG(type) = 1;
    return type;
}



/* g95_get_typenode()-- Get a type node given a typespec. */

tree g95_get_typenode(g95_typespec *spec, int flag) {
tree basetype;

    switch(spec->type) {
    case BT_INTEGER:
	basetype = g95_get_int_type(spec->kind);
	break;

    case BT_REAL:
	basetype = g95_get_real_type(spec->kind);
	break;

    case BT_COMPLEX:
	basetype = g95_get_complex_type(spec->kind);
	break;

    case BT_LOGICAL:
	basetype = g95_get_logical_type(spec->kind);
	break;

    case BT_CHARACTER:
	basetype = get_character_type(spec->kind, spec->cl, flag);
	break;

    case BT_DERIVED:
	basetype = get_derived_type(spec->derived);
	break;

    case BT_PROCEDURE:
	basetype = build_function_type(void_type_node, NULL_TREE);
	break;

    default:
	g95_internal_error("g95_get_typenode(): Bad typespec");
	break;
    }

    return basetype;
}



/* g95_ts_size()-- Given a typespec, return a tree that gives its size. */

tree g95_ts_size(g95_typespec *ts) {
tree t;

    if (ts->type != BT_CHARACTER || ts->cl->length == NULL)
	t = size_in_bytes(g95_get_typenode(ts, 0));

    else if (ts->cl == &g95_unknown_charlen)
	t = integer_zero_node;

    else if (ts->cl->backend_decl != NULL_TREE)
	t = ts->cl->backend_decl;

    else {
	g95_init_character_len(&g95_context->pre, ts->cl, NULL_TREE);
	t = ts->cl->backend_decl;
    }

    return t;
}



/* g95_int_ts_size()-- Given a typespec, return an int that give its size */

int g95_int_ts_size(g95_typespec *ts) {

    return int_size_in_bytes(g95_get_typenode(ts, 0));
}



/* g95_component_vinfo()-- Initialize a variable_info structure from a
 * component structure. */

void g95_component_vinfo(g95_component *c, variable_info *vinfo) {

    vinfo->ts      = c->ts;
    vinfo->as      = c->as;
    vinfo->cas     = c->cas;
    vinfo->pointer = c->pointer;
    vinfo->dummy   = 0;

    vinfo->static_storage = 0;
    vinfo->noinit         = 0;
    vinfo->value          = c->initializer;
    vinfo->allocatable    = c->allocatable;
    vinfo->desc           = NULL_TREE;
}



/* g95_symbol_vinfo()-- Given a symbol pointer, initialize a
 * variable_info structure. */

void g95_symbol_vinfo(g95_symbol *sym, variable_info *vinfo) {

    vinfo->ts      = sym->ts;
    vinfo->as      = sym->as;
    vinfo->cas     = sym->cas;
    vinfo->pointer = sym->attr.pointer;
    vinfo->dummy   = sym->attr.dummy;

    vinfo->static_storage = g95_static_symbol(sym);
    vinfo->allocatable    = 0;
    vinfo->noinit         = sym->attr.noinit;

    vinfo->value = sym->attr.use_assoc ? NULL : sym->value;
    vinfo->desc  = sym->backend_decl;
}



/* g95_get_descriptor()-- Given a pointer to a variable_info
 * structure, return the type node that corresponds to that symbol.
 * For array variables, a descriptor type is returned. */

tree g95_get_descriptor(variable_info *vinfo) {
int rank, corank;
tree type;

    if (vinfo->as != NULL || vinfo->cas != NULL) {
	g95_get_typenode(&vinfo->ts, 1); /* init length spec, other types. */

	rank   = (vinfo->as == NULL)  ? 0 : vinfo->as->rank;
	corank = (vinfo->cas == NULL) ? 0 : vinfo->cas->corank;
	type   = g95_get_array_desc(rank, corank);

    } else {
	type = g95_get_typenode(&vinfo->ts, 1);
	if (vinfo->pointer)
	    type = build_pointer_type(type);
    }

    if (vinfo->dummy)
	type = build_pointer_type(type);

    return type;
}



/* g95_finish_type()-- Layout and output debug info for a record type.  */

void g95_finish_type(tree type) {
tree decl;

    decl = build_decl(TYPE_DECL, NULL_TREE, type);
    TYPE_STUB_DECL(type) = decl;
    layout_type(type);
    rest_of_type_compilation(type, 1);
    rest_of_decl_compilation(decl, 1, 0);
}



/* g95_result_type()-- Return the return value for the procedure. */

tree g95_result_type(g95_symbol *sym) {
g95_typespec *ts;
tree type;

    if (sym->ts.interface != NULL)
	return g95_result_type(sym->ts.interface);

    if (sym->attr.flavor == FL_VARIABLE)
	ts = g95_get_default_type(sym, sym->ns);

    else if (sym->attr.intrinsic)
	ts = &g95_find_function(sym->name)->ts;

    else if (!sym->attr.function)
	return g95_default_integer;

    else {
	ts = &sym->ts;

	sym = sym->result;
	if (sym->as) {  /* Arrays and array pointers are the same */
	    type = g95_get_array_desc(sym->as->rank, 0);
	    return build_pointer_type(type);
	}

	if (sym->attr.pointer || sym->attr.allocatable) {
	    type = g95_get_typenode(&sym->ts, 0);
	    return build_pointer_type(type);
	}
    }

    switch(ts->type) {
    case BT_CHARACTER:
	type = pchar_type_node;
	break;

    case BT_DERIVED:
	if (g95_c_ptr(ts)) {
	    type = pvoid_type_node;
	    break;
	}

    default:
	type = g95_get_typenode(ts, 0);
	break;
    }

    return type;
}



/* g95_dummy_arg_type()-- Return a type node for a dummy argument.
 * These are slightly different than regular variables. */

tree g95_dummy_arg_type(g95_symbol *sym, int flag) {
tree type;

    if (sym->attr.flavor == FL_PROCEDURE) {
	type = g95_procedure_type(sym);
	return build_pointer_type(type);
    }

    type = (sym->ts.type == BT_CHARACTER)
	? g95_character1_type_node
	: g95_get_typenode(&sym->ts, flag);

    if (sym->as == NULL) {  /* Scalar dummy argument */
	if (sym->attr.pointer || sym->attr.allocatable)
	    type = build_pointer_type(type);

    } else {   /* Array arguments */
	/* Assumed shape arrays and pointer arrays pass a pointer to the
	 * descriptor. */

	if (sym->as->type == AS_ASSUMED_SHAPE ||
	    sym->as->type == AS_DEFERRED)
	    type = g95_get_array_desc(sym->as->rank, 0);
    }

    if (!sym->attr.value && !sym->attr.by_value)
	type = build_pointer_type(type);

    return type;
}



/* g95_procedure_type()-- Get the type of a procedure, which
 * includes the types in its argument list. */

tree g95_procedure_type(g95_symbol *sym) {
tree type, typelist, typelist_tail;
g95_formal_arglist *f;
g95_symbol *arg;
bt result_type;

    if (sym->backend_decl)
	return TREE_TYPE(sym->backend_decl);

    typelist      = NULL_TREE;
    typelist_tail = NULL_TREE;

    /* Build the argument types for the function */

    if (sym->attr.function) {
	result_type = sym->result->ts.type;

	if (sym->result->as == NULL && !sym->result->attr.pointer &&
	    result_type == BT_CHARACTER) {

	    typelist = g95_chainon_list(typelist, pchar_type_node);
	    typelist = g95_chainon_list(typelist, g95_default_integer);
	}
    }

    /* User-specified parameters */

    for(f=sym->formal; f; f=f->next) {
	arg = f->sym;
	if (arg == NULL)
	    continue;

	type = (arg->attr.flavor == FL_PROCEDURE)
	    ? g95_procedure_type(arg)
	    : g95_dummy_arg_type(arg, 0);

	/* Just about everything is passed by reference */

	if (G95_DESCRIPTOR_P(type) || TREE_CODE(type) == FUNCTION_TYPE)
	    type = build_pointer_type(type);

	typelist = g95_chainon_list(typelist, type);

	if (arg->ts.type == BT_CHARACTER)
	    typelist_tail = g95_chainon_list(typelist_tail, g95_default_integer);
    }

    if (typelist_tail != NULL_TREE)
	typelist = chainon(typelist, typelist_tail);

    typelist = g95_chainon_list(typelist, void_type_node);

    type = g95_result_type(sym);

    return build_function_type(type, typelist);
}



/* g95_type_for_size()-- Return an integer type with BITS bits of
 * precision, that is unsigned if UNSIGNEDP is nonzero, otherwise
 * signed.  */

tree g95_type_for_size(unsigned bits, int unsignedp) {

    if (bits == TYPE_PRECISION(integer_type_node))
	return unsignedp ? unsigned_type_node : integer_type_node;

    if (bits == TYPE_PRECISION(signed_char_type_node))
	return unsignedp ? unsigned_char_type_node : signed_char_type_node;

    if (bits == TYPE_PRECISION(short_integer_type_node))
	return unsignedp ? short_unsigned_type_node : short_integer_type_node;

    if (bits == TYPE_PRECISION(long_integer_type_node))
	return unsignedp ? long_unsigned_type_node : long_integer_type_node;

    if (bits == TYPE_PRECISION(long_long_integer_type_node))
	return (unsignedp ? long_long_unsigned_type_node
		: long_long_integer_type_node);

/*TODO: We currently don't initialise this...
  if (bits == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);*/

    if (bits <= TYPE_PRECISION(intQI_type_node))
	return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

    if (bits <= TYPE_PRECISION(intHI_type_node))
	return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

    if (bits <= TYPE_PRECISION(intSI_type_node))
	return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

    if (bits <= TYPE_PRECISION(intDI_type_node))
	return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

    return 0;
}



/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree g95_type_for_mode(enum machine_mode mode, int unsignedp) {

    if (mode == TYPE_MODE (integer_type_node))
	return unsignedp ? unsigned_type_node : integer_type_node;

    if (mode == TYPE_MODE (signed_char_type_node))
	return unsignedp ? unsigned_char_type_node : signed_char_type_node;

    if (mode == TYPE_MODE (short_integer_type_node))
	return unsignedp ? short_unsigned_type_node : short_integer_type_node;

    if (mode == TYPE_MODE (long_integer_type_node))
	return unsignedp ? long_unsigned_type_node : long_integer_type_node;

    if (mode == TYPE_MODE (long_long_integer_type_node))
	return unsignedp
	    ? long_long_unsigned_type_node
	    : long_long_integer_type_node;

/*TODO: see above
  if (mode == TYPE_MODE (widest_integer_literal_type_node))
    return unsignedp ? widest_unsigned_literal_type_node
                     : widest_integer_literal_type_node;
*/

    if (mode == QImode)
	return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

    if (mode == HImode)
	return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

    if (mode == SImode)
	return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

    if (mode == DImode)
	return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
    if (mode == TYPE_MODE(intTI_type_node))
	return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

    if (mode == TYPE_MODE(float_type_node))
	return float_type_node;

    if (mode == TYPE_MODE(double_type_node))
	return double_type_node;

#if defined(FPU_387) || defined(FPU_SSE)
    if (mode == XFmode)
	return x87_type_node;
#endif

    if (mode == TFmode)
	return quad_type_node;

    if (mode == TCmode)
	return quad_complex_type_node;

    if (mode == TYPE_MODE(build_pointer_type(char_type_node)))
	return build_pointer_type (char_type_node);

    if (mode == TYPE_MODE(build_pointer_type(integer_type_node)))
	return build_pointer_type (integer_type_node);

    if (VECTOR_MODE_P(mode)) {
	enum machine_mode inner_mode = GET_MODE_INNER(mode);
	tree inner_type = g95_type_for_mode(inner_mode, unsignedp);

	if (inner_type != NULL_TREE)
	    return build_vector_type_for_mode(inner_type, mode);
    }

    return 0;
}



/* g95_unsigned_type()-- Return an unsigned type the same as TYPE in
 * other respects. */

tree g95_unsigned_type(tree type) {
tree type1;

    type1 = TYPE_MAIN_VARIANT(type);

    if (type1 == signed_char_type_node || type1 == char_type_node)
	return unsigned_char_type_node;

    if (type1 == integer_type_node)
	return unsigned_type_node;

    if (type1 == short_integer_type_node)
	return short_unsigned_type_node;

    if (type1 == long_integer_type_node)
	return long_unsigned_type_node;

    if (type1 == long_long_integer_type_node)
	return long_long_unsigned_type_node;

/*TODO :see others
  if (type1 == widest_integer_literal_type_node)
    return widest_unsigned_literal_type_node;
*/
#if HOST_BITS_PER_WIDE_INT >= 64
    if (type1 == intTI_type_node)
	return unsigned_intTI_type_node;
#endif

    if (type1 == intDI_type_node)
	return unsigned_intDI_type_node;

    if (type1 == intSI_type_node)
	return unsigned_intSI_type_node;

    if (type1 == intHI_type_node)
	return unsigned_intHI_type_node;

    if (type1 == intQI_type_node)
	return unsigned_intQI_type_node;

    return g95_signed_or_unsigned_type(1, type);
}


/* g95_signed_type()-- Return a signed type the same as TYPE in other
 * respects. */

tree g95_signed_type(tree type) {
tree type1;

    type1 = TYPE_MAIN_VARIANT(type);

    if (type1 == unsigned_char_type_node || type1 == char_type_node)
	return signed_char_type_node;

    if (type1 == unsigned_type_node)
	return integer_type_node;

    if (type1 == short_unsigned_type_node)
	return short_integer_type_node;

    if (type1 == long_unsigned_type_node)
	return long_integer_type_node;

    if (type1 == long_long_unsigned_type_node)
	return long_long_integer_type_node;

/*TODO: see others
  if (type1 == widest_unsigned_literal_type_node)
    return widest_integer_literal_type_node;
*/
#if HOST_BITS_PER_WIDE_INT >= 64
    if (type1 == unsigned_intTI_type_node)
	return intTI_type_node;
#endif

    if (type1 == unsigned_intDI_type_node)
	return intDI_type_node;

    if (type1 == unsigned_intSI_type_node)
	return intSI_type_node;

    if (type1 == unsigned_intHI_type_node)
	return intHI_type_node;

    if (type1 == unsigned_intQI_type_node)
	return intQI_type_node;

    return g95_signed_or_unsigned_type(0, type);
}


/* g95_signed_or_unsigned_type()-- Return a type the same as TYPE
 * except unsigned or signed according to UNSIGNEDP. */

tree g95_signed_or_unsigned_type (int unsignedp, tree type) {

    if (!INTEGRAL_TYPE_P(type) || TYPE_UNSIGNED (type) == unsignedp)
	return type;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
	return unsignedp ? unsigned_char_type_node : signed_char_type_node;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
	return unsignedp ? unsigned_type_node : integer_type_node;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node))
	return unsignedp ? short_unsigned_type_node : short_integer_type_node;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node))
	return unsignedp ? long_unsigned_type_node : long_integer_type_node;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node))
	return unsignedp
	    ? long_long_unsigned_type_node
	    : long_long_integer_type_node;

/*TODO: see others
  if (TYPE_PRECISION (type) == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);
*/

#if HOST_BITS_PER_WIDE_INT >= 64
    if (TYPE_PRECISION (type) == TYPE_PRECISION (intTI_type_node))
	return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

    if (TYPE_PRECISION (type) == TYPE_PRECISION (intDI_type_node))
	return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (intSI_type_node))
	return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (intHI_type_node))
	return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

    if (TYPE_PRECISION (type) == TYPE_PRECISION (intQI_type_node))
	return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

    return type;
}
