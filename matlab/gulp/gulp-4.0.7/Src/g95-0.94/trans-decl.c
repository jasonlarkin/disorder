/* Backend function setup
   Copyright (C) 2000-2008 Free Software Foundation, Inc.
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

/* trans-decl.c -- Handling of backend function and variable decls, etc */

#include "trans.h"


#define MAX_LABEL_VALUE 99999

/* Holds the result of the current function */

static tree current_function_return_label, snan_4, snan_8, snan_10, snan_16;

tree g95_string_len, g95_junk_stat, g95_ac_info_type;
tree library_select_string;

/* Math functions.  Many other math functions are handled in
 * trans-intrinsic.c.  */

g95_trans_context *g95_context = NULL;
static g95_symbol *sym_head;
extern tree x87_type_node;

/* None of the specific intrinsics which can be passed as actual arguments
 * for dummy procedures has more then two parameters.  */

#define G95_MAX_SPECIFIC_ARGS 2


typedef struct var_chain {
    g95_symbol *sym;
    enum { SPEC_ARRAY, SPEC_LEN } spec;
    struct var_chain *prev;
} var_chain;

static var_chain *var_base = NULL;

static struct {
    tree type, label, len, pointer;
} label_info;


static enum {
    SPEC_INQUIRE_NONE, SPEC_INQUIRE_LEN, SPEC_INQUIRE_BOUNDS
} spec_inquire = SPEC_INQUIRE_NONE;



typedef struct default_list {
    struct default_list *next;

    bt type;
    int kind;
    tree init;
} default_list;


static void find_dependent_vars(g95_symbol *);
static void traverse_spec_expr(g95_expr *);
static void create_st_function(g95_symbol *);




/* g95_build_label_decl()-- Build a backend label declaration.  Set
 * TREE_USED for named labels.  For artificial labels it's up to the
 * caller to mark the label as used.  */

tree g95_build_label_decl(tree label_id) {
char *name;
tree decl;

    if (label_id != NULL_TREE)
	name = NULL;

    else     /* Build an internal label name. */
	label_id = g95_unique_identifier("label");

    /* Build the LABEL_DECL node. Labels have no type. */
    decl = build_decl(LABEL_DECL, label_id, void_type_node);

    DECL_CONTEXT(decl) = current_function_decl;
    DECL_MODE(decl) = VOIDmode;

    DECL_ARTIFICIAL(decl) = 1;
    TREE_USED(decl) = 1;
  
    /* We always define the label as used, even if the original source
     * file never references the label.  We don't want all kinds of
     * spurious warnings for old-style Fortran code with too many
     * labels. */

    return decl;
}



/* g95_get_return_label()-- Returns the return label for the current
 * function. */

tree g95_get_return_label(void) {
char name[3*G95_MAX_SYMBOL_LEN + 20];

    if (current_function_return_label)
	return current_function_return_label;

    sprintf(name, "__return_%s",
	    IDENTIFIER_POINTER(DECL_NAME(current_function_decl)));

    current_function_return_label = g95_build_label_decl(get_identifier(name));

    DECL_ARTIFICIAL(current_function_return_label) = 1;

    return current_function_return_label;
}



/* g95_get_label_decl()-- Return the backend label declaration for a
 * given label structure, or create it if it doesn't exist yet.  */

tree g95_get_label_decl(g95_st_label *lp) {
char name[20];
tree decl;

    if (lp->backend_decl)
	return lp->backend_decl;

    sprintf(name, "__label_%d", lp->value);
    decl = g95_build_label_decl(get_identifier(name));

    if (lp->value <= MAX_LABEL_VALUE) {
	DECL_SOURCE_LINE(decl) = lp->where.lb->linenum;
	DECL_SOURCE_FILE(decl) = lp->where.lb->file->filename;

    } else
	DECL_ARTIFICIAL(decl) = 1;

    lp->backend_decl = decl;
    return decl;
}



/* g95_sym_identifier()-- Convert a name of a g95_symbol to an
 * identifier name. */

tree g95_sym_identifier(g95_symbol *sym, int global, char *suffix) {
char *p;

    p = g95_mangle_sym(sym, global, suffix);
    return get_identifier(p);
}



/* finish_decl()-- Finish processing of a declaration and install its
 * initial value. */

static void finish_decl(tree decl, tree init) {

    if (TREE_CODE(decl) == PARM_DECL)
	assert(init == NULL_TREE);

    /* Remember that PARM_DECL doesn't have a DECL_INITIAL field per se
     * -- it overlaps DECL_ARG_TYPE.  */

    else if (init == NULL_TREE)
	assert(DECL_INITIAL (decl) == NULL_TREE);
    else
	assert(DECL_INITIAL (decl) == error_mark_node);

    if (init != NULL_TREE) {
	if (TREE_CODE(decl) != TYPE_DECL)
	    DECL_INITIAL(decl) = init;
	else {
	    /* typedef foo = bar; store the type of bar as the type of foo.  */
	    TREE_TYPE(decl) = TREE_TYPE(init);
	    DECL_INITIAL(decl) = init = 0;
	}
    }

    if (TREE_CODE(decl) == VAR_DECL) {
	if (DECL_SIZE (decl) == NULL_TREE 
	    && TYPE_SIZE(TREE_TYPE(decl)) != NULL_TREE)
	    layout_decl(decl, 0);

	if (DECL_SIZE(decl) == NULL_TREE && (TREE_STATIC(decl) ?
          /* A static variable with an incomplete type is an error if it is
             initialized. Also if it is not file scope. Otherwise, let it
             through, but if it is not `extern' then it may cause an error
             message later.  */
					     (DECL_INITIAL (decl) != 0
					      || DECL_CONTEXT (decl) != 0) :
          /* An automatic variable with an incomplete type is an error.  */
					     !DECL_EXTERNAL (decl))) {
	    g95_fatal_error("storage size not known");
	}

	if ((DECL_EXTERNAL(decl) || TREE_STATIC(decl)) &&
	    DECL_SIZE(decl) != 0 &&
	    TREE_CODE(DECL_SIZE(decl)) != INTEGER_CST) {
	    g95_fatal_error("storage size not constant");
	}
    }
}



/* finish_var_decl()-- Apply symbol attributes to a variable, and add
 * it to the function scope.  */

static void finish_var_decl(tree decl, g95_symbol *sym) {

    /* TREE_ADDRESSABLE means the address of this variable is needed for
     * a TARGET variable.  We also need to set this if the variable is
     * passed by reference in a CALL statement. */

    if (sym->attr.target)
	TREE_ADDRESSABLE(decl) = 1;

    /* If it wasn't used we wouldn't be getting it.  */
    TREE_USED(decl) = 1;

    /* If a variable is USE associated, it's always external.  */

    if (sym->attr.use_assoc) {
	DECL_EXTERNAL(decl) = 1;
	TREE_PUBLIC(decl)   = 1;

    } else if (g95_module_symbol(sym) && !sym->attr.function &&
	       !sym->attr.result_var) { /* Module variables */
	TREE_PUBLIC(decl) = 1;
	TREE_STATIC(decl) = 1;
#ifdef __APPLE__ 
#if __MAC_OS_X_VERSION_MIN_REQUIRED < 1050
	DECL_COMMON(decl) = 1;    /* Apple changes their minds... */
#endif
#endif
    }

    /* This must be able to override a DECL_EXTERNAL, so it has to be
     * near-last. */

    if (sym->attr.bind) {
	DECL_COMMON(decl)   = 1;
	TREE_PUBLIC(decl)   = 1;
	TREE_STATIC(decl)   = 1;
	DECL_EXTERNAL(decl) = 0;
    }

    if (g95_static_symbol(sym) && !sym->attr.use_assoc)
	TREE_STATIC(decl) = 1;

    DECL_CONTEXT(decl) = (sym->attr.use_assoc)
	? NULL_TREE : sym->ns->backend_decl;
}



/* g95_default_scalar_value()-- Given a symbol, return a tree that is
 * the default initial value determined from the command line options.
 * Return NULL if there is no default initialization. */

tree g95_default_scalar_value(g95_symbol *sym) {
static default_list *root = NULL;
static tree invalid = NULL;
tree init, type, tmp;
default_list *p;
bignum b;

    init = NULL;

    if (sym->attr.pointer) {
	switch(g95_option.pointer_init) {
	case POINTER_INIT_NONE:
	    init = g95_option.zero_init
		? null_pointer_node
		: NULL_TREE;
	    break;

	case POINTER_INIT_NULL:
	    init = null_pointer_node;
	    break;

	case POINTER_INIT_INVALID:
	    if (invalid == NULL)
		invalid = convert(pvoid_type_node,
				  g95_addr_expr(g95_junk_stat));

	    init = invalid;
	    break;
	}

    } else {  /* Nonpointer var, look for the decl in the cache */
	for(p=root; p; p=p->next)
	    if (p->type == sym->ts.type && p->kind == sym->ts.kind)
		return p->init;

	type = g95_get_typenode(&sym->ts, 0);

	switch(sym->ts.type) {
	case BT_INTEGER:
	    if (g95_option.integer_init) {
		tmp = g95_build_int(g95_option.integer_value,
				    g95_option.integer_value < 0 ? -1 : 0);
		init = convert(type, tmp);

	    } else if (g95_option.zero_init)
		init = convert(type, integer_zero_node);

	    break;

	case BT_LOGICAL:
	    switch(g95_option.logical_init) {
	    case LOGICAL_INIT_NONE:
		if (!g95_option.zero_init)
		    break;

		/* Fall through */

	    case LOGICAL_INIT_FALSE:
		init = convert(type, boolean_false_node);
		break;

	    case LOGICAL_INIT_TRUE:
		init = convert(type, boolean_true_node);
		break;
	    }

	    break;

	case BT_REAL:
	case BT_COMPLEX:
	    b = NULL;

	    switch(g95_option.real_init) {
	    case REAL_INIT_NONE:
		if (!g95_option.zero_init)
		    break;

		/* fall through */

	    case REAL_INIT_ZERO:
		b = bg_get_exceptional(FF_ZERO, sym->ts.kind, 0);
		break;

	    case REAL_INIT_NAN:
		b = bg_get_exceptional(FF_NAN, sym->ts.kind, 0);
		break;

	    case REAL_INIT_PLUS_INF:
		b = bg_get_exceptional(FF_INFINITY, sym->ts.kind, 1);
		break;

	    case REAL_INIT_MINUS_INF:
		b = bg_get_exceptional(FF_INFINITY, sym->ts.kind, -1);
		break;
	    }

	    if (b != NULL)
		init = bg_to_tree(b, sym->ts.kind);

	    if (sym->ts.type == BT_COMPLEX && init != NULL_TREE)
		init = build_complex(NULL_TREE, init, init);

	    break;

	default:
	    break;
	}

	if (init != NULL_TREE) {
	    p = g95_getmem(sizeof(default_list));

	    p->type = sym->ts.type;
	    p->kind = sym->ts.kind;
	    p->next = root;
	    p->init = init;

	    root = p;
	}
    }

    return init;
}



/* init_nan()-- Special case: if the symbol is a real, non-static
 * symbol initialized to a not-a-number, we generate a memcpy to avoid
 * generating an exception when initializing the value. */

static tree init_nan(tree var_addr, g95_typespec *ts) {
tree tmp, args;

    switch(ts->kind) {
    case 4:   tmp = snan_4;   break;
    case 8:   tmp = snan_8;   break;
    case 10:  tmp = snan_10;  break;
    case 16:  tmp = snan_16;  break;
    default:  g95_internal_error("init_nan(): Bad kind");
    }

    args = g95_chainon_list(NULL_TREE, var_addr);
    args = g95_chainon_list(args, tmp);
    args = g95_chainon_list(args, g95_ts_size(ts));

    tmp = built_in_decls[BUILT_IN_MEMCPY];
    tmp = g95_build_function_call(tmp, args);

    return tmp;
}



/* default_scalar_init()-- Set the default initialization for a scalar
 * variable. */

static void default_scalar_init(g95_symbol *sym) {
tree tmp;

    if ((sym->ts.type != BT_REAL && sym->ts.type != BT_COMPLEX) ||
	sym->attr.pointer || g95_option.real_init != REAL_INIT_NAN ||
	g95_static_symbol(sym))
	DECL_INITIAL(sym->backend_decl) = g95_default_scalar_value(sym);

    else {
	tmp = init_nan(g95_addr_expr(sym->backend_decl), &sym->ts);
	g95_add_expr_to_block(&g95_context->pre, tmp);
    }
}



/* initial_scalar_value()-- Set the initial value for a scalar variable. */

static void initial_scalar_value(g95_symbol *sym) {
tree t, len, tmp;
g95_se se;

    if (sym->attr.use_assoc)
	return;

    if (sym->attr.data) {
	DECL_INITIAL(sym->backend_decl) = g95_generate_data(sym);
	return;
    }

    g95_init_se(&se, NULL);

    if (sym->value == NULL) {
	if (sym->ts.type != BT_DERIVED || sym->attr.pointer ||
	    sym->ts.derived->attr.itype != ITYPE_NONE)
	    default_scalar_init(sym);

	else {
	    if (TREE_CODE(TREE_TYPE(sym->backend_decl)) != POINTER_TYPE ||
		!g95_initialized_component(sym->ts.derived))
		DECL_INITIAL(sym->backend_decl) =
		    G95_DTYPE_INITIAL(sym->ts.derived->backend_decl);

	    else {
		t = sym->backend_decl;
		t = g95_build_indirect(TREE_TYPE(TREE_TYPE(t)), t);

		tmp = G95_DTYPE_INITIAL(sym->ts.derived->backend_decl);
		g95_add_modify_expr(&g95_context->pre, t, tmp);
	    }
	}

    } else {
	g95_conv_constant(&se, sym->value);

	if (STRING_P(se.expr)) {
	    len = sym->ts.cl->backend_decl;
	    se.expr = g95_resize_string_constant(se.expr, len);
	}

	DECL_INITIAL(sym->backend_decl) = se.expr;
    }
}



/* g95_initial_array_value()-- Return the initial value for an array. */

tree g95_initial_array_value(g95_symbol *sym) {
variable_info vinfo;

    if (sym->attr.use_assoc)
	return NULL_TREE;

    if (sym->attr.data)
	return g95_generate_data(sym);

    if (sym->value == NULL &&
	(sym->ts.type != BT_DERIVED ||
	 sym->ts.derived->attr.itype != ITYPE_NONE ||
	 G95_DTYPE_INITIAL(sym->ts.derived->backend_decl) == NULL_TREE))
	return NULL_TREE;

    g95_symbol_vinfo(sym, &vinfo);
    return g95_conv_array_initializer(&vinfo, NULL);
}



/* g95_deallocate_components()-- Deallocate any allocatable array
 * components that this variable has. */

void g95_deallocate_components(g95_symbol *sym, stmtblock_t *block) {
tree tmp;

    if (sym->attr.dummy || sym->attr.pointer || sym->ts.type != BT_DERIVED ||
	sym->ts.derived->attr.itype != ITYPE_NONE || g95_static_symbol(sym))
	return;

    tmp = G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl);
    if (tmp == NULL_TREE)
	return;

    tmp = g95_call_library(void_type_node, PREFIX "deep_dealloc",
			   g95_addr_expr(sym->backend_decl), tmp, NULL_TREE);

    g95_add_expr_to_block(block, tmp);
}



/* init_varlen_character()-- Given a vinfo node for a character
 * variable, see if the length is a constant or not.  For a constant
 * length, we return a typenode for a block of memory for the string.
 * Otherwise we return a character pointer type and generate code to
 * initialize the pointer to heap memory. */

static void init_varlen_character(variable_info *vinfo, tree var) {
tree length;
g95_se se;

    if (vinfo->pointer || vinfo->ts.type != BT_CHARACTER || 
	TREE_TYPE(var) != pchar_type_node || vinfo->ts.cl->length == NULL)
	return;

    g95_init_se(&se, NULL);
    length = g95_conv_char_length(&se, &vinfo->ts);

    length = g95_build_max(g95_default_integer, length, integer_zero_node);

    g95_call_temp_alloc(&g95_context->pre, var, length);
    g95_call_temp_free(&g95_context->post, var);

    g95_add_block_to_block(&g95_context->pre, &se.post);
}



/* scalar_variable()-- Create the declaration for a scalar variable. */

static void scalar_variable(g95_symbol *sym) {
variable_info vinfo;
tree decl, t, type;
int flag;

    g95_symbol_vinfo(sym, &vinfo);
    type = g95_get_descriptor(&vinfo);

    flag = g95_static_symbol(sym) || sym->ts.type != BT_DERIVED ||
	g95_stack_variable(type);

    if (sym->ts.type == BT_DERIVED &&
	(g95_allocatable_component(sym->ts.derived) ||
	 g95_initialized_component(sym->ts.derived)))
	flag = 1;

    if (flag) {
	decl = build_decl(VAR_DECL, g95_sym_identifier(sym, 0, NULL), type);
	pushdecl(decl);
	finish_var_decl(decl, sym);

    } else {
	type = build_pointer_type(type);

	decl = build_decl(VAR_DECL, g95_sym_identifier(sym, 0, NULL), type);
	pushdecl(decl);
	finish_var_decl(decl, sym);

	g95_call_temp_alloc(&g95_context->pre, decl,
			    size_in_bytes(TREE_TYPE(type)));
    }

    if (sym->attr.volatile_)
	TREE_THIS_VOLATILE(decl) = 1;

    sym->backend_decl = decl;

    initial_scalar_value(sym);

    if (g95_context != NULL && !g95_static_symbol(sym)) {
	t = build1(DECL_EXPR, TREE_TYPE(decl), decl);
	g95_add_expr_to_block(&g95_context->pre, t);
    }

    init_varlen_character(&vinfo, decl);

    if (!g95_static_symbol(sym))
	g95_deallocate_components(sym, &g95_context->post);

    if (!flag)
	g95_call_temp_free(&g95_context->post, decl);
}



/* deallocate_var()-- Deallocate an allocatable array. */

static void deallocate_var(g95_symbol *sym) {
tree tmp, kind, alloc;
char *name;

    tmp = g95_addr_expr(G95_ARRAY_DESC(sym->backend_decl));

    kind = g95_build_int(g95_default_integer_kind(0), 0);

    alloc = (sym->ts.type == BT_DERIVED &&
	     sym->ts.derived->attr.itype == ITYPE_NONE &&
	     G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl) != NULL_TREE)
	? G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl)
	: null_pointer_node;

    name = (sym->cas == NULL)
	? PREFIX "deallocate_array"
	: PREFIX "deallocate_coarray";

    tmp = g95_call_library(void_type_node, name,
			   tmp, alloc, integer_one_node, NULL_TREE);

    g95_add_expr_to_block(&g95_context->post, tmp);
}



/* coarray_desc()-- Get a coarray descriptor. */

static tree coarray_desc(g95_symbol *sym, tree storage) {
tree tmp, type, field, c1, c2, decl, z, lo, hi;
g95_se se;
auto int i;

    type = g95_get_array_desc(0, sym->cas->corank);
    decl = build_decl(VAR_DECL, g95_sym_identifier(sym, 0, NULL), type);

    field = g95_find_field(type, "offset");
    c1 = tree_cons(field, null_pointer_node, NULL);

    field = g95_find_field(type, "base");

    tmp = (storage == NULL)
	? null_pointer_node
	: g95_addr_expr(storage);

    c1 = tree_cons(field, tmp, c1);

    field = g95_find_field(type, "rank");
    c1 = tree_cons(field, integer_zero_node, c1);

    field = g95_find_field(type, "corank");
    c1 = tree_cons(field, g95_build_int(sym->cas->corank, 0), c1);

    field = g95_find_field(type, "esize");
    c1 = tree_cons(field, g95_ts_size(&sym->ts), c1);

    g95_init_se(&se, NULL);
    c2 = NULL;

    z = convert(g95_pointer_integer, g95_ts_size(&sym->ts));

    for(i=0; i<sym->cas->corank; i++) {
        if (sym->cas->type == CAS_DEFERRED)
	    lo = hi = z;

	else {
	    g95_conv_constant(&se, sym->cas->lower[i]);
	    lo = convert(g95_pointer_integer, se.expr);

	    if (i == sym->cas->corank - 1)
		hi = z;

	    else {
		g95_conv_constant(&se, sym->cas->upper[i]);
		hi = convert(g95_pointer_integer, se.expr);
	    }
	}

	c2 = tree_cons(g95_build_int(3*i,   0),  z, c2);
	c2 = tree_cons(g95_build_int(3*i+1, 0), lo, c2);
	c2 = tree_cons(g95_build_int(3*i+2, 0), hi, c2);
    }

    tmp = g95_build_int(2*sym->cas->corank, 0);
    tmp = build_range_type(g95_pointer_integer, integer_zero_node, tmp);
    tmp = build_array_type(g95_pointer_integer, tmp);

    c2 = g95_build_constructor(tmp, nreverse(c2));

    field = g95_find_field(type, "info");
    c1 = tree_cons(field, c2, c1);

    DECL_INITIAL(decl) = g95_build_constructor(type, nreverse(c1));

    pushdecl(decl);
    finish_var_decl(decl, sym);

    return decl;
}



/* array_variable()-- Create the declaration for an array variable */

static void array_variable(g95_symbol *sym) {
tree array, decl, t, initial, type, storage;
variable_info vinfo;
char *suffix;

    sym->backend_decl = array = g95_array_node();

    g95_symbol_vinfo(sym, &vinfo);

    if (vinfo.as->type == AS_DEFERRED || vinfo.as->type == AS_ASSUMED_SHAPE ||
	sym->attr.desc || g95_static_symbol(sym) || sym->cas != NULL ||
	!g95_constant_array_spec(vinfo.as, 0) ||
	(sym->ts.type == BT_DERIVED &&
	 g95_allocatable_component(sym->ts.derived)) ||
	(sym->ts.type == BT_CHARACTER &&
	 (sym->ts.cl->length == NULL ||
	  sym->ts.cl->length->type != EXPR_CONSTANT))) {

	type = g95_get_descriptor(&vinfo);
	t = g95_sym_identifier(sym, 0, NULL);

	decl = build_decl(VAR_DECL, t, type);

	pushdecl(decl);
	finish_var_decl(decl, sym);

	G95_ARRAY_DESC(array) = decl;
    }

    type = g95_get_array_storage(&vinfo, array);

    if (type != NULL) {
	initial = g95_initial_array_value(sym);

	if (g95_context == NULL || initial != NULL || g95_static_symbol(sym) ||
	    g95_stack_variable(type)) { 
	    /* Variable not on heap */

	    suffix = (G95_ARRAY_DESC(array) == NULL_TREE) ? NULL : "data";

	    storage = build_decl(VAR_DECL, g95_sym_identifier(sym, 0, suffix),
				 type);
	    DECL_INITIAL(storage) = initial;

	    if (g95_context != NULL && !g95_static_symbol(sym)) {
		t = build1(DECL_EXPR, TREE_TYPE(storage), storage);
		g95_add_expr_to_block(&g95_context->pre, t);
	    }

	    pushdecl(storage);
	    finish_var_decl(storage, sym);

	    G95_ARRAY_STORAGE(array) = storage;

	} else if (G95_ARRAY_DESC(array) == NULL_TREE) {  /* Heap variable */
	    t = g95_create_var(pchar_type_node);
	    G95_ARRAY_STORAGE(array) = t;

	    g95_call_temp_alloc(&g95_context->pre, t, G95_ARRAY_SIZE(array));
	    g95_call_temp_free(&g95_context->post, t);
	}
    }

    /* Generate code to deallocate this array automatically */

    if (sym->attr.allocatable && !sym->attr.save && !sym->ns->save_all &&
	!g95_module_symbol(sym) && !sym->attr.artificial)
	deallocate_var(sym);

    else if (!sym->attr.allocatable && !sym->attr.pointer &&
	     sym->ts.type == BT_DERIVED &&
	     g95_allocatable_component(sym->ts.derived) &&
	     G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl) != NULL_TREE &&
	     !g95_static_symbol(sym) && !g95_module_symbol(sym)) {

	t = g95_call_library(void_type_node, PREFIX "deep_dealloc1",
			     g95_addr_expr(G95_ARRAY_DESC(array)),
			     G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl),
			     NULL_TREE);
	g95_add_expr_to_block(&g95_context->post, t);
    }

    if (G95_ARRAY_DESC(array) != NULL_TREE)
	g95_init_array_desc(&vinfo, array);
}



/* allocate_array_return()-- Generate code to initialize an array
 * return. */

static void allocate_array_return(g95_symbol *result, tree identifier) {
tree init, type, tmp, var, desc, info[2 + 2*G95_MAX_DIMENSIONS];
stmtblock_t pre, post;
g95_array_spec *as;
int rank, i, n;
g95_expr *e;
g95_se se;

    g95_init_block(&pre);
    g95_init_block(&post);

    n = 0;
    as = result->as;
    rank = as->rank;

    info[n++] = g95_build_int(rank, 0);

    if (result->ts.type != BT_CHARACTER)
	info[n++] = g95_ts_size(&result->ts);

    else {
	g95_init_se(&se, NULL);

	e = result->ts.cl->length;
	g95_conv_spec_expr(&se, e);

	info[n++] = g95_build_section_info(e, &se, &pre, &post);
    }

    for(i=0; i<rank; i++) {
	g95_init_se(&se, NULL);
	g95_conv_spec_expr(&se, as->lower[i]);

	info[n++] = g95_build_section_info(as->lower[i], &se, &pre, &post);

	g95_init_se(&se, NULL);
	g95_conv_spec_expr(&se, as->upper[i]);
	info[n++] = g95_build_section_info(as->upper[i], &se, &pre, &post);
    }

    for(i=0; i<n; i++)
	g95_set_section_info(&pre, i, info[i]);

    if (result->ts.type != BT_DERIVED ||
	G95_DTYPE_INITIAL(result->ts.derived->backend_decl) == NULL_TREE)
	init = null_pointer_node;

    else {
	tmp = G95_DTYPE_INITIAL(result->ts.derived->backend_decl);
	var = g95_create_var(TREE_TYPE(tmp));
	DECL_INITIAL(var) = tmp;
	TREE_STATIC(var) = 1;

	TREE_ADDRESSABLE(var) = 1;
	init = g95_addr_expr(var);
    }

    type = g95_get_array_pdesc(result->as->rank, 0);
    desc = build_decl(VAR_DECL, identifier, type);

    pushdecl(desc);

    G95_ARRAY_DESC(g95_context->result) = desc;
    result->backend_decl = g95_context->result;

    tmp = g95_call_library(pvoid_type_node, PREFIX "array_from_section",
			   init, NULL_TREE);
    g95_add_modify_expr(&pre, desc, tmp);

    g95_add_block_to_block(&g95_context->pre, &pre);
    g95_add_block_to_block(&g95_context->pre, &post);
}



/* init_array_return()-- Initialize a return value for a pointer. */

static void init_array_return(g95_symbol *result, tree identifier) {
tree type, desc, array;
variable_info vinfo;

    array = g95_array_node();
    g95_context->result = array;

    if (!result->attr.pointer && !result->attr.allocatable)
	allocate_array_return(result, identifier);

    else {
	type = g95_get_array_desc(result->as->rank, 0);
	desc = build_decl(VAR_DECL, identifier, type);
	G95_ARRAY_DESC(array) = desc; 

	g95_symbol_vinfo(result, &vinfo);
	g95_init_array_desc(&vinfo, array);

	pushdecl(desc);
	finish_var_decl(desc, result);
    }
}



/* get_parm_decl()-- Get a declaration associated with a dummy variable */

static void get_parm_decl(g95_symbol *sym) {
tree parm, type;

    type = g95_dummy_arg_type(sym, 1);
    parm = build_decl(PARM_DECL, g95_sym_identifier(sym, 0, NULL), type);

    DECL_ARG_TYPE(parm) = type;

#if TARGET_GCC_VERSION < 410
    DECL_ARG_TYPE_AS_WRITTEN(parm) = type;
#endif

    TREE_READONLY(parm) = 1;

    sym->backend_decl = parm;
}



/* init_result()-- Create the variable used to hold the function
 * return value.  This is whether the result variable is explicit or
 * not. */

static void init_result(g95_symbol *sym) {
tree type, decl, len, identifier, cons, tmp, b;
g95_symbol *result;
stmtblock_t block;
g95_se se;

    if (g95_context->result != NULL_TREE)
	return;

    len = NULL_TREE;
    result = sym->result;

    if (sym->attr.subroutine) {
	decl = build_decl(VAR_DECL, NULL, g95_default_integer);
	goto finish_decl;
    }

    sym_head = NULL;
    find_dependent_vars(result);

    identifier = (sym->result == sym)
	? g95_sym_identifier(sym, 0, "result")
	: g95_sym_identifier(sym->result, 0, NULL);

    if (result->as != NULL) { /* Pointer array returns */
	init_array_return(sym->result, identifier);
	decl = g95_context->result;

    } else if (sym->attr.pointer) {
	if (result->ts.type == BT_CHARACTER &&
	    result->ts.cl->length != NULL &&
	    result->ts.cl->length->type != EXPR_CONSTANT) {
	    len = g95_create_var(g95_default_integer);

	    g95_init_block(&block);

	    g95_init_se(&se, NULL);
	    g95_conv_spec_expr(&se, result->ts.cl->length);

	    g95_add_block_to_block(&block, &se.pre);
	    g95_add_modify_expr(&block, len, se.expr);
	    g95_add_block_to_block(&block, &se.post);

	    b = g95_finish_block(&block);
	    g95_add_expr_to_block(&g95_context->pre, b);

	    result->ts.cl->backend_decl = len;

	    g95_add_modify_expr(&g95_context->post, g95_string_len, len);
	}

	goto pointer;

    } else
	switch(sym->ts.type) {
	case BT_CHARACTER:
	    decl = DECL_ARGUMENTS(g95_context->current_procedure->backend_decl);
	    len = TREE_CHAIN(decl);
	    g95_context->result = decl;

	    if (sym->ts.cl->length != NULL) {
		g95_init_block(&block);

		g95_init_se(&se, NULL);
		g95_conv_spec_expr(&se, result->ts.cl->length);

		g95_add_block_to_block(&block, &se.pre);
		g95_add_modify_expr(&block, len, se.expr);
		g95_add_block_to_block(&block, &se.post);

		g95_call_temp_alloc(&block, decl, len);

		b = g95_finish_block(&block);
		tmp = g95_build_eq(decl, null_pointer_node);

		b = g95_build_cond(void_type_node, tmp, b, empty_stmt_node);
		g95_add_expr_to_block(&g95_context->pre, b);

		g95_add_modify_expr(&g95_context->post, g95_string_len, len);
	    }

	    break;

	case BT_DERIVED:
	    switch(result->ts.derived->attr.itype) {
	    case ITYPE_C_PTR:
	    case ITYPE_C_FUNPTR:
		decl = build_decl(VAR_DECL, identifier, pvoid_type_node);
		goto finish_decl;

	    default:
		break;
	    }

	default:
	pointer:
	    type = g95_result_type(sym);
	    decl = build_decl(VAR_DECL, identifier, type);

	finish_decl:
	    TREE_ADDRESSABLE(decl) = 1;
	    DECL_ARTIFICIAL(decl) = 1;
	    DECL_EXTERNAL(decl) = 0;
	    TREE_PUBLIC(decl) = 0;

	    layout_decl(decl, 0);

	    pushdecl(decl);
	    rest_of_decl_compilation(decl, 1, 0);
	    TREE_USED(decl) = 1;

	    g95_context->result = decl;
	    break;
	}

    if (sym->attr.subroutine)
	g95_add_modify_expr(&g95_context->pre, g95_context->result,
			    integer_zero_node);

    if (result != NULL && result != sym)
	result->backend_decl = decl;

    else if (sym->backend_decl == NULL)
	sym->backend_decl = decl;

    if (len != NULL_TREE)
	result->ts.cl->backend_decl = len;

    if (result != NULL && result->as == NULL && sym->ts.type == BT_DERIVED &&
	result->ts.derived->attr.itype == ITYPE_NONE &&
	!result->attr.pointer &&
	G95_DTYPE_INITIAL(sym->ts.derived->backend_decl) != NULL_TREE) {

	g95_init_se(&se, NULL);
	se.expr = decl;

	g95_reflevel(&se, 0);
	cons = G95_DTYPE_INITIAL(sym->ts.derived->backend_decl);
	g95_add_modify_expr(&g95_context->pre, se.expr, cons);
    }
}



/* entry_result()-- Generate an entry result within the master
 * function.  The result variable is the second argument to the
 * current function. */

static void entry_result(g95_symbol *sym) {
tree decl, type, tmp;

    if (g95_context == NULL)
	return;

    sym->save = sym->backend_decl;

    decl = DECL_ARGUMENTS(g95_current_ns->proc_name->backend_decl);
    decl = TREE_CHAIN(decl);

    if (sym->as != NULL) {
	tmp = g95_array_node();

	type = g95_get_array_pdesc(sym->as->rank, 0);
	G95_ARRAY_DESC(tmp) = convert(type, decl);
	decl = tmp;

    } else if (!sym->attr.pointer) {
	type = g95_get_typenode(&sym->ts, 0);
	type = build_pointer_type(type);
	decl = convert(type, decl);
    }

    sym->backend_decl = decl;

    if (sym->ts.type == BT_CHARACTER &&
	(sym->ts.cl->length == NULL || sym->ts.cl->backend_decl == NULL)) {
	decl = DECL_ARGUMENTS(g95_current_ns->proc_name->backend_decl);
	decl = TREE_CHAIN(decl);
	decl = TREE_CHAIN(decl);
	decl = g95_build_indirect(g95_default_integer, decl);

	sym->ts.cl->backend_decl = decl;
    }
}



/* restore_entries()-- Restore entry results to functions (possibly). */

static void restore_entries(g95_namespace *ns) {
g95_symbol *sym;

    sym = g95_find_entries(ns->sym_root, 1);

    while(sym != NULL) {
	if (sym->save) {
	    sym->backend_decl = sym->save;
	    sym->save = NULL_TREE;
	}

	sym = sym->tlink;
    }
}



/* deallocate_coarray()-- Deallocate allocatable coarrays. */

static void deallocate_coarray(g95_symbol *sym) {
tree tmp;

    tmp = sym->backend_decl;
    if (G95_ARRAY(tmp))
	tmp = G95_ARRAY_DESC(tmp);

    tmp = g95_addr_expr(tmp);

    tmp = g95_call_library(void_type_node, PREFIX "deallocate_coarray",
			   tmp, integer_zero_node, NULL_TREE);

    g95_add_expr_to_block(&g95_context->post, tmp);
}



/* scalar_coarray()-- Create a scalar coarray variable. */

static void scalar_coarray(g95_symbol *sym) {
tree desc, node, storage, tmp;
g95_se se;

    if (sym->attr.allocatable)
	storage = NULL;

    else {
	storage = g95_create_var(g95_get_typenode(&sym->ts, 1));
	TREE_STATIC(storage) = 1;
	TREE_ADDRESSABLE(storage) = 1;

	sym->backend_decl = storage;   /* For initial_scalar_value() */
	initial_scalar_value(sym);
    }

    desc = coarray_desc(sym, storage);

    sym->backend_decl = node = g95_array_node();
    G95_COARRAY(node) = 1;

    G95_ARRAY_DESC(node) = desc;
    G95_ARRAY_CORANK(node) = integer_zero_node;

    /* Special case-- make sure an otherwise automatic coarray in a
     * program unit is static. */

    if (sym->ns->proc_name->attr.flavor == FL_PROGRAM)
	TREE_STATIC(desc) = 1;

    if (sym->value != NULL) {
	g95_init_se(&se, NULL);
	g95_conv_constant(&se, sym->value);
	DECL_INITIAL(storage) = se.expr;
    }

    if (sym->attr.allocatable && !g95_static_symbol(sym)) {
	tmp = build1(DECL_EXPR, TREE_TYPE(desc), desc);
	g95_add_expr_to_block(&g95_context->pre, tmp);
    }

    if (sym->attr.allocatable && !sym->attr.save && !sym->ns->save_all &&
	!g95_module_symbol(sym) && !sym->attr.artificial)
	deallocate_coarray(sym);
}



/* g95_get_symbol_decl()-- Create the declaration for a symbol. */

void g95_get_symbol_decl(g95_symbol *sym) {

    if (sym->attr.result_var)
	init_result(sym->ns->proc_name);

    else if (sym->attr.flavor == FL_PROCEDURE)
	init_result(sym);

    else if (sym->attr.dummy)
	get_parm_decl(sym);

    else if (sym->as != NULL)
	array_variable(sym);

    else if (sym->cas != NULL)
	scalar_coarray(sym);

    else
	scalar_variable(sym);
}



/* set_purity()-- Set GCC's idea of purity.  A procedure must be
 * fortran pure and have all INTENT(IN) arguments to be what GCC
 * considers pure.  Functions that return types through their first
 * argument cannot be gcc pure. */

static void set_purity(g95_symbol *sym, tree decl) {
g95_formal_arglist *f;
int flag;
bt type;

    if (!sym->attr.pure && !sym->attr.elemental)
	return;

    if (sym->attr.function) {
	type = sym->result->ts.type;
	if (type == BT_CHARACTER || type == BT_DERIVED || type == BT_COMPLEX)
	    return;
    }

    flag = 1;

    for(f=sym->formal; f; f=f->next)
	if (f->sym != NULL && f->sym->attr.intent != INTENT_IN)
	    flag = 0;

    if (flag) {
	DECL_IS_PURE(decl) = 1;
	TREE_SIDE_EFFECTS(decl) = 0;
    }
}



/* g95_function_decl()-- Get a declaration for an external function. */

void g95_function_decl(g95_symbol *sym) {
tree name, type, fndecl, decl;
g95_intrinsic_sym *s;
int external;

    if (sym->backend_decl != NULL || sym->attr.abstract)
	return;

    if (sym->attr.intrinsic)
	external = 0;

    else if (sym->attr.external || sym->attr.entry || sym->attr.subroutine ||
	     sym->attr.if_source == IFSRC_IFBODY ||
	     sym->attr.proc == PROC_MODULE)
	external = 1;

    else
	external = (g95_find_function(sym->name) == NULL);

    if (external)
	name = g95_sym_identifier(sym, 1, NULL);

    else {
	s = g95_find_function(sym->name);
	if (s == NULL)
	    s = g95_find_subroutine(sym->name);

	name = get_identifier(s->actual != NULL ? s->actual : s->lib_name);
    }

    /* Disabled code for function pointer targets, not sure what an
     * abandoned function was. */

    if (0 && external && sym->ts.type == BT_UNKNOWN && !sym->attr.external)
	return;  /* Abandoned function. */

    if (sym->attr.iproc != IPROC_NONE)
	return;

    type = g95_procedure_type(sym);
    fndecl = build_decl(FUNCTION_DECL, name, type);

    /* If the return type is a pointer, avoid alias issues by setting
     * DECL_IS_MALLOC to nonzero. This means that the function should be
     * treated as if it were a malloc, meaning it returns a pointer that
     * is not an alias.  */

    if (POINTER_TYPE_P(type))
	DECL_IS_MALLOC(fndecl) = 1;

    /* Set up all attributes for the function.  */

    DECL_CONTEXT(fndecl) = NULL;
    DECL_EXTERNAL(fndecl) = 1;

    /* This specifies if a function is globally addressable, ie. it is
     * the opposite of declaring static in C. */

    TREE_PUBLIC(fndecl) = 1;

    set_purity(sym, fndecl);
    sym->backend_decl = fndecl;

    /* If the symbol has never been used as a function or subroutine and
     * is marked as external, create a variable that is initialized to a
     * pointer to the subroutine to force a BLOCK DATA to be loaded from
     * an external library. */

    if (sym->attr.external && !sym->attr.function && !sym->attr.subroutine) {
	decl = g95_create_var(pvoid_type_node);

	TREE_STATIC(decl) = 1;
	TREE_USED(decl) = 1;
	TREE_ADDRESSABLE(decl) = 1;
	DECL_INITIAL(decl) = g95_addr_expr(fndecl);
    }
}



/* g95_library_decl()-- Builds a function decl.  The remaining
 * parameters are the types of the function arguments.  Negative nargs
 * indicates a varargs function.  */

tree g95_library_decl(char *name, tree rettype, int nargs, ...) {
tree arglist, argtype, fntype, fndecl;
va_list p;
int n;

    /* Library functions must be declared with global scope.  */
    assert(current_function_decl == NULL_TREE);

    va_start(p, nargs);

    /* Create a list of the argument types */
    for(arglist=NULL_TREE, n=abs(nargs); n>0; n--) {
	argtype = va_arg(p, tree);
	arglist = g95_chainon_list(arglist, argtype);
    }

    /* Terminate the list */

    if (nargs >= 0)
	arglist = g95_chainon_list(arglist, void_type_node);

    /* Build the function type and decl */

    fntype = build_function_type(rettype, arglist);
    fndecl = build_decl(FUNCTION_DECL, get_identifier(name), fntype);

    /* Mark this decl as external */

    DECL_EXTERNAL(fndecl) = 1;
    TREE_PUBLIC(fndecl) = 1;
    va_end(p);

    pushdecl(fndecl);

    rest_of_decl_compilation(fndecl, 1, 0);
    return fndecl;
}



/* g95_build_builtin_decls()-- Make prototypes for runtime
 * library functions. */

void g95_build_builtin_decls(void) {
tree decl, tmp;

    decl = build_decl(VAR_DECL, get_identifier(PREFIX "null_string"),
		      g95_character1_type_node);

    TREE_PUBLIC(decl) = 1;
    DECL_EXTERNAL(decl) = 1;

    null_string_node = g95_addr_expr(decl);

    g95_string_len = build_decl(VAR_DECL, get_identifier(PREFIX "string_len"),
				g95_default_integer);
    DECL_EXTERNAL(g95_string_len) = 1;
    TREE_PUBLIC(g95_string_len) = 1;

    /* Junk status */

    g95_junk_stat = build_decl(VAR_DECL, get_identifier(PREFIX "junk_stat"),
			       g95_default_integer);
    DECL_EXTERNAL(g95_junk_stat) = 1;
    TREE_PUBLIC(g95_junk_stat) = 1;

    empty_stmt_node = build_empty_stmt();

    /* Static not-a-number variables */

    snan_4 = build_decl(VAR_DECL, get_identifier(PREFIX "snan_4"),
			g95_real4_type_node);
    DECL_EXTERNAL(snan_4) = 1;
    TREE_PUBLIC(snan_4) = 1;
    snan_4 = g95_addr_expr(snan_4);

    snan_8 = build_decl(VAR_DECL, get_identifier(PREFIX "snan_8"),
			g95_real8_type_node);
    DECL_EXTERNAL(snan_8) = 1;
    TREE_PUBLIC(snan_8) = 1;
    snan_8 = g95_addr_expr(snan_8);

#if defined(FPU_387) || defined(FPU_SSE)
    snan_10 = build_decl(VAR_DECL, get_identifier(PREFIX "snan_10"),
			 x87_type_node);
    DECL_EXTERNAL(snan_10) = 1;
    TREE_PUBLIC(snan_10) = 1;
    snan_10 = g95_addr_expr(snan_10);
#endif

    snan_16 = build_decl(VAR_DECL, get_identifier(PREFIX "snan_16"),
			 g95_real16_type_node);
    DECL_EXTERNAL(snan_16) = 1;
    TREE_PUBLIC(snan_16) = 1;
    snan_16 = g95_addr_expr(snan_16);

  /* Label info */

    label_info.type = make_node(RECORD_TYPE);

    label_info.label =
	g95_add_field(label_info.type, "label", g95_default_integer);

    label_info.len =
	g95_add_field(label_info.type, "len", g95_default_integer);

    label_info.pointer =
	g95_add_field(label_info.type, "pointer", pvoid_type_node);

    g95_finish_type(label_info.type);

    /* Array constructor info */

    g95_ac_info_type = make_node(RECORD_TYPE);

    g95_add_field(g95_ac_info_type, "desc",         pvoid_type_node);
    g95_add_field(g95_ac_info_type, "dynamic",      g95_pointer_integer);
    g95_add_field(g95_ac_info_type, "full",         g95_pointer_integer);
    g95_add_field(g95_ac_info_type, "real_size",    g95_pointer_integer);
    g95_add_field(g95_ac_info_type, "element_size", g95_pointer_integer);

    tmp = build_index_type(g95_build_int(G95_MAX_DIMENSIONS-1, 0));
    tmp = build_array_type(g95_pointer_integer, tmp);

    g95_add_field(g95_ac_info_type, "index", tmp);
    g95_finish_type(g95_ac_info_type);

    g95_init_array_types();
    g95_init_io();
}



/* specification_variable()-- Given a variable that is part of a
 * specification expression, create it if it needs to be created. */

static void specification_variable(g95_expr *e) {
g95_array_ref *ar;
g95_symbol *sym;
var_chain *s;
g95_ref *ref;
int flag, i;

    sym = e->symbol;
    if (sym->ns != g95_current_ns && !sym->attr.function)
	return;

    if (sym->backend_decl == NULL || (sym->attr.dummy && !sym->mark) ||
	(sym->attr.function && sym->result == sym)) {

	flag = 0;

	for(s=var_base; s; s=s->prev)
	    if (sym == s->sym) {
		flag = 1;

		if (spec_inquire == SPEC_INQUIRE_NONE ||
		    (spec_inquire == SPEC_INQUIRE_LEN &&
		     s->spec == SPEC_LEN) ||
		    (spec_inquire == SPEC_INQUIRE_BOUNDS &&
		     s->spec == SPEC_ARRAY)) {
		    g95_error("Circular specification in variable '%s' at %L",
			      sym->name, &sym->declared_at);
		    return;
		}
	    }

	if (!flag) {
	    find_dependent_vars(sym);
	    g95_create_procedure_variable(sym);
	}
    }

    /* Traverse variable references */

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_SUBSTRING:
	    traverse_spec_expr(ref->u.ss.start);
	    traverse_spec_expr(ref->u.ss.end);
	    break;

	case REF_COMPONENT:
	    break;

	case REF_ARRAY:
	    ar = &ref->u.ar;

	    for(i=0; i<ar->dimen; i++) {
		traverse_spec_expr(ar->start[i]);
		traverse_spec_expr(ar->end[i]);
		traverse_spec_expr(ar->stride[i]);
	    }

	    break;

	case REF_COARRAY:
	    for(i=0; i<ref->u.car.dimen; i++)
		traverse_spec_expr(ref->u.car.element[i]);

	    break;
	}
}



/* traverse_spec_expr()-- Traverse a specification expression, looking
 * for as yet undefined variables. */

static void traverse_spec_expr(g95_expr *e) {
g95_actual_arglist *arg;
int save;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_VARIABLE:
	specification_variable(e);
	break;

    case EXPR_OP:
	traverse_spec_expr(e->value.op.op1);
	traverse_spec_expr(e->value.op.op2);
	break;

    case EXPR_FUNCTION:
	if (e->symbol != NULL && e->symbol->backend_decl == NULL)
	    g95_create_procedure_variable(e->symbol);

	save = spec_inquire;
	if (e->value.function.isym != NULL)
	    switch(e->value.function.isym->id) {

	    case G95_ISYM_LEN:
		spec_inquire = SPEC_INQUIRE_LEN;
		break;

	    case G95_ISYM_LBOUND: case G95_ISYM_UBOUND: case G95_ISYM_SIZE:
	    case G95_ISYM_SHAPE:
		spec_inquire = SPEC_INQUIRE_BOUNDS;
		break;

	    default:
		break;
	    }

	for(arg=e->value.function.actual; arg; arg=arg->next)
	    traverse_spec_expr(arg->u.expr);

	spec_inquire = save;
	break;

    default:
	break;
    }
}



/* create_st_vars()-- Create any variables needed to make a statement
 * function, as well as any other statement functions.  We've already
 * guaranteed that there aren't any recursive statement functions
 * (purely through statement functions). */

static void create_st_vars(g95_expr *e) {
g95_actual_arglist *a;
g95_symbol *sym;
g95_ref *ref;
int i;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	create_st_vars(e->value.op.op1);
	create_st_vars(e->value.op.op2);
	break;

    case EXPR_VARIABLE:
	sym = e->symbol;
	g95_create_procedure_variable(sym);

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++) {
		    create_st_vars(ref->u.ar.start[i]);
		    create_st_vars(ref->u.ar.end[i]);
		    create_st_vars(ref->u.ar.stride[i]);
		}

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    create_st_vars(ref->u.car.element[i]);

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		create_st_vars(ref->u.ss.start);
		create_st_vars(ref->u.ss.end);
		break;
	    }

	break;

    case EXPR_FUNCTION:
	sym = e->symbol;
	if (sym != NULL) {
	    if (sym->attr.proc == PROC_ST_FUNCTION)
		create_st_function(sym);

	    else if (sym->backend_decl == NULL)
		g95_function_decl(sym);
	}

	for(a=e->value.function.actual; a; a=a->next)
	    if (a->u.expr != NULL)
		create_st_vars(a->u.expr);

	break;

    default:
	break;
    }
}



/* create_st_function()-- Create a statement function.  These end up
 * being gcc contained procedures.  The interesting thing about this
 * is that it is a microcosm of what it takes to get a single function
 * out of the back end. */

static void create_st_function(g95_symbol *sym) {
tree *save, fndecl, tmp, result;
g95_trans_context context;
g95_formal_arglist *f;
stmtblock_t block;
g95_se se, len_se;
int n;

    if (sym->backend_decl != NULL)
	return;

    create_st_vars(sym->value);

    /* Save any existing decls of the formal parameters of the statement
     * function, and replace them with PARM_DECLs a little later. */

    n = 0;
    for(f=sym->formal; f; f=f->next)
	n++;

    save = g95_getmem(n * sizeof(tree));

    n = 0;
    for(f=sym->formal; f; f=f->next) {
	save[n++] = f->sym->backend_decl;
	f->sym->backend_decl = NULL_TREE;
    }

    g95_build_procedure_decl(sym);
    fndecl = sym->backend_decl;

    context.parent = g95_context;
    g95_context = &context;

    context.frame_size = 0;
    context.labels = NULL_TREE;
    context.iodesc = NULL;
    context.saved_current_function = current_function_decl;
    context.current_procedure = NULL;
    context.result = NULL_TREE;

    g95_init_block(&context.pre);
    g95_init_block(&context.post);

    push_function_context();
    current_function_decl = fndecl;

    g95_init_se(&se, NULL);

    announce_function(fndecl);
    make_decl_rtl(fndecl);

    g95_set_backend_locus(&sym->declared_at);
    init_function_start(fndecl);
    g95_start_block(&block);
    pushlevel(0);

    switch(sym->ts.type) {
    case BT_CHARACTER:
	se.reflevel = 1;
	/* Fall through */

	g95_conv_expr(&se, sym->value);
	result = DECL_ARGUMENTS(sym->backend_decl);

	g95_add_block_to_block(&block, &context.pre);
	g95_add_block_to_block(&block, &se.pre);

	g95_init_se(&len_se, NULL);
	g95_conv_constant(&len_se, sym->ts.cl->length);

	tmp = g95_call_library(void_type_node, PREFIX "copy_string",
			       result, len_se.expr,
			       se.expr, se.string_length, NULL_TREE);
	g95_add_expr_to_block(&block, tmp);
	break;

    case BT_DERIVED:
    case BT_COMPLEX:
    case BT_INTEGER:
    case BT_REAL:
    case BT_LOGICAL:
	g95_conv_expr(&se, sym->value);

	g95_add_block_to_block(&block, &context.pre);
	g95_add_block_to_block(&block, &se.pre);

	tmp = build2(MODIFY_EXPR, TREE_TYPE(se.expr), DECL_RESULT(fndecl),
		     se.expr);
	tmp = build1(RETURN_EXPR, void_type_node, tmp);
	g95_add_expr_to_block(&block, tmp);

	break;

    default:
	g95_internal_error("create_st_function(): Bad type");
    }

    g95_add_block_to_block(&block, &se.post);
    g95_add_block_to_block(&block, &context.post);

    DECL_SAVED_TREE(fndecl) = g95_finish_block(&block);

    poplevel(1, 0, 1);
    BLOCK_SUPERCONTEXT(DECL_INITIAL(fndecl)) = fndecl;

    cfun->function_end_locus = input_location;
    cfun = NULL;

    dump_function(TDI_generic, fndecl);
    pop_function_context();

    current_function_decl = context.saved_current_function;
    g95_context = context.parent;

    n = 0;
    for(f=sym->formal; f; f=f->next)
	f->sym->backend_decl = save[n++];

    g95_free(save);
}



/* find_dependent_vars()-- Given a symbol that we are about to create,
 * traverse any array specification or character length, and find
 * other variables that need to be created before this one.  Mutually
 * recursive with g95_create_procedure_variable().  If the stack depth
 * exceeds the number of symbols in the namespace, then we have found
 * variables that depend on one other, which shouldn't happen.  The
 * symbol is guaranteed to be a variable.  This recursive solution
 * avoids a nasty topological sort. */

static void find_dependent_vars(g95_symbol *sym) {
g95_array_spec *as;
var_chain stack;
g95_symbol *s;
int i;

    for(s=sym_head; s; s=s->old_symbol)
	if (s == sym)
	    break;

    if (s != sym) {  /* Add if not there already */
	sym->old_symbol = sym_head;
	sym_head = sym;
    }

    stack.sym  = sym;
    stack.prev = var_base;
    var_base   = &stack;
    stack.spec = SPEC_LEN;

    if (sym->ts.type == BT_CHARACTER && sym->ts.cl != NULL &&
	sym->ts.cl->length != NULL)
	traverse_spec_expr(sym->ts.cl->length);

    as = sym->as; 
    if (as != NULL) {
	stack.spec = SPEC_ARRAY;

	for(i=0; i<as->rank; i++) {
	    traverse_spec_expr(as->lower[i]);
	    traverse_spec_expr(as->upper[i]);
	}
    }

    var_base = stack.prev;
}



/* unused_external()-- For an unused external procedure, create a
 * static variable that points to it so that the corresponding block
 * data symbol is pulled in from a library. */

static void unused_external(g95_symbol *sym) {
tree decl;

    decl = g95_create_var(pvoid_type_node);

    TREE_STATIC(decl) = 1;
    g95_function_decl(sym);
    DECL_INITIAL(decl) = g95_addr_expr(sym->backend_decl);
}



/* fix_dummy_coarray()-- Replace scalar PARM_DECL coarray with a
 * descriptor, and initialize the descriptor. */

static void fix_dummy_coarray(g95_symbol *sym) {
tree parm, type, desc, array, field, tmp, extent, info, upper, lower;
tree multiplier, offset;
stmtblock_t block;
int rank, i, m;
g95_se se;

    parm = sym->backend_decl;
    if (TREE_CODE(parm) != PARM_DECL)
	return;

    rank = sym->as == NULL
	? 0
	: sym->as->rank;

    type = g95_get_array_desc(rank, sym->cas->corank);

    desc = build_decl(VAR_DECL, g95_sym_identifier(sym, 0, "desc"), type);
    pushdecl(desc);
    finish_var_decl(desc, sym);

    array = g95_array_node();

    G95_ARRAY_DESC(array)   = desc;
    G95_ARRAY_RANK(array)   = g95_build_int(rank, 0);
    G95_ARRAY_CORANK(array) = g95_build_int(sym->cas->corank, 0);
    G95_ARRAY_DUMMY(array)  = parm;
    G95_ARRAY_BASE(array)   = parm;
    G95_COARRAY(array)      = 1;

    g95_init_block(&block);

    tmp   = g95_base_ref(desc);
    g95_add_modify_expr(&block, tmp, parm);

    field = g95_find_field(type, "info");
    info  = g95_build_component(TREE_TYPE(field), desc, field);

    g95_add_modify_expr(&block, g95_esize_ref(desc), g95_ts_size(&sym->ts));

    g95_add_modify_expr(&block, g95_rank_ref(desc),
			G95_ARRAY_RANK(array));

    g95_add_modify_expr(&block, g95_corank_ref(desc),
			G95_ARRAY_CORANK(array));

    if (sym->as != NULL && sym->as->type == AS_ASSUMED_SHAPE) {
	for(i=0; i<rank; i++) {
	    tmp = g95_lbound_ref(desc, i);

	    if (sym->as->lower[i] == NULL) {
		G95_ARRAY_LBOUND(array, i) =
		    convert(g95_pointer_integer, integer_one_node);

		g95_add_modify_expr(&block, tmp, integer_one_node);

	    } else {
		g95_init_se(&se, NULL);
		g95_conv_spec_expr(&se, sym->as->lower[i]);
		g95_add_block_to_block(&block, &se.pre);

		g95_add_modify_expr(&block, tmp, se.expr);
		g95_add_block_to_block(&block, &se.post);
	    }
	}

	tmp = g95_call_library(void_type_node, PREFIX "init_assumed_shape",
			       parm, g95_addr_expr(G95_ARRAY_DESC(array)),
			       null_pointer_node, NULL_TREE);
	g95_add_expr_to_block(&block, tmp);

    } else {
	/* Build the regular part of the descriptor. */
	offset = g95_base_ref(desc);

	for(i=0; i<rank; i++) {
	    g95_init_se(&se, NULL);
	    g95_conv_spec_expr(&se, sym->as->lower[i]);
	    g95_add_block_to_block(&block, &se.pre);

	    tmp = g95_lbound_ref(desc, i);
	    g95_add_modify_expr(&block, tmp, se.expr);
	    g95_add_block_to_block(&block, &se.post);

	    if (sym->as->upper[i] != NULL) {
		g95_init_se(&se, NULL);
		g95_conv_spec_expr(&se, sym->as->upper[i]);
		g95_add_block_to_block(&block, &se.pre);
	    }

	    tmp = g95_ubound_ref(desc, i);
	    g95_add_modify_expr(&block, tmp, se.expr);
	    g95_add_block_to_block(&block, &se.post);

	    if (i == 0) {
		tmp = g95_multiplier_ref(desc, 0);
		g95_add_modify_expr(&block, tmp, g95_ts_size(&sym->ts));

	    } else {
		extent = g95_build_minus(g95_pointer_integer,
					 g95_ubound_ref(desc, i-1),
					 g95_lbound_ref(desc, i-1));

		extent = g95_build_plus(g95_pointer_integer, extent,
					integer_one_node);

		extent = g95_build_max(g95_pointer_integer, extent,
				       convert(g95_pointer_integer,
					       integer_zero_node));

		multiplier = g95_build_mult(g95_pointer_integer, extent,
					    g95_multiplier_ref(desc, i-1));

		tmp = g95_multiplier_ref(desc, i);
		g95_add_modify_expr(&block, tmp, multiplier);
	    }

	    tmp = g95_build_mult(g95_pointer_integer,
				 g95_lbound_ref(desc, i),
				 g95_multiplier_ref(desc, i));

	    offset = g95_build_minus(g95_pointer_integer, offset, tmp);
	}

	if (rank > 0) {
	    tmp = g95_offset_ref(desc);
	    g95_add_modify_expr(&block, tmp, offset);
	}
    }

    /* Build the coarray part of the descriptor. */

    if (!sym->attr.allocatable) {
	for(m=0; m<sym->cas->corank; m++) {
	    i = rank + m;

	    g95_init_se(&se, NULL);
	    g95_conv_spec_expr(&se, sym->cas->lower[m]);
	    g95_add_block_to_block(&block, &se.pre);

	    tmp = g95_build_int(3*i+1, 0);
	    lower = g95_build_array_ref(g95_pointer_integer, info, tmp);

	    g95_add_modify_expr(&block, lower, se.expr);
	    g95_add_block_to_block(&block, &se.post);

	    if (m != sym->cas->corank - 1) {
		g95_init_se(&se, NULL);
		g95_conv_spec_expr(&se, sym->cas->upper[m]);
		g95_add_block_to_block(&block, &se.pre);

		tmp = g95_build_int(3*i+2, 0);
		upper = g95_build_array_ref(g95_pointer_integer, info, tmp);

		g95_add_modify_expr(&block, upper, se.expr);

		extent = g95_build_minus(g95_pointer_integer, upper, lower);
		tmp = convert(g95_pointer_integer, integer_one_node);

		extent = g95_build_plus(g95_pointer_integer, tmp, extent);
		tmp = g95_build_int(3*i, 0);
		tmp = g95_build_array_ref(g95_pointer_integer, info, tmp);

		g95_add_modify_expr(&block, tmp, extent);
		g95_add_block_to_block(&block, &se.post);
	    }
	}

	tmp = g95_addr_expr(desc);
	tmp = g95_call_library(void_type_node, PREFIX "last_co_ubound",
			       tmp, NULL);

	g95_add_expr_to_block(&block, tmp);
    }    

    if (sym->attr.intent == INTENT_OUT && sym->ts.type == BT_DERIVED &&
	!sym->attr.pointer && sym->ts.derived->attr.itype == ITYPE_NONE &&
	g95_allocatable_component(sym->ts.derived)) {

	tmp = G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl);
	tmp = g95_call_library(void_type_node, PREFIX "deep_dealloc",
			       g95_addr_expr(desc), tmp, NULL_TREE);
	g95_add_expr_to_block(&block, tmp);
    }

    tmp = g95_finish_block(&block);
    g95_add_expr_to_block(&g95_context->pre, tmp);

    sym->backend_decl = array;
}


/* add_array_fixup()-- Add code to fix a dummy array to the startup
 * code. */

static void add_array_fixup(g95_symbol *sym, tree decl) {
g95_symbol *s;
tree tmp, t;

    if (sym_head != sym &&
	g95_context->current_procedure->proc_name->attr.artificial) {

	tmp = boolean_true_node;

	for(s=sym_head; s!=sym && s!=NULL; s=s->old_symbol) {
	    if (!s->attr.dummy)
		continue;

	    t = s->backend_decl;
	    if (s->attr.dimension)
		t = G95_ARRAY_DUMMY(t);

	    t = g95_build_ne(t, null_pointer_node);
	    tmp = g95_build_andif(t, tmp);
	}

	if (decl == NULL)
	    decl = empty_stmt_node;

	decl = g95_build_cond(void_type_node, tmp, decl, empty_stmt_node);
    }

    g95_add_expr_to_block(&g95_context->pre, decl);
}



/* g95_create_procedure_variable()-- Create a variable */

void g95_create_procedure_variable(g95_symbol *sym) {
tree alloc, kind, tmp, var, cond;
g95_intrinsic_sym *isym;
stmtblock_t block;
g95_typespec ts;

    if (sym->attr.entry_result && sym->save == NULL) {
	entry_result(sym);
	return;
    }

    if (sym->attr.proc == PROC_ST_FUNCTION) {
	create_st_function(sym);
	return;
    }

    if (sym->ns != g95_current_ns && g95_current_ns->proc_name != sym)
	return;

    if (sym->attr.dummy && !sym->mark) {
	find_dependent_vars(sym);

	if (sym->cas != NULL) {
	    fix_dummy_coarray(sym);
	    return;
	}

	if (sym->ts.type == BT_CHARACTER && sym->ts.cl->backend_decl == NULL) {
	    g95_init_block(&block);
	    g95_init_character_len(&block, sym->ts.cl, NULL_TREE);
	    add_array_fixup(sym, g95_finish_block(&block));
	}

	if (!sym->attr.function && sym->attr.dimension && sym->cas == NULL) {
	    tmp = g95_fix_dummy_array(sym);
	    if (tmp != NULL_TREE)
		add_array_fixup(sym, tmp);

	    if (sym->attr.allocatable && sym->attr.intent == INTENT_OUT) {
		kind = g95_build_int(g95_default_integer_kind(0), 0);

		alloc = (sym->ts.type == BT_DERIVED &&
		 G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl) != NULL_TREE)
		    ? G95_DTYPE_ALLOCS(sym->ts.derived->backend_decl)
		    : null_pointer_node;

		var = G95_ARRAY_DESC(sym->backend_decl);
		tmp = g95_call_library(void_type_node,
				       PREFIX "deallocate_array", var, alloc,
				       integer_one_node, NULL_TREE);

		if (sym->attr.optional) {
		    cond = g95_build_ne(var, null_pointer_node);
		    tmp = g95_build_cond(void_type_node, cond, tmp,
					 empty_stmt_node);
		}

		g95_add_expr_to_block(&g95_context->pre, tmp);
	    }

	    sym->mark = 1;
	}

	return;
    }

    if (g95_context != NULL && sym->attr.function &&
	sym == g95_context->current_procedure->proc_name)
	goto doit;

    if (sym->backend_decl != NULL || sym->attr.in_common)
	return;

    if (sym->attr.flavor == FL_PROCEDURE &&
	(sym->attr.dummy_proc || sym->attr.if_source == IFSRC_IFBODY ||
	 sym->attr.entry || sym->attr.external || sym->attr.intrinsic ||
	 (sym->attr.proc != PROC_INTRINSIC &&
	  sym->attr.proc != PROC_UNKNOWN))) {

	if (sym->attr.intrinsic) {
	    isym = g95_find_function(sym->name);
	    if (isym == NULL || isym->ts.type == BT_UNKNOWN)
		return;
	}


	g95_function_decl(sym);
	return;
    }

    if (sym->attr.flavor == FL_DERIVED && sym->backend_decl == NULL_TREE) {
	memset(&ts, '\0', sizeof(ts));

	ts.type = BT_DERIVED;
	ts.derived = sym;

	g95_get_typenode(&ts, 0);
	return;
    }

    if (sym->attr.external && !sym->attr.used)
	unused_external(sym);

    if (sym->attr.flavor != FL_VARIABLE &&
	sym->attr.flavor != FL_PARAMETER)
	return;

    if (sym->attr.flavor == FL_PARAMETER && sym->as == NULL &&
	(sym->ts.type != BT_DERIVED && sym->ts.type != BT_CHARACTER))
	return;

    if (!sym->attr.used && !sym->attr.set &&
	sym->ns->proc_name->attr.flavor != FL_MODULE)
	return;

    /* At this point, start thinking about creating the variable */

doit:
    find_dependent_vars(sym);

    g95_get_symbol_decl(sym);
}



/* create_procvar()-- Recursively traverse a namespace, creating
 * variables as we go. */

static void create_procvar(g95_symtree *st) {

    if (st == NULL)
	return;

    create_procvar(st->left);
    create_procvar(st->right);

    sym_head = NULL;
    g95_create_procedure_variable(st->n.sym);
}



/* fixup_pointer()-- Fix scalar pointers for a single symbol.  These
 * aren't filled in on the first pass. */

static void fixup_pointer(g95_symbol *sym) {
variable_info vinfo;
g95_component *c;
tree field;

    if (sym->attr.flavor != FL_DERIVED || sym->attr.itype != ITYPE_NONE)
	return;

    field = TYPE_FIELDS(G95_DTYPE_TYPE(sym->backend_decl));

    for(c=sym->components; c; c=c->next, field=TREE_CHAIN(field)) {
	if (c->ts.type != BT_DERIVED || !c->pointer || c->as != NULL)
	    continue;

	g95_component_vinfo(c, &vinfo);
	TREE_TYPE(field) = g95_get_descriptor(&vinfo);
    }
}



/* fixup_pointers()-- Recursively traverse a namespace, fixing scalar
 * pointers to derived types. */

static void fixup_pointers(g95_symtree *st) {

    if (st == NULL)
	return;

    fixup_pointers(st->left);
    fixup_pointers(st->right);

    fixup_pointer(st->n.sym);
}



/* save_charlens()-- Transform character lengths that are SAVE_EXPRs
 * into real variables. */

static void save_charlens(g95_symtree *st) {
g95_symbol *sym;
g95_se se;

    if (st == NULL)
	return;

    save_charlens(st->left);
    save_charlens(st->right);

    sym = st->n.sym;

    if (sym->attr.flavor == FL_VARIABLE && sym->ts.type == BT_CHARACTER &&
	sym->ts.cl->backend_decl != NULL_TREE &&
	TREE_CODE(sym->ts.cl->backend_decl) == SAVE_EXPR) {

	g95_init_se(&se, NULL);
	se.expr = sym->ts.cl->backend_decl;
	g95_save_expr(&se);

	sym->ts.cl->backend_decl = se.expr;

	g95_add_block_to_block(&g95_context->pre, &se.pre);
	g95_add_block_to_block(&g95_context->post, &se.post);
    }
}



/* g95_generate_procedure_variables()-- Generate variables. */

void g95_generate_procedure_variables(g95_namespace *ns) {

    create_procvar(ns->sym_root);
    create_procvar(ns->sym_root);

    save_charlens(ns->sym_root);
    fixup_pointers(ns->sym_root);
}



/* g95_build_procedure_decl()-- Create a declaration for a procedure.
 * For external functions (in the C sense) use g95_function_decl(). */

void g95_build_procedure_decl(g95_symbol *sym) {
tree proc_decl, type, result_decl, typelist, arglist, arglist_tail,
     parm, result_pointer, result_length, tmp;
symbol_attribute attr;
g95_formal_arglist *f;
g95_symbol *formal;

    if (sym->backend_decl)
	return;

    assert(!sym->attr.external);

    attr = sym->attr;
    type = g95_procedure_type(sym);
    tmp  = g95_sym_identifier(sym, 1, NULL);

    input_location.line = sym->declared_at.lb->linenum;

    proc_decl = build_decl(FUNCTION_DECL, tmp, type);

    /* Figure out the return type of the procedure, and build a
     * RESULT_DECL for it.  If this is subroutine with alternate
     * returns, build a RESULT_DECL for it. */

    result_decl = build_decl(RESULT_DECL, NULL_TREE, g95_result_type(sym));
    DECL_CONTEXT(result_decl) = proc_decl;
    DECL_RESULT(proc_decl) = result_decl;

    /* Don't call layout_decl for a RESULT_DECL.
       layout_decl(result_decl, 0); */

    /* If the return type is a pointer, avoid alias issues by setting
     * DECL_IS_MALLOC to nonzero. This means that the function should
     * be treated as if it were a malloc, meaning it returns a pointer
     * that is not an alias. */

    if (POINTER_TYPE_P(type))
	DECL_IS_MALLOC(proc_decl) = 1;

    /* Set up all attributes for the function. */

    DECL_EXTERNAL(proc_decl) = 0;
    DECL_CONTEXT(proc_decl) = sym->ns->backend_decl;

    cgraph_node(proc_decl);

    /* This specifies if a function is globally addressable, ie. it is
     * the opposite of declaring static in C.  */

    if (current_function_decl == NULL_TREE &&
	(!sym->attr.artificial || sym->attr.flavor == FL_PROGRAM))
	TREE_PUBLIC(proc_decl) = 1;

    /* TREE_STATIC means the function body is defined here.  */
    TREE_STATIC(proc_decl) = 1;

    set_purity(sym, proc_decl);

    /* Layout the function declaration and put it in the binding level
     * of the current function.  */

    pushdecl(proc_decl);

    /* Build formal argument list. Make sure that their TREE_CONTEXT
     * is the new FUNCTION_DECL node. */

    current_function_decl = proc_decl;
    arglist = NULL_TREE;
    arglist_tail = NULL_TREE;

    typelist = TYPE_ARG_TYPES(TREE_TYPE(proc_decl));

    /* Declare prepended arguments used for returning non-simple values */

    result_pointer = NULL_TREE;
    result_length  = NULL_TREE;

    if (sym->attr.function && !sym->result->attr.pointer &&
	sym->result->as == NULL)

	if (sym->result->ts.type == BT_CHARACTER) {
	    tmp = g95_sym_identifier(sym, 0, "result");
	    result_pointer = build_decl(PARM_DECL, tmp, pchar_type_node);

	    DECL_CONTEXT(result_pointer) = proc_decl;
	    DECL_ARG_TYPE(result_pointer) = pchar_type_node;

	    finish_decl(result_pointer, NULL_TREE);

	    arglist = chainon(arglist, result_pointer);
	    typelist = TREE_CHAIN(typelist);

	    tmp = g95_sym_identifier(sym, 0, "result_len");
	    result_length = build_decl(PARM_DECL, tmp, g95_default_integer);

	    DECL_CONTEXT(result_length)  = proc_decl;
	    DECL_ARG_TYPE(result_length) = g95_default_integer;
	    TREE_USED(result_length)     = 1;

	    finish_decl(result_length, NULL_TREE);

	    arglist = chainon(arglist, result_length);
	    typelist = TREE_CHAIN(typelist);
	}

    for(f=sym->formal; f; f=f->next) {
	formal = f->sym;
	if (formal == NULL)
	    continue;   /* ignore alt return placeholders. */

	formal->mark = 0;

	if (formal->backend_decl == NULL)
	    get_parm_decl(formal);

	parm = formal->backend_decl;
	DECL_CONTEXT(parm) = proc_decl;

	if (formal->attr.used)
	    TREE_USED(parm) = 1;

	finish_decl(parm, NULL_TREE);

	arglist  = chainon(arglist, parm);
	typelist = TREE_CHAIN(typelist);

	if (formal->ts.type == BT_CHARACTER) {
	    parm = build_decl(PARM_DECL, g95_sym_identifier(formal, 0, "len"),
			      g95_default_integer);

	    DECL_CONTEXT(parm) = proc_decl;
	    DECL_ARG_TYPE(parm) = g95_default_integer;

#if TARGET_GCC_VERSION < 410
	    DECL_ARG_TYPE_AS_WRITTEN(parm) = g95_default_integer;
#endif

	    TREE_READONLY(parm) = 1;
	    TREE_USED(parm) = 1;
	    finish_decl(parm, NULL_TREE);

	    arglist_tail = chainon(arglist_tail, parm);

	    /* Give an assumed length character its length, otherwise ignore.*/

	    if (formal->ts.cl->length == NULL)
		formal->ts.cl->backend_decl = parm;
	}
    }

    arglist = chainon(arglist, arglist_tail);
    DECL_ARGUMENTS(proc_decl) = arglist;

    /* Restore the old context */
    current_function_decl = DECL_CONTEXT(proc_decl);

    sym->backend_decl = proc_decl;
}



/* init_parameters()-- Initialize INTENT(OUT) derived type parameters
 * that have a default initialization.  Arrays are handled elsewhere. */

static void init_parameters(g95_symbol *sym) {
g95_formal_arglist *f;
stmtblock_t block;
g95_symbol *parm;
tree value, tmp;

    for(f=sym->formal; f; f=f->next) {
	parm = f->sym;

	if (parm == NULL       || parm->ts.type != BT_DERIVED     ||
	    parm->attr.pointer || parm->attr.intent != INTENT_OUT ||
	    parm->as != NULL   || parm->cas != NULL               ||
	    parm->ts.derived->attr.itype != ITYPE_NONE)
	    continue;

	value = G95_DTYPE_INITIAL(parm->ts.derived->backend_decl);
	if (value == NULL)
	    continue;

	g95_init_block(&block);

	if (g95_allocatable_component(parm->ts.derived)) {
	    tmp = G95_DTYPE_ALLOCS(parm->ts.derived->backend_decl);
	    tmp = g95_call_library(void_type_node, PREFIX "deep_dealloc",
				   parm->backend_decl, tmp, NULL_TREE);
	    g95_add_expr_to_block(&block, tmp);
	}

	tmp = parm->backend_decl;
	tmp = g95_build_indirect(TREE_TYPE(value), tmp);

	g95_add_modify_expr(&block, tmp, value);

	tmp = g95_build_ne(parm->backend_decl, null_pointer_node);
	tmp = g95_build_cond(void_type_node, tmp, g95_finish_block(&block),
			     empty_stmt_node);

	g95_add_expr_to_block(&g95_context->pre, tmp);
    }
}



/* pointer_array_return()-- Generate code for a pointer array return.
 * Up until now, the result variable has been an array descriptor, but
 * we now have to return a pointer to the descriptor.  The descriptor
 * is on the stack, so we copy the variable to a static temporary and
 * return a pointer to the temporary. */

static tree pointer_array_return(stmtblock_t *block) {
tree var;

    var = g95_create_var(TREE_TYPE(G95_ARRAY_DESC(g95_context->result)));
    TREE_STATIC(var) = 1;
    TREE_ADDRESSABLE(var) = 1;

    g95_add_modify_expr(block, var, G95_ARRAY_DESC(g95_context->result));

    return g95_addr_expr(var);
}



/* swap_charlen()-- Given a namespace, swap all character length decls
 * in its siblings with the saved version.  This prevents the target
 * namespace from accessing character lengths defined in sibling
 * spaces. */

static void swap_charlen(g95_namespace *target) {
g95_namespace *ns;
g95_symbol *sym;
g95_charlen *cl;

    if (target->parent == NULL)
	return;

    for(ns=target->parent->contained; ns; ns=ns->sibling) {
	if (ns->state != COMP_FUNCTION || ns == target)
	    continue;

	sym = ns->proc_name->result;
	if (sym->ts.type != BT_CHARACTER)
	    continue;

	cl = sym->ts.cl;

	if (cl->save != NULL_TREE) {
	    cl->backend_decl = cl->save;
	    cl->save = NULL_TREE;
	
	} else if (cl->backend_decl != NULL_TREE) {
	    cl->save = cl->backend_decl;
	    cl->backend_decl =
		(cl->length != NULL && cl->length->type == EXPR_CONSTANT) 
		? bi_to_tree(cl->length->value.integer, -1)
		: integer_zero_node;
	}
    }
}



/* gimplify_function-- Convert to gimple */

static void gimplify_function(tree fndecl) {
struct cgraph_node *cgn;

    gimplify_function_tree(fndecl);

    /* Convert all nested functions to GIMPLE now.  We do things in this order
       so that items like VLA sizes are expanded properly in the context of the
       correct function.  */

    cgn = cgraph_node(fndecl);

    for(cgn=cgn->nested; cgn; cgn=cgn->next_nested)
	gimplify_function(cgn->decl);
}



/* finalize()-- Finalize DECL and all nested functions with cgraph.  */

static void finalize(tree decl) {
struct cgraph_node *cgn;

    cgn = cgraph_node(decl);
    for(cgn=cgn->nested; cgn; cgn=cgn->next_nested)
	finalize(cgn->decl);

    cgraph_finalize_function(decl, false);
}



/* init_trace()-- Initialize trace information. */

static void init_trace(g95_namespace *ns) {
tree tmp, var;

    if (g95_option.trace == TRACE_NONE || ns->code == NULL ||
	ns->proc_name->attr.artificial)
	return;

    if (ns->state == COMP_PROGRAM) {
	g95_set_locus(&g95_context->pre, &ns->code->where);
	return;
    }

    var = g95_create_var(g95_frame_type.type);

    tmp = g95_build_component(TREE_TYPE(g95_frame_type.filename), var,
			      g95_frame_type.filename);
    g95_add_modify_expr(&g95_context->pre, tmp, g95_locus_file);

    tmp = g95_build_component(TREE_TYPE(g95_frame_type.line), var,
			      g95_frame_type.line);
    g95_add_modify_expr(&g95_context->pre, tmp, g95_locus_line);

    tmp = g95_build_component(TREE_TYPE(g95_frame_type.next), var,
			      g95_frame_type.next);
    g95_add_modify_expr(&g95_context->pre, tmp, g95_frame_type.base);

    tmp = g95_addr_expr(var);
    g95_add_modify_expr(&g95_context->pre, g95_frame_type.base, tmp);

    if (g95_option.trace == TRACE_FRAME)
	g95_add_modify_expr(&g95_context->pre, g95_locus_line,
			    integer_zero_node);

    /* Procedure end */

    tmp = g95_build_component(TREE_TYPE(g95_frame_type.next), var,
			      g95_frame_type.next);
    g95_add_modify_expr(&g95_context->post, g95_frame_type.base, tmp);
}



/* label_cons()-- Build a label constructor. */

static tree label_cons(tree label, tree len, tree pointer) {
tree node;

    node = tree_cons(label_info.label, label, NULL_TREE);
    node = tree_cons(label_info.len, len, node);
    node = tree_cons(label_info.pointer, pointer, node);

    return g95_build_constructor(label_info.type, nreverse(node));
}



/* finish_labels()-- Populate the elements of the label constructor
 * now that they are defined. */

static void finish_labels(void) {
tree array, node, type, len;
g95_st_label *m;
int n;

    array = NULL_TREE;
    n = 0;

    for(m=g95_current_ns->st_labels; m; m=m->next) {
	if (m->value > 99999)
	    continue;

	switch(m->defined) {
	case ST_LABEL_TARGET:
	    if (!g95_current_ns->goto_label)
		continue;

	    FORCED_LABEL(m->backend_decl) = 1;
	    node = label_cons(g95_build_int(m->value, 0),
			      convert(g95_default_integer,
				      integer_minus_one_node),
			      g95_addr_expr(m->backend_decl));
	    break;

	case ST_LABEL_FORMAT:
	    if (!g95_current_ns->format_label)
		continue;

	    g95_conv_format(m);

	    len = g95_build_int(TREE_STRING_LENGTH(m->backend_decl), 0);
	    node = label_cons(g95_build_int(m->value, 0), len, 
			      g95_addr_expr(m->backend_decl));
	    break;

	default:
	    continue;
	}

	array = tree_cons(g95_build_int(n++, 0), node, array);
    }

    node = label_cons(integer_zero_node, integer_zero_node,
		      null_pointer_node);
    array = tree_cons(g95_build_int(n, 0), node, array);

    type = build_array_type(label_info.type,
			    build_index_type(g95_build_int(n-1, 0)));

    DECL_INITIAL(g95_context->labels) =
	g95_build_constructor(type, nreverse(array));
}



/* init_labels()-- Initialize labels. */

static void init_labels(void) {
g95_st_label *p;
tree type;
int n;

    g95_context->labels = g95_build_constructor(label_info.type, NULL_TREE);

    n = 1;

    for(p=g95_current_ns->st_labels; p; p=p->next) {
	if (p->value > 99999)
	    continue;

	switch(p->defined) {
	case ST_LABEL_TARGET:
	case ST_LABEL_FORMAT:
	    n++;

	default:
	    break;
	}
    }

    type = build_array_type(label_info.type,
			    build_index_type(g95_build_int(n-1, 0)));

    g95_context->labels = g95_create_var(type);

    TREE_CONSTANT(g95_context->labels) = 1;
    TREE_STATIC(g95_context->labels) = 1;
    TREE_ADDRESSABLE(g95_context->labels) = 1;
}



/* init_intent_out()-- Initialize INTENT(OUT) variables if we should. */

static void init_intent_out(g95_symbol *proc) {
tree t, tmp, var, value;
g95_formal_arglist *f;
g95_symbol *sym;

    for(f=proc->formal; f; f=f->next) {
	sym = f->sym;

	if (sym == NULL || sym->as != NULL || sym->attr.intent != INTENT_OUT)
	    continue;

	value = g95_default_scalar_value(sym);
	if (value == NULL)
	    continue;

	var = sym->backend_decl;

	if (!sym->attr.pointer && sym->ts.type == BT_REAL &&
	    g95_option.real_init == REAL_INIT_NAN)
	    tmp = init_nan(var, &sym->ts);

	else {
	    var = g95_build_indirect(TREE_TYPE(TREE_TYPE(var)), var);
	    tmp = build2(MODIFY_EXPR, TREE_TYPE(var), var, value);
	}

	if (sym->attr.optional) {
	    t = g95_build_ne(sym->backend_decl, null_pointer_node);
	    tmp = g95_build_cond(void_type_node, t, tmp, empty_stmt_node);
	}

	g95_add_expr_to_block(&g95_context->pre, tmp);
    }
}



/* g95_dump_coarray()-- Dump coarray section. */

void g95_dump_coarray(g95_symtree *st, int *flag) {
g95_symbol *sym;
unsigned size;
tree desc;
rtx r;

    if (st == NULL)
	return;

    g95_dump_coarray(st->left,  flag);
    g95_dump_coarray(st->right, flag);

    sym = st->n.sym;
    if (sym->cas == NULL    || sym->attr.dummy || sym->attr.allocatable ||
	sym->attr.use_assoc || sym->backend_decl == NULL ||
	sym->cas->artificial)
	return;

    if (*flag) {
#ifdef __APPLE__
	named_section(NULL, ".g95coarray, coarray", 0);
#else
	named_section(NULL, ".g95coarray", 0);
#endif
	*flag = 0;
    }

    desc = G95_ARRAY_DESC(sym->backend_decl);

    r = expand_expr(g95_addr_expr(desc), NULL_RTX, Pmode, 0);
    assemble_integer(r, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);

    size = g95_int_ts_size(&sym->ts);

    if (sym->as != NULL)
	size *= bi_to_int(g95_array_spec_size(sym->as));

    assemble_integer(GEN_INT(size), POINTER_SIZE/BITS_PER_UNIT,
		     POINTER_SIZE, 1);
}



/* g95_generate_procedure()-- Generate code for a procedure.  */

void g95_generate_procedure(g95_namespace *ns) {
tree fndecl, tmp, saved_function_decl;
g95_trans_context context;
stmtblock_t block, body;
g95_namespace *child;
g95_symbol *sym;
int n;

    g95_current_ns = ns;
    sym = ns->proc_name;
    ns->backend_decl = sym->backend_decl;

    n = sym->attr.entry_result;
    sym->attr.entry_result = 0;

    /* Create the declaration for functions with global scope */

    context.parent = g95_context;
    g95_context = &context;

    context.frame_size = 0;
    context.labels = NULL_TREE;
    context.iodesc = NULL;
    context.saved_current_function = current_function_decl;
    context.current_procedure = ns;
    context.result = NULL_TREE;

    g95_init_block(&context.pre);
    g95_init_block(&context.post);

    if (context.parent != NULL)
	push_function_context();

    /* let GCC know the current scope is this function */

    current_function_decl = fndecl = sym->backend_decl;
    context.current_function_decl = fndecl;

    /* print function name on the console at compile time unless this
     * feature was switched of by command line option "-quiet" */

    announce_function(fndecl);

    /* create RTL for function declaration */

    if (DECL_CONTEXT(fndecl) == NULL_TREE)
	rest_of_decl_compilation(fndecl, 1, 0);

    /* create RTL for function definition */
    make_decl_rtl(fndecl);

    /* Set the line and filename.  sym->declared_at seems to point to
     * the last statement for subroutines, but it'll do for now. */

    g95_set_backend_locus(&sym->declared_at);

    /* line and file should not be 0 */

    init_function_start(fndecl);

    /* Even though we're inside a function body, we still don't want to
     * call expand_expr to calculate the size of a variable-sized array.
     * We haven't necessarily assigned RTL to all variables yet, so it's
     * not safe to try to expand expressions involving them.  */

    cfun->x_dont_save_pending_sizes_p = 1;

    g95_start_block(&block);

    /* function.c requires a push at the start of the function */

    pushlevel(0);

    if (ns->format_label || ns->goto_label)
	init_labels();

    /* Now generate the code for the body of this function */

    g95_init_block(&body);

    /* Procedure declarations for contained procedures */

    for(child=ns->contained; child; child=child->sibling)
	if (child->parent == ns)
	    g95_build_procedure_decl(child->proc_name);

    swap_charlen(ns);

    /* Generate common blocks */

    g95_trans_common(ns);

    if (sym->attr.subroutine)
	init_result(sym);

    current_function_return_label = NULL;

    /* If we have a function without an explicit result variable, the
     * function name is the result variable.  Save the declaration and
     * replace it with the result variable.  When done, restore the
     * original declaration. */

    saved_function_decl = NULL_TREE;

    if (sym->attr.function && sym->result == sym) {
	saved_function_decl = sym->backend_decl;
	sym->backend_decl = NULL;
    }

    g95_generate_procedure_variables(ns);

    if (sym->attr.function && sym->result->as == NULL &&
	(sym->ts.type == BT_INTEGER || sym->ts.type == BT_REAL ||
	 sym->ts.type == BT_LOGICAL)) {

	default_scalar_init(sym->result);

	tmp = sym->result->backend_decl;
	if (DECL_INITIAL(tmp) != NULL_TREE) {
	    tmp = build1(DECL_EXPR, TREE_TYPE(tmp), tmp);
	    g95_add_expr_to_block(&g95_context->pre, tmp);
	}
    }

    init_parameters(sym);
    init_trace(ns);
    init_intent_out(sym);

    tmp = g95_trans_code(ns->code);

    g95_add_block_to_block(&body, &context.pre);
    g95_add_expr_to_block(&body, tmp);

    /* Add a return label if needed. */

    if (current_function_return_label) {
	tmp = g95_build_label(current_function_return_label);
	g95_add_expr_to_block(&body, tmp);
    }

    tmp = g95_finish_block(&body);
    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &context.post);

    /* Stop coarrays in the main program */

    if (ns->proc_name->attr.flavor == FL_PROGRAM) {
	tmp = g95_call_library(void_type_node, PREFIX "stop_coarray",
			       NULL_TREE);
	g95_add_expr_to_block(&block, tmp);
    }

    if (ns->format_label || ns->goto_label)
	finish_labels();

    /* Generate code for contained procedures. */

    for(child=ns->contained; child; child=child->sibling) {
	if (child->parent != ns)
	    continue;

	g95_generate_procedure(child);
    }

    /* Restore the symbol declaration to the function version if we had
     * to replace it with the result variable. */

    if (saved_function_decl != NULL_TREE)
	sym->backend_decl = saved_function_decl;

    restore_entries(ns);

    if (g95_context->result != NULL_TREE) {
	if (!sym->attr.function || sym->result->as == NULL)
	    tmp = g95_context->result;

	else if (sym->result->attr.pointer || sym->result->attr.allocatable)
	    tmp = pointer_array_return(&block);

	else
	    tmp = G95_ARRAY_DESC(g95_context->result);

	tmp = build2(MODIFY_EXPR, TREE_TYPE(tmp), DECL_RESULT(fndecl), tmp);
	tmp = build1(RETURN_EXPR, void_type_node, tmp);
	g95_add_expr_to_block(&block, tmp);
    }

    DECL_SAVED_TREE(fndecl) = g95_finish_block(&block);

    swap_charlen(ns);
    sym->attr.entry_result = n;

    /* Finish off this function and send it for code generation. */

    poplevel(1, 0, 1);
    BLOCK_SUPERCONTEXT(DECL_INITIAL(fndecl)) = fndecl;

    cfun->function_end_locus = input_location;
    cfun = NULL;

    dump_function(TDI_generic, fndecl);

    /* RTL generation */

    if (context.parent != NULL)
	pop_function_context();

    else if (decl_function_context(fndecl))
	/* Register this function with cgraph just far enough to get it
	 * added to our parent's nested function list.  */
	cgraph_node(fndecl);

    else {
	n = DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(fndecl);
	DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(fndecl) = 1;
	gimplify_function(fndecl);

	DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(fndecl) = n;
	gimplify_function(fndecl);

//    dump_function(TDI_generic, fndecl);

	lower_nested_functions(fndecl);
	finalize(fndecl);
    }

    n = 1;
    g95_dump_coarray(ns->sym_root, &n);

    current_function_decl = context.saved_current_function;
    g95_context = context.parent;
}


