/* Code translation
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

/* trans.c-- generate GCC trees from g95_code */

#include "trans.h"
#include <setjmp.h>


jmp_buf g95_backend_jump;

extern int g95_in_backend;

tree g95_locus_file, g95_locus_line;


/* Naming convention for backend interface code:
 * g95_trans_* translate g95_code into STMT trees.
 * g95_conv_* expression conversion
 * g95_get_* get a backend tree representation of a decl or type  */



/* g95_chainon_list()-- Wrap a node in a list node and add it to the
 * end of a list.  */

tree g95_chainon_list(tree list, tree add) {
tree l;

    l = tree_cons(NULL_TREE, add, NULL_TREE);
    return chainon(list, l);
}



/* g95_prepend_list()-- Add something to the start of a list */

tree g95_prepend_list(tree list, tree add) {
tree l;

    l = tree_cons(NULL_TREE, add, NULL_TREE);
    TREE_CHAIN(l) = list;
    return l;
}



/* g95_add_field()-- Add a field to the ioparm structure. */

tree g95_add_field(tree stype, char *name, tree type) {
tree decl;

    decl = build_decl(FIELD_DECL, get_identifier(name), type);

    DECL_CONTEXT(decl) = stype;
    DECL_INITIAL(decl) = 0;
    DECL_ALIGN(decl) = 0;
    DECL_USER_ALIGN(decl) = 0;
    TYPE_FIELDS(stype) = chainon(TYPE_FIELDS(stype), decl);

    return decl;
}



/* g95_find_field()-- Find a field of a derived type. */

tree g95_find_field(tree stype, char *name) {
tree t, n;

    n = maybe_get_identifier(name);

    for(t=TYPE_FIELDS(stype); t; t=TREE_CHAIN(t))
	if (DECL_NAME(t) == n)
	    break;

    return t;
}



/* g95_create_var_np()-- Create a decl for an artificial decl with the
 * given type. */

tree g95_create_var_np(tree type) {
tree var;

    type = build_type_variant(type, 0, 0);
    var = build_decl(VAR_DECL, g95_unique_identifier("U"), type);

    /* The variable was declared by the compiler.  */
    DECL_ARTIFICIAL(var) = 1;

    /* Make the variable writable.  */
    TREE_READONLY(var) = 0;

    DECL_EXTERNAL(var) = 0;
    TREE_STATIC(var)   = 0;
    TREE_USED(var)     = 1;

    TREE_ADDRESSABLE(var) = 1;

    return var;
}



/* g95_create_var()-- Like above, but also adds it to the current scope.  */

tree g95_create_var(tree type) {
tree tmp;

    tmp = g95_create_var_np(type);
    pushdecl(tmp);

    return tmp;
}



/* g95_unique_identifier()-- Get a unique identifier within a single
 * object file. */

tree g95_unique_identifier(char *base) {
static int serial;
char name[100];

    sprintf(name, "%s%d", base, serial++);
    return get_identifier(name);
}



/* g95_add_modify_expr()-- Add a MODIFY_EXPR to a block.  */

void g95_add_modify_expr(stmtblock_t *pblock, tree lhs, tree rhs) {
tree tmp;

    tmp = build2(MODIFY_EXPR, TREE_TYPE(lhs), lhs, rhs);
    g95_add_expr_to_block(pblock, tmp);
}



/* g95_addr_expr()-- Return the address of an expression. */

tree g95_addr_expr(tree t) {
tree type;

    type = build_pointer_type(TREE_TYPE(t));
    return build1(ADDR_EXPR, type, t);
}



/* g95_case_label()-- Get a case label. */

tree g95_case_label(tree low, tree high) {
tree t;

    t = build_decl(LABEL_DECL, NULL_TREE, NULL_TREE);
    DECL_CONTEXT(t) = current_function_decl;

    return build3(CASE_LABEL_EXPR, void_type_node, low, high, t);
}



/* g95_start_block()-- Create a new scope/binding level and initialize
 * a block. */

void g95_start_block(stmtblock_t *block) {

    pushlevel(0);

    block->head = NULL_TREE;
    block->has_scope = 1;
}



/* g95_init_block()-- Initialize a block without creating a new scope.
 * This function must not allocate anything that requires freeing as
 * it may be discarded without being used.  */

void g95_init_block(stmtblock_t *block) {

    block->head = NULL_TREE;
    block->has_scope = 0;
}



/* g95_finish_block()-- Finish a scope containing a block of statements.  */

tree g95_finish_block(stmtblock_t *stmtblock) {
tree decl, expr, block;

    expr = stmtblock->head;
    stmtblock->head = NULL_TREE;

    if (stmtblock->has_scope) {
	decl = getdecls();

	if (decl == NULL_TREE)
	    poplevel(0, 0, 0);

	else {
	    block = poplevel(1, 0, 0);
	    expr = build3(BIND_EXPR, void_type_node, decl, expr, block);
	}
    }

    return expr;
}



/* g95_build_function_call()-- Build a CALL_EXPR.  */

tree g95_build_function_call(tree decl, tree arglist) {
tree tmp, call;

    tmp = g95_addr_expr(decl);
    call = build3(CALL_EXPR, TREE_TYPE(TREE_TYPE(decl)), tmp, arglist, NULL);
    TREE_SIDE_EFFECTS(call) = 1;

    return call;
}



/* g95_set_locus()-- Store the current file and line number to
 * variables so that if a library call goes awry, we can tell the user
 * where the problem is. */

void g95_set_locus(stmtblock_t *block, g95_locus *where) {
g95_linebuf *lb;
char *filename;
tree tmp;
int line;

    lb = where->lb;
    filename = lb->name;
    line = lb->linenum;

    tmp = g95_build_string_const(strlen(filename)+1, filename);
    tmp = g95_addr_expr(tmp);
    g95_add_modify_expr(block, g95_locus_file, tmp);

    g95_add_modify_expr(block, g95_locus_line, g95_build_int(line, 0));
}



/* g95_call_temp_alloc()-- Calls the temp_alloc() library function to
 * get some memory from the heap. */

void g95_call_temp_alloc(stmtblock_t *block, tree var, tree length) {
tree tmp;

    tmp = g95_call_library(pvoid_type_node, PREFIX "temp_alloc",
			   length, NULL_TREE);

    g95_add_modify_expr(block, var, tmp);
}



/* g95_call_temp_free()-- Generate a call to the temp_free() library
 * function. */

void g95_call_temp_free(stmtblock_t *block, tree var) {
tree tmp;

    var = g95_addr_expr(var); 

    tmp = g95_call_library(void_type_node, PREFIX "temp_free",
			   var, NULL_TREE);

    g95_add_expr_to_block(block, tmp);
}



/* g95_add_expr_to_block()-- Add a statement to a bock.  */

void g95_add_expr_to_block(stmtblock_t *block, tree expr) {

    assert(block);

    if (expr == empty_stmt_node || expr == NULL)
	return;

    if (block->head)
	block->head = build2(COMPOUND_EXPR, void_type_node, block->head, expr);

    else
	block->head = expr;
}



/* g95_add_block_to_block()-- Add a block the end of a block.  */

void g95_add_block_to_block(stmtblock_t *block, stmtblock_t *append) {

    assert(append);
    assert(!append->has_scope);

    g95_add_expr_to_block(block, append->head);
    append->head = NULL_TREE;
}



/* g95_add_se_expr()-- Add pre and post expressions to the top parent
 * of an se. */

void g95_add_se_expr(g95_se *se, tree pre, tree post) {

    while(se->parent != NULL)
	se = se->parent;

    if (pre != NULL_TREE)
	g95_add_expr_to_block(&se->pre, pre);

    if (post != NULL_TREE)
	g95_add_expr_to_block(&se->post, post);
}



/* g95_raise_chains()-- Move any pre or post chains into the parent */

void g95_raise_chains(g95_se *se) {
g95_se *top;

    top = se;
    while(top->parent != NULL)
	top = top->parent;

    while(se != top) {
	if (se->pre.head != NULL) {
	    g95_add_block_to_block(&top->pre, &se->pre);
	    se->pre.head = NULL;
	}

	if (se->post.head != NULL) {
	    g95_add_block_to_block(&top->post, &se->post);
	    se->post.head = NULL;
	}

	se = se->parent;
    }
}



bool g95_post_options(const char **filename_p) {

    g95_source_file = (char *) *filename_p;

    if (g95_source_file == NULL || strcmp(g95_source_file, "-") == 0)
	g95_source_file = "";

    flag_inline_trees = 1;
    if (!flag_no_inline)
	flag_no_inline = 1;

    if (flag_inline_functions) {
	flag_inline_trees = 2;
	flag_inline_functions = 0;
    }

    flag_inline_trees = 0;
    flag_inline_functions = 0;

    return false;
}



/* g95_array_node()-- Return an empty array node */

tree g95_array_node(void) {
tree decl;

    decl = make_tree_vec_stat(G95_ARRAY_WORDS);
    G95_ARRAY(decl) = 1;

    return decl;
}



/* g95_dtype_node()-- Return a derived type node */

tree g95_dtype_node(void) {
tree decl;

    decl = make_tree_vec_stat(G95_DTYPE_WORDS);
    G95_DTYPE(decl) = 1;

    return decl;
}



/* g95_set_backend_locus()-- Set the current locus. */

void g95_set_backend_locus(g95_locus *loc) {

    input_line = loc->lb->linenum;
    input_filename = loc->lb->name;
}



/* g95_save_expr()-- Build a temporary to save an expression for later
 * use.  This subroutine is used when gcc's save_expr() won't suffice.
 * For example, if the saved value is used on opposite sides of a
 * conditional. */

void g95_save_expr(g95_se *se) {
tree var;

    if (!G95_CONSTANT_P(se->expr) && TREE_CODE(se->expr) != VAR_DECL) {
	var = g95_create_var(TREE_TYPE(se->expr));
	g95_add_modify_expr(&se->pre, var, se->expr);
	se->expr = var;
    }
}



/* g95_call_library()-- Generate a library call to a subroutine.  This
 * subroutine has a variable number of arguments, as many as the
 * library call and is terminated by a NULL_TREE element. */

tree g95_call_library(tree rtype, char *name, ...) {
tree arg, arglist, typelist, decl;
va_list ap;

    va_start(ap, name);

    arglist  = NULL_TREE;
    typelist = NULL_TREE;

    for(;;) {
	arg = va_arg(ap, tree);
	if (arg == NULL_TREE)
	    break;

	arglist  = g95_chainon_list(arglist, arg);
	typelist = g95_chainon_list(typelist, TREE_TYPE(arg));
    }

    va_end(ap);

    typelist = g95_chainon_list(typelist, void_type_node);

    decl = build_function_type(rtype, typelist);
    decl = build_decl(FUNCTION_DECL, get_identifier(name), decl);

    DECL_EXTERNAL(decl) = 1;
    TREE_PUBLIC(decl)   = 1;

    pushdecl(decl);
    rest_of_decl_compilation(decl, 1, 0);

    return g95_build_function_call(decl, arglist);
}



/* g95_build_label()-- Build a label expression */

tree g95_build_label(tree identifier) {

    return build1(LABEL_EXPR, void_type_node, identifier);
}



/* g95_build_goto()-- Build a GOTO expression */

tree g95_build_goto(tree label) {

    return build1(GOTO_EXPR, void_type_node, label);
}


/* g95_build_loop()-- Build a LOOP expression */

tree g95_build_loop(tree body) {

    return build1(LOOP_EXPR, void_type_node, body);
}


/* g95_build_switch()-- Build a SWITCH expression */

tree g95_build_switch(tree value, tree body) {

    return build3(SWITCH_EXPR, void_type_node, value, body, NULL_TREE);
}



/* g95_build_cond()-- Build a COND statement */

tree g95_build_cond(tree type, tree pred, tree b1, tree b2) {

    return fold(build3(COND_EXPR, type, pred, b1, b2));
}


/* g95_build_eq()-- Build an EQ expression */

tree g95_build_eq(tree arg1, tree arg2) {

    return fold(build2(EQ_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_ne()-- Build an NE expression */

tree g95_build_ne(tree arg1, tree arg2) {

    return fold(build2(NE_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_gt()-- Build a GT expression */

tree g95_build_gt(tree arg1, tree arg2) {

    return fold(build2(GT_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_ge()-- Build a GE expression */

tree g95_build_ge(tree arg1, tree arg2) {

    return fold(build2(GE_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_lt()-- Build a LT expression */

tree g95_build_lt(tree arg1, tree arg2) {

    return fold(build2(LT_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_le()-- Build a LE expression */

tree g95_build_le(tree arg1, tree arg2) {

    return fold(build2(LE_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_andif()-- Build a TRUTH_ANDIF_EXPR */

tree g95_build_andif(tree arg1, tree arg2) {

    return fold(build2(TRUTH_ANDIF_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_orif()-- Build a TRUTH_ORIF_EXPR */

tree g95_build_orif(tree arg1, tree arg2) {

    return fold(build2(TRUTH_ORIF_EXPR, boolean_type_node, arg1, arg2));
}



/* g95_build_constructor()-- Build a CONSTRUCTOR expression. */

tree g95_build_constructor(tree type, tree arg) {

#if TARGET_GCC_VERSION >= 410
    return build_constructor_from_list(type, arg);
#else
    return build1(CONSTRUCTOR, type, arg);
#endif
}



/* g95_build_component()-- Build a COMPONENT_REF expression */

tree g95_build_component(tree type, tree var, tree field) {

    return build3(COMPONENT_REF, type, var, field, NULL_TREE);
}



/* g95_build_array_ref()-- Build a ARRAY_REF expression */

tree g95_build_array_ref(tree type, tree array, tree index) {

    return build4(ARRAY_REF, type, array, index, NULL_TREE, NULL_TREE);
}



/* g95_build_plus()-- Build a PLUS_EXPR expression */

tree g95_build_plus(tree type, tree arg1, tree arg2) {

    return fold(build2(PLUS_EXPR, type, arg1, arg2));
}



/* g95_build_minus()-- Build a MINUS_EXPR expression */

tree g95_build_minus(tree type, tree arg1, tree arg2) {

    return fold(build2(MINUS_EXPR, type, arg1, arg2));
}



/* g95_build_mult()-- Build a MULT_EXPR expression */

tree g95_build_mult(tree type, tree arg1, tree arg2) {

    return fold(build2(MULT_EXPR, type, arg1, arg2));
}



/* g95_build_max()-- Build a MAX_EXPR expression */

tree g95_build_max(tree type, tree arg1, tree arg2) {

    return fold(build2(MAX_EXPR, type, arg1, arg2));
}



/* g95_build_and()-- Build a BIT_AND_EXPR expression */

tree g95_build_and(tree type, tree arg1, tree arg2) {

    return fold(build2(BIT_AND_EXPR, type, arg1, arg2));
}



/* g95_build_ior()-- Build a BIT_IOR_EXPR expression */

tree g95_build_ior(tree type, tree arg1, tree arg2) {

    return fold(build2(BIT_IOR_EXPR, type, arg1, arg2));
}



/* g95_build_lshift()-- Build a LSHIFT_EXPR expression */

tree g95_build_lshift(tree type, tree value, tree shift) {

    return fold(build2(LSHIFT_EXPR, type, value, shift));
}



/* g95_build_rshift()-- Build a RSHIFT_EXPR expression */

tree g95_build_rshift(tree type, tree value, tree shift) {

    return fold(build2(RSHIFT_EXPR, type, value, shift));
}



/* g95_build_abs()-- Build an ABS_EXPR expression */

tree g95_build_abs(tree type, tree value) {

    return fold(build1(ABS_EXPR, type, value));
}



/* g95_build_indirect()-- Build an INDIRECT_REF expression. */

tree g95_build_indirect(tree type, tree arg) {

    if (TREE_CODE(arg) == ADDR_EXPR)
	return TREE_OPERAND(arg, 0);

    return build1(INDIRECT_REF, type, arg);
}



/* generate_module()-- This function is called after a module
 * has been parsed and resolved. */

static void generate_module(g95_namespace *ns) {
g95_namespace *child;
int n;

    for(child=ns->contained; child; child=child->sibling) {
	if (child->parent != ns)
	    continue;    /* Skip namespaces from used modules */

	g95_build_procedure_decl(child->proc_name);
    }

    g95_trans_common(ns);
    g95_generate_procedure_variables(ns);

    n = 1;
    g95_dump_coarray(ns->sym_root, &n);

    for(child=ns->contained; child; child=child->sibling)
	if (child->parent == ns)
	    g95_generate_procedure(child);
}



/* generate_block_data()-- Generate a stub procedure associated with a
 * BLOCK DATA.  This allows the restriction on one block data name per
 * program to be enforced as well giving the linker something to grab
 * when pulling in block data initializations from libraries. */

static void generate_block_data(g95_namespace *ns) {
tree identifier, decl, result;

    identifier = g95_sym_identifier(ns->proc_name, 1, NULL);

    decl = build_function_type(void_type_node, NULL_TREE);
    decl = build_decl(FUNCTION_DECL, identifier, decl);
    TREE_PUBLIC(decl) = 1;

    result = build_decl(RESULT_DECL, NULL_TREE, void_type_node);
    DECL_CONTEXT(result) = decl;
    DECL_RESULT(decl) = result;

    ns->proc_name->backend_decl = decl;
}



/* g95_generate_code()-- Generate code for the parsed and resolved
 * namespaces */

void g95_generate_code(g95_namespace *ns) {
g95_symbol *sym;

    g95_in_backend = 1;
    if (setjmp(g95_backend_jump))
	return;

    input_location.file = main_input_filename;

    g95_init_common(ns);

    for(; ns; ns=ns->sibling) {
	g95_current_ns = ns;
	sym = ns->proc_name;

	switch(ns->state) {
	case COMP_MODULE:
	    generate_module(ns);
	    break;

	case COMP_PROGRAM:
	case COMP_SUBROUTINE:
	case COMP_FUNCTION:
	    g95_build_procedure_decl(ns->proc_name);
	    g95_generate_procedure(ns);
	    break;

	case COMP_BLOCK_DATA:
	    generate_block_data(ns);
	    g95_generate_procedure(ns);
	    break;

	default:
	    g95_internal_error("g95_generate_code(): Bad program unit");
	}
    }
}
