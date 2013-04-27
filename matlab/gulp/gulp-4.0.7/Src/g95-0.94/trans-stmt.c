/* Statement translation
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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

/* trans-stmt.c-- generate GCC trees from g95_code */

#include "trans.h"

extern tree g95_ac_info_type;

/* trans_goto()-- Translate a GOTO statement */

static tree trans_goto(g95_code *code) {
stmtblock_t block;
g95_se se;
tree decl;

    g95_init_block(&block);

    if (code->expr == NULL)
	decl = g95_build_goto(g95_get_label_decl(code->label));

    else if (!g95_current_ns->goto_label)
	decl = empty_stmt_node;

    else {
	g95_init_se(&se, NULL);
	g95_conv_expr(&se, code->expr);

	decl = g95_call_library(pvoid_type_node, PREFIX "select_label",
				se.expr,
				g95_addr_expr(g95_context->labels), NULL_TREE);
	decl = g95_build_goto(decl);
    }

    g95_add_expr_to_block(&block, decl);
    return g95_finish_block(&block);
}



/* alt_return_jump()-- Generate a switch to jump to the right
 * alternate return point. */

static tree alt_return_jump(tree var, g95_actual_arglist *actual) {
stmtblock_t body;
tree tmp;
int n;

    g95_init_block(&body);
    n = 1;

    for(; actual!=NULL; actual=actual->next) {
	if (actual->type != ARG_ALT_RETURN)
	    continue;

	tmp = g95_build_int(n++, 0);
	tmp = g95_case_label(tmp, tmp);
	g95_add_expr_to_block(&body, tmp);

	tmp = g95_get_label_decl(actual->u.label);
	tmp = g95_build_goto(tmp);
	g95_add_expr_to_block(&body, tmp);
    }

    tmp = g95_finish_block(&body);
    return g95_build_switch(var, tmp);
}



/* trans_call()-- Translate a CALL statement */

static tree trans_call(g95_code *code) {
tree tmp, var, args, name, decl;
g95_se se;
int alt;

    if (code->ext.sub.pointer == NULL && code->sym != NULL &&
	code->sym->attr.iproc != IPROC_NONE)
	return g95_conv_modproc_subroutine(code);

    g95_init_se(&se, NULL);
    g95_init_block(&se.pre);

    if (g95_option.trace == TRACE_FRAME || g95_option.trace == TRACE_FULL)
	g95_set_locus(&se.pre, &code->where);

    if (code->ext.sub.pointer != NULL) {
	se.reflevel = 0;
	g95_conv_expr(&se, code->ext.sub.pointer);

	decl = se.expr;
	alt = 0;

    } else if (code->ext.sub.isym == NULL) {
	decl = code->sym->backend_decl;

	if (decl == NULL) {
	    g95_function_decl(code->sym);
	    decl = code->sym->backend_decl;
	}

	if (POINTER_TYPE_P(TREE_TYPE(decl)))
	    decl = g95_build_indirect(TREE_TYPE(TREE_TYPE(decl)), decl);

	alt = code->sym->attr.artificial &&
	    g95_context->current_procedure->proc_name->attr.subroutine;

    } else {
	decl = g95_conv_intrinsic_subroutine(code);
	if (decl != NULL_TREE)
	    return decl;

	/* Call sub_name */

	alt = 0;
	name = get_identifier(code->ext.sub.sub_name);

	decl = build_function_type(void_type_node, NULL_TREE);
	decl = build_decl(FUNCTION_DECL, name, decl);

	DECL_EXTERNAL(decl) = 1;
	TREE_PUBLIC(decl) = 1;

	pushdecl(decl);
	rest_of_decl_compilation(decl, 1, 0);
    }

    args = g95_trans_arglist(code->ext.sub.actual, &se);
    tmp  = g95_build_function_call(decl, args);

    if (alt) {
	g95_add_modify_expr(&se.pre, g95_context->result, tmp);

    } else if (!g95_has_alt_return(code->ext.sub.actual)) {
	g95_add_expr_to_block(&se.pre, tmp);
	g95_add_block_to_block(&se.pre, &se.post);

    } else {
	var = g95_create_var(g95_default_integer);
	g95_add_modify_expr(&se.pre, var, tmp);

	g95_add_block_to_block(&se.pre, &se.post);

	tmp = alt_return_jump(var, code->ext.sub.actual);
	g95_add_expr_to_block(&se.pre, tmp);
    }

    return g95_finish_block(&se.pre);
}



/* trans_return()-- Translate a RETURN statement */

static tree trans_return(g95_code *code) {
stmtblock_t body;
g95_se se;
tree tmp;

    g95_init_block(&body);

    if (code->expr != NULL) {
	g95_init_se(&se, NULL);
	g95_conv_expr(&se, code->expr);

	g95_add_block_to_block(&body, &se.pre);
	g95_add_modify_expr(&body, g95_context->result, se.expr);
	g95_add_block_to_block(&body, &se.post);
    }

    tmp = g95_build_goto(g95_get_return_label());
    g95_add_expr_to_block(&body, tmp);

    return g95_finish_block(&body);
}



/* trans_stop()-- Translate a STOP and ALL STOP statements. */

static tree trans_stop(g95_code *code) {
char *sub;
g95_se se;
tree tmp;

    g95_init_se(&se, NULL);
    g95_init_block(&se.pre);
    
    if (code->type == EXEC_STOP)
	sub = PREFIX "stop";

    else if (code->type == EXEC_ERROR_STOP)
	sub = PREFIX "error_stop";

    else
	g95_internal_error("trans_stop()-- Bad code");

    if (code->ext.stop_code != -1) {
	tmp = g95_build_int(code->ext.stop_code, 0);
	tmp = g95_call_library(void_type_node, sub, tmp, null_pointer_node,
			       NULL_TREE);

    } else if (code->expr != NULL) {
	se.reflevel = 1;
	g95_conv_expr(&se, code->expr);

	tmp = g95_call_library(void_type_node, sub, integer_minus_one_node,
			       se.expr, se.string_length, NULL_TREE);

    } else
	tmp = g95_call_library(void_type_node, sub, integer_minus_one_node,
			       null_pointer_node, NULL_TREE);

    g95_set_locus(&se.pre, &code->where);
    g95_add_expr_to_block(&se.pre, tmp);

    return g95_finish_block(&se.pre);
}



/* trans_pause()-- Translate a PAUSE statement */

static tree trans_pause(g95_code *code) {
stmtblock_t body;
g95_se se;
tree tmp;

    g95_init_block(&body);

    if (code->ext.stop_code != -1) {
	tmp = g95_build_int(code->ext.stop_code, 0);
	tmp = g95_call_library(void_type_node, PREFIX "pause_integer",
			       tmp, NULL);
	g95_add_expr_to_block(&body, tmp);

    } else if (code->expr != NULL) {
	g95_init_se(&se, NULL);
	se.reflevel = 1;

	g95_conv_expr(&se, code->expr);
	g95_add_block_to_block(&body, &se.pre);

	tmp = g95_call_library(void_type_node, PREFIX "pause_string", se.expr,
			       se.string_length, NULL);

	g95_add_expr_to_block(&body, tmp);
	g95_add_block_to_block(&body, &se.post);

    } else {
	tmp = g95_call_library(void_type_node, PREFIX "pause", NULL_TREE);
	g95_add_expr_to_block(&body, tmp);
    }

    return g95_finish_block(&body);
}



/* trans_if()-- Translate an IF statement.  */

static tree trans_if(g95_code *code) {
tree then_stmt, else_stmt, predicate, tmp;
stmtblock_t block;
g95_se se;

    g95_init_se(&se, NULL);
    g95_init_block(&block);

    g95_conv_expr(&se, code->expr);
    g95_add_block_to_block(&block, &se.pre);

    then_stmt = g95_trans_code(code->block);
    if (then_stmt == NULL_TREE)
	then_stmt = empty_stmt_node;

    else_stmt = g95_trans_code(code->ext.block);
    if (else_stmt == NULL_TREE)
	else_stmt = empty_stmt_node;

    /* If there is a post chain, we have to stuff the result into a
     * temporary variable, clean up, then take the branch based on the
     * variable. */

    if (se.post.head == NULL_TREE)
	predicate = fold(se.expr);

    else {
	predicate = g95_create_var(boolean_type_node);
	g95_add_modify_expr(&block, predicate, se.expr);

	g95_add_block_to_block(&block, &se.post);
    }

    tmp = g95_build_cond(void_type_node, predicate, then_stmt, else_stmt);
    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}



/* trans_arithmetic_if()-- Translate an arithmetic IF statement.  The
 * expression might contain a function call somehow, so we create and
 * test a variable.*/

static tree trans_arithmetic_if(g95_code *code) {
tree tmp, branch1, branch2, zero, var;
g95_se se;

    g95_init_se(&se, NULL);
    g95_init_block(&se.pre);

    g95_conv_expr(&se, code->expr);

    var = g95_create_var(TREE_TYPE(se.expr));
    g95_add_modify_expr(&se.pre, var, se.expr);

    zero = g95_build_const(TREE_TYPE(se.expr), integer_zero_node);

    branch1 = g95_build_goto(g95_get_label_decl(code->label));
    branch2 = g95_build_goto(g95_get_label_decl(code->label2));

    tmp = g95_build_lt(var, zero);
    branch1 = g95_build_cond(void_type_node, tmp, branch1, branch2);
    branch2 = g95_build_goto(g95_get_label_decl(code->label3));
    tmp = g95_build_le(var, zero);
    branch1 = g95_build_cond(void_type_node, tmp, branch1, branch2);

    g95_add_expr_to_block(&se.pre, branch1);

    return g95_finish_block(&se.pre);
}


/* trans_do()-- Translate a DO statement.  Real DO loop parameter
 * handling added by Tom Crane (T.Crane@rhul.ac.uk), Sept. 2007. */

static tree trans_do(g95_code *code) {
tree dovar, from, to, t, t1, t2, step, count, type, cond, trips;
tree cycle_label, exit_label, tmp, zero;
stmtblock_t block, body;
g95_se se;

    g95_init_block(&block);

    g95_init_se(&se, NULL);
    g95_conv_expr(&se, code->ext.iterator->var);
    g95_add_block_to_block(&block, &se.pre);

    dovar = se.expr;
    type = TREE_TYPE(dovar);

    if (type != g95_default_integer && type != g95_int8_type_node &&
	TREE_CODE(type) != REAL_TYPE)
	type = g95_default_integer;

    zero = g95_build_const(type, integer_zero_node); 
  
    g95_init_se(&se, NULL);
    g95_conv_expr_type(&se, code->ext.iterator->start, type);

    g95_add_block_to_block(&block, &se.pre);

    if (TREE_CONSTANT(se.expr))
	from = se.expr;

    else {
	from = g95_create_var(type);
	g95_add_modify_expr(&block, from, se.expr);
    }

    g95_add_block_to_block(&block, &se.post);

    g95_init_se(&se, NULL);
    g95_conv_expr_type(&se, code->ext.iterator->end, type);

    g95_add_block_to_block(&block, &se.pre);

    if (TREE_CONSTANT(se.expr))
	to = se.expr;

    else {
	to = g95_create_var(type);
	g95_add_modify_expr(&block, to, se.expr);
    }

    g95_add_block_to_block(&block, &se.post);

    g95_init_se(&se, NULL);
    g95_conv_expr_type(&se, code->ext.iterator->step, type);

    g95_add_block_to_block(&block, &se.pre);

    if (TREE_CONSTANT(se.expr))
	step = convert(type, se.expr);

    else {
	step = g95_create_var(type);
	g95_add_modify_expr(&block, step, se.expr);
    }

    g95_add_block_to_block(&block, &se.post);

    /* Initialize the trip count.  This code is executed before we enter
     * the loop body.  We generate: count = (to + step - from) / step.  */

    trips = g95_build_minus(type, step, from);
    trips = g95_build_plus(type, to, trips);

    if (TREE_CODE(type) == INTEGER_TYPE)
	trips = fold(build2(TRUNC_DIV_EXPR, type, trips, step));

    else {
	trips = fold(build2(RDIV_EXPR, type, trips, step));
	trips = fold(build1(FIX_TRUNC_EXPR, type, trips));
    }

    t1  = g95_build_gt(step, zero);
    t2  = g95_build_gt(from, to);
    tmp = g95_build_andif(t1, t2);

    t1 = g95_build_lt(step, zero);
    t2 = g95_build_lt(from, to);
    t  = g95_build_andif(t1, t2);

    tmp = g95_build_orif(t, tmp);
    trips = g95_build_cond(type, tmp,
			   convert(type, integer_zero_node), trips);

    if (g95_option.onetrip)
	trips = g95_build_max(type, trips,
			      convert(type, integer_one_node));

    count = g95_create_var(type);
    g95_add_modify_expr(&block, count, trips);

    /* Initialize the DO variable: dovar = from.  */
    g95_add_modify_expr(&block, dovar, from);

    /* Loop body */
    g95_init_block(&body);

    /* Cycle and exit statements are implemented with gotos.  Put
     * these labels where they can be found later. We put the labels
     * in a TREE_LIST node (because TREE_CHAIN is already
     * used). cycle_label goes in TREE_PURPOSE (backend_decl), exit
     * label in TREE_VALUE (backend_decl).  */

    cycle_label = g95_build_label_decl(NULL_TREE);
    exit_label  = g95_build_label_decl(NULL_TREE);
    code->backend_decl = tree_cons(cycle_label, exit_label, NULL);

    /* Start with the loop condition.  Loop until trip count == 0.  */

    cond = g95_build_eq(count, integer_zero_node);
    tmp = g95_build_goto(exit_label);
    TREE_USED(exit_label) = 1;

    tmp = g95_build_cond(void_type_node, cond, tmp, empty_stmt_node);
    g95_add_expr_to_block(&body, tmp);

    /* Main loop body. */

    tmp = g95_trans_code(code->block);

    g95_add_expr_to_block(&body, tmp);

    /* Label for cycle statements (if needed). */
    if (TREE_USED(cycle_label)) {
	tmp = g95_build_label(cycle_label);
	g95_add_expr_to_block(&body, tmp);
    }

    /* Increment the loop variable. */
    tmp = g95_build_plus(type, dovar, step);
    g95_add_modify_expr(&body, dovar, tmp);

    /* Decrement the trip count. */
    tmp = g95_build_minus(type, count, convert(type, integer_one_node));
    g95_add_modify_expr(&body, count, tmp);

    /* End of loop body. */
    tmp = g95_finish_block(&body);

    /* The for loop itself. */
    tmp = g95_build_loop(tmp);
    g95_add_expr_to_block(&block, tmp);

    /* Add the exit label */
    tmp = g95_build_label(exit_label);
    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}



/* trans_do_while()-- Translate a DO WHILE statement.  By the time we
 * get here, this statement is always an infinite loop with an exit
 * statement up front. */

static tree trans_do_while(g95_code *code) {
tree tmp, cycle_label, exit_label;
stmtblock_t block;
location_t end;
g95_code *c;

    g95_init_block(&block);

    cycle_label = g95_build_label_decl(NULL_TREE);
    exit_label  = g95_build_label_decl(NULL_TREE);

    code->backend_decl = tree_cons(cycle_label, exit_label, NULL);

    tmp = g95_trans_code(code->block);
    g95_add_expr_to_block(&block, tmp);

    if (TREE_USED(cycle_label)) {
	tmp = g95_build_label(cycle_label);
	g95_add_expr_to_block(&block, tmp);
    }

    tmp = g95_finish_block(&block);
    g95_init_block(&block);

    tmp = g95_build_loop(tmp);
    g95_add_expr_to_block(&block, tmp);

    tmp = g95_build_label(exit_label);
    end = input_location;

    if (code->next != NULL)
	end.line = code->next->where.lb->linenum;

    else if (code->block == NULL) {
	end.line = code->where.lb->linenum+3;   /* Punt */

    } else {
	c = code->block;
	while(c->next != NULL)
	    c = c->next;

	end.line = c->where.lb->linenum+1;
    }

    annotate_with_locus(tmp, end);

    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}



/* string_address()-- Return the address of a string */

static tree string_address(tree s) {

    return (s == null_string_node)
	? null_string_node
	: g95_addr_expr(s);
}



/* logical_select()-- Build a logical selection statement. */

static tree logical_select(g95_code *code, g95_expr *selector) {
g95_code *d, *true_case, *false_case, *default_case;
stmtblock_t block, body;
tree tmp, end_label;
g95_case *c;
g95_se se;

    true_case = false_case = default_case = NULL;

    /* Extract out the cases.  Illegal cases have already been flagged. */

    for(d=code->block; d; d=d->block)
	for(c=d->ext.case_list; c; c=c->next)
	    if (c->low == NULL && c->high == NULL)
		default_case = d->next;

	    else if (c->low->value.logical)
		true_case = d->next;

	    else
		false_case = d->next;

    /* Special case that won't work with the below code.  This is a
     * select statement with true and false branches that are the same. */

    if (true_case != NULL && true_case == false_case)
	return g95_trans_code(true_case);

    g95_init_block(&block);
    g95_init_block(&body);

    g95_init_se(&se, NULL);
    g95_conv_expr(&se, selector);

    end_label = g95_build_label_decl(NULL_TREE);

    tmp = g95_case_label(integer_zero_node, NULL_TREE);
    g95_add_expr_to_block(&body, tmp);

    if (false_case != NULL) {
	tmp = g95_trans_code(false_case);
	g95_add_expr_to_block(&body, tmp);
    }

    tmp = g95_build_goto(end_label);
    g95_add_expr_to_block(&body, tmp);

    tmp = g95_case_label(integer_one_node, NULL_TREE);
    g95_add_expr_to_block(&body, tmp);

    if (true_case != NULL) {
	tmp = g95_trans_code(true_case);
	g95_add_expr_to_block(&body, tmp);
    }

    tmp = g95_build_goto(end_label);
    g95_add_expr_to_block(&body, tmp);

    /* Finish up */

    tmp = g95_finish_block(&body);
    tmp = g95_build_switch(convert(g95_default_integer, se.expr), tmp);

    g95_add_expr_to_block(&block, tmp);

    tmp = g95_build_label(end_label);
    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}



/* character_select()-- Implement a character SELECT statement.  We
 * generate an array of structures describing the cases in order and
 * call a library subroutine that locates the right case.  The library
 * subroutine returns a pointer to jump to or NULL if no branches are
 * to be taken. */

static tree character_select(g95_code *code, g95_expr *selector) {
tree init, node, end_label, len, m, tmp, var;
stmtblock_t block, body;
g95_se se, se0;
g95_case *d;
g95_code *c;
int i, n;

static tree select_struct, ss_string1, ss_string1_len,
            ss_string2, ss_string2_len, ss_target;

    if (select_struct == NULL) {
	select_struct = make_node(RECORD_TYPE);

	TYPE_NAME(select_struct) = g95_unique_identifier("jump_struct");
  
	ss_string1_len = g95_add_field(select_struct, "string1_len",
				       g95_default_integer);
	ss_string1     = g95_add_field(select_struct, "string1",
				       pchar_type_node);
	ss_string2_len = g95_add_field(select_struct, "string2_len",
				       g95_default_integer);
	ss_string2     = g95_add_field(select_struct, "string2",
				       pchar_type_node);
	ss_target      = g95_add_field(select_struct, "target",
				       g95_default_integer);

	g95_finish_type(select_struct);
    }

    end_label = g95_build_label_decl(NULL_TREE);

/* Generate the body */

    g95_init_block(&block);
    g95_init_block(&body);
    g95_init_se(&se0, NULL);

    n = 0;
    for(c=code->block; c; c=c->block, n++) {
	m = g95_build_int(n, 0);

	tmp = g95_case_label(m, m);
	g95_add_expr_to_block(&body, tmp);

	tmp = g95_trans_code(c->next);
	g95_add_expr_to_block(&body, tmp);

	tmp = g95_build_goto(end_label);
	g95_add_expr_to_block(&body, tmp);

	for(d=c->ext.case_list; d; d=d->next)
	    d->n = n;
    }

/* Generate the structure describing the branches */

    init = NULL_TREE;
    i = 0;

    d = code->block->ext.case_list;
    while(d->cprev != NULL)
	d = d->cprev;

    for(; d; d=d->cnext) {
	node = NULL_TREE;

	if (d->low == NULL) {
	    node = tree_cons(ss_string1_len, integer_zero_node, node);
	    node = tree_cons(ss_string1, null_pointer_node, node);

	} else {
	    g95_conv_expr(&se0, d->low);
	    tmp = string_address(se0.expr);

	    node = tree_cons(ss_string1_len, se0.string_length, node);
	    node = tree_cons(ss_string1, tmp, node);
	}

	if (d->high == NULL) {
	    node = tree_cons(ss_string2_len, integer_zero_node, node);
	    node = tree_cons(ss_string2, null_pointer_node, node);

	} else {
	    g95_conv_expr(&se0, d->high);
	    tmp = string_address(se0.expr);

	    node = tree_cons(ss_string2_len, se0.string_length, node);
	    node = tree_cons(ss_string2, tmp, node);
	}

	node = tree_cons(ss_target, g95_build_int(d->n, 0), node);

	tmp = g95_build_constructor(select_struct, nreverse(node));
	init = tree_cons(g95_build_int(i++, 0), tmp, init);
    }

    tmp = build_array_type(select_struct,
			   build_index_type(g95_build_int(n-1, 0)));

    init = g95_build_constructor(tmp, nreverse(init));

    var = g95_create_var(TREE_TYPE(init));
  
    TREE_CONSTANT(var) = 1;
    TREE_STATIC(var) = 1;
    DECL_INITIAL(var) = init;

    init = g95_addr_expr(var);

    len = g95_build_int(i, 0);

    g95_init_se(&se, NULL);
    se.reflevel = 1;
    g95_conv_expr(&se, selector);

    g95_add_block_to_block(&block, &se.pre);

    var = g95_create_var(g95_default_integer);
    tmp = g95_call_library(g95_default_integer, PREFIX "select_string",
			   init, len, se.expr, se.string_length, NULL_TREE);

    g95_add_modify_expr(&block, var, tmp);
    g95_add_block_to_block(&block, &se.post);

    tmp = g95_finish_block(&body);
    tmp = g95_build_switch(var, tmp);

    g95_add_expr_to_block(&block, tmp);

    tmp = g95_build_label(end_label);
    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}



/* integer_select()-- Build an integer selection statement */

static tree integer_select(g95_code *code, g95_expr *selector) {
tree end_label, ulow_label, uhigh_label, tmp, low, high, goto_expr;
g95_case *cp, *unbounded_low, *unbounded_high;
stmtblock_t block, body;
g95_code *c;
g95_se se;
int kind;

    g95_init_block(&block);

    g95_init_se(&se, NULL);
    g95_conv_expr(&se, selector);

    kind = selector->ts.kind;

    end_label = g95_build_label_decl(NULL_TREE);

    g95_init_block(&body);

    /* Look for the unbounded low and unbounded high cases. */

    unbounded_low  = NULL;
    unbounded_high = NULL;

    ulow_label  = NULL_TREE;
    uhigh_label = NULL_TREE;

    cp = code->block->ext.case_list;

    while(cp->cnext != NULL)
	cp = cp->cnext;

    if (cp->high == NULL && cp->low != NULL) {
	unbounded_low = cp;
	ulow_label = g95_build_label_decl(NULL_TREE);
    }

    cp = code->block->ext.case_list;

    while(cp->cprev != NULL)
	cp = cp->cprev;

    /* The unbounded high is the first or second element */

    if (cp->low == NULL && cp->high != NULL)
	unbounded_high = cp;

    else {
	cp = cp->cnext;
	if (cp != NULL && cp->low == NULL && cp->high != NULL) unbounded_high = cp;
    }

    if (unbounded_high != NULL)
	uhigh_label = g95_build_label_decl(NULL_TREE);

    if (unbounded_high != NULL || unbounded_low != NULL)
	se.expr = save_expr(se.expr);

    g95_add_block_to_block(&block, &se.pre);

/* Build branch statements to the unbounded cases */

    if (unbounded_high != NULL) {
	high = bi_to_tree(unbounded_high->high->value.integer, kind);

	tmp = g95_build_le(se.expr, high);
	goto_expr = g95_build_goto(uhigh_label);

	tmp = g95_build_cond(void_type_node, tmp, goto_expr, empty_stmt_node);
	g95_add_expr_to_block(&block, tmp);
    }

    if (unbounded_low != NULL) {
	low = bi_to_tree(unbounded_low->low->value.integer, kind);

	tmp = g95_build_ge(se.expr, low);
	goto_expr = g95_build_goto(ulow_label);

	tmp = g95_build_cond(void_type_node, tmp, goto_expr, empty_stmt_node);
	g95_add_expr_to_block(&block, tmp);
    }

    /* Build the body */

    for(c=code->block; c; c=c->block) {
	for(cp=c->ext.case_list; cp; cp=cp->next) {
	    if (cp == unbounded_low) {
		tmp = g95_build_label(ulow_label);
		g95_add_expr_to_block(&body, tmp);
		continue;
	    }

	    if (cp == unbounded_high) {
		tmp = g95_build_label(uhigh_label);
		g95_add_expr_to_block(&body, tmp);
		continue;
	    }

	    if (cp->low == NULL && cp->high == NULL)  /* Case DEFAULT.  */
		low = high = NULL_TREE;

	    else {
		low = bi_to_tree(cp->low->value.integer, kind);

		high = (cp->low == cp->high)
		    ? NULL_TREE
		    : bi_to_tree(cp->high->value.integer, kind);
	    }

	    /* Add this case label.  */

	    tmp = g95_case_label(low, high);
	    g95_add_expr_to_block(&body, tmp);
	}

	/* Add the statements for this case.  */

	tmp = g95_trans_code(c->next);
	g95_add_expr_to_block(&body, tmp);

	/* Break to the end of the loop. */

	tmp = g95_build_goto(end_label);
	g95_add_expr_to_block(&body, tmp);
    }

    tmp = g95_finish_block(&body);
    tmp = g95_build_switch(se.expr, tmp);
    g95_add_expr_to_block(&block, tmp);

    tmp = g95_build_label(end_label);
    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}



/* trans_select()-- Translate a SELECT block */

static tree trans_select(g95_code *code) {
g95_expr *expr;
tree tmp;

    if (code->block == NULL || code->block->ext.case_list == NULL)
	return empty_stmt_node;

    /* Normal select statements put the condition in expr, computed GOTO
     * statements put it in expr2. */

    expr = (code->expr == NULL) ? code->expr2 : code->expr;

    switch(expr->ts.type) {
    case BT_INTEGER:    tmp = integer_select(code, expr);    break;
    case BT_LOGICAL:    tmp = logical_select(code, expr);    break;
    case BT_CHARACTER:  tmp = character_select(code, expr);  break;
    default:
	g95_internal_error("g95_trans_select(): Bad type");
	tmp = NULL_TREE;
    }

    return tmp;
}



/* trans_cycle()-- CYCLE a DO loop. The label decl has already
 * been created by g95_trans_do(), it's in TREE_PURPOSE(backend_decl)
 * of the g95_code node at the head of the loop. We must mark the
 * label as used. */

static tree trans_cycle(g95_code *code) {
tree cycle_label;

    cycle_label = TREE_PURPOSE(code->ext.block->backend_decl);
    TREE_USED(cycle_label) = 1;
    return g95_build_goto(cycle_label);
}



/* trans_exit()-- EXIT a DO loop.  Similar to CYCLE, but now the
 * label is in TREE_VALUE(backend_decl) of the g95_code node at the
 * head of the loop. */

static tree trans_exit(g95_code *code) {
tree exit_label;

    exit_label = TREE_VALUE(code->ext.block->backend_decl);
    TREE_USED(exit_label) = 1;
    return g95_build_goto(exit_label);
}



/* g95_derived_type_init()-- Return the address of a derived type
 * variable that is initialized in the default manner.  If the
 * typespec is not a derived type or there is no default
 * initialization, we just return a null_pointer_node. */

tree g95_derived_type_init(g95_typespec *ts) {
tree decl, var, value;

    if (ts->type != BT_DERIVED || ts->derived->attr.itype != ITYPE_NONE)
	return null_pointer_node;

    decl = ts->derived->backend_decl;
    value = G95_DTYPE_INITIAL(decl);

    if (value == NULL_TREE)
	return null_pointer_node;

    if (G95_DTYPE_INIT_VAR(decl) == NULL) {
	var = g95_create_var(G95_DTYPE_TYPE(decl));
	TREE_STATIC(var) = 1;
	TREE_ADDRESSABLE(var) = 1;
	DECL_INITIAL(var) = value;

	G95_DTYPE_INIT_VAR(decl) = var;
    }

    return g95_addr_expr(G95_DTYPE_INIT_VAR(decl));
}



/* allocate_scalar()-- Allocate a scalar object. */

static void allocate_scalar(g95_expr *expr, g95_se *se, tree flag) {
tree length, tmp, init;
g95_se se0;

    g95_init_se(&se0, se);
    se0.reflevel = 2;

    g95_conv_expr(&se0, expr);

    if (expr->ts.type == BT_CHARACTER) {
	if (expr->ts.cl->length == NULL)
	    tmp = g95_call_library(void_type_node, PREFIX "allocate_string",
				   se0.expr, se0.string_length, flag,
				   NULL_TREE);

	else {
	    se0.reflevel = 0;
	    length = g95_conv_char_length(&se0, &expr->ts);

	    tmp = g95_call_library(void_type_node, PREFIX "allocate_string",
				   se0.expr, length, flag, NULL_TREE);
	}

    } else {
	init = g95_derived_type_init(&expr->ts);

	length = g95_ts_size(&expr->ts);
	tmp = g95_call_library(void_type_node, PREFIX "allocate_scalar",
			       se0.expr, length, init, flag, NULL_TREE);
    }

    g95_add_expr_to_block(&se->pre, tmp);
}



/* init_section_info()-- Initialize the section_info[] array with
 * information about the array being allocated.  The first integer is
 * the rank, the second is the element size.  This is followed by a
 * pair of integers for each dimension that give the upper and lower
 * bound, except for the final upper bound of a coarray. */

static void init_section_info(g95_alloc *a, g95_se *se) {
tree info[3*2*G95_MAX_DIMENSIONS];
stmtblock_t pre, post;
g95_typespec *ts;
int i, n, d;
g95_se se0;

    g95_init_block(&pre);
    g95_init_block(&post);

    n = 0;

    info[n++] = g95_build_int(a->rank + (a->corank << 8), 0);

    ts = &a->expr->ts;

    info[n++] = (ts->type != BT_CHARACTER)
	? g95_ts_size(ts)
	: ts->cl->backend_decl;

    d = a->rank + a->corank;
    for(i=0; i<d; i++) {
	if (a->lower[i] == NULL)
	    info[n++] = integer_one_node;

	else {
	    g95_init_se(&se0, NULL);
	    g95_conv_expr(&se0, a->lower[i]);

	    info[n++] = g95_build_section_info(a->lower[i], &se0, &pre, &post);
	}

	if (i < d-1 || a->corank == 0) {
	    g95_init_se(&se0, NULL);
	    g95_conv_expr(&se0, a->upper[i]);

	    info[n++] = g95_build_section_info(a->upper[i], &se0, &pre, &post);
	}
    }

    g95_add_block_to_block(&se->pre, &pre);
    g95_add_block_to_block(&se->post, &post);

    for(i=0; i<n; i++)
	g95_set_section_info(&se->pre, i, info[i]);
}



/* allocate_array()-- Generate code to allocate an array. */

static void allocate_array(g95_alloc *a, g95_se *se, tree flag) {
tree tmp, init, pointer;

    g95_conv_expr(se, a->expr);
    se->expr = G95_ARRAY_DESC(se->expr);
    g95_reflevel(se, 1);

    init_section_info(a, se);  /* Delicate ordering here */

    pointer = g95_pointer_expr(a->expr)
	? integer_one_node
	: integer_zero_node;

    init = g95_derived_type_init(&a->expr->ts);

    tmp = g95_call_library(void_type_node, PREFIX "allocate_array",
			   se->expr, pointer, init, flag, NULL_TREE);

    g95_add_expr_to_block(&se->pre, tmp);
}



/* allocate_coarray()-- Generate code to allocate a coarray. */

static void allocate_coarray(g95_alloc *a, g95_se *se, tree flag) {
tree tmp, init;

    se->reflevel = 1;
    g95_conv_descriptor(se, a->expr, 0);

    init_section_info(a, se);

    init = g95_derived_type_init(&a->expr->ts);

    tmp = g95_call_library(void_type_node, PREFIX "allocate_coarray",
			   se->expr, init, flag, NULL_TREE);

    g95_add_expr_to_block(&se->pre, tmp);
}



/* trans_allocate()-- Translate an ALLOCATE statement.  If the
 * statement has a STAT variable, we zero it, then generate statements
 * that initialize the right descriptors followed by allocate_array()
 * calls for arrays or just simple allocate_scalar() calls.  The
 * library calls update STAT if something goes wrong, or does nothing
 * if the STAT variable is already nonzero.  If something goes wrong
 * and there is no STAT variable, the program is terminated. */

static tree trans_allocate(g95_code *code) {
stmtblock_t block;
g95_alloc *alloc;
g95_se se;
tree flag;

    g95_init_block(&block);
    g95_set_locus(&block, &code->ext.alloc_list->expr->where);

    g95_init_se(&se, NULL);

    if (code->expr == NULL)
	flag = integer_zero_node;

    else {
	flag = integer_one_node;
	g95_add_modify_expr(&block, g95_junk_stat, integer_zero_node);
    }

    /* Loop over the allocations */

    for(alloc=code->ext.alloc_list; alloc; alloc=alloc->next)
	if (alloc->corank > 0)
	    allocate_coarray(alloc, &se, flag);

	else if (alloc->rank > 0)
	    allocate_array(alloc, &se, flag);

	else
	    allocate_scalar(alloc->expr, &se, flag);

    g95_add_block_to_block(&block, &se.pre);
    g95_add_block_to_block(&block, &se.post);

    if (code->expr != NULL) {
	g95_init_se(&se, NULL);
	g95_conv_expr(&se, code->expr);

	g95_add_block_to_block(&block, &se.pre);
	g95_add_modify_expr(&block, se.expr, g95_junk_stat);
	g95_add_block_to_block(&block, &se.post);
    }

    return g95_finish_block(&block);
}



static void deallocate_pointer(g95_se *se, g95_expr *expr, tree flag) {
g95_se se0;
tree t, tmp;

    g95_init_se(&se0, se);
    se0.reflevel = 2;
    g95_conv_expr(&se0, expr);

    if (expr->ts.type == BT_DERIVED &&
	G95_DTYPE_ALLOCS(expr->ts.derived->backend_decl) != NULL_TREE) {

	tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(se0.expr)), se0.expr);

	t = G95_DTYPE_ALLOCS(expr->ts.derived->backend_decl);

	tmp = g95_call_library(void_type_node, PREFIX "deep_dealloc", tmp,
			       t, NULL_TREE);
	g95_add_expr_to_block(&se->pre, tmp);
    }

    tmp = g95_call_library(void_type_node, PREFIX "deallocate_pointer",
			   se0.expr, flag, NULL_TREE);

    g95_add_expr_to_block(&se->pre, tmp);
}



/* deallocate_array()-- Deallocate an allocatable array or an
 * array pointer. */

static void deallocate_array(g95_se *se, char *name, g95_expr *expr,
			     tree flag) {
tree tmp, alloc;
g95_se se0;

    g95_init_se(&se0, se);
    se0.reflevel = 1;
    g95_conv_descriptor(&se0, expr, 1);

    alloc = (expr->ts.type == BT_DERIVED && !g95_c_ptr(&expr->ts) &&
	     G95_DTYPE_ALLOCS(expr->ts.derived->backend_decl) != NULL_TREE)
	? G95_DTYPE_ALLOCS(expr->ts.derived->backend_decl)
	: null_pointer_node;

    tmp = g95_call_library(void_type_node, name,
			   se0.expr, alloc, flag, NULL_TREE);

    g95_add_expr_to_block(&se->pre, tmp);
}



/* trans_deallocate()-- Translate a DEALLOCATE statement */

static tree trans_deallocate(g95_code *code) {
stmtblock_t block;
g95_alloc *alloc;
g95_expr *expr;
g95_se se;
tree flag;

    g95_init_se(&se, NULL);
    g95_set_locus(&se.pre, &code->ext.alloc_list->expr->where);

    if (code->expr == NULL)
	flag = integer_zero_node;

    else {
	g95_add_modify_expr(&se.pre, g95_junk_stat, integer_zero_node);
	flag = integer_one_node;
    }

    for(alloc=code->ext.alloc_list; alloc; alloc=alloc->next) {
	expr = alloc->expr;

	if (alloc->corank > 0)
	    deallocate_array(&se, PREFIX "deallocate_coarray", expr, flag);

	else if (expr->rank > 0)
	    deallocate_array(&se, PREFIX "deallocate_array", expr, flag);

	else if (expr->ts.type == BT_CHARACTER &&
		 expr->ts.cl == &g95_unknown_charlen)
	    g95_call_temp_free(&se.post, expr->symbol->backend_decl);

	else
	    deallocate_pointer(&se, expr, flag);
    }

    if (code->expr != NULL) {
	g95_conv_expr(&se, code->expr);
	g95_add_modify_expr(&se.pre, se.expr, g95_junk_stat);
    }

    g95_init_block(&block);

    g95_add_block_to_block(&block, &se.pre);
    g95_add_block_to_block(&block, &se.post);

    return g95_finish_block(&block);
}


static tree trans_label_assign(g95_code *code) {
stmtblock_t block;
g95_se se;

    g95_init_block(&block);

    g95_init_se(&se, NULL);
    se.expr = code->sym->backend_decl;
    g95_reflevel(&se, 0);

    g95_add_modify_expr(&block, se.expr, g95_build_int(code->label->value, 0));

    return g95_finish_block(&block);
}



/* annotate_node()-- Annotate a node with line number information. */

static void annotate_node(tree decl) {

    if (!EXPR_HAS_LOCATION(decl))
	annotate_with_locus(decl, input_location);

    if (TREE_CODE(decl) == COMPOUND_EXPR) {
	annotate_node(TREE_OPERAND(decl, 0));
	annotate_node(TREE_OPERAND(decl, 1));
    }
}



/* ac_start()-- Start a constructor assignment. */

static tree ac_start(g95_code *code) {
tree var, tmp, array, element_size, dynamic;
stmtblock_t block;
int i, n, rank;
g95_se se, se0;

    if (code->sym->backend_decl == NULL)
	g95_create_procedure_variable(code->sym); /* Hack */

    array = code->sym->backend_decl;
    g95_init_block(&block);

    if (G95_ARRAY_AC(array) == NULL_TREE) {
	var = g95_create_var(g95_ac_info_type);
	G95_ARRAY_AC(array) = g95_addr_expr(var);
    }

    element_size = (code->sym->ts.type == BT_CHARACTER &&
		    code->sym->ts.cl == &g95_unknown_charlen)
	? integer_minus_one_node
	: g95_ts_size(&code->sym->ts);

    g95_init_se(&se, NULL);

    if (code->ext.shape == NULL)
	dynamic = integer_one_node;

    else {
	dynamic = integer_zero_node;

	n = 0;
	rank = code->sym->as->rank;

	g95_set_section_info(&se.pre, n++, g95_build_int(rank, 0));
	g95_set_section_info(&se.pre, n++, element_size);

	for(i=0; i<rank; i++) {
	    g95_set_section_info(&se.pre, n++, integer_one_node);

	    g95_init_se(&se0, &se);
	    g95_conv_expr(&se0, code->ext.shape[i]);
	    g95_raise_chains(&se0);

	    g95_set_section_info(&se.pre, n++, se0.expr);
	}
    }

    g95_add_block_to_block(&block, &se.pre);
    g95_add_block_to_block(&block, &se.post);

    tmp = g95_call_library(pvoid_type_node, PREFIX "start_ac_assign",
			   G95_ARRAY_AC(array),
			   g95_addr_expr(G95_ARRAY_DESC(array)),
			   dynamic, element_size, NULL_TREE);

    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}


/* ac_assign()-- Assign to an expandable constructor. */

static tree ac_assign(g95_code *code) {
tree info, tmp, len;
stmtblock_t block;
g95_se se;

    g95_init_se(&se, NULL);
    se.reflevel = g95_c_ptr(&code->sym->ts) ? 2 : 1;

    info = G95_ARRAY_AC(code->sym->backend_decl);

    if (code->expr->rank == 0) {
	g95_conv_expr(&se, code->expr);

	len = (se.string_length == NULL)
	    ? integer_minus_one_node
	    : se.string_length;

	tmp = g95_call_library(pvoid_type_node, PREFIX "ac_assign",
			       info, se.expr, len, NULL_TREE);

    } else {
	g95_conv_descriptor(&se, code->expr, 1);

	len = (se.string_length == NULL)
	    ? integer_minus_one_node
	    : se.string_length;

	tmp = g95_call_library(pvoid_type_node, PREFIX "ac_array",
			       info, se.expr, len, NULL_TREE);
    }

    g95_init_block(&block);
    g95_add_block_to_block(&block, &se.pre);
    g95_add_expr_to_block(&block, tmp);
    g95_add_block_to_block(&block, &se.post);

    return g95_finish_block(&block);
}



/* trans_where_assign()-- where assignment within a forall. */

static tree trans_where_assign(g95_code *code) {
stmtblock_t block;
g95_se se1, se2;
tree tmp;

    g95_init_block(&block);
    g95_init_se(&se1, NULL);
    g95_init_se(&se2, NULL);

    se1.reflevel = 1;
    se2.reflevel = 1;

    g95_conv_expr(&se1, code->expr);
    g95_add_block_to_block(&block, &se1.pre);

    g95_conv_expr(&se2, code->expr2);
    g95_add_block_to_block(&block, &se2.pre);

    tmp = g95_ts_size(&code->expr->ts);
    tmp = g95_call_library(void_type_node, PREFIX "where_copy",
			   se1.expr, se2.expr, tmp, NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);

    return g95_finish_block(&block);
}


/* trans_critical()-- Translate a CRITICAL block.  This amounts to
 * calling barriers before and after the block. */

static tree trans_critical(g95_code *code) {
stmtblock_t block;
tree tmp;
    
    g95_init_block(&block);

    tmp = g95_call_library(void_type_node, PREFIX "start_critical", NULL);
    g95_add_expr_to_block(&block, tmp);

    g95_add_expr_to_block(&block, g95_trans_code(code->block));

    tmp = g95_call_library(void_type_node, PREFIX "end_critical", NULL);
    g95_add_expr_to_block(&block, tmp);

    return g95_finish_block(&block);
}


/* trans_sync()-- Translate SYNC ALL and SYNC MEMORY statements. */

static tree trans_sync(char *name, g95_code *code) {
stmtblock_t block;
g95_se se1, se2;
tree tmp;

    g95_init_block(&block);
    g95_init_se(&se1, NULL);

    if (code->ext.sync.stat == NULL)
	se1.expr = null_pointer_node;

    else {
	se1.reflevel = 1;
	g95_conv_expr(&se1, code->ext.sync.stat);
    }

    g95_init_se(&se2, NULL);
    if (code->ext.sync.errmsg == NULL) {
	se2.expr = null_pointer_node;
	se2.string_length = integer_zero_node;

    } else {
	se2.reflevel = 1;
	g95_conv_expr(&se2, code->ext.sync.errmsg);
    }

    g95_add_block_to_block(&block, &se1.pre);
    g95_add_block_to_block(&block, &se2.pre);

    tmp = g95_call_library(void_type_node, name,
			   se1.expr, se2.expr, se2.string_length, NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);

    return g95_finish_block(&block);
}


/* trans_scalar_notify()-- Translate scalar NOTIFY and SYNC IMAGES
 * statements. */

static tree trans_scalar_notify(g95_code *code, char *name) {
g95_se se1, se2, se3;
stmtblock_t block;
tree tmp;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);

    if (code->expr == NULL)
	se1.expr = integer_minus_one_node;

    else
	g95_conv_expr(&se1, code->expr);

    g95_init_se(&se2, NULL);

    if (code->ext.sync.stat == NULL)
	se2.expr = null_pointer_node;

    else {
	se2.reflevel = 1;
	g95_conv_expr(&se2, code->ext.sync.stat);
    }

    g95_init_se(&se3, NULL);
    if (code->ext.sync.errmsg == NULL) {
	se3.expr = null_pointer_node;
	se3.string_length = integer_zero_node;

    } else {
	se3.reflevel = 1;
	g95_conv_expr(&se3, code->ext.sync.errmsg);
    }

    g95_add_block_to_block(&block, &se1.pre);
    g95_add_block_to_block(&block, &se2.pre);
    g95_add_block_to_block(&block, &se3.pre);

    tmp = g95_call_library(void_type_node, name,
			   se1.expr, se2.expr, se3.expr, se3.string_length,
			   NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);
    g95_add_block_to_block(&block, &se3.post);

    return g95_finish_block(&block);
}



/* trans_vector_notify()-- Translate vector NOTIFY and SYNC IMAGES
 * statements. */

static tree trans_vector_notify(g95_code *code, char *name) {
g95_se se1, se2, se3;
stmtblock_t block;
tree tmp;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);
    se1.reflevel = 1;

    g95_conv_descriptor(&se1, code->expr, 0);

    g95_init_se(&se2, NULL);

    if (code->ext.sync.stat == NULL)
	se2.expr = null_pointer_node;

    else {
	se2.reflevel = 1;
	g95_conv_expr(&se2, code->ext.sync.stat);
    }

    g95_init_se(&se3, NULL);
    if (code->ext.sync.errmsg == NULL) {
	se3.expr = null_pointer_node;
	se3.string_length = integer_zero_node;

    } else {
	se3.reflevel = 1;
	g95_conv_expr(&se3, code->ext.sync.errmsg);
    }

    g95_add_block_to_block(&block, &se1.pre);
    g95_add_block_to_block(&block, &se2.pre);
    g95_add_block_to_block(&block, &se3.pre);

    tmp = g95_call_library(void_type_node, name,
			   se1.expr, se2.expr, se3.expr, se3.string_length,
			   NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);
    g95_add_block_to_block(&block, &se3.post);

    return g95_finish_block(&block);
}



/* trans_scalar_query()-- Translate a scalar QUERY statements */

static tree trans_scalar_query(g95_code *code) {
g95_se se1, se2, se3, se4;
stmtblock_t block;
tree tmp;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);

    if (code->expr == NULL)
	se1.expr = integer_minus_one_node;

    else
	g95_conv_expr(&se1, code->expr);

    g95_init_se(&se2, NULL);

    if (code->ext.sync.stat == NULL)
	se2.expr = null_pointer_node;

    else {
	se2.reflevel = 1;
	g95_conv_expr(&se2, code->ext.sync.stat);
    }

    g95_init_se(&se3, NULL);
    if (code->ext.sync.errmsg == NULL) {
	se3.expr = null_pointer_node;
	se3.string_length = integer_zero_node;

    } else {
	se3.reflevel = 1;
	g95_conv_expr(&se3, code->ext.sync.errmsg);
    }

    g95_init_se(&se4, NULL);

    if (code->ext.sync.ready == NULL)
	se4.expr = null_pointer_node;

    else {
	se4.reflevel = 1;
	g95_conv_expr(&se4, code->ext.sync.ready);
    }

    g95_add_block_to_block(&block, &se1.pre);
    g95_add_block_to_block(&block, &se2.pre);
    g95_add_block_to_block(&block, &se3.pre);
    g95_add_block_to_block(&block, &se4.pre);

    tmp = g95_call_library(void_type_node, PREFIX "query0",
			   se1.expr, se2.expr, se3.expr, se3.string_length,
			   se4.expr, NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);
    g95_add_block_to_block(&block, &se3.post);
    g95_add_block_to_block(&block, &se4.post);

    return g95_finish_block(&block);
}



/* trans_vector_notify()-- Translate a vector QUERY statements */

static tree trans_vector_query(g95_code *code) {
g95_se se1, se2, se3, se4;
stmtblock_t block;
tree tmp;

    g95_init_block(&block);

    g95_init_se(&se1, NULL);

    g95_conv_descriptor(&se1, code->expr, 0);

    g95_init_se(&se2, NULL);

    if (code->ext.sync.stat == NULL)
	se2.expr = null_pointer_node;

    else {
	se2.reflevel = 1;
	g95_conv_expr(&se2, code->ext.sync.stat);
    }

    g95_init_se(&se3, NULL);
    if (code->ext.sync.errmsg == NULL) {
	se3.expr = null_pointer_node;
	se3.string_length = integer_zero_node;

    } else {
	se3.reflevel = 1;
	g95_conv_expr(&se3, code->ext.sync.errmsg);
    }

    g95_init_se(&se4, NULL);
    if (code->ext.sync.ready == NULL)
	se4.expr = null_pointer_node;

    else {
	se4.reflevel = 1;
	g95_conv_expr(&se4, code->ext.sync.ready);
    }

    g95_add_block_to_block(&block, &se1.pre);
    g95_add_block_to_block(&block, &se2.pre);
    g95_add_block_to_block(&block, &se3.pre);
    g95_add_block_to_block(&block, &se4.pre);

    tmp = g95_call_library(void_type_node, PREFIX "query1",
			   se1.expr, se2.expr, se3.expr, se3.string_length,
			   se4.expr, NULL);

    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &se1.post);
    g95_add_block_to_block(&block, &se2.post);
    g95_add_block_to_block(&block, &se3.post);
    g95_add_block_to_block(&block, &se4.post);

    return g95_finish_block(&block);
}



/* g95_trans_code()-- Translate a list of executable statements. */

tree g95_trans_code(g95_code *code) {
stmtblock_t block;
int line, flag;
tree decl;

    if (code == NULL)
	return NULL_TREE;

    g95_init_block(&block);
    line = input_line;

    for(; code; code=code->next) {
	g95_set_backend_locus(&code->where);
	if (code->here != NULL) {
	    decl = g95_build_label(g95_get_label_decl(code->here));
	    g95_add_expr_to_block(&block, decl);
	}

	flag = 0;

	switch (code->type) {
	case EXEC_NOP:
	case EXEC_CONTINUE:
	case EXEC_ENTRY:
	    decl = NULL_TREE;
	    break;

	case EXEC_ASSIGN:
	    decl = g95_trans_assignment(code->expr, code->expr2);
	    flag = 1;
	    break;

	case EXEC_AC_START:
	    decl = ac_start(code);
	    flag = 1;
	    break;

	case EXEC_WHERE_ASSIGN:
	    decl = trans_where_assign(code);
	    flag = 1;
	    break;

	case EXEC_AC_ASSIGN:
	    decl = ac_assign(code);
	    flag = 1;
	    break;

	case EXEC_POINTER_ASSIGN:
	    decl = g95_trans_pointer_assign(code);
	    flag = 1;
	    break;

	case EXEC_CYCLE:
	    decl = trans_cycle(code);
	    break;

	case EXEC_EXIT:
	    decl = trans_exit(code);
	    break;

	case EXEC_GOTO:
	    decl = trans_goto(code);
	    break;

	case EXEC_ERROR_STOP:
	case EXEC_STOP:
	    decl = trans_stop(code);
	    break;

	case EXEC_PAUSE:
	    decl = trans_pause(code);
	    break;

	case EXEC_CALL:
	    decl = trans_call(code);
	    flag = 1;
	    break;

	case EXEC_RETURN:
	    decl = trans_return(code);
	    break;

	case EXEC_IF:
	    decl = trans_if(code);
	    flag = 1;
	    break;

	case EXEC_ARITHMETIC_IF:
	    decl = trans_arithmetic_if(code);
	    flag = 1;
	    break;

	case EXEC_DO:
	    decl = trans_do(code);
	    flag = 1;
	    break;

	case EXEC_DO_WHILE:
	    decl = trans_do_while(code);
	    flag = 1;
	    break;

	case EXEC_SELECT:
	    decl = trans_select(code);
	    flag = 1;
	    break;

	case EXEC_FORALL:
	    g95_internal_error("g95_trans_code(): Unexpected FORALL");
	    break;

	case EXEC_WHERE:
	    g95_internal_error("g95_trans_code(): Unexpected WHERE");
	    break;

	case EXEC_ALLOCATE:
	    decl = trans_allocate(code);
	    break;

	case EXEC_ENDFILE:
	    decl = g95_trans_endfile(code);
	    break;

	case EXEC_INQUIRE:
	    decl = g95_trans_inquire(code);
	    break;

	case EXEC_REWIND:
	    decl = g95_trans_rewind(code);
	    break;

	case EXEC_TRANSFER:
	    decl = g95_trans_transfer(code);
	    break;

	case EXEC_DT_END:
	    decl = g95_trans_dt_end(code);
	    break;

	case EXEC_LABEL_ASSIGN:
	    decl = trans_label_assign(code);
	    flag = 1;
	    break;

	case EXEC_DEALLOCATE:
	    decl = trans_deallocate(code);
	    break;

	case EXEC_OPEN:
	    decl = g95_trans_open(code);
	    break;

	case EXEC_CLOSE:
	    decl = g95_trans_close(code);
	    break;

	case EXEC_FLUSH:
	    decl = g95_trans_flush(code);
	    break;

	case EXEC_WAIT:
	    decl = g95_trans_wait(code);
	    break;

	case EXEC_READ:
	    decl = g95_trans_read(code);
	    break;

	case EXEC_WRITE:
	    decl = g95_trans_write(code);
	    break;

	case EXEC_IOLENGTH:
	    decl = g95_trans_iolength(code);
	    break;

	case EXEC_BACKSPACE:
	    decl = g95_trans_backspace(code);
	    break;

	case EXEC_SYNC_ALL:
	    decl = trans_sync(PREFIX "sync_all", code);
	    break;

	case EXEC_SYNC_MEMORY:
	    decl = trans_sync(PREFIX "sync_memory", code);
	    break;

	case EXEC_SYNC_IMAGES:
	    decl = (code->expr == NULL || code->expr->rank == 0)
		? trans_scalar_notify(code, PREFIX "sync_images0")
		: trans_vector_notify(code, PREFIX "sync_images1");

	    break;

	case EXEC_NOTIFY:
	    decl = (code->expr == NULL || code->expr->rank == 0)
		? trans_scalar_notify(code, PREFIX "notify0")
		: trans_vector_notify(code, PREFIX "notify1");
	    break;

	case EXEC_QUERY:
	    decl = (code->expr == NULL || code->expr->rank == 0)
		? trans_scalar_query(code)
		: trans_vector_query(code);
	    break;

	case EXEC_SYNC_TEAM:
	    decl = empty_stmt_node;
	    break;

	case EXEC_CRITICAL:
	    decl = trans_critical(code);
	    break;

	default:
	    g95_internal_error("g95_trans_code(): Bad statement code");
	}

	if (flag && g95_option.trace == TRACE_FULL)
	    g95_set_locus(&block, &code->where);

	if (decl != NULL && !IS_EMPTY_STMT(decl)) {
	    annotate_node(decl);
	    g95_add_expr_to_block(&block, decl);
	}
    }

    /* Return the finished block. */
    input_line = line;
    return g95_finish_block(&block);
}
