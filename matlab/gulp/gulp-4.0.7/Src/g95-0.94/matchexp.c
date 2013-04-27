/* Expression matcher
   Copyright (C) 2000-2008 Free Software Foundation, Inc.
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

/* matchexp.c-- Expression parser */


#include <string.h>
#include <ctype.h>
#include "g95.h"

static char expression_syntax[] = "Syntax error in expression at %C";

static g95_locus operator_locus;



/* g95_match_defined_op_name()-- Match a user-defined operator name.
 * This is a normal name with a few restrictions.  The error_flag
 * controls whether an error is raised if 'true' or 'false' are used
 * or not. */

match g95_match_defined_op_name(char *result, int error_flag) {
static char *badops[] = {
 "and", "or", "not", "eqv", "neqv", "eq", "ne", "ge", "le", "lt", "gt", NULL };

char name[G95_MAX_SYMBOL_LEN+1];
g95_locus old_loc;
match m;
int i;

    old_loc = g95_current_locus;

    m = g95_match(" . %n .", name);
    if (m != MATCH_YES)
	return m;

/* .true. and .false. have interpretations as constants.  Trying to
 * use these as operators will fail at a later time */

    if (strcmp(name, "true") == 0 || strcmp(name, "false") == 0) {
	if (error_flag)
	    goto error;

	g95_current_locus = old_loc;
	return MATCH_NO;
    }

    for(i=0; badops[i]; i++)
	if (strcmp(badops[i], name) == 0)
	    goto error;

    for(i=0; name[i]; i++)
	if (!g95_uopchar(name[i])) {
	    g95_error("Bad character '%c' in OPERATOR name at %L",
		      name[i], &g95_def_locus);
	    return MATCH_ERROR;
	}

    strcpy(result, name);
    return MATCH_YES;

error:
    g95_error("The name '%s' cannot be used as a defined operator at %L",
	      name, &g95_def_locus);

    g95_current_locus = old_loc;
    return MATCH_ERROR;
}



/* match_defined_operator()-- Match a user defined operator.  The
 * symbol found must be an operator already. */

static match match_defined_operator(g95_user_op **result) {
char name[G95_MAX_SYMBOL_LEN+1];
match m;

    m = g95_match_defined_op_name(name, 0);
    if (m != MATCH_YES)
	return m;

    *result = g95_get_uop(name);
    return MATCH_YES;
}



/* next_operator()-- Checks to see if the given operator is next on
 * the input.  If this is not the case, the parse pointer remains
 * where it was. */

static int next_operator(g95_intrinsic_op t) {
g95_intrinsic_op u;
g95_locus old_loc;

    old_loc = g95_current_locus;

    g95_gobble_whitespace();
    operator_locus = g95_current_locus;

    if (g95_match_intrinsic_op(&u) == MATCH_YES && t == u)
	return 1;

    g95_current_locus = old_loc;
    return 0;
}



/* match_primary()-- Match a primary expression */

static match match_primary(g95_expr **result) {
g95_expr *e, *f;
match m;

    m = g95_match_literal_constant(result, 0);
    if (m != MATCH_NO)
	return m;

    m = g95_match_array_constructor(result);
    if (m != MATCH_NO)
	return m;

    m = g95_match_rvalue(result);
    if (m != MATCH_NO)
	return m;

/* Match an expression in parenthesis */

    if (g95_match_char('(') != MATCH_YES)
	return MATCH_NO;

    m = g95_match_expr(&f);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) return m;

    m = g95_match_char(')');
    if (m == MATCH_NO) {
	g95_error("Expected a right parenthesis in expression at %C");
	m = MATCH_ERROR;
    }

    if (m != MATCH_YES) {
	g95_free_expr(f);
	return m;
    }

    if (f->type != EXPR_VARIABLE)
	*result = f;

    else {
	e = g95_get_expr();

	e->type = EXPR_OP;
	e->value.op.operator = INTRINSIC_PAREN;
	e->value.op.op1 = f;
	e->where = f->where;

	*result = e;
    }

    return MATCH_YES;

syntax:
    g95_error(expression_syntax);
    return MATCH_ERROR;
}



/* build_node()-- Build an operator expression node. */

static g95_expr *build_node(g95_intrinsic_op operator, g95_locus *where,
			    g95_expr *op1, g95_expr *op2) {
g95_expr *new;

    new = g95_get_expr();
    new->type = EXPR_OP;
    new->where = *where;

    new->value.op.operator = operator;
    new->value.op.op1 = op1;
    new->value.op.op2 = op2;

    return new;
}



/* match_level_1()-- Match a level 1 expression */

static match match_level_1(g95_expr **result) {
g95_user_op *uop;
g95_expr *e, *f;
g95_locus where;
match m;

    where = g95_current_locus; 
    uop = NULL; 

    m = match_defined_operator(&uop);
    if (m == MATCH_ERROR)
	return m;

    m = match_primary(&e);
    if (m != MATCH_YES)
	return m;

    if (uop == NULL)
	*result = e;

    else {
	f = build_node(INTRINSIC_USER, &where, e, NULL);
	f->value.op.uop = uop;
	*result = f;
    }

    return MATCH_YES;
}



static match match_mult_operand(g95_expr **result) {
g95_expr *e, *exp, *r;
g95_locus where;
match m;

    m = match_level_1(&e);
    if (m != MATCH_YES)
	return m;

    if (!next_operator(INTRINSIC_POWER)) {
	*result = e;
	return MATCH_YES;
    }

    where = operator_locus;

    m = match_mult_operand(&exp);
    if (m == MATCH_NO) g95_error("Expected exponent in expression at %C");
    if (m != MATCH_YES) {
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    r = g95_power(e, exp);
    if (r == NULL) {
	g95_free_expr(e);
	g95_free_expr(exp);
	return MATCH_ERROR;
    }

    r->where = where;
    *result = r;

    return MATCH_YES;
}



static match match_add_operand(g95_expr **result) {
g95_expr *all, *e, *total;
g95_locus where, old_loc;
match m;
int i;

    m = match_mult_operand(&all);
    if (m != MATCH_YES)
	return m;

    for(;;) {    /* Build up a string of products or quotients */
	i = 0;

	old_loc = g95_current_locus;

	if (next_operator(INTRINSIC_TIMES))
	    i = INTRINSIC_TIMES;

	else if (next_operator(INTRINSIC_DIVIDE))
	    i = INTRINSIC_DIVIDE;

	else
	    break;

	where = operator_locus;

	m = match_mult_operand(&e);
	if (m == MATCH_NO) {
	    g95_current_locus = old_loc;
	    break;
	}

	if (m == MATCH_ERROR) {
	    g95_free_expr(all);
	    return MATCH_ERROR;
	}

	total = (i == INTRINSIC_TIMES)
	    ? g95_multiply(all, e)
	    : g95_divide(all, e);

	if (total == NULL) {
	    g95_free_expr(all);
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	all = total;
	all->where = operator_locus;
    }

    *result = all;
    return MATCH_YES;
}



static int match_add_op(void) {

    if (next_operator(INTRINSIC_MINUS))
	return -1;

    if (next_operator(INTRINSIC_PLUS))
	return 1;

    return 0;
}



/* match_level_2()-- Match a level 2 expression.  */

static match match_level_2(g95_expr **result) {
g95_expr *all, *e, *total;
g95_locus where;
match m;
int i;

    i = match_add_op();
    where = operator_locus;

    m = match_add_operand(&e);
    if (i != 0 && m == MATCH_NO) {
	g95_error(expression_syntax);
	m = MATCH_ERROR;
    }

    if (m != MATCH_YES)
	return m;

    if (i == 0)
	all = e;

    else {
	all = (i == -1)
	    ? g95_uminus(e)
	    : g95_uplus(e);

	if (all == NULL) {
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	all->where = where;
    }

/* Append add-operands to the sum */

    for(;;) {
	i = match_add_op();
	if (i == 0)
	    break;

	where = operator_locus;

	m = match_add_operand(&e);
	if (m == MATCH_NO)
	    g95_error(expression_syntax);

	if (m != MATCH_YES) {
	    g95_free_expr(all);
	    return MATCH_ERROR;
	}

	total = (i == -1)
	    ? g95_subtract(all, e)
	    : g95_add(all, e);

	if (total == NULL) {
	    g95_free_expr(all);
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	total->where = where;
	all = total;
    }

    *result = all;
    return MATCH_YES;
}



/* match_level_3()-- Match a level three expression */

static match match_level_3(g95_expr **result) {
g95_expr *all, *e, *total;
g95_locus where;
match m;

    m = match_level_2(&all);
    if (m != MATCH_YES)
	return m;

    for(;;) {
	if (!next_operator(INTRINSIC_CONCAT))
	    break;

	where = operator_locus;

	m = match_level_2(&e);
	if (m == MATCH_NO) {
	    g95_error(expression_syntax);
	    g95_free_expr(all);
	}

	if (m != MATCH_YES)
	    return MATCH_ERROR;

	total = g95_concat(all, e);

	if (total == NULL) {
	    g95_free_expr(all);
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	total->where = where;
	all = total;
    }

    *result = all;
    return MATCH_YES;
}



/* match_level_4()-- Match a level 4 expression */

static match match_level_4(g95_expr **result) {
g95_expr *left, *right, *r;
g95_locus old_loc, where;
g95_intrinsic_op i;
match m;

    m = match_level_3(&left);
    if (m != MATCH_YES)
	return m;

    old_loc = g95_current_locus;

    g95_gobble_whitespace();
    where = g95_current_locus;

    if (g95_match_intrinsic_op(&i) != MATCH_YES) {
	*result = left;
	return MATCH_YES;
    }

    if (i != INTRINSIC_EQ && i != INTRINSIC_NE && i != INTRINSIC_GE &&
	i != INTRINSIC_LE && i != INTRINSIC_LT && i != INTRINSIC_GT) {
	g95_current_locus = old_loc;
	*result = left;
	return MATCH_YES;
    }

    m = match_level_3(&right);
    if (m == MATCH_NO)
	g95_error(expression_syntax);

    if (m != MATCH_YES) {
	g95_free_expr(left);
	return MATCH_ERROR;
    }

    switch(i) {
    case INTRINSIC_EQ:
	r = g95_eq(left, right);
	break;

    case INTRINSIC_NE:
	r = g95_ne(left, right);
	break;

    case INTRINSIC_LT:
	r = g95_lt(left, right);
	break;

    case INTRINSIC_LE:
	r = g95_le(left, right);
	break;

    case INTRINSIC_GT:
	r = g95_gt(left, right);
	break;

    case INTRINSIC_GE:
	r = g95_ge(left, right);
	break;

    default:
	g95_internal_error("match_level_4(): Bad operator");
    }

    if (r == NULL) {
	g95_free_expr(left);
	g95_free_expr(right);
	return MATCH_ERROR;
    }

    r->where = where;
    *result = r;

    return MATCH_YES;
}



static match match_and_operand(g95_expr **result) {
g95_expr *e, *r;
g95_locus where;
match m;
int i;

    i = next_operator(INTRINSIC_NOT);
    where = operator_locus;

    m = match_level_4(&e);
    if (m != MATCH_YES)
	return m;

    r = e;
    if (i) {
	r = g95_not(e);
	if (r == NULL) {
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	r->where = where;
    }

    *result = r;

    return MATCH_YES;
}



static match match_or_operand(g95_expr **result) {
g95_expr *all, *e, *total;
g95_locus where;
match m;

    m = match_and_operand(&all);
    if (m != MATCH_YES)
	return m;

    for(;;) {
	if (!next_operator(INTRINSIC_AND))
	    break;

	where = operator_locus;

	m = match_and_operand(&e);
	if (m == MATCH_NO)
	    g95_error(expression_syntax);

	if (m != MATCH_YES) {
	    g95_free_expr(all);
	    return MATCH_ERROR;
	}

	total = g95_and(all, e);
	if (total == NULL) {
	    g95_free_expr(all);
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	total->where = where;
	all = total;
    }

    *result = all;
    return MATCH_YES;
}



static match match_equiv_operand(g95_expr **result) {
g95_expr *all, *e, *total;
g95_locus where;
match m;

    m = match_or_operand(&all);
    if (m != MATCH_YES)
	return m;

    for(;;) {
	if (!next_operator(INTRINSIC_OR))
	    break;

	where = operator_locus;

	m = match_or_operand(&e);
	if (m == MATCH_NO)
	    g95_error(expression_syntax);

	if (m != MATCH_YES) {
	    g95_free_expr(all);
	    return MATCH_ERROR;
	}

	total = g95_or(all, e);

	if (total == NULL) {
	    g95_free_expr(all);
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	total->where = where;
	all = total;
    }

    *result = all;
    return MATCH_YES;
}



/* match_level_5()-- Match a level 5 expression */

static match match_level_5(g95_expr **result) {
g95_expr *all, *e, *total;
g95_locus where;
match m;
int i;

    m = match_equiv_operand(&all);
    if (m != MATCH_YES)
	return m;

    for(;;) {
	if (next_operator(INTRINSIC_EQV))
	    i = INTRINSIC_EQV;

	else if (next_operator(INTRINSIC_NEQV))
	    i = INTRINSIC_NEQV;

	else
	    break;

	where = operator_locus;

	m = match_equiv_operand(&e);
	if (m == MATCH_NO)
	    g95_error(expression_syntax);

	if (m != MATCH_YES) {
	    g95_free_expr(all);
	    return MATCH_ERROR;
	}

	total = (i == INTRINSIC_EQV)
	    ? g95_eqv(all, e)
	    : g95_neqv(all, e);

	total->where = where;

	if (total == NULL) {
	    g95_free_expr(all);
	    g95_free_expr(e);
	    return MATCH_ERROR;
	}

	all = total;
    }

    *result = all;
    return MATCH_YES;
}



/* g95_match_expr()-- Match an expression.  At this level, we are
 * stringing together level 5 expressions separated by binary operators. */

match g95_match_expr(g95_expr **result) {
g95_expr *all, *e;
g95_user_op *uop;
match m;

    m = match_level_5(&all);
    if (m != MATCH_YES)
	return m;

    for(;;) {
	m = match_defined_operator(&uop);
	if (m == MATCH_NO)
	    break;

	if (m == MATCH_ERROR) {
	    g95_free_expr(all);
	    return MATCH_ERROR;
	}

	m = match_level_5(&e);
	if (m == MATCH_NO)
	    g95_error(expression_syntax);

	if (m != MATCH_YES) {
	    g95_free_expr(all);
	    return MATCH_ERROR;
	}

	all = build_node(INTRINSIC_USER, &g95_current_locus, all, e);
	all->value.op.uop = uop;
    }

    *result = all;
    return MATCH_YES;
}
