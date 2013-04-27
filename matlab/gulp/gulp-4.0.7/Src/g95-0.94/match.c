/* Matching subroutines
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

/* match.c-- matchers in all sizes, shapes and colors. */

#include <stdarg.h>
#include <ctype.h>
#include <string.h>

#include "g95.h"

static match var_element(g95_data_variable *);
static int match_value, enum_bindc;
extern int g95_spec_flag;


/******************** Generic matching subroutines ************************/


/* g95_match_space()-- In free form, match at least one space.  Always
 * matches in fixed form. */

match g95_match_space(void) {
g95_locus old_loc;
int c;

    if (g95_current_form == FORM_FIXED)
	return MATCH_YES;

    old_loc = g95_current_locus;

    c = g95_next_char();
    if (!g95_is_whitespace(c)) {
	g95_current_locus = old_loc;
	return MATCH_NO;
    }

    g95_gobble_whitespace();

    return MATCH_YES;
}



/* g95_match_eos()-- Match an end of statement.  End of statement is
 * optional whitespace, followed by a ';' or '\n' or comment '!'.  If
 * a semicolon is found, we continue to eat whitespace and semicolons. */

match g95_match_eos(void) {
g95_locus old_loc;
int flag, c;

    flag = 0;

    for(;;) { 
	old_loc = g95_current_locus;
	g95_gobble_whitespace();
   
	c = g95_next_char();
	switch(c) {
	case '!':
	    do
		c = g95_next_char();
	    while(c != '\n');

	    /* Fall through */
	case '\n':
	    return MATCH_YES;

	case ';':
	    if (G95_STRICT_F())
		break;

	    flag = 1;
	    continue;
	}

	break;
    }

    g95_current_locus = old_loc;
    return (flag) ? MATCH_YES : MATCH_NO;
}



/* g95_match_small_int()-- Match a small, constant integer expression,
 * like in a kind statement.  On MATCH_YES, 'value' is set. */

match g95_match_small_int(int *value) {
g95_expr *expr;
char *p;
match m;
int j;

    m = g95_match_expr(&expr);
    if (m != MATCH_YES)
	return m;

    p = g95_extract_int(expr, &j);
    g95_free_expr(expr);

    if (p != NULL) {
	g95_error(p);
	m = MATCH_ERROR;
    }

    *value = j;
    return m;
}



/* g95_match_small_literal_int()-- Match a literal integer on the
 * input, setting the value on MATCH_YES.  Literal integers occur in
 * kind-parameters as well as old-style character length
 * specifications. */

match g95_match_small_literal_int(int *value, int stop_code) {
g95_locus old_loc, start;
int digits, i;
char c;

    old_loc = g95_current_locus;
    g95_gobble_whitespace();
    start = g95_current_locus;

    c = g95_next_char();

    if (!isdigit((int) c)) {
	g95_current_locus = old_loc;
	return MATCH_NO;
    }

    i = c - '0';
    digits = 1;

    for(;;) {
	old_loc = g95_current_locus;
	c = g95_next_char();

	if (!isdigit((int) c))
	    break;

	i = 10*i + c - '0';

	if (++digits > 5 && stop_code) {
	    g95_error("Too many digits in stop/pause code at %L", &start);
	    return MATCH_ERROR;
	}

	if (i > 99999999) {
	    g95_error("Integer too large at %L", &start);
	    return MATCH_ERROR;
	}
    }

    g95_current_locus = old_loc;

    *value = i;
    return MATCH_YES;
}



/* g95_match_st_label()-- Matches a statement label.  Uses
 * g95_match_small_literal_int() to do most of the work. */

match g95_match_st_label(g95_st_label **label, int allow_zero) {
g95_locus old_loc;
int label_value;
match m;

    old_loc = g95_current_locus;

    g95_gobble_whitespace();
    g95_label_locus = g95_current_locus;

    m = g95_match_small_literal_int(&label_value, 0);
    if (m != MATCH_YES) {
	g95_current_locus = old_loc;
	return m;
    }

    if ((label_value == 0 && allow_zero) || label_value <= 99999) {
	*label = g95_get_st_label(label_value);

	g95_update_locus(&(*label)->where, &g95_label_locus);
	return MATCH_YES;
    }

    g95_error("Statement label at %C is out of range");
    g95_current_locus = old_loc;
    return MATCH_ERROR;
}



/* g95_match_label()-- Match and validate a label associated with a
 * named IF, DO or SELECT statement.  If the symbol does not have the
 * label attribute, we add it.  We also make sure the symbol does not
 * refer to another (active) block.  A matched label is pointed to by
 * g95_new_block. */

match g95_match_label(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_locus start, where;
match m;

    g95_new_block = NULL;
    start = g95_current_locus;

    g95_gobble_whitespace();
    where = g95_current_locus;

    m = g95_match("%n :", name);
    if (m != MATCH_YES) {
	g95_current_locus = start;
	return m;
    }

    if (g95_get_symbol(name, NULL, &g95_new_block))
	return MATCH_ERROR;

    if (g95_new_block->attr.flavor == FL_LABEL) {
	g95_error("Duplicate construct label '%s' at %L", name, &where);
	return MATCH_ERROR;
    }

    if (g95_add_flavor(&g95_new_block->attr, FL_LABEL,
		       g95_new_block->name, &where) == FAILURE)
	return MATCH_ERROR;

    return MATCH_YES;
}



/* g95_match_strings()-- Try and match the input against an array of
 * possibilities.  If one potential matching string is a substring of
 * another, the longest match takes precedence.  Spaces in the target
 * strings are optional spaces that do not necessarily have to be
 * found in the input stream.  In fixed mode, spaces never appear.  If
 * whitespace is matched, it matches unlimited whitespace in the
 * input.  For this reason, the 'mp' member of the mstring structure
 * is used to track the progress of each potential match.
 *
 * If there is no match we return the tag associated with the
 * terminating NULL mstring structure and leave the locus pointer
 * where it started.  If there is a match we return the tag member of
 * the matched mstring and leave the locus pointer after the matched
 * character.
 *
 * A '%' character is a mandatory space.
 */

int g95_match_strings(mstring *a) {
int no_match, c, possibles;
mstring *p, *best_match;
g95_locus match_loc;

    possibles = 0;

    for(p=a; p->string != NULL; p++) {
	p->mp = p->string;
	possibles++;
    }

    no_match = p->tag;

    best_match = NULL;
    match_loc = g95_current_locus;

    g95_gobble_whitespace();

    while(possibles > 0) {
	c = g95_next_char();

/* Apply the next character to the current possibilities */

	for(p=a; p->string!=NULL; p++) {
	    if (p->mp == NULL)
		continue;

	    if (*p->mp == ' ') {    /* Space matches 1+ whitespace(s) */
		if ((g95_current_form == FORM_FREE) && g95_is_whitespace(c))
		    continue;

		p->mp++;
	    }

	    if (*p->mp != c) {      /* Match failed */
		p->mp = NULL;
		possibles--;
		continue;
	    }

	    p->mp++;
	    if (*p->mp == '\0') {   /* Found a match */
		match_loc = g95_current_locus;
		best_match = p;
		possibles--;
		p->mp = NULL;
	    }
	}
    }

    g95_current_locus = match_loc;

    return (best_match == NULL) ? no_match : best_match->tag;
}



/* g95_match_symbol()-- Match a symbol on the input.  Modifies the
 * pointer to the symbol pointer if successful. */

match g95_match_symbol(g95_symbol **matched_symbol, int host_assoc) {
char buffer[G95_MAX_SYMBOL_LEN+1];
match m;

    m = g95_match_name(buffer);
    if (m != MATCH_YES)
	return m;

    if (host_assoc)
	return (g95_get_ha_symbol(buffer, matched_symbol))
	    ? MATCH_ERROR : MATCH_YES;

    if (g95_get_symbol(buffer, NULL, matched_symbol))
	return MATCH_ERROR;

    return MATCH_YES;
}



/* g95_match_name()-- See if the current input looks like a name of
 * some sort.  Modifies the passed buffer which must be at least
 * G95_MAX_SYMBOL_LEN+1 bytes long.  Optionally stores the locus of
 * the first character in the name. */

match g95_match_name(char *buffer) {
g95_locus old_loc;
int i, c;

    old_loc = g95_current_locus;
    g95_gobble_whitespace();

    g95_def_locus = g95_current_locus;

    c = g95_next_char();
    if (!g95_varchar(c, 1)) {
	g95_current_locus = old_loc;
	return MATCH_NO;
    }

    i = 0;

    do {
	buffer[i++] = c;

	if (i > g95_option.symbol_len) {
	    g95_error("Name at %L is too long", &g95_def_locus);
	    return MATCH_ERROR;
	}

	old_loc = g95_current_locus;
	c = g95_next_char();
    } while(g95_varchar(c, 0));

    buffer[i] = '\0';

    if (G95_STRICT_F() && buffer[i-1] == '_') {
	g95_error("Name '%s' at %L cannot end with an underscore in F mode",
		  buffer, &g95_def_locus);
	return MATCH_ERROR;
    }

    g95_current_locus = old_loc;

    return MATCH_YES;
}



/* g95_match_intrinsic_op()-- Match an intrinsic operator.  Returns an
 * INTRINSIC enum. */

match g95_match_intrinsic_op(g95_intrinsic_op *result) {
g95_intrinsic_op op;
static mstring operators_in[] = {
    minit("+",     INTRINSIC_PLUS),   minit("-",      INTRINSIC_MINUS),
    minit("**",    INTRINSIC_POWER),  minit("//",     INTRINSIC_CONCAT),
    minit("*",     INTRINSIC_TIMES),  minit("/",      INTRINSIC_DIVIDE),
    minit(".and.", INTRINSIC_AND),    minit(".or.",   INTRINSIC_OR),
    minit(".eqv.", INTRINSIC_EQV),    minit(".neqv.", INTRINSIC_NEQV),

    minit("==",    INTRINSIC_EQ),     minit("/=",     INTRINSIC_NE),
    minit(">=",    INTRINSIC_GE),     minit("<=",     INTRINSIC_LE),
    minit("<",     INTRINSIC_LT),     minit(">",      INTRINSIC_GT),
    minit(".not.", INTRINSIC_NOT),    minit(NULL,     INTRINSIC_NONE) },

    old_operators[] = {
	minit(".eq.",  INTRINSIC_EQ),   minit(".ne.",  INTRINSIC_NE), 
	minit(".ge.",  INTRINSIC_GE),   minit(".le.",  INTRINSIC_LE), 
	minit(".lt.",  INTRINSIC_LT),   minit(".gt.",  INTRINSIC_GT), 
	minit(NULL,    INTRINSIC_NONE) };

    op = g95_match_strings(operators_in);
    if (op == INTRINSIC_NONE) {
	if (G95_STRICT_F())
	    return MATCH_NO;

	op = g95_match_strings(old_operators);
	if (op == INTRINSIC_NONE)
	    return MATCH_NO;
    }

    *result = op;
    return MATCH_YES;
}


char *g95_op2string(int i) {
static mstring operators_out[] = {
    minit("+",     INTRINSIC_PLUS),   minit("-",      INTRINSIC_MINUS),
    minit("+",     INTRINSIC_UPLUS),  minit("-",      INTRINSIC_UMINUS),
    minit("**",    INTRINSIC_POWER),  minit("//",     INTRINSIC_CONCAT),
    minit("*",     INTRINSIC_TIMES),  minit("/",      INTRINSIC_DIVIDE),
    minit(".and.", INTRINSIC_AND),    minit(".or.",   INTRINSIC_OR),
    minit(".eqv.", INTRINSIC_EQV),    minit(".neqv.", INTRINSIC_NEQV),
    minit(".eq.",  INTRINSIC_EQ),     minit("==",     INTRINSIC_EQ),
    minit(".ne.",  INTRINSIC_NE),     minit("/=",     INTRINSIC_NE),
    minit(".ge.",  INTRINSIC_GE),     minit(">=",     INTRINSIC_GE),
    minit(".le.",  INTRINSIC_LE),     minit("<=",     INTRINSIC_LE),
    minit(".lt.",  INTRINSIC_LT),     minit("<",      INTRINSIC_LT),
    minit(".gt.",  INTRINSIC_GT),     minit(">",      INTRINSIC_GT),
    minit(".not.", INTRINSIC_NOT),    minit(NULL,     INTRINSIC_NONE) };

    if (i == INTRINSIC_ASSIGN)
	return "=";

    return g95_code2string(operators_out, i);
}



/* g95_match_char()-- Tries to match the next non-whitespace character
 * on the input.  This subroutine does not return MATCH_ERROR.  */

match g95_match_char(char c) {
g95_locus where;

    where = g95_current_locus;
    g95_gobble_whitespace();

    if (g95_next_char() == c)
	return MATCH_YES;

    g95_current_locus = where;
    return MATCH_NO;
}



/* g95_match()-- General purpose matching subroutine.  The target
 * string is a scanf-like format string in which spaces correspond to
 * arbitrary whitespace (including no whitespace), characters
 * correspond to themselves.  The %-codes are:
 *
 * %%  Literal percent sign
 * %e  Expression, pointer to a pointer is set
 * %s  Symbol, pointer to the symbol is set
 * %n  Name, character buffer is set to name
 * %t  Matches end of statement.
 * %o  Matches an intrinsic operator, returned as an INTRINSIC enum.
 * %l  Matches a statement label
 * %v  Matches a variable expression (an lvalue)
 * %   Matches a required space (in free form) and optional spaces.
 */

match g95_match(char *target, ...) {
g95_st_label **label;
g95_locus old_loc;
int matches, *ip;
va_list argp;
char c, *np;
match m, n;
void **vp;
char *p;

    old_loc = g95_current_locus;
    va_start(argp, target);
    m = MATCH_NO;
    matches = 0;
    p = target;

loop:
    c = *p++;
    switch(c) {
    case ' ':   g95_gobble_whitespace(); goto loop;
    case '\0':  m = MATCH_YES; break;

    case '%':
	c = *p++;
	switch(c) {
	case 'e':
	    vp = va_arg(argp, void **);
	    n = g95_match_expr((g95_expr **) vp);
	    if (n != MATCH_YES) { m = n; goto not_yes; }

	    matches++;
	    goto loop;

	case 'v':
	    vp = va_arg(argp, void **);
	    n = g95_match_variable((g95_expr **) vp, 0, 0, 0);
	    if (n != MATCH_YES) { m = n; goto not_yes; }

	    matches++;
	    goto loop;

	case 's':
	    vp = va_arg(argp, void **);
	    n = g95_match_symbol((g95_symbol **) vp, 0);
	    if (n != MATCH_YES) { m = n; goto not_yes; }

	    matches++;
	    goto loop;

	case 'n':
	    np = va_arg(argp, char *);
	    n = g95_match_name(np);
	    if (n != MATCH_YES) { m = n; goto not_yes; }

	    matches++;
	    goto loop;

	case 'l':
	    label = va_arg(argp, g95_st_label **);
	    n = g95_match_st_label(label, 0);
	    if (n != MATCH_YES) { m = n; goto not_yes; }

	    matches++;
	    goto loop;

	case 'o':
	    ip = va_arg(argp, int *);
	    n = g95_match_intrinsic_op((g95_intrinsic_op *) ip);
	    if (n != MATCH_YES) { m = n; goto not_yes; }

	    matches++;
	    goto loop;

	case 't':
	    if (g95_match_eos() != MATCH_YES) { m = MATCH_NO; goto not_yes; }
	    goto loop;

	case ' ':
	    if (g95_match_space() == MATCH_YES) goto loop;
	    m = MATCH_NO;
	    goto not_yes;

	case '%':
	    goto match_char;

	default:
	    g95_internal_error("g95_match(): Bad match code %c", c);
	}

    match_char:
    default:
	if (c == g95_next_char()) goto loop;
	break;
    }

not_yes:
    va_end(argp);

    if (m != MATCH_YES) {   /* Clean up after a failed match */
	g95_current_locus = old_loc;
	va_start(argp, target);

	p = target;
	for(; matches>0; matches--) {
	    while(*p++ != '%');

	    switch(*p++) {
	    case '%': matches++; break;   /* Skip */

	    case 'o': case 'l':	/* Matches that don't have to be undone */
	    case 'n': case 's':
		vp = va_arg(argp, void **);
		break;

	    case 'e': case 'E': case 'v':
		vp = va_arg(argp, void **);
		g95_free_expr(*vp);
		*vp = NULL;
		break;
	    }
	}

	va_end(argp);
    }

    return m;
}



/* g95_match_iterator()-- Match a loop control phrase:
 *
 *     <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]
 *
 * If the final integer expression is not present, a constant unity
 * expression is returned.  We don't return MATCH_ERROR until after
 * the equals sign is seen. */

match g95_match_iterator(g95_iterator *iter) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_expr *var, *e1, *e2, *e3;
g95_locus start;
match m;

    /* Match the start of an iterator without affecting the symbol table */

    start = g95_current_locus;    
    m = g95_match(" %n =", name);
    g95_current_locus = start;

    if (m != MATCH_YES)
	return MATCH_NO;

    /* Shockingly, an iterator variable can be a PARAMETER if the
     * iterator is part of an array constructor. */

    m = g95_match_variable(&var, 0, 0, 1);
    if (m != MATCH_YES)
	return MATCH_NO;

    g95_match_char('=');

    e1 = e2 = e3 = NULL;

    m = g95_match_expr(&e1);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_char(',') != MATCH_YES)
	goto syntax;

    m = g95_match_expr(&e2);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_char(',') != MATCH_YES) {
	e3 = g95_int_expr(1);
	goto done;
    }

    m = g95_match_expr(&e3);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) {
	g95_error("Expected a step value in iterator at %C");
	goto cleanup;
    }

done:
    iter->var = var;
    iter->start = e1;
    iter->end = e2;
    iter->step = e3;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in iterator at %C");
   
cleanup:
    g95_free_expr(e1);
    g95_free_expr(e2);
    g95_free_expr(e3);
    
    return MATCH_ERROR;
}



/* g95_match_program()-- Matches the start of a program unit, which is
 * the program keyword followed by an optional symbol. */

match g95_match_program(void) {
g95_symbol *sym;
match m;

    m = g95_match("% %s%t", &sym);
    if (m == MATCH_NO) {
	g95_error("Invalid form of PROGRAM statement at %C");
	m = MATCH_ERROR;
    }

    if (m == MATCH_ERROR)
	return m;

    if (g95_add_flavor(&sym->attr, FL_PROGRAM, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    g95_new_block = sym;
    g95_current_ns->unit_name = sym->name;

    return MATCH_YES;
}



/* g95_match_assignment(void)-- Match a simple assignment statement */

match g95_match_assignment(void) {
g95_expr *lvalue, *rvalue;
g95_locus start, where;
match m;

    start = g95_current_locus; 

    lvalue = rvalue = NULL;

    m = g95_match_variable(&lvalue, 0, 0, 0);
    if (m != MATCH_YES)
	goto cleanup;

    g95_gobble_whitespace();
    where = g95_current_locus;

    m = g95_match_char('=');
    if (m != MATCH_YES)
	goto cleanup;

    if (lvalue->symbol->attr.flavor == FL_PARAMETER) {
	g95_error("Cannot assign to a PARAMETER variable at %C");
	m = MATCH_ERROR;
	goto cleanup;
    }

    m = g95_match(" %e%t", &rvalue);
    if (m != MATCH_YES)
	goto cleanup;

    new_st.type  = EXEC_ASSIGN;
    new_st.expr  = lvalue;
    new_st.expr2 = rvalue;
    new_st.where = where;

    g95_check_do_variable(lvalue->symbol, &lvalue->where);

    return MATCH_YES;

cleanup:
    g95_current_locus = start;
    g95_free_expr(lvalue);
    g95_free_expr(rvalue);
    return m;
}



/* match_procedure_target()-- Match a procedure target of a pointer
 * assignment.  This has to be a name that is a procedure, a function
 * returning a procedure pointer, or a procedure component reference. */

static match match_procedure_target(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_locus save;
g95_expr *e;
match m;

    save = g95_current_locus; 

    m = g95_match_name(name);
    if (m == MATCH_ERROR)
	return m;

    if (m == MATCH_NO) {
	g95_error("Syntax error in pointer assignment statement at %C");
	return MATCH_ERROR;
    }

    if (g95_match_eos() == MATCH_NO) {
	/* Function call or procedure component reference */
	g95_current_locus = save;
	return g95_match(" %e%t", result);
    }

    /* Name by itself is a procedure target */

    if (g95_get_ha_symbol(name, &sym))
	return MATCH_ERROR;

    if (sym->attr.flavor == FL_VARIABLE) {
	if (sym->ts.type != BT_PROCEDURE) {
	    g95_error("Procedure pointer assignment at %C "
		      "must assign a procedure pointer");
	    return MATCH_ERROR;
	}

    } else if (sym->attr.flavor != FL_PROCEDURE &&
	       g95_add_procedure(&sym->attr, PROC_EXTERNAL,
				 name, &g95_def_locus) == FAILURE)
	return MATCH_ERROR;

    e = g95_get_expr();

    e->type    = EXPR_PROCEDURE;
    e->ts.type = BT_PROCEDURE;
    e->symbol  = sym;
    e->where   = g95_current_locus;

    *result = e;
    return MATCH_YES;
}



/* g95_match_pointer_assignment()-- Match a pointer assignment statement */

match g95_match_pointer_assignment(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_locus old_loc, w, where;
g95_expr *lvalue, *rvalue;
g95_symbol *sym;
match m;

    old_loc = g95_current_locus;
    lvalue = rvalue = NULL;

    m = g95_match_variable(&lvalue, 0, 0, 0);
    if (m != MATCH_YES) {
	m = MATCH_NO;
	goto cleanup;
    }

    g95_gobble_whitespace();
    where = g95_current_locus;

    m = g95_match("=>");
    if (m != MATCH_YES) {
	m = MATCH_NO;
	goto cleanup;
    }

    sym = lvalue->symbol;

    if (sym->attr.flavor == FL_PARAMETER) {
	g95_error("Cannot assign to a PARAMETER variable at %C");
	m = MATCH_ERROR;
	goto cleanup;
    }

    w = g95_current_locus;
    m = g95_match_name(name);
    if (m == MATCH_ERROR)
	goto cleanup;

    if (m == MATCH_NO) {
	g95_error("Missing pointer target at %C");
	m = MATCH_ERROR;
	goto cleanup;
    }

    g95_current_locus = w;

    m = (g95_pointer_expr(lvalue) && lvalue->ts.type == BT_PROCEDURE)
	? match_procedure_target(&rvalue)
	: g95_match(" %e%t", &rvalue);

    if (m != MATCH_YES)
	goto cleanup;

    new_st.type  = EXEC_POINTER_ASSIGN;
    new_st.expr  = lvalue;
    new_st.expr2 = rvalue;
    new_st.where = where;

    return MATCH_YES;

cleanup:
    g95_current_locus = old_loc;
    g95_free_expr(lvalue);
    g95_free_expr(rvalue);
    return m;
}



/* g95_free_forall_iterator()-- Free a list of FORALL iterators */

void g95_free_forall_iterator(g95_forall_iterator *iter) {
g95_forall_iterator *next;

    while(iter) {
	next = iter->next;

	g95_free_expr(iter->var);
	g95_free_expr(iter->start);
	g95_free_expr(iter->end);
	g95_free_expr(iter->stride);

	g95_free(iter);
	iter = next;
    }
}



/* match_forall_iterator()-- Match an iterator as part of a FORALL
 * statement.  The format is:
 *     <var> = <start>:<end>[:<stride>][, <scalar mask>]  */

static match match_forall_iterator(g95_forall_iterator **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_forall_iterator *iter;
g95_locus where, iter_pos;
g95_symbol *sym;
match m;

    where = g95_current_locus; 
    iter = g95_getmem(sizeof(g95_forall_iterator));

    m = g95_match_name(name);
    iter_pos = g95_current_locus;
    if (m != MATCH_YES)
	goto cleanup;

    if (g95_match_char('=') != MATCH_YES) {
	m = MATCH_NO;
	goto cleanup;
    }

    m = g95_match_expr(&iter->start);
    if (m != MATCH_YES)
	goto cleanup;

    if (g95_get_symbol(name, NULL, &sym)) {
	m = MATCH_ERROR;
	goto cleanup;
    }

    iter->var = g95_get_variable_expr(sym, &iter_pos);

    if (g95_match_char(':') != MATCH_YES)
	goto syntax;

    m = g95_match_expr(&iter->end);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_char(':') == MATCH_NO)
	iter->stride = g95_int_expr(1);

    else {
	m = g95_match_expr(&iter->stride);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;
    }

    *result = iter;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in FORALL iterator at %C");
    m = MATCH_ERROR;

cleanup:
    g95_current_locus = where;
    g95_free_forall_iterator(iter);
    return m;
}



/* duplicate_iterator()-- Make sure an iterator variable isn't
 * duplicated.  Returns nonzero if there is a problem. */

static int duplicate_iterator(g95_forall_iterator *iter, g95_expr *var) {
g95_forall_iterator *p;

    for(p=iter; p; p=p->next)
	if (p->var->symbol == var->symbol) {
	    g95_error("Duplicate FORALL iterator '%s' at %L and %L",
		      p->var->symbol->name, &var->where, &p->var->where);
	    return 1;
	}

    return 0;
}



/* match_forall_header()-- Match the header of a FORALL statement. */

static match match_forall_header(g95_forall_iterator **header,
				 g95_expr **mask_p) {
g95_forall_iterator *head, *tail, *new;
g95_state_data *s;
g95_expr *mask;
match m;

    g95_gobble_whitespace();

    head = tail = NULL;
    mask = NULL;

    if (g95_match_char('(') != MATCH_YES)
	return MATCH_NO;

    m = match_forall_iterator(&new);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    head = tail = new;

    for(s=g95_state_stack; s; s=s->previous)
	if (s->state == COMP_FORALL &&
	    duplicate_iterator(s->ext.iter, new->var))
	    goto cleanup;

    for(;;) {
	if (g95_match_char(',') != MATCH_YES)
	    break;

	m = match_forall_iterator(&new);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_YES) {
	    if (duplicate_iterator(head, new->var))
		goto cleanup;

	    for(s=g95_state_stack; s; s=s->previous)
		if (s->state == COMP_FORALL &&
		    duplicate_iterator(s->ext.iter, new->var))
		    goto cleanup;

	    tail->next = new;
	    tail = new;
	    continue;
	}

/* Have to have a mask expression */

	m = g95_match_expr(&mask);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	break;
    }

    if (g95_match_char(')') == MATCH_NO)
	goto syntax;

    *header = head;
    *mask_p = mask;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_FORALL);

cleanup:
    g95_free_expr(mask);
    g95_free_forall_iterator(head);

    return MATCH_ERROR;
}



/* g95_match_forall()-- Match a FORALL statement */

match g95_match_forall(g95_statement *st) {
g95_forall_iterator *head;
g95_expr *mask;
g95_code *c;
match m0, m;

    head = NULL; 
    mask = NULL;
    c = NULL;

    m0 = g95_match_label(); 
    if (m0 == MATCH_ERROR)
	return MATCH_ERROR;

    m = g95_match(" forall");
    if (m != MATCH_YES)
	return m;

    m = match_forall_header(&head, &mask);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    if (g95_match_eos() == MATCH_YES) {
	if (G95_STRICT_F() && m0 == MATCH_YES) {
	    g95_error("Statement label not allowed in FORALL at %C in F mode");
	    m = MATCH_ERROR;
	    goto cleanup;
	}

	*st = ST_FORALL_BLOCK;

	new_st.type = EXEC_FORALL;
	new_st.expr = mask;
	new_st.ext.forall_iterator = head;

	return MATCH_YES;
    }

    m = g95_match_assignment();
    if (m == MATCH_ERROR)
	goto cleanup;

    if (m == MATCH_NO) {
	m = g95_match_pointer_assignment();
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

    if (G95_STRICT_F() && m0 == MATCH_YES) {
	g95_error("Statement label not allowed in FORALL at %C in F mode");
	m = MATCH_ERROR;
	goto cleanup;
    }

    c = g95_get_code(-1, NULL);
    *c = new_st;
    c->where = g95_current_locus;

    g95_clear_new_st();
    new_st.type = EXEC_FORALL;
    new_st.expr = mask;
    new_st.ext.forall_iterator = head;
    new_st.block = c;

    *st = ST_FORALL;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_FORALL);

cleanup:
    g95_free_forall_iterator(head);
    g95_free_expr(mask);
    g95_free_statements(c);

    return m;
}



/* check_aif_obsolescent()-- Check for obsolescence in an arithmetic IF
 * statement. */

static void check_aif_obsolescent(void) {

    if (g95_option.obsolescent)
	g95_warning(124, "Arithmetic IF statement at %C is obsolescent");
}



/* match_arithmetic_if()-- Match an arithmetic IF statement that
 * happens to follow a simple IF statement. */

static match match_arithmetic_if(void) {
g95_st_label *l1, *l2, *l3;
g95_expr *e;
match m;

    m = g95_match(" ( %e ) %l , %l , %l %t", &e, &l1, &l2, &l3);
    if (m != MATCH_YES)
	return m;

    if (g95_reference_st_label(l1, ST_LABEL_TARGET) == FAILURE ||
      g95_reference_st_label(l2, ST_LABEL_TARGET) == FAILURE ||
	g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    new_st.type = EXEC_ARITHMETIC_IF;
    new_st.expr = e;
    new_st.label  = l1;
    new_st.label2 = l2;
    new_st.label3 = l3;

    check_aif_obsolescent();
    return MATCH_YES;
}



/* match_simple_where()-- Match the rest of a simple WHERE statement
 * that follows an IF statement. */

static match match_simple_where(void) {
g95_expr *expr;
g95_code *c;
match m;

    m = g95_match(" ( %e )", &expr);
    if (m != MATCH_YES)
	return m;

    m = g95_match_assignment();
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_eos() != MATCH_YES)
	goto syntax;

    c = g95_get_code(EXEC_WHERE, NULL);
    c->expr = expr;
    c->next = g95_get_code(-1, NULL);

    *c->next = new_st;
    g95_clear_new_st();

    new_st.type = EXEC_WHERE;
    new_st.block = c;

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_WHERE);

cleanup:
    g95_free_expr(expr);
    return MATCH_ERROR;
}



/* match_simple_forall()-- Match the rest of a simple FORALL statement
 * that follows an IF statement. */

static match match_simple_forall(void) {
g95_forall_iterator *head;
g95_expr *mask;
g95_code *c;
match m;

    mask = NULL;
    head = NULL;
    c = NULL;

    m = match_forall_header(&head, &mask);

    if (m == MATCH_NO) goto syntax;
    if (m != MATCH_YES) goto cleanup;

    m = g95_match_assignment();

    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) {
	m = g95_match_pointer_assignment();
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

    /* EOS is implicitly matched by the assignment matchers. */

    c = g95_get_code(-1, NULL);
    *c = new_st;
    c->where = g95_current_locus;

    g95_clear_new_st();
    new_st.type = EXEC_FORALL;
    new_st.expr = mask;
    new_st.ext.forall_iterator = head;
    new_st.block = c;

    return MATCH_YES;
  
syntax:
    g95_syntax_error(ST_FORALL);

cleanup:
    g95_free_forall_iterator(head);
    g95_free_expr(mask);
    g95_free_statements(c);

    return MATCH_ERROR;
}



/* g95_match_if()-- The IF statement is a bit of a pain.  First of
 * all, there are three forms of it, the simple IF, the IF that starts
 * a block and the arithmetic IF.
 *
 * There is a problem with the simple IF and that is the fact that we
 * only have a single level of undo information on symbols.  What this
 * means is for a simple IF, we must re-match the whole IF statement
 * multiple times in order to guarantee that the symbol table ends up
 * in the proper state. */

match g95_match_if(g95_statement *if_type) {
g95_st_label *l1, *l2, *l3;
g95_locus where, clause;
match mm, m, n;
g95_expr *expr;
g95_code *p;

    n = g95_match_label();
    if (n == MATCH_ERROR)
	return n;

    g95_gobble_whitespace();

    where = g95_current_locus;

    m = g95_match("if ( %e", &expr);
    if (m != MATCH_YES)
	return m;

    if (g95_match_char(')') != MATCH_YES) {
	g95_error("Syntax error in IF-expression at %C");
	g95_free_expr(expr);
	return MATCH_ERROR;
    }

    m = g95_match(" %l , %l , %l%t", &l1, &l2, &l3);

    if (m == MATCH_YES) {
	if (n == MATCH_YES) {
	    g95_error("Block label not appropriate for arithmetic IF "
		      "statement at %C");

	    g95_free_expr(expr);
	    return MATCH_ERROR;
	}

	if (g95_reference_st_label(l1, ST_LABEL_TARGET) == FAILURE ||
	    g95_reference_st_label(l2, ST_LABEL_TARGET) == FAILURE ||
	    g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {
	    g95_free_expr(expr);
	    return MATCH_ERROR;
	}

	new_st.type = EXEC_ARITHMETIC_IF;
	new_st.expr = expr;
	new_st.label  = l1;
	new_st.label2 = l2;
	new_st.label3 = l3;

	*if_type = ST_ARITHMETIC_IF;

	if (G95_STRICT_F()) {
	    g95_error("Arithmetic IF at %C not allowed in F");
	    return MATCH_ERROR;
	}

	check_aif_obsolescent();
	return MATCH_YES;
    }

    if (g95_match(" then %t") == MATCH_YES) {
	new_st.type = EXEC_IF;
	new_st.expr = expr;

	*if_type = ST_IF_BLOCK;
	return MATCH_YES;
    }

    if (n == MATCH_YES) {
	g95_error("Block label is not appropriate for the IF statement at %C");

	g95_free_expr(expr);
	return MATCH_ERROR;
    }

/* At this point the only thing left is a simple IF statement.  At
 * this point, n has to be MATCH_NO, so we don't have to worry about
 * re-matching a block label.  From what we've got so far, try
 * matching an assignment. */

    *if_type = ST_SIMPLE_IF;
    clause = g95_current_locus;

    mm = g95_match_assignment();
    if (mm == MATCH_YES)
	goto got_match;

    g95_free_expr(expr);
    g95_undo_symbols();
    g95_current_locus = where;

    g95_match("if ( %e ) ", &expr);  /* Guaranteed to match */
    clause = g95_current_locus;

    mm = g95_match_pointer_assignment();
    if (mm == MATCH_YES)
	goto got_match;

    g95_free_expr(expr);
    g95_undo_symbols();
    g95_current_locus = where;

    g95_match("if ( %e ) ", &expr);  /* Guaranteed to match */

/* Look at the next word to see which matcher to call.  Matching the
 * keyword doesn't affect the symbol table, so we don't have to
 * restore between tries. */

    g95_gobble_whitespace();
    g95_statement_locus = g95_current_locus;

#define match(string, subr) \
  if (g95_match(string) == MATCH_YES) { mm = subr(); goto got_match; }

    g95_clear_error();

    match("all stop",     g95_match_error_stop)
    match("allocate",     g95_match_allocate)
    match("assign",       g95_match_assign)
    match("backspace",    g95_match_backspace)
    match("call",         g95_match_call)
    match("close",        g95_match_close)
    match("continue",     g95_match_continue)
    match("cycle",        g95_match_cycle)
    match("deallocate",   g95_match_deallocate)
    match("end file",     g95_match_endfile)
    match("error stop",   g95_match_error_stop)
    match("exit",         g95_match_exit)
    match("flush",        g95_match_flush)
    match("forall",       match_simple_forall)
    match("go to",        g95_match_goto)
    match("if",           match_arithmetic_if)
    match("inquire",      g95_match_inquire)
    match("notify",       g95_match_notify)
    match("nullify",      g95_match_nullify)
    match("open",         g95_match_open)
    match("pause",        g95_match_pause)
    match("print",        g95_match_print)
    match("query",        g95_match_query)
    match("read",         g95_match_read)
    match("return",       g95_match_return)
    match("rewind",       g95_match_rewind)
    match("stop",         g95_match_stop)
    match("where",        match_simple_where)
    match("write",        g95_match_write)
    match("sync all",     g95_match_sync_all)
    match("sync memory",  g95_match_sync_memory)
    match("sync images",  g95_match_sync_images)
    match("sync team",    g95_match_sync_team)

/* All else has failed, so give up.  See if any of the matchers has
 * stored an error message of some sort. */

   if (g95_error_check() == 0)
       g95_error("Unclassifiable statement following IF-clause at %C");

    g95_free_expr(expr);
    return MATCH_ERROR;

got_match:
    if (mm == MATCH_NO)
	g95_error("Syntax error in IF-clause at %C");

    if (mm != MATCH_YES) {
	g95_free_expr(expr);
	return MATCH_ERROR;
    }

    if (G95_STRICT_F()) {
	g95_error("Single-line IF statement at %C not permitted in F");
	return MATCH_ERROR;
    }

/* At this point, we've matched the single IF and the action clause is
 * in new_st.  Rearrange things so that the IF statement appears in new_st */

    p = g95_get_code(-1, NULL);
    *p = new_st;

    if (p->where.lb == NULL)
	p->where = clause;

    g95_clear_new_st();

    new_st.type  = EXEC_IF;
    new_st.block = p;
    new_st.expr  = expr;
    new_st.where = where;

    return MATCH_YES;
}

#undef match



/* g95_match_elseif()-- Match an ELSE IF statement */

match g95_match_elseif(void) {
char *block_name, name[G95_MAX_SYMBOL_LEN+1];
g95_expr *expr;
match m;

    m = g95_match(" ( %e ) then", &expr);
    if (m != MATCH_YES)
	return m;

    if (g95_match_eos() == MATCH_YES)
	goto done;

    if (g95_match_name(name) != MATCH_YES || g95_current_block() == NULL ||
	g95_match_eos() != MATCH_YES) {
	g95_error("Unexpected junk after ELSE IF statement at %C");
	goto cleanup;
    }

    if (G95_STRICT_F()) {
	g95_error("Construct name at %C in ELSE IF statement not permitted "
		  "in F");
	goto cleanup;
    }

    block_name = g95_current_block_name();

    if (strcmp(name, block_name) != 0) {
	g95_error("Label '%s' at %C doesn't match IF label '%s'", name,
		  block_name);
	goto cleanup;
    }

done:
    new_st.type = EXEC_IF;
    new_st.expr = expr;
    return MATCH_YES;

cleanup:
    g95_free_expr(expr);
    return MATCH_ERROR;
}



/* g95_match_else()-- Match an ELSE statement */

match g95_match_else(void) {
char *block_name, name[G95_MAX_SYMBOL_LEN+1];
 
    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    if (g95_match_name(name) != MATCH_YES || g95_current_block() == NULL ||
	g95_match_eos() != MATCH_YES) {
	g95_error("Unexpected junk after ELSE statement at %C");
	return MATCH_ERROR;
    }

    block_name = g95_current_block_name();
    if (strcmp(name, block_name) != 0) {
	g95_error("Label '%s' at %C doesn't match IF label '%s'", name,
		  block_name);
	return MATCH_ERROR;
    }

    return MATCH_YES;
}



/* g95_match_do()-- Match a DO statement */

match g95_match_do(void) {
g95_expr *while_condition;
g95_locus old_loc, where;
g95_iterator iter, *ip;
g95_st_label *label;
match m;

    old_loc = g95_current_locus;

    label = NULL;
    iter.var = iter.start = iter.end = iter.step = NULL;
    while_condition = NULL;

    m = g95_match_label();
    if (m == MATCH_ERROR)
	return m;

    if (g95_match(" do") != MATCH_YES)
	return MATCH_NO;

/* Match an infinite DO, make it like a DO WHILE(.TRUE.) */

    if (g95_match_eos() == MATCH_YES) {
	new_st.type = EXEC_DO_WHILE;
	goto done;
    }

    m = g95_match_st_label(&label, 0);
    if (m == MATCH_ERROR)
	goto cleanup;

    if (g95_match_eos() == MATCH_YES) {
	new_st.type = EXEC_DO_WHILE;
	goto done;
    }

    where = g95_current_locus;

    m = g95_match_char(',');
    if (m != MATCH_YES && g95_match_space() != MATCH_YES)
	return MATCH_NO;

    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_current_locus.nextc--;
	g95_error("Comma at %C in DO statement not allowed in F mode");
	g95_current_locus.nextc++;
	return MATCH_ERROR;
    }

/* See if we have a DO WHILE */

    if (!G95_STRICT_F() &&
	g95_match(" while ( %e )%t", &while_condition) == MATCH_YES) {
	new_st.type = EXEC_DO_WHILE;
	goto done;
    }

/* The abortive DO WHILE may have done something to the symbol table,
 * so we start over: */

    g95_undo_symbols();
    g95_current_locus = old_loc;

    g95_match_label();    /* This won't error */
    g95_match(" do ");    /* This will work */

    g95_match_st_label(&label, 0);  /* Can't error out */
    g95_match_char(',');            /* Optional comma */

    m = g95_match_iterator(&iter);
    if (m == MATCH_NO) return MATCH_NO;
    if (m == MATCH_ERROR) goto cleanup;

    g95_check_do_variable(iter.var->symbol, &iter.var->where);

    if (g95_match_eos() != MATCH_YES) {
	g95_syntax_error(ST_DO);
	goto cleanup;
    }

    new_st.type = EXEC_DO;

    if (G95_STRICT_F() && label != NULL) {
	g95_error("Statement label on DO statement at %C not permitted in F");
	goto cleanup;
    }

done:
    if (label != NULL &&
	g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)
	goto cleanup;

    new_st.label = label;

    if (new_st.type == EXEC_DO_WHILE)
	new_st.expr = while_condition;

    else {
	new_st.ext.iterator = ip = g95_get_iterator();
	*ip = iter;
    }

    return MATCH_YES;

cleanup:
    g95_free_iterator(&iter, 0);
    return MATCH_ERROR;
}



/* g95_free_iterator()-- Free a g95_iterator structure */

void g95_free_iterator(g95_iterator *iter, int flag) {

    if (iter == NULL)
	return;

    g95_free_expr(iter->var);
    g95_free_expr(iter->start);
    g95_free_expr(iter->end);
    g95_free_expr(iter->step);

    if (flag)
	g95_free(iter);
}



/* match_exit_cycle()-- Match an EXIT or CYCLE statement */

static match match_exit_cycle(g95_statement st, g95_exec_op op) {
g95_state_data *p;
g95_symbol *sym;
g95_annot *a;
match m;

    if (g95_match_eos() == MATCH_YES)
	sym = NULL;

    else {
	m = g95_match("% %s%t", &sym);
	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	if (m == MATCH_NO) {
	    g95_syntax_error(st);
	    return MATCH_ERROR;
	}

	if (sym->attr.flavor != FL_LABEL) {
	    g95_error("Name '%s' in %s statement at %C is not a loop name",
		      sym->name, g95_ascii_statement(st));
	    return MATCH_ERROR;
	}
    }

/* Find the loop mentioned specified by the label (or lack of a label) */

    for(p=g95_state_stack; p; p=p->previous)
	if (p->state == COMP_DO && (sym == NULL || sym == p->sym))
	    break;

    if (p == NULL) {
	if (sym == NULL)
	    g95_error("%s statement at %C is not within a loop",
		      g95_ascii_statement(st));
	else
	    g95_error("%s statement at %C is not within loop '%s'",
		      g95_ascii_statement(st), sym->name);

	return MATCH_ERROR;
    }

    a = g95_annotate(ANNOT_LABEL, &g95_def_locus);
    a->u.sym = sym;

    /* Save the first statement in the loop - needed by the backend */

    new_st.ext.block = p->top;
    new_st.type = op;
    new_st.sym = sym;

    return MATCH_YES;
}



/* g95_match_exit()-- Match the EXIT statement */

match g95_match_exit(void) {

    return match_exit_cycle(ST_EXIT,  EXEC_EXIT);
}



/* g95_match_cycle()-- Match the CYCLE statement */

match g95_match_cycle(void) {

    return match_exit_cycle(ST_CYCLE, EXEC_CYCLE);
}



/* g95_match_import()-- Match an IMPORT statement */

match g95_match_import(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_state_data *p;
g95_symtree *st;
g95_symbol *sym;

    if (g95_current_state() != COMP_SUBROUTINE &&
	g95_current_state() != COMP_FUNCTION)
	return MATCH_NO;

    p = g95_state_stack->previous;
    if (p == NULL || p->state != COMP_INTERFACE)
	return MATCH_NO;

    /* We're in an interface body */

    if (g95_match_eos() == MATCH_YES) {
	g95_current_ns->import = 1;
	return MATCH_YES;
    }

    g95_match(" :: ");

    for(;;) {
	switch(g95_match_name(name)) {
	case MATCH_YES:
	    break;

	case MATCH_NO:
	    goto syntax;

	case MATCH_ERROR:
	    return MATCH_ERROR;
	}

	if (g95_find_symbol(name, g95_current_ns, 1, &sym))
	    return MATCH_ERROR;

	if (sym == NULL) {
	    g95_error("'%s' name in IMPORT statement does not exist in a "
		      "host program unit", name);
	    return MATCH_ERROR;
	}

	st = g95_find_symtree(g95_current_ns->sym_root, name);
	if (st != NULL) {
	    g95_error("'%s' name in IMPORT statement at %C is already defined",
		      name);
	    return MATCH_ERROR;
	}

	st = g95_new_symtree(&g95_current_ns->sym_root, name);
	st->n.sym = sym;
	sym->refs++;

	if (g95_match_eos() == MATCH_YES)
	    break;

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_IMPORT);
    return MATCH_ERROR;
}



/* match_stop_code()-- Match a stop code */

static match match_stop_code(g95_statement st, g95_expr **result, int *code) {
g95_expr *e, *f;
g95_locus where;
match m;

    *result = NULL;
    *code = -1;
    e = NULL;
    where = g95_current_locus;

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    m = g95_match_small_literal_int(code, 1);
    switch(m) {
    case MATCH_ERROR:
	return MATCH_ERROR;

    case MATCH_YES:
	if (*code > 99999) {
	    g95_error("stop-code out of range at %C");
	    return MATCH_ERROR;
	}

	if (g95_match_eos() != MATCH_YES) {
	    g95_current_locus = where;
	    *code = -1;
	    break;
	}

	return MATCH_YES;

    case MATCH_NO:
	g95_current_locus = where;
	break;
    }

    m = g95_match_expr(&e);
    switch(m) {
    case MATCH_ERROR:
	return MATCH_ERROR;

    case MATCH_NO:
	goto syntax;

    case MATCH_YES:
	break;
    }

    if (g95_match_eos() != MATCH_YES || e->ts.type != BT_CHARACTER)
	goto syntax;

    if (e->type == EXPR_VARIABLE && e->symbol->attr.flavor == FL_PARAMETER) {
	g95_set_usage(e->symbol, &where, 0, 1);

	f = g95_copy_expr(e->symbol->value);
	g95_free_expr(e);
	e = f;
    }

    if (e->type != EXPR_CONSTANT)
	goto syntax;

    *result = e;
    return MATCH_YES;

syntax:
    g95_syntax_error(st);
    g95_free_expr(e);
    return MATCH_ERROR;
}



/* g95_match_goto()-- Match the GO TO statement.  As a computed GOTO
 * statement is matched, it is transformed into an equivalent SELECT
 * block.  No tree is necessary, and the resulting jumps-to-jumps are
 * specifically optimized away by the back end. */

match g95_match_goto(void) {
g95_code *head, *tail;
g95_st_label *label;
g95_symbol *sym;
g95_expr *expr;
g95_case *cp;
int vanilla, c, i;
match m;

    head = tail = NULL;
    vanilla = 0;
    label = NULL;
    expr = NULL;

    if (g95_match(" %l%t", &label) == MATCH_YES) {
	if (g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)
	    return MATCH_ERROR;

	new_st.type = EXEC_GOTO;
	new_st.label = label;
	vanilla = 1;
	goto success;
    }

    if (g95_match_symbol(&sym, 1) == MATCH_YES) {
	if (sym->attr.flavor != FL_VARIABLE &&
	    sym->attr.flavor != FL_UNKNOWN) {
	    g95_error("Expected a variable in assigned GOTO at %L",
		      &g95_def_locus);
	    goto cleanup;
	}

	if (g95_match_eos() != MATCH_YES) {
	    g95_gobble_whitespace();

	    c = g95_next_char();
	    if (c == ',') {
		g95_gobble_whitespace();
		c = g95_next_char();
	    }

	    if (c != '(')
		goto syntax;

	    for(;;) {
		m = g95_match(" %l", &label);
		if (m == MATCH_ERROR)
		    return MATCH_ERROR;

		if (m == MATCH_NO)
		    goto syntax;

		if (g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)
		    return MATCH_ERROR;

		g95_gobble_whitespace();
		c = g95_next_char();

		if (c == ')')
		    break;

		if (c != ',')
		    goto syntax;
	    }

	    if (g95_match_eos() != MATCH_YES)
		goto syntax;
	}

	if (g95_option.fmode != 0)
	    g95_warning(132, "The assigned GO TO statement at %C is no "
			"longer legal fortran");

	new_st.type = EXEC_GOTO;
	new_st.expr = g95_get_variable_expr(sym, &g95_current_locus);

	goto success;
    }

/* Last chance is a computed GO TO statement */

    if (g95_match_char('(') != MATCH_YES) {
	g95_syntax_error(ST_GOTO);
	return MATCH_ERROR;
    }

    i = 1;

    do {
	m = g95_match_st_label(&label, 0);
	if (m != MATCH_YES)
	    goto syntax;

	if (g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)
	    goto cleanup;

	if (head == NULL)
	    head = tail = g95_get_code(EXEC_SELECT, NULL);

	else {
	    tail->block = g95_get_code(EXEC_SELECT, NULL);
	    tail = tail->block;
	}

	cp = g95_get_case();
	cp->low = cp->high = g95_int_expr(i++);

	tail->ext.case_list = cp;

	tail->next = g95_get_code(EXEC_GOTO, NULL);
	tail->next->label = label;
    } while(g95_match_char(',') == MATCH_YES);

    if (g95_match_char(')') != MATCH_YES)
	goto syntax;

    if (head == NULL) {
	g95_error("Statement label list in GOTO at %C cannot be empty");
	goto syntax;
    }

/* Get the rest of the statement */

    g95_match_char(',');

    if (g95_match(" %e%t", &expr) != MATCH_YES)
	goto syntax;

/* At this point, a computed GOTO has been fully matched and an
 * equivalent SELECT statement constructed. */

    if (g95_option.obsolescent)
	g95_warning(125, "Computed GOTO statement at %C is obsolescent");

    new_st.type = EXEC_SELECT;
    new_st.expr = NULL;
    /* For a "real" SELECT, the expression is in expr. We put it in expr2. */
    new_st.expr2 = expr;
    new_st.block = head;

success:
    if (G95_STRICT_F()) {
	if (!vanilla) {
	    g95_error("Non-simple GOTO statement at %C is not allowed in F");
	    return MATCH_ERROR;
	}

	if (label->defined == ST_LABEL_TARGET) {
	    g95_error("GOTO statement at %C must be to a forward label in F");
	    return MATCH_ERROR;
	}
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_GOTO);

cleanup:
    g95_free_statements(head);
    g95_free_expr(expr);
    return MATCH_ERROR;
}



/* g95_match_pause()-- Match the (deprecated) PAUSE statement */

match g95_match_pause(void) {
int stop_code;
g95_expr *e;

    if (match_stop_code(ST_PAUSE, &e, &stop_code) == MATCH_ERROR)
	return MATCH_ERROR;

    if (g95_pure(NULL, 1)) {
	g95_error("PAUSE statement not allowed in PURE procedure at %C");
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    if (G95_STRICT_F()) {
	g95_error("PAUSE statement at %C is not legal F");
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    if (g95_option.fmode != 0)
	g95_warning(118,"PAUSE statement at %C is no longer legal fortran 95");

    new_st.type = EXEC_PAUSE;
    new_st.expr = e;
    new_st.ext.stop_code = stop_code;

    return MATCH_YES;
}



/* g95_match_stop()-- Match the STOP statement.  We can't match a
 * label here because labels can't be zero and a stop code can. */

match g95_match_stop(void) {
int stop_code;
g95_expr *e;

    if (match_stop_code(ST_STOP, &e, &stop_code) == MATCH_ERROR)
	return MATCH_ERROR;

    if (g95_pure(NULL, 0)) {
	g95_error("STOP statement not allowed in PURE procedure at %C");
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    new_st.type = EXEC_STOP;
    new_st.expr = e;
    new_st.ext.stop_code = stop_code;

    return MATCH_YES;
}



/* g95_match_error_stop()-- Match the ERROR STOP statement. */

match g95_match_error_stop(void) {
int stop_code;
g95_expr *e;

    if (match_stop_code(ST_ERROR_STOP, &e, &stop_code) == MATCH_ERROR)
	return MATCH_ERROR;

    if (g95_pure(NULL, 0)) {
	g95_error("ERROR STOP statement not allowed in PURE procedure at %C");
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    new_st.type = EXEC_ERROR_STOP;
    new_st.expr = e;
    new_st.ext.stop_code = stop_code;

    return MATCH_YES;
}



/* g95_match_enum()-- Match an ENUM statement */

match g95_match_enum(void) {

    enum_bindc = (g95_match(" , bind ( c )") == MATCH_YES);

    if (g95_match_eos() != MATCH_YES) {
	g95_syntax_error(ST_ENUM);
	return MATCH_ERROR;
    }

    if (g95_option.fmode != 0 && g95_option.fmode != 2003) {
	g95_error("ENUM statement at %C not permitted in non F2003 mode");
	return MATCH_ERROR;
    }

    match_value = 0;
    return MATCH_YES;
}



/* g95_match_enumerator()-- Match an ENUMERATOR statement */

match g95_match_enumerator(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_typespec ts;
g95_locus where;
match m;
char c;

    g95_clear_ts(&ts);
    ts.type = BT_INTEGER;
    ts.kind = sizeof(int);   /* Matches the companion cc */

    g95_match(" ::");        /* Gobble double colon */

    for(;;) {
	m = g95_match_name(name);
	if (m == MATCH_NO) {
	    g95_error("Missing name in ENUMERATOR statement at %C");
	    m = MATCH_ERROR;
	}

	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	if (g95_get_symbol(name, NULL, &sym))
	    return MATCH_ERROR;

	if (g95_add_flavor(&sym->attr, FL_PARAMETER,
			   sym->name, NULL) == FAILURE)
	    return MATCH_ERROR;

	if (g95_add_type(sym, &ts, NULL) == FAILURE)
	    return MATCH_ERROR;

	g95_gobble_whitespace();

	c = g95_next_char();

	if (c == '\n') {
	    sym->value = g95_int_expr(match_value++);
	    break;
	}

	if (c == '=') {
	    g95_gobble_whitespace();
	    where = g95_current_locus;

	    m = g95_match_init_expr(&sym->value);
	    switch(m) {
	    case MATCH_NO:
		g95_error("Missing value in ENUMERATOR statement at %L",
			  &where);
		return MATCH_ERROR;

	    case MATCH_ERROR:
		return MATCH_ERROR;

	    default:
		break;
	    }

	    if (sym->value->ts.type != BT_INTEGER) {
		g95_error("Enumerators must have integer values at %L",
			  &where);
		return MATCH_ERROR;
	    }

	    if (sym->value->ts.kind != g95_default_integer_kind(0) &&
		g95_convert_type(sym->value, &ts, 0) == FAILURE)
		return MATCH_ERROR;

	    if (g95_range_check(sym->value)) {
		g95_error("Enumerator value out of range at %L", &where);
		return MATCH_ERROR;
	    }

	    match_value = bi_to_int(sym->value->value.integer) + 1;

	    if (g95_match_eos() == MATCH_YES)
		break;

	    g95_gobble_whitespace();
	    if (g95_next_char() != ',')
		goto syntax;

	} else if (c == ',')
	    sym->value = g95_int_expr(match_value++);

	else
	    goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_ENUMERATOR);
    return MATCH_ERROR;
}



/* g95_match_assign()-- Match the (deprecated) ASSIGN statement. */

match g95_match_assign(void) {
g95_st_label *label;
g95_symbol *sym;
g95_locus where;
match m;

    m = g95_match(" %l to ", &label);
    if (m != MATCH_YES)
	return m;

    where = g95_current_locus;

    m = g95_match("%s%t", &sym);
    if (m == MATCH_YES) {
	if (sym->attr.intent == INTENT_IN) {
	    g95_error("Cannot ASSIGN to INTENT(IN) parameter '%s' at %L",
		      sym->name, &g95_def_locus);
	    return MATCH_ERROR;
	}

	if (g95_option.fmode != 0)
	    g95_warning(119, "ASSIGN statement at %C is no longer legal "
			"fortran 95");

	new_st.type = EXEC_LABEL_ASSIGN;
	new_st.sym = sym;
	new_st.label = label;
	new_st.where = where;

	g95_reference_st_label(label, ST_LABEL_REFFED);
    }

    if (G95_STRICT_F()) {
	g95_error("ASSIGN statement at %C not allowed in F");
	return MATCH_ERROR;
    }

    return m;
}



/* g95_match_continue()-- match a CONTINUE statement */

match g95_match_continue(void) {

    if (g95_match_eos() != MATCH_YES) {
	g95_syntax_error(ST_CONTINUE);
	return MATCH_ERROR;
    }

    new_st.type = EXEC_CONTINUE;
    return MATCH_YES;
}



/* g95_get_alloc()-- Get a g95_alloc structure. */

g95_alloc *g95_get_alloc(void) {

    return g95_getmem(sizeof(g95_alloc));
}



/* g95_free_alloc_list()-- Frees a list of g95_alloc structures */

void g95_free_alloc_list(g95_alloc *p) {
g95_alloc *q;
int i, n;

    n = sizeof(p->upper) / sizeof(p->upper[0]);

    for(; p; p=q) {
	q = p->next;

	g95_free_expr(p->expr);

	for(i=0; i<n; i++) {
	    g95_free_expr(p->lower[i]);
	    g95_free_expr(p->upper[i]);
	}

	g95_free(p);
    }
}



/* g95_match_deallocate()-- Match a DEALLOCATE statement */

match g95_match_deallocate(void) {
g95_alloc *head, *tail, *a;
g95_symbol *sym;
g95_expr *stat;
match m;

    head = tail = NULL;
    stat = NULL;

    if (g95_match_char('(') != MATCH_YES)
	goto syntax;

    for(;;) {
	m = g95_match_dealloc_var(&a);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	if (head == NULL)
	    head = tail = a;

	else {
	    tail->next = a;
	    tail = a;
	}

	if (g95_check_do_variable(tail->expr->symbol, &tail->expr->where))
	    goto cleanup;

	sym = tail->expr->symbol;

	if (g95_pure(NULL, 1) && g95_impure_variable(sym)) {
	    g95_error("Illegal deallocate-expression in DEALLOCATE at %C for "
		      "a PURE procedure");
	    goto cleanup;
	}

	if (sym->attr.intent == INTENT_IN &&
	    (sym->attr.pointer || sym->attr.allocatable)) {
	    g95_error("Cannot DEALLOCATE INTENT(IN) dummy variable '%s' at %L",
		      sym->name, &tail->expr->where);
	    goto cleanup;
	}

	if (g95_match_char(',') != MATCH_YES)
	    break;

	m = g95_match(" stat = %v", &stat);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_YES) break;
    }

    if (stat != NULL) {
	sym = stat->symbol;

	if (sym->attr.intent == INTENT_IN) {
	    g95_error("STAT variable '%s' of DEALLOCATE statement at %C "
		      "cannot be INTENT(IN)", sym->name);
	    goto cleanup;
	}

	if (g95_pure(NULL, 1) && g95_impure_variable(sym)) {
	    g95_error("Illegal STAT variable in DEALLOCATE statement at %C "
		      "for a PURE procedure");
	    goto cleanup;
	}

	if (sym->attr.flavor != FL_VARIABLE &&
	    (g95_current_ns->proc_name != sym || !sym->attr.function ||
	     sym->result != sym)) {
	    g95_error("STAT expression at %C must be a variable");
	    goto cleanup;
	}

	g95_check_do_variable(stat->symbol, &stat->where);
    }

    if (g95_match(" )%t") != MATCH_YES)
	goto syntax;

    new_st.type = EXEC_DEALLOCATE;
    new_st.expr = stat;
    new_st.ext.alloc_list = head;

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_DEALLOCATE);

cleanup:
    g95_free_expr(stat);
    g95_free_alloc_list(head);
    return MATCH_ERROR;
}



/* g95_match_return()-- Match a RETURN statement */

match g95_match_return(void) {
g95_compile_state state;
g95_expr *e;
match m;

    e = NULL;
    if (g95_match_eos() == MATCH_YES)
	goto done;

    if (g95_find_state(COMP_SUBROUTINE) == FAILURE) {
	g95_error("Alternate RETURN statement at %C is only allowed within "
		  "a SUBROUTINE");
	goto cleanup;
    }

    m = g95_match(" %e%t", &e);
    if (m == MATCH_YES) goto done;
    if (m == MATCH_ERROR) goto cleanup;

    g95_syntax_error(ST_RETURN);

cleanup:
    g95_free_expr(e);
    return MATCH_ERROR;

done:
    if (g95_option.fmode == 95) {
	g95_enclosing_unit(&state);

	if (state == COMP_PROGRAM)
	    g95_warning(127, "RETURN statement at %C not allowed in a "
			"PROGRAM unit");
    }

    new_st.type = EXEC_RETURN;
    new_st.expr = e;

    return MATCH_YES;
}



/* g95_match_nullify()-- Match a NULLIFY statement. A NULLIFY
 * statement is transformed into a set of pointer assignments to
 * intrinsic NULL(). */

match g95_match_nullify(void) {
g95_expr *e, *p;
g95_code *tail;
match m;

    tail = NULL;

    if (g95_match_char('(') != MATCH_YES)
	goto syntax;

    for(;;) {
	m = g95_match_variable(&p, 0, 0, 0);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	if (g95_check_do_variable(p->symbol, &p->where))
	    goto cleanup;

	if (g95_pure(NULL, 1) && g95_impure_variable(p->symbol)) {
	    g95_error("Illegal variable in NULLIFY at %C for a "
		      "PURE procedure");
	    goto cleanup;
	}

	if (p->symbol->attr.intent == INTENT_IN) {
	    g95_error("Cannot NULLIFY the INTENT(IN) pointer '%s' at %L",
		      p->symbol->name, &g95_def_locus);
	    goto cleanup;
	}

	/* build ' => NULL() ' */
	e = g95_null_expr(NULL);

	/* Chain to list */
	if (tail == NULL) {
	    tail = &new_st;
	    tail->type = EXEC_POINTER_ASSIGN;

	} else {
	    tail->next = g95_get_code(EXEC_POINTER_ASSIGN, NULL);
	    tail = tail->next;
	}

	tail->expr = p;
	tail->expr2 = e;

	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    if (g95_match_eos() != MATCH_YES)
	goto syntax;

    if (G95_STRICT_F()) {
	g95_error("NULLIFY statement at %C is not legal in F");
	return MATCH_ERROR;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_NULLIFY);

cleanup:
    return MATCH_ERROR;
}



/* g95_match_allocate()-- Match an ALLOCATE statement */

match g95_match_allocate(void) {
g95_alloc *head, *tail, *a;
match m, component_ref;
g95_symbol *sym;
g95_expr *stat;
g95_ref *ref;

    head = tail = NULL;
    stat = NULL;

    if (g95_match_char('(') != MATCH_YES)
	goto syntax;

    for(;;) {
	m = g95_match_alloc_var(&a);

	if (m == MATCH_NO)    goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	if (head == NULL)
	    head = tail = a;

	else {
	    tail->next = a;
	    tail = a;
	}

	sym = tail->expr->symbol;

	if (g95_check_do_variable(sym, &tail->expr->where))
	    goto cleanup;

	if (g95_pure(NULL, 1) && g95_impure_variable(sym)) {
	    g95_error("Bad allocate-object in ALLOCATE statement at %C for a "
		      "PURE procedure");
	    goto cleanup;
	}

	component_ref = 0;
	for(ref=tail->expr->ref; ref; ref=ref->next)
	    component_ref |= (ref->type == REF_COMPONENT);

	if (sym->attr.intent == INTENT_IN && !component_ref &&
	    (sym->attr.pointer || sym->attr.allocatable)) {
	    g95_error("Cannot ALLOCATE INTENT(IN) dummy variable '%s' at %L",
		      sym->name, &tail->expr->where);
	    goto cleanup;
	}

	if (g95_match_char(',') != MATCH_YES)
	    break;

	m = g95_match(" stat = %v", &stat);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_YES)   break;
    }

    if (stat != NULL) {
	sym = stat->symbol;

	if (sym->attr.intent == INTENT_IN) {
	    g95_error("STAT variable '%s' of ALLOCATE statement at %C "
		      "cannot be INTENT(IN)", sym->name);
	    goto cleanup;
	}

	if (g95_pure(NULL, 1) && g95_impure_variable(sym)) {
	    g95_error("Illegal STAT variable in ALLOCATE statement at %C "
		      "for a PURE procedure");
	    goto cleanup;
	}

	if (sym->attr.flavor != FL_VARIABLE &&
	    (g95_current_ns->proc_name != sym || !sym->attr.function ||
	     sym->result != sym)) {
	    g95_error("STAT expression at %C must be a variable");
	    goto cleanup;
	}

	g95_check_do_variable(sym, &stat->where);
    }

    if (g95_match(" )%t") != MATCH_YES)
	goto syntax;

    new_st.type = EXEC_ALLOCATE;
    new_st.expr = stat;
    new_st.ext.alloc_list = head;

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_ALLOCATE);

cleanup:
    g95_free_expr(stat);
    g95_free_alloc_list(head);
    return MATCH_ERROR;
}



/* g95_match_call()-- Match a CALL statement.  The tricky part here
 * are possible alternate return specifiers.  We handle these by
 * having all "subroutines" actually return an integer via a register
 * that gives the return number.  If the call specifies alternate
 * returns, we generate code for a SELECT statement whose case clauses
 * contain GOTOs to the various labels. */

match g95_match_call(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_actual_arglist *a, *arglist;
g95_locus where;
g95_symbol *sym;
match m;
int i;

    arglist = NULL;

    g95_gobble_whitespace();
    where = g95_current_locus; /* Location of a CALL is the subroutine name. */

    m = g95_match_name(name);
    if (m == MATCH_NO)
	goto syntax;

    if (m != MATCH_YES)
	return m;

    /* Host association has to happen later */

    if (g95_get_symbol(name, NULL, &sym))
	return MATCH_ERROR;

    if (sym->attr.flavor == FL_UNKNOWN && g95_peek_char() == '%' &&
	g95_add_flavor(&sym->attr, FL_VARIABLE, name, NULL) == FAILURE)
	return FAILURE;

    if (sym->attr.flavor == FL_PROCEDURE) {
	new_st.ext.sub.name = g95_get_string(name);

	if (sym->ts.type == BT_UNKNOWN)
	    sym->ts.type = BT_PROCEDURE;

    } else if (sym->attr.flavor == FL_VARIABLE) {
	m = g95_match_call_expr(sym, &new_st.ext.sub.pointer);

	if (m == MATCH_NO)
	    goto syntax;

	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

    } else if (sym->attr.flavor == FL_UNKNOWN) {
	if (sym->attr.dummy && sym->attr.flavor != FL_PROCEDURE &&
	    g95_add_flavor(&sym->attr, FL_PROCEDURE, name, NULL) == FAILURE)
	    return FAILURE;

	new_st.ext.sub.name = g95_get_string(name);

    } else {
	g95_error("Bad subroutine '%s' at %C", name);
	return MATCH_ERROR;
    }

    if (g95_match_eos() == MATCH_YES) {
	if (G95_STRICT_F()) {
	    g95_error("CALL statement at %C requires parens in F");
	    return MATCH_ERROR;
	}

    } else {
	m = g95_match_actual_arglist(1, 0, &arglist);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	if (g95_match_eos() != MATCH_YES)
	    goto syntax;
    }

    i = 0;
    for(a=arglist; a; a=a->next)
	if (a->type == ARG_ALT_RETURN && 
	    g95_reference_st_label(a->u.label, ST_LABEL_TARGET) == FAILURE)
	    i = 1;
    
    if (i)
	goto cleanup;

    new_st.type = EXEC_CALL;
    new_st.ext.sub.actual = arglist;

    new_st.where = where;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_CALL);

cleanup:
    g95_free_actual_arglist(arglist);
    return MATCH_ERROR;
}



/* free_variable()-- Free a g95_data_variable structure and everything
 * beneath it */

static void free_variable(g95_data_variable *p) {
g95_data_variable *q;

    for(; p; p=q) {
	q = p->next;
	g95_free_expr(p->expr);
	g95_free_iterator(&p->iter, 0);
	free_variable(p->list);
    
	g95_free(p);
    }
}



/* free_value()-- Free a g95_data_value structure and everything
 * beneath it */

static void free_value(g95_data_value *p) {
g95_data_value *q;

    for(; p; p=q) {
	q = p->next;
	g95_free_expr(p->expr);
	g95_free(p);
    }
}



/* g95_free_data()-- Free a list of g95_data structures */

void g95_free_data(g95_data *p) {
g95_data *q;

    for(; p; p=q) {
	q = p->next;

	free_variable(p->var);
	free_value(p->value);

	g95_free(p);
    }
}



/* var_list()-- Match a list of variables terminated by an iterator
 * and a right paren. */

static match var_list(g95_data_variable *parent) {
g95_data_variable *tail, var;
match m;

    m = var_element(&var);
    if (m == MATCH_ERROR) return MATCH_ERROR;
    if (m == MATCH_NO) goto syntax;

    tail = g95_get_data_variable();
    *tail = var;

    parent->list = tail;

    for(;;) {
	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;

	m = g95_match_iterator(&parent->iter);
	if (m == MATCH_YES) break;
	if (m == MATCH_ERROR) return MATCH_ERROR;

	m = var_element(&var);
	if (m == MATCH_ERROR) return MATCH_ERROR;
	if (m == MATCH_NO) goto syntax;

	tail->next = g95_get_data_variable();
	tail = tail->next;

	*tail = var;
    }

    if (!g95_iterator_variable(parent->iter.var->symbol)) {
	g95_error("Iterator variable '%s' at %L must be of type INTEGER",
		  parent->iter.var->symbol->name, &parent->iter.var->where);
	return MATCH_ERROR;
    }

    if (g95_match_char(')') != MATCH_YES)
	goto syntax;

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_DATA);
    return MATCH_ERROR;
}



/* var_element()-- Match a single element in a data variable list,
 * which can be a variable-iterator list. */

static match var_element(g95_data_variable *new) {
g95_symbol *sym;
match m;

    memset(new, '\0', sizeof(g95_data_variable));

    if (g95_match_char('(') == MATCH_YES)
	return var_list(new);

    m = g95_match_variable(&new->expr, 1, 1, 0);
    if (m != MATCH_YES)
	return m;

    sym = new->expr->symbol;

    if (sym->value != NULL) {
	g95_error("Variable '%s' at %L already has an initialization",
		  sym->name, &new->expr->where);
	return MATCH_ERROR;
    }

    if (g95_add_data(&sym->attr, sym->name, &new->expr->where) == FAILURE)
	return MATCH_ERROR;

    if (sym->ts.type == BT_UNKNOWN &&
	g95_set_default_type(sym, 1, g95_current_ns) == FAILURE)
	return MATCH_ERROR;

    return MATCH_YES;
}



/* top_var_list()-- Match the top-level list of data variables */

static match top_var_list(g95_data *d) {
g95_data_variable var, *tail, *new;
match m;

    tail = NULL;

    for(;;) {
	m = var_element(&var);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) return MATCH_ERROR;

	g95_fixup_data(&var);

	new = g95_get_data_variable();
	*new = var;

	if (tail == NULL)
	    d->var = new;
	else
	    tail->next = new;

	tail = new;

	if (g95_match_char('/') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_DATA);
    return MATCH_ERROR;
}



static match match_data_constant(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_locus where;
g95_symbol *sym;
g95_expr *expr;
match m;

    m = g95_match_literal_constant(&expr, 1);
    if (m == MATCH_YES) {
	*result = expr;
	return MATCH_YES;
    }

    if (m == MATCH_ERROR)
	return MATCH_ERROR;

    where = g95_current_locus;

    m = g95_match_null(result);
    if (m != MATCH_NO)
	return m;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    if (g95_find_symbol(name, NULL, 1, &sym))
	return MATCH_ERROR;

    if (sym != NULL && sym->attr.flavor == FL_DERIVED)
	return g95_match_structure_constructor(sym, result);

    if (sym == NULL || sym->attr.flavor != FL_PARAMETER) {
	g95_error("Symbol '%s' must be a PARAMETER in DATA statement at %L",
		  name, &g95_def_locus);
	return MATCH_ERROR;
    }

    if (sym->as != NULL) { /* Back up and reparse a complicated repeat count */
	g95_current_locus = where;

	m = g95_match_expr(&expr);
	if (m == MATCH_ERROR)
	    return m;

	if (g95_simplify_init_expr(expr) == FAILURE)
	    return MATCH_ERROR;

	if (expr->rank != 0) {
	    g95_error("repeat-count at %C must be scalar");
	    return MATCH_ERROR;
	}

	*result = expr;

    } else if (sym->ts.type != BT_DERIVED)
	*result = g95_copy_expr(sym->value);
    
    else {
	g95_current_locus = where;
	m = g95_match_variable(result, 0, 1, 1);

	if (m == MATCH_YES)
	    g95_simplify_expr(*result);
    }

    return m;
}



/* top_val_list()-- Match a list of values in a DATA statement.  The
 * leading '/' has already been seen at this point.  Elements with a
 * zero repeat count are dealt with at this level by not adding them
 * to the list. */

static match top_val_list(g95_data *data) {
g95_data_value *new, *tail;
g95_expr *expr;
int repeat;
char *msg;
match m;

    tail = NULL;

    for(;;) {
	m = match_data_constant(&expr);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) return MATCH_ERROR;

	if (expr->ts.type != BT_INTEGER || g95_match_char('*') != MATCH_YES)
	    repeat = 1;

	else {
	    msg = g95_extract_int(expr, &repeat);
	    g95_free_expr(expr);

	    if (msg != NULL) {
		g95_error(msg);
		return MATCH_ERROR;
	    }

	    if (repeat < 0) {
		g95_error("Negative repeat count at %C is illegal");
		return MATCH_ERROR;
	    }

	    m = match_data_constant(&expr);

	    if (m == MATCH_NO) goto syntax;
	    if (m == MATCH_ERROR) return MATCH_ERROR;
	}

	if (repeat == 0)
	    g95_free_expr(expr);

	else {
	    new = g95_get_data_value();

	    if (tail == NULL)
		data->value = new;
	    else
		tail->next = new;

	    tail = new;

	    tail->expr = expr;
	    tail->repeat = repeat;

	    if (g95_option.verbose) {
		g95_status("DATA element:  %d * ", tail->repeat);
		g95_show_expr(tail->expr);
		g95_status("\n");
	    }
	}

	if (g95_match_char('/') == MATCH_YES) break;
	if (g95_match_char(',') == MATCH_NO) goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_DATA);
    return MATCH_ERROR;
}



/* g95_match_data()-- Match a DATA statement */

match g95_match_data(void) {
g95_data *new;
match m;
  
    for(;;) {
	new = g95_get_data();
	new->where = g95_current_locus;

	m = top_var_list(new);
	if (m != MATCH_YES)
	    goto cleanup;

	m = top_val_list(new);
	if (m != MATCH_YES)
	    goto cleanup;

	new->next = g95_current_ns->data;
	g95_current_ns->data = new;

	if (g95_match_eos() == MATCH_YES)
	    break;

	g95_match_char(',');  /* Optional comma */
    }

    if (g95_pure(NULL, 1)) {
	g95_error("DATA statement at %C is not allowed in a PURE procedure");
	return MATCH_ERROR;
    }

    if (g95_option.obsolescent && !g95_spec_flag)
	g95_warning(123, "DATA statement in executable statements at %C is "
		    "obsolescent");

    if (G95_STRICT_F()) {
	g95_error("DATA statement at %C is not permitted in F");
	return MATCH_ERROR;
    }

    return MATCH_YES;

cleanup:
    g95_free_data(new);
    return MATCH_ERROR;
}



/* g95_match_block_data()-- Match a BLOCK DATA program unit */

match g95_match_block_data(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
match m;

    if (g95_match_eos() == MATCH_YES)
	strcpy(name, BLANK_BLOCK_DATA_NAME);

    else {
	m = g95_match("% %n%t", name);
	if (m != MATCH_YES)
	    return MATCH_ERROR;
    }

    if (g95_get_symbol(name, NULL, &sym))
	return MATCH_ERROR;

    if (g95_add_flavor(&sym->attr, FL_BLOCK_DATA, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    g95_new_block = sym;

    return MATCH_YES;
}



/* match_common_name()-- Match a common block name.  Returns a null
 * string for the blank common. */

static match match_common_name(char *name) {
match m;

    if (g95_match_char('/') == MATCH_NO) {
	name[0] = '\0';
	return MATCH_YES;
    }

    if (g95_match_char('/') == MATCH_YES) {
	name[0] = '\0';
	return MATCH_YES;
    }

    m = g95_match_name(name);

    if (m == MATCH_ERROR)
	return MATCH_ERROR;

    if (m == MATCH_YES && g95_match_char('/') == MATCH_YES)
	return MATCH_YES;

    g95_error("Syntax error in common block name at %C");
    return MATCH_ERROR;
}



/* g95_find_common()-- Given a name, return a pointer to the common
 * head structure, returning NULL it if it does not exist. */

g95_common_head *g95_find_common(char *name, g95_namespace *ns) {
g95_symtree *st;

    if (ns == NULL)
	ns = g95_current_ns;

    st = g95_find_symtree(ns->common_root, name);
    return (st == NULL) ? NULL : st->n.common;
}



/* g95_get_common()-- Given a name, return a pointer to the common
 * head structure, creating it if it does not exist. */

g95_common_head *g95_get_common(char *name, g95_locus *where) {
g95_symtree *st;

    st = g95_find_symtree(g95_current_ns->common_root, name);
    if (st == NULL)
	st = g95_new_symtree(&g95_current_ns->common_root, name);

    if (where == NULL)
	where = &g95_current_locus;

    if (st->n.common == NULL) {
	st->n.common = g95_get_common_head();
	st->n.common->where = *where;
    }

    return st->n.common;
}



/* g95_match_common()-- Match a COMMON statement */

match g95_match_common(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym, **head, *tail;
g95_common_head *h, *g;
g95_coarray_spec *cas;
g95_gsymbol *global;
int blank_extended;
g95_array_spec *as;
g95_locus where;
match m;

    blank_extended = 0;

    h  = NULL;
    as = NULL;

    if (g95_match_eos() == MATCH_YES)
	goto done;

    for(;;) {
	m = match_common_name(name);
	if (m == MATCH_ERROR)
	    goto cleanup;

	if (name[0] == '\0') {
	    if (g95_current_ns->blank_common != NULL &&
		!g95_current_ns->blank_common->use_assoc)
		h = g95_current_ns->blank_common;

	    else {
		blank_extended = 1;
		h = g95_get_common_head();

		h->next = g95_current_ns->blank_common;
		g95_current_ns->blank_common = h;
	    }

	} else {
	    h = g95_get_common(name, &g95_def_locus);

	    if (h->use_assoc) {
		g = g95_get_common_head();
		*g = *h;

		memset(h, '\0', sizeof(g95_common_head));
		h->next = g;
	    }

	    global = g95_check_global(name, GSYM_COMMON, &g95_def_locus);
	    if (global == NULL)
		goto cleanup;

	    global->size = -1;
	}

	head = &h->head;

	if (*head == NULL)
	    tail = NULL;

	else {
	    tail = *head;
	    while(tail->common_next)
		tail = tail->common_next;
	}

/* Grab the list of symbols for this common. */

	if (g95_match_eos() == MATCH_YES) {
	    if (*head == NULL && g95_option.fmode != 0)
		goto empty;

	    goto done;
	}

    for(;;) {
	m = g95_match_symbol(&sym, 0);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	where = g95_def_locus;

	if (sym->attr.in_common) {
	    g95_error("Symbol '%s' at %L is already in a COMMON block",
		      sym->name, &where);
	    goto cleanup;
	}

	if (sym->attr.use_assoc) {
	    g95_error("Symbol '%s' at %L has already been USE-associated",
		      sym->name, &where);
	    goto cleanup;
	}

	if (g95_add_in_common(&sym->attr, sym->name, &where) == FAILURE)
	    goto cleanup;

	if (tail != NULL)
	    tail->common_next = sym;

	else
	    *head = sym;

	tail = sym;

/* Deal with an optional array specification after the symbol name */

	m = g95_match_coarray_spec(&cas);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_YES) goto coarray;

	m = g95_match_array_spec(&as);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_YES) {
	    if (g95_add_dimension(&sym->attr, sym->name, &where) == FAILURE) 
		goto cleanup;

	    sym->as = as;
	    as = NULL;
	}

	m = g95_match_coarray_spec(&cas);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_YES) goto coarray;

	g95_gobble_whitespace();

	if (g95_match_eos() == MATCH_YES)
	    goto done;

	if (g95_peek_char() == '/')
	    break;

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;

	g95_gobble_whitespace();

	if (g95_peek_char() == '/')
	    break;
    }

    if (*head == NULL && g95_option.fmode != 0)
	goto empty;
    }

done:
    if (G95_STRICT_F()) {
	g95_error("COMMON block at %C is not permitted in F");
	goto cleanup;
    }

    return MATCH_YES;

coarray:
    g95_free_coarray_spec(cas);
    g95_error("Coarray specification not permitted in COMMON block at %C");
    goto cleanup;

empty:
    g95_error("Empty common block at %C");
    goto cleanup;

syntax:
    g95_syntax_error(ST_COMMON);

cleanup:
    if (h != NULL)
	h->head = NULL;

    if (blank_extended) {
	h = g95_current_ns->blank_common;
	g95_current_ns->blank_common = h->next;
	g95_free(h);
    }

    g95_free_array_spec(as);
    return MATCH_ERROR;
}



/* g95_match_module()-- Match a MODULE statement */

match g95_match_module(void) {
match m;

    m = g95_match(" %s%t", &g95_new_block);
    if (m != MATCH_YES)
	return m;

    if (g95_add_flavor(&g95_new_block->attr, FL_MODULE, g95_new_block->name,
		       NULL) == FAILURE)
	return MATCH_ERROR;

    return MATCH_YES;
}



/* match_equiv_ref_expr()-- Match array and substring constants that
 * appear in an equivalence variable specification. */

static try match_equiv_ref_expr(g95_expr **result) {
g95_expr *e;
match m;

    m = g95_match_expr(&e);
    if (m == MATCH_ERROR)
	return FAILURE;

    if (m == MATCH_NO) {
	g95_syntax_error(ST_EQUIVALENCE);
	return FAILURE;
    }

    if (g95_simplify_expr(e) == FAILURE) {
	g95_free_expr(e);
	return FAILURE;
    }

    if (e->type != EXPR_CONSTANT) {
	g95_error("Expression in EQUIVALENCE reference at %C must be "
		  "constant");
	g95_free_expr(e);
	return FAILURE;
    }

    *result = e;
    return SUCCESS;
}



/* match_equivalence_var()-- Match a variable in an equivalence
 * statement.  This subroutine is required because equivalence
 * statements can precede declaration statements, hence we have to
 * match an array reference before we know that an entity is even an
 * array.  The good news is that an equivalence variable consists of
 * the symbol by itself, an optional array reference and an optional
 * substring reference.  All of the expressions in the references have
 * to be constants.  There can't be array sections in the array
 * reference, so a colon means a substring reference. */

static match match_equivalence_var(g95_expr **result) {
g95_symbol *sym;
g95_expr *e, *f;
g95_ref *ref;
int i, c;
match m;

    m = g95_match_symbol(&sym, 0);
    if (m != MATCH_YES)
	return m;

    if (sym->attr.flavor != FL_VARIABLE && sym->attr.flavor != FL_UNKNOWN) {
	g95_error("Symbol '%s' at %L cannot appear in an EQUIVALENCE",
		  sym->name, &g95_def_locus);
	return MATCH_ERROR;
    }

    e = g95_get_expr();
    e->type = EXPR_VARIABLE;
    e->symbol = sym;
    e->rank = -1;
    e->where = g95_def_locus;

    f = NULL;
    g95_gobble_whitespace();

    if (g95_peek_char() != '(') {
	*result = e;
	return MATCH_YES;
    }

    g95_next_char();
    g95_gobble_whitespace();

    if (g95_peek_char() == ':') {
	g95_next_char();
	g95_gobble_whitespace();
	ref = g95_extend_ref(e, REF_SUBSTRING, NULL);
	goto substring_end;
    }

    if (match_equiv_ref_expr(&f) == FAILURE)
	goto cleanup;

    g95_gobble_whitespace();
    c = g95_next_char();
    g95_gobble_whitespace();

    switch(c) {
    case ')':
    case ',':
	ref = g95_extend_ref(e, REF_ARRAY, NULL);
	ref->u.ar.dimen = 1;
	ref->u.ar.type = AR_ELEMENT;
	ref->u.ar.start[0] = f;
	ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
	ref->u.ar.where[0] = g95_current_locus;
	f = NULL;

	if (c == ')')
	    goto check_substring;

	break;

    case ':':
	ref = g95_extend_ref(e, REF_SUBSTRING, NULL);
	ref->u.ss.start = f;
	f = NULL;
	goto substring_end;

    default:
	goto syntax;
    }

/* At this point, we've matched a left paren, an expression and a
 * comma, so we have a multidimensional array reference. */

    i = 1;
    for(;;) {
	if (match_equiv_ref_expr(&ref->u.ar.start[i]) == FAILURE)
	    goto cleanup;

	g95_gobble_whitespace();
	ref->u.ar.dimen_type[i] = DIMEN_ELEMENT;

	c = g95_next_char();
	g95_gobble_whitespace();

	if (c == ')')
	    break;

	if (c != ',')
	    goto syntax;

	if (++i >= G95_MAX_DIMENSIONS) {
	    g95_error("Too many dimensions in array reference at %C");
	    goto cleanup;
	}
    }

    ref->u.ar.dimen = i+1;

/* We've finished the array reference.  See if we have an additional
 * substring */

check_substring:
    if (g95_peek_char() != '(')
	goto done;

    g95_next_char();
    g95_gobble_whitespace();

    ref = g95_extend_ref(e, REF_SUBSTRING, NULL);

    if (g95_peek_char() == ':') {
	g95_next_char();
	g95_gobble_whitespace();
	goto substring_end;
    }

    if (match_equiv_ref_expr(&ref->u.ss.start) == FAILURE)
	goto cleanup;

    g95_gobble_whitespace();
    if (g95_next_char() != ':')
	goto syntax;

substring_end:
    if (g95_peek_char() != ')' &&
	match_equiv_ref_expr(&ref->u.ss.end) == FAILURE)
	goto cleanup;

    g95_gobble_whitespace();
    if (g95_next_char() != ')')
	goto syntax;

done:
    *result = e;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_EQUIVALENCE);

cleanup:
    g95_free_expr(e);
    g95_free_expr(f);
    return MATCH_ERROR;
}



/* g95_match_equivalence()-- Match an EQUIVALENCE statement */

match g95_match_equivalence(void) {
g95_equiv *eq, *set, *tail;
g95_locus where;
g95_ref *ref;
g95_expr *e;
int first;
match m;

    tail = NULL;
    e = NULL;
    where = g95_current_locus;

    for(;;) {
	eq = g95_get_equiv();
	if (tail == NULL)
	    tail = eq;

	eq->next = g95_current_ns->equiv;
	g95_current_ns->equiv = eq;

	if (g95_match_char('(') != MATCH_YES)
	    goto syntax;

	set = eq;
	first = 1;

	for(;;) {
	    m = match_equivalence_var(&e);
	    if (m == MATCH_ERROR) goto cleanup;
	    if (m == MATCH_NO) goto syntax;

	    set->expr = e;

	    if (g95_add_equivalence(&e->symbol->attr, e->symbol->name,
				    &e->where) == FAILURE)
		goto cleanup;

	    for(ref=e->ref; ref; ref=ref->next)
		if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION) {
		    g95_error("Array reference in EQUIVALENCE at %C cannot "
			      "be an array section");
		    goto cleanup;
		}

	    if (g95_match_char(')') == MATCH_YES) {
		if (first) {
		    g95_error("EQUIVALENCE list at %L requires at least "
			      "two names", &where);
		    goto cleanup;
		}

		break;
	    }

	    first = 0;

	    if (g95_match_char(',') != MATCH_YES)
		goto syntax;

	    set->eq = g95_get_equiv();
	    set = set->eq;
	}

	if (g95_match_eos() == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    if (G95_STRICT_F()) {
	g95_error("EQUIVALENCE statement at %C is not permitted in F");
	return MATCH_ERROR;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_EQUIVALENCE);

cleanup:
    eq = tail->next;
    tail->next = NULL;

    g95_free_equiv(g95_current_ns->equiv);
    g95_current_ns->equiv = eq;

    return MATCH_ERROR;
}



/* g95_free_equiv()-- Free equivalence sets and lists.  Recursively is
 * the easiest way to do this. */

void g95_free_equiv(g95_equiv *eq) {

    if (eq == NULL)
	return;

    g95_free_equiv(eq->eq);
    g95_free_equiv(eq->next);

    g95_free_expr(eq->expr);
    g95_free(eq);
}



/* g95_free_namelist()-- Free a namelist structure */

void g95_free_namelist(g95_namelist *name) {
g95_namelist *n;

    for(;name; name=n) {
	n = name->next;
	g95_free(name);
    }
}



/* g95_match_namelist()-- Match a NAMELIST statement */

match g95_match_namelist(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *group_name, *sym;
g95_namelist *nl;
g95_locus where;
match m, m2;
int flag;

    m = g95_match(" / %s /", &group_name);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto error;

    for(;;) {
	if (group_name->attr.use_assoc) {
	    g95_error("NAMELIST group '%s' at %C is already use-associated",
		      group_name->name);
	    goto error;
	}

	if (group_name->ts.type != BT_UNKNOWN) {
	    g95_error("Namelist group name '%s' at %C already has a "
		      "basic type of %s", group_name->name,
		      g95_typename(&group_name->ts));
	    return MATCH_ERROR;
	}

	if (group_name->attr.flavor != FL_NAMELIST &&
	    g95_add_flavor(&group_name->attr, FL_NAMELIST,
			   group_name->name, NULL) == FAILURE)
	    return MATCH_ERROR;

	for(;;) {
	    g95_gobble_whitespace();
	    where = g95_current_locus;

	    g95_match_name(name);
	    g95_current_locus = where;

	    m = g95_match_symbol(&sym, 1);
	    if (m == MATCH_NO) goto syntax;
	    if (m == MATCH_ERROR) goto error;

	    flag = (g95_current_ns->proc_name == sym);

	    if (sym->attr.in_namelist == 0 &&
		g95_add_in_namelist(&sym->attr, sym->name,
				    &where, flag) == FAILURE)
		goto error;

	    nl = g95_get_namelist();
	    nl->name = g95_get_string(name);
	    nl->sym = sym;
	    nl->where = g95_def_locus;

	    if (group_name->namelist == NULL) 
		group_name->namelist = group_name->namelist_tail = nl;

	    else {
		group_name->namelist_tail->next = nl;
		group_name->namelist_tail = nl;
	    }

	    if (g95_match_eos() == MATCH_YES)
		goto done;

	    m = g95_match_char(',');

	    if (g95_match_char('/') == MATCH_YES) {
		m2 = g95_match(" %s /", &group_name);
		if (m2 == MATCH_YES) break;
		if (m2 == MATCH_ERROR) goto error;
		goto syntax;
	    }

	    if (m != MATCH_YES)
		goto syntax;
	}
    }

done:
    if (G95_STRICT_F()) {
	g95_error("NAMELIST statement at %C is not legal in F");
	return MATCH_ERROR;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_NAMELIST);

error:
    return MATCH_ERROR;
}



/* g95_match_where()-- Match a WHERE statement */

match g95_match_where(g95_statement *st) {
g95_expr *expr;
match m0, m;
g95_code *c;

    m0 = g95_match_label();
    if (m0 == MATCH_ERROR)
	return m0;

    m = g95_match(" where ( %e )", &expr);
    if (m != MATCH_YES)
	return m;

    if (g95_match_eos() == MATCH_YES) {
	*st = ST_WHERE_BLOCK;

	new_st.type = EXEC_WHERE;
	new_st.expr = expr;
	return MATCH_YES;
    }

    m = g95_match_assignment();
    if (m == MATCH_NO)
	g95_syntax_error(ST_WHERE);

    if (m != MATCH_YES) {
	g95_free_expr(expr);
	return MATCH_ERROR;
    }

/* We've got a simple WHERE statement */

    *st = ST_WHERE;

    c = g95_get_code(EXEC_WHERE, NULL);
    c->expr = expr;
    c->next = g95_get_code(-1, NULL);

    *c->next = new_st;
    g95_clear_new_st();

    new_st.type = EXEC_WHERE;
    new_st.block = c;

    return MATCH_YES;
}



/* g95_match_elsewhere()-- Match an ELSEWHERE statement.  We leave
 * behind a WHERE node in new_st if successful. */

match g95_match_elsewhere(void) {
char *block_name, name[G95_MAX_SYMBOL_LEN+1];
g95_expr *expr;
match m;

    if (g95_current_state() != COMP_WHERE) {
	g95_error("ELSEWHERE statement at %C not enclosed in WHERE block");
	return MATCH_ERROR;
    }

    expr = NULL;

    if (g95_match_char('(') == MATCH_YES) {
	m = g95_match_expr(&expr);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) return MATCH_ERROR;

	if (g95_match_char(')') != MATCH_YES)
	    goto syntax;
    }

    if (g95_match_eos() != MATCH_YES) { /* Better be a name at this point */
	m = g95_match_name(name);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	if (g95_match_eos() != MATCH_YES)
	    goto syntax;

	block_name = g95_current_block_name();
	if (strcmp(name, block_name) != 0) {
	    g95_error("Label '%s' at %C doesn't match WHERE label '%s'",
		      name, block_name);
	    goto cleanup;
	}

	if (G95_STRICT_F()) {
	    g95_error("Construct label not permitted in ELSE WHERE "
		      "statement at %C in F");
	    goto cleanup;
	}
    }

    new_st.type = EXEC_WHERE;
    new_st.expr = expr;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_ELSEWHERE);

cleanup:
    g95_free_expr(expr);
    return MATCH_ERROR;
}



/* match_sync_stat()-- Match a (required) sync-stat list. */

static match match_sync_stat(int st) {
match m;

    new_st.ext.sync.stat   = NULL;
    new_st.ext.sync.errmsg = NULL;
    new_st.ext.sync.ready  = NULL;

    if (g95_match_char('(') != MATCH_YES) {
	g95_error("Syntax error in sync stat list at %C");
	return MATCH_ERROR;
    }

    if (st == ST_SYNC_ALL || st == ST_SYNC_MEMORY) {
	if (g95_match_char(')') == MATCH_YES)
	    goto end;

    } else {
	if (g95_match_char('*') == MATCH_YES)
	    new_st.expr = NULL;

	else {
	    m = g95_match_expr(&new_st.expr);
	    if (m == MATCH_ERROR)
		return MATCH_ERROR;

	    if (m == MATCH_NO) {
		g95_error("Missing image number at %C");
		return MATCH_ERROR;
	    }
	}

	if (g95_match_char(')') == MATCH_YES)
	    goto end;

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;
    }

    for(;;) {
	if (g95_match(" stat = ") == MATCH_YES) {
	    if (new_st.ext.sync.stat != NULL) {
		g95_error("Duplicate STAT variable at %C");
		return MATCH_ERROR;
	    }

	    m = g95_match_variable(&new_st.ext.sync.stat, 1, 0, 0);
	    if (m == MATCH_ERROR)
		return m;

 	    if (m == MATCH_NO) {
		g95_error("Missing STAT variable at %C");
		return MATCH_ERROR;
	    }

	} else if (g95_match(" errmsg = ") == MATCH_YES) {
	    if (new_st.ext.sync.errmsg != NULL) {
		g95_error("Duplicate ERRMSG variable at %C");
		return MATCH_ERROR;
	    }

	    m = g95_match_variable(&new_st.ext.sync.errmsg, 1, 0, 0);
	    if (m == MATCH_ERROR)
		return m;

	    if (m == MATCH_NO) {
		g95_error("Missing ERRMSG variable at %C");
		return MATCH_ERROR;
	    }

	} else if (st == ST_QUERY && g95_match(" ready = ") == MATCH_YES) {
	    if (new_st.ext.sync.ready != NULL) {
		g95_error("Duplicate READY variable at %C");
		return MATCH_ERROR;
	    }

	    m = g95_match_variable(&new_st.ext.sync.ready, 1, 0, 0);
	    if (m == MATCH_ERROR)
		return m;

	    if (m == MATCH_NO) {
		g95_error("Missing READY variable at %C");
		return MATCH_ERROR;
	    }

	} else
	    goto syntax;

	if (g95_match_char(',') == MATCH_YES)
	    continue;

	if (g95_match_char(')') != MATCH_YES)
	    goto syntax;

	break;
    }

end:
    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

syntax:
    g95_error("Syntax error in sync stat list at %C");
    return MATCH_ERROR;
}



/* g95_match_sync_all()-- Match the SYNC ALL statement. */

match g95_match_sync_all(void) {

    new_st.type = EXEC_SYNC_ALL;

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    return match_sync_stat(ST_SYNC_ALL);
}



/* g95_match_sync_images()-- Match the SYNC IMAGE statement. */

match g95_match_sync_images(void) {

    new_st.type = EXEC_SYNC_IMAGES;

    return match_sync_stat(ST_SYNC_IMAGES);
}



/* g95_match_sync_memory()-- Match the SYNC MEMORY statement. */

match g95_match_sync_memory(void) {

    new_st.type = EXEC_SYNC_MEMORY;

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    return match_sync_stat(ST_SYNC_MEMORY);
}


/* g95_match_notify()-- Match a NOTIFY statement. */

match g95_match_notify(void) {

    new_st.type = EXEC_NOTIFY;

    return match_sync_stat(ST_NOTIFY);
}


/* g95_match_query()-- Match a QUERY statement. */

match g95_match_query(void) {

    new_st.type = EXEC_QUERY;

    return match_sync_stat(ST_QUERY);
}


/* g95_match_critical()-- Match the CRITICAL statement. */

match g95_match_critical(void) {

    if (g95_match_label() == MATCH_ERROR)
	return MATCH_ERROR;

    if (g95_match(" critical") != MATCH_YES)
	return MATCH_NO;

    if (g95_match_eos() != MATCH_YES) {
	g95_syntax_error(ST_CRITICAL);
	return MATCH_ERROR;
    }

    new_st.type = EXEC_CRITICAL;
    return MATCH_YES;
}



/* g95_match_sync_team()-- Match the SYNC TEAM statement. */

match g95_match_sync_team(void) {
g95_expr *e;

    e = NULL;

    if (g95_match_char('(') != MATCH_YES ||
	g95_match_variable(&e, 1, 0, 0) != MATCH_YES ||
	g95_match_char(')') != MATCH_YES ||
	g95_match_eos() != MATCH_YES) {

	g95_syntax_error(ST_SYNC_TEAM);
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    new_st.type = EXEC_SYNC_TEAM;
    new_st.expr = e;

    return MATCH_YES;
}

