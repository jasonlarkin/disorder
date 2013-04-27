/* FORMAT statement
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

/* format.c-- match the FORMAT statement and check format string */

#include <ctype.h>
#include <string.h>
#include "g95.h"

/* format tokens returned by format_lex() */

typedef enum {
    FMT_NONE, FMT_UNKNOWN, FMT_SIGNED_INT, FMT_ZERO, FMT_POSINT, FMT_PERIOD,
    FMT_COMMA, FMT_COLON, FMT_SLASH, FMT_DOLLAR, FMT_POS, FMT_LPAREN,
    FMT_RPAREN, FMT_X, FMT_SIGN, FMT_BLANK, FMT_CHAR, FMT_P, FMT_I, FMT_BOZ,
    FMT_F, FMT_E, FMT_EXT, FMT_G, FMT_L, FMT_A, FMT_D, FMT_H, FMT_DC, FMT_DP,
    FMT_END, FMT_ERROR
} format_token;


/* Local variables for checking format strings.  The saved_token is
 * used to back up by a single format token during the parsing process. */

static char last_char, *format_string, *format_start, f_message[100];
static int format_length, use_last_char, repeat, seen_data_desc, read_flag;

static format_token saved_token;
static g95_locus where, new_where;

static enum { MODE_STRING, MODE_FORMAT, MODE_COPY } mode;


/* next_char()-- Return the next character in the format string */

static char next_char(int in_string) {
char c;

    if (use_last_char) {
	use_last_char = 0;
	return last_char;
    }

    format_length++;

    if (mode == MODE_STRING)
	c = *format_string++;

    else {
	do {
	    c = g95_next_char_literal(in_string);
	    if (c == '\n')
		c = '\0';

	} while(!in_string && c == ' ');

	if (mode == MODE_COPY)
	    *format_string++ = c;
    }

    c = toupper(c);
    last_char = c;
    return c;
}



/* unget_char()-- Back up one character position.  Only works once. */

static void unget_char(void) {

    use_last_char = 1;
}



/* format_lex()-- Simple lexical analyzer for getting the next token
 * in a FORMAT statement. */

static format_token format_lex(void) {
format_token token;
char c, d, delim;

    if (saved_token != FMT_NONE) {
	token = saved_token;
	saved_token = FMT_NONE;
	return token;
    }

    if (mode == MODE_FORMAT)
	where = new_where;

    do {
	c = next_char(0);
    } while(g95_is_whitespace(c));

    switch(c) {
    case '-':
    case '+':
	c = next_char(0);
	if (!isdigit((int) c)) {
	    token = FMT_UNKNOWN;
	    break;
	}

	do
	    c = next_char(0);
	while(isdigit((int) c));

	unget_char();
	token = FMT_SIGNED_INT;
	break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	repeat = c - '0';

	for(;;) {
	    c = next_char(0);
	    if (!isdigit((int) c))
		break;

	    repeat = 10*repeat + c - '0';
	}

	unget_char();
	token = (repeat == 0) ? FMT_ZERO : FMT_POSINT;
	break;

    case '.':
	token = FMT_PERIOD;
	break;

    case ',':
	token = FMT_COMMA;
	break;

    case ':':
	token = FMT_COLON;
	break;

    case '/':
	token = FMT_SLASH;
	break;

    case '$': case '\\':
	if (g95_option.fmode == 0)
	    token = FMT_DOLLAR;

	else {
	    sprintf(f_message, "Descriptor '%c' is not standard", c);
	    token = FMT_ERROR;
	}

	break;

    case 'T':
	d = next_char(0);
	switch(d) {
	case 'L': case 'l':
	case 'R': case 'r':
	    break;

	default:
	    unget_char();
	    break;
	}

	token = FMT_POS;
	break;

    case '(':
	token = FMT_LPAREN;
	break;

    case ')':
	token = FMT_RPAREN;
	break;

    case 'X':
	token = FMT_X;
	break;

    case 'S':
	d = next_char(0);
	switch(d) {
	case 'p':  case 'P':
	case 's':  case 'S':
	    break;

	default:
	    unget_char();
	    break;
	}

	token = FMT_SIGN;
	break;

    case 'B':
	d = next_char(0);
	switch(d) {
	case 'N': case 'n':
	case 'Z': case 'z':
	    token = FMT_BLANK;

	    if (G95_STRICT_F()) {
		sprintf(f_message, "'B%c' descriptor not allowed in F mode",d);
		token = FMT_ERROR;
	    }

	    break;

	default:
	    seen_data_desc = 1;
	    unget_char();
	    token = FMT_BOZ;
	    break;
	}

	break;

    case '\'': case '"':
	delim = c;      

	for(;;) {
	    d = next_char(1);
	    if (d == '\0') {
		token = FMT_END;
		break;
	    }

	    if (d == delim) {
		d = next_char(0);

		if (d == '\0') {
		    token = FMT_END;
		    break;
		}

		if (d != delim) {
		    unget_char();
		    token = FMT_CHAR;
		    break;
		}
	    }
	}

	break;

    case 'P':
	token = FMT_P;
	break;

    case 'I':
	seen_data_desc = 1;
	token = FMT_I;
	break;

    case 'O':
    case 'Z':
	seen_data_desc = 1;
	token = FMT_BOZ;
	break;

    case 'F':
	seen_data_desc = 1;
	token = FMT_F;
	break;

    case 'E':
	d = next_char(0);
	switch(d) {
	case 'N': case 'n':
	case 'S': case 's':
	    token = FMT_EXT;
	    break;

	default:
	    token = FMT_E;
	    unget_char();
	    break;
	}

	seen_data_desc = 1;
	break;

    case 'G':
	seen_data_desc = 1;
	token = FMT_G;
	break;

    case 'H':
	seen_data_desc = 1;
	token = FMT_H;
	break;

    case 'L':
	seen_data_desc = 1;
	token = FMT_L;
	break;

    case 'A':
	seen_data_desc = 1;
	token = FMT_A;
	break;

    case 'D':
	switch(next_char(0)) {
	case 'P': case 'p':
	    token = FMT_DP;
	    break;

	case 'C': case 'c':
	    token = FMT_DC;
	    break;

	default:
	    unget_char();
	    token = FMT_D;
	    seen_data_desc = 1;
	    break;
	}

	break;

    case '\0':
	token = FMT_END;
	break;

    default:
	token = FMT_UNKNOWN;
	break;
    }

    if (G95_STRICT_F())
	switch(token) {
	case FMT_BOZ:  case FMT_D:  case FMT_G:
	case FMT_H:    case FMT_P:  case FMT_X:
	    sprintf(f_message, "'%c' descriptor not allowed in F mode", c);
	    token = FMT_ERROR;
	    break;

	case FMT_CHAR:
	    strcpy(f_message, "Constant string formats not allowed in F mode");
	    token = FMT_ERROR;
	    break;

	default:
	    break;
	}

    new_where = g95_current_locus;
    return token;
}



/* check_format()-- Check a format statement.  The format string,
 * either from a FORMAT statement or a constant in an I/O statement
 * has already been parsed by itself, and we are checking it for validity.
 *
 * The dual origin means that the warning message is a little less than
 * great. */

static try check_format(void) {
char *error,
     *posint_required = "Positive width required",
     *period_required = "Period required",
     *nonneg_required = "Nonnegative width required",
     *unexpected_element = "Unexpected element",
     *unexpected_end = "Unexpected end of format string",
     *missing = "Missing edit descriptor after comma";

format_token t, u;
int level, comma;
try rv;

    use_last_char = 0; 
    saved_token = FMT_NONE;
    seen_data_desc = 0;
    level = 0;
    comma = 0;
    rv = SUCCESS;

    t = format_lex();
    if (t != FMT_LPAREN) {
	error = "Missing leading left parenthesis";
	goto syntax;
    }

/* In this state, the next thing has to be a format item */

format_item:
    t = format_lex();
    switch(t) {
    case FMT_POSINT:
	t = format_lex();
	if (t == FMT_LPAREN) {
	    comma = 0;
	    level++;
	    goto format_item;
	}

	if (t == FMT_SLASH)
	    goto optional_comma;

	goto data_desc;

    case FMT_ZERO:
	t = format_lex();
	if (t != FMT_P) {
	    error = "Zero repeat count not allowed";
	    goto syntax;
	}

	goto p_descriptor;

    case FMT_LPAREN:
	level++;
	goto format_item;

    case FMT_RPAREN:
	if (comma) {
	    error = missing;
	    goto syntax;
	}

	goto rparen;

    case FMT_SIGNED_INT:  /* Signed integer can only precede a P format */
	t = format_lex();
	if (t != FMT_P) {
	    error = "Expected P edit descriptor";
	    goto syntax;
	}

	goto data_desc;

    case FMT_P:       /* P and X require a prior number */
	error = "P descriptor requires leading scale factor";
	goto syntax;

    case FMT_X:
	error = "X descriptor requires leading space count";
	goto syntax;

    case FMT_SIGN:
    case FMT_DP:
    case FMT_DC:
    case FMT_BLANK:
    case FMT_CHAR:
	goto between_desc;

    case FMT_COLON:
    case FMT_SLASH:
	if (G95_STRICT_F())
	    goto between_desc;

	goto optional_comma;

    case FMT_DOLLAR:
	while(level >= 0) {
	    t = format_lex();
	    if (t != FMT_RPAREN) {
		error = "$ or \\ descriptor must be the last specifier";
		goto syntax;
	    }

	    level--;
	}

	goto finished;

    case FMT_E:
	if (G95_STRICT_F()) {
	    error = "E format not allowed in F-mode";
	    goto syntax;
	}

	goto data_desc;

    case FMT_POS:  case FMT_I:  case FMT_BOZ:  case FMT_F:  case FMT_EXT:
    case FMT_G:    case FMT_L:  case FMT_A:    case FMT_D:
	goto data_desc;

    case FMT_H:
	repeat = 1;
	goto handle_hollerith;

    case FMT_END:
	error = unexpected_end;
	goto syntax;

    case FMT_COMMA:
	error = "Unexpected comma";
	goto syntax;

    case FMT_ERROR:
	error = f_message;
	goto syntax;

    default:
	error = unexpected_element;
	goto syntax;
    }

/* In this state, t must currently be a data descriptor.  Deal with
 * things that can/must follow the descriptor */

data_desc:
    switch(t) {
    case FMT_SIGN:
    case FMT_DC:
    case FMT_DP:
    case FMT_BLANK:
    case FMT_X:
	break;

    case FMT_P:
    p_descriptor:
	if (g95_option.fmode != 0) {
	    t = format_lex();
	    if (t == FMT_POSINT) {
		error = "Repeat count cannot follow P descriptor";
		goto syntax;
	    }

	    saved_token = t;
	}

	goto optional_comma;

    case FMT_POS:
    case FMT_L:
	t = format_lex();
	if (t == FMT_POSINT)
	    break;

	error = posint_required;
	goto syntax;

    case FMT_A:
	t = format_lex();
	if (t != FMT_POSINT)
	    saved_token = t;

	break;

    case FMT_D:  case FMT_E:
    case FMT_G:  case FMT_EXT:
	u = format_lex();
	if (u != FMT_POSINT) {
	    error = posint_required;
	    goto syntax;
	}

	u = format_lex();
	if (u != FMT_PERIOD) {
	    error = period_required;
	    goto syntax;
	}

	u = format_lex();
	if (u != FMT_ZERO && u != FMT_POSINT) {
	    error = nonneg_required;
	    goto syntax;
	}

	if (t == FMT_D)
	    break;

/* Look for optional exponent */

	u = format_lex();
	if (u != FMT_E)
	    saved_token = u;

	else {
	    u = format_lex();
	    if (u != FMT_POSINT) {
		error = "Positive exponent width required";
		goto syntax;
	    }
	}

	break;

    case FMT_F:
	t = format_lex();
	if (t != FMT_ZERO && t != FMT_POSINT) {
	    error = nonneg_required;
	    goto syntax;
	}

	if (t == FMT_ZERO && read_flag) {
	    error = posint_required;
	    goto syntax;
	}

	t = format_lex();
	if (t != FMT_PERIOD) {
	    error = period_required;
	    goto syntax;
	}

	t = format_lex();
	if (t != FMT_ZERO && t != FMT_POSINT) {
	    error = nonneg_required;
	    goto syntax;
	}

	break;

    case FMT_H:
    handle_hollerith:
	if (g95_option.fmode != 0) {
	    error = "The H format specifier is a deleted language feature";
	    goto syntax;
	}

	while(repeat>0) {
	    if (next_char(1) == '\0') {
		error = unexpected_end;
		goto syntax;
	    }

	    repeat--;
	}

	break;

    case FMT_I:
    case FMT_BOZ:
	t = format_lex();
	if (t != FMT_ZERO && t != FMT_POSINT) {
	    error = nonneg_required;
	    goto syntax;
	}

	if (t == FMT_ZERO && read_flag) {
	    error = posint_required;
	    goto syntax;
	}

	t = format_lex();
	if (t != FMT_PERIOD)
	    saved_token = t;

	else {
	    t = format_lex();
	    if (t != FMT_ZERO && t != FMT_POSINT) {
		error = nonneg_required;
		goto syntax;
	    }
	}

	break;

    case FMT_ERROR:
	error = f_message;
	goto syntax;

    default:
	error = unexpected_element;
	goto syntax;
    }

/* Between a descriptor and what comes next */

between_desc:
    comma = 0;
    t = format_lex();

    switch(t) {
    case FMT_COMMA:
	comma = 1;
	goto format_item;

    case FMT_RPAREN:
    rparen:
	level--;
	if (level < 0)
	    goto finished;

	goto between_desc;

    case FMT_COLON:
    case FMT_SLASH:
	goto optional_comma;

    case FMT_END:
	error = unexpected_end;
	goto syntax;

    case FMT_ERROR:
	error = f_message;
	goto syntax;

    default:
	if (g95_option.fmode == 0) { /* extension */
	    saved_token = t;
	    goto format_item;
	}

	error = "Missing comma";
	goto syntax;
    }

/* Optional comma is a weird between state where we've just finished
 * reading a colon, slash or P descriptor. */

optional_comma:
    comma = 0;
    t = format_lex();

    switch(t) {
    case FMT_COMMA:
	comma = 1;
	break;

    case FMT_RPAREN:
	level--;
	if (level < 0)
	    goto finished;

	goto between_desc;

    default:     /* Assume that we have another format item */
	saved_token = t;
	break;
    }

    goto format_item;

/* Something went wrong.  If the format we're checking is a string,
 * generate a warning, since the program is correct.  If the format is
 * in a FORMAT statement, this messes up parsing, which is an error. */

syntax:
    if (mode != MODE_STRING)
	g95_error("%s in format string at %L", error, &where);

    else
	g95_warning(100, "%s in format string at %L", error, &where);

    rv = FAILURE;

finished:
    return rv;
}



/* g95_check_format_string()-- Given an expression node that is a
 * constant string, see if it looks like a format string */

void g95_check_format_string(g95_expr *e, int reading, int have_data_item) {

    mode = MODE_STRING;
    format_string = format_start = e->value.character.string;
    format_length = e->value.character.length;
    read_flag = reading;
    where = e->where;
    check_format();

    if (have_data_item && !seen_data_desc)
	g95_warning(162, "No data descriptors seen in format string at %L",
		    &where);
}



/* g95_match_format()-- Match a FORMAT statement.  This amounts to
 * actually parsing the format descriptors in order to correctly
 * locate the end of the format string. */

match g95_match_format(void) {
g95_locus start;
g95_expr *e;

    if (G95_STRICT_F()) {
	g95_error("FORMAT statement at %C not permitted in F");
	return MATCH_ERROR;
    }

    if (g95_statement_label == NULL) {
	g95_error("FORMAT statement at %C does not have a statement label");
	return MATCH_ERROR;
    }

    g95_gobble_whitespace();

    mode = MODE_FORMAT;
    format_length = 0;
    read_flag = 0;

    start = where = new_where = g95_current_locus;

    if (check_format() == FAILURE)
	return MATCH_ERROR;

    if (g95_match_eos() != MATCH_YES) {
	g95_syntax_error(ST_FORMAT);
	return MATCH_ERROR;
    }

    /* The label doesn't get created until after the statement is done
     * being matched, so we have to leave the string for later. */

    g95_current_locus = start;      /* Back to the beginning */

    e = g95_get_expr();
    e->type    = EXPR_CONSTANT;
    e->ts.type = BT_CHARACTER;
    e->ts.kind = g95_default_character_kind();

    e->where = start;
    e->value.character.string = format_string = g95_getmem(format_length+1);
    e->value.character.length = format_length;

    g95_statement_label->format = e;

    mode = MODE_COPY;
    check_format();       /* Guaranteed to succeed */

    g95_match_eos();      /* Guaranteed to succeed */
    new_st.type = EXEC_NOP;

    return MATCH_YES;
}

