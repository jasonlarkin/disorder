/* Primary expression subroutines
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

/* primary.c-- Match primary expressions */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "g95.h"

typedef enum {
    RVAL_VARIABLE, RVAL_FUNCTION, RVAL_SUBROUTINE, RVAL_PROC,
    RVAL_DERIVED,  RVAL_UNKNOWN,  RVAL_ERROR,
} rvalue_type;

g95_charlen g95_unknown_charlen;



/* match_kind_param()-- Matches a kind-parameter expression, which is
 * either a named symbolic constant or a nonnegative integer constant.
 * If successful, sets the kind value to the correct integer.  */

static match match_kind_param(int *kind) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
char *p;
match m;

    m = g95_match_small_literal_int(kind, 0);
    if (m != MATCH_NO)
	return m;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    if (g95_find_symbol(name, NULL, 1, &sym))
	return MATCH_ERROR;

    if (sym == NULL || sym->attr.flavor != FL_PARAMETER)
	return MATCH_NO;

    g95_set_usage(sym, &sym->declared_at, 0, 1);

    p = g95_extract_int(sym->value, kind);
    if (p != NULL)
	return MATCH_NO;

    return MATCH_YES;
}



/* check_digit()-- Given a character and a radix, see if the character
 * is a valid digit in that radix. */

static int check_digit(int c, int radix) {
int r;

    switch(radix) {
    case 2:
	r = ('0' <= c && c <= '1');
	break;

    case 8:
	r = ('0' <= c && c <= '7');
	break;

    case 10:
	r = ('0' <= c && c <= '9');
	break;

    case 16:
	r = ('0' <= c && c <= '9') || ('a' <= c && c <= 'f');
	break;

    default:
	g95_internal_error("check_digit(): bad radix");
    }

    return r;
}



/* get_kind()-- Gets a trailing kind-specification for non-character
 * variables.  Normally returns the integer kind value or
 *   -1   if an error was generated
 *   -2   if no kind was found
 */

static int get_kind(void) {
int kind;
match m;

    if (g95_match_char('_') != MATCH_YES)
	return -2;

    m = match_kind_param(&kind);
    switch(m) {
    case MATCH_YES:
	if (kind >= 0)
	    return kind;

	g95_error("Invalid kind-parameter at %C");
	kind = -1;
	break;

    case MATCH_NO:
	g95_error("Missing kind-parameter at %C");
	break;

    case MATCH_ERROR:
	break;
    }

    return -1;
}



/* match_digits()-- Match the digit string part of an integer.
 * If the buffer is NULL, we just count characters for the second
 * pass.  Returns the number of characters matched, -1 for no match. */

static int match_digits(int signflag, int radix, char *buffer) {
g95_locus where;
int length, c;

    length = 0;
    c = g95_next_char();

    if (signflag && (c == '+' || c == '-')) {
	if (buffer != NULL)
	    *buffer++ = c;

	if (g95_option.fmode == 0)
	    g95_gobble_whitespace();

	c = g95_next_char();
	length++;
    }

    if (!check_digit(c, radix))
	return -1;

    length++;
    if (buffer != NULL)
	*buffer++ = c;

    for(;;) {
	where = g95_current_locus;
	c = g95_next_char();

	if (!check_digit(c, radix))
	    break;

	if (buffer != NULL)
	    *buffer++ = c;

	length++;
    }

    g95_current_locus = where;

    return length;
}



/* match_integer_constant()-- Match an integer (digit string and
 * optional kind). */

static match match_integer_constant(g95_expr **result, int signflag) {
g95_locus old_loc, where;
int length, kind;
char *buffer;
g95_expr *e;

    old_loc = g95_current_locus;
    g95_gobble_whitespace();

    length = match_digits(signflag, 10, NULL);
    g95_current_locus = old_loc;
    if (length == -1)
	return MATCH_NO;

    buffer = alloca(length+1);
    memset(buffer, '\0', length+1);

    g95_gobble_whitespace();
    where = g95_current_locus;

    match_digits(signflag, 10, buffer);

    kind = get_kind();
    if (kind == -2) kind = g95_default_integer_kind(1);
    if (kind == -1) return MATCH_ERROR;

    if (g95_validate_kind(BT_INTEGER, kind) == -1) {
	g95_error("Integer kind %d at %C not available", kind);
	return MATCH_ERROR;
    }

    e = g95_convert_integer(buffer, kind, 10, &where);

    *result = e;
    return MATCH_YES;
}



/* match_boz_constant()-- Match a binary, octal or hexadecimal
 * constant that can be found in a DATA statement */

static match match_boz_constant(g95_expr **result) {
int radix, delim, length, kind;
g95_locus old_loc, where;
char *buffer;
g95_expr *e;
char *rname;

    old_loc = g95_current_locus;
    g95_gobble_whitespace();
    where = g95_current_locus;

    switch (g95_next_char()) {
    case 'x':
	if (g95_option.fmode != 0)
	    goto backup;

	/* Fall through */
    case 'z':  radix = 16; rname = "hexadecimal";  break;
    case 'b':  radix = 2;  rname = "binary";       break;
    case 'o':  radix = 8;  rname = "octal";        break;

    default: goto backup;
    }

    /* no whitespace allowed here */

    delim = g95_next_char();
    if (delim != '\'' && delim != '\"')
	goto backup;

    old_loc = g95_current_locus;
  
    length = match_digits(0, radix, NULL);
    if (length == -1) {
	g95_error("Empty set of digits in %s constants at %L", rname, &where);
	return MATCH_ERROR;
    }

    if (g95_next_char() != delim) {
	g95_error("Illegal character in %s constant at %L.", rname, &where);
	return MATCH_ERROR;
    }

    g95_current_locus = old_loc;

    buffer = alloca(length+1);
    memset(buffer, '\0', length+1);

    match_digits(0, radix, buffer);
    g95_next_char();  /* Gobble trailing ' */

    kind = get_kind();
    if (kind == -1) return MATCH_ERROR;
    if (kind == -2) kind = g95_default_integer_kind(1);

    if (G95_STRICT_F()) {
	g95_error("BOZ constant at %L is not allowed in F", &where);
	return MATCH_ERROR;
    }

    e = g95_convert_integer(buffer, kind, radix, &where);
    e->value.integer->typeless = 1;

    *result = e;
    return MATCH_YES;

backup:
    g95_current_locus = old_loc;
    return MATCH_NO;
}



/* match_real_constant()-- Match a real constant of some sort. */

static match match_real_constant(g95_expr **result, int signflag) {
int kind, c, count, seen_dp, seen_digits, exp_char, digit_flags;
g95_locus old_loc, temp_loc, where;
char *p, *buffer;
g95_expr *e;

    old_loc = g95_current_locus;
    g95_gobble_whitespace();
    where = g95_current_locus;

    e = NULL;
    buffer = NULL;

    count = 0;
    seen_dp = 0;
    seen_digits = 0;
    exp_char = ' ';
    digit_flags = 0;

    c = g95_next_char();
    if (signflag && (c == '+' || c == '-'))
	do {
	    count++;
	    c = g95_next_char();
	} while(g95_option.fmode == 0 && g95_is_whitespace(c));

/* Scan significand */

    for(;; c=g95_next_char(), count++) {
	if (c == '.') {
	    if (seen_dp)
		goto done;

	   /* Check to see if "." goes with a following operator like ".eq." */

	    temp_loc = g95_current_locus;
	    c = g95_next_char();

	    if (c == 'e' || c == 'd' || c == 'q') {
		c = g95_next_char();
		if (c == '.')
		    goto done;   /* Operator named .e. or .d. */
	    }

	    if (g95_uopchar(c))
		goto done;   /* Distinguish 1.e9 from 1.eq.2 */

	    g95_current_locus = temp_loc;
	    seen_dp = 1;
	    continue;
	}

	if (isdigit(c)) {
	    seen_digits = 1;
	    digit_flags |= (seen_dp) ? 2 : 1;
	    continue;
	}

	break;
    }

    if (!seen_digits || (c != 'e' && c != 'd' && c != 'q'))
	goto done;

    exp_char = c;

/* scan exponent */

    c = g95_next_char();
    count++;

    if (c == '+' || c == '-') {  /* optional sign */
	c = g95_next_char();
	count++;
    }

    if (!isdigit(c)) {
	if (!seen_digits) {
	    g95_current_locus = old_loc;
	    return MATCH_NO;   /* ".e" can be something else */
	}

	g95_error("Missing exponent in real number at %C");
	return MATCH_ERROR;
    }

    while(isdigit(c)) {
	c = g95_next_char();
	count++;
    }

/* See what we've got */

done:
    if (!seen_digits || (!seen_dp && exp_char == ' ')) {
	g95_current_locus = old_loc;
	return MATCH_NO;
    }

    if (G95_STRICT_F()) {
	if (seen_dp && digit_flags != 3) {
	    g95_error("Real constant at %C must have digits on both sides "
		      "of the decimal point in F mode");
	    return MATCH_ERROR;
	}

	if (exp_char != 'e' && exp_char != ' ') {
	    g95_error("Real constant at %C must have an 'e' exponent in "
		      "F mode");
	    return MATCH_ERROR;
	}
    }

/* Convert the number */

    g95_current_locus = old_loc;
    g95_gobble_whitespace();

    buffer = alloca(count+10);

    p = buffer;
    for(; count>0; count--) {
	c = g95_next_char();
	if (g95_is_whitespace(c))
	    continue;

	*p = c;

	if (*p == 'd' || *p == 'q')
	    *p = 'e';   /* Hack for conversion */

	p++;
    }

    *p = '\0';

    kind = get_kind();
    if (kind == -1)
	goto cleanup;

    switch(exp_char) {
    case 'd':
	if (kind != -2) {
	    g95_error("Real number at %C has a 'd' exponent and an "
		      "explicit kind");
	    goto cleanup;
	}

	kind = g95_default_double_kind();
	break;

    case 'q':
	if (kind != -2) {
	    g95_error("Real number at %C has a 'q' exponent and an "
		      "explicit kind");
	    goto cleanup;
	}

	kind = g95_option.q_kind;
	break;

    default:
	if (kind == -2)
	    kind = g95_default_real_kind(1);

	if (g95_validate_kind(BT_REAL, kind) == -1) {
	    g95_error("Invalid real kind %d at %C", kind);
	    goto cleanup;
	}
    }

    *result = g95_convert_real(buffer, kind, &where);
    return MATCH_YES;

cleanup:
    g95_free_expr(e);
    return MATCH_ERROR;
}



/* match_substring()-- Match a substring reference associated with a
 * constant string. */

static match match_substring(g95_expr *e) {
g95_expr *start, *end;
g95_locus old_loc;
g95_ref *ref;
match m;

    start = NULL;
    end = NULL;

    old_loc = g95_current_locus;

    m = g95_match_char('(');
    if (m != MATCH_YES)
	return MATCH_NO;

    if (g95_match_char(':') != MATCH_YES) {
	m = g95_match_expr(&start);
	if (m != MATCH_YES) {
	    m = MATCH_NO;
	    goto cleanup;
	}

	m = g95_match_char(':');
	if (m != MATCH_YES)
	    goto syntax;
    }

    if (g95_match_char(')') != MATCH_YES) {
	m = g95_match_expr(&end);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	m = g95_match_char(')');
	if (m == MATCH_NO)
	    goto syntax;
    }

/* Optimize away the (:) reference */

    if (start != NULL || end != NULL) {
	ref = g95_extend_ref(e, REF_SUBSTRING, NULL);

	ref->u.ss.start = start;
	ref->u.ss.end = end;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in SUBSTRING specification at %C");
    m = MATCH_ERROR;

cleanup:
    g95_free_expr(start);
    g95_free_expr(end);

    g95_current_locus = old_loc;
    return m;
}



/* g95_next_string_char()-- Reads the next character of a string constant,
 * taking care to return doubled delimiters on the input as a single
 * instance of the delimiter.  Special return values are:
 *   -1   End of the string, as determined by the delimiter
 *   -2   Unterminated string detected
 *
 * Backslash codes are also expanded at this time. */

int g95_next_string_char(char delimiter) {
g95_locus old_locus;
int c;

    c = (unsigned char) g95_next_char_literal(1);
    if (c == '\n')
	return -2;

    if (!g95_option.no_backslash && c == '\\') {
	old_locus = g95_current_locus;

	switch(g95_next_char_literal(1)) {
	case 'a':  c = '\a'; break;
	case 'b':  c = '\b'; break;
	case 't':  c = '\t'; break;
	case 'f':  c = '\f'; break;
	case 'n':  c = '\n'; break;
	case 'r':  c = '\r'; break;
	case 'v':  c = '\v'; break;
	case '\\': c = '\\'; break;

	default:     /* Unknown backslash codes are simply not expanded */
	    g95_current_locus = old_locus;
	    break;
	}
    }

    if (c != delimiter)
	return c;

    old_locus = g95_current_locus;
    c = g95_next_char_literal(0);

    if (c == delimiter)
	return c;

    g95_current_locus = old_locus;
    return -1;
}



/* match_charkind_name()-- Special case of g95_match_name() that
 * matches a parameter kind name before a string constant.  This takes
 * case of the weird but legal case of:
 *             kind_____'string'
 *
 * where kind____ is a parameter. g95_match_name() will happily slurp
 * up all the underscores, which leads to problems.  If we return
 * MATCH_YES, the parse pointer points to the final underscore, which
 * is not part of the name.  We never return MATCH_ERROR-- errors in
 * the name will be detected later. */

static match match_charkind_name(char *name) {
g95_locus old_loc;
char c, peek;
int len;

    g95_gobble_whitespace(); 
    c = g95_next_char();
    if (!g95_varchar(c, 1))
	return MATCH_NO;

    *name++ = c;
    len = 1;

    for(;;) {
	old_loc = g95_current_locus;
	c = g95_next_char();

	if (c == '_') {
	    peek = g95_peek_char();

	    if (peek == '\'' || peek == '\"') {
		g95_current_locus = old_loc;
		*name = '\0';
		return MATCH_YES;
	    }
	}

	if (!g95_varchar(c, 0))
	    break;

	*name++ = c;
	if (++len > G95_MAX_SYMBOL_LEN)
	    break;
    }

    return MATCH_NO;
}



/* match_logical_constant().  Match a .true. or .false. */ 

static match match_logical_constant(g95_expr **result) {
static mstring logical_ops[] = {
    minit(".false.", 0), 
    minit(".true.", 1),
    minit(NULL, -1) };

g95_locus where;
g95_expr *e;
int i, kind;

    g95_gobble_whitespace();
    where = g95_current_locus;

    i = g95_match_strings(logical_ops);
    if (i == -1)
	return MATCH_NO;

    kind = get_kind();
    if (kind == -1) return MATCH_ERROR;
    if (kind == -2) kind = g95_default_logical_kind();

    if (g95_validate_kind(BT_LOGICAL, kind) == -1) {
	g95_error("Bad kind for logical constant at %C");
	return MATCH_ERROR;
    }

    e = g95_get_expr();

    e->type = EXPR_CONSTANT;
    e->value.logical = i;
    e->ts.type = BT_LOGICAL;
    e->ts.kind = kind;
    e->where = where;

    *result = e;
    return MATCH_YES;
}



/* match_hollerith_constant()-- Read a Hollerith constant.  The length
 * and the H have already been matched. */

static void match_hollerith_constant(g95_expr *e) {
int length;
char c, *p;

    p = e->value.character.string;
    length = e->value.character.length;
    e->value.character.hollerith = 1;

    while(length > 0) {
	c = g95_next_char_literal(1);

	if (c == '\n') {
	    memset(p, ' ', length);
	    break;
	}

	*p++ = c;
	length--;
    }
}



/* match_string_constant()-- See if the current input matches a
 * character constant.  Lots of contortions have to be done to match
 * the kind parameter which comes before the actual string.  The main
 * consideration is that we don't want to error out too quickly.  For
 * example, we don't actually do any validation of the kinds until we
 * have actually seen a legal delimiter.  Using match_kind_param()
 * generates errors too quickly. */

static match match_string_constant(g95_expr **result) {
char *p, name[G95_MAX_SYMBOL_LEN+1];
int i, c, kind, length, delimiter;
g95_locus old_locus, start_locus;
g95_symbol *sym;
g95_expr *e;
char *q;
match m;

    old_locus = g95_current_locus;

    g95_gobble_whitespace();

    start_locus = g95_current_locus;

    c = g95_next_char();
    if ((c == '\'' && !G95_STRICT_F()) || c == '"') {
	kind = g95_default_character_kind();
	goto got_delim;
    }

    if (isdigit(c)) {
	kind = 0;

	while(isdigit(c)) {
	    kind = kind*10 + c - '0';
	    if (kind > 99999999)
		goto no_match;
	    c = g95_next_char();
	}

    } else {
	g95_current_locus = old_locus;

	m = match_charkind_name(name);
	if (m != MATCH_YES)
	    goto no_match;

	if (g95_find_symbol(name, NULL, 1, &sym) || sym == NULL ||
	    sym->attr.flavor != FL_PARAMETER)
	    goto no_match;

	kind = -1;
	c = g95_next_char();
    }

    if (c == ' ') { 
	g95_gobble_whitespace();
	c = g95_next_char();
    }

    if (c == 'h') {
	*result = g95_char_expr(kind, g95_default_character_kind(),
				&start_locus);
	match_hollerith_constant(*result);
	return MATCH_YES;
    }

    if (c != '_')
	goto no_match;

    g95_gobble_whitespace();
    start_locus = g95_current_locus;

    c = g95_next_char();

    if (G95_STRICT_F()) {
	if (c != '\'')
	    goto no_match;
    } else {
	if (c != '\'' && c != '"')
	    goto no_match;
    }

    if (kind == -1) {
	q = g95_extract_int(sym->value, &kind);
	if (q != NULL) {
	    g95_error(q);
	    return MATCH_ERROR;
	}
    }

    if (g95_validate_kind(BT_CHARACTER, kind) == -1) {
	g95_error("Invalid kind %d for CHARACTER constant at %C", kind);
	return MATCH_ERROR;
    }

/* Scan the string into a block of memory by first figuring out how
 * long it is, allocating the structure, then re-reading it.  This
 * isn't particularly efficient, but string constants aren't that
 * common in most code.  Someday I'll read more on obstacks. */

got_delim:
    delimiter = c;
    length = 0;

    for(;;) {
	c = g95_next_string_char(delimiter);
	if (c == -1) break;
	if (c == -2) {
	    g95_current_locus = start_locus;
	    g95_error("Unterminated character constant beginning at %C");
	    return MATCH_ERROR;
	}

	length++;
    }

    e = g95_char_expr(length, kind, &start_locus);
    p = e->value.character.string;

    g95_current_locus = start_locus;
    g95_next_char();              /* Skip delimiter */

    for(i=0; i<length; i++)
	*p++ = g95_next_string_char(delimiter);

    *p = '\0';     /* C-style string is for development/debug purposes */

    if (g95_next_string_char(delimiter) != -1)
	g95_internal_error("match_string_constant(): Delimiter not found");

    if (match_substring(e) != MATCH_NO) {
	if (e->ref == NULL)
	    e->type = EXPR_CONSTANT;

	else {
	    e->type = EXPR_SUBSTRING;
	    e->ts.cl = &g95_unknown_charlen;
	    e->ref->u.ss.length = g95_get_charlen(NULL);
	    e->ref->u.ss.length->length =
		g95_int_expr(e->value.character.length);
	}
    }

    *result = e;
    return MATCH_YES;

no_match:
    g95_current_locus = old_locus;
    return MATCH_NO;
}



/* match_sym_complex_part()-- Match a real or imaginary part of a complex
 * constant that is a symbolic constant. */

static match match_sym_complex_part(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_expr *e;
match m;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    if (g95_find_symbol(name, NULL, 1, &sym) || sym == NULL)
	return MATCH_NO;

    if (sym->attr.flavor != FL_PARAMETER) {
	g95_error("Expected PARAMETER symbol in complex constant at %C");
	return MATCH_ERROR;
    }

    if (g95_option.fmode != 0 && g95_option.fmode != 2003) {
	g95_error("PARAMETER in complex constant at %C is an F2003 construct");
	return MATCH_ERROR;
    }

    if (!g95_numeric_ts(&sym->value->ts)) {
	g95_error("Numeric PARAMETER required in complex constant at %C");
	return MATCH_ERROR;
    }

    if (sym->value->rank != 0) {
	g95_error("Scalar PARAMETER required in complex constant at %C");
	return MATCH_ERROR;
    }

    sym->attr.used = 1;

    switch(sym->value->ts.type) {
    case BT_REAL:
	e = g95_copy_expr(sym->value);
	break;

    case BT_COMPLEX:
	e = g95_complex2real(sym->value, sym->value->ts.kind);
	if (e == NULL)
	    goto error;

	break;

    case BT_INTEGER:
	e = g95_int2real(sym->value, g95_default_real_kind(1));
	if (e == NULL)
	    goto error;

	break;

    default:
	g95_internal_error("g95_match_sym_complex_part(): Bad type");
    }

    *result = e;     /* e is a scalar, real, constant expression */
    return MATCH_YES;

error:
    g95_error("Error converting PARAMETER constant in complex constant at %C");
    return MATCH_ERROR;
}



/* match_const_complex_part()-- Match the real and imaginary parts of
 * a complex number.  This subroutine is essentially
 * match_real_constant() modified in a couple of ways: A sign is
 * always allowed and numbers that would look like an integer to
 * match_real_constant() are automatically created as floating point
 * numbers.  The messiness involved with making sure a decimal point
 * belongs to the number and not a trailing operator is not necessary
 * here either (Hooray!). */

static match match_const_complex_part(g95_expr **result) {
int kind, seen_digits, seen_dp, count, digit_flags;
char *p, c, exp_char, *buffer;
g95_locus old_loc;

    old_loc = g95_current_locus; 
    g95_gobble_whitespace();

    seen_dp = 0;
    seen_digits = 0;
    count = 0;
    digit_flags = 0;
    exp_char = ' ';

    c = g95_next_char();
    if (c == '-' || c == '+')
	do {
	    count++;
	    c = g95_next_char();
	} while(g95_is_whitespace(c));

    for(;; c=g95_next_char(), count++) {
	if (c == '.') {
	    if (seen_dp)
		goto no_match;

	    seen_dp = 1;
	    continue;
	}

	if (isdigit((int) c)) {
	    digit_flags |= (seen_dp) ? 2 : 1;
	    seen_digits = 1;
	    continue;
	}

	break;
    }

    if (!seen_digits || (c != 'd' && c != 'e'))
	goto done;

    exp_char = c;

/* scan exponent */

    c = g95_next_char();
    count++;

    if (c == '+' || c == '-') {  /* optional sign */
	c = g95_next_char();
	count++;
    }

    if (!isdigit((int) c)) {
	g95_error("Missing exponent in real number at %C");
	return MATCH_ERROR;
    }

    while(isdigit((int) c)) {
	c = g95_next_char();
	count++;
    }

done:
    if (!seen_digits)
	goto no_match;

    if (G95_STRICT_F()) {
	if (digit_flags != 3) {
	    g95_error("Complex constant at %C must have digits on both sides "
		      "of the decimal point in F mode");
	    return MATCH_ERROR;
	}

	if (exp_char != 'e' && exp_char != ' ') {
	    g95_error("Complex constant at %C must have an 'e' exponent "
		      "in F mode");
	    return MATCH_ERROR;
	}
    }

/* Convert the number */

    g95_current_locus = old_loc;
    g95_gobble_whitespace();

    buffer = alloca(count+1);
    memset(buffer, '\0', count+1);

    p = buffer;
    while(count>0) {
	c = g95_next_char();
	if (c == 'd')
	    c = 'e';   /* Hack for conversion */

	if (!g95_is_whitespace(c))
	    *p++ = c;

	count--;
    }

    *p = '\0';

    kind = get_kind();
    if (kind == -1)
	return MATCH_ERROR;

/* If the number looked like an integer, forget about a kind we may
 * have seen, otherwise validate the kind against real kinds. */

    if (seen_dp == 0 && exp_char == ' ') {
	if (kind != -2 && g95_validate_kind(BT_INTEGER, kind) < 0) {
	    g95_error("Invalid integer kind %d at %C", kind);
	    return MATCH_ERROR;
	}

	kind = g95_default_real_kind(1);
    } else {
	if (exp_char == 'd') {
	    if (kind != -2) {
		g95_error("Real number at %C has a 'd' exponent and an "
			  "explicit kind");
		return MATCH_ERROR;
	    }

	    kind = g95_default_double_kind();
      
	} else if (kind == -2)
	    kind = g95_default_real_kind(1);

	if (g95_validate_kind(BT_REAL, kind) < 0) {
	    g95_error("Invalid real kind %d at %C", kind);
	    return MATCH_ERROR;
	}
    }

    *result = g95_convert_real(buffer, kind, &g95_current_locus);
    return MATCH_YES;

no_match:
    g95_current_locus = old_loc;
    return MATCH_NO;
}



/* match_complex_part()-- Match a real or imaginary part of a complex number */

static match match_complex_part(g95_expr **result) {
match m;

    m = match_sym_complex_part(result);
    if (m != MATCH_NO)
	return m;

    return match_const_complex_part(result);
}



/* match_complex_constant()-- Try to match a complex constant */

static match match_complex_constant(g95_expr **result) {
g95_expr *e, *real, *imag;
g95_locus old_loc, where;
g95_error_buf old_error;
g95_typespec target;
int kind;
match m;

    old_loc = g95_current_locus;
    real = imag = e = NULL;

    m = g95_match_char('(');
    if (m != MATCH_YES)
	return m;

    where = g95_current_locus;

    g95_push_error(&old_error);

    m = match_complex_part(&real);
    if (m == MATCH_NO)
	goto cleanup;

    if (g95_match_char(',') == MATCH_NO) {
	g95_pop_error(&old_error);
	m = MATCH_NO;
	goto cleanup;
    }

/* If m is error, then something was wrong with the real part and we
 * assume we have a complex constant because we've seen the ','.  An
 * ambiguous case here is the start of an iterator list of some sort.
 * These sort of lists are matched prior to coming here. */
  
    if (m == MATCH_ERROR)
	goto cleanup;

    g95_pop_error(&old_error);

    m = match_complex_part(&imag);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    m = g95_match_char(')');
    if (m != MATCH_YES) goto cleanup;

/* Decide on the kind of this complex number */

    if (G95_STRICT_F() && real->ts.type == imag->ts.type &&
	real->ts.kind != imag->ts.kind) {
	g95_error("Real and imaginary parts of complex constant at %C must be "
		  "the same type and kinds");
	m = MATCH_ERROR;
	goto cleanup;
    }

    kind = g95_kind_max(real, imag);
    target.type = BT_REAL;
    target.kind = kind;

    if (kind != real->ts.kind)
	g95_convert_type(real, &target, 1);

    if (kind != imag->ts.kind)
	g95_convert_type(imag, &target, 1);

    e = g95_convert_complex(real, imag, kind);
    e->where = where;

    g95_free_expr(real);
    g95_free_expr(imag);

    *result = e;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in COMPLEX constant at %C");
    m = MATCH_ERROR;

cleanup:
    g95_free_expr(e);
    g95_free_expr(real);
    g95_free_expr(imag);
    g95_current_locus = old_loc;

    return m;
}



/* g95_match_literal_constant()-- Match constants in any of several
 * forms.  Returns nonzero for a match, zero for no match. */

match g95_match_literal_constant(g95_expr **result, int signflag) {
match m;

    m = match_complex_constant(result);
    if (m != MATCH_NO)
	return m;

    m = match_string_constant(result);
    if (m != MATCH_NO)
	return m;

    m = match_boz_constant(result);
    if (m != MATCH_NO)
	return m;

    m = match_real_constant(result, signflag);
    if (m != MATCH_NO)
	return m;

    m = match_integer_constant(result, signflag);
    if (m != MATCH_NO)
	return m;

    m = match_logical_constant(result);
    if (m != MATCH_NO)
	return m;

    return MATCH_NO;
}



/* ha_symbol()-- Given a name, look for it in parent scopes, stopping
 * when required.  Creates a new symbol in the current namespace if
 * nothing is found. */

static g95_symbol *ha_symbol(char *name) {
g95_state_data *comp;
g95_namespace *ns;
g95_symbol *sym;
int import;

    ns = g95_current_ns;
    comp = g95_state_stack;
    import = g95_current_ns->import;

    for(;;) {
	if (g95_find_symbol(name, ns, 0, &sym)) {
	    g95_error("Symbol '%s' at %C is defined in multiple modules",
		      name);
	    return NULL;
	}

	if (sym != NULL && g95_local_symbol(sym))
	    break;

	ns = ns->parent;
	if (ns == NULL)
	    goto unknown;

	import |= ns->import;

    loop:
	comp = comp->previous;
	switch(comp->state) {
	case COMP_INTERFACE:
	    if (import)
		goto loop;

	    goto unknown;

	case COMP_SUBROUTINE:  case COMP_FUNCTION:   case COMP_BLOCK_DATA:
	case COMP_NONE:        case COMP_PROGRAM:    case COMP_MODULE:
	    break;

	case COMP_FORALL:   case COMP_WHERE:  case COMP_CONTAINS:
	case COMP_DERIVED:  case COMP_IF:     case COMP_SELECT:
	case COMP_DO:
	    goto loop;

	default:
	    g95_internal_error("ha_symbol(): Unexpected state");
	    break;
	}
    }

    return sym;

unknown:
    g95_get_symbol(name, NULL, &sym);
    return sym;
}



/* compiling()-- Return nonzero if we are currently compiling the
 * given symbol.  This is used for determining if a function name is
 * its own result variable. */

static int compiling(g95_symbol *sym) {
g95_namespace *ns;
g95_symbol *s;

    for(ns=g95_current_ns; ns; ns=ns->parent)
	if (!sym->attr.entry) {
	    if (ns->proc_name == sym)
		return 1;

	} else {
	    s = NULL;
	    g95_find_symbol(sym->name, ns, 0, &s);

	    if (s == sym)
		return 1;
	}

    return 0;
}



/* get_rvalue_type()-- Given the name of an rvalue, figure out what
 * kind it is, peeking into host scopes if necessary. */

static rvalue_type get_rvalue_type(char *name, g95_symbol **s, int local) {
g95_symbol *sym;
rvalue_type rv;

    if (local)
	g95_get_symbol(name, NULL, &sym);
    else
	sym = ha_symbol(name);

    *s = sym;

    if (sym == NULL)
	return RVAL_ERROR;

    g95_save_symbol_data(sym);

    switch(sym->attr.flavor) {
    case FL_PROCEDURE:
	if (sym->attr.subroutine) {
	    rv = RVAL_SUBROUTINE;
	    break;
	}

	if (sym->attr.proc == PROC_ST_FUNCTION) {
	    rv = RVAL_FUNCTION;
	    break;
	}

	rv = (sym->attr.function && sym->result == sym && compiling(sym))
	    ? RVAL_VARIABLE
	    : RVAL_FUNCTION;

	break;

	/* Fall through */

    default:
    error:
	g95_error("Symbol '%s' at %C is not appropriate for a primary "
		  "expression", name);
	rv = RVAL_ERROR;
	break;

    case FL_VARIABLE:
	if (sym->ts.type == BT_PROCEDURE) {
	    if (sym->attr.subroutine)
		goto error;

	    return RVAL_PROC;
	}

	/* Fall through */

    case FL_PARAMETER:
	rv = RVAL_VARIABLE;
	break;

    case FL_UNKNOWN:
	if (sym->attr.external || sym->attr.intrinsic) {
	    rv = RVAL_FUNCTION;
	    break;
	}

	if (sym->as != NULL) {
	    rv = RVAL_VARIABLE;
	    break;
	}

	rv = RVAL_UNKNOWN;
	break;

    case FL_DERIVED:
	rv = RVAL_DERIVED;
	break;
    }

    return rv;
}



/* single_name_arg()-- Given an actual argument that is a single name,
 * figure out what to do with it.  The name can potentially be a dummy
 * procedure that we only find out about later. */

static match single_name_arg(char *name, g95_expr **result) {
g95_symbol *sym;
g95_typespec ts;
int expr_type;
g95_expr *e;

    g95_clear_ts(&ts); 
    expr_type = 0;

    switch(get_rvalue_type(name, &sym, 0)) {
    case RVAL_VARIABLE:
    case RVAL_PROC:
	expr_type = EXPR_VARIABLE;
	if (sym->ts.type != BT_UNKNOWN)
	    ts = sym->ts;

	break;

    case RVAL_FUNCTION:
    case RVAL_SUBROUTINE:
	expr_type = EXPR_VARIABLE;
	ts.type = BT_PROCEDURE;
	break;

    case RVAL_DERIVED:
	g95_error("Actual argument '%s' at %C is a derived type name", name);
	/* Fall through */

    case RVAL_ERROR:
	return MATCH_ERROR;

    case RVAL_UNKNOWN:
	expr_type = EXPR_UNKNOWN;
	if (sym->ts.type != BT_UNKNOWN)
	    ts = sym->ts;
	break;
    }

    e = g95_get_expr();
    e->symbol = sym;
    e->type = expr_type;
    e->ts = ts;
    e->where = g95_def_locus;

    if (sym->as == NULL)
	e->rank = -1;

    else {
	e->ref = g95_full_ref(&g95_def_locus);
	e->rank = sym->as->rank;
    }

    *result = e;
    return MATCH_YES;
}



/* match_actual_arg0()-- match a single actual argument.  An expression
 * consisting of a single name is a very special case handled elsewhere */

static match match_actual_arg0(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_locus where, w;
int c;

    where = g95_current_locus;

    switch(g95_match_name(name)) {
    case MATCH_ERROR:
	return MATCH_ERROR;

    case MATCH_NO:
	break;

    case MATCH_YES:
	w = g95_current_locus;
	g95_gobble_whitespace();
	c = g95_next_char();
	g95_current_locus = w;

	if (c != ',' && c != ')')
	    break;

	return single_name_arg(name, result);
    }

    g95_current_locus = where;
    return g95_match_expr(result);
}



/* match_actual_arg()-- Match a single actual argument. */

static match match_actual_arg(g95_actual_arglist *a) {
match m;

    if (g95_match(" %%val (") == MATCH_YES)
	a->cb = CB_VALUE;

    else if (g95_match(" %%ref (") == MATCH_YES)
	a->cb = CB_REFERENCE;

    m = match_actual_arg0(&a->u.expr);
    if (m != MATCH_YES)
	return m;

    if (a->cb != CB_NONE) {
	m = g95_match_char(')');
	if (m != MATCH_YES) {
	    g95_error("Syntax error in actual argument at %C");
	    m = MATCH_ERROR;
	}
    }

    return m;
}



/* match_keyword_arg()-- Match a keyword argument. */

static match match_keyword_arg(g95_actual_arglist *actual,
			       g95_actual_arglist *base) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_actual_arglist *a;
g95_locus name_locus;
match m;

    name_locus = g95_current_locus;
    m = g95_match_name(name);

    if (m != MATCH_YES)
	goto cleanup;

    if (g95_match_char('=') != MATCH_YES) {
	m = MATCH_NO;
	goto cleanup;
    }

    m = match_actual_arg(actual);
    if (m != MATCH_YES)
	goto cleanup;

    actual->type = ARG_EXPR;

    /* Make sure this name has not appeared yet */

    if (name[0] != '\0') {
	for(a=base; a; a=a->next)
	    if (g95_strcmp(a->name, name) == 0) {
		g95_error("Keyword '%s' at %C has already appeared in the "
			  "current argument list", name);
		return MATCH_ERROR;;
	    }
    }

    actual->name = g95_get_string(name);
    return MATCH_YES;

cleanup:
    g95_current_locus = name_locus;
    return m;
}



/* g95_match_actual_arglist()-- Matches an actual argument list of a
 * function or subroutine, from the opening parenthesis to the closing
 * parenthesis.  The argument list is assumed to allow keyword
 * arguments because we don't know if the symbol associated with the
 * procedure has an implicit interface or not.  We make sure keywords
 * are unique. */

match g95_match_actual_arglist(int sub_flag, int st_function,
			       g95_actual_arglist **argp) {
g95_actual_arglist *head, *tail;
g95_st_label *label;
g95_locus old_loc;
int seen_keyword;
match m;

    *argp = tail = NULL;
    old_loc = g95_current_locus;

    seen_keyword = 0;

    if (g95_match_char('(') == MATCH_NO)
	return (sub_flag) ? MATCH_YES : MATCH_NO;

    if (g95_match_char(')') == MATCH_YES)
	return MATCH_YES;

    head = NULL;

    for(;;) {
	if (head == NULL)
	    head = tail = g95_get_actual_arglist();

	else {
	    tail->next = g95_get_actual_arglist();
	    tail = tail->next;
	}

	g95_gobble_whitespace();
	tail->start = g95_current_locus;

	if (sub_flag && !G95_STRICT_F() && g95_match_char('*') == MATCH_YES) {
	    m = g95_match_st_label(&label, 0);
	    if (m == MATCH_NO)
		g95_error("Expected alternate return label at %C");

	    if (m != MATCH_YES)
		goto cleanup;

	    tail->u.label = label;
	    tail->type = ARG_ALT_RETURN;
	    goto next;
	}

	/* After the first keyword argument is seen, the following
	 * arguments must also have keywords. */

	if (seen_keyword) {
	    m = match_keyword_arg(tail, head);

	    if (m == MATCH_ERROR) goto cleanup;
	    if (m == MATCH_NO) {
		g95_error("Missing keyword name in actual argument list "
			  "at %C");
		goto cleanup;
	    }

	} else {   /* See if we have the first keyword argument */
	    m = match_keyword_arg(tail, head);
	    if (m == MATCH_YES) {
		seen_keyword = 1;

		if (st_function) {
		    g95_error("Keyword arguments not allow in statement "
			      "function call at %C");
		    goto cleanup;
		}
	    }

	    if (m == MATCH_ERROR)
		goto cleanup;

	    if (m == MATCH_NO) {  /* Try for a non-keyword argument */
		m = match_actual_arg(tail);
		tail->type = ARG_EXPR;

		if (m == MATCH_ERROR) goto cleanup;
		if (m == MATCH_NO) goto syntax;
	    }
	}

    next:
	tail->end = g95_current_locus;

	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    *argp = head;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in argument list at %C");

cleanup:
    g95_free_actual_arglist(head);
    g95_current_locus = old_loc;

    return MATCH_ERROR;
}



/* g95_match_structure_constructor()-- Match a structure constructor.
 * The initial symbol has already been seen. */

match g95_match_structure_constructor(g95_symbol *sym, g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_constructor *head, *tail;
g95_component *comp;
g95_locus where, w;
int c, seen_kw;
g95_annot *a;
g95_expr *e;
match m;

    head = tail = g95_get_constructor();

    if (g95_match_char('(') != MATCH_YES)
	goto syntax;

    where = g95_def_locus;

    if (sym->component_access == ACCESS_PRIVATE && sym->attr.use_assoc) {
	g95_error("Components of structure constructor '%s' at %L are PRIVATE",
		  sym->name, &where);
	goto cleanup;
    }

    if (sym->components == NULL)
	return MATCH_NO;

    for(comp=sym->components->next; comp; comp=comp->next) {
	tail->next = g95_get_constructor();
	tail = tail->next;
    }

    comp = sym->components;
    seen_kw = 0;
    tail = head;

    g95_gobble_whitespace();
    if (g95_peek_char() == ')') {
	g95_next_char();
	goto null_sc;
    }

    for(;;) {
	w = g95_current_locus;

	if (g95_match_name(name) != MATCH_YES ||
	    g95_match_char('=') != MATCH_YES) {
	    g95_current_locus = w;
      
	    if (seen_kw) {
		g95_error("Component keyword required at %C");
		goto cleanup;
	    }

	    if (tail == NULL) {
		g95_error("Too many elements in structure constructor at %C");
		goto cleanup;
	    }

	} else {  /* Keyword */
	    if (G95_STRICT_F95()) {
		g95_error("Component constructor keywords at %C not allowed "
			  "in %s mode", g95_mode_name());
		goto cleanup;
	    }

	    tail = head;
	    for(comp=sym->components; comp; comp=comp->next, tail=tail->next)
		if (strcmp(name, comp->name) == 0)
		    break;

	    if (comp == NULL) {
		g95_error("Component name '%s' at %C not found", name);
		goto cleanup;
	    }

	    if (tail->expr != NULL) {
		g95_error("Component name '%s' at %C is already defined",
			  name);
		goto cleanup;
	    }

	    seen_kw = 1;
	}
	
	m = g95_match_expr(&tail->expr);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	tail = tail->next;

	g95_gobble_whitespace();
	c = g95_next_char();

	if (c == ')') break;
	if (c != ',') goto syntax;

	comp = comp->next;
    }

    if (!seen_kw && G95_STRICT_F95()) {
	if (tail != NULL) {
	    g95_error("Premature end of structure constructor at %C");
	    goto cleanup;
	}

    } else {
    null_sc:
	comp = sym->components;
	for(tail=head; tail; tail=tail->next, comp=comp->next) {
	    if (tail->expr != NULL)
		continue;

	    if (comp->initializer == NULL &&
		(!comp->allocatable ||
		 (g95_option.fmode != 0 && g95_option.fmode <= 2003))) {
		    g95_error("Derived type '%s' does not have a default "
			      "initializer for component '%s' at %C",
			      sym->name, comp->name);
		    goto cleanup;
	    }

	    tail->expr = g95_copy_expr(comp->initializer);
	}
    }

    g95_set_usage(sym, &sym->declared_at, 0, 1);

    a = g95_annotate(ANNOT_DERIVED, &where);
    a->u.sym = sym;

    e = g95_get_expr();

    e->type = EXPR_STRUCTURE;
    e->ts.type = BT_DERIVED;
    e->ts.derived = sym;
    e->symbol = sym;

    e->where = where;
    e->value.constructor.c = head;

    *result = e;
    return MATCH_YES;  

syntax:
    g95_error("Syntax error in structure constructor at %C");

cleanup:
    g95_free_constructor(head);
    return MATCH_ERROR;
}



/* g95_pointer_expr()-- Return nonzero if the expression amounts to a
 * fortran pointer (not C_PTR). */

int g95_pointer_expr(g95_expr *e) {
g95_symbol *sym;
g95_ref *ref;
int pointer;

    switch(e->type) {
    case EXPR_NULL:
	return 1;

    case EXPR_VARIABLE:
	break;

    case EXPR_OP:         case EXPR_CONSTANT:  case EXPR_SUBSTRING:
    case EXPR_STRUCTURE:  case EXPR_ARRAY:     case EXPR_PROCEDURE:
	return 0;

    case EXPR_FUNCTION:
	if (e->value.function.isym != NULL)
	    return e->value.function.isym->id == G95_ISYM_NULL;

	if (e->value.function.pointer != NULL) {
	    sym = e->value.function.pointer->symbol;
	    return sym->ts.interface->attr.pointer;
	}

	if (e->symbol != NULL)
	    return e->symbol->attr.pointer;

	return 0;

    default:
	g95_internal_error("g95_pointer_expr(): Bad expression");
    }

    pointer = e->symbol->attr.pointer;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type != AR_FULL)
		pointer = 0;

	    break;

	case REF_SUBSTRING:
	case REF_COARRAY:
	    pointer = 0;
	    break;

	case REF_COMPONENT:
	    pointer = (ref->u.c.component == NULL)
		? 0 
		: ref->u.c.component->pointer;
	    break;
	}

    return pointer;
}



/* g95_proc_pointer_expr()-- Return nonzero if the expression amount
 * to a procedure pointer. */

int g95_proc_pointer_expr(g95_expr *e) {
int proc_pointer;
g95_symbol *sym;
g95_ref *ref;

    switch(e->type) {
    case EXPR_NULL:
	return 1;

    case EXPR_VARIABLE:
	break;

    case EXPR_OP:         case EXPR_CONSTANT:  case EXPR_SUBSTRING:
    case EXPR_STRUCTURE:  case EXPR_ARRAY:     case EXPR_PROCEDURE:
	return 0;

    case EXPR_FUNCTION:
	if (e->value.function.isym != NULL)
	    return e->value.function.isym->id == G95_ISYM_NULL;

	if (e->value.function.pointer != NULL) {
	    sym = e->value.function.pointer->symbol;
	    return sym->ts.interface->attr.pointer &&
		sym->ts.type == BT_PROCEDURE;
	}

	if (e->symbol != NULL)
	    return e->symbol->attr.pointer && e->ts.type == BT_PROCEDURE;

	return 0;

    default:
	g95_internal_error("g95_proc_pointer_expr(): Bad expression");
    }

    proc_pointer = e->symbol->attr.pointer && e->ts.type == BT_PROCEDURE;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type != AR_FULL)
		proc_pointer = 0;

	    break;

	case REF_SUBSTRING:
	case REF_COARRAY:
	    proc_pointer = 0;
	    break;

	case REF_COMPONENT:
	    proc_pointer = (ref->u.c.component == NULL)
		? 0 
		: ref->u.c.component->pointer &&
		  ref->u.c.component->ts.type == BT_PROCEDURE;
	    break;
	}

    return proc_pointer;
}



/* g95_target_expr()-- Return nonzero if the expression amounts to a
 * target. */

int g95_target_expr(g95_expr *e) {
g95_ref *ref;
int target;

    if (e->type != EXPR_VARIABLE)
	return 0;

    target = e->symbol->attr.target;

    if (e->symbol->attr.pointer)
	target = 1;

    for(ref=e->ref; ref; ref=ref->next)
	if (ref->type == REF_COMPONENT && ref->u.c.component->pointer)
	    target = 1;

    return target;
}



/* g95_allocatable_expr()-- Return nonzero if the expression is
 * ALLOCATABLE. */

int g95_allocatable_expr(g95_expr *e) {
int allocatable;
g95_ref *ref;

    switch(e->type) {
    case EXPR_VARIABLE:
	break;

    case EXPR_FUNCTION:
	return (e->value.function.isym != NULL)
	    ? 0
	    : e->symbol->result->attr.allocatable;

    default:
	return 0;
    }

    allocatable = e->symbol->attr.allocatable;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_COMPONENT:
	    allocatable |= ref->u.c.component->allocatable;
	    break;

	case REF_SUBSTRING:
	case REF_COARRAY:
	    allocatable = 0;
	    break;

	case REF_ARRAY:
	    allocatable &= (ref->u.ar.type == AR_FULL ||
			    ref->u.ar.type == AR_SECTION);
	    break;
	}

    return allocatable;
}



/* g95_extend_ref()-- Extend the reference list by one element. */

g95_ref *g95_extend_ref(g95_expr *primary, int type, g95_locus *where) {
g95_ref *tail;

    if (primary->ref == NULL)
	primary->ref = tail = g95_get_ref();

    else {
	tail = primary->ref;
	while(tail->next != NULL)
	    tail = tail->next;

	tail->next = g95_get_ref();
	tail->next->where = g95_current_locus;
	tail = tail->next;
    }

    tail->type = type;
    tail->where = (where != NULL) ? *where : g95_current_locus;

    return tail;
}



/* pointer_result()-- A variable expression just became a function
 * call. */

static try pointer_result(g95_expr *base, g95_expr **result) {
g95_actual_arglist *actual;
g95_expr *e;
match m;

    if (base->ts.interface == NULL) {
	*result = base;
	return MATCH_YES;
    }

    if (base->ts.interface->attr.subroutine)
	*result = base;

    else {
	m = g95_match_actual_arglist(0, 0, &actual);
	if (m != MATCH_YES)
	    return m;

	e = g95_get_expr(); 
	e->type = EXPR_FUNCTION;
	e->value.function.pointer = base;
	e->where = g95_current_locus;
	e->value.function.actual = actual;

	e->ts = base->ts.interface->ts;
	*result = e;
    }

    return MATCH_YES;
}



/* match_variable_part()-- Match the parts of a variable that come
 * after the initial name. */

static match match_variable_part(g95_symbol *sym, g95_expr **result,
				 int paren_ok) {
char name[G95_MAX_SYMBOL_LEN+1];
int array_flag;
g95_component *c;
g95_ref *ref;
g95_expr *e;
match m;

    e = g95_get_expr();

    e->type   = EXPR_VARIABLE;
    e->symbol = sym;
    e->where  = g95_current_locus;
    e->ts     = sym->ts;

    array_flag = (sym->as != NULL);

    paren_ok |= (sym->as != NULL || sym->ts.type == BT_CHARACTER ||
		 (sym->ts.type == BT_UNKNOWN &&
		  g95_get_default_type(sym, NULL)->type == BT_CHARACTER));

    if (sym->ts.type == BT_UNKNOWN)
	e->ts = *g95_get_default_type(sym, NULL);

loop:
    g95_gobble_whitespace();
    switch(g95_peek_char()) {
    case '(':
	if (e->ts.type == BT_PROCEDURE)
	    return pointer_result(e, result);

	if (!paren_ok) {
	    g95_free_expr(e);
	    g95_error("Unexpected array reference at %C");
	    return MATCH_ERROR;
	}

	if (!array_flag && e->ts.type == BT_CHARACTER)
	    m = match_substring(e);

	else {
	    ref = g95_extend_ref(e, REF_ARRAY, NULL);
	    m = g95_match_array_ref(&ref->u.ar, 0);
	}

	paren_ok = array_flag && e->ts.type == BT_CHARACTER; /*substring refs*/
	array_flag = 0;

	if (m == MATCH_YES)
	    goto loop;

	goto cleanup;

    case '[':
	for(ref=e->ref; ref; ref=ref->next)
	    if (ref->type == REF_COARRAY)
		break;

	if (ref != NULL) {
	    g95_error("Second coarray reference at %C");
	    m = MATCH_ERROR;
	    goto cleanup;
	}

	ref = g95_extend_ref(e, REF_COARRAY, NULL);

	m = g95_match_coarray_ref(&ref->u.car);
	if (m == MATCH_ERROR)
	    goto cleanup;

	if (m == MATCH_NO) {
	    g95_error("Missing coarray reference at %C");
	    m = MATCH_ERROR;
	    goto cleanup;
	}

	goto loop;

    case '%':
	g95_next_char();
	ref = g95_extend_ref(e, REF_COMPONENT, NULL);

	m = g95_match_name(name);
	switch(m) {
	case MATCH_ERROR:
	    goto cleanup;

	case MATCH_NO:
	    g95_error("Missing component name at %C");
	    m = MATCH_ERROR;
	    goto cleanup;

	case MATCH_YES:
	    ref->u.c.name = g95_get_string(name);
	    array_flag = 0;

	    if (e->ts.type == BT_DERIVED) {
		if (!e->ts.derived->attr.set) {
		    g95_error("TYPE '%s' at %C has not been declared yet",
			      e->ts.derived->name);
		    m = MATCH_ERROR;
		    goto cleanup;
		}

		for(c=e->ts.derived->components; c; c=c->next)
		    if (strcmp(ref->u.c.name, c->name) == 0)
			break;

		if (c == NULL) {
		    g95_error("Unknown component name '%s' at %C",
			      ref->u.c.name);
		    m = MATCH_ERROR;
		    goto cleanup;
		}

		e->ts = c->ts;
		array_flag = c->dimension;
		ref->u.c.component = c;
	    }

	    paren_ok = array_flag || e->ts.type == BT_CHARACTER;
	    goto loop;
	}

    default:
	break;
    }

    switch(sym->attr.flavor) {
    case FL_VARIABLE:
    case FL_PARAMETER:
	break;

    case FL_PROCEDURE:
	if (sym->attr.function && sym->result == sym)
	    break;

	/* Fall through to error */

    default:
	if (g95_add_flavor(&sym->attr,
			   FL_VARIABLE, sym->name, NULL) == FAILURE) {
	    m = MATCH_ERROR;
	    goto cleanup;
	}
    }

    if (array_flag) {
	ref = g95_extend_ref(e, REF_ARRAY, &e->where);
	ref->u.ar.type = AR_FULL;
    }

    *result = e;
    return MATCH_YES;

cleanup:
    g95_free_expr(e);
    return m;
}



/* match_function_call()-- Match a function reference */

static match match_function_call(char *name, g95_symbol *sym,
				 g95_expr **result) {
g95_actual_arglist *actual;
g95_locus where;
g95_expr *e;
int flag;
match m;

    if ((sym->attr.flavor != FL_VARIABLE || sym->ts.type != BT_PROCEDURE) &&
	g95_add_function(&sym->attr, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    where = g95_current_locus;

    flag = sym->attr.proc == PROC_ST_FUNCTION;

    m = g95_match_actual_arglist(0, flag, &actual);

    switch(m) {
    case MATCH_YES:
	e = g95_get_expr();
	e->type = EXPR_FUNCTION;
	e->where = where;

	if (sym->ts.type != BT_PROCEDURE) 
	    e->value.function.name = g95_get_string(name);

	else
	    e->value.function.pointer = g95_get_variable_expr(sym, NULL);

	e->value.function.actual = actual;
	e->rank = -1;

	if (sym->result == NULL)
	    sym->result = sym;

	*result = e;
	break;

    case MATCH_NO:
	g95_error("Missing actual argument list in function call to '%s' "
		  "at %C", sym->name);
	m = MATCH_ERROR;
	break;

    case MATCH_ERROR:
	break;
    }

    return m;
}



/* match_aa_rest()-- Match an actual argument list of a function where
 * the first argument has already been read. */

static match match_aa_rest(g95_expr *first, call_by cb,
			   g95_actual_arglist **argp) {
g95_actual_arglist *head, *tail;
g95_locus old_loc;
int seen_keyword;
match m;
int c;

    old_loc = g95_current_locus;

    seen_keyword = 0;

    head = tail = g95_get_actual_arglist();
    head->type = ARG_EXPR;
    head->u.expr = first;
    head->cb = cb;

    for(;;) {
	g95_gobble_whitespace();
	c = g95_next_char();
	if (c == ')') break;
	if (c != ',') goto syntax;

	tail->next = g95_get_actual_arglist();
	tail = tail->next;

	/* After the first keyword argument is seen, the following
	 * arguments must also have keywords. */

	if (seen_keyword) {
	    m = match_keyword_arg(tail, head);

	    if (m == MATCH_ERROR) goto cleanup;
	    if (m == MATCH_NO) {
		g95_error("Missing keyword name in actual argument list "
			  "at %C");
		goto cleanup;
	    }

	} else {   /* See if we have the first keyword argument */

	    m = match_keyword_arg(tail, head);
	    if (m == MATCH_YES) seen_keyword = 1;
	    if (m == MATCH_ERROR) goto cleanup;

	    if (m == MATCH_NO) {  /* Try for a non-keyword argument */
		m = match_actual_arg(tail);
		tail->type = ARG_EXPR;

		if (m == MATCH_ERROR) goto cleanup;
		if (m == MATCH_NO) goto syntax;
	    }
	}
    }

    *argp = head;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in argument list at %C");

cleanup:
    g95_free_actual_arglist(head);
    g95_current_locus = old_loc;

    return MATCH_ERROR;
}



/* match_function_rest()-- Match the rest of a function call where the
 * first argument has already been matched. */

static match match_function_rest(char *name, g95_symbol *sym, g95_locus *where,
				 g95_expr *first, call_by cb,
				 g95_expr **result) {
g95_actual_arglist *actual;
g95_expr *e;
match m;

    if (g95_add_function(&sym->attr, sym->name, where) == FAILURE) {
	g95_free_expr(first);
	return MATCH_ERROR;
    }

    if (sym->result == NULL)
	sym->result = sym;

    m = match_aa_rest(first, cb, &actual);
    if (m == MATCH_ERROR) return m;
    if (m == MATCH_NO) abort();

    e = g95_get_expr();
    e->type = EXPR_FUNCTION;
    e->where = *where;
    e->rank = -1;

    e->value.function.name = g95_get_string(name);
    e->value.function.actual = actual;

    *result = e;
    return MATCH_YES;
}



/* match_substring_rest()-- Match the trailing part of a substring.  A
 * colon is guaranteed to be next.  */

static match match_substring_rest(g95_symbol *sym, g95_locus *where,
				  g95_expr *start, g95_expr **result) {
g95_expr *e, *end;
g95_ref *ref;
match m;

    g95_next_char(); 
    end = NULL;

    g95_gobble_whitespace();
    if (g95_peek_char() != ')') {
	m = g95_match_expr(&end);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

    if (g95_match_char(')') != MATCH_YES)
	goto syntax;

    e = g95_get_expr();
    e->type = EXPR_VARIABLE;
    e->symbol = sym;
    e->where = *where;
    e->ts.type = BT_CHARACTER;
    e->ts.cl = &g95_unknown_charlen;

    e->ref = ref = g95_get_ref();

    ref->type = REF_SUBSTRING;
    ref->u.ss.start = start;
    ref->u.ss.end = end;
    ref->where = *where;

    *result = e;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in substring reference at %C");

cleanup:
    if (start != NULL)
	g95_free_expr(start);

    if (end   != NULL)
	g95_free_expr(end);

    return MATCH_ERROR;
}



/* match_ss_fc()-- Match a substring reference or function call,
 * figuring out what we have in the process.  The colon is mandatory
 * in the substring and cannot appear in the function argument list.
 * A keyword argument also indicates a function call.  A left paren is
 * next on the input. */

static match match_ss_fc(char *fname, g95_symbol *sym, g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_actual_arglist a;
g95_locus where;
match m;
int c;

    where = g95_current_locus; 
    g95_match_char('(');

    g95_gobble_whitespace();
    c = g95_peek_char() == ')' ||
	(g95_match_name(name) == MATCH_YES &&
	 g95_match_char('=') == MATCH_YES);

    g95_current_locus = where;
    if (c)
	return match_function_call(fname, sym, result);

    g95_match_char('(');
    g95_gobble_whitespace();

    if (g95_peek_char() == ':') {
	m = match_substring_rest(sym, &where, NULL, result);
	goto done;
    }

    memset(&a, '\0', sizeof(a));
    m = match_actual_arg(&a);

    switch(m) {
    case MATCH_YES:
	break;

    case MATCH_NO:
	g95_error("Syntax error at %C");
	m = MATCH_ERROR;

	/* Fall through */
    case MATCH_ERROR:
	return m;
    }

    g95_gobble_whitespace();

    if (g95_peek_char() != ':')
	return match_function_rest(fname, sym, &where, a.u.expr, a.cb, result);

    m = match_substring_rest(sym, &where, a.u.expr, result);

done:
    if (m == MATCH_YES &&
	g95_add_flavor(&sym->attr, FL_VARIABLE, sym->name, NULL) == FAILURE)
	m = MATCH_ERROR;

    return m;
}



/* g95_match_variable()-- Match a variable, ie something that can be
 * assigned to.  This starts as a symbol, can be a structure
 * component, array reference or substring.  If the symbol has not
 * been previously seen, we assume it is a variable. */

match g95_match_variable(g95_expr **result, int decl_flag, int local,
			 int parameter) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_locus where;
match m;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    where = g95_def_locus;

    switch(get_rvalue_type(name, &sym, local)) {
    case RVAL_VARIABLE:
    case RVAL_PROC:
    case RVAL_UNKNOWN:
	m = match_variable_part(sym, result, decl_flag);
	break;

    case RVAL_ERROR:
	m = MATCH_ERROR;
	break;

    default:
	g95_error("Expected '%s' at %L to be a VARIABLE", name, &where);
	m = MATCH_ERROR;
    }

    if (m == MATCH_YES) {
	g95_variable_rank(*result);

	if (!parameter && (*result)->symbol->attr.flavor == FL_PARAMETER) {
	    g95_error("Expected '%s' at %L to be a VARIABLE", name, &where);
	    m = MATCH_ERROR;
	}
    }

    if (m == MATCH_YES)
	(*result)->where = where;

    return m;
}



/* g95_match_rvalue()-- Matches a variable name followed by anything
 * that might follow it-- array reference, argument list of a
 * function, etc. */

match g95_match_rvalue(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_locus where;
match m; 

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    where = g95_def_locus;

    switch(get_rvalue_type(name, &sym, 0)) {
    case RVAL_VARIABLE:
	m = match_variable_part(sym, result, 0);
	break;

    case RVAL_FUNCTION:
    case RVAL_PROC:
	m = match_function_call(name, sym, result);
	break;

    case RVAL_DERIVED:
	m = g95_match_structure_constructor(sym, result);
	break;

    case RVAL_UNKNOWN:
	g95_gobble_whitespace();

	if (g95_peek_char() != '(')
	    m = match_variable_part(sym, result, 0);
	else
	    m = match_ss_fc(name, sym, result);

	break;

    case RVAL_SUBROUTINE:
	g95_error("Expected '%s' at %L to be a VARIABLE", name, &where);
	/* Fall through */

    case RVAL_ERROR:
	m = MATCH_ERROR;
	break;
    }

    if (m == MATCH_YES) {
	g95_variable_rank(*result);
	(*result)->where = where;
    }

    return m;
}



/* g95_match_call_expr()-- Match a pointer expression suitable for a
 * CALL statement.  We start with a symbol.  It is also the
 * responsibility of this subroutine to make sure that the final
 * result is callable. */

match g95_match_call_expr(g95_symbol *sym, g95_expr **result) {
g95_expr *e;
match m;

    m = match_variable_part(sym, result, 0);
    if (m != MATCH_YES)
	return m;

    e = *result;
    if (e->ts.type == BT_PROCEDURE)
	return MATCH_YES;

    g95_error("Expression at %C must be CALL-able");
    return MATCH_ERROR;
}



/* alloc_array_part()-- At this point we can have an array element
 * reference or the spec part of an ALLOCATE statement, which are
 * pretty close to one another.  Parse the whole thing into the
 * g95_alloc structure, which is the more general case.  If we're
 * wrong, it's easy to fix.  The leading left paren has already been
 * seen, we parse up to the trailing right paren.  Returns:
 *
 *  -1  if an error was detected and thrown
 *   0  The spec is compatible with a scalar array reference
 *   1  The spec had a range in it and can only be an allocate-spec
 */

static int alloc_array_part(g95_alloc *a, int rank) {
int i, seen_range;
g95_expr *e;
match m;
char c;

   seen_range = 0;
   a->rank = rank;

   for(i=0; i<rank; i++) {
       m = g95_match_expr(&e);
       if (m == MATCH_ERROR)
	   return -1;

       if (m == MATCH_NO)
	   goto syntax;

       if (g95_peek_char() != ':')
	   a->upper[i] = e;

       else {
	   seen_range = 1;
	   a->lower[i] = e;

	   g95_next_char();
	   m = g95_match_expr(&a->upper[i]);
	   if (m == MATCH_NO)
	       goto syntax;

	   if (m == MATCH_ERROR)
	       return -1;
       }

       c = g95_next_char();

       if (i < rank-1) {
	   if (c != ',')
	       goto syntax;
	   
       } else {
	   if (c != ')')
	       goto syntax;
       }
   }

   return seen_range;

syntax:
   g95_error("Syntax error in specification at %C");
   return -1;
}



/* fix_alloc_spec()-- Transform an allocate-spec into an array
 * reference. */

static void fix_alloc_spec(g95_alloc *a) {
g95_ref *ref;
int i, rank;

    ref = g95_extend_ref(a->expr, REF_ARRAY, NULL);

    rank = a->rank;
    a->rank = 0;

    ref->u.ar.type  = AR_ELEMENT;
    ref->u.ar.dimen = rank;

    for(i=0; i<rank; i++) {
	ref->u.ar.start[i] = a->upper[i];
	a->upper[i] = NULL;

	ref->u.ar.dimen_type[i] = DIMEN_ELEMENT;
    }
}



/* match_coarray_alloc()-- Parse a coarray allocation specification.
 * The left square bracket has already been seen.  Returns nonzero on
 * error. */

static int match_coarray_alloc(g95_alloc *a) {
g95_expr *e;
match m;
int i;

    for(i=0; i<a->corank-1; i++) {
	m = g95_match_expr(&e);
	if (m != MATCH_YES) {
	    if (m == MATCH_NO)
		goto syntax;

	    return 1;
	}

	if (g95_match_char(':') == MATCH_NO) {
	    a->lower[a->rank + i] = g95_int_expr(1);
	    a->upper[a->rank + i] = e;

	} else {
	    a->lower[a->rank + i] = e;

	    m = g95_match_expr(&a->upper[a->rank + i]);
	    if (m != MATCH_YES) {
		if (m == MATCH_NO)
		    goto syntax;

		return 1;
	    }
	}

	if (g95_match_char(',') != MATCH_YES) {
	    g95_error("Not enough codimensions in coarray allocation at %C");
	    return 1;
	}
    }

    if (g95_match_char('*') != MATCH_YES) {
	m = g95_match_expr(&a->lower[a->rank + i]);
	if (m != MATCH_YES) {
	    if (m == MATCH_NO)
		goto syntax;

	    return 1;
	}

	if (g95_match_char(':') != MATCH_YES ||
	    g95_match_char('*') != MATCH_YES) {

	    g95_error("Last upper bound of coarray allocation at %C "
		      "must be '*'");
	    return 1;
	}
    }

    if (g95_match_char(']') == MATCH_YES)
	return 0;

syntax:
    g95_error("Syntax error in coarray allocation specification at %C");
    return 1;
}



/* g95_match_alloc_var()-- Match an allocation specification.  This is
 * a special case of variable matching.  Since this is inside an
 * ALLOCATE statement, we already know everything there is to know
 * about the variable and its structure. */

match g95_match_alloc_var(g95_alloc **result) {
int i, allocatable, pointer, rank, seen_array_alloc;
char name[G95_MAX_SYMBOL_LEN+1];
g95_coarray_spec *cas;
g95_array_spec *as;
g95_component *c;
g95_symbol *sym;
g95_alloc *a;
g95_ref *ref;
g95_expr *e;
match m;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    a = g95_get_alloc();

    sym = ha_symbol(name);
    if (sym == NULL)
	goto bad_exp;

    a->expr = e = g95_get_expr();

    e->where  = g95_current_locus;
    e->type   = EXPR_VARIABLE;
    e->symbol = sym;
    e->ts     = sym->ts;

    as  = sym->as;
    cas = sym->cas;

    allocatable = sym->attr.allocatable;
    pointer     = sym->attr.pointer;

    seen_array_alloc = 0;

loop:
    g95_gobble_whitespace();

    switch(g95_peek_char()) {
    case '(':
	g95_next_char();
	rank = (as == NULL) ? 0 : as->rank;

	if (rank == 0) {
	    g95_error("Unexpected array reference at %C");
	    goto cleanup;
	}

	i = alloc_array_part(a, rank);
	as = NULL;

	if (i == -1)
	    goto cleanup;

	seen_array_alloc = 1;
	g95_gobble_whitespace();

	if (g95_peek_char() == '%' && i == 0) {
	    fix_alloc_spec(a);
	    seen_array_alloc = 0;
	}

	goto loop;

    case '%':
	seen_array_alloc = 0;
	g95_next_char();

	if (e->ts.type != BT_DERIVED) {
	    g95_error("Expression preceding '%%' at %C is not a derived type");
	    goto cleanup;
	}

	ref = g95_extend_ref(e, REF_COMPONENT, NULL);

	m = g95_match_name(name);
	if (m != MATCH_YES) {
	    if (m == MATCH_NO)
		g95_error("Missing component name at %C");

	    goto cleanup;
	}

	for(c=e->ts.derived->components; c; c=c->next)
	    if (strcmp(name, c->name) == 0)
		break;

	if (c == NULL) {
	    g95_error("Unknown component name '%s' at %C", name);
	    goto cleanup;
	}

	ref->u.c.component = c;
	ref->u.c.name      = c->name;

	e->ts       = c->ts;
	allocatable = c->allocatable;
	pointer     = c->pointer;
	cas         = c->cas;
	as          = c->as;

	goto loop;

    case '[':
	g95_next_char();

	if (!seen_array_alloc && as != NULL) {
	    g95_error("Coarray specification at %C must follow "
		      "array specification");
	    goto cleanup;
	}

	if (cas == NULL) {
	    g95_error("Expression at %C is not a coarray");
	    goto cleanup;
	}

	a->corank = cas->corank;

	if (match_coarray_alloc(a))
	    goto cleanup;

	cas = NULL;
	break;

    default:   /* Something doesn't fit, we're done. */
	break;
    }

    if (!pointer && !allocatable)
	goto bad_exp;

    if (as != NULL) {
	g95_error("Missing array allocation specification at %C");
	goto cleanup;
    }

    if (cas != NULL) {
	g95_error("Missing coarray allocate specification at %C");
	goto cleanup;
    }

    *result = a;
    return MATCH_YES;

bad_exp:
    g95_error("Expression at %C is not a POINTER or ALLOCATABLE expression");

cleanup:
    g95_free_alloc_list(a);
    return MATCH_ERROR;
}



/* g95_match_dealloc_var()-- Match an deallocation or nullify
 * specification.  This is a special case of variable matching.  Since
 * this is inside a NULLIFY or DEALLOCATE statement, we already know
 * everything there is to know about the variable and its
 * structure. */

match g95_match_dealloc_var(g95_alloc **result) {
char name[G95_MAX_SYMBOL_LEN+1];
int allocatable, pointer, rank;
g95_coarray_spec *cas;
g95_array_spec *as;
g95_component *c;
g95_symbol *sym;
g95_alloc *a;
g95_ref *ref;
g95_expr *e;
match m;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    a = g95_get_alloc();

    sym = ha_symbol(name);
    if (sym == NULL)
	goto bad_exp;

    a->expr = e = g95_get_expr();

    e->where  = g95_current_locus;
    e->type   = EXPR_VARIABLE;
    e->symbol = sym;
    e->ts     = sym->ts;

    as  = sym->as;
    cas = sym->cas;

    allocatable = sym->attr.allocatable;
    pointer     = sym->attr.pointer;

loop:
    g95_gobble_whitespace();

    switch(g95_peek_char()) {
    case '(':
	rank = (as == NULL) ? 0 : as->rank;

	if (rank == 0) {
	    g95_error("Unexpected array reference at %C");
	    goto cleanup;
	}

	ref = g95_extend_ref(e, REF_ARRAY, NULL);

	m = g95_match_array_ref(&ref->u.ar, 0);
	if (m == MATCH_YES)
	    goto loop;

	goto cleanup;

    case '%':
	g95_next_char();

	if (e->ts.type != BT_DERIVED) {
	    g95_error("Expression preceding '%%' at %C is not a derived type");
	    goto cleanup;
	}

	ref = g95_extend_ref(e, REF_COMPONENT, NULL);

	m = g95_match_name(name);
	if (m != MATCH_YES) {
	    if (m == MATCH_NO)
		g95_error("Missing component name at %C");

	    goto cleanup;
	}

	for(c=e->ts.derived->components; c; c=c->next)
	    if (strcmp(name, c->name) == 0)
		break;

	if (c == NULL) {
	    g95_error("Unknown component name '%s' at %C", name);
	    goto cleanup;
	}

	ref->u.c.component = c;
	ref->u.c.name      = c->name;

	e->ts       = c->ts;
	allocatable = c->allocatable;
	pointer     = c->pointer;
	cas         = c->cas;
	as          = c->as;

	goto loop;

    case '[':
	g95_error("Can't reference a remote coarray in DEALLOCATE "
		  "statement at %C");
	goto cleanup;

    default:   /* Something doesn't fit, we're done. */
	break;
    }

    if (cas != NULL)
	a->corank = cas->corank;

    else if (as != NULL)
	a->rank = as->rank;

    else if (!pointer)
	goto bad_exp;

    *result = a;
    return MATCH_YES;

bad_exp:
    g95_error("Expression at %C is not a POINTER or ALLOCATABLE expression");

cleanup:
    g95_free_alloc_list(a);
    return MATCH_ERROR;
}

