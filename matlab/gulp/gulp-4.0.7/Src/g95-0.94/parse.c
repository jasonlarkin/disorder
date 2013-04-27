/* Main parser
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

/* parse.c-- Main parser */

#include <ctype.h>
#include <string.h>
#include <setjmp.h>

#include "g95.h"

/* Current statement label.  NULL means no statement label.  Because
 * new_st can get wiped during statement matching, we have to keep it
 * separate. */

g95_st_label *g95_statement_label;
g95_locus g95_label_locus;
static jmp_buf eof;

g95_state_data *g95_state_stack;
extern g95_locus g95_ts_locus;
extern char *g95_ts_locus_end;
static void parse_where_block(void);

int g95_spec_flag;
g95_locus g95_statement_locus;


/* Macros that expand to case-labels for various classes of
 * statements.  Start with executable statements that directly do
 * things. */

#define case_executable case ST_ALLOCATE: case ST_BACKSPACE: case ST_CALL: \
  case ST_CLOSE: case ST_CONTINUE: case ST_DEALLOCATE: case ST_END_FILE:   \
  case ST_FLUSH: case ST_GOTO: case ST_INQUIRE: case ST_NULLIFY:           \
  case ST_OPEN: case ST_READ: case ST_RETURN: case ST_REWIND:              \
  case ST_SIMPLE_IF: case ST_STOP: case ST_WRITE: case ST_ASSIGNMENT:      \
  case ST_ASSIGN: case ST_POINTER_ASSIGNMENT: case ST_EXIT: case ST_CYCLE: \
  case ST_ARITHMETIC_IF: case ST_WHERE: case ST_FORALL: case ST_PAUSE:     \
  case ST_WAIT: case ST_SYNC_ALL: case ST_SYNC_TEAM: case ST_SYNC_IMAGES:  \
  case ST_SYNC_MEMORY: case ST_ERROR_STOP: case ST_NOTIFY: case ST_QUERY


/* Statements that mark other executable statements */

#define case_exec_markers case ST_DO: case ST_FORALL_BLOCK:                \
  case ST_IF_BLOCK: case ST_WHERE_BLOCK: case ST_SELECT_CASE:              \
  case ST_CRITICAL

/* Declaration statements */

#define case_decl case ST_ATTR_DECL: case ST_COMMON: case ST_DATA_DECL:    \
  case ST_EQUIVALENCE: case ST_NAMELIST: case ST_STATEMENT_FUNCTION:       \
  case ST_TYPE: case ST_INTERFACE

/* Block end statements.  Errors associated with interchanging these
 * are detected in g95_match_end(). */

#define case_end case ST_END_BLOCK_DATA: case ST_END_FUNCTION:             \
                 case ST_END_PROGRAM: case ST_END_SUBROUTINE

typedef struct {
    enum { ORDER_START, ORDER_USE, ORDER_IMPORT, ORDER_IMPLICIT_NONE,
	   ORDER_IMPLICIT, ORDER_SPEC, ORDER_EXEC } state;

    g95_statement last_statement;
    g95_locus where;
} st_state;

static g95_statement parse_executable(g95_statement);
static void parse_progunit(g95_statement);
static g95_statement parse_spec(g95_statement);


/* match_word()-- A sort of half-matching function.  We try to match
 * the word on the input with the passed string.  If this succeeds, we
 * call the keyword-dependent matching function that will match the
 * rest of the statement.  For single keywords, the matching
 * subroutine is g95_match_eos(). */

static match match_word(char *str, match (*subr)(void), g95_locus *old_locus) {
match m;

    if (str != NULL) {
	m = g95_match(str);
	if (m != MATCH_YES)
	    return m;

	new_st.where = g95_current_locus;
	new_st.where.nextc--;
    }

    m = (*subr)();

    if (m != MATCH_YES) {
	g95_current_locus = *old_locus;
	g95_reject_statement();
    }

    return m;
}



/* function_return_type()-- Get a function's type. */

static void function_return_type(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_locus locus;
match m;

    if (g95_current_ns->proc_name == NULL ||
	!g95_current_ns->proc_name->attr.function ||
	g95_ts_locus.nextc == NULL)
	return;

    locus = g95_current_locus;
    m = g95_match("import ");
    g95_current_locus = locus;

    if (m == MATCH_YES)
	return;

    if ((g95_match(" use% %n ", name) != MATCH_YES &&
	 g95_match(" use , intrinsic :: %n ", name) != MATCH_YES) ||
	(g95_match_eos() != MATCH_YES && g95_match_char(',') != MATCH_YES)) {

	sym = g95_current_ns->proc_name;

	if (sym->ts.type != BT_UNKNOWN)
	    g95_error("Function '%s' at %C already has a type of %s", name,
		      g95_basic_typename(sym->ts.type));

	else {
	    g95_current_locus = g95_ts_locus;
	    m = g95_match_type_spec(&sym->result->ts, 1);

	    if (g95_current_locus.nextc[-1] != ')')
		g95_match_space();

	    if (m == MATCH_ERROR)
		g95_error_check();

	    if (m == MATCH_YES && G95_STRICT_F())
		g95_error_now("Function type at %L cannot appear in FUNCTION "
			      "statement in F", &g95_ts_locus);

	    else if (m == MATCH_YES &&
		     g95_current_locus.nextc != g95_ts_locus_end)
		g95_error_now("Syntax error in FUNCTION type at %L",
			      &g95_ts_locus);

	    if (m == MATCH_YES)
		g95_commit_symbols();

	    if (sym->result->ts.type == BT_DERIVED &&
		sym->result->ts.derived == sym)
		g95_error_now("Conflict between PROCEDURE and DERIVED "
			      "attribute in symbol '%s' at %C", sym->name);

	    g95_ts_locus.nextc = NULL;
	}
    }

    g95_current_locus = locus;
}



#define match(keyword, subr, st)   do {  	\
    m = match_word(keyword, subr, &old_locus);  \
    if (m == MATCH_YES) return st; } while(0)


/* decode_statement()-- Figure out what the next statement is,
 * (mostly) regardless of proper ordering */

static g95_statement decode_statement(void) {
g95_locus old_locus;
g95_statement st;
match m;
int c;

#ifdef G95_DEBUG
    g95_symbol_state();
#endif

    g95_clear_error();    /* Clear any stored errors */
    g95_clear_warning();  /* Clear any stored warnings */

    g95_statement_locus = g95_current_locus;

    if (g95_match_eos() == MATCH_YES)
	return ST_NONE;

    function_return_type();

    old_locus = g95_current_locus;

/* Try matching a data declaration or function declaration. The input
 * "REALFUNCTIONA(N)" can mean several things in different contexts,
 * so it (and its relatives) get special treatment. */

    if (g95_current_state() == COMP_NONE ||
	g95_current_state() == COMP_INTERFACE ||
	g95_current_state() == COMP_CONTAINS) {

	m = g95_match_function();
	if (m == MATCH_YES)
	    return ST_FUNCTION;

	if (m == MATCH_ERROR) {
	    g95_error_check();
	    g95_error_recovery();
	    g95_reject_statement();
	    return ST_NONE;
	}

	g95_undo_symbols();
	g95_current_locus = old_locus;
    }

/* Match statements whose error messages are meant to be overwritten
 * by something better */

    match(NULL, g95_match_st_function, ST_STATEMENT_FUNCTION);
    match(NULL, g95_match_assignment, ST_ASSIGNMENT);
    match(NULL, g95_match_pointer_assignment, ST_POINTER_ASSIGNMENT);
    match(NULL, g95_match_data_decl, ST_DATA_DECL);

/* Try to match a subroutine statement, which has the same optional
 * prefixes that functions can have. */

    if (g95_match_subroutine() == MATCH_YES)
	return ST_SUBROUTINE;

    g95_undo_symbols();
    g95_current_locus = old_locus;

/* Check for the IF, DO, SELECT, WHERE and FORALL statements, which
 * might begin with a block label.  The match functions for these
 * statements are unusual in that their keyword is not seen before the
 * matcher is called */

    if (g95_match_if(&st) == MATCH_YES) return st;
    g95_undo_symbols();
    g95_current_locus = old_locus;

    if (g95_match_where(&st) == MATCH_YES) return st;
    g95_undo_symbols();
    g95_current_locus = old_locus;

    if (g95_match_forall(&st) == MATCH_YES) return st;
    g95_undo_symbols();
    g95_current_locus = old_locus;

    match(NULL, g95_match_do, ST_DO);
    match(NULL, g95_match_select, ST_SELECT_CASE);
    match(NULL, g95_match_critical, ST_CRITICAL);

/* General statement matching: Instead of testing every possible
 * statement, we eliminate most possibilities by peeking at the first
 * character. */

    c = g95_peek_char();

    switch(c) {
    case 'a':
	match("abstract% interface",g95_match_abstract_interface,ST_INTERFACE);
	match("all% stop",    g95_match_error_stop,  ST_ERROR_STOP);
	match("allocate",     g95_match_allocate,    ST_ALLOCATE);
	match("allocatable",  g95_match_allocatable, ST_ATTR_DECL);
	match("assign% ",     g95_match_assign,      ST_ASSIGN);
	match("asynchronous", g95_match_async,       ST_ATTR_DECL);
	break;

    case 'b':
	match("backspace",  g95_match_backspace,  ST_BACKSPACE);
	match("bind ( c",   g95_match_bind,       ST_ATTR_DECL);
	match("block data", g95_match_block_data, ST_BLOCK_DATA);
	break;

    case 'c':
	match("call% ",       g95_match_call,         ST_CALL);
	match("case",         g95_match_case,         ST_CASE);
	match("close",        g95_match_close,        ST_CLOSE);
	match("codimension",  g95_match_codimension,  ST_ATTR_DECL);
	match("common",       g95_match_common,       ST_COMMON);
	match("contains",     g95_match_eos,          ST_CONTAINS);
	match("continue",     g95_match_continue,     ST_CONTINUE);
	match("cycle",        g95_match_cycle,        ST_CYCLE);
	break;

    case 'd':
	match("deallocate", g95_match_deallocate, ST_DEALLOCATE);
	match("data",       g95_match_data,       ST_DATA);
	match("dimension",  g95_match_dimension,  ST_ATTR_DECL);
	break;

    case 'e':
	match("end file",    g95_match_endfile,    ST_END_FILE);
	match("error% stop", g95_match_error_stop, ST_ERROR_STOP);
	match("exit",        g95_match_exit,       ST_EXIT);
	match("else",        g95_match_else,       ST_ELSE);
	match("else where",  g95_match_elsewhere,  ST_ELSEWHERE);
	match("else if",     g95_match_elseif,     ST_ELSEIF);

	if (g95_match_end(&st) == MATCH_YES)
	    return st;

	match("entry% ",     g95_match_entry,       ST_ENTRY);
	match("enum",        g95_match_enum,        ST_ENUM);
	match("enumerator",  g95_match_enumerator,  ST_ENUMERATOR);
	match("equivalence", g95_match_equivalence, ST_EQUIVALENCE);
	match("external",    g95_match_external,    ST_ATTR_DECL);
	break;

    case 'f':
	match("format", g95_match_format, ST_FORMAT);

	if (g95_option.fmode == 0 || g95_option.fmode == 2003)
	    match("flush", g95_match_flush, ST_FLUSH);

	break;

    case 'g':
	match("go to", g95_match_goto, ST_GOTO);
	break;

    case 'i':
	match("inquire",        g95_match_inquire,       ST_INQUIRE);
	match("implicit",       g95_match_implicit,      ST_IMPLICIT);
	match("implicit% none", g95_match_implicit_none, ST_IMPLICIT_NONE);

	if (g95_option.fmode == 0 || g95_option.fmode == 2003)
	    match("import", g95_match_import, ST_IMPORT);

	match("interface", g95_match_interface, ST_INTERFACE);
	match("intent",    g95_match_intent,    ST_ATTR_DECL);
	match("intrinsic", g95_match_intrinsic, ST_ATTR_DECL);
	break;

    case 'm':
	match("module% procedure% ", g95_match_modproc, ST_MODULE_PROC);
	match("module",              g95_match_module,  ST_MODULE);
	break;

    case 'n':
	match("nullify",  g95_match_nullify,  ST_NULLIFY);
	match("namelist", g95_match_namelist, ST_NAMELIST);
	match("notify",   g95_match_notify,   ST_NOTIFY);
	break;

    case 'o':
	match("open",     g95_match_open,     ST_OPEN);
	match("optional", g95_match_optional, ST_ATTR_DECL);
	break;

    case 'p':
	match("parameter", g95_match_parameter, ST_PARAMETER);
	match("pause",     g95_match_pause,     ST_PAUSE);
	match("pointer",   g95_match_pointer,   ST_ATTR_DECL);
	match("print",     g95_match_print,     ST_WRITE);

	if (g95_match_private(&st) == MATCH_YES)
	    return st;

	if (!G95_STRICT_F95()) {
	    match("procedure (", g95_match_procedure, ST_DATA_DECL);

	    if (m == MATCH_NO)
		match("procedure",   g95_match_modproc,   ST_MODULE_PROC);
	}

	match("program",   g95_match_program,   ST_PROGRAM);
	match("protected", g95_match_protected, ST_DATA_DECL);

	if (g95_match_public(&st) == MATCH_YES)
	    return st;

	break;

    case 'q':
	match("query",  g95_match_query,  ST_QUERY);
	break;

    case 'r':
	match("read",   g95_match_read,   ST_READ);
	match("return", g95_match_return, ST_RETURN);
	match("rewind", g95_match_rewind, ST_REWIND);
	break;

    case 's':
	match("save", g95_match_save, ST_ATTR_DECL);
	if (!G95_STRICT_F())
	    match("sequence", g95_match_eos, ST_SEQUENCE);

	match("stop",         g95_match_stop,        ST_STOP);
	match("sync% all",    g95_match_sync_all,    ST_SYNC_ALL);
/*	match("sync% team",   g95_match_sync_team,   ST_SYNC_TEAM); */
	match("sync% images", g95_match_sync_images, ST_SYNC_IMAGES);
	match("sync% memory", g95_match_sync_memory, ST_SYNC_MEMORY);
	break;

    case 't':
	if (!G95_STRICT_F())
	    match("target", g95_match_target, ST_ATTR_DECL);

	match("type", g95_match_derived_decl, ST_DERIVED_DECL);
	break;

    case 'u':
	match("use", g95_match_use, ST_USE);
	break;

    case 'v':
	match("value",    g95_match_value,    ST_ATTR_DECL);
	match("volatile", g95_match_volatile, ST_ATTR_DECL);
	break;

    case 'w':
	if (g95_option.fmode == 0 || g95_option.fmode == 2003)
	    match("wait", g95_match_wait, ST_WAIT);

	match("write", g95_match_write, ST_WRITE);
	break;
    }

/* All else has failed, so give up.  See if any of the matchers has
 * stored an error message of some sort. */

    if (g95_error_check() == 0)
	g95_error_now("Unclassifiable statement at %C");

    g95_reject_statement();

    g95_error_recovery();

    return ST_NONE;
}

#undef match


/* zero_label()-- Complain about a zero valued statement label. */

static void zero_label(void) {

    g95_warning_now(106, "Zero is not a valid statement label at %L",
		    &g95_label_locus);
}



/* next_free()-- Get the next statement in free form source */

static g95_statement next_free(void) {
match m;
int c;

    g95_gobble_whitespace();

    c = g95_peek_char();

    if (isdigit(c)) {        /* Found a statement label? */
	m = g95_match_st_label(&g95_statement_label, 0);

	if (m != MATCH_YES || !g95_is_whitespace(g95_peek_char())) {
	    g95_warning_now(103, "Ignoring bad statement label at %C");
	    g95_statement_label = NULL;

	    do
		c = g95_next_char();
	    while(isdigit(c));

	} else {
	    if (g95_statement_label->value == 0)
		zero_label();

	    g95_gobble_whitespace();

	    if (g95_match_eos() == MATCH_YES) {
		g95_warning_now(105, "Ignoring statement label in empty "
				"statement at %C");

		if (g95_statement_label->defined == ST_LABEL_UNKNOWN &&
		    g95_statement_label->referenced == ST_LABEL_UNKNOWN) {
		    g95_free_st_label(g95_statement_label);
		    g95_statement_label = NULL;
		}

		return ST_NONE;
	    }
	}
    }

    return decode_statement();
}



/* next_fixed()-- Get the next statement in fixed-form source */

static g95_statement next_fixed(void) {
int label, digit_flag, i;
g95_locus loc;
char c;

    if (!g95_at_bol())
	return decode_statement();

/* Skip past the current label field, parsing a statement label if one
 * is there.  This is a weird number parser, since the number is
 * contained within five columns and can have any kind of embedded
 * spaces.  We also check for characters that make the rest of the
 * line a comment */

    label = 0;
    digit_flag = 0;

    for(i=0; i<5; i++) {
	if (!digit_flag)
	    g95_label_locus = g95_current_locus;

	c = g95_next_char_literal(0);

	switch(c) {
	case ' ':
	    break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	    label = label*10 + c - '0';
	    digit_flag = 1;
	    break;

/* Comments have already been skipped by the time we get here so don't
 * bother checking for them. */

	default:
	    g95_buffer_error(0);
	    g95_error("Non-numeric character in statement label at %C");
	    return ST_NONE;
	}
    }

    if (digit_flag) {
	if (label == 0)
	    zero_label();
	else   /* We've found a valid statement label.  */
	    g95_statement_label = g95_get_st_label(label);
    }

/* Since this line starts a statement, it cannot be a continuation of
 * a previous statement.  If we see something here besides a space or
 * zero, it must be a bad continuation line/ */

    c = g95_next_char_literal(0);
    if (c == '\n')
	goto blank_line;

    if (c != ' ' && c != '0') {
	g95_buffer_error(0);
	g95_error("Bad continuation line at %C");
	return ST_NONE;
    }

/* Now that we've taken care of the statement label columns, we have
 * to make sure that the first nonblank character is not a '!'.  If it
 * is, the rest of the line is a comment. */

    do { 
	loc = g95_current_locus;
	c = g95_next_char_literal(0);
    } while(g95_is_whitespace(c));

    if (c == '!')
	goto blank_line;

    g95_current_locus = loc;

    if (g95_match_eos() == MATCH_YES)
	goto blank_line;

/* At this point, we've got a nonblank statement to parse. */

    return decode_statement();

blank_line:
    if (digit_flag) g95_warning(107, "Statement label in blank line will be "
				"ignored at %C");
    g95_advance_line();
    return ST_NONE;
}



/* check_statement_label()-- If the current statement has a statement
 * label, make sure that it is allowed to have one. */

static void check_statement_label(g95_statement st) {
g95_sl_type type;

    if (st == ST_NONE || g95_statement_label == NULL ||
	g95_statement_label->value == 0)
	return;

    if (g95_current_state() == COMP_DERIVED || st == ST_INTERFACE ||
	st == ST_DERIVED_DECL) {
	g95_free_st_label(g95_statement_label);
	g95_statement_label = NULL;
	return;
    }

    switch(st) {
    case ST_END_PROGRAM:    case ST_END_FUNCTION:  case ST_END_SUBROUTINE:
    case ST_ENDDO:          case ST_ENDIF:         case ST_END_SELECT:
    case ST_ENTRY:
    case_executable:
    case_exec_markers:
	type = ST_LABEL_TARGET;
	break;

    case ST_FORMAT:
	type = ST_LABEL_FORMAT;
	break;

/* Statement labels are not restricted from appearing on a particular
 * line.  However, there are plenty of situations where the resulting
 * label can't be referenced. */

    default:
	type = ST_LABEL_BAD_TARGET;
	break;
    }

    g95_define_st_label(g95_statement_label, type, &g95_label_locus);
    new_st.here = g95_statement_label;

    if (G95_STRICT_F() && st != ST_CONTINUE)
	g95_error("Statement label in %s statement at %C not permitted in F", 
		  g95_ascii_statement(st));
}



/* next_statement()-- Return the next non-ST_NONE statement to the
 * caller.  We also worry about including files and the ends of
 * include files at this stage */

static g95_statement next_statement(void) {
g95_statement st;

    g95_new_block = NULL;

    for(;;) {
	g95_statement_label = NULL;
	g95_buffer_error(1);

	if (g95_at_eol())
	    g95_advance_line();

	g95_skip_comments();

	if (g95_at_end()) {
	    st = ST_NONE;
	    break;
	}

	st = (g95_current_form == FORM_FIXED)
	    ? next_fixed()
	    : next_free();

	if (st != ST_NONE)
	    break;
    }

    g95_buffer_error(0);
    check_statement_label(st);

    return st;
}



/* push_state()-- Push a new state onto the stack */

static void push_state(g95_state_data *p, g95_compile_state new_state,
		       g95_symbol *sym) {

    p->state = new_state;
    p->previous = g95_state_stack;
    p->sym = sym;
    p->head = NULL;
    p->next = NULL;
    p->do_variable = NULL;

    g95_state_stack = p;
}



/* pop_state()-- Pop the current state */

static void pop_state(void) {

    g95_state_stack = g95_state_stack->previous;
}



/* g95_find_state()-- Try to find the given state in the state stack. */

try g95_find_state(g95_compile_state state) {
g95_state_data *p;

    for(p=g95_state_stack; p; p=p->previous)
	if (p->state == state)
	    break;

    return (p == NULL) ? FAILURE : SUCCESS;
}



/* g95_enclosing_unit()-- Figures out what the enclosing program unit
 * is.  This will be a function, subroutine, program, block data or
 * module. */

g95_state_data *g95_enclosing_unit(g95_compile_state *result) {
g95_state_data *p;

    for(p=g95_state_stack; p; p=p->previous)
	if (p->state == COMP_FUNCTION || p->state == COMP_SUBROUTINE ||
	    p->state == COMP_MODULE || p->state == COMP_BLOCK_DATA ||
	    p->state == COMP_PROGRAM) {

	    if (result != NULL)
		*result = p->state;

	    return p;
	}

    if (result != NULL)
	*result = COMP_PROGRAM;

    return NULL;
}



/* g95_current_block_name()-- Return the name of the current block.
 * Returns a null string if there is no block name. */

char *g95_current_block_name(void) {
g95_symbol *sym;

    sym = g95_current_block();
    return (sym == NULL) ? "" : sym->name;
}



/* g95_ascii_statement()-- Translate a statement enum to a string. */

char *g95_ascii_statement(g95_statement st) {
char *p;

    switch(st) {
    case ST_ARITHMETIC_IF:  p = "arithmetic IF";         break;
    case ST_ALLOCATE:       p = "ALLOCATE";              break;
    case ST_ATTR_DECL:      p = "attribute declaration"; break;
    case ST_BACKSPACE:      p = "BACKSPACE";             break;
    case ST_BLOCK_DATA:     p = "BLOCK DATA";            break;
    case ST_CALL:           p = "CALL";                  break;
    case ST_CASE:           p = "CASE";                  break;
    case ST_CLOSE:          p = "CLOSE";                 break;
    case ST_COMMON:         p = "COMMON";                break;
    case ST_CONTINUE:       p = "CONTINUE";              break;
    case ST_CONTAINS:       p = "CONTAINS";              break;
    case ST_CRITICAL:       p = "CRITICAL";              break;
    case ST_CYCLE:          p = "CYCLE";                 break;
    case ST_DATA_DECL:      p = "data declaration";      break;
    case ST_DATA:           p = "DATA";                  break;
    case ST_DEALLOCATE:     p = "DEALLOCATE";            break;
    case ST_DERIVED_DECL:   p = "Derived type declaration"; break;
    case ST_DO:             p = "DO";                    break;
    case ST_ELSE:           p = "ELSE";                  break;
    case ST_ELSEIF:         p = "ELSE IF";               break;
    case ST_ELSEWHERE:      p = "ELSEWHERE";             break;
    case ST_END_BLOCK_DATA: p = "END BLOCK DATA";        break;
    case ST_ENDDO:          p = "END DO";                break;
    case ST_END_CRITICAL:   p = "END CRITICAL";          break;
    case ST_END_ENUM:       p = "END ENUM";              break;
    case ST_END_FILE:       p = "END FILE";              break;
    case ST_END_FORALL:     p = "END FORALL";            break;
    case ST_END_FUNCTION:   p = "END FUNCTION";          break;
    case ST_END_INTERFACE:  p = "END INTERFACE";         break;
    case ST_END_MODULE:     p = "END MODULE";            break;
    case ST_END_PROGRAM:    p = "END PROGRAM";           break;
    case ST_END_SELECT:     p = "END SELECT";            break;
    case ST_END_SUBROUTINE: p = "END SUBROUTINE";        break;
    case ST_END_WHERE:      p = "END WHERE";             break;
    case ST_END_TYPE:       p = "END TYPE";              break;
    case ST_ENDIF:          p = "END IF";                break;
    case ST_ENTRY:          p = "ENTRY";                 break;
    case ST_ENUM:           p = "ENUM";                  break;
    case ST_ENUMERATOR:     p = "ENUMERATOR";            break;
    case ST_EQUIVALENCE:    p = "EQUIVALENCE";           break;
    case ST_ERROR_STOP:     p = "ERROR STOP";            break;
    case ST_EXIT:           p = "EXIT";                  break;
    case ST_FLUSH:          p = "FLUSH";                 break;
    case ST_FORALL_BLOCK:   /* Fall through */
    case ST_FORALL:         p = "FORALL";                break;
    case ST_FORMAT:         p = "FORMAT";                break;
    case ST_FUNCTION:       p = "FUNCTION";              break;
    case ST_GOTO:           p = "GOTO";                  break;
    case ST_IF_BLOCK:       p = "block IF";              break;
    case ST_IMPLICIT:       p = "IMPLICIT";              break;
    case ST_IMPLICIT_NONE:  p = "IMPLICIT NONE";         break;
    case ST_IMPLIED_ENDDO:  p = "implied END DO";        break;
    case ST_IMPORT:         p = "IMPORT";                break;
    case ST_INQUIRE:        p = "INQUIRE";               break;
    case ST_INTERFACE:      p = "INTERFACE";             break;
    case ST_PARAMETER:      p = "PARAMETER";             break;
    case ST_PRIVATE:        p = "PRIVATE";               break;
    case ST_PUBLIC:         p = "PUBLIC";                break;
    case ST_MODULE:         p = "MODULE";                break;
    case ST_MODULE_PROC:    p = "MODULE PROCEDURE";      break;
    case ST_NAMELIST:       p = "NAMELIST";              break;
    case ST_NOTIFY:         p = "NOTIFY";                break;
    case ST_NULLIFY:        p = "NULLIFY";               break;
    case ST_OPEN:           p = "OPEN";                  break;
    case ST_PAUSE:          p = "PAUSE";                 break;
    case ST_PROGRAM:        p = "PROGRAM";               break;
    case ST_QUERY:          p = "QUERY";                 break;
    case ST_READ:           p = "READ";                  break;
    case ST_RETURN:         p = "RETURN";                break;
    case ST_REWIND:         p = "REWIND";                break;
    case ST_STOP:           p = "STOP";                  break;
    case ST_SUBROUTINE:     p = "SUBROUTINE";            break;
    case ST_TYPE:           p = "TYPE";                  break;
    case ST_USE:            p = "USE";                   break;
    case ST_WAIT:           p = "WAIT";                  break;
    case ST_WHERE_BLOCK:    /* Fall through */
    case ST_WHERE:          p = "WHERE";                 break;
    case ST_WRITE:          p = "WRITE";                 break;
    case ST_ASSIGNMENT:           p = "assignment";      break;
    case ST_POINTER_ASSIGNMENT:   p = "pointer assignment"; break;
    case ST_SELECT_CASE:          p = "SELECT CASE";     break;
    case ST_SEQUENCE:             p = "SEQUENCE";        break;
    case ST_SIMPLE_IF:            p = "Simple IF";       break;
    case ST_STATEMENT_FUNCTION:   p = "STATEMENT FUNCTION"; break;
    case ST_SYNC_ALL:             p = "SYNC ALL";        break;
    case ST_SYNC_MEMORY:          p = "SYNC MEMORY";     break;
    case ST_SYNC_TEAM:            p = "SYNC TEAM";       break;
    case ST_SYNC_IMAGES:          p = "SYNC IMAGES";     break;
    default:
	g95_internal_error("g95_ascii_statement(): Bad statement code");
    }

    return p;
}



/* g95_state_name()-- Return the name of a compile state */

char *g95_state_name(g95_compile_state state) {
char *p;

    switch(state) {
    case COMP_PROGRAM:     p = "a PROGRAM";               break;
    case COMP_MODULE:      p = "a MODULE";                break;
    case COMP_SUBROUTINE:  p = "a SUBROUTINE";            break;
    case COMP_FUNCTION:    p = "a FUNCTION";              break;
    case COMP_BLOCK_DATA:  p = "a BLOCK DATA";            break;
    case COMP_INTERFACE:   p = "an INTERFACE";            break;
    case COMP_DERIVED:     p = "a DERIVED TYPE block";    break;
    case COMP_IF:          p = "an IF-THEN block";        break;
    case COMP_DO:          p = "a DO block";              break;
    case COMP_SELECT:      p = "a SELECT block";          break;
    case COMP_FORALL:      p = "a FORALL block";          break;
    case COMP_WHERE:       p = "a WHERE block";           break;
    case COMP_CONTAINS:    p = "a contained subprogram";  break;
    case COMP_ENUM:        p = "an ENUM block";           break;

    default:
	g95_internal_error("g95_state_name(): Bad state");
    }

    return p;
}



/* verify_st_order()-- Given the next statement seen by the matcher,
 * make sure that it is in proper order with the last.  This
 * subroutine is initialized by calling it with an argument of
 * ST_NONE.  If there is a problem, we issue an error and return
 * FAILURE.  Otherwise we return SUCCESS.
 *
 * Individual parsers need to verify that the statements seen are
 * valid before calling here, ie ENTRY statements are not allowed in
 * INTERFACE blocks.  The following diagram is taken from the standard:

            +---------------------------------------+
            | program  subroutine  function  module |
            +---------------------------------------+
            |                 use                   |
            |---------------------------------------+
            |               import                  |
            |---------------------------------------+
            |        |        implicit none         |
            |        +-----------+------------------+
            |        | parameter |  implicit        |
            |        +-----------+------------------+
            | format |           |  derived type    |
            | entry  | parameter |  interface       |
            |        |   data    |  specification   |
            |        |           |  statement func  |
            |        +-----------+------------------+
            |        |   data    |    executable    |
            +--------+-----------+------------------+
            |                contains               |
            +---------------------------------------+
            |      internal module/subprogram       |
            +---------------------------------------+
            |                   end                 |
            +---------------------------------------+

*/

static try verify_st_order(st_state *p, g95_statement st) {

    switch(st) {
    case ST_NONE:
	p->state = ORDER_START;
	break;

    case ST_USE:
	if (p->state > ORDER_USE)
	    goto order;

	p->state = ORDER_USE;
	break;

    case ST_IMPORT:
	if (p->state > ORDER_IMPORT)
	    goto order;

	p->state = ORDER_IMPORT;
	break;

    case ST_IMPLICIT_NONE:
	if (p->state > ORDER_IMPLICIT_NONE)
	    goto order;

/* The '>' sign cannot be a '>=', because a FORMAT or ENTRY statement
 * disqualifies a USE but not an IMPLICIT NONE.  Duplicate IMPLICIT
 * NONEs are caught when the implicit types are set. */

	p->state = ORDER_IMPLICIT_NONE;
	break;

    case ST_IMPLICIT:
	if (p->state > ORDER_IMPLICIT)
	    goto order;

	p->state = ORDER_IMPLICIT;
	break;

    case ST_FORMAT:
    case ST_ENTRY:
	if (p->state < ORDER_IMPLICIT_NONE)
	    p->state = ORDER_IMPLICIT_NONE;

	break;

    case ST_PARAMETER:
    case ST_ENUM:
	if (p->state >= ORDER_EXEC)
	    goto order;

	if (p->state < ORDER_IMPLICIT)
	    p->state = ORDER_IMPLICIT;

	break;

    case ST_DATA:
	if (p->state < ORDER_SPEC) p->state = ORDER_SPEC;
	break;

    case ST_PUBLIC:          case ST_PRIVATE:
    case ST_DERIVED_DECL:
    case_decl:
	if (p->state >= ORDER_EXEC)
	    goto order;

	if (p->state < ORDER_SPEC)
	    p->state = ORDER_SPEC;

	break;

    case_executable:
    case_exec_markers:
	if (p->state < ORDER_EXEC)
	    p->state = ORDER_EXEC;

	break;

    default:
	g95_internal_error("Unexpected %s statement in verify_st_order() "
			   "at %C", g95_ascii_statement(st));
    }

/* All is well, record the statement in case we need it next time. */

    p->where = g95_current_locus;
    p->last_statement = st;
    return SUCCESS;

order:
    g95_error("%s statement at %C cannot follow %s statement at %L",
	      g95_ascii_statement(st), g95_ascii_statement(p->last_statement),
	      &p->where);

    return FAILURE;
}



/* accept_statement()-- Do whatever is necessary to accept the last
 * statement */

static g95_code *accept_statement(g95_statement st) {
g95_code *p;

    p = NULL;

    switch(st) {
    case ST_USE:
	g95_use_module();
	break;

    case ST_IMPLICIT_NONE:
	g95_set_implicit_none();
	break;

    case ST_IMPLICIT:
	g95_set_implicit();
	break;

    case ST_FUNCTION: case ST_SUBROUTINE: case ST_MODULE: case ST_BLOCK_DATA:
	g95_current_ns->proc_name = g95_new_block;
	break;

    /* If the statement is the end of a block, lay down a special code
     * that allows a branch to the end of the block from within the
     * construct. */

    case ST_ENDIF:  case ST_END_SELECT:
	if (g95_statement_label != NULL) {
	    new_st.type = EXEC_NOP;
	    new_st.ext.end_code = st;
	    p = g95_add_statement();
	}

	break;

    /* The end-of-program unit statements do not get the special
     * marker and require a statement of some sort if they are a
     * branch target. */

    case ST_END_PROGRAM:  case ST_END_FUNCTION:  case ST_END_SUBROUTINE:
	if (g95_statement_label != NULL) {
	    new_st.type = EXEC_RETURN;
	    p = g95_add_statement();
	}

	break;

    case_executable:
    case_exec_markers:
    case ST_ENTRY:
	p = g95_add_statement();
	break;

    default:
	break;
    }

    g95_commit_symbols();
    g95_warning_check();
    g95_clear_new_st();

    return p;
}



/* unexpected_eof()-- Handle an unexpected end of file.  This is a
 * show-stopper... */

static void unexpected_eof(void) {
g95_state_data *p;

    g95_error("Unexpected end of file in '%s'", g95_source_file);

    /* Memory cleanup.  Move to "second to last" */

    for(p=g95_state_stack; p && p->previous && p->previous->previous;
	p=p->previous);

    g95_current_ns->code = (p && p->previous) ? p->head : NULL;
    g95_done_2();

    longjmp(eof, 1);
}



/* unexpected_statement()-- Generic complaint about an out of order
 * statement.  We also do whatever is necessary to clean up.  */

static void unexpected_statement(g95_statement st) {

    g95_error("Unexpected %s statement at %C", g95_ascii_statement(st));

    g95_reject_statement();
}



/* g95_reject_statement()-- Undo anything tentative that has been built
 * for the current statement. */

void g95_reject_statement(void) {

    g95_undo_symbols();
    g95_undo_statement();
    g95_clear_warning();
    g95_clear_new_st();
}



/* stf_pre_match()-- See if the thing on the input sort of looks like a
 * statement function.  This is:  WORD ( [ WORD [, WORD ] ... ] ) =
 * The idea is to eliminate things that can't possibly be statement
 * functions without messing with the symbol table.  We never return
 * error. */

static match stf_pre_match(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_locus where;
match m;
char c;

    where = g95_current_locus;

    m = MATCH_NO;
    if (g95_match_name(name) != MATCH_YES) goto done;
    if (g95_match_char('(') != MATCH_YES) goto done;

    g95_gobble_whitespace();
    if (g95_peek_char() == ')')
	g95_next_char();

    else
	for(;;) {
	    if (g95_match_name(name) != MATCH_YES)
		goto done;

	    g95_gobble_whitespace();
	    c = g95_next_char(); 

	    if (c == ')') break;
	    if (c != ',') goto done;
	}

    m = g95_match_char('=');

done:
    g95_current_locus = where;
    return m;
}



/* implicit_type_st_func()-- Implicitly type the things appearing in a
 * statement function expression.  Don't throw errors on symbols
 * without implicit types because errors are suppressed right now
 * anyway. */

static void implicit_type_st_func(g95_expr *e) {
g95_actual_arglist *a;
g95_symbol *sym;
g95_ref *ref;
char *name;
int i;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_FUNCTION:
	name = e->value.function.name;

	if (name != NULL && !g95_get_symbol(name, NULL, &sym) &&
	    sym->ts.type == BT_UNKNOWN)
	    g95_set_default_type(sym, 0, NULL);

	for(a=e->value.function.actual; a; a=a->next)
	    implicit_type_st_func(a->u.expr);

	break;

    case EXPR_OP:
	implicit_type_st_func(e->value.op.op1);
	implicit_type_st_func(e->value.op.op2);
	break;

    case EXPR_VARIABLE:
	sym = e->symbol;

	if (sym->ts.type == BT_UNKNOWN)
	    g95_set_default_type(sym, 0, NULL);

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++) {
		    implicit_type_st_func(ref->u.ar.start[i]);
		    implicit_type_st_func(ref->u.ar.end[i]);
		    implicit_type_st_func(ref->u.ar.stride[i]);
		}

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    implicit_type_st_func(ref->u.car.element[i]);

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		implicit_type_st_func(ref->u.ss.start);
		implicit_type_st_func(ref->u.ss.end);
		break;
	    }

	break;

    default:
	break;
    }
}



/* g95_match_st_function()-- Match a statement function declaration.
 * It is so easy to match non-statement function statements with a
 * MATCH_ERROR as opposed to MATCH_NO that we suppress error message
 * in most cases. */

match g95_match_st_function(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_error_buf old_error;
g95_formal_arglist *f;
g95_symbol *sym, *v;
match m;

    if (G95_STRICT_F() || stf_pre_match() == MATCH_NO)
	return MATCH_NO;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    /* If a host unit contains a variable with a dimension attribute, it
     * can't be a statement function. */

    if (g95_find_symbol(name, NULL, 1, &sym) != 0)
	return MATCH_ERROR;

    if (sym != NULL && sym->attr.dimension)
	return MATCH_NO;

    if (g95_get_symbol(name, 0, &sym) != 0)
	return MATCH_ERROR;

    if (sym->attr.dimension)
	return MATCH_NO;

    g95_push_error(&old_error);

    if (g95_add_procedure(&sym->attr, PROC_ST_FUNCTION,
			  sym->name, NULL) == FAILURE) {
	m = MATCH_ERROR;
	goto cleanup;
    }

    sym->result = sym;

    if (g95_match_formal_arglist(sym, FORMAL_ST_FUNCTION, 0) != MATCH_YES) {
	m = MATCH_NO;
	goto cleanup;
    }

    for(f=sym->formal; f; f=f->next) {
	if (f->sym == sym) {
	    m = MATCH_NO;
	    goto cleanup;
	}

	g95_get_symbol(f->sym->name, NULL, &v);

	if (v->ts.type == BT_UNKNOWN &&
	    g95_set_default_type(v, 1, NULL) == FAILURE) {
	    m = MATCH_NO;
	    goto cleanup;
	}

	f->sym->ts = v->ts;
	g95_set_usage(v, &v->declared_at, 1, 1);

	if (f->sym->ts.type == BT_CHARACTER &&
	    (f->sym->ts.cl->length == NULL ||
	     f->sym->ts.cl->length->type != EXPR_CONSTANT)) {
	    g95_error_now("CHARACTER statement function dummy argument "
			  "'%s' at %C must have a constant length",
			  f->sym->name);
	    m = MATCH_ERROR;
	    goto cleanup;
	}
    }

    m = g95_match(" = %e%t", &sym->value);
    if (m != MATCH_YES)
	goto cleanup;

    if (sym->ts.type == BT_CHARACTER &&
	(sym->ts.cl->length == NULL ||
	 sym->ts.cl->length->type != EXPR_CONSTANT)) {
	g95_error_now("CHARACTER statement function '%s' at %C must have a "
		      "constant length", name);
	m = MATCH_ERROR;
	goto cleanup;
    }

    implicit_type_st_func(sym->value);

    if (g95_option.obsolescent)
	g95_warning(122, "Statement function at %C is obsolescent");

    return MATCH_YES;

cleanup:
    g95_undo_symbols();

    g95_pop_error(&old_error);
    return m;
}



/* parse_derived()-- Parse a derived type */

static void parse_derived(void) {
int compiling_type, seen_private, seen_sequence, seen_component, error_flag;
g95_statement st;
g95_state_data s;

    error_flag = 0;

    accept_statement(ST_DERIVED_DECL);
    push_state(&s, COMP_DERIVED, g95_new_block);

    g95_new_block->component_access = ACCESS_PUBLIC;

    seen_private   = 0;
    seen_sequence  = 0;
    seen_component = 0;

    compiling_type = 1;
    g95_new_block->attr.set = 1;   /* Type has been defined */

    while(compiling_type) {
	st = next_statement();
	switch(st) {
	case ST_NONE:
	    unexpected_eof();

	case ST_DATA_DECL:
	    accept_statement(st);
	    seen_component = 1;      
	    break;

	case ST_END_TYPE:
	    compiling_type = 0;

	    if (!seen_component) {
		g95_error("Derived type definition at %C has no components");
		error_flag = 1;
	    }

	    accept_statement(ST_END_TYPE);
	    break;

	case ST_PRIVATE:
	    if (g95_find_state(COMP_MODULE) == FAILURE) {
		g95_error("PRIVATE statement in TYPE at %C must be inside "
			  "a MODULE");
		error_flag = 1;
		break;
	    }

	    if (seen_component) {
		g95_error("PRIVATE statement at %C must precede "
			  "structure components");
		error_flag = 1;
		break;
	    }

	    if (seen_private) {
		g95_error("Duplicate PRIVATE statement at %C");
		error_flag = 1;
	    }

	    s.sym->component_access = ACCESS_PRIVATE;
	    accept_statement(ST_PRIVATE);
	    seen_private = 1;
	    break;

	case ST_SEQUENCE:
	    if (seen_component) {
		g95_error("SEQUENCE statement at %C must precede "
			  "structure components");
		error_flag = 1;
		break;
	    }

	    if (g95_current_block()->attr.bind)
		g95_error("SEQUENCE statement at %C conflicts with "
			  "BIND(C) attribute");

	    if (g95_current_block()->attr.sequence)
		g95_warning(120, "SEQUENCE attribute at %C already specified "
			    "in TYPE statement");

	    if (seen_sequence) {
		g95_error("Duplicate SEQUENCE statement at %C");
		error_flag = 1;
	    }

	    seen_sequence = 1;
	    g95_add_sequence(&g95_current_block()->attr,
			     g95_current_block()->name, NULL);
	    break;

	default:
	    unexpected_statement(st);
	    break;  
	}
    }

    pop_state();
}



/* parse_interface()-- Parse an interface.  We must be able to deal
 * with the possibility of recursive interfaces.  The parse_spec()
 * subroutine is mutually recursive with parse_interface(). */

static void parse_interface(void) {
g95_compile_state new_state, current_state;
g95_symbol *prog_unit, *sym;
g95_locus where, where2;
g95_interface_info save;
g95_formal_arglist *f;
g95_state_data s1, s2;
g95_statement st;
g95_gsymbol *g;

    accept_statement(ST_INTERFACE);

    current_interface.ns = g95_current_ns;
    save = current_interface;

    sym = (current_interface.type == INTERFACE_GENERIC ||
	   current_interface.type == INTERFACE_USER_OP)
	? g95_new_block
	: NULL;

    push_state(&s1, COMP_INTERFACE, sym);
    current_state = COMP_NONE;

loop:
    g95_current_ns = g95_get_namespace(current_interface.ns, 0);
    g95_current_ns->interface = 1;

    where = g95_current_locus;
    g = NULL;

    st = next_statement();
    switch(st) {
    case ST_NONE:
	unexpected_eof();

    case ST_FUNCTION: 
	new_state = COMP_FUNCTION;
	g95_add_explicit_interface(g95_new_block, IFSRC_IFBODY,
				   g95_new_block->formal, NULL);

	if (current_interface.type == INTERFACE_ABSTRACT)
	    g95_new_block->attr.abstract = 1;

	if (current_interface.type == INTERFACE_NAMELESS &&
	    !g95_new_block->attr.dummy)
	    g = g95_check_global(g95_new_block->name, GSYM_FUNCTION, NULL);

	break;

    case ST_SUBROUTINE:
	new_state = COMP_SUBROUTINE;
	g95_add_explicit_interface(g95_new_block, IFSRC_IFBODY,
				   g95_new_block->formal, NULL);

	if (current_interface.type == INTERFACE_ABSTRACT)
	    g95_new_block->attr.abstract = 1;

	if (current_interface.type == INTERFACE_NAMELESS &&
	    !g95_new_block->attr.dummy)
	    g = g95_check_global(g95_new_block->name, GSYM_SUBROUTINE,
				 &g95_new_block->declared_at);

	break;

    case ST_MODULE_PROC:  /* The module procedure matcher makes sure the
			   * context is correct */
	accept_statement(st);
	g95_free_namespace(g95_current_ns);
	goto loop;

    case ST_END_INTERFACE:
	g95_free_namespace(g95_current_ns);
	g95_current_ns = current_interface.ns;
	goto done;

    case ST_INTERFACE:
	current_interface = save;   /* Restore trashed current_interface */
	/* Fall through */

    default:
	g95_error("Unexpected %s statement in INTERFACE block at %C",
		  g95_ascii_statement(st));
	g95_reject_statement();
	g95_free_namespace(g95_current_ns);
	goto loop;
    }

    where2 = g95_new_block->declared_at;

    push_state(&s2, new_state, g95_new_block);
    accept_statement(st);
    prog_unit = g95_new_block;
    prog_unit->formal_ns = g95_current_ns;

    /* Read data declaration statements */

decl:
    st = parse_spec(ST_NONE);
    if (st != ST_END_SUBROUTINE && st != ST_END_FUNCTION) {
	g95_error("Unexpected %s statement at %C in INTERFACE body",
		  g95_ascii_statement(st));
	g95_reject_statement();
	goto decl;
    }

    current_interface = save;
    g95_add_interface(prog_unit, &where2);

    for(f=prog_unit->formal; f; f=f->next) {
	sym = f->sym;
	if (sym == NULL)
	    continue;

	if (sym->result != NULL)
	    sym = sym->result;

	if (sym->ts.type == BT_UNKNOWN &&
	    (sym->attr.flavor != FL_PROCEDURE || sym->attr.function))
	    g95_set_default_type(sym, 1, prog_unit->formal_ns);
    }

    if (g != NULL)
	g95_define_global(g, prog_unit, &where);

    pop_state();
    goto loop;

done:
    pop_state();
}



/* new_level()-- Starts a new level in the statement list. */

static g95_code *new_level(g95_code *q) {
g95_code *p;

    p = q->block = g95_get_code(-1, NULL);

    g95_state_stack->head = NULL;
    g95_state_stack->next = &p->next;

    return p;
}


/* parse_enum()-- Parse an ENUM block.  This is relatively easy, since
 * the only statement that can appear is an ENUMERATOR statement.  */

static void parse_enum(void) {
g95_state_data state;
g95_statement st;

    accept_statement(ST_ENUM);
    push_state(&state, COMP_ENUM, NULL);

    for(;;) {
	st = next_statement();

	if (st == ST_ENUMERATOR) {
	    accept_statement(st);
	    continue;
	}

	if (st == ST_END_ENUM)
	    break;

	g95_error("Unexpected %s statement in ENUM block at %C",
		  g95_ascii_statement(st));
	g95_reject_statement();
    }

    pop_state();
}



/* parse_spec()-- Parse a set of specification statements.  Returns
 * the statement that doesn't fit. */

static g95_statement parse_spec(g95_statement st) {
st_state ss;
int save;

    verify_st_order(&ss, ST_NONE);
    save = g95_spec_flag;
    g95_spec_flag = 1;

    if (st == ST_NONE)
	st = next_statement();

loop:
    if (st == ST_USE)
	g95_current_ns->seen_use = 1;

    switch(st) {
    case ST_NONE:
	unexpected_eof();

    case ST_FORMAT: case ST_ENTRY:
	if (g95_current_state() == COMP_MODULE ||
	    g95_find_state(COMP_INTERFACE) == SUCCESS)
	    break;

	/* Fall through */

    case ST_DATA: /* Not allowed in interfaces */
	if (g95_find_state(COMP_INTERFACE) == SUCCESS)
	    break;

	/* Fall through */

    case ST_DERIVED_DECL:    case ST_USE:       case ST_PUBLIC:
    case ST_IMPLICIT_NONE:   case ST_IMPLICIT:  case ST_PARAMETER:  
    case ST_PRIVATE:         case ST_IMPORT:    case ST_ENUM:
    case_decl:
	if (st == ST_STATEMENT_FUNCTION &&
	    (g95_current_state() == COMP_MODULE ||
	     g95_find_state(COMP_INTERFACE) == SUCCESS))
	    break;

	if (g95_current_state() == COMP_BLOCK_DATA &&
	    (st == ST_NAMELIST || st == ST_STATEMENT_FUNCTION ||
	     st == ST_FORMAT   || st == ST_INTERFACE))
	    break;

	if (verify_st_order(&ss, st) == FAILURE) {
	    g95_reject_statement();
	    st = next_statement();
	    goto loop;
	}

	switch(st) {
	case ST_ENUM:
	    parse_enum();
	    break;

	case ST_INTERFACE:
	    parse_interface();
	    break;

	case ST_DERIVED_DECL:
	    parse_derived();
	    break;

	case ST_PUBLIC:  case ST_PRIVATE:
	    if (g95_current_state() != COMP_MODULE) {
		g95_error("%s statement must appear in a MODULE",
			  g95_ascii_statement(st));
		break;
	    }

	    if (g95_current_ns->default_access != ACCESS_UNKNOWN) {
		g95_error("%s statement at %C follows another accessibility "
			  "specification", g95_ascii_statement(st));
		break;
	    }

	    g95_current_ns->default_access = (st == ST_PUBLIC)
		? ACCESS_PUBLIC : ACCESS_PRIVATE;

	    break;

	default:
	    break;
	}

	accept_statement(st);
	st = next_statement();
	goto loop;

    default:
	break;
    }

    g95_spec_flag = save;
    return st;
}



/* parse_critical_block()-- Parse statements inside a CRITICAL block. */

static void parse_critical_block(void) {
g95_code *c, *top;
g95_statement st;
g95_state_data s;

    top = accept_statement(ST_CRITICAL);
    push_state(&s, COMP_CRITICAL, g95_new_block);

    g95_state_stack->next = &top->block;
    g95_state_stack->top = top;

    c = g95_get_code(EXEC_CRITICAL, NULL);
    top->block = c;

    st = parse_executable(ST_NONE);

    if (st == ST_NONE)
	unexpected_eof();

    if (st != ST_END_CRITICAL)
	unexpected_statement(st);

    pop_state();
    accept_statement(st);
}



/* parse_if_block()-- parse the statements of an IF-THEN-ELSEIF-ELSE-ENDIF
 * block. */

static void parse_if_block(void) {
g95_locus else_locus;
g95_code *top, *c;
g95_statement st;
g95_state_data s;
int seen_else;

    seen_else = 0;
    top = accept_statement(ST_IF_BLOCK);

    push_state(&s, COMP_IF, g95_new_block);
    g95_state_stack->next = &top->block;

    do {
	st = parse_executable(ST_NONE);

	switch(st) {
	case ST_NONE:
	    unexpected_eof();

	case ST_ELSE:
	    if (seen_else) {
		g95_error("Duplicate ELSE statements at %L and %C",
			  &else_locus);
		g95_reject_statement();
		break;
	    }

	    seen_else = 1;
	    else_locus = g95_current_locus;

	    g95_state_stack->next = &top->ext.block;

	    accept_statement(st);
	    break;

	case ST_ELSEIF:
	    if (seen_else) {
		g95_error("ELSE IF statement at %L cannot follow ELSE "
			  "statement at %L", &new_st.where, &else_locus);

		g95_reject_statement();
		break;
	    }

	    c = g95_get_code(EXEC_IF, NULL);
	    c->expr = new_st.expr;

	    top->ext.block = c;
	    top = c;

	    g95_state_stack->next = &top->block;
	    accept_statement(st);
	    break;

	case ST_ENDIF:
	    break;

	default:
	    unexpected_statement(st);
	    break;
	}
    } while(st != ST_ENDIF);
    pop_state();
    accept_statement(st);
}



/* parse_forall_block()-- Parse a FORALL block (not a simple FORALL
 * statement) */

static void parse_forall_block(void) {
g95_state_data s;
g95_statement st;
g95_code *top;

    memset(&s, '\0', sizeof(s)); 
    s.ext.iter = new_st.ext.forall_iterator;

    top = accept_statement(ST_FORALL_BLOCK);

    push_state(&s, COMP_FORALL, g95_new_block);
    g95_state_stack->next = &top->block;

    do {
	st = next_statement();
	switch(st) {

	case ST_ASSIGNMENT:
	case ST_POINTER_ASSIGNMENT:
	case ST_WHERE:
	case ST_FORALL:
	    accept_statement(st);
	    break;

	case ST_END_FORALL:
	    accept_statement(st);
	    break;

	case ST_WHERE_BLOCK:
	    parse_where_block();
	    break;

	case ST_FORALL_BLOCK:
	    parse_forall_block();
	    break;

	case ST_NONE:
	    unexpected_eof();

	default:
	    g95_error("Unexpected %s statement in FORALL block at %C",
		      g95_ascii_statement(st));

	    g95_reject_statement();
	    break;
	}
    } while(st != ST_END_FORALL);

    pop_state();
}



/* parse_where_block()-- Parse a WHERE block, (not a simple WHERE statement) */

static void parse_where_block(void) {
int seen_empty_else;
g95_code *top, *d;
g95_state_data s;
g95_statement st;

    top = accept_statement(ST_WHERE_BLOCK);
    push_state(&s, COMP_WHERE, g95_new_block);
    g95_state_stack->next = &top->next;

    d = g95_get_code(EXEC_WHERE, NULL);
    d->expr = top->expr;

    top->expr = NULL;
    top->block = d;
    top = d;

    g95_state_stack->next = &d->next;
    seen_empty_else = 0;

    do {
	st = next_statement();
	switch(st) {
	case ST_NONE:
	    unexpected_eof();

	case ST_WHERE_BLOCK:
	    parse_where_block();
	    /* Fall through */

	case ST_ASSIGNMENT:
	case ST_WHERE:
	    accept_statement(st);
	    break;

	case ST_ELSEWHERE:
	    if (seen_empty_else)
		g95_error("ELSEWHERE statement at %C follows previous "
			  "unmasked ELSEWHERE");

	    else {
		if (new_st.expr == NULL)
		    seen_empty_else = 1;

		top->block = g95_get_code(EXEC_WHERE, NULL);
		top = top->block;
		top->expr = new_st.expr;

		g95_state_stack->next = &top->next;
	    }

	    accept_statement(st);
	    break;

	case ST_END_WHERE:
	    accept_statement(st);
	    break;

	default:
	    g95_error("Unexpected %s statement in WHERE block at %C",
		      g95_ascii_statement(st));
	    g95_reject_statement();
	    break;
	}

    } while(st != ST_END_WHERE);

    pop_state();
}



/* parse_select_block()-- Parse a SELECT block */

static void parse_select_block(void) {
g95_statement st;
g95_state_data s;
g95_code *p;

    p = accept_statement(ST_SELECT_CASE);
    push_state(&s, COMP_SELECT, g95_new_block);

/* Make sure that the next statement is a CASE or END SELECT */

    for(;;) {
	st = next_statement();
	if (st == ST_NONE) unexpected_eof();
	if (st == ST_END_SELECT) { /* empty SELECT CASE is OK */
	    pop_state();
	    accept_statement(st);
	    return;
	}

	if (st == ST_CASE)
	    break;

	g95_error("Expected a CASE or END SELECT statement following "
		  "SELECT CASE at %C");

	g95_reject_statement();
    }

/* At this point, we're got a nonempty select block */

    p = new_level(p);
    *p = new_st;

    accept_statement(st);

    do {
	st = parse_executable(ST_NONE);
	switch(st) {
	case ST_NONE:
	    unexpected_eof();

	case ST_CASE:
	    p = new_level(p);
	    *p = new_st;
	    g95_clear_new_st();
	    accept_statement(st);
	    break;

	case ST_END_SELECT:
	    break;

/* Can't have an executable statement because of parse_executable() */

	default:
	    unexpected_statement(st);
	    break;
	}
    } while(st != ST_END_SELECT);

    pop_state();
    accept_statement(st);
}



/* g95_check_do_variable()-- Given a symbol, make sure it is not an
 * iteration variable for a DO statement.  This subroutine is called
 * when the symbol is seen in a context that causes it to become
 * redefined.  If the symbol is an iterator, we generate an error
 * message and return nonzero. */

int g95_check_do_variable(g95_symbol *sym, g95_locus *where) {
g95_state_data *p;

    for(p=g95_state_stack; p; p=p->previous)
	if (p->do_variable == sym) {
	    g95_error_now("Variable '%s' at %L is a DO-iterator and cannot be "
			  "redefined", sym->name, where);
	    return 1;
	}

    return 0;
}



/* check_do_closure()-- Checks to see if the current statement label
 * closes an enddo.  Returns 0 if not, 1 if closes an ENDDO correctly,
 * or 2 (and issues an error) if it incorrectly closes an ENDDO. */

static int check_do_closure(void) {
g95_state_data *p;

    if (g95_statement_label == NULL)
	return 0;

    for(p=g95_state_stack; p; p=p->previous)
	if (p->state == COMP_DO)
	    break;

    if (p == NULL)
	return 0;  /* No loops to close */

    if (p->ext.end_do_label == g95_statement_label) {
	if (p == g95_state_stack) {
	    if (g95_option.obsolescent)
		g95_warning(142, "Nonblock DO statement at %C is obsolescent");

	    return 1;
	}

	g95_error("End of nonblock DO statement at %C is within "
		  "another block");
	return 2;
    }

/* At this point, the label doesn't terminate the innermost loop.
 * Make sure it doesn't terminate another one. */

    for(; p; p=p->previous)
	if (p->state == COMP_DO
	    && p->ext.end_do_label == g95_statement_label) {
	    g95_error("End of nonblock DO statement at %C is interwoven "
		      "with another DO loop");
	    return 2;
	}

    return 0;
}



/* parse_do_block()-- Parse a DO loop.  Note that the ST_CYCLE and
 * ST_EXIT statements are handled inside of parse_executable(),
 * because they aren't really loop statements. */

static void parse_do_block(void) {
g95_statement st;
g95_state_data s;
g95_symbol *sym;
g95_code *top;

    s.ext.end_do_label = new_st.label;

    sym = (new_st.ext.iterator == NULL)
	? NULL
	: new_st.ext.iterator->var->symbol;

    top = accept_statement(ST_DO);
    push_state(&s, COMP_DO, g95_new_block);

    s.do_variable = sym;

    g95_state_stack->next = &top->block;
    g95_state_stack->top = top;

loop:
    st = parse_executable(ST_NONE);

    switch(st) {
    case ST_NONE:
	unexpected_eof();

    case ST_ENDDO:
	if (s.ext.end_do_label != NULL
	    && s.ext.end_do_label != g95_statement_label)
	    g95_error_now("Statement label in ENDDO at %C doesn't match DO "
			  "label");

	if (g95_statement_label != NULL) {
	    new_st.type = EXEC_NOP;
	    new_st.ext.end_code = ST_ENDDO;
	    g95_add_statement();
	}

	break;

    case ST_IMPLIED_ENDDO:
	if (s.sym != NULL)
	    g95_error_now("End of DO-loop at %C must end with the '%s' label",
			  s.sym->name);
	break;

    default:
	unexpected_statement(st);
	goto loop;
    }

    pop_state();
    accept_statement(st);
}



/* parse_executable()-- Accept a series of executable statements.  We
 * return the first statement that doesn't fit to the caller.  Any
 * block statements are passed on to the correct handler, which
 * usually passes the buck right back here. */

static g95_statement parse_executable(g95_statement st) {
int close_flag;

    if (st == ST_NONE)
	st = next_statement();

    for(;; st=next_statement()) {
	close_flag = check_do_closure();
	if (close_flag)
	    switch(st) {
	    case ST_GOTO:
		if (new_st.type == EXEC_SELECT)
		    break;   /* Computed goto */

		if (new_st.expr != NULL)
		    break;   /* Assigned goto */

	    case ST_END_SUBROUTINE: case ST_DO:          case ST_FORALL_BLOCK:
	    case ST_WHERE_BLOCK:    case ST_SELECT_CASE: case ST_ASSIGN:
	    case ST_END_PROGRAM:    case ST_RETURN:      case ST_EXIT:
	    case ST_END_FUNCTION:   case ST_CYCLE:       case ST_STOP:
		g95_error("%s statement at %C cannot terminate a non-block "
			  "DO loop", g95_ascii_statement(st));
		break;

	    default:
		break;
	    }

	switch(st) {
	case ST_NONE:
	    unexpected_eof();

	case ST_FORMAT:    case ST_DATA:    case ST_ENTRY:
	case_executable:
	    accept_statement(st);
	    if (close_flag == 1)
		return ST_IMPLIED_ENDDO;

	    continue;

	case ST_CRITICAL:
	    parse_critical_block();
	    continue;

	case ST_IF_BLOCK:
	    parse_if_block();
	    continue;

	case ST_SELECT_CASE:
	    parse_select_block();
	    continue;

	case ST_DO:
	    parse_do_block();
	    if (check_do_closure() == 1)
		return ST_IMPLIED_ENDDO;

	    continue;

	case ST_WHERE_BLOCK:
	    parse_where_block();
	    continue;

	case ST_FORALL_BLOCK:
	    parse_forall_block();
	    continue;

	default:
	    break;
	}
    
	break;
    }

    return st;
}



/* parse_contained()-- Parse a series of contained program units */

static void parse_contained(int module) {
g95_namespace *head, *tail, *parent_ns;
g95_state_data s1, s2;
g95_compile_state s;
g95_statement st;
g95_symbol *sym;
int m;

    push_state(&s1, COMP_CONTAINS, NULL);
    parent_ns = g95_current_ns;
    head = tail = NULL;

    do {
	g95_current_ns = g95_get_namespace(parent_ns, 1);
	st = next_statement();

	switch(st) {
	case ST_NONE:
	    unexpected_eof();

	case ST_FUNCTION:
	case ST_SUBROUTINE:
	    accept_statement(st);

	    s = (st == ST_FUNCTION) ? COMP_FUNCTION : COMP_SUBROUTINE;
	    g95_current_ns->declared_at = g95_statement_locus;
	    g95_current_ns->state = s;

	    push_state(&s2, s, g95_new_block);

	    /* For contained procedures, create/update the symbol in
	     * the parent namespace */

	    if (g95_get_symbol(g95_new_block->name, parent_ns, &sym)) {
		g95_error("Contained procedure '%s' at %C is already "
			  "ambiguous", g95_new_block->name);
		goto parse;
	    }

	    if (sym->attr.contained) {
		g95_error("Duplicated contained procedure '%s' at %C",
			  g95_new_block->name);
		goto parse;
	    }

	    sym->attr.contained = 1;

	    if (sym->attr.proc == PROC_UNKNOWN && 
		g95_add_procedure(&sym->attr,
				  module ? PROC_MODULE : PROC_INTERNAL,
				  sym->name,
				  &g95_new_block->declared_at) == FAILURE)
		goto parse;

	    if (st == ST_FUNCTION)
		g95_add_function(&sym->attr, sym->name,
				 &g95_new_block->declared_at);

	    else {
		g95_add_subroutine(&sym->attr, sym->name,
				   &g95_new_block->declared_at);

		if (sym->ts.type == BT_UNKNOWN)
		    sym->ts.type = BT_PROCEDURE;
	    }

	parse:
	    g95_commit_symbols();
	    parse_progunit(ST_NONE);

	    g95_current_ns->code = s2.head;

	    if (head == NULL)
		head = tail = g95_current_ns;

	    else {
		tail->sibling = g95_current_ns;
		tail = g95_current_ns;
	    }

	    g95_current_ns = parent_ns;
	    pop_state();
	    break;

/* These statements are associated with the end of the host unit */

	case ST_END_PROGRAM:    case ST_END_SUBROUTINE:
	case ST_END_FUNCTION:   case ST_END_MODULE:
	    break;

	default:
	    g95_error("Unexpected %s statement in CONTAINS section at %C", 
		      g95_ascii_statement(st));
	    g95_reject_statement();
	    break;
	}
    } while(st != ST_END_FUNCTION && st != ST_END_SUBROUTINE &&
	    st != ST_END_MODULE   && st != ST_END_PROGRAM);

    /* The current namespace is guaranteed to not have anything
     * (worthwhile) in it. */

    m = (g95_statement_label != NULL) ? g95_statement_label->value : -1;

    g95_free_namespace(g95_current_ns);
    g95_current_ns = parent_ns;

    if (head == NULL)
	g95_error("CONTAINS block at %C is empty");

    else
	g95_current_ns->contained = head;

    pop_state();

    if (m == -1)
	g95_statement_label = NULL;

    else {
	g95_statement_label = g95_get_st_label(m);
	g95_define_st_label(g95_statement_label, ST_LABEL_TARGET,
			    &g95_current_locus);
    }

    new_st.here = g95_statement_label;

    accept_statement(st);
}



/* parse_progunit()-- Parse a PROGRAM, SUBROUTINE or FUNCTION unit */

static void parse_progunit(g95_statement st) {
g95_state_data *p;
int n;

    g95_state_stack->next = &g95_current_ns->code;

    st = parse_spec(st);
    switch(st) {
    case ST_NONE:
	unexpected_eof();

    case ST_CONTAINS:
	goto contains;

    case_end:
	accept_statement(st);
	goto done;

    default:
	break;
    }

loop:
    for(;;) {
	st = parse_executable(st);

	switch(st) {
	case ST_NONE:
	    unexpected_eof();

	case ST_CONTAINS:
	    goto contains;
  
	case_end:
	    accept_statement(st);
	    goto done;

	default:
	    break;
	}

	unexpected_statement(st);
	g95_reject_statement();
	st = next_statement();
    }

contains:
    n = 0;

    for(p=g95_state_stack; p; p=p->previous)
	if (p->state == COMP_CONTAINS)
	    n++;

    if (g95_find_state(COMP_MODULE) == SUCCESS)
	n--;

    if (n > 0) {
	g95_error("CONTAINS statement at %C is already in a contained "
		  "program unit");
	st = next_statement();
	goto loop;
    }

    parse_contained(0);

done:
    return;
}



/* parse_block_data()-- Parse a block data program unit */

static void parse_block_data(void) {
static g95_locus blank_locus;
static int blank_block=0;
g95_statement st;

    if (g95_new_block != NULL)
	g95_check_global(g95_new_block->name, GSYM_BLOCK_DATA, NULL);

    else if (blank_block)
	g95_error("Blank BLOCK DATA at %C conflicts with prior BLOCK DATA "
		  "at %L", &blank_locus);

    else {
	blank_block = 1;
	blank_locus = g95_current_locus;
    }

    st = parse_spec(ST_NONE);

    while(st != ST_END_BLOCK_DATA) {
	g95_error("Unexpected %s statement in BLOCK DATA at %C",
		  g95_ascii_statement(st));
	g95_reject_statement();
	st = next_statement();
    }
}



/* parse_module()-- Parse a module subprogram */

static void parse_module(void) {
g95_statement st;

    g95_check_global(g95_new_block->name, GSYM_MODULE,
		     &g95_new_block->declared_at);

    st = parse_spec(ST_NONE);

loop:
    switch(st) {
    case ST_NONE:
	unexpected_eof();

    case ST_CONTAINS:
	parse_contained(1);
	break;

    case ST_END_MODULE:
	accept_statement(st);
	break;

    default:
	g95_error("Unexpected %s statement in MODULE at %C",
		  g95_ascii_statement(st));

	g95_reject_statement();
	st = next_statement();
	goto loop;
    }
}



/* define_program()-- Define a symbol for the name of a PROGRAM unit.
 * Note that is isn't possible for the user to define a symbol with
 * uppercase letters, so MAIN is unique. */

static void define_program(int explicit) {
g95_symbol *sym;

    g95_get_symbol(G95_MAIN, g95_current_ns, &sym);

    sym->attr.flavor = FL_PROGRAM;
    sym->attr.proc = PROC_UNKNOWN;
    sym->attr.subroutine = 1;
    sym->attr.access = ACCESS_PUBLIC;
    sym->attr.artificial = 1;

    if (explicit)
	sym->declared_at = g95_current_locus;

    g95_current_ns->proc_name = sym;
    g95_commit_symbols();
}



/* g95_parse_file()-- Top level parser. */

void g95_parse_file(void) {
g95_namespace *ns, *head, *tail;
int errors, seen_program;
g95_state_data top, s;
g95_locus prog_locus;
g95_statement st;
g95_symbol *sym;
g95_gsymbol *g;

    g95_check_options();

    top.state = COMP_NONE;
    top.sym = NULL;
    top.previous = NULL;
    top.head = NULL;
    top.next = NULL;
    top.do_variable = NULL;

    head = tail = NULL;

    g95_state_stack = &top;
    g95_clear_new_st();
    g95_statement_label = NULL;

    if (setjmp(eof))
	return;   /* Come here on unexpected EOF */

    seen_program = 0;

loop:
    g = NULL;
    sym = NULL;

    g95_init_2();

    st = next_statement();
    switch(st) {
    case ST_NONE:
	g95_check_global_refs();

	g95_get_errors(NULL, &errors);
	if (errors != 0)
	    goto done;
  
	g95_done_2();

#ifdef IN_GCC
	g95_generate_code(head);
#endif

	goto done;

    case ST_PROGRAM:
	if (seen_program)
	    goto duplicate_main;

	seen_program = 1;
	prog_locus = g95_current_locus;

	g95_current_ns->declared_at = g95_def_locus;
	g95_current_ns->state = COMP_PROGRAM;
	push_state(&s, COMP_PROGRAM, g95_new_block);
	accept_statement(st);

	if (g95_new_block != NULL)
	    g95_check_global(g95_new_block->name, GSYM_PROGRAM,
			     &g95_new_block->declared_at);

	define_program(1);
	parse_progunit(ST_NONE);
	break;

    case ST_BLOCK_DATA:
	if (G95_STRICT_F())
	    g95_error("BLOCK DATA program unit at %C is not permitted in F");

	g95_current_ns->declared_at = g95_statement_locus;
	g95_current_ns->state = COMP_BLOCK_DATA;
	push_state(&s, COMP_BLOCK_DATA, g95_new_block);
	accept_statement(st);
	parse_block_data();
	break;

    case ST_MODULE:
	g95_current_ns->declared_at = g95_statement_locus;
	g95_current_ns->state = COMP_MODULE;
	g95_current_ns->name_pos = g95_def_locus;
	g95_current_ns->unit_name = g95_new_block->name;

	push_state(&s, COMP_MODULE, g95_new_block);
	accept_statement(st);

	parse_module();
	break;

    case ST_SUBROUTINE:
	if (G95_STRICT_F())
	    g95_error("External subroutine '%s' at %C not allowed in F",
		      g95_new_block->name);

	g = g95_check_global(g95_new_block->name, GSYM_SUBROUTINE,
			     &g95_new_block->declared_at);
	sym = g95_new_block;

	g95_current_ns->declared_at = g95_statement_locus;
	g95_current_ns->state = COMP_SUBROUTINE;
	push_state(&s, COMP_SUBROUTINE, g95_new_block);
	accept_statement(st);
	parse_progunit(ST_NONE);
	break;

    case ST_FUNCTION:
	if (G95_STRICT_F())
	    g95_error("External function '%s' at %C not allowed in F",
		      g95_new_block->name);

	g = g95_check_global(g95_new_block->name, GSYM_FUNCTION,
			     &g95_new_block->declared_at);
	sym = g95_new_block;

	g95_current_ns->declared_at = g95_statement_locus;
	g95_current_ns->state = COMP_FUNCTION;
	push_state(&s, COMP_FUNCTION, g95_new_block);
	accept_statement(st);
	parse_progunit(ST_NONE);

	break;

/* Anything else starts a nameless main program block */

    default:
	if (seen_program)
	    goto duplicate_main;

	seen_program = 1;
	prog_locus = g95_current_locus;

	if (G95_STRICT_F())
	    g95_error("Missing PROGRAM statement at %C in F mode");

	define_program(0);

	g95_current_ns->declared_at = g95_statement_locus;
	g95_current_ns->state = COMP_PROGRAM;
	push_state(&s, COMP_PROGRAM, NULL);
	parse_progunit(st);
	break;
    }

    g95_current_ns->code = s.head;

    /* Resolve, scalarize, dump modules and process entries. */

    g95_get_errors(NULL, &errors);
    if (errors != 0)
	goto error;

    g95_resolve(g95_current_ns);

    g95_get_errors(NULL, &errors);
    if (errors != 0) 
	goto error;

    if (g != NULL)
	g95_define_global(g, sym, &sym->declared_at);

    g95_dump(g95_current_ns);

    g95_scalarize(g95_current_ns);

    g95_get_errors(NULL, &errors);
    if (errors != 0)
	goto error;

    /* We have a valid fortran program */

    if (s.state == COMP_MODULE)
	g95_dump_module(s.sym->name);

    if (g95_option.c_binding)
	g95_show_cbinding(g95_current_ns);

    g95_process_entry(g95_current_ns);

    if (g95_option.verbose)
	for(ns=g95_current_ns; ns; ns=ns->sibling)
	    g95_show_namespace(ns);

    if (head == NULL)
	head = tail = g95_current_ns;

    else
	tail->sibling = g95_current_ns;

    while(tail->sibling != NULL)
	tail = tail->sibling;

    pop_state();
    g95_done_2();
    goto loop;

error:
    g95_free_namespace(g95_current_ns);
    goto done;

/* If we see a duplicate main program, shut down.  If the second
 * instance is an implied main program, ie data declarations or
 * executable statements, we're in for lots of errors. */

duplicate_main:
    g95_error("Two main PROGRAM units at %L and %C", &prog_locus);
    g95_reject_statement();
    g95_done_2();

done:
    for(ns=head; ns; ns=tail) {
	tail = ns->sibling;
	g95_free_namespace(ns);
    }

//    g95_free_namespace(g95_current_ns);
    g95_current_ns = NULL;

    g95_dump_done();
}

