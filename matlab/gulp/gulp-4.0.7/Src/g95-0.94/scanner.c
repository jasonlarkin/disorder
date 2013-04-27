/* Character scanner
   Copyright (C) 2000 - 2008 Free Software Foundation, Inc.
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

/* scanner.c-- Set of subroutines to (ultimately) return the next
 * character to the various matching subroutines.  This file's job is
 * to read files and build up lines that are parsed by the parser.
 * This means that we handle continuation lines and "include" lines.
 * 
 * The first thing the scanner does is to load an entire file into
 * memory.  We load the entire file into memory for a couple reasons.
 * The first is that we want to be able to deal with nonseekable input
 * (pipes, stdin) and there is a lot of backing up involved during
 * parsing.
 *
 * The second is that we want to be able to print the locus of errors,
 * and an error on line 999999 could conflict with something on line
 * one.  Given nonseekable input, we've got to store the whole thing.
 *
 * One thing that helps are the column truncation limits that give us
 * an upper bound on the size of individual lines.  We don't store the
 * truncated stuff.
 * 
 * From the scanner's viewpoint, the higher level subroutines ask for
 * new characters and do a lot of jumping backwards. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

#if IN_GCC
#include "trans.h"
#include "cpplib.h"
extern int fputs_unlocked(const char *, FILE *);
#include "../libcpp/internal.h"

static int line_offset=0, initial_switch=0;

#else
#include <ctype.h>
#include "g95.h"
#endif

typedef struct g95_cpp_macro {
    struct g95_cpp_macro *next;
    char *name;
    int define;
} g95_cpp_macro;

static int line_width, continue_flag, end_flag, bad_line, seen_include;
static g95_file *file_head, *file_stack, *current_file;
static g95_cpp_macro *cpp_macro_head, *cpp_macro_tail;
static g95_linebuf *line_head, *line_tail;
static int include_level = 0;
static char *current_name;

char *g95_source_file, *g95_search_prefix;
g95_source_form g95_current_form;
g95_locus g95_current_locus, g95_def_locus;

static try load_file(char *, char *, int);

#define G95_MAX_LINE 10000




/* g95_define_cpp_macro()-- Define or undefine a preprocessor macro. */

void g95_define_cpp_macro(char *arg, int flag) {
g95_cpp_macro *p;

    p = g95_getmem(sizeof(g95_cpp_macro));
    p->name   = arg;
    p->define = flag; 

    if (cpp_macro_head == NULL)
	cpp_macro_head = p;
    else
	cpp_macro_tail->next = p;

    cpp_macro_tail = p;
}



/* g95_scanner_init_1()-- Main scanner initialization. */

void g95_scanner_init_1(void) {

    file_head = NULL;
    line_head = NULL;
    line_tail = NULL;
    file_stack = NULL;

    end_flag = 0;
}



/* g95_scanner_done_1()-- Main scanner destructor */

void g95_scanner_done_1(void) {
g95_linebuf *b;
g95_cpp_macro *p;
g95_file *f;

    while(line_head != NULL) {
	b = line_head->next;
	g95_free(line_head);
	line_head = b;
    }

    while(file_head != NULL) {
	f = file_head->next;
	g95_free(file_head);
	file_head = f;
    }

    while(cpp_macro_head != NULL) {
	p = cpp_macro_head;
	cpp_macro_head = p->next;
	g95_free(p);
    }
}



/* g95_first_line()-- Return a pointer to the first line. */

g95_linebuf *g95_first_line(void) {

    return line_head;
}



/* g95_at_end()-- Test to see if we're at the end of the main source file. */

int g95_at_end(void) {

    return end_flag;
}



/* g95_at_eof()-- Test to see if we're at the end of the current file */

int g95_at_eof(void) {

    if (g95_at_end() || line_head == NULL ||   /* Null file */
	g95_current_locus.lb == NULL)
	return 1;

    return 0;
}



/* g95_at_bol()-- Test to see if we're at the beginning of a new line */

int g95_at_bol(void) {

    return g95_at_eof()
	? 1
	: (g95_current_locus.nextc == g95_current_locus.lb->line);
}



/* g95_at_eol()-- Test to see if we're at the end of a line */

int g95_at_eol(void) {

    return g95_at_eof()
	? 1
	: *g95_current_locus.nextc == '\0';
}



/* check_bad_line()-- Make sure the current line isn't a special case
 * that isn't allowed (3.3.1.3). */

static void check_bad_line(void) {
int seen_amp;
char *p;

    if (bad_line || g95_option.fmode != 95)
	return;

    seen_amp = 0;
    if (g95_current_locus.lb == NULL)
	return;

    for(p=g95_current_locus.lb->line; *p; p++) {
	if (*p == ' ')
	    continue;

	if (*p == '!' && seen_amp)
	    break;

	if (seen_amp)
	    return;

	if (*p == '&')
	    seen_amp = 1;
	else
	    return;
    }

    if (!seen_amp)
	return;

    g95_error_now("Bad continuation line at %C");
    bad_line = 1;
}


/* g95_advance_line()-- Advance the current line pointer to the next line */

void g95_advance_line(void) {

    if (g95_at_end())
	return;

    if (g95_current_locus.lb == NULL) {
	end_flag = 1;
	return;
    }

    g95_current_locus.lb = g95_current_locus.lb->next;
    g95_current_locus.column = 1;

    if (g95_current_locus.lb != NULL)
	g95_current_locus.nextc = g95_current_locus.lb->line;

    else {
	g95_current_locus.nextc = NULL;
	end_flag = 1;
    }
}



/* next_char()-- Get the next character from the input, advancing the
 * current locus.  When we hit the end of the line or the end of the
 * file, we start returning a '\n' in order to complete the current
 * statement.  No fortran line conventions are implemented here.
 *
 * Requiring explicit advances to the next line prevents the parse
 * pointer from being on the wrong line if the current statement ends
 * prematurely. */

static int next_char(int comment) {
static g95_linebuf *where;
int c;

    if (g95_current_locus.nextc == NULL) 
	return '\n';

    c = *g95_current_locus.nextc++;

    if (c == '\0') {
	g95_current_locus.nextc--;    /* Stay stuck on this line */

	c = (g95_current_form == FORM_FIXED && g95_current_locus.column <= 72)
	    ? ' ' : '\n';

    } else if (g95_current_locus.column > line_width) {
	while(*g95_current_locus.nextc != '\0')
	    g95_current_locus.nextc++;

	c = '\n';

	if (g95_option.line_truncation && !comment &&
	    g95_current_locus.lb != where) {
	    g95_warning_now(115, "Line %d of %s is being truncated",
			    g95_current_locus.lb->linenum,
			    g95_current_locus.lb->file->filename);
	    where = g95_current_locus.lb;
	}
    }

    if (g95_current_form == FORM_FIXED && g95_current_locus.column == 1 &&
	g95_option.d_comment && (c == 'd' || c == 'D'))
	c = ' ';

    g95_current_locus.column++;
    return c;
}



/* g95_skip_comment_line()-- Skip a comment.  When we come here the parse
 * pointer is positioned immediately after the comment character.  If
 * we ever implement compiler directives within comments, here is
 * where we parse the directive. */

void g95_skip_comment_line(void) {
char c;

    do
	c = next_char(1);
    while(c != '\n');

    g95_advance_line();
}



/* skip_fixed_comments()-- Skip comment lines in fixed source mode.
 * We have the same rules as in skip_free_comment(), except that we
 * can have a 'c', 'C' or '*' in column 1. and a '!' cannot be in
 * column 6. */

static void skip_fixed_comments(void) {
g95_locus start;
int col;
char c;

    for(;;) {
	start = g95_current_locus;
	if (g95_at_eof())
	    break;

	c = next_char(1);
	if (c == '\n') {
	    g95_advance_line();
	    continue;
	}

	if (c == '!' || c == 'c' || c == 'C' || c == 'd' || c == 'D' ||
	    c == '*') {
	    g95_skip_comment_line();
	    continue;
	}

	col = 1;
	do {
	    c = next_char(1);
	    col++;
	} while(g95_is_whitespace(c));

	if (c == '\n') {
	    g95_advance_line();
	    continue;
	}

	if (col != 6 && c == '!') {
	    g95_skip_comment_line();
	    continue;
	}

	break;
    }

    g95_current_locus = start;
}



/* skip_free_comments()-- Comment lines are null lines, lines containing
 * only blanks or lines on which the first nonblank line is a '!' */

static void skip_free_comments(void) {
g95_locus start;
char c;

    for(;;) {
	start = g95_current_locus;
	if (g95_at_eof())
	    break;

	do
	    c = next_char(1);
	while (g95_is_whitespace(c));

	if (c == '\n') {
	    g95_advance_line();
	    continue;
	}

	if (c == '!') {
	    g95_skip_comment_line();
	    continue;
	}

	break;
    }

    g95_current_locus = start;
}



/* g95_skip_comments()-- Skips the current line if it is a comment.
 * Assumes that we are at the start of the current line. */

void g95_skip_comments(void) {

    if (!g95_at_bol() || g95_current_form == FORM_FREE)
	skip_free_comments();

    else
	skip_fixed_comments();
}



/* g95_next_char_literal()-- Get the next character from the input,
 * taking continuation lines and end-of-line comments into account.
 * This implies that comment lines between continued lines must be
 * eaten here.  For higher-level subroutines, this flattens continued
 * lines into a single logical line.  The in_string flag denotes
 * whether we're inside a character context or not. */

int g95_next_char_literal(int in_string) {
static int bad_continue=0;
g95_locus old_loc;
int i, c;

    continue_flag = 0;

    if (g95_current_form == FORM_FREE && g95_at_bol())
	check_bad_line();

restart:
    c = next_char(0);
    if (g95_at_end())
	return c;

    if (g95_current_form == FORM_FREE) {
	if (!in_string && c == '!') {   /* This line can't be continued */
	    do
		c = next_char(1);
	    while(c != '\n');

	    goto done;
	}

	if (c != '&')
	    goto done;

/* If the next nonblank character is a ! or \n, we've got a
 * continuation line. */

	old_loc = g95_current_locus;

	c = next_char(1);
	while(g95_is_whitespace(c))
	    c = next_char(1);

/* Character constants to be continued cannot have commentary after the '&' */

	if (in_string && c != '\n') {
	    g95_current_locus = old_loc;
	    c = '&';
	    goto done;
	}

	if (c != '!' && c != '\n') {
	    g95_current_locus = old_loc;
	    c = '&';
	    goto done;
	}

	continue_flag = 1;
	if (c == '!')
	    g95_skip_comment_line();

	else
	    g95_advance_line();

/* We've got a continuation line and need to find where it continues.
 * First eat any comment lines. */

	g95_skip_comments();

/* Now that we have a non-comment line, probe ahead for the first
 * non-whitespace character.  If it is another '&', then reading
 * starts at the next character, otherwise we must back up to where
 * the whitespace started and resume from there. */

	old_loc = g95_current_locus;

	c = next_char(1);
	while(g95_is_whitespace(c))
	    c = next_char(1);

	if (c != '&') {
	    if (in_string && !bad_continue) {
		g95_current_locus.nextc--;
		g95_error_now("Missing '&' in continued character constant "
			      "at %C");
		g95_current_locus.nextc++;
		bad_continue = 1;
	    }

	    g95_current_locus = old_loc;

	} else if (G95_STRICT_F() && !bad_continue) {
	    g95_current_locus.nextc--;
	    g95_error_now("Continuation line at %C cannot begin with '&' in "
			  "F mode");
	    g95_current_locus.nextc++;
	    bad_continue = 1;
	}

    } else {   /* Fixed form continuation */
	if (!in_string && c == '!')   /* skip comment at end of line */
	    do
		c = next_char(1);
	    while(c != '\n');

	if (c != '\n')
	    goto done;

	continue_flag = 1;
	old_loc = g95_current_locus;

	g95_advance_line();
	g95_skip_comments();

/* See if this line is a continuation line */

	for(i=0; i<5; i++) {
	    c = next_char(0);
	    if (c != ' ')
		goto not_continuation;
	}
      
	c = next_char(0);
	if (c == '0' || c == ' ')
	    goto not_continuation;
    }

/* Ready to read first character of continuation line, which might be
 * another continuation line! */

    goto restart;

not_continuation:
    c = '\n';
    g95_current_locus = old_loc;

done:
    continue_flag = 0;
    return c;
}



/* g95_next_char()-- Get the next character of input, folded to
 * lowercase.  In fixed form mode, we also ignore spaces.  When
 * matcher subroutines are parsing character literals, they have to
 * call g95_next_char_literal(). */

int g95_next_char(void) {
int c;

    do
	c = g95_next_char_literal(0);
    while(g95_current_form == FORM_FIXED && g95_is_whitespace(c));

    if ('A' <= c && c <= 'Z')
	c = c - 'A' + 'a';

    return c;
}



int g95_peek_char(void) {
g95_locus old_loc;
int c;

    old_loc = g95_current_locus;
    c = g95_next_char();
    g95_current_locus = old_loc;

    return c;
}



/* g95_error_recovery()-- Recover from an error.  We try to get past
 * the current statement and get lined up for the next.  The next
 * statement follows a '\n' or a ';'.  We also assume that we are not
 * within a character constant, and deal with finding a '\'' or
 * '"'. */

void g95_error_recovery(void) {
char c, delim;
  
    if (g95_at_eof())
	return;

    for(;;) {
	c = g95_next_char();
	if (c == '\n' || c == ';')
	    break;

	if (c != '\'' && c != '"') {
	    if (g95_at_eof())
		break;

	    continue;
	}

	delim = c;

	for(;;) {
	    c = next_char(1);

	    if (c == delim)
		break;

	    if (c == '\n')
		goto done;

	    if (c == '\\') {
		c = next_char(1);
		if (c == '\n')
		    goto done;
	    }
	}

	if (g95_at_eof())
	    break;
    }

done:
    if (c == '\n')
	g95_advance_line();
}



/* g95_gobble_whitespace()-- Read ahead until the next character to be
 * read is not whitespace */

void g95_gobble_whitespace(void) {
g95_locus old_loc;
int c;

    do {
	old_loc = g95_current_locus;
	c = g95_next_char_literal(0);
    } while (g95_is_whitespace(c));

    g95_current_locus = old_loc;
}



/* include_line()-- Checks a line buffer to see if it is an include
 * line.  If so, we call load_file() recursively to load the included
 * file.  We never return a syntax error because a statement like
 * "include = 5" is perfectly legal.  We return zero if no include was
 * processed or nonzero if we matched an include. */

static int include_line(char *line) {
char quote, *p, *start, *end;
int i;

    if (G95_STRICT_F())
	return 0;

    p = line;
    i = 0;

    while(p[i] == ' ' || p[i] == '\t')
	i++;

    if (strncasecmp(p+i, "include", 7) != 0)
	return 0;

    i += 7;
    while(p[i] == ' ' || p[i] == '\t')
	i++;

    quote = p[i++];
    if (quote != '"' && quote != '\'')
	return 0;

    start = p + i;

    while(p[i] != quote && p[i] != '\0')
	i++;

    if (p[i] == '\0')
	return 0;

    end = p + i;
    i++;

    while(p[i] == ' ' || p[i] == '\t')
	i++;

    if (p[i] != '\0' && p[i] != '!' && i < line_width)
	return 0;

    /* We have an include line at this point */

    *end = '\0';   /* OK to trash the buffer */
    seen_include = 1;

    load_file(start, start, 1);
    return 1;
}



/* new_line()-- Decide what to do with a new line of input.  The
 * choices here are an include line or a source line. */

static void new_line(char *line) {
g95_linebuf *b;
int i;

    if (include_line(line))
	return;

    if (g95_option.preprocess_only) {
	puts(line);
	return;
    }

    if (g95_current_form == FORM_FREE)
	i = -1;

    else {
	for(i=0;;) {
	    if (line[i] == '\t')
		break;

	    if (line[i] == '\0' || ++i == 6) {
		i = -1;
		break;
	    }
	}
    }

    if (i == -1) {
	b = g95_getmem(sizeof(g95_linebuf) + strlen(line));
	strcpy(b->line, line);

    } else {
	b = g95_getmem(sizeof(g95_linebuf) + 6 - i + strlen(line));

	memcpy(b->line, line, i);
	memset(b->line+i, ' ', 6-i);
	strcpy(b->line+6, line+i+1);
    }

    b->linenum = current_file->line;
    b->file = current_file;
    b->name = current_name;

    if (line_head == NULL)
	line_head = b;

    else
	line_tail->next = b;

    line_tail = b;
}



/* fgetc0()-- Wrapper for fgetc(). */

static int fgetc0(FILE *input) {
int c;

    c = fgetc(input);
    if (c == EOF)
	current_file->eof_flag = 1;

    return c;
}



/* load_line()-- Load a single line into a buffer.  We truncate lines
 * that are too long. */

static void load_line(FILE *input, char *buffer) {
int c, i;

    i = 0;
    for(;;) {
	c = fgetc0(input);

	if (c == EOF || c == '\n') break;
	if (c == '\r' || c == '\0') continue;   /* Gobble characters */

	if (c == '\032') {         /* Control-Z ends the file */
	    while(fgetc0(input) != EOF);
	    break;
	}

	buffer[i] = c;

	if (i >= line_width+1) {
	    do
		c = fgetc0(input);
	    while(c != '\n' && c != EOF);

	    break;
	}

	i++;
    }

    buffer[i] = '\0';
}



/* new_file()-- Get a g95_file structure and initialize it */

static void new_file(char *name) {
g95_file *f;

    f = g95_getmem(sizeof(g95_file));
    f->save = current_name;
    f->filename = current_name = g95_get_string(name);

    f->next = file_head;
    file_head = f;

    f->included_by = current_file;
    if (current_file != NULL)
	f->inclusion_line = current_file->line;

    f->up = current_file;
    current_file = f;

    include_level++;
    if (include_level > 500)
      g95_fatal_error("Too many include files.  Recursive includes?");
}



/* leave_file()-- Leave the current file */

static void leave_file(void) {

    current_name = current_file->save;
    current_file = current_file->up;

    include_level--;
}



#if IN_GCC

/* file_change()-- Callback that the C preprocessor uses to tell us
 * that it has changed files on us. */

static void file_change(cpp_reader *input, const struct line_map *map) {

    if (map == NULL)
	return;

  /* Deal with some cpp weirdness.  Lines are off by one if a file
   * change comes at the very start of a source file. */

    line_offset = (line_head == NULL) ? 1 : 0;

    switch(map->reason) {
    case LC_RENAME:
	current_name = (char *) map->to_file;
	current_file->line = map->to_line;
	g95_dependent_file(current_name);

	if (line_head == NULL) {
	    main_input_filename = map->to_file;
	    initial_switch = 1;
	}

	break;

    case LC_ENTER:
	current_file->line++;
	new_file((char *) map->to_file);

	g95_dependent_file((char *) map->to_file);
	break;

    case LC_LEAVE:
	leave_file();
	break;
    }
}



/* source_line()-- Calculate the source line of a preprocessed file. */

static int source_line(cpp_reader *input, const struct line_map *map,
		       source_location n) {
static const char *initial = NULL;
int line;

    if (initial == NULL)
	initial = map->to_file;

    line = SOURCE_LINE(map, n);

    if (map->to_file == initial)
	line--;

    if (seen_include)
	line--;

    return line + line_offset;
}



/* scan_traditional()-- Scan with the traditional C preprocessor */

static void scan_traditional(cpp_reader *input) {
const struct line_map *map;
int len;
char c;

    while(_cpp_read_logical_line_trad(input)) {
	map = linemap_lookup(&line_table, input->out.first_line);
	current_file->line = source_line(input, map, input->out.first_line);

	len = input->out.cur - input->out.base;

	c = input->out.base[len];
	input->out.base[len] = '\0';

	new_line((char *) input->out.base);
	input->out.base[len] = c;
    }
}



/* scan_regular()-- Scan with the regular C preprocessor.  Much of
 * this code is transliterated from scan_translation_unit(). */

static void scan_regular(cpp_reader *input) {
const cpp_token *token, *source, *prev;
const struct line_map *map;
char *p, buffer[9999];
int first, flag;

    p = buffer;
    source = NULL;
    prev = NULL;
    flag = 0;
    first = 1;

    for(;;) {
	token = cpp_get_token(input);

	if (token->flags & BOL) {
	eol:
	    flag = 0;
	    *p++ = '\0';

	    if (!first)
		new_line(buffer);

	    first = 0;
	    p = buffer;

	    map = linemap_lookup(&line_table, token->src_loc);
	    current_file->line = source_line(input, map, token->src_loc);

	    if (token->type == CPP_EOF)
		current_file->line++;
	}

	if (token->type == CPP_PADDING) {
	    if (flag)
		goto eol;

	    flag = 1;
	    if (source == NULL ||
		(!(source->flags & PREV_WHITE) && token->val.source == NULL))
		source = token->val.source;

	    continue;
	}

	if (token->type == CPP_EOF)
	    break;

	if (flag) {
	    if (source == NULL)
		source = token;

	    if (source->flags & PREV_WHITE
		|| (prev && cpp_avoid_paste(input, prev, token))
		|| (prev == NULL && token->type == CPP_HASH))

		*p++ = ' ';

	} else if (token->flags & PREV_WHITE)
	    *p++ = ' ';

	flag = 0;
	source = NULL;
	prev = token;

#if TARGET_GCC_VERSION >= 410
	p = cpp_spell_token(input, token, p, false);
#else
	p = (char *) cpp_spell_token(input, token, (unsigned char *) p);
#endif
    }
}


#include "macros.h"


#if TARGET_GCC_VERSION >= 401

/* my_cpp_error()-- Throw a CPP error.  Most of this code is copied
 * from libcpp/cpp_error().  Our goal is to flag an error here so that
 * we don't produce an object file. */

static void my_cpp_error(cpp_reader *input, int level, const char *msgid,
			 va_list *ap) {
source_location src_loc;

    if (CPP_OPTION(input, traditional)) {
	if (input->state.in_directive)
	    src_loc = input->directive_line;
	else
	    src_loc = input->line_table->highest_line;
  
    } else
	src_loc = input->cur_token[-1].src_loc;

    if (_cpp_begin_message(input, level, src_loc, 0)) {
	vfprintf(stderr, msgid, *ap);
	putc('\n', stderr);
    }

    if (level == CPP_DL_ERROR)
	g95_flag_error();
}

#endif



/* search_includes()-- Search include directories.  Leaves path as a
 * found file, or one that will not be found. */

static void search_includes(char *path, char *name1, char *name2) {
struct stat stat_buf;
g95_directorylist *d;

    strcpy(path, name2);

    if (current_file == NULL || stat(path, &stat_buf) == 0)
	return;

    for(d=g95_option.include_dirs; d; d=d->next) {
	if (g95_search_prefix != NULL && d->path[0] != '/') {
	    strcat(path, g95_search_prefix);
	    strcat(path, "/");

	    strcat(path, d->path);
	    strcat(path, name2);

	    if (stat(path, &stat_buf) == 0)
		break;
	}
    }

    if (d == NULL) {
	if (g95_search_prefix == NULL)
	    strcpy(path, name2);

	else {
	    strcpy(path, g95_search_prefix);
	    strcat(path, "/");
	    strcat(path, name2);
	}

	if (stat(path, &stat_buf) != 0)
	    g95_fatal_error("File '%s' not found", name1);
    }
}



/* load_file_cpp()-- Load a file using the C preprocessor */

static try load_file_cpp(char *name1, char *name2) {
char **r, path[PATH_MAX], *source_name;
cpp_dir *head, *tail, *q;
g95_directorylist *d;
cpp_reader *input;
cpp_callbacks *cb;
g95_cpp_macro *p;

    input = cpp_create_reader(CLK_GNUC89, ident_hash, &line_table);

    search_includes(path, name1, name2);

    source_name = (char *) cpp_read_main_file(input, path);
    if (source_name == NULL)
	return FAILURE;

    new_file(source_name);

    cb = cpp_get_callbacks(input);
    cb->file_change = file_change;

    cpp_post_options(input);
    cpp_set_lang(input, CLK_STDC94);

    CPP_OPTION(input, traditional) = g95_option.traditional;
    CPP_OPTION(input, trigraphs) = 0;
    CPP_OPTION(input, warn_trigraphs) = 0;

#if TARGET_GCC_VERSION >= 403
    cb->error = my_cpp_error;
    CPP_OPTION(input, client_diagnostic) = 1;
#endif

    if (current_file->up == NULL) {
	cpp_init_builtins(input, 1);

	for(p=cpp_macro_head; p; p=p->next)
	    if (p->define)
		cpp_define(input, p->name);
	    else
		cpp_undef(input, p->name);

	for(r=builtin_macros; *r != NULL; r++)
	    _cpp_define_builtin(input, *r);

	linemap_add(input->line_table, LC_RENAME, 0, source_name, 2);
    }

    head = tail = NULL;

    for(d=g95_option.include_dirs; d; d=d->next) {
	if (g95_search_prefix != NULL && d->path[0] != '/') {
	    q = g95_getmem(sizeof(cpp_dir));

	    q->len  = strlen(g95_search_prefix) + 2 + strlen(d->path);
	    q->name = g95_getmem(q->len);

	    strcpy(q->name, g95_search_prefix);
	    strcat(q->name, "/");
	    strcat(q->name, d->path);

	    q->user_supplied_p = true;

	    if (head == NULL)
		head = q;
	    else
		tail->next = q;

	    tail = q;
	}

	q = g95_getmem(sizeof(cpp_dir));

	q->name = d->path;
	q->len  = strlen(q->name);

	q->user_supplied_p = true;

	if (head == NULL)
	    head = q;
	else
	    tail->next = q;

	tail = q;
    }

    cpp_set_include_chains(input, head, NULL, 0);

    if (cpp_get_options(input)->traditional)
	scan_traditional(input);

    else
	scan_regular(input);

    cpp_destroy(input);

    if (!initial_switch)
	g95_dependent_file(name1);

    return SUCCESS;
}
#endif



/* load_file_nocpp()-- Load a file without running it through the C
 * preprocessor. */

static try load_file_nocpp(char *name1, char *name2) {
char line[G95_MAX_LINE+10]; 
FILE *fp;
int len;
try t;

    new_file(name1);
    current_file->line = 1;
    current_file->eof_flag = 0;
    t = FAILURE;

    if (current_file->up == NULL) {
	fp = g95_open_file(name2);
	if (fp == NULL) {
	    g95_error_now("Can't open file '%s'", name2);
	    goto done;
	}

    } else {
	fp = g95_open_included_file(name2);
	if (fp == NULL) {
	    g95_error_now("Can't open included file '%s'", name2);
	    goto done;
	}
    }

    for(;;) {
	load_line(fp, line);

	len = strlen(line);
	if (current_file->eof_flag && len == 0)
	    break;

	new_line(line);
	current_file->line++;
    }

    fclose(fp);
    t = SUCCESS;

done:
    leave_file();

    return t;
}



/* load_file()-- Recursive function for loading the next file. */

static try load_file(char *filename1, char *filename2, int inc_line) {

#if IN_GCC
    if (g95_option.cpp && !inc_line)
	return load_file_cpp(filename1, filename2);
#endif

    return load_file_nocpp(filename1, filename2);
}



/* form_from_filename() -- determines the source form from the
 * filename extension.  We assume case insensitivity. */

static g95_source_form form_from_filename(char *filename) {

static struct {
    char *extension;
    g95_source_form form;
} exttype[] = {
    {".f90", FORM_FREE     },
    {".f95", FORM_FREE     },
    {".f03", FORM_FREE     },
    {".f",   FORM_FIXED    },
    {".for", FORM_FIXED    },
    {".fpp", FORM_FIXED    },
    {"",     FORM_UNKNOWN  } };   /* sentinel value */

g95_source_form form;
char *ext;
int i;

    ext = strrchr(filename, '.');
    if (ext == NULL)
	return FORM_UNKNOWN;

    form = FORM_UNKNOWN;
    for(i=0; exttype[i].form!=FORM_UNKNOWN; i++)
	if (strcasecmp(ext, exttype[i].extension) == 0) {
	    form = exttype[i].form;
	    break;
	}

    return form;
}



/* cpp_from_filename()-- Figure out if we should invoke the C
 * preprocessor depending on the end of the filename.  Returns nonzero
 * if we should. */

static int cpp_from_filename(char *name) {
static char *cpp_ext[] = { ".F03", ".F90", ".F95", ".F", ".FOR", ".fpp",
			   ".FPP", NULL};
char *ext;
int i;

    ext = strrchr(name, '.');
    if (ext == NULL)
	return 0;

    for(i=0; cpp_ext[i]!=NULL; i++)
	if (strcmp(ext, cpp_ext[i]) == 0)
	    return 1;

    return 0;
}


/* init_search_path()-- Initialize the search path from the source
 * filename. */

static void init_search_path(void) {
char *p;
int n;

    p = strchr(g95_source_file, '\0');

    while(p > g95_source_file && !g95_directory_separator(*p))
	p--;

    if (p == g95_source_file)
	return;

    n = p - g95_source_file;
    g95_search_prefix = g95_getmem(n+1);

    memcpy(g95_search_prefix, g95_source_file, n);
    g95_search_prefix[n] = '\0';
}



/* g95_new_file()-- Top level subroutine for opening the initial
 * source file and reading it in. */

try g95_new_file(void) {
char *source;
try t;

    if (g95_source_file[0] == '\0') {
	g95_source_file = "<stdin>";
	source = "";

    } else
	source = g95_source_file;

    if (g95_option.form != FORM_UNKNOWN)
	g95_current_form = g95_option.form;

    else {
	g95_current_form = form_from_filename(g95_source_file);

	if (g95_current_form == FORM_UNKNOWN) {
	    g95_current_form = FORM_FREE;
	    g95_warning_now(116, "Reading file %s as free form",
			    g95_source_file);
	}
    }

    if (g95_current_form == FORM_FREE)
	g95_option.line_truncation = 1;

    line_width = (g95_current_form == FORM_FREE)
	? ((g95_option.huge_line) ? G95_MAX_LINE : 132)
	: g95_option.fixed_line_length;

    if (g95_option.cpp == -1)
	g95_option.cpp = cpp_from_filename(g95_source_file);

    init_search_path();

    t = load_file(g95_source_file, source, 0);

    if (g95_option.preprocess_only)
	exit(0);

    g95_current_locus.lb = line_head;
    g95_current_locus.column = 1;
    g95_current_locus.nextc = (line_head == NULL)
	? NULL
	: line_head->line;

    if (g95_current_form == FORM_FIXED && G95_STRICT_F())
	g95_fatal_error("Fixed source not allowed in F mode");

#if 0
    for(; line_head; line_head=line_head->next)
	g95_status("%s:%3d \"%s\"\n",
		   line_head->file->filename, line_head->linenum,
		   line_head->line);

    exit(0);
#endif

    if (g95_search_prefix != NULL) {
	g95_free(g95_search_prefix);
	g95_search_prefix = NULL;
    }

    return t;
}
