/* Miscellaneous things
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

/* misc.c-- Miscellaneous stuff that doesn't fit anywhere else */

#include <stdlib.h>
#include <string.h>

#include "g95.h"

#ifdef __GLIBC__
#include <mcheck.h>
#endif

int isalpha(int);

static g95_depfile *infiles, *outfiles = NULL;
static g95_cbinding *cbinding_root;



static mstring flavors[] = {
    minit("UNKNOWN-FL",  FL_UNKNOWN),      minit("PROGRAM",     FL_PROGRAM),
    minit("BLOCK-DATA",  FL_BLOCK_DATA),   minit("MODULE",      FL_MODULE),
    minit("VARIABLE",    FL_VARIABLE),     minit("PARAMETER",   FL_PARAMETER),
    minit("LABEL",       FL_LABEL),        minit("PROCEDURE",   FL_PROCEDURE),
    minit("DERIVED",     FL_DERIVED),      minit("NAMELIST",    FL_NAMELIST),
    minit(NULL, -1)
},

intents[] = {
    minit("UNKNOWN-INTENT", INTENT_UNKNOWN),  minit("IN", INTENT_IN),
    minit("OUT", INTENT_OUT),                 minit("INOUT", INTENT_INOUT),
    minit(NULL, -1)
},

procedures[] = {
    minit("UNKNOWN-PROC",   PROC_UNKNOWN),   minit("MODULE-PROC", PROC_MODULE),
    minit("INTERNAL-PROC",  PROC_INTERNAL),  minit("DUMMY-PROC",  PROC_DUMMY),
    minit("INTRINSIC-PROC", PROC_INTRINSIC),
    minit("EXTERNAL-PROC",  PROC_EXTERNAL),
    minit("STATEMENT-PROC", PROC_ST_FUNCTION), minit(NULL, -1)
},

accessibility[] = {
    minit("UNKNOWN-ACCESS", ACCESS_UNKNOWN),   minit("PUBLIC", ACCESS_PUBLIC),
    minit("PRIVATE", ACCESS_PRIVATE),          minit(NULL, -1)
};




/* g95_getmem()-- Get a block of memory.  Many callers assume that the
 * memory we return is zeroed. */

void *g95_getmem(size_t n) {
void *p;

    if (n == 0)
	return NULL;

    p = calloc(n, 1);
    if (p == NULL)
	g95_fatal_error("Out of memory-- malloc() failed");

    return p;
}



#define temp free
#undef free

void g95_free(void *p) {

    if (p != NULL)
	free(p);
}

#define free temp
#undef temp



/* misc_done()-- Clean up */

static void misc_done(void) {
g95_depfile *p, *q;

#ifdef IN_GCC
extern char *aux_base_name;

    if (g95_option.deps) {
	g95_status("%s.o", aux_base_name == NULL ? "a.out" : aux_base_name);

	for(p=outfiles; p; p=p->next)
	    g95_status(" %s", p->name);

	g95_status(":");

	for(p=infiles; p; p=p->next)
	    g95_status(" %s", p->name);

	g95_status_char('\n');
    }
#endif

    for(p=infiles; p; p=q) {
	q = p->next;
	g95_free(p);
    }

    for(p=outfiles; p; p=q) {
	q = p->next;
	g95_free(p);
    }
}


/* g95_clear_ts()-- Initialize a typespec to BT_UNKNOWN. */

void g95_clear_ts(g95_typespec *ts) {

    ts->type = BT_UNKNOWN;
    ts->kind = 0;
    ts->derived = NULL;
    ts->cl = NULL;
    ts->interface = NULL;
}


/* g95_dependent_file()-- Record the name of a dependent file. */

void g95_dependent_file(char *name) {
g95_depfile *p;

    for(p=outfiles; p; p=p->next)
	if (strcmp(p->name, name) == 0)
	    return;

    for(p=infiles; p; p=p->next)
	if (strcmp(p->name, name) == 0)
	    break;

    if (p == NULL) {
	p = g95_getmem(sizeof(g95_depfile));

	p->name = g95_get_string(name);
	p->next = infiles;
	infiles = p;
    }
}



/* g95_target_file()-- Record the name of a target file. */

void g95_target_file(char *name) {
g95_depfile *p;

    for(p=outfiles; p; p=p->next)
	if (strcmp(p->name, name) == 0)
	    break;

    if (p == NULL) {
	p = g95_getmem(sizeof(g95_depfile));

	p->name = g95_get_string(name);
	p->next = outfiles;
	outfiles = p;
    }
}



/* g95_open_file()-- Open a file for reading */

FILE *g95_open_file(char *name) {
FILE *fp;

    if (name[0] == '\0')
	return stdin;

    fp = fopen(name, "r");
    if (fp != NULL)
	g95_dependent_file(name);

    return fp;
}



/* g95_directory_separator()-- Determine if the character is a
 * directory separator character.  Return nonzero if so. */

int g95_directory_separator(char c) {

    if (c == '/')
	return 1;

#ifdef HAVE_WINDOWS
    if (c == '\\')
	return 1;
#endif

    return 0;
}


#if HAVE_WINDOWS
#define INCLUDE_SEPARATOR ';'
#else
#define INCLUDE_SEPARATOR ':'
#endif


/* absolute_path()-- Return nonzero if the path is absolute. */

static int absolute_path(char *path) {

#if HAVE_WINDOWS
    if (path[0] == '\\')
	return 1;

    if ((('a' <= path[0] && path[0] <= 'z') ||
	 ('A' <= path[0] && path[0] <= 'Z')) &&
	path[1] == ':' && (path[2] == '\\' || path[2] == '/'))
	return 1;

    return 0;

#else
    return path[0] == '/';
#endif
}



/* g95_open_included_file()-- opens file for reading, searching
 * through the include directories given if necessary */

FILE *g95_open_included_file(char *name) {
char fullname[2*PATH_MAX], *intrinsic_paths[] = { "/usr/include/", NULL };
g95_directorylist *p;
char **q, *e, *r;
FILE *f;
int m;

    if (absolute_path(name))
	return g95_open_file(name);

    /* See if the file can be found in the path of the main source file. */

    e = strchr(g95_source_file, '\0');

    for(;;) {
	if (g95_directory_separator(*e)) {
	    m = e - g95_source_file + 1;
	    memcpy(fullname, g95_source_file, m);
	    strcpy(fullname+m, name);

	    f = g95_open_file(fullname);
	    if (f != NULL)
		return f;

	    break;
	}

	if (e-- == g95_source_file)
	    break;
    }

    /* Now check the current directory */

    if (g95_search_prefix == NULL)
	f = g95_open_file(name);

    else {
	strcpy(fullname, g95_search_prefix);
	strcat(fullname, "/");
	strcat(fullname, name);

	f = g95_open_file(name);
    }

    if (f != NULL)
	return f;

    /* Check user-specified directories */

    for(p=g95_option.include_dirs; p; p=p->next) {
	if (g95_search_prefix != NULL && p->path[0] != '/') {
	    strcpy(fullname, g95_search_prefix);
	    strcat(fullname, "/");

	    strcat(fullname, p->path);
	    strcat(fullname, name);

	    f = g95_open_file(fullname);
	    if (f != NULL)
		return f;
	}

	strcpy(fullname, p->path);
	strcat(fullname, name);

	f = g95_open_file(fullname);
	if (f != NULL)
	    return f;
    }

    /* Now the environment-specified include directories */

    e = getenv("G95_INCLUDE_PATH");
    while(e != NULL) {
	strcpy(fullname, e);
	r = strchr(fullname, INCLUDE_SEPARATOR);
	if (r != NULL)
	    *r = '\0';

	strcat(fullname, "/");
	strcat(fullname, name);
	f = g95_open_file(fullname);
	if (f != NULL)
	    return f;

	e = strchr(e, INCLUDE_SEPARATOR);
	if (e != NULL)
	    e++;
    }

    /* Getting desperate now, try the system directories */

    for(q=intrinsic_paths; *q!=NULL; q++) {
	strcpy(fullname, *q);
	strcat(fullname, name);

	f = g95_open_file(fullname);
	if (f != NULL)
	    return f;
    }

    /* Nooooooooooooo */

    return NULL;
}



/* g95_article()-- Given a word, return the correct article */

char *g95_article(char *word) {
char *p;

    switch(*word) {
    case 'a':  case 'e':  case 'i':  case 'o':  case 'u':
    case 'A':  case 'E':  case 'I':  case 'O':  case 'U':
	p = "an";
	break;

    default:
	p = "a";
    }

    return p;
}



/* g95_typename()-- Return a string for each type */

char *g95_basic_typename(bt type) {
char *p;

    switch(type) {
    case BT_INTEGER:    p = "INTEGER";    break;
    case BT_REAL:       p = "REAL";       break;
    case BT_COMPLEX:    p = "COMPLEX";    break;
    case BT_LOGICAL:    p = "LOGICAL";    break;
    case BT_CHARACTER:  p = "CHARACTER";  break;
    case BT_DERIVED:    p = "DERIVED";    break;
    case BT_PROCEDURE:  p = "PROCEDURE";  break;
    case BT_UNKNOWN:    p = "UNKNOWN";    break;
    default:
	g95_internal_error("g95_basic_typename(): Undefined type");
    }

    return p;
}



/* g95_mode_name()-- Return string that gives the language mode */

char *g95_mode_name(void) {

    switch(g95_option.fmode) {
    case 0:     return "Regular";
    case 95:    return "strict f95";
    case 96:    return "strict F";
    case 2003:  return "strict f2003";
    }

    g95_internal_error("g95_mode_name(): Bad mode");
    return NULL;
}



/* g95_typename()-- Return a string describing the type and kind of a
 * typespec.  Because we return alternating buffers, this subroutine
 * can appear twice in the argument list of a single statement. */

char *g95_typename(g95_typespec *ts) {
static char buffer1[60], buffer2[60];
static int flag = 0;
char *buffer;

    buffer = flag ? buffer1 : buffer2;
    flag = !flag;

    switch(ts->type) {
    case BT_INTEGER:    sprintf(buffer, "INTEGER(%d)", ts->kind);    break;
    case BT_REAL:       sprintf(buffer, "REAL(%d)", ts->kind);       break;
    case BT_COMPLEX:    sprintf(buffer, "COMPLEX(%d)", ts->kind);    break;
    case BT_LOGICAL:    sprintf(buffer, "LOGICAL(%d)", ts->kind);    break;
    case BT_CHARACTER:  sprintf(buffer, "CHARACTER(%d)", ts->kind);  break;
    case BT_DERIVED:    sprintf(buffer, "TYPE(%s)", ts->derived->name); break;
    case BT_PROCEDURE:  strcpy(buffer,  "PROCEDURE");  break;
    case BT_UNKNOWN:    strcpy(buffer,  "UNKNOWN");    break;
    default:
	g95_internal_error("g95_typename(): Undefined type");
    }

    return buffer;
}



/* g95_show_locus()-- Show where something is */

void g95_show_locus(g95_locus *where) {

    g95_status("'%s:%d'", where->lb->file->filename, where->lb->linenum);
}



/* g95_code2string()-- Given an mstring array and a code, locate the
 * code in the table, returning a pointer to the string. */

char *g95_code2string(mstring *m, int code) {

    while(m->string != NULL) {
	if (m->tag == code)
	    return m->string;

	m++;
    }

    g95_internal_error("g95_code2string(): Bad code");
    return NULL;
}



/* g95_string2code()-- Given an mstring array and a string, returns
 * the value of the tag field.  Returns the final tag if no matches to
 * the string are found. */

int g95_string2code(mstring *m, char *string) {

    for(; m->string != NULL; m++)
	if (strcmp(m->string, string) == 0)
	    return m->tag;

    return m->tag;
}



/* g95_intent_string()-- Convert an intent code to a string. */

char *g95_intent_string(g95_intent intent) {

    return g95_code2string(intents, intent);
}



/* g95_flavor_string()-- Convert a flavor code to a string. */

char *g95_flavor_string(sym_flavor flavor) {

    return g95_code2string(flavors, flavor);
}



/* g95_procedure_string()-- Convert a procedure_type to a string */

char *g95_procedure_string(procedure_type type) {

    return g95_code2string(procedures, type);
}



/* g95_access_string()-- Convert a g95_access to a string. */

char *g95_access_string(g95_access type) {

    return g95_code2string(accessibility, type);
}



/* g95_strcmp()-- Like strcmp(), except treats null pointers like null
 * strings. */

int g95_strcmp(char *p, char *q) {

    if (p == NULL)
	p = "";

    if (q == NULL)
	q = "";

    return strcmp(p, q);
}



/* g95_uopchar()-- Returns true if the letter can appear in a
 * user-defined operator. */

int g95_uopchar(char c) {

    if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
	return 1;

    if (g95_option.fmode == 0 && isalpha(c))
	return 1;

    return 0;
}



/* g95_varchar()-- Test if a character is suitable as a variable
 * letter.  The test for initial variable letters is much stricter
 * than those that follow. */

int g95_varchar(char c, int first) {

    if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
	return 1;

    if (first)
	return 0;

    if (('0' <= c && c <= '9') || c == '_' || (c == '$' && g95_option.dollar))
	return 1;

    if (g95_option.fmode == 0 && isalpha(c))
	return 1;

    return 0;
}



/* g95_update_locus()-- If where1 is not blank, set it to where2. */

void g95_update_locus(g95_locus *where1, g95_locus *where2) {

    if (where1->nextc == NULL)
	*where1 = *where2;
}



/* release_options()-- Release resources */

static void release_options(void) {
g95_directorylist *p;

    g95_free(g95_option.module_dir);
 
    while(g95_option.include_dirs != NULL) {
	p = g95_option.include_dirs;
	g95_option.include_dirs = g95_option.include_dirs->next;
	g95_free(p->path);
	g95_free(p);
    }
}



/* g95_mangle_sym()-- Convert a name of a g95_symbol to a mangled name. */

char *g95_mangle_sym(g95_symbol *sym, int global, char *suffix) {
static char mangled_name[3*G95_MAX_SYMBOL_LEN+4];
char *p;
int n;

    if (sym->attr.bind)
	return sym->bind;

    if (sym->ns == NULL || !g95_module_symbol(sym) ||
	g95_strcmp(sym->module, "(global)") == 0)
	sprintf(mangled_name, "%s", sym->name);

    else {
	sprintf(mangled_name, "%s_MP_%s", sym->module, sym->name);

	if (suffix != NULL) {
	    strcat(mangled_name, ".");
	    strcat(mangled_name, suffix);
	}

	goto no_mangle;
    }

    if (sym->attr.flavor == FL_PROGRAM)
	goto no_mangle;

    if (g95_option.case_upper) {
	p = mangled_name;
	while(*p != '\0') {
	    if ('a' <= *p && *p <= 'z')
		*p += 'A' - 'a';

	    p++;
	}
    }

    if (g95_option.leading_underscore) {
	memmove(mangled_name+1, mangled_name, strlen(mangled_name)+1);
	mangled_name[0] = '_';
    }

    if (suffix != NULL) {
	strcat(mangled_name, ".");
	strcat(mangled_name, suffix);
    }

    if (!global || g95_option.no_underscoring)
	n = 0;

    else if (strchr(sym->name, '_') == NULL || g95_option.no_second_underscore)
	n = 1;

    else
	n = 2;

    while(n > 0) {
	strcat(mangled_name, "_");
	n--;
    }

no_mangle:
    return mangled_name;
}



/* show_ctype()-- Show a C type given a fortran type */

static void show_ctype(g95_typespec *ts) {

    switch(ts->type) {
    case BT_INTEGER:
    case BT_LOGICAL:
	switch(ts->kind) {
	case 1:  printf("char");       break;
	case 2:  printf("short");      break;
	case 4:  printf("int");        break;
	case 8:  printf("long long");  break;
	default:
	    g95_internal_error("show_ctype(): Bad integer/logical kind");
	}

	break;

    case BT_CHARACTER:
	printf("char *");
	break;

    case BT_REAL:
	switch(ts->kind) {
	case 4:   printf("float");   break;
	case 8:   printf("double");  break;
	case 10:  printf("real10");  break;
	case 16:  printf("real16");  break;
	default:
	    g95_internal_error("show_ctype(): Bad real kind");
	}

	break;

    case BT_COMPLEX:
	printf("complex%d", ts->kind);
	break;

    case BT_DERIVED:
	printf("struct %s", ts->derived->name);
	break;

    case BT_PROCEDURE:
	printf("proc");
	break;

    default:
	g95_internal_error("show_ctype(): Bad type");
    }
}



/* show_cbinding()-- Show C binding for a procedure. */

static void show_cbinding(g95_symbol *sym) {
int need_comma, flag, label;
g95_formal_arglist *f;
g95_symbol *result;
g95_typespec *ts;
array_type type;

    if (sym == NULL)
	return;

    flag = 0;
    ts = NULL;
    result = NULL;

    if (sym->attr.subroutine)
	flag = 1;

    else {
	result = sym->result;
	ts = &result->ts;

	if (ts->type == BT_CHARACTER || ts->type == BT_DERIVED ||
	    ts->type == BT_COMPLEX)
	    flag = 1;
    }

    if (result != NULL && result->as != NULL)
	printf("g95_array_descriptor * ");

    else if (flag)
	printf("void ");

    else {
	show_ctype(ts);
	putchar(' ');
    }

    fputs(g95_mangle_sym(sym, 1, NULL), stdout);
    putchar('(');
    need_comma = 0;

    if (result != NULL)
	switch(ts->type) {
	case BT_COMPLEX:
	    show_ctype(ts);
	    printf(" *result");
	    need_comma = 1;
	    break;

	case BT_DERIVED:
	    show_ctype(ts);
	    printf(" *result");
	    need_comma = 1;
	    break;

	case BT_CHARACTER:
	    printf("char *RESULT, int RESULT_len");
	    need_comma = 1;
	    break;

	default:
	    break;
	}

    label = 1;

    for(f=sym->formal; f; f=f->next) {
	if (need_comma) 
	    printf(", ");

	need_comma = 1;

	if (f->sym == NULL) {
	    printf("int label%d", label++);
	    continue;
	}

	type = AS_UNKNOWN;
	if (f->sym->as != NULL)
	    type = f->sym->as->type;

	if (type == AS_ASSUMED_SHAPE || type == AS_DEFERRED)
	    printf("g95_array_descriptor *%s", f->sym->name);

	else if (type == AS_EXPLICIT || type == AS_ASSUMED_SIZE) {
	    show_ctype(&f->sym->ts);
	    printf(" %s[]", f->sym->name);

	} else if (f->sym->ts.type == BT_CHARACTER) {
	    show_ctype(&f->sym->ts);
	    fputs(f->sym->name, stdout);

	} else {
	    show_ctype(&f->sym->ts);

	    putchar(' ');
	    if (!f->sym->attr.value)
		putchar('*');

	    fputs(f->sym->name, stdout);
	}
    }

    for(f=sym->formal; f; f=f->next)
	if (f->sym != NULL && f->sym->ts.type == BT_CHARACTER)
	    printf(", int %s_len", f->sym->name);

    puts(");");
}



/* g95_show_cbinding()-- Show C prototypes for a namespace. */

void g95_show_cbinding(g95_namespace *ns) {
g95_namespace *p;

    switch(ns->state) {
    case COMP_MODULE:
	for(p=ns->contained; p; p=p->sibling)
	    show_cbinding(p->proc_name);
	break;

    case COMP_FUNCTION:
    case COMP_SUBROUTINE:
	show_cbinding(ns->proc_name);
	break;

    default:
	break;
    }
}



/* compare_cbinding()-- Compare two cbindings. */

static int compare_cbinding(g95_cbinding *a, g95_cbinding *b) {

    return strcmp(a->name, b->name);
}



/* cbinding_name()-- Return the name of a cbinding name. */

static char *cbinding_name(int type) {

    switch(type) {
    case CBIND_VARIABLE:   return "VARIABLE";
    case CBIND_PROCEDURE:  return "PROCEDURE";
    case CBIND_COMMON:     return "COMMMON";
    default:               break;
    }

    g95_internal_error("cbinding_name(): Bad type");
    return NULL;
}



/* g95_add_cbinding()-- Given a name, record it's type for c-binding
 * purposes.  Generates an error if something is inconsistent.
 * Returns nonzero if an error was generated. */

int g95_add_cbinding(char *name, int type, g95_locus *where) {
g95_cbinding *p;
int c;

    p = cbinding_root;

    for(;;) {
	if (p == NULL)
	    break;

	c = strcmp(name, p->name);
	if (c == 0)
	    break;

	p = (c < 0) ? p->left : p->right;
    }

    if (p == NULL) {
	p = g95_getmem(sizeof(g95_cbinding));
	p->name = g95_get_string(name);

	p->type = type;
	p->where = *where;

	g95_insert_bbt(&cbinding_root, p, compare_cbinding);

    } else if (p->type == CBIND_UNKNOWN) {
	p->type = type;
	p->where = *where;

    } else if (type != CBIND_UNKNOWN && p->type != type) {
	g95_error("C-binding name '%s' is %s at %L while it is %s at %L",
		  name, cbinding_name(type), where, cbinding_name(p->type), 
		  &p->where);

	return 1;
    }

    return 0;
}



/* free_cbindings()-- Free the cbindings tree */

static void free_cbindings(g95_cbinding *p) {

    if (p == NULL)
	return;

    free_cbindings(p->left);
    free_cbindings(p->right);

    g95_free(p);
}



/* g95_init_1()-- Top level initialization */

void g95_init_1(void) {

    big_initialize();
    bg_init();
    g95_error_init_1();
    g95_scanner_init_1();
    g95_arith_init_1();
    g95_intrinsic_init_1();
    g95_iresolve_init_1();
    g95_simplify_init_1();
    g95_io_init();
    free_cbindings(cbinding_root);
}



/* g95_init_2()-- Per program unit initialization */

void g95_init_2(void) {

    g95_symbol_init_2();
    g95_module_init_2();
}



/* g95_done_1()-- Call all of the top level destructors */

void g95_done_1(void) {

    misc_done();
    g95_scanner_done_1();
    g95_intrinsic_done_1();
    g95_simplify_done_1();
    g95_iresolve_done_1();
    g95_arith_done_1();
    g95_expr_done_1();
    g95_options_done();
    g95_free_gsymbol(g95_gsym_root);
    release_options();
    bg_done();
    big_terminate();
}



/* g95_done_2()-- Per program unit destructors */

void g95_done_2(void) {

    g95_module_done_2();
}



int g95_front_ts_size(g95_typespec *ts) {
g95_component *c;
g95_expr *e;
int size, n;

    switch(ts->type) {
    case BT_INTEGER:
    case BT_LOGICAL:
    case BT_REAL:
	n = ts->kind;
	break;

    case BT_COMPLEX:
	n = 2*ts->kind;
	break;

    case BT_CHARACTER:
	e = ts->cl->length;
	if (e->type != EXPR_CONSTANT)
	    g95_internal_error("g95_int_ts_size(): Unknown character length");

	n = bi_to_int(e->value.integer);
	break;

    case BT_DERIVED:
	n = 0;
	for(c=ts->derived->components; c; c=c->next) {
	    size = g95_front_ts_size(&c->ts);

	    if (c->as == NULL)
		n += size;

	    else
		n += 2*sizeof(void *) + 3*sizeof(int) +
		    c->as->rank * 3 * sizeof(void *);
	}

	break;

    default:
	g95_internal_error("g95_int_ts_size(): Bad type");
	n = 0;
    }

    return n;
}



#ifndef IN_GCC

/* main()-- Compile a fortran program */

int main(int argc, char *argv[]) {
int errors, warnings, i;

#ifdef __GLIBC__
    mtrace();
#endif

    g95_init_options(0, NULL);

    argv++;

    while(argc > 1) {
	i = g95_parse_arg(argc, argv);
	if (i < 0) i = -i;

	argc -= i;
	argv += i;
    }

    g95_init_1();

    if (g95_source_file == NULL)
	g95_fatal_error("Need a file to compile");

    if (g95_new_file() != SUCCESS)
	return 3;

    g95_parse_file();

    g95_done_1();
    release_options();

    g95_get_errors(&warnings, &errors);

    if (!g95_option.quiet)
	g95_status("Warnings: %d  Errors: %d\n", warnings, errors);

    if (errors > 0) return 2;
    if (warnings > 0) return 1;
    return 0;
}

#endif
