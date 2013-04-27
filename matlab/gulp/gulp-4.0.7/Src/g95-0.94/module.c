/* Module module
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

/* module.c-- Handle modules, which amounts to loading and saving
 * symbols and their attendant structures.  */

/* The syntax of g95 modules resembles that of lisp lists, ie a
 * sequence of atoms, which can be left or right parenthesis, names,
 * integers or strings.  Parenthesis are always matched which allows
 * us to skip over sections at high speed without having to know
 * anything about the internal structure of the lists.  A "name" is
 * usually a fortran 95 identifier, but can also start with '@' in
 * order to reference a hidden symbol.
 *
 * The first line of a module is an informational message about what
 * created the module, the file it came from and when it was created.
 * The second line is a warning for people not to edit the module.
 * The rest of the module looks like:
 *
 * <integer compatibility version>
 *
 * ( ( <Interface info for UPLUS> )
 *   ( <Interface info for UMINUS> )
 *   ...
 * )
 * ( ( <name of operator interface> <module of op interface> <i/f1> ... )
 *   ...
 * )
 * ( ( <name of generic interface> <i/f1> ... )
 *   ...
 * )
 * ( ( <common name> <symbol> <saved flag> )
 *   ...
 * )
 * ( equivalence list )
 * ( <Symbol Number (in no particular order)>
 *   <True name of symbol>
 *   <Module name of symbol>
 *   ( <symbol information> )
 *   ...
 * )
 * ( <Symtree name>
 *   <Ambiguous flag>
 *   <Symbol number>
 *   ...
 * )
 *
 * In general, symbols refer to other symbols by their symbol number,
 * which are zero based.  Symbols are written to the module in no
 * particular order. */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>

#include "g95.h"

#define MODULE_EXTENSION ".mod"

/* The COMPAT_VERSION macro needs to be changed whenever there is an
 * incompatible change made to the module format.  This prevents weird
 * internal errors associated with reading an incompatible format. */

#define COMPAT_VERSION 9


/* The ifdefs for __GLIBC__ are for debugging on Andy's system and
 * need to be removed someday (perhaps when he upgrades glibc) */

#ifdef __GLIBC__
#include <mcheck.h>
#endif

/* Structure that describes a position within a module file */

typedef struct {
    int column, line;
    fpos_t pos;
} module_locus;


enum { SECT_INTRINSIC_OP=0, SECT_USER_OP, SECT_GENERIC, SECT_COMMON,
       SECT_EQUIV, SECT_SYMBOL, SECT_SYMTREE, SECTIONS };

typedef enum {
    P_UNKNOWN=0, P_OTHER, P_NAMESPACE, P_COMPONENT, P_SYMBOL } pointer_t;

/* The fixup structure lists pointers to pointers that have to be
 * updated when a pointer value becomes known. */

typedef struct fixup_t {
    char **pointer;
    struct fixup_t *next;
} fixup_t;


/* Structure for holding extra info needed for pointers being read */

typedef struct pointer_info {
    BBT_HEADER(pointer_info)

    int integer;
    pointer_t type;

    /* The first component of each member of the union is the pointer
     * being stored */

    fixup_t *fixup;

    union {
	char *pointer;         /* Member for doing pointer searches */

	struct {
	    g95_symbol *sym;
	    char true_name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];
	    enum { UNUSED, NEEDED, USED } state;
	    int ns, referenced;
	    module_locus where;
	} rsym;

	struct {
	    g95_symbol *sym;
	    enum { UNREFERENCED=0, NEEDS_WRITE, WRITTEN } state;
	    struct pointer_info *link;
	} wsym;
    } u;

} pointer_info;

#define g95_get_pointer_info() g95_getmem(sizeof(pointer_info))


/* Lists of rename info for the USE statement */

typedef struct g95_use_rename {
    char local_name[G95_MAX_SYMBOL_LEN+1], use_name[G95_MAX_SYMBOL_LEN+1];
    struct g95_use_rename *next;
    int found, mark, operator;
    g95_locus where;
} g95_use_rename;

#define g95_get_use_rename() g95_getmem(sizeof(g95_use_rename))

/* Local variables */

static FILE *module_fp, *old_module_fp;

static g95_use_rename *rename_list;
static pointer_info *pi_root, *secondary;
static int symbol_number;         /* Counter for assigning symbol numbers */

static char module_name[G95_MAX_SYMBOL_LEN+1];
static int module_line, module_column, only_flag;
static enum { IO_INPUT, IO_OUTPUT } iomode;

static char imod[] = "(intrinsic-module)";

static g95_locus only_name;


/* This tree is used to avoid a brute-force search for a combination
 * of true name and module name.  While symtree names, the name that a
 * particular symbol is known but can changed with USE statements, we
 * still have to keep track of the true names to generate the correct
 * reference, and also avoid loading the same real symbol twice in a
 * program unit.
 *
 * When we start reading, the true name tree is built and maintained
 * as symbols are read.  The tree is searched as we load new symbols
 * to see if it already exists someplace in the namespace. */

typedef struct true_name {
    BBT_HEADER(true_name) 

    g95_symbol *sym;
} true_name;

static true_name *true_name_root;

static enum {
    MOD_UNKNOWN, MOD_INTRINSIC, MOD_NONINTRINSIC
} mod_kind;


/* Tokens for module reading and writing */

typedef enum {
    ATOM_NAME, ATOM_LPAREN, ATOM_RPAREN, ATOM_INTEGER, ATOM_STRING
} atom_type;

static atom_type last_atom;


/* The name buffer must be at least as long as a symbol name.  real
 * constants are stored as bigints, since this allows the exact number
 * to be preserved. */

#define MAX_ATOM_SIZE 100

static int atom_int;
static char *atom_string, atom_name[MAX_ATOM_SIZE];

static void mio_expr(g95_expr **);
static void mio_symbol_ref(g95_symbol **);

enum { AB_ALLOCATABLE, AB_DIMENSION, AB_EXTERNAL, AB_INTRINSIC, AB_OPTIONAL,
       AB_IN_COMMON, AB_FUNCTION, AB_SUBROUTINE, AB_SEQUENCE, AB_ELEMENTAL,
       AB_POINTER, AB_TARGET, AB_DUMMY, AB_DATA, AB_IN_NAMELIST,
       AB_PURE, AB_RECURSIVE, AB_BIND, AB_VALUE, AB_PROTECTED, AB_VOLATILE,
       AB_INVOKED, AB_ABSTRACT
};


static mstring attr_bits[] = {
    minit("ALLOCATABLE", AB_ALLOCATABLE), minit("DIMENSION",   AB_DIMENSION),
    minit("EXTERNAL",    AB_EXTERNAL),    minit("INTRINSIC",   AB_INTRINSIC),
    minit("OPTIONAL",    AB_OPTIONAL),    minit("POINTER",     AB_POINTER),
    minit("TARGET",      AB_TARGET),      minit("DUMMY",       AB_DUMMY), 
    minit("DATA",        AB_DATA),        minit("IN_NAMELIST", AB_IN_NAMELIST),
    minit("IN_COMMON",   AB_IN_COMMON),   minit("FUNCTION",    AB_FUNCTION),
    minit("SUBROUTINE",  AB_SUBROUTINE),  minit("SEQUENCE",    AB_SEQUENCE),
    minit("ELEMENTAL",   AB_ELEMENTAL),   minit("PURE",        AB_PURE),
    minit("RECURSIVE",   AB_RECURSIVE),   minit("BIND",        AB_BIND),
    minit("VALUE",       AB_VALUE),       minit("PROTECTED",   AB_PROTECTED),
    minit("VOLATILE",    AB_VOLATILE),    minit("INVOKED",     AB_INVOKED),
    minit("ABSTRACT",    AB_ABSTRACT),    minit(NULL, -1)
};

static mstring procedures[] = {
    minit("UNKNOWN",   PROC_UNKNOWN),   minit("MODULE-PROC", PROC_MODULE),
    minit("INTERNAL",  PROC_INTERNAL),  minit("DUMMY", PROC_DUMMY),
    minit("INTRINSIC", PROC_INTRINSIC), minit("ST-FUNCTION", PROC_ST_FUNCTION),
    minit("EXTERNAL",  PROC_EXTERNAL),  minit(NULL, -1)
};

static mstring array_ref_types[] = {
    minit("FULL", AR_FULL),         minit("ELEMENT", AR_ELEMENT),
    minit("SECTION", AR_SECTION),   minit(NULL, -1)
};

static mstring ifsrc_types[] = {
    minit("UNKNOWN",  IFSRC_UNKNOWN),
    minit("DECL",     IFSRC_DECL),
    minit("BODY",     IFSRC_IFBODY),
    minit("USAGE",    IFSRC_USAGE)
};


static mstring ref_types[] = {
    minit("ARRAY", REF_ARRAY),            minit("COMPONENT", REF_COMPONENT),
    minit("SUBSTRING", REF_SUBSTRING),    minit(NULL, -1)
};

static mstring array_spec_types[] = {
    minit("EXPLICIT", AS_EXPLICIT),   minit("ASSUMED_SHAPE", AS_ASSUMED_SHAPE),
    minit("DEFERRED", AS_DEFERRED),   minit("ASSUMED_SIZE",  AS_ASSUMED_SIZE),
    minit(NULL, -1)
};

static mstring callby_types[] = {
    minit("NONE", CB_NONE), minit("VAL", CB_VALUE), minit("REF", CB_REFERENCE),
    minit(NULL, -1)
};

static mstring access_types[] = {
    minit("UNKNOWN",   ACCESS_UNKNOWN),
    minit("PRIVATE",   ACCESS_PRIVATE),
    minit("PUBLIC",    ACCESS_PUBLIC),
    minit(NULL, -1)
};

static mstring expr_types[] = {
    minit("OP",         EXPR_OP),         minit("FUNCTION",   EXPR_FUNCTION),
    minit("CONSTANT",   EXPR_CONSTANT),   minit("VARIABLE",   EXPR_VARIABLE),
    minit("SUBSTRING",  EXPR_SUBSTRING),  minit("STRUCTURE",  EXPR_STRUCTURE),
    minit("ARRAY",      EXPR_ARRAY),      minit("NULL",       EXPR_NULL),
    minit("PROC",       EXPR_PROCEDURE),  minit(NULL, -1)
};

static mstring intents[] = {
    minit("UNKNOWN", INTENT_UNKNOWN),  minit("IN", INTENT_IN),
    minit("OUT", INTENT_OUT),          minit("INOUT", INTENT_INOUT),
    minit(NULL, -1)
};

static mstring flavors[] = {
    minit("UNKNOWN",     FL_UNKNOWN),      minit("PROGRAM",     FL_PROGRAM),
    minit("BLOCK-DATA",  FL_BLOCK_DATA),   minit("MODULE",      FL_MODULE),
    minit("VARIABLE",    FL_VARIABLE),     minit("PARAMETER",   FL_PARAMETER),
    minit("LABEL",       FL_LABEL),        minit("PROCEDURE",   FL_PROCEDURE),
    minit("DERIVED",     FL_DERIVED),      minit("NAMELIST",    FL_NAMELIST),
    minit(NULL, -1)
};

static mstring itypes[] = {
    minit("NONE",     ITYPE_NONE),       minit("C_PTR",   ITYPE_C_PTR),
    minit("C_FUNPTR", ITYPE_C_FUNPTR),   minit("CLASS",   ITYPE_IEEE_CLASS),
    minit("FLAG",     ITYPE_IEEE_FLAG),  minit("STATUS",  ITYPE_IEEE_STATUS),
    minit("ROUND",    ITYPE_IEEE_ROUND), minit("FEADURE", ITYPE_IEEE_FEATURES),
    minit(NULL, -1)
};

static mstring iprocs[] = {
    minit("NONE",         IPROC_NONE),
    minit("C_LOC",        IPROC_C_LOC),
    minit("C_FUNLOC",     IPROC_C_FUNLOC),
    minit("C_ASSOCIATED", IPROC_C_ASSOCIATED),
    minit("C_F_POINTER",  IPROC_C_F_POINTER),
    minit("C_F_PROCPOINTER",       IPROC_C_F_PROCPOINTER),
    minit("IEEE_SUPPORT_FLAG",     IPROC_IEEE_SUPPORT_FLAG),
    minit("IEEE_SUPPORT_HALTING",  IPROC_IEEE_SUPPORT_HALTING),
    minit("IEEE_GET_FLAG",         IPROC_IEEE_GET_FLAG),
    minit("IEEE_GET_HALTING_MODE", IPROC_IEEE_GET_HALTING_MODE),
    minit("IEEE_SET_FLAG",         IPROC_IEEE_SET_FLAG),
    minit("IEEE_SET_HALTING_MODE", IPROC_IEEE_SET_HALTING_MODE),
    minit("IEEE_GET_STATUS",       IPROC_IEEE_GET_STATUS),
    minit("IEEE_SET_STATUS",       IPROC_IEEE_SET_STATUS),
    minit("IEEE_SUPPORT_DATATYPE", IPROC_IEEE_SUPPORT_DATATYPE),
    minit("IEEE_SUPPORT_DENORMAL", IPROC_IEEE_SUPPORT_DENORMAL),
    minit("IEEE_SUPPORT_DIVIDE",   IPROC_IEEE_SUPPORT_DIVIDE),
    minit("IEEE_SUPPORT_INF",      IPROC_IEEE_SUPPORT_INF),
    minit("IEEE_SUPPORT_NAN",      IPROC_IEEE_SUPPORT_NAN),
    minit("IEEE_SUPPORT_ROUNDING", IPROC_IEEE_SUPPORT_ROUNDING),
    minit("IEEE_SUPPORT_SQRT",     IPROC_IEEE_SUPPORT_SQRT),
    minit("IEEE_SUPPORT_STANDARD", IPROC_IEEE_SUPPORT_STANDARD),
    minit("IEEE_SUPPORT_UNDERFLOW_CONTROL",
	  IPROC_IEEE_SUPPORT_UNDERFLOW_CONTROL),
    minit("IEEE_CLASS", IPROC_IEEE_CLASS),
    minit("IEEE_COPY_SIGN",   IPROC_IEEE_COPY_SIGN),
    minit("IEEE_IS_FINITE",   IPROC_IEEE_IS_FINITE),
    minit("IEEE_IS_NAN",      IPROC_IEEE_IS_NAN),
    minit("IEEE_IS_NEGATIVE", IPROC_IEEE_IS_NEGATIVE),
    minit("IEEE_IS_NORMAL",   IPROC_IEEE_IS_NORMAL),
    minit("IEEE_LOGB",        IPROC_IEEE_LOGB),
    minit("IEEE_NEXT_AFTER",  IPROC_IEEE_NEXT_AFTER),
    minit("IEEE_REM",         IPROC_IEEE_REM),
    minit("IEEE_RINT",        IPROC_IEEE_RINT),
    minit("IEEE_SCALB",       IPROC_IEEE_SCALB),
    minit("IEEE_UNORDERED",   IPROC_IEEE_UNORDERED),
    minit("IEEE_VALUE",       IPROC_IEEE_VALUE),
    minit("IEEE_GET_ROUNDING_MODE",  IPROC_IEEE_GET_ROUNDING_MODE),
    minit("IEEE_GET_UNDERFLOW_MODE", IPROC_IEEE_GET_UNDERFLOW_MODE),
    minit("IEEE_SET_ROUNDING_MODE",  IPROC_IEEE_SET_ROUNDING_MODE),
    minit("IEEE_SET_UNDERFLOW_MODE", IPROC_IEEE_SET_UNDERFLOW_MODE),
    minit("IEEE_SELECTED_REAL_KIND", IPROC_IEEE_SELECTED_REAL_KIND),
    minit(NULL, -1)
};

static mstring bt_types[] = {
    minit("INTEGER",    BT_INTEGER),      minit("REAL",       BT_REAL),
    minit("COMPLEX",    BT_COMPLEX),      minit("LOGICAL",    BT_LOGICAL),
    minit("CHARACTER",  BT_CHARACTER),    minit("DERIVED",    BT_DERIVED),
    minit("PROCEDURE",  BT_PROCEDURE),    minit("UNKNOWN",    BT_UNKNOWN),
  minit(NULL, -1)
};

/* INTRINSIC_ASSIGN is missing because it is used as an index for
 * generic operators, not in expressions.  INTRINSIC_USER is also
 * replaced by the correct function name by the time we see it. */

static mstring intrinsics[] = {
    minit("UPLUS",  INTRINSIC_UPLUS),  minit("UMINUS",  INTRINSIC_UMINUS),
    minit("PLUS",   INTRINSIC_PLUS),   minit("MINUS",   INTRINSIC_MINUS),
    minit("TIMES",  INTRINSIC_TIMES),  minit("DIVIDE",  INTRINSIC_DIVIDE),
    minit("POWER",  INTRINSIC_POWER),  minit("CONCAT",  INTRINSIC_CONCAT),
    minit("AND",    INTRINSIC_AND),    minit("OR",      INTRINSIC_OR),
    minit("EQV",    INTRINSIC_EQV),    minit("NEQV",    INTRINSIC_NEQV),
    minit("EQ",     INTRINSIC_EQ),     minit("NE",      INTRINSIC_NE),
    minit("GT",     INTRINSIC_GT),     minit("GE",      INTRINSIC_GE),
    minit("LT",     INTRINSIC_LT),     minit("LE",      INTRINSIC_LE),
    minit("NOT",    INTRINSIC_NOT),    minit("PA",      INTRINSIC_PAREN),
    minit(NULL, -1)
};



/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen.  This subroutine never returns.  */

static void bad_module(char *message) {
char *p;

    switch(iomode) {
    case IO_OUTPUT:  p = "Writing";  break;
    case IO_INPUT:   p = "Reading";  break;
    default:         p = "???";      break;
    }

    fclose(module_fp);

    g95_fatal_error("%s module %s at line %d column %d: %s", p,
		    module_name, module_line, module_column, message);
}



/* compare_pointers()-- Compare pointers when searching by pointer.
 * Used when writing a module. */

static int compare_pointers(pointer_info *sn1, pointer_info *sn2) {

    if (sn1->u.pointer < sn2->u.pointer) return -1;
    if (sn1->u.pointer > sn2->u.pointer) return 1;

    return 0;
}



/* compare_integers()-- Compare integers when searching by integer.
 * Used when reading a module. */

static int compare_integers(pointer_info *sn1, pointer_info *sn2) {

    if (sn1->integer < sn2->integer) return -1;
    if (sn1->integer > sn2->integer) return 1;

    return 0;
}



/* free_pi_tree()-- Recursively free the tree of pointer structures */

static void free_pi_tree(pointer_info *p) {

    if (p == NULL)
	return;

    if (p->fixup != NULL)
	g95_internal_error("free_pi_tree(): Unresolved fixup");

    free_pi_tree(p->left);
    free_pi_tree(p->right);

    g95_free(p);
}



/* init_pi_tree()-- Initialize the pointer_info tree. */

static void init_pi_tree(void) {
int (*compare)(pointer_info *, pointer_info *);
pointer_info *p;

    pi_root = NULL;
    compare = (iomode == IO_INPUT) ? compare_integers : compare_pointers;

    /* Pointer 0 is the NULL pointer */

    p = g95_get_pointer_info();
    p->u.pointer = NULL;
    p->integer = 0;
    p->type = P_OTHER;

    g95_insert_bbt(&pi_root, p, compare);

    /* Pointer 1 is the current namespace */

    p = g95_get_pointer_info();
    p->u.pointer = (char *) g95_current_ns;
    p->integer = 1;
    p->type = P_NAMESPACE;

    g95_insert_bbt(&pi_root, p, compare);

    symbol_number = 2;
}



/* find_integer()-- Given an integer, find it in the tree.  Returns
 * NULL if not found. */

static pointer_info *find_integer(int integer) {
pointer_info *p, t;
int c;

    t.integer = integer;

    p = pi_root;
    while(p != NULL) {
	c = compare_integers(&t, p);
	if (c == 0)
	    break;

	p = (c < 0) ? p->left : p->right;
    }

    return p;
}



/* get_integer()-- Given an integer during reading, find it in the
 * pointer_info tree, creating the node if not found. */

static pointer_info *get_integer(int integer) {
pointer_info *p;

    p = find_integer(integer); 

    if (p == NULL) {
	p = g95_get_pointer_info();
	p->integer = integer;
	p->u.pointer = NULL;

	g95_insert_bbt(&pi_root, p, compare_integers);
    }

    return p;
}



/* find_pointer()-- During module writing, call here with a pointer
 * to something, returning the pointer_info node. */

static pointer_info *find_pointer(void *gp) {
pointer_info *p;
char *cp;

    cp = (char *) gp;
  
    p = pi_root;
    while(p != NULL) {
	if (p->u.pointer == cp)
	    break;

	p = (cp < p->u.pointer)
	    ? p->left
	    : p->right;
    }

    return p;
}



/* get_pointer()-- Given a pointer while writing, returns the
 * pointer_info tree node, creating it if it doesn't exist. */

static pointer_info *get_pointer(void *gp) {
pointer_info *p;

    p = find_pointer(gp); 
    if (p != NULL)
	return p;

    /* Pointer doesn't have an integer.  Give it one. */

    p = g95_get_pointer_info();

    p->u.pointer = gp;
    p->integer = symbol_number++;

    g95_insert_bbt(&pi_root, p, compare_pointers);

    return p;
}



/* associate_integer_pointer()-- Call here during module reading when
 * we know what pointer to associate with an integer.  Any fixups that
 * exist are resolved at this time. */

static void associate_integer_pointer(pointer_info *p, void *gp) {
fixup_t *f, *g;

    if (p->u.pointer != NULL)
	g95_internal_error("associate_integer_pointer(): Already associated");

    p->u.pointer = gp;

    for(f=p->fixup; f; f=g) {
	g = f->next;

	*(f->pointer) = gp;
	g95_free(f);
    }

    p->fixup = NULL;
}



/* add_fixup()-- During module reading, given an integer and a pointer
 * to a pointer, either store the pointer from an already-known value
 * or create a fixup structure in order to store things later.
 * Returns zero if the reference has been actually stored, or nonzero
 * if the reference must be fixed later (ie associate_integer_pointer
 * must be called sometime later.  Returns the pointer_info structure. */

static pointer_info *add_fixup(int integer, void *gp) {
pointer_info *p;
fixup_t *f;
char **cp;

    p = get_integer(integer);

    if (p->integer == 0 || p->u.pointer != NULL) {
	cp = gp;
	*cp = p->u.pointer;

    } else {
	f = g95_getmem(sizeof(fixup_t));

	f->next = p->fixup;
	p->fixup = f;

	f->pointer = gp;
    }

    return p;
}



/* find_use_operator()-- Try to find the operator in the current list */

static g95_use_rename *find_use_operator(int operator) {
g95_use_rename *u;

    for(u=rename_list; u; u=u->next)
	if (u->operator == operator)
	    return u;

    return NULL;
}



/* get_unique_symtree()-- Return a symtree node with a name that is
 * guaranteed to be unique within the namespace and corresponds to an
 * illegal fortran name */

static g95_symtree *get_unique_symtree(g95_namespace *ns) {
char name[G95_MAX_SYMBOL_LEN+1]; 
static int serial=0;

    sprintf(name, "@%d", serial++); 
    return g95_new_symtree(&ns->sym_root, name);
}



/* check_unique_name()-- See if a name is a generated name. */

static int check_unique_name(char *name) {

    return *name == '@';
}



/* compare_true_names()-- Compare two true_name structures. */

static int compare_true_names(true_name *t1, true_name *t2) {
int c;

    c = g95_strcmp(t1->sym->module, t2->sym->module);
    if (c != 0)
	return c;

    return g95_strcmp(t1->sym->name, t2->sym->name);
}



/* find_true_name()-- Given a true name, search the true name tree to
 * see if it exists within the main namespace. */

static g95_symbol *find_true_name(char *name, char *module) {
true_name t, *p;
g95_symbol sym;
int c;

    if (module == NULL || module[0] == '\0')
	return NULL; /* Don't match a hidden symbol */

    sym.name = g95_get_string(name);
    sym.module = g95_get_string(module);

    t.sym = &sym;

    p = true_name_root;
    while(p != NULL) {
	c = compare_true_names(&t, p);
	if (c == 0)
	    return p->sym;

	p = (c < 0) ? p->left : p->right;
    }

    return NULL;
}



/* add_true_name()-- Given a g95_symbol pointer that is not in the
 * true name tree, add it. */

static void add_true_name(g95_symbol *sym) {
true_name *t;

    t = g95_getmem(sizeof(true_name));
    t->sym = sym;

    g95_insert_bbt(&true_name_root, t, compare_true_names);
}



/* build_tnt()-- Recursive function to build the initial true name
 * tree by recursively traversing the current namespace. */

static void build_tnt(g95_symtree *st) {

    if (st == NULL)
	return;

    build_tnt(st->left);
    build_tnt(st->right);

    if (find_true_name(st->n.sym->name, st->n.sym->module) != NULL)
	return;

    add_true_name(st->n.sym);
}



/* free_true_name()-- Recursively free a true name tree node. */

static void free_true_name(true_name *t) {

    if (t == NULL)
	return;

    free_true_name(t->left);
    free_true_name(t->right);

    g95_free(t);
}



/* init_true_name_tree()-- Initialize the true name tree with the
 * current namespace. */

static void init_true_name_tree(void) {

    true_name_root = NULL;
    build_tnt(g95_current_ns->sym_root);
}



/* free_rename()-- Free the rename list left behind by a USE
 * statement. */

static void free_rename(void) {
g95_use_rename *next;

    for(;rename_list; rename_list=next) {
	next = rename_list->next;
	g95_free(rename_list);
    }
}



/* find_use_name()-- Given a name, return the name under which to load
 * this symbol.  Returns NULL if there are no more entries with this
 * name. */

static char *find_use_name(char *name) {
static int first=1;
g95_use_rename *u;
int seen;
char *p;

    seen = 0;
    only_name.nextc = NULL;

    for(u=rename_list; u; u=u->next) {
	if (strcmp(u->use_name, name) != 0)
	    continue;

	seen = 1;

	if (u->mark)
	    continue;

	u->mark = 1;
	break;
    }

    if (u != NULL) {
	u->found = 1;
	p = (u->local_name[0] != '\0')
	    ? u->local_name
	    : name;

	only_name = u->where;

    } else if (seen || only_flag)
	p = NULL;

    else
	p = first ? name : NULL;

    if (p != NULL)
	first = 0;

    else {
	first = 1;
	for(u=rename_list; u; u=u->next)
	    u->mark = 0;
    }

    return p;
}



/* get_module_locus()-- Get the module's input pointer so that we can
 * restore it later. */

static void get_module_locus(module_locus *m) {

    m->column = module_column;
    m->line   = module_line;
    fgetpos(module_fp, &m->pos);
}



/* set_module_locus()-- Set the module's input pointer */

static void set_module_locus(module_locus *m) {

    module_column = m->column;
    module_line   = m->line;
    fsetpos(module_fp, &m->pos);
}



/* module_char()-- Get the next character in the module, updating our
 * reckoning of where we are. */

static int module_char(void) {
int c;

    c = fgetc(module_fp);

    if (c == EOF)
	bad_module("Unexpected EOF");

    if (c == '\n') {
	module_line++;
	module_column = 0;
    }

    module_column++;
    return c;
}



/* parse_integer()-- Parse a small integer. */

static void parse_integer(int c) {
module_locus m;

    atom_int = c - '0';

    for(;;) {
	get_module_locus(&m);

	c = module_char();
	if (!isdigit(c))
	    break;

	atom_int = 10*atom_int + c - '0';
	if (atom_int > 99999999) bad_module("Integer overflow");
    }

    set_module_locus(&m);
}



/* parse_name()-- Parse a name.  */

static void parse_name(int c) {
module_locus m;
char *p;
int len;

    p = atom_name;

    *p++ = c;
    len = 1;

    get_module_locus(&m);

    for(;;) {
	c = module_char();
	if (!isalnum(c) && c != '_' && c != '-')
	    break;

	*p++ = c;
	if (++len > G95_MAX_SYMBOL_LEN) bad_module("Name too long");
    }

    *p = '\0';

    fseek(module_fp, -1, SEEK_CUR);
    module_column = m.column + len - 1;

    if (c == '\n')
	module_line--;
}



/* parse_string()-- Parse a string constant.  The delimiter is
 * guaranteed to be a single quote. */

static void parse_string(void) {
module_locus start;
int len, c;
char *p;

    get_module_locus(&start);

    len = 0;

/* See how long the string is */

loop:
    c = module_char();
    if (c == EOF)
	bad_module("Unexpected end of module in string constant");

    if (c != '\'') {
	len++;
	goto loop;
    }

    c = module_char();
    if (c == '\'') {
	len++;
	goto loop;
    }

    set_module_locus(&start);

    atom_string = p = g95_getmem(len+1);

    for(;len>0; len--) {
	c = module_char();
	if (c == '\'') module_char();  /* Guaranteed to be another \' */
	*p++ = c;
    }

    module_char();        /* Terminating \' */
    *p = '\0';            /* C-style string for debug purposes */
}



/* parse_atom()-- Read the next atom in the module's input stream. */

static atom_type parse_atom(void) {
int c;

    do
	c = module_char();
    while (c == ' ' || c == '\n' || c == '\r');

    switch(c) {
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
    case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
    case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
    case 'X': case 'Y': case 'Z':
	parse_name(c);
	return ATOM_NAME;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
	parse_integer(c);
	return ATOM_INTEGER;

    case ')':
	return ATOM_RPAREN;

    case '(':
	return ATOM_LPAREN;

    case '\'':
	parse_string();
	return ATOM_STRING;

    default:
	bad_module("Bad name");
    }

    return 0;   /* Not reached */
}



/* require_atom()-- Read the next atom from the input, requiring that
 * it be a particular kind */

static void require_atom(atom_type type) {
module_locus m;
atom_type t;
char *p;

    get_module_locus(&m); 

    t = parse_atom();
    if (t != type) {
	switch(type) {
	case ATOM_INTEGER:  p = "Expected integer";            break;
	case ATOM_STRING:   p = "Expected string";             break;
	case ATOM_NAME:     p = "Expected name";               break;
	case ATOM_LPAREN:   p = "Expected left parenthesis";   break;
	case ATOM_RPAREN:   p = "Expected right parenthesis";  break;
	default: 
	    g95_internal_error("require_atom(): bad atom type required");
	}

	set_module_locus(&m);
	bad_module(p);
    }
}



/* find_enum()-- Given a pointer to an mstring array, require that
 * the current input be one of the strings in the array.  We return
 * the enum value. */

static int find_enum(mstring *m) {
int i;

    i = g95_string2code(m, atom_name);
    if (i >= 0)
	return i;

    bad_module("find_enum(): Enum not found");
    return 0;  /* Not reached */
}



/* peek_atom()-- Peek at the next atom on the input */

static atom_type peek_atom(void) {
module_locus m;
atom_type a;

    get_module_locus(&m); 

    a = parse_atom();
    if (a == ATOM_STRING)
	g95_free(atom_string);

    set_module_locus(&m);
    return a;
}



/* write_char()-- Output a character to a module file */

static void write_char(char out) {

    if (fputc(out, module_fp) == EOF)
	g95_fatal_error("Error writing modules file: %s", strerror(errno));

    if (out != '\n')
	module_column++;

    else {
	module_column = 1;
	module_line++;
    }
}



/* write_atom()-- Write an atom to a module.  The line wrapping isn't
 * perfect, but it should work most of the time.  This isn't that big
 * of a deal, since the file really isn't meant to be read by people
 * anyway. */

static void write_atom(atom_type atom, void *v) {
char buffer[20];
int i, len;
char *p;

    switch(atom) {
    case ATOM_STRING:
    case ATOM_NAME:
	p = v;
	break;

    case ATOM_LPAREN:
	p = "(";
	break;

    case ATOM_RPAREN:
	p = ")";
	break;

    case ATOM_INTEGER:
	i = *((int *) v);
	if (i < 0)
	    g95_internal_error("write_atom(): Writing negative integer");

	sprintf(buffer, "%d", i);
	p = buffer;
	break;
    
    default:
	g95_internal_error("write_atom(): Trying to write dab atom");
    }

    len = (p == NULL) ? 0 : strlen(p);

    if (atom != ATOM_RPAREN) {
	if (module_column + len > 72)
	    write_char('\n');

	else if (last_atom != ATOM_LPAREN && module_column != 1)
	    write_char(' ');
    }

    if (atom == ATOM_STRING)
	write_char('\'');

    for(; len>0; len--) {
	if (atom == ATOM_STRING && *p == '\'')
	    write_char('\'');

	write_char(*p++);
    }

    if (atom == ATOM_STRING)
	write_char('\'');

    last_atom = atom;
}



/* g95_match_use()-- Match a USE statement */

match g95_match_use(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_use_rename *tail=NULL, *new;
interface_type type;
g95_symbol *sym;
g95_locus where;
int operator;
match m;

    if (G95_STRICT_F95()) {
	if (g95_match_space() == MATCH_NO)
	    return MATCH_NO;

    } else {
	if (g95_match(", intrinsic") == MATCH_YES)
	    mod_kind = MOD_INTRINSIC;

	else if (g95_match(", non_intrinsic") == MATCH_YES)
	    mod_kind = MOD_NONINTRINSIC;

	else if (g95_match_space() == MATCH_YES)
	    mod_kind = MOD_UNKNOWN;

	else
	    return MATCH_NO;

	g95_match(" ::");
    }

    m = g95_match_name(module_name);
    if (m != MATCH_YES)
	return m;

    where = g95_def_locus;

    if (g95_get_symbol(module_name, NULL, &sym))
	return MATCH_ERROR;

    if (sym->attr.flavor != FL_MODULE && 
	g95_add_flavor(&sym->attr, FL_MODULE, sym->name,
		       &sym->declared_at) == FAILURE)
	return MATCH_ERROR;

    if (sym->module == NULL)
	sym->module = sym->name;

    free_rename();
    only_flag = 0;

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    if (g95_match_char(',') != MATCH_YES)
	goto syntax;

    if (g95_match(" only :") == MATCH_YES)
	only_flag = 1;

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    for(;;) {
	new = g95_get_use_rename();
	new->found = 0;

	if (rename_list == NULL)
	    rename_list = new;

	else
	    tail->next = new;

	tail = new;
	new->operator = -1;

	g95_gobble_whitespace();
	new->where = g95_current_locus;

	if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)
	    goto cleanup;

	switch(type) {
	case INTERFACE_NAMELESS:
	    g95_error("Missing generic specification in USE statement at %C");
	    goto cleanup;

	case INTERFACE_GENERIC:
	    m = g95_match(" =>");

	    if (only_flag) {
		if (m != MATCH_YES)
		    strcpy(new->use_name, name);

		else {
		    strcpy(new->local_name, name);

		    m = g95_match_name(new->use_name);
		    if (m == MATCH_NO) goto syntax;
		    if (m == MATCH_ERROR) goto cleanup;
		}

	    } else {
		if (m != MATCH_YES) goto syntax;
		strcpy(new->local_name, name);

		m = g95_match_name(new->use_name);
		if (m == MATCH_NO) goto syntax;
		if (m == MATCH_ERROR) goto cleanup;
	    }

	    break;

	case INTERFACE_USER_OP:
	    strcpy(new->use_name, name);
	    /* Fall through */

	case INTERFACE_INTRINSIC_OP:
	    new->operator = operator;
	    break;

	default:
	    g95_internal_error("g95_match_use(): Bad interface");
	}

	if (g95_match_eos() == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_USE);

cleanup:
    free_rename();
    return MATCH_ERROR;
}



static void mio_rparen(void) {

    if (iomode == IO_OUTPUT)
	write_atom(ATOM_RPAREN, NULL);

    else
	require_atom(ATOM_RPAREN);
}



static void mio_lparen(void) {

    if (iomode == IO_OUTPUT)
	write_atom(ATOM_LPAREN, NULL);

    else
	require_atom(ATOM_LPAREN);
}



/* mio_name()-- Read or write an enumerated value.  On writing, we
 * return the input value for the convenience of callers.  We avoid
 * using an integer pointer because enums are sometimes inside bitfields. */

static int mio_name(int t, mstring *m) {

    if (iomode == IO_OUTPUT)
	write_atom(ATOM_NAME, g95_code2string(m, t));

    else {
	require_atom(ATOM_NAME);
	t = find_enum(m);
    }

    return t;
}



static void mio_integer(int *ip) {

    if (iomode == IO_OUTPUT)
	write_atom(ATOM_INTEGER, ip);

    else {
	require_atom(ATOM_INTEGER);
	*ip = atom_int;
    }
}



/* mio_allocated_string()-- Read or write a character pointer that
 * points to a string on the heap */

static void mio_allocated_string(char **sp) {

    if (iomode == IO_OUTPUT)
	write_atom(ATOM_STRING, *sp);

    else {
	require_atom(ATOM_STRING);
	*sp = atom_string;
    }
}



static void mio_pool_string(char **sp) {

    if (iomode == IO_OUTPUT)
	write_atom(ATOM_STRING, *sp);

    else {
	require_atom(ATOM_STRING);
	*sp = g95_get_string(atom_string);
	g95_free(atom_string);
    }
}



/* mio_internal_string()-- Read or write a string that is in static
 * memory or inside of some already-allocated structure */

static void mio_internal_string(char *string) {

    if (iomode == IO_OUTPUT)
	write_atom(ATOM_STRING, string);

    else {
	require_atom(ATOM_STRING);
	strcpy(string, atom_string);
	g95_free(atom_string);
    }
}



/* mio_symbol_attribute()-- Symbol attributes are stored in list with
 * the first three elements being the enumerated fields, while the
 * remaining elements (if any) indicate the individual attribute bits.
 * The access field is not saved-- it controls what symbols are
 * exported when a module is written. */

static void mio_symbol_attribute(symbol_attribute *attr) {
atom_type t;

    mio_lparen();

    attr->flavor    = mio_name(attr->flavor, flavors);
    attr->intent    = mio_name(attr->intent, intents);
    attr->proc      = mio_name(attr->proc, procedures);
    attr->if_source = mio_name(attr->if_source, ifsrc_types);
    attr->itype     = mio_name(attr->itype, itypes);
    attr->iproc     = mio_name(attr->iproc, iprocs);

    if (iomode == IO_OUTPUT) {
	if (attr->allocatable)   mio_name(AB_ALLOCATABLE, attr_bits);
	if (attr->dimension)     mio_name(AB_DIMENSION,   attr_bits);
	if (attr->external)      mio_name(AB_EXTERNAL,    attr_bits);
	if (attr->intrinsic)     mio_name(AB_INTRINSIC,   attr_bits);
	if (attr->optional)      mio_name(AB_OPTIONAL,    attr_bits);
	if (attr->pointer)       mio_name(AB_POINTER,     attr_bits);
	if (attr->target)        mio_name(AB_TARGET,      attr_bits);
	if (attr->dummy)         mio_name(AB_DUMMY,       attr_bits);
    
	if (attr->function)      mio_name(AB_FUNCTION,    attr_bits);
	if (attr->subroutine)    mio_name(AB_SUBROUTINE,  attr_bits);

	if (attr->sequence)      mio_name(AB_SEQUENCE,    attr_bits);
	if (attr->elemental)     mio_name(AB_ELEMENTAL,   attr_bits);
	if (attr->pure)          mio_name(AB_PURE,        attr_bits);
	if (attr->recursive)     mio_name(AB_RECURSIVE,   attr_bits);
	if (attr->bind)          mio_name(AB_BIND,        attr_bits);
	if (attr->value)         mio_name(AB_VALUE,       attr_bits);
	if (attr->protected)     mio_name(AB_PROTECTED,   attr_bits);

	if (attr->data)          mio_name(AB_DATA,        attr_bits);
	if (attr->in_namelist)   mio_name(AB_IN_NAMELIST, attr_bits);
	if (attr->in_common)     mio_name(AB_IN_COMMON,   attr_bits);
	if (attr->volatile_)     mio_name(AB_VOLATILE,    attr_bits);
	if (attr->invoked)       mio_name(AB_INVOKED,     attr_bits);
	if (attr->abstract)      mio_name(AB_ABSTRACT,    attr_bits);

	mio_rparen();

    } else {
	for(;;) {
	    t = parse_atom();
	    if (t == ATOM_RPAREN)
		break;

	    if (t != ATOM_NAME)
		bad_module("Expected attribute bit name");

	    switch(find_enum(attr_bits)) {
	    case AB_RECURSIVE:     attr->recursive = 1;     break;
	    case AB_ALLOCATABLE:   attr->allocatable = 1;   break;
	    case AB_DIMENSION:     attr->dimension = 1;     break;
	    case AB_EXTERNAL:      attr->external = 1;      break;
	    case AB_INTRINSIC:     attr->intrinsic = 1;     break;
	    case AB_OPTIONAL:      attr->optional = 1;      break;
	    case AB_POINTER:       attr->pointer = 1;       break;
	    case AB_TARGET:        attr->target = 1;        break;
	    case AB_DUMMY:         attr->dummy = 1;         break;
	    case AB_DATA:          attr->data = 1;          break;
	    case AB_IN_NAMELIST:   attr->in_namelist = 1;   break;
	    case AB_IN_COMMON:     attr->in_common = 1;     break;
	    case AB_FUNCTION:      attr->function = 1;      break;
	    case AB_SUBROUTINE:    attr->subroutine = 1;    break;
	    case AB_SEQUENCE:      attr->sequence = 1;      break;
	    case AB_ELEMENTAL:     attr->elemental = 1;     break;
	    case AB_PURE:          attr->pure = 1;          break;
	    case AB_PROTECTED:     attr->protected = 1;     break;
	    case AB_BIND:          attr->bind = 1;          break;
	    case AB_VALUE:         attr->value = 1;         break;
	    case AB_VOLATILE:      attr->volatile_ = 1;     break;
	    case AB_INVOKED:       attr->invoked = 1;       break;
	    case AB_ABSTRACT:      attr->abstract = 1;      break;
	    }
	}
    }
}



static void mio_charlen(g95_charlen **clp) {
g95_charlen *cl;
g95_expr *e;

    e = NULL;
    mio_lparen();

    if (iomode == IO_OUTPUT) {
	cl = *clp;
	if (cl == NULL)
	    ;

	else if (cl == &g95_unknown_charlen ||
		 (cl->length != NULL && cl->length->type != EXPR_CONSTANT))
	    mio_internal_string("UNK");

	else
	    mio_expr(&cl->length);

    } else {
	switch(peek_atom()) {
	case ATOM_STRING:
	    require_atom(ATOM_STRING);
 
	    if (strcmp(atom_string, "UNK") == 0)
		*clp = &g95_unknown_charlen;

	    else
		bad_module("Bad charlen");

	    g95_free(atom_string);
	    break;

	case ATOM_RPAREN:
	    break;

	case ATOM_LPAREN:
	    *clp = cl = g95_get_charlen(NULL);
	    mio_expr(&cl->length);
	    break;

	default:
	    bad_module("Bad charlen (2)");
	}
    }

    mio_rparen();
}



static void mio_array_spec(g95_array_spec **asp) {
g95_array_spec *as;
int i;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	if (*asp == NULL)
	    goto done;
	as = *asp;

    } else {
	if (peek_atom() == ATOM_RPAREN) {
	    *asp = NULL;
	    goto done;
	}

	*asp = as = g95_get_array_spec();
    }

    mio_integer(&as->rank);
    as->type = mio_name(as->type, array_spec_types);

    for(i=0; i<as->rank; i++) {
	mio_expr(&as->lower[i]);
	mio_expr(&as->upper[i]);
    }

done:
    mio_rparen();
}



static void mio_coarray_spec(g95_coarray_spec **cas_p) {
g95_coarray_spec *cas;
int i;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	if (*cas_p == NULL)
	    goto done;
	
	cas = *cas_p;

	i = (cas->type == CAS_DEFERRED);
	mio_integer(&i);

	mio_integer(&cas->corank);

    } else {
	if (peek_atom() == ATOM_RPAREN) {
	    *cas_p = NULL;
	    goto done;
	}

	*cas_p = cas = g95_get_coarray_spec();

	cas->artificial = 0;

	mio_integer(&i);

	cas->type = i ? CAS_DEFERRED : CAS_ASSUMED;
	mio_integer(&cas->corank);
    }

    for(i=0; i<cas->corank; i++) {
	mio_expr(&cas->lower[i]);
	mio_expr(&cas->upper[i]);
    }

done:
    mio_rparen();
}



static void mio_typespec(g95_typespec *ts) {

    mio_lparen();

    if (iomode == IO_INPUT)
	g95_clear_ts(ts);

    ts->type = mio_name(ts->type, bt_types);

    switch(ts->type) {
    case BT_INTEGER:
    case BT_REAL:
    case BT_COMPLEX:
    case BT_LOGICAL:
	mio_integer(&ts->kind);
	break;

    case BT_CHARACTER:
	mio_integer(&ts->kind);
	mio_charlen(&ts->cl);
	break;

    case BT_DERIVED:
	if (iomode == IO_OUTPUT && ts->derived->module == NULL &&
	    ts->derived->ns->state != COMP_MODULE)
	    ts->derived->module = "(unique)";

	mio_symbol_ref(&ts->derived);
	break;

    case BT_PROCEDURE:
	mio_symbol_ref(&ts->interface);
	break;

    case BT_UNKNOWN:
	break;

    default:
	g95_internal_error("mio_typespec(): Bad type");
    }

    mio_rparen();
}



/* mio_array_ref()-- Given a pointer to an array reference
 * structure (which lives in a g95_ref structure), find the
 * corresponding array specification structure.  Storing the pointer
 * in the ref structure doesn't quite work when loading from a module.
 * Generating code for an array reference also needs more infomation
 * than just the array specification. */

static void mio_array_ref(g95_array_ref *ar) {
int i;

    mio_lparen();
    ar->type = mio_name(ar->type, array_ref_types);
    mio_integer(&ar->dimen);

    switch(ar->type) {
    case AR_SECTION:
	for(i=0; i<ar->dimen; i++) {
	    mio_expr(&ar->start[i]);
	    mio_expr(&ar->end[i]);
	    mio_expr(&ar->stride[i]);
	}

	break;

    case AR_FULL:
	break;

    case AR_ELEMENT:
	for(i=0; i<ar->dimen; i++)
	    mio_expr(&ar->start[i]);

	break;

    case AR_UNKNOWN:
	g95_internal_error("mio_array_ref(): Unknown array ref");
    }

    for(i=0; i<ar->dimen; i++)
	mio_integer((int *) &ar->dimen_type[i]);

    if (iomode == IO_INPUT) {
	for(i=0; i<ar->dimen; i++)
	    ar->where[i] = g95_current_locus;
    }

    mio_rparen();
}



/* mio_coarray_ref()-- Read or write a coarray reference. */

static void mio_coarray_ref(g95_coarray_ref *car) {
int i;

    mio_lparen();

    mio_integer(&car->dimen);

    for(i=0; i<car->dimen; i++)
	mio_expr(&car->element[i]);

    mio_rparen();
}


/* mio_pointer_ref()-- Saves or restores a pointer.  The pointer is
 * converted back and forth from an integer.  We return the
 * pointer_info pointer so that the caller can take additional action
 * based on the pointer type. */

static pointer_info *mio_pointer_ref(void *gp) {
pointer_info *p;

    if (iomode == IO_OUTPUT) {
	p = get_pointer(*((char **) gp));
	write_atom(ATOM_INTEGER, &p->integer);

    } else {
	require_atom(ATOM_INTEGER);
	p = add_fixup(atom_int, gp);
    }

    return p;
}



/* mio_actual_arg()-- Save/restore an actual argument.  The argument
 * can't be part of a subroutine call, so we don't have to worry about
 * alternate return specs. */

static void mio_actual_arg(g95_actual_arglist *a) {
int *ip;

    mio_lparen();
    mio_pool_string(&a->name);

    ip = (int *) &a->type;
    mio_integer(ip);
    mio_expr(&a->u.expr);

    mio_rparen();
}



static void mio_actual_arglist(g95_actual_arglist **ap) {
g95_actual_arglist *a, *tail;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	for(a=*ap; a; a=a->next)
	    mio_actual_arg(a);

    } else {
	tail = NULL;

	for(;;) {
	    if (peek_atom() != ATOM_LPAREN)
		break;

	    a = g95_get_actual_arglist();

	    if (tail == NULL)
		*ap = a;

	    else
		tail->next = a;

	    tail = a;
	    mio_actual_arg(a);
	}
    }

    mio_rparen();
}



/* mio_formal_arglist()-- Read and write formal argument lists. */

static void mio_formal_arglist(g95_symbol *sym) {
g95_formal_arglist *f, *tail;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	for(f=sym->formal; f; f=f->next) {
	    mio_symbol_ref(&f->sym);
	    mio_name(f->cb, callby_types);
	}

    } else {
	sym->formal = tail = NULL;

	while(peek_atom() != ATOM_RPAREN) {
	    f = g95_get_formal_arglist();
	    mio_symbol_ref(&f->sym);
	    f->cb = mio_name(f->cb, callby_types);
	    f->where = g95_current_locus;

	    if (sym->formal == NULL)
		sym->formal = f;
	    else
		tail->next = f;

	    tail = f;
	}
    }

    mio_rparen();
}



static void mio_component(g95_component *c) {
pointer_info *p;
int n;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	p = get_pointer(c);
	mio_integer(&p->integer);

    } else {
	mio_integer(&n);
	p = get_integer(n);
	associate_integer_pointer(p, c);
    }

    if (p->type == P_UNKNOWN)
	p->type = P_COMPONENT;

    mio_pool_string(&c->name);
    mio_typespec(&c->ts);
    mio_array_spec(&c->as);
    mio_coarray_spec(&c->cas);

    mio_integer(&c->dimension);
    mio_integer(&c->pointer);
    mio_integer(&c->allocatable);

    mio_expr(&c->initializer);
    mio_rparen();
}



static void mio_iterator(g95_iterator **ip) {
g95_iterator *iter;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	if (*ip == NULL)
	    goto done;

    } else {
	if (peek_atom() == ATOM_RPAREN) {
	    *ip = NULL;
	    goto done;
	}

	*ip = g95_get_iterator();
    }

    iter = *ip;

    mio_expr(&iter->var);
    mio_expr(&iter->start);
    mio_expr(&iter->end);
    mio_expr(&iter->step);

done:
    mio_rparen();
}



static void mio_component_list(g95_component **cp) {
g95_component *c, *tail;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	for(c=*cp; c; c=c->next)
	    mio_component(c);

    } else {
	*cp  = NULL;
	tail = NULL;

	while(peek_atom() != ATOM_RPAREN) {
	    c = g95_get_component();
	    mio_component(c);

	    if (tail == NULL)
		*cp = c;

	    else
		tail->next = c;

	    tail = c;
	}
    }

    mio_rparen();
}



static void mio_constructor(g95_constructor **cp) {
g95_constructor *c, *tail;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	for(c=*cp; c; c=c->next) {
	    mio_lparen();
	    mio_expr(&c->expr);
	    mio_iterator(&c->iterator);
	    mio_rparen();
	}

    } else {
	*cp = NULL;
	tail = NULL;

	while(peek_atom() != ATOM_RPAREN) {
	    c = g95_get_constructor();

	    if (tail == NULL)
		*cp = c;

	    else
		tail->next = c;

	    tail = c;

	    mio_lparen();
	    mio_expr(&c->expr);
	    mio_iterator(&c->iterator);
	    mio_rparen();
	}
    }

    mio_rparen();
}



/* mio_symbol_ref()-- Save or restore a reference to a symbol node */

void mio_symbol_ref(g95_symbol **symp) {
pointer_info *p;

    p = mio_pointer_ref(symp);
    if (p->type == P_UNKNOWN)
	p->type = P_SYMBOL;

    if (iomode == IO_OUTPUT) {
	if (p->u.wsym.state == UNREFERENCED) {
	    p->u.wsym.state = NEEDS_WRITE;
	    p->u.wsym.link = secondary;
	    secondary = p;
	}

    } else {
	if (p->u.rsym.state == UNUSED)
	    p->u.rsym.state = NEEDED;
    }
}



static void mio_ref(g95_ref **rp) {
g95_ref *r;

    mio_lparen();

    r = *rp; 
    r->type = mio_name(r->type, ref_types);

    switch(r->type) {
    case REF_SUBSTRING:
	mio_expr(&r->u.ss.start);
	mio_expr(&r->u.ss.end);
	mio_charlen(&r->u.ss.length);
	break;

    case REF_ARRAY:
	mio_array_ref(&r->u.ar);
	break;

    case REF_COARRAY:
	mio_coarray_ref(&r->u.car);
	break;

    case REF_COMPONENT:
	mio_symbol_ref(&r->u.c.sym);
	mio_pool_string(&r->u.c.name);

	/* For input, r->u.c.component pointer fixed during resolution */
	break;
    }

    mio_rparen();
}



static void mio_ref_list(g95_ref **rp) {
g95_ref *ref, *head, *tail;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	for(ref=*rp; ref; ref=ref->next)
	    mio_ref(&ref);

    } else {
	head = tail = NULL;

	while(peek_atom() != ATOM_RPAREN) {
	    if (head == NULL)
		head = tail = g95_get_ref();
	    else {
		tail->next = g95_get_ref();
		tail = tail->next;
	    }

	    mio_ref(&tail);
	}

	*rp = head;
    }

    mio_rparen();
}



/* mio_bigint()-- Read and write an integer value */

static void mio_bigint(bignum *integer) {

    if (iomode == IO_INPUT) {
	if (parse_atom() != ATOM_STRING)
	    bad_module("Expected integer string");

	*integer = bi_from_string(atom_string, 10);
	big_permanent(*integer);
	g95_free(atom_string);

    } else
	write_atom(ATOM_STRING, bi_to_string(*integer));
}



static void mio_real(bignum *real, int kind) {
bignum b;
int sign;

    if (iomode == IO_INPUT) {
	if (parse_atom() != ATOM_STRING)
	    bad_module("Expected real string");

	b = big_clone(bi_from_string(atom_string, 10));
	b->ff = g95_get_ff(kind);
	g95_free(atom_string);

	big_permanent(b);
	*real = b;

	mio_integer(&sign);
	b->real_sign = sign ? -1 : 1;
	mio_integer(&b->real_exp);

    } else {
	b = *real;
	write_atom(ATOM_STRING, bi_to_string(b));

	sign = (b->real_sign < 0);
	mio_integer(&sign);
	mio_integer(&b->real_exp);
    }
}



/* mio_shape()-- Save and restore the shape of an array constructor. */

static void mio_shape(bignum **pshape, int rank) {
bignum *shape;
atom_type t;
int n;

    /* A NULL shape is represented by ().  */
    mio_lparen ();

    if (iomode == IO_OUTPUT) {
	shape = *pshape;
	if (!shape) {
	    mio_rparen();
	    return;
	}

    } else {
	t = peek_atom();
	if (t == ATOM_RPAREN) {
	    *pshape = NULL;
	    mio_rparen();
	    return;
	}

	shape = g95_get_shape(rank);
	*pshape = shape;
    }

    for(n=0; n<rank; n++)
	mio_bigint(&shape[n]);

    mio_rparen();
}



/* mio_expr()-- Read and write expressions.  The form "()" is allowed
 * to indicate a NULL expression */

static void mio_expr(g95_expr **ep) {
g95_expr *e;
atom_type t;
int flag;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	if (*ep == NULL) {
	    mio_rparen();
	    return;
	}

	e = *ep;
	mio_name(e->type, expr_types);

    } else {
	t = parse_atom();
	if (t == ATOM_RPAREN) {
	    *ep = NULL;
	    return;
	}

	if (t != ATOM_NAME)
	    bad_module("Expected expression type");

	e = *ep = g95_get_expr();
	e->where = g95_current_locus;
	e->type = find_enum(expr_types);
    }

    mio_typespec(&e->ts);
    mio_integer(&e->rank);

    switch(e->type) {
    case EXPR_PROCEDURE:
	mio_symbol_ref(&e->symbol);
	break;

    case EXPR_NULL:
	break;

    case EXPR_STRUCTURE:
	mio_symbol_ref(&e->symbol);
	mio_integer((int *) &e->value.constructor.ivalue);

	/* Fall through */

    case EXPR_ARRAY:
	mio_constructor(&e->value.constructor.c);
	mio_shape(&e->shape, e->rank);
	break;

    case EXPR_OP:
	e->value.op.operator = mio_name(e->value.op.operator, intrinsics);

	switch(e->value.op.operator) {
	case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  case INTRINSIC_NOT:
	case INTRINSIC_PAREN:
	    mio_expr(&e->value.op.op1);
	    break;

	case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:
	case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:
	case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:
	case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:
	case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:
	case INTRINSIC_LE:
	    mio_expr(&e->value.op.op1);
	    mio_expr(&e->value.op.op2);
	    break;

	default:
	    bad_module("Bad operator");
	}

	break;

    case EXPR_CONSTANT:
	switch(e->ts.type) {
	case BT_REAL:
	    mio_real(&e->value.real, e->ts.kind);
	    break;

	case BT_CHARACTER:
	    mio_integer(&e->value.character.length);
	    mio_allocated_string(&e->value.character.string);
	    break;

	case BT_COMPLEX:
	    mio_real(&e->value.complex.r, e->ts.kind);
	    mio_real(&e->value.complex.i, e->ts.kind);
	    break;

	case BT_INTEGER:
	    mio_bigint(&e->value.integer);
	    break;

	case BT_LOGICAL:
	    mio_integer(&e->value.logical);
	    break;

	default:
	    bad_module("Bad type in constant expression");
	}

	break;

    case EXPR_FUNCTION:
	mio_symbol_ref(&e->symbol);
	mio_actual_arglist(&e->value.function.actual);

	if (iomode == IO_OUTPUT) {
	    mio_allocated_string(&e->value.function.iname);
	    flag = e->value.function.isym == NULL;
	    mio_integer(&flag);

	    if (flag)
		mio_symbol_ref(&e->symbol);
	    else
		write_atom(ATOM_STRING, e->value.function.isym->name);

	} else {
	    require_atom(ATOM_STRING);
	    e->value.function.iname = e->value.function.name =
		g95_get_string(atom_string);

	    g95_free(atom_string);

	    mio_integer(&flag);

	    if (flag)
		mio_symbol_ref(&e->symbol);

	    else {
		require_atom(ATOM_STRING);
		e->value.function.isym = g95_find_function(atom_string);
		g95_free(atom_string);
	    }
	}

	break;

    case EXPR_VARIABLE:
	mio_symbol_ref(&e->symbol);
	mio_ref_list(&e->ref);
	break;

    case EXPR_SUBSTRING:
	mio_allocated_string(&e->value.character.string);
	mio_ref_list(&e->ref);
	break;

    case EXPR_UNKNOWN:
	g95_internal_error("mio_expr(): Unknown expression");
    }

    mio_rparen();
}



/* mio_interface_rest()-- Save/restore lists of g95_interface
 * structures.  When loading an interface, we are really appending to
 * the existing list of interfaces.  Checking for ambiguous interfaces
 * has to be done later when all symbols have been loaded */

static void mio_interface_rest(g95_interface **ip) {
g95_interface *tail, *p, *q;

    if (iomode == IO_OUTPUT) {
	if (ip != NULL)
	    for(p=*ip; p; p=p->next) {
		for(q=*ip; q!=p; q=q->next)
		    if (q->sym == p->sym)
			break;

		if (q == p)
		    mio_symbol_ref(&p->sym);
	    }

    } else {
	if (*ip == NULL)
	    tail = NULL;
	else {
	    tail = *ip;
	    while(tail->next)
		tail = tail->next;
	}

	for(;;) {
	    if (peek_atom() == ATOM_RPAREN)
		break;

	    p = g95_get_interface();
	    p->where = g95_current_locus;
	    mio_symbol_ref(&p->sym);

	    if (tail == NULL)
		*ip = p;
	    else
		tail->next = p;

	    tail = p;
	}
    }

    mio_rparen();
}



/* mio_interface()-- Save/restore a nameless operator interface */

static void mio_interface(g95_interface **ip) {

    mio_lparen();
    mio_interface_rest(ip);
}



/* mio_namelist()-- Read and write namelists */

static void mio_namelist(g95_symbol *sym) {
g95_namelist *n, *m;

    mio_lparen();

    if (iomode == IO_OUTPUT) {
	for(n=sym->namelist; n; n=n->next) {
	    mio_symbol_ref(&n->sym);
	    mio_pool_string(&n->name);
	}

    } else {
	m = NULL;
	while(peek_atom() != ATOM_RPAREN) {
	    n = g95_get_namelist();
	    mio_symbol_ref(&n->sym);
	    mio_pool_string(&n->name);

	    if (sym->namelist == NULL)
		sym->namelist = n;

	    else
		m->next = n;

	    m = n;
	}
    }

    mio_rparen();
}



static void mio_namespace_ref(g95_namespace **nsp) {
g95_namespace *ns;
pointer_info *p;

    p = mio_pointer_ref(nsp);

    if (p->type == P_UNKNOWN)
	p->type = P_NAMESPACE;

    if (iomode == IO_INPUT && p->integer != 0 && p->u.pointer == NULL) {
	ns = g95_get_namespace(NULL, 0);
	associate_integer_pointer(p, ns);
    }
}



/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in. */

static void mio_symbol(g95_symbol *sym) {

    mio_lparen();

    mio_symbol_attribute(&sym->attr);

    if (iomode == IO_INPUT && sym->attr.flavor == FL_DERIVED) {
	g95_set_usage(sym, NULL, 1, 0);

	if (sym->attr.itype != IPROC_NONE &&
	    g95_current_ns->itypes[sym->attr.itype] == NULL)
	    g95_current_ns->itypes[sym->attr.itype] = sym;
    }

    mio_typespec(&sym->ts);
    mio_namespace_ref(&sym->formal_ns);

    mio_symbol_ref(&sym->common_next);  /* Save/restore common block links */
    mio_formal_arglist(sym);

    if (sym->attr.flavor == FL_PARAMETER)  
	mio_expr(&sym->value);

    mio_array_spec(&sym->as);
    mio_coarray_spec(&sym->cas);

    mio_allocated_string(&sym->bind);

    if (iomode == IO_INPUT && sym->attr.function)
	sym->result = sym;

/* Note that components are always saved, even if they are supposed
 * to be private.  Component access is checked during searching */

    mio_component_list(&sym->components);

    if (sym->components != NULL)
	sym->component_access = mio_name(sym->component_access, access_types);

    mio_namelist(sym);
    mio_rparen();
}



/* skip_list()-- Skip a list between balanced left and right parens. */

static void skip_list(void) {
int level; 

    level = 0;
    do {
	switch(parse_atom()) {
	case ATOM_LPAREN:
	    level++;
	    break;

	case ATOM_RPAREN:
	    level--;
	    break;

	case ATOM_STRING:
	    g95_free(atom_string);
	    break;

	case ATOM_INTEGER:
	case ATOM_NAME:
	    break;
	}
    } while(level > 0);
}



/* load_operator_interfaces()-- Load operator interfaces from the
 * module.  Interfaces are unusual in that they attach themselves to
 * existing symbols. */

static void load_operator_interfaces(void) {
char *p, name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];
g95_user_op *uop;
module_locus m;

    mio_lparen();

    while(peek_atom() != ATOM_RPAREN) {
	mio_lparen();

	mio_internal_string(name);
	mio_internal_string(module);

	get_module_locus(&m);

	for(;;) {
	    p = find_use_name(name);
	    if (p == NULL)
		break;

	    uop = g95_get_uop(p);
	    mio_interface_rest(&uop->operator);

	    set_module_locus(&m);
	}

	while(parse_atom() != ATOM_RPAREN);
    }

    mio_rparen();
}



/* read_commons()-- Load common blocks */

static void read_commons(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_common_head *h, *g;
int flag;

    mio_lparen();

    while(peek_atom() != ATOM_RPAREN) {
	mio_internal_string(name);
	mio_integer(&flag);

	while(peek_atom() == ATOM_INTEGER) {

	    if (name[0] == '\0') {   /* Blank common */
		h = g95_get_common_head();
		h->next = g95_current_ns->blank_common;
		g95_current_ns->blank_common = h;

	    } else {                 /* Nonblank common */
		h = g95_find_common(name, NULL);
		if (h == NULL)
		    h = g95_get_common(name, NULL);

		else {
		    g = g95_get_common_head();
		    g->where = g95_current_locus;

		    g->next = h->next;
		    h->next = g;
		    h = g;
		}
	    }

	    h->use_assoc = 1;
	    h->saved = flag;
	    mio_symbol_ref(&h->head);
	}
    }

    mio_rparen();
}



/* load_needed()-- Recursive function to traverse the pointer_info
 * tree and load a needed symbol.  We return nonzero if we load a
 * symbol and stop the traversal, because the act of loading can alter
 * the tree. */

static int load_needed(pointer_info *p) {
g95_namespace *ns;
pointer_info *q;
g95_symbol *sym;
char *name;

    if (p == NULL)
	return 0;

    if (load_needed(p->left) || load_needed(p->right))
	return 1;

    name = g95_current_ns->proc_name->name;

    if (p->type != P_SYMBOL || p->u.rsym.state != NEEDED)
	return 0;

    p->u.rsym.state = USED;

    set_module_locus(&p->u.rsym.where);

    sym = p->u.rsym.sym;
    if (sym == NULL) {
	q = get_integer(p->u.rsym.ns);

	ns = (g95_namespace *) q->u.pointer;
	if (ns == NULL) {
	    /* Create an interface namespace if necessary.  These are
	     * the namespaces that hold the formal parameters of
	     * module procedures. */

	    ns = g95_get_namespace(NULL, 0);
	    associate_integer_pointer(q, ns);
	}

	sym = g95_new_symbol(p->u.rsym.true_name, ns);
	sym->module = g95_get_string(p->u.rsym.module);

	associate_integer_pointer(p, sym);
    }

    mio_symbol(sym);
    sym->attr.use_assoc = 1;

    return 1;
}



/* read_generic_interfaces()-- Load generic interfaces from the module. */

static void read_generic_interfaces(void) {
g95_symtree *st;
module_locus m;
char *p;

    mio_lparen();

    while(peek_atom() != ATOM_RPAREN) {
	mio_lparen();

	require_atom(ATOM_STRING);
	get_module_locus(&m);

	for(;;) {
	    p = find_use_name(atom_string);
	    if (p == NULL)
		break;

	    st = g95_get_generic(p, NULL);

	    mio_interface_rest(&st->n.generic);
	    set_module_locus(&m);
	}

	while(parse_atom() != ATOM_RPAREN);

	g95_free(atom_string);
    }

    mio_rparen();
}



/* read_cleanup()-- Recursive function for cleaning up things after a
 * module has been read. */

static void read_cleanup(pointer_info *p) {
g95_symtree *st;
pointer_info *q;

    if (p == NULL)
	return;

    read_cleanup(p->left);
    read_cleanup(p->right);

    if (p->type == P_SYMBOL && p->u.rsym.state == USED &&
	!p->u.rsym.referenced) {

	q = get_integer(p->u.rsym.ns);
	st = get_unique_symtree((g95_namespace *) q->u.pointer);

	st->n.sym = p->u.rsym.sym;
	st->n.sym->refs++;
	if (st->n.sym->refs == 1)
	    st->n.sym->attr.hidden = 1;
    }

    if (p->type == P_SYMBOL && p->u.rsym.state == UNUSED)
	g95_free_symbol(p->u.rsym.sym);
}



/* load_equiv()-- Load equivalences. */

static void load_equiv(void) {
g95_equiv *head, *tail, *head0, *tail0, *p1, *p2, *q1, *q2;
int flag;

    mio_lparen();

    head0 = tail0 = NULL;

    while(peek_atom() != ATOM_RPAREN) {
	mio_lparen();
	head = tail = NULL;

	while(peek_atom() != ATOM_RPAREN) {
	    if (head == NULL)
		head = tail = g95_get_equiv();
	    else {
		tail->eq = g95_get_equiv();
		tail = tail->eq;
	    }

	    mio_pool_string(&tail->module);
	    mio_expr(&tail->expr);
	}

	if (tail0 == NULL)
	    head0 = tail0 = head;

	else {
	    tail0->next = head;
	    tail0 = tail0->next;
	}

	mio_rparen();
    }

    mio_rparen();

    /* At this point we have the new equivalence chain in head0.  If
     * the new chain contains a not-seen-before module, add it.
     * Otherwise, we can end up with lots of duplicate equivs. */

    flag = 0;

    for(q1=head0; q1; q1=q1->next)
	for(q2=q1; q2; q2=q2->eq) {

	    for(p1=g95_current_ns->equiv; p1; p1=p1->next)
		for(p2=p1; p2; p2=p2->eq)
		    if (p2->module == q2->module)
			goto found;

	    flag = 1;
	    goto done;

	found:
	    ;
	}

done:
    if (flag) {
	if (g95_current_ns->equiv == NULL)
	    g95_current_ns->equiv = head0;

	else {
	    p1 = g95_current_ns->equiv;
	    while(p1->next != NULL)
		p1 = p1->next;

	    p1->next = head0;
	}
    }
}



/* check_access()-- Given an access type that is specific to an entity
 * and the default access, return nonzero if we should write the
 * entity. */

static int check_access(g95_access specific_access,
			g95_access default_access) {

    if (specific_access == ACCESS_PUBLIC)  return 1;
    if (specific_access == ACCESS_PRIVATE) return 0;

    if (g95_option.module_access_private) {
	if (default_access == ACCESS_PUBLIC)  return 1;

    } else if (default_access != ACCESS_PRIVATE) return 1;

    return 0;
}



/* read_module()-- Read a module file */

static void read_module(void) {
module_locus where, section[SECTIONS];
char *p, name[G95_MAX_SYMBOL_LEN+1];
int i, ambiguous, symbol;
pointer_info *info;
g95_use_rename *u;
g95_symtree *st;
g95_symbol *sym;

    memset(&section, '\0', sizeof(section));

    for(i=0; i<SECTIONS; i++) {
	get_module_locus(&section[i]);
	skip_list();
    }

    set_module_locus(&section[SECT_SYMBOL]);
    mio_lparen();

    while(peek_atom() != ATOM_RPAREN) {
	require_atom(ATOM_INTEGER);
	info = get_integer(atom_int);

	info->type = P_SYMBOL;
	info->u.rsym.state = UNUSED;

	mio_internal_string(info->u.rsym.true_name);
	mio_internal_string(info->u.rsym.module);

	require_atom(ATOM_INTEGER);
	info->u.rsym.ns = atom_int;

	get_module_locus(&info->u.rsym.where);
	skip_list();

	/* See if the symbol has already been loaded by a previous
	 * module.  If so, we reference the existing symbol and
	 * prevent it from being loaded again. */

	sym = find_true_name(info->u.rsym.true_name, info->u.rsym.module);
	if (sym != NULL) {
	    info->u.rsym.state = USED;
	    info->u.rsym.referenced = 1;
	    info->u.rsym.sym = sym;
	}
    }

    mio_rparen();

    /* Parse the symtree lists.  This lets us mark which symbols need
     * to be loaded.  Renaming is also done at this point by replacing
     * the symtree name. */

    mio_lparen();

    while(peek_atom() != ATOM_RPAREN) {
	mio_internal_string(name);
	mio_integer(&ambiguous);
	mio_integer(&symbol);

	info = get_integer(symbol);

	/* See what we need to do with this name. */

	for(;;) {
	    p = find_use_name(name);
	    if (p == NULL)
		break;

	    st = g95_find_symtree(g95_current_ns->sym_root, p);
	    if (st != NULL) {
		sym = st->n.sym;

		if (sym->attr.flavor == FL_MODULE &&
		    strcmp(sym->name, module_name) == 0)
		    continue;  /* Special case */

		if (strcmp(sym->name, info->u.rsym.true_name) != 0 ||
		    (g95_strcmp(sym->module, info->u.rsym.module) != 0 &&
		     strcmp(info->u.rsym.module, "(global)") != 0))
		    st->ambiguous = 1;

		if (g95_current_block() == sym) {
		    g95_error("Module '%s' at %C redefines the current "
			      "program unit '%s'", module_name, sym->name);
		    st->ambiguous = 1;
		}

	    } else {
		st = check_unique_name(p)
		    ? get_unique_symtree(g95_current_ns)
		    : g95_new_symtree(&g95_current_ns->sym_root, p);

		st->ambiguous = ambiguous;
		sym = info->u.rsym.sym;

		if (sym == NULL) {
		    sym = info->u.rsym.sym =
			g95_new_symbol(info->u.rsym.true_name, g95_current_ns);

		    sym->module = g95_get_string(info->u.rsym.module);

		    if (only_name.nextc != NULL)
			sym->declared_at = only_name;
		}

		st->n.sym = sym;
		st->n.sym->refs++;

		if (info->u.rsym.state == UNUSED)
		    info->u.rsym.state = NEEDED;

		info->u.rsym.referenced = 1;
	    }
	}
    }

    mio_rparen();

    /* Load intrinsic operator interfaces. */

    set_module_locus(&section[SECT_INTRINSIC_OP]);
    mio_lparen();

    for(i=0; i<G95_INTRINSIC_OPS; i++) {
	if (i == INTRINSIC_USER)
	    continue;

	if (only_flag) {
	    u = find_use_operator(i);

	    if (u == NULL) {
		skip_list();
		continue;
	    }

	    get_module_locus(&where);

	    parse_atom();
	    u->found = (parse_atom() != ATOM_RPAREN);

	    set_module_locus(&where);
	}

	mio_interface(&g95_current_ns->operator[i]);
    }

    mio_rparen();

/* Load generic and user operator interfaces.  These must follow the
 * loading of symtree because otherwise symbols can be marked as
 * ambiguous */

    set_module_locus(&section[SECT_USER_OP]);

    load_operator_interfaces();

    skip_list();

    read_commons();
    load_equiv();

    /* At this point, we read those symbols that are needed but
     * haven't been loaded yet.  If one symbol requires another, the
     * other gets marked as NEEDED if its previous state was
     * UNUSED. */

    while(load_needed(pi_root));

    set_module_locus(&section[SECT_GENERIC]);
    read_generic_interfaces();

    while(load_needed(pi_root));

    g95_check_interfaces(g95_current_ns);

/* Clean up symbol nodes that were never loaded, create references to
 * hidden symbols. */

    read_cleanup(pi_root);
}



/* set_module_name()-- Decide on the module name of this symbol.
 * Procedures that are not module procedures of this module and aren't
 * generic don't get a module name. */

static void set_module_name(g95_symbol *sym) {
g95_namespace *ns;

    if (sym->module != NULL || sym->attr.dummy)
	return;

    if (sym->attr.flavor == FL_PROCEDURE && sym->attr.proc != PROC_MODULE) {
	for(ns=sym->ns->contained; ns; ns=ns->sibling)
	    if (ns->proc_name == sym)
		break;

	/* At this point, the symbol is an external procedure that does
	 * not live in a module.  Symbols without modules are not matched
	 * globally by the module read subroutine, but these need to be,
	 * even though they are not in a module.  We stick them into a
	 * "module" with an otherwise illegal name. */

	if (ns == NULL) {
	    sym->module = g95_get_string("(global)");
	    return;
	}
    }

    sym->module = g95_get_string(module_name);
}



/* write_symbol()-- Write a symbol to the module. */

static void write_symbol(int n, g95_symbol *sym) {
g95_symbol new;

    if (sym->attr.flavor == FL_UNKNOWN || sym->attr.flavor == FL_LABEL)
	g95_internal_error("write_symbol(): bad module symbol '%s'",
			   sym->name);

    if (sym->attr.artificial)
	return;

    mio_integer(&n);
    mio_internal_string(sym->name);
    mio_internal_string(sym->module);
    mio_pointer_ref(&sym->ns);

    if (!sym->attr.function || sym->result == sym ||
	sym->attr.iproc != IPROC_NONE)
	mio_symbol(sym);

    else {
	new = *sym;

	new.ts = sym->result->ts;
	new.as = sym->result->as;

	new.attr.allocatable = sym->result->attr.allocatable;
	new.attr.dimension = sym->result->attr.dimension;
	new.attr.pointer = sym->result->attr.pointer;

	new.result = NULL;

	mio_symbol(&new);
    }

    write_char('\n');
}



/* write_secondary()-- Unlink the head of the secondary symbol list
 * and write it. */

static void write_secondary(void) {
pointer_info *p;

    p = secondary;

    secondary = p->u.wsym.link;
    p->u.wsym.link = NULL;

    if (p->type == P_SYMBOL) {
	p->u.wsym.state = WRITTEN;
	write_symbol(p->integer, p->u.wsym.sym);
    }
}



/* write_symbol0()-- Recursive traversal function to write the initial
 * set of symbols to the module.  We check to see if the symbol should
 * be written according to the access specification. */

static void write_symbol0(g95_symtree *st) {
g95_symbol *sym;
pointer_info *p;

    if (st == NULL)
	return;

    write_symbol0(st->left);

    sym = st->n.sym;
    set_module_name(sym);

    if (check_access(sym->attr.access, sym->ns->default_access)) {
	p = get_pointer(sym);
	if (p->type == P_UNKNOWN)
	    p->type = P_SYMBOL;

	if (p->u.wsym.state != WRITTEN) {
	    write_symbol(p->integer, sym);
	    p->u.wsym.state = WRITTEN;
	}
    }

    write_symbol0(st->right);
}



/* write_generic_interfaces()-- Write generic interfaces. */

static void write_generic_interfaces(g95_symtree *st) {

    if (st == NULL)
	return;

    write_generic_interfaces(st->left);

    if (check_access(st->access, g95_current_ns->default_access)) {
	mio_lparen();
	mio_internal_string(st->name);
	mio_interface_rest(&st->n.generic);
    }

    write_generic_interfaces(st->right);
}



/* write_operator()-- Write operator interfaces associated with a symbol. */

static void write_operator(g95_user_op *uop) {
static char nullstring[] = "";

    if (uop->operator == NULL ||
	!check_access(uop->access, uop->ns->default_access))
	return;

    mio_lparen();

    mio_internal_string(uop->name);
    mio_internal_string(nullstring);

    mio_interface_rest(&uop->operator);
}



/* write_common()-- Write a common block to the module */

static void write_common(g95_symtree *st) {
g95_common_head *g, *h;

    if (st == NULL)
	return;

    write_common(st->left);
    write_common(st->right);

    mio_internal_string(st->name);

    h = st->n.common;
    mio_integer(&h->saved);

    for(; h; h=h->next) {
	for(g=st->n.common; g!=h; g=g->next)
	    if (g->head == h->head)
		break;

	if (g == h)
	    mio_symbol_ref(&h->head);
    }
}



/* write_blank_common()-- Save a blank commons defined in the current
 * module. */

static void write_blank_common(void) {
g95_common_head *h;
char name[1];
int flag;

    name[0] = '\0';
    flag = 0;

    for(h=g95_current_ns->blank_common; h; h=h->next) {
	mio_internal_string(name);
	mio_integer(&flag);
	mio_symbol_ref(&h->head);
    }
}



/* write_equiv()-- Write equivalences */

static void write_equiv(void) {
g95_equiv *eq, *e;

    for(eq=g95_current_ns->equiv; eq; eq=eq->next) {
	mio_lparen();

	for(e=eq; e; e=e->eq) {
	    if (e->module == NULL)
		e->module = g95_get_string(module_name);

	    mio_allocated_string(&e->module);
	    mio_expr(&e->expr);
	}

	mio_rparen();
    }
}



static void write_symtree(g95_symtree *st) {
g95_symbol *sym;
pointer_info *p;

    sym = st->n.sym;

    /* The inner outputs here prevent writing statement constructs,
     * but will write statement constructs that are explicitly declared.*/

    if (!check_access(sym->attr.access, sym->ns->default_access) ||
	(sym->attr.implicit_type && sym->attr.st_construct) ||
	(sym->attr.st_construct && sym->attr.artificial) ||
	check_unique_name(st->name))
	return;

    p = find_pointer(sym);
    if (p == NULL)
	g95_internal_error("write_symtree(): Symbol not written");

    mio_internal_string(st->name);
    mio_integer(&st->ambiguous);
    mio_integer(&p->integer);
}



static void write_module(void) {
int i;

    /* Write the operator interfaces */
    mio_lparen();

    for(i=0; i<G95_INTRINSIC_OPS; i++) {
	if (i == INTRINSIC_USER)
	    continue;

	mio_interface(check_access(g95_current_ns->operator_access[i],
				   g95_current_ns->default_access)
		      ? &g95_current_ns->operator[i] : NULL);
    }

    mio_rparen();
    write_char('\n');  write_char('\n');

    mio_lparen();
    g95_traverse_user_op(g95_current_ns, write_operator);
    mio_rparen();
    write_char('\n');  write_char('\n');

    mio_lparen();
    write_generic_interfaces(g95_current_ns->generic_root);
    mio_rparen();
    write_char('\n');  write_char('\n');

    mio_lparen();
    write_blank_common();
    write_common(g95_current_ns->common_root);
    mio_rparen();
    write_char('\n');  write_char('\n');

    mio_lparen();
    write_equiv();
    mio_rparen();
    write_char('\n');  write_char('\n');

    /* Write symbol information.  First we traverse all symbols in the
     * primary namespace, writing those that need to be written.
     * Sometimes writing one symbol will cause another to need to be
     * written.  A list of these symbols ends up on the write stack, and
     * we end by popping the bottom of the stack and writing the symbol
     * until the stack is empty.  */

    mio_lparen();

    write_symbol0(g95_current_ns->sym_root);

    while(secondary != NULL)
	write_secondary();

//  while(write_symbol1(pi_root));

    mio_rparen();
    write_char('\n');  write_char('\n');

    mio_lparen();
    g95_traverse_symtree(g95_current_ns, write_symtree);
    mio_rparen();
}



/* g95_module_parameter()-- Create a parameter for an intrinsic
 * module.  Returns nonzero if we the symbol was inserted into the
 * current module. */

int g95_module_parameter(char *name, g95_expr *value) {
char *local_name;
g95_symbol *sym;
g95_symtree *st;
int rc;

    rc = 0;

    for(;;) {
	local_name = find_use_name(name);
	if (local_name == NULL)
	    break;

	st = g95_find_symtree(g95_current_ns->sym_root, local_name);
	if (st != NULL) {
	    sym = st->n.sym;
	    if (strcmp(sym->module, imod) != 0)
		st->ambiguous = 1;

	} else {
	    st = g95_new_symtree(&g95_current_ns->sym_root, local_name);
	    st->n.sym = sym = g95_new_symbol(local_name, g95_current_ns);

	    sym->module = imod;
	    sym->attr.flavor = FL_PARAMETER;
	    sym->attr.use_assoc = 1;
	    sym->attr.set = 1;
	    sym->ts = value->ts;
	    sym->declared_at = (only_name.nextc != NULL)
		? only_name
		: g95_current_locus;
	    sym->refs++;

	    sym->value = g95_copy_expr(value);
	    rc = 1;
	}
    }

    g95_free_expr(value);
    return rc;
}



/* init_type_symbol()-- Initialize an internal type symbol */

static void init_type_symbol(g95_symbol *sym, internal_type itype) {

    sym->module = imod;
    sym->attr.flavor = FL_DERIVED;
    sym->attr.use_assoc = 1;
    sym->attr.itype = itype;
    sym->component_access = ACCESS_PRIVATE;

    sym->declared_at = g95_current_locus;
    sym->refs++;

    if (g95_current_ns->itypes[itype] == NULL)
	g95_current_ns->itypes[itype] = sym;
}



/* g95_module_operator()-- Create an intrinsic module operator.
 * Really just sets the appropriate flag. */

void g95_module_operator(int op) {
g95_use_rename *u;

    for(u=rename_list; u; u=u->next)
	if (u->operator == op)
	    break;

    if (u == NULL && only_flag)
	return;

    if (u != NULL)
	u->found = 1;

    if (op == INTRINSIC_EQ)
	g95_current_ns->ieee_class_eq = 1;

    else if (op == INTRINSIC_NE)
	g95_current_ns->ieee_class_ne = 1;

    else
	g95_internal_error("g95_module_operator()-- Unknown operator");
}



/* g95_module_proc()-- Create an intrinsic module procedure */

void g95_module_proc(char *name, internal_proc proc) {
char *local_name;
g95_typespec ts;
g95_symbol *sym;
g95_symtree *st;
int sub;

    g95_clear_ts(&ts);
    sub = 0;

    switch(proc) {
    case IPROC_NONE:
	g95_internal_error("g95_intrinsic_proc(): got IPROC_NONE");
	break;

    case IPROC_C_ASSOCIATED:
	ts.type = BT_LOGICAL;
	ts.kind = g95_default_logical_kind();
	break;

    case IPROC_C_LOC:
	ts.type = BT_DERIVED;
	ts.derived = g95_current_ns->itypes[ITYPE_C_PTR];
	break;

    case IPROC_C_FUNLOC:
	ts.type = BT_DERIVED;
	ts.derived = g95_current_ns->itypes[ITYPE_C_FUNPTR];
	break;

    case IPROC_C_F_PROCPOINTER:
    case IPROC_C_F_POINTER:
	sub = 1;
	break;

    default:
	break;
    }

    for(;;) {
	local_name = find_use_name(name);
	if (local_name == NULL)
	    break;

	st = g95_find_symtree(g95_current_ns->sym_root, local_name);
	if (st != NULL) {
	    sym = st->n.sym;
	    if (strcmp(sym->module, imod) != 0)
		st->ambiguous = 1;

	} else {
	    st = g95_new_symtree(&g95_current_ns->sym_root, local_name);
	    st->n.sym = sym = g95_new_symbol(local_name, g95_current_ns);

	    sym->module = imod;
	    sym->attr.flavor = FL_PROCEDURE;
	    sym->attr.use_assoc = 1;
	    sym->attr.iproc = proc;
	    sym->attr.proc = PROC_MODULE;
	    sym->attr.if_source = IFSRC_DECL;
	    sym->ts = ts;
	    sym->declared_at = (only_name.nextc != NULL)
		? only_name
		: g95_current_locus;
	    sym->refs++;

	    if (sub)
		sym->attr.subroutine = 1;
	    else
		sym->attr.function   = 1;
	}
    }
}



/* g95_module_type()-- Create an internal module type. */

void g95_module_type(char *name, internal_type itype) {
char *local_name;
g95_symbol *sym;
g95_symtree *st;
int flag;

    flag = 1;

    for(;;) {
	local_name = find_use_name(name);
	if (local_name == NULL)
	    break;

	st = g95_find_symtree(g95_current_ns->sym_root, local_name);
	if (st != NULL) {
	    sym = st->n.sym;

	    if (strcmp(sym->module, imod) != 0)
		st->ambiguous = 1;

	    continue;
	}

	st = g95_find_symtree(g95_current_ns->sym_root, local_name);

	if (st != NULL) {
	    sym = st->n.sym;
	    if (sym->module != imod)
		st->ambiguous = 1;

	} else {
	    flag = 0;

	    st = g95_new_symtree(&g95_current_ns->sym_root, local_name);
	    st->n.sym = sym = g95_new_symbol(local_name, g95_current_ns);

	    init_type_symbol(sym, itype);

	    if (only_name.nextc != NULL)
		sym->declared_at = only_name;
	}
    }

    /* If we didn't actually create the type node, create an
     * inaccessible one now, and mark it as used. */

    if (flag) {
	st = get_unique_symtree(g95_current_ns);
	st->n.sym = sym = g95_new_symbol(name, g95_current_ns);

	init_type_symbol(sym, itype);
	sym->attr.used = 1;
    }
}



/* compare_module()-- Compare a module with its earlier incarnation.
 * Returns nonzero if they are the same.  The first lines are almost
 * certainly different. */

static int compare_module(void) {
int c;

    fseek(module_fp,     0L, SEEK_SET);
    fseek(old_module_fp, 0L, SEEK_SET);

    /* Skip the banner lines */

    do {
	c = fgetc(module_fp);
	if (c == EOF)
	    return 0;

    } while(c != '\n');

    do {
	c = fgetc(old_module_fp);
	if (c == EOF)
	    return 0;

    } while(c != '\n');

    /* Compare the module bodies */

    do {
	c = fgetc(module_fp);
	if (c != fgetc(old_module_fp))
	    return 0;

    } while(c != EOF);

    return 1;
}



/* g95_dump_module()-- The current program unit is a module that needs
 * to be dumped to disk. */

void g95_dump_module(char *name) {
char filename[PATH_MAX], tempfile[PATH_MAX], *p;
time_t now;
int m;

    filename[0] = '\0';
    if (g95_option.module_dir != NULL)
	strcpy(filename, g95_option.module_dir);

    strcat(filename, name);
    strcat(filename, MODULE_EXTENSION);

    g95_target_file(filename);

    tempfile[0] = '\0';
    if (g95_option.module_dir != NULL)
	strcpy(tempfile, g95_option.module_dir);

    strcat(tempfile, name);
    strcat(tempfile, ".tmod");

    old_module_fp = fopen(filename, "r");
    module_fp = fopen((old_module_fp != NULL) ? tempfile : filename, "w+");

    if (module_fp == NULL)
	g95_fatal_error("Can't open module file '%s' at %C for writing: %s", 
			filename, strerror(errno));

#ifdef __GLIBC__
    muntrace();
#endif

    now = time(NULL);
    p = ctime(&now);  /* GLIBC 2.1 has a memory leak here */

#ifdef __GLIBC__
    mtrace();
#endif

    *strchr(p, '\n') = '\0';

    fprintf(module_fp, "G95 module created on %s from %s\n", p,
	    g95_source_file);
    fprintf(module_fp, "If you edit this, you'll get what you deserve.\n"
	    "module-version %d\n", COMPAT_VERSION);

    iomode = IO_OUTPUT;
    strcpy(module_name, name);

    init_pi_tree();

    write_module();

    free_pi_tree(pi_root);
    pi_root = NULL;

    write_char('\n');

    if (fflush(module_fp))
	g95_fatal_error("Error writing module file '%s': %s", filename,
			strerror(errno));

    if (old_module_fp == NULL) {
	fclose(module_fp);
	return;
    }

    m = compare_module();

    fclose(module_fp);
    fclose(old_module_fp);

    if (m)
	unlink(tempfile);

    /* Call unlink() because of a bug in mingw's rename(). */
    else if (unlink(filename) || rename(tempfile, filename))
	g95_fatal_error("Error overwriting module file '%s': %s",
			filename, strerror(errno));
}



/* use_external()-- Use an external module.  Returns nonzero if
 * something goes wrong and sets the error message. */

static int use_external(char *message) {
char filename[G95_MAX_SYMBOL_LEN+5];
int c, line, version;
g95_state_data *p;

    strcpy(filename, module_name);
    strcat(filename, MODULE_EXTENSION);

    module_fp = g95_open_included_file(filename);
    if (module_fp == NULL) {
	sprintf(message, "Can't open module file '%s' at %%C for reading: %s", 
		filename, strerror(errno));
	return 1;
    }

    iomode = IO_INPUT;
    module_line = 1;
    module_column = 1;

    /* Skip the first two lines of the module */

    line = 0;
    while(line < 2) {
	c = module_char();
	if (c == EOF)
	    bad_module("Unexpected end of module");

	if (c == '\n')
	    line++;
    }

    /* Get the version */

    if (fscanf(module_fp, "module-version %d", &version) != 1)
	version = 0;

    if (version != COMPAT_VERSION)
	g95_fatal_error("While reading module '%s' found module version %d, "
			"expected %d.", module_name, version, COMPAT_VERSION);

    do
	c = fgetc(module_fp);
    while(c != '\n' && c != EOF);

    /* Make sure we're not reading the same module that we may be building */

    for(p=g95_state_stack; p; p=p->previous)
	if (p->state == COMP_MODULE && strcmp(p->sym->name, module_name) == 0)
	    g95_fatal_error("Can't USE the same module we're building!");

    init_pi_tree();
    init_true_name_tree();

    read_module();

    free_true_name(true_name_root);
    true_name_root = NULL;

    free_pi_tree(pi_root);
    pi_root = NULL;

    fclose(module_fp);

    return 0;
}



/* check_renames()-- Make sure all elements of the rename-list were
 * found in the module */

static void check_renames(void) {
g95_use_rename *u;

    for(u=rename_list; u; u=u->next) {
	if (u->found)
	    continue;

	if (u->operator == -1) {
	    g95_error("Symbol '%s' referenced at %L not found in module '%s'",
		      u->use_name, &u->where, module_name);
	    continue;
	}

	if (u->operator == INTRINSIC_USER) {
	    g95_error("User operator '%s' referenced at %L not found in "
		      "module '%s'", u->use_name, &u->where, module_name);
	    continue;
	}

	g95_error("Intrinsic operator '%s' referenced at %L not found in "
		  "module '%s'", g95_op2string(u->operator), &u->where,
		  module_name);
    }
}



void g95_use_module(void) {
char message[1000];
g95_use_rename *u;
g95_symbol *sym;
char *q;

    switch(mod_kind) {
    case MOD_NONINTRINSIC:
	if (use_external(message))
	    g95_fatal_error(message);

	break;

    case MOD_UNKNOWN:
	if (use_external(message) == 0)
	    break;

	if (g95_use_internal(module_name) == 0)
	    break;

	g95_fatal_error(message);  /* Complain about the external module */

    case MOD_INTRINSIC:
	if (g95_use_internal(module_name))
	    g95_fatal_error("Internal module '%s' at %C not found",
			    module_name);
	break;

    default:
	g95_internal_error("g95_use_module(): Bad module kind");
    }

    check_renames();

    for(u=rename_list; u; u=u->next) {
	q = u->local_name[0] != '\0'
	    ? u->local_name
	    : u->use_name;

	g95_find_symbol(q, NULL, 0, &sym);
	if (sym != NULL)
	    sym->attr.only = 1;
    }
}



void g95_module_done_2(void) {

    free_rename();
}


void g95_module_init_2(void) {

    last_atom = ATOM_LPAREN;
}
