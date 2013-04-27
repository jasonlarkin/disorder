/* G95 header file
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

/* g95.h-- It's probably insane to have this large of a header file,
 * but it seemed like everything had to be recompiled anyway when a
 * change was made to a header file, and there were ordering issues
 * with multiple header files.  Besides, Microsoft's winnt.h was 250k
 * last time I looked, so by comparison this is perfectly
 * reasonable. */


#ifndef IN_GCC         /* Defined only if included by backend code. */
typedef int bool;
#endif


/* The following ifdefs are recommended by the autoconf documentation
 * for any code using alloca */

/* AIX requires this to be the first thing in the file. */
#ifdef __GNUC__
#else /* not __GNUC__ */
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else /* do not HAVE_ALLOCA_H */
#ifdef _AIX
 #pragma alloca
#else
#ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#endif /* not predefined */
#endif /* not _AIX */
#endif /* do not HAVE_ALLOCA_H */
#endif /* not __GNUC__ */


#undef PACKAGE
#include "g95-config.h"

#include <stdio.h> /* need FILE * here */

/* Major control parameters */

#define G95_MAX_SYMBOL_LEN   63
#define G95_MAX_DIMENSIONS    7  /* Maximum dimensions in an array */
#define MAX_ERROR_MESSAGE  9000  /* Maximum length of an error message */
#define G95_LETTERS          26  /* Number of letters in the alphabet */

#define PREFIX    "_g95_"
#define G95_MAIN  "MAIN_"

#define BLANK_BLOCK_DATA_NAME PREFIX "blank_block_data"


#define free(x) Use_g95_free_instead_of_free()
#define g95_is_whitespace(c) ((c==' ') || (c=='\t'))

#ifndef NULL
#define NULL ((void *) 0)
#endif

#ifndef GCC_TREE_H
typedef void *tree; /* Just a dummy place holder. */
#endif

/* Stringization */

#define stringize(x) expand_macro(x)
#define expand_macro(x) # x

/* Macro to initialize an mstring structure */

#define minit(s, t) { s, NULL, t }


/*************************** Enums *****************************/

/* The author remains confused to this day about the convention of
 * returning '0' for 'SUCCESS'... or was it the other way around?  The
 * following enum makes things much more readable.  We also start
 * values off at one instead of zero. */

typedef enum { SUCCESS=1, FAILURE } try;

/* Matchers return one of these three values.  The difference between
 * MATCH_NO and MATCH_ERROR is that MATCH_ERROR means that a match was
 * successful, but that something non-syntactic is wrong and an error
 * has already been issued. */

typedef enum { MATCH_NO=1, MATCH_YES, MATCH_ERROR } match;

typedef enum { FORM_FREE, FORM_FIXED, FORM_UNKNOWN } g95_source_form;

typedef enum { BT_UNKNOWN=1, BT_INTEGER, BT_REAL, BT_COMPLEX,
	       BT_LOGICAL, BT_CHARACTER, BT_DERIVED, BT_PROCEDURE
} bt;

typedef enum { END_LITTLE, END_BIG } g95_endian;

typedef enum {
    ITYPE_NONE=0, ITYPE_C_PTR, ITYPE_C_FUNPTR, ITYPE_IEEE_CLASS,
    ITYPE_IEEE_FLAG, ITYPE_IEEE_STATUS, ITYPE_IEEE_ROUND, ITYPE_IEEE_FEATURES,
    ITYPE_MAX
} internal_type;


typedef enum {
    IVALUE_NONE=0, IVALUE_C_NULL_PTR, IVALUE_C_NULL_FUNPTR, IVALUE_OVERFLOW,
    IVALUE_DIVIDE_BY_ZERO, IVALUE_INVALID, IVALUE_UNDERFLOW, IVALUE_INEXACT,
    IVALUE_SIGNALING_NAN, IVALUE_QUIET_NAN, IVALUE_NEGATIVE_INF,
    IVALUE_NEGATIVE_DENORMAL, IVALUE_NEGATIVE_ZERO, IVALUE_NEGATIVE_NORMAL,
    IVALUE_POSITIVE_INF, IVALUE_POSITIVE_DENORMAL, IVALUE_POSITIVE_ZERO,
    IVALUE_POSITIVE_NORMAL, IVALUE_OTHER_VALUE, IVALUE_ROUND_NEAREST,
    IVALUE_ROUND_TO_ZERO, IVALUE_ROUND_UP, IVALUE_ROUND_DOWN, IVALUES_DATATYPE,
    IVALUES_DENORMAL, IVALUES_DIVIDE, IVALUES_HALTING, IVALUES_INEXACT_FLAG,
    IVALUES_INF, IVALUES_INVALID_FLAG, IVALUES_NAN, IVALUES_ROUNDING,
    IVALUES_SQRT, IVALUES_UNDERFLOW_FLAG
} internal_value;


/* real number classes, must match declarations in runtime lib. */

typedef enum {
    CLASS_OTHER_VALUE=0,
    CLASS_SIGNALING_NAN=1,  CLASS_QUIET_NAN=2,

    CLASS_NEGATIVE_INF=3,   CLASS_NEGATIVE_DENORMAL=4,
    CLASS_NEGATIVE_ZERO=5,  CLASS_NEGATIVE_NORMAL=6,

    CLASS_POSITIVE_INF=7,   CLASS_POSITIVE_DENORMAL=8,
    CLASS_POSITIVE_ZERO=9,  CLASS_POSITIVE_NORMAL=10,
} ieee_class;



typedef enum {
    IPROC_NONE=0, IPROC_C_LOC, IPROC_C_FUNLOC, IPROC_C_ASSOCIATED,
    IPROC_C_F_POINTER, IPROC_C_F_PROCPOINTER, IPROC_IEEE_SUPPORT_FLAG,
    IPROC_IEEE_SUPPORT_HALTING, IPROC_IEEE_GET_FLAG,
    IPROC_IEEE_GET_HALTING_MODE, IPROC_IEEE_SET_FLAG,
    IPROC_IEEE_SET_HALTING_MODE, IPROC_IEEE_GET_STATUS,
    IPROC_IEEE_SET_STATUS, IPROC_IEEE_SUPPORT_DATATYPE,
    IPROC_IEEE_SUPPORT_DENORMAL, IPROC_IEEE_SUPPORT_DIVIDE,
    IPROC_IEEE_SUPPORT_INF, IPROC_IEEE_SUPPORT_NAN,
    IPROC_IEEE_SUPPORT_ROUNDING, IPROC_IEEE_SUPPORT_SQRT,
    IPROC_IEEE_SUPPORT_STANDARD, IPROC_IEEE_SUPPORT_UNDERFLOW_CONTROL,
    IPROC_IEEE_CLASS, IPROC_IEEE_COPY_SIGN, IPROC_IEEE_IS_FINITE,
    IPROC_IEEE_IS_NAN, IPROC_IEEE_IS_NEGATIVE, IPROC_IEEE_IS_NORMAL,
    IPROC_IEEE_LOGB, IPROC_IEEE_NEXT_AFTER, IPROC_IEEE_REM, IPROC_IEEE_RINT,
    IPROC_IEEE_SCALB, IPROC_IEEE_UNORDERED, IPROC_IEEE_VALUE,
    IPROC_IEEE_GET_ROUNDING_MODE, IPROC_IEEE_GET_UNDERFLOW_MODE,
    IPROC_IEEE_SET_ROUNDING_MODE, IPROC_IEEE_SET_UNDERFLOW_MODE,
    IPROC_IEEE_SELECTED_REAL_KIND, IPROC_LAST
} internal_proc;


/* Array types */

typedef enum { AS_EXPLICIT=1, AS_ASSUMED_SHAPE, AS_DEFERRED,
	       AS_ASSUMED_SIZE, AS_UNKNOWN
} array_type;

typedef enum { AR_FULL=1, AR_ELEMENT, AR_SECTION, AR_UNKNOWN } ar_type;

typedef enum { M_READ, M_WRITE, M_PRINT, M_INQUIRE } io_kind;

/* Expression node types */

typedef enum { EXPR_UNKNOWN=1, EXPR_OP, EXPR_FUNCTION, EXPR_CONSTANT,
	       EXPR_VARIABLE, EXPR_SUBSTRING, EXPR_STRUCTURE, EXPR_ARRAY,
	       EXPR_PROCEDURE, EXPR_NULL
} expr_t;

/* Statement label types */

typedef enum { ST_LABEL_UNKNOWN=1, ST_LABEL_TARGET, ST_LABEL_BAD_TARGET,
	       ST_LABEL_BAD_TARGET2, ST_LABEL_FORMAT, ST_LABEL_REFFED
} g95_sl_type;

/* Comparison results */

typedef enum { CMP_LT, CMP_EQ, CMP_GT, CMP_UNKNOWN } comparison;

/* Intrinsic operators */

typedef enum { INTRINSIC_NONE=-1, INTRINSIC_UPLUS=0, INTRINSIC_UMINUS,
	       INTRINSIC_PLUS, INTRINSIC_MINUS, INTRINSIC_TIMES,
	       INTRINSIC_DIVIDE, INTRINSIC_POWER, INTRINSIC_CONCAT,
	       INTRINSIC_AND, INTRINSIC_OR, INTRINSIC_EQV, INTRINSIC_NEQV,
	       INTRINSIC_EQ, INTRINSIC_NE, INTRINSIC_GT, INTRINSIC_GE,
	       INTRINSIC_LT, INTRINSIC_LE, INTRINSIC_NOT, INTRINSIC_PAREN,
	       INTRINSIC_USER, INTRINSIC_ASSIGN
} g95_intrinsic_op;

/* Intent types */

typedef enum { INTENT_UNKNOWN=0, INTENT_IN, INTENT_OUT, INTENT_INOUT
} g95_intent;

/* Access types */

typedef enum { ACCESS_PUBLIC=1, ACCESS_PRIVATE, ACCESS_UNKNOWN
} g95_access;




/* This macro is the number of intrinsic operators that exist.
 * Assumptions are made about the numbering of the interface_op enums. */

#define G95_INTRINSIC_OPS (INTRINSIC_ASSIGN+1)


/* Enum for what the compiler is currently doing */

typedef enum {
    COMP_NONE, COMP_PROGRAM, COMP_MODULE, COMP_SUBROUTINE, COMP_FUNCTION,
    COMP_BLOCK_DATA, COMP_INTERFACE, COMP_DERIVED, COMP_IF, COMP_DO, COMP_ENUM,
    COMP_SELECT, COMP_FORALL, COMP_WHERE, COMP_CONTAINS, COMP_CRITICAL
} g95_compile_state;


typedef enum {
    SIMP_REGULAR, SIMP_INIT, SIMP_SPEC
} simp_t;


enum {
    FORMAL_FUNCTION=1, FORMAL_ST_FUNCTION, FORMAL_SUBROUTINE
};


/* Types of interfaces that we can have.  Assignment interfaces are
 * considered to be intrinsic operators */

typedef enum {
    INTERFACE_NAMELESS=1,    INTERFACE_GENERIC,
    INTERFACE_INTRINSIC_OP,  INTERFACE_USER_OP,   INTERFACE_ABSTRACT
} interface_type;


/* Statements */

typedef enum {
    ST_ARITHMETIC_IF, ST_ALLOCATE, ST_ATTR_DECL, ST_BACKSPACE, ST_BLOCK_DATA,
    ST_CALL, ST_CASE, ST_CLOSE, ST_COMMON, ST_CONTINUE, ST_CONTAINS, ST_CYCLE,
    ST_DATA, ST_DATA_DECL, ST_DEALLOCATE, ST_DO, ST_ELSE, ST_ELSEIF,
    ST_ELSEWHERE, ST_END_BLOCK_DATA, ST_END_CRITICAL, ST_ENDDO,
    ST_IMPLIED_ENDDO, ST_END_ENUM, ST_END_FILE, ST_END_FORALL, ST_END_FUNCTION,
    ST_ENDIF, ST_END_INTERFACE, ST_END_MODULE, ST_END_PROGRAM, ST_END_SELECT,
    ST_END_SUBROUTINE, ST_END_WHERE, ST_END_TYPE, ST_ENUM, ST_ENUMERATOR,
    ST_ENTRY, ST_EQUIVALENCE, ST_EXIT, ST_FORALL, ST_FORALL_BLOCK, ST_FORMAT,
    ST_FLUSH, ST_FUNCTION, ST_GOTO, ST_IF_BLOCK, ST_IMPLICIT, ST_IMPORT,
    ST_IMPLICIT_NONE, ST_INQUIRE, ST_INTERFACE, ST_PARAMETER, ST_MODULE,
    ST_MODULE_PROC, ST_NAMELIST, ST_NULLIFY, ST_OPEN, ST_PAUSE, ST_PRIVATE,
    ST_PROGRAM, ST_PUBLIC, ST_READ, ST_RETURN, ST_REWIND, ST_STOP,
    ST_SUBROUTINE, ST_TYPE, ST_USE, ST_WAIT, ST_WHERE, ST_WHERE_BLOCK,
    ST_WRITE, ST_ASSIGNMENT, ST_POINTER_ASSIGNMENT, ST_SELECT_CASE,
    ST_SEQUENCE, ST_SIMPLE_IF, ST_STATEMENT_FUNCTION, ST_DERIVED_DECL,
    ST_ASSIGN, ST_SYNC_ALL, ST_SYNC_IMAGES, ST_SYNC_TEAM, ST_SYNC_MEMORY,
    ST_CRITICAL, ST_ERROR_STOP, ST_NOTIFY, ST_QUERY, ST_NONE
} g95_statement;


/* Symbol flavors: these are all mutually exclusive.  */

typedef enum {
    FL_UNKNOWN=0, FL_PROGRAM, FL_BLOCK_DATA, FL_MODULE, FL_VARIABLE,
    FL_PARAMETER, FL_LABEL, FL_PROCEDURE, FL_DERIVED, FL_NAMELIST
} sym_flavor;   /* 10 elements = 4 bits */


typedef enum {
    CB_NONE=0, CB_REFERENCE, CB_VALUE
} call_by;


/* Procedure types */

typedef enum {
    PROC_UNKNOWN, PROC_MODULE, PROC_INTERNAL, PROC_DUMMY,
    PROC_INTRINSIC, PROC_ST_FUNCTION, PROC_EXTERNAL
} procedure_type;   /* 7 elements = 3 bits */


typedef enum {
    IFSRC_UNKNOWN=0, IFSRC_DECL, IFSRC_IFBODY, IFSRC_USAGE
} ifsrc;   /* 4 elements = 2 bits */


/* Executable statements that fill g95_code structures */

typedef enum {
    EXEC_NOP=1, EXEC_ASSIGN, EXEC_POINTER_ASSIGN, EXEC_GOTO, EXEC_CALL,
    EXEC_AC_START, EXEC_AC_ASSIGN, EXEC_WHERE_ASSIGN,
    EXEC_PAUSE, EXEC_RETURN, EXEC_STOP, EXEC_CONTINUE, EXEC_ENTRY, EXEC_IF,
    EXEC_ARITHMETIC_IF, EXEC_DO, EXEC_DO_WHILE, EXEC_SELECT,
    EXEC_FORALL, EXEC_WHERE,
    EXEC_CYCLE, EXEC_EXIT, EXEC_LABEL_ASSIGN,
    EXEC_ALLOCATE, EXEC_DEALLOCATE,
    EXEC_OPEN, EXEC_CLOSE, EXEC_FLUSH, EXEC_WAIT,
    EXEC_READ, EXEC_WRITE, EXEC_DT_END, EXEC_IOLENGTH, EXEC_TRANSFER,
    EXEC_BACKSPACE, EXEC_ENDFILE, EXEC_INQUIRE, EXEC_REWIND,
    EXEC_SYNC_ALL, EXEC_SYNC_TEAM, EXEC_SYNC_IMAGES, EXEC_SYNC_MEMORY,
    EXEC_NOTIFY, EXEC_QUERY, EXEC_CRITICAL, EXEC_ERROR_STOP
} g95_exec_op;


/* Enumeration of all the generic intrinsic functions.  Used for
 * identification of a function. */

typedef enum {
  /* G95_ISYM_NONE is used for an unknown intrinsic */

    G95_ISYM_NONE=0, G95_ISYM_ABS, G95_ISYM_ACHAR, G95_ISYM_ACOS,
    G95_ISYM_ADJUSTL, G95_ISYM_ADJUSTR, G95_ISYM_AIMAG, G95_ISYM_AINT,
    G95_ISYM_ANINT, G95_ISYM_ALL, G95_ISYM_ALLOCATED, G95_ISYM_ANINIT,
    G95_ISYM_ANY, G95_ISYM_ASIN, G95_ISYM_ASSOCIATED, G95_ISYM_ATAN,
    G95_ISYM_ATAN2, G95_ISYM_BIT_SIZE, G95_ISYM_BTEST, G95_ISYM_CEILING,
    G95_ISYM_CHAR, G95_ISYM_CMPLX, G95_ISYM_CONJG, G95_ISYM_COS, G95_ISYM_COSH,
    G95_ISYM_COUNT, G95_ISYM_CPU_TIME, G95_ISYM_CSHIFT,
    G95_ISYM_DATE_AND_TIME, G95_ISYM_DBLE, G95_ISYM_DIGITS, G95_ISYM_DIM,
    G95_ISYM_DOT_PRODUCT, G95_ISYM_DPROD, G95_ISYM_EOSHIFT, G95_ISYM_EPSILON,
    G95_ISYM_EXP, G95_ISYM_EXPONENT, G95_ISYM_FLOOR, G95_ISYM_FRACTION,
    G95_ISYM_HUGE, G95_ISYM_IACHAR, G95_ISYM_IAND, G95_ISYM_IBCLR,
    G95_ISYM_IBITS, G95_ISYM_IBSET, G95_ISYM_ICHAR, G95_ISYM_IEOR,
    G95_ISYM_INDEX, G95_ISYM_INT, G95_ISYM_IOR, G95_ISYM_ISHFT,
    G95_ISYM_ISHFTC, G95_ISYM_KIND, G95_ISYM_LBOUND, G95_ISYM_LEN,
    G95_ISYM_LEN_TRIM, G95_ISYM_LGE, G95_ISYM_LGT, G95_ISYM_LLE, G95_ISYM_LLT,
    G95_ISYM_LOC, G95_ISYM_LOG, G95_ISYM_LOG10, G95_ISYM_LOGICAL,
    G95_ISYM_MATMUL, G95_ISYM_MAX, G95_ISYM_MAXEXPONENT, G95_ISYM_MAXLOC,
    G95_ISYM_MAXVAL, G95_ISYM_MERGE, G95_ISYM_MIN, G95_ISYM_MINEXPONENT,
    G95_ISYM_MINLOC, G95_ISYM_MINVAL, G95_ISYM_MOD, G95_ISYM_MODULO,
    G95_ISYM_MOVE_ALLOC, G95_ISYM_MVBITS, G95_ISYM_NEAREST, G95_ISYM_NINT,
    G95_ISYM_NOT, G95_ISYM_NULL, G95_ISYM_PACK, G95_ISYM_PRECISION,
    G95_ISYM_PRESENT, G95_ISYM_PRODUCT, G95_ISYM_RADIX, G95_ISYM_RANDOM_NUMBER,
    G95_ISYM_RANDOM_SEED, G95_ISYM_RANGE, G95_ISYM_REAL, G95_ISYM_REPEAT,
    G95_ISYM_RESHAPE, G95_ISYM_RRSPACING, G95_ISYM_SCALE, G95_ISYM_SCAN,
    G95_ISYM_SELECTED_INT_KIND, G95_ISYM_SELECTED_REAL_KIND,
    G95_ISYM_SET_EXPONENT, G95_ISYM_SHAPE, G95_ISYM_SIGN, G95_ISYM_SIN,
    G95_ISYM_SINH, G95_ISYM_SIZE, G95_ISYM_SIZEOF, G95_ISYM_SPACING,
    G95_ISYM_SPREAD, G95_ISYM_SQRT, G95_ISYM_SUM, G95_ISYM_SYSTEM_CLOCK,
    G95_ISYM_TAN, G95_ISYM_TANH, G95_ISYM_TINY, G95_ISYM_TRANSFER,
    G95_ISYM_TRANSPOSE, G95_ISYM_TRIM, G95_ISYM_UBOUND, G95_ISYM_UNPACK,
    G95_ISYM_VERIFY, G95_ISYM_CONVERSION, G95_ISYM_EXTENSION
} g95_isym_id;



/* Classes of floating-point numbers */

typedef enum {
    FF_ZERO, FF_NORMAL, FF_DENORMAL, FF_INFINITY, FF_NAN
} ff_class;


/************************* Structures *****************************/

/* Symbol attribute structure. */

typedef struct {  /* Variable attributes */
    unsigned allocatable:1, dimension:1,  external:1, intrinsic:1, volatile_:1,
	     optional:1,    pointer:1,    save:1,     target:1,    dummy:1,
             result_var:1,  entry:1,      value:1,    async:1,     procedure:1;

    unsigned data:1,          /* Symbol is named in a DATA statement */
	     use_assoc:1,     /* Symbol has been use-associated */
             equivalenced:1;  /* symbol appears in an EQUIVALENCE statement */

    unsigned contained:1, by_value:1, desc:1, noinit:1, only:1, protected:1;
    unsigned implicit_type:1, untyped:1;  /* Type defined via implicit rules */
    unsigned used:1, set:1, alloced:1, dealloced:1, st_construct:1, hidden:1;
    unsigned st_construct0:1, artificial:1, current:1, restore:1, no_restore:1;
    unsigned targetted:1, aliasable:1, st_dummy:1, abstract:1, invoked:1;

    unsigned seen_formal:1, used_formal:1;
    unsigned in_namelist:1, in_common:1, entry_result:1;
    unsigned function:1, subroutine:1, dummy_proc:1;

/* Function/subroutine attributes */

    unsigned sequence:1, elemental:1, pure:1, recursive:1, bind:1;
    unsigned resolved:1, modproc:1;

/* Mutually exclusive multibit attributes */

    procedure_type proc:3;
    internal_type itype:4;
    internal_proc iproc:6;
    sym_flavor flavor:4;
    g95_access access:2;
    ifsrc if_source:2;
    g95_intent intent:2;

} symbol_attribute;


typedef struct g95_linebuf {
    int linenum;
    struct g95_file *file;
    char *name;
    struct g95_linebuf *next;

    char line[1];  /* Expands */
} g95_linebuf;


typedef struct g95_depfile {
    char *name;
    struct g95_depfile *next;
} g95_depfile;


typedef struct {
    char *nextc;
    g95_linebuf *lb;
    int column;
} g95_locus;


typedef struct g95_file {
    struct g95_file *included_by, *next, *up;
    int inclusion_line, line, eof_flag;
    char *filename, *save;
} g95_file;


#include <limits.h>
#include <sys/param.h>

#ifndef PATH_MAX
#define PATH_MAX MAXPATHLEN
#endif


/* Structure for storing strings to be matched by g95_match_string */

typedef struct {
    char *string, *mp;
    int tag;
} mstring;


typedef struct {
    int flag;
    char message[MAX_ERROR_MESSAGE];
} g95_error_buf;

extern int g95_suppress_error;


/* Character length structures hold the expression that gives the
 * length of a character variable.  We avoid putting these into
 * g95_typespec because doing so prevents us from doing structure
 * copies and forces us to deallocate any typespecs we create, as well
 * as structures that contain typespecs.  They also can have multiple
 * character typespecs pointing to them.
 *
 * These structures form a singly linked list within the current
 * namespace and are deallocated with the namespace.  It is possible to
 * end up with g95_charlen structures that have nothing pointing to them. */

typedef struct g95_charlen {
    struct g95_expr *length;
    struct g95_charlen *next;
    int resolved;
    tree backend_decl, save;
} g95_charlen;

/* Type specification structure */

typedef struct {
    bt type;
    int kind;
    struct g95_symbol *derived, *interface;
    g95_charlen *cl;      /* For character types */
} g95_typespec;

/* Array specification */

typedef struct {
    int rank;          /* A rank of zero means that a variable is a scalar */
    array_type type;

    struct g95_expr *lower[G95_MAX_DIMENSIONS], *upper[G95_MAX_DIMENSIONS];
} g95_array_spec;

#define g95_get_array_spec() g95_getmem(sizeof(g95_array_spec))


/* Coarray specification */

typedef struct {
    int corank, artificial;
    enum { CAS_ASSUMED, CAS_DEFERRED } type;

    struct g95_expr *lower[G95_MAX_DIMENSIONS], *upper[G95_MAX_DIMENSIONS];
} g95_coarray_spec;

#define g95_get_coarray_spec() g95_getmem(sizeof(g95_coarray_spec))



/* The g95_actual_arglist structure is for actual arguments */

typedef struct g95_actual_arglist {
    char *name;

    enum {
	ARG_EXPR, ARG_ALT_RETURN, ARG_ARRAY, ARG_ARRAY_ELEMENT,
	ARG_ARRAY_DESC
    } type;

    call_by cb;
    int pointer, dealloc;
    g95_intent intent;
    g95_locus start, end;  /* start is inclusive, end is exclusive */

    union {
	struct g95_st_label *label;
	struct g95_expr *expr;
    } u;

    bt missing_arg_type;

    struct g95_actual_arglist *next;
} g95_actual_arglist;



/* Formal argument lists are lists of symbols.  */

typedef struct g95_formal_arglist {
    struct g95_symbol *sym;
    g95_locus where;
    call_by cb;
    struct g95_formal_arglist *next;
} g95_formal_arglist;

#define g95_get_formal_arglist() g95_getmem(sizeof(g95_formal_arglist))


/* Components of derived types */

typedef struct g95_component {
    char *name;
    g95_typespec ts;

    g95_access access;
    tree backend_decl;
    g95_locus where;
    struct g95_expr *initializer;

    int pointer, dimension, allocatable, arg, nopass;
    g95_coarray_spec *cas;
    g95_array_spec *as;

    struct g95_component *next;
} g95_component;

#define g95_get_component() g95_getmem(sizeof(g95_component))


/* Because a symbol can belong to multiple namelists, they must be
 * linked externally to the symbol itself. */

typedef struct g95_namelist {
    struct g95_symbol *sym;
    char *name;
    g95_locus where;
    struct g95_namelist *next;
} g95_namelist;

#define g95_get_namelist() g95_getmem(sizeof(g95_namelist))


/* g95_interface()-- Interfaces are lists of symbols strung together */

typedef struct g95_interface {
    struct g95_symbol *sym;
    g95_locus where;
    struct g95_interface *next;
} g95_interface;

#define g95_get_interface() g95_getmem(sizeof(g95_interface))

typedef struct g95_annot {
    enum { ANNOT_PARAMETER=1, ANNOT_DERIVED, ANNOT_LABEL,
	   ANNOT_OPERATOR } type;

    union {
	char *name;
	struct g95_symbol *sym;
    } u;

    g95_locus where;
    struct g95_annot *next;

} g95_annot;


/* The g95_st_label structure is a doubly linked list attached to a
 * namespace that records the usage of statement labels within that space */

typedef struct g95_st_label {
    int value;

    g95_sl_type defined, referenced;

    struct g95_expr *format;
    int length;
    g95_locus where;

    tree backend_decl;

    struct g95_st_label *prev, *next;
} g95_st_label;


/* User operator nodes.  These are like stripped down symbols */

typedef struct {
    char *name;

    g95_interface *operator;
    struct g95_namespace *ns;
    g95_access access;
} g95_user_op;


/* Symbol nodes.  These are important things.  They are what the
 * standard refers to as "entities".  The possibly multiple names that
 * refer to the same entity are accomplished by a binary tree of
 * symtree structures that is balanced by the red-black method-- more
 * than one symtree node can point to any given symbol. */

typedef struct g95_symbol {
    char *name,     /* Primary name, before renaming */
	 *module;   /* Module this symbol came from */
    g95_locus declared_at;

    g95_typespec ts;
    symbol_attribute attr;

/* the interface member points to the formal argument list if the
 * symbol is a function or subroutine name. */

    g95_access component_access;

    g95_formal_arglist *formal;
    struct g95_namespace *formal_ns;

    struct g95_symbol *result;        /* function result symbol */
    g95_component *components;        /* Derived type components */
    struct g95_symbol *extends;       /* Derived type extension */

    struct g95_expr *value;           /* Parameter/Initializer value */
    g95_array_spec *as;
    g95_coarray_spec *cas;

    struct g95_symbol *common_next;   /* Links for COMMON syms */
    char *bind;

    g95_namelist *namelist, *namelist_tail;

/* Change management fields.  Symbols that might be modified by the
 * current statement have the mark member nonzero and are kept in a
 * singly linked list through the tlink field.  Of these symbols,
 * symbols with old_symbol equal to NULL are symbols created within
 * the current statement.  Otherwise, old_symbol points to a copy of
 * the old symbol. */

    struct g95_symbol *old_symbol, *tlink;
    unsigned mark:1, new:1;
    int refs;
    struct g95_namespace *ns;    /* namespace containing this symbol */

    tree save, backend_decl;

} g95_symbol;


typedef struct common_head {
    g95_locus where;
    int saved, use_assoc;
    char *bind;

    g95_symbol *head;
    struct common_head *next;
} g95_common_head;

#define g95_get_common_head() g95_getmem(sizeof(g95_common_head))


/* Within a namespace, symbols are pointed to by symtree nodes that
 * are linked together in a balanced binary tree.  There can be
 * several symtrees pointing to the same symbol node via USE
 * statements. */

#define BBT_HEADER(self) int priority; struct self *left, *right;

typedef struct g95_symtree {
    BBT_HEADER(g95_symtree)

    char *name;
    int ambiguous;
    union {
	g95_common_head *common;
	g95_interface *generic;
	g95_symbol *sym;             /* Symbol associated with this node */
	g95_user_op *uop;
    } n;

    g95_access access;
    struct g95_symtree *link;

} g95_symtree;


typedef struct g95_namespace {
    g95_symtree *sym_root, *uop_root, *common_root, *generic_root;

    int set_flag[G95_LETTERS];
    g95_typespec default_type[G95_LETTERS];    /* IMPLICIT typespecs */

    g95_interface *operator[G95_INTRINSIC_OPS];
    struct g95_symbol *proc_name, *itypes[ITYPE_MAX];
    struct g95_namespace *parent, *contained, *sibling;
    struct g95_code *code;
    struct g95_equiv *equiv;
    g95_common_head *blank_common;
    g95_access default_access, operator_access[G95_INTRINSIC_OPS];

    g95_charlen *cl_list, *cl0;
    g95_compile_state state;
    char *unit_name;
    int format_label, goto_label, seen_branch_target, seen_use,
	save_all, seen_save, interface, seen_implicit_none, import;

    unsigned int ieee_datatype:1, ieee_denormal:1, ieee_divide:1,
	ieee_halting:1, ieee_inf:1, ieee_nan:1, ieee_rounding:1,
	ieee_sqrt:1, ieee_inexact:1, ieee_invalid:1, ieee_underflow:1,
	ieee_class_eq:1, ieee_class_ne:1;

    g95_annot *annotation;
    g95_st_label *st_labels;
    struct g95_data *data;
    g95_locus declared_at, name_pos;

    tree backend_decl;
} g95_namespace;

extern g95_namespace *g95_current_ns;


/* Information on interfaces being built */

typedef struct {
    interface_type type;
    g95_namespace *ns;
    g95_user_op *uop;
    g95_symtree *generic;
    int op;
} g95_interface_info;

extern g95_interface_info current_interface;


/* Array reference */

typedef struct g95_array_ref {
    ar_type type;
    int dimen;                  /* # of components in the reference */

    struct g95_expr *start[G95_MAX_DIMENSIONS], *end[G95_MAX_DIMENSIONS],
	            *stride[G95_MAX_DIMENSIONS];

    enum { DIMEN_ELEMENT=1, DIMEN_RANGE, DIMEN_VECTOR, DIMEN_UNKNOWN }
    dimen_type[G95_MAX_DIMENSIONS];
    g95_locus where[G95_MAX_DIMENSIONS];     /* All expressions can be NULL */

} g95_array_ref;



typedef struct g95_coarray_ref {
    int dimen;
    struct g95_expr *element[G95_MAX_DIMENSIONS];

} g95_coarray_ref;




#define g95_get_array_ref() g95_getmem(sizeof(g95_array_ref))

/* Component reference nodes.  A variable is stored as an expression
 * node that points to the base symbol.  After that, a singly linked
 * list of component reference nodes gives the variable's complete
 * resolution.  The array_ref component may be present and comes
 * before the component component.  */

typedef struct g95_ref {
    enum { REF_ARRAY, REF_COMPONENT, REF_SUBSTRING, REF_COARRAY } type;

    union {
	struct g95_array_ref ar;
	struct g95_coarray_ref car;

	struct {
	    struct g95_expr *start, *end;       /* Substring */
	    g95_charlen *length;
	} ss;

	struct {
	    char *name;
	    g95_symbol *sym;
	    g95_component *component;
	    int alloc;
	} c;
    } u;

    g95_locus where;
    struct g95_ref *next;
} g95_ref;

#define g95_get_ref() g95_getmem(sizeof(g95_ref))


/* Stack element for the current compilation state.  These structures
 * are allocated as automatic variables.  */

typedef struct g95_state_data {
    g95_compile_state state;
    g95_symbol *sym;            /* Block name associated with this level */
    g95_symbol *do_variable;    /* For DO blocks, the iterator variable. */

    struct g95_code *top, *head, *tail, **next;
    struct g95_state_data *previous;

/* Block-specific state data. */

    union {
	g95_st_label *end_do_label;
	struct g95_forall_iterator *iter;
    } ext;
} g95_state_data;

extern g95_state_data *g95_state_stack;

#define g95_current_state() (g95_state_stack->state)
#define g95_current_block() (g95_state_stack->sym)


/* Structures representing intrinsic symbols and their arguments lists */

typedef struct g95_intrinsic_arg {
    char *name;

    g95_typespec ts;
    int optional, array_desc;
    g95_intent intent;
    g95_actual_arglist *actual;

    struct g95_intrinsic_arg *next;

} g95_intrinsic_arg;


typedef struct g95_intrinsic_sym {
    char *name, *lib_name, *actual;
    g95_intrinsic_arg *formal;
    g95_typespec ts;
    int elemental, pure, generic, alias, specific, hidden, len;

    struct g95_expr *(*simplify)();
    try (*check)();
    void (*resolve)();
    struct g95_intrinsic_sym *specific_head, *next;
    g95_isym_id id;

} g95_intrinsic_sym;


/* Structures for information associated with different kinds of
 * numbers.  The first set of integer parameters define all there is
 * to know about a particular kind.  The rest of the elements are
 * computed from the first elements.  */

typedef struct {
    int kind, bit_size;
} g95_logical_info;

extern g95_logical_info g95_logical_kinds[];


typedef struct {
    int kind, radix, digits, bit_size;

    int range;
    void *min, *max, *umax;  /* Values really representable by the target */
} g95_integer_info;

extern g95_integer_info g95_integer_kinds[];



typedef struct {
    int kind, radix;

    g95_endian endian;
    unsigned int totalsize;       /* Total size of number in bits */

    /* Sign bit is always one bit long.  1 means negative, 0 means positive. */
    unsigned int sign_start;
    unsigned int exp_start;
    unsigned int exp_len;
    /* Amount added to "true" exponent.  0x3fff for many IEEE extendeds.  */
    unsigned int exp_bias;

    /* Exponent value which indicates NaN.  This is the actual value
     * stored in the float, not adjusted by the exp_bias.  This
     * usually consists of all one bits.  */

    int exp_nan;
    unsigned int man_start;
    unsigned int man_len;

    /* Is the integer bit explicit or implicit?  */
    enum { MSB_EXPLICIT, MSB_IMPLICIT } msb;

    /* Derived quantities */

    int digits, min_exponent, max_exponent;
    int range, precision;

} g95_ff;

extern g95_ff g95_real_kinds[];



/* Copyright © 2000 by Jef Poskanzer <jef@acme.com>.
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions
** are met:
** 1. Redistributions of source code must retain the above copyright
**    notice, this list of conditions and the following disclaimer.
** 2. Redistributions in binary form must reproduce the above copyright
**    notice, this list of conditions and the following disclaimer in the
**    documentation and/or other materials provided with the distribution.
**
** THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
** FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
** DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
** OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
** HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
** LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
** OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
** SUCH DAMAGE.
*/

/* Type definition for bignum, originally from Jeff Poskanzer,
 * extended here. */

typedef long comp;

struct _bignum {
    int refs;

    struct _bignum *next;
    int num_comps, max_comps;
    int sign, serial, typeless;

    int real_sign, real_exp;
    g95_ff *ff;
    comp *comps;
} _bignum;

typedef struct _bignum * bignum;


/* Expression nodes.  The expression node types deserve explanations, since
 * the last couple can be easily misconstrued:
 *
 * EXPR_OP         Operator node pointing to one or two other nodes
 * EXPR_FUNCTION   Function call, symbol points to function's name
 * EXPR_CONSTANT   A scalar constant: Logical, String, Real, Int or Complex
 * EXPR_VARIABLE   An Lvalue with a root symbol and possible reference list
 *                 which expresses structure, array and substring refs.
 * EXPR_NULL       The NULL pointer value (which also has a basic type).
 * EXPR_SUBSTRING  A substring of a constant string
 * EXPR_STRUCTURE  A structure constructor
 * EXPR_ARRAY      An array constructor
 * EXPR_PROCEDURE  A procedure pointer target
 */

typedef struct g95_expr {
    expr_t type;
    int rank, serial;
    bignum *shape;

    g95_typespec ts;
    g95_locus where;
    g95_symbol *symbol;
    g95_ref *ref;

    union {
	bignum real;
	bignum integer;
	int logical;

	struct {
	    g95_intrinsic_op operator;
	    struct g95_expr *op1, *op2;
	    g95_user_op *uop;
	} op;

	struct {
	    int length, hollerith;
	    char *string;
	} character;

	struct {
	    bignum r, i;
	} complex;

	struct {
	    g95_actual_arglist *actual;
	    char *name;
	    char *iname;   /* Points to the ultimate name of the function */
	    struct g95_expr *pointer;
	    g95_intrinsic_sym *isym;
	    internal_proc iproc;
	} function;

	struct {
	    internal_value ivalue;
	    struct g95_constructor *c;
	} constructor;

    } value;

} g95_expr;



/* Shape containers. */

#define G95_SHAPES 1000

typedef struct g95_shapes {
    struct g95_shapes *next;
    bignum buffer[G95_SHAPES];
} g95_shapes;


/* Equivalence structures.  Equivalent lvalues are linked along the
 * 'eq' pointer, equivalence sets are strung along the 'next' node.  */

typedef struct g95_equiv {
    struct g95_equiv *next, *eq;
    g95_expr *expr;
    char *module;
    int common, used;
} g95_equiv;

#define g95_get_equiv() g95_getmem(sizeof(g95_equiv))

/* g95_case stores the selector list of a case statement.  The *low
 * and *high pointers can point to the same expression in the case of
 * a single value.  If *high is NULL, the selection is from *low
 * upwards, if *low is NULL the selection is *high downwards.  */

typedef struct g95_case {
    g95_expr *low, *high;

    g95_locus where;
    int n;

    struct g95_case *next, *cprev, *cnext;
} g95_case;

#define g95_get_case() g95_getmem(sizeof(g95_case))


typedef struct {
    g95_expr *var, *start, *end, *step;
} g95_iterator;

#define g95_get_iterator() g95_getmem(sizeof(g95_iterator))


typedef struct g95_forall_iterator {
    g95_expr *var, *start, *end, *stride;
    g95_symbol *save;
    struct g95_forall_iterator *next;
} g95_forall_iterator;


typedef struct {
    g95_expr *unit, *status, *iostat, *iomsg;
    g95_st_label *err;
} g95_close;


typedef struct {
    g95_expr *unit, *iostat, *iomsg;
    g95_st_label *err;
} g95_filepos;


typedef struct {
    g95_expr *io_unit, *format_expr, *rec, *advance, *iostat, *iomsg,
	*size, *ambig, *pos, *decimal;

    g95_symbol *namelist;

/* A format_label of `g95_format_asterisk' indicates the "*" format */
    g95_st_label *format_label;
    g95_st_label *err, *end, *eor;

    g95_locus err_where, eor_where, end_where, namelist_where, where;
    io_kind type;
    int default_unit, have_data_item;
} g95_dt;

#define g95_get_dt() g95_getmem(sizeof(g95_dt))


/* Allocation structure for ALLOCATE, DEALLOCATE and NULLIFY statements. */

typedef struct g95_alloc {
    g95_expr *expr;   /* Pointer or allocatable part */
    struct g95_alloc *next;

    int rank, corank;
    g95_expr *lower[2*G95_MAX_DIMENSIONS];
    g95_expr *upper[2*G95_MAX_DIMENSIONS];
} g95_alloc;


typedef struct {
    g95_expr *unit, *file, *iostat, *iomsg, *exist, *opened, *number, *named,
             *name, *access, *sequential, *direct, *form, *formatted,
             *unformatted, *recl, *nextrec, *blank, *position, *action, *read,
	     *write, *readwrite, *delim, *pad, *pos, *iolength, *size, *stream;

    g95_st_label *err;
    g95_locus where;
} g95_inquire;


typedef struct {
    g95_expr *stat, *errmsg, *ready;
} g95_sync_stat;


typedef struct {
    g95_expr *unit, *id, *iostat, *iomsg;
    g95_st_label *err, *end, *eor;
} g95_wait;


typedef struct {
    g95_expr *unit, *file, *status, *access, *form, *recl, *decimal, *iomsg,
	     *blank, *position, *action, *delim, *pad, *iostat, *convert;
    g95_st_label *err;
    g95_locus where;
} g95_open;


typedef struct {
    g95_expr *unit, *iostat, *iomsg;
    g95_st_label *err;
} g95_flush;


typedef struct g95_code {
    g95_exec_op type;

    struct g95_code *block, *next;
    g95_locus where;

    g95_st_label *here, *label, *label2, *label3;
    g95_symbol *sym;
    g95_expr *expr, *expr2;

    union {
	struct {
	    g95_intrinsic_sym *isym;   /* Subroutine references */
	    char *name, *sub_name;
	    g95_actual_arglist *actual;
	    g95_expr *pointer;
	} sub;

	g95_open *open;
	g95_close *close;
	g95_filepos *filepos;
	g95_sync_stat sync;
	g95_wait *wait;
	g95_case *case_list;
	g95_iterator *iterator;
	g95_inquire *inquire;
	g95_dt *dt;
	g95_forall_iterator *forall_iterator;
	g95_flush *flush;
	g95_alloc *alloc_list;
	struct g95_code *block;
	int stop_code, end_code;
	io_kind transfer;
	g95_expr **shape;
    } ext;     /* Points to additional structures required by statement */

    /* backend_decl is used for cycle and break labels in do loops. */
    tree backend_decl;
} g95_code;


/* Storage for DATA statements */

typedef struct g95_data_variable {
    g95_expr *expr;
    g95_iterator iter;
    struct g95_data_variable *list, *next;
} g95_data_variable;


typedef struct g95_data_value {
    int repeat;
    g95_expr *expr;

    struct g95_data_value *next;
} g95_data_value;


typedef struct g95_data {
    g95_data_variable *var;
    g95_data_value *value;
    g95_locus where;

    struct g95_data *next;
} g95_data;


#define g95_get_data_value() g95_getmem(sizeof(g95_data_value))
#define g95_get_data_variable() g95_getmem(sizeof(g95_data_variable))
#define g95_get_data() g95_getmem(sizeof(g95_data))

/* Structure for holding module and include file search path */

typedef struct g95_directorylist {
    char *path;
    struct g95_directorylist *next;
} g95_directorylist;


typedef struct g95_warning_list {
    int warning;
    struct g95_warning_list *next;
} g95_warning_list;




/* Structure for holding compile options */

typedef struct {
    int implicit_none, fixed_line_length, max_frame_size, no_underscoring,
	d_comment, case_upper, leading_underscore, cpp, preprocess_only,
	unused_module_vars, pedantic, bounds_check, no_backslash, fmode,
	unused_vars, unset_vars, unused_module_procs, default_integer, deps,
	obsolescent, endian, werror, short_circuit, module_access_private,
	dollar, static_var, symbol_len, verbose, intrinsic_extensions, tr15581,
	integer_init, integer_value, zero_init, unused_label, line_truncation,
	prec_loss, pack_derived, uninit, q_kind, quiet, r_value, l1, huge_line,
	no_second_underscore, sloppy_char, onetrip, real_loops,
	unused_parameter, unused_target, multiple_save, one_error,
	traditional, unused_types, missing_intent, globals,
	implicit_interface, unused_internal_procs, c_binding;

    enum { ROUND_NEAREST, ROUND_PLUS, ROUND_MINUS, ROUND_ZERO } round;

    enum { REAL_INIT_NONE=0, REAL_INIT_ZERO, REAL_INIT_NAN, REAL_INIT_PLUS_INF,
	   REAL_INIT_MINUS_INF } real_init;

    enum { LOGICAL_INIT_NONE=0, LOGICAL_INIT_TRUE,
	   LOGICAL_INIT_FALSE } logical_init;

    enum { TRACE_NONE, TRACE_FRAME, TRACE_FULL } trace;

    enum { POINTER_INIT_NONE=0, POINTER_INIT_NULL,
	   POINTER_INIT_INVALID } pointer_init;

    g95_source_form form;
    g95_directorylist *include_dirs;
    g95_warning_list *nowarn, *error_list;
    char *module_dir, *intrinsics;
} g95_option_t;

extern g95_option_t g95_option;

#define G95_STRICT_F95() (g95_option.fmode == 95 || g95_option.fmode == 96)
#define G95_STRICT_F() (g95_option.fmode == 96)


typedef struct g95_global_ref {
    g95_symbol *sym;
    g95_actual_arglist *actual;
    g95_locus where;

    struct g95_global_ref *next;
} g95_global_ref;

#define g95_get_global_ref() g95_getmem(sizeof(g95_global_ref))



typedef struct g95_gsymbol {
    BBT_HEADER(g95_gsymbol)

    char *name, *bind;
    enum { GSYM_UNKNOWN=1, GSYM_PROGRAM, GSYM_FUNCTION, GSYM_SUBROUTINE,
	   GSYM_MODULE, GSYM_COMMON, GSYM_BLOCK_DATA } type;

    int size, defined, used;
    g95_locus where;

    g95_symbol *global;
    g95_global_ref *ref;

    tree backend_decl;
} g95_gsymbol;



typedef struct g95_cbinding {
    BBT_HEADER(g95_cbinding)

    char *name;
    enum { CBIND_UNKNOWN=1, CBIND_VARIABLE, CBIND_PROCEDURE,
	   CBIND_COMMON } type;
    g95_locus where;

} g95_cbinding;



/* Constructor nodes for array and structure constructors. */

typedef struct g95_constructor {
    g95_expr *expr;
    g95_iterator *iterator;
    g95_locus where;
    struct g95_constructor *next;
} g95_constructor;

#define g95_get_constructor() g95_getmem(sizeof(g95_constructor))



/************************ Function prototypes *************************/

/* symbol.c */

extern g95_gsymbol *g95_gsym_root;

try g95_add_pure(symbol_attribute *, g95_locus *);
try g95_add_function(symbol_attribute *, char *, g95_locus *);
try g95_add_subroutine(symbol_attribute *, char *, g95_locus *);
try g95_add_intrinsic(symbol_attribute *, g95_locus *);
try g95_add_optional(symbol_attribute *, char *, g95_locus *);
try g95_add_dimension(symbol_attribute *, char *, g95_locus *);
try g95_add_dummy(symbol_attribute *, char *, g95_locus *);
try g95_add_bind(symbol_attribute *, char *, g95_locus *);
try g95_add_abstract(symbol_attribute *, char *, g95_locus *);
try g95_add_in_common(symbol_attribute *, char *, g95_locus *);
try g95_add_data(symbol_attribute *, char *, g95_locus *);
try g95_add_equivalence(symbol_attribute *, char *, g95_locus *);
try g95_add_sequence(symbol_attribute *, char *, g95_locus *);
try g95_add_pointer(symbol_attribute *, char *, g95_locus *);
try g95_add_result(symbol_attribute *, char *, g95_locus *);
try g95_add_save(symbol_attribute *, char *, g95_locus *);
try g95_add_target(symbol_attribute *, g95_locus *);
try g95_add_value(symbol_attribute *, g95_locus *);
try g95_add_volatile(symbol_attribute *, g95_locus *);
try g95_add_async(symbol_attribute *, g95_locus *);
try g95_add_elemental(symbol_attribute *, g95_locus *);
try g95_add_protected(symbol_attribute *, g95_locus *);
try g95_add_in_namelist(symbol_attribute *, char *, g95_locus *, int);
try g95_add_external(symbol_attribute *, g95_locus *);
try g95_add_allocatable(symbol_attribute *, g95_locus *);
try g95_add_recursive(symbol_attribute *, g95_locus *);

g95_typespec *g95_get_default_type(g95_symbol *, g95_namespace *);
try g95_set_default_type(g95_symbol *, int, g95_namespace *);
int g95_local_symbol(g95_symbol *);
int g95_iterator_variable(g95_symbol *);
try g95_check_assign(g95_expr *, g95_expr *);
try g95_check_pointer_assign(g95_expr *, g95_expr *);
match g95_match_implicit_none(void);
void g95_set_implicit_none(void);
match g95_match_implicit(void);
void g95_set_implicit(void);
g95_access g95_symbol_access(g95_symbol *);
g95_access g95_symtree_access(g95_symtree *);
void g95_clear_sym_mark(g95_symtree *);
char *g95_symbol_name(g95_symbol *);
try g95_check_assign_symbol(g95_symbol *, g95_expr *);

try g95_add_access(symbol_attribute *, g95_access, char *, g95_locus *);
try g95_add_flavor(symbol_attribute *, sym_flavor, char *, g95_locus *);
try g95_add_entry(symbol_attribute *, char *, g95_locus *);
try g95_add_procedure(symbol_attribute *, procedure_type, char *, g95_locus *);
try g95_add_intent(symbol_attribute *, g95_intent, g95_locus *);
try g95_add_explicit_interface(g95_symbol *, ifsrc, g95_formal_arglist *,
			       g95_locus *);
try g95_add_type(g95_symbol *, g95_typespec *, g95_locus *);

try g95_add_component(g95_symbol *, char *, g95_component **);
void g95_show_components(g95_symbol *);
g95_symbol *g95_use_derived(g95_symbol *);

int g95_get_ha_symbol(char *, g95_symbol **);
g95_namespace *g95_get_namespace(g95_namespace *, int);
int g95_compare_symtree(g95_symtree *, g95_symtree *);
g95_symtree *g95_new_symtree(g95_symtree **, char *);
g95_symtree *g95_find_symtree(g95_symtree *, char *);
g95_expr *g95_assign_boz(g95_typespec *, g95_expr *);
g95_user_op *g95_get_uop(char *);
g95_user_op *g95_find_uop(char *, g95_namespace *);
void g95_free_symbol(g95_symbol *);
g95_symbol *g95_new_symbol(char *, g95_namespace *);
int g95_find_symbol(char *, g95_namespace *, int, g95_symbol **);
int g95_get_symbol(char *, g95_namespace *, g95_symbol **);
int g95_module_symbol(g95_symbol *);
g95_access g96_symbol_access(g95_symbol *);
void g95_set_usage(g95_symbol *, g95_locus *, int, int);
void g95_implicit_types(g95_namespace *, int);

g95_st_label *g95_get_st_label(int);
void g95_free_st_label(g95_st_label *);
g95_st_label *g95_new_internal_label(void);
void g95_define_st_label(g95_st_label *, g95_sl_type, g95_locus *);
try g95_reference_st_label(g95_st_label *, g95_sl_type);

int g95_compare_attr(symbol_attribute *, symbol_attribute *);
void g95_clear_attr(symbol_attribute *);
try g95_copy_attr(symbol_attribute *, symbol_attribute *, g95_locus *);

void g95_global_used(g95_gsymbol *, g95_locus *);
void g95_traverse_symtree(g95_namespace *, void (*)(g95_symtree *));
void g95_traverse_ns(g95_namespace *, void (*)(g95_symbol *));
void g95_traverse_user_op(g95_namespace *, void (*)(g95_user_op *));
int g95_derived_private(g95_symbol *);
int g95_pointer_component(g95_symbol *);
int g95_allocatable_component(g95_symbol *);
int g95_initialized_component(g95_symbol *);
int g95_coarray_component(g95_symbol *);
int g95_pointer_array_component(g95_symbol *);
int g95_static_symbol(g95_symbol *);
void g95_symbol_state(void);
g95_symtree *g95_find_generic(char *, g95_namespace *);
g95_symtree *g95_get_generic(char *, g95_namespace *);
int g95_generic_name(char *);
g95_gsymbol *g95_get_gsymbol(char *);
g95_gsymbol *g95_find_gsymbol(char *);
void g95_free_gsymbol(g95_gsymbol *);

void g95_symbol_init_2(void);
void g95_save_symbol_data(g95_symbol *);
void g95_fixup_ac(g95_expr *);
void g95_fixup_data(g95_data_variable *);
void g95_undo_symbols(void);
void g95_commit_symbols(void);
void g95_free_namespace(g95_namespace *);
void g95_free_namespace0(g95_namespace *);
g95_gsymbol *g95_check_global(char *, int, g95_locus *);

/* decl.c */

extern g95_symbol *g95_new_block;

match g95_match_null(g95_expr **);
match g95_match_kind_spec(g95_typespec *);
match g95_match_type_spec(g95_typespec *, int);

match g95_match_end(g95_statement *);
match g95_match_data_decl(void);
match g95_match_procedure(void);
match g95_match_formal_arglist(g95_symbol *, int, int);
match g95_match_function(void);
match g95_match_entry(void);
match g95_match_subroutine(void);
match g95_match_derived_decl(void);

/* Matchers for attribute declarations */

match g95_match_volatile(void);
match g95_match_async(void);
match g95_match_allocatable(void);
match g95_match_save(void);
match g95_match_modproc(void);
match g95_match_target(void);
match g95_match_external(void);
match g95_match_intent(void);
match g95_match_bind(void);
match g95_match_intrinsic(void);
match g95_match_optional(void);
match g95_match_dimension(void);
match g95_match_codimension(void);
match g95_match_pointer(void);
match g95_match_protected(void);
match g95_match_private(g95_statement *);
match g95_match_public(g95_statement *);
match g95_match_parameter(void);
match g95_match_value(void);

/* intrinsic.c */

extern simp_t g95_simplify_mode;
extern g95_charlen g95_unity_charlen;

void g95_intrinsic_init_1(void);
void g95_intrinsic_done_1(void);
int g95_intrinsic_symbol(char *, int, g95_locus *);
int g95_is_transformational(g95_intrinsic_sym *);

char g95_type_letter(bt);
g95_intrinsic_sym *g95_find_function(char *);
g95_intrinsic_sym *g95_find_subroutine(char *);
int g95_find_id(char *);
int g95_has_alt_return(g95_actual_arglist *);
g95_expr *g95_convert_hollerith(g95_expr *, g95_typespec *);
match g95_intrinsic_func_interface(g95_expr *, int);
match g95_intrinsic_sub_interface(g95_code *, int);
try g95_convert_type(g95_expr *, g95_typespec *, int);
int g95_generic_intrinsic(char *, int);
int g95_specific_intrinsic(char *);
int g95_intrinsic_name(char *, int);
try g95_check_intrinsic_modfunc(g95_symbol *, g95_expr *);
try g95_check_intrinsic_modsub(g95_symbol *, g95_code *);

/* primary.c */

match g95_match_actual_arglist(int, int, g95_actual_arglist **);
match g95_match_structure_constructor(g95_symbol *, g95_expr **);
g95_ref *g95_extend_ref(g95_expr *, int, g95_locus *);
match g95_match_rvalue(g95_expr **);
match g95_match_variable(g95_expr **, int, int, int);
match g95_match_call_expr(g95_symbol *, g95_expr **);
int g95_next_string_char(char);
match g95_match_literal_constant(g95_expr **, int);
int g95_pointer_expr(g95_expr *);
int g95_proc_pointer_expr(g95_expr *);
int g95_target_expr(g95_expr *);
int g95_allocatable_expr(g95_expr *);
match g95_match_alloc_var(g95_alloc **);
match g95_match_dealloc_var(g95_alloc **);

extern g95_charlen g95_unknown_charlen;

/* parse.c */

try g95_find_state(g95_compile_state);
char *g95_state_name(g95_compile_state);
void g95_reject_statement(void);
int g95_check_do_variable(g95_symbol *, g95_locus *);
void g95_parse_file(void);
g95_state_data *g95_enclosing_unit(g95_compile_state *);
match g95_match_st_function(void);
char *g95_current_block_name(void);
char *g95_ascii_statement(g95_statement);

extern g95_st_label *g95_statement_label;
extern g95_locus g95_statement_locus;

/* error.c */

void g95_error_init_1(void);
void g95_buffer_error(int);

void g95_warning(int, char *, ...);
void g95_warning_now(int, char *, ...);
void g95_clear_warning(void);
void g95_warning_check(void);
void g95_flag_error(void);

void g95_error(char *, ...);
void g95_error_now(char *, ...);
void g95_fatal_error(char *, ...)
#ifdef __GNUC__
 __attribute__ ((noreturn))
#endif
;

void g95_internal_error(char *, ...)
#ifdef __GNUC__
 __attribute__ ((noreturn))
#endif
;

void g95_clear_error(void);
int g95_error_check(void);
void g95_syntax_error(g95_statement st);
void g95_push_error(g95_error_buf *);
void g95_pop_error(g95_error_buf *);

void g95_status(char *, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;
void g95_status_char(char);
try g95_open_status(char *);
try g95_close_status(void);

void g95_get_errors(int *, int *);

/* scanner.c */

void g95_scanner_done_1(void);
void g95_scanner_init_1(void);

g95_linebuf *g95_first_line(void);
int g95_at_end(void);
int g95_at_eof(void);
int g95_at_bol(void);
int g95_at_eol(void);
void g95_advance_line(void);

void g95_skip_comment_line(void);
void g95_skip_comments(void);
int g95_next_char_literal(int);
int g95_next_char(void);
int g95_peek_char(void);
void g95_error_recovery(void);
void g95_gobble_whitespace(void);
void g95_define_cpp_macro(char *, int);
try g95_new_file(void);

extern g95_file *g95_current_file;
extern g95_source_form g95_current_form;
extern char *g95_source_file, *g95_search_prefix;
extern g95_locus g95_current_locus, g95_def_locus, g95_label_locus;

/* iresolve.c */

char *g95_get_string(char *);
char *g95_get_fstring(char *, ...);

void g95_iresolve_init_1(void);
void g95_iresolve_done_1(void);

/* misc.c */

void *g95_getmem(size_t);
void g95_free(void *);
void g95_clear_ts(g95_typespec *);
FILE *g95_open_file(char *);
int g95_directory_separator(char);
FILE *g95_open_included_file(char *);
char *g95_article(char *);
char *g95_basic_typename(bt);
char *g95_mode_name(void);
int g95_varchar(char, int);
void g95_update_locus(g95_locus *, g95_locus *);
int g95_uopchar(char);
char *g95_typename(g95_typespec *);
void g95_show_locus(g95_locus *);
int g95_int_ts_size(g95_typespec *);

char *g95_code2string(mstring *, int);
int g95_string2code(mstring *, char *);
char *g95_intent_string(g95_intent);
char *g95_flavor_string(sym_flavor);
char *g95_procedure_string(procedure_type);
char *g95_access_string(g95_access);
int g95_strcmp(char *, char *);
int g95_front_ts_size(g95_typespec *ts);
void g95_dependent_file(char *);
void g95_target_file(char *);
char *g95_mangle_sym(g95_symbol *, int, char *);
int g95_add_cbinding(char *, int, g95_locus *);
void g95_show_cbinding(g95_namespace *);

void g95_init_1(void);
void g95_init_2(void);
void g95_done_1(void);
void g95_done_2(void);

/* lang-options.c */

int g95_parse_arg(int, char **);
unsigned g95_init_options(unsigned, const char *argv[]);
void g95_options_done(void);
int g95_handle_arg(size_t, const char *, int);
void g95_set_extra(void);
void g95_check_options(void);

/* io.c */

extern g95_st_label g95_format_asterisk;

void g95_free_open(g95_open *);
match g95_match_open(void);
try g95_resolve_open(g95_open *);

void g95_free_inquire(g95_inquire *);
match g95_match_inquire(void);
try g95_resolve_inquire(g95_inquire *);
try g95_resolve_flush(g95_flush *);
void g95_free_flush(g95_flush *);
try g95_resolve_wait(g95_wait *);
void g95_free_wait(g95_wait *);

void g95_free_dt(g95_dt *);
try g95_resolve_dt(g95_code *);
match g95_match_read(void);
match g95_match_write(void);
match g95_match_print(void);
void g95_io_init(void);

void g95_free_close(g95_close *);
match g95_match_close(void);
try g95_resolve_close(g95_close *);

void g95_free_filepos(g95_filepos *);
try g95_resolve_filepos(g95_filepos *);
match g95_match_endfile(void);
match g95_match_backspace(void);
match g95_match_rewind(void);
match g95_match_flush(void);
match g95_match_wait(void);

/* match.c */

match g95_match_space(void);
match g95_match_eos(void);
match g95_match_small_literal_int(int *, int);
match g95_match_st_label(g95_st_label **, int);
match g95_match_label(void);
match g95_match_small_int(int *);
int g95_match_strings(mstring *);
match g95_match_name(char *);
match g95_match_symbol(g95_symbol **, int);
match g95_match_intrinsic_op(g95_intrinsic_op *);
char *g95_op2string(int);
match g95_match_char(char);
match g95_match(char *, ...);
match g95_match_iterator(g95_iterator *);
void g95_free_iterator(g95_iterator *, int);
void g95_free_forall_iterator(g95_forall_iterator *);

/* Statement matchers */

match g95_match_call(void);
void g95_free_equiv(g95_equiv *);
match g95_match_equivalence(void);
void g95_free_data(g95_data *);
match g95_match_data(void);
match g95_match_where(g95_statement *);
g95_common_head *g95_get_common(char *, g95_locus *);
g95_common_head *g95_find_common(char *, g95_namespace *ns);
match g95_match_common(void);
match g95_match_block_data(void);
void g95_free_namelist(g95_namelist *);
match g95_match_namelist(void);
match g95_match_module(void);
match g95_match_elsewhere(void);
match g95_match_forall(g95_statement *);
g95_alloc *g95_get_alloc(void);
void g95_free_alloc_list(g95_alloc *);
match g95_match_allocate(void);
match g95_match_nullify(void);
match g95_match_deallocate(void);
match g95_match_return(void);

match g95_match_cycle(void);
match g95_match_exit(void);
match g95_match_pause(void);
match g95_match_stop(void);
match g95_match_continue(void);
match g95_match_assign(void);
match g95_match_goto(void);
match g95_match_program(void);
match g95_match_pointer_assignment(void);
match g95_match_assignment(void);
match g95_match_import(void);
match g95_match_if(g95_statement *);
match g95_match_else(void);
match g95_match_elseif(void);
match g95_match_do(void);
match g95_match_enum(void);
match g95_match_enumerator(void);

match g95_match_critical(void);
match g95_match_sync_all(void);
match g95_match_sync_images(void);
match g95_match_sync_team(void);
match g95_match_sync_memory(void);
match g95_match_notify(void);
match g95_match_query(void);
match g95_match_error_stop(void);

/* arith.c */

void g95_arith_init_1(void);
void g95_arith_done_1(void);

g95_expr *g95_constant_result(bt, int, g95_locus *);
int g95_validate_kind(bt, int);
int g95_range_check(g95_expr *);

int g95_default_integer_kind(int);
int g95_pointer_integer_kind(void);
int g95_default_real_kind(int);
int g95_default_double_kind(void);
int g95_extended_kind(void);
int g95_quad_kind(void);
int g95_default_character_kind(void);
int g95_default_logical_kind(void);
int g95_default_complex_kind(void);
int g95_compare_string(g95_expr *, g95_expr *, int *);

g95_expr *g95_and(g95_expr *, g95_expr *);
g95_expr *g95_or(g95_expr *, g95_expr *);
g95_expr *g95_neqv(g95_expr *, g95_expr *);
g95_expr *g95_eq(g95_expr *, g95_expr *);
g95_expr *g95_ne(g95_expr *, g95_expr *);
g95_expr *g95_gt(g95_expr *, g95_expr *);
g95_expr *g95_ge(g95_expr *, g95_expr *);
g95_expr *g95_lt(g95_expr *, g95_expr *);
g95_expr *g95_le(g95_expr *, g95_expr *);
g95_expr *g95_uplus(g95_expr *op);
g95_expr *g95_uminus(g95_expr *op);
g95_expr *g95_not(g95_expr *);
g95_expr *g95_eqv(g95_expr *, g95_expr *);
g95_expr *g95_add(g95_expr *, g95_expr *);
g95_expr *g95_subtract(g95_expr *, g95_expr *);
g95_expr *g95_multiply(g95_expr *, g95_expr *);
g95_expr *g95_divide(g95_expr *, g95_expr *);
g95_expr *g95_power(g95_expr *, g95_expr *);
g95_expr *g95_concat(g95_expr *, g95_expr *);

g95_expr *g95_convert_integer(char *, int, int, g95_locus *);
g95_expr *g95_convert_real(char *, int, g95_locus *);
g95_expr *g95_convert_complex(g95_expr *, g95_expr *, int);

g95_expr *g95_int2int(g95_expr *, int);
g95_expr *g95_int2real(g95_expr *, int);
g95_expr *g95_int2complex(g95_expr *, int);
g95_expr *g95_real2int(g95_expr *, int);
g95_expr *g95_real2real(g95_expr *, int);
g95_expr *g95_real2complex(g95_expr *, int);
g95_expr *g95_complex2int(g95_expr *, int);
g95_expr *g95_complex2real(g95_expr *, int);
g95_expr *g95_complex2complex(g95_expr *, int);
g95_expr *g95_log2log(g95_expr *, int);

/* array.c */

void g95_free_array_spec(g95_array_spec *);
void g95_free_coarray_spec(g95_coarray_spec *);
void g95_free_array_ref(g95_array_ref *);
try g95_simplify_array_spec(g95_array_spec *);

match g95_match_coarray_spec(g95_coarray_spec **);
match g95_match_array_spec(g95_array_spec **);
g95_coarray_spec *g95_copy_coarray_spec(g95_coarray_spec *);
match g95_match_coarray_ref(g95_coarray_ref *);
match g95_match_array_ref(g95_array_ref *, int);
int g95_compare_array_spec(g95_array_spec *, g95_array_spec *);
try g95_resolve_array_constructor(g95_expr *);
try g95_check_constructor_type(g95_expr *);
try g95_check_iter_variable(g95_expr *);
try g95_check_constructor(g95_expr *, try (*)(g95_expr *));
g95_constructor *g95_copy_constructor(g95_constructor *src);
g95_expr *g95_get_array_element(g95_expr *, bignum);
bignum g95_array_size(g95_expr *);
int g95_zero_size_ac(g95_expr *);
int g95_zero_size_array(g95_expr *);
bignum g95_array_spec_size(g95_array_spec *);
bignum g95_spec_dimen_size(g95_array_spec *, int);
bignum g95_array_dimen_size(g95_expr *, int);
try g95_array_ref_shape(g95_array_ref *, g95_array_spec *, bignum *, int *);
void g95_find_array_ref(g95_expr *, g95_array_ref **, g95_array_spec **, int);
try g95_set_array_spec(g95_symbol *, g95_array_spec *, g95_locus *);
g95_array_spec *g95_copy_array_spec(g95_array_spec *);
int g95_constant_array_spec(g95_array_spec *, int);
try g95_resolve_array_spec(g95_array_spec *);

g95_expr *g95_start_constructor(g95_typespec *ts, g95_locus *);
void g95_append_constructor(g95_expr *, g95_expr *);
void g95_free_constructor(g95_constructor *);
match g95_match_array_constructor(g95_expr **);
try g95_simplify_iterator_var(g95_expr *);
try g95_expand_iterator(g95_iterator *, try (*)(void *), void *);
try g95_expand_constructor(g95_expr *);
try g95_expand_data_constructor(g95_expr *);
int g95_constant_constructor(g95_expr *);
int g95_expanded_ac(g95_expr *);
try g95_check_conformance(const char *, g95_expr *, g95_expr *);

/* simplify.c */

void g95_simplify_init_1(void);
void g95_simplify_done_1(void);

/* st.c */

extern g95_code new_st;

void g95_clear_new_st(void);
g95_code *g95_get_code(int, g95_locus *);
g95_code *g95_add_statement(void);
g95_code *g95_append_code(g95_code *, g95_code *);
void g95_free_statements(g95_code *);
void g95_undo_statement(void);

/* resolve.c */

extern int g95_where_flag;

try g95_resolve_expr(g95_expr *);
int g95_impure_variable(g95_symbol *);
int g95_pure(g95_symbol *, int);
int g95_elemental(g95_symbol *);
try g95_resolve_iterator(g95_iterator *);
int g95_generic_sym(char *, g95_namespace *, int);
void g95_process_entry(g95_namespace *);
void g95_variable_rank(g95_expr *);
try g95_resolve_array_ref(g95_array_ref *, g95_array_spec *, g95_locus *);
int g95_derived_init(g95_symbol *);
void g95_resolve_null(g95_expr *, g95_typespec *, int);
try g95_resolve(g95_namespace *);
try g95_resolve_variable(g95_expr *, int);

/* entry.c */

g95_symbol *g95_find_entries(g95_symtree *, int);
try g95_resolve_entry(g95_namespace *ns);

/* expr.c */

g95_actual_arglist *g95_get_actual_arglist(void);
void g95_free_actual_arglist(g95_actual_arglist *);
g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *);
try g95_check_parameter(g95_symbol *);
try g95_simplify_expr(g95_expr *);
try g95_simplify_spec_expr(g95_expr *);
try g95_simplify_init_expr(g95_expr *);
char *g95_extract_int(g95_expr *, int *);
bignum *g95_get_shape(int);
g95_expr *g95_null_expr(g95_locus *);
g95_expr *g95_len_expr(g95_expr *);
g95_symbol *g95_get_temporary(g95_typespec *ts, int, int);
g95_symbol *g95_get_temporary_int(void);
g95_symbol *g95_get_array_int(void);
g95_expr *g95_get_variable_expr(g95_symbol *sym, g95_locus *);
g95_expr *g95_build_funcall(g95_symbol *func, ...);
int g95_scalar_function(g95_expr *);
int g95_vector_subscripts(g95_expr *e);
int g95_coindexed_expr(g95_expr *);
int g95_coarray_expr(g95_expr *);
void g95_free_ref_list(g95_ref *);
void g95_type_convert_binary(g95_expr *);
int g95_is_constant_expr(g95_expr *);

g95_charlen *g95_get_charlen(g95_namespace *);
g95_expr *g95_get_expr(void);
void g95_free_expr(g95_expr *);
void g95_free_expr0(g95_expr *);
void g95_replace_expr(g95_expr *, g95_expr *);
g95_expr *g95_int_expr(int);
g95_expr *g95_logical_expr(int, g95_locus *);
g95_expr *g95_char_expr(int, int, g95_locus *);
g95_formal_arglist *g95_copy_formal_arglist(g95_formal_arglist *);
g95_ref *g95_full_ref(g95_locus *where);
g95_expr *g95_copy_expr(g95_expr *);

try g95_check_init_expr(g95_expr *);
match g95_match_init_expr(g95_expr **);
try g95_specification_expr(g95_expr *);
void g95_binary_result_type(g95_typespec *, g95_typespec *, g95_typespec *);

int g95_numeric_ts(g95_typespec *);
int g95_kind_max(g95_expr *, g95_expr *);
comparison g95_compare_expr(g95_expr *, g95_expr *);
comparison g95_compare_expr_int(g95_expr *, int);
int g95_expr_corank(g95_expr *);
int g95_c_ptr(g95_typespec *);
void g95_expr_done_1(void);

/* interface.c */

void g95_free_interface(g95_interface *);
match g95_match_generic_spec(interface_type *, char *, int *);
match g95_match_interface(void);
match g95_match_abstract_interface(void);
match g95_match_end_interface(void);
int g95_compare_types(g95_typespec *, g95_typespec *);
void g95_check_interfaces(g95_namespace *);
void g95_arg_usage(g95_intent, g95_expr *);
void g95_procedure_use(g95_symbol *, g95_actual_arglist **, g95_locus *);
int g95_self_interface(char *, g95_symbol **);
g95_symbol *g95_search_interface(g95_interface *, int, g95_actual_arglist **);
try g95_extend_expr(g95_expr *);
void g95_free_formal_arglist(g95_formal_arglist *);
int g95_extend_assign(g95_code *, g95_namespace *);
void g95_check_global_refs(void);
void g95_add_global_ref(g95_gsymbol *, g95_symbol *, g95_actual_arglist *,
			g95_locus *);
int g95_delete_global_ref(g95_gsymbol *, g95_actual_arglist *);
int g95_copy_global_ref(g95_gsymbol *, g95_actual_arglist *,
			g95_actual_arglist *);
void g95_define_global(g95_gsymbol *, g95_symbol *, g95_locus *);
try g95_add_interface(g95_symbol *sym, g95_locus *where);

/* scalarize.c */

int g95_elemental_function(g95_expr *);
g95_code *g95_scalarize_spec_expr(g95_expr *);
g95_code *g95_expand_where(g95_code *);
void g95_scalarize(g95_namespace *);
int g95_check_dependency(g95_expr *, g95_expr *);
void g95_scalarize_assignment(g95_code **);


#define SEEN_FULL     1
#define SEEN_SECTION  2
#define SEEN_SCALAR   4
#define SEEN_POINTER  8
#define SEEN_EQUIV    16


/* format.c */

void g95_check_format_string(g95_expr *, int, int);
match g95_match_format(void);

/* module.c */

void g95_module_init_2(void);
void g95_module_done_2(void);
match g95_match_use(void);
void g95_dump_module(char *);
int g95_module_parameter(char *, g95_expr *);
void g95_module_type(char *, internal_type);
void g95_module_operator(int);
void g95_module_proc(char *, internal_proc);
void g95_use_module(void);

int g95_check_ieee_datatype(g95_namespace *);
int g95_check_ieee_denormal(g95_namespace *);
int g95_check_ieee_divide(g95_namespace *);
int g95_check_ieee_halting(g95_namespace *);
int g95_check_ieee_inf(g95_namespace *);
int g95_check_ieee_nan(g95_namespace *);
int g95_check_ieee_rounding(g95_namespace *);
int g95_check_ieee_sqrt(g95_namespace *);
int g95_check_ieee_inexact(g95_namespace *);
int g95_check_ieee_invalid(g95_namespace *);
int g95_check_ieee_underflow(g95_namespace *);

/* bigreal.c */

void g95_zero_division(g95_locus *);
int bg_compare_int(bignum, int);
int bg_compare_lt(bignum, bignum);
int bg_compare_eq(bignum, bignum);
int bg_compare_ne(bignum, bignum);
int bg_compare_ge(bignum, bignum);
int bg_compare_le(bignum, bignum);
int bg_compare_gt(bignum, bignum);
ff_class bg_real_type(bignum);
bignum bg_nearest(bignum, int);
bignum bg_rrspacing(bignum);
bignum bg_scale(bignum, int);
bignum bg_set_exponent(bignum, int);
bignum bg_spacing(bignum);
bignum bg_abs(bignum);
bignum bg_get_exceptional(ff_class, int, int);

bignum bg_sine(bignum);
bignum bg_cosine(bignum);
bignum bg_exponential(bignum);
bignum bg_hyperbolic_sine(bignum);
bignum bg_hyperbolic_cosine(bignum);
bignum bg_ln(bignum);
bignum bg_log(bignum);
bignum bg_power(bignum, bignum);
bignum bg_arctangent(bignum);
bignum bg_hypot(bignum, bignum);

bignum bg_negate(bignum);
bignum bg_divide(bignum, bignum);
bignum bg_divide_int(bignum, int);
bignum bg_pow_int(bignum, int);
bignum bg_pow(bignum, bignum);
bignum bg_add(bignum, bignum);
bignum bg_add_int(bignum, int);
bignum bg_subtract(bignum, bignum);
bignum bg_multiply(bignum, bignum);
bignum bg_multiply_int(bignum, int);

bignum bg_to_bi(bignum);
bignum bg_from_bi(bignum, g95_ff *);
bignum bg_from_int(int, g95_ff *);
int bg_to_int(bignum);
bignum bg_halves(int, g95_ff *);
bignum bg_convert(bignum, g95_ff *);
bignum bg_trunc(bignum);
bignum bg_round(bignum);
bignum bg_floor(bignum);
bignum bg_ceiling(bignum);
bignum bg_sqrt(bignum);

bignum bg_huge(g95_ff *);
bignum bg_tiny(g95_ff *);
bignum bg_epsilon(g95_ff *);

int bg_exponent(bignum);
bignum bg_fraction(bignum);

void g95_pack_real(bignum, char *);
bignum g95_unpack_real(char *, g95_ff *);

bignum bg_from_string(char *, g95_ff *);
char *bg_to_string(bignum);
g95_ff *g95_get_ff(int);

void bg_init(void);
void bg_done(void);

/* bigint.c */

extern bignum bi_0, bi_1, bi_2, bi_10, bi_m1, bi_maxint, bi_minint;

void big_initialize(void);
void big_terminate(void);
void big_no_check(void);
bignum big_copy(bignum);
bignum big_clone_perm(bignum);
void big_permanent(bignum);
void big_depermanent(bignum);
void big_free(bignum);
bignum big_clone(bignum);
int bi_compare(bignum, bignum);
bignum int_to_bi(int);
bignum bi_from_string(char *, int);
char *bi_to_string(bignum);
int bi_to_int(bignum);
void bi_print(FILE *, bignum bi);
bignum bi_scan(FILE *);
bignum bi_int_add(bignum, int);
bignum bi_int_subtract(bignum, int);
bignum bi_int_multiply(bignum, int);
bignum bi_int_divide(bignum, int);
int bi_int_rem(bignum, int);
int bi_int_mod(bignum, int);

bignum bi_add(bignum, bignum);
bignum bi_subtract(bignum, bignum);
bignum bi_multiply(bignum, bignum);
bignum bi_divide(bignum, bignum);
bignum bi_bad_divide(bignum, bignum);
bignum bi_rem(bignum, bignum);
bignum bi_mod(bignum, bignum);

/* Some less common operations. */

bignum bi_negate(bignum);
bignum bi_abs(bignum);
bignum bi_half(bignum);
bignum bi_double(bignum);
bignum bi_square(bignum);
bignum bi_power(bignum, bignum);
bignum bi_2n(int);
bignum bi_sqrt(bignum);
bignum bi_factorial(bignum);

int bi_is_odd(bignum);
int bi_is_even(bignum);
int bi_is_zero(bignum);
int bi_is_one(bignum);
int bi_is_negative(bignum);
int bi_bits(bignum);

bignum bi_and(bignum, bignum, int);
bignum bi_or(bignum, bignum, int);
bignum bi_xor(bignum, bignum, int);
bignum bi_not(bignum, int);

bignum bi_huge(int);
void g95_pack_int(bignum, int, char *);
bignum g95_unpack_int(char *, int);
void g95_pack_logical(int, int, char *);
int g95_unpack_logical(char *, int);
bignum bi_setbit(bignum, int, int, int);
int bi_getbit(bignum, int, int);
bignum bi_shift(bignum, int, int, int);
int g95_bit_size(int);
g95_integer_info *g95_int_info(int);

extern bignum bg_pi, bg_half_pi, bg_two_pi, bg_e, bg_ln2, bg_ln10;
extern g95_locus *g95_arithmetic_locus;

/* trans.c */

void g95_generate_code(g95_namespace *);

/* matchexp.c */

match g95_match_defined_op_name(char *, int);
match g95_match_expr(g95_expr **);

/* imodule.c */

int g95_internal_derived(g95_typespec *, internal_type);
int g95_use_internal(char *);
int g95_ieee_class_compare(int);
int g95_iproc_purity(internal_proc);
char *g95_iproc_name(internal_proc);

/* show.c */

void g95_show_expr(g95_expr *);
void g95_show_namespace(g95_namespace *);

/* dump.c */

void g95_dump(g95_namespace *);
void g95_dump_done(void);
g95_annot *g95_annotate(int, g95_locus *);

/* bbt.c */

void g95_insert_bbt(void *, void *, int (*)());
void g95_delete_bbt(void *, void *, int (*)());

/* forall.c */

void g95_expand_forall(g95_code *);
try g95_resolve_forall_iterators(g95_forall_iterator *);
int g95_find_variable(g95_expr *, g95_symbol *);
g95_code *g95_forall_loops(g95_code *);
int g95_expanding_forall(void);

/* data.c */

try g95_expand_ac_element(g95_expr *);

/* select.c */

void g95_free_case_list(g95_case *);
match g95_match_case(void);
match g95_match_select(void);
try g95_resolve_select(g95_code *);

