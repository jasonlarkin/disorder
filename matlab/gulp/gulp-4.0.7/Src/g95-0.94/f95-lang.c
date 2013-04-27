/* G95 Backend interface
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook.

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

/* f95-lang.c-- GCC backend interface stuff */

#include "trans.h"


/* Language-dependent contents of an identifier.  */

/* The limbo_value is used for block level extern declarations, which need
   to be type checked against subsequent extern declarations.  They can't
   be referenced after they fall out of scope, so they can't be global.

   The rid_code field is used for keywords.  It is in all
   lang_identifier nodes, because some keywords are only special in a
   particular context.  */

struct lang_identifier
GTY (())
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union lang_tree_node
GTY ((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE")))
{
    union tree_node GTY ((tag ("0"),
			  desc ("tree_node_structure (&%h)"))) generic;
    struct lang_identifier GTY ((tag ("1"))) identifier;
};


/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct language_function GTY (()) {
  /* struct g95_language_function base; */
    tree named_labels;
    tree shadowed_labels;
    int returns_value;
    int returns_abnormally;
    int warn_about_return_type;
    int extern_inline;
    struct binding_level *binding_level;
};




/* These functions and variables deal with binding contours.  We only
   need these functions for the list of PARM_DECLs, but we leave the
   functions more general; these are a simplified version of the
   functions from GNAT.  */

/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

        the global one
        one for each subprogram definition
        one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct binding_level
GTY (())
{
  /* A chain of ..._DECL nodes for all variables, constants, functions,
     parameters and type declarations.  These ..._DECL nodes are chained
     through the TREE_CHAIN field. Note that these ..._DECL nodes are stored
     in the reverse of the order supplied to be compatible with the
     back-end.  */
    tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
    tree blocks;
  /* The back end may need, for its own internal processing, to create a BLOCK
     node. This field is set aside for this purpose. If this field is non-null
     when the level is popped, i.e. when poplevel is invoked, we will use such
     block instead of creating a new one from the 'names' field, that is the
     ..._DECL nodes accumulated so far.  Typically the routine 'pushlevel'
     will be called before setting this field, so that if the front-end had
     inserted ..._DECL nodes in the current block they will not be lost.   */
    tree block_created_by_back_end;
  /* The binding level containing this one (the enclosing binding level). */
    struct binding_level *level_chain;
};


/* The binding level currently in effect.  */
static GTY(()) struct binding_level *current_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static GTY(()) struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = { NULL, NULL, NULL, NULL };



/* Each front end provides its own.  */
static bool g95_init(void);

static void print_identifier(FILE *, tree, int);
void do_function_end (void);
int global_bindings_p (void);
void insert_block (tree);
void set_block (tree);



static void expand_function(tree fndecl) {

    tree_rest_of_compilation(fndecl);
}


/* Clear the binding stack.  */
static void clear_binding_stack(void) {

    while(!global_bindings_p())
	poplevel(0, 0, 0);
}


/* Builtin function initialisation.  */
/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function's attribute list.  */

tree builtin_function(const char *name, tree type, int function_code,
		      enum built_in_class class, const char *library_name,
		      tree attrs ATTRIBUTE_UNUSED) {

    tree decl = build_decl(FUNCTION_DECL, get_identifier (name), type);
    DECL_EXTERNAL(decl) = 1;
    TREE_PUBLIC(decl) = 1;

    if (library_name)
	SET_DECL_ASSEMBLER_NAME(decl, get_identifier (library_name));

    make_decl_rtl(decl);
    pushdecl(decl);

    DECL_BUILT_IN_CLASS(decl) = class;
    DECL_FUNCTION_CODE(decl) = function_code;

#if 0
    if (attrs)
	decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
    else
	decl_attributes (&decl, NULL_TREE, 0);
#endif
    return decl;
}


/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   In Fortran 95 this is only the case for variables with
   the TARGET attribute, but we implement it here for a
   likely future Cray pointer extension.
   Value is 1 if successful.  */
/* TODO: Check/fix mark_addressable.  */

static bool mark_addressable(tree exp) {
register tree x = exp;

    while (1)
	switch (TREE_CODE (x)) {
	case COMPONENT_REF:
	    if (DECL_C_BIT_FIELD (TREE_OPERAND (x, 1))) {
		error ("cannot take address of bitfield `%s'",
		       IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (x, 1))));
		return 0;
	    }

	/* ... fall through ...  */

	case ADDR_EXPR:
	case ARRAY_REF:
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	    x = TREE_OPERAND (x, 0);
	    break;

	case CONSTRUCTOR:
	    TREE_ADDRESSABLE (x) = 1;
	    return 1;

	case VAR_DECL:
	case CONST_DECL:
	case PARM_DECL:
	case RESULT_DECL:
	    if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x) &&
		DECL_NONLOCAL (x)) {
		if (TREE_PUBLIC (x)) {
		    error("global register variable `%s' used in nested function",
			 IDENTIFIER_POINTER (DECL_NAME (x)));
		    return 0;
		}
		pedwarn ("register variable `%s' used in nested function",
			 IDENTIFIER_POINTER (DECL_NAME (x)));
	    }
	    else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
		{
		    if (TREE_PUBLIC (x))
			{
			    error ("address of global register variable `%s' requested",
				   IDENTIFIER_POINTER (DECL_NAME (x)));
			    return 0;
			}
		    
#if 0
	    /* If we are making this addressable due to its having
	       volatile components, give a different error message.  Also
	       handle the case of an unnamed parameter by not trying
	       to give the name.  */

		    else if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (x)))
			{
			    error ("cannot put object with volatile field into register");
			    return 0;
			}
#endif

		    pedwarn ("address of register variable `%s' requested",
			     IDENTIFIER_POINTER (DECL_NAME (x)));
		}

	/* drops in */
	case FUNCTION_DECL:
	    TREE_ADDRESSABLE (x) = 1;

	default:
	    return 1;
	}
}


/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */


/* Tree code classes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const enum tree_code_class tree_code_type[] = {
#include "tree.def"
  tcc_exceptional,
#include "c-common.def"
};
#undef DEFTREECODE


/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0,
#include "c-common.def"
};
#undef DEFTREECODE



/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy",
#include "c-common.def"
};
#undef DEFTREECODE

static tree named_labels;

#define NULL_BINDING_LEVEL (struct binding_level *) NULL

/* A chain of binding_level structures awaiting reuse.  */

static GTY(()) struct binding_level *free_binding_level;


/* language-specific flags.  */

/* static tree shadowed_labels=NULL; */

static struct stmt_tree_s g95_stmt_tree;



/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, boolean_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `boolean_type_node'.  */

tree g95_truthvalue_conversion(tree expr) {

    switch(TREE_CODE(TREE_TYPE(expr))) {
    case BOOLEAN_TYPE:
	if (TREE_TYPE(expr) == boolean_type_node)
	    return expr;

	if (TREE_CODE_CLASS(TREE_CODE(expr)) == '<') {
	    TREE_TYPE(expr) = boolean_type_node;
	    return expr;
	}

	if (TREE_CODE(expr) == NOP_EXPR)
	    return build1(NOP_EXPR, boolean_type_node, TREE_OPERAND(expr, 0));

	return build1(NOP_EXPR, boolean_type_node, expr);
    
    case INTEGER_TYPE:
	if (TREE_CODE(expr) == INTEGER_CST)
	    return integer_zerop (expr)
		? boolean_false_node : boolean_true_node;

	return g95_build_ne(expr, integer_zero_node);

    default:
	internal_error("Unexpected type in truthvalue_conversion");
    }
}



int anon_aggr_type_p(tree node ATTRIBUTE_UNUSED) {
    return 0;
}



stmt_tree current_stmt_tree (void) {

    return &g95_stmt_tree;
}




static void define_builtin(const char *name, tree type, int code,
			   const char *library_name, bool const_p) {
tree decl;

    decl = builtin_function(name, type, code, BUILT_IN_NORMAL,
			    library_name, NULL_TREE);

    if (const_p)
	TREE_READONLY(decl) = 1;

    built_in_decls[code] = decl;
    implicit_built_in_decls[code] = decl;
}



static void build_builtins(void) {
tree tmp, type;

    tmp = tree_cons(NULL_TREE, size_type_node, void_list_node);
    type = build_function_type(pvoid_type_node, tmp);
    define_builtin("__builtin_alloca", type, BUILT_IN_ALLOCA, "alloca", false);

    type = build_function_type(pvoid_type_node, void_list_node);
    define_builtin("__builtin_stack_save", type, BUILT_IN_STACK_SAVE,
		   "stack_save", false);

    tmp = tree_cons(NULL_TREE, pvoid_type_node, void_list_node);
    type = build_function_type (void_type_node, tmp);
    define_builtin("__builtin_stack_restore", type, BUILT_IN_STACK_RESTORE,
		   "stack_restore", false);

    tmp = tree_cons(NULL_TREE, pvoid_type_node, void_list_node);
    tmp = tree_cons(NULL_TREE, pvoid_type_node, tmp);
    tmp = tree_cons(NULL_TREE, pvoid_type_node, tmp);
    type = build_function_type (void_type_node, tmp);
    define_builtin("__builtin_init_trampoline", type,
		   BUILT_IN_INIT_TRAMPOLINE, "init_trampoline", false);

    tmp = tree_cons (NULL_TREE, pvoid_type_node, void_list_node);
    type = build_function_type (pvoid_type_node, tmp);
    define_builtin("__builtin_adjust_trampoline", type,
		   BUILT_IN_ADJUST_TRAMPOLINE, "adjust_trampoline", true);

    tmp = g95_chainon_list(NULL_TREE, g95_real4_type_node);
    type = build_function_type(g95_real4_type_node, tmp);

    define_builtin("__builtin_sinf",   type, BUILT_IN_SINF,   "sinf",   false);
    define_builtin("__builtin_cosf",   type, BUILT_IN_COSF,   "cosf",   false);
    define_builtin("__builtin_tanf",   type, BUILT_IN_TANF,   "tanf",   false);
    define_builtin("__builtin_atanf",  type, BUILT_IN_ATANF,  "atanf",  false);
    define_builtin("__builtin_sqrtf",  type, BUILT_IN_SQRTF,  "sqrtf",  false);

    define_builtin("__builtin_expf",   type, BUILT_IN_EXPF,   "expf",   false);
    define_builtin("__builtin_logf",   type, BUILT_IN_LOGF,
		   PREFIX "logf",   false);
    define_builtin("__builtin_log10f", type, BUILT_IN_LOG10F,
		   PREFIX "log10f", false);

    tmp = g95_chainon_list(NULL_TREE, g95_real4_type_node);
    tmp = g95_chainon_list(tmp, g95_real4_type_node);
    type = build_function_type(g95_real4_type_node, tmp);

    define_builtin("__builtin_powf",   type, BUILT_IN_POWF,   "powf",   false);

    tmp = g95_chainon_list(NULL_TREE, g95_real8_type_node);
    type = build_function_type(g95_real8_type_node, tmp);

    define_builtin("__builtin_sin",   type, BUILT_IN_SIN,   "sin",   false);
    define_builtin("__builtin_cos",   type, BUILT_IN_COS,   "cos",   false);
    define_builtin("__builtin_tan",   type, BUILT_IN_TAN,   "tan",   false);
    define_builtin("__builtin_atan",  type, BUILT_IN_ATAN,  "atan",  false);
    define_builtin("__builtin_sqrt",  type, BUILT_IN_SQRT,  "sqrt",  false);

    define_builtin("__builtin_exp",   type, BUILT_IN_EXP,   "exp",   false);
    define_builtin("__builtin_log",   type, BUILT_IN_LOG,
		   PREFIX "log", false);
    define_builtin("__builtin_log10", type, BUILT_IN_LOG10,
		   PREFIX "log10", false);

    tmp = g95_chainon_list(NULL_TREE, g95_real8_type_node);
    tmp = g95_chainon_list(tmp, g95_real8_type_node);
    type = build_function_type(g95_real8_type_node, tmp);

    define_builtin("__builtin_pow",   type, BUILT_IN_POW,   "pow",   false);

    build_common_builtin_nodes();
}


/* Create tree nodes for the basic scalar types of Fortran 95,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

static void init_decl_processing(void) {

    current_function_decl = NULL;
    named_labels = NULL;
    current_binding_level = NULL_BINDING_LEVEL;
    free_binding_level = NULL_BINDING_LEVEL;

  /* Make the binding_level structure for global names. We move all
     variables that are in a COMMON block to this binding level.  */
    pushlevel(0);
    global_binding_level = current_binding_level;

  /* Build common tree nodes. char_type_node is unsigned because we
     only use it for actual characters, not for INTEGER(1). Also, we
     want double_type_node to actually have double precision.   */

    build_common_tree_nodes(false, false);

    set_sizetype(long_unsigned_type_node);
    build_common_tree_nodes_2(0);

  /* Set up F95 type nodes.  */
    g95_init_types();
}



static void be_parse_file(int set_yydebug ATTRIBUTE_UNUSED) {
int errors, warnings;

    g95_build_builtin_decls();
    build_builtins();

    if (extra_warnings)
	g95_set_extra();

    g95_parse_file();

    cgraph_finalize_compilation_unit();
    cgraph_optimize();

  /* Tell the back end about any errors.  */
    g95_get_errors(&warnings, &errors);

    errorcount   += errors;
    warningcount += warnings;
}

/* Routines Expected by GCC:  */


static void print_identifier(FILE * file ATTRIBUTE_UNUSED,
			     tree t ATTRIBUTE_UNUSED,
			     int i ATTRIBUTE_UNUSED) {

}



/* Return non-zero if we are currently in the global binding level.  */

int global_bindings_p (void) {
    return current_binding_level == global_binding_level ? -1 : 0;
}


tree getdecls(void) {
    return current_binding_level->names;
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void pushlevel(int ignore ATTRIBUTE_UNUSED) {
  struct binding_level *newlevel
    = (struct binding_level *) ggc_alloc (sizeof (struct binding_level));

    *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
    newlevel->level_chain = current_binding_level;
    current_binding_level = newlevel;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree poplevel(int keep, int reverse, int functionbody) {
  /* Points to a BLOCK tree node. This is the BLOCK node constructed for the
     binding level that we are about to exit and which is returned by this
     routine.  */

tree block_node = NULL_TREE;
tree decl_chain;
tree subblock_chain = current_binding_level->blocks;
tree subblock_node;
tree block_created_by_back_end;

  /* Reverse the list of XXXX_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitely stored in
     the right order.  */

    decl_chain = (reverse) ? nreverse (current_binding_level->names)
	: current_binding_level->names;

    block_created_by_back_end =
	current_binding_level->block_created_by_back_end;

    if (block_created_by_back_end != 0) {
	block_node = block_created_by_back_end;

      /* Check if we are about to discard some information that was gathered
         by the front-end. Nameley check if the back-end created a new block
         without calling pushlevel first. To understand why things are lost
         just look at the next case (i.e. no block created by back-end.  */
	if ((keep || functionbody) && (decl_chain || subblock_chain))
	    abort ();
    }

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */

    else if (keep || functionbody)
#if TARGET_GCC_VERSION < 410
	block_node = build_block(keep ? decl_chain : 0, 0,
				 subblock_chain, 0, 0);
#else
    block_node = build_block(keep ? decl_chain : 0, subblock_chain, 0, 0);
#endif

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
    for (subblock_node = subblock_chain; subblock_node;
	 subblock_node = TREE_CHAIN (subblock_node))
	BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

    for (subblock_node = decl_chain; subblock_node;
	 subblock_node = TREE_CHAIN (subblock_node))
	if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,
         don't forget that fact.   */
	    if (DECL_EXTERNAL (subblock_node)){
		if (TREE_USED (subblock_node))
		    TREE_USED (DECL_NAME (subblock_node)) = 1;
		if (TREE_ADDRESSABLE (subblock_node))
		    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
	    }

  /* Pop the current level.  */
    current_binding_level = current_binding_level->level_chain;

    if (functionbody) {
      /* This is the top level block of a function. The ..._DECL chain stored
         in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
         leave them in the BLOCK because they are found in the FUNCTION_DECL
         instead.  */
	DECL_INITIAL (current_function_decl) = block_node;
	BLOCK_VARS (block_node) = 0;

    } else if (block_node) {
	if (block_created_by_back_end == NULL)
	    current_binding_level->blocks
		= chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */

    else if (subblock_chain)
	current_binding_level->blocks
	    = chainon (current_binding_level->blocks, subblock_chain);
    if (block_node)
	TREE_USED (block_node) = 1;

    return block_node;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void insert_block(tree block) {

    TREE_USED (block) = 1;
    current_binding_level->blocks
	= chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void set_block(tree block) {
    current_binding_level->block_created_by_back_end = block;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree pushdecl (tree decl) {

  /* External objects aren't nested, other objects may be.  */
    if ((DECL_EXTERNAL(decl)) || (decl == current_function_decl))
	DECL_CONTEXT(decl) = 0;
    else
	DECL_CONTEXT(decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.  */

    TREE_CHAIN(decl) = current_binding_level->names;
    current_binding_level->names = decl;

  /* For the declaration of a type, set its name if it is not already set. */

    if (TREE_CODE(decl) == TYPE_DECL && TYPE_NAME(TREE_TYPE(decl)) == 0) {
	if (DECL_SOURCE_LINE(decl) == 0)
	    TYPE_NAME(TREE_TYPE(decl)) = decl;
	else
	    TYPE_NAME(TREE_TYPE(decl)) = DECL_NAME(decl);
    }

    return decl;
}

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"



bool g95_init(void) {

    init_decl_processing();

    g95_init_1();
    g95_new_file();

    return true;
}



#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_FINISH
#undef LANG_HOOKS_INIT_OPTIONS
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_PRINT_IDENTIFIER
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_MARK_ADDRESSABLE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_UNSIGNED_TYPE
#undef LANG_HOOKS_SIGNED_TYPE
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#undef LANG_HOOKS_CLEAR_BINDING_STACK
#undef LANG_HOOKS_IDENTIFIER_SIZE

#if TARGET_GCC_VERSION < 410
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION   g95_truthvalue_conversion
#endif


/* Define language hooks.  */

#if defined(FPU_PPC_AIX) || defined(FPU_PPC_LINUX)
#define LANG_HOOKS_NAME                 "GNU F95"
#elif STD_F
#define LANG_HOOKS_NAME                 "F (g95)"
#else
#define LANG_HOOKS_NAME                 "G95 Fortran 95"
#endif


#define LANG_HOOKS_INIT                 g95_init
#define LANG_HOOKS_FINISH               g95_done_1
#define LANG_HOOKS_INIT_OPTIONS         g95_init_options
#define LANG_HOOKS_POST_OPTIONS         g95_post_options
#define LANG_HOOKS_HANDLE_OPTION        g95_handle_arg
#define LANG_HOOKS_PRINT_IDENTIFIER     print_identifier
#define LANG_HOOKS_PARSE_FILE              be_parse_file
#define LANG_HOOKS_MARK_ADDRESSABLE        mark_addressable
#define LANG_HOOKS_TYPE_FOR_MODE           g95_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE           g95_type_for_size
#define LANG_HOOKS_UNSIGNED_TYPE           g95_unsigned_type
#define LANG_HOOKS_SIGNED_TYPE             g95_signed_type
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE g95_signed_or_unsigned_type
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION expand_function
#define LANG_HOOKS_CLEAR_BINDING_STACK     clear_binding_stack
#define LANG_HOOKS_IDENTIFIER_SIZE \
        (sizeof (struct c_common_identifier) + 3 * sizeof (void *))

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;



/* Stubs for do-nothing garbage collector */

void gt_pch_nx_lang_tree_node(void *p) { p = NULL; }
void gt_ggc_mx_lang_tree_node(void *p) { p = NULL; }
void gt_ggc_mx_language_function(void *p) { p = NULL; }
void gt_pch_nx_language_function(void *p) { p = NULL; }

const struct ggc_root_tab * const gt_ggc_rtab[] = { NULL };
const struct ggc_root_tab * const gt_ggc_deletable_rtab[] = { NULL };
const struct ggc_cache_tab * const gt_ggc_cache_rtab[] = { NULL };

const struct ggc_root_tab * const gt_pch_cache_rtab[] = { NULL };
const struct ggc_root_tab * const gt_pch_scalar_rtab[] = { NULL };


