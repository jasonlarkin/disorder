/* Header for code translation functions
   Copyright (C) 2000 - 2008 Free Software Foundation, Inc.
   Contributed by Paul Brook

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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"

#include "tree-inline.h"
#include "flags.h"
#include "ggc.h"
#include "toplev.h"
#include "function.h"
#include "intl.h"
#include "real.h"
#include "target.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "timevar.h"
#include "debug.h"
#include "diagnostic.h"
#include "c-common.h"
#include "cgraph.h"
#include "tree-dump.h"
#include "tree-gimple.h"
#include "rtl.h"
#include "output.h"
#include "expr.h"

#include <assert.h>
#include "g95.h"


/* Structure for holding a block of statements.  It should be treated
 * as an opaque entity and not modified directly.  This allows us to
 * change the underlying representation of statement lists.  */

typedef struct {
    tree head;
    int has_scope:1;
} stmtblock_t;


typedef struct desc_list {
    g95_symbol *sym;
    tree desc;
    struct desc_list *next;
} desc_list;


typedef struct g95_trans_context {
    g95_namespace *current_procedure;

    tree saved_current_function, current_function_decl, result,	labels;
    int frame_size;

    stmtblock_t pre, post;
    struct g95_trans_context *parent;
    desc_list *iodesc;

} g95_trans_context;

extern g95_trans_context *g95_context;


typedef struct {
    g95_typespec ts;
    g95_array_spec *as;
    g95_coarray_spec *cas;
    int pointer, allocatable, dummy, static_storage, noinit;

    g95_expr *value;
    tree desc;
} variable_info;


/* A simplified expression */

typedef struct g95_se {
    /* Code blocks to be executed before and after using the value.  */
    stmtblock_t pre;
    stmtblock_t post;

    /* The result of the expression */
    tree expr;

    /* The length of a character string value.  */
    tree string_length;
    int reflevel;

    tree active_var;
    int load_pre, store_post;

    struct g95_se *parent;
} g95_se;


typedef struct {
    tree type, base;
    tree filename, next, line;
} g95_frame_t;


#define G95_ARRAY_DESC(T)        ((T)->vec.a[0])
#define G95_ARRAY_STORAGE(T)     ((T)->vec.a[1])
#define G95_ARRAY_INIT(T)        ((T)->vec.a[2])
#define G95_ARRAY_SIZE(T)        ((T)->vec.a[3])
#define G95_ARRAY_RANK(T)        ((T)->vec.a[4])
#define G95_ARRAY_ESIZE(T)       ((T)->vec.a[5])
#define G95_ARRAY_BASE(T)        ((T)->vec.a[6])
#define G95_ARRAY_DUMMY(T)       ((T)->vec.a[7])
#define G95_ARRAY_FIELD(T)       ((T)->vec.a[8])
#define G95_ARRAY_AC(T)          ((T)->vec.a[9])
#define G95_ARRAY_CORANK(T)      ((T)->vec.a[10])

#define G95_ARRAY_MULT(T, I)     ((T)->vec.a[0*2*G95_MAX_DIMENSIONS+11+(I)])
#define G95_ARRAY_LBOUND(T, I)   ((T)->vec.a[1*2*G95_MAX_DIMENSIONS+11+(I)])
#define G95_ARRAY_UBOUND(T, I)   ((T)->vec.a[2*2*G95_MAX_DIMENSIONS+11+(I)])

#define G95_ARRAY_WORDS (3*2*G95_MAX_DIMENSIONS+11)


#define G95_DTYPE_TYPE(T)        ((T)->vec.a[0])
#define G95_DTYPE_INITIAL(T)     ((T)->vec.a[1])
#define G95_DTYPE_INIT_VAR(T)    ((T)->vec.a[2])
#define G95_DTYPE_ALLOCS(T)      ((T)->vec.a[3])

#define G95_DTYPE_WORDS 4

#define BLANK_COMMON_NAME     "_BLNK__"

/* trans-stmt.c */

tree g95_trans_code(g95_code *);
tree g95_derived_type_init(g95_typespec *);

/* trans-array.c */

tree g95_desc_element_ref(g95_se *, g95_array_ref *, g95_array_spec *,
			  g95_locus *);

void g95_array_element_ref(g95_se *, g95_array_ref *, g95_typespec *,
			   g95_array_spec *, int, g95_locus *);

tree g95_fix_dummy_array(g95_symbol *);
tree g95_desc_addr(tree);
tree g95_esize_value(tree);
tree g95_esize_ref(tree);
tree g95_base_value(tree);
tree g95_base_ref(tree);
tree g95_offset_ref(tree);
tree g95_rank_ref(tree);
tree g95_corank_ref(tree);
tree g95_character_array_len(g95_symbol *);
tree g95_transfer_result(stmtblock_t *, tree, tree, tree);
void g95_nullify_array_pointer(stmtblock_t *, tree);
void g95_set_section_info(stmtblock_t *, int, tree);
tree g95_build_section_info(g95_expr *, g95_se *, stmtblock_t *,stmtblock_t *);

tree g95_desc_info(tree, tree, int);
tree g95_multiplier_ref(tree, int);
tree g95_lbound_ref(tree, int);
tree g95_ubound_ref(tree, int);

int g95_contiguous(g95_expr *);
void g95_array_argument(g95_se *, g95_actual_arglist *);
void g95_pointer_array_assignment(g95_code *, stmtblock_t *);
void g95_init_array_types(void);
tree g95_get_array_desc(int, int);
tree g95_get_array_pdesc(int, int);
tree g95_get_array_storage(variable_info *, tree);
void g95_init_descriptor(g95_se *, tree, tree, tree, tree);
void g95_conv_descriptor(g95_se *, g95_expr *, int);
void g95_conv_nondesc(g95_se *, g95_intent, g95_expr *, tree);
tree g95_null_array(int, int);
void g95_init_array_desc(variable_info *, tree);

/* trans-intrinsic.c */

void g95_dim(g95_se *, tree, tree);
void g95_conv_intrinsic_function(g95_se *, g95_expr *);
tree g95_conv_intrinsic_subroutine(g95_code *);
void g95_conv_modproc_function(g95_se *, g95_expr *);
tree g95_conv_modproc_subroutine(g95_code *);

/* trans-types.c */

void g95_init_types(void);
tree g95_get_int_type(int);
tree g95_get_real_type(int);
tree g95_get_complex_type(int);
tree g95_get_logical_type(int);
tree g95_sym_type(g95_symbol *);
tree g95_type_spec(g95_typespec *);
tree g95_procedure_type(g95_symbol *);
tree g95_type_for_size(unsigned, int);
tree g95_type_for_mode(enum machine_mode, int);
tree g95_unsigned_type(tree);
tree g95_signed_type(tree);
tree g95_signed_or_unsigned_type(int, tree);
tree g95_get_array_type_bounds(tree, int, tree *, tree *);
tree g95_dummy_arg_type(g95_symbol *, int);

/* trans-const.c  */

tree g95_resize_string_constant(tree, tree);
tree g95_build_string_const(int, char *);
tree g95_build_const(tree, tree);
void g95_conv_constant(g95_se *, g95_expr *);
tree g95_build_int(HOST_WIDE_INT, HOST_WIDE_INT);
tree bi_to_tree(bignum, int);
tree bg_to_tree(bignum, int);

/* trans-types.c */

tree g95_get_element_type(tree);
tree g95_get_typenode(g95_typespec *, int);
void g95_finish_type(tree);

tree g95_result_type(g95_symbol *);
void g95_component_vinfo(g95_component *, variable_info *);
void g95_symbol_vinfo(g95_symbol *, variable_info *);
tree g95_get_descriptor(variable_info *);
tree g95_ts_size(g95_typespec *);
tree g95_get_storage(variable_info *);
void g95_init_character_len(stmtblock_t *, g95_charlen *, tree);

/* trans.c */

tree g95_call_library(tree, char *, ...);
tree g95_add_field(tree, char *, tree);
tree g95_find_field(tree, char *);
void g95_set_locus(stmtblock_t *, g95_locus *);
tree g95_unique_identifier(char *);
void g95_add_modify_expr(stmtblock_t *, tree, tree);
bool g95_post_option(const char **);
void g95_init_block(stmtblock_t *);
void g95_start_block(stmtblock_t *);

tree g95_finish_block(stmtblock_t *);
void g95_set_backend_locus(g95_locus *);
void g95_save_expr(g95_se *);

tree g95_trans_assignment(g95_expr *, g95_expr *);
void g95_init_io(void);
tree g95_library_decl(char *name, tree rettype, int nargs, ...);
void g95_call_temp_alloc(stmtblock_t *, tree, tree);
void g95_call_temp_free(stmtblock_t *, tree);
void g95_init_se(g95_se *, g95_se *);
tree g95_create_var(tree);
tree g95_create_var_np(tree);
tree g95_chainon_list(tree, tree);
tree g95_prepend_list(tree, tree);

tree g95_build_max(tree, tree, tree);
tree g95_build_min(tree, tree, tree);
tree g95_build_and(tree, tree, tree);
tree g95_build_ior(tree, tree, tree);
tree g95_build_lshift(tree, tree, tree);
tree g95_build_rshift(tree, tree, tree);
tree g95_build_abs(tree, tree);
tree g95_build_indirect(tree, tree);
tree g95_build_label(tree);
tree g95_build_goto(tree);
tree g95_build_loop(tree);
tree g95_build_switch(tree, tree);
tree g95_build_cond(tree, tree, tree, tree);
tree g95_build_component(tree, tree, tree);
tree g95_build_array_ref(tree, tree, tree);
tree g95_build_plus(tree, tree, tree);
tree g95_build_minus(tree, tree, tree);
tree g95_build_mult(tree, tree, tree);

tree g95_build_eq(tree, tree);
tree g95_build_ne(tree, tree);
tree g95_build_gt(tree, tree);
tree g95_build_ge(tree, tree);
tree g95_build_lt(tree, tree);
tree g95_build_le(tree, tree);
tree g95_build_andif(tree, tree);
tree g95_build_orif(tree, tree);
tree g95_build_constructor(tree, tree);

tree g95_trans_scalar_assign(g95_se *, g95_se *, bt);
tree g95_conv_string_tmp(g95_se *, tree, tree);
tree g95_addr_expr(tree);
tree g95_array_node(void);
tree g95_dtype_node(void);
tree g95_case_label(tree, tree);

void g95_add_expr_to_block(stmtblock_t *, tree);
void g95_add_block_to_block(stmtblock_t *, stmtblock_t *);
void g95_add_se_expr(g95_se *, tree, tree);
void g95_raise_chains(g95_se *);

/* trans-io.c */

tree g95_trans_open(g95_code *);
tree g95_trans_close(g95_code *);
tree g95_trans_read(g95_code *);
tree g95_trans_write(g95_code *);
tree g95_trans_flush(g95_code *);
tree g95_trans_wait(g95_code *);
tree g95_trans_iolength(g95_code *);
tree g95_trans_backspace(g95_code *);
tree g95_trans_endfile(g95_code *);
tree g95_trans_inquire(g95_code *);
tree g95_trans_rewind(g95_code *);
void g95_conv_format(g95_st_label *);

tree g95_trans_transfer(g95_code *);
tree g95_trans_dt_end(g95_code *);

/* trans-expr.c */

tree g95_trans_pointer_assign(g95_code *);
int g95_is_intrinsic_libcall(g95_expr *);
void g95_conv_function_call(g95_se *, g95_symbol *, g95_actual_arglist *);
tree g95_trans_arglist(g95_actual_arglist *, g95_se *);

/* data.c */

tree g95_generate_data(g95_symbol *);
tree g95_conv_array_initializer(variable_info *, g95_se *);
void g95_start_common(void);
void g95_init_common_var(g95_symbol *, int);
tree g95_data_initializer(int, int);
void g95_default_structure_init(g95_symbol *sym, int);
tree g95_simple_array_init(variable_info *, tree);

/* trans-decl.c */

tree g95_sym_identifier(g95_symbol *, int, char *);
void g95_build_procedure_decl(g95_symbol *);
void g95_add_decl_to_function(tree, g95_symbol *);

tree g95_get_label_decl(g95_st_label *);
void g95_get_symbol_decl(g95_symbol *);
void g95_function_decl(g95_symbol *);
tree g95_build_function_call(tree, tree);
tree g95_build_label_decl(tree);
void g95_create_procedure_variable(g95_symbol *);
tree g95_get_return_label(void);
void g95_build_builtin_decls(void);
void g95_dump_coarray(g95_symtree *, int *);
void g95_generate_procedure(g95_namespace *);
void g95_generate_procedure_variables(g95_namespace *);
tree g95_default_scalar_value(g95_symbol *);
tree g95_initial_array_value(g95_symbol *);
void g95_deallocate_components(g95_symbol *, stmtblock_t *);

/* trans-expr.c */

void g95_conv_expr(g95_se *, g95_expr *);
void g95_conv_expr0(g95_se *, g95_expr *);
void g95_conv_spec_expr(g95_se *, g95_expr *);
void g95_conv_charlen_expr(g95_se *, g95_expr *);
tree g95_conv_char_length(g95_se *, g95_typespec *);
void g95_conv_expr_type(g95_se *, g95_expr *, tree);

tree g95_temp_string(g95_se *, tree);
void g95_reflevel(g95_se *, int);
int g95_stack_variable(tree);

/* trans-common.c */

int g95_element_number(g95_array_ref *, g95_array_spec *);
void g95_trans_common(g95_namespace *);

tree g95_conv_array_ubound(tree, int);
tree g95_conv_array_lbound(tree, int);
void g95_trans_data(g95_namespace *);
void g95_init_common(g95_namespace *);

/* f95-lang.c */

tree g95_truthvalue_conversion(tree);



enum {
    F95_INT1_TYPE,
    F95_INT2_TYPE,
    F95_INT4_TYPE,
    F95_INT8_TYPE,
    F95_INT16_TYPE,
    F95_REAL4_TYPE,
    F95_REAL8_TYPE,
    F95_REAL10_TYPE,
    F95_REAL16_TYPE,
    F95_COMPLEX4_TYPE,
    F95_COMPLEX8_TYPE,
    F95_COMPLEX10_TYPE,
    F95_COMPLEX16_TYPE,
    F95_LOGICAL1_TYPE,
    F95_LOGICAL2_TYPE,
    F95_LOGICAL4_TYPE,
    F95_LOGICAL8_TYPE,
    F95_LOGICAL16_TYPE,
    F95_CHARACTER1_TYPE,
    NUM_F95_TYPES
};

#define G95_DTYPE_RANK_MASK 0x07
#define G95_DTYPE_TYPE_SHIFT 3
#define G95_DTYPE_TYPE_MASK 0x38
#define G95_DTYPE_SIZE_SHIFT 6

extern tree g95_type_nodes[NUM_F95_TYPES];
extern tree g95_default_integer;
extern tree g95_pointer_integer;
extern tree ppvoid_type_node;
extern tree pvoid_type_node;
extern tree integer_zero_unsigned;
extern tree g95_locus_file, g95_locus_line;
extern tree pfunc_type_node;
extern tree g95_junk_stat;

extern tree g95_empty_array;
extern tree pchar_type_node;
extern tree pchar_type_node_a;
extern tree g95_result_var_decl;
extern tree null_string_node;
extern tree g95_string_len;
extern tree empty_stmt_node;
extern tree g95_space;


extern g95_frame_t g95_frame_type;


#define g95_real4_type_node  g95_type_nodes[F95_REAL4_TYPE]
#define g95_real8_type_node  g95_type_nodes[F95_REAL8_TYPE]
#define g95_real10_type_node g95_type_nodes[F95_REAL10_TYPE]
#define g95_real16_type_node g95_type_nodes[F95_REAL16_TYPE]

#define g95_int1_type_node  g95_type_nodes[F95_INT1_TYPE]
#define g95_int2_type_node  g95_type_nodes[F95_INT2_TYPE]
#define g95_int4_type_node  g95_type_nodes[F95_INT4_TYPE]
#define g95_int8_type_node  g95_type_nodes[F95_INT8_TYPE]
#define g95_int16_type_node g95_type_nodes[F95_INT16_TYPE]

#define g95_complex4_type_node  g95_type_nodes[F95_COMPLEX4_TYPE]
#define g95_complex8_type_node  g95_type_nodes[F95_COMPLEX8_TYPE]
#define g95_complex10_type_node g95_type_nodes[F95_COMPELX10_TYPE]
#define g95_complex16_type_node g95_type_nodes[F95_COMPLEX16_TYPE]

#define g95_logical1_type_node  g95_type_nodes[F95_LOGICAL1_TYPE]
#define g95_logical2_type_node  g95_type_nodes[F95_LOGICAL2_TYPE]
#define g95_logical4_type_node  g95_type_nodes[F95_LOGICAL4_TYPE]
#define g95_logical8_type_node  g95_type_nodes[F95_LOGICAL8_TYPE]
#define g95_logical16_type_node g95_type_nodes[F95_LOGICAL16_TYPE]

#define g95_character1_type_node g95_type_nodes[F95_CHARACTER1_TYPE]

/* be-function.c */
void g95_convert_function_code(g95_namespace *);


/* somewhere! */
tree pushdecl(tree);
void pushlevel(int);
tree poplevel(int, int, int);
void expand_function_body(tree, int);
tree getdecls(void);

bool g95_post_options(const char **);

tree builtin_function(const char *, tree, int, enum built_in_class class,
		      const char *, tree);

/* True if node is an integer constant. */
#define INTEGER_CST_P(node) (TREE_CODE(node) == INTEGER_CST)

/* G95-specific declaration information. */

struct lang_type GTY (()) { int none; };
struct lang_decl GTY (()) { int none; };

/* Nonzero if the type is a descriptor (character or array) */

#define G95_DESCRIPTOR_P(node)  TREE_LANG_FLAG_0(node)
#define G95_CONSTANT_DESC(node) TREE_LANG_FLAG_1(node)
#define G95_ARRAY(node)         TREE_LANG_FLAG_2(node)
#define G95_DTYPE(node)         TREE_LANG_FLAG_3(node)
#define G95_COARRAY(node)       TREE_LANG_FLAG_4(node)

#define G95_CONSTANT_P(X) \
  (TREE_CODE(X) == INTEGER_CST || \
   TREE_CODE(X) == REAL_CST || \
   TREE_CODE(X) == COMPLEX_CST)

#define STRING_P(x) (TREE_CODE(x) == STRING_CST || (x) == null_string_node)

