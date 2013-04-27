
/* Internal module module
 * Copyright (C) 2000-2009 Free Software Foundation, Inc.
 * Contributed by Andy Vaught

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


#include "g95.h"
#include <string.h>



/* g95_internal_derived()-- Return nonzero if typespec is the internal
 * derived type. */

int g95_internal_derived(g95_typespec *ts, internal_type type) {

    return ts->type == BT_DERIVED && ts->derived->attr.itype == type;
}



/* derived parameter()-- Create a parameter of a derived type.
 * Returns nonzero if the value was inserted into the module. */

static int derived_parameter(char *name, internal_type itype,
			     internal_value ivalue) {
g95_expr *v;

    v = g95_get_expr();
    v->ts.type = BT_DERIVED;
    v->ts.derived = g95_current_ns->itypes[itype];

    v->type = EXPR_STRUCTURE;
    v->symbol = g95_current_ns->itypes[itype];
    v->where = g95_current_locus;
    v->value.constructor.c = NULL;
    v->value.constructor.ivalue = ivalue;

    return g95_module_parameter(name, v);
}



/* char_parameter()-- Create a single-character parameter */

static void char_parameter(char *name, char value) {
g95_expr *v;

    v = g95_char_expr(1, g95_default_character_kind(), NULL);
    v->value.character.string[0] = value;

    g95_module_parameter(name, v);
}



/* integer_parameter()-- Create an integer parameter */

static void integer_parameter(char *name, int value) {

    g95_module_parameter(name, g95_int_expr(value));
}



/* use_iso_fortan_env()-- Use the ISO_FORTRAN_ENV module */

static void use_iso_fortran_env(void) {

    integer_parameter("character_storage_size", 8);
    integer_parameter("error_unit", 0);
    integer_parameter("file_storage_size", 8);
    integer_parameter("input_unit", 5);
    integer_parameter("iostat_end", -1);
    integer_parameter("iostat_eor", -2);
    integer_parameter("numeric_storage_size", 8*g95_default_integer_kind(0));
    integer_parameter("output_unit", 6);
    integer_parameter("stat_stopped_image", 217);
}



/* use_iso_c_binding()-- Use the ISO_C_BINDING module */

static void use_iso_c_binding(void) {

    integer_parameter("c_int",            sizeof(int));
    integer_parameter("c_short",          sizeof(short));
    integer_parameter("c_long",           sizeof(long));
    integer_parameter("c_long_long",      sizeof(long long));
    integer_parameter("c_signed_char",    sizeof(signed char));
    integer_parameter("c_size_t",         SIZEOF_SIZE_T);

    integer_parameter("c_int8_t",         1);
    integer_parameter("c_int16_t",        2);
    integer_parameter("c_int32_t",        4);
    integer_parameter("c_int64_t",        8);

    integer_parameter("c_int_least8_t",   1);
    integer_parameter("c_int_least16_t",  2);
    integer_parameter("c_int_least32_t",  4);
    integer_parameter("c_int_least64_t",  8);

    integer_parameter("c_int_fast8_t",    1);
    integer_parameter("c_int_fast16_t",   SIZEOF_SIZE_T);
    integer_parameter("c_int_fast32_t",   SIZEOF_SIZE_T);
    integer_parameter("c_int_fast64_t",   8);

    integer_parameter("c_intmax_t",       g95_default_integer_kind(0));
    integer_parameter("c_intptr_t",       SIZEOF_VOID_P);

    integer_parameter("c_float",          g95_default_real_kind(0));
    integer_parameter("c_double",         g95_default_double_kind());
    integer_parameter("c_long_double",    -1);

    integer_parameter("c_float_complex",        g95_default_real_kind(0));
    integer_parameter("c_double_complex",       g95_default_double_kind());
    integer_parameter("c_long_double_complex",  -1);

    integer_parameter("c_bool",           1);

    integer_parameter("c_char",           g95_default_character_kind());

    char_parameter("c_null_char",        '\0');
    char_parameter("c_alert",            '\a');
    char_parameter("c_backspace",        '\b');
    char_parameter("c_form_feed",        '\f');
    char_parameter("c_new_line",         '\n');
    char_parameter("c_carriage_return",  '\r');
    char_parameter("c_horizontal_tab",   '\t');
    char_parameter("c_vertical_tab",     '\v');

    g95_module_type("c_ptr",    ITYPE_C_PTR);
    g95_module_type("c_funptr", ITYPE_C_FUNPTR);

    derived_parameter("c_null_ptr",    ITYPE_C_PTR,    IVALUE_C_NULL_PTR);
    derived_parameter("c_null_funptr", ITYPE_C_FUNPTR, IVALUE_C_NULL_FUNPTR);

    g95_module_proc("c_loc",            IPROC_C_LOC);
    g95_module_proc("c_funloc",         IPROC_C_FUNLOC);
    g95_module_proc("c_associated",     IPROC_C_ASSOCIATED);
    g95_module_proc("c_f_pointer",      IPROC_C_F_POINTER);
    g95_module_proc("c_f_procpointer",  IPROC_C_F_PROCPOINTER);
}



/* use_ieee_exceptions()-- Use the IEEE_EXCEPTIONS module */

static void use_ieee_exceptions(void) {

    g95_module_type("ieee_flag_type",  ITYPE_IEEE_FLAG);
    derived_parameter("ieee_overflow", ITYPE_IEEE_FLAG, IVALUE_OVERFLOW);

    derived_parameter("ieee_divide_by_zero", ITYPE_IEEE_FLAG,
		      IVALUE_DIVIDE_BY_ZERO);

    derived_parameter("ieee_invalid",   ITYPE_IEEE_FLAG, IVALUE_INVALID);
    derived_parameter("ieee_underflow", ITYPE_IEEE_FLAG, IVALUE_UNDERFLOW);
    derived_parameter("ieee_inexact",   ITYPE_IEEE_FLAG, IVALUE_INEXACT);

    g95_module_type("ieee_status_type", ITYPE_IEEE_STATUS);

    g95_module_proc("ieee_support_flag",     IPROC_IEEE_SUPPORT_FLAG);
    g95_module_proc("ieee_support_halting",  IPROC_IEEE_SUPPORT_HALTING);
    g95_module_proc("ieee_get_flag",         IPROC_IEEE_GET_FLAG);
    g95_module_proc("ieee_get_halting_mode", IPROC_IEEE_GET_HALTING_MODE);

    g95_module_proc("ieee_set_flag",         IPROC_IEEE_SET_FLAG);
    g95_module_proc("ieee_set_halting_mode", IPROC_IEEE_SET_HALTING_MODE);
    g95_module_proc("ieee_get_status",       IPROC_IEEE_GET_STATUS);
    g95_module_proc("ieee_set_status",       IPROC_IEEE_SET_STATUS);
}



/* g95_ieee_class_compare()-- Return nonzero if the given operator is
 * defined in the current or a parent module. */

int g95_ieee_class_compare(int op) {
g95_namespace *ns;

    for(ns=g95_current_ns; ns; ns=ns->parent)
	switch(op) {
	case INTRINSIC_EQ:
	    if (ns->ieee_class_eq)
		return 1;

	    break;

	case INTRINSIC_NE:
	    if (ns->ieee_class_ne)
		return 1;

	    break;
	}

    return 0;
}



/* use_ieee_arithmetic()-- Use the IEEE_ARITHMETIC module */

static void use_ieee_arithemetic(void) {

    g95_module_type("ieee_class_type", ITYPE_IEEE_CLASS);

    derived_parameter("ieee_signaling_nan",
		      ITYPE_IEEE_CLASS, IVALUE_SIGNALING_NAN);

    derived_parameter("ieee_quiet_nan", ITYPE_IEEE_CLASS, IVALUE_QUIET_NAN);

    derived_parameter("ieee_negative_inf",
		      ITYPE_IEEE_CLASS, IVALUE_NEGATIVE_INF);

    derived_parameter("ieee_negative_denormal",
		      ITYPE_IEEE_CLASS, IVALUE_NEGATIVE_DENORMAL);

    derived_parameter("ieee_negative_zero",
		      ITYPE_IEEE_CLASS, IVALUE_NEGATIVE_ZERO);

    derived_parameter("ieee_negative_normal",
		      ITYPE_IEEE_CLASS, IVALUE_NEGATIVE_NORMAL);

    derived_parameter("ieee_positive_inf",
		      ITYPE_IEEE_CLASS, IVALUE_POSITIVE_INF);

    derived_parameter("ieee_positive_denormal",
		      ITYPE_IEEE_CLASS, IVALUE_POSITIVE_DENORMAL);

    derived_parameter("ieee_positive_zero",
		      ITYPE_IEEE_CLASS, IVALUE_POSITIVE_ZERO);

    derived_parameter("ieee_positive_normal",
		      ITYPE_IEEE_CLASS, IVALUE_POSITIVE_NORMAL);

    derived_parameter("ieee_other_value",
		      ITYPE_IEEE_CLASS, IVALUE_OTHER_VALUE);

    g95_module_type("ieee_round_type", ITYPE_IEEE_ROUND);
    derived_parameter("ieee_nearest",  ITYPE_IEEE_ROUND, IVALUE_ROUND_NEAREST);
    derived_parameter("ieee_to_zero",  ITYPE_IEEE_ROUND, IVALUE_ROUND_TO_ZERO);
    derived_parameter("ieee_up",       ITYPE_IEEE_ROUND, IVALUE_ROUND_UP);
    derived_parameter("ieee_down",     ITYPE_IEEE_ROUND, IVALUE_ROUND_DOWN);

    g95_module_proc("ieee_support_datatype", IPROC_IEEE_SUPPORT_DATATYPE);
    g95_module_proc("ieee_support_denormal", IPROC_IEEE_SUPPORT_DENORMAL);
    g95_module_proc("ieee_support_divide",   IPROC_IEEE_SUPPORT_DIVIDE);
    g95_module_proc("ieee_support_inf",      IPROC_IEEE_SUPPORT_INF);
    g95_module_proc("ieee_support_nan",      IPROC_IEEE_SUPPORT_NAN);
    g95_module_proc("ieee_support_rounding", IPROC_IEEE_SUPPORT_ROUNDING);
    g95_module_proc("ieee_support_sqrt",     IPROC_IEEE_SUPPORT_SQRT);
    g95_module_proc("ieee_support_standard", IPROC_IEEE_SUPPORT_STANDARD);
    g95_module_proc("ieee_support_underflow_control",
		    IPROC_IEEE_SUPPORT_UNDERFLOW_CONTROL);

    g95_module_proc("ieee_class",       IPROC_IEEE_CLASS);
    g95_module_proc("ieee_copy_sign",   IPROC_IEEE_COPY_SIGN);
    g95_module_proc("ieee_is_finite",   IPROC_IEEE_IS_FINITE);
    g95_module_proc("ieee_is_nan",      IPROC_IEEE_IS_NAN);
    g95_module_proc("ieee_is_negative", IPROC_IEEE_IS_NEGATIVE);
    g95_module_proc("ieee_is_normal",   IPROC_IEEE_IS_NORMAL);
    g95_module_proc("ieee_logb",        IPROC_IEEE_LOGB);
    g95_module_proc("ieee_next_after",  IPROC_IEEE_NEXT_AFTER);
    g95_module_proc("ieee_rem",         IPROC_IEEE_REM);
    g95_module_proc("ieee_rint",        IPROC_IEEE_RINT);
    g95_module_proc("ieee_scalb",       IPROC_IEEE_SCALB);
    g95_module_proc("ieee_unordered",   IPROC_IEEE_UNORDERED);
    g95_module_proc("ieee_value",       IPROC_IEEE_VALUE);
    g95_module_proc("ieee_get_rounding_mode",  IPROC_IEEE_GET_ROUNDING_MODE);
    g95_module_proc("ieee_get_underflow_mode", IPROC_IEEE_GET_UNDERFLOW_MODE);
    g95_module_proc("ieee_set_rounding_mode",  IPROC_IEEE_SET_ROUNDING_MODE);
    g95_module_proc("ieee_set_underflow_mode", IPROC_IEEE_SET_UNDERFLOW_MODE);
    g95_module_proc("ieee_selected_real_kind", IPROC_IEEE_SELECTED_REAL_KIND);

    g95_module_operator(INTRINSIC_EQ);
    g95_module_operator(INTRINSIC_NE);

    use_ieee_exceptions();
}



/* use_ieee_features()-- Use the IEEE_FEATURES module */

static void use_ieee_features(void) {

    g95_module_type("ieee_features_type", ITYPE_IEEE_FEATURES);

    if (derived_parameter("ieee_datatype", ITYPE_IEEE_FEATURES,
			  IVALUES_DATATYPE))
	g95_current_ns->ieee_datatype = 1;

    if (derived_parameter("ieee_denormal", ITYPE_IEEE_FEATURES,
			  IVALUES_DENORMAL))
	g95_current_ns->ieee_denormal = 1;

    if (derived_parameter("ieee_divide", ITYPE_IEEE_FEATURES, IVALUES_DIVIDE))
	g95_current_ns->ieee_divide = 1;

    if (derived_parameter("ieee_halting", ITYPE_IEEE_FEATURES, IVALUES_HALTING))
	g95_current_ns->ieee_halting = 1;

    if (derived_parameter("ieee_inf", ITYPE_IEEE_FEATURES, IVALUES_INF))
	g95_current_ns->ieee_inf = 1;

    if (derived_parameter("ieee_nan", ITYPE_IEEE_FEATURES, IVALUES_NAN))
	g95_current_ns->ieee_nan = 1;

    if (derived_parameter("ieee_rounding", ITYPE_IEEE_FEATURES,
			  IVALUES_ROUNDING))
	g95_current_ns->ieee_rounding = 1;

    if (derived_parameter("ieee_sqrt", ITYPE_IEEE_FEATURES, IVALUES_SQRT))
	g95_current_ns->ieee_sqrt = 1;

    if (derived_parameter("ieee_inexact_flag",
			  ITYPE_IEEE_FEATURES, IVALUES_INEXACT_FLAG))
	g95_current_ns->ieee_inexact = 1;

    if (derived_parameter("ieee_invalid_flag",
			  ITYPE_IEEE_FEATURES, IVALUES_INVALID_FLAG))
	g95_current_ns->ieee_invalid = 1;

    if (derived_parameter("ieee_underflow_flag",
			  ITYPE_IEEE_FEATURES, IVALUES_UNDERFLOW_FLAG))
	g95_current_ns->ieee_underflow = 1;
}



/* g95_iproc_name()-- Given an internal procedure name, return the
 * string version. */

char *g95_iproc_name(internal_proc p) {
char *n;

    switch(p) {
    case IPROC_C_LOC:                   n = "C_LOC";                  break;
    case IPROC_C_FUNLOC:                n = "C_FUNLOC";               break;
    case IPROC_C_ASSOCIATED:            n = "C_ASSOCIATED";           break;
    case IPROC_C_F_POINTER:             n = "C_F_POINTER";            break;
    case IPROC_C_F_PROCPOINTER:         n = "C_F_PROCPOINTER";        break;
    case IPROC_IEEE_SUPPORT_FLAG:       n = "IEEE_SUPPORT_HALTING";   break;
    case IPROC_IEEE_SUPPORT_HALTING:    n = "IEEE_SUPPORT_HALTING";   break;
    case IPROC_IEEE_GET_FLAG:           n = "IEEE_GET_FLAG";          break;
    case IPROC_IEEE_GET_HALTING_MODE:   n = "IEEE_GET_HALTING_MODE:"; break;
    case IPROC_IEEE_SET_FLAG:           n = "IEEE_SET_FLAG";          break;
    case IPROC_IEEE_SET_HALTING_MODE:   n = "IEEE_SET_HALTING_MODE";  break;
    case IPROC_IEEE_GET_STATUS:         n = "IEEE_GET_STATUS";        break;
    case IPROC_IEEE_SET_STATUS:         n = "IEEE_SET_STATUS";        break;
    case IPROC_IEEE_SUPPORT_DATATYPE:   n = "IEEE_SUPPORT_DATATYPE";  break;
    case IPROC_IEEE_SUPPORT_DENORMAL:   n = "IEEE_SUPPORT_DENORMAL";  break;
    case IPROC_IEEE_SUPPORT_DIVIDE:     n = "IEEE_SUPPORT_DIVIDE";    break;
    case IPROC_IEEE_SUPPORT_INF:        n = "IEEE_SUPPORT_INF";       break;
    case IPROC_IEEE_SUPPORT_NAN:        n = "IEEE_SUPPORT_NAN";       break;
    case IPROC_IEEE_SUPPORT_ROUNDING:   n = "IEEE_SUPPORT_ROUNDING";  break;
    case IPROC_IEEE_SUPPORT_SQRT:       n = "IEEE_SUPPORT_SQRT";      break;
    case IPROC_IEEE_SUPPORT_STANDARD:   n = "IEEE_SUPPORT_STANDARD";  break;

    case IPROC_IEEE_SUPPORT_UNDERFLOW_CONTROL:
	n = "IEEE_SUPPORT_UNDERFLOW_CONTROL"; break;

    case IPROC_IEEE_CLASS:              n = "IEEE_CLASS";              break;
    case IPROC_IEEE_COPY_SIGN:          n = "IEEE_COPY_SIGN";          break;
    case IPROC_IEEE_IS_FINITE:          n = "IEEE_IS_FINITE";          break;
    case IPROC_IEEE_IS_NAN:             n = "IEEE_IS_NAN";             break;
    case IPROC_IEEE_IS_NEGATIVE:        n = "IEEE_IS_NEGATIVE:";       break;
    case IPROC_IEEE_IS_NORMAL:          n = "IEEE_IS_NORMAL";          break;
    case IPROC_IEEE_LOGB:               n = "IEEE_LOGB";               break;
    case IPROC_IEEE_NEXT_AFTER:         n = "IEEE_NEXT_AFTER";         break;
    case IPROC_IEEE_REM:                n = "IEEE_REM";                break;
    case IPROC_IEEE_RINT:               n = "IEEE_RINT";               break;
    case IPROC_IEEE_SCALB:              n = "IEEE_SCALB";              break;
    case IPROC_IEEE_UNORDERED:          n = "IEEE_UNORDERED";          break;
    case IPROC_IEEE_VALUE:              n = "IEEE_VALUE";              break;
    case IPROC_IEEE_GET_ROUNDING_MODE:  n = "IEEE_GET_ROUNDING_MODE";  break;
    case IPROC_IEEE_GET_UNDERFLOW_MODE: n = "IEEE_GET_UNDERFLOW_MODE"; break;
    case IPROC_IEEE_SET_ROUNDING_MODE:  n = "IEEE_SET_ROUNDING_MODE";  break;
    case IPROC_IEEE_SET_UNDERFLOW_MODE: n = "IEEE_SET_UNDERFLOW_MODE"; break;
    case IPROC_IEEE_SELECTED_REAL_KIND: n = "IEEE_SELECTED_REAL_KIND"; break;
    default:
	g95_internal_error("g95_iproc_name(): Bad procedure");
	n = NULL;
    }

    return n;
}



/* g95_iproc_purity()-- Return the purity of an intrinsic procedure */

int g95_iproc_purity(internal_proc p) {
int pure;

    switch(p) {
    case IPROC_C_LOC:                           pure = 1;  break;
    case IPROC_C_FUNLOC:                        pure = 1;  break;
    case IPROC_C_ASSOCIATED:                    pure = 1;  break;
    case IPROC_C_F_POINTER:                     pure = 0;  break;
    case IPROC_C_F_PROCPOINTER:                 pure = 0;  break;
    case IPROC_IEEE_SUPPORT_FLAG:               pure = 1;  break;
    case IPROC_IEEE_SUPPORT_HALTING:            pure = 1;  break;
    case IPROC_IEEE_GET_FLAG:                   pure = 1;  break;
    case IPROC_IEEE_GET_HALTING_MODE:           pure = 1;  break;
    case IPROC_IEEE_SET_FLAG:                   pure = 1;  break;
    case IPROC_IEEE_SET_HALTING_MODE:           pure = 1;  break;
    case IPROC_IEEE_GET_STATUS:                 pure = 1;  break;
    case IPROC_IEEE_SET_STATUS:                 pure = 0;  break;
    case IPROC_IEEE_SUPPORT_DATATYPE:           pure = 1;  break;
    case IPROC_IEEE_SUPPORT_DENORMAL:           pure = 1;  break;
    case IPROC_IEEE_SUPPORT_DIVIDE:             pure = 1;  break;
    case IPROC_IEEE_SUPPORT_INF:                pure = 1;  break;
    case IPROC_IEEE_SUPPORT_NAN:                pure = 1;  break;
    case IPROC_IEEE_SUPPORT_ROUNDING:           pure = 1;  break;
    case IPROC_IEEE_SUPPORT_SQRT:               pure = 1;  break;
    case IPROC_IEEE_SUPPORT_STANDARD:           pure = 1;  break;
    case IPROC_IEEE_SUPPORT_UNDERFLOW_CONTROL:  pure = 1;  break;
    case IPROC_IEEE_CLASS:                      pure = 1;  break;
    case IPROC_IEEE_COPY_SIGN:                  pure = 1;  break;
    case IPROC_IEEE_IS_FINITE:                  pure = 1;  break;
    case IPROC_IEEE_IS_NAN:                     pure = 1;  break;
    case IPROC_IEEE_IS_NEGATIVE:                pure = 1;  break;
    case IPROC_IEEE_IS_NORMAL:                  pure = 1;  break;
    case IPROC_IEEE_LOGB:                       pure = 1;  break;
    case IPROC_IEEE_NEXT_AFTER:                 pure = 1;  break;
    case IPROC_IEEE_REM:                        pure = 1;  break;
    case IPROC_IEEE_RINT:                       pure = 1;  break;
    case IPROC_IEEE_SCALB:                      pure = 1;  break;
    case IPROC_IEEE_UNORDERED:                  pure = 1;  break;
    case IPROC_IEEE_VALUE:                      pure = 1;  break;
    case IPROC_IEEE_GET_ROUNDING_MODE:          pure = 0;  break;
    case IPROC_IEEE_GET_UNDERFLOW_MODE:         pure = 0;  break;
    case IPROC_IEEE_SET_ROUNDING_MODE:          pure = 0;  break;
    case IPROC_IEEE_SET_UNDERFLOW_MODE:         pure = 0;  break;
    case IPROC_IEEE_SELECTED_REAL_KIND:         pure = 1;  break;

    default:
	g95_internal_error("g95_iproc_purity(): Bad procedure");
	pure = 0;
    }

    return pure;
}



/* use_g95()-- Use the g95 internal library module */

static void use_g95(void) {

}



/* g95_use_internal()-- Use an internal module.  Returns nonzero if
 * the module isn't found. */

int g95_use_internal(char *name) {

    if (strcmp(name, "iso_fortran_env") == 0)
	use_iso_fortran_env();

    else if (strcmp(name, "iso_c_binding") == 0)
	use_iso_c_binding();

    else if (strcmp(name, "ieee_exceptions") == 0)
	use_ieee_exceptions();

    else if (strcmp(name, "ieee_arithmetic") == 0)
	use_ieee_arithemetic();

    else if (strcmp(name, "ieee_features") == 0)
	use_ieee_features();

    else if (strcmp(name, "g95") == 0)
	use_g95();

    else
	return 1;

    return 0;
}



/* g95_check_ieee_datatype()-- Check for IEEE_DATATYPE in this and
 * parent namespaces. */

int g95_check_ieee_datatype(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_datatype)
	    return 1;

    return 0;
}


/* g95_check_ieee_denormal()-- Check for IEEE_DENORMAL in this and
 * parent namespaces. */

int g95_check_ieee_denormal(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_denormal)
	    return 1;

    return 0;
}


/* g95_check_ieee_divide()-- Check for IEEE_DIVIDE in this and parent
 * namespaces. */

int g95_check_ieee_divide(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_divide)
	    return 1;

    return 0;
}


/* g95_check_ieee_halting()-- Check for IEEE_HALTING in this and parent
 * namespaces. */

int g95_check_ieee_halting(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_halting)
	    return 1;

    return 0;
}


/* g95_check_ieee_inf()-- Check for IEEE_INF in this and parent
 * namespaces. */

int g95_check_ieee_inf(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_inf)
	    return 1;

    return 0;
}


/* g95_check_ieee_nan()-- Check for IEEE_NAN in this and parent
 * namespaces. */

int g95_check_ieee_nan(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_nan)
	    return 1;

    return 0;
}


/* g95_check_ieee_rounding()-- Check for ieee_rounding in this and
 * parent namespaces. */

int g95_check_ieee_rounding(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_rounding)
	    return 1;

    return 0;
}


/* g95_check_ieee_sqrt()-- Check for IEEE_SQRT in this and parent
 * namespaces. */

int g95_check_ieee_sqrt(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_sqrt)
	    return 1;

    return 0;
}


/* g95_check_ieee_inexact()-- Check for IEEE_INEXACT_FLAG in this and
 * parent namespaces. */

int g95_check_ieee_inexact(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_inexact)
	    return 1;

    return 0;
}



/* g95_check_ieee_invalid()-- Check for IEEE_INVALID_FLAG in this and
 * parent namespaces. */

int g95_check_ieee_invalid(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_invalid)
	    return 1;

    return 0;
}


/* g95_check_ieee_underflow()-- Check for IEEE_UNDERFLOW_FLAG in this
 * and parent namespaces. */

int g95_check_ieee_underflow(g95_namespace *ns) {

    for(; ns!=NULL; ns=ns->parent)
	if (ns->ieee_underflow)
	    return 1;

    return 0;
}

