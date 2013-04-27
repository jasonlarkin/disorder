
/* Command line option related things
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

/* options.c-- Parse and display command line options */

#include <stdlib.h>


#ifdef IN_GCC

#include "options.h"
#include "trans.h"

#define GCC_TREE_H 1

#if TARGET_GCC_VERSION >= 410
int warn_return_type;
#endif

#else

#include "g95.h"
#define CL_F95 0
#include <string.h>

#endif

g95_option_t g95_option;



/* set_Wall()-- Set the options for -Wall.  */

#ifdef IN_GCC

static void set_Wall(void) {

    g95_option.line_truncation = 1;
    g95_option.unused_label = 1;
    g95_option.unused_vars = 1;
    g95_option.unused_target = 1;
    g95_option.unset_vars = 1;
    g95_option.prec_loss = 1;
    warn_uninitialized = 2;

    warn_return_type = 1;
    warn_switch = 1;
}



/* g95_set_extra()-- Set things that aren't implied by -Wall */

void g95_set_extra(void) {

    g95_option.obsolescent = 1;
    g95_option.unused_module_vars = 1;
    g95_option.unused_module_procs = 1;
    g95_option.unused_internal_procs = 1;
    g95_option.unused_parameter = 1;
    g95_option.unused_types = 1;
    g95_option.missing_intent = 1;
    g95_option.implicit_interface = 1;
}

#endif



/* init_options()-- Initialize the options structure */

unsigned g95_init_options(unsigned argc, const char *argv[]) {

    memset(&g95_option, '\0', sizeof(g95_option));

    g95_option.fixed_line_length = 72;
    g95_option.form = FORM_UNKNOWN;
    g95_option.q_kind = g95_default_double_kind();
    g95_option.l1 = g95_default_logical_kind();
    g95_option.max_frame_size = 250000;
    g95_option.symbol_len = G95_MAX_SYMBOL_LEN;
    g95_option.cpp = -1;
    g95_option.short_circuit = 1;
    g95_option.traditional = 1;
    g95_option.globals = 1;
    g95_option.no_backslash = HAVE_WINDOWS;

    argc = 0;
    argv = NULL;

#ifdef IN_GCC
    flag_errno_math = 0;
#endif

#if STD_F
    g95_option.fmode = 96;
    g95_option.symbol_len = 31;
    set_Wall();
    g95_option.tr15581 = 1;
    g95_option.bounds_check = 1;
    g95_option.real_init = REAL_INIT_NAN;
    g95_option.trace = TRACE_FULL;
#endif

    return CL_F95;
}



/* g95_options_done() -- Cleanup options stuff. */

void g95_options_done(void) {
g95_warning_list *n;

    while(g95_option.nowarn != NULL) { 
	n = g95_option.nowarn->next;
	g95_free(g95_option.nowarn);

	g95_option.nowarn = n;
    }
}



/* set_nowarn()-- Process a -Wno= option (which has already been
 * seen). */

static void set_nowarn(const char *p) {
g95_warning_list *n;

    do {
	if (*p < '0' || *p > '9')
	    break;

	n = g95_getmem(sizeof(g95_warning_list));
	n->warning = atoi(p);

	n->next = g95_option.nowarn;
	g95_option.nowarn = n;

	while(*p >= '0' && *p <= '9')
	    p++;

    } while(*p++ == ',');
}



/* module_path()-- Add a path for writing modules. */

static void module_path(const char *option) {

    if (g95_option.module_dir != NULL)
	g95_fatal_error("Only one -fmod= option allowed\n");

    if (option[0] == '\0')
	g95_fatal_error("Directory required after -fmod=\n");

    g95_option.module_dir = (char *) g95_getmem(strlen(option)+2);

    strcpy(g95_option.module_dir, option);
    strcat(g95_option.module_dir, "/");
}



/* g95_check_options()-- Make sure that options are consistent. */

void g95_check_options(void) {
char *o1, *o2;

    o1 = o2 = NULL;

    if (g95_option.deps) {
	if (o1 == NULL)
	    o1 = "-E";
	else
	    o2 = "-E";
    }

    if (g95_option.preprocess_only) {
	if (o1 == NULL)
	    o1 = "-M";
	else
	    o2 = "-M";
    }

    if (g95_option.c_binding) {
	if (o1 == NULL)
	    o1 = "-fc-binding";
	else
	    o2 = "-fc-binding";
    }

    if (o2 != NULL)
	g95_fatal_error("The %s and %s options conflict", o1, o2);
}



/* add_path()-- adds path to the list of include directories. */

static void add_path(const char *path) {
g95_directorylist *dir, **list;
const char *p;

    list = &g95_option.include_dirs;

    p = path;
    while (*p == ' ' || *p == '\t') /* someone might do 'g95 "-I include"' */
	if (*p++ == '\0')
	    return;

    dir = *list;
    if (!dir) {
	dir = *list = g95_getmem(sizeof(g95_directorylist));
    } else {
	while(dir->next)
	    dir = dir->next;

	dir->next = g95_getmem(sizeof(g95_directorylist));
	dir = dir->next;
    }

    dir->next = NULL;
    dir->path = g95_getmem(strlen(p)+2);

    strcpy(dir->path, p);
    strcat(dir->path, "/");     /* make '/' last character */
}



#ifdef IN_GCC

#include "options.h"


/* set_error_list()-- Process a -Werror= option (which has already
 * been seen). */

static void set_error_list(const char *p) {
g95_warning_list *n;

    do {
	if (*p < '0' || *p > '9')
	    break;

	n = g95_getmem(sizeof(g95_warning_list));
	n->warning = atoi(p);

	n->next = g95_option.error_list;
	g95_option.error_list = n;

	while(*p >= '0' && *p <= '9')
	    p++;

    } while(*p++ == ',');
}



int g95_handle_arg(size_t scode, const char *arg, int value) {
enum opt_code code;
int r;

    r = 1;
    code = (enum opt_code) scode;

    if (code == N_OPTS)
	return 1;

    switch(code) {
    case OPT_arch:
	break;

    case OPT_cpp:
	g95_option.cpp = value;
	break;

    case OPT_D:
	g95_define_cpp_macro((char *) arg, 1);
	break;

    case OPT_d8:
	g95_option.default_integer = 8;
	g95_option.r_value = 8;
	break;

    case OPT_E:
	g95_option.preprocess_only = value;
	break;

    case OPT_fbackslash:
	g95_option.no_backslash = !value;
	break;

    case OPT_fbounds_check:
	g95_option.bounds_check = value;
	break;

    case OPT_fc_binding:
	g95_option.c_binding = value;
	break;

    case OPT_fcase_upper:
	g95_option.case_upper = value;
	break;

    case OPT_fdollar_ok:
	g95_option.dollar = value;
	break;

    case OPT_fd_comment:
	g95_option.d_comment = value;
	break;

    case OPT_fendian_:
	if (strcasecmp(arg, "big") == 0)
	    g95_option.endian = 1;

	else if (strcasecmp(arg, "little") == 0)
	    g95_option.endian = 2;

	else
	    g95_fatal_error("Bad value for -fendian");

	break;

    case OPT_ffixed_form:
	g95_option.form = FORM_FIXED;
	break;

    case OPT_ffixed_line_length_80:
	g95_option.fixed_line_length = 80;
	break;

    case OPT_ffixed_line_length_132:
	g95_option.fixed_line_length = 132;
	break;

    case OPT_ffree_form:
	g95_option.form = FORM_FREE;
	break;

    case OPT_ffree_line_length_huge:
	g95_option.huge_line = 1;
	break;

    case OPT_fimplicit_none:
	g95_option.implicit_none = value;
	break;

    case OPT_fintrinsic_extensions:
	g95_option.intrinsic_extensions = value;
	break;

    case OPT_fintrinsic_extensions_:
	g95_option.intrinsic_extensions = 1;
	g95_option.intrinsics = (char *) arg;
	break;

    case OPT_fleading_underscore:
	g95_option.leading_underscore = value;
	flag_leading_underscore = !value;
	break;

    case OPT_max_frame_size_:
	g95_option.max_frame_size = value;
	break;

    case OPT_fmod_:
	module_path(arg);
	add_path(arg);
	break;

    case OPT_fmodule_private:
	g95_option.module_access_private = value;
	break;

    case OPT_fmultiple_save:
	g95_option.multiple_save = value;
	break;

    case OPT_fone_error:
	g95_option.one_error = value;
	break;

    case OPT_fonetrip:
	g95_option.onetrip = value;
	break;

    case OPT_freal_loops:
	g95_option.real_loops = value;
	break;

    case OPT_fpack_derived:
	g95_option.pack_derived = value;
	break;

    case OPT_fqkind_:
	g95_option.q_kind = atoi(arg);
	if (g95_validate_kind(BT_REAL, g95_option.q_kind) < 0)
	    g95_fatal_error("Argument to -fqkind isn't a valid real kind");

	break;

    case OPT_fsecond_underscore:
	g95_option.no_second_underscore = !value;
	break;

    case OPT_fshort_circuit:
	g95_option.short_circuit = value;
	break;

    case OPT_fsloppy_char:
	g95_option.sloppy_char = value;
	break;

    case OPT_fstatic:
	g95_option.static_var = value;
	break;

    case OPT_fsyntax:
	g95_option.verbose = value;
	break;

    case OPT_ftrace_:
	if (strcasecmp(arg, "none") == 0)
	    g95_option.trace = TRACE_NONE;

	else if (strcasecmp(arg, "frame") == 0)
	    g95_option.trace = TRACE_FRAME;

	else if (strcasecmp(arg, "full") == 0)
	    g95_option.trace = TRACE_FULL;

	else
	    g95_fatal_error("Bad value for -ftrace");

	break;

    case OPT_ftr15581:
	g95_option.tr15581 = value;
	break;

    case OPT_finteger_:
	g95_option.integer_init = 1;
	g95_option.integer_value = atoi(arg);
	break;

    case OPT_flogical_:
	if (strcasecmp(arg, "none") == 0)
	    g95_option.logical_init = LOGICAL_INIT_NONE;

	else if (strcasecmp(arg, "true") == 0)
	    g95_option.logical_init = LOGICAL_INIT_TRUE;

	else if (strcasecmp(arg, "false") == 0)
	    g95_option.logical_init = LOGICAL_INIT_FALSE;

	else
	    g95_fatal_error("Bad value for -flogical");

	break;

    case OPT_freal_:
	if (strcasecmp(arg, "none") == 0)
	    g95_option.real_init = REAL_INIT_NONE;

	else if (strcasecmp(arg, "zero") == 0)
	    g95_option.real_init = REAL_INIT_ZERO;

	else if (strcasecmp(arg, "nan") == 0)
	    g95_option.real_init = REAL_INIT_NAN;

	else if (strcasecmp(arg, "inf") == 0)
	    g95_option.real_init = REAL_INIT_PLUS_INF;

	else if (strcasecmp(arg, "+inf") == 0)
	    g95_option.real_init = REAL_INIT_PLUS_INF;

	else if (strcasecmp(arg, "-inf") == 0)
	    g95_option.real_init = REAL_INIT_MINUS_INF;

	else
	    g95_fatal_error("Bad value for -freal");

	break;

    case OPT_fpointer_:
	if (strcasecmp(arg, "none") == 0)
	    g95_option.pointer_init = POINTER_INIT_NONE;

	else if (strcasecmp(arg, "null") == 0)
	    g95_option.pointer_init = POINTER_INIT_NULL;

	else if (strcasecmp(arg, "invalid") == 0)
	    g95_option.pointer_init = POINTER_INIT_INVALID;

	else
	    g95_fatal_error("Bad value for -fpointer");

	break;

    case OPT_fround_:
	if (strcasecmp(arg, "nearest") == 0)
	    g95_option.round = ROUND_NEAREST;

	else if (strcasecmp(arg, "plus") == 0)
	    g95_option.round = ROUND_PLUS;

	else if (strcasecmp(arg, "minus") == 0)
	    g95_option.round = ROUND_MINUS;

	else if (strcasecmp(arg, "zero") == 0)
	    g95_option.round = ROUND_ZERO;

	else
	    g95_fatal_error("Bad value for -fround");

	break;

    case OPT_fzero:
	g95_option.zero_init = value;
	break;

    case OPT_funderscoring:
	g95_option.no_underscoring = !value;
	break;

    case OPT_I:
	add_path(arg);
	break;

    case OPT_i4:
	g95_option.default_integer = 4;
	break;

    case OPT_i8:
	g95_option.default_integer = 8;
	break;

    case OPT_include:
	break;

    case OPT_M:
	g95_option.deps = 1;
	break;

    case OPT_r4:
	g95_option.r_value = 4;
	break;

    case OPT_r8:
	g95_option.r_value = 8;
	break;

    case OPT_r10:
#if !defined(FPU_387) && !defined(FPU_SSE)
	g95_fatal_error("r10 option not supported on this platform");
#endif

	g95_option.r_value = 10;
	break;

    case OPT_r16:
	g95_option.r_value = 16;
	break;

    case OPT_no_cpp:
	g95_option.cpp = !value;
	break;

    case OPT_std_F:
	g95_option.fmode = 96;
	g95_option.symbol_len = 31;
	break;

    case OPT_std_f2003:
	g95_option.fmode = 2003;
	g95_option.symbol_len = 63;
	break;

    case OPT_std_f95:
	g95_option.fmode = 95;
	g95_option.symbol_len = 31;
	break;

    case OPT_traditional:
	g95_option.traditional = 1;
	break;

    case OPT_nontraditional:
	g95_option.traditional = 0;
	break;

    case OPT_U:
	g95_define_cpp_macro((char *) arg, 0);
	break;

    case OPT_Wall:
	set_Wall();
	break;

    case OPT_Werror:
	g95_option.werror = value;
	break;

    case OPT_Werror_:
	set_error_list(arg);
	break;

    case OPT_Wglobals:
	g95_option.globals = value;
	break;

    case OPT_Wimplicit_interface:
	g95_option.implicit_interface = value;
	break;

    case OPT_Wimplicit_none:
	g95_option.implicit_none = value;
	break;

    case OPT_Wline_truncation:
	g95_option.line_truncation = value;
	break;

    case OPT_Wmissing_intent:
	g95_option.missing_intent = value;
	break;

    case OPT_Wno_:
	set_nowarn(arg);
	break;

    case OPT_Wobsolescent:
	g95_option.obsolescent = value;
	break;

    case OPT_Wprecision_loss:
	g95_option.prec_loss = value;
	break;

    case OPT_Wuninitialized:
	g95_option.uninit = value;

	g95_option.uninit = 0;      /* Disabled for now. */
	warn_uninitialized = 2;
	break;

    case OPT_Wunused_label:
	g95_option.unused_label = value;
	break;

    case OPT_Wunused_internal_procs:
	g95_option.unused_internal_procs = value;
	break;

    case OPT_Wunused_module_vars:
	g95_option.unused_module_vars = value;
	break;

    case OPT_Wunused_module_procs:
	g95_option.unused_module_procs = value;
	break;

    case OPT_Wunused_parameter:
	g95_option.unused_parameter = value;
	break;

    case OPT_Wunused_target:
	g95_option.unused_target = value;
	break;

    case OPT_Wunused_types:
	g95_option.unused_types = value;
	break;

    case OPT_Wunused_vars:
	g95_option.unused_vars = value;
	break;

    case OPT_Wunset_vars:
	g95_option.unset_vars = value;
	break;

    default:
	r = 0;
	break;
    }

    return r;
}


#endif

#ifndef IN_GCC

/* g95_parse_arg()-- Parse an argument on the command line.  Returns
 * the number of elements used in the argv array.  Negative values
 * indicate that the option is language-dependent (and the absolute
 * value number of arguments are consumed). */

int g95_parse_arg(int argc, char *argv[]) {
const char *option;
int i;

    option = argv[0];

    if (strcmp(option, "-v") == 0) {
	g95_option.verbose = 1;
	return 1;
    }

    if (strcmp(option, "-Wline-truncation") == 0) {
	g95_option.line_truncation = 1;
	return 1;
    }

    if (strcmp(option, "-Wunused-label") == 0) {
	g95_option.unused_label = 1;
	return -1;
    }

    if (strncmp(option, "-Wno=", 5) == 0) {
	set_nowarn(option+5);
	return 1;
    }

    if (strcmp(option, "-fimplicit-none") == 0 ||
	strcmp(option, "-Wimplicit") == 0) {
	g95_option.implicit_none = 1;
	return -1;
    }

    if (strcmp(option, "-ffixed-line-length-80") == 0) {
	g95_option.fixed_line_length = 80;
	return -1;
    }

    if (strcmp(option, "-ffixed-line-length-132") == 0) {
	g95_option.fixed_line_length = 132;
	return -1;
    }

    if (strcmp(option, "-ffree-form") == 0) {
	g95_option.form = FORM_FREE;
	return -1;
    }

    if (strcmp(option, "-ffixed-form") == 0) {
	g95_option.form = FORM_FIXED;
	return -1;
    }

    if (strcmp(option, "-fmodule-private") == 0) {
	g95_option.module_access_private = 1;
	return -1;
    }

    if (strcmp(option, "-fdollar-ok") == 0) {
	g95_option.dollar = 1;
	return 1;
    }

    if (strcmp(option, "-fno-backslash") == 0) {
	g95_option.no_backslash = 1;
	return 1;
    }

    if (strcmp(option, "-fno-underscoring") == 0) {
	g95_option.no_underscoring = 1;
	return 1;
    }

    if (strcmp(option, "-fno-second-underscore") == 0) {
	g95_option.no_second_underscore = 1;
	return 1;
    }

    if (strncmp(option, "-fqkind=", 8) == 0) {
	i = atoi(option+8);
	if (g95_validate_kind(BT_REAL, i) < 0)
	    g95_fatal_error("Argument to -fqkind isn't a valid real kind");

	g95_option.q_kind = i;
	return -1;
    }

    if (strcmp(option, "-fquiet") == 0 || strcmp(option, "-quiet") == 0) {
	g95_option.quiet = 1;
	return 1;
    }

    if (strcmp(option, "-i8") == 0) {
	g95_option.default_integer = 8;
	return -1;
    }

    if (strcmp(option, "-r8") == 0) {
	g95_option.r_value = 8;
	return -1;
    }

    if (strcmp(option, "-d8") == 0) {
	g95_option.r_value = 8;
	g95_option.default_integer = 8;
	return -1;
    }

    if (strcmp(option, "-l1") == 0) {
	g95_option.l1 = 1;
	return -1;
    }

    if (option[0] == '-' && option[1] == 'I') {
	if (option[2] != '\0') {
	    add_path(&option[2]);
	    return 1;
	}

	if (argc <= 2 || argv[1][0] == '-') {
	    g95_status("g95: Directory required after -I\n");
	    exit(3);
	}

	add_path(argv[1]);
	return 2;
    }

    if (strncmp(option, "-fmod=", 6) == 0) {
	module_path(option);
	add_path(option + 6);
	return 1;
    }

    if (option[0] == '-') {
	g95_status("g95: Unrecognized option '%s'\n", option);
	exit(3);
    }

    if (g95_source_file != NULL) {
	g95_status("g95: Second source file '%s' found\n", option);
	exit(3);
    }

    g95_source_file = (char *) option;
    return 1;
}

#endif
