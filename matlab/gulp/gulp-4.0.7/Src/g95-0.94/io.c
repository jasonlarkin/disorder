/* I/O related subroutines
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

/* io.c-- Deal with input/output statements */

#include <string.h>
#include "g95.h"

g95_st_label g95_format_asterisk;


typedef struct {
    char *name;
    int level;
} opt_value;

typedef struct {
    char *name, *spec;
    bt type;
    opt_value *values;
} io_tag;


static opt_value open_opt[] = {
    { "unknown", 95 }, { "old", 95 }, { "new", 95 }, { "replace", 95 },
    { "scratch", 95 }, { NULL, 0 } };

static opt_value access_opt[] = {
    { "sequential", 95 }, { "direct", 95 },      { "stream", 2003 },
    { "append", 95 },     { "transparent", -1 }, { NULL, 0 } };

static opt_value form_opt[] = {
    { "formatted", 95 }, { "unformatted", 95 }, { NULL, 0 }};

static opt_value blank_opt[]  = {
    { "null", 95 }, { "zero", 95 }, { NULL, 95 } };

static opt_value pos_opt[] = {
    { "asis", 95 }, { "rewind", 95 }, { "append", 95 }, { NULL, 0 } };

static opt_value action_opt[] = {
    { "read", 95 }, { "write", 95 }, { "readwrite", 95 }, { NULL, 0 } };

static opt_value delim_opt[] = {
    { "none", 95 }, { "apostrophe", 95, },  { "quote", 95 }, { NULL, 0 } };

static opt_value yesno_opt[] = {
    { "yes", 95 }, { "no", 95 }, { NULL, 0 } };

static opt_value close_opt[] = {
    { "keep", 95 }, { "delete", 95 }, { NULL, 0 } };

static opt_value decimal_opt[] = {
    { "comma", 2003 }, { "point", 2003 }, { NULL, 0 } };

static opt_value convert_opt[] = {
    { "native", 95 },  { "swap", 95 }, { "little_endian", 95 },
    { "big_endian", 95 }, { NULL, 0 }
};



static io_tag
  tag_file =        { "FILE",     " file = %e",     BT_CHARACTER, NULL        },
  tag_ostatus =     { "STATUS",   " status = %e",   BT_CHARACTER, open_opt    },
  tag_cstatus =     { "STATUS",   " status = %e",   BT_CHARACTER, close_opt   },
  tag_e_access =    { "ACCESS",   " access = %e",   BT_CHARACTER, access_opt  },
  tag_e_form =      { "FORM",     " form = %e",     BT_CHARACTER, form_opt    },
  tag_e_recl =      { "RECL",     " recl = %e",     BT_INTEGER,   NULL        },
  tag_e_blank =     { "BLANK",    " blank = %e",    BT_CHARACTER, blank_opt   },
  tag_e_position =  { "POSITION", " position = %e", BT_CHARACTER, pos_opt     },
  tag_e_action =    { "ACTION",   " action = %e",   BT_CHARACTER, action_opt  },
  tag_e_delim =     { "DELIM",    " delim = %e",    BT_CHARACTER, delim_opt   },
  tag_e_pad =       { "PAD",      " pad = %e",      BT_CHARACTER, yesno_opt   },

  tag_unit =        { "UNIT",     " unit = %e",     BT_INTEGER,   NULL },
  tag_advance =     { "ADVANCE",  " advance = %e",  BT_CHARACTER, yesno_opt },
  tag_pos =         { "POS",      " pos = %e",      BT_INTEGER,   NULL },
  tag_pos_v =       { "POS",      " pos = %v",      BT_INTEGER,   NULL },
  tag_rec =         { "REC",      " rec = %e",      BT_INTEGER,   NULL },
  tag_format =      { "FORMAT",   NULL,             BT_UNKNOWN,   NULL },
  tag_decimal =     { "DECIMAL",  " decimal = %e",  BT_CHARACTER, decimal_opt},
  tag_convert =     { "CONVERT",  " convert = %e",  BT_CHARACTER, convert_opt },

  tag_iostat =      { "IOSTAT",      " iostat = %v",      BT_INTEGER,   NULL },
  tag_size =        { "SIZE",        " size = %v",        BT_INTEGER,   NULL },
  tag_exist =       { "EXIST",       " exist = %v",       BT_LOGICAL,   NULL },
  tag_opened =      { "OPENED",      " opened = %v",      BT_LOGICAL,   NULL },
  tag_nextrec =     { "NEXTREC",     " nextrec = %v",     BT_INTEGER,   NULL },
  tag_s_blank =     { "BLANK",       " blank = %v",       BT_CHARACTER, NULL },
  tag_s_position =  { "POSITION",    " position = %v",    BT_CHARACTER, NULL },
  tag_s_action =    { "ACTION",      " action = %v",      BT_CHARACTER, NULL },
  tag_read =        { "READ",        " read = %v",        BT_CHARACTER, NULL },
  tag_write =       { "WRITE",       " write = %v",       BT_CHARACTER, NULL },
  tag_s_form =      { "FORM",        " form = %v",        BT_CHARACTER, NULL },
  tag_formatted =   { "FORMATTED",   " formatted = %v",   BT_CHARACTER, NULL },
  tag_unformatted = { "UNFORMATTED", " unformatted = %v", BT_CHARACTER, NULL },
  tag_readwrite =   { "READWRITE",   " readwrite = %v",   BT_CHARACTER, NULL },
  tag_s_delim =     { "DELIM",       " delim = %v",       BT_CHARACTER, NULL },
  tag_s_pad =       { "PAD",         " pad = %v",         BT_CHARACTER, NULL },
  tag_iolength =    { "IOLENGTH",    " iolength = %v",    BT_INTEGER,   NULL },
  tag_iomsg =       { "IOMSG",       " iomsg = %v",       BT_CHARACTER, NULL },
  tag_id =          { "ID",          " id = %e",          BT_INTEGER,   NULL },
  tag_named =       { "NAMED",       " named = %v",       BT_LOGICAL,   NULL },
  tag_name =        { "NAME",        " name = %v",        BT_CHARACTER, NULL },
  tag_number =      { "NUMBER",      " number = %v",      BT_INTEGER,   NULL },
  tag_s_access =    { "ACCESS",      " access = %v",      BT_CHARACTER, NULL },
  tag_sequential =  { "SEQUENTIAL",  " sequential = %v",  BT_CHARACTER, NULL },
  tag_direct =      { "DIRECT",      " direct = %v",      BT_CHARACTER, NULL },
  tag_s_recl =      { "RECL",        " recl = %v",        BT_INTEGER,   NULL },
  tag_stream =      { "STREAM",      " stream = %v",      BT_CHARACTER,  NULL },

  tag_err = { "ERR", " err = %l", BT_UNKNOWN, NULL },
  tag_end = { "END", " end = %l", BT_UNKNOWN, NULL },
  tag_eor = { "EOR", " eor = %l", BT_UNKNOWN, NULL };

static g95_dt *current_dt;

#define RESOLVE_TAG(x, y, flag) \
    if (resolve_tag(x, y, flag) == FAILURE) return FAILURE;

static match match_io_element(io_kind k, g95_code **);
static try resolve_tag(io_tag *, g95_expr *, int);



/* match_etag()-- Match an expression I/O tag of some sort. */

static match match_etag(io_tag *tag, g95_expr **v) {
g95_expr *result;
match m;

    m = g95_match(tag->spec, &result);
    if (m != MATCH_YES)
	return m;

    if (*v != NULL) {
	g95_error("Duplicate %s specification at %C", tag->name);
	g95_free_expr(result);
	return MATCH_ERROR;
    }

    *v = result;
    return MATCH_YES;
}



/* match_vtag()-- Match a variable I/O tag of some sort. */

static match match_vtag(io_tag *tag, g95_expr **v) {
g95_expr *result;
match m;

    result = NULL;
    m = g95_match(tag->spec, &result);
    if (m != MATCH_YES)
	return m;

    g95_simplify_expr(result);
    if (result->type != EXPR_VARIABLE) {
	g95_error("Value of tag '%s' at %C must be a VARIABLE", tag->name);
	goto cleanup;
    }

    if (*v != NULL) {
	g95_error("Duplicate %s specification at %C", tag->name);
	goto cleanup;
    }

    if (result->symbol->attr.intent == INTENT_IN) {
	g95_error("Variable tag cannot be INTENT(IN) at %C");
	goto cleanup;
    }

    if (g95_pure(NULL, 1) && g95_impure_variable(result->symbol)) {
	g95_error("Variable tag cannot be assigned in PURE procedure at %C");
	goto cleanup;
    }

    *v = result;
    return MATCH_YES;

cleanup:
    g95_free_expr(result);
    return MATCH_ERROR;
}



/* match_ltag()-- Match a label I/O tag */

static match match_ltag(io_tag *tag, g95_st_label **label) {
g95_st_label *old;
match m;

    old = *label;
    m = g95_match(tag->spec, label);
    if (m == MATCH_YES && old != NULL) {
	g95_error("Duplicate %s label specification at %C", tag->name);
	return MATCH_ERROR;
    }

    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_error("Label tag at %C is not permitted in F");
	return MATCH_ERROR;
    }

    return m;
}



/* match_out_tag()-- Match I/O tags that cause variables to become
 * redefined. */

static match match_out_tag(io_tag *tag, g95_expr **result) {
match m;

    m = match_vtag(tag, result);
    if (m == MATCH_YES)
	g95_check_do_variable((*result)->symbol, &(*result)->where);

    return m;
}



/* match_pos_tag()-- Match the POS tag */

match static match_pos_tag(int dt, g95_expr **result) {
match m;

    m = (dt)
	? match_etag(&tag_pos, result)
	: match_vtag(&tag_pos_v, result);

    if (m != MATCH_YES)
	return m;

    if (g95_option.fmode == 0 || g95_option.fmode == 2003)
	return MATCH_YES;
 
    g95_error("POS tag at %C is not legal in %s mode", g95_mode_name());
    return MATCH_ERROR;
}



/* resolve_tag()-- Do expression resolution and type-checking on an
 * expression tag */

static try resolve_tag(io_tag *tag, g95_expr *e, int output) {
opt_value *v;
int m;

    if (e == NULL)
	return SUCCESS;

    if (g95_resolve_expr(e) == FAILURE)
	return FAILURE;

    if (output && e->type == EXPR_VARIABLE)
	g95_set_usage(e->symbol, &e->where, 1, 0);

    if (tag == &tag_format) {
	if (e->ts.type == BT_INTEGER) { /* Assigned format labels */
	    if (e->type != EXPR_VARIABLE || e->rank > 0) {
		g95_error("FORMAT tag at %L must be a scalar integer variable",
			  &e->where);
		return FAILURE;
	    }

	    if (g95_option.fmode != 0)
		g95_warning(138, "Assigned format label at %L is no longer "
			    "legal fortran", &e->where);

	    g95_current_ns->format_label = 1;
	    return SUCCESS;
	}

	if (g95_option.sloppy_char)
	    return SUCCESS;

	if (e->ts.type != BT_CHARACTER) {
	    g95_error("FORMAT tag at %L must be of type CHARACTER",
		      &e->where);
	    return FAILURE;
	}

    } else {
	if (e->rank != 0) {
	    g95_error("%s tag at %L must be scalar", tag->name, &e->where);
	    return FAILURE;
	}

	if (e->ts.type != tag->type) {
	    g95_error("%s tag at %L must be of type %s", tag->name, &e->where,
		      g95_basic_typename(tag->type));
	    return FAILURE;
	}
    }

    if (e->ts.kind != g95_default_integer_kind(1) && G95_STRICT_F95()) {
	if (tag == &tag_iostat) {
	    g95_error("IOSTAT variable at %L must be a default integer",
		      &e->where);
	    return FAILURE;
	}

	if (tag == &tag_number) {
	    g95_error("NUMBER variable at %L must be a default integer",
		      &e->where);
	    return FAILURE;
	}

	if (tag == &tag_nextrec) {
	    g95_error("NEXTREC variable at %L must be a default integer",
		      &e->where);
	    return FAILURE;
	}

	if (tag == &tag_size) {
	    g95_error("SIZE variable at %L must be a default integer",
		      &e->where);
	    return FAILURE;
	}
    }

    if (e->ts.kind != g95_default_logical_kind() && g95_option.fmode != 0) {
	if (tag == &tag_exist) {
	    g95_error("EXIST variable at %L must be a default logical",
		      &e->where);
	    return FAILURE;
	}

	if (tag == &tag_named) {
	    g95_error("NAMED variable at %L must be a default logical",
		      &e->where);
	    return FAILURE;
	}

	if (tag == &tag_opened) {
	    g95_error("OPENED variable at %L must be a default logical",
		      &e->where);
	    return FAILURE;
	}
    }

    if (tag->values == NULL || e->type != EXPR_CONSTANT)
	return SUCCESS;

    for(v=tag->values; v->name; v++) {
	m = strlen(v->name);
	if (m == e->value.character.length &&
	    strncasecmp(v->name, e->value.character.string, m) == 0) {

	    if (g95_option.fmode != 0 && v->level == -1) {
		g95_warning(149, "Tag '%s' at %L is not standard",
			    tag->name, &e->where);
		return SUCCESS;
	    }

	    if (g95_option.fmode != 0 && g95_option.fmode != 2003 &&
		v->level == 2003)
		g95_warning(150, "Tag '%s' at %L is not standard in %s mode",
			    tag->name, &e->where, g95_mode_name());

	    return SUCCESS;
	}
    }

    g95_error("Invalid value for tag '%s' at %L", tag->name, &e->where);
    return FAILURE;
}



/* compare_string()-- Compare a constant string expression with a C
 * string, in a case-insensitive manner.  Assumes that the C string
 * does not end with spaces.  Returns nonzero if equal. */

static int compare_string(g95_expr *e, char *s2, int deflt) {
int len1, len2;
char *s1, *p;

    if (e == NULL || e->type != EXPR_CONSTANT || e->ts.type != BT_CHARACTER)
	return deflt;

    len2 = strlen(s2);

    s1 = e->value.character.string;
    len1 = e->value.character.length;

    if (len1 < len2 || strncasecmp(s1, s2, len2) != 0)
	return 0;

    p = s1 + len2;

    while(len2 < len1) {
	len2++;

	if (*p++ != ' ')
	    return 0;
    }

    return 1;
}



/* match_open_element()-- Match a single tag of an OPEN statement */

static match match_open_element(g95_open *open) {
match m;

    m = match_etag(&tag_unit, &open->unit);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iostat, &open->iostat);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_file, &open->file);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_ostatus, &open->status);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_access, &open->access);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_form, &open->form);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_recl, &open->recl);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_blank, &open->blank);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_position, &open->position);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_action, &open->action);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_delim, &open->delim);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_e_pad, &open->pad);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_decimal, &open->decimal);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_err, &open->err);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_convert, &open->convert);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iomsg, &open->iomsg);
    if (m != MATCH_NO)
	return m;

    return MATCH_NO;
}



/* g95_free_open()-- Free the g95_open structure and all the
 * expressions it contains. */

void g95_free_open(g95_open *open) {

    if (open == NULL)
	return;

    g95_free_expr(open->unit);
    g95_free_expr(open->iostat);
    g95_free_expr(open->file);
    g95_free_expr(open->status);
    g95_free_expr(open->access);
    g95_free_expr(open->form);
    g95_free_expr(open->recl);
    g95_free_expr(open->blank);
    g95_free_expr(open->position);
    g95_free_expr(open->action);
    g95_free_expr(open->delim);
    g95_free_expr(open->pad);
    g95_free_expr(open->decimal);
    g95_free_expr(open->convert);
    g95_free_expr(open->iomsg);

    g95_free(open);
}



/* g95_resolve_open()-- resolve everything in a g95_open structure */

try g95_resolve_open(g95_open *open) {

    RESOLVE_TAG(&tag_unit,     open->unit,    0);
    RESOLVE_TAG(&tag_iostat,   open->iostat,  1);
    RESOLVE_TAG(&tag_iomsg,    open->iomsg,   1);
    RESOLVE_TAG(&tag_file,     open->file,    0);
    RESOLVE_TAG(&tag_ostatus,  open->status,  0);
    RESOLVE_TAG(&tag_e_form,   open->form,    0);
    RESOLVE_TAG(&tag_e_recl,   open->recl,    0);
    RESOLVE_TAG(&tag_e_access, open->access,  0);
    RESOLVE_TAG(&tag_decimal,  open->decimal, 0);

    RESOLVE_TAG(&tag_e_blank,     open->blank,     0);
    RESOLVE_TAG(&tag_e_position,  open->position,  0);
    RESOLVE_TAG(&tag_e_action,    open->action,    0);
    RESOLVE_TAG(&tag_e_delim,     open->delim,     0);
    RESOLVE_TAG(&tag_e_pad,       open->pad,       0);
    RESOLVE_TAG(&tag_convert,     open->convert,   0);

    if (g95_reference_st_label(open->err, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    if (g95_option.fmode != 0 && open->convert != NULL) {
	g95_error("CONVERT tag at %L is nonstandard", &open->convert->where);
	return FAILURE;
    }

    if (G95_STRICT_F95() && open->decimal != NULL) {
	g95_error("DECIMAL tag at %L is an F2003 feature",
		  &open->decimal->where);
	return FAILURE;
    }

    if (G95_STRICT_F()) {
	if (open->status == NULL) {
	    g95_error("A STATUS tag must be specified in OPEN statement at %L",
		      &open->where);
	    return FAILURE;
	}

	if (open->action == NULL) {
	    g95_error("An ACTION tag must be specified in OPEN statement "
		      "at %L", &open->where);
	    return FAILURE;
	}

	if (open->pad != NULL) {
	    g95_error("PAD tag not allowed in OPEN statement at %L in F mode",
		      &open->pad->where);
	    return FAILURE;
	}

	if (open->delim != NULL) {
	    g95_error("DELIM tag not allowed in OPEN statement at %L in "
		      "F mode", &open->delim->where);
	    return FAILURE;
	}

	if (open->blank != NULL) {
	    g95_error("BLANK tag not allowed in OPEN statement at %L in "
		      "F mode", &open->blank->where);
	    return FAILURE;
	}

	if (compare_string(open->action, "read", 0) &&
	    (compare_string(open->status, "new", 0) ||
	     compare_string(open->status, "replace", 0))) {
	    g95_error("Open action at %L must not be READ if status is NEW or "
		      "REPLACE in F", &open->action->where);
	    return FAILURE;
	}

	if (!compare_string(open->action, "readwrite", 1) &&
	    compare_string(open->status, "scratch", 0)) {
	    g95_error("Open action must be READWRITE if status at %L is "
		      "SCRATCH", &open->status->where);
	    return FAILURE;
	}

	if (compare_string(open->status, "unknown", 0)) {
	    g95_error("Open status of UNKNOWN at %L is not permitted in F",
		      &open->status->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* g95_match_open()-- Match an OPEN statement */

match g95_match_open(void) {
g95_open *open;
match m;

    m = g95_match_char('(');
    if (m == MATCH_NO)
	return m;

    open = g95_getmem(sizeof(g95_open));
    open->where = g95_current_locus;

    m = match_open_element(open);

    if (m == MATCH_ERROR)
	goto cleanup;

    if (m == MATCH_NO) {
	if (G95_STRICT_F())
	    goto syntax;

	m = g95_match_expr(&open->unit);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;
    }

    for(;;) {
	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;

	m = match_open_element(open);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

    if (g95_match_eos() == MATCH_NO)
	goto syntax;

    if (g95_pure(NULL, 1)) {
	g95_error("OPEN statement not allowed in PURE procedure at %C");
	goto cleanup;
    }

    if (open->unit == NULL) {
	g95_error("Missing unit specification in OPEN statement at %C");
	goto cleanup;
    }

    new_st.type = EXEC_OPEN;
    new_st.ext.open = open;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_OPEN);

cleanup:
    g95_free_open(open);
    return MATCH_ERROR;
}



/* g95_free_close()-- Free a g95_close structure an all its expressions */

void g95_free_close(g95_close *close) {

    if (close == NULL)
	return;

    g95_free_expr(close->unit);
    g95_free_expr(close->iostat);
    g95_free_expr(close->iomsg);
    g95_free_expr(close->status);

    g95_free(close);
}



/* match_close_element()-- Match elements of a CLOSE statement */

static match match_close_element(g95_close *close) {
match m;

    m = match_etag(&tag_unit, &close->unit);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_cstatus, &close->status);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iostat, &close->iostat);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iomsg, &close->iomsg);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_err, &close->err);
    if (m != MATCH_NO)
	return m;

    return MATCH_NO;
}



/* g95_match_close()-- Match a CLOSE statement */

match g95_match_close(void) {
g95_close *close;
match m;

    m = g95_match_char('(');
    if (m == MATCH_NO) return m;

    close = g95_getmem(sizeof(g95_close));

    m = match_close_element(close);

    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) {
	m = g95_match_expr(&close->unit);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;
    }

    for(;;) {
	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;

	m = match_close_element(close);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

    if (g95_match_eos() == MATCH_NO) goto syntax;

    if (g95_pure(NULL, 1)) {
	g95_error("CLOSE statement not allowed in PURE procedure at %C");
	goto cleanup;
    }

    if (close->unit == NULL) {
	g95_error("Missing unit specification in CLOSE statement at %C");
	goto cleanup;
    }

    new_st.type = EXEC_CLOSE;
    new_st.ext.close = close;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_CLOSE);

cleanup:
    g95_free_close(close);
    return MATCH_ERROR;
}



/* g95_resolve_close()-- Resolve everything in a g95_close structure */

try g95_resolve_close(g95_close *close) {

    RESOLVE_TAG(&tag_unit,    close->unit,    0);
    RESOLVE_TAG(&tag_iostat,  close->iostat,  1);
    RESOLVE_TAG(&tag_iomsg,   close->iomsg,   1);
    RESOLVE_TAG(&tag_cstatus, close->status,  0);

    if (g95_reference_st_label(close->err, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* g95_free_filepos()-- Free a g95_filepos structure */

void g95_free_filepos(g95_filepos *fp) {

    g95_free_expr(fp->unit);
    g95_free_expr(fp->iostat);
    g95_free_expr(fp->iomsg);

    g95_free(fp);
}



try g95_resolve_filepos(g95_filepos *fp) {

    RESOLVE_TAG(&tag_unit,   fp->unit,   0);
    RESOLVE_TAG(&tag_iostat, fp->iostat, 1);
    RESOLVE_TAG(&tag_iomsg,  fp->iomsg,  1);

    return g95_reference_st_label(fp->err, ST_LABEL_TARGET);
}



/* match_file_element()-- Match elements of a REWIND, BACKSPACE or
 * ENDFILE statement */

static match match_file_element(g95_filepos *fp) {
match m;

    m = match_etag(&tag_unit, &fp->unit);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iostat, &fp->iostat);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iomsg, &fp->iomsg);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_err, &fp->err);
    if (m != MATCH_NO)
	return m;

    return MATCH_NO;
}



/* match_filepos()-- Match the second half of the file-positioning
 * statements, REWIND, BACKSPACE or ENDFILE. */

static match match_filepos(g95_statement st, g95_exec_op op) {
g95_filepos *fp;
match m;

    fp = g95_getmem(sizeof(g95_filepos));

    if (g95_match_char('(') == MATCH_NO) {
	if (g95_match_space() == MATCH_NO) {
	    g95_free(fp);
	    return MATCH_NO;
	}

	m = g95_match_expr(&fp->unit);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	goto done;
    }

    m = match_file_element(fp);
    if (m == MATCH_ERROR) goto done;
    if (m == MATCH_NO) {
	m = g95_match_expr(&fp->unit);
	if (m == MATCH_ERROR) goto done;
	if (m == MATCH_NO) goto syntax;
    }

    for(;;) {
	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;

	m = match_file_element(fp);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

done:
    if (g95_match_eos() != MATCH_YES) goto syntax;

    if (g95_pure(NULL, 1)) {
	g95_error("%s statement not allowed in PURE procedure at %C",
		  g95_ascii_statement(st));

	return MATCH_ERROR;
    }

    new_st.type = op;
    new_st.ext.filepos = fp;
    return MATCH_YES;

syntax:
    g95_syntax_error(st);

cleanup:
    g95_free_filepos(fp);
    return MATCH_ERROR;
}



match g95_match_backspace(void) {

    return match_filepos(ST_BACKSPACE, EXEC_BACKSPACE);
}



match g95_match_rewind(void) {

    return match_filepos(ST_REWIND, EXEC_REWIND);
}



match g95_match_endfile(void) {

    return match_filepos(ST_END_FILE, EXEC_ENDFILE);
}



/* peek_character_constant()-- See if the next thing on the input
 * could be character constant.  Returns nonzero if so. */

static int peek_character_constant(void) {
g95_locus where;
int c;

    where = g95_current_locus;
    g95_gobble_whitespace();

    c = g95_next_char();

    if (c == '\'' || c == '"')
	goto yes;

    if (c < '0' || c > '9')
	goto no;

    do
	c = g95_next_char();
    while('0' <= c && c <= '9');

    if (c != '_')
	goto no;

    c = g95_next_char();
    if (c == '\'' || c == '"')
	goto yes;

no:
    g95_current_locus = where;
    return 0;

yes:
    g95_current_locus = where;
    return 1;
}



/* match_dt_format()-- Match a format specification, and maybe a
 * namelist group name. */

static match match_dt_format(io_kind k, int namelist, g95_dt *dt) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_st_label *label;
g95_locus where;
g95_symbol *sym;
g95_expr *e;
char c;

    where = g95_current_locus;

    if (g95_match_char('*') == MATCH_YES) {
	if (dt->format_expr != NULL || dt->format_label != NULL)
	    goto conflict;

	dt->format_label = &g95_format_asterisk;
	return MATCH_YES;
    }

    if (peek_character_constant())
	goto format_expr;

    if (g95_match_st_label(&label, 0) == MATCH_YES) {
	if (dt->format_expr != NULL || dt->format_label != NULL) {
	    g95_free_st_label(label);
	    goto conflict;
	}

	if (g95_reference_st_label(label, ST_LABEL_FORMAT) == FAILURE)
	    return MATCH_ERROR;

	dt->format_label = label;
	return MATCH_YES;
    }

    if (k == M_PRINT && g95_current_form == FORM_FREE) {
	c = g95_peek_char();
	if (c != ' ' && c != '\'' && c != '"')
	    return MATCH_NO;
    }

    if (namelist && g95_match_name(name) == MATCH_YES &&
	g95_find_symbol(name, NULL, 1, &sym) == 0 && sym != NULL &&
	sym->attr.flavor == FL_NAMELIST) {
	dt->namelist = sym;
	return MATCH_YES;
    }

format_expr:
    g95_current_locus = where;

    if (g95_match_expr(&e) == MATCH_YES) {
	if (dt->format_expr != NULL || dt->format_label != NULL) {
	    g95_free_expr(e);
	    goto conflict;
	}

	if (e->ts.type != BT_UNKNOWN && e->ts.type != BT_CHARACTER &&
	    e->ts.type != BT_INTEGER) {
	    g95_free_expr(e);
	    goto no_match;
	}

	dt->format_expr = e;
	return MATCH_YES;
    }

no_match:
    g95_current_locus = where;    /* The only case where we have to restore */
    return MATCH_NO;

conflict:
    g95_error("Duplicate format specification at %C");
    return MATCH_ERROR;
}



/* default_unit()-- Return the default unit number.  The runtime
 * library translates this into the real unit number. */

static g95_expr *default_unit(void) {

    return g95_int_expr(-1);
}



/* match_dt_unit()-- Match a unit specification for a data transfer
 * statement */

static match match_dt_unit(g95_dt *dt) {
g95_expr *e;

    if (g95_match_char('*') == MATCH_YES) {
	if (dt->io_unit != NULL)
	    goto conflict;

	dt->io_unit = default_unit();
	dt->default_unit = 1;
	return MATCH_YES;
    }

    if (g95_match_expr(&e) == MATCH_YES) {
	if (dt->io_unit != NULL) {
	    g95_free_expr(e);
	    goto conflict;
	}

	dt->io_unit = e;
	return MATCH_YES;
    }

    return MATCH_NO;

conflict:
    g95_error("Duplicate UNIT specification at %C");
    return MATCH_ERROR;
}



/* check_namelist()-- Traverse a namelist that is part of a READ
 * statement to make sure that none of the variables in the namelist
 * are INTENT(IN).  Returns nonzero if we find such a variable */

static int check_namelist(g95_symbol *sym) {
g95_namelist *p;

    for(p=sym->namelist; p; p=p->next)
	if (p->sym->attr.intent == INTENT_IN) {
	    g95_error("Symbol '%s' in namelist '%s' is INTENT(IN) at %C",
		      p->sym->name, sym->name);
	    return 1;
	}

    return 0;
}



/* match_dt_element()-- Match a single data transfer element */

static match match_dt_element(io_kind k, g95_dt *dt) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
match m;

    if (g95_match(" unit =") == MATCH_YES) {
	m = match_dt_unit(dt);
	if (m != MATCH_NO)
	    return m;
    }

    if (g95_match(" fmt =") == MATCH_YES) {
	m = match_dt_format(k, 0, dt);
	if (m != MATCH_NO)
	    return m;
    }

    if (g95_match(" nml = %n", name) == MATCH_YES) {
	if (dt->namelist != NULL) {
	    g95_error("Duplicate NML specification at %C");
	    return MATCH_ERROR;
	}

	if (g95_find_symbol(name, NULL, 1, &sym))
	    return MATCH_ERROR;

	if (sym == NULL || sym->attr.flavor != FL_NAMELIST) {
	    g95_error("Symbol '%s' at %C must be a NAMELIST group name",
		      (sym != NULL) ? sym->name : name );
	    return MATCH_ERROR;
	}

	dt->namelist = sym;
	if (k == M_READ && check_namelist(sym))
	    return MATCH_ERROR;

	dt->namelist_where = g95_def_locus;
	return MATCH_YES;
    }

    m = match_etag(&tag_rec, &dt->rec);           if (m != MATCH_NO) return m;
    m = match_out_tag(&tag_iostat, &dt->iostat);  if (m != MATCH_NO) return m;
    m = match_out_tag(&tag_iomsg, &dt->iomsg);    if (m != MATCH_NO) return m;
    m = match_etag(&tag_advance, &dt->advance);   if (m != MATCH_NO) return m;
    m = match_out_tag(&tag_size, &dt->size);      if (m != MATCH_NO) return m;
    m = match_pos_tag(1, &dt->pos);               if (m != MATCH_NO) return m;
    m = match_etag(&tag_decimal, &dt->decimal);   if (m != MATCH_NO) return m;


    m = match_ltag(&tag_err, &dt->err);
    if (m == MATCH_YES)
	dt->err_where = g95_label_locus;

    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_end, &dt->end);
    if (m == MATCH_YES)
	dt->end_where = g95_label_locus;

    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_eor, &dt->eor);
    if (m == MATCH_YES)
	dt->eor_where = g95_label_locus;

    if (m != MATCH_NO)
	return m;

    return MATCH_NO;
}



/* g95_resolve_dt()-- Resolve everything in a g95_dt structure */

try g95_resolve_dt(g95_code *code) {
g95_namelist *n;
g95_expr *e;
g95_dt *dt;

    dt = code->ext.dt;

    RESOLVE_TAG(&tag_format,   dt->format_expr,  0);
    RESOLVE_TAG(&tag_rec,      dt->rec,          0);
    RESOLVE_TAG(&tag_pos,      dt->pos,          0);
    RESOLVE_TAG(&tag_advance,  dt->advance,      0);
    RESOLVE_TAG(&tag_iostat,   dt->iostat,       1);
    RESOLVE_TAG(&tag_iomsg,    dt->iomsg,        1);
    RESOLVE_TAG(&tag_size,     dt->size,         1);
    RESOLVE_TAG(&tag_decimal,  dt->decimal,      0);

    if (dt->ambig != NULL) {
	if (g95_resolve_expr(dt->ambig) == FAILURE)
	    return FAILURE;

	switch(dt->ambig->ts.type) {
	case BT_INTEGER:
	    dt->io_unit = dt->ambig;
	    break;

	case BT_CHARACTER:
	    dt->io_unit = default_unit();
	    dt->default_unit = 1;
	    dt->format_expr = dt->ambig;
	    break;

	default:
	    g95_internal_error("g95_resolve_dt(): Still-ambiguous format");
	    break;
	}

	dt->ambig = NULL;
    }

    if (dt->namelist != NULL) {
	for(n=dt->namelist->namelist; n; n=n->next)
	    if (code->type == EXEC_READ)
		g95_set_usage(n->sym, &code->where, 1, 0);
	    else
		g95_set_usage(n->sym, &code->where, 0, 1);
    }

    e = dt->io_unit;

    if (e == NULL) {
	g95_error("Missing unit in data transfer statement at %L",
		  &code->where);
	return FAILURE;
    }

    if (g95_resolve_expr(e) == FAILURE)
	return FAILURE;

    if (e->ts.type != BT_INTEGER &&
	(e->ts.type != BT_CHARACTER || e->type != EXPR_VARIABLE ||
	 e->symbol->attr.flavor == FL_PARAMETER)) {
	g95_error("UNIT specification at %L must be an INTEGER expression or "
		  "a CHARACTER variable", &e->where);
	return FAILURE;
    }

    if (e->ts.type == BT_CHARACTER) {
	if (code->type == EXEC_READ)
	    g95_set_usage(e->symbol, &e->where, 0, 1);
	else
	    g95_set_usage(e->symbol, &e->where, 1, 0);
    }

    if (e->ts.type == BT_INTEGER && e->rank != 0) {
	g95_error("Unit number at %L must be scalar", &e->where);
	return FAILURE;
    }

    if (!dt->default_unit && e->ts.type == BT_INTEGER &&
	e->type == EXPR_CONSTANT && bi_compare(e->value.integer, bi_0) < 0) {
	g95_error("Negative unit number at %L", &e->where);
	return FAILURE;
    }

    if (g95_pure(NULL, 0) && e->ts.type != BT_CHARACTER) {
	g95_error("io-unit in data transfer statement at %L must be an "
		  "internal file in a PURE procedure", &e->where);
	return FAILURE;    
    }

    if (G95_STRICT_F() && g95_current_ns->state == COMP_FUNCTION &&
	dt->type != M_PRINT && e->ts.type != BT_CHARACTER) {
	g95_error("io-unit in data transfer statement at %L must be an "
		  "internal file in a FUNCTION in F mode", &e->where);
	return FAILURE;    
    }

/* Sanity checks on data transfer statements */

    if (e->ts.type == BT_CHARACTER) {
	if (dt->rec != NULL) {  /* C919 */
	    g95_error("REC tag at %L is incompatible with an internal file",
		      &dt->rec->where);
	    return FAILURE;
	}

	if (dt->pos != NULL) {  /* C919 */
	    g95_error("POS tag at %L is incompatible with an internal file",
		      &dt->pos->where);
	    return FAILURE;
	}

	if (G95_STRICT_F95() && dt->namelist != NULL) {
	    g95_error("Internal file at %L is incompatible with namelist",
		      &dt->io_unit->where);
	    return FAILURE;
	}

	if (dt->advance != NULL) {
	    g95_error("ADVANCE tag at %L is incompatible with internal file",
		      &dt->advance->where);
	    return FAILURE;
	}

	if (g95_vector_subscripts(e)) {
	    g95_error("Cannot have vector subscripts with an internal file "
		      "at %L", &e->where);
	    return FAILURE;
	}

	if (dt->format_expr == NULL && dt->format_label == NULL &&
	    dt->namelist == NULL) {
	    g95_error("Cannot do unformatted I/O with internal files at %L",
		      &code->where);
	    return FAILURE;
	}
    }

    if (code->type == EXEC_WRITE && dt->end != NULL) {
	g95_error("END tag at %L is not compatible with output",
		  &dt->end_where);
	return FAILURE;
    }

    if (dt->rec != NULL) {
	if (dt->end != NULL) {
	    g95_error("REC tag at %L is incompatible with END tag",
		      &dt->rec->where);
	    return FAILURE;
	}

	if (dt->format_label == &g95_format_asterisk) {
	    g95_error("REC tag at %L is incompatible with list directed "
		      "format (*)", &code->where);
	    return FAILURE;
	}

	if (dt->namelist != NULL) {
	    g95_error("REC tag at %L is incompatible with namelist",
		      &dt->rec->where);
	    return FAILURE;
	}
    }

    if (dt->rec != NULL && dt->pos != NULL) { /* C927 */
	g95_error("POS tag at %L is incompatible with REC tag",
		  &dt->pos->where);
	return FAILURE;
    }

    if (dt->advance != NULL &&
	(dt->format_label == &g95_format_asterisk ||
	 (dt->format_label == NULL && dt->format_expr == NULL))) {
	g95_error("ADVANCE tag at %L is incompatible with list directed "
		  "format (*)", &dt->advance->where);
	return FAILURE;
    }

    if (code->type == EXEC_WRITE && dt->eor != NULL) {
	g95_error("EOR tag at %L can only appear in a READ statement",
		  &code->where);
	return FAILURE;
    }

    if (code->type == EXEC_WRITE && dt->size != NULL) {
	g95_error("SIZE tag at %L can only appear in a READ statement",
		  &code->where);
	return FAILURE;
    }

    if (dt->eor != 0 && dt->advance == NULL) {
	g95_error("EOR tag at %L requires an ADVANCE tag", &dt->eor_where);
	return FAILURE;
    }

    if (dt->size != NULL && dt->advance == NULL) {
	g95_error("SIZE tag at %L requires an ADVANCE tag", &dt->size->where);
	return FAILURE;
    }

    if (dt->namelist != NULL &&
	(dt->format_expr != NULL || dt->format_label != NULL)) {
	g95_error("NAMELIST tag at %L is not compatible with other formatting "
		  "options", &dt->where);
	return FAILURE;
    }

    if (dt->namelist != NULL && dt->type == M_READ)
	for(n=dt->namelist->namelist; n; n=n->next)
	    if (n->sym->attr.intent == INTENT_IN) {
		g95_error("NAMELIST read at %L contains INTENT(IN) variable "
			  "'%s'", &dt->where, n->sym->name);
		return FAILURE;
	    }

    if (G95_STRICT_F95() && dt->decimal != NULL) {
	g95_error("DECIMAL tag at %L is an F2003 feature",
		  &dt->decimal->where);
	return FAILURE;
    }

/* TODO: Make sure the ADVANCE tag is 'yes' or 'no' if it is a string
 * constant */

    if (g95_reference_st_label(dt->err, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    if (g95_reference_st_label(dt->end, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    if (g95_reference_st_label(dt->eor, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    if (dt->format_label != NULL &&
	dt->format_label->defined == ST_LABEL_UNKNOWN) {
	g95_error("Format label '%d' at %L is not defined",
		  dt->format_label->value, &dt->format_label->where);
	return FAILURE;
    }

    e = dt->format_expr;
    if (e != NULL && e->type == EXPR_CONSTANT)
	g95_check_format_string(e, (dt->type == M_READ), dt->have_data_item);

    return SUCCESS;
}



/* g95_free_dt()-- Free a data transfer structure and everything below it */

void g95_free_dt(g95_dt *dt) {

    if (dt == NULL)
	return;

    g95_free_expr(dt->io_unit);
    g95_free_expr(dt->format_expr);
    g95_free_expr(dt->rec);
    g95_free_expr(dt->advance);
    g95_free_expr(dt->iostat);
    g95_free_expr(dt->iomsg);
    g95_free_expr(dt->size);
    g95_free_expr(dt->ambig);
    g95_free_expr(dt->pos);
    g95_free_expr(dt->decimal);

    g95_free(dt);
}



/* io_kind_name()-- Given an io_kind, return its name */

static char *io_kind_name(io_kind k) {
char *name;

    switch(k) {
    case M_READ:     name = "READ";     break;
    case M_WRITE:    name = "WRITE";    break;
    case M_PRINT:    name = "PRINT";    break;
    case M_INQUIRE:  name = "INQUIRE";  break;
    default:
	g95_internal_error("io_kind_name(): bad I/O-kind");
    }

    return name;
}



/* match_io_iterator()-- Match an IO iteration statement of the form:
 *   ( [<IO element> ,] <IO element>, I = <expr>, <expr> [, <expr> ] )
 *
 * Which is equivalent to a single IO element.  This function is mutually
 * recursive with match_io_element().  */

static match match_io_iterator(io_kind k, g95_code **result) {
g95_code *head, *tail, *new, *c;
g95_iterator *iter;
g95_locus old_loc;
match m;
int n;

    iter = NULL;
    head = tail = NULL;
    old_loc = g95_current_locus;

    if (g95_match_char('(') != MATCH_YES)
	return MATCH_NO;

    m = match_io_element(k, &head);
    tail = head;

    if (m != MATCH_YES || g95_match_char(',') != MATCH_YES) {
	m = MATCH_NO;
	goto cleanup;
    }

/* Can't be anything but an IO iterator.  Build a list */

    iter = g95_get_iterator();

    for(n=1;; n++) {
	m = g95_match_iterator(iter);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_YES) {
	    g95_check_do_variable(iter->var->symbol, &iter->var->where);
	    break;
	}

	m = match_io_element(k, &new);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) {
	    if (n > 2) goto syntax;
	    goto cleanup;
	}

	tail = g95_append_code(tail, new);

	if (g95_match_char(',') != MATCH_YES) {
	    if (n > 2) goto syntax;
	    m = MATCH_NO;
	    goto cleanup;
	}
    }

    if (g95_match_char(')') != MATCH_YES)
	goto syntax;

    if (k == M_READ)
	for(c=head; c; c=c->next)
	    if (c->type == EXEC_TRANSFER && c->expr->type == EXPR_VARIABLE &&
		c->expr->symbol == iter->var->symbol) {
		g95_error("Iterator variable '%s' cannot appear in a READ "
			  "io-list at %L", iter->var->symbol->name, &c->where);
		m = MATCH_ERROR;
		goto cleanup;
	    }

    new = g95_get_code(EXEC_DO, NULL);
    new->ext.iterator = iter;
    new->block = head;

    *result = new;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in I/O iterator at %C");
    m = MATCH_ERROR;

cleanup:
    g95_free_iterator(iter, 1);
    g95_free_statements(head);
    g95_current_locus = old_loc;
    return m;
}



/* match_io_element()-- Match a single element of an IO list, which is
 * either a single expression or an IO Iterator */

static match match_io_element(io_kind k, g95_code **cpp) {
g95_expr *expr;
g95_code *cp;
match m;

    expr = NULL;

    m = match_io_iterator(k, cpp);
    if (m != MATCH_NO)
	return m;

    if (k == M_READ) {
	m = g95_match_variable(&expr, 0, 0, 0);
	if (m == MATCH_NO)
	    g95_error("Expected variable in READ statement at %C");

    } else {
	m = g95_match_expr(&expr);
	if (m == MATCH_NO)
	    g95_error("Expected expression in %s statement at %C",
		      io_kind_name(k));
    }

    if (m == MATCH_YES)
	switch(k) {
	case M_READ:
	    if (expr->symbol->attr.intent == INTENT_IN) {
		g95_error("Variable '%s' in input list at %C cannot be "
			  "INTENT(IN)", expr->symbol->name);
		m = MATCH_ERROR;
		break;
	    }

	    if (expr->symbol->attr.flavor == FL_PARAMETER) {
		g95_error("Variable '%s' in input list at %C cannot be "
			  "a PARAMETER", expr->symbol->name);
		m = MATCH_ERROR;
		break;
	    }

	    if (g95_pure(NULL, 1) && g95_impure_variable(expr->symbol) &&
		current_dt->io_unit != NULL &&
		current_dt->io_unit->ts.type == BT_CHARACTER) {
		g95_error("Cannot read to variable '%s' in PURE procedure "
			  "at %C", expr->symbol->name);
		m = MATCH_ERROR;
		break;
	    }

	    if (g95_check_do_variable(expr->symbol, &expr->where)) {
		m = MATCH_ERROR;
		break;
	    }

	    break;

	case M_WRITE:
	    if (current_dt->io_unit != NULL &&
		current_dt->io_unit->ts.type == BT_CHARACTER &&
		g95_pure(NULL, 0) &&
		current_dt->io_unit->type == EXPR_VARIABLE &&
		g95_impure_variable(current_dt->io_unit->symbol)) {

		g95_error("Cannot write to internal file unit '%s' at %C "
			  "inside a PURE procedure",
			  current_dt->io_unit->symbol->name);
		m = MATCH_ERROR;
	    }

	    break;

	default:
	    break;
	}

    if (m != MATCH_YES) {
	g95_free_expr(expr);
	return MATCH_ERROR;
    }

    cp = g95_get_code(EXEC_TRANSFER, &expr->where);
    cp->expr = expr;
    cp->ext.transfer = k;

    *cpp = cp;
    return MATCH_YES;
}



/* match_io_list()-- Match an I/O list, building g95_code structures
 * as we go. */

static match match_io_list(io_kind k, int namelist, g95_code **head_p) {
g95_code *head, *tail, *new;
match m;

    *head_p = head = tail = NULL;
    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    if (namelist) {
	g95_error("NAMELIST I/O statement at %C must not have an I/O list");
	return MATCH_ERROR;
    }

    for(;;) {
	m = match_io_element(k, &new);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	tail = g95_append_code(tail, new);
	if (head == NULL) head = new;

	if (g95_match_eos() == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    *head_p = head;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in %s statement at %C", io_kind_name(k));

cleanup:
    g95_free_statements(head);
    return MATCH_ERROR;
}



/* terminate_io()-- Attach the data transfer end node */

static void terminate_io(g95_code *io_code) {
g95_code *c;

    if (io_code == NULL)
	io_code = &new_st;

    c = g95_get_code(EXEC_DT_END, NULL);
    c->ext.dt = new_st.ext.dt;  /* Point to structure that is already there */

    g95_append_code(io_code, c);
}



/* g95_match_read()-- Match a read statement */

match g95_match_read(void) {
int comma_ok, comma_flag;
g95_code *io_code;
g95_expr *e;
g95_dt *dt;
match m;

    m = MATCH_NO;
    current_dt = dt = g95_get_dt();
    dt->where = g95_current_locus;
    dt->type = M_READ;
    io_code = NULL;

    comma_ok = 0;
    comma_flag = 0;

    if (g95_match_char('(') != MATCH_YES) {
	m = match_dt_format(M_READ, 0, dt);
	switch(m) {
	case MATCH_YES:     break;
	case MATCH_NO:	    goto syntax;
	case MATCH_ERROR:   goto cleanup;
	}

	if (g95_match_eos() == MATCH_YES) {
	    dt->io_unit = default_unit();
	    dt->default_unit = 1;
	    goto done;
	}

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;

	dt->io_unit = default_unit();
	dt->default_unit = 1;
	goto get_io_list;
    }

    m = match_dt_element(M_READ, dt);
    switch(m) {
    case MATCH_YES:    comma_flag = 1; goto control_list;
    case MATCH_NO:
	if (G95_STRICT_F())
	    goto syntax;

	break;
    case MATCH_ERROR:  goto cleanup;
    }

    /* Match a naked unit number or a paren-enclosed format */

    m = g95_match_expr(&e);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) {   /* Vanilla matching */
	m = match_dt_element(M_WRITE, dt);
	switch(m) {
	case MATCH_ERROR:   goto cleanup;
	case MATCH_YES:     comma_flag = 1; goto control_list;
	case MATCH_NO:

	    m = match_dt_unit(dt);
	    switch(m) {
	    case MATCH_ERROR:  goto cleanup;
	    case MATCH_NO:     goto control_list;
	    case MATCH_YES:    break;
	    }

	    if (g95_match_char(')') == MATCH_YES) goto get_io_list;
	    if (g95_match_char(',') != MATCH_YES) goto syntax;

	    m = match_dt_element(M_WRITE, dt);
	    switch(m) {
	    case MATCH_ERROR:   goto cleanup;
	    case MATCH_YES:     comma_flag = 1; break;
	    case MATCH_NO:      break;
	    }

	    m = match_dt_format(M_WRITE, 1, dt);
	    switch(m) {
	    case MATCH_ERROR:   goto cleanup;
	    case MATCH_YES:     comma_flag = 1; break;
	    case MATCH_NO:      break;
	    }

	    goto control_list;
	}
    }

    g95_gobble_whitespace();
    switch(g95_next_char()) {
    case ')':
	break;

    case ',':
	dt->io_unit = e;

	m = match_dt_element(M_READ, dt);
	switch(m) {
	case MATCH_ERROR:   goto cleanup;
	case MATCH_YES:     comma_flag = 1; goto control_list;
	case MATCH_NO:      break;
	}

	m = match_dt_format(M_READ, 1, dt);
	switch(m) {
	case MATCH_ERROR:   goto cleanup;
	case MATCH_YES:     comma_flag = 1; break;
	case MATCH_NO:      break;
	}

	goto control_list;

    default:
	goto syntax;
    }

    if (g95_match_eos() == MATCH_YES) {
	dt->ambig = e;
	goto done;
    }

    if (g95_match_char(',') == MATCH_YES) {
	dt->format_expr = e;
	dt->io_unit = default_unit();
	dt->default_unit = 1;
    } else
	dt->io_unit = e;

    goto get_io_list;

control_list:
    for(;;) {
	if (comma_flag) {
	    if (g95_match_char(')') == MATCH_YES) break;
	    if (g95_match_char(',') != MATCH_YES) goto syntax;
	}

	m = match_dt_element(M_READ, dt);
	if (m == MATCH_NO)    goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	comma_flag = 1;
    }

    if (g95_match_eos() != MATCH_YES) {
    get_io_list:
	g95_match_char(',');

	m = match_io_list(M_READ, dt->namelist != NULL, &io_code);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO)    goto syntax;

	if (io_code != NULL)
	    dt->have_data_item = 1;
    }

done:
    new_st.type = EXEC_READ;
    new_st.ext.dt = dt;
    new_st.next = io_code;

    terminate_io(io_code);
    return MATCH_YES;

syntax:
    g95_error("Syntax error in READ statement at %C");
    m = MATCH_ERROR;

cleanup:
    g95_free_dt(dt);
    return m;
}



/* g95_match_write()-- Match a WRITE statement. */

match g95_match_write(void) {
g95_code *io_code;
int comma_flag;
g95_dt *dt;
match m;

    current_dt = dt = g95_get_dt();
    dt->where = g95_current_locus;
    dt->type = M_WRITE;
    io_code = NULL;

    m = MATCH_NO;
    if (g95_match_char('(') != MATCH_YES)
	goto cleanup;

    comma_flag = 0;

    m = match_dt_element(M_WRITE, dt);
    switch(m) {
    case MATCH_ERROR:   goto cleanup;
    case MATCH_YES:     comma_flag = 1; goto control_list;
    case MATCH_NO:
	if (G95_STRICT_F())
	    goto syntax;

	m = match_dt_unit(dt);
	switch(m) {
	case MATCH_ERROR:  goto cleanup;
	case MATCH_NO:     goto control_list;
	case MATCH_YES:    break;
	}

	if (g95_match_char(')') == MATCH_YES) goto get_io_list;
	if (g95_match_char(',') != MATCH_YES) goto syntax;

	m = match_dt_element(M_WRITE, dt);
	switch(m) {
	case MATCH_ERROR:  goto cleanup;
	case MATCH_NO:     break;
	case MATCH_YES:    comma_flag = 1;  goto control_list;
	}

	m = match_dt_format(M_WRITE, 1, dt);
	switch(m) {
	case MATCH_ERROR:   goto cleanup;
	case MATCH_NO:      goto control_list;
	case MATCH_YES:     comma_flag = 1; break;
	}

	break;
    }

control_list:
    for(;;) {
	if (comma_flag) {
	    if (g95_match_char(')') == MATCH_YES) break;
	    if (g95_match_char(',') != MATCH_YES) goto syntax;
	}

	m = match_dt_element(M_WRITE, dt);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;

	comma_flag = 1;
    }

get_io_list:
    if (g95_match_eos() != MATCH_YES) {
	g95_match_char(',');

	m = match_io_list(M_WRITE, dt->namelist != NULL, &io_code);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO)    goto syntax;

	if (io_code != NULL)
	    dt->have_data_item = 1;
    }

    if (dt->io_unit != NULL && dt->io_unit->type == EXPR_VARIABLE &&
	dt->io_unit->ts.type == BT_CHARACTER &&
	dt->io_unit->symbol->attr.intent == INTENT_IN) {
	g95_error("Internal file '%s' at %L is INTENT(IN)",
		  dt->io_unit->symbol->name, &dt->io_unit->where);
	m = MATCH_ERROR;
	goto cleanup;
    }

    new_st.type = EXEC_WRITE;
    new_st.ext.dt = dt;
    new_st.next = io_code;

    terminate_io(io_code);
    return MATCH_YES;

syntax:
    g95_error("Syntax error in WRITE statement at %C");
    m = MATCH_ERROR;

cleanup:
    g95_free_dt(dt);
    return m;
}



/* g95_match_print()-- Match a PRINT statement. */

match g95_match_print(void) {
g95_code *io_code;
g95_dt *dt;
match m;

    current_dt = dt = g95_get_dt();

    dt->where = g95_current_locus;
    dt->type = M_PRINT;
    io_code = NULL;

    m = match_dt_format(M_PRINT, 0, dt);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO)    goto syntax;

    dt->io_unit = default_unit();
    dt->default_unit = 1;

    if (g95_match_eos() != MATCH_YES) {
	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;

	m = match_io_list(M_PRINT, dt->namelist != NULL, &io_code);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO)    goto syntax;

	if (g95_pure(NULL, 0)) {
	    g95_error("PRINT statement at %C not allowed within PURE "
		      "procedure");
	    m = MATCH_ERROR;
	    goto cleanup;
	}
    }

    new_st.type = EXEC_WRITE;
    new_st.ext.dt = dt;
    new_st.next = io_code;

    terminate_io(io_code);

    return MATCH_YES;

syntax:
    g95_error("Syntax error in PRINT statement at %C");
    m = MATCH_ERROR;

cleanup:
    g95_free_dt(dt);
    return m;
}



/* g95_free_flush()-- Free a g95_flush structure */

void g95_free_flush(g95_flush *f) {

    if (f == NULL)
	return;

    g95_free_expr(f->unit);
    g95_free_expr(f->iostat);
    g95_free_expr(f->iomsg);
}



/* g95_resolve_flush()-- Resolve a FLUSH statement. */

try g95_resolve_flush(g95_flush *f) {

    RESOLVE_TAG(&tag_unit,    f->unit,    0);
    RESOLVE_TAG(&tag_iostat,  f->iostat,  1);
    RESOLVE_TAG(&tag_iomsg,   f->iomsg,   1);

    return g95_reference_st_label(f->err, ST_LABEL_TARGET);
}



/* match_flush_element()-- Match part of a FLUSH statement */

static match match_flush_element(g95_flush *flush) {
match m;

    m = match_etag(&tag_unit, &flush->unit);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iostat, &flush->iostat);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iomsg, &flush->iomsg);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_err, &flush->err);
    if (m != MATCH_NO)
	return m;

    return MATCH_NO;
}



/* g95_match_flush()-- Match a FLUSH statement */

match g95_match_flush(void) {
g95_flush *flush;
match m;

    flush = g95_getmem(sizeof(g95_flush));

    if (g95_match_char('(') != MATCH_YES) {
	m = g95_match_expr(&flush->unit);
	if (m != MATCH_YES || g95_match_eos() != MATCH_YES)
	    goto syntax;

	return MATCH_YES;
    }

    m = match_flush_element(flush);

    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) {
	m = g95_match_expr(&flush->unit);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;
    }

    for(;;) {
	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;

	m = match_flush_element(flush);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

    if (g95_match_eos() == MATCH_NO)
	goto syntax;

    if (g95_pure(NULL, 1)) {
	g95_error("FLUSH statement not allowed in PURE procedure at %C");
	m = MATCH_ERROR;
	goto cleanup;
    }

    if (flush->unit == NULL) {
	g95_error("Missing unit specification in FLUSH statement at %C");
	m = MATCH_ERROR;
	goto cleanup;
    }

    new_st.type = EXEC_FLUSH;
    new_st.ext.flush = flush;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_FLUSH);

cleanup:
    g95_free_flush(flush);
    return m;
}



/* g95_free_inquire()-- Free a g95_inquire structure */

void g95_free_inquire(g95_inquire *inquire) {

    if (inquire == NULL)
	return;

    g95_free_expr(inquire->unit);
    g95_free_expr(inquire->file);
    g95_free_expr(inquire->iostat);
    g95_free_expr(inquire->iomsg);
    g95_free_expr(inquire->exist);
    g95_free_expr(inquire->opened);
    g95_free_expr(inquire->number);
    g95_free_expr(inquire->named);
    g95_free_expr(inquire->name);
    g95_free_expr(inquire->access);
    g95_free_expr(inquire->sequential);
    g95_free_expr(inquire->direct);
    g95_free_expr(inquire->form);
    g95_free_expr(inquire->formatted);
    g95_free_expr(inquire->unformatted);
    g95_free_expr(inquire->recl);
    g95_free_expr(inquire->nextrec);
    g95_free_expr(inquire->blank);
    g95_free_expr(inquire->position);
    g95_free_expr(inquire->action);
    g95_free_expr(inquire->read);
    g95_free_expr(inquire->write);
    g95_free_expr(inquire->readwrite);
    g95_free_expr(inquire->delim);
    g95_free_expr(inquire->pad);
    g95_free_expr(inquire->pos);
    g95_free_expr(inquire->iolength);
    g95_free_expr(inquire->size);
    g95_free_expr(inquire->stream);

    g95_free(inquire);
}



/* match_inquire_element()-- Match an element of an INQUIRE statement */

static match match_inquire_element(g95_inquire *inquire) {
match m;

    m = match_etag(&tag_unit, &inquire->unit);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_file, &inquire->file);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_err, &inquire->err);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iostat, &inquire->iostat);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iomsg, &inquire->iomsg);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_exist, &inquire->exist);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_opened, &inquire->opened);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_named, &inquire->named);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_name, &inquire->name);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_number, &inquire->number);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_s_access, &inquire->access);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_sequential, &inquire->sequential);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_direct, &inquire->direct);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_s_form, &inquire->form);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_formatted, &inquire->formatted);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_unformatted, &inquire->unformatted);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_s_recl, &inquire->recl);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_nextrec, &inquire->nextrec);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_s_blank, &inquire->blank);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_s_position, &inquire->position);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_s_action, &inquire->action);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_read, &inquire->read);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_write, &inquire->write);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_readwrite, &inquire->readwrite);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_s_delim, &inquire->delim);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_s_pad, &inquire->pad);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_stream, &inquire->stream);
    if (m != MATCH_NO)
	return m;

    m = match_pos_tag(0, &inquire->pos);
    if (m != MATCH_NO)
	return m;

    m = match_vtag(&tag_iolength, &inquire->iolength);
    if (m != MATCH_NO)
	return m;

    if (G95_STRICT_F95())
	return MATCH_NO;

    m = match_vtag(&tag_size, &inquire->size);
    if (m != MATCH_NO)
	return m;
    
    return MATCH_NO;
}



match g95_match_inquire(void) {
g95_inquire *inquire;
g95_code *code;
match m;

    m = g95_match_char('(');
    if (m == MATCH_NO)
	return m;

    inquire = g95_getmem(sizeof(g95_inquire));
    inquire->where = g95_current_locus;

    m = match_inquire_element(inquire);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) {
	if (G95_STRICT_F())
	    goto syntax;

	m = g95_match_expr(&inquire->unit);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

/* See if we have the IOLENGTH form of the inquire statement */

    if (inquire->iolength != NULL) {
	if (g95_match_char(')') != MATCH_YES) goto syntax;

	m = match_io_list(M_INQUIRE, 0, &code);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	terminate_io(code);

	new_st.type = EXEC_IOLENGTH;
	new_st.expr = inquire->iolength;
	g95_free(inquire);

	if (g95_pure(NULL, 1)) {
	    g95_free_statements(code);
	    g95_error("INQUIRE statement not allowed in PURE procedure at %C");
	    return MATCH_ERROR;
	}

	new_st.next = code;
	return MATCH_YES;
    }

/* At this point, we have the non-IOLENGTH inquire statement */

    for(;;) {
	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;

	m = match_inquire_element(inquire);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;

	if (inquire->iolength != NULL) {
	    g95_error("IOLENGTH tag invalid in INQUIRE statement at %C");
	    goto cleanup;
	}
    }

    if (g95_match_eos() != MATCH_YES)
	goto syntax;

    if (g95_pure(NULL, 1)) {
	g95_error("INQUIRE statement not allowed in PURE procedure at %C");
	return MATCH_ERROR;
    }

    new_st.type = EXEC_INQUIRE;
    new_st.ext.inquire = inquire;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_INQUIRE);

cleanup:
    g95_free_inquire(inquire);
    return MATCH_ERROR;
}



/* g95_resolve_inquire()-- Resolve everything in a g95_inquire structure */

try g95_resolve_inquire(g95_inquire *inquire) {

    RESOLVE_TAG(&tag_unit,         inquire->unit,         0);
    RESOLVE_TAG(&tag_file,         inquire->file,         0);
    RESOLVE_TAG(&tag_iostat,       inquire->iostat,       1);
    RESOLVE_TAG(&tag_iomsg,        inquire->iomsg,        1);
    RESOLVE_TAG(&tag_exist,        inquire->exist,        1);
    RESOLVE_TAG(&tag_opened,       inquire->opened,       1);
    RESOLVE_TAG(&tag_number,       inquire->number,       1);
    RESOLVE_TAG(&tag_named,        inquire->named,        1);
    RESOLVE_TAG(&tag_name,         inquire->name,         1);
    RESOLVE_TAG(&tag_s_access,     inquire->access,       1);
    RESOLVE_TAG(&tag_sequential,   inquire->sequential,   1);
    RESOLVE_TAG(&tag_direct,       inquire->direct,       1);
    RESOLVE_TAG(&tag_s_form,       inquire->form,         1);
    RESOLVE_TAG(&tag_formatted,    inquire->formatted,    1);
    RESOLVE_TAG(&tag_unformatted,  inquire->unformatted,  1);
    RESOLVE_TAG(&tag_s_recl,       inquire->recl,         1);
    RESOLVE_TAG(&tag_nextrec,      inquire->nextrec,      1);
    RESOLVE_TAG(&tag_s_blank,      inquire->blank,        1);
    RESOLVE_TAG(&tag_s_position,   inquire->position,     1);
    RESOLVE_TAG(&tag_s_action,     inquire->action,       1);
    RESOLVE_TAG(&tag_read,         inquire->read,         1);
    RESOLVE_TAG(&tag_write,        inquire->write,        1);
    RESOLVE_TAG(&tag_readwrite,    inquire->readwrite,    1);
    RESOLVE_TAG(&tag_s_delim,      inquire->delim,        1);
    RESOLVE_TAG(&tag_s_pad,        inquire->pad,          1);
    RESOLVE_TAG(&tag_size,         inquire->size,         1);
    RESOLVE_TAG(&tag_stream,       inquire->stream,       1);
  /* The IOLENGTH form generates an EXEC_IOLENGTH node */

    if (g95_reference_st_label(inquire->err, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    if (inquire->file == NULL && inquire->unit == NULL) {
	g95_error("One of FILE or UNIT must be specified in INQUIRE statement "
		  "at %L", &inquire->where);
	return FAILURE;
    }

    if (inquire->file != NULL && inquire->unit != NULL) {
	g95_error("Cannot specify both FILE and UNIT in INQUIRE statement "
		  "at %L", &inquire->where);
	return FAILURE;
    }

    if (G95_STRICT_F()) {
	if (inquire->pad != NULL) {
	    g95_error("PAD tag in INQUIRE statement at %L not permitted in "
		      "F mode", &inquire->pad->where);
	    return FAILURE;
	}

	if (inquire->delim != NULL) {
	    g95_error("DELIM tag in INQUIRE statement at %L not permitted in "
		      "F mode", &inquire->delim->where);
	    return FAILURE;
	}

	if (inquire->blank != NULL) {
	    g95_error("BLANK tag in INQUIRE statement at %L not permitted in "
		      "F mode", &inquire->blank->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* g95_free_wait()-- Free a g95_wait structure */

void g95_free_wait(g95_wait *wait) {

    if (wait == NULL)
	return;

    g95_free_expr(wait->unit);
    g95_free_expr(wait->id);
    g95_free_expr(wait->iostat);
    g95_free_expr(wait->iomsg);
}



/* g95_resolve_wait()-- Resolve a WAIT statement. */

try g95_resolve_wait(g95_wait *wait) {

    RESOLVE_TAG(&tag_unit,    wait->unit,    0);
    RESOLVE_TAG(&tag_id,      wait->id,      0);
    RESOLVE_TAG(&tag_iostat,  wait->iostat,  1);
    RESOLVE_TAG(&tag_iomsg,   wait->iomsg,   1);

    if (g95_reference_st_label(wait->err, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    if (g95_reference_st_label(wait->end, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    if (g95_reference_st_label(wait->eor, ST_LABEL_TARGET) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* match_wait_element()-- Match part of a WAIT statement */

static match match_wait_element(g95_wait *wait) {
match m;

    m = match_etag(&tag_unit, &wait->unit);
    if (m != MATCH_NO)
	return m;

    m = match_etag(&tag_id, &wait->id);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iostat, &wait->iostat);
    if (m != MATCH_NO)
	return m;

    m = match_out_tag(&tag_iomsg, &wait->iomsg);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_err, &wait->err);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_end, &wait->end);
    if (m != MATCH_NO)
	return m;

    m = match_ltag(&tag_eor, &wait->eor);
    if (m != MATCH_NO)
	return m;

    return MATCH_NO;
}



/* g95_match_wait()-- Match a WAIT statement */

match g95_match_wait(void) {
g95_wait *wait;
match m;

    if (g95_match_char('(') != MATCH_YES)
	return MATCH_NO;

    wait = g95_getmem(sizeof(g95_wait));
    m = match_wait_element(wait);

    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) {
	m = g95_match_expr(&wait->unit);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;
    }

    for(;;) {
	if (g95_match_char(')') == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;

	m = match_wait_element(wait);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
    }

    if (g95_match_eos() == MATCH_NO)
	goto syntax;

    if (g95_pure(NULL, 1)) {
	g95_error("WAIT statement not allowed in PURE procedure at %C");
	goto cleanup;
    }

    if (wait->unit == NULL) {
	g95_error("Missing unit specification in WAIT statement at %C");
	goto cleanup;
    }

    new_st.type = EXEC_WAIT;
    new_st.ext.wait = wait;
    return MATCH_YES;

syntax:
    g95_syntax_error(ST_WAIT);

cleanup:
    g95_free_wait(wait);
    return MATCH_ERROR;
}



/* g95_io_init()-- Initialize I/O things. */

void g95_io_init(void) {

    g95_format_asterisk.value  = -1;
    g95_format_asterisk.format = NULL;
    g95_format_asterisk.length = 0;
}

