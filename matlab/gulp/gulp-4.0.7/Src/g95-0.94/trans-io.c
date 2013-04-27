/* IO Code translation/library interface
   Copyright (C) 2000-2008 Free Software Foundation, Inc.
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

/* trans-io.c-- generate GCC trees from g95_code */

#include "trans.h"

static tree default_integer_pointer;

/* The global I/O variables */

static tree ioparm_var, library_return;

static stmtblock_t dt_post;
g95_frame_t g95_frame_type;



/* Members of the ioparm structure */

static tree ioparm_unit, ioparm_unit_kind, ioparm_err, ioparm_end, ioparm_eor,
    ioparm_list_format, ioparm_library_return, ioparm_sloppy_char,
    ioparm_endian, ioparm_exist, ioparm_exist_kind, ioparm_opened,
    ioparm_opened_kind, ioparm_named, ioparm_named_kind, ioparm_iostat,
    ioparm_iostat_kind, ioparm_number, ioparm_number_kind, ioparm_nextrec,
    ioparm_nextrec_kind, ioparm_size, ioparm_size_kind, ioparm_recl_in,
    ioparm_recl_in_kind, ioparm_rec, ioparm_rec_kind, ioparm_recl_out,
    ioparm_file, ioparm_file_len, ioparm_status, ioparm_status_len,
    ioparm_access, ioparm_access_len, ioparm_form, ioparm_form_len,
    ioparm_blank, ioparm_blank_len, ioparm_position, ioparm_position_len,
    ioparm_action, ioparm_action_len, ioparm_delim, ioparm_delim_len,
    ioparm_pad, ioparm_pad_len, ioparm_format, ioparm_format_len,
    ioparm_advance, ioparm_advance_len, ioparm_filename, ioparm_filename_len,
    ioparm_internal_unit, ioparm_internal_unit_len, ioparm_internal_array,
    ioparm_namelist, ioparm_sequential, ioparm_sequential_len, ioparm_direct,
    ioparm_direct_len, ioparm_formatted, ioparm_formatted_len,
    ioparm_unformatted, ioparm_unformatted_len, ioparm_read,
    ioparm_read_len, ioparm_write, ioparm_write_len, ioparm_readwrite,
    ioparm_readwrite_len, ioparm_iomsg, ioparm_iomsg_len, ioparm_pos,
    ioparm_pos_kind, ioparm_prev, ioparm_decimal, ioparm_decimal_len,
    ioparm_convert, ioparm_convert_len, ioparm_stream, ioparm_stream_len;

/* Derived type descriptor type and components */

static tree derived_desc_type, dd_name, dd_type, dd_kind, dd_component,
            dd_offset, dd_rank, dd_shape;

static tree generate_iodesc(g95_symbol *);


#define ADD_FIELD(name, type) \
  ioparm_ ## name = g95_add_field(ioparm_type, stringize(name), type)

#define ADD_STRING(name) \
   ioparm_ ## name = \
      g95_add_field(ioparm_type, stringize(name), pchar_type_node); \
   ioparm_ ## name ## _len = \
      g95_add_field(ioparm_type, stringize(name ## _len), g95_default_integer);

/* Variable for keeping track of what the last data transfer statement
 * was.  Used for deciding which subroutine to call when the data
 * transfer is complete. */

static enum { READ, WRITE, IOLENGTH } last_dt;


/* g95_init_io()-- Create function decls for IO library functions.  */

void g95_init_io(void) {
tree tmp, ioparm_type;

    default_integer_pointer = build_pointer_type(g95_default_integer);

/* Build the st_parameter structure.  Information associated with I/O
 * calls are transferred here.  This must match the one defined in the
 * library exactly. */

    ioparm_type = make_node(RECORD_TYPE);
    TYPE_NAME(ioparm_type) = get_identifier(PREFIX "ioparm");

    ADD_FIELD(unit,            pvoid_type_node);
    ADD_FIELD(unit_kind,       g95_default_integer);
    ADD_FIELD(err,             g95_default_integer);
    ADD_FIELD(end,             g95_default_integer);
    ADD_FIELD(eor,             g95_default_integer);
    ADD_FIELD(list_format,     g95_default_integer);
    ADD_FIELD(library_return,  g95_default_integer);
    ADD_FIELD(sloppy_char,     g95_default_integer);
    ADD_FIELD(endian,          g95_default_integer);

    ADD_FIELD(exist,           default_integer_pointer);
    ADD_FIELD(exist_kind,      g95_default_integer);

    ADD_FIELD(opened,          default_integer_pointer);
    ADD_FIELD(opened_kind,     g95_default_integer);

    ADD_FIELD(named,           default_integer_pointer);
    ADD_FIELD(named_kind,      g95_default_integer);

    ADD_FIELD(iostat,          default_integer_pointer);
    ADD_FIELD(iostat_kind,     g95_default_integer);

    ADD_FIELD(number,          default_integer_pointer);
    ADD_FIELD(number_kind,     g95_default_integer);

    ADD_FIELD(nextrec,         default_integer_pointer);
    ADD_FIELD(nextrec_kind,    g95_default_integer);

    ADD_FIELD(size,            default_integer_pointer);
    ADD_FIELD(size_kind,       g95_default_integer);

    ADD_FIELD(recl_in,         pvoid_type_node);
    ADD_FIELD(recl_in_kind,    g95_default_integer);
    ADD_FIELD(rec,             pvoid_type_node);
    ADD_FIELD(rec_kind,        g95_default_integer);
    ADD_FIELD(recl_out,        default_integer_pointer);

    ADD_STRING(file);
    ADD_STRING(status);
    ADD_STRING(access);
    ADD_STRING(form);
    ADD_STRING(blank);
    ADD_STRING(position);
    ADD_STRING(action);
    ADD_STRING(delim);
    ADD_STRING(pad);
    ADD_STRING(format);
    ADD_STRING(advance);
    ADD_STRING(filename);
    ADD_STRING(decimal);
    ADD_STRING(internal_unit);
    ADD_FIELD(internal_array, pvoid_type_node);
    ADD_FIELD(namelist, pchar_type_node);
    ADD_STRING(sequential);
    ADD_STRING(stream);

    ADD_STRING(direct);
    ADD_STRING(formatted);
    ADD_STRING(unformatted);
    ADD_STRING(read);
    ADD_STRING(write);
    ADD_STRING(readwrite);
    ADD_STRING(iomsg);
    ADD_FIELD(pos, pvoid_type_node);
    ADD_FIELD(pos_kind, g95_default_integer);
    ADD_STRING(convert);
    ADD_FIELD(prev, pvoid_type_node);

    g95_finish_type(ioparm_type);

    ioparm_var = build_decl(VAR_DECL, get_identifier(PREFIX "ioparm"),
			    build_pointer_type(ioparm_type));
    DECL_EXTERNAL(ioparm_var) = 1;
    TREE_PUBLIC(ioparm_var) = 1;

    library_return = build_decl(VAR_DECL,
				get_identifier(PREFIX "library_return"),
				g95_default_integer);
    DECL_EXTERNAL(library_return) = 1;
    TREE_PUBLIC(library_return) = 1;

    g95_locus_line = build_decl(VAR_DECL, get_identifier(PREFIX "line"),
				g95_default_integer);
    DECL_EXTERNAL(g95_locus_line) = 1;
    TREE_PUBLIC(g95_locus_line) = 1;

    g95_locus_file = build_decl(VAR_DECL, get_identifier(PREFIX "filename"),
				pchar_type_node);
    DECL_EXTERNAL(g95_locus_file) = 1;
    TREE_PUBLIC(g95_locus_file) = 1;

  /**************/

    derived_desc_type = make_node(RECORD_TYPE);

    dd_name = g95_add_field(derived_desc_type, "name", pchar_type_node);
    dd_type = g95_add_field(derived_desc_type, "type", g95_default_integer);
    dd_kind = g95_add_field(derived_desc_type, "kind", g95_default_integer);

    dd_offset = g95_add_field(derived_desc_type, "offset",g95_default_integer);
    dd_rank   = g95_add_field(derived_desc_type, "rank", g95_default_integer);

    tmp = build_pointer_type(g95_default_integer);
    dd_shape = g95_add_field(derived_desc_type, "shape", tmp);

    dd_component = g95_add_field(derived_desc_type, "info", pvoid_type_node);

    g95_finish_type(derived_desc_type);

    /**************/

    g95_frame_type.type = make_node(RECORD_TYPE);

    g95_frame_type.filename =
	g95_add_field(g95_frame_type.type, "filename", pchar_type_node);

    g95_frame_type.next =
	g95_add_field(g95_frame_type.type, "next", pvoid_type_node);

    g95_frame_type.line =
	g95_add_field(g95_frame_type.type, "line", g95_default_integer);

    g95_finish_type(g95_frame_type.type);

    g95_frame_type.base = build_decl(VAR_DECL, get_identifier(PREFIX "base"),
				     pvoid_type_node);
    DECL_EXTERNAL(g95_frame_type.base) = 1;
    TREE_PUBLIC(g95_frame_type.base) = 1;
}



/* set_parameter_ref()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is pass by reference. */

static void set_parameter_ref(stmtblock_t *block, stmtblock_t *postblock,
			      tree var, g95_expr *e) {
g95_se se;
tree tmp;

    g95_init_se(&se, NULL);
    se.reflevel = 1;
    g95_conv_expr(&se, e);

    g95_add_block_to_block(block, &se.pre);
    g95_add_block_to_block(postblock, &se.post);

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(TREE_TYPE(var), tmp, var);
    g95_add_modify_expr(block, tmp, se.expr);
}


/* set_integer_parameter()-- Set an integer pointer/kind parameter */

static void set_integer_parameter(stmtblock_t *block, stmtblock_t *post_block,
				  tree var, tree kind_var, g95_expr *e) {
g95_se se;
tree tmp;

    g95_init_se(&se, NULL);
    se.reflevel = 1;

    g95_conv_expr(&se, e);

    g95_add_block_to_block(block, &se.pre);

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(TREE_TYPE(var), tmp, var);
    g95_add_modify_expr(block, tmp, se.expr);

    g95_add_block_to_block(post_block, &se.post);

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(TREE_TYPE(kind_var), tmp, kind_var);
    g95_add_modify_expr(block, tmp, g95_build_int(e->ts.kind, 0));
}



/* set_string_parameter()-- Store a string parameter to the right
 * place. */

static void set_string_parameter(stmtblock_t *block, stmtblock_t *postblock,
				 tree var, tree len_var, g95_expr *e) {
g95_se se;
tree tmp;

    g95_init_se(&se, NULL);
    se.reflevel = 1;

    if (e->rank == 0) {
	g95_conv_expr(&se, e);

	g95_add_block_to_block(block, &se.pre);
	g95_add_block_to_block(postblock, &se.post);

	tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
	tmp = g95_build_component(TREE_TYPE(var), tmp, var);
	g95_add_modify_expr(block, tmp, se.expr);

	tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
	tmp = g95_build_component(TREE_TYPE(var), tmp, len_var);
	g95_add_modify_expr(block, tmp, se.string_length);

    } else {
	tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
	tmp = g95_build_component(TREE_TYPE(var), tmp, len_var);

	g95_conv_nondesc(&se, INTENT_IN, e, tmp);

	g95_add_block_to_block(block, &se.pre);
	g95_add_block_to_block(postblock, &se.post);

	tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
	tmp = g95_build_component(TREE_TYPE(var), tmp, var);
	g95_add_modify_expr(block, tmp, se.expr);
    }
}



/* set_flag()-- Set a member of the ioparm structure to one. */

static void set_flag(stmtblock_t *block, tree var) {
tree tmp;

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(TREE_TYPE(var), tmp, var);
    g95_add_modify_expr(block, tmp, integer_one_node);
}



/* array_format()-- Deal with a format expression that is a character
 * array. */

static void array_format(stmtblock_t *block, stmtblock_t *post_block,
			 g95_expr *format) {
g95_se se;
tree tmp;

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(g95_default_integer, tmp, ioparm_format_len);

    g95_init_se(&se, NULL);
    se.reflevel = 1;
    g95_conv_nondesc(&se, INTENT_IN, format, tmp);

    g95_add_block_to_block(block, &se.pre);

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(pchar_type_node, tmp, ioparm_format);
    g95_add_modify_expr(block, tmp, se.expr);

    g95_add_block_to_block(post_block, &se.post);
}



/* internal_array()-- Evaluate an internal unit that is a character array */

static void internal_array(stmtblock_t *block, stmtblock_t *post_block,
			   g95_expr *unit) {
g95_se se;
tree tmp;

    g95_init_se(&se, NULL); 
    se.reflevel = 1;
    g95_conv_descriptor(&se, unit, 0);

    g95_add_block_to_block(block, &se.pre);
  
    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(TREE_TYPE(se.expr), tmp, ioparm_internal_array);
    g95_add_modify_expr(block, tmp, se.expr);

    g95_add_block_to_block(post_block, &se.post);
}



/* add_case()-- Add a case to a IO-result switch */

static void add_case(int label_value, g95_st_label *label, stmtblock_t *body,
		     int have_iostat, g95_locus *where) {
tree tmp, value;

    if (label_value == 2 && label == NULL && !have_iostat) {
	value = g95_build_int(label_value, 0);

	tmp = g95_case_label(value, NULL_TREE);
	g95_add_expr_to_block(body, tmp);

	g95_set_locus(body, where);

	tmp = g95_call_library(void_type_node, PREFIX "unhandled_eof",
			       NULL_TREE);
	g95_add_expr_to_block(body, tmp);
	return;
    }

    if (label == NULL)
	return;   /* No label, no case */

    value = g95_build_int(label_value, 0);

    tmp = g95_case_label(value, NULL_TREE);
    g95_add_expr_to_block(body, tmp);

    tmp = g95_build_goto(g95_get_label_decl(label));
    g95_add_expr_to_block(body, tmp);
}



/* io_result()-- Generate a switch statement that branches to the
 * correct I/O result label.  The last statement of an I/O call stores
 * the result into a variable because there is often cleanup that must
 * be done before the switch, so a temporary would have to be created
 * anyway. */

static void io_result(stmtblock_t *block, g95_st_label *err_label,
		      g95_st_label *end_label, g95_st_label *eor_label,
		      int have_iostat, g95_locus *where) {
stmtblock_t body;
tree tmp;

    /* If no labels are specified, ignore the result instead of building
     * an empty switch. */

    if (err_label == NULL && end_label == NULL && eor_label == NULL)
	return;

    /* Build a switch statement */

    g95_init_block(&body);

    /* The label values here must be the same as the values in the
     * library_return enum in the runtime library */

    add_case(1, err_label, &body, 0, NULL);
    add_case(2, end_label, &body, have_iostat, where);
    add_case(3, eor_label, &body, 0, NULL);

    tmp = g95_finish_block(&body);
    tmp = g95_build_switch(library_return, tmp);

    g95_add_expr_to_block(block, tmp);
}



/* start_io()-- Common code for starting an I/O operation. */

static void start_io(stmtblock_t *block, g95_locus *where) {
tree tmp;

    tmp = g95_call_library(void_type_node, PREFIX "get_ioparm", NULL_TREE);
    g95_add_expr_to_block(block, tmp);

    g95_set_locus(block, where);
}



/* g95_trans_open()-- Translate an OPEN statement */

tree g95_trans_open(g95_code *code) {
stmtblock_t block, post_block;
g95_open *p;
tree tmp;

    g95_init_block(&block);
    g95_init_block(&post_block);

    start_io(&block, &code->where);
    p = code->ext.open;

    if (p->unit)
	set_integer_parameter(&block, &post_block, ioparm_unit,
			      ioparm_unit_kind, p->unit);

    if (p->file)
	set_string_parameter(&block, &post_block, ioparm_file, ioparm_file_len,
			     p->file);

    if (p->status)
	set_string_parameter(&block, &post_block, ioparm_status,
			     ioparm_status_len, p->status);

    if (p->access)
	set_string_parameter(&block, &post_block, ioparm_access,
			     ioparm_access_len, p->access);

    if (p->form)
	set_string_parameter(&block, &post_block, ioparm_form,
			     ioparm_form_len, p->form);

    if (p->recl)
	set_integer_parameter(&block, &post_block, ioparm_recl_in,
			      ioparm_recl_in_kind, p->recl);

    if (p->blank)
	set_string_parameter(&block, &post_block, ioparm_blank,
			     ioparm_blank_len, p->blank);

    if (p->position)
	set_string_parameter(&block, &post_block, ioparm_position,
			     ioparm_position_len, p->position);

    if (p->action)
	set_string_parameter(&block, &post_block, ioparm_action,
			     ioparm_action_len, p->action);

    if (p->delim)
	set_string_parameter(&block, &post_block, ioparm_delim,
			     ioparm_delim_len, p->delim);

    if (p->pad)
	set_string_parameter(&block, &post_block, ioparm_pad,
			     ioparm_pad_len, p->pad);

    if (p->decimal)
	set_string_parameter(&block, &post_block, ioparm_decimal,
			     ioparm_decimal_len, p->decimal);

    if (p->iostat)
	set_integer_parameter(&block, &post_block, ioparm_iostat,
			      ioparm_iostat_kind, p->iostat);

    if (p->iomsg)
	set_string_parameter(&block, &post_block, ioparm_iomsg,
			     ioparm_iomsg_len, p->iomsg);

    if (p->convert)
	set_string_parameter(&block, &post_block, ioparm_convert,
			     ioparm_convert_len, p->convert);

    if (p->err)
	set_flag(&block, ioparm_err);

    tmp = g95_call_library(void_type_node, PREFIX "st_open", NULL_TREE);
    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &post_block);

    io_result(&block, p->err, NULL, NULL, p->iostat != NULL, &code->where);

    return g95_finish_block(&block);
}



/* g95_trans_close()-- Translate a CLOSE statement */

tree g95_trans_close(g95_code *code) {
stmtblock_t block, post_block;
g95_close *p;
tree tmp;

    g95_init_block(&block);
    g95_init_block(&post_block);

    start_io(&block, &code->where);
    p = code->ext.close;

    if (p->unit)
	set_integer_parameter(&block, &post_block, ioparm_unit,
			      ioparm_unit_kind, p->unit);

    if (p->status)
	set_parameter_ref(&block, &post_block, ioparm_status, p->status);

    if (p->iostat)
	set_integer_parameter(&block, &post_block, ioparm_iostat,
			      ioparm_iostat_kind, p->iostat);

    if (p->iomsg)
	set_string_parameter(&block, &post_block, ioparm_iomsg,
			     ioparm_iomsg_len, p->iomsg);

    if (p->err)
	set_flag(&block, ioparm_err);

    tmp = g95_call_library(void_type_node, PREFIX "st_close", NULL_TREE);
    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &post_block);

    io_result(&block, p->err, NULL, NULL, p->iostat != NULL, &code->where);

    return g95_finish_block(&block);
}



/* g95_trans_inquire()-- Translate the non-IOLENGTH form of an INQUIRE
 * statement */

tree g95_trans_inquire(g95_code *code) {
stmtblock_t block, post_block;
g95_inquire *p;
tree tmp;

    g95_init_block(&block);
    g95_init_block(&post_block);

    start_io(&block, &code->where);
    p = code->ext.inquire;

    if (p->unit)
	set_integer_parameter(&block, &post_block, ioparm_unit,
			      ioparm_unit_kind, p->unit);

    if (p->file)
	set_string_parameter(&block, &post_block, ioparm_file,
			     ioparm_file_len, p->file);

    if (p->iostat)
	set_integer_parameter(&block, &post_block, ioparm_iostat,
			      ioparm_iostat_kind, p->iostat);

    if (p->iomsg)
	set_string_parameter(&block, &post_block, ioparm_iomsg,
			     ioparm_iomsg_len, p->iomsg);

    if (p->err)
	set_flag(&block, ioparm_err);

    if (p->exist)
	set_integer_parameter(&block, &post_block, ioparm_exist,
			      ioparm_exist_kind, p->exist);

    if (p->opened)
	set_integer_parameter(&block, &post_block, ioparm_opened,
			      ioparm_opened_kind, p->opened);

    if (p->named)
	set_integer_parameter(&block, &post_block, ioparm_named,
			      ioparm_named_kind, p->named);

    if (p->number)
	set_integer_parameter(&block, &post_block, ioparm_number,
			      ioparm_number_kind, p->number);

    if (p->name)
	set_string_parameter(&block, &post_block, ioparm_filename,
			     ioparm_filename_len, p->name);

    if (p->access)
	set_string_parameter(&block, &post_block, ioparm_access,
			     ioparm_access_len, p->access);

    if (p->sequential)
	set_string_parameter(&block, &post_block, ioparm_sequential,
			     ioparm_sequential_len, p->sequential);

    if (p->direct)
	set_string_parameter(&block, &post_block, ioparm_direct,
			     ioparm_direct_len, p->direct);

    if (p->form)
	set_string_parameter(&block, &post_block, ioparm_form,
			     ioparm_form_len, p->form);

    if (p->formatted)
	set_string_parameter(&block, &post_block, ioparm_formatted,
			     ioparm_formatted_len, p->formatted);

    if (p->unformatted)
	set_string_parameter(&block, &post_block, ioparm_unformatted,
			     ioparm_unformatted_len, p->unformatted);

    if (p->recl)
	set_parameter_ref(&block, &post_block, ioparm_recl_out, p->recl);

    if (p->nextrec)
	set_integer_parameter(&block, &post_block, ioparm_nextrec,
			      ioparm_nextrec_kind, p->nextrec);

    if (p->blank)
	set_string_parameter(&block, &post_block, ioparm_blank,
			     ioparm_blank_len, p->blank);

    if (p->position)
	set_string_parameter(&block, &post_block, ioparm_position,
			     ioparm_position_len, p->position);

    if (p->action)
	set_string_parameter(&block, &post_block, ioparm_action,
			     ioparm_action_len, p->action);

    if (p->read)
	set_string_parameter(&block, &post_block, ioparm_read,
			     ioparm_read_len, p->read);

    if (p->write)
	set_string_parameter(&block, &post_block, ioparm_write,
			     ioparm_write_len, p->write);

    if (p->readwrite)
	set_string_parameter(&block, &post_block, ioparm_readwrite,
			     ioparm_readwrite_len, p->readwrite);

    if (p->delim)
	set_string_parameter(&block, &post_block, ioparm_delim,
			     ioparm_delim_len, p->delim);

    if (p->pad)
	set_string_parameter(&block, &post_block, ioparm_pad,
			     ioparm_pad_len, p->pad);

    if (p->pos)
	set_integer_parameter(&block, &post_block, ioparm_pos, ioparm_pos_kind,
			      p->pos);

    if (p->size)
	set_integer_parameter(&block, &post_block, ioparm_size,
			      ioparm_size_kind, p->size);
    if (p->stream)
	set_string_parameter(&block, &post_block, ioparm_stream,
			     ioparm_stream_len, p->stream);

    tmp = g95_call_library(void_type_node, PREFIX "st_inquire", NULL_TREE);
    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &post_block);

    io_result(&block, p->err, NULL, NULL, p->iostat != NULL, &code->where);

    return g95_finish_block(&block);
}



/* set_endian()-- Set the endian flag if present */

static void set_endian(stmtblock_t *block) {
tree tmp;

    if (g95_option.endian == 0)
	return;

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(TREE_TYPE(ioparm_endian), tmp, ioparm_endian);

    g95_add_modify_expr(block, tmp, g95_build_int(g95_option.endian, 0));
}



/* build_filepos()-- Common subroutine for building a file positioning
 * statement */

static tree build_filepos(char *function, g95_code *code) {
stmtblock_t block, post_block;
g95_filepos *p;
tree tmp;

    p = code->ext.filepos; 

    g95_init_block(&block);
    g95_init_block(&post_block);

    start_io(&block, &code->where);

    if (p->unit)
	set_integer_parameter(&block, &post_block, ioparm_unit,
			      ioparm_unit_kind, p->unit);

    if (p->iostat)
	set_integer_parameter(&block, &post_block, ioparm_iostat,
			      ioparm_iostat_kind, p->iostat);

    if (p->iomsg)
	set_string_parameter(&block, &post_block, ioparm_iomsg,
			     ioparm_iomsg_len, p->iomsg);

    if (p->err)
	set_flag(&block, ioparm_err);

    set_endian(&block);

    tmp = g95_call_library(void_type_node, function, NULL_TREE);
    g95_add_expr_to_block(&block, tmp);
    g95_add_block_to_block(&block, &post_block);

    io_result(&block, p->err, NULL, NULL, p->iostat != NULL, &code->where);

    return g95_finish_block(&block);
}



/* g95_trans_backspace()-- Translate a BACKSPACE statement */

tree g95_trans_backspace(g95_code *code) {

    return build_filepos(PREFIX "st_backspace", code);
}



/* g95_trans_endfile()-- Translate an ENDFILE statement */

tree g95_trans_endfile(g95_code *code) {

    return build_filepos(PREFIX "st_endfile", code);
}



/* g95_trans_rewind()-- Translate a REWIND statement */

tree g95_trans_rewind(g95_code *code) {

    return build_filepos(PREFIX "st_rewind", code);
}



/* build_namelist()-- Build calls to initialize a namelist data
 * transfer. */

static tree build_namelist(g95_symbol *sym) {
tree type, name, dt_info, kind, tmp;
stmtblock_t block;
g95_namelist *n;
g95_se se;
int t;

    g95_init_block(&block);

    name = g95_build_string_const(strlen(sym->name)+1, sym->name);
    TREE_ADDRESSABLE(name) = 1;

    name = g95_addr_expr(name);

    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)), ioparm_var);
    tmp = g95_build_component(pchar_type_node, tmp, ioparm_namelist);
    g95_add_modify_expr(&block, tmp, name);

    for(n=sym->namelist; n; n=n->next) {
	sym = n->sym;

	name = g95_build_string_const(strlen(n->name)+1, n->name);
	name = g95_addr_expr(name);

	dt_info = null_pointer_node;

	switch(sym->ts.type) {
	case BT_INTEGER:
	    t = 'i';
	    kind = g95_build_int(sym->ts.kind, 0);
	    break;

	case BT_REAL:
	    t = 'r';
	    kind = g95_build_int(sym->ts.kind, 0);
	    break;

	case BT_LOGICAL:
	    t = 'l';
	    kind = g95_build_int(sym->ts.kind, 0);
	    break;

	case BT_CHARACTER:
	    t = 'c';
	    kind = sym->ts.cl->backend_decl;
	    break;

	case BT_COMPLEX:
	    t = 'z';
	    kind = g95_build_int(sym->ts.kind, 0);
	    break;

	case BT_DERIVED:
	    t = 'd';
	    kind = integer_zero_node;
	    dt_info = generate_iodesc(sym->ts.derived);
	    break;

	default:
	    g95_internal_error("build_namelist(): Bad type");
	}

	g95_init_se(&se, NULL);

	if (sym->as == NULL)
	    se.expr = sym->backend_decl;

	else {
	    se.expr = G95_ARRAY_DESC(sym->backend_decl);

	    if (se.expr == NULL_TREE) {
		se.expr = g95_create_var(g95_get_array_desc(sym->as->rank, 0));
		g95_init_descriptor(&se, sym->backend_decl, se.expr,
				    integer_zero_node, NULL_TREE);
	    }

	    t = t + 'A' - 'a';
	}

	type = g95_build_int(t, 0);
	g95_reflevel(&se, 1);
    
	g95_add_block_to_block(&block, &se.pre);

	tmp = g95_call_library(void_type_node, PREFIX "namelist",
			       name, type, kind, se.expr, dt_info,
			       NULL_TREE);

	g95_add_expr_to_block(&block, tmp);
	g95_add_block_to_block(&block, &se.post);
    }

    return g95_finish_block(&block);
}



/* g95_conv_format()-- Convert a format label */

void g95_conv_format(g95_st_label *f) {
g95_se se;

    if (f->backend_decl == NULL) {
	g95_conv_constant(&se, f->format);
	f->backend_decl = se.expr;
    }
}



/* build_dt()-- Create a data transfer statement.  Not all of the
 * fields are valid for both reading and writing, but improper use has
 * been filtered out by now. */

static tree build_dt(char *function, g95_code *code) {
stmtblock_t block, post_block;
tree len, tmp;
g95_dt *dt;
g95_se se;
int t;

    g95_init_block(&block); 
    g95_init_block(&post_block);

    start_io(&block, &code->where);
    dt = code->ext.dt;

    if (dt->io_unit) {
	if (dt->io_unit->ts.type == BT_INTEGER)
	    set_integer_parameter(&block, &post_block, ioparm_unit,
				  ioparm_unit_kind, dt->io_unit);

	else if (dt->io_unit->rank == 0)
	    set_string_parameter(&block, &post_block, ioparm_internal_unit,
				 ioparm_internal_unit_len, dt->io_unit);
	else
	    internal_array(&block, &post_block, dt->io_unit);
    }

    if (dt->rec)
	set_integer_parameter(&block, &post_block, ioparm_rec, ioparm_rec_kind,
			      dt->rec);

    if (dt->pos)
	set_integer_parameter(&block, &post_block, ioparm_pos, ioparm_pos_kind,
			      dt->pos);

    if (dt->advance)
	set_string_parameter(&block, &post_block, ioparm_advance,
			     ioparm_advance_len, dt->advance);

    if (dt->format_expr) {
	if (dt->format_expr->rank == 1)
	    array_format(&block, &post_block, dt->format_expr);

	else if (dt->format_expr->ts.type == BT_CHARACTER)
	    set_string_parameter(&block, &post_block, ioparm_format,
				 ioparm_format_len, dt->format_expr);

	else {
	    g95_init_se(&se, NULL);
	    g95_conv_expr(&se, dt->format_expr);

	    tmp = g95_call_library(void_type_node, PREFIX "select_label",
				   se.expr, g95_addr_expr(g95_context->labels),
				   NULL_TREE);
	    g95_add_expr_to_block(&block, tmp);
	}
    }

    if (dt->decimal)
	set_string_parameter(&block, &post_block, ioparm_decimal,
			     ioparm_decimal_len, dt->decimal);

    if (dt->format_label) {
	if (dt->format_label == &g95_format_asterisk)
	    set_flag(&block, ioparm_list_format);

	else {
	    g95_conv_format(dt->format_label);

	    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)),
				     ioparm_var);
	    tmp = g95_build_component(TREE_TYPE(ioparm_format), tmp,
				      ioparm_format);
	    
	    g95_add_modify_expr(&block, tmp,
				g95_addr_expr(dt->format_label->backend_decl));

	    tmp = g95_build_indirect(TREE_TYPE(TREE_TYPE(ioparm_var)),
				     ioparm_var);
	    tmp = g95_build_component(g95_default_integer, tmp,
				      ioparm_format_len);

	    t = TREE_STRING_LENGTH(dt->format_label->backend_decl);
	    len = g95_build_int(t, 0);
	    g95_add_modify_expr(&block, tmp, len);
	}
    }

    if (dt->iostat)
	set_integer_parameter(&block, &post_block, ioparm_iostat,
			      ioparm_iostat_kind, dt->iostat);

    if (dt->iomsg)
	set_string_parameter(&block, &post_block, ioparm_iomsg,
			     ioparm_iomsg_len, dt->iomsg);

    if (dt->size)
	set_integer_parameter(&block, &post_block, ioparm_size,
			      ioparm_size_kind, dt->size);

    if (dt->err)
	set_flag(&block, ioparm_err);

    if (dt->eor)
	set_flag(&block, ioparm_eor);

    if (dt->end)
	set_flag(&block, ioparm_end);

    if (g95_option.sloppy_char)
	set_flag(&block, ioparm_sloppy_char);

    if (dt->namelist != NULL)
	g95_add_expr_to_block(&block, build_namelist(dt->namelist));

    set_endian(&block);

    tmp = g95_call_library(void_type_node, function, NULL);
    g95_add_expr_to_block(&block, tmp);

    if (last_dt == IOLENGTH)
	g95_add_block_to_block(&block, &post_block);

    else
	g95_add_block_to_block(&dt_post, &post_block);

    return g95_finish_block(&block);
}



/* g95_trans_read()-- Translate a READ statement */

tree g95_trans_read(g95_code *code) {

    last_dt = READ;
    g95_init_block(&dt_post);

    return build_dt(PREFIX "st_read", code);
}



/* g95_trans_write()-- Translate a WRITE statement */

tree g95_trans_write(g95_code *code) {
 
    last_dt = WRITE;
    g95_init_block(&dt_post);

    return build_dt(PREFIX "st_write", code);
}



/* g95_trans_flush()-- Translate the FLUSH statement */

tree g95_trans_flush(g95_code *code) {
stmtblock_t block, post_block;
g95_flush *flush;
tree tmp;

    g95_init_block(&block);
    g95_init_block(&post_block);

    start_io(&block, &code->where); 
    flush = code->ext.flush;

    set_integer_parameter(&block, &post_block, ioparm_unit, ioparm_unit_kind,
			  flush->unit);

    if (flush->iostat)
	set_integer_parameter(&block, &post_block, ioparm_iostat,
			      ioparm_iostat_kind, flush->iostat);

    if (flush->iomsg)
	set_string_parameter(&block, &post_block, ioparm_iomsg,
			     ioparm_iomsg_len, flush->iomsg);

    if (flush->err)
	set_flag(&block, ioparm_err);

    tmp = g95_call_library(void_type_node, PREFIX "st_flush", NULL_TREE);
    g95_add_expr_to_block(&block, tmp);

    g95_add_block_to_block(&block, &post_block);

    io_result(&block, flush->err, NULL, NULL, flush->iostat != NULL,
	      &code->where);

    return g95_finish_block(&block);
}


/* g95_trans_wait()-- Translate the WAIT statement */

tree g95_trans_wait(g95_code *code) {

    code = NULL;
    return empty_stmt_node;
}



/* g95_trans_iolength()-- Translate the IOLENGTH form of an INQUIRE
 * statement.  We treat this a third sort of data transfer statement,
 * except that lengths are summed instead of actually transferring any
 * data. */

tree g95_trans_iolength(g95_code *c) {
stmtblock_t block;
g95_se se;
tree tmp;

    g95_init_block(&block);
    start_io(&block, &c->where);

    last_dt = IOLENGTH;

    g95_init_se(&se, NULL);
    se.reflevel = 1;
    g95_conv_expr(&se, c->expr);

    g95_add_block_to_block(&block, &se.pre);
    tmp = g95_call_library(void_type_node, PREFIX "st_iolength", se.expr,
			   NULL);
    g95_add_block_to_block(&block, &se.post);

    g95_add_expr_to_block(&block, tmp);
    return g95_finish_block(&block);
}



/* g95_trans_dt_end()-- Finish a data transfer statement */

tree g95_trans_dt_end(g95_code *code) {
stmtblock_t block;
char *function;
tree tmp;

    g95_init_block(&block);

    switch(last_dt) {
    case READ:     function = PREFIX "st_read_done";     break;
    case WRITE:    function = PREFIX "st_write_done";    break;
    case IOLENGTH: function = PREFIX "st_iolength_done"; break;
    default:       function = NULL;                      break;
    }

    if (last_dt != IOLENGTH)
	g95_add_block_to_block(&block, &dt_post);

    tmp = g95_call_library(void_type_node, function, NULL_TREE);
    g95_add_expr_to_block(&block, tmp);

    if (code->ext.dt != NULL)
	io_result(&block, code->ext.dt->err, code->ext.dt->end,
		  code->ext.dt->eor, code->ext.dt->iostat != NULL,
		  &code->where);

    return g95_finish_block(&block);
}



/* get_component()-- Given a component, return a constructor for it */

static tree get_component(g95_component *c) {
tree node, tmp, d, n, shape;
int t, i, kind, rank;

    if (c == NULL) {
	tmp = tree_cons(dd_name, null_pointer_node, NULL_TREE);
	return g95_build_constructor(derived_desc_type, tmp);
    }

    d = null_pointer_node;
    kind = c->ts.kind;

    switch(c->ts.type) {
    case BT_INTEGER:
	t = 'i';
	break;

    case BT_CHARACTER:
	t = 'c';
	kind = bi_to_int(c->ts.cl->length->value.integer);
	break;

    case BT_COMPLEX:
	t = 'z';
	break;

    case BT_REAL:
	t = 'r';
	break;

    case BT_LOGICAL:
	t = 'l';
	break;

    case BT_DERIVED:
	t = 'd';
	d = generate_iodesc(c->ts.derived);
	kind = int_size_in_bytes(G95_DTYPE_TYPE(c->ts.derived->backend_decl));
	break;

    default:
	g95_internal_error("get_component(): Bad type");
    }

    tmp = g95_build_string_const(strlen(c->name)+1, c->name);
    TREE_ADDRESSABLE(tmp) = 1;
    TREE_STATIC(tmp) = 1;
    tmp = g95_addr_expr(tmp);

    node = tree_cons(dd_name, tmp, NULL_TREE);

    tmp = g95_build_int(t, 0);
    node = tree_cons(dd_type, tmp, node);

    tmp = g95_build_int(kind, 0);
    node = tree_cons(dd_kind, tmp, node);

    tmp = c->backend_decl;
    if (c->dimension)
	tmp = G95_ARRAY_FIELD(tmp);
    
    tmp = byte_position(tmp);

    node = tree_cons(dd_offset, tmp, node);

    tmp = (c->as == NULL)
	? integer_zero_node
	: g95_build_int(c->as->rank, 0);

    node = tree_cons(dd_rank, tmp, node);

    if (c->as == NULL || c->pointer)
	shape = null_pointer_node;

    else {
	shape = NULL_TREE;
	rank = c->as->rank;

	for(i=0; i<rank; i++) {
	    tmp = bi_to_tree(c->as->lower[i]->value.integer,
			     g95_default_integer_kind(0));

	    n = g95_build_int(2*i, 0);
	    shape = tree_cons(n, tmp, shape);

	    tmp = bi_to_tree(c->as->upper[i]->value.integer,
			     g95_default_integer_kind(0));

	    n = g95_build_int(2*i+1, 0);
	    shape = tree_cons(n, tmp, shape);
	}

	tmp = g95_build_int(2*rank-1, 0);
	tmp = build_range_type(g95_default_integer, integer_zero_node, tmp);
	tmp = build_array_type(g95_default_integer, tmp);

	shape = g95_build_constructor(tmp, nreverse(shape));
	TREE_ADDRESSABLE(shape) = 1;
	TREE_STATIC(shape) = 1;

	shape = g95_addr_expr(shape);
    }
  
    node = tree_cons(dd_shape, shape, node);
    node = tree_cons(dd_component, d, node);

    return g95_build_constructor(derived_desc_type, nreverse(node));
}



/* generate_iodesc()-- Given a derived type, generate a descriptor
 * describing the layout of the type.  Because pointers are not
 * allowed in derived types that are part of I/O statements, there are
 * no recursion problems. */

static tree generate_iodesc(g95_symbol *sym) {
tree tmp, var, decl;
g95_component *c;
desc_list *d;
int n;

    for(d=g95_context->iodesc; d; d=d->next) 
	if (sym == d->sym)
	    return d->desc;

    d = g95_getmem(sizeof(desc_list));
    d->sym = sym;
    d->desc = NULL_TREE;

    d->next = g95_context->iodesc;
    g95_context->iodesc = d;

    /* Build a constructor for the type */

    decl = NULL_TREE;
    n = 0;

    for(c=sym->components; c; c=c->next) {
	tmp = g95_build_int(n, 0);
	decl = tree_cons(tmp, get_component(c), decl);
	n++;
    }

    tmp = g95_build_int(n+1, 0);
    decl = tree_cons(tmp, get_component(NULL), decl);

    tmp = g95_build_int(n, 0);
    tmp = build_index_type(tmp);
    tmp = build_array_type(derived_desc_type, tmp);

    tmp = g95_build_constructor(tmp, nreverse(decl));

    var = g95_create_var(TREE_TYPE(tmp));
    TREE_STATIC(var) = 1;
    DECL_INITIAL(var) = tmp;
    DECL_CONTEXT(var) = NULL;

    d->desc = g95_addr_expr(var);
    return d->desc;
}



/* transfer_array_expr()-- Generate the call for an array transfer node. */

static void transfer_array_expr(g95_se *se, g95_typespec *ts) {
tree tmp, arg2;
char *function;
int kind;

    kind = ts->kind;
    function = NULL;
    arg2 = NULL;

    switch(ts->type) {
    case BT_LOGICAL:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_logical_array";
	break;

    case BT_CHARACTER:
	arg2 = se->string_length;
	function = PREFIX "transfer_character_array";
	break;

    case BT_DERIVED:
	arg2 = generate_iodesc(ts->derived);
	function = PREFIX "transfer_derived_array";
	break;

    case BT_INTEGER:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_integer_array";
	break;

    case BT_REAL:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_real_array";
	break;

    case BT_COMPLEX:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_complex_array";
	break;

    default:
	internal_error("Bad array IO basetype (%d)", ts->type);
    }

    tmp = g95_call_library(void_type_node, function, se->expr, arg2,
			   NULL_TREE);

    g95_add_expr_to_block(&se->pre, tmp);
    g95_add_block_to_block(&se->pre, &se->post);
}



/* transfer_scalar_expr()-- Generate the call for a scalar transfer node. */

static void transfer_scalar_expr(g95_se *se, g95_typespec *ts) {
tree tmp, arg2;
char *function;
int kind;

    kind = ts->kind;
    function = NULL;
    arg2 = NULL;

    switch(ts->type) {
    case BT_INTEGER:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_integer";
	break;

    case BT_REAL:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_real";
	break;

    case BT_COMPLEX:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_complex";
	break;

    case BT_LOGICAL:
	arg2 = g95_build_int(kind, 0);
	function = PREFIX "transfer_logical";
	break;

    case BT_CHARACTER:
	arg2 = se->string_length;
	function = PREFIX "transfer_character";
	break;

    case BT_DERIVED:
	arg2 = generate_iodesc(ts->derived);
	function = PREFIX "transfer_derived";
	break;

    default:
	g95_internal_error("Bad IO basetype (%d)", ts->type);
    }

    tmp = g95_call_library(void_type_node, function, se->expr, arg2,
			   NULL_TREE);
    g95_add_expr_to_block(&se->pre, tmp);
    g95_add_block_to_block(&se->pre, &se->post);
}



/* g95_trans_transfer()-- Translate a data transfer code node */

tree g95_trans_transfer(g95_code *code) {
stmtblock_t body;
g95_typespec ts;
g95_expr *expr;
g95_se se;

    expr = code->expr;
    g95_init_block(&body);

    g95_init_se(&se, NULL);
    se.reflevel = 1;

    if (last_dt == READ)
	se.store_post = 1;

    if (expr->type == EXPR_ARRAY && expr->value.constructor.c == NULL) {
	se.expr = g95_empty_array;
	g95_clear_ts(&ts);
	ts.type = BT_INTEGER;
	ts.kind = g95_default_integer_kind(1);
	transfer_array_expr(&se, &ts);

    } else if (expr->rank == 0) {
	g95_conv_expr(&se, expr);
	transfer_scalar_expr(&se, &expr->ts);

    } else {
	g95_conv_descriptor(&se, expr, 0);
	transfer_array_expr(&se, &expr->ts);
    }

    g95_add_block_to_block(&body, &se.pre);
    g95_add_block_to_block(&body, &se.post);

    return g95_finish_block(&body);
}

