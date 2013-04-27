/* Declaration statement matcher
   Copyright (C) 2002-2008 Free Software Foundation, Inc.
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


/* decl.c -- Declaration statement matcher.  */

#include <string.h>
#include "g95.h"


/* This flag is set if a an old-style length selector is matched
 * during an type-declaration statement. */

static int old_char_selector;

/* When variables aquire types and attributes from a declaration
 * statement, they get them from the following static variables.  The
 * first part of a declaration sets these variables and the second
 * part copies these into symbol structures. */

static g95_typespec current_ts;

static symbol_attribute current_attr;
static g95_array_spec *current_as;
static g95_coarray_spec *current_cas;
static int bind_flag, seen_colon, suppress_kindspec=0;
static g95_symbol *matched_variable, *procedure_interface;
static char *bind_name;

static g95_locus type_locus;

g95_locus g95_ts_locus;
char *g95_ts_locus_end;

/* g95_new_block points to the symbol of a newly matched block. */

g95_symbol *g95_new_block;

/* Modifiers that can exist in a type statement */

typedef enum { DECL_ALLOCATABLE=0, DECL_DIMENSION, DECL_EXTERNAL,
	       DECL_IN, DECL_OUT, DECL_INOUT, DECL_INTRINSIC, DECL_OPTIONAL,
	       DECL_BIND, DECL_PARAMETER, DECL_POINTER, DECL_PRIVATE,
	       DECL_PUBLIC, DECL_SAVE, DECL_VALUE, DECL_VOLATILE, DECL_ASYNC,
	       DECL_PROTECTED, DECL_COLON, DECL_TARGET, DECL_CODIMENSION,
	       DECL_NONE
} decl_types;

#define NUM_DECL (DECL_NONE+1)  /* DECL_NONE is the last attribute */


typedef enum { PDECL_PUBLIC=0, PDECL_PRIVATE, PDECL_INTENT_IN,
	       PDECL_INTENT_OUT, PDECL_INTENT_INOUT, PDECL_OPTIONAL,
	       PDECL_POINTER, PDECL_SAVE, PDECL_COLON, PDECL_PASS,
	       PDECL_NOPASS, PDECL_NONE
} pdecl_types;




extern int g95_constructor_string_length;



/* match_intent_spec()-- Match an intent specification.  Since this
 * can only happen after an INTENT word, a legal intent-spec must
 * follow. */

static g95_intent match_intent_spec(void) {

    if (g95_match(" ( in out )") == MATCH_YES) return INTENT_INOUT;
    if (g95_match(" ( in )") == MATCH_YES)     return INTENT_IN;
    if (g95_match(" ( out )") == MATCH_YES)    return INTENT_OUT;

    g95_error("Bad INTENT specification at %C");
    return INTENT_UNKNOWN;
}



/* char_len_param_value()-- Matches a character length specification,
 * which is either a specification expression or a '*'. */

static match char_len_param_value(g95_expr **expr) {
match m;

    if (g95_match_char('*') == MATCH_YES) {
	*expr = NULL;
	return MATCH_YES;
    }

    m = g95_match_expr(expr);
    if (m != MATCH_YES)
	return m;

    if (g95_simplify_spec_expr(*expr) == FAILURE)
	return MATCH_ERROR;

    /* Verification of a spec expr has to come later */

    return MATCH_YES;
}



/* match_char_length()-- A character length is a '*' followed by a
 * literal integer or a char_len_param_value in parenthesis. */

static match match_char_length(g95_expr **expr) {
int length;
match m;

    m = g95_match_char('*');
    if (m != MATCH_YES)
	return m;

    m = g95_match_small_literal_int(&length, 0);
    if (m == MATCH_ERROR)
	return m;

    if (m == MATCH_YES) {
	*expr = g95_int_expr(length);
	return m;
    }

    if (g95_match_char('(') == MATCH_NO)
	goto syntax;

    m = char_len_param_value(expr);
    if (m == MATCH_ERROR) return m;
    if (m == MATCH_NO) goto syntax;

    if (g95_match_char(')') == MATCH_NO) {
	g95_free_expr(*expr);
	*expr = NULL;
	goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in character length specification at %C");
    return MATCH_ERROR;
}



/* find_special()-- Special subroutine for finding a symbol.  If we're
 * compiling a function or subroutine and the parent compilation unit
 * is an interface, then check to see if the name we've been given is
 * the name of the interface (located in another namespace).  If so,
 * return that symbol.  If not, use g95_get_symbol(). */

static int find_special(char *name, g95_symbol **result) {
g95_state_data *s;

    if (g95_current_state() != COMP_SUBROUTINE &&
	g95_current_state() != COMP_FUNCTION)
	goto normal;

    s = g95_state_stack->previous;
    if (s == NULL)
	goto normal;

    if (s->state != COMP_INTERFACE || s->sym == NULL)
	goto normal;   /* Nameless interface */

    if (strcmp(name, s->sym->name) == 0) {
	*result = s->sym;
	return 0;
    }

normal:
    return g95_get_symbol(name, NULL, result);
}



/* g95_match_null()-- Match a 'NULL()', and possibly take care of some
 * side effects. */

match g95_match_null(g95_expr **result) {
g95_symbol *sym;
match m;

    g95_gobble_whitespace();
    g95_def_locus = g95_current_locus;

    m = g95_match("null ( )");
    if (m != MATCH_YES)
	return m;

    /* The NULL symbol now has to be/become an intrinsic function */

    if (g95_get_symbol("null", NULL, &sym)) {
	g95_error("NULL() initialization at %C is ambiguous");
	return MATCH_ERROR;
    }

    if (g95_intrinsic_symbol(sym->name, 1, &g95_def_locus))
	return MATCH_ERROR;

    *result = g95_null_expr(&g95_def_locus);
    return MATCH_YES;
}



/* get_proc_name()-- Special subroutine for getting a symbol node
 * associated with a procedure name, used in SUBROUTINE and FUNCTION
 * statements.  The symbol is created in the parent using with symtree
 * node in the child unit pointing to the symbol.  If the current
 * namespace has no parent, then the symbol is just created in the
 * current unit. */

static int get_proc_name(char *name, g95_symbol **result) {
g95_symbol *s, *sym;
symbol_attribute *a;
g95_symtree *st;
int rc;

    if (g95_current_ns->parent == NULL)
	return g95_get_symbol(name, NULL, result);    

    rc = g95_get_symbol(name, g95_current_ns->parent, result);
    if (*result == NULL || rc != 0)
	return rc;

  /* See if the symbol is OK. */

    sym = *result;
    a = &sym->attr;

    if (sym->attr.if_source == IFSRC_DECL ||
	(sym->ts.type != BT_UNKNOWN && sym->ts.type != BT_PROCEDURE &&
	 !sym->attr.implicit_type) ||
	a->allocatable || a->dimension || a->external || a->intrinsic ||
	a->pointer || a->save || a->target || a->entry || a->data ||
	a->use_assoc || a->equivalenced) {

	rc = g95_self_interface(name, result);
	if (rc != 2)
	    return rc;

	g95_error("Symbol '%s' at %L conflicts with the same name in an "
		  "encompassing program unit", name, &sym->declared_at);
	return 1;
    }

    /* Create a link in the current space */

    st = g95_find_symtree(g95_current_ns->sym_root, name);
    if (st == NULL)
	st = g95_new_symtree(&g95_current_ns->sym_root, name);

    else {  /* Something is already there */
	s = st->n.sym;

	if (g95_copy_attr(&sym->attr, &s->attr, NULL) == FAILURE)
	    return 1;

	sym->ts = s->ts;
	g95_free_symbol(s);
    }

    st->n.sym = sym;
    sym->refs++;

    sym->declared_at = g95_current_locus;
    return 0;
}



/* set_binding()-- Set the binding for a BIND(C) variable */

static try set_binding(g95_symbol *sym) {

    if (g95_find_state(COMP_MODULE) == FAILURE) {
	g95_error("BIND(C) variable at %C must be a MODULE variable");
	return FAILURE;
    }

    if (bind_flag) {
	g95_error("Multiple variables specified with BIND(C) name at %C");
	return FAILURE;
    }

    if (bind_name == NULL)
	sym->bind = sym->name;

    else {
	sym->bind = bind_name;
	bind_flag = 1;
    }

    return SUCCESS;
}



/* build_sym()-- Function called by variable_decl() that adds a name
 * to the symbol table. */

static try build_sym(char *name, g95_charlen *cl, g95_array_spec **as,
		     g95_coarray_spec **cas, g95_locus *var_locus) {
symbol_attribute attr;
g95_symbol *sym;

    if (find_special(name, &sym))
	return FAILURE;

/* Start updating the symbol table.  Add basic type attribute if present */

    if (current_ts.type != BT_UNKNOWN &&
	(sym->attr.implicit_type == 0 ||
	 !g95_compare_types(&sym->ts, &current_ts)) &&
	g95_add_type(sym, &current_ts, var_locus) == FAILURE)
	return FAILURE;

    if (sym->ts.type == BT_CHARACTER) {
	sym->ts.cl = g95_get_charlen(NULL);
	sym->ts.cl->length = g95_copy_expr(cl->length);
    }

/* Add dimension attribute if present. */

    if (g95_set_array_spec(sym, *as, var_locus) == FAILURE)
	return FAILURE;

    *as = NULL;

    sym->cas = *cas;
    *cas = NULL;

/* Add attribute to symbol.  The copy is so that we can reset the
 * dimension attribute. */

    attr = current_attr;
    attr.dimension = 0;

    if (g95_copy_attr(&sym->attr, &attr, var_locus) == FAILURE)
	return FAILURE;

    if (current_attr.bind) {
	if (set_binding(sym) == FAILURE)
	    return FAILURE;

	g95_add_cbinding(sym->bind, CBIND_UNKNOWN, var_locus);
    }

    sym->attr.no_restore = 1;

    return SUCCESS;
}



/* validate_string_init()-- Make sure a character parameter
 * initializer is the right length.  If not, fix it. */

static void validate_string_init(g95_symbol *sym, g95_expr *v) {
int m, n;
char *p;

    if (v->ts.type != BT_CHARACTER || sym->ts.cl->length == NULL)
	return;

    m = bi_to_int(sym->ts.cl->length->value.integer);
    n = v->value.character.length;

    if (m == n)
	return;

    if (m < n) {
	v->value.character.length = m;
	v->ts.cl = sym->ts.cl;
	return;
    }

    p = g95_getmem(m + 1);

    memcpy(p, v->value.character.string, n);
    memset(p+n, ' ', m-n);

    p[m] = '\0';

    g95_free(v->value.character.string);
    v->value.character.string = p;
    v->value.character.length = m;
    v->ts.cl = sym->ts.cl;
}



/* set_component_attr()-- Set component attributes from a standard
 * symbol attribute structure. */

static void set_component_attr(g95_component *c, symbol_attribute *attr) {

    c->dimension   = attr->dimension;
    c->pointer     = attr->pointer;
    c->allocatable = attr->allocatable;
}



/* init_struct()-- Initialize a default value for a structure component */

static try init_struct(g95_expr **initp) {
g95_component *c;

    c = g95_current_block()->components;
    while(c->next != NULL)
	c = c->next;

    c->initializer = *initp;
    *initp = NULL;

    if (g95_check_init_expr(c->initializer) == FAILURE)
	return FAILURE;

    if (!g95_compare_types(&c->ts, &c->initializer->ts) &&
	(g95_convert_type(c->initializer, &c->ts, 0) == FAILURE ||
	 g95_simplify_expr(c->initializer) == FAILURE))
	return FAILURE;

    return SUCCESS;
}



/* init_symbol()-- Initialize a symbol. */

static try init_symbol(char *name, g95_expr **initp) {
g95_expr *e, *init;
g95_symbol *sym;
g95_charlen *cl;
int m;
try t;

    if (find_special(name, &sym))
	return FAILURE;

    init = *initp;

/* If this variable declaration is confirming an implicit parameter
 * type, then an initialization expression is not allowed. */

    if (current_attr.flavor != FL_PARAMETER &&
	sym->attr.flavor == FL_PARAMETER) {
	g95_error("Initializer not allowed for PARAMETER '%s' at %C",
		  sym->name);
	return FAILURE;
    }

    if (sym->attr.flavor == FL_UNKNOWN &&
	g95_add_flavor(&sym->attr, FL_VARIABLE, sym->name,
		       &sym->declared_at) == FAILURE)
	return FAILURE;

/* Add initializer */

    if (sym->attr.data) {
	g95_error("Variable '%s' at %C with an initializer already appears "
		  "in a DATA statement", sym->name);
	return FAILURE;
    }

    sym->value = init;
    *initp = NULL;

    sym->attr.current = 1;
    t = g95_check_init_expr(init);
    sym->attr.current = 0;

    if (t == SUCCESS)
	t = g95_simplify_init_expr(init);

    if (t == FAILURE)
	return FAILURE;

    if (sym->attr.dimension && init->rank == 0)
	init->rank = sym->as->rank;

    if (sym->attr.flavor == FL_PARAMETER && sym->as != NULL &&
	(g95_simplify_array_spec(sym->as) == FAILURE ||
	 !g95_constant_array_spec(sym->as, 1)))
	return FAILURE;

    /* Deal with character constants */

    if (sym->ts.type == BT_CHARACTER) {
	cl = sym->ts.cl;

	if (G95_STRICT_F() && sym->attr.flavor == FL_PARAMETER &&
	    cl->length != NULL) {
	    g95_error("CHARACTER parameter '%s' at %C must have assumed "
		      "length (*) in F mode", sym->name);
	    return FAILURE;
	}

	if (cl->length == NULL && !sym->attr.dummy && init != NULL) {
	    if (!sym->attr.flavor != FL_PARAMETER &&
		current_attr.flavor != FL_PARAMETER) {
		g95_error("Assumed length character variable '%s' at %C "
			  "must be a PARAMETER", sym->name);
		return FAILURE;
	    }

	    if (init->type == EXPR_ARRAY) {
		m = g95_constructor_string_length;
		if (m == -1)
		    m = 0;

		sym->ts.cl->length = g95_int_expr(m);

	    } else if (init->type == EXPR_CONSTANT &&
		       init->ts.type == BT_CHARACTER) {
		m = init->value.character.length;
		sym->ts.cl = cl = g95_get_charlen(NULL);
		sym->ts.cl->length = g95_int_expr(m);
	    }
	}

	if (sym->attr.flavor == FL_PARAMETER && init->type == EXPR_CONSTANT &&
	    cl->length != NULL) {

	    if (g95_simplify_init_expr(cl->length) == FAILURE)
		return FAILURE;

	    if (cl->length->type != EXPR_CONSTANT) {
		g95_error("CHARACTER PARAMETER at %C must have a constant "
			  "length");
		return FAILURE;
	    }

	    validate_string_init(sym, init);
	}
    }

    if (t == SUCCESS && sym->ts.type != BT_UNKNOWN &&
	sym->value->ts.type != BT_UNKNOWN) {

	if (!g95_compare_types(&sym->ts, &sym->value->ts)) {
	    e = g95_assign_boz(&sym->ts, sym->value);
	    if (e != NULL) {
		g95_free_expr(sym->value);
		sym->value = e;

	    } else {
		t = g95_convert_type(sym->value, &sym->ts, 0);
		if (t == SUCCESS)
		    t = g95_simplify_init_expr(init);
	    }
	}

	if (t == SUCCESS)
	    t = g95_check_assign_symbol(sym, sym->value);
    }

    return t;
}



/* build_struct()-- Function called by variable_decl() that adds a
 * name to a structure being built. */

static try build_struct(char *name, g95_charlen *cl, g95_array_spec **as,
			g95_coarray_spec **cas) {
g95_component *c;

    if ((current_ts.type == BT_DERIVED) &&
	(current_ts.derived == g95_current_block()) &&
	(current_attr.pointer == 0)) {
	g95_error("Component at %C must have the POINTER attribute");
	return FAILURE;
    }

    if (g95_current_block()->attr.pointer && (*as)->rank != 0) {
	if ((*as)->type != AS_DEFERRED && (*as)->type != AS_EXPLICIT) {
	    g95_error("Array component of structure at %C must have explicit "
		      "or deferred shape");
	    return FAILURE;
	}
    }

    if (current_ts.type == BT_CHARACTER && cl->length == NULL) {
	g95_error("CHARACTER component at %C cannot be assumed-size");
	return FAILURE;
    }

    if (g95_add_component(g95_current_block(), name, &c) == FAILURE)
	return FAILURE;

    c->ts = current_ts;
    c->ts.cl = cl;
    set_component_attr(c, &current_attr);

    c->as = *as;
    if (c->as != NULL)
	c->dimension = 1;

    *as = NULL;

    c->cas = *cas;
    *cas = NULL;

    if (current_ts.type == BT_DERIVED)
	current_ts.derived->attr.used = 1;

    /* Check array components */

    if (!c->dimension)
	return SUCCESS;

    if (c->pointer) {
	if (c->as->type != AS_DEFERRED) {
	    g95_error("POINTER array component of structure at %C "
		      "must have a deferred shape");
	    return FAILURE;
	}

    } else if (c->allocatable) {
	if (c->as->type != AS_DEFERRED) {
	    g95_error("ALLOCATABLE array component of structure at %C "
		      "must have a deferred shape");
	    return FAILURE;
	}

    } else {
	if (c->as->type != AS_EXPLICIT) {
	    g95_error("Array component of structure at %C must have an "
		      "explicit shape");
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* variable_decl()-- Match a variable name with an optional
 * initializer.  When this subroutine is called, a variable is
 * expected to be parsed next.  Depending on what is happening at the
 * moment, updates either the symbol table or the current
 * interface. */

static match variable_decl(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_expr *initializer, *char_len;
g95_coarray_spec *cas;
g95_array_spec *as;
g95_charlen *cl;
int seen_init;
match m;
try t;

    initializer = NULL;
    as          = NULL;
    cas         = NULL;
    seen_init   = 0;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	goto cleanup;

    m = g95_match_array_spec(&as);
    switch(m) {
    case MATCH_YES:
	if (G95_STRICT_F()) {
	    g95_error("Array specification following variable '%s' at %L not "
		      "allowed in F mode", name, &g95_def_locus);
	    m = MATCH_ERROR;
	    goto cleanup;
	}

	break;

    case MATCH_NO:
	as = g95_copy_array_spec(current_as);
	break;

    case MATCH_ERROR:
	goto cleanup;
    }

    char_len = NULL;
    cl = NULL;

    if (current_ts.type == BT_CHARACTER) {
	switch(match_char_length(&char_len)) {
	case MATCH_YES:
	    if (G95_STRICT_F()) {
		g95_error("CHARACTER *-length' at %C not permitted in F mode");
		return MATCH_ERROR;
	    }

	    cl = g95_get_charlen(NULL);
	    cl->length = char_len;
	    break;

	case MATCH_NO:
	    cl = current_ts.cl;
	    if (cl->length == NULL)
		cl = g95_get_charlen(NULL);

	    break;

	case MATCH_ERROR:
	    m = MATCH_ERROR;
	    goto cleanup;
	}
    }

    m = g95_match_coarray_spec(&cas);
    if (m == MATCH_ERROR)
	goto cleanup;

    if (m == MATCH_NO)
	cas = g95_copy_coarray_spec(current_cas);

/* In functions that have a RESULT variable defined, the function name
 * always refers to function calls.  Therefore, the name is not
 * allowed to appear in specification statements. */

    if (g95_current_state() == COMP_FUNCTION && g95_current_block() != NULL &&
	g95_current_block()->result != NULL &&
	g95_current_block()->result != g95_current_block() &&
	strcmp(g95_current_block_name(), name) == 0) {
	g95_error("Function name '%s' not allowed at %C", name);
	m = MATCH_ERROR;
	goto cleanup;
    }

    t = (g95_current_state() == COMP_DERIVED)
	? build_struct(name, cl, &as, &cas)
	: build_sym(name, cl, &as, &cas, &g95_def_locus);

    if (t == FAILURE) {
	m = MATCH_ERROR;
	goto cleanup;
    }

    m = MATCH_YES;

/* The double colon must be present in F */

    if (G95_STRICT_F() && !seen_colon) {
	g95_error("Data declaration statement at %C missing :: in F mode");
	m = MATCH_ERROR;
	goto cleanup;
    }

/* The double colon must be present in order to have initializers.
 * Otherwise the statement is ambiguous with an assignment statement. */

    if (seen_colon) {
	if (g95_match(" =>") == MATCH_YES) {
	    if (!current_attr.pointer) {
		g95_error("Initialization at %C isn't for a pointer variable");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    m = g95_match_null(&initializer);
	    if (m == MATCH_NO) {
		g95_error("Pointer initialization requires a NULL at %C");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    seen_init = 1;

	    if (g95_pure(NULL, 1)) {
		g95_error("Initialization of pointer at %C is not allowed in "
			  "a PURE procedure");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	} else if (g95_match_char('=') == MATCH_YES) {
	    if (current_attr.pointer) {
		g95_error("Pointer initialization at %C requires '=>', "
			  "not '='");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    /* Match a non-initialization expression here because the
	     * current variable might be a parameter that depends on
	     * its own shape.  The initialization property is checked
	     * a little later. */

	    m = g95_match_expr(&initializer);
	    if (m == MATCH_NO) {
		g95_error("Expected an initialization expression at %C");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    if (g95_simplify_expr(initializer) == FAILURE) {
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    seen_init = 1;

	    if (current_attr.flavor != FL_PARAMETER && g95_pure(NULL, 1)) {
		g95_error("Initialization of variable at %C is not allowed "
			  "in a PURE procedure");
		m = MATCH_ERROR;
		goto cleanup;
	    }
	}

	if (m == MATCH_ERROR)
	    goto cleanup;

	if (initializer != NULL) {
	    if (g95_current_state() == COMP_DERIVED)
		t = init_struct(&initializer);
	    else
		t = init_symbol(name, &initializer);

	    m = (t == SUCCESS) ? MATCH_YES : MATCH_ERROR;
	}
    }

    if (current_attr.flavor == FL_PARAMETER && !seen_init) {
	g95_error("PARAMETER at %L is missing an initializer", &g95_def_locus);
	m = MATCH_ERROR;
    }

/* Free stuff up and return */

cleanup:
    g95_free_expr(initializer);
    g95_free_array_spec(as);
    g95_free_coarray_spec(cas);

    return m;
}



/* procedure_decl()-- Match a procedure definition that is a data entity */

static match procedure_decl(void) {
static mstring decls[] = {
    minit(", intent ( in out )",  PDECL_INTENT_INOUT),
    minit(", optional",           PDECL_OPTIONAL),
    minit(", pointer",            PDECL_POINTER),
    minit(", save",               PDECL_SAVE),
    minit(", public",             PDECL_PUBLIC),
    minit(", private",            PDECL_PRIVATE),
    minit(", intent ( in )",      PDECL_INTENT_IN),
    minit(", intent ( out )",     PDECL_INTENT_OUT),
    minit(" ::",                  PDECL_COLON),
    minit(NULL,                   PDECL_NONE) };

char name[G95_MAX_SYMBOL_LEN+1];
int pointer, optional, save;
g95_access access;
g95_intent intent;
g95_symbol *sym;
g95_locus where;
g95_typespec ts;
int seen_colon;
pdecl_types d;
match m;

  /* Get the attributes, if any */

    access = ACCESS_UNKNOWN;
    intent = INTENT_UNKNOWN;
    seen_colon = 0;
    optional   = 0;
    pointer    = 0;
    save       = 0;

    do {
	d = g95_match_strings(decls);

	switch(d) {
	case PDECL_OPTIONAL:
	    if (optional) {
		g95_error("Duplicate OPTIONAL specification at %C");
		return MATCH_ERROR;
	    }

	    optional = 1;
	    break;

	case PDECL_POINTER:
	    if (pointer) {
		g95_error("Duplicate POINTER specification at %C");
		return MATCH_ERROR;
	    }

	    pointer = 1;
	    break;

	case PDECL_PUBLIC:
	    if (access != ACCESS_UNKNOWN) {
		g95_error("Second ACCESS specification at %C");
		return MATCH_ERROR;
	    }

	    access = ACCESS_PUBLIC;
	    break;

	case PDECL_PRIVATE:
	    if (access != ACCESS_UNKNOWN) {
		g95_error("Second ACCESS-specification at %C");
		return MATCH_ERROR;
	    }

	    access = ACCESS_PRIVATE;
	    break;

	case PDECL_INTENT_IN:
	    if (intent != INTENT_UNKNOWN) {
		g95_error("Multiple INTENT specification at %C");
		return MATCH_ERROR;
	    }

	    intent = INTENT_IN;
	    break;

	case PDECL_INTENT_OUT:
	    if (intent != INTENT_UNKNOWN) {
		g95_error("Multiple INTENT specification at %C");
		return MATCH_ERROR;
	    }

	    intent = INTENT_OUT;
	    break;

	case PDECL_INTENT_INOUT:
	    if (intent != INTENT_UNKNOWN) {
		g95_error("Multiple INTENT specification at %C");
		return MATCH_ERROR;
	    }

	    intent = INTENT_INOUT;
	    break;
      
	case PDECL_SAVE:
	    if (save) {
		g95_error("Duplicate SAVE specification at %C");
		return MATCH_ERROR;
	    }

	    save = 1;
	    break;

	case PDECL_COLON:
	    seen_colon = 1;
	    break;

	default:
	    g95_internal_error("procedure_decl()- Bad decl");
	    break;
	}

    } while(d != PDECL_COLON && d != PDECL_NONE);

    /* Now get the list of variables */

    g95_clear_ts(&ts);
    ts.type = BT_PROCEDURE;
    ts.interface = procedure_interface;

    for(;;) {
	m = g95_match_name(name);
	if (m == MATCH_ERROR) return m;
	if (m == MATCH_NO)    goto syntax;

	if (g95_get_symbol(name, NULL, &sym))
	    return MATCH_ERROR;

	if (g95_add_type(sym, &ts, NULL) == FAILURE)
	    return MATCH_ERROR;

	if (access != ACCESS_UNKNOWN &&
	    g95_add_access(&sym->attr, access, name, NULL) == FAILURE)
	    return MATCH_ERROR;

	if (intent != INTENT_UNKNOWN &&
	    g95_add_intent(&sym->attr, intent, NULL) == FAILURE)
	    return MATCH_ERROR;

	if (optional && g95_add_optional(&sym->attr, name, NULL) == FAILURE)
	    return MATCH_ERROR;

	if (save && g95_add_save(&sym->attr, name, NULL) == FAILURE)
	    return MATCH_ERROR;

	if (pointer) {
	    if (g95_add_pointer(&sym->attr, name, NULL) == FAILURE)
		return MATCH_ERROR;

	    if (g95_match(" => ") == MATCH_YES) {
		where = g95_current_locus;

		if (g95_match("null ( )") == MATCH_YES) 
		    sym->value = g95_null_expr(&where);
	    }

	    if (sym->attr.flavor != FL_VARIABLE &&
		g95_add_flavor(&sym->attr, FL_VARIABLE, name, NULL) == FAILURE)
		return MATCH_ERROR;

	} else {
	    if (g95_add_procedure(&sym->attr, PROC_EXTERNAL,
				  name, NULL) == FAILURE)
		return MATCH_ERROR;

	    if (!sym->attr.function && sym->attr.flavor != FL_PROCEDURE &&
		g95_add_flavor(&sym->attr, FL_PROCEDURE,
			       name, NULL) == FAILURE)
		return MATCH_ERROR;
	}

	if (g95_match_eos() == MATCH_YES)
	    break;

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in PROCEDURE declaration at %C");
    return MATCH_ERROR;
}



/* procedure_component()-- Match a procedure component of a derived type. */

static match procedure_component(void) {
static mstring decls[] = {
    minit(", pointer",            PDECL_POINTER),
    minit(", pass(",              PDECL_PASS),
    minit(", public",             PDECL_PUBLIC),
    minit(", private",            PDECL_PRIVATE),
    minit(", nopass",             PDECL_NOPASS),
    minit(" ::",                  PDECL_COLON),
    minit(NULL,                   PDECL_NONE) };

char name[G95_MAX_SYMBOL_LEN+1];
int n, nopass, pointer, arg;
g95_formal_arglist *f;
g95_access access;
g95_component *c;
pdecl_types d;
match m;

    access = ACCESS_UNKNOWN;
    nopass = 0;
    pointer = 0;
    arg = 0;

    do {
	d = g95_match_strings(decls);

	switch(d) {
	case PDECL_PRIVATE:
	    if (access != ACCESS_UNKNOWN) {
		g95_error("Second ACCESS specification at %C");
		return MATCH_ERROR;
	    }

	    access = ACCESS_PRIVATE;
	    break;

	case PDECL_POINTER:
	    if (pointer) {
		g95_error("Duplicate POINTER specification at %C");
		return MATCH_ERROR;
	    }

	    pointer = 1;
	    break;

	case PDECL_PUBLIC:
	    if (access != ACCESS_UNKNOWN) {
		g95_error("Second ACCESS specification at %C");
		return MATCH_ERROR;
	    }

	    access = ACCESS_PUBLIC;
	    break;

	case PDECL_NOPASS:
	    if (nopass) {
		g95_error("Duplicate NOPASS specification at %C");
		return MATCH_ERROR;
	    }

	    nopass = 1;
	    break;

	case PDECL_PASS:
	    if (arg != -1) {
		g95_error("Second PASS specification at %C");
		return MATCH_ERROR;
	    }

	    switch(g95_match_name(name)) {
	    case MATCH_YES:
		break;

	    case MATCH_NO:
		g95_error("Missing PASS-name expected at %C");
		return MATCH_ERROR;

	    case MATCH_ERROR:
		return MATCH_ERROR;
	    }

	    if (g95_match_char(')') != MATCH_YES)
		goto syntax;

	    arg = 1;
	    n = 1;

	    for(f=procedure_interface->formal; f; f=f->next, arg++)
		if (strcmp(f->sym->name, name) == 0) {
		    n = 0;
		    break;
		}

	    if (n) {
		g95_error("Argument '%s' in PASS at %C not in procedure "
			  "interface", name);
		return MATCH_ERROR;
	    }

	    break;

	case PDECL_COLON:
	    break;

	case PDECL_NONE:
	    goto syntax;

	default:
	    g95_internal_error("procedure_component(): Bad decl");
	}

    } while(d != PDECL_COLON);

    if (!pointer) {
	g95_error("POINTER attribute required in PROCEDURE component at %C");
	return MATCH_ERROR;
    }

    if (nopass && arg != 0) {
	g95_error("PASS() and NOPASS conflict at %C");
	return MATCH_ERROR;
    }

    /* Now declare some components */

    for(;;) {
	m = g95_match_name(name);
	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	if (m == MATCH_NO)
	    goto syntax;

	if (g95_add_component(g95_current_block(), name, &c) == FAILURE)
	    return MATCH_ERROR;

	g95_clear_ts(&c->ts);
	c->ts.type      = BT_PROCEDURE;
	c->ts.interface = procedure_interface;

	c->initializer = NULL;
	c->access      = access;
	c->arg         = arg;
	c->nopass      = nopass;
	c->pointer     = 1;
	c->allocatable = 0;
	c->dimension   = 0;
	c->as          = NULL;

	if (g95_match(" => null ( )") == MATCH_YES)
	    c->initializer = g95_null_expr(NULL);

	if (g95_match_eos() == MATCH_YES)
	    break;

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in PROCEDURE specification at %C");
    return MATCH_ERROR;
}



/* g95_match_procedure()-- Match a PROCEDURE declaration. */

match g95_match_procedure(void) {
char name[G95_MAX_SYMBOL_LEN+1];
match m;

    procedure_interface = NULL;

    if (g95_match_char(')') != MATCH_YES) {
	m = g95_match_name(name);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) return m;

	if (g95_match_char(')') == MATCH_NO)
	    goto syntax;

	if (g95_get_ha_symbol(name, &procedure_interface))
	    return MATCH_ERROR;

	if (procedure_interface->attr.flavor != FL_PROCEDURE &&
	    g95_add_flavor(&procedure_interface->attr,
			   FL_PROCEDURE, name, &g95_def_locus) == FAILURE)
	    return MATCH_ERROR;
    }

    m = (g95_current_state() == COMP_DERIVED)
	? procedure_component()
	: procedure_decl();

    procedure_interface = NULL;
    return m;

syntax:
    g95_error("Syntax error in PROCEDURE declaration at %C");
    return MATCH_ERROR;
}



/* match_old_kind_spec()-- Match an extended-f77 kind specification */

static match match_old_kind_spec(g95_typespec *ts) {
match m;
  
    if (g95_match_char('*') != MATCH_YES)
	return MATCH_NO;

    m = g95_match_small_literal_int(&ts->kind, 0);
    if (m != MATCH_YES)
	return MATCH_ERROR;

/* Massage the kind numbers for complex types */

    if (ts->type == BT_COMPLEX) {
	if (ts->kind % 2 == 1)
	    goto bad;

	ts->kind /= 2;
    }

    if (g95_validate_kind(ts->type, ts->kind) == -1) {
    bad:
	g95_error("Old-style kind %d not supported for type %s at %C",
		  ts->kind, g95_basic_typename(ts->type));

	return MATCH_ERROR;
    }

    return MATCH_YES;
}



/* match_f_kind_expr()-- Match a kind expression in F, this must be a
 * parameter. */

static match match_f_kind_expr(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
int flag;
match m;

    m = g95_match_name(name);

    switch(m) {
    case MATCH_YES:
	break;

    case MATCH_NO:
	goto error;

    case MATCH_ERROR:
	return MATCH_ERROR;
    }

    flag = (g95_find_state(COMP_INTERFACE) == FAILURE);

    if (g95_find_symbol(name, NULL, flag, &sym))
	return MATCH_ERROR;

    if (sym == NULL)
	return MATCH_NO;

    if (sym->attr.flavor != FL_PARAMETER)
	goto error;

    g95_set_usage(sym, &sym->declared_at, 0, 1);

    *result = g95_copy_expr(sym->value);
    return MATCH_YES;

error:
    g95_error("Kind expression at %C must be a PARAMETER in F");
    return MATCH_ERROR;
}



/* match_kind_expr()-- Match a kind expression.  This is an
 * initialization expression. */

static match match_kind_expr(g95_expr **result) {
match m;
try t;

    if (G95_STRICT_F())
	return match_f_kind_expr(result);

    m = g95_match_expr(result);
    if (m != MATCH_YES)
	return m;

    if ((*result)->type == EXPR_CONSTANT && G95_STRICT_F()) {
	g95_error("KIND specification at %C must be a PARAMETER in F mode");
	g95_free_expr(*result);
	return MATCH_ERROR;
    }

    t = g95_simplify_expr(*result);

    if (t == SUCCESS)
	t = g95_check_init_expr(*result);

    if (t == FAILURE) {
	g95_free_expr(*result);
	*result = NULL;
    }

    return (t == SUCCESS) ? MATCH_YES : MATCH_ERROR;
}




/* match_kind()-- Match a kind specification.  The 'kind=' has already
 * been seen. */

static match match_kind(g95_typespec *ts) {
g95_locus where;
g95_expr *e;
char *msg;
match n;

    if (suppress_kindspec) {
	n = g95_match_expr(&e);
	if (n == MATCH_YES)
	    g95_free_expr(e);

	return n;
    }

    g95_gobble_whitespace();
    where = g95_current_locus;

    n = match_kind_expr(&e);
    if (n == MATCH_NO)
	g95_error("Expected initialization expression at %L", &where);

    if (n != MATCH_YES)
	return MATCH_ERROR;

    if (e->rank != 0) {
	g95_error("Expected scalar initialization expression at %L", &where);
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    msg = g95_extract_int(e, &ts->kind);
    if (msg != NULL) {
	g95_error(msg);
	g95_free_expr(e);
	return MATCH_ERROR;
    }

    g95_free_expr(e);
    e = NULL;

    if (g95_validate_kind(ts->type, ts->kind) == -1) {
	g95_error("Kind %d not supported for type %s at %L", ts->kind,
		  g95_basic_typename(ts->type), &where);

	return MATCH_ERROR;
    }

    return MATCH_YES;
}



/* g95_match_kind_spec()-- Match a kind specification.  Since kinds
 * are generally optional, we usually return MATCH_NO if something
 * goes wrong.  If a "kind=" string is found, then we know we have an
 * error. */

match g95_match_kind_spec(g95_typespec *ts) {
g95_locus where;
g95_expr *e;
match m;

    m = MATCH_NO;
    e = NULL;

    where = g95_current_locus;

    if (g95_match_char('(') == MATCH_NO)
	return MATCH_NO;

/* Also gobbles optional text */

    if (g95_match(" kind = ") == MATCH_NO && G95_STRICT_F()) {
	g95_error("Kind specification at %C must have a KIND= in F mode");
	return MATCH_ERROR;
    }

    m = match_kind(ts);
    if (m != MATCH_YES) {
	g95_current_locus = where;
	return m;
    }

    if (g95_match_char(')') != MATCH_YES) {
	g95_error("Missing right paren at %C");
	g95_current_locus = where;
	return MATCH_ERROR;
    }

    return MATCH_YES;
}



/* match_char_spec()-- Match the various kind/length specifications in
 * a CHARACTER declaration.  We don't return MATCH_NO. */

static match match_char_spec(g95_typespec *ts, int kind_flag) {
int i, kind, seen_length;
g95_charlen *cl;
g95_locus where;
g95_expr *len;
match m;

    kind = g95_default_character_kind(); 
    len = NULL;
    seen_length = 0;

    if (!kind_flag)
	goto no_spec;

/* Try the old-style specification first */

    old_char_selector = 0;
    where = g95_current_locus;

    m = match_char_length(&len);
    if (m != MATCH_NO) {
	if (m == MATCH_YES && G95_STRICT_F()) {
	    g95_error("CHARACTER *-length at %L not permitted in F mode.",
		      &where);
	    m = MATCH_ERROR;
	    goto done;
	}

	if (m == MATCH_YES)
	    old_char_selector = 1;

	seen_length = 1;
	goto done;
    }

    m = g95_match_char('(');
    if (m != MATCH_YES) {

	if (G95_STRICT_F()) {
	    g95_error("CHARACTER specification at %C must have a length in "
		      "F-mode");
	    m = MATCH_ERROR;
	    goto done;
	}

	m = MATCH_YES;  /* character without length is a single char */
	goto done;
    }

/* Try the weird case:  ( KIND = <int> [ , LEN = <len-param> ] )   */

    if (g95_match(" kind =") == MATCH_YES) {
	m = match_kind(ts);
	if (m == MATCH_ERROR)  goto done;
	if (m == MATCH_NO)     goto syntax;

	if (g95_match(" , len =") == MATCH_NO)
	    goto rparen;

	m = char_len_param_value(&len);
	if (m == MATCH_NO)     goto syntax;
	if (m == MATCH_ERROR)  goto done;
	seen_length = 1;

	goto rparen;
    }

/* Try to match ( LEN = <len-param> ) or ( LEN = <len-param>, KIND = <int> ) */

    if (g95_match(" len =") == MATCH_YES) {
	m = char_len_param_value(&len);
	if (m == MATCH_NO)     goto syntax;
	if (m == MATCH_ERROR)  goto done;
	seen_length = 1;

	if (g95_match_char(')') == MATCH_YES) goto done;

	if (g95_match(" , kind =") != MATCH_YES) goto syntax;

	m = match_kind(ts);
	if (m == MATCH_ERROR) goto done;
	if (m == MATCH_NO) goto syntax;

	goto rparen;
    }

/* Try to match   ( <len-param> ) or ( <len-param> , [ KIND = ] <int> ) */

    m = char_len_param_value(&len);
    if (m == MATCH_NO)    goto syntax;
    if (m == MATCH_ERROR) goto done;
    seen_length = 1;

    m = g95_match_char(')');
    if (m == MATCH_YES) {
	if (G95_STRICT_F()) {
	    g95_error("LEN= required in CHARACTER declaration at %C in "
		      "F mode");
	    m = MATCH_ERROR;
	}

	goto done;
    }

    if (g95_match_char(',') != MATCH_YES)
	goto syntax;

    g95_match(" kind =");   /* Gobble optional text */

    m = match_kind(ts);
    if (m == MATCH_ERROR) goto done;
    if (m == MATCH_NO) goto syntax;

/* require a right-paren at this point */

rparen:
    m = g95_match_char(')');
    if (m == MATCH_YES)
	goto done;

syntax:
    g95_error("Syntax error in CHARACTER declaration at %C");
    m = MATCH_ERROR;

done:
    if (m == MATCH_YES && g95_validate_kind(BT_CHARACTER, kind) == -1) {
	g95_error("Kind %d is not a CHARACTER kind at %C", kind);
	m = MATCH_ERROR;
    }

    if (m != MATCH_YES) {
	g95_free_expr(len);
	len = NULL;
	return m;
    }

/* Do some final massaging of the length values */

no_spec:
    cl = g95_get_charlen(NULL);

    if (seen_length == 0) {
	cl->length = g95_int_expr(1);

	if (G95_STRICT_F()) {
	    g95_error("CHARACTER specification at %C must have a length in "
		      "F-mode");
	    m = MATCH_ERROR;
	}

    } else {
	if (len == NULL || g95_extract_int(len, &i) != NULL || i >= 0)
	    cl->length = len;
	else {
	    g95_free_expr(len);
	    cl->length = g95_int_expr(0);
	}
    }

    ts->cl = cl;
    ts->kind = kind;

    return MATCH_YES;
}



/* attr_name()-- Return the name of an attribute */

static char *attr_name(int d) {
char *p;

    switch(d) {
    case DECL_EXTERNAL:     p = "EXTERNAL";         break;
    case DECL_IN:           p = "INTENT (IN)";      break;
    case DECL_OUT:          p = "INTENT (OUT)";     break;
    case DECL_INOUT:        p = "INTENT (IN OUT)";  break;
    case DECL_INTRINSIC:    p = "INTRINSIC";        break;
    case DECL_OPTIONAL:     p = "OPTIONAL";         break;
    case DECL_PROTECTED:    p = "PROTECTED";        break;
    case DECL_PARAMETER:    p = "PARAMETER";        break;
    case DECL_CODIMENSION:  p = "CODIMENSION";      break;
    case DECL_POINTER:      p = "POINTER";          break;
    case DECL_PRIVATE:      p = "PRIVATE";          break;
    case DECL_BIND:         p = "BIND(C)";          break;
    case DECL_PUBLIC:       p = "PUBLIC";           break;
    case DECL_SAVE:         p = "SAVE";             break;
    case DECL_TARGET:       p = "TARGET";           break;
    case DECL_VALUE:        p = "VALUE";            break;
    case DECL_VOLATILE:     p = "VOLATILE";         break;
    case DECL_ASYNC:        p = "ASYNCHRONOUS";     break;
    case DECL_ALLOCATABLE:  p = "ALLOCATABLE";      break;
    case DECL_DIMENSION:    p = "DIMENSION";        break;
    default:
	g95_internal_error("attr_name(): Bad attribute");
	p = NULL;
    }

    return p;
}



/* match_bind_name()-- Match the name-spec of a BIND specification, up
 * to the trailing paren.  A null name sets result to a NULL pointer. */

static try match_bind_name(char **result) {
g95_expr *e;
char *name;
int len;

    if (g95_match(" name = ") != MATCH_YES ||
	g95_match_init_expr(&e) != MATCH_YES)
	return FAILURE;

    if (e->type != EXPR_CONSTANT || e->ts.type != BT_CHARACTER ||
	e->rank != 0) {
	g95_error("BIND name at %C must be a scalar CHARACTER initialization "
		  "expression");
	return FAILURE;
    }

    len  = e->value.character.length;
    name = e->value.character.string;

    /* Strip leading and trailing blanks */

    while(len > 0 && name[0] == ' ') {
	len--;
	name++;
    }

    while(len > 0 && name[len-1] == ' ')
	len--;

    if (len == 0)
	name = NULL;

    else {
	name[len] = '\0';
	name = g95_get_string(name);

	g95_free_expr(e);
    }

    if (g95_match(" )") != MATCH_YES)
	return FAILURE;

    *result = name;
    return SUCCESS;
}



/* match_rest_bind_attr()-- Match the rest of a BIND-specification
 * found in an attribute specification.  We either have a right paren
 * or a name specification. */

static try match_rest_bind_attr(void) {
int c;

    bind_name = NULL;
    bind_flag = 0;

    if (g95_current_state() == COMP_DERIVED) {
	g95_error("Syntax error in type declaration at %C");
	return FAILURE;
    }

    g95_gobble_whitespace();
    c = g95_next_char();
    if (c == ')')
	return SUCCESS;

    if (c != ',') {
	g95_error("Syntax error in BIND specification at %C");
	return FAILURE;
    }

    if (match_bind_name(&bind_name) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* match_attr_spec()-- Matches an attribute specification including
 * array specs.  If successful, leaves the variables current_attr and
 * current_as holding the specification.  Also sets the seen_colon
 * variable for later use by matchers associated with initializations.
 *
 * This subroutine is a little tricky in the sense that we don't know
 * if we really have an attr-spec until we hit the double colon.
 * Until that time, we can only return MATCH_NO.  This forces us to
 * check for duplicate specification at this level.  */

static match match_attr_spec(void) {

static mstring decls[] = {
    minit(", allocatable",        DECL_ALLOCATABLE),
    minit(", asynchronous",       DECL_ASYNC),
    minit(", bind ( c",           DECL_BIND),
    minit(", dimension",          DECL_DIMENSION),
    minit(", external",           DECL_EXTERNAL),
    minit(", intent ( in )",      DECL_IN),
    minit(", intent ( out )",     DECL_OUT),
    minit(", intent ( in out )",  DECL_INOUT),
    minit(", intrinsic",          DECL_INTRINSIC),
    minit(", optional",           DECL_OPTIONAL),
    minit(", parameter",          DECL_PARAMETER),
    minit(", pointer",            DECL_POINTER),
    minit(", private",            DECL_PRIVATE),
    minit(", protected",          DECL_PROTECTED),
    minit(", public",             DECL_PUBLIC),
    minit(", save",               DECL_SAVE),
    minit(", value",              DECL_VALUE),
    minit(", volatile",           DECL_VOLATILE),
    minit(", target",             DECL_TARGET),
    minit(", codimension",        DECL_CODIMENSION),
    minit("::",                   DECL_COLON),
    minit(NULL,                   DECL_NONE)
};


g95_locus start, seen_at[NUM_DECL];
int i, seen[NUM_DECL];
decl_types d;
match m;
try t;

    g95_clear_attr(&current_attr);
    start = g95_current_locus;

    current_as  = NULL;
    current_cas = NULL;

/* See if we get all of the keywords up to the final double colon */

    for(i=0; i<NUM_DECL; i++)
	seen[i] = 0;

    for(;;) {
	d = g95_match_strings(decls);

	if (d == DECL_COLON)
	    seen_colon = 1;

	if (d == DECL_BIND && match_rest_bind_attr() == FAILURE) {
	    m = MATCH_ERROR;
	    goto cleanup;
	}

	if (d == DECL_NONE || d == DECL_COLON)
	    break;

	seen[d]++;
	seen_at[d] = g95_current_locus;

	switch(d) {
	case DECL_DIMENSION:
	    m = g95_match_array_spec(&current_as);

	    if (m == MATCH_NO) {
		g95_error("Missing dimension specification at %C");
		m = MATCH_ERROR;
	    }

	    if (m == MATCH_ERROR)
		goto cleanup;

	    break;

	case DECL_CODIMENSION:
	    if (current_cas != NULL) {
		g95_error("Duplicate CODIMENSION attribute at %C");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    m = g95_match_coarray_spec(&current_cas);

	    if (m == MATCH_NO) {
		g95_error("Missing coarray specification at %C");
		m = MATCH_ERROR;
	    }

	    if (m == MATCH_ERROR)
		goto cleanup;

	    break;

	default:
	    break;
	}
    }

/* No double colon, so assume that we've been looking at something
 * else the whole time */

    if (d == DECL_NONE) {
	m = MATCH_NO;
	goto cleanup;
    }

/* Since we've seen a double colon, we have to be looking at an
 * attr-spec.  This means that we can now issue errors */

    for(d=0; d<NUM_DECL; d++)
	if (seen[d] > 1) {
	    g95_error("Duplicate %s attribute at %L",
		      attr_name(d), &seen_at[d]);
	    m = MATCH_ERROR;
	    goto cleanup;
	}

/* Now that we've dealt with duplicate attributes, add the attributes to the 
 * current attribute. */

    for(d=0; d<NUM_DECL; d++) {
	if (seen[d] == 0)
	    continue;

	if (g95_current_state() == COMP_DERIVED) {
	    if (d == DECL_ALLOCATABLE && !g95_option.tr15581 &&
		G95_STRICT_F95()) {
		g95_error("ALLOCATABLE attribute at %L is not allowed in a "
		  "TYPE definition", &seen_at[d]);
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    if (d != DECL_DIMENSION   && d != DECL_POINTER  &&
		d != DECL_COLON       && d != DECL_NONE     &&
		d != DECL_ALLOCATABLE && d != DECL_CODIMENSION) {
		g95_error("%s attribute at %L is not allowed in a TYPE "
			  "definition", attr_name(d), &seen_at[d]);
		m = MATCH_ERROR;
		goto cleanup;
	    }
	}

	switch(d) {
	case DECL_DIMENSION:
	    t = g95_add_dimension(&current_attr, NULL, &seen_at[d]);
	    break;

	case DECL_CODIMENSION:
	    t = SUCCESS;
	    break;

	case DECL_EXTERNAL:
	    t = g95_add_external(&current_attr, &seen_at[d]);
	    break;

	case DECL_IN:
	    t = g95_add_intent(&current_attr, INTENT_IN, &seen_at[d]);
	    break;

	case DECL_OUT:
	    t = g95_add_intent(&current_attr, INTENT_OUT, &seen_at[d]);
	    break;

	case DECL_INOUT:
	    t = g95_add_intent(&current_attr, INTENT_INOUT, &seen_at[d]);
	    break;

	case DECL_INTRINSIC:
	    t = g95_add_intrinsic(&current_attr, &seen_at[d]);
	    break;

	case DECL_OPTIONAL:
	    t = g95_add_optional(&current_attr, NULL, &seen_at[d]);
	    break;

	case DECL_BIND:
	    t = g95_add_bind(&current_attr, NULL, &seen_at[d]);
	    break;

	case DECL_PARAMETER:
	    t = g95_add_flavor(&current_attr, FL_PARAMETER, NULL, &seen_at[d]);
	    break;

	case DECL_POINTER:
	    t = g95_add_pointer(&current_attr, NULL, &seen_at[d]);
	    break;

	case DECL_ALLOCATABLE:
	    t = g95_add_allocatable(&current_attr, &seen_at[d]);
	    break;

	case DECL_PRIVATE:
	    if (g95_current_state() != COMP_MODULE &&
		g95_current_state() != COMP_DERIVED) {
		g95_error("PRIVATE attribute not allowed at %C");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    t = g95_add_access(&current_attr, ACCESS_PRIVATE, NULL,
			       &seen_at[d]);
	    break;

	case DECL_PROTECTED:
	    t = g95_add_protected(&current_attr, &seen_at[d]);
	    break;

	case DECL_PUBLIC:
	    if (g95_current_state() != COMP_MODULE &&
		g95_current_state() != COMP_DERIVED) {
		g95_error("PUBLIC attribute not allowed at %C");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    t = g95_add_access(&current_attr, ACCESS_PUBLIC, NULL,
			       &seen_at[d]);
	    break;

	case DECL_SAVE:
	    t = g95_add_save(&current_attr, NULL, &seen_at[d]);
	    break;

	case DECL_TARGET:
	    t = g95_add_target(&current_attr, &seen_at[d]);
	    break;

	case DECL_VALUE:
	    if (g95_option.fmode == 0 || g95_option.fmode == 2003)
		t = g95_add_value(&current_attr, &seen_at[d]);

	    else {
		g95_error("VALUE attribute at %C not supported in non-f2003 "
			  "mode");
		t = FAILURE;
	    }

	    break;

	case DECL_VOLATILE:
	    if (g95_option.fmode == 0 || g95_option.fmode == 2003)
		t = g95_add_volatile(&current_attr, &seen_at[d]);

	    else {
		g95_error("VOLATILE attribute at %C not supported in "
			  "non-f2003 mode");
		t = FAILURE;
	    }

	    break;

	case DECL_ASYNC:
	    if (g95_option.fmode == 0 || g95_option.fmode == 2003)
		t = g95_add_async(&current_attr, &seen_at[d]);

	    else {
		g95_error("ASYNCHRONOUS attribute at %C not supported in "
			  "non-f2003 mode");
		t = FAILURE;
	    }

	    break;

	default:
	    g95_internal_error("match_attr_spec(): Bad attribute");
	}

	if (t == FAILURE) {
	    m = MATCH_ERROR;
	    goto cleanup;
	}
    }

    return MATCH_YES;

cleanup:
    g95_current_locus = start;
    g95_free_array_spec(current_as);
    g95_free_coarray_spec(current_cas);

    current_as  = NULL;
    current_cas = NULL;
    return m;
}



/* g95_match_type_spec()-- Matches a type specification.  If
 * successful, sets the ts structure to the matched specification.
 * This is necessary for FUNCTION and IMPLICIT statements.
 *
 * If kind_flag is nonzero, then we check for the optional kind
 * specification.  Not doing so is needed for matching an IMPLICIT
 * statement correctly. */

match g95_match_type_spec(g95_typespec *ts, int kind_flag) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
match m;
int c;

    g95_clear_ts(ts);

    if (g95_match(" integer") == MATCH_YES) {
	ts->type = BT_INTEGER;
	ts->kind = g95_default_integer_kind(1);
	goto get_kind;
    }

    if (g95_match(" byte") == MATCH_YES) {
	if (g95_option.fmode != 0)
	    g95_warning(153, "BYTE type at %C is not standard");

	ts->type = BT_INTEGER;
	ts->kind = 1;  /* Hardwired */
	return MATCH_YES;
    }

    if (g95_match(" character") == MATCH_YES) {
	ts->type = BT_CHARACTER;
	return match_char_spec(ts, kind_flag);
    }

    if (g95_match(" real") == MATCH_YES) {
	ts->type = BT_REAL;
	ts->kind = g95_default_real_kind(1);
	goto get_kind;
    }

    if (g95_match(" double precision") == MATCH_YES) {
	if (G95_STRICT_F()) {
	    g95_error("DOUBLE PRECISION declaration at %C is not permitted "
		      "in F");
	    return MATCH_ERROR;
	}

	ts->type = BT_REAL;
	ts->kind = g95_default_double_kind();
	return MATCH_YES;
    }

    if (g95_match(" complex") == MATCH_YES) {
	ts->type = BT_COMPLEX;
	ts->kind = g95_default_complex_kind();
	goto get_kind;
    }

    if (g95_match(" double complex") == MATCH_YES) {
	if (G95_STRICT_F()) {
	    g95_error("DOUBLE COMPLEX declaration at %C is not permitted "
		      "in F");
	    return MATCH_ERROR;
	}

	ts->type = BT_COMPLEX;
	ts->kind = g95_default_double_kind();
	return MATCH_YES;
    }

    if (g95_match(" logical") == MATCH_YES) {
	ts->type = BT_LOGICAL;
	ts->kind = g95_default_logical_kind();
	goto get_kind;
    }

    m = g95_match(" type ( %n )", name);
    if (m != MATCH_YES)
	return m;

    type_locus = g95_def_locus;

    /* Search for the name but allow the components to be defined later. */

    if (g95_get_ha_symbol(name, &sym)) {
	g95_error("Type name '%s' at %C is ambiguous", name);
	return MATCH_ERROR;
    }

    if (sym->attr.flavor != FL_DERIVED &&
	g95_add_flavor(&sym->attr, FL_DERIVED, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    ts->type = BT_DERIVED;
    ts->kind = 0;
    ts->derived = sym;

    return MATCH_YES;

/* For all types except double, derived and character, look for an
 * optional kind specifier.  MATCH_NO is actually OK at this point. */

get_kind:
    if (kind_flag == 0)
	return MATCH_YES;

    if (g95_current_form == FORM_FREE) {
	c = g95_peek_char();
	if (!g95_is_whitespace(c) && c != '*' && c != '(' && c != ':' &&
	    c != ',')
	    return MATCH_NO;
    }

    m = g95_match_kind_spec(ts);
    if (m == MATCH_NO && ts->type != BT_CHARACTER && g95_option.fmode == 0)
	m = match_old_kind_spec(ts);

    if (m == MATCH_NO)
	m = MATCH_YES;  /* No kind specifier found */

    return m;
}



/* g95_match_data_decl()-- Match a data declaration statement */

match g95_match_data_decl(void) {
g95_symbol *sym;
g95_annot *a;
match m;
int i;
try t;

    m = g95_match_type_spec(&current_ts, 1);
    if (m != MATCH_YES)
	return m;

    seen_colon = 0;

    m = match_attr_spec();
    if (m == MATCH_ERROR) {
	m = MATCH_NO;
	goto cleanup;
    }

    if (current_ts.type == BT_DERIVED && !current_attr.pointer) {
	sym = g95_use_derived(current_ts.derived);

	if (sym == NULL) {
	    m = MATCH_ERROR;
	    goto cleanup;
	}

	current_ts.derived = sym;
    }

    if (m == MATCH_YES && G95_STRICT_F() && !seen_colon) {
	g95_error("Data declaration statement at %C missing :: in F mode");
	return MATCH_ERROR;
    }

    if (current_ts.type == BT_DERIVED &&
	current_ts.derived->components == NULL) {

	if (current_attr.pointer && g95_current_state() == COMP_DERIVED)
	    goto ok;

	if (g95_find_symbol(current_ts.derived->name,
			    current_ts.derived->ns->parent, 1, &sym) == 0)
	    goto ok;

	/* Hope that an ambiguous symbol is itself masked by a type
	 * definition */

	if (sym != NULL && sym->attr.flavor == FL_DERIVED)
	    goto ok;

	g95_error("Derived type at %C has not been previously defined");
	m = MATCH_ERROR;
	goto cleanup;
    }

ok:

/* Explanation is required here.  If we have an old-style character
 * declaration, and no new-style attribute specifications, then there
 * a comma is optional between the type specification and the variable
 * list. */

    if (m == MATCH_NO && current_ts.type == BT_CHARACTER && old_char_selector)
	g95_match_char(',');

    if (current_ts.type == BT_CHARACTER) {
	t = (current_attr.flavor == FL_PARAMETER)
	    ? g95_check_init_expr(current_ts.cl->length)
	    : g95_simplify_expr(current_ts.cl->length);

	if (t == FAILURE) {
	    m = MATCH_ERROR;
	    goto cleanup;
	}
    }

    if (current_attr.flavor == FL_PARAMETER && current_as != NULL)
	for(i=0; i<current_as->rank; i++)
	    if (g95_check_init_expr(current_as->lower[i]) == FAILURE ||
		g95_check_init_expr(current_as->upper[i]) == FAILURE) {
		m = MATCH_ERROR;
		goto cleanup;
	    }

/* Give the types/attributes to symbols that follow */

    for(;;) {
	matched_variable = NULL;

	m = variable_decl();
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) break;

	if (g95_match_eos() == MATCH_YES) goto cleanup;
	if (g95_match_char(',') != MATCH_YES) break;

	if (matched_variable != NULL)
	    matched_variable->new = 0;
    }
  
    g95_error("Syntax error in data declaration at %C");
    m = MATCH_ERROR;

cleanup:
    if (m == MATCH_YES && current_ts.type == BT_DERIVED) {
	a = g95_annotate(ANNOT_DERIVED, &type_locus);
	a->u.sym = current_ts.derived;
    }

    g95_free_array_spec(current_as);
    g95_free_coarray_spec(current_cas);

    current_as  = NULL;
    current_cas = NULL;

    return m;
}



/* match_prefix()-- Match a prefix associated with a function or
 * subroutine declaration.  If the typespec pointer is nonnull, then a
 * typespec can be matched.  Note that if nothing matches, MATCH_YES
 * is returned (the null string was matched). */

static match match_prefix(g95_typespec *ts) {
int seen_type;

    g95_clear_attr(&current_attr);
    seen_type = 0;
    g95_ts_locus.nextc = NULL;

loop:
    if (!seen_type && ts != NULL) {
	g95_ts_locus = g95_current_locus;

	suppress_kindspec = 1;

	if (g95_match_type_spec(ts, 1) == MATCH_YES &&
	    (g95_current_locus.nextc[-1] == ')' ||
	     g95_match_space() == MATCH_YES)) {
	    seen_type = 1;
	    suppress_kindspec = 0;
	    g95_ts_locus_end = g95_current_locus.nextc;
	    g95_gobble_whitespace();
	    goto loop;
	}

	suppress_kindspec = 0;
    }

    if (g95_match("elemental% ") == MATCH_YES ) {
	if (g95_add_elemental(&current_attr, NULL) == FAILURE)
	    return MATCH_ERROR;

	goto loop;
    }

    if (g95_match("pure% ") == MATCH_YES) {
	if (g95_add_pure(&current_attr, NULL) == FAILURE)
	    return MATCH_ERROR;

	goto loop;
    }

    if (g95_match("recursive% ") == MATCH_YES) {
	if (g95_add_recursive(&current_attr, NULL) == FAILURE)
	    return MATCH_ERROR;

	goto loop;
    }

/* At this point, the next item is not a prefix */

    return MATCH_YES;
}


/* copy_prefix()-- Copy attributes matched by match_prefix() to
 * attributes on a symbol. */

static try copy_prefix(symbol_attribute *dest, g95_locus *where) {

    if (current_attr.pure && g95_add_pure(dest, where) == FAILURE)
	return FAILURE;

    if (current_attr.elemental && g95_add_elemental(dest, where) == FAILURE)
	return FAILURE;

    if (current_attr.recursive && g95_add_recursive(dest, where) == FAILURE)
	return FAILURE;

    return SUCCESS;
}



/* g95_match_formal_arglist()-- Match a formal argument list. */

match g95_match_formal_arglist(g95_symbol *progname, int context,
			       int null_flag) {
g95_formal_arglist *head, *tail, *p, *q;
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_locus where;
call_by cb;
match m;
try t;

    head = tail = NULL;
    m = MATCH_ERROR;

    if (g95_match_char('(') != MATCH_YES) {
	if (null_flag)
	    goto ok;

	return MATCH_NO;
    }

    if (g95_match_char(')') == MATCH_YES)
	goto ok;

    for(;;) {
	cb = CB_NONE;
	g95_gobble_whitespace();
	where = g95_current_locus;

	if (g95_match_char('*') == MATCH_YES) {
	    if (context != FORMAL_SUBROUTINE) {
		g95_error("Alternate return specifier at %C can only appear "
			  "in a SUBROUTINE program unit");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    if (G95_STRICT_F()) {
		g95_error("Alternate return at %C is not allowed in F");
		m = MATCH_ERROR;
		goto cleanup;
	    }

	    sym = NULL;
	    name[0] = '\0';

	} else {
	    if (g95_match(" %%ref (") == MATCH_YES)
		cb = CB_REFERENCE;

	    else if (g95_match(" %%val (") == MATCH_YES)
		cb = CB_VALUE;

	    m = g95_match_name(name);
	    if (m != MATCH_YES)
		goto cleanup;

	    if (cb != CB_NONE && g95_match_char(')') != MATCH_YES) {
		m = MATCH_ERROR;
		g95_error("Syntax error in formal argument list at %C");
		goto cleanup;
	    }
	}

	if (name[0] != '\0' && g95_get_symbol(name, NULL, &sym))
	    goto cleanup;

	p = g95_get_formal_arglist();
	p->where = where;
	p->cb = cb;
	p->sym = sym;

	if (cb == CB_VALUE)
	    sym->attr.by_value = 1;

	if (head == NULL)
	    head = tail = p;

	else {
	    tail->next = p;
	    tail = p;
	}

	if (sym != NULL && context == FORMAL_ST_FUNCTION)
	    sym->attr.st_dummy = 1;

/* We don't add the VARIABLE flavor because the name could be a dummy
 * procedure. */

	if (sym != NULL && context != FORMAL_ST_FUNCTION &&
	    (g95_add_dummy(&sym->attr, sym->name, NULL) == FAILURE)) {
	    m = MATCH_ERROR;
	    goto cleanup;
	}

/* The name of a program unit can be in a different namespace, so
 * check for it explicitly.  After the statement is accepted, the name
 * is checked for especially in g95_get_symbol(). */

	if (g95_new_block != NULL && sym != NULL &&
	    strcmp(sym->name, g95_new_block->name) == 0) {
	    g95_error("Name '%s' at %C is the name of the procedure",
		      sym->name);
	    m = MATCH_ERROR;
	    goto cleanup;
	}

	if (g95_match_char(')') == MATCH_YES)
	    goto ok;

	m = g95_match_char(',');
	if (m != MATCH_YES) {      
	    g95_error("Unexpected junk in formal argument list at %C");
	    goto cleanup;
	}
    }

ok:
  /* Check for duplicate symbols in the formal argument list */

    if (head != NULL) {
	for(p=head; p->next; p=p->next) {
	    if (p->sym == NULL)
		continue;

	    for(q=p->next; q; q=q->next)
		if (p->sym == q->sym) {
		    g95_error("Duplicate symbol '%s' in formal argument list "
			      "at %C", p->sym->name);

		    m = MATCH_ERROR;
		    goto cleanup;
		}
	}
    }

    t = g95_add_explicit_interface(progname, IFSRC_DECL, head, NULL);
    if (t == FAILURE) {
	m = MATCH_ERROR;
	goto cleanup;
    }

    return MATCH_YES;

cleanup:
    g95_free_formal_arglist(head);
    return m;
}



/* match_result()-- Match a RESULT specification following a function
 * declaration or ENTRY statement.  Also matches the end-of-statement. */

static match match_result(g95_symbol *function, g95_symbol **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *r;
match m;

    if (g95_match(" result (") != MATCH_YES)
	return MATCH_NO;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    if (g95_match(" )") != MATCH_YES) {
	g95_error("Unexpected junk following RESULT variable at %C");
	return MATCH_ERROR;
    }

    if (strcmp(function->name, name) == 0) {
	g95_error("RESULT variable at %C must be different than function "
		  "name");
	return MATCH_ERROR;
    }

    if (g95_get_symbol(name, NULL, &r))
	return MATCH_ERROR;

    if (g95_add_flavor(&r->attr, FL_VARIABLE, r->name, NULL) == FAILURE ||
	g95_add_result(&r->attr, r->name, NULL) == FAILURE)
	return MATCH_ERROR;

    *result = r;

    return MATCH_YES;
}



/* match_bind()-- Match a bind specification */

static match match_bind(g95_symbol *proc) {
char *name;
int c;

    if (G95_STRICT_F95() || g95_match(" bind ( c") != MATCH_YES)
	return MATCH_NO;

    g95_gobble_whitespace();
    c = g95_next_char();

    switch(c) {
    case ',':
	if (match_bind_name(&name) == FAILURE)
	    return MATCH_ERROR;

	if (name == NULL)
	    name = proc->name;

	break;

    case ')':
	name = proc->name;
	break;

    default:
	goto syntax;
    }
    
    proc->bind = name;

    if (g95_add_bind(&proc->attr, proc->name, NULL) == FAILURE)
	return MATCH_ERROR;

    if (g95_add_cbinding(name, CBIND_PROCEDURE, &g95_current_locus))
	return MATCH_ERROR;

    return MATCH_YES;

syntax:
    g95_error("Syntax error in BIND specification at %C");
    return MATCH_ERROR;
}



/* match_suffix()-- Match the suffix of a procedure declaration. */

static match match_suffix(g95_symbol *proc, int func, g95_symbol **result) {
int have_result;
match m;

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    have_result = 0;

    if (func) {
	m = match_result(proc, result);
	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	if (m == MATCH_YES)
	    have_result = 1;
    }

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    if (match_bind(proc) == MATCH_ERROR)
	return MATCH_ERROR;

    if (func && !have_result && match_result(proc, result) == MATCH_ERROR)
	return MATCH_ERROR;

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

    g95_error("Syntax error in procedure suffix at %C");
    return MATCH_ERROR;
}



/* g95_match_function()-- Match a function declaration */

match g95_match_function(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym, *result;
g95_locus old_loc, where;
match m;
int i;

    if (g95_current_state() != COMP_NONE &&
	g95_current_state() != COMP_INTERFACE &&
	g95_current_state() != COMP_CONTAINS)
	return MATCH_NO;

    g95_clear_ts(&current_ts);

    old_loc = g95_current_locus;

    m = match_prefix(&current_ts);
    if (m != MATCH_YES) {
	g95_current_locus = old_loc;
	return m;
    }

    if (g95_match("function% %n", name) != MATCH_YES) {
	g95_current_locus = old_loc;
	return MATCH_NO;
    }

    where = g95_def_locus;

    if (current_ts.type == BT_CHARACTER && current_ts.cl != NULL) {
	g95_free_expr(current_ts.cl->length);
	current_ts.cl->length = NULL;
    }

    g95_undo_symbols();

    if (get_proc_name(name, &sym))
	return MATCH_ERROR;

    sym->declared_at = where;     /* Replace it always */

    if (sym->attr.entry) {
	g95_error("Function '%s' at %C is already an ENTRY", sym->name);
	return MATCH_ERROR;
    }

    g95_new_block = sym;

    m = g95_match_formal_arglist(sym, FORMAL_FUNCTION, 0);
    if (m == MATCH_NO)
	g95_error("Expected formal argument list in function definition "
		  "at %C");

    if (m != MATCH_YES) {
	g95_new_block = NULL;
	goto cleanup;
    }

    result = NULL;

    if (g95_match_eos() != MATCH_YES) {
	/* See if a result variable is present */
	m = match_suffix(sym, 1, &result);
	if (m == MATCH_NO)
	    g95_error("Unexpected junk after function declaration at %C");

	if (m != MATCH_YES) {
	    m = (g95_current_form == FORM_FIXED)
		? MATCH_NO
		: MATCH_ERROR;

	    goto cleanup;
	}
    }

/* Make changes to the symbol */

    m = MATCH_ERROR;

    if (g95_add_function(&sym->attr, sym->name, NULL) == FAILURE)
	goto cleanup;

    if (copy_prefix(&sym->attr, &sym->declared_at) == FAILURE)
	goto cleanup;

    sym->result = (result == NULL)
	? sym
	: result;

    i = (g95_current_state() == COMP_INTERFACE);
    g95_set_usage(sym->result, NULL, i, i);
    return MATCH_YES;

cleanup:
    g95_reject_statement();
    g95_current_locus = old_loc;
    return m;
}



/* bindc_common()-- Parse a BIND(C) common specification. */

static match bindc_common(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_common_head *common;
g95_gsymbol *g;
match m;

    m = g95_match_name(name);
    if (m != MATCH_YES) {
	if (m == MATCH_NO) {
	    g95_error("Missing common block name at %C");
	    m = MATCH_ERROR;
	}

	return MATCH_ERROR;
    }

    if (g95_match_char('/') != MATCH_YES) {
	g95_error("Syntax error in common block specification at %C");
	m = MATCH_ERROR;
    }

    if (g95_find_state(COMP_MODULE) == FAILURE) {
	g95_error("BIND(C) common block at %C must be in a MODULE");
	return MATCH_ERROR;
    }

    if (bind_flag) {
	g95_error("Multiple entities specified with BIND(C) name at %C");
	return MATCH_ERROR;
    }

    common = g95_get_common(name, NULL);

    if (bind_name == NULL)
	common->bind = g95_get_string(name);

    else {
	common->bind = bind_name;
	bind_flag = 1;
    }

    g = g95_check_global(name, GSYM_COMMON, &g95_def_locus);
    if (g != NULL)
	g->bind = common->bind;

    return MATCH_YES;
}



/* attr_decl1()-- Function that sets the attribute of a single variable */

static match attr_decl1(int proc_flag) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_locus var_locus, where;
g95_array_spec *as;
g95_symbol *sym;
match m;

    if (current_attr.bind && g95_match_char('/') == MATCH_YES)
	return bindc_common();

    as = NULL;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	goto cleanup;

    where = g95_def_locus;

    if (find_special(name, &sym))
	return MATCH_ERROR;

    if (sym->attr.function && sym->result != sym && !sym->attr.dummy) {
	g95_error("Bad attribute for function '%s' at %C", sym->name);
	goto error;
    }

    if (sym->attr.flavor == FL_PROCEDURE && !sym->attr.dummy &&
	sym->attr.if_source == IFSRC_IFBODY &&
	g95_find_state(COMP_INTERFACE) == FAILURE) {
	g95_error("Attribute declaration of '%s' at %L is outside of the "
		  "INTERFACE body", sym->name, &g95_def_locus);
	goto error;
    }

    var_locus = g95_current_locus;

/* Deal with possible array specification for certain attributes */

    if (current_attr.dimension || current_attr.allocatable ||
	current_attr.pointer   || current_attr.target) {
	m = g95_match_array_spec(&as);
	if (m == MATCH_ERROR)
	    goto cleanup;

	if (current_attr.dimension && m == MATCH_NO) {
	    g95_error("Missing array specification at %L in DIMENSION "
		      "statement", &var_locus);
	    goto error;
	}

	if ((current_attr.allocatable || current_attr.pointer) &&
	    (m == MATCH_YES) && (as->type != AS_DEFERRED)) {
	    g95_error("Array specification must be deferred at %L",
		      &var_locus);
	    goto error;
	}
    }

/* Update symbol table.  DIMENSION attribute is set in g95_set_array_spec(). */

    if (current_attr.dimension == 0 &&
	g95_copy_attr(&sym->attr, &current_attr, &where) == FAILURE)
	goto error;

    if (current_attr.bind) {
	if (set_binding(sym) == FAILURE)
	    goto error;

	g95_add_cbinding(sym->bind, CBIND_UNKNOWN, &where);
    }

    if (sym->attr.flavor == FL_PARAMETER && as != NULL) {
	g95_error("Can't specify a DIMENSION for scalar PARAMETER '%s' at %C",
		  sym->name);
	goto error;
    }

    if (g95_set_array_spec(sym, as, &var_locus) == FAILURE)
	goto error;

    if ((proc_flag || ((current_attr.external || current_attr.intrinsic) &&
		       sym->attr.flavor != FL_PROCEDURE)) &&
	g95_add_flavor(&sym->attr, FL_PROCEDURE, sym->name, NULL) == FAILURE)
	goto error;

    return MATCH_YES;

error:
    m = MATCH_ERROR;

cleanup:
    g95_free_array_spec(as);
    return m;
}



/* attr_decl()-- Generic attribute declaration subroutine.  Used for
 * attributes that just have a list of names. */

static match attr_decl(int proc_flag) {
match m;

    m = g95_match(" ::");   /* Gobble the (usually) optional double colon */  

    if (G95_STRICT_F() && m == MATCH_NO)
	return MATCH_NO;

    for(;;) {
	m = attr_decl1(proc_flag && G95_STRICT_F());
	if (m != MATCH_YES)
	    break;

	if (g95_match_eos() == MATCH_YES) {
	    m = MATCH_YES;
	    break;
	}

	if (g95_match_char(',') != MATCH_YES) {
	    g95_error("Unexpected character in variable list at %C");
	    m = MATCH_ERROR;
	    break;
	}
    }

    return m;
}



match g95_match_protected(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_protected(&current_attr, NULL);

    m = attr_decl(0);
    if (m == MATCH_YES && G95_STRICT_F95()) {
	g95_error("PROTECTED statement at %C is not permitted in %s mode",
		  g95_mode_name());
	m = MATCH_ERROR;
    }

    if (m == MATCH_YES && g95_current_state() != COMP_MODULE) {
	g95_error("PROTECTED statement at %C must be within a MODULE");
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_volatile(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_volatile(&current_attr, NULL);

    m = attr_decl(0);

    if (m == MATCH_YES && G95_STRICT_F95()) {
	g95_error("VOLATILE statement at %C not permitted in %s mode",
		  g95_mode_name());
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_intent(void) {
g95_intent intent;
match m;

    intent = match_intent_spec();
    if (intent == INTENT_UNKNOWN)
	return MATCH_ERROR;

    g95_clear_attr(&current_attr);
    g95_add_intent(&current_attr, intent, NULL);   /* Can't fail */

    m = attr_decl(0);
    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_error("INTENT statement at %C is not permitted under F");
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_intrinsic(void) {

    g95_clear_attr(&current_attr);
    g95_add_intrinsic(&current_attr, NULL);

    return attr_decl(0);
}



match g95_match_optional(void) {

    g95_clear_attr(&current_attr);
    g95_add_optional(&current_attr, NULL, NULL);

    return attr_decl(1);
}



match g95_match_allocatable(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_allocatable(&current_attr, NULL);

    m = attr_decl(0);
    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_error("ALLOCATABLE statement at %C is not permitted under F");
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_async(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_async(&current_attr, NULL);

    m = attr_decl(0);

    if (m == MATCH_YES && G95_STRICT_F95()) {
	g95_error("ASYNCHRONOUS statement at %C not permitted in %s mode",
		  g95_mode_name());
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_external(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_external(&current_attr, NULL);

    m = attr_decl(0);
    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_error("EXTERNAL statement at %C is not permitted under F");
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_bind(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_bind(&current_attr, NULL, NULL);

    if (match_rest_bind_attr() == FAILURE)
	return MATCH_ERROR;

    m = attr_decl(0);

    if (m == MATCH_YES && G95_STRICT_F95()) {
	g95_error("BIND statement at %C not permitted in %s mode",
		  g95_mode_name());
	m = MATCH_ERROR;
    }

    return m;
}


match g95_match_value(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_value(&current_attr, NULL);

    m = attr_decl(0);

    if (m == MATCH_YES && G95_STRICT_F95()) {
	g95_error("VALUE statement at %C not permitted in %s mode",
		  g95_mode_name());
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_codimension(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_coarray_spec *cas;
g95_symbol *sym;
match m;

    g95_match(" ::");

    for(;;) {
	m = g95_match_name(name);
	if (m != MATCH_YES)
	    goto syntax;

	if (find_special(name, &sym))
	    return MATCH_ERROR;

	m = g95_match_coarray_spec(&cas);
	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	if (m == MATCH_NO) {
	    g95_error("Missing coarray specification at %C");
	    return MATCH_ERROR;
	}

	if (sym->cas != NULL) {
	    g95_free_coarray_spec(cas);
	    g95_error("Duplicate coarray specification at %C");
	    return MATCH_ERROR;
	}

	if (g95_match_eos() == MATCH_YES)
	    break;

	if (g95_match_char(',') != MATCH_YES)
	    goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in CODIMENSION statement at %C");
    return MATCH_ERROR;
}



match g95_match_target(void) {

    g95_clear_attr(&current_attr);
    g95_add_target(&current_attr, NULL);

    return attr_decl(0);
}



match g95_match_pointer(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_pointer(&current_attr, NULL, NULL);

    m = attr_decl(0);
    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_error("POINTER statement at %C is not permitted under F");
	m = MATCH_ERROR;
    }

    return m;
}



match g95_match_dimension(void) {
match m;

    g95_clear_attr(&current_attr);
    g95_add_dimension(&current_attr, NULL, NULL);

    m = attr_decl(0);
    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_error("DIMENSION statement at %C is not permitted under F");
	m = MATCH_ERROR;
    }

    return m;
}



/* g95_match_entry()-- Match an ENTRY statement */

match g95_match_entry(void) {
g95_symbol *function, *result, *entry, *sym;
char name[G95_MAX_SYMBOL_LEN+1];
g95_formal_arglist *f;
g95_compile_state s;
g95_locus where;
match m;

    m = g95_match_name(name);
    if (m != MATCH_YES)
	return m;

    where = g95_def_locus;

    if (g95_current_state() != COMP_SUBROUTINE &&
	g95_current_state() != COMP_FUNCTION) {

	s = g95_current_state();
	if (s == COMP_NONE)
	    s = COMP_PROGRAM;

	g95_error("ENTRY statement at %C cannot appear within %s",
		  g95_state_name(s));
	return MATCH_ERROR;
    }

    if (g95_current_ns->parent != NULL &&
	g95_current_ns->parent->proc_name->attr.flavor != FL_MODULE) {
	g95_error("ENTRY statement at %C cannot appear in a contained "
		  "procedure");
	return MATCH_ERROR;
    }

    if (G95_STRICT_F()) {
	g95_error("ENTRY at %C is not permitted in F");
	return MATCH_ERROR;
    }

    /* get_proc_name() makes namespace links, so check for conflicts
     * before that can happen. */

    if (g95_current_ns->parent != NULL) {
	g95_find_symbol(name, g95_current_ns->parent, 0, &sym);

	if (sym != NULL && sym->attr.subroutine) {
	    g95_error("ENTRY '%s' at %C is already a SUBROUTINE", name);
	    return MATCH_ERROR;
	}

	if (sym != NULL && sym->attr.function) {
	    g95_error("ENTRY '%s' at %C is already a FUNCTION", name);
	    return MATCH_ERROR;
	}
    }

    if (get_proc_name(name, &entry))
	return MATCH_ERROR;

    if (g95_current_ns->proc_name == entry) {
	g95_error("ENTRY '%s' at %C is the current procedure", name);
	return MATCH_ERROR;
    }

    if (g95_current_ns->proc_name->attr.subroutine) {  /* subroutine entry */
	m = g95_match_formal_arglist(entry, FORMAL_SUBROUTINE,
				     !G95_STRICT_F());
	if (m != MATCH_YES)
	    return MATCH_ERROR;

	if (g95_add_entry(&entry->attr, entry->name, &where) == FAILURE ||
	    (!entry->attr.subroutine &&
	     g95_add_subroutine(&entry->attr, entry->name, &where) == FAILURE))
	    return MATCH_ERROR;

	if (entry->ts.type == BT_UNKNOWN)
	    entry->ts.type = BT_PROCEDURE;

    } else {      /* function entry */
	if (g95_match_eos() != MATCH_YES) {
	    m = g95_match_formal_arglist(entry, FORMAL_FUNCTION, 0);
	    if (m != MATCH_YES)
		return MATCH_ERROR;
	}

	function = g95_state_stack->sym;
	result = NULL;

	if (g95_match_eos() == MATCH_YES) {
	    if (g95_add_entry(&entry->attr, entry->name, &where) == FAILURE ||
		(!entry->attr.function &&
		 g95_add_function(&entry->attr,entry->name,&where) == FAILURE))
		return MATCH_ERROR;

	    entry->result = entry;
	    entry->attr.entry_result = 1;

	} else {
	    m = match_suffix(entry, (g95_current_state() == COMP_FUNCTION),
			     &result);
	    if (m == MATCH_NO)
		g95_syntax_error(ST_ENTRY);

	    if (m != MATCH_YES)
		return MATCH_ERROR;

	    if (result == NULL)
		result = entry;

	    entry->result = result;
	    result->attr.entry_result = 1;

	    if (entry->ts.type != BT_UNKNOWN) {
		g95_error("ENTRY '%s' at %C already has a return type",
			  entry->name);
		return MATCH_ERROR;
	    }

	    if (g95_add_entry(&entry->attr, entry->name, &where) == FAILURE ||
		g95_add_function(&entry->attr, entry->name, &where) == FAILURE)
		return MATCH_ERROR;
	}
    }

    for(f=entry->formal; f; f=f->next)
	if (f->sym == entry || f->sym == g95_current_ns->proc_name) {
	    g95_error("Syntax error in argument list at %L", &f->where);
	    return MATCH_ERROR;
	}

    if (g95_current_ns->parent != NULL &&
	entry->attr.proc != PROC_MODULE &&
	g95_add_procedure(&entry->attr, PROC_MODULE,
			  entry->name, &where) == FAILURE)
	return MATCH_ERROR;

    if (g95_match_eos() != MATCH_YES) {
	g95_syntax_error(ST_ENTRY);
	return MATCH_ERROR;
    }

    entry->attr.recursive = g95_current_ns->proc_name->attr.recursive;
    entry->attr.elemental = g95_current_ns->proc_name->attr.elemental;
    entry->attr.pure      = g95_current_ns->proc_name->attr.pure;

    new_st.type = EXEC_ENTRY;
    new_st.sym = entry;

    return MATCH_YES;
}



/* g95_match_subroutine()-- Match a subroutine statement, including
 * optional prefixes. */

match g95_match_subroutine(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_locus where;
match m;

    if (g95_current_state() != COMP_NONE &&
	g95_current_state() != COMP_INTERFACE &&
	g95_current_state() != COMP_CONTAINS)
	return MATCH_NO;

    m = match_prefix(NULL);
    if (m != MATCH_YES)
	return m;

    where = g95_current_locus;

    m = g95_match("subroutine% %n", name);
    if (m != MATCH_YES)
	return m;

    if (get_proc_name(name, &sym))
	return MATCH_ERROR;

    if (sym->attr.entry) {
	g95_error("SUBROUTINE '%s' at %C is already an ENTRY", sym->name);
	return MATCH_ERROR;
    }

    sym->declared_at = g95_def_locus;
    g95_new_block = sym;

    if (g95_add_subroutine(&sym->attr, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    if (sym->ts.type == BT_UNKNOWN)
	sym->ts.type = BT_PROCEDURE;

    if (g95_match_formal_arglist(sym, FORMAL_SUBROUTINE, !G95_STRICT_F())
	!= MATCH_YES)
	return MATCH_ERROR;

    switch(match_suffix(sym, 0, NULL)) {
    case MATCH_YES:
	break;

    case MATCH_NO:
	g95_syntax_error(ST_SUBROUTINE);
	/* Fall through */

    case MATCH_ERROR:
	return MATCH_ERROR;
    }

    if (copy_prefix(&sym->attr, &sym->declared_at) == FAILURE)
	return MATCH_ERROR;

    return MATCH_YES;
}



/* contained_procedure()-- Return nonzero if we're currently compiling
 * a contained procedure. */

static int contained_procedure(void) {
g95_state_data *s;

    for(s=g95_state_stack; s; s=s->previous)
	if ((s->state == COMP_SUBROUTINE || s->state == COMP_FUNCTION) &&
	    s->previous != NULL &&
	    s->previous->state == COMP_CONTAINS)
	    return 1;

    return 0;
}



/* g95_match_end()-- Match any of the various end-block statements.
 * Returns the type of END to the caller.  The END INTERFACE, END IF,
 * END DO and END SELECT statements cannot be replaced by a single END
 * statement. */

match g95_match_end(g95_statement *st) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_compile_state state;
g95_locus old_loc;
char *block_name;
char *target;
int eos_ok;
match m;

    old_loc = g95_current_locus;
    if (g95_match("end") != MATCH_YES)
	return MATCH_NO;

    state = g95_current_state();

    block_name = g95_current_block_name();
    if (block_name[0] == '\0')
	block_name = NULL;

    if (state == COMP_CONTAINS) {
	state = g95_state_stack->previous->state;
	block_name = g95_state_stack->previous->sym == NULL ? NULL
	    : g95_state_stack->previous->sym->name;
    }

    switch(state) {
    case COMP_NONE:
    case COMP_PROGRAM:
	*st = ST_END_PROGRAM;
	target = " program";
	eos_ok = !G95_STRICT_F();
	break;

    case COMP_SUBROUTINE:
	*st = ST_END_SUBROUTINE;
	target = " subroutine";
	eos_ok = !contained_procedure();
	break;

    case COMP_FUNCTION:
	*st = ST_END_FUNCTION;
	target = " function";
	eos_ok = !contained_procedure();
	break;

    case COMP_BLOCK_DATA:
	*st = ST_END_BLOCK_DATA;
	target = " block data";
	eos_ok = 1;
	break;

    case COMP_CRITICAL:
	*st = ST_END_CRITICAL;
	target = " critical";
	eos_ok = 1;
	break;

    case COMP_MODULE:
	*st = ST_END_MODULE;
	target = " module";
	eos_ok = 1;
	break;

    case COMP_INTERFACE:
	*st = ST_END_INTERFACE;
	target = " interface";
	eos_ok = 0;
	break;

    case COMP_DERIVED:
	*st = ST_END_TYPE;
	target = " type";
	eos_ok = 0;
	break;

    case COMP_IF:
	*st = ST_ENDIF;
	target = " if";
	eos_ok = 0;
	break;

    case COMP_DO:
	*st = ST_ENDDO;
	target = " do";
	eos_ok = 0;
	break;

    case COMP_SELECT:
	*st = ST_END_SELECT;
	target = " select";
	eos_ok = 0;
	break;

    case COMP_FORALL:
	*st = ST_END_FORALL;
	target = " forall";
	eos_ok = 0;
	break;

    case COMP_WHERE:
	*st = ST_END_WHERE;
	target = " where";
	eos_ok = 0;
	break;

    case COMP_ENUM:
	*st = ST_END_ENUM;
	target = " enum";
	eos_ok = 1;
	break;

    default:
	g95_error("Unexpected END statement at %C");
	eos_ok = 0;
	goto cleanup;
    }

    if (g95_match_eos() == MATCH_YES) {
	if (!eos_ok) {
	    g95_error("%s statement expected at %C", g95_ascii_statement(*st));
	    goto cleanup;
	}

	return MATCH_YES;
    }

/* Verify that we've got the sort of end-block that we're expecting */

    if (g95_match(target) != MATCH_YES) {
	g95_error("Expecting %s statement at %C", g95_ascii_statement(*st));
	goto cleanup;
    }

/* If we're at the end, make sure a block name wasn't required */

    if (g95_match_eos() == MATCH_YES) {
	if (G95_STRICT_F()) {
	    if (*st == ST_END_TYPE) {
		g95_error("F requires type name in END TYPE statement at %C");
		return MATCH_ERROR;
	    }

	    if (*st == ST_END_PROGRAM) {
		g95_error("F requires program name in END PROGRAM statement "
			  "at %C");
		return MATCH_ERROR;
	    }

	    if (*st == ST_END_SUBROUTINE) {
		g95_error("F requires name in END SUBROUTINE statement at %C");
		return MATCH_ERROR;
	    }

	    if (*st == ST_END_MODULE) {
		g95_error("F requires name in END MODULE statement at %C");
		return MATCH_ERROR;
	    }
	}

	if (*st != ST_ENDDO && *st != ST_ENDIF && *st != ST_END_SELECT &&
	    *st != ST_END_FORALL && *st != ST_END_WHERE)
	    return MATCH_YES;

	if (g95_current_block() == NULL)
	    return MATCH_YES;

	g95_error("Expected block name of '%s' in %s statement at %C",
		  block_name, g95_ascii_statement(*st));

	return MATCH_ERROR;
    }

/* END INTERFACE has a special handler for its several possible endings */

    if (*st == ST_END_INTERFACE)
	return g95_match_end_interface();

/* We haven't hit the end of statement, so what is left must be an end-name */

    m = g95_match_space();
    if (m == MATCH_YES)
	m = g95_match_name(name);

    if (m == MATCH_NO)
	g95_error("Expected terminating name at %C");

    if (m != MATCH_YES)
	goto cleanup;

    if (block_name == NULL)
	goto syntax;

    if (strcmp(name, block_name) != 0) {
	g95_error("Expected label '%s' for %s statement at %C", block_name,
		  g95_ascii_statement(*st));
	goto cleanup;
    }

    if (g95_match_eos() == MATCH_YES)
	return MATCH_YES;

syntax:
    g95_syntax_error(*st);

cleanup:
    g95_current_locus = old_loc;
    return MATCH_ERROR;
}



/* access_attr_decl()-- match the list of entities being specified in
 * a PUBLIC or PRIVATE statement. */

static match access_attr_decl(g95_statement st) {
char name[G95_MAX_SYMBOL_LEN+1];
int colon, operator;
interface_type type;
g95_access access;
g95_user_op *uop;
g95_symbol *sym;
g95_symtree *s;
match m;

    m = g95_match(" ::");
    colon = (m == MATCH_YES);

    if (m == MATCH_NO && g95_match_space() == MATCH_NO)
	goto done;

    access = (st == ST_PUBLIC)
	? ACCESS_PUBLIC
	: ACCESS_PRIVATE;

    for(;;) {
	m = g95_match_generic_spec(&type, name, &operator);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) return MATCH_ERROR;

	switch(type) {
	case INTERFACE_NAMELESS:
	    goto syntax;

	case INTERFACE_GENERIC:
	    s = g95_find_generic(name, NULL);
	    if (s != NULL) {
		if (s->access != ACCESS_UNKNOWN) {
		    g95_error("Generic name '%s' at %C already has an "
			      "ACCESS-spec", name);
		    return MATCH_ERROR;
		}

		s->access = access;

	    } else {
		if (g95_get_symbol(name, NULL, &sym))
		    goto done;

		if (g95_add_access(&sym->attr, access, NULL, NULL) == FAILURE)
		    return MATCH_ERROR;

		if (G95_STRICT_F() && sym->attr.flavor != FL_PROCEDURE &&
		    g95_add_procedure(&sym->attr, PROC_UNKNOWN,
				      name, NULL) == FAILURE)
		    return MATCH_ERROR;
	    }

	    break;

	case INTERFACE_INTRINSIC_OP:
	    if (g95_current_ns->operator_access[operator] == ACCESS_UNKNOWN)
		g95_current_ns->operator_access[operator] = access;

	    else {
		g95_error("Access specification of the %s operator at %C has "
			  "already been specified", g95_op2string(operator));
		goto done;
	    }

	    break;

	case INTERFACE_USER_OP:
	    uop = g95_get_uop(name);

	    if (uop->access == ACCESS_UNKNOWN)
		uop->access = access;

	    else {
		g95_error("Access specification of the .%s. operator at %C "
			  "has already been specified", sym->name);
		goto done;
	    }

	    break;

	default:
	    g95_internal_error("access_attr_decl(): Bad interface");
	}

	if (g95_match_char(',') == MATCH_NO)
	    break;
    }

    if (g95_match_eos() != MATCH_YES)
	goto syntax;

    if (G95_STRICT_F() && !colon) {
	g95_error("%s statement at %C must have a double-colon in F-mode",
		  g95_ascii_statement(st));
	return MATCH_ERROR;
    }

    if (g95_current_state() != COMP_MODULE) {  /* C548 */
	g95_error("%s statement at %C must appear in the "
		  "specification-part of a MODULE", g95_ascii_statement(st));
	return MATCH_ERROR;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(st);

done:
    return MATCH_ERROR;
}



/* g95_match_private()-- The PRIVATE statement is a bit weird in that
 * it can be a attribute declaration, but also works as a standalone
 * statement inside of a type declaration or a module. */

match g95_match_private(g95_statement *st) {

    if (g95_match("private") != MATCH_YES)
	return MATCH_NO;

    if (g95_current_state() == COMP_DERIVED) {
	if (g95_match_eos() == MATCH_YES) {
	    *st = ST_PRIVATE;
	    return MATCH_YES;
	}

	g95_syntax_error(ST_PRIVATE);
	return MATCH_ERROR;
    }

    if (g95_match_eos() == MATCH_YES) {
	*st = ST_PRIVATE;
	return MATCH_YES;
    }

    *st = ST_ATTR_DECL;
    return access_attr_decl(ST_PRIVATE);
}



match g95_match_public(g95_statement *st) {

    if (g95_match("public") != MATCH_YES)
	return MATCH_NO;

    if (g95_match_eos() == MATCH_YES) {
	*st = ST_PUBLIC;
	return MATCH_YES;
    }

    *st = ST_ATTR_DECL;
    return access_attr_decl(ST_PUBLIC);
}



/* g95_match_modproc()-- Match a module procedure statement.  Note
 * that we have to modify symbols in the parent's namespace because
 * the current one was there to receive symbols that are in a
 * interface's formal argument list. */

match g95_match_modproc(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_locus where;
match m;

    if (g95_state_stack->state != COMP_INTERFACE ||
	g95_state_stack->previous == NULL ||
	current_interface.type == INTERFACE_NAMELESS) {
	g95_error("MODULE PROCEDURE at %C must be in a generic module "
		  "interface");
	return MATCH_ERROR;
    }

    for(;;) {
	m = g95_match_name(name);
	if (m == MATCH_NO) goto syntax;
	if (m != MATCH_YES) return MATCH_ERROR;

	where = g95_def_locus;

	if (g95_get_symbol(name, g95_current_ns->parent, &sym))
	    return MATCH_ERROR;

	if (sym->attr.proc != PROC_MODULE &&
	    g95_add_procedure(&sym->attr, PROC_MODULE,
			      sym->name, NULL) == FAILURE)
	    return MATCH_ERROR;

	if (g95_add_interface(sym, &where) == FAILURE)
	    return MATCH_ERROR;

	sym->attr.modproc = 1;

	if (g95_match_eos() == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_MODULE_PROC);
    return MATCH_ERROR;
}



/* g95_match_save()-- Save statements have a special syntax */

match g95_match_save(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_common_head *h;
g95_symbol *sym;
match m;

    if (g95_match_eos() == MATCH_YES) {
	if (!g95_option.multiple_save && g95_current_ns->seen_save &&
	    G95_STRICT_F95()) {
	    g95_error("Blanket SAVE statement at %C follows previous "
		      "SAVE statement");

	    return MATCH_ERROR;
	}

	g95_current_ns->save_all = g95_current_ns->seen_save = 1;
	return MATCH_YES;
    }

    if (!g95_option.multiple_save && g95_current_ns->save_all) {
	g95_error("SAVE statement at %C follows blanket SAVE statement");
	return MATCH_ERROR;
    }

    g95_match(" ::");

    for(;;) { 
	m = g95_match_symbol(&sym, 0);
	switch(m) {
	case MATCH_YES:
	    if (g95_add_save(&sym->attr, sym->name, NULL) == FAILURE)
		return MATCH_ERROR;
	    goto next_item;

	case MATCH_NO:
	    break;

	case MATCH_ERROR:
	    return MATCH_ERROR;
	}

	m = g95_match(" / %n /", &name);
	if (m == MATCH_ERROR) return MATCH_ERROR;
	if (m == MATCH_NO) goto syntax;

	h = g95_get_common(name, &g95_def_locus);
	h->saved = 1;
	g95_current_ns->seen_save = 1;

    next_item:
	if (g95_match_eos() == MATCH_YES) break;
	if (g95_match_char(',') != MATCH_YES) goto syntax;
    }

    if (G95_STRICT_F()) {
	g95_error("SAVE statement at %C not permitted in F");
	return MATCH_ERROR;
    }

    return MATCH_YES;

syntax:
    g95_error("Syntax error in SAVE statement at %C");
    return MATCH_ERROR;
}



/* g95_match_derived_decl()-- Match the beginning of a derived type
 * declaration.  If a type name was the result of a function, then it is
 * possible to have a symbol already to be known as a derived type yet
 * have no components. */

match g95_match_derived_decl(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym, *extends;
g95_compile_state state;
symbol_attribute attr;
int double_colon;
match m;

    if (g95_current_state() == COMP_DERIVED)
	return MATCH_NO;

    g95_clear_attr(&attr);
    double_colon = 0;
    extends = NULL;

    g95_enclosing_unit(&state);

    for(;;) {
	if (g95_match(" , private") == MATCH_YES) {
	    if (state != COMP_MODULE) {
		g95_error("Derived type at %C can only be PRIVATE within "
			  "a MODULE");
		return MATCH_ERROR;
	    }

	    if (g95_add_access(&attr, ACCESS_PRIVATE, NULL, NULL) == FAILURE)
		return MATCH_ERROR;

	    continue;
	}

	if (g95_match(" , public") == MATCH_YES) {
	    if (state != COMP_MODULE) {
		g95_error("Derived type at %C can only be PUBLIC within a MODULE");
		return MATCH_ERROR;
	    }

	    if (g95_add_access(&attr, ACCESS_PUBLIC, NULL, NULL) == FAILURE)
		return MATCH_ERROR;

	    continue;
	}

	if (!G95_STRICT_F95() && g95_match(" , bind ( c )") == MATCH_YES) {
	    if (g95_add_bind(&attr, NULL, NULL) == FAILURE)
		return MATCH_ERROR;

	    continue;
	}

	if (g95_match(" , abstract") == MATCH_YES) {
	    if (g95_add_abstract(&attr, NULL, NULL) == FAILURE)
		return MATCH_ERROR;

	    continue;
	}

	m = g95_match(", extends ( %s )", &sym);
	if (m == MATCH_ERROR)
	    return MATCH_ERROR;

	if (m == MATCH_YES) {
	    if (extends != NULL) {
		g95_error("Multiple EXTENDS specifications at %C");
		return MATCH_ERROR;
	    }

	    if (sym->attr.flavor != FL_DERIVED) {
		g95_error("EXTENDS symbol '%s' at %C must be a derived type",
			  sym->name);
		return MATCH_ERROR;
	    }

	    extends = sym;
	    continue;
	}

	break;
    }

    if (g95_match(" ::") == MATCH_YES)
	double_colon = 1;

    else if (attr.access != ACCESS_UNKNOWN) {
	g95_error("Expected :: in TYPE definition at %C");
	return MATCH_ERROR;
    }

    m = g95_match(double_colon ? "%n%t" : "% %n%t", name);
    if (m != MATCH_YES)
	return m;

/* Make sure the name isn't the name of an intrinsic type.  The
 * 'double precision' type doesn't get past the name matcher */

    if (strcmp(name, "integer") == 0   || strcmp(name, "real") == 0 ||
	strcmp(name, "character") == 0 || strcmp(name, "logical") == 0 ||
	strcmp(name, "complex") == 0   || strcmp(name, "doubleprecision")==0) {
	g95_error("Type name '%s' at %C cannot be the same as an "
		  "intrinsic type", name);
	return MATCH_ERROR;
    }

    if (g95_get_symbol(name, NULL, &sym))
	return MATCH_ERROR;

    if (sym->ts.type != BT_UNKNOWN) {
	g95_error("Derived type name '%s' at %C already has a basic type "
		  "of %s", sym->name, g95_typename(&sym->ts));
	return MATCH_ERROR;
    }

/* The symbol may already have the derived attribute without the
 * components.  The ways this can happen is via a function definition,
 * an INTRINSIC statement or a subtype in another derived type that is
 * a pointer.  The first part of the AND clause is true if a the
 * symbol is not the return value of a function. */

    if (sym->attr.flavor != FL_DERIVED &&
	g95_add_flavor(&sym->attr, FL_DERIVED, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    if (sym->components != NULL) {
	g95_error("Derived type definition of '%s' at %C has already been "
		  "defined", sym->name);
	return MATCH_ERROR;
    }

    if (state != COMP_MODULE && G95_STRICT_F()) {
	g95_error("Derived type definition at %C must be in a MODULE in F");
	return MATCH_ERROR;
    }

    if (attr.access != ACCESS_UNKNOWN &&
	g95_add_access(&sym->attr, attr.access, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    if (attr.bind && g95_add_bind(&sym->attr, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    if (attr.abstract &&
	g95_add_abstract(&sym->attr, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    sym->extends = extends;
    g95_new_block = sym;

    return MATCH_YES;
}


/* single_parameter()-- Workhorse for g95_match_parameter */

static match single_parameter(void) {
g95_symbol *sym;
g95_expr *init;
match m;

    m = g95_match_symbol(&sym, 0);
    if (m == MATCH_NO)
	g95_error("Expected variable name at %C in PARAMETER statement");

    if (m != MATCH_YES)
	return m;

    if (g95_match_char('=') == MATCH_NO) {
	g95_error("Expected = sign in PARAMETER statement at %C");
	return MATCH_ERROR;
    }

    if (g95_add_flavor(&sym->attr, FL_PARAMETER, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

    m = g95_match_init_expr(&init);
    if (m == MATCH_NO)
	g95_error("Expected expression at %C in PARAMETER statement");

    if (m != MATCH_YES)
	return m;

    if (sym->ts.type == BT_UNKNOWN &&
	g95_set_default_type(sym, 1, NULL) == FAILURE)
	return MATCH_ERROR;

    current_attr.flavor = FL_PARAMETER;
    if (init_symbol(sym->name, &init) == FAILURE)
	return MATCH_ERROR;

    return MATCH_YES;
}



/* g95_match_parameter()-- Match a parameter statement, with the weird
 * syntax that these have */

match g95_match_parameter(void) {
match m;

    if (g95_match_char('(') == MATCH_NO)
	return MATCH_NO;

    for(;;) {
	m = single_parameter();
	if (m != MATCH_YES)
	    break;

	if (g95_match(" )%t") == MATCH_YES)
	    break;

	if (g95_match_char(',') != MATCH_YES) {
	    g95_error("Unexpected characters in PARAMETER statement at %C");
	    m = MATCH_ERROR;
	    break;
	}
    }

    return m;
}

