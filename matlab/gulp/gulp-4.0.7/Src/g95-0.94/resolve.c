
/* Perform type resolution on the various structures.
   Copyright (C) 2001 - 2008 Free Software Foundation, Inc.
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

#include <string.h>
#include "g95.h"

/* Stack to push the current if we descend into a block during
 * resolution.  See resolve_branch() and resolve_code().  */

typedef struct code_stack {
    struct g95_code *head, *current;
    struct code_stack *prev;
} code_stack;


static code_stack *cs_base = NULL;

static int forall_mask, data_flag, actual_resolve=2;
int g95_where_flag;

typedef struct where_frame {
    g95_code *head;
    struct where_frame *prev;
} where_frame;


typedef struct forall_frame {
    g95_code *code;
    struct forall_frame *prev;
} forall_frame;

typedef enum {
    PTYPE_GENERIC=1, PTYPE_SPECIFIC, PTYPE_UNKNOWN
} proc_type;

int g95_forall_flag;   /* Nonzero if we're inside a FORALL block */
extern int g95_constructor_string_length;

typedef struct {
    enum { EQ_NUMERIC, EQ_CHARACTER, EQ_SPECIFIC } type;
    g95_typespec ts;
} eq_class;


typedef struct data_frame {
    g95_symbol *iterator;
    struct data_frame *prev;
} data_frame;


static data_frame *data_root = NULL;
static forall_frame *forall_root = NULL;

typedef struct st_frame {
    g95_symbol *sym;
    struct st_frame *prev;
} st_frame;


static try resolve_formal_arglist(g95_symbol *);
static try resolve_symbol(g95_symbol *);
static int pure_function(g95_expr *, char **, st_frame *);



/* namespace_kind()-- Given a namespace, figure out what kind it is.
 * We return one of the g95_compile_state enums COMP_NONE,
 * COMP_MODULE, COMP_SUBROUTINE or COMP_FUNCTION. */

static g95_compile_state namespace_kind(g95_namespace *ns) {
g95_symbol *sym;

    sym = ns->proc_name;

    if (sym == NULL)
	return COMP_NONE;

    if (sym->attr.flavor == FL_MODULE)
	return COMP_MODULE;

    if (sym->attr.flavor == FL_VARIABLE || sym->attr.function)
	return COMP_FUNCTION;

    if (sym->attr.subroutine)
	return COMP_SUBROUTINE;

    return COMP_NONE;
}



/* g95_derived_init()-- Return nonzero if the derived type symbol has a
 * default initialization or contains a subtype that has a default
 * initialization.  */

int g95_derived_init(g95_symbol *sym) {
g95_component *c;

    for(c=sym->components; c; c=c->next) {
	if (c->initializer != NULL)
	    return 1;

	if (c->ts.type != BT_DERIVED || c->pointer)
	    continue;

	if (g95_derived_init(c->ts.derived))
	    return 1;
    }

    return 0;
}



/* resolve_charlen()-- Resolve character lengths */

static try resolve_charlen(g95_charlen *cl) {
g95_expr *e;

    if (cl->resolved)
	return SUCCESS;

    cl->resolved = 1;

    if (g95_resolve_expr(cl->length) == FAILURE)
	return FAILURE;

    if (g95_simplify_spec_expr(cl->length) == FAILURE)
	return FAILURE;

    if (g95_specification_expr(cl->length) == FAILURE)
	return FAILURE;

    if (cl->length != NULL && cl->length->type == EXPR_CONSTANT &&
	bi_compare(cl->length->value.integer,
		   bi_huge(g95_default_integer_kind(0))) > 0) {
	g95_error("Character length at %L too long", &cl->length->where);
	return FAILURE;
    }

    if (g95_compare_expr_int(cl->length, 0) == CMP_LT) {
	e = g95_int_expr(0);
	e->where = cl->length->where;

	g95_replace_expr(cl->length, e);
    }

    return SUCCESS;
}



/* was_declared()-- Returns 0 if a symbol was not declared with a type
 * or attribute declaration statement, nonzero otherwise. */

static int was_declared(g95_symbol *sym) {
symbol_attribute a;

    a = sym->attr;

    if (!a.implicit_type && sym->ts.type != BT_UNKNOWN &&
	sym->ts.type != BT_PROCEDURE)
	return 1;

    if (a.allocatable || a.dimension || a.external || a.intrinsic ||
	a.optional || a.pointer || a.save || a.target ||
	a.access != ACCESS_UNKNOWN || a.intent != INTENT_UNKNOWN ||
	a.proc == PROC_MODULE || a.proc == PROC_INTERNAL ||
	a.proc == PROC_ST_FUNCTION || a.proc == PROC_DUMMY)
	return 1;

    return 0;
}



/* resolve_structure_cons()-- Resolve all of the elements of a
 * structure constructor and make sure that the types are correct. */

static try resolve_structure_cons(g95_expr *expr) {
g95_constructor *cons;
g95_component *comp;
g95_locus where;
comparison cmp;
int rank;

    cons = expr->value.constructor.c;
    comp = expr->symbol->components;
    where = expr->where;

    for(; comp; comp=comp->next, cons=cons->next) {
	if (cons == NULL) {
	    g95_error("Not enough values in structure constructor at %L"
		      , &where);
	    return FAILURE;
	}

       	if (cons->expr == NULL && comp->allocatable)
	    continue;

	where = cons->expr->where;

	if (g95_resolve_expr(cons->expr) == FAILURE)
	    return FAILURE;

	if (!comp->pointer && !comp->allocatable &&
	    cons->expr->type == EXPR_NULL) {
	    g95_error("NULL() at %L cannot be a structure component for a "
		      "non-pointer", &cons->expr->where);
	    return FAILURE;
	}

	/* If we don't have the right type, try to convert it. */

	if (!g95_compare_types(&cons->expr->ts, &comp->ts)) {
	    if (comp->pointer && cons->expr->ts.type != BT_UNKNOWN) {
		g95_error("Wrong pointer type in structure constructor at %L",
			  &cons->expr->where);
		return FAILURE;
	    }

	    if (g95_convert_type(cons->expr, &comp->ts, 0) == FAILURE)
		return FAILURE;
	}

	if (comp->pointer && comp->ts.type == BT_CHARACTER) {
	    cmp = g95_compare_expr(comp->ts.cl->length,
				   cons->expr->ts.cl->length);
	    if (cmp == CMP_GT || cmp == CMP_LT) {
		g95_error("Wrong CHARACTER length in constructor pointer "
			  "at %L", &cons->expr->where);
		return FAILURE;
	    }
	}

	if (comp->pointer && !g95_pointer_expr(cons->expr) &&
	    !g95_target_expr(cons->expr)) {
	    g95_error("Structure constructor component '%s' at %L must be a "
		"POINTER or TARGET", comp->name, &cons->expr->where);
	    return FAILURE;
	}

	if (comp->allocatable && cons->expr->type != EXPR_NULL &&
	    cons->expr->rank == 0) {
	    g95_error("ALLOCATABLE structure constructor component '%s' at %L "
		      "cannot be initialized with a scalar",
		      comp->name, &cons->expr->where);
	    return FAILURE;
	}

	rank = (comp->as == NULL) ? 0 : comp->as->rank;

	if (cons->expr->rank != 0 && cons->expr->rank != rank) {
	    if (cons->expr->type == EXPR_NULL && cons->expr->rank == -1)
		cons->expr->rank = rank;

	    else {
		g95_error("Rank of structure constructor component '%s' at "
			  "%L is incompatible", comp->name,
			  &cons->expr->where);
		return FAILURE;
	    }
	}
    }

    if (cons != NULL) {
	g95_error("Too many values in structure constructor at %L",
		  &cons->expr->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* g95_generic_sym()-- Determine if a symbol is generic or not */

int g95_generic_sym(char *name, g95_namespace *ns, int sub) {
g95_symbol *sym;

    if (ns == NULL)
	return 0;

    if (g95_find_generic(name, ns) != NULL)
	return 1;

    g95_find_symbol(name, ns, 0, &sym);
    if (sym != NULL &&
	(sym->attr.intrinsic && g95_generic_intrinsic(name, sub)))
	return 1;

    if (sym != NULL && was_declared(sym))
	return 0;

    return g95_generic_sym(name, ns->parent, sub);
}



/* specific_sym()-- Determine if a symbol is specific or not */

static int specific_sym(g95_symbol *sym) {
g95_symbol *s;

    if (sym->attr.if_source == IFSRC_IFBODY || sym->attr.proc == PROC_MODULE   ||
	sym->attr.if_source == IFSRC_DECL   || sym->attr.proc == PROC_INTERNAL ||
	sym->attr.proc == PROC_ST_FUNCTION  || sym->attr.external              ||
	sym->attr.iproc != IPROC_NONE       ||
	(sym->attr.intrinsic && g95_specific_intrinsic(sym->name)))
	return 1;

    if (was_declared(sym) || sym->ns->parent == NULL)
	return 0;

    g95_find_symbol(sym->name, sym->ns->parent, 1, &s);

    return (s == NULL) ? 0 : specific_sym(s);
}



/* procedure_kind()-- Figure out if the procedure is specific, generic
 * or unknown. */

static proc_type procedure_kind(char *name) {
g95_symbol *sym;

    g95_find_symbol(name, NULL, 1, &sym);

    if (g95_generic_sym(name, sym->ns, sym->attr.subroutine))
	return PTYPE_GENERIC;

    if (specific_sym(sym))
	return PTYPE_SPECIFIC;

    return PTYPE_UNKNOWN;
}



/* find_declaration()-- Given a symbol, figure out where it is
 * declared.  Returns the symbol to use, usually the original symbol
 * itself. */

static g95_symbol *find_declaration(g95_symbol *sym) {
g95_namespace *ns;
g95_symbol *s;

    ns = sym->ns;

    for(;;) {
	if (g95_local_symbol(sym))
	    break;

	ns = ns->parent;
	if (ns == NULL || g95_find_symbol(sym->name, ns, 1, &s))
	    break;

	if (s != NULL) {
	    sym->attr.set = sym->attr.used = 0;
	    sym->attr.resolved = 1;

	    sym = s;
	}
    }

    return sym;
}



/* resolve_actual_argument()-- Resolve an actual argument. */

static try resolve_actual_argument(g95_expr *e) {
g95_symbol *sym, *old;
g95_intrinsic_sym *s;
int save;
try t;

    if (e->ts.type != BT_PROCEDURE) {
	save = actual_resolve;
	actual_resolve = 0;

	t = g95_resolve_expr(e);
	actual_resolve = save;

	if (t == FAILURE)
	    return t;

	if (e->rank > 0 && e->ref != NULL && e->ref->type == REF_ARRAY &&
	    e->ref->u.ar.type == AR_SECTION)
	    e->symbol->attr.desc = 1;

	if (e->type == EXPR_VARIABLE && e->symbol->attr.allocatable &&
	    e->ref != NULL && e->ref->type == REF_ARRAY &&
	    e->ref->u.ar.type == AR_FULL && e->ref->next == NULL)
	    e->symbol->attr.alloced = 1;

	if (e->ts.type != BT_PROCEDURE)
	    return SUCCESS;
    }

    /* See if the expression node should really be a variable reference */

    sym = find_declaration(e->symbol);
    old = e->symbol;
    e->symbol = sym;

    if (old != sym && old->ts.type == BT_UNKNOWN && sym->ts.type != BT_UNKNOWN)
	old->ts = sym->ts;

    if (sym->attr.flavor == FL_PROCEDURE) {
	if (g95_find_generic(sym->name, sym->ns) != NULL &&
	    !sym->attr.function && !sym->attr.subroutine) {
	    g95_error("Generic name '%s' used as actual argument at %L",
		      sym->name, &e->where);
	    return FAILURE;
	}

	if (sym->attr.proc == PROC_ST_FUNCTION) {
	    g95_error("Statement function '%s' at %L cannot be used as an "
		      "actual argument", sym->name, &e->where);
	    return FAILURE;
	}

	if (sym->attr.proc == PROC_INTERNAL && G95_STRICT_F95()) {
	    g95_error("Internal function '%s' at %L cannot be an actual "
		      "argument in f95 mode", sym->name, &e->where);
	    return FAILURE;
	}

	if (sym->attr.intrinsic) {
	    s = g95_find_function(e->symbol->name);

	    if (s == NULL)
		s = g95_find_subroutine(e->symbol->name);

	    if (s != NULL && s->actual == NULL) {
		g95_error("Intrinsic procedure '%s' at %L cannot be used "
			  "as an actual argument", s->name, &e->where);
		return FAILURE;
	    }
	}
    }

    if (sym->attr.flavor == FL_PROCEDURE || sym->attr.intrinsic ||
	sym->attr.external) {

	if (sym->attr.elemental) {
	    g95_error("ELEMENTAL procedure '%s' at %L cannot be used as "
		      "an actual argument", sym->name, &e->where);
	    return FAILURE;
	}

	if (sym->attr.proc == PROC_UNKNOWN) {
	    if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY)
		sym->attr.proc = PROC_EXTERNAL;

	    else if (sym->attr.intrinsic)
		sym->attr.proc = PROC_INTRINSIC;

	    if (sym->result == NULL)
		sym->result = sym;
	}

	sym->attr.dummy_proc = 1;
	old->attr.dummy_proc = 1;
	e->rank = 0;

	return SUCCESS;
    }

    e->type = EXPR_VARIABLE;
    e->ts   = sym->ts;

    if (sym->as != NULL) {
	e->rank = sym->as->rank;
	e->ref  = g95_full_ref(&e->where);
    }

    return g95_resolve_expr(e);
}



/* resolve_formal_arg()-- Resolve types of formal argument lists.
 * These have to be done early so that the formal argument lists of
 * module procedures can be copied to the containing module before the
 * individual procedures are resolved individually.  We also resolve
 * argument lists of procedures in interface blocks because they are
 * self-contained scoping units.
 *
 * Since a dummy argument cannot be a non-dummy procedure, the only
 * resort left for untyped names are the IMPLICIT types. */

static try resolve_formal_arg(g95_symbol *proc, g95_formal_arglist *f) {
g95_namespace *ns, *formal_ns;
g95_symbol *sym;
try t;

    sym = f->sym;

    if (sym == NULL) {  /* Alternate return placeholder */
	if (g95_elemental(proc))
	    g95_error("Alternate return specifier in elemental subroutine "
		      "'%s' at %L is not allowed", proc->name, &f->where);
	return FAILURE;
    }

    if (sym->as != NULL && sym->as->type == AS_DEFERRED && !sym->attr.pointer)
	sym->as->type = AS_ASSUMED_SHAPE;

    if (sym->attr.intent == INTENT_UNKNOWN &&
	sym->ts.type != BT_PROCEDURE && !sym->attr.function &&
	!sym->attr.subroutine && !sym->attr.pointer &&
	proc->attr.proc != PROC_ST_FUNCTION) {

	if (G95_STRICT_F()) {
	    g95_error("Dummy argument '%s' at %L does not have an INTENT",
		      sym->name, &f->where);
	    return FAILURE;
	}

	if (g95_option.missing_intent)
	    g95_warning(163, "Actual argument '%s' at %L does not have an "
			"INTENT", sym->name, &f->where);
    }

    if (sym->attr.intent != INTENT_OUT)
	g95_set_usage(sym, NULL, 1, 0);

    if (sym->attr.intent != INTENT_IN)
	g95_set_usage(sym, NULL, 0, 1);

    if (sym->attr.if_source != IFSRC_UNKNOWN)
	resolve_formal_arglist(sym);

    if (sym->attr.flavor == FL_UNKNOWN &&
	(sym->attr.external || sym->attr.intrinsic))
	g95_add_flavor(&sym->attr, FL_PROCEDURE, sym->name, &f->where);

    if (sym->attr.flavor == FL_PROCEDURE) {
	if (g95_pure(proc, 0) && !g95_pure(sym, 0)) {
	    g95_error("Dummy procedure '%s' of PURE procedure at %L must also "
		      "be PURE", sym->name, &f->where);
	    return FAILURE;
	}

	if (g95_elemental(proc)) {
	    g95_error("Dummy procedure at %L not allowed in ELEMENTAL "
		      "procedure", &f->where);
	    return FAILURE;
	}

	if (sym->attr.flavor == FL_UNKNOWN)
	    g95_add_flavor(&sym->attr, FL_PROCEDURE, sym->name, &f->where);

	if (sym->attr.proc == PROC_INTERNAL) {
	    g95_error("Dummy procedure at %L cannot be an internal procedure",
		      &f->where);
	    return FAILURE;
	}

	if (sym->attr.function && sym->result->ts.type == BT_UNKNOWN &&
	    g95_set_default_type(sym->result, 1, sym->formal_ns) == FAILURE)
	    return FAILURE;

	return SUCCESS;
    }

    ns = g95_current_ns;
    if (sym->formal_ns != NULL)
	g95_current_ns = sym->formal_ns;

    if (sym->ts.type == BT_UNKNOWN) {
	if (!sym->attr.function || sym->result == sym) {
	    formal_ns = (sym->formal_ns != NULL)
		? sym->formal_ns
		: sym->ns;

	    if (g95_set_default_type(sym, 1, formal_ns) == FAILURE)
		return FAILURE;

	} else {    /* Set the type of the RESULT, then copy */
	    if (sym->result->ts.type == BT_UNKNOWN)
		g95_set_default_type(sym->result, 1, sym->result->ns);

	    sym->ts = sym->result->ts;
	    if (sym->as == NULL)
		sym->as = g95_copy_array_spec(sym->result->as);
	}
    }

    t = g95_resolve_array_spec(sym->as);

    if (t == SUCCESS && sym->ts.type == BT_CHARACTER)
	t = resolve_charlen(sym->ts.cl);

    g95_current_ns = ns;

    if (t == FAILURE)
	return FAILURE;

    if (proc->attr.proc == PROC_ST_FUNCTION && sym->as != NULL)
	g95_error("Argument '%s' of statement function at %L must be scalar",
		  sym->name, &f->where);

    /* If the flavor is unknown at this point, it has to be a variable.
     * A procedure specification would have already set the type */

    if (sym->attr.flavor == FL_UNKNOWN)
	g95_add_flavor(&sym->attr, FL_VARIABLE, sym->name, &f->where);

    if (g95_pure(proc, 1)) {
	if (proc->attr.function && !sym->attr.pointer && !sym->attr.function &&
	    !sym->attr.subroutine && sym->attr.intent != INTENT_IN) {
	    g95_error("Argument '%s' of pure function '%s' at %L must be "
		      "INTENT(IN)", sym->name, proc->name, &f->where);
	    return FAILURE;
	}

	if (proc->attr.subroutine && !sym->attr.pointer &&
	    !sym->attr.function && !sym->attr.subroutine &&
	    sym->attr.intent == INTENT_UNKNOWN) {
	    g95_error("Argument '%s' of pure subroutine '%s' at %L must have "
		      "its INTENT specified", sym->name, proc->name,
		      &f->where);
	    return FAILURE;
	}
    }

    if (g95_elemental(proc)) {
	if (sym->as != NULL) {
	    g95_error("Argument '%s' of elemental procedure at %L must be "
		      "scalar", sym->name, &f->where);
	    return FAILURE;
	}

	if (sym->attr.pointer) {
	    g95_error("Argument '%s' of elemental procedure at %L cannot have "
		      "the POINTER attribute", sym->name, &f->where);
	    return FAILURE;
	}

	if (sym->attr.function || sym->attr.subroutine) {
	    g95_error("Argument '%s' of elemental procedure at %L cannot be a "
		      "dummy procedure", sym->name, &f->where);
	    return FAILURE;
	}
    }

    if (sym->as != NULL && sym->as->type == AS_ASSUMED_SIZE &&
	sym->attr.intent == INTENT_OUT && sym->ts.type == BT_DERIVED &&
	g95_derived_init(sym->ts.derived)) {
	g95_error("Dummy argument '%s' at %L cannot be an assumed size "
		  "derived type array with INTENT(OUT) and a default "
		  "initialization", sym->name, &f->where);
	return FAILURE;
    }

    /* Third constraint of 5.2.3 in f95, removed in F2003. */
    
    if (G95_STRICT_F95() && sym->ts.type == BT_DERIVED &&
	!sym->attr.use_assoc && proc->attr.proc == PROC_MODULE &&
	g95_symbol_access(proc) != ACCESS_PRIVATE &&
	g95_symbol_access(sym->ts.derived) == ACCESS_PRIVATE &&
	!sym->ts.derived->attr.use_assoc) {
	g95_error("Dummy argument '%s' at %L cannot be a PRIVATE type of a "
		  "PUBLIC procedure", sym->name, &f->where);
	return FAILURE;
    }

    if (G95_STRICT_F()) {
	if (sym->attr.flavor == FL_VARIABLE && sym->as != NULL &&
	    !sym->attr.pointer && !sym->attr.allocatable &&
	    sym->as->type != AS_ASSUMED_SHAPE) {
	    g95_error("Dummy array '%s' at %L must be assumed shape in F mode",
		      sym->name, &f->where);
	    return FAILURE;
	}

	if (sym->ts.type == BT_CHARACTER && sym->ts.cl->length != NULL) {
	    g95_error("Formal CHARACTER argument '%s' at %L must be "
		      "assumed-length in F mode", sym->name, &f->where);
	    return FAILURE;
	}
    }

    if (f->cb == CB_VALUE) {
	switch(sym->ts.type) {
	case BT_INTEGER:   case BT_REAL:   case BT_LOGICAL:   case BT_DERIVED:
	    break;

	default:
	    g95_error("Can't pass argument '%s' of type %s by value at %L",
		      sym->name, g95_basic_typename(sym->ts.type), &f->where);
	    return FAILURE;
	}

	if (sym->as != NULL) {
	    g95_error("Can't pass array argument '%s' by value at %L",
		      sym->name, &f->where);
	    return FAILURE;
	}

	if (sym->attr.pointer) {
	    g95_error("Can't pass pointer argument '%s' by value at %L",
		      sym->name, &f->where);
	    return FAILURE;
	}

	if (sym->attr.proc != PROC_UNKNOWN) {
	    g95_error("Can't pass a procedure argument '%s' by value at %L",
		      sym->name, &f->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* resolve_formal_arglist()-- Resolve a formal argument list. */

static try resolve_formal_arglist(g95_symbol *proc) {
g95_formal_arglist *f;
g95_namespace *ns;
try t;

    for(f=proc->formal; f; f=f->next) {
	ns = g95_current_ns;
	if (f->sym != NULL && f->sym->formal_ns != NULL)
	    g95_current_ns = f->sym->formal_ns;

	t = resolve_formal_arg(proc, f);

	g95_current_ns = ns;

	if (t == FAILURE)
	    return FAILURE;
    }

    return SUCCESS;
}



/* find_arglists()-- Work function called when searching for symbols
 * that have argument lists associated with them. */

static void find_arglists(g95_symbol *sym) {
g95_namespace *save, *ns;

    if (sym->attr.use_assoc)
	return;

    else if (sym->attr.if_source == IFSRC_UNKNOWN || sym->ns != g95_current_ns)
	return;

    /* If this symbol is really contained, resolve the argument list
     * with respect to the contained unit. */

    save = g95_current_ns;

    for(ns=sym->ns->contained; ns; ns=ns->sibling)
	if (ns->proc_name == sym) {
	    g95_current_ns = ns;
	    goto done;
	}

    if (sym->formal_ns != NULL)
	g95_current_ns = sym->formal_ns;

    resolve_symbol(sym);

done:
    resolve_formal_arglist(sym);
    g95_current_ns = save;
}



/* resolve_formal_arglists()-- Given a namespace, resolve all formal
 * argument lists within the namespace. */

static void resolve_formal_arglists(g95_namespace *ns) {

    if (ns != NULL)
	g95_traverse_ns(ns, find_arglists);
}



/* resolve_contained_functions()-- Resolve contained function types.
 * Because contained functions can call one another, they have to be
 * worked out before any of the contained procedures can be resolved.
 * If a function doesn't already have a type, the only way it can get
 * one is through an IMPLICIT type or a RESULT variable. */

static void resolve_contained_functions(g95_namespace *ns) {
g95_symbol *sym_upper, *sym_lower, *result;
g95_namespace *child;
int m;

    resolve_formal_arglists(ns);

    for(child=ns->contained; child; child=child->sibling) {
	sym_lower = child->proc_name;
	if (sym_lower == NULL)
	    continue;

	g95_find_symbol(sym_lower->name, ns, 0, &sym_upper);

	if (sym_upper == NULL)
	    g95_internal_error("resolve_contained_functions(): "
			       "Module procedure not found");

	m = namespace_kind(child);
	if (m == COMP_SUBROUTINE)
	    sym_upper->ts.type = BT_PROCEDURE;

	if (m != COMP_FUNCTION)
	    continue;

	if (sym_lower->result->ts.type == BT_CHARACTER &&
	    sym_lower->result->ts.cl->length == NULL)
	    g95_error("Internal function '%s' at %L cannot be an "
		      "assumed-length CHARACTER",
		      sym_lower->name, &sym_lower->declared_at);

	if (sym_lower->result != NULL)
	    sym_lower = sym_lower->result;

	if (sym_lower->ts.type == BT_UNKNOWN) {
	    if (sym_lower->result == NULL)
		g95_set_default_type(sym_lower, 1, child);
	    else {
		result = sym_lower->result;

		if (result->ts.type == BT_UNKNOWN)
		    g95_set_default_type(result, 1, child);

		sym_lower->ts = result->ts;
	    }
	}

	if (sym_upper != sym_lower) {
	    sym_upper->ts = sym_lower->ts;
	    sym_upper->as = g95_copy_array_spec(sym_lower->as);

	    sym_upper->attr.pointer = sym_lower->attr.pointer;
	    sym_upper->attr.dimension = sym_lower->attr.dimension;
	}
    }
}



/* resolve_branch()-- Given a branch to a label, see if the branch is
 * conforming.  The code node describes where the branch is
 * located. */

static try resolve_branch(g95_st_label *label, g95_code *code) {
g95_code *block, *found;
code_stack *stack;
g95_st_label *lp;
char *format;

    if (label == NULL)
	return SUCCESS;

    lp = label;

    /* Is this a valid branching target? */

    if (lp->defined == ST_LABEL_UNKNOWN) {
	g95_error("Label %d referenced at %L is never defined", lp->value,
		  &lp->where);
	lp->defined = ST_LABEL_BAD_TARGET2;
	return FAILURE;
    }

    if (lp->defined == ST_LABEL_BAD_TARGET2)
	return FAILURE;

    if (lp->defined != ST_LABEL_TARGET) {
	g95_error("Statement at %L is not a valid branch target statement "
		  "for the branch statement at %L", &lp->where, &code->where);
	return FAILURE;
    }

    if (code == NULL || code->type == EXEC_LABEL_ASSIGN)
	return SUCCESS;

    /* Try to find the label in the parse tree. To do this, we traverse
     * the tree block-by-block: first the block that contains this GOTO,
     * then the block that it is nested in, etc.  We can ignore other
     * blocks because branching into another block is not allowed. */

    found = NULL;

    for(stack=cs_base; stack; stack=stack->prev) {
	for(block=stack->head; block; block=block->next) {
	    if (block->here == label) {
		found = block;
		break;
	    }
	}

	if (found)
	    break;
    }

    if (found == NULL) {  /* still nothing, so illegal.  */
	format = "Label at %L is not in the same block as the GOTO "
	    "statement at %L";

	if (g95_option.fmode == 0)
	    g95_warning_now(104, format, &lp->where, &code->where);

	else {
	    g95_error_now(format, &lp->where, &code->where);
	    return FAILURE;
	}
    }

    /* Make sure that the branching target is legal if the statement is
     * an END {SELECT,IF}. */

    if (found != NULL && found->type == EXEC_NOP &&
	found->ext.end_code != ST_ENDDO) {

	for(stack=cs_base; stack; stack=stack->prev)
	    if (stack->current->next == found)
		break;

	if (stack == NULL) {
	    if (found->ext.end_code == ST_END_SELECT) {
		g95_error("GOTO at %L cannot jump to END of construct at %L",
			  &code->where, &found->where);
		return FAILURE;

	    } else {
		if (g95_option.fmode != 0 && found->ext.end_code == ST_ENDIF)
		    g95_warning(152, "GOTO at %L cannot jump to END of "
				"construct at %L", &code->where,
				&found->where);
		return SUCCESS;
	    }
	}
    }

    return SUCCESS;
} 



/* resolve_actual_arglist()-- Resolve an actual argument list.  Most
 * of the time, this is just resolving the expressions in the list.
 * The exception is that we sometimes have to decide whether an 
 * argument refers to a dummy procedure or a simple variable. */

static try resolve_actual_arglist(g95_actual_arglist *arg) {

    for(; arg; arg=arg->next) {
	if (arg->type == ARG_ALT_RETURN) {
	    if (resolve_branch(arg->u.label, NULL) == FAILURE)
		return FAILURE;

	    continue;
	}

	if (arg->u.expr == NULL)
	    continue;

	if (resolve_actual_argument(arg->u.expr) == FAILURE)
	    return FAILURE;

	if (arg->pointer && arg->u.expr->type == EXPR_VARIABLE)
	    arg->u.expr->symbol->attr.targetted = 1;

	if (arg->type == ARG_EXPR && arg->u.expr->rank > 0 &&
	    arg->u.expr->type != EXPR_NULL)
	    arg->type = ARG_ARRAY;

	if (arg->cb == CB_VALUE && arg->u.expr->rank != 0) {
	    g95_error("Can't pass array argument by value at %L",
		      &arg->u.expr->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* mark_external()-- It is possible for a symbol to be marked as
 * EXTERNAL or INTRINSIC in a module subprogram, but not be explicitly
 * defined.  This subroutine explicitly marks such procedures and
 * makes sure that they are being used correctly across module
 * procedures. */

static try mark_external(g95_symbol *sym, int function_flag) {

    if ((function_flag && sym->attr.subroutine) ||
	(!function_flag && sym->attr.function)) {
	g95_error("Symbol '%s' at %L is used as both a FUNCTION and "
		  "a SUBROUTINE", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.function || sym->attr.subroutine)
	return SUCCESS;

    /* Set the procedure type at this point */

    return function_flag ?
	g95_add_function(&sym->attr, sym->name, &sym->declared_at) :
	g95_add_subroutine(&sym->attr, sym->name, &sym->declared_at);
}


/* mark_interface_used()-- Mark all of the symbol in a generic
 * interface as used. */

static void mark_interface_used(g95_interface *p) {

    for(; p; p=p->next)
	p->sym->attr.used = 1;
}



/* function_type()-- Set the expression type from the function type. */

static try function_type(g95_expr *expr, g95_symbol *sym) {
g95_typespec *ts;

    if (expr->ts.type != BT_UNKNOWN)
	return SUCCESS;

    if (sym->result->ts.type != BT_UNKNOWN)
	expr->ts = sym->result->ts;

    else {
	ts = g95_get_default_type(sym->result, sym->ns);

	if (ts->type == BT_UNKNOWN) {
	    g95_error("Function '%s' at %L has no implicit type",
		      sym->name, &expr->where);
	    return FAILURE;
	}

	expr->ts = *ts;
    }

    expr->rank = (sym->result->as == NULL)
	? 0
	: sym->result->as->rank;

    if (expr->ts.type == BT_CHARACTER && expr->ts.cl->length != NULL &&
	expr->ts.cl->length->type != EXPR_CONSTANT)
	expr->ts.cl = &g95_unknown_charlen;

    return SUCCESS;
}



/* resolve_procvar_f()-- Resolve a procedure pointer reference */

static try resolve_procvar_f(g95_expr *e) {
g95_intrinsic_sym *isym;
g95_symbol *sym;

    sym = e->value.function.pointer->ts.interface;

    if (sym == NULL) {
	sym = e->value.function.pointer->symbol;

	e->ts = *g95_get_default_type(sym, sym->ns);
	e->rank = 0;

	if (e->ts.type == BT_UNKNOWN) {
	    g95_error("Procedure pointer '%s' at %L has no implicit type",
		      sym->name, &e->where);
	    return FAILURE;
	}

    } else {
	sym = find_declaration(sym);
	e->value.function.pointer->ts.interface = sym;

	if (sym->attr.subroutine) {
	    g95_error("Procedure pointer '%s' at %L is a SUBROUTINE pointer",
		      sym->name, &e->where);
	    return FAILURE;
	}

	if (!sym->attr.intrinsic) {
	    e->ts   = sym->ts;
	    e->rank = (sym->as == NULL)
		? 0
		: sym->as->rank;

	} else {
	    isym = g95_find_function(sym->name);

	    if (isym != NULL) {
		e->ts = isym->ts;
		e->rank = 0;
	    }
	}
    }

    g95_procedure_use(sym, &e->value.function.actual, &e->where);

    return SUCCESS;
}



/* resolve_generic_f0()-- Resolve a function call known to be generic.
 * Section 14.1.2.4.1. */

static match resolve_generic_f0(g95_expr *expr, g95_namespace *ns) {
char *name, *save;
g95_symtree *st;
g95_symbol *s;
match m;

    name = expr->value.function.name;

    st = g95_find_generic(name, ns);
    if (st == NULL)
	goto try_intrinsic;

    s = g95_search_interface(st->n.generic, 0, &expr->value.function.actual);
    if (s == NULL)
	goto try_intrinsic;

    mark_interface_used(st->n.generic);

    if (s->attr.proc == PROC_UNKNOWN)
	s->attr.proc = PROC_EXTERNAL;

    expr->symbol = s;
    function_type(expr, s);

    return MATCH_YES;

try_intrinsic:
    if (g95_find_symbol(name, ns, 0, &s) || s == NULL || !s->attr.intrinsic)
	return MATCH_NO;

    save = expr->value.function.name;
    expr->value.function.name = s->name;

    m = g95_intrinsic_func_interface(expr, 0);
    if (m != MATCH_YES)
	expr->value.function.name = save;

    return m;
}



static try resolve_generic_f(g95_expr *expr) {
g95_namespace *ns;
match m;

    for(ns=g95_current_ns; ns; ns=ns->parent) {
	m = resolve_generic_f0(expr, ns);
	if (m == MATCH_YES)   return SUCCESS;
	if (m == MATCH_ERROR) return FAILURE;
    }

    /* Last ditch attempt */

    if (!g95_generic_intrinsic(expr->value.function.name, 0)) {
	g95_error("Generic function '%s' at %L is not consistent with a "
		  "specific intrinsic interface",
		  expr->value.function.name, &expr->where);
	return FAILURE;
    }

    m = g95_intrinsic_func_interface(expr, 0);
    if (m == MATCH_YES)
	return SUCCESS;

    if (m == MATCH_NO)
	g95_error("Generic function '%s' at %L is not consistent with a "
		  "specific intrinsic interface",
		  expr->value.function.name, &expr->where);

    return FAILURE;
}



/* resolve_unknown_f()-- Resolve a procedure call not known to be
 * generic nor specific */

static try resolve_unknown_f(g95_expr *expr) {
g95_symbol *sym;

    g95_find_symbol(expr->value.function.name, NULL, 1, &sym);

    if (sym != NULL && sym->attr.dummy) {
	sym->attr.proc = PROC_DUMMY;
	expr->value.function.iname = sym->name;
	expr->symbol = sym;
	goto set_type;
    }

    /* See if we have an intrinsic function reference */

    if (!sym->attr.dummy_proc &&
	g95_intrinsic_name(expr->value.function.name, 0))
	switch(g95_intrinsic_func_interface(expr, 1)) {
	case MATCH_YES:
	    return SUCCESS;

	case MATCH_NO:
	    break;

	case MATCH_ERROR:
	    return FAILURE;
	}

    /* The reference is to an external name */

    sym->attr.proc = PROC_EXTERNAL;
    sym->result = sym;

    expr->symbol = sym;

set_type:
    g95_procedure_use(sym, &expr->value.function.actual, &expr->where);

    /* Type of the expression is either the type of the symbol or the
     * default type of the symbol */
  
    return function_type(expr, sym);
}



/* resolve_specific_f0()-- Resolve a function call known to be specific */

static match resolve_specific_f0(g95_symbol *sym, g95_expr *expr) {
char *name;
match m;

    if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY) {
	if (sym->attr.dummy) {
	    sym->attr.proc = PROC_DUMMY;
	    goto found;
	}

	sym->attr.proc = PROC_EXTERNAL;
	goto found;
    }

    if (sym->attr.iproc != IPROC_NONE) {
	if (g95_check_intrinsic_modfunc(sym, expr) == SUCCESS)
	    goto found;

	return MATCH_ERROR;
    }

    if (sym->attr.proc == PROC_MODULE || sym->attr.proc == PROC_INTERNAL ||
	sym->attr.proc == PROC_ST_FUNCTION ||
	sym->attr.if_source == IFSRC_DECL)
	goto found;

    if (sym->attr.intrinsic) {
	name = expr->value.function.name;
	expr->value.function.name = sym->name;

	m = g95_intrinsic_func_interface(expr, 1);
	if (m == MATCH_YES)
	    return MATCH_YES;

	expr->value.function.name = name;

	if (m == MATCH_NO)
	    g95_error("Function '%s' at %L is INTRINSIC but is not "
		      "compatible with an intrinsic", sym->name, &expr->where);

	return MATCH_ERROR;
    }

    return MATCH_NO;

found:
    g95_procedure_use(sym, &expr->value.function.actual, &expr->where);
    sym->attr.invoked = 1;

    if (sym->result == NULL)
	sym->result = sym;

    if (function_type(expr, sym) == FAILURE)
	return MATCH_ERROR;

    if (mark_external(sym, 1) == FAILURE)
	return MATCH_ERROR;

    expr->symbol = sym;

    return MATCH_YES;
}



/* resolve_specific_f()-- Resolve a function known to be specific */

static try resolve_specific_f(g95_expr *expr) {
g95_namespace *ns;
g95_symbol *sym;
match m;

    for(ns=g95_current_ns; ns; ns=ns->parent) {
	g95_find_symbol(expr->value.function.name, ns, 1, &sym);

	if (sym == NULL)
	    break;

	m = resolve_specific_f0(sym, expr);
	if (m == MATCH_YES)   return SUCCESS;
	if (m == MATCH_ERROR) return FAILURE;
    }

    g95_error("Unable to resolve the specific function '%s' at %L",
	      expr->value.function.name, &expr->where);

    return SUCCESS;
}



/* check_dot_product()-- Make sure that a dot product looks OK. */

static try check_dot_product(g95_expr *f) {
g95_expr *v1, *v2;
bignum s1, s2;
try t;

    v1 = f->value.function.actual->u.expr;
    v2 = f->value.function.actual->next->u.expr;

    s1 = g95_array_size(v1);
    s2 = g95_array_size(v2);

    t = SUCCESS;

    if (s1 != NULL && s2 != NULL &&
	bi_compare(big_copy(s1), big_copy(s2)) != 0)
	g95_error("Size difference in arguments of DOT_PRODUCT at %L (%s/%s)",
		  &v1->where,
		  bi_to_string(big_copy(s1)), bi_to_string(big_copy(s2)));

    if (s1 != NULL)
	big_free(s1);

    if (s2 != NULL)
	big_free(s2);

    return t;
}



/* pure_st_function()-- Returns nonzero if the statement function is
 * PURE-- ie it only calls PURE functions. */

static int pure_st_function(g95_expr *e, char **name, st_frame *base) {
g95_actual_arglist *a;
g95_ref *ref;
int i;

    if (e == NULL)
	return 1;

    switch(e->type) {
    case EXPR_OP:
	return pure_st_function(e->value.op.op1, name, base) &&
  	       pure_st_function(e->value.op.op2, name, base);

    case EXPR_VARIABLE:
	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (!pure_st_function(ref->u.ar.start[i],  name, base) ||
			!pure_st_function(ref->u.ar.end[i],    name, base) ||
			!pure_st_function(ref->u.ar.stride[i], name, base))
			return 0;

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    if (!pure_st_function(ref->u.car.element[i], name, base))
			return 0;

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		return pure_st_function(ref->u.ss.start, name, base) &&
		    pure_st_function(ref->u.ss.end,   name, base);
	    }

	break;

    case EXPR_FUNCTION:
	for(a=e->value.function.actual; a; a=a->next) {
	    *name = e->value.function.name;
	    if (!pure_st_function(a->u.expr, name, base))
		return 0;
	}

	return pure_function(e, name, base);

    default:
	break;
    }

    return 1;
}



/* statement_function()-- Return nonzero if the function reference is
 * to a statement function. */

static int statement_function(g95_expr *e) {

    return (e->value.function.isym != NULL)
	? 0
	: e->symbol->attr.proc == PROC_ST_FUNCTION;
}



/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to. */

static try resolve_function(g95_expr *expr) {
g95_actual_arglist *arg;
g95_symbol *sym;
g95_gsymbol *g; 
char *name;
try t;

    if (resolve_actual_arglist(expr->value.function.actual) == FAILURE)
	return FAILURE;

/* See if function is already resolved */

    if (expr->value.function.isym != NULL || expr->symbol != NULL)
	t = SUCCESS;

    else if (expr->value.function.pointer != NULL)
	t = resolve_procvar_f(expr);

    else        /* Apply the rules of section 14.1.2.4 */
	switch(procedure_kind(expr->value.function.name)) {
	case PTYPE_GENERIC:
	    t = resolve_generic_f(expr);
	    break;

	case PTYPE_SPECIFIC:
	    t = resolve_specific_f(expr);
	    break;

	case PTYPE_UNKNOWN:
	    t = resolve_unknown_f(expr);
	    break;

	default:
	    g95_internal_error("resolve_function(): bad function type");
	}

    /* If the expression is still a function (it might have
     * simplified), then we check to see if we are calling an
     * elemental function */

    if (expr->type != EXPR_FUNCTION || t == FAILURE)
	return t;

    sym = expr->symbol;

    if (sym != NULL) {
	sym->attr.used = 1;
	sym->attr.invoked = 1;
    }

    if (expr->value.function.iname == NULL && sym != NULL &&
	sym->attr.proc != PROC_INTERNAL && sym->attr.proc != PROC_MODULE &&
	sym->attr.proc != PROC_ST_FUNCTION &&
	!sym->attr.dummy && sym->ts.type != BT_PROCEDURE) {

	g = g95_check_global(sym->name, GSYM_FUNCTION, &expr->where);
	if (g != NULL)
	    g95_add_global_ref(g, sym, expr->value.function.actual,
			       &expr->where);
    }

    if (expr->value.function.isym != NULL &&
	expr->value.function.isym->id == G95_ISYM_DOT_PRODUCT &&
	check_dot_product(expr) == FAILURE)
	return FAILURE;

    if (expr->value.function.actual != NULL &&
	((expr->symbol != NULL && expr->symbol->attr.elemental) ||
	 (expr->value.function.isym != NULL &&
	  expr->value.function.isym->elemental))) {

	/* The rank of an elemental is the rank of its array argument(s) */

	for(arg=expr->value.function.actual; arg; arg=arg->next) {
	    if (arg->type != ARG_ALT_RETURN && arg->u.expr != NULL &&
		arg->u.expr->rank > 0) {
		expr->rank = arg->u.expr->rank;
		expr->shape = arg->u.expr->shape;
		break;
	    }
	}
    }

    name = NULL;

    if (!pure_function(expr, &name, NULL)) {
	if (g95_forall_flag) {
	    g95_error("Function reference to non-PURE '%s' at %L is inside a "
		      "FORALL block", name, &expr->where);
	    t = FAILURE;

	} else if (forall_mask) {
	    g95_error("Function reference to '%s' in FORALL mask at %L is "
		      "to a non-PURE procedure", name, &expr->where);
	    t = FAILURE;

	} else if (g95_pure(NULL, 1)) {
	    g95_error("Function reference to '%s' at %L is to a non-PURE "
		      "procedure within a PURE procedure", name, &expr->where);
	    t = FAILURE;
	}
    }

    if (sym != NULL) {
	if (sym->attr.abstract) {
	    g95_error("Cannot call ABSTRACT function '%s' at %L",
		      name, &expr->where);
	    return FAILURE;
	}

	if (sym == sym->ns->proc_name && !sym->attr.recursive) {
	    g95_error("Function '%s' must be RECURSIVE to reference itself "
		      "at %L", name, &expr->where);
	    return FAILURE;
	}

	if (sym->attr.pointer &&
	    (sym->attr.if_source == IFSRC_USAGE ||
	     sym->attr.if_source == IFSRC_UNKNOWN)) {
	    g95_error("POINTER function '%s' at %L must have an explicit "
		      "interface", name, &expr->where);
	    return FAILURE;
	}

	if (sym->attr.allocatable &&
	    (sym->attr.if_source == IFSRC_USAGE ||
	     sym->attr.if_source == IFSRC_UNKNOWN)) {
	    g95_error("ALLOCATABLE function '%s' at %L must have an explicit "
		      "interface", name, &expr->where);
	    return FAILURE;
	}
    }

    if (expr->ts.type == BT_CHARACTER && expr->value.function.isym == NULL &&
	expr->ts.cl != &g95_unknown_charlen && expr->ts.cl->length == NULL) {
	g95_error("CHARACTER function '%s' at %L cannot have an assumed "
		  "length", expr->symbol->name, &expr->where);
	t = FAILURE;
    }

    if (expr->ts.type == BT_DERIVED)
	expr->ts.derived->attr.used = 1;

    return t;
}



/* pure_function()-- Figure out if a function reference is pure or
 * not.  Also sets the name of the function for a potential error
 * message.  Returns nonzero if the function is PURE, zero if not. */

static int pure_function(g95_expr *e, char **name, st_frame *base) {
st_frame frame, *f;
static int seen=0;
g95_symbol *sym;
internal_proc p;
int pure;

    if (e->value.function.pointer != NULL) {
	sym = e->value.function.pointer->ts.interface;
	return (sym == NULL) ? 0 : sym->attr.pure;
    }

    if (e->value.function.isym == NULL && e->symbol == NULL) {
	resolve_function(e);

	if (e->type != EXPR_FUNCTION)
	    return 1;
    }

    if (e->value.function.isym != NULL) {
	pure = e->value.function.isym->pure ||
	       e->value.function.isym->elemental;
	*name = e->value.function.isym->name;

    } else if (statement_function(e)) {
	for(f=base; f; f=f->prev)
	    if (f->sym == e->symbol) {
		if (!seen)
		    g95_error("Statement function '%s' at %L is recursive",
			      e->symbol->name, &e->symbol->declared_at);
		seen = 1;
		return 0;
	    }

	frame.sym  = e->symbol;
	frame.prev = base;

	pure = pure_st_function(e->symbol->value, name, &frame);

    } else if (e->symbol->attr.iproc != IPROC_NONE) {
	p = e->symbol->attr.iproc;

	pure  = g95_iproc_purity(p);
	*name = g95_iproc_name(p);

    } else {
	pure  = g95_pure(e->symbol, 1);
	*name = e->symbol->name;
    }

    return pure;
}



static void pure_subroutine(g95_code *c, g95_symbol *sym) {

    if (g95_pure(sym, 0))
	return;

    if (g95_forall_flag)
	g95_error("Subroutine call to '%s' in FORALL block at %L is not PURE",
		  c->sym->name, &c->where);

    else if (g95_pure(NULL, 1))
	g95_error("Subroutine call to '%s' at %L is not PURE", c->sym->name,
		  &c->where);
}



static match resolve_generic_s0(g95_code *c, g95_namespace *ns) {
char *name, *save;
g95_symtree *st;
g95_symbol *s;
match m;

    name = c->ext.sub.name;

    st = g95_find_generic(name, ns); 
    if (st == NULL)
	goto try_intrinsic;

    s = g95_search_interface(st->n.generic, 1, &c->ext.sub.actual);
    if (s == NULL)
	goto try_intrinsic;

    mark_interface_used(st->n.generic);

    if (s->attr.proc == PROC_UNKNOWN)
	s->attr.proc = PROC_EXTERNAL;

    s->attr.invoked = 1;
    c->sym = s;
    pure_subroutine(c, s);
    return MATCH_YES;

    /* TODO: Need to search for elemental references in generic interface */

try_intrinsic:
    if (g95_find_symbol(name, ns, 0, &s) || s == NULL || !s->attr.intrinsic)
	return MATCH_NO;

    save = c->ext.sub.name;
    c->ext.sub.name = s->name;

    m = g95_intrinsic_sub_interface(c, 0);
    if (m != MATCH_YES)
	c->ext.sub.name = save;

    return m;
}



static try resolve_generic_s(g95_code *c) {
g95_namespace *ns;
char *name;
match m;

    for(ns=g95_current_ns; ns; ns=ns->parent) {
	m = resolve_generic_s0(c, ns);
	if (m == MATCH_YES)   return SUCCESS;
	if (m == MATCH_ERROR) return FAILURE;
    }

    /* Last ditch attempt */

    name = c->ext.sub.name;
    if (!g95_generic_intrinsic(name, 1)) {
	g95_error("Generic subroutine '%s' at %L is not consistent with a "
		  "specific subroutine interface", name, &c->where);
	return FAILURE;
    }

    m = g95_intrinsic_sub_interface(c, 0);
    if (m == MATCH_YES)
	return SUCCESS;

    if (m == MATCH_NO)
	g95_error("Generic subroutine '%s' at %L is not consistent with a "
		  "specific subroutine interface", name, &c->where);

    return FAILURE;
}



/* resolve_specific_s0()-- Resolve a subroutine call known to be specific */

static match resolve_specific_s0(g95_code *c, g95_symbol *sym) {
match m;

    if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY) {
	if (sym->attr.dummy) {
	    sym->attr.proc = PROC_DUMMY;
	    goto found;
	}

	sym->attr.proc = PROC_EXTERNAL;
	goto found;
    }

    if (sym->attr.iproc != IPROC_NONE)
	return (g95_check_intrinsic_modsub(sym, c) == SUCCESS)
	    ? MATCH_YES
	    : MATCH_ERROR;

    if (sym->attr.proc == PROC_MODULE || sym->attr.proc == PROC_INTERNAL ||
	sym->attr.if_source == IFSRC_DECL)
	goto found;

    if (sym->attr.intrinsic) {
	m = g95_intrinsic_sub_interface(c, 1);
	if (m == MATCH_NO)
	    g95_error("Subroutine '%s' at %L is INTRINSIC but is not "
		      "compatible with an intrinsic", sym->name, &c->where);
	return m;
    }

    return MATCH_NO;

found:
    g95_procedure_use(sym, &c->ext.sub.actual, &c->where);
    sym->attr.invoked = 1;

    mark_external(sym, 0);

    c->sym = sym;
    pure_subroutine(c, sym);

    return MATCH_YES;
}



static try resolve_specific_s(g95_code *c) {
g95_namespace *ns;
g95_symbol *sym;
match m;

    sym = c->sym;

    for(ns=g95_current_ns; ns; ns=ns->parent) {
	g95_find_symbol(c->ext.sub.name, ns, 0, &sym);
	if (sym == NULL)
	    continue;

	m = resolve_specific_s0(c, sym);
	if (m == MATCH_YES) return SUCCESS;
	if (m == MATCH_ERROR) return FAILURE;
    }

    g95_error("Unable to resolve the specific subroutine '%s' at %L",
	      c->ext.sub.name, &c->where);

    return FAILURE;
}



/* resolve_unknown_s()-- Resolve a subroutine call not known to be
 * generic or specific */

static try resolve_unknown_s(g95_code *c) {
g95_symbol *sym;

    g95_find_symbol(c->ext.sub.name, NULL, 1, &sym);

    if (sym->attr.dummy)
	sym->attr.proc = PROC_DUMMY;

    else {
	/* See if we have an intrinsic function reference */

	if (g95_intrinsic_name(c->ext.sub.name, 1) && 
	    g95_intrinsic_sub_interface(c, 0) == MATCH_YES)
	    return SUCCESS;

	/* The reference is to an external name */

	if (sym->attr.proc == PROC_UNKNOWN)
	    sym->attr.proc = PROC_EXTERNAL;
    }

    g95_procedure_use(sym, &c->ext.sub.actual, &c->where);
    c->sym = sym;

    pure_subroutine(c, sym);
    return SUCCESS;
}



/* resolve_procvar_s()-- Resolve a procedure pointer reference. */

static try resolve_procvar_s(g95_code *c) {
g95_symbol *sym;

    sym = c->ext.sub.pointer->ts.interface;

    if (sym != NULL && sym->attr.function) {
	g95_error("Procedure pointer '%s' at L is a FUNCTION pointer",
		  sym->name, &c->where);
	return FAILURE;
    }

    if (sym != NULL) {
	sym = find_declaration(sym);
	c->ext.sub.pointer->ts.interface = sym;
    }

    g95_procedure_use(sym, &c->ext.sub.actual, &c->where);
    return SUCCESS;
}



/* resolve_call()-- Resolve a subroutine call.  Although it was
 * tempting to use the same code for functions, subroutines and
 * functions are stored differently and that makes things awkward. */

static try resolve_call(g95_code *c) {
g95_symbol *sym, *s;
g95_gsymbol *g;
bt type;
try t;

    if (resolve_actual_arglist(c->ext.sub.actual) == FAILURE)
	return FAILURE;

    if (c->ext.sub.pointer != NULL)
	return resolve_procvar_s(c);

    if (c->sym != NULL)
	return SUCCESS;

    /* See if we can host-associate the subroutine name as a pointer
     * variable. */

    g95_find_symbol(c->ext.sub.name, NULL, 0, &sym);

    if (sym->attr.flavor == FL_UNKNOWN) {	
	if (!g95_local_symbol(sym)) {
	    g95_find_symbol(c->ext.sub.name, g95_current_ns->parent, 1, &s);

	    if (s != NULL && s->attr.flavor == FL_VARIABLE &&
		s->ts.type == BT_PROCEDURE) {

		c->sym = s;
		return SUCCESS;
	    }
	}

	/* The symbol must be a subroutine */
	g95_add_subroutine(&sym->attr, c->ext.sub.name, &c->where);
    }

    if (sym->attr.dummy) {
	c->sym = sym;
	g95_procedure_use(sym, &c->ext.sub.actual, &c->where);
	return SUCCESS;
    }

    if (sym->attr.flavor != FL_PROCEDURE) {
	g95_error("Can't call symbol '%s' at %L", c->ext.sub.name, &c->where);
	return FAILURE;
    }

    switch(procedure_kind(c->ext.sub.name)) {
    case PTYPE_GENERIC:
	t = resolve_generic_s(c);
	break;

    case PTYPE_SPECIFIC:
	t = resolve_specific_s(c);
	break;

    case PTYPE_UNKNOWN:
	t = resolve_unknown_s(c);
	break;

    default:
	g95_internal_error("resolve_call(): bad function type");
    }

    sym = c->sym;

    if (sym != NULL) {
	if (sym->attr.abstract) {
	    g95_error("Cannot call ABSTRACT subroutine '%s' at %L",
		      sym->name, &c->where);
	    t = FAILURE;
	}

	if (!sym->attr.recursive) {
	    if (sym == g95_current_ns->proc_name) {
		g95_error("SUBROUTINE '%s' at %L must be RECURSIVE in order "
			  "to call itself", sym->name, &c->where);
		t = FAILURE;

	    } else if (sym->attr.entry && c->sym->attr.proc != PROC_MODULE) {
		g95_error("ENTRY '%s' at %L must be RECURSIVE in order to "
			  "call itself", sym->name, &c->where);
		t = FAILURE;
	    }
	}

	sym->attr.used = 1;

	type = sym->ts.type;
	if (type != BT_UNKNOWN && type != BT_PROCEDURE) {
	    g95_error("SUBROUTINE '%s' at %L cannot have %s type",
		      sym->name, &c->where, g95_basic_typename(type));
	    t = FAILURE;
	}

	if (sym->attr.proc != PROC_INTERNAL && sym->attr.proc != PROC_MODULE &&
	    !sym->attr.dummy && !sym->attr.pointer) {
	    g = g95_check_global(sym->name, GSYM_SUBROUTINE, &c->where);
	    if (g != NULL)
		g95_add_global_ref(g, sym, c->ext.sub.actual, &c->where);
	}
    }

    return t;
}



/* recursive_derived_type()-- Return nonzero if the type is recursive. */

static int recursive_derived_type(g95_symbol *derived, g95_component *c) {

    for(; c; c=c->next) {
	if (c->ts.type != BT_DERIVED || c->pointer)
	    continue;

	if (derived == c->ts.derived ||
	    recursive_derived_type(derived, c->ts.derived->components))
	    return 1;
    }

    return 0;
}


/* resolve_derived()-- Resolve a derived type symbol */

static try resolve_derived(g95_symbol *sym) {
g95_component *c;
g95_expr *e;
int rank;

    if (!sym->attr.set) {
	g95_error("Derived type '%s' at %L never defined",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (recursive_derived_type(sym, sym->components)) {
	g95_error("Derived type '%s' at %L is recursive directly or "
		  "indirectly", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.bind && sym->component_access == ACCESS_PRIVATE) {
	g95_error("BIND(C) attribute of derived type '%s' at %L conflicts "
		  "with PRIVATE components", sym->name, &sym->declared_at);
	return MATCH_ERROR;
    }

    for(c=sym->components; c; c=c->next) {

/* If the derived type has the SEQUENCE attribute, then all derived
 * type components must also have SEQUENCE. */

	if (sym->attr.sequence && c->ts.type == BT_DERIVED &&
	    !c->ts.derived->attr.sequence) {
	    g95_error("Component '%s' of SEQUENCE type '%s' declared at %L "
		      "does not have the SEQUENCE attribute",
		      c->name, sym->name, &c->where);
	    return FAILURE;
	}

	if (c->allocatable && c->as == NULL && c->cas == NULL) {
	    g95_error("ALLOCATABLE component '%s' at %L must have an "
		      "array specification", c->name, &c->where);
	    return FAILURE;
	}

	if (c->allocatable && c->cas != NULL && c->cas->type == CAS_ASSUMED) {
	    g95_error("ALLOCATABLE component '%s' at %L cannot have an "
		      "assumed-size coarray specification", c->name, &c->where);
	    return FAILURE;
	}

	if (c->initializer != NULL) {
	    if (c->allocatable) {
		g95_error("Component '%s' of type '%s' declared at %L cannot "
			  "have a default initialization",
			  c->name, sym->name, &c->where);
		return FAILURE;
	    }

	    if (c->initializer->type == EXPR_NULL &&
		c->initializer->rank == -1)

		c->initializer->rank = (c->as == NULL)
		    ? 0
		    : c->as->rank;

	    rank = c->initializer->rank;

	    if ((c->as == NULL && rank > 0) ||
		(c->as != NULL && c->as->rank != rank && rank > 0)) {
		g95_error("Incompatible ranks in default component at %L",
			  &c->initializer->where);
		return FAILURE;
	    }

	    e = c->initializer;
	    if (e->type == EXPR_ARRAY)
		e = e->value.constructor.c->expr;

	    if (g95_convert_type(e, &c->ts, 0) == FAILURE)
		return FAILURE;
	}

	if ((c->as != NULL && g95_resolve_array_spec(c->as) == FAILURE) ||
	    !g95_constant_array_spec(c->as, 1))
	    return FAILURE;

	if (c->ts.type == BT_CHARACTER) {
	    if (resolve_charlen(c->ts.cl) == FAILURE)
		return FAILURE;

	    if (c->ts.cl->length == NULL ||
		c->ts.cl->length->type != EXPR_CONSTANT) {
		g95_error("CHARACTER Component '%s' of type '%s' at %L must "
			  "have constant length",
			  c->name, sym->name, &c->where);
		return FAILURE;
	    }
	}

	/* Constraint in f95 4.4.1, apparently removed in F2003.
	 * Richard Maine says that there is an interp to the effect
	 * that entity in one module that is rendered private in
	 * another is still public. */

	if (G95_STRICT_F95() && c->ts.type == BT_DERIVED &&
	    g95_symbol_access(c->ts.derived) == ACCESS_PRIVATE &&
	    !c->ts.derived->attr.use_assoc &&
	    g95_symbol_access(sym) != ACCESS_PRIVATE &&
	    sym->component_access != ACCESS_PRIVATE) {
	    g95_error("Component '%s' of type '%s' at %L cannot be visible",
		      c->name, sym->name, &c->where);
	    return FAILURE;
	}

	if (sym->attr.bind) {
	    if (c->allocatable) {
		g95_error("ALLOCATABLE component '%s' of type '%s' at %L "
			  "conflicts with BIND(C) attribute",
			  c->name, sym->name, &c->where);
		return FAILURE;
	    }

	    if (c->pointer) {
		g95_error("POINTER component '%s' of type '%s' at %L "
			  "conflicts with BIND(C) attribute",
			  c->name, sym->name, &c->where);
		return FAILURE;
	    }
	}

	if (c->cas != NULL && !c->allocatable) {
	    g95_error("Coarray component '%s' of type '%s' at %L must be "
		      "ALLOCATABLE", c->name, sym->name, &c->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* check_public()-- Resolve symbols that appear in a PUBLIC
 * statement in F mode. */

static try check_public(g95_symbol *sym) {
g95_interface *p;
g95_symtree *st;
int extends;

    if (!G95_STRICT_F() || sym->ns->state != COMP_MODULE)
	return SUCCESS;

    st = g95_find_generic(sym->name, sym->ns);
    if (st == NULL) {

	if (sym->attr.access != ACCESS_PUBLIC || !sym->attr.use_assoc)
	    return SUCCESS;

	switch(sym->attr.flavor) {
	case FL_DERIVED:
	    g95_error("Derived type '%s' at %L cannot appear in a PUBLIC "
		      "statement in F mode", sym->name, &sym->declared_at);
	    return FAILURE;

	case FL_PARAMETER:
	    g95_error("PARAMETER '%s' at %L cannot appear in a PUBLIC "
		      "statement in F mode", sym->name, &sym->declared_at);
	    return FAILURE;

	case FL_VARIABLE:
	    g95_error("Variable '%s' at %L cannot appear in a PUBLIC "
		      "statement in F mode", sym->name, &sym->declared_at);
	    return FAILURE;

	case FL_PROCEDURE:
	    g95_error("PROCEDURE '%s' at %L cannot appear in a PUBLIC "
		      "statement in F mode", sym->name, &sym->declared_at);
	    return FAILURE;

	default:
	    break;
	}

    } else {
	if (st->access != ACCESS_PUBLIC)
	    return SUCCESS;

	extends = 0;

	for(p=st->n.generic; p; p=p->next)
	    extends |= !p->sym->attr.use_assoc;

	if (!extends) {
	    g95_error("Generic symbol '%s' at %L must extend an interface "
		      "to appear in a PUBLIC statement in F mode",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* automatic_object()-- Determine if a variable is an automatic
 * object.  These are objects which have nonconstant specification
 * expressions.  Returns nonzero if the object is an automatic
 * object. */

static int automatic_object(g95_symbol *sym) {
g95_array_spec *as;
int i;

    if (sym->attr.flavor != FL_VARIABLE || sym->attr.dummy)
	return 0;

    if (sym->ts.type == BT_CHARACTER &&
	(sym->ts.cl->length == NULL ||
	 sym->ts.cl->length->type != EXPR_CONSTANT))
	return 1;

    as = sym->as;
    if (as != NULL && as->type == AS_EXPLICIT)
	for(i=0; i<as->rank; i++)
	    if (as->lower[i]->type != EXPR_CONSTANT ||
		as->upper[i]->type != EXPR_CONSTANT)
		return 1;

    return 0;
}



/* resolve_namelist()-- Resolve a namelist */

static try resolve_namelist(g95_symbol *sym) {
g95_symbol *s, *t;
g95_namelist *n;
int i;

    for(n=sym->namelist; n; n=n->next) {
	s = n->sym;

	if (!g95_local_symbol(s) && s->ns->parent != NULL) {
	    if (g95_find_symbol(s->name, s->ns->parent, 1, &t))
		return FAILURE;

	    if (t != NULL) {
		g95_error("Symbol '%s' at %L conflicts with namelist at %L",
			  s->name, &t->declared_at, &s->declared_at);
		return FAILURE;
	    }
	}

	if (g95_symbol_access(sym) == ACCESS_PUBLIC) {
	    if (g95_symbol_access(s) == ACCESS_PRIVATE) {
		g95_error("Variable '%s' at %L cannot be PRIVATE in a PUBLIC "
			  "namelist", s->name, &n->where);
		return FAILURE;
	    }

	    if (s->ts.type == BT_DERIVED &&
		g95_derived_private(s->ts.derived)) {
		g95_error("Namelist variable '%s' at %L cannot have PRIVATE "
			  "components in a PUBLIC namelist",
			  s->name, &n->where);
		return FAILURE;
	    }
	}

	if (s->ts.type == BT_CHARACTER &&
	    (resolve_charlen(s->ts.cl) == FAILURE ||
	     s->ts.cl->length == NULL ||
	     !g95_is_constant_expr(s->ts.cl->length))) {
	    g95_error("Variable '%s' at %L cannot have a variable length "
		      "and be in a NAMELIST", s->name, &n->where);
	    return FAILURE;
	}

	if (s->as != NULL) {
	    if (s->as->type != AS_EXPLICIT) {
		g95_error("Variable '%s' at %L must have an explicit shape "
			  "to be in a NAMELIST", s->name, &n->where);
		return FAILURE;
	    }

	    for(i=0; i<s->as->rank; i++)
		if (!g95_is_constant_expr(s->as->lower[i]) ||
		    !g95_is_constant_expr(s->as->upper[i])) {
		    g95_error("Variable '%s' at %L must have constant bounds "
			      "to be in a NAMELIST", s->name, &n->where);
		    return FAILURE;
		}
	}

	if (s->ts.type != BT_DERIVED)
	    continue;

	if (g95_pointer_component(s->ts.derived)) {
	    g95_error("Namelist variable '%s' at %L cannot have a POINTER "
		      "component", s->name, &n->where);
	    return FAILURE;
	}

	if (g95_allocatable_component(s->ts.derived)) {
	    g95_error("Namelist variable '%s' at %L cannot have an "
		      "ALLOCATABLE component", s->name, &n->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* check_public_interface()-- Check a PUBLIC generic interface and
 * make sure it has no specifics that have private derived types as
 * arguments or return values.  This was a constraint in f95, removed
 * in F2003. */

static try check_public_interface(g95_symtree *st) {
g95_formal_arglist *f;
g95_interface *p;
g95_symbol *sym;

    if (!G95_STRICT_F95())
	return SUCCESS;

    for(p=st->n.generic; p; p=p->next) {
	sym = p->sym;
	if (sym->attr.use_assoc)
	    continue;

	/* 5.2.3 in f95 */

	if (G95_STRICT_F95() && sym->attr.function &&
	    sym->result->ts.type == BT_DERIVED &&
	    g95_symbol_access(sym->result->ts.derived) == ACCESS_PRIVATE &&
	    !sym->result->ts.derived->attr.use_assoc) {
	    g95_error("Function '%s' at %L cannot return a PRIVATE type from "
		      "PUBLIC generic '%s'", sym->name, &sym->declared_at,
		      st->name);
	    return FAILURE;
	}

	for(f=sym->formal; f; f=f->next)
	    if (G95_STRICT_F95() && f->sym != NULL &&
		f->sym->ts.type == BT_DERIVED &&
		!f->sym->ts.derived->attr.use_assoc &&
		g95_symbol_access(f->sym->ts.derived) == ACCESS_PRIVATE) {
		g95_error("PUBLIC procedure '%s' at %L cannot have PRIVATE "
			  "derived type argument '%s' at %L",
			  sym->name, &sym->declared_at, f->sym->name,
			  &f->where);
		return FAILURE;
	    }
    }

    return SUCCESS;
}



/* check_generic()-- Make sure an interface list is all functions or
 * subroutines. */

static try check_generic(g95_symtree *st) {
g95_access access;
g95_interface *p;
int sub;

    if (st == NULL)
	return SUCCESS;

    if (check_generic(st->left) == FAILURE ||
	check_generic(st->right) == FAILURE)
	return FAILURE;

    p = st->n.generic;
    if (p == NULL)
	return SUCCESS;

    access = g95_symtree_access(st);

    if (access == ACCESS_PUBLIC && check_public_interface(st) == FAILURE)
	return FAILURE;

    if (access != ACCESS_PRIVATE)
	p->sym->attr.invoked = 1;

    sub = p->sym->attr.subroutine;
    p = p->next;

    for(; p; p=p->next) {
	if (access != ACCESS_PRIVATE)
	    p->sym->attr.invoked = 1;

	if (!p->sym->attr.function && !p->sym->attr.subroutine &&
	    p->sym->attr.intrinsic) {

	    if (sub && g95_find_function(p->sym->name) != NULL)
		break;

	    if (!sub && g95_find_subroutine(p->sym->name) != NULL)
		break;

	    continue;
	}

	if (sub != p->sym->attr.subroutine)
	    break;
    }

    if (p != NULL) {
	g95_error("Interfaces of generic '%s' at %L must all be FUNCTIONs "
		  "or SUBROUTINEs", st->name, &p->sym->declared_at);
	return FAILURE;
    }

    return SUCCESS;
}



/* check_st_expr()-- Check the things on the right side of a statement
 * functions.  Returns FAILURE if something went wrong. */

static try check_st_expr(g95_expr *e) {
g95_intrinsic_sym *isym;
g95_actual_arglist *a;
g95_ref *ref;
int i;

    if (e == NULL)
	return SUCCESS;

    switch(e->type) {
    case EXPR_FUNCTION:
	isym = e->value.function.isym;
	if (isym != NULL && g95_is_transformational(isym)) {
	    g95_error("Transformational function '%s' not allowed in "
		      "statement function at %L", isym->name, &e->where);
	    return FAILURE;
	}

	if (e->rank > 0) {
	    g95_error("Array valued function not allowed in statement "
		      "function at %L", &e->where);
	    return FAILURE;
	}

	for(a=e->value.function.actual; a; a=a->next)
	    if (check_st_expr(a->u.expr) == FAILURE)
		return FAILURE;

	break;

    case EXPR_OP:
	if (check_st_expr(e->value.op.op1) == FAILURE ||
	    check_st_expr(e->value.op.op2) == FAILURE)
	    return FAILURE;

	break;

    case EXPR_VARIABLE:
    case EXPR_SUBSTRING:
	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++)
		    if (check_st_expr(ref->u.ar.start[i])  == FAILURE ||
			check_st_expr(ref->u.ar.end[i])    == FAILURE ||
			check_st_expr(ref->u.ar.stride[i]) == FAILURE)
			return FAILURE;

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    if (check_st_expr(ref->u.car.element[i]) == FAILURE)
			return FAILURE;

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		if (check_st_expr(ref->u.ss.start) == FAILURE ||
		    check_st_expr(ref->u.ss.end)   == FAILURE)
		    return FAILURE;
	    }

	break;

    default:
	break;
    }

    return SUCCESS;
}



/* resolve_st_function()-- Resolve a symbol that is a statement
 * function. */

static try resolve_st_function(g95_symbol *sym) {

    if (g95_resolve_expr(sym->value) == FAILURE)
	return FAILURE;

    if (check_st_expr(sym->value) == FAILURE)
	return FAILURE;

    if (sym->as != NULL) {
	g95_error("Statement function '%s' at %L cannot be array-valued",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->ts.type == BT_CHARACTER && sym->ts.cl->length == NULL) {
	g95_error("Statement function '%s' at %L cannot be assumed-length "
		  "CHARACTER", sym->name, &sym->declared_at);
	return FAILURE;
    }

    return SUCCESS;
}



/* resolve_coarray_spec()-- Resolve a coarray specification. */

static try resolve_coarray_spec(g95_expr *e, int dummy) {

    if (e == NULL)
	return SUCCESS;

    if (g95_resolve_expr(e) == FAILURE)
	return FAILURE;

    if (e->ts.type != BT_INTEGER) {
	g95_error("Coarray specification at %L must be of type INTEGER",
		  &e->where);
	return FAILURE;
    }

    if (e->rank > 0) {
	g95_error("Coarray specification at %L must be scalar", &e->where);
	return FAILURE;
    }

    if (!dummy && e->type != EXPR_CONSTANT) {
	g95_error("Coarray specification at %L must be constant", &e->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* resolve_coarray()-- Resolve a coarray. */

static try resolve_coarray(g95_symbol *sym) {
int i, r, dummy;

    if (sym->cas->type == CAS_DEFERRED && !sym->attr.allocatable) {
	g95_error("Deferred shape coarray '%s' at %L must be allocatable",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.in_common) {
	g95_error("Coarray '%s' at %L cannot appear in a COMMON block",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.equivalenced) {
	g95_error("Coarray '%s' at %L cannot appear in an EQUIVALENCE",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.flavor == FL_PARAMETER) {
	g95_error("Coarray '%s' at %L cannot be a PARAMETER",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (!sym->attr.dummy && !g95_module_symbol(sym) &&
	!sym->attr.allocatable && !sym->attr.save && !sym->ns->save_all &&
	sym->ns->proc_name->attr.flavor != FL_PROGRAM) {
	g95_error("Coarray '%s' at %L cannot be an automatic variable",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.pointer) {
	g95_error("Coarray '%s' at %L cannot be a POINTER",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    r = sym->cas->corank;
    dummy = sym->attr.dummy;

    for(i=0; i<r; i++)
	if (resolve_coarray_spec(sym->cas->lower[i], dummy) == FAILURE ||
	    resolve_coarray_spec(sym->cas->upper[i], dummy) == FAILURE)
	    return FAILURE;

    return SUCCESS;
}



/* resolve_symbol()-- Do anything necessary to resolve a symbol.
 * Right now, we just assume that an otherwise unknown symbol is a
 * variable.  This sort of thing commonly happens for symbols in
 * module. */

static try resolve_symbol(g95_symbol *sym) {
static int formal_ns_flag = 1; /* Zero if we are checking a formal namespace */
int mp_flag, formal_ns_save, m;
g95_namespace *ns;
g95_symbol *s;
g95_expr *e;
try t;

    if (sym->attr.resolved || sym->attr.current)
	return SUCCESS;

    sym->attr.resolved = 1;
    if (sym->attr.use_assoc)
	return check_public(sym);

    if (sym->attr.flavor == FL_UNKNOWN) {
	if (sym->attr.external == 0 && sym->attr.intrinsic == 0 &&
	    g95_find_generic(sym->name, sym->ns) == NULL)
	    t = g95_add_flavor(&sym->attr, FL_VARIABLE, sym->name,
			       &sym->declared_at);
	else {
	    t = g95_add_flavor(&sym->attr, FL_PROCEDURE, sym->name,
			       &sym->declared_at);

	    if (sym->attr.dimension)
		sym->attr.function = 1;
	}

	if (t == FAILURE)
	    return FAILURE;
    }

    if (sym->attr.flavor == FL_NAMELIST)
	return resolve_namelist(sym);

    if (sym->attr.flavor == FL_DERIVED && resolve_derived(sym) == FAILURE)
	return FAILURE;

    /* Symbols that are module procedures with results (functions)
     * have the types and array specification copied for type checking
     * in procedures that call them, as well as for saving to a module
     * file.  These symbols can't stand the scrutiny that their
     * results can. */

    mp_flag = sym->result != NULL && sym->result != sym;

    /* Assign default type to symbols that need one and don't have one */

    if (sym->ts.type == BT_UNKNOWN) {
	if (sym->attr.flavor == FL_VARIABLE ||
	    sym->attr.flavor == FL_PARAMETER)
	    g95_set_default_type(sym, 1, NULL);

	if (sym->attr.flavor == FL_PROCEDURE) {
	    if (!sym->attr.function && !sym->attr.subroutine)
		g95_set_default_type(sym, 0, NULL);
	    /* Don't check return code */

	    else if (sym->attr.function) {
		if (!mp_flag)
		    g95_set_default_type(sym, 0, sym->formal_ns);

		else {
		    ns = g95_current_ns;
		    g95_current_ns = sym->result->ns;

		    t = resolve_symbol(sym->result);

		    g95_current_ns = ns;
		    if (t == FAILURE)
			return FAILURE;
		    /* Result may be in another namespace */

		    sym->ts = sym->result->ts;
		    sym->as = g95_copy_array_spec(sym->result->as);
		    sym->attr.pointer = sym->result->attr.pointer;
		    sym->attr.dimension = sym->result->attr.dimension;
		}
	    }
	}
    }

    if (sym->attr.proc == PROC_ST_FUNCTION &&
	resolve_st_function(sym) == FAILURE)
	return FAILURE;

    if (sym->as != NULL && (sym->as->type == AS_ASSUMED_SIZE || 
			    sym->as->type == AS_ASSUMED_SHAPE ) &&
	sym->attr.dummy == 0) {
	g95_error("Assumed %s array at %L must be a dummy argument",
		  sym->as->type == AS_ASSUMED_SIZE ? "size" : "shape",
		  &sym->declared_at);
	return FAILURE;
    }

    /* Mark variables with initialization expressions as having been set */

    if (sym->attr.flavor == FL_VARIABLE && sym->value != NULL)
	g95_set_usage(sym, NULL, 1, 0);

    /* Make sure that character string variables with assumed lengths
     * are dummy arguments or results. */

    if (sym->attr.flavor == FL_VARIABLE && sym->ts.type == BT_CHARACTER &&
	sym->ts.cl->length == NULL && !sym->attr.dummy &&
	!sym->attr.result_var) {
	g95_error("Assumed character length variable '%s' at %L must be a "
		  "dummy variable or result", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if ((sym->attr.flavor == FL_VARIABLE || sym->attr.function) &&
	sym->ts.type == BT_CHARACTER) {
	ns = g95_current_ns;
	if (sym->formal_ns != NULL)
	    g95_current_ns = sym->formal_ns;

	t = resolve_charlen(sym->ts.cl);
	g95_current_ns = ns;

	if (t == FAILURE)
	    return FAILURE;
    }

    if (sym->attr.flavor == FL_PROCEDURE && sym->attr.function &&
	sym->ts.type == BT_CHARACTER && sym->ts.cl->length != NULL &&
	sym->ts.cl->length->type != EXPR_CONSTANT &&
	sym->attr.if_source != IFSRC_IFBODY &&
	sym->attr.if_source != IFSRC_DECL) {
	g95_error("Nonconstant character function '%s' at %L must have an "
		  "explicit interface", sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Make sure a parameter that has been implicitly typed still
     * matches the implicit type, since PARAMETER statements can
     * precede IMPLICIT statements. */

    if (sym->attr.flavor == FL_PARAMETER &&
	g95_check_parameter(sym) == FAILURE)
	return FAILURE;

    /* Make sure the types of derived parameters are consistent.  This
     * type checking is deferred until resolution because the type may
     * refer to a derived type from the host. */

    if (sym->attr.flavor == FL_PARAMETER && sym->ts.type == BT_DERIVED &&
	!g95_compare_types(&sym->ts, &sym->value->ts)) {
	g95_error("Incompatible derived type in PARAMETER at %L",
		  &sym->value->where);
	return FAILURE;
    }

    if (sym->attr.flavor == FL_PARAMETER &&
	g95_simplify_mode != SIMP_REGULAR &&
	!g95_compare_types(&sym->ts, &sym->value->ts) &&
	g95_convert_type(sym->value, &sym->ts, 0) == FAILURE)
	return FAILURE;

    if (sym->attr.flavor == FL_PARAMETER && sym->as != NULL &&
	sym->as->type == AS_DEFERRED) {
	g95_error("Array PARAMETER '%s' at %L cannot have a deferred array "
		  "specification", sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Make sure symbols with known intent or optional are really
     * dummy variable.  Because of ENTRY statement, this has to be
     * deferred until resolution time. */

    if ((sym->attr.optional || sym->attr.intent != INTENT_UNKNOWN) &&
	sym->attr.dummy == 0) {

	g95_error("Symbol at %L is not a DUMMY variable", &sym->declared_at);
	return FAILURE;
    }

    if ((sym->attr.flavor == FL_VARIABLE ||
	 sym->attr.flavor == FL_PROCEDURE) &&
	sym->attr.access != ACCESS_UNKNOWN &&
	namespace_kind(sym->ns) != COMP_MODULE) {
	g95_error("Symbol '%s' at %L cannot have an ACCESS specification",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (G95_STRICT_F95() && sym->attr.flavor == FL_VARIABLE &&
	sym->ts.type == BT_DERIVED &&
	g95_symbol_access(sym) == ACCESS_PUBLIC &&
	g95_symbol_access(sym->ts.derived) == ACCESS_PRIVATE) {
	g95_error("Variable '%s' at %L is a PUBLIC variable of a PRIVATE type",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Constraints on deferred shape variables. */

    if (sym->attr.flavor == FL_VARIABLE || 
	(sym->attr.flavor == FL_PROCEDURE && sym->attr.function)) {
	if (sym->as == NULL ||
	    (sym->as->type != AS_ASSUMED_SHAPE &&
	     sym->as->type != AS_DEFERRED)) {

	    if (sym->attr.allocatable && sym->as != NULL) {
		g95_error("Allocatable array at %L must have a deferred shape",
			  &sym->declared_at);
		return FAILURE;
	    }

	    if (sym->attr.pointer && sym->attr.dimension) {
		g95_error("Pointer to array at %L must have a deferred shape",
			  &sym->declared_at);
		return FAILURE;
	    }

	} else if (!mp_flag && !sym->attr.allocatable && !sym->attr.pointer &&
		   !sym->attr.dummy) {
	    g95_error("Array at %L cannot have a deferred shape",
		      &sym->declared_at);
	    return FAILURE;
	}
    }

    if (sym->attr.flavor == FL_VARIABLE && sym->attr.pointer &&
	sym->as != NULL && sym->as->type != AS_DEFERRED) {
	g95_error("POINTER array '%s' at %L must be declared with a deferred "
		  "array specification", sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Make sure that an intrinsic exists */

    if (sym->attr.intrinsic && !g95_intrinsic_name(sym->name, 0)
	&& !g95_intrinsic_name(sym->name, 1)) {
	g95_error("Intrinsic '%s' at %L does not exist",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Resolve array specification.  If the array lives in static
     * memory, then the specification must be constant. */

    if (sym->as != NULL) {
	ns = g95_current_ns;
	if (sym->formal_ns != NULL)
	    g95_current_ns = sym->formal_ns;

	t = g95_resolve_array_spec(sym->as);
	if (t == SUCCESS && !sym->attr.pointer && automatic_object(sym)) {

	    if (sym->attr.save) {
		g95_error("Automatic variable '%s' at %L cannot have the SAVE "
			  "attribute", sym->name, &sym->declared_at);
		t = FAILURE;
	    }

	    if (t == SUCCESS && sym->value != NULL) {
		g95_error("Automatic variable '%s' at %L cannot have an "
			  "initial value", sym->name, &sym->declared_at);
		t = FAILURE;
	    }

	    if (t == SUCCESS && sym->attr.data) {
		g95_error("Automatic variable '%s' at %L cannot appear in "
			  "a DATA statement", sym->name, &sym->declared_at);
		t = FAILURE;
	    }

	    if (t == SUCCESS && sym->attr.equivalenced) {
		g95_error("Automatic variable '%s' at %L cannot appear in an "
			  "EQUIVALENCE statement",
			  sym->name, &sym->declared_at);
		t = FAILURE;
	    }

	    if (t == SUCCESS && sym->attr.in_common) {
		g95_error("Automatic variable '%s' at %L cannot appear in an "
			  "COMMON block", sym->name, &sym->declared_at);
		t = FAILURE;
	    }

	    if (t == SUCCESS && g95_module_symbol(sym)) {
		g95_error("Automatic variable '%s' at %L cannot appear in "
			  "MODULE specification statements",
			  sym->name, &sym->declared_at);
		t = FAILURE;
	    }
	}

	g95_current_ns = ns;
	if (t == FAILURE)
	    return FAILURE;
    }

    if (sym->attr.flavor == FL_VARIABLE && sym->ts.type == BT_CHARACTER &&
	g95_static_symbol(sym) && !sym->attr.dummy &&
	(sym->ts.cl->length == NULL ||
	 sym->ts.cl->length->type != EXPR_CONSTANT)) {
	g95_error("Character variable '%s' at %L must have a constant length",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.value) {
	if (!sym->attr.dummy) {
	    g95_error("VALUE attribute on variable '%s' at %L must be "
		      "applied to a dummy variable",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}

	if (sym->ts.type == BT_CHARACTER &&
	    (sym->ts.cl->length == NULL ||
	     sym->ts.cl->length->type != EXPR_CONSTANT)) {
	    g95_error("Character dummy variable '%s' at %L with the VALUE "
		      "attribute must have a constant length",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}
    }

    if (!g95_option.multiple_save && sym->attr.save && sym->ns->save_all) {
	g95_error("Symbol '%s' at %L has the SAVE attribute in a program unit "
		  "with the SAVE attribute", sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* C1107 */

    if (sym->attr.flavor == FL_VARIABLE && sym->ts.type == BT_DERIVED &&
	sym->ns->state == COMP_MODULE && !sym->attr.save &&
	!sym->ns->save_all && g95_initialized_component(sym->ts.derived) &&
	sym->value == NULL && !sym->attr.pointer && !sym->attr.allocatable) {
	g95_error("Module variable '%s' at %L with a component initialization "
		  "must have the SAVE attribute",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Weird case */

    if ((sym->attr.flavor == FL_VARIABLE ||
	 sym->attr.flavor == FL_PARAMETER) &&
	sym->ts.type == BT_DERIVED && sym->ns->parent != NULL &&
	!sym->attr.dummy &&
	g95_find_symbol(sym->name, sym->ns->parent, 1, &s) == 0 &&
	s != NULL && s->attr.flavor == FL_DERIVED && sym->ts.derived == s) {
	g95_error("Symbol '%s' at %L cannot be a variable and a "
		  "host-associated derived type",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Resolve formal namespaces. */

    if (formal_ns_flag && sym != NULL && sym->formal_ns != NULL) {
	formal_ns_save = formal_ns_flag;
	formal_ns_flag = 0;
	if (g95_resolve(sym->formal_ns) == FAILURE)
	    return FAILURE;

	formal_ns_flag = formal_ns_save;
    }

    if (sym->ts.type == BT_CHARACTER && sym->attr.flavor == FL_PARAMETER) {
	e = sym->ts.cl->length;
	g95_simplify_init_expr(e);

	if (e->type != EXPR_CONSTANT &&
	    (e->type != EXPR_VARIABLE ||
	     e->symbol->attr.flavor != FL_PARAMETER)) {
	    g95_error("Character parameter '%s' at %L must have a constant "
		      "length", sym->name, &sym->declared_at);

	    return FAILURE;
	}
    }

    if (sym->attr.flavor == FL_VARIABLE && g95_pure(NULL, 1) &&
	sym->ns == g95_current_ns && sym->ns->save_all) {
	g95_error("Variable '%s' at %L cannot be SAVEd in a PURE procedure",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* C535 */

    if (sym->attr.protected && sym->attr.in_common &&
	sym->ts.type != BT_PROCEDURE) {
	g95_error("Protected variable '%s' at %L cannot be in a COMMON block",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (check_public(sym) == FAILURE)
	return FAILURE;

    if (G95_STRICT_F() && sym->ns->state == COMP_MODULE &&
	sym->ns->default_access == ACCESS_UNKNOWN && !sym->attr.artificial &&
	sym->attr.access == ACCESS_UNKNOWN)

	switch(sym->attr.flavor) {
	case FL_VARIABLE:
	    g95_error("Variable '%s' at %L must have an access specification "
		      "in F mode", sym->name, &sym->declared_at);
	    return FAILURE;

	case FL_PARAMETER:
	    g95_error("PARAMETER '%s' at %L must have an access specification "
		      "in F mode", sym->name, &sym->declared_at);
	    return FAILURE;

	case FL_DERIVED:
	    g95_error("Derived type '%s' at %L must have an access "
		      "specification in F mode", sym->name, &sym->declared_at);
	    return FAILURE;

	default:
	    break;
	}

    if (sym->attr.bind && sym->attr.flavor == FL_VARIABLE) {
	m = (sym->ts.type == BT_DERIVED &&
	     sym->ts.derived->attr.itype == ITYPE_C_FUNPTR)
	    ? CBIND_PROCEDURE
	    : CBIND_VARIABLE;

	if (g95_add_cbinding(sym->bind, m, &sym->declared_at))
	    return FAILURE;
    }

    if (sym->cas != NULL && resolve_coarray(sym) == FAILURE)
	return FAILURE;

    if (sym->attr.flavor == FL_VARIABLE && !sym->attr.in_common &&
	!sym->attr.equivalenced && !sym->attr.st_construct &&
	!sym->attr.st_construct0 && sym->ns->state == COMP_BLOCK_DATA)
	g95_warning(151, "Variable '%s' at %L in BLOCK DATA unit is not in a "
		    "COMMON block", sym->name, &sym->declared_at);

    /* Make sure procedure pointer interfaces are defined. */

    if (sym->attr.flavor == FL_VARIABLE && sym->ts.type == BT_PROCEDURE &&
	sym->attr.pointer && sym->ts.interface != NULL &&
	sym->ts.interface->attr.proc == PROC_UNKNOWN &&
	!sym->ts.interface->attr.abstract && !sym->attr.used) {

	g95_error("PROCEDURE interface '%s' at %L was never defined",
		  sym->ts.interface->name, &sym->ts.interface->declared_at);
	return FAILURE;
    }

    return SUCCESS;
}



/* resolve_symbols()-- Recursively resolve all symbol in a namespace.
 * The 'set' attribute is set directly, to avoid triggering errors in
 * g95_set_usage() on a PROTECTED symbol. */

static try resolve_symbols(g95_symtree *st) {
g95_symbol *sym;

    if (st == NULL)
	return SUCCESS;

    if (resolve_symbols(st->left) == FAILURE ||
	resolve_symbols(st->right) == FAILURE)
	return FAILURE;

    sym = st->n.sym;

    if (sym->attr.flavor == FL_VARIABLE && sym->ts.type == BT_DERIVED &&
	g95_initialized_component(sym->ts.derived))
	sym->attr.set = 1;

    return resolve_symbol(sym);
}



/* is_protected()-- Return nonzero if the symbol is protected and has
 * been use-associated. */

static int is_protected(g95_symbol *sym) {

    return sym->attr.protected && sym->attr.use_assoc;
}



/* binary_op_name()-- Return the name of a binary operator. */

static char *binary_op_name(g95_intrinsic_op op) {
char *name;

    switch(op) {
    case INTRINSIC_GT:      name = ".GT. operation";            break;
    case INTRINSIC_GE:      name = ".GE. operation";            break;
    case INTRINSIC_LT:      name = ".LT. operation";            break;
    case INTRINSIC_LE:      name = ".LE. operation";            break;
    case INTRINSIC_AND:     name = ".AND. operation";           break;
    case INTRINSIC_OR:      name = ".OR. operation";            break;
    case INTRINSIC_EQV:     name = ".EQV. operation";           break;
    case INTRINSIC_NEQV:    name = ".NEQV. operation";          break;
    case INTRINSIC_PLUS:    name = "ADDITION operation";        break;
    case INTRINSIC_MINUS:   name = "SUBTRACTION operation";     break;
    case INTRINSIC_TIMES:   name = "MULTIPLICATION operation";  break;
    case INTRINSIC_DIVIDE:  name = "DIVISION operation";        break;
    case INTRINSIC_POWER:   name = "POWER operation";           break;
    case INTRINSIC_CONCAT:  name = "CONCATENATION operation";   break;
    case INTRINSIC_EQ:      name = ".EQ. operation";            break;
    case INTRINSIC_NE:      name = ".NE. operation";            break;
    default:
	g95_internal_error("binary_op_name(): Bad operator");
	name = NULL;
    }

    return name;
}



/* resolve_operator()-- Resolve an operator expression node.  This can
 * involve replacing the operation with a user defined function call. */

static try resolve_operator(g95_expr *e) {
char *name, msg[200];
g95_expr *op1, *op2;
g95_locus *where;
try t;

/* Resolve all subnodes-- give them types. */

    switch(e->value.op.operator) {
    default:
	if (g95_resolve_expr(e->value.op.op2) == FAILURE)
	    return FAILURE;

/* Fall through */

    case INTRINSIC_NOT:      case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:   case INTRINSIC_PAREN:
	if (g95_resolve_expr(e->value.op.op1) == FAILURE)
	    return FAILURE;

	break;
    }

/* Typecheck the new node. */

    op1 = e->value.op.op1;
    op2 = e->value.op.op2;

    if (op1->type == EXPR_CONSTANT && op1->ts.type == BT_INTEGER &&
	op1->value.integer->typeless) {
	e->value.op.op1 = g95_assign_boz(&op1->ts, op1);
	g95_free_expr(op1);
	op1 = e->value.op.op1;
    }

    if (op2 != NULL && op2->type == EXPR_CONSTANT &&
	op2->ts.type == BT_INTEGER && op2->value.integer->typeless) {
	e->value.op.op2 = g95_assign_boz(&op2->ts, op2);
	g95_free_expr(op2);
	op2 = e->value.op.op2;
    }

    where = NULL;
    if (op1->type == EXPR_NULL)
	where = &op1->where;

    if (op2 != NULL && op2->type == EXPR_NULL)
	where = &op2->where;

    if (where != NULL) {
	g95_error("Cannot use the NULL() pointer as an intrinsic operand "
		  "at %L", where);
	return FAILURE;
    }

    switch(e->value.op.operator) {
    case INTRINSIC_PAREN:
	e->ts = op1->ts;
	break;

    case INTRINSIC_CONCAT:
	if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {
	    e->ts.type = BT_CHARACTER;
	    e->ts.kind = op1->ts.kind;
	    break;
	}

	sprintf(msg, "Operands of string concatenation operator at %%L "
		"are %s/%s", g95_typename(&op1->ts), g95_typename(&op2->ts));
	goto bad_op;

    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:
	if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op2->ts)) {
	    g95_type_convert_binary(e);
	    break;
	}

	sprintf(msg, "Operands of binary numeric operator '%s' at %%L "
		"are %s/%s", g95_op2string(e->value.op.operator),
		g95_typename(&op1->ts), g95_typename(&op2->ts));
	goto bad_op;

    case INTRINSIC_UPLUS:  case INTRINSIC_UMINUS:
	if ((op1->ts.type == BT_INTEGER) || (op1->ts.type == BT_REAL) ||
	    (op1->ts.type == BT_COMPLEX)) {
	    e->ts = op1->ts;
	    break;
	}

	sprintf(msg, "Operand of unary numeric operator '%s' at %%L is %s",
		g95_op2string(e->value.op.operator), g95_typename(&e->ts));
	goto bad_op;

    case INTRINSIC_EQV:    case INTRINSIC_NEQV:
    case INTRINSIC_AND:    case INTRINSIC_OR:
	if (op1->ts.type == BT_LOGICAL && op2->ts.type == BT_LOGICAL) {
	    e->ts.type = BT_LOGICAL;
	    e->ts.kind = g95_kind_max(op1, op2);
	    break;
	}

	sprintf(msg, "Operands of logical operator '%s' at %%L are %s/%s",
		g95_op2string(e->value.op.operator), g95_typename(&op1->ts),
		g95_typename(&op2->ts));

	goto bad_op;
      
    case INTRINSIC_NOT:
	if (op1->ts.type == BT_LOGICAL) {
	    e->ts.type = BT_LOGICAL;
	    e->ts.kind = op1->ts.kind;
	    break;
	}

	sprintf(msg, "Operand of .NOT. operator at %%L is %s",
		g95_typename(&op1->ts));
	goto bad_op;

    case INTRINSIC_GT: case INTRINSIC_GE:
    case INTRINSIC_LT: case INTRINSIC_LE:
	if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX) {
	    strcpy(msg, "COMPLEX quantities cannot be compared at %L");
	    goto bad_op;
	}

	/* Fall through */

    case INTRINSIC_EQ: case INTRINSIC_NE:
	if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER)
	    goto ok_op;

	if (g95_option.sloppy_char &&
	    (op1->ts.type == BT_CHARACTER || op2->ts.type == BT_CHARACTER))
	    goto ok_op;

	if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op2->ts)) {
	    g95_type_convert_binary(e);
	    goto ok_op;
	}

	if (g95_ieee_class_compare(e->value.op.operator) &&
	    g95_internal_derived(&op1->ts, ITYPE_IEEE_CLASS) &&
	    g95_internal_derived(&op2->ts, ITYPE_IEEE_CLASS))
	    goto ok_op;

	sprintf(msg, "Operands of comparison operator '%s' at %%L are %s/%s",
		g95_op2string(e->value.op.operator), g95_typename(&op1->ts),
		g95_typename(&op2->ts));

	goto bad_op;

    ok_op:
	e->ts.type = BT_LOGICAL;
	e->ts.kind = g95_default_logical_kind();
	break;

    case INTRINSIC_USER:
	if (op2 == NULL)
	    sprintf(msg, "Operand of user operator '%s' at %%L is %s",
		    e->value.op.uop->name, g95_typename(&op1->ts));
	else
	    sprintf(msg, "Operands of user operator '%s' at %%L are %s/%s",
		    e->value.op.uop->name, g95_typename(&op1->ts),
		    g95_typename(&op2->ts));

	goto bad_op;

    default:
	g95_internal_error("resolve_operator(): Bad intrinsic");
    }

/* Deal with arrayness of an operand through an operator */

    t = SUCCESS;

    switch(e->value.op.operator) {
    case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:
    case INTRINSIC_POWER:   case INTRINSIC_PLUS:    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:   case INTRINSIC_DIVIDE:  case INTRINSIC_CONCAT:
    case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:
    case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:
    case INTRINSIC_LE:
	if (op1->rank == 0 && op2->rank == 0)
	    e->rank = 0;

	if (op1->rank == 0 && op2->rank != 0) {
	    e->rank = op2->rank;

	    if (e->shape == NULL)
		e->shape = op2->shape;
	}

	if (op1->rank != 0 && op2->rank == 0) {
	    e->rank = op1->rank;

	    if (e->shape == NULL)
		e->shape = op1->shape;
	}

	if (op1->rank != 0 && op2->rank != 0) {
	    if (op1->rank == op2->rank) {
		e->rank = op1->rank;
		name = binary_op_name(e->value.op.operator);

		if (op1->shape != NULL && op2->shape != NULL &&
		    g95_check_conformance(name, op1, op2) == FAILURE)
		    t = FAILURE;

		if (e->shape == NULL)
		    e->shape = (op1->shape != NULL)
			? op1->shape
			: op2->shape;

	    } else {
		sprintf(msg, "Inconsistent ranks for operator at %%L");
		t = FAILURE;

		e->rank = 0;   /* Allow higher level expressions to work */
		goto bad_op;
	    }
	}

	break;

    case INTRINSIC_NOT:     case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:  case INTRINSIC_PAREN:   
	e->rank = op1->rank;
	if (e->shape == NULL)
	    e->shape = op1->shape;

	break;           /* Simply copy arrayness attribute */

    default:
	break;
    }

    if (t == SUCCESS)
	t = g95_simplify_expr(e);

    return t;

bad_op:
    if (g95_extend_expr(e) == SUCCESS)
	return SUCCESS;

    g95_error(msg, &e->where);
    return FAILURE;
}



/* check_dimension()-- Compare a single dimension of an array
 * reference to the array specification. */

static try check_dimension(int i, g95_array_ref *ar, g95_array_spec *as) {
bignum start, end, step, n;
int flag;

/* Given start, end and stride values, calculate the minimum and
 * maximum referenced indexes. */

    switch(ar->dimen_type[i]) {
    case DIMEN_VECTOR:
	break;

    case DIMEN_ELEMENT:
	if (g95_compare_expr(ar->start[i], as->lower[i]) == CMP_LT) goto bound;
	if (g95_compare_expr(ar->start[i], as->upper[i]) == CMP_GT) goto bound;
	break;

    case DIMEN_RANGE:
	if (g95_compare_expr_int(ar->stride[i], 0) == CMP_EQ) {
	    g95_error("Illegal stride of zero at %L", &ar->where[i]);
	    return FAILURE;
	}

	/* It is perfectly legal for all section bounds to exceed the
	 * range of the array, so our checks have to be more
	 * sophisticated. */

	if ((ar->start[i]  != NULL && ar->start[i]->type  != EXPR_CONSTANT) ||
	    (ar->end[i]    != NULL && ar->end[i]->type    != EXPR_CONSTANT) ||
	    (ar->stride[i] != NULL && ar->stride[i]->type != EXPR_CONSTANT) ||
	    as->lower[i] == NULL || as->lower[i]->type != EXPR_CONSTANT ||
	    as->upper[i] == NULL || as->upper[i]->type != EXPR_CONSTANT)
	    break;

	start = (ar->start[i] == NULL)
	    ? as->lower[i]->value.integer
	    : ar->start[i]->value.integer;

	end = (ar->end[i] == NULL)
	    ? as->upper[i]->value.integer
	    : ar->end[i]->value.integer;

	step = ar->stride[i] == NULL
	    ? bi_1
	    : ar->stride[i]->value.integer;

	n = bi_divide(bi_add(bi_subtract(end, start), step), step);
	if (bi_compare(n, bi_0) <= 0)   /* Zero sized section */
	    break;

	if (bi_compare(start, as->lower[i]->value.integer) < 0 ||
	    bi_compare(start, as->upper[i]->value.integer) > 0)
	    goto bound;   /* No bignums need freeing */

	/* Calculate final section value */

	n = bi_divide(bi_add(bi_subtract(end, start), step), step);
	n = bi_add(start, bi_multiply(step, bi_subtract(n, bi_1)));

	flag = bi_compare(big_copy(n), as->lower[i]->value.integer) < 0 ||
	       bi_compare(big_copy(n), as->upper[i]->value.integer) > 0;

	big_free(n);
	if (flag)
	    goto bound;

	break;

    default:
	g95_internal_error("check_dimension(): Bad array reference");
    }

    return SUCCESS;

bound:
    g95_warning(108, "Array reference at %L is out of bounds", &ar->where[i]);
    return SUCCESS;
}



/* compare_spec_to_ref()-- Compare an array reference with an
 * array specification. */

static try compare_spec_to_ref(g95_array_ref *ar, g95_array_spec *as,
			       g95_locus *where) {
int i;

    if (ar->type == AR_FULL)
	return SUCCESS;

    if (as->rank != ar->dimen) {
	g95_error("Rank mismatch in array reference at %L (%d/%d)",
		  where, ar->dimen, as->rank);
	return FAILURE;
    }

    for(i=0; i<as->rank; i++)
	if (check_dimension(i, ar, as) == FAILURE)
	    return FAILURE;

    return SUCCESS;
}



/* resolve_index()-- Resolve one part of an array index */

static try resolve_index(g95_expr *index, int check_scalar) {
g95_typespec ts;

    if (index == NULL)
	return SUCCESS;

    if (g95_resolve_expr(index) == FAILURE)
	return FAILURE;

    if (index->ts.type != BT_INTEGER) {
	g95_error("Array index at %L must be of INTEGER type", &index->where);
	return FAILURE;
    }

    if (check_scalar && index->rank != 0) {
	g95_error("Array index at %L must be scalar", &index->where);
	return FAILURE;
    }

    if (!data_flag && index->ts.kind != g95_pointer_integer_kind()) {
	g95_clear_ts(&ts);
	ts.type = BT_INTEGER;
	ts.kind = g95_pointer_integer_kind();

	g95_convert_type(index, &ts, 1);
    }

    return SUCCESS;
}



/* resolve_coarray_ref()-- Resolve a coarray reference. */

static try resolve_coarray_ref(g95_coarray_ref *ref, g95_coarray_spec *spec,
			       g95_locus *where) {
int i;

    if (spec->corank != ref->dimen) {
	g95_error("Corank reference mismatch (%d/%d) at %L",
		  spec->corank, ref->dimen, where);
	return FAILURE;
    }

    for(i=0; i<spec->corank; i++) {
	if (g95_resolve_expr(ref->element[i]) == FAILURE)
	    return FAILURE;

	if (spec->type == CAS_DEFERRED)
	    continue;

	if (g95_compare_expr(ref->element[i], spec->lower[i]) == CMP_LT ||
	    (spec->upper[i] != NULL &&
	     g95_compare_expr(ref->element[i], spec->upper[i]) == CMP_GT)) {

	    g95_error("Coarray reference out of bounds at %L",
		      &ref->element[i]->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* g95_resolve_array_ref()-- Resolve an array reference */

try g95_resolve_array_ref(g95_array_ref *ar, g95_array_spec *as,
			  g95_locus *where) {
int i, check_scalar;

    for(i=0; i<ar->dimen; i++) {
	check_scalar = ar->dimen_type[i] == DIMEN_RANGE;

	if (resolve_index(ar->start[i],  check_scalar) == FAILURE)
	    return FAILURE;

	if (resolve_index(ar->end[i],    check_scalar) == FAILURE)
	    return FAILURE;

	if (resolve_index(ar->stride[i], check_scalar) == FAILURE)
	    return FAILURE;

	if (ar->dimen_type[i] == DIMEN_UNKNOWN)
	    switch(ar->start[i]->rank) {
	    case 0:
		ar->dimen_type[i] = DIMEN_ELEMENT;
		break;

	    case 1:
		ar->dimen_type[i] = DIMEN_VECTOR;
		break;

	    default:
		g95_error("Array index at %L is an array of rank %d",
			  &ar->where[i], ar->start[i]->rank);
		return FAILURE;
	    }
    }

    /* If the reference type is unknown, figure out what kind it is */

    if (ar->type == AR_UNKNOWN) {
	ar->type = AR_ELEMENT;
	for(i=0; i<ar->dimen; i++)
	    if (ar->dimen_type[i] == DIMEN_RANGE ||
		ar->dimen_type[i] == DIMEN_VECTOR) {
		ar->type = AR_SECTION;
		break;
	    }
    }

    if (compare_spec_to_ref(ar, as, where) == FAILURE)
	return FAILURE;

    for(i=0; i<ar->dimen; i++)
	if (ar->dimen_type[i] == DIMEN_VECTOR && ar->start[i]->rank > 1) {
	    g95_error("Vector subscript at %L must be of rank one",
		      &ar->start[i]->where);
	    return FAILURE;
	}

    return SUCCESS;
}



/* resolve_substring()-- Resolve a substring reference */

static try resolve_substring(g95_ref *ref) {
comparison cmp;

    if (ref->u.ss.start != NULL) {
	if (g95_resolve_expr(ref->u.ss.start) == FAILURE)
	    return FAILURE;

	if (ref->u.ss.start->ts.type != BT_INTEGER) {
	    g95_error("Substring start index at %L must be of type INTEGER",
		      &ref->u.ss.start->where);
	    return FAILURE;
	}

	if (ref->u.ss.start->rank != 0) {
	    g95_error("Substring start index at %L must be scalar",
		      &ref->u.ss.start->where);
	    return FAILURE;
	}
    }

    if (ref->u.ss.end != NULL) {
	if (g95_resolve_expr(ref->u.ss.end) == FAILURE)
	    return FAILURE;

	if (ref->u.ss.end->ts.type != BT_INTEGER) {
	    g95_error("Substring end index at %L must be of type INTEGER",
		      &ref->u.ss.end->where);
	    return FAILURE;
	}

	if (ref->u.ss.end->rank != 0) {
	    g95_error("Substring end index at %L must be scalar",
		      &ref->u.ss.end->where);
	    return FAILURE;
	}

	if (ref->u.ss.length != NULL &&
	    g95_compare_expr(ref->u.ss.end,
			     ref->u.ss.length->length) == CMP_GT) {
	    g95_error("Substring end index at %L is out of bounds",
		      &ref->u.ss.end->where);
	    return FAILURE;
	}
    }

    if (ref->u.ss.start != NULL && ref->u.ss.end != NULL) {
	cmp = g95_compare_expr(ref->u.ss.start, ref->u.ss.end);

	if (g95_compare_expr_int(ref->u.ss.start, 1) == CMP_LT &&
	    (cmp == CMP_EQ || cmp == CMP_LT)) {
	    g95_error("Substring start index at %L is less than one",
		      &ref->u.ss.start->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* explicit_interface()-- Return nonzero if a procedure has an
 * explicit interface. */

static int explicit_interface(g95_symbol *sym) {

    return sym->attr.if_source == IFSRC_IFBODY ||
   	g95_current_ns->proc_name == sym ||
	sym->attr.proc == PROC_MODULE ||
	sym->attr.proc == PROC_INTERNAL;
}



/* convert_substring()-- Convert an (incorrect) array reference into a
 * substring reference and resolve the node. */

static try convert_substring(g95_ref *ref, g95_charlen *cl) {
g95_expr *start, *end;

    start = ref->u.ar.start[0];
    end   = ref->u.ar.end[0];

    if (ref->u.ar.dimen > 1 || ref->u.ar.dimen_type[0] != DIMEN_RANGE ||
	ref->u.ar.stride[0] != NULL) {
	g95_error("Syntax error in substring reference at %L",
		  &ref->where);
	return FAILURE;
    }

    ref->type = REF_SUBSTRING;
    ref->u.ss.start = start;
    ref->u.ss.end = end;
    ref->u.ss.length = cl;

    return resolve_substring(ref);
}



/* find_component()-- Given a derived type node and a component name,
 * try to locate the component structure.  Returns the NULL pointer if
 * the component is not found or the components are private.  The flag
 * variable is nonzero if the parent variable has been use-associated,
 * which means the check for private components should be bypassed. */

static g95_component *find_component(g95_symbol *sym, g95_ref *ref, int flag) {
g95_component *p;

    for(p=sym->components; p; p=p->next)
	if (strcmp(p->name, ref->u.c.name) == 0)
	    break;

    if (p == NULL)
	g95_error("Element '%s' at %L is not a member of the '%s' structure",
		  ref->u.c.name, &ref->where, sym->name);

    else if (sym->attr.use_assoc && sym->component_access == ACCESS_PRIVATE &&
	     !flag) {
	g95_error("Component '%s' at %L is a PRIVATE component of '%s'",
		  ref->u.c.name, &ref->where, sym->name);
	p = NULL;
    }

    return p;
}


/* insert_full_section()-- Insert a full section reference node in the
 * middle of a reference list just before the 'ref' node. */

static void insert_full_section(g95_expr *e, g95_ref *ref, int rank) {
g95_ref *r, *new;
int i;

    new = g95_get_ref();

    new->type = REF_ARRAY;
    new->u.ar.type = AR_SECTION;
    new->u.ar.dimen = rank;
    new->next = ref;
    new->where = e->where;

    for(i=0; i<rank; i++)
	new->u.ar.dimen_type[i] = DIMEN_RANGE;

    if (e->ref == ref)
	e->ref = new;

    else {
	r = e->ref;
	while(r->next != ref)
	    r = r->next;

	r->next = new;
    }
}



/* section_ref_rank()-- Get the rank of an array section reference. */

static int section_ref_rank(g95_array_ref *ar) {
int i, rank;

    rank = 0;

    for(i=0; i<ar->dimen; i++)
	switch(ar->dimen_type[i]) {
	case DIMEN_ELEMENT:
	    break;

	case DIMEN_UNKNOWN:
	    return -1;

	case DIMEN_RANGE:
	case DIMEN_VECTOR:
	    rank++;
	    break;
	}

    return rank;
}



/* resolve_ref()-- Resolve part references. */

static try resolve_ref(g95_expr *expr) {
g95_coarray_spec *cas;
int private, r, rank;
g95_array_spec *as;
g95_component *c;
g95_typespec ts;
g95_ref *ref;

    ts  = expr->symbol->ts;
    as  = expr->symbol->as;
    cas = expr->symbol->cas;

    private = ts.type == BT_DERIVED && expr->symbol->attr.use_assoc &&
	      ts.derived->component_access == ACCESS_PRIVATE;

    r = (as == NULL) ? 0 : as->rank;

    rank = 0;

    for(ref=expr->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (as == NULL) {
		if (ts.type == BT_CHARACTER) {
		    if (convert_substring(ref, ts.cl) == FAILURE)
			return FAILURE;

		    ts.cl = &g95_unknown_charlen;
		    break;
		}

		g95_error("Unexpected array reference at %L.", &ref->where);
		return FAILURE;
	    }

	    if (g95_resolve_array_ref(&ref->u.ar, as, &ref->where) == FAILURE)
		return FAILURE;

	    if (ref->u.ar.type == AR_ELEMENT)
		r = 0;

	    else if (ref->u.ar.type == AR_FULL ||
		     ref->u.ar.type == AR_SECTION) {
		if (rank != 0)
		    goto multiple_sections;

		r = rank = (ref->u.ar.type == AR_FULL)
		    ? as->rank
		    : section_ref_rank(&ref->u.ar);
	    }

	    as = NULL;
	    break;

	case REF_COARRAY:
	    if (cas == NULL) {
		g95_error("Unexpected coarray reference in expression at %L",
			  &ref->where);
		return FAILURE;
	    }

	    if (as != NULL) {
		g95_error("Missing array section before coarray reference "
			  "at %L", &ref->where);
		return FAILURE;
	    }

	    if (resolve_coarray_ref(&ref->u.car, cas, &ref->where) == FAILURE)
		return FAILURE;

	    cas = NULL;
	    break;

	case REF_COMPONENT:
	    if (as == NULL)
		r = 0;

	    else {
		if (rank != 0)
		    goto multiple_sections;

		insert_full_section(expr, ref, as->rank);
		r = rank = as->rank;
	    }

	    if (private) {
		g95_error("Cannot reference PRIVATE components at %L",
			  &ref->where);
		return FAILURE;
	    }

	    if (ts.type != BT_DERIVED) {
		g95_error("Unexpected component reference at %L", &ref->where);
		return FAILURE;
	    }

	    c = find_component(ts.derived, ref, expr->symbol->attr.use_assoc);
	    if (c == NULL)
		return FAILURE;

	    if (rank != 0) { /* C614 */
		if (c->pointer) {
		    g95_error("Component to the right of a part reference "
			      "with nonzero rank must not have the POINTER "
			      "attribute at %L", &ref->where);
		    return FAILURE;
		}

		if (c->allocatable) {
		    g95_error("Component to the right of a part reference "
			      "with nonzero rank must not have the "
			      "ALLOCATABLE attribute at %L", &ref->where);
		    return FAILURE;
		}
	    }

	    ref->u.c.component = c;
	    ref->u.c.sym       = ts.derived;

	    ts  = c->ts;
	    as  = c->as;
	    cas = c->cas;
	    break;

	case REF_SUBSTRING:
	    if (ts.type != BT_CHARACTER) {
		g95_error("Substring reference at %L must follow a "
			  "CHARACTER variable", &ref->where);
		return FAILURE;
	    }

	    if (as != NULL) {
		g95_error("Substring reference at %L must follow a scalar "
			  "variable", &ref->where);
		return FAILURE;
	    }

	    resolve_substring(ref);

	    ts.type = BT_UNKNOWN;
	    ts.cl = &g95_unknown_charlen;
	    as = NULL;
	    break;
	}

    if (as != NULL) {  /* Insert a trailing AR_FULL */
	ref = g95_extend_ref(expr, REF_ARRAY, &expr->where);
	ref->u.ar.type = AR_FULL;

	if (rank != 0)
	    goto multiple_sections;

	rank = as->rank;
    }

    expr->ts   = ts;
    expr->rank = rank;

    return SUCCESS;

multiple_sections:
    g95_error("Only one part-reference with nonzero rank can "
	      "be specified at %L", &expr->where);
    return FAILURE;
}



/* generic_dummy()-- Make sure a generic procedure dummy argument is
 * also a specific. */

static try generic_dummy(g95_symbol *sym, g95_locus *where) {

    if (g95_generic_name(sym->name) && !specific_sym(sym)) {
	g95_error("Generic dummy procedure '%s' at %L must also be a specific "
		  "name", sym->name, where);
	return FAILURE;
    }

    return SUCCESS;
}



/* used_variable()-- Return nonzero if the variable is being 'used'
 * and is not an active forall iterator. */

static int used_variable(g95_symbol *sym) {
g95_forall_iterator *q;
code_stack *p;

    for(p=cs_base; p; p=p->prev) {
	if (p->current->type != EXEC_FORALL)
	    continue;

	for(q=p->current->ext.forall_iterator; q; q=q->next)
	    if (q->var->symbol == sym)
		return 0;
    }

    return 1;
}



/* g95_resolve_variable()-- Resolve a variable. */

try g95_resolve_variable(g95_expr *e, int used) {
g95_namespace *ns;
g95_symbol *sym;
g95_ref *ref;
int flag;

    sym = find_declaration(e->symbol);

    if (resolve_symbol(sym) == FAILURE)
	return FAILURE;

    e->symbol = sym;
    flag = 0;

    if (sym->attr.flavor == FL_PROCEDURE && !sym->attr.subroutine) {
	if (sym->attr.entry && g95_find_state(COMP_FUNCTION))
	    flag = 0;

	else {
	    flag = 1;

	    for(ns=g95_current_ns; ns; ns=ns->parent)
		if (ns->proc_name == sym) {
		    flag = (sym->result != sym);
		    break;
		}
	}

    } else if (sym->attr.flavor != FL_VARIABLE &&
	       sym->attr.flavor != FL_PARAMETER)
	flag = 1;

    if (flag) {
	g95_error("Symbol '%s' at %L is not a variable", sym->name, &e->where);
	return FAILURE;
    }

    if (used && used_variable(sym))
	g95_set_usage(sym, &e->where, 0, 1);

    if (resolve_ref(e) == FAILURE)
	return FAILURE;

    ref = e->ref;

    /* This test has to be careful because it is legal to pass a full
     * assumed size array as an actual argument. */

    if (actual_resolve != 1 && e->symbol->as != NULL &&
	e->symbol->as->type == AS_ASSUMED_SIZE && ref->type == REF_ARRAY &&
	ref->next == NULL &&
	(ref->u.ar.type == AR_FULL || 
	 (ref->u.ar.type == AR_SECTION &&
	  ref->u.ar.dimen_type[ref->u.ar.dimen-1] == DIMEN_RANGE &&
	  ref->u.ar.end[ref->u.ar.dimen-1] == NULL))) {
	g95_error("Size of assumed size array is unknown at %L", &ref->where);
	return FAILURE;
    }

    if (e->rank == -1)
	g95_variable_rank(e);

    if (e->symbol->attr.flavor == FL_PROCEDURE && !e->symbol->attr.function) {
	e->ts.type = BT_PROCEDURE;
	return generic_dummy(sym, &e->where);
    }

    if (e->symbol->ts.type == BT_UNKNOWN) {
	/* Must be a simple variable reference */

	if (g95_set_default_type(e->symbol, 1, NULL) == FAILURE)
	    return FAILURE;

	e->ts = e->symbol->ts;

    } else     /* Set type from component references */
	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		break;

	    case REF_COMPONENT:
		e->ts = ref->u.c.component->ts;
		break;

	    case REF_COARRAY:
		break;

	    case REF_SUBSTRING:
		e->ts.type = BT_CHARACTER;
		e->ts.kind = g95_default_character_kind();
		e->ts.cl = &g95_unknown_charlen;
		break;
	    }

    return SUCCESS;
}



/* resolve_unknown_expr()-- Unknown expressions are single symbol
 * actual arguments.  Figure out what the symbol is, then what the
 * expression type from that. */

static try resolve_unknown_expr(g95_expr *e) {
g95_symbol *sym;
char *p;

    sym = find_declaration(e->symbol);

    if (resolve_symbol(sym) == FAILURE)
	return FAILURE;

    e->symbol = sym;
    e->type = EXPR_VARIABLE;

    switch(sym->attr.flavor) {
    case FL_VARIABLE:
	e->ts = sym->ts;
	g95_set_usage(sym, &e->where, 0, 1);

	if (e->rank == -1)
	    g95_variable_rank(e);

	break;

    case FL_PROCEDURE:
	e->ts.type = BT_PROCEDURE;

	if (sym->attr.proc == PROC_INTERNAL) {
	    g95_error("Internal procedure '%s' at %L cannot be used as an "
		      "actual argument", sym->name, &e->where);
	    return FAILURE;
	}

	if (e->rank == -1)
	    e->rank = (sym->as == NULL) ? 0 : sym->as->rank;

	return generic_dummy(sym, &e->where);

    case FL_LABEL:       p = "Label";         goto bad;
    case FL_DERIVED:     p = "Derived type";  goto bad;
    case FL_NAMELIST:    p = "NAMELIST";
    case FL_PROGRAM:     p = "PROGRAM";       goto bad;
    case FL_BLOCK_DATA:  p = "BLOCK DATA";    goto bad;
    case FL_MODULE:      p = "MODULE";        goto bad;
    bad:
	g95_error("%s '%s' at %L cannot be used as an actual argument",
		  p, sym->name, &e->where);
	return FAILURE;

    default:
	g95_internal_error("resolve_unknown_expr(): bad flavor");
    }

    return SUCCESS;
}



/* compare_ff()-- Compare formal argument lists. */

static try compare_ff(g95_symbol *a1, g95_symbol *b1) {
g95_formal_arglist *a, *b;

    a = a1->formal;
    b = b1->formal;

    for(;;) {
	if (a == NULL || b == NULL)
	    break;

	if (g95_compare_types(&a->sym->ts, &b->sym->ts) == 0) {
	    g95_error("Interface type at %L doesn't match actual definition",
		      &b->where);
	    return FAILURE;
	}

	if (a->sym->attr.pointer    != b->sym->attr.pointer    ||
	    a->sym->attr.dimension  != b->sym->attr.dimension  ||
	    a->sym->attr.optional   != b->sym->attr.optional   ||
	    a->sym->attr.target     != b->sym->attr.target     ||
	    a->sym->attr.value      != b->sym->attr.value      ||
	    a->sym->attr.intent     != b->sym->attr.intent) {
	    g95_error("Attribute mismatch in interface at %L", &b->where);
	    return FAILURE;
	}

	a = a->next;
	b = b->next;
    }

    if (a != NULL || b != NULL) {
	g95_error("Argument count mismatch in interface at %L",
		  &b1->declared_at);
	return FAILURE;
    }

    return SUCCESS;
}



/* resolve_proc()-- Check the additional constraints on the procedure
 * itself. */

static try resolve_proc(g95_namespace *ns) {
g95_symbol *sym, *self;
g95_interface *p;
g95_symtree *st;

    sym = ns->proc_name;
    if (sym == NULL)
	return SUCCESS;

    if (sym->attr.intrinsic) {
	g95_error("Procedure '%s' at %L cannot be INTRINSIC",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.external) {
	g95_error("Procedure '%s' at %L cannot be EXTERNAL",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    st = g95_find_generic(sym->name, sym->ns);
    if (st != NULL) {
	for(p=st->n.generic; p; p=p->next)
	    if (p->sym == sym)
		break;

	if (p == NULL)
	    g95_error("Procedure '%s' at %L conflicts with generic name '%s'",
		      sym->name, &sym->declared_at, sym->name);
    }

    g95_find_symbol("SELF", NULL, 0, &self);
    if (self != NULL && compare_ff(sym, self) == FAILURE)
	return FAILURE;

    if (namespace_kind(ns) != COMP_FUNCTION)
	return SUCCESS;

    /********************** Function specific checks *************************/

    if (sym->result->ts.type == BT_UNKNOWN) {
	g95_error("Function '%s' at %L has no IMPLICIT type",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.recursive && sym->result->ts.type == BT_CHARACTER &&
	sym->result->ts.cl->length == NULL) {
	g95_error("RECURSIVE Function '%s' at %L cannot return an "
		  "assumed-size string", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (G95_STRICT_F95() && sym->result->ts.type == BT_DERIVED &&
	g95_symbol_access(sym) != ACCESS_PRIVATE &&  /* not the result! */
	g95_symbol_access(sym->result->ts.derived) == ACCESS_PRIVATE) {
	g95_error("Function '%s' at %L returning a PRIVATE type must also "
		  "be PRIVATE", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->result->ts.type == BT_DERIVED && sym->attr.bind &&
	!sym->result->ts.derived->attr.bind &&
	sym->result->ts.derived->attr.itype != ITYPE_C_PTR &&
	sym->result->ts.derived->attr.itype != ITYPE_C_FUNPTR) {
	g95_error("BIND(C) function '%s' at %L must return a BIND(C) type",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->result->ts.type == BT_CHARACTER && sym->result->attr.pointer &&
	sym->result->ts.cl->length == NULL) {
	g95_error("POINTER valued function '%s' at %L cannot return an "
		  "assumed-length character (*)",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.elemental && sym->result->as != NULL) {
	g95_error("ELEMENTAL Function '%s' at %L cannot return an array",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.elemental && sym->result->attr.pointer) {
	g95_error("ELEMENTAL Function '%s' at %L cannot return a POINTER",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.elemental && sym->result->ts.type == BT_CHARACTER &&
	sym->result->ts.cl != NULL && sym->result->ts.cl->length == NULL) {
	g95_error("ELEMENTAL Function '%s' at %L cannot return an "
		  "assumed-length CHARACTER", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.pure && sym->result->ts.type == BT_CHARACTER &&
	sym->result->ts.cl != NULL && sym->result->ts.cl->length == NULL) {
	g95_error("PURE Function '%s' at %L cannot return an assumed-length "
		  "CHARACTER", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (!sym->attr.set && !sym->attr.alloced &&
	!sym->result->attr.set && !sym->result->attr.alloced &&
	(sym->ts.type != BT_DERIVED ||
	 !g95_initialized_component(sym->ts.derived)))
	g95_warning(139, "Value of function '%s' at %L is never set",
		    sym->name, &sym->declared_at);

    if (sym->result->ts.type == BT_CHARACTER && sym->result->as != NULL &&
	sym->result->ts.cl->length == NULL) {
	g95_error("Array valued function '%s' at %L cannot return an "
		  "assumed-length character (*)",\
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->result->ts.type == BT_CHARACTER &&
	!explicit_interface(sym) &&
	(sym->result->ts.cl == &g95_unknown_charlen ||
	 (sym->result->ts.cl->length != NULL &&
	  sym->result->ts.cl->length->type != EXPR_CONSTANT))) {
	g95_error("Explicit interface required for variable-length CHARACTER "
		  "function '%s' at %L", sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->value != NULL || sym->result->value != NULL) {
	g95_error("Function '%s' at %L cannot have an initial value",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->attr.data || sym->result->attr.data) {
	g95_error("Function '%s' at %L cannot appear in a DATA statement",
		  sym->name, &sym->declared_at);
	return FAILURE;
    }

    if (sym->result->ts.type == BT_DERIVED &&
	g95_coarray_component(sym->result->ts.derived)) {
	g95_error("Function '%s' at %L cannot return a derived type "
		  "containing a coarray", sym->name, &sym->declared_at);
	return FAILURE;
    }

    /* Check some things that can't be checked at parse time */

    if (sym->result != sym) {
	if (sym->attr.proc != PROC_MODULE && sym->ns->state != COMP_MODULE &&
	    sym->attr.access != ACCESS_UNKNOWN) {
	    g95_error("Function '%s' at %L cannot have an ACCESS-SPEC",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}

	if (sym->attr.in_namelist) {
	    g95_error("Function '%s' at %L cannot be in a NAMELIST",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}

	if (sym->attr.save) {
	    g95_error("Function '%s' at %L cannot be SAVEd",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* expression_shape()-- Given an expression, determine its shape.
 * This is easier than it sounds.  Leaves the shape array NULL if it
 * is not possible to determine the shape. */

static void expression_shape(g95_expr *e) {
bignum shape[G95_MAX_DIMENSIONS];
int i, rank;

    if (e->rank == 0 || e->shape != NULL)
	return;

    for(i=0; i<G95_MAX_DIMENSIONS; i++)
	shape[i] = NULL;

    rank = e->rank;

    for(i=0; i<rank; i++) {
	shape[i] = g95_array_dimen_size(e, i);
	if (shape[i] == NULL) {
	    for(i=i-1; i>=0; i--)
		big_free(shape[i]);

	    return;
	}
    }

    e->shape = g95_get_shape(e->rank);

    for(i=0; i<rank; i++) {
	e->shape[i] = big_clone(shape[i]);
	big_permanent(e->shape[i]);
    }
}



/* g95_variable_rank()-- Given a variable expression node, compute the
 * rank of the expression by examining the base symbol and any
 * reference structures it may have. */

void g95_variable_rank(g95_expr *e) {
int seen_section, rank;
g95_array_spec *as; 
g95_ref *ref;

    if (e->symbol == NULL)
	return;

    as = e->symbol->as;
    rank = (as == NULL) ? 0 : as->rank;
    seen_section = 0;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    switch(ref->u.ar.type) {
	    case AR_ELEMENT:
		rank = 0;
		break;

	    case AR_UNKNOWN:
		rank = -1;
		break;

	    case AR_FULL:
		seen_section = 1;
		goto done;

	    case AR_SECTION:
		seen_section = 1;
		goto section;
	    }

	    break;

	case REF_COMPONENT:
	    if (rank != 0)
		goto done;

	    if (ref->u.c.component != NULL) {
		as = ref->u.c.component->as;
		rank = (as == NULL) ? 0 : as->rank;
	    }

	    break;

	case REF_SUBSTRING:
	case REF_COARRAY:
	    break;
	}

    goto done;

section:
    rank = section_ref_rank(&ref->u.ar);

done:
    e->rank = rank;
}



/* resolve_procedure_expr()-- Resolve a procedure expression */

static try resolve_procedure_expr(g95_expr *e) {
g95_symbol *sym;

    sym = find_declaration(e->symbol);

    if (resolve_symbol(sym) == FAILURE)
	return FAILURE;

    e->symbol = sym;
    g95_set_usage(sym, &e->where, 0, 1);

    return SUCCESS;
}



/* resolve_constant()-- Constants with reference structures can be
 * created by parameter substitution.  The only legal cases here are a
 * string constant followed by a substring reference or a parameter
 * array. */

static try resolve_constant(g95_expr *e) {
g95_ref *ref;

    ref = e->ref;
    if (ref == NULL)
	return SUCCESS;

    if (e->ts.type != BT_CHARACTER ||
	ref->type != REF_ARRAY || ref->next != NULL) {
	g95_error("Syntax error in variable reference at %L", &ref->where);
	return FAILURE;
    }

    if (ref->next != NULL) {
	g95_error("Syntax error in substring reference at %L", &ref->where);
	return FAILURE;
    }

    /* Convert the substring of a constant to what it should really be */

    if (convert_substring(ref, NULL) == FAILURE)
	return FAILURE;

    e->ts.cl = &g95_unknown_charlen;
    e->type = EXPR_SUBSTRING;

    return SUCCESS;
}


/* g95_resolve_null()-- Resolve the NULL() expression.  The rank and
 * type can vary depending on what it is being assigned to. */

void g95_resolve_null(g95_expr *e, g95_typespec *ts, int rank) {

    if (e->value.op.op1 == NULL) {
	e->ts = *ts;
	e->rank = rank;

    } else {
	g95_resolve_expr(e->value.op.op1);

	e->ts = e->value.op.op1->ts;
	e->rank = e->value.op.op1->rank;
    }
}


/* g95_resolve_expr()-- Resolve an expression.  Make sure that types
 * of operands agree with their operators, intrinsic operators are
 * converted to function calls for overloaded types and unresolved
 * function references are resolved. */

try g95_resolve_expr(g95_expr *e) {
try t;

    if (e == NULL)
	return SUCCESS;

    actual_resolve++;

    switch(e->type) {
    case EXPR_ARRAY:
	t = g95_resolve_array_constructor(e);
	break;

    case EXPR_PROCEDURE:
	t = resolve_procedure_expr(e);
	break;

    case EXPR_OP:
	t = resolve_operator(e);
	break;

    case EXPR_SUBSTRING:
	t = resolve_substring(e->ref);
	break;

    case EXPR_CONSTANT:
	t = resolve_constant(e);
	break;

    case EXPR_NULL:
	t = SUCCESS;
	break;

    case EXPR_FUNCTION:
	t = resolve_function(e);
	break;

    case EXPR_VARIABLE:
	t = g95_resolve_variable(e, 1);
	if (t == SUCCESS) {
	    g95_variable_rank(e);
	    expression_shape(e);
	}

	break;

    case EXPR_STRUCTURE:
	t = resolve_structure_cons(e);
	break;

    case EXPR_UNKNOWN:
	t = resolve_unknown_expr(e);
	break;

    default:
	g95_internal_error("g95_resolve_expr(): Bad expression type");
    }

    actual_resolve--;

    if (t == SUCCESS && g95_simplify_mode == SIMP_REGULAR &&
	g95_simplify_expr(e) == FAILURE)
	return FAILURE;

    return t;
}



/* resolve_iterator_part()-- Resolve the start, end or step parts of
 * an iterator expression. */

static try resolve_iterator_part(char *name, g95_expr *expr) {
bt type;

    if (g95_resolve_expr(expr) == FAILURE)
	return FAILURE;
  
    if (expr->rank != 0) {
	g95_error("%s expression in DO loop at %L must be scalar",
		  name, &expr->where);
	return FAILURE;
    }

    type = expr->ts.type;

    if (g95_option.real_loops) {
	if (type != BT_REAL && type != BT_INTEGER) {
	    g95_error("%s expression in DO loop at %L must be INTEGER or REAL",
		      name, &expr->where);
	    return FAILURE;
	}

    } else if (type == BT_REAL) {
	g95_error("REAL %s expression in DO loop at %L - to enable REAL "
		  "loop parameters use -freal-loops", name, &expr->where);
	return FAILURE;

    } else if (type != BT_INTEGER) {
	g95_error("%s expression in DO loop at %L must be INTEGER", 
		  name, &expr->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* g95_resolve_iterator()-- Resolve the expressions in an iterator
 * structure and require that they all be of integer or (optionally)
 * real type.  Real typed iterator expression handling (-freal-loops
 * switch) added by Tom Crane (T.Crane@rhul.ac.uk), Sept. 2007. */

try g95_resolve_iterator(g95_iterator *iter) {

    if (g95_resolve_expr(iter->var) == FAILURE)
	return FAILURE;

    if (iter->var->type == EXPR_CONSTANT) {
	g95_error("PARAMETER cannot be an iterator variable at %L",
		  &iter->var->where);
	return FAILURE;
    }

    if (iter->var->ts.type != BT_INTEGER) {
	if (iter->var->ts.type == BT_REAL && !g95_option.real_loops) {
	    g95_error("REAL loop variable at %L - to enable REAL "
		      "loop parameters use -freal-loops", &iter->var->where);
	    return FAILURE;
	}

	if (iter->var->ts.type != BT_REAL) {
	    g95_error("Loop variable at %L must be a scalar INTEGER or REAL",
		      &iter->var->where);
	    return FAILURE;
	}
    }
  
    if (g95_pure(NULL, 1) && g95_impure_variable(iter->var->symbol)) {
	g95_error("Cannot assign to loop variable in PURE procedure at %L",
		  &iter->var->where);
	return FAILURE;
    }
  
    if (iter->var->ref != NULL) {
	g95_error("Loop variable at %L cannot be a sub-component",
		  &iter->var->where);
	return FAILURE;
    }
  
    if (iter->var->symbol->attr.intent == INTENT_IN) {
	g95_error("Loop variable '%s' at %L cannot be INTENT(IN)",
		  iter->var->symbol->name, &iter->var->where);
	return FAILURE;
    }

    g95_set_usage(iter->var->symbol, &iter->var->where, 1, 0);

    if (resolve_iterator_part("Start", iter->start) == FAILURE)
	return FAILURE;

    if (resolve_iterator_part("End",   iter->end)   == FAILURE)
	return FAILURE;

    if (resolve_iterator_part("Step",  iter->step)  == FAILURE)
	return FAILURE;

    /* bi_is_zero() works for real constants because the mantissa of 0.0
     * is zero. */

    if (iter->step->type == EXPR_CONSTANT &&
	bi_is_zero(iter->step->value.integer)) {
	g95_error("Step expression in DO loop at %L cannot be zero",
		  &iter->step->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* resolve_deallocate_expr()-- Resolve the argument of a deallocate
 * expression.  The expression must be a pointer or a full array. */

static try resolve_deallocate_expr(g95_expr *e) {
int m, allocatable;
g95_symbol *sym;
g95_ref *ref;
try t;

    if (e->type == EXPR_VARIABLE) {
	sym = e->symbol;
	m = sym->attr.used;

    } else {
	sym = NULL;
	m = 0;
    }

    t = g95_resolve_expr(e);

    if (sym != NULL)
	sym->attr.used = m;

    if (t == FAILURE)
	return FAILURE;

    sym = e->symbol;
    sym->attr.dealloced = 1;

    g95_set_usage(sym, &e->where, 0, 1);

    if (is_protected(sym)) {
	g95_error("Can't DEALLOCATE a protected POINTER '%s' at %L", sym->name,
		  &e->where);
	return FAILURE;
    }

    if (g95_pointer_expr(e))
	return SUCCESS;

    if (e->type != EXPR_VARIABLE)
	goto bad;

    if (sym->attr.dummy && sym->attr.allocatable &&
	sym->attr.intent == INTENT_IN) {
	g95_error("Can't DEALLOCATE INTENT(IN) variable '%s' at %L",
		  sym->name, &e->where);
	return FAILURE;
    }

    allocatable = e->symbol->attr.allocatable;
    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type != AR_FULL)
		allocatable = 0;

	    break;

	case REF_COARRAY:
	    g95_error("Cannot DEALLOCATE on another image at %L", &ref->where);
	    return FAILURE;

	case REF_COMPONENT:
	    allocatable = (ref->u.c.component->as != NULL &&
			   ref->u.c.component->as->type == AS_DEFERRED);
	    break;

	case REF_SUBSTRING:
	    allocatable = 0;
	    break;
	}

    if (allocatable == 0) {
    bad:
	g95_error("Expression in DEALLOCATE statement at %L must be "
		  "ALLOCATABLE or a POINTER", &e->where);
    }

    return SUCCESS;
}



/* resolve_alloc()-- Resolve a g95_alloc structure. */

static try resolve_alloc(g95_alloc *a) {
g95_ref *ref;
g95_expr *e;
int i, n;

    if (g95_resolve_expr(a->expr) == FAILURE)
	return FAILURE;

    e = a->expr;

    if (e->type == EXPR_VARIABLE) {
	for(ref=e->ref; ref; ref=ref->next)
	    if (ref->type == REF_COMPONENT)
		ref->u.c.alloc = 1;

	e->symbol->attr.alloced = 1;

	if (e->symbol->attr.dummy && e->symbol->attr.allocatable &&
	    e->symbol->attr.intent == INTENT_IN) {
	    g95_error("Can't ALLOCATE INTENT(IN) variable '%s' at %L",
		      e->symbol->name, &e->where);
	    return FAILURE;
	}
    }

    n = a->rank + a->corank;

    for(i=0; i<n; i++) {
	if (g95_resolve_expr(a->lower[i]) == FAILURE ||
	    g95_resolve_expr(a->upper[i]) == FAILURE)
	    return FAILURE;

	e = a->lower[i];
	if (e != NULL) {
	    if (e->ts.type != BT_INTEGER)
		goto integer;

	    if (e->rank != 0)
		goto scalar;
	}

	e = a->upper[i];
	if (e != NULL) {
	    if (e->ts.type != BT_INTEGER)
		goto integer;

	    if (e->rank != 0)
		goto scalar;
	}
    }

    return SUCCESS;

integer:
    g95_error("ALLOCATE bound at %L must of be of type INTEGER", &e->where);
    return FAILURE;

scalar:
    g95_error("ALLOCATE bound at %L must be scalar", &e->where);
    return FAILURE;
}



/* transform_while()-- Transform a DO WHILE statement into an infinite
 * loop with an appropriate EXIT statement at the front. */

static void transform_while(g95_code *code) {
g95_code *c1, *c2;
g95_expr *e;

    e = code->expr;
    if (e->type == EXPR_CONSTANT && e->value.logical)
	g95_free_expr(e);

    else {
	e = g95_get_expr(); 
	e->type = EXPR_OP;
	e->value.op.operator = INTRINSIC_NOT;
	e->where = code->where;

	e->ts.type = BT_LOGICAL;
	e->ts.kind = g95_default_logical_kind();
	e->value.op.op1 = code->expr;

	c2 = g95_get_code(EXEC_EXIT, NULL);
	c2->ext.block = code;

	c1 = g95_get_code(EXEC_IF, NULL);
	c1->block = c2;
	c1->expr = e;

	c1->next = code->block;
	code->block = c1;
    }

    code->expr = NULL;
}



/* scalar_where()-- Convert a scalar WHERE statement into the
 * equivalent series of IF statements. */

static void scalar_where(g95_code *code) {
g95_code *p, *q, *r;

    r = NULL;
    for(p=code;; p=q) {
	q = p->block;

	if (q == NULL)
	    break;

	p->type = EXEC_IF;

	if (q->expr == NULL) {
	    r->ext.block = q->next;
	    break;
	}

	p->expr = q->expr;
	q->expr = NULL;

	p->block = q->next;
	q->next = NULL;

	if (r != NULL)
	    r->ext.block = p;

	if (q == NULL) {
	    g95_free(p->ext.block);
	    p->ext.block = NULL;
	    break;
	}

	r = p;
    }
}



/* where_conformance()-- Check for conformance between
 * WHERE/ELSEWHEREs.  Returns nonzero if there was a problem. */

static int where_conformance(g95_code *code, where_frame *prev) {
where_frame *f, frame;
g95_code *c, *p, *q;

    frame.prev = prev;
    frame.head = code;

    for(p=code; p; p=p->block) {
	if (p->expr == NULL)
	    continue;

	for(f=&frame; f; f=f->prev) {
	    q = (f == &frame)
		? p->block
		: f->head->block;

	    for(; q; q=q->block) {
		if (q->expr == NULL)
		    continue;

		if (p->expr->rank != q->expr->rank) {
		    g95_error("Rank mismatch in WHERE block at %L and %L",
			      &p->expr->where, &q->expr->where);
		    return 1;
		}

		if (p->expr->rank != 0 &&
		    g95_check_conformance("WHERE/ELSEWHERE mask",
					  p->expr, q->expr) == FAILURE)
		    return 1;
	    }
	}

	for(c=p->next; c; c=c->next)
	    switch(c->type) {
	    case EXEC_ASSIGN:
		if (c->expr->rank == 0)
		    break;

		if (c->expr->rank != p->expr->rank) {
		    g95_error("Rank mismatch between WHERE mask at %L and "
			      "array assignment at %L",
			      &c->expr->where, &p->expr->where);
		    return 1;
		}

		if (g95_check_conformance("WHERE mask/body",
					  p->expr, c->expr) == FAILURE)
		    return 1;

		break;

	    case EXEC_WHERE:
		if (where_conformance(c, &frame))
		    return 1;

		break;

	    default:
		break;
	    }
    }

    if (code->block->expr->rank == 0)
	scalar_where(code);

    return 0;
}



/* resolve_transfer()-- Resolve a transfer statement.  This is mainly
 * just making sure that a derived type being transferred has only
 * nonpointer components. */

static try resolve_transfer(g95_code *code) {
g95_symbol *sym, *d;
g95_typespec *ts;
g95_expr *e;

    e = code->expr;
    ts = &e->ts;

    if (e->type == EXPR_NULL) {
	g95_error("Can't transfer a NULL() pointer at %L", &code->expr->where);
	return FAILURE;
    }

    if (ts->type == BT_DERIVED) {
	d = ts->derived;

	if (g95_pointer_component(d)) {
	    g95_error("Data transfer element at %L cannot have POINTER "
		      "components", &code->where);
	    return FAILURE;
	}

	if (g95_allocatable_component(d)) {
	    g95_error("Data transfer element at %L cannot have ALLOCATABLE "
		      "components", &code->where);
	    return FAILURE;
	}

	if (d->component_access == ACCESS_PRIVATE &&
	    (d->ns->state != COMP_MODULE || d->module != NULL)) {
	    g95_error("Data transfer element at %L cannot have PRIVATE "
		      "components", &code->where);
	    return FAILURE;
	}
    }

    if (code->expr->type != EXPR_VARIABLE)
	return SUCCESS;

    sym = e->symbol;

    switch(code->ext.transfer) {
    case M_READ:
	g95_set_usage(sym, &e->where, 1, 0);
	break;

    case M_WRITE:
    case M_PRINT:
	g95_set_usage(sym, &e->where, 0, 1);
	break;

    case M_INQUIRE:
	g95_set_usage(sym, &e->where, 1, 1);
	break;
    }

    if (sym->as != NULL && sym->as->type == AS_ASSUMED_SIZE &&
	e->ref->type == REF_ARRAY &&
	(e->ref->u.ar.type == AR_FULL ||
	 (e->ref->u.ar.type == AR_SECTION &&
	  e->ref->u.ar.dimen_type[e->ref->u.ar.dimen-1] == DIMEN_RANGE &&
	  e->ref->u.ar.end[e->ref->u.ar.dimen-1] == NULL))) {
	g95_error("Data transfer element at %L cannot be an unbounded "
		  "reference to an assumed-size array", &code->where);
	return FAILURE;
    }

    if (e->rank > 0 && e->ref != NULL && e->ref->type == REF_ARRAY)
	sym->attr.desc = 1;

    return SUCCESS;
}



/* pure_assignment()-- Check for assignment problems within a PURE
 * procedure. */

static void pure_assignment(g95_expr *lhs, g95_expr *rhs, g95_locus *where) {
g95_symbol *s, *sym;
g95_equiv *p, *q;
int m;

    sym = lhs->symbol;

    if (g95_impure_variable(sym)) {
	g95_error("Cannot assign to variable '%s' in PURE procedure at %L",
		  sym->name, where);
	return;
    }

    if (lhs->ts.type == BT_DERIVED && g95_pointer_component(lhs->ts.derived) &&
	rhs->type == EXPR_VARIABLE && g95_impure_variable(rhs->symbol)) {
	g95_error("Implicit pointer assignment at %L in PURE procedure",
		  where);
	return;
    }

    for(p=g95_current_ns->equiv; p; p=p->next) {
	m = 0;

	for(q=p; q; q=q->eq) {
	    s = q->expr->symbol;
	    if (s == sym)
		m |= 1;

	    if (s->attr.in_common)
		m |= 2;
	}

	if (m == 3) {
	    g95_error("Variable '%s' at %L cannot be equivalenced to a COMMON "
		      "variable and assigned within a PURE procedure",
		      sym->name, &lhs->where);
	    break;
	}
    }
}



/* check_entry()-- Check an ENTRY statement for using the formal
 * arguments before the ENTRY. */

static void check_entry(g95_code *code) {
g95_formal_arglist *f;
g95_symbol *sym;

    for(f=code->sym->formal; f; f=f->next) {
	sym = f->sym;
	if (sym == NULL || sym->attr.seen_formal)
	    continue;

	sym->attr.seen_formal = 1;

	if (sym->attr.used_formal)
	    g95_error("Formal parameter '%s' in ENTRY statement at %L is used "
		      "before the ENTRY statement", sym->name, &f->where);
    }
}


/* clear_used_formal()-- Clear the used_formal bits on all members of
 * a formal argument list. */

static void clear_used_formal(g95_formal_arglist *f) {

    for(; f; f=f->next)
	if (f->sym != NULL)
	    f->sym->attr.used_formal = 0;
}


/* init_check_entry0()-- Work function for init_check_entry */

static void init_check_entry0(g95_symbol *sym) {

    if (sym->attr.entry)
	clear_used_formal(sym->formal);
}


/* init_check_entry()-- Get things ready for check_entry. */

static void init_check_entry(g95_namespace *ns) {
g95_formal_arglist *f;

    if (ns->proc_name != NULL) {
	clear_used_formal(ns->proc_name->formal);

	for(f=ns->proc_name->formal; f; f=f->next)
	    if (f->sym != NULL)
		f->sym->attr.seen_formal = 1;
    }

    g95_traverse_ns(ns, init_check_entry0);
}


/* check_forall_iterators()-- Given the a variable in an assignment or
 * pointer assignment statement, make sure it depends on all of the
 * active forall iterators. */

static void check_forall_iterators(g95_expr *lhs) {
g95_forall_iterator *iter;
forall_frame *f;
g95_symbol *sym;

    for(f=forall_root; f; f=f->prev)
	for(iter=f->code->ext.forall_iterator; iter; iter=iter->next) {
	    sym = iter->var->symbol;

	    if (lhs->symbol == sym)
		g95_error("FORALL iterator variable '%s' at %L cannot be "
			  "assigned in FORALL block", sym->name, &lhs->where);

	    if (!g95_find_variable(lhs, sym))
		g95_error("Left side of assignment at %L in FORALL block must "
			  "contain the '%s' iterator", &lhs->where, sym->name);
	}
}



/* resolve_sync()-- Resolve a sync_state structure. */

static try resolve_sync(g95_sync_stat *sync) {

    if (sync->stat != NULL) {
	if (g95_resolve_expr(sync->stat) == FAILURE)
	    return FAILURE;

	if (sync->stat->ts.type != BT_INTEGER) {
	    g95_error("STAT tag at %L must be an INTEGER", &sync->stat->where);
	    return FAILURE;
	}

	if (sync->stat->rank != 0) {
	    g95_error("STAT tag at %L must be scalar", &sync->stat->where);
	    return FAILURE;
	}
    }

    if (sync->errmsg != NULL) {
	if (g95_resolve_expr(sync->errmsg) == FAILURE)
	    return FAILURE;

	if (sync->errmsg->ts.type != BT_CHARACTER) {
	    g95_error("ERRMSG tag at %L must be an INTEGER",
		      &sync->errmsg->where);
	    return FAILURE;
	}

	if (sync->errmsg->rank != 0) {
	    g95_error("ERRMSG tag at %L must be scalar", &sync->errmsg->where);
	    return FAILURE;
	}
    }

    if (sync->ready != NULL) {
	if (g95_resolve_expr(sync->ready) == FAILURE)
	    return FAILURE;

	if (sync->ready->ts.type != BT_LOGICAL) {
	    g95_error("READY tag at %L must be LOGICAL", &sync->ready->where);
	    return FAILURE;
	}

	if (sync->ready->rank != 0) {
	    g95_error("READY tag at %L must be scalar", &sync->ready->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* compare_allocate_expr()-- Compare allocation expressions.  Return
 * nonzero if equal. */

static int compare_allocate_expr(g95_expr *a, g95_expr *b) {
g95_ref *ra, *rb;

    if (a->symbol != b->symbol)
	return 0;

    ra = a->ref;
    rb = b->ref;

    for(;;) {
	if (ra == NULL && rb == NULL)
	    return 1;

	if (ra == NULL || rb == NULL || ra->type != rb->type)
	    break;

	if (ra->type == REF_COMPONENT &&
	    ra->u.c.component != rb->u.c.component)
	    break;

	if (ra->type == REF_ARRAY)
	    break;  /* TODO-- compare array refs */

	ra = ra->next;
	rb = rb->next;
    }

    return 0;
}



/* resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */

static try resolve_code(g95_code *code, g95_namespace *ns) {
int m, used, set, dead, forall_save=0;
forall_frame forall;
code_stack frame;
g95_alloc *a, *b;
g95_typespec ts;
g95_symbol *sym;
g95_code *d;
try rc, t;

    if (code == NULL)
	return SUCCESS;

    frame.prev = cs_base;
    frame.head = code;
    cs_base = &frame;
    used = set = 0;
    dead = 0;
    rc = SUCCESS;

    for(; code; code=code->next) {
	frame.current = code;

	if (code->here != NULL && code->here->referenced == ST_LABEL_TARGET)
	    ns->seen_branch_target = 1;

	if ((code->type == EXEC_ASSIGN || code->type == EXEC_POINTER_ASSIGN ||
	     code->type == EXEC_TRANSFER) &&
	    code->expr->type == EXPR_VARIABLE) {
	    used = code->expr->symbol->attr.used;
	    set = code->expr->symbol->attr.set;
	}

	if (code->here != NULL)
	    dead = 0;

	if (code->type == EXEC_FORALL)
	    forall_mask = 1;

	t = g95_resolve_expr(code->expr);

	if ((code->type == EXEC_ASSIGN || code->type == EXEC_TRANSFER ||
	     code->type == EXEC_POINTER_ASSIGN) &&
	    code->expr->type == EXPR_VARIABLE) {
	    code->expr->symbol->attr.used = used;
	    code->expr->symbol->attr.set = set;
	}

	if (code->type == EXEC_FORALL)
	    forall_mask = 0;

	if (t == FAILURE || g95_resolve_expr(code->expr2) == FAILURE) {
	    rc = FAILURE;
	    goto done;
	}

	sym = code->sym;
	if (sym != NULL && resolve_symbol(sym) == FAILURE) {
	    rc = FAILURE;
	    goto done;
	}

	switch(code->type) {
	case EXEC_CYCLE:     case EXEC_STOP:   case EXEC_EXIT:
	case EXEC_ERROR_STOP:
	    dead = 1;
	    /* Fall through */
    
	case EXEC_CONTINUE:  case EXEC_NOP:    case EXEC_DT_END:
	    break;

	case EXEC_ENTRY:
	    check_entry(code);
	    break;

	case EXEC_RETURN:
	    if (code->expr != NULL) {
		if (code->expr->ts.type != BT_INTEGER)
		    g95_error("Alternate RETURN statement at %L requires an "
			      "INTEGER return specifier", &code->expr->where);

		if (code->expr->rank != 0)
		    g95_error("Alternate RETURN statement at %L must be "
			      "scalar", &code->expr->where);
	    }

	    dead = 1;
	    break;

	case EXEC_ASSIGN:
	    m = g95_extend_assign(code, ns);
	    if (m == 0)
		goto call;

	    else if (m == 2)
		break;

	    sym = code->expr->symbol;

	    if (sym->attr.pointer)
		g95_set_usage(sym, &code->expr->where, 0, 1);

	    else
		g95_set_usage(sym, &code->expr->where, 1, 0);

	    if (sym->attr.entry || sym->attr.result_var)
		g95_set_usage(sym->ns->proc_name, NULL, 1, 0);

	    if (g95_pure(NULL, 1))
		pure_assignment(code->expr, code->expr2, &code->where);

	    if (g95_check_assign(code->expr, code->expr2) == FAILURE)
		break;

	    if (g95_forall_flag)
		check_forall_iterators(code->expr);

	    break;

	case EXEC_PAUSE:
	    if (code->expr != NULL &&
		code->expr->ts.type != BT_INTEGER &&
		code->expr->ts.type != BT_CHARACTER)
		g95_error("PAUSE code at %L must be INTEGER or CHARACTER",
			  &code->expr->where);
	    break;

	case EXEC_GOTO:
	    if (code->label != NULL &&
		resolve_branch(code->label, code) == FAILURE)
		break;

	    if (code->expr != NULL) {
		sym = code->expr->symbol;

		if (sym->ts.type != BT_INTEGER) {
		    g95_error("GOTO variable '%s' at %L must be INTEGER",
			      sym->name, &code->where);
		    break;
		}

		if (code->expr->rank != 0) {
		    g95_error("GOTO variable '%s' at %L must be scalar",
			      sym->name, &code->where);
		    break;
		}

		if (code->expr->ts.kind != g95_default_integer_kind(0)) {
		    g95_clear_ts(&ts);
		    ts.type = BT_INTEGER;
		    ts.kind = g95_default_integer_kind(0);
		    g95_convert_type(code->expr, &ts, 1);
		}

		g95_set_usage(sym, &code->expr->where, 0, 1);
	    }

	    if (!dead && code->expr != NULL)
		g95_current_ns->goto_label = 1;

	    dead = 1;
	    break;

	case EXEC_ARITHMETIC_IF:
	    if (code->expr->ts.type != BT_INTEGER &&
		code->expr->ts.type != BT_REAL) {
		g95_error("Arithmetic IF statement at %L requires a numeric "
			  "expression", &code->expr->where);
		break;
	    }

	    if (resolve_branch(code->label,  code) == FAILURE ||
		resolve_branch(code->label2, code) == FAILURE ||
		resolve_branch(code->label3, code) == FAILURE) {
		rc = FAILURE;
		break;
	    }

	    dead = 1;
	    break;

	case EXEC_POINTER_ASSIGN:
	    g95_set_usage(code->expr->symbol, &code->expr->where, 1, 0);

	    if (code->expr2->type == EXPR_VARIABLE) {
		sym = code->expr2->symbol;
		sym->attr.targetted = 1;
		g95_set_usage(sym, NULL, 0, 1);
	    }

	    g95_check_pointer_assign(code->expr, code->expr2);

	    if (g95_coindexed_expr(code->expr)) {
		g95_error("Cannot associate a coarray pointer at %L",
			  &code->expr->where);
		rc = FAILURE;
	    }

	    if (g95_coindexed_expr(code->expr2)) {
		g95_error("Target pointer at %L cannot be coindexed",
			  &code->expr2->where);
		rc = FAILURE;
	    }

	    if (is_protected(code->expr->symbol)) {
		g95_error("Cannot assign to a PROTECTED pointer at %L",
			  &code->expr->where);
		rc = FAILURE;
	    }

	    if (g95_forall_flag)
		check_forall_iterators(code->expr);

	    break;

	case EXEC_IF:
	    if (code->expr != NULL &&
		(code->expr->ts.type != BT_LOGICAL || code->expr->rank != 0)) {
		g95_error("IF clause at %L requires a scalar LOGICAL "
			  "expression", &code->expr->where);
		break;
	    }

	    if (resolve_code(code->block, ns) == FAILURE)
		rc = FAILURE;

	    if (resolve_code(code->ext.block, ns) == FAILURE)
		rc = FAILURE;

	    break;

	case EXEC_CALL:
	call:
	    if (resolve_call(code) == FAILURE)
		rc = FAILURE;

	    break;

	case EXEC_WHERE:
	    m = g95_where_flag;
	    g95_where_flag = 1;

	    for(d=code->block; d; d=d->block) {
		if (g95_resolve_expr(d->expr) == FAILURE)
		    rc = FAILURE;

		if (d->expr != NULL && d->expr->ts.type != BT_LOGICAL) {
		    g95_error("WHERE mask at %L must be of LOGICAL type",
			      &d->expr->where);
		    rc = FAILURE;
		}

		if (resolve_code(d->next, ns) == FAILURE)
		    rc = FAILURE;
	    }

	    g95_where_flag = m;

	    where_conformance(code, NULL);
	    break;

	case EXEC_SELECT:      /* Select is complicated */
	    if (g95_resolve_select(code) == FAILURE)
		rc = FAILURE;

	    for(d=code->block; d; d=d->block) {
		if (g95_resolve_expr(d->expr) == FAILURE)
		    rc = FAILURE;

		if (resolve_code(d->next, ns) == FAILURE)
		    rc = FAILURE;
	    }

	    break;

	case EXEC_DO_WHILE:
	    if (resolve_code(code->block, ns) == FAILURE)
		rc = FAILURE;

	    if (code->expr == NULL)
		break;

	    if (code->expr->rank != 0 || code->expr->ts.type != BT_LOGICAL)
		g95_error("Exit condition of DO WHILE loop at %L must be "
			  "a scalar LOGICAL expression",
			  &code->expr->where);

	    transform_while(code);
	    break;

	case EXEC_DO:
	    if (code->ext.iterator != NULL)
		g95_resolve_iterator(code->ext.iterator);

	    sym = code->ext.iterator->var->symbol;

	    if (G95_STRICT_F() &&
		(sym->ns != g95_current_ns || sym->attr.use_assoc ||
		 sym->attr.dummy)) {
		g95_error("DO-variable '%s' at %L must be in the same scope "
			  "as the DO-loop under F",
			  code->ext.iterator->var->symbol->name,
			  &code->where);
		rc = FAILURE;
	    }

	    if (resolve_code(code->block, ns) == FAILURE)
		rc = FAILURE;

	    break;

	case EXEC_ALLOCATE:
	    if (code->expr != NULL) {
		g95_set_usage(code->expr->symbol, &code->expr->where, 1, 0);

		if (code->expr->ts.type != BT_INTEGER) {
		    g95_error("STAT tag in ALLOCATE statement at %L must be "
			      "of type INTEGER", &code->expr->where);
		    break;
		}

		if (code->expr->rank != 0) {
		    g95_error("STAT tag in ALLOCATE statement at %L must "
			      "be scalar", &code->expr->where);
		    rc = FAILURE;
		    break;
		}
	    }

	    for(a=code->ext.alloc_list; a; a=a->next)
		if (resolve_alloc(a) == FAILURE) {
		    rc = FAILURE;
		    break;
		}

	    for(a=code->ext.alloc_list; a; a=a->next) {
		for(b=a->next; b; b=b->next)
		    if (compare_allocate_expr(a->expr, b->expr))
			g95_warning(168, "Duplicate allocation at %L and %L",
				    &a->expr->where, &b->expr->where);

		if (a->expr->rank != 0 || a->expr->ref != NULL ||
		    a->expr->ts.type != BT_INTEGER)
		    continue;

		for(b=a->next; b; b=b->next)
		    if (g95_check_dependency(a->expr, b->expr) &
			(SEEN_FULL | SEEN_SECTION | SEEN_SCALAR)) {
			g95_error("Pointer '%s' at %L is used to ALLOCATE "
				  "memory after being allocated itself",
				  a->expr->symbol->name, &a->expr->where);
			rc = FAILURE;
		    }
	    }

	    break;

	case EXEC_DEALLOCATE:
	    if (code->expr != NULL) {
		g95_set_usage(code->expr->symbol, &code->expr->where, 1, 0);

		if (code->expr->ts.type != BT_INTEGER) {
		    g95_error("STAT tag in DEALLOCATE statement at %L must "
			      "be of type INTEGER", &code->expr->where);
		    rc = FAILURE;
		    break;
		}

		if (code->expr->rank != 0) {
		    g95_error("STAT tag in DEALLOCATE statement at %L must "
			      "be scalar", &code->expr->where);
		    rc = FAILURE;
		    break;
		}
	    }

	    for(a=code->ext.alloc_list; a; a=a->next) {
		if (resolve_deallocate_expr(a->expr) == FAILURE)
		    break;

		for(b=a->next; b; b=b->next)
		    if (compare_allocate_expr(a->expr, b->expr))
			g95_warning(169, "Duplicate deallocation at %L and %L",
				    &a->expr->where, &b->expr->where);
	    }

	    break;

	case EXEC_INQUIRE:
	    if (g95_resolve_inquire(code->ext.inquire) == SUCCESS)
		resolve_branch(code->ext.inquire->err, code);

	    break;

	case EXEC_BACKSPACE:    case EXEC_ENDFILE:    case EXEC_REWIND:
	    if (g95_resolve_filepos(code->ext.filepos) == SUCCESS)
		resolve_branch(code->ext.filepos->err, code);

	    break;

	case EXEC_IOLENGTH:
	    if (code->expr->ts.type != BT_INTEGER ||
		code->expr->ts.kind != g95_default_integer_kind(0)) {
		g95_error("IOLENGTH variable in INQUIRE statement at %L "
			  "must be default integer", &code->expr->where);
		rc = FAILURE;
		break;
	    }

	    if (code->expr->type != EXPR_VARIABLE) {
		g95_error("IOLENGTH tag at %L must specify a variable",
			  &code->expr->where);
		rc = FAILURE;
		break;
	    }

	    g95_set_usage(code->expr->symbol, &code->expr->where, 1, 0);
	    break;

	case EXEC_OPEN:
	    if (g95_resolve_open(code->ext.open) == SUCCESS)
		resolve_branch(code->ext.open->err, code);

	    break;

	case EXEC_TRANSFER:
	    if (resolve_transfer(code) == FAILURE)
		rc = FAILURE;

	    break;

	case EXEC_FORALL:
	    t = g95_resolve_forall_iterators(code->ext.forall_iterator);
	    if (t == FAILURE)
		break;

	    if (code->expr != NULL) {
		if (code->expr->ts.type != BT_LOGICAL) {
		    g95_error("FORALL mask at %L requires a LOGICAL "
			      "expression", &code->expr->where);
		    rc = FAILURE;

		} else if (code->expr->rank != 0) {
		    g95_error("FORALL mask at %L must be a scalar expression",
			      &code->expr->where);
		    rc = FAILURE;

		} else if (code->expr->ts.kind != g95_default_integer_kind(0)) {
		    g95_clear_ts(&ts);
		    ts.type = BT_LOGICAL;
		    ts.kind = g95_default_integer_kind(0);
		    g95_convert_type(code->expr, &ts, 1);
		}
	    }

	    forall_save = g95_forall_flag;
	    g95_forall_flag = 1;

	    forall.code = code;
	    forall.prev = forall_root;
	    forall_root = &forall;

	    if (resolve_code(code->block, ns) == FAILURE)
		rc = FAILURE;

	    forall_root = forall.prev;
	    g95_forall_flag = forall_save;
	    break;

	case EXEC_CLOSE:
	    if (g95_resolve_close(code->ext.close) == SUCCESS)
		resolve_branch(code->ext.close->err, code);

	    break;

	case EXEC_READ:
	case EXEC_WRITE:
	    if (g95_resolve_dt(code) == FAILURE ||
		resolve_branch(code->ext.dt->err, code) == FAILURE ||
		resolve_branch(code->ext.dt->end, code) == FAILURE ||
		resolve_branch(code->ext.dt->eor, code) == FAILURE) {
		rc = FAILURE;
		break;
	    }

	    break;

	case EXEC_FLUSH:
	    if (g95_resolve_flush(code->ext.flush) == FAILURE)
		rc = FAILURE;

	    break;

	case EXEC_WAIT:
	    if (g95_resolve_wait(code->ext.wait) == FAILURE)
		rc = FAILURE;

	    break;

	case EXEC_LABEL_ASSIGN:
	    if (code->sym->as != NULL) {
		g95_error("ASSIGN variable '%s' at %L must be scalar",
			  code->sym->name, &code->where);
		rc = FAILURE;
		break;
	    }

	    if (code->sym->attr.flavor != FL_VARIABLE) {
		g95_error("ASSIGN statement at %L must be to a variable",
			  &code->where);
		rc = FAILURE;
		break;
	    }

	    if (code->sym->ts.type != BT_INTEGER) {
		g95_error("ASSIGN variable at %L must be of type INTEGER",
			  &code->where);
		rc = FAILURE;
		break;
	    }

	    if (code->label->defined == ST_LABEL_UNKNOWN) {
		g95_error("Label %d at %L is never defined",
			  code->label->value,
			  &code->label->where);
		rc = FAILURE;
		break;
	    }

	    g95_set_usage(code->sym, &code->where, 1, 0);
	    break;

	case EXEC_SYNC_IMAGES:
	    if (code->expr != NULL) {
		if (code->expr->ts.type != BT_INTEGER) {
		    g95_error("Image number at %L must be an INTEGER",
			      &code->expr->where);
		    rc = FAILURE;
		}

		if (code->expr->rank > 1) {
		    g95_error("Image list %L can be at most rank one",
			      &code->expr->where);
		    rc = FAILURE;
		}
	    }

	    /* Fall through */

	case EXEC_NOTIFY:
	case EXEC_QUERY:
	case EXEC_SYNC_MEMORY:
	case EXEC_SYNC_ALL:
	    if (resolve_sync(&code->ext.sync) == FAILURE)
		rc = FAILURE;

	    break;

	case EXEC_SYNC_TEAM:
	    break;

	case EXEC_CRITICAL:
	    if (resolve_code(code->block, ns) == FAILURE)
		rc = FAILURE;

	    break;

	default:    
	    g95_internal_error("resolve_code(): Bad statement code");
	}

	if (rc == FAILURE)
	    break;
    }

done:
    cs_base = frame.prev;
    return rc;
}



/* resolve_common_var()-- Check the extra restrictions on variables
 * within a common block. */

static try resolve_common_var(g95_symbol *sym, int blank_flag) {
g95_array_spec *as;

    /* Derived types in commons must have the SEQUENCE attribute */

    if (sym->ts.type == BT_DERIVED) {
	if (!sym->ts.derived->attr.sequence) {
	    g95_error("Derived type variable '%s' at %L must have the "
		      "SEQUENCE attribute to be in a COMMON",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}

	if (g95_derived_init(sym->ts.derived)) {
	    g95_error("Derived type variable '%s' at %L cannot have a default "
		      "initialization and be in a COMMON",
		      sym->name, &sym->declared_at);
	    return FAILURE;
	}
    }

    as = sym->as;
    if (as != NULL) {
	if (g95_resolve_array_spec(as) == FAILURE ||
	    !g95_constant_array_spec(as, 1))
	    return FAILURE;

	if (sym->attr.pointer && as->type == AS_EXPLICIT) {
	    g95_error("POINTER array specification of '%s' in COMMON at %L "
		      "cannot be explicit", sym->name, &sym->declared_at);
	    return FAILURE;
	}
    }

    if (sym->value != NULL || sym->attr.data) {
	if (blank_flag) {
	    g95_error("Variable '%s' in blank COMMON at %L cannot be "
		      "initialized", sym->name, &sym->declared_at);
	    return FAILURE;
	}

	if (g95_option.fmode != 0 &&
	    g95_current_ns->state != COMP_BLOCK_DATA) {
	    g95_warning(126, "Variable '%s' at %L must be initialized in a "
			"BLOCK DATA subprogram", sym->name, &sym->declared_at);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* resolve_common_block()-- Resolve the blocks as a whole */

static try resolve_common_block(g95_symtree *st) {
g95_common_head *h;
g95_symbol *s;
int i;

    if (st == NULL)
	return SUCCESS;

    resolve_common_block(st->left);
    resolve_common_block(st->right);

    i = 0;
    h = st->n.common;

    for(s=h->head; s; s=s->common_next) {
	if (resolve_common_var(s, 0) == FAILURE)
	    return FAILURE;

	if (s->value != NULL) {
	    i = 1;
	    break;
	}
    }

    if (g95_option.fmode != 0 && i && !h->saved) {
	g95_error("Initialized COMMON block '%s' at %L must have the SAVE "
		  "attribute", st->name, &st->n.common->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* resolve_common_block1()-- Checking a common block after everything
 * else has been resolved. */

static void resolve_common_block1(g95_symtree *st) {
g95_symbol *sym;

    if (st == NULL)
	return;

    resolve_common_block1(st->left);
    resolve_common_block1(st->right);

    g95_find_symbol(st->name, NULL, 0, &sym);

    if (sym != NULL) {
	if (sym->attr.flavor == FL_PARAMETER)
	    g95_error("COMMON block '%s' at %L is also a PARAMETER",
		      sym->name, &sym->declared_at);

	else if (sym->attr.flavor == FL_PROCEDURE &&
		 sym->attr.proc != PROC_ST_FUNCTION &&
		 sym->attr.proc != PROC_INTERNAL)
	    g95_error("COMMON block '%s' at %L is also a PROCEDURE",
		      sym->name, &sym->declared_at);

	else if (sym->attr.function && g95_current_ns->proc_name == sym)
	    g95_error("COMMON block '%s' at %L is also a function result",
		      sym->name, &sym->declared_at);
    }
}



/* save_commons()-- Mark common blocks as saved */

static void save_commons(g95_symtree *st) {

    if (st == NULL)
	return;

    save_commons(st->left);
    save_commons(st->right);

    st->n.common->saved = 1;
}



/* check_data_element_index()-- Make sure the expression is OK for an
 * array element with a DATA variable. */

static try check_data_element_index(g95_expr *e) {
data_frame *p;

    if (e == NULL)
	return SUCCESS;

    switch(e->type) {
    case EXPR_CONSTANT:
	break;

    case EXPR_VARIABLE:
	for(p=data_root; p; p=p->prev)
	    if (p->iterator == e->symbol)
		return SUCCESS;

	g95_error("Variable '%s' at %L must be a DATA implied iterator",
		  e->symbol->name, &e->where);

	return FAILURE;

    case EXPR_OP:
	if (check_data_element_index(e->value.op.op1) == FAILURE)
	    return FAILURE;

	if (e->value.op.op2 != NULL)
	    return check_data_element_index(e->value.op.op1);

	break;

    case EXPR_FUNCTION:
	if (e->value.function.iname != NULL &&
	    strncmp(e->value.function.iname, "__convert", 9) == 0)
	    break;

    case EXPR_NULL:
	g95_error("FUNCTION not allowed in DATA array element at %L",
		  &e->where);
	return FAILURE;

    case EXPR_STRUCTURE:
	g95_error("Structure constructor not allowed in DATA array element "
		  "at %L", &e->where);
	return FAILURE;

    case EXPR_ARRAY:
	g95_error("Array constructor not allowed in DATA array element at %L",
		  &e->where);
	return FAILURE;

    default:
	g95_internal_error("check_data_element_index(): Bad type");
    }

    return SUCCESS;
}



/* check_data_element()-- Check the element of a data list for validity.
 * It must be an array element, or a scalar structure component. */

static try check_data_element(g95_expr *e) {
int seen_element, seen_component, i;
g95_ref *ref;

    if (data_root == NULL)
	return SUCCESS;

    seen_element = 0;
    seen_component = 0;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    if (ref->u.ar.type != AR_ELEMENT) {
		g95_error("Array section not allowed in DATA statement at %L",
			  &ref->where);
		return FAILURE;
	    }

	    seen_element = 1;

	    for(i=0; i<ref->u.ar.dimen; i++)
		if (check_data_element_index(ref->u.ar.start[i]) == FAILURE)
		    return FAILURE;

	    break;

	case REF_COARRAY:
	    g95_error("Cannot specify DATA element on another image at %L",
		      &ref->where);
	    return FAILURE;

	case REF_COMPONENT:
	    seen_component = 1;
	    break;

	case REF_SUBSTRING:
	    if (check_data_element_index(ref->u.ss.start) == FAILURE ||
		check_data_element_index(ref->u.ss.end)   == FAILURE)
		return FAILURE;

	    break;
	}

    if (!seen_element && !seen_component) {
	g95_error("DATA element inside iterator at %L must be an array "
		  "element or a scalar structure component", &e->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* resolve_data_variables()-- Resolve the expressions and iterators
 * associated with a data statement.  This is separate from the
 * assignment checking because data lists only should be resolved
 * once. */

static try resolve_data_variables(g95_data_variable *d) {
data_frame frame, *p;
g95_symbol *sym;
int save;
try t;

    for(; d; d=d->next) {
	if (d->list == NULL) {
	    save = data_flag;
	    data_flag = 1;
	    t = g95_resolve_expr(d->expr);
	    data_flag = save;

	    if (t == FAILURE)
		return FAILURE;

	    sym = d->expr->symbol;

	    if (d->expr->ts.type == BT_DERIVED &&
		g95_initialized_component(d->expr->ts.derived)) {
		g95_error("Variable '%s' which has a default initialization "
			  "cannot appear in a DATA statement at %L",
			  sym->name, &d->expr->where);
		return FAILURE;
	    }

	    if (sym->attr.pointer && d->expr->ref != NULL) {
		g95_error("Can't dereference POINTER in DATA statement at %L",
			  &d->expr->where);
		return FAILURE;
	    }

	    g95_set_usage(sym, NULL, 1, 0);

	    if (automatic_object(sym)) {
		g95_error("Automatic variable '%s' at %L cannot appear in "
			  "a DATA statement", sym->name, &d->expr->where);
		return FAILURE;
	    }

	    if (check_data_element(d->expr) == FAILURE)
		return FAILURE;

	} else {
	    if (g95_resolve_iterator(&d->iter) == FAILURE)
		return FAILURE;

	    frame.iterator = d->iter.var->symbol;
	    frame.prev = data_root;

	    for(p=data_root; p; p=p->prev)
		if (frame.iterator == p->iterator) {
		    g95_error("Duplicate DATA iterator '%s' at %L",
			      frame.iterator->name, &d->iter.var->where);
		    return FAILURE;
		}

	    if (check_data_element_index(d->iter.start) == FAILURE ||
		check_data_element_index(d->iter.end) == FAILURE ||
		(d->iter.step != NULL &&
		 check_data_element_index(d->iter.step) == FAILURE))
		return FAILURE;

	    data_root = &frame;
	    t = resolve_data_variables(d->list);
	    data_root = frame.prev;

	    if (t == FAILURE)
		return FAILURE;
	}
    }

    return SUCCESS; 
}



/* resolve_data()-- Resolve a single DATA statement. */

static try resolve_data(g95_data *d) {
g95_data_value *val;

    if (resolve_data_variables(d->var) == FAILURE)
	return FAILURE;

    for(val=d->value; val; val=val->next) {
	if (g95_resolve_expr(val->expr) == FAILURE)
	    return FAILURE;

	switch(val->expr->type) {
	case EXPR_OP:     case EXPR_FUNCTION:    case EXPR_VARIABLE:
	case EXPR_ARRAY:  case EXPR_PROCEDURE:
	    g95_error("Illegal expression for DATA value at %L", &d->where);
	    return FAILURE;

	default:
	    break;
	}
    }

    return SUCCESS;
}



/* resolve_initial_values()-- Resolve initial values and make sure
 * they are compatible with the variable */

static try resolve_initial_values(g95_symtree *st) {
g95_symbol *sym;

    if (st == NULL)
	return SUCCESS;

    if (resolve_initial_values(st->left) == FAILURE ||
	resolve_initial_values(st->right) == FAILURE)
	return FAILURE;

    sym = st->n.sym;
    if (sym->value == NULL || sym->attr.use_assoc)
	return SUCCESS;

    if (g95_resolve_expr(sym->value) == FAILURE)
	return FAILURE;

    if (sym->attr.dummy) {
	g95_error("Dummy argument '%s' at %L cannot have an initialization",
		  sym->name, &sym->value->where);
	return FAILURE;
    }

    if (sym->attr.result_var) {
	g95_error("RESULT variable '%s' at %L cannot have an initialization",
		  sym->name, &sym->value->where);
	return FAILURE;
    }

    if (sym->attr.allocatable) {
	g95_error("ALLOCATABLE variable '%s' at %L cannot have an "
		  "initialization", sym->name, &sym->value->where);
	return FAILURE;
    }

    if (resolve_symbol(sym) == FAILURE)
	return FAILURE;

    if (sym->attr.flavor == FL_VARIABLE && automatic_object(sym)) {
	g95_error("Automatic variable '%s' at %L cannot have an "
		  "initialization", sym->name, &sym->value->where);
	return FAILURE;
    }

    return g95_check_assign_symbol(sym, sym->value);
}



/* g95_pure()-- Test whether a symbol is pure or not.  For a NULL
 * pointer, checks the symbol of the current procedure.  If 'flag' is
 * set, we consider functions in F to be pure as well. */

int g95_pure(g95_symbol *sym, int flag) {
symbol_attribute attr;

    if (sym == NULL)
	sym = g95_current_ns->proc_name;

    if (sym == NULL)
	return 0;

    attr = sym->attr;

    if (flag && attr.function && G95_STRICT_F())
	return 1;

    return attr.flavor == FL_PROCEDURE && (attr.pure || attr.elemental);
}



/* g95_elemental()-- Test whether the current procedure is elemental or not */

int g95_elemental(g95_symbol *sym) {
symbol_attribute attr;

    if (sym == NULL)
	sym = g95_current_ns->proc_name;

    if (sym == NULL)
	return 0;

    attr = sym->attr;

    return attr.flavor == FL_PROCEDURE && attr.elemental;
}



/* g95_impure_variable()-- Determines if a variable is not 'pure', ie
 * not assignable within a pure procedure.  Returns zero if assignment
 * is OK, nonzero if there is a problem. */

int g95_impure_variable(g95_symbol *sym) {

    if (sym->attr.dummy)
	return sym->ns->proc_name->attr.function
	    ? 1
	    : (sym->attr.intent == INTENT_IN);

    if (sym->attr.use_assoc || sym->attr.in_common)
	return 1;

    if (sym->attr.function)
	return 0;

  /* TODO: Check storage association through EQUIVALENCE statements */

    return 0;
}



/* warn_unused_label()-- Warn about unused labels. */

static void warn_unused_label(g95_namespace *ns){
g95_st_label *l;

    l = ns->st_labels;
    if (l == NULL)
	return;

    while(l->next)
	l = l->next;
  
    for(; l; l=l->prev) {
	if (l->defined == ST_LABEL_UNKNOWN)
	    continue;

	switch(l->referenced){
	case ST_LABEL_UNKNOWN:
	    g95_warning(110, "Label %d at %L defined but not used",
			l->value, &l->where);
	    break;

	case ST_LABEL_BAD_TARGET:
	    g95_warning(111, "Label %d at %L defined but cannot be used",
			l->value, &l->where);
	    break;

	default:
	    break;
	}
    }
}



/* check_usage()-- Check the usage of a symbol, issuing warnings about
 * using/setting. */

static void check_usage(g95_symbol *sym) {
int zero_size;
bignum n;

    if (sym->as == NULL)
	zero_size = 0;

    else {
	n = g95_array_spec_size(sym->as);
	zero_size = (n != NULL && bi_is_zero(n));
    }

    if (g95_option.unused_parameter && sym->attr.flavor == FL_PARAMETER &&
	!sym->attr.used && !sym->attr.use_assoc &&
	(sym->ns->state != COMP_MODULE ||
	 g95_symbol_access(sym) == ACCESS_PRIVATE)) {
	g95_warning(159, "PARAMETER '%s' at %L is never used",
		    sym->name, &sym->declared_at);
	return;
    }

    if (g95_option.unused_types && sym->attr.flavor == FL_DERIVED &&
	sym->attr.use_assoc && !sym->attr.hidden && !sym->attr.used) {
	g95_warning(161, "TYPE '%s' at %L is never used", sym->name,
		    &sym->declared_at);
	return;
    }

    if (sym->attr.flavor != FL_VARIABLE || sym->attr.in_common ||
	sym->attr.restore || sym->attr.equivalenced || sym->attr.result_var)
	return;

    if (sym->attr.use_assoc) {
	if (g95_option.unused_module_vars && !sym->attr.used && !sym->attr.set
	    && !sym->attr.alloced && !sym->attr.dealloced && !sym->attr.hidden)
	    g95_warning(136, "Module variable '%s' at %L is never used",
			sym->name, &sym->declared_at);

	if (g95_option.unused_vars && sym->attr.only &&
	    !sym->attr.used && !sym->attr.set && !sym->attr.alloced &&
	    !sym->attr.dealloced)
	    g95_warning(109, "ONLY variable '%s' USE-d at %L is never used "
			"or set", sym->name, &sym->declared_at);
	return;
    }

    if (!g95_option.unused_vars && !g95_option.unset_vars)
	return;

    if (sym->ns->state == COMP_MODULE &&
	(g95_symbol_access(sym) != ACCESS_PRIVATE || sym->value != NULL))
	return;

    if (!sym->attr.set && !sym->attr.used &&
	(g95_option.unused_vars || g95_option.unset_vars)) {
	if (!sym->attr.st_construct && !sym->attr.st_construct0)
	    g95_warning(137, "Variable '%s' at %L is never used and never set",
			sym->name, &sym->declared_at);

    } else if (!sym->attr.used && sym->attr.set && g95_option.unused_vars &&
	       sym->attr.intent != INTENT_IN) {
	g95_warning(112, "Variable '%s' at %L is set but never used",
		    sym->name, &sym->declared_at);

    } else if (!sym->attr.set && sym->attr.used && g95_option.unset_vars &&
	       !sym->attr.st_construct && !sym->attr.st_construct0 &&
	       !sym->attr.allocatable && !sym->attr.pointer &&
	       sym->attr.intent != INTENT_OUT && !zero_size) {
	g95_warning(113, "Variable '%s' at %L is used but not set", sym->name,
		    &sym->declared_at);

    } else if (sym->attr.allocatable && sym->attr.set && !sym->attr.alloced) {
	g95_warning(146, "Variable '%s' at %L is set and never allocated",
		    sym->name, &sym->declared_at);

    } else if (sym->attr.allocatable && sym->attr.used && !sym->attr.alloced) {
	g95_warning(147, "Variable '%s' at %L is used and never allocated",
		    sym->name, &sym->declared_at);

    } else if (sym->attr.target && !sym->attr.targetted &&
	       !sym->attr.use_assoc && g95_option.unused_target)
	g95_warning(157, "Variable '%s' at %L has the TARGET attribute, "
		    "but is never pointed to", sym->name, &sym->declared_at);
}



/* check_formal_usage()-- Check the formal arguments of a procedure */

static void check_formal_usage(g95_namespace *ns) {
g95_formal_arglist *f;
g95_symbol *sym;

    if (ns->state != COMP_SUBROUTINE && ns->state != COMP_FUNCTION)
	return;

    for(f=ns->proc_name->formal; f; f=f->next) {
	sym = f->sym;

	if (sym != NULL && sym->attr.dummy && sym->attr.intent == INTENT_OUT &&
	    !sym->attr.optional && sym->attr.flavor == FL_VARIABLE &&
	    (!sym->attr.set ||
	     (sym->attr.pointer && !sym->attr.alloced && !sym->attr.set)))
	    g95_warning(158, "INTENT(OUT) variable '%s' at %L is never set",
			sym->name, &sym->declared_at);
    }
}



/* check_modproc_usage()-- Check that module procedures that are
 * use-associated are referenced. */

static void check_modproc_usage(g95_symbol *sym) {

    if (sym->attr.proc == PROC_MODULE && sym->attr.use_assoc &&
	!sym->attr.used && !sym->attr.invoked && sym->refs == 1 &&
	!sym->attr.hidden)
	g95_warning(102, "MODULE PROCEDURE '%s' USEd at %L is not referenced",
		    sym->name, &sym->declared_at);
}



/* is_default_kind()-- Return nonzero if the kind of a type is the
 * default kind.  Derived types return zero. */

static int is_default_kind(g95_typespec *ts) {

    switch(ts->type) {
    case BT_INTEGER:
	if (ts->kind == g95_default_integer_kind(1))
	    return 1;

	break;

    case BT_REAL:
    case BT_COMPLEX:
	if (ts->kind == g95_default_real_kind(1) ||
	    ts->kind == g95_default_double_kind())
	    return 1;

	break;

    case BT_LOGICAL:
	if (ts->kind == g95_default_logical_kind())
	    return 1;

	break;

    case BT_CHARACTER:
	if (ts->kind == g95_default_character_kind())
	    return 1;

	break;

    default:
	break;
    }

    return 0;
}



/* character_sequence_type()-- Return nonzero if the type is a numeric
 * sequence derived type. */

static int character_sequence_type(g95_typespec *ts) {
g95_component *c;
g95_symbol *sym;

    if (ts->type != BT_DERIVED)
	return 0;

    sym = ts->derived;
    if (!sym->attr.sequence)
	return 0;

    for(c=sym->components; c; c=c->next)
	if (c->pointer || c->ts.type != BT_CHARACTER ||
	    !is_default_kind(&c->ts))
	    return 0;
    
    return 1;
}



/* numeric_sequence_type()-- Return nonzero if the type is a numeric
 * sequence derived type. */

static int numeric_sequence_type(g95_typespec *ts) {
g95_component *c;
g95_symbol *sym;

    if (ts->type != BT_DERIVED)
	return 0;

    sym = ts->derived;
    if (!sym->attr.sequence)
	return 0;

    for(c=sym->components; c; c=c->next) {
	if (c->pointer)
	    return 0;

	ts = &c->ts;
	if ((ts->type != BT_INTEGER && ts->type != BT_REAL &&
	     ts->type != BT_COMPLEX && ts->type != BT_LOGICAL) ||
	    !is_default_kind(ts))
	    return 0;
    }

    return 1;
}


/* get_eq_class()-- Give a typespec, determine the equivalence class */

static void get_eq_class(g95_typespec *ts, eq_class *class) {

    class->ts = *ts;

    switch(ts->type) {
    case BT_CHARACTER:
	class->type = EQ_CHARACTER;
	break;

    case BT_REAL:
	if (ts->kind == g95_default_double_kind()) {
	    class->type = EQ_NUMERIC;
	    break;
	}

	/* Fall through */

    case BT_INTEGER:  case BT_COMPLEX:  case BT_LOGICAL:
	class->type = is_default_kind(ts)
	    ? EQ_NUMERIC
	    : EQ_SPECIFIC;

	break;

    case BT_DERIVED:
	if (numeric_sequence_type(ts)) {
	    class->type = EQ_NUMERIC;
	    break;
	}

	if (character_sequence_type(ts)) {
	    class->type = EQ_CHARACTER;
	    break;
	}

	class->type = EQ_SPECIFIC;
	break;

    default:
	g95_internal_error("get_eq_class(): Bad type");
    }
}



/* compare_eq_class()-- Compare equivalence classes.  Returns nonzero
 * if not equal. */

static int compare_eq_class(eq_class *a, eq_class *b) {

    if (a->type != b->type)
	return 1;

    if (a->type != EQ_SPECIFIC)
	return 0;

    if (a->ts.type != b->ts.type)
	return 1;

    return (a->ts.type == BT_DERIVED)
	? a->ts.derived != b->ts.derived
	: a->ts.kind != b->ts.kind;
}



/* resolve_equivalenced_var()-- Resolve equivalenced variables and
 * check the additional restrictions that they must satisfy.  Because
 * of the slack way that these expressions were parsed, we can't use
 * the main resolution, but the number of things that can go wrong are
 * small. */

static try resolve_equivalenced_var(g95_expr *e, int in_common) {
g95_symbol *sym;
g95_ref *ref;

    sym = e->symbol;
    if (resolve_symbol(sym) == FAILURE)
	return FAILURE;

    e->rank = 0;
    e->ts.type = sym->ts.type;
    e->ts.kind = sym->ts.kind;

    if (e->ts.type == BT_DERIVED)
	e->ts.derived = sym->ts.derived;

    if (sym->attr.dummy) {
	g95_error("Dummy variable '%s' at %L cannot be EQUIVALENCEd",
		  sym->name, &e->where);
	return FAILURE;
    }

    if (sym->attr.flavor == FL_PARAMETER) {
	g95_error("PARAMETER '%s' at %L cannot be EQUIVALENCEd",
		  sym->name, &e->where);
	return FAILURE;
    }

    if (sym->attr.allocatable) {
	g95_error("Allocatable array '%s' at %L cannot be EQUIVALENCEd",
		  sym->name, &e->where);
	return FAILURE;
    }

    if (sym->attr.pointer) {
	g95_error("Pointer variable '%s' at %L cannot be EQUIVALENCEd",
		  sym->name, &e->where);
	return FAILURE;
    }

    if (sym->ts.type == BT_DERIVED) {
	if (!sym->ts.derived->attr.sequence) {
	    g95_error("Derived variable '%s' in EQUIVALENCE at %L must be a "
		      "SEQUENCE type", sym->name, &e->where);
	    return FAILURE;
	}

	if (g95_pointer_component(sym->ts.derived)) {
	    g95_error("Derived variable '%s' in EQUIVALENCE at %L contains a "
		      "pointer component", sym->name, &e->where);
	    return FAILURE;
	}

	if (g95_derived_init(sym->ts.derived) && in_common) {
	    g95_error("Derived variable '%s' in EQUIVALENCE at %L contains a "
		      "component with a default initialization in a COMMON "
		      "block", sym->name, &e->where);
	    return FAILURE;
	}
    }

    if (sym->attr.function) {
	g95_error("Function '%s' at %L cannot be EQUIVALENCEd", sym->name,
		  &e->where);
	return FAILURE;
    }

    if (sym->attr.result_var) {
	g95_error("RESULT variable '%s' at %L cannot be EQUIVALENCEd",
		  sym->name, &e->where);
	return FAILURE;
    }

    if (sym->attr.entry) {
	g95_error("ENTRY name '%s' at %L cannot be EQUIVALENCEd", sym->name,
		  &e->where);
	return FAILURE;
    }

    /* Parameters are already taken care of */

    if (sym->attr.target) {
	g95_error("TARGET variable '%s' at %L cannot be EQUIVALENCEd",
		  sym->name, &e->where);
	return FAILURE;
    }

    ref = e->ref;

    if (ref != NULL && ref->type == REF_ARRAY) {
	if (sym->as == NULL) {
	    g95_error("Unexpected array reference in EQUIVALENCE at %L",
		      &ref->where);
	    return FAILURE;
	}

	if (compare_spec_to_ref(&ref->u.ar, sym->as, &ref->where) == FAILURE)
	    return FAILURE;

	ref = ref->next;
    }

    if (ref != NULL) {
	if (sym->ts.type != BT_CHARACTER) {
	    g95_error("Unexpected substring reference in EQUIVALENCE at %L",
		      &ref->where);
	    return FAILURE;
	}

	if (ref->u.ss.start != NULL && ref->u.ss.end != NULL &&
	    bi_compare(ref->u.ss.end->value.integer,
		       ref->u.ss.start->value.integer) < 0) {
	    g95_error("Zero-length substring at %L is not allowed in an "
		      "EQUIVALENCE", &ref->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* resolve_equivalence()-- Validate a single equivalence by making
 * sure that if one of a set of equivalent variables is in a common
 * block, the rest are not in another block. */

static try resolve_equivalence(g95_equiv *e) {
g95_symbol *sym;
eq_class a, b;
g95_equiv *f;
int flag;

    flag = -1;

    for(f=e; f; f=f->eq) {
	if (resolve_equivalenced_var(f->expr, f->common) == FAILURE)
	    return FAILURE;

	if (flag == -1)
	    flag = f->expr->symbol->attr.use_assoc;

	else if (flag != f->expr->symbol->attr.use_assoc) {
	    sym = flag ? e->expr->symbol : f->expr->symbol;
	    g95_error("Non-module symbol '%s' at %L cannot be EQUIVALENCEd "
		      "with non-module symbols", sym->name, &sym->declared_at);
	    return FAILURE;
	}
    }

    /* Check type restrictions */

    get_eq_class(&e->expr->ts, &a);

    for(f=e->eq; f; f=f->eq) {
	get_eq_class(&f->expr->ts, &b);

	if (!compare_eq_class(&a, &b))
	    continue;

	if (g95_option.fmode == 0)
	    g95_warning(101, "EQUIVALENCE-ing variable '%s' at %L "
			"with '%s' at %L is nonstandard",
			e->expr->symbol->name, &e->expr->where,
			f->expr->symbol->name, &f->expr->where);
	else {
	    g95_error("EQUIVALENCE-ing variable '%s' at %L with '%s' at %L is "
		      "nonstandard",
		      e->expr->symbol->name, &e->expr->where,
		      f->expr->symbol->name, &f->expr->where);

	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* resolve_equivalences()-- Resolve all equivalences in a namespace */

static try resolve_equivalences(g95_namespace *ns) {
g95_equiv *f, *g, *a, *b;

    /* Find individual equivalences in common blocks */ 

    for(f=ns->equiv; f; f=f->next)
	for(g=f; g; g=g->eq)
	    if (g->expr->symbol->attr.in_common) {
		f->common = 1;
		break;
	    }

    /* Flood fill the common flag to other equivalences */

start:
    for(a=ns->equiv; a; a=a->next)
	if (a->common)
	    for(f=ns->equiv; f; f=f->next)
		if (!f->common)
		    for(b=a; b; b=b->eq)
			for(g=f; g; g=g->eq)
			    if (b->expr->symbol == g->expr->symbol) {
				f->common = 1;
				goto start;
			    }

    for(f=ns->equiv; f; f=f->next)
	if (resolve_equivalence(f) == FAILURE)
	    return FAILURE;

    return SUCCESS;
}



/* resolve_uop()-- Resolve user operators. */

static try resolve_uop(g95_symtree *st) {
g95_access access;
g95_interface *p;

    if (st == NULL)
	return SUCCESS;

    if (resolve_uop(st->left) == FAILURE ||
	resolve_uop(st->right) == FAILURE)
	return FAILURE;

    access = st->n.uop->access;

    for(p=st->n.uop->operator; p; p=p->next) {
	if (access != ACCESS_PRIVATE)
	    p->sym->attr.invoked = 1;

	if (!p->sym->attr.function) {
	    g95_error("User operator procedure '%s' at %L must be a FUNCTION",
		      p->sym->name, &p->sym->declared_at);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* mark_intrinisc_operators()-- Mark the specifics of intrinsic
 * operators as invoked. */

static void mark_intrinsic_operators(g95_namespace *ns) {
g95_interface *p;
int i;

    for(i=0; i<G95_INTRINSIC_OPS; i++)
	if (ns->operator_access[i] == ACCESS_PUBLIC ||
	    ns->default_access == ACCESS_PUBLIC)
	    for(p=ns->operator[i]; p; p=p->next)
		p->sym->attr.invoked = 1;
}



/* g95_resolve()-- This function is called after a complete program
 * unit has been compiled.  Its purpose is to examine all of the
 * expressions associated with a program unit, assign types to all
 * intermediate expressions, make sure that all assignments are to
 * compatible types and figure out which names refer to which
 * functions or subroutines. */

try g95_resolve(g95_namespace *ns) {
g95_namespace *old_ns, *n;
code_stack *cs_save;
g95_common_head *h;
g95_symbol *sym;
g95_charlen *cl;
g95_data *d;
int count;
try t;

    old_ns = g95_current_ns;
    g95_current_ns = ns;

    resolve_contained_functions(ns);
    g95_get_errors(NULL, &count);
    if (count != 0)
	return FAILURE;

    t = FAILURE;

    for(n=ns->contained; n; n=n->sibling) {
	if (g95_pure(ns->proc_name, 0) && n->proc_name != NULL &&
	    !g95_pure(n->proc_name, 0))
	    g95_error("Contained procedure '%s' at %L of a PURE procedure "
		      "must also be PURE", n->proc_name->name,
		      &n->proc_name->declared_at);

	if (g95_resolve(n) == FAILURE)
	    goto done;
    }

    g95_forall_flag = 0;
    g95_check_interfaces(ns);

    if (resolve_initial_values(ns->sym_root) == FAILURE)
	goto done;

    if (ns->save_all)
	save_commons(ns->common_root);

    for(d=ns->data; d; d=d->next)
	if (resolve_data(d) == FAILURE)
	    goto done;

    if (resolve_common_block(ns->common_root) == FAILURE)
	goto done;

    for(h=ns->blank_common; h; h=h->next)
	for(sym=h->head; sym; sym=sym->common_next)
	    resolve_common_var(sym, 1);

    if (resolve_equivalences(ns) == FAILURE)
	goto done;

    init_check_entry(ns);

    cs_save = cs_base;
    cs_base = NULL;
    t = resolve_code(ns->code, ns);
    cs_base = cs_save;

    if (t == FAILURE)
	goto done;

    if (resolve_symbols(ns->sym_root) == FAILURE)
	goto done;

    if (g95_option.unused_label)
	warn_unused_label(ns);

    if ((g95_option.unused_vars || g95_option.unset_vars ||
	 g95_option.unused_types || g95_option.unused_module_vars ||
	 g95_option.unused_parameter) &&
	(ns->state == COMP_SUBROUTINE || ns->state == COMP_FUNCTION ||
	 ns->state == COMP_PROGRAM || ns->state == COMP_MODULE))
	g95_traverse_ns(ns, check_usage);

    check_formal_usage(ns);

    if (resolve_proc(ns) == FAILURE)
	goto done;

    for(cl=ns->cl_list; cl; cl=cl->next)
	if (resolve_charlen(cl) == FAILURE)
	    goto done;

    if (g95_option.unused_module_procs)
	g95_traverse_ns(ns, check_modproc_usage);

    resolve_common_block1(ns->common_root);

    if (g95_resolve_entry(ns) == FAILURE)
	goto done;

    if (resolve_uop(ns->uop_root) == FAILURE)
	goto done;

    mark_intrinsic_operators(ns);

    if (check_generic(ns->generic_root) == FAILURE)
	goto done;

    for(n=ns->contained; n; n=n->sibling) {
	sym = n->proc_name;

	if (g95_option.unused_internal_procs && sym != NULL &&
	    !sym->attr.invoked && sym->attr.proc == PROC_INTERNAL)
	    g95_warning(166, "Internal procedure '%s' at %L is never used",
			sym->name, &sym->declared_at);
    }

    if (ns->state == COMP_MODULE && g95_option.unused_module_procs) {
	for(n=ns->contained; n; n=n->sibling) {
	    sym = n->proc_name;
	    if (sym == NULL)
		continue;

	    if (!sym->attr.invoked && g95_symbol_access(sym) == ACCESS_PRIVATE)
		g95_warning(167, "PRIVATE module procedure '%s' at %L is "
			    "never invoked", sym->name, &sym->declared_at);
	}
    }

    if (G95_STRICT_F() && ns->state == COMP_MODULE)
	for(n=ns->contained; n; n=n->sibling)
	    if (n->proc_name != NULL &&
		n->proc_name->attr.access == ACCESS_UNKNOWN) {
		g95_error("Module procedure '%s' at %L has no access "
			  "specification",
			  n->proc_name->name, &n->proc_name->declared_at);
		break;
	    }

    t = SUCCESS;

done:
    g95_current_ns = old_ns;
    return t;
}

