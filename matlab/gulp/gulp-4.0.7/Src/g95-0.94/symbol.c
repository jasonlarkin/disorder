/* Symbol handling
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

/* symbol.c-- Maintains binary trees of symbols */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "g95.h"

g95_namespace *g95_current_ns;

static g95_symbol *changed_syms = NULL;
static g95_symtree *changed_st = NULL;
static char bad_letter;
g95_gsymbol *g95_gsym_root = NULL;

/* The following static variables hold the default types set by
 * IMPLICIT statements.  We have to store kind information because of
 * IMPLICIT DOUBLE PRECISION statements.  IMPLICIT NONE stores a
 * BT_UNKNOWN into all elements.  The arrays of flags indicate whether
 * a particular element has been explicitly set or not.  */

static g95_typespec new_ts[G95_LETTERS];
static int namelist_proc_override=0;

typedef struct var_stack {
    g95_symbol *old, *new;
    struct var_stack *next;
} var_stack;



/* Basic overview: Fortran 95 requires a potentially unlimited number
 * of distinct namespaces when compiling a program unit.  This case
 * occurs during a compilation of internal subprograms because all of
 * the internal subprograms must be read before we can start
 * generating code for the host.
 * 
 * Given the tricky nature of the fortran grammar, we must be able to
 * undo changes made to a symbol table if the current interpretation
 * of a statement is found to be incorrect.  Whenever a symbol is
 * looked up, we make a copy of it and link to it.  All of these
 * symbols are kept in a singly linked list so that we can commit or
 * undo the changes at a later time. 
 *
 * A symtree may point to a symbol node outside of it's namespace.  In
 * this case, that symbol has been used as a host associated variable
 * at some previous time.  */



/* g95_match_implicit_none()-- Match an IMPLICIT NONE statement. */

match g95_match_implicit_none(void) {
g95_compile_state state;
match m;

    m = (g95_match_eos() == MATCH_YES) ? MATCH_YES : MATCH_NO;

    if (m == MATCH_YES && G95_STRICT_F()) {
	g95_enclosing_unit(&state);

	if (state != COMP_MODULE && state != COMP_PROGRAM) {
	    g95_error("IMPLICIT NONE statement at %C only permitted in MODULE "
		      "or PROGRAM units in F mode");
	    m = MATCH_ERROR;
	}
    }

    return m;
}



/* g95_set_implicit_none()-- Handle a correctly parsed IMPLICIT NONE */

void g95_set_implicit_none(void) {
int i;

    if (g95_current_ns->seen_implicit_none) {
	g95_error("Duplicate IMPLICIT NONE statement at %C");
	return;
    }

    g95_current_ns->seen_implicit_none = 1;

    for(i='a'; i<='z'; i++) {
	g95_clear_ts(&g95_current_ns->default_type[i - 'a']);
	g95_current_ns->set_flag[i - 'a'] = 1;
    }
}



/* match_implicit_range()-- Match the letter range(s) of an IMPLICIT
 * statement.  The new_flag[] array is updated. */

static match match_implicit_range(g95_typespec *ts) {
int c, i, c1, c2, flag;
g95_locus cur_loc;

    cur_loc = g95_current_locus; 

    g95_gobble_whitespace();
    c = g95_next_char();
    if (c != '(') {
	g95_error("Missing character range in IMPLICIT at %C");
	goto bad;
    }

    flag = 1;
    while(flag) {
	g95_gobble_whitespace();
	c1 = c2 = g95_next_char();

	if (!g95_varchar(c1, 1))
	    goto bad;

	g95_gobble_whitespace();
	c = g95_next_char();

	switch(c) {
	case ')':
	    flag = 0;
	    break;

	case ',':
	    break;

	case '-':
	    g95_gobble_whitespace();
	    c2 = g95_next_char();
	    if (!g95_varchar(c2, 1))
		goto bad;

	    g95_gobble_whitespace();
	    c = g95_next_char();

	    if (c != ',' && c != ')')
		goto bad;

	    if (c == ')')
		flag = 0;

	    break;

	default:
	    goto bad;
	}

	if (c1 > c2) {
	    g95_error("Letters must be in alphabetic order in IMPLICIT "
		      "statement at %C");
	    goto bad;
	}

	c1 -= 'a'; 
	c2 -= 'a';

	for(i=c1; i<=c2; i++) {
	    if (new_ts[i].type != BT_UNKNOWN || g95_current_ns->set_flag[i])
		bad_letter = i + 'A';

	    else
		new_ts[i] = *ts;

	    /* Don't signal an error until the whole statement has
	     * been correctly matched.  The upper layer regards errors
	     * here as syntax errors, which this not. */
	}
    }

    return MATCH_YES;

bad:
    g95_syntax_error(ST_IMPLICIT);

    g95_current_locus = cur_loc;
    return MATCH_ERROR;
}



/* g95_match_implicit()-- Match an IMPLICIT statement, storing the
 * types for g95_set_implicit() if the statement is accepted by the
 * parser.  There is a strange looking, but legal syntactic
 * construction possible.  It looks like
 *                  IMPLICIT INTEGER (a-b) (c-d)
 *
 * This is legal if "a-b" is a constant expression that happens to
 * equal one of the legal kinds for integers.  The real problem
 * happens with an implicit specification that looks like
 *                  IMPLICIT INTEGER (a-b)
 *
 * In this case, a typespec matcher that is "greedy" (as most of the
 * matchers are) gobbles the character range as a kindspec, leaving
 * nothing left.  We therefore have to go a bit more slowly in the
 * matching process by inhibiting the kindspec checking during
 * typespec matching and checking for a kind later. */

match g95_match_implicit(void) {
g95_typespec ts, save_ts[G95_LETTERS];
g95_locus cur_loc;
int c, i;
match m;

    for(i=0; i<G95_LETTERS; i++)
	g95_clear_ts(&new_ts[i]);

    if (g95_match_eos() == MATCH_YES) {
	g95_error("Empty IMPLICIT statement at %C");
	return MATCH_ERROR;
    }

    bad_letter = ' ';

    do {
	cur_loc = g95_current_locus;

	m = g95_match_type_spec(&ts, 0); /* A basic type is mandatory here */
	if (m == MATCH_ERROR) goto error;
	if (m == MATCH_NO) goto syntax;

	for(i=0; i<G95_LETTERS; i++)
	    save_ts[i] = new_ts[i];

	m = match_implicit_range(&ts);
	if (m == MATCH_YES) {   /* Looks like we have the <TYPE> (<RANGE>) */
	    g95_gobble_whitespace();
	    c = g95_next_char();
	    if (c == '\n' || c == ',')
		continue;
	}

	for(i=0; i<G95_LETTERS; i++)
	    new_ts[i] = save_ts[i];

	/* Last chance-- check <TYPE> (<KIND>) (<RANGE>) */

	g95_current_locus = cur_loc;
	m = g95_match_type_spec(&ts, 1);
	if (m == MATCH_ERROR) goto error;
	if (m == MATCH_NO) goto syntax;

	m = match_implicit_range(&ts);
	if (m == MATCH_ERROR) goto error;
	if (m == MATCH_NO) goto syntax;

	g95_gobble_whitespace();
	c = g95_next_char();
	if ((c != '\n') && (c != ','))
	    goto syntax;

    } while(c == ',');

    /* Statement syntax is correct */

    if (bad_letter != ' ') {
	g95_error("Letter '%c' already set in IMPLICIT statement at %C",
		  bad_letter);
	return MATCH_ERROR;
    }

    if (G95_STRICT_F()) {
	g95_error("IMPLICIT statement at %C not permitted in F");
	return MATCH_ERROR;
    }

    return MATCH_YES;

syntax:
    g95_syntax_error(ST_IMPLICIT);

error:
    return MATCH_ERROR;
}



/* g95_set_implicit()-- Sets the implicit types parsed by the previous
 * g95_match_implicit(). */

void g95_set_implicit(void) {
int i;

    for(i=0; i<G95_LETTERS; i++)
	if (new_ts[i].type != BT_UNKNOWN) {
	    g95_current_ns->default_type[i] = new_ts[i];
	    g95_current_ns->set_flag[i] = 1;
	}
}



/* g95_set_default_type()-- Given a pointer to a symbol, set its type
 * according to the first letter of its name.  Fails if the letter in
 * question has no default type. */

try g95_set_default_type(g95_symbol *sym, int error_flag, g95_namespace *ns) {
g95_typespec *ts;

    if (sym->ts.type != BT_UNKNOWN)
	g95_internal_error("g95_set_default_type(): "
			   "symbol already has a type");

    ts = g95_get_default_type(sym, ns);

    if (ts->type == BT_UNKNOWN) {
	if (error_flag && !sym->attr.untyped) {
	    g95_error("Symbol '%s' at %L has no IMPLICIT type", sym->name,
		      &sym->declared_at);

	    sym->attr.untyped = 1;
	}

	return FAILURE;
    }

    sym->ts = *ts;
    sym->attr.implicit_type = 1;

    if (sym->ts.type == BT_CHARACTER && sym->ts.cl->length == NULL)
	sym->ts.cl = g95_get_charlen(sym->ns);

    return SUCCESS;
}



/* g95_get_default_type()-- Given a symbol, return a pointer to the
 * typespec for it's default type */

g95_typespec *g95_get_default_type(g95_symbol *sym, g95_namespace *ns) {
char letter;

    letter = *sym->name;
    if (letter < 'a' || letter > 'z')
	g95_internal_error("g95_get_default_type(): Bad symbol");

    if (ns == NULL)
	ns = g95_current_ns;

    return &ns->default_type[letter - 'a'];
}



/* g95_assign_boz()-- Given the LHS type and the RHS, convert a BOZ
 * constant to the correct target bit pattern. */

g95_expr *g95_assign_boz(g95_typespec *ts, g95_expr *e) {
g95_expr *result;
char mem[100];
g95_ff *ff;

    if (e->type != EXPR_CONSTANT || e->ts.type != BT_INTEGER ||
	!e->value.integer->typeless)
	return NULL;

    if (g95_option.fmode == 95 && ts->type != BT_INTEGER)
	g95_warning(160, "BOZ constant conversion at %L is nonstandard",
		    &e->where);

    memset(mem, '\0', sizeof(mem));
    g95_pack_int(e->value.integer, ts->kind, mem);

    result = g95_constant_result(ts->type, ts->kind, &e->where);

    switch(ts->type) {
    case BT_INTEGER:
	result->value.integer = g95_unpack_int(mem, ts->kind);
	big_permanent(result->value.integer);
	break;

    case BT_REAL:
	result->value.real = g95_unpack_real(mem, g95_get_ff(ts->kind));
	big_permanent(result->value.real);
	break;

    case BT_COMPLEX:
	ff = g95_get_ff(ts->kind);
	result->value.complex.r = g95_unpack_real(mem, ff);
	big_permanent(result->value.complex.r);

	result->value.complex.i = g95_unpack_real(mem + ts->kind, ff);
	big_permanent(result->value.complex.i);
	break;

    case BT_LOGICAL:
	result->value.logical = g95_unpack_logical(mem, ts->kind);
	break;

    case BT_CHARACTER:
	result->value.character.string = g95_getmem(ts->kind);
	result->value.character.length = ts->kind;

	memcpy(result->value.character.string, mem, ts->kind);
	break;

    default:
	g95_internal_error("g95_assign_boz(): Bad type");
    }

    return result;
}



/* g95_check_pointer_assign()-- Check that a pointer assignment is OK.
 * We first check lvalue, and we only check rvalue if it's not an
 * assignment to NULL() or a NULLIFY statement. */

try g95_check_pointer_assign(g95_expr *lvalue, g95_expr *rvalue) {
g95_symbol *sym;
g95_ref *ref;
int is_pure;

    if (lvalue->symbol->ts.type == BT_UNKNOWN) {
	g95_error("Pointer assignment target is not a POINTER at %L",
		  &lvalue->where);
	return FAILURE;
    }

    if (!g95_pointer_expr(lvalue)) {
	g95_error("Pointer assignment to non-POINTER at %L",
		  &lvalue->where);
	return FAILURE;
    }

    for(ref=lvalue->ref; ref; ref=ref->next)
	if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION) {
	    g95_error("Pointer assignment of array section at %L is an F2003 "
		      "feature", &ref->where);
	    return FAILURE;
	}

    sym = lvalue->symbol;

    if (sym->attr.intent == INTENT_IN) {
	g95_error("Can't assign to INTENT(IN) pointer '%s' at %L",
		  sym->name, &lvalue->where);
	return FAILURE;
    }

    is_pure = g95_pure(NULL, 1);

    if (is_pure && g95_impure_variable(lvalue->symbol)) {
	g95_error("Bad pointer object in PURE procedure at %L",
		  &lvalue->where);
	return FAILURE;
    }

    if (rvalue->ts.type != BT_UNKNOWN) {
	if (!g95_compare_types(&lvalue->ts, &rvalue->ts)) {
	    g95_error("Different types in pointer assignment at %L",
		      &lvalue->where);
	    return FAILURE;
	}

	if (lvalue->ts.kind != rvalue->ts.kind) {
	    g95_error("Different kind type parameters in pointer assignment "
		      "at %L", &lvalue->where);
	    return FAILURE;
	}

	if (rvalue->ts.type == BT_CHARACTER && rvalue->ts.cl != NULL &&
	    rvalue->ts.cl->length != NULL &&
	    rvalue->ts.cl->length->type == EXPR_CONSTANT &&
	    lvalue->ts.cl != NULL && lvalue->ts.cl->length != NULL &&
	    lvalue->ts.cl->length->type == EXPR_CONSTANT &&
	    bi_compare(lvalue->ts.cl->length->value.integer,
		       rvalue->ts.cl->length->value.integer) != 0) {
	    g95_error("Different lengths in character pointer assignment "
		      "at %L", &rvalue->where);
	    return FAILURE;
	}
    }

    if (rvalue->type == EXPR_PROCEDURE) {
	sym = rvalue->symbol;
	if (sym->attr.abstract) {
	    g95_error("Cannot point to ABSTRACT interface '%s' at %L",
		      sym->name, &rvalue->where);
	    return FAILURE;
	}
    }

    if (rvalue->type == EXPR_VARIABLE) {
	sym = rvalue->symbol;

	if (sym->as != NULL && sym->as->type == AS_ASSUMED_SIZE &&
	    rvalue->ref->type == REF_ARRAY &&
	    rvalue->ref->u.ar.type == AR_FULL) {
	    g95_error("Pointer assignment at %L cannot be a full reference to "
		      "an assumed-size array", &rvalue->where);
	    return FAILURE;
	}
    }

    if (rvalue->type == EXPR_NULL)
	g95_resolve_null(rvalue, &lvalue->ts, lvalue->rank);

    if (lvalue->rank != rvalue->rank) {
	g95_error("Unequal ranks in pointer assignment at %L (%d/%d)",
		  &rvalue->where, lvalue->rank, rvalue->rank);
	return FAILURE;
    }

    if (rvalue->type != EXPR_NULL && rvalue->type != EXPR_PROCEDURE) {
	if (!g95_target_expr(rvalue) && !g95_pointer_expr(rvalue)) {
	    g95_error("Pointer assignment target is neither TARGET nor "
		      "POINTER at %L", &rvalue->where);
	    return FAILURE;
	}

	if (is_pure && g95_impure_variable(rvalue->symbol)) {
	    g95_error("Bad target in pointer assignment in PURE procedure "
		      "at %L", &rvalue->where);
	    return FAILURE;
	}

	if (rvalue->rank != 0 && g95_vector_subscripts(rvalue)) {
	    g95_error("Vector subscripts in the target of pointer assignment "
		      "at %L", &rvalue->where);
	    return FAILURE;
	}
    }

    return SUCCESS;
}



/* g95_check_assign_symbol()-- Relative of g95_check_assign() except
 * that the lvalue is a single symbol. */

try g95_check_assign_symbol(g95_symbol *sym, g95_expr *rvalue) {
g95_expr lvalue;
g95_ref ref;

    memset(&lvalue, '\0', sizeof(g95_expr));
    memset(&ref,    '\0', sizeof(g95_ref));

    lvalue.type = EXPR_VARIABLE;
    lvalue.ts = sym->ts;

    if (sym->as) {
	lvalue.rank = sym->as->rank;
	lvalue.ref = &ref;

	ref.type = REF_ARRAY;
	ref.u.ar.type = AR_FULL;
    }

    lvalue.symbol = sym;
    lvalue.where = sym->declared_at;

    return sym->attr.pointer
	? g95_check_pointer_assign(&lvalue, rvalue)
	: g95_check_assign(&lvalue, rvalue);
}



/* g95_iterator_variable()-- Given a symbol that is probably a
 * statement construct, make sure it has a default integer type.
 * Returns nonzero if this is so. */

int g95_iterator_variable(g95_symbol *sym) {
g95_typespec *ts;

    ts = (sym->ts.type == BT_UNKNOWN)
	? &g95_current_ns->default_type[sym->name[0] - 'a']
	: &sym->ts;

    return ts->type == BT_INTEGER;
}



/* g95_check_assign()-- Given an assignable expression and an
 * arbitrary expression, make sure that the assignment can take
 * place. */

try g95_check_assign(g95_expr *lvalue, g95_expr *rvalue) {
g95_symbol *sym;
g95_ref *ref;
g95_expr *e;
int m;

    sym = lvalue->symbol;

    if (sym->attr.intent == INTENT_IN) {
	m = sym->attr.pointer;
	for(ref=lvalue->ref; ref; ref=ref->next)
	    if (ref->type == REF_COMPONENT)
		m |= ref->u.c.component->pointer;

	if (!m) {
	    g95_error("Can't assign to INTENT(IN) variable '%s' at %L",
		      sym->name, &lvalue->where);
	    return FAILURE;
	}
    }

    if (rvalue->rank != 0 && lvalue->rank != rvalue->rank) {
	g95_error("Incompatible ranks in assignment at %L (%d/%d)",
		  &lvalue->where, lvalue->rank, rvalue->rank);
	return FAILURE;
    }

    if (lvalue->ts.type == BT_UNKNOWN) {
	g95_error("Variable type is UNKNOWN in assignment at %L",
		  &lvalue->where);
	return FAILURE;
    }

    if (rvalue->type == EXPR_NULL) {
	g95_error("NULL() at %L must appear in a pointer assignment",
		  &rvalue->where);
	return FAILURE;
    }

    if (lvalue->ts.type == BT_PROCEDURE) {
	g95_error("Can't dereference procedure pointer at %L", &lvalue->where);
	return FAILURE;
    }

    if ((g95_coindexed_expr(lvalue) || g95_coindexed_expr(rvalue)) &&
	lvalue->ts.type == BT_DERIVED &&
	g95_allocatable_component(lvalue->ts.derived)) {

	g95_error("Coindexed assignment at %L cannot have an "
		  "ALLOCATABLE component", &lvalue->where);
	return FAILURE;
    }

    /* Check size of array assignments */

    if (lvalue->rank != 0 && rvalue->rank != 0 &&
	g95_check_conformance("Array assignment", lvalue, rvalue) == FAILURE)
	return FAILURE;

    e = g95_assign_boz(&lvalue->ts, rvalue);
    if (e != NULL)
	g95_replace_expr(rvalue, e);

    if (rvalue->ts.type == BT_INTEGER && rvalue->type == EXPR_CONSTANT &&
	g95_range_check(rvalue)) {
	g95_error("Integer too big for its kind at %L", &rvalue->where);
	return FAILURE;
    }

    return g95_compare_types(&lvalue->ts, &rvalue->ts)
	? SUCCESS
	: g95_convert_type(rvalue, &lvalue->ts, 0);
}



/* check_conflict()-- This is a generic conflict-checker.  We do this
 * to avoid having a single conflict in two places. */

#define conf(a, b) \
      if (attr->a && attr->b) { attr1 = a; attr2 = b; goto conflict; }
#define conf2(a) if (attr->a) { attr2 = a; goto conflict; }

static try check_conflict(symbol_attribute *attr, char *name,
			  g95_locus *where) {
char *attr1, *attr2;

static char *dummy = "DUMMY", *save = "SAVE", *pointer = "POINTER",
  *target = "TARGET", *external = "EXTERNAL", *intent = "INTENT",
  *equivalenced = "EQUIVALENCE", *intrinsic = "INTRINSIC",
  *allocatable = "ALLOCATABLE", *data = "DATA", *elemental = "ELEMENTAL",
  *private = "PRIVATE", *recursive = "RECURSIVE", *in_common = "COMMON",
  *result_var = "RESULT", *in_namelist = "NAMELIST", *public = "PUBLIC",
  *optional = "OPTIONAL", *entry = "ENTRY", *function = "FUNCTION",
  *subroutine = "SUBROUTINE", *dimension = "DIMENSION", *value = "VALUE",
  *protected = "PROTECTED", *volatile_ = "VOLATILE";

    if (where == NULL)
	where = &g95_current_locus;

    if (G95_STRICT_F95() && attr->pointer && attr->intent != INTENT_UNKNOWN) {
	attr1 = pointer;
	attr2 = intent;
	goto conflict;
    }

    /* Check conflicts with access specifications */

    switch(attr->access) {
    case ACCESS_UNKNOWN:
	break;

    case ACCESS_PRIVATE: attr2 = private; goto check_attr;
    case ACCESS_PUBLIC:  attr2 = public;
    check_attr:
	attr1 = g95_flavor_string(attr->flavor);

	switch(attr->flavor) {
	case FL_PROGRAM:  case FL_BLOCK_DATA:  case FL_MODULE:
	case FL_LABEL:    /* NAMELIST conflict checked elsewhere */
	    goto conflict;

	default:
	    break;
	}
    }

/* Check for attributes not allowed in a BLOCK DATA */

    if (g95_current_state() == COMP_BLOCK_DATA) {
	attr1 = NULL;

	if (attr->allocatable) attr1 = allocatable;
	if (attr->external) attr1 = external;
	if (attr->optional) attr1 = optional;
	if (attr->intent != INTENT_UNKNOWN) attr1 = intent;

	if (attr1 != NULL) {
	    g95_error("%s attribute not allowed in BLOCK DATA program unit "
		      "at %L", attr1, where);
	    return FAILURE;
	}
    }

    conf(external, intrinsic);
    conf(allocatable, pointer);

    conf(external, volatile_);
    conf(intrinsic, volatile_);
    conf(value, volatile_);

    conf(pointer, target);
    conf(pointer, external);
    conf(pointer, intrinsic);
    conf(target, external);
    conf(target, intrinsic);
    conf(external, dimension);

    conf(dummy, save);
    conf(dummy, data);
    conf(dummy, entry);
    conf(dummy, intrinsic);

    conf(save, result_var);
    conf(data, result_var);
    conf(data, allocatable);

    conf(protected, external);
    conf(protected, intrinsic);

    if (attr->value && attr->intent == INTENT_INOUT) {
	attr1 = value;
	attr2 = "INTENT_INOUT";
	goto conflict;
    }

    if (attr->volatile_ && attr->intent == INTENT_IN) {
	attr1 = volatile_;
	attr2 = "INTENT(IN)";
	goto conflict;
    }

    if (attr->value && attr->intent == INTENT_OUT) {
	attr1 = value;
	attr2 = "INTENT_OUT";
	goto conflict;
    }

    if (!g95_option.tr15581 && G95_STRICT_F95()) {
	conf(allocatable, dummy);
	conf(allocatable, function);
	conf(allocatable, result_var);
    }

    conf(elemental, recursive);

    conf(in_common, function);
    conf(in_common, entry);
    conf(in_common, save);
    conf(in_common, dummy);
    conf(in_common, allocatable);
    conf(in_common, result_var);

    conf(dummy, result_var);

    conf(in_namelist, pointer);
    conf(in_namelist, allocatable);

    conf(entry, result_var);
    conf(function, subroutine);

    conf(value, external);
    conf(value, pointer);
    conf(value, allocatable);
    conf(value, dimension);

    attr1 = g95_flavor_string(attr->flavor);

    if (attr->in_namelist && attr->flavor != FL_VARIABLE &&
	attr->flavor != FL_UNKNOWN && !namelist_proc_override) {
	attr2 = in_namelist;
	goto conflict;
    }

    switch(attr->flavor) {
    case FL_PROCEDURE:
	conf2(intent);         conf2(value);
	conf2(equivalenced);   conf2(save);

	if (attr->subroutine) {
	    conf2(in_namelist);    conf2(allocatable);   conf2(pointer);
	    conf2(target);         conf2(result_var);    conf2(function);
	}

	switch(attr->proc) {
	case PROC_DUMMY:
	    conf2(result_var);
	    conf2(in_common);
	    conf2(save);
	    break;

	case PROC_ST_FUNCTION:
	    conf2(in_common);
	    conf2(dummy);
	    break;

	case PROC_MODULE:
	    conf2(dummy);
	    break;

	default:
	    break;
	}

	break;

    case FL_PROGRAM: case FL_BLOCK_DATA: case FL_MODULE: case FL_LABEL:
	conf2(dummy);         conf2(save);          conf2(pointer);
	conf2(target);        conf2(external);      conf2(intrinsic);
	conf2(allocatable);   conf2(result_var);    conf2(in_namelist);
	conf2(optional);      conf2(function);      conf2(subroutine);
	conf2(dimension);     conf2(equivalenced);
	break;

    case FL_NAMELIST:
	break;

    case FL_DERIVED:
	conf2(dummy);        conf2(save);        conf2(pointer);
	conf2(target);       conf2(external);    conf2(intrinsic);
	conf2(allocatable);  conf2(optional);    conf2(entry);
	conf2(function);     conf2(subroutine);
      
	if (attr->intent != INTENT_UNKNOWN) { attr2 = intent; goto conflict; }
	break;

    case FL_VARIABLE:
	conf2(external);
	conf2(intrinsic);
	break;

    case FL_PARAMETER:
	conf2(external);      conf2(intrinsic);    conf2(optional);
	conf2(allocatable);   conf2(function);     conf2(subroutine);
	conf2(entry);         conf2(pointer);      conf2(target);
	conf2(dummy);         conf2(in_common);    conf2(save);
	conf2(value);         conf2(volatile_);    conf2(protected);
	break;

    default:
	break;
    }

    return SUCCESS;

conflict:
    if (name == NULL)
	g95_error("%s attribute conflicts with %s attribute at %L",
		  attr1, attr2, where);
    else
	g95_error("%s attribute conflicts with %s attribute in '%s' at %L",
		  attr1, attr2, name, where);

    return FAILURE;
}

#undef conf
#undef conf2



/* check_used()-- Common subroutine called by attribute changing
 * subroutines in order to prevent them from changing a symbol that
 * has been use-associated.  Returns zero if it is OK to change the
 * symbol, nonzero if not. */

static int check_used(symbol_attribute *attr, char *name, g95_locus *where) {

    if (attr->use_assoc == 0)
	return 0;

    if (where == NULL)
	where = &g95_current_locus;

    if (name == NULL)
	g95_error("Cannot change attributes of USE-associated symbol at %L",
		  where);
    else
	g95_error("Cannot change attributes of USE-associated symbol '%s' "
		  "at %L", name, where);

    return 1;
}



/* duplicate_attr()-- Generate an error because of a duplicate attribute */

static void duplicate_attr(char *attr, g95_locus *where) {

    if (where == NULL)
	where = &g95_current_locus;

    g95_error("Duplicate %s attribute specified at %L", attr, where);
}



try g95_add_intrinsic(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->intrinsic) {
	duplicate_attr("INTRINSIC", where);
	return FAILURE;
    }

    attr->intrinsic = 1;

    return check_conflict(attr, NULL, where);
}



try g95_add_bind(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    if (attr->bind) {
	duplicate_attr("BIND", where);
	return FAILURE;
    }

    attr->bind = 1;

    return check_conflict(attr, name, where);
}


try g95_add_abstract(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    if (attr->abstract) {
	duplicate_attr("ABSTRACT", where);
	return FAILURE;
    }

    attr->abstract = 1;

    return check_conflict(attr, name, where);
}


try g95_add_optional(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    if (attr->optional) {
	duplicate_attr("OPTIONAL", where);
	return FAILURE;
    }

    attr->optional = 1;
    return check_conflict(attr, name, where);
}



try g95_add_allocatable(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->allocatable) {
	duplicate_attr("ALLOCATABLE", where);
	return FAILURE;
    }

    attr->allocatable = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_dimension(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    if (attr->dimension) {
	duplicate_attr("DIMENSION", where);
	return FAILURE;
    }

    if (attr->data) {
	g95_error("DATA variable '%s' at %L must have DIMENSION attribute "
		  "specified before the DATA statement", name, where);
	return FAILURE;
    }

    attr->dimension = 1;
    return check_conflict(attr, name, where);
}



try g95_add_external(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;
  
    if (attr->external) {
	duplicate_attr("EXTERNAL", where);
	return FAILURE;
    }

    attr->external = 1;

    return check_conflict(attr, NULL, where);
}



try g95_add_target(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->target) {
	duplicate_attr("TARGET", where);
	return FAILURE;
    }

    attr->target = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_volatile(symbol_attribute *attr, g95_locus *where) {

    if (attr->volatile_) {
	duplicate_attr("VOLATILE", where);
	return FAILURE;
    }

    attr->volatile_ = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_value(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->value) {
	duplicate_attr("VALUE", where);
	return FAILURE;
    }

    attr->value = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_async(symbol_attribute *attr, g95_locus *where) {

    if (attr->async) {
	duplicate_attr("ASYNCHRONOUS", where);
	return FAILURE;
    }

    attr->async = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_result(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    attr->result_var = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_pointer(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    attr->pointer = 1;
    return check_conflict(attr, name, where);
}



try g95_add_save(symbol_attribute *attr, char *name, g95_locus *where) {

    if (where == NULL)
	where = &g95_current_locus;

    if (check_used(attr, name, where))
	return FAILURE;

    if (g95_pure(NULL, 1)) {
	g95_error("SAVE attribute at %L cannot be specified in a "
		  "PURE procedure", where);
	return FAILURE;
    }

    if (!g95_option.multiple_save && attr->save) {
	duplicate_attr("SAVE", where);
	return FAILURE;
    }

    attr->save = 1;
    return check_conflict(attr, name, where);
}



try g95_add_dummy(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    /* Duplicate dummy arguments are allow due to ENTRY statements */

    attr->dummy = 1;
    return check_conflict(attr, name, where);
}



try g95_add_in_common(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    /* Duplicate attribute already checked for */

    attr->in_common = 1;
    if (check_conflict(attr, name, where) == FAILURE)
	return FAILURE;

    if (attr->flavor == FL_VARIABLE)
	return SUCCESS;

    return g95_add_flavor(attr, FL_VARIABLE, name, where);
}



try g95_add_in_namelist(symbol_attribute *attr, char *name, g95_locus *where,
			int flag) {
int r;

    namelist_proc_override = flag;

    attr->in_namelist = 1;
    attr->desc = 1;
    r = check_conflict(attr, name, where);

    namelist_proc_override = 0;
    return r;
}



try g95_add_elemental(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->elemental) {
	duplicate_attr("ELEMENTAL", where);
	return FAILURE;
    }

    attr->elemental = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_pure(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->pure) {
	duplicate_attr("PURE", where);
	return FAILURE;
    }

    attr->pure = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_protected(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    attr->protected = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_equivalence(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    attr->equivalenced = 1;
    return check_conflict(attr, name, where);
}



try g95_add_sequence(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    attr->sequence = 1;
    return check_conflict(attr, name, where);
}



try g95_add_data(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    attr->data = 1;
    return check_conflict(attr, name, where);
}



try g95_add_recursive(symbol_attribute *attr, g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->recursive) {
	duplicate_attr("RECURSIVE", where);
	return FAILURE;
    }

    attr->recursive = 1;
    return check_conflict(attr, NULL, where);
}



try g95_add_entry(symbol_attribute *attr, char *name, g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    if (attr->entry) {
	duplicate_attr("ENTRY", where);
	return FAILURE;
    }

    attr->entry = 1;
    return check_conflict(attr, name, where);
}



try g95_add_function(symbol_attribute *attr, char *name, g95_locus *where) {

    if (attr->flavor != FL_PROCEDURE &&
	g95_add_flavor(attr, FL_PROCEDURE, name, where) == FAILURE)
	return FAILURE;

    attr->function = 1;
    return check_conflict(attr, name, where);
}



try g95_add_subroutine(symbol_attribute *attr, char *name, g95_locus *where) {

    if (attr->flavor != FL_PROCEDURE &&
	g95_add_flavor(attr, FL_PROCEDURE, name, where) == FAILURE)
	return FAILURE;

    attr->subroutine = 1;
    return check_conflict(attr, name, where);
}



/* g95_add_flavor()-- Flavors are special because some flavors are not
 * what fortran considers attributes and can be reaffirmed multiple
 * times. */

try g95_add_flavor(symbol_attribute *attr, sym_flavor f, char *name,
		   g95_locus *where) {

    if ((f == FL_PROGRAM || f == FL_BLOCK_DATA || f == FL_MODULE ||
	 f == FL_PARAMETER || f == FL_LABEL || f == FL_DERIVED ||
	 f == FL_NAMELIST) && check_used(attr, name, where))
	return FAILURE;

    if (attr->flavor == f && f == FL_VARIABLE)
	return SUCCESS;

    if (attr->flavor != FL_UNKNOWN) {
	if (where == NULL)
	    where = &g95_current_locus;

	g95_error("%s attribute conflicts with %s attribute at %L.",
		  g95_flavor_string(attr->flavor),
		  g95_flavor_string(f), where);

	return FAILURE;
    }

    attr->flavor = f;

    return check_conflict(attr, name, where);
}



try g95_add_procedure(symbol_attribute *attr, procedure_type t, char *name,
		      g95_locus *where) {

    if (check_used(attr, name, where))
	return FAILURE;

    if (attr->flavor != FL_PROCEDURE &&
	g95_add_flavor(attr, FL_PROCEDURE, name, where) == FAILURE)
	return FAILURE;

    if (where == NULL)
	where = &g95_current_locus;

    if (attr->proc != PROC_UNKNOWN) {
	g95_error("%s procedure '%s' at %L is already %s %s procedure",
		  g95_procedure_string(t), name, where,
		  g95_article(g95_procedure_string(attr->proc)),
		  g95_procedure_string(attr->proc));

	return FAILURE;
    }

    if (t != PROC_UNKNOWN)
	attr->proc = t;

/* Statement functions are always scalar and functions */

    if (t == PROC_ST_FUNCTION &&
	((!attr->function && g95_add_function(attr, name, where) == FAILURE) 
	 || attr->dimension))
	return FAILURE;

    return check_conflict(attr, name, where);
}



try g95_add_intent(symbol_attribute *attr, g95_intent intent,
		   g95_locus *where) {

    if (check_used(attr, NULL, where))
	return FAILURE;

    if (attr->intent == INTENT_UNKNOWN) {
	attr->intent = intent;
	return check_conflict(attr, NULL, where);
    }

    if (where == NULL)
	where = &g95_current_locus;

    g95_error("INTENT (%s) conflicts with INTENT(%s) at %L",
	      g95_intent_string(attr->intent),
	      g95_intent_string(intent), where);

    return FAILURE;
}



/* No checks for use-association in public and private statements */

try g95_add_access(symbol_attribute *attr, g95_access access, char *name,
		   g95_locus *where) {

    if (attr->access == ACCESS_UNKNOWN) {
	attr->access = access;
	return check_conflict(attr, name, where);
    }

    if (where == NULL)
	where = &g95_current_locus;

    g95_error("ACCESS specification at %L was already specified", where);

    return FAILURE;
}



try g95_add_explicit_interface(g95_symbol *sym, ifsrc source,
			       g95_formal_arglist *formal, g95_locus *where) {
g95_formal_arglist *f;

    if (check_used(&sym->attr, sym->name, where))
	return FAILURE;

    if (where == NULL)
	where = &g95_current_locus;

    if (sym->attr.if_source != IFSRC_UNKNOWN &&
	sym->attr.if_source != IFSRC_DECL) {
	g95_error("Symbol '%s' at %L already has an explicit interface",
		  sym->name, where);
	return FAILURE;
    }

    sym->formal = formal;
    sym->attr.if_source = source;

    if (source == IFSRC_IFBODY)
	for(f=sym->formal; f; f=f->next)
	    if (f->sym != NULL)
		f->sym->attr.used = f->sym->attr.set = 1;

    return SUCCESS;
}



/* g95_add_type()-- Add a type to a symbol. */

try g95_add_type(g95_symbol *sym, g95_typespec *ts, g95_locus *where) {
sym_flavor flavor;

    if (where == NULL)
	where = &g95_current_locus;

    flavor = sym->attr.flavor;

    if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA ||
	flavor == FL_MODULE || flavor == FL_LABEL ||
	(flavor == FL_PROCEDURE && sym->attr.subroutine) ||
	flavor == FL_DERIVED || flavor == FL_NAMELIST) {
	g95_error("Symbol '%s' at %L cannot have a type", sym->name, where);
	return FAILURE;
    }

    if (sym->attr.implicit_type) {
	if (sym->ts.type != ts->type ||
	    ((ts->type == BT_INTEGER || ts->type == BT_REAL ||
	      ts->type == BT_LOGICAL || ts->type == BT_COMPLEX ||
	      ts->type == BT_CHARACTER) && (sym->ts.kind != ts->kind))) {

	    g95_error("Symbol '%s' at %L being given type %s already has "
		      "implicit type %s", sym->name, where, g95_typename(ts),
		      g95_typename(&sym->ts));
	    return FAILURE;
	}

	sym->attr.implicit_type = 0;  /* No longer implicitly typed! */

    } else if (sym->ts.type != BT_UNKNOWN) {
	g95_error("Symbol '%s' at %L already has basic type of %s", sym->name,
		  where, g95_basic_typename(sym->ts.type));
	return FAILURE;
    }

    sym->ts = *ts;

    if (ts->type == BT_DERIVED)
	ts->derived->attr.used = 1;

    return SUCCESS;
}



/* g95_copy_attr()-- copy an attribute to a symbol attribute, bit by
 * bit.  Some attributes have a lot of side-effects but cannot be
 * present given where we are called from, so we ignore some bits */

try g95_copy_attr(symbol_attribute *dest, symbol_attribute *src,
		  g95_locus *where) {

    if (src->allocatable && g95_add_allocatable(dest, where) == FAILURE)
	goto fail;

    if (src->dimension && g95_add_dimension(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->value && g95_add_value(dest, where) == FAILURE)
	goto fail;

    if (src->volatile_ && g95_add_volatile(dest, where) == FAILURE)
	goto fail;

    if (src->async && g95_add_async(dest, where) == FAILURE)
	goto fail;

    if (src->pointer && g95_add_pointer(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->save && g95_add_save(dest, NULL, where) == FAILURE)   goto fail;
    if (src->target && g95_add_target(dest, where) == FAILURE)     goto fail;
    if (src->dummy && g95_add_dummy(dest, NULL, where) == FAILURE) goto fail;
    if (src->result_var && g95_add_result(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->optional && g95_add_optional(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->in_namelist &&
	g95_add_in_namelist(dest, NULL, where, 0) == FAILURE)
	goto fail;

    if (src->in_common && g95_add_in_common(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->function && g95_add_function(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->subroutine && g95_add_subroutine(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->sequence && g95_add_sequence(dest, NULL, where) == FAILURE)
	goto fail;

    if (src->flavor != FL_UNKNOWN && !src->function && !src->subroutine &&
	g95_add_flavor(dest, src->flavor, NULL, where) == FAILURE) goto fail;

    if (src->intent != INTENT_UNKNOWN &&
	g95_add_intent(dest, src->intent, where) == FAILURE) goto fail;

    if (src->elemental && g95_add_elemental(dest, where) == FAILURE) goto fail;
    if (src->pure && g95_add_pure(dest, where) == FAILURE)           goto fail;
    if (src->recursive && g95_add_recursive(dest, where) == FAILURE) goto fail;

    if (src->access != ACCESS_UNKNOWN &&
	g95_add_access(dest, src->access, NULL, where) == FAILURE)   goto fail;

    if (src->external && g95_add_external(dest, where) == FAILURE)   goto fail;
    if (src->intrinsic && g95_add_intrinsic(dest, where) == FAILURE) goto fail;
    if (src->entry && g95_add_entry(dest, NULL, where) == FAILURE)   goto fail;
    if (src->bind && g95_add_bind(dest, NULL, where) == FAILURE)     goto fail;
    if (src->protected && g95_add_protected(dest, where) == FAILURE) goto fail;

    return SUCCESS;

fail:
    return FAILURE;
}



/* g95_compare_attr()-- Compares two attributes */

int g95_compare_attr(symbol_attribute *a1, symbol_attribute *a2) {

    return a1->allocatable == a2->allocatable &&
	a1->dimension == a2->dimension     && a1->external == a2->external &&
	a1->intrinsic == a2->intrinsic     && a1->optional == a2->optional &&
	a1->pointer == a2->pointer         && a1->save == a2->save &&
	a1->target == a2->target           && a1->dummy == a2->dummy &&
	a1->result_var == a2->result_var   && a1->entry == a2->entry &&
	a1->data == a2->data               && a1->use_assoc == a2->use_assoc &&
	a1->in_namelist == a2->in_namelist && a1->in_common == a2->in_common &&
	a1->function == a2->function       && a1->subroutine == a2->subroutine &&
	a1->sequence == a2->sequence       && a1->elemental == a2->elemental &&
	a1->pure == a2->pure               && a1->recursive == a2->recursive &&
	a1->access == a2->access           && a1->intent == a2->intent &&
	a1->flavor == a2->flavor           && a1->proc == a2->proc;
}



/* g95_clear_attr()-- Clears all attributes */

void g95_clear_attr(symbol_attribute *attr) {

    memset(attr, '\0', sizeof(symbol_attribute));
 
    attr->access    = ACCESS_UNKNOWN;
    attr->intent    = INTENT_UNKNOWN;
    attr->flavor    = FL_UNKNOWN;
    attr->proc      = PROC_UNKNOWN;
    attr->if_source = IFSRC_UNKNOWN;
}



/************** Component name management ************/

/* Component names of a derived type form their own little namespaces
 * that are separate from all other spaces.  The space is composed of
 * a singly linked list of g95_component structures whose head is
 * located in the parent symbol. */

/* g95_add_component()-- Add a component name to a symbol.  The call
 * fails if the name is already present.  On success, the component
 * pointer is modified to point to the additional component structure. */

try g95_add_component(g95_symbol *sym, char *name, g95_component **component) {
g95_component *c, *tail;
g95_symbol *parent;

    tail = NULL;

    for(c=sym->components; c; c=c->next) {
	if (strcmp(c->name, name) == 0) {
	    g95_error("Component '%s' at %C already declared at %L",
		      name, &c->where);
	    return FAILURE;
	}

	tail = c;
    }

    /* Search parent types for the component name */

    parent = sym->extends;
    while(parent != NULL) {
	for(c=parent->components; c; c=c->next)
	    if (strcmp(c->name, name) == 0) {
		g95_error("Component '%s' at %C declared in parent type '%s'",
			  name, parent->name);
		return FAILURE;
	    }

	parent = parent->extends;
    }

/* Allocate new component */

    c = g95_get_component();

    if (tail == NULL)
	sym->components = c;

    else
	tail->next = c;

    c->name  = g95_get_string(name);
    c->where = g95_def_locus;

    *component = c;
    return SUCCESS;
}



/* free_components()-- Given a symbol, free all of the component
 * structures and everything they point to. */

static void free_components(g95_component *p) {
g95_component *q;

    for(; p; p=q) {
	q = p->next;

	g95_free_array_spec(p->as);
	g95_free_expr(p->initializer);

	g95_free(p);
    }
}



/* switch_types()-- Recursive function to switch derived types of all
 * symbol in a namespace. */

static void switch_types(g95_symtree *st, g95_symbol *from, g95_symbol *to) {
g95_symbol *sym;

    if (st == NULL)
	return;

    sym = st->n.sym;
    if (sym->ts.type == BT_DERIVED && sym->ts.derived == from)
	sym->ts.derived = to;

    switch_types(st->left, from, to);
    switch_types(st->right, from, to);
}



/* g95_use_derived()-- This subroutine is called when a derived type
 * is used in order to make the final determination about which
 * version to use.  The standard requires that a type be defined
 * before it is 'used', but such types can appear in IMPLICIT
 * statements before the actual definition.  'Using' in this context
 * means declaring a variable to be that type or using the type
 * constructor.
 *
 * If a type is used and the components haven't been defined, then we
 * have to have a derived type in a parent unit.  We find the node in
 * the other namespace and point the symtree node in this namespace to
 * that node.  Further reference to this name point to the correct
 * node.  If we can't find the node in a parent namespace, then have
 * an error.
 *
 * This subroutine takes a pointer to a symbol node and returns a
 * pointer to the translated node or NULL for an error.  Usually there
 * is no translation and we return the node we were passed.  */

g95_symbol *g95_use_derived(g95_symbol *sym) {
g95_namespace *ns;
g95_symbol *s, *p;
g95_typespec *t;
g95_symtree *st;
int i;

    if (sym->components != NULL || sym->attr.itype != ITYPE_NONE)
	return sym;   /* Already defined */

    if (sym->ns->parent == NULL)
	goto bad;

    ns = sym->ns->parent;

    for(;;) {
	if (ns == NULL || ns->state == COMP_NONE)
	    goto bad;

	if (g95_find_symbol(sym->name, ns, 0, &s)) {
	    g95_error("Symbol '%s' at %L is defined in multiple modules",
		      sym->name, &g95_def_locus);
	    return NULL;
	}

	ns = ns->parent;
    }

    if (s == NULL || s->attr.flavor != FL_DERIVED)
	goto bad;

    /* Get rid of symbol sym, translating all references to s */

    for(i=0; i<G95_LETTERS; i++) {
	t = &sym->ns->default_type[i];
	if (t->derived == sym)
	    t->derived = s;
    }

    st = g95_find_symtree(sym->ns->sym_root, sym->name);
    st->n.sym = s;

    s->refs++;

    /* Unlink from list of modified symbols */

    if (changed_syms == sym)
	changed_syms = sym->tlink;

    else
	for(p=changed_syms; p; p=p->tlink)
	    if (p->tlink == sym) {
		p->tlink = sym->tlink;
		break;
	    }

    switch_types(sym->ns->sym_root, sym, s);

    /* TODO: Also have to replace sym -> s in other lists like
     * namelists, common lists and interface lists.  */

    g95_free_symbol(sym);
    return s;

bad:
    g95_error("Derived type '%s' at %L is being used before it is defined",
	      sym->name, &g95_def_locus);
    return NULL;
}



/******************** Statement label management ********************/

/* free_st_labels()-- Free a whole list of g95_st_label structures.  */

static void free_st_labels(g95_st_label *l1) {
g95_st_label *l2;

    for(; l1; l1=l2) {
	l2 = l1->next;
	if (l1->format != NULL)
	    g95_free_expr(l1->format);

	g95_free(l1);
    }
}



/* Free a single g95_st_label structure, making sure the list is not
 * messed up.  This function is called only when some parse error
 * occurs. */

void g95_free_st_label(g95_st_label *label) {

    if (label == NULL)
	return;

    if (label->prev)
	(label->prev->next = label->next);
 
    if (label->next)
	(label->next->prev = label->prev);

    if (g95_current_ns->st_labels == label)
	g95_current_ns->st_labels = label->next;

    if (label->format != NULL)
	g95_free_expr(label->format);

    g95_free(label);
}


/* g95_reference_st_label()-- Reference a label.  Given a label
 * and its type, see if that reference is consistent with what is
 * known about that label, updating the unknown state.  Returns
 * FAILURE if something goes wrong. */

try g95_reference_st_label(g95_st_label *lp, g95_sl_type type) {
g95_sl_type label_type;
int labelno;
try rc;

    if (lp == NULL)
	return SUCCESS;

    labelno = lp->value;

    if (lp->defined != ST_LABEL_UNKNOWN)
	label_type = lp->defined;
    else {
	label_type = lp->referenced;
	g95_update_locus(&lp->where, &g95_current_locus);
    }

    if (label_type == ST_LABEL_FORMAT && type == ST_LABEL_TARGET) {
	g95_error("Label %d at %C previously used as a FORMAT label", labelno);
	rc = FAILURE;
	goto done;
    }

    if ((label_type == ST_LABEL_TARGET || label_type == ST_LABEL_BAD_TARGET)
	&& type == ST_LABEL_FORMAT) {
	g95_error("Label %d at %C previously used as branch target", labelno);
	rc = FAILURE;
	goto done;
    }

    if (lp->referenced == ST_LABEL_UNKNOWN ||
	lp->referenced == ST_LABEL_REFFED)
	lp->referenced = type;

    rc = SUCCESS;

done:
    return rc;
}



/* g95_get_st_label()-- Given a label number, search for and return a
 * pointer to the label structure, creating it if it does not exist.
 * A label number less than zero means return a unique label. */

g95_st_label *g95_get_st_label(int number) {
static int unique_label = 100000;
g95_st_label *lp;

    if (number < 0)
	number = unique_label++;

/* First see if the label is already in this namespace.  */

    for(lp=g95_current_ns->st_labels; lp; lp=lp->next)
	if (lp->value == number)
	    break;

    if (lp != NULL)
	return lp;
  
    lp = g95_getmem(sizeof(g95_st_label));

    lp->value = number;

    if (number >= 100000) {
	lp->defined = ST_LABEL_TARGET;
	lp->referenced = ST_LABEL_TARGET;

    } else {
	lp->defined = ST_LABEL_UNKNOWN;
	lp->referenced = ST_LABEL_UNKNOWN;
    }

    lp->prev = NULL;
    lp->next = g95_current_ns->st_labels;

    if (g95_current_ns->st_labels)
	g95_current_ns->st_labels->prev = lp;

    g95_current_ns->st_labels = lp;

    return lp;
}



/* g95_define_st_label()-- Called when a statement with a statement
 * label is about to be accepted.  We add the label to the list of the
 * current namespace, making sure it hasn't been defined previously
 * and referenced correctly. */

void g95_define_st_label(g95_st_label *lp, g95_sl_type type,
                         g95_locus *label_locus) {
int number;

    number = lp->value;

    if (lp->defined != ST_LABEL_UNKNOWN)
	g95_error("Duplicate statement label %d at %L and %L", number,
		  &lp->where, label_locus);
    else {
	lp->where = *label_locus;

	switch(type) {
	case ST_LABEL_FORMAT:
	    if (lp->referenced == ST_LABEL_TARGET) 
		g95_error("Label %d at %C already referenced as branch target",
			  number);
	    else
		lp->defined = ST_LABEL_FORMAT;

	    break;

	case ST_LABEL_TARGET:
	    if (lp->referenced == ST_LABEL_FORMAT)
		g95_error("Label %d at %C already referenced as a "
			  "format label", number);
	    else
		lp->defined = ST_LABEL_TARGET;

	    break;

	default:
	    lp->defined = ST_LABEL_BAD_TARGET;
	    lp->referenced = ST_LABEL_BAD_TARGET;
	}
    }
}



/* g95_implicit_types()-- Set the default implicit types in a namespace. */

void g95_implicit_types(g95_namespace *ns, int parent_types) {
g95_typespec *ts;
int i;

    for(i='a'; i<='z'; i++) {
	ns->set_flag[i - 'a'] = 0;
	ts = &ns->default_type[i - 'a'];

	if (parent_types && ns->parent != NULL) {    /* Copy parent settings */
	    *ts = ns->parent->default_type[i - 'a'];
	    continue;
	}

	if (g95_option.implicit_none != 0 || G95_STRICT_F()) {
	    g95_clear_ts(ts);
	    continue;
	}

	if ('i' <= i && i <= 'n') {
	    ts->type = BT_INTEGER;
	    ts->kind = g95_default_integer_kind(1);
	} else {
	    ts->type = BT_REAL;
	    ts->kind = g95_default_real_kind(1);
	}
    }
}



/* g95_get_namespace()-- Allocate a new namespace structure. */

g95_namespace *g95_get_namespace(g95_namespace *parent, int parent_types) {
g95_namespace *ns;
int i;
 
    ns = g95_getmem(sizeof(g95_namespace));
    ns->sym_root = NULL;
    ns->uop_root = NULL;
    ns->default_access = ACCESS_UNKNOWN;
    ns->parent = parent;

    for(i=0; i<G95_INTRINSIC_OPS; i++)
	ns->operator_access[i] = ACCESS_UNKNOWN;

    g95_implicit_types(ns, parent_types);

    return ns;
}



/* g95_compare_symtree()-- Comparison function for symtree nodes. */

int g95_compare_symtree(g95_symtree *st1, g95_symtree *st2) {

    return strcmp(st1->name, st2->name);
}



/* g95_new_symtree()-- Allocate a new symtree node and associate it
 * with the new symbol. */

g95_symtree *g95_new_symtree(g95_symtree **root, char *name) {
g95_symtree *st;

    st = g95_getmem(sizeof(g95_symtree));
    st->name = g95_get_string(name);

    g95_insert_bbt(root, st, g95_compare_symtree);
    return st;
}



/* delete_symtree()-- delete a symbol from the tree.  Does not free the
 * symbol itself! */

static void delete_symtree(g95_symtree **root, char *name) {
g95_symtree st, *st0;

    st0 = g95_find_symtree(*root, name); 

    st.name = g95_get_string(name);
    g95_delete_bbt(root, &st, g95_compare_symtree);

    g95_free(st0);
}



/* g95_find_symtree()-- Given a root symtree node and a name, try to
 * find the symbol within the namespace.  Returns NULL if the symbol
 * is not found. */

g95_symtree *g95_find_symtree(g95_symtree *st, char *name) {
int c;

    while(st != NULL) {
	c = strcmp(name, st->name);
	if (c == 0)
	    return st;

	st = (c < 0) ? st->left : st->right;
    }

    return NULL;
}



/* g95_get_uop()-- Given a name find a user operator node, creating it
 * if it doesn't exist.  These are much simpler than symbols because
 * they can't be ambiguous with one another */

g95_user_op *g95_get_uop(char *name) {
g95_user_op *uop;
g95_symtree *st;

    st = g95_find_symtree(g95_current_ns->uop_root, name);
    if (st != NULL)
	return st->n.uop;

    st = g95_new_symtree(&g95_current_ns->uop_root, name);

    uop = st->n.uop = g95_getmem(sizeof(g95_user_op));

    uop->name   = g95_get_string(name);
    uop->access = ACCESS_UNKNOWN;
    uop->ns     = g95_current_ns;

    return uop;
}



/* g95_find_uop()-- Given a name find the user operator node.  Returns
 * NULL if it does not exist. */

g95_user_op *g95_find_uop(char *name, g95_namespace *ns) {
g95_symtree *st;

    if (ns == NULL)
	ns = g95_current_ns;

    st = g95_find_symtree(ns->uop_root, name);
    return (st == NULL) ? NULL : st->n.uop;
}



/* g95_find_generic()-- Find the symtree node for the generic name,
 * returns NULL if not found. */

g95_symtree *g95_find_generic(char *name, g95_namespace *ns) {

    if (ns == NULL)
	ns = g95_current_ns;

    return g95_find_symtree(ns->generic_root, name);
}



/* g95_generic_name()-- Returns nonzero if the name is a generic name
 * in this or a parent namespace. */

int g95_generic_name(char *name) {
g95_namespace *ns;

    for(ns=g95_current_ns; ns; ns=ns->parent)
	if (g95_find_generic(name, ns))
	    return 1;

    return 0;
}



/* g95_get_generic()-- Find the symtree for the given generic node,
 * creating it if it does not exist. */

g95_symtree *g95_get_generic(char *name, g95_namespace *ns) {
g95_symtree *st;

    st = g95_find_generic(name, ns); 
    if (st != NULL)
	return st;

    if (ns == NULL)
	ns = g95_current_ns;

    st = g95_new_symtree(&ns->generic_root, name);
    st->access = ACCESS_UNKNOWN;

    return st;
}



/* g95_free_symbol()-- Remove a g95_symbol structure and everything it
 * points to. */

void g95_free_symbol(g95_symbol *sym) {

    if (sym == NULL)
	return;

    g95_free_array_spec(sym->as);
    g95_free_coarray_spec(sym->cas);

    free_components(sym->components);

    g95_free_expr(sym->value);
    g95_free_namelist(sym->namelist);
    g95_free_namespace(sym->formal_ns);
    g95_free_formal_arglist(sym->formal);

    g95_free(sym);
}



/* g95_new_symbol()-- Allocate and initialize a new symbol node */

g95_symbol *g95_new_symbol(char *name, g95_namespace *ns) {
g95_symbol *p;

    p = g95_getmem(sizeof(g95_symbol));

    g95_clear_ts(&p->ts);
    g95_clear_attr(&p->attr);
    p->ns = ns;

    p->declared_at = g95_def_locus;
    p->name = g95_get_string(name);

    return p;
}



/* ambiguous_symbol()-- Generate an error if a symbol is ambiguous. */

static void ambiguous_symbol(char *name, g95_symtree *st) {

    if (st->n.sym->module != NULL)
	g95_error("Name '%s' at %C is an ambiguous reference to '%s' "
		  "from module '%s'", name, st->n.sym->name,
		  st->n.sym->module);
    else
	g95_error("Name '%s' at %C is an ambiguous reference to '%s' "
		  "from current program unit", name, st->n.sym->name);
}



/* g95_save_symbol_data()-- Save symbol with the information necessary
 * to back it out. */

void g95_save_symbol_data(g95_symbol *sym) {

    if (sym->new || sym->old_symbol != NULL)
	return;

    sym->old_symbol = g95_getmem(sizeof(g95_symbol));
    *(sym->old_symbol) = *sym;

    sym->tlink = changed_syms;
    changed_syms = sym;
}



/* g95_find_symbol()-- search for a symbol starting in the current
 * namespace, resorting to any parent namespaces if requested by a
 * nonzero parent_flag.  Returns nonzero if the symbol is ambiguous.
 * If a symbol that is found is changed, then g95_save_symbol() data
 * must be called, since generally we return symbols that are not
 * subsequently changed. */

int g95_find_symbol(char *name, g95_namespace *ns, int parent_flag,
		    g95_symbol **result) {
g95_symtree *st;

    if (ns == NULL)
	ns = g95_current_ns;

    do {
	st = g95_find_symtree(ns->sym_root, name);
	if (st != NULL) {
	    *result = st->n.sym;
	    if (st->ambiguous) {
		ambiguous_symbol(name, st);
		return 1;
	    }

	    return 0;
	}

	if (!parent_flag)
	    break;

	ns = ns->parent;
    } while (ns != NULL);

    *result = NULL;
    return 0;
}



/* g95_get_ha_symbol()-- Subroutine that searches for a symbol,
 * creating it if it doesn't exist, but tries to host-associate the
 * symbol if possible. */

int g95_get_ha_symbol(char *name, g95_symbol **result) {
g95_namespace *ns;
g95_state_data *s;
g95_symbol *sym;
g95_symtree *st;
int i, import;

    ns = g95_current_ns;
    s = g95_state_stack;
    import = ns->import;

    for(;;) {
	i = g95_find_symbol(name, ns, 0, &sym);
	if (sym != NULL) {
	    g95_save_symbol_data(sym);

	    if (ns != g95_current_ns) {
		st = g95_find_symtree(g95_current_ns->sym_root, name);
		if (st == NULL) {
		    st = g95_new_symtree(&g95_current_ns->sym_root, name);

		    st->link = changed_st;
		    changed_st = st;
		}

		st->n.sym = sym;
		sym->refs++;
	    }

	    *result = sym;
	    return i;
	}

	ns = ns->parent;
	if (ns == NULL)
	    goto create;

	import |= ns->import;

    loop:
	s = s->previous;
	if (s == NULL)
	    goto create;

	switch(s->state) {
	case COMP_NONE:        case COMP_PROGRAM:    case COMP_MODULE:
	case COMP_SUBROUTINE:  case COMP_FUNCTION:   case COMP_BLOCK_DATA:
	case COMP_DERIVED:
	    break;

	case COMP_INTERFACE:
	    if (!import)
		goto create;

	    break;

	case COMP_ENUM:
	    goto create;

	case COMP_IF:        case COMP_DO:       case COMP_SELECT:
	case COMP_FORALL:    case COMP_WHERE:    case COMP_CONTAINS:
	case COMP_CRITICAL:
	    goto loop;
	}
    }

create:
    return g95_get_symbol(name, g95_current_ns, result);
}



/* g95_get_symbol()-- Given a name, find a symbol, or create it if it does
 * not exist yet in the current namespace.
 * If the symbol is found we make sure that it's OK.
 *
 * The integer return code indicates
 *  0   All OK
 *  1   The symbol name was ambiguous
 *  2   The name meant to be established was already host associated.
 *
 * So if nonzero, then an error was issued.  This subroutine assumes
 * that a returned symbol is about to be changed and saves it. */

int g95_get_symbol(char *name, g95_namespace *ns, g95_symbol **result) {
g95_symtree *st;
g95_symbol *p;

    /* This doesn't usually happen during resolution.  */
    if (ns == NULL)
	ns = g95_current_ns;

    /* Try to find the symbol. */
    st = g95_find_symtree(ns->sym_root, name);

    if (st == NULL) {         /* If not there, create a new symbol */
	p = g95_new_symbol(name, ns); 

	p->old_symbol = NULL;   /* Add to the list of tentative symbols. */
	p->tlink = changed_syms;
	p->mark = 1;
	p->new = 1;
	changed_syms = p;

	st = g95_new_symtree(&ns->sym_root, name);
	st->n.sym = p;
	p->refs++;

    } else {    /* Make sure the existing symbol is OK */
	if (st->ambiguous) {
	    ambiguous_symbol(name, st);
	    return 1;
	}

	p = st->n.sym;

	if (p->ns != ns && !p->attr.entry &&
	    ((!p->attr.subroutine && !p->attr.function) ||
	     ns->proc_name != p)) {
	    /* Symbol is from another namespace */
	    g95_error("Symbol '%s' at %C has already been host associated",
		      name);
	    return 2;
	}

	p->mark = 1;

	g95_save_symbol_data(p);      /* Copy in case this symbol is changed */
    }

    *result = p;
    return 0;
}



/* g95_symbol_access()-- Return the accessibility of a symbol. */

g95_access g95_symbol_access(g95_symbol *sym) {
g95_namespace *ns;

    if (sym->attr.access != ACCESS_UNKNOWN)
	return sym->attr.access;

    ns = sym->ns;
    if (ns->default_access != ACCESS_UNKNOWN)
	return ns->default_access;

    while(sym->attr.flavor == FL_PROCEDURE && ns->parent != NULL) {
	ns = ns->parent;
	if (ns->default_access != ACCESS_UNKNOWN)
	    return ns->default_access;
    }

    /* Symbols that don't live in a module don't have an accessibility */

    return (ns->state == COMP_MODULE)
	? ACCESS_PUBLIC
	: ACCESS_UNKNOWN;
}



/* g95_symtree_access()-- Return the accessibility of a symtree.  The
 * symtree must be in g95_current_ns. */

g95_access g95_symtree_access(g95_symtree *st) {

    if (st->access != ACCESS_UNKNOWN)
	return st->access;

    if (g95_current_ns->default_access != ACCESS_UNKNOWN)
	return g95_current_ns->default_access;

    return ACCESS_PUBLIC;
}



/* g95_local_symbol()-- Return nonzero if this symbol is a local
 * symbol in it's namespace and prevents another symbol of the same
 * name from being host associated */

int g95_local_symbol(g95_symbol *sym) {
symbol_attribute a;

    a = sym->attr; 

    if ((sym->ts.type != BT_UNKNOWN && sym->ts.type != BT_PROCEDURE &&
	 !a.implicit_type) ||
	a.proc == PROC_ST_FUNCTION || a.pointer || a.save || a.target ||
	a.flavor == FL_PARAMETER || sym->as != NULL || a.allocatable ||
	a.in_common || a.data || a.dummy || a.result_var || a.intrinsic ||
	sym->namelist != NULL || g95_find_generic(sym->name, NULL) != NULL ||
	a.flavor == FL_LABEL || a.equivalenced || a.st_dummy)
	return 1;

    if (a.flavor == FL_PROCEDURE && a.if_source == IFSRC_DECL)
	return 1;

    if (a.proc == PROC_MODULE || a.flavor == FL_DERIVED)
	return 1;

    if (sym->ns->interface)
	return 1;

    return 0;
}



/* free_charlen()-- Free a charlen structure */

static void free_charlen(g95_charlen *cl) {

    g95_free_expr(cl->length);
    g95_free(cl);
}



/* restore_symbol()-- Restore the state of a symbol */

static void restore_symbol(g95_symbol *sym) {
g95_symbol *old;

    if (sym->new) {  /* Symbol was new */
	delete_symtree(&sym->ns->sym_root, sym->name);

	sym->refs--;
	if (sym->refs < 0)
	    g95_internal_error("g95_undo_symbols(): Negative refs");

	if (sym->refs == 0)
	    g95_free_symbol(sym);

	return;
    }

/* Restore state of a previously existing symbol. */

    sym->mark = 0;
    old = sym->old_symbol;

    sym->ts.type = old->ts.type;
    sym->ts.kind = old->ts.kind;

    sym->attr = old->attr;

    if (sym->value != old->value) {
	g95_free_expr(old->value);
	sym->value = NULL;
    }

    if (sym->as != old->as) {
	if (sym->as)
	    g95_free_array_spec(sym->as);

	sym->as = old->as;
    }

    sym->component_access = old->component_access;

    if (sym->namelist != NULL && old->namelist == NULL) {
	g95_free_namelist(sym->namelist);
	sym->namelist = NULL;

    } else if (sym->namelist_tail != old->namelist_tail) {
	g95_free_namelist(old->namelist_tail);
	old->namelist_tail->next = NULL;
    }

    sym->namelist_tail = old->namelist_tail;

    if (sym->formal != old->formal) {
	g95_free_formal_arglist(sym->formal);
	sym->formal = old->formal;
    }

    g95_free(sym->old_symbol);
    sym->old_symbol = NULL;
    sym->tlink = NULL;
}



/* fixup_ac()-- Recursive function for g95_fixup_ac(). */

static void fixup_ac(g95_expr *e, var_stack *head) {
g95_actual_arglist *a;
var_stack stack, *v;
g95_constructor *c;
g95_typespec ts;
g95_ref *ref;
int i;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	fixup_ac(e->value.op.op1, head);
	fixup_ac(e->value.op.op2, head);
	break;

    case EXPR_CONSTANT:
    case EXPR_PROCEDURE:
    case EXPR_NULL:
	break;

    case EXPR_FUNCTION:
	for(a=e->value.function.actual; a; a=a->next)
	    fixup_ac(a->u.expr, head);

	break;

    case EXPR_UNKNOWN:
    case EXPR_VARIABLE:
    case EXPR_SUBSTRING:
	if (e->symbol != NULL)
	    for(v=head; v; v=v->next)
		if (v->old == e->symbol) {
		    e->symbol = v->new;
		    break;
		}

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++) {
		    fixup_ac(ref->u.ar.start[i],  head);
		    fixup_ac(ref->u.ar.end[i],    head);
		    fixup_ac(ref->u.ar.stride[i], head);
		}

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    fixup_ac(ref->u.car.element[i], head);

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		fixup_ac(ref->u.ss.start, head);
		fixup_ac(ref->u.ss.end,   head);
		break;
	    }

	break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
	for(c=e->value.constructor.c; c; c=c->next) {
	    if (c->iterator == NULL) {
		fixup_ac(c->expr, head);
		continue;
	    }

	    fixup_ac(c->iterator->start, head);
	    fixup_ac(c->iterator->end, head);
	    fixup_ac(c->iterator->step, head);

	    stack.old = c->iterator->var->symbol;

	    g95_clear_ts(&ts);
	    ts.type = BT_INTEGER;
	    ts.kind = (stack.old->ts.kind == 0)
		? g95_default_integer_kind(0)
		: stack.old->ts.kind;

	    stack.new = g95_get_temporary(&ts, 0, 0);
	    stack.new->attr.st_construct = 1;

	    stack.next = head;

	    c->iterator->var->symbol = stack.new;
	    fixup_ac(c->expr, &stack);

	    stack.old->attr.restore = 1;
	}

	break;

    default:
	g95_internal_error("fixup_ac(): Bad type");
    }
}



/* restore_st_constructs()-- Restores any changes made to the real
 * variables. */

static void restore_st_constructs(void) {
g95_symbol **prev_link, *sym;
int new;

    prev_link = &changed_syms;

    while(*prev_link != NULL) {
	sym = *prev_link;

	if (!sym->attr.restore || sym->attr.no_restore) {
	    g95_set_usage(sym, &sym->declared_at, 0, 1);
	    prev_link = &sym->tlink;
	    continue;
	}

	*prev_link = sym->tlink;  /* Unlink */
	sym->tlink = NULL;

	new = sym->new;
	restore_symbol(sym);

	if (!new)
	    sym->attr.st_construct0 = 1;
    }
}



/* g95_fixup_ac()-- Given an array constructor, traverse it, and
 * replace all iterator variables with statement construct variables. */

void g95_fixup_ac(g95_expr *e) {

    fixup_ac(e, NULL);
    restore_st_constructs();
}



/* fixup_data1()-- Traverse an expression that is a data variable */

static void fixup_data1(g95_expr *e, var_stack *head) {
g95_actual_arglist *a;
var_stack *v;
g95_ref *ref;
int i;

    if (e == NULL)
	return;

    switch(e->type) {
    case EXPR_OP:
	fixup_data1(e->value.op.op1, head);
	fixup_data1(e->value.op.op2, head);
	break;

    case EXPR_CONSTANT:    case EXPR_NULL:   case EXPR_PROCEDURE:
    case EXPR_STRUCTURE:   case EXPR_ARRAY:
	break;

    case EXPR_FUNCTION:  /* Utterly illegal, but can't error yet */
	for(a=e->value.function.actual; a; a=a->next)
	    fixup_data1(a->u.expr, head);

	break;

    case EXPR_VARIABLE:
    case EXPR_SUBSTRING:
    case EXPR_UNKNOWN:
	if (e->symbol != NULL) {
	    for(v=head; v; v=v->next)
		if (v->old == e->symbol) {
		    e->symbol = v->new;
		    break;
		}

	    if (v == NULL)
		e->symbol->attr.no_restore = 1;
	}

	for(ref=e->ref; ref; ref=ref->next)
	    switch(ref->type) {
	    case REF_ARRAY:
		for(i=0; i<ref->u.ar.dimen; i++) {
		    fixup_data1(ref->u.ar.start[i],   head);
		    fixup_data1(ref->u.ar.end[i],     head);
		    fixup_data1(ref->u.ar.stride[i],  head);
		}

		break;

	    case REF_COARRAY:
		for(i=0; i<ref->u.car.dimen; i++)
		    fixup_data1(ref->u.car.element[i], head);

		break;

	    case REF_COMPONENT:
		break;

	    case REF_SUBSTRING:
		fixup_data1(ref->u.ss.start, head);
		fixup_data1(ref->u.ss.end, head);
		break;
	    }

	break;

    default:
	g95_internal_error("fixup_data1(): Bad expression");
    }
}



/* fixup_data2()-- Traverse the list of DATA variables */

static void fixup_data2(g95_data_variable *var, var_stack *head) {
g95_data_variable *p;
var_stack stack;

    if (var == NULL)
	return;

    for(p=var; p; p=p->next)
	if (p->expr != NULL)
	    fixup_data1(p->expr, head);

	else {
	    fixup_data1(p->iter.start, head);
	    fixup_data1(p->iter.end,   head);
	    fixup_data1(p->iter.step,  head);

	    stack.new = g95_get_temporary_int();
	    stack.new->attr.st_construct = 1;

	    stack.old = p->iter.var->symbol;
	    stack.old->attr.restore = 1;

	    stack.next = head;

	    p->iter.var->symbol = stack.new;
	    fixup_data2(p->list, &stack);
	}
}



/* g95_fixup_data()-- Given a data variable, traverse it, replacing
 * all the iterator variables with statement construct variables. */

void g95_fixup_data(g95_data_variable *var) {

    fixup_data2(var, NULL);
    restore_st_constructs();
}



/* g95_undo_symbols()-- Undoes all the changes made to symbols in the
 * current statement.  This subroutine is made simpler due to the fact
 * that attributes are never removed once added. */

void g95_undo_symbols(void) {
g95_symbol *sym, *q;
g95_symtree *v, *w;

/* if (changed_syms != NULL) g95_status("Undoing symbols\n"); */

    for(sym=changed_syms; sym; sym=q) {
	q = sym->tlink;
	/* g95_status("Undoing %s\n", sym->name); */

	restore_symbol(sym);
    }

    changed_syms = NULL;

    /* Unlink host associated symtrees */

    for(v=changed_st; v; v=w) {
	w = v->link;

	g95_delete_bbt(&g95_current_ns->sym_root, v, g95_compare_symtree);
	g95_free(v);
    }

    changed_st = NULL;
}



/* g95_commit_symbols()-- Makes the changes made in the current
 * statement permanent-- gets rid of undo information. */

void g95_commit_symbols(void) {
g95_symbol *p, *q;
g95_symtree *v, *w;

#if 0
    if (changed_syms != NULL)
	g95_status("Committing symbols\n");
#endif

    for(p=changed_syms; p; p=q) {
	q = p->tlink;
	p->tlink = NULL;
	p->mark = 0;
	p->new = 0;

	if (p->old_symbol != NULL) {
	    g95_free(p->old_symbol);
	    p->old_symbol = NULL;
	}
    }

    changed_syms = NULL;

    for(v=changed_st; v; v=w) {
	w = v->link;
	v->link = NULL;
    }

    changed_st = NULL;
    g95_current_ns->cl0 = g95_current_ns->cl_list;
}



/* free_uop_tree()-- Recursive function that deletes an entire
 * red-black tree and all the user operator nodes that it contains. */

static void free_uop_tree(g95_symtree *rb) {

    if (rb == NULL)
	return;

    free_uop_tree(rb->left);
    free_uop_tree(rb->right);

    g95_free_interface(rb->n.uop->operator);

    g95_free(rb->n.uop);
    g95_free(rb);
}



/* free_sym_tree()-- Recursive function that deletes an entire treap
 * and all the symbols that it contains. */

static void free_sym_tree(g95_symtree *st) {
g95_namespace *ns;
g95_symbol *sym;

    if (st == NULL)
	return;

    free_sym_tree(st->left);
    free_sym_tree(st->right);

    sym = st->n.sym;

    sym->refs--;
    if (sym->refs < 0)
	g95_internal_error("free_sym_tree(): Negative refs");

    if (sym->formal_ns != NULL && sym->refs == 1) {
	/* as formal_ns contains a reference to sym, delete formal_ns just
	 * before the deletion of sym. */
	ns = sym->formal_ns;
	sym->formal_ns = NULL;
	g95_free_namespace(ns);

    } else if (sym->refs == 0)   /* Go ahead and delete the symbol */
	g95_free_symbol(sym);

    g95_free(st);
}



/* free_common_tree()-- Recursively free a list of common head nodes */

static void free_common_tree(g95_symtree *st) {

    if (st == NULL)
	return;

    free_common_tree(st->left);
    free_common_tree(st->right);

    g95_free(st->n.common);
    g95_free(st);
}



/* free_generic()-- Recursively free generic interfaces. */

static void free_generic(g95_symtree *st) {

    if (st == NULL)
	return;

    free_generic(st->left);
    free_generic(st->right);

    g95_free_interface(st->n.generic);
    g95_free(st);
}



/* g95_free_namespace0()-- Free the contents of a namespace without
 * getting rid of the memory itself. */

void g95_free_namespace0(g95_namespace *ns) {
g95_charlen *cl, *cl2;
g95_annot *next;
int i;

    g95_free_statements(ns->code);
    ns->code = NULL;

    free_sym_tree(ns->sym_root);
    ns->sym_root = NULL;

    free_uop_tree(ns->uop_root);
    ns->uop_root = NULL;

    free_common_tree(ns->common_root);
    ns->common_root = NULL;

    free_generic(ns->generic_root);
    ns->generic_root = NULL;

    for(cl=ns->cl_list; cl; cl=cl2) {
	cl2 = cl->next;
	free_charlen(cl);
    }

    ns->cl_list = NULL;

    free_st_labels(ns->st_labels);
    ns->st_labels = NULL;

    g95_free_equiv(ns->equiv);
    ns->equiv = NULL;

    for(i=0; i<G95_INTRINSIC_OPS; i++) {
	g95_free_interface(ns->operator[i]);
	ns->operator[i] = NULL;
    }

    while(ns->annotation != NULL) {
	next = ns->annotation->next;
	g95_free(ns->annotation);
	ns->annotation = next;
    }
    
    g95_free_data(ns->data);
    ns->data = NULL;
}



/* g95_free_namespace()-- Free a namespace structure and everything
 * below it.  Interface lists associated with intrinsic operators are
 * not freed.  These are taken care of when a specific name is freed. */

void g95_free_namespace(g95_namespace *ns) {
g95_namespace *p, *q;

    if (ns == NULL)
	return;

    g95_free_namespace0(ns);

    /* Recursively free any contained namespaces */

    p = ns->contained;
    g95_free(ns);

    while(p != NULL) {
	q = p;
	p = p->sibling;

	g95_free_namespace(q);
    }
}



void g95_symbol_init_2(void) {

    g95_current_ns = g95_get_namespace(NULL, 0);
}



/* g95_symbol_name()-- Return the name of a symbol */

char *g95_symbol_name(g95_symbol *sym) {
static char buffer[2*G95_MAX_SYMBOL_LEN+5];

    if (sym->module != NULL)
	sprintf(buffer, "'%s:%s'", sym->module, sym->name);
    else
	sprintf(buffer, "'%s'", sym->name);

    return buffer;
}



/* g95_set_usage()-- Set the usage of a variable. */

void g95_set_usage(g95_symbol *sym, g95_locus *where, int set, int used) {

    sym->attr.used_formal = 1;
    sym->attr.set |= set;

    if (set && where != NULL && sym->ns != g95_current_ns &&
	g95_current_ns->proc_name != NULL &&
	g95_current_ns->proc_name->result != sym) {

	if (g95_pure(g95_current_ns->proc_name, 1)) {
	    g95_error("Host-associated variable '%s' is used in a defining "
		      "context in a PURE procedure at %L", sym->name, where);
	    return;
	}

	if (g95_elemental(g95_current_ns->proc_name)) {
	    g95_error("Host-associated variable '%s' is used in a defining "
		      "context in an ELEMENTAL procedure at %L",
		      sym->name, where);
	    return;
	}
    }

    if (set && sym->attr.protected && sym->attr.use_assoc) {
	g95_error("PROTECTED variable '%s' cannot be used in a defining "
		  "context at %L", sym->name, where);
	return;
    }

    if (g95_option.uninit && sym->ns == g95_current_ns &&
	!sym->attr.in_common && !sym->attr.use_assoc &&
	g95_current_ns->state != COMP_MODULE &&
	!g95_current_ns->seen_branch_target) {

	if (sym->attr.used && !sym->attr.set) {
	    g95_warning(143, "Variable '%s' at %L is used before it is set",
			sym->name, &sym->declared_at);
	    sym->attr.set = sym->attr.used = 1;

	} else if (sym->attr.allocatable && !sym->attr.alloced && set) {
	    g95_warning(144, "Variable '%s' at %L is set before it is "
			"ALLOCATEd", sym->name, &sym->declared_at);
	    sym->attr.alloced = 1;

	} else if (sym->attr.allocatable && !sym->attr.alloced && used) {
	    g95_warning(145, "Variable '%s' at %L is used before it is "
			"ALLOCATEd", sym->name, &sym->declared_at);
	    sym->attr.alloced = 1;
	}
    }

    sym->attr.used |= used;
}



/* traverse_symtree()-- Recursively traverse the symtree nodes. */

static void traverse_symtree(g95_symtree *st, void (*func)(g95_symtree *)) {

    if (st != NULL) {
	traverse_symtree(st->left, func);
	(*func)(st);
	traverse_symtree(st->right, func);
    }
}



void g95_traverse_symtree(g95_namespace *ns, void (*func)(g95_symtree *)) {

    traverse_symtree(ns->sym_root, func);
}



/* traverse_ns()-- Recursive namespace traversal function. */

static void traverse_ns(g95_symtree *st, void (*func)(g95_symbol *)) {

    if (st == NULL)
	return;

    if (st->n.sym->mark == 0)
	(*func)(st->n.sym);

    st->n.sym->mark = 1;

    traverse_ns(st->left, func);
    traverse_ns(st->right, func);
}



/* g95_clear_sym_mark()-- Clear mark bits from symbol nodes associated
 * with a symtree node */

void g95_clear_sym_mark(g95_symtree *st) {

    st->n.sym->mark = 0;
}



/* g95_traverse_ns()-- Call a given function for all symbols in the
 * namespace.  We take care that each g95_symbol node is called
 * exactly once. */

void g95_traverse_ns(g95_namespace *ns, void (*func)(g95_symbol *)) {

    g95_traverse_symtree(ns, g95_clear_sym_mark);

    traverse_ns(ns->sym_root, func);
}



/* traverse_uop()-- Function for traversing the user operator symtree */

static void traverse_uop(g95_symtree *st, void (*func)(g95_user_op *)) {

    if (st == NULL)
	return;

    (*func)(st->n.uop);

    traverse_uop(st->left, func);
    traverse_uop(st->right, func);
}



/* g95_traverse_user_op()-- Traverse the tree of user operator nodes.  */

void g95_traverse_user_op(g95_namespace *ns, void (*func)(g95_user_op *)) {

    traverse_uop(ns->uop_root, func);
}



/* g95_static_symbol()-- Return nonzero if symbol should end up in
 * static memory. */

int g95_static_symbol(g95_symbol *sym) {
symbol_attribute *a;
g95_namespace *ns;

    if (!g95_constant_array_spec(sym->as, 0) || sym->attr.function ||
	sym->attr.result_var)
	return 0;

    if (sym->attr.save || sym->ns->save_all || sym->value != NULL ||
	sym->attr.data || sym->attr.equivalenced || sym->attr.in_common ||
	sym->attr.bind || g95_module_symbol(sym))
	return 1;

    if (sym->cas != NULL && !sym->attr.dummy && !sym->attr.allocatable)
	return 1;

    if (!g95_option.static_var)
	return 0;

    if (sym->ts.type == BT_CHARACTER &&
	(sym->ts.cl->length == NULL ||
	 sym->ts.cl->length->type != EXPR_CONSTANT))
	return 0;

    for(ns=sym->ns; ns; ns=ns->parent) {
	if (ns->state != COMP_SUBROUTINE && ns->state != COMP_FUNCTION)
	    continue;

	a = &ns->proc_name->attr;
	if (a->recursive || a->elemental || a->pure)
	    return 0;
    }

    return 1;
}



/* g95_module_symbol()-- Return nonzero if the symbol lives in a
 * module namespace or has been use-associated. */

int g95_module_symbol(g95_symbol *sym) {

    return (sym->ns->proc_name != NULL &&
	    sym->ns->proc_name->attr.flavor == FL_MODULE)
	|| sym->attr.use_assoc;
}



/* g95_derived_private()-- Given a derived type, return nonzero if it
 * has any private components or subtypes with private components. */

int g95_derived_private(g95_symbol *sym) {
g95_component *c;

    if (sym->component_access == ACCESS_PRIVATE)
	return 1;

    for(c=sym->components; c; c=c->next)
	if (!c->pointer && c->ts.type == BT_DERIVED &&
	    g95_derived_private(c->ts.derived))
	    return 1;

    return 0;
}



/* g95_pointer_component()-- Given a pointer to a symbol that is a
 * derived type, see if any components have the POINTER attribute.
 * The search is recursive if necessary.  Returns zero if no pointer
 * components are found, nonzero otherwise. */

int g95_pointer_component(g95_symbol *sym) {
g95_component *c;

    for(c=sym->components; c; c=c->next) {
	if (c->pointer)
	    return 1;

	if (c->ts.type == BT_DERIVED &&
	    g95_pointer_component(c->ts.derived))
	    return 1;
    }

    return 0;
}



/* g95_allocatable_component()-- See if a derived type ultimately
 * contains any allocatable components.  Returns nonzero if so. */

int g95_allocatable_component(g95_symbol *sym) {
g95_component *c;

    for(c=sym->components; c; c=c->next) {
	if (c->allocatable)
	    return 1;

	if (!c->pointer && c->ts.type == BT_DERIVED &&
	    g95_allocatable_component(c->ts.derived))
	    return 1;
    }

    return 0;
}



/* g95_initialized_component()-- Check to see if a derived type has a
 * default initialization at some level. */

int g95_initialized_component(g95_symbol *sym) {
g95_component *c;

    for(c=sym->components; c; c=c->next) {
	if (c->initializer != NULL)
	    return 1;

	if (!c->pointer && c->ts.type == BT_DERIVED &&
	    g95_initialized_component(c->ts.derived))
	    return 1;
    }

    return 0;
}



/* g95_coarray_component()-- Return nonzero if a derived type has a
 * coarray at some level. */

int g95_coarray_component(g95_symbol *sym) {
g95_component *c;

    for(c=sym->components; c; c=c->next) {
	if (c->cas != NULL)
	    return 1;

	if (!c->pointer && c->ts.type == BT_DERIVED &&
	    g95_coarray_component(c->ts.derived))
	    return 1;
    }

    return 0;
}



/* g95_pointer_array_component()-- Return nonzero if the derived type
 * has a pointer array component, which can also be an allocatable
 * array. */

int g95_pointer_array_component(g95_symbol *sym) {
g95_component *c;

    for(c=sym->components; c; c=c->next) {
	if (c->as != NULL && (c->pointer || c->allocatable))
	    return 1;

	if (!c->pointer && c->ts.type == BT_DERIVED &&
	    g95_pointer_array_component(c->ts.derived))
	    return 1;
    }

    return 0;
}



/* g95_symbol_state()-- Makes sure that no changes to symbols are pending */

void g95_symbol_state(void) {

    if (changed_syms != NULL)
	g95_internal_error("Symbol changes still pending");
}



/* g95_free_gsymbol()-- Free a global symbol and its children. */

void g95_free_gsymbol(g95_gsymbol *sym) {

    if (sym == NULL)
	return;

    g95_free_gsymbol(sym->left);
    g95_free_gsymbol(sym->right);

    g95_free(sym);
}



/* gsym_compare()-- Compare two symbols */

static int gsym_compare(g95_gsymbol *a, g95_gsymbol *b) {

    return strcmp(a->name, b->name);
}



/* find_gsymbol()-- Recursive function for g95_find_gsymbol. */

static g95_gsymbol *find_gsymbol(g95_gsymbol *sym, char *name) {
g95_gsymbol *s;

    if (sym == NULL)
	return NULL;

    if (strcmp(sym->name, name) == 0)
	return sym;

    s = find_gsymbol(sym->left, name);
    if (s != NULL)
	return s;

    s = find_gsymbol(sym->right, name);
    if (s != NULL)
	return s;

    return NULL;
}



/* g95_find_gsymbol()-- Search a tree for the global symbol. */

g95_gsymbol *g95_find_gsymbol(char *name) {

    return find_gsymbol(g95_gsym_root, name);
}



/* g95_get_gsymbol()-- Get a global symbol, creating it if it doesn't
 * exist. */

g95_gsymbol *g95_get_gsymbol(char *name) {
g95_gsymbol *s;

    s = g95_find_gsymbol(name);
    if (s != NULL)
	return s;

    s = g95_getmem(sizeof(g95_gsymbol));
    s->type = GSYM_UNKNOWN;
    s->name = g95_get_string(name);

    g95_insert_bbt(&g95_gsym_root, s, gsym_compare);

    return s;
}



/* g95_global_used()-- Come here to complain about a global symbol
 * already in use as something else. */

void g95_global_used(g95_gsymbol *sym, g95_locus *where) {
char *name;

    if (where == NULL)
	where = &g95_current_locus;

    switch(sym->type) {
    case GSYM_PROGRAM:      name = "PROGRAM";     break;
    case GSYM_FUNCTION:     name = "FUNCTION";    break;
    case GSYM_SUBROUTINE:   name = "SUBROUTINE";  break;
    case GSYM_COMMON:       name = "COMMON";      break;
    case GSYM_BLOCK_DATA:   name = "BLOCK DATA";  break;
    case GSYM_MODULE:       name = "MODULE";      break;
    default:
	g95_internal_error("g95_global_used(): Bad type");
	name = NULL;
    }

    g95_error("Global name '%s' at %L is already being used as a %s at %L",
	      sym->name, where, name, &sym->where);
}



/* g95_check_global()-- Check the usage of a global symbol.  If
 * everything goes OK, returns the global symbol. */

g95_gsymbol *g95_check_global(char *name, int type, g95_locus *where) {
g95_gsymbol *g;

    if (where == NULL)
	where = &g95_current_locus;

    g = g95_get_gsymbol(name);

    if (type == GSYM_COMMON || type == GSYM_MODULE)
	g95_update_locus(&g->where, where);

    if (g->type == GSYM_UNKNOWN) {
	g->type = type;
	g95_update_locus(&g->where, where);

    } else if (g->type != type) {
	g95_global_used(g, where);
	g = NULL;
    }

    return g;
}

