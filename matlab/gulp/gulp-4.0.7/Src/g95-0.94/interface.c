/* Deal with interfaces
   Copyright (C) 2000-2008  Free Software Foundation, Inc.
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


/* interface.c-- Deal with interfaces.  An explicit interface is
   represented as a singly linked list of formal argument structures
   attached to the relevant symbols.  For an implicit interface, the
   arguments don't point to symbols.  Explicit interfaces point to
   namespaces that contain the symbols within that interface.

   Implicit interfaces are linked together in a singly linked list
   along the next_if member of symbol nodes.  Since a particular
   symbol can only have a single explicit interface, the symbol cannot
   be part of multiple lists and a single next-member suffices.

   This is not the case for general classes, though.  An operator
   definition is independent of just about all other uses and has it's
   own head pointer.


Nameless interfaces:
   Nameless interfaces create symbols with explicit interfaces within
   the current namespace.  They are otherwise unlinked.

Generic interfaces:
   The generic name points to a linked list of symbols.  Each symbol
   has an explicit interface.  Each explicit interface has it's own
   namespace containing the arguments.  Module procedures are symbols in
   which the interface is added later when the module procedure is parsed.

User operators:
   User-defined operators are stored in a their own set of symtrees
   separate from regular symbols.  The symtrees point to g95_user_op
   structures which in turn head up a list of relevant interfaces.

Extended intrinsics and assignment:
   The head of these interface lists are stored in the containing namespace.

Implicit interfaces:
   An implicit interface is represented as a singly linked list of
   formal argument list structures that don't point to any symbol
   nodes-- they just contain types.


When a subprogram is defined, the program unit's name points to an
interface as usual, but the link to the namespace is NULL and the
formal argument list points to symbols within the same namespace as
the program unit name.

*/

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "g95.h"


extern int g95_forall_flag;


/* The current_interface structure holds information about the
 * interface currently being parsed.  This structure is saved and
 * restored during recursive interfaces. */

g95_interface_info current_interface;
static int compare_interfaces(g95_symbol *, g95_symbol *, int);

typedef struct {
    g95_formal_arglist *f;
    g95_actual_arglist *a;
} argpair;

typedef struct {
    int flag;
    g95_symbol *sym;
} arginfo;


typedef struct type_comparison {
    g95_typespec *ts1, *ts2;

    struct type_comparison *previous;
} type_comparison;


static type_comparison *comparison_root = NULL;



/* g95_free_interface()-- Frees a singly linked list of g95_interface
 * structures */

void g95_free_interface(g95_interface *intr) {
g95_interface *next;

    for(; intr; intr=next) {
	next = intr->next;
	g95_free(intr);
    }
}



/* g95_free_formal_arglist()-- Gets rid of a formal argument list.  We
 * do not free symbols.  Symbols are freed when a namespace is freed. */

void g95_free_formal_arglist(g95_formal_arglist *p) {
g95_formal_arglist *q;

    for(; p; p=q) {
	q = p->next;
	g95_free(p);
    }
}



/* fold_unary()-- Change the operators unary plus and minus into
 * binary plus and minus respectively, leaving the rest unchanged.  */

static int fold_unary(int operator) {

    switch(operator) {
    case INTRINSIC_UPLUS:   operator = INTRINSIC_PLUS;   break;
    case INTRINSIC_UMINUS:  operator = INTRINSIC_MINUS;  break;
    default: break;
    }

    return operator;
}



/* g95_compare_types()-- Compare two typespecs, recursively if
 * necessary.  Returns nonzero if equal. */

int g95_compare_types(g95_typespec *ts1, g95_typespec *ts2) {
g95_component *dt1, *dt2;
type_comparison c, *p;
int rc;

    if (ts1->type != ts2->type)
	return 0;

    if (ts1->type != BT_DERIVED)
	return (ts1->kind == ts2->kind);

/* Compare derived types. */

    if (ts1->derived == ts2->derived)
	return 1;

    if (ts1->derived->attr.itype == ts2->derived->attr.itype &&
	ts1->derived->attr.itype != ITYPE_NONE)
	return 1;

    if (ts1->derived->attr.itype != ITYPE_NONE ||
	ts2->derived->attr.itype != ITYPE_NONE)
	return 0;

/* Have we been here before? */

    for(p=comparison_root; p; p=p->previous)
	if (p->ts1 == ts1 && p->ts2 == ts2)
	    return 1;

    c.ts1 = ts1;
    c.ts2 = ts2;
    c.previous = comparison_root;

    comparison_root = &c;

/* Special case for comparing derived types across namespaces.  If the
 * true names and module names are the same and the module name is
 * nonnull, then they are equal. */

    if (g95_strcmp(ts1->derived->name, ts2->derived->name) == 0 &&
	ts1->derived->module != NULL &&
	g95_strcmp(ts1->derived->module, "(unique)") != 0 &&
	g95_strcmp(ts2->derived->module, "(unique)") != 0 &&
	g95_strcmp(ts1->derived->module, ts2->derived->module) == 0) {
	rc = 1;
	goto cleanup;
    }

/* Compare type via the rules of the standard.  Both types must have
 * the SEQUENCE attribute to be equal */

    if (g95_strcmp(ts1->derived->name, ts2->derived->name)) {
	rc = 0;
	goto cleanup;
    }

    dt1 = ts1->derived->components;
    dt2 = ts2->derived->components;

    rc = 0;

    if (ts1->derived->component_access == ACCESS_PRIVATE ||
	ts2->derived->component_access == ACCESS_PRIVATE) {
	goto cleanup;
    }

    if ((!ts1->derived->attr.sequence || !ts2->derived->attr.sequence) &&
	(!ts1->derived->attr.bind     || !ts2->derived->attr.bind))
	goto cleanup;

/* Since subtypes of SEQUENCE types must be SEQUENCE types as well, a
 * simple test can speed things up.  Otherwise, lots of things have to
 * match. */

    for(;;) {
	rc = 0;

	if (strcmp(dt1->name, dt2->name) != 0)
	    goto cleanup;

	if (dt1->pointer != dt2->pointer)
	    goto cleanup;

	if (dt1->dimension != dt2->dimension)
	    goto cleanup;

	if (dt1->dimension && g95_compare_array_spec(dt1->as, dt2->as) == 0)
	    goto cleanup;

	if (g95_compare_types(&dt1->ts, &dt2->ts) == 0)
	    goto cleanup;

	dt1 = dt1->next;
	dt2 = dt2->next;

	if (dt1 == NULL && dt2 == NULL)
	    break;

	if (dt1 == NULL || dt1 == NULL) {
	    rc = 0;
	    goto cleanup;
	}
    }

    rc = 1;

cleanup:
    comparison_root = c.previous;
    return rc;
}



/* g95_match_generic_spec()-- Match a generic specification.
 * Depending on which type of interface is found, the 'name' or
 * 'operator' pointers may be set.  This subroutine doesn't return
 * MATCH_NO. */

match g95_match_generic_spec(interface_type *type, char *name,
			     int *operator) {
char buffer[G95_MAX_SYMBOL_LEN+1];
match m;
int i;
 
    if (g95_match(" assignment ( = )") == MATCH_YES) {
	*type = INTERFACE_INTRINSIC_OP;
	*operator = INTRINSIC_ASSIGN;
	return MATCH_YES;
    }

    if (g95_match(" operator ( %o )", &i) == MATCH_YES) { /* Operator i/f */
	*type = INTERFACE_INTRINSIC_OP;
	*operator = fold_unary(i);
	return MATCH_YES;
    }

    if (g95_match(" operator ( ") == MATCH_YES) {
	m = g95_match_defined_op_name(buffer, 1);
	if (m == MATCH_NO) goto syntax;
	if (m != MATCH_YES) return MATCH_ERROR;

	m = g95_match_char(')');
	if (m == MATCH_NO) goto syntax;
	if (m != MATCH_YES) return MATCH_ERROR;

	strcpy(name, buffer);
	*type = INTERFACE_USER_OP;
	*operator = INTRINSIC_USER;
	return MATCH_YES;
    }

    if (g95_match_name(buffer) == MATCH_YES) {
	strcpy(name, buffer);
	*type = INTERFACE_GENERIC;    
	return MATCH_YES;
    }

    *type = INTERFACE_NAMELESS;
    return MATCH_YES;

syntax:
    g95_error("Syntax error in generic specification at %C");
    return MATCH_ERROR;
}



/* g95_match_interface()-- Match one of the five forms of an interface
 * statement. */

match g95_match_interface(void) {
char name[G95_MAX_SYMBOL_LEN+1];
interface_type type;
g95_symbol *sym;
int operator;
match m;

    m = g95_match_space();

    if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)
	return MATCH_ERROR;

    if (g95_match_eos() != MATCH_YES || 
	(type != INTERFACE_NAMELESS && m != MATCH_YES)) {
	g95_syntax_error(ST_INTERFACE);
	return MATCH_ERROR;
    }

    current_interface.type = type;

    switch(type) {
    case INTERFACE_GENERIC:
	sym = NULL;

	if (g95_find_symbol(name, NULL, 0, &sym) == 0 && sym != NULL) {
	    if (sym->attr.dummy) {
		g95_error("Dummy procedure '%s' at %C cannot have a generic "
			  "interface", name);
		return MATCH_ERROR;
	    }

	    if (sym->attr.flavor != FL_PROCEDURE &&
		g95_add_flavor(&sym->attr, FL_PROCEDURE, name,NULL) == FAILURE)
		return MATCH_ERROR;

	    if (sym->attr.pointer) {
		g95_error("Generic interface '%s' at %L cannot have the "
			  "POINTER attribute", sym->name, &g95_def_locus);
		return MATCH_ERROR;
	    }

	    if (sym->attr.target) {
		g95_error("Generic interface '%s' at %L cannot have the "
			  "TARGET attribute", sym->name, &g95_def_locus);
		return MATCH_ERROR;
	    }
	}

	current_interface.generic = g95_get_generic(name, NULL);

	if (sym != NULL && sym->attr.access != ACCESS_UNKNOWN)
	    current_interface.generic->access = sym->attr.access;

	g95_new_block = NULL;
	break;

    case INTERFACE_USER_OP:
	current_interface.uop = g95_get_uop(name);
	break;

    case INTERFACE_INTRINSIC_OP:
	current_interface.op = operator;
	break;

    case INTERFACE_NAMELESS:
	break;

    default:
	g95_internal_error("g95_match_interface(): Bad interface type");
    }

    return MATCH_YES;
}



/* g95_match_abstract_interface()-- Match an ABSTRACT INTERFACE
 * statement. */

match g95_match_abstract_interface(void) {

    if (g95_match_eos() != MATCH_YES) {
	g95_syntax_error(ST_INTERFACE);
	return MATCH_ERROR;
    }

    current_interface.type = INTERFACE_ABSTRACT;
    return MATCH_YES;
}



/* g95_match_end_interface()-- Match the different sort of
 * generic-specs that can be present after the END INTERFACE itself. */

match g95_match_end_interface(void) {
char name[G95_MAX_SYMBOL_LEN+1];
interface_type type;
int operator;
match m;

    m = g95_match_space();

    if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)
	return MATCH_ERROR;

    if (g95_match_eos() != MATCH_YES ||
	(type != INTERFACE_NAMELESS && m != MATCH_YES)) {
	g95_syntax_error(ST_END_INTERFACE);
	return MATCH_ERROR;
    }

    m = MATCH_YES;

    switch(current_interface.type) {
    case INTERFACE_NAMELESS:
	if (type != current_interface.type) {
	    g95_error("INTERFACE needs an END INTERFACE at %C");
	    m = MATCH_ERROR;
	}

	break;

    case INTERFACE_ABSTRACT:
	if (type != INTERFACE_NAMELESS) {
	    g95_error("ABSTRACT INTERFACE needs an END INTERFACE at %C");
	    m = MATCH_ERROR;
	}

	break;

    case INTERFACE_INTRINSIC_OP:
	if (type != current_interface.type ||
	    operator != current_interface.op) {

	    if (current_interface.op == INTRINSIC_ASSIGN)
		g95_error("Expected 'END INTERFACE ASSIGNMENT (=)' at %C");
	    else 
		g95_error("Expecting 'END INTERFACE OPERATOR (%s)' at %C",
			  g95_op2string(current_interface.op));

	    m = MATCH_ERROR;
	}

	break;

    case INTERFACE_USER_OP:
	/* Comparing the symbol node names is OK because only use-associated
	 * symbols can be renamed */

	if (type != current_interface.type || 
	    g95_strcmp(current_interface.uop->name, name) != 0) {
	    g95_error("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",
		      current_interface.uop->name);
	    m = MATCH_ERROR;
	}

	break;

    case INTERFACE_GENERIC:
	if (type != current_interface.type ||
	    g95_strcmp(current_interface.generic->name, name) != 0) {
	    g95_error("Expecting 'END INTERFACE %s' at %C",
		      current_interface.generic->name);
	    m = MATCH_ERROR;
	}

	break;
    }

    return m;
}



/* compare_type_rank()-- Given two symbols that are formal arguments,
 * compare their ranks and types.  Returns nonzero if they have the
 * same rank and type, zero otherwise. */

static int compare_type_rank(g95_symbol *s1, g95_symbol *s2) {
int r1, r2;

    r1 = (s1->as != NULL) ? s1->as->rank : 0;
    r2 = (s2->as != NULL) ? s2->as->rank : 0;

    if (r1 != r2)
	return 0;   /* Ranks differ */

    return g95_compare_types(&s1->ts, &s2->ts);
}



/* find_keyword_arg()-- Given a formal argument list and a keyword
 * name, search the list for that keyword.  Returns the correct symbol
 * node if found, NULL if not found. */

static g95_symbol *find_keyword_arg(char *name, g95_formal_arglist *f) {

    for(; f; f=f->next)
	if (f->sym != NULL && strcmp(f->sym->name, name) == 0)
	    return f->sym;

    return NULL;
}



/* dummy_procedure()-- Helper function for determining if a symbol
 * represents a dummy procedure. */

static int dummy_procedure(g95_symbol *sym) {

    return sym->attr.function || sym->attr.subroutine;
}



/* count_types_test()-- Given a pair of formal argument lists, we see
 * if the two lists can be distinguished by counting the number of
 * nonoptional arguments of a given type/rank in f1 and seeing if
 * there are less then that number of those arguments in f2 (including
 * optional arguments).  Since this test is asymmetric, it has to be
 * called twice to make it symmetric.  Returns nonzero if the argument
 * lists are incompatible by this test.  This subroutine implements
 * rule 1 of section 14.1.2.3. */

static int count_types_test(g95_formal_arglist *f1, g95_formal_arglist *f2) {
int rc, ac1, ac2, i, j, k, n1;
g95_formal_arglist *f;
arginfo *arg;

    n1 = 0;

    for(f=f1; f; f=f->next)
	n1++;

    /* Build an array of integers that gives the same integer to
     * arguments of the same type/rank.  */

    arg = g95_getmem(n1*sizeof(arginfo));

    f = f1;
    for(i=0; i<n1; i++, f=f->next) {
	arg[i].flag = -1;
	arg[i].sym = f->sym;
    }

    k = 0;

    for(i=0; i<n1; i++) {
	if (arg[i].flag != -1 || arg[i].sym == NULL)
	    continue;

	if (arg[i].sym->attr.optional || dummy_procedure(arg[i].sym))
	    continue;   /* Skip optional arguments, dummy procedures */

	arg[i].flag = k;

	/* Find other nonoptional arguments of the same type/rank */

	for(j=i+1; j<n1; j++)
	    if (arg[j].sym != NULL && !arg[j].sym->attr.optional &&
		!dummy_procedure(arg[j].sym) &&
		compare_type_rank(arg[i].sym, arg[j].sym))
		arg[j].flag = k;

	k++;
    }

    /* Now loop over each distinct type found in f1 */

    k = 0;
    rc = 0;

    for(i=0; i<n1; i++) {
	if (arg[i].flag != k)
	    continue;

	ac1 = 1;
	for(j=i+1; j<n1; j++)
	    if (arg[j].flag == k)
		ac1++;

	/* Count the number of arguments in f2 with that type, including
	 * those that are optional, but not dummy procedures. */

	ac2 = 0;

	for(f=f2; f; f=f->next)
	    if (arg[i].sym != NULL && f->sym != NULL &&
		!dummy_procedure(arg[i].sym) &&
		compare_type_rank(arg[i].sym, f->sym))
		ac2++;

	if (ac1 > ac2) {
	    rc = 1;
	    break;
	}

	k++;
    }

    g95_free(arg);
    return rc;
}



/* operator_correspondence()-- Perform the abbreviated correspondence
 * test for operators.  The arguments cannot be optional and are
 * always ordered correctly, which makes this test much easier than
 * that for generic tests.
 *
 * This subroutine is also used when comparing a formal and actual
 * argument list when an actual parameter is a dummy procedure.  At
 * that point, two formal interfaces must be compared for equality
 * which is what happens here. */

static int operator_correspondence(g95_formal_arglist *f1,
				   g95_formal_arglist *f2) {
    for(;;) {
	if (f1 == NULL && f2 == NULL) break;
	if (f1 == NULL || f2 == NULL) return 1;

	if (!compare_type_rank(f1->sym, f2->sym)) return 1;

	f1 = f1->next;
	f2 = f2->next;
    }

    return 0;
}



/* compare_parm()-- Special parameter comparison for the generic
 * correspondence test.  Returns nonzero if the same, zero if
 * different. */

static int compare_parm(g95_symbol *s1, g95_symbol *s2) {
g95_symbol *r1, *r2;
int rank1, rank2;

    if (s1->attr.flavor != FL_PROCEDURE && s2->attr.flavor != FL_PROCEDURE)
	return compare_type_rank(s1, s2);

    if (s1->attr.flavor != FL_PROCEDURE || s2->attr.flavor != FL_PROCEDURE)
	return 0;

    /* Pair of procedures.  According to F2003 16.2.3, arguments are
     * "distinguishable", if neither is a subroutine and they are TKR
     * compatible.  The interfaces themselves are not checked,
     * presumably because of dummy procedures that don't have
     * interfaces. */

    if (!s1->attr.function || !s2->attr.function)
	return 0;

    r1 = s1->result;
    r2 = s2->result;

    rank1 = (r1->as == NULL) ? 0 : r1->as->rank;
    rank2 = (r2->as == NULL) ? 0 : r2->as->rank;

    return (rank1 != rank2)
	? 0
	: g95_compare_types(&r1->ts, &r2->ts);
}



/* generic_correspondence()-- Perform the correspondence test in rule
 * 2 of section 14.1.2.3.  Returns zero if no argument is found that
 * satisfies rule 2, nonzero otherwise.  This test is not symmetric in
 * f1 and f2 and must be called twice.
 *
 * This test finds problems caused by sorting the actual argument list
 * with keywords.  For example:
 *
 * INTERFACE FOO
 *     SUBROUTINE F1(A, B)
 *         INTEGER :: A ; REAL :: B
 *     END SUBROUTINE F1
 * 
 *     SUBROUTINE F2(B, A)
 *         INTEGER :: A ; REAL :: B
 *     END SUBROUTINE F1
 * END INTERFACE FOO
 *
 * At this point, 'CALL FOO(A=1, B=1.0)' is ambiguous. */

static int generic_correspondence(g95_formal_arglist *f1,
				  g95_formal_arglist *f2) {
g95_formal_arglist *g, *h;
g95_symbol *sym;

    h = f2; 
    while(f1) {
	if (f1->sym == NULL || f1->sym->attr.optional ||
	    (f2 != NULL && compare_parm(f1->sym, f2->sym)))
	    goto next;

	/* Now search for a disambiguating keyword argument at or after
	 * the current positional non-match. */

	for(g=f1; g!=NULL; g=g->next) {
	    if (g->sym == NULL || g->sym->attr.optional)
		continue;

	    sym = find_keyword_arg(g->sym->name, h);
	    if (sym == NULL || !compare_parm(g->sym, sym))
		return 1;
	}

    next:
	f1 = f1->next;
	if (f2 != NULL)
	    f2 = f2->next;
    }

    return 0;
}



/* compare_interfaces()-- 'Compare' two formal interfaces
 * associated with a pair of symbols.  We return nonzero if there
 * exists an actual argument list that would be ambiguous between the
 * two interfaces, zero otherwise. */

static int compare_interfaces(g95_symbol *s1, g95_symbol *s2,
			      int generic_flag) {
g95_formal_arglist *f1, *f2;

    if (s1->attr.function != s2->attr.function &&
	s1->attr.subroutine != s2->attr.subroutine)
	return 0;   /* disagreement between function/subroutine */

    f1 = s1->formal;
    f2 = s2->formal;

    if (f1 == NULL && f2 == NULL) return 1;   /* Special case */

    if (count_types_test(f1, f2)) return 0;
    if (count_types_test(f2, f1)) return 0;

    if (generic_flag) {
	if (generic_correspondence(f1, f2)) return 0;
	if (generic_correspondence(f2, f1)) return 0;

    } else {
	if (operator_correspondence(f1, f2)) return 0;
    }

    return 1;
}


/* resolve_interface()-- Resolve an interface */

static void resolve_interface(g95_interface *p) {
g95_symbol *sym;
char *name;

    sym = p->sym;
    name = sym->name;

    for(;;) {
	if (sym->attr.if_source != IFSRC_UNKNOWN) {
	    p->sym = sym;
	    return;
	}

	if (sym->ns->parent == NULL)
	    break;

	g95_find_symbol(name, sym->ns->parent, 1, &sym);

	if (sym == NULL)
	    break;
    }
}



/* private_args()-- Check a formal argument list of a public interface
 * to make sure that it does not have any private type arguments.
 * Returns nonzero if we find such an argument. */

static int private_args(g95_interface *p) {
g95_formal_arglist *f;
  
    for(f=p->sym->formal; f; f=f->next)
	if (G95_STRICT_F95() && f->sym != NULL &&
	    f->sym->ts.type == BT_DERIVED &&
	    g95_symbol_access(f->sym) == ACCESS_PRIVATE) {
	    g95_error("Formal argument '%s' at %L cannot be a PRIVATE type in "
		      "a PUBLIC interface", f->sym->name, &f->where);
	    return 1;
	}

    return 0;
}



/* check_interface0()-- Given a pointer to an interface pointer,
 * remove duplicate interfaces and make sure that all symbols are
 * either functions or subroutines.  Returns nonzero if something goes
 * wrong. */

static int check_interface0(g95_interface *p, g95_access access,
			    char *interface_name) {
g95_interface *q, *qlast;
int extends;

    /* Make sure all symbols in the interface have been defined as
     * functions or subroutines. */

    extends = 0;

    for(; p; p=p->next) {
	resolve_interface(p);

	if (!p->sym->attr.function && !p->sym->attr.subroutine &&
	    !p->sym->attr.intrinsic) {
	    g95_error("Procedure '%s' in %s at %L is neither function nor "
		      "subroutine", p->sym->name, interface_name,
		      &p->sym->declared_at);
	    return 1;
	}

	if (access == ACCESS_PUBLIC && private_args(p))
	    return 1;

	extends |= !p->sym->attr.use_assoc;
    }

    /* Remove duplicate interfaces in this interface list */

    for(; p; p=p->next) {
	qlast = p;

	for(q=p->next; q;) {
	    if (p->sym != q->sym) {
		qlast = q;
		q = q->next;

	    } else {           /* Duplicate interface */
		qlast->next = q->next;
		g95_free(q);
		q = qlast->next;
	    }
	}
    }

    if (G95_STRICT_F() && access == ACCESS_PUBLIC &&
	g95_current_ns->state == COMP_MODULE && !extends) {
	g95_error("PUBLIC interface of %s in '%s' at %L must be extended "
		  "in F mode", interface_name, g95_current_ns->proc_name->name,
		  &g95_current_ns->proc_name->declared_at);
	return 1;
    }

    return 0;
}



/* check_interface1()-- Check lists of interfaces to make sure that no
 * two interfaces are ambiguous.  Duplicate interfaces (from the same
 * symbol) are OK here. */

static int check_interface1(g95_interface *p, int generic_flag,
			    char *interface_name) {
g95_interface *q;

    for(q=p; q; q=q->next)
	resolve_interface(q);

    for(; p; p=p->next)
	for(q=p->next; q; q=q->next) {
	    if (p->sym == q->sym)
		continue;   /* Duplicates OK here */

	    if (g95_strcmp(p->sym->name, q->sym->name) == 0 &&
		g95_strcmp(p->sym->module, q->sym->module) == 0)
		continue;

	    if (compare_interfaces(p->sym, q->sym, generic_flag)) {
		g95_error("Ambiguous interfaces '%s' and '%s' in %s at %L",
			  p->sym->name, q->sym->name, interface_name,
			  &p->where);
		return 1;
	    }
	}

    return 0;
}



/* check_operator_interface()-- Given an operator interface and the
 * operator, make sure that all interfaces for that operator are legal. */

static void check_operator_interface(g95_interface *intr, int operator) {
int args, opt1, opt2, a1, a2, binary_ok;
g95_formal_arglist *formal;
g95_typespec *ts1, *ts2;
g95_intent i1, i2;
g95_symbol *sym;

    if (intr == NULL)
	return; 

    args = 0;
    ts1  = ts2  = NULL;
    i1   = i2   = INTENT_UNKNOWN;
    opt1 = opt2 = 0;
    a1   = a2   = 0;

    for(formal=intr->sym->formal; formal; formal=formal->next) {
	sym = formal->sym;

	if (args == 0 && sym != NULL) {
	    ts1  = &sym->ts;
	    i1   = sym->attr.intent;
	    opt1 = sym->attr.optional;
	    a1   = (sym->as == NULL) ? 0 : sym->as->rank;
	}

	if (args == 1 && sym != NULL) {
	    ts2  = &sym->ts;
	    i2   = sym->attr.intent;
	    opt2 = sym->attr.optional;
	    a2   = (sym->as == NULL) ? 0 : sym->as->rank;
	}

	args++;
    }

    binary_ok = (a1 == a2) || (a1 == 0) || (a2 == 0);

    if (args == 0 || args > 2)
	goto num_args;

    sym = intr->sym;

    if (operator == INTRINSIC_ASSIGN) {
	if (!sym->attr.subroutine) {
	    g95_error("Assignment operator interface '%s' at %L must be a "
		      "SUBROUTINE", sym->name, &intr->where);
	    return;
	}

	for(formal=sym->formal; formal; formal=formal->next)
	    if (formal->sym == NULL) {
		g95_error("Assignment operator interface '%s' at %L cannot "
			  "have alternate returns", sym->name, &intr->where);
		return;
	    }

    } else {
	if (!sym->attr.function) {
	    g95_error("Intrinsic operator interface at %L must be a FUNCTION",
		      &intr->where);
	    return;
	}
    }

    switch(operator) {
    case INTRINSIC_PLUS:     /* Numeric unary or binary */
    case INTRINSIC_MINUS:
	if (!binary_ok)
	    break;

	if (args == 1 && g95_numeric_ts(ts1))
	    goto bad_repl;

	if (args == 2 && g95_numeric_ts(ts1) && g95_numeric_ts(ts2))
	    goto bad_repl;

	break;

    case INTRINSIC_POWER:    /* Binary numeric */
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:

    case INTRINSIC_EQ:
    case INTRINSIC_NE:
	if (!binary_ok)
	    break;

	if (args == 1)
	    goto num_args;

	if (g95_numeric_ts(ts1) && g95_numeric_ts(ts2))
	    goto bad_repl;

	if ((operator == INTRINSIC_EQ || operator == INTRINSIC_NE) &&
	    (ts1->type == BT_CHARACTER && ts2->type == BT_CHARACTER &&
	     ts1->kind == ts2->kind))
	    goto bad_repl;

	break;

    case INTRINSIC_GE:  /* Binary numeric operators that do not support */
    case INTRINSIC_LE:  /* complex numbers */
    case INTRINSIC_LT:
    case INTRINSIC_GT:
	if (!binary_ok)
	    break;

	if (args == 1)
	    goto num_args;

	if ((ts1->type == BT_INTEGER || ts1->type == BT_REAL) &&
	    (ts2->type == BT_INTEGER || ts2->type == BT_REAL))
	    goto bad_repl;

	if (ts1->type == BT_CHARACTER && ts2->type == BT_CHARACTER &&
	    ts1->kind == ts2->kind)
	    goto bad_repl;

	break;

    case INTRINSIC_OR:       /* Binary logical */
    case INTRINSIC_AND:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
	if (!binary_ok)
	    break;

	if (args == 1)
	    goto num_args;

	if (ts1->type == BT_LOGICAL && ts2->type == BT_LOGICAL)
	    goto bad_repl;

	break;

    case INTRINSIC_NOT:      /* Unary logical */
	if (args != 1)
	    goto num_args;

	if (ts1->type == BT_LOGICAL)
	    goto bad_repl;

	break;

    case INTRINSIC_CONCAT:   /* Binary string */
	if (!binary_ok)
	    break;

	if (args != 2)
	    goto num_args;

	if (ts1->type == BT_CHARACTER && ts2->type == BT_CHARACTER &&
	    ts1->kind == ts2->kind)
	    goto bad_repl;

	break;

    case INTRINSIC_ASSIGN:   /* Class by itself */
	if (args != 2)
	    goto num_args;

	if (a1 == 0 && a2 > 0)
	    break;

	if (g95_numeric_ts(ts1) && g95_numeric_ts(ts2))
	    goto bad_repl;

	if (ts1->type == BT_CHARACTER && ts2->type == BT_CHARACTER &&
	    ts1->kind == ts2->kind)
	    goto bad_repl;

	if (ts1->type == BT_LOGICAL && ts2->type == BT_LOGICAL)
	    goto bad_repl;

	break;
    }

    /* Check intents on operator interfaces */

    if (operator == INTRINSIC_ASSIGN) {
	if (i1 != INTENT_OUT && i1 != INTENT_INOUT)
	    g95_error("First argument of defined assignment at %L must be "
		      "INTENT(IN) or INTENT(INOUT)", &intr->where);

	if (i2 != INTENT_IN)
	    g95_error("Second argument of defined assignment at %L must be "
		      "INTENT(IN)", &intr->where);
    } else {
	if (i1 != INTENT_IN)
	    g95_error("First argument of operator interface at %L must be "
		      "INTENT(IN)", &intr->where);

	if (args == 2 && i2 != INTENT_IN)
	    g95_error("Second argument of operator interface at %L must be "
		      "INTENT(IN)", &intr->where);
    }

    if (opt1)
	g95_error("First argument of operator interface at %L cannot be "
		  "OPTIONAL", &intr->where);

    if (opt2)
	g95_error("Second argument of operator interface at %L cannot be "
		  "OPTIONAL", &intr->where);

    sym = intr->sym;

    if (operator != INTRINSIC_ASSIGN && sym->result->ts.type == BT_CHARACTER &&
	sym->result->ts.cl != &g95_unknown_charlen &&
	sym->result->ts.cl->length == NULL)
	g95_error("Operator interface of '%s' at %L cannot return an "
		  "assumed-length CHARACTER", sym->name, &intr->where);

    return;

bad_repl:
    g95_error("Operator interface at %L conflicts with the intrinsic "
	      "operation", &intr->where);
    return;

num_args:
    g95_error("Operator interface at %L has the wrong number of arguments",
	      &intr->where);
}



/* check_uop_interfaces()-- Check user operator interfaces. */

static void check_uop_interfaces(g95_user_op *uop) {
char interface_name[100];
g95_interface *p;

    sprintf(interface_name, "operator interface '%s'", uop->name);
    if (check_interface0(uop->operator, uop->access, interface_name))
	return;

    check_interface1(uop->operator, 0, interface_name);

    for(p=uop->operator; p; p=p->next)
	check_operator_interface(p, INTRINSIC_USER);
}



/* check_sym_interfaces()-- Check the generic and operator interfaces of
 * symbols to make sure that none of the interfaces conflict.  The
 * check has to be done after all of the symbols are actually loaded. */

static void check_sym_interfaces(g95_symtree *st) {
char interface_name[100];

    if (st == NULL)
	return;

    check_sym_interfaces(st->left);
    check_sym_interfaces(st->right);

    sprintf(interface_name, "generic interface '%s'", st->name);

    if (check_interface0(st->n.generic, st->access, interface_name))
	return;

    check_interface1(st->n.generic, 1, interface_name);
}



/* g95_check_interfaces()-- For the namespace, check generic, user
 * operator and intrinsic operator interfaces for consistency and to
 * remove duplicate interfaces.  We traverse the whole namespace,
 * counting on the fact that most symbols will not have generic or
 * operator interfaces. */

void g95_check_interfaces(g95_namespace *ns) {
char interface_name[100];
g95_namespace *old_ns;
int i;

    old_ns = g95_current_ns;
    g95_current_ns = ns;

    check_sym_interfaces(ns->generic_root);

    g95_traverse_user_op(ns, check_uop_interfaces);

    for(i=0; i<G95_INTRINSIC_OPS; i++) {
	if (i == INTRINSIC_USER || i == INTRINSIC_PAREN)
	    continue;

	if (i == INTRINSIC_ASSIGN)
	    strcpy(interface_name, "intrinsic assignment operator");

	else
	    sprintf(interface_name, "intrinsic '%s' operator",
		    g95_op2string(i));

	if (check_interface0(ns->operator[i], ns->operator_access[i],
			     interface_name))
	    continue;

	check_operator_interface(ns->operator[i], i);

	check_interface1(ns->operator[i], 0, interface_name);
    }

    g95_current_ns = old_ns;
}



/* symbol_rank()-- Return the rank of a symbol */

static int symbol_rank(g95_symbol *sym) {

    return (sym->as == NULL) ? 0 : sym->as->rank;
}



/* compare_formal()-- Compare formal interfaces of two procedures.
 * This is done when checking dummy procedures being passed to actual
 * procedures.  Returns zero if there is a mismatch. */

static int compare_formal(g95_symbol *s1, g95_symbol *s2) {
g95_formal_arglist *f1, *f2;
g95_symbol *a1, *a2;

    f1 = s1->formal; 
    f2 = s2->formal;

    for(; f1 != NULL && f2 != NULL; f1=f1->next, f2=f2->next) {
	a1 = f1->sym;
	a2 = f2->sym;

	if ((a1 == NULL) != (a1 == NULL) || a1 == NULL)
	    return 0;

	if ((a1->attr.flavor == FL_PROCEDURE) !=
	    (a2->attr.flavor == FL_PROCEDURE))
	    return 0;

	if (a1->attr.flavor != FL_PROCEDURE) {
	    if (!g95_compare_types(&a1->ts, &a2->ts))
		return 0;

	    if (a1->attr.intent != INTENT_UNKNOWN &&
		a2->attr.intent != INTENT_UNKNOWN &&
		a1->attr.intent != a2->attr.intent)
		return 0;

	} else {
	    if (a1->attr.subroutine != a1->attr.subroutine ||
		a1->attr.function != a1->attr.function)
		return 0;

	    if (!compare_formal(a1, a2))
		return 0;
	}
    }

    return (f1 == NULL && f2 == NULL);
}



/* compare_dummy_procedure()-- Compare formal and actual arguments
 * where the argument is a dummy procedure.  Returns zero if the two
 * are not compatible, nonzero if they are. */

static int compare_dummy_procedure(g95_symbol *formal, g95_expr *actual,
				   int error_flag) {
int i;

    if (formal->ts.type != BT_PROCEDURE &&
	formal->attr.flavor != FL_PROCEDURE &&
	!formal->attr.external && !formal->attr.intrinsic) {
	if (error_flag)
	    g95_error("Formal parameter '%s' at %L is not a PROCEDURE",
		      formal->name, &actual->where);
	return 0;
    }

    if (actual->ts.type != BT_PROCEDURE) {
	if (error_flag)
	    g95_error("Actual parameter '%s' at %L is not a PROCEDURE",
		      formal->name, &actual->where);
	return 0;
    }

    if (formal->attr.function && actual->symbol->attr.subroutine) {
	if (error_flag)
	    g95_error("Actual procedure at %L is a FUNCTION where dummy "
		      "procedure is a SUBROUTINE", &actual->where);
	return 0;
    }

    if (formal->attr.subroutine && actual->symbol->attr.function) {
	if (error_flag)
	    g95_error("Actual procedure at %L is a SUBROUTINE where dummy "
		      "procedure is a FUNCTION", &actual->where);
	return 0;
    }

    if ((formal->attr.function || formal->ts.type != BT_UNKNOWN) &&
	!formal->attr.subroutine && !actual->symbol->attr.subroutine &&
	(actual->symbol->attr.function ||
	 (actual->symbol->ts.type != BT_UNKNOWN &&
	  actual->symbol->ts.type != BT_PROCEDURE)) &&

	formal->ts.interface != NULL && formal->ts.interface->result != NULL &&
	actual->symbol->result != NULL &&
	!compare_type_rank(formal->ts.interface->result,
			   actual->symbol->result)) {
	if (error_flag)
	    g95_error("Type/rank of actual function at %L does not match the "
		      "dummy function", &actual->where);
	return 0;
    }

    if ((formal->attr.pure || formal->attr.elemental) &&
	!actual->symbol->attr.pure && !actual->symbol->attr.elemental) {
	if (error_flag)
	    g95_error("Dummy procedure '%s' at %L must be PURE",
		      actual->symbol->name, &actual->where);
	return 0;
    }

    if (formal->attr.if_source == IFSRC_UNKNOWN ||
	actual->symbol->attr.if_source == IFSRC_UNKNOWN)
	return 1;

    i = compare_formal(formal, actual->symbol);

    if (i == 0 && error_flag)
	g95_error("Interface of actual procedure does not match interface "
		  "of dummy procedure at %L", &actual->where);

    return i;
}


/* array_element()-- Return nonzero if the expression is ultimately an
 * array element. */

static int array_element(g95_expr *e) {
g95_ref *ref;
int flag;

    if (e->type != EXPR_VARIABLE || e->ref == NULL || e->rank > 0)
	return 0;

    flag = 0;

    for(ref=e->ref; ref; ref=ref->next) {
	if (ref->type != REF_ARRAY)
	    flag = 0;

	else if (ref->u.ar.type == AR_ELEMENT)
	    flag = 1;

	else if (ref->u.ar.type == AR_FULL || ref->u.ar.type == AR_SECTION) {
	    flag = 0;
	}
    }

    return flag;
}



/* compare_parameter()-- Given a symbol of a formal argument list and
 * an expression, see if the two are compatible as arguments.  Returns
 * nonzero if compatible, zero if not compatible. */

static int compare_parameter(g95_symbol *formal, g95_actual_arglist *a,
			     int elemental, int st_function, int error_flag) {
g95_expr *actual, *l1, *l2;
int rc, formal_rank;
array_type type;
bignum s1, s2;
comparison c;

    actual = a->u.expr;

    if (actual->ts.type == BT_PROCEDURE || formal->attr.flavor == FL_PROCEDURE)
	return compare_dummy_procedure(formal, actual, error_flag);

    if (actual->ts.type != BT_UNKNOWN &&
	!g95_compare_types(&formal->ts, &actual->ts)) {

	if (error_flag) {
	    if (formal->ts.type == BT_DERIVED &&
		actual->ts.type == BT_DERIVED &&
		strcmp(formal->ts.derived->name, actual->ts.derived->name)==0)
		g95_error("Type mismatch in parameter '%s' at %L.  "
			  "%s is not the same type between formal/actual",
			  formal->name, &a->u.expr->where,
			  g95_typename(&formal->ts));

	    else
		g95_error("Type mismatch in parameter '%s' at %L.  Passing "
			  "%s to %s", formal->name, &a->u.expr->where,
			  g95_typename(&actual->ts),
			  g95_typename(&formal->ts));
	}

	return 0;
    }

    if (actual->ts.type == BT_CHARACTER && actual->ts.cl != NULL &&
	formal->ts.cl != NULL) {
	l1 = actual->ts.cl->length;
	l2 = formal->ts.cl->length;

	c = (l1 != NULL && l2 != NULL)
	    ? g95_compare_expr(l1, l2)
	    : CMP_UNKNOWN;

	if (error_flag && c != CMP_EQ && c != CMP_UNKNOWN &&
	    formal->as != NULL && formal->as->type == AS_ASSUMED_SHAPE) {
	    g95_error("Character length of dummy argument at %L must agree "
		      "with assumed shape array", &a->u.expr->where);
	    return 0;
	}

	if (error_flag && c == CMP_LT)
	    g95_warning(135, "Actual character argument at %L is shorter in "
			"length than the formal argument", &actual->where);

	if (error_flag && c != CMP_EQ && c != CMP_UNKNOWN &&
	    formal->attr.pointer) {
	    g95_error("Character pointer lengths do not agree at %L",
		      &a->u.expr->where);
	    return 0;
	}
    }

    if (actual->type != EXPR_VARIABLE && !st_function &&
	(formal->attr.intent == INTENT_OUT ||
	 formal->attr.intent == INTENT_INOUT)) {
	if (error_flag)
	    g95_error("Argument for parameter '%s' at %L is INTENT(%s) and "
		      "actual argument is not a variable", formal->name,
		      &actual->where, g95_intent_string(formal->attr.intent));
	return 0;
    }

    formal_rank = symbol_rank(formal);

    if (formal_rank > 0 && actual->rank > 0 && actual->type != EXPR_NULL &&
	(formal->as->type == AS_ASSUMED_SHAPE ||
	 formal->as->type == AS_DEFERRED)) {
	a->type = ARG_ARRAY_DESC;

	if (actual->type == EXPR_VARIABLE)
	    actual->symbol->attr.desc = 1;
    }

    a->intent = formal->attr.intent;

    if (actual->type == EXPR_NULL && actual->rank == -1) {
	actual->rank = formal_rank;

	if (formal_rank > 0)
	    a->type = ARG_ARRAY_DESC;
    }

    if (formal->cas != NULL && !g95_coarray_expr(actual)) {
	if (error_flag)
	    g95_error("Argument for coarray parameter '%s' at %L must "
		      "be a coarray", formal->name, &actual->where);

	return 0;
    }

    if (g95_coindexed_expr(actual) && actual->ts.type == BT_DERIVED &&
	g95_allocatable_component(actual->ts.derived) &&
	formal->attr.intent != INTENT_IN) {

	g95_error("Coindexed argument '%s' at %L with an ALLOCATABLE"
		  " component must be to an INTENT(IN) parameter",
		  formal->name, &actual->where);
	return 0;
    }

/* If we're checking use, ranks can disagree.  If we're checking
 * interfaces, ranks have to agree. */

    if (!error_flag)
	return (elemental && formal_rank == 0) ||
	    (formal_rank == actual->rank);

    /* Scalar to scalar */

    if (formal_rank == 0 && actual->rank == 0)
	return 1;

    /* Array to array. */

    if (formal_rank > 0 && actual->rank > 0) {
	if (formal_rank != actual->rank &&
	    (formal->as->type == AS_ASSUMED_SHAPE ||
	     formal->as->type == AS_DEFERRED)) {

	    if (error_flag)
		g95_error("Rank mismatch for assumed-shape array in parameter"
			  " '%s' at %L", formal->name, &actual->where);
	    return 0;
	}

	if (formal->as->type == AS_ASSUMED_SHAPE &&
	    actual->type == EXPR_VARIABLE && actual->symbol->as != NULL &&
	    actual->symbol->as->type == AS_ASSUMED_SIZE &&
	    actual->ref->type == REF_ARRAY &&
	    actual->ref->u.ar.type == AR_FULL) {

	    if (error_flag)
		g95_error("Passing assumed-size array to assumed-shape "
			  "array at %L", &actual->where);
	    return 0;
	}

	if (!g95_allocatable_expr(actual) && formal->attr.allocatable) {
	    if (error_flag)
		g95_error("Passing non-ALLOCATABLE array to ALLOCATABLE "
			  "dummy at %L", &actual->where);
	    return 0;
	}

	if (formal->attr.allocatable && !g95_option.tr15581 &&
	    G95_STRICT_F95())
	    g95_warning(134, "ALLOCATABLE array argument at %L is not part of "
			"standard fortran 95", &actual->where);

	rc = 1;

	s1 = g95_array_size(actual);
	s2 = g95_array_spec_size(formal->as);

	if (actual->ts.type == BT_CHARACTER && s1 != NULL && s2 != NULL) {
	    if (actual->ts.cl->length != NULL &&
		actual->ts.cl->length->type == EXPR_CONSTANT)
		s1 = bi_multiply(s1, actual->ts.cl->length->value.integer);
	    else {
		big_free(s1);
		s1 = NULL;
	    }

	    if (s2 != NULL && formal->ts.cl->length != NULL &&
		formal->ts.cl->length->type == EXPR_CONSTANT)
		s2 = bi_multiply(s2, formal->ts.cl->length->value.integer);
	    else {
		big_free(s2);
		s2 = NULL;
	    }
	}

	if (s1 != NULL && s2 != NULL &&
	    bi_compare(big_copy(s1), big_copy(s2)) < 0) {
	    g95_error("Array argument at %L is smaller than the dummy size",
		      &actual->where);
	    rc = 0;
	}

	if (s1 != NULL) big_free(s1);
	if (s2 != NULL) big_free(s2);

	return rc;
    }

    /* Array to scalar.  The reference must be elemental. */

    if (formal_rank == 0 && actual->rank > 0) {
	if (elemental)
	    return 1;

	if (error_flag)
	    g95_error("Cannot pass array to scalar parameter '%s' at %L",
		      formal->name, &a->u.expr->where);
	return 0;
    }

    /* Scalar to array.  The array cannot be assumed-shape and the final
     * reference of the actual argument must be an array element. */

    if (actual->ts.type == BT_CHARACTER) {
	if (formal->as != NULL) {
	    type = formal->as->type;
	    if (type == AS_ASSUMED_SHAPE || type == AS_DEFERRED)
		goto error;
	}

	if (!G95_STRICT_F95())
	    return 1;
    }

    if (!array_element(actual) || actual->symbol->attr.pointer)
	goto error;

    if (formal->as != NULL && formal->as->type == AS_ASSUMED_SHAPE)
	goto error;

    a->type = ARG_ARRAY_ELEMENT;
    return 1;

error:
    if (error_flag)
	g95_error("Cannot pass scalar to array argument '%s' at %L",
		  formal->name, &a->u.expr->where);
    return 0;
}



/* compare_actual_formal()-- Given formal and actual argument lists,
 * see if they are compatible.  If they are compatible, the actual
 * argument list is sorted to correspond with the formal list, and
 * elements for missing optional arguments are inserted.
 *
 * If the 'where' pointer is nonnull, then we issue errors when things
 * don't match instead of just returning the status code. */

static int compare_actual_formal(g95_actual_arglist **ap,
				 g95_formal_arglist *formal, int elemental,
				 int st_function, g95_locus *where) {
g95_actual_arglist **new, *a, *actual, temp;
g95_formal_arglist *f;
int i, n, na;

    actual = *ap;

    for(a=actual; a; a=a->next)
	if (a->type != ARG_ALT_RETURN)
	    a->type = (a->u.expr == NULL || a->u.expr->rank == 0 ||
		       a->u.expr->type == EXPR_NULL)
		? ARG_EXPR
		: ARG_ARRAY;

    if (actual == NULL && formal == NULL)
	return 1;

    n = 0;
    for(f=formal; f; f=f->next)
	n++;

    new = (g95_actual_arglist **) alloca(n*sizeof(g95_actual_arglist *));

    for(i=0; i<n; i++)
	new[i] = NULL;

    na = 0;
    f = formal;
    i = 0;

    for(a=actual; a; a=a->next, f=f->next) {
	if (a->name != NULL) {
	    i = 0;
	    for(f=formal; f; f=f->next, i++) {
		if (f->sym == NULL)
		    continue;

		if (strcmp(f->sym->name, a->name) == 0)
		    break;
	    }

	    if (f == NULL) {
		if (where)
		    g95_error("Keyword argument '%s' at %L is not in the "
			      "procedure", a->name, &a->start);
		return 0;
	    }

	    if (new[i] != NULL) {
		if (where)
		    g95_error("Keyword argument '%s' at %L is already "
			      "associated with another actual argument",
			      a->name, &a->start);
		return 0;
	    }
	}

	if (f == NULL) {
	    if (where)
		g95_error("More actual than formal arguments in procedure "
			  "call at %L", where);
	    return 0;
	}

	if (a->type == ARG_ALT_RETURN) {
	    if (f->sym == NULL)
		goto match;

	    if (where)
		g95_error("Unexpected alternate return spec in subroutine "
			  "call at %L", where);

	    return 0;
	}

	/* The argument is an expression */

	if (f->sym == NULL) {
	    if (where)
		g95_error("Missing alternate return spec in subroutine call "
			  "at %L", where);
	    return 0;
	}

	if (a->u.expr == NULL)
	    goto match;

	if (!compare_parameter(f->sym, a, elemental, st_function,
			       where != NULL))
	    return 0;

	if (a->cb == CB_VALUE && f->cb == CB_REFERENCE) {
	    g95_error("Parameter '%s' at %L is passed by value instead of by "
		      "reference", f->sym->name, where);
	    return 0;
	}

	if (a->cb == CB_REFERENCE && f->cb == CB_VALUE) {
	    g95_error("Parameter '%s' at %L is passed by reference instead "
		      "of by value", f->sym->name, where);
	    return 0;
	}

	if (a->cb == CB_NONE && (f->cb == CB_VALUE || f->sym->attr.value))
	    a->cb = CB_VALUE;

	/* Make sure we have a pointer if required */

	a->pointer = 0;

	if (g95_pointer_expr(a->u.expr)) {
	    if (f->sym->attr.pointer &&
		(a->u.expr->type != EXPR_VARIABLE ||
		 a->u.expr->symbol->attr.flavor != FL_PROCEDURE))
		a->pointer = 1;  /* Passing a pointer */

	} else if (f->sym->attr.pointer) {
	    if (where)
		g95_error("Actual argument for '%s' must be a pointer at %L",
			  f->sym->name, &a->u.expr->where);
	    return 0;
	}

    match:
	if (a == actual)
	    na = i;

	new[i++] = a;
    }

    /* Make sure missing actual arguments are optional */

    i = 0;
    for(f=formal; f; f=f->next, i++) {
	if (new[i] != NULL)
	    continue;

	if (f->sym != NULL && !f->sym->attr.optional) {
	    if (where)
		g95_error("Missing actual argument for argument '%s' at %L",
			  f->sym->name, where);
	    return 0;
	}
    }

    /* The argument lists are compatible.  We now relink a new actual
     * argument list with null arguments in the right places.  The
     * head of the list remains the head. */

    for(i=0; i<n; i++)
	if (new[i] == NULL)
	    new[i] = g95_get_actual_arglist();

    if (na != 0) {
	temp = *new[0];
	*new[0] = *actual;
	*actual = temp;

	a = new[0];
	new[0] = new[na];
	new[na] = a;
    }

    for(i=0; i<n-1; i++)
	new[i]->next = new[i+1];

    new[i]->next = NULL;

    if (*ap == NULL && n > 0)
	*ap = new[0];

    /* Copy types for missing arguments */

    for(a=*ap, f=formal; a; a=a->next, f=f->next)
	if (a->type != ARG_ALT_RETURN && a->u.expr == NULL && f->sym != NULL) {
	    a->missing_arg_type = f->sym->ts.type;

	    if (f->sym->as != NULL && f->sym->as->type == AS_ASSUMED_SHAPE)
		a->type = ARG_ARRAY_DESC;
	}

    return 1;
}



/* vector_section()-- Return nonzero if an actual argument has a
 * vector subscript in it. */

static int vector_section(g95_expr *e) {
g95_ref *ref;
int i;

    if (e == NULL || e->type != EXPR_VARIABLE || e->rank == 0)
	return 0;

    for(ref=e->ref; ref; ref=ref->next) {
	if (ref->type != REF_ARRAY || ref->u.ar.type != AR_SECTION)
	    continue;

	for(i=0; i<ref->u.ar.dimen; i++)
	    if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
		return 1;
    }

    return 0;
}



/* implicit_interface()-- For a symbol without an interface, massage
 * the actual argument list. */

static try implicit_interface(g95_actual_arglist *actual) {
g95_symbol *sym;

    for(; actual; actual=actual->next) {
	if (actual->type != ARG_ALT_RETURN && actual->u.expr != NULL &&
	    actual->u.expr->type == EXPR_VARIABLE) {
	    sym = actual->u.expr->symbol;
	    g95_set_usage(sym, NULL, 1, 1);
	}

	if (actual->name != NULL) {
	    g95_error("Procedure with a keyword argument at %L does not have "
		      "an explicit interface", &actual->u.expr->where);
	    return FAILURE;
	}

	if (actual->type == ARG_ALT_RETURN || actual->u.expr == NULL)
	    continue;

	if (array_element(actual->u.expr) &&
	    actual->u.expr->symbol->as != NULL &&
	    actual->u.expr->symbol->as->type != AS_DEFERRED &&
	    actual->u.expr->symbol->as->type != AS_ASSUMED_SHAPE)
	    actual->type = ARG_ARRAY_ELEMENT;

	else if (actual->u.expr->rank > 0 && actual->u.expr->type != EXPR_NULL)
	    actual->type = ARG_ARRAY;
    }

    return SUCCESS;
}



/* elemental_conformance()-- For an elemental procedure make sure that 
 * all the array arguments conform with one another. */

static void elemental_conformance(g95_actual_arglist *a) {
g95_actual_arglist *p, *q;

    for(p=a; p; p=p->next) {
	if (p->type == ARG_ALT_RETURN || p->u.expr == NULL ||
	    p->u.expr->rank == 0)
	    continue;

	for(q=p->next; q; q=q->next) {
	    if (q->type == ARG_ALT_RETURN || q->u.expr == NULL ||
		q->u.expr->rank == 0)
		continue;

	    if (g95_check_conformance("ELEMENTAL procedure arguments",
				      p->u.expr, q->u.expr) == FAILURE)
		return;
	}
    }
}



/* g95_arg_usage()-- Given an expression that is an actual argument
 * and the intent of that argument, mark the underlying symbol as used
 * or set according to the intent. */

void g95_arg_usage(g95_intent intent, g95_expr *e) {
g95_symbol *sym;

    if (e->type != EXPR_VARIABLE)
	return;

    sym = e->symbol;

    switch(intent) {
    case INTENT_UNKNOWN:
	g95_set_usage(sym, &e->where, 1, 1);  /* Conservative assumption */
	break;

    case INTENT_IN:
	g95_set_usage(sym, &e->where, 0, 1);
	break;

    case INTENT_OUT:
	g95_set_usage(sym, &e->where, 1, 0);
	break;

    case INTENT_INOUT:
	g95_set_usage(sym, &e->where, 1, 1);
	break;

    default:
	break;
    }
}



/* check_intent()-- Check the intent of a single argument. */

static try check_intent(g95_intent formal_intent, g95_expr *a, int pointer) {
g95_intent a_intent;
g95_symbol *sym;

    if (a->type == EXPR_CONSTANT) {
	if (formal_intent == INTENT_OUT || formal_intent == INTENT_INOUT) {
	    g95_error("Constant actual argument %L to INTENT(%s) dummy "
		      "argument", &a->where, g95_intent_string(formal_intent));
	    return FAILURE;
	}

	return SUCCESS;
    }

    if (a->type != EXPR_VARIABLE) {
	if (formal_intent == INTENT_OUT || formal_intent == INTENT_INOUT) {
	    g95_error("Non-variable argument %L to INTENT(%s) dummy argument", 
		      &a->where, g95_intent_string(formal_intent));
	    return FAILURE;
	}

	return SUCCESS;
    }

    /* Argument is EXPR_VARIABLE */

    sym = a->symbol;
    a_intent = sym->attr.intent;

    if (sym->ts.type == BT_PROCEDURE && !sym->attr.pointer) {
	if (formal_intent != INTENT_UNKNOWN)
	    g95_error("PROCEDURE argument at %L must not have an INTENT",
		      &a->where);

	return FAILURE;
    }

    if (sym->attr.flavor == FL_PARAMETER &&
	(formal_intent == INTENT_OUT || formal_intent == INTENT_INOUT)) {
	g95_error("PARAMETER '%s' is associated with an INTENT(%s) dummy "
		  "argument at %L", sym->name, g95_intent_string(formal_intent),
		  &a->where);
	return FAILURE;
    }

    g95_arg_usage(formal_intent, a);

    if (a_intent == INTENT_IN &&
	(formal_intent == INTENT_INOUT || formal_intent == INTENT_OUT)) {

	g95_error("Procedure argument at %L is INTENT(IN) while interface "
		  "specifies INTENT(%s)", &a->where,
		  g95_intent_string(formal_intent));
	return FAILURE;
    }

    if (g95_pure(NULL, 1) && g95_impure_variable(sym)) {
	if (formal_intent == INTENT_INOUT || formal_intent == INTENT_OUT) {
	    g95_error("Procedure argument at %L is local to a PURE procedure "
		      "and is passed to an INTENT(%s) argument", &a->where,
		      g95_intent_string(formal_intent));
	    return FAILURE;
	}

	if (pointer) {
	    g95_error("Procedure argument at %L is local to a PURE procedure "
		      "and has the POINTER attribute", &a->where);
	    return FAILURE;
	}
    }

    if ((formal_intent == INTENT_OUT || formal_intent == INTENT_INOUT) &&
	vector_section(a)) {
	g95_error("Actual argument at %L is a vector subscript associated "
		  "with an INTENT(OUT)/INTENT(INOUT) argument", &a->where);
	return FAILURE;
    }

    return SUCCESS;
}



/* check_intents()-- Given formal and actual argument lists that
 * correspond to one another, check that they are compatible in the
 * sense that intents are not mismatched.  */

static try check_intents(g95_formal_arglist *f, g95_actual_arglist *a,
			 int st_function) {
g95_intent formal_intent;
try t;

    t = SUCCESS;

    for(;; f=f->next, a=a->next) {
	if (f == NULL && a == NULL)
	    break;

	if (f == NULL || a == NULL)
	    g95_internal_error("check_intents(): List mismatch");

	if (a->type == ARG_ALT_RETURN || a->u.expr == NULL)
	    continue;

	formal_intent = f->sym->attr.intent;
	if (st_function)
	    formal_intent = INTENT_UNKNOWN;

	if (check_intent(formal_intent, a->u.expr, a->pointer) == FAILURE)
	    t = FAILURE;
    }

    return t;
}



/* g95_procedure_use()-- Check how a procedure is used against its
 * interface.  If all goes well, the actual argument list will also
 * end up being properly sorted. */

void g95_procedure_use(g95_symbol *sym, g95_actual_arglist **ap,
		       g95_locus *where) {
g95_formal_arglist *f, *formal;
g95_actual_arglist *a;
g95_namespace *ns;
g95_symbol *s;
int flag;

    if (sym == NULL || sym->attr.if_source == IFSRC_UNKNOWN) {
	implicit_interface(*ap);

	if (g95_option.implicit_interface)
	    g95_warning(165, "Implicit interface '%s' called at %L",
			sym->name, where);
	return;
    }

    if (sym->attr.iproc != IPROC_NONE)
	return;

    if (sym->attr.proc != PROC_ST_FUNCTION && sym->attr.proc != PROC_INTERNAL)
	for(ns=sym->ns; ns; ns=ns->parent)
	    if (g95_find_common(sym->name, ns) != NULL) {
		g95_error("COMMON name '%s' conflicts with procedure name "
			  "at %L", sym->name, where);
		return;
	    }

    formal = sym->formal;

    if (!compare_actual_formal(ap, formal, sym->attr.elemental,
			       sym->attr.proc == PROC_ST_FUNCTION, where))
	return;

    check_intents(formal, *ap, sym->attr.proc == PROC_ST_FUNCTION);

    if (sym->attr.elemental) {
	elemental_conformance(*ap);

	flag = 0;

	for(a=*ap; a; a=a->next)
	    if (a->u.expr != NULL && a->u.expr->rank > 0) {
		flag = 1;
		break;
	    }

	if (flag) {
	    a = *ap;

	    for(f=formal; f; f=f->next, a=a->next) {
		s = f->sym;

		if ((s->attr.intent == INTENT_OUT ||
		     s->attr.intent == INTENT_INOUT) &&
		    a->u.expr != NULL && a->u.expr->rank == 0) {
		    g95_error("Scalar argument at %L to an ELEMENTAL "
			      "procedure must be a conforming array",
			      &a->u.expr->where);
		    break;
		}
	    }
	}
    }
}



/* g95_search_interface()-- Given an interface pointer and an actual
 * argument list, search for a formal argument list that matches the
 * actual.  If found, returns a pointer to the symbol of the correct
 * interface.  Returns NULL if not found. */

g95_symbol *g95_search_interface(g95_interface *intr, int sub_flag,
				 g95_actual_arglist **ap) {
g95_interface *match;
g95_actual_arglist *a;

    for(a=*ap; a; a=a->next) {
	if (a->type == ARG_EXPR && a->u.expr != NULL &&
	    a->u.expr->type == EXPR_NULL && a->u.expr->ts.type == BT_UNKNOWN) {

	    g95_error("Typeless NULL() at %L must have a MOLD argument",
		      &a->u.expr->where);
	    return NULL;
	}
    }

    match = NULL;

    for(; intr; intr=intr->next) {
	if ((sub_flag && intr->sym->attr.function) ||
	    (!sub_flag && intr->sym->attr.subroutine))
	    continue;

	/* Non-elemental references have priority over elemental
	 * references. */

	if (compare_actual_formal(ap, intr->sym->formal,
				  intr->sym->attr.elemental, 0, NULL)) {
	    match = intr;
	    if (!intr->sym->attr.elemental)
		break;
	}
    }

    if (match != NULL)
	check_intents(match->sym->formal, *ap, 0);

    return (match == NULL)
	? NULL
	: match->sym;
}



/* g95_self_interface()-- Deal with a bizarre case that occasionally
 * comes up, mostly from C programmers-- an explicit interface that
 * has the same name as the function being compiled.  For module
 * procedures or recursive subroutines or functions with explicit
 * results, this is illegal.  Otherwise it appears to be legal but
 * useless.  Returns 0 if everything is OK, 1 if there was an error or
 * 2 if there wasn't a self-interface issue. */

int g95_self_interface(char *name, g95_symbol **result) {
g95_state_data *p;
g95_symbol *sym;
char *self;

    if (g95_current_state() != COMP_INTERFACE ||
	current_interface.type != INTERFACE_NAMELESS ||
	g95_state_stack->previous == NULL)
	return 2;

    p = g95_state_stack->previous;

    if ((p->state != COMP_SUBROUTINE && p->state != COMP_FUNCTION) ||
	(strcmp(p->sym->name, name) != 0))
	return 2;

    /* We have a self-interface at this point.  Make sure it isn't illegal */

    if (p->sym->attr.proc == PROC_MODULE ||
	(p->sym->attr.recursive &&
	 (p->sym->attr.subroutine ||
	  (p->sym->attr.function && p->sym->result != p->sym)))) {
	g95_error("Interface of recursive procedure '%s' at %C is already"
		  " implicit", name);
	return 1;
    }

    /* Deal with the problem.  Create a fake symbol that is inaccessible
     * for other purposes and return that. */

    self = "SELF";
  
    if (g95_find_symbol(self, g95_current_ns->parent, 0, &sym))
	return 1;

    if (sym != NULL) {
	g95_error("Duplicate interface of '%s' at %C", name);
	return 1;
    }

    g95_get_symbol(self, g95_current_ns->parent, &sym);
    sym->name = g95_get_string(name);

    *result = sym;
    return 0;
}



/* g95_extend_assign()-- Tries to replace an assignment code node with
 * a subroutine call to the subroutine associated with the assignment
 * operator.  Returns zero if the assignment was successfully
 * replaced, one if the assignment was not replaced, or two if
 * something went wrong. */

int g95_extend_assign(g95_code *c, g95_namespace *ns) {
g95_actual_arglist *actual;
g95_expr *e, *lhs, *rhs;
g95_symbol *sym;

    lhs = c->expr;
    rhs = c->expr2;

    /* Don't allow an intrinsic assignment to be replaced */

    if (lhs->ts.type != BT_DERIVED && rhs->ts.type != BT_DERIVED &&
	((lhs->rank == 0 && rhs->rank == 0) || rhs->rank == 0 ||
	 lhs->rank == rhs->rank) &&
	(lhs->ts.type == rhs->ts.type ||
	 (g95_numeric_ts(&lhs->ts) && g95_numeric_ts(&rhs->ts))))
	return 1;

    actual = g95_get_actual_arglist();
    actual->type = ARG_EXPR;
    actual->u.expr = lhs;
    actual->dealloc = 1;

    actual->next = g95_get_actual_arglist();
    actual->next->type = ARG_EXPR;
    actual->next->u.expr = rhs;

    sym = NULL;

    for(; ns; ns=ns->parent) {
	sym = g95_search_interface(ns->operator[INTRINSIC_ASSIGN], 1, &actual);
	if (sym != NULL)
	    break;
    }

    if (sym == NULL) {
	g95_free(actual->next);
	g95_free(actual);
	return 1;
    }

    if (sym == g95_current_ns->proc_name) {
	g95_error("Recursive intrinsic assignment at %L in '%s'",
		  &c->where, sym->name);
	return 1;
    }

    if ((g95_pure(NULL, 0) || g95_forall_flag) && !g95_pure(sym, 0)) {
	g95_error("Subroutine '%s' called in lieu of assignment at %L must be "
		  "PURE", sym->name, &c->where);
	return 1;
    }

    if (g95_where_flag && !sym->attr.elemental) {
	g95_error("User-defined subroutine '%s' for assignment at %L must be "
		  "ELEMENTAL within a WHERE block", sym->name, &c->where);
	return 1;
    }

    if (sym->attr.elemental && lhs->rank != 0 && rhs->rank != 0 &&
	g95_check_conformance("Defined array assignment", lhs, rhs) == FAILURE)
	return 2;

    /* Replace the assignment with the call */

    if (rhs->ts.type != BT_DERIVED)
	e = rhs;

    else {
	e = g95_get_expr();
	e->where = rhs->where;
	e->ts = rhs->ts;
	e->rank = rhs->rank;
	e->type = EXPR_OP;
	e->value.op.operator = INTRINSIC_PAREN;
	e->value.op.op1 = rhs;
    }

    actual->next->u.expr = e;

    c->type = EXEC_CALL;
    c->sym = sym;
    c->expr = NULL;
    c->expr2 = NULL;

    c->ext.sub.sub_name = sym->name;
    c->ext.sub.actual = actual;

    return 0;
}



/* g95_extend_expr()-- This subroutine is called when an expression is
 * being resolved.  The expression node in question is either a user
 * defined operator or an intrinsic operator with arguments that
 * aren't compatible with the operator.  This subroutine builds an
 * actual argument list corresponding to the operands, then searches
 * for a compatible interface.  If one is found, the expression node
 * is replaced with the appropriate function call.  */

try g95_extend_expr(g95_expr *e) {
g95_actual_arglist *actual;
g95_namespace *ns;
g95_user_op *uop;
g95_symbol *sym;
g95_annot *a;
int i;

    sym = NULL;

    actual = g95_get_actual_arglist();
    actual->type = ARG_EXPR;
    actual->u.expr = e->value.op.op1;

    if (e->value.op.op2 != NULL) {
	actual->next = g95_get_actual_arglist();

	actual->next->type = ARG_EXPR;
	actual->next->u.expr = e->value.op.op2;
    }

    i = fold_unary(e->value.op.operator);

    if (i == INTRINSIC_USER) {
	for(ns=g95_current_ns; ns; ns=ns->parent) {
	    uop = g95_find_uop(e->value.op.uop->name, ns);
	    if (uop == NULL)
		continue;

	    sym = g95_search_interface(uop->operator, 0, &actual);
	    if (sym != NULL)
		break;
	}

    } else
	for(ns=g95_current_ns; ns; ns=ns->parent) {
	    sym = g95_search_interface(ns->operator[i], 0, &actual);
	    if (sym != NULL)
		break;
	}

    if (sym == NULL) {  /* Don't use g95_free_actual_arglist() */
	if (actual->next != NULL)
	    g95_free(actual->next);

	g95_free(actual);
	return FAILURE;
    }

/* Change the expression node to a function call */

    e->type = EXPR_FUNCTION;
    e->symbol = sym;

    e->value.function.actual  = actual;
    e->value.function.isym    = NULL;
    e->value.function.pointer = NULL;
    e->value.function.name    = sym->name;

    e->ts = sym->result->ts;
    e->rank = (sym->result->as == NULL) ? 0 : sym->result->as->rank;

    if (g95_pure(NULL, 0) && !g95_pure(sym, 0)) {
	g95_error("Function '%s' called in lieu of an operator at %L must "
		  "be PURE", sym->name, &e->where);
	return FAILURE;
    }

    a = g95_annotate(ANNOT_OPERATOR, &e->where);
    a->u.sym = sym;

    return g95_resolve_expr(e);
}



/* type_warning()-- Warn about inconsistent types. */

static void type_warning(g95_typespec *ts1, g95_typespec *ts2,
			 g95_locus *w1, g95_locus *w2) {

    g95_warning(155, "Inconsistent types (%s/%s) in actual argument lists at "
		"%L and %L", g95_typename(ts1), g95_typename(ts2), w1, w2);
}



static void pointer_warning(g95_locus *w1, g95_locus *w2) {

    g95_warning(156, "Inconsistent pointer usage between %L and %L", w1, w2);
}
		


/* compare_aa()-- Compare a pair of actual interfaces to see if the
 * usage is consistent.  This function is made easier because we only
 * have to check f77 style calls.  Returns nonzero if the number of
 * arguments is inconsistent. */

static int compare_aa(g95_actual_arglist *a1, g95_actual_arglist *a2) {

    for(;;) {
	if (a1 == NULL || a2 == NULL)
	    break;

	if (a1->type != ARG_ALT_RETURN && a2->type != ARG_ALT_RETURN &&
	    a1->u.expr != NULL && a2->u.expr != NULL &&
	    a1->u.expr->ts.type != BT_PROCEDURE &&
	    a2->u.expr->ts.type != BT_PROCEDURE) {

	    if (!g95_compare_types(&a1->u.expr->ts, &a2->u.expr->ts)) {
		type_warning(&a1->u.expr->ts, &a2->u.expr->ts,
			     &a1->u.expr->where, &a2->u.expr->where);
		return 0;
	    }

	    if (a1->pointer ^ a2->pointer) {
		pointer_warning(&a1->u.expr->where, &a2->u.expr->where);
		return 0;
	    }
	}

	a1 = a1->next;
	a2 = a2->next;
    }

    return (a1 != NULL) || (a2 != NULL);
}



/* compare_fa()-- Compare formal and actual argument lists in
 * differing program units.  This function only does f77 style
 * checking that is limited to type.  Returns nonzero if the number of
 * arguments is inconsistent. */

static int compare_fa(g95_formal_arglist *f, g95_actual_arglist *a) {

    for(;;) {
	if (a == NULL || f == NULL)
	    break;

	if (a->type != ARG_ALT_RETURN && a->u.expr != NULL &&
	    a->u.expr->ts.type != BT_PROCEDURE && f->sym != NULL) {

	    if (!g95_compare_types(&a->u.expr->ts, &f->sym->ts)) {
		type_warning(&a->u.expr->ts, &f->sym->ts,
			     &a->u.expr->where, &f->where);
		return 0;
	    }

	    if (a->pointer ^ f->sym->attr.pointer) {
		pointer_warning(&a->u.expr->where, &f->where);
		return 0;
	    }

	    if (a->type == ARG_ARRAY && f->sym->as != NULL &&
		f->sym->as->type == AS_ASSUMED_SHAPE) {
		g95_error("Passing array at %L to assumed-shape actual "
			  "argument at %L via an implicit interface",
			  &a->u.expr->where, &f->where);
		return 0;
	    }
	}

	a = a->next;
	f = f->next;
    }

    return (a != NULL) || (f != NULL);
}



/* narg_warning()-- Warn about inconsistent number of arguments. */

static void narg_warning(char *name, g95_locus *w1, g95_locus *w2) {

    g95_warning(154, "Inconsistent number of arguments in reference to '%s' "
		"at %L and %L", name, w1, w2);
}



/* check_global_ref()-- Check references to a global symbol */

static void check_global_ref(g95_gsymbol *g) {
g95_global_ref *ref;
int rank1, rank2;
g95_typespec *ts;
g95_locus *where;

    if (g->type != GSYM_FUNCTION && g->type != GSYM_SUBROUTINE)
	return;

    if (!g95_option.globals)
	return;

    if (g->type == GSYM_FUNCTION) {
	ref = NULL;
	ts = NULL;
	rank1 = 0;

	if (g->global != NULL) {
	    ref = g->ref;
	    rank1 = (g->global->as == NULL) ? 0 : g->global->as->rank;
	    ts = &g->global->ts;
	    where = &g->where;

	} else if (g->ref != NULL && g->ref->sym != NULL) {
	    ref = g->ref;

	    rank1 = (ref->sym->as == NULL) ? 0 : ref->sym->as->rank;
	    ts = &ref->sym->ts;
	    where = &ref->where;
	    ref = ref->next;
	}

	if (g->global != NULL)
	    for(; ref; ref=ref->next) {
		rank2 = (ref->sym == NULL || ref->sym->as == NULL)
		    ? 0
		    : ref->sym->as->rank;

		if (rank1 != rank2) {
		    g95_error("FUNCTION '%s' is of rank %d at %L and rank "
			      "%d at %L", g->name, rank1, &g->where,
			      rank2, &ref->where);
		    return;
		}

		if (ref->sym != NULL &&
		    !g95_compare_types(ts, &ref->sym->ts)) {
		    g95_error("FUNCTION '%s' is of type %s at %L and type "
			      "%s at %L", g->name, g95_typename(ts), &g->where,
			      g95_typename(&ref->sym->ts), &ref->where);
		    return;
		}
	    }
    }

    if (g->global == NULL) {  /* Compare formal arglists */
	if (g->ref == NULL)
	    return;

	for(ref=g->ref->next; ref; ref=ref->next)
	    if (compare_aa(g->ref->actual, ref->actual)) {
		narg_warning(g->name, &g->ref->where, &ref->where);
		break;
	    }

    } else {  /* Compare formal and actual arglists */
	for(ref=g->ref; ref; ref=ref->next)
	    if (compare_fa(g->global->formal, ref->actual)) {
		narg_warning(g->name, &g->global->declared_at, &ref->where);
		break;
	    }
    }
}



/* traverse_global()-- Traverse all global symbols. */

static void traverse_global(g95_gsymbol *g) {

    if (g == NULL)
	return;

    check_global_ref(g);

    traverse_global(g->left);
    traverse_global(g->right);
}



/* g95_check_global_refs()-- Traverse all globals symbols to check use
 * vs definition. */

void g95_check_global_refs(void) {

    traverse_global(g95_gsym_root);
}



/* g95_add_global_ref()-- Given a global symbol, a location and an
 * actual argument list, create a new global_ref. */

void g95_add_global_ref(g95_gsymbol *g, g95_symbol *sym,
			g95_actual_arglist *actual, g95_locus *where) {
g95_global_ref *ref;

    ref = g95_get_global_ref();

    ref->sym = sym;
    ref->actual = actual;
    ref->where = *where;

    ref->next = g->ref;
    g->ref = ref;
}



/* g95_delete_global_ref()-- Recursive work function for deleting a
 * global reference that is about to go away.  Returns nonzero if
 * we're cutting the search short because we found the reference. */

int g95_delete_global_ref(g95_gsymbol *sym, g95_actual_arglist *actual) {
g95_global_ref *ref, **tail;

    if (sym == NULL)
	return 0;

    tail = &sym->ref;

    for(;;) {
	ref = *tail;
	if (ref == NULL)
	    break;

	if (ref->actual == actual) {
	    *tail = ref->next;
	    g95_free(ref);
	    return 1;
	}

	tail = &ref->next;
    }

    return g95_delete_global_ref(sym->left, actual) ||
	   g95_delete_global_ref(sym->right, actual);
}



/* g95_copy_global_ref()-- Create a g95_global_ref structure for an
 * actual argument list that is being copied from another.  Returns
 * nonzero if we cut short the search because we found the
 * reference. */

int g95_copy_global_ref(g95_gsymbol *sym, g95_actual_arglist *old,
			g95_actual_arglist *new) {
g95_global_ref *ref;

    if (sym == NULL) 
	return 0;

    for(ref=sym->ref; ref; ref=ref->next)
	if (ref->actual == old) {
	    g95_add_global_ref(sym, sym->global, new, &ref->where);
	    return 1;
	}

    return g95_copy_global_ref(sym->left, old, new) ||
	   g95_copy_global_ref(sym->right, old, new);
}



/* g95_define_global()-- Define a global procedure, checking that a
 * new definition does not conflict with an older one. */

void g95_define_global(g95_gsymbol *g, g95_symbol *new, g95_locus *where) {
g95_formal_arglist *f1, *f2;
g95_symbol *s1, *s2;
g95_locus *w;
int p1, p2;

    if (g == NULL)
	return;

    if (g->global == NULL) {
	g->global = new;
	g->where = *where;
	return;
    }

    if (g->type != GSYM_SUBROUTINE && g->type != GSYM_FUNCTION)
	return;

    if (!g95_option.globals)
	return;

    if (g->type == GSYM_FUNCTION) {
	if (g->global->attr.pure != new->attr.pure) {
	    g95_error("Differing PURE attribute in FUNCTION '%s' at %L and %L",
		      new->name, &g->global->declared_at, &new->declared_at);
	    return;
	}

	if (g->global->attr.elemental != new->attr.elemental) {
	    g95_error("Differing ELEMENTAL attribute in FUNCTION '%s' at "
		      "%L and %L", &g->global->declared_at, &new->declared_at);
	    return;
	}
    }

    /* Compare formal interfaces */

    f1 = g->global->formal;
    f2 = new->formal;

    for(;;) {
	if (f1 == NULL && f2 == NULL)
	    break;

	w = NULL;

	if (f1 == NULL) 
	    w = &f2->where;

	if (f2 == NULL)
	    w = &f1->where;

	if (w != NULL) {
	    g95_error("Extra formal argument for '%s' at %L", new->name, w);
	    break;
	}

	s1 = f1->sym;
	s2 = f2->sym;

	if (s1 == NULL && s2 == NULL)
	    goto cont;

	if (s1 == NULL || s2 == NULL) {
	    g95_error("Inconsistent alternate return in format arguments at "
		      "%L and %L", &f1->where, &f2->where);
	    break;
	}

	p1 = (s1->attr.external || s1->attr.function || s1->attr.subroutine);
	p2 = (s2->attr.external || s2->attr.function || s2->attr.subroutine);

	if (p1 && p2)
	    goto cont;

	if (p1 || p2) {
	    g95_error("Different procedure/variable flavors in formal "
		      "arguments at %L and %L", &f1->where, &f2->where);
	    break;
	}

	if (!g95_compare_types(&s1->ts, &s2->ts)) {
	    g95_error("Differing types %s and %s in formal arguments at "
		      "%L and %L", g95_typename(&s1->ts),
		      g95_typename(&s2->ts), &f1->where, &f2->where);
	    break;
	}

	if (symbol_rank(s1) != symbol_rank(s2)) {
	    g95_error("Differing ranks in formal arguments at %L and %L",
		      &f1->where, &f2->where);
	    break;
	}

	if (s1->attr.pointer != s2->attr.pointer) {
	    g95_error("Differing POINTER attributes in formal arguments at "
		      "%L and %L", &f1->where, &f2->where);
	    break;
	}

	if (s1->attr.optional != s2->attr.optional) {
	    g95_error("Differing OPTIONAL attributes in formal arguments at "
		      "%L and %L", &f1->where, &f2->where);
	    break;
	}

	if (s1->attr.value != s2->attr.value) {
	    g95_error("Differing VALUE attributes in formal arguments at "
		      "%L and %L", &f1->where, &f2->where);
	    break;
	}

	if (s1->attr.intent != s2->attr.intent) {
	    g95_error("Differing INTENT attributes in formal arguments at "
		      "%L and %L", &f1->where, &f2->where);
	    break;
	}

	if (s1->attr.allocatable != s2->attr.allocatable) {
	    g95_error("Differing ALLOCATABLE attributes in formal arguments "
		      "at %L and %L", &f1->where, &f2->where);
	    break;
	}

	if (s1->attr.target != s2->attr.target) {
	    g95_error("Differing TARGET attributes in formal arguments "
		      "at %L and %L", &f1->where, &f2->where);
	    break;
	}

    cont:
	f1 = f1->next;
	f2 = f2->next;
    }
}



/* check_new_interface()-- Make sure that the interface just parsed is
 * not already present in the given interface list.  Ambiguity isn't
 * checked yet since module procedures can be present without
 * interfaces.  */

static try check_new_interface(g95_interface *base, g95_symbol *new) {
g95_interface *ip;

    for(ip=base; ip; ip=ip->next)
	if (ip->sym == new) {
	    g95_error("Entity '%s' at %C is already present in the interface",
		      new->name);
	    return FAILURE;
	}

    return SUCCESS;
}



/* g95_add_interface()-- Add a symbol to the current interface */

try g95_add_interface(g95_symbol *new, g95_locus *where) {
g95_interface **head, *intr;
g95_namespace *ns;

    switch(current_interface.type) {
    case INTERFACE_NAMELESS:
    case INTERFACE_ABSTRACT:
	return SUCCESS;

    case INTERFACE_INTRINSIC_OP:
	for(ns=current_interface.ns; ns; ns=ns->parent)
	    if (check_new_interface(ns->operator[current_interface.op],
				    new) == FAILURE)
		return FAILURE;

	head = &current_interface.ns->operator[current_interface.op];
	break;

    case INTERFACE_GENERIC:
	intr = current_interface.generic->n.generic;
	if (check_new_interface(intr, new) == FAILURE)
	    return FAILURE;

	head = &current_interface.generic->n.generic;
	break;

    case INTERFACE_USER_OP:
	if (check_new_interface(current_interface.uop->operator, new)==FAILURE)
	    return FAILURE;

	head = &current_interface.uop->operator;
	break;

    default:
	g95_internal_error("g95_add_interface(): Bad interface type");
    }

    intr = g95_get_interface();
    intr->sym = new;
    intr->where = *where;

    intr->next = *head;
    *head = intr;

    return SUCCESS;
}

