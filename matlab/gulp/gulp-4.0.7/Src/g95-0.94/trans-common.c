
/* Common and equivalence block handling
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


/* Transform common blocks.  An integral part of this is processing
 * EQUIVALENCE variables.  Equivalenced variables that are not in a
 * common block end up in a private block of their own.
 *
 * Each common block is a global character array of some length.  The
 * blank common has its own name that is an otherwise inaccessible
 * name.  Variables within the block are represented as an array
 * element within the block.  While it should be possible to declare
 * the areas as a union, it seems much easier to represent things this
 * way, particularly for an initialized common (a block of bytes).
 *
 * So if two variables are equivalenced, they just point to a common
 * area in memory.
 *
 * Mathematically, laying out an equivalence block is equivalent to
 * solving a linear system of equations.  The matrix is a sparse
 * matrix in which each row contains all zero elements except for a +1
 * and a -1, a sort of a generalized Vandermonde matrix.  The matrix
 * is usually block diagonal.  The system can be overdetermined,
 * underdetermined or have a unique solution.  If the system is
 * inconsistent, the program is not standard conforming.  The solution
 * vector is integral, since all of the pivots are +1 or -1.
 *
 * How we lay out an equivalence block is a little less complicated.
 * In an equivalence list with n elements, there are n-1 conditions to
 * be satisfied.  The conditions partition the variables into what we
 * will call segments.  If A and B are equivalenced then A and B are
 * in the same segment.  If B and C are equivalenced as well, then A,
 * B and C are in a segment and so on.  Each segment is a block of
 * memory that has one or more variables equivalenced in some way.  A
 * common block is made up of a series of segments that are joined one
 * after the other.  In the linear system, a segment is a block
 * diagonal.
 *
 * To lay out a segment we first start with some variable and
 * determine its length.  The first variable is assumed to start at
 * offset one and extends to however long it is.  We then traverse the
 * list of equivalences to find an unused condition that involves at
 * least one of the variables currently in the segment.
 *
 * Each equivalence condition amounts to the condition B+b=C+c where B
 * and C are the offsets of the B and C variables, and b and c are
 * constants which are nonzero for array elements, substrings or
 * structure components.  So for
 *
 *   EQUIVALENCE(B(2), C(3))
 * we have
 *   B + 2*size of B's elements = C + 3*size of C's elements.
 *
 * If B and C are known we check to see if the condition already
 * holds.  If B is known we can solve for C.  Since we know the length
 * of C, we can see if the minimum and maximum extents of the segment
 * are affected.  Eventually, we make a full pass through the
 * equivalence list without finding any new conditions and the segment
 * is fully specified.
 *
 * At this point, the segment is added to the current common block.
 * The seed symbol in the common block determines the extent of the
 * segment, the rest of the variables are along for the ride.  The
 * usual case here is that there are no equivalence statements and the
 * common block is series of segments with one variable each, which is
 * a diagonal matrix in the matrix formulation.
 *
 * Once all common blocks have been created, the list of equivalences
 * is examined for still-unused equivalence conditions.  If these
 * exist, a variable from an unused equivalence is placed into a
 * private block in order to start a new segment, which is built as
 * usual.  This process continues until all equivalenced variables
 * have been put into a block.
 *
 * The overall process for creating common blocks is to examine all
 * common blocks within the source file, and create those common
 * blocks that are initialized.  The remaining common blocks are
 * created as uninitialized memory.  When processing program units, we
 * again loop over common blocks in the program unit.  The variables
 * within the common are then declared as offsets within the block.
 */

#include "trans.h"


typedef struct segment_info {
    g95_symbol *sym;
    int offset, length;
    struct segment_info *next;
    g95_equiv *rule;
} segment_info;

static segment_info *current_segment, *current_common;
static int common_length, real_common, seen_init;

static tree blank_common_decl;
static g95_locus blank_common_locus;
static int blank_common_length, blank_common_seen=0;

#define get_segment_info() g95_getmem(sizeof(segment_info))

typedef enum {
    CT_NONE, CT_COMMON, CT_PUBLIC, CT_PUBLIC_EXTERNAL
} ctype;



/* calculate_length()-- Given a variable symbol, calculate the total
 * length in bytes of the variable. */

static int calculate_length(g95_symbol *sym) {
int element_size;
bignum b;

    if (sym->as != NULL && sym->attr.pointer)
	return int_size_in_bytes(g95_get_array_desc(sym->as->rank, 0));

    if (sym->attr.pointer)
	return int_size_in_bytes(pchar_type_node);

    element_size = g95_int_ts_size(&sym->ts);
    if (sym->as == NULL)
	return element_size;

    /* Calculate the number of elements in the array */

    b = g95_array_spec_size(sym->as);
    if (b == NULL)
	g95_internal_error("calculate_length(): "
			   "Unable to determine array size");

    return element_size * bi_to_int(b);
}



/* get_value()-- Given an expression node, make sure it is a constant
 * integer and return the value. */

static bignum get_value(g95_expr *e) {

    if (e->type != EXPR_CONSTANT)
	g95_internal_error("get_value(): Not an integer constant");

    return e->value.integer;
}



/* g95_element_number()-- Given an array specification and an array
 * reference, figure out the array element number (zero based).
 * Bounds and elements are guaranteed to be constants. */

int g95_element_number(g95_array_ref *ar, g95_array_spec *as) {
bignum multiplier, offset, extent, m, index, lower, upper;
int i, rank;

    rank = as->rank;

    multiplier = bi_1;
    offset = bi_0;

    for(i=0; i<rank; i++) {
	if (ar->dimen_type[i] != DIMEN_ELEMENT)
	    g95_internal_error("g95_element_number(): Bad dimension type");

	index = get_value(ar->start[i]);

	lower = get_value(as->lower[i]);
	upper = get_value(as->upper[i]);

	if (bi_compare(big_copy(index), big_copy(lower)) < 0 ||
	    bi_compare(big_copy(index), big_copy(upper)) > 0)
	    g95_error("Out of bounds array reference at %L", &ar->where[i]);

	m = bi_multiply(bi_subtract(index, big_copy(lower)),
			big_copy(multiplier));
	offset = bi_add(offset, m);

	extent = bi_int_add(bi_subtract(upper, lower), 1);

	if (bi_is_negative(big_copy(extent))) {
	    big_free(extent);
	    extent = bi_0;
	}

	multiplier = bi_multiply(multiplier, extent);
    }

    big_free(multiplier);

    return bi_to_int(offset);
}



/* calculate_offset()-- Given a single element of an equivalence list,
 * figure out the offset from the base symbol.  For simple variables
 * or full arrays, this is simply zero.  For an array element we have
 * to calculate the array element number and multiply by the element
 * size.  For a substring we have to calculate the further reference. */

static int calculate_offset(g95_expr *e) {
int offset; 
g95_typespec *element_type;
g95_array_spec *as;
g95_ref *ref;

    offset = 0;
    element_type = &e->symbol->ts;

    as = e->symbol->as;

    for(ref=e->ref; ref; ref=ref->next)
	switch(ref->type) {
	case REF_ARRAY:
	    switch(ref->u.ar.type) {
	    case AR_FULL:
		break;

	    case AR_ELEMENT:
		offset += g95_element_number(&ref->u.ar, as)
		        * g95_int_ts_size(element_type);
		break;

	    default:
		g95_internal_error("calculate_offset(): Bad array reference");
	    }

	    break;

	case REF_SUBSTRING:
	    if (ref->u.ss.start != NULL)
		offset += bi_to_int(get_value(ref->u.ss.start)) - 1;

	    break;

	default:
	    g95_internal_error("calculate_offset(): Bad ref");
	    break;
	}

    return offset;
}



/* check_init()-- If the given symbol is initialized, record the fact. */

static void check_init(g95_symbol *sym) {

    if (sym->value != NULL || sym->attr.data ||
	(sym->ts.type == BT_DERIVED &&
	 g95_initialized_component(sym->ts.derived)))
	seen_init = 1;
}



/* new_condition()-- Add a new segment_info structure to the current
 * eq1 is already in the list at s1, eq2 is not. */

static void new_condition(segment_info *s1, g95_equiv *eq1, g95_equiv *eq2) {
int offset1, offset2;
segment_info *s2;

    offset1 = calculate_offset(eq1->expr);
    offset2 = calculate_offset(eq2->expr);

    s2 = get_segment_info();

    s2->sym = eq2->expr->symbol;
    s2->offset = s1->offset + offset1 - offset2;
    s2->length = calculate_length(eq2->expr->symbol);
    s2->rule = eq1;

    s2->next = current_segment;
    current_segment = s2;

    if (real_common && s2->offset < 0)
	g95_error("EQUIVALENCE involving '%s' at %L caused the storage block "
		  "to be extended before the first variable", s1->sym->name,
		  &eq1->expr->where);
}



/* equivalence_conflict()-- Complain about a symbol at two offsets */

static void equivalence_conflict(char *name, g95_equiv *rule, int a, int b) {

    g95_error("EQUIVALENCE conflict, '%s' at %L has conflicting "
	      "offsets %d and %d", name, &rule->expr->where, a, b);
}



/* confirm_condition()-- Given two equivalence structures that are
 * both already in the list, make sure that this new condition is not
 * violated, generating an error if it is. */

static void confirm_condition(segment_info *s1, g95_equiv *eq1,
			      segment_info *s2, g95_equiv *eq2) {
int offset1, offset2;

    offset1 = calculate_offset(eq1->expr);
    offset2 = calculate_offset(eq2->expr);

    if (s1->offset + offset1 != s2->offset + offset2)
	equivalence_conflict(s1->sym->name, eq1, s1->offset + offset1,
			     s2->offset + offset2);
}



/* find_segment_info()-- Given a symbol, find it in the current list
 * segment list.  Returns NULL if not found. */

static segment_info *find_segment_info(g95_symbol *sym) {
segment_info *s;

    for(s=current_segment; s; s=s->next)
	if (s->sym == sym)
	    return s;

    return NULL;
}



/* add_condition()-- At this point we have a new equivalence condition
 * to process.  If both variables are already present, then we are
 * confirming that the condition holds.  Otherwise we are adding a new
 * variable to the segment list. */

static void add_condition(g95_equiv *eq1, g95_equiv *eq2) {
segment_info *s1, *s2;

    eq2->used = 1;

    s1 = find_segment_info(eq1->expr->symbol);
    s2 = find_segment_info(eq2->expr->symbol);

    if (s1 == NULL && s2 == NULL) abort();    /* Can't happen */
    if (s1 != NULL && s2 == NULL) new_condition(s1, eq1, eq2);
    if (s1 == NULL && s2 != NULL) new_condition(s2, eq2, eq1);
    if (s1 != NULL && s2 != NULL) confirm_condition(s1, eq1, s2, eq2);
}



/* find_equivalence()-- Given a symbol, search through the equivalence
 * lists for an unused condition that involves the symbol.  If a rule
 * is found, we return nonzero, the rule is marked as used and the eq1
 * and eq2 pointers point to the rule. */

static int find_equivalence(g95_symbol *sym, g95_equiv **eq1, g95_equiv **eq2){
g95_equiv *e1, *e2;

    for(e1=sym->ns->equiv; e1; e1=e1->next)
	for(e2=e1->eq; e2; e2=e2->eq) {
	    if (e2->used)
		continue;

	    if (e1->expr->symbol == sym || e2->expr->symbol == sym) {
		*eq1 = e1;
		*eq2 = e2;

		check_init(e1->expr->symbol);
		check_init(e2->expr->symbol);
		return 1;
	    }
	}

    return 0;
}



/* add_equivalences()-- Function for adding symbols to current
 * segment.  Returns zero if the segment was modified.  Equivalence
 * rules are considered to be between the first expression in the list
 * and each of the other expressions in the list.  Symbols are scanned
 * multiple times because a symbol can be equivalenced more than once. */

static int add_equivalences(void) {
int segment_modified;
g95_equiv *eq1, *eq2;
segment_info *s;

    segment_modified = 0;

    for(s=current_segment; s; s=s->next)
	if (find_equivalence(s->sym, &eq1, &eq2))
	    break;

    if (s != NULL) {
	add_condition(eq1, eq2);
	segment_modified = 1;
    }

    return segment_modified;
}



/* new_segment()-- Given a seed symbol, create a new segment
 * consisting of that symbol and all of the symbols equivalenced with
 * that symbol.  In a real common, the seed symbols are placed next to
 * one another, causing equivalenced symbols to possible overlap in
 * various ways.  In an equivalence common, the segments do not
 * overlap. */

static void new_segment(g95_symbol *sym) {
int e, seg_start, seg_end;
segment_info *s;

    check_init(sym); 

    for(s=current_common; s; s=s->next)
	if (s->sym == sym)
	    break;

/* If the current symbol is already in the common, make sure the
 * offset is correct.  Any equivalences to this symbol have already
 * been processed in that case. */

    if (s != NULL) {
	if (s->offset != common_length)
	    equivalence_conflict(s->sym->name, s->rule, s->offset,
				 common_length);

	common_length += s->length;

    } else {   /* New symbol */
	current_segment = s = get_segment_info();
	current_segment->sym = sym;
	current_segment->length = calculate_length(sym);

	/* Align 64+ bit common variables to 8-byte boundaries */

	if (G95_ALIGN_DOUBLE && sym->ts.type != BT_CHARACTER &&
	    sym->ts.type != BT_DERIVED && sym->ts.kind >= 8)
	    common_length = (common_length + 7) & (~0x07);

	current_segment->offset = common_length;

	while(add_equivalences());

	if (real_common)
	    common_length += s->length;

	else {
	    seg_start = current_segment->offset;
	    seg_end   = current_segment->offset + current_segment->length;

	    for(s=current_segment->next; s; s=s->next) {
		if (s->offset < seg_start) seg_start = s->offset;

		e = s->offset + s->length;
		if (e > seg_end) seg_end = e;
	    }

	    /* Translate the segment to the right place. */

	    e = common_length - seg_start;
	    for(s=current_segment; s; s=s->next)
		s->offset += e;

	    common_length += seg_end - seg_start;
	}

	/* Append the current segment to the current common */

	s = current_segment;
	while(s->next != NULL)
	    s = s->next;

	s->next = current_common;
	current_common = current_segment;
	current_segment = NULL;
    }
}



/* seg_compare()-- Compare the offsets for two segments. */

static int seg_compare(const void *a, const void *b) {
segment_info *p, *q;

    p = *((segment_info **) a);
    q = *((segment_info **) b);

    return p->offset - q->offset;
}



/* sort_common()-- Sort the current_common list so that all of the
 * nodes are in order of ascending offsets. */

static void sort_common(void) {
segment_info *s, **a;
int i, n;

    if (current_common == NULL)
	return;

    n = 0;
    s = current_common;
    while(s != NULL) {
	s = s->next;
	n++;
    }

    a = g95_getmem(n*sizeof(segment_info *));

    s = current_common;
    for(i=0; i<n; i++) {
	a[i] = s;
	s = s->next;
    }

    qsort(a, n, sizeof(segment_info *), seg_compare);

    current_common = a[0];

    for(i=0; i<n-1; i++)
	a[i]->next = a[i+1];

    a[n-1]->next = NULL;

    g95_free(a);
}



#if 0
/* show_common()-- Show the current common block layout */

static void show_common(void) {
segment_info *s;

    for(s=current_common; s; s=s->next)
	g95_status("%d  %d  %s\n", s->offset, s->length, s->sym->name);

    g95_status("total length = %d\n", common_length);
}

#endif



/* unmark_equivalences()-- Given a namespace, mark all of the
 * equivalences as unused. */

static void unmark_equivalences(g95_namespace *ns) {
g95_equiv *e, *f;

    for(e=ns->equiv; e; e=e->next)
	for(f=e; f; f=f->eq)
	    f->used = 0;
}



/* free_current_common()-- Free the list of segments. */

static void free_current_common(void) {
segment_info *next;

    while(current_common != NULL) {
	next = current_common->next;
	g95_free(current_common);
	current_common = next;
    }
}



/* traverse_common()-- Traverse a single common block, figuring out
 * where each element is located. */

static void traverse_common(g95_common_head *common) {
g95_symbol *sym;
segment_info *s;
int n;

    current_common = NULL;
    common_length = 0;
    real_common = 1;
    seen_init = 0;

    for(sym=common->head; sym; sym=sym->common_next)
	new_segment(sym);

    if (!real_common) {   /* shift things to a zero offset */
	n = 0;
	for(s=current_common; s; s=s->next)
	    if (s->offset < n) n = s->offset;

	n = -n;
	for(s=current_common; s; s=s->next)
	    s->offset += n;
    }

    /* Figure out the real length of the common block.  It may have been
     * extended by an equivalence. */

    for(s=current_common; s; s=s->next) {
	n = s->offset + s->length;
	if (n > common_length)
	    common_length = n;
    }
}



/* build_common_vars()-- Given a common block, create the declarations
 * for those variables, which consists of an offset into the common block. */

static void build_common_vars(tree block_var) {
tree block, offset, base_type, type, decl, storage;
variable_info vinfo;
segment_info *s;
rtx tmp;

    block = convert(pchar_type_node, g95_addr_expr(block_var));

    for(s=current_common; s; s=s->next) {
	offset = g95_build_int(s->offset, 0);

	g95_symbol_vinfo(s->sym, &vinfo);
	base_type = g95_get_descriptor(&vinfo);

	if (vinfo.as != NULL && vinfo.pointer) {
	    decl = g95_array_node();

	    type = build_pointer_type(base_type);
	    TYPE_REF_CAN_ALIAS_ALL(type) = 1;
	    G95_ARRAY_DESC(decl) = g95_build_plus(type, block, offset);

	} else if (vinfo.as == NULL || vinfo.pointer) {
	    type = build_pointer_type(base_type);
	    TYPE_REF_CAN_ALIAS_ALL(type) = 1;

	    decl = g95_build_plus(type, block, offset);

	} else {
	    decl = g95_array_node();
	    g95_get_array_storage(&vinfo, decl);

	    storage = g95_build_plus(pchar_type_node_a, block, offset);
	    G95_ARRAY_STORAGE(decl) = storage;
	}

	if (s->sym->backend_decl != NULL_TREE && !s->sym->attr.use_assoc)
	    g95_error("Symbol '%s' at %L is equivalenced across multiple "
		      "common blocks", s->sym->name, &s->sym->declared_at);

	s->sym->backend_decl = decl;

	/* Build a fake variable that shows up in debugging info. */

	decl = build_decl(VAR_DECL, get_identifier(s->sym->name), base_type);
	TREE_STATIC(decl) = 1;
	DECL_INITIAL(decl) = NULL_TREE;
	TREE_ASM_WRITTEN(decl) = 1;

	tmp = plus_constant(XEXP(DECL_RTL(block_var), 0), s->offset);
	SET_DECL_RTL(decl, gen_rtx_MEM(TYPE_MODE(base_type), tmp));
	pushdecl(decl);
    }
}



/* set_common_decl()-- Set the proper type for the declaration.  There are
 * several possibilities. */

static void set_common_decl(tree decl, ctype c) {

    switch(c) {
    case CT_NONE:
	TREE_STATIC(decl) = 1;
	break;

    case CT_COMMON:
	DECL_COMMON(decl) = 1;
	TREE_PUBLIC(decl) = 1;
	TREE_STATIC(decl) = 1;
	break;

    case CT_PUBLIC_EXTERNAL:
	DECL_EXTERNAL(decl) = 1;   /* Fall through */

    case CT_PUBLIC:
	TREE_PUBLIC(decl) = 1;
	TREE_STATIC(decl) = 1;
	break;

    default:
	g95_internal_error("set_common_decl(): Bad type");
    }
}



/* build_common_decl()-- Declare memory for an initialized common
 * block and create declarations for all of the elements. */

static tree build_common_decl(tree identifier, ctype c) {
tree tmp, decl, initial_value;
segment_info *s;
int m, scalar;

    sort_common();
    g95_start_common();

    for(s=current_common; s; s=s->next)
	g95_init_common_var(s->sym, s->offset);

    for(s=current_common; s; s=s->next)
	g95_default_structure_init(s->sym, s->offset);

    scalar = current_common == NULL ||
	(current_common->next == NULL &&
	 current_common->sym->as == NULL);

    initial_value = g95_data_initializer(scalar, common_length);
    m = (initial_value == NULL_TREE);

    if (m)
	initial_value = integer_zero_node;

    tmp = size_in_bytes(TREE_TYPE(initial_value));
    tmp = g95_build_minus(TREE_TYPE(tmp), tmp, integer_one_node);
    tmp = build_index_type(tmp);
    tmp = build_array_type(g95_character1_type_node, tmp);

    decl = build_decl(VAR_DECL, identifier, tmp);
    TREE_STATIC(decl) = 1;
    DECL_ALIGN(decl) = 64;

    set_common_decl(decl, c);

    DECL_INITIAL(decl) = initial_value;

    pushdecl(decl);
    rest_of_decl_compilation(decl, 1, 0);

    return decl;
}



/* blank_block()-- Declare a blank character array that will hold a
 * common that is uninitialized at least in the current source file. */

static tree blank_block(tree identifier, int length, ctype c) {
tree decl, tmp;

    if (length == 0)
	length = 1;  /* Some assemblers don't like zero length objects */

    tmp = g95_build_int(length-1, 0);
    tmp = build_index_type(tmp);
    tmp = build_array_type(g95_character1_type_node, tmp);

    decl = build_decl(VAR_DECL, identifier, tmp);
    DECL_ALIGN(decl) = 64;

    set_common_decl(decl, c);

    pushdecl(decl);
    rest_of_decl_compilation(decl, 1, 0);

    return decl;
}



/* build_module_equivalences()-- Build equivalence blocks for a
 * module.  The rules for the module are created first to build the
 * block as in the real module. */

static void build_module_equivalences(char *name) {
char ident[G95_MAX_SYMBOL_LEN+20];
tree identifier, decl;
g95_equiv *e1, *e2;
ctype c;

    current_common = NULL;
    common_length = 0;
    real_common = 0;
    seen_init = 0;

    for(e1=g95_current_ns->equiv; e1; e1=e1->next) {
	if (e1->module != name)
	    continue;

	e1->used = 1;

	for(e2=e1->eq; e2; e2=e2->eq) {
	    if (e2->used)
		continue;

	    new_segment(e1->expr->symbol);
	    break;
	}
    }

    if (current_common == NULL)
	return;

    if (name == NULL) {
	identifier = g95_unique_identifier("equiv.common");
	c = CT_NONE;

    } else {
	strcpy(ident, name);
	strcat(ident, "_EQUIV");
	identifier = get_identifier(ident);

	c = current_common->sym->attr.use_assoc
	    ? CT_PUBLIC_EXTERNAL
	    : CT_PUBLIC;
    }

    decl = (seen_init)
	? build_common_decl(identifier, c)
	: blank_block(identifier, common_length, c);

    build_common_vars(decl);
    free_current_common();
}



/* finish_equivalences()-- Create a new block that contains all
 * remaining equivalences.  Loop over modules, then create a private
 * block for anything that is left. */

static void finish_equivalences(g95_namespace *ns) {
g95_equiv *eq;

    for(eq=g95_current_ns->equiv; eq; eq=eq->next) {
	if (eq->used || eq->module == NULL)
	    continue;

	build_module_equivalences(eq->module);
    }

    build_module_equivalences(NULL);
}



/* trans_common()-- Work function for translating a named common block.  */

static void trans_common(g95_symtree *st) {
g95_common_head *c;
g95_gsymbol *g;

    if (st == NULL)
	return;

    trans_common(st->left);
    trans_common(st->right);

    g = g95_find_gsymbol(st->name);
    for(c=st->n.common; c; c=c->next) {
	traverse_common(c);

	build_common_vars(g->backend_decl);
	free_current_common();
    }
}



/* g95_trans_common()-- Create common variables within a namespace.
 * Unlike other variables, these have to be created before code,
 * because the backend_decl depends on the rest of the common
 * block. */

void g95_trans_common(g95_namespace *ns) {
g95_common_head *h;

    unmark_equivalences(ns);

    for(h=ns->blank_common; h; h=h->next) {
	traverse_common(h);
	build_common_vars(blank_common_decl);
	free_current_common();
    }

    trans_common(ns->common_root);

    finish_equivalences(ns);
}



/* common_identifier()-- Given a common name, return an identifier for it. */

static tree common_identifier(char *name) {
g95_symbol sym;

    if (name == NULL)
	return get_identifier(BLANK_COMMON_NAME);

    memset(&sym, '\0', sizeof(sym));
    sym.name = g95_get_string(name);

    sym.attr.flavor = FL_BLOCK_DATA;  /* A global name */

    return g95_sym_identifier(&sym, 1, NULL);
}



/* build_uninitialized_common()-- Traverse the global symbol tree
 * looking for uninitialized common blocks.  Create these as character
 * arrays of the correct size. */

static void build_uninitialized_common(g95_gsymbol *g) {
tree identifier;

    if (g == NULL)
	return;

    build_uninitialized_common(g->left);
    build_uninitialized_common(g->right);

    if (g->size == -1)
	return;

    if (g->type == GSYM_COMMON && g->backend_decl == NULL_TREE) {
	identifier = (g->bind != NULL)
	    ? get_identifier(g->bind)
	    : common_identifier(g->name);

	g->backend_decl = blank_block(identifier, g->size, CT_COMMON);
    }
}



/* init_blank_common()-- See about initializing the blank common if it
 * is initialized. */

static void init_blank_common(g95_common_head *common) {

    if (common->head == NULL)
	return;

    blank_common_seen = 1;
    traverse_common(common);

    if (blank_common_length < common_length)
	blank_common_length = common_length;

    if (seen_init) {
	if (blank_common_decl != NULL_TREE)
	    g95_error("Blank common at %L is initialized in another "
		      "program unit", &blank_common_locus);
	else {
	    blank_common_decl = build_common_decl(common_identifier(NULL),
						  CT_COMMON);
	    blank_common_locus = common->where;
	}
    }

    free_current_common();
}



/* init_common()-- Figure out how big a particular common is, make
 * sure the size is consistent and initialize a block if necessary. */

static void init_common(char *name, g95_common_head *common) {
g95_gsymbol *g;
tree ident;

    traverse_common(common);
    g = g95_get_gsymbol(name);

    switch(g->type) {
    case GSYM_UNKNOWN:
	g->type = GSYM_COMMON;
	g->size = common_length;
	g->where = common->where;
	break;

    case GSYM_COMMON:
	if (g->size == -1) {
	    g->size = common_length;
	    break;
	}

	if (g->size != common_length && strcmp(name, BLANK_COMMON_NAME) != 0) {
	    g95_warning(121, "COMMON block '%s' is %d bytes at %L and %d "
			"bytes at %L", name, common_length, &common->where,
			g->size, &g->where);

	    if (common_length > g->size)
		g->size = common_length;
	}

	break;

    case GSYM_FUNCTION:
	break;

	/* Fall through */

    default:
	g95_global_used(g, &common->where);
	g = NULL;
	goto done;
    }

    /* If the common is initialized, declare it now */

    if (seen_init) {
	if (g->backend_decl != NULL_TREE)
	    g95_error("COMMON block '%s' at %L is initialized in another "
		      "program unit", name, &common->where);

	else {
	    ident = (common->bind == NULL)
		? common_identifier(name)
		: get_identifier(common->bind);

            g->backend_decl = build_common_decl(ident, CT_COMMON);
	} 
    }

done:
    free_current_common();
}



/* init_common0()-- Traverse all common blocks within a namespace,
 * figuring out how large each block is, and taking care of an
 * initialization if present. */

static void init_common0(g95_symtree *st) {
g95_common_head *c;

    if (st == NULL)
	return;

    init_common0(st->left);
    init_common0(st->right);

    for(c=st->n.common; c; c=c->next)
	init_common(st->name, c);
}



/* init_common1()-- Traverse namespaces. */

static void init_common1(g95_namespace *ns) {
g95_common_head *h;

    for(; ns; ns=ns->sibling) {
	for(h=ns->blank_common; h; h=h->next)
	    init_blank_common(h);

	unmark_equivalences(ns);
	init_common0(ns->common_root);
	init_common1(ns->contained);
    }
}



/* g95_init_common()-- Recursively scan all common blocks in all
 * namespaces.  Build up the sizes of common blocks, and
 * initializations. */

void g95_init_common(g95_namespace *ns) {
tree identifier;

    init_common1(ns);
    build_uninitialized_common(g95_gsym_root);

    if (blank_common_seen && blank_common_decl == NULL_TREE) {
	identifier = common_identifier(NULL);
	blank_common_decl =
	    blank_block(identifier, blank_common_length, CT_COMMON);
    }
}

