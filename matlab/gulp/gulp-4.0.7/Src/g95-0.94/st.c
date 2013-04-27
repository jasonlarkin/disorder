/* Build executable statement trees
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

/* st.c-- Executable statements are strung together into a singly
 * linked list of code structures.  These structures are later
 * translated into GBE tree structures and from there to executable
 * code for a target.  */

#include "g95.h"
#include <string.h>

g95_code new_st;



/* g95_clear_new_st()-- Zeroes out the new_st structure. */

void g95_clear_new_st(void) {

    memset(&new_st, '\0', sizeof(new_st));
    new_st.type = EXEC_NOP;
}



/* g95_add_statement()-- Takes the current new_st code structure and
 * adds it to the current program unit.  As a side-effect, it zeroes
 * the new_st. */

g95_code *g95_add_statement(void) {
g95_code *p;

    p = g95_get_code(-1, NULL);
    *p = new_st;

    g95_update_locus(&p->where, &g95_current_locus);

    if (g95_state_stack->head == NULL)
	g95_state_stack->head = p;

    *(g95_state_stack->next) = p;

    while(p->next != NULL)
	p = p->next;

    g95_state_stack->tail = p;
    g95_state_stack->next = &p->next;

    g95_clear_new_st();

    return p;
}



/* g95_append_code()-- Given some part of a g95_code structure, append
 * a set of code to its tail, returning a pointer to the new tail. */

g95_code *g95_append_code(g95_code *tail, g95_code *new) {

    if (tail != NULL) {
	while(tail->next != NULL)
	    tail = tail->next;

	tail->next = new;
    }

    while(new->next != NULL)
	new = new->next;

    return new;
}



/* g95_get_code()-- Get a g95_code structure */

g95_code *g95_get_code(int type, g95_locus *where) {
g95_code *c;

    c = g95_getmem(sizeof(g95_code));

    c->type = type;
    c->where = (where == NULL) ? g95_current_locus : *where;

    return c;
}



/* free_statement()-- Free a single code structure, but not the actual
 * structure itself. */

static void free_statement(g95_code *p) {

    if (p->expr)
	g95_free_expr(p->expr);

    if (p->expr2)
	g95_free_expr(p->expr2);

    switch(p->type) {
    case EXEC_NOP:          case EXEC_ASSIGN:         case EXEC_GOTO:
    case EXEC_CYCLE:        case EXEC_RETURN:         case EXEC_STOP:
    case EXEC_EXIT:         case EXEC_PAUSE:          case EXEC_WHERE:
    case EXEC_IOLENGTH:     case EXEC_CONTINUE:       case EXEC_ENTRY:
    case EXEC_DO_WHILE:     case EXEC_ARITHMETIC_IF:  case EXEC_POINTER_ASSIGN:
    case EXEC_TRANSFER:     case EXEC_AC_ASSIGN:      case EXEC_AC_START:
    case EXEC_SYNC_TEAM:    case EXEC_CRITICAL:       case EXEC_ERROR_STOP:
    case EXEC_LABEL_ASSIGN: case EXEC_WHERE_ASSIGN:
	break;

    case EXEC_SYNC_ALL:     case EXEC_SYNC_MEMORY:    case EXEC_SYNC_IMAGES:
    case EXEC_NOTIFY:       case EXEC_QUERY:
	g95_free_expr(p->ext.sync.stat);
	g95_free_expr(p->ext.sync.errmsg);
	g95_free_expr(p->ext.sync.ready);
	break;

    case EXEC_CALL:
	g95_free_actual_arglist(p->ext.sub.actual);
	break;

    case EXEC_SELECT:
	if (p->ext.case_list)
	    g95_free_case_list(p->ext.case_list);

	break;

    case EXEC_DO:
	g95_free_iterator(p->ext.iterator, 1);
	break;

    case EXEC_ALLOCATE:
    case EXEC_DEALLOCATE:
	g95_free_alloc_list(p->ext.alloc_list);
	break;

    case EXEC_OPEN:
	g95_free_open(p->ext.open);
	break;

    case EXEC_CLOSE:
	g95_free_close(p->ext.close);
	break;

    case EXEC_FLUSH:
	g95_free_flush(p->ext.flush);
	break;

    case EXEC_WAIT:
	g95_free_wait(p->ext.wait);
	break;

    case EXEC_BACKSPACE:
    case EXEC_ENDFILE:
    case EXEC_REWIND:
	g95_free_filepos(p->ext.filepos);
	break;

    case EXEC_IF:
	g95_free_statements(p->ext.block);
	break;

    case EXEC_INQUIRE:
	g95_free_inquire(p->ext.inquire);
	break;

    case EXEC_READ:
    case EXEC_WRITE:
	g95_free_dt(p->ext.dt);
	break;

    case EXEC_DT_END:
    /* The ext.dt member is a duplicate pointer and doesn't need to be freed */
	break;

    case EXEC_FORALL:
	g95_free_forall_iterator(p->ext.forall_iterator);
	break;

    default:
	g95_internal_error("free_statement(): Bad statement");
    }
}



/* g95_free_statements()-- Free a code statement and all other code
 * structures linked to it. */

void g95_free_statements(g95_code *p) {
g95_code *q;

    for(; p; p=q) {
	q = p->next;

	if (p->block)
	    g95_free_statements(p->block);

	free_statement(p);
	g95_free(p);
    }
}



/* g95_undo_statement()-- Frees everything associated with the current
 * statement.  */

void g95_undo_statement(void) {

    g95_free_statements(new_st.block);
    g95_free_statements(new_st.next);

    free_statement(&new_st);
    g95_clear_new_st();
}

