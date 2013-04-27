
/* Internal structure dumps
   Copyright (C) 2006 Andy Vaught

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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "g95.h"


static FILE *dump_file = NULL;


#define dump_char(x) fputc(x, dump_file)

static void dump_expr(g95_expr *);
static int st_n = 1;


/* dumpf()-- Formatted output to the dump file */

static void dumpf(char *format, ...) {
char *p, c, buffer[100];
g95_locus *where;
va_list ap;
long ptr;
int m;

    va_start(ap, format);

    for(;;) {
	c = *format++;

	if (c == '\0')
	    break;

	if (c != '%') {
	    dump_char(c);
	    continue;
	}

	switch(*format++) {
	case 'd':
	    sprintf(buffer, "%d", va_arg(ap, int));
	    p = buffer;
	    while(*p != '\0')
		dump_char(*p++);

	    break;

	case 's':
	    p = va_arg(ap, char *);
	    while(*p != '\0')
		dump_char(*p++);

	    break;

	case 'p':
	    ptr = va_arg(ap, long);

	    dump_char('"');
	    dump_char(':');

	    do {
		dump_char('A' + (ptr & 0x0F));
		ptr >>= 4;
	    } while(ptr != 0);

	    dump_char('"');
	    break;

	case 'S':
	    dump_char('\'');
	    p = va_arg(ap, char *);

	    if (p != NULL)
		while(*p) {
		    c = *p++;
		    switch(c) {
		    case '\'':
		    case '\\':
			dump_char('\\');
			/* Fall through */

		    default:
			dump_char(c);
			break;
		    }
		}

	    dump_char('\'');
	    break;

	case 'L':
	    where = va_arg(ap, g95_locus *);

	    if (where == NULL)
		p = "None";

	    else {
		sprintf(buffer, "loc(%d,%d)", where->lb->linenum,
			where->column);
		p = buffer;
	    }

	    while(*p != '\0')
		dump_char(*p++);

	    break;

	case 'C':    /* Statement lists.  Zero is the null list. */
	    m = va_arg(ap, int);

	    if (m == 0) {
		dump_char('[');
		dump_char(']');

	    } else {
		sprintf(buffer, "st%d", m);
		p = buffer;
		while(*p != '\0')
		    dump_char(*p++);
	    }

	    break;

	case '%':
	    dump_char('%');
	    break;

	default:
	    g95_internal_error("dumpf(): Bad %-code");
	    break;
	}
    }

    va_end(ap);
}



/* dump_symtree()-- Dump a symtree if it hasn't been dumped yet. */

static void dump_symtree(g95_symtree *st) {
g95_symbol *sym, *result;
char *module, *name;
sym_flavor flavor;
int rank;

    if (st == NULL)
	return;

    dump_symtree(st->left);
    dump_symtree(st->right);

    sym = st->n.sym;
    if (sym->mark)
	return;

    sym->mark = 1;
    flavor = sym->attr.flavor;

    module = sym->module;
    if (module == NULL && sym->ns->state == COMP_MODULE)
	module = sym->ns->proc_name->name;

    if (g95_current_ns->proc_name == sym && sym->attr.function &&
	sym->result == sym)
	sym->attr.flavor = FL_VARIABLE;

    switch(sym->attr.flavor) {
    case FL_PROGRAM:
    case FL_BLOCK_DATA:
    case FL_MODULE:
	break;

    case FL_VARIABLE:
	rank = (sym->as == NULL) ? 0 : sym->as->rank;

	if (sym->attr.result_var)
	    name = "sym_result1";

	else if (sym->attr.function && sym->result == sym)
	    name = "sym_result2";

	else
	    name = "sym_variable";

	dumpf("%s(%p, %S, %S, %L, %d, %S, %d, %d, %d, %d)\n",
	      name, sym, sym->name, module, &sym->declared_at,
	      sym->attr.use_assoc, g95_typename(&sym->ts), rank,
	      sym->attr.dummy, sym->attr.pointer, sym->attr.in_common);
	break;

    case FL_PARAMETER:
	rank = (sym->as == NULL) ? 0 : sym->as->rank;

	dumpf("sym_parameter(%p, %S, %S, %L, %S, %d)\n",
	      sym, sym->name, sym->module, &sym->declared_at,
	      g95_typename(&sym->ts), rank);
	break;

    case FL_LABEL:
	dumpf("sym_label(%p, %S, %L)\n", sym, sym->name, &sym->declared_at);
	break;

    case FL_PROCEDURE:
	switch(sym->attr.proc) {
	case PROC_INTRINSIC:
	    break;

	case PROC_ST_FUNCTION:
	    dumpf("sym_st_function(%p, %S, %L, %S)\n", sym, sym->name,
		  &sym->declared_at, g95_typename(&sym->ts));
	    break;

	case PROC_MODULE:
	case PROC_INTERNAL:
	case PROC_DUMMY:
	case PROC_EXTERNAL:
	case PROC_UNKNOWN:
	    if (sym == g95_current_ns->proc_name)
		break;

	    if (!sym->attr.function && !sym->attr.subroutine)
		dumpf("sym_procedure(%p, %S, %L)\n", sym, sym->name,
		      &sym->declared_at);

	    else if (sym->attr.subroutine)
		dumpf("sym_subroutine(%p, %S, %S, %L, %d, %d)\n",
		      sym, sym->name, sym->module, &sym->declared_at,
		      sym->attr.use_assoc, sym->attr.proc == PROC_INTERNAL);

	    else {
		result = sym->result;
		rank = (result->as == NULL) ? 0 : result->as->rank;

		dumpf("sym_function(%p, %S, %S, %L, %d, %d, %S, %d, %d)\n",
		      sym, sym->name, sym->module, &sym->declared_at,
		      sym->attr.use_assoc, sym->attr.proc == PROC_INTERNAL,
		      g95_typename(&result->ts), rank, result->attr.pointer,
		      result->attr.pointer);       
	    }
 
	    break;

	default:
	    g95_internal_error("dump_symtree(): Bad procedure\n");
	    break;
	}

	break;

    case FL_DERIVED:
	dumpf("sym_derived(%p, %S, %S, %d, %L)\n", sym, sym->name, sym->module,
	      sym->attr.use_assoc, &sym->declared_at);
	break;

    case FL_NAMELIST:
	dumpf("sym_namelist(%S, %L)\n", sym->name, &sym->declared_at);
	break;

    default:
	g95_internal_error("dump_symtree(): Bad flavor");
	break;
    }

    sym->attr.flavor = flavor;
}



/* dump_name()-- Dump a symbol reference */

static void dump_name(g95_symbol *sym, g95_intrinsic_sym *isym) {

    if (isym == NULL)
	dumpf("%p", sym);

    else if (isym->name[0] != '\0')
	dumpf("%S", isym->name);

    else   /* Nameless intrinsics */
	switch(isym->id) {
	case G95_ISYM_ABS:     dumpf("'abs'");    break;
	case G95_ISYM_ACOS:    dumpf("'acos'");   break;
	case G95_ISYM_AIMAG:   dumpf("'aimag'");  break;
	case G95_ISYM_ASIN:    dumpf("'asin'");   break;
	case G95_ISYM_ATAN:    dumpf("'atan'");   break;
	case G95_ISYM_ATAN2:   dumpf("'atan2'");  break;
	case G95_ISYM_CONJG:   dumpf("'conjg'");  break;
	case G95_ISYM_COS:     dumpf("'cos'");    break;
	case G95_ISYM_COSH:    dumpf("'cosh'");   break;
	case G95_ISYM_EXP:     dumpf("'exp'");    break;
	case G95_ISYM_LOG:     dumpf("'log'");    break;
	case G95_ISYM_LOG10:   dumpf("'log10'");  break;
	case G95_ISYM_MOD:     dumpf("'mod'");    break;
	case G95_ISYM_SIN:     dumpf("'sin'");    break;
	case G95_ISYM_SINH:    dumpf("'sinh'");   break;
	case G95_ISYM_SQRT:    dumpf("'sqrt'");   break;
	case G95_ISYM_TAN:     dumpf("'tan'");    break;
	case G95_ISYM_TANH:    dumpf("'tanh'");   break;
	default:
	    g95_internal_error("dump_name(): Nameless intrinsic!");
	}
}



/* dump_actual()-- Dump an actual argument list */

static void dump_actual(g95_actual_arglist *actual) {

    dump_char('[');

    while(actual != NULL) {
	if (actual->type == ARG_ALT_RETURN)
	    dumpf("%d", actual->u.label->value);

	else if (actual->u.expr == NULL)
	    dumpf("None");

	else
	    dump_expr(actual->u.expr);

	actual = actual->next;

	if (actual != NULL)
	    dump_char(',');
    }

    dump_char(']');
}


/* dump_formal()-- Dump a formal argument list */

static void dump_formal(g95_symbol *sym) {
g95_formal_arglist *f;

    dump_char('[');

    for(f=sym->formal; f; f=f->next)
	if (f->sym == NULL)
	    dumpf("None,");

	else
	    dumpf("%S,", f->sym->name);

    dump_char(']');
}



/* dump_intrinsic()-- Dump an intrinsic operator */

static void dump_intrinsic(g95_expr *e) {
char *name;
int binary;

    binary = 1;
    switch(e->value.op.operator) {
    case INTRINSIC_UPLUS:   name = "uplus";      binary = 0; break;
    case INTRINSIC_NOT:     name = "unot";       binary = 0; break;
    case INTRINSIC_UMINUS:  name = "uminus";     binary = 0; break;
    case INTRINSIC_PLUS:    name = "plus";          break;
    case INTRINSIC_MINUS:   name = "minus";         break;
    case INTRINSIC_TIMES:   name = "times";         break;
    case INTRINSIC_DIVIDE:  name = "divide";        break;
    case INTRINSIC_POWER:   name = "power";         break;
    case INTRINSIC_CONCAT:  name = "concat";        break;
    case INTRINSIC_AND:     name = "logical_and";   break;
    case INTRINSIC_OR:      name = "logical_or";    break;
    case INTRINSIC_EQV:     name = "logical_eqv";   break;
    case INTRINSIC_NEQV:    name = "logical_neqv";  break;
    case INTRINSIC_EQ:      name = "cmp_eq";        break;
    case INTRINSIC_NE:      name = "cmp_ne";        break;
    case INTRINSIC_GT:      name = "cmp_gt";        break;
    case INTRINSIC_GE:      name = "cmp_ge";        break;
    case INTRINSIC_LT:      name = "cmp_lt";        break;
    case INTRINSIC_LE:      name = "cmp_le";        break;

    case INTRINSIC_PAREN:
	dump_expr(e->value.op.op1);
	return;
	
    default:
	g95_internal_error("dump_intrinsic(): Bad intrinsic");
    }

    dumpf("%s(%L,", name, &e->where);
    dump_expr(e->value.op.op1);

    if (binary) {
	dump_char(',');
	dump_expr(e->value.op.op2);
    }

    dump_char(')');
}


/* dump_constant()-- Dump a constant expression */

static void dump_constant(g95_expr *e) {

    switch(e->ts.type) {
    case BT_INTEGER:
	dumpf("integer(%L,%s)", &e->where, bi_to_string(e->value.integer));
	break;

    case BT_REAL:
	dumpf("real(%L,'%s')", &e->where, bg_to_string(e->value.real));
	break;

    case BT_COMPLEX:
	dumpf("complex(%L,'%s','%s')", &e->where,
	      bg_to_string(e->value.complex.r),
	      bg_to_string(e->value.complex.i));
	break;

    case BT_LOGICAL:
	dumpf("logical(%L,%d)", &e->where, e->value.logical);
	break;

    case BT_CHARACTER:
	dumpf("char(%L, %S)", &e->where, e->value.character.string);
	break;

    default:
	g95_internal_error("dump_constant(): Bad constant");
    }
}



/* dump_variable()-- Dump a variable expression. */

static void dump_variable(g95_expr *e) {
g95_ref *ref;
int i;

    dumpf("var(%L,%p,[", &e->where, e->symbol);

    for(ref=e->ref; ref; ref=ref->next) {

	switch(ref->type) {
	case REF_ARRAY:
	    switch(ref->u.ar.type) {
	    case AR_FULL:
		dumpf("ar_full()");
		break;

	    case AR_ELEMENT:
		dumpf("ar_element([");
		for(i=0; i<ref->u.ar.dimen; i++) {
		    dump_expr(ref->u.ar.start[i]);
		    if (i < ref->u.ar.dimen-1)
			dump_char(',');
		}

		dumpf("])");
		break;

	    case AR_SECTION:
		dumpf("ar_section([");
		for(i=0; i<ref->u.ar.dimen; i++) {
		    switch(ref->u.ar.dimen_type[i]) {
		    case DIMEN_ELEMENT:
		    case DIMEN_VECTOR:
			dump_expr(ref->u.ar.start[i]);
			break;

		    case DIMEN_RANGE:
			dump_char('(');
			dump_expr(ref->u.ar.start[i]);
			dump_char(',');
			dump_expr(ref->u.ar.end[i]);
			dump_char(',');
			dump_expr(ref->u.ar.stride[i]);
			dump_char(')');
			break;

		    default:
			g95_internal_error("dump_variable(): Bad dimen");
		    }

		    if (i < ref->u.ar.dimen-1)
			dump_char(',');
		}

		dumpf("])");
		break;

	    default:
		g95_internal_error("dump_variable(): Bad array ref");
	    }

	    break;

	case REF_COARRAY:
	    dumpf("coarray([");

	    for(i=0; i<ref->u.car.dimen; i++) {
		dump_expr(ref->u.car.element[i]);
		dump_char(',');
	    }

	    dumpf("])");
	    break;

	case REF_COMPONENT:
	    dumpf("component(%S)", ref->u.c.name);
	    break;

	case REF_SUBSTRING:
	    dumpf("substring(");

	    dump_expr(ref->u.ss.start);
	    dump_char(',');
	    dump_expr(ref->u.ss.end);
	    dump_char(')');
	    break;

	default:
	    g95_internal_error("dump_variable(): Bad ref");
	}

	if (ref->next != NULL)
	    dump_char(',');
    }

    dump_char(']');
    dump_char(')');
}



/* dump_cons()-- Dump a constructor. */

static void dump_cons(char *name, g95_expr *e) {
g95_constructor *c;

    dumpf("%s(%L,[", name, &e->where);

    for(c=e->value.constructor.c; c; c=c->next) {
	if (c->iterator == NULL)
	    dump_expr(c->expr);

	else {
	    dump_char('(');
	    dump_expr(c->iterator->var);
	    dump_char(',');
	    dump_expr(c->iterator->start);
	    dump_char(',');
	    dump_expr(c->iterator->end);
	    dump_char(',');
	    dump_expr(c->iterator->step);
	    dump_char(',');
	    dump_expr(c->expr);
	    dump_char(')');
	}

	if (c->next != NULL)
	    dump_char(',');
    }

    dumpf("])");
}



/* dump_expr()-- Dump an expression. */

static void dump_expr(g95_expr *e) {

    if (e == NULL) {
	dumpf("None");
	return;
    }

    switch(e->type) {
    case EXPR_NULL:
	dumpf("null(%L,%S,%d)", &e->where, g95_typename(&e->ts), e->rank);
	break;

    case EXPR_OP:
	dump_intrinsic(e);
	break;

    case EXPR_CONSTANT:
	dump_constant(e);
	break;

    case EXPR_VARIABLE:
	dump_variable(e);
	break;

    case EXPR_FUNCTION:
	if (e->value.function.isym != NULL &&
	    e->value.function.isym->id == G95_ISYM_CONVERSION)
	    dump_expr(e->value.function.actual->u.expr);

	else {
	    dumpf("fcall(%L,", &e->where);
	    dump_name(e->symbol, e->value.function.isym);
	    dumpf(",%S,%d,", g95_typename(&e->ts), e->rank);
	    dump_actual(e->value.function.actual);
	    dump_char(')');
	}

	break;

    case EXPR_PROCEDURE:
	dumpf("procedure(%L,", &e->where);
	dump_name(e->symbol, NULL);
	dump_char(')');
	break;

    case EXPR_STRUCTURE:
	dump_cons("scons", e);
	break;

    case EXPR_ARRAY:
	dump_cons("acons", e);
	break;

    case EXPR_SUBSTRING:
	dumpf("substring_exp(%L,", &e->where);
	dump_constant(e);
	dump_char(',');
	dump_expr(e->ref->u.ss.start);
	dump_char(',');
	dump_expr(e->ref->u.ss.end);
	dump_char(')');
	break;

    default:
	g95_internal_error("dump_expr(): Bad expression");
    }
}



/* dump_code()-- Dump a list of code nodes.  Returns the statement
 * list number. */

static int dump_code(g95_code *c) {
int m, n, list_size, *list, node[2];
g95_forall_iterator *f;
g95_filepos *filepos;
g95_inquire *inquire;
g95_close *close;
g95_flush *flush;
g95_alloc *alloc;
g95_open *open;
g95_wait *wait;
g95_case *sel;
g95_code *d;
g95_dt *dt;

    if (c == NULL)
	return 0;

    n = st_n++;
    list = NULL;
    list_size = 0;

    dumpf("%C = []\n", n);

    for(; c; c=c->next) {
	switch(c->type) {
	case EXEC_CONTINUE:
	case EXEC_NOP:
	case EXEC_DT_END:
	    dumpf("%C.append(st_nop(%L", n, &c->where);
	    break;

	case EXEC_ASSIGN:
	    dumpf("%C.append(st_assign(%L,", n, &c->where);
	    dump_expr(c->expr);
	    dump_char(',');
	    dump_expr(c->expr2);
	    break;

	case EXEC_POINTER_ASSIGN:
	    dumpf("%C.append(st_ptr_assign(%L,", n, &c->where);
	    dump_expr(c->expr);
	    dump_char(',');
	    dump_expr(c->expr2);
	    break;

	case EXEC_GOTO:
	    dumpf("%C.append(st_goto(%L, %d", n, &c->where, c->label->value);
	    break;

	case EXEC_PAUSE:
	    dumpf("%C.append(st_pause(%L", n, &c->where);
	    break;

	case EXEC_STOP:
	    dumpf("%C.append(st_stop(%L", n, &c->where);
	    break;

	case EXEC_RETURN:
	    dumpf("%C.append(st_return(%L", n, &c->where);
	    if (c->expr != NULL) {
		dumpf(",rc=");
		dump_expr(c->expr);
	    }

	    break;

	case EXEC_IF:
	    node[0] = dump_code(c->block);
	    node[1] = dump_code(c->ext.block);
	    list = node;
	    list_size = 2;

	    dumpf("%C.append(st_if(%L,", n, &c->where);
	    dump_expr(c->expr);
	    dumpf(",%C,%C", node[0], node[1]);
	    break;

	case EXEC_DO_WHILE:
	    node[0] = dump_code(c->block);
	    list = node;
	    list_size = 1;

	    dumpf("%C.append(st_do_while(%L,", n, &c->where, node[0]);
	    dump_expr(c->expr);

	    dumpf(",%C", node[0]);

	    if (c->sym != NULL)
		dumpf(",label='%s'", c->sym->name);

	    break;

	case EXEC_DO:
	    node[0] = dump_code(c->block);
	    list = node;
	    list_size = 1;

	    dumpf("%C.append(st_do(%L, ", n, &c->where);
	    dump_expr(c->ext.iterator->var);

	    dump_char(',');
	    dump_expr(c->ext.iterator->start);

	    dump_char(',');
	    dump_expr(c->ext.iterator->end);

	    dump_char(',');
	    dump_expr(c->ext.iterator->step);

	    dumpf(",%C", node[0]);

	    if (c->sym != NULL)
		dumpf(",label='%s'", c->sym->name);

	    break;

	case EXEC_OPEN:
	    open = c->ext.open;
	    dumpf("%C.append(st_open(%L", n, &c->where);

	    if (open->unit != NULL) {
		dumpf(",unit=");
		dump_expr(open->unit);
	    }

	    if (open->file != NULL) {
		dumpf(",file=");
		dump_expr(open->file);
	    }

	    if (open->status != NULL) {
		dumpf(",status=");
		dump_expr(open->status);
	    }

	    if (open->access != NULL) {
		dumpf(",access=");
		dump_expr(open->access);
	    }

	    if (open->form != NULL) {
		dumpf(",form=");
		dump_expr(open->form);
	    }

	    if (open->recl != NULL) {
		dumpf(",recl=");
		dump_expr(open->recl);
	    }

	    if (open->decimal != NULL) {
		dumpf(",decimal=");
		dump_expr(open->decimal);
	    }

	    if (open->blank != NULL) {
		dumpf(",blank=");
		dump_expr(open->position);
	    }

	    if (open->position != NULL) {
		dumpf(",position=");
		dump_expr(open->position);
	    }

	    if (open->action != NULL) {
		dumpf(",action=");
		dump_expr(open->action);
	    }

	    if (open->delim != NULL) {
		dumpf(",delim=");
		dump_expr(open->delim);
	    }

	    if (open->pad != NULL) {
		dumpf(",pad=");
		dump_expr(open->pad);
	    }

	    if (open->iostat != NULL) {
		dumpf(",iostat=");
		dump_expr(open->iostat);
	    }

	    if (open->err != NULL)
		dumpf(",err=%d", open->err->value);

	    break;

	case EXEC_CLOSE:
	    close = c->ext.close;
	    dumpf("%C.append(st_close(%L", n, &c->where);

	    if (close->unit != NULL) {
		dumpf(",unit=");
		dump_expr(close->unit);
	    }

	    if (close->status != NULL) {
		dumpf(",status=");
		dump_expr(close->status);
	    }

	    if (close->iostat != NULL) {
		dumpf(",iostat=");
		dump_expr(close->iostat);
	    }

	    if (close->err != NULL)
		dumpf(",err=%d", close->err->value);

	    break;

	case EXEC_BACKSPACE:
	    dumpf("%C.append(st_backspace(%L", n, &c->where);
	    goto show_filepos;

	case EXEC_ENDFILE:
	    dumpf("%C.append(st_endfile(%L", n, &c->where);
	    goto show_filepos;

	case EXEC_REWIND:
	    dumpf("%C.append(st_rewind(%L", n, &c->where);

	show_filepos:
	    filepos = c->ext.filepos;

	    if (filepos->unit != NULL) {
		dumpf(",unit=");
		dump_expr(filepos->unit);
	    }

	    if (filepos->iostat != NULL) {
		dumpf(",iostat=");
		dump_expr(filepos->iostat);
	    }

	    if (filepos->err != NULL)
		dumpf(",err=%d", filepos->err->value);

	    break;

	case EXEC_INQUIRE:
	    dumpf("%C.append(st_inquire(%L", n, &c->where);

	    inquire = c->ext.inquire;

	    if (inquire->unit != NULL) {
		dumpf(",unit=");
		dump_expr(inquire->unit);
	    }

	    if (inquire->file != NULL) {
		dumpf(",file=");
		dump_expr(inquire->file);
	    }

	    if (inquire->iostat != NULL) {
		dumpf(",iostat=");
		dump_expr(inquire->iostat);
	    }

	    if (inquire->exist != NULL) {
		dumpf(",exist=");
		dump_expr(inquire->exist);
	    }

	    if (inquire->opened != NULL) {
		dumpf(",opened=");
		dump_expr(inquire->opened);
	    }

	    if (inquire->number != NULL) {
		dumpf(",number=");
		dump_expr(inquire->number);
	    }

	    if (inquire->named != NULL) {
		dumpf(",named=");
		dump_expr(inquire->named);
	    }

	    if (inquire->name != NULL) {
		dumpf(",name=");
		dump_expr(inquire->name);
	    }

	    if (inquire->access != NULL) {
		dumpf(",access=");
		dump_expr(inquire->access);
	    }

	    if (inquire->sequential != NULL) {
		dumpf(",sequential=");
		dump_expr(inquire->sequential);
	    }

	    if (inquire->direct != NULL) {
		dumpf(",direct=");
		dump_expr(inquire->direct);
	    }

	    if (inquire->form != NULL) {
		dumpf(",form=");
		dump_expr(inquire->form);
	    }

	    if (inquire->formatted != NULL) {
		dumpf(",formatted=");
		dump_expr(inquire->formatted);
	    }

	    if (inquire->unformatted != NULL) {
		dumpf(",unformatted=");
		dump_expr(inquire->unformatted);
	    }

	    if (inquire->recl != NULL) {
		dumpf(",recl=");
		dump_expr(inquire->recl);
	    }

	    if (inquire->nextrec != NULL) {
		dumpf(",nextrec=");
		dump_expr(inquire->nextrec);
	    }

	    if (inquire->blank != NULL) {
		dumpf(",blank=");
		dump_expr(inquire->blank);
	    }

	    if (inquire->position != NULL) {
		dumpf(",position=");
		dump_expr(inquire->position);
	    }

	    if (inquire->action != NULL) {
		dumpf(",action=");
		dump_expr(inquire->action);
	    }

	    if (inquire->read != NULL) {
		dumpf(",read=");
		dump_expr(inquire->read);
	    }

	    if (inquire->write != NULL) {
		dumpf(",write=");
		dump_expr(inquire->write);
	    }

	    if (inquire->readwrite != NULL) {
		dumpf(",readwrite=");
		dump_expr(inquire->readwrite);
	    }

	    if (inquire->delim != NULL) {
		dumpf(",delim=");
		dump_expr(inquire->delim);
	    }

	    if (inquire->pad != NULL) {
		dumpf(",pad=");
		dump_expr(inquire->pad);
	    }

	    if (inquire->pos != NULL) {
		dumpf(",pos=");
		dump_expr(inquire->pos);
	    }

	    if (inquire->iolength != NULL) {
		dumpf(",iolength=");
		dump_expr(inquire->iolength);
	    }

	    if (inquire->size != NULL) {
		dumpf(",size=");
		dump_expr(inquire->size);
	    }

	    if (inquire->err != NULL)
		dumpf(",err=%d", inquire->err->value);

	    break;

	case EXEC_FLUSH:
	    dumpf("%C.append(st_flush(%L", n, &c->where);

	    flush = c->ext.flush;

	    if (flush->unit != NULL) {
		dumpf(",unit=");
		dump_expr(flush->unit);
	    }

	    if (flush->iostat != NULL) {
		dumpf(",iostat=");
		dump_expr(flush->iostat);
	    }

	    if (flush->iomsg != NULL) {
		dumpf(",iomsg=");
		dump_expr(flush->iomsg);
	    }

	    if (flush->err != NULL)
		dumpf(",err=%d", flush->err->value);

	    break;

	case EXEC_WAIT:
	    dumpf("%C.append(st_wait(%L", n, &c->where);

	    wait = c->ext.wait;

	    if (wait->unit != NULL) {
		dumpf(",unit=");
		dump_expr(wait->unit);
	    }

	    if (wait->id != NULL) {
		dumpf(",id=");
		dump_expr(wait->id);
	    }

	    if (wait->iostat != NULL) {
		dumpf(",iostat=");
		dump_expr(wait->iostat);
	    }

	    if (wait->iomsg != NULL) {
		dumpf(",iomsg=");
		dump_expr(wait->iomsg);
	    }

	    if (wait->err != NULL)
		dumpf(",err=%d", wait->err->value);

	    if (wait->end != NULL)
		dumpf(",end=%d", wait->end->value);

	    if (wait->eor != NULL)
		dumpf(",eof=%d", wait->eor->value);

	    break;

	case EXEC_IOLENGTH:
	    dumpf("%C.append(st_iolength(%L,", n, &c->where);
	    dump_expr(c->expr);
	    break;

	case EXEC_WRITE:
	    dumpf("%C.append(st_write(%L", n, &c->where);
	    goto show_dt;

	case EXEC_READ:
	    dumpf("%C.append(st_read(%L", n, &c->where);

	show_dt:
	    dt = c->ext.dt;

	    if (dt->io_unit->ts.type == BT_INTEGER)
		dumpf(",unit=");

	    else
		dumpf(",internal_unit=");

	    dump_expr(dt->io_unit);

	    if (dt->format_expr != NULL) {
		dumpf(",format_expr=");
		dump_expr(dt->format_expr);
	    }

	    if (dt->rec != NULL) {
		dumpf(",rec=");
		dump_expr(dt->rec);
	    }

	    if (dt->advance != NULL) {
		dumpf(",advance=");
		dump_expr(dt->advance);
	    }

	    if (dt->iostat != NULL) {
		dumpf(",iostat=");
		dump_expr(dt->iostat);
	    }

	    if (dt->size != NULL) {
		dumpf(",size=");
		dump_expr(dt->size);
	    }

	    if (dt->pos != NULL) {
		dumpf(",pos=");
		dump_expr(dt->pos);
	    }

	    if (dt->decimal != NULL) {
		dumpf(",decimal=");
		dump_expr(dt->decimal);
	    }

	    if (dt->namelist != NULL)
		dumpf(",namelist=(%S,%L)", dt->namelist->name,
		      &dt->namelist_where);

	    if (dt->format_label != NULL)
		dumpf(",format_label=%d", dt->format_label->value);

	    if (dt->err != NULL)
		dumpf(",err=%d", dt->err->value);

	    if (dt->end != NULL)
		dumpf(",end=%d", dt->end->value);

	    if (dt->eor != NULL)
		dumpf(",eof=%d", dt->eor->value);

	    break;

	case EXEC_TRANSFER:
	    dumpf("%C.append(st_transfer(%L,%d,", n, &c->expr->where,
		  c->ext.transfer == M_READ);
	    dump_expr(c->expr);
	    break;

	case EXEC_ALLOCATE:
	    dumpf("%C.append(st_allocate(%L,", n, &c->where);
	    goto show_alloc;

	case EXEC_DEALLOCATE:
	    dumpf("%C.append(st_deallocate(%L,", n, &c->where);

	show_alloc:
	    dumpf("[");
	    alloc = c->ext.alloc_list;

	    while(alloc != NULL) {
		dump_expr(alloc->expr);
		
		if (alloc->next != NULL)
		    dump_char(',');

		alloc = alloc->next;
	    }

	    dumpf("]");

	    if (c->expr != NULL) {
		dumpf(",stat=");
		dump_expr(c->expr);
	    }

	    break;

	case EXEC_ARITHMETIC_IF:
	    dumpf("%C.append(st_arith_if(%L,", n, &c->where);
	    dump_expr(c->expr);
	    dumpf(", %d, %d, %d", c->label->value, c->label2->value,
		  c->label3->value);
	    break;

	case EXEC_LABEL_ASSIGN:
	    dumpf("%C.append(st_label_assign(%L,", n, &c->where);
	    dump_expr(c->expr);
	    dumpf(", %d", c->label->value);
	    break;

	case EXEC_SELECT:
	    for(d=c->block; d; d=d->block)
		list_size++;

	    list = g95_getmem(list_size * sizeof(int));
	    m = 0;

	    for(d=c->block; d; d=d->block)
		list[m++] = dump_code(d->next);

	    dumpf("%C.append(st_select(%L, ", n, &c->where);

	    dump_expr(c->expr);
	    dumpf(",[");

	    m = 0;

	    for(d=c->block; d; d=d->next) {
		dumpf("[");

		for(sel=d->ext.case_list; sel; sel=sel->next) {
		    dump_char('(');

		    if (sel->low == NULL)
			dumpf("None");
		    else
			dump_expr(sel->low);

		    dumpf(",");

		    if (sel->high == NULL)
			dumpf("None");
		    else
			dump_expr(sel->high);
		}

		dumpf("],%C,", list[m++]);
	    }

	    dump_char(']');
	    break;

	case EXEC_CYCLE:
	    dumpf("%C.append(st_cycle(%L", n, &c->where);

	    if (c->sym != NULL)
		dumpf(",label=%p", c->sym);

	    break;

	case EXEC_EXIT:
	    dumpf("%C.append(st_exit(%L", n, &c->where);

	    if (c->sym != NULL)
		dumpf(",label=%p", c->sym);

	    break;

	case EXEC_ENTRY:
	    dumpf("%C.append(st_entry(%L,'%s',", n, &c->where, c->sym->name);
	    dump_formal(c->sym);
	    break;

	case EXEC_WHERE:
	    for(d=c->block; d; d=d->block)
		list_size++;

	    list = g95_getmem(list_size * sizeof(int));

	    m = 0;
	    for(d=c->block; d; d=d->block)
		list[m++] = dump_code(d->next);

	    dumpf("%C.append(st_where(%L, [", n, &c->where);

	    m = 0;
	    for(d=c->block; d; d=d->block) {
		dump_char('(');

		if (d->expr == NULL)
		    dumpf("None");
		else
		    dump_expr(d->expr);

		dumpf(",%C),", list[m++]);
	    }

	    dump_char(']');
	    break;

	case EXEC_FORALL:
	    node[0] = dump_code(c->block);
	    list = node;
	    list_size = 1;

	    dumpf("%C.append(st_forall(%L, [", n, &c->where);

	    for(f=c->ext.forall_iterator; f; f=f->next) {
		dump_char('(');
		dump_expr(f->var);
		dump_char(',');
		dump_expr(f->start);
		dump_char(',');
		dump_expr(f->end);
		dump_char(',');
		dump_expr(f->stride);
		dump_char(')');

		if (f->next != NULL)
		    dump_char(',');
	    }

	    dumpf("], %C", node[0]);

	    if (c->expr != NULL) {
		dumpf(", mask=");
		dump_expr(c->expr);
	    }

	    break;

	case EXEC_CALL:
	    dumpf("%C.append(st_call(%L,", n, &c->where);
	    dump_name(c->sym, c->ext.sub.isym);
	    dump_char(',');
	    dump_actual(c->ext.sub.actual);
	    break;

	default:
	    g95_internal_error("dump_code(): Bad code");
	    break;
	}

	if (c->here != NULL)
	    dumpf(",here=%d", c->here->value);

	dumpf("))\n");

	for(m=0; m<list_size; m++)
	    if (list[m] != 0)
		dumpf("del %C\n", list[m]);

	list_size = 0;

	if (list != NULL && list != node)
	    g95_free(list);
    }

    return n;
}



static void dump_common(g95_symtree *st) {
g95_common_head *c;

    if (st == NULL)
	return;

    dump_common(st->left);
    dump_common(st->right);

    dumpf("sym_common(%S, [", st->name);

    for(c=st->n.common; c; c=c->next)
	dumpf("%L,", &c->where);

    dumpf("])\n");
}



/* dump_ns()-- Dump a namespace. */

static void dump_ns(g95_namespace *ns) {
g95_symbol *sym, *result;
g95_namespace *p, *save;
g95_locus *where;
g95_annot *a;
int m, rank;

    save = g95_current_ns; 
    g95_current_ns = ns;
  
    where = &ns->declared_at;
    sym = ns->proc_name;

    switch(ns->state) {
    case COMP_PROGRAM:
	if (ns->unit_name == NULL)
	    dumpf("program(None, %L)\n", where);
	else
	    dumpf("program(%S,%L)\n", ns->unit_name, where);

	break;

    case COMP_MODULE:
	dumpf("module(%S,%L,%L)\n", sym->name, where,
	      &ns->proc_name->declared_at);
	break;

    case COMP_SUBROUTINE:
	dumpf("subroutine(%S,%S,%L,", sym->name, sym->module,
	      &ns->proc_name->declared_at);
	dump_formal(ns->proc_name);
	dumpf(")\n");
	break;

    case COMP_FUNCTION:
	result = sym->result;
	rank = (result->as == NULL) ? 0 : result->as->rank;

	dumpf("function(%S,%S,%L,%S,%d,%d,", sym->name, sym->module,
	      &ns->proc_name->declared_at, g95_typename(&result->ts), rank,
	      result->attr.pointer);

	dump_formal(ns->proc_name);
	dumpf(")\n");
	break;

    case COMP_BLOCK_DATA:
	if (ns->proc_name->name == NULL)
	    dumpf("block_data(None,%L)\n", where);
	else
	    dumpf("block_data(%S,%L)\n", sym->name, where);

	break;

    case COMP_NONE:
	return;

    default:
	g95_internal_error("dump_ns(): Bad state");
    }

    g95_traverse_symtree(ns, g95_clear_sym_mark);
    g95_traverse_symtree(ns, dump_symtree);

    dump_common(ns->common_root);

    for(a=ns->annotation; a; a=a->next)
	switch(a->type) {
	case ANNOT_PARAMETER:
	    dumpf("parameter_use(%p,%L)\n", a->u.sym, &a->where);
	    break;

	case ANNOT_DERIVED:
	    dumpf("derived_use(%p,%L)\n", a->u.sym, &a->where);
	    break;

	case ANNOT_LABEL:
	    dumpf("label_use(%p,%L)\n", a->u.sym, &a->where);
	    break;

	case ANNOT_OPERATOR:
	    dumpf("operator_use(%p,%L)\n", a->u.sym, &a->where);
	    break;

	default:
	    g95_internal_error("init_dump(): Bad type");
	}

    m = dump_code(ns->code);
    dumpf("add_code(%C)\n", m);

    if (m != 0)
	dumpf("del %C\n", m);

    for(p=ns->contained; p; p=p->sibling)
	dump_ns(p);

    dumpf("end()\n");
    g95_current_ns = save;
}



/* init_dump()-- Start the dump */

static void init_dump(void) {
char *p, name[PATH_MAX+20];
g95_linebuf *b;
    
    strcpy(name, g95_source_file);

    p = strchr(name, '\0');
    while(p >= name && *p != '.')
	p--;

    if (p < name)
	return;

    strcpy(p, ".g95");

    dump_file = fopen(name, "w+");
    if (dump_file == NULL)
	return;

    dumpf("source('G95 Rocks! http://www.g95.org', %S)\n", g95_source_file);

    b = g95_first_line();

    while(b) {
	dumpf("line(%S)\n", b->line);
	b = b->next;
    }
}



/* g95_dump()-- Dump a source file. */

void g95_dump(g95_namespace *ns) {

    if (dump_file == NULL && getenv("G95_DUMP") != NULL)
	init_dump();

    if (dump_file != NULL)
	dump_ns(ns);
}



/* g95_dump_done()-- Finalize the dump. */

void g95_dump_done(void) {

    if (dump_file != NULL)
	fclose(dump_file);
}



/* g95_annotate()-- Add an element to the annotation list. */

g95_annot *g95_annotate(int type, g95_locus *where) {
g95_annot *a;

    a = g95_getmem(sizeof(g95_annot));

    a->type = type;
    a->where = *where;

    a->next = g95_current_ns->annotation;
    g95_current_ns->annotation = a;

    return a;
}

