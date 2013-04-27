/* Internal structure dumps
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

#include "g95.h"


static int show_level=0;


static mstring array_specs[] = {
    minit("EXPLICIT",       AS_EXPLICIT),
    minit("ASSUMED-SHAPE",  AS_ASSUMED_SHAPE),
    minit("DEFERRED",       AS_DEFERRED),
    minit("ASSUMED-SIZE",   AS_ASSUMED_SIZE),
    minit(NULL, 0) };


void show_code(int, g95_code *);



/* show_actual_arglist()-- Show an actual argument list */

static void show_actual_arglist(g95_actual_arglist *a) {

    g95_status_char('[');

    for(; a; a=a->next) {
	g95_status("arg(");  

	if (a->type == ARG_ALT_RETURN)
	    g95_status(", alt=%d", a->u.label->value);

	else if (a->u.expr == NULL)
	    g95_status("None");

	else {
	    g95_show_expr(a->u.expr);

	    if (a->pointer)
		g95_status("pointer=1");

	    if (a->name != NULL)
		g95_status("name='%s'", a->name);

	    switch(a->type) {
	    case ARG_ARRAY:          g95_status(", array=1");   break;
	    case ARG_ARRAY_ELEMENT:  g95_status(", element=1"); break;
	    case ARG_ARRAY_DESC:     g95_status(", desc=1 ");   break;
	    default:                                            break;
	    }
	}

	g95_status_char(')');

	if (a->next != NULL)
	    g95_status(", ");
    }

    g95_status_char(']');
}




/* show_array_ref()-- Show an array reference */

static void show_array_ref(g95_array_ref *ar) {
int i;

    switch(ar->type) {
    case AR_FULL:
	g95_status("full()");
	break;

    case AR_SECTION: 
	g95_status("section(");
	for(i=0; i<ar->dimen; i++) {
	    g95_status_char('(');
	    g95_show_expr(ar->start[i]);

	    if (ar->end[i] != NULL) {
		g95_status(", ");
		g95_show_expr(ar->end[i]);
	    }

	    if (ar->stride[i] != NULL) {
		g95_status(", ");
		g95_show_expr(ar->stride[i]);
	    }

	    g95_status_char(')');

	    if (i != ar->dimen-1)
		g95_status(", ");
	}

	g95_status_char(')');
	break;

    case AR_ELEMENT:
	g95_status("element(");

	for(i=0; i<ar->dimen; i++) {
	    g95_show_expr(ar->start[i]);
	    if (i != ar->dimen - 1)
		g95_status(", ");
	}

	g95_status_char(')');
	break;

    case AR_UNKNOWN:
	g95_status("UNKNOWN");
	break;

    default:
	g95_internal_error("g95_show_array_ref(): Unknown array reference");
    }
}



/* show_ref()-- Show a single g95_ref structure. */

static void show_ref(g95_ref *p) {
int i;

    switch(p->type) {
    case REF_ARRAY:
	show_array_ref(&p->u.ar);
	break;

    case REF_COMPONENT:
	g95_status("comp('%s')", p->u.c.component->name);
	break;

    case REF_SUBSTRING:
	g95_status("substr(start=");
	g95_show_expr(p->u.ss.start);

	g95_status(", end=");
	g95_show_expr(p->u.ss.end);

	g95_status_char(')');
	break;

    case REF_COARRAY:
	g95_status("coarray(");

	for(i=0; i<p->u.car.dimen; i++) {
	    g95_show_expr(p->u.car.element[i]);

	    if (i != p->u.car.dimen - 1)
		g95_status_char(',');
	}

	g95_status_char(')');
	break;

    default:
	g95_internal_error("show_ref(): Bad component code");
    }
}



/* show_refs()-- Show a list of reference structures */

static void show_refs(g95_ref *ref) {

    g95_status_char('[');

    for(; ref; ref=ref->next) {
	show_ref(ref);

	if (ref->next != NULL)
	    g95_status(", ");
    }

    g95_status_char(']');
}



/* show_constructor()-- Display a constructor.  Works recursively for
 * array constructors. */

static void show_constructor(g95_constructor *c) {

    for(;c ;c=c->next) {
	if (c->iterator == NULL)
	    g95_show_expr(c->expr);
	else {
	    g95_status_char('(');
	    g95_show_expr(c->expr);

	    g95_status_char(' ');
	    g95_show_expr(c->iterator->var);
	    g95_status_char('=');
	    g95_show_expr(c->iterator->start);
	    g95_status_char(',');
	    g95_show_expr(c->iterator->end);
	    g95_status_char(',');
	    g95_show_expr(c->iterator->step);

	    g95_status_char(')');
	}

	if (c->next != NULL)
	    g95_status(", ");
    }
}



/* show_string_constant()-- Dump a string constant */

static void show_string_constant(char *p, int length) {
int i, quote, seen_single, seen_double;

    seen_single = seen_double = 0;

    for(i=0; i<length; i++) {
	seen_single |= (p[i] == '\'');
	seen_double |= (p[i] == '"');
    }

    quote = (seen_single && !seen_double) ? '"' : '\'';

    g95_status_char(quote);

    for(i=0; i<length; i++) {
	g95_status_char(p[i]);

	if (p[i] == '\\' || p[i] == quote)
	    g95_status_char(p[i]);
    }

    g95_status_char(quote);
}



/* show_constant()-- Show a constant */

static void show_constant(g95_expr *p) {

    switch(p->ts.type) {
    case BT_INTEGER:
	g95_status_char('\'');
	fputs(bi_to_string(p->value.integer), stdout);

	if (p->ts.kind != g95_default_integer_kind(0))
	    g95_status("_%d", p->ts.kind);

	g95_status_char('\'');
	break;

    case BT_LOGICAL:
	g95_status(p->value.logical ? "'.true.'" : "'.false.'");
	break;

    case BT_REAL:
	g95_status_char('\'');
	fputs(bg_to_string(p->value.real), stdout);

	if (p->ts.kind != g95_default_real_kind(1))
	    g95_status("_%d", p->ts.kind);

	g95_status_char('\'');
	break;

    case BT_CHARACTER:
	show_string_constant(p->value.character.string,
			     p->value.character.length);
	break;

    case BT_COMPLEX:
	g95_status_char('\'');

	fputs(bg_to_string(p->value.complex.r), stdout);
	if (p->ts.kind != g95_default_complex_kind())
	    g95_status("_%d", p->ts.kind);

	g95_status_char(' ');

	fputs(bg_to_string(p->value.complex.i), stdout);
	if (p->ts.kind != g95_default_complex_kind())
	    g95_status("_%d", p->ts.kind);

	g95_status_char('\'');
	break;

    default:
	g95_internal_error("show_constant(): Bad type");
	break;
    }
}



/* g95_show_expr()-- show an expression */

void g95_show_expr(g95_expr *p) {
g95_symbol *sym;

    if (p == NULL) {
	g95_status("expr()");
	return;
    }

/* Show expression */

    switch(p->type) {
    case EXPR_SUBSTRING:
	g95_status("substr(");

	show_string_constant(p->value.character.string,
			     p->value.character.length);

	g95_status(", start=");
	g95_show_expr(p->ref->u.ss.start);
	g95_status(", end=");
	g95_show_expr(p->ref->u.ss.end);
	break;

    case EXPR_STRUCTURE:
	g95_status("scons('%s', cons=", p->symbol->name);
	show_constructor(p->value.constructor.c);
	break;

    case EXPR_ARRAY:
	g95_status("acons(cons=");
	show_constructor(p->value.constructor.c);
 
	if (p->ref != NULL) {
	    g95_status(", ref=");
	    show_refs(p->ref);
	}

	break;

    case EXPR_NULL:
	g95_status("null()");
	break;

    case EXPR_CONSTANT:
	g95_status("const(");
	show_constant(p);
	break;

    case EXPR_VARIABLE:
	g95_status("var(");

	sym = p->symbol;

	if (sym->ns->proc_name != NULL && sym->ns->proc_name->name != NULL)
	    g95_status("'%s:%s'", sym->ns->proc_name->name, sym->name);
	else
	    g95_status("'%s'", sym->name);

	if (p->ref != NULL) {
	    g95_status(", ref=");
	    show_refs(p->ref);
	}

	break;

    case EXPR_OP:
	g95_status("op(");
	switch(p->value.op.operator) {
	case INTRINSIC_UPLUS:   g95_status("'U+'");    break;
	case INTRINSIC_UMINUS:  g95_status("'U-'");    break;
	case INTRINSIC_PLUS:    g95_status("'+'");     break;
	case INTRINSIC_MINUS:   g95_status("'-'");     break;
	case INTRINSIC_TIMES:   g95_status("'*'");     break;
	case INTRINSIC_DIVIDE:  g95_status("'/'");     break;
	case INTRINSIC_POWER:   g95_status("'**'");    break;
	case INTRINSIC_CONCAT:  g95_status("'//'");    break;
	case INTRINSIC_AND:     g95_status("'AND'");   break;
	case INTRINSIC_OR:      g95_status("'OR'");    break;
	case INTRINSIC_EQV:     g95_status("'EQV'");   break;
	case INTRINSIC_NEQV:    g95_status("'NEQV'");  break;
	case INTRINSIC_EQ:      g95_status("'='");     break;
	case INTRINSIC_NE:      g95_status("'<>'");    break;
	case INTRINSIC_GT:      g95_status("'>'");     break;
	case INTRINSIC_GE:      g95_status("'>='");    break;
	case INTRINSIC_LT:      g95_status("'<'");     break;
	case INTRINSIC_LE:      g95_status("'<='");    break;
	case INTRINSIC_NOT:     g95_status("'NOT'");   break;
	case INTRINSIC_PAREN:   g95_status("()");      break;

	default:
	    g95_internal_error("g95_show_expr(): "
			       "Bad intrinsic in expression!");
	}

	g95_status(", op1=");
	g95_show_expr(p->value.op.op1);

	if (p->value.op.op2) {
	    g95_status(", op2=");
	    g95_show_expr(p->value.op.op2);
	}

	break;

    case EXPR_FUNCTION:
	g95_status("func(");
	if (p->value.function.isym == NULL)
	    g95_status("'%s:%s'", p->symbol->module, p->symbol->name);
	else
	    g95_status("'%s'", p->value.function.iname);

	g95_status(", arg=");
	show_actual_arglist(p->value.function.actual);
	break;

    case EXPR_UNKNOWN:
	g95_status("unknown('%s'", p->symbol->name);
	break;

    default:
	g95_internal_error("g95_show_expr(): Bad type");
	break;
    }

    g95_status_char(')');
}



/* code_indent()-- Do indentation for a specific level */

static void code_indent(int level, g95_st_label *label) {
int i;

    if (label != NULL)
	g95_status("%-6d; ", label->value);
    else
	g95_status("        ");

    for(i=0; i<2*level; i++)
	g95_status_char(' ');
}



/* show_code0()-- Show a single code node and everything underneath it
 * if necessary. */

static void show_code0(int level, g95_code *c) {
g95_forall_iterator *fa;
char *module, *name;
g95_close *close;
g95_filepos *fp;
g95_inquire *i;
g95_open *open;
int m, comma;
g95_case *cp;
g95_alloc *a;
g95_code *d;
g95_dt *dt;

    code_indent(level, c->here); 

    switch(c->type) {
    case EXEC_NOP:
	g95_status("nop(");
	comma = 0;
	break;

    case EXEC_CONTINUE:
	g95_status("cont(");
	comma = 0;
	break;

    case EXEC_AC_START:
	g95_status("ac_start(sym='%s'", c->sym->name);
	comma = 1;
	break;

    case EXEC_AC_ASSIGN:
	g95_status("ac_assign(sym='%s', expr=", c->sym->name);
	g95_show_expr(c->expr);
	break;

    case EXEC_ASSIGN:
	g95_status("assign(lhs=");
	g95_show_expr(c->expr);
	g95_status(", rhs=");
	g95_show_expr(c->expr2);
	comma = 1;
	break;

    case EXEC_WHERE_ASSIGN:
	g95_status("where_assign(lhs=");
	g95_show_expr(c->expr);
	g95_status(", rhs=");
	g95_show_expr(c->expr2);
	comma = 1;
	break;

    case EXEC_POINTER_ASSIGN:
	g95_status("ptr_assign(lhs=");
	g95_show_expr(c->expr);
	g95_status(", rhs=");
	g95_show_expr(c->expr2);
	comma = 1;
	break;

    case EXEC_GOTO:
	if (c->label != NULL)
	    g95_status("goto(label=%d", c->label->value);
	else
	    g95_status("goto(var='%s'", c->sym->name);

	comma = 1;
	break;

    case EXEC_CALL:
	if (c->sym == NULL)
	    module = name = "";
	else {
	    module = c->sym->module;
	    name   = c->sym->name;
	}

	g95_status("call(name='%s:%s', sub='%s', arg=",
		   module, name, c->ext.sub.sub_name);
	show_actual_arglist(c->ext.sub.actual);
	comma = 1;
	break;

    case EXEC_RETURN:
	g95_status("ret(");
	comma = 0;

	if (c->expr) {
	    g95_status("value=");
	    g95_show_expr(c->expr);
	    comma = 1;
	}

	break;

    case EXEC_STOP:
	g95_status("stop(");

	if (c->expr != NULL) {
	    g95_status("expr=");
	    g95_show_expr(c->expr);
	} else
	    g95_status("code=%d", c->ext.stop_code);

	comma = 1;
	break;

  case EXEC_PAUSE:
      g95_status("pause(");

      if (c->expr != NULL) {
	  g95_status("expr=");
	  g95_show_expr(c->expr);
      } else
	  g95_status("code=%d", c->ext.stop_code);

      comma = 1;
      break;

    case EXEC_ARITHMETIC_IF:
	g95_status("arith_if(expr=");

	g95_show_expr(c->expr);

	g95_status(", lt0=%d, eq0=%d, gt0=%d",
		   c->label->value, c->label2->value, c->label3->value);

	comma = 1;
	break;

    case EXEC_IF:
	g95_status("log_if(expr=");
	g95_show_expr(c->expr);

	g95_status(", true=\n");
	show_code(level+1, c->block);

	if (c->ext.block != NULL) {
	    code_indent(level, 0);
	    g95_status(", else=\n");
	    show_code(level+1, c->ext.block);
	}

	code_indent(level, c->label);
	g95_status(") # ENDIF\n");
	break;

    case EXEC_SELECT:
	d = c->block;
	g95_status("select(expr=");
	g95_show_expr((c->expr != NULL) ? c->expr : c->expr2);
	g95_status(", cases=[\n");

	for(;d ;d=d->block) {
	    code_indent(level, 0);

	    g95_status("case(");
	    for(cp=d->ext.case_list; cp; cp=cp->next) {
		g95_status_char('(');
		g95_show_expr(cp->low);
		g95_status_char(' ');
		g95_show_expr(cp->high);
		g95_status_char(')');
		g95_status_char(' ');
	    }

	    g95_status_char('\n');

	    show_code(level+1, d->next);
	}

	code_indent(level, c->label);
	g95_status("END SELECT");
	break;

    case EXEC_WHERE:
	g95_status("WHERE ");

	d = c->block;
	g95_show_expr(d->expr);
	g95_status_char('\n');

	show_code(level+1, d->next);

	for(d=d->block; d; d=d->block) {
	    code_indent(level, 0);
	    g95_status("ELSE WHERE ");
	    g95_show_expr(d->expr);
	    g95_status_char('\n');
	    show_code(level+1, d->next);
	}

	code_indent(level, 0);
	g95_status("END WHERE");
	break;

    case EXEC_FORALL:
	g95_status("FORALL ");
	for(fa=c->ext.forall_iterator; fa; fa=fa->next) {
	    g95_show_expr(fa->var);
	    g95_status_char(' ');
	    g95_show_expr(fa->start);
	    g95_status_char(':');
	    g95_show_expr(fa->end);
	    g95_status_char(':');
	    g95_show_expr(fa->stride);

	    if (fa->next != NULL)
		g95_status_char(',');
	}

	if (c->expr != NULL) {
	    g95_status_char(',');
	    g95_show_expr(c->expr);
	}

	g95_status_char('\n');

	show_code(level+1, c->block);

	code_indent(level, 0);
	g95_status("END FORALL");
	break;
    
    case EXEC_DO:
	g95_status("do(var=");
	g95_show_expr(c->ext.iterator->var);

	g95_status(", start=");
	g95_show_expr(c->ext.iterator->start);

	g95_status(", end=");
	g95_show_expr(c->ext.iterator->end);

	g95_status(", step=");
	g95_show_expr(c->ext.iterator->step);

	g95_status(", body=\n");
	show_code(level+1, c->block);

	code_indent(level, 0);
	comma = 1;
	break;

    case EXEC_DO_WHILE:
	g95_status("do_while(expr=");
	g95_show_expr(c->expr);
	g95_status(", body=");

	show_code(level+1, c->block);

	code_indent(level, c->label);
	break;

    case EXEC_CYCLE:
	g95_status("cycle(");
	comma = 0;

	if (c->sym) {
	    g95_status("label='%s'", c->sym->name);
	    comma = 1;
	}

	break;

    case EXEC_EXIT:
	g95_status("exit(");
	comma = 0;

	if (c->sym) {
	    g95_status("label='%s'", c->sym->name);
	    comma = 1;
	}

	break;

    case EXEC_ALLOCATE:
	g95_status("allocate(");
	comma = 0;

	if (c->expr) {
	    g95_status("stat=");
	    g95_show_expr(c->expr);
	    comma = 1;
	}

	if (comma)
	    g95_status(", ");

	g95_status("alloc=[");

	for(a=c->ext.alloc_list; a; a=a->next) {
	    g95_show_expr(a->expr);
	    g95_status_char('(');

	    for(m=0; m < a->rank+a->corank; m++) {
		g95_show_expr(a->lower[m]);
		g95_status_char(':');
		g95_show_expr(a->upper[m]);

		if (m != a->rank + a->corank - 1)
		    g95_status_char(',');
	    }

	    g95_status_char(')');

	    if (a->next != NULL)
		g95_status(", ");
	}

	g95_status_char(']');
	comma = 1;
	break;

    case EXEC_DEALLOCATE:
	g95_status("deallocate(");
	comma = 0;

	if (c->expr) {
	    g95_status("stat=");
	    g95_show_expr(c->expr);
	    comma = 1;
	}

	if (comma)
	    g95_status(", ");

	g95_status("alloc=[");

	for(a=c->ext.alloc_list; a; a=a->next) {
	    g95_show_expr(a->expr);
	    if (a->next != NULL)
		g95_status(", ");
	}

	g95_status_char(']');
	comma = 1;
	break;

    case EXEC_OPEN:
	g95_status("open_(");
	open = c->ext.open;
	comma = 0;

	if (open->unit) {
	    if (comma)
		g95_status(", ");

	    g95_status("unit=");
	    g95_show_expr(open->unit);
	    comma = 1;
	}

	if (open->iostat) {
	    if (comma)
		g95_status(", ");

	    g95_status("iostat=");
	    g95_show_expr(open->iostat);
	    comma = 1;
	}

	if (open->file) {
	    if (comma)
		g95_status(", ");

	    g95_status("file=");
	    g95_show_expr(open->file);
	    comma = 1;
	}

	if (open->status) {
	    if (comma)
		g95_status(", ");

	    g95_status("status=");
	    g95_show_expr(open->status);
	    comma = 1;
	}

	if (open->access) {
	    if (comma)
		g95_status(", ");

	    g95_status("access=");
	    g95_show_expr(open->access);
	    comma = 1;
	}

	if (open->form) {
	    if (comma)
		g95_status(", ");

	    g95_status("form=");
	    g95_show_expr(open->form);
	    comma = 1;
	}

	if (open->recl) {
	    if (comma)
		g95_status(", ");

	    g95_status("recl=");
	    g95_show_expr(open->recl);
	    comma = 1;
	}

	if (open->blank) {
	    if (comma)
		g95_status(", ");

	    g95_status("blank=");
	    g95_show_expr(open->blank);
	    comma = 1;
	}

	if (open->position) {
	    if (comma)
		g95_status(", ");

	    g95_status("position=");
	    g95_show_expr(open->position);
	    comma = 1;
	}

	if (open->action) {
	    if (comma)
		g95_status(", ");

	    g95_status("action=");
	    g95_show_expr(open->action);
	    comma = 1;
	}

	if (open->delim) {
	    if (comma)
		g95_status(", ");

	    g95_status("delim=");
	    g95_show_expr(open->delim);
	    comma = 1;
	}

	if (open->pad) {
	    if (comma)
		g95_status(", ");

	    g95_status("pad=");
	    g95_show_expr(open->pad);
	    comma = 1;
	}

	if (open->err != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("err=%d", open->err->value);
	    comma = 1;
	}

	break;

    case EXEC_CLOSE:
	g95_status("close(");
	close = c->ext.close;
	comma = 0;

	if (close->unit) {
	    if (comma)
		g95_status(", ");
	    
	    g95_status("unit=");
	    g95_show_expr(close->unit);
	    comma = 1;
	}

	if (close->iostat) {
	    if (comma)
		g95_status(", ");

	    g95_status("iostat=");
	    g95_show_expr(close->iostat);
	    comma = 1;
	}

	if (close->status) {
	    if (comma)
		g95_status(", ");

	    g95_status("status=");
	    g95_show_expr(close->status);
	    comma = 1;
	}

	if (close->err != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("err=%d", close->err->value);
	    comma = 1;
	}

	break;

    case EXEC_BACKSPACE:
	g95_status("backspace(");
	goto show_filepos;

    case EXEC_ENDFILE:
	g95_status("endfile(");
	goto show_filepos;

    case EXEC_REWIND:
	g95_status("rewind(");

    show_filepos:
	fp = c->ext.filepos;
	comma = 0;

	if (fp->unit) {
	    if (comma)
		g95_status(", ");

	    g95_status("unit=");
	    g95_show_expr(fp->unit);
	    comma = 1;
	}

	if (fp->iostat) {
	    if (comma)
		g95_status(", ");
	    
	    g95_status("iostat=");
	    g95_show_expr(fp->iostat);
	    comma = 1;
	}

	if (fp->err != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("err=%d", fp->err->value);
	    comma = 1;
	}

	break;

    case EXEC_INQUIRE:
	g95_status("inquire(");
	i = c->ext.inquire;
	comma = 0;

	if (i->unit) {
	    if (comma)
		g95_status(", ");

	    g95_status("unit=");
	    g95_show_expr(i->unit);
	    comma = 1;
	}

	if (i->file) {
	    if (comma)
		g95_status(", ");

	    g95_status("file=");
	    g95_show_expr(i->file);
	    comma = 1;
	}

	if (i->iostat) {
	    if (comma)
		g95_status(", ");

	    g95_status("iostat=");
	    g95_show_expr(i->iostat);
	    comma = 1;
	}

	if (i->exist) {
	    if (comma)
		g95_status(", ");

	    g95_status("exist=");
	    g95_show_expr(i->exist);
	    comma = 1;
	}

	if (i->opened) {
	    if (comma)
		g95_status(", ");

	    g95_status("opened=");
	    g95_show_expr(i->opened);
	    comma = 1;
	}

	if (i->number) {
	    if (comma)
		g95_status(", ");

	    g95_status("number=");
	    g95_show_expr(i->number);
	    comma = 1;
	}

	if (i->named) {
	    if (comma)
		g95_status(", ");

	    g95_status("named=");
	    g95_show_expr(i->named);
	    comma = 1;
	}

	if (i->name) {
	    if (comma)
		g95_status(", ");

	    g95_status("name=");
	    g95_show_expr(i->name);
	    comma = 1;
	}

	if (i->access) {
	    if (comma)
		g95_status(", ");

	    g95_status("access=");
	    g95_show_expr(i->access);
	    comma = 1;
	}

	if (i->sequential) {
	    if (comma)
		g95_status(", ");

	    g95_status("sequential=");
	    g95_show_expr(i->sequential);
	    comma = 1;
	}

	if (i->direct) {
	    if (comma)
		g95_status(", ");

	    g95_status("direct=");
	    g95_show_expr(i->direct);
	    comma = 1;
	}

	if (i->form) {
	    if (comma)
		g95_status(", ");

	    g95_status("form=");
	    g95_show_expr(i->form);
	    comma = 1;
	}

	if (i->formatted) {
	    if (comma)
		g95_status(", ");

	    g95_status("formatted=");
	    g95_show_expr(i->formatted);
	    comma = 1;
	}

	if (i->unformatted) {
	    if (comma)
		g95_status(", ");

	    g95_status("unformatted=");
	    g95_show_expr(i->unformatted);
	    comma = 1;
	}

	if (i->recl) {
	    if (comma)
		g95_status(", ");
	    
	    g95_status("recl=");
	    g95_show_expr(i->recl);
	    comma = 1;
	}

	if (i->nextrec) {
	    if (comma)
		g95_status(", ");

	    g95_status("nextrec=");
	    g95_show_expr(i->nextrec);
	    comma = 1;
	}

	if (i->blank) {
	    if (comma)
		g95_status(", ");

	    g95_status("blank=");
	    g95_show_expr(i->blank);
	    comma = 1;
	}

	if (i->position) {
	    if (comma)
		g95_status(", ");

	    g95_status("position=");
	    g95_show_expr(i->position);
	    comma = 1;
	}

	if (i->action) {
	    if (comma)
		g95_status(", ");

	    g95_status("action=");
	    g95_show_expr(i->action);
	    comma = 1;
	}

	if (i->read) {
	    if (comma)
		g95_status(", ");

	    g95_status("read=");
	    g95_show_expr(i->read);
	    comma = 1;
	}

	if (i->write) {
	    if (comma)
		g95_status(", ");

	    g95_status("write=");
	    g95_show_expr(i->write);
	    comma = 1;
	}

	if (i->readwrite) {
	    if (comma)
		g95_status(", ");

	    g95_status("readwrite=");
	    g95_show_expr(i->readwrite);
	    comma = 1;
	}

	if (i->delim) {
	    if (comma)
		g95_status(", ");
	    
	    g95_status("delim=");
	    g95_show_expr(i->delim);
	    comma = 1;
	}

	if (i->stream != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("stream=");
	    g95_show_expr(i->stream);
	    comma = 1;
	}

	if (i->pad) {
	    if (comma)
		g95_status(", ");

	    g95_status("pad=");
	    g95_show_expr(i->pad);
	    comma = 1;
	}

	if (i->err != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("err=%d", i->err->value);
	    comma = 1;
	}

	break;

    case EXEC_IOLENGTH:
	g95_status("iolength(expr=");
	g95_show_expr(c->expr);
	comma = 0;
	break;

    case EXEC_READ:
	g95_status("read(expr=");
	goto show_dt;

    case EXEC_WRITE:
	g95_status("write(expr=");

    show_dt:
	dt = c->ext.dt;
	comma = 0;

	if (dt->io_unit) {
	    if (comma)
		g95_status(", ");

	    g95_status("unit=");
	    g95_show_expr(dt->io_unit);
	    comma = 1;
	}

	if (dt->format_expr) {
	    if (comma)
		g95_status(", ");

	    g95_status("format_expr=");
	    g95_show_expr(dt->format_expr);
	    comma = 1;
	}

	if (dt->format_label != NULL) {
	    if (comma)
		g95_status(", ");
	    
	    g95_status("format_label=%d", dt->format_label->value);
	    comma = 1;
	}

	if (dt->namelist != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("nml=%s", dt->namelist->name);
	    comma = 1;
	}

	if (dt->iostat != NULL) {
	    if (comma)
		g95_status(", ");
	    g95_status("iostat=");
	    g95_show_expr(dt->iostat);
	    comma = 1;
	}

	if (dt->size != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("size=");
	    g95_show_expr(dt->size);
	    comma = 1;
	}

	if (dt->rec != NULL) {
	    if (comma)
		g95_status(", ");

	    g95_status("rec=");
	    g95_show_expr(dt->rec);
	    comma = 1;
	}

	if (dt->advance) {
	    if (comma)
		g95_status(", ");

	    g95_status("advance=");
	    g95_show_expr(dt->advance);
	    comma = 1;
	}

	break;

  case EXEC_TRANSFER:
      g95_status("transfer(expr=");
      g95_show_expr(c->expr);
      comma = 1;
      break;

  case EXEC_DT_END:
      g95_status("dt_end(");
      comma = 0;

      dt = c->ext.dt;
      if (dt != NULL) {
	  if (dt->err != NULL) {
	      if (comma)
		  g95_status(", ");
	      
	      g95_status("err=%d", dt->err->value);
	      comma = 1;
	  }

	  if (dt->end != NULL) {
	      if (comma)
		  g95_status(", ");
	      g95_status("end=%d", dt->end->value);
	      comma = 1;
	  }

	  if (dt->eor != NULL) {
	      if (comma)
		  g95_status(", ");

	      g95_status("eor=%d", dt->eor->value);
	      comma = 1;
	  }
      }

      break;

    case EXEC_ENTRY:
	g95_status("entry(name='%s'", c->sym->name);
	comma = 1;
	break;

    case EXEC_LABEL_ASSIGN:
	g95_status("assign_label(expr=");
	g95_show_expr(c->expr);
	g95_status(", label=%d", c->label->value);
	comma = 1;
	break;

    case EXEC_ERROR_STOP:
	g95_status("ERROR STOP(");
	break;

    case EXEC_SYNC_ALL:
	g95_status("SYNC ALL(");
	break;

    case EXEC_SYNC_MEMORY:
	g95_status("SYNC MEMORY(");
	break;

    case EXEC_SYNC_IMAGES:
	g95_status("SYNC IMAGES(");

	if (c->expr == NULL)
	    g95_status_char('*');

	else
	    g95_show_expr(c->expr);

	break;

    case EXEC_SYNC_TEAM:
	g95_status("SYNC TEAM(");

	if (c->expr == NULL)
	    g95_status_char('*');

	else
	    g95_show_expr(c->expr);

	break;

    case EXEC_CRITICAL:
	g95_status("CRITICAL\n");
	show_code(level+1, c->block);

	code_indent(level, 0);
	g95_status("END CRITICAL");
	break;

    default:
	g95_internal_error("show_code0(): Bad statement code");
    }

    g95_status_char(')');
    g95_status_char('\n');
}



/* show_code()-- Show a list of code structures. */

void show_code(int level, g95_code *c) {

    for(;c ; c=c->next)
	show_code0(level, c);
}



/* show_array_spec()-- Show an array specification. */

static void show_array_spec(g95_array_spec *as) {
int i;

    g95_status("as('%s', %d",
	       g95_code2string(array_specs, as->type), as->rank);

    for(i=0; i<as->rank; i++) {
	g95_status(", lower%d=", i);
	g95_show_expr(as->lower[i]);

	g95_status(", upper%d=", i);
	g95_show_expr(as->upper[i]);
    }

    g95_status(")");
}



static void show_typespec(g95_typespec *ts) {

    switch(ts->type) {
    case BT_INTEGER:    g95_status("ts('INTEGER', kind=%d)", ts->kind); break;
    case BT_REAL:       g95_status("ts('REAL', kind=%d)",    ts->kind); break;
    case BT_COMPLEX:    g95_status("ts('COMPLEX', kind=%d",  ts->kind); break;
    case BT_LOGICAL:    g95_status("ts('LOGICAL', kind=%d",  ts->kind); break;
    case BT_PROCEDURE:  g95_status("ts('PROCEDURE')");  break;
    case BT_UNKNOWN:    g95_status("ts('UNKNOWN')");    break;
    case BT_DERIVED:    g95_status("ts('DERIVED', derived=%s",
				   g95_symbol_name(ts->derived)); break;

    case BT_CHARACTER:  g95_status("ts('CHARACTER', kind=%d, len=", ts->kind);
	if (ts->cl == &g95_unknown_charlen)
	    g95_status("None");
	else if (ts->cl == NULL)
	    g95_status("'*'");
	else
	    g95_show_expr(ts->cl->length);

	g95_status(")");
	break;

    default:
	g95_internal_error("show_typespec(): Undefined type");
    }
}



/* show_component()-- Show an expression component */

static void show_component(g95_component *c) {

    g95_status("comp(%s, ts=", c->name);
    show_typespec(&c->ts);

    if (c->pointer)
	g95_status(", pointer=1");

    if (c->dimension)
	g95_status(", dimension=1");

    if (c->allocatable)
	g95_status(", allocatable=1");

    if (c->as != NULL) {
	g95_status(", as=");
	show_array_spec(c->as);
    }

    g95_status(")");
}



/* show_indent()-- Print spaces to get the required indentation. */

static void show_indent(void) {
int i;

    g95_status_char('\n');

    for(i=0; i<2*show_level; i++)
	g95_status_char(' ');
}


/* show_symbol()-- Show a symbol.  If a symbol is an ENTRY,
 * SUBROUTINE or FUNCTION, we show the interface.  Information needed
 * to reconstruct the list of specific interfaces associated with a
 * generic symbol is done within that symbol. */

static void show_symbol(g95_symbol *sym) {
g95_formal_arglist *formal;
symbol_attribute *attr;
g95_component *c;

    if (sym == NULL)
	return;

    show_indent();

    g95_status("symbol(%s, ts=", g95_symbol_name(sym));
    show_typespec(&sym->ts);

    attr = &sym->attr;
    g95_status(", flavor='%s'", g95_flavor_string(attr->flavor));

    if (attr->intent != INTENT_UNKNOWN)
	g95_status(", intent='%s'", g95_intent_string(attr->intent));

    if (attr->access != ACCESS_UNKNOWN)
	g95_status(", access='%s'", g95_access_string(attr->access));

    if (attr->proc != PROC_UNKNOWN)
	g95_status(", proc='%s'", g95_procedure_string(attr->proc));

    if (attr->allocatable)
	g95_status(", allocatable=1");

    if (attr->dimension)
	g95_status(", dimension=1");

    if (attr->external)
	g95_status(", external=1");

    if (attr->intrinsic)
	g95_status(", intrinsic=1");

    if (attr->optional)
	g95_status(", optional=1");

    if (attr->pointer)
	g95_status(", pointer=1");

    if (attr->save)
	g95_status(", save=1");

    if (attr->target)
	g95_status(", target=1");

    if (attr->dummy)
	g95_status(", dummy=1");

    if (attr->result_var)
	g95_status(", result=1");

    if (attr->entry)
	g95_status(", entry=1");

    if (attr->data)
	g95_status(", data=1");

    if (attr->use_assoc)
	g95_status(", use_assoc=1");

    if (attr->in_namelist)
	g95_status(", in_namelist=1");

    if (attr->in_common)
	g95_status(", in_common=1");

    if (attr->function)
	g95_status(", function=1");

    if (attr->subroutine)
	g95_status(", subroutine=1");

    if (attr->sequence)
	g95_status(", sequence=1");

    if (attr->elemental)
	g95_status(", elemental=1");

    if (attr->pure)
	g95_status(", pure=1");

    if (attr->recursive)
	g95_status(", recursive=1");

    if (attr->artificial)
	g95_status(", artificial=1");

    if (sym->value) {
	g95_status(", value=");
	g95_show_expr(sym->value);
    }

    if (sym->as != NULL) {
	g95_status(", as=");
	show_array_spec(sym->as);
    }

    if (sym->result)
	g95_status(", result=%s", g95_symbol_name(sym->result));

    if (sym->components) {
	g95_status(", components=[");
	for(c=sym->components; c; c=c->next)
	    show_component(c);

	g95_status("]");
    }

    if (sym->formal) {
	show_indent();
	g95_status("formal=[");

	for(formal=sym->formal; formal; formal=formal->next)
	    if (formal->sym == NULL)
		g95_status("*");
	    else
		g95_status("'%s', ", formal->sym->name);

	g95_status("]");
    }

    g95_status(")\n");
}



static void show_uop(g95_user_op *uop) {
g95_interface *intr;

    g95_status("user_op('%s', [", uop->name);

    for(intr=uop->operator; intr; intr=intr->next)
	g95_status(" '%s',", intr->sym->name);

    g95_status("])\n");
}



/* show_symtree()-- Worker function to display the symbol tree */

static void show_symtree(g95_symtree *st) {

    show_indent();
    g95_status("symtree('%s', symbol=%s", st->name,
	       g95_symbol_name(st->n.sym));

    if (st->ambiguous)
	g95_status(", ambiguous=1");

    g95_status(")");

    if (st->n.sym->ns != g95_current_ns)
	g95_status("  # from namespace %s\n", st->n.sym->ns->proc_name->name);

    else {
	g95_status_char('\n');
	show_symbol(st->n.sym);
    }
}



/* g95_show_namespace()-- Show a namespace */

void g95_show_namespace(g95_namespace *ns) {
g95_interface *intr;
g95_namespace *save;
int i;

    if (ns == NULL)
	return;

    save = g95_current_ns; 

    show_indent();
    g95_status("ns(");

    for(i=0; i<G95_LETTERS; i++) {
	g95_status("%c=", i+'A');
	show_typespec(&ns->default_type[i]);

	if (i != G95_LETTERS-1)
	    g95_status_char(',');

	g95_status_char((i % 3 == 2) ? '\n' : ' ');
    }

    if (ns->proc_name != NULL)
	g95_status(", name='%s'", ns->proc_name->name);

    g95_status(")\n");

    for(i=0; i<G95_INTRINSIC_OPS; i++) {    /* User operator interfaces */
	intr = ns->operator[i];
	if (intr == NULL)
	    continue;

	g95_status("operator('%s', [", g95_op2string(i));

	for(; intr; intr=intr->next)
	    g95_status(" '%s',", intr->sym->name);

	g95_status("])\n");
    }

    g95_traverse_user_op(ns, show_uop);

    g95_traverse_symtree(ns, g95_clear_sym_mark);

    g95_current_ns = ns;
    g95_traverse_symtree(ns, show_symtree);
    g95_status("\n\n");

    show_code(0, ns->code);
    show_level++; 

    for(ns=ns->contained; ns; ns=ns->sibling) {
	g95_status("# Contains\n");
	g95_show_namespace(ns);
    }

    show_level--;
    g95_status_char('\n');
    g95_current_ns = save;
}

