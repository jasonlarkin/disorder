
/* Copyright (C) 2000-2008 Free Software Foundation, Inc.
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

/* bbt.c -- Implement a balanced binary trees using treaps.  The idea
 * is to balance the tree using pseudorandom numbers.  The main
 * constraint on this implementation is that we have several distinct
 * structures that have to be arranged in a binary tree.  These
 * structures all contain a BBT_HEADER() in front that gives the
 * treap-related information.  The key and value are assumed to reside
 * in the rest of the structure.
 *
 * When calling, we are also passed a comparison function that
 * compares two nodes.  We don't implement a separate 'find' function
 * here, but rather use separate functions for each variety of tree.
 * We are also restricted to not copy treap structures, which most
 * implementations find convenient, because we otherwise would need to
 * know how long the structure is.
 *
 * This implementation is based on Stefan Nilsson's article in the
 * July 1997 Doctor Dobb's Journal, "Treaps in Java". */

#include "g95.h"


typedef struct g95_treap {
    BBT_HEADER(g95_treap)
} g95_bbt;



/* pseudo_random()-- Simple linear congruential pseudorandom number
 * generator.  The period of this generator is 44071, which is plenty
 * for our purposes.  */

static int pseudo_random(void) {
static int x0=5341;

    x0 = (22611*x0 + 10) % 44071;
    return x0;
}



/* rotate_right()-- Rotate the treap right */

static g95_bbt *rotate_right(g95_bbt *t) {
g95_bbt *temp;

    temp = t->left;
    t->left = t->left->right;
    temp->right = t;

    return temp;
}



/* rotate_left()-- Rotate the treap left */

static g95_bbt *rotate_left(g95_bbt *t) {
g95_bbt *temp;

    temp =  t->right;
    t->right = t->right->left;
    temp->left = t;

    return temp;
}



static g95_bbt *delete_root(g95_bbt *t) {
g95_bbt *temp;

    if (t->left == NULL)
	return t->right;

    if (t->right == NULL)
	return t->left;

    if (t->left->priority > t->right->priority) {
	temp = rotate_right(t);
	temp->right = delete_root(t);

    } else {
	temp = rotate_left(t);
	temp->left = delete_root(t);
    }

    return temp;
}



/* delete_treap()-- Delete an element from a tree.  The 'old' value
 * does not necessarily have to point to the element to be deleted, it
 * must just point to a treap structure with the key to be deleted.
 * Returns the new root node of the tree. */

static g95_bbt *delete_treap(g95_bbt *old, g95_bbt *t, int (*compare)()) {
int c;

    if (t == NULL)
	return NULL; 

    c = compare(old, t);

    if (c == 0)
	t = delete_root(t);

    else if (c < 0)
	t->left = delete_treap(old, t->left, compare);

    else if (c > 0)
	t->right = delete_treap(old, t->right, compare);

    return t;
}



void g95_delete_bbt(void *root, void *old, int (*compare)()) {
g95_bbt **t;

    t = (g95_bbt **) root; 

    *t = delete_treap((g95_bbt *) old, *t, compare);
}



/* insert()-- Recursive insertion function.  Returns the updated treap. */

static g95_bbt *insert(g95_bbt *new, g95_bbt *t, int (*compare)()) {
int c;

    if (t == NULL)
	return new;

    c = (*compare)(new, t);

    if (c == 0)
	g95_internal_error("insert(): Duplicate key found!");

    if (c < 0) {
	t->left = insert(new, t->left, compare);
	if (t->priority < t->left->priority)
	    t = rotate_right(t);

    } else {
	t->right = insert(new, t->right, compare);
	if (t->priority < t->right->priority)
	    t = rotate_left(t);
    }


  return t;
}



/* g95_insert_bbt()-- Given root pointer, a new node and a
 * comparison function, insert the new node into the treap.  It is an
 * error to insert a key that already exists. */

void g95_insert_bbt(void *root, void *new, int (*compare)()) {
g95_bbt **r, *n;

    r = (g95_bbt **) root; 
    n = (g95_bbt *) new;

    n->priority = pseudo_random();
    *r = insert(n, *r, compare);
}

