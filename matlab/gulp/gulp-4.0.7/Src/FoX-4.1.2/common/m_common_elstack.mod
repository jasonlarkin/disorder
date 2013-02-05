G95 module created on Fri Feb  1 21:14:45 2013 from m_common_elstack.F90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () () () () () () () () () () () ())

()

(('is_empty' 2) ('len' 3))

()

()

(4 'checkcontentmodel' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 (5 NONE 6 NONE) ()
() '' () ())
7 'checkcontentmodeltoend' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 (8 NONE) () () '' ()
())
9 'destroy_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (10 NONE) () () ''
() ())
11 'elementcontent' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 (12 NONE) () () '' () ())
13 'elstack_t' 'm_common_elstack' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((14 'n_items' (INTEGER 4) () () 0
0 0 ()) (15 'stack' (DERIVED 16) (1 DEFERRED () ()) () 1 1 0 (NULL (
DERIVED 16) 1))) PRIVATE ())
17 'emptycontent' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 (18 NONE) () () '' () ())
19 'get_top_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION PURE) (CHARACTER 1 ('UNK')) 0 0 (20
NONE) () () '' () ())
21 'init_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (22 NONE) () () ''
() ())
23 'is_empty' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' () ())
24 'len' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE NONE) (
UNKNOWN) 0 0 () () () '' () ())
25 'pop_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION) (CHARACTER 1 ('UNK')) 0 0 (26 NONE) () () '' ()
())
27 'print_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (28 NONE 29 NONE) () () ''
() ())
30 'push_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (31 NONE 32 NONE 33 NONE) ()
() '' () ())
34 'reset_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (35 NONE) () () '' () ())
35 'elstack' '' 36 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
33 'cp' '' 37 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE NONE OPTIONAL
POINTER DUMMY) (DERIVED 38) 0 0 () () () '' () ())
38 'content_particle_t' 'm_common_content_model' 1 ((DERIVED UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((39 'name' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (40 'operator' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (41 'repeater' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (42 'nextsibling' (
DERIVED 38) () () 0 1 0 (NULL (DERIVED 38) 0)) (43 'parent' (DERIVED 38)
() () 0 1 0 (NULL (DERIVED 38) 0)) (44 'firstchild' (DERIVED 38) () () 0
1 0 (NULL (DERIVED 38) 0))) PUBLIC ())
32 'name' '' 37 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
31 'elstack' '' 37 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
29 'unit' '' 45 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER
4) 0 0 () () () '' () ())
28 'elstack' '' 45 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
26 'elstack' '' 46 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
22 'elstack' '' 47 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
20 'elstack' '' 48 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
18 'elstack' '' 49 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
16 'elstack_item' 'm_common_elstack' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((50 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (51 'cp' (DERIVED 38) ()
() 0 1 0 (NULL (DERIVED 38) 0))) PUBLIC ())
12 'elstack' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
10 'elstack' '' 53 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
8 'elstack' '' 54 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
6 'name' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
5 'elstack' '' 55 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
3 'number_of_items' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION INVOKED) (INTEGER 4) 0 0 (56 NONE) () () '' () ())
56 'elstack' '' 57 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
2 'is_empty_elstack' 'm_common_elstack' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION PURE INVOKED) (LOGICAL 4) 0 0 (58
NONE) () () '' () ())
58 'elstack' '' 59 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 13) 0 0 () () () '' () ())
)

('checkcontentmodel' 0 4 'checkcontentmodeltoend' 0 7 'destroy_elstack'
0 9 'elementcontent' 0 11 'elstack_t' 0 13 'emptycontent' 0 17
'get_top_elstack' 0 19 'init_elstack' 0 21 'is_empty' 0 23 'len' 0 24
'pop_elstack' 0 25 'print_elstack' 0 27 'push_elstack' 0 30
'reset_elstack' 0 34)
