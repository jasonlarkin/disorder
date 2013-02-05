G95 module created on Fri Feb  1 21:14:43 2013 from m_common_error.F90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () ()
() () () () () () () () () ())

()

(('fox_error' 2) ('fox_fatal' 3) ('fox_warning' 4))

()

()

(5 'add_error' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (6 NONE 7 NONE 8 NONE 9 NONE) ()
() '' () ())
10 'destroy_error_stack' 'm_common_error' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (11 NONE) () ()
'' () ())
12 'err_error' 'm_common_error' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '2') () () '' () ())
13 'err_fatal' 'm_common_error' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '3') () () '' () ())
14 'err_null' 'm_common_error' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '0') () () '' () ())
15 'err_warning' 'm_common_error' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '1') () () '' () ())
16 'error_stack' 'm_common_error' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((17 'stack' (DERIVED 18) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 18) 1))) PUBLIC ())
18 'error_t' 'm_common_error' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' ((19 'severity' (INTEGER 4) () () 0 0 0
(CONSTANT (INTEGER 4) 0 '0')) (20 'error_code' (INTEGER 4) () () 0 0 0 (
CONSTANT (INTEGER 4) 0 '0')) (21 'msg' (CHARACTER 1 ((CONSTANT (INTEGER
4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) 1))) PUBLIC ())
2 'fox_error_base' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (22 NONE) () () ''
() ())
3 'fox_fatal_base' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (23 NONE) () () ''
() ())
24 'fox_get_fatal_errors' 'm_common_error' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 () () () '' () ())
25 'fox_get_fatal_warnings' 'm_common_error' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 () () () '' () ())
26 'fox_set_fatal_errors' 'm_common_error' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (27 NONE) () ()
'' () ())
28 'fox_set_fatal_warnings' 'm_common_error' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (29 NONE) () ()
'' () ())
4 'fox_warning_base' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (30 NONE) () () ''
() ())
31 'in_error' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE FUNCTION) (LOGICAL 4) 0 0 (32 NONE) () () '' () ())
33 'init_error_stack' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (34 NONE) () () ''
() ())
34 'stack' '' 35 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 16) 0 0 () () () '' () ())
32 'stack' '' 36 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 16) 0 0 () () () '' () ())
30 'msg' '' 37 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
29 'newvalue' '' 38 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
LOGICAL 4) 0 0 () () () '' () ())
27 'newvalue' '' 39 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
LOGICAL 4) 0 0 () () () '' () ())
23 'msg' '' 40 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
22 'msg' '' 41 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
18 'error_t' 'm_common_error' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' ((19 'severity' (INTEGER 4) () () 0 0 0
(CONSTANT (INTEGER 4) 0 '0')) (20 'error_code' (INTEGER 4) () () 0 0 0 (
CONSTANT (INTEGER 4) 0 '0')) (21 'msg' (CHARACTER 1 ((CONSTANT (INTEGER
4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) 1))) PUBLIC ())
11 'stack' '' 42 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 16) 0 0 () () () '' () ())
9 'error_code' '' 43 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (INTEGER 4) 0 0 () () () '' () ())
8 'severity' '' 43 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (INTEGER 4) 0 0 () () () '' () ())
7 'msg' '' 43 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER
1 (())) 0 0 () () () '' () ())
6 'stack' '' 43 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 16) 0 0 () () () '' () ())
4 'fox_warning_base' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (30 NONE) () () ''
() ())
3 'fox_fatal_base' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (23 NONE) () () ''
() ())
2 'fox_error_base' 'm_common_error' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (22 NONE) () () ''
() ())
)

('add_error' 0 5 'destroy_error_stack' 0 10 'err_error' 0 12 'err_fatal'
0 13 'err_null' 0 14 'err_warning' 0 15 'error_stack' 0 16 'error_t' 0
18 'fox_error_base' 0 2 'fox_fatal_base' 0 3 'fox_get_fatal_errors' 0 24
'fox_get_fatal_warnings' 0 25 'fox_set_fatal_errors' 0 26
'fox_set_fatal_warnings' 0 28 'fox_warning_base' 0 4 'in_error' 0 31
'init_error_stack' 0 33)
