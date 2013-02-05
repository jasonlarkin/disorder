G95 module created on Fri Feb  1 21:14:45 2013 from m_common_struct.F90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () () () () () () () () () () () ())

()

()

()

()

(2 'destroy_xml_doc_state' 'm_common_struct' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (3 NONE) () () ''
() ())
4 'init_xml_doc_state' 'm_common_struct' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (5 NONE) () () ''
() ())
6 'register_external_ge' 'm_common_struct' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (7 NONE 8 NONE
9 NONE 10 NONE 11 NONE 12 NONE 13 NONE) () () '' () ())
14 'register_external_pe' 'm_common_struct' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (15 NONE 16
NONE 17 NONE 18 NONE 19 NONE 20 NONE) () () '' () ())
21 'register_internal_ge' 'm_common_struct' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (22 NONE 23
NONE 24 NONE 25 NONE 26 NONE) () () '' () ())
27 'register_internal_pe' 'm_common_struct' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (28 NONE 29
NONE 30 NONE 31 NONE 32 NONE) () () '' () ())
33 'xml_doc_state' 'm_common_struct' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((34 'building' (LOGICAL 4) () () 0
0 0 (CONSTANT (LOGICAL 4) 0 0)) (35 'xml_version' (INTEGER 4) () () 0 0
0 (CONSTANT (INTEGER 4) 0 '10')) (36 'standalone_declared' (LOGICAL 4) ()
() 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (37 'standalone' (LOGICAL 4) () ()
0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (38 'entitylist' (DERIVED 39) () () 0
0 0 ()) (40 'pelist' (DERIVED 39) () () 0 0 0 ()) (41 'nlist' (DERIVED
42) () () 0 0 0 ()) (43 'element_list' (DERIVED 44) () () 0 0 0 ()) (45
'warning' (LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (46 'valid'
(LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (47 'livenodelists'
(LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (48 'encoding' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (49 'inputencoding'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (50 'documenturi'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (51 'intsubset' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
44 'element_list' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((52 'list' (DERIVED 53) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 53) 1))) PUBLIC ())
53 'element_t' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((54 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (55 'empty' (LOGICAL 4)
() () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (56 'any' (LOGICAL 4) () () 0 0
0 (CONSTANT (LOGICAL 4) 0 0)) (57 'mixed' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 0)) (58 'id_declared' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 0)) (59 'internal' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 1)) (60 'cp' (DERIVED 61) () () 0 1 0 (NULL (
DERIVED 61) 0)) (62 'model' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0
'1'))) 1)) (63 'attlist' (DERIVED 64) () () 0 0 0 ())) PUBLIC ())
64 'attribute_list' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((65 'list' (DERIVED 66) (
1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 66) 1))) PUBLIC ())
66 'attribute_t' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((67 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (68 'atttype' (INTEGER 4)
() () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (69 'attdefault' (INTEGER 4) ()
() 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (70 'enumerations' (DERIVED 71) ()
() 0 0 0 ()) (72 'default' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0
'1'))) 1)) (73 'internal' (LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4)
0 1))) PUBLIC ())
71 'string_list' 'fox_m_fsys_string_list' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((74 'list' (DERIVED 75) (
1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 75) 1))) PUBLIC ())
75 'string_t' 'fox_m_fsys_string_list' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((76 's' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
61 'content_particle_t' 'm_common_content_model' 1 ((DERIVED UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((77 'name' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (78 'operator' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (79 'repeater' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (80 'nextsibling' (
DERIVED 61) () () 0 1 0 (NULL (DERIVED 61) 0)) (81 'parent' (DERIVED 61)
() () 0 1 0 (NULL (DERIVED 61) 0)) (82 'firstchild' (DERIVED 61) () () 0
1 0 (NULL (DERIVED 61) 0))) PUBLIC ())
42 'notation_list' 'm_common_notations' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((83 'list' (DERIVED 84) (
1 DEFERRED () ()) () 1 1 0 ())) PUBLIC ())
84 'notation' 'm_common_notations' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((85 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 ()) (86
'systemid' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ())
() 1 1 0 ()) (87 'publicid' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 ())) PUBLIC ())
39 'entity_list' 'm_common_entities' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((88 'list' (DERIVED 89) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 89) 1))) PRIVATE ())
89 'entity_t' 'm_common_entities' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((90 'external' (LOGICAL 4) () () 0
0 0 ()) (91 'wfc' (LOGICAL 4) () () 0 0 0 ()) (92 'name' (CHARACTER 1 (
(CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (93 'text' (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (94 'publicid' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (95 'systemid' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (96 'notation' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (97 'baseuri' (
DERIVED 98) () () 0 1 0 (NULL (DERIVED 98) 0))) PUBLIC ())
98 'uri' 'fox_m_utils_uri' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' ((99 'scheme' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1)) (100 'authority' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (101 'userinfo' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (102 'host' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (103 'port' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '-1')) (104 'path' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (105 'segments' (
DERIVED 106) (1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 106) 1)) (107
'query' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ())
() 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (108
'fragment' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ())
() 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PRIVATE
())
106 'path_segment' 'fox_m_utils_uri' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((109 's' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
32 'baseuri' '' 110 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE NONE POINTER
DUMMY) (DERIVED 98) 0 0 () () () '' () ())
31 'wfc' '' 110 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (LOGICAL
4) 0 0 () () () '' () ())
30 'text' '' 110 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
29 'name' '' 110 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
28 'xds' '' 110 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 33) 0 0 () () () '' () ())
26 'baseuri' '' 111 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE NONE POINTER
DUMMY) (DERIVED 98) 0 0 () () () '' () ())
25 'wfc' '' 111 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (LOGICAL
4) 0 0 () () () '' () ())
24 'text' '' 111 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
23 'name' '' 111 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
22 'xds' '' 111 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 33) 0 0 () () () '' () ())
20 'publicid' '' 112 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (CHARACTER 1 (())) 0 0 () () () '' () ())
19 'baseuri' '' 112 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE NONE POINTER
DUMMY) (DERIVED 98) 0 0 () () () '' () ())
18 'wfc' '' 112 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (LOGICAL
4) 0 0 () () () '' () ())
17 'systemid' '' 112 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
16 'name' '' 112 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
15 'xds' '' 112 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 33) 0 0 () () () '' () ())
13 'notation' '' 113 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (CHARACTER 1 (())) 0 0 () () () '' () ())
12 'publicid' '' 113 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (CHARACTER 1 (())) 0 0 () () () '' () ())
11 'baseuri' '' 113 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE NONE POINTER
DUMMY) (DERIVED 98) 0 0 () () () '' () ())
10 'wfc' '' 113 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (LOGICAL
4) 0 0 () () () '' () ())
9 'systemid' '' 113 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
8 'name' '' 113 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
7 'xds' '' 113 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 33) 0 0 () () () '' () ())
5 'xds' '' 114 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 33) 0 0 () () () '' () ())
3 'xds' '' 115 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 33) 0 0 () () () '' () ())
)

('destroy_xml_doc_state' 0 2 'init_xml_doc_state' 0 4
'register_external_ge' 0 6 'register_external_pe' 0 14
'register_internal_ge' 0 21 'register_internal_pe' 0 27 'xml_doc_state'
0 33)
