G95 module created on Sat Oct 13 11:37:07 2012 from m_common_entity_expand.F90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () () () () () () () () () () () ())

()

()

()

()

(2 'expand_entity_value_alloc' 'm_common_entity_expand' 1 ((PROCEDURE
UNKNOWN MODULE-PROC DECL NONE NONE DIMENSION POINTER FUNCTION) (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 0 0 (3 NONE 4 NONE 5 NONE) (
1 DEFERRED () ()) () '' () ())
5 'stack' '' 6 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 7) 0 0 () () () '' () ())
7 'error_stack' 'm_common_error' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((8 'stack' (DERIVED 9) (1 DEFERRED
() ()) () 1 1 0 (NULL (DERIVED 9) 1))) PUBLIC ())
9 'error_t' 'm_common_error' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' ((10 'severity' (INTEGER 4) () () 0 0 0
(CONSTANT (INTEGER 4) 0 '0')) (11 'error_code' (INTEGER 4) () () 0 0 0 (
CONSTANT (INTEGER 4) 0 '0')) (12 'msg' (CHARACTER 1 ((CONSTANT (INTEGER
4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) 1))) PUBLIC ())
4 'xds' '' 6 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (DERIVED 13)
0 0 () () () '' () ())
13 'xml_doc_state' 'm_common_struct' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((14 'building' (LOGICAL 4) () () 0
0 0 (CONSTANT (LOGICAL 4) 0 0)) (15 'xml_version' (INTEGER 4) () () 0 0
0 (CONSTANT (INTEGER 4) 0 '10')) (16 'standalone_declared' (LOGICAL 4) ()
() 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (17 'standalone' (LOGICAL 4) () ()
0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (18 'entitylist' (DERIVED 19) () () 0
0 0 ()) (20 'pelist' (DERIVED 19) () () 0 0 0 ()) (21 'nlist' (DERIVED
22) () () 0 0 0 ()) (23 'element_list' (DERIVED 24) () () 0 0 0 ()) (25
'warning' (LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (26 'valid'
(LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (27 'livenodelists'
(LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (28 'encoding' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (29 'inputencoding'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (30 'documenturi'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (31 'intsubset' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
24 'element_list' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((32 'list' (DERIVED 33) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 33) 1))) PUBLIC ())
33 'element_t' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((34 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (35 'empty' (LOGICAL 4)
() () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (36 'any' (LOGICAL 4) () () 0 0
0 (CONSTANT (LOGICAL 4) 0 0)) (37 'mixed' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 0)) (38 'id_declared' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 0)) (39 'internal' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 1)) (40 'cp' (DERIVED 41) () () 0 1 0 (NULL (
DERIVED 41) 0)) (42 'model' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0
'1'))) 1)) (43 'attlist' (DERIVED 44) () () 0 0 0 ())) PUBLIC ())
44 'attribute_list' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((45 'list' (DERIVED 46) (
1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 46) 1))) PUBLIC ())
46 'attribute_t' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((47 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (48 'atttype' (INTEGER 4)
() () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (49 'attdefault' (INTEGER 4) ()
() 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (50 'enumerations' (DERIVED 51) ()
() 0 0 0 ()) (52 'default' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0
'1'))) 1)) (53 'internal' (LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4)
0 1))) PUBLIC ())
51 'string_list' 'fox_m_fsys_string_list' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((54 'list' (DERIVED 55) (
1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 55) 1))) PUBLIC ())
55 'string_t' 'fox_m_fsys_string_list' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((56 's' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
41 'content_particle_t' 'm_common_content_model' 1 ((DERIVED UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((57 'name' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (58 'operator' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (59 'repeater' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (60 'nextsibling' (
DERIVED 41) () () 0 1 0 (NULL (DERIVED 41) 0)) (61 'parent' (DERIVED 41)
() () 0 1 0 (NULL (DERIVED 41) 0)) (62 'firstchild' (DERIVED 41) () () 0
1 0 (NULL (DERIVED 41) 0))) PUBLIC ())
22 'notation_list' 'm_common_notations' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((63 'list' (DERIVED 64) (
1 DEFERRED () ()) () 1 1 0 ())) PUBLIC ())
64 'notation' 'm_common_notations' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((65 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 ()) (66
'systemid' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ())
() 1 1 0 ()) (67 'publicid' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 ())) PUBLIC ())
19 'entity_list' 'm_common_entities' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((68 'list' (DERIVED 69) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 69) 1))) PRIVATE ())
69 'entity_t' 'm_common_entities' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((70 'external' (LOGICAL 4) () () 0
0 0 ()) (71 'wfc' (LOGICAL 4) () () 0 0 0 ()) (72 'name' (CHARACTER 1 (
(CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (73 'text' (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (74 'publicid' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (75 'systemid' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (76 'notation' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (77 'baseuri' (
DERIVED 78) () () 0 1 0 (NULL (DERIVED 78) 0))) PUBLIC ())
78 'uri' 'fox_m_utils_uri' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' ((79 'scheme' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1)) (80 'authority' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (81 'userinfo' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (82 'host' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (83 'port' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '-1')) (84 'path' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (85 'segments' (
DERIVED 86) (1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 86) 1)) (87 'query'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (88 'fragment' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PRIVATE ())
86 'path_segment' 'fox_m_utils_uri' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((89 's' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
3 'repl' '' 6 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 0 0 () (1 ASSUMED_SHAPE () ())
() '' () ())
)

('expand_entity_value_alloc' 0 2)
