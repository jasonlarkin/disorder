G95 module created on Fri Feb  1 21:14:48 2013 from m_wcml_core.F90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () () () () () () () () () () () ())

()

()

()

()

(2 'cmladdnamespace' 'm_wcml_core' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (3 NONE 4 NONE 5 NONE) () ()
'' () ())
6 'cmlbeginfile' 'm_wcml_core' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (7 NONE 8 NONE 9 NONE 10 NONE) ()
() '' () ())
11 'cmlendcml' 'm_wcml_core' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL NONE
NONE SUBROUTINE) (PROCEDURE 0) 0 0 (12 NONE) () () '' () ())
13 'cmlfinishfile' 'm_wcml_core' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (14 NONE) () () '' () ())
15 'cmlstartcml' 'm_wcml_core' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (16 NONE 17 NONE 18 NONE 19 NONE
20 NONE 21 NONE 22 NONE) () () '' () ())
22 'version' '' 23 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (CHARACTER 1 (())) 0 0 () () () '' () ())
21 'fileid' '' 23 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL DUMMY)
(CHARACTER 1 (())) 0 0 () () () '' () ())
20 'dictref' '' 23 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (CHARACTER 1 (())) 0 0 () () () '' () ())
19 'convention' '' 23 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (CHARACTER 1 (())) 0 0 () () () '' () ())
18 'title' '' 23 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL DUMMY)
(CHARACTER 1 (())) 0 0 () () () '' () ())
17 'id' '' 23 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
16 'xf' '' 23 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 24) 0 0 () () () '' () ())
24 'xmlf_t' 'm_wxml_core' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' ((25 'xds' (DERIVED 26) () () 0 0 0 ()) (27
'lun' (INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '-1')) (28 'buffer'
(DERIVED 29) () () 0 0 0 ()) (30 'stack' (DERIVED 31) () () 0 0 0 ()) (
32 'dict' (DERIVED 33) () () 0 0 0 ()) (34 'state_1' (INTEGER 4) () () 0
0 0 (CONSTANT (INTEGER 4) 0 '-1')) (35 'state_2' (INTEGER 4) () () 0 0 0
(CONSTANT (INTEGER 4) 0 '-1')) (36 'state_3' (INTEGER 4) () () 0 0 0 (
CONSTANT (INTEGER 4) 0 '-1')) (37 'minimize_overrun' (LOGICAL 4) () () 0
0 0 (CONSTANT (LOGICAL 4) 0 1)) (38 'pretty_print' (LOGICAL 4) () () 0 0
0 (CONSTANT (LOGICAL 4) 0 0)) (39 'canonical' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 0)) (40 'indent' (INTEGER 4) () () 0 0 0 (
CONSTANT (INTEGER 4) 0 '0')) (41 'name' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 ()) (42 'namespace' (
LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (43 'nsdict' (
DERIVED 44) () () 0 0 0 ())) PRIVATE ())
44 'namespacedictionary' 'm_common_namespaces' 1 ((DERIVED UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((45 'defaults' (
DERIVED 46) (1 DEFERRED () ()) () 1 1 0 ()) (47 'prefixes' (DERIVED 48)
(1 DEFERRED () ()) () 1 1 0 ())) PRIVATE ())
48 'prefixmapping' 'm_common_namespaces' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((49 'prefix' (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 ()) (50
'urilist' (DERIVED 46) (1 DEFERRED () ()) () 1 1 0 ())) PUBLIC ())
46 'urimapping' 'm_common_namespaces' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((51 'uri' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 ()) (52 'ix' (
INTEGER 4) () () 0 0 0 ())) PUBLIC ())
33 'dictionary_t' 'm_common_attrs' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((53 'list' (DERIVED 54) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 54) 1)) (55 'base' (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PRIVATE ())
54 'dict_item_ptr' 'm_common_attrs' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((56 'd' (DERIVED 57) () () 0 1 0 (
NULL (DERIVED 57) 0))) PUBLIC ())
57 'dict_item' 'm_common_attrs' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' ((58 'nsuri' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1)) (59 'localname' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (60 'prefix' (CHARACTER
1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (61 'key' (CHARACTER 1 (
(CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (62 'value' (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (63 'specified' (
LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (64 'declared' (
LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (65 'isid' (LOGICAL 4)
() () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (66 'type' (INTEGER 4) () () 0 0
0 (CONSTANT (INTEGER 4) 0 '11'))) PUBLIC ())
31 'elstack_t' 'm_common_elstack' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((67 'n_items' (INTEGER 4) () () 0
0 0 ()) (68 'stack' (DERIVED 69) (1 DEFERRED () ()) () 1 1 0 (NULL (
DERIVED 69) 1))) PRIVATE ())
69 'elstack_item' 'm_common_elstack' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((70 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (71 'cp' (DERIVED 72) ()
() 0 1 0 (NULL (DERIVED 72) 0))) PUBLIC ())
72 'content_particle_t' 'm_common_content_model' 1 ((DERIVED UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((73 'name' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (74 'operator' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (75 'repeater' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (76 'nextsibling' (
DERIVED 72) () () 0 1 0 (NULL (DERIVED 72) 0)) (77 'parent' (DERIVED 72)
() () 0 1 0 (NULL (DERIVED 72) 0)) (78 'firstchild' (DERIVED 72) () () 0
1 0 (NULL (DERIVED 72) 0))) PUBLIC ())
29 'buffer_t' 'm_common_buffer' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' ((79 'size' (INTEGER 4) () () 0 0 0 ())
(80 'str' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1024'))) () () 0 0 0 ())
(81 'unit' (INTEGER 4) () () 0 0 0 ()) (82 'xml_version' (INTEGER 4) ()
() 0 0 0 ())) PRIVATE ())
26 'xml_doc_state' 'm_common_struct' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((83 'building' (LOGICAL 4) () () 0
0 0 (CONSTANT (LOGICAL 4) 0 0)) (84 'xml_version' (INTEGER 4) () () 0 0
0 (CONSTANT (INTEGER 4) 0 '10')) (85 'standalone_declared' (LOGICAL 4) ()
() 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (86 'standalone' (LOGICAL 4) () ()
0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (87 'entitylist' (DERIVED 88) () () 0
0 0 ()) (89 'pelist' (DERIVED 88) () () 0 0 0 ()) (90 'nlist' (DERIVED
91) () () 0 0 0 ()) (92 'element_list' (DERIVED 93) () () 0 0 0 ()) (94
'warning' (LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (95 'valid'
(LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (96 'livenodelists'
(LOGICAL 4) () () 0 0 0 (CONSTANT (LOGICAL 4) 0 1)) (97 'encoding' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (98 'inputencoding'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (99 'documenturi'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (100 'intsubset'
(CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
93 'element_list' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((101 'list' (DERIVED 102) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 102) 1))) PUBLIC ())
102 'element_t' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((103 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (104 'empty' (LOGICAL 4)
() () 0 0 0 (CONSTANT (LOGICAL 4) 0 0)) (105 'any' (LOGICAL 4) () () 0 0
0 (CONSTANT (LOGICAL 4) 0 0)) (106 'mixed' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 0)) (107 'id_declared' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 0)) (108 'internal' (LOGICAL 4) () () 0 0 0 (
CONSTANT (LOGICAL 4) 0 1)) (109 'cp' (DERIVED 72) () () 0 1 0 (NULL (
DERIVED 72) 0)) (110 'model' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0
'1'))) 1)) (111 'attlist' (DERIVED 112) () () 0 0 0 ())) PUBLIC ())
112 'attribute_list' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((113 'list' (DERIVED 114)
(1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 114) 1))) PUBLIC ())
114 'attribute_t' 'm_common_element' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((115 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (116 'atttype' (INTEGER
4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (117 'attdefault' (INTEGER
4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '0')) (118 'enumerations' (
DERIVED 119) () () 0 0 0 ()) (120 'default' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1)) (121 'internal' (LOGICAL 4) () () 0 0
0 (CONSTANT (LOGICAL 4) 0 1))) PUBLIC ())
119 'string_list' 'fox_m_fsys_string_list' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((122 'list' (DERIVED 123)
(1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 123) 1))) PUBLIC ())
123 'string_t' 'fox_m_fsys_string_list' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((124 's' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
91 'notation_list' 'm_common_notations' 1 ((DERIVED UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (UNKNOWN) 0 0 () () () '' ((125 'list' (DERIVED 126)
(1 DEFERRED () ()) () 1 1 0 ())) PUBLIC ())
126 'notation' 'm_common_notations' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((127 'name' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 ()) (128
'systemid' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ())
() 1 1 0 ()) (129 'publicid' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1')))
(1 DEFERRED () ()) () 1 1 0 ())) PUBLIC ())
88 'entity_list' 'm_common_entities' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((130 'list' (DERIVED 131) (1
DEFERRED () ()) () 1 1 0 (NULL (DERIVED 131) 1))) PRIVATE ())
131 'entity_t' 'm_common_entities' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((132 'external' (LOGICAL 4) () ()
0 0 0 ()) (133 'wfc' (LOGICAL 4) () () 0 0 0 ()) (134 'name' (CHARACTER
1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (135 'text' (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (136 'publicid' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (137 'systemid' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (138 'notation' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (139 'baseuri' (
DERIVED 140) () () 0 1 0 (NULL (DERIVED 140) 0))) PUBLIC ())
140 'uri' 'fox_m_utils_uri' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' ((141 'scheme' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1)) (142 'authority' (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (143 'userinfo' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (144 'host' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (145 'port' (
INTEGER 4) () () 0 0 0 (CONSTANT (INTEGER 4) 0 '-1')) (146 'path' (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0
(NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (147 'segments' (
DERIVED 148) (1 DEFERRED () ()) () 1 1 0 (NULL (DERIVED 148) 1)) (149
'query' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ())
() 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1)) (150
'fragment' (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) (1 DEFERRED () ())
() 1 1 0 (NULL (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '1'))) 1))) PRIVATE
())
148 'path_segment' 'fox_m_utils_uri' 1 ((DERIVED UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' ((151 's' (CHARACTER 1 ((CONSTANT (
INTEGER 4) 0 '1'))) (1 DEFERRED () ()) () 1 1 0 (NULL (CHARACTER 1 ((
CONSTANT (INTEGER 4) 0 '1'))) 1))) PUBLIC ())
14 'xf' '' 152 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 24) 0 0 () () () '' () ())
12 'xf' '' 153 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 24) 0 0 () () () '' () ())
10 'replace' '' 154 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE OPTIONAL
DUMMY) (LOGICAL 4) 0 0 () () () '' () ())
9 'unit' '' 154 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER
4) 0 0 () () () '' () ())
8 'filename' '' 154 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
7 'xf' '' 154 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DUMMY) (DERIVED
24) 0 0 () () () '' () ())
5 'uri' '' 155 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
4 'prefix' '' 155 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
3 'xf' '' 155 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY) (
DERIVED 24) 0 0 () () () '' () ())
)

('cmladdnamespace' 0 2 'cmlbeginfile' 0 6 'cmlendcml' 0 11 'cmlfinishfile'
0 13 'cmlstartcml' 0 15)
