G95 module created on Sat Oct 13 11:37:04 2012 from m_common_charset.F90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () () () () () () () () () () () ())

()

()

()

()

(2 'allowed_encoding' 'm_common_charset' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 (3 NONE) () () '' ()
())
4 'checkchars' 'm_common_charset' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (5 NONE 6 NONE) () () '' () ())
7 'digits' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '10'))) 0 0 () (CONSTANT (
CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '10'))) 0 10 '0123456789') () () ''
() ())
8 'hexdigits' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '22'))) 0 0 () (
CONSTANT (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '22'))) 0 22
'0123456789abcdefABCDEF') () () '' () ())
9 'isinitialnamechar' 'm_common_charset' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (10 NONE 11
NONE) () () '' () ())
12 'isinitialncnamechar' 'm_common_charset' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (13 NONE 14
NONE) () () '' () ())
15 'islegalchar' 'm_common_charset' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (16 NONE 17 NONE 18 NONE)
() () '' () ())
19 'islegalcharref' 'm_common_charset' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (20 NONE 21 NONE) () () ''
() ())
22 'isnamechar' 'm_common_charset' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (23 NONE 24 NONE) () () ''
() ())
25 'isncnamechar' 'm_common_charset' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (26 NONE 27 NONE) () () ''
() ())
28 'isrepcharref' 'm_common_charset' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION PURE) (LOGICAL 4) 0 0 (29 NONE 30 NONE) () () ''
() ())
31 'isusascii' 'm_common_charset' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE FUNCTION) (LOGICAL 4) 0 0 (32 NONE) () () '' () ())
33 'isxml1_0_namechar' 'm_common_charset' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 (34 NONE) () () ''
() ())
35 'isxml1_1_namechar' 'm_common_charset' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE FUNCTION) (LOGICAL 4) 0 0 (36 NONE) () () ''
() ())
37 'uppercase' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '26'))) 0 0 () (
CONSTANT (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '26'))) 0 26
'ABCDEFGHIJKLMNOPQRSTUVWXYZ') () () '' () ())
38 'validchars' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '98'))) 0 0 () (
CONSTANT (CHARACTER 1 ()) 0 98
' 
	!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~')
() () '' () ())
39 'whitespace' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '4'))) 0 0 () (
CONSTANT (CHARACTER 1 ()) 0 4 ' 
	') () () '' () ())
40 'xml1_0' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '10') () () '' ()
())
41 'xml1_0_initialnamechars' 'm_common_charset' 1 ((PARAMETER UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '54')))
0 0 () (CONSTANT (CHARACTER 1 ()) 0 54
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_:') () () '' () ())
42 'xml1_0_namechars' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '66'))) 0 0 ()
(CONSTANT (CHARACTER 1 ()) 0 66
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789.-:') ()
() '' () ())
43 'xml1_1' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () (CONSTANT (INTEGER 4) 0 '11') () () '' ()
())
44 'xml1_1_initialnamechars' 'm_common_charset' 1 ((PARAMETER UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '54')))
0 0 () (CONSTANT (CHARACTER 1 ()) 0 54
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_:') () () '' () ())
45 'xml1_1_namechars' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '66'))) 0 0 ()
(CONSTANT (CHARACTER 1 ()) 0 66
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789.-:') ()
() '' () ())
46 'xml_encodingchars' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '65'))) 0 0 ()
(CONSTANT (CHARACTER 1 ()) 0 65
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-') ()
() '' () ())
47 'xml_initialencodingchars' 'm_common_charset' 1 ((PARAMETER UNKNOWN
UNKNOWN UNKNOWN NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '52')))
0 0 () (CONSTANT (CHARACTER 1 ()) 0 52
'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ') () () '' () ())
48 'xml_whitespace' 'm_common_charset' 1 ((PARAMETER UNKNOWN UNKNOWN
UNKNOWN NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '4'))) 0 0 () (
CONSTANT (CHARACTER 1 ()) 0 4 ' 
	') () () '' () ())
36 'c' '' 49 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) 0 0 () () () '' () ())
34 'c' '' 50 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) 0 0 () () () '' () ())
32 'encoding' '' 51 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
30 'xml_version' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
INTEGER 4) 0 0 () () () '' () ())
29 'i' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER 4)
0 0 () () () '' () ())
27 'xml_version' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
INTEGER 4) 0 0 () () () '' () ())
26 'c' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER 1
(())) 0 0 () () () '' () ())
24 'xml_version' '' 54 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
INTEGER 4) 0 0 () () () '' () ())
23 'c' '' 54 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER 1
(())) 0 0 () () () '' () ())
21 'xml_version' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
INTEGER 4) 0 0 () () () '' () ())
20 'i' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER 4)
0 0 () () () '' () ())
18 'xml_version' '' 56 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
INTEGER 4) 0 0 () () () '' () ())
17 'ascii_p' '' 56 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
LOGICAL 4) 0 0 () () () '' () ())
16 'c' '' 56 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) 0 0 () () () '' () ())
14 'xml_version' '' 57 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
INTEGER 4) 0 0 () () () '' () ())
13 'c' '' 57 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) 0 0 () () () '' () ())
11 'xml_version' '' 58 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
INTEGER 4) 0 0 () () () '' () ())
10 'c' '' 58 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (CHARACTER 1
((CONSTANT (INTEGER 4) 0 '1'))) 0 0 () () () '' () ())
6 'xv' '' 59 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER 4)
0 0 () () () '' () ())
5 'value' '' 59 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
3 'encoding' '' 60 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
CHARACTER 1 (())) 0 0 () () () '' () ())
)

('allowed_encoding' 0 2 'checkchars' 0 4 'digits' 0 7 'hexdigits' 0 8
'isinitialnamechar' 0 9 'isinitialncnamechar' 0 12 'islegalchar' 0 15
'islegalcharref' 0 19 'isnamechar' 0 22 'isncnamechar' 0 25 'isrepcharref'
0 28 'isusascii' 0 31 'isxml1_0_namechar' 0 33 'isxml1_1_namechar' 0 35
'uppercase' 0 37 'validchars' 0 38 'whitespace' 0 39 'xml1_0' 0 40
'xml1_0_initialnamechars' 0 41 'xml1_0_namechars' 0 42 'xml1_1' 0 43
'xml1_1_initialnamechars' 0 44 'xml1_1_namechars' 0 45 'xml_encodingchars'
0 46 'xml_initialencodingchars' 0 47 'xml_whitespace' 0 48)
