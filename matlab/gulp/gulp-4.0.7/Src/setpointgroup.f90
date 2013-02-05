  subroutine setpointgroup
!
!  Sets up the character tables for the point groups
!
!  Conditions of use:
!
!  GULP is available free of charge to academic institutions
!  and non-commerical establishments only. Copies should be
!  obtained from the author only and should not be distributed
!  in any form by the user to a third party without the express
!  permission of the author. This notice applies to all parts
!  of the program, except any library routines which are
!  distributed with the code for completeness. All rights for
!  such routines remain with the original distributor.
!
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2005
!
!  Julian Gale, Curtin University, July 2005
!
  use datatypes
  use pointgroups
  implicit none
!
!  Local variables
!
  integer(i4)            :: i
!
!  Initialise arrays
!
  nchartables(1:12,1:12,1:32) = 1
!***********************
!  Set label pointers  *
!***********************
!   1 = C_1
  nchartablelabel(1,1) = 1
!   2 = C_i/S_2
  nchartablelabel(1,2) = 2
  nchartablelabel(2,2) = 3
!   3 = C_2 
  nchartablelabel(1,3) = 1
  nchartablelabel(2,3) = 6
!   4 = C_s 
  nchartablelabel(1,4) = 4
  nchartablelabel(2,4) = 5
!   5 = C_2h
  nchartablelabel(1,5) = 2
  nchartablelabel(2,5) = 14
  nchartablelabel(3,5) = 3
  nchartablelabel(4,5) = 15
!   6 = D_2 
  nchartablelabel(1,6) = 1
  nchartablelabel(2,6) = 12
  nchartablelabel(3,6) = 13
  nchartablelabel(4,6) = 20
!   7 = C_2v
  nchartablelabel(1,7) = 10
  nchartablelabel(2,7) = 11
  nchartablelabel(3,7) = 12
  nchartablelabel(4,7) = 13
!   8 = D_2h
  nchartablelabel(1,8) = 2
  nchartablelabel(2,8) = 25
  nchartablelabel(3,8) = 26
  nchartablelabel(4,8) = 27
  nchartablelabel(5,8) = 3
  nchartablelabel(6,8) = 28
  nchartablelabel(7,8) = 29
  nchartablelabel(8,8) = 30
!   9 = C_4 
  nchartablelabel(1,9) = 1
  nchartablelabel(2,9) = 6
  nchartablelabel(3,9) = 7
  nchartablelabel(4,9) = 7
!  10 = S_4 
  nchartablelabel(1,10) = 1
  nchartablelabel(2,10) = 6
  nchartablelabel(3,10) = 7
  nchartablelabel(4,10) = 7
!  11 = C_4h
  nchartablelabel(1,11) = 2
  nchartablelabel(2,11) = 14
  nchartablelabel(3,11) = 18
  nchartablelabel(4,11) = 3
  nchartablelabel(5,11) = 15
  nchartablelabel(6,11) = 19
!  12 = D_4 
  nchartablelabel(1,12) = 10
  nchartablelabel(2,12) = 11
  nchartablelabel(3,12) = 12
  nchartablelabel(4,12) = 13
  nchartablelabel(5,12) = 7
!  13 = C_4v
  nchartablelabel(1,13) = 10
  nchartablelabel(2,13) = 11
  nchartablelabel(3,13) = 12
  nchartablelabel(4,13) = 13
  nchartablelabel(5,13) = 7
!  14 = D_2d
  nchartablelabel(1,14) = 10
  nchartablelabel(2,14) = 11
  nchartablelabel(3,14) = 12
  nchartablelabel(4,14) = 13
  nchartablelabel(5,14) = 7
!  15 = D_4h
  nchartablelabel(1,15) = 21
  nchartablelabel(2,15) = 22
  nchartablelabel(3,15) = 25
  nchartablelabel(4,15) = 26
  nchartablelabel(5,15) = 18
  nchartablelabel(6,15) = 23
  nchartablelabel(7,15) = 24
  nchartablelabel(8,15) = 28
  nchartablelabel(9,15) = 29
  nchartablelabel(10,15) = 19
!  16 = C_3 
  nchartablelabel(1,16) = 1
  nchartablelabel(2,16) = 7
  nchartablelabel(3,16) = 7
!  17 = C_3i/S_6
  nchartablelabel(1,17) = 2
  nchartablelabel(2,17) = 18
  nchartablelabel(3,17) = 3
  nchartablelabel(4,17) = 19
!  18 = D_3 
  nchartablelabel(1,18) = 1
  nchartablelabel(2,18) = 12
  nchartablelabel(3,18) = 13
  nchartablelabel(4,18) = 20
!  19 = C_3v
  nchartablelabel(1,19) = 10
  nchartablelabel(2,19) = 11
  nchartablelabel(3,19) = 7
!  20 = D_3d
  nchartablelabel(1,20) = 21
  nchartablelabel(2,20) = 22
  nchartablelabel(3,20) = 18
  nchartablelabel(4,20) = 23
  nchartablelabel(5,20) = 24
  nchartablelabel(6,20) = 19
!  21 = C_6 
  nchartablelabel(1,21) = 1
  nchartablelabel(2,21) = 6
  nchartablelabel(3,21) = 8
  nchartablelabel(4,21) = 8
  nchartablelabel(5,21) = 9
  nchartablelabel(6,21) = 9
!  22 = C_3h
  nchartablelabel(1,22) = 4
  nchartablelabel(2,22) = 16
  nchartablelabel(3,22) = 16
  nchartablelabel(4,22) = 5
  nchartablelabel(5,22) = 17
  nchartablelabel(6,22) = 17
!  23 = C_6h
  nchartablelabel(1,23) = 2
  nchartablelabel(2,23) = 14
  nchartablelabel(3,23) = 35
  nchartablelabel(4,23) = 35
  nchartablelabel(5,23) = 36
  nchartablelabel(6,23) = 36
  nchartablelabel(7,23) = 3
  nchartablelabel(8,23) = 15
  nchartablelabel(9,23) = 37
  nchartablelabel(10,23) = 37
  nchartablelabel(11,23) = 38
  nchartablelabel(12,23) = 38
!  24 = D_6 
  nchartablelabel(1,24) = 10
  nchartablelabel(2,24) = 11
  nchartablelabel(3,24) = 12
  nchartablelabel(4,24) = 13
  nchartablelabel(5,24) = 8
  nchartablelabel(6,24) = 9
!  25 = C_6v
  nchartablelabel(1,25) = 10
  nchartablelabel(2,25) = 11
  nchartablelabel(3,25) = 12
  nchartablelabel(4,25) = 13
  nchartablelabel(5,25) = 8
  nchartablelabel(6,25) = 9
!  26 = D_3h
  nchartablelabel(1,26) = 31
  nchartablelabel(2,26) = 32
  nchartablelabel(3,26) = 16
  nchartablelabel(4,26) = 33
  nchartablelabel(5,26) = 34
  nchartablelabel(6,26) = 17
!  27 = D_6h
  nchartablelabel(1,27) = 21
  nchartablelabel(2,27) = 22
  nchartablelabel(3,27) = 25
  nchartablelabel(4,27) = 26
  nchartablelabel(5,27) = 35
  nchartablelabel(6,27) = 36
  nchartablelabel(7,27) = 23
  nchartablelabel(8,27) = 24
  nchartablelabel(9,27) = 28
  nchartablelabel(10,27) = 29
  nchartablelabel(11,27) = 37
  nchartablelabel(12,27) = 38
!  28 = T 
  nchartablelabel(1,28) = 1
  nchartablelabel(2,28) = 7
  nchartablelabel(3,28) = 7
  nchartablelabel(4,28) = 39
!  29 = T_h 
  nchartablelabel(1,29) = 2
  nchartablelabel(2,29) = 18
  nchartablelabel(3,29) = 18
  nchartablelabel(4,29) = 40
  nchartablelabel(5,29) = 3
  nchartablelabel(6,29) = 19
  nchartablelabel(7,29) = 19
  nchartablelabel(8,29) = 41
!  30 = O 
  nchartablelabel(1,30) = 10
  nchartablelabel(2,30) = 11
  nchartablelabel(3,30) = 7
  nchartablelabel(4,30) = 42
  nchartablelabel(5,30) = 43
!  31 = T_d 
  nchartablelabel(1,31) = 10
  nchartablelabel(2,31) = 11
  nchartablelabel(3,31) = 7
  nchartablelabel(4,31) = 42
  nchartablelabel(5,31) = 43
!  32 = O_h 
  nchartablelabel(1,32) = 21
  nchartablelabel(2,32) = 22
  nchartablelabel(3,32) = 18
  nchartablelabel(4,32) = 44
  nchartablelabel(5,32) = 45
  nchartablelabel(6,32) = 23
  nchartablelabel(7,32) = 24
  nchartablelabel(8,32) = 19
  nchartablelabel(9,32) = 46
  nchartablelabel(10,32) = 47
!**************************
!  Set operator pointers  *
!**************************
!  1 = E
!  2 = i
!  3 = Sigma_h(xy)
!  4 = C2(z)
!  5 = C3(z)+
!  6 = C3(z)-
!  7 = C4(z)+
!  8 = C4(z)-
!  9 = C6(z)+
! 10 = C6(z)-
! 11 = Sigma_v(xz)
! 12 = Sigma_v(yz)
! 13 = Sigma_v(general)
! 14 = Sigma_d(general)
! 15 = S3(z)+
! 16 = S3(z)-
! 17 = S4(z)+
! 18 = S4(z)-
! 19 = C2(x)
! 20 = C2(y)
! 21 = C2'  (d)
! 22 = C2'' (d)
! 23 = S6(z)+
! 24 = S6(z)-
!
!  Set identity for all groups
  nchartableoperator(1,1:32) = 1
!   1 = C_1 
  nchartableops(1) = 1
!   2 = C_i/S_2 
  nchartableops(1) = 2
  nchartableoperator(2,2) = 1
!   3 = C_2 
!   4 = C_s
!   5 = C_2h 
!   6 = D_2 
!   7 = C_2v 
!   8 = D_2h
!   9 = C_4
!  10 = S_4 
!  11 = C_4h 
!  12 = D_4 
!  13 = C_4v 
!  14 = D_2d
!  15 = D_4h 
!  16 = C_3 
!  17 = C_3i/S_6 
!  18 = D_3 
!  19 = C_3v 
!  20 = D_3d
!  21 = C_6 
!  22 = C_3h 
!  23 = C_6h 
!  24 = D_6 
!  25 = C_6v 
!  26 = D_3h 
!  27 = D_6h 
!  28 = T 
!  29 = T_h 
!  30 = O 
!  31 = T_d 
!  32 = O_h 
  return
  end
