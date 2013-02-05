  subroutine sixbody(nptr6,n6ty,sdist,esix,e1d,e2d,e3d,rksix,lgrad1,lgrad2,lgrad3)
!
!  Calculates six-body potential first, second and third derivatives with respect to 
!  the interatomic distances that make the six-body term.
!
!  Note at that the moment it is constructed to handle the out of plane cross potential
!
!   1/06 Created based on fourbody.f
!   6/06 Derivatives finished
!  12/07 Unused variables removed
!
!  On entry :
!
!  nptr6   = pointer to potential number
!  n6ty    = pointer to type of six-body potential
!  sdist   = array of distances between atoms 
!  rksix   = first parameter associated with potential type
!  lgrad1  = if .true. calculate the first derivatives
!  lgrad2  = if .true. calculate the second derivatives
!  lgrad3  = if .true. calculate the third derivatives
!
!  On exit :
!
!  esix    = contribution to three-body energy
!  e1d     = array of first derivative terms
!  e2d     = array of second derivative terms
!  e3d     = array of third derivative terms
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, December 2007
!
  use constants
  use control
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: n6ty
  integer(i4), intent(in)  :: nptr6
  logical,     intent(in)  :: lgrad1
  logical,     intent(in)  :: lgrad2
  logical,     intent(in)  :: lgrad3
  real(dp),    intent(out) :: e1d(15)
  real(dp),    intent(out) :: e2d(120)
  real(dp),    intent(out) :: e3d(680)
  real(dp),    intent(out) :: esix
  real(dp),    intent(in)  :: sdist(15)
  real(dp),    intent(in)  :: rksix
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: ii
  integer(i4)              :: ij
  integer(i4)              :: ik
  integer(i4)              :: j
  integer(i4)              :: jk
  integer(i4)              :: k
  integer(i4)              :: kb2(15,15)
  real(dp)                 :: b0a
  real(dp)                 :: b0b
  real(dp)                 :: b1ai
  real(dp)                 :: b1aj
  real(dp)                 :: b1ak
  real(dp)                 :: b1bi
  real(dp)                 :: b1bj
  real(dp)                 :: b1bk
  real(dp)                 :: b2aij
  real(dp)                 :: b2aik
  real(dp)                 :: b2ajk
  real(dp)                 :: b2bij
  real(dp)                 :: b2bik
  real(dp)                 :: b2bjk
  real(dp)                 :: bot0a
  real(dp)                 :: bot0b
  real(dp)                 :: cos1a
  real(dp)                 :: cos1b
  real(dp)                 :: cos3a
  real(dp)                 :: cos3b
  real(dp)                 :: cosphia
  real(dp)                 :: cosphib
  real(dp)                 :: cos1a1d(15),cos3a1d(15)
  real(dp)                 :: cos1b1d(15),cos3b1d(15)
  real(dp)                 :: cos1a2d(120),cos3a2d(680)
  real(dp)                 :: cos1b2d(120),cos3b2d(680)
  real(dp)                 :: cos1a3d(56),cos3a3d(56)
  real(dp)                 :: cos1b3d(56),cos3b3d(56)
  real(dp)                 :: cosp1da(15),cosp2da(120),cosp3da(680)
  real(dp)                 :: cosp1db(15),cosp2db(120),cosp3db(680)
  real(dp)                 :: ctrm1a
  real(dp)                 :: ctrm1b
  real(dp)                 :: ctrm2a
  real(dp)                 :: ctrm2b
  real(dp)                 :: oopda
  real(dp)                 :: oopd1a(15),oopd2a(120)
  real(dp)                 :: oopdb
  real(dp)                 :: oopd1b(15),oopd2b(120)
  real(dp)                 :: rctrm1a,rctrm1ad1(15),rctrm1ad2(120)
  real(dp)                 :: rctrm1b,rctrm1bd1(15),rctrm1bd2(120)
  real(dp)                 :: rctrm2a,rctrm2ad1(15),rctrm2ad2(120)
  real(dp)                 :: rctrm2b,rctrm2bd1(15),rctrm2bd2(120)
  real(dp)                 :: rrctrm1a
  real(dp)                 :: rrctrm1b
  real(dp)                 :: rrctrm2a
  real(dp)                 :: rrctrm2b
  real(dp)                 :: r12
  real(dp)                 :: r122
  real(dp)                 :: rr12
  real(dp)                 :: rr122
  real(dp)                 :: rr124
  real(dp)                 :: r13
  real(dp)                 :: r132
  real(dp)                 :: r14
  real(dp)                 :: r142
  real(dp)                 :: r15
  real(dp)                 :: r152
  real(dp)                 :: rr15
  real(dp)                 :: rr152
  real(dp)                 :: rr154
  real(dp)                 :: r16
  real(dp)                 :: r162
  real(dp)                 :: rr16
  real(dp)                 :: rr162
  real(dp)                 :: rr164
  real(dp)                 :: r23
  real(dp)                 :: r232
  real(dp)                 :: rr23
  real(dp)                 :: rr232
  real(dp)                 :: rr234
  real(dp)                 :: r24
  real(dp)                 :: r242
  real(dp)                 :: rr24
  real(dp)                 :: rr242
  real(dp)                 :: rr244
  real(dp)                 :: r25
  real(dp)                 :: r252
  real(dp)                 :: r26
  real(dp)                 :: r262
  real(dp)                 :: r34
  real(dp)                 :: r342
  real(dp)                 :: r56
  real(dp)                 :: r562
  real(dp)                 :: rsin1a
  real(dp)                 :: rsin1b
  real(dp)                 :: rsin3a
  real(dp)                 :: rsin3b
  real(dp)                 :: rtan1a
  real(dp)                 :: rtan1b
  real(dp)                 :: rtan3a
  real(dp)                 :: rtan3b
  real(dp)                 :: sin1a1d(15),sin3a1d(15)
  real(dp)                 :: sin1a2d(120),sin3a2d(120)
  real(dp)                 :: sin1a3d(56),sin3a3d(56)
  real(dp)                 :: sin1b1d(15),sin3b1d(15)
  real(dp)                 :: sin1b2d(120),sin3b2d(120)
  real(dp)                 :: sin1b3d(56),sin3b3d(56)
  real(dp)                 :: sin1a
  real(dp)                 :: sin1b
  real(dp)                 :: sin3a
  real(dp)                 :: sin3b
  real(dp)                 :: t0a
  real(dp)                 :: t0b
  real(dp)                 :: t1ai
  real(dp)                 :: t1aj
  real(dp)                 :: t1ak
  real(dp)                 :: t1bi
  real(dp)                 :: t1bj
  real(dp)                 :: t1bk
  real(dp)                 :: t2aij
  real(dp)                 :: t2aik
  real(dp)                 :: t2ajk
  real(dp)                 :: t2bij
  real(dp)                 :: t2bik
  real(dp)                 :: t2bjk
  real(dp)                 :: top0a
  real(dp)                 :: top0b
  real(dp)                 :: top1a(15),bot1a(15)
  real(dp)                 :: top2a(120),bot2a(120)
  real(dp)                 :: top3a(56),bot3a(56)
  real(dp)                 :: top1b(15),bot1b(15)
  real(dp)                 :: top2b(120),bot2b(120)
  real(dp)                 :: top3b(56),bot3b(56)
!
!  Set up kb2
!
  if (lgrad2) then
    ii = 0
    do i = 1,15
      do j = i,15
        ii = ii + 1
        kb2(j,i) = ii
        kb2(i,j) = ii
      enddo
    enddo
  endif
!
!  Zero terms
!
  esix = 0.0_dp
  if (lgrad1) then
    do i = 1,15
      e1d(i) = 0.0_dp
      top1a(i) = 0.0_dp
      top1b(i) = 0.0_dp
      bot1a(i) = 0.0_dp
      bot1b(i) = 0.0_dp
      cos1a1d(i) = 0.0_dp
      cos1b1d(i) = 0.0_dp
      cos3a1d(i) = 0.0_dp
      cos3b1d(i) = 0.0_dp
      cosp1da(i) = 0.0_dp
      cosp1db(i) = 0.0_dp
      sin1a1d(i) = 0.0_dp
      sin1b1d(i) = 0.0_dp
      sin3a1d(i) = 0.0_dp
      sin3b1d(i) = 0.0_dp
    enddo
    if (lgrad2) then
      do i = 1,120
        e2d(i) = 0.0_dp
        top2a(i) = 0.0_dp
        top2b(i) = 0.0_dp
        bot2a(i) = 0.0_dp
        bot2b(i) = 0.0_dp
        cos1a2d(i) = 0.0_dp
        cos1b2d(i) = 0.0_dp
        cos3a2d(i) = 0.0_dp
        cos3b2d(i) = 0.0_dp
        cosp2da(i) = 0.0_dp
        cosp2db(i) = 0.0_dp
        sin1a2d(i) = 0.0_dp
        sin1b2d(i) = 0.0_dp
        sin3a2d(i) = 0.0_dp
        sin3b2d(i) = 0.0_dp
      enddo
      if (lgrad3) then
        do i = 1,56
          e3d(i) = 0.0_dp
          top3a(i) = 0.0_dp
          top3b(i) = 0.0_dp
          bot3a(i) = 0.0_dp
          bot3b(i) = 0.0_dp
          cos1a3d(i) = 0.0_dp
          cos1b3d(i) = 0.0_dp
          cos3a3d(i) = 0.0_dp
          cos3b3d(i) = 0.0_dp
          cosp3da(i) = 0.0_dp
          cosp3db(i) = 0.0_dp
          sin1a3d(i) = 0.0_dp
          sin1b3d(i) = 0.0_dp
          sin3a3d(i) = 0.0_dp
          sin3b3d(i) = 0.0_dp
        enddo
      endif
    endif
  endif
!
!  Set up local constants :
!
!  a => terms for 1-2-3-4 quartet
!  b => terms for 2-1-5-6 quartet
!
  r12 = sdist(1)
  r13 = sdist(2)
  r14 = sdist(3)
  r15 = sdist(4)
  r16 = sdist(5)
  r23 = sdist(6)
  r24 = sdist(7)
  r25 = sdist(8)
  r26 = sdist(9)
  r34 = sdist(10)
  r56 = sdist(15)
  r122 = r12*r12
  r132 = r13*r13
  r142 = r14*r14
  r152 = r15*r15
  r162 = r16*r16
  r232 = r23*r23
  r242 = r24*r24
  r252 = r25*r25
  r262 = r26*r26
  r342 = r34*r34
  r562 = r56*r56
  rr12 = 1.0_dp/r12
  rr15 = 1.0_dp/r15
  rr16 = 1.0_dp/r16
  rr23 = 1.0_dp/r23
  rr24 = 1.0_dp/r24
  rr122 = rr12*rr12
  rr152 = rr15*rr15
  rr162 = rr16*rr16
  rr232 = rr23*rr23
  rr242 = rr24*rr24
  if (lgrad2) then
    rr124 = rr122*rr122
    rr154 = rr152*rr152
    rr164 = rr162*rr162
    rr234 = rr232*rr232
    rr244 = rr242*rr242
  endif
!***************************************
!  Set up potential independent terms  *
!***************************************
!$$$$$$$$$$$$$$$$$
!  Cosine terms  $
!$$$$$$$$$$$$$$$$$
!
!  Cosine theta 1a = 1-2-3, 2a = 2-3-4 and 3a = 3-2-4
!
  cos1a = 0.5_dp*(r232+r122-r132)/(r12*r23)
  cos3a = 0.5_dp*(r232+r242-r342)/(r23*r24)
!
!  Cosine theta 1b = 2-1-5, 2b = 1-5-6 and 3b = 5-1-6
!
  cos1b = 0.5_dp*(r152+r122-r252)/(r12*r15)
  cos3b = 0.5_dp*(r152+r162-r562)/(r15*r16)
!
!  Check for angles which are 0 or 180 degrees with potentials
!  which cannot cope with these. For those that can the four-
!  body contribution must go to zero when the angle approaches
!  this limit - hence we can just return having set all the
!  derivatives and energy to zero
!
  if (abs(cos1a).ge.0.99999999_dp) then
    call outerror('Sixbody angle has become 0/180 degrees',0_i4)
    if (ioproc) then
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
      write(ioout,'(''!!  Sixbody potential = '',i3,/)') nptr6
      write(ioout,'(''!!  Problem angle = 1-2-3'',/)')
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
    endif
    call stopnow('sixbody')
  endif
  if (abs(cos1b).ge.0.99999999_dp) then
    call outerror('Sixbody angle has become 0/180 degrees',0_i4)
    if (ioproc) then
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
      write(ioout,'(''!!  Sixbody potential = '',i3,/)') nptr6
      write(ioout,'(''!!  Problem angle = 2-1-5'',/)')
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
    endif
    call stopnow('sixbody')
  endif
  if (lgrad1) then
!
!  First
!       a    b
!  1 = r12  r12
!  2 = r13  r25
!  3 = r14  r26
!  4 = r23  r15
!  5 = r24  r16
!  6 = r34  r56
!
    cos1a1d(1)  = rr12*rr23 - cos1a*rr122
    cos1a1d(2)  = - rr12*rr23
    cos1a1d(6)  = rr12*rr23 - cos1a*rr232
!
    cos3a1d(6)  = rr23*rr24 - cos3a*rr232
    cos3a1d(7)  = rr23*rr24 - cos3a*rr242
    cos3a1d(10) = - rr23*rr24
!
    cos1b1d(1)  = rr12*rr15 - cos1b*rr122
    cos1b1d(4)  = rr12*rr15 - cos1b*rr152
    cos1b1d(8)  = - rr12*rr15
!
    cos3b1d(4)  = rr15*rr16 - cos3b*rr152
    cos3b1d(5)  = rr15*rr16 - cos3b*rr162
    cos3b1d(15) = - rr15*rr16
!
    if (lgrad2) then
!
!  Second
!
!  1 = 11  7 = 22 12 = 33 16 = 44 19 = 55 21 = 66
!  2 = 21  8 = 32 13 = 43 17 = 54 20 = 65
!  3 = 31  9 = 42 14 = 53 18 = 64
!  4 = 41 10 = 52 15 = 63
!  5 = 51 11 = 62
!  6 = 61 
!
      cos1a2d(kb2(1,1)) = -2.0_dp*rr122*rr12*rr23 + 3.0_dp*cos1a*rr124
      cos1a2d(kb2(2,1)) = rr122*rr12*rr23
      cos1a2d(kb2(6,1)) = rr12*rr23*(cos1a*rr12*rr23 - rr122 - rr232)
      cos1a2d(kb2(6,2)) = rr232*rr23*rr12
      cos1a2d(kb2(6,6)) = -2.0_dp*rr232*rr23*rr12 + 3.0_dp*cos1a*rr234
!
      cos3a2d(kb2(6,6))  = -2.0_dp*rr232*rr23*rr24 + 3.0_dp*cos3a*rr234
      cos3a2d(kb2(7,6))  = rr23*rr24*(cos3a*rr23*rr24 - rr232 - rr242)
      cos3a2d(kb2(7,7))  = -2.0_dp*rr242*rr24*rr23 + 3.0_dp*cos3a*rr244
      cos3a2d(kb2(10,6)) = rr232*rr23*rr24
      cos3a2d(kb2(10,7)) = rr242*rr24*rr23
!
      cos1b2d(kb2(1,1)) = -2.0_dp*rr122*rr12*rr15 + 3.0_dp*cos1b*rr124
      cos1b2d(kb2(4,1)) = rr12*rr15*(cos1b*rr12*rr15 - rr122 - rr152)
      cos1b2d(kb2(4,4)) = -2.0_dp*rr152*rr15*rr12 + 3.0_dp*cos1b*rr154
      cos1b2d(kb2(8,1)) = rr122*rr12*rr15
      cos1b2d(kb2(8,4)) = rr152*rr15*rr12
!
      cos3b2d(kb2(4,4))  = -2.0_dp*rr152*rr15*rr16 + 3.0_dp*cos3b*rr154
      cos3b2d(kb2(5,4))  = rr15*rr16*(cos3b*rr15*rr16 - rr152 - rr162)
      cos3b2d(kb2(5,5))  = -2.0_dp*rr162*rr16*rr15 + 3.0_dp*cos3b*rr164
      cos3b2d(kb2(15,4)) = rr152*rr15*rr16
      cos3b2d(kb2(15,5)) = rr162*rr16*rr15
!
      if (lgrad3) then
!
!  Third
!
!   1 = 111  22 = 222  37 = 333  47 = 444  53 = 555  56 = 666
!   2 = 211  23 = 322  38 = 433  48 = 544  54 = 655
!   3 = 311  24 = 422  39 = 533  49 = 644  55 = 665
!   4 = 411  25 = 522  40 = 633  50 = 554
!   5 = 511  26 = 622  41 = 443  51 = 654
!   6 = 611  27 = 332  42 = 543  52 = 664
!   7 = 221  28 = 432  43 = 643
!   8 = 321  29 = 532  44 = 553
!   9 = 421  30 = 632  45 = 653
!  10 = 521  31 = 442  46 = 663
!  11 = 621  32 = 542
!  12 = 331  33 = 642
!  13 = 431  34 = 552
!  14 = 531  35 = 652
!  15 = 631  36 = 662
!  16 = 441 
!  17 = 541 
!  18 = 641 
!  19 = 551 
!  20 = 651
!  21 = 661
!
        cos1a3d(1) = rr124*(9.0_dp*rr12*rr23 - 15.0_dp*cos1a*rr122)
        cos1a3d(2) = - 3.0_dp*rr124*rr12*rr23
        cos1a3d(4) = rr122*(2.0_dp*rr12*rr232*rr23 + 3.0_dp*rr122*rr12*rr23 - 3.0_dp*cos1a*rr122*rr232)
        cos1a3d(9) = - rr122*rr12*rr232*rr23
        cos1a3d(16) = rr232*(2.0_dp*rr23*rr122*rr12 + 3.0_dp*rr232*rr23*rr12 - 3.0_dp*cos1a*rr232*rr122)
        cos1a3d(31) = - 3.0_dp*rr234*rr23*rr12
        cos1a3d(47) = rr234*(9.0_dp*rr23*rr12 - 15.0_dp*cos1a*rr232)
!
        cos3a3d(47) = rr234*(9.0_dp*rr23*rr24 - 15.0_dp*cos3a*rr232)
        cos3a3d(48) = rr232*(2.0_dp*rr23*rr242*rr24 + 3.0_dp*rr232*rr23*rr24 - 3.0_dp*cos3a*rr232*rr242)
        cos3a3d(49) = - 3.0_dp*rr234*rr23*rr24
        cos3a3d(50) = rr242*(2.0_dp*rr24*rr232*rr23 + 3.0_dp*rr242*rr24*rr23 - 3.0_dp*cos3a*rr242*rr232)
        cos3a3d(51) = - rr232*rr23*rr242*rr24
        cos3a3d(53) = rr244*(9.0_dp*rr24*rr23 - 15.0_dp*cos3a*rr242)
        cos3a3d(54) = - 3.0_dp*rr244*rr24*rr23
!
        cos1b3d(1) = rr124*(9.0_dp*rr12*rr15 - 15.0_dp*cos1b*rr122)
        cos1b3d(2) = - 3.0_dp*rr124*rr12*rr15
        cos1b3d(4) = rr122*(2.0_dp*rr12*rr152*rr15 + 3.0_dp*rr122*rr12*rr15 - 3.0_dp*cos1b*rr122*rr152)
        cos1b3d(9) = - rr122*rr12*rr152*rr15
        cos1b3d(16) = rr152*(2.0_dp*rr15*rr122*rr12 + 3.0_dp*rr152*rr15*rr12 - 3.0_dp*cos1b*rr152*rr122)
        cos1b3d(31) = - 3.0_dp*rr154*rr15*rr12
        cos1b3d(47) = rr154*(9.0_dp*rr15*rr12 - 15.0_dp*cos1b*rr152)
!
        cos3b3d(47) = rr154*(9.0_dp*rr15*rr16 - 15.0_dp*cos3b*rr152)
        cos3b3d(48) = rr152*(2.0_dp*rr15*rr162*rr16 + 3.0_dp*rr152*rr15*rr16 - 3.0_dp*cos3b*rr152*rr162)
        cos3b3d(49) = - 3.0_dp*rr154*rr15*rr16
        cos3b3d(50) = rr162*(2.0_dp*rr16*rr152*rr15 + 3.0_dp*rr162*rr16*rr15 - 3.0_dp*cos3b*rr162*rr152)
        cos3b3d(51) = - rr152*rr15*rr162*rr16
        cos3b3d(53) = rr164*(9.0_dp*rr16*rr15 - 15.0_dp*cos3b*rr162)
        cos3b3d(54) = - 3.0_dp*rr164*rr16*rr15
!
      endif
    endif
  endif
!$$$$$$$$$$$$$$$
!  Sine terms  $
!$$$$$$$$$$$$$$$
  sin1a = sqrt(1.0_dp-cos1a*cos1a)
  sin3a = sqrt(1.0_dp-cos3a*cos3a)
  sin1b = sqrt(1.0_dp-cos1b*cos1b)
  sin3b = sqrt(1.0_dp-cos3b*cos3b)
  rsin1a = 1.0_dp/sin1a
  rsin3a = 1.0_dp/sin3a
  rsin1b = 1.0_dp/sin1b
  rsin3b = 1.0_dp/sin3b
  if (lgrad1) then
!
!  First derivatives
!
    rtan1a = cos1a*rsin1a
    rtan3a = cos3a*rsin3a
    rtan1b = cos1b*rsin1b
    rtan3b = cos3b*rsin3b
    do i = 1,15
      sin1a1d(i) = - rtan1a*cos1a1d(i)
      sin3a1d(i) = - rtan3a*cos3a1d(i)
      sin1b1d(i) = - rtan1b*cos1b1d(i)
      sin3b1d(i) = - rtan3b*cos3b1d(i)
    enddo
    if (lgrad2) then
!
!  Second derivatives
!
      ii = 0
      do i = 1,15
        do j = i,15
          ii = ii + 1
          sin1a2d(ii) = - rtan1a*cos1a2d(ii) - rsin1a*cos1a1d(i)*cos1a1d(j)*(1.0_dp+rtan1a*rtan1a)
          sin3a2d(ii) = - rtan3a*cos3a2d(ii) - rsin3a*cos3a1d(i)*cos3a1d(j)*(1.0_dp+rtan3a*rtan3a)
          sin1b2d(ii) = - rtan1b*cos1b2d(ii) - rsin1b*cos1b1d(i)*cos1b1d(j)*(1.0_dp+rtan1b*rtan1b)
          sin3b2d(ii) = - rtan3b*cos3b2d(ii) - rsin3b*cos3b1d(i)*cos3b1d(j)*(1.0_dp+rtan3b*rtan3b)
        enddo
      enddo
      if (lgrad3) then
!
!  Third derivatives
!
        ii = 0
        do i = 1,15
          do j = i,15
            ij = kb2(j,i)
            do k = j,15
              ii = ii + 1
              ik = kb2(k,i)
              jk = kb2(k,j)
              sin1a3d(ii) = - rtan1a*cos1a3d(ii) - rsin1a*(1.0_dp+rtan1a*rtan1a)*(cos1a1d(i)*cos1a2d(jk) +  &
                cos1a1d(j)*cos1a2d(ik) + cos1a1d(k)*cos1a2d(ij)) - cos1a1d(i)*cos1a1d(j)*cos1a1d(k)*rtan1a* &
                rsin1a*rsin1a*3.0_dp*(1.0_dp+rtan1a*rtan1a)
              sin3a3d(ii) = - rtan3a*cos3a3d(ii) - rsin3a*(1.0_dp+rtan3a*rtan3a)*(cos3a1d(i)*cos3a2d(jk) +  &
                cos3a1d(j)*cos3a2d(ik) + cos3a1d(k)*cos3a2d(ij)) - cos3a1d(i)*cos3a1d(j)*cos3a1d(k)*rtan3a* &
                rsin3a*rsin3a*3.0_dp*(1.0_dp+rtan3a*rtan3a)
              sin1b3d(ii) = - rtan1b*cos1b3d(ii) - rsin1b*(1.0_dp+rtan1b*rtan1b)*(cos1b1d(i)*cos1b2d(jk) +  &
                cos1b1d(j)*cos1b2d(ik) + cos1b1d(k)*cos1b2d(ij)) - cos1b1d(i)*cos1b1d(j)*cos1b1d(k)*rtan1b* &
                rsin1b*rsin1b*3.0_dp*(1.0_dp+rtan1b*rtan1b)
              sin3b3d(ii) = - rtan3b*cos3b3d(ii) - rsin3b*(1.0_dp+rtan3b*rtan3b)*(cos3b1d(i)*cos3b2d(jk) +  &
                cos3b1d(j)*cos3b2d(ik) + cos3b1d(k)*cos3b2d(ij)) - cos3b1d(i)*cos3b1d(j)*cos3b1d(k)*rtan3b* &
                rsin3b*rsin3b*3.0_dp*(1.0_dp+rtan3b*rtan3b)
            enddo
          enddo
        enddo
      endif
    endif
  endif
!$$$$$$$$$$$$$$
!  Phi terms  $
!$$$$$$$$$$$$$$
  top0a = r122 + r242 - r142 - 2.0_dp*r12*r24*cos1a*cos3a
  bot0a = rr12*rr24*rsin1a*rsin3a
  top0b = r122 + r162 - r262 - 2.0_dp*r12*r16*cos1b*cos3b
  bot0b = rr12*rr16*rsin1b*rsin3b
  cosphia = 0.5_dp*top0a*bot0a
  cosphib = 0.5_dp*top0b*bot0b
  if (abs(cosphia).gt.1.0_dp) cosphia = sign(1.0_dp,cosphia)
  if (abs(cosphib).gt.1.0_dp) cosphib = sign(1.0_dp,cosphib)
  if (lgrad1) then
!
!  First
!       a    b
!  1 = r12  r12
!  2 = r13  r25
!  3 = r14  r26
!  4 = r23  r15
!  5 = r24  r16
!  6 = r34  r56
!
    top1a(1) = 2.0_dp - 2.0_dp*r24*cos1a*cos3a*rr12
    top1a(3) = - 2.0_dp
    top1a(7) = 2.0_dp - 2.0_dp*r12*cos1a*cos3a*rr24
    top1b(1) = 2.0_dp - 2.0_dp*r16*cos1b*cos3b*rr12
    top1b(5) = 2.0_dp - 2.0_dp*r12*cos1b*cos3b*rr16
    top1b(9) = - 2.0_dp
    do i = 1,15
      top1a(i) = top1a(i) - 2.0_dp*r12*r24*(cos1a1d(i)*cos3a+cos1a*cos3a1d(i))
      top1b(i) = top1b(i) - 2.0_dp*r12*r16*(cos1b1d(i)*cos3b+cos1b*cos3b1d(i))
    enddo
    bot1a(1) = - rr24*rsin1a*rsin3a*(rr12**3)
    bot1a(7) = - rr12*rsin1a*rsin3a*(rr24**3)
    bot1b(1) = - rr16*rsin1b*rsin3b*(rr12**3)
    bot1b(5) = - rr12*rsin1b*rsin3b*(rr16**3)
    do i = 1,15
      bot1a(i) = bot1a(i) - rr12*rr24*(rsin1a**2*sin1a1d(i)*rsin3a + rsin1a*rsin3a**2*sin3a1d(i))
      bot1b(i) = bot1b(i) - rr12*rr16*(rsin1b**2*sin1b1d(i)*rsin3b + rsin1b*rsin3b**2*sin3b1d(i))
    enddo
!
!  Combine derivatives
!
    do i = 1,15
      cosp1da(i) = 0.5_dp*(bot0a*top1a(i) + top0a*bot1a(i))
      cosp1db(i) = 0.5_dp*(bot0b*top1b(i) + top0b*bot1b(i))
    enddo
    if (lgrad2) then
!
!  Second
!
!  1 = 11  7 = 22 12 = 33 16 = 44 19 = 55 21 = 66
!  2 = 21  8 = 32 13 = 43 17 = 54 20 = 65
!  3 = 31  9 = 42 14 = 53 18 = 64
!  4 = 41 10 = 52 15 = 63
!  5 = 51 11 = 62
!  6 = 61 
!
!
!  Top / bottom part of cosphi derivatives 
!
      ii = 0
      do i = 1,15
        do j = i,15
          ii = ii + 1
          top2a(ii) = top2a(ii) - 2.0_dp*r12*r24*(cos1a2d(ii)*cos3a+cos1a*cos3a2d(ii))
          top2a(ii) = top2a(ii) - 2.0_dp*r12*r24*(cos1a1d(i)*cos3a1d(j)+cos1a1d(j)*cos3a1d(i))
          top2b(ii) = top2b(ii) - 2.0_dp*r12*r16*(cos1b2d(ii)*cos3b+cos1b*cos3b2d(ii))
          top2b(ii) = top2b(ii) - 2.0_dp*r12*r16*(cos1b1d(i)*cos3b1d(j)+cos1b1d(j)*cos3b1d(i))
          if (i.eq.1) then
            top2a(ii) = top2a(ii) - 2.0_dp*r24*rr12*(cos1a*cos3a1d(j)+cos3a*cos1a1d(j))
            if (j.eq.1) then
              top2a(ii) = top2a(ii) + 2.0_dp*cos1a*cos3a*r24*rr122*rr12
            endif
          elseif (i.eq.7) then
            top2a(ii) = top2a(ii) - 2.0_dp*r12*rr24*(cos1a*cos3a1d(j)+cos3a*cos1a1d(j))
            if (j.eq.7) then
              top2a(ii) = top2a(ii) + 2.0_dp*cos1a*cos3a*r12*rr242*rr24
            elseif (j.eq.1) then
              top2a(ii) = top2a(ii) - 2.0_dp*cos1a*cos3a*rr24*rr12
            endif
          endif
          if (j.eq.1) then
            top2a(ii) = top2a(ii) - 2.0_dp*r24*rr12*(cos1a*cos3a1d(i)+cos3a*cos1a1d(i))
          elseif (j.eq.7) then
            top2a(ii) = top2a(ii) - 2.0_dp*r12*rr24*(cos1a*cos3a1d(i)+cos3a*cos1a1d(i))
            if (i.eq.1) then
              top2a(ii) = top2a(ii) - 2.0_dp*cos1a*cos3a*rr24*rr12
            endif
          endif
          if (i.eq.1) then
            top2b(ii) = top2b(ii) - 2.0_dp*r16*rr12*(cos1b*cos3b1d(j)+cos3b*cos1b1d(j))
            if (j.eq.1) then
              top2b(ii) = top2b(ii) + 2.0_dp*cos1b*cos3b*r16*rr122*rr12
            endif
          elseif (i.eq.5) then
            top2b(ii) = top2b(ii) - 2.0_dp*r12*rr16*(cos1b*cos3b1d(j)+cos3b*cos1b1d(j))
            if (j.eq.5) then
              top2b(ii) = top2b(ii) + 2.0_dp*cos1b*cos3b*r12*rr162*rr16
            elseif (j.eq.1) then
              top2b(ii) = top2b(ii) - 2.0_dp*cos1b*cos3b*rr16*rr12
            endif
          endif
          if (j.eq.1) then
            top2b(ii) = top2b(ii) - 2.0_dp*r16*rr12*(cos1b*cos3b1d(i)+cos3b*cos1b1d(i))
          elseif (j.eq.5) then
            top2b(ii) = top2b(ii) - 2.0_dp*r12*rr16*(cos1b*cos3b1d(i)+cos3b*cos1b1d(i))
            if (i.eq.1) then
              top2b(ii) = top2b(ii) - 2.0_dp*cos1b*cos3b*rr16*rr12
            endif
          endif
        enddo
      enddo
      ii = 0
      do i = 1,15
        do j = i,15
          ii = ii + 1
          bot2a(ii) = bot2a(ii) + bot0a*((rsin1a**2)*sin1a1d(i)*sin1a1d(j)+(rsin3a**2)*sin3a1d(i)*sin3a1d(j))
          bot2a(ii) = bot2a(ii) - bot0a*(rsin1a*sin1a2d(ii)+rsin3a*sin3a2d(ii))
          bot2a(ii) = bot2a(ii) - (rsin1a*sin1a1d(i)+rsin3a*sin3a1d(i))*bot1a(j)
          if (i.eq.1.and.j.eq.1) then
            bot2a(ii) = bot2a(ii) - rr122*bot1a(j)
            bot2a(ii) = bot2a(ii) + 2.0_dp*rr122*rr122*bot0a
          elseif (i.eq.7.and.j.eq.7) then
            bot2a(ii) = bot2a(ii) - rr242*bot1a(j)
            bot2a(ii) = bot2a(ii) + 2.0_dp*rr242*rr242*bot0a
          elseif (i.eq.1) then
            bot2a(ii) = bot2a(ii) - rr122*bot1a(j)
          elseif (i.eq.7) then
            bot2a(ii) = bot2a(ii) - rr242*bot1a(j)
          endif
          bot2b(ii) = bot2b(ii) + bot0b*((rsin1b**2)*sin1b1d(i)*sin1b1d(j)+(rsin3b**2)*sin3b1d(i)*sin3b1d(j))
          bot2b(ii) = bot2b(ii) - bot0b*(rsin1b*sin1b2d(ii)+rsin3b*sin3b2d(ii))
          bot2b(ii) = bot2b(ii) - (rsin1b*sin1b1d(i)+rsin3b*sin3b1d(i))*bot1b(j)
          if (i.eq.1.and.j.eq.1) then
            bot2b(ii) = bot2b(ii) - rr122*bot1b(j)
            bot2b(ii) = bot2b(ii) + 2.0_dp*rr122*rr122*bot0b
          elseif (i.eq.5.and.j.eq.5) then
            bot2b(ii) = bot2b(ii) - rr162*bot1b(j)
            bot2b(ii) = bot2b(ii) + 2.0_dp*rr162*rr162*bot0b
          elseif (i.eq.1) then
            bot2b(ii) = bot2b(ii) - rr122*bot1b(j)
          elseif (i.eq.5) then
            bot2b(ii) = bot2b(ii) - rr162*bot1b(j)
          endif
        enddo
      enddo
!
!  Combine derivatives
!
      ii = 0
      do i = 1,15
        do j = i,15
          ii = ii + 1
          cosp2da(ii) = 0.5_dp*(bot0a*top2a(ii) + top0a*bot2a(ii))
          cosp2db(ii) = 0.5_dp*(bot0b*top2b(ii) + top0b*bot2b(ii))
          cosp2da(ii) = cosp2da(ii) + 0.5_dp*(bot1a(j)*top1a(i) + top1a(j)*bot1a(i))
          cosp2db(ii) = cosp2db(ii) + 0.5_dp*(bot1b(j)*top1b(i) + top1b(j)*bot1b(i))
        enddo
      enddo
      if (lgrad3) then
!
!  Third 
!
!   1 = 111  22 = 222  37 = 333  47 = 444  53 = 555  56 = 666
!   2 = 211  23 = 322  38 = 433  48 = 544  54 = 655
!   3 = 311  24 = 422  39 = 533  49 = 644  55 = 665
!   4 = 411  25 = 522  40 = 633  50 = 554
!   5 = 511  26 = 622  41 = 443  51 = 654
!   6 = 611  27 = 332  42 = 543  52 = 664
!   7 = 221  28 = 432  43 = 643
!   8 = 321  29 = 532  44 = 553
!   9 = 421  30 = 632  45 = 653
!  10 = 521  31 = 442  46 = 663
!  11 = 621  32 = 542
!  12 = 331  33 = 642
!  13 = 431  34 = 552
!  14 = 531  35 = 652
!  15 = 631  36 = 662
!  16 = 441 
!  17 = 541 
!  18 = 641 
!  19 = 551 
!  20 = 651
!  21 = 661
!
!  Top part
!
        t0a = 2.0_dp*cos1a*cos3a
        t0b = 2.0_dp*cos1b*cos3b
        ii = 0
        do i = 1,15
          t1ai = cos1a*cos3a1d(i) + cos3a*cos1a1d(i)
          t1bi = cos1b*cos3b1d(i) + cos3b*cos1b1d(i)
          do j = i,15
            ij = kb2(j,i)
            t1aj = cos1a*cos3a1d(j) + cos3a*cos1a1d(j)
            t1bj = cos1b*cos3b1d(j) + cos3b*cos1b1d(j)
            t2aij = cos1a*cos3a2d(ij) + cos3a*cos1a2d(ij) + cos1a1d(i)*cos3a1d(j) + cos1a1d(j)*cos3a1d(i)
            t2bij = cos1b*cos3b2d(ij) + cos3b*cos1b2d(ij) + cos1b1d(i)*cos3b1d(j) + cos1b1d(j)*cos3b1d(i)
            do k = j,15
              ii = ii + 1
              ik = kb2(k,i)
              jk = kb2(k,j)
              t1ak = cos1a*cos3a1d(k) + cos3a*cos1a1d(k)
              t1bk = cos1b*cos3b1d(k) + cos3b*cos1b1d(k)
              t2aik = cos1a*cos3a2d(ik) + cos3a*cos1a2d(ik) + cos1a1d(i)*cos3a1d(k) + cos1a1d(k)*cos3a1d(i)
              t2ajk = cos1a*cos3a2d(jk) + cos3a*cos1a2d(jk) + cos1a1d(j)*cos3a1d(k) + cos1a1d(k)*cos3a1d(j)
              t2bik = cos1b*cos3b2d(ik) + cos3b*cos1b2d(ik) + cos1b1d(i)*cos3b1d(k) + cos1b1d(k)*cos3b1d(i)
              t2bjk = cos1b*cos3b2d(jk) + cos3b*cos1b2d(jk) + cos1b1d(j)*cos3b1d(k) + cos1b1d(k)*cos3b1d(j)
              top3a(ii) = - 2.0_dp*r12*r24*(cos1a*cos3a3d(ii) + cos3a*cos1a3d(ii) + cos3a1d(i)*cos1a2d(jk) +  &
                cos3a1d(j)*cos1a2d(ik) + cos3a1d(k)*cos1a2d(ij) + cos1a1d(i)*cos3a2d(jk) +  &
                cos1a1d(j)*cos3a2d(ik) + cos1a1d(k)*cos3a2d(ij))
              top3b(ii) = - 2.0_dp*r12*r16*(cos1b*cos3b3d(ii) + cos3b*cos1b3d(ii) + cos3b1d(i)*cos1b2d(jk) +  &
                cos3b1d(j)*cos1b2d(ik) + cos3b1d(k)*cos1b2d(ij) + cos1b1d(i)*cos3b2d(jk) +  &
                cos1b1d(j)*cos3b2d(ik) + cos1b1d(k)*cos3b2d(ij))
              if (i.eq.1) then
                top3a(ii) = top3a(ii) - 2.0_dp*r24*rr12*t2ajk
                top3b(ii) = top3b(ii) - 2.0_dp*r16*rr12*t2bjk
                if (j.eq.1) then
                  top3a(ii) = top3a(ii) + 2.0_dp*r24*rr122*rr12*t1ak
                  top3b(ii) = top3b(ii) + 2.0_dp*r16*rr122*rr12*t1bk
                  if (k.eq.1) then
                    top3a(ii) = top3a(ii) - 3.0_dp*r24*rr124*rr12*t0a
                    top3b(ii) = top3b(ii) - 3.0_dp*r16*rr124*rr12*t0b
                  elseif (k.eq.5) then
                    top3a(ii) = top3a(ii) + rr24*rr122*rr12*t0a
                    top3b(ii) = top3b(ii) + rr16*rr122*rr12*t0b
                  endif
                endif
                if (k.eq.1) then
                  top3a(ii) = top3a(ii) + 2.0_dp*r24*rr122*rr12*t1aj
                  top3b(ii) = top3b(ii) + 2.0_dp*r16*rr122*rr12*t1bj
                endif
                if (j.eq.5) then
                  top3a(ii) = top3a(ii) - 2.0_dp*rr12*rr24*t1ak
                  top3b(ii) = top3b(ii) - 2.0_dp*rr12*rr16*t1bk
                  if (k.eq.5) then
                    top3a(ii) = top3a(ii) + rr12*rr242*rr24*t0a
                    top3b(ii) = top3b(ii) + rr12*rr162*rr16*t0b
                  endif
                endif
                if (k.eq.5) then
                  top3a(ii) = top3a(ii) - 2.0_dp*rr12*rr24*t1aj
                  top3b(ii) = top3b(ii) - 2.0_dp*rr12*rr16*t1bj
                endif
              elseif (i.eq.5) then
                top3a(ii) = top3a(ii) - 2.0_dp*r12*rr24*t2ajk
                top3b(ii) = top3b(ii) - 2.0_dp*r12*rr16*t2bjk
                if (j.eq.5) then
                  top3a(ii) = top3a(ii) + 2.0_dp*r12*rr242*rr24*t1ak
                  top3b(ii) = top3b(ii) + 2.0_dp*r12*rr162*rr16*t1bk
                  if (k.eq.5) then
                    top3a(ii) = top3a(ii) - 3.0_dp*r12*rr244*rr24*t0a
                    top3b(ii) = top3b(ii) - 3.0_dp*r12*rr164*rr16*t0b
                  endif
                endif
                if (k.eq.5) then
                  top3a(ii) = top3a(ii) + 2.0_dp*r12*rr242*rr24*t1aj
                  top3b(ii) = top3b(ii) + 2.0_dp*r12*rr162*rr16*t1bj
                endif
              endif
              if (j.eq.1) then
                top3a(ii) = top3a(ii) - 2.0_dp*r24*rr12*t2aik
                top3b(ii) = top3b(ii) - 2.0_dp*r16*rr12*t2bik
                if (k.eq.1) then
                  top3a(ii) = top3a(ii) + 2.0_dp*r24*rr122*rr12*t1ai
                  top3b(ii) = top3b(ii) + 2.0_dp*r16*rr122*rr12*t1bi
                endif
                if (k.eq.5) then
                  top3a(ii) = top3a(ii) - 2.0_dp*rr12*rr24*t1ai
                  top3b(ii) = top3b(ii) - 2.0_dp*rr12*rr16*t1bi
                endif
              elseif (j.eq.5) then
                top3a(ii) = top3a(ii) - 2.0_dp*r12*rr24*t2aik
                top3b(ii) = top3b(ii) - 2.0_dp*r12*rr16*t2bik
                if (k.eq.5) then
                  top3a(ii) = top3a(ii) + 2.0_dp*r12*rr242*rr24*t1ai
                  top3b(ii) = top3b(ii) + 2.0_dp*r12*rr162*rr16*t1bi
                endif
              endif
              if (k.eq.1) then
                top3a(ii) = top3a(ii) - 2.0_dp*r24*rr12*t2aij
                top3b(ii) = top3b(ii) - 2.0_dp*r16*rr12*t2bij
              elseif (k.eq.5) then
                top3a(ii) = top3a(ii) - 2.0_dp*r12*rr24*t2aij
                top3b(ii) = top3b(ii) - 2.0_dp*r12*rr16*t2bij
              endif
            enddo
          enddo
        enddo
!
!  Bottom part
!
        b0a = sin1a*sin3a
        b0b = sin1b*sin3b
        ii = 0
        do i = 1,15
          b1ai = sin1a*sin3a1d(i) + sin3a*sin1a1d(i)
          b1bi = sin1b*sin3b1d(i) + sin3b*sin1b1d(i)
          do j = i,15
            ij = kb2(j,i)
            b1aj = sin1a*sin3a1d(j) + sin3a*sin1a1d(j)
            b1bj = sin1b*sin3b1d(j) + sin3b*sin1b1d(j)
            b2aij = sin1a*sin3a2d(ij) + sin3a*sin1a2d(ij) + sin1a1d(i)*sin3a1d(j) + sin1a1d(j)*sin3a1d(i)
            b2bij = sin1b*sin3b2d(ij) + sin3b*sin1b2d(ij) + sin1b1d(i)*sin3b1d(j) + sin1b1d(j)*sin3b1d(i)
            do k = j,15
              ii = ii + 1
              ik = kb2(k,i)
              jk = kb2(k,j)
              b1ak = sin1a*sin3a1d(k) + sin3a*sin1a1d(k)
              b1bk = sin1b*sin3b1d(k) + sin3b*sin1b1d(k)
              b2aik = sin1a*sin3a2d(ik) + sin3a*sin1a2d(ik) + sin1a1d(i)*sin3a1d(k) + sin1a1d(k)*sin3a1d(i)
              b2ajk = sin1a*sin3a2d(jk) + sin3a*sin1a2d(jk) + sin1a1d(j)*sin3a1d(k) + sin1a1d(k)*sin3a1d(j)
              b2bik = sin1b*sin3b2d(ik) + sin3b*sin1b2d(ik) + sin1b1d(i)*sin3b1d(k) + sin1b1d(k)*sin3b1d(i)
              b2bjk = sin1b*sin3b2d(jk) + sin3b*sin1b2d(jk) + sin1b1d(j)*sin3b1d(k) + sin1b1d(k)*sin3b1d(j)
!
              bot3a(ii) = r12*r24*(sin1a*sin3a3d(ii) + sin3a*sin1a3d(ii) + sin3a1d(i)*sin1a2d(jk) + sin3a1d(j)* &
                sin1a2d(ik) + sin3a1d(k)*sin1a2d(ij) + sin1a1d(i)*sin3a2d(jk) + sin1a1d(j)*sin3a2d(ik) +  &
                sin1a1d(k)*sin3a2d(ij))
              bot3b(ii) = r12*r16*(sin1b*sin3b3d(ii) + sin3b*sin1b3d(ii) + sin3b1d(i)*sin1b2d(jk) + sin3b1d(j)* &
                sin1b2d(ik) + sin3b1d(k)*sin1b2d(ij) + sin1b1d(i)*sin3b2d(jk) + sin1b1d(j)*sin3b2d(ik) +  &
                sin1b1d(k)*sin3b2d(ij))
              if (i.eq.1) then
                bot3a(ii) = bot3a(ii) + r24*rr12*b2ajk
                bot3b(ii) = bot3b(ii) + r16*rr12*b2bjk
                if (j.eq.1) then
                  bot3a(ii) = bot3a(ii) - r24*rr122*rr12*b1ak
                  bot3b(ii) = bot3b(ii) - r16*rr122*rr12*b1bk
                  if (k.eq.1) then
                    bot3a(ii) = bot3a(ii) + 3.0_dp*r24*rr124*rr12*b0a
                    bot3b(ii) = bot3b(ii) + 3.0_dp*r16*rr124*rr12*b0b
                  elseif (k.eq.5) then
                    bot3a(ii) = bot3a(ii) - rr24*rr122*rr12*b0a
                    bot3b(ii) = bot3b(ii) - rr16*rr122*rr12*b0b
                  endif
                endif
                if (k.eq.1) then
                  bot3a(ii) = bot3a(ii) - r24*rr122*rr12*b1aj
                  bot3b(ii) = bot3b(ii) - r16*rr122*rr12*b1bj
                endif
                if (j.eq.5) then
                  bot3a(ii) = bot3a(ii) + rr12*rr24*b1ak
                  bot3b(ii) = bot3b(ii) + rr12*rr16*b1bk
                  if (k.eq.5) then
                    bot3a(ii) = bot3a(ii) - rr12*rr242*rr24*b0a
                    bot3b(ii) = bot3b(ii) - rr12*rr162*rr16*b0b
                  endif
                endif
                if (k.eq.5) then
                  bot3a(ii) = bot3a(ii) + rr12*rr24*b1aj
                  bot3b(ii) = bot3b(ii) + rr12*rr16*b1bj
                endif
              elseif (i.eq.5) then
                bot3a(ii) = bot3a(ii) + r12*rr24*b2ajk
                bot3b(ii) = bot3b(ii) + r12*rr16*b2bjk
                if (j.eq.5) then
                  bot3a(ii) = bot3a(ii) - r12*rr242*rr24*b1ak
                  bot3b(ii) = bot3b(ii) - r12*rr162*rr16*b1bk
                  if (k.eq.5) then
                    bot3a(ii) = bot3a(ii) + 3.0_dp*r12*rr244*rr24*b0a
                    bot3b(ii) = bot3b(ii) + 3.0_dp*r12*rr164*rr16*b0b
                  endif
                endif
                if (k.eq.5) then
                  bot3a(ii) = bot3a(ii) - r12*rr242*rr24*b1aj
                  bot3b(ii) = bot3b(ii) - r12*rr162*rr16*b1bj
                endif
              endif
              if (j.eq.1) then
                bot3a(ii) = bot3a(ii) + r24*rr12*b2aik
                bot3b(ii) = bot3b(ii) + r16*rr12*b2bik
                if (k.eq.1) then
                  bot3a(ii) = bot3a(ii) - r24*rr122*rr12*b1ai
                  bot3b(ii) = bot3b(ii) - r16*rr122*rr12*b1bi
                endif
                if (k.eq.5) then
                  bot3a(ii) = bot3a(ii) + rr12*rr24*b1ai
                  bot3b(ii) = bot3b(ii) + rr12*rr16*b1bi
                endif
              elseif (j.eq.5) then
                bot3a(ii) = bot3a(ii) + r12*rr24*b2aik
                bot3b(ii) = bot3b(ii) + r12*rr16*b2bik
                if (k.eq.5) then
                  bot3a(ii) = bot3a(ii) - r12*rr242*rr24*b1ai
                  bot3b(ii) = bot3b(ii) - r12*rr162*rr16*b1bi
                endif
              endif
              if (k.eq.1) then
                bot3a(ii) = bot3a(ii) + r24*rr12*b2aij
                bot3b(ii) = bot3b(ii) + r16*rr12*b2bij
              elseif (k.eq.5) then
                bot3a(ii) = bot3a(ii) + r12*rr24*b2aij
                bot3b(ii) = bot3b(ii) + r12*rr16*b2bij
              endif
            enddo
          enddo
        enddo
!
!  Combine derivatives
!
        ii = 0
        do i = 1,15
          do j = i,15
            do k = j,15
              ii = ii + 1
              cosp3da(ii) = bot0a*bot0a*bot0a*(top1a(i)*bot1a(j)*bot1a(k)+top1a(j)*bot1a(i)*bot1a(k) +  &
                top1a(k)*bot1a(i)*bot1a(j)+top0a*(bot1a(i)*bot2a(kb2(k,j))+bot1a(j)*bot2a(kb2(k,i))+ &
                bot1a(k)*bot2a(kb2(j,i))))
              cosp3da(ii) = cosp3da(ii) - 0.5_dp*bot0a*bot0a*(bot1a(i)*top2a(kb2(k,j))+bot1a(j)*top2a(kb2(k,i))+bot1a(k)* &
                top2a(kb2(j,i))+top1a(i)*bot2a(kb2(k,j))+top1a(j)*bot2a(kb2(k,i))+top1a(k)*bot2a(kb2(j,i)))
              cosp3da(ii) = cosp3da(ii) - 3.0_dp*top0a*(bot0a**4)*bot1a(i)*bot1a(j)*bot1a(k)
!
              cosp3db(ii) = bot0b*bot0b*bot0b*(top1b(i)*bot1b(j)*bot1b(k)+top1b(j)*bot1b(i)*bot1b(k) +  &
                top1b(k)*bot1b(i)*bot1b(j)+top0b*(bot1b(i)*bot2b(kb2(k,j))+bot1b(j)*bot2b(kb2(k,i))+ &
                bot1b(k)*bot2b(kb2(j,i))))
              cosp3db(ii) = cosp3db(ii) - 0.5_dp*bot0b*bot0b*(bot1b(i)*top2b(kb2(k,j))+bot1b(j)*top2b(kb2(k,i))+bot1b(k)* &
                top2b(kb2(j,i))+top1b(i)*bot2b(kb2(k,j))+top1b(j)*bot2b(kb2(k,i))+top1b(k)*bot2b(kb2(j,i)))
              cosp3db(ii) = cosp3db(ii) - 3.0_dp*top0b*(bot0b**4)*bot1b(i)*bot1b(j)*bot1b(k)
            enddo
          enddo
        enddo
        do i = 1,56
          cosp3da(i) = cosp3da(i) + 0.5_dp*bot0a*(top3a(i)-bot0a*top0a*bot3a(i))
          cosp3db(i) = cosp3db(i) + 0.5_dp*bot0b*(top3b(i)-bot0b*top0b*bot3b(i))
        enddo
      endif
    endif
  endif
!******************************
!  Potential dependent terms  *
!******************************
  if (n6ty.eq.1) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Cross - out of plane potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ctrm1a = (1.0_dp - cos1a*cos1a)
    ctrm2a = (1.0_dp - cosphia*cosphia)
    ctrm1b = (1.0_dp - cos1b*cos1b)
    ctrm2b = (1.0_dp - cosphib*cosphib)
    rctrm1a = sqrt(ctrm1a)
    rctrm2a = sqrt(ctrm2a)
    rctrm1b = sqrt(ctrm1b)
    rctrm2b = sqrt(ctrm2b)
    oopda = r12*rctrm1a*rctrm2a
    oopdb = r12*rctrm1b*rctrm2b
    esix = rksix*oopda*oopdb
    if (lgrad1) then
!
!  First derivatives
!
      rrctrm1a = 1.0_dp/ctrm1a
      rrctrm2a = 1.0_dp/ctrm2a
      rrctrm1b = 1.0_dp/ctrm1b
      rrctrm2b = 1.0_dp/ctrm2b
      do k = 1,15
        rctrm1ad1(k) = - rctrm1a*cos1a*rrctrm1a*cos1a1d(k)
        rctrm2ad1(k) = - rctrm2a*cosphia*rrctrm2a*cosp1da(k)
        oopd1a(k) = r12*(rctrm1ad1(k)*rctrm2a + rctrm1a*rctrm2ad1(k))
        rctrm1bd1(k) = - rctrm1b*cos1b*rrctrm1b*cos1b1d(k)
        rctrm2bd1(k) = - rctrm2b*cosphib*rrctrm2b*cosp1db(k)
        oopd1b(k) = r12*(rctrm1bd1(k)*rctrm2b + rctrm1b*rctrm2bd1(k))
      enddo
      oopd1a(1) = oopd1a(1) + oopda*rr122
      oopd1b(1) = oopd1b(1) + oopdb*rr122
      do k = 1,15
        e1d(k) = rksix*(oopd1a(k)*oopdb + oopda*oopd1b(k))
      enddo
      if (lgrad2) then
!
!  Second derivatives
!
        ii = 0
        do i = 1,15
          do j = i,15
            ii = ii + 1
            rctrm1ad2(ii) = - rctrm1a*rrctrm1a*(cos1a*cos1a2d(ii) + cos1a1d(i)*cos1a1d(j))
            rctrm1ad2(ii) = rctrm1ad2(ii) - rctrm1a*((rrctrm1a*cos1a)**2)*cos1a1d(i)*cos1a1d(j)
            rctrm1bd2(ii) = - rctrm1b*rrctrm1b*(cos1b*cos1b2d(ii) + cos1b1d(i)*cos1b1d(j))
            rctrm1bd2(ii) = rctrm1bd2(ii) - rctrm1b*((rrctrm1b*cos1b)**2)*cos1b1d(i)*cos1b1d(j)
            rctrm2ad2(ii) = - rctrm2a*rrctrm2a*(cosphia*cosp2da(ii) + cosp1da(i)*cosp1da(j))
            rctrm2ad2(ii) = rctrm2ad2(ii) - rctrm2a*((rrctrm2a*cosphia)**2)*cosp1da(i)*cosp1da(j)
            rctrm2bd2(ii) = - rctrm2b*rrctrm2b*(cosphib*cosp2db(ii) + cosp1db(i)*cosp1db(j))
            rctrm2bd2(ii) = rctrm2bd2(ii) - rctrm2b*((rrctrm2b*cosphib)**2)*cosp1db(i)*cosp1db(j)
            oopd2a(ii) = r12*(rctrm1ad2(ii)*rctrm2a + rctrm1a*rctrm2ad2(ii))
            oopd2a(ii) = oopd2a(ii) + r12*(rctrm1ad1(i)*rctrm2ad1(j) + rctrm1ad1(j)*rctrm2ad1(i))
            oopd2b(ii) = r12*(rctrm1bd2(ii)*rctrm2b + rctrm1b*rctrm2bd2(ii))
            oopd2b(ii) = oopd2b(ii) + r12*(rctrm1bd1(i)*rctrm2bd1(j) + rctrm1bd1(j)*rctrm2bd1(i))
            if (i.eq.1) then
              oopd2a(ii) = oopd2a(ii) + rr12*(rctrm1ad1(j)*rctrm2a + rctrm1a*rctrm2ad1(j))
              oopd2b(ii) = oopd2b(ii) + rr12*(rctrm1bd1(j)*rctrm2b + rctrm1b*rctrm2bd1(j))
            endif
            if (j.eq.1) then
              oopd2a(ii) = oopd2a(ii) + rr12*(rctrm1ad1(i)*rctrm2a + rctrm1a*rctrm2ad1(i))
              oopd2b(ii) = oopd2b(ii) + rr12*(rctrm1bd1(i)*rctrm2b + rctrm1b*rctrm2bd1(i))
            endif
          enddo
        enddo
        oopd2a(1) = oopd2a(1) - oopda*rr124
        oopd2b(1) = oopd2b(1) - oopdb*rr124
        ii = 0
        do i = 1,15
          do j = i,15
            ii = ii + 1
            e2d(ii) = rksix*(oopd2a(ii)*oopdb + oopda*oopd2b(ii))
            e2d(ii) = e2d(ii) + rksix*(oopd1a(i)*oopd1b(j) + oopd1a(j)*oopd1b(i))
          enddo
        enddo
        if (lgrad3) then
!
!  Third derivatives
!
!          ii = 0
!          do i = 1,15
!            do j = i,15
!              ij = kb2(j,i)
!              do k = j,15
!                ii = ii + 1
!                ik = kb2(k,i)
!                jk = kb2(k,j)
!                e3d(ii) = rksix*(oopd3a(ii)*oopdb + oopda*oopd3b(ii))
!                e3d(ii) = e3d(ii) + rksix*(oopd2a(jk)*oopd1b(i) + oopd1a(i)*oopd2b(jk))
!                e3d(ii) = e3d(ii) + rksix*(oopd2a(ik)*oopd1b(j) + oopd1a(j)*oopd2b(ik))
!                e3d(ii) = e3d(ii) + rksix*(oopd2a(ij)*oopd1b(k) + oopd1a(k)*oopd2b(ij))
!              enddo
!            enddo
!          enddo
        endif
      endif
    endif
  endif
!
  return
  end
