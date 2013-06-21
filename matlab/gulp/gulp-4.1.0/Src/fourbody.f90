  subroutine fourbody(nptr4,n4ty,r12,r13,r14,r23,r24,r34,efor,e1d,e2d,e3d,rkfor,rn,phi0in, &
    isgnin,fpoly,lgrad1,lgrad2,lgrad3)
!
!  Calculates four-body potential first, second and third derivatives with respect to the 
!  six interatomic distances that make the four-body term.
!
!   6/98 Created from threebody and adapted
!   7/98 Fpoly added to hold the occupancy weight polynomial constants
!   8/98 Sine derivatives introduced to make cosphi derivatives easier
!   8/98 Out of plane potential now included in this routine
!   8/98 Phi0.ne.0 handled : when phi=0 derivatives are zero for 1st
!        and indeterminate for second
!   8/98 ESFF torsional potential added
!   7/02 K4 added for outofplane potential
!  10/02 Torharm potential added
!   4/04 Exponentially decaying torsion added, including ESFF form
!   4/04 Tapered torsion added
!  11/04 Torangle potential added
!  11/04 Theta terms added
!  10/05 Inversion out of plane potential added 
!  10/05 Calculation of cosphi and derivatives moved to subroutine
!   6/06 Inversion squared potential added
!  10/06 Modified so that angle in torsion leads to forces being skipped rather than an error
!   1/07 UFF4 added
!   4/07 If cosines of angles are ill-defined then action changed to setting energy to zero
!        and returning
!  10/07 Angle-angle cross potential added
!   5/08 UFFoop potential added
!   5/08 UFFoop potential corrected
!   5/08 Sign of cospsi terms corrected for UFF inversion potential
!  11/08 Third derivatives of theta3 added for case where potential number = 14
!  11/08 xcosangleangle potential added
!  11/08 torcosangle potential added
!   2/10 Out of plane derivatives rewritten and simplified to fix bug in k4 third derivatives
!   4/13 Test on whether phi is too small & so potential underflow trap is needed changed to 
!        be a test on sinphi in order to handle periodicity.
!
!  nptr4   = pointer to potential number
!  n4ty    = pointer to type of four-body potential
!  r12     = distance between atoms 1 and 2
!  r13     = distance between atoms 1 and 3
!  r14     = distance between atoms 1 and 4
!  r23     = distance between atoms 2 and 3
!  r24     = distance between atoms 2 and 4
!  r34     = distance between atoms 3 and 4
!  efor    = contribution to four-body energy
!  e1d     = array of first derivative terms
!  e2d     = array of second derivative terms
!  e3d     = array of third derivative terms
!  rkfor   = first parameter associated with potential type
!  phi0    = second parameter associated with potential type
!  isgn    = sign of cosine term, if appropriate
!  fpoly   = polynomial coefficients
!  rn      = third parameter associated with potential type
!  lgrad1  = if .true. calculate the first derivatives
!  lgrad2  = if .true. calculate the second derivatives
!  lgrad3  = if .true. calculate the third derivatives
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, April 2013
!
  use constants
  use control
  use general, only : nwarn
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4) :: isgnin
  integer(i4) :: n4ty
  integer(i4) :: nptr4
  logical     :: lgrad1
  logical     :: lgrad2
  logical     :: lgrad3
  real(dp)    :: e1d(6)
  real(dp)    :: e2d(21)
  real(dp)    :: e3d(56)
  real(dp)    :: efor
  real(dp)    :: fpoly(*)
  real(dp)    :: phi0in
  real(dp)    :: r12
  real(dp)    :: r13
  real(dp)    :: r14
  real(dp)    :: r23
  real(dp)    :: r24
  real(dp)    :: r34
  real(dp)    :: rkfor
  real(dp)    :: rn
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: ij
  integer(i4) :: ik
  integer(i4) :: isgn
  integer(i4) :: j
  integer(i4) :: jk
  integer(i4) :: k
  integer(i4) :: kb(6,6)
  integer(i4) :: nmax
  integer(i4) :: norder
  logical     :: lgrad1loc
  logical     :: lgrad2loc
  logical     :: lgrad3loc
  logical     :: lk4
  logical     :: loldbehaviour
  logical     :: lphi0non0
  logical     :: lphieq0
  real(dp)    :: c0
  real(dp)    :: c1
  real(dp)    :: c2
  real(dp)    :: cos1
  real(dp)    :: cos2
  real(dp)    :: cos3
  real(dp)    :: cosk0
  real(dp)    :: cosphi
  real(dp)    :: cosphi1
  real(dp)    :: cosphi2
  real(dp)    :: cosphi3
  real(dp)    :: cos11d(6),cos21d(6),cos31d(6),cosp1d(6)
  real(dp)    :: cos12d(21),cos22d(21),cos32d(21),cosp2d(21)
  real(dp)    :: cos13d(56),cos23d(56),cos33d(56),cosp3d(56)
  real(dp)    :: cosp1d1(6)
  real(dp)    :: cosp1d2(6)
  real(dp)    :: cosp1d3(6)
  real(dp)    :: cosp2d1(21)
  real(dp)    :: cosp2d2(21)
  real(dp)    :: cosp2d3(21)
  real(dp)    :: cosp3d1(56)
  real(dp)    :: cosp3d2(56)
  real(dp)    :: cosp3d3(56)
  real(dp)    :: cosd1
  real(dp)    :: cosd2
  real(dp)    :: cosd3
  real(dp)    :: cosmphi
  real(dp)    :: cosnphi
  real(dp)    :: cosphim
  real(dp)    :: cospsi1
  real(dp)    :: cospsi1d1(6)
  real(dp)    :: cospsi1d2(21)
  real(dp)    :: cospsi1d3(56)
  real(dp)    :: cos2psi1
  real(dp)    :: cos2psi1d1(6)
  real(dp)    :: cos2psi1d2(21)
  real(dp)    :: cos2psi1d3(56)
  real(dp)    :: cospsi2
  real(dp)    :: cospsi2d1(6)
  real(dp)    :: cospsi2d2(21)
  real(dp)    :: cospsi2d3(56)
  real(dp)    :: cos2psi2
  real(dp)    :: cos2psi2d1(6)
  real(dp)    :: cos2psi2d2(21)
  real(dp)    :: cos2psi2d3(56)
  real(dp)    :: cospsi3
  real(dp)    :: cospsi3d1(6)
  real(dp)    :: cospsi3d2(21)
  real(dp)    :: cospsi3d3(56)
  real(dp)    :: cos2psi3
  real(dp)    :: cos2psi3d1(6)
  real(dp)    :: cos2psi3d2(21)
  real(dp)    :: cos2psi3d3(56)
  real(dp)    :: cphi0
  real(dp)    :: cphi1
  real(dp)    :: cphi2
  real(dp)    :: cphi3
  real(dp)    :: ctrm1
  real(dp)    :: ctrm1d1(6)
  real(dp)    :: ctrm1d2(21)
  real(dp)    :: ctrm1d3(56)
  real(dp)    :: ctrm12d1(6)
  real(dp)    :: ctrm12d2(21)
  real(dp)    :: ctrm12d3(56)
  real(dp)    :: ctrm2
  real(dp)    :: ctrm2d1(6)
  real(dp)    :: ctrm2d2(21)
  real(dp)    :: ctrm2d3(56)
  real(dp)    :: ctrm22d1(6)
  real(dp)    :: ctrm22d2(21)
  real(dp)    :: ctrm22d3(56)
  real(dp)    :: ctrm11
  real(dp)    :: ctrm21
  real(dp)    :: ctrm31
  real(dp)    :: ctrm12
  real(dp)    :: ctrm22
  real(dp)    :: ctrm32
  real(dp)    :: delth1
  real(dp)    :: delth2
  real(dp)    :: dsinrat
  real(dp)    :: dsinratio
  real(dp)    :: e0a
  real(dp)    :: e0b
  real(dp)    :: e1
  real(dp)    :: e1p0
  real(dp)    :: e2
  real(dp)    :: e2ab1
  real(dp)    :: e2ab2
  real(dp)    :: e2ab3
  real(dp)    :: e2ab4
  real(dp)    :: e2ab5
  real(dp)    :: e2p0
  real(dp)    :: e3
  real(dp)    :: e3ab1
  real(dp)    :: e3ab2
  real(dp)    :: e3ab3
  real(dp)    :: e3ab4
  real(dp)    :: e3p0
  real(dp)    :: ee0
  real(dp)    :: ee1d(6)
  real(dp)    :: ee2d(21)
  real(dp)    :: ee3d(56)
  real(dp)    :: ee1dnoee0(6)
  real(dp)    :: ee2dnoee0(21)
  real(dp)    :: ep0
  real(dp)    :: et0
  real(dp)    :: et1d(6)
  real(dp)    :: et2d(21)
  real(dp)    :: et3d(56)
  real(dp)    :: f12
  real(dp)    :: f23
  real(dp)    :: f34
  real(dp)    :: fp
  real(dp)    :: df12dr
  real(dp)    :: df23dr
  real(dp)    :: df34dr
  real(dp)    :: d2f12dr2
  real(dp)    :: d2f23dr2
  real(dp)    :: d2f34dr2
  real(dp)    :: d3f12dr3
  real(dp)    :: d3f23dr3
  real(dp)    :: d3f34dr3
  real(dp)    :: dsinrat2
  real(dp)    :: k0
  real(dp)    :: k1
  real(dp)    :: k2
  real(dp)    :: k3
  real(dp)    :: phi
  real(dp)    :: phi0
  real(dp)    :: r122
  real(dp)    :: r132
  real(dp)    :: r142
  real(dp)    :: r232
  real(dp)    :: r242
  real(dp)    :: r342
  real(dp)    :: rcospsi1
  real(dp)    :: rcospsi2
  real(dp)    :: rcospsi3
  real(dp)    :: rho12
  real(dp)    :: rho23
  real(dp)    :: rho34
  real(dp)    :: rkfor4
  real(dp)    :: rkover3
  real(dp)    :: rmax12
  real(dp)    :: rmax23
  real(dp)    :: rmax34
  real(dp)    :: rr12
  real(dp)    :: rr122
  real(dp)    :: rr124
  real(dp)    :: rr13
  real(dp)    :: rr132
  real(dp)    :: rr134
  real(dp)    :: rr14
  real(dp)    :: rr142
  real(dp)    :: rr144
  real(dp)    :: rr23
  real(dp)    :: rr232
  real(dp)    :: rr234
  real(dp)    :: rr24
  real(dp)    :: rr242
  real(dp)    :: rr244
  real(dp)    :: rr34
  real(dp)    :: rr342
  real(dp)    :: rr344
  real(dp)    :: rsin1
  real(dp)    :: rsin2
  real(dp)    :: rsin3
  real(dp)    :: rsinp
  real(dp)    :: rsinphi
  real(dp)    :: rsinphi2
  real(dp)    :: rsinth1
  real(dp)    :: rsinth12
  real(dp)    :: rsinth2
  real(dp)    :: rsinth22
  real(dp)    :: rsinth3
  real(dp)    :: rsinth32
  real(dp)    :: rtan1
  real(dp)    :: rtan2
  real(dp)    :: rtan3
  real(dp)    :: sin11d(6),sin21d(6),sin31d(6)
  real(dp)    :: sin12d(21),sin22d(21),sin32d(21)
  real(dp)    :: sin13d(56),sin23d(56),sin33d(56)
  real(dp)    :: sin1
  real(dp)    :: sin1n
  real(dp)    :: sin1nm2
  real(dp)    :: sin1nm3
  real(dp)    :: sin2
  real(dp)    :: sin3
  real(dp)    :: sin3n
  real(dp)    :: sin3nm2
  real(dp)    :: sin3nm3
  real(dp)    :: sinnphi
  real(dp)    :: sinphi
  real(dp)    :: sinrat
  real(dp)    :: sinratio
  real(dp)    :: sinth12
  real(dp)    :: sinth22
  real(dp)    :: sinth32
  real(dp)    :: sphi0
  real(dp)    :: phi1d(6),phi2d(21),phi3d(56)
  real(dp)    :: theta0_1
  real(dp)    :: theta0_2
  real(dp)    :: theta0_3
  real(dp)    :: theta1
  real(dp)    :: theta2
  real(dp)    :: theta3
  real(dp)    :: theta11d(6)
  real(dp)    :: theta12d(21)
  real(dp)    :: theta13d(56)
  real(dp)    :: theta21d(6)
  real(dp)    :: theta22d(21)
  real(dp)    :: theta23d(56)
  real(dp)    :: theta31d(6)
  real(dp)    :: theta32d(21)
  real(dp)    :: theta33d(56)
  real(dp)    :: ttrm1
  real(dp)    :: ttrm2
  real(dp)    :: ttrm3
  real(dp)    :: trm1
  real(dp)    :: trm2
  real(dp)    :: trm3
  real(dp)    :: trm3a
  real(dp)    :: trm3b
!
  data kb/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17, &
          18,5,10,14,17,19,20,6,11,15,18,20,21/
  isgn = isgnin
  phi0 = phi0in
!
!  Zero terms
!
  efor = 0.0_dp
  if (lgrad1) then
    do i = 1,6
      e1d(i) = 0.0_dp
      cos11d(i) = 0.0_dp
      cos21d(i) = 0.0_dp
      cos31d(i) = 0.0_dp
      cosp1d(i) = 0.0_dp
      sin11d(i) = 0.0_dp
      sin21d(i) = 0.0_dp
      sin31d(i) = 0.0_dp
    enddo
    if (lgrad2) then
      do i = 1,21
        e2d(i) = 0.0_dp
        cos12d(i) = 0.0_dp
        cos22d(i) = 0.0_dp
        cos32d(i) = 0.0_dp
        cosp2d(i) = 0.0_dp
        sin12d(i) = 0.0_dp
        sin22d(i) = 0.0_dp
        sin32d(i) = 0.0_dp
      enddo
      if (lgrad3) then
        do i = 1,56
          e3d(i) = 0.0_dp
          cos13d(i) = 0.0_dp
          cos23d(i) = 0.0_dp
          cos33d(i) = 0.0_dp
          cosp3d(i) = 0.0_dp
          sin13d(i) = 0.0_dp
          sin23d(i) = 0.0_dp
          sin33d(i) = 0.0_dp
        enddo
      endif
    endif
  endif
!
!  Set up local constants
!
  r122 = r12*r12
  r132 = r13*r13
  r142 = r14*r14
  r232 = r23*r23
  r242 = r24*r24
  r342 = r34*r34
  rr12 = 1.0_dp/r12
  rr13 = 1.0_dp/r13
  rr14 = 1.0_dp/r14
  rr23 = 1.0_dp/r23
  rr24 = 1.0_dp/r24
  rr34 = 1.0_dp/r34
  rr122 = rr12*rr12
  rr132 = rr13*rr13
  rr142 = rr14*rr14
  rr232 = rr23*rr23
  rr242 = rr24*rr24
  rr342 = rr34*rr34
  if (lgrad2) then
    rr124 = rr122*rr122
    rr134 = rr132*rr132
    rr144 = rr142*rr142
    rr234 = rr232*rr232
    rr244 = rr242*rr242
    rr344 = rr342*rr342
  endif
!
!  Set local derivative flags
!
  loldbehaviour = .false.
  lgrad1loc = lgrad1
  lgrad2loc = lgrad2
  lgrad3loc = lgrad3
!***************************************
!  Set up potential independent terms  *
!***************************************
!$$$$$$$$$$$$$$$$$
!  Cosine terms  $
!$$$$$$$$$$$$$$$$$
  if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.14.or.n4ty.eq.15.or.n4ty.eq.16) then
!
!  For inversion potential we need different angles
!
!  Cosine theta 1 = 2-1-3, 2 = 2-1-4 and 3 = 3-1-4
!
    cos1 = 0.5_dp*(r122+r132-r232)/(r12*r13)
    cos2 = 0.5_dp*(r122+r142-r242)/(r12*r14)
    cos3 = 0.5_dp*(r132+r142-r342)/(r13*r14)
  else
!
!  Cosine theta 1 = 1-2-3, 2 = 2-3-4 and 3 = 3-2-4
!
    cos1 = 0.5_dp*(r232+r122-r132)/(r12*r23)
    cos2 = 0.5_dp*(r232+r342-r242)/(r23*r34)
    cos3 = 0.5_dp*(r232+r242-r342)/(r23*r24)
  endif
!
!  Check for angles which are 0 or 180 degrees with potentials
!  which cannot cope with these. For those that can the four-
!  body contribution must go to zero when the angle approaches
!  this limit - hence we can just return having set all the
!  derivatives and energy to zero
!
  if (abs(cos1).ge.0.99999999_dp) then
    if (n4ty.eq.4.or.n4ty.eq.7.or.n4ty.eq.9) return
    if (loldbehaviour) then
      call outerror('Torsional angle has become 0/180 degrees',0_i4)
      if (ioproc) then
        write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
        write(ioout,'(''!!  Torsional potential = '',i3,/)') nptr4
        write(ioout,'(''!!  Problem angle = 1-2-3'',/)')
        write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
      endif
      call stopnow('fourbody')
    else
      lgrad1loc = .false.
      lgrad2loc = .false.
      lgrad3loc = .false.
    endif
!
! Energy can be unstable when torsion is undefined so return
!
    efor = 0.0_dp
    return
  endif
  if (abs(cos2).ge.0.99999999_dp) then
    if (n4ty.eq.4.or.n4ty.eq.7.or.n4ty.eq.9) return
    if (loldbehaviour) then
      call outerror('Torsional angle has become 0/180 degrees',0_i4)
      if (ioproc) then
        write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
        write(ioout,'(''!!  Torsional potential = '',i3,/)') nptr4
        write(ioout,'(''!!  Problem angle = 2-3-4'',/)')
        write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
      endif
      call stopnow('fourbody')
    else
      lgrad1loc = .false.
      lgrad2loc = .false.
      lgrad3loc = .false.
    endif
!
! Energy can be unstable when torsion is undefined so return
!
    efor = 0.0_dp
    return
  endif
  if (lgrad1loc) then
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r14
!  4 = r23
!  5 = r24
!  6 = r34
!
    if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.14.or.n4ty.eq.15.or.n4ty.eq.16) then
      cos11d(1) = rr12*rr13 - cos1*rr122
      cos11d(2) = rr12*rr13 - cos1*rr132
      cos11d(4) = - rr12*rr13
!
      cos21d(1) = rr12*rr14 - cos2*rr122
      cos21d(3) = rr12*rr14 - cos2*rr142
      cos21d(5) = - rr12*rr14
!
      cos31d(2) = rr13*rr14 - cos3*rr132
      cos31d(3) = rr13*rr14 - cos3*rr142
      cos31d(6) = - rr13*rr14
    else
      cos11d(1) = rr12*rr23 - cos1*rr122
      cos11d(2) = - rr12*rr23
      cos11d(4) = rr12*rr23 - cos1*rr232
!
      cos21d(4) = rr23*rr34 - cos2*rr232
      cos21d(5) = - rr23*rr34
      cos21d(6) = rr23*rr34 - cos2*rr342
!
      cos31d(4) = rr23*rr24 - cos3*rr232
      cos31d(5) = rr23*rr24 - cos3*rr242
      cos31d(6) = - rr23*rr24
    endif
    if (lgrad2loc) then
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
      if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.14.or.n4ty.eq.15.or.n4ty.eq.16) then
        cos12d(1) = - 2.0_dp*rr122*rr12*rr13 + 3.0_dp*cos1*rr124
        cos12d(2) = rr12*rr13*(cos1*rr12*rr13 - rr122 - rr132)
        cos12d(4) = rr122*rr12*rr13
        cos12d(7) = - 2.0_dp*rr132*rr13*rr12 + 3.0_dp*cos1*rr134
        cos12d(9) = rr132*rr13*rr12
!
        cos22d(1)  = - 2.0_dp*rr122*rr12*rr14 + 3.0_dp*cos2*rr124
        cos22d(3)  = rr12*rr14*(cos2*rr12*rr14 - rr122 - rr142)
        cos22d(5)  = rr122*rr12*rr14
        cos22d(12) = - 2.0_dp*rr142*rr14*rr12 + 3.0_dp*cos2*rr144
        cos22d(14) = rr142*rr14*rr12
!
        cos32d(7)  = - 2.0_dp*rr132*rr13*rr14 + 3.0_dp*cos3*rr134
        cos32d(8)  = rr13*rr14*(cos3*rr13*rr14 - rr132 - rr142)
        cos32d(11) = rr132*rr13*rr14
        cos32d(12) = - 2.0_dp*rr142*rr14*rr13 + 3.0_dp*cos3*rr144
        cos32d(15) = rr142*rr14*rr13
      else
        cos12d(1) = - 2.0_dp*rr122*rr12*rr23 + 3.0_dp*cos1*rr124
        cos12d(2) = rr122*rr12*rr23
        cos12d(4) = rr12*rr23*(cos1*rr12*rr23 - rr122 - rr232)
        cos12d(9) = rr232*rr23*rr12
        cos12d(16) = - 2.0_dp*rr232*rr23*rr12 + 3.0_dp*cos1*rr234
!
        cos22d(16) = - 2.0_dp*rr232*rr23*rr34 + 3.0_dp*cos2*rr234
        cos22d(17) = rr232*rr23*rr34
        cos22d(18) = rr23*rr34*(cos2*rr23*rr34 - rr232 - rr342)
        cos22d(20) = rr342*rr34*rr23
        cos22d(21) = - 2.0_dp*rr342*rr34*rr23 + 3.0_dp*cos2*rr344
!
        cos32d(16) = - 2.0_dp*rr232*rr23*rr24 + 3.0_dp*cos3*rr234
        cos32d(17) = rr23*rr24*(cos3*rr23*rr24 - rr232 - rr242)
        cos32d(18) = rr232*rr23*rr24
        cos32d(19) = - 2.0_dp*rr242*rr24*rr23 + 3.0_dp*cos3*rr244
        cos32d(20) = rr242*rr24*rr23
      endif
!
      if (lgrad3loc) then
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
        if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.14.or.n4ty.eq.15.or.n4ty.eq.16) then
          cos13d(1) = rr124*(9.0_dp*rr12*rr13-15.0_dp*cos1*rr122)
          cos13d(2) = rr122*(2.0_dp*rr12*rr132*rr13+3.0_dp*rr122*rr12*rr13-3.0_dp*cos1*rr122*rr132)
          cos13d(4) = -3.0_dp*rr124*rr12*rr13
          cos13d(7) = rr132*(2.0_dp*rr13*rr122*rr12+3.0_dp*rr132*rr13*rr12-3.0_dp*cos1*rr132*rr122)
          cos13d(9) = -rr122*rr12*rr132*rr13
          cos13d(22) = rr134*(9.0_dp*rr13*rr12-15.0_dp*cos1*rr132)
          cos13d(24) = -3.0_dp*rr134*rr13*rr12
!
          cos23d(1)  = rr124*(9.0_dp*rr12*rr14-15.0_dp*cos2*rr122)
          cos23d(3)  = rr122*(2.0_dp*rr12*rr142*rr14+3.0_dp*rr122*rr12*rr14-3.0_dp*cos2*rr122*rr142)
          cos23d(5)  = -3.0_dp*rr124*rr12*rr14
          cos23d(12) = rr142*(2.0_dp*rr14*rr122*rr12+3.0_dp*rr142*rr14*rr12-3.0_dp*cos2*rr142*rr122)
          cos23d(14) = -rr122*rr12*rr142*rr14
          cos23d(37) = rr144*(9.0_dp*rr14*rr12-15.0_dp*cos2*rr142)
          cos23d(39) = -3.0_dp*rr144*rr14*rr12
!
          cos33d(22) = rr134*(9.0_dp*rr13*rr14-15.0_dp*cos3*rr132)
          cos33d(23) = rr132*(2.0_dp*rr13*rr142*rr14+3.0_dp*rr132*rr13*rr14-3.0_dp*cos3*rr132*rr142)
          cos33d(26) = -3.0_dp*rr134*rr13*rr14
          cos33d(27) = rr142*(2.0_dp*rr14*rr132*rr13+3.0_dp*rr142*rr14*rr13-3.0_dp*cos3*rr142*rr132)
          cos33d(30) = -rr132*rr13*rr142*rr14
          cos33d(37) = rr144*(9.0_dp*rr14*rr13-15.0_dp*cos3*rr142)
          cos33d(40) = -3.0_dp*rr144*rr14*rr13
        else
          cos13d(1) = rr124*(9.0_dp*rr12*rr23-15.0_dp*cos1*rr122)
          cos13d(2) = -3.0_dp*rr124*rr12*rr23
          cos13d(4) = rr122*(2.0_dp*rr12*rr232*rr23+3.0_dp*rr122*rr12*rr23-3.0_dp*cos1*rr122*rr232)
          cos13d(9) = -rr122*rr12*rr232*rr23
          cos13d(16) = rr232*(2.0_dp*rr23*rr122*rr12+3.0_dp*rr232*rr23*rr12-3.0_dp*cos1*rr232*rr122)
          cos13d(31) = -3.0_dp*rr234*rr23*rr12
          cos13d(47) = rr234*(9.0_dp*rr23*rr12-15.0_dp*cos1*rr232)
!
          cos23d(47) = rr234*(9.0_dp*rr23*rr34-15.0_dp*cos2*rr232)
          cos23d(48) = -3.0_dp*rr234*rr23*rr34
          cos23d(49) = rr232*(2.0_dp*rr23*rr342*rr34+3.0_dp*rr232*rr23*rr34-3.0_dp*cos2*rr232*rr342)
          cos23d(51) = -rr232*rr23*rr342*rr34
          cos23d(52) = rr342*(2.0_dp*rr34*rr232*rr23+3.0_dp*rr342*rr34*rr23-3.0_dp*cos2*rr342*rr232)
          cos23d(55) = -3.0_dp*rr344*rr34*rr23
          cos23d(56) = rr344*(9.0_dp*rr34*rr23-15.0_dp*cos2*rr342)
!
          cos33d(47) = rr234*(9.0_dp*rr23*rr24-15.0_dp*cos3*rr232)
          cos33d(48) = rr232*(2.0_dp*rr23*rr242*rr24+3.0_dp*rr232*rr23*rr24-3.0_dp*cos3*rr232*rr242)
          cos33d(49) = -3.0_dp*rr234*rr23*rr24
          cos33d(50) = rr242*(2.0_dp*rr24*rr232*rr23+3.0_dp*rr242*rr24*rr23-3.0_dp*cos3*rr242*rr232)
          cos33d(51) = -rr232*rr23*rr242*rr24
          cos33d(53) = rr244*(9.0_dp*rr24*rr23-15.0_dp*cos3*rr242)
          cos33d(54) = -3.0_dp*rr244*rr24*rr23
        endif
      endif
    endif
  endif
!$$$$$$$$$$$$$$$
!  Sine terms  $
!$$$$$$$$$$$$$$$
  sin1 = sqrt(1.0_dp - cos1*cos1)
  sin3 = sqrt(1.0_dp - cos3*cos3)
  rsin1 = 1.0_dp/sin1
  rsin3 = 1.0_dp/sin3
  if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.15) then
    sin2 = sqrt(1.0_dp - cos2*cos2)
    rsin2 = 1.0_dp/sin2
  endif
  if (lgrad1loc) then
!
!  First derivatives
!
    rtan1 = cos1*rsin1
    rtan3 = cos3*rsin3
    do i = 1,6
      sin11d(i) = - rtan1*cos11d(i)
      sin31d(i) = - rtan3*cos31d(i)
    enddo
    if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.15) then
      rtan2 = cos2*rsin2
      do i = 1,6
        sin21d(i) = - rtan2*cos21d(i)
      enddo
    endif
    if (lgrad2loc) then
!
!  Second derivatives
!
      if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.15) then
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            sin12d(ii) = - rtan1*cos12d(ii) - rsin1*cos11d(i)*cos11d(j)*(1.0_dp + rtan1*rtan1)
            sin22d(ii) = - rtan2*cos22d(ii) - rsin2*cos21d(i)*cos21d(j)*(1.0_dp + rtan2*rtan2)
            sin32d(ii) = - rtan3*cos32d(ii) - rsin3*cos31d(i)*cos31d(j)*(1.0_dp + rtan3*rtan3)
          enddo
        enddo
      else
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            sin12d(ii) = - rtan1*cos12d(ii) - rsin1*cos11d(i)*cos11d(j)*(1.0_dp + rtan1*rtan1)
            sin32d(ii) = - rtan3*cos32d(ii) - rsin3*cos31d(i)*cos31d(j)*(1.0_dp + rtan3*rtan3)
          enddo
        enddo
      endif
      if (lgrad3loc) then
!
!  Third derivatives
!
        if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.15) then
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                sin13d(ii) = - rtan1*cos13d(ii) - rsin1*(1.0_dp+rtan1*rtan1)*(cos11d(i)*cos12d(jk) +  &
                  cos11d(j)*cos12d(ik) + cos11d(k)*cos12d(ij)) - cos11d(i)*cos11d(j)*cos11d(k)*rtan1* &
                  rsin1*rsin1*3.0_dp*(1.0_dp+rtan1*rtan1)
                sin23d(ii) = - rtan2*cos23d(ii) - rsin2*(1.0_dp+rtan2*rtan2)*(cos21d(i)*cos22d(jk) +  &
                  cos21d(j)*cos22d(ik) + cos21d(k)*cos22d(ij)) - cos21d(i)*cos21d(j)*cos21d(k)*rtan2* &
                  rsin2*rsin2*3.0_dp*(1.0_dp+rtan2*rtan2)
                sin33d(ii) = - rtan3*cos33d(ii) - rsin3*(1.0_dp+rtan3*rtan3)*(cos31d(i)*cos32d(jk) +  &
                  cos31d(j)*cos32d(ik) + cos31d(k)*cos32d(ij)) - cos31d(i)*cos31d(j)*cos31d(k)*rtan3* &
                  rsin3*rsin3*3.0_dp*(1.0_dp+rtan3*rtan3)
              enddo
            enddo
          enddo
        else
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                sin13d(ii) = - rtan1*cos13d(ii) - rsin1*(1.0_dp+rtan1*rtan1)*(cos11d(i)*cos12d(jk) +  &
                  cos11d(j)*cos12d(ik) + cos11d(k)*cos12d(ij)) - cos11d(i)*cos11d(j)*cos11d(k)*rtan1* &
                  rsin1*rsin1*3.0_dp*(1.0_dp+rtan1*rtan1)
                sin23d(ii) = - rtan2*cos23d(ii) - rsin2*(1.0_dp+rtan2*rtan2)*(cos21d(i)*cos22d(jk) +  &
                  cos21d(j)*cos22d(ik) + cos21d(k)*cos22d(ij)) - cos21d(i)*cos21d(j)*cos21d(k)*rtan2* &
                  rsin2*rsin2*3.0_dp*(1.0_dp+rtan2*rtan2)
                sin33d(ii) = - rtan3*cos33d(ii) - rsin3*(1.0_dp+rtan3*rtan3)*(cos31d(i)*cos32d(jk) +  &
                  cos31d(j)*cos32d(ik) + cos31d(k)*cos32d(ij)) - cos31d(i)*cos31d(j)*cos31d(k)*rtan3* &
                  rsin3*rsin3*3.0_dp*(1.0_dp+rtan3*rtan3)
              enddo
            enddo
          enddo
        endif
      endif
    endif
  endif
!$$$$$$$$$$$$$$
!  Phi terms  $
!$$$$$$$$$$$$$$
  if (n4ty.eq.11.or.n4ty.eq.12.or.n4ty.eq.15) then
    call getphi(r12,r24,r14,1_i4,5_i4,3_i4,cos1,cos3,sin1,sin3,cos11d,cos31d,sin11d,sin31d, &
                cos12d,cos32d,sin12d,sin32d,cos13d,cos33d,sin13d,sin33d,cosphi1,cosp1d1, &
                cosp2d1,cosp3d1,lgrad1loc,lgrad2loc,lgrad3loc)
    call getphi(r13,r34,r14,2_i4,6_i4,3_i4,cos1,cos2,sin1,sin2,cos11d,cos21d,sin11d,sin21d, &
                cos12d,cos22d,sin12d,sin22d,cos13d,cos23d,sin13d,sin23d,cosphi2,cosp1d2, &
                cosp2d2,cosp3d2,lgrad1loc,lgrad2loc,lgrad3loc)
    call getphi(r14,r34,r13,3_i4,6_i4,2_i4,cos2,cos1,sin2,sin1,cos21d,cos11d,sin21d,sin11d, &
                cos22d,cos12d,sin22d,sin12d,cos23d,cos13d,sin23d,sin13d,cosphi3,cosp1d3, &
                cosp2d3,cosp3d3,lgrad1loc,lgrad2loc,lgrad3loc)
  elseif (n4ty.ne.14.and.n4ty.ne.16) then
    call getphi(r12,r14,r24,1_i4,3_i4,5_i4,cos1,cos3,sin1,sin3,cos11d,cos31d,sin11d,sin31d, &
                cos12d,cos32d,sin12d,sin32d,cos13d,cos33d,sin13d,sin33d,cosphi,cosp1d, &
                cosp2d,cosp3d,lgrad1loc,lgrad2loc,lgrad3loc)
  endif
  if (n4ty.eq.10.or.n4ty.eq.14) then
!$$$$$$$$$$$$$$$$
!  Theta terms  $
!$$$$$$$$$$$$$$$$
    theta1 = acos(cos1)
    theta2 = acos(cos2)
    if (n4ty.eq.14) then
      theta3 = acos(cos3)
    endif
    if (lgrad1loc) then
!
!  Calculate inverse sin(theta) as this forms part of d(theta)/dr
!
      sinth12 = 1.0_dp - cos1**2
      rsinth12 = 1.0_dp/sinth12
      rsinth1 = sqrt(rsinth12)
!
      sinth22 = 1.0_dp - cos2**2
      rsinth22 = 1.0_dp/sinth22
      rsinth2 = sqrt(rsinth22)
!
      if (n4ty.eq.14) then
        sinth32 = 1.0_dp - cos3**2
        rsinth32 = 1.0_dp/sinth32
        rsinth3 = sqrt(rsinth32)
      endif
!
!  First
!
      theta11d(1:6) = 0.0_dp
      theta21d(1:6) = 0.0_dp
!
      theta11d(1) = - cos11d(1)*rsinth1
      theta11d(2) = - cos11d(2)*rsinth1
      theta11d(4) = - cos11d(4)*rsinth1
!
      if (n4ty.eq.14) then
        theta21d(1) = - cos21d(1)*rsinth2
        theta21d(3) = - cos21d(3)*rsinth2
        theta21d(5) = - cos21d(5)*rsinth2
!
        theta31d(1:6) = 0.0_dp
!
        theta31d(2) = - cos31d(2)*rsinth3
        theta31d(3) = - cos31d(3)*rsinth3
        theta31d(6) = - cos31d(6)*rsinth3
      else
        theta21d(4) = - cos21d(4)*rsinth2
        theta21d(5) = - cos21d(5)*rsinth2
        theta21d(6) = - cos21d(6)*rsinth2
      endif
!
      if (lgrad2loc) then
!
!  Second
!
        theta12d(1:21) = 0.0_dp
        theta22d(1:21) = 0.0_dp
!
        trm1 = cos1*rsinth12
        theta12d(1) = trm1*cos11d(1)*theta11d(1) - rsinth1*cos12d(1)
        theta12d(2) = trm1*cos11d(1)*theta11d(2) - rsinth1*cos12d(2)
        theta12d(4) = trm1*cos11d(1)*theta11d(4) - rsinth1*cos12d(4)
        theta12d(7) = trm1*cos11d(2)*theta11d(2) - rsinth1*cos12d(7)
        theta12d(9) = trm1*cos11d(2)*theta11d(4) - rsinth1*cos12d(9)
        theta12d(16) = trm1*cos11d(4)*theta11d(4) - rsinth1*cos12d(16)
!
        trm2 = cos2*rsinth22
        if (n4ty.eq.14) then
          theta32d(1:21) = 0.0_dp
!
          theta22d(1)  = trm2*cos21d(1)*theta21d(1) - rsinth2*cos22d(1)
          theta22d(3)  = trm2*cos21d(1)*theta21d(3) - rsinth2*cos22d(3)
          theta22d(5)  = trm2*cos21d(1)*theta21d(5) - rsinth2*cos22d(5)
          theta22d(12) = trm2*cos21d(3)*theta21d(3) - rsinth2*cos22d(12)
          theta22d(14) = trm2*cos21d(3)*theta21d(5) - rsinth2*cos22d(14)
          theta22d(19) = trm2*cos21d(5)*theta21d(5) - rsinth2*cos22d(19)
!
          trm3 = cos3*rsinth32
          theta32d(7)  = trm3*cos31d(2)*theta31d(2) - rsinth3*cos32d(7)
          theta32d(8)  = trm3*cos31d(2)*theta31d(3) - rsinth3*cos32d(8)
          theta32d(11) = trm3*cos31d(2)*theta31d(6) - rsinth3*cos32d(11)
          theta32d(12) = trm3*cos31d(3)*theta31d(3) - rsinth3*cos32d(12)
          theta32d(15) = trm3*cos31d(3)*theta31d(6) - rsinth3*cos32d(15)
          theta32d(21) = trm3*cos31d(6)*theta31d(6) - rsinth3*cos32d(21)
        else
          theta22d(16) = trm2*cos21d(4)*theta21d(4) - rsinth2*cos22d(16)
          theta22d(17) = trm2*cos21d(4)*theta21d(5) - rsinth2*cos22d(17)
          theta22d(18) = trm2*cos21d(4)*theta21d(6) - rsinth2*cos22d(18)
          theta22d(19) = trm2*cos21d(5)*theta21d(5) - rsinth2*cos22d(19)
          theta22d(20) = trm2*cos21d(5)*theta21d(6) - rsinth2*cos22d(20)
          theta22d(21) = trm2*cos21d(6)*theta21d(6) - rsinth2*cos22d(21)
        endif
!
        if (lgrad3loc) then
!
!  Third
!
          theta13d(1:56) = 0.0_dp
          theta23d(1:56) = 0.0_dp
!
          theta13d(1) =  - rsinth1*cos13d(1) + trm1*theta11d(1)*cos12d(1) &
                         + trm1*theta11d(1)*cos12d(1) + trm1*theta12d(1)*cos11d(1) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(1)*theta11d(1)*theta11d(1) &
                         - rsinth1*cos11d(1)*theta11d(1)*theta11d(1)
          theta13d(2) =  - rsinth1*cos13d(2) + trm1*theta11d(2)*cos12d(1) &
                         + trm1*theta11d(1)*cos12d(2) + trm1*theta12d(2)*cos11d(1) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(1)*theta11d(1)*theta11d(2) &
                         - rsinth1*cos11d(1)*theta11d(1)*theta11d(2)
          theta13d(4) =  - rsinth1*cos13d(4) + trm1*theta11d(4)*cos12d(1) &
                         + trm1*theta11d(1)*cos12d(4) + trm1*theta12d(3)*cos11d(1) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(1)*theta11d(1)*theta11d(4) &
                         - rsinth1*cos11d(1)*theta11d(1)*theta11d(4)
          theta13d(7) =  - rsinth1*cos13d(7) + trm1*theta11d(2)*cos12d(2) &
                         + trm1*theta11d(2)*cos12d(2) + trm1*theta12d(7)*cos11d(1) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(1)*theta11d(2)*theta11d(2) &
                         - rsinth1*cos11d(1)*theta11d(2)*theta11d(2)
          theta13d(9) =  - rsinth1*cos13d(9) + trm1*theta11d(4)*cos12d(2) &
                         + trm1*theta11d(2)*cos12d(4) + trm1*theta12d(9)*cos11d(1) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(1)*theta11d(2)*theta11d(4) &
                         - rsinth1*cos11d(1)*theta11d(2)*theta11d(4)
          theta13d(16) = - rsinth1*cos13d(16) + trm1*theta11d(4)*cos12d(4) &
                         + trm1*theta11d(4)*cos12d(4) + trm1*theta12d(16)*cos11d(1) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(1)*theta11d(4)*theta11d(4) &
                         - rsinth1*cos11d(1)*theta11d(4)*theta11d(4)
          theta13d(22) = - rsinth1*cos13d(22) + trm1*theta11d(2)*cos12d(7) &
                         + trm1*theta11d(2)*cos12d(7) + trm1*theta12d(7)*cos11d(2) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(2)*theta11d(2)*theta11d(2) &
                         - rsinth1*cos11d(2)*theta11d(2)*theta11d(2)
          theta13d(24) = - rsinth1*cos13d(24) + trm1*theta11d(4)*cos12d(7) &
                         + trm1*theta11d(2)*cos12d(9) + trm1*theta12d(9)*cos11d(2) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(2)*theta11d(2)*theta11d(4) &
                         - rsinth1*cos11d(2)*theta11d(2)*theta11d(4)
          theta13d(31) = - rsinth1*cos13d(31) + trm1*theta11d(4)*cos12d(9) &
                         + trm1*theta11d(4)*cos12d(9) + trm1*theta12d(16)*cos11d(2) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(2)*theta11d(4)*theta11d(4) &
                         - rsinth1*cos11d(2)*theta11d(4)*theta11d(4)
          theta13d(47) = - rsinth1*cos13d(47) + trm1*theta11d(4)*cos12d(16) &
                         + trm1*theta11d(4)*cos12d(16) + trm1*theta12d(16)*cos11d(4) &
                         - 2.0_dp*cos1*rsinth1*trm1*cos11d(4)*theta11d(4)*theta11d(4) &
                         - rsinth1*cos11d(4)*theta11d(4)*theta11d(4)
!
          if (n4ty.eq.14) then
            theta33d(1:56) = 0.0_dp
!
            theta23d(1)  = - rsinth2*cos23d(1) + trm2*theta21d(1)*cos22d(1) &
                           + trm2*theta21d(1)*cos22d(1) + trm2*theta22d(1)*cos21d(1) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(1)*theta21d(1)*theta21d(1) &
                           - rsinth2*cos21d(1)*theta21d(1)*theta21d(1)
            theta23d(3)  = - rsinth2*cos23d(3) + trm2*theta21d(1)*cos22d(3) &
                           + trm2*theta21d(3)*cos22d(1) + trm2*theta22d(1)*cos21d(3) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(1)*theta21d(3)*theta21d(1) &
                           - rsinth2*cos21d(3)*theta21d(1)*theta21d(1)
            theta23d(5)  = - rsinth2*cos23d(5) + trm2*theta21d(1)*cos22d(5) &
                           + trm2*theta21d(5)*cos22d(1) + trm2*theta22d(1)*cos21d(5) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(1)*theta21d(1)*theta21d(5) &
                           - rsinth2*cos21d(5)*theta21d(1)*theta21d(1)
            theta23d(12) = - rsinth2*cos23d(12) + trm2*theta21d(5)*cos22d(17) &
                           + trm2*theta21d(5)*cos22d(17) + trm2*theta22d(19)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(5)*theta21d(5) &
                           - rsinth2*cos21d(1)*theta21d(3)*theta21d(5)
            theta23d(14) = - rsinth2*cos23d(14) + trm2*theta21d(6)*cos22d(17) &
                           + trm2*theta21d(5)*cos22d(18) + trm2*theta22d(20)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(5)*theta21d(6) &
                           - rsinth2*cos21d(4)*theta21d(5)*theta21d(6)
            theta23d(19) = - rsinth2*cos23d(19) + trm2*theta21d(6)*cos22d(18) &
                           + trm2*theta21d(6)*cos22d(18) + trm2*theta22d(21)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(6)*theta21d(6) &
                           - rsinth2*cos21d(4)*theta21d(6)*theta21d(6)
            theta23d(37) = - rsinth2*cos23d(37) + trm2*theta21d(5)*cos22d(19) &
                           + trm2*theta21d(5)*cos22d(19) + trm2*theta22d(19)*cos21d(5) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(5)*theta21d(5)*theta21d(5) &
                           - rsinth2*cos21d(5)*theta21d(5)*theta21d(5)
            theta23d(39) = - rsinth2*cos23d(39) + trm2*theta21d(6)*cos22d(19) &
                           + trm2*theta21d(5)*cos22d(20) + trm2*theta22d(20)*cos21d(5) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(5)*theta21d(5)*theta21d(6) &
                           - rsinth2*cos21d(5)*theta21d(5)*theta21d(6)
            theta23d(44) = - rsinth2*cos23d(44) + trm2*theta21d(6)*cos22d(20) &
                           + trm2*theta21d(6)*cos22d(20) + trm2*theta22d(21)*cos21d(5) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(5)*theta21d(6)*theta21d(6) &
                           - rsinth2*cos21d(5)*theta21d(6)*theta21d(6)
            theta23d(53) = - rsinth2*cos23d(53) + trm2*theta21d(6)*cos22d(21) &
                           + trm2*theta21d(6)*cos22d(21) + trm2*theta22d(21)*cos21d(6) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(6)*theta21d(6)*theta21d(6) &
                           - rsinth2*cos21d(6)*theta21d(6)*theta21d(6)
!
            trm3a = trm3*rsinth3
            trm3b = rsinth3*(3.0_dp*trm3*trm3 + rsinth32)
!
            theta33d(22) = - rsinth3*cos33d(22) - trm3b*cos31d(2)*cos31d(2)*cos31d(2) &
                           - 3.0_dp*trm3a*cos31d(2)*cos32d(7)
            theta33d(23) = - rsinth3*cos33d(23) - trm3b*cos31d(2)*cos31d(2)*cos31d(3) &
                           - trm3a*(2.0_dp*cos31d(2)*cos32d(8)+cos31d(3)*cos32d(7))
            theta33d(26) = - rsinth3*cos33d(26) - trm3b*cos31d(2)*cos31d(2)*cos31d(6) &
                           - trm3a*(2.0_dp*cos31d(2)*cos32d(11)+cos31d(6)*cos32d(7))
            theta33d(27) = - rsinth3*cos33d(27) - trm3b*cos31d(2)*cos31d(3)*cos31d(3) &
                           - trm3a*(2.0_dp*cos31d(3)*cos32d(8)+cos31d(2)*cos32d(12))
            theta33d(30) = - rsinth3*cos33d(30) - trm3b*cos31d(2)*cos31d(3)*cos31d(6) &
                           - trm3a*(cos31d(2)*cos32d(15)+cos31d(3)*cos32d(11)+cos31d(6)*cos32d(8))
            theta33d(36) = - rsinth3*cos33d(36) - trm3b*cos31d(2)*cos31d(6)*cos31d(6) &
                           - trm3a*(2.0_dp*cos31d(6)*cos32d(11)+cos31d(2)*cos32d(21))
            theta33d(37) = - rsinth3*cos33d(37) - trm3b*cos31d(3)*cos31d(3)*cos31d(3) &
                           - 3.0_dp*trm3a*cos31d(3)*cos32d(12)
            theta33d(40) = - rsinth3*cos33d(40) - trm3b*cos31d(3)*cos31d(3)*cos31d(6) &
                           - trm3a*(2.0_dp*cos31d(3)*cos32d(15)+cos31d(6)*cos32d(12))
            theta33d(46) = - rsinth3*cos33d(46) - trm3b*cos31d(3)*cos31d(6)*cos31d(6) &
                           - trm3a*(2.0_dp*cos31d(6)*cos32d(15)+cos31d(3)*cos32d(21))
            theta33d(56) = - rsinth3*cos33d(56) - trm3b*cos31d(6)*cos31d(6)*cos31d(6) &
                           - 3.0_dp*trm3a*cos31d(6)*cos32d(21)
          else
            theta23d(47) = - rsinth2*cos23d(47) + trm2*theta21d(4)*cos22d(16) &
                           + trm2*theta21d(4)*cos22d(16) + trm2*theta22d(16)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(4)*theta21d(4) &
                           - rsinth2*cos21d(4)*theta21d(4)*theta21d(4)
            theta23d(48) = - rsinth2*cos23d(48) + trm2*theta21d(5)*cos22d(16) &
                           + trm2*theta21d(4)*cos22d(17) + trm2*theta22d(17)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(4)*theta21d(5) &
                           - rsinth2*cos21d(4)*theta21d(4)*theta21d(5)
            theta23d(49) = - rsinth2*cos23d(49) + trm2*theta21d(6)*cos22d(16) &
                           + trm2*theta21d(4)*cos22d(18) + trm2*theta22d(18)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(4)*theta21d(6) &
                           - rsinth2*cos21d(4)*theta21d(4)*theta21d(6)
            theta23d(50) = - rsinth2*cos23d(50) + trm2*theta21d(5)*cos22d(17) &
                           + trm2*theta21d(5)*cos22d(17) + trm2*theta22d(19)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(5)*theta21d(5) &
                           - rsinth2*cos21d(4)*theta21d(5)*theta21d(5)
            theta23d(51) = - rsinth2*cos23d(51) + trm2*theta21d(6)*cos22d(17) &
                           + trm2*theta21d(5)*cos22d(18) + trm2*theta22d(20)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(5)*theta21d(6) &
                           - rsinth2*cos21d(4)*theta21d(5)*theta21d(6)
            theta23d(52) = - rsinth2*cos23d(52) + trm2*theta21d(6)*cos22d(18) &
                           + trm2*theta21d(6)*cos22d(18) + trm2*theta22d(21)*cos21d(4) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(4)*theta21d(6)*theta21d(6) &
                           - rsinth2*cos21d(4)*theta21d(6)*theta21d(6)
            theta23d(53) = - rsinth2*cos23d(53) + trm2*theta21d(5)*cos22d(19) &
                           + trm2*theta21d(5)*cos22d(19) + trm2*theta22d(19)*cos21d(5) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(5)*theta21d(5)*theta21d(5) &
                           - rsinth2*cos21d(5)*theta21d(5)*theta21d(5)
            theta23d(54) = - rsinth2*cos23d(54) + trm2*theta21d(6)*cos22d(19) &
                           + trm2*theta21d(5)*cos22d(20) + trm2*theta22d(20)*cos21d(5) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(5)*theta21d(5)*theta21d(6) &
                           - rsinth2*cos21d(5)*theta21d(5)*theta21d(6)
            theta23d(55) = - rsinth2*cos23d(55) + trm2*theta21d(6)*cos22d(20) &
                           + trm2*theta21d(6)*cos22d(20) + trm2*theta22d(21)*cos21d(5) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(5)*theta21d(6)*theta21d(6) &
                           - rsinth2*cos21d(5)*theta21d(6)*theta21d(6)
            theta23d(56) = - rsinth2*cos23d(56) + trm2*theta21d(6)*cos22d(21) &
                           + trm2*theta21d(6)*cos22d(21) + trm2*theta22d(21)*cos21d(6) &
                           - 2.0_dp*cos2*rsinth2*trm2*cos21d(6)*theta21d(6)*theta21d(6) &
                           - rsinth2*cos21d(6)*theta21d(6)*theta21d(6)
          endif
        endif
      endif
    endif
  endif
!******************************
!  Potential dependent terms  *
!******************************
  if (n4ty.eq.1) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Standard torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    phi = acos(cosphi)
    lphi0non0 = (abs(phi0).gt.1.0d-5)
    if (lphi0non0) then
      cphi0 = cos(phi0)
      sphi0 = sin(phi0)
      if (cphi0.lt.-0.99999_dp) then
        lphi0non0 = .false.
        isgn = - isgn
        cphi0 = 1.0_dp
        sphi0 = 0.0_dp
      endif
    else
      cphi0 = 1.0_dp
      sphi0 = 0.0_dp
    endif
    cosnphi = cos(rn*phi)
    efor = rkfor*(1.0_dp + isgn*cosnphi*cphi0)
    if (lphi0non0) then
      sinnphi = sin(rn*phi)
      sinphi = sin(phi)
      efor = efor + rkfor*isgn*sinnphi*sphi0
!
!  Trap phi0 ne 0, phi = 0 case
!
      if (abs(phi).lt.1.0d-6) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''  **** Warning - phi is zero with non-zero phi0 => derivative problem ****'',/)')
        endif
        return
      endif
    endif
    if (lgrad1loc) then
!
!  First derivatives of energy
!
!  d(cosnphi)/dcosphi = sinnphi/sinphi
!
!  to avoid divide by zero error when sinphi = 0 we use a series
!  expansion in cosphi
!
      e1 = rkfor*isgn*rn*cphi0
      e2 = e1
      e3 = e1
      nmax = nint(rn)
      cosd1 = sinratio(nmax,phi)
      e1 = e1*cosd1
      do k = 1,6
        e1d(k) = e1*cosp1d(k)
      enddo
      if (lphi0non0) then
!
!  Phi0 contribution to first derivatives
!
        rsinp = 1.0_dp/sinphi
        ep0 = rkfor*isgn*rn*sphi0
        e1p0 = ep0*cosnphi*rsinp
        do k = 1,6
          e1d(k) = e1d(k) - e1p0*cosp1d(k)
        enddo
      endif
      if (lgrad2loc) then
!
!  Second derivatives of energy
!
        cosd2 = dsinratio(nmax,phi)
        e2 = e2*cosd2
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2*cosp1d(i)*cosp1d(j) + e1*cosp2d(ii)
          enddo
        enddo
        if (lphi0non0) then
!
!  Phi0 contribution to second derivatives
!
          e2p0 = ep0*rsinp*rsinp*(rn*sinnphi+cosnphi*cosphi*rsinp)
          ii = 0
          do i = 1,6
            do j = i,6
              ii = ii + 1
              e2d(ii) = e2d(ii) - e1p0*cosp2d(ii) - e2p0*cosp1d(i)*cosp1d(j)
            enddo
          enddo
        endif
        if (lgrad3loc) then
!
!  Third derivatives of energy
!
          cosd3 = 0.0_dp
          if (nmax.gt.1) then
            cosd3 = dble(nmax-1)*dsinratio((nmax-1_i4),phi)
            cosd3 = cosd3 + dble(nmax-2)*cosphi*dsinratio((nmax-2_i4),phi)
            cosd3 = cosd3 + 2.0_dp*dble(nmax-2)*sinratio((nmax-2_i4),phi)
            cosphim = 1.0_dp
            do k = 3,nmax
              cosmphi = cos(dble(nmax-k)*phi)
              cosd3 = cosd3 + dble((k-2)*(k-1))*cosphim*cosmphi
              cosphim = cosphim*cosphi
              cosd3 = cosd3 + dble(2*(k-1)*(nmax-k))*cosphim*sinratio((nmax-k),phi)
              cosd3 = cosd3 + dble(nmax-k)*cosphim*cosphi*dsinratio((nmax-k),phi)
            enddo
            e3 = e3*cosd3
          endif
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = e3*cosp1d(i)*cosp1d(j)*cosp1d(k)
                e3d(ii) = e3d(ii) + e1*cosp3d(ii)
                e3d(ii) = e3d(ii) + e2*cosp1d(i)*cosp2d(jk)
                e3d(ii) = e3d(ii) + e2*cosp1d(j)*cosp2d(ik)
                e3d(ii) = e3d(ii) + e2*cosp1d(k)*cosp2d(ij)
              enddo
            enddo
          enddo
          if (lphi0non0) then
!
!  Phi0 contribution to third derivatives
!
            e3p0 = ep0*rsinp*rsinp*rsinp*(3.0_dp*cosphi*rsinp*(rn*sinnphi+cosnphi*cosphi*rsinp)+cosnphi*(1.0_dp-rn*rn))
            ii = 0
            do i = 1,6
              do j = i,6
                ij = kb(j,i)
                do k = j,6
                  ik = kb(k,i)
                  jk = kb(k,j)
                  ii = ii + 1
                  e3d(ii) = e3d(ii) - e1p0*cosp3d(ii)
                  e3d(ii) = e3d(ii) - e2p0*(cosp1d(i)*cosp2d(jk) + cosp1d(j)*cosp2d(ik) + cosp1d(k)*cosp2d(ij))
                  e3d(ii) = e3d(ii) - e3p0*cosp1d(i)*cosp1d(j)*cosp1d(k)
                enddo
              enddo
            enddo
          endif
        endif
      endif
    endif
  elseif (n4ty.eq.2) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Ryckaert-Bellemanns potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  E = k + c1.cosphi + c2.(cosphi**2) + ..... cn.(cosphi**n)
!
    norder = nint(rn)
    efor = rkfor
    e1 = 0.0_dp
    e2 = 0.0_dp
    e3 = 0.0_dp
    cphi1 = 1.0_dp
    cphi2 = 1.0_dp
    cphi3 = 1.0_dp
    do i = 1,norder
      fp = fpoly(i)
      if (lgrad1loc) then
        e1 = e1 + dble(i)*fp*cphi1
        if (lgrad2loc.and.i.ge.2) then
          e2 = e2 + dble(i*(i-1))*fp*cphi2
          cphi2 = cphi2*cosphi
          if (lgrad3loc.and.i.ge.3) then
            e3 = e3 + dble(i*(i-1)*(i-2))*fp*cphi3
            cphi3 = cphi3*cosphi
          endif
        endif
      endif
      cphi1 = cphi1*cosphi
      efor = efor + fp*cphi1
    enddo
!
!  First derivatives
!
    if (lgrad1loc) then
      do k = 1,6
        e1d(k) = e1*cosp1d(k)
      enddo
      if (lgrad2loc) then
!
!  Second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2*cosp1d(i)*cosp1d(j) + e1*cosp2d(ii)
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives of energy
!
          ii = 0
          do i = 1,6
            do j = i,6
              do k = j,6
                ii = ii + 1
                e3d(ii) = e3*cosp1d(i)*cosp1d(j)*cosp1d(k)
                e3d(ii) = e3d(ii) + e1*cosp3d(ii)
                e3d(ii) = e3d(ii) + e2*cosp1d(i)*cosp2d(kb(k,j))
                e3d(ii) = e3d(ii) + e2*cosp1d(j)*cosp2d(kb(k,i))
                e3d(ii) = e3d(ii) + e2*cosp1d(k)*cosp2d(kb(j,i))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.3) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Out of plane potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
    lk4 = (abs(fpoly(1)).gt.1.0d-10)
    ctrm1 = (1.0_dp - cos1*cos1)
    ctrm2 = (1.0_dp - cosphi*cosphi)
    ctrm12 = ctrm1*ctrm1
    ctrm22 = ctrm2*ctrm2
    efor = rkfor*r122*ctrm1*ctrm2
    if (lk4) then
      rkfor4 = fpoly(1)
      efor = efor + rkfor4*r122*r122*ctrm12*ctrm22
    endif
    if (lgrad1loc) then
!
!  First derivatives of contributing terms
!
      do k = 1,6
        ctrm1d1(k) = - 2.0_dp*cos1*cos11d(k)
        ctrm2d1(k) = - 2.0_dp*cosphi*cosp1d(k)
      enddo
      if (lk4) then
        do k = 1,6
          ctrm12d1(k) = 2.0_dp*ctrm1*ctrm1d1(k)
          ctrm22d1(k) = 2.0_dp*ctrm2*ctrm2d1(k)
        enddo
      endif
!
!  First derivatives of energy
!
      do k = 1,6
        e1d(k) = rkfor*r122*(ctrm1*ctrm2d1(k) + ctrm1d1(k)*ctrm2)
      enddo
      e1d(1) = e1d(1) + 2.0_dp*rkfor*ctrm1*ctrm2
      if (lk4) then
        do k = 1,6
          e1d(k) = e1d(k) + rkfor4*r122*r122*(ctrm12d1(k)*ctrm22 + ctrm12*ctrm22d1(k))
        enddo
        e1d(1) = e1d(1) + 4.0_dp*rkfor4*r122*ctrm12*ctrm22
      endif
      if (lgrad2loc) then
!
!  Second derivatives of contributing terms
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            ctrm1d2(ii) = - 2.0_dp*(cos1*cos12d(ii) + cos11d(i)*cos11d(j))
            ctrm2d2(ii) = - 2.0_dp*(cosphi*cosp2d(ii) + cosp1d(i)*cosp1d(j))
          enddo
        enddo
        if (lk4) then
          ii = 0
          do i = 1,6
            do j = i,6
              ii = ii + 1
              ctrm12d2(ii) = 2.0_dp*(ctrm1d1(i)*ctrm1d1(j) + ctrm1*ctrm1d2(ii))
              ctrm22d2(ii) = 2.0_dp*(ctrm2d1(i)*ctrm2d1(j) + ctrm2*ctrm2d2(ii))
            enddo
          enddo
        endif
!
!  Second derivatives of energy
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = rkfor*r122*(ctrm1d2(ii)*ctrm2 + ctrm1*ctrm2d2(ii) + ctrm1d1(i)*ctrm2d1(j) + ctrm1d1(j)*ctrm2d1(i))
!
!  Delta terms on one distance
!
            if (i.eq.1) then
              e2d(ii) = e2d(ii) + 2.0_dp*rkfor*(ctrm1*ctrm2d1(j) + ctrm2*ctrm1d1(j))
            endif
            if (j.eq.1) then
              e2d(ii) = e2d(ii) + 2.0_dp*rkfor*(ctrm1*ctrm2d1(i) + ctrm2*ctrm1d1(i))
            endif
          enddo
        enddo
        if (lk4) then
          ii = 0
          do i = 1,6
            do j = i,6
              ii = ii + 1
              e2d(ii) = e2d(ii) + rkfor4*r122*r122*(ctrm12d2(ii)*ctrm22 + ctrm12*ctrm22d2(ii) + &
                                                    ctrm12d1(i)*ctrm22d1(j) + ctrm12d1(j)*ctrm22d1(i))
!
!  Delta terms on one distance
!
              if (i.eq.1) then
                e2d(ii) = e2d(ii) + 4.0_dp*rkfor4*r122*(ctrm12*ctrm22d1(j) + ctrm22*ctrm12d1(j))
              endif
              if (j.eq.1) then
                e2d(ii) = e2d(ii) + 4.0_dp*rkfor4*r122*(ctrm12*ctrm22d1(i) + ctrm22*ctrm12d1(i))
              endif
            enddo
          enddo
!
!  Delta term on two distances
!
          e2d(1) = e2d(1) + 8.0_dp*rkfor4*ctrm12*ctrm22
        endif
        if (lgrad3loc) then
!
!  Third derivatives of contributing terms
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                ctrm1d3(ii) = - 2.0_dp*(cos11d(i)*cos12d(jk) + cos11d(j)*cos12d(ik) + cos11d(k)*cos12d(ij) + cos1*cos13d(ii))
                ctrm2d3(ii) = - 2.0_dp*(cosp1d(i)*cosp2d(jk) + cosp1d(j)*cosp2d(ik) + cosp1d(k)*cosp2d(ij) + cosphi*cosp3d(ii))
              enddo
            enddo
          enddo
          if (lk4) then
            ii = 0
            do i = 1,6
              do j = i,6
                ij = kb(j,i)
                do k = j,6
                  ii = ii + 1
                  ik = kb(k,i)
                  jk = kb(k,j)
                  ctrm12d3(ii) = 2.0_dp*(ctrm1*ctrm1d3(ii) + ctrm1d1(i)*ctrm1d2(jk) + &
                                         ctrm1d1(j)*ctrm1d2(ik) + ctrm1d1(k)*ctrm1d2(ij))
                  ctrm22d3(ii) = 2.0_dp*(ctrm2*ctrm2d3(ii) + ctrm2d1(i)*ctrm2d2(jk) + &
                                         ctrm2d1(j)*ctrm2d2(ik) + ctrm2d1(k)*ctrm2d2(ij))
                enddo
              enddo
            enddo
          endif
!
!  Third derivatives of energy
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = rkfor*r12*r12*(ctrm1d3(ii)*ctrm2 + ctrm1*ctrm2d3(ii) + &
                                         ctrm1d1(i)*ctrm2d2(jk) + ctrm1d1(j)*ctrm2d2(ik) + ctrm1d1(k)*ctrm2d2(ij) + &
                                         ctrm2d1(i)*ctrm1d2(jk) + ctrm2d1(j)*ctrm1d2(ik) + ctrm2d1(k)*ctrm1d2(ij))
!
!  Delta terms on one distance
!
                if (i.eq.1) then
                  e3d(ii) = e3d(ii) + 2.0_dp*rkfor*(ctrm1d1(j)*ctrm2d1(k) + ctrm1d1(k)*ctrm2d1(j) + &
                                                    ctrm1d2(jk)*ctrm2 + ctrm1*ctrm2d2(jk))
                endif
                if (j.eq.1) then
                  e3d(ii) = e3d(ii) + 2.0_dp*rkfor*(ctrm1d1(i)*ctrm2d1(k) + ctrm1d1(k)*ctrm2d1(i) + &
                                                    ctrm1d2(ik)*ctrm2 + ctrm1*ctrm2d2(ik))
                endif
                if (k.eq.1) then
                  e3d(ii) = e3d(ii) + 2.0_dp*rkfor*(ctrm1d1(i)*ctrm2d1(j) + ctrm1d1(j)*ctrm2d1(i) + &
                                                    ctrm1d2(ij)*ctrm2 + ctrm1*ctrm2d2(ij))
                endif
              enddo
            enddo
          enddo
          if (lk4) then
            ii = 0
            do i = 1,6
              do j = i,6
                ij = kb(j,i)
                do k = j,6
                  ii = ii + 1
                  ik = kb(k,i)
                  jk = kb(k,j)
                  e3d(ii) = e3d(ii) + rkfor4*r122*r122*(ctrm12d3(ii)*ctrm22 + ctrm12*ctrm22d3(ii) + &
                                      ctrm12d1(i)*ctrm22d2(jk) + ctrm12d1(j)*ctrm22d2(ik) + ctrm12d1(k)*ctrm22d2(ij) + &
                                      ctrm22d1(i)*ctrm12d2(jk) + ctrm22d1(j)*ctrm12d2(ik) + ctrm22d1(k)*ctrm12d2(ij))
!
!  Delta terms on one distance
!
                  if (i.eq.1) then
                    e3d(ii) = e3d(ii) + 4.0_dp*rkfor4*r122*(ctrm12d1(j)*ctrm22d1(k) + ctrm12d1(k)*ctrm22d1(j) + &
                                                            ctrm12d2(jk)*ctrm22 + ctrm12*ctrm22d2(jk))
                  endif
                  if (j.eq.1) then
                    e3d(ii) = e3d(ii) + 4.0_dp*rkfor4*r122*(ctrm12d1(i)*ctrm22d1(k) + ctrm12d1(k)*ctrm22d1(i) + &
                                                            ctrm12d2(ik)*ctrm22 + ctrm12*ctrm22d2(ik))
                  endif
                  if (k.eq.1) then
                    e3d(ii) = e3d(ii) + 4.0_dp*rkfor4*r122*(ctrm12d1(i)*ctrm22d1(j) + ctrm12d1(j)*ctrm22d1(i) + &
                                                            ctrm12d2(ij)*ctrm22 + ctrm12*ctrm22d2(ij))
                  endif
!
!  Delta terms on two distances
!
                  if (i.eq.1.and.j.eq.1) then
                    e3d(ii) = e3d(ii) + 8.0_dp*rkfor4*(ctrm12*ctrm22d1(k) + ctrm12d1(k)*ctrm22)
                  endif
                  if (i.eq.1.and.k.eq.1) then
                    e3d(ii) = e3d(ii) + 8.0_dp*rkfor4*(ctrm12*ctrm22d1(j) + ctrm12d1(j)*ctrm22)
                  endif
                  if (j.eq.1.and.k.eq.1) then
                    e3d(ii) = e3d(ii) + 8.0_dp*rkfor4*(ctrm12*ctrm22d1(i) + ctrm12d1(i)*ctrm22)
                  endif
                enddo
              enddo
            enddo
          endif
        endif
      endif
    endif
  elseif (n4ty.eq.4) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  ESFF torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Create constants for first and second terms
!
!  first  term = k1
!  second term = k2*isgn
!
    phi = acos(cosphi)
    cosnphi = cos(rn*phi)
    e0a = rkfor
    e0b = phi0*isgn
    sin1nm2 = sin1**(rn-2.0_dp)
    sin3nm2 = sin3**(rn-2.0_dp)
    efor = sin1*sin1*sin3*sin3*(e0a + e0b*sin1nm2*sin3nm2*cosnphi)
    if (lgrad1loc) then
!
!  First derivatives
!
      nmax = nint(rn)
      sinrat = sinratio(nmax,phi)
      sin1n = sin1nm2*sin1*sin1
      sin3n = sin3nm2*sin3*sin3
      do k = 1,6
        e1d(k) = rn*e0b*(sin1n*sin3n*sinrat*cosp1d(k) + cosnphi* &
          (sin1nm2*sin1*sin3n*sin11d(k) + sin3nm2*sin3*sin1n*sin31d(k)))
        e1d(k) = e1d(k) + 2.0_dp*e0a*(sin1*sin3*sin3*sin11d(k) + sin3*sin1*sin1*sin31d(k))
      enddo
      if (lgrad2loc) then
!
!  Second derivatives
!
        dsinrat = dsinratio(nmax,phi)
        e2ab1 = (4.0_dp*e0a+rn*rn*e0b*cosnphi*sin1nm2*sin3nm2)*sin1*sin3
        e2ab2 = (2.0_dp*e0a+rn*e0b*sin1nm2*sin3nm2*cosnphi)*sin1*sin3
        e2ab3 = (2.0_dp*e0a+rn*(rn-1.0_dp)*e0b*cosnphi*sin1nm2*sin3nm2)
        e2ab4 = rn*e0b*sin1n*sin3n
        e2ab5 = rn*rn*e0b*sinrat*sin1nm2*sin1*sin3*sin3nm2
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2ab1*(sin11d(i)*sin31d(j)+sin11d(j)*sin31d(i))
            e2d(ii) = e2d(ii) + e2ab2*(sin3*sin12d(ii)+sin1*sin32d(ii))
            e2d(ii) = e2d(ii) + e2ab3*(sin3*sin3*sin11d(i)*sin11d(j) + sin1*sin1*sin31d(i)*sin31d(j))
            e2d(ii) = e2d(ii) + e2ab4*(sinrat*cosp2d(ii) + dsinrat*cosp1d(i)*cosp1d(j))
            e2d(ii) = e2d(ii) + e2ab5*(sin3*cosp1d(i)*sin11d(j) + sin1*cosp1d(i)*sin31d(j) +  &
                               sin3*cosp1d(j)*sin11d(i)+sin1*cosp1d(j)*sin31d(i))
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives
!
!  Start by calculating second derivative of (sinnphi/sinphi) w.r.t.
!  cosphi while avoiding problems as phi -> zero
!
          dsinrat2 = 0.0_dp
          if (nmax.gt.1) then
            dsinrat2 = dble(nmax-1)*dsinratio((nmax-1_i4),phi)
            dsinrat2 = dsinrat2+dble(nmax-2)*cosphi*dsinratio((nmax-2_i4),phi)
            dsinrat2 = dsinrat2+2.0_dp*dble(nmax-2)*sinratio((nmax-2_i4),phi)
            cosphim = 1.0_dp
            do k = 3,nmax
              cosmphi = cos(dble(nmax-k)*phi)
              dsinrat2 = dsinrat2 + dble((k-2)*(k-1))*cosphim*cosmphi
              cosphim = cosphim*cosphi
              dsinrat2 = dsinrat2 + dble(2*(k-1)*(nmax-k))*cosphim*sinratio((nmax-k),phi)
              dsinrat2 = dsinrat2 + dble(nmax-k)*cosphim*cosphi*dsinratio((nmax-k),phi)
            enddo
          endif
          sin1nm3 = sin1nm2/sin1
          sin3nm3 = sin3nm2/sin3
          e3ab1 = (4.0_dp*e0a+rn*rn*(rn-1.0_dp)*e0b*cosnphi*sin1nm2*sin3nm2)
          e3ab2 = rn*(rn-1.0_dp)*(rn-2.0_dp)*e0b*cosnphi
          e3ab3 = rn*e0b*dsinrat*sin1nm2*sin3nm2*sin1*sin3
          e3ab4 = rn*rn*(rn-1.0_dp)*e0b*sinrat*sin1nm2*sin3nm2
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = e2ab2*(sin3*sin13d(ii)+sin1*sin33d(ii))
                e3d(ii) = e3d(ii) + e2ab1*(sin12d(ij)*sin31d(k)+ &
                  sin12d(ik)*sin31d(j)+sin12d(jk)*sin31d(i)+ &
                  sin32d(ij)*sin11d(k)+sin32d(ik)*sin11d(j)+ &
                  sin32d(jk)*sin11d(i))
                e3d(ii) = e3d(ii) + e2ab3*(sin1*sin1*(sin32d(ij)* &
                  sin31d(k)+sin32d(ik)*sin31d(j)+sin32d(jk)* &
                  sin31d(i))+sin3*sin3*(sin12d(ij)*sin11d(k)+ &
                  sin12d(ik)*sin11d(j)+sin12d(jk)*sin11d(i)))
                e3d(ii) = e3d(ii) + e3ab1*(sin1*(sin11d(i)*sin31d(j)* &
                  sin31d(k)+sin11d(j)*sin31d(i)*sin31d(k)+ &
                  sin11d(k)*sin31d(i)*sin31d(j))+sin3*(sin31d(i)* &
                  sin11d(j)*sin11d(k)+sin31d(j)*sin11d(i)*sin11d(k)+ &
                  sin31d(k)*sin11d(i)*sin11d(j)))
                e3d(ii) = e3d(ii) + e2ab4*dsinrat2*cosp1d(i)*cosp1d(j)*cosp1d(k)
                e3d(ii) = e3d(ii) + e3ab2*(sin1nm3*sin3n*sin11d(i)* &
                  sin11d(j)*sin11d(k)+sin1n*sin3nm3*sin31d(i)* &
                  sin31d(j)*sin31d(k))
                e3d(ii) = e3d(ii) + rn*e2ab5*(sin11d(i)*(sin31d(j)* &
                  cosp1d(k)+sin31d(k)*cosp1d(j))+sin11d(j)*( &
                  sin31d(i)*cosp1d(k)+sin31d(k)*cosp1d(i))+ &
                  sin11d(k)*(sin31d(i)*cosp1d(j)+sin31d(j)* &
                  cosp1d(i)))
                e3d(ii) = e3d(ii) + e3ab3* &
                  (sin1*sin3*(cosp2d(ij)*cosp1d(k)+cosp2d(ik)* &
                  cosp1d(j)+cosp2d(jk)*cosp1d(i))+rn*(sin1*( &
                  sin31d(i)*cosp1d(j)*cosp1d(k)+sin31d(j)* &
                  cosp1d(i)*cosp1d(k)+sin31d(k)*cosp1d(i)* &
                  cosp1d(j))+sin3*(sin11d(i)*cosp1d(j)*cosp1d(k)+ &
                  sin11d(j)*cosp1d(i)*cosp1d(k)+sin11d(k)* &
                  cosp1d(i)*cosp1d(j))))
                e3d(ii) = e3d(ii) + e3ab4* &
                  (sin1*sin1*(cosp1d(i)*sin31d(j)*sin31d(k)+ &
                  cosp1d(j)*sin31d(i)*sin31d(k)+cosp1d(k)*sin31d(i)* &
                  sin31d(j))+sin3*sin3*(cosp1d(i)*sin11d(j)* &
                  sin11d(k)+cosp1d(j)*sin11d(i)*sin11d(k)+cosp1d(k)* &
                  sin11d(i)*sin11d(j)))
                e3d(ii) = e3d(ii) + e2ab4*sinrat*cosp3d(ii)
                e3d(ii) = e3d(ii) + e2ab5*(sin1*(cosp2d(ij)*sin31d(k)+ &
                  cosp2d(ik)*sin31d(j)+cosp2d(jk)*sin31d(i)+ &
                  sin32d(ij)*cosp1d(k)+sin32d(ik)*cosp1d(j)+ &
                  sin32d(jk)*cosp1d(i))+sin3*(cosp2d(ij)*sin11d(k)+ &
                  cosp2d(ik)*sin11d(j)+cosp2d(jk)*sin11d(i)+ &
                  sin12d(ij)*cosp1d(k)+sin12d(ik)*cosp1d(j)+ &
                  sin12d(jk)*cosp1d(i)))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.5) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Harmonic torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    phi = acos(cosphi)
    efor = 0.5_dp*rkfor*(phi - phi0)**2
    if (lgrad1loc) then
!
!  First derivatives of phi
!               
      sinphi = sin(phi)
      lphieq0 = (abs(sinphi).lt.1.0d-6) 
      if (lphieq0) then
        do k = 1,6
          phi1d(k) = 0.0_dp
        enddo
      else
        rsinphi = 1.0_dp/sinphi
        do k = 1,6
          phi1d(k) = - rsinphi*cosp1d(k)
        enddo
      endif
!
!  First derivatives of energy
!
      e1 = rkfor*(phi - phi0)
      do k = 1,6
        e1d(k) = e1*phi1d(k)
      enddo
!
      if (lgrad2loc) then
!    
!  Second derivatives of phi
!             
        rsinphi2 = rsinphi*rsinphi
        trm1 = cosphi*rsinphi2
        if (lphieq0) then
          phi2d(1:21) = 0.0_dp
        else
          ii = 0
          do i = 1,6
            do j = i,6
              ii = ii + 1
              phi2d(ii) = trm1*cosp1d(i)*phi1d(j) - rsinphi*cosp2d(ii)
            enddo
          enddo
        endif
!
!  Second derivatives of energy
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = rkfor*phi1d(i)*phi1d(j) + e1*phi2d(ii)
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third
!
          if (lphieq0) then
            phi3d(1:56) = 0.0_dp
          else
            ii=0
            do i=1,6
              do j=i,6
                ij=kb(j,i)
                do k=j,6  
                  ii=ii+1
                  ik=kb(k,i)
                  jk=kb(k,j)
                  phi3d(ii) = - rsinphi*cosp3d(ii) + trm1*(phi1d(i)*cosp2d(jk) + phi1d(j)*cosp2d(ik) + phi1d(k)*cosp2d(ij)) &
                              - rsinphi*trm1*cosphi*cosp1d(i)*phi1d(j)*phi1d(k) &
                              - rsinphi*cosp1d(i)*phi1d(j)*phi1d(k)
                enddo
              enddo
            enddo
          endif
!
!  Third derivatives of the energy
!
          ii=0
          do i=1,6
            do j=i,6
              ij=kb(j,i)
              do k=j,6  
                ii=ii+1
                ik=kb(k,i)
                jk=kb(k,j)
                e3d(ii) = e1*phi3d(ii) + rkfor*(phi1d(i)*phi2d(jk) + phi1d(j)*phi2d(ik) + phi1d(k)*phi2d(ij))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.6) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Exponentially decaying torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    phi = acos(cosphi)
    lphi0non0 = (abs(phi0).gt.1.0d-5)
    if (lphi0non0) then
      cphi0 = cos(phi0)
      sphi0 = sin(phi0)
      if (cphi0.lt.-0.99999_dp) then
        lphi0non0 = .false.
        isgn = - isgn
        cphi0 = 1.0_dp
        sphi0 = 0.0_dp
      endif
    else
      cphi0 = 1.0_dp
      sphi0 = 0.0_dp
    endif
!
!  Distance terms
!
    if (abs(fpoly(2)).gt.1.0d-10) then
      rho12 = 1.0_dp/fpoly(2)
    else
      rho12 = 0.0_dp
    endif
    if (abs(fpoly(3)).gt.1.0d-10) then
      rho23 = 1.0_dp/fpoly(3)
    else
      rho23 = 0.0_dp
    endif
    if (abs(fpoly(4)).gt.1.0d-10) then
      rho34 = 1.0_dp/fpoly(4)
    else
      rho34 = 0.0_dp
    endif
    ee0 = exp(-rho12*r12)*exp(-rho23*r23)*exp(-rho34*r34)
!
    cosnphi = cos(rn*phi)
    et0 = rkfor*(1.0_dp + isgn*cosnphi*cphi0)
!
    if (lphi0non0) then
      sinnphi = sin(rn*phi)
      sinphi = sin(phi)
      et0 = et0 + rkfor*isgn*sinnphi*sphi0
!
!  Trap phi0 ne 0, phi = 0 case
!
      if (abs(phi).lt.1.0d-6) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''  **** Warning - phi is zero with non-zero phi0 => derivative problem ****'',/)')
        endif
        return
      endif
    endif
    efor = et0*ee0
    if (lgrad1loc) then
!
!  First derivatives of energy
!
!  d(cosnphi)/dcosphi = sinnphi/sinphi
!
!  to avoid divide by zero error when sinphi = 0 we use a series
!  expansion in cosphi
!
      e1 = rkfor*isgn*rn*cphi0
      e2 = e1
      e3 = e1
      nmax = nint(rn)
      cosd1 = sinratio(nmax,phi)
      e1 = e1*cosd1
      do k = 1,6
        et1d(k) = e1*cosp1d(k)
      enddo
      if (lphi0non0) then
!
!  Phi0 contribution to first derivatives
!
        rsinp = 1.0_dp/sinphi
        ep0 = rkfor*isgn*rn*sphi0
        e1p0 = ep0*cosnphi*rsinp
        do k = 1,6
          et1d(k) = et1d(k) - e1p0*cosp1d(k)
        enddo
      endif
!
!  Calculate first derivatives with respect to distance term
!
      ee1dnoee0(1) = - rho12*rr12
      ee1dnoee0(2) = 0.0_dp
      ee1dnoee0(3) = 0.0_dp
      ee1dnoee0(4) = - rho23*rr23
      ee1dnoee0(5) = 0.0_dp
      ee1dnoee0(6) = - rho34*rr34
      do k = 1,6
        ee1d(k) = ee0*ee1dnoee0(k)
      enddo
!
!  Combine torsion and distance first derivatives
!
      do k = 1,6
        e1d(k) = et1d(k)*ee0 + et0*ee1d(k)
      enddo
!
      if (lgrad2loc) then
!
!  Second derivatives of energy
!
        cosd2 = dsinratio(nmax,phi)
        e2 = e2*cosd2
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            et2d(ii) = e2*cosp1d(i)*cosp1d(j) + e1*cosp2d(ii)
          enddo
        enddo
        if (lphi0non0) then
!
!  Phi0 contribution to second derivatives
!
          e2p0 = ep0*rsinp*rsinp*(rn*sinnphi+cosnphi*cosphi*rsinp)
          ii = 0
          do i = 1,6
            do j = i,6
              ii = ii + 1
              et2d(ii) = et2d(ii) - e1p0*cosp2d(ii) - e2p0*cosp1d(i)*cosp1d(j)
            enddo
          enddo
        endif
!
!  Calculate second derivatives with respect to distance term
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            ee2dnoee0(ii) = ee1dnoee0(i)*ee1dnoee0(j)
            if (i.eq.j) then
              if (i.eq.1) then
                ee2dnoee0(ii) = ee2dnoee0(ii) + rho12*rr12*rr12*rr12
              elseif (i.eq.4) then
                ee2dnoee0(ii) = ee2dnoee0(ii) + rho23*rr23*rr23*rr23
              elseif (i.eq.6) then
                ee2dnoee0(ii) = ee2dnoee0(ii) + rho34*rr34*rr34*rr34
              endif
            endif
          enddo
        enddo
        do k = 1,21
          ee2d(k) = ee0*ee2dnoee0(k)
        enddo
!
!  Combine torsion and distance second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2d(ii) + et2d(ii)*ee0 + et1d(i)*ee1d(j) + et1d(j)*ee1d(i) + et0*ee2d(ii)
          enddo
        enddo
!
        if (lgrad3loc) then
!
!  Third derivatives of energy
!
          cosd3 = 0.0_dp
          if (nmax.gt.1) then
            cosd3 = dble(nmax-1)*dsinratio((nmax-1_i4),phi)
            cosd3 = cosd3 + dble(nmax-2)*cosphi*dsinratio((nmax-2_i4),phi)
            cosd3 = cosd3 + 2.0_dp*dble(nmax-2)*sinratio((nmax-2_i4),phi)
            cosphim = 1.0_dp
            do k = 3,nmax
              cosmphi = cos(dble(nmax-k)*phi)
              cosd3 = cosd3 + dble((k-2)*(k-1))*cosphim*cosmphi
              cosphim = cosphim*cosphi
              cosd3 = cosd3 + dble(2*(k-1)*(nmax-k))*cosphim*sinratio((nmax-k),phi)
              cosd3 = cosd3 + dble(nmax-k)*cosphim*cosphi*dsinratio((nmax-k),phi)
            enddo
            e3 = e3*cosd3
          endif
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                et3d(ii) = e3*cosp1d(i)*cosp1d(j)*cosp1d(k)
                et3d(ii) = et3d(ii) + e1*cosp3d(ii)
                et3d(ii) = et3d(ii) + e2*cosp1d(i)*cosp2d(jk)
                et3d(ii) = et3d(ii) + e2*cosp1d(j)*cosp2d(ik)
                et3d(ii) = et3d(ii) + e2*cosp1d(k)*cosp2d(ij)
              enddo
            enddo
          enddo
          if (lphi0non0) then
!
!  Phi0 contribution to third derivatives
!
            e3p0 = ep0*rsinp*rsinp*rsinp*(3.0_dp*cosphi*rsinp*(rn*sinnphi+cosnphi*cosphi*rsinp)+cosnphi*(1.0_dp-rn*rn))
            ii = 0
            do i = 1,6
              do j = i,6
                ij = kb(j,i)
                do k = j,6
                  ik = kb(k,i)
                  jk = kb(k,j)
                  ii = ii + 1
                  et3d(ii) = et3d(ii) - e1p0*cosp3d(ii)
                  et3d(ii) = et3d(ii) - e2p0*(cosp1d(i)*cosp2d(jk) + cosp1d(j)*cosp2d(ik) + cosp1d(k)*cosp2d(ij))
                  et3d(ii) = et3d(ii) - e3p0*cosp1d(i)*cosp1d(j)*cosp1d(k)
                enddo
              enddo
            enddo
          endif
!
!  Calculate third derivatives with respect to distance term
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                if (i.eq.j) then
                  if (i.eq.k) then
                    if (i.eq.1) then
                      ee3d(ii) = - ee0*rho12*rr12*rr12*rr12*(rho12*rho12 + 3.0_dp*(rho12*rr12 + rr12*rr12))
                    elseif (i.eq.4) then
                      ee3d(ii) = - ee0*rho23*rr23*rr23*rr23*(rho23*rho23 + 3.0_dp*(rho23*rr23 + rr23*rr23))
                    elseif (i.eq.6) then
                      ee3d(ii) = - ee0*rho34*rr34*rr34*rr34*(rho34*rho34 + 3.0_dp*(rho34*rr34 + rr34*rr34))
                    endif
                  else
                    ee3d(ii) = ee0*ee2dnoee0(ij)*ee1dnoee0(k)
                  endif
                elseif (i.eq.k) then
                  ee3d(ii) = ee0*ee2dnoee0(ik)*ee1dnoee0(j)
                elseif (j.eq.k) then
                  ee3d(ii) = ee0*ee2dnoee0(jk)*ee1dnoee0(i)
                else
                  ee3d(ii) = ee0*ee1dnoee0(i)*ee1dnoee0(j)*ee1dnoee0(k)
                endif
              enddo
            enddo
          enddo
!
!  Combine torsion and distance third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = et3d(ii)*ee0 + et0*ee3d(ii)
                e3d(ii) = e3d(ii) + et2d(jk)*ee1d(i) + et1d(i)*ee2d(jk)
                e3d(ii) = e3d(ii) + et2d(ik)*ee1d(j) + et1d(j)*ee2d(ik)
                e3d(ii) = e3d(ii) + et2d(ij)*ee1d(k) + et1d(k)*ee2d(ij)
              enddo
            enddo
          enddo
!
        endif
      endif
    endif
  elseif (n4ty.eq.7) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Exponentially decaying ESFF torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Create constants for first and second terms
!
!  first  term = k1
!  second term = k2*isgn
!
    phi = acos(cosphi)
    cosnphi = cos(rn*phi)
    e0a = rkfor
    e0b = phi0*isgn
    sin1nm2 = sin1**(rn-2.0_dp)
    sin3nm2 = sin3**(rn-2.0_dp)
    et0 = sin1*sin1*sin3*sin3*(e0a + e0b*sin1nm2*sin3nm2*cosnphi)
!
!  Distance terms
!
    if (abs(fpoly(2)).gt.1.0d-10) then
      rho12 = 1.0_dp/fpoly(2)
    else
      rho12 = 0.0_dp
    endif
    if (abs(fpoly(3)).gt.1.0d-10) then
      rho23 = 1.0_dp/fpoly(3)
    else
      rho23 = 0.0_dp
    endif
    if (abs(fpoly(4)).gt.1.0d-10) then
      rho34 = 1.0_dp/fpoly(4)
    else
      rho34 = 0.0_dp
    endif
    ee0 = exp(-rho12*r12)*exp(-rho23*r23)*exp(-rho34*r34)
!
    efor = et0*ee0
!
    if (lgrad1loc) then
!
!  First derivatives
!
      nmax = nint(rn)
      sinrat = sinratio(nmax,phi)
      sin1n = sin1nm2*sin1*sin1
      sin3n = sin3nm2*sin3*sin3
      do k = 1,6
        et1d(k) = rn*e0b*(sin1n*sin3n*sinrat*cosp1d(k) + cosnphi* &
          (sin1nm2*sin1*sin3n*sin11d(k) + sin3nm2*sin3*sin1n*sin31d(k)))
        et1d(k) = et1d(k) + 2.0_dp*e0a*(sin1*sin3*sin3*sin11d(k) + sin3*sin1*sin1*sin31d(k))
      enddo
!
!  Calculate first derivatives with respect to distance term
!
      ee1dnoee0(1) = - rho12*rr12
      ee1dnoee0(2) = 0.0_dp
      ee1dnoee0(3) = 0.0_dp
      ee1dnoee0(4) = - rho23*rr23
      ee1dnoee0(5) = 0.0_dp
      ee1dnoee0(6) = - rho34*rr34
      do k = 1,6
        ee1d(k) = ee0*ee1dnoee0(k)
      enddo
!
!  Combine torsion and distance first derivatives
!
      do k = 1,6
        e1d(k) = et1d(k)*ee0 + et0*ee1d(k)
      enddo
!
      if (lgrad2loc) then
!
!  Second derivatives
!
        dsinrat = dsinratio(nmax,phi)
        e2ab1 = (4.0_dp*e0a+rn*rn*e0b*cosnphi*sin1nm2*sin3nm2)*sin1*sin3
        e2ab2 = (2.0_dp*e0a+rn*e0b*sin1nm2*sin3nm2*cosnphi)*sin1*sin3
        e2ab3 = (2.0_dp*e0a+rn*(rn-1.0_dp)*e0b*cosnphi*sin1nm2*sin3nm2)
        e2ab4 = rn*e0b*sin1n*sin3n
        e2ab5 = rn*rn*e0b*sinrat*sin1nm2*sin1*sin3*sin3nm2
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            et2d(ii) = e2ab1*(sin11d(i)*sin31d(j)+sin11d(j)*sin31d(i))
            et2d(ii) = et2d(ii) + e2ab2*(sin3*sin12d(ii)+sin1*sin32d(ii))
            et2d(ii) = et2d(ii) + e2ab3*(sin3*sin3*sin11d(i)*sin11d(j) + sin1*sin1*sin31d(i)*sin31d(j))
            et2d(ii) = et2d(ii) + e2ab4*(sinrat*cosp2d(ii) + dsinrat*cosp1d(i)*cosp1d(j))
            et2d(ii) = et2d(ii) + e2ab5*(sin3*cosp1d(i)*sin11d(j) + sin1*cosp1d(i)*sin31d(j) +  &
                               sin3*cosp1d(j)*sin11d(i)+sin1*cosp1d(j)*sin31d(i))
          enddo
        enddo
!
!  Calculate second derivatives with respect to distance term
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            ee2dnoee0(ii) = ee1dnoee0(i)*ee1dnoee0(j)
            if (i.eq.j) then
              if (i.eq.1) then
                ee2dnoee0(ii) = ee2dnoee0(ii) + rho12*rr12*rr12*rr12
              elseif (i.eq.4) then
                ee2dnoee0(ii) = ee2dnoee0(ii) + rho23*rr23*rr23*rr23
              elseif (i.eq.6) then
                ee2dnoee0(ii) = ee2dnoee0(ii) + rho34*rr34*rr34*rr34
              endif
            endif
          enddo
        enddo
        do k = 1,21
          ee2d(k) = ee0*ee2dnoee0(k)
        enddo
!
!  Combine torsion and distance second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2d(ii) + et2d(ii)*ee0 + et1d(i)*ee1d(j) + et1d(j)*ee1d(i) + et0*ee2d(ii)
          enddo
        enddo
!
        if (lgrad3loc) then
!
!  Third derivatives
!
!  Start by calculating second derivative of (sinnphi/sinphi) w.r.t.
!  cosphi while avoiding problems as phi -> zero
!
          dsinrat2 = 0.0_dp
          if (nmax.gt.1) then
            dsinrat2 = dble(nmax-1)*dsinratio((nmax-1_i4),phi)
            dsinrat2 = dsinrat2+dble(nmax-2)*cosphi*dsinratio((nmax-2_i4),phi)
            dsinrat2 = dsinrat2+2.0_dp*dble(nmax-2)*sinratio((nmax-2_i4),phi)
            cosphim = 1.0_dp
            do k = 3,nmax
              cosmphi = cos(dble(nmax-k)*phi)
              dsinrat2 = dsinrat2 + dble((k-2)*(k-1))*cosphim*cosmphi
              cosphim = cosphim*cosphi
              dsinrat2 = dsinrat2 + dble(2*(k-1)*(nmax-k))*cosphim*sinratio((nmax-k),phi)
              dsinrat2 = dsinrat2 + dble(nmax-k)*cosphim*cosphi*dsinratio((nmax-k),phi)
            enddo
          endif
          sin1nm3 = sin1nm2/sin1
          sin3nm3 = sin3nm2/sin3
          e3ab1 = (4.0_dp*e0a+rn*rn*(rn-1.0_dp)*e0b*cosnphi*sin1nm2*sin3nm2)
          e3ab2 = rn*(rn-1.0_dp)*(rn-2.0_dp)*e0b*cosnphi
          e3ab3 = rn*e0b*dsinrat*sin1nm2*sin3nm2*sin1*sin3
          e3ab4 = rn*rn*(rn-1.0_dp)*e0b*sinrat*sin1nm2*sin3nm2
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                et3d(ii) = e2ab2*(sin3*sin13d(ii)+sin1*sin33d(ii))
                et3d(ii) = et3d(ii) + e2ab1*(sin12d(ij)*sin31d(k)+ &
                  sin12d(ik)*sin31d(j)+sin12d(jk)*sin31d(i)+ &
                  sin32d(ij)*sin11d(k)+sin32d(ik)*sin11d(j)+ &
                  sin32d(jk)*sin11d(i))
                et3d(ii) = et3d(ii) + e2ab3*(sin1*sin1*(sin32d(ij)* &
                  sin31d(k)+sin32d(ik)*sin31d(j)+sin32d(jk)* &
                  sin31d(i))+sin3*sin3*(sin12d(ij)*sin11d(k)+ &
                  sin12d(ik)*sin11d(j)+sin12d(jk)*sin11d(i)))
                et3d(ii) = et3d(ii) + e3ab1*(sin1*(sin11d(i)*sin31d(j)* &
                  sin31d(k)+sin11d(j)*sin31d(i)*sin31d(k)+ &
                  sin11d(k)*sin31d(i)*sin31d(j))+sin3*(sin31d(i)* &
                  sin11d(j)*sin11d(k)+sin31d(j)*sin11d(i)*sin11d(k)+ &
                  sin31d(k)*sin11d(i)*sin11d(j)))
                et3d(ii) = et3d(ii) + e2ab4*dsinrat2*cosp1d(i)*cosp1d(j)*cosp1d(k)
                et3d(ii) = et3d(ii) + e3ab2*(sin1nm3*sin3n*sin11d(i)* &
                  sin11d(j)*sin11d(k)+sin1n*sin3nm3*sin31d(i)* &
                  sin31d(j)*sin31d(k))
                et3d(ii) = et3d(ii) + rn*e2ab5*(sin11d(i)*(sin31d(j)* &
                  cosp1d(k)+sin31d(k)*cosp1d(j))+sin11d(j)*( &
                  sin31d(i)*cosp1d(k)+sin31d(k)*cosp1d(i))+ &
                  sin11d(k)*(sin31d(i)*cosp1d(j)+sin31d(j)* &
                  cosp1d(i)))
                et3d(ii) = et3d(ii) + e3ab3* &
                  (sin1*sin3*(cosp2d(ij)*cosp1d(k)+cosp2d(ik)* &
                  cosp1d(j)+cosp2d(jk)*cosp1d(i))+rn*(sin1*( &
                  sin31d(i)*cosp1d(j)*cosp1d(k)+sin31d(j)* &
                  cosp1d(i)*cosp1d(k)+sin31d(k)*cosp1d(i)* &
                  cosp1d(j))+sin3*(sin11d(i)*cosp1d(j)*cosp1d(k)+ &
                  sin11d(j)*cosp1d(i)*cosp1d(k)+sin11d(k)* &
                  cosp1d(i)*cosp1d(j))))
                et3d(ii) = et3d(ii) + e3ab4* &
                  (sin1*sin1*(cosp1d(i)*sin31d(j)*sin31d(k)+ &
                  cosp1d(j)*sin31d(i)*sin31d(k)+cosp1d(k)*sin31d(i)* &
                  sin31d(j))+sin3*sin3*(cosp1d(i)*sin11d(j)* &
                  sin11d(k)+cosp1d(j)*sin11d(i)*sin11d(k)+cosp1d(k)* &
                  sin11d(i)*sin11d(j)))
                et3d(ii) = et3d(ii) + e2ab4*sinrat*cosp3d(ii)
                et3d(ii) = et3d(ii) + e2ab5*(sin1*(cosp2d(ij)*sin31d(k)+ &
                  cosp2d(ik)*sin31d(j)+cosp2d(jk)*sin31d(i)+ &
                  sin32d(ij)*cosp1d(k)+sin32d(ik)*cosp1d(j)+ &
                  sin32d(jk)*cosp1d(i))+sin3*(cosp2d(ij)*sin11d(k)+ &
                  cosp2d(ik)*sin11d(j)+cosp2d(jk)*sin11d(i)+ &
                  sin12d(ij)*cosp1d(k)+sin12d(ik)*cosp1d(j)+ &
                  sin12d(jk)*cosp1d(i)))
              enddo
            enddo
          enddo
!
!  Calculate third derivatives with respect to distance term
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                if (i.eq.j) then
                  if (i.eq.k) then
                    if (i.eq.1) then
                      ee3d(ii) = - ee0*rho12*rr12*rr12*rr12*(rho12*rho12 + 3.0_dp*(rho12*rr12 + rr12*rr12))
                    elseif (i.eq.4) then
                      ee3d(ii) = - ee0*rho23*rr23*rr23*rr23*(rho23*rho23 + 3.0_dp*(rho23*rr23 + rr23*rr23))
                    elseif (i.eq.6) then
                      ee3d(ii) = - ee0*rho34*rr34*rr34*rr34*(rho34*rho34 + 3.0_dp*(rho34*rr34 + rr34*rr34))
                    endif
                  else
                    ee3d(ii) = ee0*ee2dnoee0(ij)*ee1dnoee0(k)
                  endif
                elseif (i.eq.k) then
                  ee3d(ii) = ee0*ee2dnoee0(ik)*ee1dnoee0(j)
                elseif (j.eq.k) then
                  ee3d(ii) = ee0*ee2dnoee0(jk)*ee1dnoee0(i)
                else
                  ee3d(ii) = ee0*ee1dnoee0(i)*ee1dnoee0(j)*ee1dnoee0(k)
                endif
              enddo
            enddo
          enddo
!
!  Combine torsion and distance third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = et3d(ii)*ee0 + et0*ee3d(ii)
                e3d(ii) = e3d(ii) + et2d(jk)*ee1d(i) + et1d(i)*ee2d(jk)
                e3d(ii) = e3d(ii) + et2d(ik)*ee1d(j) + et1d(j)*ee2d(ik)
                e3d(ii) = e3d(ii) + et2d(ij)*ee1d(k) + et1d(k)*ee2d(ij)
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.8) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Tapered torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    phi = acos(cosphi)
    lphi0non0 = (abs(phi0).gt.1.0d-5)
    if (lphi0non0) then
      cphi0 = cos(phi0)
      sphi0 = sin(phi0)
      if (cphi0.lt.-0.99999_dp) then
        lphi0non0 = .false.
        isgn = - isgn
        cphi0 = 1.0_dp
        sphi0 = 0.0_dp
      endif
    else
      cphi0 = 1.0_dp
      sphi0 = 0.0_dp
    endif
!
!  Distance terms
!
    rmax12 = fpoly(3)
    rmax23 = fpoly(4)
    rmax34 = fpoly(5)
    call ctaper(r12,rmax12-fpoly(2),rmax12,f12,df12dr,d2f12dr2,d3f12dr3,lgrad1loc,lgrad2loc,lgrad3loc)
    call ctaper(r23,rmax23-fpoly(2),rmax23,f23,df23dr,d2f23dr2,d3f23dr3,lgrad1loc,lgrad2loc,lgrad3loc)
    call ctaper(r34,rmax34-fpoly(2),rmax34,f34,df34dr,d2f34dr2,d3f34dr3,lgrad1loc,lgrad2loc,lgrad3loc)
    ee0 = f12*f23*f34
!
    cosnphi = cos(rn*phi)
    et0 = rkfor*(1.0_dp + isgn*cosnphi*cphi0)
!
    if (lphi0non0) then
      sinnphi = sin(rn*phi)
      sinphi = sin(phi)
      et0 = et0 + rkfor*isgn*sinnphi*sphi0
!
!  Trap phi0 ne 0, phi = 0 case
!
      if (abs(phi).lt.1.0d-6) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''  **** Warning - phi is zero with non-zero phi0 => derivative problem ****'',/)')
        endif
        return
      endif
    endif
    efor = et0*ee0
    if (lgrad1loc) then
!
!  First derivatives of energy
!
!  d(cosnphi)/dcosphi = sinnphi/sinphi
!
!  to avoid divide by zero error when sinphi = 0 we use a series
!  expansion in cosphi
!
      e1 = rkfor*isgn*rn*cphi0
      e2 = e1
      e3 = e1
      nmax = nint(rn)
      cosd1 = sinratio(nmax,phi)
      e1 = e1*cosd1
      do k = 1,6
        et1d(k) = e1*cosp1d(k)
      enddo
      if (lphi0non0) then
!
!  Phi0 contribution to first derivatives
!
        rsinp = 1.0_dp/sinphi
        ep0 = rkfor*isgn*rn*sphi0
        e1p0 = ep0*cosnphi*rsinp
        do k = 1,6
          et1d(k) = et1d(k) - e1p0*cosp1d(k)
        enddo
      endif
!
!  Calculate first derivatives with respect to distance term
!
      df12dr = rr12*df12dr
      df23dr = rr23*df23dr
      df34dr = rr34*df34dr
      ee1d(1) = f23*f34*df12dr
      ee1d(2) = 0.0_dp
      ee1d(3) = 0.0_dp
      ee1d(4) = f12*f34*df23dr
      ee1d(5) = 0.0_dp
      ee1d(6) = f12*f23*df34dr
!
!  Combine torsion and distance first derivatives
!
      do k = 1,6
        e1d(k) = et1d(k)*ee0 + et0*ee1d(k)
      enddo
!
      if (lgrad2loc) then
!
!  Second derivatives of energy
!
        cosd2 = dsinratio(nmax,phi)
        e2 = e2*cosd2
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            et2d(ii) = e2*cosp1d(i)*cosp1d(j) + e1*cosp2d(ii)
          enddo
        enddo
        if (lphi0non0) then
!
!  Phi0 contribution to second derivatives
!
          e2p0 = ep0*rsinp*rsinp*(rn*sinnphi+cosnphi*cosphi*rsinp)
          ii = 0
          do i = 1,6
            do j = i,6
              ii = ii + 1
              et2d(ii) = et2d(ii) - e1p0*cosp2d(ii) - e2p0*cosp1d(i)*cosp1d(j)
            enddo
          enddo
        endif
!
!  Calculate second derivatives with respect to distance term
!
        d2f12dr2 = rr122*(d2f12dr2 - df12dr)
        d2f23dr2 = rr232*(d2f23dr2 - df23dr)
        d2f34dr2 = rr342*(d2f34dr2 - df34dr)
!
        ee2d(1:21) = 0.0_dp
        ee2d(1)  = f23*f34*d2f12dr2
        ee2d(4)  = f34*df12dr*df23dr
        ee2d(6)  = f23*df12dr*df34dr
        ee2d(16) = f12*f34*d2f23dr2
        ee2d(18) = f12*df23dr*df34dr
        ee2d(21) = f12*f23*d2f34dr2
!
!  Combine torsion and distance second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2d(ii) + et2d(ii)*ee0 + et1d(i)*ee1d(j) + et1d(j)*ee1d(i) + et0*ee2d(ii)
          enddo
        enddo
!
        if (lgrad3loc) then
!
!  Third derivatives of energy
!
          cosd3 = 0.0_dp
          if (nmax.gt.1) then
            cosd3 = dble(nmax-1)*dsinratio((nmax-1_i4),phi)
            cosd3 = cosd3 + dble(nmax-2)*cosphi*dsinratio((nmax-2_i4),phi)
            cosd3 = cosd3 + 2.0_dp*dble(nmax-2)*sinratio((nmax-2_i4),phi)
            cosphim = 1.0_dp
            do k = 3,nmax
              cosmphi = cos(dble(nmax-k)*phi)
              cosd3 = cosd3 + dble((k-2)*(k-1))*cosphim*cosmphi
              cosphim = cosphim*cosphi
              cosd3 = cosd3 + dble(2*(k-1)*(nmax-k))*cosphim*sinratio((nmax-k),phi)
              cosd3 = cosd3 + dble(nmax-k)*cosphim*cosphi*dsinratio((nmax-k),phi)
            enddo
            e3 = e3*cosd3
          endif
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                et3d(ii) = e3*cosp1d(i)*cosp1d(j)*cosp1d(k)
                et3d(ii) = et3d(ii) + e1*cosp3d(ii)
                et3d(ii) = et3d(ii) + e2*cosp1d(i)*cosp2d(jk)
                et3d(ii) = et3d(ii) + e2*cosp1d(j)*cosp2d(ik)
                et3d(ii) = et3d(ii) + e2*cosp1d(k)*cosp2d(ij)
              enddo
            enddo
          enddo
          if (lphi0non0) then
!
!  Phi0 contribution to third derivatives
!
            e3p0 = ep0*rsinp*rsinp*rsinp*(3.0_dp*cosphi*rsinp*(rn*sinnphi+cosnphi*cosphi*rsinp)+cosnphi*(1.0_dp-rn*rn))
            ii = 0
            do i = 1,6
              do j = i,6
                ij = kb(j,i)
                do k = j,6
                  ik = kb(k,i)
                  jk = kb(k,j)
                  ii = ii + 1
                  et3d(ii) = et3d(ii) - e1p0*cosp3d(ii)
                  et3d(ii) = et3d(ii) - e2p0*(cosp1d(i)*cosp2d(jk) + cosp1d(j)*cosp2d(ik) + cosp1d(k)*cosp2d(ij))
                  et3d(ii) = et3d(ii) - e3p0*cosp1d(i)*cosp1d(j)*cosp1d(k)
                enddo
              enddo
            enddo
          endif
!
!  Calculate third derivatives with respect to distance term
!
          d3f12dr3 = rr122*(rr12*d3f12dr3 - 3.0_dp*d2f12dr2)
          d3f23dr3 = rr232*(rr23*d3f23dr3 - 3.0_dp*d2f23dr2)
          d3f34dr3 = rr342*(rr34*d3f34dr3 - 3.0_dp*d2f34dr2)
!
          ee3d(1:56) = 0.0_dp
          ee3d(1)  = f23*f34*d3f12dr3
          ee3d(4)  = f34*d2f12dr2*df23dr
          ee3d(6)  = f23*d2f12dr2*df34dr
          ee3d(16) = f34*d2f23dr2*df12dr
          ee3d(18) = df12dr*df23dr*df34dr
          ee3d(21) = f23*d2f34dr2*df12dr
          ee3d(47) = f12*f34*d3f23dr3
          ee3d(49) = f12*d2f23dr2*df34dr
          ee3d(52) = f12*d2f34dr2*df23dr
          ee3d(56) = f12*f23*d3f34dr3
!
!  Combine torsion and distance third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = et3d(ii)*ee0 + et0*ee3d(ii)
                e3d(ii) = e3d(ii) + et2d(jk)*ee1d(i) + et1d(i)*ee2d(jk)
                e3d(ii) = e3d(ii) + et2d(ik)*ee1d(j) + et1d(j)*ee2d(ik)
                e3d(ii) = e3d(ii) + et2d(ij)*ee1d(k) + et1d(k)*ee2d(ij)
              enddo
            enddo
          enddo
!
        endif
      endif
    endif
  elseif (n4ty.eq.9) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Tapered ESFF torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Create constants for first and second terms
!
!  first  term = k1
!  second term = k2*isgn
!
    phi = acos(cosphi)
    cosnphi = cos(rn*phi)
    e0a = rkfor
    e0b = phi0*isgn
    sin1nm2 = sin1**(rn-2.0_dp)
    sin3nm2 = sin3**(rn-2.0_dp)
    et0 = sin1*sin1*sin3*sin3*(e0a + e0b*sin1nm2*sin3nm2*cosnphi)
!
!  Distance terms
!
    rmax12 = fpoly(3)
    rmax23 = fpoly(4)
    rmax34 = fpoly(5)
    call ctaper(r12,rmax12-fpoly(2),rmax12,f12,df12dr,d2f12dr2,d3f12dr3,lgrad1loc,lgrad2loc,lgrad3loc)
    call ctaper(r23,rmax23-fpoly(2),rmax23,f23,df23dr,d2f23dr2,d3f23dr3,lgrad1loc,lgrad2loc,lgrad3loc)
    call ctaper(r34,rmax34-fpoly(2),rmax34,f34,df34dr,d2f34dr2,d3f34dr3,lgrad1loc,lgrad2loc,lgrad3loc)
    ee0 = f12*f23*f34
!
    efor = et0*ee0
!
    if (lgrad1loc) then
!
!  First derivatives
!
      nmax = nint(rn)
      sinrat = sinratio(nmax,phi)
      sin1n = sin1nm2*sin1*sin1
      sin3n = sin3nm2*sin3*sin3
      do k = 1,6
        et1d(k) = rn*e0b*(sin1n*sin3n*sinrat*cosp1d(k) + cosnphi* &
          (sin1nm2*sin1*sin3n*sin11d(k) + sin3nm2*sin3*sin1n*sin31d(k)))
        et1d(k) = et1d(k) + 2.0_dp*e0a*(sin1*sin3*sin3*sin11d(k) + sin3*sin1*sin1*sin31d(k))
      enddo
!
!  Calculate first derivatives with respect to distance term
!
      df12dr = rr12*df12dr
      df23dr = rr23*df23dr
      df34dr = rr34*df34dr
      ee1d(1) = f23*f34*df12dr
      ee1d(2) = 0.0_dp
      ee1d(3) = 0.0_dp
      ee1d(4) = f12*f34*df23dr
      ee1d(5) = 0.0_dp
      ee1d(6) = f12*f23*df34dr
!
!  Combine torsion and distance first derivatives
!
      do k = 1,6
        e1d(k) = et1d(k)*ee0 + et0*ee1d(k)
      enddo
!
      if (lgrad2loc) then
!
!  Second derivatives
!
        dsinrat = dsinratio(nmax,phi)
        e2ab1 = (4.0_dp*e0a+rn*rn*e0b*cosnphi*sin1nm2*sin3nm2)*sin1*sin3
        e2ab2 = (2.0_dp*e0a+rn*e0b*sin1nm2*sin3nm2*cosnphi)*sin1*sin3
        e2ab3 = (2.0_dp*e0a+rn*(rn-1.0_dp)*e0b*cosnphi*sin1nm2*sin3nm2)
        e2ab4 = rn*e0b*sin1n*sin3n
        e2ab5 = rn*rn*e0b*sinrat*sin1nm2*sin1*sin3*sin3nm2
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            et2d(ii) = e2ab1*(sin11d(i)*sin31d(j)+sin11d(j)*sin31d(i))
            et2d(ii) = et2d(ii) + e2ab2*(sin3*sin12d(ii)+sin1*sin32d(ii))
            et2d(ii) = et2d(ii) + e2ab3*(sin3*sin3*sin11d(i)*sin11d(j) + sin1*sin1*sin31d(i)*sin31d(j))
            et2d(ii) = et2d(ii) + e2ab4*(sinrat*cosp2d(ii) + dsinrat*cosp1d(i)*cosp1d(j))
            et2d(ii) = et2d(ii) + e2ab5*(sin3*cosp1d(i)*sin11d(j) + sin1*cosp1d(i)*sin31d(j) +  &
                               sin3*cosp1d(j)*sin11d(i)+sin1*cosp1d(j)*sin31d(i))
          enddo
        enddo
!
!  Calculate second derivatives with respect to distance term
!
        d2f12dr2 = rr122*(d2f12dr2 - df12dr)
        d2f23dr2 = rr232*(d2f23dr2 - df23dr)
        d2f34dr2 = rr342*(d2f34dr2 - df34dr)
!
        ee2d(1:21) = 0.0_dp
        ee2d(1)  = f23*f34*d2f12dr2
        ee2d(4)  = f34*df12dr*df23dr
        ee2d(6)  = f23*df12dr*df34dr
        ee2d(16) = f12*f34*d2f23dr2
        ee2d(18) = f12*df23dr*df34dr
        ee2d(21) = f12*f23*d2f34dr2
!
!  Combine torsion and distance second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2d(ii) + et2d(ii)*ee0 + et1d(i)*ee1d(j) + et1d(j)*ee1d(i) + et0*ee2d(ii)
          enddo
        enddo
!
        if (lgrad3loc) then
!
!  Third derivatives
!
!  Start by calculating second derivative of (sinnphi/sinphi) w.r.t.
!  cosphi while avoiding problems as phi -> zero
!
          dsinrat2 = 0.0_dp
          if (nmax.gt.1) then
            dsinrat2 = dble(nmax-1)*dsinratio((nmax-1_i4),phi)
            dsinrat2 = dsinrat2+dble(nmax-2)*cosphi*dsinratio((nmax-2_i4),phi)
            dsinrat2 = dsinrat2+2.0_dp*dble(nmax-2)*sinratio((nmax-2_i4),phi)
            cosphim = 1.0_dp
            do k = 3,nmax
              cosmphi = cos(dble(nmax-k)*phi)
              dsinrat2 = dsinrat2 + dble((k-2)*(k-1))*cosphim*cosmphi
              cosphim = cosphim*cosphi
              dsinrat2 = dsinrat2 + dble(2*(k-1)*(nmax-k))*cosphim*sinratio((nmax-k),phi)
              dsinrat2 = dsinrat2 + dble(nmax-k)*cosphim*cosphi*dsinratio((nmax-k),phi)
            enddo
          endif
          sin1nm3 = sin1nm2/sin1
          sin3nm3 = sin3nm2/sin3
          e3ab1 = (4.0_dp*e0a+rn*rn*(rn-1.0_dp)*e0b*cosnphi*sin1nm2*sin3nm2)
          e3ab2 = rn*(rn-1.0_dp)*(rn-2.0_dp)*e0b*cosnphi
          e3ab3 = rn*e0b*dsinrat*sin1nm2*sin3nm2*sin1*sin3
          e3ab4 = rn*rn*(rn-1.0_dp)*e0b*sinrat*sin1nm2*sin3nm2
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                et3d(ii) = e2ab2*(sin3*sin13d(ii)+sin1*sin33d(ii))
                et3d(ii) = et3d(ii) + e2ab1*(sin12d(ij)*sin31d(k) + sin12d(ik)*sin31d(j) + sin12d(jk)*sin31d(i) + &
                  sin32d(ij)*sin11d(k) + sin32d(ik)*sin11d(j) + sin32d(jk)*sin11d(i))
                et3d(ii) = et3d(ii) + e2ab3*(sin1*sin1*(sin32d(ij)*sin31d(k) + sin32d(ik)*sin31d(j) + sin32d(jk)* &
                  sin31d(i)) + sin3*sin3*(sin12d(ij)*sin11d(k)+ sin12d(ik)*sin11d(j) + sin12d(jk)*sin11d(i)))
                et3d(ii) = et3d(ii) + e3ab1*(sin1*(sin11d(i)*sin31d(j)*sin31d(k) + sin11d(j)*sin31d(i)*sin31d(k) + &
                  sin11d(k)*sin31d(i)*sin31d(j)) + sin3*(sin31d(i)*sin11d(j)*sin11d(k)+sin31d(j)*sin11d(i)*sin11d(k) + &
                  sin31d(k)*sin11d(i)*sin11d(j)))
                et3d(ii) = et3d(ii) + e2ab4*dsinrat2*cosp1d(i)*cosp1d(j)*cosp1d(k)
                et3d(ii) = et3d(ii) + e3ab2*(sin1nm3*sin3n*sin11d(i)*sin11d(j)*sin11d(k) + sin1n*sin3nm3*sin31d(i)* &
                  sin31d(j)*sin31d(k))
                et3d(ii) = et3d(ii) + rn*e2ab5*(sin11d(i)*(sin31d(j)*cosp1d(k) + sin31d(k)*cosp1d(j)) + sin11d(j)*( &
                  sin31d(i)*cosp1d(k) + sin31d(k)*cosp1d(i)) + sin11d(k)*(sin31d(i)*cosp1d(j) + sin31d(j)*cosp1d(i)))
                et3d(ii) = et3d(ii) + e3ab3*(sin1*sin3*(cosp2d(ij)*cosp1d(k) + cosp2d(ik)*cosp1d(j) +  &
                  cosp2d(jk)*cosp1d(i)) + rn*(sin1*(sin31d(i)*cosp1d(j)*cosp1d(k) + sin31d(j)*cosp1d(i)*cosp1d(k) + &
                  sin31d(k)*cosp1d(i)*cosp1d(j)) + sin3*(sin11d(i)*cosp1d(j)*cosp1d(k) + sin11d(j)*cosp1d(i)*cosp1d(k)  &
                  + sin11d(k)*cosp1d(i)*cosp1d(j))))
                et3d(ii) = et3d(ii) + e3ab4*(sin1*sin1*(cosp1d(i)*sin31d(j)*sin31d(k) + cosp1d(j)*sin31d(i)*sin31d(k)  &
                  + cosp1d(k)*sin31d(i)*sin31d(j)) + sin3*sin3*(cosp1d(i)*sin11d(j)*sin11d(k) +  &
                  cosp1d(j)*sin11d(i)*sin11d(k) + cosp1d(k)*sin11d(i)*sin11d(j)))
                et3d(ii) = et3d(ii) + e2ab4*sinrat*cosp3d(ii)
                et3d(ii) = et3d(ii) + e2ab5*(sin1*(cosp2d(ij)*sin31d(k) + cosp2d(ik)*sin31d(j) + cosp2d(jk)*sin31d(i) + &
                  sin32d(ij)*cosp1d(k) + sin32d(ik)*cosp1d(j) + sin32d(jk)*cosp1d(i)) + sin3*(cosp2d(ij)*sin11d(k) + &
                  cosp2d(ik)*sin11d(j) + cosp2d(jk)*sin11d(i) + sin12d(ij)*cosp1d(k) + sin12d(ik)*cosp1d(j) + &
                  sin12d(jk)*cosp1d(i)))
              enddo
            enddo
          enddo
!
!  Calculate third derivatives with respect to distance term
!
          d3f12dr3 = rr122*(rr12*d3f12dr3 - 3.0_dp*d2f12dr2)
          d3f23dr3 = rr232*(rr23*d3f23dr3 - 3.0_dp*d2f23dr2)
          d3f34dr3 = rr342*(rr34*d3f34dr3 - 3.0_dp*d2f34dr2)
!
          ee3d(1:56) = 0.0_dp
          ee3d(1)  = f23*f34*d3f12dr3
          ee3d(4)  = f34*d2f12dr2*df23dr
          ee3d(6)  = f23*d2f12dr2*df34dr
          ee3d(16) = f34*d2f23dr2*df12dr
          ee3d(18) = df12dr*df23dr*df34dr
          ee3d(21) = f23*d2f34dr2*df12dr
          ee3d(47) = f12*f34*d3f23dr3
          ee3d(49) = f12*d2f23dr2*df34dr
          ee3d(52) = f12*d2f34dr2*df23dr
          ee3d(56) = f12*f23*d3f34dr3
!
!  Combine torsion and distance third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = et3d(ii)*ee0 + et0*ee3d(ii)
                e3d(ii) = e3d(ii) + et2d(jk)*ee1d(i) + et1d(i)*ee2d(jk)
                e3d(ii) = e3d(ii) + et2d(ik)*ee1d(j) + et1d(j)*ee2d(ik)
                e3d(ii) = e3d(ii) + et2d(ij)*ee1d(k) + et1d(k)*ee2d(ij)
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.10) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Torsion-angle cross term potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    delth1 = theta1 - fpoly(1)
    delth2 = theta2 - fpoly(2)
    efor = rkfor*delth1*delth2*cosphi
    if (lgrad1loc) then
      do i = 1,6
        e1d(i) = rkfor*((theta11d(i)*delth2 + delth1*theta21d(i))*cosphi + delth1*delth2*cosp1d(i))
      enddo
      if (lgrad2loc) then
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = rkfor*((theta12d(ii)*delth2 + delth1*theta22d(ii))*cosphi + delth1*delth2*cosp2d(ii))
            e2d(ii) = e2d(ii) + rkfor*(theta11d(i)*theta21d(j) + theta11d(j)*theta21d(i))*cosphi
            e2d(ii) = e2d(ii) + rkfor*(theta11d(i)*delth2 + delth1*theta21d(i))*cosp1d(j)
            e2d(ii) = e2d(ii) + rkfor*(theta11d(j)*delth2 + delth1*theta21d(j))*cosp1d(i)
          enddo
        enddo
        if (lgrad3loc) then
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = rkfor*((theta13d(ii)*delth2 + delth1*theta23d(ii))*cosphi + delth1*delth2*cosp3d(ii))
                e3d(ii) = e3d(ii) + rkfor*(theta11d(i)*theta21d(j) + theta11d(j)*theta21d(i))*cosp1d(k)
                e3d(ii) = e3d(ii) + rkfor*(theta11d(i)*theta21d(k) + theta11d(k)*theta21d(i))*cosp1d(j)
                e3d(ii) = e3d(ii) + rkfor*(theta11d(j)*theta21d(k) + theta11d(k)*theta21d(j))*cosp1d(i)
                e3d(ii) = e3d(ii) + rkfor*theta12d(ij)*(delth2*cosp1d(k) + theta21d(k)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*theta12d(ik)*(delth2*cosp1d(j) + theta21d(j)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*theta12d(jk)*(delth2*cosp1d(i) + theta21d(i)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*theta22d(ij)*(delth1*cosp1d(k) + theta11d(k)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*theta22d(ik)*(delth1*cosp1d(j) + theta11d(j)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*theta22d(jk)*(delth1*cosp1d(i) + theta11d(i)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*cosp2d(ij)*(delth1*theta21d(k) + theta11d(k)*delth2)
                e3d(ii) = e3d(ii) + rkfor*cosp2d(ik)*(delth1*theta21d(j) + theta11d(j)*delth2)
                e3d(ii) = e3d(ii) + rkfor*cosp2d(jk)*(delth1*theta21d(i) + theta11d(i)*delth2)
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.11) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Inversion out of plane potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ctrm11 = (1.0_dp - cos1*cos1)
    ctrm21 = (1.0_dp - cos1*cos1)
    ctrm31 = (1.0_dp - cos2*cos2)
    ctrm12 = (1.0_dp - cosphi1*cosphi1)
    ctrm22 = (1.0_dp - cosphi2*cosphi2)
    ctrm32 = (1.0_dp - cosphi3*cosphi3)
    cospsi1 = sqrt(1.0_dp - ctrm11*ctrm12)
    cospsi2 = sqrt(1.0_dp - ctrm21*ctrm22)
    cospsi3 = sqrt(1.0_dp - ctrm31*ctrm32)
    rkover3 = rkfor/3.0_dp
    efor = rkover3*(3.0_dp - cospsi1 - cospsi2 - cospsi3)
    if (lgrad1loc) then
!
!  First derivatives
!
      rcospsi1 = 1.0_dp/cospsi1
      rcospsi2 = 1.0_dp/cospsi2
      rcospsi3 = 1.0_dp/cospsi3
      do k = 1,6
        cospsi1d1(k) = rcospsi1*(ctrm11*cosphi1*cosp1d1(k) + ctrm12*cos1*cos11d(k))
        cospsi2d1(k) = rcospsi2*(ctrm21*cosphi2*cosp1d2(k) + ctrm22*cos1*cos11d(k))
        cospsi3d1(k) = rcospsi3*(ctrm31*cosphi3*cosp1d3(k) + ctrm32*cos2*cos21d(k))
      enddo
      do k = 1,6
        e1d(k) = - rkover3*(cospsi1d1(k) + cospsi2d1(k) + cospsi3d1(k))
      enddo
      if (lgrad2loc) then
!
!  Second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            cospsi1d2(ii) = rcospsi1*(ctrm11*(cosp1d1(i)*cosp1d1(j)+cosphi1*cosp2d1(ii)) &
              + ctrm12*(cos11d(i)*cos11d(j) + cos1*cos12d(ii))  &
              - 2.0_dp*cos1*cosphi1*(cosp1d1(i)*cos11d(j) + cosp1d1(j)*cos11d(i)) - cospsi1d1(i)*cospsi1d1(j))
            cospsi2d2(ii) = rcospsi2*(ctrm21*(cosp1d2(i)*cosp1d2(j)+cosphi2*cosp2d2(ii)) &
              + ctrm22*(cos11d(i)*cos11d(j) + cos1*cos12d(ii))  &
              - 2.0_dp*cos1*cosphi2*(cosp1d2(i)*cos11d(j) + cosp1d2(j)*cos11d(i)) - cospsi2d1(i)*cospsi2d1(j))
            cospsi3d2(ii) = rcospsi3*(ctrm31*(cosp1d3(i)*cosp1d3(j)+cosphi3*cosp2d3(ii)) &
              + ctrm32*(cos21d(i)*cos21d(j) + cos2*cos22d(ii))  &
              - 2.0_dp*cos2*cosphi3*(cosp1d3(i)*cos21d(j) + cosp1d3(j)*cos21d(i)) - cospsi3d1(i)*cospsi3d1(j))
          enddo
        enddo
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = - rkover3*(cospsi1d2(ii) + cospsi2d2(ii) + cospsi3d2(ii))
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                cospsi1d3(ii) = - rcospsi1*(cospsi1d1(i)*cospsi1d2(jk)+cospsi1d1(j)*cospsi1d2(ik)+cospsi1d1(k)*cospsi1d2(ij))
                cospsi2d3(ii) = - rcospsi2*(cospsi2d1(i)*cospsi2d2(jk)+cospsi2d1(j)*cospsi2d2(ik)+cospsi2d1(k)*cospsi2d2(ij))
                cospsi3d3(ii) = - rcospsi3*(cospsi3d1(i)*cospsi3d2(jk)+cospsi3d1(j)*cospsi3d2(ik)+cospsi3d1(k)*cospsi3d2(ij))
!
                cospsi1d3(ii) = cospsi1d3(ii) + rcospsi1*( &
                  ctrm11*(cosp2d1(ij)*cosp1d1(k)+cosp2d1(ik)*cosp1d1(j)+cosp2d1(jk)*cosp1d1(i)+cosphi1*cosp3d1(ii))  &
                  + ctrm12*(cos12d(ij)*cos11d(k)+cos12d(ik)*cos11d(j)+cos12d(jk)*cos11d(i)+cos1*cos13d(ii))  &
                  - 2.0_dp*cos1*(cos11d(k)*cosp1d1(i)*cosp1d1(j)+cos11d(j)*cosp1d1(i)*cosp1d1(k) &
                    +cos11d(i)*cosp1d1(j)*cosp1d1(k)) &
                  - 2.0_dp*cosphi1*(cosp1d1(k)*cos11d(i)*cos11d(j)+cosp1d1(j)*cos11d(i)*cos11d(k) &
                    +cosp1d1(i)*cos11d(j)*cos11d(k)) &
                  - 2.0_dp*cos1*cosphi1*(cos11d(k)*cosp2d1(ij)+cos11d(j)*cosp2d1(ik)+cos11d(i)*cosp2d1(jk)) &
                  - 2.0_dp*cos1*cosphi1*(cosp1d1(k)*cos12d(ij)+cosp1d1(j)*cos12d(ik)+cosp1d1(i)*cos12d(jk)))
!
                cospsi2d3(ii) = cospsi2d3(ii) + rcospsi2*( &
                  ctrm21*(cosp2d2(ij)*cosp1d2(k)+cosp2d2(ik)*cosp1d2(j)+cosp2d2(jk)*cosp1d2(i)+cosphi2*cosp3d2(ii))  &
                  + ctrm22*(cos12d(ij)*cos11d(k)+cos12d(ik)*cos11d(j)+cos12d(jk)*cos11d(i)+cos1*cos13d(ii))  &
                  - 2.0_dp*cos1*(cos11d(k)*cosp1d2(i)*cosp1d2(j)+cos11d(j)*cosp1d2(i)*cosp1d2(k) &
                    +cos11d(i)*cosp1d2(j)*cosp1d2(k)) &
                  - 2.0_dp*cosphi2*(cosp1d2(k)*cos11d(i)*cos11d(j)+cosp1d2(j)*cos11d(i)*cos11d(k) &
                    +cosp1d2(i)*cos11d(j)*cos11d(k)) &
                  - 2.0_dp*cos1*cosphi2*(cos11d(k)*cosp2d2(ij)+cos11d(j)*cosp2d2(ik)+cos11d(i)*cosp2d2(jk)) &
                  - 2.0_dp*cos1*cosphi2*(cosp1d2(k)*cos12d(ij)+cosp1d2(j)*cos12d(ik)+cosp1d2(i)*cos12d(jk)))
!
                cospsi3d3(ii) = cospsi3d3(ii) + rcospsi3*( &
                  ctrm31*(cosp2d3(ij)*cosp1d3(k)+cosp2d3(ik)*cosp1d3(j)+cosp2d3(jk)*cosp1d3(i)+cosphi3*cosp3d3(ii))  &
                  + ctrm32*(cos22d(ij)*cos21d(k)+cos22d(ik)*cos21d(j)+cos22d(jk)*cos21d(i)+cos2*cos23d(ii))  &
                  - 2.0_dp*cos2*(cos21d(k)*cosp1d3(i)*cosp1d3(j)+cos21d(j)*cosp1d3(i)*cosp1d3(k) &
                    +cos21d(i)*cosp1d3(j)*cosp1d3(k)) &
                  - 2.0_dp*cosphi3*(cosp1d3(k)*cos21d(i)*cos21d(j)+cosp1d3(j)*cos21d(i)*cos21d(k) &
                    +cosp1d3(i)*cos21d(j)*cos21d(k)) &
                  - 2.0_dp*cos2*cosphi3*(cos21d(k)*cosp2d3(ij)+cos21d(j)*cosp2d3(ik)+cos21d(i)*cosp2d3(jk)) &
                  - 2.0_dp*cos2*cosphi3*(cosp1d3(k)*cos22d(ij)+cosp1d3(j)*cos22d(ik)+cosp1d3(i)*cos22d(jk)))
              enddo
            enddo
          enddo
          ii = 0
          do i = 1,6
            do j = i,6
              do k = j,6
                ii = ii + 1
                e3d(ii) = - rkover3*(cospsi1d3(ii) + cospsi2d3(ii) + cospsi3d3(ii))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.12) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Inversion squared out of plane potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ctrm11 = (1.0_dp - cos1*cos1)
    ctrm21 = (1.0_dp - cos1*cos1)
    ctrm31 = (1.0_dp - cos2*cos2)
    ctrm12 = (1.0_dp - cosphi1*cosphi1)
    ctrm22 = (1.0_dp - cosphi2*cosphi2)
    ctrm32 = (1.0_dp - cosphi3*cosphi3)
    cospsi1 = sqrt(1.0_dp - ctrm11*ctrm12)
    cospsi2 = sqrt(1.0_dp - ctrm21*ctrm22)
    cospsi3 = sqrt(1.0_dp - ctrm31*ctrm32)
    k0 = fpoly(1)
    cosk0 = cos(k0)
    rkover3 = 0.5_dp*rkfor/(3.0_dp*sin(k0)**2)
    efor = rkover3*((cospsi1 - cosk0)**2 + (cospsi2 - cosk0)**2 + (cospsi3 - cosk0)**2)
    if (lgrad1loc) then
!
!  First derivatives
!
      rcospsi1 = 1.0_dp/cospsi1
      rcospsi2 = 1.0_dp/cospsi2
      rcospsi3 = 1.0_dp/cospsi3
      do k = 1,6
        cospsi1d1(k) = rcospsi1*(ctrm11*cosphi1*cosp1d1(k) + ctrm12*cos1*cos11d(k))
        cospsi2d1(k) = rcospsi2*(ctrm21*cosphi2*cosp1d2(k) + ctrm22*cos1*cos11d(k))
        cospsi3d1(k) = rcospsi3*(ctrm31*cosphi3*cosp1d3(k) + ctrm32*cos2*cos21d(k))
      enddo
      do k = 1,6
        e1d(k) = 2.0_dp*rkover3*((cospsi1 - cosk0)*cospsi1d1(k) + (cospsi2 - cosk0)*cospsi2d1(k) +  &
          (cospsi3 - cosk0)*cospsi3d1(k))
      enddo
      if (lgrad2loc) then
!
!  Second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            cospsi1d2(ii) = rcospsi1*(ctrm11*(cosp1d1(i)*cosp1d1(j)+cosphi1*cosp2d1(ii)) &
              + ctrm12*(cos11d(i)*cos11d(j) + cos1*cos12d(ii))  &
              - 2.0_dp*cos1*cosphi1*(cosp1d1(i)*cos11d(j) + cosp1d1(j)*cos11d(i)) - cospsi1d1(i)*cospsi1d1(j))
            cospsi2d2(ii) = rcospsi2*(ctrm21*(cosp1d2(i)*cosp1d2(j)+cosphi2*cosp2d2(ii)) &
              + ctrm22*(cos11d(i)*cos11d(j) + cos1*cos12d(ii))  &
              - 2.0_dp*cos1*cosphi2*(cosp1d2(i)*cos11d(j) + cosp1d2(j)*cos11d(i)) - cospsi2d1(i)*cospsi2d1(j))
            cospsi3d2(ii) = rcospsi3*(ctrm31*(cosp1d3(i)*cosp1d3(j)+cosphi3*cosp2d3(ii)) &
              + ctrm32*(cos21d(i)*cos21d(j) + cos2*cos22d(ii))  &
              - 2.0_dp*cos2*cosphi3*(cosp1d3(i)*cos21d(j) + cosp1d3(j)*cos21d(i)) - cospsi3d1(i)*cospsi3d1(j))
          enddo
        enddo
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = 2.0_dp*rkover3*((cospsi1 - cosk0)*cospsi1d2(ii) + (cospsi2 - cosk0)*cospsi2d2(ii) +  &
                                      (cospsi3 - cosk0)*cospsi3d2(ii))
            e2d(ii) = e2d(ii) + 2.0_dp*rkover3*(cospsi1d1(i)*cospsi1d1(j) + cospsi2d1(i)*cospsi2d1(j) +  &
                                                cospsi3d1(i)*cospsi3d1(j))
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                cospsi1d3(ii) = - rcospsi1*(cospsi1d1(i)*cospsi1d2(jk)+cospsi1d1(j)*cospsi1d2(ik)+cospsi1d1(k)*cospsi1d2(ij))
                cospsi2d3(ii) = - rcospsi2*(cospsi2d1(i)*cospsi2d2(jk)+cospsi2d1(j)*cospsi2d2(ik)+cospsi2d1(k)*cospsi2d2(ij))
                cospsi3d3(ii) = - rcospsi3*(cospsi3d1(i)*cospsi3d2(jk)+cospsi3d1(j)*cospsi3d2(ik)+cospsi3d1(k)*cospsi3d2(ij))
!
                cospsi1d3(ii) = cospsi1d3(ii) + rcospsi1*( &
                  ctrm11*(cosp2d1(ij)*cosp1d1(k)+cosp2d1(ik)*cosp1d1(j)+cosp2d1(jk)*cosp1d1(i)+cosphi1*cosp3d1(ii))  &
                  + ctrm12*(cos12d(ij)*cos11d(k)+cos12d(ik)*cos11d(j)+cos12d(jk)*cos11d(i)+cos1*cos13d(ii))  &
                  - 2.0_dp*cos1*(cos11d(k)*cosp1d1(i)*cosp1d1(j)+cos11d(j)*cosp1d1(i)*cosp1d1(k) &
                    +cos11d(i)*cosp1d1(j)*cosp1d1(k)) &
                  - 2.0_dp*cosphi1*(cosp1d1(k)*cos11d(i)*cos11d(j)+cosp1d1(j)*cos11d(i)*cos11d(k) &
                    +cosp1d1(i)*cos11d(j)*cos11d(k)) &
                  - 2.0_dp*cos1*cosphi1*(cos11d(k)*cosp2d1(ij)+cos11d(j)*cosp2d1(ik)+cos11d(i)*cosp2d1(jk)) &
                  - 2.0_dp*cos1*cosphi1*(cosp1d1(k)*cos12d(ij)+cosp1d1(j)*cos12d(ik)+cosp1d1(i)*cos12d(jk)))
!
                cospsi2d3(ii) = cospsi2d3(ii) + rcospsi2*( &
                  ctrm21*(cosp2d2(ij)*cosp1d2(k)+cosp2d2(ik)*cosp1d2(j)+cosp2d2(jk)*cosp1d2(i)+cosphi2*cosp3d2(ii))  &
                  + ctrm22*(cos12d(ij)*cos11d(k)+cos12d(ik)*cos11d(j)+cos12d(jk)*cos11d(i)+cos1*cos13d(ii))  &
                  - 2.0_dp*cos1*(cos11d(k)*cosp1d2(i)*cosp1d2(j)+cos11d(j)*cosp1d2(i)*cosp1d2(k) &
                    +cos11d(i)*cosp1d2(j)*cosp1d2(k)) &
                  - 2.0_dp*cosphi2*(cosp1d2(k)*cos11d(i)*cos11d(j)+cosp1d2(j)*cos11d(i)*cos11d(k) &
                    +cosp1d2(i)*cos11d(j)*cos11d(k)) &
                  - 2.0_dp*cos1*cosphi2*(cos11d(k)*cosp2d2(ij)+cos11d(j)*cosp2d2(ik)+cos11d(i)*cosp2d2(jk)) &
                  - 2.0_dp*cos1*cosphi2*(cosp1d2(k)*cos12d(ij)+cosp1d2(j)*cos12d(ik)+cosp1d2(i)*cos12d(jk)))
!
                cospsi3d3(ii) = cospsi3d3(ii) + rcospsi3*( &
                  ctrm31*(cosp2d3(ij)*cosp1d3(k)+cosp2d3(ik)*cosp1d3(j)+cosp2d3(jk)*cosp1d3(i)+cosphi3*cosp3d3(ii))  &
                  + ctrm32*(cos22d(ij)*cos21d(k)+cos22d(ik)*cos21d(j)+cos22d(jk)*cos21d(i)+cos2*cos23d(ii))  &
                  - 2.0_dp*cos2*(cos21d(k)*cosp1d3(i)*cosp1d3(j)+cos21d(j)*cosp1d3(i)*cosp1d3(k) &
                    +cos21d(i)*cosp1d3(j)*cosp1d3(k)) &
                  - 2.0_dp*cosphi3*(cosp1d3(k)*cos21d(i)*cos21d(j)+cosp1d3(j)*cos21d(i)*cos21d(k) &
                    +cosp1d3(i)*cos21d(j)*cos21d(k)) &
                  - 2.0_dp*cos2*cosphi3*(cos21d(k)*cosp2d3(ij)+cos21d(j)*cosp2d3(ik)+cos21d(i)*cosp2d3(jk)) &
                  - 2.0_dp*cos2*cosphi3*(cosp1d3(k)*cos22d(ij)+cosp1d3(j)*cos22d(ik)+cosp1d3(i)*cos22d(jk)))
              enddo
            enddo
          enddo
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = 2.0_dp*rkover3*((cospsi1 - cosk0)*cospsi1d3(ii) + (cospsi2 - cosk0)*cospsi2d3(ii) +  &
                                          (cospsi3 - cosk0)*cospsi3d3(ii))
                e3d(ii) = e3d(ii) + 2.0_dp*rkover3*(cospsi1d1(i)*cospsi1d2(jk) + cospsi2d1(i)*cospsi2d2(jk) +  &
                                                    cospsi3d1(i)*cospsi3d2(jk))
                e3d(ii) = e3d(ii) + 2.0_dp*rkover3*(cospsi1d1(j)*cospsi1d2(ik) + cospsi2d1(j)*cospsi2d2(ik) +  &
                                                    cospsi3d1(j)*cospsi3d2(ik))
                e3d(ii) = e3d(ii) + 2.0_dp*rkover3*(cospsi1d1(k)*cospsi1d2(ij) + cospsi2d1(k)*cospsi2d2(ij) +  &
                                                    cospsi3d1(k)*cospsi3d2(ij))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.13) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  UFF4 torsional potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    phi = acos(cosphi)
    cphi0 = cos(rn*phi0)
    cosnphi = cos(rn*phi)
    efor = 0.5_dp*rkfor*(1.0_dp - cosnphi*cphi0)
    if (lgrad1loc) then
!
!  First derivatives of energy
!
!  d(cosnphi)/dcosphi = sinnphi/sinphi
!
!  to avoid divide by zero error when sinphi = 0 we use a series
!  expansion in cosphi
!
      e1 = - 0.5_dp*rkfor*rn*cphi0
      e2 = e1
      e3 = e1
      nmax = nint(rn)
      cosd1 = sinratio(nmax,phi)
      e1 = e1*cosd1
      do k = 1,6
        e1d(k) = e1*cosp1d(k)
      enddo
      if (lgrad2loc) then
!
!  Second derivatives of energy
!
        cosd2 = dsinratio(nmax,phi)
        e2 = e2*cosd2
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = e2*cosp1d(i)*cosp1d(j) + e1*cosp2d(ii)
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives of energy
!
          cosd3 = 0.0_dp
          if (nmax.gt.1) then
            cosd3 = dble(nmax-1)*dsinratio((nmax-1_i4),phi)
            cosd3 = cosd3 + dble(nmax-2)*cosphi*dsinratio((nmax-2_i4),phi)
            cosd3 = cosd3 + 2.0_dp*dble(nmax-2)*sinratio((nmax-2_i4),phi)
            cosphim = 1.0_dp
            do k = 3,nmax
              cosmphi = cos(dble(nmax-k)*phi)
              cosd3 = cosd3 + dble((k-2)*(k-1))*cosphim*cosmphi
              cosphim = cosphim*cosphi
              cosd3 = cosd3 + dble(2*(k-1)*(nmax-k))*cosphim*sinratio((nmax-k),phi)
              cosd3 = cosd3 + dble(nmax-k)*cosphim*cosphi*dsinratio((nmax-k),phi)
            enddo
            e3 = e3*cosd3
          endif
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = e3*cosp1d(i)*cosp1d(j)*cosp1d(k)
                e3d(ii) = e3d(ii) + e1*cosp3d(ii)
                e3d(ii) = e3d(ii) + e2*cosp1d(i)*cosp2d(jk)
                e3d(ii) = e3d(ii) + e2*cosp1d(j)*cosp2d(ik)
                e3d(ii) = e3d(ii) + e2*cosp1d(k)*cosp2d(ij)
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.14) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Angle-angle cross potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    k1 = rkfor
    k2 = fpoly(1)
    k3 = fpoly(2)
    theta0_1 = fpoly(3)
    theta0_2 = fpoly(4)
    theta0_3 = fpoly(5)
    ttrm1 = theta1 - theta0_1
    ttrm2 = theta2 - theta0_2
    ttrm3 = theta3 - theta0_3
    efor = k1*ttrm1*ttrm2 + k2*ttrm1*ttrm3 + k3*ttrm2*ttrm3
    if (lgrad1loc) then
!
!  First derivatives
!
      do k = 1,6
        e1d(k) = k1*(ttrm1*theta21d(k) + ttrm2*theta11d(k)) + &
                 k2*(ttrm1*theta31d(k) + ttrm3*theta11d(k)) + &
                 k3*(ttrm2*theta31d(k) + ttrm3*theta21d(k))
      enddo
      if (lgrad2loc) then
!
!  Second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = k1*(ttrm1*theta22d(ii) + ttrm2*theta12d(ii) + theta11d(j)*theta21d(i) + theta11d(i)*theta21d(j)) + &
                      k2*(ttrm1*theta32d(ii) + ttrm3*theta12d(ii) + theta11d(j)*theta31d(i) + theta11d(i)*theta31d(j)) + &
                      k3*(ttrm2*theta32d(ii) + ttrm3*theta22d(ii) + theta21d(j)*theta31d(i) + theta21d(i)*theta31d(j))
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = k1*(ttrm1*theta23d(ii) + ttrm2*theta13d(ii) + &
                              theta11d(i)*theta22d(jk) + theta11d(j)*theta22d(ik) + theta11d(k)*theta22d(ij) + &
                              theta21d(i)*theta12d(jk) + theta21d(j)*theta12d(ik) + theta21d(k)*theta12d(ij)) + &
                          k2*(ttrm1*theta33d(ii) + ttrm3*theta13d(ii) + &
                              theta11d(i)*theta32d(jk) + theta11d(j)*theta32d(ik) + theta11d(k)*theta32d(ij) + &
                              theta31d(i)*theta12d(jk) + theta31d(j)*theta12d(ik) + theta31d(k)*theta12d(ij)) + &
                          k3*(ttrm3*theta23d(ii) + ttrm2*theta33d(ii) + &
                              theta31d(i)*theta22d(jk) + theta31d(j)*theta22d(ik) + theta31d(k)*theta22d(ij) + &
                              theta21d(i)*theta32d(jk) + theta21d(j)*theta32d(ik) + theta21d(k)*theta32d(ij))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.15) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  UFF out of plane potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ctrm11 = cos1*cos1 + sin1*sin1*cosphi1*cosphi1
    ctrm21 = cos1*cos1 + sin1*sin1*cosphi2*cosphi2
    ctrm31 = cos2*cos2 + sin2*sin2*cosphi3*cosphi3
    cospsi1 = sqrt(ctrm11)
    cospsi2 = sqrt(ctrm21)
    cospsi3 = sqrt(ctrm31)
!
!  Determine signs of cosines of psi
!
    if ((cos1+cos2).gt.0.0_dp) then
      cospsi1 = - cospsi1
    endif
    if ((cos1+cos3).gt.0.0_dp) then
      cospsi2 = - cospsi2
    endif
    if ((cos2+cos3).gt.0.0_dp) then
      cospsi3 = - cospsi3
    endif
!
    cos2psi1 = 2.0_dp*ctrm11 - 1.0_dp
    cos2psi2 = 2.0_dp*ctrm21 - 1.0_dp
    cos2psi3 = 2.0_dp*ctrm31 - 1.0_dp
    rkover3 = rkfor/3.0_dp
    c0 = fpoly(1)
    c1 = fpoly(2)
    c2 = fpoly(3)
    efor = rkover3*(3.0_dp*c0 + c1*(cospsi1 + cospsi2 + cospsi3) + c2*(cos2psi1 + cos2psi2 + cos2psi3))
!
    if (lgrad1loc) then
!
!  First derivatives
!
      rcospsi1 = 1.0_dp/cospsi1
      rcospsi2 = 1.0_dp/cospsi2
      rcospsi3 = 1.0_dp/cospsi3
      do k = 1,6
        cospsi1d1(k) = rcospsi1*(cos1*cos11d(k) + sin1*sin11d(k)*cosphi1*cosphi1 + sin1*sin1*cosphi1*cosp1d1(k))
        cospsi2d1(k) = rcospsi2*(cos1*cos11d(k) + sin1*sin11d(k)*cosphi2*cosphi2 + sin1*sin1*cosphi2*cosp1d2(k))
        cospsi3d1(k) = rcospsi3*(cos2*cos21d(k) + sin2*sin21d(k)*cosphi3*cosphi3 + sin2*sin2*cosphi3*cosp1d3(k))
!
        cos2psi1d1(k) = 4.0_dp*cospsi1*cospsi1d1(k)
        cos2psi2d1(k) = 4.0_dp*cospsi2*cospsi2d1(k)
        cos2psi3d1(k) = 4.0_dp*cospsi3*cospsi3d1(k)
      enddo
      do k = 1,6
        e1d(k) = rkover3*(c1*(cospsi1d1(k) + cospsi2d1(k) + cospsi3d1(k)) + &
                          c2*(cos2psi1d1(k) + cos2psi2d1(k) + cos2psi3d1(k))) 
      enddo
      if (lgrad2loc) then
!
!  Second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            cospsi1d2(ii) = rcospsi1*(cos1*cos12d(ii) + cos11d(i)*cos11d(j) + &
                                      cosphi1*cosphi1*(sin1*sin12d(ii) + sin11d(i)*sin11d(j)) + &
                                      sin1*sin1*(cosphi1*cosp2d1(ii) + cosp1d1(i)*cosp1d1(j)) + &
                                      2.0_dp*sin1*cosphi1*(sin11d(i)*cosp1d1(j) + sin11d(j)*cosp1d1(i)) - &
                                      cospsi1d1(i)*cospsi1d1(j))
            cospsi2d2(ii) = rcospsi2*(cos1*cos12d(ii) + cos11d(i)*cos11d(j) + &
                                      cosphi2*cosphi2*(sin1*sin12d(ii) + sin11d(i)*sin11d(j)) + &
                                      sin1*sin1*(cosphi2*cosp2d2(ii) + cosp1d2(i)*cosp1d2(j)) + &
                                      2.0_dp*sin1*cosphi2*(sin11d(i)*cosp1d2(j) + sin11d(j)*cosp1d2(i)) - &
                                      cospsi2d1(i)*cospsi2d1(j))
            cospsi3d2(ii) = rcospsi3*(cos2*cos22d(ii) + cos21d(i)*cos21d(j) + &
                                      cosphi3*cosphi3*(sin2*sin22d(ii) + sin21d(i)*sin21d(j)) + &
                                      sin2*sin2*(cosphi3*cosp2d3(ii) + cosp1d3(i)*cosp1d3(j)) + &
                                      2.0_dp*sin2*cosphi3*(sin21d(i)*cosp1d3(j) + sin21d(j)*cosp1d3(i)) - &
                                      cospsi3d1(i)*cospsi3d1(j))
!
            cos2psi1d2(ii) = 4.0_dp*(cospsi1*cospsi1d2(ii) + cospsi1d1(i)*cospsi1d1(j))
            cos2psi2d2(ii) = 4.0_dp*(cospsi2*cospsi2d2(ii) + cospsi2d1(i)*cospsi2d1(j))
            cos2psi3d2(ii) = 4.0_dp*(cospsi3*cospsi3d2(ii) + cospsi3d1(i)*cospsi3d1(j))
          enddo
        enddo
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = rkover3*(c1*(cospsi1d2(ii) + cospsi2d2(ii) + cospsi3d2(ii)) + &
                               c2*(cos2psi1d2(ii) + cos2psi2d2(ii) + cos2psi3d2(ii)))
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                cospsi1d3(ii) = - rcospsi1*(cospsi1d1(i)*cospsi1d2(jk)+cospsi1d1(j)*cospsi1d2(ik)+cospsi1d1(k)*cospsi1d2(ij))
                cospsi2d3(ii) = - rcospsi2*(cospsi2d1(i)*cospsi2d2(jk)+cospsi2d1(j)*cospsi2d2(ik)+cospsi2d1(k)*cospsi2d2(ij))
                cospsi3d3(ii) = - rcospsi3*(cospsi3d1(i)*cospsi3d2(jk)+cospsi3d1(j)*cospsi3d2(ik)+cospsi3d1(k)*cospsi3d2(ij))
!
                cospsi1d3(ii) = cospsi1d3(ii) + rcospsi1*( &
                  cos1*cos13d(ii) + cos11d(i)*cos12d(jk) + cos11d(j)*cos12d(ik) + cos11d(k)*cos12d(ij) + &
                  cosphi1*cosphi1*(sin1*sin13d(ii) + sin11d(i)*sin12d(jk) + sin11d(j)*sin12d(ik) + sin11d(k)*sin12d(ij)) + &
                  sin1*sin1*(cosphi1*cosp3d1(ii) + cosp1d1(i)*cosp2d1(jk) + cosp1d1(j)*cosp2d1(ik) + cosp1d1(k)*cosp2d1(ij)) + &
                  2.0_dp*sin1*cosphi1*(sin11d(i)*cosp2d1(jk) + sin11d(j)*cosp2d1(ik) + sin11d(k)*cosp2d1(ij) + &
                                       cosp1d1(i)*sin12d(jk) + cosp1d1(j)*sin12d(ik) + cosp1d1(k)*sin12d(ij)) + &
                  2.0_dp*cosphi1*(cosp1d1(i)*sin11d(j)*sin11d(k) + cosp1d1(j)*sin11d(i)*sin11d(k) + &
                                  cosp1d1(k)*sin11d(i)*sin11d(j)) + &
                  2.0_dp*sin1*(sin11d(i)*cosp1d1(j)*cosp1d1(k) + sin11d(j)*cosp1d1(i)*cosp1d1(k) + &
                               sin11d(k)*cosp1d1(i)*cosp1d1(j)))
!
                cospsi2d3(ii) = cospsi2d3(ii) + rcospsi2*( &
                  cos1*cos13d(ii) + cos11d(i)*cos12d(jk) + cos11d(j)*cos12d(ik) + cos11d(k)*cos12d(ij) + &
                  cosphi2*cosphi2*(sin1*sin13d(ii) + sin11d(i)*sin12d(jk) + sin11d(j)*sin12d(ik) + sin11d(k)*sin12d(ij)) + &
                  sin1*sin1*(cosphi2*cosp3d2(ii) + cosp1d2(i)*cosp2d2(jk) + cosp1d2(j)*cosp2d2(ik) + cosp1d2(k)*cosp2d2(ij)) + &
                  2.0_dp*sin1*cosphi2*(sin11d(i)*cosp2d2(jk) + sin11d(j)*cosp2d2(ik) + sin11d(k)*cosp2d2(ij) + &
                                       cosp1d2(i)*sin12d(jk) + cosp1d2(j)*sin12d(ik) + cosp1d2(k)*sin12d(ij)) + &
                  2.0_dp*cosphi2*(cosp1d2(i)*sin11d(j)*sin11d(k) + cosp1d2(j)*sin11d(i)*sin11d(k) + &
                                  cosp1d2(k)*sin11d(i)*sin11d(j)) + &
                  2.0_dp*sin1*(sin11d(i)*cosp1d2(j)*cosp1d2(k) + sin11d(j)*cosp1d2(i)*cosp1d2(k) + &
                               sin11d(k)*cosp1d2(i)*cosp1d2(j)))
!
                cospsi3d3(ii) = cospsi3d3(ii) + rcospsi3*( &
                  cos2*cos23d(ii) + cos21d(i)*cos22d(jk) + cos21d(j)*cos22d(ik) + cos21d(k)*cos22d(ij) + &
                  cosphi3*cosphi3*(sin2*sin23d(ii) + sin21d(i)*sin22d(jk) + sin21d(j)*sin22d(ik) + sin21d(k)*sin22d(ij)) + &
                  sin2*sin2*(cosphi3*cosp3d3(ii) + cosp1d3(i)*cosp2d3(jk) + cosp1d3(j)*cosp2d3(ik) + cosp1d3(k)*cosp2d3(ij)) + &
                  2.0_dp*sin2*cosphi3*(sin21d(i)*cosp2d3(jk) + sin21d(j)*cosp2d3(ik) + sin21d(k)*cosp2d3(ij) + &
                                       cosp1d3(i)*sin22d(jk) + cosp1d3(j)*sin22d(ik) + cosp1d3(k)*sin22d(ij)) + &
                  2.0_dp*cosphi3*(cosp1d3(i)*sin21d(j)*sin21d(k) + cosp1d3(j)*sin21d(i)*sin21d(k) + &
                                  cosp1d3(k)*sin21d(i)*sin21d(j)) + &
                  2.0_dp*sin2*(sin21d(i)*cosp1d3(j)*cosp1d3(k) + sin21d(j)*cosp1d3(i)*cosp1d3(k) + &
                               sin21d(k)*cosp1d3(i)*cosp1d3(j)))
!
                cos2psi1d3(ii) = 4.0_dp*(cospsi1*cospsi1d3(ii) + cospsi1d1(i)*cospsi1d2(jk) + &
                                         cospsi1d1(j)*cospsi1d2(ik) + cospsi1d1(k)*cospsi1d2(ij))
                cos2psi2d3(ii) = 4.0_dp*(cospsi2*cospsi2d3(ii) + cospsi2d1(i)*cospsi2d2(jk) + &
                                         cospsi1d1(j)*cospsi1d2(ik) + cospsi1d1(k)*cospsi1d2(ij))
                cos2psi3d3(ii) = 4.0_dp*(cospsi3*cospsi3d3(ii) + cospsi3d1(i)*cospsi3d2(jk) + &
                                         cospsi1d1(j)*cospsi1d2(ik) + cospsi1d1(k)*cospsi1d2(ij))
              enddo
            enddo
          enddo
          ii = 0
          do i = 1,6
            do j = i,6
              do k = j,6
                ii = ii + 1
                e3d(ii) = rkover3*(c1*(cospsi1d3(ii) + cospsi2d3(ii) + cospsi3d3(ii)) + &
                                   c2*(cos2psi1d3(ii) + cos2psi2d3(ii) + cos2psi3d3(ii)))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.16) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Cosine angle-cosine angle cross potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    k1 = rkfor
    k2 = fpoly(1)
    k3 = fpoly(2)
    theta0_1 = cos(fpoly(3))
    theta0_2 = cos(fpoly(4))
    theta0_3 = cos(fpoly(5))
    ttrm1 = cos1 - theta0_1
    ttrm2 = cos2 - theta0_2
    ttrm3 = cos3 - theta0_3
    efor = k1*ttrm1*ttrm2 + k2*ttrm1*ttrm3 + k3*ttrm2*ttrm3
    if (lgrad1loc) then
!
!  First derivatives
!
      do k = 1,6
        e1d(k) = k1*(ttrm1*cos21d(k) + ttrm2*cos11d(k)) + &
                 k2*(ttrm1*cos31d(k) + ttrm3*cos11d(k)) + &
                 k3*(ttrm2*cos31d(k) + ttrm3*cos21d(k))
      enddo
      if (lgrad2loc) then
!
!  Second derivatives
!
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = k1*(ttrm1*cos22d(ii) + ttrm2*cos12d(ii) + cos11d(j)*cos21d(i) + cos11d(i)*cos21d(j)) + &
                      k2*(ttrm1*cos32d(ii) + ttrm3*cos12d(ii) + cos11d(j)*cos31d(i) + cos11d(i)*cos31d(j)) + &
                      k3*(ttrm2*cos32d(ii) + ttrm3*cos22d(ii) + cos21d(j)*cos31d(i) + cos21d(i)*cos31d(j))
          enddo
        enddo
        if (lgrad3loc) then
!
!  Third derivatives
!
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = k1*(ttrm1*cos23d(ii) + ttrm2*cos13d(ii) + &
                              cos11d(i)*cos22d(jk) + cos11d(j)*cos22d(ik) + cos11d(k)*cos22d(ij) + &
                              cos21d(i)*cos12d(jk) + cos21d(j)*cos12d(ik) + cos21d(k)*cos12d(ij)) + &
                          k2*(ttrm1*cos33d(ii) + ttrm3*cos13d(ii) + &
                              cos11d(i)*cos32d(jk) + cos11d(j)*cos32d(ik) + cos11d(k)*cos32d(ij) + &
                              cos31d(i)*cos12d(jk) + cos31d(j)*cos12d(ik) + cos31d(k)*cos12d(ij)) + &
                          k3*(ttrm3*cos23d(ii) + ttrm2*cos33d(ii) + &
                              cos31d(i)*cos22d(jk) + cos31d(j)*cos22d(ik) + cos31d(k)*cos22d(ij) + &
                              cos21d(i)*cos32d(jk) + cos21d(j)*cos32d(ik) + cos21d(k)*cos32d(ij))
              enddo
            enddo
          enddo
        endif
      endif
    endif
  elseif (n4ty.eq.17) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Torsion-cosine angle cross term potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    delth1 = cos1 - cos(fpoly(1))
    delth2 = cos2 - cos(fpoly(2))
    efor = rkfor*delth1*delth2*cosphi
    if (lgrad1loc) then
      do i = 1,6
        e1d(i) = rkfor*((cos11d(i)*delth2 + delth1*cos21d(i))*cosphi + delth1*delth2*cosp1d(i))
      enddo
      if (lgrad2loc) then
        ii = 0
        do i = 1,6
          do j = i,6
            ii = ii + 1
            e2d(ii) = rkfor*((cos12d(ii)*delth2 + delth1*cos22d(ii))*cosphi + delth1*delth2*cosp2d(ii))
            e2d(ii) = e2d(ii) + rkfor*(cos11d(i)*cos21d(j) + cos11d(j)*cos21d(i))*cosphi
            e2d(ii) = e2d(ii) + rkfor*(cos11d(i)*delth2 + delth1*cos21d(i))*cosp1d(j)
            e2d(ii) = e2d(ii) + rkfor*(cos11d(j)*delth2 + delth1*cos21d(j))*cosp1d(i)
          enddo
        enddo
        if (lgrad3loc) then
          ii = 0
          do i = 1,6
            do j = i,6
              ij = kb(j,i)
              do k = j,6
                ii = ii + 1
                ik = kb(k,i)
                jk = kb(k,j)
                e3d(ii) = rkfor*((cos13d(ii)*delth2 + delth1*cos23d(ii))*cosphi + delth1*delth2*cosp3d(ii))
                e3d(ii) = e3d(ii) + rkfor*(cos11d(i)*cos21d(j) + cos11d(j)*cos21d(i))*cosp1d(k)
                e3d(ii) = e3d(ii) + rkfor*(cos11d(i)*cos21d(k) + cos11d(k)*cos21d(i))*cosp1d(j)
                e3d(ii) = e3d(ii) + rkfor*(cos11d(j)*cos21d(k) + cos11d(k)*cos21d(j))*cosp1d(i)
                e3d(ii) = e3d(ii) + rkfor*cos12d(ij)*(delth2*cosp1d(k) + cos21d(k)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*cos12d(ik)*(delth2*cosp1d(j) + cos21d(j)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*cos12d(jk)*(delth2*cosp1d(i) + cos21d(i)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*cos22d(ij)*(delth1*cosp1d(k) + cos11d(k)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*cos22d(ik)*(delth1*cosp1d(j) + cos11d(j)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*cos22d(jk)*(delth1*cosp1d(i) + cos11d(i)*cosphi)
                e3d(ii) = e3d(ii) + rkfor*cosp2d(ij)*(delth1*cos21d(k) + cos11d(k)*delth2)
                e3d(ii) = e3d(ii) + rkfor*cosp2d(ik)*(delth1*cos21d(j) + cos11d(j)*delth2)
                e3d(ii) = e3d(ii) + rkfor*cosp2d(jk)*(delth1*cos21d(i) + cos11d(i)*delth2)
              enddo
            enddo
          enddo
        endif
      endif
    endif
  endif
!
  return
  end
!
  function dsinratio(n,phi)
!
!  Calculates the value of d[sin(n.phi)/sin(phi)]/dcosphi using
!  a series which handles the limit of phi=0 correctly.
!
!  On entry :
!
!  n   = value of multiple angle (integer)
!  phi = angle (real*8)
!
!  On exit :
!
!  sinratio = first derivative of sin(n.phi)/sin(phi) (real*8)
!             with respect to cos(phi)
!  n and phi are unchanged
!
!   7/98 Created from part of fourbody
!  12/02 Rewritten for greater clarity.
!
!  Julian Gale, Curtin University, November 2004
!
  use datatypes
  implicit none
!       
!  Passed variables
!     
  integer(i4), intent(in) :: n
  real(dp),    intent(in) :: phi
  real(dp)                :: dsinratio
!
!  Local variables
!
  integer(i4)             :: k
  real(dp)                :: cosphi
  real(dp)                :: sinratio
  real(dp)                :: sinrationm1
!
  dsinratio = 0.0_dp
  cosphi = cos(phi)
  if (n.gt.1) then
    sinrationm1 = 1.0_dp
    do k = 2,n
      dsinratio = dsinratio*cosphi + dble(k)*sinrationm1
      sinrationm1 = sinratio(k,phi)
    enddo
  endif
!
  return
  end
!
  function sinratio(n,phi)
!
!  Calculates the value of sin(n.phi)/sin(phi) using
!  a series which handles the limit of phi=0 correctly
!
!  On entry :
!
!  n   = value of multiple angle (integer)
!  phi = angle (real*8)
!
!  On exit :
!
!  sinratio = sin(n.phi)/sin(phi) (real*8)
!  n and phi are unchanged
!
!   7/98 Created from part of fourbody
!  12/02 Rewritten for greater clarity.
!
!  Julian Gale, Curtin University, November 2004
!
  use datatypes
  implicit none
!     
!  Passed variables
!     
  integer(i4), intent(in) :: n
  real(dp),    intent(in) :: phi
  real(dp)                :: sinratio
!     
!  Local variables
!  
  integer(i4)             :: k
  real(dp)                :: cosphi
  real(dp)                :: cosmphi
!
  sinratio = 1.0_dp
  if (n.gt.1) then
    cosphi = cos(phi)
    do k = 2,n
      cosmphi = cos(dble(k-1)*phi)
      sinratio = sinratio*cosphi + cosmphi
    enddo
  endif
!
  return
  end
!
  function dsinratio2(n,phi)
!
!  Calculates the value of d2[sin(n.phi)/sin(phi)]/dcosphi2 using
!  a series which handles the limit of phi=0 correctly
!
!  On entry :
!
!  n   = value of multiple angle (integer)
!  phi = angle (real*8)
!
!  On exit :
!
!  sinratio = first derivative of sin(n.phi)/sin(phi) (real*8)
!             with respect to cos(phi)
!  n and phi are unchanged
!
!  12/02 Created from dsinratio new version.
!
!  Julian Gale, Curtin University, November 2004
!
  use datatypes
  implicit none
!     
!  Passed variables
!     
  integer(i4), intent(in) :: n
  real(dp),    intent(in) :: phi
  real(dp)                :: dsinratio2
!     
!  Local variables
!  
  integer(i4)             :: k
  real(dp)                :: cosphi
  real(dp)                :: dsinratio
  real(dp)                :: dsinrationm1
!
  dsinratio2 = 0.0_dp
  cosphi = cos(phi)
  if (n.gt.2) then
    dsinrationm1 = 2.0_dp
    do k = 3,n
      dsinratio2 = dsinratio2*cosphi + dble(k+1)*dsinrationm1
      dsinrationm1 = dsinratio(k,phi)
    enddo
  endif
!
  return
  end
