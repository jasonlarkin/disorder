  subroutine threebody(ipivot,n3ty,r12,r13,r23,e1d1,e1d2,e1d3,ethb,e2d,e3d,cut12,cut13,cut23, &
                       rho1,rho2,rho3,rho4,rho5,rkthb,rkthb3,rkthb4,theta0,thet,dot,lgrad1,lgrad2,lgrad3, &
                       nptr3,ql1,ql2,ql3,d0q1,d0q2,d0q3,d1q,d2q,lthetatap,thetamin,thetamax)
!
!  Calculates three-body potential first, second and third derivatives with respect to the three 
!  interatomic distances that make the three body term.
!
!   5/98 Created from three0d3
!   6/98 Murrell-Mottram potential added
!   6/98 Potential number pointer added to call - only used for 3-body
!        coefficients
!   8/98 Third derivatives of Murrell-Mottram added and tested
!  10/98 Bond-angle potential added
!   8/99 Linear-three potential added
!   4/01 Explicit zeroing of derivatives for l180 case added
!   5/01 SW3 modified for 2 different rhos
!  10/02 Bcoscross potential added
!  12/02 Bcoscross changed to include power of cosine and derivatives corrected
!   9/04 Charges passed in as arguments for sw3jb potential
!   9/04 Charge derivatives passed back
!  10/04 Charge derivatives for second derivatives added
!  10/05 Hydrogen-bond potential added
!  11/05 Hydrogen-bond potential correct to go to zero below 90 degrees.
!  12/05 Equatorial ESFF potential added
!   9/06 Theta tapering added
!   9/06 Missing initialisation of d1d1/d1d2/d1d3 added for hydrogen bonding potential
!   1/07 UFF3 potential added
!  11/08 bacoscross potential added
!   3/08 3coulomb potential added
!   6/09 Module name changed from three to m_three
!   7/09 Exp2 potential added
!   7/09 Modifications for exp2 potential added in the form of rho4/rho5 arguments
!   5/10 g3coulomb potential added
!   2/13 BAGcross potential added
!
!  ipivot = pointer to i,j,k according to which is the central atom
!           for an asymmetric potential
!  n3ty   = pointer to type of three-body potential
!  r12    = distance between atoms 1 and 2
!  r13    = distance between atoms 1 and 3
!  r23    = distance between atoms 2 and 3
!  e1d1   = 1/r12 dE/dr12
!  e1d2   = 1/r13 dE/dr13
!  e1d3   = 1/r23 dE/dr23
!  ethb   = contribution to three-body energy
!  e2d    = array of second derivative terms
!  e3d    = array of third derivative terms
!  cut12  = cutoff between atoms 1 and 2
!  cut13  = cutoff between atoms 1 and 3
!  cut23  = cutoff between atoms 2 and 3
!  rho1   = rho value between 1 and 2
!  rho2   = rho value between 1 and 3
!  rho3   = rho value between 2 and 3
!  rkthb  = first parameter associated with potential type
!  rkthb3 = second parameter associated with potential type
!  rkthb4 = third parameter associated with potential type
!  theta0 = parameter associated with potential type
!  thet   = angle
!  dot    = cosine of angle
!  lgrad1 = if .true. calculate the first derivatives
!  lgrad2 = if .true. calculate the second derivatives
!  lgrad3 = if .true. calculate the third derivatives
!  nptr3  = pointer to potential number
!  ql1    = charge of atom 1
!  ql2    = charge of atom 2
!  ql3    = charge of atom 3
!  d0q1   = derivative of energy with respect to charge of atom 1
!  d0q2   = derivative of energy with respect to charge of atom 2
!  d0q3   = derivative of energy with respect to charge of atom 3
!  d1q    = derivatives of first derivative with respect to charges 
!  d2q    = second derivatives of energy with respect to charges 
!  lthetatap = if .true. then theta dependence is to be tapered
!  thetamin  = minimum theta for start of taper
!  thetamax  = maximum theta for end of taper
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
!  Julian Gale, NRI, Curtin University, February 2013
!
  use constants
  use m_three
  use numbers,   only : third
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: ipivot
  integer(i4), intent(in)  :: n3ty
  integer(i4), intent(in)  :: nptr3
  logical,     intent(in)  :: lgrad1
  logical,     intent(in)  :: lgrad2
  logical,     intent(in)  :: lgrad3
  logical,     intent(in)  :: lthetatap
  real(dp),    intent(in)  :: cut12
  real(dp),    intent(in)  :: cut13
  real(dp),    intent(in)  :: cut23
  real(dp),    intent(in)  :: dot
  real(dp),    intent(out) :: d0q1
  real(dp),    intent(out) :: d0q2
  real(dp),    intent(out) :: d0q3
  real(dp),    intent(out) :: d1q(3,3)
  real(dp),    intent(out) :: d2q(6)
  real(dp),    intent(out) :: e1d1
  real(dp),    intent(out) :: e1d2
  real(dp),    intent(out) :: e1d3
  real(dp),    intent(out) :: e2d(6)
  real(dp),    intent(out) :: e3d(10)
  real(dp),    intent(out) :: ethb
  real(dp),    intent(in)  :: ql1
  real(dp),    intent(in)  :: ql2
  real(dp),    intent(in)  :: ql3
  real(dp),    intent(in)  :: r12
  real(dp),    intent(in)  :: r13
  real(dp),    intent(in)  :: r23
  real(dp),    intent(in)  :: rho1
  real(dp),    intent(in)  :: rho2
  real(dp),    intent(in)  :: rho3
  real(dp),    intent(in)  :: rho4
  real(dp),    intent(in)  :: rho5
  real(dp),    intent(in)  :: rkthb
  real(dp),    intent(in)  :: rkthb3
  real(dp),    intent(in)  :: rkthb4
  real(dp),    intent(in)  :: thet
  real(dp),    intent(in)  :: theta0
  real(dp),    intent(in)  :: thetamax
  real(dp),    intent(in)  :: thetamin
!
!  Local variables
!
  integer(i4)              :: i1
  integer(i4)              :: ii
  integer(i4)              :: k
  integer(i4)              :: m
  integer(i4)              :: mm
  integer(i4)              :: n
  integer(i4)              :: nmax
  integer(i4)              :: pr1
  integer(i4)              :: pr2
  integer(i4)              :: pt
  logical                  :: l180
  real(dp)                 :: a1d1
  real(dp)                 :: a1d2
  real(dp)                 :: a1d3
  real(dp)                 :: a2d(6)
  real(dp)                 :: a3d(10)
  real(dp)                 :: atrm
  real(dp)                 :: b12
  real(dp)                 :: b13
  real(dp)                 :: btrm
  real(dp)                 :: c0
  real(dp)                 :: c1
  real(dp)                 :: c2
  real(dp)                 :: c3
  real(dp)                 :: c4
  real(dp)                 :: c5
  real(dp)                 :: c6
  real(dp)                 :: c7
  real(dp)                 :: c8
  real(dp)                 :: c9
  real(dp)                 :: c10
  real(dp)                 :: c2d(6)
  real(dp)                 :: cos2d(6)
  real(dp)                 :: cos3d(10)
  real(dp)                 :: cos1d1
  real(dp)                 :: cos1d2
  real(dp)                 :: cos1d3
  real(dp)                 :: cosd1
  real(dp)                 :: cosd2
  real(dp)                 :: cosd3
  real(dp)                 :: cosnthetam
  real(dp)                 :: cosntheta
  real(dp)                 :: costh
  real(dp)                 :: cq1
  real(dp)                 :: cq2
  real(dp)                 :: cq3
  real(dp)                 :: d1d1
  real(dp)                 :: d1d2
  real(dp)                 :: d1d3
  real(dp)                 :: d2d(6)
  real(dp)                 :: d3d(10)
  real(dp)                 :: delth
  real(dp)                 :: delth2
  real(dp)                 :: delthm1
  real(dp)                 :: delthm2
  real(dp)                 :: delthm3
  real(dp)                 :: dsinratio
  real(dp)                 :: dsinratio2
  real(dp)                 :: dc2
  real(dp)                 :: dc3
  real(dp)                 :: dc4
  real(dp)                 :: dot2
  real(dp)                 :: dotk
  real(dp)                 :: dotkm1
  real(dp)                 :: dotkm2
  real(dp)                 :: dotkm3
  real(dp)                 :: dotmax
  real(dp)                 :: dotmax2
  real(dp)                 :: dotmin
  real(dp)                 :: dotmin2
  real(dp)                 :: e0
  real(dp)                 :: e1
  real(dp)                 :: e2
  real(dp)                 :: e3
  real(dp)                 :: e1c
  real(dp)                 :: e3c2
  real(dp)                 :: e3c3
  real(dp)                 :: e1dt1
  real(dp)                 :: e1dt2
  real(dp)                 :: e1dt3
  real(dp)                 :: ec2
  real(dp)                 :: et1
  real(dp)                 :: g12
  real(dp)                 :: g13
  real(dp)                 :: p
  real(dp)                 :: p1
  real(dp)                 :: p2
  real(dp)                 :: p3
  real(dp)                 :: p1d1
  real(dp)                 :: p1d2
  real(dp)                 :: p1d3
  real(dp)                 :: p1dq1
  real(dp)                 :: p1dq2
  real(dp)                 :: p1dq3
  real(dp)                 :: p2d(6)
  real(dp)                 :: p2dq(6)
  real(dp)                 :: p3d(10)
  real(dp)                 :: p3dq(10)
  real(dp)                 :: prd1
  real(dp)                 :: prd1m1
  real(dp)                 :: prd1m2
  real(dp)                 :: prd2
  real(dp)                 :: prd2m1
  real(dp)                 :: prd2m2
  real(dp)                 :: ptdelth
  real(dp)                 :: q12d(3)
  real(dp)                 :: q13d(3)
  real(dp)                 :: q22d(3)
  real(dp)                 :: q23d(3)
  real(dp)                 :: q32d(3)
  real(dp)                 :: q33d(3)
  real(dp)                 :: q1
  real(dp)                 :: q2
  real(dp)                 :: q3
  real(dp)                 :: q1d1
  real(dp)                 :: q1d2
  real(dp)                 :: q1d3
  real(dp)                 :: q2d1
  real(dp)                 :: q2d2
  real(dp)                 :: q2d3
  real(dp)                 :: q3d1
  real(dp)                 :: q3d2
  real(dp)                 :: q3d3
  real(dp)                 :: r1d(3)
  real(dp)                 :: r2d(6)
  real(dp)                 :: r3d(10)
  real(dp)                 :: r1
  real(dp)                 :: r2
  real(dp)                 :: r3
  real(dp)                 :: r120
  real(dp)                 :: r130
  real(dp)                 :: r122
  real(dp)                 :: r132
  real(dp)                 :: r232
  real(dp)                 :: rd1
  real(dp)                 :: rd2
  real(dp)                 :: rd3
  real(dp)                 :: rh1
  real(dp)                 :: rh2
  real(dp)                 :: rk1
  real(dp)                 :: rk2
  real(dp)                 :: rk8
  real(dp)                 :: rktrm
  real(dp)                 :: rn
  real(dp)                 :: ro1
  real(dp)                 :: ro2
  real(dp)                 :: ro3
  real(dp)                 :: rqtrm12
  real(dp)                 :: rqtrm122
  real(dp)                 :: rqtrm13
  real(dp)                 :: rqtrm132
  real(dp)                 :: rr12
  real(dp)                 :: rr122
  real(dp)                 :: rr124
  real(dp)                 :: rr13
  real(dp)                 :: rr132
  real(dp)                 :: rr134
  real(dp)                 :: rr23
  real(dp)                 :: rr232
  real(dp)                 :: rr234
  real(dp)                 :: rrho1
  real(dp)                 :: rrho2
  real(dp)                 :: rrho3
  real(dp)                 :: rsinth
  real(dp)                 :: rsinth2
  real(dp)                 :: rtrm
  real(dp)                 :: rtrm0
  real(dp)                 :: rtrm1
  real(dp)                 :: rtrm2
  real(dp)                 :: sinratio
  real(dp)                 :: sinth2
  real(dp)                 :: t12
  real(dp)                 :: t13
  real(dp)                 :: t23
  real(dp)                 :: t2d(6)
  real(dp)                 :: t3d(10)
  real(dp)                 :: t1d1
  real(dp)                 :: t1d2
  real(dp)                 :: t1d3
  real(dp)                 :: the1d1
  real(dp)                 :: the1d2
  real(dp)                 :: the1d3
  real(dp)                 :: the2d(6)
  real(dp)                 :: the3d(10)
  real(dp)                 :: thetrm
  real(dp)                 :: trm0
  real(dp)                 :: trm1
  real(dp)                 :: trm2
  real(dp)                 :: trm3
  real(dp)                 :: ttrm1
  real(dp)                 :: ttrm2
  real(dp)                 :: ttrm3
  real(dp)                 :: ttap
  real(dp)                 :: dttapdr
  real(dp)                 :: d2ttapdr2
  real(dp)                 :: d3ttapdr3
!
!  Zero terms
!
  ethb = 0.0_dp
  if (lgrad1) then
    d0q1 = 0.0_dp
    d0q2 = 0.0_dp
    d0q3 = 0.0_dp
    if (lgrad2) then
      d1q(1:3,1:3) = 0.0_dp
      d2q(1:6) = 0.0_dp
    endif
  endif
!
!  Set up local constants
!
  r122 = r12*r12
  r132 = r13*r13
  r232 = r23*r23
  rr12 = 1.0_dp/r12
  rr13 = 1.0_dp/r13
  rr23 = 1.0_dp/r23
  rr122 = rr12*rr12
  rr132 = rr13*rr13
  rr232 = rr23*rr23
  rr124 = rr122*rr122
  rr134 = rr132*rr132
  rr234 = rr232*rr232
!
!  If theta= 0 or 180 then skip derivatives if there is no
!  distance dependence
!
  l180 = (abs(dot).gt.0.999999)
  costh = dot
!
!  For theta = 180 case ensure derivatives are zeroed
!  ready for return
!
  if (l180) then
    if (lgrad1) then
      e1d1 = 0.0_dp
      e1d2 = 0.0_dp
      e1d3 = 0.0_dp
      if (lgrad2) then
        e2d(1:6) = 0.0_dp
        if (lgrad3) then
          e3d(1:10) = 0.0_dp
        endif
      endif
    endif
  endif
!***************************************
!  Set up potential independent terms  *
!***************************************
  if (.not.l180) then
    if (n3ty.ne.3.and.n3ty.ne.4.and.n3ty.ne.6.and.n3ty.ne.7.and.n3ty.ne.10.and.n3ty.ne.12.and.n3ty.ne.13 &
        .and.n3ty.ne.19.and.n3ty.ne.20.and.n3ty.ne.21) then
!
!  Calculate inverse sin(theta) as this forms part of d(theta)/dr
!
      sinth2 = 1.0_dp - costh**2
      rsinth2 = 1.0_dp/sinth2
      rsinth = sqrt(rsinth2)
    endif
  endif
  if (n3ty.eq.1.or.n3ty.eq.2.or.n3ty.eq.3.or.n3ty.eq.5.or.n3ty.eq.8.or.n3ty.eq.9.or.n3ty.eq.11.or. &
      n3ty.eq.12.or.n3ty.eq.13.or.n3ty.eq.14.or.n3ty.eq.15.or.n3ty.eq.16.or.n3ty.eq.17.or.n3ty.eq.18.or. &
      n3ty.eq.22) then
!****************************************
!  Calculate derivatives of cos(theta)  *
!****************************************
    if (lgrad1) then
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r23
!
      if (ipivot.eq.1) then
        cos1d1 = rr12*rr13 - costh*rr122
        cos1d2 = rr12*rr13 - costh*rr132
        cos1d3 = - rr12*rr13
      elseif (ipivot.eq.2) then
        cos1d1 = rr12*rr23 - costh*rr122
        cos1d2 = - rr12*rr23
        cos1d3 = rr12*rr23 - costh*rr232
      elseif (ipivot.eq.3) then
        cos1d1 = - rr13*rr23
        cos1d2 = rr13*rr23 - costh*rr132
        cos1d3 = rr13*rr23 - costh*rr232
      endif
      if (lgrad2) then
!
!  Second
!
!  1 = 11
!  2 = 21
!  3 = 31
!  4 = 22
!  5 = 32
!  6 = 33
!
        if (ipivot.eq.1) then
          cos2d(1) = - 2.0_dp*rr122*rr12*rr13 + 3.0_dp*costh*rr124
          cos2d(2) = costh*rr122*rr132 - rr12*rr13*(rr122+rr132)
          cos2d(3) = rr122*rr12*rr13
          cos2d(4) = - 2.0_dp*rr132*rr13*rr12 + 3.0_dp*costh*rr134
          cos2d(5) = rr132*rr12*rr13
          cos2d(6) = 0.0_dp
        elseif (ipivot.eq.2) then
          cos2d(1) = - 2.0_dp*rr122*rr12*rr23 + 3.0_dp*costh*rr124
          cos2d(2) = rr122*rr12*rr23
          cos2d(3) = costh*rr122*rr232 - rr12*rr23*(rr122+rr232)
          cos2d(4) = 0.0_dp
          cos2d(5) = rr232*rr12*rr23
          cos2d(6) = - 2.0_dp*rr232*rr23*rr12 + 3.0_dp*costh*rr234
        elseif (ipivot.eq.3) then
          cos2d(1) = 0.0_dp
          cos2d(2) = rr132*rr13*rr23
          cos2d(3) = rr232*rr23*rr13
          cos2d(4) = - 2.0_dp*rr132*rr13*rr23 + 3.0_dp*costh*rr134
          cos2d(5) = costh*rr132*rr232 - rr13*rr23*(rr132+rr232)
          cos2d(6) = - 2.0_dp*rr232*rr23*rr13 + 3.0_dp*costh*rr234
        endif
        if (lgrad3) then
!
!  Third
!
!  1 = 111
!  2 = 211
!  3 = 311
!  4 = 221
!  5 = 321
!  6 = 331
!  7 = 222
!  8 = 322
!  9 = 332
! 10 = 333
!
          if (ipivot.eq.1) then
            cos3d(1) = rr124*(9.0_dp*rr12*rr13 - 15.0_dp*costh*rr122)
            cos3d(2) = rr122*(2.0_dp*rr12*rr132*rr13 + 3.0_dp*rr122*rr12*rr13 - 3.0_dp*costh*rr122*rr132)
            cos3d(3) = - 3.0_dp*rr124*rr12*rr13
            cos3d(4) = rr132*(2.0_dp*rr13*rr122*rr12 + 3.0_dp*rr132*rr13*rr12 - 3.0_dp*costh*rr132*rr122)
            cos3d(5) = - rr122*rr12*rr132*rr13
            cos3d(6) = 0.0_dp
            cos3d(7) = rr134*(9.0_dp*rr13*rr12 - 15.0_dp*costh*rr132)
            cos3d(8) = - 3.0_dp*rr134*rr13*rr12
            cos3d(9) = 0.0_dp
            cos3d(10) = 0.0_dp
          elseif (ipivot.eq.2) then
            cos3d(1) = rr124*(9.0_dp*rr12*rr23-15.0_dp*costh*rr122)
            cos3d(2) = - 3.0_dp*rr124*rr12*rr23
            cos3d(3) = rr122*(2.0_dp*rr12*rr232*rr23+3.0_dp*rr122*rr12*rr23-3.0_dp*costh*rr122*rr232)
            cos3d(4) = 0.0_dp
            cos3d(5) = - rr122*rr12*rr232*rr23
            cos3d(6) = rr232*(2.0_dp*rr23*rr122*rr12+3.0_dp*rr232*rr23*rr12-3.0_dp*costh*rr232*rr122)
            cos3d(7) = 0.0_dp
            cos3d(8) = 0.0_dp
            cos3d(9) = - 3.0_dp*rr234*rr23*rr12
            cos3d(10) = rr234*(9.0_dp*rr23*rr12-15.0_dp*costh*rr232)
          elseif (ipivot.eq.3) then
            cos3d(1) = 0.0_dp
            cos3d(2) = 0.0_dp
            cos3d(3) = 0.0_dp
            cos3d(4) = - 3.0_dp*rr134*rr13*rr23
            cos3d(5) = - rr132*rr13*rr232*rr23
            cos3d(6) = - 3.0_dp*rr234*rr23*rr13
            cos3d(7) = rr134*(9.0_dp*rr13*rr23-15.0_dp*costh*rr132)
            cos3d(8) = rr132*(2.0_dp*rr13*rr232*rr23+3.0_dp*rr132*rr13*rr23-3.0_dp*costh*rr132*rr232)
            cos3d(9) = rr232*(2.0_dp*rr23*rr132*rr13+3.0_dp*rr232*rr23*rr13-3.0_dp*costh*rr232*rr132)
            cos3d(10) = rr234*(9.0_dp*rr23*rr13-15.0_dp*costh*rr232)
          endif
        endif
      endif
    endif
  endif
  if ((n3ty.eq.1.or.n3ty.eq.2.or.n3ty.eq.8.or.n3ty.eq.11.or.n3ty.eq.22).and..not.l180) then
!***********************************
!  Calculate derivatives of theta  *
!***********************************
    if (lgrad1) then
!
!  First
!
      the1d1 = - cos1d1*rsinth
      the1d2 = - cos1d2*rsinth
      the1d3 = - cos1d3*rsinth
      if (lgrad2) then
!
!  Second
!
        trm1 = costh*rsinth2
        the2d(1) = trm1*cos1d1*the1d1 - rsinth*cos2d(1)
        the2d(2) = trm1*cos1d1*the1d2 - rsinth*cos2d(2)
        the2d(3) = trm1*cos1d1*the1d3 - rsinth*cos2d(3)
        the2d(4) = trm1*cos1d2*the1d2 - rsinth*cos2d(4)
        the2d(5) = trm1*cos1d2*the1d3 - rsinth*cos2d(5)
        the2d(6) = trm1*cos1d3*the1d3 - rsinth*cos2d(6)
        if (lgrad3) then
!
!  Third
!
          the3d(1) =  - rsinth*cos3d(1) + trm1*the1d1*cos2d(1) &
                    + trm1*the1d1*cos2d(1) + trm1*the2d(1)*cos1d1 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d1*the1d1*the1d1 &
                    - rsinth*cos1d1*the1d1*the1d1
          the3d(2) =  - rsinth*cos3d(2) + trm1*the1d2*cos2d(1) &
                    + trm1*the1d1*cos2d(2) + trm1*the2d(2)*cos1d1 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d1*the1d1*the1d2 &
                    - rsinth*cos1d1*the1d1*the1d2
          the3d(3) =  - rsinth*cos3d(3) + trm1*the1d3*cos2d(1) &
                    + trm1*the1d1*cos2d(3) + trm1*the2d(3)*cos1d1 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d1*the1d1*the1d3 &
                    - rsinth*cos1d1*the1d1*the1d3
          the3d(4) =  - rsinth*cos3d(4) + trm1*the1d2*cos2d(2) &
                    + trm1*the1d2*cos2d(2) + trm1*the2d(4)*cos1d1 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d1*the1d2*the1d2 &
                    - rsinth*cos1d1*the1d2*the1d2
          the3d(5) =  - rsinth*cos3d(5) + trm1*the1d3*cos2d(2) &
                    + trm1*the1d2*cos2d(3) + trm1*the2d(5)*cos1d1 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d1*the1d2*the1d3 &
                    - rsinth*cos1d1*the1d2*the1d3
          the3d(6) =  - rsinth*cos3d(6) + trm1*the1d3*cos2d(3) &
                    + trm1*the1d3*cos2d(3) + trm1*the2d(6)*cos1d1 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d1*the1d3*the1d3 &
                    - rsinth*cos1d1*the1d3*the1d3
          the3d(7) =  - rsinth*cos3d(7) + trm1*the1d2*cos2d(4) &
                    + trm1*the1d2*cos2d(4) + trm1*the2d(4)*cos1d2 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d2*the1d2*the1d2 &
                    - rsinth*cos1d2*the1d2*the1d2
          the3d(8) =  - rsinth*cos3d(8) + trm1*the1d3*cos2d(4) &
                    + trm1*the1d2*cos2d(5) + trm1*the2d(5)*cos1d2 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d2*the1d2*the1d3 &
                    - rsinth*cos1d2*the1d2*the1d3
          the3d(9) =  - rsinth*cos3d(9) + trm1*the1d3*cos2d(5) &
                    + trm1*the1d3*cos2d(5) + trm1*the2d(6)*cos1d2 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d2*the1d3*the1d3 &
                    - rsinth*cos1d2*the1d3*the1d3
          the3d(10) =  - rsinth*cos3d(10) + trm1*the1d3*cos2d(6) &
                    + trm1*the1d3*cos2d(6) + trm1*the2d(6)*cos1d3 &
                    - 2.0_dp*costh*rsinth*trm1*cos1d3*the1d3*the1d3 &
                    - rsinth*cos1d3*the1d3*the1d3
        endif
      endif
    endif
  endif
!******************************
!  Potential dependent terms  *
!******************************
  if (n3ty.eq.1) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Harmonic three body term  +  k3 + k4  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    delth = thet - theta0
    delth2 = delth*delth
    ethb = 0.5_dp*rkthb*delth2
    if (rkthb3.ne.0.0_dp) then
      ethb = ethb + rkthb3*delth2*delth
    endif
    if (rkthb4.ne.0.0_dp) then
      ethb = ethb + rkthb4*delth2*delth2
    endif
    if (l180) return
    if (lgrad1) then
      e1 = rkthb*delth
      e2 = rkthb
      e3 = 0.0_dp
      if (rkthb3.ne.0.0_dp) then
        e1 = e1 + 3.0_dp*rkthb3*delth2
        e2 = e2 + 6.0_dp*rkthb3*delth
        e3 = e3 + 6.0_dp*rkthb3
      endif
      if (rkthb4.ne.0.0_dp) then
        e1 = e1 + 4.0_dp*rkthb4*delth2*delth
        e2 = e2 + 12.0_dp*rkthb4*delth2
        e3 = e3 + 24.0_dp*rkthb4*delth
      endif
!
!  First derivatives of energy
!
      e1d1 = e1*the1d1
      e1d2 = e1*the1d2
      e1d3 = e1*the1d3
      if (lgrad2) then
!
!  Second derivatives of energy
!
        e2d(1) = e1*the2d(1) + e2*the1d1*the1d1
        e2d(2) = e1*the2d(2) + e2*the1d1*the1d2
        e2d(3) = e1*the2d(3) + e2*the1d1*the1d3
        e2d(4) = e1*the2d(4) + e2*the1d2*the1d2
        e2d(5) = e1*the2d(5) + e2*the1d2*the1d3
        e2d(6) = e1*the2d(6) + e2*the1d3*the1d3
        if (lgrad3) then
!
!  Third derivatives of energy
!
          e3d(1) = e3*the1d1*the1d1*the1d1 + e1*the3d(1)
          e3d(2) = e3*the1d1*the1d1*the1d2 + e1*the3d(2)
          e3d(3) = e3*the1d1*the1d1*the1d3 + e1*the3d(3)
          e3d(4) = e3*the1d1*the1d2*the1d2 + e1*the3d(4)
          e3d(5) = e3*the1d1*the1d2*the1d3 + e1*the3d(5)
          e3d(6) = e3*the1d1*the1d3*the1d3 + e1*the3d(6)
          e3d(7) = e3*the1d2*the1d2*the1d2 + e1*the3d(7)
          e3d(8) = e3*the1d2*the1d2*the1d3 + e1*the3d(8)
          e3d(9) = e3*the1d2*the1d3*the1d3 + e1*the3d(9)
          e3d(10) = e3*the1d3*the1d3*the1d3 + e1*the3d(10)
!
          e3d(1) = e3d(1) + 3.0_dp*e2*the2d(1)*the1d1
          e3d(2) = e3d(2) + e2*(2.0_dp*the2d(2)*the1d1 + the2d(1)*the1d2)
          e3d(3) = e3d(3) + e2*(2.0_dp*the2d(3)*the1d1 + the2d(1)*the1d3)
          e3d(4) = e3d(4) + e2*(2.0_dp*the2d(2)*the1d2 + the2d(4)*the1d1)
          e3d(5) = e3d(5) + e2*(the2d(2)*the1d3 + the2d(3)*the1d2 + the2d(5)*the1d1)
          e3d(6) = e3d(6) + e2*(2.0_dp*the2d(3)*the1d3 + the2d(6)*the1d1)
          e3d(7) = e3d(7) + 3.0_dp*e2*the2d(4)*the1d2
          e3d(8) = e3d(8) + e2*(2.0_dp*the2d(5)*the1d2 + the2d(4)*the1d3)
          e3d(9) = e3d(9) + e2*(2.0_dp*the2d(5)*the1d3 + the2d(6)*the1d2)
          e3d(10) = e3d(10) + 3.0_dp*e2*the2d(6)*the1d3
        endif
      endif
    endif
  elseif (n3ty.eq.2) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Distance dependent three body term  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Construct angle based terms
!
    delth = thet - theta0
    delth2 = delth*delth
    e0 = 0.5_dp*rkthb*delth2
    if (l180) then
      a1d1 = 0.0_dp
      a1d2 = 0.0_dp
      a1d3 = 0.0_dp
      do mm = 1,6
        a2d(mm) = 0.0_dp
      enddo
      do mm = 1,10
        a3d(mm) = 0.0_dp
      enddo
    else
      if (lgrad1) then
        e1 = rkthb*delth
        e2 = rkthb
!
!  First derivatives of energy w.r.t. to angle
!
        a1d1 = e1*the1d1
        a1d2 = e1*the1d2
        a1d3 = e1*the1d3
        if (lgrad2) then
!
!  Second derivatives of energy w.r.t. to angle
!
          a2d(1) = e1*the2d(1) + e2*the1d1*the1d1
          a2d(2) = e1*the2d(2) + e2*the1d1*the1d2
          a2d(3) = e1*the2d(3) + e2*the1d1*the1d3
          a2d(4) = e1*the2d(4) + e2*the1d2*the1d2
          a2d(5) = e1*the2d(5) + e2*the1d2*the1d3
          a2d(6) = e1*the2d(6) + e2*the1d3*the1d3
          if (lgrad3) then
!
!  Third derivatives of energy w.r.t. to angle
!
            a3d(1) = e1*the3d(1) + 3.0_dp*e2*the2d(1)*the1d1
            a3d(2) = e1*the3d(2) + e2*(2.0_dp*the2d(2)*the1d1+the2d(1)*the1d2)
            a3d(3) = e1*the3d(3) + e2*(2.0_dp*the2d(3)*the1d1+the2d(1)*the1d3)
            a3d(4) = e1*the3d(4) + e2*(2.0_dp*the2d(2)*the1d2+the2d(4)*the1d1)
            a3d(5) = e1*the3d(5) + e2*(the2d(2)*the1d3+the2d(3)*the1d2+the2d(5)*the1d1)
            a3d(6) = e1*the3d(6) + e2*(2.0_dp*the2d(3)*the1d3+the2d(6)*the1d1)
            a3d(7) = e1*the3d(7) + 3.0_dp*e2*the2d(4)*the1d2
            a3d(8) = e1*the3d(8) + e2*(2.0_dp*the2d(5)*the1d2+the2d(4)*the1d3)
            a3d(9) = e1*the3d(9) + e2*(2.0_dp*the2d(5)*the1d3+the2d(6)*the1d2)
            a3d(10) = e1*the3d(10) + 3.0_dp*e2*the2d(6)*the1d3
          endif
        endif
      endif
    endif
!
!  Distance dependent terms
!
    if (ipivot.eq.1) then
      rh1 = rho1
      rh2 = rho2
      et1 = exp( - r12*rh1)*exp( - r13*rh2)
    elseif (ipivot.eq.2) then
      rh1 = rho1
      rh2 = rho3
      et1 = exp( - r12*rh1)*exp( - r23*rh2)
    else
      rh1 = rho2
      rh2 = rho3
      et1 = exp( - r13*rh1)*exp( - r23*rh2)
    endif
    ethb = e0*et1
    if (lgrad1) then
!
!  First derivatives w.r.t. distance
!
      if (ipivot.eq.1) then
        d1d1 =  - rh1*rr12*et1
        d1d2 =  - rh2*rr13*et1
        d1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        d1d1 =  - rh1*rr12*et1
        d1d2 = 0.0_dp
        d1d3 =  - rh2*rr23*et1
      else
        d1d1 = 0.0_dp
        d1d2 =  - rh1*rr13*et1
        d1d3 =  - rh2*rr23*et1
      endif
      if (lgrad2) then
!
!  Second derivatives w.r.t. distance
!
        do mm = 1,6
          d2d(mm) = 0.0_dp
        enddo
        if (ipivot.eq.1) then
          d2d(1) = rh1*rr122*et1*(rr12 + rh1)
          d2d(2) = rh1*rh2*rr12*rr13*et1
          d2d(4) = rh2*rr132*et1*(rr13 + rh2)
        elseif (ipivot.eq.2) then
          d2d(1) = rh1*rr122*et1*(rr12 + rh1)
          d2d(3) = rh1*rh2*rr12*rr23*et1
          d2d(6) = rh2*rr232*et1*(rr23 + rh2)
        else
          d2d(4) = rh1*rr132*et1*(rr13 + rh1)
          d2d(5) = rh1*rh2*rr13*rr23*et1
          d2d(6) = rh2*rr232*et1*(rr23 + rh2)
        endif
        if (lgrad3) then
!
!  Third derivatives w.r.t. distance
!
          do mm = 1,10
            d3d(mm) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            d3d(1)  = - rh1*rr122*rr12*et1*(3.0_dp*(rr122 + rh1*rr12)+rh1*rh1)
            d3d(2)  = - rh1*rh2*rr122*rr13*et1*(rr12 + rh1)
            d3d(4)  = - rh1*rh2*rr132*rr12*et1*(rr13 + rh2)
            d3d(7)  = - rh2*rr132*rr13*et1*(3.0_dp*(rr132 + rh2*rr13)+rh2*rh2)
          elseif (ipivot.eq.2) then
            d3d(1)  = - rh1*rr122*rr12*et1*(3.0_dp*(rr122 + rh1*rr12)+rh1*rh1)
            d3d(3)  = - rh1*rh2*rr122*rr23*et1*(rr12 + rh1)
            d3d(6)  = - rh1*rh2*rr232*rr12*et1*(rr23 + rh2)
            d3d(10) = - rh2*rr232*rr23*et1*(3.0_dp*(rr232 + rh2*rr23)+rh2*rh2)
          else
            d3d(7)  = - rh1*rr132*rr13*et1*(3.0_dp*(rr132 + rh1*rr13)+rh1*rh1)
            d3d(8)  = - rh1*rh2*rr132*rr23*et1*(rr13 + rh1)
            d3d(9)  = - rh1*rh2*rr232*rr13*et1*(rr23 + rh2)
            d3d(10) = - rh2*rr232*rr23*et1*(3.0_dp*(rr232 + rh2*rr23)+rh2*rh2)
          endif
        endif
      endif
!
!  Combine derivatives from angle and distance terms
!
!  First derivatives
!
      e1d1 = a1d1*et1 + e0*d1d1
      e1d2 = a1d2*et1 + e0*d1d2
      e1d3 = a1d3*et1 + e0*d1d3
      if (lgrad2) then
!
!  Second derivatives
!
        e2d(1) = a2d(1)*et1 + 2.0_dp*a1d1*d1d1 + e0*d2d(1)
        e2d(2) = a2d(2)*et1 + a1d1*d1d2 + a1d2*d1d1 + e0*d2d(2)
        e2d(3) = a2d(3)*et1 + a1d1*d1d3 + a1d3*d1d1 + e0*d2d(3)
        e2d(4) = a2d(4)*et1 + 2.0_dp*a1d2*d1d2 + e0*d2d(4)
        e2d(5) = a2d(5)*et1 + a1d2*d1d3 + a1d3*d1d2 + e0*d2d(5)
        e2d(6) = a2d(6)*et1 + 2.0_dp*a1d3*d1d3 + e0*d2d(6)
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) = a3d(1)*et1 + 3.0_dp*a2d(1)*d1d1 + 3.0_dp*a1d1*d2d(1) + e0*d3d(1)
          e3d(2) = a3d(2)*et1 + 2.0_dp*a2d(2)*d1d1 + a2d(1)*d1d2 + &
                 2.0_dp*d2d(2)*a1d1 + d2d(1)*a1d2 + e0*d3d(2)
          e3d(3) = a3d(3)*et1 + 2.0_dp*a2d(3)*d1d1 + a2d(1)*d1d3 + &
                 2.0_dp*d2d(3)*a1d1 + d2d(1)*a1d3 + e0*d3d(3)
          e3d(4) = a3d(4)*et1 + 2.0_dp*a2d(2)*d1d2 + a2d(4)*d1d1 + &
                 2.0_dp*d2d(2)*a1d2 + d2d(4)*a1d1 + e0*d3d(4)
          e3d(5) = a3d(5)*et1 + a2d(2)*d1d3 + a2d(3)*d1d2 + a2d(5)*d1d1 + &
                 a1d1*d2d(5) + a1d2*d2d(3) + a1d3*d2d(2) + e0*d3d(5)
          e3d(6) = a3d(6)*et1 + 2.0_dp*a2d(3)*d1d3+a2d(6)*d1d1 + &
                 2.0_dp*d2d(3)*a1d3 + d2d(6)*a1d1 + e0*d3d(6)
          e3d(7) = a3d(7)*et1 + 3.0_dp*a2d(4)*d1d2 + 3.0_dp*a1d2*d2d(4) + e0*d3d(7)
          e3d(8) = a3d(8)*et1 + 2.0_dp*a2d(5)*d1d2 + a2d(4)*d1d3 + &
                 2.0_dp*d2d(5)*a1d2 + d2d(4)*a1d3 + e0*d3d(8)
          e3d(9) = a3d(9)*et1 + 2.0_dp*a2d(5)*d1d3 + a2d(6)*d1d2 + &
                 2.0_dp*d2d(5)*a1d3 + d2d(6)*a1d2 + e0*d3d(9)
          e3d(10) = a3d(10)*et1 + 3.0_dp*a2d(6)*d1d3 + 3.0_dp*a1d3*d2d(6) + e0*d3d(10)
        endif
      endif
    endif
  elseif (n3ty.eq.3) then
!$$$$$$$$$$$$$$$$$$$
!  Axilrod - Teller  $
!$$$$$$$$$$$$$$$$$$$
    p1 = r132 + r232 - r122
    p2 = r122 + r232 - r132
    p3 = r122 + r132 - r232
    rk8 = 0.125_dp*rkthb*rr124*rr12*rr134*rr13*rr234*rr23
    e0 = rk8*(8.0_dp*r122*r132*r232 + 3.0_dp*p1*p2*p3)
    ethb = e0
    if (lgrad1) then
!
!  First derivatives
!
      e1dt1 = 16.0_dp*r132*r232 + 6.0_dp*(p1*p2+p1*p3 - p2*p3)
      e1dt2 = 16.0_dp*r122*r232 + 6.0_dp*(p1*p2+p2*p3 - p1*p3)
      e1dt3 = 16.0_dp*r122*r132 + 6.0_dp*(p1*p3+p2*p3 - p1*p2)
      e1d1 =  - 5.0_dp*e0*rr122 + rk8*e1dt1
      e1d2 =  - 5.0_dp*e0*rr132 + rk8*e1dt2
      e1d3 =  - 5.0_dp*e0*rr232 + rk8*e1dt3
      if (lgrad2) then
!
!  Second derivatives
!
        e2d(1) = 10.0_dp*rr124*e0 - 5.0_dp*e1d1*rr122 + 24.0_dp* &
               rk8*(p1 - p2 - p3) - 5.0_dp*rr122*rk8*e1dt1
        e2d(2) =  - 5.0_dp*rr122*e1d2 - 5.0_dp*rr132*rk8*e1dt1 +  &
               8.0_dp*rk8*(4.0_dp*r232 + 3.0_dp*p3)
        e2d(3) =  - 5.0_dp*rr122*e1d3 - 5.0_dp*rr232*rk8*e1dt1 +  &
               8.0_dp*rk8*(4.0_dp*r132 + 3.0_dp*p2)
        e2d(4) = 10.0_dp*rr134*e0 - 5.0_dp*e1d2*rr132 + 24.0_dp* &
               rk8*(p2 - p1 - p3) - 5.0_dp*rr132*rk8*e1dt2
        e2d(5) =  - 5.0_dp*rr132*e1d3 - 5.0_dp*rr232*rk8*e1dt2 +  &
               8.0_dp*rk8*(4.0_dp*r122 + 3.0_dp*p1)
        e2d(6) = 10.0_dp*rr234*e0 - 5.0_dp*e1d3*rr232 + 24.0_dp* &
               rk8*(p3 - p1 - p2) - 5.0_dp*rr232*rk8*e1dt3
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) =  - 40.0_dp*rr124*rr122*e0 + 20.0_dp*rr124*e1d1 &
                 - 5.0_dp*rr122*e2d(1) - 240.0_dp*rr122*rk8*(p1 - p2 - p3) +  &
                 35.0_dp*rr124*rk8*e1dt1 - 144.0_dp*rk8
          e3d(2) = 10.0_dp*e1d2*rr124 - 5.0_dp*rr122*e2d(2) + 25.0_dp* &
                 rr122*rr132*rk8*e1dt1 - 40.0_dp*rr122*rk8*(4.0_dp* &
                 r232 + 3.0_dp*p3) - 120.0_dp*rr132*rk8*(p1 - p2 - p3)+ &
                 48.0_dp*rk8
          e3d(3) = 10.0_dp*e1d3*rr124 - 5.0_dp*rr122*e2d(3) + 25.0_dp* &
                 rr122*rr232*rk8*e1dt1 - 40.0_dp*rr122*rk8*(4.0_dp* &
                 r132 + 3.0_dp*p2) - 120.0_dp*rr232*rk8*(p1 - p2 - p3)+ &
                 48.0_dp*rk8
          e3d(4) = 10.0_dp*e1d1*rr134 - 5.0_dp*rr132*e2d(2) + 25.0_dp* &
                 rr122*rr132*rk8*e1dt2 - 40.0_dp*rr132*rk8*(4.0_dp* &
                 r232 + 3.0_dp*p3) - 120.0_dp*rr122*rk8*(p2 - p1 - p3)+ &
                 48.0_dp*rk8
          e3d(5) =  - 5.0_dp*rr122*e2d(5) + 25.0_dp*rr132*rr232*rk8* &
                 e1dt1 - 40.0_dp*rr132*rk8*(4.0_dp*r132 + 3.0_dp* &
                 p2) - 40.0_dp*rr232*rk8*(4.0_dp*r232 + 3.0_dp*p3) &
                 + 16.0_dp*rk8
          e3d(6) = 10.0_dp*e1d1*rr234 - 5.0_dp*rr232*e2d(3) + 25.0_dp* &
                 rr122*rr232*rk8*e1dt3 - 40.0_dp*rr232*rk8*(4.0_dp* &
                 r132 + 3.0_dp*p2) - 120.0_dp*rr122*rk8*(p3 - p1 - p2)+ &
                 48.0_dp*rk8
          e3d(7) =  - 40.0_dp*rr134*rr132*e0 + 20.0_dp*rr134*e1d2 &
                 - 5.0_dp*rr132*e2d(4) - 240.0_dp*rr132*rk8*(p2 - p1 - p3) +  &
                 35.0_dp*rr134*rk8*e1dt2 - 144.0_dp*rk8
          e3d(8) = 10.0_dp*e1d3*rr134 - 5.0_dp*rr132*e2d(5) + 25.0_dp* &
                 rr132*rr232*rk8*e1dt2 - 40.0_dp*rr132*rk8*(4.0_dp* &
                 r122 + 3.0_dp*p1) - 120.0_dp*rr232*rk8*(p2 - p1 - p3)+ &
                 48.0_dp*rk8
          e3d(9) = 10.0_dp*e1d2*rr234 - 5.0_dp*rr232*e2d(5) + 25.0_dp* &
                 rr132*rr232*rk8*e1dt3 - 40.0_dp*rr232*rk8*(4.0_dp* &
                 r122 + 3.0_dp*p1) - 120.0_dp*rr132*rk8*(p3 - p1 - p2)+ &
                 48.0_dp*rk8
          e3d(10) =  - 40.0_dp*rr234*rr232*e0 + 20.0_dp*rr234*e1d3 &
                  - 5.0_dp*rr232*e2d(6) - 240.0_dp*rr232*rk8*(p3 - p1 - p2) +  &
                 35.0_dp*rr234*rk8*e1dt3 - 144.0_dp*rk8
        endif
      endif
    endif
  elseif (n3ty.eq.4) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Triple exponential 3 - body  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    e0 = rkthb*exp( - r12*rho1)*exp( - r13*rho2)*exp( - r23*rho3)
    ethb = e0
    if (lgrad1) then
!
!  First derivatives
!
      t12 = rho1*rr12
      t13 = rho2*rr13
      t23 = rho3*rr23
      e1d1 =  - t12*e0
      e1d2 =  - t13*e0
      e1d3 =  - t23*e0
      if (lgrad2) then
!
!  Second derivatives
!
        e2d(1) = t12*t12*e0 + t12*rr12*rr12*e0
        e2d(2) = t12*t13*e0
        e2d(3) = t12*t23*e0
        e2d(4) = t13*t13*e0 + t13*rr13*rr13*e0
        e2d(5) = t13*t23*e0
        e2d(6) = t23*t23*e0 + t23*rr23*rr23*e0
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) =  - t12*t12*t12*e0 - 3.0_dp*rr12*rr12*e2d(1)
          e3d(2) =  - t12*t12*t13*e0 - t12*t12*rr12*rr13*e0
          e3d(3) =  - t12*t12*t23*e0 - t12*t12*rr12*rr23*e0
          e3d(4) =  - t12*t13*t13*e0 - t13*t13*rr13*rr12*e0
          e3d(5) =  - t12*t13*t23*e0
          e3d(6) =  - t12*t23*t23*e0 - t23*t23*rr23*rr12*e0
          e3d(7) =  - t13*t13*t13*e0 - 3.0_dp*rr13*rr13*e2d(4)
          e3d(8) =  - t13*t13*t23*e0 - t13*t13*rr13*rr23*e0
          e3d(9) =  - t13*t23*t23*e0 - t23*t23*rr23*rr13*e0
          e3d(10) =  - t23*t23*t23*e0 - 3.0_dp*rr23*rr23*e2d(6)
        endif
      endif
    endif
  elseif (n3ty.eq.5) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Stillinger - Weber 3 - body  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Construct angle based terms
!
    e0 = rkthb*(dot - theta0)*(dot - theta0)
    if (lgrad1) then
      e1 = 2.0_dp*rkthb*(dot - theta0)
      e2 = 2.0_dp*rkthb
!
!  First derivatives of energy w.r.t. to angle
!
      a1d1 = e1*cos1d1
      a1d2 = e1*cos1d2
      a1d3 = e1*cos1d3
      if (lgrad2) then
!
!  Second derivatives of energy w.r.t. to angle
!
        a2d(1) = e1*cos2d(1) + e2*cos1d1*cos1d1
        a2d(2) = e1*cos2d(2) + e2*cos1d1*cos1d2
        a2d(3) = e1*cos2d(3) + e2*cos1d1*cos1d3
        a2d(4) = e1*cos2d(4) + e2*cos1d2*cos1d2
        a2d(5) = e1*cos2d(5) + e2*cos1d2*cos1d3
        a2d(6) = e1*cos2d(6) + e2*cos1d3*cos1d3
        if (lgrad3) then
!
!  Third derivatives of energy w.r.t. to angle
!
          a3d(1) = e1*cos3d(1) + 3.0_dp*e2*cos2d(1)*cos1d1
          a3d(2) = e1*cos3d(2) + e2*(2.0_dp*cos2d(2)*cos1d1+cos2d(1)*cos1d2)
          a3d(3) = e1*cos3d(3) + e2*(2.0_dp*cos2d(3)*cos1d1+cos2d(1)*cos1d3)
          a3d(4) = e1*cos3d(4) + e2*(2.0_dp*cos2d(2)*cos1d2+cos2d(4)*cos1d1)
          a3d(5) = e1*cos3d(5) + e2*(cos2d(2)*cos1d3+cos2d(3)*cos1d2+cos2d(5)*cos1d1)
          a3d(6) = e1*cos3d(6) + e2*(2.0_dp*cos2d(3)*cos1d3+cos2d(6)*cos1d1)
          a3d(7) = e1*cos3d(7) + 3.0_dp*e2*cos2d(4)*cos1d2
          a3d(8) = e1*cos3d(8) + e2*(2.0_dp*cos2d(5)*cos1d2+cos2d(4)*cos1d3)
          a3d(9) = e1*cos3d(9) + e2*(2.0_dp*cos2d(5)*cos1d3+cos2d(6)*cos1d2)
          a3d(10) = e1*cos3d(10) + 3.0_dp*e2*cos2d(6)*cos1d3
        endif
      endif
    endif
!
!  Distance dependent terms
!
    if (ipivot.eq.1) then
      ro1  =  rho1
      ro2  =  rho2
      if (r12.ge.cut12.or.r13.ge.cut13) then
        rtrm1 = 0.0_dp
        rtrm2 = 0.0_dp
        et1 = 0.0_dp
      else
        rtrm1 = 1.0_dp/(r12 - cut12 + 1.0d-28)
        rtrm2 = 1.0_dp/(r13 - cut13 + 1.0d-28)
        et1 = exp(ro1*rtrm1 + ro2*rtrm2)
      endif
    elseif (ipivot.eq.2) then
      ro1  =  rho1
      ro2  =  rho3
      if (r12.ge.cut12.or.r23.ge.cut23) then
        rtrm1 = 0.0_dp
        rtrm2 = 0.0_dp
        et1 = 0.0_dp
      else
        rtrm1 = 1.0_dp/(r12 - cut12 + 1.0d-28)
        rtrm2 = 1.0_dp/(r23 - cut23 + 1.0d-28)
        et1 = exp(ro1*rtrm1 + ro2*rtrm2)
      endif
    else
      ro1  =  rho2
      ro2  =  rho3
      if (r13.ge.cut13.or.r23.ge.cut23) then
        rtrm1 = 0.0_dp
        rtrm2 = 0.0_dp
        et1 = 0.0_dp
      else
        rtrm1 = 1.0_dp/(r13 - cut13 + 1.0d-28)
        rtrm2 = 1.0_dp/(r23 - cut23 + 1.0d-28)
        et1 = exp(ro1*rtrm1 + ro2*rtrm2)
      endif
    endif
    ethb = e0*et1
    if (lgrad1) then
!
!  First derivatives w.r.t. distance
!
      if (ipivot.eq.1) then
        d1d1 =  - ro1*rtrm1*rtrm1*rr12*et1
        d1d2 =  - ro2*rtrm2*rtrm2*rr13*et1
        d1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        d1d1 =  - ro1*rtrm1*rtrm1*rr12*et1
        d1d2 = 0.0_dp
        d1d3 =  - ro2*rtrm2*rtrm2*rr23*et1
      else
        d1d1 = 0.0_dp
        d1d2 =  - ro1*rtrm1*rtrm1*rr13*et1
        d1d3 =  - ro2*rtrm2*rtrm2*rr23*et1
      endif
      if (lgrad2) then
!
!  Second derivatives w.r.t. distance
!
        do mm = 1,6
          d2d(mm) = 0.0_dp
        enddo
        if (ipivot.eq.1) then
          d2d(1) = ro1*rtrm1*rtrm1*rr122*et1*(ro1*rtrm1*rtrm1 + rr12+2.0_dp*rtrm1)
          d2d(2) = ro1*ro2*rtrm1*rtrm1*rtrm2*rtrm2*rr12*rr13*et1
          d2d(4) = ro2*rtrm2*rtrm2*rr132*et1*(ro2*rtrm2*rtrm2 + rr13+2.0_dp*rtrm2)
        elseif (ipivot.eq.2) then
          d2d(1) = ro1*rtrm1*rtrm1*rr122*et1*(ro1*rtrm1*rtrm1 + rr12+2.0_dp*rtrm1)
          d2d(3) = ro1*ro2*rtrm1*rtrm1*rtrm2*rtrm2*rr12*rr23*et1
          d2d(6) = ro2*rtrm2*rtrm2*rr232*et1*(ro2*rtrm2*rtrm2 + rr23+2.0_dp*rtrm2)
        else
          d2d(4) = ro1*rtrm1*rtrm1*rr132*et1*(ro1*rtrm1*rtrm1 + rr13+2.0_dp*rtrm1)
          d2d(5) = ro1*ro2*rtrm1*rtrm1*rtrm2*rtrm2*rr13*rr23*et1
          d2d(6) = ro2*rtrm2*rtrm2*rr232*et1*(ro2*rtrm2*rtrm2 + rr23+2.0_dp*rtrm2)
        endif
        if (lgrad3) then
!
!  Third derivatives w.r.t. distance
!
          do mm = 1,10
            d3d(mm) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            d3d(1) =  - ro1*rr122*rr12*rtrm1*rtrm1*et1*(ro1*ro1*rtrm1* &
                 rtrm1*rtrm1*rtrm1 + 3.0_dp*ro1*rr12*rtrm1*rtrm1+ &
                 6.0_dp*ro1*rtrm1*rtrm1*rtrm1 + 3.0_dp*rr122+6.0_dp* &
                 rr12*rtrm1 + 6.0_dp*rtrm1*rtrm1)
            d3d(2) =  - d2d(1)*ro2*rr13*rtrm2*rtrm2
            d3d(4) =  - d2d(4)*ro1*rr12*rtrm1*rtrm1
            d3d(7) =  - ro2*rr132*rr13*rtrm2*rtrm2*et1*(ro2*ro2*rtrm2* &
                 rtrm2*rtrm2*rtrm2 + 3.0_dp*ro1*rr13*rtrm2*rtrm2+ &
                 6.0_dp*ro2*rtrm2*rtrm2*rtrm2 + 3.0_dp*rr132+6.0_dp* &
                 rr13*rtrm2 + 6.0_dp*rtrm2*rtrm2)
          elseif (ipivot.eq.2) then
            d3d(1) =  - ro1*rr122*rr12*rtrm1*rtrm1*et1*(ro1*ro1*rtrm1* &
                 rtrm1*rtrm1*rtrm1 + 3.0_dp*ro1*rr12*rtrm1*rtrm1+ &
                 6.0_dp*ro1*rtrm1*rtrm1*rtrm1 + 3.0_dp*rr122+6.0_dp* &
                 rr12*rtrm1 + 6.0_dp*rtrm1*rtrm1)
            d3d(3) =  - d2d(1)*ro2*rr23*rtrm2*rtrm2
            d3d(6) =  - d2d(6)*ro1*rr12*rtrm1*rtrm1
            d3d(10) =  - ro2*rr232*rr23*rtrm2*rtrm2*et1*(ro2*ro2*rtrm2* &
                 rtrm2*rtrm2*rtrm2 + 3.0_dp*ro2*rr23*rtrm2*rtrm2+ &
                 6.0_dp*ro2*rtrm2*rtrm2*rtrm2 + 3.0_dp*rr232+6.0_dp* &
                 rr23*rtrm2 + 6.0_dp*rtrm2*rtrm2)
          else
            d3d(7) =  - ro1*rr132*rr13*rtrm1*rtrm1*et1*(ro1*ro1*rtrm1* &
                 rtrm1*rtrm1*rtrm1 + 3.0_dp*ro1*rr13*rtrm1*rtrm1+ &
                 6.0_dp*ro1*rtrm1*rtrm1*rtrm1 + 3.0_dp*rr132+6.0_dp* &
                 rr13*rtrm1 + 6.0_dp*rtrm1*rtrm1)
            d3d(8) =  - d2d(4)*ro2*rr23*rtrm2*rtrm2
            d3d(9) =  - d2d(6)*ro1*rr13*rtrm1*rtrm1
            d3d(10) =  - ro2*rr232*rr23*rtrm2*rtrm2*et1*(ro2*ro2*rtrm2* &
                 rtrm2*rtrm2*rtrm2 + 3.0_dp*ro2*rr23*rtrm2*rtrm2+ &
                 6.0_dp*ro2*rtrm2*rtrm2*rtrm2 + 3.0_dp*rr232+6.0_dp* &
                 rr23*rtrm2 + 6.0_dp*rtrm2*rtrm2)
          endif
        endif
      endif
!
!  Combine derivatives from angle and distance terms
!
!  First derivatives
!
      e1d1 = a1d1*et1 + e0*d1d1
      e1d2 = a1d2*et1 + e0*d1d2
      e1d3 = a1d3*et1 + e0*d1d3
      if (lgrad2) then
!
!  Second derivatives
!
        e2d(1) = a2d(1)*et1 + 2.0_dp*a1d1*d1d1 + e0*d2d(1)
        e2d(2) = a2d(2)*et1 + a1d1*d1d2 + a1d2*d1d1 + e0*d2d(2)
        e2d(3) = a2d(3)*et1 + a1d1*d1d3 + a1d3*d1d1 + e0*d2d(3)
        e2d(4) = a2d(4)*et1 + 2.0_dp*a1d2*d1d2 + e0*d2d(4)
        e2d(5) = a2d(5)*et1 + a1d2*d1d3 + a1d3*d1d2 + e0*d2d(5)
        e2d(6) = a2d(6)*et1 + 2.0_dp*a1d3*d1d3 + e0*d2d(6)
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) = a3d(1)*et1 + 3.0_dp*a2d(1)*d1d1 + 3.0_dp*a1d1*d2d(1) + e0*d3d(1)
          e3d(2) = a3d(2)*et1 + 2.0_dp*a2d(2)*d1d1 + a2d(1)*d1d2 + 2.0_dp*d2d(2)*a1d1 + d2d(1)*a1d2 + e0*d3d(2)
          e3d(3) = a3d(3)*et1 + 2.0_dp*a2d(3)*d1d1 + a2d(1)*d1d3 + 2.0_dp*d2d(3)*a1d1 + d2d(1)*a1d3 + e0*d3d(3)
          e3d(4) = a3d(4)*et1 + 2.0_dp*a2d(2)*d1d2 + a2d(4)*d1d1 + 2.0_dp*d2d(2)*a1d2 + d2d(4)*a1d1 + e0*d3d(4)
          e3d(5) = a3d(5)*et1 + a2d(2)*d1d3 + a2d(3)*d1d2 + a2d(5)*d1d1 + a1d1*d2d(5) + a1d2*d2d(3) + a1d3*d2d(2) + e0*d3d(5)
          e3d(6) = a3d(6)*et1 + 2.0_dp*a2d(3)*d1d3 + a2d(6)*d1d1 + 2.0_dp*d2d(3)*a1d3 + d2d(6)*a1d1 + e0*d3d(6)
          e3d(7) = a3d(7)*et1 + 3.0_dp*a2d(4)*d1d2 + 3.0_dp*a1d2*d2d(4) + e0*d3d(7)
          e3d(8) = a3d(8)*et1 + 2.0_dp*a2d(5)*d1d2 + a2d(4)*d1d3 + 2.0_dp*d2d(5)*a1d2 + d2d(4)*a1d3 + e0*d3d(8)
          e3d(9) = a3d(9)*et1 + 2.0_dp*a2d(5)*d1d3 + a2d(6)*d1d2 + 2.0_dp*d2d(5)*a1d3 + d2d(6)*a1d2 + e0*d3d(9)
          e3d(10) = a3d(10)*et1 + 3.0_dp*a2d(6)*d1d3 + 3.0_dp*a1d3*d2d(6) + e0*d3d(10)
        endif
      endif
    endif
  elseif (n3ty.eq.6) then
!$$$$$$$$$$$$$$$$$$$$$$$$$
!  Cross bond potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$
    rd1 = r12 - rho1
    rd2 = r13 - rho2
    rd3 = r23 - rho3
    if (ipivot.eq.1) then
      ethb = rkthb*rd1*rd2
    elseif (ipivot.eq.2) then
      ethb = rkthb*rd1*rd3
    else
      ethb = rkthb*rd2*rd3
    endif
    if (lgrad1) then
!
!  First derivatives
!
      if (ipivot.eq.1) then
        e1d1 = rkthb*rd2*rr12
        e1d2 = rkthb*rd1*rr13
        e1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        e1d1 = rkthb*rd3*rr12
        e1d2 = 0.0_dp
        e1d3 = rkthb*rd1*rr23
      else
        e1d1 = 0.0_dp
        e1d2 = rkthb*rd3*rr13
        e1d3 = rkthb*rd2*rr23
      endif
      if (lgrad2) then
!
!  Second derivatives
!
        if (ipivot.eq.1) then
          e2d(1) =  - e1d1*rr122
          e2d(2) = rkthb*rr12*rr13
          e2d(3) = 0.0_dp
          e2d(4) =  - e1d2*rr132
          e2d(5) = 0.0_dp
          e2d(6) = 0.0_dp
        elseif (ipivot.eq.2) then
          e2d(1) =  - e1d1*rr122
          e2d(2) = 0.0_dp
          e2d(3) = rkthb*rr12*rr23
          e2d(4) = 0.0_dp
          e2d(5) = 0.0_dp
          e2d(6) =  - e1d3*rr232
        else
          e2d(1) = 0.0_dp
          e2d(2) = 0.0_dp
          e2d(3) = 0.0_dp
          e2d(4) =  - e1d2*rr132
          e2d(5) = rkthb*rr13*rr23
          e2d(6) =  - e1d3*rr232
        endif
        if (lgrad3) then
!
!  Third derivatives
!
          do m = 1,9
            e3d(m) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            e3d(1) =  - 3.0_dp*e2d(1)*rr122
            e3d(2) =  - e2d(2)*rr122
            e3d(4) =  - e2d(2)*rr132
            e3d(7) =  - 3.0_dp*e2d(4)*rr132
          elseif (ipivot.eq.2) then
            e3d(1) =  - 3.0_dp*e2d(1)*rr122
            e3d(3) =  - e2d(3)*rr122
            e3d(6) =  - e2d(3)*rr232
            e3d(10) =  - 3.0_dp*e2d(6)*rr232
          else
            e3d(7) =  - 3.0_dp*e2d(4)*rr132
            e3d(8) =  - e2d(5)*rr132
            e3d(9) =  - e2d(5)*rr232
            e3d(10) =  - 3.0_dp*e2d(6)*rr232
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.7) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Urey - Bradley potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    if (ipivot.eq.1) then
      ethb = 0.5_dp*rkthb*(r23 - theta0)*(r23 - theta0)
    elseif (ipivot.eq.2) then
      ethb = 0.5_dp*rkthb*(r13 - theta0)*(r13 - theta0)
    else
      ethb = 0.5_dp*rkthb*(r12 - theta0)*(r12 - theta0)
    endif
    if (lgrad1) then
!
!  First derivatives
!
      e1d1 = 0.0_dp
      e1d2 = 0.0_dp
      e1d3 = 0.0_dp
      if (ipivot.eq.1) then
        e1d3 = rkthb*(r23 - theta0)*rr23
      elseif (ipivot.eq.2) then
        e1d2 = rkthb*(r13 - theta0)*rr13
      else
        e1d1 = rkthb*(r12 - theta0)*rr12
      endif
      if (lgrad2) then
!
!  Second derivatives
!
        do mm = 1,6
          e2d(mm) = 0.0_dp
        enddo
        if (ipivot.eq.1) then
          e2d(6) = rr23*rr23*(rkthb - e1d3)
        elseif (ipivot.eq.2) then
          e2d(4) = rr13*rr13*(rkthb - e1d2)
        else
          e2d(1) = rr12*rr12*(rkthb - e1d1)
        endif
        if (lgrad3) then
!
!  Third derivatives 
!
          do mm = 1,10
            e3d(mm) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            e3d(10) =  - 3.0_dp*rr23*rr23*e2d(6)
          elseif (ipivot.eq.2) then
            e3d(7) =  - 3.0_dp*rr13*rr13*e2d(4)
          else
            e3d(1) =  - 3.0_dp*rr12*rr12*e2d(1)
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.8) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Distance dependent three body term  -  Vessal form  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Construct angle based terms
!
    delth = thet - pi
    delth2 = delth*delth
    thetrm = (theta0 - delth2)
    e0 = 0.5_dp*rkthb*thetrm*thetrm
    if (l180) then
      a1d1 = 0.0_dp
      a1d2 = 0.0_dp
      a1d3 = 0.0_dp
      do mm = 1,6
        a2d(mm) = 0.0_dp
      enddo
      do mm = 1,10
        a3d(mm) = 0.0_dp
      enddo
    else
    if (lgrad1) then
      e1 =  - 2.0_dp*rkthb*thetrm*delth
      e2 =  - 2.0_dp*rkthb*(theta0 - 3.0_dp*delth2)
      e3 = 12.0_dp*rkthb*delth
!
!  First derivatives of energy w.r.t. to angle
!
      a1d1 = e1*the1d1
      a1d2 = e1*the1d2
      a1d3 = e1*the1d3
      if (lgrad2) then
!
!  Second derivatives of energy w.r.t. to angle
!
        a2d(1) = e1*the2d(1) + e2*the1d1*the1d1
        a2d(2) = e1*the2d(2) + e2*the1d1*the1d2
        a2d(3) = e1*the2d(3) + e2*the1d1*the1d3
        a2d(4) = e1*the2d(4) + e2*the1d2*the1d2
        a2d(5) = e1*the2d(5) + e2*the1d2*the1d3
        a2d(6) = e1*the2d(6) + e2*the1d3*the1d3
        if (lgrad3) then
!
!  Third derivatives of energy w.r.t. to angle
!
          a3d(1) = e1*the3d(1) + 3.0_dp*e2*the2d(1)*the1d1+e3*the1d1*the1d1*the1d1
          a3d(2) = e1*the3d(2) + e2*(2.0_dp*the2d(2)*the1d1+the2d(1)*the1d2)+e3*the1d1*the1d1*the1d2
          a3d(3) = e1*the3d(3) + e2*(2.0_dp*the2d(3)*the1d1+the2d(1)*the1d3)+e3*the1d1*the1d1*the1d3
          a3d(4) = e1*the3d(4) + e2*(2.0_dp*the2d(2)*the1d2+the2d(4)*the1d1)+e3*the1d1*the1d2*the1d2
          a3d(5) = e1*the3d(5) + e2*(the2d(2)*the1d3+the2d(3)*the1d2+the2d(5)*the1d1)+e3*the1d1*the1d2*the1d3
          a3d(6) = e1*the3d(6) + e2*(2.0_dp*the2d(3)*the1d3+the2d(6)*the1d1)+e3*the1d1*the1d3*the1d3
          a3d(7) = e1*the3d(7) + 3.0_dp*e2*the2d(4)*the1d2+e3*the1d2*the1d2*the1d2
          a3d(8) = e1*the3d(8) + e2*(2.0_dp*the2d(5)*the1d2+the2d(4)*the1d3)+e3*the1d2*the1d2*the1d3
          a3d(9) = e1*the3d(9) + e2*(2.0_dp*the2d(5)*the1d3+the2d(6)*the1d2)+e3*the1d2*the1d3*the1d3
          a3d(10) = e1*the3d(10) + 3.0_dp*e2*the2d(6)*the1d3+e3*the1d3*the1d3*the1d3
        endif
      endif
    endif
    endif
!
!  Distance dependent terms
!
    if (ipivot.eq.1) then
      rh1 = rho1
      rh2 = rho2
      et1 = exp( - r12*rh1)*exp( - r13*rh2)
    elseif (ipivot.eq.2) then
      rh1 = rho1
      rh2 = rho3
      et1 = exp( - r12*rh1)*exp( - r23*rh2)
    else
      rh1 = rho2
      rh2 = rho3
      et1 = exp( - r13*rh1)*exp( - r23*rh2)
    endif
    ethb = e0*et1
    if (lgrad1) then
!
!  First derivatives w.r.t. distance
!
      if (ipivot.eq.1) then
        d1d1 =  - rh1*rr12*et1
        d1d2 =  - rh2*rr13*et1
        d1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        d1d1 =  - rh1*rr12*et1
        d1d2 = 0.0_dp
        d1d3 =  - rh2*rr23*et1
      else
        d1d1 = 0.0_dp
        d1d2 =  - rh1*rr13*et1
        d1d3 =  - rh2*rr23*et1
      endif
      if (lgrad2) then
!
!  Second derivatives w.r.t. distance
!
        do mm = 1,6
          d2d(mm) = 0.0_dp
        enddo
        if (ipivot.eq.1) then
          d2d(1) = rh1*rr122*et1*(rr12 + rh1)
          d2d(2) = rh1*rh2*rr12*rr13*et1
          d2d(4) = rh2*rr132*et1*(rr13 + rh2)
        elseif (ipivot.eq.2) then
          d2d(1) = rh1*rr122*et1*(rr12 + rh1)
          d2d(3) = rh1*rh2*rr12*rr23*et1
          d2d(6) = rh2*rr232*et1*(rr23 + rh2)
        else
          d2d(4) = rh1*rr132*et1*(rr13 + rh1)
          d2d(5) = rh1*rh2*rr13*rr23*et1
          d2d(6) = rh2*rr232*et1*(rr23 + rh2)
        endif
        if (lgrad3) then
!
!  Third derivatives w.r.t. distance
!
          do mm = 1,10
            d3d(mm) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            d3d(1) =  - rh1*rr122*rr12*et1*(3.0_dp*(rr122 + rh1*rr12)+rh1*rh1)
            d3d(2) =  - rh1*rh2*rr122*rr13*et1*(rr12 + rh1)
            d3d(4) =  - rh1*rh2*rr132*rr12*et1*(rr13 + rh2)
            d3d(7) =  - rh2*rr132*rr13*et1*(3.0_dp*(rr132 + rh2*rr13)+rh2*rh2)
          elseif (ipivot.eq.2) then
            d3d(1) =  - rh1*rr122*rr12*et1*(3.0_dp*(rr122 + rh1*rr12)+rh1*rh1)
            d3d(3) =  - rh1*rh2*rr122*rr23*et1*(rr12 + rh1)
            d3d(6) =  - rh1*rh2*rr232*rr12*et1*(rr23 + rh2)
            d3d(10) =  - rh2*rr232*rr23*et1*(3.0_dp*(rr232 + rh2*rr23)+rh2*rh2)
          else
            d3d(7) =  - rh1*rr132*rr13*et1*(3.0_dp*(rr132 + rh1*rr13)+rh1*rh1)
            d3d(8) =  - rh1*rh2*rr132*rr23*et1*(rr13 + rh1)
            d3d(9) =  - rh1*rh2*rr232*rr13*et1*(rr23 + rh2)
            d3d(10) =  - rh2*rr232*rr23*et1*(3.0_dp*(rr232 + rh2*rr23)+rh2*rh2)
          endif
        endif
      endif
!
!  Combine derivatives from angle and distance terms
!
!  First derivatives
!
      e1d1 = a1d1*et1 + e0*d1d1
      e1d2 = a1d2*et1 + e0*d1d2
      e1d3 = a1d3*et1 + e0*d1d3
      if (lgrad2) then
!
!  Second derivatives
!
        e2d(1) = a2d(1)*et1 + 2.0_dp*a1d1*d1d1+e0*d2d(1)
        e2d(2) = a2d(2)*et1 + a1d1*d1d2+a1d2*d1d1+e0*d2d(2)
        e2d(3) = a2d(3)*et1 + a1d1*d1d3+a1d3*d1d1+e0*d2d(3)
        e2d(4) = a2d(4)*et1 + 2.0_dp*a1d2*d1d2+e0*d2d(4)
        e2d(5) = a2d(5)*et1 + a1d2*d1d3+a1d3*d1d2+e0*d2d(5)
        e2d(6) = a2d(6)*et1 + 2.0_dp*a1d3*d1d3+e0*d2d(6)
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) = a3d(1)*et1 + 3.0_dp*a2d(1)*d1d1+3.0_dp*a1d1*d2d(1)+e0*d3d(1)
          e3d(2) = a3d(2)*et1 + 2.0_dp*a2d(2)*d1d1+a2d(1)*d1d2+2.0_dp*d2d(2)*a1d1+d2d(1)*a1d2+e0*d3d(2)
          e3d(3) = a3d(3)*et1 + 2.0_dp*a2d(3)*d1d1+a2d(1)*d1d3+2.0_dp*d2d(3)*a1d1+d2d(1)*a1d3+e0*d3d(3)
          e3d(4) = a3d(4)*et1 + 2.0_dp*a2d(2)*d1d2+a2d(4)*d1d1+2.0_dp*d2d(2)*a1d2+d2d(4)*a1d1+e0*d3d(4)
          e3d(5) = a3d(5)*et1 + a2d(2)*d1d3+a2d(3)*d1d2+a2d(5)*d1d1+a1d1*d2d(5)+a1d2*d2d(3)+a1d3*d2d(2)+e0*d3d(5)
          e3d(6) = a3d(6)*et1 + 2.0_dp*a2d(3)*d1d3+a2d(6)*d1d1+2.0_dp*d2d(3)*a1d3+d2d(6)*a1d1+e0*d3d(6)
          e3d(7) = a3d(7)*et1 + 3.0_dp*a2d(4)*d1d2+3.0_dp*a1d2*d2d(4)+e0*d3d(7)
          e3d(8) = a3d(8)*et1 + 2.0_dp*a2d(5)*d1d2+a2d(4)*d1d3+2.0_dp*d2d(5)*a1d2+d2d(4)*a1d3+e0*d3d(8)
          e3d(9) = a3d(9)*et1 + 2.0_dp*a2d(5)*d1d3+a2d(6)*d1d2+2.0_dp*d2d(5)*a1d3+d2d(6)*a1d2+e0*d3d(9)
          e3d(10) = a3d(10)*et1 + 3.0_dp*a2d(6)*d1d3+3.0_dp*a1d3*d2d(6)+e0*d3d(10)
        endif
      endif
    endif
  elseif (n3ty.eq.9) then
!$$$$$$$$$$$$$$$$$$$$$$$$
!  Cosine  -  harmonic  $
!$$$$$$$$$$$$$$$$$$$$$$$$
    ethb = 0.5_dp*rkthb*(costh - theta0)*(costh - theta0)
    if (lgrad1) then
!
!  First derivatives
!
      e1 = rkthb*(costh - theta0)
      e1d1 = e1*cos1d1
      e1d2 = e1*cos1d2
      e1d3 = e1*cos1d3
      if (lgrad2) then
!
!  Second derivatives
!
        e2d(1) = rkthb*cos1d1*cos1d1 + e1*cos2d(1)
        e2d(2) = rkthb*cos1d1*cos1d2 + e1*cos2d(2)
        e2d(3) = rkthb*cos1d1*cos1d3 + e1*cos2d(3)
        e2d(4) = rkthb*cos1d2*cos1d2 + e1*cos2d(4)
        e2d(5) = rkthb*cos1d2*cos1d3 + e1*cos2d(5)
        e2d(6) = rkthb*cos1d3*cos1d3 + e1*cos2d(6)
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) = rkthb*(3.0_dp*cos1d1*cos2d(1)) + e1*cos3d(1)
          e3d(2) = rkthb*(cos1d2*cos2d(1) + 2.0_dp*cos1d1*cos2d(2)) + e1*cos3d(2)
          e3d(3) = rkthb*(cos1d3*cos2d(1) + 2.0_dp*cos1d1*cos2d(3)) + e1*cos3d(3)
          e3d(4) = rkthb*(cos1d1*cos2d(4) + 2.0_dp*cos1d2*cos2d(2)) + e1*cos3d(4)
          e3d(5) = rkthb*(cos1d1*cos2d(5) + cos1d2*cos2d(3)+cos1d3*cos2d(2)) + e1*cos3d(5)
          e3d(6) = rkthb*(cos1d1*cos2d(6) + 2.0_dp*cos1d3*cos2d(3)) + e1*cos3d(6)
          e3d(7) = rkthb*(3.0_dp*cos1d2*cos2d(4)) + e1*cos3d(7)
          e3d(8) = rkthb*(cos1d3*cos2d(4) + 2.0_dp*cos1d2*cos2d(5)) + e1*cos3d(8)
          e3d(9) = rkthb*(cos1d2*cos2d(6) + 2.0_dp*cos1d3*cos2d(5)) + e1*cos3d(9)
          e3d(10) = rkthb*(3.0_dp*cos1d3*cos2d(6)) + e1*cos3d(10)
        endif
      endif
    endif
  elseif (n3ty.eq.10) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Murrell - Mottram potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Set local variables
!
    c0 = threepoly(1,nptr3)
    c1 = threepoly(2,nptr3)
    c2 = threepoly(3,nptr3)
    c3 = threepoly(4,nptr3)
    c4 = threepoly(5,nptr3)
    c5 = threepoly(6,nptr3)
    c6 = threepoly(7,nptr3)
    c7 = threepoly(8,nptr3)
    c8 = threepoly(9,nptr3)
    c9 = threepoly(10,nptr3)
    c10 = threepoly(11,nptr3)
    rrho1 = 1.0_dp/rho1
    rrho2 = 1.0_dp/rho2
    rrho3 = 1.0_dp/rho3
!
!  Calculate basic terms
!
    r1 = (r12 - rho1)*rrho1
    r2 = (r13 - rho2)*rrho2
    r3 = (r23 - rho3)*rrho3
    cq1 = 1.0_dp/sqrt(3.0_dp)
    cq2 = 1.0_dp/sqrt(2.0_dp)
    cq3 = cq1*cq2
    q1 = cq1*(r1 + r2+r3)
    q2 = cq2*(r2 - r3)
    q3 = cq3*(2.0_dp*r1 - r2 - r3)
    p = c0 + q1*(c1+q1*(c2+q1*(c4+q1*c7)))+(c3+c5*q1+c8*q1*q1+c9* &
      (q2*q2 + q3*q3))*(q2*q2+q3*q3)+(c6+c10*q1)*q3*(q3*q3 - 3.0_dp*q2*q2)
    rktrm = rkthb*exp( - theta0*q1)
!
!  Energy
!
    ethb = rktrm*p
    if (lgrad1) then
!
!  First derivatives
!
      p1dq1 = c1 + q1*(2.0_dp*c2+q1*(3.0_dp*c4+q1*4.0_dp*c7))+ &
            (c5 + 2.0_dp*c8*q1)*(q2*q2+q3*q3)+c10*(q3*q3*q3 - 3.0_dp*q3*q2*q2)
      p1dq2 = 2.0_dp*q2*(c3 + c5*q1) - 6.0_dp*q3*q2*(c6+c10*q1)+ &
            2.0_dp*q2*(c8*q1*q1 + 2.0_dp*c9*(q2*q2+q3*q3))
      p1dq3 = 2.0_dp*q3*(c3 + c5*q1)+3.0_dp*(q3*q3 - q2*q2)*(c6+c10* &
            q1) + 2.0_dp*q3*(c8*q1*q1+2.0_dp*c9*(q2*q2+q3*q3))
      q1d1 = cq1*rrho1*rr12
      q1d2 = cq1*rrho2*rr13
      q1d3 = cq1*rrho3*rr23
      q2d1 = 0.0_dp
      q2d2 = cq2*rrho2*rr13
      q2d3 =  - cq2*rrho3*rr23
      q3d1 = 2.0_dp*cq3*rrho1*rr12
      q3d2 =  - cq3*rrho2*rr13
      q3d3 =  - cq3*rrho3*rr23
      p1d1 = p1dq1*q1d1 + p1dq2*q2d1+p1dq3*q3d1
      p1d2 = p1dq1*q1d2 + p1dq2*q2d2+p1dq3*q3d2
      p1d3 = p1dq1*q1d3 + p1dq2*q2d3+p1dq3*q3d3
!
!  Combine components to obtain total derivatives
!
      e1d1 = rktrm*(p1d1 - theta0*p*q1d1)
      e1d2 = rktrm*(p1d2 - theta0*p*q1d2)
      e1d3 = rktrm*(p1d3 - theta0*p*q1d3)
      if (lgrad2) then
!
!  Second derivatives  -  only on diagonal elements of d2Q/dr2
!  are non - zero and this assumption is used in working
!
        q12d(1) =  - q1d1*rr122
        q12d(2) =  - q1d2*rr132
        q12d(3) =  - q1d3*rr232
        q22d(1) = 0.0_dp
        q22d(2) =  - q2d2*rr132
        q22d(3) =  - q2d3*rr232
        q32d(1) =  - q3d1*rr122
        q32d(2) =  - q3d2*rr132
        q32d(3) =  - q3d3*rr232
!
!  Second derivatives of P w.r.t. Qs
!
        p2dq(1) = 2.0_dp*(c2 + 3.0_dp*q1*(c4+2.0_dp*c7*q1)+c8*(q2*q2+q3*q3))
        p2dq(2) = 2.0_dp*q2*(c5 + 2.0_dp*c8*q1 - 3.0_dp*c10*q3)
        p2dq(3) = 2.0_dp*q3*(c5 + 2.0_dp*c8*q1)+3.0_dp*c10*(q3*q3 - q2*q2)
        p2dq(4) = 2.0_dp*(c3 + q1*(c5+c8*q1) - 3.0_dp*q3*(c6+c10*q1)+2.0_dp*c9*(3.0_dp*q2*q2+q3*q3))
        p2dq(5) = q2*(8.0_dp*c9*q3 - 6.0_dp*(c6 + c10*q1))
        p2dq(6) = 2.0_dp*(c3 + q1*(c5+c8*q1)+3.0_dp*q3*(c6+c10*q1)+2.0_dp*c9*(q2*q2+3.0_dp*q3*q3))
!
!  Second derivatives of P w.r.t. to distances
!
        p2d(1) = p2dq(1)*q1d1*q1d1 + 2.0_dp*p2dq(2)*q1d1*q2d1+ &
          2.0_dp*p2dq(3)*q1d1*q3d1 + p2dq(4)*q2d1*q2d1+ &
          2.0_dp*p2dq(5)*q2d1*q3d1 + p2dq(6)*q3d1*q3d1+ &
          p1dq1*q12d(1) + p1dq2*q22d(1)+p1dq3*q32d(1)
        p2d(2) = p2dq(1)*q1d1*q1d2 + p2dq(2)*q1d1*q2d2+ &
          p2dq(2)*q2d1*q1d2 + p2dq(3)*q1d1*q3d2+ &
          p2dq(3)*q3d1*q1d2 + p2dq(4)*q2d1*q2d2+ &
          p2dq(5)*q2d1*q3d2 + p2dq(5)*q3d1*q2d2+ &
          p2dq(6)*q3d1*q3d2
        p2d(3) = p2dq(1)*q1d1*q1d3 + p2dq(2)*q1d1*q2d3+ &
          p2dq(2)*q2d1*q1d3 + p2dq(3)*q1d1*q3d3+ &
          p2dq(3)*q3d1*q1d3 + p2dq(4)*q2d1*q2d3+ &
          p2dq(5)*q2d1*q3d3 + p2dq(5)*q3d1*q2d3+ &
          p2dq(6)*q3d1*q3d3
        p2d(4) = p2dq(1)*q1d2*q1d2 + 2.0_dp*p2dq(2)*q1d2*q2d2+ &
          2.0_dp*p2dq(3)*q1d2*q3d2 + p2dq(4)*q2d2*q2d2+ &
          2.0_dp*p2dq(5)*q2d2*q3d2 + p2dq(6)*q3d2*q3d2+ &
          p1dq1*q12d(2) + p1dq2*q22d(2)+p1dq3*q32d(2)
        p2d(5) = p2dq(1)*q1d2*q1d3 + p2dq(2)*q1d2*q2d3+ &
          p2dq(2)*q2d2*q1d3 + p2dq(3)*q1d2*q3d3+ &
          p2dq(3)*q3d2*q1d3 + p2dq(4)*q2d2*q2d3+ &
          p2dq(5)*q2d2*q3d3 + p2dq(5)*q3d2*q2d3+ &
          p2dq(6)*q3d2*q3d3
        p2d(6) = p2dq(1)*q1d3*q1d3 + 2.0_dp*p2dq(2)*q1d3*q2d3+ &
          2.0_dp*p2dq(3)*q1d3*q3d3 + p2dq(4)*q2d3*q2d3+ &
          2.0_dp*p2dq(5)*q2d3*q3d3 + p2dq(6)*q3d3*q3d3+ &
          p1dq1*q12d(3) + p1dq2*q22d(3)+p1dq3*q32d(3)
!
!  Combine components to obtain total derivatives
!
        e2d(1) = rktrm*(p2d(1) - theta0*(2.0_dp*q1d1*p1d1 + p*q12d(1))+theta0*theta0*p*q1d1*q1d1)
        e2d(2) = rktrm*(p2d(2) - theta0*(q1d1*p1d2 + q1d2*p1d1)+theta0*theta0*p*q1d1*q1d2)
        e2d(3) = rktrm*(p2d(3) - theta0*(q1d1*p1d3 + q1d3*p1d1)+theta0*theta0*p*q1d1*q1d3)
        e2d(4) = rktrm*(p2d(4) - theta0*(2.0_dp*q1d2*p1d2 + p*q12d(2))+theta0*theta0*p*q1d2*q1d2)
        e2d(5) = rktrm*(p2d(5) - theta0*(q1d2*p1d3 + q1d3*p1d2)+theta0*theta0*p*q1d2*q1d3)
        e2d(6) = rktrm*(p2d(6) - theta0*(2.0_dp*q1d3*p1d3 + p*q12d(3))+theta0*theta0*p*q1d3*q1d3)
        if (lgrad3) then
!
!  Third derivatives  -  only on diagonal elements of d2Q/dr2
!  are non - zero and this assumption is used in working
!
          q13d(1) =  - 3.0_dp*q12d(1)*rr122
          q13d(2) =  - 3.0_dp*q12d(2)*rr132
          q13d(3) =  - 3.0_dp*q12d(3)*rr232
          q23d(1) = 0.0_dp
          q23d(2) =  - 3.0_dp*q22d(2)*rr132
          q23d(3) =  - 3.0_dp*q22d(3)*rr232
          q33d(1) =  - 3.0_dp*q32d(1)*rr122
          q33d(2) =  - 3.0_dp*q32d(2)*rr132
          q33d(3) =  - 3.0_dp*q32d(3)*rr232
!
!  Third derivatives of P w.r.t. Qs
!
          p3dq(1) = 6.0_dp*(c4 + 4.0_dp*c7*q1)
          p3dq(2) = 4.0_dp*c8*q2
          p3dq(3) = 4.0_dp*c8*q3
          p3dq(4) = 2.0_dp*(c5 + 2.0_dp*c8*q1 - 3.0_dp*c10*q3)
          p3dq(5) =  - 6.0_dp*c10*q2
          p3dq(6) = 2.0_dp*(c5 + 2.0_dp*c8*q1+3.0_dp*c10*q3)
          p3dq(7) = 24.0_dp*c9*q2
          p3dq(8) =  - 6.0_dp*c6 + 8.0_dp*c9*q3 - 6.0_dp*c10*q1
          p3dq(9) = 8.0_dp*c9*q2
          p3dq(10) = 6.0_dp*c6 + 24.0_dp*c9*q3+6.0_dp*c10*q1
!
!  Third derivatives of P w.r.t. to distances
!
!  First set of terms
!
          p3d(1) = p3dq(1)*q1d1*q1d1*q1d1 + 3.0*p3dq(2)*q1d1*q1d1*q2d1+ &
             3.0*p3dq(3)*q1d1*q1d1*q3d1 + 3.0*p3dq(4)*q1d1*q2d1*q2d1+ &
             6.0*p3dq(5)*q1d1*q2d1*q3d1 + 3.0*p3dq(6)*q1d1*q3d1*q3d1+ &
             p3dq(7)*q2d1*q2d1*q2d1 + 3.0*p3dq(8)*q2d1*q2d1*q3d1+ &
             3.0*p3dq(9)*q2d1*q3d1*q3d1 + p3dq(10)*q3d1*q3d1*q3d1
          p3d(2) = p3dq(1)*q1d1*q1d1*q1d2 + p3dq(2)*q1d1*q1d1*q2d2+ &
             2.0*p3dq(2)*q1d1*q1d2*q2d1 + p3dq(3)*q1d1*q1d1*q3d2+ &
             2.0*p3dq(3)*q1d1*q1d2*q3d1 + p3dq(4)*q1d2*q2d1*q2d1+ &
             2.0*p3dq(4)*q1d1*q2d1*q2d2 + 2.0*p3dq(5)*q1d1*q2d1*q3d2+ &
             2.0*p3dq(5)*q1d1*q2d2*q3d1 + 2.0*p3dq(5)*q1d2*q2d1*q3d1+ &
             2.0*p3dq(6)*q1d1*q3d1*q3d2 + p3dq(6)*q1d2*q3d1*q3d1+ &
             p3dq(7)*q2d1*q2d1*q2d2 + 2.0*p3dq(8)*q2d2*q2d1*q3d1+ &
             p3dq(8)*q2d1*q2d1*q3d2 + 2.0*p3dq(9)*q2d1*q3d1*q3d2+ &
             p3dq(9)*q2d2*q3d1*q3d1 + p3dq(10)*q3d1*q3d1*q3d2
          p3d(3) = p3dq(1)*q1d1*q1d1*q1d3 + p3dq(2)*q1d1*q1d1*q2d3+ &
             2.0*p3dq(2)*q1d1*q1d3*q2d1 + p3dq(3)*q1d1*q1d1*q3d3+ &
             2.0*p3dq(3)*q1d1*q1d3*q3d1 + p3dq(4)*q1d3*q2d1*q2d1+ &
             2.0*p3dq(4)*q1d1*q2d1*q2d3 + 2.0*p3dq(5)*q1d1*q2d1*q3d3+ &
             2.0*p3dq(5)*q1d1*q2d3*q3d1 + 2.0*p3dq(5)*q1d3*q2d1*q3d1+ &
             2.0*p3dq(6)*q1d1*q3d1*q3d3 + p3dq(6)*q1d3*q3d1*q3d1+ &
             p3dq(7)*q2d1*q2d1*q2d3 + 2.0*p3dq(8)*q2d3*q2d1*q3d1+ &
             p3dq(8)*q2d1*q2d1*q3d3 + 2.0*p3dq(9)*q2d1*q3d1*q3d3+ &
             p3dq(9)*q2d3*q3d1*q3d1 + p3dq(10)*q3d1*q3d1*q3d3
          p3d(4) = p3dq(1)*q1d2*q1d2*q1d1 + p3dq(2)*q1d2*q1d2*q2d1+ &
             2.0*p3dq(2)*q1d2*q1d1*q2d2 + p3dq(3)*q1d2*q1d2*q3d1+ &
             2.0*p3dq(3)*q1d2*q1d1*q3d2 + p3dq(4)*q1d1*q2d2*q2d2+ &
             2.0*p3dq(4)*q1d2*q2d1*q2d2 + 2.0*p3dq(5)*q1d2*q2d2*q3d1+ &
             2.0*p3dq(5)*q1d2*q2d1*q3d2 + 2.0*p3dq(5)*q1d1*q2d2*q3d2+ &
             2.0*p3dq(6)*q1d2*q3d2*q3d1 + p3dq(6)*q1d1*q3d2*q3d2+ &
             p3dq(7)*q2d2*q2d2*q2d1 + 2.0*p3dq(8)*q2d1*q2d2*q3d2+ &
             p3dq(8)*q2d2*q2d2*q3d1 + 2.0*p3dq(9)*q2d2*q3d2*q3d1+ &
             p3dq(9)*q2d1*q3d2*q3d2 + p3dq(10)*q3d2*q3d2*q3d1
          p3d(5) = p3dq(1)*q1d1*q1d2*q1d3 + p3dq(2)*q1d1*q1d2*q2d3+ &
             p3dq(2)*q1d1*q1d3*q2d2 + p3dq(2)*q1d2*q1d3*q2d1+ &
             p3dq(3)*q1d1*q1d2*q3d3 + p3dq(3)*q1d1*q1d3*q3d2+ &
             p3dq(3)*q1d2*q1d3*q3d1 + p3dq(4)*q1d1*q2d2*q2d3+ &
             p3dq(4)*q1d2*q2d1*q2d3 + p3dq(4)*q1d3*q2d1*q2d2+ &
             p3dq(5)*q1d1*q2d2*q3d3 + p3dq(5)*q1d1*q2d3*q3d2+ &
             p3dq(5)*q1d2*q2d1*q3d3 + p3dq(5)*q1d2*q2d3*q3d1+ &
             p3dq(5)*q1d3*q2d1*q3d2 + p3dq(5)*q1d3*q2d2*q3d1+ &
             p3dq(6)*q1d1*q3d2*q3d3 + p3dq(6)*q1d2*q3d1*q3d3+ &
             p3dq(6)*q1d3*q3d1*q3d2 + p3dq(7)*q2d1*q2d2*q2d3+ &
             p3dq(8)*q2d1*q2d2*q3d3 + p3dq(8)*q2d1*q2d3*q3d2+ &
             p3dq(8)*q2d2*q2d3*q3d1 + p3dq(9)*q2d1*q3d2*q3d3+ &
             p3dq(9)*q2d2*q3d1*q3d3 + p3dq(9)*q2d3*q3d1*q3d2+ &
             p3dq(10)*q3d1*q3d2*q3d3
          p3d(6) = p3dq(1)*q1d3*q1d3*q1d1 + p3dq(2)*q1d3*q1d3*q2d1+ &
             2.0*p3dq(2)*q1d3*q1d1*q2d3 + p3dq(3)*q1d3*q1d3*q3d1+ &
             2.0*p3dq(3)*q1d3*q1d1*q3d3 + p3dq(4)*q1d1*q2d3*q2d3+ &
             2.0*p3dq(4)*q1d3*q2d1*q2d3 + 2.0*p3dq(5)*q1d3*q2d3*q3d1+ &
             2.0*p3dq(5)*q1d3*q2d1*q3d3 + 2.0*p3dq(5)*q1d1*q2d3*q3d3+ &
             2.0*p3dq(6)*q1d3*q3d3*q3d1 + p3dq(6)*q1d1*q3d3*q3d3+ &
             p3dq(7)*q2d3*q2d3*q2d1 + 2.0*p3dq(8)*q2d1*q2d3*q3d3+ &
             p3dq(8)*q2d3*q2d3*q3d1 + 2.0*p3dq(9)*q2d3*q3d3*q3d1+ &
             p3dq(9)*q2d1*q3d3*q3d3 + p3dq(10)*q3d3*q3d3*q3d1
          p3d(7) = p3dq(1)*q1d2*q1d2*q1d2 + 3.0*p3dq(2)*q1d2*q1d2*q2d2+ &
             3.0*p3dq(3)*q1d2*q1d2*q3d2 + 3.0*p3dq(4)*q1d2*q2d2*q2d2+ &
             6.0*p3dq(5)*q1d2*q2d2*q3d2 + 3.0*p3dq(6)*q1d2*q3d2*q3d2+ &
             p3dq(7)*q2d2*q2d2*q2d2 + 3.0*p3dq(8)*q2d2*q2d2*q3d2+ &
             3.0*p3dq(9)*q2d2*q3d2*q3d2 + p3dq(10)*q3d2*q3d2*q3d2
          p3d(8) = p3dq(1)*q1d2*q1d2*q1d3 + p3dq(2)*q1d2*q1d2*q2d3+ &
             2.0*p3dq(2)*q1d2*q1d3*q2d2 + p3dq(3)*q1d2*q1d2*q3d3+ &
             2.0*p3dq(3)*q1d2*q1d3*q3d2 + p3dq(4)*q1d3*q2d2*q2d2+ &
             2.0*p3dq(4)*q1d2*q2d3*q2d2 + 2.0*p3dq(5)*q1d2*q2d2*q3d3+ &
             2.0*p3dq(5)*q1d2*q2d3*q3d2 + 2.0*p3dq(5)*q1d3*q2d2*q3d2+ &
             2.0*p3dq(6)*q1d2*q3d2*q3d3 + p3dq(6)*q1d3*q3d2*q3d2+ &
             p3dq(7)*q2d2*q2d2*q2d3 + 2.0*p3dq(8)*q2d3*q2d2*q3d2+ &
             p3dq(8)*q2d2*q2d2*q3d3 + 2.0*p3dq(9)*q2d2*q3d2*q3d3+ &
             p3dq(9)*q2d3*q3d2*q3d2 + p3dq(10)*q3d2*q3d2*q3d3
          p3d(9) = p3dq(1)*q1d3*q1d3*q1d2 + p3dq(2)*q1d3*q1d3*q2d2+ &
             2.0*p3dq(2)*q1d3*q1d2*q2d3 + p3dq(3)*q1d3*q1d3*q3d2+ &
             2.0*p3dq(3)*q1d3*q1d2*q3d3 + p3dq(4)*q1d2*q2d3*q2d3+ &
             2.0*p3dq(4)*q1d3*q2d2*q2d3 + 2.0*p3dq(5)*q1d3*q2d3*q3d2+ &
             2.0*p3dq(5)*q1d3*q2d2*q3d3 + 2.0*p3dq(5)*q1d2*q2d3*q3d3+ &
             2.0*p3dq(6)*q1d3*q3d3*q3d2 + p3dq(6)*q1d2*q3d3*q3d3+ &
             p3dq(7)*q2d3*q2d3*q2d2 + 2.0*p3dq(8)*q2d2*q2d3*q3d3+ &
             p3dq(8)*q2d3*q2d3*q3d2 + 2.0*p3dq(9)*q2d3*q3d3*q3d2+ &
             p3dq(9)*q2d2*q3d3*q3d3 + p3dq(10)*q3d3*q3d3*q3d2
          p3d(10) = p3dq(1)*q1d3*q1d3*q1d3 + 3.0*p3dq(2)*q1d3*q1d3*q2d3+ &
             3.0*p3dq(3)*q1d3*q1d3*q3d3 + 3.0*p3dq(4)*q1d3*q2d3*q2d3+ &
             6.0*p3dq(5)*q1d3*q2d3*q3d3 + 3.0*p3dq(6)*q1d3*q3d3*q3d3+ &
             p3dq(7)*q2d1*q2d3*q2d3 + 3.0*p3dq(8)*q2d3*q2d3*q3d3+ &
             3.0*p3dq(9)*q2d3*q3d3*q3d3 + p3dq(10)*q3d3*q3d3*q3d3
!
!  Second set of terms
!
          p3d(1) = p3d(1) + p1dq1*q13d(1)+p1dq2*q23d(1)+p1dq3*q33d(1)
          p3d(7) = p3d(7) + p1dq1*q13d(2)+p1dq2*q23d(2)+p1dq3*q33d(2)
          p3d(10) = p3d(10) + p1dq1*q13d(3)+p1dq2*q23d(3)+p1dq3*q33d(3)
!
!  Third set of terms
!
          p3d(1) = p3d(1) + 3.0*p2dq(1)*q12d(1)*q1d1+ &
                        3.0*p2dq(2)*q12d(1)*q2d1 +  &
                        3.0*p2dq(2)*q22d(1)*q1d1 +  &
                        3.0*p2dq(3)*q12d(1)*q3d1 +  &
                        3.0*p2dq(3)*q32d(1)*q1d1 +  &
                        3.0*p2dq(4)*q22d(1)*q2d1 +  &
                        3.0*p2dq(5)*q22d(1)*q3d1 +  &
                        3.0*p2dq(5)*q32d(1)*q2d1 +  &
                        3.0*p2dq(6)*q32d(1)*q3d1
          p3d(2) = p3d(2) + p2dq(1)*q12d(1)*q1d2+ &
                        p2dq(2)*q12d(1)*q2d2 + p2dq(2)*q1d2*q22d(1)+ &
                        p2dq(3)*q12d(1)*q3d2 + p2dq(3)*q1d2*q32d(1)+ &
                        p2dq(4)*q22d(1)*q2d2 +  &
                        p2dq(5)*q22d(1)*q3d2 + p2dq(5)*q2d2*q32d(1)+ &
                        p2dq(6)*q32d(1)*q3d2
          p3d(3) = p3d(3) + p2dq(1)*q12d(1)*q1d3+ &
                        p2dq(2)*q12d(1)*q2d3 + p2dq(2)*q1d3*q22d(1)+ &
                        p2dq(3)*q12d(1)*q3d3 + p2dq(3)*q1d3*q32d(1)+ &
                        p2dq(4)*q22d(1)*q2d3 +  &
                        p2dq(5)*q22d(1)*q3d3 + p2dq(5)*q2d3*q32d(1)+ &
                        p2dq(6)*q32d(1)*q3d3
          p3d(4) = p3d(4) + p2dq(1)*q12d(2)*q1d1+ &
                        p2dq(2)*q12d(2)*q2d1 + p2dq(2)*q1d1*q22d(2)+ &
                        p2dq(3)*q12d(2)*q3d1 + p2dq(3)*q1d1*q32d(2)+ &
                        p2dq(4)*q22d(2)*q2d1 +  &
                        p2dq(5)*q22d(2)*q3d1 + p2dq(5)*q2d1*q32d(2)+ &
                        p2dq(6)*q32d(2)*q3d1
          p3d(6) = p3d(6) + p2dq(1)*q12d(3)*q1d1+ &
                        p2dq(2)*q12d(3)*q2d1 + p2dq(2)*q1d1*q22d(3)+ &
                        p2dq(3)*q12d(3)*q3d1 + p2dq(3)*q1d1*q32d(3)+ &
                        p2dq(4)*q22d(3)*q2d1 +  &
                        p2dq(5)*q22d(3)*q3d1 + p2dq(5)*q2d1*q32d(3)+ &
                        p2dq(6)*q32d(3)*q3d1
          p3d(7) = p3d(7) + 3.0*p2dq(1)*q12d(2)*q1d2+ &
                        3.0*p2dq(2)*q12d(2)*q2d2 +  &
                        3.0*p2dq(2)*q22d(2)*q1d2 +  &
                        3.0*p2dq(3)*q12d(2)*q3d2 +  &
                        3.0*p2dq(3)*q32d(2)*q1d2 +  &
                        3.0*p2dq(4)*q22d(2)*q2d2 +  &
                        3.0*p2dq(5)*q22d(2)*q3d2 +  &
                        3.0*p2dq(5)*q32d(2)*q2d2 +  &
                        3.0*p2dq(6)*q32d(2)*q3d2
          p3d(8) = p3d(8) + p2dq(1)*q12d(2)*q1d3+ &
                        p2dq(2)*q12d(2)*q2d3 + p2dq(2)*q1d3*q22d(2)+ &
                        p2dq(3)*q12d(2)*q3d3 + p2dq(3)*q1d3*q32d(2)+ &
                        p2dq(4)*q22d(2)*q2d3 +  &
                        p2dq(5)*q22d(2)*q3d3 + p2dq(5)*q2d3*q32d(2)+ &
                        p2dq(6)*q32d(2)*q3d3
          p3d(9) = p3d(9) + p2dq(1)*q12d(3)*q1d2+ &
                        p2dq(2)*q12d(3)*q2d2 + p2dq(2)*q1d2*q22d(3)+ &
                        p2dq(3)*q12d(3)*q3d2 + p2dq(3)*q1d2*q32d(3)+ &
                        p2dq(4)*q22d(3)*q2d2 +  &
                        p2dq(5)*q22d(3)*q3d2 + p2dq(5)*q2d2*q32d(3)+ &
                        p2dq(6)*q32d(3)*q3d2
          p3d(10) = p3d(10) + 3.0*p2dq(1)*q12d(3)*q1d3+ &
                        3.0*p2dq(2)*q12d(3)*q2d3 +  &
                        3.0*p2dq(2)*q22d(3)*q1d3 +  &
                        3.0*p2dq(3)*q12d(3)*q3d3 +  &
                        3.0*p2dq(3)*q32d(3)*q1d3 +  &
                        3.0*p2dq(4)*q22d(3)*q2d3 +  &
                        3.0*p2dq(5)*q22d(3)*q3d3 +  &
                        3.0*p2dq(5)*q32d(3)*q2d3 +  &
                        3.0*p2dq(6)*q32d(3)*q3d3
!
!  Combine derivatives
!
!  Term 1
!
          do ii = 1,10
            e3d(ii) = rktrm*p3d(ii)
          enddo
!
!  Term 2
!
          e3d(1) = e3d(1) - 3.0*rktrm*theta0*(p2d(1)*q1d1 + q12d(1)*p1d1)
          e3d(2) = e3d(2) - rktrm*theta0*(p2d(1)*q1d2 + 2.0*p2d(2)*q1d1+q12d(1)*p1d2)
          e3d(3) = e3d(3) - rktrm*theta0*(p2d(1)*q1d3 + 2.0*p2d(3)*q1d1+q12d(1)*p1d3)
          e3d(4) = e3d(4) - rktrm*theta0*(p2d(4)*q1d1 + 2.0*p2d(2)*q1d2+q12d(2)*p1d1)
          e3d(5) = e3d(5) - rktrm*theta0*(p2d(2)*q1d3 + p2d(3)*q1d2+p2d(5)*q1d1)
          e3d(6) = e3d(6) - rktrm*theta0*(p2d(6)*q1d1 + 2.0*p2d(3)*q1d3+q12d(3)*p1d1)
          e3d(7) = e3d(7) - 3.0*rktrm*theta0*(p2d(2)*q1d2 + q12d(2)*p1d2)
          e3d(8) = e3d(8) - rktrm*theta0*(p2d(4)*q1d3 + 2.0*p2d(5)*q1d2+q12d(2)*p1d3)
          e3d(9) = e3d(9) - rktrm*theta0*(p2d(6)*q1d2 + 2.0*p2d(5)*q1d3+q12d(3)*p1d2)
          e3d(10) = e3d(10) - 3.0*rktrm*theta0*(p2d(3)*q1d3 + q12d(3)*p1d3)
!
!  Term 3
!
          e3d(1) = e3d(1) + 3.0*rktrm*theta0*theta0*(p1d1*q1d1*q1d1)
          e3d(2) = e3d(2) + rktrm*theta0*theta0*(2.0*p1d1*q1d1*q1d2+p1d2*q1d1*q1d1)
          e3d(3) = e3d(3) + rktrm*theta0*theta0*(2.0*p1d1*q1d1*q1d3+p1d3*q1d1*q1d1)
          e3d(4) = e3d(4) + rktrm*theta0*theta0*(2.0*p1d2*q1d1*q1d2+p1d1*q1d2*q1d2)
          e3d(5) = e3d(5) + rktrm*theta0*theta0*(p1d1*q1d2*q1d3+p1d2*q1d1*q1d3+p1d3*q1d1*q1d2)
          e3d(6) = e3d(6) + rktrm*theta0*theta0*(2.0*p1d3*q1d1*q1d3+p1d1*q1d3*q1d3)
          e3d(7) = e3d(7) + 3.0*rktrm*theta0*theta0*(p1d2*q1d2*q1d2)
          e3d(8) = e3d(8) + rktrm*theta0*theta0*(2.0*p1d2*q1d3*q1d2+p1d3*q1d2*q1d2)
          e3d(9) = e3d(9) + rktrm*theta0*theta0*(2.0*p1d3*q1d2*q1d3+p1d2*q1d3*q1d3)
          e3d(10) = e3d(10) + 3.0*rktrm*theta0*theta0*(p1d3*q1d3*q1d3)
!
!  Term 4
!
          e3d(1) = e3d(1) + 3.0*rktrm*theta0*theta0*p*(q1d1*q12d(1))
          e3d(2) = e3d(2) + rktrm*theta0*theta0*p*(q1d2*q12d(1))
          e3d(3) = e3d(3) + rktrm*theta0*theta0*p*(q1d3*q12d(1))
          e3d(4) = e3d(4) + rktrm*theta0*theta0*p*(q1d1*q12d(2))
          e3d(6) = e3d(6) + rktrm*theta0*theta0*p*(q1d1*q12d(3))
          e3d(7) = e3d(7) + 3.0*rktrm*theta0*theta0*p*(q1d2*q12d(2))
          e3d(8) = e3d(8) + rktrm*theta0*theta0*p*(q1d3*q12d(2))
          e3d(9) = e3d(9) + rktrm*theta0*theta0*p*(q1d2*q12d(3))
          e3d(10) = e3d(10) + 3.0*rktrm*theta0*theta0*p*(q1d3*q12d(3))
!
!  Term 5
!
          e3d(1) = e3d(1) - rktrm*theta0*theta0*theta0*p*q1d1*q1d1*q1d1
          e3d(2) = e3d(2) - rktrm*theta0*theta0*theta0*p*q1d1*q1d1*q1d2
          e3d(3) = e3d(3) - rktrm*theta0*theta0*theta0*p*q1d1*q1d1*q1d3
          e3d(4) = e3d(4) - rktrm*theta0*theta0*theta0*p*q1d1*q1d2*q1d2
          e3d(5) = e3d(5) - rktrm*theta0*theta0*theta0*p*q1d1*q1d2*q1d3
          e3d(6) = e3d(6) - rktrm*theta0*theta0*theta0*p*q1d1*q1d3*q1d3
          e3d(7) = e3d(7) - rktrm*theta0*theta0*theta0*p*q1d2*q1d2*q1d2
          e3d(8) = e3d(8) - rktrm*theta0*theta0*theta0*p*q1d2*q1d2*q1d3
          e3d(9) = e3d(9) - rktrm*theta0*theta0*theta0*p*q1d2*q1d3*q1d3
          e3d(10) = e3d(10) - rktrm*theta0*theta0*theta0*p*q1d3*q1d3*q1d3
!
!  Term 6
! 
          e3d(1) = e3d(1) - rktrm*theta0*p*q13d(1)
          e3d(7) = e3d(7) - rktrm*theta0*p*q13d(2)
          e3d(10) = e3d(10) - rktrm*theta0*p*q13d(3)
        endif
      endif
    endif
  elseif (n3ty.eq.11) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Bond - angle cross potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    if (ipivot.eq.1) then
      rd1 = r12 - rho1
      rd2 = r13 - rho2
      rktrm = (rkthb*rd1 + rkthb3*rd2)
    elseif (ipivot.eq.2) then
      rd1 = r12 - rho1
      rd2 = r23 - rho3
      rktrm = (rkthb*rd1 + rkthb3*rd2)
    else
      rd1 = r13 - rho2
      rd2 = r23 - rho3
      rktrm = (rkthb*rd1 + rkthb3*rd2)
    endif
    delth = thet - theta0
    ethb = rktrm*delth
    if (lgrad1) then
!
!  First derivatives
!
      if (ipivot.eq.1) then
        e1d1 = rkthb*rr12*delth
        e1d2 = rkthb3*rr13*delth
        e1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        e1d1 = rkthb*rr12*delth
        e1d2 = 0.0_dp
        e1d3 = rkthb3*rr23*delth
      else
        e1d1 = 0.0_dp
        e1d2 = rkthb*rr13*delth
        e1d3 = rkthb3*rr23*delth
      endif
      e1d1 = e1d1 + rktrm*the1d1
      e1d2 = e1d2 + rktrm*the1d2
      e1d3 = e1d3 + rktrm*the1d3
      if (lgrad2) then
!
!  Second derivatives
!
        do m = 1,6
          e2d(m) = rktrm*the2d(m)
        enddo
        if (ipivot.eq.1) then
          e2d(1) = e2d(1) + rkthb*rr12*(2.0_dp*the1d1 - rr122*delth)
          e2d(2) = e2d(2) + rkthb*rr12*the1d2+rkthb3*rr13*the1d1
          e2d(3) = e2d(3) + rkthb*rr12*the1d3
          e2d(4) = e2d(4) + rkthb3*rr13*(2.0_dp*the1d2 - rr132*delth)
          e2d(5) = e2d(5) + rkthb3*rr13*the1d3
        elseif (ipivot.eq.2) then
          e2d(1) = e2d(1) + rkthb*rr12*(2.0_dp*the1d1 - rr122*delth)
          e2d(2) = e2d(2) + rkthb*rr12*the1d2
          e2d(3) = e2d(3) + rkthb*rr12*the1d3+rkthb3*rr23*the1d1
          e2d(5) = e2d(5) + rkthb3*rr23*the1d2
          e2d(6) = e2d(6) + rkthb3*rr23*(2.0_dp*the1d3 - rr232*delth)
        else
          e2d(2) = e2d(2) + rkthb*rr13*the1d1
          e2d(3) = e2d(3) + rkthb3*rr23*the1d1
          e2d(4) = e2d(4) + rkthb*rr13*(2.0_dp*the1d2 - rr132*delth)
          e2d(5) = e2d(5) + rkthb*rr13*the1d3+rkthb3*rr23*the1d2
          e2d(6) = e2d(6) + rkthb3*rr23*(2.0_dp*the1d3 - rr232*delth)
        endif
        if (lgrad3) then
!
!  Third derivatives
!
          do m = 1,10
            e3d(m) = rktrm*the3d(m)
          enddo
          if (ipivot.eq.1) then
            e3d(1) = e3d(1) + 3.0_dp*rkthb*rr12*(rr122*(rr122*delth - the1d1)+the2d(1))
            e3d(2) = e3d(2) + 2.0_dp*rkthb*rr12*the2d(2)+rkthb3*rr13*the2d(1) - rkthb*rr122*rr12*the1d2
            e3d(3) = e3d(3) + 2.0_dp*rkthb*rr12*the2d(3) - rkthb*rr122*rr12*the1d3
            e3d(4) = e3d(4) + 2.0_dp*rkthb3*rr13*the2d(2)+rkthb*rr12*the2d(4) - rkthb3*rr132*rr13*the1d1
            e3d(5) = e3d(5) + rkthb*rr12*the2d(5)+rkthb3*rr13*the2d(3)
            e3d(6) = e3d(6) + rkthb*rr12*the2d(6)
            e3d(7) = e3d(7) + 3.0_dp*rkthb3*rr13*(rr132*(rr132*delth - the1d2)+the2d(4))
            e3d(8) = e3d(8) + 2.0_dp*rkthb3*rr13*the2d(5) - rkthb3*rr132*rr13*the1d3
            e3d(9) = e3d(9) + rkthb3*rr13*the2d(6)
          elseif (ipivot.eq.2) then
            e3d(1) = e3d(1) + 3.0_dp*rkthb*rr12*(rr122*(rr122*delth - the1d1)+the2d(1))
            e3d(2) = e3d(2) + 2.0_dp*rkthb*rr12*the2d(2) - rkthb*rr122*rr12*the1d2
            e3d(3) = e3d(3) + 2.0_dp*rkthb*rr12*the2d(3)+rkthb3*rr23*the2d(1) - rkthb*rr122*rr12*the1d3
            e3d(4) = e3d(4) + rkthb*rr12*the2d(4)
            e3d(5) = e3d(5) + rkthb*rr12*the2d(5)+rkthb3*rr23*the2d(2)
            e3d(6) = e3d(6) + 2.0_dp*rkthb3*rr23*the2d(3)+rkthb*rr12*the2d(6) - rkthb3*rr232*rr23*the1d1
            e3d(8) = e3d(8) + rkthb3*rr23*the2d(4)
            e3d(9) = e3d(9) + 2.0_dp*rkthb3*rr23*the2d(5) - rkthb3*rr232*rr23*the1d2
            e3d(10) = e3d(10) + 3.0_dp*rkthb3*rr23*(rr232*(rr232*delth - the1d3)+the2d(6))
          else
            e3d(2) = e3d(2) + rkthb*rr13*the2d(1)
            e3d(3) = e3d(3) + rkthb3*rr23*the2d(1)
            e3d(4) = e3d(4) + 2.0_dp*rkthb*rr13*the2d(2) - rkthb*rr132*rr13*the1d1
            e3d(5) = e3d(5) + rkthb*rr13*the2d(3)+rkthb3*rr23*the2d(2)
            e3d(6) = e3d(6) + 2.0_dp*rkthb3*rr23*the2d(3) - rkthb3*rr232*rr23*the1d1
            e3d(7) = e3d(7) + 3.0_dp*rkthb*rr13*(rr132*(rr132*delth - the1d2)+the2d(4))
            e3d(8) = e3d(8) + 2.0_dp*rkthb*rr13*the2d(5)+rkthb3*rr23*the2d(4) - rkthb*rr132*rr13*the1d3
            e3d(9) = e3d(9) + 2.0_dp*rkthb3*rr23*the2d(5)+rkthb*rr13*the2d(6) - rkthb3*rr232*rr23*the1d2
            e3d(10) = e3d(10) + 3.0_dp*rkthb3*rr23*(rr232*(rr232*delth - the1d3)+the2d(6))
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.12) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Linear-three potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
    cosntheta = cos(rkthb3*thet)
    ethb = rkthb*(1.0_dp + theta0*cosntheta)
    if (lgrad1) then
!
!  First derivatives of energy
!
!  d(cosntheta)/dcostheta = sinntheta/sintheta
!
!  to avoid divide by zero error when sintheta = 0 we use a series
!  expansion in costheta
!
      e1 = rkthb*theta0*rkthb3
      e2 = e1
      e3 = e1
      nmax = nint(rkthb3)
      cosd1 = sinratio(nmax,thet)
      e1 = e1*cosd1
      e1d1 = e1*cos1d1
      e1d2 = e1*cos1d2
      e1d3 = e1*cos1d3
      if (lgrad2) then
!
!  Second derivatives of energy
!
        do i1 = 1,6
          e2d(i1) = e1*cos2d(i1)
        enddo
        if (abs(nmax).gt.1) then
          cosd2 = dsinratio(nmax,thet)
          e2 = e2*cosd2
          e2d(1) = e2d(1) + e2*cos1d1*cos1d1
          e2d(2) = e2d(2) + e2*cos1d1*cos1d2
          e2d(3) = e2d(3) + e2*cos1d1*cos1d3
          e2d(4) = e2d(4) + e2*cos1d2*cos1d2
          e2d(5) = e2d(5) + e2*cos1d2*cos1d3
          e2d(6) = e2d(6) + e2*cos1d3*cos1d3
        endif
        if (lgrad3) then
!
!  Third derivatives of energy
!
          do i1 = 1,10
            e3d(i1) = e1*cos3d(i1)
          enddo
          if (abs(nmax).gt.1) then
            cosd3 = dsinratio2(nmax,thet)
            e3 = e3*cosd3
            e3d(1) = e3d(1) + e3*cos1d1*cos1d1*cos1d1 + 3.0_dp*e2*cos2d(1)*cos1d1
            e3d(2) = e3d(2) + e3*cos1d1*cos1d1*cos1d2 + e2*(2.0_dp*cos2d(2)*cos1d1 + cos2d(1)*cos1d2)
            e3d(3) = e3d(3) + e3*cos1d1*cos1d1*cos1d3 + e2*(2.0_dp*cos2d(3)*cos1d1 + cos2d(1)*cos1d3)
            e3d(4) = e3d(4) + e3*cos1d1*cos1d2*cos1d2 + e2*(2.0_dp*cos2d(2)*cos1d2 + cos2d(4)*cos1d1)
            e3d(5) = e3d(5) + e3*cos1d1*cos1d2*cos1d3 + e2*(cos2d(2)*cos1d3 + cos2d(3)*cos1d2 + cos2d(5)*cos1d1)
            e3d(6) = e3d(6) + e3*cos1d1*cos1d3*cos1d3 + e2*(2.0_dp*cos2d(3)*cos1d3 + cos2d(6)*cos1d1)
            e3d(7) = e3d(7) + e3*cos1d2*cos1d2*cos1d2 + 3.0_dp*e2*cos2d(4)*cos1d2
            e3d(8) = e3d(8) + e3*cos1d2*cos1d2*cos1d3 + e2*(2.0_dp*cos2d(5)*cos1d2 + cos2d(4)*cos1d3)
            e3d(9) = e3d(9) + e3*cos1d2*cos1d3*cos1d3 + e2*(2.0_dp*cos2d(5)*cos1d3 + cos2d(6)*cos1d2)
            e3d(10) = e3d(10) + e3*cos1d3*cos1d3*cos1d3 + 3.0_dp*e2*cos2d(6)*cos1d3
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.13) then
!$$$$$$$$$$$$$$$$$$$$$$$$
!  Bcoscross potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$
    rd1 = r12 - rho1
    rd2 = r13 - rho2
    rd3 = r23 - rho3
    if (ipivot.eq.1) then
      rtrm = rd1*rd2
    elseif (ipivot.eq.2) then
      rtrm = rd1*rd3
    else
      rtrm = rd2*rd3
    endif
    m = nint(threepoly(1,nptr3))
    cosntheta = cos(rkthb3*thet)
    cosnthetam = cosntheta**m
    e0 = rkthb*(1.0_dp + theta0*cosnthetam)
    ethb = e0*rtrm
    if (lgrad1) then
!
!  First derivatives of energy
!
!  d(cosntheta)/dcostheta = sinntheta/sintheta
!
!  to avoid divide by zero error when sintheta = 0 we use a series
!  expansion in costheta
!
      c1 = dble(m)*cosntheta**(m-1)
      e1 = rkthb*theta0*rkthb3
      e2 = e1
      e3 = e1
      nmax = nint(rkthb3)
      cosd1 = sinratio(nmax,thet)
      e1 = e1*cosd1
      e1c = e1*c1
      if (ipivot.eq.1) then
        r1d(1) = rd2*rr12
        r1d(2) = rd1*rr13
        r1d(3) = 0.0_dp
      elseif (ipivot.eq.2) then
        r1d(1) = rd3*rr12
        r1d(2) = 0.0_dp
        r1d(3) = rd1*rr23
      else
        r1d(1) = 0.0_dp
        r1d(2) = rd3*rr13
        r1d(3) = rd2*rr23
      endif
      e1d1 = e1c*cos1d1*rtrm + e0*r1d(1)
      e1d2 = e1c*cos1d2*rtrm + e0*r1d(2)
      e1d3 = e1c*cos1d3*rtrm + e0*r1d(3)
      if (lgrad2) then
!
!  Second derivatives of energy
!
!  Second derivatives with respect to angle only
!
        do i1 = 1,6
          c2d(i1) = e1c*cos2d(i1)
        enddo
        if (abs(nmax).gt.1) then
          cosd2 = dsinratio(nmax,thet)
          e2 = e2*cosd2
          c2d(1) = c2d(1) + e2*c1*cos1d1*cos1d1
          c2d(2) = c2d(2) + e2*c1*cos1d1*cos1d2
          c2d(3) = c2d(3) + e2*c1*cos1d1*cos1d3
          c2d(4) = c2d(4) + e2*c1*cos1d2*cos1d2
          c2d(5) = c2d(5) + e2*c1*cos1d2*cos1d3
          c2d(6) = c2d(6) + e2*c1*cos1d3*cos1d3
        endif
        if (m.gt.1) then
          c2 = dble(m*(m-1))*cosntheta**(m-2)
          ec2 = e1*c2*cosd1*rkthb3
          c2d(1) = c2d(1) + ec2*cos1d1*cos1d1
          c2d(2) = c2d(2) + ec2*cos1d1*cos1d2
          c2d(3) = c2d(3) + ec2*cos1d1*cos1d3
          c2d(4) = c2d(4) + ec2*cos1d2*cos1d2
          c2d(5) = c2d(5) + ec2*cos1d2*cos1d3
          c2d(6) = c2d(6) + ec2*cos1d3*cos1d3
        endif
!
!  Second derivatives with respect to distance only
!
        r2d(1:6) = 0.0_dp
        if (ipivot.eq.1) then
          r2d(1) =  - rd2*rr12*rr122
          r2d(2) =  rr12*rr13
          r2d(4) =  - rd1*rr13*rr132
        elseif (ipivot.eq.2) then
          r2d(1) =  - rd3*rr12*rr122
          r2d(3) =  rr12*rr23
          r2d(6) =  - rd1*rr23*rr232
        else
          r2d(4) =  - rd3*rr13*rr132
          r2d(5) =  rr13*rr23
          r2d(6) =  - rd2*rr23*rr232
        endif
!
        do i1 = 1,6
          e2d(i1) = c2d(i1)*rtrm + e0*r2d(i1)
        enddo
!
!  Second derivatives with respect to distance and angle
!
        e2d(1) = e2d(1) + 2.0_dp*e1c*cos1d1*r1d(1)
        e2d(2) = e2d(2) + e1c*(cos1d1*r1d(2) + cos1d2*r1d(1))
        e2d(3) = e2d(3) + e1c*(cos1d1*r1d(3) + cos1d3*r1d(1))
        e2d(4) = e2d(4) + 2.0_dp*e1c*cos1d2*r1d(2)
        e2d(5) = e2d(5) + e1c*(cos1d2*r1d(3) + cos1d3*r1d(2))
        e2d(6) = e2d(6) + 2.0_dp*e1c*cos1d3*r1d(3)
!
        if (lgrad3) then
!
!  Third derivatives of energy
!
!  Add angle only terms
!
          do i1 = 1,10
            e3d(i1) = e1c*cos3d(i1)*rtrm
          enddo
          if (abs(nmax).gt.1) then
            cosd3 = dsinratio2(nmax,thet)
            e3 = e3*cosd3*rtrm
            e3d(1) = e3d(1) + e3*c1*cos1d1*cos1d1*cos1d1 + 3.0_dp*e2*c1*cos2d(1)*cos1d1*rtrm
            e3d(2) = e3d(2) + e3*c1*cos1d1*cos1d1*cos1d2 + e2*c1*(2.0_dp*cos2d(2)*cos1d1 + cos2d(1)*cos1d2)*rtrm
            e3d(3) = e3d(3) + e3*c1*cos1d1*cos1d1*cos1d3 + e2*c1*(2.0_dp*cos2d(3)*cos1d1 + cos2d(1)*cos1d3)*rtrm
            e3d(4) = e3d(4) + e3*c1*cos1d1*cos1d2*cos1d2 + e2*c1*(2.0_dp*cos2d(2)*cos1d2 + cos2d(4)*cos1d1)*rtrm
            e3d(5) = e3d(5) + e3*c1*cos1d1*cos1d2*cos1d3 + e2*c1*(cos2d(2)*cos1d3 + cos2d(3)*cos1d2+cos2d(5)*cos1d1)*rtrm
            e3d(6) = e3d(6) + e3*c1*cos1d1*cos1d3*cos1d3 + e2*c1*(2.0_dp*cos2d(3)*cos1d3 + cos2d(6)*cos1d1)*rtrm
            e3d(7) = e3d(7) + e3*c1*cos1d2*cos1d2*cos1d2 + 3.0_dp*e2*c1*cos2d(4)*cos1d2*rtrm
            e3d(8) = e3d(8) + e3*c1*cos1d2*cos1d2*cos1d3 + e2*c1*(2.0_dp*cos2d(5)*cos1d2 + cos2d(4)*cos1d3)*rtrm
            e3d(9) = e3d(9) + e3*c1*cos1d2*cos1d3*cos1d3 + e2*c1*(2.0_dp*cos2d(5)*cos1d3 + cos2d(6)*cos1d2)*rtrm
            e3d(10) = e3d(10) + e3*c1*cos1d3*cos1d3*cos1d3 + 3.0_dp*e2*c1*cos2d(6)*cos1d3*rtrm
          endif
!
!  Derivatives from powers of cos(n*theta)
!
          if (m.gt.1) then
            e3c2 = e1*c2
            dc2 = 3.0_dp*e3c2*rtrm*cosd2*rkthb3
            dc3 = e3c2*rtrm*rkthb3*cosd1
            e3d(1) = e3d(1) + dc2*cos1d1*cos1d1*cos1d1 + 3.0_dp*dc3*cos2d(1)*cos1d1
            e3d(2) = e3d(2) + dc2*cos1d1*cos1d1*cos1d2 + dc3*(2.0_dp*cos2d(2)*cos1d1 + cos2d(1)*cos1d2)
            e3d(3) = e3d(3) + dc2*cos1d1*cos1d1*cos1d3 + dc3*(2.0_dp*cos2d(3)*cos1d1 + cos2d(1)*cos1d3)
            e3d(4) = e3d(4) + dc2*cos1d1*cos1d2*cos1d2 + dc3*(2.0_dp*cos2d(2)*cos1d2 + cos2d(4)*cos1d1)
            e3d(5) = e3d(5) + dc2*cos1d1*cos1d2*cos1d3 + dc3*(cos2d(2)*cos1d3 + cos2d(3)*cos1d2 + cos2d(5)*cos1d1)
            e3d(6) = e3d(6) + dc2*cos1d1*cos1d3*cos1d3 + dc3*(2.0_dp*cos2d(3)*cos1d3 + cos2d(6)*cos1d1)
            e3d(7) = e3d(7) + dc2*cos1d2*cos1d2*cos1d2 + 3.0_dp*dc3*cos2d(4)*cos1d2
            e3d(8) = e3d(8) + dc2*cos1d2*cos1d2*cos1d3 + dc3*(2.0_dp*cos2d(5)*cos1d2 + cos2d(4)*cos1d3)
            e3d(9) = e3d(9) + dc2*cos1d2*cos1d3*cos1d3 + dc3*(2.0_dp*cos2d(5)*cos1d3 + cos2d(6)*cos1d2)
            e3d(10) = e3d(10) + dc2*cos1d3*cos1d3*cos1d3 + 3.0_dp*dc3*cos2d(6)*cos1d3
!
            dc4 = e3c2*rkthb3*cosd1
            e3d(1) = e3d(1) + dc4*(3.0_dp*cos1d1*cos1d1*r1d(1))
            e3d(2) = e3d(2) + dc4*(cos1d1*cos1d1*r1d(2) + 2.0_dp*cos1d1*cos1d2*r1d(1))
            e3d(3) = e3d(3) + dc4*(cos1d1*cos1d1*r1d(3) + 2.0_dp*cos1d1*cos1d3*r1d(1))
            e3d(4) = e3d(4) + dc4*(cos1d2*cos1d2*r1d(1) + 2.0_dp*cos1d1*cos1d2*r1d(2))
            e3d(5) = e3d(5) + dc4*(cos1d1*cos1d2*r1d(3) + cos1d1*cos1d3*r1d(2) + cos1d2*cos1d3*r1d(1))
            e3d(6) = e3d(6) + dc4*(cos1d3*cos1d3*r1d(1) + 2.0_dp*cos1d1*cos1d3*r1d(3))
            e3d(7) = e3d(7) + dc4*(3.0_dp*cos1d2*cos1d2*r1d(2))
            e3d(8) = e3d(8) + dc4*(cos1d2*cos1d2*r1d(3) + 2.0_dp*cos1d2*cos1d3*r1d(2))
            e3d(9) = e3d(9) + dc4*(cos1d3*cos1d3*r1d(2) + 2.0_dp*cos1d2*cos1d3*r1d(3))
            e3d(10) = e3d(10) + dc4*(3.0_dp*cos1d3*cos1d3*r1d(3))
          endif
          if (m.gt.2) then
            c3 = dble(m*(m-1)*(m-2))*cosntheta**(m-3)
            e3c3 = e1*c3*rtrm*(rkthb3*cosd1)**2
            e3d(1) = e3d(1) + e3c3*cos1d1*cos1d1*cos1d1 
            e3d(2) = e3d(2) + e3c3*cos1d1*cos1d1*cos1d2 
            e3d(3) = e3d(3) + e3c3*cos1d1*cos1d1*cos1d3 
            e3d(4) = e3d(4) + e3c3*cos1d1*cos1d2*cos1d2 
            e3d(5) = e3d(5) + e3c3*cos1d1*cos1d2*cos1d3 
            e3d(6) = e3d(6) + e3c3*cos1d1*cos1d3*cos1d3 
            e3d(7) = e3d(7) + e3c3*cos1d2*cos1d2*cos1d2 
            e3d(8) = e3d(8) + e3c3*cos1d2*cos1d2*cos1d3 
            e3d(9) = e3d(9) + e3c3*cos1d2*cos1d3*cos1d3 
            e3d(10) = e3d(10) + e3c3*cos1d3*cos1d3*cos1d3 
          endif
!
!  Add distance only terms
!
          r3d(1:10) = 0.0_dp
          if (ipivot.eq.1) then
            r3d(1) = - 3.0_dp*r2d(1)*rr122
            r3d(2) = - e2d(2)*rr122
            r3d(4) = - e2d(2)*rr132
            r3d(7) = - 3.0_dp*r2d(4)*rr132
          elseif (ipivot.eq.2) then
            r3d(1) = - 3.0_dp*r2d(1)*rr122
            r3d(3) = - r2d(3)*rr122
            r3d(6) = - r2d(3)*rr232
            r3d(10) = - 3.0_dp*r2d(6)*rr232
          else
            r3d(7) = - 3.0_dp*r2d(4)*rr132
            r3d(8) = - r2d(5)*rr132
            r3d(9) = - r2d(5)*rr232
            r3d(10) = - 3.0_dp*r2d(6)*rr232
          endif
          do i1 = 1,10
            e3d(i1) = e3d(i1) + e0*r3d(i1)
          enddo
!
!  Add distance - angle terms
!
!  Products of first derivatives
!
          e3d(1) = e3d(1) + 3.0_dp*e2*c1*cos1d1*cos1d1*r1d(1)
          e3d(2) = e3d(2) + e2*c1*(2.0_dp*cos1d1*cos1d2*r1d(1) + cos1d1*cos1d1*r1d(2))
          e3d(3) = e3d(3) + e2*c1*(2.0_dp*cos1d1*cos1d3*r1d(1) + cos1d1*cos1d1*r1d(3))
          e3d(4) = e3d(4) + e2*c1*(2.0_dp*cos1d1*cos1d2*r1d(2) + cos1d2*cos1d2*r1d(1))
          e3d(5) = e3d(5) + e2*c1*(cos1d1*cos1d2*r1d(3) + cos1d1*cos1d3*r1d(2) + cos1d3*cos1d2*r1d(1))
          e3d(6) = e3d(6) + e2*c1*(2.0_dp*cos1d1*cos1d3*r1d(3) + cos1d3*cos1d3*r1d(1))
          e3d(7) = e3d(7) + 3.0_dp*e2*c1*cos1d2*cos1d2*r1d(2)
          e3d(8) = e3d(8) + e2*c1*(2.0_dp*cos1d2*cos1d3*r1d(2) + cos1d2*cos1d2*r1d(3))
          e3d(9) = e3d(9) + e2*c1*(2.0_dp*cos1d2*cos1d3*r1d(3) + cos1d3*cos1d3*r1d(2))
          e3d(10) = e3d(10) + 3.0_dp*e2*c1*cos1d3*cos1d3*r1d(3)
!
!  Products of first and second derivatives
!
          e3d(1) = e3d(1) + 3.0_dp*e1c*(cos2d(1)*r1d(1) + cos1d1*r2d(1))
          e3d(2) = e3d(2) + e1c*(2.0_dp*(cos2d(2)*r1d(1) + cos1d1*r2d(2)) + cos2d(1)*r1d(2) + cos1d2*r2d(1))
          e3d(3) = e3d(3) + e1c*(2.0_dp*(cos2d(3)*r1d(1) + cos1d1*r2d(3)) + cos2d(1)*r1d(3) + cos1d3*r2d(1))
          e3d(4) = e3d(4) + e1c*(2.0_dp*(cos2d(2)*r1d(2) + cos1d2*r2d(2)) + cos2d(4)*r1d(1) + cos1d1*r2d(4))
          e3d(5) = e3d(5) + e1c*(cos2d(5)*r1d(1) + cos2d(3)*r1d(2) + cos2d(2)*r1d(3)  &
                          + cos1d1*r2d(5) + cos1d2*r2d(3) + cos1d3*r2d(2))
          e3d(6) = e3d(6) + e1c*(2.0_dp*(cos2d(3)*r1d(3) + cos1d3*r2d(3)) + cos2d(6)*r1d(1) + cos1d1*r2d(6))
          e3d(7) = e3d(7) + 3.0_dp*e1c*(cos2d(4)*r1d(2) + cos1d2*r2d(4))
          e3d(8) = e3d(8) + e1c*(2.0_dp*(cos2d(5)*r1d(2) + cos1d2*r2d(5)) + cos2d(4)*r1d(3) + cos1d3*r2d(4))
          e3d(9) = e3d(9) + e1c*(2.0_dp*(cos2d(5)*r1d(3) + cos1d3*r2d(5)) + cos2d(6)*r1d(2) + cos1d2*r2d(6))
          e3d(10) = e3d(10) + 3.0_dp*e1c*(cos2d(3)*r1d(3) + cos1d3*r2d(6))
        endif
      endif
    endif
  elseif (n3ty.eq.14) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Stillinger - Weber 3 - body / Jiang & Brown modification  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Construct charge softening functions
!
    if (ipivot.eq.1) then
      ro3 = rho3
    else
      call outerror('pivot must be atom 1 in call to threebody for sw3jb',0_i4)
      call stopnow('threebody')
    endif
    if (ql1+ql2.lt.ro3) then
      rqtrm12 = 1.0_dp/(ql1+ql2-ro3)
      g12 = exp(1.0_dp/ro3)*exp(rqtrm12)
    else
      rqtrm12 = 0.0_dp
      g12 = 0.0_dp
    endif
    if (ql1+ql3.lt.ro3) then
      rqtrm13 = 1.0_dp/(ql1+ql3-ro3)
      g13 = exp(1.0_dp/ro3)*exp(rqtrm13)
    else
      rqtrm13 = 0.0_dp
      g13 = 0.0_dp
    endif
!
!  Construct angle based terms
!
    e0 = rkthb*(dot - theta0)*(dot - theta0)*g12*g13
    if (lgrad1) then
      e1 = 2.0_dp*rkthb*(dot - theta0)*g12*g13
      e2 = 2.0_dp*rkthb*g12*g13
!
!  First derivatives of energy w.r.t. to angle
!
      a1d1 = e1*cos1d1
      a1d2 = e1*cos1d2
      a1d3 = e1*cos1d3
!
!  Charge derivatives
!
      rqtrm122 = rqtrm12*rqtrm12
      rqtrm132 = rqtrm13*rqtrm13
      d0q2 = - e0*rqtrm122
      d0q3 = - e0*rqtrm132
      d0q1 = d0q2 + d0q3
!
      if (lgrad2) then
!
!  Second charge derivatives
!
        d2q(1) = e0*(2.0_dp*rqtrm122*rqtrm132 + rqtrm122*(rqtrm122 + 2.0_dp*rqtrm12) + rqtrm132*(rqtrm132 + 2.0_dp*rqtrm13))
        d2q(2) = e0*(rqtrm122*rqtrm132 + rqtrm122*(rqtrm122 + 2.0_dp*rqtrm12))
        d2q(3) = e0*(rqtrm122*rqtrm132 + rqtrm132*(rqtrm132 + 2.0_dp*rqtrm13))
        d2q(4) = e0*(rqtrm122*(rqtrm122 + 2.0_dp*rqtrm12))
        d2q(5) = e0*(rqtrm122*rqtrm132)
        d2q(6) = e0*(rqtrm132*(rqtrm132 + 2.0_dp*rqtrm13))
!
!  Second derivatives of energy w.r.t. to angle
!
        a2d(1) = e1*cos2d(1) + e2*cos1d1*cos1d1
        a2d(2) = e1*cos2d(2) + e2*cos1d1*cos1d2
        a2d(3) = e1*cos2d(3) + e2*cos1d1*cos1d3
        a2d(4) = e1*cos2d(4) + e2*cos1d2*cos1d2
        a2d(5) = e1*cos2d(5) + e2*cos1d2*cos1d3
        a2d(6) = e1*cos2d(6) + e2*cos1d3*cos1d3
        if (lgrad3) then
!
!  Third derivatives of energy w.r.t. to angle
!
          a3d(1) = e1*cos3d(1) + 3.0_dp*e2*cos2d(1)*cos1d1
          a3d(2) = e1*cos3d(2) + e2*(2.0_dp*cos2d(2)*cos1d1+cos2d(1)*cos1d2)
          a3d(3) = e1*cos3d(3) + e2*(2.0_dp*cos2d(3)*cos1d1+cos2d(1)*cos1d3)
          a3d(4) = e1*cos3d(4) + e2*(2.0_dp*cos2d(2)*cos1d2+cos2d(4)*cos1d1)
          a3d(5) = e1*cos3d(5) + e2*(cos2d(2)*cos1d3+cos2d(3)*cos1d2+cos2d(5)*cos1d1)
          a3d(6) = e1*cos3d(6) + e2*(2.0_dp*cos2d(3)*cos1d3+cos2d(6)*cos1d1)
          a3d(7) = e1*cos3d(7) + 3.0_dp*e2*cos2d(4)*cos1d2
          a3d(8) = e1*cos3d(8) + e2*(2.0_dp*cos2d(5)*cos1d2+cos2d(4)*cos1d3)
          a3d(9) = e1*cos3d(9) + e2*(2.0_dp*cos2d(5)*cos1d3+cos2d(6)*cos1d2)
          a3d(10) = e1*cos3d(10) + 3.0_dp*e2*cos2d(6)*cos1d3
        endif
      endif
    endif
!
!  Distance dependent terms
!
    if (ipivot.eq.1) then
      ro1 = rho1
      ro2 = rho2
      if (r12.ge.cut12.or.r13.ge.cut13) then
        rtrm1 = 0.0_dp
        rtrm2 = 0.0_dp
        et1 = 0.0_dp
      else
        rtrm1 = 1.0_dp/(r12 - cut12 + 1.0d-28)
        rtrm2 = 1.0_dp/(r13 - cut13 + 1.0d-28)
        et1 = exp(ro1*rtrm1 + ro2*rtrm2)
      endif
    elseif (ipivot.eq.2) then
      ro1  =  rho1
      ro2  =  rho3
      if (r12.ge.cut12.or.r23.ge.cut23) then
        rtrm1 = 0.0_dp
        rtrm2 = 0.0_dp
        et1 = 0.0_dp
      else
        rtrm1 = 1.0_dp/(r12 - cut12 + 1.0d-28)
        rtrm2 = 1.0_dp/(r23 - cut23 + 1.0d-28)
        et1 = exp(ro1*rtrm1 + ro2*rtrm2)
      endif
    else
      ro1  =  rho2
      ro2  =  rho3
      if (r13.ge.cut13.or.r23.ge.cut23) then
        rtrm1 = 0.0_dp
        rtrm2 = 0.0_dp
        et1 = 0.0_dp
      else
        rtrm1 = 1.0_dp/(r13 - cut13 + 1.0d-28)
        rtrm2 = 1.0_dp/(r23 - cut23 + 1.0d-28)
        et1 = exp(ro1*rtrm1 + ro2*rtrm2)
      endif
    endif
    ethb = e0*et1
    if (lgrad1) then
!
!  Multiply derivatives with respect to charge by et1
!
      d0q1 = d0q1*et1
      d0q2 = d0q2*et1
      d0q3 = d0q3*et1
!
!  First derivatives w.r.t. distance
!
      if (ipivot.eq.1) then
        d1d1 = - ro1*rtrm1*rtrm1*rr12*et1
        d1d2 = - ro2*rtrm2*rtrm2*rr13*et1
        d1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        d1d1 = - ro1*rtrm1*rtrm1*rr12*et1
        d1d2 = 0.0_dp
        d1d3 = - ro2*rtrm2*rtrm2*rr23*et1
      else
        d1d1 = 0.0_dp
        d1d2 = - ro1*rtrm1*rtrm1*rr13*et1
        d1d3 = - ro2*rtrm2*rtrm2*rr23*et1
      endif
      if (lgrad2) then
!
!  Multiply derivatives with respect to charge by et1
!
        d2q(1) = d2q(1)*et1
        d2q(2) = d2q(2)*et1
        d2q(3) = d2q(3)*et1
        d2q(4) = d2q(4)*et1
        d2q(5) = d2q(5)*et1
        d2q(6) = d2q(6)*et1
!
!  Second derivatives w.r.t. distance
!
        do mm = 1,6
          d2d(mm) = 0.0_dp
        enddo
        if (ipivot.eq.1) then
          d2d(1) = ro1*rtrm1*rtrm1*rr122*et1*(ro1*rtrm1*rtrm1 + rr12+2.0_dp*rtrm1)
          d2d(2) = ro1*ro2*rtrm1*rtrm1*rtrm2*rtrm2*rr12*rr13*et1
          d2d(4) = ro2*rtrm2*rtrm2*rr132*et1*(ro2*rtrm2*rtrm2 + rr13+2.0_dp*rtrm2)
        elseif (ipivot.eq.2) then
          d2d(1) = ro1*rtrm1*rtrm1*rr122*et1*(ro1*rtrm1*rtrm1 + rr12+2.0_dp*rtrm1)
          d2d(3) = ro1*ro2*rtrm1*rtrm1*rtrm2*rtrm2*rr12*rr23*et1
          d2d(6) = ro2*rtrm2*rtrm2*rr232*et1*(ro2*rtrm2*rtrm2 + rr23+2.0_dp*rtrm2)
        else
          d2d(4) = ro1*rtrm1*rtrm1*rr132*et1*(ro1*rtrm1*rtrm1 + rr13+2.0_dp*rtrm1)
          d2d(5) = ro1*ro2*rtrm1*rtrm1*rtrm2*rtrm2*rr13*rr23*et1
          d2d(6) = ro2*rtrm2*rtrm2*rr232*et1*(ro2*rtrm2*rtrm2 + rr23+2.0_dp*rtrm2)
        endif
        if (lgrad3) then
!
!  Third derivatives w.r.t. distance
!
          do mm = 1,10
            d3d(mm) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            d3d(1) =  - ro1*rr122*rr12*rtrm1*rtrm1*et1*(ro1*ro1*rtrm1* &
                 rtrm1*rtrm1*rtrm1 + 3.0_dp*ro1*rr12*rtrm1*rtrm1+ &
                 6.0_dp*ro1*rtrm1*rtrm1*rtrm1 + 3.0_dp*rr122+6.0_dp* &
                 rr12*rtrm1 + 6.0_dp*rtrm1*rtrm1)
            d3d(2) =  - d2d(1)*ro2*rr13*rtrm2*rtrm2
            d3d(4) =  - d2d(4)*ro1*rr12*rtrm1*rtrm1
            d3d(7) =  - ro2*rr132*rr13*rtrm2*rtrm2*et1*(ro2*ro2*rtrm2* &
                 rtrm2*rtrm2*rtrm2 + 3.0_dp*ro1*rr13*rtrm2*rtrm2+ &
                 6.0_dp*ro2*rtrm2*rtrm2*rtrm2 + 3.0_dp*rr132+6.0_dp* &
                 rr13*rtrm2 + 6.0_dp*rtrm2*rtrm2)
          elseif (ipivot.eq.2) then
            d3d(1) =  - ro1*rr122*rr12*rtrm1*rtrm1*et1*(ro1*ro1*rtrm1* &
                 rtrm1*rtrm1*rtrm1 + 3.0_dp*ro1*rr12*rtrm1*rtrm1+ &
                 6.0_dp*ro1*rtrm1*rtrm1*rtrm1 + 3.0_dp*rr122+6.0_dp* &
                 rr12*rtrm1 + 6.0_dp*rtrm1*rtrm1)
            d3d(3) =  - d2d(1)*ro2*rr23*rtrm2*rtrm2
            d3d(6) =  - d2d(6)*ro1*rr12*rtrm1*rtrm1
            d3d(10) =  - ro2*rr232*rr23*rtrm2*rtrm2*et1*(ro2*ro2*rtrm2* &
                 rtrm2*rtrm2*rtrm2 + 3.0_dp*ro2*rr23*rtrm2*rtrm2+ &
                 6.0_dp*ro2*rtrm2*rtrm2*rtrm2 + 3.0_dp*rr232+6.0_dp* &
                 rr23*rtrm2 + 6.0_dp*rtrm2*rtrm2)
          else
            d3d(7) =  - ro1*rr132*rr13*rtrm1*rtrm1*et1*(ro1*ro1*rtrm1* &
                 rtrm1*rtrm1*rtrm1 + 3.0_dp*ro1*rr13*rtrm1*rtrm1+ &
                 6.0_dp*ro1*rtrm1*rtrm1*rtrm1 + 3.0_dp*rr132+6.0_dp* &
                 rr13*rtrm1 + 6.0_dp*rtrm1*rtrm1)
            d3d(8) =  - d2d(4)*ro2*rr23*rtrm2*rtrm2
            d3d(9) =  - d2d(6)*ro1*rr13*rtrm1*rtrm1
            d3d(10) =  - ro2*rr232*rr23*rtrm2*rtrm2*et1*(ro2*ro2*rtrm2* &
                 rtrm2*rtrm2*rtrm2 + 3.0_dp*ro2*rr23*rtrm2*rtrm2+ &
                 6.0_dp*ro2*rtrm2*rtrm2*rtrm2 + 3.0_dp*rr232+6.0_dp* &
                 rr23*rtrm2 + 6.0_dp*rtrm2*rtrm2)
          endif
        endif
      endif
!
!  Combine derivatives from angle and distance terms
!
!  First derivatives
!
      e1d1 = a1d1*et1 + e0*d1d1
      e1d2 = a1d2*et1 + e0*d1d2
      e1d3 = a1d3*et1 + e0*d1d3
      if (lgrad2) then
!
!  Charge derivatives w.r.t. first derivatives
!
        d1q(1,1) = - e1d1*(rqtrm12*rqtrm12 + rqtrm13*rqtrm13)
        d1q(1,2) = - e1d1*rqtrm12*rqtrm12
        d1q(1,3) = - e1d1*rqtrm13*rqtrm13
        d1q(2,1) = - e1d2*(rqtrm12*rqtrm12 + rqtrm13*rqtrm13)
        d1q(2,2) = - e1d2*rqtrm12*rqtrm12
        d1q(2,3) = - e1d2*rqtrm13*rqtrm13
        d1q(3,1) = - e1d3*(rqtrm12*rqtrm12 + rqtrm13*rqtrm13)
        d1q(3,2) = - e1d3*rqtrm12*rqtrm12
        d1q(3,3) = - e1d3*rqtrm13*rqtrm13
!
!  Second derivatives
!
        e2d(1) = a2d(1)*et1 + 2.0_dp*a1d1*d1d1+e0*d2d(1)
        e2d(2) = a2d(2)*et1 + a1d1*d1d2+a1d2*d1d1+e0*d2d(2)
        e2d(3) = a2d(3)*et1 + a1d1*d1d3+a1d3*d1d1+e0*d2d(3)
        e2d(4) = a2d(4)*et1 + 2.0_dp*a1d2*d1d2+e0*d2d(4)
        e2d(5) = a2d(5)*et1 + a1d2*d1d3+a1d3*d1d2+e0*d2d(5)
        e2d(6) = a2d(6)*et1 + 2.0_dp*a1d3*d1d3+e0*d2d(6)
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) = a3d(1)*et1 + 3.0_dp*a2d(1)*d1d1+3.0_dp*a1d1*d2d(1)+e0*d3d(1)
          e3d(2) = a3d(2)*et1 + 2.0_dp*a2d(2)*d1d1+a2d(1)*d1d2+2.0_dp*d2d(2)*a1d1+d2d(1)*a1d2+e0*d3d(2)
          e3d(3) = a3d(3)*et1 + 2.0_dp*a2d(3)*d1d1+a2d(1)*d1d3+2.0_dp*d2d(3)*a1d1+d2d(1)*a1d3+e0*d3d(3)
          e3d(4) = a3d(4)*et1 + 2.0_dp*a2d(2)*d1d2+a2d(4)*d1d1+2.0_dp*d2d(2)*a1d2+d2d(4)*a1d1+e0*d3d(4)
          e3d(5) = a3d(5)*et1 + a2d(2)*d1d3+a2d(3)*d1d2+a2d(5)*d1d1+a1d1*d2d(5)+a1d2*d2d(3)+a1d3*d2d(2)+e0*d3d(5)
          e3d(6) = a3d(6)*et1 + 2.0_dp*a2d(3)*d1d3+a2d(6)*d1d1+2.0_dp*d2d(3)*a1d3+d2d(6)*a1d1+e0*d3d(6)
          e3d(7) = a3d(7)*et1 + 3.0_dp*a2d(4)*d1d2+3.0_dp*a1d2*d2d(4)+e0*d3d(7)
          e3d(8) = a3d(8)*et1 + 2.0_dp*a2d(5)*d1d2+a2d(4)*d1d3+2.0_dp*d2d(5)*a1d2+d2d(4)*a1d3+e0*d3d(8)
          e3d(9) = a3d(9)*et1 + 2.0_dp*a2d(5)*d1d3+a2d(6)*d1d2+2.0_dp*d2d(5)*a1d3+d2d(6)*a1d2+e0*d3d(9)
          e3d(10) = a3d(10)*et1 + 3.0_dp*a2d(6)*d1d3+3.0_dp*a1d3*d2d(6)+e0*d3d(10)
        endif
      endif
    endif
  elseif (n3ty.eq.15) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Hydrogen-bond 3 - body  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Construct angle based terms
!
    m = nint(rho1)
    n = nint(rho2)
    k = nint(rho3)
    if (dot.gt.0.0_dp) then
      ethb = 0.0_dp
      if (lgrad1) then
        e1d1 = 0.0_dp
        e1d2 = 0.0_dp
        e1d3 = 0.0_dp
        if (lgrad2) e2d(1:6) = 0.0_dp
        if (lgrad3) e3d(1:10) = 0.0_dp
      endif
    else
      dotk = dot**k
      if (lthetatap) then
!
!  Compute taper function if required
!
        dot2 = dot*dot
        dotmin = cos(thetamin)
        dotmax = cos(thetamax)
        dotmin2 = dotmin**2
        dotmax2 = dotmax**2
        call ataper(.false.,dot2,dotmin2,dotmax2,ttap,dttapdr,d2ttapdr2,d3ttapdr3,lgrad1,lgrad2,lgrad3)
        e0 = ttap*dotk
      else
        ttap = 1.0_dp
        e0 = dotk
      endif
      if (lgrad1) then
        if (k.ge.1) then
          dotkm1 = dble(k)*dot**(k-1)
          e1 = ttap*dotkm1
        else
          dotkm1 = 0.0_dp
          e1 = 0.0_dp
        endif
!
!  First derivatives of energy w.r.t. to angle
!
        a1d1 = e1*cos1d1
        a1d2 = e1*cos1d2
        a1d3 = e1*cos1d3
        if (lthetatap) then
          ttrm1 = 2.0_dp*dot*dttapdr
          t1d1 = ttrm1*cos1d1
          t1d2 = ttrm1*cos1d2
          t1d3 = ttrm1*cos1d3
          a1d1 = a1d1 + dotk*t1d1
          a1d2 = a1d2 + dotk*t1d2
          a1d3 = a1d3 + dotk*t1d3
        endif
        if (lgrad2) then
!
!  Second derivatives of energy w.r.t. to angle
!
          if (k.ge.2) then
            dotkm2 = (k-1)*k*dot**(k-2)
            e2 = dotkm2*ttap
          else
            dotkm2 = 0.0_dp
            e2 = 0.0_dp
          endif
          a2d(1) = e1*cos2d(1) + e2*cos1d1*cos1d1
          a2d(2) = e1*cos2d(2) + e2*cos1d1*cos1d2
          a2d(3) = e1*cos2d(3) + e2*cos1d1*cos1d3
          a2d(4) = e1*cos2d(4) + e2*cos1d2*cos1d2
          a2d(5) = e1*cos2d(5) + e2*cos1d2*cos1d3
          a2d(6) = e1*cos2d(6) + e2*cos1d3*cos1d3
          if (lthetatap) then
            ttrm2 = 2.0_dp*dttapdr + 4.0_dp*dot*dot*d2ttapdr2
            t2d(1) = ttrm1*cos2d(1) + ttrm2*cos1d1*cos1d1
            t2d(2) = ttrm1*cos2d(2) + ttrm2*cos1d1*cos1d2
            t2d(3) = ttrm1*cos2d(3) + ttrm2*cos1d1*cos1d3
            t2d(4) = ttrm1*cos2d(4) + ttrm2*cos1d2*cos1d2
            t2d(5) = ttrm1*cos2d(5) + ttrm2*cos1d2*cos1d3
            t2d(6) = ttrm1*cos2d(6) + ttrm2*cos1d3*cos1d3
!
            a2d(1) = a2d(1) + dotk*t2d(1) + dotkm1*(t1d1*cos1d1 + t1d1*cos1d1)
            a2d(2) = a2d(2) + dotk*t2d(2) + dotkm1*(t1d1*cos1d2 + t1d2*cos1d1)
            a2d(3) = a2d(3) + dotk*t2d(3) + dotkm1*(t1d1*cos1d3 + t1d3*cos1d1)
            a2d(4) = a2d(4) + dotk*t2d(4) + dotkm1*(t1d2*cos1d2 + t1d2*cos1d2)
            a2d(5) = a2d(5) + dotk*t2d(5) + dotkm1*(t1d2*cos1d3 + t1d3*cos1d2)
            a2d(6) = a2d(6) + dotk*t2d(6) + dotkm1*(t1d3*cos1d3 + t1d3*cos1d3)
          endif
          if (lgrad3) then
!
!  Third derivatives of energy w.r.t. to angle
! 
!  NB = add e3 term in here
!
            if (k.ge.3) then
              dotkm3 = (k-2)*(k-1)*k*dot**(k-3)
              e3 = dotkm3*ttap
            else
              dotkm3 = 0.0_dp
              e3 = 0.0_dp
            endif
            a3d(1) = e1*cos3d(1) + 3.0_dp*e2*cos2d(1)*cos1d1 + e3*cos1d1*cos1d1*cos1d1
            a3d(2) = e1*cos3d(2) + e2*(2.0_dp*cos2d(2)*cos1d1+cos2d(1)*cos1d2) + e3*cos1d1*cos1d1*cos1d2
            a3d(3) = e1*cos3d(3) + e2*(2.0_dp*cos2d(3)*cos1d1+cos2d(1)*cos1d3) + e3*cos1d1*cos1d1*cos1d3
            a3d(4) = e1*cos3d(4) + e2*(2.0_dp*cos2d(2)*cos1d2+cos2d(4)*cos1d1) + e3*cos1d1*cos1d2*cos1d2
            a3d(5) = e1*cos3d(5) + e2*(cos2d(2)*cos1d3+cos2d(3)*cos1d2+cos2d(5)*cos1d1) + e3*cos1d1*cos1d2*cos1d3
            a3d(6) = e1*cos3d(6) + e2*(2.0_dp*cos2d(3)*cos1d3+cos2d(6)*cos1d1) + e3*cos1d1*cos1d3*cos1d3
            a3d(7) = e1*cos3d(7) + 3.0_dp*e2*cos2d(4)*cos1d2 + e3*cos1d2*cos1d2*cos1d2
            a3d(8) = e1*cos3d(8) + e2*(2.0_dp*cos2d(5)*cos1d2+cos2d(4)*cos1d3) + e3*cos1d2*cos1d2*cos1d3
            a3d(9) = e1*cos3d(9) + e2*(2.0_dp*cos2d(5)*cos1d3+cos2d(6)*cos1d2) + e3*cos1d2*cos1d3*cos1d3
            a3d(10) = e1*cos3d(10) + 3.0_dp*e2*cos2d(6)*cos1d3 + e3*cos1d3*cos1d3*cos1d3
            if (lthetatap) then
              ttrm3 = dot*(12.0_dp*d2ttapdr2 + 8.0_dp*dot*dot*d3ttapdr3)
              t3d(1)  = ttrm1*cos3d(1)  + ttrm2*(3.0_dp*cos2d(1)*cos1d1) + &
                        ttrm3*cos1d1*cos1d1*cos1d1
              t3d(2)  = ttrm1*cos3d(2)  + ttrm2*(2.0_dp*cos2d(2)*cos1d1 + cos2d(1)*cos1d2) + &
                        ttrm3*cos1d1*cos1d1*cos1d2
              t3d(3)  = ttrm1*cos3d(3)  + ttrm2*(2.0_dp*cos2d(3)*cos1d1 + cos2d(1)*cos1d3) + &
                        ttrm3*cos1d1*cos1d1*cos1d3
              t3d(4)  = ttrm1*cos3d(4)  + ttrm2*(2.0_dp*cos2d(2)*cos1d2 + cos2d(4)*cos1d1) + &
                        ttrm3*cos1d1*cos1d2*cos1d2
              t2d(5)  = ttrm1*cos3d(5)  + ttrm2*(cos2d(2)*cos1d3 + cos2d(3)*cos1d2 + cos2d(5)*cos1d1) + &
                        ttrm3*cos1d1*cos1d2*cos1d3
              t3d(6)  = ttrm1*cos3d(6)  + ttrm2*(2.0_dp*cos2d(3)*cos1d3 + cos2d(6)*cos1d1) + &
                        ttrm3*cos1d1*cos1d3*cos1d3
              t3d(7)  = ttrm1*cos3d(7)  + ttrm2*(3.0_dp*cos2d(4)*cos1d2) + &
                        ttrm3*cos1d2*cos1d2*cos1d2
              t3d(8)  = ttrm1*cos3d(8)  + ttrm2*(2.0_dp*cos2d(5)*cos1d2 + cos2d(4)*cos1d3) + &
                        ttrm3*cos1d2*cos1d2*cos1d3
              t3d(9)  = ttrm1*cos3d(9)  + ttrm2*(2.0_dp*cos2d(5)*cos1d3 + cos2d(6)*cos1d2) + &
                        ttrm3*cos1d2*cos1d3*cos1d3
              t3d(10) = ttrm1*cos3d(10) + ttrm2*(3.0_dp*cos2d(6)*cos1d3) + &
                        ttrm3*cos1d3*cos1d3*cos1d3
!
              a3d(1)  = a3d(1)  + dotk*t3d(1)  + 3.0_dp*dotkm1*(cos2d(1)*t1d1 + t2d(1)*cos1d1) +  &
                                  3.0_dp*dotkm2*cos1d1*cos1d1*t1d1
              a3d(2)  = a3d(2)  + dotk*t3d(2)  + dotkm1*(2.0_dp*cos2d(2)*t1d1 + cos2d(1)*t1d2 +  &
                                                         2.0_dp*t2d(2)*cos1d1 + t2d(1)*cos1d2) +  &
                                  dotkm2*(2.0_dp*cos1d1*cos1d2*t1d1 + cos1d1*cos1d1*t1d2)
              a3d(3)  = a3d(3)  + dotk*t3d(3)  + dotkm1*(2.0_dp*cos2d(3)*t1d1 + cos2d(1)*t1d3 +  &
                                                         2.0_dp*t2d(3)*cos1d1 + t2d(1)*cos1d3) +  &
                                  dotkm2*(2.0_dp*cos1d1*cos1d3*t1d1 + cos1d1*cos1d1*t1d3)
              a3d(4)  = a3d(4)  + dotk*t3d(4)  + dotkm1*(2.0_dp*cos2d(2)*t1d2 + cos2d(4)*t1d1 +  &
                                                         2.0_dp*t2d(2)*cos1d2 + t2d(4)*cos1d1) +  &
                                  dotkm2*(2.0_dp*cos1d1*cos1d2*t1d2 + cos1d2*cos1d2*t1d1)
              a3d(5)  = a3d(5)  + dotk*t3d(5)  + dotkm1*(cos2d(2)*t1d3 + cos2d(3)*t1d2 + cos2d(5)*t1d1 +  &
                                                         t2d(2)*cos1d3 + t2d(3)*cos1d2 + t2d(5)*cos1d1) +  &
                                  dotkm2*(cos1d1*cos1d2*t1d3 + cos1d1*cos1d3*t1d2 + cos1d2*cos1d3*t1d1)
              a3d(6)  = a3d(6)  + dotk*t3d(6)  + dotkm1*(2.0_dp*cos2d(3)*t1d3 + cos2d(6)*t1d1 +  &
                                                         2.0_dp*t2d(3)*cos1d3 + t2d(6)*cos1d1) +  &
                                  dotkm2*(2.0_dp*cos1d1*cos1d3*t1d3 + cos1d3*cos1d3*t1d1)
              a3d(7)  = a3d(7)  + dotk*t3d(7)  + 3.0_dp*dotkm1*(cos2d(4)*t1d2 + t2d(4)*cos1d2) +  &
                                  3.0_dp*dotkm2*cos1d2*cos1d2*t1d2
              a3d(8)  = a3d(8)  + dotk*t3d(8)  + dotkm1*(2.0_dp*cos2d(5)*t1d2 + cos2d(4)*t1d3 +  &
                                                         2.0_dp*t2d(5)*cos1d2 + t2d(4)*cos1d3) +  &
                                  dotkm2*(2.0_dp*cos1d2*cos1d3*t1d2 + cos1d2*cos1d3*t1d2)
              a3d(9)  = a3d(9)  + dotk*t3d(9)  + dotkm1*(2.0_dp*cos2d(5)*t1d3 + cos2d(6)*t1d2 +  &
                                                         2.0_dp*t2d(5)*cos1d3 + t2d(6)*cos1d2) +  &
                                  dotkm2*(2.0_dp*cos1d2*cos1d3*t1d3 + cos1d3*cos1d3*t1d2)
              a3d(10) = a3d(10) + dotk*t3d(10) + 3.0_dp*dotkm1*(cos2d(6)*t1d3 + t2d(6)*cos1d3) + &
                                  3.0_dp*dotkm2*cos1d3*cos1d3*t1d3
            endif
          endif
        endif
      endif
!
!  Distance dependent terms
!
      if (ipivot.eq.1) then
        atrm = rkthb*rr23**m
        btrm = rkthb3*rr23**n 
      elseif (ipivot.eq.2) then
        atrm = rkthb*rr13**m
        btrm = rkthb3*rr13**n 
      else
        atrm = rkthb*rr12**m 
        btrm = rkthb3*rr12**n 
      endif
      et1 = atrm - btrm
      ethb = e0*et1
      if (lgrad1) then
!
!  First derivatives w.r.t. distance
!
        atrm = dble(m)*atrm
        btrm = dble(n)*btrm
        if (ipivot.eq.1) then
          d1d1 = 0.0_dp
          d1d2 = 0.0_dp
          d1d3 = - (atrm - btrm)*rr232
        elseif (ipivot.eq.2) then
          d1d1 = 0.0_dp
          d1d2 = - (atrm - btrm)*rr132
          d1d3 = 0.0_dp
        else
          d1d1 = - (atrm - btrm)*rr122
          d1d2 = 0.0_dp
          d1d3 = 0.0_dp
        endif
        if (lgrad2) then
!
!  Second derivatives w.r.t. distance
!
          do mm = 1,6
            d2d(mm) = 0.0_dp
          enddo
          atrm = dble(m+2)*atrm
          btrm = dble(n+2)*btrm
          if (ipivot.eq.1) then
            d2d(6) = (atrm - btrm)*rr232*rr232
          elseif (ipivot.eq.2) then
            d2d(4) = (atrm - btrm)*rr132*rr132
          else
            d2d(1) = (atrm - btrm)*rr122*rr122
          endif
          if (lgrad3) then
!
!  Third derivatives w.r.t. distance
!
            do mm = 1,10
              d3d(mm) = 0.0_dp
            enddo
            atrm = dble(m+4)*atrm
            btrm = dble(n+4)*btrm
            if (ipivot.eq.1) then
              d3d(10) = - (atrm - btrm)*rr232*rr232*rr232
            elseif (ipivot.eq.2) then
              d3d(7) = - (atrm - btrm)*rr132*rr132*rr132
            else
              d3d(1) = - (atrm - btrm)*rr122*rr122*rr122
            endif
          endif
        endif
!
!  Combine derivatives from angle and distance terms
!
!  First derivatives
!
        e1d1 = a1d1*et1 + e0*d1d1
        e1d2 = a1d2*et1 + e0*d1d2
        e1d3 = a1d3*et1 + e0*d1d3
        if (lgrad2) then
!
!  Second derivatives
!
          e2d(1) = a2d(1)*et1 + 2.0_dp*a1d1*d1d1 + e0*d2d(1)
          e2d(2) = a2d(2)*et1 + a1d1*d1d2 + a1d2*d1d1 + e0*d2d(2)
          e2d(3) = a2d(3)*et1 + a1d1*d1d3 + a1d3*d1d1 + e0*d2d(3)
          e2d(4) = a2d(4)*et1 + 2.0_dp*a1d2*d1d2 + e0*d2d(4)
          e2d(5) = a2d(5)*et1 + a1d2*d1d3 + a1d3*d1d2 + e0*d2d(5)
          e2d(6) = a2d(6)*et1 + 2.0_dp*a1d3*d1d3 + e0*d2d(6)
          if (lgrad3) then
!
!  Third derivatives
!
            e3d(1)  = a3d(1)*et1  + 3.0_dp*a2d(1)*d1d1 + 3.0_dp*a1d1*d2d(1) + e0*d3d(1)
            e3d(2)  = a3d(2)*et1  + 2.0_dp*a2d(2)*d1d1 + a2d(1)*d1d2 + 2.0_dp*d2d(2)*a1d1 + d2d(1)*a1d2 + e0*d3d(2)
            e3d(3)  = a3d(3)*et1  + 2.0_dp*a2d(3)*d1d1 + a2d(1)*d1d3 + 2.0_dp*d2d(3)*a1d1 + d2d(1)*a1d3 + e0*d3d(3)
            e3d(4)  = a3d(4)*et1  + 2.0_dp*a2d(2)*d1d2 + a2d(4)*d1d1 + 2.0_dp*d2d(2)*a1d2 + d2d(4)*a1d1 + e0*d3d(4)
            e3d(5)  = a3d(5)*et1  + a2d(2)*d1d3 + a2d(3)*d1d2 + a2d(5)*d1d1 + d2d(2)*a1d3 + d2d(3)*a1d2 + d2d(5)*a1d1 +  &
                      e0*d3d(5)
            e3d(6)  = a3d(6)*et1  + 2.0_dp*a2d(3)*d1d3 + a2d(6)*d1d1 + 2.0_dp*d2d(3)*a1d3 + d2d(6)*a1d1 + e0*d3d(6)
            e3d(7)  = a3d(7)*et1  + 3.0_dp*a2d(4)*d1d2 + 3.0_dp*a1d2*d2d(4) + e0*d3d(7)
            e3d(8)  = a3d(8)*et1  + 2.0_dp*a2d(5)*d1d2 + a2d(4)*d1d3 + 2.0_dp*d2d(5)*a1d2 + d2d(4)*a1d3 + e0*d3d(8)
            e3d(9)  = a3d(9)*et1  + 2.0_dp*a2d(5)*d1d3 + a2d(6)*d1d2 + 2.0_dp*d2d(5)*a1d3 + d2d(6)*a1d2 + e0*d3d(9)
            e3d(10) = a3d(10)*et1 + 3.0_dp*a2d(6)*d1d3 + 3.0_dp*a1d3*d2d(6) + e0*d3d(10)
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.16) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Equatorial ESFF three-body potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    n = nint(theta0)
    rn = dble(n)
    cosntheta = cos(rn*thet)
    rk1 = 2.0_dp*rkthb/rn**2
    if (ipivot.eq.1) then
      rk2 = 2.0_dp*rkthb*exp(-rho1*(r23-rho2))
    elseif (ipivot.eq.2) then
      rk2 = 2.0_dp*rkthb*exp(-rho1*(r13-rho2))
    else
      rk2 = 2.0_dp*rkthb*exp(-rho1*(r12-rho2))
    endif
    ethb = rk1*(1.0_dp - cosntheta) + rk2
    if (lgrad1) then
!
!  First derivatives of energy
!
!  d(cosntheta)/dcostheta = sinntheta/sintheta
!
!  to avoid divide by zero error when sintheta = 0 we use a series
!  expansion in costheta
!
      e1 = - rn*rk1
      e2 = e1
      e3 = e1
      cosd1 = sinratio(n,thet)
      e1 = e1*cosd1
      e1d1 = e1*cos1d1
      e1d2 = e1*cos1d2
      e1d3 = e1*cos1d3
      rk2 = rk2*rho1
      if (ipivot.eq.1) then
        rk2 = rk2*rr23
        e1d3 = e1d3 - rk2
      elseif (ipivot.eq.2) then
        rk2 = rk2*rr13
        e1d2 = e1d2 - rk2
      else
        rk2 = rk2*rr12
        e1d1 = e1d1 - rk2
      endif
      if (lgrad2) then
!
!  Second derivatives of energy
!
        do i1 = 1,6
          e2d(i1) = e1*cos2d(i1)
        enddo
        if (abs(n).gt.1) then
          cosd2 = dsinratio(n,thet)
          e2 = e2*cosd2
          e2d(1) = e2d(1) + e2*cos1d1*cos1d1
          e2d(2) = e2d(2) + e2*cos1d1*cos1d2
          e2d(3) = e2d(3) + e2*cos1d1*cos1d3
          e2d(4) = e2d(4) + e2*cos1d2*cos1d2
          e2d(5) = e2d(5) + e2*cos1d2*cos1d3
          e2d(6) = e2d(6) + e2*cos1d3*cos1d3
        endif
        if (ipivot.eq.1) then
          rk2 = rk2*rr23
          e2d(6) = e2d(6) + rk2*(rho1 + rr23)
        elseif (ipivot.eq.2) then
          rk2 = rk2*rr13
          e2d(4) = e2d(4) + rk2*(rho1 + rr13)
        else
          rk2 = rk2*rr12
          e2d(1) = e2d(1) + rk2*(rho1 + rr12)
        endif
        if (lgrad3) then
!
!  Third derivatives of energy
!
          do i1 = 1,10
            e3d(i1) = e1*cos3d(i1)
          enddo
          if (abs(n).gt.1) then
            cosd3 = dsinratio2(n,thet)
            e3 = e3*cosd3
            e3d(1) = e3d(1) + e3*cos1d1*cos1d1*cos1d1 + 3.0_dp*e2*cos2d(1)*cos1d1
            e3d(2) = e3d(2) + e3*cos1d1*cos1d1*cos1d2 + e2*(2.0_dp*cos2d(2)*cos1d1 + cos2d(1)*cos1d2)
            e3d(3) = e3d(3) + e3*cos1d1*cos1d1*cos1d3 + e2*(2.0_dp*cos2d(3)*cos1d1 + cos2d(1)*cos1d3)
            e3d(4) = e3d(4) + e3*cos1d1*cos1d2*cos1d2 + e2*(2.0_dp*cos2d(2)*cos1d2 + cos2d(4)*cos1d1)
            e3d(5) = e3d(5) + e3*cos1d1*cos1d2*cos1d3 + e2*(cos2d(2)*cos1d3 + cos2d(3)*cos1d2 + cos2d(5)*cos1d1)
            e3d(6) = e3d(6) + e3*cos1d1*cos1d3*cos1d3 + e2*(2.0_dp*cos2d(3)*cos1d3 + cos2d(6)*cos1d1)
            e3d(7) = e3d(7) + e3*cos1d2*cos1d2*cos1d2 + 3.0_dp*e2*cos2d(4)*cos1d2
            e3d(8) = e3d(8) + e3*cos1d2*cos1d2*cos1d3 + e2*(2.0_dp*cos2d(5)*cos1d2 + cos2d(4)*cos1d3)
            e3d(9) = e3d(9) + e3*cos1d2*cos1d3*cos1d3 + e2*(2.0_dp*cos2d(5)*cos1d3 + cos2d(6)*cos1d2)
            e3d(10) = e3d(10) + e3*cos1d3*cos1d3*cos1d3 + 3.0_dp*e2*cos2d(6)*cos1d3
          endif
          if (ipivot.eq.1) then
            e3d(10) = e3d(10) - rk2*rr23*(rho1*rho1 + 3.0_dp*rr23*(rho1 + rr23))
          elseif (ipivot.eq.2) then
            e3d(7)  = e3d(7)  - rk2*rr13*(rho1*rho1 + 3.0_dp*rr13*(rho1 + rr13))
          else
            e3d(1)  = e3d(1)  - rk2*rr12*(rho1*rho1 + 3.0_dp*rr12*(rho1 + rr12))
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.17) then
!$$$$$$$$$$$$$$$$$$$$$$$$$
!  UFF3 three potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$
!
!  Here the coefficients C0, C1 and C2 are passed in as
!  theta0, rkthb3, rkthb4, respectively.
!
    cosntheta = cos(2.0_dp*thet)
    ethb = rkthb*(theta0 + rkthb3*costh + rkthb4*cosntheta)
    if (lgrad1) then
!
!  First derivatives of energy
!
!  d(cosntheta)/dcostheta = sinntheta/sintheta
!
!  to avoid divide by zero error when sintheta = 0 we use a series
!  expansion in costheta
!
      cosd1 = sinratio(2_i4,thet)
      e1 = rkthb*(rkthb3 + 2.0_dp*rkthb4*cosd1)
      e1d1 = e1*cos1d1
      e1d2 = e1*cos1d2
      e1d3 = e1*cos1d3
      if (lgrad2) then
!
!  Second derivatives of energy
!
        do i1 = 1,6
          e2d(i1) = e1*cos2d(i1)
        enddo
        cosd2 = dsinratio(2_i4,thet)
        e2 = 2.0_dp*rkthb*rkthb4*cosd2
        e2d(1) = e2d(1) + e2*cos1d1*cos1d1
        e2d(2) = e2d(2) + e2*cos1d1*cos1d2
        e2d(3) = e2d(3) + e2*cos1d1*cos1d3
        e2d(4) = e2d(4) + e2*cos1d2*cos1d2
        e2d(5) = e2d(5) + e2*cos1d2*cos1d3
        e2d(6) = e2d(6) + e2*cos1d3*cos1d3
        if (lgrad3) then
!
!  Third derivatives of energy
!
          do i1 = 1,10
            e3d(i1) = e1*cos3d(i1)
          enddo
          cosd3 = dsinratio2(2_i4,thet)
          e3 = 2.0_dp*rkthb*rkthb4*cosd3
          e3d(1) = e3d(1) + e3*cos1d1*cos1d1*cos1d1 + 3.0_dp*e2*cos2d(1)*cos1d1
          e3d(2) = e3d(2) + e3*cos1d1*cos1d1*cos1d2 + e2*(2.0_dp*cos2d(2)*cos1d1 + cos2d(1)*cos1d2)
          e3d(3) = e3d(3) + e3*cos1d1*cos1d1*cos1d3 + e2*(2.0_dp*cos2d(3)*cos1d1 + cos2d(1)*cos1d3)
          e3d(4) = e3d(4) + e3*cos1d1*cos1d2*cos1d2 + e2*(2.0_dp*cos2d(2)*cos1d2 + cos2d(4)*cos1d1)
          e3d(5) = e3d(5) + e3*cos1d1*cos1d2*cos1d3 + e2*(cos2d(2)*cos1d3 + cos2d(3)*cos1d2 + cos2d(5)*cos1d1)
          e3d(6) = e3d(6) + e3*cos1d1*cos1d3*cos1d3 + e2*(2.0_dp*cos2d(3)*cos1d3 + cos2d(6)*cos1d1)
          e3d(7) = e3d(7) + e3*cos1d2*cos1d2*cos1d2 + 3.0_dp*e2*cos2d(4)*cos1d2
          e3d(8) = e3d(8) + e3*cos1d2*cos1d2*cos1d3 + e2*(2.0_dp*cos2d(5)*cos1d2 + cos2d(4)*cos1d3)
          e3d(9) = e3d(9) + e3*cos1d2*cos1d3*cos1d3 + e2*(2.0_dp*cos2d(5)*cos1d3 + cos2d(6)*cos1d2)
          e3d(10) = e3d(10) + e3*cos1d3*cos1d3*cos1d3 + 3.0_dp*e2*cos2d(6)*cos1d3
        endif
      endif
    endif
  elseif (n3ty.eq.18) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Bond - angle cosine cross potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    if (ipivot.eq.1) then
      rd1 = r12 - rho1
      rd2 = r13 - rho2
      rktrm = (rkthb*rd1 + rkthb3*rd2)
    elseif (ipivot.eq.2) then
      rd1 = r12 - rho1
      rd2 = r23 - rho3
      rktrm = (rkthb*rd1 + rkthb3*rd2)
    else
      rd1 = r13 - rho2
      rd2 = r23 - rho3
      rktrm = (rkthb*rd1 + rkthb3*rd2)
    endif
    delth = dot - theta0
    ethb = rktrm*delth
    if (lgrad1) then
!
!  First derivatives
!
      if (ipivot.eq.1) then
        e1d1 = rkthb*rr12*delth
        e1d2 = rkthb3*rr13*delth
        e1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        e1d1 = rkthb*rr12*delth
        e1d2 = 0.0_dp
        e1d3 = rkthb3*rr23*delth
      else
        e1d1 = 0.0_dp
        e1d2 = rkthb*rr13*delth
        e1d3 = rkthb3*rr23*delth
      endif
      e1d1 = e1d1 + rktrm*cos1d1
      e1d2 = e1d2 + rktrm*cos1d2
      e1d3 = e1d3 + rktrm*cos1d3
      if (lgrad2) then
!
!  Second derivatives
!
        do m = 1,6
          e2d(m) = rktrm*cos2d(m)
        enddo
        if (ipivot.eq.1) then
          e2d(1) = e2d(1) + rkthb*rr12*(2.0_dp*cos1d1 - rr122*delth)
          e2d(2) = e2d(2) + rkthb*rr12*cos1d2 + rkthb3*rr13*cos1d1
          e2d(3) = e2d(3) + rkthb*rr12*cos1d3
          e2d(4) = e2d(4) + rkthb3*rr13*(2.0_dp*cos1d2 - rr132*delth)
          e2d(5) = e2d(5) + rkthb3*rr13*cos1d3
        elseif (ipivot.eq.2) then
          e2d(1) = e2d(1) + rkthb*rr12*(2.0_dp*cos1d1 - rr122*delth)
          e2d(2) = e2d(2) + rkthb*rr12*cos1d2
          e2d(3) = e2d(3) + rkthb*rr12*cos1d3 + rkthb3*rr23*cos1d1
          e2d(5) = e2d(5) + rkthb3*rr23*cos1d2
          e2d(6) = e2d(6) + rkthb3*rr23*(2.0_dp*cos1d3 - rr232*delth)
        else
          e2d(2) = e2d(2) + rkthb*rr13*cos1d1
          e2d(3) = e2d(3) + rkthb3*rr23*cos1d1
          e2d(4) = e2d(4) + rkthb*rr13*(2.0_dp*cos1d2 - rr132*delth)
          e2d(5) = e2d(5) + rkthb*rr13*cos1d3 + rkthb3*rr23*cos1d2
          e2d(6) = e2d(6) + rkthb3*rr23*(2.0_dp*cos1d3 - rr232*delth)
        endif
        if (lgrad3) then
!
!  Third derivatives
!
          do m = 1,10
            e3d(m) = rktrm*cos3d(m)
          enddo
          if (ipivot.eq.1) then
            e3d(1) = e3d(1) + 3.0_dp*rkthb*rr12*(rr122*(rr122*delth - cos1d1)+cos2d(1))
            e3d(2) = e3d(2) + 2.0_dp*rkthb*rr12*cos2d(2) + rkthb3*rr13*cos2d(1) - rkthb*rr122*rr12*cos1d2
            e3d(3) = e3d(3) + 2.0_dp*rkthb*rr12*cos2d(3) - rkthb*rr122*rr12*cos1d3
            e3d(4) = e3d(4) + 2.0_dp*rkthb3*rr13*cos2d(2) + rkthb*rr12*cos2d(4) - rkthb3*rr132*rr13*cos1d1
            e3d(5) = e3d(5) + rkthb*rr12*cos2d(5) + rkthb3*rr13*cos2d(3)
            e3d(6) = e3d(6) + rkthb*rr12*cos2d(6)
            e3d(7) = e3d(7) + 3.0_dp*rkthb3*rr13*(rr132*(rr132*delth - cos1d2) + cos2d(4))
            e3d(8) = e3d(8) + 2.0_dp*rkthb3*rr13*cos2d(5) - rkthb3*rr132*rr13*cos1d3
            e3d(9) = e3d(9) + rkthb3*rr13*cos2d(6)
          elseif (ipivot.eq.2) then
            e3d(1) = e3d(1) + 3.0_dp*rkthb*rr12*(rr122*(rr122*delth - cos1d1)+cos2d(1))
            e3d(2) = e3d(2) + 2.0_dp*rkthb*rr12*cos2d(2) - rkthb*rr122*rr12*cos1d2
            e3d(3) = e3d(3) + 2.0_dp*rkthb*rr12*cos2d(3) + rkthb3*rr23*cos2d(1) - rkthb*rr122*rr12*cos1d3
            e3d(4) = e3d(4) + rkthb*rr12*cos2d(4)
            e3d(5) = e3d(5) + rkthb*rr12*cos2d(5) + rkthb3*rr23*cos2d(2)
            e3d(6) = e3d(6) + 2.0_dp*rkthb3*rr23*cos2d(3) + rkthb*rr12*cos2d(6) - rkthb3*rr232*rr23*cos1d1
            e3d(8) = e3d(8) + rkthb3*rr23*cos2d(4)
            e3d(9) = e3d(9) + 2.0_dp*rkthb3*rr23*cos2d(5) - rkthb3*rr232*rr23*cos1d2
            e3d(10) = e3d(10) + 3.0_dp*rkthb3*rr23*(rr232*(rr232*delth - cos1d3)+cos2d(6))
          else
            e3d(2) = e3d(2) + rkthb*rr13*cos2d(1)
            e3d(3) = e3d(3) + rkthb3*rr23*cos2d(1)
            e3d(4) = e3d(4) + 2.0_dp*rkthb*rr13*cos2d(2) - rkthb*rr132*rr13*cos1d1
            e3d(5) = e3d(5) + rkthb*rr13*cos2d(3) + rkthb3*rr23*cos2d(2)
            e3d(6) = e3d(6) + 2.0_dp*rkthb3*rr23*cos2d(3) - rkthb3*rr232*rr23*cos1d1
            e3d(7) = e3d(7) + 3.0_dp*rkthb*rr13*(rr132*(rr132*delth - cos1d2)+cos2d(4))
            e3d(8) = e3d(8) + 2.0_dp*rkthb*rr13*cos2d(5) + rkthb3*rr23*cos2d(4) - rkthb*rr132*rr13*cos1d3
            e3d(9) = e3d(9) + 2.0_dp*rkthb3*rr23*cos2d(5) + rkthb*rr13*cos2d(6) - rkthb3*rr232*rr23*cos1d2
            e3d(10) = e3d(10) + 3.0_dp*rkthb3*rr23*(rr232*(rr232*delth - cos1d3)+cos2d(6))
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.19) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Three-body Coulomb subtraction  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    rktrm = rkthb*angstoev
    if (ipivot.eq.1) then
      ethb =  - rktrm*rr23
    elseif (ipivot.eq.2) then
      ethb = - rktrm*rr13
    else
      ethb = - rktrm*rr12
    endif
    if (lgrad1) then
!
!  First derivatives
!
      e1d1 = 0.0_dp
      e1d2 = 0.0_dp
      e1d3 = 0.0_dp
      if (ipivot.eq.1) then
        e1d3 = rktrm*rr23*rr232
      elseif (ipivot.eq.2) then
        e1d2 = rktrm*rr13*rr132
      else
        e1d1 = rktrm*rr12*rr122
      endif
      if (lgrad2) then
!
!  Second derivatives
!
        do mm = 1,6
          e2d(mm) = 0.0_dp
        enddo
        if (ipivot.eq.1) then
          e2d(6) = - 3.0_dp*rr232*e1d3
        elseif (ipivot.eq.2) then
          e2d(4) = - 3.0_dp*rr132*e1d2
        else
          e2d(1) = - 3.0_dp*rr122*e1d1
        endif
        if (lgrad3) then
!
!  Third derivatives
!
          do mm = 1,10
            e3d(mm) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            e3d(10) =  - 5.0_dp*rr232*e2d(6)
          elseif (ipivot.eq.2) then
            e3d(7)  =  - 5.0_dp*rr132*e2d(4)
          else
            e3d(1)  =  - 5.0_dp*rr122*e2d(1)
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.20) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Double exponential 3 - body  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    b12 = rho1
    r120 = rho4
    b13 = rho2
    r130 = rho5
    e0 = rkthb*exp(-b12*(r12-r120))*exp(-b13*(r13-r130))
    ethb = e0
    if (lgrad1) then
!
!  First derivatives
!
      t12 = b12*rr12
      t13 = b13*rr13
      e1d1 = - t12*e0
      e1d2 = - t13*e0
      e1d3 = 0.0_dp
      if (lgrad2) then
!
!  Second derivatives
!
        e2d(1) = t12*t12*e0 + t12*rr12*rr12*e0
        e2d(2) = t12*t13*e0
        e2d(3) = 0.0_dp
        e2d(4) = t13*t13*e0 + t13*rr13*rr13*e0
        e2d(5) = 0.0_dp
        e2d(6) = 0.0_dp
        if (lgrad3) then
!
!  Third derivatives
!
          e3d(1) =  - t12*t12*t12*e0 - 3.0_dp*rr12*rr12*e2d(1)
          e3d(2) =  - t12*t12*t13*e0 - t12*t12*rr12*rr13*e0
          e3d(3) =  0.0_dp
          e3d(4) =  - t12*t13*t13*e0 - t13*t13*rr13*rr12*e0
          e3d(5) = 0.0_dp
          e3d(6) = 0.0_dp
          e3d(7) =  - t13*t13*t13*e0 - 3.0_dp*rr13*rr13*e2d(4)
          e3d(8) = 0.0_dp
          e3d(9) = 0.0_dp
          e3d(10)= 0.0_dp
        endif
      endif
    endif
  elseif (n3ty.eq.21) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Three-body gCoulomb subtraction  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    rktrm = rkthb*angstoev
! theta0 is gamma
    if (ipivot.eq.1) then
      trm0 = 1.0_dp/(r232*r23 + theta0)
    elseif (ipivot.eq.2) then
      trm0 = 1.0_dp/(r132*r13 + theta0)
    else
      trm0 = 1.0_dp/(r122*r12 + theta0)
    endif
    rtrm0 = rktrm*(trm0**third)
    ethb  = - rtrm0
    if (lgrad1) then
!
!  First derivatives
!
      e1d1 = 0.0_dp
      e1d2 = 0.0_dp
      e1d3 = 0.0_dp
      if (ipivot.eq.1) then
        trm1 = rtrm0*trm0*r23
        e1d3 = trm1
      elseif (ipivot.eq.2) then
        trm1 = rtrm0*trm0*r13
        e1d2 = trm1
      else
        trm1 = rtrm0*trm0*r12
        e1d1 = trm1
      endif
      if (lgrad2) then
!
!  Second derivatives
!
        do mm = 1,6
          e2d(mm) = 0.0_dp
        enddo
        if (ipivot.eq.1) then
          trm2 = trm1*trm0*r23
          e2d(6) = - 4.0_dp*trm2 + trm1*rr232
        elseif (ipivot.eq.2) then
          trm2 = trm1*trm0*r13
          e2d(4) = - 4.0_dp*trm2 + trm1*rr132
        else
          trm2 = trm1*trm0*r12
          e2d(1) = - 4.0_dp*trm2 + trm1*rr122
        endif
        if (lgrad3) then
!
!  Third derivatives
!
          do mm = 1,10
            e3d(mm) = 0.0_dp
          enddo
          if (ipivot.eq.1) then
            trm3 = trm2*trm0*r23
            e3d(10) = 28.0_dp*trm3 - 12.0_dp*trm2*rr232 - trm1*rr234
          elseif (ipivot.eq.2) then
            trm3 = trm2*trm0*r13
            e3d(7)  = 28.0_dp*trm3 - 12.0_dp*trm2*rr132 - trm1*rr134
          else
            trm3 = trm2*trm0*r12
            e3d(1)  = 28.0_dp*trm3 - 12.0_dp*trm2*rr122 - trm1*rr124
          endif
        endif
      endif
    endif
  elseif (n3ty.eq.22) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  Bond - angle general cross potential  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    pr1 = nint(threepoly(1,nptr3))            ! Power of distance 1
    pr2 = nint(threepoly(2,nptr3))            ! Power of distance 2
    pt  = nint(threepoly(3,nptr3))            ! Power of theta
    if (ipivot.eq.1) then
      rd1 = r12 - rho1
      rd2 = r13 - rho2
    elseif (ipivot.eq.2) then
      rd1 = r12 - rho1
      rd2 = r23 - rho3
    else
      rd1 = r13 - rho2
      rd2 = r23 - rho3
    endif
    prd1 = rd1**pr1
    prd2 = rd2**pr2
    rktrm = rkthb*prd1*prd2
    delth = thet - theta0
    ptdelth = delth**pt
    ethb = rktrm*ptdelth
    if (lgrad1) then
!
!  First derivatives
!
      if (ipivot.eq.1) then
        if (pr1.ge.1) then
          prd1m1 = rr12*dble(pr1)*rd1**(pr1-1)
        else
          prd1m1 = 0.0_dp
        endif
        if (pr2.ge.1) then
          prd2m1 = rr13*dble(pr2)*rd2**(pr2-1)
        else
          prd2m1 = 0.0_dp
        endif
        e1d1 = rkthb*prd1m1*prd2*ptdelth
        e1d2 = rkthb*prd2m1*prd1*ptdelth
        e1d3 = 0.0_dp
      elseif (ipivot.eq.2) then
        if (pr1.ge.1) then
          prd1m1 = rr12*dble(pr1)*rd1**(pr1-1)
        else
          prd1m1 = 0.0_dp
        endif
        if (pr2.ge.1) then
          prd2m1 = rr23*dble(pr2)*rd2**(pr2-1)
        else
          prd2m1 = 0.0_dp
        endif
        e1d1 = rkthb*prd1m1*prd2*ptdelth
        e1d2 = 0.0_dp
        e1d3 = rkthb*prd2m1*prd1*ptdelth
      else
        if (pr1.ge.1) then
          prd1m1 = rr13*dble(pr1)*rd1**(pr1-1)
        else
          prd1m1 = 0.0_dp
        endif
        if (pr2.ge.1) then
          prd2m1 = rr23*dble(pr2)*rd2**(pr2-1)
        else
          prd2m1 = 0.0_dp
        endif
        e1d1 = 0.0_dp
        e1d2 = rkthb*prd1m1*prd2*ptdelth
        e1d3 = rkthb*prd2m1*prd1*ptdelth
      endif
      if (pt.ge.2) then
        delthm1 = dble(pt)*delth**(pt-1)
      elseif (pt.eq.1) then
        delthm1 = 1.0_dp
      else
        delthm1 = 0.0_dp
      endif
      e1d1 = e1d1 + delthm1*rktrm*the1d1
      e1d2 = e1d2 + delthm1*rktrm*the1d2
      e1d3 = e1d3 + delthm1*rktrm*the1d3
      if (lgrad2) then
!
!  Second derivatives
!
        if (pt.ge.2) then
          delthm2 = dble(pt*(pt-1))*delth**(pt-2)
        else
          delthm2 = 0.0_dp
        endif
        e2d(1) = rktrm*(delthm1*the2d(1) + delthm2*the1d1*the1d1)
        e2d(2) = rktrm*(delthm1*the2d(2) + delthm2*the1d1*the1d2)
        e2d(3) = rktrm*(delthm1*the2d(3) + delthm2*the1d1*the1d3)
        e2d(4) = rktrm*(delthm1*the2d(4) + delthm2*the1d2*the1d2)
        e2d(5) = rktrm*(delthm1*the2d(5) + delthm2*the1d2*the1d3)
        e2d(6) = rktrm*(delthm1*the2d(6) + delthm2*the1d3*the1d3)
        if (ipivot.eq.1) then
          if (pr1.ge.2) then
            prd1m2 = rr122*(dble(pr1*(pr1-1))*rd1**(pr1-2) - prd1m1)
          elseif (pr1.eq.1) then
            prd1m2 = - rr122*prd1m1
          else
            prd1m2 = 0.0_dp
          endif
          if (pr2.ge.2) then
            prd2m2 = rr132*(dble(pr2*(pr2-1))*rd2**(pr2-2) - prd2m1)
          elseif (pr2.eq.1) then
            prd2m2 = - rr132*prd2m1
          else
            prd2m2 = 0.0_dp
          endif
          e2d(1) = e2d(1) + rkthb*(2.0_dp*delthm1*the1d1*prd1m1 + ptdelth*prd1m2)*prd2
          e2d(2) = e2d(2) + rkthb*(delthm1*(the1d1*prd2m1*prd1 + the1d2*prd1m1*prd2) + ptdelth*prd1m1*prd2m1)
          e2d(3) = e2d(3) + rkthb*(delthm1*the1d3*prd1m1*prd2)
          e2d(4) = e2d(4) + rkthb*(2.0_dp*delthm1*the1d2*prd2m1 + ptdelth*prd2m2)*prd1
          e2d(5) = e2d(5) + rkthb*(delthm1*the1d3*prd2m1*prd1)
        elseif (ipivot.eq.2) then
          if (pr1.ge.2) then
            prd1m2 = rr122*(dble(pr1*(pr1-1))*rd1**(pr1-2) - prd1m1)
          elseif (pr1.eq.1) then
            prd1m2 = - rr122*prd1m1
          else
            prd1m2 = 0.0_dp
          endif
          if (pr2.ge.2) then
            prd2m2 = rr232*(dble(pr2*(pr2-1))*rd2**(pr2-2) - prd2m1)
          elseif (pr2.eq.1) then
            prd2m2 = - rr232*prd2m1
          else
            prd2m2 = 0.0_dp
          endif
          e2d(1) = e2d(1) + rkthb*(2.0_dp*delthm1*the1d1*prd1m1 + ptdelth*prd1m2)*prd2
          e2d(2) = e2d(2) + rkthb*(delthm1*the1d2*prd1m1*prd2)
          e2d(3) = e2d(3) + rkthb*(delthm1*(the1d1*prd2m1*prd1 + the1d3*prd1m1*prd2) + ptdelth*prd1m1*prd2m1)
          e2d(5) = e2d(5) + rkthb*(delthm1*the1d2*prd2m1*prd1)
          e2d(6) = e2d(6) + rkthb*(2.0_dp*delthm1*the1d3*prd2m1 + ptdelth*prd2m2)*prd1
        else
          if (pr1.ge.2) then
            prd1m2 = rr132*(dble(pr1*(pr1-1))*rd1**(pr1-2) - prd1m1)
          elseif (pr1.eq.1) then
            prd1m2 = - rr132*prd1m1
          else
            prd1m2 = 0.0_dp
          endif
          if (pr2.ge.2) then
            prd2m2 = rr232*(dble(pr2*(pr2-1))*rd2**(pr2-2) - prd2m1)
          elseif (pr2.eq.1) then
            prd2m2 = - rr232*prd2m1
          else
            prd2m2 = 0.0_dp
          endif
          e2d(2) = e2d(2) + rkthb*(delthm1*the1d1*prd1m1*prd2)
          e2d(3) = e2d(3) + rkthb*(delthm1*the1d1*prd2m1*prd1)
          e2d(4) = e2d(4) + rkthb*(2.0_dp*delthm1*the1d2*prd1m1 + ptdelth*prd1m2)*prd2
          e2d(5) = e2d(5) + rkthb*(delthm1*(the1d2*prd2m1*prd1 + the1d3*prd1m1*prd2) + ptdelth*prd1m1*prd2m1)
          e2d(6) = e2d(6) + rkthb*(2.0_dp*delthm1*the1d3*prd2m1 + ptdelth*prd2m2)*prd1
        endif
        if (lgrad3) then
!
!  Third derivatives
!
! DEBUG
          call outerror('third derivatives not finished for bagcross potential',0_i4)
          call stopnow('threebody')
! DEBUG
          if (pt.ge.3) then
            delthm3 = dble(pt*(pt-1)*(pt-2))*delth**(pt-3)
          else
            delthm3 = 0.0_dp
          endif
          e3d(1)  = rktrm*(delthm1*the3d(1)  + &
            delthm2*3.0_dp*the2d(1)*the1d1 + delthm3*the1d1*the1d1*the1d1)
          e3d(2)  = rktrm*(delthm1*the3d(2)  + &
            delthm2*(2.0_dp*the2d(2)*the1d1 + the2d(1)*the1d2) + delthm3*the1d1*the1d1*the1d2)
          e3d(3)  = rktrm*(delthm1*the3d(3)  + &
            delthm2*(2.0_dp*the2d(3)*the1d1 + the2d(1)*the1d3) + delthm3*the1d1*the1d1*the1d3)
          e3d(4)  = rktrm*(delthm1*the3d(4)  + &
            delthm2*(2.0_dp*the2d(4)*the1d1 + the2d(4)*the1d1) + delthm3*the1d1*the1d2*the1d2)
          e3d(5)  = rktrm*(delthm1*the3d(5)  + &
            delthm2*(the2d(5)*the1d1+the2d(3)*the1d2+the2d(2)*the1d3) + delthm3*the1d1*the1d2*the1d3)
          e3d(6)  = rktrm*(delthm1*the3d(6)  + &
            delthm2*(2.0_dp*the2d(3)*the1d3 + the2d(6)*the1d1) + delthm3*the1d1*the1d3*the1d3)
          e3d(7)  = rktrm*(delthm1*the3d(7) + &
            delthm2*3.0_dp*the2d(4)*the1d2 + delthm3*the1d2*the1d2*the1d2)
          e3d(8)  = rktrm*(delthm1*the3d(8) + &
            delthm2*(2.0_dp*the2d(5)*the1d2 + the2d(4)*the1d3) + delthm3*the1d2*the1d2*the1d3)
          e3d(9)  = rktrm*(delthm1*the3d(9) + &
            delthm2*(2.0_dp*the2d(5)*the1d3 + the2d(6)*the1d2) + delthm3*the1d2*the1d3*the1d3)
          e3d(10) = rktrm*(delthm1*the3d(10) + &
            delthm2*3.0_dp*the2d(6)*the1d3 + delthm3*the1d3*the1d3*the1d3)
!
          if (ipivot.eq.1) then
            if (pr1.ge.3) then
              prd1m2 = rr122*(dble(pr1*(pr1-1)*(pr1-2))*rd1**(pr1-3) - prd1m1)
            elseif (pr1.eq.1) then
              prd1m2 = - rr122*prd1m1
            else
              prd1m2 = 0.0_dp
            endif
          elseif (ipivot.eq.2) then
          else
          endif
        endif
      endif
    endif
  endif
!
  return
  end
