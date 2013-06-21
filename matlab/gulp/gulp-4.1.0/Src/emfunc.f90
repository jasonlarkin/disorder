  subroutine emfunc(lm,u,x,signx,y,z,a,emf,d1emf,d2emf,d1emfs, &
    d2emfs,d2emfm,d3emf,d3emfm,lgrad1,lgrad2,lgrad3)
!
!  Calculates the Euler-Maclaurin integrals required by the 1-D Coulomb sum.
!
!   9/01 Created
!  10/01 Derivatives added
!  10/01 Derivative algorithm simplified using recursive relationship of
!        W terms w.r.t. differentation
!  12/01 First strain derivative added
!  12/01 Second derivatives added
!   5/02 Second strain derivatives added
!   5/02 Third derivatives added
!   7/05 Style updated
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: lm
  real(dp),    intent(in)  :: a
  real(dp),    intent(out) :: d1emf(3)
  real(dp),    intent(out) :: d2emf(6)
  real(dp),    intent(out) :: d2emfm(3)
  real(dp),    intent(out) :: d3emf(10)
  real(dp),    intent(out) :: d3emfm(6)
  real(dp),    intent(out) :: d1emfs
  real(dp),    intent(out) :: d2emfs
  real(dp),    intent(out) :: emf
  real(dp),    intent(in)  :: signx
  real(dp),    intent(in)  :: u
  real(dp),    intent(in)  :: x
  real(dp),    intent(in)  :: y
  real(dp),    intent(in)  :: z
  logical,     intent(in)  :: lgrad1
  logical,     intent(in)  :: lgrad2
  logical,     intent(in)  :: lgrad3
!
!  Local variables
!
  integer  :: i
  integer  :: ii
  integer  :: maxlmorder
  integer  :: maxlmorderd1
  integer  :: maxlmorderd2
  real(dp) :: alpha
  real(dp) :: ap
  real(dp) :: dwde
  real(dp) :: d2wde2
  real(dp) :: g
  real(dp) :: d1g(3)
  real(dp) :: d2g(6)
  real(dp) :: ecoeff(5)
  real(dp) :: gtrm1
  real(dp) :: gtrm2
  real(dp) :: raptrm
  real(dp) :: trm1s
  real(dp) :: ux
  real(dp) :: w(0:11)
  real(dp) :: dw(3,0:11)
  real(dp) :: d2w(6,0:11)
  real(dp) :: d3w(10,0:11)
  real(dp) :: d3ws(6,0:11)
!
  if (lm.gt.5) then
    call outerror(' Order of Euler-MacLaurin expansion exceeds 5!',0_i4)
    call stopnow('emfunc')
  endif
!
  ecoeff(1) = -0.5d0/12.0d0
  ecoeff(2) =  0.875d0/720.0d0
  ecoeff(3) = -0.96875d0/30240.0d0
  ecoeff(4) =  0.9921875d0/1209600.0d0
  ecoeff(5) = -0.998046875d0/47900160.0d0
!
  alpha = y*y + z*z
  ux = u + x
!
!  Set order of W to calculate to - increase by 1 for each level of derivatives
!
  maxlmorder = 2*lm - 1
  maxlmorderd1 = 2*lm - 1
  maxlmorderd2 = 2*lm - 1
  if (lgrad1) then
    maxlmorder = maxlmorder + 1
    if (lgrad2) then
      maxlmorder = maxlmorder + 1
      maxlmorderd1 = maxlmorderd1 + 1
      if (lgrad3) then
        maxlmorder = maxlmorder + 1
        maxlmorderd1 = maxlmorderd1 + 1
        maxlmorderd2 = maxlmorderd2 + 1
      endif
    endif
  endif
!
  g = (ux*ux+alpha)
  raptrm = 1.0_dp/g
  w(0) = sqrt(raptrm)
  w(1) = -ux*w(0)*raptrm
  do i = 2,maxlmorder
    w(i) = (-dble(2*i-1)*ux*w(i-1) - dble((i-1)**2)*w(i-2))*raptrm
  enddo
!
!  Function
!
  emf = 0.0_dp
  ap = a
  do i = 1,lm
    emf = emf - ecoeff(i)*ap*w(2*i-1)
    ap = ap*a*a
  enddo
!
!  Derivatives
!
  if (lgrad1) then
!
!  Cartesian derivatives of W
!
    do i = 0,maxlmorderd1
      dw(1,i) = w(i+1)*signx
    enddo
    dw(2,0) = - w(0)*raptrm*y
    dw(3,0) = - w(0)*raptrm*z
    dw(2,1) = -(2.0_dp*w(1)*y + ux*dw(2,0))*raptrm
    dw(3,1) = -(2.0_dp*w(1)*z + ux*dw(3,0))*raptrm
    do i = 2,maxlmorderd1
      dw(2,i) = - (2.0_dp*w(i)*y + (dble(2*i-1)*ux*dw(2,i-1) + dble((i-1)**2)*dw(2,i-2)))*raptrm
      dw(3,i) = - (2.0_dp*w(i)*z + (dble(2*i-1)*ux*dw(3,i-1) + dble((i-1)**2)*dw(3,i-2)))*raptrm
    enddo
    if (lgrad2) then
!
!  Cartesian - cartesian derivatives of W
!
      d2w(1,0) = w(2)
      d2w(2,0) = w(0)*raptrm*(3.0_dp*y*y*raptrm - 1.0d0)
      d2w(3,0) = w(0)*raptrm*(3.0_dp*z*z*raptrm - 1.0d0)
      d2w(4,0) = w(0)*raptrm*3.0_dp*y*z*raptrm
      d2w(5,0) = - w(1)*signx*raptrm*z
      d2w(6,0) = - w(1)*signx*raptrm*y
      d2w(1,1) = w(3)
      d2w(2,1) = - raptrm*(4.0_dp*raptrm*y*dw(2,0) + ux*d2w(2,0) + 2.0_dp*w(1))
      d2w(3,1) = - raptrm*(4.0_dp*raptrm*z*dw(3,0) + ux*d2w(3,0) + 2.0_dp*w(1))
      d2w(4,1) = - raptrm*(4.0_dp*raptrm*z*dw(2,0) + ux*d2w(4,0))
      d2w(5,1) = -(2.0_dp*w(2)*z*signx + signx*dw(3,0) + ux*d2w(5,0))*raptrm
      d2w(6,1) = -(2.0_dp*w(2)*y*signx + signx*dw(2,0) + ux*d2w(6,0))*raptrm
      do i = 2,maxlmorderd2
        d2w(1,i) = w(i+2)
        d2w(2,i) = - 4.0_dp*raptrm*y*dw(2,i) - (dble(2*i-1)*ux*d2w(2,i-1) + dble((i-1)**2)*d2w(2,i-2) + 2.0*w(i))*raptrm
        d2w(3,i) = - 4.0_dp*raptrm*z*dw(3,i) - (dble(2*i-1)*ux*d2w(3,i-1) + dble((i-1)**2)*d2w(3,i-2) + 2.0*w(i))*raptrm
        d2w(4,i) = - 4.0_dp*raptrm*z*dw(2,i) - (dble(2*i-1)*ux*d2w(4,i-1) + dble((i-1)**2)*d2w(4,i-2))*raptrm
        d2w(5,i) = - (2.0_dp*w(i+1)*z*signx + (dble(2*i-1)*ux*d2w(5,i-1) +  &
          signx*dble(2*i-1)*dw(3,i-1) + dble((i-1)**2)*d2w(5,i-2)))*raptrm
        d2w(6,i) = - (2.0_dp*w(i+1)*y*signx + (dble(2*i-1)*ux*d2w(6,i-1) +  &
          signx*dble(2*i-1)*dw(2,i-1) + dble((i-1)**2)*d2w(6,i-2)))*raptrm
      enddo
      if (lgrad3) then
        d1g(1) = 2.0_dp*ux*signx
        d1g(2) = 2.0_dp*y 
        d1g(3) = 2.0_dp*z 
        d2g(1) = 2.0_dp
        d2g(2) = 2.0_dp
        d2g(3) = 2.0_dp
        d2g(4) = 0.0_dp
        d2g(5) = 0.0_dp
        d2g(6) = 0.0_dp
        gtrm1 = 0.75_dp*raptrm*raptrm*w(0)
        gtrm2 = - 2.5_dp*gtrm1*raptrm
!
!  Cartesian - cartesian - cartesian derivatives of W
!
        d3w(1,0) = gtrm2*d1g(1)*d1g(1)*d1g(1) + gtrm1*(3.0_dp*d2g(1)*d1g(1))
        d3w(2,0) = gtrm2*d1g(2)*d1g(1)*d1g(1) + gtrm1*(2.0_dp*d2g(6)*d1g(1) + d2g(1)*d1g(2))
        d3w(3,0) = gtrm2*d1g(3)*d1g(1)*d1g(1) + gtrm1*(2.0_dp*d2g(5)*d1g(1) + d2g(1)*d1g(3))
        d3w(4,0) = gtrm2*d1g(2)*d1g(2)*d1g(1) + gtrm1*(2.0_dp*d2g(6)*d1g(2) + d2g(2)*d1g(1))
        d3w(5,0) = gtrm2*d1g(3)*d1g(2)*d1g(1) + gtrm1*(d2g(4)*d1g(1) + d2g(5)*d1g(2) + d2g(6)*d1g(3))
        d3w(6,0) = gtrm2*d1g(3)*d1g(3)*d1g(1) + gtrm1*(2.0_dp*d2g(5)*d1g(1) + d2g(3)*d1g(1))
        d3w(7,0) = gtrm2*d1g(2)*d1g(2)*d1g(2) + gtrm1*(3.0_dp*d2g(2)*d1g(2))
        d3w(8,0) = gtrm2*d1g(3)*d1g(2)*d1g(2) + gtrm1*(2.0_dp*d2g(4)*d1g(2) + d2g(2)*d1g(3))
        d3w(9,0) = gtrm2*d1g(3)*d1g(3)*d1g(2) + gtrm1*(2.0_dp*d2g(4)*d1g(3) + d2g(3)*d1g(2))
        d3w(10,0)= gtrm2*d1g(3)*d1g(3)*d1g(3) + gtrm1*(3.0_dp*d2g(3)*d1g(3))
!
        d3w(1,1) = -6.0_dp*ux*(dw(1,0)*dw(1,0)*dw(1,0) + w(0)*(3.0_dp*d2w(1,0)*dw(1,0) +  &
          0.5_dp*w(0)*d3w(1,0))) - 3.0_dp*signx*w(0)*(6.0_dp*dw(1,0)*dw(1,0) + w(0)*(3.0_dp*d2w(1,0)))
        d3w(2,1) = -6.0_dp*ux*(dw(2,0)*dw(1,0)*dw(1,0) + w(0)*(2.0_dp*d2w(6,0)*dw(1,0) + d2w(1,0)*dw(2,0) + &
          0.5_dp*w(0)*d3w(2,0))) - 6.0_dp*signx*w(0)*(2.0_dp*dw(2,0)*dw(1,0) + w(0)*d2w(6,0))
        d3w(3,1) = -6.0_dp*ux*(dw(3,0)*dw(1,0)*dw(1,0) + w(0)*(2.0_dp*d2w(5,0)*dw(1,0) + d2w(1,0)*dw(3,0) + &
          0.5_dp*w(0)*d3w(3,0))) - 6.0_dp*signx*w(0)*(2.0_dp*dw(3,0)*dw(1,0) + w(0)*d2w(5,0))
        d3w(4,1) = -6.0_dp*ux*(dw(2,0)*dw(2,0)*dw(1,0) + w(0)*(2.0_dp*d2w(6,0)*dw(2,0) + d2w(2,0)*dw(1,0) + &
          0.5_dp*w(0)*d3w(4,0))) - 6.0_dp*signx*w(0)*(dw(2,0)*dw(2,0) + 0.5_dp*w(0)*d2w(2,0))
        d3w(5,1) = -6.0_dp*ux*(dw(3,0)*dw(2,0)*dw(1,0) + w(0)*(d2w(4,0)*dw(1,0) + d2w(5,0)*dw(2,0) + &
          d2w(6,0)*dw(3,0) + 0.5_dp*w(0)*d3w(5,0))) - 6.0_dp*signx*w(0)*(dw(3,0)*dw(2,0) + 0.5_dp*w(0)*d2w(4,0))
        d3w(6,1) = -6.0_dp*ux*(dw(3,0)*dw(3,0)*dw(1,0) + w(0)*(2.0_dp*d2w(5,0)*dw(3,0) + d2w(3,0)*dw(1,0) + &
          0.5_dp*w(0)*d3w(6,0))) - 6.0_dp*signx*w(0)*(dw(3,0)*dw(3,0) + 0.5_dp*w(0)*d2w(3,0))
        d3w(7,1) = -6.0_dp*ux*(dw(2,0)*dw(2,0)*dw(2,0) + w(0)*(3.0_dp*d2w(2,0)*dw(2,0) + 0.5_dp*w(0)*d3w(7,0)))
        d3w(8,1) = -6.0_dp*ux*(dw(3,0)*dw(2,0)*dw(2,0) + w(0)*(2.0_dp*d2w(5,0)*dw(2,0) + d2w(2,0)*dw(3,0) + &
          0.5_dp*w(0)*d3w(8,0)))
        d3w(9,1) = -6.0_dp*ux*(dw(3,0)*dw(3,0)*dw(2,0) + w(0)*(2.0_dp*d2w(5,0)*dw(3,0) + d2w(3,0)*dw(2,0) + &
          0.5_dp*w(0)*d3w(9,0)))
        d3w(10,1) = -6.0_dp*ux*(dw(3,0)*dw(3,0)*dw(3,0) + w(0)*(3.0_dp*d2w(3,0)*dw(3,0) + 0.5_dp*w(0)*d3w(10,0)))
!
        do i = 2,2*lm-1
          d3w(1,i) = - raptrm*(3.0_dp*(d2w(1,i)*d1g(1) + dw(1,i)*d2g(1)) + &
            dble(2*i-1)*(signx*(3.0_dp*d2w(1,i-1)) + ux*d3w(1,i-1)) + dble((i-1)**2)*d3w(1,i-2))
          d3w(2,i) = - raptrm*( &
            2.0_dp*d2w(6,i)*d1g(1) + d2w(1,i)*d1g(2) + 2.0_dp*dw(1,i)*d2g(6) + dw(2,i)*d2g(1) + &
            dble(2*i-1)*(signx*(2.0_dp*d2w(6,i-1)) + ux*d3w(2,i-1)) + dble((i-1)**2)*d3w(2,i-2))
          d3w(3,i) = - raptrm*(2.0_dp*d2w(5,i)*d1g(1) + d2w(1,i)*d1g(3) + &
            2.0_dp*dw(1,i)*d2g(5) + dw(3,i)*d2g(1) + dble(2*i-1)*(signx*(2.0_dp*d2w(5,i-1)) +  &
            ux*d3w(3,i-1)) + dble((i-1)**2)*d3w(3,i-2))
          d3w(4,i) = - raptrm*(2.0_dp*d2w(6,i)*d1g(2) + d2w(2,i)*d1g(1) + 2.0_dp*dw(2,i)*d2g(6) + dw(1,i)*d2g(2) + &
            dble(2*i-1)*(signx*(d2w(2,i-1)) + ux*d3w(4,i-1)) + dble((i-1)**2)*d3w(4,i-2))
          d3w(5,i) = - raptrm*(d2w(4,i)*d1g(1) + d2w(5,i)*d1g(2) + d2w(6,i)*d1g(3) + dw(1,i)*d2g(4) +  &
            dw(2,i)*d2g(5) + dw(3,i)*d2g(6) + dble(2*i-1)*(signx*(d2w(4,i-1)) + ux*d3w(5,i-1)) + dble((i-1)**2)*d3w(5,i-2))
          d3w(6,i) = - raptrm*(2.0_dp*d2w(5,i)*d1g(3) + d2w(3,i)*d1g(1) + 2.0_dp*dw(3,i)*d2g(5) + dw(1,i)*d2g(3) + &
            dble(2*i-1)*(signx*(d2w(3,i-1)) + ux*d3w(6,i-1)) + dble((i-1)**2)*d3w(6,i-2))
          d3w(7,i) = - raptrm*(3.0_dp*(d2w(2,i)*d1g(2) + dw(2,i)*d2g(2)) + &
            dble(2*i-1)*(ux*d3w(7,i-1)) + dble((i-1)**2)*d3w(7,i-2))
          d3w(8,i) = - raptrm*(2.0_dp*d2w(4,i)*d1g(2) + d2w(2,i)*d1g(3) + &
            2.0_dp*dw(2,i)*d2g(4) + dw(3,i)*d2g(2) + dble(2*i-1)*(ux*d3w(8,i-1)) + dble((i-1)**2)*d3w(8,i-2))
          d3w(9,i) = - raptrm*(2.0_dp*d2w(4,i)*d1g(3) + d2w(3,i)*d1g(2) + 2.0_dp*dw(3,i)*d2g(4) + dw(2,i)*d2g(3) + &
            dble(2*i-1)*(ux*d3w(9,i-1)) + dble((i-1)**2)*d3w(9,i-2))
          d3w(10,i) = - raptrm*(3.0_dp*(d2w(3,i)*d1g(3) + dw(3,i)*d2g(3)) + &
            dble(2*i-1)*(ux*d3w(10,i-1)) + dble((i-1)**2)*d3w(10,i-2))
        enddo
!
!  Strain - cartesian - cartesian derivatives of W
!
        do i = 0,2*lm-1
          d3ws(1,i) = d2w(1,i+1)*u + d3w(1,i)*x*signx + 2.0_dp*d2w(1,i)*signx
          d3ws(2,i) = d2w(2,i+1)*u + d3w(4,i)*x*signx
          d3ws(3,i) = d2w(3,i+1)*u + d3w(6,i)*x*signx
          d3ws(4,i) = d2w(4,i+1)*u + d3w(5,i)*x*signx
          d3ws(5,i) = d2w(5,i+1)*u + d3w(3,i)*x*signx + d2w(5,i)*signx
          d3ws(6,i) = d2w(6,i+1)*u + d3w(2,i)*x*signx + d2w(6,i)*signx
        enddo
      endif
    endif
!
!  Initialise total derivatives to zero
!
    d1emf(1:3) = 0.0_dp
    d1emfs = 0.0_dp
    if (lgrad2) then
      d2emfs = 0.0_dp
      d2emf(1:6) = 0.0_dp
      d2emfm(1:3) = 0.0_dp
      if (lgrad3) then
        d3emf(1:10) = 0.0_dp
        d3emfm(1:6) = 0.0_dp
      endif
    endif
    ap = a
    do i = 1,lm
!
!  First derivatives : cartesian
!
      d1emf(1) = d1emf(1) - ecoeff(i)*ap*dw(1,2*i-1)
      d1emf(2) = d1emf(2) - ecoeff(i)*ap*dw(2,2*i-1)
      d1emf(3) = d1emf(3) - ecoeff(i)*ap*dw(3,2*i-1)
!
!  First derivatives : strain
!
      dwde = dw(1,2*i-1)*x*signx + w(2*i)*u
      trm1s = ecoeff(i)*ap*(dble(2*i-1)*w(2*i-1) + dwde)
      d1emfs = d1emfs - trm1s
      if (lgrad2) then
!
!  Second derivatives : cartesian - cartesian
!
        d2emf(1) = d2emf(1) - ecoeff(i)*ap*d2w(1,2*i-1)
        d2emf(2) = d2emf(2) - ecoeff(i)*ap*d2w(2,2*i-1)
        d2emf(3) = d2emf(3) - ecoeff(i)*ap*d2w(3,2*i-1)
        d2emf(4) = d2emf(4) - ecoeff(i)*ap*d2w(4,2*i-1)
        d2emf(5) = d2emf(5) - ecoeff(i)*ap*d2w(5,2*i-1)
        d2emf(6) = d2emf(6) - ecoeff(i)*ap*d2w(6,2*i-1)
!
!  Second derivatives : strain - strain
!
        d2wde2 = dwde + w(2*i+1)*u*u + d2w(1,2*i-1)*x*x + 2.0_dp*dw(1,2*i)*x*signx*u
        d2emfs = d2emfs - ecoeff(i)*ap*d2wde2 - dble(2*i-1)*trm1s - dble(2*i-1)*ecoeff(i)*ap*dwde
!
!  Second derivatives : cartesian - strain
!
        d2emfm(1) = d2emfm(1) - ecoeff(i)*ap*(dw(1,2*i)*u + d2w(1,2*i-1)*x*signx + dble(2*i-1)*dw(1,2*i-1) + dw(1,2*i-1))
        d2emfm(2) = d2emfm(2) - ecoeff(i)*ap*(dw(2,2*i)*u + d2w(6,2*i-1)*x*signx + dble(2*i-1)*dw(2,2*i-1))
        d2emfm(3) = d2emfm(3) - ecoeff(i)*ap*(dw(3,2*i)*u + d2w(5,2*i-1)*x*signx + dble(2*i-1)*dw(3,2*i-1))
        if (lgrad3) then
!
!  Third derivatives : cartesian - cartesian - cartesian
!
          do ii = 1,10
            d3emf(ii) = d3emf(ii) - ecoeff(i)*ap*d3w(ii,2*i-1)
          enddo
!
!  Third derivatives : cartesian - cartesian - strain
!
          do ii = 1,6
            d3emfm(ii) = d3emfm(ii) - ecoeff(i)*ap*(d3ws(ii,2*i-1) + dble(2*i-1)*d2w(ii,2*i-1))
          enddo
        endif
      endif
      ap = ap*a*a
    enddo
!
!  Correct second derivatives for terms in strfin
!
    if (lgrad2) then
      d2emfs = d2emfs - 2.0_dp*d1emfs
      d2emfm(1) = d2emfm(1) + 2.0_dp*d1emf(1)
    endif
  endif
!
  return
  end
