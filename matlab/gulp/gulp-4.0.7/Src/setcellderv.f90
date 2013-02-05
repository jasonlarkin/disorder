!****************
!  3-D systems  *
!****************
  subroutine setcellderv3D(lgrad2,lcosine,linvert)
!
!  Converts the derivatives in terms of the strains into 
!  cell parameter derivatives.
!
!  On entry:
!
!  lcosine = if .true. then the angular terms are returned as derivatives
!            with respect to cosines of the angles, rather than the angles
!            themselves
!  linvert = if .true. then the inverse derivative matrix is returned
!
!  On exit:
!
!  cderv(m,n) contains the derivatives of cell parameter n with respect to
!  strain m or the inverse of this matrix.
!
!  11/06 Logical controls of cosines and inversion added
!  12/07 Unused variables removed
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
  use current
  use derivatives, only : cderv, cderv2
  implicit none
!
!  Passed variables
!
  logical,     intent(in)  :: lgrad2
  logical,     intent(in)  :: lcosine
  logical,     intent(in)  :: linvert
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: ifail
  real(dp)                 :: drvde(6,3,3)
  real(dp)                 :: dotab
  real(dp)                 :: dotac
  real(dp)                 :: dotbc
  real(dp)                 :: ra
  real(dp)                 :: rb
  real(dp)                 :: rc
  real(dp)                 :: rsina
  real(dp)                 :: rsinb
  real(dp)                 :: rsinc
  real(dp)                 :: sina
  real(dp)                 :: sinb
  real(dp)                 :: sinc
  real(dp)                 :: wrk(12)
!
!  Set reciprocal cell parameters
!
  ra = 1.0_dp/a
  rb = 1.0_dp/b
  rc = 1.0_dp/c
!
  cderv(1:6,1:6) = 0.0_dp
  drvde(1:6,1:3,1:3) = 0.0_dp
  if (lgrad2) then
    cderv2(1:6,1:6) = 0.0_dp
  endif
!
!  Set derivatives of cell vectors with respect to strain
!
  drvde(1,1,1) = r1x
  drvde(5,1,1) = 0.5_dp*r1z
  drvde(6,1,1) = 0.5_dp*r1y
  drvde(1,1,2) = r2x
  drvde(5,1,2) = 0.5_dp*r2z
  drvde(6,1,2) = 0.5_dp*r2y
  drvde(1,1,3) = r3x
  drvde(5,1,3) = 0.5_dp*r3z
  drvde(6,1,3) = 0.5_dp*r3y
  drvde(2,2,1) = r1y
  drvde(4,2,1) = 0.5_dp*r1z
  drvde(6,2,1) = 0.5_dp*r1x
  drvde(2,2,2) = r2y
  drvde(4,2,2) = 0.5_dp*r2z
  drvde(6,2,2) = 0.5_dp*r2x
  drvde(2,2,3) = r3y
  drvde(4,2,3) = 0.5_dp*r3z
  drvde(6,2,3) = 0.5_dp*r3x
  drvde(3,3,1) = r1z
  drvde(4,3,1) = 0.5_dp*r1y
  drvde(5,3,1) = 0.5_dp*r1x
  drvde(3,3,2) = r2z
  drvde(4,3,2) = 0.5_dp*r2y
  drvde(5,3,2) = 0.5_dp*r2x
  drvde(3,3,3) = r3z
  drvde(4,3,3) = 0.5_dp*r3y
  drvde(5,3,3) = 0.5_dp*r3x
!
!  Calculate first derivatives of cell lengths with respect to strain
!
! a :
  do i = 1,6
    cderv(i,1) = ra*(r1x*drvde(i,1,1) + r1y*drvde(i,2,1) + r1z*drvde(i,3,1))
  enddo
! b : 
  do i = 1,6
    cderv(i,2) = rb*(r2x*drvde(i,1,2) + r2y*drvde(i,2,2) + r2z*drvde(i,3,2))
  enddo
! c : 
  do i = 1,6
    cderv(i,3) = rc*(r3x*drvde(i,1,3) + r3y*drvde(i,2,3) + r3z*drvde(i,3,3))
  enddo
!
!  Compute dot products
!
  dotab = r1x*r2x + r1y*r2y + r1z*r2z
  dotac = r1x*r3x + r1y*r3y + r1z*r3z
  dotbc = r2x*r3x + r2y*r3y + r2z*r3z
! Alpha :
  do i = 1,6
    cderv(i,4) = rb*rc*(r2x*drvde(i,1,3) + r3x*drvde(i,1,2) + r2y*drvde(i,2,3) +  &
                        r3y*drvde(i,2,2) + r2z*drvde(i,3,3) + r3z*drvde(i,3,2))
  enddo
! Beta :
  do i = 1,6
    cderv(i,5) = ra*rc*(r1x*drvde(i,1,3) + r3x*drvde(i,1,1) + r1y*drvde(i,2,3) +  &
                        r3y*drvde(i,2,1) + r1z*drvde(i,3,3) + r3z*drvde(i,3,1))
  enddo
! Gamma :
  do i = 1,6
    cderv(i,6) = ra*rb*(r2x*drvde(i,1,1) + r1x*drvde(i,1,2) + r2y*drvde(i,2,1) +  &
                        r1y*drvde(i,2,2) + r2z*drvde(i,3,1) + r1z*drvde(i,3,2))
  enddo
!
!  Add cell length derivative contributions to derivatives w.r.t. angle cosines
!
  do i = 1,6
    cderv(i,4) = cderv(i,4) - dotbc*rb*rc*(rb*cderv(i,2) + rc*cderv(i,3))
    cderv(i,5) = cderv(i,5) - dotac*ra*rc*(ra*cderv(i,1) + rc*cderv(i,3))
    cderv(i,6) = cderv(i,6) - dotab*ra*rb*(ra*cderv(i,1) + rb*cderv(i,2))
  enddo
!
!  Convert angle cosine derivatives to angle derivatives if required
!
! alpha / beta / gamma :
  if (.not.lcosine) then
    sina = sin(alpha*degtorad)*degtorad
    sinb = sin(beta*degtorad)*degtorad
    sinc = sin(gamma*degtorad)*degtorad
    rsina = - 1.0_dp/sina
    rsinb = - 1.0_dp/sinb
    rsinc = - 1.0_dp/sinc
    do i = 1,6
      cderv(i,4) = rsina*cderv(i,4)
      cderv(i,5) = rsinb*cderv(i,5)
      cderv(i,6) = rsinc*cderv(i,6)
    enddo
  endif
  if (linvert) then
!
!  Invert cderv(s)
!
    call matinv(cderv,6_i4,6_i4,wrk,ifail)
    if (ifail.ne.0) then
      call outerror('failure to invert cell-strain matrix in cellderv',0_i4)
      call stopnow('setcellderv')
    endif
  endif
!
  return
  end
!****************
!  2-D systems  *
!****************
  subroutine setcellderv2D(lgrad2,lcosine,linvert)
!
!  Converts the derivatives in terms of the strains into 
!  cell parameter derivatives.
!
!  On entry:
!
!  lcosine = if .true. then the angular terms are returned as derivatives
!            with respect to cosines of the angles, rather than the angles
!            themselves
!  linvert = if .true. then the inverse derivative matrix is returned
!
!  On exit:
!
!  cderv(m,n) contains the derivatives of cell parameter n with respect to
!  strain m or the inverse of this matrix.
!
!  11/06 Logical controls of cosines and inversion added
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006
!
  use constants
  use current
  use derivatives, only : cderv, cderv2
  implicit none
!
!  Passed variables
!
  logical,     intent(in)  :: lgrad2
  logical,     intent(in)  :: lcosine
  logical,     intent(in)  :: linvert
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: ifail
  real(dp)                 :: drvde(3,2,2)
  real(dp)                 :: dotab
  real(dp)                 :: ra
  real(dp)                 :: rb
  real(dp)                 :: rsina
  real(dp)                 :: sina
  real(dp)                 :: wrk(12)
!
!  Set reciprocal cell parameters
!
  ra = 1.0_dp/a
  rb = 1.0_dp/b
!
  cderv(1:3,1:3) = 0.0_dp
  drvde(1:3,1:2,1:2) = 0.0_dp
  if (lgrad2) then
    cderv2(1:3,1:3) = 0.0_dp
  endif
!
!  Set derivatives of cell vectors with respect to strain
!
  drvde(1,1,1) = r1x
  drvde(3,1,1) = 0.5_dp*r1y
  drvde(1,1,2) = r2x
  drvde(3,1,2) = 0.5_dp*r2y
  drvde(2,2,1) = r1y
  drvde(3,2,1) = 0.5_dp*r1x
  drvde(2,2,2) = r2y
  drvde(3,2,2) = 0.5_dp*r2x
!
!  Calculate first derivatives of cell lengths with respect to strain
!
! a :
  do i = 1,3
    cderv(i,1) = ra*(r1x*drvde(i,1,1) + r1y*drvde(i,2,1))
  enddo
! b : 
  do i = 1,3
    cderv(i,2) = rb*(r2x*drvde(i,1,2) + r2y*drvde(i,2,2))
  enddo
!
!  Compute dot products
!
  dotab = r1x*r2x + r1y*r2y 
! Alpha :
  do i = 1,3
    cderv(i,3) = ra*rb*(r2x*drvde(i,1,1) + r1x*drvde(i,1,2) + r2y*drvde(i,2,1) + r1y*drvde(i,2,2))
  enddo
!
!  Add cell length derivative contributions to derivatives w.r.t. angle cosines
!
  do i = 1,3
    cderv(i,3) = cderv(i,3) - dotab*ra*rb*(ra*cderv(i,1) + rb*cderv(i,2))
  enddo
!
!  Convert angle cosine derivatives to angle derivatives if required
!
! alpha / beta / gamma :
  if (.not.lcosine) then
    sina = sin(alpha*degtorad)*degtorad
    rsina = - 1.0_dp/sina
    do i = 1,3
      cderv(i,3) = rsina*cderv(i,3)
    enddo
  endif
  if (linvert) then
!
!  Invert cderv(s)
!
    call matinv(cderv,6_i4,3_i4,wrk,ifail)
    if (ifail.ne.0) then
      call outerror('failure to invert cell-strain matrix in cellderv',0_i4)
      call stopnow('setcellderv')
    endif
  endif
!
  return
  end
!****************
!  1-D systems  *
!****************
  subroutine setcellderv1D(lgrad2,linvert)
!
!  Converts the derivatives in terms of the strains into 
!  cell parameter derivatives.
!
!  11/12 cderv2 now set when lgrad2 is true
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
!  Copyright Curtin University 2012
!
!  Julian Gale, Curtin University, November 2012
!
  use constants
  use current
  use derivatives, only : cderv, cderv2
  implicit none
!
!  Passed variables
!
  logical,     intent(in)  :: lgrad2
  logical,     intent(in)  :: linvert
!
!  Set cell derivative matrix element
!
  if (linvert) then
    cderv(1,1) = 1.0_dp/a
  else
    cderv(1,1) = a
  endif
  if (lgrad2) then
    cderv2(1,1) = 0.0_dp
  endif
!
  return
  end
