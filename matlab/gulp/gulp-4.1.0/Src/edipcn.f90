  subroutine EDIPcnpair(nzij,rij,Z,dZdr,lgrad1,lnonzero)
!
!  Subroutine to calculate the coordination number for EDIP. 
!
!  On entry : 
!
!  nzij            = index for pair of EDIP species
!  rij             = distance between i and j
!  lgrad1          = if .true. compute the first derivative 
!
!  On exit :
!
!  Z               = coordination number contribution
!  dZdr            = first derivative of Z w.r.t. r x 1/r if lgrad1
!  lnonzero        = if .true. then a non-zero bond order was computed
!
!   9/10 Created
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, September 2010
!
  use EDIPdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nzij
  real(dp),    intent(in)             :: rij
  real(dp),    intent(out)            :: Z
  real(dp),    intent(out)            :: dZdr
  logical,     intent(in)             :: lgrad1
  logical,     intent(out)            :: lnonzero
!
!  Local variables
!
  real(dp)                            :: alpha
  real(dp)                            :: dxdr
  real(dp)                            :: dxtrmdx
  real(dp)                            :: fhigh
  real(dp)                            :: flow
  real(dp)                            :: rfdiff
  real(dp)                            :: x
  real(dp)                            :: xtrm
!
!  Initialise value and derivatives 
!
  Z = 0.0_dp
  if (lgrad1) then
    dZdr = 0.0_dp
  endif
  lnonzero = .false.
!
!  If this is a valid pair then calculate
!
  if (lEDIPpairOK(nzij)) then
    if (rij.le.EDIPflow(nzij)) then
      Z = 1.0_dp
      lnonzero = .true.
    elseif (rij.ge.EDIPfhigh(nzij)) then
      Z = 0.0_dp
    else
      alpha = EDIPalpha(nzij)
      fhigh = EDIPfhigh(nzij)
      flow = EDIPflow(nzij)
      rfdiff = 1.0_dp/(fhigh-flow)
      x = (rij - flow)*rfdiff
      xtrm = 1.0_dp/(1.0_dp - x**(-3.0_dp))
      Z = exp(alpha*xtrm)
      if (lgrad1) then
        dxdr = rfdiff
        dxtrmdx = - 3.0_dp*xtrm*xtrm*x**(-4.0_dp)
        dZdr = alpha*Z*dxtrmdx*dxdr
      endif
      lnonzero = .true.
    endif
  endif
!
  return
  end
!
  subroutine EDIPppair(npij,rij,P,dPdr,lgrad1,lnonzero)
!
!  Subroutine to calculate the P function for EDIP. 
!
!  On entry : 
!
!  npij            = index for pair of EDIP species
!  rij             = distance between i and j
!  lgrad1          = if .true. compute the first derivative 
!
!  On exit :
!
!  P               = P function
!  dPdr            = first derivative of P w.r.t. r x 1/r if lgrad1
!  lnonzero        = if .true. then a non-zero P function was computed
!
!  10/10 Created
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, October 2010
!
  use EDIPdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: npij
  real(dp),    intent(in)             :: rij
  real(dp),    intent(out)            :: P
  real(dp),    intent(out)            :: dPdr
  logical,     intent(in)             :: lgrad1
  logical,     intent(out)            :: lnonzero
!
!  Local variables
!
  real(dp)                            :: alpha
  real(dp)                            :: dxdr
  real(dp)                            :: dxtrmdx
  real(dp)                            :: phigh
  real(dp)                            :: plow
  real(dp)                            :: rpdiff
  real(dp)                            :: x
  real(dp)                            :: xtrm
!
!  Initialise value and derivatives 
!
  P = 0.0_dp
  if (lgrad1) then
    dPdr = 0.0_dp
  endif
  lnonzero = .false.
!
!  If this is a valid pair then calculate
!
  if (lEDIPpairOK(npij)) then
    if (rij.le.EDIPplow(npij)) then
      P = 1.0_dp
      lnonzero = .true.
    elseif (rij.ge.EDIPphigh(npij)) then
      P = 0.0_dp
    else
      alpha = EDIPalpha(npij)
      phigh = EDIPphigh(npij)
      plow = EDIPplow(npij)
      rpdiff = 1.0_dp/(phigh-plow)
      x = (rij - plow)*rpdiff
      xtrm = 1.0_dp/(1.0_dp - x**(-3.0_dp))
      P = exp(alpha*xtrm)
      if (lgrad1) then
        dxdr = rpdiff
        dxtrmdx = - 3.0_dp*xtrm*xtrm*x**(-4.0_dp)
        dPdr = alpha*P*dxtrmdx*dxdr
      endif
      lnonzero = .true.
    endif
  endif
!
  return
  end
!
  subroutine EDIP_pi3(Z,pi3,dpi3dZ,lgrad1)
!
!  Subroutine to calculate the pi_3 taper for EDIP. 
!
!  On entry : 
!
!  Z               = uncorrected coordination number of i
!  lgrad1          = if .true. compute the first derivative 
!
!  On exit :
!
!  pi3             = pi3 taper function
!  dpi3dZ          = first derivative of pi3 w.r.t. Z
!
!  10/10 Created
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, October 2010
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: Z
  real(dp),    intent(out)            :: pi3
  real(dp),    intent(out)            :: dpi3dZ
  logical,     intent(in)             :: lgrad1
!
!  Local variables
!
  real(dp)                            :: x
!
  if (Z.ge.4.0_dp) then
!
!  Out of range - set to zero
!
    pi3 = 0.0_dp
    if (lgrad1) then
      dpi3dZ = 0.0_dp
    endif
  else
    x = (Z - 3.0_dp)**2 - 1.0_dp
    pi3 = x*x
    if (lgrad1) then
      dpi3dZ = 4.0_dp*x*(Z - 3.0_dp)
    endif
  endif
!
  return
  end
!
  subroutine EDIP_pi2(Z,pi2,dpi2dZ,lgrad1)
!
!  Subroutine to calculate the pi_2 taper for EDIP. 
!
!  On entry : 
!
!  Z               = uncorrected coordination number of i
!  lgrad1          = if .true. compute the first derivative 
!
!  On exit :
!
!  pi2             = pi2 taper function
!  dpi2dZ          = first derivative of pi2 w.r.t. Z
!
!  10/10 Created
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, October 2010
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: Z
  real(dp),    intent(out)            :: pi2
  real(dp),    intent(out)            :: dpi2dZ
  logical,     intent(in)             :: lgrad1
!
!  Local variables
!
  real(dp)                            :: x
!
  if (Z.ge.3.0_dp) then
!
!  Out of range - set to zero
!
    pi2 = 0.0_dp
    if (lgrad1) then
      dpi2dZ = 0.0_dp
    endif
  else
    x = (Z - 2.0_dp)**2 - 1.0_dp
    pi2 = x*x
    if (lgrad1) then
      dpi2dZ = 4.0_dp*x*(Z - 2.0_dp)
    endif
  endif
!
  return
  end
!
  subroutine EDIP_pi(Z,pi0,dpi0dZ,lgrad1)
!
!  Subroutine to calculate the pi taper for EDIP. 
!
!  On entry : 
!
!  Z               = uncorrected coordination number of i
!  lgrad1          = if .true. compute the first derivative 
!
!  On exit :
!
!  pi0             = pi taper function
!  dpi0dZ          = first derivative of pi0 w.r.t. Z
!
!  10/10 Created
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, October 2010
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: Z
  real(dp),    intent(out)            :: pi0
  real(dp),    intent(out)            :: dpi0dZ
  logical,     intent(in)             :: lgrad1
!
  if (Z.ge.3.0_dp) then
!
!  For Z > 3, pi0 = pi3 ...
!
    call EDIP_pi3(Z,pi0,dpi0dZ,lgrad1)
  else
!
!  ... else pi0 = 1.
!
    pi0 = 1.0_dp
    if (lgrad1) then
      dpi0dZ = 0.0_dp
    endif
  endif
!
  return
  end
