  subroutine meamscreen(rij2,rik2,rjk2,Sikj,dSikjdr,lpartial,lgrad1)
!
!  Calculates the screening function for a trio of atoms
!
!  On entry :
!
!  rij2         = i-j distance squared
!  rik2         = i-k distance squared
!  rjk2         = j-k distance squared
!  lgrad1       = if .true. then calculate the first derivatives of the screening function w.r.t. each distance
!
!  On exit :
!
!  lpartial     = if .true. then this atom has a screening factor between 0 and 1; therefore it contributes to the
!                 derivatives in the energy
!  Sikj         = screening function contribution of k on i-j
!  dSikjdr(3)   = (1/r) x first derivatives of Sikj w.r.t. r, if lgrad1 is .true.
!
!  For dSikjdr:
!
!    dSikjdr(1) = derivative w.r.t. rij
!    dSikjdr(2) = derivative w.r.t. rik
!    dSikjdr(3) = derivative w.r.t. rjk
!
!  Note: GULP uses (1/r dS/dr) in the derivatives, which equals 2*(dS/dr2) for simplicity here.
!        Similarly, second derivative (1/r d/dr(1/r dS/dr)) = 4*(d2S/dr2dr2).
!
!   4/09 Created 
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, April 2009
!
  use eam
  implicit none
!
!  Passed variables
!
  logical,     intent(in)    :: lgrad1
  logical,     intent(out)   :: lpartial
  real(dp),    intent(in)    :: rij2
  real(dp),    intent(in)    :: rik2
  real(dp),    intent(in)    :: rjk2
  real(dp),    intent(out)   :: Sikj
  real(dp),    intent(out)   :: dSikjdr(3)
!
!  Local variables
!
  real(dp)                   :: C                ! Elliptical function, C
  real(dp)                   :: Cbot             ! Denominator of C
  real(dp)                   :: Ctop             ! Numerator of C
  real(dp)                   :: dCdr2(3)         ! Derivatives of C w.r.t. r2 values
  real(dp)                   :: dCdXik           ! Derivative of C w.r.t. Xik
  real(dp)                   :: dCdXjk           ! Derivative of C w.r.t. Xjk
  real(dp)                   :: sqtrm            ! (Xik - Xjk)**2
  real(dp)                   :: dSikjdx          ! Derivative of Sikj w.r.t. x
  real(dp)                   :: dxdC             ! Derivative of x w.r.t. C
  real(dp)                   :: x                ! (C - Cmin)/(Cmax - Cmin)
  real(dp)                   :: xtrm             ! (1 - (1-x)**4)
  real(dp)                   :: Xik              ! rik2/rij2
  real(dp)                   :: Xjk              ! rjk2/rij2
  real(dp)                   :: dXikdrij2        ! Derivative of Xik w.r.t. rij2
  real(dp)                   :: dXikdrik2        ! Derivative of Xik w.r.t. rik2
  real(dp)                   :: dXjkdrij2        ! Derivative of Xjk w.r.t. rij2
  real(dp)                   :: dXjkdrjk2        ! Derivative of Xjk w.r.t. rjk2
!
!  Set lpartial as a return argument
!
  lpartial = .false.
!
!  Quick sanity check on values - angles j-i-k and i-j-k must have positive cosine.
!  The subtraction of 1 x 10-6 is just to avoid issues with rounding error.
!
  if (rjk2.ge.(rij2+rik2-1.0d-6).or.rik2.ge.(rij2+rjk2-1.0d-6)) then
    Sikj = 1.0_dp
    if (lgrad1) then
      dSikjdr(1:3) = 0.0_dp
    endif
    return
  endif
!
!  Compute X values
!
  Xik = rik2/rij2
  Xjk = rjk2/rij2
!
!  Compute C value
!
  sqtrm = (Xik - Xjk)**2
  Ctop = (2.0_dp*(Xik + Xjk) - sqtrm - 1.0_dp)
  Cbot = 1.0_dp/(1.0_dp - sqtrm)
  C = Ctop*Cbot
!
  if (C.lt.meam_Cmin) then
!
!  If C is below Cmin => Sikj = 0
!
    Sikj = 0.0_dp
!
!  Derivatives
!
    if (lgrad1) then
      dSikjdr(1:3) = 0.0_dp
    endif
  elseif (C.gt.meam_Cmax) then
!
!  If C is above Cmax => Sikj = 0
!
    Sikj = 1.0_dp
!
!  Derivatives
!
    if (lgrad1) then
      dSikjdr(1:3) = 0.0_dp
    endif
  else
!
!  If C is between Cmin and Cmax => compute Sikj explicitly
!
    lpartial = .true.
    dxdC = 1.0_dp/(meam_Cmax - meam_Cmin)
    x = (C - meam_Cmin)*dxdC
    xtrm = (1.0_dp - (1.0_dp - x)**4)
    Sikj = xtrm**2
!
!  Derivatives
!
    if (lgrad1) then
      dSikjdx   = 8.0_dp*xtrm*(1.0_dp - x)**3
      dCdXik    = 2.0_dp*(Xik - Xjk)*Ctop*Cbot*Cbot + 2.0_dp*(1.0_dp - (Xik - Xjk))*Cbot
      dCdXjk    = 2.0_dp*(Xjk - Xik)*Ctop*Cbot*Cbot + 2.0_dp*(1.0_dp - (Xjk - Xik))*Cbot
      dXikdrik2 = 1.0_dp/rij2
      dXikdrij2 = - Xik/rij2
      dXjkdrjk2 = 1.0_dp/rij2
      dXjkdrij2 = - Xjk/rij2
!
      dCdr2(1)  = dCdXik*dXikdrij2 + dCdXjk*dXjkdrij2
      dCdr2(2)  = dCdXik*dXikdrik2
      dCdr2(3)  = dCdXjk*dXjkdrjk2
!
      dSikjdr(1) = 2.0_dp*dSikjdx*dxdC*dCdr2(1)
      dSikjdr(2) = 2.0_dp*dSikjdx*dxdC*dCdr2(2)
      dSikjdr(3) = 2.0_dp*dSikjdx*dxdC*dCdr2(3)
    endif
  endif
!
  return
  end
