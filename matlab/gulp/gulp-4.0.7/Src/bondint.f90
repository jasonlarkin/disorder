  subroutine bondint(nion)
!
!  Find best location for bonded interstitial using covalent radii
!
!  nion = pointer to region 1 ion to which bond is to be formed
!
!   2/07 Referencing of atom arrays corrected to use new defect ones
!
!  Note that because of the point at which this routine is called
!  from setdef the last ion in region 1 is the one being added so
!  the search loop for region 1 is only to (nreg1-1).
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
!  Julian Gale, NRI, Curtin University, February 2007
!
  use current
  use defects, only : nreg1, xdefe, ydefe, zdefe, natdefe
  use element
  use molecule
  use species
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)   :: nion
!
!  Local variables
!
  integer(i4)               :: i
  integer(i4)               :: inat
  integer(i4)               :: nati
  integer(i4)               :: natp
  integer(i4)               :: nbond
  real(dp)                  :: r
  real(dp)                  :: radi
  real(dp)                  :: radp
  real(dp)                  :: rbond
  real(dp)                  :: rsum
  real(dp)                  :: rtoll
  real(dp)                  :: rtot
  real(dp)                  :: xcp
  real(dp)                  :: ycp
  real(dp)                  :: zcp
  real(dp)                  :: xdiff
  real(dp)                  :: ydiff
  real(dp)                  :: zdiff
  real(dp)                  :: xvsum
  real(dp)                  :: yvsum
  real(dp)                  :: zvsum
!
!  If there is only one ion in region 1 then trap
!  Chose direction at random!
!
  if (nreg1.eq.2) then
    xvsum = 1.0_dp
    yvsum = 0.0_dp
    zvsum = 0.0_dp
    goto 20
  endif
!
!  Locate all bonded cores for pivot atom
!  Sum all interatomic vectors
!
  xcp = xdefe(nion)
  ycp = ydefe(nion)
  zcp = zdefe(nion)
  natp = natdefe(nion)
  if (natp.gt.maxele) natp = natp - maxele
  radp = rcov(natp)
  xvsum = 0.0_dp
  yvsum = 0.0_dp
  zvsum = 0.0_dp
  nbond = 0
  rtoll = rtol
10 continue
  do i = 1,nreg1 - 1
!
!  Exclude ion itself
!
    if (i.ne.nion) then
      nati = natdefe(i)
!
!  Exclude shells
!
      if (nati.le.maxele) then
        radi = rcov(nati)
        rbond = rtoll*(radi + radp)
        rbond = rbond*rbond
        xdiff = xdefe(i) - xcp
        ydiff = ydefe(i) - ycp
        zdiff = zdefe(i) - zcp
        r = xdiff*xdiff + ydiff*ydiff + zdiff*zdiff
        if (r.lt.rbond) then
          nbond = nbond + 1
          xvsum = xvsum + xdiff
          yvsum = yvsum + ydiff
          zvsum = zvsum + zdiff
        endif
      endif
    endif
  enddo
!
!  Normalise vector
!
  rsum = xvsum*xvsum + yvsum*yvsum + zvsum*zvsum
  if (rsum.gt.1.0d-12) then
    rsum = 1.0_dp/sqrt(rsum)
    xvsum = xvsum*rsum
    yvsum = yvsum*rsum
    zvsum = zvsum*rsum
  elseif (nbond.eq.0) then
!
!  No bonds - have to increase rtol and try again
!
    rtoll = 1.5_dp*rtoll
    goto 10
  else
!
!  Symmetric environment - chose at random!
!
    xvsum = 1.0_dp
    yvsum = 0.0_dp
    zvsum = 0.0_dp
  endif
!
!  Place interstitial at sum of covalent radii along the direction
!  of the negative sum of the other interatomic vectors
!
20 inat = natdefe(nreg1)
  if (inat.gt.maxele) inat = inat - maxele
  radi = rcov(inat)
  rtot = radi + radp
  xdefe(nreg1) = xcp - rtot*xvsum
  ydefe(nreg1) = ycp - rtot*yvsum
  zdefe(nreg1) = zcp - rtot*zvsum
!
  return
  end
