  subroutine setcutoffmaxbo
!
!  Calculates the maximum cut-off distance over all bond order potentials 
!  required for a spatial decomposition of the atoms into boxes.
!
!   9/04 Separate routine created from setcutoffmaxbo
!   9/04 Cutoff for nboQ potentials added
!  10/04 Brenner cutoff removed from this routine
!  12/04 Brenner cutoff added back!
!   7/09 cutoffmaxbo now passed via general module
!  10/10 EDIP cutoffs added
!
!  On exit :
!
!  cutoffmax = maximum cut-off distance needed over bond order potentials
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
  use bondorderdata
  use brennerdata,   only : bR2, bTR2
  use control,       only : lbrenner, lEDIP
  use EDIPdata,      only : EDIPcutoff
  use general,       only : cutoffmaxbo
  use reaxFFdata,    only : nreaxFFspec, reaxFFcutoff, reaxFFcutoffVDW
  implicit none
!
!  Local variables
!
  integer(i4)               :: n
!
!  Initialise the cut-off distance to zero
!
  cutoffmaxbo = 0.0_dp
!
!  Compare with Brenner cut-off
!
  if (lbrenner) cutoffmaxbo = max(cutoffmaxbo,bR2(1),bR2(2),bR2(3),bTR2(1),bTR2(2),bTR2(3))
!
!  Compare with bond-order cut-off
!
  do n = 1,nbopot
    cutoffmaxbo = max(cutoffmaxbo,rBOmax(n))
  enddo
  do n = 1,nboQ
    cutoffmaxbo = max(cutoffmaxbo,rBOmaxQ(n))
  enddo
!
!  Check for ReaxFF cutoffs
!
  if (nreaxFFspec.gt.0) then
    cutoffmaxbo = max(cutoffmaxbo,reaxFFcutoff)
    cutoffmaxbo = max(cutoffmaxbo,reaxFFcutoffVDW)
  endif
!
!  Compare with EDIP cut-off
!
  if (lEDIP) cutoffmaxbo = max(cutoffmaxbo,EDIPcutoff)
!
  return
  end
