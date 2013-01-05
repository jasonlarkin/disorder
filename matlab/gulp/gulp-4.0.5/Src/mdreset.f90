  subroutine mdreset
!
!  Reset averages to zero after equilibriation
!
!   7/07 suminertia initialised
!   4/08 Modified for NPH ensemble
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, April 2008.
!
  use current
  use moldyn
  use velocities
  implicit none
!
!  Accumulate running averages
!
  naverpt = 0
  sumvsq = 0.0_dp
  sumener = 0.0_dp
  sumvir = 0.0_dp
  sumcons = 0.0_dp
  suminertia = 0.0_dp
  sumstress(1:nstrains) = 0.0_dp
  if (nensemble(ncf).eq.3.or.nensemble(ncf).eq.4) then
    sumacell = 0.0_dp
    sumbcell = 0.0_dp
    sumccell = 0.0_dp
    sumalpcell = 0.0_dp
    sumbetcell = 0.0_dp
    sumgamcell = 0.0_dp
    sumvol = 0.0_dp
  endif
  sumtem = 0.0_dp
  sumcst = 0.0_dp
  if (lmdconstrain(ncf)) then
    sumlambdaR = 0.0_dp
    sumlambdaV = 0.0_dp
  endif
!
!  If canonical ensemble reset sfactor terms
!
  if (nensemble(ncf).ge.2) then
    sfac = 0.0_dp
    sfac0 = 0.0_dp
    sumsfac = 0.0_dp
    svel = 0.0_dp
    if (nensemble(ncf).eq.3) then
      velc(1:nstrains) = 0.0_dp
    endif
  endif
!
  return
  end
