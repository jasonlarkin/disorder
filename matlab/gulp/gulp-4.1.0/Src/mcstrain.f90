  subroutine mcstrain(mode)
!
!  MC routine for cell strain. 
!
!  mode = if mode = 1, then create new trial strain
!         if mode = 2, then undo previous strain
!
!   5/07 Created
!   6/07 Call to x0tostrcentroid added for undo since x0 array is
!        modified to correct fractional coordinates in here.
!        Not needed for trial step since this is done in 
!        call to energy.
!  12/07 Unused variables removed
!   1/08 random -> GULP_random
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
!  Julian Gale, NRI, Curtin University, January 2008
!
  use current
  use general
  use genetic, only : iseed
  use molecule
  use montecarlo
  use parallel
!
!  Passed variables
!
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                  :: mode
!
!  Local variables
!
  integer(i4),                        save :: ntostrain
  real(dp),                           save :: deltastrain = 0.0_dp
!
  logical                                  :: lgeometryOK
  real(dp)                                 :: randnum
  real(dp)                                 :: GULP_random
!
  if (mode.eq.2) then
!*************************************
!  Mode 2 : Undo last displacements  *
!*************************************
    x0(1:nstrains) = 1.0_dp
    x0(ntostrain) = 1.0_dp/(1.0_dp + deltastrain)
    call x0strain(lgeometryOK)
    call x0tostrcentroid
  else
!*******************************
!  Mode 1 : New displacements  *
!*******************************
    x0(1:nstrains) = 1.0_dp
!
!  Choose cell parameter to strain
!
    randnum = GULP_random(iseed,1_i4)
    ntostrain = nstrainable*randnum + 1_i4
    if (ntostrain.gt.nstrainable) ntostrain = nstrainable
    ntostrain = nptrstrainable(ntostrain)
!
!  Find displacement to apply
!
    deltastrain = smaxmc*GULP_random(iseed,2_i4)
!
!  Apply strain to configuration array
!
    x0(ntostrain) = x0(ntostrain) + deltastrain
    call x0strain(lgeometryOK)
  endif
!
  return
  end
