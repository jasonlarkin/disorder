  subroutine strsym
!
!  Symmetry adapts strain derivatives
!
!   9/97 Created from strfin
!  11/04 Rhombohedral cell special case removed
!   4/09 Modified to include rstrd for non-radial forces
!   5/12 Symmetrisation of the atomic stresses added
!   5/12 Symmetrisation of the atomic stresses removed
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use current
  use derivatives
  use numbers
  implicit none
!
!  Local variables
!
  real(dp)        :: sum
!**************************************************
!  Symmetrisation of strain vectors and matrices  *
!**************************************************
!
!  If symmetry adaption of first strain derivatives is being used
!  then a time saving trick must be used on the strain derivatives
!  by averaging dE/de1 and dE/de2 to get the correct answer
!
  if (ictype.eq.6) then
!----------
!  Cubic  !
!----------
    sum = strderv(1) + strderv(2) + strderv(3)
    sum = third*sum
    strderv(1) = sum
    strderv(2) = sum
    strderv(3) = sum
    strderv(4) = 0.0_dp
    strderv(5) = 0.0_dp
    strderv(6) = 0.0_dp
    sum = rstrd(1) + rstrd(2) + rstrd(3)
    sum = third*sum
    rstrd(1) = sum
    rstrd(2) = sum
    rstrd(3) = sum
    rstrd(4) = 0.0_dp
    rstrd(5) = 0.0_dp
    rstrd(6) = 0.0_dp
    sum = rstrdnr(1) + rstrdnr(2) + rstrdnr(3)
    sum = third*sum
    rstrdnr(1) = sum
    rstrdnr(2) = sum
    rstrdnr(3) = sum
    rstrdnr(4) = 0.0_dp
    rstrdnr(5) = 0.0_dp
    rstrdnr(6) = 0.0_dp
  elseif (ictype.eq.4.or.ictype.eq.5) then
!-----------------------------------------
!  Tetragonal or hexagonal/rhombohedral  !
!-----------------------------------------
    sum = strderv(1) + strderv(2)
    sum = 0.5_dp*sum
    strderv(1) = sum
    strderv(2) = sum
    strderv(4) = 0.0_dp
    strderv(5) = 0.0_dp
    strderv(6) = 0.0_dp
    sum = rstrd(1) + rstrd(2)
    sum = 0.5_dp*sum
    rstrd(1) = sum
    rstrd(2) = sum
    rstrd(4) = 0.0_dp
    rstrd(5) = 0.0_dp
    rstrd(6) = 0.0_dp
    sum = rstrdnr(1) + rstrdnr(2)
    sum = 0.5_dp*sum
    rstrdnr(1) = sum
    rstrdnr(2) = sum
    rstrdnr(4) = 0.0_dp
    rstrdnr(5) = 0.0_dp
    rstrdnr(6) = 0.0_dp
  elseif (ictype.eq.3) then
!-----------------
!  Orthorhombic  !
!-----------------
    strderv(4) = 0.0_dp
    strderv(5) = 0.0_dp
    strderv(6) = 0.0_dp
    rstrd(4) = 0.0_dp
    rstrd(5) = 0.0_dp
    rstrd(6) = 0.0_dp
    rstrdnr(4) = 0.0_dp
    rstrdnr(5) = 0.0_dp
    rstrdnr(6) = 0.0_dp
  endif
!
  return
  end
