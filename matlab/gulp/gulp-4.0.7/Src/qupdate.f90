  subroutine qupdate
!
!  Updates the charges for all configurations based
!  on the charges stored in the species arrays
!  Called during charge fitting
!
!   5/95 lmask added - protect different charges on same species
!        by limiting charge update to those atoms with species
!        specification or fitted charges.
!  10/02 Style updated
!  11/05 lqinspec added to further protect charges so that charge
!        update only applies if charges were input
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
!  Julian Gale, NRI, Curtin University, November 2005
!
  use configurations
  use current
  use species
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: j
  integer(i4) :: ns
  integer(i4) :: ntyp
  real(dp)    :: qs
!
  do i = 1,nspec
    if (lmask(i).and.lqinspec(i)) then
      qs = qlspec(i)
      ns = natspec(i)
      ntyp = ntypspec(i)
      do j = 1,nasum
        if (natcfg(j).eq.ns.and.(ntypcfg(j).eq.ntyp.or.ntyp.eq.0)) qlcfg(j) = qs
      enddo
    endif
  enddo
!
  return
  end
