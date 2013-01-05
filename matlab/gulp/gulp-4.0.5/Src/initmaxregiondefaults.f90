  subroutine initmaxregiondefaults(i)
!
!  Initialises the arrays associated with maxregion
!
!   9/10 Created from changemax routine
!  11/11 Initialisation of eregion2region added
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, November 2011
!
  use configurations, only : lopfreg, lregionrigid, maxcfg, maxregion, nregiontype
  use derivatives
  use energies,       only : eregion2region
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
!  Local variables
!  
  integer(i4)             :: j
!
!  Initialise new parts of data arrays
!  
  if (i.ge.1.and.i.le.maxregion) then
    do j = 1,maxcfg
      nregiontype(i,j) = 0
      if (i.eq.2) then
        lregionrigid(i,j) = .true.
      else
        lregionrigid(i,j) = .false.
      endif
      lopfreg(i,j) = .false.
    enddo
!
    do j = 1,maxregion
      eregion2region(j,i) = 0.0_dp
      eregion2region(i,j) = 0.0_dp
    enddo
  endif
!
  return
  end
