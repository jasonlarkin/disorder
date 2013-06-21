  subroutine initqmatrixc
!
!  Routine sets up terms needed by qmatrixelementc.
!
!   4/05 Created from initqmatrix
!  12/08 Migrated to version 3.5 and converted to f90 format
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
!  Julian Gale, NRI, Curtin University, December 2008
!
  use datatypes
  use current,   only : rv, ndim
  use wolfcosmo, only : cutwc, rmax2c, maxloopc
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  real(dp)    :: rv2
!
  if (ndim.gt.0) then
!
!  Estimate upper limits to looping
!
    if (ndim.eq.3) then
      do i = 1,3
        rv2 = rv(1,i)**2 + rv(2,i)**2 + rv(3,i)**2
        rv2 = sqrt(rv2)
        maxloopc(i) = (cutwc/rv2) + 2
      enddo
    elseif (ndim.eq.2) then
      do i = 1,2
        rv2 = rv(1,i)**2 + rv(2,i)**2
        rv2 = sqrt(rv2)
        maxloopc(i) = (cutwc/rv2) + 2
      enddo
      maxloopc(3) = 0
    elseif (ndim.eq.1) then
      maxloopc(1) = (cutwc/rv(1,1)) + 1
      maxloopc(2) = 0
      maxloopc(3) = 0
    endif
    rmax2c = cutwc*cutwc
  else
    maxloopc(1:3) = 0
    rmax2c = cutwc*cutwc
  endif
!
  return
  end
