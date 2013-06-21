  subroutine celldrv(strderv,cellderv)
!
!  Converts the derivatives in terms of the strains into 
!  cell parameter derivatives.
!
!  11/07 Unused variables cleaned up
!   4/08 lgrad2 removed from arguments as this is unused
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
!  Julian Gale, NRI, Curtin University, April 2008
!
  use current
  use derivatives, only : cderv
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out) :: cellderv(6)
  real(dp),    intent(in)  :: strderv(6)
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: j
!
!  Initialise cell derivative vector
!
  do i = 1,6
    cellderv(i) = 0.0_dp
  enddo
  if (ndim.eq.3) then
    call setcellderv3D(.false.,.false.,.true.)
!
!  Calculate first derivatives of energy with respect to cell parameters
!
    do i = 1,6
      do j = 1,6
        cellderv(j) = cellderv(j) + strderv(i)*cderv(j,i)
      enddo
    enddo
  elseif (ndim.eq.2) then
    call setcellderv2D(.false.,.false.,.true.)
!
!  Calculate first derivatives of energy with respect to cell parameters
!
    do i = 1,3
      do j = 1,3
        cellderv(j) = cellderv(j) + strderv(i)*cderv(j,i)
      enddo
    enddo
  elseif (ndim.eq.1) then
    call setcellderv1D(.false.,.true.)
    cellderv(1) = strderv(1)*cderv(1,1)
  endif
!
  return
  end
