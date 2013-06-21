  subroutine sumderv1(n)
!
!  Completes first derivatives
!  Called by energy.
!
!   3/09 Created from sumderv2
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
!  Julian Gale, NRI, Curtin University, March 2009
!
  use current,        only : nstrains
  use derivatives
  use symmetry,       only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: n          ! Number of atoms in the asymmetric unit
!
!  Local variables
!
  integer(i4)             :: i          ! Looping index over atoms
!***********************************************
!  Sum over radial and non-radial derivatives  *
!***********************************************
  do i = 1,n
    xdrv(i) = xdrv(i) + xdrvnr(i)
    ydrv(i) = ydrv(i) + ydrvnr(i)
    zdrv(i) = zdrv(i) + zdrvnr(i)
  enddo
  if (lstr) then
    do i = 1,nstrains
      rstrd(i) = rstrd(i) + rstrdnr(i)
    enddo
  endif
!
  return
  end
