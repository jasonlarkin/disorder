  subroutine initmaxbonddefaults(i)
!
!  Initialises the arrays associated with maxbond
!
!   9/10 Created from changemax routine
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
!  Julian Gale, NRI, Curtin University, September 2010
!
  use current
  use defects
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
  integer(i4)             :: j
!
!  Initialise new part of data array
!
  if (i.ge.1.and.i.le.maxbond) then
    do j = 1,maxat
      nbonded(i,j) = 0
      nbondedtype(1:2,i,j) = 1
    enddo
    do j = 1,maxr1at
      nbondeddef(i,j) = 0
    enddo
  endif
!
  return
  end
