  subroutine initmaxreaxFFval3defaults(i)
!
!  Initialises the arrays associated with maxreaxFFval3
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
  use library
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
  integer(i4)             :: maxreaxFFspec2
!
!  Some things depend on pairs of species
!
  maxreaxFFspec2 = maxreaxFFspec*(maxreaxFFspec + 1)/2
!
!  Initialise new parts of data arrays
!
  if (i.ge.1.and.i.le.maxreaxFFval3) then
    reaxFFval3(1:6,i,1:maxreaxFFspec2,1:maxreaxFFspec) = 0.0_dp
  endif
!
  return
  end
