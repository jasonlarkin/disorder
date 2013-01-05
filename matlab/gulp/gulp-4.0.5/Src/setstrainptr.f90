  subroutine setstrainptr
!
!  Sets up pointer to strains that can be strained
!
!   5/07 Created
!  12/07 Unused variables removed
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, December 2007
!
  use current
  use montecarlo
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)   :: i
  integer(i4)   :: ind
  integer(i4)   :: status
  logical, save :: lfirstime = .true.
!
!  Initialise memory on first call
!
  if (lfirstime) then
    allocate(nptrstrainable(6_i4),stat=status)
    if (status/=0) call outofmemory('setstrainptr','nptrstrainable')
    lfirstime = .false.
  endif
!
!  Setup pointer to atoms that can be moved
!
  nstrainable = 0
  do i = 1,nvar
    ind = iopt(i)
    if (ind.le.nstrains) then
      nstrainable = nstrainable + 1
      nptrstrainable(nstrainable) = ind
    endif
  enddo
!
  return
  end
