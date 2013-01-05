  subroutine setmoveptr
!
!  Sets up pointer to atoms that can be moved
!
!   1/01 Created
!   1/08 Modified to allow for use of tether option
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
  use optimisation, only : lopf
  use montecarlo
  use moldyn,       only : lfix
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)   :: i, ierror
  integer(i4)   :: status
  logical, save :: lfirstime = .true.
!
!  Initialise memory on first call
!
  if (lfirstime) then
    maxmoveable = numat
    allocate(nptrmoveable(maxmoveable),stat=status)
    if (status/=0) call outofmemory('setmoveptr','nptrmoveable')
    lfirstime = .false.
  endif
!
!  Setup pointer to atoms that can be moved
!
  nmoveable = 0
  do i = 1,numat
    if (lopf(i).and..not.lfix(i)) then
      nmoveable = nmoveable + 1
      if (nmoveable.gt.maxmoveable) then
        maxmoveable = nmoveable + 10
        call realloc(nptrmoveable,maxmoveable,ierror)
        if (ierror.ne.0) call outofmemory('setmoveptr','nptrmoveable')
      endif
      nptrmoveable(nmoveable) = i
    endif
  enddo
!
  return
  end
