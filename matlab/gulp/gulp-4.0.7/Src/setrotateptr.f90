  subroutine setrotateptr
!
!  Sets up pointer to molecules that can be rotated
!
!   1/01 Created
!   6/05 Style updated
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
  use molecule
  use optimisation, only : lopf
  use moldyn,       only : lfix
  use molecule
  use montecarlo
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)                        :: i, ii, ierror, status
  logical, dimension(:), allocatable :: lrotateable
  logical,                      save :: lfirstime = .true.
!
!  Initialise memory on first call
!
  if (lfirstime) then
    maxrotateable = max(nmol,1)
    allocate(nptrrotateable(maxrotateable),stat=status)
    if (status/=0) call outofmemory('setrotateptr','nptrrotateable')
    lfirstime = .false.
  endif
!
!  Check that there are molecules otherwise there is nothing to do
!
  if (nmol.eq.0) then
    nrotateable = 0
    return
  endif
!
!  Check array dimension
!
  if (nmol.gt.maxrotateable) then
    maxrotateable = nmol
    call realloc(nptrrotateable,maxrotateable,ierror)
    if (ierror.ne.0) call outofmemory('setrotateptr','nptrrotateable')
  endif
!
!  Create local logical array to indicate whether a molecule is rotateable
!
  allocate(lrotateable(nmol),stat=status)
  if (status/=0) call outofmemory('setrotateptr','lrotateable')
  lrotateable(1:nmol) = .true.
!
!  Setup pointer to molecules that can be rotated
!
!  All atoms in the molecule must be marked as moveable
!
  do i = 1,numat
    ii = natmol(i)
    if (ii.gt.0.and.(.not.lopf(i).or.lfix(i))) then
      lrotateable(ii) = .false.
    endif
  enddo
!
!  Count number of molecules that are free to be rotated
!
  nrotateable = 0
  do i = 1,nmol
    if (lrotateable(i)) then
      nrotateable = nrotateable + 1
      nptrrotateable(nrotateable) = i
    endif
  enddo
!
!  Free local memory
!
  deallocate(lrotateable,stat=status)
  if (status/=0) call deallocate_error('setrotateptr','lrotateable')
!
  return
  end
