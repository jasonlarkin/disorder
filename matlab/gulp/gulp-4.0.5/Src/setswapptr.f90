  subroutine setswapptr
!
!  Sets up pointer to atoms that can be swapped
!
!   1/09 Created from setmoveptr
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
!  Julian Gale, NRI, Curtin University, January 2009
!
  use current
  use montecarlo
  use molecule,     only : natmol
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)   :: i, ierror
  integer(i4)   :: status
  logical, save :: lfirstime = .true.
  logical       :: lmatchany
  logical       :: lmatched
!
!  Initialise memory on first call
!
  if (lfirstime) then
    maxswapable = numat
    allocate(nptrswapable(maxswapable),stat=status)
    if (status/=0) call outofmemory('setswapptr','nptrswapable')
    lfirstime = .false.
  endif
!
!  Setup pointer to atoms that can be swapped 
!
  nswapable = 0
  do i = 1,numat
!
!  Exclude atoms in molecules
!
    if (natmol(i).eq.0) then
!
      if (lmcswapany) then
        nswapable = nswapable + 1
        if (nswapable.gt.maxswapable) then
          maxswapable = nswapable + 10
          call realloc(nptrswapable,maxswapable,ierror)
          if (ierror.ne.0) call outofmemory('setswapptr','nptrswapable')
        endif
        nptrswapable(nswapable) = i
      else
        lmatched = lmatchany(nat(i),nftype(i),nmcswapspec,nmcswapnat,nmcswaptype)
        if (lmatched) then
          nswapable = nswapable + 1
          if (nswapable.gt.maxswapable) then
            maxswapable = nswapable + 10
            call realloc(nptrswapable,maxswapable,ierror)
            if (ierror.ne.0) call outofmemory('setswapptr','nptrswapable')
          endif
          nptrswapable(nswapable) = i
        endif
      endif
    endif
  enddo
!
  return
  end
