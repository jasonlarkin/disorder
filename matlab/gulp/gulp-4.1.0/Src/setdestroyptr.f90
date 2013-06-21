  subroutine setdestroyptr
!
!  Sets up point to atoms that can be destroyed
!
!   1/01 Created
!  12/04 Counting of total number of destroyable entities added
!  12/04 Setting of lfound in now based on ngcmcspec & ngcmcmol
!  11/06 Checking on matching stoichiometry added to molecule
!        destruction setup
!   7/07 lgcmcmol added
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
!  Julian Gale, NRI, Curtin University, July 2007
!
  use current
  use molecule
  use montecarlo
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: ierror
  integer(i4)                :: j
  integer(i4)                :: nm
  integer(i4)                :: status
  logical                    :: lfound
  logical,              save :: lfirstime = .true.
!
!  Initialise memory on first call
!
  if (lfirstime) then
    maxdestroyable = numat
    allocate(nptrdestroyable(maxdestroyable),stat=status)
    if (status/=0) call outofmemory('setdestroyptr','nptrdestroyable')
    lfirstime = .false.
  endif
!
!  Setup pointer to atoms that can be destroyed
!
  ndestroyable = 0
  ndestroyablemol = 0
!
! NB: Here we need to check that all atoms in the molecule are GCMC species,
! that the total number of atoms in the molecule is correct, and that all 
! the atoms in the molecule have been matched
! 
  do i = 1,numat
    lfound = .false.
!
!  Find whether this atom is part of a GCMC molecule
!
    nm = natmol(i)
    lfound = lgcmcmol(nm)
!
!  Search for whether this atom is a GCMC species
!
    if (.not.lfound.and.ngcmcspec.gt.0) then
      j = 0
      do while (j.lt.ngcmcspec.and..not.lfound)
        j = j + 1
        lfound = (nat(i).eq.ngcmcnat(j).and.(nftype(i).eq.ngcmctype(j).or.ngcmctype(j).eq.0))
      enddo
    endif
    if (lfound) then
      ndestroyable = ndestroyable + 1
      if (ndestroyable.gt.maxdestroyable) then
        maxdestroyable = ndestroyable + 10
        call realloc(nptrdestroyable,maxdestroyable,ierror)
        if (ierror.ne.0) call outofmemory('setdestroyptr','nptrdestroyable')
      endif
      nptrdestroyable(ndestroyable) = i
    endif
  enddo
!
!  Count total number of destroyable molecules
!
  do i = 1,nmol
    if (lgcmcmol(i)) then
      ndestroyablemol = ndestroyablemol + 1
    endif
  enddo
!
  return
  end
