  subroutine mcdestroy(mode)
!
!  MC routine for destruction of atoms/molecules.
!  If an atom is chosen which is in a molecule then the
!  whole molecule must be removed.
!
!  mode = if mode = 1, choose atoms to destroy
!         if mode = 2, then create new trial destructions
!         if mode = 3, then undo previous destructions
!
!   1/01 Created
!   6/01 Order of destructions reversed to avoid correction
!        of pointer after atoms are sorted in destroy.
!  10/02 Molecule handling now fixed up
!  11/04 ntodestroy etc moved into module
!  12/04 Handling of shells corrected
!   1/08 Extra mode added to pick atoms
!   1/08 Common array now used to store pointer to trial atoms
!   1/08 random -> GULP_random
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
  use general
  use genetic, only : iseed
  use molecule
  use montecarlo
  use parallel
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                  :: mode
!
!  Local variables
!
  integer(i4), dimension(:), pointer       :: naddbackptr => null()
  integer(i4),                        save :: originalnasym = 0
  integer(i4),                        save :: originalnewmol = 0
  integer(i4),                        save :: originalnmol = 0
  integer(i4),                        save :: originalnumat = 0
!
  integer(i4)                              :: i
  integer(i4)                              :: ii
  integer(i4)                              :: j
  integer(i4)                              :: naddback
  integer(i4)                              :: natom
  integer(i4),                        save :: nmoldestroy
  integer(i4)                              :: ndestroy
  integer(i4)                              :: ntodestroyold
  integer(i4)                              :: status
  logical                                  :: lfound
  real(dp)                                 :: randnum
  real(dp)                                 :: GULP_random
!
  if (mode.eq.3) then
!************************************
!  Mode 3 : Replace original atoms  *
!************************************
!
!  Assumes that data was just rearranged in destroy, not overwritten,
!  except for molecules where connectivity must be rebuilt to allow
!  for shuffle.
!
    if (nmol.ne.originalnmol) then
      naddback = originalnumat - numat
      allocate(naddbackptr(naddback),stat=status)
      if (status/=0) call outofmemory('mcdestroy','naddbackptr')
      do i = 1,naddback
        naddbackptr(i) = numat + i
      enddo
      call mcmolconnect(naddback,naddbackptr,originalnewmol,.false.)
      deallocate(naddbackptr,stat=status)
      if (status/=0) call deallocate_error('mcdestroy','naddbackptr')
    endif
    numat = originalnumat
    nasym = originalnasym
  elseif (mode.eq.1) then
!***********************************
!  Mode 1 : Find atoms to destroy  *
!***********************************
!
!  Save numbers of atoms from before destruction
!
    originalnumat = numat
    originalnasym = nasym
    originalnmol  = nmol 
!
!  Choose atom / molecule to destroy
!
    randnum = GULP_random(iseed,1_i4)
    ndestroy = ndestroyable*randnum + 1_i4
    if (ndestroy.gt.ndestroyable) ndestroy = ndestroyable
    natom = nptrdestroyable(ndestroy)
!
!  Is atom in a molecule? If so build list of all atoms to destroy
!
    nmoldestroy = natmol(natom)
    if (nmoldestroy.gt.0) then
      ntrialatom = 0
      originalnewmol = molgcmc(nmoldestroy)
      do i = 1,ndestroyable
        if (natmol(nptrdestroyable(i)).eq.nmoldestroy) then
          ntrialatom = ntrialatom + 1
          if (ntrialatom.gt.maxtrialatom) then
            maxtrialatom = ntrialatom + 10
            call changemaxtrialatom
          endif
          nptrtrialatom(ntrialatom) = nptrdestroyable(i)
        endif
      enddo
    else
      ntrialatom = 1
      nptrtrialatom(1) = natom
    endif
!
!  Check for attached cores/shells to destroy
!
    if (nshell.gt.0) then
      ntodestroyold = ntrialatom
      do ii = 1,ntodestroyold
        i = nptrtrialatom(ii)
        if (ncsptr(i).gt.0) then
          lfound = .false.
          j = 0
          if (j.lt.ndestroyable.and..not.lfound) then
            j = j + 1
            lfound = (nptrdestroyable(j).eq.ncsptr(i))
          endif
          if (.not.lfound) then
            ntrialatom = ntrialatom + 1
            if (ntrialatom.gt.maxtrialatom) then
              maxtrialatom = ntrialatom + 10
              call changemaxtrialatom
            endif
            nptrtrialatom(ntrialatom) = ncsptr(i)
          endif
        endif
      enddo
    endif
  elseif (mode.eq.2) then
!***************************
!  Mode 2 : Destroy atoms  *
!***************************
!
!  Apply destructions in reverse order to avoid atoms to be destroyed from having moved!
!
    do i = ntrialatom,1,-1
      ii = nptrtrialatom(i)
      call destroy(ii)
    enddo
    if (nmoldestroy.gt.0) then
!
!  Decrease number of molecules and correct molecule numbers for remaining molecules
!
      nmol = nmol - 1
      do i = 1,numat
        if (natmol(i).gt.nmoldestroy) then
          natmol(i) = natmol(i) - 1
          moldim(natmol(i)) = moldim(natmol(i+1))
          moldimi(natmol(i)) = moldimi(natmol(i+1))
          molgcmc(natmol(i)) = molgcmc(natmol(i+1))
        endif
      enddo
    endif
  endif
!
  return
  end
