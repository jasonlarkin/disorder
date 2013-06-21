  subroutine mcmove(mode)
!
!  MC routine for translation of atoms/molecules. If
!  atom in a molecule is chosen then all atoms within
!  molecule are translated.
!
!  mode = if mode = 1, choose atoms to apply move to
!         if mode = 2, then create new trial displacements
!         if mode = 3, then undo previous displacements
!
!   1/01 Created
!   9/04 Random displacements scale into fractional units were
!        needed for the system type
!   9/04 When atoms are moved it is ensured that their centroid
!        stays in the central cell.
!  12/05 Bug fixed for ndim = 0
!   5/07 lmodco & connectunwrap option introduced
!   6/07 Set up of dmaxx/y/z now done on every call in case
!        strains are being applied
!   6/07 Restriction on maximum move size added to keep things
!        sensible in periodic boundary conditions.
!  12/07 nptrmove nullified
!   1/08 Saving of cell indices added in case of move not being accepted
!        Code that ensures atoms stay within the cell removed for nomod
!        case since this can cause problems for molecules
!        Call to connectunwrap removed since this is no longer needed 
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
  use reallocate
!
!  Passed variables
!
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                  :: mode
!
!  Local variables
!
  integer(i4), dimension(:), pointer, save :: nmolindsave => null()
  logical,                            save :: lfirstcall = .true.
  real(dp),                           save :: deltax = 0.0_dp
  real(dp),                           save :: deltay = 0.0_dp
  real(dp),                           save :: deltaz = 0.0_dp
  real(dp),                           save :: dmaxx
  real(dp),                           save :: dmaxy
  real(dp),                           save :: dmaxz
!
  integer(i4)                              :: i
  integer(i4)                              :: ierror
  integer(i4)                              :: ii
  integer(i4)                              :: natom
  integer(i4)                              :: nmolmove
  integer(i4)                              :: nmove
  integer(i4)                              :: status
  real(dp)                                 :: randnum
  real(dp)                                 :: GULP_random
!
!  Allocate local memory on first call only - needs to preserved
!  subsequently for undoing of trial steps.
!
  if (lfirstcall) then
    lfirstcall = .false.
    allocate(nmolindsave(maxtrialatom),stat=status)
    if (status/=0) call outofmemory('mcmove','nmolindsave')
  endif
  if (mode.eq.3) then
!*************************************
!  Mode 3 : Undo last displacements  *
!*************************************
    do i = 1,ntrialatom
      ii = nptrtrialatom(i)
      nmolind(ii) = nmolindsave(i)
      x0(3*ii+nstrains-2) = x0(3*ii+nstrains-2) - deltax
      x0(3*ii+nstrains-1) = x0(3*ii+nstrains-1) - deltay
      x0(3*ii+nstrains)   = x0(3*ii+nstrains)   - deltaz
    enddo
  elseif (mode.eq.1) then
!*******************************
!  Mode 1 : New displacements  *
!*******************************
!
!  Choose atom / molecule to move
!
    randnum = GULP_random(iseed,1_i4)
    nmove = nmoveable*randnum + 1_i4
    if (nmove.gt.nmoveable) nmove = nmoveable
    natom = nptrmoveable(nmove)
!
!  Is atom in a molecule? If so build list of all atoms to move
!
    nmolmove = natmol(natom)
    if (nmolmove.gt.0) then
      ntrialatom = 0
      do i = 1,nmoveable
        if (natmol(nptrmoveable(i)).eq.nmolmove) then
          ntrialatom = ntrialatom + 1
          if (ntrialatom.gt.maxtrialatom) then
            maxtrialatom = ntrialatom + 10
            call changemaxtrialatom
            call realloc(nmolindsave,maxtrialatom,ierror)
            if (ierror.ne.0) call outofmemory('mcmove','nmolindsave')
          endif
          nptrtrialatom(ntrialatom) = nptrmoveable(i)
        endif
      enddo
    else
      ntrialatom = 1
      nptrtrialatom(1) = natom
    endif
  elseif (mode.eq.2) then
!*********************************
!  Mode 2 : Apply displacements  *
!*********************************
!
!  Set bounds of allowed displacements
!
    if (ndim.eq.3) then
      dmaxx = dmaxmc/a
      dmaxy = dmaxmc/b
      dmaxz = dmaxmc/c
      dmaxx = min(dmaxx,0.5_dp)
      dmaxy = min(dmaxy,0.5_dp)
      dmaxz = min(dmaxz,0.5_dp)
    elseif (ndim.eq.2) then
      dmaxx = dmaxmc/a
      dmaxy = dmaxmc/b
      dmaxz = dmaxmc
      dmaxx = min(dmaxx,0.5_dp)
      dmaxy = min(dmaxy,0.5_dp)
    elseif (ndim.eq.1) then
      dmaxx = dmaxmc/a
      dmaxy = dmaxmc
      dmaxz = dmaxmc
      dmaxx = min(dmaxx,0.5_dp)
    elseif (ndim.eq.0) then
      dmaxx = dmaxmc
      dmaxy = dmaxmc
      dmaxz = dmaxmc
    endif
!
!  Find displacements to apply
!
    deltax = dmaxx*GULP_random(iseed,2_i4)
    deltay = dmaxy*GULP_random(iseed,2_i4)
    deltaz = dmaxz*GULP_random(iseed,2_i4)
!
!  Apply displacements to configuration array
!
    do i = 1,ntrialatom
      ii = nptrtrialatom(i)
      nmolindsave(i) = nmolind(ii)
      x0(3*ii+nstrains-2) = x0(3*ii+nstrains-2) + deltax
      x0(3*ii+nstrains-1) = x0(3*ii+nstrains-1) + deltay
      x0(3*ii+nstrains)   = x0(3*ii+nstrains)   + deltaz
    enddo
  endif
!
  return
  end
