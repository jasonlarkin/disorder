  subroutine mccreate(mode)
!
!  MC routine for creation of atoms/molecules.
!
!   1/01 Created
!   2/01 Creation of molecules added
!   7/04 Order of destroy operations reversed
!  11/04 ncreated / nptrcreated made into global variables
!  11/04 Checking of maxcreated added
!  11/06 When looking for species for gcmc types, the type number 
!        must now match exactly.
!   7/07 Flag added so that we remember to remove a molecule that
!        was created but not accepted
!   1/08 Extra dummy mode added 
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
  use general
  use genetic,    only : iseed
  use molecule,   only : nmol
  use montecarlo
  use parallel
  use species
  implicit none
!
!  Passed variables
!
  integer(i4)                              :: mode
!
!  Local variables
!
  logical,                            save :: lmoleculecreated = .false.
!
  integer(i4)                              :: i
  integer(i4)                              :: j
  integer(i4)                              :: newfullspecies
  integer(i4)                              :: newmol
  integer(i4)                              :: newspecies
  real(dp)                                 :: randnum
  real(dp)                                 :: GULP_random
!
  if (mode.eq.3) then
!**********************************
!  Mode 3 : Remove created atoms  *
!**********************************
!
!  Note that the atoms should be destroyed in reverse order
!  to avoid the atom pointer exceeding the remaining number 
!  of atoms in the system.
!
    do i = ntrialatom,1,-1
      call destroy(nptrtrialatom(i))
    enddo
!
!  If a molecule was created then remove it
!
    if (lmoleculecreated) nmol = nmol - 1
  elseif (mode.eq.1) then
!*************************************
!  Mode 1 : Dummy call - do nothing  *
!*************************************
    return
  else
!**************************
!  Mode 2 : New creation  *
!**************************
!
!  Choose species to create from list of species and list of GCMC molecules
!
    randnum = GULP_random(iseed,1_i4)
    newspecies = (ngcmcspec+ngcmcmol)*randnum + 1_i4
    if (newspecies.gt.(ngcmcspec+ngcmcmol)) newspecies = ngcmcspec + ngcmcmol
!
    if (newspecies.le.ngcmcspec) then
!***********************
!  Create new species  *
!***********************
      lmoleculecreated = .false.
!
!  Translate new species number from GCMC species to full species
!
      newfullspecies = 0
      i = 0
      do while (i.lt.nspec.and.newfullspecies.eq.0)
        i = i + 1
        if (natspec(i).eq.ngcmcnat(newspecies).and.ntypspec(i).eq.ngcmctype(newspecies)) &
          newfullspecies = i
      enddo
!
!  Check that something was found
!
      if (newfullspecies.eq.0) then
        call outerror('GCMC species not in full species list',0_i4)
        call stopnow('mccreate')
      endif
!
!  Create atom and set pointer to new atom
!
      ntrialatom = 1
      do i = 1,ntrialatom
        call create(newfullspecies,nptrtrialatom(i))
      enddo
    else
!************************
!  Create new molecule  *
!************************
      lmoleculecreated = .true.
      newmol = newspecies - ngcmcspec
      ntrialatom = 0
!
!  Loop over all atoms in new molecule to check that species are present 
!
      do i = 1,ngcmcmolat(newmol)
!
!  Translate new species number from GCMC species to full species
!
        newfullspecies = 0
        j = 0
        do while (j.lt.nspec.and.newfullspecies.eq.0)
          j = j + 1
          if (natspec(j).eq.ngcmcmolnat(i,newmol).and.ntypspec(j).eq.ngcmcmoltype(i,newmol)) &
            newfullspecies = j
        enddo
!
!  Check that something was found
!
        if (newfullspecies.eq.0) then
          call outerror('GCMC species not in full species list',0_i4)
          call stopnow('mccreate')
        endif
!
!  Create atom and set pointer to new atom
!
        ntrialatom = ntrialatom + 1
        if (ntrialatom.gt.maxtrialatom) then
          maxtrialatom = ntrialatom + 10
          call changemaxtrialatom
        endif
        call create(newfullspecies,nptrtrialatom(i))
!
      enddo
!
!  Having created the atoms for the molecule, now manipulate
!  the coordinates to be those of the molecule randomly
!  inserted.
!
      call mcmolinsert(newmol)
    endif
  endif
!
  return
  end
