  subroutine setspec
!
!  Set up species arrays
!
!   5/95 lmask added to protect charges
!   3/97 Correction added to set charges of species which
!        are added to the list, where the charge is given
!        by a previous species
!   7/00 lflags now placed in control module
!   1/01 checking of GCMC species added
!   2/01 checking of GCMC molecule species added
!   9/02 species not added if generic (type=0) species is present
!  11/02 setting of lbsmat corrected
!   7/05 Pointer from atoms to species number added
!   8/05 Impurity and interstitial species added to list and
!        bug in species setting for GCMC fixed
!   5/06 Handling of species mass now added
!   3/07 Keyword to preserve individual charges added
!   3/13 Modified as scratch files are no longer used for defect
!        information. Includes removal of call to qdupdate. 
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use configurations
  use control
  use current
  use defects
  use element,    only : maxele, atmass
  use montecarlo
  use species
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: j
  integer(i4)                                 :: k
  integer(i4)                                 :: natd
  integer(i4)                                 :: nspecin
  logical                                     :: lfound
!
!  For each species in the configuration list check whether
!  it is present in the species list
!
  nspecin = nspec
  do i = 1,nspecin
    lmask(i) = .true.
  enddo
!
!  Loop over bulk atoms
!
  do i = 1,nasum
    lfound = .false.
    j = 0
    do while (.not.lfound.and.j.lt.nspec)
      j = j + 1
      lfound = (natcfg(i).eq.natspec(j).and.(ntypcfg(i).eq.ntypspec(j).or.ntypspec(j).eq.0))
    enddo
    if (lfound) then
!
!  Set species properties from standard list if not added species
!
!  Note : for breathing shell species don't override definitions set in structure input
!
      if (j.le.nspecin.and.lqinspec(j)) then
        if (.not.lpreserveQ) qlcfg(i) = qlspec(j)
        if (radcfg(i).eq.0.0_dp) radcfg(i) = radspec(j)
        if (.not.lbsmat(i)) lbsmat(i) = lbrspec(j)
      endif
!
!  Set pointer from atom to species
!
      nspecptrcfg(i) = j
    else
!
!  New species - add to list
!
      nspec = nspec + 1
      if (nspec.gt.maxspec) then
        maxspec = nspec + 20
        call changemaxspec
      endif
      natspec(nspec) = natcfg(i)
      ntypspec(nspec) = ntypcfg(i)
      qlspec(nspec) = qlcfg(i)
      radspec(nspec) = radcfg(i)
      lbrspec(nspec) = lbsmat(i)
      lqinspec(nspec) = .false.
      if (natspec(nspec).le.maxele) then
        massspec(nspec) = atmass(natspec(nspec))
      else 
        massspec(nspec) = 0.0_dp
      endif
!
!  Loop over previous species to check whether charge should be
!  set by any of these as qlcfg may not contain anything
!
      do j = 1,nspec - 1
        if (natspec(nspec).eq.natspec(j).and.ntypspec(j).eq.0) then
          qlspec(nspec) = qlspec(j)
        endif
      enddo
!
!  Set pointer from atom to species
!
      nspecptrcfg(i) = nspec
    endif
  enddo
!
!  Loop over GCMC species
!
  do i = 1,ngcmcspec
    lfound = .false.
    j = 0
    do while (.not.lfound.and.j.lt.nspec)
      j = j + 1
      lfound = (ngcmcnat(i).eq.natspec(j).and.ngcmctype(i).eq.ntypspec(j))
    enddo
    if (.not.lfound) then
!
!  New species - add to list
!
      nspec = nspec + 1
      if (nspec.gt.maxspec) then
        maxspec = nspec + 20
        call changemaxspec
      endif
      natspec(nspec) = ngcmcnat(i)
      ntypspec(nspec) = ngcmctype(i)
      qlspec(nspec) = 0.0_dp
      radspec(nspec) = 0.0_dp
      lbrspec(nspec) = .false.
      lqinspec(nspec) = .false.
      if (natspec(nspec).le.maxele) then
        massspec(nspec) = atmass(natspec(nspec))
      else 
        massspec(nspec) = 0.0_dp
      endif
!
!  Loop over previous species to check whether charge should be
!  set by any of these as qlcfg may not contain anything
!
      do j = 1,nspec - 1
        if (natspec(nspec).eq.natspec(j).and.ntypspec(j).eq.0) then
          qlspec(nspec) = qlspec(j)
        endif
      enddo
    endif
  enddo
!
!  Loop over GCMC molecule species
!
  do i = 1,ngcmcmol
    do j = 1,ngcmcmolat(i)
      lfound = .false.
      k = 0
      do while (.not.lfound.and.k.lt.nspec)
        k = k + 1
        lfound = (ngcmcmolnat(j,i).eq.natspec(k).and.ngcmcmoltype(j,i).eq.ntypspec(k))
      enddo
      if (.not.lfound) then
!
!  New species - add to list
!
        nspec = nspec + 1
        if (nspec.gt.maxspec) then
          maxspec = nspec + 20
          call changemaxspec
        endif
        natspec(nspec) = ngcmcmolnat(j,i)
        ntypspec(nspec) = ngcmcmoltype(j,i)
        qlspec(nspec) = 0.0_dp
        radspec(nspec) = 0.0_dp
        lbrspec(nspec) = .false.
        lqinspec(nspec) = .false.
        if (natspec(nspec).le.maxele) then
          massspec(nspec) = atmass(natspec(nspec))
        else 
          massspec(nspec) = 0.0_dp
        endif
!
!  Loop over previous species to check whether charge should be
!  set by any of these as qlcfg may not contain anything
!
        do k = 1,nspec - 1
          if (natspec(nspec).eq.natspec(k).and.ntypspec(k).eq.0) then
            qlspec(nspec) = qlspec(k)
          endif
        enddo
      endif
    enddo
  enddo
!
!  Loop over defect atoms to species properties
!
  do i = 1,nreg1tot
    lfound = .false.
    j = 0
    do while (.not.lfound.and.j.lt.nspec)
      j = j + 1
      lfound = (natdefecfg(i).eq.natspec(j).and.(ntypdefecfg(i).eq.ntypspec(j).or.ntypspec(j).eq.0))
    enddo
    if (lfound) then
!
!  Set species properties from standard list if not added species
!
      if (j.le.nspecin) then
        qdefecfg(i) = qlspec(j)
        if (radefecfg(i).eq.0.0_dp) radefecfg(i) = radspec(j)
        ldefbsmatcfg(i) = (lbrspec(j).or.ldefbsmatcfg(i))
      endif
    else
!
!  New species - add to list
!
      nspec = nspec + 1
      if (nspec.gt.maxspec) then
        maxspec = nspec + 20
        call changemaxspec
      endif
      natspec(nspec) = natdefecfg(i)
      ntypspec(nspec) = ntypdefecfg(i)
      qlspec(nspec) = qdefecfg(i)
      radspec(nspec) = radefecfg(i)
      lbrspec(nspec) = ldefbsmatcfg(i)
      lqinspec(nspec) = .false.
      if (natspec(nspec).le.maxele) then
        massspec(nspec) = atmass(natspec(nspec))
      else
        massspec(nspec) = 0.0_dp
      endif
    endif
  enddo
!
!  Loop over impurity or interstitial species
!
  do i = 1,ndef
    lfound = .false.
    j = 0
    if (ndefnat(i).gt.2*maxele) then
      natd = ndefnat(i) - 2*maxele
    else
      natd = ndefnat(i)
    endif
!
!  If there is no species associated with this defect ndefnat will be zero
!  => set lfound to true to skip adding to species list
!
    if (natd.eq.0) then
      lfound = .true.
    endif
    do while (.not.lfound.and.j.lt.nspec)
      j = j + 1
      lfound = (natd.eq.natspec(j).and.ndeftp(i).eq.ntypspec(j))
    enddo
    if (.not.lfound) then
!
!  New species - add to list
!
      nspec = nspec + 1
      if (nspec.gt.maxspec) then
        maxspec = nspec + 20
        call changemaxspec
      endif
      natspec(nspec) = natd
      ntypspec(nspec) = ndeftp(i)
      qlspec(nspec) = 0.0_dp
      radspec(nspec) = 0.0_dp
      lbrspec(nspec) = .false.
      lqinspec(nspec) = .false.
      if (natspec(nspec).le.maxele) then
        massspec(nspec) = atmass(natspec(nspec))
      else 
        massspec(nspec) = 0.0_dp
      endif
!
!  Loop over previous species to check whether charge should be
!  set by any of these as qlcfg may not contain anything
!
      do j = 1,nspec - 1
        if (natspec(nspec).eq.natspec(j).and.ntypspec(j).eq.0) then
          qlspec(nspec) = qlspec(j)
        endif
      enddo
!
!  Now handle possible complementary core or shell
!
      lfound = .false.
      j = 0
      if (natd.lt.maxele) then
        natd = natd + maxele
      else
        natd = natd - maxele
      endif
      do while (.not.lfound.and.j.lt.nspec)
        j = j + 1
        lfound = (natd.eq.natspec(j).and.ndeftp(i).eq.ntypspec(j))
      enddo
      if (.not.lfound) then
!
!  New species - add to list
!
        nspec = nspec + 1
        if (nspec.gt.maxspec) then
          maxspec = nspec + 20
          call changemaxspec
        endif
        natspec(nspec) = natd
        ntypspec(nspec) = ndeftp(i)
        qlspec(nspec) = 0.0_dp
        radspec(nspec) = 0.0_dp
        lbrspec(nspec) = .false.
        lqinspec(nspec) = .false.
        if (natspec(nspec).le.maxele) then
          massspec(nspec) = atmass(natspec(nspec))
        else 
          massspec(nspec) = 0.0_dp
          ldefshspec(nspec) = .true.
        endif
!
!  Loop over previous species to check whether charge should be
!  set by any of these as qlcfg may not contain anything
!
        do j = 1,nspec - 1
          if (natspec(nspec).eq.natspec(j).and.ntypspec(j).eq.0) then
            qlspec(nspec) = qlspec(j)
          endif
        enddo
      endif
    endif
  enddo
!
  do i = nspecin+1,nspec
    lmask(i) = .false.
  enddo
!
!  Update charges of ions
!
  if (.not.lpreserveQ) then
    call qupdate
!
!  Update defect charges
!
    do i = 1,nreg1tot
      do j = 1,nspec
        if (lmask(j)) then
          if (natdefecfg(i).eq.natspec(j).and.(ntypdefecfg(i).eq.ntypspec(j).or.ntypspec(j).eq.0)) qdefecfg(i) = qlspec(j)
        endif
      enddo
    enddo
  endif
!
  return
  end
