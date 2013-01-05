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
!  Julian Gale, NRI, Curtin University, March 2007
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
  integer(i4)                                 :: ix
  integer(i4)                                 :: iy
  integer(i4)                                 :: iz
  integer(i4)                                 :: j
  integer(i4)                                 :: k
  integer(i4)                                 :: n
  integer(i4)                                 :: natd
  integer(i4)                                 :: nati
  integer(i4)                                 :: ncurr
  integer(i4)                                 :: nds
  integer(i4)                                 :: nmi
  integer(i4)                                 :: nmindi
  integer(i4)                                 :: nspecin
  integer(i4)                                 :: nt
  integer(i4)                                 :: ntypi
  integer(i4)                                 :: status
  logical,    dimension(:), allocatable       :: ler1
  logical                                     :: lfound
  logical                                     :: ldbi
  logical                                     :: ldqi
  real(dp)                                    :: oci
  real(dp)                                    :: qi
  real(dp)                                    :: radi
  real(dp)                                    :: xci
  real(dp)                                    :: yci
  real(dp)                                    :: zci
!
!  Allocate local memory
!
  allocate(ler1(ncfg),stat=status)
  if (status/=0) call outofmemory('setspec','ler1')
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
!  Loop over defect atoms for explicit region 1
!
  nds = 0
  do i = 1,ncfg
    ler1(i) = .false.
  enddo
  do i = 1,ndef
    if (ndeftyp(i).eq.0) then
      nds = nds + 1
      ler1(ndefcfg(i)) = .true.
    endif
  enddo
  if (nds.gt.0) then
    rewind(41)
    rewind(42)
    do n = 1,ncfg
      if (ler1(n)) then
        read(41) ncurr,nreg1
        if (ioproc) write(42) ncurr,nreg1
        do i = 1,nreg1
          read(41) nati,ntypi,xci,yci,zci,qi,radi,oci,nmi,nmindi,ldbi,ldqi
          if (lflags) then
            read(41) ix,iy,iz
          endif
          lfound = .false.
          j = 0
          do while (.not.lfound.and.j.lt.nspec)
            j = j + 1
            lfound = (nati.eq.natspec(j).and.(ntypi.eq.ntypspec(j).or.ntypspec(j).eq.0))
          enddo
          if (lfound) then
!
!  Set species properties from standard list if not added species
!
            if (j.le.nspecin) then
              qi = qlspec(j)
              if (radi.eq.0.0d0) radi = radspec(j)
              ldbi = (lbrspec(j).or.ldbi)
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
            natspec(nspec) = nati
            ntypspec(nspec) = ntypi
            qlspec(nspec) = qi
            radspec(nspec) = radi
            lbrspec(nspec) = ldbi
            lqinspec(nspec) = .false.
            if (natspec(nspec).le.maxele) then
              massspec(nspec) = atmass(natspec(nspec))
            else 
              massspec(nspec) = 0.0_dp
            endif
          endif
          if (ioproc) write(42) nati,ntypi,xci,yci,zci,qi,radi,oci,nmi,nmindi,ldbi,ldqi
          if (lflags.and.ioproc) then
            write(42) ix,iy,iz
          endif
        enddo
      endif
      if (ldeflin(n)) then
        read(41) nvaca,ninte
        if (ioproc) write(42) nvaca,ninte
        nt = nvaca + ninte
        read(41) (ndptr(k),k=1,nt)
        if (ioproc) write(42) (ndptr(k),k=1,nt)
      endif
      if (lreldin(n)) then
        read(41) (ndptr(k),k=1,nreg1)
        if (ioproc) write(42) (ndptr(k),k=1,nreg1)
      endif
    enddo
  endif
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
    if (nds.gt.0) call qdupdate(lflags,ler1)
  endif
!
!  Free local memory
!
  deallocate(ler1,stat=status)
  if (status/=0) call deallocate_error('setspec','ler1')
!
  return
  end
