  subroutine setmoldef(lcreated)
!
!  Determines number of molecules and their constituent atoms
!
!   7/00 lcreated now explicitly passed from setdef
!   7/05 Memory deallocation cleaned 
!   8/06 Pointers nullified on first call
!   1/07 Modified for noautobond option
!   5/08 Defect bonding array structure changed
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
!  Julian Gale, NRI, Curtin University, May 2008
!
  use current
  use defects
  use element
  use molecule
  use parallel
  use reallocate
  implicit none
!
!  Passed variables
!
  logical                                      :: lcreated(*)
!
!  Local variables
!
  integer(i4), dimension(:), pointer,     save :: nbat
  integer(i4), dimension(:), pointer,     save :: nmlist
  integer(i4), dimension(:), allocatable       :: nexist
  integer(i4), dimension(:), allocatable       :: nptr
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ierror
  integer(i4)                                  :: ii
  integer(i4)                                  :: iii
  integer(i4)                                  :: indb
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: l
  integer(i4)                                  :: na
  integer(i4)                                  :: nb1
  integer(i4)                                  :: nb2
  integer(i4)                                  :: nbond
  integer(i4)                                  :: ni
  integer(i4)                                  :: ni1
  integer(i4)                                  :: ninc
  integer(i4)                                  :: ninter
  integer(i4)                                  :: nj
  integer(i4)                                  :: nj1
  integer(i4)                                  :: nlow
  integer(i4)                                  :: nml
  integer(i4)                                  :: nmolfin
  integer(i4)                                  :: nmolold
  integer(i4)                                  :: nti
  integer(i4)                                  :: nti1
  integer(i4)                                  :: ntj
  integer(i4)                                  :: ntj1
  integer(i4)                                  :: status
  logical                                      :: lbondok
  logical                                      :: lfind
  logical,                                save :: firstcall = .true.
  real(dp)                                     :: rcut
  real(dp)                                     :: ri
  real(dp)                                     :: rij
  real(dp)                                     :: rj
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  lbondok = .true.
!
!  Nullify pointers on first call
!
  if (firstcall) then
    nullify(nbat)
    nullify(nmlist)
    firstcall = .false.
  endif
!
!  Allocate local arrays
!
  call realloc(nbat,maxbond,ierror)
  if (ierror.ne.0) call outofmemory('setmoldef','nbat')
  call realloc(nmlist,maxbond,ierror)
  if (ierror.ne.0) call outofmemory('setmoldef','nmlist')
  allocate(nexist(nreg1),stat=status)
  if (status/=0) call outofmemory('setmoldef','nexist')
  allocate(nptr(nreg1),stat=status)
  if (status/=0) call outofmemory('setmoldef','nptr')
!
!  Initialisation
!
  ninter = 0
  do i = 1,nreg1
    if (lcreated(i)) then
      ninter = ninter + 1
      nptr(ninter) = i
      ndefmol(i) = 0
      ndefind(i) = 0
    endif
    nbondsdef(i) = 0
    do j = 1,maxbond
      nbondeddef(j,i) = 0
    enddo
  enddo
!********************************************************
!  Check if interstitials belong to existing molecules  *
!********************************************************
  do i = 1,nreg1
    xal = xdefe(i)
    yal = ydefe(i)
    zal = zdefe(i)
    ni1 = natdefe(i)
    nti = ntypdefe(i)
    ni = ni1
    if (ni.gt.maxele) ni = ni-maxele
    ri = rcov(ni)
!
!  Find all atoms bonded to atom i
!
    if (.not.lnoautobond.and.ri.ne.0.0_dp) then
      nbond = 0
      do j = 1,nreg1
        xcd = xdefe(j)
        ycd = ydefe(j)
        zcd = zdefe(j)
        nj1 = natdefe(j)
        ntj = ntypdefe(j)
        nj = nj1
        if (nj.gt.maxele) nj = nj - maxele
        rj = rcov(nj)
!
!  Check whether bond type is excluded
!
        if (nnobo.gt.0) then
          if (ni1.eq.nj1) then
            indb = nj1 + 1000*ni1
            if (nti.lt.ntj) then
              nti1 = nti
              ntj1 = ntj
            else
              nti1 = ntj
              ntj1 = nti
            endif
          elseif (ni1.lt.nj1) then
            indb = nj1 + 1000*ni1
            nti1 = nti
            ntj1 = ntj
          else
            indb = ni1 + 1000*nj1
            nti1 = ntj
            ntj1 = nti
          endif
          lbondok = .true.
          ii = 1
          do while (lbondok.and.(ii.le.nnobo))
            if (indb.eq.nobond(ii)) then
              nb1 = nobotyp(ii)/1000
              nb2 = nobotyp(ii) - 1000*nb1
              if ((nb1.eq.nti1.or.nb1.eq.0).and.(nb2.eq.ntj1.or.nb2.eq.0)) lbondok = .false.
              if (ni1.eq.nj1.and.lbondok) then
                if ((nb1.eq.ntj1.or.nb1.eq.0).and.(nb2.eq.nti1.or.nb2.eq.0)) lbondok = .false.
              endif
            endif
            ii = ii + 1
          enddo
        endif
!
!  Distance check
!
        if (rj.ne.0.0_dp.and.i.ne.j.and.lbondok) then
          rcut = rtol*(ri + rj)
          rcut = rcut*rcut
          if (rj.ne.0.0_dp) then
            xcrd = xcd - xal
            ycrd = ycd - yal
            zcrd = zcd - zal
            rij = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
            if (rij.le.rcut.and.rij.gt.0.0_dp) then
!
!  Valid bond
!
              if (lcreated(i).and.ndefmol(j).gt.0) then
                ndefmol(i) = ndefmol(j)
                ndefind(i) = ndefind(j)
              endif
              nbondsdef(i) = nbondsdef(i) + 1
              if (nbondsdef(i).gt.maxbond) then
                maxbond = nbondsdef(i) + 2
                call changemaxbond
                call realloc(nbat,maxbond,ierror)
                if (ierror.ne.0) call outofmemory('setmoldef','nbat')
                call realloc(nmlist,maxbond,ierror)
                if (ierror.ne.0) call outofmemory('setmoldef','nmlist')
              endif
              nbondeddef(nbondsdef(i),i) = j
            endif
          endif
        endif
      enddo
    endif
  enddo
!
!  Find residual interstitials that are unassigned
!
  ninter = 0
  do i = 1,nreg1
    if (lcreated(i).and.ndefmol(i).eq.0) then
      ninter = ninter + 1
      nptr(ninter) = i
    endif
  enddo
  if (ninter.eq.0) goto 999
!***************************************************
!  Check for new molecules amoungst interstitials  *
!***************************************************
  nmolold = nmol
  nmol = 0
  do i = 1,ninter
    ii = nptr(i)
    xal = xdefe(ii)
    yal = ydefe(ii)
    zal = zdefe(ii)
    ni1 = natdefe(ii)
    nti = ntypdefe(ii)
    ni = ni1
    if (ni.gt.maxele) ni = ni - maxele
    ri = rcov(ni)
!
!  Find all interstitials bonded to atom i
!
    if (.not.lnoautobond.and.ri.ne.0.0_dp) then
      nbond = 0
      do j = 1,ninter
        jj = nptr(j)
        xcd = xdefe(jj)
        ycd = ydefe(jj)
        zcd = zdefe(jj)
        nj1 = natdefe(jj)
        ntj = ntypdefe(jj)
        nj = nj1
        if (nj.gt.maxele) nj = nj - maxele
        rj = rcov(nj)
!
!  Check whether bond type is excluded
!
        if (nnobo.gt.0) then
          if (ni1.eq.nj1) then
            indb = nj1 + 1000*ni1
            if (nti.lt.ntj) then
              nti1 = nti
              ntj1 = ntj
            else
              nti1 = ntj
              ntj1 = nti
            endif
          elseif (ni1.lt.nj1) then
            indb = nj1 + 1000*ni1
            nti1 = nti
            ntj1 = ntj
          else
            indb = ni1 + 1000*nj1
            nti1 = ntj
            ntj1 = nti
          endif
          lbondok = .true.
          iii = 1
          do while (lbondok.and.(iii.le.nnobo))
            if (indb.eq.nobond(iii)) then
              nb1 = nobotyp(iii)/1000
              nb2 = nobotyp(iii) - 1000*nb1
              if ((nb1.eq.nti1.or.nb1.eq.0).and.(nb2.eq.ntj1.or.nb2.eq.0)) lbondok = .false.
              if (ni1.eq.nj1.and.lbondok) then
                if ((nb1.eq.ntj1.or.nb1.eq.0).and.(nb2.eq.nti1.or.nb2.eq.0)) lbondok = .false.
              endif
            endif
            iii = iii + 1
          enddo
        endif
!
!  Distance check
!
        if (rj.ne.0.0_dp.and.i.ne.j.and.lbondok) then
          rcut = rtol*(ri + rj)
          rcut = rcut*rcut
          if (rj.ne.0.0_dp) then
            xcrd = xcd - xal
            ycrd = ycd - yal
            zcrd = zcd - zal
            rij = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
            if (rij.le.rcut.and.rij.gt.0.0_dp) then
!
!  Valid bond
!
              nbond = nbond + 1
              if (nbond.gt.maxbond) then
                maxbond  =  nbond  +  2
                call changemaxbond
                call realloc(nbat,maxbond,ierror)
                if (ierror.ne.0) call outofmemory('setmoldef','nbat')
                call realloc(nmlist,maxbond,ierror)
                if (ierror.ne.0) call outofmemory('setmoldef','nmlist')
              endif
              nbat(nbond) = jj
            endif
          endif
        endif
      enddo
      if (nbond.gt.0) then
!
!  Include reference atom i in list
!
        nbond = nbond + 1
        if (nbond.gt.maxbond) then
          maxbond  =  nbond  +  2
          call changemaxbond
          call realloc(nbat,maxbond,ierror)
          if (ierror.ne.0) call outofmemory('setmoldef','nbat')
          call realloc(nmlist,maxbond,ierror)
          if (ierror.ne.0) call outofmemory('setmoldef','nmlist')
        endif
        nbat(nbond) = ii
!
!  Make a list of molecule numbers
!
        nml = 0
        do k = 1,nbond
          na = ndefmol(nbat(k))
          if (na.ne.0) then
            nml = nml + 1
            nmlist(nml) = na
          endif
        enddo
!
!  Check if any of the bonded atoms are in molecule already
!  and find lowest molecule number
!
        nlow = 100
        do k = 1,nbond
          na = ndefmol(nbat(k))
          if (na.lt.nlow.and.na.gt.0) nlow = na
        enddo
!
!  If connected atom is already classified apply lowest number to all
!  atoms in molecule else create new molecule. Also apply to all atoms
!  with same molecule number everywhere.
!
        if (nlow.eq.100) then
          nmol = nmol + 1
          nlow = nmol
        endif
!
!  Do bonded atoms first to makesure they are located
!
        do k = 1,nbond
          na = nbat(k)
          ndefmol(na) = nlow
          ndefind(na) = 555
        enddo
!
!  Do other previously designated atoms which may be linked
!  to atoms altered in the current pass.
!
        do k = 1,ninter
          kk = nptr(k)
          lfind = .false.
          do l = 1,nml
            if (ndefmol(kk).eq.nmlist(l)) lfind = .true.
          enddo
          if (lfind) then
            ndefmol(kk) = nlow              
            ndefind(kk) = 555
          endif
        enddo
      endif
    endif
  enddo
!************************************
!  Sift out non-existent molecules  *
!************************************
  do i = 1,nmol
    ninc = 0
    do j = 1,ninter
      if (ndefmol(nptr(j)).eq.i) then
        ninc = ninc + 1
      endif
    enddo
    if (ninc.gt.0) then
      nexist(i) = 1
    else
      nexist(i) = 0
    endif
  enddo
  nmolfin = 0
  do i = 1,nmol
    if (nexist(i).gt.0) then
      nmolfin = nmolfin + 1
    else
      do j = 1,ninter
        jj = nptr(j)
        if (ndefmol(jj).gt.i) then
          ndefmol(jj) = ndefmol(jj) - 1
        endif
      enddo
    endif
  enddo
  nmol = nmolfin + nmolold
!
!  Set dimensionality for new molecules - must be equal to 0
!
  do i = nmolold + 1,nmol
    moldim(i) = 0
    moldimi(i) = 0
  enddo
!
!  Need to output new molecules found
!
!
!  Add nmolold to molecule numbers
!
  do i = 1,ninter
    if (ndefmol(nptr(i)).ne.0) then
      ndefmol(nptr(i)) = ndefmol(nptr(i)) + nmolold
    endif
  enddo
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local arrays
!
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('setmoldef','nptr')
  deallocate(nexist,stat=status)
  if (status/=0) call deallocate_error('setmoldef','nexist')
  call realloc(nbat,0_i4,ierror)
  call realloc(nmlist,0_i4,ierror)
!
  return
  end
