  subroutine reorder(nc,iptr)
!
!  Reorders atoms according to a pointer which is passed as an argument
!
!  On entry :
!
!  nc   = configuration to reorder
!  iptr = pointer indicating the new atom number for each atom
!
!  11/01 Created from sort.f
!  11/01 Slice atom logical added to data to reorder
!   8/02 Reordering of forcecfg added
!   2/04 Reordering of time dependent force data added
!   4/04 Reordering of ltranat added
!   5/04 Reordering of NEB final coordinates added
!   8/04 Trap for zero atom case added
!   5/06 Handling of nspecptr added
!  11/06 Reordering of NEB final coordinates added
!  11/06 nebfinalradius added
!   2/07 Connectivity information reordering added
!   5/07 Out of bounds check added for connectivity atom numbers
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
!  Julian Gale, NRI, Curtin University, April 2008
!
  use control
  use configurations
  use current
  use defects
  use element, only : maxele
  use moldyn,  only : lfix, lmdconstrain, nmdconstrainatom
  use molecule
  use neb,     only : nebfinalxyz, nebfinalradius
  use observables
  use projectdos
  use scan,    only : ltranat
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: nc
  integer(i4)                                  :: iptr(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ij
  integer(i4)                                  :: ind
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: n
  integer(i4)                                  :: naddi
  integer(i4)                                  :: naddj
  integer(i4)                                  :: ni
  integer(i4)                                  :: ni2
  integer(i4)                                  :: nj
  integer(i4)                                  :: nj2
  integer(i4)                                  :: noffset
  integer(i4)                                  :: np
  integer(i4)                                  :: npc
  integer(i4)                                  :: npi
  integer(i4)                                  :: npfirst
  integer(i4)                                  :: nplast
  integer(i4)                                  :: npifirst
  integer(i4)                                  :: npilast
  integer(i4)                                  :: nsum
  integer(i4), dimension(:), allocatable       :: nrel2sorted
  integer(i4)                                  :: status
  logical,     dimension(:), allocatable       :: ltmp
  real(dp),    dimension(:), allocatable       :: rtmp
  real(dp),    dimension(:), allocatable       :: tmp
!
!  Get number of atoms for this configuration
!
  nasym = nascfg(nc)
!
!  If number of atoms is zero then return as there is nothing to do
!
  if (nasym.eq.0) return
!
!  Allocate local memory
!
  allocate(itmp(nasym),stat=status)
  if (status/=0) call outofmemory('reorder','itmp')
  allocate(rtmp(nasym),stat=status)
  if (status/=0) call outofmemory('reorder','rtmp')
  allocate(tmp(nasym),stat=status)
  if (status/=0) call outofmemory('reorder','tmp')
  allocate(ltmp(3*nasym),stat=status)
  if (status/=0) call outofmemory('reorder','ltmp')
!
!  Swap simple atom referenced data around
!
  call icollect(nasym,natcfg(nsft+1),itmp,iptr)
  call icollect(nasym,nregionno(nsft+1),itmp,iptr)
  call icollect(nasym,ntypcfg(nsft+1),itmp,iptr)
  call icollect(nasym,nspecptrcfg(nsft+1),itmp,iptr)
  call collect(nasym,cncfg(nsft+1),tmp,iptr)
  call collect(nasym,xcfg(nsft+1),tmp,iptr)
  call collect(nasym,ycfg(nsft+1),tmp,iptr)
  call collect(nasym,zcfg(nsft+1),tmp,iptr)
  call collect(nasym,qlcfg(nsft+1),tmp,iptr)
  call collect(nasym,occucfg(nsft+1),tmp,iptr)
  call collect(nasym,oxcfg(nsft+1),tmp,iptr)
  call collect(nasym,radcfg(nsft+1),tmp,iptr)
  call lcollect(nasym,lbsmat(nsft+1),ltmp,iptr)
  call lcollect(nasym,lqmatom(nsft+1),ltmp,iptr)
  call lcollect(nasym,lsliceatom(nsft+1),ltmp,iptr)
  call lcollect(nasym,ltranat(nsft+1),ltmp,iptr)
  call lcollect(nasym,lfix(nsft+1),ltmp,iptr)
!
!  Swap data with more complex dependendancy on the atom numbers
!
!  Change MD constraint if present
!
  if (lmdconstrain(nc)) then
    nmdconstrainatom(1,nc) = iptr(nmdconstrainatom(1,nc))
    nmdconstrainatom(2,nc) = iptr(nmdconstrainatom(2,nc))
  endif
!
!  Change around lopfi
!
  noffset = 3*nsft
  do i = 1,3*nasym
    ltmp(i) = lopfi(noffset+i)
  enddo
  do i = 1,nasym
    lopfi(noffset+3*(i-1)+1) = ltmp(3*(iptr(i)-1)+1)
    lopfi(noffset+3*(i-1)+2) = ltmp(3*(iptr(i)-1)+2)
    lopfi(noffset+3*(i-1)+3) = ltmp(3*(iptr(i)-1)+3)
  enddo
!
!  Change NEB final coordinates and radii
!
  do i = 1,3
    do j = 1,nasym
      rtmp(j) = nebfinalxyz(i,j,nc)
    enddo
    call collect(nasym,rtmp,tmp,iptr)
    do j = 1,nasym
      nebfinalxyz(i,j,nc) = rtmp(j)
    enddo
  enddo
  do j = 1,nasym
    rtmp(j) = nebfinalradius(j,nc)
  enddo
  call collect(nasym,rtmp,tmp,iptr)
  do j = 1,nasym
    nebfinalradius(j,nc) = rtmp(j)
  enddo
!
!  Change external forces
!
  do i = 1,3
    do j = 1,nasym
      rtmp(j) = forcecfg(i,nsft+j)
    enddo
    call collect(nasym,rtmp,tmp,iptr)
    do j = 1,nasym
      forcecfg(i,nsft+j) = rtmp(j)
    enddo
  enddo
  do i = 1,3
    do j = 1,nasym
      ltmp(j) = ltdforcecfg(i,nsft+j)
    enddo
    call lcollect(nasym,ltmp,ltmp(nasym+1),iptr)
    do j = 1,nasym
      ltdforcecfg(i,nsft+j) = ltmp(j)
    enddo
  enddo
  do i = 1,3
    do k = 1,3
      do j = 1,nasym
        rtmp(j) = tdforcecfg(k,i,nsft+j)
      enddo
      call collect(nasym,rtmp,tmp,iptr)
      do j = 1,nasym
        tdforcecfg(k,i,nsft+j) = rtmp(j)
      enddo
    enddo
  enddo
!
!  Change defect atom pointers
!
  do i = 1,ndef
    if (ndefcfg(i).eq.nc) then
      if (ndeftyp(i).eq.1.or.ndeftyp(i).eq.11.or.ndeftyp(i).eq.12) then
        ni = nint(xdef(i))
        do k = 1,nasym
          if (iptr(k).eq.ni) then
            xdef(i) = k
          endif
        enddo
      endif
    endif
  enddo
!
!  Change defect centre pointer
!
  if (ndcentyp(nc).eq.1.or.ndcentyp(nc).eq.2) then
    ni = nint(xdcent(nc))
    do k = 1,nasym
      if (iptr(k).eq.ni) then
        xdcent(nc) = k
      endif
    enddo
  endif
!
!  Change fitted gradient pointers
!
  do i = 1,nfgrad
    if (nfgracfg(i).eq.nc) then
      ni = nfgrat(i)
      do k = 1,nasym
        if (iptr(k).eq.ni) then
          nfgrat(i) = k
        endif
      enddo
    endif
  enddo
!
!  Change any constraints
!
  if (ncontot.gt.0) then
    do i = 1,ncontot
      if (nconcfg(i).eq.nc) then
        ind = ncfixcfg(i) - nstrains
        if (ind.gt.3*nasym) then
!
!  Breathing shell constraint
!
          ii = ind - 3*nasym
          ind = ncvarcfg(i) - nstrains
          ij = ind - 3*nasym
          do j = 1,nasym
            if (ii.eq.iptr(j)) then
              ncfixcfg(i) = ncfixcfg(i) + j - ii
            elseif (ij.eq.iptr(j)) then
              ncvarcfg(i) = ncvarcfg(i) +j - ij
            endif
          enddo
        else
!
!  Coordinate constraint
!
          if (ind.gt.0) then
            ii = 1 + (ind-1)/3
          else
            ii = 0
          endif
          ind = ncvarcfg(i) - nstrains
          if (ind.gt.0) then
            ij = 1 + (ind-1)/3
          else
            ij = 0
          endif
          if (ii.gt.0.or.ij.gt.0) then
           do j = 1,nasym
              if (ii.eq.iptr(j)) then
                ncfixcfg(i) = ncfixcfg(i) + 3*(j-ii)
              elseif (ij.eq.iptr(j)) then
                ncvarcfg(i) = ncvarcfg(i) + 3*(j-ij)
              endif
            enddo
          endif
        endif
      endif
    enddo
  endif
!
!  Change connectivity pointers
!
  if (nconnect.gt.0) then
    if (nasym.ne.numat) then
!
!  Count position of asymmetric unit atoms in full cell for symmetry case
!
      allocate(nrel2sorted(nasym),stat=status)
      if (status/=0) call outofmemory('reorder','nrel2sorted')
      do i = 1,nasym
        nrel2sorted(iptr(i)) = neqv(i)
      enddo
      nsum = 0
      do i = 1,nasym
        ni2 = nrel2sorted(i)
        nrel2sorted(i) = nsum + 1
        nsum = nsum + ni2
      enddo
    endif
    do n = 1,nconnect
      if (nconnectcfg(n).eq.nc) then
        i = n1connect(n)
        j = n2connect(n)
!
!  Check that i & j are in bounds
!
        if (i.gt.numat.or.j.gt.numat) then
          call outerror('Atom number in connectivity is greater than number of atoms',0_i4)
          call stopnow('reorder')
        endif
        if (nasym.eq.numat) then
!
!  Simple no symmetry sort
!
          n1connect(n) = iptr(i)
          n2connect(n) = iptr(j)
        else
!
!  More complex symmetry adapted sort
!
          ni = nrelat(i)
          nj = nrelat(j)
          ni2 = iptr(ni)
          nj2 = iptr(nj)
          naddi = i - nrel2(ni)
          naddj = j - nrel2(nj)
          n1connect(n) = nrel2sorted(ni2) + naddi
          n2connect(n) = nrel2sorted(nj2) + naddj
        endif
      endif
    enddo
    if (nasym.ne.numat) then
      deallocate(nrel2sorted,stat=status)
      if (status/=0) call deallocate_error('reorder','nrel2sorted')
    endif
  endif
!
!  Change weights - code not correct because of cell constraints
!  also may not be worth including as people work out which
!  observable to weight by observable table
!
!      if (lfit) then
!        nlower = noffset + nobs + 6*nc
!        nupper = nlower + 3*nasym
!        do i = nlower+1,nupper
!          tmp(i-nlower) = weight(i)
!        enddo
!        do i = 1,nasym
!          weight(nlower+3*(i-1)+1) = tmp(3*(iptr(i)-1)+1)
!          weight(nlower+3*(i-1)+2) = tmp(3*(iptr(i)-1)+2)
!          weight(nlower+3*(i-1)+3) = tmp(3*(iptr(i)-1)+3)
!        enddo
!      endif
!
!  Change projections
!
  if ((nprojcfg(nc)-nprojdef(nc)).gt.0) then
    npfirst = 1
    npifirst = 1
    ii = 0
    do i = 1,nc-1
      npc = nprojcfg(i)
      npfirst = npfirst + npc
      do j = 1,npc
        npifirst = npifirst + nprojit(ii+j)
      enddo
      ii = ii + npc
    enddo
    nplast = npfirst + nprojcfg(nc) - 1
    npilast = npifirst
    do i = 1,nprojcfg(nc)
      npilast = npilast + nprojit(ii+i)
    enddo
    npilast = npilast - 1
    do np = npfirst,nplast
      if (nprojdb(np).eq.1) then
        do npi = npifirst,npilast
          if (nprojptr(npi).eq.np) then
            if (nprojtyp(npi).gt.99) then
              ind = nprojnat(npi)
              do k = 1,nasym
                if (iptr(k).eq.ind) nprojnat(npi) = k
              enddo
            endif
          endif
        enddo
      endif
    enddo
  endif
!
!  Free local memory
!
  deallocate(ltmp,stat=status)
  if (status/=0) call deallocate_error('reorder','ltmp')
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('reorder','tmp')
  deallocate(rtmp,stat=status)
  if (status/=0) call deallocate_error('reorder','rtmp')
  deallocate(itmp,stat=status)
  if (status/=0) call deallocate_error('reorder','itmp')
!
  return
  end
