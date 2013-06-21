  subroutine destroy(natom)
!
!  Destroys an atom and removes all data relating to it
!
!  On entry :
!
!  natom = atom in asymmetric unit whose symmetry related
!          images and itself will be removed
!
!   6/01 Pointer for moving atoms corrected
!  10/02 Correction to condensing of nbonded added
!   5/06 Handling of nspecptr added
!   4/07 Searching for bonded atoms now uses nbonds number rather
!        than checking for a non-zero type
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   5/11 Algorithm changed for handling of nbonds/nbonded/nbondind
!        to correct a bug.
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, May 2011
!
  use cellmultipole,  only : nboxat
  use configurations, only : lopfi
  use current
  use eam,            only : lMEAM, maxmeamcomponent
  use molecule,       only : natmol, nmolind
  use optimisation,   only : lopf
  use polarise,       only : dpolar, qpolar
  use potentialxyz
  use shell,          only : ncsptr, nshptr
  use sutton,         only : scrho, scrho12
  use velocities 
  implicit none
!
!  Passed variables
!
  integer(i4)                            :: natom
!
!  Local variables
!
  integer(i4)                            :: neqvatom
  integer(i4)                            :: i
  integer(i4)                            :: ii
  integer(i4)                            :: j
  integer(i4)                            :: k
  integer(i4)                            :: mvar
  integer(i4), dimension(:), allocatable :: iptr,itmp,itmp2
  integer(i4)                            :: status
  logical                                :: lreduce_nbonds
  logical,     dimension(:), allocatable :: ltmp
  logical,     dimension(:), allocatable :: ltmp2
  real(dp),    dimension(:), allocatable :: rtmp, rtmp2
!
!  Allocate local memory
!
  allocate(iptr(numat),stat=status)
  if (status/=0) call outofmemory('destroy','iptr')
  allocate(itmp(numat),stat=status)
  if (status/=0) call outofmemory('destroy','itmp')
  allocate(itmp2(numat),stat=status)
  if (status/=0) call outofmemory('destroy','itmp2')
  allocate(ltmp(numat),stat=status)
  if (status/=0) call outofmemory('destroy','ltmp')
  allocate(ltmp2(numat),stat=status)
  if (status/=0) call outofmemory('destroy','ltmp2')
  allocate(rtmp(numat),stat=status)
  if (status/=0) call outofmemory('destroy','rtmp')
  allocate(rtmp2(numat),stat=status)
  if (status/=0) call outofmemory('destroy','rtmp2')
!
!  Find all images in full cell and sort to the end
!
  neqvatom = neqv(natom)
  do i = 1,natom-1
    iptr(i) = i
  enddo
  do i = natom,numat-1
    iptr(i) = i + 1
  enddo
  iptr(numat) = natom
!
!  Sort bonding data before compression
!
!  Remove atoms no longer present from bonding list
!
  do i = 1,numat
    lreduce_nbonds = .false.
    do j = 1,nbonds(i)
      if (nbonded(j,i).eq.natom) then
        do k = j+1,nbonds(i)
          nbonded(k-1,i) = nbonded(k,i)
          nbondind(k-1,i) = nbondind(k,i)
        enddo
        lreduce_nbonds = .true.
      endif
    enddo
    if (lreduce_nbonds) then
      nbonds(i) = nbonds(i) - 1
    endif
  enddo
!
!  Move information in nbonded before nbonds is sorted
!
  do i = natom+1,numat
    do j = 1,nbonds(i)
      nbonded(j,i-1) = nbonded(j,i)
      nbondind(j,i-1) = nbondind(j,i)
    enddo
  enddo
!
!  Sort data
!
  call collect(numat,c6f(1),rtmp,iptr)
  call collect(numat,cnf(1),rtmp,iptr)
  call icollect(numat,icosx(1),itmp,iptr)
  call icollect(numat,icosy(1),itmp,iptr)
  call icollect(numat,icosz(1),itmp,iptr)
  call collect(numat,mass(1),rtmp,iptr)
  call icollect(numat,nat(1),itmp,iptr)
  call icollect(numat,natmol(1),itmp,iptr)
  call icollect(numat,nbonds(1),itmp,iptr)
  call icollect(numat,nboxat(1),itmp,iptr)
  call icollect(numat,ncsptr(1),itmp,iptr)
  call icollect(numat,nftype(1),itmp,iptr)
  call icollect(numat,nmolind(1),itmp,iptr)
  call icollect(numat,nrelat(1),itmp,iptr)
  call icollect(numat,nrotop(1),itmp,iptr)
  call icollect(numat,nshptr(1),itmp,iptr)
  call collect(numat,occuf(1),rtmp,iptr)
  call collect(numat,oxf(1),rtmp,iptr)
  call collect(numat,qf(1),rtmp,iptr)
  call collect(numat,radf(1),rtmp,iptr)
  call collect(numat,rmass(1),rtmp,iptr)
  call collect(numat,velx(1),rtmp,iptr)
  call collect(numat,vely(1),rtmp,iptr)
  call collect(numat,velz(1),rtmp,iptr)
  call collect(numat,xclat(1),rtmp,iptr)
  call collect(numat,yclat(1),rtmp,iptr)
  call collect(numat,zclat(1),rtmp,iptr)
  call collect(numat,xfrac(1),rtmp,iptr)
  call collect(numat,yfrac(1),rtmp,iptr)
  call collect(numat,zfrac(1),rtmp,iptr)
  call collect(numat,x2(1),rtmp,iptr)
  call collect(numat,y2(1),rtmp,iptr)
  call collect(numat,z2(1),rtmp,iptr)
  call collect(numat,x3(1),rtmp,iptr)
  call collect(numat,y3(1),rtmp,iptr)
  call collect(numat,z3(1),rtmp,iptr)
  call collect(numat,x4(1),rtmp,iptr)
  call collect(numat,y4(1),rtmp,iptr)
  call collect(numat,z4(1),rtmp,iptr)
  call collect(numat,x5(1),rtmp,iptr)
  call collect(numat,y5(1),rtmp,iptr)
  call collect(numat,z5(1),rtmp,iptr)
!
!  If atom in bonded list is greater than natom, then shift number down by 1
!
  do i = 1,numat-1
    do j = 1,nbonds(i)
      if (nbonded(j,i).gt.natom) nbonded(j,i) = nbonded(j,i) - 1
    enddo
  enddo
!
!  Move data to overwrite atom in asymmetric unit
!
  do i = 1,nasym-1
    iptr(i) = i
  enddo
  do i = natom,nasym-1
    iptr(i) = i + 1
  enddo
  iptr(nasym) = natom
!
!  Sort data
!
  call collect(nasym,c6a(1),rtmp,iptr)
  call collect(nasym,cna(1),rtmp,iptr)
  call icollect(nasym,iatn(1),itmp,iptr)
  call lcollect(nasym,lopf(1),ltmp,iptr)
  call icollect(nasym,natype(1),itmp,iptr)
  call icollect(nasym,nspecptr(1),itmp,iptr)
  call icollect(nasym,neqv(1),itmp,iptr)
  call icollect(nasym,nrel2(1),itmp,iptr)
  call collect(nasym,occua(1),rtmp,iptr)
  call collect(nasym,oxa(1),rtmp,iptr)
  call collect(nasym,dpolar(1),rtmp,iptr)
  call collect(nasym,qpolar(1),rtmp,iptr)
  call collect(nasym,qa(1),rtmp,iptr)
  call collect(nasym,rada(1),rtmp,iptr)
  call collect(nasym,vx(1),rtmp,iptr)
  call collect(nasym,vy(1),rtmp,iptr)
  call collect(nasym,vz(1),rtmp,iptr)
  call collect(nasym,vx12(1),rtmp,iptr)
  call collect(nasym,vy12(1),rtmp,iptr)
  call collect(nasym,vz12(1),rtmp,iptr)
!
  if (lMEAM) then
    do j = 1,maxmeamcomponent
      do i = 1,nasym
        rtmp2(i) = scrho(j,i)
      enddo
      call collect(nasym,rtmp2,rtmp,iptr)
      do i = 1,nasym
        scrho(j,i) = rtmp(i)
        rtmp2(i) = scrho12(j,i)
      enddo
      call collect(nasym,rtmp2,rtmp,iptr)
      do i = 1,nasym
        scrho12(j,i) = rtmp(i)
      enddo
    enddo
  else
    do i = 1,nasym
      rtmp2(i) = scrho(1,i)
    enddo
    call collect(nasym,rtmp2,rtmp,iptr)
    do i = 1,nasym
      scrho(1,i) = rtmp(i)
      rtmp2(i) = scrho12(1,i)
    enddo
    call collect(nasym,rtmp2,rtmp,iptr)
    do i = 1,nasym
      scrho12(1,i) = rtmp(i)
    enddo
  endif
!
  do i = 1,6
    do j = 1,nasym
      rtmp2(j) = v2xyz(i,j)
    enddo
    call collect(nasym,rtmp2,rtmp,iptr)
    do j = 1,nasym
      v2xyz(i,j) = rtmp(j)
    enddo
    do j = 1,nasym
      rtmp2(j) = v2xyz12(i,j)
    enddo
    call collect(nasym,rtmp2,rtmp,iptr)
    do j = 1,nasym
      v2xyz12(i,j) = rtmp(j)
    enddo
  enddo
  call collect(nasym,xalat(1),rtmp,iptr)
  call collect(nasym,yalat(1),rtmp,iptr)
  call collect(nasym,zalat(1),rtmp,iptr)
  call collect(nasym,xstore(1),rtmp,iptr)
  call collect(nasym,ystore(1),rtmp,iptr)
  call collect(nasym,zstore(1),rtmp,iptr)
  call collect(nasym,rstore(1),rtmp,iptr)
!
!  Optimisation variables - sort lopfi and rebuild iopt
!
  lopfi(1:3*nasym) = .false.
  do i = 1,nvar
    ii = iopt(i) - nstrains
    if (ii.gt.0) lopfi(ii) = .true.
  enddo
  do i = 1,nasym
    ltmp2(i) = lopfi(3*i-2)
  enddo
  call lcollect(nasym,ltmp2(1),ltmp,iptr)
  do i = 1,nasym
    lopfi(3*i-2) = ltmp2(i)
  enddo
  do i = 1,nasym
    ltmp2(i) = lopfi(3*i-1)
  enddo
  call lcollect(nasym,ltmp2(1),ltmp,iptr)
  do i = 1,nasym
    lopfi(3*i-1) = ltmp2(i)
  enddo
  do i = 1,nasym
    ltmp2(i) = lopfi(3*i)
  enddo
  call lcollect(nasym,ltmp2(1),ltmp,iptr)
  do i = 1,nasym
    lopfi(3*i) = ltmp2(i)
  enddo
  nvar = ncell
  do i = 1,3*nasym
    if (lopfi(i)) then
      nvar = nvar + 1
      iopt(nvar) = i + nstrains
    endif
  enddo
!
!  Coordinate variables
!
  do i = 1,nasym
    rtmp2(i) = x0(3*i-2+nstrains)
  enddo
  call collect(nasym,rtmp2(1),rtmp,iptr)
  do i = 1,nasym
    x0(3*i-2+nstrains) = rtmp2(i)
  enddo
  do i = 1,nasym
    rtmp2(i) = x0(3*i-1+nstrains)
  enddo
  call collect(nasym,rtmp2(1),rtmp,iptr)
  do i = 1,nasym
    x0(3*i-1+nstrains) = rtmp2(i)
  enddo
  do i = 1,nasym
    rtmp2(i) = x0(3*i+nstrains)
  enddo
  call collect(nasym,rtmp2(1),rtmp,iptr)
  do i = 1,nasym
    x0(3*i+nstrains) = rtmp2(i)
  enddo
!
  if (nbsm.gt.0) then
    mvar = nstrains + 3*nasym
    call icollect(nasym,iopt(mvar+1),itmp,iptr)
    call collect(nasym,x0(mvar+1),rtmp,iptr)
  endif
!
!  Correct totals
!
  nasym = nasym - 1
  numat = numat - neqvatom
!
!  Free local memory
!
  deallocate(rtmp2,stat=status)
  if (status/=0) call deallocate_error('destroy','rtmp2')
  deallocate(rtmp,stat=status)
  if (status/=0) call deallocate_error('destroy','rtmp')
  deallocate(ltmp2,stat=status)
  if (status/=0) call deallocate_error('destroy','ltmp2')
  deallocate(ltmp,stat=status)
  if (status/=0) call deallocate_error('destroy','ltmp')
  deallocate(itmp2,stat=status)
  if (status/=0) call deallocate_error('destroy','itmp2')
  deallocate(itmp,stat=status)
  if (status/=0) call deallocate_error('destroy','itmp')
  deallocate(iptr,stat=status)
  if (status/=0) call deallocate_error('destroy','iptr')
!
  return
  end
