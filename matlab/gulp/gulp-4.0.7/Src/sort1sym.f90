  subroutine sort1sym(iop)
!
!  Sort all region 1 ions such that symmetry equivalent
!  images are adjacent.
!
!   7/00 iop made into argument passed in 
!   6/05 Order of deallocation reversed
!  12/07 Unused variables removed
!   5/08 Defect bonding array structure changed
!
!  Julian Gale, NRI, Curtin University, May 2008
!
  use current
  use defects
  use molecule
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: iop(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: iptr
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4), dimension(:), allocatable       :: itmp2
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4), dimension(:), allocatable       :: nptr
  integer(i4)                                  :: nri
  integer(i4)                                  :: status
  logical                                      :: lfound
  logical,     dimension(:), allocatable       :: ltmp
  real(dp),    dimension(:), allocatable       :: tmp
!
!  Allocate local memory
!
  allocate(nptr(nreg1),stat=status)
  if (status/=0) call outofmemory('sort1sym','nptr')
  allocate(itmp(nreg1),stat=status)
  if (status/=0) call outofmemory('sort1sym','itmp')
  allocate(itmp2(nreg1),stat=status)
  if (status/=0) call outofmemory('sort1sym','itmp2')
  allocate(tmp(nreg1),stat=status)
  if (status/=0) call outofmemory('sort1sym','tmp')
  allocate(ltmp(3*nreg1),stat=status)
  if (status/=0) call outofmemory('sort1sym','ltmp')
!
  do i = 1,nreg1
    nptr(i) = 0
  enddo
!**********************************************
!  Order region 1 by symmetry related images  *
!**********************************************
!
!  Reset ndsptr
!
  ii = 1
  do i = 1,ndasym
    ndsptr(i) = ii
    ii = ii + ndeqv(i)
  enddo
!
!  Create pointer to correct storage element
!
  do i = 1,nreg1
    nri = ndrel(i)
    iptr = ndsptr(nri)
    lfound = .false.
    do while (.not.lfound)
      if (nptr(iptr).eq.0) then
        lfound = .true.
        nptr(iptr) = i
      else
        iptr = iptr + 1
      endif
    enddo
  enddo
!
!  Perform shuffle
!
  call icollect(nreg1,natdefe,itmp,nptr)
  call icollect(nreg1,ntypdefe,itmp,nptr)
  call icollect(nreg1,ndefmol,itmp,nptr)
  call icollect(nreg1,ndefind,itmp,nptr)
  call icollect(nreg1,ndrel,itmp,nptr)
  call icollect(nreg1,ndrelop,itmp,nptr)
  call icollect(nreg1,nreldef,itmp,nptr)
  call collect(nreg1,xdefe,tmp,nptr)
  call collect(nreg1,ydefe,tmp,nptr)
  call collect(nreg1,zdefe,tmp,nptr)
  call collect(nreg1,qdefe,tmp,nptr)
  call collect(nreg1,occdefe,tmp,nptr)
  call collect(nreg1,radefe,tmp,nptr)
  call lcollect(nreg1,ldefbsmat,ltmp,nptr)
!
!  Change around iop
!
  do i = 1,3
    do j = 1,nreg1
      itmp2(j) = iop(3*(j-1)+i)
    enddo
    call icollect(nreg1,itmp2,itmp,nptr)
    do j = 1,nreg1
      iop(3*(j-1)+i) = itmp2(j)
    enddo
  enddo
!
!  Change around bonding list and adjust bonded atom numbers
!
  call icollect(nreg1,nbondsdef,itmp,nptr)
  do i = 1,maxbond
    do j = 1,nreg1
      itmp2(j) = nbondeddef(i,j)
    enddo
    call icollect(nreg1,itmp2,itmp,nptr)
    do j = 1,nreg1
      k = 1
      lfound = .false.
      do while (.not.lfound.and.k.le.nreg1)
        if (itmp2(j).eq.nptr(k)) then
          nbondeddef(i,j) = k
          lfound = .true.
        endif
        k = k + 1
      enddo
    enddo
  enddo
!
!  Free local memory
!
  deallocate(ltmp,stat=status)
  if (status/=0) call deallocate_error('sort1sym','ltmp')
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('sort1sym','tmp')
  deallocate(itmp2,stat=status)
  if (status/=0) call deallocate_error('sort1sym','itmp2')
  deallocate(itmp,stat=status)
  if (status/=0) call deallocate_error('sort1sym','itmp')
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('sort1sym','nptr')
!
  return
  end
