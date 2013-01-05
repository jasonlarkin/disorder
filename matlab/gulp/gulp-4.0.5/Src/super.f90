  subroutine super
!
!  Generate supercell
!
!   1/93 Initially created
!   7/95 Check for periodicity is moved to this routine
!  12/00 Modified to handle surface supercells
!   5/02 1-D case enabled
!   4/04 Manipulation of region/slice labels added
!   1/05 Scaling of sbulkecfg added
!   5/06 Expansion of species pointer added
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, May 2006
!
  use configurations
  use control
  use current
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: isfct
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: kk
  integer(i4)                                  :: nbv
  integer(i4)                                  :: nnew
  integer(i4)                                  :: nstart2
  integer(i4)                                  :: ntotal
  integer(i4), dimension(:), allocatable       :: ntemp
  logical,     dimension(:), allocatable       :: ltemp
  logical,     dimension(:), allocatable       :: ltemp2
  integer(i4)                                  :: status
  real(dp)                                     :: rix
  real(dp)                                     :: riy
  real(dp)                                     :: riz
  real(dp)                                     :: xadd
  real(dp)                                     :: yadd
  real(dp)                                     :: zadd
!
!  Check that system is periodic
!
  if (ndim.lt.1) then
    call outerror('supercell cannot be specified for a cluster',0_i4)
    call stopnow('super')
  endif
!
!  Convert supercell index into components
!
  ind = nsuper(ncf)
  ix = ind/10000
  ind = ind - 10000*ix
  iy = ind/100
  iz = ind - 100*iy
!
!  Check to see whether the atomic arrays need re-allocating
!
  isfct = ix*iy*iz
  ntotal = numat*isfct
  nnew = ntotal - numat
  if (ntotal.gt.maxat) then
    maxat = ntotal
    call changemaxat
  endif
  if (nnew+nasum.gt.maxatot) then
    maxatot = nnew + nasum
    call changemaxatot
  endif
  nasym = nascfg(ncf)
  nsft = 0
  do i = 1,ncf-1
    nsft = nsft + nascfg(i)
  enddo
  nstart2 = nsft + nasym + 1
!
!  Move following configurations to make space
!
  do i = nasum,nstart2,-1
    natcfg(i+nnew) = natcfg(i)
    ntypcfg(i+nnew) = ntypcfg(i)
    nspecptrcfg(i+nnew) = nspecptrcfg(i)
    qlcfg(i+nnew) = qlcfg(i)
    occucfg(i+nnew) = occucfg(i)
    radcfg(i+nnew) = radcfg(i)
    nregionno(i+nnew) = nregionno(i)
    lbsmat(i+nnew) = lbsmat(i)
    lsliceatom(i+nnew) = lsliceatom(i)
    xcfg(i+nnew) = xcfg(i)
    ycfg(i+nnew) = ycfg(i)
    zcfg(i+nnew) = zcfg(i)
  enddo
!
!  Ion attributes
!
  ind = nsft - isfct
  allocate(ltemp(numat),stat=status)
  if (status/=0) call outofmemory('super','ltemp')
  allocate(ltemp2(numat),stat=status)
  if (status/=0) call outofmemory('super','ltemp2')
  allocate(ntemp(numat),stat=status)
  if (status/=0) call outofmemory('super','rtemp')
  do i = 1,numat
    ltemp(i) = lbsmat(i+nsft)
    ltemp2(i) = lsliceatom(i+nsft)
    ntemp(i) = nregionno(i+nsft)
  enddo
  do i = 1,numat
    ind = ind + isfct
    do j = 1,isfct
      natcfg(j+ind) = nat(i)
      ntypcfg(j+ind) = nftype(i)
      nspecptrcfg(j+ind) = nspecptr(i)
      qlcfg(j+ind) = qf(i)
      occucfg(j+ind) = occuf(i)
      radcfg(j+ind) = radf(i)
      nregionno(j+ind) = ntemp(nrelat(i))
      lbsmat(j+ind) = ltemp(nrelat(i))
      lsliceatom(j+ind) = ltemp2(nrelat(i))
      xcfg(j+ind) = xfrac(i)
      ycfg(j+ind) = yfrac(i)
      zcfg(j+ind) = zfrac(i)
    enddo
  enddo
  deallocate(ntemp,stat=status)
  if (status/=0) call deallocate_error('super','rtemp')
  deallocate(ltemp2,stat=status)
  if (status/=0) call deallocate_error('super','ltemp2')
  deallocate(ltemp,stat=status)
  if (status/=0) call deallocate_error('super','ltemp')
!
!  Coordinates
!  Correct fractional coordinates for new cell size
!
  ind = nsft
  rix = 1.0_dp/ix
  riy = 1.0_dp/iy
  riz = 1.0_dp/iz
  do i = 1,numat
    do ii = 1,ix
      xadd = rix*(ii-1)
      do jj = 1,iy
        do kk = 1,iz
          ind = ind + 1
          xcfg(ind) = rix*xfrac(i) + xadd
        enddo
      enddo
    enddo
  enddo
  if (ndim.ge.2) then
    ind = nsft
    do i = 1,numat
      do ii = 1,ix
        do jj = 1,iy
          yadd = riy*(jj-1)
          do kk = 1,iz
            ind = ind + 1
            ycfg(ind) = riy*yfrac(i) + yadd
          enddo
        enddo
      enddo
    enddo
  endif
  if (ndim.eq.3) then
    ind = nsft
    do i = 1,numat
      do ii = 1,ix
        do jj = 1,iy
          do kk = 1,iz
            zadd = riz*(kk-1)
            ind = ind + 1
            zcfg(ind) = riz*zfrac(i) + zadd
          enddo
        enddo
      enddo
    enddo
  endif
!
!  Expand optimisation flags
!
  if (.not.lconp.and..not.lconv) then
    allocate(ltemp(3*numat),stat=status)
    if (status/=0) call outofmemory('super','ltemp')
    nbv = 3*(nstart2 - 1) + 1
    do i = 3*nasum,nbv,-1
      lopfi(i+3*nnew) = lopfi(i)
    enddo
    ind = 3*nsft
    do i = 1,3*numat
      ltemp(i) = lopfi(ind+i)
    enddo
    ind = ind - 3
    do i = 1,numat
      indi = 3*(i-1)
      do ii = 1,isfct
        ind = ind + 3
        lopfi(ind+1) = ltemp(indi+1)
        lopfi(ind+2) = ltemp(indi+2)
        lopfi(ind+3) = ltemp(indi+3)
      enddo
    enddo
    deallocate(ltemp,stat=status)
    if (status/=0) call deallocate_error('super','ltemp')
  endif
!
!  Change number of atoms and set space group to P1
!
  nascfg(ncf) = ntotal
  nasum = nasum + nnew
!
!  Change cell to primitive cell
!
  do j = 1,3
    rvcfg(j,1,ncf) = ix*rvcfg(j,1,ncf)
    rvcfg(j,2,ncf) = iy*rvcfg(j,2,ncf)
    rvcfg(j,3,ncf) = iz*rvcfg(j,3,ncf)
  enddo
!
!  Scale bulk energy
!
  sbulkecfg(ncf) = isfct*sbulkecfg(ncf)
!
  return
  end
