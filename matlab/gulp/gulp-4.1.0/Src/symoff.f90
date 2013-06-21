  subroutine symoff
!
!  Turn off symmetry after initial generation 
!  Rearrange data in configuration arrays
!
!  lfull    =  generate full cell after removal of symmetry instead
!            of primitive form
!
!   3/98 ltranat adjusted for symmetry removal
!  12/03 Modified to handle manual space group operator input
!   5/06 Handling of nspecptr added
!   6/09 Modified for charge as a coordinate option
!   2/10 Allocated sizes of ltemp/ltemp2 changed to nasym rather than
!        numat.
!   7/11 Modified to handle symmetry turning off of fractional coordinates
!        in electrostatic potential sites.
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
!  Julian Gale, NRI, Curtin University, July 2011
!
  use configurations
  use control
  use current
  use potentialsites,  only : npotsitecfg, npotsites, xpotsite, ypotsite, zpotsite
  use scan
  use symmetry
  implicit none
!
!  Local variables
!
  character(len=1)                                  :: p1(16)
  integer(i4)                                       :: i
  integer(i4)                                       :: iadd
  integer(i4)                                       :: icf
  integer(i4)                                       :: ifail
  integer(i4)                                       :: ind
  integer(i4)                                       :: indi
  integer(i4)                                       :: j
  integer(i4)                                       :: nbv
  integer(i4)                                       :: nnew
  integer(i4)                                       :: nri
  integer(i4)                                       :: nstart2
  integer(i4)                                       :: status
  logical                                           :: lfull
  logical,          dimension(:), allocatable       :: ltemp
  logical,          dimension(:), allocatable       :: ltemp2
  real(dp)                                          :: rr(6)
  real(dp)                                          :: rvf(3,3)
  real(dp)                                          :: rvt(3,3)
  real(dp)                                          :: v(3)
  real(dp)                                          :: x(3)
  real(dp)                                          :: xt(3)
  real(dp)                                          :: xx(3)
!
  data p1/'P',' ','1',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
!
  lfull = ((index(keyword,' full').ne.0.or.index(keyword,'full').eq.1).and.ncbl.gt.1) 
  nasym = nascfg(ncf)
  nsft = 0
  do i = 1,ncf-1
    nsft = nsft + nascfg(i)
  enddo
  nstart2 = nsft + nasym + 1
  if (lfull) then
    icf = icentfct(ncbl)
    nnew = icf*numat - nasym
  else
    icf = 1
    nnew = numat - nasym
  endif
  if (icf*numat.gt.maxat) then
    maxat = icf*numat
    call changemaxat
  endif
  if (nasum + nnew.gt.maxatot) then
    maxatot = nasum + nnew
    call changemaxatot
  endif
!*******************
!  Ion attributes  *
!*******************
  allocate(ltemp(nasym),stat=status)
  if (status/=0) call outofmemory('symoff','ltemp')
  allocate(ltemp2(nasym),stat=status)
  if (status/=0) call outofmemory('symoff','ltemp2')
  do i = nasum,nstart2, - 1
    cncfg(i + nnew) = cncfg(i)
    natcfg(i + nnew) = natcfg(i)
    ntypcfg(i + nnew) = ntypcfg(i)
    nspecptrcfg(i + nnew) = nspecptrcfg(i)
    qlcfg(i + nnew) = qlcfg(i)
    occucfg(i + nnew) = occucfg(i)
    oxcfg(i + nnew) = oxcfg(i)
    radcfg(i + nnew) = radcfg(i)
    lbsmat(i + nnew) = lbsmat(i)
    ltranat(i + nnew) = ltranat(i)
  enddo
  do i = 1,nasym
    ltemp(i) = lbsmat(i + nsft)
    ltemp2(i) = ltranat(i + nsft)
  enddo
  if (lfull.and.icf.gt.1) then
    iadd = nsft
    do i = 1,numat
      do j = 1,icf
        cncfg(j + iadd) = cnf(i)
        natcfg(j + iadd) = nat(i)
        ntypcfg(j + iadd) = nftype(i)
        nspecptrcfg(j + iadd) = nspecptr(nrelat(i))
        qlcfg(j + iadd) = qf(i)
        occucfg(j + iadd) = occuf(i)
        oxcfg(j + iadd) = oxf(i)
        radcfg(j + iadd) = radf(i)
        lbsmat(j + iadd) = ltemp(nrelat(i))
        ltranat(j + iadd) = ltemp2(nrelat(i))
      enddo
      iadd = iadd + icf
    enddo
  else
    do i = 1,numat
      cncfg(i + nsft) = cnf(i)
      natcfg(i + nsft) = nat(i)
      ntypcfg(i + nsft) = nftype(i)
      nspecptrcfg(i + nsft) = nspecptr(nrelat(i))
      qlcfg(i + nsft) = qf(i)
      occucfg(i + nsft) = occuf(i)
      oxcfg(i + nsft) = oxf(i)
      radcfg(i + nsft) = radf(i)
      lbsmat(i + nsft) = ltemp(nrelat(i))
      ltranat(i + nsft) = ltemp2(nrelat(i))
    enddo
  endif
  deallocate(ltemp2,stat=status)
  if (status/=0) call deallocate_error('symoff','ltemp2')
  deallocate(ltemp,stat=status)
  if (status/=0) call deallocate_error('symoff','ltemp')
!****************
!  Coordinates  *
!****************
  do i = nasum,nstart2, - 1
    xcfg(i+nnew) = xcfg(i)
    ycfg(i+nnew) = ycfg(i)
    zcfg(i+nnew) = zcfg(i)
  enddo
  if (lfull) then
    do i = 1,3
      rvf(1,i) = rvcfg(1,i,ncf)
      rvf(2,i) = rvcfg(2,i,ncf)
      rvf(3,i) = rvcfg(3,i,ncf)
    enddo
    call uncentre(rvf)
    do i = 1,3
      rvcfg(1,i,ncf) = rvf(1,i)
      rvcfg(2,i,ncf) = rvf(2,i)
      rvcfg(3,i,ncf) = rvf(3,i)
    enddo
    ifail = 0
    call matinv(rvf,3_i4,3_i4,rr,ifail)
    do i = 1,3
      do j = 1,3
        rvt(j,i) = rvf(j,1)*rv(1,i) + rvf(j,2)*rv(2,i) + rvf(j,3)*rv(3,i)
      enddo
    enddo
!
!  Transform primitive fractional coordinates to full set
!
    do i = 1,numat
      xt(1) = xfrac(i)
      xt(2) = yfrac(i)
      xt(3) = zfrac(i)
      xfrac(i) = rvt(1,1)*xt(1) + rvt(1,2)*xt(2) + rvt(1,3)*xt(3)
      yfrac(i) = rvt(2,1)*xt(1) + rvt(2,2)*xt(2) + rvt(2,3)*xt(3)
      zfrac(i) = rvt(3,1)*xt(1) + rvt(3,2)*xt(2) + rvt(3,3)*xt(3)
    enddo
    iadd = nsft
    do i = 1,numat
      iadd = iadd + 1
      xcfg(iadd) = xfrac(i)
      ycfg(iadd) = yfrac(i)
      zcfg(iadd) = zfrac(i)
!
!  Expand by centring operators
!
      if (ncbl.eq.2) then
        xt(1) = 0.0_dp
        xt(2) = 0.5_dp
        xt(3) = 0.5_dp
      elseif (ncbl.eq.3) then
        xt(1) = 0.5_dp
        xt(2) = 0.0_dp
        xt(3) = 0.5_dp
      elseif (ncbl.eq.4) then
        xt(1) = 0.5_dp
        xt(2) = 0.5_dp
        xt(3) = 0.0_dp
      elseif (ncbl.eq.5) then
        xt(1) = 0.0_dp
        xt(2) = 0.5_dp
        xt(3) = 0.5_dp
      elseif (ncbl.eq.6) then
        xt(1) = 0.5_dp
        xt(2) = 0.5_dp
        xt(3) = 0.5_dp
      else
        xt(1) = 2.0_dp/3.0_dp
        xt(2) = 1.0_dp/3.0_dp
        xt(3) = 1.0_dp/3.0_dp
      endif
      iadd = iadd + 1
      xcfg(iadd) = xfrac(i) + xt(1)
      ycfg(iadd) = yfrac(i) + xt(2)
      zcfg(iadd) = zfrac(i) + xt(3)
      if (ncbl.eq.7) then
        iadd = iadd + 1
        xt(1) = 1.0_dp/3.0_dp
        xt(2) = 2.0_dp/3.0_dp
        xt(3) = 2.0_dp/3.0_dp
        xcfg(iadd) = xfrac(i) + xt(1)
        ycfg(iadd) = yfrac(i) + xt(2)
        zcfg(iadd) = zfrac(i) + xt(3)
      elseif (ncbl.eq.5) then
        iadd = iadd + 1
        xt(1) = 0.5_dp
        xt(2) = 0.0_dp
        xt(3) = 0.5_dp
        xcfg(iadd) = xfrac(i) + xt(1)
        ycfg(iadd) = yfrac(i) + xt(2)
        zcfg(iadd) = zfrac(i) + xt(3)
        iadd = iadd + 1
        xt(1) = 0.5_dp
        xt(2) = 0.5_dp
        xt(3) = 0.0_dp
        xcfg(iadd) = xfrac(i) + xt(1)
        ycfg(iadd) = yfrac(i) + xt(2)
        zcfg(iadd) = zfrac(i) + xt(3)
      endif
    enddo
    do i = 1,numat*icf
      xcfg(i+nsft) = mod((xcfg(i+nsft) + 1.0_dp),1.0_dp)
      ycfg(i+nsft) = mod((ycfg(i+nsft) + 1.0_dp),1.0_dp)
      zcfg(i+nsft) = mod((zcfg(i+nsft) + 1.0_dp),1.0_dp)
    enddo
  else
    do i = 1,numat
      xcfg(i+nsft) = xfrac(i)
      ycfg(i+nsft) = yfrac(i)
      zcfg(i+nsft) = zfrac(i)
    enddo
  endif
!**********************************
!  Electrostatic site potentials  *
!**********************************
  if (.not.lfull) then
!
!  If this is not a nosym full case then we need to convert centred fractional coordinates to the primitive cell
!
    do i = 1,npotsites
      if (npotsitecfg(i).eq.ncf) then
        if (ndim.eq.3) then
!
!  Convert input fractional coordinates for centred cell to those for primitive cell
!
          xx(1) = xpotsite(i)
          xx(2) = ypotsite(i)
          xx(3) = zpotsite(i)
          x(1) = 0.0_dp
          x(2) = 0.0_dp
          x(3) = 0.0_dp
          v(1) = vit(1,1)
          v(2) = vit(2,1)
          v(3) = vit(3,1)
          call GULP_mxmb(rop(1,1,1),1_i4,3_i4,xx,1_i4,1_i4,v,1_i4,1_i4,3_i4,3_i4,1_i4)
          call GULP_mxmb(w1(ncbl,1,1),7_i4,21_i4,v,1_i4,1_i4,x,1_i4,1_i4,3_i4,3_i4,1_i4)
          xpotsite(i) = x(1)
          ypotsite(i) = x(2)
          zpotsite(i) = x(3)
        endif
      endif
    enddo
  endif
!***********************
!  Optimisation flags  *
!***********************
  if (.not.lconp.and..not.lconv) then
    allocate(ltemp(3*nasym),stat=status)
    if (status/=0) call outofmemory('symoff','ltemp')
    nbv = 3*(nstart2 - 1) + 1
    do i = 3*nasum,nbv, - 1
      lopfi(i + 3*nnew) = lopfi(i)
    enddo
    indi = 3*nsft
    do i = 1,3*nasym
      ltemp(i) = lopfi(indi + i)
    enddo
    if (lfull) then
      do i = 1,numat
        nri = nrelat(i)
        ind = 3*(nri - 1)
        do j = 1,icf
          lopfi(indi + 1) = ltemp(ind + 1)
          lopfi(indi + 2) = ltemp(ind + 2)
          lopfi(indi + 3) = ltemp(ind + 3)
          indi = indi + 3
        enddo
      enddo
    else
      do i = 1,numat
        nri = nrelat(i)
        ind = 3*(nri - 1)
        lopfi(indi + 3*(i - 1) + 1) = ltemp(ind + 1)
        lopfi(indi + 3*(i - 1) + 2) = ltemp(ind + 2)
        lopfi(indi + 3*(i - 1) + 3) = ltemp(ind + 3)
      enddo
    endif
    deallocate(ltemp,stat=status)
    if (status/=0) call deallocate_error('symoff','ltemp')
  endif
!
!  Change number of atoms and set space group to P1
!
  nascfg(ncf) = icf*numat
  nspcg(ncf) = 1
  ngocfg(ncf) = 1
  ncbl = 1
  do i = 1,16
    hmssg(i,ncf) = p1(i)
  enddo
  nasum = nasum + icf*numat - nasym
!
!  Change cell to primitive cell
!
  if (.not.lfull) then
    do i = 1,3
      rvcfg(1,i,ncf) = rv(1,i)
      rvcfg(2,i,ncf) = rv(2,i)
      rvcfg(3,i,ncf) = rv(3,i)
    enddo
  endif
!
  return
  end
