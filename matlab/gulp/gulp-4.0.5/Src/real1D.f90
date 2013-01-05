  subroutine real1D(erealin,esregion12,esregion2,lgrad1,lgrad2)
!
!  This subroutine calculates the electrostatic energy of a 1-D system 
!  in real space based on the algorithm implemented in CRYSTAL. A 
!  neutralising uniform background charge density is applied and then 
!  subtracted again. Because a sum over neutral unit cells is required, 
!  this code is kept separate from the other real space routines. The 
!  conventional real space routines are used to handle the Coulomb
!  subtraction issues in order to keep this routine simple.
!
!   9/01 Created
!  10/01 Extra efficiency modifications made
!  10/01 Cartesian derivatives added
!  12/01 Strain derivatives added 
!   5/02 Second strain derivatives finished
!   5/02 Occupancies and core-shell exclusion added
!   5/02 hfunc call modified for third derivatives
!   9/02 Calculation of hfunc/emfunc for m < 1 disabled
!   9/02 Regions added
!   5/03 Modified so that ereal on entry is not overwritten since this
!        may contain contributions from real space Coulomb subtraction.
!   9/04 Charge first derivatives added
!   9/04 Charge second derivatives added
!  10/04 Symmetrisation of second derivatives moved to subroutine
!   7/05 Error in last call to d1charge corrected
!   5/07 QM/MM scheme added
!   3/09 Explicit 1.0d-15 replaced by global value smallself from general module
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
!   5/12 Atomic stresses added
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
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use constants,      only : angstoev
  use configurations, only : nregionno, nregions, nregiontype, QMMMmode
  use control,        only : lnoreal, lseok, keyword, lDoQDeriv1, lDoQDeriv2, latomicstress
  use current
  use derivatives
  use element,        only : maxele
  use energies,       only : eregion2region
  use general,        only : accuracy, nmaxcells, nemorder, smallself
  use iochannels,     only : ioout
  use optimisation,   only : lfreeze, lopf
  use qmedata,        only : maxloop
  use shell,          only : cuts
  use symmetry,       only : lstr
  use times
  implicit none
!
!  Passed arguments
!
  real(dp), intent(inout) :: erealin
  real(dp), intent(inout) :: esregion12
  real(dp), intent(inout) :: esregion2
  logical,  intent(in)    :: lgrad1
  logical,  intent(in)    :: lgrad2
!
!  Local variables
!
  integer                 :: i
  integer                 :: ix,iy,iz 
  integer                 :: ixc,iyc,izc
  integer                 :: j 
  integer                 :: jx,jy,jz 
  integer                 :: jxc,jyc,jzc
  integer                 :: m
  integer                 :: nati
  integer                 :: natj
  integer                 :: nff
  integer                 :: nregioni
  integer                 :: nregionj
  integer                 :: nregiontypi
  integer                 :: nregiontypj
  logical                 :: lconverged
  logical                 :: lcspair
  logical                 :: lopi
  logical                 :: lopj
  logical                 :: lreg2one
  logical                 :: lreg2pair
  real(dp)                :: accf
  real(dp)                :: acell
  real(dp)                :: cputime
  real(dp)                :: cut2s
  real(dp)                :: d0
  real(dp)                :: d0i
  real(dp)                :: d0j
  real(dp)                :: d0term
  real(dp)                :: d1
  real(dp)                :: d1i
  real(dp)                :: d1is
  real(dp)                :: d1ix
  real(dp)                :: d1iy
  real(dp)                :: d1iz
  real(dp)                :: d1j
  real(dp)                :: d1js
  real(dp)                :: d1jx
  real(dp)                :: d1jy
  real(dp)                :: d1jz
  real(dp)                :: d1s
  real(dp)                :: d1snoq
  real(dp)                :: d2
  real(dp)                :: d2i2
  real(dp)                :: d2ij
  real(dp)                :: d2j2
  real(dp)                :: dh1(3)
  real(dp)                :: dh2(3)
  real(dp)                :: dh1s
  real(dp)                :: dh2s
  real(dp)                :: d2h1(6)
  real(dp)                :: d2h2(6)
  real(dp)                :: d2h1m(3)
  real(dp)                :: d2h2m(3)
  real(dp)                :: d2h1s
  real(dp)                :: d2h2s
  real(dp)                :: d3h1(10)
  real(dp)                :: d3h1m(6)
  real(dp)                :: ediff
  real(dp)                :: elast
  real(dp)                :: ereal
  real(dp)                :: esum
  real(dp)                :: esumem
  real(dp)                :: esumh
  real(dp)                :: esum12
  real(dp)                :: esumem12
  real(dp)                :: esumh12
  real(dp)                :: esum2
  real(dp)                :: esumem2
  real(dp)                :: esumh2
  real(dp)                :: e1
  real(dp)                :: e2
  real(dp)                :: h1
  real(dp)                :: h2
  real(dp)                :: lna
  real(dp)                :: oci
  real(dp)                :: ocj
  real(dp)                :: qi
  real(dp)                :: qj
  real(dp)                :: qii
  real(dp)                :: qij
  real(dp)                :: r
  real(dp)                :: rcut
  real(dp)                :: rr
  real(dp)                :: t1, t2
  real(dp)                :: u
  real(dp)                :: x
  real(dp)                :: y
  real(dp)                :: z
!
  t1 = cputime()
!
!  If noreal specified, return
!
  if (lnoreal) then
    erealin = 0.0_dp
    return
  endif
!********************************************************
!  Calculate Coulomb sum converged to desired accuracy  *
!********************************************************
!       
!  Find number of unfrozen atoms
!           
  if (lfreeze) then
    nff = 0
    do i = 1,nasym
      if (lopf(i)) nff = nff + neqv(i)
    enddo
  else  
    nff = numat
  endif
!
!  Loop over number of cells in sum
!
  accf = 10.0**(-accuracy)
  lna = log(a)
  cut2s = cuts*cuts
  m = - 1
  lconverged = .false.
  elast = 0.0_dp
  esum = 0.0_dp
  esum12 = 0.0_dp
  esum2 = 0.0_dp
  if (index(keyword,'verb').ne.0) then
    write(ioout,'(/,'' Convergence of 1-D electrostatic summation:'',/)')
  endif
  do while (m.lt.nmaxcells.and..not.lconverged) 
    m = m + 1
!********************************************
!  Direct sum component over neutral cells  *
!********************************************
    acell = dble(m)*a
    if (lopf(1).or..not.lfreeze) then
      ixc = 1
      iyc = 2
      izc = 3
    else
      ixc = -2
      iyc = -1
      izc =  0
    endif
    do i = 2,numat
      nati = nat(i)
      oci = occuf(i)
      qi = qf(i)*oci
      nregioni = nregionno(nsft+i)
      nregiontypi = nregiontype(nregioni,ncf)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      if (lopi) then
        ixc = ixc + 3
        iyc = iyc + 3
        izc = izc + 3
      endif
      jxc = -2
      jyc = -1
      jzc =  0
      jloop: do j = 1,i-1
        natj = nat(j)
        ocj = occuf(j)
        qj = qf(j)*ocj
        nregionj = nregionno(nsft+j)
        nregiontypj = nregiontype(nregionj,ncf)
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        lcspair = (abs(nati-natj).eq.maxele.or.(oci+ocj).lt.1.0001d0)
        if (lcspair) then
          rcut = cut2s
        else
          rcut = smallself
        endif
        if (lopi.or.lopj) then
          if (lopj) then
            jxc = jxc + 3
            jyc = jyc + 3
            jzc = jzc + 3  
          endif
!
!  Set flags such that if one atom is not being
!  optimised place 3 x 3 second derivative matrix
!  in the on-diagonal block
!
          if (lopi.and.lopj) then
            ix = ixc
            iy = iyc
            iz = izc
            jx = jxc
            jy = jyc
            jz = jzc
          elseif (lopi) then
            ix = ixc
            iy = iyc
            iz = izc
            jx = ixc
            jy = iyc
            jz = izc
          elseif (lopj) then
            ix = jxc
            iy = jyc      
            iz = jzc
            jx = jxc
            jy = jyc
            jz = jzc
          endif
!
!  Set region 2 pair flags
!        
          lreg2one  = .false.
          lreg2pair = .false.
          if (lseok.and.nregions(ncf).ge.2) then
            lreg2pair = (nregioni.eq.2.and.nregionj.eq.2)
            if (.not.lreg2pair) lreg2one = (nregioni.eq.2.or.nregionj.eq.2)
          endif
!
!  QM/MM handling : i & j are both QM atoms => exclude
!
          if (QMMMmode(ncf).gt.0) then
            if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop
!           
!  QM/MM : Electrostatic embedding : If either i or j are QM atoms => exclude 
!       
            if (QMMMmode(ncf).eq.2.and.(nregiontypi.eq.1.or.nregiontypj.eq.1)) cycle jloop
          endif
!
          x = acell + xclat(j) - xclat(i)
          y = yclat(j) - yclat(i)
          z = zclat(j) - zclat(i)
          r = x*x + y*y + z*z
          if (r.gt.rcut) then
            r = sqrt(r)
            rr = 1.0_dp/r
            d0 = qi*qj*rr
            if (lreg2one) then
              esum12 = esum12 + d0
            elseif (lreg2pair) then
              esum2 = esum2 + d0
            else
              esum = esum + d0
            endif
!
            eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + d0*angstoev
!
            if (lgrad1) then
              d1 = d0*rr*rr*angstoev
              if (lopi) then
                xdrv(i) = xdrv(i) + d1*x
                ydrv(i) = ydrv(i) + d1*y
                zdrv(i) = zdrv(i) + d1*z
              endif
              if (lopj) then
                xdrv(j) = xdrv(j) - d1*x
                ydrv(j) = ydrv(j) - d1*y
                zdrv(j) = zdrv(j) - d1*z
              endif
              if (nregioni.ne.nregionj) then
                xregdrv(nregioni) = xregdrv(nregioni) + d1*x
                yregdrv(nregioni) = yregdrv(nregioni) + d1*y
                zregdrv(nregioni) = zregdrv(nregioni) + d1*z
                xregdrv(nregionj) = xregdrv(nregionj) - d1*x
                yregdrv(nregionj) = yregdrv(nregionj) - d1*y
                zregdrv(nregionj) = zregdrv(nregionj) - d1*z
              endif
              if (lstr) then
                rstrd(1) = rstrd(1) - d1*x*x
                if (latomicstress) then
                  atomicstress(1,i) = atomicstress(1,i) - 0.5_dp*d1*x*x
                  atomicstress(1,j) = atomicstress(1,j) - 0.5_dp*d1*x*x
                endif
              endif
              if (lDoQDeriv1) then
                d0i = qj*rr*angstoev
                d0j = qi*rr*angstoev
                call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
              endif
!
              if (lgrad2) then
                d2 = - 3.0_dp*d1*rr*rr
                derv2(jx,ix) = derv2(jx,ix) + d2*x*x
                derv2(jy,ix) = derv2(jy,ix) + d2*y*x
                derv2(jz,ix) = derv2(jz,ix) + d2*z*x
                derv2(jx,iy) = derv2(jx,iy) + d2*x*y
                derv2(jy,iy) = derv2(jy,iy) + d2*y*y
                derv2(jz,iy) = derv2(jz,iy) + d2*z*y
                derv2(jx,iz) = derv2(jx,iz) + d2*x*z
                derv2(jy,iz) = derv2(jy,iz) + d2*y*z
                derv2(jz,iz) = derv2(jz,iz) + d2*z*z
                derv2(jx,ix) = derv2(jx,ix) + d1
                derv2(jy,iy) = derv2(jy,iy) + d1
                derv2(jz,iz) = derv2(jz,iz) + d1
                if (lstr) then
                  derv3(ix,1) = derv3(ix,1) + d2*x*x*x
                  derv3(iy,1) = derv3(iy,1) + d2*x*x*y
                  derv3(iz,1) = derv3(iz,1) + d2*x*x*z
                  derv3(jx,1) = derv3(jx,1) - d2*x*x*x
                  derv3(jy,1) = derv3(jy,1) - d2*x*x*y
                  derv3(jz,1) = derv3(jz,1) - d2*x*x*z
                  sderv2(1,1) = sderv2(1,1) - d2*x*x*x*x
                endif
!*******************************************************
!  Variable charge contribution to second derivatives  *
!*******************************************************
                if (lDoQDeriv2) then
                  d0i  = qj*rr*angstoev
                  d0j  = qi*rr*angstoev
                  d1i  = - d0i*rr*rr
                  d1j  = - d0j*rr*rr
                  d2i2 = 0.0_dp
                  d2ij = rr*angstoev
                  d2j2 = 0.0_dp
                  d1ix = d1i*x
                  d1iy = d1i*y
                  d1iz = d1i*z
                  d1jx = d1j*x
                  d1jy = d1j*y
                  d1jz = d1j*z
                  d1is = d1i*x*x
                  d1js = d1j*x*x
                  call d2charge(i,j,1_i4,ix,iy,iz,jx,jy,jz,lopi,lopj,d0i,d0j,d1ix,d1iy,d1iz, &
                                d1jx,d1jy,d1jz,d1is,d1js,d2i2,d2ij,d2j2,0.0_dp,0.0_dp,0.0_dp, &
                                .true.,.false.)
                endif
              endif
            endif
          endif
          if (m.gt.0) then
            x = - acell + xclat(j) - xclat(i)
            r = x*x + y*y + z*z
            if (r.gt.rcut) then
              r = sqrt(r)
              rr = 1.0_dp/r
              d0 = qi*qj*rr
              if (lreg2one) then
                esum12 = esum12 + d0
              elseif (lreg2pair) then
                esum2 = esum2 + d0
              else
                esum = esum + d0
              endif
!
              eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + d0*angstoev
!
              if (lgrad1) then
                d1 = d0*rr*rr*angstoev
                if (lopj) then
                  xdrv(j) = xdrv(j) - d1*x
                  ydrv(j) = ydrv(j) - d1*y
                  zdrv(j) = zdrv(j) - d1*z
                endif
                if (lopi) then
                  xdrv(i) = xdrv(i) + d1*x
                  ydrv(i) = ydrv(i) + d1*y
                  zdrv(i) = zdrv(i) + d1*z
                endif
                if (nregioni.ne.nregionj) then
                  xregdrv(nregioni) = xregdrv(nregioni) + d1*x
                  yregdrv(nregioni) = yregdrv(nregioni) + d1*y
                  zregdrv(nregioni) = zregdrv(nregioni) + d1*z
                  xregdrv(nregionj) = xregdrv(nregionj) - d1*x
                  yregdrv(nregionj) = yregdrv(nregionj) - d1*y
                  zregdrv(nregionj) = zregdrv(nregionj) - d1*z
                endif
                if (lstr) then
                  rstrd(1) = rstrd(1) - d1*x*x
                  if (latomicstress) then
                    atomicstress(1,i) = atomicstress(1,i) - 0.5_dp*d1*x*x
                    atomicstress(1,j) = atomicstress(1,j) - 0.5_dp*d1*x*x
                  endif
                endif
                if (lDoQDeriv1) then
                  d0i = qj*rr*angstoev
                  d0j = qi*rr*angstoev
                  call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
                endif
                if (lgrad2) then
                  d2 = - 3.0_dp*d1*rr*rr
                  derv2(jx,ix) = derv2(jx,ix) + d2*x*x
                  derv2(jy,ix) = derv2(jy,ix) + d2*y*x
                  derv2(jz,ix) = derv2(jz,ix) + d2*z*x
                  derv2(jx,iy) = derv2(jx,iy) + d2*x*y
                  derv2(jy,iy) = derv2(jy,iy) + d2*y*y
                  derv2(jz,iy) = derv2(jz,iy) + d2*z*y
                  derv2(jx,iz) = derv2(jx,iz) + d2*x*z
                  derv2(jy,iz) = derv2(jy,iz) + d2*y*z
                  derv2(jz,iz) = derv2(jz,iz) + d2*z*z
                  derv2(jx,ix) = derv2(jx,ix) + d1
                  derv2(jy,iy) = derv2(jy,iy) + d1
                  derv2(jz,iz) = derv2(jz,iz) + d1
                  if (lstr) then
                    derv3(ix,1) = derv3(ix,1) + d2*x*x*x
                    derv3(iy,1) = derv3(iy,1) + d2*x*x*y
                    derv3(iz,1) = derv3(iz,1) + d2*x*x*z
                    derv3(jx,1) = derv3(jx,1) - d2*x*x*x
                    derv3(jy,1) = derv3(jy,1) - d2*x*x*y
                    derv3(jz,1) = derv3(jz,1) - d2*x*x*z
                    sderv2(1,1) = sderv2(1,1) - d2*x*x*x*x
                  endif
!*******************************************************
!  Variable charge contribution to second derivatives  *
!*******************************************************
                  if (lDoQDeriv2) then
                    d0i  = qj*rr*angstoev
                    d0j  = qi*rr*angstoev
                    d1i  = - d0i*rr*rr
                    d1j  = - d0j*rr*rr
                    d2i2 = 0.0_dp
                    d2ij = rr*angstoev
                    d2j2 = 0.0_dp
                    d1ix = d1i*x
                    d1iy = d1i*y
                    d1iz = d1i*z
                    d1jx = d1j*x
                    d1jy = d1j*y
                    d1jz = d1j*z
                    d1is = d1i*x*x
                    d1js = d1j*x*x
                    call d2charge(i,j,1_i4,ix,iy,iz,jx,jy,jz,lopi,lopj,d0i,d0j,d1ix,d1iy,d1iz, &
                                  d1jx,d1jy,d1jz,d1is,d1js,d2i2,d2ij,d2j2,0.0_dp,0.0_dp,0.0_dp, &
                                  .true.,.false.)
                  endif
                endif
              endif
            endif
          endif
        endif
      enddo jloop
    enddo
!**********************
!  Self interactions  *
!**********************
    ix = -2
    iy = -1 
    iz =  0
    iloop: do i = 1,numat
      oci = occuf(i)
      qi = qf(i)*oci
      nregioni = nregionno(nsft+i)
      nregiontypi = nregiontype(nregioni,ncf)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
!
!  QM/MM handling : i is a QM atom => exclude
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1) cycle iloop
      endif
      if (lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        r = abs(acell)
        if (r.gt.smallself) then
          rr = 1.0_dp/r
          d0 = qi*qi*rr
!
!  Set region 2 pair flags
!        
          lreg2pair = .false.
          if (lseok.and.nregions(ncf).ge.2) then
            lreg2pair = (nregioni.eq.2)
          endif
          if (lreg2pair) then
            esum2 = esum2 + d0
          else
            esum = esum + d0
          endif
!
          eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + d0*angstoev
!
          if (lgrad1.and.lstr) then
            d1 = d0*angstoev
            rstrd(1) = rstrd(1) - d1
            if (latomicstress) then
              atomicstress(1,i) = atomicstress(1,i) - d1
            endif
            if (lgrad2) then
              d2 = - 3.0_dp*d1
              sderv2(1,1) = sderv2(1,1) - d2
            endif
          endif
          if (lgrad1.and.lDoQDeriv1) then
            d0i = qj*rr*angstoev
            call d1charge(i,i,lopi,lopi,1_i4,d0i,d0i)
          endif
!*******************************************************
!  Variable charge contribution to second derivatives  *
!*******************************************************
          if (lgrad2.and.lDoQDeriv2) then
            d0i  = qi*rr*angstoev
            d1i  = - d0i*rr*rr
            d2i2 = 0.0_dp
            d2ij = rr*angstoev
            d2j2 = 0.0_dp
            d1ix = d1i*acell
            d1is = d1i*acell*acell
            call d2charge(i,i,1_i4,ix,iy,iz,ix,iy,iz,lopi,lopi,d0i,d0i,d1ix,0.0_dp,0.0_dp, &
                          d1ix,0.0_dp,0.0_dp,d1is,d1is,d2i2,d2ij,d2j2,0.0_dp,0.0_dp,0.0_dp, &
                          .true.,.false.)
          endif
        endif
      endif
    enddo iloop
!
!  Neutralising terms
!
!  Background
!
!  and
!
!  Euler-MacLaurin component
!
    esumh = 0.0_dp
    esumh12 = 0.0_dp
    esumh2 = 0.0_dp
    esumem = 0.0_dp
    esumem12 = 0.0_dp
    esumem2 = 0.0_dp
    if (m.gt.0) then
      u = (dble(m)+0.5_dp)*a
      do i = 2,numat
        oci = occuf(i)
        qi = qf(i)*oci
        nregioni = nregionno(nsft+i)
        nregiontypi = nregiontype(nregioni,ncf)
        lopi = (.not.lfreeze.or.lopf(nrelat(i)))
        jloop2: do j = 1,i-1
          ocj = occuf(j)
          qj = qf(j)*ocj
          nregionj = nregionno(nsft+j)
          nregiontypj = nregiontype(nregionj,ncf)
          lopj = (.not.lfreeze.or.lopf(nrelat(j)))
          if (lopi.or.lopj) then
!     
!  Set region 2 pair flags
!     
            lreg2one  = .false.
            lreg2pair = .false.
            if (lseok.and.nregions(ncf).ge.2) then
              lreg2pair = (nregioni.eq.2.and.nregionj.eq.2)
              if (.not.lreg2pair) lreg2one = (nregioni.eq.2.or.nregionj.eq.2)
            endif
!
!  QM/MM handling : i & j are QM atoms => exclude
!
            if (QMMMmode(ncf).gt.0) then
              if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop2
!
!  QM/MM : Electrostatic embedding : If either i or j are QM atoms => exclude 
!
              if (QMMMmode(ncf).eq.2.and.(nregiontypi.eq.1.or.nregiontypj.eq.1)) cycle jloop2
            endif
!
            qij = qi*qj
            x = xclat(j) - xclat(i)
            y = yclat(j) - yclat(i)
            z = zclat(j) - zclat(i)
            call hfunc(u,+x,1.0_dp,y,z,h1,dh1,d2h1,d3h1,.false.,.false.,.false.)
            call hfunc(u,-x,-1.0_dp,y,z,h2,dh2,d2h2,d3h1,.false.,.false.,.false.)
            if (lreg2one) then
              esumh12 = esumh12 - qij*(h1 + h2 - 2.0_dp*lna)/a
            elseif (lreg2pair) then
              esumh2 = esumh2 - qij*(h1 + h2 - 2.0_dp*lna)/a
            else
              esumh = esumh - qij*(h1 + h2 - 2.0_dp*lna)/a
            endif
            call emfunc(nemorder,u,+x,1.0_dp,y,z,a,e1,dh1,d2h1,dh1s,d2h1s,d2h1m,d3h1,d3h1m,.false.,.false.,.false.)
            call emfunc(nemorder,u,-x,-1.0_dp,y,z,a,e2,dh2,d2h2,dh2s,d2h2s,d2h2m,d3h1,d3h1m,.false.,.false.,.false.)
            if (lreg2one) then
              esumem12 = esumem12 + qij*(e1 + e2)
            elseif (lreg2pair) then
              esumem2 = esumem2 + qij*(e1 + e2)
            else
              esumem = esumem + qij*(e1 + e2)
            endif
!
            eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + &
              qij*(e1 + e2 - (h1 + h2 - 2.0_dp*lna)/a)*angstoev
          endif
        enddo jloop2
      enddo
      iloop2: do i = 1,numat
        oci = occuf(i)
        qi = qf(i)*oci
        nregioni = nregionno(nsft+i)
        nregiontypi = nregiontype(nregioni,ncf)
        lopi = (.not.lfreeze.or.lopf(nrelat(i)))
!
!  QM/MM handling : i is a QM atom => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1) cycle iloop2
        endif
        if (lopi) then
!
!  Set region 2 pair flags
!
          lreg2pair = .false.
          if (lseok.and.nregions(ncf).ge.2) then
            lreg2pair = (nregioni.eq.2)
          endif
!
          qii = qi*qi
          call hfunc(u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,h1,dh1,d2h1,d3h1,.false.,.false.,.false.)
          if (lreg2pair) then
            esumh2 = esumh2 - qii*(h1 - lna)/a
          else
            esumh = esumh - qii*(h1 - lna)/a
          endif
          call emfunc(nemorder,u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,a,e1,dh1,d2h1,dh1s,d2h1s,d2h1m, &
            d3h1,d3h1m,.false.,.false.,.false.)
          if (lreg2pair) then
            esumem2 = esumem2 + qii*e1
          else
            esumem = esumem + qii*e1
          endif
!
          eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + qii*(e1 - (h1 - lna)/a)*angstoev
        endif
      enddo iloop2
    endif
!
!  Sum up terms
!
    ereal = esum + esumh + esumem
!
!  Compare to energy with previous number of cells
!  and check for convergence to the required
!  accuracy.
!
    if (abs(ereal).lt.accf) lconverged = .true.
    if (.not.lconverged) then
      ediff = abs((ereal - elast)/ereal)
      lconverged = (ediff.lt.accf)
    endif
    if (index(keyword,'verb').ne.0) then
      write(ioout,'('' Mcell = '',i4,''  Ereal(1-D) = '',f15.6,'' eV'')') m,ereal*angstoev
    endif
    elast = ereal
  enddo
  if (index(keyword,'verb').ne.0) write(ioout,'(/)')
  ereal = ereal*angstoev
  erealin = erealin + ereal
  esregion12 = esregion12 + (esum12 + esumh12 + esumem12)*angstoev
  esregion2 = esregion2 + (esum2 + esumh2 + esumem2)*angstoev
!
!  Save number of cells needed
!
  maxloop(1) = m
!
!  Derivatives of Euler-MacLaurin terms since these are not cumulative
!
  if (lgrad1.and.m.gt.0) then
    u = (dble(m)+0.5_dp)*a
    if (lopf(1).or..not.lfreeze) then
      ixc = 1
      iyc = 2
      izc = 3
    else
      ixc = -2
      iyc = -1
      izc =  0
    endif
    do i = 2,numat
      oci = occuf(i)
      qi = qf(i)*oci
      nregioni = nregionno(nsft+i)
      nregiontypi = nregiontype(nregioni,ncf)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      if (lopi) then
        ixc = ixc + 3
        iyc = iyc + 3
        izc = izc + 3
      endif
      jxc = - 2
      jyc = - 1
      jzc =   0
      jloop3: do j = 1,i-1
        ocj = occuf(j)
        qj = qf(j)*ocj
        nregionj = nregionno(nsft+j)
        nregiontypj = nregiontype(nregionj,ncf)
!
!  QM/MM handling : i & j are QM atoms => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop3
!
!  QM/MM : Electrostatic embedding : If either i or j are QM atoms => exclude 
!
          if (QMMMmode(ncf).eq.2.and.(nregiontypi.eq.1.or.nregiontypj.eq.1)) cycle jloop3
        endif
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        if (lopi.or.lopj) then
          if (lopj) then
            jxc = jxc + 3
            jyc = jyc + 3
            jzc = jzc + 3  
          endif
!
!  Set flags such that if one atom is not being
!  optimised place 3 x 3 second derivative matrix
!  in the on-diagonal block
!
          if (lopi.and.lopj) then
            ix = ixc
            iy = iyc
            iz = izc
            jx = jxc
            jy = jyc
            jz = jzc
          elseif (lopi) then
            ix = ixc
            iy = iyc
            iz = izc
            jx = ixc
            jy = iyc
            jz = izc
          elseif (lopj) then
            ix = jxc
            iy = jyc      
            iz = jzc
            jx = jxc
            jy = jyc
            jz = jzc
          endif
!
          qij = qi*qj
          x = xclat(j) - xclat(i)
          y = yclat(j) - yclat(i)
          z = zclat(j) - zclat(i)
          call hfunc(u,+x,1.0_dp,y,z,h1,dh1,d2h1,d3h1,lgrad1,lgrad2,.false.)
          call hfunc(u,-x,-1.0_dp,y,z,h2,dh2,d2h2,d3h1,lgrad1,lgrad2,.false.)
          d1 = qij*angstoev/a
          if (lopi) then
            xdrv(i) = xdrv(i) + d1*(dh1(1) + dh2(1))
            ydrv(i) = ydrv(i) + d1*(dh1(2) + dh2(2))
            zdrv(i) = zdrv(i) + d1*(dh1(3) + dh2(3))
          endif
          if (lopj) then
            xdrv(j) = xdrv(j) - d1*(dh1(1) + dh2(1))
            ydrv(j) = ydrv(j) - d1*(dh1(2) + dh2(2))
            zdrv(j) = zdrv(j) - d1*(dh1(3) + dh2(3))
          endif
          if (nregioni.ne.nregionj) then
            xregdrv(nregioni) = xregdrv(nregioni) + d1*(dh1(1) + dh2(1))
            yregdrv(nregioni) = yregdrv(nregioni) + d1*(dh1(2) + dh2(2))
            zregdrv(nregioni) = zregdrv(nregioni) + d1*(dh1(3) + dh2(3))
            xregdrv(nregionj) = xregdrv(nregionj) - d1*(dh1(1) + dh2(1))
            yregdrv(nregionj) = yregdrv(nregionj) - d1*(dh1(2) + dh2(2))
            zregdrv(nregionj) = zregdrv(nregionj) - d1*(dh1(3) + dh2(3))
          endif
          if (lstr) then
            d1s = - (dh1(1)*(u+x)-dh2(1)*(u-x))
            d1s = d1s + (h1+h2)
            d1s = d1s + 2.0_dp*(1.0d0-lna)
            d1s = d1s*angstoev/a
            d1snoq = d1s
            d1s = d1s*qij
            rstrd(1) = rstrd(1) + d1s
            if (latomicstress) then
              atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s
              atomicstress(1,j) = atomicstress(1,j) + 0.5_dp*d1s
            endif
          endif
!
!  Compute variable charge term = E without charges
!
          d0term = - angstoev*(h1 + h2 - 2.0_dp*lna)/a
          if (lgrad2) then
!
!  Second derivatives
!
            d2 = qij*angstoev/a
            derv2(jx,ix) = derv2(jx,ix) + d2*(d2h1(1)+d2h2(1))
            derv2(jy,ix) = derv2(jy,ix) + d2*(d2h1(6)+d2h2(6))
            derv2(jz,ix) = derv2(jz,ix) + d2*(d2h1(5)+d2h2(5))
            derv2(jx,iy) = derv2(jx,iy) + d2*(d2h1(6)+d2h2(6))
            derv2(jy,iy) = derv2(jy,iy) + d2*(d2h1(2)+d2h2(2))
            derv2(jz,iy) = derv2(jz,iy) + d2*(d2h1(4)+d2h2(4))
            derv2(jx,iz) = derv2(jx,iz) + d2*(d2h1(5)+d2h2(5))
            derv2(jy,iz) = derv2(jy,iz) + d2*(d2h1(4)+d2h2(4))
            derv2(jz,iz) = derv2(jz,iz) + d2*(d2h1(3)+d2h2(3))
            if (lstr) then
              derv3(ix,1) = derv3(ix,1) + d2*(d2h1(1)*(u+x) - d2h2(1)*(u-x))  &
                                        - d1*(dh1(1) + dh2(1)) - d1*(dh1(1) + dh2(1))
              derv3(iy,1) = derv3(iy,1) + d2*(d2h1(6)*(u+x) - d2h2(6)*(u-x))  &
                                        - d1*(dh1(2) + dh2(2))
              derv3(iz,1) = derv3(iz,1) + d2*(d2h1(5)*(u+x) - d2h2(5)*(u-x))  &
                                        - d1*(dh1(3) + dh2(3))
              derv3(jx,1) = derv3(jx,1) - d2*(d2h1(1)*(u+x) - d2h2(1)*(u-x))  &
                                        + d1*(dh1(1) + dh2(1)) + d1*(dh1(1) + dh2(1))
              derv3(jy,1) = derv3(jy,1) - d2*(d2h1(6)*(u+x) - d2h2(6)*(u-x))  &
                                        + d1*(dh1(2) + dh2(2))
              derv3(jz,1) = derv3(jz,1) - d2*(d2h1(5)*(u+x) - d2h2(5)*(u-x))  &
                                        + d1*(dh1(3) + dh2(3))
              sderv2(1,1) = sderv2(1,1) - d2*(d2h1(1)*(u+x)*(u+x) + d2h2(1)*(u-x)*(u-x))  &
                                        - 2.0_dp*d2 - 3.0_dp*d1s
            endif
            if (lDoQDeriv2) then
!
!  Accumulate terms that contribute to the variable charge second derivatives
!
              d1ix = - qj*angstoev*(dh1(1) + dh2(1))/a
              d1iy = - qj*angstoev*(dh1(2) + dh2(2))/a
              d1iz = - qj*angstoev*(dh1(3) + dh2(3))/a
              d1jx = - qi*angstoev*(dh1(1) + dh2(1))/a
              d1jy = - qi*angstoev*(dh1(2) + dh2(2))/a
              d1jz = - qi*angstoev*(dh1(3) + dh2(3))/a
              if (lstr) then
                d1is = d1snoq*qj
                d1js = d1snoq*qi
              else
                d1is = 0.0_dp
                d1js = 0.0_dp
              endif
            endif
          endif
!
          call emfunc(nemorder,u,+x,1.0_dp,y,z,a,e1,dh1,d2h1,dh1s, &
                      d2h1s,d2h1m,d3h1,d3h1m,lgrad1,lgrad2,.false.)
          call emfunc(nemorder,u,-x,-1.0_dp,y,z,a,e2,dh2,d2h2,dh2s, &
                      d2h2s,d2h2m,d3h1,d3h1m,lgrad1,lgrad2,.false.)
          d1 = qij*angstoev
          if (lopi) then
            xdrv(i) = xdrv(i) - d1*(dh1(1) + dh2(1))
            ydrv(i) = ydrv(i) - d1*(dh1(2) + dh2(2))
            zdrv(i) = zdrv(i) - d1*(dh1(3) + dh2(3))
          endif
          if (lopj) then
            xdrv(j) = xdrv(j) + d1*(dh1(1) + dh2(1))
            ydrv(j) = ydrv(j) + d1*(dh1(2) + dh2(2))
            zdrv(j) = zdrv(j) + d1*(dh1(3) + dh2(3))
          endif
          if (nregioni.ne.nregionj) then
            xregdrv(nregioni) = xregdrv(nregioni) - d1*(dh1(1) + dh2(1))
            yregdrv(nregioni) = yregdrv(nregioni) - d1*(dh1(2) + dh2(2))
            zregdrv(nregioni) = zregdrv(nregioni) - d1*(dh1(3) + dh2(3))
            xregdrv(nregionj) = xregdrv(nregionj) + d1*(dh1(1) + dh2(1))
            yregdrv(nregionj) = yregdrv(nregionj) + d1*(dh1(2) + dh2(2))
            zregdrv(nregionj) = zregdrv(nregionj) + d1*(dh1(3) + dh2(3))
          endif
          if (lstr) then
            rstrd(1) = rstrd(1) + d1*(dh1s + dh2s)
            if (latomicstress) then
              atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1*(dh1s + dh2s)
              atomicstress(1,j) = atomicstress(1,j) + 0.5_dp*d1*(dh1s + dh2s)
            endif
          endif
          d0term = d0term + (e1 + e2)*angstoev 
          if (lDoQDeriv1) then
            d0i = qj*d0term
            d0j = qi*d0term
            call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
          endif
          if (lgrad2) then
            d2 = qij*angstoev
            derv2(jx,ix) = derv2(jx,ix) - d2*(d2h1(1)+d2h2(1))
            derv2(jy,ix) = derv2(jy,ix) - d2*(d2h1(6)+d2h2(6))
            derv2(jz,ix) = derv2(jz,ix) - d2*(d2h1(5)+d2h2(5))
            derv2(jx,iy) = derv2(jx,iy) - d2*(d2h1(6)+d2h2(6))
            derv2(jy,iy) = derv2(jy,iy) - d2*(d2h1(2)+d2h2(2))
            derv2(jz,iy) = derv2(jz,iy) - d2*(d2h1(4)+d2h2(4))
            derv2(jx,iz) = derv2(jx,iz) - d2*(d2h1(5)+d2h2(5))
            derv2(jy,iz) = derv2(jy,iz) - d2*(d2h1(4)+d2h2(4))
            derv2(jz,iz) = derv2(jz,iz) - d2*(d2h1(3)+d2h2(3))
            if (lstr) then
              derv3(ix,1) = derv3(ix,1) + d2*(d2h1m(1)+d2h2m(1))
              derv3(iy,1) = derv3(iy,1) + d2*(d2h1m(2)+d2h2m(2))
              derv3(iz,1) = derv3(iz,1) + d2*(d2h1m(3)+d2h2m(3))
              derv3(jx,1) = derv3(jx,1) - d2*(d2h1m(1)+d2h2m(1))
              derv3(jy,1) = derv3(jy,1) - d2*(d2h1m(2)+d2h2m(2))
              derv3(jz,1) = derv3(jz,1) - d2*(d2h1m(3)+d2h2m(3))
              sderv2(1,1) = sderv2(1,1) + d2*(d2h1s + d2h2s)
            endif
!*******************************************************
!  Variable charge contribution to second derivatives  *
!*******************************************************
            if (lDoQDeriv2) then
              d0i  = qj*d0term
              d0j  = qi*d0term
              d2i2 = 0.0_dp
              d2ij = d0term
              d2j2 = 0.0_dp
              d1ix = d1ix + qj*angstoev*(dh1(1) + dh2(1))
              d1iy = d1iy + qj*angstoev*(dh1(2) + dh2(2))
              d1iz = d1iz + qj*angstoev*(dh1(3) + dh2(3))
              d1jx = d1jx + qi*angstoev*(dh1(1) + dh2(1))
              d1jy = d1jy + qi*angstoev*(dh1(2) + dh2(2))
              d1jz = d1jz + qi*angstoev*(dh1(3) + dh2(3))
              if (lstr) then
                d1is = d1is + (dh1s + dh2s)*qj*angstoev
                d1js = d1js + (dh1s + dh2s)*qi*angstoev
              endif
              call d2charge(i,j,1_i4,ix,iy,iz,jx,jy,jz,lopi,lopj,d0i,d0j,d1ix,d1iy,d1iz, &
                            d1jx,d1jy,d1jz,d1is,d1js,d2i2,d2ij,d2j2,0.0_dp,0.0_dp,0.0_dp, &
                            .true.,.false.)
            endif
          endif
        endif
      enddo jloop3
    enddo
    if (lstr.or.lDoQDeriv1.or.lDoQDeriv2) then
!***************************
!  Self-interaction terms  *
!***************************
      ix = - 2
      iy = - 1
      iz =   0
      iloop3: do i = 1,numat
        oci = occuf(i)
        qi = qf(i)*oci
        lopi = (.not.lfreeze.or.lopf(nrelat(i)))
        nregioni = nregionno(nsft+i)
        nregiontypi = nregiontype(nregioni,ncf)
        if (lopi) then
          ix = ix + 3
          iy = iy + 3
          iz = iz + 3
        endif
!
!  QM/MM handling : i is a QM atom => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1) cycle iloop3
        endif
        qii = qi*qi
        call hfunc(u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,h1,dh1,d2h1,d3h1,lgrad1,lgrad2,.false.)
        if (lstr) then
          d1 = angstoev/a
          d1s = - d1*dh1(1)*u
          d1s = d1s + d1*h1
          d1s = d1s + d1*(1.0d0-lna)
          d1snoq = d1s
          d1 = d1*qii
          d1s = d1s*qii
          rstrd(1) = rstrd(1) + d1s
          if (latomicstress) then
            atomicstress(1,i) = atomicstress(1,i) + d1s
          endif
          if (lgrad2) then
            sderv2(1,1) = sderv2(1,1) - d1*d2h1(1)*u*u - d1 - 3.0_dp*d1s
          endif
        endif
!
!  Compute variable charge term = E without charges
!
        d0term = - angstoev*(h1 - lna)/a
        if (lgrad2.and.lDoQDeriv2) then
!
!  Accumulate terms that contribute to the variable charge second derivatives
!
          d1ix = - qi*angstoev*dh1(1)/a
          d1iy = - qi*angstoev*dh1(2)/a
          d1iz = - qi*angstoev*dh1(3)/a
          if (lstr) then
            d1i = d1snoq*qi
          else
            d1i = 0.0_dp
          endif
        endif                       
        call emfunc(nemorder,u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,a,e1,dh1,d2h1, &
                    dh1s,d2h1s,d2h1m,d3h1,d3h1m,lgrad1,lgrad2,.false.)
        if (lstr) then
          rstrd(1) = rstrd(1) + qii*dh1s*angstoev
          if (latomicstress) then
            atomicstress(1,i) = atomicstress(1,i) + qii*dh1s*angstoev
          endif
          if (lgrad2) then
            sderv2(1,1) = sderv2(1,1) + qii*d2h1s*angstoev
          endif
        endif
        d0term = d0term + e1*angstoev
        if (lDoQDeriv1) then
          d0i = qj*d0term
          call d1charge(i,i,lopi,lopi,1_i4,d0i,d0i)
        endif
!*******************************************************
!  Variable charge contribution to second derivatives  *
!*******************************************************
        if (lgrad2.and.lDoQDeriv2) then
          d0i  = qi*d0term
          d2i2 = 0.0_dp
          d2ij = d0term
          d2j2 = 0.0_dp
          d1ix = d1ix + qi*angstoev*dh1(1)
          d1iy = d1iy + qi*angstoev*dh1(2)
          d1iz = d1iz + qi*angstoev*dh1(3)
          if (lstr) then
            d1i = d1i + dh1s*qi*angstoev
          endif
          call d2charge(i,i,1_i4,ix,iy,iz,ix,iy,iz,lopi,lopi,d0i,d0i,d1ix,d1iy,d1iz, &
                        d1ix,d1iy,d1iz,d1i,d1i,d2i2,d2ij,d2j2,0.0_dp,0.0_dp,0.0_dp, &
                        .true.,.false.)
        endif
      enddo iloop3
    endif
  endif
!
  t2 = cputime()
  tatom = tatom + t2 - t1
!
  return
  end
