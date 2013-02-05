  subroutine pirealrecip
!
!  Calculates the first derivatives of the polarisation energy.
!
!   6/00 Created from realrecip3d3.F
!   2/01 Modifications for general dimensionality added
!   2/01 Strain first derivatives added for polarisation energy
!   9/01 lmolq calculations accelerated using lneedmol
!   2/02 lneedmol algorithm corrected
!   1/03 Wolf sum modifications made
!   3/03 Rsearch calls replaces local code
!   1/05 rp no longer sqrt'd and passed to rsearch routines
!   4/05 Mods for cosh-spring added
!   5/07 Argument list for twobody call modified
!  12/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody argument list
!   6/09 Virial terms added
!  11/09 Region derivatives added
!   4/12 xvir, yvir and zvir removed
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
!
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations, only : nregionno
  use constants
  use control
  use current
  use datatypes
  use derivatives
  use element,        only : maxele
  use general
  use kspace
  use molecule
  use optimisation
  use parallel
  use polarise
  use potentialxyz
  use realvectors
  use shell
  use symmetry
  use times
  use two
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ixi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyi
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izi
  integer(i4)                                  :: izj
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lcspair
  logical                                      :: lewaldtype
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lorder12
  logical                                      :: lself
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2e
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: cut2w
  real(dp)                                     :: d2(3,3)
  real(dp)                                     :: d2k(3,3)
  real(dp)                                     :: d2s(3,6)
  real(dp)                                     :: eatom
  real(dp)                                     :: ec6
  real(dp)                                     :: ereal
  real(dp)                                     :: factor
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: poli
  real(dp)                                     :: polj
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: rdl1
  real(dp)                                     :: rdl2
  real(dp)                                     :: rdl3
  real(dp)                                     :: rdl4
  real(dp)                                     :: rdl5
  real(dp)                                     :: rdl6
  real(dp)                                     :: rp
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: rtrm1
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
  real(dp)                                     :: xdrvi
  real(dp)                                     :: ydrvi
  real(dp)                                     :: zdrvi
  real(dp)                                     :: xdrvj
  real(dp)                                     :: ydrvj
  real(dp)                                     :: zdrvj
!
  time1 = cputime()
!
!  Zero energies although not needed to avoid overflow
!
  eatom = 0.0_dp
  ereal = 0.0_dp
  ec6 = 0.0_dp
!***************************
!  Set up local variables  *
!***************************
!
!  Set the Coulomb term type based on dimensionality :
!
!  1-D         => 1/r
!  2-D and 3-D => erfc(seta*r)/r
!
  lewaldtype = (ndim.ne.1.or.lwolf)
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  cut2e = rmx2
  cut2s = cuts*cuts
  cut2w = cutw*cutw
  if (lwolf) then        
    cut2q = cut2w        
  else
    cut2q = cut2e
  endif
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('pirealrecip','npotl')
!
!  Initialise K vector terms
!
  call setktrmdp
!***************************************************************
!  Atomistic and real space electrostatic second derivatives   *
!***************************************************************
!
!  Outer loop over sites
!
  do i = 1,numat
!
!  Inner loop over second site
!
    xal = xclat(i)
    yal = yclat(i)
    zal = zclat(i)
    nati = nat(i)
    ntypi = nftype(i)
    nregioni = nregionno(nsft+nrelat(i))
    qli = qf(i)
    oci = occuf(i)
    lopi = (.not.lfreeze.or.lopf(nrelat(i)))
!
!  Molecule handling
!
    if (lmol) then
      nmi = natmol(i)
      indm = nmolind(i)
      call mindtoijk(indm,ixi,iyi,izi)
    endif
!
!  Start of second atom loop
!
    jloop: do j = 1,i
      natj = nat(j)
      ntypj = nftype(j)
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
      if (nati.eq.natj) then
        nat1 = nati
        nat2 = natj
        if (ntypi.lt.ntypj) then
          lorder12 = .true.
          ntyp1 = ntypi
          ntyp2 = ntypj
        else
          lorder12 = .false.
          ntyp1 = ntypj
          ntyp2 = ntypi
        endif
      elseif (nati.lt.natj) then
        lorder12 = .true.
        nat1 = nati
        nat2 = nat(j)
        ntyp1 = ntypi
        ntyp2 = nftype(j)
      else
        lorder12 = .false.
        nat1 = nat(j)
        nat2 = nati
        ntyp1 = nftype(j)
        ntyp2 = ntypi
      endif
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      qlj = qf(j)
      ocj = occuf(j)
      ofct = oci*ocj
      nregionj = nregionno(nsft+nrelat(j))
      lopj = (.not.lfreeze.or.lopf(nrelat(j)))
      factor = qli*qlj*ofct*angstoev
!
!  Possible core-shell flag
!
      lcspair = (abs(nat1-nat2).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
!
!  Molecule handling
!
      if (lmol) then
        nmj = natmol(j)
        indmj = nmolind(j)
        call mindtoijk(indmj,ixj,iyj,izj)
        ixj = ixj - ixi
        iyj = iyj - iyi
        izj = izj - izi
        lmolok = (nmi.eq.nmj.and.nmi.gt.0)
      else
        lmolok = .false.
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      lneedmol = (lmol.and..not.lmolq)
      do n = 1,npote
        if (nat1.eq.nspec1(n).and.nat2.eq.nspec2(n)) then
          if ((ntyp1.eq.nptyp1(n).or.nptyp1(n).eq.0).and.(ntyp2.eq.nptyp2(n).or.nptyp2(n).eq.0)) then
            npots = npots + 1
            npotl(npots) = n
            if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol = .true.
            if (nptype(n).eq.8.or.nptype(n).eq.33) then
              if (cuts.gt.rp) rp = cuts
            else
              if (rpot(n).gt.rp) rp = rpot(n)
            endif
          endif
        endif
      enddo
      cut2r = rp*rp
      if (cut2r.gt.cut2p) cut2r = cut2p
      cut2 = cut2r
      if (cut2e.gt.cut2.and.lewald) cut2 = cut2e
      if (cut2w.gt.cut2.and.lwolf) cut2 = cut2w
!
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok = .false.
!
      nmolonly = 0
      nor = 0
!
!  Zero second derivative arrays
!
      do jj = 1,3
        d2(1,jj) = 0.0_dp
        d2(2,jj) = 0.0_dp
        d2(3,jj) = 0.0_dp
      enddo
      if (lstr) then
        do jj = 1,nstrains
          d2s(1,jj) = 0.0_dp
          d2s(2,jj) = 0.0_dp
          d2s(3,jj) = 0.0_dp
        enddo
      endif
!
!  If no valid potentials and charge product is zero then no need to search for distances
!
      if (npots.eq.0.and.abs(factor).lt.1.0d-8) cycle jloop
!***********************
!  Find valid vectors  *
!***********************
      if (ndim.eq.3) then
        call rsearch3D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
      elseif (ndim.eq.2) then
        call rsearch2D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
      elseif (ndim.eq.1) then
        call rsearch1D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
      endif
      if (nor.eq.0) cycle jloop
!
!  Sqrt distances
!
      do k = 1,nor
        dist(k) = sqrt(dist(k))
      enddo
!
      call twobody(eatom,ereal,ec6,.true.,.true.,.false.,nor,1,npots,npotl,cut2,cut2q,cut2s, &
                   nmolonly,factor,ofct,0.0_dp,rtrm1,sctrm1,sctrm2,qli,qlj,lcspair,lewaldtype, &
                   .false.,.false.,.false.,lorder12)
!*****************************************************************
!  Calculate reciprocal space contribution to third derivatives  *
!*****************************************************************
      if (lewald) then
        call reciptrmdp(xcrd,ycrd,zcrd,lstr,ofct,d2,d2s)
      endif
!****************************
!  Loop over all distances  *
!****************************
      poli = qlj*dpolar(i)/angstoev
      polj = qli*dpolar(j)/angstoev
      do kk = 1,nor
!
!  Generate products for derivatives
!
        xcrd = xtmp(kk)
        ycrd = ytmp(kk)
        zcrd = ztmp(kk)
        rdl1 = xcrd*xcrd
        rdl2 = ycrd*ycrd
        rdl3 = zcrd*zcrd
        rdl4 = ycrd*zcrd
        rdl5 = xcrd*zcrd
        rdl6 = xcrd*ycrd
!
!  Second derivative terms for first derivatives
!
        d2k(1,1) = derive2(kk)*rdl1 + derive(kk)
        d2k(2,1) = derive2(kk)*rdl6
        d2k(3,1) = derive2(kk)*rdl5
        d2k(1,2) = derive2(kk)*rdl6
        d2k(2,2) = derive2(kk)*rdl2 + derive(kk)
        d2k(3,2) = derive2(kk)*rdl4
        d2k(1,3) = derive2(kk)*rdl5
        d2k(2,3) = derive2(kk)*rdl4
        d2k(3,3) = derive2(kk)*rdl3 + derive(kk)
!
!  Add to cumulative arrays
!
        do ii = 1,3
          d2(1,ii) = d2(1,ii) + d2k(1,ii)
          d2(2,ii) = d2(2,ii) + d2k(2,ii)
          d2(3,ii) = d2(3,ii) + d2k(3,ii)
        enddo
!
        if (lstr) then
          if (ndim.eq.3) then
            d2s(1,1) = d2s(1,1) + d2k(1,1)*xcrd
            d2s(2,1) = d2s(2,1) + d2k(2,1)*xcrd
            d2s(3,1) = d2s(3,1) + d2k(3,1)*xcrd
            d2s(1,2) = d2s(1,2) + d2k(2,1)*ycrd
            d2s(2,2) = d2s(2,2) + d2k(2,2)*ycrd
            d2s(3,2) = d2s(3,2) + d2k(3,2)*ycrd
            d2s(1,3) = d2s(1,3) + d2k(3,1)*zcrd
            d2s(2,3) = d2s(2,3) + d2k(3,2)*zcrd
            d2s(3,3) = d2s(3,3) + d2k(3,3)*zcrd
            d2s(1,4) = d2s(1,4) + d2k(3,2)*xcrd
            d2s(2,4) = d2s(2,4) + (d2k(2,2) - 0.5*derive(kk))*zcrd
            d2s(3,4) = d2s(3,4) + (d2k(3,3) - 0.5*derive(kk))*ycrd
            d2s(1,5) = d2s(1,5) + (d2k(1,1) - 0.5*derive(kk))*zcrd
            d2s(2,5) = d2s(2,5) + d2k(3,2)*xcrd
            d2s(3,5) = d2s(3,5) + (d2k(3,3) - 0.5*derive(kk))*xcrd
            d2s(1,6) = d2s(1,6) + (d2k(1,1) - 0.5*derive(kk))*ycrd
            d2s(2,6) = d2s(2,6) + (d2k(2,2) - 0.5*derive(kk))*xcrd
            d2s(3,6) = d2s(3,6) + d2k(3,2)*xcrd
          elseif (ndim.eq.2) then
            d2s(1,1) = d2s(1,1) + d2k(1,1)*xcrd
            d2s(2,1) = d2s(2,1) + d2k(2,1)*xcrd
            d2s(3,1) = d2s(3,1) + d2k(3,1)*xcrd
            d2s(1,2) = d2s(1,2) + d2k(2,1)*ycrd
            d2s(2,2) = d2s(2,2) + d2k(2,2)*ycrd
            d2s(3,2) = d2s(3,2) + d2k(3,2)*ycrd
            d2s(1,3) = d2s(1,3) + (d2k(1,1) - 0.5*derive(kk))*ycrd
            d2s(2,3) = d2s(2,3) + (d2k(2,2) - 0.5*derive(kk))*xcrd
            d2s(3,3) = d2s(3,3) + d2k(3,2)*xcrd
          endif
        endif
!****************************
!  End loop over distances  *
!****************************
      enddo
!**************************
!  Coordinate Derivatives *
!**************************
!
!  First derivatives
!
      xdrvi = - poli*(vx(i)*d2(1,1) + vy(i)*d2(2,1) + vz(i)*d2(3,1))
      ydrvi = - poli*(vx(i)*d2(1,2) + vy(i)*d2(2,2) + vz(i)*d2(3,2))
      zdrvi = - poli*(vx(i)*d2(1,3) + vy(i)*d2(2,3) + vz(i)*d2(3,3))
      xdrvi = xdrvi + polj*(vx(j)*d2(1,1) + vy(j)*d2(2,1) + vz(j)*d2(3,1))
      ydrvi = ydrvi + polj*(vx(j)*d2(1,2) + vy(j)*d2(2,2) + vz(j)*d2(3,2))
      zdrvi = zdrvi + polj*(vx(j)*d2(1,3) + vy(j)*d2(2,3) + vz(j)*d2(3,3))
      if (lopi) then
        xdrv(i) = xdrv(i) + xdrvi
        ydrv(i) = ydrv(i) + ydrvi
        zdrv(i) = zdrv(i) + zdrvi
      endif
      xdrvj = poli*(vx(i)*d2(1,1) + vy(i)*d2(2,1) + vz(i)*d2(3,1))
      ydrvj = poli*(vx(i)*d2(1,2) + vy(i)*d2(2,2) + vz(i)*d2(3,2))
      zdrvj = poli*(vx(i)*d2(1,3) + vy(i)*d2(2,3) + vz(i)*d2(3,3))
      xdrvj = xdrvj - polj*(vx(j)*d2(1,1) + vy(j)*d2(2,1) + vz(j)*d2(3,1))
      ydrvj = ydrvj - polj*(vx(j)*d2(1,2) + vy(j)*d2(2,2) + vz(j)*d2(3,2))
      zdrvj = zdrvj - polj*(vx(j)*d2(1,3) + vy(j)*d2(2,3) + vz(j)*d2(3,3))
      if (lopj) then
        xdrv(j) = xdrv(j) + xdrvj
        ydrv(j) = ydrv(j) + ydrvj
        zdrv(j) = zdrv(j) + zdrvj
      endif
!
      if (nregioni.ne.nregionj) then
        xregdrv(nregioni) = xregdrv(nregioni) + xdrvi
        yregdrv(nregioni) = yregdrv(nregioni) + ydrvi
        zregdrv(nregioni) = zregdrv(nregioni) + zdrvi
        xregdrv(nregionj) = xregdrv(nregionj) + xdrvj
        yregdrv(nregionj) = yregdrv(nregionj) + ydrvj
        zregdrv(nregionj) = zregdrv(nregionj) + zdrvj
      endif
      if (lstr) then
        rstrdloc(1:nstrains) = 0.0_dp
        do ii = 1,nstrains
          rstrdloc(ii) = rstrdloc(ii) + poli*(vx(i)*d2s(1,ii) + vy(i)*d2s(2,ii) + vz(i)*d2s(3,ii))
          rstrdloc(ii) = rstrdloc(ii) - polj*(vx(j)*d2s(1,ii) + vy(j)*d2s(2,ii) + vz(j)*d2s(3,ii))
          rstrd(ii) = rstrd(ii) + rstrdloc(ii)
        enddo
        if (latomicstress) then
          do ii = 1,nstrains
            atomicstress(ii,i) = atomicstress(ii,i) + 0.5_dp*rstrdloc(ii)
            atomicstress(ii,j) = atomicstress(ii,j) + 0.5_dp*rstrdloc(ii)
          enddo
        endif
      endif
    enddo jloop
  enddo
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('pirealrecip','npotl')
!
!  Timing
!
  time2 = cputime()
  tpolar = tpolar + time2 - time1
!
  return
  end
