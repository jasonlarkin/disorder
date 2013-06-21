  subroutine pireal0d
!
!  Subroutine for calculating the polarisation contribution
!  to the first derivatives for a finite cluster => 0 D system
!
!   6/00 Created from real0d.f
!   2/01 Symmetrisation corrected for frozen case
!   9/01 lmolq calculations accelerated using lneedmol
!   2/02 lneedmol algorithm corrected
!   1/05 Sqrt of distance added
!   4/05 Mods for cosh-spring added
!   2/07 Bonding types added
!   3/07 Bonding types modified
!   5/07 Argument list for twobody call modified
!  12/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!  11/08 x/y/z components passed to twobody1
!   1/09 Integer datatypes all explicitly declared
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   6/09 Virial terms added
!  11/09 Region derivatives added
!   4/12 xvir, yvir and zvir removed
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
!  Julian Gale, NRI, Curtin University, April 2012
!
  use configurations, only : nregionno
  use constants
  use control
  use current
  use derivatives
  use element,        only : maxele
  use general,        only : cutw
  use molecule
  use optimisation
  use polarise
  use potentialxyz
  use shell
  use times
  use two
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ix
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iy
  integer(i4)                                  :: iyy
  integer(i4)                                  :: iz
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jx
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jzz
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nff
  integer(i4)                                  :: nfi
  integer(i4)                                  :: nfj
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
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lcspair
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lorder12
  logical                                      :: lptrmol
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1iq
  real(dp)                                     :: d1jq
  real(dp)                                     :: d2i2q
  real(dp)                                     :: d2ijq
  real(dp)                                     :: d2j2q
  real(dp)                                     :: d2loc(3,3)
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: eatom
  real(dp)                                     :: ec6
  real(dp)                                     :: ereal
  real(dp)                                     :: factor
  real(dp)                                     :: fct
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: poli
  real(dp)                                     :: polj
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r
  real(dp)                                     :: rderiv
  real(dp)                                     :: rp
  real(dp)                                     :: rpd1
  real(dp)                                     :: rpd2
  real(dp)                                     :: rpd3
  real(dp)                                     :: rpd4
  real(dp)                                     :: rpd5
  real(dp)                                     :: rpd6
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp)                                     :: small
  real(dp)                                     :: small2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  time1 = cputime()
!
!  Local variables
!
  small = 1.0d-12
  small2 = 1.0d-2
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  cut2s = cuts*cuts
  if (lwolf) then        
    cut2q = cutw*cutw
  else
    cut2q = 1.0d12
  endif
!
  if (lnoreal) goto 999
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('pireal0d','npotl')
!
!  Find total number of variable atoms
!
  if (lfreeze) then
    nff = 0
    do i = 1,numat
      if (lopf(i)) nff = nff + 1
    enddo
  else
    nff = numat
  endif
!
!  Outer loop over sites
!
  if (.not.lfreeze.or.lopf(1)) then
    ix = 1
    iy = 2
    iz = 3
    nfi = 1
  else
    ix =  - 2
    iy =  - 1
    iz = 0
    nfi = 0
  endif
  do i = 2,numat
    xal = xclat(i)
    yal = yclat(i)
    zal = zclat(i)
    nati = nat(i)
    ntypi = nftype(i)
    nregioni = nregionno(nsft+nrelat(i))
    qli = qf(i)
    oci = occuf(i)
    lopi = (.not.lfreeze.or.lopf(i))
    if (lopi) then
      ix = ix + 3
      iy = iy + 3
      iz = iz + 3
      nfi = nfi + 1
    endif
!
!  Molecule handling
!
    if (lmol) then
      nmi = natmol(i)
      indmi = nmolind(i)
    endif
!
!  Inner loop over second site
!
    jx =  - 2
    jy =  - 1
    jz = 0
    nfj = 0
    jloop: do j = 1,i - 1
      lopj = (.not.lfreeze.or.lopf(j))
      if (lopj) then
        jx = jx + 3
        jy = jy + 3
        jz = jz + 3
        nfj = nfj + 1
      endif
      if (.not.lopi.and..not.lopj) cycle jloop
      natj = nat(j)
      ntypj = nftype(j)
      nregionj = nregionno(nsft+nrelat(j))
      qlj = qf(j)
      ocj = occuf(j)
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
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
        nat2 = natj
        ntyp1 = ntypi
        ntyp2 = ntypj
      else
        lorder12 = .false.
        nat1 = natj
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
      ofct = oci*ocj
      fct = ofct*angstoev
      factor = qli*qlj*fct
!
!  Possible core - shell flag
!
      lcspair = (abs(nat1 - nat2).eq.maxele.or.(oci + ocj).lt.1.0001_dp)
!
!  Molecule handling
!
      if (lmol) then
        nmj = natmol(j)
        indmj = nmolind(j)
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      lneedmol  =  (lmol.and..not.lmolq)
      do n = 1,npote
        if (nat1.eq.nspec1(n).and.nat2.eq.nspec2(n)) then
          if ((ntyp1.eq.nptyp1(n).or.nptyp1(n).eq.0).and.(ntyp2.eq.nptyp2(n).or.nptyp2(n).eq.0)) then
            npots = npots + 1
            npotl(npots) = n
            if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol  =  .true.
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
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  Molecule and bonding checks
!
      if (lmol) then
        lmolok  =  (nmi.eq.nmj.and.nmi.ne.0)
      else
        lmolok  =  .false.
      endif
!
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok  =  .false.
!
      if (lmolok.and.(r.gt.cut2s.or..not.lcspair)) then
        ind = indmj - indmi
        lptrmol = (ind.eq.0)
        if (.not.lptrmol) then
          call mindtoijk(indmj,jxx,jyy,jzz)
          call mindtoijk(indmi,ixx,iyy,izz)
          jxx = jxx - ixx
          jyy = jyy - iyy
          jzz = jzz - izz
          call samemol(lptrmol,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
        endif
        if (lptrmol) then
          call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
        else
          lbonded = .false.
          l2bonds = .false.
          l3bonds = .false.
        endif
      else
        lptrmol = .false.
        lbonded = .false.
        l2bonds = .false.
        l3bonds = .false.
      endif
      if (abs(r - small2).lt.1.0d-12) r = small2
      if (r.lt.small) then
        cycle jloop
      else
!
!  Store vector
!
        nor = 1
        dist = sqrt(r)
      endif
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
      eatom = 0.0_dp
      ereal = 0.0_dp
      ec6 = 0.0_dp
      call twobody1(eatom,ereal,ec6,.true.,.true.,.false.,nor,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,0.0_dp, &
                    cut2q,cut2s,lptrmol,nmolonly,factor,ofct,0.0_dp,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1iq,d1jq,d2i2q,d2ijq,d2j2q)
!**************************
!  Coordinate Derivatives *
!**************************
!
!  First derivatives
!
      rpd1 = xcrd*xcrd
      rpd2 = ycrd*ycrd
      rpd3 = zcrd*zcrd
      rpd4 = ycrd*zcrd
      rpd5 = xcrd*zcrd
      rpd6 = xcrd*ycrd
      d2loc(1,1) = derive2*rpd1 + derive
      d2loc(2,1) = derive2*rpd6
      d2loc(3,1) = derive2*rpd5
      d2loc(1,2) = derive2*rpd6
      d2loc(2,2) = derive2*rpd2 + derive
      d2loc(3,2) = derive2*rpd4
      d2loc(1,3) = derive2*rpd5
      d2loc(2,3) = derive2*rpd4
      d2loc(3,3) = derive2*rpd3 + derive
      poli = qlj*dpolar(i)/angstoev
      polj = qli*dpolar(j)/angstoev
      if (lopi) then
        xdrv(i) = xdrv(i) - poli*(vx(i)*d2loc(1,1) + vy(i)*d2loc(2,1) + vz(i)*d2loc(3,1))
        ydrv(i) = ydrv(i) - poli*(vx(i)*d2loc(1,2) + vy(i)*d2loc(2,2) + vz(i)*d2loc(3,2))
        zdrv(i) = zdrv(i) - poli*(vx(i)*d2loc(1,3) + vy(i)*d2loc(2,3) + vz(i)*d2loc(3,3))
        xdrv(i) = xdrv(i) + polj*(vx(j)*d2loc(1,1) + vy(j)*d2loc(2,1) + vz(j)*d2loc(3,1))
        ydrv(i) = ydrv(i) + polj*(vx(j)*d2loc(1,2) + vy(j)*d2loc(2,2) + vz(j)*d2loc(3,2))
        zdrv(i) = zdrv(i) + polj*(vx(j)*d2loc(1,3) + vy(j)*d2loc(2,3) + vz(j)*d2loc(3,3))
      endif
      if (lopj) then
        xdrv(j) = xdrv(j) + poli*(vx(i)*d2loc(1,1) + vy(i)*d2loc(2,1) + vz(i)*d2loc(3,1))
        ydrv(j) = ydrv(j) + poli*(vx(i)*d2loc(1,2) + vy(i)*d2loc(2,2) + vz(i)*d2loc(3,2))
        zdrv(j) = zdrv(j) + poli*(vx(i)*d2loc(1,3) + vy(i)*d2loc(2,3) + vz(i)*d2loc(3,3))
        xdrv(j) = xdrv(j) - polj*(vx(j)*d2loc(1,1) + vy(j)*d2loc(2,1) + vz(j)*d2loc(3,1))
        ydrv(j) = ydrv(j) - polj*(vx(j)*d2loc(1,2) + vy(j)*d2loc(2,2) + vz(j)*d2loc(3,2))
        zdrv(j) = zdrv(j) - polj*(vx(j)*d2loc(1,3) + vy(j)*d2loc(2,3) + vz(j)*d2loc(3,3))
      endif
      if (nregioni.ne.nregionj) then
        xregdrv(nregioni) = xregdrv(nregioni) - poli*(vx(i)*d2loc(1,1) + vy(i)*d2loc(2,1) + vz(i)*d2loc(3,1))
        yregdrv(nregioni) = yregdrv(nregioni) - poli*(vx(i)*d2loc(1,2) + vy(i)*d2loc(2,2) + vz(i)*d2loc(3,2))
        zregdrv(nregioni) = zregdrv(nregioni) - poli*(vx(i)*d2loc(1,3) + vy(i)*d2loc(2,3) + vz(i)*d2loc(3,3))
        xregdrv(nregioni) = xregdrv(nregioni) + polj*(vx(j)*d2loc(1,1) + vy(j)*d2loc(2,1) + vz(j)*d2loc(3,1))
        yregdrv(nregioni) = yregdrv(nregioni) + polj*(vx(j)*d2loc(1,2) + vy(j)*d2loc(2,2) + vz(j)*d2loc(3,2))
        zregdrv(nregioni) = zregdrv(nregioni) + polj*(vx(j)*d2loc(1,3) + vy(j)*d2loc(2,3) + vz(j)*d2loc(3,3))
!
        xregdrv(nregionj) = xregdrv(nregionj) + poli*(vx(i)*d2loc(1,1) + vy(i)*d2loc(2,1) + vz(i)*d2loc(3,1))
        yregdrv(nregionj) = yregdrv(nregionj) + poli*(vx(i)*d2loc(1,2) + vy(i)*d2loc(2,2) + vz(i)*d2loc(3,2))
        zregdrv(nregionj) = zregdrv(nregionj) + poli*(vx(i)*d2loc(1,3) + vy(i)*d2loc(2,3) + vz(i)*d2loc(3,3))
        xregdrv(nregionj) = xregdrv(nregionj) - polj*(vx(j)*d2loc(1,1) + vy(j)*d2loc(2,1) + vz(j)*d2loc(3,1))
        yregdrv(nregionj) = yregdrv(nregionj) - polj*(vx(j)*d2loc(1,2) + vy(j)*d2loc(2,2) + vz(j)*d2loc(3,2))
        zregdrv(nregionj) = zregdrv(nregionj) - polj*(vx(j)*d2loc(1,3) + vy(j)*d2loc(2,3) + vz(j)*d2loc(3,3))
      endif
!
!  Skip to here if frozen pair
!
    enddo jloop
  enddo
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('pireal0d','npotl')
!
!  End of real space part - perform general tasks
!
999 continue
!
!  Timing
!
  time2 = cputime()
  tpolar = tpolar + time2 - time1
!
  return
  end
