  subroutine real0d3(mcvmin,mcvmax,iocptr)
!
!  Subroutine for calculating the real space phonon derivatives
!  for a finite cluster => 0 D system
!
!  NOTE : Currently BSM is not allowed for.
!         Neither are methods with charge derivatives
!
!  mcvmin = lower bound to mode range
!  mcvmax = upper bound to mode range
!  ncfoc  = number of unique core sites
!  nsfoc  = number of unique shell sites
!  iocptr = pointer from atom number to unique site number
!
!   8/97 Initially created from real0d.f
!   9/97 Lower half triangular use of d3 only now
!   9/97 Calculation of three body contribution to i-j added to
!        avoid multiple calls of projection routine
!   9/97 Array d33 added for many body non i-j terms
!   7/98 Four-body potential modifications added. Strategy is to
!        treat each four-body term like two three-body terms to
!        the non i-j atoms, thus allowing all the existing three
!        body code to be used for both types of potential
!   8/98 Extra array added for fourbody terms for third derivatives
!        between atoms k and l not directly involved in i-j element
!   1/99 1-4 interaction scaling added
!   6/99 Partial occupancy modifications added
!   7/99 Many-body potentials added based on four-body algorithm
!   6/00 xal,yal,zal,xcrd,ycrd,zcrd dropped from argument list
!        to four0d3 as they get corrupted and are not needed
!   6/00 maxmany and maxmany2 added to the argument list so that
!        dimensions of arrays can be checked
!   9/01 lmolq calculations accelerated using lneedmol 
!   2/02 lneedmol algorithm corrected
!  11/02 Wildcard atoms added
!  11/03 Alloy scaling added for EAM
!  11/03 Workspace arrays moved into module for resizing
!   9/04 Call to twobody1 modified
!   4/05 Mods for cosh-spring added
!   7/05 Deallocation of pointers in modules removed as this should be
!        done via changemaxmany
!   2/07 Bonding types added
!   3/07 Bonding types modified
!   5/07 Argument list for twobody call modified
!  11/07 Unused variables cleaned up
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!  11/08 x/y/z components passed to twobody1
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   5/09 Calls to many body routines removed for now
!   6/09 Module name changed from three to m_three
!
!  nmanyk = number of three-/four-body term K atoms whose derivatives
!          depend on the i-j block
!  nptrmanyk = pointer to the atoms for each nmanyk
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use configurations, only : lbsmat
  use constants
  use control
  use current
  use derivatives
  use eam,            only : maxmeamcomponent
  use element,        only : maxele
  use general,        only : cutw
  use feworkspace
  use four
  use m_three
  use molecule
  use shell
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: iocptr(*)
  integer(i4), intent(in)                      :: mcvmax
  integer(i4), intent(in)                      :: mcvmin
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ioc
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: joc
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nforkl
  integer(i4)                                  :: nmanyk
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nor
  integer(i4)                                  :: npots
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lcorei
  logical                                      :: lcorej
  logical                                      :: lcspair
  logical                                      :: lfor
  logical                                      :: lmany
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lptrmol
  logical                                      :: lthb
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1i
  real(dp)                                     :: d1j
  real(dp)                                     :: d2i2
  real(dp)                                     :: d2ij
  real(dp)                                     :: d2j2
  real(dp)                                     :: d3(3,3,3)
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: dfedx
  real(dp)                                     :: dfedy
  real(dp)                                     :: dfedz
  real(dp)                                     :: eatom
  real(dp)                                     :: ec6
  real(dp)                                     :: ereal
  real(dp)                                     :: factor
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rderiv
  real(dp)                                     :: rmassi
  real(dp)                                     :: rmassj
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
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: small
  real(dp)                                     :: small2
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
  real(dp)                                     :: xd2
  real(dp)                                     :: yd2
  real(dp)                                     :: zd2
!
  time1 = cputime()
!
!  Local variables
!
  small = 1.0d-12
  small2 = 1.0d-2
  lthb = (nthb.gt.0)
  lfor = (nfor.gt.0)
  lmany = (lthb.or.lfor)
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
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('real0d3','npotl')
!
!  Zero energies
!
  eatom = 0.0_dp
  ereal = 0.0_dp
  ec6 = 0.0_dp
  if (lnoreal) goto 999
!
!  Outer loop over sites
!
  do i = 2,numat
    xal = xclat(i)
    yal = yclat(i)
    zal = zclat(i)
    nati = nat(i)
    lcorei = (nati.le.maxele)
    ntypi = nftype(i)
    qli = qf(i)
    oci = occuf(i)
    ioc = iocptr(i)
    if (lcorei) rmassi = rmass(ioc)
    ix = 3*(ioc-1) + 1
    iy = ix + 1
    iz = ix + 2
    if (lbsmat(i+nsft)) then
      radi = radf(i)
    else
      radi = 0.0_dp
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
    do j = 1,i-1
      joc = iocptr(j)
      jx = 3*(joc-1) + 1
      jy = jx + 1
      jz = jx + 2
      natj = nat(j)
      lcorej = (natj.le.maxele)
      ntypj = nftype(j)
      qlj = qf(j)
      ocj = occuf(j)
      if (lcorej) rmassj = rmass(joc)
      if (lbsmat(nsft+j)) then
        radj = radf(j)
      else
        radj = 0.0_dp
      endif
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      radsum = radi + radj
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
      endif
!******************************
!  Locate twobody potentials  *
!******************************
      rp = 0.0_dp
      npots = 0
      lneedmol = (lmol.and..not.lmolq)
      do n = 1,npote
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
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
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  Molecule and bonding checks
!
      if (lmol) then
        lmolok = (nmi.eq.nmj.and.nmi.ne.0)
      else
        lmolok = .false.
      endif
!  
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok = .false.
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
          nbtypeij  = 0
          nbtypeij2 = 0
        endif
      else
        lptrmol   = .false.
        lbonded   = .false.
        l2bonds   = .false.
        l3bonds   = .false.
        nbtypeij  = 0
        nbtypeij2 = 0
      endif
      if (abs(r-small2).lt.1.0d-12) r = small2
      if (r.lt.small) then
!
!  Core-shell spring constant makes no contribution
!  at small distances to third derivatives
!
        goto 1000
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
      call twobody1(eatom,ereal,ec6,.true.,.true.,.true.,nor,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                    cut2r,cut2q,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
!
!  Generate products for derivatives
!
      rpd1 = xcrd*xcrd*deriv3
      rpd2 = ycrd*ycrd*deriv3
      rpd3 = zcrd*zcrd*deriv3
      rpd4 = ycrd*zcrd*deriv3
      rpd5 = xcrd*zcrd*deriv3
      rpd6 = xcrd*ycrd*deriv3
      xd2 = xcrd*deriv2
      yd2 = ycrd*deriv2
      zd2 = zcrd*deriv2
!**************************
!  Coordinate Derivatives *
!**************************
!
!  Calculate third derivative matrix
!
      d3(1,1,1) = rpd1*xcrd + 3.0_dp*xd2
      d3(2,1,1) = rpd6*xcrd + yd2
      d3(3,1,1) = rpd5*xcrd + zd2
      d3(2,2,1) = rpd2*xcrd + xd2
      d3(3,2,1) = rpd4*xcrd
      d3(3,3,1) = rpd3*xcrd + xd2
      d3(2,2,2) = rpd2*ycrd + 3.0_dp*yd2
      d3(3,2,2) = rpd4*ycrd + zd2
      d3(3,3,2) = rpd3*ycrd + yd2
      d3(3,3,3) = rpd3*zcrd + 3.0_dp*zd2
!********************************************
!  Symmetrise d3 matrices for 2-body terms  *
!********************************************
      d3(1,2,1) = d3(2,1,1)
      d3(1,3,1) = d3(3,1,1)
      d3(2,3,1) = d3(3,2,1)
      d3(2,3,2) = d3(3,2,2)
      d3(1,1,2) = d3(2,1,1)
      d3(2,1,2) = d3(2,2,1)
      d3(3,1,2) = d3(3,2,1)
      d3(1,2,2) = d3(2,2,1)
      d3(1,3,2) = d3(3,2,1)
      d3(2,3,2) = d3(3,2,2)
      d3(1,1,3) = d3(3,1,1)
      d3(2,1,3) = d3(3,2,1)
      d3(3,1,3) = d3(3,3,1)
      d3(1,2,3) = d3(3,2,1)
      d3(2,2,3) = d3(3,2,2)
      d3(3,2,3) = d3(3,3,2)
      d3(1,3,3) = d3(3,3,1)
      d3(2,3,3) = d3(3,3,2)
!****************************
!  Three-body contribution  *
!****************************
      nmanyk = 0
      nforkl = 0
      if (lthb) then
        call three0d3(i,j,nati,ntypi,natj,ntypj,d3,xal,yal,zal,xcrd,ycrd,zcrd,nmanyk)
      endif
!***************************
!  Four-body contribution  *
!***************************
      if (lfor) then
        call four0d3(i,j,d3,nmanyk,nforkl)
      endif
      t1 = cputime()
!*********************************************
!  Project contributions on to phonon modes  *
!*********************************************
      call projd0(mcvmin,mcvmax,d3,i,ix,iy,iz,j,jx,jy,jz,lcorei,lcorej,rmassi,rmassj,dfedx,dfedy,dfedz, &
        derv2,dervi,maxd2,lmany,nmanyk,nptrmanyk,d33,nforkl,nptrfork,nptrforl,d34)
      t2 = cputime()
      tproj = tproj + t2 - t1
!
!  Add on derivative contributions
!
      xdrv(i) = xdrv(i) + dfedx
      ydrv(i) = ydrv(i) + dfedy
      zdrv(i) = zdrv(i) + dfedz
      xdrv(j) = xdrv(j) - dfedx
      ydrv(j) = ydrv(j) - dfedy
      zdrv(j) = zdrv(j) - dfedz
!
!  Skip to here if frozen pair
!
1000  continue
    enddo
  enddo
!
!  End of real space part 
!
999 continue
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real0d3','npotl')
!
!  Timing
!
  time2 = cputime()
  tderv3 = tderv3 + time2 - time1
!
  return
  end
