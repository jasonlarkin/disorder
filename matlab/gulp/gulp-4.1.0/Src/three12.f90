  subroutine three12(ethb,lgrad1,lgrad2,imode)
!
!  Subroutine for defect three-body energy
!
!  imode = 1 => defective region 1 and 2a calculation
!  imode = 2 => perfect region 1 and 2a calculation
!
!  Algorithm requires that region 2 contains all valid three
!  body ions that have an interaction with region 1. Given
!  that this is normally nearest neighbour only this is
!  nearly always true.
!  Now modified for symmetry in imode 1 - lin1,lin2 and lin3 are
!  used to screen out terms which are not needed because none of
!  the species are in the symmetry reduced unit.
!
!   1/95 Intra/intermolecular specification added
!   1/95 K3 added for three-body potential
!   2/95 Exponential and SW three-body potentials added
!   2/95 Bonded specification added for three-body terms
!   3/95 Bcross potential added
!   3/95 Corrections for periodic molecules added
!   3/95 Bug in k looping corrected
!   4/95 Bug in k atom for symmetric potentials corrected
!   6/95 Correction added for symmetric potentials with
!        different cutoffs
!   3/96 Urey-Bradley potential added
!   4/96 Exponentially decaying Vessal form added
!   6/96 General theta0 added to SW3
!  10/96 Displacements no longer added to region 2a coordinates
!        by default
!   4/97 Error in second derivative storage when breathing shells
!        are present corrected
!   9/97 Bug in sw3 derivatives fixed
!   3/98 Cosine-harmonic form added
!   4/98 Small constant added to rtrm1/rtrm2 in SW3 to avoid overflow
!   4/98 Error in derivatives for distance dependent potentials when
!        theta = 180 corrected
!   5/98 Potential dependent parts placed in separated subroutine
!   6/98 Murrell-Mottram potential added
!  10/98 BAcross potential added
!  10/98 Conversion of theta to rad now done in here
!   8/99 Linear-three potential added
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   5/01 Modifications added for rhos in sw3
!   6/01 Passing of cut-offs to threebody fixed
!   9/01 lmolq calculations accelerated using lneedmol 
!   1/02 Algorithm for calculation of symmetric potentials corrected - now
!        evaluates all terms and divides by 3 for simplicity
!  10/02 Bcoscross potential added
!  11/02 Wildcard atom type added
!   9/04 New arguments added to threebody
!   6/05 Order of deallocation reversed
!  10/05 Hydrogen-bond potential added
!  12/05 ESFF equatorial potential added
!   8/06 e3d resized to 10 even though not used for benefit of 
!        NAG compiler
!   9/06 Theta tapering added
!   1/07 UFF3 potential added
!   2/07 Bonding types added
!   5/07 Dreiding option added
!   6/07 Dreiding option bonded2donorJK check added
!   7/07 Checking of bond orders added 
!   7/07 Missing check on lower cutoff for ldiff23 added
!  10/07 Error in checking of exocyclic attribute for bonds corrected
!  12/07 Unused variables removed
!   5/08 Defect bonding array structure changed
!   5/08 Handling of asymmetric bond orders corrected
!   6/08 Check for bond numbers added
!  11/08 BAcoscross form added
!   3/08 3coulomb potential added
!   3/09 Bug in addressing qdefe fixed
!   6/09 Module name changed from three to m_three
!   7/09 Modifications for exp2 potential added
!   5/10 g3coulomb potential added
!   2/13 BAGcross potential added
!   3/13 Meaning of nreldef value changed
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use constants
  use control
  use current
  use defects
  use derivatives
  use m_three
  use molecule
  use region2a
  use times
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                   :: imode
  logical,     intent(in)                   :: lgrad1
  logical,     intent(in)                   :: lgrad2
  real(dp),    intent(inout)                :: ethb
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: icm
  integer(i4)                               :: ii
  integer(i4)                               :: imm
  integer(i4)                               :: in3
  integer(i4)                               :: ind
  integer(i4)                               :: indm
  integer(i4)                               :: indmj
  integer(i4)                               :: indmk
  integer(i4)                               :: indmmi
  integer(i4)                               :: indmmj
  integer(i4)                               :: indmmk
  integer(i4)                               :: ir
  integer(i4)                               :: ixx
  integer(i4)                               :: iyy
  integer(i4)                               :: izz
  integer(i4)                               :: ixx2
  integer(i4)                               :: iyy2
  integer(i4)                               :: izz2
  integer(i4)                               :: j
  integer(i4)                               :: jj
  integer(i4)                               :: jr
  integer(i4)                               :: jxx
  integer(i4)                               :: jyy
  integer(i4)                               :: jzz
  integer(i4)                               :: jxx2
  integer(i4)                               :: jyy2
  integer(i4)                               :: jzz2
  integer(i4)                               :: k
  integer(i4)                               :: kk
  integer(i4)                               :: kl
  integer(i4)                               :: kr
  integer(i4)                               :: ksub
  integer(i4)                               :: kxx
  integer(i4)                               :: kyy
  integer(i4)                               :: kzz
  integer(i4)                               :: kxx2
  integer(i4)                               :: kyy2
  integer(i4)                               :: kzz2
  integer(i4)                               :: n
  integer(i4)                               :: n1x
  integer(i4)                               :: n11
  integer(i4)                               :: n21
  integer(i4)                               :: n31
  integer(i4)                               :: n11r
  integer(i4)                               :: n21r
  integer(i4)                               :: n31r
  integer(i4)                               :: n1xr
  integer(i4)                               :: n2xr
  integer(i4)                               :: n3xr
  integer(i4)                               :: n3x
  integer(i4)                               :: n12
  integer(i4)                               :: n22
  integer(i4)                               :: n32
  integer(i4)                               :: n12r
  integer(i4)                               :: n22r
  integer(i4)                               :: n32r
  integer(i4)                               :: n2x
  integer(i4)                               :: n3ty
  integer(i4)                               :: nbotyp11
  integer(i4)                               :: nbotyp12
  integer(i4)                               :: nbotyp21
  integer(i4)                               :: nbotyp22
  integer(i4)                               :: nbtyp11
  integer(i4)                               :: nbtyp12
  integer(i4)                               :: nbtyp21
  integer(i4)                               :: nbtyp22
  integer(i4)                               :: nbtypeij
  integer(i4)                               :: nbtypeik
  integer(i4)                               :: nbtypejk
  integer(i4)                               :: nbtypeij2
  integer(i4)                               :: nbtypeik2
  integer(i4)                               :: nbtypejk2
  integer(i4)                               :: nbondsi
  integer(i4)                               :: neq
  integer(i4)                               :: neqi
  integer(i4)                               :: neqj
  integer(i4)                               :: neqk
  integer(i4)                               :: ni
  integer(i4)                               :: nj
  integer(i4)                               :: nk
  integer(i4)                               :: nloopj
  integer(i4)                               :: nloopk
  integer(i4)                               :: nmi
  integer(i4)                               :: nmj
  integer(i4)                               :: nmk
  integer(i4)                               :: noffset
  integer(i4)                               :: npsi
  integer(i4)                               :: npsj
  integer(i4)                               :: npsk
  integer(i4)                               :: nr1
  integer(i4)                               :: nsame
  integer(i4)                               :: nt1
  integer(i4)                               :: nt2
  integer(i4)                               :: nt3
  integer(i4)                               :: ntmp
  integer(i4)                               :: nto
  integer(i4)                               :: ntot
  integer(i4)                               :: ntyp1
  integer(i4)                               :: ntyp2
  integer(i4)                               :: ntyp3
  integer(i4)                               :: ntypi
  integer(i4)                               :: ntypj
  integer(i4)                               :: ntypk
  integer(i4)                               :: ntypo
  integer(i4)                               :: status
  logical                                   :: bonded2donor
  logical                                   :: l2bonds
  logical                                   :: lbonded
  logical                                   :: lbondnoOK
  logical                                   :: lbondtypeOK
  logical                                   :: lbtyp
  logical                                   :: ldiff23typ
  logical                                   :: ldiff23cut
  logical                                   :: ldiff23bo
  logical                                   :: ldsl
  logical                                   :: lin1
  logical                                   :: lin2
  logical                                   :: lin3
  logical                                   :: linter_only
  logical                                   :: lintra_only
  logical                                   :: lmatch
  logical                                   :: lmolok
  logical                                   :: lneedmol
  logical                                   :: lnodisp
  logical                                   :: lregion1i
  logical                                   :: lregion1j
  logical                                   :: lregion1k
  logical                                   :: lsymijk
  logical                                   :: lswaprho
  logical                                   :: lswapk
  real(dp)                                  :: ang
  real(dp)                                  :: cputime
  real(dp)                                  :: d0i
  real(dp)                                  :: d0j
  real(dp)                                  :: d0k
  real(dp)                                  :: d1q(3,3)
  real(dp)                                  :: d2q(6)
  real(dp)                                  :: dot
  real(dp)                                  :: e2d(6)
  real(dp)                                  :: e3d(10)
  real(dp)                                  :: ed11
  real(dp)                                  :: ed12
  real(dp)                                  :: ed13
  real(dp)                                  :: edrr(3,3,3)
  real(dp)                                  :: ethb1
  real(dp)                                  :: ofct
  real(dp)                                  :: oci
  real(dp)                                  :: ocj
  real(dp)                                  :: ock
  real(dp)                                  :: one
  real(dp)                                  :: qli
  real(dp)                                  :: qlj
  real(dp)                                  :: qlk
  real(dp)                                  :: r12
  real(dp)                                  :: r122
  real(dp)                                  :: r13
  real(dp)                                  :: r132
  real(dp)                                  :: r23
  real(dp)                                  :: r232
  real(dp)                                  :: r12v(3)
  real(dp)                                  :: r13v(3)
  real(dp)                                  :: r23v(3)
  real(dp)                                  :: rho1
  real(dp)                                  :: rho2
  real(dp)                                  :: rho3
  real(dp)                                  :: rho4
  real(dp)                                  :: rho5
  real(dp)                                  :: rk32
  real(dp)                                  :: rk33
  real(dp)                                  :: rk34
  real(dp)                                  :: rkthb
  real(dp)                                  :: rkthb3
  real(dp)                                  :: rkthb4
  real(dp)                                  :: rktmp
  real(dp)                                  :: ro1
  real(dp)                                  :: ro2
  real(dp)                                  :: ro3
  real(dp)                                  :: ro4
  real(dp)                                  :: ro5
  real(dp)                                  :: rro
  real(dp)                                  :: symfct
  real(dp)                                  :: the0
  real(dp)                                  :: time1
  real(dp)                                  :: time2
  real(dp)                                  :: tr1
  real(dp)                                  :: tr11
  real(dp)                                  :: tr11m
  real(dp)                                  :: tr1m
  real(dp)                                  :: tr2
  real(dp)                                  :: tr21
  real(dp)                                  :: tr21m
  real(dp)                                  :: tr2m
  real(dp)                                  :: tr3
  real(dp)                                  :: tr31
  real(dp)                                  :: tr31m
  real(dp)                                  :: tr3m
  real(dp)                                  :: trm1
  real(dp)                                  :: trm11
  real(dp)                                  :: trm12
  real(dp)                                  :: trm13
  real(dp)                                  :: ttmp
  real(dp)                                  :: ttr1
  real(dp)                                  :: ttr1m
  real(dp)                                  :: ttr11
  real(dp)                                  :: ttr2
  real(dp)                                  :: ttr2m
  real(dp)                                  :: ttr21
  real(dp)                                  :: x23
  real(dp)                                  :: y23
  real(dp)                                  :: z23
  real(dp)                                  :: x21
  real(dp)                                  :: y21
  real(dp)                                  :: z21
  real(dp)                                  :: x31
  real(dp)                                  :: y31
  real(dp)                                  :: z31
  real(dp)                                  :: xc1
  real(dp)                                  :: yc1
  real(dp)                                  :: zc1
  real(dp), dimension(:), allocatable, save :: xderv
  real(dp), dimension(:), allocatable, save :: yderv
  real(dp), dimension(:), allocatable, save :: zderv
!
  time1 = cputime()
  ldsl = (ld1sym.and.(.not.lgrad2.or.ld2sym).and.imode.eq.1)
  lnodisp = (index(keyword,'r234').eq.0)
  if (.not.ldsl) then
    lin1 = .false.
    lin2 = .false.
    lin3 = .false.
  endif
!
!  Charges are just dummies here since defect calculations are not enabled with variable charges, except for n3ty = 19
!
  qli = 0.0_dp
  qlj = 0.0_dp
  qlk = 0.0_dp
!
!  Initialisation
!
  ethb = 0.0_dp
  one = 1.0_dp
  if (imode.eq.1) then
    nr1 = nreg1
  else
    nr1 = nreg1old
  endif
  ntot = nr1 + ntreg2
  noffset = 3*nreg1
  if (ldbsm) noffset = noffset + nreg1
!
!  Allocate local memory
!
  allocate(xderv(nr1),stat=status)
  if (status/=0) call outofmemory('three12','xderv')
  allocate(yderv(nr1),stat=status)
  if (status/=0) call outofmemory('three12','yderv')
  allocate(zderv(nr1),stat=status)
  if (status/=0) call outofmemory('three12','zderv')
!
!  Zero local gradient vectors
!
  do i = 1,nr1
    xderv(i) = 0.0_dp
    yderv(i) = 0.0_dp
    zderv(i) = 0.0_dp
  enddo
!*************************
!  Loop over potentials  *
!*************************
  potl: do n = 1,nthb
    n3ty = nthrty(n)
    nt1 = ntspec1(n)
    nt2 = ntspec2(n)
    nt3 = ntspec3(n)
    ntyp1 = ntptyp1(n)
    ntyp2 = ntptyp2(n)
    ntyp3 = ntptyp3(n)
    nbtyp11 = n3botype(1,1,n)
    nbtyp12 = n3botype(2,1,n)
    nbtyp21 = n3botype(1,2,n)
    nbtyp22 = n3botype(2,2,n)
    tr11m = thr1min(n)
    tr21m = thr2min(n)
    tr31m = thr3min(n)
    tr11 = thr1(n)
    tr21 = thr2(n)
    tr31 = thr3(n)
!
!  Check that atomic numbers match
!
    ldiff23typ = (ntyp2.eq.ntyp3.and.nt2.eq.nt3)
!
!  ..and the cutoffs..
!
    ldiff23cut = (tr11.eq.tr21.and.tr11m.eq.tr21m)
!
!  ..and the bond orders
!
    ldiff23bo = (nbtyp11.ne.nbtyp21.or.nbtyp12.ne.nbtyp22)
!
    tr1m = tr11m*tr11m
    tr2m = tr21m*tr21m
    tr3m = tr31m*tr31m
    tr1 = tr11*tr11
    tr2 = tr21*tr21
    tr3 = tr31*tr31
    lbtyp = (mmtexc(n).eq.1)
    rkthb = thbk(n)
    rkthb3 = 0.0_dp
    rkthb4 = 0.0_dp
    ro1 = 0.0_dp
    ro2 = 0.0_dp
    ro3 = 0.0_dp
    ro4 = 0.0_dp
    ro5 = 0.0_dp
    if (n3ty.eq.2) then
      the0 = theta(n)*degtorad
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      if (ro1.ne.0.0_dp) ro1 = 1.0_dp/ro1
      if (ro2.ne.0.0_dp) ro2 = 1.0_dp/ro2
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.1) then
      the0 = theta(n)*degtorad
      rkthb3 = thrho2(n)/6.0_dp
      rkthb4 = thrho1(n)/24.0_dp
      lsymijk = .false.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.3) then
      the0 = 0.0_dp
      lsymijk = .true.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.4) then
      the0 = 0.0_dp
      ro1 = theta(n)
      ro2 = thrho1(n)
      ro3 = thrho2(n)
      lsymijk = .true.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.5) then
      the0 = cos(theta(n)*degtorad)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.6) then
      the0 = 0.0_dp
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.7) then
      the0 = theta(n)
      lsymijk = .false.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.8) then
      the0 = theta(n)*degtorad
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      if (ro1.ne.0.0_dp) ro1 = 1.0_dp/ro1
      if (ro2.ne.0.0_dp) ro2 = 1.0_dp/ro2
      the0 = the0 - pi
      the0 = the0*the0
      rkthb = 0.25_dp*rkthb/the0
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.9) then
      the0 = cos(theta(n)*degtorad)
      lsymijk = .false.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.10) then
      the0 = theta(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      ro3 = thrho3(n)
      lsymijk = .true.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.11) then
      rkthb3 = thrho3(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      the0 = theta(n)*degtorad
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .true.
    elseif (n3ty.eq.12) then
      the0 = theta(n)
      rkthb3 = nint(thrho1(n))
      lsymijk = .false.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.13) then
      the0 = theta(n)
      rkthb3 = thrho3(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)  
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.14) then
      the0 = cos(theta(n)*degtorad)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      ro3 = thrho3(n)
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.15) then
      rkthb3 = theta(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      ro3 = thrho3(n)
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.16) then
      the0 = theta(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.17) then
      the0 = theta(n)*degtorad
      rkthb4 = 1.0_dp/(2.0_dp*sin(the0))**2
      rkthb3 = - 4.0_dp*rkthb4*cos(the0)
      the0 = rkthb4*(2.0_dp*cos(the0)**2 + 1.0_dp)
      lsymijk = .false.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.18) then
      rkthb3 = thrho3(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      the0 = cos(theta(n)*degtorad)
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .true.
    elseif (n3ty.eq.19) then
      lsymijk = .false.
      lswaprho = .false.
      lswapk = .false.
    elseif (n3ty.eq.20) then
      the0 = 0.0_dp
      ro1 = theta(n)
      ro2 = thrho2(n)
      ro4 = thrho1(n)
      ro5 = thrho3(n)
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .false.
    elseif (n3ty.eq.21) then
      the0 = theta(n)
      lsymijk = .false.
      lswaprho = .false.
    elseif (n3ty.eq.22) then
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      the0 = theta(n)*degtorad
      lsymijk = .false.
      lswaprho = .true.
      lswapk = .true.
    endif
    lintra_only = (ltintra(n).and..not.ltinter(n))
    linter_only = (ltinter(n).and..not.ltintra(n))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Work out symmetry factor for symmetric potentials
!
    if (lsymijk) then
      nsame = 0
      if (nt1.eq.nt2.and.ntyp1.eq.ntyp2) nsame = nsame + 1
      if (nt1.eq.nt3.and.ntyp1.eq.ntyp3) nsame = nsame + 1
      if (nsame.eq.0) then
        symfct = 1.0_dp
      elseif (nsame.eq.1) then
        symfct = 0.5_dp
      elseif (nsame.eq.2) then
        symfct = 1.0_dp/3.0_dp
      endif
    endif
!**************************
!  Outer loop over sites  *
!**************************
    iloop: do i = 1,ntot
      if (i.le.nr1) then
!
!  Region 1 ion
!
        lregion1i = .true.
        if (imode.eq.1) then
          ni = natdefe(i)
          ntypi = ntypdefe(i)
          xc1 = xdefe(i)
          yc1 = ydefe(i)
          zc1 = zdefe(i)
          oci = occdefe(i)
          if (lmol.and.lneedmol) then
            nmi = ndefmol(i)
            indm = ndefind(i)
          endif
          if (nreldef(i).gt.0) then
            npsi = nreldef(i)
          else
            npsi = 0
          endif
          nbondsi = nbondsdef(i)
        else
          ni = natp(i)
          ntypi = ntypep(i)
          xc1 = xperf(i)
          yc1 = yperf(i)
          zc1 = zperf(i)
          oci = occp(i)
          if (lmol.and.lneedmol) then
            nmi = ndefmolp(i)
            indm = ndefindp(i)
          endif
          npsi = npsite(i)
          nbondsi = nbonds(npsi)
        endif
        if (ldsl) then
          lin1 = (ndrelop(i).eq.1)
          ir = ndrel(i)
          neqi = ndeqv(ir)
        endif
      else
!
!  Region 2 ion
!
        lregion1i = .false.
        lin1 = .false.
        ii = i - nr1
        ni = nr2a(ii)
        ntypi = ntr2a(ii)
        if (lnodisp) then
          xc1 = xr2a(ii)
          yc1 = yr2a(ii)
          zc1 = zr2a(ii)
        else
          xc1 = xr2a(ii) + xdis(ii)
          yc1 = yr2a(ii) + ydis(ii)
          zc1 = zr2a(ii) + zdis(ii)
        endif
        oci = or2a(ii)
        if (lmol.and.lneedmol) then
          nmi = nmr2a(ii)
          indm = nmir2a(ii)
        endif
        npsi = nps(ii)
        nbondsi = nbonds(npsi)
      endif
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
!     
!  Check number of bonds if necessary
!  
      if (n3bondnono(1,n).gt.0) then
        lbondnoOK = .false.
        do in3 = 1,n3bondnono(1,n)
          if (nbondsi.eq.n3bondno(in3,1,n)) lbondnoOK = .true.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
      if (n3bondnono(2,n).gt.0) then
        lbondnoOK = .true.
        do in3 = 1,n3bondnono(2,n)
          if (nbondsi.eq.n3bondno(in3,2,n)) lbondnoOK = .false.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
! 
!  Dreiding option handling                 
! 
      if (ltdreiding(n)) then               
        if (.not.bonded2donor(npsi)) cycle iloop
      endif
!
      if (lregion1i) then
        nloopj = ntot
      else
        nloopj = nr1
      endif
!*******************************
!  Inner loop over first site  *
!*******************************
!
!  Do loop in reverse order to minimise use of region 2
!
      jloop: do j = 1,nloopj
        if (j.le.nr1) then
!
!  Region 1 ion
!
          lregion1j = .true.
          if (imode.eq.1) then
            nj = natdefe(j)
            ntypj = ntypdefe(j)
            x21 = xdefe(j) - xc1
            y21 = ydefe(j) - yc1
            z21 = zdefe(j) - zc1
            ocj = occdefe(j)
            qlj = qdefe(j)
            if (lmol.and.lneedmol) then
              nmj = ndefmol(j)
              indmj = ndefind(j)
            endif
            if (nreldef(j).gt.0) then
              npsj = nreldef(j)
            else
              npsj = 0
            endif
          else
            nj = natp(j)
            ntypj = ntypep(j)
            x21 = xperf(j) - xc1
            y21 = yperf(j) - yc1
            z21 = zperf(j) - zc1
            qlj = qp(j)
            ocj = occp(j)
            if (lmol.and.lneedmol) then
              nmj = ndefmolp(j)
              indmj = ndefindp(j)
            endif
            npsj = npsite(j)
          endif
          if (ldsl) then
            lin2 = (ndrelop(j).eq.1)
            jr = ndrel(j)
            neqj = ndeqv(jr)
          endif
        else
!
!  Region 2 ion
!
          lregion1j = .false.
          lin2 = .false.
          jj = j - nr1
          nj = nr2a(jj)
          ntypj = ntr2a(jj)
          if (lnodisp) then
            x21 = xr2a(jj) - xc1
            y21 = yr2a(jj) - yc1
            z21 = zr2a(jj) - zc1
          else
            x21 = xr2a(jj) - xc1 + xdis(jj)
            y21 = yr2a(jj) - yc1 + ydis(jj)
            z21 = zr2a(jj) - zc1 + zdis(jj)
          endif
          ocj = or2a(jj)
          qlj = qr2a(jj)
          if (lmol.and.lneedmol) then
            nmj = nmr2a(jj)
            indmj = nmir2a(jj)
          endif
          npsj = nps(jj)
        endif
!
!  Check j is allowed for n
!
        if (lmatch(nj,ntypj,nt2,ntyp2,.false.)) then
          nto = nt3
          ntypo = ntyp3
          nbotyp11 = nbtyp11
          nbotyp12 = nbtyp12
          nbotyp21 = nbtyp21
          nbotyp22 = nbtyp22
          ttr1m = tr1m
          ttr2m = tr2m
          ttr1 = tr1
          ttr2 = tr2
          ttr11 = tr11
          ttr21 = tr21
          rho1 = ro1
          rho2 = ro2
          rho3 = ro3
          rho4 = ro4
          rho5 = ro5
        elseif (lmatch(nj,ntypj,nt3,ntyp3,.true.)) then
          nto = nt2
          ntypo = ntyp2
          nbotyp11 = nbtyp21
          nbotyp12 = nbtyp22
          nbotyp21 = nbtyp11
          nbotyp22 = nbtyp12
          ttr1m = tr2m
          ttr2m = tr1m
          ttr1 = tr2
          ttr2 = tr1
          ttr11 = tr21
          ttr21 = tr11
          rho1 = ro2
          rho2 = ro1
          rho3 = ro3
          rho4 = ro5
          rho5 = ro4
        else
          cycle jloop
        endif
        lbonded = .false.
        if (lmol.and.lneedmol) then
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
          if (lmolok) then
            ind = indmj - indm
            lmolok = (ind.eq.0)
            if (.not.lmolok) then
              call mindtoijk(indmj,jxx,jyy,jzz)
              call mindtoijk(indm,ixx,iyy,izz)
              jxx = jxx - ixx
              jyy = jyy - iyy
              jzz = jzz - izz
              call samemol(lmolok,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
            endif
          endif
          if (lmolok.and.linter_only) cycle jloop
          if (.not.lmolok.and.lintra_only) cycle jloop
          if (lmolok.and.lbtyp) then
            if (imode.eq.1) then
              if (lregion1i.and.lregion1j) then
                lbonded = .false.
                icm = 1
                do while (icm.le.nbondsdef(i).and..not.lbonded)
                  imm = nbondeddef(icm,i)
                  lbonded = (imm.eq.j)
                  icm = icm + 1
                enddo
              else
                if (npsi.gt.0.and.npsj.gt.0) then
                  call mindtoijk(indmj,jxx,jyy,jzz)
                  call mindtoijk(indm,ixx,iyy,izz)
                  jxx = jxx - ixx
                  jyy = jyy - iyy
                  jzz = jzz - izz
                  indmmj = nmolind(npsj)
                  indmmi = nmolind(npsi)
                  call mindtoijk(indmmj,jxx2,jyy2,jzz2)
                  call mindtoijk(indmmi,ixx2,iyy2,izz2)
                  jxx = jxx + jxx2 - ixx2
                  jyy = jyy + jyy2 - iyy2
                  jzz = jzz + jzz2 - izz2
                  call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,npsi,npsj,jxx,jyy,jzz)
                else
                  lbonded = .false.
                endif
              endif
            else
              if (npsi.gt.0.and.npsj.gt.0) then
                call mindtoijk(indmj,jxx,jyy,jzz)
                call mindtoijk(indm,ixx,iyy,izz)
                jxx = jxx - ixx
                jyy = jyy - iyy
                jzz = jzz - izz
                indmmj = nmolind(npsj)
                indmmi = nmolind(npsi)
                call mindtoijk(indmmj,jxx2,jyy2,jzz2)
                call mindtoijk(indmmi,ixx2,iyy2,izz2)
                jxx = jxx + jxx2 - ixx2
                jyy = jyy + jyy2 - iyy2
                jzz = jzz + jzz2 - izz2
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,npsi,npsj,jxx,jyy,jzz)
              else
                lbonded = .false.
              endif
            endif
            if (.not.lbonded) cycle jloop
          endif
        else
          lmolok = .false.
        endif
        if (lbtyp.and..not.lmolok) cycle jloop
!
!  Check r12 is OK
!
        r122 = x21*x21 + y21*y21 + z21*z21
        if (r122.lt.1.0d-12) cycle jloop
        if ((r122.gt.ttr1.or.r122.lt.ttr1m).and.(.not.lbtyp.or..not.lbonded)) then
          if (ldiff23typ.or..not.ldiff23cut) cycle jloop
          if (r122.lt.ttr2.and.r122.gt.ttr2m) then
            ttmp = ttr2m
            ttr2m = ttr1m
            ttr1m = ttmp
            ttmp = ttr2
            ttr2 = ttr1
            ttr1 = ttmp
            if (lswaprho) then
              rro = rho1
              rho1 = rho2
              rho2 = rro
              rro = rho4
              rho4 = rho5
              rho5 = rro
              if (lswapk) then
                rktmp = rkthb
                rkthb = rkthb3
                rkthb3 = rktmp
              endif
            endif
          else
            cycle jloop
          endif
        endif
        r12 = sqrt(r122)
!
!  Select correct looping for third loop
!  ksub is used to correct pointer for derivatives
!
        if (lregion1i) then
          nloopk = j - 1
          ksub = 0
        else
          nloopk = ntreg2 + j - 1
          ksub = ntreg2
        endif
!********************************
!  Inner loop over second site  *
!********************************
        kloop: do k = 1,nloopk
          if (lregion1i) then
            if (k.le.nr1) then
!
!  Region 1 ion
!
              if (imode.eq.1) then
                nk = natdefe(k)
                ntypk = ntypdefe(k)
                x31 = xdefe(k) - xc1
                y31 = ydefe(k) - yc1
                z31 = zdefe(k) - zc1
                ock = occdefe(k)
                qlk = qdefe(k)
                if (lmol.and.lneedmol) then
                  nmk = ndefmol(k)
                  indmk = ndefind(k)
                endif
                if (nreldef(k).gt.0) then
                  npsk = nreldef(k)
                else
                  npsk = 0
                endif
              else
                nk = natp(k)
                ntypk = ntypep(k)
                x31 = xperf(k) - xc1
                y31 = yperf(k) - yc1
                z31 = zperf(k) - zc1
                qlk = qp(k)
                ock = occp(k)
                if (lmol.and.lneedmol) then
                  nmk = ndefmolp(k)
                  indmk = ndefindp(k)
                endif
                npsk = npsite(k)
              endif
              if (ldsl) then
                lin3 = (ndrelop(k).eq.1)
                kr = ndrel(k)
                neqk = ndeqv(kr)
              endif
!
!  Region 2 ion
!
            else
              lin3 = .false.
              kk = k - nr1
              nk = nr2a(kk)
              ntypk = ntr2a(kk)
              if (lnodisp) then
                x31 = xr2a(kk) - xc1
                y31 = yr2a(kk) - yc1
                z31 = zr2a(kk) - zc1
              else
                x31 = xr2a(kk) - xc1 + xdis(kk)
                y31 = yr2a(kk) - yc1 + ydis(kk)
                z31 = zr2a(kk) - zc1 + zdis(kk)
              endif
              ock = or2a(kk)
              qlk = qr2a(kk)
              if (lmol.and.lneedmol) then
                nmk = nmr2a(kk)
                indmk = nmir2a(kk)
              endif
              npsk = nps(kk)
            endif
            if (k.le.nr1) then
              lregion1k = .true.
            else
              lregion1k = .false.
            endif
          else
!
!  Region 2 ion
!
            if (k.le.ntreg2) then
              nk = nr2a(k)
              ntypk = ntr2a(k)
              if (lnodisp) then
                x31 = xr2a(k) - xc1
                y31 = yr2a(k) - yc1
                z31 = zr2a(k) - zc1
              else
                x31 = xr2a(k) - xc1 + xdis(k)
                y31 = yr2a(k) - yc1 + ydis(k)
                z31 = zr2a(k) - zc1 + zdis(k)
              endif
              ock = or2a(k)
              qlk = qr2a(k)
              if (lmol.and.lneedmol) then
                nmk = nmr2a(k)
                indmk = nmir2a(k)
              endif
              lin3 = .false.
              npsk = nps(k)
            else
!
!  Region 1 ion
!
              kk = k - ntreg2
              if (imode.eq.1) then
                nk = natdefe(kk)
                ntypk = ntypdefe(kk)
                x31 = xdefe(kk) - xc1
                y31 = ydefe(kk) - yc1
                z31 = zdefe(kk) - zc1
                ock = occdefe(kk)
                qlk = qdefe(kk)
                if (lmol.and.lneedmol) then
                  nmk = ndefmol(kk)
                  indmk = ndefind(kk)
                endif
                if (nreldef(kk).gt.0) then
                  npsk = nreldef(kk)
                else
                  npsk = 0
                endif
              else
                nk = natp(kk)
                ntypk = ntypep(kk)
                x31 = xperf(kk) - xc1
                y31 = yperf(kk) - yc1
                z31 = zperf(kk) - zc1
                ock = occp(kk)
                qlk = qp(kk)
                if (lmol.and.lneedmol) then
                  nmk = ndefmolp(kk)
                  indmk = ndefindp(kk)
                endif
                npsk = npsite(kk)
              endif
              if (ldsl) then
                lin3 = (ndrelop(kk).eq.1)
                kr = ndrel(kk)
                neqk = ndeqv(kr)
              endif
            endif
            if (k.le.ntreg2) then
              lregion1k = .false.
            else
              lregion1k = .true.
            endif
          endif
!
!  Check k is allowed for n, and not equivalent to j
!
          if (.not.lmatch(nk,ntypk,nto,ntypo,.true.)) cycle kloop
!
!  Molecularity check
!
          lbonded = .false.
          if (lmol.and.lneedmol) then
            lmolok = (nmi.eq.nmk.and.nmi.gt.0)
            if (lmolok) then
              ind = indmk - indm
              lmolok = (ind.eq.0)
              if (.not.lmolok) then
                call mindtoijk(indmk,kxx,kyy,kzz)
                call mindtoijk(indm,ixx,iyy,izz)
                kxx = kxx - ixx
                kyy = kyy - iyy
                kzz = kzz - izz
                call samemol(lmolok,nmi,kxx,kyy,kzz,0_i4,0_i4,0_i4)
              endif
            endif
            if (lmolok.and.linter_only) cycle kloop
            if (.not.lmolok.and.lintra_only) cycle kloop
            if (lmolok.and.lbtyp) then
              if (imode.eq.1) then
                if (lregion1i.and.lregion1k) then
                  icm = 1
                  lbonded = .false.
                  do while (icm.le.nbondsdef(i).and..not.lbonded)
                    imm = nbondeddef(icm,i)
                    lbonded = (imm.eq.k)
                    icm = icm + 1
                  enddo
                else
                  if (npsi.gt.0.and.npsk.gt.0) then
                    call mindtoijk(indmk,kxx,kyy,kzz)
                    call mindtoijk(indm,ixx,iyy,izz)
                    kxx = kxx - ixx
                    kyy = kyy - iyy
                    kzz = kzz - izz
                    indmmk = nmolind(npsk)
                    indmmi = nmolind(npsi)
                    call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                    call mindtoijk(indmmi,ixx2,iyy2,izz2)
                    kxx = kxx + kxx2 - ixx2
                    kyy = kyy + kyy2 - iyy2
                    kzz = kzz + kzz2 - izz2
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,npsi,npsk,kxx,kyy,kzz)
                  else
                    lbonded = .false.
                  endif
                endif
              else
                if (npsi.gt.0.and.npsk.gt.0) then
                  call mindtoijk(indmk,kxx,kyy,kzz)
                  call mindtoijk(indm,ixx,iyy,izz)
                  kxx = kxx - ixx
                  kyy = kyy - iyy
                  kzz = kzz - izz
                  indmmk = nmolind(npsk)
                  indmmi = nmolind(npsi)
                  call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                  call mindtoijk(indmmi,ixx2,iyy2,izz2)
                  kxx = kxx + kxx2 - ixx2
                  kyy = kyy + kyy2 - iyy2
                  kzz = kzz + kzz2 - izz2
                  call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,npsi,npsk,kxx,kyy,kzz)
                else
                  lbonded = .false.
                endif
              endif
              if (.not.lbonded) cycle kloop
!
!  Symmetric potential bond check
!
              if (lsymijk) then
                if (imode.eq.1) then
                  if (lregion1j.and.lregion1k) then
                    icm = 1
                    lbonded = .false.
                    do while (icm.le.nbondsdef(j).and..not.lbonded)
                      imm = nbondeddef(icm,j)
                      lbonded = (imm.eq.k)
                      icm = icm + 1
                    enddo
                  else
                    if (npsj.gt.0.and.npsk.gt.0) then
                      call mindtoijk(indmk,kxx,kyy,kzz)
                      call mindtoijk(indmj,jxx,jyy,jzz)
                      kxx = kxx - jxx
                      kyy = kyy - jyy
                      kzz = kzz - jzz
                      indmmk = nmolind(npsk)
                      indmmj = nmolind(npsj)
                      call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                      call mindtoijk(indmmj,jxx2,jyy2,jzz2)
                      kxx = kxx + kxx2 - jxx2
                      kyy = kyy + kyy2 - jyy2
                      kzz = kzz + kzz2 - jzz2
                      call bonded(lbonded,l2bonds,nbtypejk,nbtypejk2,npsj,npsk,kxx,kyy,kzz)
                    else
                      lbonded = .false.
                    endif
                  endif
                else
                  if (npsj.gt.0.and.npsk.gt.0) then
                    call mindtoijk(indmk,kxx,kyy,kzz)
                    call mindtoijk(indmj,jxx,jyy,jzz)
                    kxx = kxx - jxx
                    kyy = kyy - jyy
                    kzz = kzz - jzz
                    indmmk = nmolind(npsk)
                    indmmj = nmolind(npsj)
                    call mindtoijk(indmmk,kxx2,kyy2,kzz2)
                    call mindtoijk(indmmj,jxx2,jyy2,jzz2)
                    kxx = kxx + kxx2 - jxx2
                    kyy = kyy + kyy2 - jyy2
                    kzz = kzz + kzz2 - jzz2
                    call bonded(lbonded,l2bonds,nbtypejk,nbtypejk2,npsj,npsk,kxx,kyy,kzz)
                  else
                    lbonded = .false.
                  endif
                endif
                if (.not.lbonded) cycle kloop
              endif
            endif
          else
            lmolok = .false.
          endif
          if (lbtyp.and..not.lmolok) cycle kloop
!
!  Check r13 is OK
!
          r132 = x31*x31 + y31*y31 + z31*z31
          if (r132.lt.1.0d-12.and.(.not.lbtyp.or..not.lbonded)) cycle kloop
!
!  Modification to handle case where species for 2 and 3 are the same
!  but cutoffs are different
!
          if (r132.gt.ttr2.or.r132.lt.ttr2m) then
            if (ldiff23typ.or..not.ldiff23cut) cycle kloop
            if (r122.gt.ttr2.or.r132.gt.ttr1) cycle kloop
            if (r122.lt.ttr2m.or.r132.lt.ttr1m) cycle kloop
            if (lswaprho) then
              rro = rho1
              rho1 = rho2
              rho2 = rro
              rro = rho4
              rho4 = rho5
              rho5 = rro
              if (lswapk) then
                rktmp = rkthb
                rkthb = rkthb3
                rkthb3 = rktmp
              endif
            endif
          endif
          if (lbtyp) then
!
!  Bond type checking
!  
!  If we've made it this far then the atoms must be bonded. We just have
!  to check if the bond orders match.
!               
            lbondtypeOK = .true.
!               
!  Check i-j bond for correct order
!               
            if (nbotyp11.gt.0.and.nbotyp11.ne.nbtypeij) lbondtypeOK = .false.
            if (nbotyp12.gt.1.and.nbotyp12.ne.nbtypeij2) lbondtypeOK = .false.
!               
!  Check i-k bond for correct order
!               
            if (nbotyp21.gt.0.and.nbotyp21.ne.nbtypeik) lbondtypeOK = .false.
            if (nbotyp22.gt.1.and.nbotyp22.ne.nbtypeik2) lbondtypeOK = .false.
!               
            if (.not.lbondtypeOK) then
!  
!  If bond types don't match, but atom types are symmetric then try other permutation
!                 
              if (.not.ldiff23typ.or..not.ldiff23bo) cycle kloop
!  
!  Check i-j bond for correct order
!                 
              if (nbotyp21.gt.0.and.nbotyp21.ne.nbtypeij) cycle kloop
              if (nbotyp22.gt.1.and.nbotyp22.ne.nbtypeij2) cycle kloop
!               
!  Check i-k bond for correct order
!                 
              if (nbotyp11.gt.0.and.nbotyp11.ne.nbtypeik) cycle kloop
              if (nbotyp12.gt.1.and.nbotyp12.ne.nbtypeik2) cycle kloop
!  
!  If we make it to here then bond orders are the wrong way wrong and i-j/i-k terms should be swapped
!
              if (lswaprho) then
                ntmp = nbotyp11
                nbotyp11 = nbotyp21
                nbotyp21 = ntmp
                ntmp = nbotyp12
                nbotyp12 = nbotyp22
                nbotyp22 = ntmp
                rro = rho1
                rho1 = rho2
                rho2 = rro
                rro = rho4
                rho4 = rho5
                rho5 = rro
                if (lswapk) then
                  rktmp = rkthb
                  rkthb = rkthb3
                  rkthb3 = rktmp
                endif
              endif
            endif
          endif
!
          r13 = sqrt(r132)
!
!  Check r23 is OK
!
          x23 = x31 - x21
          y23 = y31 - y21
          z23 = z31 - z21
          r232 = x23**2 + y23**2 + z23**2
          if (r232.gt.tr3.and..not.lbtyp) cycle kloop
          if (r232.lt.tr3m.or.r232.lt.1d-12) cycle kloop
!
!  Valid three-body term  = > calculate potential
!
          r23v(1) = - x23
          r23v(2) = - y23
          r23v(3) = - z23
          r12v(1) = - x21
          r12v(2) = - y21
          r12v(3) = - z21
          r13v(1) = - x31
          r13v(2) = - y31
          r13v(3) = - z31
          if (n3ty.ne.3.and.n3ty.ne.4.and.n3ty.ne.6.and.n3ty.ne.7.and.n3ty.ne.19) then
            dot = x21*x31 + y21*y31 + z21*z31
            dot = dot/(r12*r13)
            if (abs(dot).gt.0.999999999999_dp) dot = sign(one,dot)
            if (n3ty.eq.9) then
              ang = dot
            else
              ang = acos(dot)
            endif
          else
            dot = 0.0_dp
            ang = 0.0_dp
          endif
          r23 = sqrt(r232)
          ofct = oci*ocj*ock
          if (lsymijk) ofct = ofct*symfct
          if (n3ty.eq.19) then
            rk32 = rkthb*ofct*qlj*qlk
          else
            rk32 = rkthb*ofct
          endif
          if (n3ty.eq.12.or.n3ty.eq.17) then
            rk33 = rkthb3
          else
            rk33 = rkthb3*ofct
          endif
          if (n3ty.eq.17) then
            rk34 = rkthb4
          else
            rk34 = rkthb4*ofct
          endif
          if (n3ty.eq.15) then
            rho1 = thrho1(n)
            rho2 = thrho2(n)
            rho3 = thrho3(n)
          elseif (n3ty.eq.16) then
            rho1 = thrho1(n)
            rho2 = thrho2(n)
          endif
!*****************************************************
!  Calculate derivatives with respect to potentials  *
!*****************************************************
          call threebody(1_i4,n3ty,r12,r13,r23,ed11,ed12,ed13,ethb1,e2d,e3d,ttr11,ttr21,tr31,rho1,rho2, &
                         rho3,rho4,rho5,rk32,rk33,rk34,the0,ang,dot,lgrad1,lgrad2,.false.,n,qli,qlj,qlk, &
                         d0i,d0j,d0k,d1q,d2q,lthetataper(n),thetatapermin(n),thetatapermax(n))
          ethb = ethb + ethb1
!*************************
!  Start of derivatives  *
!*************************
!
!  Check that at least one species is in the asymmetric unit
!  for symmetry adapted run otherwise gradients can be skipped.
!
          if (ldsl.and..not.lin1.and..not.lin2.and..not.lin3) cycle kloop
          if (lgrad2) then
            if (lregion1i) then
              n1x = 3*(i-1) + 1
              if (lin1) then
                n1xr = 3*(ir-1) + 1
              endif
            else
              n1x = noffset + 1
            endif
            if (lregion1j) then
              n2x = 3*(j-1) + 1
              if (lin2) then
                n2xr = 3*(jr-1) + 1
              endif
            else
              n2x = noffset + 1
            endif
            if (lregion1k) then
              n3x = 3*(k-ksub-1) + 1
              if (lin3) then
                n3xr = 3*(kr-1) + 1
              endif
            else
              n3x = noffset + 1
            endif
          endif
!*************************
!  Internal derivatives  *
!*************************
          if (lgrad1) then
            if (lregion1i) then
              xderv(i) = xderv(i) - x21*ed11 - x31*ed12
              yderv(i) = yderv(i) - y21*ed11 - y31*ed12
              zderv(i) = zderv(i) - z21*ed11 - z31*ed12
            endif
            if (lregion1j) then
              xderv(j) = xderv(j) + x21*ed11 - x23*ed13
              yderv(j) = yderv(j) + y21*ed11 - y23*ed13
              zderv(j) = zderv(j) + z21*ed11 - z23*ed13
            endif
            if (lregion1k) then
              xderv(k-ksub) = xderv(k-ksub) + x31*ed12 + x23*ed13
              yderv(k-ksub) = yderv(k-ksub) + y31*ed12 + y23*ed13
              zderv(k-ksub) = zderv(k-ksub) + z31*ed12 + z23*ed13
            endif
          endif
          if (lgrad2) then
            edrr(1,1,1) = e2d(2)*x21*x31
            edrr(1,2,1) = e2d(2)*y21*x31
            edrr(1,3,1) = e2d(2)*z21*x31
            edrr(1,1,2) = e2d(2)*x21*y31
            edrr(1,2,2) = e2d(2)*y21*y31
            edrr(1,3,2) = e2d(2)*z21*y31
            edrr(1,1,3) = e2d(2)*x21*z31
            edrr(1,2,3) = e2d(2)*y21*z31
            edrr(1,3,3) = e2d(2)*z21*z31
            edrr(2,1,1) = e2d(3)*x21*x23
            edrr(2,2,1) = e2d(3)*y21*x23
            edrr(2,3,1) = e2d(3)*z21*x23
            edrr(2,1,2) = e2d(3)*x21*y23
            edrr(2,2,2) = e2d(3)*y21*y23
            edrr(2,3,2) = e2d(3)*z21*y23
            edrr(2,1,3) = e2d(3)*x21*z23
            edrr(2,2,3) = e2d(3)*y21*z23
            edrr(2,3,3) = e2d(3)*z21*z23
            edrr(3,1,1) = e2d(5)*x31*x23
            edrr(3,2,1) = e2d(5)*y31*x23
            edrr(3,3,1) = e2d(5)*z31*x23
            edrr(3,1,2) = e2d(5)*x31*y23
            edrr(3,2,2) = e2d(5)*y31*y23
            edrr(3,3,2) = e2d(5)*z31*y23
            edrr(3,1,3) = e2d(5)*x31*z23
            edrr(3,2,3) = e2d(5)*y31*z23
            edrr(3,3,3) = e2d(5)*z31*z23
            do kk = 1,3
              n11 = n1x - 1  + kk
              n21 = n2x - 1 + kk
              n31 = n3x - 1 + kk
!
!  First term
!
              if (ld2sym) then
                if (lin1) then
                  n11r = n1xr - 1 + kk
                  derv2(n21,n11r) = derv2(n21,n11r) - ed11*neqi
                  derv2(n31,n11r) = derv2(n31,n11r) - ed12*neqi
                endif
                if (lin2) then
                  n21r = n2xr - 1 + kk
                  derv2(n11,n21r) = derv2(n11,n21r) - ed11*neqj
                  derv2(n31,n21r) = derv2(n31,n21r) - ed13*neqj
                endif
                if (lin3) then
                  n31r = n3xr - 1 + kk
                  derv2(n11,n31r) = derv2(n11,n31r) - ed12*neqk
                  derv2(n21,n31r) = derv2(n21,n31r) - ed13*neqk
                endif
              else
                derv2(n21,n11) = derv2(n21,n11) - ed11
                derv2(n31,n11) = derv2(n31,n11) - ed12
                derv2(n11,n21) = derv2(n11,n21) - ed11
                derv2(n31,n21) = derv2(n31,n21) - ed13
                derv2(n11,n31) = derv2(n11,n31) - ed12
                derv2(n21,n31) = derv2(n21,n31) - ed13
              endif
!
!  Second term
!
              do kl = 1,3
                n12 = n1x - 1 + kl
                n22 = n2x - 1 + kl
                n32 = n3x - 1 + kl
                trm11 = edrr(1,kk,kl)
                trm12 = edrr(2,kk,kl)
                trm13 = edrr(3,kk,kl)
                if (ld2sym) then
                  if (lin1) then
                    n12r = n1xr - 1 + kl
                    trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                    derv2(n22,n11r) = derv2(n22,n11r) + trm1*neqi
                    derv2(n21,n12r) = derv2(n21,n12r) - trm11*neqi
                    trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                    derv2(n32,n11r) = derv2(n32,n11r)+trm1*neqi
                  endif
                  if (lin2) then
                    n22r = n2xr - 1 + kl
                    trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                    derv2(n11,n22r) = derv2(n11,n22r) + trm1*neqj
                    derv2(n12,n21r) = derv2(n12,n21r) - trm11*neqj
                    trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                    derv2(n32,n21r) = derv2(n32,n21r) + trm1*neqj
                    derv2(n31,n22r) = derv2(n31,n22r) - trm13*neqj
                  endif
                  if (lin3) then
                    n32r = n3xr - 1 + kl
                    trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                    derv2(n11,n32r) = derv2(n11,n32r) + trm1*neqk
                    trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                    derv2(n21,n32r) = derv2(n21,n32r) + trm1*neqk
                    derv2(n22,n31r) = derv2(n22,n31r) - trm13*neqk
                  endif
                else
                  trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                  derv2(n11,n22) = derv2(n11,n22) + trm1
                  derv2(n22,n11) = derv2(n22,n11) + trm1
                  derv2(n12,n21) = derv2(n12,n21) - trm11
                  derv2(n21,n12) = derv2(n21,n12) - trm11
                  trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                  derv2(n11,n32) = derv2(n11,n32) + trm1
                  derv2(n32,n11) = derv2(n32,n11) + trm1
                  trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                  derv2(n21,n32) = derv2(n21,n32) + trm1
                  derv2(n32,n21) = derv2(n32,n21) + trm1
                  derv2(n22,n31) = derv2(n22,n31) - trm13
                  derv2(n31,n22) = derv2(n31,n22) - trm13
                endif
              enddo
            enddo
          endif
!
!  End of inner loops
!
        enddo kloop
      enddo jloop
!
!  End of outer loops
!
    enddo iloop
  enddo potl
  if (lgrad1) then
    if (ldsl) then
!
!  Symmetry reduce gradients
!
      do i = 1,ndasym
        ii = ndsptr(i)
        neq = ndeqv(i)
        xdrv(i) = xdrv(i) + neq*xderv(ii)
        ydrv(i) = ydrv(i) + neq*yderv(ii)
        zdrv(i) = zdrv(i) + neq*zderv(ii)
      enddo
    else
!
!  Add to main gradient vectors
!
      do i = 1,nr1
        xdrv(i) = xdrv(i) + xderv(i)
        ydrv(i) = ydrv(i) + yderv(i)
        zdrv(i) = zdrv(i) + zderv(i)
      enddo
    endif
  endif
!
!  Free local memory
!
  deallocate(zderv,stat=status)
  if (status/=0) call deallocate_error('three12','zderv')
  deallocate(yderv,stat=status)
  if (status/=0) call deallocate_error('three12','yderv')
  deallocate(xderv,stat=status)
  if (status/=0) call deallocate_error('three12','xderv')
!
!  Timing
!
  time2 = cputime()
  treg3 = treg3 + time2 - time1
!
  return
  end
