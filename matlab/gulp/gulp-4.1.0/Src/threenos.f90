  subroutine threenos(ethb,esregion12,esregion2,eattach,lgrad1,lgrad2)
!
!  Subroutine for three-body energy
!  Modified for distance dependent three body terms
!
!  Strategy - sift by potential first, then cutoffs
!
!  lsymijk = if .true. then potential is symmetric w.r.t. i,j and k
!            and is not of angle centred form
!
!  11/94 Axilrod-Teller now added
!   1/95 Intra/intermolecular specification added
!   1/95 K3 added for three-body potential
!   2/95 Exponential and SW three-body potentials added
!   2/95 Bonded specification added for three-body terms
!   3/95 Bcross potential type added
!   3/95 Periodic molecule corrections added
!   6/95 Correction for different cutoffs for same species added
!   3/96 Urey-Bradley potential added
!   4/96 Exponentially decaying Vessal form added
!   6/96 General theta0 added to SW3
!  11/96 Compression of second derivatives added if lfreeze=.true.
!        Freezing also added for three body terms
!   9/97 Bug in sw3 derivatives fixed
!   3/98 Cosine-harmonic form added
!   4/98 Small constant added to rtrm1/rtrm2 in SW3 to avoid overflow
!   4/98 Error in derivatives for distance dependent potentials when
!        theta = 180 corrected
!   5/98 Potential dependent parts placed in separated subroutine
!   6/98 Murrell-Mottram potential added
!   6/98 Indices jx,jy,jz,kx,ky,kz corrected for symmetric potentials 
!  10/98 BAcross potential added
!  10/98 Conversion to theta to rad now performed in here
!   8/99 Linear-three potential added
!   2/01 Generalised for any dimensionality
!   4/01 Calculation of region 2 energies for surfaces added
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   5/01 Modifications added for rhos in sw3
!   5/01 Minimum cut-offs added
!   6/01 Setting of lsamemol altered
!   6/01 Passing of cut-offs to threebody fixed
!   9/01 Marvin compatibility option for SE added
!   9/01 lmolq calculations accelerated using lneedmol 
!  11/01 Attachment energy added
!   1/02 Algorithm for calculation of symmetric potentials corrected - now
!        evaluates all terms and divides by 3 for simplicity
!   8/02 Surface energy calculation algorithm changed
!  10/02 Bcoscross potential added
!  10/02 Second derivatives fixed for freezing case
!  11/02 Wildcard atom type added
!   6/04 Sign of virial corrected
!   9/04 New arguments added to threebody
!  10/04 Charge derivatives added
!   6/05 Deallocation order reversed
!  10/05 Hydrogen-bond potential added
!  12/05 ESFF equatorial potential added
!   8/06 e3d resized to 10 even though not used for benefit of
!        NAG compiler
!   9/06 Theta tapering added
!   1/07 UFF3 potential added
!   2/07 Bonding types added
!   2/07 Calculation of derv3 now checks lopi/lopj/lopk to avoid bounds error
!   5/07 QMMM schemes added
!   5/07 Dreiding option added
!   6/07 Dreiding option bonded2donorJK check added
!   7/07 Checking of bond orders added 
!  10/07 Error in checking of exocyclic attribute for bonds corrected
!  12/07 Unused variables removed
!   5/08 Handling of asymmetric bond orders corrected
!   6/08 Checking of bond numbers added
!  11/08 BAcoscross form added
!  11/08 Option to output energy terms added
!   3/08 3coulomb potential added
!   6/09 Module name changed from three to m_three
!   7/09 Modifications for exp2 potential added
!  11/09 Region derivatives added
!   4/10 Modified in the style of threemd to accelerate bonded potential case
!   5/10 g3coulomb potential added
!   7/10 Setting of lbtyp moved before first reference to variable
!  11/11 Region-region energy contributions stored
!   4/12 Explicit virial calculation removed as no longer needed
!   5/12 Atomic stresses added
!   2/13 BAGcross potential added
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
!  Julian Gale, NRI, Curtin University, February 2013
!
  use configurations, only : lsliceatom, nregions, nregionno, nregiontype, QMMMmode
  use constants
  use control,        only : lmarvreg2, lseok, lDoQDeriv1, lDoQDeriv2, latomicstress
  use current
  use derivatives
  use energies,       only : eregion2region
  use iochannels,     only : ioout
  use m_three
  use mdlogic
  use molecule
  use numbers,        only : third
  use optimisation
  use parallel,       only : ioproc
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  real(dp),   intent(inout)                 :: ethb
  real(dp),   intent(inout)                 :: esregion12
  real(dp),   intent(inout)                 :: esregion2
  real(dp),   intent(inout)                 :: eattach
  logical,    intent(in)                    :: lgrad1
  logical,    intent(in)                    :: lgrad2
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: ii
  integer(i4)                               :: imax
  integer(i4)                               :: in3
  integer(i4)                               :: ind
  integer(i4)                               :: indm
  integer(i4)                               :: indmj
  integer(i4)                               :: indmk
  integer(i4), dimension(:), allocatable    :: ioptptr
  integer(i4)                               :: ix
  integer(i4)                               :: iy
  integer(i4)                               :: iz
  integer(i4)                               :: ixi
  integer(i4)                               :: iyi
  integer(i4)                               :: izi
  integer(i4)                               :: ixj
  integer(i4)                               :: iyj
  integer(i4)                               :: izj
  integer(i4)                               :: ixk
  integer(i4)                               :: iyk
  integer(i4)                               :: izk
  integer(i4)                               :: ixx
  integer(i4)                               :: iyy
  integer(i4)                               :: izz
  integer(i4)                               :: j
  integer(i4)                               :: jj
  integer(i4)                               :: jjmin
  integer(i4)                               :: jloop
  integer(i4)                               :: jmax
  integer(i4)                               :: jx
  integer(i4)                               :: jy
  integer(i4)                               :: jz
  integer(i4)                               :: jxx
  integer(i4)                               :: jyy
  integer(i4)                               :: jzz
  integer(i4)                               :: k
  integer(i4)                               :: kk
  integer(i4)                               :: kl
  integer(i4)                               :: kloop
  integer(i4)                               :: kloopmin
  integer(i4)                               :: kx
  integer(i4)                               :: ky
  integer(i4)                               :: kz
  integer(i4)                               :: kmax
  integer(i4)                               :: lj
  integer(i4)                               :: lk
  integer(i4)                               :: lu
  integer(i4),                         save :: maxvector = 100
  integer(i4)                               :: m
  integer(i4)                               :: n
  integer(i4)                               :: n112
  integer(i4)                               :: n113
  integer(i4)                               :: n122
  integer(i4)                               :: n211
  integer(i4)                               :: n213
  integer(i4)                               :: n221
  integer(i4)                               :: n223
  integer(i4)                               :: n311
  integer(i4)                               :: n312
  integer(i4)                               :: n321
  integer(i4)                               :: n322
  integer(i4)                               :: n1x
  integer(i4)                               :: n1y
  integer(i4)                               :: n1z
  integer(i4)                               :: n1x2
  integer(i4)                               :: n1x3
  integer(i4)                               :: n2x
  integer(i4)                               :: n2y
  integer(i4)                               :: n2z
  integer(i4)                               :: n2x1
  integer(i4)                               :: n2x3
  integer(i4)                               :: n3x
  integer(i4)                               :: n3y
  integer(i4)                               :: n3z
  integer(i4)                               :: n3x1
  integer(i4)                               :: n3x2
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
  integer(i4)                               :: neq
  integer(i4)                               :: ni
  integer(i4)                               :: nj
  integer(i4)                               :: nk
  integer(i4)                               :: nmi
  integer(i4)                               :: nmid
  integer(i4)                               :: nmj
  integer(i4)                               :: nmk
  integer(i4)                               :: nr
  integer(i4)                               :: nregioni
  integer(i4)                               :: nregionj
  integer(i4)                               :: nregionk
  integer(i4)                               :: nregiontypi
  integer(i4)                               :: nregiontypj
  integer(i4)                               :: nregiontypk
  integer(i4)                               :: nsame
  integer(i4)                               :: nt1
  integer(i4)                               :: nt2
  integer(i4)                               :: nt3
  integer(i4)                               :: ntmp
  integer(i4)                               :: nto
  integer(i4)                               :: ntyp1
  integer(i4)                               :: ntyp2
  integer(i4)                               :: ntyp3
  integer(i4)                               :: ntypi
  integer(i4)                               :: ntypj
  integer(i4)                               :: ntypk
  integer(i4)                               :: ntypo
  integer(i4)                               :: nuniquej
  integer(i4), dimension(:), allocatable    :: nuniquejptr
  integer(i4)                               :: nvector
  integer(i4)                               :: status
  logical                                   :: bonded2donor
  logical                                   :: bonded2donorJK
  logical                                   :: l2bonds
  logical                                   :: lattach
  logical                                   :: lbonded
  logical                                   :: lbondnoOK
  logical                                   :: lbondtypeOK
  logical                                   :: lbtyp
  logical                                   :: ldiff23typ
  logical                                   :: ldiff23cut
  logical                                   :: ldiff23bo
  logical                                   :: linter_only
  logical                                   :: lintra_only
  logical,  dimension(:), allocatable, save :: lijdone
  logical                                   :: ljbond
  logical                                   :: lkbond
  logical                                   :: lmatch
  logical                                   :: lmolok
  logical                                   :: lmolok2
  logical                                   :: lneedmol
  logical                                   :: lopi
  logical                                   :: lopj
  logical                                   :: lopk
  logical                                   :: lreg12
  logical                                   :: lreg2trio
  logical                                   :: lsamemol
  logical                                   :: lsg1
  logical                                   :: lslicei
  logical                                   :: lslicej
  logical                                   :: lslicek
  logical                                   :: lsymijk
  logical                                   :: lswapk
  logical                                   :: lswaprho
  logical                                   :: lunique
  real(dp)                                  :: ang
  real(dp)                                  :: cputime
  real(dp)                                  :: cut
  real(dp)                                  :: d0i
  real(dp)                                  :: d0j
  real(dp)                                  :: d0k
  real(dp)                                  :: d1q(3,3)
  real(dp)                                  :: d1qxyz(3,3,3)
  real(dp)                                  :: d1qs(6,3)
  real(dp)                                  :: d2q(6)
  real(dp)                                  :: dot
  real(dp)                                  :: e2d(6)
  real(dp)                                  :: e3d(10)
  real(dp)                                  :: ed11
  real(dp)                                  :: ed12
  real(dp)                                  :: ed13
  real(dp)                                  :: esregion12l
  real(dp)                                  :: ethb1
  real(dp)                                  :: ofct
  real(dp)                                  :: oci
  real(dp)                                  :: ocj
  real(dp)                                  :: ock
  real(dp)                                  :: one
  real(dp)                                  :: qli
  real(dp)                                  :: qlj
  real(dp)                                  :: qlk
  real(dp)                                  :: rprod(6,3)
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
  real(dp)                                  :: rstrdloc(6)
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
  real(dp)                                  :: tm1(3,3,3)
  real(dp)                                  :: tm2(6,3)
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
  real(dp)                                  :: xt21
  real(dp)                                  :: yt21
  real(dp)                                  :: zt21
  real(dp)                                  :: xt31
  real(dp)                                  :: yt31
  real(dp)                                  :: zt31
  real(dp), dimension(:), allocatable, save :: xvec
  real(dp), dimension(:), allocatable, save :: yvec
  real(dp), dimension(:), allocatable, save :: zvec
!
  time1 = cputime()
  lsg1 = (lstr.and.lgrad1)
  ethb = 0.0_dp
  esregion12l = 0.0_dp
  one = 1.0_dp
!
!  Openning banner for energy decomposition
!
  if (lPrintThree) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Three: Atom No. 1  Atom No. 2  Atom No. 3              Threebody energy (eV)  '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!
!  Allocate local memory
!
  allocate(xderv(numat),stat=status)
  if (status/=0) call outofmemory('threenos','xderv')
  allocate(yderv(numat),stat=status)
  if (status/=0) call outofmemory('threenos','yderv')
  allocate(zderv(numat),stat=status)
  if (status/=0) call outofmemory('threenos','zderv')
  allocate(lijdone(numat),stat=status)
  if (status/=0) call outofmemory('threenos','lijdone')
  allocate(nuniquejptr(numat),stat=status)
  if (status/=0) call outofmemory('threenos','nuniquejptr')
  allocate(ioptptr(numat),stat=status)
  if (status/=0) call outofmemory('threenos','ioptptr')
  if (ndim.gt.0) then
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('threenos','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('threenos','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('threenos','zvec')
  else
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('threenos','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('threenos','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('threenos','zvec')
  endif
  lijdone(1:numat) = .false.
!
!  Initialisation
!
  lu = 0
  do i = 1,numat
    if (lopf(nrelat(i)).or..not.lfreeze) then
      lu = lu + 1
      ioptptr(i) = lu
    else
      ioptptr(i) = 0
    endif
  enddo
  if (lgrad1) then
    do i = 1,numat
      xderv(i) = 0.0_dp
      yderv(i) = 0.0_dp
      zderv(i) = 0.0_dp
    enddo
  endif
!
!  Loop over potentials
!
  potentials: do n = 1,nthb
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
    tr1m = tr11m*tr11m
    tr2m = tr21m*tr21m
    tr3m = tr31m*tr31m
    tr1 = tr11*tr11
    tr2 = tr21*tr21
    tr3 = tr31*tr31
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
    lbtyp = (mmtexc(n).eq.1)
    lintra_only = (ltintra(n).and..not.ltinter(n))
    linter_only = (ltinter(n).and..not.ltintra(n))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
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
!
!  Create lattice vectors
!
    if (ndim.gt.0) then
      cut = tr1
      if (tr2.gt.cut) cut = tr2
      if (lsymijk.and.tr3.gt.cut) cut = tr3
      cut = sqrt(cut)
      call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      if (nvector.gt.maxvector) then
!
!  Too many vectors
!
        deallocate(zvec,stat=status)
        if (status/=0) call deallocate_error('threenos','zvec')
        deallocate(yvec,stat=status)
        if (status/=0) call deallocate_error('threenos','yvec')
        deallocate(xvec,stat=status)
        if (status/=0) call deallocate_error('threenos','xvec')
        maxvector = nint(1.1*nvector)
        allocate(xvec(maxvector),stat=status)
        if (status/=0) call outofmemory('threenos','xvec')
        allocate(yvec(maxvector),stat=status)
        if (status/=0) call outofmemory('threenos','yvec')
        allocate(zvec(maxvector),stat=status)
        if (status/=0) call outofmemory('threenos','zvec')
        call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      endif
    else
      nvector = 1
      nmid = 1
      xvec(1) = 0.0_dp
      yvec(1) = 0.0_dp
      zvec(1) = 0.0_dp
    endif
!
!  Outer loop over sites
!
    ix = - 2
    iy = - 1
    iz =   0
    iloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
      lopi = (lopf(nrelat(i)).or..not.lfreeze)
      if (lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
      endif
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
!
!  Set properties of i
!
      nregioni = nregionno(nsft+nrelat(i))
      nregiontypi = nregiontype(nregioni,ncf)
      oci = occuf(i)
      qli = qf(i)
      lslicei = lsliceatom(nsft + nrelat(i))
!
!  QM/MM handling
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.lbtyp) cycle iloop
      endif
!
!  Dreiding option handling
!
      if (ltdreiding(n)) then
        if (.not.bonded2donor(i)) cycle iloop
      endif
!
!  Molecule handling
!
      if (lmol.and.lneedmol) then
        nmi = natmol(i)
        if (ndim.gt.0) then
          indm = nmolind(i)
          izi = (indm/100) - 5
          ind = indm - 100*(izi+5)
          iyi = (ind/10) - 5
          ind = ind - 10*(iyi+5)
          ixi = ind - 5
        endif
      endif
!     
!  Check number of bonds if necessary
!  
      if (n3bondnono(1,n).gt.0) then
        lbondnoOK = .false.
        do in3 = 1,n3bondnono(1,n)
          if (nbonds(i).eq.n3bondno(in3,1,n)) lbondnoOK = .true.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
      if (n3bondnono(2,n).gt.0) then
        lbondnoOK = .true.
        do in3 = 1,n3bondnono(2,n)
          if (nbonds(i).eq.n3bondno(in3,2,n)) lbondnoOK = .false.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
!
!  Set loop range for j
!
      if (lmol.and.lneedmol.and.lbtyp) then
        ljbond = .true.
        if (nbonds(i).gt.0) then
          nuniquej = 1
          nuniquejptr(1) = nbonded(1,i)
          do jloop = 2,nbonds(i)
            lunique = .true.
            do lu = 1,nuniquej
              if (nbonded(jloop,i).eq.nuniquejptr(lu)) lunique = .false.
            enddo
            if (lunique) then
              nuniquej = nuniquej + 1
              nuniquejptr(nuniquej) = nbonded(jloop,i)
            endif
          enddo
          jloop = nuniquej
        else
          jloop = 0
        endif
      else
        ljbond = .false.
        jloop = numat
      endif
!
!  Skip if jloop is zero
!
      if (jloop.eq.0) cycle iloop
!
      xc1 = xclat(i)
      yc1 = yclat(i)
      zc1 = zclat(i)
!
!  Inner loop over second site
!
      ljloop: do lj = 1,jloop
        if (ljbond) then
          j = nuniquejptr(lj)
        else
          j = lj
        endif
        nj = nat(j)
        ntypj = nftype(j)
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
          cycle ljloop
        endif
!
!  Set properties for atom j
!
        nregionj = nregionno(nsft+nrelat(j))
        nregiontypj = nregiontype(nregionj,ncf)
        ocj = occuf(j)
        qlj = qf(j)
        lopj = (lopf(nrelat(j)).or..not.lfreeze)
        lslicej = lsliceatom(nsft + nrelat(j))
        if (lopj) then
          jx = 3*(ioptptr(j)-1) + 1
          jy = jx + 1
          jz = jx + 2
        endif
        if (lmol.and.lneedmol) then
!
!  Molecule handling
!
          nmj = natmol(j)
          if (ndim.gt.0) then
            indmj = nmolind(j)
            izj = (indmj/100) - 5
            ind = indmj - 100*(izj+5)
            iyj = (ind/10) - 5
            ind = ind - 10*(iyj+5)
            ixj = ind - 5
            ixj = ixj - ixi
            iyj = iyj - iyi
            izj = izj - izi
          endif
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
        else
          lmolok = .false.
        endif
!
!  Check for intra and but not in same molecule
!
        if (lintra_only.and..not.lmolok) cycle ljloop
        if (lbtyp.and..not.lmolok) cycle ljloop
!
        x21 = xclat(j) - xc1
        y21 = yclat(j) - yc1
        z21 = zclat(j) - zc1
!
!  Check r12 is OK
!  Loop over cell vectors
!
        iiloop: do ii = 1,nvector
          r122 = (xvec(ii)+x21)**2 + (yvec(ii)+y21)**2 + (zvec(ii)+z21)**2
          if (r122.lt.1.0d-10) cycle iiloop
!
!  Molecule checking
!
          lbonded = .false.
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle iiloop
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
                if (.not.lbonded) cycle iiloop
              endif
            else
              call lintoijk(ixx,iyy,izz,ii,imax,jmax,kmax)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ixx,iyy,izz)
                if (.not.lbonded) cycle iiloop
                lsamemol = (lbonded.or.l2bonds)
              else
                lsamemol = .false.
              endif
              if (.not.lsamemol) then
                call samemol(lsamemol,nmi,ixx,iyy,izz,ixj,iyj,izj)
              endif
              if (lintra_only.and..not.lsamemol) cycle iiloop
              if (linter_only.and.lsamemol) cycle iiloop
            endif
          endif
!
!  Distance checking - if this fails but types are symmetric, try switching
!
          if ((r122.gt.ttr1.or.r122.lt.ttr1m).and.(.not.lbtyp.or..not.lbonded)) then
            if (ldiff23typ.or..not.ldiff23cut) cycle iiloop
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
              cycle iiloop
            endif
          endif
          r12 = sqrt(r122)
!
!  Set loop range for k
!
          if (lmol.and.lneedmol.and.lbtyp) then
            lkbond = .true.
            kloopmin = 1
            kloop = nbonds(i) + nbonds(j)
            do lk = 1,nbonds(i)
              lijdone(nbonded(lk,i)) = .false.
            enddo
            do lk = 1,nbonds(j)
              lijdone(nbonded(lk,j)) = .false.
            enddo
          else
            lkbond = .false.
            kloopmin = j
            kloop = numat
          endif
!
!  Skip if kloop is zero
!
          if (kloop.eq.0) cycle iiloop
!
!  Inner loop over third site
!
          lkloop: do lk = kloopmin,kloop
            if (lkbond) then
              if (lk.le.nbonds(i)) then
                k = nbonded(lk,i)
              else
                k = nbonded(lk-nbonds(i),j)
              endif
!
!  Check to make sure that atom is not done twice
!
              if (lijdone(k)) cycle lkloop
              lijdone(k) = .true.
              if (k.lt.j) cycle lkloop
            else
              k = lk
            endif
            nk = nat(k)
            ntypk = nftype(k)
!
!  Check k is allowed for n, and not equivalent to j
!
            if (.not.lmatch(nk,ntypk,nto,ntypo,.true.)) cycle lkloop
!
!  Set properties for atom k
!
            nregionk = nregionno(nsft+nrelat(k))
            nregiontypk = nregiontype(nregionk,ncf)
            ock = occuf(k)
            qlk = qf(k)
            lopk = (lopf(nrelat(k)).or..not.lfreeze)
            if (lopk) then
              kx = 3*(ioptptr(k)-1) + 1
              ky = kx + 1
              kz = kx + 2
            endif
!
!  If lfreeze and all atoms are fixed then skip this
!  three body term
!
            if (.not.lopi.and..not.lopj.and..not.lopk) cycle lkloop
!
!  Dreiding option handling
!
            if (ltdreiding(n)) then
              if (.not.bonded2donorJK(i,j,k)) cycle lkloop
            endif
!
!  Set region 2 trio flag
!
            lreg12    = .false.
            lreg2trio = .false.
            if (lseok.and.nregions(ncf).gt.1) then
              lreg2trio = (nregioni.gt.1.and.nregionj.gt.1.and.nregionk.gt.1)
              if (.not.lreg2trio) lreg12 = (nregioni.gt.1.or.nregionj.gt.1.or.nregionk.gt.1)
            endif
            lslicek = lsliceatom(nsft + nrelat(k))
            lattach = .true.
            if (lslicei.and.lslicej.and.lslicek) lattach = .false.  
            if (.not.lslicei.and..not.lslicej.and..not.lslicek) lattach = .false.
!       
!  QM/MM handling
!       
            if (QMMMmode(ncf).gt.0) then
              if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1) cycle lkloop
            endif
!
            if (lmol.and.lneedmol) then
!
!  Molecule handling
!
              nmk = natmol(k)
              if (ndim.gt.0) then
                indmk = nmolind(k)
                izk = (indmk/100) - 5
                ind = indmk-100*(izk+5)
                iyk = (ind/10) - 5
                ind = ind - 10*(iyk+5)
                ixk = ind - 5
                ixk = ixk - ixi
                iyk = iyk - iyi
                izk = izk - izi
              endif
              lmolok2 = (nmi.eq.nmk.and.nmi.gt.0)
            else
              lmolok2 = .false.
            endif
!
!  Check for intra and but not in same molecule
!
            if (lintra_only.and..not.lmolok2) cycle lkloop
            if (lbtyp.and..not.lmolok2) cycle lkloop
!
            x31 = xclat(k) - xc1
            y31 = yclat(k) - yc1
            z31 = zclat(k) - zc1
            if (j.eq.k) then
              jjmin = ii + 1
            else
              jjmin = 1
            endif
!
!  Check r13 is OK
!  Loop over cell vectors
!
            jjloop: do jj = jjmin,nvector
              r132 = (xvec(jj)+x31)**2 + (yvec(jj)+y31)**2 + (zvec(jj)+z31)**2
              if (r132.lt.1.0d-10) cycle jjloop
!
!  Molecule checking
!
              lbonded = .false.
              if (lmolok2) then
                if (ndim.eq.0) then
                  if (linter_only) cycle jjloop
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,0_i4,0_i4,0_i4)
                    if (.not.lbonded) cycle jjloop
                    if (lsymijk) then
                      call bonded(lbonded,l2bonds,nbtypejk,nbtypejk2,j,k,0_i4,0_i4,0_i4)
                      if (.not.lbonded) cycle jjloop
                    endif
                  endif
                else
                  call lintoijk(jxx,jyy,jzz,jj,imax,jmax,kmax)
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,jxx,jyy,jzz)
                    if (.not.lbonded) cycle jjloop
                    if (lsymijk) then
                      call bonded(lbonded,l2bonds,nbtypejk,nbtypejk2,j,k,jxx-ixx,jyy-iyy,jzz-izz)
                      if (.not.lbonded) cycle jjloop
                    endif
                    lsamemol = (lbonded.or.l2bonds)
                  else
                    lsamemol = .false.
                  endif
                  if (.not.lsamemol) then
                    call samemol(lsamemol,nmi,jxx,jyy,jzz,ixk,iyk,izk)
                  endif
                  if (lintra_only.and..not.lsamemol) cycle jjloop
                  if (linter_only.and.lsamemol) cycle jjloop
                endif
              endif
!
!  Distance checking
!
!  Modification to handle case where species for 2 and 3 are the same
!  but cutoffs are different
!
              if ((r132.gt.ttr2.or.r132.lt.ttr2m).and.(.not.lbtyp.or..not.lbonded)) then
                if (ldiff23typ.or..not.ldiff23cut) cycle jjloop
                if (r122.gt.ttr2.or.r132.gt.ttr1) cycle jjloop
                if (r122.lt.ttr2m.or.r132.lt.ttr1m) cycle jjloop
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
                  if (.not.ldiff23typ.or..not.ldiff23bo) cycle jjloop
!
!  Check i-j bond for correct order
!  
                  if (nbotyp21.gt.0.and.nbotyp21.ne.nbtypeij) cycle jjloop
                  if (nbotyp22.gt.1.and.nbotyp22.ne.nbtypeij2) cycle jjloop
!               
!  Check i-k bond for correct order
!  
                  if (nbotyp11.gt.0.and.nbotyp11.ne.nbtypeik) cycle jjloop
                  if (nbotyp12.gt.1.and.nbotyp12.ne.nbtypeik2) cycle jjloop
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
              xt31 = x31 + xvec(jj)
              yt31 = y31 + yvec(jj)
              zt31 = z31 + zvec(jj)
              xt21 = x21 + xvec(ii)
              yt21 = y21 + yvec(ii)
              zt21 = z21 + zvec(ii)
              x23 = xt31 - xt21
              y23 = yt31 - yt21
              z23 = zt31 - zt21
              r232 = x23**2 + y23**2 + z23**2
              if (r232.gt.tr3.and..not.lbtyp) cycle jjloop
              if (r232.lt.tr3m.or.r232.lt.1.0d-10) cycle jjloop
!
!  Valid three-body term  = > calculate potential
!
              r23v(1) = - x23
              r23v(2) = - y23
              r23v(3) = - z23
              r12v(1) = - xt21
              r12v(2) = - yt21
              r12v(3) = - zt21
              r13v(1) = - xt31
              r13v(2) = - yt31
              r13v(3) = - zt31
              if (n3ty.ne.3.and.n3ty.ne.4.and.n3ty.ne.6.and.n3ty.ne.7.and.n3ty.ne.19) then
                dot = xt21*xt31 + yt21*yt31 + zt21*zt31
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
                rk32 = rkthb*ofct*qf(j)*qf(k)
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
              call threebody(1_i4,n3ty,r12,r13,r23,ed11,ed12,ed13,ethb1,e2d,e3d,ttr11,ttr21,tr31, &
                             rho1,rho2,rho3,rho4,rho5,rk32,rk33,rk34,the0,ang,dot,lgrad1,lgrad2,.false.,n, &
                             qli,qlj,qlk,d0i,d0j,d0k,d1q,d2q,lthetataper(n),thetatapermin(n), &
                             thetatapermax(n))
              if (lreg2trio) then
                esregion2 = esregion2 + ethb1
              elseif (lreg12) then
                esregion12l = esregion12l + ethb1
              else
                ethb = ethb + ethb1
              endif
              if (lattach) eattach = eattach + ethb1
!
              eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + third*ethb1
              eregion2region(nregionk,nregioni) = eregion2region(nregionk,nregioni) + third*ethb1
              eregion2region(nregionk,nregionj) = eregion2region(nregionk,nregionj) + third*ethb1
!
!  Output energy contribution
!
              if (lPrintThree) then
                write(ioout,'(4x,3i12,8x,f27.10)') i,j,k,ethb1
              endif
!*************************
!  Start of derivatives  *
!*************************
              if (lgrad1) then
!
!  Set up strain products
!
                if (lsg1) then
                  call threestrterms(ndim,rprod,xt21,yt21,zt21,xt31,yt31,zt31,x23,y23,z23)
                endif
!
!  Charge derivatives
!
                if (lDoQDeriv1) then
                  call d1charge3(i,j,k,lopi,lopj,lopk,1_i4,d0i,d0j,d0k)
                endif
              endif
              if (lgrad2) then
                n1x = ix
                n1y = iy
                n1z = iz
                n2x = jx
                n2y = jy
                n2z = jz
                n3x = kx
                n3y = ky
                n3z = kz
                if (lopi.and.lopj.and.lopk) then
                  n1x2 = ix
                  n1x3 = ix
                  n2x1 = jx
                  n2x3 = jx
                  n3x1 = kx
                  n3x2 = kx
                elseif (lopi.and.lopj) then
                  n1x2 = ix
                  n1x3 = ix
                  n2x1 = jx
                  n2x3 = jx
                  n3x1 = ix
                  n3x2 = jx
                elseif (lopi.and.lopk) then
                  n1x2 = ix
                  n1x3 = ix
                  n2x1 = ix
                  n2x3 = kx
                  n3x1 = kx
                  n3x2 = kx
                elseif (lopj.and.lopk) then
                  n1x2 = jx
                  n1x3 = kx
                  n2x1 = jx
                  n2x3 = jx
                  n3x1 = kx
                  n3x2 = kx
                elseif (lopi) then
                  n1x2 = ix
                  n1x3 = ix
                  n2x1 = ix
                  n3x1 = ix
                elseif (lopj) then
                  n1x2 = jx
                  n2x1 = jx
                  n2x3 = jx
                  n3x2 = jx
                elseif (lopk) then
                  n1x3 = kx
                  n2x3 = kx
                  n3x1 = kx
                  n3x2 = kx
                endif
              endif
!***********************
!  Strain derivatives  *
!***********************
              if (lsg1) then
!
!  First strain derivatives
!
                rstrdloc(1:nstrains) = 0.0_dp
                do kl = 1,nstrains
                  rstrdloc(kl) = rstrdloc(kl) + ed11*rprod(kl,1)
                  rstrdloc(kl) = rstrdloc(kl) + ed12*rprod(kl,2)
                  rstrdloc(kl) = rstrdloc(kl) + ed13*rprod(kl,3)
                  rstrd(kl) = rstrd(kl) + rstrdloc(kl)
                enddo
                if (latomicstress) then
                  do kl = 1,nstrains
                    atomicstress(kl,i) = atomicstress(kl,i) + third*rstrdloc(kl)
                    atomicstress(kl,j) = atomicstress(kl,j) + third*rstrdloc(kl)
                    atomicstress(kl,k) = atomicstress(kl,k) + third*rstrdloc(kl)
                  enddo
                endif
!
!  Second strain derivatives
!
!  Strain only
!
                if (lgrad2.and.lstr) then
                  do kk = 1,nstrains
                    tm2(kk,1) = e2d(1)*rprod(kk,1) + e2d(2)*rprod(kk,2) + e2d(3)*rprod(kk,3)
                    tm2(kk,2) = e2d(2)*rprod(kk,1) + e2d(4)*rprod(kk,2) + e2d(5)*rprod(kk,3)
                    tm2(kk,3) = e2d(3)*rprod(kk,1) + e2d(5)*rprod(kk,2) + e2d(6)*rprod(kk,3)
                  enddo
                  do kk = 1,nstrains
                    do kl = 1,nstrains
                      sderv2(kl,kk) = sderv2(kl,kk) + rprod(kk,1)*tm2(kl,1) + rprod(kk,2)*tm2(kl,2) + rprod(kk,3)*tm2(kl,3)
                    enddo
                  enddo
!
!  Mixed derivatives
!
                  if (lopi) then
                    do kk = 1,nstrains
                      derv3(n1x,kk) = derv3(n1x,kk) - xt21*tm2(kk,1) - xt31*tm2(kk,2)
                      derv3(n1y,kk) = derv3(n1y,kk) - yt21*tm2(kk,1) - yt31*tm2(kk,2)
                      derv3(n1z,kk) = derv3(n1z,kk) - zt21*tm2(kk,1) - zt31*tm2(kk,2)
                    enddo
                  endif
                  if (lopj) then
                    do kk = 1,nstrains
                      derv3(n2x,kk) = derv3(n2x,kk) + xt21*tm2(kk,1) - x23*tm2(kk,3)
                      derv3(n2y,kk) = derv3(n2y,kk) + yt21*tm2(kk,1) - y23*tm2(kk,3)
                      derv3(n2z,kk) = derv3(n2z,kk) + zt21*tm2(kk,1) - z23*tm2(kk,3)
                    enddo
                  endif
                  if (lopk) then
                    do kk = 1,nstrains
                      derv3(n3x,kk) = derv3(n3x,kk) + xt31*tm2(kk,2) + x23*tm2(kk,3)
                      derv3(n3y,kk) = derv3(n3y,kk) + yt31*tm2(kk,2) + y23*tm2(kk,3)
                      derv3(n3z,kk) = derv3(n3z,kk) + zt31*tm2(kk,2) + z23*tm2(kk,3)
                    enddo
                  endif
                endif
              endif
!*************************
!  Internal derivatives  *
!*************************
              if (lgrad1) then
                xderv(i) = xderv(i) - xt21*ed11 - xt31*ed12
                yderv(i) = yderv(i) - yt21*ed11 - yt31*ed12
                zderv(i) = zderv(i) - zt21*ed11 - zt31*ed12
                xderv(j) = xderv(j) + xt21*ed11 - x23*ed13
                yderv(j) = yderv(j) + yt21*ed11 - y23*ed13
                zderv(j) = zderv(j) + zt21*ed11 - z23*ed13
                xderv(k) = xderv(k) + xt31*ed12 + x23*ed13
                yderv(k) = yderv(k) + yt31*ed12 + y23*ed13
                zderv(k) = zderv(k) + zt31*ed12 + z23*ed13
              endif
              if (lgrad2) then
!
!  Charge derivatives
!
                if (lDoQDeriv2) then
                  do m = 1,3
                    d1qxyz(1,1,m) = d1q(1,m)*xt21
                    d1qxyz(2,1,m) = d1q(1,m)*yt21
                    d1qxyz(3,1,m) = d1q(1,m)*zt21
                    d1qxyz(1,2,m) = d1q(2,m)*xt31
                    d1qxyz(2,2,m) = d1q(2,m)*yt31
                    d1qxyz(3,2,m) = d1q(2,m)*zt31
                    d1qxyz(1,3,m) = d1q(3,m)*x23
                    d1qxyz(2,3,m) = d1q(3,m)*y23
                    d1qxyz(3,3,m) = d1q(3,m)*z23
                  enddo
                  if (lstr) then
                    do m = 1,3
                      do kl = 1,nstrains
                        d1qs(kl,m) = d1q(1,m)*rprod(kl,1) + d1q(2,m)*rprod(kl,2) + d1q(3,m)*rprod(kl,3)
                      enddo
                    enddo
                  endif
                  call d2charge3(i,j,k,1_i4,ix,iy,iz,jx,jy,jz,kx,ky,kz,lopi,lopj,lopk,d0i,d0j,d0k,d1qxyz,d1qs,d2q)
                endif
!
                tm1(1,1,1) = e2d(2)*xt21*xt31
                tm1(1,2,1) = e2d(2)*yt21*xt31
                tm1(1,3,1) = e2d(2)*zt21*xt31
                tm1(1,1,2) = e2d(2)*xt21*yt31
                tm1(1,2,2) = e2d(2)*yt21*yt31
                tm1(1,3,2) = e2d(2)*zt21*yt31
                tm1(1,1,3) = e2d(2)*xt21*zt31
                tm1(1,2,3) = e2d(2)*yt21*zt31
                tm1(1,3,3) = e2d(2)*zt21*zt31
                tm1(2,1,1) = e2d(3)*xt21*x23
                tm1(2,2,1) = e2d(3)*yt21*x23
                tm1(2,3,1) = e2d(3)*zt21*x23
                tm1(2,1,2) = e2d(3)*xt21*y23
                tm1(2,2,2) = e2d(3)*yt21*y23
                tm1(2,3,2) = e2d(3)*zt21*y23
                tm1(2,1,3) = e2d(3)*xt21*z23
                tm1(2,2,3) = e2d(3)*yt21*z23
                tm1(2,3,3) = e2d(3)*zt21*z23
                tm1(3,1,1) = e2d(5)*xt31*x23
                tm1(3,2,1) = e2d(5)*yt31*x23
                tm1(3,3,1) = e2d(5)*zt31*x23
                tm1(3,1,2) = e2d(5)*xt31*y23
                tm1(3,2,2) = e2d(5)*yt31*y23
                tm1(3,3,2) = e2d(5)*zt31*y23
                tm1(3,1,3) = e2d(5)*xt31*z23
                tm1(3,2,3) = e2d(5)*yt31*z23
                tm1(3,3,3) = e2d(5)*zt31*z23
                do kk = 1,3
                  n112 = n1x2 - 1 + kk
                  n113 = n1x3 - 1 + kk
                  n211 = n2x1 - 1 + kk
                  n213 = n2x3 - 1 + kk
                  n311 = n3x1 - 1 + kk
                  n312 = n3x2 - 1 + kk
!
!  First term
!
                  if (lopi.and.lopj) then
                    derv2(n112,n211) = derv2(n112,n211) - ed11
                    derv2(n211,n112) = derv2(n211,n112) - ed11
                  elseif (lopi.or.lopj) then
                    derv2(n112,n211) = derv2(n112,n211) - ed11
                  endif
                  if (lopi.and.lopk) then
                    derv2(n113,n311) = derv2(n113,n311) - ed12
                    derv2(n311,n113) = derv2(n311,n113) - ed12
                  elseif (lopi.or.lopk) then
                    derv2(n113,n311) = derv2(n113,n311) - ed12
                  endif
                  if (lopj.and.lopk) then
                    derv2(n213,n312) = derv2(n213,n312) - ed13
                    derv2(n312,n213) = derv2(n312,n213) - ed13
                  elseif (lopj.or.lopk) then
                    derv2(n213,n312) = derv2(n213,n312) - ed13
                  endif
!
!  Second term
!
                  do kl = 1,3
                    n122 = n1x2 - 1 + kl
                    n221 = n2x1 - 1 + kl
                    n223 = n2x3 - 1 + kl
                    n321 = n3x1 - 1 + kl
                    n322 = n3x2 - 1 + kl
                    trm11 = tm1(1,kk,kl)
                    trm12 = tm1(2,kk,kl)
                    trm13 = tm1(3,kk,kl)
                    if (lopi.and.lopj) then
                      trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                      derv2(n112,n221) = derv2(n112,n221) + trm1
                      derv2(n221,n112) = derv2(n221,n112) + trm1
                      derv2(n122,n211) = derv2(n122,n211) - trm11
                      derv2(n211,n122) = derv2(n211,n122) - trm11
                    elseif (lopj) then
                      trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                      derv2(n112,n221) = derv2(n112,n221) + trm1
                      derv2(n122,n211) = derv2(n122,n211) - trm11
                    elseif (lopi) then
                      trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                      derv2(n221,n112) = derv2(n221,n112) + trm1
                      derv2(n211,n122) = derv2(n211,n122) - trm11
                    endif
                    if (lopi.and.lopk) then
                      trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                      derv2(n113,n321) = derv2(n113,n321) + trm1
                      derv2(n321,n113) = derv2(n321,n113) + trm1
                    elseif (lopk) then
                      trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                      derv2(n113,n321) = derv2(n113,n321) + trm1
                    elseif (lopi) then
                      trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                      derv2(n321,n113) = derv2(n321,n113) + trm1
                    endif
                    if (lopj.and.lopk) then
                      trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                      derv2(n213,n322) = derv2(n213,n322) + trm1
                      derv2(n322,n213) = derv2(n322,n213) + trm1
                      derv2(n223,n312) = derv2(n223,n312) - trm13
                      derv2(n312,n223) = derv2(n312,n223) - trm13
                    elseif (lopk) then
                      trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                      derv2(n213,n322) = derv2(n213,n322) + trm1
                      derv2(n223,n312) = derv2(n223,n312) - trm13
                    elseif (lopj) then
                      trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                      derv2(n322,n213) = derv2(n322,n213) + trm1
                      derv2(n312,n223) = derv2(n312,n223) - trm13
                    endif
                  enddo
                enddo
              endif
!
!  End of inner loops
!
            enddo jjloop
          enddo lkloop
        enddo iiloop
      enddo ljloop
!
!  End of outer loops
!
    enddo iloop
  enddo potentials
!
!  Closing banner for energy decomposition
!
  if (lPrintThree) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  Marvin compatibility option -> all three body terms are in region 1
!
  if (lmarvreg2) then
    ethb = ethb + esregion12l
  else
    esregion12 = esregion12 + esregion12l
  endif
!
!  If symmetry adapted derivatives have been calculated elsewhere
!  then add derivatives of related atoms
!
  if (lgrad1) then
    if (lsymderv.and.(.not.lgrad2)) then
      do i = 1,nasym
        nr = nrel2(i)
        neq = neqv(i)
        xdrv(i) = xdrv(i) + neq*xderv(nr)
        ydrv(i) = ydrv(i) + neq*yderv(nr)
        zdrv(i) = zdrv(i) + neq*zderv(nr)
!
        nregioni = nregionno(nsft+i)
        xregdrv(nregioni) = xregdrv(nregioni) + neq*xderv(nr)
        yregdrv(nregioni) = yregdrv(nregioni) + neq*yderv(nr)
        zregdrv(nregioni) = zregdrv(nregioni) + neq*zderv(nr)
      enddo
    else
      do i = 1,numat
        xdrv(i) = xdrv(i) + xderv(i)
        ydrv(i) = ydrv(i) + yderv(i)
        zdrv(i) = zdrv(i) + zderv(i)
        nregioni = nregionno(nsft+nrelat(i))
        xregdrv(nregioni) = xregdrv(nregioni) + xderv(i)
        yregdrv(nregioni) = yregdrv(nregioni) + yderv(i)
        zregdrv(nregioni) = zregdrv(nregioni) + zderv(i)
      enddo
    endif
  endif
!
!  Free local memory
!
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('threenos','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('threenos','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('threenos','xvec')
  deallocate(ioptptr,stat=status)
  if (status/=0) call deallocate_error('threenos','ioptptr')
  deallocate(nuniquejptr,stat=status)
  if (status/=0) call deallocate_error('threenos','nuniquejptr')
  deallocate(lijdone,stat=status)
  if (status/=0) call deallocate_error('threenos','lijdone')
  deallocate(zderv,stat=status)
  if (status/=0) call deallocate_error('threenos','zderv')
  deallocate(yderv,stat=status)
  if (status/=0) call deallocate_error('threenos','yderv')
  deallocate(xderv,stat=status)
  if (status/=0) call deallocate_error('threenos','xderv')
!
!  Timing
!
  time2 = cputime()
  tthree = tthree + time2 - time1
!
  return
  end
