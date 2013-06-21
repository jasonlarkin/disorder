  subroutine threesd2(ethb,lgrad1,lgrad2)
!
!  Subroutine for three-body energy using symmetry to 
!  reduce derivative work.
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
!   6/95 Correction added for asymmetric cutoffs with
!        symmetric potential
!   3/96 Urey-Bradley potential added
!   4/96 Exponentially decaying Vessal form added
!   6/96 General theta0 added to SW3
!  11/96 Compression of second derivatives added / freezing
!   9/97 Bug in sw3 derivatives fixed
!   3/98 Cosine-harmonic form added
!   4/98 Small constant added to rtrm1/rtrm2 in SW3 to avoid overflow
!   4/98 Error in derivatives for distance dependent potentials when
!        theta = 180 corrected
!   5/98 Potential dependent parts placed in separated subroutine
!   6/98 Murrell-Mottram potential added
!  10/98 BAcross potential added
!  10/98 Conversion of theta to rad now done in here
!   3/99 Renamed from threesd.f
!   8/99 Linear-three potential added
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   5/01 Modifications added for rhos in sw3
!   5/01 Minimum cut-offs added
!   6/01 Setting of lsamemol altered
!   6/01 Passing of cut-offs to threebody fixed
!   9/01 lmolq calculations accelerated using lneedmol 
!   1/02 Algorithm for calculation of symmetric potentials corrected - now
!        evaluates all terms and divides by 3 for simplicity
!  10/02 Bcoscross potential added
!  11/02 Wildcard atom type added
!   9/04 New arguments added to threebody
!   6/05 Deallocation order reversed
!  10/05 Hydrogen-bond potential added
!  12/05 ESFF equatorial potential added
!   9/06 Theta tapering added
!   1/07 UFF3 potential added
!   2/07 Bonding types added
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
!   4/10 Modified in the style of threemd to accelerate bonded potential case
!   5/10 g3coulomb potential added
!   5/12 Atomic stresses added
!   5/12 Atomic stresses removed for routines involving symmetry
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
  use constants
  use current
  use derivatives
  use iochannels,     only : ioout
  use m_three
  use mdlogic
  use molecule
  use numbers
  use optimisation
  use parallel,       only : ioproc
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  real(dp),    intent(inout)                :: ethb
  logical,     intent(in)                   :: lgrad1
  logical,     intent(in)                   :: lgrad2
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: ii
  integer(i4)                               :: ii2
  integer(i4)                               :: imax
  integer(i4)                               :: in3
  integer(i4)                               :: ind
  integer(i4)                               :: indm
  integer(i4)                               :: indmj
  integer(i4)                               :: indmk
  integer(i4), dimension(:), allocatable    :: ioptptr
  integer(i4), dimension(:), allocatable    :: ioptaptr
  integer(i4)                               :: ix
  integer(i4)                               :: iy
  integer(i4)                               :: iz
  integer(i4)                               :: ixf
  integer(i4)                               :: iyf
  integer(i4)                               :: izf
  integer(i4)                               :: ixfo
  integer(i4)                               :: iyfo
  integer(i4)                               :: izfo
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
  integer(i4)                               :: jxf
  integer(i4)                               :: jxx
  integer(i4)                               :: jyy
  integer(i4)                               :: jzz
  integer(i4)                               :: k
  integer(i4)                               :: kk
  integer(i4)                               :: kl
  integer(i4)                               :: kloop
  integer(i4)                               :: kloopmin
  integer(i4)                               :: kmax
  integer(i4)                               :: kx
  integer(i4)                               :: ky
  integer(i4)                               :: kz
  integer(i4)                               :: kxf
  integer(i4)                               :: lj
  integer(i4)                               :: lk
  integer(i4)                               :: lu
  integer(i4),                         save :: maxvector = 100
  integer(i4)                               :: n
  integer(i4)                               :: n1x
  integer(i4)                               :: n1y
  integer(i4)                               :: n1z
  integer(i4)                               :: n1xf
  integer(i4)                               :: n11
  integer(i4)                               :: n11f
  integer(i4)                               :: n12
  integer(i4)                               :: n12f
  integer(i4)                               :: n2x
  integer(i4)                               :: n2xf
  integer(i4)                               :: n2y
  integer(i4)                               :: n2z
  integer(i4)                               :: n21
  integer(i4)                               :: n21f
  integer(i4)                               :: n22
  integer(i4)                               :: n22f
  integer(i4)                               :: n3x
  integer(i4)                               :: n3xf
  integer(i4)                               :: n3y
  integer(i4)                               :: n3z
  integer(i4)                               :: n31
  integer(i4)                               :: n31f
  integer(i4)                               :: n32
  integer(i4)                               :: n32f
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
  integer(i4)                               :: neqi
  integer(i4)                               :: neqj
  integer(i4)                               :: neqk
  integer(i4)                               :: ni
  integer(i4)                               :: nj
  integer(i4)                               :: nk
  integer(i4)                               :: nloopi
  integer(i4)                               :: nmi
  integer(i4)                               :: nmid
  integer(i4)                               :: nmj
  integer(i4)                               :: nmk
  integer(i4)                               :: nreli
  integer(i4)                               :: nrelj
  integer(i4)                               :: nrelk
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
  logical                                   :: lasu1
  logical                                   :: lasu2
  logical                                   :: lasu3
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
  logical                                   :: lsamemol
  logical                                   :: lsg1
  logical                                   :: lsymijk
  logical                                   :: lswaprho
  logical                                   :: lswapk
  logical                                   :: lunique
  real(dp)                                  :: ang
  real(dp)                                  :: cputime
  real(dp)                                  :: cut
  real(dp)                                  :: d0i
  real(dp)                                  :: d0j
  real(dp)                                  :: d0k
  real(dp)                                  :: d1q(3,3)
  real(dp)                                  :: d2q(6)
  real(dp)                                  :: dot
  real(dp)                                  :: e2d(6)
  real(dp)                                  :: e3d(1)
  real(dp)                                  :: ed11
  real(dp)                                  :: ed12
  real(dp)                                  :: ed13
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
  real(dp)                                  :: ro1
  real(dp)                                  :: ro2
  real(dp)                                  :: ro3
  real(dp)                                  :: ro4
  real(dp)                                  :: ro5
  real(dp)                                  :: rro
  real(dp)                                  :: rsfct
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
!  Initialisation
!
  ethb = 0.0_dp
  one = 1.0_dp
!
!  Allocate local memory
!
  allocate(lijdone(numat),stat=status)
  if (status/=0) call outofmemory('threesd2','lijdone')
  allocate(nuniquejptr(numat),stat=status)
  if (status/=0) call outofmemory('threesd2','nuniquejptr')
  allocate(ioptptr(numat),stat=status)
  if (status/=0) call outofmemory('threesd2','ioptptr')
  allocate(ioptaptr(nasym),stat=status)
  if (status/=0) call outofmemory('threesd2','ioptaptr')
  if (ndim.gt.1) then
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('threesd2','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('threesd2','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('threesd2','zvec')
  else
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('threesd2','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('threesd2','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('threesd2','zvec')
  endif
!
!  Initialisation
!
  lijdone(1:numat) = .false.
  lu = 0
  do i = 1,nasym
    if (lopf(i).or..not.lfreeze) then
      lu = lu + 1
      ioptaptr(i) = lu
    else
      ioptaptr(i) = 0
    endif
  enddo
  lu = 0
  do i = 1,numat
    if (lopf(nrelat(i)).or..not.lfreeze) then
      lu = lu + 1
      ioptptr(i) = lu
    else
      ioptptr(i) = 0
    endif
  enddo
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
    lbtyp = (mmtexc(n).eq.1)
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
!
!  If only the energy is required the centre atom
!  loop can be performed over only the asymmetric
!  unit atoms. For symmetric potentials then the
!  outer loop can always be over the asymmetric 
!  unit.
!
    if (lgrad1) then
      nloopi = numat
    else
      nloopi = nasym
    endif
!
!  Create lattice vectors
!
    if (ndim.eq.3) then
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
        if (status/=0) call deallocate_error('threesd2','zvec')
        deallocate(yvec,stat=status)
        if (status/=0) call deallocate_error('threesd2','yvec')
        deallocate(xvec,stat=status)
        if (status/=0) call deallocate_error('threesd2','xvec')
        maxvector = nint(1.1*nvector)
        allocate(xvec(maxvector),stat=status)
        if (status/=0) call outofmemory('threesd2','xvec')
        allocate(yvec(maxvector),stat=status)
        if (status/=0) call outofmemory('threesd2','yvec')
        allocate(zvec(maxvector),stat=status)
        if (status/=0) call outofmemory('threesd2','zvec')
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
    ixf = - 2
    iyf = - 1
    izf =   0
    ixfo = 1
    iyfo = 2
    izfo = 3
    iloop: do i = 1,nloopi
      if (lgrad1) then
        ni = nat(i)
        ntypi = nftype(i)
        oci = occuf(i)
        qli = qf(i)
        nreli = nrelat(i)
        ii2 = i
        lasu1 = (nrel2(nreli).eq.i)
        if (lasu1) then
          neqi = neqv(nreli)
        else
          neqi = 1
        endif
        lopi = (.not.lfreeze.or.lopf(nrelat(i)))
        if (lopi) then
          if (lasu1) then
            ix = ix + 3
            iy = iy + 3
            iz = iz + 3
          endif
          ixf = ixf + 3
          iyf = iyf + 3
          izf = izf + 3
        endif
      else
        ni = iatn(i)
        ntypi = natype(i)
        oci = occua(i)
        neqi = neqv(i)
        ii2 = nrel2(i)
        nreli = i
        lasu1 = .true.
        lopi = (.not.lfreeze.or.lopf(i))
        if (lopi) then
          ix = ix + 3
          iy = iy + 3
          iz = iz + 3
          ixf = ixfo
          iyf = iyfo
          izf = izfo
          ixfo = ixfo + 3*neqi
          iyfo = iyfo + 3*neqi
          izfo = izfo + 3*neqi
        endif
      endif
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
! 
!  Dreiding option handling                 
! 
      if (ltdreiding(n)) then               
        if (.not.bonded2donor(ii2)) cycle iloop
      endif
!
      if (lgrad1) then
        xc1 = xclat(i)
        yc1 = yclat(i)
        zc1 = zclat(i)
      else
        xc1 = xalat(i)
        yc1 = yalat(i)
        zc1 = zalat(i)
      endif
!
!  Molecule handling
!
      if (lmol.and.lneedmol) then
        nmi = natmol(ii2)
        if (ndim.gt.0) then
          indm = nmolind(ii2)
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
          if (nbonds(ii2).eq.n3bondno(in3,1,n)) lbondnoOK = .true.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
      if (n3bondnono(2,n).gt.0) then
        lbondnoOK = .true.
        do in3 = 1,n3bondnono(2,n)
          if (nbonds(ii2).eq.n3bondno(in3,2,n)) lbondnoOK = .false.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
!
!  Set loop range for j
!
      if (lmol.and.lneedmol.and.lbtyp) then
        ljbond = .true.
        if (nbonds(ii2).gt.0) then
          nuniquej = 1
          nuniquejptr(1) = nbonded(1,ii2)
          do jloop = 2,nbonds(ii2)
            lunique = .true.
            do lu = 1,nuniquej
              if (nbonded(jloop,ii2).eq.nuniquejptr(lu)) lunique = .false.
            enddo
            if (lunique) then
              nuniquej = nuniquej + 1
              nuniquejptr(nuniquej) = nbonded(jloop,ii2)
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
        ocj = occuf(j)
        qlj  =  qf(j)
        nrelj = nrelat(j)
        lasu2 = (nrel2(nrelj).eq.j)
        if (lasu2) neqj = neqv(nrelj)
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        if (lopj) then
          if (lasu2) then
            jx = 3*(ioptaptr(nrelj)-1) + 1
            jy = jx + 1
            jz = jx + 2
          endif
          jxf = 3*(ioptptr(j)-1) + 1
        endif
!
        if (lmol.and.lneedmol) then
!
!  Molecule handling
!
          nmj = natmol(j)
          if (ndim.gt.0) then
            indmj = nmolind(j)
            izj = (indmj/100) - 5
            ind = indmj-100*(izj+5)
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
          if (r122.lt.1.0d-12) cycle iiloop
!
!  Molecule checking
!
          lbonded = .false.
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle iiloop
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,ii2,j,0_i4,0_i4,0_i4)
                if (.not.lbonded) cycle iiloop
              endif
            else
              call lintoijk(ixx,iyy,izz,ii,imax,jmax,kmax)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,ii2,j,ixx,iyy,izz)
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
!  Distance checking
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
            kloop = nbonds(ii2) + nbonds(j)
            do lk = 1,nbonds(ii2)
              lijdone(nbonded(lk,ii2)) = .false.
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
              if (lk.le.nbonds(ii2)) then
                k = nbonded(lk,ii2)
              else
                k = nbonded(lk-nbonds(ii2),j)
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
!  Set properties for k
!
            ock = occuf(k)
            qlk = qf(k)
            nrelk = nrelat(k)
            lasu3 = (nrel2(nrelk).eq.k)
            if (lasu3) neqk = neqv(nrelk)
            lopk = (.not.lfreeze.or.lopf(nrelat(k)))
            if (lopk) then
              if (lasu3) then
                kx = 3*(ioptaptr(nrelk)-1) + 1
                ky = kx + 1
                kz = kx + 2
              endif
              kxf = 3*(ioptptr(k)-1) + 1
            endif
!
!  If all frozen then skip term
!
            if (.not.lopi.and..not.lopj.and..not.lopk) cycle lkloop
            if (.not.lasu1.and..not.lasu2.and..not.lasu3) cycle lkloop
!
!  Dreiding option handling
!
            if (ltdreiding(n)) then
              if (.not.bonded2donorJK(ii2,j,k)) cycle lkloop
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
                ind = indmk - 100*(izk+5)
                iyk = (ind/10)-5
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
              if (r132.lt.1.0d-12) cycle jjloop
!
!  Molecule checking
!
            lbonded = .false.
            if (lmolok2) then
              if (ndim.eq.0) then
                if (linter_only) cycle jjloop
                if (lbtyp) then
                  call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,ii2,k,0_i4,0_i4,0_i4)
                  if (.not.lbonded) cycle jjloop
                  if (lsymijk) then
                    call bonded(lbonded,l2bonds,nbtypejk,nbtypejk2,j,k,0_i4,0_i4,0_i4)
                    if (.not.lbonded) cycle jjloop
                  endif
                endif
              else
                call lintoijk(jxx,jyy,jzz,jj,imax,jmax,kmax)
                if (lbtyp) then
                  call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,ii2,k,jxx,jyy,jzz)
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
            if (r232.lt.tr3m.or.r232.lt.1d-12) cycle jjloop
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
            rsfct = neqi
!*****************************************************
!  Calculate derivatives with respect to potentials  *
!*****************************************************
            call threebody(1_i4,n3ty,r12,r13,r23,ed11,ed12,ed13,ethb1,e2d,e3d,ttr11,ttr21,tr31,rho1,rho2,rho3, &
                           rho4,rho5,rk32,rk33,rk34,the0,ang,dot,lgrad1,lgrad2,.false.,n,qli,qlj,qlk,d0i,d0j,d0k, &
                           d1q,d2q,lthetataper(n),thetatapermin(n),thetatapermax(n))
            if (lasu1) then
              ethb = ethb + ethb1*rsfct
!
!  Output energy contribution
!
              if (lPrintThree) then
                write(ioout,'(4x,3i12,8x,f27.10)') i,j,k,ethb1*rsfct
              endif
            endif
!*************************
!  Start of derivatives  *
!*************************
            if (lgrad1) then
!
!  Set up strain products
!
              if (lsg1) then
                rprod(1,1) = xt21*xt21
                rprod(2,1) = yt21*yt21
                rprod(3,1) = zt21*zt21
                rprod(4,1) = yt21*zt21
                rprod(5,1) = xt21*zt21
                rprod(6,1) = xt21*yt21
                rprod(1,2) = xt31*xt31
                rprod(2,2) = yt31*yt31
                rprod(3,2) = zt31*zt31
                rprod(4,2) = yt31*zt31
                rprod(5,2) = xt31*zt31
                rprod(6,2) = xt31*yt31
                rprod(1,3) = x23*x23
                rprod(2,3) = y23*y23
                rprod(3,3) = z23*z23
                rprod(4,3) = y23*z23
                rprod(5,3) = x23*z23
                rprod(6,3) = x23*y23
              endif
            endif
            if (lgrad2) then
              if (lasu1) then
                n1x = ix
                n1y = iy
                n1z = iz
              endif
              if (lasu2) then
                n2x = jx
                n2y = jy
                n2z = jz
              endif
              if (lasu3) then
                n3x = kx
                n3y = ky
                n3z = kz
              endif
              n1xf = ixf
              n2xf = jxf
              n3xf = kxf
            endif
!***********************
!  Strain derivatives  *
!***********************
            if (lsg1) then
!
!  First strain derivatives
!
              if (lasu1) then
                do kl = 1,6
                  rstrd(kl) = rstrd(kl) + ed11*rprod(kl,1)*rsfct
                  rstrd(kl) = rstrd(kl) + ed12*rprod(kl,2)*rsfct
                  rstrd(kl) = rstrd(kl) + ed13*rprod(kl,3)*rsfct
                enddo
              endif
!
!  Second strain derivatives
!
!  Strain only
!
              if (lgrad2.and.lstr) then
                do kk = 1,6
                  tm2(kk,1) = e2d(1)*rprod(kk,1) + e2d(2)*rprod(kk,2) + e2d(3)*rprod(kk,3)
                  tm2(kk,2) = e2d(2)*rprod(kk,1) + e2d(4)*rprod(kk,2) + e2d(5)*rprod(kk,3)
                  tm2(kk,3) = e2d(3)*rprod(kk,1) + e2d(5)*rprod(kk,2) + e2d(6)*rprod(kk,3)
                enddo
                if (lasu1) then
                  sderv2(1,1) = sderv2(1,1) + (rprod(1,1)*tm2(1,1) + rprod(1,2)*tm2(1,2) + rprod(1,3)*tm2(1,3))*rsfct
                  sderv2(2,1) = sderv2(2,1) + (rprod(1,1)*tm2(2,1) + rprod(1,2)*tm2(2,2) + rprod(1,3)*tm2(2,3))*rsfct
                  sderv2(3,1) = sderv2(3,1) + (rprod(1,1)*tm2(3,1) + rprod(1,2)*tm2(3,2) + rprod(1,3)*tm2(3,3))*rsfct
                  sderv2(4,1) = sderv2(4,1) + (rprod(1,1)*tm2(4,1) + rprod(1,2)*tm2(4,2) + rprod(1,3)*tm2(4,3))*rsfct
                  sderv2(5,1) = sderv2(5,1) + (rprod(1,1)*tm2(5,1) + rprod(1,2)*tm2(5,2) + rprod(1,3)*tm2(5,3))*rsfct
                  sderv2(6,1) = sderv2(6,1) + (rprod(1,1)*tm2(6,1) + rprod(1,2)*tm2(6,2) + rprod(1,3)*tm2(6,3))*rsfct
                  sderv2(2,2) = sderv2(2,2) + (rprod(2,1)*tm2(2,1) + rprod(2,2)*tm2(2,2) + rprod(2,3)*tm2(2,3))*rsfct
                  sderv2(3,2) = sderv2(3,2) + (rprod(2,1)*tm2(3,1) + rprod(2,2)*tm2(3,2) + rprod(2,3)*tm2(3,3))*rsfct
                  sderv2(4,2) = sderv2(4,2) + (rprod(2,1)*tm2(4,1) + rprod(2,2)*tm2(4,2) + rprod(2,3)*tm2(4,3))*rsfct
                  sderv2(5,2) = sderv2(5,2) + (rprod(2,1)*tm2(5,1) + rprod(2,2)*tm2(5,2) + rprod(2,3)*tm2(5,3))*rsfct
                  sderv2(6,2) = sderv2(6,2) + (rprod(2,1)*tm2(6,1) + rprod(2,2)*tm2(6,2) + rprod(2,3)*tm2(6,3))*rsfct
                  sderv2(3,3) = sderv2(3,3) + (rprod(3,1)*tm2(3,1) + rprod(3,2)*tm2(3,2) + rprod(3,3)*tm2(3,3))*rsfct
                  sderv2(4,3) = sderv2(4,3) + (rprod(3,1)*tm2(4,1) + rprod(3,2)*tm2(4,2) + rprod(3,3)*tm2(4,3))*rsfct
                  sderv2(5,3) = sderv2(5,3) + (rprod(3,1)*tm2(5,1) + rprod(3,2)*tm2(5,2) + rprod(3,3)*tm2(5,3))*rsfct
                  sderv2(6,3) = sderv2(6,3) + (rprod(3,1)*tm2(6,1) + rprod(3,2)*tm2(6,2) + rprod(3,3)*tm2(6,3))*rsfct
                  sderv2(4,4) = sderv2(4,4) + (rprod(4,1)*tm2(4,1) + rprod(4,2)*tm2(4,2) + rprod(4,3)*tm2(4,3))*rsfct
                  sderv2(5,4) = sderv2(5,4) + (rprod(4,1)*tm2(5,1) + rprod(4,2)*tm2(5,2) + rprod(4,3)*tm2(5,3))*rsfct
                  sderv2(6,4) = sderv2(6,4) + (rprod(4,1)*tm2(6,1) + rprod(4,2)*tm2(6,2) + rprod(4,3)*tm2(6,3))*rsfct
                  sderv2(5,5) = sderv2(5,5) + (rprod(5,1)*tm2(5,1) + rprod(5,2)*tm2(5,2) + rprod(5,3)*tm2(5,3))*rsfct
                  sderv2(6,5) = sderv2(6,5) + (rprod(5,1)*tm2(6,1) + rprod(5,2)*tm2(6,2) + rprod(5,3)*tm2(6,3))*rsfct
                  sderv2(6,6) = sderv2(6,6) + (rprod(6,1)*tm2(6,1) + rprod(6,2)*tm2(6,2) + rprod(6,3)*tm2(6,3))*rsfct
!
!  Mixed derivatives
!
                  do kk = 1,6
                    derv3(n1x,kk) = derv3(n1x,kk) - (xt21*tm2(kk,1) + xt31*tm2(kk,2))*neqi
                    derv3(n1y,kk) = derv3(n1y,kk) - (yt21*tm2(kk,1) + yt31*tm2(kk,2))*neqi
                    derv3(n1z,kk) = derv3(n1z,kk) - (zt21*tm2(kk,1) + zt31*tm2(kk,2))*neqi
                  enddo
                endif
                if (lasu2) then
                  do kk = 1,6
                    derv3(n2x,kk) = derv3(n2x,kk) + (xt21*tm2(kk,1) - x23*tm2(kk,3))*neqj
                    derv3(n2y,kk) = derv3(n2y,kk) + (yt21*tm2(kk,1) - y23*tm2(kk,3))*neqj
                    derv3(n2z,kk) = derv3(n2z,kk) + (zt21*tm2(kk,1) - z23*tm2(kk,3))*neqj
                  enddo
                endif
                if (lasu3) then
                  do kk = 1,6
                    derv3(n3x,kk) = derv3(n3x,kk) + (xt31*tm2(kk,2) + x23*tm2(kk,3))*neqk
                    derv3(n3y,kk) = derv3(n3y,kk) + (yt31*tm2(kk,2) + y23*tm2(kk,3))*neqk
                    derv3(n3z,kk) = derv3(n3z,kk) + (zt31*tm2(kk,2) + z23*tm2(kk,3))*neqk
                  enddo
                endif
              endif
            endif
!*************************
!  Internal derivatives  *
!*************************
            if (lgrad1) then
              if (lasu1) then
                xdrv(nreli) = xdrv(nreli) - (xt21*ed11 + xt31*ed12)*neqi
                ydrv(nreli) = ydrv(nreli) - (yt21*ed11 + yt31*ed12)*neqi
                zdrv(nreli) = zdrv(nreli) - (zt21*ed11 + zt31*ed12)*neqi
              endif
              if (lasu2) then
                xdrv(nrelj) = xdrv(nrelj) + (xt21*ed11 - x23*ed13)*neqj
                ydrv(nrelj) = ydrv(nrelj) + (yt21*ed11 - y23*ed13)*neqj
                zdrv(nrelj) = zdrv(nrelj) + (zt21*ed11 - z23*ed13)*neqj
              endif
              if (lasu3) then
                xdrv(nrelk) = xdrv(nrelk) + (xt31*ed12 + x23*ed13)*neqk
                ydrv(nrelk) = ydrv(nrelk) + (yt31*ed12 + y23*ed13)*neqk
                zdrv(nrelk) = zdrv(nrelk) + (zt31*ed12 + z23*ed13)*neqk
              endif
            endif
            if (lgrad2) then
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
                n11f = n1xf - 1 + kk
                n21f = n2xf - 1 + kk
                n31f = n3xf - 1 + kk
!
!  First term
!
                if (lasu1.and.lopi) then
                  n11 = n1x - 1 + kk
                  if (lopj) then
                    derv2(n21f,n11) = derv2(n21f,n11) - ed11*neqi
                  else
                    derv2(n11f,n11) = derv2(n11f,n11) - ed11*neqi
                  endif
                  if (lopk) then
                    derv2(n31f,n11) = derv2(n31f,n11) - ed12*neqi
                  else
                    derv2(n11f,n11) = derv2(n11f,n11) - ed12*neqi
                  endif
                endif
                if (lasu2.and.lopj) then
                  n21 = n2x - 1 + kk
                  if (lopi) then
                    derv2(n11f,n21) = derv2(n11f,n21) - ed11*neqj
                  else
                    derv2(n21f,n21) = derv2(n21f,n21) - ed11*neqj
                  endif
                  if (lopk) then
                    derv2(n31f,n21) = derv2(n31f,n21) - ed13*neqj
                  else
                    derv2(n21f,n21) = derv2(n21f,n21) - ed13*neqj
                  endif
                endif
                if (lasu3.and.lopk) then
                  n31 = n3x - 1 + kk
                  if (lopi) then
                    derv2(n11f,n31) = derv2(n11f,n31) - ed12*neqk
                  else
                    derv2(n31f,n31) = derv2(n31f,n31) - ed12*neqk
                  endif
                  if (lopj) then
                    derv2(n21f,n31) = derv2(n21f,n31) - ed13*neqk
                  else
                    derv2(n31f,n31) = derv2(n31f,n31) - ed13*neqk
                  endif
                endif
!
!  Second term
!
                do kl = 1,3
                  n12f = n1xf - 1 + kl
                  n22f = n2xf - 1 + kl
                  n32f = n3xf - 1 + kl
                  trm11 = tm1(1,kk,kl)
                  trm12 = tm1(2,kk,kl)
                  trm13 = tm1(3,kk,kl)
                  trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                  if (lasu1.and.lopi) then
                    n12 = n1x - 1 + kl
                    if (lopj) then
                      derv2(n22f,n11) = derv2(n22f,n11) + trm1*neqi
                      derv2(n21f,n12) = derv2(n21f,n12) - trm11*neqi
                    else
                      derv2(n12f,n11) = derv2(n12f,n11) + trm1*neqi
                      derv2(n11f,n12) = derv2(n11f,n12) - trm11*neqi
                    endif
                  endif
                  if (lasu2.and.lopj) then
                    n22 = n2x - 1 + kl
                    if (lopi) then
                      derv2(n12f,n21) = derv2(n12f,n21) - trm11*neqj
                      derv2(n11f,n22) = derv2(n11f,n22) + trm1*neqj
                    else
                      derv2(n22f,n21) = derv2(n22f,n21) - trm11*neqj
                      derv2(n21f,n22) = derv2(n21f,n22) + trm1*neqj
                    endif
                  endif
                  trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                  if (lasu1.and.lopi) then
                    if (lopk) then
                      derv2(n32f,n11) = derv2(n32f,n11) + trm1*neqi
                    else
                      derv2(n12f,n11) = derv2(n12f,n11) + trm1*neqi
                    endif
                  endif
                  if (lasu3.and.lopk) then
                    n32 = n3x - 1 + kl
                    if (lopi) then
                      derv2(n11f,n32) = derv2(n11f,n32) + trm1*neqk
                    else
                      derv2(n31f,n32) = derv2(n31f,n32) + trm1*neqk
                    endif
                  endif
                  trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                  if (lasu2.and.lopj) then
                    if (lopk) then
                      derv2(n32f,n21) = derv2(n32f,n21) + trm1*neqj
                      derv2(n31f,n22) = derv2(n31f,n22) - trm13*neqj
                    else
                      derv2(n22f,n21) = derv2(n22f,n21) + trm1*neqj
                      derv2(n21f,n22) = derv2(n21f,n22) - trm13*neqj
                    endif
                  endif
                  if (lasu3.and.lopk) then
                    if (lopj) then
                      derv2(n21f,n32) = derv2(n21f,n32) + trm1*neqk
                      derv2(n22f,n31) = derv2(n22f,n31) - trm13*neqk
                    else
                      derv2(n31f,n32) = derv2(n31f,n32) + trm1*neqk
                      derv2(n32f,n31) = derv2(n32f,n31) - trm13*neqk
                    endif
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
!  Free local memory
!
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('threesd2','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('threesd2','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('threesd2','xvec')
  deallocate(ioptaptr,stat=status)
  if (status/=0) call deallocate_error('threesd2','ioptaptr')
  deallocate(ioptptr,stat=status)
  if (status/=0) call deallocate_error('threesd2','ioptptr')
  deallocate(nuniquejptr,stat=status)
  if (status/=0) call deallocate_error('threesd2','nuniquejptr')
  deallocate(lijdone,stat=status)
  if (status/=0) call deallocate_error('threesd2','lijdone')
!
!  Timing
!
  time2 = cputime()
  tthree = tthree + time2 - time1
!
  return
  end
