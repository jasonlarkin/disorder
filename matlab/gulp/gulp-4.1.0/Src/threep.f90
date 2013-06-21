  subroutine threep(xkv,ykv,zkv)
!
!  Subroutine for three-body contribution to phonons.
!  Modified for distance dependent three body terms.
!
!  Strategy - sift by potential first, then cutoffs
!
!  11/94 Axilrod-Teller now added
!   1/95 Intra/intermolecular specification added
!   2/95 Exponential and SW three-body potentials added
!   3/95 Bcross potential added
!   3/95 Periodic molecule corrections added
!   6/95 Correction added for asymmetric cutoffs with symmetric
!        potential
!   3/96 Urey-Bradley potential added
!   4/96 Exponentially decaying Vessal form added
!   6/96 General theta0 added to SW3
!   9/97 Bug in sw3 derivatives fixed
!   3/98 Cosine-harmonic form added
!   5/98 Sign of dervi switched to match change in real/recip
!   5/98 Potential dependent parts placed in separated subroutine
!   6/98 Murrell-Mottram potential added
!   8/98 Diagonal blocks set to be difference from gamma point
!  10/98 BAcross potential added
!  10/98 Conversion of theta to rad now done here
!   8/99 Linear-three potential added
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   4/01 Passed argument changed from K point number to K point 
!        coordinate
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
!  10/04 Modifications for bond order charges added
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
!   3/08 3coulomb potential added
!   6/09 Module name changed from three to m_three
!   7/09 Modifications for exp2 potential added
!   4/10 Modified in the style of threemd to accelerate bonded potential case
!   5/10 g3coulomb potential added
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
  use control,      only : lDoQDeriv2
  use current
  use derivatives
  use m_three
  use molecule
  use times
  implicit none
!
!  Passed variables
!
  real(dp),   intent(in)                    :: xkv
  real(dp),   intent(in)                    :: ykv
  real(dp),   intent(in)                    :: zkv
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
  integer(i4)                               :: jxx
  integer(i4)                               :: jyy
  integer(i4)                               :: jzz
  integer(i4)                               :: k
  integer(i4)                               :: kk
  integer(i4)                               :: kl
  integer(i4)                               :: kloop
  integer(i4)                               :: kloopmin
  integer(i4)                               :: kmax
  integer(i4)                               :: lj
  integer(i4)                               :: lk
  integer(i4)                               :: lu
  integer(i4)                               :: m
  integer(i4),                         save :: maxvector = 100
  integer(i4)                               :: n
  integer(i4)                               :: n11
  integer(i4)                               :: n12
  integer(i4)                               :: n21
  integer(i4)                               :: n22
  integer(i4)                               :: n31
  integer(i4)                               :: n32
  integer(i4)                               :: n1x
  integer(i4)                               :: n1y
  integer(i4)                               :: n1z
  integer(i4)                               :: n2x
  integer(i4)                               :: n2y
  integer(i4)                               :: n2z
  integer(i4)                               :: n3x
  integer(i4)                               :: n3y
  integer(i4)                               :: n3z
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
  integer(i4)                               :: ni
  integer(i4)                               :: nj
  integer(i4)                               :: nk
  integer(i4)                               :: nmi
  integer(i4)                               :: nmid
  integer(i4)                               :: nmj
  integer(i4)                               :: nmk
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
  logical                                   :: lsamemol
  logical                                   :: lsymijk
  logical                                   :: lswaprho
  logical                                   :: lswapk
  logical                                   :: lunique
  real(dp)                                  :: ang
  real(dp)                                  :: cos12
  real(dp)                                  :: cos13
  real(dp)                                  :: cos23
  real(dp)                                  :: cputime
  real(dp)                                  :: cut
  real(dp)                                  :: d0i
  real(dp)                                  :: d0j
  real(dp)                                  :: d0k
  real(dp)                                  :: d1q(3,3)
  real(dp)                                  :: d1qxyz(3,3,3)
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
  real(dp)                                  :: oneij
  real(dp)                                  :: oneik
  real(dp)                                  :: onejk
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
  real(dp)                                  :: sin12
  real(dp)                                  :: sin13
  real(dp)                                  :: sin23
  real(dp)                                  :: symfct
  real(dp)                                  :: the0
  real(dp)                                  :: time1
  real(dp)                                  :: time2
  real(dp)                                  :: tm1(3,3,3)
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
!
!  Allocate local memory
!
  allocate(lijdone(numat),stat=status)
  if (status/=0) call outofmemory('threep','lijdone')
  allocate(nuniquejptr(numat),stat=status)
  if (status/=0) call outofmemory('threep','nuniquejptr')
  allocate(xvec(maxvector),stat=status)
  if (status/=0) call outofmemory('threep','xvec')
  allocate(yvec(maxvector),stat=status)
  if (status/=0) call outofmemory('threep','yvec')
  allocate(zvec(maxvector),stat=status)
  if (status/=0) call outofmemory('threep','zvec')
!
  lijdone(1:numat) = .false.
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
!
!  Create lattice vectors
!
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
      if (status/=0) call deallocate_error('threep','zvec')
      deallocate(yvec,stat=status)
      if (status/=0) call deallocate_error('threep','yvec')
      deallocate(xvec,stat=status)
      if (status/=0) call deallocate_error('threep','xvec')
      maxvector = nint(1.1*nvector)
      allocate(xvec(maxvector),stat=status)
      if (status/=0) call outofmemory('threep','xvec')
      allocate(yvec(maxvector),stat=status)
      if (status/=0) call outofmemory('threep','yvec')
      allocate(zvec(maxvector),stat=status)
      if (status/=0) call outofmemory('threep','zvec')
      call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
    endif
!
!  Outer loop over sites
!
    iloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
!
!  Set properties of i
!
      oci = occuf(i)
      qli = qf(i)
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
        indm = nmolind(i)
        izi = (indm/100) - 5
        ind = indm - 100*(izi+5)
        iyi = (ind/10) - 5
        ind = ind - 10*(iyi+5)
        ixi = ind - 5
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
!  Set properties of atom j
!
        ocj = occuf(j)
        qlj = qf(j)
!
        if (lmol.and.lneedmol) then
!
!  Molecule handling
!
          nmj = natmol(j)
          indmj = nmolind(j)
          izj = (indmj/100) - 5
          ind = indmj-100*(izj+5)
          iyj = (ind/10) - 5
          ind = ind - 10*(iyj+5)
          ixj = ind - 5
          ixj = ixj - ixi
          iyj = iyj - iyi
          izj = izj - izi
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
!  Set properties of atom k
!
            ock = occuf(k)
            qlk = qf(k)
!
!  Dreiding option handling
!
            if (ltdreiding(n)) then
              if (.not.bonded2donorJK(i,j,k)) cycle lkloop
            endif
!
            if (j.eq.k) then
              jjmin = ii + 1
            else
              jjmin = 1
            endif
            if (lmol.and.lneedmol) then
!
!  Molecule handling
!
              nmk = natmol(k)
              indmk = nmolind(k)
              izk = (indmk/100) - 5
              ind = indmk - 100*(izk+5)
              iyk = (ind/10) - 5
              ind = ind - 10*(iyk+5)
              ixk = ind - 5
              ixk = ixk - ixi
              iyk = iyk - iyi
              izk = izk - izi
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
              if (r232.lt.tr3m.or.r232.lt.1d-10) cycle jjloop
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
                if (abs(dot).gt.0.999999999999_dp) dot = sign(1.0_dp,dot)
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
              call threebody(1_i4,n3ty,r12,r13,r23,ed11,ed12,ed13,ethb1,e2d,e3d,ttr11,ttr21,tr31,rho1, &
                             rho2,rho3,rho4,rho5,rk32,rk33,rk34,the0,ang,dot,.true.,.true.,.false.,n,qli,qlj,qlk, &
                             d0i,d0j,d0k,d1q,d2q,lthetataper(n),thetatapermin(n),thetatapermax(n))
!
              n1x = 3*(i-1) + 1
              n1y = n1x + 1
              n1z = n1x + 2
              n2x = 3*(j-1) + 1
              n2y = n2x + 1
              n2z = n2x + 2
              n3x = 3*(k-1) + 1
              n3y = n3x + 1
              n3z = n3x + 2
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
                call d2charge3p(i,j,k,1_i4,n1x,n1y,n1z,n2x,n2y,n2z,n3x,n3y,n3z,xkv,ykv,zkv,d0i,d0j,d0k,d1qxyz,d2q)
              endif
!
!  Regular derivatives
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
!***************************************
!  Generate phased second derivatives  *
!***************************************
!
!  Set diagonal blocks to be difference from gamma point
!
              if (i.eq.j) then
                oneij = 1.0_dp
              else
                oneij = 0.0_dp
              endif
              if (i.eq.k) then
                oneik = 1.0_dp
              else
                oneik = 0.0_dp
              endif
              if (j.eq.k) then
                onejk = 1.0_dp
              else
                onejk = 0.0_dp
              endif
!
!  Calculate phase factors for each of three pairs
!
              cos12 = xkv*xt21 + ykv*yt21 + zkv*zt21
              sin12 = sin(cos12)
              cos12 = cos(cos12) - oneij
              cos13 = xkv*xt31 + ykv*yt31 + zkv*zt31
              sin13 = sin(cos13)
              cos13 = cos(cos13) - oneik
              cos23 = xkv*x23 + ykv*y23 + zkv*z23
              sin23 = sin(cos23)
              cos23 = cos(cos23) - onejk
!
!  Outer loop over cartesian coordinates
!
              do kk = 1,3
                n11 = n1x - 1 + kk
                n21 = n2x - 1 + kk
                n31 = n3x - 1 + kk
                derv2(n11,n21) = derv2(n11,n21) - ed11*cos12
                derv2(n21,n11) = derv2(n21,n11) - ed11*cos12
                derv2(n11,n31) = derv2(n11,n31) - ed12*cos13
                derv2(n31,n11) = derv2(n31,n11) - ed12*cos13
                derv2(n21,n31) = derv2(n21,n31) - ed13*cos23
                derv2(n31,n21) = derv2(n31,n21) - ed13*cos23
                dervi(n11,n21) = dervi(n11,n21) + ed11*sin12
                dervi(n21,n11) = dervi(n21,n11) - ed11*sin12
                dervi(n11,n31) = dervi(n11,n31) + ed12*sin13
                dervi(n31,n11) = dervi(n31,n11) - ed12*sin13
                dervi(n21,n31) = dervi(n21,n31) + ed13*sin23
                dervi(n31,n21) = dervi(n31,n21) - ed13*sin23
!
!  Inner loop over cartesian coordinates
!
                do kl = 1,3
                  n12 = n1x - 1 + kl
                  n22 = n2x - 1 + kl
                  n32 = n3x - 1 + kl
                  trm11 = tm1(1,kk,kl)
                  trm12 = tm1(2,kk,kl)
                  trm13 = tm1(3,kk,kl)
                  trm1 = trm12 + trm13 - e2d(1)*r12v(kk)*r12v(kl)
                  derv2(n11,n22) = derv2(n11,n22) + trm1*cos12
                  derv2(n22,n11) = derv2(n22,n11) + trm1*cos12
                  derv2(n12,n21) = derv2(n12,n21) - trm11*cos12
                  derv2(n21,n12) = derv2(n21,n12) - trm11*cos12
                  dervi(n11,n22) = dervi(n11,n22) - trm1*sin12
                  dervi(n22,n11) = dervi(n22,n11) + trm1*sin12
                  dervi(n12,n21) = dervi(n12,n21) + trm11*sin12
                  dervi(n21,n12) = dervi(n21,n12) - trm11*sin12
                  trm1 = - trm11 - trm12 - trm13 - e2d(4)*r13v(kk)*r13v(kl)
                  derv2(n11,n32) = derv2(n11,n32) + trm1*cos13
                  derv2(n32,n11) = derv2(n32,n11) + trm1*cos13
                  dervi(n11,n32) = dervi(n11,n32) - trm1*sin13
                  dervi(n32,n11) = dervi(n32,n11) + trm1*sin13
                  trm1 = trm11 + trm12 - e2d(6)*r23v(kk)*r23v(kl)
                  derv2(n21,n32) = derv2(n21,n32) + trm1*cos23
                  derv2(n32,n21) = derv2(n32,n21) + trm1*cos23
                  derv2(n22,n31) = derv2(n22,n31) - trm13*cos23
                  derv2(n31,n22) = derv2(n31,n22) - trm13*cos23
                  dervi(n21,n32) = dervi(n21,n32) - trm1*sin23
                  dervi(n32,n21) = dervi(n32,n21) + trm1*sin23
                  dervi(n22,n31) = dervi(n22,n31) + trm13*sin23
                  dervi(n31,n22) = dervi(n31,n22) - trm13*sin23
                enddo
              enddo
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
!  Free local memory
!
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('threep','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('threep','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('threep','xvec')
  deallocate(nuniquejptr,stat=status)
  if (status/=0) call deallocate_error('threep','nuniquejptr')
  deallocate(lijdone,stat=status)
  if (status/=0) call deallocate_error('threep','lijdone')
!
!  Timing
!
  time2 = cputime()
  tthree = tthree + time2 - time1
!
  return
  end
