  subroutine fournos(efor,eoop,esregion12,esregion2,eattach,lgrad1,lgrad2)
!
!  Subroutine for four-body energy
!
!   1/95 Intra/inter-molecular specification added
!   2/95 Phi0 offset added for standard torsion potential
!   2/95 Bonded specification added for four-body terms
!   3/95 Periodic molecule corrections added
!  11/96 Compression of second derivatives added if lfreeze=.true.
!        Freezing also added for four body terms
!   3/97 Modified to handle out of plane potentials
!   6/98 Potential parts placed in separate subroutine
!   7/98 Second derivatives re-written for clarity
!   2/01 Call to rlist added to generalise to n-D and 14=>iimid
!   2/01 Modifications for general dimensionality added
!   2/01 lintoijk calls now use imaxl,jmaxl and kmaxl
!   4/01 Calculation of region 2 energies for surfaces added
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   6/01 Setting of lsamemol altered
!   9/01 Marvin compatibility option for SE added
!   9/01 lmolq calculations accelerated using lneedmol 
!  11/01 Attachment energy added
!   8/02 Surface energy calculation algorithm changed
!  10/02 Torharm potential added
!  10/02 Second derivatives modified to correct freezing bug
!  11/02 Wildcard atom types added
!   4/03 Exponentially decaying torsion added
!   4/03 Tapered torsion added
!   5/03 Looping over lattice vectors generalised
!   6/03 Bug in second derivatives with freezing corrected
!   9/03 imaxl, jmaxl & kmaxl renamed to avoid conflict with global symbols
!   4/04 Typo in lop flags for second derivatives corrected
!   6/04 Sign of virial corrected
!  11/04 Modifications for torangle potential added
!   6/06 Inversion squared potential added
!   9/06 Order of atom search changed to allow for Dreiding 
!   9/06 Dreiding scheme for force constant added as an option
!   1/07 Wildcard handling in lmatch calls corrected
!   1/07 UFF4 added
!   2/07 Bonding types and bonding test added
!   2/07 Calculation of derv3 now checks lopi/lopj/lopk/lopl to avoid bounds error
!   4/07 Code reordered so that atom j/k loops are on the outside and potentials on the inside
!   4/07 Screening of j & k for potential middle species added
!   4/07 Logicals introduced to reduce number of square roots
!   4/07 Bond type checking for ndim > 0
!   5/07 QM/MM schemes added
!   6/07 lmolok reset to initial value for each potential loop
!  10/07 Angle-angle cross potential added
!  10/07 Error in checking of exocyclic attribute for bonds corrected
!  10/07 Handling of n4botype(2,n) modified
!  12/07 Unused variables removed
!   1/08 More unused variables removed
!   5/08 UFFoop potential added
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 Setting of maximum cutoffs now only looks at non-out of plane potentials
!  11/08 New logic for matching species introduced to handle case of the same element
!        being the middle atom, but one with a specific type and the other without.
!  11/08 Corrections for potential dependent swapping of terms according to atom 
!        assignments added.
!  11/08 ixl etc defined relative to ixj etc rather than ixk to correct error
!  11/08 Option to output energy terms added
!  11/09 Region derivatives added
!   4/10 Code modified to increase speed for bonded potentials by only searching over
!        bonded atoms
!   4/10 Error in reordering of initial lines for first atom type fixed
!  11/11 Region-region energy contributions stored
!   4/12 Explicit virial calculation removed as no longer needed
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
  use configurations, only : lsliceatom, nregions, nregionno, nregiontype, QMMMmode
  use constants
  use control,        only : lmarvreg2, lseok, latomicstress
  use current
  use derivatives
  use energies,       only : eregion2region
  use four
  use iochannels,     only : ioout
  use mdlogic
  use molecule
  use parallel,       only : ioproc
  use optimisation
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  real(dp), intent(inout)                      :: efor
  real(dp), intent(inout)                      :: eoop
  real(dp), intent(inout)                      :: esregion12
  real(dp), intent(inout)                      :: esregion2
  real(dp), intent(inout)                      :: eattach
  logical,  intent(in)                         :: lgrad1
  logical,  intent(in)                         :: lgrad2
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: iloop
  integer(i4)                                  :: iloopmin
  integer(i4)                                  :: imax
  integer(i4)                                  :: isgn
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4), dimension(:), allocatable       :: ioptptr
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: ixk
  integer(i4)                                  :: iyk
  integer(i4)                                  :: izk
  integer(i4)                                  :: ixl
  integer(i4)                                  :: iyl
  integer(i4)                                  :: izl
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jmax
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kb(6,6)
  integer(i4)                                  :: ki
  integer(i4)                                  :: kj
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: kloop
  integer(i4)                                  :: kloopmin
  integer(i4)                                  :: kmax
  integer(i4)                                  :: kx
  integer(i4)                                  :: ky
  integer(i4)                                  :: kz
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kzz
  integer(i4)                                  :: l
  integer(i4)                                  :: l4
  integer(i4)                                  :: li
  integer(i4)                                  :: lk
  integer(i4)                                  :: ll
  integer(i4)                                  :: lloop
  integer(i4)                                  :: lloopmin
  integer(i4)                                  :: lu
  integer(i4)                                  :: lx
  integer(i4)                                  :: ly
  integer(i4)                                  :: lz
  integer(i4),                            save :: maxvector = 27
  integer(i4)                                  :: n
  integer(i4)                                  :: n112
  integer(i4)                                  :: n113
  integer(i4)                                  :: n114
  integer(i4)                                  :: n1x
  integer(i4)                                  :: n1y
  integer(i4)                                  :: n1z
  integer(i4)                                  :: n1x2
  integer(i4)                                  :: n1x3
  integer(i4)                                  :: n1x4
  integer(i4)                                  :: n211
  integer(i4)                                  :: n213
  integer(i4)                                  :: n214
  integer(i4)                                  :: n221
  integer(i4)                                  :: n2x
  integer(i4)                                  :: n2y
  integer(i4)                                  :: n2z
  integer(i4)                                  :: n2x1
  integer(i4)                                  :: n2x3
  integer(i4)                                  :: n2x4
  integer(i4)                                  :: n311
  integer(i4)                                  :: n312
  integer(i4)                                  :: n314
  integer(i4)                                  :: n321
  integer(i4)                                  :: n322
  integer(i4)                                  :: n3x
  integer(i4)                                  :: n3y
  integer(i4)                                  :: n3z
  integer(i4)                                  :: n3x1
  integer(i4)                                  :: n3x2
  integer(i4)                                  :: n3x4
  integer(i4)                                  :: n411
  integer(i4)                                  :: n412
  integer(i4)                                  :: n413
  integer(i4)                                  :: n421
  integer(i4)                                  :: n422
  integer(i4)                                  :: n423
  integer(i4)                                  :: n4x
  integer(i4)                                  :: n4y
  integer(i4)                                  :: n4z
  integer(i4)                                  :: n4x1
  integer(i4)                                  :: n4x2
  integer(i4)                                  :: n4x3
  integer(i4)                                  :: n3vec(3,4)
  integer(i4), dimension(:), allocatable       :: natmiddle
  integer(i4), dimension(:), allocatable       :: ntypmiddle
  integer(i4)                                  :: nbtypeji
  integer(i4)                                  :: nbtypejk
  integer(i4)                                  :: nbtypekl
  integer(i4)                                  :: nbtypeji2
  integer(i4)                                  :: nbtypejk2
  integer(i4)                                  :: nbtypekl2
  integer(i4)                                  :: neq
  integer(i4)                                  :: nfortype
  integer(i4)                                  :: nfornonoop
  integer(i4)                                  :: ni
  integer(i4)                                  :: nil
  integer(i4)                                  :: niltor
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nmi 
  integer(i4)                                  :: nmiddle
  integer(i4)                                  :: nmid
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: nn
  integer(i4)                                  :: noofp
  integer(i4)                                  :: npha
  integer(i4), dimension(:), allocatable       :: nptrnfornonoop
  integer(i4)                                  :: nr
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregionl
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nregiontypl
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: ntmp
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: nuniquei
  integer(i4)                                  :: nuniquek
  integer(i4)                                  :: nuniquel
  integer(i4), dimension(:), allocatable       :: nuniqueiptr
  integer(i4), dimension(:), allocatable       :: nuniquekptr
  integer(i4), dimension(:), allocatable       :: nuniquelptr
  integer(i4)                                  :: nvector
  integer(i4)                                  :: status
  logical                                      :: l2bondsij
  logical                                      :: l2bondsjk
  logical                                      :: l2bondskl
  logical                                      :: lallbtyp
  logical                                      :: lanybtyp
  logical                                      :: lanyneedmol
  logical                                      :: lattach
  logical                                      :: lbondedij
  logical                                      :: lbondedjk
  logical                                      :: lbondedkl
  logical                                      :: lbtyp
  logical                                      :: lexactmatch
  logical                                      :: libond
  logical                                      :: liok
  logical                                      :: limatch1
  logical                                      :: limatch4
  logical                                      :: lintra_only
  logical                                      :: linter_only
  logical                                      :: ljmatch2
  logical                                      :: ljmatch3
  logical                                      :: ljkmatch
  logical                                      :: lkbond
  logical                                      :: lkjmatch
  logical                                      :: lkmatch2
  logical                                      :: lkmatch3
  logical                                      :: llbond
  logical                                      :: lmatch
  logical                                      :: lmatchany
  logical                                      :: lmatchpair
  logical                                      :: lmeither
  logical                                      :: lmolloc
  logical                                      :: lmolok
  logical                                      :: lmolokjk
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lopk
  logical                                      :: lopl
  logical                                      :: lreg12
  logical                                      :: lreg2qtet
  logical                                      :: lrooted1
  logical                                      :: lrooted32
  logical                                      :: lsamemolij
  logical                                      :: lsamemoljk
  logical                                      :: lsamemolkl
  logical                                      :: lsg1
  logical                                      :: lslicei
  logical                                      :: lslicej
  logical                                      :: lslicek
  logical                                      :: lslicel
  logical                                      :: lswitchil
  logical                                      :: lswitchjk
  logical                                      :: ltsyme_exact
  logical                                      :: lunique
  real(dp)                                     :: cputime
  real(dp)                                     :: cut
  real(dp)                                     :: cutmax
  real(dp)                                     :: e1d(6)
  real(dp)                                     :: e2d(21)
  real(dp)                                     :: e3d(1)
  real(dp)                                     :: esregion12l
  real(dp)                                     :: esregion12loop
  real(dp)                                     :: eterm
  real(dp)                                     :: eterm6th
  real(dp)                                     :: fpoly(5)
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ocl 
  real(dp)                                     :: ofct 
  real(dp)                                     :: phi0 
  real(dp)                                     :: phi0o
  real(dp)                                     :: r21
  real(dp)                                     :: r212
  real(dp)                                     :: r31
  real(dp)                                     :: r312
  real(dp)                                     :: r32
  real(dp)                                     :: r322
  real(dp)                                     :: r41
  real(dp)                                     :: r412
  real(dp)                                     :: r42
  real(dp)                                     :: r422
  real(dp)                                     :: r43
  real(dp)                                     :: r432
  real(dp)                                     :: rkforloc
  real(dp)                                     :: rko
  real(dp)                                     :: rn
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: rtmp
  real(dp)                                     :: rprod(6,6)
  real(dp)                                     :: t12
  real(dp)                                     :: t13
  real(dp)                                     :: t14
  real(dp)                                     :: t23
  real(dp)                                     :: t24
  real(dp)                                     :: t34
  real(dp)                                     :: temp(6)
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr2max
  real(dp)                                     :: tr3
  real(dp)                                     :: tr4
  real(dp)                                     :: vec(3,3,4)
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x21t
  real(dp)                                     :: y21t
  real(dp)                                     :: z21t
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x32
  real(dp)                                     :: y32
  real(dp)                                     :: z32
  real(dp)                                     :: x32t
  real(dp)                                     :: y32t
  real(dp)                                     :: z32t
  real(dp)                                     :: x42
  real(dp)                                     :: y42
  real(dp)                                     :: z42
  real(dp)                                     :: x43
  real(dp)                                     :: y43
  real(dp)                                     :: z43
  real(dp)                                     :: x43t
  real(dp)                                     :: y43t
  real(dp)                                     :: z43t
  real(dp)                                     :: xc1t
  real(dp)                                     :: yc1t
  real(dp)                                     :: zc1t
  real(dp)                                     :: xc2
  real(dp)                                     :: yc2
  real(dp)                                     :: zc2
  real(dp)                                     :: xc3
  real(dp)                                     :: yc3
  real(dp)                                     :: zc3
  real(dp)                                     :: xc3t
  real(dp)                                     :: yc3t
  real(dp)                                     :: zc3t
  real(dp)                                     :: xc4t
  real(dp)                                     :: yc4t
  real(dp)                                     :: zc4t
  real(dp), dimension(:), allocatable          :: xderv
  real(dp), dimension(:), allocatable          :: yderv
  real(dp), dimension(:), allocatable          :: zderv
  real(dp), dimension(:), allocatable          :: xvec
  real(dp), dimension(:), allocatable          :: yvec
  real(dp), dimension(:), allocatable          :: zvec
!
  data kb/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17,18,5,10,14,17,19,20,6,11,15,18,20,21/
  data n3vec/1,2,3,1,4,5,2,4,6,3,5,6/
!
  time1 = cputime()
  lsg1 = (lstr.and.lgrad1)
  lmolloc = (nmol.gt.0)
!
!  Allocate local memory
!
  allocate(natmiddle(2*nfor),stat=status)
  if (status/=0) call outofmemory('fournos','natmiddle')
  allocate(ntypmiddle(2*nfor),stat=status)
  if (status/=0) call outofmemory('fournos','ntypmiddle')
  allocate(nptrnfornonoop(nfor),stat=status)
  if (status/=0) call outofmemory('fournos','nptrnfornonoop')
  allocate(nuniqueiptr(numat),stat=status)
  if (status/=0) call outofmemory('fournos','nuniqueiptr')
  allocate(nuniquekptr(numat),stat=status)
  if (status/=0) call outofmemory('fournos','nuniquekptr')
  allocate(nuniquelptr(numat),stat=status)
  if (status/=0) call outofmemory('fournos','nuniquelptr')
  allocate(ioptptr(numat),stat=status)
  if (status/=0) call outofmemory('fournos','ioptptr')
  allocate(xderv(numat),stat=status)
  if (status/=0) call outofmemory('fournos','xderv')
  allocate(yderv(numat),stat=status)
  if (status/=0) call outofmemory('fournos','yderv')
  allocate(zderv(numat),stat=status)
  if (status/=0) call outofmemory('fournos','zderv')
  if (ndim.gt.0) then
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('fournos','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('fournos','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('fournos','zvec')
  else
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('fournos','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('fournos','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('fournos','zvec')
  endif
!
!  Initialisation
!
  efor = 0.0_dp
  eoop = 0.0_dp
  esregion12l = 0.0_dp
  esregion12loop = 0.0_dp
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
!  Check how many four-body potentials are of out of plane type and how many aren't
!
  noofp = 0
  nfornonoop = 0
  do n = 1,nfor
    if (loutofplane(n)) then
      noofp = noofp + 1
    else
      nfornonoop = nfornonoop + 1
      nptrnfornonoop(nfornonoop) = n
    endif
  enddo
!
!  Find out if any require molecule information and whether any potential is of bonded type
!
  lallbtyp = .true.
  lanybtyp = .false.
  lanyneedmol = .false.
  do n = 1,nfor
    if (.not.loutofplane(n)) then
      lbtyp = (mmfexc(n).eq.1)
      lintra_only = (lfintra(n).and..not.lfinter(n))
      linter_only = (lfinter(n).and..not.lfintra(n))
      lneedmol = (lintra_only.or.linter_only.or.lbtyp)
      if (lneedmol) lanyneedmol = .true.
      if (lbtyp) lanybtyp = .true.
      if (.not.lbtyp) lallbtyp = .false.
    endif
  enddo
!
!  Build a list of middle atom species types for potentials
!
  nmiddle = 0
  do n = 1,nfor
    if (.not.loutofplane(n)) then
      if (.not.lmatchany(nfspec2(n),nfptyp2(n),nmiddle,natmiddle,ntypmiddle)) then
        nmiddle = nmiddle + 1
        natmiddle(nmiddle) = nfspec2(n)
        ntypmiddle(nmiddle) = nfptyp2(n)
      endif
      if (.not.lmatchany(nfspec3(n),nfptyp3(n),nmiddle,natmiddle,ntypmiddle)) then
        nmiddle = nmiddle + 1
        natmiddle(nmiddle) = nfspec3(n)
        ntypmiddle(nmiddle) = nfptyp3(n)
      endif
    endif
  enddo
!
!  Find maximum cutoff distance for ends and middle atoms
!
  cutmax = 0.0_dp
  tr2max = 0.0_dp
  do n = 1,nfor
    if (.not.loutofplane(n)) then
      cut = for1(n) + for2(n) + for3(n)
      if (for4(n).gt.0.0_dp) cut = for4(n)
      cutmax = max(cut,cutmax)
      tr2 = for2(n)**2
      tr2max = max(tr2,tr2max)
    endif
  enddo
!
!  Create lattice vectors
!
  if (ndim.gt.0) then
    call rtlist(nvector,cutmax,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
    if (nvector.gt.maxvector) then
!
!  Too many vectors
!
      deallocate(zvec,stat=status)
      if (status/=0) call deallocate_error('fournos','zvec')
      deallocate(yvec,stat=status)
      if (status/=0) call deallocate_error('fournos','yvec')
      deallocate(xvec,stat=status)
      if (status/=0) call deallocate_error('fournos','xvec')
      maxvector = nint(1.1*nvector)
      allocate(xvec(maxvector),stat=status)
      if (status/=0) call outofmemory('fournos','xvec')
      allocate(yvec(maxvector),stat=status)
      if (status/=0) call outofmemory('fournos','yvec')
      allocate(zvec(maxvector),stat=status)
      if (status/=0) call outofmemory('fournos','zvec')
      call rtlist(nvector,cutmax,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
    endif
  else
    nvector = 1
    nmid = 1
    xvec(1) = 0.0_dp
    yvec(1) = 0.0_dp
    zvec(1) = 0.0_dp
  endif
!****************************************************************************
!  If there are no non out of plane potentials then we can skip everything  *
!****************************************************************************
  if (nfornonoop.eq.0) goto 5
!
!  Openning banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Four : Atom No. 1  Atom No. 2  Atom No. 3  Atom No. 4    Torsion energy (eV)  '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!********************************
!  Loop over middle site 2 / j  *
!********************************
  jx = - 2
  jy = - 1
  jz =   0
  ljloop:  do j = 1,numat
    nj = nat(j)
    ntypj = nftype(j)
    lopj = (lopf(nrelat(j)).or..not.lfreeze)
    if (lopj) then
      jx = jx + 3
      jy = jy + 3
      jz = jz + 3
    endif
!
!  Check whether species may be valid
!
    if (.not.lmatchany(nj,ntypj,nmiddle,natmiddle,ntypmiddle)) cycle ljloop
!
!  Set properties for atom j
!
    nregionj = nregionno(nsft+nrelat(j))
    nregiontypj = nregiontype(nregionj,ncf)
    ocj = occuf(j)
    lslicej = lsliceatom(nsft + nrelat(j))
!
!  Set loop range for k
!
    if (lallbtyp) then
      lkbond = .true.
      if (nbonds(j).gt.0) then
        nuniquek = 1
        nuniquekptr(1) = nbonded(1,j)
        do kloop = 2,nbonds(j)
          lunique = .true.
          do lu = 1,nuniquek
            if (nbonded(kloop,j).eq.nuniquekptr(lu)) lunique = .false.
          enddo
          if (lunique) then
            nuniquek = nuniquek + 1
            nuniquekptr(nuniquek) = nbonded(kloop,j)
          endif
        enddo
        kloop = nuniquek
      else
        kloop = 0
      endif
      kloopmin = 1
    else
      lkbond = .false.
      kloopmin = j
      kloop = numat
    endif
!
!  Skip if kloop is zero
!
    if (kloop.eq.0) cycle ljloop
!
!  Molecule handling
!
    if (lmolloc.and.lanyneedmol) then
      nmj = natmol(j)
      if (ndim.gt.0) then
        indmj = nmolind(j)
        call mindtoijk(indmj,ixj,iyj,izj)
      endif
    endif
    xc2 = xclat(j)
    yc2 = yclat(j)
    zc2 = zclat(j)
!***************************************
!  Loop over second middle site 3 / k  *
!***************************************
    lkloop: do lk = kloopmin,kloop
      if (lkbond) then
        k = nuniquekptr(lk)
      else
        k = lk
      endif
!
!  Only do upper triangular set of atoms
!
      if (k.lt.j) cycle lkloop
!
      nk = nat(k)
      ntypk = nftype(k)
!
!  Check whether species may be valid
!
      if (.not.lmatchany(nk,ntypk,nmiddle,natmiddle,ntypmiddle)) cycle lkloop
      if (.not.lmatchpair(nj,ntypj,nk,ntypk,nfor,nfspec2,nfptyp2,nfspec3,nfptyp3)) cycle lkloop
!
!  Set properties for atom k
!
      lopk = (lopf(nrelat(k)).or..not.lfreeze)
      nregionk = nregionno(nsft+nrelat(k))
      nregiontypk = nregiontype(nregionk,ncf)
      ock = occuf(k)
      lslicek = lsliceatom(nsft + nrelat(k))
      if (lopk) then
        kx = 3*(ioptptr(k) - 1) + 1
        ky = kx + 1
        kz = kx + 2
      endif
!
      if (lmolloc.and.lanyneedmol) then
!
!  Molecule handling
!
        nmk = natmol(k)
        if (ndim.gt.0) then
          indmk = nmolind(k)
          call mindtoijk(indmk,ixk,iyk,izk)
          ixk = ixk - ixj
          iyk = iyk - iyj
          izk = izk - izj
        endif
        lmolokjk = (nmj.eq.nmk.and.nmj.gt.0)
      else
        lmolokjk = .false.
      endif
      xc3t = xclat(k)
      yc3t = yclat(k)
      zc3t = zclat(k)
      x32t = xc3t - xc2
      y32t = yc3t - yc2
      z32t = zc3t - zc2
!
!  Check r32 is OK
!  Loop over cell vectors
!
      do 120 jj = 1,nvector
        r322 = (xvec(jj)+x32t)**2 + (yvec(jj)+y32t)**2 + (zvec(jj)+z32t)**2
        if (r322.lt.1d-12) goto 120
        if (k.eq.j.and.jj.eq.nmid) goto 120
!
!  Molecule checking
!
        lbondedjk = .false.
        if (lmolokjk) then
          if (ndim.eq.0) then
            if (lanybtyp) then
              call bonded(lbondedjk,l2bondsjk,nbtypejk,nbtypejk2,j,k,0_i4,0_i4,0_i4)
            endif
          else
            call lintoijk(jxx,jyy,jzz,jj,imax,jmax,kmax)
            if (lanybtyp) then
              call bonded(lbondedjk,l2bondsjk,nbtypejk,nbtypejk2,j,k,jxx,jyy,jzz)
              lsamemoljk = (lbondedjk.or.l2bondsjk)
            else
              lsamemoljk = .false.
            endif
            if (.not.lsamemoljk) then
              call samemol(lsamemoljk,nmj,jxx,jyy,jzz,ixk,iyk,izk)
            endif
          endif
        endif
!
!  Distance checking
!
        if (r322.gt.tr2max.and.(.not.lanybtyp.or..not.lbondedjk)) goto 120
        xc3 = xc3t + xvec(jj)
        yc3 = yc3t + yvec(jj)
        zc3 = zc3t + zvec(jj)
        x32 = x32t + xvec(jj)
        y32 = y32t + yvec(jj)
        z32 = z32t + zvec(jj)
!
!  Set counter for number of valid i/l end atom combinations
!
        lrooted32 = .false.
        niltor = 0
!***********************************
!  Loop over four-body potentials  *
!***********************************
        pots: do nn = 1,nfornonoop
          n = nptrnfornonoop(nn)
          nfortype = nforty(n)
          nt1 = nfspec1(n)
          nt2 = nfspec2(n)
          nt3 = nfspec3(n)
          nt4 = nfspec4(n)
          ntyp1 = nfptyp1(n)
          ntyp2 = nfptyp2(n)
          ntyp3 = nfptyp3(n)
          ntyp4 = nfptyp4(n)
          tr1 = for1(n)**2
          tr2 = for2(n)**2
          tr3 = for3(n)**2
          tr4 = for4(n)**2
          ltsyme_exact = lexactmatch(nt1,ntyp1,nt4,ntyp4)
          lbtyp = (mmfexc(n).eq.1)
          lintra_only = (lfintra(n).and..not.lfinter(n))
          linter_only = (lfinter(n).and..not.lfintra(n))
          lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Reset lmolok to initial state for j-k pair for each potential
!
          lmolok = lmolokjk
!
!  QM/MM handling : j & k are both QM atoms and potential is of bonded type => exclude
!
          if (QMMMmode(ncf).gt.0) then
            if (nregiontypj.eq.1.and.nregiontypk.eq.1.and.lbtyp) cycle pots
          endif
!************************************
!  Validate potential for j-k pair  *
!************************************
!
!  Check whether j and k are allowed for n
!
          ljmatch2 = lmatch(nj,ntypj,nt2,ntyp2,.true.)
          ljmatch3 = lmatch(nj,ntypj,nt3,ntyp3,.true.)
          lkmatch2 = lmatch(nk,ntypk,nt2,ntyp2,.true.)
          lkmatch3 = lmatch(nk,ntypk,nt3,ntyp3,.true.)
!
!  Check whether j-k or k-j orders are OK for 2-3
!
          ljkmatch = (ljmatch2.and.lkmatch3)
          lkjmatch = (ljmatch3.and.lkmatch2)
!
!  If no pair of matches can be found then cycle
!
          if (.not.ljkmatch.and..not.lkjmatch) cycle pots
          lswitchil = .false.
          if (.not.ljkmatch) then
!
!  If j-k doesn't match, but k-j does then swap terms
!
            ntmp = nt2
            nt2 = nt3
            nt3 = ntmp
            ntmp = ntyp2
            ntyp2 = ntyp3
            ntyp3 = ntmp
            rtmp = tr1
            tr1 = tr3
            tr3 = rtmp
            if (.not.ltsyme_exact) then
              ntmp = nt1
              nt1 = nt4
              nt4 = ntmp
              ntmp = ntyp1
              ntyp1 = ntyp4
              ntyp4 = ntmp
            endif
            lswitchil = .true.
          endif
!
!  Set flag indicating whether middle atoms could be matched either way round
!
          lmeither = (ljkmatch.and.lkjmatch)
!
!  Distance checking for j-k
!
          if (r322.gt.tr2.and.(.not.lbtyp.or..not.lbondedjk)) cycle pots
!       
!  Check for intra and but not in same molecule
!       
          if (lintra_only.and..not.lmolok) cycle pots
          if (lbtyp.and..not.lmolok) cycle pots
!                 
!  Molecule checking
!
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle pots
              if (lbtyp) then
                if (.not.lbondedjk) cycle pots
!               
!  Check central bond type for correct order
!
                if (n4botype(1,n).gt.0) then
                  if (n4botype(1,n).ne.nbtypejk) cycle pots
                endif
                if (n4botype(2,n).ne.nbtypejk2) cycle pots
              endif
            else
              if (lbtyp) then
                if (.not.lbondedjk) cycle pots
!               
!  Check central bond type for correct order
!
                if (n4botype(1,n).gt.0) then
                  if (n4botype(1,n).ne.nbtypejk) cycle pots
                endif
                if (n4botype(2,n).ne.nbtypejk2) cycle pots
              endif
              if (lintra_only.and..not.lsamemoljk) cycle pots
              if (linter_only.and.lsamemoljk) cycle pots
            endif
          endif
!
!  Set loop range for i
!
          if (lbtyp) then
            libond = .true.
            if (nbonds(j).gt.0) then
              nuniquei = 1
              nuniqueiptr(1) = nbonded(1,j)
              do iloop = 2,nbonds(j)
                lunique = .true.
                do lu = 1,nuniquei
                  if (nbonded(iloop,j).eq.nuniqueiptr(lu)) lunique = .false.
                enddo
                if (lunique) then
                  nuniquei = nuniquei + 1
                  nuniqueiptr(nuniquei) = nbonded(iloop,j)
                endif
              enddo
              iloop = nuniquei
            else
              iloop = 0
            endif
            iloopmin = 1
          else
            libond = .false.
            iloopmin = 1
            iloop = numat
          endif
!
!  Skip if iloop is zero
!
          if (iloop.eq.0) cycle pots
!*****************************
!  Loop over end site 1 / i  *
!*****************************
          liloop: do li = iloopmin,iloop
            if (libond) then
              i = nuniqueiptr(li)
            else
              i = li
            endif
            ni = nat(i)
            ntypi = nftype(i)
!
!  Check whether i matches either of types 1 and 4
!
            limatch1 = lmatch(ni,ntypi,nt1,ntyp1,.true.)
            limatch4 = lmatch(ni,ntypi,nt4,ntyp4,.true.)
!
!  Is i allowed for type 1, or type 4 if the middle atoms can be switched?
!
            liok = (limatch1.or.(limatch4.and.lmeither))
            if (.not.liok) cycle liloop
!
!  Set properties for atom i
!
            nregioni = nregionno(nsft+nrelat(i))
            nregiontypi = nregiontype(nregioni,ncf)
            oci = occuf(i)
            lopi = (lopf(nrelat(i)).or..not.lfreeze)
            lslicei = lsliceatom(nsft + nrelat(i))
            if (lopi) then
              ix = 3*(ioptptr(i) - 1) + 1
            endif
!
            lswitchjk = .false.
            if (.not.limatch1.and.(limatch4.and.lmeither)) then
!
!  Switch round order of torsional atoms
!
              ntmp = nt1
              nt1 = nt4
              nt4 = ntmp
              ntmp = ntyp1
              ntyp1 = ntyp4
              ntyp4 = ntmp
              rtmp = tr1
              tr1 = tr3
              tr3 = rtmp
              lswitchjk = .true.
            endif
!
!  Molecule handling
!
            if (lmolloc.and.lneedmol) then
              nmi = natmol(i)
              if (ndim.gt.0) then
                indmi = nmolind(i)
                call mindtoijk(indmi,ixi,iyi,izi)
                ixi = ixi - ixj
                iyi = iyi - iyj
                izi = izi - izj
              endif
              lmolok = (nmj.eq.nmi.and.nmj.gt.0)
            else
              lmolok = .false.
            endif
!
!  Check for intra and but not in same molecule
!
            if (lintra_only.and..not.lmolok) cycle liloop
            if (lbtyp.and..not.lmolok) cycle liloop
!
            xc1t = xclat(i)
            yc1t = yclat(i)
            zc1t = zclat(i)
            x21t = xc2 - xc1t
            y21t = yc2 - yc1t
            z21t = zc2 - zc1t
!
!  Check r21 is OK
!  Loop over cell vectors
!
            iiloop: do ii = 1,nvector
              r212 = (-xvec(ii) + x21t)**2 + (-yvec(ii) + y21t)**2 + (-zvec(ii) + z21t)**2
              if (r212.lt.1d-12) cycle iiloop
!
!  Prevent atoms i and k being the same atom
!
              if (k.eq.i.and.ii.eq.jj) cycle iiloop
!
!  Molecule checking
!
              lbondedij = .false.
              if (lmolok) then
                if (ndim.eq.0) then
                  if (linter_only) cycle iiloop
                  if (lbtyp) then
                    call bonded(lbondedij,l2bondsij,nbtypeji,nbtypeji2,j,i,0_i4,0_i4,0_i4)
                    if (.not.lbondedij) cycle iiloop
                  endif
                else
                  call lintoijk(ixx,iyy,izz,ii,imax,jmax,kmax)
                  if (lbtyp) then
                    call bonded(lbondedij,l2bondsij,nbtypeji,nbtypeji2,j,i,ixx,iyy,izz)
                    if (.not.lbondedij) cycle iiloop
                    lsamemolij = (lbondedij.or.l2bondsij)
                  else
                    lsamemolij = .false.
                  endif
                  if (.not.lsamemolij) then
                    call samemol(lsamemolij,nmj,ixx,iyy,izz,ixi,iyi,izi)
                  endif
                  if (lintra_only.and..not.lsamemolij) cycle iiloop
                  if (linter_only.and.lsamemolij) cycle iiloop
                endif
              endif
!
!  Distance checking
!
              if (r212.gt.tr1.and.(.not.lbtyp.or..not.lbondedij)) cycle iiloop
              x21 = x21t - xvec(ii)
              y21 = y21t - yvec(ii)
              z21 = z21t - zvec(ii)
!
!  Check r31 is OK
!
              x31 = x32 + x21
              y31 = y32 + y21
              z31 = z32 + z21
              r312 = x31*x31 + y31*y31 + z31*z31
              if (r312.lt.1.0d-12) cycle iiloop
              lrooted1 = .false.
!
!  Set loop range for l
!
              if (lbtyp) then
                llbond = .true.
                if (nbonds(k).gt.0) then
                  nuniquel = 1
                  nuniquelptr(1) = nbonded(1,k)
                  do lloop = 2,nbonds(k)
                    lunique = .true.
                    do lu = 1,nuniquel
                      if (nbonded(lloop,k).eq.nuniquelptr(lu)) lunique = .false.
                    enddo
                    if (lunique) then
                      nuniquel = nuniquel + 1
                      nuniquelptr(nuniquel) = nbonded(lloop,k)
                    endif
                  enddo
                  lloop = nuniquel
                else
                  lloop = 0
                endif
                lloopmin = 1
              else
                llbond = .false.
                lloopmin = 1
                lloop = numat
              endif
!
!  Skip if lloop is zero
!
              if (lloop.eq.0) cycle iiloop
!**********************************
!  Loop over last end site 4 / l  *
!**********************************
              l4loop: do l4 = lloopmin,lloop
                if (llbond) then
                  l = nuniquelptr(l4)
                else
                  l = l4
                endif
                nl = nat(l)
                ntypl = nftype(l)
!
!  Check l is allowed for n
!             
                if (.not.lmatch(nl,ntypl,nt4,ntyp4,.true.)) cycle l4loop
!
!  Set properties for atom l
!
                nregionl = nregionno(nsft+nrelat(l))
                nregiontypl = nregiontype(nregionl,ncf)
                ocl = occuf(l)
                lopl = (lopf(nrelat(l)).or..not.lfreeze)
                if (lopl) then
                  lx = 3*(ioptptr(l) - 1) + 1
                endif
!
!  If lfreeze=.true. and no atoms have any variables
!  then skip this four body term
!
                if (.not.lopi.and..not.lopj.and..not.lopk.and..not.lopl) cycle l4loop
!
                if (lmolloc.and.lanyneedmol) then
!
!  Molecule handling
!
                  nml = natmol(l)
                  if (ndim.gt.0) then
                    indml = nmolind(l)
                    call mindtoijk(indml,ixl,iyl,izl)
                    ixl = ixl - ixj
                    iyl = iyl - iyj
                    izl = izl - izj
                  endif
                  lmolok = (nmj.eq.nml.and.nmj.gt.0)
                else
                  lmolok = .false.
                endif
!
!  Check for intra and but not in same molecule
!
                if (lintra_only.and..not.lmolok) cycle l4loop
                if (lbtyp.and..not.lmolok) cycle l4loop
!
!  Set region 2 quartet flag
!
                lreg12    = .false.
                lreg2qtet = .false.
                if (lseok.and.nregions(ncf).gt.1) then
                  lreg2qtet = (nregioni.gt.1.and.nregionj.gt.1.and.nregionk.gt.1.and.nregionl.gt.1)
                  if (.not.lreg2qtet) lreg12 = (nregioni.gt.1.or.nregionj.gt.1.or.nregionk.gt.1.or.nregionl.gt.1)
                endif
                lslicel = lsliceatom(nsft + nrelat(l))
                lattach = .true.
                if (lslicei.and.lslicej.and.lslicek.and.lslicel) lattach = .false.  
                if (.not.lslicei.and..not.lslicej.and..not.lslicek.and..not.lslicel) lattach = .false.
!
!  QM/MM handling : i, j, k & l are all QM atoms => exclude
!
                if (QMMMmode(ncf).gt.0) then
                  if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1.and.nregiontypl.eq.1) cycle l4loop
                endif
!
                xc4t = xclat(l)
                yc4t = yclat(l)
                zc4t = zclat(l)
                x43t = xc4t - xc3
                y43t = yc4t - yc3
                z43t = zc4t - zc3
!
!  Check r43 is OK
!  Loop over cell vectors
!
                llloop: do ll = 1,nvector
                  r432 = (xvec(ll)+x43t)**2 + (yvec(ll)+y43t)**2 + (zvec(ll)+z43t)**2
                  if (r432.lt.1d-12) cycle llloop
!
!  Molecule checking
!
                  lbondedkl = .false.
                  if (lmolok) then
                    if (ndim.eq.0) then
                      if (linter_only) cycle llloop
                      if (lbtyp) then
                        call bonded(lbondedkl,l2bondskl,nbtypekl,nbtypekl2,k,l,0_i4,0_i4,0_i4)
                        if (.not.lbondedkl) cycle llloop
                      endif
                    else
                      call lintoijk(kxx,kyy,kzz,ll,imax,jmax,kmax)
                      if (lbtyp) then
                        call bonded(lbondedkl,l2bondskl,nbtypekl,nbtypekl2,k,l,kxx-jxx,kyy-jyy,kzz-jzz)
                        if (.not.lbondedkl) cycle llloop
                        lsamemolkl = (lbondedkl.or.l2bondskl)
                      else
                        lsamemolkl = .false.
                      endif
                      if (.not.lsamemolkl) then
                        call samemol(lsamemolkl,nmj,kxx,kyy,kzz,ixl,iyl,izl)
                      endif
                      if (lintra_only.and..not.lsamemolkl) cycle llloop
                      if (linter_only.and.lsamemolkl) cycle llloop
                    endif
                  endif
!
!  Distance checking
!
                  if (r432.gt.tr3.and.(.not.lbtyp.or..not.lbondedkl)) cycle llloop
                  x43 = x43t + xvec(ll)
                  y43 = y43t + yvec(ll)
                  z43 = z43t + zvec(ll)
!
!  Check r41 is OK
!
                  x41 = x43 + x32 + x21
                  y41 = y43 + y32 + y21
                  z41 = z43 + z32 + z21
                  r412 = x41*x41 + y41*y41 + z41*z41
                  if (r412.gt.tr4.and.tr4.gt.0.0_dp.and..not.lbtyp) cycle llloop
                  if (r412.lt.1.0d-12) cycle llloop
!
!  Check r42 is OK
!
                  x42 = x32 + x43
                  y42 = y32 + y43
                  z42 = z32 + z43
                  r422 = x42*x42 + y42*y42 + z42*z42
                  if (r422.lt.1.0d-12) cycle llloop
!*********************************
!  Valid four-body term located  *
!*********************************
!
!  Finish calculating distances
!
                  if (.not.lrooted32) then
                    r32 = sqrt(r322)
                    lrooted32 = .true.
                  endif
                  if (.not.lrooted1) then
                    r21 = sqrt(r212)
                    r31 = sqrt(r312)
                    lrooted1 = .true.
                  endif
                  r41 = sqrt(r412)
                  r42 = sqrt(r422)
                  r43 = sqrt(r432)
!
!  Store information into iltor arrays
!
                  niltor = niltor + 1
                  if (niltor.gt.maxiltor) then
                    maxiltor = niltor + 3
                    call changemaxiltor
                  endif
!
                  nfortor(niltor) = n
                  liltorswitch(niltor) = lswitchil
                  ljktorswitch(niltor) = lswitchjk
!
                  iltor(1,niltor) = i
                  iltor(2,niltor) = l
                  ilxtor(1,niltor) = ix
                  ilxtor(2,niltor) = lx
!
                  riltor(1,niltor) = r21
                  riltor(2,niltor) = r31
                  riltor(3,niltor) = r41
                  riltor(4,niltor) = r42
                  riltor(5,niltor) = r43
!
                  xiltor(1,niltor) = x21
                  yiltor(1,niltor) = y21
                  ziltor(1,niltor) = z21
                  xiltor(2,niltor) = x31
                  yiltor(2,niltor) = y31
                  ziltor(2,niltor) = z31
                  xiltor(3,niltor) = x41
                  yiltor(3,niltor) = y41
                  ziltor(3,niltor) = z41
                  xiltor(4,niltor) = x42
                  yiltor(4,niltor) = y42
                  ziltor(4,niltor) = z42
                  xiltor(5,niltor) = x43
                  yiltor(5,niltor) = y43
                  ziltor(5,niltor) = z43
!
                  oiltor(niltor) = oci*ocj*ock*ocl
                  lopiltor(1,niltor) = lopi
                  lopiltor(2,niltor) = lopl
                  lsurfiltor(1,niltor) = lreg12
                  lsurfiltor(2,niltor) = lreg2qtet
                  lsurfiltor(3,niltor) = lattach
!
!  End of inner loops over atoms and cell vectors
!
                enddo llloop
              enddo l4loop
            enddo iiloop
          enddo liloop
!
!  End loop over potentials
!
        enddo pots
!*******************************
!  Loop over i/l combinations  *
!*******************************
        do nil = 1,niltor
!
!  Return values to local variables
!
          n = nfortor(nil)
          lswitchil = liltorswitch(nil)
          lswitchjk = ljktorswitch(nil)
!
          i = iltor(1,nil)
          l = iltor(2,nil)
!
          ix = ilxtor(1,nil)
          iy = ix + 1
          iz = ix + 2
          lx = ilxtor(2,nil)
          ly = lx + 1
          lz = lx + 2
!
          r21 = riltor(1,nil)
          r31 = riltor(2,nil)
          r41 = riltor(3,nil)
          r42 = riltor(4,nil)
          r43 = riltor(5,nil)
!
          x21 = xiltor(1,nil)
          y21 = yiltor(1,nil)
          z21 = ziltor(1,nil)
          x31 = xiltor(2,nil)
          y31 = yiltor(2,nil)
          z31 = ziltor(2,nil)
          x41 = xiltor(3,nil)
          y41 = yiltor(3,nil)
          z41 = ziltor(3,nil)
          x42 = xiltor(4,nil)
          y42 = yiltor(4,nil)
          z42 = ziltor(4,nil)
          x43 = xiltor(5,nil)
          y43 = yiltor(5,nil)
          z43 = ziltor(5,nil)
!
          ofct = oiltor(nil)
          lopi = lopiltor(1,nil)
          lopl = lopiltor(2,nil)
          lreg12 = lsurfiltor(1,nil) 
          lreg2qtet = lsurfiltor(2,nil) 
          lattach = lsurfiltor(3,nil) 
!
!  Set terms for potentials
!
          rkforloc = fork(n)
!
!  If this is Dreiding mode then divide force constant by number of torsions
!
          if (lfdreiding(n)) then
            rkforloc = rkforloc/dble(niltor)
          endif
          npha = 0
          nfortype = nforty(n)
          if (nfortype.eq.1) then
            npha = npfor(n)
            if (npha.gt.0) then
              isgn = 1
            else
              isgn = - 1
            endif
            npha = abs(npha)
            phi0 = forpoly(1,n)*degtorad
          elseif (nfortype.eq.4.or.nfortype.eq.6.or.nfortype.eq.7) then
            npha = npfor(n)
            if (npha.gt.0) then
              isgn = 1
            else
              isgn = - 1
            endif
            npha = abs(npha)
            if (nfortype.eq.6) then
              phi0 = forpoly(1,n)*degtorad
            else
              phi0 = forpoly(1,n)
            endif
            if (nfortype.eq.6.or.nfortype.eq.7) then
              fpoly(2:4) = forpoly(2:4,n)
            endif
          elseif (nfortype.eq.8.or.nfortype.eq.9) then
            npha = npfor(n)
            if (npha.gt.0) then
              isgn = 1
            else
              isgn = - 1
            endif
            npha = abs(npha)
            if (nfortype.eq.8) then
              phi0 = forpoly(1,n)*degtorad
            else
              phi0 = forpoly(1,n)
            endif
            fpoly(2) = forpoly(2,n)
            fpoly(3) = for1(n)
            fpoly(4) = for2(n)
            fpoly(5) = for3(n)
          elseif (nfortype.eq.2) then
            npha = npfor(n)
          elseif (nfortype.eq.5) then
            phi0 = forpoly(1,n)*degtorad
          elseif (nfortype.eq.10.or.nfortype.eq.17) then
            fpoly(1) = forpoly(1,n)*degtorad
            fpoly(2) = forpoly(2,n)*degtorad
          elseif (nfortype.eq.13) then
            npha = abs(npfor(n))
            phi0 = forpoly(1,n)*degtorad
          endif
          rn = dble(npha)
!
!  Switch terms if necessary
!
          if (lswitchil) then
            if (nfortype.eq.8.or.nfortype.eq.9) then
              rtmp = fpoly(3)
              fpoly(3) = fpoly(5)
              fpoly(5) = rtmp
            elseif (nfortype.eq.6.or.nfortype.eq.7) then
              rtmp = fpoly(2)
              fpoly(2) = fpoly(4)
              fpoly(4) = rtmp
            elseif (nfortype.eq.10.or.nfortype.eq.17) then
              rtmp = fpoly(2)
              fpoly(2) = fpoly(1)
              fpoly(1) = rtmp
            endif
          endif
          if (lswitchjk) then
            if (nfortype.eq.8.or.nfortype.eq.9) then
              rtmp = fpoly(3)
              fpoly(3) = fpoly(5)
              fpoly(5) = rtmp
            elseif (nfortype.eq.6.or.nfortype.eq.7) then
              rtmp = fpoly(2)
              fpoly(2) = fpoly(4)
              fpoly(4) = rtmp
            elseif (nfortype.eq.10.or.nfortype.eq.17) then
              rtmp = fpoly(2)
              fpoly(2) = fpoly(1)
              fpoly(1) = rtmp
            endif
          endif
!
!  Scaling of terms
!
          rko = rkforloc*ofct
          phi0o = phi0
          if (nfortype.eq.2) then
            do kk = 1,npha
              fpoly(kk) = forpoly(kk,n)*ofct
            enddo
          elseif (nfortype.eq.4.or.nfortype.eq.7.or.nfortype.eq.9) then
            phi0o = phi0*ofct
          endif
!
!  Call subroutine to calculate energy and derivatives
!
          call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d, &
                        rko,rn,phi0o,isgn,fpoly,lgrad1,lgrad2,.false.)
          if (lreg2qtet) then
            esregion2 = esregion2 + eterm
          elseif (lreg12) then
            esregion12l = esregion12l + eterm
          else
            efor = efor + eterm
          endif
          if (lattach) eattach = eattach + eterm
!
          eterm6th = eterm/6.0_dp
          eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eterm6th
          eregion2region(nregionk,nregioni) = eregion2region(nregionk,nregioni) + eterm6th
          eregion2region(nregionl,nregioni) = eregion2region(nregionl,nregioni) + eterm6th
          eregion2region(nregionk,nregionj) = eregion2region(nregionk,nregionj) + eterm6th
          eregion2region(nregionl,nregionj) = eregion2region(nregionl,nregionj) + eterm6th
          eregion2region(nregionl,nregionk) = eregion2region(nregionl,nregionk) + eterm6th
!
!  Output energy contribution
!
          if (lPrintFour) then
            write(ioout,'(4x,4i12,1x,f22.10)') i,j,k,l,eterm
          endif
!**************************
!  Torsional derivatives  *
!**************************
          if (lgrad1) then
!
!  Set up strain products
!
            if (lsg1) then
              call fourstrterms(ndim,rprod,x21,y21,z21,x31,y31,z31, &
                               x41,y41,z41,x32,y32,z32,x42,y42,z42,x43,y43,z43)
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
            n4x = lx
            n4y = ly
            n4z = lz
            if (lopi.and.lopj.and.lopk.and.lopl) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
            elseif (lopi.and.lopj.and.lopk) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x1 = ix
              n4x2 = jx
              n4x3 = kx
            elseif (lopi.and.lopj.and.lopl) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x1 = ix
              n3x2 = jx
              n3x4 = lx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
            elseif (lopi.and.lopk.and.lopl) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = ix
              n2x3 = kx
              n2x4 = lx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
            elseif (lopj.and.lopk.and.lopl) then
              n1x2 = jx
              n1x3 = kx
              n1x4 = lx
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
            elseif (lopi.and.lopj) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x1 = ix
              n3x2 = jx
              n4x1 = ix
              n4x2 = jx
            elseif (lopi.and.lopk) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = ix
              n2x3 = kx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x1 = ix
              n4x3 = kx
            elseif (lopi.and.lopl) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = ix
              n2x4 = lx
              n3x1 = ix
              n3x4 = lx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
            elseif (lopj.and.lopk) then
              n1x2 = jx
              n1x3 = kx
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x2 = jx
              n4x3 = kx
            elseif (lopj.and.lopl) then
              n1x2 = jx
              n1x4 = lx
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x2 = jx
              n3x4 = lx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
            elseif (lopk.and.lopl) then
              n1x3 = kx
              n1x4 = lx
              n2x3 = kx
              n2x4 = lx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
            elseif (lopi) then
              n1x2 = ix
              n1x3 = ix
              n1x4 = ix
              n2x1 = ix
              n3x1 = ix
              n4x1 = ix
            elseif (lopj) then
              n1x2 = jx
              n2x1 = jx
              n2x3 = jx
              n2x4 = jx
              n3x2 = jx
              n4x2 = jx
            elseif (lopk) then
              n1x3 = kx
              n2x3 = kx
              n3x1 = kx
              n3x2 = kx
              n3x4 = kx
              n4x3 = kx
            elseif (lopl) then
              n1x4 = lx
              n2x4 = lx
              n3x4 = lx
              n4x1 = lx
              n4x2 = lx
              n4x3 = lx
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
              rstrdloc(kl) = rstrdloc(kl) + e1d(1)*rprod(kl,1)
              rstrdloc(kl) = rstrdloc(kl) + e1d(2)*rprod(kl,2)
              rstrdloc(kl) = rstrdloc(kl) + e1d(3)*rprod(kl,3)
              rstrdloc(kl) = rstrdloc(kl) + e1d(4)*rprod(kl,4)
              rstrdloc(kl) = rstrdloc(kl) + e1d(5)*rprod(kl,5)
              rstrdloc(kl) = rstrdloc(kl) + e1d(6)*rprod(kl,6)
              rstrd(kl) = rstrd(kl) + rstrdloc(kl)
            enddo
            if (latomicstress) then
              do kl = 1,nstrains
                atomicstress(kl,i) = atomicstress(kl,i) + 0.25_dp*rstrdloc(kl)
                atomicstress(kl,j) = atomicstress(kl,j) + 0.25_dp*rstrdloc(kl)
                atomicstress(kl,k) = atomicstress(kl,k) + 0.25_dp*rstrdloc(kl)
                atomicstress(kl,l) = atomicstress(kl,l) + 0.25_dp*rstrdloc(kl)
              enddo
            endif
!
!  Second strain derivatives
!
!  Strain only
!
            if (lgrad2.and.lstr) then
              do kk = 1,nstrains
                do kl = 1,6
                  temp(kl) = e2d(kb(1,kl))*rprod(kk,1)
                  temp(kl) = temp(kl) + e2d(kb(2,kl))*rprod(kk,2)
                  temp(kl) = temp(kl) + e2d(kb(3,kl))*rprod(kk,3)
                  temp(kl) = temp(kl) + e2d(kb(4,kl))*rprod(kk,4)
                  temp(kl) = temp(kl) + e2d(kb(5,kl))*rprod(kk,5)
                  temp(kl) = temp(kl) + e2d(kb(6,kl))*rprod(kk,6)
                enddo
!
!  Strain-strain second derivatives
!
                do kl = 1,nstrains
                  sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,1)*temp(1)
                  sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,2)*temp(2)
                  sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,3)*temp(3)
                  sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,4)*temp(4)
                  sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,5)*temp(5)
                  sderv2(kl,kk) = sderv2(kl,kk) + rprod(kl,6)*temp(6)
                enddo
              enddo
!
!  Mixed derivatives
!
              if (lopi) then
                do kk = 1,nstrains
                  derv3(n1x,kk) = derv3(n1x,kk) - x21*temp(1) - x31*temp(2) - x41*temp(3)
                  derv3(n1y,kk) = derv3(n1y,kk) - y21*temp(1) - y31*temp(2) - y41*temp(3)
                  derv3(n1z,kk) = derv3(n1z,kk) - z21*temp(1) - z31*temp(2) - z41*temp(3)
                enddo
              endif
              if (lopj) then
                do kk = 1,nstrains
                  derv3(n2x,kk) = derv3(n2x,kk) - x32*temp(4) + x21*temp(1) - x42*temp(5)
                  derv3(n2y,kk) = derv3(n2y,kk) - y32*temp(4) + y21*temp(1) - y42*temp(5)
                  derv3(n2z,kk) = derv3(n2z,kk) - z32*temp(4) + z21*temp(1) - z42*temp(5)
                enddo
              endif
              if (lopk) then
                do kk = 1,nstrains
                  derv3(n3x,kk) = derv3(n3x,kk) + x32*temp(4) - x43*temp(6) + x31*temp(2)
                  derv3(n3y,kk) = derv3(n3y,kk) + y32*temp(4) - y43*temp(6) + y31*temp(2)
                  derv3(n3z,kk) = derv3(n3z,kk) + z32*temp(4) - z43*temp(6) + z31*temp(2)
                enddo
              endif
              if (lopl) then
                do kk = 1,nstrains
                  derv3(n4x,kk) = derv3(n4x,kk) + x43*temp(6) + x42*temp(5) + x41*temp(3)
                  derv3(n4y,kk) = derv3(n4y,kk) + y43*temp(6) + y42*temp(5) + y41*temp(3)
                  derv3(n4z,kk) = derv3(n4z,kk) + z43*temp(6) + z42*temp(5) + z41*temp(3)
                enddo
              endif
            endif
          endif
!*************************
!  Internal derivatives  *
!*************************
          if (lgrad1) then
            xderv(i) = xderv(i) - x21*e1d(1) - x31*e1d(2) - x41*e1d(3)
            yderv(i) = yderv(i) - y21*e1d(1) - y31*e1d(2) - y41*e1d(3)
            zderv(i) = zderv(i) - z21*e1d(1) - z31*e1d(2) - z41*e1d(3)
            xderv(j) = xderv(j) - x32*e1d(4) + x21*e1d(1) - x42*e1d(5)
            yderv(j) = yderv(j) - y32*e1d(4) + y21*e1d(1) - y42*e1d(5)
            zderv(j) = zderv(j) - z32*e1d(4) + z21*e1d(1) - z42*e1d(5)
            xderv(k) = xderv(k) + x32*e1d(4) - x43*e1d(6) + x31*e1d(2)
            yderv(k) = yderv(k) + y32*e1d(4) - y43*e1d(6) + y31*e1d(2)
            zderv(k) = zderv(k) + z32*e1d(4) - z43*e1d(6) + z31*e1d(2)
            xderv(l) = xderv(l) + x43*e1d(6) + x42*e1d(5) + x41*e1d(3)
            yderv(l) = yderv(l) + y43*e1d(6) + y42*e1d(5) + y41*e1d(3)
            zderv(l) = zderv(l) + z43*e1d(6) + z42*e1d(5) + z41*e1d(3)
          endif
          if (lgrad2) then
!
!  New vector array between atoms to handle sign 
!
!  Atom 1
!
            vec(1,1,1) = -x21
            vec(2,1,1) = -y21
            vec(3,1,1) = -z21
            vec(1,2,1) = -x31
            vec(2,2,1) = -y31
            vec(3,2,1) = -z31
            vec(1,3,1) = -x41
            vec(2,3,1) = -y41
            vec(3,3,1) = -z41
!
!  Atom 2
!
            vec(1,1,2) = x21
            vec(2,1,2) = y21
            vec(3,1,2) = z21
            vec(1,2,2) = -x32
            vec(2,2,2) = -y32
            vec(3,2,2) = -z32
            vec(1,3,2) = -x42
            vec(2,3,2) = -y42
            vec(3,3,2) = -z42
!
!  Atom 3
!
            vec(1,1,3) = x31
            vec(2,1,3) = y31
            vec(3,1,3) = z31
            vec(1,2,3) = x32
            vec(2,2,3) = y32
            vec(3,2,3) = z32
            vec(1,3,3) = -x43
            vec(2,3,3) = -y43
            vec(3,3,3) = -z43
!
!  Atom 4
!
            vec(1,1,4) = x41
            vec(2,1,4) = y41
            vec(3,1,4) = z41
            vec(1,2,4) = x42
            vec(2,2,4) = y42
            vec(3,2,4) = z42
            vec(1,3,4) = x43
            vec(2,3,4) = y43
            vec(3,3,4) = z43
!
!  Loop over first coordinate
!
            do kk = 1,3
              n112 = n1x2 - 1 + kk
              n113 = n1x3 - 1 + kk
              n114 = n1x4 - 1 + kk
              n211 = n2x1 - 1 + kk
              n213 = n2x3 - 1 + kk
              n214 = n2x4 - 1 + kk
              n311 = n3x1 - 1 + kk
              n312 = n3x2 - 1 + kk
              n314 = n3x4 - 1 + kk
              n411 = n4x1 - 1 + kk
              n412 = n4x2 - 1 + kk
              n413 = n4x3 - 1 + kk
!
!  First term
!
              if (lopi.and.lopj) then
                derv2(n112,n211) = derv2(n112,n211) - e1d(1)
                derv2(n211,n112) = derv2(n211,n112) - e1d(1)
              elseif (lopi.or.lopj) then
                derv2(n112,n211) = derv2(n112,n211) - e1d(1)
              endif
              if (lopi.and.lopk) then
                derv2(n113,n311) = derv2(n113,n311) - e1d(2)
                derv2(n311,n113) = derv2(n311,n113) - e1d(2)
              elseif (lopi.or.lopk) then
                derv2(n113,n311) = derv2(n113,n311) - e1d(2)
              endif
              if (lopi.and.lopl) then
                derv2(n114,n411) = derv2(n114,n411) - e1d(3)
                derv2(n411,n114) = derv2(n411,n114) - e1d(3)
              elseif (lopi.or.lopl) then
                derv2(n114,n411) = derv2(n114,n411) - e1d(3)
              endif
              if (lopj.and.lopk) then
                derv2(n213,n312) = derv2(n213,n312) - e1d(4)
                derv2(n312,n213) = derv2(n312,n213) - e1d(4)
              elseif (lopj.or.lopk) then
                derv2(n213,n312) = derv2(n213,n312) - e1d(4)
              endif
              if (lopj.and.lopl) then
                derv2(n214,n412) = derv2(n214,n412) - e1d(5)
                derv2(n412,n214) = derv2(n412,n214) - e1d(5)
              elseif (lopj.or.lopl) then
                derv2(n214,n412) = derv2(n214,n412) - e1d(5)
              endif
              if (lopk.and.lopl) then
                derv2(n314,n413) = derv2(n314,n413) - e1d(6)
                derv2(n413,n314) = derv2(n413,n314) - e1d(6)
              elseif (lopk.or.lopl) then
                derv2(n314,n413) = derv2(n314,n413) - e1d(6)
              endif
!
!  Loop over second coordinate
!
              do kl = 1,3
                n221 = n2x1 - 1 + kl
                n321 = n3x1 - 1 + kl
                n322 = n3x2 - 1 + kl
                n421 = n4x1 - 1 + kl
                n422 = n4x2 - 1 + kl
                n423 = n4x3 - 1 + kl
!
!  Sum over vectors atom-atom second derivatives
!
                t12 = 0.0_dp
                t13 = 0.0_dp
                t14 = 0.0_dp
                t23 = 0.0_dp
                t24 = 0.0_dp
                t34 = 0.0_dp
                do ki = 1,3
                  do kj = 1,3
                    t12 = t12 + vec(kk,ki,1)*vec(kl,kj,2)*e2d(kb(n3vec(ki,1),n3vec(kj,2)))
                    t13 = t13 + vec(kk,ki,1)*vec(kl,kj,3)*e2d(kb(n3vec(ki,1),n3vec(kj,3)))
                    t14 = t14 + vec(kk,ki,1)*vec(kl,kj,4)*e2d(kb(n3vec(ki,1),n3vec(kj,4)))
                    t23 = t23 + vec(kk,ki,2)*vec(kl,kj,3)*e2d(kb(n3vec(ki,2),n3vec(kj,3)))
                    t24 = t24 + vec(kk,ki,2)*vec(kl,kj,4)*e2d(kb(n3vec(ki,2),n3vec(kj,4)))
                    t34 = t34 + vec(kk,ki,3)*vec(kl,kj,4)*e2d(kb(n3vec(ki,3),n3vec(kj,4)))
                  enddo
                enddo
!
!  Add terms to total second derivative matrix
!
                if (lopi.and.lopj) then
                  derv2(n112,n221) = derv2(n112,n221) + t12
                  derv2(n221,n112) = derv2(n221,n112) + t12
                elseif (lopj) then
                  derv2(n112,n221) = derv2(n112,n221) + t12
                elseif (lopi) then
                  derv2(n221,n112) = derv2(n221,n112) + t12
                endif
                if (lopi.and.lopk) then
                  derv2(n113,n321) = derv2(n113,n321) + t13
                  derv2(n321,n113) = derv2(n321,n113) + t13
                elseif (lopk) then
                  derv2(n113,n321) = derv2(n113,n321) + t13
                elseif (lopi) then
                  derv2(n321,n113) = derv2(n321,n113) + t13
                endif
                if (lopi.and.lopl) then
                  derv2(n114,n421) = derv2(n114,n421) + t14
                  derv2(n421,n114) = derv2(n421,n114) + t14
                elseif (lopl) then
                  derv2(n114,n421) = derv2(n114,n421) + t14
                elseif (lopi) then
                  derv2(n421,n114) = derv2(n421,n114) + t14
                endif
                if (lopj.and.lopk) then
                  derv2(n213,n322) = derv2(n213,n322) + t23
                  derv2(n322,n213) = derv2(n322,n213) + t23
                elseif (lopk) then
                  derv2(n213,n322) = derv2(n213,n322) + t23
                elseif (lopj) then
                  derv2(n322,n213) = derv2(n322,n213) + t23
                endif
                if (lopj.and.lopl) then
                  derv2(n214,n422) = derv2(n214,n422) + t24
                  derv2(n422,n214) = derv2(n422,n214) + t24
                elseif (lopl) then
                  derv2(n214,n422) = derv2(n214,n422) + t24
                elseif (lopj) then
                  derv2(n422,n214) = derv2(n422,n214) + t24
                endif
                if (lopk.and.lopl) then
                  derv2(n314,n423) = derv2(n314,n423) + t34
                  derv2(n423,n314) = derv2(n423,n314) + t34
                elseif (lopl) then
                  derv2(n314,n423) = derv2(n314,n423) + t34
                elseif (lopk) then
                  derv2(n423,n314) = derv2(n423,n314) + t34
                endif
              enddo
            enddo
          endif
        enddo
120   continue
    enddo lkloop
  enddo ljloop
!
!  Closing banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  End of outer loops
!
5 continue
!****************************
!  Out of plane potentials  *
!****************************
  if (noofp.gt.0) call fouroop(eoop,esregion12loop,esregion2,eattach,lgrad1,lgrad2,xderv,yderv,zderv)
!   
!  Marvin compatibility option -> all three body terms are in region 1
!   
  if (lmarvreg2) then
    efor = efor + esregion12l
    eoop = eoop + esregion12loop
  else
    esregion12 = esregion12 + esregion12l + esregion12loop
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
!
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
  if (status/=0) call deallocate_error('fournos','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('fournos','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('fournos','xvec')
  deallocate(zderv,stat=status)
  if (status/=0) call deallocate_error('fournos','zderv')
  deallocate(yderv,stat=status)
  if (status/=0) call deallocate_error('fournos','yderv')
  deallocate(xderv,stat=status)
  if (status/=0) call deallocate_error('fournos','xderv')
  deallocate(ioptptr,stat=status)
  if (status/=0) call deallocate_error('fournos','ioptptr')
  deallocate(nuniquelptr,stat=status)
  if (status/=0) call deallocate_error('fournos','nuniquelptr')
  deallocate(nuniquekptr,stat=status)
  if (status/=0) call deallocate_error('fournos','nuniquekptr')
  deallocate(nuniqueiptr,stat=status)
  if (status/=0) call deallocate_error('fournos','nuniqueiptr')
  deallocate(nptrnfornonoop,stat=status)
  if (status/=0) call deallocate_error('fournos','nptrnfornonoop')
  deallocate(ntypmiddle,stat=status)
  if (status/=0) call deallocate_error('fournos','ntypmiddle')
  deallocate(natmiddle,stat=status)
  if (status/=0) call deallocate_error('fournos','natmiddle')
!
!  Timing
!
  time2 = cputime()
  tfour = tfour + time2 - time1
!
  return
  end
