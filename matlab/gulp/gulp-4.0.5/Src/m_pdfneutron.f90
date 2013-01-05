!****************************************************************************!
!                                                                            !
!       ******                 module m_pdfneutron            ******         !
!                              ==============                                !
!                                                                            !
! this module controls the calculation of PDFs                               !
!                                                                            !
! If you believe you have found a bug in this module contact the author.     !
!                                                                            !
!                                                                            !
!                    **   subroutines and functions    **                    !
!                                                                            !
!Contains neutron subroutines:                                               !
!  allocateneutronarrays                                                     !
!  deallocateneutronarrays                                                   !
!  atomcord                                                                  !
!  bose_eins                                                                 !
!  changefreq                                                                !
!  initchanges_pdf_cfg                                                       !
!  changethisfreq                                                            ! 
!  changemaxpdfcfg                                                           !
!  cmpxdiv                                                                   !
!  datetimechannel                                                           !
!  init_pdf                                                                  !
!  makeeigenarrays                                                           !
!  makeintvector                                                             !
!  masses                                                                    !
!  mod_vector                                                                !
!  pdfword                                                                   !
!  nrelat_setup                                                              !
!  nullpointer_neutron                                                       !
!  nextline                                                                  !
!  outpdfin                                                                  !
!  setcurk                                                                   !
!  setupcores                                                                !
!  setuppdfphonon                                                            !
!  euler                                                                     !
!                                                                            !
!                                               Beth Cope [ers29] (c) 2007-9 !
!                                                          ers29@cam.ac.uk   !
!  Adapted for inclusion in GULP by JDG, June 2009                           !
!  Revisions included    in GULP by JDG, Aug  2010                           !
!                                                                            !
!****************************************************************************!
!Conditions of use:                                                          !
!                                                                            !
!  GULP is available free of charge to academic institutions                 !
!  and non-commerical establishments only. Copies should be                  !
!  obtained from the author only and should not be distributed               !
!  in any form by the user to a third party without the express              !
!  permission of the author. This notice applies to all parts                !
!  of the program, except any library routines which are                     ! 
!  distributed with the code for completeness. All rights for                !
!  such routines remain with the original distributor.                       !
!                                                                            !
!  No claim is made that this program is free from errors and                !
!  no liability will be accepted for any loss or damage that                 !
!  may result. The user is responsible for checking the validity             !
!  of their results.                                                         !
!                                                                            !
!  Copyright Curtin University 2009                                            !
!                                                                            !
!  Julian Gale, NRI, Curtin University, 2009                                 !
!  Elizabeth Cope, Cambridge University, 2009                                !
!****************************************************************************!
module m_pdfneutron
  use datatypes 
  use iochannels !ers29 added 18 nov 06
  implicit none
  save
!
!  logicals, all initialised to false in init_pdf
!
  logical                                           :: lcoreinfo ! output core info
  logical                                           :: lfreqcut  ! PDF cutoff flag
  logical                                           :: lkeepcut  ! PDF cutoff flag 
  logical                                           :: lmakeeigarray ! store eigenvector and freqs for further use
  logical                                           :: lneutronall !apply this neutron block to all cfgs
  logical                                           :: lpdfout   ! something to output from neutron block
  logical                                           :: lnoksym   ! use whole BZ; no symmetry
  logical                                           :: lnowidth  ! PDF width output suppressed
  logical                                           :: lpartial  ! PDF partial output
  logical                                           :: lpdf      ! Perform PDF calculation
  ! largest magnitude is all real for all eigenvectors, and renormalise
  ! to do with element input
  logical                                           :: lneutronele ! neutron ele data read in OK 
  logical                                           :: lelecurrent ! neutron ele data read from actualy file called "elefile" in current directory
  logical                                           :: lelefile ! neutron ele data read from string 'elefile' 
!
!     files
!
  character(len=80), dimension(:), pointer :: pdffiles => null()
!
!  dynamical matrix "eigen-arrays"
!
  complex(dpc),    dimension(:,:,:,:), allocatable    :: epolar_array
!
! epolar_array is allocated with (3[x,y,z], nu, atom, k)
!
  real(dp),        dimension(:,:),     allocatable    :: omega_array
!
! omega_array is allocated with (nu,k)
!
!  for core info
!
  real(dp), dimension(3,3) :: kvT !transpose of kv to allow matmul:  recip frac to cart
  real(dp), dimension(3,3) :: kvTinv !inverse of kvT to allow recip cart to frac
  real(dp), dimension(3,3) :: rvT ! transpose of rv to allow matmul: real frac to cart
  real(dp), dimension(3,3) :: kvT2pi !recip frac to ISIScart (inc 2pi)
  real(dp), dimension(3,3) :: kvT2piinv ! recip ISIScart to frac

  integer(i4),     dimension(:),     allocatable    :: nrelat_cores
  real(dp),        dimension(:,:),   allocatable    :: fracvec
  real(dp),        dimension(:,:),   allocatable    :: atomvec
  real(dp),        dimension(:,:),   allocatable    :: cartvec
  real(dp),        dimension(:),     allocatable    :: arraymasses_kgmol
  real(dp),        dimension(:),     allocatable    :: arraymasses_amu
  real(dp),        dimension(:),     allocatable    :: arraymasses_kg
  real(dp),        dimension(:),     allocatable    :: bbar_cores
  real(dp),        dimension(:),     allocatable    :: bbar_cores_A
  real(dp),        dimension(:),     allocatable    :: siglengthbysqrtmass! added ers29 2/09
  real(dp),        dimension(:),     allocatable    :: bbarbysqrtmass! added ers29 2/09
  real(dp),        dimension(:),     allocatable    :: siginc_cores
  real(dp),        dimension(:),     allocatable    :: siginc_cores_A2
  character(len=10), dimension(:),   allocatable    :: nameslist
!
!  for k information
!
  integer(i4)                                       :: icurrentk
  real(dp),        dimension(3)                     :: veck
  real(dp),        dimension(3)                     :: veckcart
!
!  for pdf information
!
  real(dp),        dimension(3)                     :: Rlbeth
  integer(i4)                                       :: partialno
  real(dp),        dimension(:),       allocatable  :: rbins
  character(len=80)                                 :: pdffilecfg
  real(dp),        dimension(:,:),     allocatable  :: partialmasses
  real(dp),        dimension(:),       allocatable  :: partialfactor ! holds cicj
  character(len=5),dimension(:,:),     allocatable  :: partialnames
  real(dp),        dimension(:,:),     allocatable  :: pdf
  real(dp),        dimension(:),       allocatable  :: pdf_ij
  real(dp),        dimension(:),       allocatable  :: pdfpair
  real(dp),        dimension(:),       allocatable  :: T_r
  real(dp),        dimension(:),       allocatable  :: D_r
  real(dp),        dimension(:,:),     allocatable  :: G_r
  real(dp),        dimension(:),       allocatable  :: G_PDF_r
  real(dp),        dimension(:,:),     allocatable  :: G_RMC_r
!
!  pointers for PDF config
!
  real(dp),        dimension(:),       pointer      :: rmaxcfgs => null()
  real(dp),        dimension(:),       pointer      :: wmaxcfgs => null()
  real(dp),        dimension(:),       pointer      :: wmincfgs => null()
  integer(i4),     dimension(:),       pointer      :: nrbinscfgs => null()
!
!  others
!
  real(dp),                            parameter    :: zero = 1D-7
  integer(i4),     dimension(:) ,      pointer      :: nkpointscfg => null()
  logical ,        dimension(:),       pointer      :: lshrinkset => null()
  logical ,        dimension(:),       pointer      :: ldispersionset => null()
  integer(i4)                                       :: nkpoints
  integer(i4)                                       :: neffectivek
  integer(i4)                                       :: nmodes
  integer(i4)                                       :: natomp
  integer(i4)                                       :: natomfull
!
!  for frequency units: now all pointers april 2007
!
  logical,          dimension(:),      pointer      :: lunitrad => null()
  logical,          dimension(:),      pointer      :: lunitthz => null()
  logical,          dimension(:),      pointer      :: lunitmev => null()
  logical,          dimension(:),      pointer      :: lunitcm => null()
  character(len=6), dimension(:),      pointer      :: unitsnamecfgs => null()
  real(dp)                                          :: converttoradps

Contains

!****************************************************************************!
  subroutine allocateneutronarrays
!
!  Allocates those saved arrays used in neutron module
!  nrelat_cores, fracvec, atomvec,cartvec, arraymasses_kgmol,arraymasses_amu
!  arraymasses_kg, nameslist, bbar_cores, bbar_cores_A, siginc_cores
!  siginc_cores_A2, omega_array,epolar_array,  siglengthbysqrtmass, bbarbysqrtmass 
!
!  Written by Elizabeth !ope (ers29) 2006
!  4/07 made .f90 and use allocation error subroutines
!
  implicit none
!
!  number of cores in phonon calculation is natomp stored in neutron
!
  integer (i4) :: ierr = 0 !for allocation stat (= 0 when OK)
!
!  first check none have been previously allocated
!
  call deallocateneutronarrays
!
!  now allocate them all
!
  allocate(nrelat_cores(natomfull), stat=ierr) !corrected ers29 8th nov 06
  if (ierr .ne. 0) call outofmemory("allocateneutronarrays", "nrelat_cores")
  nrelat_cores = 0

  allocate(fracvec(natomp,3), stat=ierr)
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","fracvec")
  fracvec = 0

  allocate(atomvec(natomp,3), stat=ierr)
  if (ierr .ne. 0) call outofmemory("allocateneutronarrays","atomvec")
  atomvec = 0

  allocate(cartvec(natomp,3), stat=ierr)
  if (ierr .ne. 0) call outofmemory("allocateneutronarrays","cartvec")
  cartvec = 0

  allocate(arraymasses_kgmol(natomp), stat=ierr)
  if (ierr.ne.0) call outofmemory("allocateneutronarrays","arraymasses_kgmol")
  arraymasses_kgmol = 0

  allocate(arraymasses_amu(natomp), stat=ierr)
  if (ierr .ne. 0) call outofmemory("allocateneutronarrays","arraymasses_amu")
  arraymasses_amu = 0

  allocate(arraymasses_kg(natomp), stat=ierr)
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","arraymasses_kg")
  arraymasses_kg = 0

  allocate(nameslist(natomp), stat=ierr)
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","nameslist")
  nameslist = ''

  allocate(bbar_cores(natomp),stat=ierr)
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","bbar_cores")
  bbar_cores = 0

  allocate(siginc_cores(natomp),stat=ierr)
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","siginc_cores")
  siginc_cores = 0

  allocate( siglengthbysqrtmass(natomp),stat=ierr) !ers29 2/08
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays"," siglengthbysqrtmass")
  siglengthbysqrtmass= 0

  allocate( bbarbysqrtmass(natomp),stat=ierr) !ers29 2/08
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays"," bbarbysqrtmass")
  bbarbysqrtmass= 0

  allocate(bbar_cores_A(natomp),stat=ierr)
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","bbar_cores")
  bbar_cores_A = 0

  allocate(siginc_cores_A2(natomp),stat=ierr)
  if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","siginc_cores_A2")
  siginc_cores_A2 = 0

  if (lmakeeigarray) then
    neffectivek = nkpoints
    allocate( omega_array(nmodes, neffectivek), STAT = ierr)
    if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","omega_array")
    omega_array = 0
    allocate( epolar_array(3,nmodes,natomp,neffectivek), STAT = ierr)
    ! epolar_array is allocated with (3[x,y,z], nu, atom, k)
    if (ierr .ne. 0)  call outofmemory("allocateneutronarrays","epolar_array")
    epolar_array = 0
  endif

  return
  end subroutine allocateneutronarrays

!****************************************************************************!
  subroutine deallocateneutronarrays 
!
!  Deallocate the arrays allocated in allocateneutronarrays
!
  implicit none
  integer (i4) :: ierr = 1 !for allocation stat (= 0 when OK)
!
!  first check none have been previously allocated
!
  if (allocated(nrelat_cores)) then
    deallocate(nrelat_cores,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","nrelat_cores")
  endif
  if (allocated(fracvec)) then
    deallocate(fracvec,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","fracvec")
  endif
  if (allocated(atomvec)) then
    deallocate(atomvec,stat=ierr)
    if (ierr .ne. 0)  call deallocate_error("deallocateneutronarrays","atomvec")
  endif
  if (allocated(cartvec)) then
    deallocate(cartvec,stat=ierr)
    if (ierr .ne. 0)  call deallocate_error("deallocateneutronarrays","cartvec")
  endif

  if (allocated(arraymasses_kgmol)) then
    deallocate(arraymasses_kgmol,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","arraymasses_kgmol")
  endif
  if (allocated(arraymasses_amu)) then
    deallocate(arraymasses_amu,stat=ierr)
    if (ierr .ne. 0)  call deallocate_error("deallocateneutronarrays","arraymasses_amy")
  endif
  if (allocated(arraymasses_kg)) then
    deallocate(arraymasses_kg,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","arraymasses_kg")
  endif
  if (allocated(nameslist)) then
    deallocate(nameslist,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","nameslist")
  endif
  if (allocated(bbar_cores)) then
    deallocate(bbar_cores,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","bbar_cores")
  endif
  if (allocated(siginc_cores)) then
    deallocate(siginc_cores,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","siginc_cores")
  endif

  if (allocated(siglengthbysqrtmass)) then !ers29 2/09
    deallocate( siglengthbysqrtmass,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays"," siglengthbysqrtmass")
  endif

  if (allocated( bbarbysqrtmass)) then !ers29 2/09
    deallocate( bbarbysqrtmass,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays"," bbarbysqrtmass")
  endif

  if (allocated(bbar_cores_A)) then
    deallocate(bbar_cores_A,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","bbar_cores")
  endif
  if (allocated(siginc_cores_A2)) then
    deallocate(siginc_cores_A2,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","siginc_cores")
  endif
  if (allocated(omega_array)) then
    deallocate(omega_array,stat=ierr)
    if (ierr .ne. 0)  call deallocate_error("deallocateneutronarrays","omega_array")
  endif
  if (allocated(epolar_array)) then
    deallocate(epolar_array,stat=ierr)
    if (ierr .ne. 0) call deallocate_error("deallocateneutronarrays","epolar_array")
  endif

  end subroutine deallocateneutronarrays

!****************************************************************************!

  function bose_eins(omega_rads)
!
!  written by Elizabeth Cope(ers29) to calculate the bose_einstien n(w)
!  modified 23rd feb-3rd march 06 for gulp3
!  n(w) with input in rads/s (hence use of hbar)
!  BUG FIX 20th jan 2006
!  04/07 made .f90 and intents added
!
  use datatypes
  use current, only   : temperature
  use constants, only : hbar_js, boltz
  implicit none
  real(dp)             :: bose_eins
  real(dp), intent(in) :: omega_rads

  if (temperature == 0.0_dp) then
    bose_eins = 1.0_dp
    return
  endif

  bose_eins = 1.0_dp/(exp((hbar_JS*omega_rads)/(boltz*temperature))-1.0_dp)

  end function bose_eins
!
!     NB ISIS use E(n(w)+1), subtract 1 to get E(n(w))
!      bose_eins = hbar_JS*omega_rads
!     *     *(1.0_dp/(1.0_dp- exp(((-hbar_JS*omega_rads)/(boltz*temperature)) ))  -1.0_dp)
!

!****************************************************************************!

  subroutine atomcord
!
!  SUBROUTINE atomcord
!  will produce an array of the atomic coordinates of the CORES
!  OUTPUTS:
!      nameslist(ncores) :: the concatonated name of all contributing atoms
!                           for each core (in normal order)
!      atomvec(ncores,3) :: the position vector (x,y,z) for each 
!                           core (in normal order) (A)
!      cartvec(ncores,3) :: the carteisan position vector (x,y,z) for each 
!                           core (in normal order) (A)
!      kvT(3,3) :: transpose of kv matrix to convert recip fracs into cartei
!      rvT(3,3) :: transpose of kv matrix to convert real fracs into carteisan
!
!  This cycles through all the atoms, and whenever a core is found 
!  produces both the nameslist array entry (containing the concatonated
!  names of all atoms contributing (if a partial)) and the
!  atomvec(atom, 1:3) atomic postion vector from the inbuilt (all config)
!  xclat,yclat, xclat arrays.
! 
!  it also picks up on fractional coordinates from xfrac, yfrac and zfrac,
!  and converts these to cartesians with rvT
!
!  CREATED by Elizabeth Cope 31st March 05
!  MODIFIED 31/3/05 ers29
!  modified 13/6/06 ers29 to reference 'irreducible' cell
!  modified for gulp 3 7th feb 06
!  04/07 made .90
!  07/07 reworked to use partial module
!
  use element,   only : maxele
  use partial,   only : nsfoc, ncfoc, iocptr
  use shell,     only : ncore
  use current
  use constants, only : pi
  implicit none
!
!  Local variables
!
  integer(i4)                         :: icores ! to hold core count
  integer(i4)                         :: ifail  ! error flag for matinv
  character(len=5)                    :: lab    ! to hold atom name
  character(len=10), dimension(numat) :: listall
  logical                             :: lpocc
  integer(i4)                         :: inat, itype, i, ii
  real(dp)                            :: wrk(6)        ! workspace for matinv

  if (ndim .ne. 3) then !stop as PDF and neutron code only supports three dimenstions
    call outerror("Neutron module only supports three dimensions",0_i4)
    call stopnow("atomcord")
  endif
!
!  first setup kvT and rvT stored in neutron module
!
  kvT(1,1) = kv(1,1)
  kvT(1,2) = kv(2,1)
  kvT(1,3) = kv(3,1)
  kvT(2,1) = kv(1,2)
  kvT(3,1) = kv(1,3)
  kvT(2,2) = kv(2,2)
  kvT(2,3) = kv(3,2)
  kvT(3,3) = kv(3,3)
  kvT(3,2) = kv(2,3)

  kvT2pi = kvT*(2*pi)
    
  kvT2piinv(1:3,1:3) = kvT2pi(1:3,1:3)
  call matinv(kvT2piinv,3_i4,3_i4,wrk,ifail)
  if (ifail.gt.0) then
    call outerror('cartesian conversion matrix could not be inverted togive kvT2piinv',0_i4)
    stop
  endif
    
  kvTinv(1:3,1:3) = kvT(1:3,1:3)
  call matinv(kvTinv,3_i4,3_i4,wrk,ifail)
  if (ifail.gt.0) then
    call outerror('cartesian conversion matrix could not be inverted togive kvT2inv',0_i4)
    stop
  endif

  rvT(1,1) = rv(1,1)
  rvT(1,2) = rv(2,1)
  rvT(1,3) = rv(3,1)
  rvT(2,1) = rv(1,2)
  rvT(3,1) = rv(1,3)
  rvT(2,2) = rv(2,2)
  rvT(2,3) = rv(3,2)
  rvT(3,3) = rv(3,3)
  rvT(3,2) = rv(2,3)
!
!  the main code
!
  listall = "          "
  icores = 1
  lpocc = (nsfoc+ncfoc.ne.numat)

  if (lpocc) then !modification for partial occupancies
!
!  first set up nameslist
!
    do i = 1,numat
      inat = iatn(nrelat(i))    ! use pointer to asym atom
      itype = natype(nrelat(i)) ! use pointer to asym atom
      ii = iocptr(i)
      if ((inat .le. maxele).and. (inat .ne. 0)) then 
        ! it must be a core           
        call label(inat,itype,lab)
        listall(ii) = trim(listall(ii))//trim(lab)
      else
        call label(inat,itype,lab)
        listall(ii) = "shell-"//trim(lab)   
      endif
    enddo
!
!  then set up cart list
!
    do i = 1,ncore
      ii = iocptr(i)
      inat = iatn(nrelat(i))    ! use pointer to asym atom
      itype = natype(nrelat(i)) ! use pointer to asym atom 

      if ((inat .le. maxele).and. (inat .ne. 0)) then 
        ! it must be a core
        atomvec(ii, 1) = xclat(i)
        atomvec(ii, 2) = yclat(i)
        atomvec(ii, 3) = zclat(i)
        fracvec(ii, 1) = xfrac(i)
        fracvec(ii, 2) = yfrac(i)
        fracvec(ii, 3) = zfrac(i)
        cartvec(ii,1:3) = matmul(fracvec(ii,1:3),rvT) !! real space convertion of frac to cart
        nameslist(ii) = listall(ii)
      endif
    enddo

  else                      !original full occupancy code
    !     only enter in atomvec if it is a core
    do i = 1,numat !changed from maxat to numat ers29 27th feb
      inat = iatn(nrelat(i))    ! use pointer to asym atom
      itype = natype(nrelat(i)) ! use pointer to asym atom
      if ((inat .le. maxele).and. (inat .ne. 0)) then ! it must be a core
        atomvec(icores, 1) = xclat(i)
        atomvec(icores, 2) = yclat(i)
        atomvec(icores, 3) = zclat(i)
        fracvec(icores, 1) = xfrac(i)
        fracvec(icores, 2) = yfrac(i)
        fracvec(icores, 3) = zfrac(i)
        cartvec(icores,1:3) = matmul(fracvec(icores,1:3),rvT) !real space conversion of frac to cart
        call label(inat,itype,lab)
        nameslist(icores) = lab
        icores = icores + 1 !increment the core count
      endif
    enddo
  endif

  end subroutine atomcord
!****************************************************************************!
  subroutine changefreq(ltoradps, nconfig)
!
! subroutine to change angular frequency units
! written by Elizabeth Cope
!       4/07 added nconfig to use with multiple configurations
!            and made f90
  use constants
  implicit none
  logical, intent(in) :: ltoradps
  integer(i4), intent(in) :: nconfig

  if (lunitrad(nconfig)) return      !no changes needed

  if (lunitthz(nconfig)) converttoradps = thztorad
  if (lunitcm(nconfig)) converttoradps = cmtorads
  if (lunitmev(nconfig)) converttoradps = mevtorad
  if (ltoradps) then        !convert
    unitsnamecfgs(nconfig) = "rad/s"         
    wmaxcfgs(nconfig) = wmaxcfgs(nconfig)*converttoradps
    wmincfgs(nconfig) = wmincfgs(nconfig)*converttoradps
  else
    if (lunitthz(nconfig))     unitsnamecfgs(nconfig) = "THz"
    if (lunitcm(nconfig))     unitsnamecfgs(nconfig) = "cm**-1"
    if (lunitmev(nconfig))     unitsnamecfgs(nconfig) = "meV"
    wmaxcfgs(nconfig) = wmaxcfgs(nconfig)/converttoradps
    wmincfgs(nconfig) = wmincfgs(nconfig)/converttoradps
  endif
  end subroutine changefreq

!****************************************************************************!
  subroutine changemaxpdfcfg
!
!  Alters the size of the arrays associated with maxcfg to be called from changemaxcfg
!
!   4/07 created from changemaxcfg by Elizabeth Cope (ers29)
!   6/09 Modified by merging with separate init routine - JDG
!
  use datatypes
  use configurations
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxcfg = 0
!
!  Configuration data
!
  call realloc(ldispersionset,maxcfg,ierror) ! ers29 9/5/07
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','ldispersionset')
  call realloc(lshrinkset,maxcfg,ierror) ! ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','lshrinkset')
  call realloc(nkpointscfg,maxcfg,ierror) !ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','nkpointscfg')
!
!  for PDFS
!
  call realloc_ch80(pdffiles,maxcfg,ierror) 
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','pdffiles')
  call realloc(nrbinscfgs,maxcfg,ierror) !ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','nrbinscfgs')
  call realloc(rmaxcfgs,maxcfg,ierror) !ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','rmaxcfgs')
  call realloc(wmaxcfgs,maxcfg,ierror) !ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','wmaxcfgs')
  call realloc(wmincfgs,maxcfg,ierror) !ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','wmincfgs')
!
!  for frequency units
!
  call realloc_ch6(unitsnamecfgs,maxcfg,ierror) 
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','unitsnamecfgs')    
  call realloc(lunitrad,maxcfg,ierror) ! ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','lunitrad')
  call realloc(lunitthz,maxcfg,ierror) ! ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','lunitthz')
  call realloc(lunitmev,maxcfg,ierror) ! ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','lunitmev')
  call realloc(lunitcm,maxcfg,ierror) ! ers29
  if (ierror.ne.0) call outofmemory('changemaxpdfcfg','lunitcm')
!
!  Initialise defaults for new part of array
!
  if (maxcfg.gt.oldmaxcfg) then
    do i = oldmaxcfg+1,maxcfg
      nkpointscfg(i) = 0 ! ers29 26/2/07
      lshrinkset(i) = .false.
      ldispersionset(i) = .false.
      !     for PDFs
      pdffiles(i) = ' ' 
      nrbinscfgs(i) = 100 !default value
      rmaxcfgs(i) = 5.0_dp !default value
      wmaxcfgs(i) = 0.0_dp
      wmincfgs(i) = 0.0_dp
      !     for units freq
      unitsnamecfgs(i) = '  ' 
      lunitrad(i) = .false.
      lunitthz(i) = .true. !default
      lunitmev(i) = .false.
      lunitcm(i) = .false.
    enddo
  endif
!
!  Save current value of maxcfg for next call
!
  oldmaxcfg = maxcfg

  end subroutine changemaxpdfcfg

!****************************************************************************!

  subroutine changethisfreq(ltoradps, thisfreq, thisunit)
!
! subroutine to change just the input freq (and thisunit string)
! between internal units (rad/s) and i/o units
! use ltoradps = T for converting TO rads, and F for converting back
!
!  written by Elizabeth Cope (ers29)
!  modified september 2006
!  4/07 moved to neutron module, intents added, 
!       changed for multiple configs and made .f90
!
  use current, only : ncf
  use constants
  implicit none
  logical,          intent(in)            :: ltoradps
  real(dp),         intent(inout)         :: thisfreq
  character(len=6), intent(out), optional :: thisunit

  if (lunitrad(ncf)) return      !no changes needed

  if (lunitthz(ncf)) converttoradps = thztorad
  if (lunitcm(ncf)) converttoradps = cmtorads
  if (lunitmev(ncf)) converttoradps = mevtorad
  if (ltoradps) then        !convert
    if (present(thisunit)) thisunit = "rad/s"         
    thisfreq = thisfreq*converttoradps
  else
    if (present(thisunit)) then
      if (lunitthz(ncf))     thisunit = "THz"
      if (lunitcm(ncf))      thisunit = "cm**-1"
      if (lunitmev(ncf))     thisunit = "meV"
    endif
    thisfreq = thisfreq/converttoradps
  endif
  end subroutine changethisfreq

!****************************************************************************!

  function cmpxdiv(cmpx1, cmpx2)
!
!  PURPOSE: to perform complex division on two complex numbers
!          according to the method in CUP Numerical Methods
!          in F77:
!          http://www.library.cornell.edu/nr/bookfpdf/f5-4.pdf
!          soln = cmpx1/cmpx2
!
!  CREATED: by Elizabeth Cope(ers29) on 10th Jan 05
!  MODIFIED ers29 3/5/05 ers29 to make a function
!  modified for gulp3 ers29 feb 06
!  04.07 made .f90
!

  use datatypes
  implicit none
  complex(dpc)             :: cmpxdiv
!
!  Passed variables
!
  complex(dpc), intent(IN) :: cmpx1, cmpx2 !input complex numbers
!
!  Local variables
!
  real(dp)                 :: a,b,c,d, realsoln, imsoln
!
!  break down complex numbers into reals:
!  cmpx1 = a+ib
!  cmpx2 = c+id     
!
  a = real(cmpx1)
  b = aimag(cmpx1)
  c = real(cmpx2)
  d = aimag(cmpx2)

  if (abs(c) >= abs(d)) then
    realsoln = (a+b*(d/c))/(c+d*(d/c))
    imsoln = (b-a*(d/c))/(c+d*(d/c))
  else
    realsoln = (a*(c/d)+b)/(c*(c/d)+d)
    imsoln = (b*(c/d)-a)/(c*(c/d)+d)
  endif
  cmpxdiv = cmplx(realsoln, imsoln)

  end function cmpxdiv

  function complexmodsq(z)
  use datatypes
  implicit none

  real(dp) :: complexmodsq
  complex(dpc), intent(in) :: z
  real(dp) ::  x,y

  x = real(z)
  y = aimag(z)
  complexmodsq = (x**2 + y**2)

  end function complexmodsq

!****************************************************************************!

  subroutine datetimechannel(imode, channel)
!
!  Outputs date/time
!
!  imode = 1 => label as start time
!  imode = 2 => label as current time
!  imode = 3 => label as finish time
!
!   3/07 copied from datetime to allow output to other channels, ers29
!     Elizabeth Cope
!   4/07 made f90
!
  use iochannels
  use parallel
  implicit none
!
!  Passed Arguments
!
  integer(i4), intent(in)   :: imode 
  integer(i4), intent(in)   :: channel
!
!  Local variables
!
  integer(i4)               :: nmonth
  real(dp)                  :: rmonth
  character(len=60)         :: cdatetime
  character(len=10)         :: ctime
  character(len=9)          :: months(12)
  character(len=8)          :: cdate
  character(len=3)          :: cmonth
!
  data months/'January','February','March','April','May', &
              'June','July','August','September','October', &
              'November','December'/
!
  if (ioproc) then
!
!  Date stamp run
!
    call date_and_time(cdate,ctime)
    !  Initialise strings
    cdatetime = ' '
    !  Hours
    cdatetime(1:2) = ctime(1:2)
    cdatetime(3:3) = ':'
    !  Minutes
    cdatetime(4:5) = ctime(3:4)
    cdatetime(6:6) = '.'
    !  Seconds
    cdatetime(7:8) = ctime(5:6)
    !  Day
    cdatetime(10:11) = cdate(7:8)
    cmonth(1:2) = cdate(7:8)
    cmonth(3:3) = '.'
    call wtof(cmonth,rmonth,3_i4,3_i4)
    nmonth = nint(rmonth)
    if (nmonth.lt.10) cdatetime(10:10) = ' '
    nmonth = mod(nmonth,10_i4)
    if (nmonth.eq.1.and.rmonth.ne.11.0_dp) then
      cdatetime(12:13) = 'st'
    elseif (nmonth.eq.2.and.rmonth.ne.12.0_dp) then
      cdatetime(12:13) = 'nd'
    elseif (nmonth.eq.3.and.rmonth.ne.13.0_dp) then
      cdatetime(12:13) = 'rd'
    else
      cdatetime(12:13) = 'th'
    endif
    !  Month
    cmonth(1:2) = cdate(5:6)
    cmonth(3:3) = '.'
    call wtof(cmonth,rmonth,3_i4,3_i4)
    nmonth = nint(rmonth)
    cdatetime(15:24) = months(nmonth)
    !  Year
    cdatetime(26:29) = cdate(1:4)
    !  Write out strings
    if (imode.le.1) then
      write(channel,'(/,''  Job Started  at '',a60,/)') cdatetime
    elseif (imode.eq.2) then
      write(channel,'(''  Written at '',a60,/)') cdatetime
    else
      write(channel,'(''  Job Finished at '',a60,/)') cdatetime
    endif
  endif

  end subroutine datetimechannel

!****************************************************************************!

  subroutine init_pdf
!
!  Initialises PDF variables (previously in initial.f)
!
  implicit none
!     
!  PDF logicals 
!     
  lcoreinfo = .false.
  lfreqcut = .false.
  lkeepcut = .false.
  lmakeeigarray = .false.
  lneutronall = .false.
  lpdfout = .false.
  lnoksym = .false.
  lnowidth = .false.
  lpartial = .false.
  lpdf = .false.
  lneutronele = .false.
  lelecurrent = .false.
  lelefile = .false.
!     
!  Reals and integers for PDFs: ers29
!   
  natomp = 0

  end subroutine init_pdf

!****************************************************************************!

  subroutine makeeigenarrays(eig_r, eig_i, frequency,m)
!
!  Takes the real and imaginary eigenvectors, and frequencyuencies
!  for the current wavevector (veck in common)
!  and store as polarisation vectors(e) as used by Willis
!  & Pryor, while putting the frequencies into rad/s
!  Written by Elizabeth Cope(ers29) 8th Feb 2005
!  4/07 modified to handle gamma point (sets eig_i to zero)
! 11/09 all rephasing removed from code
!
  use constants
  implicit none
!
!  Dummy variables
!
  integer(i4)                                  :: m
  real(dp),             intent(in)             :: frequency(*)
  real(dp),             intent(in)             :: eig_r(m,*)
  real(dp), optional,   intent(in)             :: eig_i(m,*)
!
!  frequency in wavenumbers, for each mode
!
!  Local variables
!
  complex(dpc) :: polar(3)       ! as given in gulp: _sigma_
  integer(i4)  :: ind, i, iatnumber
!
! epolar_array is allocated with (3[x,y,z], nu, atom, k)
! omega_array is allocated with (nu,k)
!
! if no eig_i then at gamma
!
! These are "e" eigenvectors. The dynamical matrix is phased by exp{-tau.r_j}
! where tau is the reciprocal lattice vector and r_j is the position of the
! atom within the unit cell.
!
! Note that the eispack diagonaliser sets the bottom right eigenvector component
! Therefore, "shrink" shifted to be gamma centred, allowing e(-k) = e*(k)
!
  if (present(eig_i)) then !not at gamma point, so complex
    atomloop_ng : do ind=0, (nmodes-1), 3 !steps of three
      iatnumber = (ind+4)/3  ! holding the "atom number"
      iloop_ng: do i=1, nmodes ! loop over all 3n branches
        !     set up complex vector
        polar(1) = cmplx(eig_r(ind+1, i), eig_i(ind+1, i))
        polar(2) = cmplx(eig_r(ind+2, i), eig_i(ind+2, i))
        polar(3) = cmplx(eig_r(ind+3, i), eig_i(ind+3, i))
!! could do this to convert to sigma, but unecesary as only using 1 BZ
!!        polar = polar*euler(dot_product(veckcart,cartvec(iatnumber,1:3)))
        !     put polar into common epolar_array
        epolar_array(1:3,i,iatnumber, icurrentk)=polar(1:3)
        !     put into omega array
        omega_array(i,icurrentk) = frequency(i)*cmtorads
      enddo iloop_ng
    enddo atomloop_ng
  else ! at gamma point
    atomloop : do ind=0, (nmodes-1), 3 !steps of three
      iatnumber = (ind+4)/3  ! holding the "atom number"
      iloop: do i=1, nmodes ! loop over all 3n branches
        !     set up complex vector
        polar(1) = cmplx(eig_r(ind+1, i), 0.0_dp)
        polar(2) = cmplx(eig_r(ind+2, i), 0.0_dp)
        polar(3) = cmplx(eig_r(ind+3, i), 0.0_dp)
        !     put polar into common epolar_array
        epolar_array(1:3,i,iatnumber, icurrentk)=polar(1:3)
        !     put into omega array
        omega_array(i,icurrentk) = frequency(i)*cmtorads
      enddo iloop
    enddo atomloop
  endif

  end subroutine makeeigenarrays

!****************************************************************************!

  subroutine makeintvector(realvector, intvector)
!
!  subroutine makeintvector written by Elizabeth Cope (ers29)
!  march 2006 to find nearest integer vector
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), dimension(3)   :: intvector 
  real(dp), dimension(3)      :: realvector 
!
!  Local variables
!
  integer(i4)                 :: n=0
  logical                     :: lnotint=.true.
  real(dp)                    :: test = 1E-5
  integer(i4)                 :: maxn = 100
  real(dp)                    :: na,nb,nc

  do while (lnotint)
    n = n + 1
    if (n.gt.maxn) exit !give up and round things
    na =abs(real(n)*realvector(1))
    nb =abs(real(n)*realvector(2))
    nc =abs(real(n)*realvector(3))
    if (((na - int(na))<test).and.((na - int(na))<test).and.((na - int(na))<test)) lnotint = .false.
  enddo

  intvector(1) = nint(na) 
  intvector(2) = nint(nb) 
  intvector(3) = nint(nc) 

  end subroutine makeintvector

!****************************************************************************!

  subroutine masses(npatptr,iok)
!
!  Produces arrays containing atomic information: 
!  masses and neutron scattering details
!  Normally within GULP all masses and other elemental information is looked up 
!  each time via the atomic number, taking account of partial occupancies etc. 
!  To make the PDF calculations smoother, particually when using partial
!  occupancies, the neutron module contains code to make a set of arrays 
!  (for the current configuration) containing relevant elemental information 
!  in the internal (core) atom order.
!  This is particaully important with partial occupancy, as for phonon 
!  calculations a mean field atom is created, and only counted once. 
!  (see neutron module subroutine masses.) 
!
!  Thus this routine makes the following arrays available:
!	arraymasses_kg(ncores)         units kgmol
!	arraymasses_kgmol(ncores)      units kg
!	bbar_cores(ncores)             units Barn^(1/2)
!	bbar_cores_A(ncores)           units A
!	siginc_cores(ncores)           units Barn
!	siginc_cores_A2(ncores)        units A^2
!       siglengthbysqrtmass(ncores)    units(A/sqrt(kg)) 
!       bbarbysqrtmass(ncores)         units(A/sqrt(kg)) 
!
!  written by Elizabeth Cope (ers29)
!     
!  nru = fortran channel for reading input
!  ncurr contains current configuration number  ers29
!  07/07 reworked to use partial module ers29
!
  use constants
  use current
  use element
  use partial, only : ncfoc, iocptr
  use shell,   only : ncore
  implicit none
!
!  Dummy variables
!
  integer(i4), intent(in)                  :: npatptr(numat)
  !     npatptr = pointer from nphonat to numat reference
  integer(i4), intent(out)                 :: iok  !0 means OK
!
!  Local variables
!
  integer(i4) :: i, ii,ni, icores
  real(dp)    :: rmassers29(ncore), rbbar(ncore), rsiginc(ncore)
  real(dp)    :: rmassers29i, rbbari, rsiginci
!
  iok = 0 !at the moment, all is OK
!
!  Calculate  masses, bbars and sigincs
!
  do i = 1,ncore
    rmassers29(i) = 0.0_dp
    rbbar(i) = 0.0_dp
    rsiginc(i) = 0.0_dp
  enddo
  do i = 1,ncore
    ii = iocptr(i)
    ni = nat(npatptr(i))
    rmassers29i = atmass(ni)*occuf(npatptr(i))
    rbbari = bbar(ni)*occuf(npatptr(i))
    rsiginci = siginc(ni)*occuf(npatptr(i))
    if (rmassers29i.eq.0.0_dp) then
      call outerror('mass of element '//atsym(ni)//' is zero',0_i4)
      iok = 1             !will catch error within phonon now
      return
    endif
    if (rbbari.eq.0.0_dp) then
      call outerror('bbar of element '//atsym(ni)//' is zero',0_i4)
      iok = 1             !will catch error within phonon now
      return
    endif
    if (rsiginci.eq.0.0_dp) then
      call outerror('siginc of element '//atsym(ni)//' is zero',0_i4)
      iok = 1             !will catch error within phonon now
      return
    endif
    rmassers29(ii) = rmassers29(ii) + rmassers29i
    rbbar(ii) = rbbar(ii) + rbbari
    rsiginc(ii) = rsiginc(ii) + rsiginci
  enddo

  do i = 1,ncfoc              !check none have been left as zero
    if (rmassers29(i).eq.0.0_dp) then
      call outerror('site  has total mass of zero in phonon',0_i4)
      call stopnow('phonon:masses')
    endif
    if (rbbar(i).eq.0.0_dp) then
      call outerror('site  has total bbar of zero in phonon',0_i4)
      call stopnow('phonon:masses')
    endif
    if (rsiginc(i).eq.0.0_dp) then
      call outerror('site  has total siginc of zero in phonon',0_i4)
      call stopnow('phonon:masses')
    endif
  enddo

  do icores = 1,ncfoc
    arraymasses_kgmol(icores) = rmassers29(icores)*(1.0D-3) !kgmol
    arraymasses_amu(icores) = rmassers29(icores)*(1.0d0) !g mol = amu
    arraymasses_kg(icores) =arraymasses_kgmol(icores)/avogadro ! kg
    bbar_cores(icores) = rbbar(icores) !bn^(1/2)
    siginc_cores(icores) = rsiginc(icores) !bn
  enddo
  bbar_cores_A = bbar_cores*(1.0D-4)
  siginc_cores_A2 = siginc_cores*(1.0D-8)
  siglengthbysqrtmass = sqrt(siginc_cores_A2/(4.0*pi*arraymasses_kg)) !ers29 2/09
  bbarbysqrtmass = bbar_cores_A/sqrt(arraymasses_kg) !ers29 2/09
  iok = 0 ! all OK
  return

  end subroutine masses

!****************************************************************************!

  function mod_vector(vector)
!
!  to find the mod of a 3 dimension vector
!  written by Elizbeth Cope(ers29) 31st March 2005
!
  use datatypes
  implicit none
  real(dp)               :: mod_vector
  real(dp), dimension(3) :: vector

  mod_vector = sqrt(vector(1)**2 + vector(2)**2 + vector(3)**2)

  end function mod_vector

!****************************************************************************!

  subroutine pdfword(nru,word,lwordok,iline,line,l55,l1000,ncurr)
!     
!  Processes input for neutron related words
!  written by Elizabeth Cope (ers29)
! 3/07 now put the words into lowercase
! 4/07 line processing moved to nextline subroutine
! 11/07 Q and w binning now centre-anchored (ers29)
!     
!  nru = fortran channel for reading input
!  ncurr contains current configuration number
!     
  use configurations
  use control
  use dispersion
  use gulpinput
  use iochannels
  use ksample
  use parallel
  use phonout

  implicit none
!     
!  Passed variables
!     
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: iline
  integer(i4)                  :: ncurr
  integer(i4)                  :: nru
  logical                      :: lwordok
  logical                      :: l55
  logical                      :: l1000
!     
!  Local variables
!     
  real(dp)                     :: a
  logical                      :: lneutron
  logical                      :: lneutronwordok
  character(len=maxlinelength) :: warningstring
!
  lwordok = .false. ! when we start
  if (lwordok) then
    return
  endif
!
!  Check haven't already got a neutron block
!
  if (lneutronall) then
    call outerror("Have already set 'pdf all' block .to on all configurations",0_i4)
    call stopnow("pdfword")
  endif
!
!  Read in neutron block
!
  lneutronwordok = .false. ! at the start should be false
  if (index(words(1),'pdf').eq.1) then
    lneutron = .true.      ! we are in a neutron input block
    if (index(words(2), 'all').eq.1) then
      lneutronall = .true.
    endif
    line = '  '        
    call nextline(nru,line,iline, .true.) !read and process next line
    if (index(words(1),'end').eq.1) lneutron = .false. 
    do while (lneutron)
      if (ndimen(ncurr).ne.3) then
        call outerror('Neutron scattering routines can only be used with three dimensions',iline)
        call stopnow('neutronword')
      endif
!     
!  Frequency input/output option
!     
      if (index(words(1), 'unit').ne.0) then
        if ((index(words(2), 'freq').ne.0)) then
          if (index(words(3),'rad').ne.0)  then
            lunitrad(ncurr) = .true.
            unitsnamecfgs(ncurr) = "rad/s"
          elseif (index(words(3),'thz').ne.0) then
            lunitthz(ncurr) = .true.
            unitsnamecfgs(ncurr) = "THz"
          elseif ((index(words(3),'wav').ne.0).or.(index(words(3),'cm').ne.0)) then
            lunitcm(ncurr) = .true.
            unitsnamecfgs(ncurr) = "cm**-1"
          elseif (index(words(3),'mev').ne.0) then
            lunitmev(ncurr)= .true.
            unitsnamecfgs(ncurr) = "meV"
          else          !none set 
            call outerror('possible freq unit options abbreviations: rad, thz, wav (or cm), mev',iline)
            call stopnow('neutronword')
          endif
        else
          call outerror('Only frequency input/output units can be adjusted at present',iline)
          call stopnow('pdfword')
        endif
        lneutronwordok = .true.
      endif
!     
!  Output options
!     
      if (index(words(1), 'outp').eq.1) then
        call genword(nru,words(1),lwordok,iline,line,l55,l1000,.false.,.false.,ncurr)
        if (lwordok) lneutronwordok = .true.
      endif
!     
!  PDF inputs
!     
      if (lpdf) then
        if (index(words(1),'rmax').eq.1) then
          rmaxcfgs(ncurr) = floats(1)
          lneutronwordok = .true.
        endif
        if (index(words(1),'rbins').eq.1) then
          nrbinscfgs(ncurr) = int(floats(1))
          lneutronwordok = .true.
        endif
        if (index(words(1),'wmax').eq.1) then
          wmaxcfgs(ncurr) = floats(1)
          lneutronwordok = .true.
        endif
        if (index(words(1), 'wmin').eq.1) then
          wmincfgs(ncurr) = floats(1)
          lneutronwordok = .true.
        endif
      endif
!
!  w information input in units as set by unit freq <>
!  now centre anchored (11/07 ers29)
!
      if (index(words(1),'wbin').eq.1) then !this line contains w info
        if (nfloat == 2) then !wmax, nw
          wmaxcfgs(ncurr) = floats(1) !upper limit of top bin NOT NAME
        else 
          if (nfloat == 3) then !wmin, wmax, nw
            wmincfgs(ncurr) = floats(1) !lower limit of first bin NOT NAME
            wmaxcfgs(ncurr) = floats(2) !upper limit of top bin NOT NAME
            ! ensure that min is always less than max
            if (wmaxcfgs(ncurr)<wmincfgs(ncurr)) then !swap
              a = wmincfgs(ncurr)
              wmincfgs(ncurr) = wmaxcfgs(ncurr)
              wmaxcfgs(ncurr) = a
            endif
          else 
            call outerror('w input data should be "wbins [<wmin>] <wmax> <nw>"',iline)
            call stopnow('pdfword')
          endif
        endif
        ! bin setup moved to sqw_setup
        lneutronwordok = .true.
      endif
!      
!     Emax input. 10.08 maps to wmax
!      
      if (index(words(1),'emax').eq.1) then !max energybin 
        if (nfloat ==1) then
          wmaxcfgs(ncurr) = floats(1) !in units specified by unit freq
        else 
          call outerror('Emax input data should be "Emax <Emax>" (units set with "unit freq")',iline)
          call stopnow('pdfword')
        endif
        if (.not.(wmaxcfgs(ncurr)>0)) then 
          call outerror('Emax should be greater than zero',iline)
          call stopnow('pdfword')
        endif
        lneutronwordok = .true.
      endif

      if (lneutronwordok) then !continue to next line
        call nextline(nru,line,iline, .true.) !read and process next line
        if (index(words(1),'end').eq.1) lneutron = .false. !will stop cycling
      else                !unrecognised word in neutron block
        write(warningstring,*) trim(adjustl(words(1)))," is unrecognised in neutron block without suitable keywords"
        call outerror(trim(adjustl(warningstring)),iline)
        call stopnow('pdfword')
      endif
    enddo

    lwordok = .true.
    lpdfout = .true.   !there is something to output
    if (lneutronall) call allconfigs(ncurr) !copy to all
  endif
  return

contains

  subroutine allconfigs(ncurr)         
!
!  This subroutine copied everything from this config to all configs for use with "pdf all"          
!
  use configurations, only: ncfg
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(in) :: ncurr
!
!  Local variables
!
  integer(i4)              :: i
!
  if (ioproc) write(ioout,*) "Copying current pdf block settings to all configurations"
  do i = 1,ncfg
!
!  units
!
    lunitrad(i) = lunitrad(ncurr)
    lunitthz(i) = lunitthz(ncurr)
    lunitcm(i) = lunitcm(ncurr)
    lunitmev(i) = lunitmev(ncurr)
    unitsnamecfgs(i) = unitsnamecfgs(ncurr)
!
!  PDF inputs
!
    rmaxcfgs(i) = rmaxcfgs(ncurr)
    nrbinscfgs(i) =  nrbinscfgs(ncurr)
    wmaxcfgs(i) =  wmaxcfgs(ncurr) ! upper limit (not name)
    wmincfgs(i) =  wmincfgs(ncurr) ! lower limit (not name)
  enddo
  end subroutine allconfigs

  end subroutine pdfword

!****************************************************************************!

  subroutine nrelat_setup(n)
!
!  Create a pointer array for cores (only), relating core number to asym
!  core number, and handling partial occupancies
!
!  Written by Elizabeth Cope (ers29) 2005
!
  use element, only : maxele
  use current !contains nrelat etc
  implicit none

  integer(i4) :: icorecount
  integer(i4) :: inat,  n,i
  icorecount = 0

  do i = 1,n
    inat = iatn(nrelat(i)) ! use pointer to asym atom
    if ((inat .le. maxele).and. (inat .ne. 0)) then ! it must be a core
      icorecount = icorecount + 1 !increment pointer
      if (icorecount .gt. n) then !this should never happen
        call outerror("Have more cores than  nphonatc",0_i4)
        call stopnow("nrelat_setup")
      endif
      nrelat_cores(icorecount) = nrelat(i) 
    endif
  enddo

  end subroutine nrelat_setup

!****************************************************************************!

  subroutine nullpointer_neutron()
  implicit none
  nullify(lshrinkset)
  nullify(ldispersionset)
  nullify(lunitrad)
  nullify(lunitthz)
  nullify(lunitmev)
  nullify(lunitcm)
  nullify(nkpointscfg)
  nullify(nrbinscfgs)
  nullify(pdffiles)
  nullify(rmaxcfgs)
  nullify(unitsnamecfgs)
  nullify(wmaxcfgs)
  nullify(wmincfgs)
  end subroutine nullpointer_neutron

!****************************************************************************!

  subroutine nextline(chan,line,iline,lnext,lend)      
!
!  read in next line, add to count, 
!  process line and put words in lower case
!
  use gulpinput
  implicit none
  integer(i4), intent(in)        :: chan !channel
  integer(i4), intent(inout)     :: iline !count
  character(len=maxlinelength),intent(inout)   :: line !line contents
  logical, intent(in) :: lnext !should I read next line, or process input line
  logical, optional, intent(out) :: lend !is this the end of file
  integer(i4) :: iw ,ierr      !loop variable

  if (lnext) then
!
!  Check for end of input file
!
    read(chan,'(a)',iostat=ierr) line !read in next line
    if (ierr<0) then !end of file
      lend = .true.
      return
    endif
    iline = iline + 1
  endif
  call linepro(chan,line,iline)
  do iw = 1,nword         ! put words in lower case
    call stolc(words(iw),80_i4)
  enddo
  end subroutine nextline

!****************************************************************************!

  subroutine outpdfin
!     
!  output for PDF related settings
!  must be called from gulpsetup even when no output to perform checks without writing
!
!  NB coversion of frequencies to internal units perfomed in here!
!
!  written by Elizabeth Cope (ers29) 2006
!  modified 31.12.06 to be called for checks when ioproc false.   
!  4/07 CML call moved to gulp_setup (before call to outpdfin)
!  4/07 modified to use sperate configuration output
!     

  use configurations, only : ncfg !total number of configs
  use general,        only : elefile
  use parallel
  implicit none
!
!  Local variables
!
  character(len=120) :: warningstring !extended 2/4/07
  integer(i4)        :: i

  if (ioproc) then
    write(ioout, '(/)')
    write(ioout,'("********************************************************************************")')
    write(ioout,'("*  PDF Input Settings                                                          *")')
    write(ioout,'("********************************************************************************")')
    write(ioout, '(/)')
  endif
  if (lneutronele.and.ioproc) then !modified 26th april
    if (lelecurrent) &
      write(ioout,*) "Neutron information sucessfully read from 'elefile' in current directory"
    if (lelefile)&
      write(ioout,*) "Neutron information sucessfully read from ", trim(adjustl(elefile))
  endif
  if (lneutronall.and.(ncfg>1)) then
    call outadvice("'pdf all' option acts on all configurations")
  endif

  if ((ncfg >1).and.(ioproc)) then
    write(ioout,'("********************************************************************************")')
    write(ioout,'("*  PDF parameters for each configuration ")')
    write(ioout,'("*")')
  endif
  do i = 1,ncfg
    if ((ncfg >1).and.(ioproc)) then
      write(ioout,'("*  Configuration ",i3)')i
      write(ioout,'("********************************************************************************")')
    endif
!  
!  PDF checks
!     
    if (lpdf) then         !check for shrink
      if (.not.(lshrinkset(i))) then
        write(warningstring,'("   To use PDF module need to use <shrink> option to set up Monkhorst-Pack grid")')
        call outerror(warningstring,0_i4)
        call stopnow('outpdfin')
      endif

      if (Rmaxcfgs(i) <= 0.0_dp) then ! check for Rmax
        call outerror ("rmax must be greater than zero",0_i4)
        call stopnow('outpdfin')
      endif

      if (nrbinscfgs(i) <= 0) then !check for Rbins 
        call outerror ("number of r bins must be greater than zero",0_i4)
        call stopnow('outpdfin')
      endif

      if (ioproc) then   !write out
        write(ioout,'(" Pair Distribution Function to be calculated up to ", F7.4, " A")') rmaxcfgs(i)
        write(ioout,'(I4, " rbins to be used.")') nrbinscfgs(i)
        if (wmaxcfgs(i) .ne. 0) write(ioout,*)" wmax = ",  wmaxcfgs(i),unitsnamecfgs(i)
        if (wmincfgs(i) .ne. 0) write(ioout,*)" wmin = ",wmincfgs(i),unitsnamecfgs(i)
      endif
    endif
!     
!  Now convert to internal units
!     
    call changefreq(.true.,i) !change freqs to use rad/s (internal units)
    if (ioproc) then
      if ((wmaxcfgs(i).ne.0) .or.(wmincfgs(i).ne.0)) write(ioout,'(/," Converting to internal frequency units:")')
      if (wmincfgs(i) .ne. 0) write(ioout,*)"wmin = ",wmincfgs(i),unitsnamecfgs(i)
      if (wmaxcfgs(i) .ne. 0) write(ioout,*)"wmax = ",  wmaxcfgs(i),unitsnamecfgs(i)
      write(ioout,"('********************************************************************************')")
    endif
  enddo

  return

  end subroutine outpdfin

!****************************************************************************!

  subroutine setcurk(k, lallk)
!
!  A subroutine to update veck and icurrentk and to convert to cartesian k
!
!  Written by Elizabeth Cope (ers29 2005)
!  10/06  modified to work with multiple configurations
!  05/07  modified to use lallk when called from phonon 
!         (#k out of list of all k
!         as opposed to #k out of list of k for this config)
!
  use ksample
  use current,    only: ncf
  implicit none 
!
!  Passed variables
!
  integer(i4) ,      intent(in) :: k ! k number
  logical,           intent(in) :: lallk ! is k wrt all k, or just this config?
!
!  Local variables
!
  integer(i4)                   :: kallk
  integer(i4)                   :: iconfigsk

  iconfigsk = 0  
  if (lallk) then ! input k is wrt all configs
    if (ncf>1) then
      iconfigsk = sum(nkpointscfg(1:(ncf-1)))
    endif
    icurrentk = k - iconfigsk ! icurrentk wrt to *this* config
    kallk = k ! this k is already out of all configs
  else
    if (ncf>1) then
      iconfigsk = sum(nkpointscfg(1:(ncf-1)))
    endif
    icurrentk = k ! already just wrt this config
    kallk = k + iconfigsk ! now wrt all config
  endif

  veck(1) = xkpt(kallk)
  veck(2) = ykpt(kallk)
  veck(3) = zkpt(kallk) 
  veckcart =  matmul(veck, kvT) !recip space conversion of frac to cart

  end subroutine setcurk

!****************************************************************************!

  subroutine setupcores(nphonatptr,iok)
!
!  Produces arrays containing atomic information for cores
!  handling partial occupancies
!
!  written by Elizabeth Cope (ers29)
!  07/07 reworked to use partial module
!     
  use partial,   only : ncfoc
  use constants, only : pi
  use current,   only : numat
  use parallel,  only : ioproc
  implicit none
!
!  Dummy variables
!
  integer(i4),  dimension(numat)      :: nphonatptr
  integer(i4)                         :: iok
!
!  Local variable
!
  integer(i4)                         :: i
!
  call nrelat_setup(natomfull) 
  call atomcord                ! produces an array of atomic coordinate vectors
  call masses(nphonatptr,iok)  ! set up lists of the core masses and cross sections
  if (iok.ne.0) then
    call outerror('error in masses',0_i4)
    call stopnow ('setupcores') ! error in masses, end safely
  endif
!
!  Now output core info (if keyword set)
!
  if (lcoreinfo.and.ioproc) then
    write(ioout,*) "(Core) Atomic information:"
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,ncfoc
      write(ioout,'("Atom ",i2,"(",a,"):")')  i, trim(nameslist(i))
      write(ioout,'("  Mass     = ", e10.4," kg")') arraymasses_kg(i) 
      write(ioout,'("  bbar     = ", e10.4," Ang")')  bbar_cores_A(i)
      write(ioout,'("  sigma_inc     = ",e10.4," Ang^2")') siginc_cores_A2(i)
      write(ioout,'("  Sigma_coh     = ",e10.4," Ang^2")') 4*pi*bbar_cores_A(i)*bbar_cores_A(i)
      write(ioout,'("  Cartesian  position = ",3(f12.8,2x),"Ang")') atomvec(i,1:3)
      write(ioout,'("  Fractional position = ",3(f12.8,2x))') fracvec(i,1:3)
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  end subroutine setupcores

!****************************************************************************!

  subroutine setuppdfphonon(nllkpt,mcv,nphonatc,nphonatptr,ncurr)
!
!  The collection of PDF-related initialisations and function calls performed at start of phonon 
!  
!   4/07 Created by Elizabeth Cope (ers29)
!   6/09 natomp setup moved outside condition to avoid crash in cml
!
  use parallel, only : ioproc
  use partial,  only : ncfoc
  implicit none
!
!  Passed variables
!
  integer(i4)  ,intent(in)                             :: nllkpt
  integer(i4)  ,intent(in)                             :: mcv
  integer(i4)  ,intent(in)                             :: nphonatc
  integer(i4),  dimension(*), intent(in)               :: nphonatptr
  integer(i4)  ,intent(in)                             :: ncurr !current config number
!
!  Local variables
!
  integer(i4)                                          ::iok
!
!  Set up value of natomp for use later
!
  natomp = ncfoc 
  if (lmakeeigarray.or.lcoreinfo) then 
    nkpoints = nllkpt              ! pass into neutron module
    nkpointscfg(ncurr) = nkpoints
    nmodes = mcv
    natomfull = nphonatc           ! carry number of cores (counting partials individually)
    call allocateneutronarrays
    call setupcores(nphonatptr,iok)
    if (iok.ne.0) then !catch errors
      call outerror('Error when setting up core information',0_i4)
      call stopnow('setuppdfphonon')
    endif
  endif
  if (ioproc) then
    write(ioout,'(''  Start of PDF calculation'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    call gflush(ioout) !flush to buffer after allocating 
  endif

  end subroutine setuppdfphonon

!****************************************************************************!

  function euler(x)
!
!  Euler function for complex numbers
!  takes a real variable x and returns the complex
!     exp{ix} = cos(x) + i sin(x)
!
!  7/05 written by Elizabeth Cope (ers29)
!  11/06 modified for GULP3 (ers29)
!   4.07 made .f90
!
    use datatypes
    implicit none

    complex(dpc)             euler ! function type
    real(dp), intent(IN)  :: x     !input

    euler = cmplx( cos(x),sin(x) )
  end function euler

!****************************************************************************!

end module m_pdfneutron
