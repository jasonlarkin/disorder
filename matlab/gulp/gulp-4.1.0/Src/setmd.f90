  subroutine setmd(lmain)
!
!  Initialises the control parameters and arrays prior to a
!  molecular dynamics run.
!
!  lmain - if .true. then all tasks to be performed.
!          if .false. then this is a restart with a changed
!          set of conditions.
!
!   2/97 Modification added (JRH) for GC and shell model MD
!   3/97 Modifications for breathing shells in MD added
!   7/97 Correction to target KE added
!   7/98 History file option added
!  11/00 Shell velocities initialised to zero explicitly
!  12/00 Generalised for more dimensionalities
!   2/01 Dimensionality added to trajectory file format
!   6/01 List based methods turned off for more three body potentials
!  10/01 For zero dimensional systems, call to mdfunct is added so
!        that the first derivatives are correct.
!  10/02 Constrain force sums zeroed
!  12/02 Molecule internal velocities initialised to zero
!   4/04 Modifications for output of pressure tensor file added
!   4/04 Handling of case where user inputs initial velocities added
!   6/04 Call to optewald removed since it causes variability in the
!        results
!  11/04 Pi accessed from module
!   1/05 Logical to compute interatomic vector table added in call
!        to mdfunct
!   4/05 Mods for cosh-spring added
!   7/05 ratiom made species specific
!   8/05 Arc file header PBC string corrected for 1- and 2-D cases
!   5/06 Mass now uses species values
!   5/07 Breathing shell pointer data moved to module
!   7/07 suminertia initialised
!   1/08 random -> GULP_random
!   3/08 Saving of initial volume added for mdmaxvolume option
!   7/08 Initialisation of arc file moved to a subroutine
!   7/08 Number of atoms added to header for history file
!   1/09 Name of new integrator changed to stochastic
!   6/09 Module name changed from three to m_three
!   8/10 p_iso and p_flx now only set if not read in.
!   9/10 lmain input flag added
!   8/11 Output of plumed details added
!   8/11 Call to mdfunct modified to pass dummy step number
!  11/11 Tethering enabled with stochastic integrator
!   3/12 Output of DCD file added
!   5/12 Initialisation of sum of atomic stresses added
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
  use constants, only : pi, kjmtoev
  use control
  use current
  use derivatives
  use dump
  use element
  use files
  use four
  use general
  use genetic
  use iochannels
  use kspace
  use mdlogic
  use moldyn
  use molecule
  use m_pr
  use m_three
  use optimisation
  use parallel
  use partial
  use plumed
  use pr_random_module,   only : gauss_rand, init_genrand
  use shell
  use shellextrapolation, only : lextrapolateshells, maxextrapol
  use six
  use species
  use two
  use velocities
  implicit none
!
!  Passed variables
!
  logical, intent(in) :: lmain
!
!  Local variables
!
  character(len=4)   :: lab
  integer(i4)        :: i
  integer(i4)        :: iseed_pr
  integer(i4)        :: icp
  integer(i4)        :: ind
  integer(i4)        :: isp
  integer(i4)        :: keytrj
  integer(i4)        :: n
  integer(i4)        :: nati
  integer(i4)        :: ncellfree
  integer(i4)        :: nn
  integer(i4)        :: npt
  integer(i4)        :: nrms
  integer(i4)        :: nsi
  logical            :: lcellanglefree
  logical            :: lfound
  logical            :: lgauss
  logical            :: lwarn
  real(dp)           :: cellmax
  real(dp)           :: cutmax
  real(dp)           :: efct
  real(dp)           :: fc
  real(dp)           :: GULP_grandom
  real(dp)           :: GULP_random
  real(dp)           :: rmi
  real(dp)           :: rms
  real(dp)           :: rrmi
  real(dp)           :: stdev
  real(dp)           :: stdfct
  real(dp)           :: stepsq
  real(dp)           :: sumvelx
  real(dp)           :: sumvely
  real(dp)           :: sumvelz
  real(dp)           :: tperiod
  real(dp)           :: tstp
  real(dp)           :: vfct
  real(dp)           :: vol
  real(dp)           :: volume
  real(dp)           :: waveno
!
  lgauss = (index(keyword,'gaus').ne.0) 
  leprint = (index(keyword,'debu').ne.0.and.ioproc)
!
!  Check that temperature is greater than zero
!
  if (temperature.lt.0.5_dp) then
    call outerror('Temperature is too small in molecular dynamics',0_i4)
    call stopnow('setmd')
  endif
  if (lmain) then
    llist3 = (index(keyword,'noli').eq.0)
    llist4 = (index(keyword,'noli').eq.0)
    llist6 = (index(keyword,'noli').eq.0)
!
!  Check that ensemble is consistent with degrees of freedom
!
    if (nensemble(ncf).eq.3.and.index(keyword,'conp').eq.0) then
      call outerror('conp must be specified for an NPT MD run',0_i4)
      call stopnow('setmd')
    endif
    if (nensemble(ncf).eq.4.and.index(keyword,'conp').eq.0) then
      call outerror('conp must be specified for an NPH MD run',0_i4)
      call stopnow('setmd')
    endif
    if (nensemble(ncf).lt.3.and.index(keyword,'conp').ne.0) then
      call outerror('conp cannot be specified for an NVE or NVT MD',0_i4)
      call stopnow('setmd')
    endif
!
!  Disable variable cell dynamics for surfaces and polymers
!
    if (nensemble(ncf).eq.3.and.ndim.eq.2) then
      call outerror('NPT ensemble not yet allowed for MD of surfaces',0_i4)
      call stopnow('setmd')
    endif
    if (nensemble(ncf).eq.3.and.ndim.eq.1) then
      call outerror('NPT ensemble not yet allowed for MD of polymers',0_i4)
      call stopnow('setmd')
    endif
    if (nensemble(ncf).eq.4.and.ndim.eq.2) then
      call outerror('NPH ensemble not yet allowed for MD of surfaces',0_i4)
      call stopnow('setmd')
    endif
    if (nensemble(ncf).eq.4.and.ndim.eq.1) then
      call outerror('NPH ensemble not yet allowed for MD of polymers',0_i4)
      call stopnow('setmd')
    endif
!
    if (nmdintegrator.eq.1) then
      if (ioproc) then
        write(ioout,'(/,''  Equations of motion will be integrated using Gear''''s algorithm.'')')
      endif
      if (nensemble(ncf).ne.1) then
        call outerror('Gear algorithm only available for NVE',0_i4)
        call stopnow('setmd')
      endif
    elseif (nmdintegrator.eq.2) then
      if (ioproc) then
        write(ioout,'(/,''  Equations of motion will be integrated using the velocity Verlet algorithm.'')')
      endif
      if (nensemble(ncf).gt.2) then
        call outerror('Velocity Verlet only available for NVE/NVT',0_i4)
        call stopnow('setmd')
      endif
    elseif (nmdintegrator.eq.3) then
      if (ioproc) then
        write(ioout,'(/,''  Equations of motion will be integrated using the leapfrog Verlet algorithm.'')')
      endif
    elseif (nmdintegrator.eq.4) then
      if (ioproc) then
        write(ioout,'(/,''  Equations of motion will be integrated using stochastic algorithm.'')')
      endif
    endif
!
!  Check that minimum image and cell are compatible if specified
!
    if (index(keyword,'mini').gt.0.and.ndim.gt.0) then
      select case(ndim)
      case(1)
        cellmax = a
      case(2)
        cellmax = max(a,b)
      case(3)
        cellmax = max(a,b,c)
      end select
      cutmax = max(sqrt(rmx2),rpmax)
      if (cutmax.gt.cellmax) then
        nwarn = nwarn + 1
        call outwarning('Cell is smaller than cut-offs. Minimum images differs from exact',0_i4)
        if (sqrt(rmx2).gt.cellmax.and.rpmax.lt.cellmax) then
          call outadvice('Try increasing rspeed to reduce real space cutoff for minimum image')
        endif
      endif
    endif
!
!  Check list based methods are OK for three-body method
!
    if (llist3) then
      lwarn = .false.
      do i = 1,nthb
        if (nthrty(i).eq.2.or.nthrty(i).eq.3.or.nthrty(i).eq.4.or.nthrty(i).eq.5.or.nthrty(i).eq.8) then
          if (.not.lwarn) then
            llist3 = .false.
            call outwarning('List-based method turned off due to 3-body potential types',0_i4)
          endif
          lwarn = .true.
        endif
      enddo
    endif
  endif
!
  efct = 9648.462267_dp
  refct = 1.0_dp/efct
  vfct = 0.1_dp*8.31441_dp
  rvfct = 1.0_dp/vfct
  pfct = efct/(60.2205_dp*3.0_dp)
  smdfct = 0.5_dp*vfct*refct
!
!  Initialise sums
!
  naverpt = 0
  sumvir = 0.0_dp
  sumvsq = 0.0_dp
  sumener = 0.0_dp
  sumtem = 0.0_dp
  sumcst = 0.0_dp
  sumcons = 0.0_dp
  sumlambdaR = 0.0_dp
  sumlambdaV = 0.0_dp
  sumstress(6) = 0.0_dp
  suminertia = 0.0_dp
  if (nensemble(ncf).eq.3.or.nensemble(ncf).eq.4) then
    sumacell = 0.0_dp
    sumbcell = 0.0_dp
    sumccell = 0.0_dp
    sumalpcell = 0.0_dp
    sumbetcell = 0.0_dp
    sumgamcell = 0.0_dp
    sumvol = 0.0_dp
  endif
  if (latomicstress) then
    sumatomicstress(1:nstrains,1:numat) = 0.0_dp
  endif
!
  if (lmain) then
    if (.not.ladiabatic.and.nshell.ne.0) then
!
!  Set shell mass ratios for atoms
!
      call setratiom
!
!  Calculate core/shell "wave numbers" (wn=1/(2*pi*c)*sqrt(k/M), where
!  M is the reduced mass x*(1-x)*m)
!
      if (ioproc) then
        write(ioout,'(/,''  Core-shell wave numbers:'',/)')
      endif
      tperiod = 1000.0_dp
      do i = 1,npote
        npt = nptype(i)
        if ((npt.eq.5.or.npt.eq.8.or.npt.eq.33).and.nspec2(i).gt.maxele) then
!
!  Find shellmass ratio type
!
          lfound = .false.
          rms = 0.0_dp
          nrms = 0
          do while (nrms.lt.nratiomspec.and..not.lfound)
            nrms = nrms + 1
            if (nspec1(i).eq.natratiomspec(nrms).and.(nptyp1(i).eq.ntypratiomspec(nrms).or.ntypratiomspec(nrms).eq.0)) then
              lfound = .true.
              rms = ratiomspec(nrms)*(1.0_dp - ratiomspec(nrms))
            endif
          enddo
!
!  If no shellmass ratio has been found for a shell potential skip output - presence of a particle in the
!  system with near zero mass has already been checked in setratiom.
!
          if (lfound) then
!
!  The following is approximate for npt = 33 since the force constant is
!  distance dependent, but it gives a good idea in the r = 0 limit
!
            waveno = 1.0_dp/(2.0_dp*pi*2.99792458d10)*sqrt(twopot(1,i) &
                /(rms*atmass(nspec1(i))*1.036435d-28))
            tperiod = min(tperiod,100.0_dp/waveno*2.99792458_dp)
            call label(nspec1(i),nptyp1(i),lab)
            if (ioproc) then
              write(ioout,'(4x,a4,'': '',f10.2,'' cm**(-1)'')') lab,waveno
            endif
          endif
        endif
      enddo
      if (ioproc) then
        write(ioout,'(/,''  Above wave numbers must be larger than the vibrational modes for this system.'',/)')
      endif
      if (tstep(ncf).gt.tperiod) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''  **** Warning - time step is too large for integration of core/shell motion, ****'')')
          write(ioout,'(''  **** shortest vibrational period is '',f7.3,'' fs ****'',/)') 1000._dp*tperiod
        endif
      endif
    endif
  endif
!*****************************
!  Length of run parameters  *
!*****************************
  tstp = tstep(ncf)
  if (tstp.eq.0.0_dp) then
    call outerror('timestep in MD run is zero',0_i4)
    call stopnow('setmd')
  endif
  stepsq = tstp*tstp
  stpsqh = 0.5_dp*stepsq*efct
  refct = refct/stepsq
  rvfct = rvfct/stepsq
  if (nmdeq(ncf).eq.0) nmdeq(ncf) = nint(tmdeq(ncf)/tstp)
  if (nmdprod(ncf).eq.0) nmdprod(ncf) = nint(tmdprod(ncf)/tstp)
  if (nmdsamp(ncf).eq.0) nmdsamp(ncf) = nint(tmdsamp(ncf)/tstp)
  if (nmdwrite(ncf).eq.0) nmdwrite(ncf) = nint(tmdwrite(ncf)/tstp)
  if (nmdsamp(ncf).eq.0) then
    nmdsamp(ncf) = 20
    tmdsamp(ncf) = 20.0_dp*tstp
  endif
  if (nmdwrite(ncf).eq.0) then
    nmdwrite(ncf) = 20
    tmdwrite(ncf) = 20.0_dp*tstp
  endif
  if (tmdscale(ncf).eq.0.0_dp) tmdscale(ncf) = nmdeq(ncf)*tstp
  if (lmain) then
!******************
!  General setup  *
!******************
    call setup(.true.)
    if (nthb.gt.0.and.llist3) call setlist3
    if (nfor.gt.0.and.llist4) call setlist4
    if (nsix.gt.0.and.llist6) call setlist6
!
!  Check that breathing cores/shells are not present
!
    if (nbsmat.gt.0.and..not.ladiabatic) then
      call outerror('For breathing species MD use minimisation algorithm',0_i4)
      call stopnow('setmd')
    endif
!
!  Trap MD with partial occupancies as it won't work properly yet
!
    do i = 1,numat
      if (occuf(i).lt.1.0_dp) then
        call outerror('MD not allowed with partial occupancies',0_i4)
        call stopnow('setmd')
      endif
    enddo
!
!  Set up array of masses (shells get their masses assigned
!  here, cores get the full ion mass here and their mass is
!  corrected after the velocities have been assigned)
!  
!  cellmass = sum of all masses within the unit cell
!
    cellmass = 0.0_dp
    do i = 1,numat
      nati = nat(i)
      if (nati.gt.maxele) then
        nsi = nspecptr(ncsptr(i))
        mass(i) = ratiom(i)*massspec(nsi)
        if (ratiom(i).eq.0.0_dp) then
          rmass(i) = 0.0_dp
          goto 10
        endif
      else
        nsi = nspecptr(i)
        mass(i) = massspec(nsi)*occuf(i)
      endif
      cellmass = cellmass + mass(i)
      rmass(i) = 1.0_dp/mass(i)
 10   continue
    enddo
!*****************************
!  Initialise cell pointers  *
!*****************************
    do i = 1,numat
      icosx(i) = 0
      icosy(i) = 0
      icosz(i) = 0
    enddo
!***********************
!  Set freezing flags  *
!***********************
    do i = 1,numat
      lopf(i) = .false.
    enddo
    do i = 1,nvar
      ind = iopt(i)
      if (ind.gt.nstrains) then
        ind = ind - (nstrains+1)
        ind = (ind/3) + 1
        lopf(ind) = .true.
      endif
    enddo
    if (nshell.ne.0.and.ladiabatic) then
      if (ioproc) then
        write(ioout,'(/,''  Shell masses are zero thus shell positions will be optimised each time step'')')
        write(ioout,'(''  until the forces on the shells are less than '',d10.5,'' or for a maximum of '')') sgtol
        write(ioout,'(2x,i3,'' iterations.'',/)') moptit+1
        if (lextrapolateshells) then
          write(ioout,'(''  Shell positions to be extrapolated to order = '',i4,/)') maxextrapol
        endif
      endif
    endif
!
!  Setup breathing shell pointer
!
    if (nbsmat.ne.0) then
      if (ioproc) then
        write(ioout,'(/,''  Radii will be optimised at each time step '',/)')
      endif
    endif
    nmoving = 0
    rnmoving = 0.0_dp
    do i = 1,numat
      if (lopf(i).and.nat(i).le.maxele.and..not.lfix(i)) then
        nmoving = nmoving + 1
        rnmoving = rnmoving + occuf(i)
      endif
    enddo
!
!  Check for one atom only frozen as residue from flag setting
!
    if (nmoving.eq.(numat-1).and..not.lopf(1)) then
      nmoving = numat
      rnmoving = rnmoving + occuf(1)
      lopf(1) = .true.
    endif
    if (nmdintegrator.eq.4) then
!
!  Set flags for new integrator
!
      ncellfree = 0
      lcellanglefree = .false.
      do i = 1,min(nvar,nstrains)
        ind = iopt(i)
        if (ind.le.nstrains) then
          ncellfree = ncellfree + 1
          if (ind.ge.4) lcellanglefree = .true.
        endif
      enddo
      lpr_stress   = .false.
      lpr_baro_flx = .false.
      lpr_baro_iso = .false.
      lpr_baro_ort = .false.
      if (ncellfree.eq.1.and..not.lcellanglefree) then
!
!  Isotropic case
!
        lpr_stress   = .true.
        lpr_baro_iso = .true.
      elseif (ncellfree.gt.1.and..not.lcellanglefree) then
!
!  Orthorhombic special case
!
        lpr_stress   = .true.
        lpr_baro_flx = .true.
        lpr_baro_ort = .true.
      elseif (ncellfree.ne.0) then
!
!  General NPT
!
        lpr_stress   = .true.
        lpr_baro_flx = .true.
      endif
!
!  Set thermostat flags
!
      if (nensemble(ncf).eq.2.or.nensemble(ncf).eq.3) then
        lpr_thermo = .true.
      else
        lpr_thermo = .false.
      endif
      lpr_globalt = (index(keyword,'nogl').ne.1.and.index(keyword,' nogl').eq.0)
    endif
!
!  Calculate degrees of freedom
!
!  = 3 x number of atoms which are free to move -3 for translation
!    - 3 if this is a finite system to remove rotation
!
!  Degrees of freedom for new integrator
!
    if (nmdintegrator.eq.4) then
      ndof_atm  = 3*nmoving - 3
      ndof_baro = 0
! Remove centre of mass motion
      if (ndim.eq.0) ndof_atm = ndof_atm - 3
      if (lpr_baro_iso) ndof_baro = 1
      if (lpr_baro_flx) ndof_baro = 9
      if (lpr_baro_ort) ndof_baro = 3
      if (ndof_atm.le.0) then
        call outerror('insufficient degrees of freedom in MD run',0_i4)
        call stopnow('setmd')
      endif
      ndof = ndof_atm
    else
!
!  Degrees of freedom for other integrators
!
      if (ndim.eq.0) then
        ndof = 3*nmoving - 6
      else
        ndof = 3*nmoving - 3
      endif
    endif
!
!  Check that there are some degrees of freedom
!
    if (ndof.le.0) then
      call outerror('insufficient degrees of freedom in MD run',0_i4)
      call stopnow('setmd')
    endif
  endif
!
!  Multiply temperature bath factor constant by number of mobile atoms
!
  smdfct = smdfct*dble(ndof)
  smdfctt = smdfct*temperature
!
!  Store initial volume for volume checks
!
  if (ndim.eq.3) then
    rmdvol0 = volume(rv)
  endif
!
!  Output plumed info
!
  if (lplumed) then
    if (ioproc) then
      write(ioout,'(/,''  PLUMED run with input from '',a50,/)') plumedfile(1:50)
    endif
  endif
!***********************************************
!  Initialise velocities if not input by user  *
!***********************************************
  if (.not.linputvelxyz) then
!
!  Create random velocities
!
    if (lgauss) then
!
!  Gaussian distribution
!
      stdfct = sqrt(0.831441_dp*temperature)*tstp
      do i = 1,numat
        if (lopf(i).and.nat(i).le.maxele.and..not.lfix(i)) then
          rmi = mass(i)
          stdev = stdfct/sqrt(rmi)
          velx(i) = GULP_grandom(iseed,stdev)
          vely(i) = GULP_grandom(iseed,stdev)
          velz(i) = GULP_grandom(iseed,stdev)
        else
          velx(i) = 0.0_dp
          vely(i) = 0.0_dp
          velz(i) = 0.0_dp
        endif
      enddo
    else
!
!  Linear distribution
!
      do i = 1,numat
        if (lopf(i).and.nat(i).le.maxele.and..not.lfix(i)) then
          velx(i) = GULP_random(iseed,2_i4)
          vely(i) = GULP_random(iseed,2_i4)
          velz(i) = GULP_random(iseed,2_i4)
        else
          velx(i) = 0.0_dp
          vely(i) = 0.0_dp
          velz(i) = 0.0_dp
        endif
      enddo
    endif
!***********************************************************
!  Molecule velocities  - remove internal motion at start  *
!***********************************************************
    if (nmol.gt.0.and.index(keyword,'nomol').ne.0) then
      do n = 1,nmol
        sumvelx = 0.0_dp
        sumvely = 0.0_dp
        sumvelz = 0.0_dp
        nn = 0
!
!  Sum velocities over all atoms in the molecule
!
        do i = 1,numat
          if (natmol(i).eq.n) then
            nn = nn + 1
            sumvelx = sumvelx + velx(i)
            sumvely = sumvely + vely(i)
            sumvelz = sumvelz + velz(i)
          endif
        enddo
!
!  Calculate average velocity of molecule
!
        sumvelx = sumvelx/dble(nn)
        sumvely = sumvely/dble(nn)
        sumvelz = sumvelz/dble(nn)
!
!  Set velocity of all atoms in the molecule to be the same initially
!
        do i = 1,numat
          if (natmol(i).eq.n) then
            velx(i) = sumvelx
            vely(i) = sumvely
            velz(i) = sumvelz
          endif
        enddo
      enddo
    endif
!*************************************************************
!  Correct velocities to remove linear and angular momentum  *
!*************************************************************
    call mdvelcor(ndim.eq.0,.false.,.false.)
!
!  Set shell velocities to match those of core
!
    if (nshell.gt.0.and..not.ladiabatic) then
      do i = 1,nshell
        isp = nshptr(i)
        icp = ncsptr(isp)
        velx(isp) = velx(icp)
        vely(isp) = vely(icp)
        velz(isp) = velz(icp)
      enddo
    endif
  endif
  if (lmain) then
!
!  Correct masses of cores if shells have masses, also adjust
!  velocities of shells to that of the corresponding cores
!  (core-shell relative velocity = 0) if shell motions are to
!  be integrated
!
    if (nshell.gt.0.and..not.ladiabatic) then
      do i = 1,nshell
        isp = nshptr(i)
        icp = ncsptr(isp)
        mass(icp) = ratiom(icp)*mass(icp)
        rmass(icp) = 1.0_dp/mass(icp)
      enddo
    endif
  endif
!**************************************
!  Initialise cell velocities if NPT  *
!**************************************
  if (.not.linputvelc) then
    if (nensemble(ncf).eq.3) then
      do i = 1,nstrains
        velc(i) = 0.0_dp
      enddo
    endif
  endif
  if (nensemble(ncf).eq.3.or.nensemble(ncf).eq.4) then
!
!  Initialise xcell which contains cell components
!
    if (ndim.eq.3) then
      xcell(1) = r1x
      xcell(2) = r1y
      xcell(3) = r1z
      xcell(4) = r2x
      xcell(5) = r2y
      xcell(6) = r2z
      xcell(7) = r3x
      xcell(8) = r3y
      xcell(9) = r3z
    elseif (ndim.eq.2) then
      xcell(1) = r1x
      xcell(2) = r1y
      xcell(3) = r2x
      xcell(4) = r2y
      do i = 5,9
        xcell(i) = 0.0_dp
      enddo
    elseif (ndim.eq.1) then
      xcell(1) = r1x
      do i = 2,9
        xcell(i) = 0.0_dp
      enddo
    endif
!
!  Calculate pressure friction factor divided by NkT
!  psfct is in (eV)-1, so P.V needs to be in eV
!
    psfct = qpres(ncf)*efct/(dble(numat)*vfct)
!
!  Scale by time step
!
    psfct = psfct*tstp
    psfctt = psfct/temperature
  endif
!
!  Calculate KE at desired temperature
!
  velfct = dble(ndof)*vfct*stepsq
  velfctt = velfct*temperature
  if (ndof.gt.0) then
    rvfct = rvfct/dble(ndof)
  else
    rvfct = 0.0_dp
  endif
!****************************************************
!  Copy parameters to variables for new integrator  *
!****************************************************
  if (nmdintegrator.eq.4) then
    dt  = tstp
    dth = 0.5_dp*tstp
    taub = taubcfg(ncf)
    taut = tautcfg(ncf)
    pr_target_temp = temperature
    pr_target_press = press
    pr_target_press_tens = 0.0_dp
    do i = 1,3
      pr_target_press_tens(i,i) = pr_target_press
    enddo
!
!  Set up random number sequence
!
    iseed_pr = 73709
    call init_genrand(iseed_pr)
!***************************************
!  Initialisations for new integrator  *
!***************************************
    pr_boltz = 8.31441d-3*kjmtoev
!
!  Temperature related quantities
!
    pr_ekin = 0.5_dp*dble(ndof_atm)*pr_boltz*temperature
    pr_temp = 2.0_dp*pr_ekin/pr_boltz/dble(ndof_atm)
!
!  Target kinetic energies
!
    pr_ekintarget_atm  = 0.5_dp*pr_boltz*pr_target_temp
    pr_ekintarget_baro = 0.5_dp*pr_boltz*pr_target_temp
!
!  Barostat
!
    if (lpr_baro_iso) then
      bmass_iso = 2.0_dp*pr_ekintarget_atm*dble(ndof_atm)*taub**2
      if (.not.lpisoinput) then
        p_iso = sqrt(pr_ekintarget_baro*dble(ndof_baro)*bmass_iso)*gauss_rand()
      endif
      if (.not.lpflxinput) then
        p_flx = 0.0_dp
      endif
      lpisooutput = .true.
      pr_ekinbaro = 0.5_dp/bmass_iso*p_iso**2
    elseif (lpr_baro_flx) then
      bmass_flx = 2.0_dp*pr_ekintarget_atm*dble(ndof_atm)*taub**2/3.0_dp
      if (.not.lpisoinput) then
        p_iso = 0.0_dp
      endif
      if (.not.lpflxinput) then
        p_flx = 1.d-8
      endif
      lpflxoutput = .true.
      pr_ekinbaro = 0.0_dp
    else
      if (.not.lpisoinput) then
        p_iso = 0.0_dp
      endif
      if (.not.lpflxinput) then
        p_flx = 0.0_dp
      endif
      pr_ekinbaro = 0.0_dp
    endif
!
!  Initialise conversed quantity integral
!
    pr_cons = 0.0_dp
  endif
!**************************
!  Initial function call  *
!**************************
  call mdfunct(0_i4,fc,.false.,.true.,.true.)
!
!  Zero higher time derivatives initially, except for acceleration
!
  do i = 1,numat
    if (lopf(i).and..not.lfix(i)) then
      rrmi = rmass(i)
      x2(i) = - xdrv(i)*stpsqh*rrmi
      y2(i) = - ydrv(i)*stpsqh*rrmi
      z2(i) = - zdrv(i)*stpsqh*rrmi
      x3(i) = 0.0_dp
      y3(i) = 0.0_dp
      z3(i) = 0.0_dp
      x4(i) = 0.0_dp
      y4(i) = 0.0_dp
      z4(i) = 0.0_dp
      x5(i) = 0.0_dp
      y5(i) = 0.0_dp
      z5(i) = 0.0_dp
    endif
  enddo
!
!  Scale velocities to required temperature
!  Must be done after accelerations are initialised
!
  call mdscale(.false.)
  if (lmain) then
!***********************************
!  Initialise MD trajectory files  *
!***********************************
!
!  Channels :
!
!    31 = trajectory file
!    32 = archive file
!    33 = xyz file
!    34 = history file
!
    if (ltrj.and.ioproc) then
      if (trjfile(1:1).ne.' ') then
        if (ltrjascii) then
          open(31,file=trjfile,status='unknown',form='formatted')
        else
          open(31,file=trjfile,status='unknown',form='unformatted')
        endif
      endif
      rewind(31)
      if (ltrjascii) then
        write(31,'(f5.2)') version
        write(31,'(i8,1x,i1)') numat, ndim
      else
        write(31) version
        write(31) numat, ndim
      endif
    endif
    if (larc.and.lmovie.and.ioproc) then
      call initarc(32_i4,mdafil)
    endif
    if (lxyz.and.lxyzmovie.and.ioproc) then
      if (xyzfile.ne.' ') then
        open(33,file=xyzfile,status='unknown',form='formatted')
      else
        open(33)
      endif
    endif
    if (lhis.and.ioproc) then
      if (hisfile.ne.' ') then
        open(34,file=hisfile,status='unknown',form='formatted')
      else
        open(34)
      endif
      if (ntitle.gt.0) then
        write(34,'(a80)') titleword(1)
      else
        write(34,'(''History file written from GULP'',a50)')
      endif
      keytrj = 1
      write(34,'(3i10)') keytrj,ndim,numat
    endif
    if (lpre.and.ioproc) then
      if (prefile.ne.' ') then
        open(35,file=prefile,status='unknown',form='formatted')
      else
        open(35)
      endif
    endif
    if (ldcd.and.ioproc) then
      if (dcdfile.ne.' ') then
        open(36,file=dcdfile,status='unknown',form='unformatted')
      else
        open(36)
      endif
      call outdcdheader(36_i4)
    endif
  endif
  if (nmdintegrator.eq.4) then
!*************************************************************
!  For new MD integrator initialise the pressure and stress  *
!*************************************************************
    vol = volume(rv)
    pr_press = pfct*(2.0_dp*pr_ekin - virial)/(3.0_dp*vol)
    if (lpr_baro_flx) then
      call compute_kinstress()
      pr_stress = pfct*(2.0_dp*pr_kinstress - virial_m)/vol
    endif
  endif
!
  return
  end
