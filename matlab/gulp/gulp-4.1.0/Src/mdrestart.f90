  subroutine mdrestart
!
!  Initialises the control parameters and arrays for a
!  restarted molecular dynamics run.
!
!  The absolute cartesian coordinates are stored on channel 39
!
!   2/97 Modifications added for GC/shell model MD (JRH)
!   3/97 Modifications for breathing shells in MD added 
!   7/97 Correction to target KE added
!   7/98 History file option added
!   2/00 Channel for absolute Cartesian coordinates changed to 39
!        to avoid conflict with archive files
!  12/00 Minor modifications for 1-D/2-D cases added
!  12/00 Set up for minimum image convention has been added as
!        per setmd.f
!   6/01 Initialisation of accelerations corrected for non-Gear
!        integration schemes.
!   6/01 List based methods turned off for more three body potentials
!  10/01 Use of channel 39 replaced with arrays in module moldyn
!  10/02 Conversion of lambda averages to internal units added
!  11/03 Calculation of nmoving corrected to match setmd
!   1/04 Handling of ndim in trajectory files corrected
!  11/04 Six-body potentials added
!   7/05 ratiom made species specific
!   5/06 Mass now uses species values
!   5/07 Breathing shell pointer data moved to module
!  12/07 Unused variables removed
!   3/08 Saving of initial volume added for mdmaxvolume option
!   3/08 New MD integrator added
!   4/08 Modified to allow for NPH ensemble and new integrator
!   4/08 Goto construct removed
!   8/08 Conserved quantity integral now initialised from restart file
!   1/09 Name of new integrator changed to stochastic
!   6/09 Module name changed from three to m_three
!  11/09 Handling of header for movie archive file on restart modified
!   8/10 Copy of absolute coordinates to x/y/zalat moved to before 
!        setup call otherwise molecules get messed up.
!   8/10 p_iso and p_flx now only set if not read in.
!   5/11 Compatibility check on integrator and tethering added
!   7/11 ndof_atm corrected for stocastic thermostat
!   8/11 Output of plumed details added
!  11/11 Tethering enabled with stochastic integrator
!   3/12 Output of DCD file added
!   4/12 Handling of opening files modified due to gfortran change
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
  use constants,         only : kjmtoev
  use control
  use current
  use dump,              only : mdafil
  use derivatives
  use element
  use files
  use four
  use general
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
  use pr_random_module,  only : gauss_rand, init_genrand, adv_rand
  use randomnumbers,     only : npr_randomcalls_adv, npr_grandomcalls_adv
  use shell
  use six
  use species,           only : massspec
  use two
  use velocities
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ind
  integer(i4)        :: iseed_pr
  integer(i4)        :: keytrj
  integer(i4)        :: nati
  integer(i4)        :: ncellfree
  integer(i4)        :: noldat
  integer(i4)        :: nolddim
  integer(i4)        :: nsi
  integer(i4)        :: nsofar
  logical            :: lcellanglefree
  logical            :: leof
  logical            :: lwarn
  real(dp)           :: cellmax
  real(dp)           :: cutmax
  real(dp)           :: efct
  real(dp)           :: flno
  real(dp)           :: oldversion
  real(dp)           :: stepsq
  real(dp)           :: tfct
  real(dp)           :: tstp
  real(dp)           :: vfct
  real(dp)           :: vol
  real(dp)           :: volume
!
  llist3 = (index(keyword,'noli').eq.0)
  llist4 = (index(keyword,'noli').eq.0)
  llist6 = (index(keyword,'noli').eq.0)
!
!  Check that ensemble is consistent with degrees of freedom
!
  if (nensemble(ncf).eq.3.and.index(keyword,'conp').eq.0) then
    call outerror('conp must be specified for an NPT MD run',0_i4)
    call stopnow('mdrestart')
  endif
  if (nensemble(ncf).eq.4.and.index(keyword,'conp').eq.0) then
    call outerror('conp must be specified for an NPH MD run',0_i4)
    call stopnow('mdrestart')
  endif
  if (nensemble(ncf).lt.3.and.index(keyword,'conp').ne.0) then
    call outerror('conp cannot be specified for an NVE or NVT MD',0_i4)
    call stopnow('mdrestart')
  endif
!
!  Check that temperature is greater than zero
!
  if (temperature.lt.0.5_dp) then 
    call outerror('Temperature is too small in molecular dynamics',0_i4)
    call stopnow('mdrestart')
  endif
!
!  Disable variable cell dynamics for surfaces and polymers
!
  if (nensemble(ncf).eq.3.and.ndim.eq.2) then
    call outerror('NPT ensemble not yet allowed for MD of surfaces',0_i4)
    call stopnow('mdrestart')
  endif
  if (nensemble(ncf).eq.3.and.ndim.eq.1) then
    call outerror('NPT ensemble not yet allowed for MD of polymers',0_i4)
    call stopnow('mdrestart')
  endif
  if (nensemble(ncf).eq.4.and.ndim.eq.2) then
    call outerror('NPH ensemble not yet allowed for MD of surfaces',0_i4)
    call stopnow('mdrestart')
  endif
  if (nensemble(ncf).eq.4.and.ndim.eq.1) then
    call outerror('NPH ensemble not yet allowed for MD of polymers',0_i4)
    call stopnow('mdrestart')
  endif
!
  if (nmdintegrator.eq.1) then
    if (ioproc) then
      write(ioout,'(/,''  Equations of motion will be integrated using Gear''''s algorithm.'')')
    endif
    if (nensemble(ncf).ne.1) then
      call outerror('Gear algorithm only available for NVE',0_i4)
      call stopnow('mdrestart')
    endif
  elseif (nmdintegrator.eq.2) then
    if (ioproc) then
      write(ioout,'(/,''  Equations of motion will be integrated using the velocity Verlet algorithm.'')')
    endif
    if (nensemble(ncf).gt.2) then
      call outerror('Velocity Verlet only available for NVE/NVT',0_i4)
      call stopnow('mdrestart')
    endif
  elseif (nmdintegrator.eq.3) then
    if (ioproc) then
      write(ioout,'(/,''  Equations of motion will be integrated using the leapfrog Verlet algorithm.'')')
    endif
    if (nensemble(ncf).gt.3) then
      call outerror('Velocity Verlet only available for NVE/NVT/NPT',0_i4)
      call stopnow('mdrestart')
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
    select case (ndim)
    case(1)
      cellmax = a
    case(2)
      cellmax = max(a,b)
    case(3)
      cellmax = max(a,b,c)
    end select
    cutmax = max(sqrt(rmx2),rpmax)
    if (cutmax.gt.cellmax) then
      if (ioproc) then
        write(ioout,'(/,'' **** Warning - Cell is smaller than cut-offs      ****'')')
        write(ioout,'('' **** Minimum image results will differ from exact ****'')')
        nwarn = nwarn + 1
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
          nwarn = nwarn + 1
          if (ioproc) then
            write(ioout,'(/,''  **** Warning - list-based method turned off for three-body terms ****'')')
            write(ioout,'(''  **** due to the presence of non-intramolecular potential types   ****'',/)')
          endif
        endif
        lwarn = .true.
      endif
    enddo
  endif
!
  efct = 9648.462267_dp
  refct = 1.0_dp/efct
  vfct = 0.1_dp*8.31441_dp
  rvfct = 1.0_dp/vfct
  pfct = efct/(60.2205_dp*3.0_dp)
  smdfct = 0.5_dp*vfct*refct
!*****************************
!  Length of run parameters  *
!*****************************
  tstp = tstep(ncf)
  if (tstp.eq.0.0_dp) then
    call outerror('timestep in MD run is zero',0_i4)
    call stopnow('mdrestart')
  endif
  stepsq = tstp*tstp
  stpsqh = 0.5_dp*stepsq*efct
  refct = refct/stepsq
  rvfct = rvfct/stepsq
  if (nmdeq(ncf).eq.0) nmdeq(ncf) = nint(tmdeq(ncf)/tstp)
  if (nmdprod(ncf).eq.0) nmdprod(ncf) = nint(tmdprod(ncf)/tstp)
  if (nmdsamp(ncf).eq.0) nmdsamp(ncf) = nint(tmdsamp(ncf)/tstp)
  if (nmdwrite(ncf).eq.0) nmdwrite(ncf) = nint(tmdwrite(ncf)/tstp)
  if (nmdsamp(ncf).eq.0) nmdsamp(ncf) = 20
  if (nmdwrite(ncf).eq.0) nmdwrite(ncf) = 20
  if (tmdscale(ncf).eq.0.0_dp) tmdscale(ncf) = nmdeq(ncf)*tstp
!
!  Check that timesofar is less than run length
!
  nsofar = nint(timesofar/tstp)
  if (nsofar.gt.(nmdeq(ncf)+nmdprod(ncf))) then
    call outerror('current_time in MD restart exceeds length',0_i4)
    call stopnow('mdrestart')
  endif
!******************
!  General setup  *
!******************
!
!  Copy absolute coordinates to current coordinates
!
  do i = 1,numat
    xalat(i) = xabsco(i)
    yalat(i) = yabsco(i)
    zalat(i) = zabsco(i)
  enddo
  call setup(.true.)
  if (nthb.gt.0.and.llist3) call setlist3
  if (nfor.gt.0.and.llist4) call setlist4
  if (nsix.gt.0.and.llist6) call setlist6
!
!  Initialise xcell which contains cell components
!
  if (nensemble(ncf).eq.3.or.nensemble(ncf).eq.4) then
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
    call rlist
  endif
!
!  Set up shell mass ratios 
!
  call setratiom
!
!  Set up array of masses (shells get their masses assigned
!  here, cores get the full ion mass here and their mass is
!  corrected after the velocities have been assigned)
!
!  cellmass = sum of all masses within the unit cell
!
  cellmass = 0.0_dp
  massloop: do i = 1,numat
    nati = nat(i)
    if (nati.gt.maxele) then
      nsi = nspecptr(ncsptr(i))
      mass(i) = ratiom(i)*massspec(nsi)
      if (ratiom(i).eq.0.0_dp) then
        rmass(i) = 0.0_dp
        cycle massloop
      endif
    else
      nsi = nspecptr(i)
      mass(i) = massspec(nsi)*occuf(i)
    endif
    cellmass = cellmass + mass(i)
    rmass(i) = 1.0_dp/mass(i)
   enddo massloop
!
!  Check that breathing cores/shells are not present
!
  if (nbsmat.gt.0.and..not.ladiabatic) then
    call outerror('For breathing species MD use minimisation algorithm',0_i4)
    call stopnow('mdrestart')
  endif
!
!  Trap MD with partial occupancies as it won't work properly yet
!
  do i = 1,numat
    if (occuf(i).lt.1.0_dp) then
      call outerror('MD not allowed with partial occupancies',0_i4)
      call stopnow('mdrestart')
    endif
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
      ind = ind - (nstrains + 1)
      ind = (ind/3) + 1
      lopf(ind) = .true.
    endif
  enddo
!
!  Setup breathing shell pointer
!
  if (nbsmat.ne.0) then
    if (ioproc) then
      write(ioout,'(/,''  Radii will be optimised at each time step'')')
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
!  Set cell flags for new integrator
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
!  Output plumed info
!
  if (lplumed) then
    if (ioproc) then
      write(ioout,'(/,''  PLUMED run with input from '',a50,/)') plumedfile(1:50)
    endif
  endif
!
!  Multiply temperature bath factor constant by number of mobile atoms
!
  smdfct = smdfct*dble(ndof)
  smdfctt = smdfct*temperature
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
!
!  Setup velocities and acceleration - rescale by timestep and
!  power of ten to keep numbers reasonable in restart files!
!
  if (nmdintegrator.eq.1) then
    do i = 1,numat
      if (lopf(i)) then
        velx(i) = velx(i)*tstp
        vely(i) = vely(i)*tstp
        velz(i) = velz(i)*tstp
        tfct = tstp*tstp*1000.0_dp
        x2(i) = x2(i)*tfct
        y2(i) = y2(i)*tfct
        z2(i) = z2(i)*tfct
        tfct = tfct*tstp*100.0_dp
        x3(i) = x3(i)*tfct
        y3(i) = y3(i)*tfct
        z3(i) = z3(i)*tfct
        tfct = tfct*tstp*100.0_dp
        x4(i) = x4(i)*tfct
        y4(i) = y4(i)*tfct
        z4(i) = z4(i)*tfct
        tfct = tfct*tstp*100.0_dp
        x5(i) = x5(i)*tfct
        y5(i) = y5(i)*tfct
        z5(i) = z5(i)*tfct
      else
        velx(i) = 0.0_dp
        vely(i) = 0.0_dp
        velz(i) = 0.0_dp
        x2(i) = 0.0_dp
        y2(i) = 0.0_dp
        z2(i) = 0.0_dp
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
  elseif (nmdintegrator.eq.2) then
    do i = 1,numat
      if (lopf(i)) then
        velx(i) = velx(i)*tstp
        vely(i) = vely(i)*tstp
        velz(i) = velz(i)*tstp
        tfct = tstp*tstp*1000.0_dp
        x2(i) = x2(i)*tfct
        y2(i) = y2(i)*tfct
        z2(i) = z2(i)*tfct
      else
        velx(i) = 0.0_dp
        vely(i) = 0.0_dp
        velz(i) = 0.0_dp
        x2(i) = 0.0_dp
        y2(i) = 0.0_dp
        z2(i) = 0.0_dp
      endif
      x3(i) = 0.0_dp                                                                                        
      y3(i) = 0.0_dp                                                                                        
      z3(i) = 0.0_dp                                                                                        
      x4(i) = 0.0_dp                                                                                        
      y4(i) = 0.0_dp                                                                                        
      z4(i) = 0.0_dp                                                                                        
      x5(i) = 0.0_dp                                                                                        
      y5(i) = 0.0_dp                                                                                        
      z5(i) = 0.0_dp                                                                                        
    enddo
  elseif (nmdintegrator.eq.3.or.nmdintegrator.eq.4) then
    do i = 1,numat
      if (lopf(i)) then
        velx(i) = velx(i)*tstp
        vely(i) = vely(i)*tstp
        velz(i) = velz(i)*tstp
      else
        velx(i) = 0.0_dp
        vely(i) = 0.0_dp
        velz(i) = 0.0_dp
      endif
      x2(i) = 0.0_dp
      y2(i) = 0.0_dp
      z2(i) = 0.0_dp
      x3(i) = 0.0_dp  
      y3(i) = 0.0_dp  
      z3(i) = 0.0_dp  
      x4(i) = 0.0_dp  
      y4(i) = 0.0_dp  
      z4(i) = 0.0_dp  
      x5(i) = 0.0_dp  
      y5(i) = 0.0_dp  
      z5(i) = 0.0_dp  
    enddo
  endif
  if (nensemble(ncf).ge.2) then
    sfac = sfac*tstp
    sumsfac = sumsfac*tstp
  endif
  if (nensemble(ncf).eq.3) then
    do i = 1,nstrains
      velc(i) = velc(i)*tstp
    enddo
!
!  Calculate pressure friction factor divided by NkT
!  psfct is in (eV)-1, so P.V needs to be in eV
!
    psfct = qpres(ncf)*efct/(dble(numat)*vfct)
!
!  Scale by timestep
!
    psfct = psfct*tstp
    psfctt = psfct/temperature
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
    pr_cons = pr_conscfg(ncf)
!
!  Move random number counter to the right place for a restart
!
    if ((npr_randomcalls_adv+npr_grandomcalls_adv).gt.1) then
      call adv_rand(npr_randomcalls_adv,npr_grandomcalls_adv)
    endif
  endif
!
!  Convert units of lambda averages
!
  if (lmdconstrain(ncf)) then
    sumlambdaR = 0.25_dp*sumlambdaR*stpsqh
    sumlambdaV = 0.25_dp*sumlambdaV*stpsqh
  endif
!
!  Set pointer to the end of any existing MD dumpfile on channel 31
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
    noldat = 0
    if (ltrjascii) then
      read(31,*,end=100,err=100) oldversion
      read(31,*,end=100,err=100) noldat,nolddim
    else
      read(31,end=100,err=100) oldversion
      read(31,end=100,err=100) noldat,nolddim
    endif
    leof = .false.
    if (ltrjascii) then
      do while (.not.leof)
        read(31,*,end=100,err=100) flno
      enddo
    else
      do while (.not.leof)
        read(31,end=100,err=100) flno
      enddo
    endif
100 continue
    if (.not.ltrjascii) backspace(31)
    if (noldat.eq.0) then
      if (ltrjascii) then
        write(31,'(f5.2)') version
        write(31,'(i8,1x,i1)') numat, ndim
      else
        write(31) version
        write(31) numat, ndim
      endif
    elseif (noldat.ne.numat.or.nolddim.ne.ndim) then
      call outwarning('Old MD dumpfile is for a different system and will be replaced',0_i4)
      nwarn = nwarn + 1
      rewind(31)
      write(31) numat, ndim
    elseif (oldversion.eq.1.1_dp.and.nensemble(ncf).ne.1) then
      call outerror('trajectory from 1.1 does not support NVT/NPT',0_i4)
      call stopnow('mdrestart')
    endif
  endif
!
!  Set pointer to end of the arc file if one exists on channel 32
!
  if (larc.and.lmovie.and.ioproc) then
    if (mdafil(1:1).ne.' ') then
      open(32,file=mdafil,status='old',access='append',form='formatted',err=103)
      goto 105
    else
      open(32,status='old',access='append',form='formatted',err=103)
      goto 105
    endif
103 continue
!
!  No archive file found
!
    call initarc(32_i4,mdafil)
105 continue
  endif
!
!  Set pointer to end of the history file if one exists on channel 34
!
  if (lhis.and.ioproc) then
    if (hisfile(1:1).ne.' ') then
      open(34,file=hisfile,status='old',access='append',form='formatted',err=107)
      goto 110
    else
      open(34,status='old',access='append',form='formatted',err=107)
      goto 110
    endif
107 continue
!
!  No history file found
!
    if (hisfile(1:1).ne.' ') then
      open(34,file=hisfile,status='new',form='formatted')
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
110 continue
  endif
!
!  Set pointer to end of the pressure file if one exists on channel 35
!
  if (lpre.and.ioproc) then
    if (prefile(1:1).ne.' ') then
      open(35,file=prefile,status='old',access='append',form='formatted',err=113)
      goto 120
    else
      open(35,status='old',access='append',form='formatted',err=113)
      goto 120
    endif
113 continue
    if (prefile(1:1).ne.' ') then
      open(35,file=prefile,status='new',form='formatted')
    else
      open(35)
    endif
120 continue
  endif
!
!  Set pointer to end of the DCD file if one exists on channel 36
!
  if (ldcd.and.ioproc) then
    if (dcdfile(1:1).ne.' ') then
      open(36,file=dcdfile,status='old',form='unformatted',access='append',err=130)
    endif
    goto 135
130 continue
!
!  File presumably doesn't exist if there was an error so we can open and write header
!
    open(36,file=dcdfile,status='new',form='unformatted')
    call outdcdheader(36_i4)
135 continue
  endif
!
!  Store initial volume for volume checks
!
  if (ndim.eq.3) then
    rmdvol0 = volume(rv)
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
