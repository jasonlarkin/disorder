  subroutine runmc
!
!  Main subroutine for Monte Carlo run control
!
!  MC variables from module:
!
!  dmaxmc           = maximum displacement in any Cartesian direction
!  rmaxmc           = maximum rotation about any Cartesian direction
!  rmaxmc           = maximum strain 
!  mcfile           = file name (a60) for sampling results (binary)
!  ngcmcspec        = number of species for GCMC creation/destruction
!  ngcmcnat         = species atomic numbers for GCMC species
!  ngcmctype        = species atomic types for GCMC species
!  nmcoutfreq       = number of trial steps between outputs
!  nmcsample        = sampling frequency - write out every N accepted trials
!  nmctrial         = number of trial MC steps
!  ntargetfreq      = frequency of adjustment of dmaxmc
!  ntargetfreqr     = frequency of adjustment of rmaxmc
!  ntargetfreqs     = frequency of adjustment of smaxmc
!  pcreate          = relative probability of a creation operation
!  pdestroy         = relative probability of a destruction operation
!  pmove            = relative probability of a translation operation
!  protate          = relative probability of a rotation operation
!  pstrain          = relative probability of a cell strain operation
!  pswap            = relative probability of a swap operation
!  nrotationtype    = number of types of rotation to attempt
!  nptrrotationtype = pointer to type of rotation to attempt
!  targetmove       = target acceptance ratio for translation
!  targetrota       = target acceptance ratio for rotation 
!  targetstrain     = target acceptance ratio for strain
!  targetswap       = target acceptance ratio for swap
!  ngcmcmol         = number of molecules available for insertion in GCMC
!  ngcmcmolat       = number of atoms in each GCMC molecule
!  ngcmcmolnat      = atomic number of each atom in GCMC molecule
!  ngcmcmoltype     = atomic type of each atom in GCMC molecule
!  xgcmcmol         = x Cartesian coordinate of each atom in GCMC molecule
!  ygcmcmol         = y Cartesian coordinate of each atom in GCMC molecule
!  zgcmcmol         = z Cartesian coordinate of each atom in GCMC molecule
!  maxgcmcmol       = maximum dimension of arrays for number of GCMC molecules
!  maxgcmcmolat     = maximum number of atoms in each GCMC molecule
!
!   1/01 Created
!  10/02 Made implicit none
!  10/02 Units of betamc corrected
!  10/04 Fixes added 
!  11/04 Modifications to correct acceptance ratio in GCMC added
!  11/04 Pi accessed from module
!   5/07 Temperature annealing added
!   5/07 GMC file changed to formatted form
!   5/07 Step number and temperature passed to outmc
!   5/07 Option to write out configurations when ever energy lowers added
!   5/07 Probability of straining cell added
!   5/07 nstrainable now used in test to set lx0centroid rather than lconp
!   6/07 rotation types added
!   6/07 lx0centroid is now only set to true for strain operations since 
!        the algorithm will not work for other cases.
!   6/07 Calls to x0str routines added when move is not accepted
!   1/08 Timing calls added
!   1/08 Energy difference algorithm added
!   1/08 random -> GULP_random
!   1/09 swap move added as an option
!   1/09 lowest energy now output
!   5/09 Calls to x0tostr routines moved from energy to calling routine
!   9/12 Call to dumpdur moved from outmc to dumpdur
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
!  Julian Gale, NRI, Curtin University, September 2012
!
  use bondorderdata,  only : nbopot
  use configurations, only : ntempstp, ntempstpstart, tempcfg, tempstp
  use constants
  use control
  use current
  use dump,           only : idump, ncycd
  use files
  use general
  use genetic,        only : iseed
  use iochannels
  use molecule
  use montecarlo
  use parallel
  use polarise,       only : lpolar
  use shell
  use sutton,         only : lsuttonc
  use symmetry
  use terse,          only : lterseoutcoords, lterseoutcell
  use times
  implicit none
!
!  Local variables
!
  character(len=6)                         :: rotword(3)
  integer(i4)                              :: i
  integer(i4)                              :: naccepted
  integer(i4)                              :: nacceptedlasttime
  integer(i4)                              :: ncreate
  integer(i4)                              :: ncreatetried
  integer(i4)                              :: ndestroy
  integer(i4)                              :: ndestroytried
  integer(i4)                              :: nmcstart
  integer(i4)                              :: nmove
  integer(i4)                              :: nmovetried
  integer(i4)                              :: noperation
  integer(i4)                              :: nrotate
  integer(i4)                              :: nrotatetried
  integer(i4)                              :: nstrain
  integer(i4)                              :: nstraintried
  integer(i4)                              :: nswap
  integer(i4)                              :: nswaptried
  integer(i4)                              :: ntest
  integer(i4)                              :: status
  logical                                  :: laccept
  logical                                  :: lEdifference
  logical                                  :: lvariableT
  real(dp)                                 :: betamc
  real(dp)                                 :: currentratio
  real(dp)                                 :: currentratior
  real(dp)                                 :: currentratios
  real(dp)                                 :: cpfactor
  real(dp)                                 :: cputime
  real(dp)                                 :: debrogliefct
  real(dp)                                 :: ebefore
  real(dp)                                 :: ecurrent
  real(dp)                                 :: elowest
  real(dp)                                 :: etrial
  real(dp)                                 :: expftrial
  real(dp)                                 :: ftrial
  real(dp)                                 :: percentcreate
  real(dp)                                 :: percentdestroy
  real(dp)                                 :: percentmove
  real(dp)                                 :: percentrotate
  real(dp)                                 :: percentstrain
  real(dp)                                 :: percentswap
  real(dp)                                 :: percenttotal
  real(dp)                                 :: plcreate
  real(dp)                                 :: pldestroy
  real(dp)                                 :: plmove
  real(dp)                                 :: plrotate
  real(dp)                                 :: plstrain
  real(dp)                                 :: plswap
  real(dp)                                 :: ptotal
  real(dp)                                 :: randnum
  real(dp)                                 :: GULP_random
  real(dp)                                 :: t1
  real(dp)                                 :: t2
  real(dp)                                 :: t3
  real(dp)                                 :: t4
  real(dp)                                 :: tenergy
  real(dp)                                 :: volm3
  real(dp)                                 :: volume
!
  t1 = cputime()
!**************************************************
!  Check whether MC is allowed for present model  *
!**************************************************
!
!  No shells or BSM allowed
!
  if (nshell.gt.0) then
    call outerror('shells not currently allowed in Monte Carlo',0_i4)
    call stopnow('runmc')
  endif
  if (nbsmat.gt.0) then
    call outerror('breathing shells not allowed in Monte Carlo',0_i4)
    call stopnow('runmc')
  endif
!
!  Temperature is zero
!
  if (abs(temperature).eq.0.0_dp) then
    call outerror('temperature is zero in Monte Carlo',0_i4)
    call stopnow('runmc')
  endif
!
!  GCMC only allowed for 3-D systems
!
  if (ndim.ne.3.and.((pcreate+pdestroy).gt.0.0)) then
    call outerror('GCMC only currently allowed for 3-D systems',0_i4)
    call stopnow('runmc')
  endif
!
!  Symmetry must be turned off
!
  if (ndim.eq.3.and.(nspcg(ncf).gt.1.or.ngocfg(ncf).gt.1)) then
    call outerror('no symmetry is allowed in MC and is turned off',0_i4)
    call symoff
    nwarn = nwarn + 1
  endif
!*****************************
!  Set up parameters for MC  *
!*****************************
!
!  Initialise counters
!
!  naccepted     = number of accepted trials so far
!  ncreate       = number of accepted creation trials so far
!  ndestroy      = number of accepted destruction trials so far
!  nmove         = number of accepted movement trials so far
!  nrotate       = number of accepted rotation trials so far
!  nstrain       = number of accepted strain trials so far
!  nswap         = number of accepted swap trials so far
!  ncreatetried  = number of attempted creation trials so far
!  ndestroytried = number of attempted destruction trials so far
!  nmovetried    = number of attempted movement trials so far
!  nrotatetried  = number of attempted rotation trials so far
!  nstraintried  = number of attempted strain trials so far
!  nswaptried    = number of attempted swap trials so far
!
  naccepted = 0
  nacceptedlasttime = 0
  ncreate = 0
  ndestroy = 0
  nmove = 0
  nrotate = 0
  nstrain = 0
  nswap = 0
  ncreatetried = 0
  ndestroytried = 0
  nmovetried = 0
  nrotatetried = 0
  nstraintried = 0
  nswaptried = 0
!
!  Do we use full energy at every point or energy differences?
!
!  Can't use this algorithm for the following cases:
!    - constant pressure runs since all atoms change energy
!    - EAM runs since densities need handling at other centres
!    - EEM runs since charges will change on all atoms
!    - ReaxFF runs since charges will change on all atoms
!    - lpolar = .true. since fields will change on other atoms
!    - Brenner & bond order runs not yet enabled
!    - spatial decomposition
!
  lEdifference = (index(keyword,'nomce').eq.0.and..not.lseok.and..not.lsuttonc &
                  .and..not.leem.and..not.lreaxFF.and..not.lpolar.and..not.lspatial.and..not.lbrenner.and. &
                  nbopot.eq.0.and.pstrain.eq.0.0_dp)
!
!  Sampling set up
!
!  If sampling frequency is zero then turn off output
!
  if (nmcsample.eq.0) lmcout = .false.
  if (lmcout) then
!
!  Open sampling file - if no name set then use a default
!
    if (mcfile.eq.' ') mcfile = 'gulp.gmc'
    open(31,file=mcfile,status='unknown',form='formatted')
  endif
!
!  Initialise mean properties
!
!  mcemean = mean energy of the system
!  mcnmean = mean number of atoms in the system
!
  if (nmcstep.eq.1) then
    mcemean = 0.0_dp
    mcnmean = 0.0_dp
    nmcaccepted = 0
  endif
!
!  Initialise lowest properties
!
!  mcelowest = lowest energy of the system
!
  if (nmcstep.eq.1) then
    mcelowest = 0.0_dp
  endif
!
!  Normalise probability of operations and create boundaries
!
  ptotal = pcreate + pdestroy + pmove + protate + pstrain + pswap
  plcreate = pcreate/ptotal
  pldestroy = plcreate + pdestroy/ptotal
  plmove = pldestroy + pmove/ptotal
  plrotate = plmove + protate/ptotal
  plswap = plrotate + pswap/ptotal
  plstrain = 1.0_dp
!
!  Setup pointer to moveable atoms
!
!  nmoveable    = total number of moveable atoms
!  nptrmoveable = pointer to moveable atoms 
!
  nmoveable = 0
  call setmoveptr
!
!  Setup pointer to atoms that can be destroyed
!
!  ndestroyable    = total number of atoms that could be destroyed
!  nptrdestroyable = pointer to destroyable atoms 
!
  ndestroyable = 0
  call setdestroyptr
!
!  Setup pointer to molecules that can be rotated
!
!  nrotateable    = total number of molecules that could be rotated
!  nptrrotateable = pointer to rotateable molecules
!
  nrotateable = 0
  call setrotateptr
!
!  Setup pointer to swapable atoms
!
!  nswapable    = total number of swapable atoms
!  nptrswapable = pointer to swapable atoms 
!
  nswapable = 0
  call setswapptr
!
!  Setup pointer to strains that can be strained
!
!  nstrainable    = total number of strains that could be strained
!  nptrstrainable = pointer to strainable strains
!
  nstrainable = 0
  call setstrainptr
!
!  Check that something is rotateable if rotation has been allowed
!
!      if (nrotateable.eq.0.and.(plrotate-plmove).gt.0.0_dp) then
!        call outerror('no molecules available to rotate',0_i4)
!        call stopnow('runmc')
!      endif
!
!  Calculate 1/kT in eV
!
  betamc = evtoj/(boltz*temperature)
!
!  Calculate factor involving the chemical potential which
!  influences the probability of acceptance for creation/
!  destruction depending on the number of atoms. Note that
!  the species mass (in g/mol) is factored out since this
!  must summed over the species present if we allow for
!  more than one type of GCMC species.
!
  debrogliefct = 2.0_dp*pi*1.0d-3*boltz*temperature/(avogadro*(planck**2))
  debrogliefct = debrogliefct**1.5_dp
  if (mcvolume.ne.0.0_dp) then
    volm3 = mcvolume * 1.0d-30
  else
    volm3 = volume(rv) * 1.0d-30
  endif
  cpfactor = exp(chempot*betamc)*debrogliefct*volm3
!
!  Is this a variable temperature run?
!
  lvariableT = (abs(tempstp(ncf)*dble(ntempstp(ncf))).gt.1.0d-6)
!
!  Convert linear structure array to main structure arrays
!
  if (lx0centroid) then
    call x0tostrcentroid
  else
    call x0tostr
  endif
!
!  Initialise molecule related data
!
  call mcmolinitial
!
!  Calculate energy for initial configuration
!
  t3 = cputime()
  call energy(ecurrent,.false.,.false.)
  t4 = cputime()
  tenergy = t4 - t3
!
!  Set initial value of lowest energy so far
!
  elowest = ecurrent
!
!  Surface energy
!  
  if (lseok) call surfaceenergy(ecurrent)
  if (ioproc) then
!
!  Output banner
!
    write(ioout,'(/,''********************************************************************************'')')
    write(ioout,'(''*  Monte Carlo                                                                 *'')')
    write(ioout,'(''********************************************************************************'',/)')
!
!  Output of MC parameters
!
    write(ioout,'(''  Number of trial MC steps   = '',i12,/)') nmctrial
    write(ioout,'(''  Output frequency in steps  = '',i12,/)') nmcoutfreq
    write(ioout,'(''  Sample frequency in steps  = '',i12,/)') nmcsample
    write(ioout,'(''  Probability of creation    = '',f12.6)') plcreate
    write(ioout,'(''  Probability of destruction = '',f12.6)') pldestroy-plcreate
    write(ioout,'(''  Probability of translation = '',f12.6)') plmove-pldestroy
    write(ioout,'(''  Probability of rotation    = '',f12.6)') plrotate-plmove
    write(ioout,'(''  Probability of swap        = '',f12.6)') plswap-plrotate
    write(ioout,'(''  Probability of strain      = '',f12.6)') plstrain-plswap
    write(ioout,'(/,''  Maximum Cartesian shift    = '',f12.6,'' Angstroms'')') dmaxmc
    write(ioout,'(''  Maximum rotational shift   = '',f12.6,'' degrees'')') rmaxmc*180.0_dp
    write(ioout,'(''  Maximum strain shift       = '',f12.6)') smaxmc
    if (protate.gt.0) then
      do i = 1,nrotationtype
        if (nptrrotationtype(i).eq.1) then
          rotword(i) = 'centre'
        elseif (nptrrotationtype(i).eq.2) then
          rotword(i) = 'atom'
        elseif (nptrrotationtype(i).eq.3) then
          rotword(i) = 'line'
        endif
      enddo
      write(ioout,'(/,''  Types of rotation allowed  ='',3(1x,a6))') (rotword(i),i=1,nrotationtype)
    endif
    if (pcreate.gt.0.or.pdestroy.gt.0) then
      write(ioout,'(/,''  Target chemical potential  = '',f12.6,'' eV'')') chempot
    endif
    if (mcvolume.ne.0.0_dp) then
      write(ioout,'(''  Accessible volume          = '',f12.6,'' Angs**3'')') mcvolume
    endif
    write(ioout,'(/)')
    if (lmcout) then
      if (lmclowestwrite) then
        write(ioout,'(''  File for MC samples = '',a60)') mcfile(1:60)
        write(ioout,'(''  Write only when energy of configuration is lowest so far '',/)')
      else
        write(ioout,'(''  File for MC samples = '',a60,/)') mcfile(1:60)
      endif
    endif
  endif
!****************************
!  Main loop over MC steps  *
!****************************
  nmcstart = nmcstep 
  do nmcstep = nmcstart,nmctrial
!
!  For variable temperature runs set the temperature and compute related variables
!
    if (lvariableT) then
      if (nmcstep.gt.ntempstpstart(ncf).and.nmcstep.le.(ntempstpstart(ncf)+ntempstp(ncf))) then
        temperature = tempcfg(ncf) + dble(nmcstep-ntempstpstart(ncf))*tempstp(ncf)
        betamc = evtoj/(boltz*temperature)
        debrogliefct = 2.0_dp*pi*1.0d-3*boltz*temperature/(avogadro*(planck**2))
        debrogliefct = debrogliefct**1.5_dp
        cpfactor = exp(chempot*betamc)*debrogliefct*volm3
      endif
    endif
!
!  Choose operation - with checks that the move is possible
!
100 randnum = GULP_random(iseed,1_i4)
    lx0centroid = .false.
    if (randnum.le.plcreate) then
      noperation = 1
      ncreatetried = ncreatetried + 1
    elseif (randnum.le.pldestroy) then
      if (ndestroyable.gt.0) then
        noperation = 2
        ndestroytried = ndestroytried + 1
      elseif (pcreate.gt.0.0) then
        noperation = 1
        ncreatetried = ncreatetried + 1
      elseif (pmove.gt.0.0.and.nmoveable.gt.0) then
        noperation = 3
        nmovetried = nmovetried + 1
      else
        call outerror('no valid trial for current configuration',0_i4)
        goto 100
      endif
    elseif (randnum.le.plmove) then
      if (nmoveable.gt.0) then
        noperation = 3
        nmovetried = nmovetried + 1
      elseif (pcreate.gt.0.0) then
        noperation = 1
        ncreatetried = ncreatetried + 1
      else
        call outerror('no valid trial for current configuration',0_i4)
        goto 100
      endif
    elseif (randnum.le.plrotate) then
      if (nrotateable.gt.0) then
        noperation = 4
        nrotatetried = nrotatetried + 1
      elseif (pcreate.gt.0.0) then
        noperation = 1 
        ncreatetried = ncreatetried + 1
      else 
        call outerror('no valid trial for current configuration',0_i4)
        goto 100
      endif
    elseif (randnum.le.plswap) then
      if (nswapable.gt.0) then
        noperation = 6
        nswaptried = nswaptried + 1
      elseif (pcreate.gt.0.0) then
        noperation = 1
        ncreatetried = ncreatetried + 1
      else
        call outerror('no valid trial for current configuration',0_i4)
        goto 100
      endif
    else
      noperation = 5
      nstraintried = nstraintried + 1
      lx0centroid = .true.
    endif
!
!  Find atoms to operate on
!
    if (noperation.eq.1) then
      call mccreate(1_i4)
    elseif (noperation.eq.2) then
      call mcdestroy(1_i4)
    elseif (noperation.eq.3) then
      call mcmove(1_i4)
    elseif (noperation.eq.4) then
      call mcrotate(1_i4)
    elseif (noperation.eq.6) then
      call mcswap(1_i4)
    else
      call mcstrain(1_i4)
    endif
    t3 = cputime()
    if (lEdifference) then
!
!  For energy difference approach compute energy of trial atoms before move
!
      if (noperation.eq.1) then
        ebefore = 0.0_dp
      else
        call mcenergy(ebefore,ntrialatom,nptrtrialatom,ltrialatom)
      endif
    else
      ebefore = ecurrent
    endif
    t4 = cputime()
    tenergy = tenergy + t4 - t3
!
!  Perform operation
!
    if (noperation.eq.1) then
      call mccreate(2_i4)
    elseif (noperation.eq.2) then
      call mcdestroy(2_i4)
    elseif (noperation.eq.3) then
      call mcmove(2_i4)
    elseif (noperation.eq.4) then
      call mcrotate(2_i4)
    elseif (noperation.eq.6) then
      call mcswap(2_i4)
    endif
!
!  Convert linear structure array to main structure arrays
!
    if (lx0centroid) then
      call x0tostrcentroid
    else
      call x0tostr
    endif
!
!  Calculate energy
!
    t3 = cputime()
    if (lEdifference) then
      if (noperation.eq.2) then
        etrial = 0.0_dp
      else
        call mcenergy(etrial,ntrialatom,nptrtrialatom,ltrialatom)
      endif
    else
      call energy(etrial,.false.,.false.)
    endif
    t4 = cputime()
    tenergy = tenergy + t4 - t3
!
!  Surface energy
!  
    if (lseok) call surfaceenergy(etrial)
!
!  Compute trial function
!
    call mctrialfunction(etrial,ebefore,betamc,cpfactor,noperation,ftrial)
    if (lEdifference) then
      etrial = ecurrent + etrial - ebefore
    endif
!
!  Accept move or not?
!
!  Ftrial < 0 => unconditional acceptance
!  Ftrial > 0 => accept according to exp(-dF/kT)
!
!  here F is the energy for regular MC, but is the creation or
!  destruction function for GCMC
!
    laccept = .false.
    if (ftrial.le.0.0_dp) then
      laccept = .true.
    else
!
!  Choose random number
!
      randnum = GULP_random(iseed,1_i4)
      expftrial = exp(-ftrial)
      laccept = (randnum.le.expftrial)
    endif
    if (laccept) then
!
!  Save configuration data
!
      ecurrent = etrial
      mcemean = (mcemean*nmcaccepted+ecurrent)/dble(nmcaccepted+1)
      mcnmean = (mcnmean*nmcaccepted+numat)/dble(nmcaccepted+1)
      mcelowest = min(mcelowest,ecurrent)
!
!  Update counters
!
      naccepted = naccepted + 1
      nmcaccepted = nmcaccepted + 1
      if (noperation.eq.1) then
        ncreate = ncreate + 1
        call setmoveptr
        call setdestroyptr
        call setrotateptr
      elseif (noperation.eq.2) then
        ndestroy = ndestroy + 1
        call setmoveptr
        call setdestroyptr
        call setrotateptr
      elseif (noperation.eq.3) then
        nmove = nmove + 1
      elseif (noperation.eq.4) then
        nrotate = nrotate + 1
      elseif (noperation.eq.6) then
        nswap = nswap + 1
      else
        nstrain = nstrain + 1
      endif
    else
!
!  Restore previous configuration
!
      if (noperation.eq.1) then
        call mccreate(3_i4)
      elseif (noperation.eq.2) then
        call mcdestroy(3_i4)
      elseif (noperation.eq.3) then
        call mcmove(3_i4)
      elseif (noperation.eq.4) then
        call mcrotate(3_i4)
      elseif (noperation.eq.6) then
        call mcswap(3_i4)
      else
        call mcstrain(2_i4)
      endif
!
!  Call routines to restore structure
!
      if (lx0centroid) then
        call x0tostrcentroid
      else  
        call x0tostr
      endif
    endif
    if (mod(nmcstep,nmcoutfreq).eq.0.and.ioproc) then
!
!  Output 
!
      if (nmcaccepted.eq.0) then
        write(ioout,'(''  Trials:'',i10,'' Accepted:'',i10,'' Mean E/N:'',f16.6,1x,f12.6)') &
          nmcstep,naccepted,ecurrent,dble(numat)
      else
        write(ioout,'(''  Trials:'',i10,'' Accepted:'',i10,'' Mean E/N:'',f16.6,1x,f12.6)') &
          nmcstep,naccepted,mcemean,mcnmean
      endif
    endif
    if (lmcout) then
      if (lmclowestwrite) then
        if (etrial.lt.elowest) then
          call outmc(etrial,nmcstep,temperature)
          elowest = etrial
        endif
      elseif (naccepted.ne.nacceptedlasttime) then
        if (mod(naccepted,nmcsample).eq.0.and.ioproc) then
!
!  Sample saving to file for post-analysis
!
          call outmc(etrial,nmcstep,temperature)
        endif
        nacceptedlasttime = naccepted
      endif
    endif
!
!  Ordinary dumpfile
!
    if (idump.gt.0) then
      ntest = nmcstep/ncycd
      ntest = nmcstep - ntest*ncycd
      if (ntest.eq.0) then
        call dumpdur(idump,nmcstep)
        call flush(idump)
      endif
    endif
!
    if (ntargetfreq.gt.0) then
      if (mod(nmcstep,ntargetfreq).eq.0) then
!
!  Adjust maximum displacement to achieve desired acceptance ratio
!
        currentratio = dble(nmove)/dble(nmovetried)
        if (currentratio.gt.targetmove) then
          dmaxmc = dmaxmc * 1.05_dp
        elseif (currentratio.lt.targetmove) then
          dmaxmc = dmaxmc * 0.95_dp
        endif
      endif
    endif
    if (ntargetfreqr.gt.0) then
      if (mod(nmcstep,ntargetfreqr).eq.0) then
!
!  Adjust maximum rotation to achieve desired acceptance ratio
!
        currentratior = dble(nrotate)/dble(nrotatetried)
        if (currentratior.gt.targetrota) then
          rmaxmc = rmaxmc * 1.05_dp
        elseif (currentratior.lt.targetrota) then
          rmaxmc = rmaxmc * 0.95_dp
        endif
!
!  Ensure that rmaxmc doesn't exceed 180 degrees
!
        rmaxmc = min(rmaxmc,1.0_dp)
      endif
    endif
    if (ntargetfreqs.gt.0) then
      if (mod(nmcstep,ntargetfreqs).eq.0) then
!  
!  Adjust maximum strain to achieve desired acceptance ratio
!  
        currentratios = dble(nstrain)/dble(nstraintried)
        if (currentratios.gt.targetstrain) then
          smaxmc = smaxmc * 1.05_dp
        elseif (currentratios.lt.targetstrain) then
          smaxmc = smaxmc * 0.95_dp
        endif
      endif
    endif
  enddo
!**********************
!  Output of results  *
!**********************
  if (ioproc) then
!
!  Mean properties output
!
    write(ioout,'(/,''  Monte Carlo properties :'',/)')
    write(ioout,'(''  Mean number of atoms = '',f15.6)') mcnmean
    write(ioout,'(''  Mean   energy        = '',f15.6)') mcemean
    write(ioout,'(''  Lowest energy        = '',f15.6)') mcelowest
!
!  Output acceptance ratios
!
    write(ioout,'(/,''  Acceptance ratios :'')')
    write(ioout,'(/,''--------------------------------------------------------------------------------'')')
    write(ioout,'('' Operation'',10x,''Attempts'',12x,''Accepted'',12x,''%Accepted'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    if (ncreatetried.gt.0) then
      percentcreate = dble(ncreate)/dble(ncreatetried)*100.0_dp
      write(ioout,'('' Creation   '',8x,i12,8x,i12,8x,f9.4)') ncreatetried,ncreate,percentcreate
    endif
    if (ndestroytried.gt.0) then
      percentdestroy = dble(ndestroy)/dble(ndestroytried)*100.0_dp
      write(ioout,'('' Destruction'',8x,i12,8x,i12,8x,f9.4)') ndestroytried,ndestroy,percentdestroy
    endif
    if (nmovetried.gt.0) then
      percentmove = dble(nmove)/dble(nmovetried)*100.0_dp
      write(ioout,'('' Translation'',8x,i12,8x,i12,8x,f9.4)') nmovetried,nmove,percentmove
    endif
    if (nrotatetried.gt.0) then
      percentrotate = dble(nrotate)/dble(nrotatetried)*100.0_dp
      write(ioout,'('' Rotation   '',8x,i12,8x,i12,8x,f9.4)') nrotatetried,nrotate,percentrotate
    endif
    if (nswaptried.gt.0) then
      percentswap = dble(nswap)/dble(nswaptried)*100.0_dp
      write(ioout,'('' Swap       '',8x,i12,8x,i12,8x,f9.4)') nswaptried,nswap,percentswap
    endif
    if (nstraintried.gt.0) then
      percentstrain = dble(nstrain)/dble(nstraintried)*100.0_dp
      write(ioout,'('' Cell strain'',8x,i12,8x,i12,8x,f9.4)') nstraintried,nstrain,percentstrain
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    percenttotal = dble(naccepted)/dble(nmctrial)*100.0_dp
    write(ioout,'('' Total      '',8x,i12,8x,i12,8x,f9.4)') nmctrial,naccepted,percenttotal
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Output final structure
!
  if (.not.lterseoutcell) call outcell
  if (.not.lterseoutcoords) call outstructure
!
!  Return final structure to main configuration arrays
!
  call mcstorecfg
!
!  Free memory
!
  deallocate(nptrswapable,stat=status)
  if (status/=0) call deallocate_error('runmc','nptrswapable')
  deallocate(nptrstrainable,stat=status)
  if (status/=0) call deallocate_error('runmc','nptrstrainable')
  deallocate(nptrdestroyable,stat=status)
  if (status/=0) call deallocate_error('runmc','nptrdestroyable')
  deallocate(nptrrotateable,stat=status)
  if (status/=0) call deallocate_error('runmc','nptrrotateable')
  deallocate(nptrmoveable,stat=status)
  if (status/=0) call deallocate_error('runmc','nptrmoveable')
!
!  Close sampling file
!
  if (lmcout) then
    close(31)
  endif
!
  t2 = cputime()
  tmc = tmc + t2 - t1 - tenergy
!
  return
  end
