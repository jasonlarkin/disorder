  subroutine mdword(nru,word,lwordok,iline,line,l55,l1000,ncurr)
!
!  Processes input for molecular dynamics related words
!
!   2/97 Grand Canonical options added (JRH)
!   3/97 NVT ensemble added
!   7/97 Integrator options added
!   4/98 Input channel for reading generalised
!   2/00 Channel for absolute Cartesian coordinates changed to 39
!        to avoid conflict with archive files
!   6/01 Initialisation of line added for benefit of some compilers
!   9/01 Bug in reading of accelerations corrected
!  10/01 Use of channel 39 replaced by arrays in moldyn module
!  10/02 MD constraint added
!   2/04 Force application delay added for MD
!   6/04 End time for force application added
!   1/05 Option to reset vectors added
!   4/05 Shell extrapolation added to iteration option
!   7/05 Extrapolation of shells now not turned off by default when
!        iteration option is given
!   7/05 ladiabatic set to false when shellmass ratio is non-zero
!   7/05 ratiom made into array
!   8/06 Channel passed through to linepro
!   7/07 Metadynamics options added
!  12/07 nint used when setting number of iterations
!  12/07 mdmaxtemp option added
!   3/08 mdmaxvolume option added
!   3/08 New MD integrator added
!   4/08 NPH ensemble added
!   5/08 Distance option added for metadynamics
!   8/08 Integral of conserved quantity added
!   8/08 Option to read number of random number calls added for
!        MD restarting
!   9/08 Coordinates added as metadynamics variables
!  12/08 Module input renamed to gulpinput
!   1/09 Name of new integrator changed to stochastic
!   8/10 Reading of p_iso and p_flx added for MD restart
!  12/10 Setting of qpres added for ensemble = NPH
!   5/11 Incompatibility between tethering and integrator type trapped
!   8/11 Case of words changed to lower case for several options to avoid case sensitivity
!   9/11 Metadynamics internal code replaced with Plumed
!  11/11 Bounds check added in tethering option
!  11/11 Extra calls to changemaxextrapol added to avoid arrays being incorrectly dimension.
!   5/12 Reading of current atomic stress average added
!   9/12 Unique sub-option added to mdarchive option
!  12/12 delayforce and endforce renamed to delay_force and end_force
!  12/12 delay_field and end_field added
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
!  Julian Gale, NRI, Curtin University, December 2012
!
  use configurations,     only : maxatot, ndimen
  use control
  use current
  use derivatives,        only : sumatomicstress
  use distances,          only : ndistancereset, extracutoff
  use dump
  use general
  use gulpinput
  use iochannels
  use mdlogic,            only : ladiabatic
  use moldyn
  use m_pr,               only : taubcfg, tautcfg, pr_conscfg, p_iso, p_flx
  use m_pr,               only : lpisoinput, lpflxinput
  use parallel
  use randomnumbers,      only : nrandomcalls, npr_randomcalls_adv, npr_grandomcalls_adv, lGaussianLast
  use shell
  use shellextrapolation
  use species,            only : maxspec
  use velocities
  implicit none
!
!  Passed variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: iline
  integer(i4)                  :: ncurr
  integer(i4)                  :: nru
  logical                      :: l55
  logical                      :: l1000
  logical                      :: lwordok
!
!  Local variables
!
  integer(i4)                  :: i
  integer(i4)                  :: iend
  integer(i4)                  :: ii
  integer(i4)                  :: inat
  integer(i4)                  :: inum
  integer(i4)                  :: itype
  integer(i4)                  :: j
  integer(i4)                  :: k
  integer(i4)                  :: l
  integer(i4)                  :: naccel
  integer(i4)                  :: nfl
  logical                      :: lsymbol
  real(dp)                     :: rnfl
  real(dp)                     :: units
!
  if (index(word,'times').eq.1) goto 100
  if (index(word,'equi').eq.1) goto 110
  if (index(word,'prod').eq.1) goto 120
  if (index(word,'samp').eq.1) goto 130
  if (index(word,'writ').eq.1) goto 140
  if (index(word,'tsca').eq.1) goto 150
  if (index(word,'curr').eq.1) goto 160
  if (index(word,'aver').eq.1) goto 170
  if (index(word,'velo').eq.1) goto 180
  if (index(word,'acce').eq.1) goto 190
  if (index(word,'abso').eq.1) goto 200
  if (index(word,'velm').eq.1) goto 210
  if (index(word,'mdco').eq.1) goto 220
  if (index(word,'delay_fo').eq.1) goto 230
  if (index(word,'teth').eq.1) goto 240
  if (index(word,'shel').eq.1) goto 250
  if (index(word,'cfav').eq.1) goto 260
  if (index(word,'end_fo').eq.1) goto 270
  if (index(word,'extr').eq.1) goto 280
  if (index(word,'mdar').eq.1) goto 290
  if (index(word,'iter').eq.1) goto 300
  if (index(word,'rese').eq.1) goto 310
  if (index(word,'cave').eq.1) goto 320
  if (index(word,'ense').eq.1) goto 330
  if (index(word,'inte').eq.1) goto 340
  if (index(word,'cato').eq.1) goto 350
  if (index(word,'mdmaxt').eq.1) goto 380
  if (index(word,'mdmaxv').eq.1) goto 390
  if (index(word,'tau_b').eq.1) goto 400
  if (index(word,'tau_t').eq.1) goto 410
  if (index(word,'intc').eq.1) goto 420
  if (index(word,'rand').eq.1) goto 430
  if (index(word,'p_is').eq.1) goto 440
  if (index(word,'p_fl').eq.1) goto 450
  if (index(word,'delay_fi').eq.1) goto 460
  if (index(word,'end_fi').eq.1) goto 470
  return
!*************
!  Timestep  *
!*************
100 if (nfloat.eq.0) then
    call outerror('timestep is missing from input',iline)
    call stopnow('mdword')
  endif
!
!  Default units = ps
!
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  tstep(ncurr) = floats(1)*units
  lwordok = .true.
  return
!************************
!  Equilibration time  *
!************************
110 if (nfloat.eq.0) then
    call outerror('equilibration time is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (floats(1).lt.0.0_dp) units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  nfl = nint(floats(1))
  rnfl = dble(nfl)
  if (abs(floats(1)-rnfl).gt.1.0d-4) then
!
!  Floating point number must be time in default units
!
    tmdeq(ncurr) = floats(1)*units
  elseif (units.eq.0.0_dp) then
!
!  Input is number of timesteps
!
    nmdeq(ncurr) = nint(floats(1))
  else
!
!  Input is in units of time
!
    tmdeq(ncurr) = floats(1)*units
  endif
  lwordok = .true.
  return
!********************
!  Production time  *
!********************
120 if (nfloat.eq.0) then
    call outerror('production time is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (floats(1).lt.0.0_dp) units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  nfl = nint(floats(1))
  rnfl = dble(nfl)
  if (abs(floats(1)-rnfl).gt.1.0d-4) then
!
!  Floating point number must be time in default units
!
    tmdprod(ncurr) = floats(1)*units
  elseif (units.eq.0.0_dp) then
!
!  Input is number of timesteps
!
    nmdprod(ncurr) = nint(floats(1))
  else
!
!  Input is in units of time
!
    tmdprod(ncurr) = floats(1)*units
  endif
  lwordok = .true.
  return
!***********************
!  Sampling frequency  *
!***********************
130 if (nfloat.eq.0) then
    call outerror('sampling time is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 0.0_dp
  if (floats(1).lt.1.0_dp) units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  if (units.eq.0.0_dp) then
!
!  Input is number of timesteps
!
    nmdsamp(ncurr) = nint(floats(1))
  else
!
!  Input is in units of time
!
    tmdsamp(ncurr) = floats(1)*units
  endif
  lwordok = .true.
  return
!**************************************
!  Frequency for writing MD dumpfile  *
!**************************************
140 if (nfloat.eq.0) then
    call outerror('write interval time is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 0.0_dp
  if (floats(1).lt.0.0_dp) units=1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  if (units.eq.0.0_dp) then
!
!  Input is number of timesteps
!
    nmdwrite(ncurr) = nint(floats(1))
  else
!
!  Input is in units of time
!
    tmdwrite(ncurr) = floats(1)*units
  endif
  lwordok = .true.
  return
!******************************************************
!  Length of time for which scaling is to be applied  *
!******************************************************
150 if (nfloat.eq.0) then
    call outerror('scaling time is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  tmdscale(ncurr) = floats(1)*units
  if (nfloat.gt.1) tmdscint(ncurr) = floats(2)*units
  lwordok = .true.
  return
!***********************************
!  Current simulation time so far  *
!***********************************
160 if (nfloat.eq.0) then
    call outerror('current_time value is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  timesofar = floats(1)*units
  ncfmdrun = ncurr
  lwordok = .true.
  return
!***************************
!  Running average totals  *
!***************************
170 if (nfloat.ge.5) then
    sumvsq = floats(1)
    sumener = floats(2)
    sumvir = floats(3)
    sumtem = floats(4)
    sumcons = floats(5)
  elseif (nfloat.eq.4) then
    sumvsq = floats(1)
    sumener = floats(2)
    sumvir = floats(3)
    sumtem = floats(4)
    sumcons = 0.0_dp
  elseif (nfloat.eq.3) then
    sumvsq = floats(1)
    sumener = floats(2)
    sumvir = floats(3)
    sumtem = 0.0_dp
    sumcons = 0.0_dp
  elseif (nfloat.eq.2) then
    sumvsq = floats(1)
    sumener = floats(2)
    sumvir = 0.0_dp
    sumtem = 0.0_dp
    sumcons = 0.0_dp
  elseif (nfloat.eq.1) then
    sumvsq = floats(1)
    sumener = 0.0_dp
    sumvir = 0.0_dp
    sumtem = 0.0_dp
    sumcons = 0.0_dp
  endif
  line = '  '
  read(nru,'(a)',end=175) line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nfloat.ge.2) then
    sumcst = floats(1)
    naverpt = nint(floats(2))
  elseif (nfloat.eq.1) then
    sumcst = floats(1)
    naverpt = 0
  else
    goto 175
  endif
  lwordok = .true.
  return
175 call outerror('average values are missing from input',iline)
  call stopnow('mdword')
!***************
!  Velocities  *
!***************
180 line = '  '
  linputvelc   = .true.
  linputvelxyz = .true.
  read(nru,'(a)',end=185) line
  iline = iline + 1
  call linepro(nru,line,iline)
  call stolc(words(1),80_i4)
  if (nword.gt.0.and.index(words(1),'cve').ne.1.and.index(words(1),'ther').ne.1) then
    l55 = .true.
    goto 185
  endif
  if (nword.gt.0) then
    if (index(words(1),'cve').eq.1) then
      if (nfloat.lt.4) then
        call outerror('velocities are missing from input',iline)
        call stopnow('mdword')
      endif
      ii = nint(floats(1))
      ii = 3*(ii - 1)
      if (ii.eq.0.or.ii.eq.3) then
        velc(ii+1) = floats(2)
        velc(ii+2) = floats(3)
        velc(ii+3) = floats(4)
      endif
    elseif (index(words(1),'ther').eq.1) then
      sfac = floats(1)
      sumsfac = floats(2)
    endif
  elseif (nfloat.gt.0) then
    if (nfloat.lt.4) then
      call outerror('velocities are missing from input',iline)
      call stopnow('mdword')
    endif
    ii = nint(floats(1))
    if (ii.gt.maxat) then
      maxat = ii
      call changemaxat
    endif
    velx(ii) = floats(2)
    vely(ii) = floats(3)
    velz(ii) = floats(4)
  endif
  goto 180
185 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!******************
!  Accelerations  *
!******************
190 if (nfloat.gt.0) then
    naccel = nint(floats(1))
    if (naccel.ne.1.and.naccel.ne.4) then
      call outerror('invalid number of accelerations',iline)
      call stopnow('mdword')
    endif
  else
    call outerror('no. of accelerations is missing from input',iline)
    call stopnow('mdword')
  endif
192 line = '  '
  read(nru,'(a)',end=195) line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nword.gt.0) then
    l55 = .true.
    goto 195
  elseif (nfloat.eq.0) then
    goto 192
  endif
  if ((nfloat.lt.7.and.naccel.eq.4).or.(nfloat.lt.4.and.naccel.eq.1)) then
    call outerror('accelerations are missing from input',iline)
    call stopnow('mdword')
  endif
  ii = nint(floats(1))
  if (ii.gt.maxat) then
    maxat = ii
    call changemaxat
  endif
  x2(ii) = floats(2)
  y2(ii) = floats(3)
  z2(ii) = floats(4)
  if (naccel.eq.1) goto 192
  x3(ii) = floats(5)
  y3(ii) = floats(6)
  z3(ii) = floats(7)
  line = '  '
  read(nru,'(a)',end=195)line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nword.gt.0) then
    l55 = .true.
    goto 195
  endif
  if (nfloat.lt.6) then
    call outerror('accelerations are missing from input',iline)
    call stopnow('mdword')
  endif
  x4(ii) = floats(1)
  y4(ii) = floats(2)
  z4(ii) = floats(3)
  x5(ii) = floats(4)
  y5(ii) = floats(5)
  z5(ii) = floats(6)
  goto 192
195 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***********************************
!  Absolute cartesian coordinates  *
!***********************************
200 continue
202 line = '  '
  read(nru,'(a)',end=205) line
  iline = iline + 1
  call linepro(nru,line,iline)
  call stolc(words(1),80_i4)
  if (nword.gt.0.and.index(words(1),'cve').ne.1) then
    l55 = .true.
    goto 205
  endif
  if (nword.gt.0) then
    if (nfloat.lt.4) then
      call outerror('absolute coordinates are missing from input',iline)
      call stopnow('mdword')
    endif
    ii = nint(floats(1))
    ii = 3*(ii-1)
    if (ii.ge.0.and.ii.le.6) then
      xcell(ii+1) = floats(2)
      xcell(ii+2) = floats(3)
      xcell(ii+3) = floats(4)
    endif
  else
    if (nfloat.lt.4) then
      call outerror('absolute coordinates are missing from input',iline)
      call stopnow('mdword')
    endif
    ii = nint(floats(1))
    if (ii.gt.maxat) then
      call outerror('atom pointer is too large in absolute coords',iline)
      call stopnow('mdword')
    endif
    xabsco(ii) = floats(2)
    yabsco(ii) = floats(3)
    zabsco(ii) = floats(4)
  endif
  goto 202
205 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***************************
!  Maximum velocity ratio  *
!***************************
210 if (nfloat.eq.0) then
    call outerror('velmax is missing from input',iline)
    call stopnow('mdword')
  endif
  velmax = floats(1)
  lwordok = .true.
  return
!******************
!  MD constraint  *
!******************
220 if (nfloat.lt.3) then
    call outerror('insufficient constraint data for MD',iline)
    call stopnow('mdword')
  endif
  lmdconstrain(ncurr) = .true.
  nmdconstrainatom(1,ncurr) = abs(nint(floats(1)))
  nmdconstrainatom(2,ncurr) = abs(nint(floats(2)))
  nmdconstraindist(ncurr) = abs(floats(3))
  if (nmdconstrainatom(1,ncurr).eq.nmdconstrainatom(2,ncurr)) then
    call outerror('cannot constrain an atom to itself in MD',iline)
    call stopnow('mdword')
  endif
  if (nmdconstrainatom(1,ncurr).eq.0) then
    call outerror('atom number for MD constraint is 0',iline)
    call stopnow('mdword')
  endif
  if (nmdconstrainatom(2,ncurr).eq.0) then
    call outerror('atom number for MD constraint is 0',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!*********************************
!  Force application delay time  *
!*********************************
230 if (nfloat.eq.0) then
    call outerror('delay time for force is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  tmdforcestart(ncurr) = abs(floats(1))*units
  lwordok = .true.
  return
!*************************************
!  Atoms to be kept fixed during MD  *
!*************************************
240 i = index(line,' ')
241 line = line(i+1:)
  j = index(line,',')
  k = index(line,'-')
  if ((j.lt.k.and.k.ne.0.and.j.ne.0).or.(j.ne.0.and.k.eq.0)) then
    read(line(1:j-1),*) inum
!
!  Check that inum doesn't exceed array dimension
!
    if (inum.gt.maxatot) then
      call outerror('atom number for tethering exceeds number of atoms',0_i4)
      call stopnow('mdword')
    endif
!
    lfix(inum) = .true.
    i = j
    goto 241
  endif
  if (k.lt.j.and.j.ne.0.and.k.ne.0) then
    read(line(1:k-1),*) inum
    read(line(k+1:j-1),*) iend
!
!  Check that iend doesn't exceed array dimension
!
    if (iend.gt.maxatot) then
      call outerror('atom number for tethering exceeds number of atoms',0_i4)
      call stopnow('mdword')
    endif
!
    do l = inum,iend
      lfix(l) = .true.
    enddo
    i = j
    goto 241
  endif
  if (j.eq.0.and.k.ne.0) then
    read(line(1:k-1),*) inum
    read(line(k+1:),*) iend
!
!  Check that iend doesn't exceed array dimension
!
    if (iend.gt.maxatot) then
      call outerror('atom number for tethering exceeds number of atoms',0_i4)
      call stopnow('mdword')
    endif
!
    do l = inum,iend
      lfix(l) = .true.
    enddo
  endif
  if (j.eq.0.and.k.eq.0) then
    read(line(1:),*,end=242) inum
!
!  Check that inum doesn't exceed array dimension
!
    if (inum.gt.maxatot) then
      call outerror('atom number for tethering exceeds number of atoms',0_i4)
      call stopnow('mdword')
    endif
!
    if (inum.ne.0) lfix(inum) = .true.
  endif
  ltethered = .true.
  lwordok = .true.
  return
242 call outerror('atom specifications missing from input',iline)
  call stopnow('mdword')
!********************************
!  Mass ratio of shell to core  *
!********************************
250 ladiabatic = .false.
255 line = '  '
  read(nru,'(a)',end=258) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 255
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 255
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      call stolc(word,20_i4)
      if (index(word,'end').eq.1) then
        lwordok = .true.
        return
      else
        l55 = .true.
        goto 258
      endif
    endif
  endif
  nratiomspec = nratiomspec + 1
  if (nratiomspec.gt.maxspec) then
    maxspec = nratiomspec + 20
    call changemaxspec
  endif
  if (nword.eq.0) then
    inat = int(floats(1))
    if (inat.gt.100) inat = inat - 100
    natratiomspec(nratiomspec) = inat
    ntypratiomspec(nratiomspec) = 0
    ratiomspec(nratiomspec) = floats(2)
  elseif (nword.ge.1) then
    call ltont(words(1),inat,itype)
    natratiomspec(nratiomspec) = inat
    ntypratiomspec(nratiomspec) = itype
    ratiomspec(nratiomspec) = floats(1)
  endif
  goto 255
258 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************************************
!  Running average totals for constraint forces  *
!*************************************************
260 if (nfloat.ge.2) then
    sumlambdaR = floats(1)
    sumlambdaV = floats(2)
  else
    call outerror('wrong number of lambdas for average',iline)
    call stopnow('mdword')
  endif
  lwordok=.true.
  return
!*******************************
!  Force application end time  *
!*******************************
270 if (nfloat.eq.0) then
    call outerror('end time for force is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  tmdforcestop(ncurr) = abs(floats(1))*units
  lwordok = .true.
  return
!************************************
!  Extra cutoff for vector storage  *
!************************************
280 if (nfloat.gt.0) then
    extracutoff = abs(floats(1))
  endif
  lwordok = .true.
  return
!**********************************
!  File name for MD archive file  *
!**********************************
290 if (nword.gt.1) then
    if (nword.gt.2) then
!
!  Save word in case lower case conversion alters it unnecessarily
!
      words(4) = words(2)
      call stolc(words(2),80_i4)
      if (index(words(2),'uni').eq.1) then
        lmdarcunique = .true.
        mdafil = words(3)
      else
        mdafil = words(4)
      endif
    else
      mdafil = words(2)
    endif
  else
    call outerror('filename for MD archive is missing from input',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!***********************************************************************
!  Maximum number of iterations for shell position optimisation in MD  *
!***********************************************************************
300 if (nword.gt.1) then
    call stolc(words(2),80_i4)
    lextrapolateshells = (index(words(2),'noex').ne.1)
    if (lextrapolateshells) then
      maxextrapol = 8
      call changemaxextrapol
    endif
  else
    maxextrapol = 8
    call changemaxextrapol
  endif
  if (nfloat.ge.3) then
    moptit = int(floats(1)) - 1
    sgtol = abs(floats(2))
    if (sgtol.ge.1.0_dp) then
      sgtol = 10.0_dp**(-sgtol)
    endif
    maxextrapol = nint(abs(floats(3)))
    call changemaxextrapol
  elseif (nfloat.eq.2) then
    if (lextrapolateshells) then
      moptit = int(floats(1)) - 1
      maxextrapol = nint(abs(floats(2)))
      call changemaxextrapol
    else
      moptit = int(floats(1)) - 1
      sgtol = abs(floats(2))
      if (sgtol.ge.1.0_dp) then
        sgtol = 10.0_dp**(-sgtol)
      endif
    endif
  elseif (nfloat.eq.1) then
    moptit = int(floats(1)) - 1
  else
    if (ioproc) then
      write(ioout,'(/,''  **** Error - values for maximum number of iterations and gradient ****'')')
      write(ioout,'(  ''  **** threshold for shell position  optimisation are  missing from ****'')')
      write(ioout,'(  ''  **** specification at line '',i3,36x,''****'')') iline
    endif
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!****************************************************
!  Frequency of reset for interatomic vector table  *
!****************************************************
310 if (nfloat.gt.0) then
    ndistancereset(ncurr) = max(nint(floats(1)),1)
  endif
  lwordok = .true.
  return
!************************************
!  Running average totals for cell  *
!************************************
320 if (nfloat.ge.3) then
    sumacell = floats(1)
    sumbcell = floats(2)
    sumccell = floats(3)
  else
    call outerror('wrong number of cells for average',iline)
    call stopnow('mdword')
  endif
  line = '  '
  read(nru,'(a)',end=325) line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nfloat.ge.4) then
    sumalpcell = floats(1)
    sumbetcell = floats(2)
    sumgamcell = floats(3)
    sumvol = floats(4)
  elseif (nfloat.eq.3) then
    sumalpcell = floats(1)
    sumbetcell = floats(2)
    sumgamcell = floats(3)
    sumvol = 0.0_dp
  else
    call outerror('wrong number of cells for average',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
325 l1000 = .true.
  return
!******************
!  Ensemble type  *
!******************
330 if (nword.ge.2) then
    call stolc(words(2),80_i4)
    if (index(words(2),'nve').ne.0) then
      nensemble(ncurr) = 1
    elseif (index(words(2),'nvt').ne.0) then
      nensemble(ncurr) = 2
      if (nfloat.ge.1) then
        qtemp(ncurr) = abs(floats(1))
      endif
    elseif (index(words(2),'npt').ne.0) then
      nensemble(ncurr) = 3
      if (nfloat.ge.1) then
        qtemp(ncurr) = abs(floats(1))
      endif
      if (nfloat.ge.2) then
        qpres(ncurr) = abs(floats(2))
      endif
    elseif (index(words(2),'nph').ne.0) then
      nensemble(ncurr) = 4
      if (nfloat.ge.1) then
        qpres(ncurr) = abs(floats(1))
      endif
    else
      call outerror('invalid ensemble specified for MD',iline)
      call stopnow('mdword')
    endif
  else
    call outerror('ensemble name is missing from input',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!********************
!  Integrator type  *
!********************
340 if (nword.ge.2) then
    call stolc(words(2),80_i4)
    if (index(words(2),'gear').ne.0) then
      nmdintegrator = 1
    elseif (index(words(2),'vel').ne.0) then
      nmdintegrator = 2
    elseif (index(words(2),'lea').ne.0) then
      nmdintegrator = 3
      if (nfloat.ge.1) then
        nmditer = abs(nint(floats(1)))
      endif
    elseif (index(words(2),'ver').ne.0) then
      nmdintegrator = 3
      if (nfloat.ge.1) then
        nmditer = abs(nint(floats(1)))
      endif
    elseif (index(words(2),'pao').ne.0) then
      nmdintegrator = 4
    elseif (index(words(2),'sto').ne.0) then
      nmdintegrator = 4
    else
      call outerror('invalid integrator specifed for MD',iline)
      call stopnow('mdword')
    endif
  else
    call outerror('integrator type is missing from input',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!**************************
!  Average atomic stress  *
!**************************
350 line = '  '
  read(nru,'(a)',end=355) line
  iline = iline + 1
  call linepro(nru,line,iline)
  call stolc(words(1),80_i4)
  if (nword.gt.0) then
    l55 = .true.
    goto 355
  endif
  if (nfloat.gt.0) then
    if (ndimen(ncurr).eq.3) then
      if (nfloat.lt.7) then
        call outerror('atomic stresses are missing from input',iline)
        call stopnow('mdword')
      endif
      ii = nint(floats(1))
      if (ii.gt.maxat) then
        maxat = ii
        call changemaxat
      endif
      sumatomicstress(1,ii) = floats(2)
      sumatomicstress(2,ii) = floats(3)
      sumatomicstress(3,ii) = floats(4)
      sumatomicstress(4,ii) = floats(5)
      sumatomicstress(5,ii) = floats(6)
      sumatomicstress(6,ii) = floats(7)
    elseif (ndimen(ncurr).eq.2) then
      if (nfloat.lt.4) then
        call outerror('atomic stresses are missing from input',iline)
        call stopnow('mdword')
      endif
      ii = nint(floats(1))
      if (ii.gt.maxat) then
        maxat = ii
        call changemaxat
      endif
      sumatomicstress(1,ii) = floats(2)
      sumatomicstress(2,ii) = floats(3)
      sumatomicstress(3,ii) = floats(4)
    elseif (ndimen(ncurr).eq.1) then
      if (nfloat.lt.2) then
        call outerror('atomic stresses are missing from input',iline)
        call stopnow('mdword')
      endif
      ii = nint(floats(1))
      if (ii.gt.maxat) then
        maxat = ii
        call changemaxat
      endif
      sumatomicstress(1,ii) = floats(2)
    endif
  endif
  goto 350
355 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************************************
!  Maximum temperature trap for MD runs  *
!*****************************************
380 if (nfloat.eq.0) then
    call outerror('value is missing from input for mdmaxtemp',iline)
    call stopnow('mdword')
  endif
  rmdmaxtemp = abs(floats(1))
  if (rmdmaxtemp.le.1.0_dp) then
    call outerror('mdmaxtemp value cannot be less than or equal to one',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!************************************
!  Maximum volume trap for MD runs  *
!************************************
390 if (nfloat.eq.0) then
    call outerror('value is missing from input for mdmaxvolume',iline)
    call stopnow('mdword')
  endif
  rmdmaxvol = abs(floats(1))
  if (rmdmaxvol.le.1.0_dp) then
    call outerror('mdmaxvolume value cannot be less than or equal to one',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!*****************************
!  Barostat relaxation time  *
!*****************************
400 if (nfloat.eq.0) then
    call outerror('tau_barostat is missing from input',iline)
    call stopnow('mdword')
  endif
!
!  Default units = ps
!
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  taubcfg(ncurr) = abs(floats(1)*units)
  if (taubcfg(ncurr).lt.1.0d-12) then
    call outerror('barostat relaxation time is too small',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!*******************************
!  Thermostat relaxation time  *
!*******************************
410 if (nfloat.eq.0) then
    call outerror('tau_thermostat is missing from input',iline)
    call stopnow('mdword')
  endif
!
!  Default units = ps
!
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  tautcfg(ncurr) = abs(floats(1)*units)
  if (tautcfg(ncurr).lt.1.0d-12) then
    call outerror('thermostat relaxation time is too small',iline)
    call stopnow('mdword')
  endif
  lwordok = .true.
  return
!***********************************
!  Integral of conserved quantity  *
!***********************************
420 if (nfloat.eq.0) then
    call outerror('intconserved is missing from input',iline)
    call stopnow('mdword')
  endif
  pr_conscfg(ncurr) = floats(1)
  lwordok = .true.
  return
!***************************
!  Random number counters  *
!***************************
430 if (nfloat.lt.3) then
    call outerror('number of calls is missing from input for random',iline)
    call stopnow('mdword')
  endif
  nrandomcalls = nint(floats(1))
  npr_randomcalls_adv = nint(floats(2))
  npr_grandomcalls_adv = nint(floats(3))
  if (nword.gt.1) then
    if (index(words(2),'G').eq.1.or.index(words(2),'g').eq.1) then
      lGaussianLast = .true.
    else
      lGaussianLast = .false.
    endif
  endif
  lwordok = .true.
  return
!*************************
!  p_iso for MD restart  *
!*************************
440 if (nfloat.lt.1) then
    call outerror('value of p_iso is missing from input',iline)
    call stopnow('mdword')
  endif
  p_iso = floats(1)
  lpisoinput = .true.
  lwordok = .true.
  return
!*************************
!  p_flx for MD restart  *
!*************************
450 continue
  line = '  '
  read(nru,'(a)') line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nfloat.lt.3) then
    call outerror('insufficient values for p_flx in input',iline)
    call stopnow('mdword')
  endif
  p_flx(1,1) = floats(1)
  p_flx(2,1) = floats(2)
  p_flx(3,1) = floats(3)
!
  line = '  '
  read(nru,'(a)') line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nfloat.lt.3) then
    call outerror('insufficient values for p_flx in input',iline)
    call stopnow('mdword')
  endif
  p_flx(1,2) = floats(1)
  p_flx(2,2) = floats(2)
  p_flx(3,2) = floats(3)
!
  line = '  '
  read(nru,'(a)') line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nfloat.lt.3) then
    call outerror('insufficient values for p_flx in input',iline)
    call stopnow('mdword')
  endif
  p_flx(1,3) = floats(1)
  p_flx(2,3) = floats(2)
  p_flx(3,3) = floats(3)
  lpflxinput = .true.
  lwordok = .true.
  return
!*********************************
!  Field application delay time  *
!*********************************
460 if (nfloat.eq.0) then
    call outerror('delay time for field is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  tmdfieldstart(ncurr) = abs(floats(1))*units
  lwordok = .true.
  return
!*******************************
!  Field application end time  *
!*******************************
470 if (nfloat.eq.0) then
    call outerror('end time for field is missing from input',iline)
    call stopnow('mdword')
  endif
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),80_i4)
    if (index(words(2),'s').eq.1) then
      units = 1.0d+12
    elseif (index(words(2),'ns').eq.1) then
      units = 1.0d+3
    elseif (index(words(2),'ps').eq.1) then
      units = 1.0_dp
    elseif (index(words(2),'fs').eq.1) then
      units = 1.0d-3
    endif
  endif
  tmdfieldstop(ncurr) = abs(floats(1))*units
  lwordok = .true.
  return
!
  end
