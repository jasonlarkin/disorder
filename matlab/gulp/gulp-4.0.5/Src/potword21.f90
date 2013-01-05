  subroutine potword21(iin,word,lwordok,iline,line,l55,l1000,linr,lint,llibrary,lfflags,ncurr)
!
!  Processes potential input for two-body potentials
!
!  iin = input fortran channel
!
!  Potential types given by nptype():
!
!   1 = Buckingham
!   2 = Lennard - Jones
!   3 = Morse 
!   4 = Morse with coulomb subtraction
!   5 = Harmonic
!   6 = Harmonic with coulomb subtraction
!   7 = General (Del Re)
!   8 = Spring (core-shell only)
!   9 = Coulomb subtract
!  10 = Four range Buckingham
!  11 = Spline potential
!  12 = Lennard-Jones with epsilon/sigma input : sigma = r when E = 0
!  13 = Lennard-Jones with epsilon/sigma input : sigma = r at minimum
!  14 = BSM - breathing shell model
!  15 = Stillinger-Weber 2 body potential
!  16 = Inverse gaussian potential
!  17 = BSM - exponential breathing shell model
!  18 = damped dispersion potential
!  19 = Many-body potential for metals
!  20 = Rose-Smith-Guinea-Ferrante potential
!  21 = Lennard-Jones with epsilon/sigma input : ESFF combination rules
!  22 = Q taper (short range Coulomb taper)
!  23 = Polynomial of order n (n >= 1)
!  24 = qerfc (Coulomb with erfc) 
!  25 = Covalent Exponential form
!  26 = Fermi-Dirac potential
!  27 = Lennard-Jones with shift
!  28 = Squared harmonic
!  29 = Squared harmonic with Coulomb correction
!  30 = Tsuneyuki Coulomb correction potential
!  31 = BSM - single exponential breathing shell model
!  32 = Stillinger-Weber with bond softening
!  33 = Spring with cosh functional form
!  34 = EAM potential shift
!  35 = poly harmonic
!  36 = qoverr2
!  37 = force constant
!  38 = sr_glue
!  39 = Morse with inverse exponential taper
!  40 = Morse with inverse exponential taper with Coloumb subtraction
!  41 = Mei-Davenport
!  42 = erferfc potential
!  43 = reperfc potential
!  44 = erfpot potential
!  45 = Baskes potential
!  46 = VBO_twobody potential
!  47 = exppowers potential
!  48 = Grimme_C6
!  49 = cfm_harmonic
!  50 = cfm_gaussian
!  51 = cfm_power
!  52 = cfm_fermi
!  53 = gcoulomb
!  54 = Becke_Johnson_C6
!
!  Potentials 12 and 13 can be used with combining rule option
!
!  llibrary => if .true. then check species are present before
!              accepting potential
!
!  Molecular Mechanics:
!  --------------------
!
!  mmexc specifies potential type for molecular mechanics calculation:
!
!    0 => bonded and nonbonded potential (default)
!    1 => bonded only potential 
!    2 => nonbonded only potential (excludes atoms up to 1 bond away)
!    3 => nonbonded only potential (excludes atoms up to 2 bonds away)
!    4 => 1-4 interaction potential only (intra)
!    5 => 1-4 and greater (intra and inter)
!
!  option word "molmec" after potential name selects default MM type
!  for marvin compatibility
!
!   3/95 Stillinger-Weber twobody potential added
!   3/95 General specification of input channel added
!   4/95 Library species check added
!   4/95 A/B combination rules added for lennard-jones
!   5/96 Igauss added
!   2/97 Exponential breathing and damped dispersion potentials added
!   4/97 EAM potential added
!   7/97 Rose-Smith-Guinea-Ferrante potential added
!   4/98 ESFF form of Lennard-Jones combination rules added
!   4/98 Assignment of species type for library call changed
!        so that potential file form is accepted
!  10/98 Coding of fitting variables simplified
!   5/00 C10 term added to damped dispersion potential
!   6/00 Option to truncate distance search for valid manybody pots
!        added
!   7/00 CovExp added
!   6/01 Initialisation of line added for benefit of some compilers
!   5/02 Shift multiplier for each configuration added
!   5/02 Brenner potential added
!  12/02 Automatic generation of L-J potentials for combination rules added
!   3/04 Squared harmonic potential added
!   3/04 Geomtric combination rule added for L-J potential
!  10/04 Calls to getpotsymbol introduced to reduce code size
!  10/04 Setting of species in auto-LJ corrected for case where the atomic
!        numbers are the same, but the types are different
!  12/04 Missing initialisation of nbeg added for lennard-jones with combi
!   4/05 cosh spring potential added
!   7/05 Error in handling of potcut calls for multiple potentials in the
!        same read fixed
!  10/05 Option for Voter style tapering of Morse added
!  11/05 EAM potential shift added
!   3/06 Poly harmonic added
!   3/06 Error in fitting flag assignment for polynomial fixed
!   5/06 Format of warning statements updated
!   7/06 Voter suboption removed from Morse
!   8/06 ltype01 added to okspec calls
!   8/06 iin passed to linepro
!   8/06 Separate flag added for fitting flags
!   8/06 Bonding type sub-option added
!   9/06 Literal symbols returned from getpotsymbol2
!  11/06 Calls to init2bodydefaults added to guard against hangovers from 
!        potentials that might have got rejected after changing initial values.
!  11/06 Bug in ordering of species in autolj set up for epsilon/sigma case fixed
!   1/07 Force constant potential added
!   1/07 Amide bond type added
!   5/07 UFF1 input added
!   5/07 Bond charge increments added
!   5/07 Morse with inverse exponential taper added
!   5/07 Handling of symbol2 corrected when changing species order
!   7/07 UFF symbol saved to an array on reading
!   3/08 erferfc, erf & reperfc potentials added
!   4/08 Conversion of units for UFFtor added
!   5/08 New UFF generators for OOP added
!   5/08 UFFchi added
!  12/08 Module input renamed to gulpinput
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 Use of nfvar for two-body potentials modified
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   1/09 VBO_twobody potential added
!   3/09 Uninitialised case for npot1 corrected
!   7/09 Shifting of Morse minimum to zero added as an option
!   9/09 Maximum order of polynomial potential increased to 8
!   9/09 r0 added for standard polynomial potential
!  10/09 Handling of polynomial order read improved
!   1/10 One-body potential added
!   3/10 Analytic third derivatives disabled for EAM/MEAM by setting lnoanald3 to be
!        true if lsuttonc is set to true
!   5/10 gcoulomb potential added
!   7/10 Missing r0 handling for one case in polynomial potential added
!   1/12 Copying of scale14 when there are multiple potentials under the same option added
!   1/12 g12 mmexc=5 option added
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
!  Julian Gale, NRI, Curtin University, January 2012
!
  use bondcharge
  use constants
  use control
  use element, only : maxele
  use fitting
  use general, only : nwarn
  use gulpinput
  use iochannels
  use molecule
  use one
  use parallel
  use shell
  use shifts
  use species
  use splinedata
  use sutton
  use two
  use uffdata
  implicit none
!
!  Passed variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: iin
  integer(i4)                  :: iline
  integer(i4)                  :: ncurr
  logical                      :: l55
  logical                      :: l1000
  logical                      :: lfflags
  logical                      :: linr
  logical                      :: lint
  logical                      :: llibrary
  logical                      :: lwordok
!
!  Local variables
!
  character(len=5)             :: sym21
  character(len=5)             :: sym22
  integer(i4)                  :: i
  integer(i4)                  :: iadd
  integer(i4)                  :: ilp
  integer(i4)                  :: ind
  integer(i4)                  :: itype1
  integer(i4)                  :: itype2
  integer(i4)                  :: j
  integer(i4)                  :: n1
  integer(i4)                  :: n2
  integer(i4)                  :: n3
  integer(i4)                  :: n4
  integer(i4)                  :: n5
  integer(i4)                  :: n6
  integer(i4)                  :: nbeg
  integer(i4)                  :: nfit0
  integer(i4)                  :: norder
  integer(i4)                  :: np
  integer(i4)                  :: npot1
  integer(i4)                  :: nvar1
  integer(i4)                  :: nvar2
  logical                      :: lautolj
  logical                      :: letaper
  logical                      :: lk3
  logical                      :: lk4
  logical                      :: lsymbol
  logical                      :: lfound
  logical                      :: lok1
  logical                      :: lpharm
  logical                      :: ltype01
  logical                      :: lvalidpot
  logical                      :: lwarnp
  real(dp)                     :: const
  real(dp)                     :: ratio
  real(dp)                     :: rdiff
  real(dp)                     :: rm
  real(dp)                     :: rn
  real(dp)                     :: rscale
  real(dp)                     :: units
  real(dp)                     :: zero
!
!  Initialise local variables
!
  lwarnp = .not.lmol
  if (index(word,'buck4').eq.1) goto 740
  if (index(word,'ashi').eq.1) goto 170
  if (index(word,'buck').eq.1) goto 180
  if (index(word,'lenn').eq.1) goto 190
  if (index(word,'mors').eq.1) goto 200
  if (index(word,'epsi').eq.1) goto 210
  if (index(word,'atom').eq.1) goto 220
  if (index(word,'damp').eq.1) goto 230
  if (index(word,'many').eq.1) goto 240
  if (index(word,'scma').eq.1) goto 250
  if (index(word,'squa').eq.1) goto 260
  if (index(word,'qinc').eq.1) goto 270
  if (index(word,'sshi').eq.1) goto 290
  if (index(word,'shif').eq.1) goto 300
  if (index(word,'cove').eq.1) goto 310
  if (index(word,'cosh').eq.1) goto 320
  if (index(word,'erfe').eq.1) goto 330
  if (index(word,'repe').eq.1) goto 340
  if (index(word,'forc').eq.1) goto 350
  if (index(word,'harm').eq.1) goto 360
  if (index(word,'eam_p').eq.1) goto 370
  if (index(word,'uff1').eq.1) goto 380
  if (index(word,'erfp').eq.1) goto 390
  if (index(word,'poly').eq.1) goto 400
  if (index(word,'gcou').eq.1) goto 410
  return
!*************************************************
!  Parameters for one-body self energy / ashift  *
!*************************************************
170 if (none.eq.maxone) then
    maxone = none + 10
    call changemaxone
  endif
  call init1bodydefaults(none+1_i4)
!
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      endif
      i = i + 1
    enddo
  endif
  npot1 = none + 1
175 line  =  '  '
  read(iin,'(a)',end=188) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 175
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 178
    endif
  endif
  none = none + 1
  if (none.gt.maxone) then
    maxone  =  none + 10
    call changemaxone
  endif
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit  =  nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 38
      nfvar(nfit) = none
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym21,0_i4,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    none = none - 1
    nfit = nfit0
    goto 175
  endif
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.1) then
    onepot(none) = floats(1+nbeg)*units
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword21')
  endif
  nspec11(none) = nvar1
  nptyp11(none) = itype1
  symbol1(none) = sym21
  goto 175
178 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!****************************************
!  Parameters for Buckingham potential  *
!****************************************
180 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'ener').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .false.
      elseif (index(words(i),'grad').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
185 line = '  '
  read(iin,'(a)',end=188) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 185
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 188
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-2))
    n2 = int(floats(nfloat-1))
    n3 = int(floats(nfloat))
    nfloat = nfloat - 3
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 3
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 185
  endif
  if (mmexc(npote).eq.1) then
!
!  If number of floats is greater than 3, assume that fitting flags have been left on line
!
    if (nfloat.gt.3) nfloat = nfloat - 3
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
!
!  If number of floats is greater than 5, assume that fitting flags have been left on line
!
    if (nfloat.gt.5) nfloat = nfloat - 3
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
      rpot2(npote)    = floats(4+nbeg)
      rpot(npote)      = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
      rpot(npote)     = floats(4+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = 0.0_dp
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
!
!  Check rho is non-zero
!
  if (twopot(2,npote).lt.1.0d-8) then
    call outerror('Buckingham potential has rho of zero',iline)
    call stopnow('potword21')
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 1
  goto 185
188 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!*******************************************
!  Parameters for Lennard-Jones potential  *
!*******************************************
190 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  tpot(1,npote+1) = 12.0_dp
  tpot(2,npote+1) = 6.0_dp
  nptype(npote+1) = 2
  ncombipower(npote+1) = 6
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lautolj = .false.
  iadd = 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'all').eq.1) then
        lautolj = .true.
      elseif (index(words(i),'eps').eq.1) then
        nptype(npote+1) = 12
      elseif (index(words(i),'zer').eq.1) then
        iadd = 0
      elseif (index(words(i),'esf').eq.1) then
        nptype(npote+1) = 21
        lcombine(npote+1) = .true.
        tpot(1,npote+1) = 9.0_dp
        tpot(2,npote+1) = 6.0_dp
      elseif (index(words(i),'zer').eq.1) then
        iadd = 0
      elseif (index(words(i),'com').eq.1) then
        lcombine(npote+1) = .true.
      elseif (index(words(i),'geo').eq.1) then
        ncombipower(npote+1) = 2
        lcombine(npote+1) = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'ener').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .false.
      elseif (index(words(i),'grad').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
    if (nptype(npote+1).eq.12) nptype(npote+1) = nptype(npote+1) + iadd
    if (nptype(npote+1).eq.21.and.(nint(tpot(1,npote+1)).ne.9.or.nint(tpot(2,npote+1)).ne.6)) then
      call outerror('Lennard-Jones ESFF is only allowed for 9/6 form',iline)
      call stopnow('potword21')
    endif
  endif
  if (.not.lcombine(npote+1)) lautolj = .false.
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  Set exponents if specified
!
  if (nfloat.ge.2) then
    tpot(1,npote+1) = floats(1)
    tpot(2,npote+1) = floats(2)
  elseif (nfloat.eq.1) then
    tpot(1,npote+1) = floats(1)
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.ge.3) then
    rscale = abs(floats(3))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
195 line = '  '
  read(iin,'(a)',end=198) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 195
  if (nword.gt.0.and..not.lautolj) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 198
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  tpot(1,npote) = tpot(1,npot1)
  tpot(2,npote) = tpot(2,npot1)
  scale14(npote) = scale14(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  nptype(npote) = nptype(npot1)
  lcombine(npote) = lcombine(npot1)
  ncombipower(npote) = ncombipower(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and..not.lcombine(npote).and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 2
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  if (.not.lautolj) then
    call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
    if (.not.lvalidpot) then
      npote = npote - 1
      nfit = nfit0
      goto 195
    endif
  else
    nbeg = 0
  endif
  if (nfloat.gt.4) nfloat = nfloat - 2
  if (lcombine(npote)) then
!
!  Combination rules - coefficients not input, but worked out
!  zero apot indicates combination potential
!
    if (mmexc(npote).eq.1) then
      twopot(1,npote) = 0.0_dp
      twopot(2,npote) = 0.0_dp
    else
      twopot(1,npote) = 0.0_dp
      twopot(2,npote) = 0.0_dp
      if (nfloat.eq.2) then
        rpot2(npote) = floats(1+nbeg)
        rpot(npote)  = floats(2+nbeg)
      elseif (nfloat.eq.1) then
        rpot2(npote) = 0.0_dp
        rpot(npote)  = floats(1+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  else
!
!  Assign coefficients and cutoffs
!
    if (mmexc(npote).eq.1) then
      if (nfloat.ge.2) then
        twopot(1,npote) = floats(1+nbeg)*units
        if (nptype(npote).eq.2) then
          twopot(2,npote) = floats(2+nbeg)*units
        else
          twopot(2,npote) = floats(2+nbeg)
        endif
      elseif (nfloat.eq.1) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(2,npote) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    else
      if (nfloat.eq.4) then
        twopot(1,npote) = floats(1+nbeg)*units
        if (nptype(npote).eq.2) then
          twopot(2,npote) = floats(2+nbeg)*units
        else
          twopot(2,npote) = floats(2+nbeg)
        endif
        rpot2(npote) = floats(3+nbeg)
        rpot(npote)  = floats(4+nbeg)
      elseif (nfloat.eq.3) then
        twopot(1,npote) = floats(1+nbeg)*units
        if (nptype(npote).eq.2) then
          twopot(2,npote) = floats(2+nbeg)*units
        else
          twopot(2,npote) = floats(2+nbeg)
        endif
        rpot(npote)  = floats(3+nbeg)
        rpot2(npote) = 0.0_dp
      elseif (nfloat.eq.2) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(2,npote) = 0.0_dp
        rpot(npote)     = floats(2+nbeg)
        rpot2(npote)    = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  endif
  if (.not.lautolj) then
    if (nvar1.eq.nvar2) then
      nspec1(npote) = nvar1
      nspec2(npote) = nvar2
      if (itype1.lt.itype2) then
        nptyp1(npote) = itype1
        nptyp2(npote) = itype2
        symbol2(1,npote) = sym21
        symbol2(2,npote) = sym22
      else
        nptyp1(npote) = itype2
        nptyp2(npote) = itype1
        symbol2(1,npote) = sym22
        symbol2(2,npote) = sym21
      endif
    elseif (nvar1.lt.nvar2) then
      nspec1(npote) = nvar1
      nspec2(npote) = nvar2
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nspec1(npote) = nvar2
      nspec2(npote) = nvar1
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  endif
!
!  Evaluate constants needed for epsilon-sigma form
!
  if (nptype(npote).eq.12.or.nptype(npote).eq.13) then
    rm = tpot(1,npote)
    rn = tpot(2,npote)
    rdiff = 1.0_dp/(rm-rn)
    if (nptype(npote).eq.12) then
      ratio = rm/rn
      const = ratio**rdiff
      twopot(3,npote) = rdiff*rn*const**rm
      twopot(4,npote) = rdiff*rm*const**rn
    else
      twopot(3,npote) = rdiff*rn
      twopot(4,npote) = rdiff*rm
    endif
  endif
  if (lautolj) then
    read(iin,'(a)',end=198) line
    iline = iline + 1
    call linepro(iin,line,iline)
    l55 = .true.
  else
    goto 195
  endif
198 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np).and..not.lautolj) call potcut(np)
  enddo
  lwordok = .true.
!
!  Do automatic generation of further L-J potentials if necessary
!
  if (lautolj) then
    npot1 = npote
    npote = npote - 1
    if (nptype(npot1).eq.2) then
!
!  Atom A/B form
!
      if (npote+natab*(natab+1)/2.gt.maxpot) then
        maxpot = npote + natab*(natab+1)/2
        call changemaxpot
      endif
      do i = 1,natab
        do j = 1,i
          npote = npote + 1
!
!  Set species
!
          if (nattab(i).eq.nattab(j)) then
            nspec1(npote) = nattab(i)
            nspec2(npote) = nattab(j)
            if (ntypab(i).lt.ntypab(j)) then
              nptyp1(npote) = ntypab(i)
              nptyp2(npote) = ntypab(j)
            else
              nptyp1(npote) = ntypab(j)
              nptyp2(npote) = ntypab(i)
            endif
          elseif (nattab(i).lt.nattab(j)) then
            nspec1(npote) = nattab(i)
            nspec2(npote) = nattab(j)
            nptyp1(npote) = ntypab(i)
            nptyp2(npote) = ntypab(j)
          else
            nspec1(npote) = nattab(j)
            nspec2(npote) = nattab(i)
            nptyp1(npote) = ntypab(j)
            nptyp2(npote) = ntypab(i)
          endif
!
!  Copy remaining details
!
          mmexc(npote) = mmexc(npot1)
          lintra(npote) = lintra(npot1)
          linter(npote) = linter(npot1)
          tpot(1,npote) = tpot(1,npot1)
          tpot(2,npote) = tpot(2,npot1)
          tpot(3,npote) = tpot(3,npot1)
          tpot(4,npote) = tpot(4,npot1)
          leshift(npote) = leshift(npot1)
          lgshift(npote) = lgshift(npot1)
          nptype(npote) = nptype(npot1)
          lcombine(npote) = lcombine(npot1)
          ncombipower(npote) = ncombipower(npot1)
          twopot(1,npote) = twopot(1,npot1)
          twopot(2,npote) = twopot(2,npot1)
          rpot2(npote)    = rpot2(npot1)
          rpot(npote)     = rpot(npot1)
          twopot(3,npote) = twopot(3,npot1)
          twopot(4,npote) = twopot(4,npot1)
        enddo
      enddo
    else
!
!  Epsilon/sigma form
!
      if (npote+nseps*(nseps+1)/2.gt.maxpot) then
        maxpot = npote + nseps*(nseps+1)/2
        call changemaxpot
      endif
      do i = 1,nseps
        do j = 1,i
          npote = npote + 1
!
!  Set species
!
          if (natse(i).eq.natse(j)) then
            nspec1(npote) = natse(i)
            nspec2(npote) = natse(j)
            if (ntypse(i).lt.ntypse(j)) then
              nptyp1(npote) = ntypse(i)
              nptyp2(npote) = ntypse(j)
            else
              nptyp1(npote) = ntypse(j)
              nptyp2(npote) = ntypse(i)
            endif
          elseif (natse(i).lt.natse(j)) then
            nspec1(npote) = natse(i)
            nspec2(npote) = natse(j)
            nptyp1(npote) = ntypse(i)
            nptyp2(npote) = ntypse(j)
          else
            nspec1(npote) = natse(j)
            nspec2(npote) = natse(i)
            nptyp1(npote) = ntypse(j)
            nptyp2(npote) = ntypse(i)
          endif
!
!  Copy remaining details
!
          mmexc(npote) = mmexc(npot1)
          lintra(npote) = lintra(npot1)
          linter(npote) = linter(npot1)
          tpot(1,npote) = tpot(1,npot1)
          tpot(2,npote) = tpot(2,npot1)
          tpot(3,npote) = tpot(3,npot1)
          tpot(4,npote) = tpot(4,npot1)
          leshift(npote) = leshift(npot1)
          lgshift(npote) = lgshift(npot1)
          nptype(npote) = nptype(npot1)
          lcombine(npote) = lcombine(npot1)
          ncombipower(npote) = ncombipower(npot1)
          twopot(1,npote) = twopot(1,npot1)
          twopot(2,npote) = twopot(2,npot1)
          rpot2(npote)    = rpot2(npot1)
          rpot(npote)     = rpot(npot1)
          twopot(3,npote) = twopot(3,npot1)
          twopot(4,npote) = twopot(4,npot1)
        enddo
      enddo
    endif
    do np = npot1,npote
      if (leshift(np).and..not.lautolj) call potcut(np)
    enddo
  endif
  return
!***********************************
!  Parameters for Morse potential  *
!***********************************
200 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  tpot(5,npote+1) = 0.0_dp
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  letaper = .false.
  zero = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'ener').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .false.
      elseif (index(words(i),'grad').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      elseif (index(words(i),'eta').eq.1) then
        letaper = .true.
      elseif (index(words(i),'zero').eq.1) then
        zero = 0.0_dp
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  Ensure that energy/gradient shifts aren't applied with etaper
!
  if (letaper) then
    leshift(npote+1) = .false.
    lgshift(npote+1) = .false.
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
205 line = '  '
  read(iin,'(a)',end=208) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 205
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 208
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  tpot(5,npote) = tpot(5,npot1)
  scale14(npote) = scale14(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-2))
    n2 = int(floats(nfloat-1))
    n3 = int(floats(nfloat))
    nfloat = nfloat - 3
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 3
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 205
  endif
!
!  Assign scale coefficient for De
!
  twopot(5,npote) = zero
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.4) nfloat = nfloat - 3
    if (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.6) nfloat = nfloat - 3
    if (nfloat.eq.6) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = floats(4+nbeg)
      rpot2(npote)    = floats(5+nbeg)
      rpot(npote)     = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = floats(4+nbeg)
      rpot(npote)     = floats(5+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = 0.0_dp
      rpot(npote)     = floats(4+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  if (letaper) then
    if (abs(twopot(4,npote)).gt.0.0) then
      nptype(npote) = 40
    else
      nptype(npote) = 39
    endif
  else
    if (abs(twopot(4,npote)).gt.0.0) then
      nptype(npote) = 4
    else
      nptype(npote) = 3
    endif
  endif
  goto 205
208 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!********************************
!  Sigma/epsilon specification  *
!********************************
210 units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      endif
      i = i + 1
    enddo
  endif
215 line = '  '
  read(iin,'(a)',end=218) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 215
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 218
    endif
  endif
  nseps = nseps + 1
  if (nseps.gt.maxspec) then
    maxspec = nseps + 20
    call changemaxspec
  endif
  if (nword.gt.0) then
    if (llibrary) then
      call okspec(lok1,words(1),ilp,.true.,ltype01)
      if (.not.lok1) then
        nseps = nseps - 1
        goto 215
      endif
      if (ilp.gt.0.and.ilp.le.nspec) then
        nvar1 = natspec(ilp)
        if (ltype01) then
          itype1 = 0
        else
          itype1 = ntypspec(ilp)
        endif
      elseif (ilp.eq.-1) then
        nvar1 = maxele
        itype1 = 0
      endif
    else
      call ltont(words(1),nvar1,itype1)
    endif
    natse(nseps) = nvar1
    ntypse(nseps) = itype1
    ind = 0
    if (nword.gt.1) then
      if (index(words(2),'S').eq.1.or.index(words(2),'s').eq.1) natse(nseps)=natse(nseps)+maxele
    endif
  elseif (nfloat.gt.0) then
    natse(nseps) = nint(floats(1))
    ntypse(nseps) = 0
    ind = 1
  else
    call outerror('Incorrect potential input for epsilon/sigma',iline)
    call stopnow('potword21')
  endif
  if (lfit.and.lfflags) then
    if (nint(floats(nfloat-1)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 7
      nfvar(nfit) = nseps
    endif
    if (nint(floats(nfloat)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 8
      nfvar(nfit) = nseps
    endif
    nfloat = nfloat - 2
  endif
  if (nfloat.ge.ind+2) then
    epsilon(nseps) = floats(ind+1)*units
    sigma(nseps) = floats(ind+2)
  else
    call outerror('Incorrect potential input for epsilon/sigma',iline)
    call stopnow('potword21')
  endif
  goto 215
218 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*********************************************
!  Atom A B specification for lennard-jones  *
!*********************************************
220 units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      endif
      i = i + 1
    enddo
  endif
225 line = '  '
  read(iin,'(a)',end=228) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 225
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 228
    endif
  endif
  natab = natab + 1
  if (natab.gt.maxspec) then
    maxspec = natab + 20
    call changemaxspec
  endif
  if (nword.gt.0) then
    ltype01 = .false.
    if (llibrary) then
      call okspec(lok1,words(1),ilp,.true.,ltype01)
      if (.not.lok1) then
        natab = natab - 1
        goto 225
      endif
    endif
    call ltont(words(1),nvar1,itype1)
    nattab(natab) = nvar1
    if (ltype01) then
      ntypab(natab) = 0
    else
      ntypab(natab) = itype1
    endif
    ind = 0
    if (nword.gt.1) then
      if (index(words(2),'S').eq.1.or.index(words(2),'s').eq.1) nattab(natab)=nattab(natab)+maxele
    endif
  elseif (nfloat.gt.0) then
    nattab(natab) = nint(floats(1))
    ntypab(natab) = 0
    ind = 1
  else
    call outerror('Incorrect potential input for atomab',iline)
    call stopnow('potword21')
  endif
  if (lfit.and.lfflags) then
    if (nint(floats(nfloat-1)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 9
      nfvar(nfit) = natab
    endif
    if (nint(floats(nfloat)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 10
      nfvar(nfit) = natab
    endif
    nfloat = nfloat - 2
  endif
  if (nfloat.ge.ind+2) then
    atoma(natab) = floats(ind+1)*units
    atomb(natab) = floats(ind+2)*units
  else
    call outerror('Incorrect potential input for atomab',iline)
    call stopnow('potword21')
  endif
  goto 225
228 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***********************************************
!  Parameters for damped dispersion potential  *
!***********************************************
230 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
235 line = '  '
  read(iin,'(a)',end=238) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 235
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 238
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-5))
    n2 = int(floats(nfloat-4))
    n3 = int(floats(nfloat-3))
    n4 = int(floats(nfloat-2))
    n5 = int(floats(nfloat-1))
    n6 = int(floats(nfloat))
    nfloat = nfloat - 6
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 10
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 3
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 4
    endif
    if (n6.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 11
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 235
  endif
  if (mmexc(npote).eq.1) then
!
!  If number of floats is greater than 6, assume that fitting flags have been left on line
!
    if (nfloat.gt.6) nfloat = nfloat - 6
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.6) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote) = floats(3+nbeg)*units
      twopot(3,npote) = floats(4+nbeg)
      twopot(4,npote) = floats(5+nbeg)
      tpot(2,npote) = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote) = floats(3+nbeg)*units
      twopot(3,npote) = floats(4+nbeg)
      twopot(4,npote) = floats(5+nbeg)
      tpot(2,npote) = 0.0_dp
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote) = 0.0_dp
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = floats(4+nbeg)
      tpot(2,npote) = 0.0_dp
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote) = 0.0_dp
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = 0.0_dp
      tpot(2,npote) = 0.0_dp
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote) = 0.0_dp
      twopot(3,npote) = 0.0_dp
      twopot(4,npote) = 0.0_dp
      tpot(2,npote) = 0.0_dp
    elseif (nfloat.eq.1) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = 0.0_dp
      tpot(1,npote) = 0.0_dp
      twopot(3,npote) = 0.0_dp
      twopot(4,npote) = 0.0_dp
      tpot(2,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
!
!  If number of floats is greater than 8, assume that fitting flags have been left on line
!
    if (nfloat.gt.8) nfloat = nfloat - 6
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.8) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote)   = floats(3+nbeg)*units
      twopot(3,npote) = floats(4+nbeg)
      twopot(4,npote) = floats(5+nbeg)
      tpot(2,npote)   = floats(6+nbeg)
      rpot2(npote)    = floats(7+nbeg)
      rpot(npote)     = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote)   = floats(3+nbeg)*units
      twopot(3,npote) = floats(4+nbeg)
      twopot(4,npote) = floats(5+nbeg)
      tpot(2,npote)   = 0.0_dp
      rpot2(npote)    = floats(6+nbeg)
      rpot(npote)     = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote)   = 0.0_dp
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = floats(4+nbeg)
      tpot(2,npote)   = 0.0_dp
      rpot2(npote)    = floats(5+nbeg)
      rpot(npote)     = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote)   = 0.0_dp
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = floats(4+nbeg)
      tpot(2,npote)   = 0.0_dp
      rpot(npote)     = floats(5+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote)   = 0.0_dp
      twopot(3,npote) = floats(3+nbeg)
      twopot(4,npote) = 0.0_dp
      tpot(2,npote)   = 0.0_dp
      rpot(npote)     = floats(4+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      tpot(1,npote)   = 0.0_dp
      twopot(3,npote) = 0.0_dp
      twopot(4,npote) = 0.0_dp
      tpot(2,npote)   = 0.0_dp
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = 0.0_dp
      tpot(1,npote)   = 0.0_dp
      twopot(3,npote) = 0.0_dp
      twopot(4,npote) = 0.0_dp
      tpot(2,npote)   = 0.0_dp
      rpot(npote)     = floats(2+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 18
  goto 235
238 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!******************************
!  Many potential for metals  *
!******************************
240 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  nptype(npote+1) = 19
  mmexc(npote+1) = 0
  lintra(npote+1) = .true.
  linter(npote+1) = .true.
  lcombine(npote+1) = .false.
  npot1 = npote + 1
!
!  Sutton-Chen logical set to be true so that we
!  know that the many-body energy must be called
!
  lsuttonc = .true.
!
!  Disable third derivatives for EAM/MEAM case
!
  lnoanald3 = .true.
!
245 line = '  '
  read(iin,'(a)',end=248) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 245
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 248
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  nptype(npote) = nptype(npot1)
  lcombine(npote) = lcombine(npot1)
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    goto 245
  endif
!
!  Assign cutoffs
!
  if (nfloat.eq.2) then
    rpot2(npote) = floats(1+nbeg)
    rpot(npote)  = floats(2+nbeg)
  elseif (nfloat.eq.1) then
    rpot(npote)  = floats(1+nbeg)
    rpot2(npote) = 0.0_dp
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword21')
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  goto 245
248 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************************************************
!  Maximum search radius control for S-C potentials  *
!*****************************************************
250 if (nfloat.eq.0) then
    line = '  '
    read(iin,'(a)') line
    iline = iline + 1
    call linepro(iin,line,iline)
  endif
  scmaxsearch = abs(floats(1))
!
!  There is no point exceeding a value of 3.0
!
  scmaxsearch = min(scmaxsearch,3.0_dp)
  lwordok = .true.
  return
!******************************************************************************
!  Parameters for squared Harmonic potential with Coulomb subtracted option   *
!  which allows the potential to act as a core-shell spring constant          *
!******************************************************************************
260 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'ener').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .false.
      elseif (index(words(i),'grad').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
265 line  =  '  '
  read(iin,'(a)',end = 268) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 265
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 268
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 2
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 265
  endif
!
!  Check for flags on the end of the line
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.3) nfloat = nfloat - 2
    if (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      tpot(1,npote) = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      tpot(1,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.5) nfloat = nfloat - 2
    if (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      tpot(1,npote)   = floats(3+nbeg)
      rpot2(npote)    = floats(4+nbeg)
      rpot(npote)     = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      tpot(1,npote)   = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      tpot(1,npote)   = 0.0_dp
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  if (abs(tpot(1,npote)).gt.0.0) then
    nptype(npote) = 29
  else
    nptype(npote) = 28
  endif
!
!  Check that if harmonic is core-shell spring constant then 
!  coulomb subtraction is turned off.
!
  if (abs(nvar1-nvar2).eq.maxele.and.rpot2(npote).eq.0.0) then
    if (tpot(1,npote).eq.1) then
      nwarn = nwarn + 1
      if (ioproc) then
        write(ioout,'(/,''**** Warning - core-shell spring constant set for coulomb subtraction ****'')')
        write(ioout,'(''**** This is already accounted for so flag will be reset accordingly  ****'')')
      endif
      tpot(1,npote) = 0.0_dp
      nptype(npote) = 28
    endif
  endif
  goto 265
268 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!***************************
!  Bond charge increments  *
!***************************
270 if (nbondQ.eq.maxbondQ) then
    maxbondQ = nbondQ + 10
    call changemaxbondQ
  endif
275 line = '  '
  read(iin,'(a)',end = 278) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 275
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 278
    endif
  endif
  nbondQ = nbondQ + 1
  if (nbondQ.gt.maxbondQ) then
    maxbondQ = nbondQ + 10
    call changemaxbondQ
  endif
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 27
      nfvar(nfit) = nbondQ
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nbondQ = nbondQ - 1
    nfit = nfit0
    goto 275
  endif
!
!  Check for flags on the end of the line
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.1) then
    bondQincrement(nbondQ) = floats(1+nbeg)
  else
    call outerror('Bond increment missing',iline)
    call stopnow('potword21')
  endif
  if (nvar1.eq.nvar2) then
    nbondQspec1(nbondQ) = nvar1
    nbondQspec2(nbondQ) = nvar2
    if (itype1.lt.itype2) then
      nbondQtyp1(nbondQ) = itype1
      nbondQtyp2(nbondQ) = itype2
      symbolbondQ(1,npote) = sym21
      symbolbondQ(2,npote) = sym22
    else
      nbondQtyp1(nbondQ) = itype2
      nbondQtyp2(nbondQ) = itype1
      symbolbondQ(1,npote) = sym22
      symbolbondQ(2,npote) = sym21
      bondQincrement(nbondQ) = - bondQincrement(nbondQ)
    endif
  elseif (nvar1.lt.nvar2) then
    nbondQspec1(nbondQ) = nvar1
    nbondQspec2(nbondQ) = nvar2
    nbondQtyp1(nbondQ) = itype1
    nbondQtyp2(nbondQ) = itype2
    symbolbondQ(1,npote) = sym21
    symbolbondQ(2,npote) = sym22
  else
    nbondQspec1(nbondQ) = nvar2
    nbondQspec2(nbondQ) = nvar1
    nbondQtyp1(nbondQ) = itype2
    nbondQtyp2(nbondQ) = itype1
    symbolbondQ(1,npote) = sym22
    symbolbondQ(2,npote) = sym21
    bondQincrement(nbondQ) = - bondQincrement(nbondQ)
  endif
  goto 275
278 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***************************************
!  Shift multiplier for configuration  *
!***************************************
290 if (nfloat.gt.0) then
    shscalecfg(ncurr) = floats(1)
  else
    line = '  '
    read(iin,'(a)') line
    iline = iline + 1
    call linepro(iin,line,iline)
    if (nfloat.gt.0) then
      shift(ncurr) = floats(1)
    else
      call outerror('Scale for shift missing',iline)
      call stopnow('potword21')
    endif
  endif
  lwordok = .true.
  return
!***********************
!  Energy shift value  *
!***********************
300 nshift = nshift+1
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoev
    elseif (index(words(2),'kcal').eq.1) then
      units = kcaltoev
    elseif (index(words(2),'kjmo').eq.1) then
      units = kjmtoev
    endif
  endif
  if (nfloat.gt.0) then
    shift(nshift) = floats(1)*units
  else
    line  =  '  '
    read(iin,'(a)') line
    iline = iline + 1
    call linepro(iin,line,iline)
    shift(nshift) = floats(1)
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'au').eq.1) then
        units = autoev
      elseif (index(words(1),'kcal').eq.1) then
        units = kcaltoev
      elseif (index(words(1),'kjmo').eq.1) then
        units = kjmtoev
      endif
    endif
    shift(nshift) = shift(nshift)*units
  endif
!
!  Need to correct configuration shift pointer
!
  nshcfg(ncurr) = nshift
  lwordok = .true.
  return
!**************************************************
!  Parameters for Covalent Exponential potential  *
!**************************************************
310 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'ener').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .false.
      elseif (index(words(i),'grad').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn+1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
315 line  =  '  '
  read(iin,'(a)',end = 318) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 315
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 318
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot  =  npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-2))
    n2 = int(floats(nfloat-1))
    n3 = int(floats(nfloat))
    nfloat = nfloat - 3
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit  =  nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit  =  nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit  =  nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 3
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 315
  endif
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.3) nfloat = nfloat - 3
    if (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.5) nfloat = nfloat - 3
    if (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      rpot2(npote)    = floats(4+nbeg)
      rpot(npote)     = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 25
  goto 315
318 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!**********************************************
!  Cosh form of core - shell spring constant  *
!**********************************************
320 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  lintra(npote+1) = .true.
  linter(npote+1) = .true.
  npot1 = npote + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      endif
      i = i + 1
    enddo
  endif
325 line = '  '
  read(iin,'(a)',end=328) line
  iline = iline + 1
  call linepro(iin,line,iline)
!  
!  Check whether input is symbol or option
!     
  if ((nword+nfloat).eq.0) goto 325
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 328
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!  
!  Copy first line info from first potential
!     
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 2
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  else
!
!  Check to make sure that no fitting flags have been left on the line
!
    if (floats(nfloat).eq.1.0_dp.or.floats(nfloat).eq.0.0_dp) then
      nfloat = nfloat - 1
    endif
    if (floats(nfloat).eq.1.0_dp.or.floats(nfloat).eq.0.0_dp) then
      nfloat = nfloat - 1
    endif
  endif 
!           
!  Process symbol input
!             
  ltype01 = .false.
  if (nword.gt.0) then
    if (llibrary) then
      call okspec(lok1,words(1),ilp,.true.,ltype01)
      if (.not.lok1) then
        npote = npote - 1
        nfit = nfit0
        goto 325
      endif
    endif
    if (nword.eq.2) then
      if (index(words(2),'cor').eq.1) nword = 1
      if (index(words(2),'she').eq.1) nword = 1
    endif
    if (nword.eq.1) then
      call ltont(words(1),nvar1,itype1)
      if (ltype01) itype1 = 0
      nvar2 = nvar1 + maxele
      itype2 = itype1
    elseif (nword.eq.2) then
      call ltont(words(1),nvar1,itype1)
      call ltont(words(2),nvar2,itype2)
      if (ltype01) itype1 = 0
    elseif (nword.eq.3) then
      call ltont(words(1),nvar1,itype1)
      if (ltype01) itype1 = 0
      word = words(2)(1:20)
      call stolc(word,20_i4)
      if (index(word,'cor').eq.1) then
        call ltont(words(3),nvar2,itype2)
      elseif (index(word,'she').eq.1) then
        nvar1 = nvar1 + maxele
        call ltont(words(3),nvar2,itype2)
      else
        call ltont(words(2),nvar2,itype2)
        word = words(3)(1:20)
        call stolc(word,20_i4)
        if (index(word,'she').eq.1) then
          nvar2 = nvar2 + maxele
        endif
      endif
    elseif (nword.eq.4) then
      call ltont(words(1),nvar1,itype1)
      call ltont(words(3),nvar2,itype2)
      if ((index(words(2),'s').eq.1).or.(index(words(2),'S').eq.1)) nvar1 = nvar1 + maxele
      if ((index(words(4),'s').eq.1).or.(index(words(4),'S').eq.1)) nvar2 = nvar2 + maxele
      if (ltype01) itype1 = 0
    else
      call outerror('Incorrect potential species input',iline)
      call stopnow('potword21')
    endif
    nbeg = 0  
!         
!  Numeric input
!         
  elseif (nfloat.ge.4) then
    nvar1 = int(floats(1))
    nvar2 = int(floats(2))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
    itype1 = 0
    itype2 = 0
    nbeg = 2
    nfloat = nfloat - 2
  else
    nvar1 = int(floats(1))
    if (nvar1.le.100) then
      nvar2 = nvar1 + maxele
    else
      nvar1 = nvar1 - 100
      nvar2 = nvar1 + maxele
    endif
    itype1 = 0
    itype2 = 0
    nbeg = 1
    nfloat = nfloat - 1
  endif
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.2) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword21')
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
  endif
  nptype(npote) = 33
  goto 325
328 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************************
!  Parameters for erferfc potential  *
!*************************************
330 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
335 line = '  '
  read(iin,'(a)',end=338) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 335
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 338
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  tpot(5,npote) = tpot(5,npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-2))
    n2 = int(floats(nfloat-1))
    n3 = int(floats(nfloat))
    nfloat = nfloat - 3
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 3
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 335
  endif
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.3) nfloat = nfloat - 3
    if (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.5) nfloat = nfloat - 3
    if (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      rpot2(npote)    = floats(4+nbeg)
      rpot(npote)     = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
!
! Check whether parameters are too small
!
  if (abs(twopot(2,npote)).lt.1.0d-12) then
    call outerror('Alpha is too close to zero in erferfc potential',iline)
    call stopnow('potword21')
  endif
  if (abs(twopot(3,npote)).lt.1.0d-12) then
    call outerror('Beta is too close to zero in erferfc potential',iline)
    call stopnow('potword21')
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 42
  goto 335
338 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************************
!  Parameters for reperfc potential  *
!*************************************
340 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  tpot(5,npote+1) = 0.0_dp
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
345 line = '  '
  read(iin,'(a)',end=348) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 345
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 348
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  tpot(5,npote) = tpot(5,npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 2
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 345
  endif
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.2) nfloat = nfloat - 2
    if (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.4) nfloat = nfloat - 2
    if (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      rpot2(npote)    = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
!
! Check whether parameter is too small
!
  if (abs(twopot(2,npote)).lt.1.0d-12) then
    call outerror('Beta is too close to zero in reperfc potential',iline)
    call stopnow('potword21')
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 43
  goto 345
348 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************************
!  Force constant potential  *
!*****************************
350 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
355 line = '  '
  read(iin,'(a)',end=358) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 355
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 358
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 2
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 355
  endif
!
!  Check for flags on the end of the line
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.2) nfloat = nfloat - 2
    if (nfloat.ge.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
    elseif (nfloat.eq.1) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.4) nfloat = nfloat - 2
    if (nfloat.ge.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      rpot2(npote)    = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = 0.0_dp
      rpot(npote)     = floats(2+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 37
  goto 355
358 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!**********************************************************************
!  Parameters for Harmonic potential with Coulomb subtracted option   *
!  which allows the potential to act as a core-shell spring constant  *
!**********************************************************************
360 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lk3 = .false.
  lk4 = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'k3').eq.1) then
        lk3 = .true.
      elseif (index(words(i),'k4').eq.1) then
        lk4 = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'ener').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .false.
      elseif (index(words(i),'grad').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
365 line = '  '
  read(iin,'(a)',end=368) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 365
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 368
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    if (lk3.and.lk4) then
      n1 = int(floats(nfloat-3))
      n2 = int(floats(nfloat-2))
      n3 = int(floats(nfloat-1))
      n4 = int(floats(nfloat))
      nfloat = nfloat - 4
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit  =  nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 1
      endif
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 3
      endif
      if (n3.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 4
      endif
      if (n4.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 2
      endif
    elseif (lk3) then
      n1 = int(floats(nfloat-2))
      n2 = int(floats(nfloat-1))
      n3 = int(floats(nfloat))
      nfloat = nfloat - 3
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 1
      endif
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 3
      endif
      if (n3.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 2
      endif
    elseif (lk4) then
      n1 = int(floats(nfloat-2))
      n2 = int(floats(nfloat-1))
      n3 = int(floats(nfloat))
      nfloat = nfloat - 3
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 1
      endif
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 4
      endif
      if (n3.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 2
      endif
    else
      n1 = int(floats(nfloat-1))
      n2 = int(floats(nfloat))
      nfloat = nfloat - 2
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 1
      endif
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 2
        nfpot(nfit) = npote
        nfvar(nfit) = 2
      endif
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 365
  endif
!
!  Check for flags on the end of the line
!  Assign coefficients and cutoffs
!
  if (lk3.and.lk4) then
    if (mmexc(npote).eq.1) then
      if (nfloat.gt.5) nfloat = nfloat - 4
      if (nfloat.eq.5) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(4,npote) = floats(3+nbeg)*units
        twopot(2,npote) = floats(4+nbeg)
        tpot(1,npote) = floats(5+nbeg)
      elseif (nfloat.eq.4) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(4,npote) = floats(3+nbeg)*units
        twopot(2,npote) = floats(4+nbeg)
        tpot(1,npote) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    else
      if (nfloat.gt.7) nfloat = nfloat - 4
      if (nfloat.eq.7) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(4,npote) = floats(3+nbeg)*units
        twopot(2,npote) = floats(4+nbeg)
        tpot(1,npote)   = floats(5+nbeg)
        rpot2(npote)    = floats(6+nbeg)
        rpot(npote)     = floats(7+nbeg)
      elseif (nfloat.eq.6) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(4,npote) = floats(3+nbeg)*units
        twopot(2,npote) = floats(4+nbeg)
        tpot(1,npote)   = floats(5+nbeg)
        rpot(npote)     = floats(6+nbeg)
        rpot2(npote)    = 0.0_dp
      elseif (nfloat.eq.5) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(4,npote) = floats(3+nbeg)*units
        twopot(2,npote) = floats(4+nbeg)
        tpot(1,npote)   = 0.0_dp
        rpot(npote)     = floats(5+nbeg)
        rpot2(npote)    = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  elseif (lk3) then
    if (mmexc(npote).eq.1) then
      if (nfloat.gt.4) nfloat = nfloat - 3
      if (nfloat.eq.4) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = floats(4+nbeg)
      elseif (nfloat.eq.3) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    else
      if (nfloat.gt.6) nfloat = nfloat - 3
      if (nfloat.eq.6) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = floats(4+nbeg)
        rpot2(npote)    = floats(5+nbeg)
        rpot(npote)     = floats(6+nbeg)
      elseif (nfloat.eq.5) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = floats(4+nbeg)
        rpot(npote)     = floats(5+nbeg)
        rpot2(npote)    = 0.0_dp
      elseif (nfloat.eq.4) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(3,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = 0.0_dp
        rpot(npote)     = floats(4+nbeg)
        rpot2(npote)    = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  elseif (lk4) then
    if (mmexc(npote).eq.1) then
      if (nfloat.gt.4) nfloat = nfloat - 3
      if (nfloat.eq.4) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(4,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = floats(4+nbeg)
      elseif (nfloat.eq.3) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(4,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    else
      if (nfloat.gt.6) nfloat = nfloat - 3
      if (nfloat.eq.6) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(4,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = floats(4+nbeg)
        rpot2(npote)    = floats(5+nbeg)
        rpot(npote)     = floats(6+nbeg)
      elseif (nfloat.eq.5) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(4,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = floats(4+nbeg)
        rpot(npote)     = floats(5+nbeg)
        rpot2(npote)    = 0.0_dp
      elseif (nfloat.eq.4) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(4,npote) = floats(2+nbeg)*units
        twopot(2,npote) = floats(3+nbeg)
        tpot(1,npote)   = 0.0_dp
        rpot(npote)     = floats(4+nbeg)
        rpot2(npote)    = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  else
    if (mmexc(npote).eq.1) then
      if (nfloat.gt.3) nfloat = nfloat - 2
      if (nfloat.eq.3) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(2,npote) = floats(2+nbeg)
        tpot(1,npote)   = floats(3+nbeg)
      elseif (nfloat.eq.2) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(2,npote) = floats(2+nbeg)
        tpot(1,npote)   = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    else
      if (nfloat.gt.5) nfloat = nfloat - 2
      if (nfloat.eq.5) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(2,npote) = floats(2+nbeg)
        tpot(1,npote)   = floats(3+nbeg)
        rpot2(npote)    = floats(4+nbeg)
        rpot(npote)     = floats(5+nbeg)
      elseif (nfloat.eq.4) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(2,npote) = floats(2+nbeg)
        tpot(1,npote)   = floats(3+nbeg)
        rpot(npote)     = floats(4+nbeg)
        rpot2(npote)    = 0.0_dp
      elseif (nfloat.eq.3) then
        twopot(1,npote) = floats(1+nbeg)*units
        twopot(2,npote) = floats(2+nbeg)
        tpot(1,npote)   = 0.0_dp
        rpot(npote)     = floats(3+nbeg)
        rpot2(npote)    = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  if (abs(tpot(1,npote)).gt.0.0) then
    nptype(npote) = 6
  else
    nptype(npote) = 5
  endif
!
!  Check that if harmonic is core-shell spring constant then 
!  coulomb subtraction is turned off.
!
  if (abs(nvar1-nvar2).eq.maxele.and.rpot2(npote).eq.0.0) then
    if (tpot(1,npote).eq.1) then
      nwarn = nwarn + 1
      if (ioproc) then
        write(ioout,'(/,''**** Warning - core-shell spring constant set for coulomb subtraction ****'')')
        write(ioout,'(''**** This is already accounted for so flag will be reset accordingly  ****'')')
      endif
      tpot(1,npote) = 0.0_dp
      nptype(npote) = 5
    endif
  endif
  goto 365
368 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!*******************************************
!  EAM potential shift two-body potential  *
!*******************************************
370 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'ener').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .false.
      elseif (index(words(i),'grad').eq.1) then
        leshift(npote+1) = .true.
        lgshift(npote+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
375 line = '  '
  read(iin,'(a)',end=378) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 375
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 378
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 2
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 375
  endif
!
!  Check for flags on the end of the line
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.2) nfloat = nfloat - 2
    if (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.4) nfloat = nfloat - 2
    if (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      rpot2(npote)    = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 34
  goto 375
378 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!********************************
!  UFF parameter specification  *
!********************************
380 units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      endif
      i = i + 1
    enddo
  endif
385 line = '  '
  read(iin,'(a)',end=388) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 385
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 388
    endif
  endif
  nUFFspec = nUFFspec + 1
  if (nUFFspec.gt.maxUFFspec) then
    maxUFFspec = nUFFspec + 20
    call changemaxuffspec
  endif
  if (nword.gt.0) then
    if (llibrary) then
      call okspec(lok1,words(1),ilp,.true.,ltype01)
      if (.not.lok1) then
        nUFFspec = nUFFspec - 1
        goto 385
      endif
      if (ilp.gt.0.and.ilp.le.nspec) then
        nvar1 = natspec(ilp)
        if (ltype01) then
          itype1 = 0
        else
          itype1 = ntypspec(ilp)
        endif
      elseif (ilp.eq.-1) then
        nvar1 = maxele
        itype1 = 0
      endif
    else
      call ltont(words(1),nvar1,itype1)
    endif
    natUFFspec(nUFFspec) = nvar1
    ntypUFFspec(nUFFspec) = itype1
    ind = 0
    if (nword.gt.1) then
      if (index(words(2),'S').eq.1.or.index(words(2),'s').eq.1) natUFFspec(nUFFspec) = natUFFspec(nUFFspec) + maxele
    endif
  elseif (nfloat.gt.0) then
    natUFFspec(nUFFspec) = nint(floats(1))
    ntypUFFspec(nUFFspec) = 0
    ind = 1
  else
    call outerror('Incorrect potential input for UFF1',iline)
    call stopnow('potword21')
  endif
!
!  Save symbol
!
  if (nword.gt.0) then
    symbolUFFspec(nUFFspec) = words(1)(1:5)
  else
    symbolUFFspec(nUFFspec) = ' '
  endif
  if (lfit.and.lfflags) then
    if (nint(floats(nfloat-9)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 20
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-8)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 21
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-7)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 22
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-6)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 23
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-5)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 24
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-4)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 25
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-3)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 26
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-2)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 30
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat-1)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 31
      nfvar(nfit) = nUFFspec
    endif
    if (nint(floats(nfloat)).eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 32
      nfvar(nfit) = nUFFspec
    endif
    nfloat = nfloat - 10
  endif
  if (nfloat.ge.ind+11) then
    UFFr(nUFFspec) = floats(ind+1)
    UFFtheta(nUFFspec) = floats(ind+2)
    UFFx(nUFFspec) = floats(ind+3)
    UFFd(nUFFspec) = floats(ind+4)*units
    UFFzeta(nUFFspec) = floats(ind+5)
    UFFZeff(nUFFspec) = floats(ind+6)
    nUFFtype(nUFFspec) = nint(floats(ind+7))
    UFFtor(nUFFspec) = floats(ind+8)*units
    UFFKoop(nUFFspec) = floats(ind+9)*units
    UFFthetaoop(nUFFspec) = floats(ind+10)
    UFFchi(nUFFspec) = floats(ind+11)
  else
    call outerror('Incorrect potential input for UFF1',iline)
    call stopnow('potword21')
  endif
  goto 385
388 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*********************************
!  Parameters for erf potential  *
!*********************************
390 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
395 line = '  '
  read(iin,'(a)',end=398) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 395
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 398
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  tpot(5,npote) = tpot(5,npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 2
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 395
  endif
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.2) nfloat = nfloat - 2
    if (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
    if (nfloat.gt.4) nfloat = nfloat - 2
    if (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      rpot2(npote)    = floats(3+nbeg)
      rpot(npote)      = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
!
! Check whether parameter is too small
!
  if (abs(twopot(2,npote)).lt.1.0d-12) then
    call outerror('Alpha is too close to zero in erf potential',iline)
    call stopnow('potword21')
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 44
  goto 395
398 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!********************************************************
!  Parameters for polynomial potential up to 8th order  *
!********************************************************
400 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lpharm = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'harm').eq.1) then
        lpharm = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
!
!  Set polynomial order
!
  line = '  '
  read(iin,'(a)',end=408) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.eq.1) then
    norder = nint(floats(1))
  elseif (nfloat.eq.0) then
    call outerror('Polynomial order missing from input',iline)
    call stopnow('potword21')
  else
    call outerror('Too many inputs for polynomial order',iline)
    call stopnow('potword21')
  endif
  if (norder.gt.8) then
    call outerror('Only polynomials up to 8th order allowed',iline)
    call stopnow('potword21')
  elseif (norder.lt.1) then
    call outerror('Order of polynomial is too low - see shift for zeroth order',iline)
    call stopnow('potword21')
  endif
!
!  Increment norder to account for zeroth order term
!
  norder = norder + 1
  npot1 = npote + 1
405 line = '  '
  read(iin,'(a)',end=408) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 405
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 408
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    if (lpharm) then
      do i = 1,norder+1
        if (nint(floats(nfloat-(i-1))).eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 2
          nfpot(nfit) = npote
          if (i.eq.1) then
            nfvar(nfit) = 1
          else
            nfvar(nfit) = 10 + norder - (i-1)
          endif
        endif
      enddo
      nfloat = nfloat - norder - 1
    else
      do i = 1,norder
        if (nint(floats(nfloat-(i-1))).eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 2
          nfpot(nfit) = npote
          nfvar(nfit) = 9 + norder - (i-1)
        endif
      enddo
      nfloat = nfloat - norder
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 405
  endif
  if (mmexc(npote).eq.1) then
!
!  If number of floats is greater than norder+1, assume that fitting flags have been left on line
!
    if (lpharm) then
      if (nfloat.gt.norder+1) nfloat = nfloat - norder - 1
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.norder+1) then
        do i = 1,norder
          tpot(i,npote) = floats(i+nbeg)*units
        enddo
        twopot(1,npote) = floats(norder+1+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    else
      if (nfloat.gt.norder) nfloat = nfloat - norder
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.norder) then
        do i = 1,norder
          tpot(i,npote) = floats(i+nbeg)*units
        enddo
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  else
!
!  If number of floats is greater than norder+3, assume that fitting flags have been left on line
!
    if (lpharm) then
      if (nfloat.gt.(norder+3)) nfloat = nfloat - norder - 1
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.norder+3) then
        do i = 1,norder
          tpot(i,npote) = floats(i+nbeg)*units
        enddo
        twopot(1,npote) = floats(norder+1+nbeg)
        rpot2(npote)    = floats(norder+2+nbeg)
        rpot(npote)     = floats(norder+3+nbeg)
      elseif (nfloat.eq.norder+2) then
        do i = 1,norder
          tpot(i,npote) = floats(i+nbeg)*units
        enddo
        twopot(1,npote) = floats(norder+1+nbeg)
        rpot2(npote)    = 0.0_dp
        rpot(npote)     = floats(norder+2+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    else
      if (nfloat.gt.(norder+3)) nfloat = nfloat - norder
!
!  Assign coefficients and cutoffs
!

      if (nfloat.ge.norder+3) then
        do i = 1,norder
          tpot(i,npote) = floats(i+nbeg)*units
        enddo
        twopot(1,npote) = floats(norder+1+nbeg)
        rpot2(npote)    = floats(norder+2+nbeg)
        rpot(npote)     = floats(norder+3+nbeg)
      elseif (nfloat.eq.norder+2) then
        do i = 1,norder
          tpot(i,npote) = floats(i+nbeg)*units
        enddo
        twopot(1,npote) = floats(norder+1+nbeg)
        rpot2(npote)    = 0.0_dp
        rpot(npote)     = floats(norder+2+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword21')
      endif
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  if (lpharm) then
    nptype(npote) = 35
  else
    nptype(npote) = 23
  endif
  twopot(4,npote) = dble(norder)
  goto 405
408 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***********************************
!  gCoulomb subtraction potential  *
!***********************************
410 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
415 line  =  '  '
  read(iin,'(a)',end = 418) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 415
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 418
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-1))
    n2 = int(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 415
  endif
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).ne.1) then
    if (nfloat.ge.4) then
      twopot(1,npote) = floats(1+nbeg)
      twopot(2,npote) = floats(2+nbeg)
      rpot2(npote)    = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)
      twopot(2,npote) = floats(2+nbeg)
      rpot2(npote)    = 0.0_dp
      rpot(npote)     = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 53
!
!  Check that if harmonic is core-shell spring constant then 
!  coulomb subtraction is turned off.
!
  if (abs(nvar1-nvar2).eq.maxele) then
    if (rpot2(npote).lt.cuts) then
      nwarn = nwarn + 1
      if (ioproc) then
        write(ioout,'(/,''**** Warning - core-shell pair is being doubly coulomb subtracted ****'',/)')
      endif
    endif
  endif
  goto 415
418 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***************************************************
!  Parameters for Four Range Buckingham potential  *
!***************************************************
740 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote+1) = .false.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote+1) = 1
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote+1) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote+1) = 3
      elseif (index(words(i),'o14').eq.1) then
        mmexc(npote+1) = 4
        lintra(npote+1) = .true.
        linter(npote+1) = .false.
        lfound = .true.
      elseif (index(words(i),'g14').eq.1) then
        mmexc(npote+1) = 5
        lintra(npote+1) = .true.
        linter(npote+1) = .true.
        lfound = .true.
      elseif (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n2botype(1,npote+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n2botype(1,npote+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n2botype(1,npote+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n2botype(1,npote+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n2botype(1,npote+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n2botype(1,npote+1) = 6
      elseif (index(words(i),'cyc').eq.1) then
        n2botype(2,npote+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n2botype(2,npote+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
!
!  1-4 scaling factor option
!
  if (nfloat.gt.0) then
    rscale = abs(floats(1))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
745 line = '  '
  read(iin,'(a)',end=748) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 745
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 748
    endif
  endif
  npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
!
!  Copy first line info from first potential
!
  mmexc(npote) = mmexc(npot1)
  n2botype(1:2,npote) = n2botype(1:2,npot1)
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
  scale14(npote) = scale14(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    n1 = int(floats(nfloat-3))
    n2 = int(floats(nfloat-2))
    n3 = int(floats(nfloat-1))
    n4 = int(floats(nfloat))
    nfloat = nfloat - 4
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 4
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 745
  endif
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.6) nfloat = nfloat - 4
!
!  Assign coefficients and cutoffs
!
    if (nfloat.ge.6) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
      tpot(1,npote) = floats(4+nbeg)
      twopot(4,npote) = floats(5+nbeg)
      tpot(2,npote) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  else
!
!  If number of floats is greater than 8, assume that fitting flags have been left on line
!
    if (nfloat.gt.8) nfloat = nfloat - 4
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.8) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
      rpot2(npote)    = floats(4+nbeg)
      tpot(1,npote)   = floats(5+nbeg)
      twopot(4,npote) = floats(6+nbeg)
      tpot(2,npote)   = floats(7+nbeg)
      rpot(npote)     = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
      rpot2(npote)    = 0.0_dp
      tpot(1,npote)   = floats(4+nbeg)
      twopot(4,npote) = floats(5+nbeg)
      tpot(2,npote)   = floats(6+nbeg)
      rpot(npote)     = floats(7+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword21')
    endif
  endif
!
!  Check rho is non-zero
!
  if (twopot(2,npote).lt.1.0d-8) then
    call outerror('Four range Buckingham potl has a rho of zero',iline)
    call stopnow('potword21')
  endif
!
  if (nvar1.eq.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    if (itype1.lt.itype2) then
      nptyp1(npote) = itype1
      nptyp2(npote) = itype2
      symbol2(1,npote) = sym21
      symbol2(2,npote) = sym22
    else
      nptyp1(npote) = itype2
      nptyp2(npote) = itype1
      symbol2(1,npote) = sym22
      symbol2(2,npote) = sym21
    endif
  elseif (nvar1.lt.nvar2) then
    nspec1(npote) = nvar1
    nspec2(npote) = nvar2
    nptyp1(npote) = itype1
    nptyp2(npote) = itype2
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  else
    nspec1(npote) = nvar2
    nspec2(npote) = nvar1
    nptyp1(npote) = itype2
    nptyp2(npote) = itype1
    symbol2(1,npote) = sym22
    symbol2(2,npote) = sym21
  endif
  nptype(npote) = 10
  call buck4(npote)
  goto 745
748 if (.not.l55) l1000 = .true.
  lwordok = .true.
!
  return
  end
