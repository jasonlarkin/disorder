  subroutine potword22(iin,word,lwordok,iline,line,l55,l1000,linr,lint,llibrary,lfflags,ncurr)
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
!  20 = Rydberg / Rose-Smith-Guinea-Ferrante potential
!  21 = Lennard-Jones with epsilon/sigma input : ESFF combination rules
!  22 = Q taper (short range Coulomb taper)
!  23 = Polynomial of order n (n >= 1)
!  24 = Qerfc (Coulomb multipied by erfc)
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
!  38 = srglue
!  39 = Morse with inverse exponential taper
!  40 = Morse with inverse exponential taper with Coloumb subtraction
!  41 = Mei-Davenport
!  42 = erferfc potential
!  43 = reperfc potential
!  44 = erf potential
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
!   4/98 Assignment of species type for library call changed
!        so that potential file form is accepted
!   5/98 qtaper potential added
!  10/98 Codes for fitting variables simplified
!   1/99 1-4 only option added for Coulomb potential and general scaling
!   4/99 Qerfc potential added
!   2/01 Fermi-Dirac potential added
!   6/01 Initialisation of line added for benefit of some compilers
!   5/02 BSM made core/shell specific
!   8/02 Lennard-Jones potential with shift added
!  11/02 Einstein model input added
!   5/04 Tsuneyuki potential added
!   9/04 Stillinger-Weber with Jiang-Brown bond softening added
!  10/04 Calls to getpotsymbol introduced to reduce code size
!   6/05 Ambiguity with split/spline fixed
!   7/05 Error in handling of potcut calls for multiple potentials in the
!        same read fixed
!   5/06 Format of warning statements updated
!   8/06 iin passed to linepro
!   8/06 Separate flag added for fitting flags
!   8/06 Bonding type sub-option added
!   9/06 Literal symbols now returned from getpotsymbol2
!  10/06 Literal symbols now returned from getpotsymbol1
!  10/06 Error in setting of nvar2 fixed
!  11/06 Calls to init2bodydefaults added to guard against hangovers from
!        previous potentials that might have been rejected.
!  11/06 qoverr2 potential added 
!   1/07 Amide bond type added
!   3/07 srglue potential added
!   5/07 Handling of symbol2 corrected when changing species order
!   7/07 Plane potential added
!  11/07 Mei-Davenport potential added
!  12/07 Unused variables removed
!  12/08 Module input renamed to gulpinput
!  12/08 Baskes potential added
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 Use of nfvar for two-body potentials modified
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   1/09 VBO_twobody potential added
!   3/09 Incorrect setting of nfit using nfit0 removed for Coulomb/r2 pot
!   4/09 Reading of Baskes input corrected for case where symbol is not valid
!   4/09 d term added to Baskes potental 
!   4/09 Flag checking for spring potential modified so that case spring constant can be 1.0
!   7/09 Exppowers potential added
!   7/09 Bug in Baskes potential fitting flags corrected
!   8/09 Grimme_C6 potential added
!   2/10 Central force model potentials added
!   7/11 Missing return statement after spline read added
!  10/11 Call to cart2frac modified by adding cell indices
!   1/12 Copying of scale14 when there are multiple potentials under the same option added
!   1/12 g12 mmexc=5 option added
!   5/12 Becke_Johnson_C6 added
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
  use configurations
  use constants
  use control
  use element, only : maxele
  use fitting
  use general, only : nwarn
  use gulpinput
  use iochannels
  use molecule
  use parallel
  use plane
  use shell
  use shifts
  use species
  use splinedata
  use two
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
  integer(i4)                  :: icx
  integer(i4)                  :: icy
  integer(i4)                  :: icz
  integer(i4)                  :: itype1
  integer(i4)                  :: itype2
  integer(i4)                  :: n
  integer(i4)                  :: n1
  integer(i4)                  :: n2
  integer(i4)                  :: n3
  integer(i4)                  :: n4
  integer(i4)                  :: n5
  integer(i4)                  :: nbeg
  integer(i4)                  :: nfit0
  integer(i4)                  :: np
  integer(i4)                  :: npadd
  integer(i4)                  :: npot1
  integer(i4)                  :: nplanepot1
  integer(i4)                  :: nvar1
  integer(i4)                  :: nvar2
  integer(i4)                  :: nw
  logical                      :: lcartin
  logical                      :: leof
  logical                      :: lfound
  logical                      :: lk4
  logical                      :: lreverse
  logical                      :: lsymbol
  logical                      :: lvalidpot
  logical                      :: lwarnp
  real(dp)                     :: rd5
  real(dp)                     :: rscale
  real(dp)                     :: ta
  real(dp)                     :: ta2
  real(dp)                     :: tb
  real(dp)                     :: tb2
  real(dp)                     :: units
  real(dp)                     :: xloc
  real(dp)                     :: yloc
  real(dp)                     :: zloc
!
!  Initialise local variables
!
  lwarnp = .not.lmol
  if (index(word,'gener').eq.1) goto 450
  if (index(word,'ljbuf').eq.1) goto 460
  if (index(word,'intera').eq.1) goto 470
  if (index(word,'inter ').eq.1) goto 470
  if (index(word,'intr').eq.1) goto 480
  if (index(word,'both').eq.1) goto 490
  if (index(word,'bsm') .eq.1) goto 500
  if (index(word,'tsun').eq.1) goto 510
  if (index(word,'srgl').eq.1) goto 520
  if (index(word,'plan').eq.1) goto 530
  if (index(word,'spri').eq.1) goto 590
  if (index(word,'coul').eq.1) goto 600
  if (index(word,'sw2 ').eq.1) goto 610
  if (index(word,'igau').eq.1) goto 620
  if (index(word,'rose').eq.1) goto 630
  if (index(word,'ryd').eq.1)  goto 630
  if (index(word,'qtap').eq.1) goto 640
  if (index(word,'qerf').eq.1) goto 650
  if (index(word,'ferm').eq.1) goto 660
  if (index(word,'eins').eq.1) goto 670
  if (index(word,'sw2j').eq.1) goto 680
  if (index(word,'qove').eq.1) goto 690
  if (index(word,'mei').eq.1)  goto 700
  if (index(word,'bas').eq.1)  goto 710
  if (index(word,'vbo').eq.1)  goto 720
  if (index(word,'expp').eq.1) goto 730
  if (index(word,'grim').eq.1) goto 740
  if (index(word,'beck').eq.1) goto 750
  if (index(word,'splin').eq.1) goto 770
  if (index(word,'cfm_h').eq.1) goto 800
  if (index(word,'cfm_g').eq.1) goto 810
  if (index(word,'cfm_p').eq.1) goto 820
  if (index(word,'cfm_f').eq.1) goto 830
  return
!**********************
!  General potential  *
!**********************
!  Has options to be Buck, LJ, have energy and gradient shift
!
450 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Before reading second line check for additional parameters
!
  if (nfloat.eq.1) then
    tpot(1,npote+1) = floats(1)
    tpot(2,npote+1) = 6.0_dp
  elseif (nfloat.ge.2) then
    tpot(1,npote+1) = floats(1)
    tpot(2,npote+1) = floats(2)
  else
    tpot(1,npote+1) = 0.0_dp
    tpot(2,npote+1) = 6.0_dp
  endif
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
  if (nfloat.ge.3) then
    rscale = abs(floats(3))
    if (rscale.ge.0.0_dp.and.rscale.lt.1.0_dp) then
      scale14(npote+1) = rscale
    endif
  endif
  npot1 = npote + 1
455 line  =  '  '
  read(iin,'(a)',end = 458) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 455
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 458
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
    goto 455
  endif
!
  if (mmexc(npote).eq.1) then
!
!  If number of floats is greater than 5, assume that fitting flags have been left on line
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
      call stopnow('potword22')
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
      rpot(npote)     = floats(5+nbeg)
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
  nptype(npote) = 7
  goto 455
458 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!****************************************************
!  Parameters for Lennard-Jones buffered potential  *
!****************************************************
460 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  tpot(1,npote+1) = 12.0_dp
  tpot(2,npote+1) = 6.0_dp
  nptype(npote+1) = 27
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
465 line = '  '
  read(iin,'(a)',end=468) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 465
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 468
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
  nptype(npote) = nptype(npot1)
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and..not.lcombine(npote).and.lfflags) then
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
    goto 465
  endif
!
  if (nfloat.gt.5) nfloat = nfloat - 3
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.ge.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      twopot(3,npote) = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      twopot(3,npote) = 0.0_dp
    elseif (nfloat.eq.1) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = 0.0_dp
      twopot(3,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  else
    if (nfloat.ge.5) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      twopot(3,npote) = floats(3+nbeg)
      rpot2(npote)    = floats(4+nbeg)
      rpot(npote)     = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      twopot(3,npote) = floats(3+nbeg)
      rpot(npote)     = floats(4+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)*units
      twopot(3,npote) = 0.0_dp
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = 0.0_dp
      twopot(3,npote) = 0.0_dp
      rpot(npote)     = floats(2+nbeg)
      rpot2(npote)    = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
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
  goto 465
468 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!****************************************************
!  Switch flag for potentials to be intermolecular  *
!****************************************************
470 if (lmol) then
    lint = .false.
    linr = .true.
  else
    if (ioproc) then
      write(ioout,'(''  **** Molecule directive not active ****'')')
      write(ioout,'(''  **** Inter directive to be ignored ****'')')
    endif
  endif
  lwordok = .true.
  return
!****************************************************
!  Switch flag for potentials to be intramolecular  *
!****************************************************
480 if (lmol) then
    lint = .true.
    linr = .false.
  else
    if (ioproc) then
      write(ioout,'(''  **** Molecule directive not active ****'')')
      write(ioout,'(''  **** Intra directive to be ignored ****'')')
    endif
  endif
  lwordok = .true.
  return
!**************************************************************
!  Switch potentials to act both inter and intra molecularly  *
!**************************************************************
490 if (lmol) then
    lint = .true.
    linr = .true.
  else
    if (ioproc) then
      write(ioout,'(''  **** Molecule directive not active ****'')')
      write(ioout,'(''  **** Both directive to be ignored  ****'')')
    endif
  endif
  lwordok = .true.
  return
!*************************
!  Breathing shell model *
!*************************
!
!  If nptype = 14 => harmonic form
!            = 17 => exponential form
!            = 31 => single exponential form
!
500 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  lintra(npote+1) = .true.
  linter(npote+1) = .true.
  npot1 = npote + 1
  units = 1.0_dp
  npadd = 0
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'exp').eq.1) then
        npadd = 3
      elseif (index(words(i),'sin').eq.1) then
        npadd = 17
      endif
      i = i + 1
    enddo
  endif
505 line = '  '
  read(iin,'(a)',end=508) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 505
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 508
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
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
    if (nword.gt.0) then
      if (npadd.eq.0) then
        if (nfloat.ge.4) then
          n2 = int(floats(nfloat))
          n1 = int(floats(nfloat-1))
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
          n1 = int(floats(nfloat))
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
        endif
      else
        if (nfloat.ge.6) then
          n3 = int(floats(nfloat))
          n2 = int(floats(nfloat-1))
          n1 = int(floats(nfloat-2))
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
      endif
    else
      if (npadd.eq.0) then
        if (nfloat.ge.5) then
          n2 = int(floats(nfloat))
          n1 = int(floats(nfloat-1))
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
          n1 = int(floats(nfloat))
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
        endif
      else
        if (nfloat.ge.7) then
          n3 = int(floats(nfloat))
          n2 = int(floats(nfloat-1))
          n1 = int(floats(nfloat-2))
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
      endif
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
    if (npadd.eq.3) then
      if (floats(nfloat).eq.1.0_dp.or.floats(nfloat).eq.0.0_dp) then
        nfloat = nfloat - 1
      endif
    endif
  endif
!
!  Process symbol input
!     
  if (nword.gt.0) then
    sym22 = ' '
    call getpotsymbol1(iline,llibrary,nvar1,itype1,sym21,0_i4,nbeg,lvalidpot)
    if (.not.lvalidpot) then
      npote = npote - 1
      nfit = nfit0
      goto 505
    endif
    sym22 = sym21
    nvar2 = nvar1
    itype2 = itype1
!
!  Numeric input
!
  else
    nvar1 = int(floats(1))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    nvar2 = nvar1
    itype1 = 0
    itype2 = 0
    nbeg = 1
    nfloat = nfloat - 1
  endif
!
!  Assign coefficients and cutoffs
!
  if (npadd.eq.0) then
    if (nfloat.ge.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
    elseif (nfloat.eq.1) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  else
    if (nfloat.ge.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = 0.0_dp
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
  nptype(npote) = 14 + npadd
!
!  Set small cutoffs so that potential can't be used
!  for anything other than breathing shell
!
  rpot2(npote) = 0.0_dp
  rpot(npote)  = 0.1_dp
  goto 505
508 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************
!  Coulomb erfc  *
!*****************
510 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  tpot(1,npote+1) = 1.0_dp
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
      elseif (index(words(i),'form2').eq.1) then
        tpot(1,npote+1) = 2.0_dp
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
        write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
        write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
  npot1 = npote + 1
515 line = '  '
  read(iin,'(a)',end=518) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 515
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 518
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
  leshift(npote) = leshift(npot1)
  lgshift(npote) = lgshift(npot1)
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
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 515
  endif
!
  if (mmexc(npote).eq.1) then
!
!  If number of floats is greater than 3, assume that fitting flags have been left on line
!
    if (nfloat.gt.3) nfloat = nfloat - 1
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  else
!
!  If number of floats is greater than 5, assume that fitting flags have been left on line
!
    if (nfloat.gt.5) nfloat = nfloat - 1
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.5) then
      twopot(1,npote) = floats(1+nbeg)
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
      rpot2(npote)    = floats(4+nbeg)
      rpot(npote)     = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)*units
      rpot2(npote)    = 0.0_dp
      rpot(npote)     = floats(4+nbeg)
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
  nptype(npote) = 30
  goto 515
518 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!********************
!  Short-range Glue *
!********************
520 if (npote.eq.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  tpot(1,npote+1) = 1.0_dp
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
        write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
        write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
  npot1 = npote + 1
525 line = '  '
  read(iin,'(a)',end=528) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 525
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 528
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    goto 525
  endif
!
  if (mmexc(npote).eq.1) then
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.1) then
      twopot(1,npote) = floats(1+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  else
!
!  Assign coefficients and cutoffs
!
    if (nfloat.ge.3) then
      twopot(1,npote) = floats(1+nbeg)
      rpot2(npote)    = floats(2+nbeg)
      rpot(npote)     = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)
      rpot2(npote)    = 0.0_dp
      rpot(npote)     = floats(2+nbeg)
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
  nptype(npote) = 38
!
!  First polynomial coefficient line
!
  line = '  '
  read(iin,'(a)',end=528) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.ge.5) then
    tpot(1,npote) = floats(1)*units
    tpot(2,npote) = floats(2)*units
    tpot(3,npote) = floats(3)*units
    tpot(4,npote) = floats(4)*units
    tpot(5,npote) = floats(5)*units
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
  endif
!
!  Second polynomial coefficient line
!
  line = '  '
  read(iin,'(a)',end=528) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.ge.7) then
    tpot(6,npote)  = floats(1)*units
    tpot(7,npote)  = floats(2)*units
    tpot(8,npote)  = floats(3)*units
    tpot(9,npote)  = floats(4)*units
    tpot(10,npote) = floats(5)*units
    tpot(11,npote) = floats(6)*units
    tpot(12,npote) = floats(7)*units
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
  endif
!
  goto 525
528 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************************************
!  Parameters for plane Lennard-Jones potential  *
!*************************************************
530 if (nplanepot.eq.maxplanepot) then
    maxplanepot = nplanepot + 1
    call changemaxplanepot
  endif
  nplanepotpower(1,nplanepot+1) = 10
  nplanepotpower(2,nplanepot+1) =  4
  nplanepottype(nplanepot+1) = 1
!
!  Set intra / inter / both flags
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
!
!  Set exponents if specified
!
  if (nfloat.ge.2) then
    nplanepotpower(1,nplanepot+1) = nint(floats(1))
    nplanepotpower(2,nplanepot+1) = nint(floats(2))
  elseif (nfloat.eq.1) then
    nplanepotpower(1,nplanepot+1) = nint(floats(1))
  endif
  nplanepot1 = nplanepot + 1
535 line = '  '
  read(iin,'(a)',end=538) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 535
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 538
    endif
  endif
  nplanepot = nplanepot + 1
  if (nplanepot.gt.maxplanepot) then
    maxplanepot = nplanepot + 1
    call changemaxplanepot
  endif
!
!  Copy first line info from first potential
!
  nplanepotpower(1,nplanepot) = nplanepotpower(1,nplanepot1)
  nplanepotpower(2,nplanepot) = nplanepotpower(2,nplanepot1)
  nplanepottype(nplanepot) = nplanepottype(nplanepot1)
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
      nftyp(nfit) = 1
      nfpot(nfit) = 28
      nfvar(nfit) = nplanepot
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 29
      nfvar(nfit) = nplanepot
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym21,0_i4,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nplanepot = nplanepot - 1
    nfit = nfit0
    goto 535
  endif
!
!  Assign species type and symbol
!
  natplanepot(nplanepot) = nvar1
  ntypplanepot(nplanepot) = itype1
  planepotsymbol(nplanepot) = sym21
  if (nfloat.gt.5) nfloat = nfloat - 2
!
!  Assign coefficients and cutoffs
!
  if (nfloat.eq.5) then
    planepot(1,nplanepot) = floats(1+nbeg)
    planepot(2,nplanepot) = floats(2+nbeg)*units
    planepot(3,nplanepot) = floats(3+nbeg)*units
    planepotrmin(nplanepot) = floats(4+nbeg)
    planepotrmax(nplanepot) = floats(5+nbeg)
  elseif (nfloat.eq.4) then
    planepot(1,nplanepot) = floats(1+nbeg)
    planepot(2,nplanepot) = floats(2+nbeg)*units
    planepot(3,nplanepot) = floats(3+nbeg)*units
    planepotrmin(nplanepot) = 0.0_dp
    planepotrmax(nplanepot) = floats(4+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
  endif
  goto 535
538 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*********************************
!  Core - shell spring constant  *
!*********************************
590 if (npote.eq.maxpot) then
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
595 line  =  '  '
  read(iin,'(a)',end = 598) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 595
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 598
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
  lintra(npote) = lintra(npot1)
  linter(npote) = linter(npot1)
!
!  Fitting flags
!
  lk4 = .false.
  nfit0 = nfit
  if (lfit.and.lfflags) then
    if (nword.gt.0) then
      if (nfloat.ge.4) then
        lk4 = .true.
        n2 = int(floats(nfloat))
        n1 = int(floats(nfloat-1))
        nfloat = nfloat - 2
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
      else
        n1 = int(floats(nfloat))
        nfloat = nfloat - 1
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
      endif
    else
      if (nfloat.ge.5) then
        lk4 = .true.
        n2 = int(floats(nfloat))
        n1 = int(floats(nfloat-1))
        nfloat = nfloat - 2
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
      else
        n1 = int(floats(nfloat))
        nfloat = nfloat - 1
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
      endif
    endif
  else
!
!  Check to make sure that no fitting flags have been left on the line
!
    if (nfloat.gt.1.and.(floats(nfloat).eq.1.0_dp.or.floats(nfloat).eq.0.0_dp)) then
      nfloat = nfloat - 1
    endif
    if (nfloat.gt.1.and.(floats(nfloat).eq.1.0_dp.or.floats(nfloat).eq.0.0_dp)) then
      nfloat = nfloat - 1
      lk4 = .true.
    endif
  endif
!
!  Process symbol input
!
  if (nword.gt.0) then
    sym22 = ' '
    call getpotsymbol1(iline,llibrary,nvar1,itype1,sym21,0_i4,nbeg,lvalidpot)
    if (.not.lvalidpot) then
      npote = npote - 1
      nfit = nfit0
      goto 595
    endif
    sym22 = sym21
    if (nvar1.gt.maxele) then
      nvar2 = nvar1 - maxele
    else
      nvar2 = nvar1 + maxele
    endif
    itype2 = itype1
!
!  Numeric input
!
  elseif (nfloat.ge.3.and..not.lk4) then
    nvar1 = int(floats(1))
    nvar2 = int(floats(2))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
    itype1 = 0
    itype2 = 0
    nbeg = 2
    nfloat = nfloat - 2
  elseif (nfloat.ge.4.and.lk4) then
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
  elseif (nfloat.eq.1) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = 0.0_dp
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
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
    symbol2(1,npote) = sym21
    symbol2(2,npote) = sym22
  endif
  nptype(npote) = 8
  goto 595
598 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**********************************
!  Coulomb subtraction potential  *
!**********************************
600 if (npote.eq.maxpot) then
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
605 line  =  '  '
  read(iin,'(a)',end = 608) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 605
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 608
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
    n1 = int(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 605
  endif
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).ne.1) then
    if (nfloat.ge.3) then
      twopot(4,npote) = floats(1+nbeg)
      rpot2(npote)    = floats(2+nbeg)
      rpot(npote)     = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(4,npote) = 1.0_dp
      rpot2(npote)    = floats(1+nbeg)
      rpot(npote)     = floats(2+nbeg)
    elseif (nfloat.eq.1) then
      twopot(4,npote) = 1.0_dp
      rpot(npote)     = floats(1+nbeg)
      rpot2(npote)    = 0.0_dp
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
  nptype(npote) = 9
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
  goto 605
608 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***************************************
!  Stillinger-Weber twobody potential  *
!***************************************
610 if (npote.eq.maxpot) then
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
615 line = '  '
  read(iin,'(a)',end=618) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 615
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 618
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 615
  endif
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
    call stopnow('potword22')
  endif
!
!  Check rho is non-zero
!
  if (twopot(2,npote).lt.1.0d-8) then
    call outerror('SW2 potential has a rho of zero',iline)
    call stopnow('potword22')
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
  nptype(npote) = 15
  goto 615
618 if (.not.l55) l1000 = .true.
  lwordok=.true.
  return
!*******************************
!  Inverse gaussian potential  *
!*******************************
620 if (npote.eq.maxpot) then
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
625 line = '  '
  read(iin,'(a)',end=628) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 625
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 628
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,symbol2(1,npote),nvar2,itype2,symbol2(2,npote),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 625
  endif
!
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
      twopot(3,npote) = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
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
      twopot(3,npote) = floats(3+nbeg)
      rpot2(npote)    = floats(4+nbeg)
      rpot(npote)     = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
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
      call stopnow('potword22')
    endif
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
  nptype(npote) = 16
  goto 625
628 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!*****************************************
!  Rose/Smith/Guinea/Ferrante potential  *
!*****************************************
630 if (npote.eq.maxpot) then
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
635 line = '  '
  read(iin,'(a)',end=638) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 635
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 638
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,symbol2(1,npote),nvar2,itype2,symbol2(2,npote),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 635
  endif
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
    call stopnow('potword22')
  endif
!
!  Check rho is non-zero
!
  if (twopot(3,npote).lt.1.0d-8) then
    call outerror('Rose potential has a r0 of zero',iline)
    call stopnow('potword22')
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
  nptype(npote) = 20
  goto 635
638 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!******************
!  Coulomb taper  *
!******************
640 if (npote.eq.maxpot) then
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
645 line = '  '
  read(iin,'(a)',end=648) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 645
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 648
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
    n1 = int(floats(nfloat))
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
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,symbol2(1,npote),nvar2,itype2,symbol2(2,npote),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 645
  endif
!
  if (mmexc(npote).eq.1) then
!
!  If number of floats is greater than 3, assume that fitting flags have been left on line
!
    if (nfloat.gt.1) nfloat = nfloat - 1
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.1) then
      twopot(1,npote) = floats(1+nbeg)*units
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  else
!
!  If number of floats is greater than 2, assume that fitting flags have been left on line
!
    if (nfloat.gt.2) nfloat = nfloat - 1
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      rpot2(npote) = 0.0_dp
      rpot(npote)  = floats(2+nbeg)
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
  nptype(npote) = 22
!
!  Set up taper constants
!
  if (rpot(npote).gt.1.0d-3) then
    tb = rpot(npote)
    tb2 = tb*tb
    ta = tb - rpot(npote)
    ta2 = ta*ta
    rd5 = 1.0_dp/(rpot(npote)**5.0_dp)
    tpot(5,npote) = (10.0_dp*ta2*tb+(tb-5.0_dp*ta)*tb2)*tb2*rd5
    tpot(6,npote) = -30.0_dp*ta2*tb2*rd5
    tpot(7,npote) = 30.0_dp*(ta2*tb+ta*tb2)*rd5
    tpot(8,npote) = -10.0_dp*(ta2+4.0_dp*ta*tb+tb2)*rd5
    tpot(9,npote) = 15.0_dp*(ta+tb)*rd5
    tpot(10,npote) = -6.0_dp*rd5
  endif
  goto 645
648 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************
!  Coulomb erfc  *
!*****************
650 if (npote.eq.maxpot) then
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
        units = autoangs
      elseif (index(words(i),'pm').eq.1) then
        units = 0.01_dp
      elseif (index(words(i),'nm').eq.1) then
        units = 10.0_dp
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
        write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
        write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
  npot1 = npote + 1
655 line = '  '
  read(iin,'(a)',end=658) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 655
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 658
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
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 655
  endif
!
  if (mmexc(npote).eq.1) then
!
!  If number of floats is greater than 1, assume that fitting flags have been left on line
!
    if (nfloat.gt.1) nfloat = nfloat - 1
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.1) then
      twopot(1,npote) = floats(1+nbeg)*units
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  else
!
!  If number of floats is greater than 2, assume that fitting flags have been left on line
!
    if (nfloat.gt.2) nfloat = nfloat - 1
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      rpot2(npote)    = 0.0_dp
      rpot(npote)     = floats(2+nbeg)
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
  nptype(npote) = 24
  goto 655
658 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!***********************************
!  Parameters for Fermi potential  *
!***********************************
660 if (npote.eq.maxpot) then
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
665 line = '  '
  read(iin,'(a)',end=668) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 665
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 668
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 665
  endif
!
!  Assign coefficients and cutoffs
!
  if (mmexc(npote).eq.1) then
    if (nfloat.gt.3) nfloat = nfloat - 3
    if (nfloat.ge.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword22')
    endif
  else
    if (nfloat.gt.5) nfloat = nfloat-3
    if (nfloat.ge.5) then
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
    elseif (nfloat.eq.3) then
      twopot(1,npote) = floats(1+nbeg)*units
      twopot(2,npote) = floats(2+nbeg)
      twopot(3,npote) = 0.0_dp
      rpot(npote)     = floats(3+nbeg)
      rpot2(npote)    = 0.0_dp
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
  nptype(npote) = 26
  goto 665
668 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!************************
!  Einstein model data  *
!************************
670 lcartin = .false.
  if (nword.gt.1) then
    do nw = 2,nword
      call stolc(words(nw),maxword)
      if (index(words(nw),'cart').eq.1) then
        lcartin = .true.
      endif
    enddo
  endif
!
!  Start of input loop
!
675 line = '  '
  read(iin,'(a)',end=678) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether the line is blank
!
  if ((nword+nfloat).eq.0) goto 675
!
!  Check whether there is a word on the line in which case this is a new option
!
  if (nword.gt.0) then
    l55 = .true.
    goto 678
  endif
!
!  Check whether there is enough data on the line
!
  if (nfloat.lt.5) then
    call outerror('Missing data in Einstein model data',iline)
    call stopnow('potword22')
  endif
!
!  Check that first number is a valid atom number for the present structure
!
  n = nint(floats(1))
  if (n.gt.nascfg(ncurr)) then
    call outerror('Invalid atom number in Einstein model data',iline)
    call stopnow('potword22')
  endif
  n = nasum - nascfg(ncurr) + n
  leinsteinat(n) = .true.
  xeinsteinat(n) = floats(2)
  yeinsteinat(n) = floats(3)
  zeinsteinat(n) = floats(4)
  keinsteinat(n) = floats(5)
  goto 675
!
!  End of input loop
!
678 if (.not.l55) l1000 = .true.
!
!  Convert coordinates from Cartesian to fractional if necessary
!
  if (lcartin) then
    n = nascfg(ncurr)
    do i = 1,n
      xloc = xeinsteinat(i)
      yloc = yeinsteinat(i)
      zloc = zeinsteinat(i)
      call cart2frac(ndimen(ncfg),xloc,yloc,zloc,rvcfg(1,1,ncurr),xeinsteinat(i),yeinsteinat(i),zeinsteinat(i), &
                     icx,icy,icz)
    enddo
  endif
  lwordok = .true.
  return
!********************************************************************
!  Stillinger-Weber twobody potential - Jiang & Brown modification  *
!********************************************************************
680 if (npote.eq.maxpot) then
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
        lfound=.true.
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
685 line = '  '
  read(iin,'(a)',end=688) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 685
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 688
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 685
  endif
!
!  If number of floats is greater than 6, assume that fitting flags have been left on line
!
  if (nfloat.gt.6) nfloat = nfloat - 4
!
!  Assign coefficients and cutoffs
!
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
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
  endif
!
!  Check rho is non-zero
!
  if (twopot(2,npote).lt.1.0d-8) then
    call outerror('SW2jb potential has a rho of zero',iline)
    call stopnow('potword22')
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
  nptype(npote) = 32
  goto 685
688 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!********************
!  Coulomb over r2  *
!********************
690 if (npote.eq.maxpot) then
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
        units = autoangs
      elseif (index(words(i),'pm').eq.1) then
        units = 0.01_dp
      elseif (index(words(i),'nm').eq.1) then
        units = 10.0_dp
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
        write(ioout,'(/,''**** Warning - inter/intra/both directive given on potential line ****'')')
        write(ioout,'(''**** but the molecule directive is currently inactive             ****'')')
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    lintra(npote+1) = lint
    linter(npote+1) = linr
  endif
  npot1 = npote + 1
695 line = '  '
  read(iin,'(a)',end=698) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 695
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 698
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
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    goto 695
  endif
!
  if (mmexc(npote).ne.1) then
!
!  Assign coefficients and cutoffs
!
    if (nfloat.eq.1) then
      rpot(npote) = floats(1+nbeg)
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
  nptype(npote) = 36
  goto 695
698 if (.not.l55) l1000 = .true.
  do np = npot1,npote
    if (leshift(np)) call potcut(np)
  enddo
  lwordok = .true.
  return
!************************************
!  Mei-Davenport twobody potential  *
!************************************
700 if (npote.eq.maxpot) then
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
        lfound=.true.
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
705 line = '  '
  read(iin,'(a)',end=708) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 705
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 708
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 705
  endif
!
!  If number of floats is greater than 6, assume that fitting flags have been left on line
!
  if (nfloat.gt.6) nfloat = nfloat - 4
!
!  Assign coefficients and cutoffs
!
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
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
  endif
!
!  Check r0 is non-zero
!
  if (twopot(4,npote).lt.1.0d-8) then
    call outerror('Mei-Davenport potential has an r0 of zero',iline)
    call stopnow('potword22')
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
  nptype(npote) = 41
  goto 705
708 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************************
!  Baskes twobody potential  *
!*****************************
710 if (npote.eq.maxpot) then
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
        lfound=.true.
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
715 line = '  '
  read(iin,'(a)',end=718) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 715
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 718
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 715
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
    n1 = int(floats(nfloat-4))
    n2 = int(floats(nfloat-3))
    n3 = int(floats(nfloat-2))
    n4 = int(floats(nfloat-1))
    n5 = int(floats(nfloat))
    nfloat = nfloat - 5
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
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 7
    endif
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 715
  endif
!
!  If number of floats is greater than 9, assume that fitting flags have been left on line
!
  if (nfloat.gt.9) nfloat = nfloat - 5
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.9) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    twopot(6,npote) = floats(6+nbeg)
    twopot(7,npote) = floats(7+nbeg)
    rpot2(npote)    = floats(8+nbeg)
    rpot(npote)     = floats(9+nbeg)
  elseif (nfloat.eq.8) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    twopot(6,npote) = floats(6+nbeg)
    twopot(7,npote) = floats(7+nbeg)
    rpot(npote)     = floats(8+nbeg)
    rpot2(npote)    = 0.0_dp
  elseif (nfloat.eq.7) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    twopot(6,npote) = floats(6+nbeg)
    twopot(7,npote) = 0.0_dp
    rpot(npote)     = floats(8+nbeg)
    rpot2(npote)    = 0.0_dp
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
  endif
!
!  Check r0 is non-zero
!
  if (twopot(4,npote).lt.1.0d-8) then
    call outerror('Baskes potential has an r0 of zero',iline)
    call stopnow('potword22')
  endif
!
!  Check rho0 is non-zero
!
  if (twopot(6,npote).lt.1.0d-8) then
    call outerror('Baskes potential has a rho0 of zero',iline)
    call stopnow('potword22')
  endif
!
!  Read density components
!
!  Line 1
!
  line = '  '
  read(iin,'(a)',end=718) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.ge.4) then
    tpot(1,npote) = floats(1)
    tpot(2,npote) = floats(2)
    tpot(3,npote) = floats(3)
    tpot(4,npote) = floats(4)
  else
    call outerror('incorrect density input for baskes potential',iline)
    call stopnow('potword22')
  endif
!
!  Line 2
!
  line = '  '
  read(iin,'(a)',end=718) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.ge.4) then
    tpot(5,npote) = floats(1)
    tpot(6,npote) = floats(2)
    tpot(7,npote) = floats(3)
    tpot(8,npote) = floats(4)
  else
    call outerror('incorrect density input for baskes potential',iline)
    call stopnow('potword22')
  endif
!
!  Line 3
!
  line = '  '
  read(iin,'(a)',end=718) line
  iline = iline + 1 
  call linepro(iin,line,iline)
  if (nfloat.ge.4) then
    tpot(9,npote)  = floats(1)
    tpot(10,npote) = floats(2)
    tpot(11,npote) = floats(3)
    tpot(12,npote) = floats(4)
  else
    call outerror('incorrect density input for baskes potential',iline)
    call stopnow('potword22')
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
  nptype(npote) = 45
  goto 715
718 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**************************
!  VBO_twobody potential  *
!**************************
720 if (npote.eq.maxpot) then
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
        lfound=.true.
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
725 line = '  '
  read(iin,'(a)',end=728) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 725
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 728
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 725
  endif
!
!  If number of floats is greater than 4, assume that fitting flags have been left on line
!
  if (nfloat.gt.4) nfloat = nfloat - 4
!
!  Assign coefficients and cutoffs
!
  if (nfloat.eq.4) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = abs(floats(3+nbeg))
    twopot(4,npote) = abs(floats(4+nbeg))
    rpot2(npote)    = 0.0_dp
! NB: For this potential the cutoff is equal to the delta parameter
    rpot(npote)     = abs(floats(4+nbeg))
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
  endif
!
!  Check r0 is non-zero
!
  if (twopot(4,npote).lt.1.0d-8) then
    call outerror('VBO_twobody potential has a delta of zero',iline)
    call stopnow('potword22')
  endif
!
!  Compute normalisation constant and store as twopot(5,npote)
!
  call setvbon(npote)
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
  nptype(npote) = 46
  goto 725
728 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!********************************
!  Exppowers twobody potential  *
!********************************
730 if (npote.eq.maxpot) then
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
        lfound=.true.
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
735 line = '  '
  read(iin,'(a)',end=738) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 735
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 738
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 735
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
    n1 = int(floats(nfloat-4))
    n2 = int(floats(nfloat-3))
    n3 = int(floats(nfloat-2))
    n4 = int(floats(nfloat-1))
    n5 = int(floats(nfloat))
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
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 5
    endif
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 735
  endif
!
!  If number of floats is greater than 7, assume that fitting flags have been left on line
!
  if (nfloat.gt.7) nfloat = nfloat - 5
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.7) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    rpot2(npote)    = floats(6+nbeg)
    rpot(npote)     = floats(7+nbeg)
  elseif (nfloat.eq.6) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    rpot(npote)     = floats(6+nbeg)
    rpot2(npote)    = 0.0_dp
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
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
  nptype(npote) = 47
  goto 735
738 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!************************
!  Grimme_C6 potential  *
!************************
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
        lfound=.true.
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
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 745
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 745
  endif
!
!  If number of floats is greater than 5, assume that fitting flags have been left on line
!
  if (nfloat.gt.5) nfloat = nfloat - 3
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.5) then
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
    call stopnow('potword22')
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
  nptype(npote) = 48
  goto 745
748 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*******************************
!  Becke_Johnson_C6 potential  *
!*******************************
750 if (npote.eq.maxpot) then
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
        lfound=.true.
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
755 line = '  '
  read(iin,'(a)',end=758) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 755
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 758
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 755
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 755
  endif
!
!  If number of floats is greater than 4, assume that fitting flags have been left on line
!
  if (nfloat.gt.4) nfloat = nfloat - 2
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.4) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    rpot2(npote)    = floats(3+nbeg)
    rpot(npote)     = floats(4+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
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
  nptype(npote) = 54
  goto 755
758 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!************************************
!  Parameters for Spline potential  *
!************************************
770 npote = npote + 1
  if (npote.gt.maxpot) then
    maxpot = npote + 10
    call changemaxpot
  endif
  call init2bodydefaults(npote+1_i4)
  nsplty(npote) = 1
  lreverse = .false.
!
!  Set intra / inter / both flags
!
  lfound = .false.
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lintra(npote) = .false.
        linter(npote) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lintra(npote) = .true.
        linter(npote) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lintra(npote) = .true.
        linter(npote) = .true.
        lfound = .true.
      elseif (index(words(i),'cub').eq.1) then
        nsplty(npote) = 3
      elseif (index(words(i),'rev').eq.1) then
        lreverse = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmexc(npote) = 1
        lintra(npote) = .true.
        linter(npote) = .false.
        lfound = .true.
      elseif (index(words(i),'x12').eq.1) then
        mmexc(npote) = 2
      elseif (index(words(i),'x13').eq.1) then
        mmexc(npote) = 3
      elseif (index(words(i),'mol').eq.1) then
        mmexc(npote) = 1
        lintra(npote) = .true.
        linter(npote) = .false.
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
    lintra(npote) = lint
    linter(npote) = linr
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
  line = '  '
  read(iin,'(a)') line
  iline = iline + 1
  call linepro(iin,line,iline)
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
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 1
    endif
  else
!
!  Check for residual fitting flags
!
    if (floats(nfloat).eq.1.0_dp.or.floats(nfloat).eq.0.0_dp) then
      nfloat = nfloat - 1
    endif
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 775
  endif
!
  if (mmexc(npote).eq.1) then
!
!  Assign coefficients and cutoffs
!
    if (nfloat.ge.1) then
      twopot(1,npote) = floats(1+nbeg)
    else
      twopot(1,npote) = 0.0_dp
    endif
  else
    if (nfloat.ge.3) then
      twopot(1,npote) = floats(1+nbeg)
      rpot2(npote)    = floats(2+nbeg)
      rpot(npote)     = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      twopot(1,npote) = floats(1+nbeg)
      rpot(npote)     = floats(2+nbeg)
      rpot2(npote)    = 0.0_dp
    elseif (nfloat.eq.1) then
      twopot(1,npote) = 0.0_dp
      rpot(npote)     = floats(1+nbeg)
      rpot2(npote)    = 0.0_dp
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
  nptype(npote) = 11
!
!  Read in spline points until option found
!
  nsplpt(npote) = 0
  leof = .false.
  do while (.not.leof)
    line = '  '
    read(iin,'(a)',end=776) line
    iline = iline + 1
    call linepro(iin,line,iline)
    if (nword.ne.0.and.nlorder(1).eq.1) goto 775
    nsplpt(npote) = nsplpt(npote) + 1
    if (nsplpt(npote).gt.maxpts) then
      maxpts = maxpts + 50
      call changemaxpts
    endif
    if (nfloat.ge.2) then
      if (lreverse) then
        splr(nsplpt(npote),npote) = floats(1)
        splf(nsplpt(npote),npote) = floats(2)*units
      else
        splf(nsplpt(npote),npote) = floats(1)*units
        splr(nsplpt(npote),npote) = floats(2)
      endif
    endif
  enddo
!
!  Set up splines
!
775 call setspl(splr(1,npote),splf(1,npote),nsplpt(npote),npote,rpot(npote))
  l55 = .true.
  lwordok = .true.
  word = words(1)(1:20)
  return
!
776 call setspl(splr(1,npote),splf(1,npote),nsplpt(npote),npote,rpot(npote))
  l1000 = .true.
  lwordok = .true.
  return
!***********************************
!  Central force model - harmonic  *
!***********************************
800 if (npote.eq.maxpot) then
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
        lfound=.true.
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
805 line = '  '
  read(iin,'(a)',end=808) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 805
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 808
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 805
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 805
  endif
!
!  If number of floats is greater than 6, assume that fitting flags have been left on line
!
  if (nfloat.gt.6) nfloat = nfloat - 4
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.6) then
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
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
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
  nptype(npote) = 49
  goto 805
808 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***********************************
!  Central force model - gaussian  *
!***********************************
810 if (npote.eq.maxpot) then
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
        lfound=.true.
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
815 line = '  '
  read(iin,'(a)',end=818) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 815
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 818
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 815
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
    n1 = int(floats(nfloat-4))
    n2 = int(floats(nfloat-3))
    n3 = int(floats(nfloat-2))
    n4 = int(floats(nfloat-1))
    n5 = int(floats(nfloat))
    nfloat = nfloat - 5
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
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 5
    endif
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 815
  endif
!
!  If number of floats is greater than 7, assume that fitting flags have been left on line
!
  if (nfloat.gt.7) nfloat = nfloat - 5
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.7) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    rpot2(npote)    = floats(6+nbeg)
    rpot(npote)     = floats(7+nbeg)
  elseif (nfloat.eq.6) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    rpot(npote)     = floats(6+nbeg)
    rpot2(npote)    = 0.0_dp
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
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
  nptype(npote) = 50
  goto 815
818 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!********************************
!  Central force model - power  *
!********************************
820 if (npote.eq.maxpot) then
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
        lfound=.true.
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
825 line = '  '
  read(iin,'(a)',end=828) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 825
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 828
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 825
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
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 825
  endif
!
!  If number of floats is greater than 6, assume that fitting flags have been left on line
!
  if (nfloat.gt.6) nfloat = nfloat - 4
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.6) then
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
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
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
  nptype(npote) = 51
  goto 825
828 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!********************************
!  Central force model - Fermi  *
!********************************
830 if (npote.eq.maxpot) then
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
        lfound=.true.
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
835 line = '  '
  read(iin,'(a)',end=838) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 835
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 838
    endif
  elseif (nfloat.gt.0) then
!
!  If this is a coefficient line then skip
!
    goto 835
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
    n1 = int(floats(nfloat-4))
    n2 = int(floats(nfloat-3))
    n3 = int(floats(nfloat-2))
    n4 = int(floats(nfloat-1))
    n5 = int(floats(nfloat))
    nfloat = nfloat - 5
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
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 2
      nfpot(nfit) = npote
      nfvar(nfit) = 5
    endif
  endif
!
!  Process symbols in input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym21,nvar2,itype2,sym22,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    npote = npote - 1
    nfit = nfit0
    goto 835
  endif
!
!  If number of floats is greater than 7, assume that fitting flags have been left on line
!
  if (nfloat.gt.7) nfloat = nfloat - 5
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.7) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    rpot2(npote)    = floats(6+nbeg)
    rpot(npote)     = floats(7+nbeg)
  elseif (nfloat.eq.6) then
    twopot(1,npote) = floats(1+nbeg)*units
    twopot(2,npote) = floats(2+nbeg)
    twopot(3,npote) = floats(3+nbeg)
    twopot(4,npote) = floats(4+nbeg)
    twopot(5,npote) = floats(5+nbeg)
    rpot(npote)     = floats(6+nbeg)
    rpot2(npote)    = 0.0_dp
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword22')
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
  nptype(npote) = 52
  goto 835
838 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!
  return
  end
