  subroutine genword(nru,word,lwordok,iline,line,l55,l1000,llibrary,lfflags,ncurr)
!
!  Processes input after keyword line looking for general input
!
!  nru is fortran read channel
!
!   5/00 Created from inword
!   6/01 Initialisation of line added for benefit of some compilers
!   6/01 Modification of VDW radii added
!  10/01 1-D accuracy parameters added
!   5/02 Minimum cell parameter added
!   5/02 Scaling of lower shift rename to avoid conflict
!   8/02 Files added for DLV
!   8/02 Flag for ASCII trajectory file added
!  10/02 Separate option for iseed added
!   1/03 Wolf sum parameters added
!   5/03 XML option added
!   5/03 Potential sites specification added
!   6/03 lmbfgs_order added
!   4/04 Output of pressure file added
!   4/04 Terse option added
!   9/04 Seed for random number generator forced to be negative
!  11/04 Sqrt pi accessed from constants
!   1/05 output sas option added
!   2/05 Potential interpolation option added 
!   4/05 cwolf option added
!   6/05 Fitting flags added for electronegativity input
!   6/05 llibrary flag added as input to genword
!   7/05 Input for Streitz and Mintmire parameters added
!   7/05 Input of number of table points added
!  11/05 Handling of "bin" sub-option in output option modified so that
!        it is only used as such if this is for frequency output
!  11/05 Voter taper option added to cutp
!   3/06 Output file added for oscillator strengths
!   3/06 Bug in adding of .trg to trjfile fixed
!   5/06 Reading of species masses accommodated as well as updating of
!        any previously set species masses
!   7/06 Checking added so that only masses for library species that
!        are relevant are added to list
!   8/06 Radius added to QEq parameter input - note this has been done
!        to try to maintain backwards compatibility
!   8/06 nru passed to linepro
!   8/06 Separate flag added for fitting flags
!   9/06 Literal symbol added to arguments for getpotsymbol1
!  10/06 CML mods added
!   1/07 Gasteiger options added
!   3/07 Radial force added
!   3/07 Gauss renamed to GULP_gauss
!   5/07 Exponential taper added
!   6/07 Option to control some units from the input file added
!  11/07 MDF taper added
!  11/07 Option to output geo file for ReaxFF added
!  12/07 Option to control the frequency of archive frame writes added
!  12/07 730 -> option to set second derivative size remove as it is redundant
!  12/07 Typo reference to qeqtol replaced by qeqscfcrit
!   4/08 Option to input spatial decomposition region size added
!   4/08 Option to input phondiff for phonon finite differences added
!  10/08 Option to prevent overwriting of dumpfiles added
!  10/08 Wolf/COSMIC options merged in
!  12/08 Module input renamed to gulpinput
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   2/09 Old xml calls removed
!   3/09 Anisotropic rcspatial suboption added
!   3/09 lfinitediff now set when finite option is present
!   5/09 finite difference intervals for property evaluation added
!  11/09 Bug in setting of mass for general element fixed
!  12/09 Handling of units modified for potsites input
!   3/10 Line converted to lower case before checking for end in title option
!   4/10 COSMO file type added
!   8/10 Interpretation of output option modified to allowed for numeric filenames
!  10/10 qbo file output added
!  11/10 Anisotropic pressure added
!   1/11 Force minimisation forced for anisotropic pressure case
!   3/11 Lammps potential file added
!   8/11 Plumed option added
!   3/12 Output of DCD files added
!   5/12 Logic of dump option corrected
!   6/12 Option to select the maths library added
!   6/12 File handling changed to allow for spaces in names
!   9/12 NMR parameters added for species
!  10/12 Modifications added for lammps file output
!  11/12 Modified Wolf sum of Fennell and Gezelter added
!  11/12 Copy of floatwords to word now restricted to size of word. 
!  12/12 q0 parameters added to electronegativity methods
!   5/13 Option to set the bond order for UFF added
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
!  Julian Gale, NRI, Curtin University, May 2013
!
  use cellmultipole
  use configurations
  use constants
  use control
  use current
  use dump
  use element
  use files
  use fitting
  use general
  use genetic,                only : iseed
  use gulpinput
  use gulp_cml,               only : lcml, lvcml, cmlfilename
  use iochannels
  use kspace,                 only : tweatpi
  use m_pdfneutron,           only : pdffiles
  use maths,                  only : leispack_eigensolve, ldivide_and_conquer
  use molecule
  use optimisation
  use parallel
  use plumed
  use polarise
  use potentialgrid
  use potentialinterpolation, only : nptsinterpolate, lpotlinterpolate
  use potentialsites
  use radial
  use shell
  use spatial,                only : rcspatial, lrcspatial_anisotropic
  use spatial,                only : rcspatialx, rcspatialy, rcspatialz
  use spatialbo,              only : rcspatialbo, lrcspatialBO_anisotropic
  use spatialbo,              only : rcspatialbox, rcspatialboy, rcspatialboz
  use species
  use terse
  use two
  use uffdata,                only : UFFbondorder
  use wolfcosmo
  implicit none
!
!  Passed variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  character(len=maxlinelength) :: linelc
  integer(i4)                  :: iline
  integer(i4)                  :: ncurr
  integer(i4)                  :: nru
  logical                      :: l55
  logical                      :: l1000
  logical                      :: lfflags
  logical                      :: llibrary
  logical                      :: lwordok
!
!  Local variables
!
  character(len=2)             :: cha
  character(len=2)             :: s1
  character(len=5)             :: sym1
  integer(i4)                  :: i
  integer(i4)                  :: iend
  integer(i4)                  :: ii
  integer(i4)                  :: inat
  integer(i4)                  :: ind
  integer(i4)                  :: istart
  integer(i4)                  :: istoan
  integer(i4)                  :: itype
  integer(i4)                  :: itype1
  integer(i4)                  :: itype2
  integer(i4)                  :: iwc
  integer(i4)                  :: j
  integer(i4)                  :: mcal
  integer(i4)                  :: n
  integer(i4)                  :: n1
  integer(i4)                  :: n2
  integer(i4)                  :: n3
  integer(i4)                  :: n4
  integer(i4)                  :: na
  integer(i4)                  :: nbeg
  integer(i4)                  :: nextraword
  integer(i4)                  :: never
  integer(i4)                  :: nflo
  integer(i4)                  :: nfloatused
  integer(i4)                  :: nonzero
  integer(i4)                  :: ns
  integer(i4)                  :: ntot
  integer(i4)                  :: nvar1
  integer(i4)                  :: nvar2
  integer(i4)                  :: nwo
  logical                      :: latmassin
  logical                      :: lcart
  logical                      :: lever
  logical                      :: lfound
  logical                      :: lin
  logical                      :: llocalmovie
  logical                      :: lnodot
  logical                      :: lout
  logical                      :: lsymbol
  logical                      :: lset
  logical                      :: lvalid
  real(dp)                     :: derfc
  real(dp)                     :: rd5
  real(dp)                     :: rmat(3,4)
  real(dp)                     :: ta
  real(dp)                     :: ta2
  real(dp)                     :: taperrange
  real(dp)                     :: tb
  real(dp)                     :: tb2
  real(dp)                     :: units
  real(dp)                     :: val
!
!  Options
!
  if (index(word,'accu').eq.1) goto 100
  if (index(word,'qele').eq.1) goto 110
  if (index(word,'cmm').eq.1)  goto 120
  if (index(word,'elec').eq.1) goto 130
  if (index(word,'name').eq.1) goto 140
  if (index(word,'cutb').eq.1) goto 150
  if (index(word,'cutd').eq.1) goto 150
  if (index(word,'cutp').eq.1) goto 160
  if (index(word,'site').eq.1) goto 170
  if (index(word,'seed').eq.1) goto 180
  if (index(word,'ewal').eq.1) goto 190
  if (index(word,'slow').eq.1) goto 200
  if (index(word,'pola').eq.1) goto 210
  if (index(word,'delt').eq.1) goto 220
  if (index(word,'maxc').eq.1) goto 230
  if (index(word,'step').eq.1) goto 240
  if (index(word,'xtol').eq.1) goto 250
  if (index(word,'swit').eq.1) goto 260
  if (index(word,'potg').eq.1) goto 270
  if (index(word,'time ').eq.1) goto 280
  if (index(word,'qeqt').eq.1) goto 290
  if (index(word,'qeqi').eq.1) goto 300
  if (index(word,'qeqr').eq.1) goto 310
  if (index(word,'titl').eq.1) goto 320
  if (index(word,'rtol').eq.1) goto 330
  if (index(word,'cdum').eq.1) goto 340
  if (index(word,'dump').eq.1) goto 350
  if (index(word,'fini').eq.1) goto 360
  if (index(word,'cuts').eq.1) goto 370
  if (index(word,'minc').eq.1) goto 380
  if (index(word,'gtol').eq.1) goto 390
  if (index(word,'gmax').eq.1) goto 400
  if (index(word,'ters').eq.1) goto 410
  if (index(word,'ftol').eq.1) goto 420
  if (index(word,'prin').eq.1) goto 430
  if (index(word,'elem').eq.1) goto 440
  if (index(word,'mass').eq.1) goto 445
  if (index(word,'ioni').eq.1) goto 445
  if (index(word,'cova').eq.1) goto 445
  if (index(word,'vdw').eq.1)  goto 445
  if (index(word,'nmr').eq.1)  goto 445
  if (index(word,'symb').eq.1) goto 445
  if (index(word,'bbar').eq.1) goto 445
  if (index(word,'sigi').eq.1) goto 445
  if (index(word,'smel').eq.1) goto 490
  if (index(word,'maxl').eq.1) goto 500
  if (index(word,'outp').eq.1) goto 510
  if (index(word,'rspe').eq.1) goto 520
  if (index(word,'nadd').eq.1) goto 530
  if (index(word,'gdcr').eq.1) goto 540
  if (index(word,'nobo').eq.1) goto 550
  if (index(word,'upda').eq.1) goto 560
  if (index(word,'lbfg').eq.1) goto 570
  if (index(word,'line').eq.1) goto 580
  if (index(word,'pots').eq.1) goto 590
  if (index(word,'pote').eq.1) goto 600
  if (index(word,'delf').eq.1) goto 610
  if (index(word,'marv').eq.1) goto 620
  if (index(word,'qmmm').eq.1) goto 630
  if (index(word,'unit').eq.1) goto 640
  if (index(word,'math').eq.1) goto 650
  if (index(word,'stre').eq.1) goto 680
  if (index(word,'maxi').eq.1) goto 690
  if (index(word,'pres').eq.1) goto 700
  if (index(word,'qwol').eq.1) goto 710
  if (index(word,'radi').eq.1) goto 720
  if (index(word,'rcsp').eq.1) goto 730
  if (index(word,'gastt').eq.1) goto 740
  if (index(word,'gasti').eq.1) goto 750
  if (index(word,'pfin').eq.1) goto 760
  if (index(word,'qwol').eq.1) goto 770
  if (index(word,'cwol').eq.1) goto 780
  if (index(word,'sfin').eq.1) goto 790
  if (index(word,'anis').eq.1) goto 800
  if (index(word,'plum').eq.1) goto 810
  if (index(word,'uff_b').eq.1) goto 820
  return
!*************************************
!  Accuracy factor for lattice sums  *
!*************************************
100 if (nfloat.gt.0) then
    accuracy = floats(1)
    if (nfloat.ge.2) then
      nemorder = nint(floats(2))
    endif
    if (nfloat.ge.3) then
      nmaxcells = nint(floats(3))
    endif
  else
    read(nru,*,err=99) accuracy
    iline = iline + 1
  endif
  lwordok = .true.
  return
!**************************
!  Cell multipole method  *
!**************************
120 icmm = 3
  if (nfloat.gt.0) then
    rbox = floats(1)
  endif
  if (nword.gt.1) then
    do i = 2,nword
      if (index(words(i),'mo').eq.1) icmm = 1
      if (index(words(i),'di').eq.1) icmm = 2
      if (index(words(i),'qu').eq.1) icmm = 3
      if (index(words(i),'oc').eq.1) icmm = 4
    enddo
  endif
  lwordok = .true.
  return
!*********************************
!  Electronegativity parameters  *
!*********************************
110 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) units = autoangs
  endif
!
!  Start of input loop
!
115 line = '  '
  read(nru,'(a)',end=1000) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 115
!
!  Check for old fashion specification of number of atoms
!
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      return
    endif
  endif
!
!  Symbols used in input
!
  if (nword.gt.0) then
    call ltont(words(1),inat,itype)
    leemparaltered(inat) = .true.
    qeqchi(inat) = floats(1)
    if (nfloat.gt.1) then
      qeqmu(inat) = floats(2)
      if (nfloat.gt.2.and.nfloat.ne.5) then
        qeqrad(inat) = abs(floats(3))
        if (nfloat.gt.3) then
          qeqq0(inat) = floats(4)
          nfloatused = 4
        else
          nfloatused = 3
        endif
      else
        nfloatused = 2
      endif
    else
      nfloatused = 1
    endif
  else
!
!  Numeric input
!
    inat = int(floats(1))
    leemparaltered(inat) = .true.
    qeqchi(inat) = floats(2)
    if (nfloat.gt.2) then
      qeqmu(inat) = floats(3)
      if (nfloat.gt.3.and.nfloat.ne.6) then
        qeqrad(inat) = abs(floats(5))
        if (nfloat.gt.4) then
          qeqq0(inat) = floats(5)
          nfloatused = 5
        else
          nfloatused = 4
        endif
      else
        nfloatused = 3
      endif
    else
      nfloatused = 2
    endif
  endif
!     
!  Fitting flags
!       
  if (lfit.and.lfflags) then
    if (nfloat.ge.nfloatused+3) then
      n1 = int(floats(nfloatused+1))
      n2 = int(floats(nfloatused+2))
      n3 = int(floats(nfloatused+3))
    elseif (nfloat.ge.nfloatused+2) then
      n1 = int(floats(nfloatused+1))
      n2 = int(floats(nfloatused+2))
      n3 = 0
    elseif (nfloat.eq.nfloatused+1) then
      n1 = int(floats(nfloatused+1))
      n2 = 0
      n3 = 0
    else
      n1 = 0
      n2 = 0
      n3 = 0
    endif
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 13
      nfvar(nfit) = inat
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 14
      nfvar(nfit) = inat
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 19
      nfvar(nfit) = inat
    endif
  endif
!
  goto 115
!*********************************
!  Electronegativity parameters  *
!*********************************
130 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) units = autoangs
  endif
!
!  Start of input loop
!
135 line = '  '
  read(nru,'(a)',end=1000) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 135
!
!  Check for old fashion specification of number of atoms
!
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      return
    endif
  endif
!
!  Symbols used in input
!
  if (nword.gt.0) then
    call ltont(words(1),inat,itype)
    leemparaltered(inat) = .true.
    chi(inat) = floats(1)
    if (nfloat.gt.1) then
      rmu(inat) = floats(2)
      if (nfloat.gt.2) then
        q0(inat) = floats(3)
        nfloatused = 3
      else
        nfloatused = 2
      endif
    else
      nfloatused = 1
    endif
  else
!
!  Numeric input
!
    inat = int(floats(1))
    leemparaltered(inat) = .true.
    chi(inat) = floats(2)
    if (nfloat.gt.2) then
      rmu(inat) = floats(3)
      if (nfloat.gt.3) then
        q0(inat) = floats(4)
        nfloatused = 4
      else
        nfloatused = 3
      endif
    else
      nfloatused = 3
    endif
  endif
!     
!  Fitting flags
!        
  if (lfit.and.lfflags) then
    if (nfloat.ge.nfloatused+2) then
      n1 = int(floats(nfloatused+1))
      n2 = int(floats(nfloatused+2))
    elseif (nfloat.eq.nfloatused+1) then
      n1 = int(floats(nfloatused+1))
      n2 = 0
    else
      n1 = 0
      n2 = 0
    endif
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 11
      nfvar(nfit) = inat
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 12
      nfvar(nfit) = inat
    endif
  endif
  goto 135
!*******************
!  Structure name  *
!*******************
140 if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  if (nword.eq.1) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    names(ncfg+1) = words(1)
  else
    names(ncfg+1) = words(2)
  endif
  lwordok = .true.
  return
!*************************************
!  Cutoff for distance calculations  *
!*************************************
150 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) units = autoangs
  endif
  if (nfloat.gt.0) then
    cutb = floats(1)*units
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    cutb = floats(1)
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'au').eq.1) units = autoangs
    endif
    cutb = cutb*units
  endif
  lwordok = .true.
  return
!**************************************************
!  Cutoff for interatomic potential calculations  *
!**************************************************
160 units = 1.0_dp
  if (nword.gt.1) then
    do ii = 2,nword
      call stolc(words(ii),maxword)
      if (index(words(ii),'au').eq.1) then
        units = autoangs
      elseif (index(words(ii),'po').eq.1) then
        tapertype = 1
      elseif (index(words(ii),'co').eq.1) then
        tapertype = 2
      elseif (index(words(ii),'vo').eq.1) then
        tapertype = 3
      elseif (index(words(ii),'ex').eq.1) then
        tapertype = 4
      elseif (index(words(ii),'md').eq.1) then
        tapertype = 5
      endif
    enddo
  endif
  if (tapertype.eq.3) then
!
!  Voter style taper
!
    if (nfloat.gt.0) then
      cutp = floats(1)*units
      if (nfloat.gt.1) then
        taperm = abs(floats(2))
      endif
    else
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
      cutp = floats(1)
      if (nfloat.gt.1) then
        taperm = abs(floats(2))
      endif
      if (nword.ge.1) then
        call stolc(words(1),maxword)
        if (index(words(1),'au').eq.1) units = autoangs
      endif
      cutp = cutp*units
    endif
    if (taperm.lt.1.0d-12) then
      call outerror('Voter taper m cannot be zero',iline)
      call stopnow('genword')
    endif
    taperrange = cutp
  elseif (tapertype.eq.4) then
!
!  Exponential style taper
!
    if (nfloat.gt.0) then
      cutp = floats(1)*units
    else
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
      cutp = floats(1)
      if (nword.ge.1) then
        call stolc(words(1),maxword)
        if (index(words(1),'au').eq.1) units = autoangs
      endif
      cutp = cutp*units
    endif
    taperrange = cutp
  else
    if (nfloat.gt.0) then
      cutp = floats(1)*units
      if (nfloat.gt.1) then
        taperrange = abs(floats(2))*units
      endif
    else
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
      cutp = floats(1)
      if (nfloat.gt.1) then
        taperrange = abs(floats(2))
      endif
      if (nword.ge.1) then
        call stolc(words(1),maxword)
        if (index(words(1),'au').eq.1) units = autoangs
      endif
      cutp = cutp*units
      taperrange = taperrange*units
    endif
  endif
!
!  Set up taper constants
!
  if (taperrange.gt.1.0d-2) then
    tapermax = cutp
    tapermin = cutp - taperrange
    tb = cutp
    tb2 = tb*tb
    ta = tb - taperrange
    ta2 = ta*ta
    rd5 = 1.0_dp/(taperrange**5.0_dp)
    pts0 = (10.0_dp*ta2*tb+(tb-5.0_dp*ta)*tb2)*tb2*rd5
    pts1 = - 30.0_dp*ta2*tb2*rd5
    pts2 = 30.0_dp*(ta2*tb+ta*tb2)*rd5
    pts3 = - 10.0_dp*(ta2+4.0_dp*ta*tb+tb2)*rd5
    pts4 = 15.0_dp*(ta+tb)*rd5
    pts5 = - 6.0_dp*rd5
  endif
  lwordok = .true.
  return
!*********************
!  Site list option  *
!*********************
170 if (ioproc) then
    write(ioout,'(''  **** Site list option has been withdrawn ****'')')
  endif
  lwordok = .true.
  return
!*************************************
!  Seed for random number generator  *
!*************************************
180 if (nfloat.ge.1) then  
    iseed = nint(floats(1))
  else  
    read(nru,*,err=99) iseed
    iline = iline + 1
  endif
  iseed = - abs(iseed)
  lwordok = .true.
  return
!**************************************************
!  Target real space radius for Ewald/Parry sums  *
!**************************************************
190 if (nfloat.ge.1) then  
    targetrradmax = abs(floats(1))
  else  
    read(nru,*,err=99) targetrradmax
    iline = iline + 1
  endif
  lwordok = .true.
  return
!*****************************************************
!  Change default value of scaling for lower option  *
!*****************************************************
200 if (nfloat.gt.0) then
    lowerscale = floats(1)
  else
    read(nru,*,err=99) lowerscale
    iline = iline + 1
  endif
  lwordok = .true.
  return
!************************
!  Polarisability data  *
!************************
210 lpolar = .true.
215 line = '  '
  read(nru,'(a)',end=218) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 215
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 215
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
        goto 218
      endif
    endif
  endif
  npolspec = npolspec + 1
  if (npolspec.gt.maxspec) then
    maxspec = npolspec + 20
    call changemaxspec
  endif
  if (nword.eq.0) then
    inat = int(floats(1))
    if (inat.gt.100) inat = inat - 100
    natpolspec(npolspec) = inat
    ntyppolspec(npolspec) = 0
    dpolspec(npolspec) = floats(2)
    if (nfloat.gt.2) then
      qpolspec(npolspec) = floats(3)
    endif
  elseif (nword.ge.1) then
    call ltont(words(1),inat,itype)
    natpolspec(npolspec) = inat
    ntyppolspec(npolspec) = itype
    dpolspec(npolspec) = floats(1)
    if (nfloat.gt.3) then
      qpolspec(npolspec) = floats(4)
    endif
  endif
  goto 215
218 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**********************************
!  Change default value of delta  *
!**********************************
220 if (nfloat.gt.0) then
    delta = floats(1)
  else
    read(nru,*,err=99) delta
    iline = iline + 1
  endif
!
  delta = abs(delta)
  if (delta.ge.1.0_dp) delta = 10.0**(-delta)
  lwordok = .true.
  return
!***********************************
!  Change default value of maxcal  *
!***********************************
230 if (nword.gt.1) then
    if (index(words(2),'opt').eq.1) then
!
!  Set only optimisation maxcal
!
      if (nfloat.gt.0) then
        maxcal = int(floats(1))
      else
        read(nru,*,err=99) maxcal
        iline = iline + 1
      endif
    elseif (index(words(2),'fit').eq.1) then
!
!  Set only fit maxcal
!
      if (nfloat.gt.0) then
        maxfcal = int(floats(1))
      else
        read(nru,*,err=99) maxfcal
        iline = iline + 1
      endif
    else
!
!  Set both
!
      if (nfloat.gt.0) then
        mcal = int(floats(1))
      else
        read(nru,*,err=99) mcal
        iline = iline + 1
      endif
      if (lfit) maxfcal = mcal
      if (lopt) maxcal = mcal
    endif
  else
!
!  Set both
!
    if (nfloat.gt.0) then
      maxcal = int(floats(1))
    else
      read(nru,*,err=99) maxcal
      iline = iline + 1
    endif
    maxfcal = maxcal
  endif
  lwordok = .true.
  return
!************************************
!  Change default value of stepmax  *
!************************************
240 if (nword.gt.1) then
    if (index(words(2),'opt').eq.1) then
!
!  Set only optimisation stepmax
!
      if (nfloat.gt.0) then
        stepmax = floats(1)
      else
        read(nru,*,err=99) stepmax
        iline = iline + 1
      endif
    elseif (index(words(2),'fit').eq.1) then
!
!  Set only fit stepmax
!
      if (nfloat.gt.0) then
        fstepmx = floats(1)
      else
        read(nru,*,err=99) fstepmx
        iline = iline + 1
      endif
    else
!
!  Set according to job type
!
      if (lopt) then
        if (nfloat.gt.0) then
          stepmax = floats(1)
        else
          read(nru,*,err=99) stepmax
          iline = iline + 1
        endif
        if (lfit) fstepmx = stepmax
      elseif (lfit) then
        if (nfloat.gt.0) then
          fstepmx = floats(1)
        else
          read(nru,*,err=99) fstepmx
          iline = iline + 1
        endif
      endif
    endif
  else
!
!  Set according to job type
!
    if (lopt) then
      if (nfloat.gt.0) then
        stepmax = floats(1)
      else
        read(nru,*,err=99) stepmax
        iline = iline + 1
      endif
      if (lfit) fstepmx = stepmax
    elseif (lfit) then
      if (nfloat.gt.0) then
        fstepmx = floats(1)
      else
        read(nru,*,err=99) fstepmx
        iline = iline + 1
      endif
    endif
  endif
  lwordok = .true.
  return
!*********************************
!  Change default value of xtol  *
!*********************************
250 if (nword.gt.1) then
    if (index(words(2),'opt').eq.1) then
!
!  Set only optimisation xtol
!
      if (nfloat.gt.0) then
        xtol = floats(1)
      else
        read(nru,*,err=99) xtol
        iline = iline + 1
      endif
    elseif (index(words(2),'fit').eq.1) then
!
!  Set only fit xtol
!
      if (nfloat.gt.0) then
        fxtol = floats(1)
      else
        read(nru,*,err=99) fxtol
        iline = iline + 1
      endif
    else
!
!  Set according to job type
!
      if (lopt) then
        if (nfloat.gt.0) then
          xtol = floats(1)
        else
          read(nru,*,err=99)xtol
          iline = iline + 1
        endif
        if (lfit) fxtol = xtol
      elseif (lfit) then
        if (nfloat.gt.0) then
          fxtol = floats(1)
        else
          read(nru,*,err=99) fxtol
          iline = iline + 1
        endif
      endif
    endif
  else
!
!  Set according to job type
!
    if (lopt) then
      if (nfloat.gt.0) then
        xtol = floats(1)
      else
        read(nru,*,err=99)xtol
        iline = iline + 1
      endif
      if (lfit) fxtol = xtol
    elseif (lfit) then
      if (nfloat.gt.0) then
        fxtol = floats(1)
      else
        read(nru,*,err=99) fxtol
        iline = iline + 1
      endif
    endif
  endif
  xtol = abs(xtol)
  if (xtol.ge.1) xtol = 10.0**(-xtol)
  fxtol = abs(fxtol)
  if (fxtol.ge.1) fxtol = 10.0**(-fxtol)
  lwordok = .true.
  return
!*********************
!  Switch minimiser  *
!*********************
!
!  lminch    = logical controlling whether change is allowed
!  mintype   = index for new minimiser
!              1 => exact hessian + BFGS
!              2 => exact hessian + RFO
!              3 => unit hessian + BFGS
!              4 => numerical (numerical diagonal)
!              5 => conjugate gradients
!              6 => limited memory BFGS
!  minchcrit = indicator to change criteria
!              1 => no. of cycles
!              2 => gradient norm
!  chcrit    = change criteria (no. of cycles / gnorm)
!
260 if (nfloat+nword.eq.1) then
    line = '  '
    read(nru,'(a)',end=1000) line
    iline = iline + 1
    call linepro(nru,line,iline)
    istart = 1
  else
    istart = 2
  endif
  minchcrit = 2
  mintype = 0
  lminch = .true.
!
!  Find option words
!
  do i = istart,nword
    if (index(words(i),'bfgs').eq.1) then
      mintype = 1
    elseif (index(words(i),'rfo').eq.1) then
      mintype = 2
    elseif (index(words(i),'unit').eq.1) then
      mintype = 3
    elseif (index(words(i),'nume').eq.1) then
      mintype = 4
    elseif (index(words(i),'conj').eq.1) then
      mintype = 5
    elseif (index(words(i),'lbfg').eq.1) then
      mintype = 6
    elseif (index(words(i),'cyc').eq.1) then
      minchcrit = 1
    endif
  enddo
  if (mintype.eq.0) then
    call outerror('no new minimiser type given in switch option',iline)
    call stopnow('genword')
  endif
  if (nfloat.eq.0) then
    call outerror('no change criterion given in switch option',iline)
    call stopnow('genword')
  endif
  chcrit = floats(1)
  lwordok = .true.
  return
!************************************
!  Potential on a grid calculation  *
!************************************
270 if (nfloat.eq.0) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
  endif
  if (nfloat.ge.9) then
    xminpg(ncurr) = floats(1)
    xmaxpg(ncurr) = floats(2)
    yminpg(ncurr) = floats(3)
    ymaxpg(ncurr) = floats(4)
    zminpg(ncurr) = floats(5)
    zmaxpg(ncurr) = floats(6)
    nxpg(ncurr) = nint(floats(7))
    nypg(ncurr) = nint(floats(8))
    nzpg(ncurr) = nint(floats(9))
  elseif (nfloat.ge.3) then
    nxpg(ncurr) = nint(floats(1))
    nypg(ncurr) = nint(floats(2))
    nzpg(ncurr) = nint(floats(3))
  else
    call outerror('insufficient input for potgrid option',iline)
    call stopnow('genword')
  endif
  if (nxpg(ncurr).eq.0.or.nypg(ncurr).eq.0.or.nzpg(ncurr).eq.0) then
    call outerror('no. of grid points in zero for potgrid',iline)
    call stopnow('genword')
  endif
  lwordok = .true.
  return
!*******************
!  Set time limit  *
!*******************
280 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'min').eq.1) then
      units = 60.0_dp
    elseif (index(words(2),'hou').eq.1) then
      units = 3600.0_dp
    endif
  endif
  if (nfloat.gt.0) then
    timmax = floats(1)*units
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    timmax = floats(1)
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'min').eq.1) then
        units = 60.0_dp
      elseif (index(words(1),'hou').eq.1) then
        units = 3600.0_dp
      endif
    endif
    timmax = timmax*units
  endif
  lwordok = .true.
  return
!******************************
!  QEq convergance tolerance  *
!******************************
290 if (nfloat.gt.0) then
    qeqscfcrit = floats(1)
  else
    read(nru,*,err=99) qeqscfcrit
    iline = iline + 1
  endif
  qeqscfcrit = max(abs(qeqscfcrit),1.0d-8)
  lwordok = .true.
  return
!*************************************
!  QEq maximum number of iterations  *
!*************************************
300 if (nfloat.gt.0) then
    nqeqitermax = nint(floats(1))
  else
    read(nru,*,err=99) nqeqitermax
    iline = iline + 1
  endif
  nqeqitermax = max(1,abs(nqeqitermax))
  lwordok = .true.
  return
!************************
!  QEq integral radius  *
!************************
310 if (nfloat.gt.0) then
    rqeq = floats(1)
  else
    read(nru,*,err=99) rqeq
    iline = iline + 1
  endif
  rqeq = abs(rqeq)
  lwordok = .true.
  return
!******************
!  Read in title  *
!******************
320 if (nfloat.gt.0) then
    ntitle = nint(floats(1))
    if (ntitle.gt.maxtitle) then
      maxtitle = ntitle + 5
      call changemaxtitle
    endif
    do i = 1,ntitle
      read(nru,'(a)') titleword(i)
      if (ioproc) then
        write(ioout,'(''* '',a76,'' *'')') titleword(i)(1:76)
      endif
      iline = iline + 1
    enddo
  else
325 read(nru,'(a)',end=1000) line
    iline = iline + 1
!
!  Convert line to lower case before checking for "end"
!
    linelc = line
    call stolc(linelc,maxlinelength)
    if (index(linelc,'end').eq.1) goto 328
!
    ntitle = ntitle + 1
    if (ntitle.gt.maxtitle) then
      maxtitle = ntitle + 5
      call changemaxtitle
    endif
    titleword(ntitle) = line
    goto 325
  endif
328 continue
  lwordok = .true.
  return
!**************************
!  Bond length tolerance  *
!  Default = 1.2          *
!**************************
330 if (nfloat.gt.0) then
    rtol = floats(1)
  else
    read(nru,*,err=99) rtol
    iline = iline + 1
  endif
  lwordok = .true.
  return
!**********************************
!  Fortran channel for dump file  *
!  Default = 12                   *
!**********************************
340 if (nfloat.gt.0) then
    idump = int(floats(1))
  else
    read(nru,*,err=99) idump
    iline = iline + 1
  endif
  lwordok = .true.
  return
!******************************
!  Write out dumpfile at end  *
!******************************
350 idump = 12
  ntot = nfloat + nword
  if (ntot.gt.0) then
    nwo = 1
    nflo = 0
    never = -1
    j = 2
    lever = .false.
    do while (j.le.ntot)
      if (nlorder(j).eq.1) then
        nwo = nwo + 1
        if (index(words(nwo),'ever').eq.1) then
          lever = .true.
          if ((nfloat-nflo).gt.0) then
            nflo = nflo + 1
            ncycd = nint(floats(nflo))
            never = nflo
          else
            ncycd = 1
          endif
        elseif (index(words(nwo),'duri').eq.1) then
          lever = .true.
          ncycd = 1
        elseif (index(words(nwo),'cart').eq.1) then
          ldumpcart = .true.
        elseif (index(words(nwo),'noov').eq.1) then
          ldumpnooverwrite = .true.
        elseif (index(words(nwo),'conn').eq.1) then
          ldumpconnectivity = .true.
        else
          dfile = words(nwo)
        endif
      else
        if (nflo.eq.never) then
          never = -1
        else
          nflo = nflo + 1
          idump = nint(floats(nflo))
        endif
      endif
      j = j + 1
    enddo
  endif
!
!  If not 'dump every' turn off ldumpnooverwrite option
!
  if (.not.lever.and.ncycd.ne.1) ldumpnooverwrite = .false.
!
  lwordok = .true.
  return
!*******************************************************************
!  Finite difference interval for numerical free energy gradients  *
!*******************************************************************
360 lfinitediff = .true.
  if (nfloat.gt.0) then
    findiff = abs(floats(1))
  else
    findiff = 1.0d-5
  endif
  lwordok = .true.
  return
!**********************
!  Core-shell cutoff  *
!**********************
370 units = 1.0_dp
  if (nword.gt.1) then
    if (index(words(2),'au').eq.1) then
      units=autoangs
    endif
  endif
  if (nfloat.gt.0) then
    cuts = abs(floats(1))*units
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    cuts = abs(floats(1))
    if (nword.ge.1) then
      if (index(words(1),'au').eq.1) then
        units = autoangs
      endif
    endif
    cuts = cuts*units
  endif
  cuts = max(cuts,1.0d-8)
  lwordok = .true.
  return
!***************************
!  Minimum cell parameter  *
!***************************
380 units = 1.0_dp
  if (nword.gt.1) then
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.gt.0) then
    cellmin = abs(floats(1))*units
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    cellmin = abs(floats(1))
    if (nword.ge.1) then
      if (index(words(1),'au').eq.1) then
        units = autoangs
      endif
    endif
    cellmin = cellmin*units
  endif
  cellmin = max(cellmin,1.0d-8)
  lwordok = .true.
  return
!*********************************
!  Change default value of gtol  *
!*********************************
390 if (nword.gt.1) then
    if (index(words(2),'opt').eq.1) then
!
!  Set only optimisation gtol
!
      if (nfloat.gt.0) then
        gtol = floats(1)
      else
        read(nru,*,err=99) gtol
        iline = iline + 1
      endif
    elseif (index(words(2),'fit').eq.1) then
!
!  Set only fit gtol
!
      if (nfloat.gt.0) then
        fgtol = floats(1)
      else
        read(nru,*,err=99) fgtol
        iline = iline + 1
      endif
    else
!
!  Set according to job type
!
      if (lopt) then
        if (nfloat.gt.0) then
          gtol = floats(1)
        else
          read(nru,*,err=99) gtol
          iline = iline + 1
        endif
        if (lfit) fgtol = gtol
      elseif (lfit) then
        if (nfloat.gt.0) then
          fgtol = floats(1)
        else
          read(nru,*,err=99)fgtol
          iline = iline + 1
        endif
      endif
    endif
  else
!
!  Set according to job type
!
    if (lopt) then
      if (nfloat.gt.0) then
        gtol = floats(1)
      else
        read(nru,*,err=99) gtol
        iline = iline + 1
      endif
      if (lfit) fgtol = gtol
    elseif (lfit) then
      if (nfloat.gt.0) then
        fgtol = floats(1)
      else
        read(nru,*,err=99) fgtol
        iline = iline + 1
      endif
    endif
  endif
  gtol = abs(gtol)
  if (gtol.ge.1.0) gtol = 10.0**(-gtol)
  fgtol = abs(fgtol)
  if (fgtol.ge.1.0) fgtol = 10.0**(-fgtol)
  lwordok = .true.
  return
!*********************************
!  Change default value of gmax  *
!*********************************
400 if (nword.gt.1) then
    if (index(words(2),'opt').eq.1) then
!
!  Set only optimisation grmax
!
      if (nfloat.gt.0) then
        grmax = floats(1)
      else
        read(nru,*,err=99) grmax
        iline = iline + 1
      endif
    elseif (index(words(2),'fit').eq.1) then
!
!  Set only fit gmax
!
      if (nfloat.gt.0) then
        fgmax = floats(1)
      else
        read(nru,*,err=99) fgmax
        iline = iline + 1
      endif
    else
!
!  Set according to job type
!
      if (lopt) then
        if (nfloat.gt.0) then
          grmax = floats(1)
        else
          read(nru,*,err=99) grmax
          iline = iline + 1
        endif
        if (lfit) fgmax = grmax
      elseif (lfit) then
        if (nfloat.gt.0) then
          fgmax = floats(1)
        else
          read(nru,*,err=99) fgmax
          iline = iline + 1
        endif
      endif
    endif
  else
!
!  Set according to job type
!
    if (lopt) then
      if (nfloat.gt.0) then
        grmax = floats(1)
      else
        read(nru,*,err=99) grmax
        iline = iline + 1
      endif
      if (lfit) fgmax = grmax
    elseif (lfit) then
      if (nfloat.gt.0) then
        fgmax = floats(1)
      else
        read(nru,*,err=99) fgmax
        iline = iline + 1
      endif
    endif
  endif
  grmax = abs(grmax)
  if (grmax.ge.1.0) grmax = 10.0**(-grmax)
  fgmax = abs(fgmax)
  if (fgmax.ge.1.0) fgmax = 10.0**(-fgmax)
  lwordok = .true.
  return
!******************************************
!  Set options to make output more terse  *
!******************************************
410 if (nword.gt.1) then
    lin = .true.
    lout = .true.
    do n = 2,nword
      if (index(words(n),'in ').eq.1) then
        lin = .true.
        lout = .false.
      elseif (index(words(n),'out').eq.1) then
        lin = .false.
        lout = .true.
      elseif (index(words(n),'ino').eq.1) then
        lin = .true.
        lout = .true.
      elseif (index(words(n),'cell').eq.1) then
        if (lin.and.lout) then
          lterseincell = .true.
          lterseoutcell = .true.
        elseif (lin) then
          lterseincell = .true.
        elseif (lout) then
          lterseoutcell = .true.
        endif
      elseif (index(words(n),'cord').eq.1) then
        if (lin.and.lout) then
          lterseincoords = .true.
          lterseoutcoords = .true.
        elseif (lin) then
          lterseincoords = .true.
        elseif (lout) then
          lterseoutcoords = .true.
        endif
      elseif (index(words(n),'stru').eq.1) then
        if (lin.and.lout) then
          lterseincell = .true.
          lterseincoords = .true.
          lterseoutcell = .true.
          lterseoutcoords = .true.
        elseif (lin) then
          lterseincell = .true.
          lterseincoords = .true.
        elseif (lout) then
          lterseoutcell = .true.
          lterseoutcoords = .true.
        endif
      elseif (index(words(n),'pot').eq.1) then
        ltersepotentials = .true.
      elseif (index(words(n),'der').eq.1) then
        ltersederivs = .true.
      endif
    enddo
  endif
  lwordok = .true.
  return
!**************************************************************
!  Change default value of ftol                               *
!  Controls accuracy in energy during optimisation with BFGS  *
!**************************************************************
420 if (nword.gt.1) then
    if (index(words(2),'opt').eq.1) then
!
!  Set only optimisation ftol
!
      if (nfloat.gt.0) then
        ftol = floats(1)
      else
        read(nru,*,err=99) ftol
        iline = iline + 1
      endif
    elseif (index(words(2),'fit').eq.1) then
!
!  Set only fit ftol
!
      if (nfloat.gt.0) then
        fftol = floats(1)
      else
        read(nru,*,err=99) fftol
        iline = iline + 1
      endif
    else
!
!  Set according to job type
!
      if (lopt) then
        if (nfloat.gt.0) then
          ftol = floats(1)
        else
          read(nru,*,err=99) ftol
          iline = iline + 1
        endif
        if (lfit) fftol = ftol
      elseif (lfit) then
        if (nfloat.gt.0) then
          fftol = floats(1)
        else
          read(nru,*,err=99) fftol
          iline = iline + 1
        endif
      endif
    endif
  else
!
!  Set according to job type
!
    if (lopt) then
      if (nfloat.gt.0) then
        ftol = floats(1)
      else
        read(nru,*,err=99) ftol
        iline = iline + 1
      endif
      if (lfit) fftol = ftol
    elseif (lfit) then
      if (nfloat.gt.0) then
        fftol = floats(1)
      else
        read(nru,*,err=99) fftol
        iline = iline + 1
      endif
    endif
  endif
  ftol = abs(ftol)
  if (ftol.ge.1.0) ftol = 10.0**(-ftol)
  fftol = abs(fftol)
  if (fftol.ge.1.0) fftol = 10.0**(-fftol)
  lwordok = .true.
  return
!************************************************************
!  Output current parameters every ncycp cycles of fitting  *
!************************************************************
430 if (nfloat.gt.0) then
    ncycp = int(floats(1))
  else
    read(nru,*,err=99) ncycp
    iline = iline + 1
  endif
  lwordok = .true.
  return
!************************
!  Change element data  *
!************************
440 line = '  '
  read(nru,'(a)',err=99) line
  iline = iline + 1
  if (index(line,'#').eq.1) goto 440
  call linepro(nru,line,iline)
445 word = words(1)(1:20)
  latmassin = .true.
  if (index(word,'mass').eq.1) then
    call getpotsymbol1(iline,llibrary,inat,itype,sym1,1_i4,nbeg,lvalid)
    if (lvalid) then
      if (itype.eq.0) then
!
!  Standard atomic mass
!
        if (nfloat.ge.nbeg+1) then
          val = floats(nbeg+1)
        else
          read(nru,*,err=99) val
          iline = iline + 1
        endif
      else
!
!  Species specific mass
!
        latmassin = .false.
!
!  Find species
!
        lfound = .false.
        ns = 0
        do while (ns.lt.nspec.and..not.lfound)
          ns = ns + 1
          if (inat.eq.natspec(ns).and.itype.eq.ntypspec(ns)) then
            lfound = .true.
          endif
        enddo
        if (lfound) then
          massspec(ns) = floats(1)
          lmassinspec(ns) = .true.
        else
          nspec = nspec + 1
          if (nspec.gt.maxspec) then
            maxspec = nspec + 20 
            call changemaxspec
          endif
          linspec(nspec) = .true.
          natspec(nspec) = inat
          ntypspec(nspec) = itype
          massspec(nspec) = floats(1)
          lmassinspec(nspec) = .true.
          qlspec(nspec) = 0.0_dp
          radspec(nspec) = 0.0_dp
        endif
      endif
    endif
    if (latmassin.and.lvalid) then
      atmass(inat) = val
!
!  Update species masses where not set already based on specific species
!
      do i = 1,nspec
        if (.not.lmassinspec(i)) then
          if (natspec(i).eq.inat) then
            if (natspec(i).le.maxele) then
              massspec(i) = atmass(natspec(i))
            else
              massspec(i) = 0.0_dp
            endif
          endif
        endif
      enddo
    endif
    goto 440
  elseif (index(word,'nmr').eq.1) then
    call getpotsymbol1(iline,llibrary,inat,itype,sym1,1_i4,nbeg,lvalid)
    if (lvalid) then
!
!  Species specific NMR parameters
!
!  Find species
!
      lfound = .false.
      ns = 0
      do while (ns.lt.nspec.and..not.lfound)
        ns = ns + 1
        if (inat.eq.natspec(ns).and.itype.eq.ntypspec(ns)) then
          lfound = .true.
        endif
      enddo
      if (nfloat.le.3) then
        call outerror('insufficient input reading nmr',iline)
        call stopnow('genword')
      endif
      if (lfound) then
        nmrspec(1,ns) = floats(1)
        nmrspec(2,ns) = floats(2)
        nmrspec(3,ns) = floats(3)
        lnmrinspec(ns) = .true.
      else
        nspec = nspec + 1
        if (nspec.gt.maxspec) then
          maxspec = nspec + 20
          call changemaxspec
        endif
        linspec(nspec) = .true.
        natspec(nspec) = inat
        ntypspec(nspec) = itype
        nmrspec(1,nspec) = floats(1)
        nmrspec(2,nspec) = floats(2)
        nmrspec(3,nspec) = floats(3)
        lnmrinspec(nspec) = .true.
        ns = nspec
      endif
    endif
    goto 440
  elseif (index(word,'ioni').eq.1) then
    if (nword.ge.2) then
      s1 = words(2)(1:2)
      na = istoan(s1)
      if (nfloat.ge.1) then
        val = floats(1)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    elseif (nfloat.ge.1) then
      na = nint(floats(1))
      if (nfloat.ge.2) then
        val = floats(2)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    else
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nword.gt.0.and.nfloat.gt.0) then
        s1 = words(1)(1:2)
        na = istoan(s1)
        val = floats(1)
      elseif (nfloat.ge.2) then
        na = nint(floats(1))
        val = floats(2)
      else
        call outerror('insufficient input reading mass',iline)
        call stopnow('genword')
      endif
    endif
    rion(na) = val
    goto 440
  elseif (index(word,'cova').eq.1) then
    if (nword.ge.2) then
      s1 = words(2)(1:2)
      na = istoan(s1)
      if (nfloat.ge.1) then
        val = floats(1)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    elseif (nfloat.ge.1) then
      na = nint(floats(1))
      if (nfloat.ge.2) then
        val = floats(2)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    else
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nword.gt.0.and.nfloat.gt.0) then
        s1 = words(1)(1:2)
        na = istoan(s1)
        val = floats(1)
      elseif (nfloat.ge.2) then
        na = nint(floats(1))
        val = floats(2)
      else
        call outerror('insufficient input reading covalent radius',iline)
        call stopnow('genword')
      endif
    endif
    rcov(na) = val
    goto 440
  elseif (index(word,'vdw').eq.1) then
    if (nword.ge.2) then
      s1 = words(2)(1:2)
      na = istoan(s1)
      if (nfloat.ge.1) then
        val = floats(1)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    elseif (nfloat.ge.1) then
      na = nint(floats(1))
      if (nfloat.ge.2) then
        val = floats(2)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    else
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nword.gt.0.and.nfloat.gt.0) then
        s1 = words(1)(1:2)
        na = istoan(s1)
        val = floats(1)
      elseif (nfloat.ge.2) then
        na = nint(floats(1))
        val = floats(2)
      else
        call outerror('insufficient input reading van der Waals radius',iline)
        call stopnow('genword')
      endif
    endif
    rvdw(na) = val
    goto 440
  elseif (index(word,'symb').eq.1) then
    if (nfloat.ge.1) then
      na = nint(floats(1))
      if (nword.ge.2) then
        cha = words(2)(1:2)
      else
        line = '  '
        read(nru,'(a)',err=99) line
        iline = iline + 1
        call linepro(nru,line,iline)
        cha = words(1)(1:2)
      endif
    else
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.gt.0.and.nword.gt.0) then
        na = nint(floats(1))
        cha = words(1)(1:2)
      else
        call outerror('insufficient input reading element symbol',iline)
        call stopnow('genword')
      endif
    endif
    atsym(na) = cha
    goto 440
  elseif (index(word,'bbar').eq.1) then
    if (nword.ge.2) then
      s1 = words(2)(1:2)
      na = istoan(s1)
      if (nfloat.ge.1) then
        val = floats(1)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    elseif (nfloat.ge.1) then
      na = nint(floats(1))
      if (nfloat.ge.2) then
        val = floats(2)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    else
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nword.gt.0.and.nfloat.gt.0) then
        s1 = words(1)(1:2)
        na = istoan(s1)
        val = floats(1)
      elseif (nfloat.ge.2) then
        na = nint(floats(1))
        val = floats(2)
      else
        call outerror('insufficient input reading bbar',iline)
        call stopnow('genword')
      endif
    endif
    bbar(na) = val !in Angstrom
    goto 440
  elseif (index(word,'sigi').eq.1) then
    if (nword.ge.2) then
      s1 = words(2)(1:2)
      na = istoan(s1)
      if (nfloat.ge.1) then
        val = floats(1)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    elseif (nfloat.ge.1) then
      na = nint(floats(1))
      if (nfloat.ge.2) then
        val = floats(2)
      else
        read(nru,*,err=99) val
        iline = iline + 1
      endif
    else
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nword.gt.0.and.nfloat.gt.0) then
        s1 = words(1)(1:2)
        na = istoan(s1)
        val = floats(1)
      elseif (nfloat.ge.2) then
        na = nint(floats(1))
        val = floats(2)
      else
        call outerror('insufficient input reading sigi',iline)
        call stopnow('genword')
      endif
    endif
    siginc(na) = val*1.0d8 !input units Angstrom squared, stored in barns
    goto 440
  elseif (index(word,'end').eq.1) then
    lwordok = .true.
    return
  else
    l55 = .true.
    return
  endif
!*****************************************
!  S and M electronegativity parameters  *
!*****************************************
490 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) units = autoangs
  endif
!
!  Start of input loop
!
495 line = '  '
  read(nru,'(a)',end=1000) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 495
!
!  Check for old fashion specification of number of atoms
!
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      return
    endif
  endif
!
!  Symbols used in input
!
  if (nword.gt.0) then
    call ltont(words(1),inat,itype)
    leemparaltered(inat) = .true.
    smchi(inat) = floats(1)
    if (nfloat.gt.1) then
      smmu(inat) = floats(2)
      if (nfloat.gt.2) then
        smzeta(inat) = floats(3)
        if (nfloat.gt.3) then
          smZnuc(inat) = floats(4)
          if (nfloat.gt.4) then
            smq0(inat) = floats(5)
            nfloatused = 5
          else
            nfloatused = 4
          endif
        else
          nfloatused = 3
        endif
      else
        nfloatused = 2
      endif
    else
      nfloatused = 1
    endif
  else
!
!  Numeric input
!
    inat = int(floats(1))
    leemparaltered(inat) = .true.
    smchi(inat) = floats(2)
    if (nfloat.gt.2) then
      smmu(inat) = floats(3)
      if (nfloat.gt.3) then
        smzeta(inat) = floats(4)
        if (nfloat.gt.4) then
          smZnuc(inat) = floats(5)
          if (nfloat.gt.5) then
            smq0(inat) = floats(6)
            nfloatused = 6
          else
            nfloatused = 5
          endif
        else
          nfloatused = 4
        endif
      else
        nfloatused = 3
      endif
    else
      nfloatused = 2
    endif
  endif
!     
!  Fitting flags
!       
  if (lfit.and.lfflags) then
    if (nfloat.ge.nfloatused+4) then
      n1 = int(floats(nfloatused+1))
      n2 = int(floats(nfloatused+2))
      n3 = int(floats(nfloatused+3))
      n4 = int(floats(nfloatused+4))
    elseif (nfloat.eq.nfloatused+3) then
      n1 = int(floats(nfloatused+1))
      n2 = int(floats(nfloatused+2))
      n3 = int(floats(nfloatused+3))
      n4 = 0
    elseif (nfloat.eq.nfloatused+2) then
      n1 = int(floats(nfloatused+1))
      n2 = int(floats(nfloatused+2))
      n3 = 0
      n4 = 0
    elseif (nfloat.eq.nfloatused+1) then
      n1 = int(floats(nfloatused+1))
      n2 = 0
      n3 = 0
      n4 = 0
    else
      n1 = 0
      n2 = 0
      n3 = 0
      n4 = 0
    endif
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 15
      nfvar(nfit) = inat
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 16
      nfvar(nfit) = inat
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 17
      nfvar(nfit) = inat
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 18
      nfvar(nfit) = inat
    endif
  endif
!
  goto 495
!*****************************************
!  Maximum number of bfgs line searches  *
!*****************************************
500 if (nfloat.gt.0) then
    maxline = int(floats(1))
  else
    read(nru,*,err=99) maxline
    iline = iline + 1
  endif
  lwordok = .true.
  return
!************************************************************************
!  Select additional output file formats - option to supply file names  *
!************************************************************************
510 if (nword.lt.2) then
    if (ioproc) then
      write(ioout,'(''  **** No output file type specified ****'')')
      write(ioout,'(''  **** Directive ignored             ****'')')
    endif
    lwordok = .true.
    return
  endif
  iwc = 0
  llocalmovie = .false.
  nextraword = 0
  do i = 2,nword
    word = words(i)(1:20)
    lset = .false.
    lnodot = (index(word,'.').eq.0)
!
!  Check that there is no dot in the word, 
!  otherwise it's a file name not an option
!
    if (lnodot) then
      if (.not.lmarv) then
        if (index(word,'marv').eq.1) then
          lmarv = .true.
          lmarv2 = (index(word,'2').ne.0)
          iwc = 1
          lset = .true.
        endif
      endif
      if (.not.lthb) then
        if (index(word,'thb ').eq.1) then
          lthb = .true.
          iwc = 2
          lset = .true.
        endif
      endif
      if (.not.lxtl) then
        if (index(word,'xtl ').eq.1) then
          lxtl = .true.
          iwc = 3
          lset = .true.
        endif
      endif
      if (.not.lphono) then
        if (index(word,'phon').eq.1) then
          lphono = .true.
          iwc = 4
          lset = .true.
        endif
      endif
      if (.not.larc) then
        if (index(word,'arc ').eq.1) then
          larc = .true.
          iwc = 5
          lset = .true.
        endif
      endif
      if (.not.lxr) then
        if (index(word,'xr ').eq.1) then
          lxr = .true.
          iwc = 6
          lset = .true.
        endif
      endif
      if (.not.lcssr) then
        if (index(word,'cssr').eq.1) then
          lcssr = .true.
          iwc = 7
          lset = .true.
        endif
      endif
      if (.not.ltrj) then
        if (index(word,'traj').eq.1) then
          ltrj = .true.
          iwc = 8
          lset = .true.
        endif
      else
        if (index(word,'asci').eq.1) then
          ltrjascii  =  .true.
        endif
        if (index(word,'equi').eq.1) then
          ltrjequil  =  .true.
        endif
      endif
      if (.not.lfrq) then
        if (index(word,'freq').eq.1) then
          lfrq = .true.
          iwc = 9
          lset = .true.
        endif
      endif
      if (.not.lxyz) then
        if (index(word,'xyz ').eq.1) then
          lxyz = .true.
          iwc = 10
          lset = .true.
        endif
      endif
      if (.not.lhis) then
        if (index(word,'his').eq.1) then
          lhis = .true.
          iwc = 11
          lset = .true.
        endif
      endif
      if (.not.lfdf) then
        if (index(word,'fdf ').eq.1) then
          lfdf = .true.
          iwc = 12
          lset = .true.
        endif
      endif
      if (.not.ldrv) then
        if (index(word,'der').eq.1.or.index(word,'drv ').eq.1) then
          ldrv = .true.
          iwc = 13
          lset = .true.
        endif
      endif
      if (.not.lfrc) then
        if (index(word,'frc ').eq.1) then
          lfrc = .true.
          iwc = 14
          lset = .true.
        endif
      endif
      if (.not.lcif) then
        if (index(word,'cif ').eq.1) then
          lcif = .true.
          iwc = 15
          lset = .true.
        endif
      endif
      if (.not.ldlv) then
        if (index(word,'str ').eq.1) then
          ldlv = .true.
          iwc = 16
          lset = .true.
        endif
      endif
      if (.not.leig) then
        if (index(word,'eig ').eq.1) then
          leig = .true.
          iwc = 17
          lset = .true.
        endif
      endif
      if (.not.lpre) then
        if (index(word,'pres').eq.1) then
          if (ioproc) lpre = .true.
          iwc = 19
          lset = .true.
        endif
      endif
      if (.not.lsas) then
        if (index(word,'sas ').eq.1) then
          if (ioproc) lsas = .true.
          iwc = 20
          lset = .true.
        endif
      endif
      if (.not.losc) then
        if (index(word,'osc ').eq.1) then
          if (ioproc) losc = .true.
          iwc = 21
          lset = .true.
        endif
      endif
      if (.not.lbio) then
        if (index(word,'bio ').eq.1) then
          if (ioproc) lbio = .true.
          iwc = 22
          lset = .true.
        endif
      endif
      if (index(word,'pdf').eq.1) then
        iwc = 23
        lset = .true.
      endif
      if (.not.lcosmofile) then
        if (index(word,'cos').eq.1) then
          lcosmofile = .true.
          iwc = 24
          lset = .true.
        endif
      endif
      if (.not.lqbo) then
        if (index(word,'qbo ').eq.1) then
          if (ioproc) lqbo = .true.
          iwc = 25
          lset = .true.
        endif
      endif
      if (.not.llammpspots) then
        if (index(word,'lammps_p').eq.1) then
          llammpspots = .true.
          iwc = 26
          lset = .true.
        endif
      endif
      if (.not.ldcd) then
        if (index(word,'dcd').eq.1) then
          ldcd = .true.
          iwc = 27
          lset = .true.
        endif
      endif
      if (.not.llammps) then
        if (index(word,'lam').eq.1) then
          llammps = .true.
          iwc = 28
          lset = .true.
        endif
      endif
!
!  CML output
!
      if (.not.lcml) then
        if (index(word,'cml ').eq.1) then
          if (ioproc) lcml = .true.
          iwc = 1000 
          lset = .true.
        endif 
      endif
      if (.not.lvcml) then 
        if (index(word,'vcml ').eq.1) then
          if (ioproc) lcml = .true.
          if (ioproc) lvcml = .true.
          iwc = 1001
          lset = .true.
        endif
      endif
    endif
    if (index(word,'movi').eq.1.and.lnodot) then
      llocalmovie = .true.
      nextraword = nextraword + 1
    elseif (index(word,'shel').eq.1.and.lnodot) then
      loutshell = .true.
      nextraword = nextraword + 1
    elseif (lfrq.and.index(word,'bin').eq.1.and.lnodot) then
      lfrqbin = .true.
      nextraword = nextraword + 1
    elseif (index(word,'tex').eq.1.and.lnodot) then
      lfrqbin = .false.
      nextraword = nextraword + 1
    elseif (lqbo.and.index(word,'app').eq.1.and.lnodot) then
      lqboappend = .true.
      nextraword = nextraword + 1
    elseif (.not.lset) then
!
!  Assign any output files that may have been given
!
      if (iwc.eq.1) then
        marvfile = words(i)
        if (index(marvfile,'.mvn').eq.0) then
          call endstring(marvfile,len(marvfile),iend)
          if (iend.eq.0) iend = 57
          marvfile(iend:iend+3) = '.mvn'
        endif
      elseif (iwc.eq.2) then
        thbfile = words(i)
      elseif (iwc.eq.3) then
        xtlfile = words(i)
        if (index(xtlfile,'.xtl').eq.0) then
          call endstring(xtlfile,len(xtlfile),iend)
          if (iend.eq.0) iend = 57
          xtlfile(iend:iend+3) = '.xtl'
        endif
      elseif (iwc.eq.4) then
        phonfile = words(i)
      elseif (iwc.eq.5) then
        arcfile = words(i)
        if (index(arcfile,'.arc').eq.0.and.index(arcfile,'.car').eq.0) then
          call endstring(arcfile,len(arcfile),iend)
          if (iend.eq.0) iend = 57
          arcfile(iend:iend+3) = '.arc'
        endif
        lmovie = llocalmovie
        if (nfloat.gt.0.and.lmovie) then
          narcwrite = nint(abs(floats(1)))
          narcwrite = max(narcwrite,1_i4)
        endif
      elseif (iwc.eq.6) then
        xrfile = words(i)
        if (index(xrfile,'.xr').eq.0) then
          call endstring(xrfile,len(xrfile),iend)
          if (iend.eq.0) iend = 28
          xrfile(iend:iend+2) = '.xr'
        endif
      elseif (iwc.eq.7) then
        cssrfile = words(i)
        if (index(cssrfile,'.cssr').eq.0) then
          call endstring(cssrfile,len(cssrfile),iend)
          if (iend.eq.0) iend = 26
          cssrfile(iend:iend+4) = '.cssr'
        endif
      elseif (iwc.eq.8) then
        trjfile = words(i)
        if (index(trjfile,'.trg').eq.0) then
          call endstring(trjfile,len(trjfile),iend)
          if (iend.eq.0) iend = 26
          trjfile(iend:iend+4) = '.trg'
        endif
      elseif (iwc.eq.9) then
        freqfile = words(i)
        if (nfloat.gt.0) then
          nfreqdecimals = abs(nint(floats(1)))
          nfreqdecimals = min(nfreqdecimals,12_i4)
          nfreqdecimals = max(nfreqdecimals,1_i4)
        endif
      elseif (iwc.eq.10) then
        xyzfile = words(i)
        if (index(xyzfile,'.xyz').eq.0) then
          call endstring(xyzfile,len(xyzfile),iend)
          if (iend.eq.0) iend = 57
          xyzfile(iend:iend+3) = '.xyz'
        endif
        lxyzmovie = llocalmovie
      elseif (iwc.eq.11) then
        hisfile = words(i)
        if (index(hisfile,'.his').eq.0) then
          call endstring(hisfile,len(hisfile),iend)
          if (iend.eq.0) iend = 57
          hisfile(iend:iend+3) = '.his'
        endif
      elseif (iwc.eq.12) then
        fdffile = words(i)
        if (index(fdffile,'.fdf').eq.0) then
          call endstring(fdffile,len(fdffile),iend)
          if (iend.eq.0) iend = 57
          fdffile(iend:iend+3) = '.fdf'
        endif
      elseif (iwc.eq.13) then
        drvfile = words(i)
        if (index(drvfile,'.drv').eq.0) then
          call endstring(drvfile,len(drvfile),iend)
          if (iend.eq.0) iend = 57
          drvfile(iend:iend+3) = '.drv'
        endif
      elseif (iwc.eq.14) then
        frcfile = words(i)
        if (index(frcfile,'.frc').eq.0) then
          call endstring(frcfile,len(frcfile),iend)
          if (iend.eq.0) iend = 57
          frcfile(iend:iend+3) = '.frc'
        endif
      elseif (iwc.eq.15) then
        ciffile = words(i)
        if (index(ciffile,'.cif').eq.0) then
          call endstring(ciffile,len(ciffile),iend)
          if (iend.eq.0) iend = 57
          ciffile(iend:iend+3) = '.cif'
        endif
      elseif (iwc.eq.16) then
        dlvfile = words(i)
        if (index(dlvfile,'.str').eq.0) then
          call endstring(dlvfile,len(dlvfile),iend)
          if (iend.eq.0) iend = 57
          dlvfile(iend:iend+3) = '.str'
        endif
      elseif (iwc.eq.17) then
        eigfile = words(i)
        if (index(eigfile,'.eig').eq.0) then
          call endstring(eigfile,len(eigfile),iend)
          if (iend.eq.0) iend = 57
          eigfile(iend:iend+3) = '.eig'
        endif
      elseif (iwc.eq.19) then
        prefile = words(i)
        if (index(prefile,'.pre').eq.0) then
          call endstring(prefile,len(prefile),iend)
          if (iend.eq.0) iend = 57
          prefile(iend:iend+3) = '.pre'
        endif
      elseif (iwc.eq.20) then
        sasfile = words(i)
        if (index(sasfile,'.sas').eq.0) then
          call endstring(sasfile,len(sasfile),iend)
          if (iend.eq.0) iend = 57
          sasfile(iend:iend+3) = '.sas'
        endif
      elseif (iwc.eq.21) then
        oscfile = words(i)
        if (index(oscfile,'.osc').eq.0) then
          call endstring(oscfile,len(oscfile),iend)
          if (iend.eq.0) iend = 57
          oscfile(iend:iend+3) = '.osc'
        endif
      elseif (iwc.eq.22) then
        biofile = words(i)
        if (index(biofile,'.bio').eq.0) then
          call endstring(biofile,len(biofile),iend)
          if (iend.eq.0) iend = 57
          biofile(iend:iend+3) = '.bio'
        endif
      elseif (iwc.eq.23) then
        pdffiles(ncurr) = words(i)
        if (index(pdffiles(ncurr),'.wid').eq.0) then
          iend = index(pdffiles(ncurr),' ')
          call endstring(pdffiles(ncurr),len(pdffiles(ncurr)),iend)
          if (iend.eq.0) iend = 57
          pdffiles(ncurr)(iend:iend+3) = '.wid'
        endif
      elseif (iwc.eq.24) then
        cosmofile = words(i)
        if (index(cosmofile,'.cosmo').eq.0) then
          call endstring(cosmofile,len(cosmofile),iend)
          if (iend.eq.0) iend = 55
          cosmofile(iend:iend+5) = '.cosmo'
        endif
      elseif (iwc.eq.25) then
        qbofile = words(i)
        if (index(qbofile,'.qbo').eq.0) then
          call endstring(qbofile,len(qbofile),iend)
          if (iend.eq.0) iend = 57
          qbofile(iend:iend+3) = '.qbo'
        endif
      elseif (iwc.eq.26) then
        lammpspotsfile = words(i)
        if (index(lammpspotsfile,'.tab').eq.0) then
          call endstring(lammpspotsfile,len(lammpspotsfile),iend)
          if (iend.eq.0) iend = 57
          lammpspotsfile(iend:iend+3) = '.tab'
        endif
        if (nfloat.ge.3) then
          lammps_r0   = abs(floats(1))
          lammps_rend = abs(floats(2))
          if (lammps_rend.lt.lammps_r0) then
            call outerror('lammps potentials - end point before start',iline)
            call stopnow('genword')
          endif
          nlammpspoints   = nint(abs(floats(3)))
          if (nlammpspoints.le..1) then
            call outerror('lammps potentials - number of points too small',iline)
            call stopnow('genword')
          endif
        else
          call outerror('value(s) missing from input for lammps potentials',iline)
          call stopnow('genword')
        endif
      elseif (iwc.eq.27) then
        dcdfile = words(i)
        if (index(dcdfile,'.dcd').eq.0) then
          call endstring(dcdfile,len(dcdfile),iend)
          if (iend.eq.0) iend = 57
          dcdfile(iend:iend+3) = '.dcd'
        endif
      elseif (iwc.eq.28) then
        lammpsfile = words(i)
        if (index(lammpsfile,'.lmp').eq.0) then
          call endstring(lammpsfile,len(lammpsfile),iend)
          if (iend.eq.0) iend = 57
          lammpsfile(iend:iend+3) = '.lmp'
        endif
!
! CML output
!
      elseif (iwc.eq.1000) then
        cmlfilename = words(i)
        if (index(cmlfilename,'.xml').eq.0) then
          call endstring(cmlfilename,len(cmlfilename),iend)
          if (iend.eq.0) iend = 57
          cmlfilename(iend:iend+3) = '.xml'
        endif
      elseif (iwc.eq.1001) then
        cmlfilename = words(i)
        if (index(cmlfilename,'.xml').eq.0) then
          call endstring(cmlfilename,len(cmlfilename),iend)
          if (iend.eq.0) iend = 57
          cmlfilename(iend:iend+3) = '.xml'
        endif
      else
        nwarn = nwarn + 1
        call outwarning('invalid filetype supplied in output option',0_i4)
      endif
    endif
  enddo
  if (((nword-nextraword).eq.2).and.nfloat.ge.1) then
!
!  File name was not given but there is a numeric file name
!
    if (nfloat.ge.2) then
      word = floatwords(2)(1:20)
    else
      word = floatwords(1)(1:20)
    endif
    lset = .false.
    lnodot = (index(word,'.').eq.0)
!
!  Check that there is no dot in the word, 
!  otherwise it's a file name not an option
!
    if (.not.lset) then
!
!  Assign any output files that may have been given
!
      if (iwc.eq.1) then
        marvfile = word
        if (index(marvfile,'.mvn').eq.0) then
          call endstring(marvfile,len(marvfile),iend)
          if (iend.eq.0) iend = 57
          marvfile(iend:iend+3) = '.mvn'
        endif
      elseif (iwc.eq.2) then
        thbfile = word
      elseif (iwc.eq.3) then
        xtlfile = word
        if (index(xtlfile,'.xtl').eq.0) then
          call endstring(xtlfile,len(xtlfile),iend)
          if (iend.eq.0) iend = 57
          xtlfile(iend:iend+3) = '.xtl'
        endif
      elseif (iwc.eq.4) then
        phonfile = word
      elseif (iwc.eq.5) then
        arcfile = word
        if (index(arcfile,'.arc').eq.0.and.index(arcfile,'.car').eq.0) then
          call endstring(arcfile,len(arcfile),iend)
          if (iend.eq.0) iend = 57
          arcfile(iend:iend+3) = '.arc'
        endif
        lmovie = llocalmovie
        if (nfloat.gt.1.and.lmovie) then
          narcwrite = nint(abs(floats(1)))
          narcwrite = max(narcwrite,1_i4)
        endif
      elseif (iwc.eq.6) then
        xrfile = word
        if (index(xrfile,'.xr').eq.0) then
          iend = index(xrfile,' ')
          if (iend.eq.0) iend = 28
          xrfile(iend:iend+2) = '.xr'
        endif
      elseif (iwc.eq.7) then
        cssrfile = word
        if (index(cssrfile,'.cssr').eq.0) then
          call endstring(cssrfile,len(cssrfile),iend)
          if (iend.eq.0) iend = 26
          cssrfile(iend:iend+4) = '.cssr'
        endif
      elseif (iwc.eq.8) then
        trjfile = word
        if (index(trjfile,'.trg').eq.0) then
          call endstring(trjfile,len(trjfile),iend)
          if (iend.eq.0) iend = 26
          trjfile(iend:iend+4) = '.trg'
        endif
      elseif (iwc.eq.9) then
        freqfile = word
      elseif (iwc.eq.10) then
        xyzfile = word
        if (index(xyzfile,'.xyz').eq.0) then
          call endstring(xyzfile,len(xyzfile),iend)
          if (iend.eq.0) iend = 57
          xyzfile(iend:iend+3) = '.xyz'
        endif
        lxyzmovie = llocalmovie
      elseif (iwc.eq.11) then
        hisfile = word
        if (index(hisfile,'.his').eq.0) then
          call endstring(hisfile,len(hisfile),iend)
          if (iend.eq.0) iend = 57
          hisfile(iend:iend+3) = '.his'
        endif
      elseif (iwc.eq.12) then
        fdffile = word
        if (index(fdffile,'.fdf').eq.0) then
          call endstring(fdffile,len(fdffile),iend)
          if (iend.eq.0) iend = 57
          fdffile(iend:iend+3) = '.fdf'
        endif
      elseif (iwc.eq.13) then
        drvfile = word
        if (index(drvfile,'.drv').eq.0) then
          call endstring(drvfile,len(drvfile),iend)
          if (iend.eq.0) iend = 57
          drvfile(iend:iend+3) = '.drv'
        endif
      elseif (iwc.eq.14) then
        frcfile = word
        if (index(frcfile,'.frc').eq.0) then
          call endstring(frcfile,len(frcfile),iend)
          if (iend.eq.0) iend = 57
          frcfile(iend:iend+3) = '.frc'
        endif
      elseif (iwc.eq.15) then
        ciffile = word
        if (index(ciffile,'.cif').eq.0) then
          call endstring(ciffile,len(ciffile),iend)
          if (iend.eq.0) iend = 57
          ciffile(iend:iend+3) = '.cif'
        endif
      elseif (iwc.eq.16) then
        dlvfile = word
        if (index(dlvfile,'.str').eq.0) then
          call endstring(dlvfile,len(dlvfile),iend)
          if (iend.eq.0) iend = 57
          dlvfile(iend:iend+3) = '.str'
        endif
      elseif (iwc.eq.17) then
        eigfile = word
        if (index(eigfile,'.eig').eq.0) then
          call endstring(eigfile,len(eigfile),iend)
          if (iend.eq.0) iend = 57
          eigfile(iend:iend+3) = '.eig'
        endif
      elseif (iwc.eq.19) then
        prefile = word
        if (index(prefile,'.pre').eq.0) then
          call endstring(prefile,len(prefile),iend)
          if (iend.eq.0) iend = 57
          prefile(iend:iend+3) = '.pre'
        endif
      elseif (iwc.eq.20) then
        sasfile = word
        if (index(sasfile,'.sas').eq.0) then
          call endstring(sasfile,len(sasfile),iend)
          if (iend.eq.0) iend = 57
          sasfile(iend:iend+3) = '.sas'
        endif
      elseif (iwc.eq.21) then
        oscfile = word
        if (index(oscfile,'.osc').eq.0) then
          call endstring(oscfile,len(oscfile),iend)
          if (iend.eq.0) iend = 57
          oscfile(iend:iend+3) = '.osc'
        endif
      elseif (iwc.eq.22) then
        biofile = word
        if (index(biofile,'.bio').eq.0) then
          call endstring(biofile,len(biofile),iend)
          if (iend.eq.0) iend = 57
          biofile(iend:iend+3) = '.bio'
        endif
      elseif (iwc.eq.23) then
        pdffiles(ncurr) = word
        if (index(pdffiles(ncurr),'.wid').eq.0) then
          iend = index(pdffiles(ncurr),' ')
          call endstring(pdffiles(ncurr),len(pdffiles(ncurr)),iend)
          if (iend.eq.0) iend = 57
          pdffiles(ncurr)(iend:iend+3) = '.wid'
        endif
      elseif (iwc.eq.24) then
        cosmofile = word
        if (index(cosmofile,'.cosmo').eq.0) then
          call endstring(cosmofile,len(cosmofile),iend)
          if (iend.eq.0) iend = 55
          cosmofile(iend:iend+5) = '.cosmo'
        endif
      elseif (iwc.eq.25) then
        qbofile = word
        if (index(qbofile,'.qbo').eq.0) then
          call endstring(qbofile,len(qbofile),iend)
          if (iend.eq.0) iend = 57
          qbofile(iend:iend+5) = '.qbo'
        endif
      elseif (iwc.eq.27) then
        dcdfile = word
        if (index(dcdfile,'.dcd').eq.0) then
          call endstring(dcdfile,len(dcdfile),iend)
          if (iend.eq.0) iend = 57
          dcdfile(iend:iend+5) = '.dcd'
        endif
!
! CML output
!
      elseif (iwc.eq.1000) then
        cmlfilename = word
        if (index(cmlfilename,'.xml').eq.0) then
          call endstring(cmlfilename,len(cmlfilename),iend)
          if (iend.eq.0) iend = 57
          cmlfilename(iend:iend+3) = '.xml'
        endif
      elseif (iwc.eq.1001) then
        cmlfilename = word
        if (index(cmlfilename,'.xml').eq.0) then
          call endstring(cmlfilename,len(cmlfilename),iend)
          if (iend.eq.0) iend = 57
          cmlfilename(iend:iend+3) = '.xml'
        endif
      else
        nwarn = nwarn + 1
        call outwarning('invalid filetype supplied in output option',0_i4)
      endif
    endif
  endif
  lwordok = .true.
  return
!********************************************************************
!  Relative speed of reciprocal and real space electrostatic terms  *
!********************************************************************
520 if (nfloat.ge.1) then
    rspeed0 = abs(floats(1))
  else
    call outerror('rspeed value(s) missing from input',iline)
    call stopnow('genword')
  endif
  lwordok = .true.
  return
!*********
!  Nadd  *
!*********
530 if (nfloat.gt.0) then
    nadd = int(floats(1))
  else
    read(nru,*,err=99) nadd
    iline = iline + 1
  endif
  lwordok = .true.
  return
!*********************************************************
!  gdcrit - for switch between energy and force balance  *
!*********************************************************
540 if (nfloat.gt.0) then
    gdcrit = floats(1)
  else
    read(nru,*,err=99) gdcrit
    iline = iline + 1
  endif
  gdcrit = abs(gdcrit)
  lwordok = .true.
  return
!******************************************************************
!  No bond - prevent molecule option from locating bonds between  *
!  specified atom types                                           *
!  Save index as six figure number - each three digits implies    *
!  atom type                                                      *
!******************************************************************
550 nnobo = nnobo + 1
  if (nnobo.gt.maxnobo) then
    maxnobo = nnobo + 10
    call changemaxnobo
  endif
  if (nfloat.ge.2) then
    nvar1 = nint(floats(1))
    if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
    nvar2 = nint(floats(2))
    if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
    itype1 = 0
    itype2 = 0
  elseif (nword.eq.3) then
    call ltont(words(2),nvar1,itype1)
    call ltont(words(3),nvar2,itype2)
  elseif (nword.eq.4) then
    call ltont(words(2),nvar1,itype1)
    word = words(3)(1:20)
    call stolc(word,20_i4)
    if (index(word,'cor').eq.1.or.index(word,'bco').eq.1) then
      call ltont(words(4),nvar2,itype2)
    elseif (index(word,'she').eq.1.or.index(word,'bsh').eq.1) then
      nvar1 = nvar1 + maxele
      call ltont(words(4),nvar2,itype2)
    else
      call ltont(words(3),nvar2,itype2)
      word = words(4)(1:20)
      call stolc(word,20_i4)
      if (index(word,'she').eq.1.or.index(word,'bsh').eq.1) then
        nvar2 = nvar2 + maxele
      endif
    endif
  elseif (nword.ge.5) then
    call ltont(words(2),nvar1,itype1)
    call ltont(words(4),nvar2,itype2)
    if ((index(words(3),'s').eq.1).or.(index(words(3),'S').eq.1)) nvar1 = nvar1 + maxele
    if ((index(words(5),'s').eq.1).or.(index(words(5),'S').eq.1)) nvar2 = nvar2 + maxele
    if ((index(words(3),'bs').eq.1).or.(index(words(3),'BS').eq.1)) nvar1 = nvar1 + maxele
    if ((index(words(5),'bs').eq.1).or.(index(words(5),'BS').eq.1)) nvar2 = nvar2 + maxele
  else
    line = '  '
    read(nru,'(a)',err=99) line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.ge.2) then
      nvar1 = nint(floats(1))
      if (nvar1.gt.100) nvar1 = nvar1 - 100 + maxele
      nvar2 = nint(floats(2))
      if (nvar2.gt.100) nvar2 = nvar2 - 100 + maxele
      itype1 = 0
      itype2 = 0
    elseif (nword.eq.2) then
      call ltont(words(1),nvar1,itype1)
      call ltont(words(2),nvar2,itype2)
    elseif (nword.eq.3) then
      call ltont(words(1),nvar1,itype1)
      word = words(2)(1:20)
      call stolc(word,20_i4)
      if (index(word,'cor').eq.1.or.index(word,'bco').eq.1) then
        call ltont(words(3),nvar2,itype2)
      elseif (index(word,'she').eq.1.or.index(word,'bsh').eq.1) then
        nvar1 = nvar1 + maxele
        call ltont(words(3),nvar2,itype2)
      else
        call ltont(words(2),nvar2,itype2)
        word = words(3)(1:20)
        call stolc(word,20_i4)
        if (index(word,'she').eq.1.or.index(word,'bsh').eq.1) then
          nvar2 = nvar2 + maxele
        endif
      endif
    elseif (nword.ge.4) then
      call ltont(words(1),nvar1,itype1)
      call ltont(words(3),nvar2,itype2)
      if ((index(words(2),'s').eq.1).or.(index(words(2),'S').eq.1)) nvar1 = nvar1 + maxele
      if ((index(words(4),'s').eq.1).or.(index(words(4),'S').eq.1)) nvar2 = nvar2 + maxele
      if ((index(words(2),'bs').eq.1).or.(index(words(2),'BS').eq.1)) nvar1 = nvar1 + maxele
      if ((index(words(4),'bs').eq.1).or.(index(words(4),'BS').eq.1)) nvar2 = nvar2 + maxele
    else
      call outerror('Error in species input for nobond option',iline)
      call stopnow('genword')
    endif
  endif
  if (nvar1.eq.nvar2) then
    nvar2 = nvar2 + 1000*nvar1
    nobond(nnobo) = nvar2
    if (itype1.lt.itype2) then
      itype2 = itype2 + 1000*itype1
      nobotyp(nnobo) = itype2
    else
      itype1 = itype1 + 1000*itype2
      nobotyp(nnobo) = itype1
    endif
  elseif (nvar1.lt.nvar2) then
    nvar2 = nvar2 + 1000*nvar1
    nobond(nnobo) = nvar2
    itype2 = itype2 + 1000*itype1
    nobotyp(nnobo) = itype2
  else
    nvar1 = nvar1 + 1000*nvar2
    nobond(nnobo) = nvar1
    itype1 = itype1 + 1000*itype2
    nobotyp(nnobo) = itype1
  endif
  lwordok = .true.
  return
!****************************************************
!  Number of cycles before enforced Hessian update  *
!****************************************************
560 if (nword.gt.1) then
    if (index(words(2),'opt').eq.1) then
!
!  Set only optimisation update
!
      if (nfloat.gt.0) then
        nupdate = nint(floats(1))
      else
        read(nru,*,err=99) nupdate
        iline = iline + 1
      endif
    elseif (index(words(2),'fit').eq.1) then
!
!  Set only fit update
!
      if (nfloat.gt.0) then
        nfupdate = nint(floats(1))
      else
        read(nru,*,err=99) nfupdate
        iline = iline + 1
      endif
    else
!
!  Set according to job type
!
      if (lopt) then
        if (nfloat.gt.0) then
          nupdate = nint(floats(1))
        else
          read(nru,*,err=99) nupdate
          iline = iline + 1
        endif
        if (lfit) nfupdate = nupdate
      elseif (lfit) then
        if (nfloat.gt.0) then
          nfupdate = nint(floats(1))
        else
          read(nru,*,err=99) nfupdate
          iline = iline + 1
        endif
      endif
    endif
  else
!
!  Set according to job type
!
    if (lopt) then
      if (nfloat.gt.0) then
        nupdate = nint(floats(1))
      else
        read(nru,*,err=99)nupdate
        iline = iline + 1
      endif
      if (lfit) nfupdate = nupdate
    elseif (lfit) then
      if (nfloat.gt.0) then
        nfupdate = nint(floats(1))
      else
        read(nru,*,err=99)nfupdate
        iline = iline + 1
      endif
    endif
  endif
  if (nupdate.le.0) then
    nwarn = nwarn + 1
    if (ioproc) then
      write(ioout,'(/,''  **** Warning - updating of Hessian every zero cycles is meaningless ****'')')
      write(ioout,'(''  **** The update parameter will be reset to the default value        ****'')')
    endif
    nupdate = 10
  endif
  if (nfupdate.le.0) then
    nwarn = nwarn + 1
    if (ioproc) then
      write(ioout,'(/,''  **** Warning - updating of Hessian every zero cycles is meaningless ****'')')
      write(ioout,'(''  **** The update parameter will be reset to the default value        ****'')')
    endif
    nfupdate = 20
  endif
  lwordok = .true.
  return
!***********************************************
!  Order of the limited memory BFGS algorithm  *
!***********************************************
570 if (nfloat.ge.1) then
    lmbfgsorder = nint(floats(1))
  else
    read(nru,*,err=99) lmbfgsorder
    iline=iline+1
  endif
  lwordok = .true.    
  return
!**************************************************
!  Maximum number of points in line minimisation  *
!**************************************************
580 if (nfloat.ge.1) then
    nlinmin = nint(floats(1))
  else
    read(nru,*,err=99) nlinmin
    iline=iline+1
  endif
  lwordok = .true.
  return
!********************
!  Potential sites  *
!********************
590 units = 1.0_dp
  lcart = (ndimen(ncurr).eq.0)
  if (nword.gt.1) then
    do i = 2,nword
      if (index(words(i),'cart').eq.1) lcart = .true.
      if (index(words(i),'au').eq.1) units = autoangs
    enddo
  endif
595 line = '  '
  read(nru,'(a)',err=1000) line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nword.gt.0) then
    l55 = .true.
    return
  elseif ((nword+nfloat).eq.0) then
    goto 595
  endif
  npotsites = npotsites + 1
  if (npotsites.gt.maxpotsites) then
    maxpotsites = npotsites + 20
    call changemaxpotsites
  endif
  if (nfloat.lt.3) then
    call outerror('Insufficient coordinates specified',iline)
    call stopnow('genword')
  endif
  npotsitecfg(npotsites) = ncurr
  if (lcart.and.ndimen(ncurr).gt.0) then
    if (ndimen(ncurr).eq.3) then
      rmat(1,4) = floats(1)*units
      rmat(2,4) = floats(2)*units
      rmat(3,4) = floats(3)*units
      do j = 1,3
        rmat(1,j) = rv(1,j)
        rmat(2,j) = rv(2,j)
        rmat(3,j) = rv(3,j)
      enddo
      call GULP_gauss(3_i4,3_i4,1_i4,rmat)
      xpotsite(npotsites) = rmat(1,4)
      ypotsite(npotsites) = rmat(2,4)
      zpotsite(npotsites) = rmat(3,4)
    elseif (ndimen(ncurr).eq.2) then
      rmat(1,3) = floats(1)*units
      rmat(2,3) = floats(2)*units
      do j = 1,2
        rmat(1,j) = rv(1,j)
        rmat(2,j) = rv(2,j)
      enddo
      call GULP_gauss(2_i4,3_i4,1_i4,rmat)
      xpotsite(npotsites) = rmat(1,3)
      ypotsite(npotsites) = rmat(2,3)
      zpotsite(npotsites) = floats(3)*units
    elseif (ndimen(ncurr).eq.1) then
      xpotsite(npotsites) = floats(1)*units*rv(1,1)
      ypotsite(npotsites) = floats(2)*units
      zpotsite(npotsites) = floats(3)*units
    endif
  elseif (lcart) then
    xpotsite(npotsites) = floats(1)*units
    ypotsite(npotsites) = floats(2)*units
    zpotsite(npotsites) = floats(3)*units
  else
    if (ndimen(ncurr).eq.1) then
      xpotsite(npotsites) = floats(1)
      ypotsite(npotsites) = floats(2)*units
      zpotsite(npotsites) = floats(3)*units
    elseif (ndimen(ncurr).eq.2) then
      xpotsite(npotsites) = floats(1)
      ypotsite(npotsites) = floats(2)
      zpotsite(npotsites) = floats(3)*units
    elseif (ndimen(ncurr).eq.3) then
      xpotsite(npotsites) = floats(1)
      ypotsite(npotsites) = floats(2)
      zpotsite(npotsites) = floats(3)
    endif
  endif
  goto 595
!****************************
!  Potential interpolation  *
!****************************
600 if (nfloat.gt.0) then
    lpotlinterpolate = .true.
    nptsinterpolate = nint(abs(floats(1)))
  endif
  lwordok = .true.
  return
!************************************************************
!  Maximum change in function before recalculating Hessian  *
!************************************************************
610 if (nfloat.ge.1) then
    delfc = floats(1)
  else
    read(nru,*,err=99) delfc
    iline = iline + 1
  endif
  lwordok = .true.
  return
!****************************************
!  Marvin insert for Marvin input file  *
!****************************************
620 if (lmarv) then
    ind = index(marvfile,'.mvn') + 1
    if (ind.eq.0) then
      marvtemp = 'marvin.gmt'
    else
      marvtemp = marvfile
      marvtemp(ind:ind+3) = 'gmt'
    endif
  else
    marvtemp = 'marvin.gmt'
  endif
  open(7,file=marvtemp,status='unknown',err=625)
623 read(nru,'(a)',end=625,err=625) line
  if (index(line,'end').eq.1) goto 625
  if (ioproc) then
    write(7,'(a80)')line
  endif
  goto 623
625 close(7)
  lwordok = .true.
  return
!***********************
!  QM/MM mode control  *
!***********************
630 if (nword.ge.2) then
    call stolc(words(2),maxword)
    if (index(words(2),'me').eq.1) then
      QMMMmode(ncurr) = 1
    elseif (index(words(2),'el').eq.1) then
      QMMMmode(ncurr) = 2
    endif
  endif
  lwordok = .true.
  return
!***********************
!  Units modification  *
!***********************
640 if (nword.ge.2.and.nfloat.ge.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'kcaltoev').eq.1) then
      kcaltoev = floats(1)
    elseif (index(words(2),'angstoev').eq.1) then
      angstoev = floats(1)
    endif
  endif
  lwordok = .true.
  return
!*******************************
!  Selection of maths library  *
!*******************************
650 if (nword.ge.2) then
    call stolc(words(2),maxword)
    if (index(words(2),'lap').eq.1) then
      leispack_eigensolve = .false.
      if (nword.ge.3) then
        call stolc(words(3),maxword)
        if (index(words(3),'nod').eq.1) ldivide_and_conquer = .false.
      endif
    elseif (index(words(2),'eis').eq.1) then
      leispack_eigensolve = .true.
    endif
  endif
  lwordok = .true.
  return
!******************
!  Stress tensor  *
!******************
680 units = 1.0_dp
  if (nword.ge.2) then
    call stolc(words(2),maxword)
    if (index(words(2),'pa ').eq.1) then
      units = 1.0d-9
    elseif (index(words(2),'kpa ').eq.1) then
      units = 1.0d-6
    elseif (index(words(2),'atm').eq.1) then
      units = 101.325d-6
    elseif (index(words(2),'nm-2').eq.1) then
      units = 1.0d-9
    elseif (index(words(2),'kbar').eq.1) then
      units = 0.1_dp
    endif
  endif
  if (nfloat.lt.6) then
    line = '  '
    read(nru,'(a)')line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.ge.6) then
      if (nword.ge.1) then
        call stolc(words(1),maxword)
        if (index(words(1),'pa ').eq.1) then
          units = 1.0d-9
        elseif (index(words(1),'kpa ').eq.1) then
          units = 1.0d-6
        elseif (index(words(1),'atm').eq.1) then
          units = 101.325d-6
        elseif (index(words(1),'nm-2').eq.1) then
          units = 1.0d-9
        elseif (index(words(1),'kbar').eq.1) then
          units = 0.1_dp
        endif
      endif
    else
      call outerror('Stress tensor is missing or incomplete',iline)
      call stopnow('genword')
    endif
  endif
  stresscfg(1,ncurr) = floats(1)*units
  stresscfg(2,ncurr) = floats(2)*units
  stresscfg(3,ncurr) = floats(3)*units
  stresscfg(4,ncurr) = floats(4)*units
  stresscfg(5,ncurr) = floats(5)*units
  stresscfg(6,ncurr) = floats(6)*units
!
!  Set pressure according to average of on diagonal elements of stress tensor
!
  presscfg(ncurr) = (stresscfg(1,ncurr)+stresscfg(2,ncurr)+stresscfg(3,ncurr))
  nonzero = 0
  if (abs(stresscfg(1,ncurr)).gt.1.0d-12) nonzero = nonzero + 1
  if (abs(stresscfg(2,ncurr)).gt.1.0d-12) nonzero = nonzero + 1
  if (abs(stresscfg(3,ncurr)).gt.1.0d-12) nonzero = nonzero + 1
  if (nonzero.gt.0) presscfg(ncurr) = presscfg(ncurr)/dble(nonzero)
  lwordok = .true.
  return
!******************************
!  Maximise using RFO method  *
!******************************
690 if (nword.gt.1) then
    if (index(words(2),'mode').eq.1) then
      if (morder.gt.1) then
        call outerror('Mode following only allowed for 1st order TS',iline)
        call stopnow('genword')
      endif
      if (nfloat.ge.1) then
        mode = nint(floats(1))
        morder = 1
      else
        nwarn = nwarn + 1
        call outwarning('no mode given for maximisation',iline)
      endif
    elseif (index(words(2),'orde').eq.1) then
      if (nfloat.ge.1) then
        morder = nint(floats(1))
      else
        nwarn = nwarn + 1
        call outwarning('no mode given for maximisation',iline)
      endif
      if (mode.gt.0.and.morder.gt.1) then
        call outerror('Mode following only allowed for 1st order TS',iline)
        call stopnow('genword')
      endif
    endif
  endif
  lwordok = .true.
  return
!******************************
!  Pressure of configuration  *
!******************************
700 if (nfloat.ge.1) then
    press = floats(1)
    if (nword.ge.2) then
      call stolc(words(2),maxword)
      if (index(words(2),'pa ').eq.1) then
        press = press*1.0d-9
      elseif (index(words(2),'kpa ').eq.1) then
        press = press*1.0d-6
      elseif (index(words(2),'atm').eq.1) then
        press = press*101.325d-6
      elseif (index(words(2),'nm-2').eq.1) then
        press = press*1.0d-9
      elseif (index(words(2),'kbar').eq.1) then
        press = press*0.1_dp
      endif
    endif
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.ge.1) then
      press = floats(1)
      if (nword.ge.1) then
        call stolc(words(1),maxword)
        if (index(words(1),'pa ').eq.1) then
          press = press*1.0d-9
        elseif (index(words(1),'kpa ').eq.1) then
          press = press*1.0d-6
        elseif (index(words(1),'atm').eq.1) then
          press = press*101.325d-6
        elseif (index(words(1),'nm-2').eq.1) then
          press = press*1.0d-9
        elseif (index(words(1),'kbar').eq.1) then
          press = press*0.1_dp
        endif
      endif
    else
      call outerror('Pressure value is missing from input',iline)
      call stopnow('genword')
    endif
  endif
  presscfg(ncurr) = press
!
!  Set pressure in stress tensor
!
  stresscfg(1,ncurr) = press
  stresscfg(2,ncurr) = press
  stresscfg(3,ncurr) = press
  stresscfg(4,ncurr) = 0.0_dp
  stresscfg(5,ncurr) = 0.0_dp
  stresscfg(6,ncurr) = 0.0_dp
  lwordok = .true.
  return
!****************************
!  Parameters for Wolf sum  *
!****************************
710 if (nfloat.ge.2) then
    etaw = abs(floats(1))
    cutw = abs(floats(2))
    if (nword.gt.1) then
      lwolforiginal = (index(words(2),'ori').eq.1)
      lwolffennell  = (index(words(2),'fen').eq.1)
    endif
  else
    call outerror('insufficient parameters in input for qwolf',iline)
    call stopnow('genword')
  endif
!
!  Set Wolf flag
!
  lwolf = .true.
!
!  Check that this is not a defect calculation
!
  if (ldefect) then
    call outerror('Wolf sum cannot be used in a defect calculation yet',iline)
    call stopnow('genword')
  endif
!
!  Set tweatpi
!
  tweatpi = 2.0_dp*etaw/sqrtpi
  if (abs(cutw).gt.1.0d-12) then
    rkw = 1.0_dp/cutw
    selfwolf = derfc(etaw*cutw)*rkw 
    if (lwolffennell) then
      selfwolfforce  = selfwolf*rkw + tweatpi*exp(-(etaw*cutw)**2)*rkw
      selfwolfenergy = selfwolfforce
    elseif (lwolforiginal) then
      selfwolfforce  = selfwolf*rkw + tweatpi*exp(-(etaw*cutw)**2)*rkw
      selfwolfenergy = 0.0_dp
    else
      selfwolfforce  = 0.0_dp
      selfwolfenergy = 0.0_dp
    endif
  endif
!
  lwordok = .true.
  return
!*****************
!  Radial force  *
!*****************
720 if (nfloat.ge.4) then
    radialKcfg(ncurr) = floats(1)
    radialXYZcfg(1,ncurr) = floats(2)
    radialXYZcfg(2,ncurr) = floats(3)
    radialXYZcfg(3,ncurr) = floats(4)
  else
    call outerror('insufficient parameters in input for radial_force',iline)
    call stopnow('genword')
  endif
!
!  Check that this configuration is non-periodic
!
  if (ndimen(ncurr).gt.0) then
    call outerror('Radial force cannot be used for periodic systems',iline)
    call stopnow('genword')
  endif
!
!  Set radial force flag
!
  lradialcfg(ncurr) = .true.
!
  lwordok = .true.
  return
!************************************
!  Spatial decomposition cutoff(s)  *
!************************************
730 continue
  if (nword.gt.1) then
    lrcspatial_anisotropic = (index(words(2),'ani').eq.1) 
    lrcspatialBO_anisotropic = lrcspatial_anisotropic
  endif
  if (lrcspatial_anisotropic) then
    if (nfloat.ge.6) then  
      rcspatialx   = abs(floats(1))
      rcspatialy   = abs(floats(2))
      rcspatialz   = abs(floats(3))
      rcspatialbox = abs(floats(4))
      rcspatialboy = abs(floats(5))
      rcspatialboz = abs(floats(6))
    elseif (nfloat.ge.3) then
      rcspatialx   = abs(floats(1))
      rcspatialy   = abs(floats(2))
      rcspatialz   = abs(floats(3))
      rcspatialbox = 0.0_dp
      rcspatialboy = 0.0_dp
      rcspatialboz = 0.0_dp
    endif
  else
    if (nfloat.ge.2) then  
      rcspatial   = abs(floats(1))
      rcspatialbo = abs(floats(2))
    elseif (nfloat.eq.1) then
      rcspatial   = abs(floats(1))
      rcspatialbo = rcspatial
    endif
  endif
  lwordok = .true.
  return
!************************************
!  Gasteiger convergance tolerance  *
!************************************
740 if (nfloat.gt.0) then
    gasttol = floats(1)
  else
    read(nru,*,err=99) gasttol
    iline = iline + 1
  endif
  gasttol = max(abs(gasttol),1.0d-10)
  lwordok = .true.
  return
!*******************************************
!  Gasteiger maximum number of iterations  *
!*******************************************
750 if (nfloat.gt.0) then
    ngastitermax = nint(floats(1))
  else
    read(nru,*,err=99) ngastitermax
    iline = iline + 1
  endif
  ngastitermax = max(1,abs(ngastitermax))
  lwordok = .true.
  return
!*****************************************************
!  Finite difference interval for numerical phonons  *
!*****************************************************
760 if (nfloat.gt.0) then
    phondiff = abs(floats(1))
  else
    phondiff = 1.0d-5
  endif
  lwordok = .true.
  return
!****************************
!  Parameters for Wolf sum  *
!****************************
770 if (nfloat.ge.2) then
    etaw = abs(floats(1))
    cutw = abs(floats(2))
    if (nword.gt.1) then
      lwolforiginal = (index(words(1),'ori').eq.1)
    endif
  else
    call outerror('insufficient parameters in input for qwolf',iline)
    call stopnow('genword')
  endif
!
!  Set Wolf flag
!
  lwolf = .true.
!
!  Check that this is not a defect calculation
!
  if (ldefect) then
    call outerror('Wolf sum cannot be used in a defect calculation yet',iline)
    call stopnow('genword')
  endif
!  
!  Set tweatpi
!     
  tweatpi = 2.0_dp*etaw/sqrtpi
  if (abs(cutw).gt.1.0d-10) then
    rkw = 1.0_dp/cutw
    selfwolf = derfc(etaw*cutw)*rkw
  endif
!     
  lwordok = .true.
  return
!*****************************************
!  Parameters for COSMO/COSMIC Wolf sum  *
!*****************************************
780 if (nfloat.ge.2) then
    etawc = abs(floats(1))
    cutwc = abs(floats(2))
  else
    call outerror('insufficient parameters in input for cwolf',iline)
    call stopnow('genword')
  endif
!
!  Set tweatpi
!
  tweatpic = 2.0_dp*etawc/sqrtpi
  if (abs(cutwc).gt.1.0d-10) then
    selfwolfc = derfc(etawc*cutwc)/cutwc
  endif
!
  lwordok = .true.
  return
!*****************************************************************
!  Finite difference interval for numerical property evaluation  *
!*****************************************************************
790 if (nfloat.ge.2) then
    findiffc = abs(floats(1))
    findiffs = abs(floats(2))
  elseif (nfloat.eq.1) then
    findiffc = abs(floats(1))
  endif
  lwordok = .true.
  return
!*************************
!  Anisotropic pressure  *
!*************************
800 units = 1.0_dp
  if (nword.ge.2) then
    call stolc(words(2),maxword)
    if (index(words(2),'pa ').eq.1) then
      units = 1.0d-9
    elseif (index(words(2),'kpa ').eq.1) then
      units = 1.0d-6
    elseif (index(words(2),'atm').eq.1) then
      units = 101.325d-6
    elseif (index(words(2),'nm-2').eq.1) then
      units = 1.0d-9
    elseif (index(words(2),'kbar').eq.1) then
      units = 0.1_dp
    endif
  endif
  if (ndimen(ncurr).eq.3) then
    if (nfloat.lt.6) then
      call outerror('anisotropic pressure input is incomplete',iline)
      call stopnow('genword')
    endif
    anisotropicpresscfg(1,ncurr) = floats(1)*units
    anisotropicpresscfg(2,ncurr) = floats(2)*units
    anisotropicpresscfg(3,ncurr) = floats(3)*units
    anisotropicpresscfg(4,ncurr) = floats(4)*units
    anisotropicpresscfg(5,ncurr) = floats(5)*units
    anisotropicpresscfg(6,ncurr) = floats(6)*units
  elseif (ndimen(ncurr).eq.2) then
    if (nfloat.lt.3) then
      call outerror('anisotropic pressure input is incomplete',iline)
      call stopnow('genword')
    endif
    anisotropicpresscfg(1,ncurr) = floats(1)*units
    anisotropicpresscfg(2,ncurr) = floats(2)*units
    anisotropicpresscfg(3,ncurr) = floats(3)*units
  elseif (ndimen(ncurr).eq.1) then
    if (nfloat.lt.1) then
      call outerror('anisotropic pressure input is incomplete',iline)
      call stopnow('genword')
    endif
    anisotropicpresscfg(1,ncurr) = floats(1)*units
  endif
  lanisotropicpresscfg(ncurr) = .true.
!
!  Since an anisotropic pressure has been set then we forced to use force minimisation
!
  lforcemin = .true.
!
  lwordok = .true.
  return
!******************
!  Plumed option  *
!******************
810 continue
  if (.not.lplumed_available) then
    call outerror('PLUMED run but GULP has not been built with PLUMED'//word,iline)
    call stopnow('genword')
  endif
  lplumed = .true.
  if (nword.gt.1) then
    plumedfile = words(2)
  else
    plumedfile = 'plumed.dat'
  endif
  lwordok = .true.
  return
!*******************
!  UFF bond order  *
!*******************
820 continue
  if (nword.ge.2) then
    if (nfloat.lt.1) then
      call outerror('bond order is missing for uff_bondorder option',iline)
      call stopnow('genword')
    endif
    call stolc(words(2),maxword)
    if (index(words(2),'sin').eq.1) then
      call outerror('single bond order cannot be changed',iline)
      call stopnow('genword')
    elseif (index(words(2),'dou').eq.1) then
      UFFbondorder(2) = floats(1)
    elseif (index(words(2),'tri').eq.1) then
      UFFbondorder(3) = floats(1)
    elseif (index(words(2),'qua').eq.1) then
      UFFbondorder(4) = floats(1)
    elseif (index(words(2),'res').eq.1) then
      UFFbondorder(5) = floats(1)
    elseif (index(words(2),'ami').eq.1) then
      UFFbondorder(6) = floats(1)
    elseif (index(words(2),'cus').eq.1) then
      UFFbondorder(7) = floats(1)
    endif
  else
    call outerror('bond type missing for uff_bondorder option',iline)
    call stopnow('genword')
  endif
!
  lwordok = .true.
  return
!*****************************
!  End of input for options  *
!*****************************
!
!  Error handling
!
99 call outerror('Error reading input data - check format near '//word,iline)
  call stopnow('genword')
1000 l1000 = .true.
  return
  end
