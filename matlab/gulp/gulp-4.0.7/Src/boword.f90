  subroutine boword(iin,word,lwordok,iline,line,l55,l1000,linr,lint,llibrary,lfflags)
!
!  Processes potential input for bond-order potentials
!
!  iin = input fortran channel
!
!   7/03 Created from potword21
!   4/04 Typo in call to resize boA arrays corrected
!   7/04 Bond order charge potentials added
!   8/04 Bond order charge self energy potentials added
!  10/04 Sub-option to change taper form added to bocharge option
!   2/05 Rho added for boselfenergy
!   7/05 Charge coupled potential input added
!   7/05 Brenner potential type 1 allowed and nat2REBOspecies set
!        according to the variant
!   4/06 Fluorine added for Brenner 3 model
!   8/06 ltype flags added to calls to okspec
!   8/06 iin passed to linepro
!   8/06 Separate flag added for fitting flags
!   7/07 Input of reaxFF parameters added
!   8/07 Input of reaxFF parameters modified for new ReaxFF version
!   9/07 Cutoff for bond orders added
!  11/07 Cutoff for VDW added
!  11/07 Pairwise bond penalty from MgH paper added
!  11/07 Option to input species independent reaxFF parameters added
!  11/07 Pairwise radii added to reaxff2_morse input
!  11/07 Storage in reaxFFtor4 modified to accommodate wildcard end atoms
!  11/07 Cutoff for Coulomb term added and setting of lreaxFFqreal flag
!  12/07 ReaxFF 3-body conjugation added
!  12/07 lreaxFFbocorrect added
!   1/08 lreaxFFqreal removed
!   3/08 Bond order threshold added
!   3/08 Fitting flags added for reaxFF
!   3/08 Use of abs on reaxFFmorse parameters 4-6 removed
!   4/08 Flag handling compacted into do loop
!   4/08 ncurr removed from arguments as it is unused
!   4/08 reaxFFatol added + other thresholds
!   4/08 Multiple reaxFFval3 terms for the same species added
!   4/08 abs removed from setting of reaxFFval3 values
!   6/08 ReaxFF Q shell structure arrays added
!   7/08 pval6 added to reaxFFval3 array
!  12/08 Module input renamed to gulpinput
!   1/09 lreaxFFpboOK added 
!   1/10 nboR switched for nCCspec it size check
!   3/10 Modified to use getpotsymbol routines for correct library symbol
!        handling.
!   4/10 Calls to getpotsymbol2 replaced with getpotsymbol1 for borep and boatt options
!   6/10 Modified to handle optional second atom type for bond-order
!        attractive and repulsive terms.
!   9/10 EDIP potential input added
!  10/10 EDIP linear threebody modifications added
!  10/10 Option to input increased EDIPmaxZcutoff value added
!  10/10 EDIP accuracy parameters added for tapering
!  11/10 MgH energy added
!   8/11 Check that bond order exponents are positive added
!   8/11 nreaxFFfixQspec(ptr) added
!  11/11 Fixed charged information for reaxFF now points to species rather than element
!  12/11 Modified to allow for multiple reactive species for the same element
!   4/12 External control over undercoordination added
!  12/12 reaxff_q0 input added
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
  use bondorderdata
  use brennerdata
  use chargecoupled
  use constants
  use control
  use EDIPdata
  use element,  only : maxele, reaxFFchi, reaxFFgamma, reaxFFmu, reaxFFshell, lreaxFFqfix, reaxFFqfix, lreaxFFunder, reaxFFq0
  use fitting
  use gulpinput
  use iochannels
  use parallel
  use reaxFFdata
  use shell
  use species
  implicit none
!
!  Passed variables
!
  character(len=20)                              :: word
  character(len=maxlinelength)                   :: line
  integer(i4)                                    :: iin
  integer(i4)                                    :: iline
  logical                                        :: BOcombiloc
  logical                                        :: l55
  logical                                        :: l1000
  logical                                        :: lfflags
  logical                                        :: linr
  logical                                        :: lint
  logical                                        :: llibrary
  logical                                        :: lwordok
!
!  Local variables
!
  character(len=5)                               :: sym1
  character(len=5)                               :: sym2
  character(len=5)                               :: sym3
  character(len=5)                               :: sym4
  integer(i4),                         parameter :: maxmatch = 10
  integer(i4)                                    :: i
  integer(i4)                                    :: ifl
  integer(i4)                                    :: ii
  integer(i4)                                    :: ind
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4)                                    :: itype1
  integer(i4)                                    :: itype2
  integer(i4)                                    :: itype3
  integer(i4)                                    :: itype4
  integer(i4)                                    :: maxdone
  integer(i4)                                    :: n1
  integer(i4)                                    :: n2
  integer(i4)                                    :: n3
  integer(i4)                                    :: nfit0
  integer(i4)                                    :: nm1
  integer(i4)                                    :: nm2
  integer(i4)                                    :: nm3
  integer(i4)                                    :: nm4
  integer(i4)                                    :: nmatch1
  integer(i4)                                    :: nmatch2
  integer(i4)                                    :: nmatch3
  integer(i4)                                    :: nmatch4
  integer(i4)                                    :: ns1
  integer(i4)                                    :: ns2
  integer(i4)                                    :: ns3
  integer(i4)                                    :: ns4
  integer(i4)                                    :: nsmatch1(maxmatch)
  integer(i4)                                    :: nsmatch2(maxmatch)
  integer(i4)                                    :: nsmatch3(maxmatch)
  integer(i4)                                    :: nsmatch4(maxmatch)
  integer(i4)                                    :: nbeg
  integer(i4)                                    :: nBOtaperQdef
  integer(i4)                                    :: nboA1
  integer(i4)                                    :: nboR1
  integer(i4)                                    :: nCCspec1
  integer(i4)                                    :: nrs
  integer(i4)                                    :: nval3
  integer(i4)                                    :: nvar1
  integer(i4)                                    :: nvar2
  integer(i4)                                    :: nvar3
  integer(i4)                                    :: nvar4
  integer(i4)                                    :: status
  logical,     dimension(:),   allocatable, save :: ldone
  logical,     dimension(:,:), allocatable, save :: ldone2
  logical                                        :: l2atoms
  logical                                        :: lbocorr1
  logical                                        :: lbocorr2
  logical                                        :: lmatch
  logical                                        :: lsymbol
  logical                                        :: lvalidpot
  real(dp)                                       :: units
!
!  Initialise local variables
!
  if (index(word,'bren').eq.1) goto 100
  if (index(word,'rebo').eq.1) goto 100
  if (index(word,'botw').eq.1) goto 110
  if (index(word,'bore').eq.1) goto 120
  if (index(word,'boat').eq.1) goto 130
  if (index(word,'boch').eq.1) goto 140
  if (index(word,'bose').eq.1) goto 150
  if (index(word,'cc_p').eq.1) goto 160
  if (index(word,'reaxff1_r').eq.1) goto 170
  if (index(word,'reaxff2_bo ').eq.1) goto 180
  if (index(word,'reaxfft').eq.1) goto 190
  if (index(word,'reaxff1_v').eq.1) goto 200
  if (index(word,'reaxff2_bon').eq.1) goto 210
  if (index(word,'reaxff1_o').eq.1) goto 220
  if (index(word,'reaxff1_l').eq.1) goto 230
  if (index(word,'reaxff2_o').eq.1) goto 240
  if (index(word,'reaxff1_u').eq.1) goto 250
  if (index(word,'reaxff1_a').eq.1) goto 260
  if (index(word,'reaxff3_a').eq.1) goto 270
  if (index(word,'reaxff3_p').eq.1) goto 280
  if (index(word,'reaxff4_t').eq.1) goto 290
  if (index(word,'reaxff3_h').eq.1) goto 300
  if (index(word,'reaxff2_m').eq.1) goto 310
  if (index(word,'reaxff1_mo').eq.1) goto 320
  if (index(word,'reaxff_mu').eq.1) goto 330
  if (index(word,'reaxff_c').eq.1) goto 340
  if (index(word,'reaxff_g').eq.1) goto 350
  if (index(word,'reaxffc').eq.1) goto 360
  if (index(word,'reaxffv').eq.1) goto 370
  if (index(word,'reaxff2_p').eq.1) goto 380
  if (index(word,'reaxffq').eq.1) goto 390
  if (index(word,'reaxff0_b').eq.1) goto 400
  if (index(word,'reaxff0_o').eq.1) goto 410
  if (index(word,'reaxff0_va').eq.1) goto 420
  if (index(word,'reaxff0_p').eq.1) goto 430
  if (index(word,'reaxff0_t').eq.1) goto 440
  if (index(word,'reaxff0_vd').eq.1) goto 450
  if (index(word,'reaxff0_l').eq.1) goto 460
  if (index(word,'reaxff3_c').eq.1) goto 470
  if (index(word,'reaxff_q').eq.1) goto 480
  if (index(word,'reaxff_f').eq.1) goto 490
  if (index(word,'edip_c').eq.1) goto 500
  if (index(word,'edip_tw').eq.1) goto 510
  if (index(word,'edip_th').eq.1) goto 520
  if (index(word,'edip_z').eq.1) goto 530
  if (index(word,'edip_a').eq.1) goto 540
  if (index(word,'reaxff0_m').eq.1) goto 550
  if (index(word,'reaxff1_i').eq.1) goto 560
  if (index(word,'reaxff_q0').eq.1) goto 570
  return
!**********************
!  Brenner potential  *
!**********************
100 lbrenner = .true.
  lnoanald3 = .true.
  if (nfloat.gt.0) then
    nbrennertype = abs(nint(floats(1)))
    if (nbrennertype.lt.1.or.nbrennertype.gt.3) then
      call outerror('invalid Brenner potential number',iline)
      call stopnow('boword')
    endif
    if (nbrennertype.eq.2) then
      call outerror('Brenner potential number 2 not yet implemented',iline)
      call stopnow('boword')
    endif
  endif
  if (nword.gt.1) then
    do i = 2,nword
      if (index(words(i),'nos').eq.1) then
        if (i.lt.nword) then
          if (index(words(i+1),'fh').eq.1) then
            lbrennersplinef = .false.
            lbrennersplineh = .false.
          elseif (index(words(i+1),'f').eq.1) then
            lbrennersplinef = .false.
          elseif (index(words(i+1),'h').eq.1) then
            lbrennersplineh = .false.
          endif
        else
          lbrennersplinef = .false.
          lbrennersplineh = .false.
        endif
      endif
    enddo
  endif
!
!  Set nat2REBOspecies for particular variant of the Brenner potential
!
  nat2REBOspecies(1:maxele) = 0
  if (nbrennertype.eq.1) then
    nat2REBOspecies(6) = 1
    nat2REBOspecies(1) = 2
    nat2REBOspecies(14) = 3
  elseif (nbrennertype.eq.3) then
    nat2REBOspecies(6) = 1
    nat2REBOspecies(1) = 2
    nat2REBOspecies(8) = 3
    nat2REBOspecies(9) = 4
  endif
  lwordok = .true.
  return
!******************************************************************
!  Parameters for two-body component of the bond-order potential  *
!******************************************************************
110 if (nbopot.eq.maxnbopot) then
    maxnbopot = nbopot + 10
    call changemaxnbopot
  endif
  lnoanald3 = .true.
!
  units = 1.0_dp
  BOcombiloc = .false.
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoev
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'co').eq.1) then
        BOcombiloc = .true.
      endif
      i = i + 1
    enddo
  endif
115 line = '  '
  read(iin,'(a)',end=118) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 115
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 118
    endif
  endif
  nbopot = nbopot + 1
  if (nbopot.gt.maxnbopot) then
    maxnbopot = nbopot + 10
    call changemaxnbopot
  endif
  BOcombi(nbopot) = BOcombiloc
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (BOcombiloc) then
      if (nfloat.lt.2) then
        call outerror('Incorrect input during fitting flags',iline)
        call stopnow('boword')
      endif
      do ifl = 1,2
        n1 = nint(floats(nfloat-2+ifl))
        if (n1.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 6
          nfpot(nfit) = nbopot
          nfvar(nfit) = 16 + ifl
        endif
      enddo
      nfloat = nfloat - 2
    else
      if (nfloat.lt.4) then
        call outerror('Incorrect input during fitting flags',iline)
        call stopnow('boword')
      endif
      do ifl = 1,4
        n1 = nint(floats(nfloat-4+ifl))
        if (n1.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 6
          nfpot(nfit) = nbopot
          nfvar(nfit) = ifl
        endif
      enddo
      nfloat = nfloat - 4
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nbopot = nbopot - 1
    goto 115
  endif
!
!  Assign coefficients and cutoffs
!
  if (BOcombi(nbopot)) then
    if (nfloat.ge.2) then
      BOchiR(nbopot) = floats(1+nbeg)
      BOchiA(nbopot) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('boword')
    endif
  else
    if (nfloat.ge.6) then
      BOacoeff(nbopot) = abs(floats(1+nbeg))*units
      BObcoeff(nbopot) = abs(floats(2+nbeg))*units
      BOzacoeff(nbopot) = floats(3+nbeg)
      BOzbcoeff(nbopot) = floats(4+nbeg)
      rBOmin(nbopot) = floats(5+nbeg)
      rBOmax(nbopot) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('boword')
    endif
  endif
!
  if (nvar1.eq.nvar2) then
    nBOspec1(nbopot) = nvar1
    nBOspec2(nbopot) = nvar2
    if (itype1.lt.itype2) then
      nBOtyp1(nbopot) = itype1
      nBOtyp2(nbopot) = itype2
    else
      nBOtyp1(nbopot) = itype2
      nBOtyp2(nbopot) = itype1
    endif
  elseif (nvar1.lt.nvar2) then
    nBOspec1(nbopot) = nvar1
    nBOspec2(nbopot) = nvar2
    nBOtyp1(nbopot) = itype1
    nBOtyp2(nbopot) = itype2
  else
    nBOspec1(nbopot) = nvar2
    nBOspec2(nbopot) = nvar1
    nBOtyp1(nbopot) = itype2
    nBOtyp2(nbopot) = itype1
  endif
  nBOtypeT(nbopot) = 1
  goto 115
118 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************************************
!  Parameters for bond-order for repulsive case  *
!*************************************************
120 if (nboR.eq.maxnboR) then
    maxnboR = nboR + 10
    call changemaxnbor
  endif
  lnoanald3 = .true.
!
  nboR1 = nboR + 1
  nBOtypeR(nboR1) = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'the').eq.1) then
        nBOtypeR(nboR1) = 2
      endif
      i = i + 1
    enddo
  endif
125 line = '  '
  read(iin,'(a)',end=128) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 125
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 128
    endif
  endif
  nboR = nboR + 1
  if (nboR.gt.maxnboR) then
    maxnboR = nboR + 10
    call changemaxnbor
  endif
  nBOtypeR(nboR) = nBOtypeR(nboR1)
!******************
!  Fitting flags  *
!******************
  if (nBOtypeR(nboR).eq.1) then
!
!  No theta term form
!
    if (lfit.and.lfflags) then
      if (nfloat.lt.3) then
        call outerror('Incorrect input during fitting flags',iline)
        call stopnow('boword')
      endif
      do ifl = 1,3
        n1 = nint(floats(nfloat-3+ifl))
        if (n1.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 6
          nfpot(nfit) = nboR
          nfvar(nfit) = 4 + ifl
        endif
      enddo
      nfloat = nfloat - 3
    endif
  elseif (nBOtypeR(nboR).eq.2) then
!
!  Theta term form
!
    if (lfit.and.lfflags) then
      if (nfloat.lt.6) then
        call outerror('Incorrect input during fitting flags',iline)
        call stopnow('boword')
      endif
      do ifl = 1,6
        n1 = nint(floats(nfloat-6+ifl))
        if (n1.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 6
          nfpot(nfit) = nboR
          nfvar(nfit) = 4 + ifl
        endif
      enddo
      nfloat = nfloat - 6
    endif
  endif
!
!  Process symbol input - work out whether there is 1 or 2 atoms input
!
  l2atoms = .false.
  if (nword.ge.3) then
    l2atoms = .true.
  elseif (nword.eq.2) then
    if (index(words(2),'cor').eq.0.and.index(words(2),'she').eq.0) l2atoms = .true.
  endif
  if (l2atoms) then
    call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  else
    call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,nbeg,lvalidpot)
  endif
  if (.not.lvalidpot) then
    nboR = nboR - 1
    goto 125
  endif
!
  if (l2atoms) then
    nBOspecR1(nboR) = nvar1
    nBOtypR1(nboR) = itype1
    nBOspecR2(nboR) = nvar2
    nBOtypR2(nboR) = itype2
  else
    nBOspecR1(nboR) = nvar1
    nBOtypR1(nboR) = itype1
    nBOspecR2(nboR) = nvar1
    nBOtypR2(nboR) = itype1
  endif
!
  if (nBOtypeR(nboR).eq.1) then
!*******************
!  Non-theta form  *
!*******************
!
!  Assign coefficients and cutoffs
!
    if (nfloat.ge.4) then
      BOecoeffR(nboR) = floats(1+nbeg)
      BOmcoeffR(nboR) = floats(2+nbeg)
      BOncoeffR(nboR) = floats(3+nbeg)
      BOlcoeffR(nboR) = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      BOecoeffR(nboR) = floats(1+nbeg)
      BOmcoeffR(nboR) = 3.0_dp
      BOncoeffR(nboR) = floats(2+nbeg)
      BOlcoeffR(nboR) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('boword')
    endif
  elseif (nBOtypeR(nboR).eq.2) then
!***************
!  Theta form  *
!***************
!
!  Assign coefficients and cutoffs
!
    if (nfloat.ge.7) then
      BOecoeffR(nboR) = floats(1+nbeg)
      BOmcoeffR(nboR) = floats(2+nbeg)
      BOncoeffR(nboR) = floats(3+nbeg)
      BOlcoeffR(nboR) = floats(4+nbeg)
      BOccoeffR(nboR) = floats(5+nbeg)
      BOdcoeffR(nboR) = floats(6+nbeg)
      BOhcoeffR(nboR) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      BOecoeffR(nboR) = floats(1+nbeg)
      BOmcoeffR(nboR) = 3.0_dp
      BOncoeffR(nboR) = floats(2+nbeg)
      BOlcoeffR(nboR) = floats(3+nbeg)
      BOccoeffR(nboR) = floats(4+nbeg)
      BOdcoeffR(nboR) = floats(5+nbeg)
      BOhcoeffR(nboR) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('boword')
    endif
  endif
!
  goto 125
128 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**************************************************
!  Parameters for bond-order for attractive case  *
!**************************************************
130 if (nboA.eq.maxnboA) then
    maxnboA = nboA + 10
    call changemaxnboa
  endif
  lnoanald3 = .true.
!
  nboA1 = nboA + 1
  nBOtypeA(nboA1) = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'the').eq.1) then
        nBOtypeA(nboA1) = 2
      endif
      i = i + 1
    enddo
  endif
135 line = '  '
  read(iin,'(a)',end=138) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 135
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 138
    endif
  endif
  nboA = nboA + 1
  if (nboA.gt.maxnboA) then
    maxnboA = nboA + 10
    call changemaxnboa
  endif
  nBOtypeA(nboA) = nBOtypeA(nboA1)
!******************
!  Fitting flags  *
!******************
  if (nBOtypeA(nboA).eq.1) then
!
!  No theta term form
!
    if (lfit.and.lfflags) then
      if (nfloat.lt.3) then
        call outerror('Incorrect input during fitting flags',iline)
        call stopnow('boword')
      endif
      do ifl = 1,3
        n1 = nint(floats(nfloat-3+ifl))
        if (n1.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 6
          nfpot(nfit) = nboA
          nfvar(nfit) = 10 + ifl
        endif
      enddo
      nfloat = nfloat - 3
    endif
  elseif (nBOtypeA(nboA).eq.2) then
!
!  Theta term form
!
    if (lfit.and.lfflags) then
      if (nfloat.lt.6) then
        call outerror('Incorrect input during fitting flags',iline)
        call stopnow('boword')
      endif
      do ifl = 1,6
        n1 = nint(floats(nfloat-6+ifl))
        if (n1.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 6 
          nfpot(nfit) = nboA
          nfvar(nfit) = 10 + ifl
        endif
      enddo
      nfloat = nfloat - 6
    endif
  endif
!
!  Process symbol input - work out whether there is 1 or 2 atoms input
!
  l2atoms = .false.
  if (nword.ge.3) then
    l2atoms = .true.
  elseif (nword.eq.2) then
    if (index(words(2),'cor').eq.0.and.index(words(2),'she').eq.0) l2atoms = .true.
  endif
  if (l2atoms) then
    call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  else
    call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,nbeg,lvalidpot)
  endif
  if (.not.lvalidpot) then
    nboA = nboA - 1
    goto 135
  endif
!
  if (l2atoms) then
    nBOspecA1(nboA) = nvar1
    nBOtypA1(nboA) = itype1
    nBOspecA2(nboA) = nvar2
    nBOtypA2(nboA) = itype2
  else
    nBOspecA1(nboA) = nvar1
    nBOtypA1(nboA) = itype1
    nBOspecA2(nboA) = nvar1
    nBOtypA2(nboA) = itype1
  endif
!
  if (nBOtypeA(nboA).eq.1) then
!*******************
!  Non-theta form  *
!*******************
!
!  Assign coefficients and cutoffs
!
    if (nfloat.ge.4) then
      BOecoeffA(nboA) = floats(1+nbeg)
      BOmcoeffA(nboA) = floats(2+nbeg)
      BOncoeffA(nboA) = floats(3+nbeg)
      BOlcoeffA(nboA) = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      BOecoeffA(nboA) = floats(1+nbeg)
      BOmcoeffA(nboA) = 3.0_dp
      BOncoeffA(nboA) = floats(2+nbeg)
      BOlcoeffA(nboA) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('boword')
    endif
  elseif (nBOtypeA(nboA).eq.2) then
!***************
!  Theta form  *
!***************
!
!  Assign coefficients and cutoffs
!
    if (nfloat.ge.7) then
      BOecoeffA(nboA) = floats(1+nbeg)
      BOmcoeffA(nboA) = floats(2+nbeg)
      BOncoeffA(nboA) = floats(3+nbeg)
      BOlcoeffA(nboA) = floats(4+nbeg)
      BOccoeffA(nboA) = floats(5+nbeg)
      BOdcoeffA(nboA) = floats(6+nbeg)
      BOhcoeffA(nboA) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      BOecoeffA(nboA) = floats(1+nbeg)
      BOmcoeffA(nboA) = 3.0_dp
      BOncoeffA(nboA) = floats(2+nbeg)
      BOlcoeffA(nboA) = floats(3+nbeg)
      BOccoeffA(nboA) = floats(4+nbeg)
      BOdcoeffA(nboA) = floats(5+nbeg)
      BOhcoeffA(nboA) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('boword')
    endif
  endif
!
  goto 135
138 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**********************************************
!  Parameters for bond-order charge potential *
!**********************************************
140 if (nboQ.eq.maxnboQ) then
    maxnboQ = nboQ + 10
    call changemaxnboq
  endif
!
  units = 1.0_dp
  nBOtaperQdef = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoangs
      elseif (index(words(i),'stap').eq.1) then
        nBOtaperQdef = 2
      endif
      i = i + 1
    enddo
  endif
145 line = '  '
  read(iin,'(a)',end=148) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 145
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 148
    endif
  endif
  nboQ = nboQ + 1
  if (nboQ.gt.maxnboQ) then
    maxnboQ = nboQ + 10
    call changemaxnboq
  endif
!
!  Set flags to turn off options that would not be valid with this model
!
  lnoanald3 = .true.
  lDoQDeriv1 = .true.
  lDoQDeriv2 = .true.
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nboQ = nboQ - 1
    goto 145
  endif
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.3) then
    BOq0(nboQ) = floats(1+nbeg)
    rBOminQ(nboQ) = floats(2+nbeg)*units
    rBOmaxQ(nboQ) = floats(3+nbeg)*units
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('boword')
  endif
!
  if (nvar1.eq.nvar2) then
    nBOspecQ1(nboQ) = nvar1
    nBOspecQ2(nboQ) = nvar2
    if (itype1.lt.itype2) then
      nBOtypQ1(nboQ) = itype1
      nBOtypQ2(nboQ) = itype2
    else
      nBOtypQ1(nboQ) = itype2
      nBOtypQ2(nboQ) = itype1
    endif
  elseif (nvar1.lt.nvar2) then
    nBOspecQ1(nboQ) = nvar1
    nBOspecQ2(nboQ) = nvar2
    nBOtypQ1(nboQ) = itype1
    nBOtypQ2(nboQ) = itype2
  else
    nBOspecQ1(nboQ) = nvar2
    nBOspecQ2(nboQ) = nvar1
    nBOtypQ1(nboQ) = itype2
    nBOtypQ2(nboQ) = itype1
  endif
  nBOtypeQ(nboQ) = 1
  nBOtaperQ(nboQ) = nBOtaperQdef
  goto 145
148 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***************************************************
!  Parameters for bond-order self charge potential *
!***************************************************
150 if (nboQ0.eq.maxnboQ0) then
    maxnboQ0 = nboQ0 + 10
    call changemaxnboq0
  endif
!
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'au').eq.1) then
        units = autoangs
      endif
      i = i + 1
    enddo
  endif
155 line = '  '
  read(iin,'(a)',end=158) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 155
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 158
    endif
  endif
  nboQ0 = nboQ0 + 1
  if (nboQ0.gt.maxnboQ0) then
    maxnboQ0 = nboQ0 + 10
    call changemaxnboq0
  endif
!
!  Set flags to turn off options that would not be valid with this model
!
  lnoanald3 = .true.
  lDoQDeriv1 = .true.
  lDoQDeriv2 = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nboQ0 = nboQ0 - 1
    goto 155
  endif
!
!  Assign coefficients and cutoffs
!
  if (nfloat.ge.3) then
    BOq0pot(nboQ0) = floats(1+nbeg)
    BOq0rho(nboQ0) = floats(2+nbeg)
    BOq0ref(nboQ0) = floats(3+nbeg)
  elseif (nfloat.eq.2) then
    BOq0pot(nboQ0) = floats(1+nbeg)
    BOq0ref(nboQ0) = floats(2+nbeg)
    BOq0rho(nboQ0) = 1.0_dp
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('boword')
  endif
!
  nBOspecQ0(nboQ0) = nvar1
  nBOtypQ0(nboQ0) = itype1
  nBOtypeQ0(nboQ0) = 1
  goto 155
158 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!********************************************
!  Parameters for charge coupled potential  *
!********************************************
160 if (nCCspec.eq.maxnboR) then
    maxCCspec = nCCspec + 10
    call changemaxCCspec
  endif
  lnoanald3 = .true.
!
  nCCspec1 = nCCspec + 1
  if (nfloat.gt.0) nCCparNb(nCCspec1) = nint(floats(1))
165 line = '  '
  read(iin,'(a)',end=168) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 165
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 168
    endif
  endif
  nCCspec = nCCspec + 1
  if (nCCspec.gt.maxCCspec) then
    maxCCspec = nCCspec + 10
    call changemaxCCspec
  endif
  nCCparNb(nCCspec) = nCCparNb(nCCspec1)
!******************
!  Fitting flags  *
!******************
  if (lfit.and.lfflags) then
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nCCspec = nCCspec - 1
    goto 165
  endif
!
  natCCspec(nCCspec) = nvar1
  ntypCCspec(nCCspec) = itype1
!********************
!  Read parameters  *
!********************
  if (nfloat.ge.6) then
    CCparA(nCCspec) = floats(1+nbeg)
    CCparB(nCCspec) = floats(2+nbeg)
    CCvdwC(nCCspec) = floats(3+nbeg)
    CClambda(nCCspec) = floats(4+nbeg)
    CCmu(nCCspec) = floats(5+nbeg)
    CCbeta(nCCspec) = floats(6+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('boword')
  endif
!
!  Read next line
!
  read(iin,'(a)',end=168) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.ge.6) then
    CCparN(nCCspec) = floats(1+nbeg)
    CCparM(nCCspec) = floats(2+nbeg)
    CCparC(nCCspec) = floats(3+nbeg)
    CCparD(nCCspec) = floats(4+nbeg)
    CCparH(nCCspec) = floats(5+nbeg)
    CCeta(nCCspec) = floats(6+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('boword')
  endif
!
!  Read next line
!
  read(iin,'(a)',end=168) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.ge.6) then
    CCparIE(nCCspec) = floats(1+nbeg)
    CCparAE(nCCspec) = floats(2+nbeg)
    CCparQL(nCCspec) = floats(3+nbeg)
    CCparQU(nCCspec) = floats(4+nbeg)
    CCparDL(nCCspec) = floats(5+nbeg)
    CCparDU(nCCspec) = floats(6+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('boword')
  endif
!
!  Read next line
!
  read(iin,'(a)',end=168) line
  iline = iline + 1
  call linepro(iin,line,iline)
  if (nfloat.ge.4) then
    rCCminS(nCCspec) = floats(1+nbeg)
    rCCmaxS(nCCspec) = floats(2+nbeg)
    rCCminL(nCCspec) = floats(3+nbeg)
    rCCmaxL(nCCspec) = floats(4+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('boword')
  endif
!
  goto 165
168 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**********************
!  ReaxFF bond radii  *
!**********************
170 continue
175 line = '  '
  read(iin,'(a)',end=178) line
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
  lreaxFF = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 175
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.3) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    do ifl = 1,3
      n1 = nint(floats(nfloat-3+ifl))
      if (n1.eq.1) then
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit)  = 8 
          nfpot(nfit)  = ns1
          nfvar(nfit)  = 1
          nfvar2(nfit) = ifl
          if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
            nfcon = nfcon + 1
            if (nfcon.gt.maxfcon) then
              maxfcon = maxfcon + 10
              call changemaxfcon
              nfcfix(nfcon) = nfit
              nfcvar(nfcon) = nfit - nm1 + 1
              nfcotyp(nfcon) = 1
              fconadd(nfcon) = 0.0_dp
              fconco(nfcon) = 1.0_dp
              fconpower(nfcon) = 1.0_dp
            endif
          endif
        enddo
      endif
    enddo
    nfloat = nfloat - 3
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    if (nfloat.ge.ind+3) then
      reaxFFr(1,ns1) = floats(ind+1)
      reaxFFr(2,ns1) = floats(ind+2)
      reaxFFr(3,ns1) = floats(ind+3)
    else
      call outerror('Incorrect potential input for reaxFF_radii',iline)
      call stopnow('boword')
    endif
  enddo
  goto 175
178 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*********************************
!  Parameters for the bond-order *
!*********************************
180 continue
  lbocorr1 = .false.
  lbocorr2 = .false.
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'over').eq.1) then
        lbocorr1 = .true.
      elseif (index(words(i),'bo13').eq.1) then
        lbocorr2 = .true.
      endif
      i = i + 1
    enddo
  endif
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 185
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.6) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone(maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone')
!
    do ifl = 1,6
      n1 = nint(floats(nfloat-6+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone(:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
            if (ns1.ge.ns2) then
              ind = ns1*(ns1-1)/2 + ns2
            else
              ind = ns2*(ns2-1)/2 + ns1
            endif
            if (.not.ldone(ind)) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit)  = 8
              nfpot(nfit)  = ind
              nfvar(nfit)  = 18
              nfvar2(nfit) = ifl
!
              if (nm1+nm2.gt.2) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                nfcon = nfcon + 1
                if (nfcon.gt.maxfcon) then
                  maxfcon = maxfcon + 10
                  call changemaxfcon
                  nfcfix(nfcon) = nfit
                  nfcvar(nfcon) = nfit0
                  nfcotyp(nfcon) = 1
                  fconadd(nfcon) = 0.0_dp
                  fconco(nfcon) = 1.0_dp
                  fconpower(nfcon) = 1.0_dp
                endif
              endif
              ldone(ind) = .true.
            endif
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 6
!
    deallocate(ldone,stat=status)
    if (status/=0) call deallocate_error('boword','ldone')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.6) then
        reaxFFpbo(1,ind) = floats(1+nbeg)
        reaxFFpbo(2,ind) = floats(2+nbeg)
        reaxFFpbo(3,ind) = floats(3+nbeg)
        reaxFFpbo(4,ind) = floats(4+nbeg)
        reaxFFpbo(5,ind) = floats(5+nbeg)
        reaxFFpbo(6,ind) = floats(6+nbeg)
      elseif (nfloat.ge.4) then
        reaxFFpbo(1,ind) = floats(1+nbeg)
        reaxFFpbo(2,ind) = floats(2+nbeg)
        reaxFFpbo(3,ind) = floats(3+nbeg)
        reaxFFpbo(4,ind) = floats(4+nbeg)
        reaxFFpbo(5,ind) = 0.0_dp
        reaxFFpbo(6,ind) = 0.0_dp
      elseif (nfloat.ge.2) then
        reaxFFpbo(1,ind) = floats(1+nbeg)
        reaxFFpbo(2,ind) = floats(2+nbeg)
        reaxFFpbo(3,ind) = 0.0_dp
        reaxFFpbo(4,ind) = 0.0_dp
        reaxFFpbo(5,ind) = 0.0_dp
        reaxFFpbo(6,ind) = 0.0_dp
      else
        call outerror('Incorrect reaxFF bond order input',iline)
        call stopnow('boword')
      endif
!
!  Check that exponents for bond order are negative otherwise there will be problems!
!
      if (reaxFFpbo(1,ind).gt.0.0_dp) then
        call outwarning('Exponent for ReaxFF sigma bond order is positive',iline)
      endif
      if (reaxFFpbo(3,ind).gt.0.0_dp) then
        call outwarning('Exponent for ReaxFF pi bond order is positive',iline)
      endif
      if (reaxFFpbo(5,ind).gt.0.0_dp) then
        call outwarning('Exponent for ReaxFF pi-pi bond order is positive',iline)
      endif
!
      lreaxFFbocorrect(1,ind) = lbocorr1
      lreaxFFbocorrect(2,ind) = lbocorr2
!
!  Set flag to indicate that this pair is valid
!
      lreaxFFpboOK(ind) = .true.
    enddo
  enddo
!
  goto 185
188 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**************************
!  ReaxFF drop tolerance  *
!**************************
190 continue
  if (nfloat.gt.0) then
    reaxFFtol = abs(floats(1))
    if (reaxFFtol.gt.1.0_dp) then
      call outerror('Threshold for reaxFF bond orders is crazy - should be less than 1',iline)
    elseif (reaxFFtol.gt.0.1_dp) then
      call outwarning('Threshold for reaxFF bond orders is probably too high',iline)
    endif
    if (nfloat.gt.1) then
      reaxFFatol = abs(floats(2))
      if (reaxFFatol.gt.1.0_dp) then
        call outerror('Threshold for reaxFF angle bond orders is crazy - should be less than 1',iline)
      elseif (reaxFFatol.gt.0.1_dp) then
        call outwarning('Threshold for reaxFF angle bond orders is too high',iline)
      endif
    endif
    if (nfloat.gt.2) then
      reaxFFatol2 = abs(floats(3))
      if (reaxFFatol2.gt.1.0_dp) then
        call outerror('Threshold for reaxFF angle bond order products is crazy',iline)
      elseif (reaxFFatol2.gt.0.1_dp) then
        call outwarning('Threshold for reaxFF angle bond order products is too high',iline)
      endif
    endif
    if (nfloat.gt.3) then
      reaxFFhtol = abs(floats(4))
      if (reaxFFhtol.gt.1.0_dp) then
        call outerror('Threshold for reaxFF h-bond order is crazy',iline)
      elseif (reaxFFhtol.gt.0.1_dp) then
        call outwarning('Threshold for reaxFF h-bond order is too high',iline)
      endif
    endif
    if (nfloat.gt.4) then
      reaxFFrhtol = abs(floats(5))
      if (reaxFFrhtol.lt.1.0_dp) then
        call outerror('Threshold for reaxFF h-bond distance is crazy',iline)
      endif
    endif
    if (nfloat.gt.5) then
      reaxFFatol3 = abs(floats(6))
      if (reaxFFatol3.gt.1.0_dp) then
        call outerror('Threshold for reaxFF torsion bond order products is crazy',iline)
      elseif (reaxFFatol3.gt.0.1_dp) then
        call outwarning('Threshold for reaxFF torsion bond order products is too high',iline)
      endif
    endif
  endif
  lwordok = .true.
  return
!************************
!  ReaxFF bond valence  *
!************************
200 units = 1.0_dp
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
  lreaxFF = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 205
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
!
!  Loop over matches
!
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit)  = 8
          nfpot(nfit)  = ns1
          nfvar(nfit)  = 2
          nfvar2(nfit) = ifl
!
          if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
            nfcon = nfcon + 1
            if (nfcon.gt.maxfcon) then
              maxfcon = maxfcon + 10
              call changemaxfcon
              nfcfix(nfcon) = nfit
              nfcvar(nfcon) = nfit - nm1 + 1
              nfcotyp(nfcon) = 1
              fconadd(nfcon) = 0.0_dp
              fconco(nfcon) = 1.0_dp
              fconpower(nfcon) = 1.0_dp
            endif
          endif
        enddo
      endif
    enddo
    nfloat = nfloat - 4
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
    if (nfloat.ge.ind+4) then
!
!  Valence parameters
!
!  1 => normal valence
!  2 => boc valence
!  3 => lone pair valence
!  4 => angle valence
!
      reaxFFval(1,ns1) = floats(ind+1)
      reaxFFval(2,ns1) = floats(ind+2)
      reaxFFval(3,ns1) = floats(ind+3)
      reaxFFval(4,ns1) = floats(ind+4)
    else
      call outerror('Incorrect potential input for reaxFF_valence',iline)
      call stopnow('boword')
    endif
  enddo
  goto 205
208 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**************************************
!  Parameters for ReaxFF bond energy  *
!**************************************
210 continue
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 215
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.5) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone(maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone')
!
    do ifl = 1,5
      n1 = nint(floats(nfloat-5+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone(:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
            if (ns1.ge.ns2) then
              ind = ns1*(ns1-1)/2 + ns2
            else
              ind = ns2*(ns2-1)/2 + ns1
            endif
            if (.not.ldone(ind)) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit)  = 8
              nfpot(nfit)  = ind
              nfvar(nfit)  = 16
              nfvar2(nfit) = ifl
!
              if (nm1+nm2.gt.2) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                nfcon = nfcon + 1
                if (nfcon.gt.maxfcon) then
                  maxfcon = maxfcon + 10
                  call changemaxfcon
                  nfcfix(nfcon) = nfit
                  nfcvar(nfcon) = nfit0
                  nfcotyp(nfcon) = 1
                  fconadd(nfcon) = 0.0_dp
                  fconco(nfcon) = 1.0_dp
                  fconpower(nfcon) = 1.0_dp
                endif
              endif
              ldone(ind) = .true.
            endif
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 5
!
    deallocate(ldone,stat=status)
    if (status/=0) call deallocate_error('boword','ldone')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.5) then
        reaxFFDe(1,ind)  = abs(floats(1+nbeg))*units
        reaxFFDe(2,ind)  = abs(floats(2+nbeg))*units
        reaxFFDe(3,ind)  = abs(floats(3+nbeg))*units
        reaxFFpbe(1,ind) = floats(4+nbeg)
        reaxFFpbe(2,ind) = floats(5+nbeg)
      else
        call outerror('Incorrect reaxFF bond energy input',iline)
        call stopnow('boword')
      endif
    enddo
  enddo
!
  goto 215
218 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!****************************
!  ReaxFF overcoordination  *
!****************************
220 units = 1.0_dp
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
  lreaxFF = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 225
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
!
!  Loop over matches
!
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit)  = 8
          nfpot(nfit)  = ns1
          nfvar(nfit)  = 3
          nfvar2(nfit) = ifl
!
          if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
            nfcon = nfcon + 1
            if (nfcon.gt.maxfcon) then
              maxfcon = maxfcon + 10
              call changemaxfcon
              nfcfix(nfcon) = nfit
              nfcvar(nfcon) = nfit - nm1 + 1
              nfcotyp(nfcon) = 1
              fconadd(nfcon) = 0.0_dp
              fconco(nfcon) = 1.0_dp
              fconpower(nfcon) = 1.0_dp
            endif
          endif
        enddo
      endif
    enddo
    nfloat = nfloat - 4
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
    if (nfloat.ge.ind+4) then
      reaxFFpboc(1,ns1) = floats(ind+1)
      reaxFFpboc(2,ns1) = floats(ind+2)
      reaxFFpboc(3,ns1) = floats(ind+3)
      reaxFFoc1(ns1) = floats(ind+4)
    else
      call outerror('Incorrect potential input for reaxFF1_over',iline)
      call stopnow('boword')
    endif
  enddo
  goto 225
228 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**********************
!  ReaxFF lone pairs  *
!**********************
230 units = 1.0_dp
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
  lreaxFF = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 235
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.2) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    do ifl = 1,2
      n1 = nint(floats(nfloat-2+ifl))
      if (n1.eq.1) then
!
!  Loop over matches
!
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit)  = 8
          nfpot(nfit)  = ns1
          nfvar(nfit)  = 5
          nfvar2(nfit) = ifl
!
          if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
            nfcon = nfcon + 1
            if (nfcon.gt.maxfcon) then
              maxfcon = maxfcon + 10
              call changemaxfcon
              nfcfix(nfcon) = nfit
              nfcvar(nfcon) = nfit - nm1 + 1
              nfcotyp(nfcon) = 1
              fconadd(nfcon) = 0.0_dp
              fconco(nfcon) = 1.0_dp
              fconpower(nfcon) = 1.0_dp
            endif
          endif
        enddo
      endif
    enddo
    nfloat = nfloat - 2
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
    if (nfloat.ge.ind+2) then
      reaxFFlp(1,ns1) = floats(ind+1)
      reaxFFlp(2,ns1) = floats(ind+2)*units
! Parameter below is not currently used
      reaxFFlp(3,ns1) = 0.0_dp
    else
      call outerror('Incorrect potential input for reaxFF1_lonepair',iline)
      call stopnow('boword')
    endif
  enddo
  goto 235
238 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************************************
!  Parameters for ReaxFF bond over coordination  *
!*************************************************
240 continue
  units = 1.0_dp
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 245
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone(maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone')
!
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
!
!  Loop over pairs of matches
!
      ldone(:) = .false.
      nfit0 = nfit + 1
      do nm1 = 1,nmatch1
        ns1 = nsmatch1(nm1)
        do nm2 = 1,nmatch2
          ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
          if (ns1.ge.ns2) then
            ind = ns1*(ns1-1)/2 + ns2
          else
            ind = ns2*(ns2-1)/2 + ns1
          endif
          if (.not.ldone(ind)) then
            nfit = nfit + 1
            if (nfit.ge.maxfit) then
              maxfit = nfit + 10
              call changemaxfit
            endif
            nftyp(nfit)  = 8
            nfpot(nfit)  = ind
            nfvar(nfit)  = 17
            nfvar2(nfit) = 1
!
            if (nm1+nm2.gt.2) then
!
!  Add constraint so that parameter remains the same as for the first match
!
              nfcon = nfcon + 1
              if (nfcon.gt.maxfcon) then
                maxfcon = maxfcon + 10
                call changemaxfcon
                nfcfix(nfcon) = nfit
                nfcvar(nfcon) = nfit0
                nfcotyp(nfcon) = 1
                fconadd(nfcon) = 0.0_dp
                fconco(nfcon) = 1.0_dp
                fconpower(nfcon) = 1.0_dp
              endif
            endif
            ldone(ind) = .true.
          endif
        enddo
      enddo
    endif
!
    deallocate(ldone,stat=status)
    if (status/=0) call deallocate_error('boword','ldone')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.1) then
        reaxFFoc2(ind)  = floats(1+nbeg)*units
      else
        call outerror('Incorrect pairwise reaxFF input',iline)
        call stopnow('boword')
      endif
    enddo
  enddo
!
  goto 245
248 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!******************************
!  ReaxFF under coordination  *
!******************************
250 units = 1.0_dp
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
255 line = '  '
  read(iin,'(a)',end=258) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 255
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 258
    endif
  endif
  lreaxFF = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 255
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flag
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
!
!  Loop over matches
!
      do nm1 = 1,nmatch1
        ns1 = nsmatch1(nm1)
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = ns1
        nfvar(nfit)  = 4
        nfvar2(nfit) = 1
!
        if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
          nfcon = nfcon + 1
          if (nfcon.gt.maxfcon) then
            maxfcon = maxfcon + 10
            call changemaxfcon
            nfcfix(nfcon) = nfit
            nfcvar(nfcon) = nfit - nm1 + 1
            nfcotyp(nfcon) = 1
            fconadd(nfcon) = 0.0_dp
            fconco(nfcon) = 1.0_dp
            fconpower(nfcon) = 1.0_dp
          endif
        endif
      enddo
    endif
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
    if (nfloat.ge.ind+1) then
      reaxFFuc1(ns1) = abs(floats(ind+1))*units
    else
      call outerror('Incorrect potential input for reaxFF1_under',iline)
      call stopnow('boword')
    endif
  enddo
  goto 255
258 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************
!  ReaxFF valence angle  *
!*************************
260 units = 1.0_dp
265 line = '  '
  read(iin,'(a)',end=268) line
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
  lreaxFF = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 265
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.2) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    do ifl = 1,2
      n1 = nint(floats(nfloat-2+ifl))
      if (n1.eq.1) then
!
!  Loop over matches
!
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit)  = 8
          nfpot(nfit)  = ns1
          nfvar(nfit)  = 6
          nfvar2(nfit) = ifl
!
          if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
            nfcon = nfcon + 1
            if (nfcon.gt.maxfcon) then
              maxfcon = maxfcon + 10
              call changemaxfcon
              nfcfix(nfcon) = nfit
              nfcvar(nfcon) = nfit - nm1 + 1
              nfcotyp(nfcon) = 1
              fconadd(nfcon) = 0.0_dp
              fconco(nfcon) = 1.0_dp
              fconpower(nfcon) = 1.0_dp
            endif
          endif
        enddo
      endif
    enddo
    nfloat = nfloat - 2
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
!  reaxFFval1(1,:) => p_val3
!  reaxFFval1(2,:) => p_val5
!
    if (nfloat.ge.ind+2) then
      reaxFFval1(1,ns1) = floats(ind+1)
      reaxFFval1(2,ns1) = floats(ind+2)
    else
      call outerror('Incorrect potential input for reaxFF1_lonepair',iline)
      call stopnow('boword')
    endif
  enddo
  goto 265
268 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**************************************
!  Parameters for ReaxFF angle triad  *
!**************************************
270 continue
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
275 line = '  '
  read(iin,'(a)',end=278) line
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
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 275
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Find matches for species 3 within list if they exists
!
  nmatch3 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar3,itype3,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch3 = nmatch3 + 1
      if (nmatch3.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch3(nmatch3) = nrs
    endif
  enddo
  if (nmatch3.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar3
    ntypreaxFFspec(nreaxFFspec) = itype3
    symbolreaxFFspec(nreaxFFspec) = sym3
    nmatch3 = 1
    nsmatch3(1) = nreaxFFspec
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
      do nm3 = 1,nmatch3
        ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
        if (ns2.ge.ns3) then
          ind = ns2*(ns2-1)/2 + ns3
        else
          ind = ns3*(ns3-1)/2 + ns2
        endif
!
!  Increment number of angle potentials
!
        nreaxFFval3(ind,ns1) = nreaxFFval3(ind,ns1) + 1
        if (nreaxFFval3(ind,ns1).gt.maxreaxFFval3) then
          maxreaxFFval3 = nreaxFFval3(ind,ns1)
          call changemaxreaxFFval3
        endif
        nval3 = nreaxFFval3(ind,ns1)
      enddo
    enddo
  enddo
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.6) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone2(maxdone,maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone2')
!
    do ifl = 1,6
      n1 = nint(floats(nfloat-6+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone2(:,:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
            do nm3 = 1,nmatch3
              ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
              if (ns2.ge.ns3) then
                ind = ns2*(ns2-1)/2 + ns3
              else
                ind = ns3*(ns3-1)/2 + ns2
              endif
              nval3 = nreaxFFval3(ind,ns1)
              if (.not.ldone2(ind,ns1)) then
                nfit = nfit + 1
                if (nfit.ge.maxfit) then
                  maxfit = nfit + 10
                  call changemaxfit
                endif
                nftyp(nfit)  = 8
                nfpot(nfit)  = ns1
                nfpot2(nfit) = ind
                nfpot3(nfit) = nval3
                nfvar(nfit)  = 21
                nfvar2(nfit) = ifl
!
                if (nm1+nm2+nm3.gt.3) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                  nfcon = nfcon + 1
                  if (nfcon.gt.maxfcon) then
                    maxfcon = maxfcon + 10
                    call changemaxfcon
                    nfcfix(nfcon) = nfit
                    nfcvar(nfcon) = nfit0
                    nfcotyp(nfcon) = 1
                    fconadd(nfcon) = 0.0_dp
                    fconco(nfcon) = 1.0_dp
                    fconpower(nfcon) = 1.0_dp
                  endif
                endif
                ldone2(ind,ns1) = .true.
              endif
            enddo
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 6
!
    deallocate(ldone2,stat=status)
    if (status/=0) call deallocate_error('boword','ldone2')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
      do nm3 = 1,nmatch3
        ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
        if (ns2.ge.ns3) then
          ind = ns2*(ns2-1)/2 + ns3
        else
          ind = ns3*(ns3-1)/2 + ns2
        endif
        nval3 = nreaxFFval3(ind,ns1)
!
!  Assign coefficients and cutoffs
!
!  reaxFFval3(1,:,:,:) = Theta_0_0
!  reaxFFval3(2,:,:,:) = p_val1
!  reaxFFval3(3,:,:,:) = p_val2
!  reaxFFval3(4,:,:,:) = p_val4
!  reaxFFval3(5,:,:,:) = p_val7
!  reaxFFval3(6,:,:,:) = p_val6
!
        if (nfloat.ge.6) then
          reaxFFval3(1,nval3,ind,ns1) = floats(1+nbeg)
          reaxFFval3(2,nval3,ind,ns1) = floats(2+nbeg)*units
          reaxFFval3(3,nval3,ind,ns1) = floats(3+nbeg)
          reaxFFval3(4,nval3,ind,ns1) = floats(4+nbeg)
          reaxFFval3(5,nval3,ind,ns1) = floats(5+nbeg)
          reaxFFval3(6,nval3,ind,ns1) = floats(6+nbeg)
        elseif (nfloat.eq.5) then
          reaxFFval3(1,nval3,ind,ns1) = floats(1+nbeg)
          reaxFFval3(2,nval3,ind,ns1) = floats(2+nbeg)*units
          reaxFFval3(3,nval3,ind,ns1) = floats(3+nbeg)
          reaxFFval3(4,nval3,ind,ns1) = floats(4+nbeg)
          reaxFFval3(5,nval3,ind,ns1) = floats(5+nbeg)
          reaxFFval3(6,nval3,ind,ns1) = 0.0_dp
        else
          call outerror('Incorrect triad reaxFF input',iline)
          call stopnow('boword')
        endif
      enddo
    enddo
  enddo
!
  goto 275
278 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!****************************************
!  Parameters for ReaxFF penalty triad  *
!****************************************
280 continue
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
285 line = '  '
  read(iin,'(a)',end=288) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 285
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 288
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 285
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Find matches for species 3 within list if they exists
!
  nmatch3 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar3,itype3,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch3 = nmatch3 + 1
      if (nmatch3.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch3(nmatch3) = nrs
    endif
  enddo
  if (nmatch3.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar3
    ntypreaxFFspec(nreaxFFspec) = itype3
    symbolreaxFFspec(nreaxFFspec) = sym3
    nmatch3 = 1
    nsmatch3(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone2(maxdone,maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone2')
!
    n1 = nint(floats(nfloat))
    if (n1.eq.1) then
!
!  Loop over pairs of matches
!
      ldone2(:,:) = .false.
      nfit0 = nfit + 1
      do nm1 = 1,nmatch1
        ns1 = nsmatch1(nm1)
        do nm2 = 1,nmatch2
          ns2 = nsmatch2(nm2)
          do nm3 = 1,nmatch3
            ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
            if (ns2.ge.ns3) then
              ind = ns2*(ns2-1)/2 + ns3
            else
              ind = ns3*(ns3-1)/2 + ns2
            endif
            if (.not.ldone2(ind,ns1)) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit)  = 8
              nfpot(nfit)  = ns1
              nfpot2(nfit) = ind
              nfvar(nfit)  = 22
              nfvar2(nfit) = 1
!
              if (nm1+nm2+nm3.gt.3) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                nfcon = nfcon + 1
                if (nfcon.gt.maxfcon) then
                  maxfcon = maxfcon + 10
                  call changemaxfcon
                  nfcfix(nfcon) = nfit
                  nfcvar(nfcon) = nfit0
                  nfcotyp(nfcon) = 1
                  fconadd(nfcon) = 0.0_dp
                  fconco(nfcon) = 1.0_dp
                  fconpower(nfcon) = 1.0_dp
                endif
              endif
              ldone2(ind,ns1) = .true.
            endif
          enddo
        enddo
      enddo
    endif
    nfloat = nfloat - 1
!
    deallocate(ldone2,stat=status)
    if (status/=0) call deallocate_error('boword','ldone2')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
      do nm3 = 1,nmatch3
        ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
        if (ns2.ge.ns3) then
          ind = ns2*(ns2-1)/2 + ns3
        else
          ind = ns3*(ns3-1)/2 + ns2
        endif
!
!  Assign coefficients and cutoffs
!
!  reaxFFpen3(:,:) = p_pen1
!
        if (nfloat.ge.1) then
          reaxFFpen3(ind,ns1) = floats(1+nbeg)*units
        else
          call outerror('Incorrect triad reaxFF input',iline)
          call stopnow('boword')
        endif
      enddo
    enddo
  enddo
!
  goto 285
288 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**********************************
!  Parameters for ReaxFF torsion  *
!**********************************
290 continue
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
295 line = '  '
  read(iin,'(a)',end=298) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 295
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 298
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2, &
                     nvar3,itype3,sym3,nvar4,itype4,sym4,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 295
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    if (nvar1.eq.maxele) then
      nmatch1 = 1
      nsmatch1(1) = 0
    else
      nreaxFFspec = nreaxFFspec + 1
      if (nreaxFFspec.gt.maxreaxFFspec) then
        maxreaxFFspec = nreaxFFspec
        call changemaxreaxFFspec
      endif
      natreaxFFspec(nreaxFFspec) = nvar1
      ntypreaxFFspec(nreaxFFspec) = itype1
      symbolreaxFFspec(nreaxFFspec) = sym1
      nmatch1 = 1
      nsmatch1(1) = nreaxFFspec
    endif
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Find matches for species 3 within list if they exists
!
  nmatch3 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar3,itype3,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch3 = nmatch3 + 1
      if (nmatch3.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch3(nmatch3) = nrs
    endif
  enddo
  if (nmatch3.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar3
    ntypreaxFFspec(nreaxFFspec) = itype3
    symbolreaxFFspec(nreaxFFspec) = sym3
    nmatch3 = 1
    nsmatch3(1) = nreaxFFspec
  endif
!
!  Find matches for species 4 within list if they exists
!
  nmatch4 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar4,itype4,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch4 = nmatch4 + 1
      if (nmatch4.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch4(nmatch4) = nrs
    endif
  enddo
  if (nmatch4.eq.0) then
    if (nvar4.eq.maxele) then
      nmatch4 = 1
      nsmatch4(1) = 0
    else
      nreaxFFspec = nreaxFFspec + 1
      if (nreaxFFspec.gt.maxreaxFFspec) then
        maxreaxFFspec = nreaxFFspec
        call changemaxreaxFFspec
      endif
      natreaxFFspec(nreaxFFspec) = nvar4
      ntypreaxFFspec(nreaxFFspec) = itype4
      symbolreaxFFspec(nreaxFFspec) = sym4
      nmatch4 = 1
      nsmatch4(1) = nreaxFFspec
    endif
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.5) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone2(maxdone,maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone2')
!
    do ifl = 1,5
      n1 = nint(floats(nfloat-5+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone2(:,:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
            do nm3 = 1,nmatch3
              ns3 = nsmatch3(nm3)
              do nm4 = 1,nmatch4
                ns4 = nsmatch4(nm4)
!
!  Compute index of place to store pairwise reaxFF data
!
                if (ns2.ge.ns3) then
                  ind1 = ns2*(ns2-1)/2 + ns3
                else
                  ind1 = ns3*(ns3-1)/2 + ns2
                endif
!
!  Trap wildcard ends
!
                if (ns1.eq.0.and.ns4.eq.0) then
                  ind2 = 1
                elseif (ns1.eq.0.and.ns4.ne.0.or.ns1.ne.0.and.ns4.eq.0) then
                  call outerror('Incorrect reaxFF4 input - both ends must be wildcards',iline)
                  call stopnow('boword')
                else
                  if (ns1.ge.ns4) then
                    ind2 = ns1*(ns1-1)/2 + ns4 + 1
                  else
                    ind2 = ns4*(ns4-1)/2 + ns1 + 1
                  endif
                endif
                if (.not.ldone2(ind2,ind1)) then
                  nfit = nfit + 1
                  if (nfit.ge.maxfit) then
                  maxfit = nfit + 10
                    call changemaxfit
                  endif
                  nftyp(nfit)  = 8
                  nfpot(nfit)  = ind1
                  nfpot2(nfit) = ind2
                  nfvar(nfit)  = 25
                  nfvar2(nfit) = ifl
!
                  if (nm1+nm2+nm3+nm4.gt.4) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                    nfcon = nfcon + 1
                    if (nfcon.gt.maxfcon) then
                      maxfcon = maxfcon + 10
                      call changemaxfcon
                      nfcfix(nfcon) = nfit
                      nfcvar(nfcon) = nfit0
                      nfcotyp(nfcon) = 1
                      fconadd(nfcon) = 0.0_dp
                      fconco(nfcon) = 1.0_dp
                      fconpower(nfcon) = 1.0_dp
                    endif
                  endif
                  ldone2(ind2,ind1) = .true.
                endif
              enddo
            enddo
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 5
!
    deallocate(ldone2,stat=status)
    if (status/=0) call deallocate_error('boword','ldone2')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
      do nm3 = 1,nmatch3
        ns3 = nsmatch3(nm3)
        do nm4 = 1,nmatch4
          ns4 = nsmatch4(nm4)
!
!  Compute index of place to store pairwise reaxFF data
!
          if (ns2.ge.ns3) then
            ind1 = ns2*(ns2-1)/2 + ns3
          else
            ind1 = ns3*(ns3-1)/2 + ns2
          endif
!
!  Trap wildcard ends
!
          if (ns1.eq.0.and.ns4.eq.0) then
            ind2 = 1
          elseif (ns1.eq.0.and.ns4.ne.0.or.ns1.ne.0.and.ns4.eq.0) then
            call outerror('Incorrect reaxFF4 input - both ends must be wildcards',iline)
            call stopnow('boword')
          else
            if (ns1.ge.ns4) then
              ind2 = ns1*(ns1-1)/2 + ns4 + 1
            else
              ind2 = ns4*(ns4-1)/2 + ns1 + 1
            endif
          endif
!
!  Assign coefficients and cutoffs
!
!  reaxFFtor4(1,:,:) = V1
!  reaxFFtor4(2,:,:) = V2
!  reaxFFtor4(3,:,:) = V3
!  reaxFFtor4(4,:,:) = p_tor1
!  reaxFFtor4(5,:,:) = p_cot1
!
          if (nfloat.ge.5) then
            lreaxFFtorsinput(ind2,ind1) = .true.
            reaxFFtor4(1,ind2,ind1) = floats(1+nbeg)*units
            reaxFFtor4(2,ind2,ind1) = floats(2+nbeg)*units
            reaxFFtor4(3,ind2,ind1) = floats(3+nbeg)*units
            reaxFFtor4(4,ind2,ind1) = floats(4+nbeg)
            reaxFFtor4(5,ind2,ind1) = floats(5+nbeg)*units
          else
            call outerror('Incorrect reaxFF4 input',iline)
            call stopnow('boword')
          endif
        enddo
      enddo
    enddo
  enddo
!
  goto 295
298 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!****************************************
!  Parameters for ReaxFF hydrogen bond  *
!****************************************
300 continue
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
305 line = '  '
  read(iin,'(a)',end=308) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 305
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 308
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 305
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Find matches for species 3 within list if they exists
!
  nmatch3 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar3,itype3,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch3 = nmatch3 + 1
      if (nmatch3.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch3(nmatch3) = nrs
    endif
  enddo
  if (nmatch3.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar3
    ntypreaxFFspec(nreaxFFspec) = itype3
    symbolreaxFFspec(nreaxFFspec) = sym3
    nmatch3 = 1
    nsmatch3(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone2(maxdone,maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone2')
!
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone2(:,:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
            do nm3 = 1,nmatch3
              ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
              if (ns2.ge.ns3) then
                ind = ns2*(ns2-1)/2 + ns3
              else
                ind = ns3*(ns3-1)/2 + ns2
              endif
              if (.not.ldone2(ind,ns1)) then
                nfit = nfit + 1
                if (nfit.ge.maxfit) then
                  maxfit = nfit + 10
                  call changemaxfit
                endif
                nftyp(nfit)  = 8
                nfpot(nfit)  = ns1
                nfpot2(nfit) = ns2
                nfpot3(nfit) = ns3
                nfvar(nfit)  = 24
                nfvar2(nfit) = ifl
!
                if (nm1+nm2+nm3.gt.3) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                  nfcon = nfcon + 1
                  if (nfcon.gt.maxfcon) then
                    maxfcon = maxfcon + 10
                    call changemaxfcon
                    nfcfix(nfcon) = nfit
                    nfcvar(nfcon) = nfit0
                    nfcotyp(nfcon) = 1
                    fconadd(nfcon) = 0.0_dp
                    fconco(nfcon) = 1.0_dp
                    fconpower(nfcon) = 1.0_dp
                  endif
                endif
                ldone2(ind,ns1) = .true.
              endif
            enddo
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 4
!
    deallocate(ldone2,stat=status)
    if (status/=0) call deallocate_error('boword','ldone2')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
      do nm3 = 1,nmatch3
        ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
        if (ns2.ge.ns3) then
          ind = ns2*(ns2-1)/2 + ns3
        else
          ind = ns3*(ns3-1)/2 + ns2
        endif
!
!  Assign coefficients and cutoffs
!
!  reaxFFhb3(1,:,:,:) = r(hb)
!  reaxFFhb3(2,:,:,:) = p_hb1
!  reaxFFhb3(3,:,:,:) = p_hb2
!  reaxFFhb3(4,:,:,:) = p_hb3
!
        if (nfloat.ge.4) then
          reaxFFhb3(1,ns3,ns2,ns1) = floats(1+nbeg)
          reaxFFhb3(2,ns3,ns2,ns1) = floats(2+nbeg)*units
          reaxFFhb3(3,ns3,ns2,ns1) = floats(3+nbeg)
          reaxFFhb3(4,ns3,ns2,ns1) = floats(4+nbeg)
        else
          call outerror('Incorrect reaxFF hydrogen bond input',iline)
          call stopnow('boword')
        endif
      enddo
    enddo
  enddo
!
  goto 305
308 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!************************************
!  Parameters for ReaxFF Morse VDW  *
!************************************
310 continue
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
315 line = '  '
  read(iin,'(a)',end=318) line
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 315
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
! 
!  Fitting flags
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.6) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone(maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone')
!
    do ifl = 1,6
      n1 = nint(floats(nfloat-6+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone(:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
            if (ns1.ge.ns2) then
              ind = ns1*(ns1-1)/2 + ns2
            else
              ind = ns2*(ns2-1)/2 + ns1
            endif
            if (.not.ldone(ind)) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then 
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit)  = 8
              nfpot(nfit)  = ind
              nfvar(nfit)  = 19
              nfvar2(nfit) = ifl
!
              if (nm1+nm2.gt.2) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                nfcon = nfcon + 1
                if (nfcon.gt.maxfcon) then
                  maxfcon = maxfcon + 10
                  call changemaxfcon
                  nfcfix(nfcon) = nfit
                  nfcvar(nfcon) = nfit0
                  nfcotyp(nfcon) = 1
                  fconadd(nfcon) = 0.0_dp
                  fconco(nfcon) = 1.0_dp
                  fconpower(nfcon) = 1.0_dp
                endif
              endif
              ldone(ind) = .true.
            endif
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 6
!
    deallocate(ldone,stat=status)
    if (status/=0) call deallocate_error('boword','ldone')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
!  1 = Dij
!  2 = alpha
!  3 = r0
!  4 = r_sigma
!  5 = r_pi
!  6 = r_pipi
!
      if (nfloat.ge.6) then
        reaxFFmorse(1,ind)  = abs(floats(1+nbeg))*units
        reaxFFmorse(2,ind)  = abs(floats(2+nbeg))
        reaxFFmorse(3,ind)  = abs(floats(3+nbeg))
        reaxFFmorse(4,ind)  = floats(4+nbeg)
        reaxFFmorse(5,ind)  = floats(5+nbeg)
        reaxFFmorse(6,ind)  = floats(6+nbeg)
        lreaxFFmorseinput(ind) = .true.
      elseif (nfloat.eq.5) then
        reaxFFmorse(1,ind)  = abs(floats(1+nbeg))*units
        reaxFFmorse(2,ind)  = abs(floats(2+nbeg))
        reaxFFmorse(3,ind)  = abs(floats(3+nbeg))
        reaxFFmorse(4,ind)  = floats(4+nbeg)
        reaxFFmorse(5,ind)  = floats(5+nbeg)
        reaxFFmorse(6,ind)  = -1.0_dp
        lreaxFFmorseinput(ind) = .true.
      elseif (nfloat.eq.4) then
        reaxFFmorse(1,ind)  = abs(floats(1+nbeg))*units
        reaxFFmorse(2,ind)  = abs(floats(2+nbeg))
        reaxFFmorse(3,ind)  = abs(floats(3+nbeg))
        reaxFFmorse(4,ind)  = floats(4+nbeg)
        reaxFFmorse(5,ind)  = -1.0_dp
        reaxFFmorse(6,ind)  = -1.0_dp
        lreaxFFmorseinput(ind) = .true.
      elseif (nfloat.eq.3) then
        reaxFFmorse(1,ind)  = abs(floats(1+nbeg))*units
        reaxFFmorse(2,ind)  = abs(floats(2+nbeg))
        reaxFFmorse(3,ind)  = abs(floats(3+nbeg))
        reaxFFmorse(4,ind)  = -1.0_dp
        reaxFFmorse(5,ind)  = -1.0_dp
        reaxFFmorse(6,ind)  = -1.0_dp
        lreaxFFmorseinput(ind) = .true.
      else
        call outerror('Incorrect pairwise reaxFF input',iline)
        call stopnow('boword')
      endif
    enddo
  enddo
!
  goto 315
318 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************************************
!  ReaxFF species-wise Morse parameters  *
!*****************************************
320 continue
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
  lreaxFF = .true.
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 325
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
!
!  Loop over matches
!
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit)  = 8
          nfpot(nfit)  = ns1
          nfvar(nfit)  = 7
          nfvar2(nfit) = ifl
!
          if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
            nfcon = nfcon + 1
            if (nfcon.gt.maxfcon) then
              maxfcon = maxfcon + 10
              call changemaxfcon
              nfcfix(nfcon) = nfit
              nfcvar(nfcon) = nfit - nm1 + 1
              nfcotyp(nfcon) = 1
              fconadd(nfcon) = 0.0_dp
              fconco(nfcon) = 1.0_dp
              fconpower(nfcon) = 1.0_dp
            endif
          endif
        enddo
      endif
    enddo
    nfloat = nfloat - 4
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    if (nfloat.ge.ind+4) then
      reaxFFalpha(ns1) = floats(ind+1)
      reaxFFeps(ns1) = floats(ind+2)*units
      reaxFFrvdw(ns1) = floats(ind+3)
      reaxFFgammaw(ns1) = floats(ind+4)
    else
      call outerror('Incorrect potential input for reaxFF1_gammaw',iline)
      call stopnow('boword')
    endif
  enddo
  goto 325
328 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!******************
!  ReaxFF EEM mu  *
!******************
330 continue
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
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 335
!
!  Check that element number is OK
!
  if (nvar1.gt.maxele) then
    call outerror('Invalid element number in input for reaxFF_mu',iline)
    call stopnow('boword')
  endif
!
!  Fitting flag
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = nvar1
      nfvar(nfit)  = 8
      nfvar2(nfit) = 2
    endif
  endif
  if (nfloat.ge.ind+1) then
    reaxFFmu(nvar1) = floats(ind+1)*units
  else
    call outerror('Incorrect input for reaxFF_mu',iline)
    call stopnow('boword')
  endif
  goto 335
338 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*******************
!  ReaxFF EEM chi  *
!*******************
340 continue
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
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 345
!
!  Check that element number is OK
!
  if (nvar1.gt.maxele) then
    call outerror('Invalid element number in input for reaxFF_chi',iline)
    call stopnow('boword')
  endif
!
!  Fitting flag
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = nvar1
      nfvar(nfit)  = 8
      nfvar2(nfit) = 1
    endif
  endif
  if (nfloat.ge.ind+1) then
    reaxFFchi(nvar1) = floats(ind+1)*units
  else
    call outerror('Incorrect input for reaxFF_chi',iline)
    call stopnow('boword')
  endif
  goto 345
348 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*********************
!  ReaxFF EEM gamma  *
!*********************
350 continue
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
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 355
!
!  Check that element number is OK
!
  if (nvar1.gt.maxele) then
    call outerror('Invalid element number in input for reaxFF_gamma',iline)
    call stopnow('boword')
  endif
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flag
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
!
!  Loop over matches
!
      do nm1 = 1,nmatch1
        ns1 = nsmatch1(nm1)
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = ns1
        nfvar(nfit)  = 8
        nfvar2(nfit) = 3
!
        if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
          nfcon = nfcon + 1
          if (nfcon.gt.maxfcon) then
            maxfcon = maxfcon + 10
            call changemaxfcon
            nfcfix(nfcon) = nfit
            nfcvar(nfcon) = nfit - nm1 + 1
            nfcotyp(nfcon) = 1
            fconadd(nfcon) = 0.0_dp
            fconco(nfcon) = 1.0_dp
            fconpower(nfcon) = 1.0_dp
          endif
        endif
      enddo
    endif
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
    if (nfloat.ge.ind+1) then
      reaxFFgamma(ns1) = floats(ind+1)
    else
      call outerror('Incorrect input for reaxFF_gamma',iline)
      call stopnow('boword')
    endif
  enddo
  goto 355
358 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************
!  ReaxFF cutoff *
!*****************
360 continue
  if (nfloat.gt.0) then
    reaxFFcutoff = abs(floats(1))
    if (reaxFFcutoff.lt.5.0_dp) then
      call outwarning('Tolerance for reaxFF is probably dangerously too low',iline)
    endif
    if (nfloat.gt.1) then
      reaxFFtapermin = abs(floats(2))
    else
      reaxFFtapermin = 0.9_dp*reaxFFcutoff
    endif
  endif
  lwordok = .true.
  return
!*********************
!  ReaxFF VDW cutoff *
!*********************
370 continue
  if (nfloat.gt.0) then
    reaxFFcutoffVDW = abs(floats(1))
  endif
  lwordok = .true.
  return
!*********************************
!  Parameters for the bond-order *
!*********************************
380 continue
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 385
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
! 
!  Fitting flags
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.3) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone(maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone')
!
    do ifl = 1,3
      n1 = nint(floats(nfloat-3+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone(:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
            if (ns1.ge.ns2) then
              ind = ns1*(ns1-1)/2 + ns2
            else
              ind = ns2*(ns2-1)/2 + ns1
            endif
            if (.not.ldone(ind)) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then 
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit)  = 8
              nfpot(nfit)  = ind
              nfvar(nfit)  = 20
              nfvar2(nfit) = ifl
!
              if (nm1+nm2.gt.2) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                nfcon = nfcon + 1
                if (nfcon.gt.maxfcon) then
                  maxfcon = maxfcon + 10
                  call changemaxfcon
                  nfcfix(nfcon) = nfit
                  nfcvar(nfcon) = nfit0
                  nfcotyp(nfcon) = 1
                  fconadd(nfcon) = 0.0_dp
                  fconco(nfcon) = 1.0_dp
                  fconpower(nfcon) = 1.0_dp
                endif
              endif
              ldone(ind) = .true.
            endif
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 3
!
    deallocate(ldone,stat=status)
    if (status/=0) call deallocate_error('boword','ldone')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.3) then
        reaxFFpen2(1,ind) = floats(1+nbeg)*units
        reaxFFpen2(2,ind) = floats(2+nbeg)
        reaxFFpen2(3,ind) = floats(3+nbeg)
      else
        call outerror('Incorrect reaxFF bond penalty input',iline)
        call stopnow('boword')
      endif
    enddo
  enddo
!
  goto 385
388 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************
!  ReaxFF Coulomb cutoff *
!*************************
390 continue
  if (nfloat.gt.0) then
    reaxFFcutoffQ = abs(floats(1))
  endif
  lwordok = .true.
  return
!******************************************************
!  ReaxFF species independent parameters for bonding  *
!******************************************************
400 continue
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.2) then
      call outerror('Incorrect reaxff0_bond input',iline)
      call stopnow('boword')
    endif
    do ifl = 1,2
      n1 = nint(floats(nfloat-2+ifl))
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = 1
        nfvar(nfit)  = 9
        nfvar2(nfit) = ifl
      endif
    enddo
    nfloat = nfloat - 2
  endif
  if (nfloat.ge.2) then
    reaxFFlam(1) = floats(1)
    reaxFFlam(2) = floats(2)
  else
    call outerror('Incorrect reaxff0_bond input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!**********************************************************************
!  ReaxFF species independent parameters for over/under coordination  *
!**********************************************************************
410 continue
!
!  Fitting flags
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.5) then
      call outerror('Incorrect reaxff0_over input',iline)
      call stopnow('boword')
    endif
    do ifl = 1,5
      n1 = nint(floats(nfloat-5+ifl))
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = 1
        nfvar(nfit)  = 10
        nfvar2(nfit) = ifl
      endif
    enddo
    nfloat = nfloat - 5
  endif
  if (nfloat.ge.5) then
    reaxFFlam(6)  = floats(1)
    reaxFFlam(31) = floats(2)
    reaxFFlam(7)  = floats(3)
    reaxFFlam(8)  = floats(4)
    reaxFFlam(9)  = floats(5)
  else
    call outerror('Incorrect reaxff0_over input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!******************************************************
!  ReaxFF species independent parameters for valence  *
!******************************************************
420 continue
!
!  Fitting flags
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect reaxff0_val input',iline)
      call stopnow('boword')
    endif
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = 1
        nfvar(nfit)  = 11
        nfvar2(nfit) = ifl
      endif
    enddo
    nfloat = nfloat - 4
  endif
  if (nfloat.ge.4) then
    reaxFFlam(15) = floats(1)
    reaxFFlam(16) = floats(2)
    reaxFFlam(17) = floats(3)
    reaxFFlam(18) = floats(4)
  else
    call outerror('Incorrect reaxff0_valence input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!******************************************************
!  ReaxFF species independent parameters for penalty  *
!******************************************************
430 continue
!
!  Fitting flags
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.3) then
      call outerror('Incorrect reaxff0_pen input',iline)
      call stopnow('boword')
    endif
    do ifl = 1,3 
      n1 = nint(floats(nfloat-3+ifl))
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = 1
        nfvar(nfit)  = 12
        nfvar2(nfit) = ifl
      endif
    enddo
    nfloat = nfloat - 3
  endif
  if (nfloat.ge.3) then
    reaxFFlam(20) = floats(1)
    reaxFFlam(21) = floats(2)
    reaxFFlam(22) = floats(3)
  else
    call outerror('Incorrect reaxff0_penalty input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!******************************************************
!  ReaxFF species independent parameters for torsions *
!******************************************************
440 continue
!
!  Fitting flags
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect reaxff0_tors input',iline)
      call stopnow('boword')
    endif
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = 1
        nfvar(nfit)  = 13
        nfvar2(nfit) = ifl
      endif
    enddo
    nfloat = nfloat - 4
  endif
  if (nfloat.ge.4) then
    reaxFFlam(24) = floats(1)
    reaxFFlam(25) = floats(2)
    reaxFFlam(26) = floats(3)
    reaxFFlam(27) = floats(4)
  else
    call outerror('Incorrect reaxff0_torsion input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!**************************************************
!  ReaxFF species independent parameters for VDW  *
!**************************************************
450 continue
!
!  Fitting flag
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect reaxff0_vdw input',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = 1
      nfvar(nfit)  = 14
      nfvar2(nfit) = 1
    endif
  endif
  if (nfloat.ge.1) then
    reaxFFlam(28) = floats(1)
  else
    call outerror('Incorrect reaxff0_vdw input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!*******************************************************
!  ReaxFF species independent parameters for lonepairs *
!*******************************************************
460 continue
!
!  Fitting flag
!   
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect reaxff0_lp input',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = 1
      nfvar(nfit)  = 15
      nfvar2(nfit) = 1
    endif
  endif
  if (nfloat.ge.1) then
    reaxFFlam(29) = floats(1)
  else
    call outerror('Incorrect reaxff0_lonepair input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!**************************************************
!  Parameters for ReaxFF 3-body conjugation term  *
!**************************************************
470 continue
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
475 line = '  '
  read(iin,'(a)',end=478) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 475
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 478
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 475
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar2,itype2,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar2
    ntypreaxFFspec(nreaxFFspec) = itype2
    symbolreaxFFspec(nreaxFFspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nreaxFFspec
  endif
!
!  Find matches for species 3 within list if they exists
!
  nmatch3 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar3,itype3,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch3 = nmatch3 + 1
      if (nmatch3.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch3(nmatch3) = nrs
    endif
  enddo
  if (nmatch3.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar3
    ntypreaxFFspec(nreaxFFspec) = itype3
    symbolreaxFFspec(nreaxFFspec) = sym3
    nmatch3 = 1
    nsmatch3(1) = nreaxFFspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone2(maxdone,maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone2')
!
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone2(:,:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
            do nm3 = 1,nmatch3
              ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
              if (ns2.ge.ns3) then
                ind = ns2*(ns2-1)/2 + ns3
              else
                ind = ns3*(ns3-1)/2 + ns2
              endif
              if (.not.ldone2(ind,ns1)) then
                nfit = nfit + 1
                if (nfit.ge.maxfit) then
                  maxfit = nfit + 10
                  call changemaxfit
                endif
                nftyp(nfit)  = 8
                nfpot(nfit)  = ns1
                nfpot2(nfit) = ind
                nfvar(nfit)  = 23
                nfvar2(nfit) = ifl
!
                if (nm1+nm2+nm3.gt.3) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                  nfcon = nfcon + 1
                  if (nfcon.gt.maxfcon) then
                    maxfcon = maxfcon + 10
                    call changemaxfcon
                    nfcfix(nfcon) = nfit
                    nfcvar(nfcon) = nfit0
                    nfcotyp(nfcon) = 1
                    fconadd(nfcon) = 0.0_dp
                    fconco(nfcon) = 1.0_dp
                    fconpower(nfcon) = 1.0_dp
                  endif
                endif
                ldone2(ind,ns1) = .true.
              endif
            enddo
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 4
!
    deallocate(ldone2,stat=status)
    if (status/=0) call deallocate_error('boword','ldone2')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
      do nm3 = 1,nmatch3
        ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
        if (ns2.ge.ns3) then
          ind = ns2*(ns2-1)/2 + ns3
        else
          ind = ns3*(ns3-1)/2 + ns2
        endif
!
!  Assign coefficients and cutoffs
!
!  reaxFFconj3(1,:,:) = p_coa1
!  reaxFFconj3(2,:,:) = p_coa2
!  reaxFFconj3(3,:,:) = p_coa3
!  reaxFFconj3(4,:,:) = p_coa4
!
        if (nfloat.ge.4) then
          reaxFFconj3(1,ind,ns1) = floats(1+nbeg)*units
          reaxFFconj3(2,ind,ns1) = floats(2+nbeg)
          reaxFFconj3(3,ind,ns1) = floats(3+nbeg)
          reaxFFconj3(4,ind,ns1) = floats(4+nbeg)
        else
          call outerror('Incorrect triad reaxFF input',iline)
          call stopnow('boword')
        endif
      enddo
    enddo
  enddo
!
  goto 475
478 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!**********************
!  ReaxFF EEM qshell  *
!**********************
480 continue
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
485 line = '  '
  read(iin,'(a)',end=488) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 485
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 488
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 485
!
!  Check that element number is OK
!
  if (nvar1.gt.maxele) then
    call outerror('Invalid element number in input for reaxFF_qshell',iline)
    call stopnow('boword')
  endif
!
!  Fitting flag
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.3) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat-2))
    n2 = nint(floats(nfloat-1))
    n3 = nint(floats(nfloat))
    nfloat = nfloat - 3
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = nvar1
      nfvar(nfit)  = 8
      nfvar2(nfit) = 4
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = nvar1
      nfvar(nfit)  = 8
      nfvar2(nfit) = 5
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = nvar1
      nfvar(nfit)  = 8
      nfvar2(nfit) = 6
    endif
  endif
  if (nfloat.ge.ind+3) then
    reaxFFshell(1,nvar1) = floats(ind+1)*units
    reaxFFshell(2,nvar1) = floats(ind+2)
    reaxFFshell(3,nvar1) = floats(ind+3)
  else
    call outerror('Incorrect input for reaxFF_qshell',iline)
    call stopnow('boword')
  endif
  goto 485
488 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!***********************
!  ReaxFF EEM fixed Q  *
!***********************
490 continue
495 line = '  '
  read(iin,'(a)',end=498) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 495
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 498
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 495
!
!  Check that element number is OK
!
  if (nvar1.gt.maxele) then
    call outerror('Invalid element number in input for reaxFF_fixq',iline)
    call stopnow('boword')
  endif
!
!  Find species matches within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nreaxFFspec
    if (lmatch(nvar1,itype1,natreaxFFspec(nrs),ntypreaxFFspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species hasn't been found add to the list
!
  if (nmatch1.eq.0) then
    nreaxFFspec = nreaxFFspec + 1
    if (nreaxFFspec.gt.maxreaxFFspec) then
      maxreaxFFspec = nreaxFFspec
      call changemaxreaxFFspec
    endif
    natreaxFFspec(nreaxFFspec) = nvar1
    ntypreaxFFspec(nreaxFFspec) = itype1
!
!  Save symbol
!
    symbolreaxFFspec(nreaxFFspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nreaxFFspec
  endif
!
!  Fitting flag
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
!
!  Loop over matches
!
      do nm1 = 1,nmatch1
        ns1 = nsmatch1(nm1)
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit)  = 8
        nfpot(nfit)  = ns1
        nfvar(nfit)  = 8
        nfvar2(nfit) = 7
!
        if (nm1.gt.1) then
!
!  Add constraint so that parameter remains the same as for the first match
!
          nfcon = nfcon + 1
          if (nfcon.gt.maxfcon) then
            maxfcon = maxfcon + 10
            call changemaxfcon
            nfcfix(nfcon) = nfit
            nfcvar(nfcon) = nfit - nm1 + 1
            nfcotyp(nfcon) = 1
            fconadd(nfcon) = 0.0_dp
            fconco(nfcon) = 1.0_dp
            fconpower(nfcon) = 1.0_dp
          endif
        endif
      enddo
    endif
  endif
!
!  Loop over matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
!
    if (nfloat.ge.ind+1) then
      lreaxFFqfix(ns1) = .true.
      reaxFFqfix(ns1) = floats(ind+1)
      nreaxFFfixQspec = nreaxFFfixQspec + 1
      if (nreaxFFfixQspec.gt.maxreaxFFfixQspec) then
        maxreaxFFfixQspec = nreaxFFfixQspec + 5
        call changemaxreaxFFfixQspec
      endif
      nreaxFFfixQspecptr(nreaxFFfixQspec) = ns1
    else
      call outerror('Incorrect input for reaxFF_fixq',iline)
      call stopnow('boword')
    endif
  enddo
  goto 495
498 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!****************************************
!  Parameters EDIP coordination number  *
!****************************************
500 continue
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 505
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nEDIPspec
    if (lmatch(nvar1,itype1,natEDIPspec(nrs),ntypEDIPspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nEDIPspec = nEDIPspec + 1
    if (nEDIPspec.gt.maxEDIPspec) then
      maxEDIPspec = nEDIPspec
      call changemaxEDIPspec
    endif
    natEDIPspec(nEDIPspec) = nvar1
    ntypEDIPspec(nEDIPspec) = itype1
    symbolEDIPspec(nEDIPspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nEDIPspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nEDIPspec
    if (lmatch(nvar2,itype2,natEDIPspec(nrs),ntypEDIPspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nEDIPspec = nEDIPspec + 1
    if (nEDIPspec.gt.maxEDIPspec) then
      maxEDIPspec = nEDIPspec
      call changemaxEDIPspec
    endif
    natEDIPspec(nEDIPspec) = nvar2
    ntypEDIPspec(nEDIPspec) = itype2
    symbolEDIPspec(nEDIPspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nEDIPspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.4) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone(maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone')
!
    do ifl = 1,4
      n1 = nint(floats(nfloat-4+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone(:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
            if (ns1.ge.ns2) then
              ind = ns1*(ns1-1)/2 + ns2
            else
              ind = ns2*(ns2-1)/2 + ns1
            endif
            if (.not.ldone(ind)) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit)  = 9
              nfpot(nfit)  = ind
              nfvar(nfit)  = 1
              nfvar2(nfit) = ifl
!
              if (nm1+nm2.gt.2) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                nfcon = nfcon + 1
                if (nfcon.gt.maxfcon) then
                  maxfcon = maxfcon + 10
                  call changemaxfcon
                  nfcfix(nfcon) = nfit
                  nfcvar(nfcon) = nfit0
                  nfcotyp(nfcon) = 1
                  fconadd(nfcon) = 0.0_dp
                  fconco(nfcon) = 1.0_dp
                  fconpower(nfcon) = 1.0_dp
                endif
              endif
              ldone(ind) = .true.
            endif
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 4
!
    deallocate(ldone,stat=status)
    if (status/=0) call deallocate_error('boword','ldone')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.8) then
        EDIPalpha(ind) = floats(1+nbeg)
        EDIPflow(ind)  = floats(2+nbeg)
        EDIPfhigh(ind) = floats(3+nbeg)
        EDIPZdih(ind)  = floats(4+nbeg)
        EDIPZrep(ind)  = floats(5+nbeg)
        EDIPc0(ind)    = floats(6+nbeg)
        EDIPplow(ind)  = floats(7+nbeg)
        EDIPphigh(ind) = floats(8+nbeg)
      else
        call outerror('Incorrect EDIP coordination input',iline)
        call stopnow('boword')
      endif
!
!  Set flag to indicate that this pair is valid
!
      lEDIPpairOK(ind) = .true.
    enddo
  enddo
  lEDIP = .true.
!
  goto 505
508 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************************************
!  Parameters EDIP twobody contribution  *
!*****************************************
510 continue
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
!
!  Process symbol input
!
  call getpotsymbol2(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 515
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nEDIPspec
    if (lmatch(nvar1,itype1,natEDIPspec(nrs),ntypEDIPspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nEDIPspec = nEDIPspec + 1
    if (nEDIPspec.gt.maxEDIPspec) then
      maxEDIPspec = nEDIPspec
      call changemaxEDIPspec
    endif
    natEDIPspec(nEDIPspec) = nvar1
    ntypEDIPspec(nEDIPspec) = itype1
    symbolEDIPspec(nEDIPspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nEDIPspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nEDIPspec
    if (lmatch(nvar2,itype2,natEDIPspec(nrs),ntypEDIPspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nEDIPspec = nEDIPspec + 1
    if (nEDIPspec.gt.maxEDIPspec) then
      maxEDIPspec = nEDIPspec
      call changemaxEDIPspec
    endif
    natEDIPspec(nEDIPspec) = nvar2
    ntypEDIPspec(nEDIPspec) = itype2
    symbolEDIPspec(nEDIPspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nEDIPspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.6) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone(maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone')
!
    do ifl = 1,6
      n1 = nint(floats(nfloat-6+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone(:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
            if (ns1.ge.ns2) then
              ind = ns1*(ns1-1)/2 + ns2
            else
              ind = ns2*(ns2-1)/2 + ns1
            endif
            if (.not.ldone(ind)) then
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit)  = 9
              nfpot(nfit)  = ind
              nfvar(nfit)  = 2
              nfvar2(nfit) = ifl
!
              if (nm1+nm2.gt.2) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                nfcon = nfcon + 1
                if (nfcon.gt.maxfcon) then
                  maxfcon = maxfcon + 10
                  call changemaxfcon
                  nfcfix(nfcon) = nfit
                  nfcvar(nfcon) = nfit0
                  nfcotyp(nfcon) = 1
                  fconadd(nfcon) = 0.0_dp
                  fconco(nfcon) = 1.0_dp
                  fconpower(nfcon) = 1.0_dp
                endif
              endif
              ldone(ind) = .true.
            endif
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 6
!
    deallocate(ldone,stat=status)
    if (status/=0) call deallocate_error('boword','ldone')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
!
!  Compute index of place to store pairwise reaxFF data
!
      if (ns1.ge.ns2) then
        ind = ns1*(ns1-1)/2 + ns2
      else
        ind = ns2*(ns2-1)/2 + ns1
      endif
!
!  Assign coefficients and cutoffs
!
      if (nfloat.ge.6) then
        EDIP2epsilon(ind) = floats(1+nbeg)
        EDIP2B(ind)       = floats(2+nbeg)
        EDIP2beta(ind)    = floats(3+nbeg)
        EDIP2sigma(ind)   = floats(4+nbeg)
        EDIP2a(ind)       = floats(5+nbeg)
        EDIP2aprime(ind)  = floats(6+nbeg)
      else
        call outerror('Incorrect EDIP twobody input',iline)
        call stopnow('boword')
      endif
!
!  Set flag to indicate that this pair is valid
!
      lEDIPpairOK(ind) = .true.
    enddo
  enddo
!
  goto 515
518 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*******************************************
!  Parameters EDIP threebody contribution  *
!*******************************************
520 continue
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
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) goto 525
!
!  Find matches for species 1 within list if they exists
!
  nmatch1 = 0
  do nrs = 1,nEDIPspec
    if (lmatch(nvar1,itype1,natEDIPspec(nrs),ntypEDIPspec(nrs),.false.)) then
      nmatch1 = nmatch1 + 1
      if (nmatch1.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch1(nmatch1) = nrs
    endif
  enddo
!
!  If species haven't been found add to the list
!
  if (nmatch1.eq.0) then
    nEDIPspec = nEDIPspec + 1
    if (nEDIPspec.gt.maxEDIPspec) then
      maxEDIPspec = nEDIPspec
      call changemaxEDIPspec
    endif
    natEDIPspec(nEDIPspec) = nvar1
    ntypEDIPspec(nEDIPspec) = itype1
    symbolEDIPspec(nEDIPspec) = sym1
    nmatch1 = 1
    nsmatch1(1) = nEDIPspec
  endif
!
!  Find matches for species 2 within list if they exists
!
  nmatch2 = 0
  do nrs = 1,nEDIPspec
    if (lmatch(nvar2,itype2,natEDIPspec(nrs),ntypEDIPspec(nrs),.false.)) then
      nmatch2 = nmatch2 + 1
      if (nmatch2.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch2(nmatch2) = nrs
    endif
  enddo
  if (nmatch2.eq.0) then
    nEDIPspec = nEDIPspec + 1
    if (nEDIPspec.gt.maxEDIPspec) then
      maxEDIPspec = nEDIPspec
      call changemaxEDIPspec
    endif
    natEDIPspec(nEDIPspec) = nvar2
    ntypEDIPspec(nEDIPspec) = itype2
    symbolEDIPspec(nEDIPspec) = sym2
    nmatch2 = 1
    nsmatch2(1) = nEDIPspec
  endif
!
!  Find matches for species 3 within list if they exists
!
  nmatch3 = 0
  do nrs = 1,nEDIPspec
    if (lmatch(nvar3,itype3,natEDIPspec(nrs),ntypEDIPspec(nrs),.false.)) then
      nmatch3 = nmatch3 + 1
      if (nmatch3.gt.maxmatch) then
        call outerror('maxmatch parameter exceeded - increase and recompile',iline)
        call stopnow('boword')
      endif
      nsmatch3(nmatch3) = nrs
    endif
  enddo
  if (nmatch3.eq.0) then
    nEDIPspec = nEDIPspec + 1
    if (nEDIPspec.gt.maxEDIPspec) then
      maxEDIPspec = nEDIPspec
      call changemaxEDIPspec
    endif
    natEDIPspec(nEDIPspec) = nvar3
    ntypEDIPspec(nEDIPspec) = itype3
    symbolEDIPspec(nEDIPspec) = sym3
    nmatch3 = 1
    nsmatch3(1) = nEDIPspec
  endif
!
!  Fitting flags
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.7) then
      call outerror('Incorrect input during fitting flags',iline)
      call stopnow('boword')
    endif
!
    maxdone = nreaxFFspec*(nreaxFFspec+1)/2
    allocate(ldone2(maxdone,maxdone),stat=status)
    if (status/=0) call outofmemory('boword','ldone2')
!
    do ifl = 1,7
      n1 = nint(floats(nfloat-7+ifl))
      if (n1.eq.1) then
!
!  Loop over pairs of matches
!
        ldone2(:,:) = .false.
        nfit0 = nfit + 1
        do nm1 = 1,nmatch1
          ns1 = nsmatch1(nm1)
          do nm2 = 1,nmatch2
            ns2 = nsmatch2(nm2)
            do nm3 = 1,nmatch3
              ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
              if (ns2.ge.ns3) then
                ind = ns2*(ns2-1)/2 + ns3
              else
                ind = ns3*(ns3-1)/2 + ns2
              endif
              if (.not.ldone2(ind,ns1)) then
                nfit = nfit + 1
                if (nfit.ge.maxfit) then
                  maxfit = nfit + 10
                  call changemaxfit
                endif
                nftyp(nfit)  = 9
                nfpot(nfit)  = ns1
                nfpot2(nfit) = ind
                nfvar(nfit)  = 3
                nfvar2(nfit) = ifl
!
                if (nm1+nm2+nm3.gt.3) then
!
!  Add constraint so that parameter remains the same as for the first match
!
                  nfcon = nfcon + 1
                  if (nfcon.gt.maxfcon) then
                    maxfcon = maxfcon + 10
                    call changemaxfcon
                    nfcfix(nfcon) = nfit
                    nfcvar(nfcon) = nfit0
                    nfcotyp(nfcon) = 1
                    fconadd(nfcon) = 0.0_dp
                    fconco(nfcon) = 1.0_dp
                    fconpower(nfcon) = 1.0_dp
                  endif
                endif
                ldone2(ind,ns1) = .true.
              endif
            enddo
          enddo
        enddo
      endif
    enddo
    nfloat = nfloat - 7
!
    deallocate(ldone2,stat=status)
    if (status/=0) call deallocate_error('boword','ldone2')
  endif
!
!  Loop over pairs of matches
!
  do nm1 = 1,nmatch1
    ns1 = nsmatch1(nm1)
    do nm2 = 1,nmatch2
      ns2 = nsmatch2(nm2)
      do nm3 = 1,nmatch3
        ns3 = nsmatch3(nm3)
!
!  Compute index of place to store pairwise reaxFF data
!
        if (ns2.ge.ns3) then
          ind = ns2*(ns2-1)/2 + ns3
        else
          ind = ns3*(ns3-1)/2 + ns2
        endif
!
!  Assign coefficients and cutoffs
!
        if (nfloat.ge.7) then
          EDIP3lambda0(ind,ns1) = floats(1+nbeg)
          EDIP3lambdap(ind,ns1) = floats(2+nbeg)
          EDIP3Z0(ind,ns1)      = floats(3+nbeg)
          EDIP3gamma0(ind,ns1)  = floats(4+nbeg)
          EDIP3gammap(ind,ns1)  = floats(5+nbeg)
          EDIP3q(ind,ns1)       = floats(6+nbeg)
          EDIP3kq2(ind,ns1)     = floats(7+nbeg)
        else
          call outerror('Incorrect EDIP threebody input',iline)
          call stopnow('boword')
        endif
!
!  Set flag to indicate that this triad is valid
!
        lEDIPtriadOK(ind,ns1) = .true.
      enddo
    enddo
  enddo
!
  goto 525
528 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*************************************
!  EDIP maximum Z value for cut-off  *
!*************************************
530 continue
  if (nfloat.ge.1) then
    EDIPmaxZcutoff = abs(floats(1))
  else
    call outerror('Maximum Z value missing from EDIP input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!*****************************
!  EDIP accuracy parameters  *
!*****************************
540 continue
  if (nfloat.ge.2) then
    EDIPaccuracy1 = abs(floats(1))
    EDIPaccuracy2 = abs(floats(2))
    if (EDIPaccuracy1.lt.EDIPaccuracy2) then
      call outerror('Accuracy1 is greater than accuracy2 in EDIP input',iline)
      call stopnow('boword')
    endif
!
!  Convert values to 1/ln(A) values
!
    EDIPaccuracy1drmax = 1.0_dp/log(EDIPaccuracy1)
    EDIPaccuracy2drmax = 1.0_dp/log(EDIPaccuracy2)
  else
    call outerror('Accuracy values missing from EDIP input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!***************************************************
!  ReaxFF species independent parameters for Mg-H  *
!***************************************************
550 continue
!
!  Fitting flag
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.1) then
      call outerror('Incorrect reaxff0_mgh input',iline)
      call stopnow('boword')
    endif
    n1 = nint(floats(nfloat))
    nfloat = nfloat - 1
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit)  = 8
      nfpot(nfit)  = 1
      nfvar(nfit)  = 15
      nfvar2(nfit) = 1
    endif
  endif
  if (nfloat.ge.1) then
    reaxFFlam(14) = floats(1)
  else
    call outerror('Incorrect reaxff0_mgh input',iline)
    call stopnow('boword')
  endif
  lwordok = .true.
  return
!******************************************
!  ReaxFF Include Undercoordination Flag  *
!******************************************
560 continue
565 line = '  '
  read(iin,'(a)',end=568) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 565
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 568
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 565
!
!  Check that element number is OK
!
  if (nvar1.gt.maxele) then
    call outerror('Invalid element number in input for reaxFF1_include_under',iline)
    call stopnow('boword')
  endif
!
!  Check flag is one or zero
!
  if (nfloat.gt.0) then
    ii = nint(floats(1))
    if (ii.ne.0.and.ii.ne.1) then
      call outerror('Invalid flag in input for reaxFF1_include_under',iline)
      call stopnow('boword')
    endif
    if (ii.eq.1) then
      lreaxFFunder(nvar1) = .true.
    else
      lreaxFFunder(nvar1) = .false.
    endif
  endif
  goto 565
568 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!******************
!  ReaxFF EEM q0  *
!******************
570 continue
575 line = '  '
  read(iin,'(a)',end=578) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 575
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 578
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol1(iline,llibrary,nvar1,itype1,sym1,0_i4,ind,lvalidpot)
  if (.not.lvalidpot) goto 575
!
!  Check that element number is OK
!
  if (nvar1.gt.maxele) then
    call outerror('Invalid element number in input for reaxFF_mu',iline)
    call stopnow('boword')
  endif
!
!  Fitting flag
!
  if (nfloat.ge.ind+1) then
    reaxFFq0(nvar1) = floats(ind+1)
  else
    call outerror('Incorrect input for reaxFF_q0',iline)
    call stopnow('boword')
  endif
  goto 575
578 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!
  end
