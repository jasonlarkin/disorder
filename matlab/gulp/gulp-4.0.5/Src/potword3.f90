  subroutine potword3(iin,word,lwordok,iline,line,l55,l1000,linr,lint,llibrary,lfflags)
!
!  Processes potential input for three body potentials
!
!  iin = input fortran channel number
!  
!  llibrary = if .true. then this is a library read call and potentials
!             are checked for whether species are present before adding
!
!  Molecular Mechanics:
!  --------------------
!
!  mmtexc specifies molecular mechanics type for three-body
!  potential types:
!
!    0 => bonded and nonbonded potential (default)
!    1 => bonded only potential
!
!  option word "molmec" after potential name selects default MM type
!  for marvin compatibility
!
!   1/95 Intra/inter-molecular facility now added for 3-body potentials
!   2/95 K3 and K4 terms added for harmonic
!   2/95 Exponential and SW three-body forms added
!   2/95 Bonded vs nonbonded types added for 3 body terms
!   3/95 bcross - cross bond potential added
!   3/96 Urey-Bradley potential added
!   4/96 Vessal form of exponential three body added
!   6/96 General value of theta0 added to SW3
!   3/97 itype1/itype2 => itype2/itype3 error fixed
!   3/98 Cosine based three-body term added
!   4/98 Assignment of species type for library call changed
!        so that potential file form is accepted
!   6/98 Murrell-Mottram potential added
!  10/98 Bond-angle cross potential added
!  10/98 Codes for fitting variables simplified
!   8/99 Linear-threebody potential added
!   5/01 Reading of cut-offs for sw3 now done even for bonded
!        potential as they are parameters of the potential
!   5/01 Option for different rhos in sw3 added
!   5/01 Minimum cut-off distances added for potentials
!   6/01 Initialisation of line added for benefit of some compilers
!  10/02 bcoscross potential added
!  10/02 Error in order of reallocation and nthrty assignments corrected
!   9/04 Jiang & Brown modification of Stillinger-Weber potential added
!  10/04 Calls to getpotsymbol introduced to reduce code size
!  10/05 Hydrogen-bond three-body potential added
!  12/05 Equatorial ESFF potential added
!   8/06 iin passed to linepro
!   8/06 Separate flag added for fitting flags
!   8/06 Bonding type sub-option added
!   9/06 Threebody theta taper added - currently only for hydrogen-bond
!   1/07 Uff3 potential added
!   1/07 Amide bond type added
!   5/07 ltdreiding added for hydrogen bonding potential
!   7/07 Modified to handle n3botype being different for each bond to pivot atom
!   7/07 Handling of n3botype added when ends of potential are swapped
!   9/07 Bug in address of ltdreiding fixed
!   6/08 Sub-option for bond number check added
!   6/08 Out of bounds addressing due to n3bondnono corrected
!  11/08 BAcoscross form added
!  11/08 Unit conversion corrected for second K in bacross potential
!  12/08 Module input renamed to gulpinput
!   3/08 3coulomb potential added
!   6/09 Module name changed from three to m_three
!   7/09 exp2 potential added
!  11/09 Initialisation of mmtexc added
!   3/10 Arguments to getpotsymbol3 modified to reflect the fact that species 1
!        is now a returned as well as 2 and 3
!   5/10 g3coulomb potential added
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, May 2010
!
  use constants
  use control
  use element, only : maxele
  use fitting
  use general, only : nwarn
  use gulpinput
  use iochannels
  use m_three
  use molecule
  use parallel
  use species
  implicit none
!
!  Passed variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: iin
  integer(i4)                  :: iline
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
  character(len=5)             :: sym1
  character(len=5)             :: sym2
  character(len=5)             :: sym3
  integer(i4)                  :: i
  integer(i4)                  :: ii
  integer(i4)                  :: itype1
  integer(i4)                  :: itype2
  integer(i4)                  :: itype3
  integer(i4)                  :: n1
  integer(i4)                  :: n2
  integer(i4)                  :: n3
  integer(i4)                  :: n4
  integer(i4)                  :: n5
  integer(i4)                  :: nbeg
  integer(i4)                  :: nbondtypein1
  integer(i4)                  :: nbondtypein2
  integer(i4)                  :: nfit0
  integer(i4)                  :: nfloatptr
  integer(i4)                  :: npot1
  integer(i4)                  :: nt
  integer(i4)                  :: ntmp
  integer(i4)                  :: nvar1
  integer(i4)                  :: nvar2
  integer(i4)                  :: nvar3
  logical                      :: lcosine
  logical                      :: lexpo
  logical                      :: lfound
  logical                      :: lk3
  logical                      :: lk4
  logical                      :: lsymbol
  logical                      :: lvalidpot
  logical                      :: lvessal
  logical                      :: lwarnp
  real(dp)                     :: units
  real(dp)                     :: var1
!
!  Initialise local variables
!
  lwarnp = (.not.lmol)
  if (index(word,'thre').eq.1) goto 310
  if (index(word,'axil').eq.1) goto 320
  if (index(word,'expo').eq.1) goto 330
  if (index(word,'stil').eq.1) goto 340
  if (index(word,'sw3 ').eq.1) goto 340
  if (index(word,'bcro').eq.1) goto 350
  if (index(word,'urey').eq.1) goto 360
  if (index(word,'murr').eq.1) goto 370
  if (index(word,'bacr').eq.1) goto 380
  if (index(word,'lin3').eq.1) goto 390
  if (index(word,'bcos').eq.1) goto 400
  if (index(word,'sw3j').eq.1) goto 410
  if (index(word,'hydr').eq.1) goto 420
  if (index(word,'equa').eq.1) goto 430
  if (index(word,'uff3').eq.1) goto 440
  if (index(word,'baco').eq.1) goto 450
  if (index(word,'3cou').eq.1) goto 460
  if (index(word,'exp2').eq.1) goto 470
  if (index(word,'g3co').eq.1) goto 480
  return
!****************************
!  Three-body interactions  *
!****************************
310 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lcosine = .false.
  lexpo = .false.
  lvessal = .false.
  lfound = .false.
  lk3 = .false.
  lk4 = .false.
  thrho1(nthb+1) = 0.0_dp
  thrho2(nthb+1) = 0.0_dp
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'cos').eq.1) then
        lcosine = .true.
      elseif (index(words(i),'exp').eq.1) then
        lexpo = .true.
      elseif (index(words(i),'ves').eq.1) then
        lvessal = .true.
        lexpo = .true.
      elseif (index(words(i),'k3').eq.1) then
        lk3 = .true.
      elseif (index(words(i),'k4').eq.1) then
        lk4 = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
  if (lexpo) then
!*******************************************
!  Exponentially decaying three-body term  *
!*******************************************
312 line = '  '
    read(iin,'(a)',end=318) line
    iline = iline + 1
    call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 312
    if (nword.gt.0) then
      word = words(1)(1:20)
      call worsy(word,lsymbol,.true.)
      if (.not.lsymbol) then
        l55 = .true.
        goto 318
      endif
    endif
    nthb = nthb + 1
    if (nthb.gt.maxthb) then
      maxthb = nthb + 10
      call changemaxthb
    endif
    nthrty(nthb) = 2
    if (lvessal) nthrty(nthb) = 8
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
        nftyp(nfit) = 3
        nfpot(nfit) = nthb
        nfvar(nfit) = 1
      endif
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 3
        nfpot(nfit) = nthb
        nfvar(nfit) = 2
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nthb = nthb - 1
      nfit = nfit0
      goto 312
    endif
!
!  Assign coefficients and cutoffs
!
    mmtexc(nthb) = mmtexc(npot1)
    n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
    n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
    if (n3bondnono(1,nthb).gt.0) then
      n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
    endif
    if (n3bondnono(2,nthb).gt.0) then
      n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
    endif
    lthetataper(nthb) = lthetataper(npot1)
    ltdreiding(nthb+1) = ltdreiding(npot1)
    thetatapermax(nthb) = thetatapermax(npot1)
    thetatapermin(nthb) = thetatapermin(npot1)
    if (mmtexc(nthb).eq.1) then
!
!  If bonded type then no cutoffs needed
!
      if (nfloat.gt.4) nfloat = nfloat - 2
      if (nfloat.ge.4) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(4+nbeg)
      elseif (nfloat.eq.3) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(3+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword3')
      endif
    else
      if (nfloat.gt.10) nfloat = nfloat - 2
      if (nfloat.ge.10) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(4+nbeg)
        thr1min(nthb) = floats(5+nbeg)
        thr1(nthb) = floats(6+nbeg)
        thr2min(nthb) = floats(7+nbeg)
        thr2(nthb) = floats(8+nbeg)
        thr3min(nthb) = floats(9+nbeg)
        thr3(nthb) = floats(10+nbeg)
      elseif (nfloat.eq.9) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(4+nbeg)
        thr1min(nthb) = floats(5+nbeg)
        thr1(nthb) = floats(6+nbeg)
        thr2min(nthb) = floats(7+nbeg)
        thr2(nthb) = floats(8+nbeg)
        thr3(nthb) = floats(9+nbeg)
      elseif (nfloat.eq.8) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(4+nbeg)
        thr1min(nthb) = floats(5+nbeg)
        thr1(nthb) = floats(6+nbeg)
        thr2(nthb) = floats(7+nbeg)
        thr3(nthb) = floats(8+nbeg)
      elseif (nfloat.eq.7) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(4+nbeg)
        thr1(nthb) = floats(5+nbeg)
        thr2(nthb) = floats(6+nbeg)
        thr3(nthb) = floats(7+nbeg)
      elseif (nfloat.eq.6) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(4+nbeg)
        thr1(nthb) = floats(5+nbeg)
        thr2(nthb) = floats(5+nbeg)
        thr3(nthb) = floats(6+nbeg)
      elseif (nfloat.eq.5) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)
        thrho1(nthb) = floats(3+nbeg)
        thrho2(nthb) = floats(3+nbeg)
        thr1(nthb) = floats(4+nbeg)
        thr2(nthb) = floats(4+nbeg)
        thr3(nthb) = floats(5+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword3')
      endif
    endif
!
!  Check that theta0 is not equal to 180 degrees otherwise
!  functional form is invalid!
!
    if (lvessal) then
      if (abs(theta(nthb)-180.0_dp).lt.1.0d-8) then
        call outerror('theta0 for Vessal 3-body is too close to 180',iline)
        call stopnow('potword3')
      endif
    endif
!
!  Assign pivot atom parameters
!
    ntspec1(nthb) = nvar1
    ntptyp1(nthb) = itype1
    symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
    if (nvar2.eq.nvar3) then
      ntspec2(nthb) = nvar2
      ntspec3(nthb) = nvar3
      if (itype2.lt.itype3) then
        ntptyp2(nthb) = itype2
        ntptyp3(nthb) = itype3
        symbol3(2,nthb) = sym2
        symbol3(3,nthb) = sym3
        if (lfit) then
          if (n3.eq.1) then
            nfit = nfit + 1
            if (nfit.ge.maxfit) then
              maxfit = nfit + 10
              call changemaxfit
            endif
            nftyp(nfit) = 3
            nfpot(nfit) = nthb
            nfvar(nfit) = 3
          endif
          if (n4.eq.1) then
            nfit = nfit + 1
            if (nfit.ge.maxfit) then
              maxfit = nfit + 10
              call changemaxfit
            endif
            nftyp(nfit) = 3
            nfpot(nfit) = nthb
            nfvar(nfit) = 4
          endif
        endif
      else
        ntptyp2(nthb) = itype3
        ntptyp3(nthb) = itype2
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
        symbol3(2,nthb) = sym3
        symbol3(3,nthb) = sym2
!
        ntmp = n3botype(1,1,nthb)
        n3botype(1,1,nthb) = n3botype(1,2,nthb)
        n3botype(1,2,nthb) = ntmp
        ntmp = n3botype(2,1,nthb)
        n3botype(2,1,nthb) = n3botype(2,2,nthb)
        n3botype(2,2,nthb) = ntmp
!
        if (lfit) then
          if (n3.eq.1) then
            nfit = nfit + 1
            if (nfit.ge.maxfit) then
              maxfit = nfit + 10
              call changemaxfit
            endif
            nftyp(nfit) = 3
            nfpot(nfit) = nthb
            nfvar(nfit) = 4
          endif
          if (n4.eq.1) then
            nfit = nfit + 1
            if (nfit.ge.maxfit) then
              maxfit = nfit + 10
              call changemaxfit
            endif
            nftyp(nfit) = 3
            nfpot(nfit) = nthb
            nfvar(nfit) = 3
          endif
        endif
        var1 = thrho1(nthb)
        thrho1(nthb) = thrho2(nthb)
        thrho2(nthb) = var1
      endif
    elseif (nvar2.lt.nvar3) then
      ntspec2(nthb) = nvar2
      ntspec3(nthb) = nvar3
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
      if (lfit) then
        if (n3.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 3
        endif
        if (n4.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 4
        endif
      endif
    else
      ntspec2(nthb) = nvar3
      ntspec3(nthb) = nvar2
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      if (lfit) then
        if (n3.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 4
        endif
        if (n4.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 3
        endif
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
    endif
    goto 312
  else
!***************************************
!  Standard or cosine three-body term  *
!***************************************
317   line = '  '
    read(iin,'(a)',end=318) line
    iline = iline + 1
    call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 317
    if (nword.gt.0) then
      word = words(1)(1:20)
      call worsy(word,lsymbol,.true.)
      if (.not.lsymbol) then
        l55 = .true.
        goto 318
      endif
    endif
    nthb = nthb + 1
    if (nthb.gt.maxthb) then
      maxthb = nthb + 10
      call changemaxthb
    endif
    if (lcosine) then
      nthrty(nthb) = 9
    else
      nthrty(nthb) = 1
    endif
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
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 1
        endif
        if (n2.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 4
        endif
        if (n3.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 3
        endif
        if (n4.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
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
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 1
        endif
        if (n2.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 4
        endif
        if (n3.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
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
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 1
        endif
        if (n2.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 3
        endif
        if (n3.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
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
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 1
        endif
        if (n2.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 3
          nfpot(nfit) = nthb
          nfvar(nfit) = 2
        endif
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nthb = nthb - 1
      nfit = nfit0
      goto 317
    endif
!
!  Assign coefficients and cutoffs
!
    mmtexc(nthb) = mmtexc(npot1)
    n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
    n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
    if (n3bondnono(1,nthb).gt.0) then
      n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
    endif
    if (n3bondnono(2,nthb).gt.0) then
      n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
    endif
    lthetataper(nthb) = lthetataper(npot1)
    ltdreiding(nthb) = ltdreiding(npot1)
    thetatapermax(nthb) = thetatapermax(npot1)
    thetatapermin(nthb) = thetatapermin(npot1)
    if (lk3.and.lk4) then
      if (mmtexc(nthb).eq.1) then
        if (nfloat.gt.4) nfloat = nfloat - 4
        if (nfloat.ge.4) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          thrho1(nthb) = floats(3+nbeg)*units
          theta(nthb) = floats(4+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      else
        if (nfloat.gt.10) nfloat = nfloat - 4
        if (nfloat.ge.10) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          thrho1(nthb) = floats(3+nbeg)*units
          theta(nthb) = floats(4+nbeg)
          thr1min(nthb) = floats(5+nbeg)
          thr1(nthb) = floats(6+nbeg)
          thr2min(nthb) = floats(7+nbeg)
          thr2(nthb) = floats(8+nbeg)
          thr3min(nthb) = floats(9+nbeg)
          thr3(nthb) = floats(10+nbeg)
        elseif (nfloat.eq.9) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          thrho1(nthb) = floats(3+nbeg)*units
          theta(nthb) = floats(4+nbeg)
          thr1min(nthb) = floats(5+nbeg)
          thr1(nthb) = floats(6+nbeg)
          thr2min(nthb) = floats(7+nbeg)
          thr2(nthb) = floats(8+nbeg)
          thr3(nthb) = floats(9+nbeg)
        elseif (nfloat.eq.8) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          thrho1(nthb) = floats(3+nbeg)*units
          theta(nthb) = floats(4+nbeg)
          thr1min(nthb) = floats(5+nbeg)
          thr1(nthb) = floats(6+nbeg)
          thr2(nthb) = floats(7+nbeg)
          thr3(nthb) = floats(8+nbeg)
        elseif (nfloat.eq.7) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          thrho1(nthb) = floats(3+nbeg)*units
          theta(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2(nthb) = floats(6+nbeg)
          thr3(nthb) = floats(7+nbeg)
        elseif (nfloat.eq.6) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          thrho1(nthb) = floats(3+nbeg)*units
          theta(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2(nthb) = floats(5+nbeg)
          thr3(nthb) = floats(6+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      endif
    elseif (lk3) then
      if (mmtexc(nthb).eq.1) then
        if (nfloat.gt.3) nfloat = nfloat - 3
        if (nfloat.ge.3) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      else
        if (nfloat.gt.9) nfloat = nfloat - 3
        if (nfloat.ge.9) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1min(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2min(nthb) = floats(6+nbeg)
          thr2(nthb) = floats(7+nbeg)
          thr3min(nthb) = floats(8+nbeg)
          thr3(nthb) = floats(9+nbeg)
        elseif (nfloat.eq.8) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1min(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2min(nthb) = floats(6+nbeg)
          thr2(nthb) = floats(7+nbeg)
          thr3(nthb) = floats(8+nbeg)
        elseif (nfloat.eq.7) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1min(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2(nthb) = floats(6+nbeg)
          thr3(nthb) = floats(7+nbeg)
        elseif (nfloat.eq.6) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1(nthb) = floats(4+nbeg)
          thr2(nthb) = floats(5+nbeg)
          thr3(nthb) = floats(6+nbeg)
        elseif (nfloat.eq.5) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho2(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1(nthb) = floats(4+nbeg)
          thr2(nthb) = floats(4+nbeg)
          thr3(nthb) = floats(5+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      endif
    elseif (lk4) then
      if (mmtexc(nthb).eq.1) then
        if (nfloat.gt.3) nfloat = nfloat - 3
        if (nfloat.ge.3) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho1(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      else
        if (nfloat.gt.9) nfloat = nfloat - 3
        if (nfloat.ge.9) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho1(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1min(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2min(nthb) = floats(6+nbeg)
          thr2(nthb) = floats(7+nbeg)
          thr3min(nthb) = floats(8+nbeg)
          thr3(nthb) = floats(9+nbeg)
        elseif (nfloat.eq.8) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho1(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1min(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2min(nthb) = floats(6+nbeg)
          thr2(nthb) = floats(7+nbeg)
          thr3(nthb) = floats(8+nbeg)
        elseif (nfloat.eq.7) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho1(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1min(nthb) = floats(4+nbeg)
          thr1(nthb) = floats(5+nbeg)
          thr2(nthb) = floats(6+nbeg)
          thr3(nthb) = floats(7+nbeg)
        elseif (nfloat.eq.6) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho1(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1(nthb) = floats(4+nbeg)
          thr2(nthb) = floats(5+nbeg)
          thr3(nthb) = floats(6+nbeg)
        elseif (nfloat.eq.5) then
          thbk(nthb) = floats(1+nbeg)*units
          thrho1(nthb) = floats(2+nbeg)*units
          theta(nthb) = floats(3+nbeg)
          thr1(nthb) = floats(4+nbeg)
          thr2(nthb) = floats(4+nbeg)
          thr3(nthb) = floats(5+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      endif
    else
      if (mmtexc(nthb).eq.1) then
        if (nfloat.gt.2) nfloat = nfloat - 2
        thrho1(nthb) = 0.0_dp
        if (nfloat.ge.2) then
          thbk(nthb) = floats(1+nbeg)*units
          theta(nthb) = floats(2+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      else
        if (nfloat.gt.8) nfloat = nfloat-2
        thrho1(nthb) = 0.0_dp
        if (nfloat.ge.8) then
          thbk(nthb) = floats(1+nbeg)*units
          theta(nthb) = floats(2+nbeg)
          thr1min(nthb) = floats(3+nbeg)
          thr1(nthb) = floats(4+nbeg)
          thr2min(nthb) = floats(5+nbeg)
          thr2(nthb) = floats(6+nbeg)
          thr3min(nthb) = floats(7+nbeg)
          thr3(nthb) = floats(8+nbeg)
        elseif (nfloat.eq.7) then
          thbk(nthb) = floats(1+nbeg)*units
          theta(nthb) = floats(2+nbeg)
          thr1min(nthb) = floats(3+nbeg)
          thr1(nthb) = floats(4+nbeg)
          thr2min(nthb) = floats(5+nbeg)
          thr2(nthb) = floats(6+nbeg)
          thr3(nthb) = floats(7+nbeg)
        elseif (nfloat.eq.6) then
          thbk(nthb) = floats(1+nbeg)*units
          theta(nthb) = floats(2+nbeg)
          thr1min(nthb) = floats(3+nbeg)
          thr1(nthb) = floats(4+nbeg)
          thr2(nthb) = floats(5+nbeg)
          thr3(nthb) = floats(6+nbeg)
        elseif (nfloat.eq.5) then
          thbk(nthb) = floats(1+nbeg)*units
          theta(nthb) = floats(2+nbeg)
          thr1(nthb) = floats(3+nbeg)
          thr2(nthb) = floats(4+nbeg)
          thr3(nthb) = floats(5+nbeg)
        elseif (nfloat.eq.4) then
          thbk(nthb) = floats(1+nbeg)*units
          theta(nthb) = floats(2+nbeg)
          thr1(nthb) = floats(3+nbeg)
          thr2(nthb) = floats(3+nbeg)
          thr3(nthb) = floats(4+nbeg)
        else
          call outerror('Incorrect potential coefficient input',iline)
          call stopnow('potword3')
        endif
      endif
    endif
!
!  Assign pivot atom parameters
!
    ntspec1(nthb) = nvar1
    ntptyp1(nthb) = itype1
    symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
    if (nvar2.eq.nvar3) then
      ntspec2(nthb) = nvar2
      ntspec3(nthb) = nvar3
      if (itype2.lt.itype3) then
        ntptyp2(nthb) = itype2
        ntptyp3(nthb) = itype3
        symbol3(2,nthb) = sym2
        symbol3(3,nthb) = sym3
      else
        ntptyp2(nthb) = itype3
        ntptyp3(nthb) = itype2
        symbol3(2,nthb) = sym3
        symbol3(3,nthb) = sym2
!
        ntmp = n3botype(1,1,nthb)
        n3botype(1,1,nthb) = n3botype(1,2,nthb)
        n3botype(1,2,nthb) = ntmp
        ntmp = n3botype(2,1,nthb)
        n3botype(2,1,nthb) = n3botype(2,2,nthb)
        n3botype(2,2,nthb) = ntmp
!
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    elseif (nvar2.lt.nvar3) then
      ntspec2(nthb) = nvar2
      ntspec3(nthb) = nvar3
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntspec2(nthb) = nvar3
      ntspec3(nthb) = nvar2
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
    goto 317
!
!  End of if over three-body potential type
!
  endif
318 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!******************************************
!  Axilrod - Teller three-body potential  *
!******************************************
320 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
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
    word=words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55=.true.
      goto 328
    endif
  endif
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 3
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 325
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.1) then
      thbk(nthb) = floats(1+nbeg)*units
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.7) then
      thbk(nthb) = floats(1+nbeg)*units
      thr1min(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2min(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3min(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      thr1min(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2min(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      thbk(nthb) = floats(1+nbeg)*units
      thr1min(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(4+nbeg)
      thr3(nthb) = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      thbk(nthb) = floats(1+nbeg)*units
      thr1(nthb) = floats(2+nbeg)
      thr2(nthb) = floats(3+nbeg)
      thr3(nthb) = floats(4+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 325
328 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!*************************************
!  Exponential three-body potential  *
!*************************************
330 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 4
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 335
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.4) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
    elseif (nfloat.eq.3) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.10) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1min(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2min(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3min(nthb) = floats(9+nbeg)
      thr3(nthb) = floats(10+nbeg)
    elseif (nfloat.eq.9) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1min(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2min(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1min(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = 0.0_dp
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 335
338 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!*********************
!  Stillinger-Weber  *
!*********************
340 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 5
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 345
  endif
!
!  Assign coefficients and cutoffs - note cut-offs needed
!  even when bonded potential since these are also 
!  effectively parameters
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (nfloat.ge.10) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thr1min(nthb) = floats(5+nbeg)
    thr1(nthb) = floats(6+nbeg)
    thr2min(nthb) = floats(7+nbeg)
    thr2(nthb) = floats(8+nbeg)
    thr3min(nthb) = floats(9+nbeg)
    thr3(nthb) = floats(10+nbeg)
  elseif (nfloat.eq.9) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thr1min(nthb) = floats(5+nbeg)
    thr1(nthb) = floats(6+nbeg)
    thr2min(nthb) = floats(7+nbeg)
    thr2(nthb) = floats(8+nbeg)
    thr3(nthb) = floats(9+nbeg)
  elseif (nfloat.eq.8) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thr1min(nthb) = floats(5+nbeg)
    thr1(nthb) = floats(6+nbeg)
    thr2(nthb) = floats(7+nbeg)
    thr3(nthb) = floats(8+nbeg)
  elseif (nfloat.eq.7) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thr1(nthb) = floats(5+nbeg)
    thr2(nthb) = floats(6+nbeg)
    thr3(nthb) = floats(7+nbeg)
  elseif (nfloat.eq.6) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = thrho1(nthb)
    thr1(nthb) = floats(4+nbeg)
    thr2(nthb) = floats(5+nbeg)
    thr3(nthb) = floats(6+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword3')
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 345
348 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!***********
!  Bcross  *
!***********
350 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 6
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
      nfit=nfit+1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit=nfit+1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n3.eq.1) then
      nfit=nfit+1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 355
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.3) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(3+nbeg)
    elseif (nfloat.ge.2) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.9) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      thr1min(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2min(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3min(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      thr1min(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2min(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      thr1min(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    elseif (nfloat.ge.5) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(4+nbeg)
      thr3(nthb) = floats(5+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
  endif
  goto 355
358 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!*****************************
!  Urey - Bradley potential  *
!*****************************
360 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 7
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 365
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.2) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.8) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2min(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3min(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2min(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(4+nbeg)
      thr3(nthb) = floats(5+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
  endif
  goto 365
368 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!******************************
!  Murrell-Mottram potential  *
!******************************
370 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 10
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 5
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 375
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.5) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.11) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3min(nthb) = floats(10+nbeg)
      thr3(nthb) = floats(11+nbeg)
    elseif (nfloat.eq.10) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3(nthb) = floats(10+nbeg)
    elseif (nfloat.eq.9) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Read in coefficient line
!
376 line = '  '
  read(iin,'(a)',end=378) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 376
!
!  Assign fitting flags and check that enough numbers are present
!
  if (lfit.and.lfflags) then
    if (nfloat.lt.22) then
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
    do ii = 1,11
      if (nint(floats(nfloat-11+ii)).eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 3
        nfpot(nfit) = nthb
        nfvar(nfit) = 5 + ii
      endif
    enddo
    nfloat = nfloat - 11
  else
    if (nfloat.lt.11) then
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Read in coefficients
!
  do ii = 1,11
    threepoly(ii,nthb) = floats(ii)
  enddo
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 375
378 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!************
!  BAcross  *
!************
380 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 11
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 5
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 385
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.5) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
    elseif (nfloat.ge.4) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      theta(nthb) = floats(4+nbeg)
    elseif (nfloat.ge.3) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(2+nbeg)
      theta(nthb) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.11) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3min(nthb) = floats(10+nbeg)
      thr3(nthb) = floats(11+nbeg)
    elseif (nfloat.eq.10) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3(nthb) = floats(10+nbeg)
    elseif (nfloat.eq.9) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      theta(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(2+nbeg)
      theta(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
      var1 = thbk(nthb)
      thbk(nthb) = thrho3(nthb)
      thrho3(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
    var1 = thbk(nthb)
    thbk(nthb) = thrho3(nthb)
    thrho3(nthb) = var1
  endif
  goto 385
388 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!*************************************
!  ESFF linear three-body potential  *
!*************************************
390 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 12
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 395
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.3) then
      thbk(nthb) = floats(1+nbeg)*units
      if (floats(2+nbeg).lt.0.0_dp) then
        theta(nthb) = - 1.0_dp
      else
        theta(nthb) = 1.0_dp
      endif
      thrho1(nthb) = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      thbk(nthb) = floats(1+nbeg)*units
      if (floats(2+nbeg).lt.0.0_dp) then
        theta(nthb) = -1.0_dp
      else
        theta(nthb) = 1.0_dp
      endif
      thrho1(nthb) = 1.0_dp
    elseif (nfloat.eq.1) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = 1.0_dp
      thrho1(nthb) = 1.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.9) then
      thbk(nthb) = floats(1+nbeg)*units
      if (floats(2+nbeg).lt.0.0_dp) then
        theta(nthb) = - 1.0_dp
      else
        theta(nthb) = 1.0_dp
      endif
      thrho1(nthb) = floats(3+nbeg)
      thr1min(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2min(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3min(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      if (floats(2+nbeg).lt.0.0_dp) then
        theta(nthb) = - 1.0_dp
      else
        theta(nthb) = 1.0_dp
      endif
      thrho1(nthb) = floats(3+nbeg)
      thr1min(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2min(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      if (floats(2+nbeg).lt.0.0_dp) then
        theta(nthb) = -1.0_dp
      else
        theta(nthb) = 1.0_dp
      endif
      thrho1(nthb) = floats(3+nbeg)
      thr1min(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      if (floats(2+nbeg).lt.0.0_dp) then
        theta(nthb) = -1.0_dp
      else
        theta(nthb) = 1.0_dp
      endif
      thrho1(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      thbk(nthb) = floats(1+nbeg)*units
      if (floats(2+nbeg).lt.0.0_dp) then
        theta(nthb) = -1.0_dp
      else
        theta(nthb) = 1.0_dp
      endif
      thrho1(nthb) = 1.0_dp
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(4+nbeg)
      thr3(nthb) = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = 1.0_dp
      thr1(nthb) = floats(2+nbeg)
      thr2(nthb) = floats(3+nbeg)
      thr3(nthb) = floats(4+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 395
398 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!**************
!  Bcoscross  *
!**************
400 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 13
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 405
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.6) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = floats(3+nbeg)
      thrho3(nthb) = floats(4+nbeg)
      thrho1(nthb) = floats(5+nbeg)
      thrho2(nthb) = floats(6+nbeg)
    elseif (nfloat.ge.5) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = 1.0_dp
      thrho3(nthb) = floats(3+nbeg)
      thrho1(nthb) = floats(4+nbeg)
      thrho2(nthb) = floats(5+nbeg)
    elseif (nfloat.ge.4) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = 1.0_dp
      thrho3(nthb) = floats(3+nbeg)
      thrho1(nthb) = floats(4+nbeg)
      thrho2(nthb) = floats(4+nbeg)
    elseif (nfloat.ge.3) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = 1.0_dp
      thrho3(nthb) = 1.0_dp
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.12) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = floats(3+nbeg)
      thrho3(nthb) = floats(4+nbeg)
      thrho1(nthb) = floats(5+nbeg)
      thrho2(nthb) = floats(6+nbeg)
      thr1min(nthb) = floats(7+nbeg)
      thr1(nthb) = floats(8+nbeg)
      thr2min(nthb) = floats(9+nbeg)
      thr2(nthb) = floats(10+nbeg)
      thr3min(nthb) = floats(11+nbeg)
      thr3(nthb) = floats(12+nbeg)
    elseif (nfloat.ge.11) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = floats(3+nbeg)
      thrho3(nthb) = floats(4+nbeg)
      thrho1(nthb) = floats(5+nbeg)
      thrho2(nthb) = floats(6+nbeg)
      thr1min(nthb) = floats(7+nbeg)
      thr1(nthb) = floats(8+nbeg)
      thr2min(nthb) = floats(9+nbeg)
      thr2(nthb) = floats(10+nbeg)
      thr3(nthb) = floats(11+nbeg)
    elseif (nfloat.eq.10) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = floats(3+nbeg)
      thrho3(nthb) = floats(4+nbeg)
      thrho1(nthb) = floats(5+nbeg)
      thrho2(nthb) = floats(6+nbeg)
      thr1min(nthb) = floats(7+nbeg)
      thr1(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3(nthb) = floats(10+nbeg)
    elseif (nfloat.eq.9) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = floats(3+nbeg)
      thrho3(nthb) = floats(4+nbeg)
      thrho1(nthb) = floats(5+nbeg)
      thrho2(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = 1.0_dp
      thrho3(nthb) = floats(3+nbeg)
      thrho1(nthb) = floats(4+nbeg)
      thrho2(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = 1.0_dp
      thrho3(nthb) = floats(3+nbeg)
      thrho1(nthb) = floats(4+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      threepoly(1,nthb) = 1.0_dp
      thrho3(nthb) = 1.0_dp
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
  endif
  goto 405
408 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!******************************************
!  Stillinger-Weber - Jiang & Brown form  *
!******************************************
410 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
415 line = '  '
  read(iin,'(a)',end=418) line
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 14
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 5
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 415
  endif
!
!  Assign coefficients and cutoffs - note cut-offs needed
!  even when bonded potential since these are also 
!  effectively parameters
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (nfloat.ge.11) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thrho3(nthb) = floats(5+nbeg)
    thr1min(nthb) = floats(6+nbeg)
    thr1(nthb) = floats(7+nbeg)
    thr2min(nthb) = floats(8+nbeg)
    thr2(nthb) = floats(9+nbeg)
    thr3min(nthb) = floats(10+nbeg)
    thr3(nthb) = floats(11+nbeg)
  elseif (nfloat.eq.10) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thrho3(nthb) = floats(5+nbeg)
    thr1min(nthb) = floats(6+nbeg)
    thr1(nthb) = floats(7+nbeg)
    thr2min(nthb) = floats(8+nbeg)
    thr2(nthb) = floats(9+nbeg)
    thr3min(nthb) = 0.0_dp
    thr3(nthb) = floats(10+nbeg)
  elseif (nfloat.eq.9) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thrho3(nthb) = floats(5+nbeg)
    thr1min(nthb) = floats(6+nbeg)
    thr1(nthb) = floats(7+nbeg)
    thr2min(nthb) = 0.0_dp
    thr2(nthb) = floats(8+nbeg)
    thr3min(nthb) = 0.0_dp
    thr3(nthb) = floats(9+nbeg)
  elseif (nfloat.eq.8) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = floats(4+nbeg)
    thrho3(nthb) = floats(5+nbeg)
    thr1min(nthb) = 0.0_dp
    thr1(nthb) = floats(6+nbeg)
    thr2min(nthb) = 0.0_dp
    thr2(nthb) = floats(7+nbeg)
    thr3min(nthb) = 0.0_dp
    thr3(nthb) = floats(8+nbeg)
  elseif (nfloat.eq.7) then
    thbk(nthb) = floats(1+nbeg)*units
    theta(nthb) = floats(2+nbeg)
    thrho1(nthb) = floats(3+nbeg)
    thrho2(nthb) = thrho1(nthb)
    thrho3(nthb) = floats(4+nbeg)
    thr1min(nthb) = 0.0_dp
    thr1(nthb) = floats(5+nbeg)
    thr2min(nthb) = 0.0_dp
    thr2(nthb) = floats(6+nbeg)
    thr3min(nthb) = 0.0_dp
    thr3(nthb) = floats(7+nbeg)
  else
    call outerror('Incorrect potential coefficient input',iline)
    call stopnow('potword3')
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 415
418 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!*****************************
!  Hydrogen-bond three bond  *
!*****************************
420 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  thrho1(nthb+1) = 12.0_dp
  thrho2(nthb+1) = 10.0_dp
  thrho3(nthb+1) = 4.0_dp
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'tap').eq.1) then
        lthetataper(nthb+1) = .true.
        thetatapermax(nthb+1) = 0.5_dp*pi
        thetatapermin(nthb+1) = 0.4_dp*pi
      elseif (index(words(i),'dre').eq.1) then
        ltdreiding(nthb+1) = .true.
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
!
!  Set exponents if specified
!
  if (nfloat.ge.3) then
    thrho1(nthb+1) = floats(1)
    thrho2(nthb+1) = floats(2)
    thrho3(nthb+1) = floats(3)
  elseif (nfloat.eq.2) then
    thrho1(nthb+1) = floats(1)
    thrho2(nthb+1) = floats(2)
  elseif (nfloat.eq.1) then
    thrho1(nthb+1) = floats(1)
  endif
!
425 line = '  '
  read(iin,'(a)',end = 428) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 425
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 428
    endif
  endif
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 15
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 425
  endif
!
!  Copy values across to this potential
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  thrho1(nthb) = thrho1(npot1)
  thrho2(nthb) = thrho2(npot1)
  thrho3(nthb) = thrho3(npot1)
!
!  Assign coefficients and cutoffs
!
  if (mmtexc(nthb).eq.1) then
    if (lthetataper(nthb)) then
      if (nfloat.gt.3) nfloat = nfloat - 2
      if (nfloat.ge.4) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thetatapermin(nthb) = floats(3+nbeg)*degtorad
        thetatapermax(nthb) = floats(4+nbeg)*degtorad
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword3')
      endif
    else
      if (nfloat.gt.2) nfloat = nfloat - 2
      if (nfloat.ge.2) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword3')
      endif
    endif
  else
    if (lthetataper(nthb)) then
      if (nfloat.gt.10) nfloat = nfloat - 2
      if (nfloat.ge.10) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thetatapermin(nthb) = floats(3+nbeg)*degtorad
        thetatapermax(nthb) = floats(4+nbeg)*degtorad
        thr1min(nthb) = floats(5+nbeg)
        thr1(nthb) = floats(6+nbeg)
        thr2min(nthb) = floats(7+nbeg)
        thr2(nthb) = floats(8+nbeg)
        thr3min(nthb) = floats(9+nbeg)
        thr3(nthb) = floats(10+nbeg)
      elseif (nfloat.eq.9) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thetatapermin(nthb) = floats(3+nbeg)*degtorad
        thr1min(nthb) = floats(4+nbeg)
        thr1(nthb) = floats(5+nbeg)
        thr2min(nthb) = floats(6+nbeg)
        thr2(nthb) = floats(7+nbeg)
        thr3min(nthb) = floats(8+nbeg)
        thr3(nthb) = floats(9+nbeg)
      elseif (nfloat.eq.8) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1min(nthb) = floats(3+nbeg)
        thr1(nthb) = floats(4+nbeg)
        thr2min(nthb) = floats(5+nbeg)
        thr2(nthb) = floats(6+nbeg)
        thr3min(nthb) = floats(7+nbeg)
        thr3(nthb) = floats(8+nbeg)
      elseif (nfloat.eq.7) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1min(nthb) = floats(3+nbeg)
        thr1(nthb) = floats(4+nbeg)
        thr2min(nthb) = floats(5+nbeg)
        thr2(nthb) = floats(6+nbeg)
        thr3(nthb) = floats(7+nbeg)
        thr3min(nthb) = 0.0_dp
      elseif (nfloat.eq.6) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1min(nthb) = floats(3+nbeg)
        thr1(nthb) = floats(4+nbeg)
        thr2(nthb) = floats(5+nbeg)
        thr3(nthb) = floats(6+nbeg)
        thr2min(nthb) = 0.0_dp
        thr3min(nthb) = 0.0_dp
      elseif (nfloat.eq.5) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1(nthb) = floats(3+nbeg)
        thr2(nthb) = floats(4+nbeg)
        thr3(nthb) = floats(5+nbeg)
        thr1min(nthb) = 0.0_dp
        thr2min(nthb) = 0.0_dp
        thr3min(nthb) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword3')
      endif
    else
      if (nfloat.gt.8) nfloat = nfloat - 2
      if (nfloat.ge.8) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1min(nthb) = floats(3+nbeg)
        thr1(nthb) = floats(4+nbeg)
        thr2min(nthb) = floats(5+nbeg)
        thr2(nthb) = floats(6+nbeg)
        thr3min(nthb) = floats(7+nbeg)
        thr3(nthb) = floats(8+nbeg)
      elseif (nfloat.eq.7) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1min(nthb) = floats(3+nbeg)
        thr1(nthb) = floats(4+nbeg)
        thr2min(nthb) = floats(5+nbeg)
        thr2(nthb) = floats(6+nbeg)
        thr3(nthb) = floats(7+nbeg)
        thr3min(nthb) = 0.0_dp
      elseif (nfloat.eq.6) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1min(nthb) = floats(3+nbeg)
        thr1(nthb) = floats(4+nbeg)
        thr2(nthb) = floats(5+nbeg)
        thr3(nthb) = floats(6+nbeg)
        thr2min(nthb) = 0.0_dp
        thr3min(nthb) = 0.0_dp
      elseif (nfloat.eq.5) then
        thbk(nthb) = floats(1+nbeg)*units
        theta(nthb) = floats(2+nbeg)*units
        thr1(nthb) = floats(3+nbeg)
        thr2(nthb) = floats(4+nbeg)
        thr3(nthb) = floats(5+nbeg)
        thr1min(nthb) = 0.0_dp
        thr2min(nthb) = 0.0_dp
        thr3min(nthb) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword3')
      endif
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 425
428 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok  =  .true.
  return
!*****************************
!  Equatorial ESFF potential *
!*****************************
430 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
!
435 line = '  '
  read(iin,'(a)',end = 438) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 435
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 438
    endif
  endif
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 16
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 435
  endif
!
!  Copy values across to this potential
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
!
!  Assign coefficients and cutoffs
!
  if (mmtexc(nthb).eq.1) then
    if (nfloat.gt.4) nfloat = nfloat - 3
    if (nfloat.ge.4) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.gt.10) nfloat = nfloat - 3
    if (nfloat.ge.10) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1min(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2min(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3min(nthb) = floats(9+nbeg)
      thr3(nthb) = floats(10+nbeg)
    elseif (nfloat.eq.9) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1min(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2min(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
      thr3min(nthb) = 0.0_dp
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1min(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
      thr2min(nthb) = 0.0_dp
      thr3min(nthb) = 0.0_dp
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
      thr1min(nthb) = 0.0_dp
      thr2min(nthb) = 0.0_dp
      thr3min(nthb) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 435
438 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok  =  .true.
  return
!****************************
!  Three-body UFF potential *
!****************************
440 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  thrho1(nthb+1) = 0.0_dp
  thrho2(nthb+1) = 0.0_dp
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
!
447 line = '  '
  read(iin,'(a)',end=448) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 447
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 448
    endif
  endif
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 17
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 447
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.gt.2) nfloat = nfloat - 2
    thrho1(nthb) = 0.0_dp
    if (nfloat.ge.2) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.gt.8) nfloat = nfloat-2
    thrho1(nthb) = 0.0_dp
    if (nfloat.ge.8) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2min(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3min(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2min(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(4+nbeg)
      thr3(nthb) = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(3+nbeg)
      thr3(nthb) = floats(4+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
  endif
  goto 447
448 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!***************
!  BAcoscross  *
!***************
450 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
455 line = '  '
  read(iin,'(a)',end=458) line
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 18
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 5
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 455
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.5) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
    elseif (nfloat.ge.4) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      theta(nthb) = floats(4+nbeg)
    elseif (nfloat.ge.3) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(2+nbeg)
      theta(nthb) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.11) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3min(nthb) = floats(10+nbeg)
      thr3(nthb) = floats(11+nbeg)
    elseif (nfloat.eq.10) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3(nthb) = floats(10+nbeg)
    elseif (nfloat.eq.9) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2(nthb) = floats(8+nbeg)
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      theta(nthb) = floats(5+nbeg)
      thr1(nthb) = floats(6+nbeg)
      thr2(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(2+nbeg)*units
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(3+nbeg)
      theta(nthb) = floats(4+nbeg)
      thr1(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)*units
      thrho3(nthb) = floats(1+nbeg)*units
      thrho1(nthb) = floats(2+nbeg)
      thrho2(nthb) = floats(2+nbeg)
      theta(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
      var1 = thbk(nthb)
      thbk(nthb) = thrho3(nthb)
      thrho3(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
    var1 = thbk(nthb)
    thbk(nthb) = thrho3(nthb)
    thrho3(nthb) = var1
  endif
  goto 455
458 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!************************
!  3 Coulomb potential  *
!************************
460 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
  endif
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 19
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    goto 465
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.1) then
      thbk(nthb) = floats(1+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.7) then
      thbk(nthb) = floats(1+nbeg)
      thr1min(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2min(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3min(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)
      thr1min(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2min(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      thbk(nthb) = floats(1+nbeg)
      thr1min(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(4+nbeg)
      thr3(nthb) = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      thbk(nthb) = floats(1+nbeg)
      thr1(nthb) = floats(2+nbeg)
      thr2(nthb) = floats(3+nbeg)
      thr3(nthb) = floats(4+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
  endif
  goto 465
468 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!********
!  Exp2 *
!********
470 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 20
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 4
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 5
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    nfit = nfit0
    goto 475
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.5) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.11) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3min(nthb) = floats(10+nbeg)
      thr3(nthb) = floats(11+nbeg)
    elseif (nfloat.eq.10) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = floats(8+nbeg)
      thr2(nthb) = floats(9+nbeg)
      thr3min(nthb) = 0.0_dp
      thr3(nthb) = floats(10+nbeg)
    elseif (nfloat.eq.9) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1min(nthb) = floats(6+nbeg)
      thr1(nthb) = floats(7+nbeg)
      thr2min(nthb) = 0.0_dp
      thr2(nthb) = floats(8+nbeg)
      thr3min(nthb) = 0.0_dp
      thr3(nthb) = floats(9+nbeg)
    elseif (nfloat.eq.8) then
      thbk(nthb) = floats(1+nbeg)*units
      theta(nthb) = floats(2+nbeg)
      thrho1(nthb) = floats(3+nbeg)
      thrho2(nthb) = floats(4+nbeg)
      thrho3(nthb) = floats(5+nbeg)
      thr1min(nthb) = 0.0_dp
      thr1(nthb) = floats(6+nbeg)
      thr2min(nthb) = 0.0_dp
      thr2(nthb) = floats(7+nbeg)
      thr3min(nthb) = 0.0_dp
      thr3(nthb) = floats(8+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho3(nthb)
      thrho3(nthb) = var1
      var1 = theta(nthb)
      theta(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho3(nthb)
    thrho3(nthb) = var1
    var1 = theta(nthb)
    theta(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
  endif
  goto 475
478 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!*************************
!  g3 Coulomb potential  *
!*************************
480 if (nthb.eq.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  lthetataper(nthb+1) = .false.
  ltdreiding(nthb+1) = .false.
  mmtexc(nthb+1) = 0
  npot1 = nthb + 1
  nbondtypein1 = 1
  nbondtypein2 = 1
  units = 1.0_dp
  nfloatptr = 1
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        ltintra(nthb+1) = .false.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmtexc(nthb+1) = 1
        ltintra(nthb+1) = .true.
        ltinter(nthb+1) = .false.
        lfound = .true.
      elseif (index(words(i),'sin').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 1
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'dou').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 2
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'tri').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 3
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'qua').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 4
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'res').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 5
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'ami').eq.1) then
        n3botype(1,nbondtypein1,nthb+1) = 6
        nbondtypein1 = nbondtypein1 + 1
        nbondtypein1 = min(nbondtypein1,2_i4)
      elseif (index(words(i),'reg').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 1
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'cyc').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 2
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'exo').eq.1) then
        n3botype(2,nbondtypein2,nthb+1) = 3
        nbondtypein2 = nbondtypein2 + 1
        nbondtypein2 = min(nbondtypein2,2_i4)
      elseif (index(words(i),'nbeq').eq.1) then
        n3bondnono(1,nthb+1) = n3bondnono(1,nthb+1) + 1
        if (n3bondnono(1,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(1,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(1,nthb+1),1,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbeq suboption',iline)
          call stopnow('potword3')
        endif
      elseif (index(words(i),'nbne').eq.1) then
        n3bondnono(2,nthb+1) = n3bondnono(2,nthb+1) + 1
        if (n3bondnono(2,nthb+1).gt.maxn3bondnono) then
          maxn3bondnono = n3bondnono(2,nthb+1)
          call changemaxn3bondnono
        endif
        if (nfloatptr.le.nfloat) then
          n3bondno(n3bondnono(2,nthb+1),2,nthb+1) = nint(floats(nfloatptr))
          nfloatptr = nfloatptr + 1
        else
          call outerror('Missing number for nbne suboption',iline)
          call stopnow('potword3')
        endif
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
        endif
        lwarnp = .false.
      endif
    enddo
  endif
  if (.not.lfound) then
    ltintra(nthb+1) = lint
    ltinter(nthb+1) = linr
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
  nthb = nthb + 1
  if (nthb.gt.maxthb) then
    maxthb = nthb + 10
    call changemaxthb
  endif
  nthrty(nthb) = 21
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
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 3
      nfpot(nfit) = nthb
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol3(iline,llibrary,nvar1,itype1,sym1,nvar2,itype2,sym2,nvar3,itype3,sym3,nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nthb = nthb - 1
    goto 485
  endif
!
!  Assign coefficients and cutoffs
!
  mmtexc(nthb) = mmtexc(npot1)
  n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,npot1)
  n3bondnono(1:2,nthb) = n3bondnono(1:2,npot1)
  if (n3bondnono(1,nthb).gt.0) then
    n3bondno(n3bondnono(1,nthb),1,nthb) = n3bondno(n3bondnono(1,nthb),1,npot1)
  endif
  if (n3bondnono(2,nthb).gt.0) then
    n3bondno(n3bondnono(2,nthb),2,nthb) = n3bondno(n3bondnono(2,nthb),2,npot1)
  endif
  lthetataper(nthb) = lthetataper(npot1)
  ltdreiding(nthb) = ltdreiding(npot1)
  thetatapermax(nthb) = thetatapermax(npot1)
  thetatapermin(nthb) = thetatapermin(npot1)
  if (mmtexc(nthb).eq.1) then
    if (nfloat.ge.2) then
      thbk(nthb) = floats(1+nbeg)
      theta(nthb) = floats(2+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  else
    if (nfloat.ge.8) then
      thbk(nthb) = floats(1+nbeg)
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2min(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3min(nthb) = floats(7+nbeg)
      thr3(nthb) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      thbk(nthb) = floats(1+nbeg)
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2min(nthb) = floats(5+nbeg)
      thr2(nthb) = floats(6+nbeg)
      thr3(nthb) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      thbk(nthb) = floats(1+nbeg)
      theta(nthb) = floats(2+nbeg)
      thr1min(nthb) = floats(3+nbeg)
      thr1(nthb) = floats(4+nbeg)
      thr2(nthb) = floats(5+nbeg)
      thr3(nthb) = floats(6+nbeg)
    elseif (nfloat.eq.5) then
      thbk(nthb) = floats(1+nbeg)
      theta(nthb) = floats(2+nbeg)
      thr1(nthb) = floats(3+nbeg)
      thr2(nthb) = floats(4+nbeg)
      thr3(nthb) = floats(5+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword3')
    endif
  endif
!
!  Assign pivot atom parameters
!
  ntspec1(nthb) = nvar1
  ntptyp1(nthb) = itype1
  symbol3(1,nthb) = sym1
!
!  Assign terminal atom parameters
!
  if (nvar2.eq.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    if (itype2.lt.itype3) then
      ntptyp2(nthb) = itype2
      ntptyp3(nthb) = itype3
      symbol3(2,nthb) = sym2
      symbol3(3,nthb) = sym3
    else
      ntptyp2(nthb) = itype3
      ntptyp3(nthb) = itype2
      symbol3(2,nthb) = sym3
      symbol3(3,nthb) = sym2
!
      ntmp = n3botype(1,1,nthb)
      n3botype(1,1,nthb) = n3botype(1,2,nthb)
      n3botype(1,2,nthb) = ntmp
      ntmp = n3botype(2,1,nthb)
      n3botype(2,1,nthb) = n3botype(2,2,nthb)
      n3botype(2,2,nthb) = ntmp
!
      if (mmtexc(nthb).eq.0) then
        var1 = thr1(nthb)
        thr1(nthb) = thr2(nthb)
        thr2(nthb) = var1
      endif
      var1 = thrho1(nthb)
      thrho1(nthb) = thrho2(nthb)
      thrho2(nthb) = var1
    endif
  elseif (nvar2.lt.nvar3) then
    ntspec2(nthb) = nvar2
    ntspec3(nthb) = nvar3
    ntptyp2(nthb) = itype2
    ntptyp3(nthb) = itype3
    symbol3(2,nthb) = sym2
    symbol3(3,nthb) = sym3
  else
    ntspec2(nthb) = nvar3
    ntspec3(nthb) = nvar2
    ntptyp2(nthb) = itype3
    ntptyp3(nthb) = itype2
    symbol3(2,nthb) = sym3
    symbol3(3,nthb) = sym2
!
    ntmp = n3botype(1,1,nthb)
    n3botype(1,1,nthb) = n3botype(1,2,nthb)
    n3botype(1,2,nthb) = ntmp
    ntmp = n3botype(2,1,nthb)
    n3botype(2,1,nthb) = n3botype(2,2,nthb)
    n3botype(2,2,nthb) = ntmp
!
    if (mmtexc(nthb).eq.0) then
      var1 = thr1(nthb)
      thr1(nthb) = thr2(nthb)
      thr2(nthb) = var1
    endif
    var1 = thrho1(nthb)
    thrho1(nthb) = thrho2(nthb)
    thrho2(nthb) = var1
  endif
  goto 485
488 if (.not.l55) l1000 = .true.
  do nt = npot1,nthb
!
!  Copy intra/inter flags
!
    ltintra(nt) = ltintra(npot1)
    ltinter(nt) = ltinter(npot1)
  enddo
  lwordok = .true.
  return
!
  end
