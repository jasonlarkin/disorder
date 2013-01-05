  subroutine potword4(iin,word,lwordok,iline,line,l55,l1000,linr,lint,llibrary,lfflags)
!
!  Processes potential input for four body potentials
!
!  iin = input fortran channel
!
!  llibrary = if .true. then this is a library input call
!
!  nforty = 1 => conventional four-body potential
!         = 2 => Ryckaert-Bellemanns potential
!         = 3 => out of plane potential (OOP)
!         = 4 => ESFF torsional potential
!         = 5 => harmonic torsional potential
!         = 6 => exponentially decaying torsion potential
!         = 7 => exponentially decaying ESFF torsion potential
!         = 8 => tapered torsional potential
!         = 9 => tapered ESFF torsion potential
!         =10 => torsional - angle cross term
!         =11 => inversion out of plane potential (OOP)
!         =12 => squared inversion out of plane potential (OOP)
!         =13 => UFF4 torsion
!         =14 => angle-angle cross potential (OOP)
!         =15 => UFF out of plane (OOP)
!         =16 => cosine angle-cosine angle cross potential (OOP)
!         =17 => torsional - cosine angle cross term
!
!  OOP indicates an out of plane potential
!
!  Molecular Mechanics:
!  --------------------
!
!  mmfexc specifies molecular mechanics type for four-body
!  potential types:
!
!    0 => bonded and nonbonded potential (default)
!    1 => bonded only potential
!
!  option word "molmec" after potential name selects default MM type
!  for marvin compatibility
!
!   1/95 Intra/inter-molecular facility now added for 4 body potentials
!   2/95 Phi0 offset added for standard torsional potential
!   2/95 Bonded vs nonbonded types added for 4 body terms
!   4/95 Library modifications added
!   3/97 Out of plane potentials added
!   4/98 Assignment of species type for library call changed
!        so that potential file form is accepted
!   8/98 ESFF torsion potential added
!  10/98 Codes for fitting variables simplified
!   6/01 Initialisation of line added for benefit of some compilers
!   7/02 K4 added for outofplane potential
!  10/02 Torharm potential added
!  11/02 Setting of lflag1/2 corrected
!   3/04 Exponentially decaying torsion potential added
!   3/04 Tapered torsion potentials added
!   3/04 Missing return statement after torharm pot added
!  10/04 Calls to getpotsymbol introduced to reduce code size
!  11/04 torangle potential added
!  11/04 Errors in end of file line numbers corrected
!  10/05 Inversion outofplane potential added
!   6/06 Squared form of inversion potential added
!   8/06 iin passed to linepro
!   8/06 Separate flag added for fitting flags
!   8/06 Bonding type sub-option added
!   9/06 Dreiding sub-option added
!   9/06 Literal symbols returned from getpotsymbol4
!   1/07 UFF4 torsion added
!   1/07 Amide bond type added
!  10/07 Angle-angle cross potential added
!  10/07 Regular sub-option specifically handled
!   4/08 Option for minimum cutoff distances in out of plane pot added
!   5/08 UFFoop potential added
!   5/08 only3 suboption added for out of plane potentials
!  11/08 xcosangleangle potential added
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 torcosangle potential added
!  11/08 Scaling of phi0 by units removed for torharm potential
!  12/08 Module input renamed to gulpinput
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use constants
  use control
  use element, only : maxele
  use fitting
  use four
  use general, only : nwarn
  use gulpinput
  use iochannels
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
  integer(i4)                  :: i
  integer(i4)                  :: itype1
  integer(i4)                  :: itype2
  integer(i4)                  :: itype3
  integer(i4)                  :: itype4
  integer(i4)                  :: n
  integer(i4)                  :: n1
  integer(i4)                  :: n2
  integer(i4)                  :: n3
  integer(i4)                  :: n4
  integer(i4)                  :: n5
  integer(i4)                  :: n6
  integer(i4)                  :: nbeg
  integer(i4)                  :: nfit0
  integer(i4)                  :: nfortyadd
  integer(i4)                  :: norder
  integer(i4)                  :: npot1
  integer(i4)                  :: nt
  integer(i4)                  :: nvar1
  integer(i4)                  :: nvar2
  integer(i4)                  :: nvar3
  integer(i4)                  :: nvar4
  logical                      :: lexpo
  logical                      :: lesff
  logical                      :: lflag1
  logical                      :: lflag2
  logical                      :: lfound
  logical                      :: lsymbol
  logical                      :: lvalidpot
  logical                      :: lwarnp
  real(dp)                     :: d1
  real(dp)                     :: d2
  real(dp)                     :: units
!
!  Initialise local variables
!
  lwarnp = .not.lmol
  if (index(word,'tors').eq.1) goto 100
  if (index(word,'four').eq.1) goto 100
  if (index(word,'ryck').eq.1) goto 100
  if (index(word,'outo').eq.1) goto 110
  if (index(word,'torh').eq.1) goto 120
  if (index(word,'tore').eq.1) goto 130
  if (index(word,'tort').eq.1) goto 140
  if (index(word,'tora').eq.1) goto 150
  if (index(word,'inve').eq.1) goto 160
  if (index(word,'uff4').eq.1) goto 170
  if (index(word,'xang').eq.1) goto 180
  if (index(word,'uffo').eq.1) goto 190
  if (index(word,'xcos').eq.1) goto 200
  if (index(word,'torc').eq.1) goto 210
  return
!***************************
!  Four-body interactions  *
!***************************
100 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lexpo = (index(word,'ryc').eq.1)
  lfound = .false.
  lesff = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'ryc').eq.1) then
        lexpo = .true.
      elseif (index(words(i),'esff').eq.1) then
        lesff = .true.
      elseif (index(words(i),'dre').eq.1) then
        lfdreiding(nfor+1) = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
          lwarnp = .false.
        endif
      endif
    enddo
  endif
  if (.not.lfound) then
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
  if (lexpo) then
!***************************************
!  Ryckaert-Bellemanns four body term  *
!***************************************
    if (nfloat.ge.1) then
      npfor(nfor+1) = nint(floats(1))
      if (npfor(nfor+1).gt.5) then
        if (ioproc) then
          write(ioout,'(/,''  **** Ryckaert-Bellemanns potential only available to fifth order ****'',/)')
        endif
        call stopnow('potword4')
      endif
    else
      npfor(nfor+1) = 5
    endif
    norder = npfor(nfor+1)
    npot1 = nfor + 1
102 line = '  '
    read(iin,'(a)',end=108) line
    iline = iline + 1
    call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 102
    if (nword.gt.0) then
      word = words(1)(1:20)
      call worsy(word,lsymbol,.true.)
      if (.not.lsymbol) then
        l55 = .true.
        goto 108
      endif
    endif
    nfor = nfor + 1
    if (nfor.gt.maxfor) then
      maxfor = nfor + 10
      call changemaxfor
    endif
    nforty(nfor) = 2
    loutofplane(nfor) = .false.
!
!  Copy first line info from first potential
!
    npfor(nfor) = npfor(npot1)
!
!  Fitting flags
!
    nfit0 = nfit
    if (lfit.and.lfflags) then
      if (nfloat.lt.norder+1) then
        call outerror('insufficient input data for 4-body potl',iline)
        call stopnow('potword4')
      endif
      do n = norder+1,1,-1
        n1 = int(floats(nfloat))
        nfloat = nfloat - 1
        if (n1.eq.1) then
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 4
          nfpot(nfit) = nfor
          nfpot(nfit) = 1 + n
        endif
      enddo
    endif
!
!  Process symbol input
!
    call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                       nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nfor = nfor - 1
      nfit = nfit0
      goto 102
    endif
!
!  Assign cutoffs
!
    mmfexc(nfor) = mmfexc(npot1)
    n4botype(1:2,nfor) = n4botype(1:2,npot1)
    lfdreiding(nfor) = lfdreiding(npot1)
    if (mmfexc(nfor).eq.1) then
      if (nfloat.ge.1) then
        fork(nfor) = floats(1+nbeg)*units
      else
        call outerror('error in cutoff input for torsion',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.5) then
        fork(nfor) = floats(1+nbeg)*units
        for1(nfor) = floats(2+nbeg)
        for2(nfor) = floats(3+nbeg)
        for3(nfor) = floats(4+nbeg)
        for4(nfor) = floats(5+nbeg)
      else
        call outerror('error in cutoff input for torsion',iline)
        call stopnow('potword4')
      endif  
    endif  
!
!  Read and assign coefficients
!
    line = '  '
    read(iin,'(a)') line
    iline = iline + 1
    call linepro(iin,line,iline)
    if (nfloat.ge.norder) then
      do i = 1,norder
        forpoly(i,nfor) = floats(i)
      enddo
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
    nfspec1(nfor) = nvar1
    nfspec2(nfor) = nvar2
    nfspec3(nfor) = nvar3
    nfspec4(nfor) = nvar4
    nfptyp1(nfor) = itype1
    nfptyp2(nfor) = itype2
    nfptyp3(nfor) = itype3
    nfptyp4(nfor) = itype4
    goto 102
  elseif (lesff) then
!***************************
!  ESFF torsion potential  *
!***************************
103   line = '  '
    read(iin,'(a)',end=108) line
    iline = iline + 1
    call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 103
    if (nword.gt.0) then
      word = words(1)(1:20)
      call worsy(word,lsymbol,.true.)
      if (.not.lsymbol) then
        l55 = .true.
        goto 108
      endif
    endif
    nfor = nfor + 1
    if (nfor.gt.maxfor) then
      maxfor = nfor + 10
      call changemaxfor
    endif
    nforty(nfor) = 4
    loutofplane(nfor) = .false.
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
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 2
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                       nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nfor = nfor - 1
      nfit = nfit0
      goto 103
    endif
!
!  Assign coefficients and cutoffs
!
    mmfexc(nfor) = mmfexc(npot1)
    lfdreiding(nfor) = lfdreiding(npot1)
    n4botype(1:2,nfor) = n4botype(1:2,npot1)
    if (mmfexc(nfor).eq.1) then
      if (nfloat.ge.3) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
      elseif (nfloat.eq.2) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.7) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
        for1(nfor) = floats(4+nbeg)
        for2(nfor) = floats(5+nbeg)
        for3(nfor) = floats(6+nbeg)
        for4(nfor) = floats(7+nbeg)
      elseif (nfloat.eq.6) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        for1(nfor) = floats(3+nbeg)
        for2(nfor) = floats(4+nbeg)
        for3(nfor) = floats(5+nbeg)
        for4(nfor) = floats(6+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    endif
    if (npfor(nfor).eq.0) then
      call outerror('phase number for torsion equals zero',iline)
      call stopnow('potword4')
    endif
    nfspec1(nfor) = nvar1
    nfspec2(nfor) = nvar2
    nfspec3(nfor) = nvar3
    nfspec4(nfor) = nvar4
    nfptyp1(nfor) = itype1
    nfptyp2(nfor) = itype2
    nfptyp3(nfor) = itype3
    nfptyp4(nfor) = itype4
    goto 103
  else
!****************************
!  Standard torsional term  *
!****************************
105   line = '  '
    read(iin,'(a)',end=108) line
    iline = iline + 1
    call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 105
    if (nword.gt.0) then
      word = words(1)(1:20)
      call worsy(word,lsymbol,.true.)
      if (.not.lsymbol) then
        l55 = .true.
        goto 108
      endif
    endif
    nfor = nfor + 1
    if (nfor.gt.maxfor) then
      maxfor = nfor + 10
      call changemaxfor
    endif
    nforty(nfor) = 1
    loutofplane(nfor) = .false.
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
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                       nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nfor = nfor - 1
      nfit = nfit0
      goto 105
    endif
!
!  Assign coefficients and cutoffs
!
    mmfexc(nfor) = mmfexc(npot1)
    lfdreiding(nfor) = lfdreiding(npot1)
    n4botype(1:2,nfor) = n4botype(1:2,npot1)
    if (mmfexc(nfor).eq.1) then
      if (nfloat.ge.3) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = floats(3+nbeg)
      elseif (nfloat.eq.2) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.7) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = floats(3+nbeg)
        for1(nfor) = floats(4+nbeg)
        for2(nfor) = floats(5+nbeg)
        for3(nfor) = floats(6+nbeg)
        for4(nfor) = floats(7+nbeg)
      elseif (nfloat.eq.6) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
        for1(nfor) = floats(3+nbeg)
        for2(nfor) = floats(4+nbeg)
        for3(nfor) = floats(5+nbeg)
        for4(nfor) = floats(6+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    endif
    if (npfor(nfor).eq.0) then
      call outerror('phase number for torsion equals zero',iline)
      call stopnow('potword4')
    endif
    nfspec1(nfor) = nvar1
    nfspec2(nfor) = nvar2
    nfspec3(nfor) = nvar3
    nfspec4(nfor) = nvar4
    nfptyp1(nfor) = itype1
    nfptyp2(nfor) = itype2
    nfptyp3(nfor) = itype3
    nfptyp4(nfor) = itype4
    goto 105
  endif
108 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!***************************
!  Out of plane potential  *
!***************************
110 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'only3').eq.1) then
        lonly3oop(nfor+1) = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 3
  loutofplane(nfor) = .true.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfflags.and.nfloat.gt.1) then
!
!  Are there one or two fitting flags?
!
    d1 = floats(nfloat)
    d2 = floats(nfloat-1)
    n1 = nint(d1)
    n2 = nint(d2)
    lflag1 = (abs(d1-1.0_dp).lt.1.0d-10.or.abs(d1).lt.1.0d-10)
    lflag2 = (abs(d2-1.0_dp).lt.1.0d-10.or.abs(d2).lt.1.0d-10)
    if (lflag1.and.lflag2) then
!
!  Two flags
!
      nfloat = nfloat - 2
      if (lfit.and.n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
      if (lfit.and.n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 2
      endif
    elseif (lflag1) then
!
!  One flag
!
      nfloat = nfloat - 1
      if (lfit.and.n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 115
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.2) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)*units
    elseif (nfloat.eq.1) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.8) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)*units
      for1min(nfor) = floats(3+nbeg)
      for1(nfor) = floats(4+nbeg)
      for2min(nfor) = floats(5+nbeg)
      for2(nfor) = floats(6+nbeg)
      for3min(nfor) = floats(7+nbeg)
      for3(nfor) = floats(8+nbeg)
    elseif (nfloat.eq.7) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = 0.0_dp
      for1min(nfor) = floats(2+nbeg)
      for1(nfor) = floats(3+nbeg)
      for2min(nfor) = floats(4+nbeg)
      for2(nfor) = floats(5+nbeg)
      for3min(nfor) = floats(6+nbeg)
      for3(nfor) = floats(7+nbeg)
    elseif (nfloat.ge.5) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)*units
      for1(nfor) = floats(3+nbeg)
      for2(nfor) = floats(4+nbeg)
      for3(nfor) = floats(5+nbeg)
      for1min(nfor) = 0.0_dp
      for2min(nfor) = 0.0_dp
      for3min(nfor) = 0.0_dp
    elseif (nfloat.eq.4) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = 0.0_dp
      for1(nfor) = floats(2+nbeg)
      for2(nfor) = floats(3+nbeg)
      for3(nfor) = floats(4+nbeg)
      for1min(nfor) = 0.0_dp
      for2min(nfor) = 0.0_dp
      for3min(nfor) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 115
118 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt)   = lfintra(npot1)
    lfinter(nt)   = lfinter(npot1)
    lonly3oop(nt) = lonly3oop(npot1)
  enddo
  lwordok = .true.
  return
!**********************
!  Torharm potential  *
!**********************
120 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'dre').eq.1) then
        lfdreiding(nfor+1) = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 5
  loutofplane(nfor) = .false.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
!
!  Are there one or two fitting flags?
!
    n1 = int(floats(nfloat))
    n2 = int(floats(nfloat-1))
!
!  Two flags
!
    nfloat = nfloat - 2
    if (lfit.and.n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 1
    endif
    if (lfit.and.n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 2
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 125
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  lfdreiding(nfor) = lfdreiding(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.2) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
    elseif (nfloat.eq.1) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.5) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
      for1(nfor) = floats(3+nbeg)
      for2(nfor) = floats(4+nbeg)
      for3(nfor) = floats(5+nbeg)
    elseif (nfloat.eq.4) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = 0.0_dp
      for1(nfor) = floats(2+nbeg)
      for2(nfor) = floats(3+nbeg)
      for3(nfor) = floats(4+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 125
128 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!*********************
!  Torexp potential  *
!*********************
130 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lesff = .false.
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'esff').eq.1) then
        lesff = .true.
      elseif (index(words(i),'dre').eq.1) then
        lfdreiding(nfor+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
  if (lesff) then
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
    nfor = nfor + 1
    if (nfor.gt.maxfor) then
      maxfor = nfor + 10
      call changemaxfor
    endif
    nforty(nfor) = 7
    loutofplane(nfor) = .false.
!
!  Fitting flags
!
    nfit0 = nfit
    if (lfit.and.lfflags) then
      n5 = int(floats(nfloat))
      n4 = int(floats(nfloat-1))
      n3 = int(floats(nfloat-2))
      n2 = int(floats(nfloat-3))
      n1 = int(floats(nfloat-4))
      nfloat = nfloat - 5
!
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
!
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 2
      endif
!
      if (n3.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 3
      endif
!
      if (n4.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 4
      endif
!
      if (n5.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 5
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                       nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nfor = nfor - 1
      nfit = nfit0
      goto 135
    endif
!
!  Assign coefficients and cutoffs
!
    mmfexc(nfor) = mmfexc(npot1)
    lfdreiding(nfor) = lfdreiding(npot1)
    n4botype(1:2,nfor) = n4botype(1:2,npot1)
    if (mmfexc(nfor).eq.1) then
      if (nfloat.ge.6) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
        forpoly(2,nfor) = floats(4+nbeg)
        forpoly(3,nfor) = floats(5+nbeg)
        forpoly(4,nfor) = floats(6+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.10) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
        forpoly(2,nfor) = floats(4+nbeg)
        forpoly(3,nfor) = floats(5+nbeg)
        forpoly(4,nfor) = floats(6+nbeg)
        for1(nfor) = floats(7+nbeg)
        for2(nfor) = floats(8+nbeg)
        for3(nfor) = floats(9+nbeg)
        for4(nfor) = floats(10+nbeg)
      elseif (nfloat.eq.9) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
        forpoly(2,nfor) = floats(4+nbeg)
        forpoly(3,nfor) = floats(5+nbeg)
        forpoly(4,nfor) = floats(6+nbeg)
        for1(nfor) = floats(7+nbeg)
        for2(nfor) = floats(8+nbeg)
        for3(nfor) = floats(9+nbeg)
        for4(nfor) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    endif
    nfspec1(nfor) = nvar1
    nfspec2(nfor) = nvar2
    nfspec3(nfor) = nvar3
    nfspec4(nfor) = nvar4
    nfptyp1(nfor) = itype1
    nfptyp2(nfor) = itype2
    nfptyp3(nfor) = itype3
    nfptyp4(nfor) = itype4
    goto 135
  else
137   line = '  '
    read(iin,'(a)',end=138) line
    iline = iline + 1
    call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 137
    if (nword.gt.0) then
      word = words(1)(1:20)
      call worsy(word,lsymbol,.true.)
      if (.not.lsymbol) then
        l55 = .true.
        goto 138
      endif
    endif
    nfor = nfor + 1
    if (nfor.gt.maxfor) then
      maxfor = nfor + 10
      call changemaxfor
    endif
    nforty(nfor) = 6
    loutofplane(nfor) = .false.
!
!  Fitting flags
!
    nfit0 = nfit
    if (lfit.and.lfflags) then
      n4 = int(floats(nfloat))
      n3 = int(floats(nfloat-1))
      n2 = int(floats(nfloat-2))
      n1 = int(floats(nfloat-3))
      nfloat = nfloat - 4
!
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
!
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 3
      endif
!
      if (n3.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 4
      endif
!
      if (n4.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 5
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                       nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nfor = nfor - 1
      nfit = nfit0
      goto 137
    endif
!
!  Assign coefficients and cutoffs
!
    mmfexc(nfor) = mmfexc(npot1)
    lfdreiding(nfor) = lfdreiding(npot1)
    n4botype(1:2,nfor) = n4botype(1:2,npot1)
    if (mmfexc(nfor).eq.1) then
      if (nfloat.ge.6) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = floats(3+nbeg)
        forpoly(2,nfor) = floats(4+nbeg)
        forpoly(3,nfor) = floats(5+nbeg)
        forpoly(4,nfor) = floats(6+nbeg)
      elseif (nfloat.eq.5) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
        forpoly(2,nfor) = floats(3+nbeg)
        forpoly(3,nfor) = floats(4+nbeg)
        forpoly(4,nfor) = floats(5+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.10) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = floats(3+nbeg)*units
        forpoly(2,nfor) = floats(4+nbeg)
        forpoly(3,nfor) = floats(5+nbeg)
        forpoly(4,nfor) = floats(6+nbeg)
        for1(nfor) = floats(7+nbeg)
        for2(nfor) = floats(8+nbeg)
        for3(nfor) = floats(9+nbeg)
        for4(nfor) = floats(10+nbeg)
      elseif (nfloat.eq.9) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
        forpoly(2,nfor) = floats(3+nbeg)
        forpoly(3,nfor) = floats(4+nbeg)
        forpoly(4,nfor) = floats(5+nbeg)
        for1(nfor) = floats(6+nbeg)
        for2(nfor) = floats(7+nbeg)
        for3(nfor) = floats(8+nbeg)
        for4(nfor) = floats(9+nbeg)
      elseif (nfloat.eq.8) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
        forpoly(2,nfor) = floats(3+nbeg)
        forpoly(3,nfor) = floats(4+nbeg)
        forpoly(4,nfor) = floats(5+nbeg)
        for1(nfor) = floats(6+nbeg)
        for2(nfor) = floats(7+nbeg)
        for3(nfor) = floats(8+nbeg)
        for4(nfor) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    endif
    nfspec1(nfor) = nvar1
    nfspec2(nfor) = nvar2
    nfspec3(nfor) = nvar3
    nfspec4(nfor) = nvar4
    nfptyp1(nfor) = itype1
    nfptyp2(nfor) = itype2
    nfptyp3(nfor) = itype3
    nfptyp4(nfor) = itype4
    goto 137
  endif
138 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!***********************
!  Tortaper potential  *
!***********************
140 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lesff = .false.
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'esff').eq.1) then
        lesff = .true.
      elseif (index(words(i),'dre').eq.1) then
        lfdreiding(nfor+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
  if (lesff) then
145   line = '  '
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
    nfor = nfor + 1
    if (nfor.gt.maxfor) then
      maxfor = nfor + 10
      call changemaxfor
    endif
    nforty(nfor) = 9
    loutofplane(nfor) = .false.
!
!  Fitting flags
!
    nfit0 = nfit
    if (lfit.and.lfflags) then
      n2 = int(floats(nfloat))
      n1 = int(floats(nfloat-1))
      nfloat = nfloat - 2
!
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
!
      if (n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 2
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                       nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nfor = nfor - 1
      nfit = nfit0
      goto 145
    endif
!
!  Assign coefficients and cutoffs
!
    mmfexc(nfor) = mmfexc(npot1)
    lfdreiding(nfor) = lfdreiding(npot1)
    n4botype(1:2,nfor) = n4botype(1:2,npot1)
    if (mmfexc(nfor).eq.1) then
      if (nfloat.ge.4) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
        forpoly(2,nfor) = floats(4+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.8) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
        forpoly(2,nfor) = floats(4+nbeg)
        for1(nfor) = floats(5+nbeg)
        for2(nfor) = floats(6+nbeg)
        for3(nfor) = floats(7+nbeg)
        for4(nfor) = floats(8+nbeg)
      elseif (nfloat.eq.7) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*units
        npfor(nfor) = nint(floats(3+nbeg))
        forpoly(2,nfor) = floats(4+nbeg)
        for1(nfor) = floats(5+nbeg)
        for2(nfor) = floats(6+nbeg)
        for3(nfor) = floats(7+nbeg)
        for4(nfor) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    endif
    nfspec1(nfor) = nvar1
    nfspec2(nfor) = nvar2
    nfspec3(nfor) = nvar3
    nfspec4(nfor) = nvar4
    nfptyp1(nfor) = itype1
    nfptyp2(nfor) = itype2
    nfptyp3(nfor) = itype3
    nfptyp4(nfor) = itype4
    goto 145
  else
147 line = '  '
    read(iin,'(a)',end=148) line
    iline = iline + 1
    call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 147
    if (nword.gt.0) then
      word = words(1)(1:20)
      call worsy(word,lsymbol,.true.)
      if (.not.lsymbol) then
        l55 = .true.
        goto 148
      endif
    endif
    nfor = nfor + 1
    if (nfor.gt.maxfor) then
      maxfor = nfor + 10
      call changemaxfor
    endif
    nforty(nfor) = 8
    loutofplane(nfor) = .false.
!
!  Fitting flags
!
    nfit0 = nfit
    if (lfit.and.lfflags) then
      n1 = int(floats(nfloat))
      nfloat = nfloat - 1
!
      if (n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
    endif
!
!  Process symbol input
!
    call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                       nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
    if (.not.lvalidpot) then
      nfor = nfor - 1
      nfit = nfit0
      goto 147
    endif
!
!  Assign coefficients and cutoffs
!
    mmfexc(nfor) = mmfexc(npot1)
    lfdreiding(nfor) = lfdreiding(npot1)
    n4botype(1:2,nfor) = n4botype(1:2,npot1)
    if (mmfexc(nfor).eq.1) then
      if (nfloat.ge.4) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = floats(3+nbeg)
        forpoly(2,nfor) = floats(4+nbeg)
      elseif (nfloat.eq.3) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
        forpoly(2,nfor) = floats(3+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.8) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = floats(3+nbeg)*units
        forpoly(2,nfor) = floats(4+nbeg)
        for1(nfor) = floats(5+nbeg)
        for2(nfor) = floats(6+nbeg)
        for3(nfor) = floats(7+nbeg)
        for4(nfor) = floats(8+nbeg)
      elseif (nfloat.eq.7) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
        forpoly(2,nfor) = floats(3+nbeg)
        for1(nfor) = floats(4+nbeg)
        for2(nfor) = floats(5+nbeg)
        for3(nfor) = floats(6+nbeg)
        for4(nfor) = floats(7+nbeg)
      elseif (nfloat.eq.6) then
        fork(nfor) = floats(1+nbeg)*units
        npfor(nfor) = nint(floats(2+nbeg))
        forpoly(1,nfor) = 0.0_dp
        forpoly(2,nfor) = floats(3+nbeg)
        for1(nfor) = floats(4+nbeg)
        for2(nfor) = floats(5+nbeg)
        for3(nfor) = floats(6+nbeg)
        for4(nfor) = 0.0_dp
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    endif
    nfspec1(nfor) = nvar1
    nfspec2(nfor) = nvar2
    nfspec3(nfor) = nvar3
    nfspec4(nfor) = nvar4
    nfptyp1(nfor) = itype1
    nfptyp2(nfor) = itype2
    nfptyp3(nfor) = itype3
    nfptyp4(nfor) = itype4
    goto 147
  endif
148 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!***********************
!  Torangle potential  *
!***********************
150 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'dre').eq.1) then
        lfdreiding(nfor+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
155 line = '  '
  read(iin,'(a)',end=108) line
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 10
  loutofplane(nfor) = .false.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
!
!  Are there one or two fitting flags?
!
    n1 = int(floats(nfloat-2))
    n2 = int(floats(nfloat-1))
    n3 = int(floats(nfloat))
!
!  Three flags
!
    nfloat = nfloat - 3
    if (lfit.and.n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 1
    endif
    if (lfit.and.n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 2
    endif
    if (lfit.and.n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 3
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 155
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  lfdreiding(nfor) = lfdreiding(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.3) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
      forpoly(2,nfor) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.6) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
      forpoly(2,nfor) = floats(3+nbeg)
      for1(nfor) = floats(4+nbeg)
      for2(nfor) = floats(5+nbeg)
      for3(nfor) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 155
158 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!*************************************
!  Inversion out of plane potential  *
!*************************************
160 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  nfortyadd = 0
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound=.true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'only3').eq.1) then
        lonly3oop(nfor+1) = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sq').eq.1) then
        nfortyadd = 1
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 11 + nfortyadd
  loutofplane(nfor) = .true.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfflags.and.nfloat.gt.1) then
!
!  Are there one or two fitting flags?
!
    d1 = floats(nfloat)
    d2 = floats(nfloat-1)
    n1 = nint(d1)
    n2 = nint(d2)
    lflag1 = (abs(d1-1.0_dp).lt.1.0d-10.or.abs(d1).lt.1.0d-10)
    lflag2 = (abs(d2-1.0_dp).lt.1.0d-10.or.abs(d2).lt.1.0d-10)
    if (lflag1.and.lflag2) then
!
!  Two flags
!
      nfloat = nfloat - 2
      if (lfit.and.n2.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
      if (lfit.and.n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 2
      endif
    elseif (lflag1) then
!
!  One flag
!
      nfloat = nfloat - 1
      if (lfit.and.n1.eq.1) then
        nfit = nfit + 1
        if (nfit.ge.maxfit) then
          maxfit = nfit + 10
          call changemaxfit
        endif
        nftyp(nfit) = 4
        nfpot(nfit) = nfor
        nfvar(nfit) = 1
      endif
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 165
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfortyadd.eq.1) then
      if (nfloat.ge.2) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*degtorad
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    else
      if (nfloat.ge.1) then
        fork(nfor) = floats(1+nbeg)*units
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif
    endif
  else
    if (nfortyadd.eq.1) then
      if (nfloat.ge.5) then
        fork(nfor) = floats(1+nbeg)*units
        forpoly(1,nfor) = floats(2+nbeg)*degtorad
        for1(nfor) = floats(3+nbeg)
        for2(nfor) = floats(4+nbeg)
        for3(nfor) = floats(5+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    else
      if (nfloat.ge.4) then
        fork(nfor) = floats(1+nbeg)*units
        for1(nfor) = floats(2+nbeg)
        for2(nfor) = floats(3+nbeg)
        for3(nfor) = floats(4+nbeg)
      else
        call outerror('Incorrect potential coefficient input',iline)
        call stopnow('potword4')
      endif  
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 165
168 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt)   = lfintra(npot1)
    lfinter(nt)   = lfinter(npot1)
    lonly3oop(nt) = lonly3oop(npot1)
  enddo
  lwordok = .true.
  return
!*****************
!  UFF4 torsion  *
!*****************
170 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'dre').eq.1) then
        lfdreiding(nfor+1) = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
      endif
      i = i + 1
      if (lfound.and.lwarnp) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/,''**** Warning - inter/intra/both/bond directive given on potential line ****'')')
          write(ioout,'(''**** but the molecule directive is currently inactive                  ****'')')
          lwarnp = .false.
        endif
      endif
    enddo
  endif
  if (.not.lfound) then
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
!****************************
!  Standard torsional term  *
!****************************
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 13
  loutofplane(nfor) = .false.
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
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 1
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 175
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  lfdreiding(nfor) = lfdreiding(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.3) then
      fork(nfor) = floats(1+nbeg)*units
      npfor(nfor) = nint(floats(2+nbeg))
      forpoly(1,nfor) = floats(3+nbeg)
    elseif (nfloat.eq.2) then
      fork(nfor) = floats(1+nbeg)*units
      npfor(nfor) = nint(floats(2+nbeg))
      forpoly(1,nfor) = 0.0_dp
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.7) then
      fork(nfor) = floats(1+nbeg)*units
      npfor(nfor) = nint(floats(2+nbeg))
      forpoly(1,nfor) = floats(3+nbeg)
      for1(nfor) = floats(4+nbeg)
      for2(nfor) = floats(5+nbeg)
      for3(nfor) = floats(6+nbeg)
      for4(nfor) = floats(7+nbeg)
    elseif (nfloat.eq.6) then
      fork(nfor) = floats(1+nbeg)*units
      npfor(nfor) = nint(floats(2+nbeg))
      forpoly(1,nfor) = 0.0_dp
      for1(nfor) = floats(3+nbeg)
      for2(nfor) = floats(4+nbeg)
      for3(nfor) = floats(5+nbeg)
      for4(nfor) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  if (npfor(nfor).eq.0) then
    call outerror('phase number for torsion equals zero',iline)
    call stopnow('potword4')
  endif
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 175
178 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!********************************
!  Angle-angle cross potential  *
!********************************
180 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 14
  loutofplane(nfor) = .true.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags.and.nfloat.ge.6) then
    n1 = int(floats(nfloat-5))
    n2 = int(floats(nfloat-4))
    n3 = int(floats(nfloat-3))
    n4 = int(floats(nfloat-2))
    n5 = int(floats(nfloat-1))
    n6 = int(floats(nfloat))
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 4
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 5
    endif
    if (n6.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 6
    endif
    nfloat = nfloat - 6
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 185
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.6) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)*units
      forpoly(2,nfor) = floats(3+nbeg)*units
      forpoly(3,nfor) = floats(4+nbeg)*degtorad
      forpoly(4,nfor) = floats(5+nbeg)*degtorad
      forpoly(5,nfor) = floats(6+nbeg)*degtorad
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.9) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)*units
      forpoly(2,nfor) = floats(3+nbeg)*units
      forpoly(3,nfor) = floats(4+nbeg)*degtorad
      forpoly(4,nfor) = floats(5+nbeg)*degtorad
      forpoly(5,nfor) = floats(6+nbeg)*degtorad
      for1(nfor) = floats(7+nbeg)
      for2(nfor) = floats(8+nbeg)
      for3(nfor) = floats(9+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 185
188 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!*******************************
!  UFF out of plane potential  *
!*******************************
190 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  nfortyadd = 0
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound=.true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'only3').eq.1) then
        lonly3oop(nfor+1) = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
195 line = '  '
  read(iin,'(a)',end=198) line
  iline = iline + 1
  call linepro(iin,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 195
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.true.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 198
    endif
  endif
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 15
  loutofplane(nfor) = .true.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags.and.nfloat.gt.1) then
!
!  Fitting flags
!
    n1 = int(floats(nfloat-3))
    n2 = int(floats(nfloat-2))
    n3 = int(floats(nfloat-1))
    n4 = int(floats(nfloat))
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 4
    endif
    nfloat = nfloat - 4
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 195
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.4) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
      forpoly(2,nfor) = floats(3+nbeg)
      forpoly(3,nfor) = floats(4+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.7) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
      forpoly(2,nfor) = floats(3+nbeg)
      forpoly(3,nfor) = floats(4+nbeg)
      for1(nfor) = floats(5+nbeg)
      for2(nfor) = floats(6+nbeg)
      for3(nfor) = floats(7+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 195
198 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt)   = lfintra(npot1)
    lfinter(nt)   = lfinter(npot1)
    lonly3oop(nt) = lonly3oop(npot1)
  enddo
  lwordok = .true.
  return
!**********************************************
!  Cosine angle-cosine angle cross potential  *
!**********************************************
200 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lfintra(nfor+1) = .false.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 16
  loutofplane(nfor) = .true.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags.and.nfloat.ge.6) then
    n1 = int(floats(nfloat-5))
    n2 = int(floats(nfloat-4))
    n3 = int(floats(nfloat-3))
    n4 = int(floats(nfloat-2))
    n5 = int(floats(nfloat-1))
    n6 = int(floats(nfloat))
    if (n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 1
    endif
    if (n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 2
    endif
    if (n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 3
    endif
    if (n4.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 4
    endif
    if (n5.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 5
    endif
    if (n6.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 6
    endif
    nfloat = nfloat - 6
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 205
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.6) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)*units
      forpoly(2,nfor) = floats(3+nbeg)*units
      forpoly(3,nfor) = floats(4+nbeg)*degtorad
      forpoly(4,nfor) = floats(5+nbeg)*degtorad
      forpoly(5,nfor) = floats(6+nbeg)*degtorad
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.9) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)*units
      forpoly(2,nfor) = floats(3+nbeg)*units
      forpoly(3,nfor) = floats(4+nbeg)*degtorad
      forpoly(4,nfor) = floats(5+nbeg)*degtorad
      forpoly(5,nfor) = floats(6+nbeg)*degtorad
      for1(nfor) = floats(7+nbeg)
      for2(nfor) = floats(8+nbeg)
      for3(nfor) = floats(9+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 205
208 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!**************************
!  Torcosangle potential  *
!**************************
210 if (nfor.eq.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nfor + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'intr').eq.1) then
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmfexc(nfor+1) = 1
        lfintra(nfor+1) = .true.
        lfinter(nfor+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'dre').eq.1) then
        lfdreiding(nfor+1) = .true.
      elseif (index(words(i),'sin').eq.1) then
        n4botype(1,nfor+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n4botype(1,nfor+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n4botype(1,nfor+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n4botype(1,nfor+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n4botype(1,nfor+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n4botype(1,nfor+1) = 6
      elseif (index(words(i),'reg').eq.1) then
        n4botype(2,nfor+1) = 1
      elseif (index(words(i),'cyc').eq.1) then
        n4botype(2,nfor+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n4botype(2,nfor+1) = 3
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
    lfintra(nfor+1) = lint
    lfinter(nfor+1) = linr
  endif
215 line = '  '
  read(iin,'(a)',end=108) line
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
  nfor = nfor + 1
  if (nfor.gt.maxfor) then
    maxfor = nfor + 10
    call changemaxfor
  endif
  nforty(nfor) = 17
  loutofplane(nfor) = .false.
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfit.and.lfflags) then
!
!  Are there one or two fitting flags?
!
    n1 = int(floats(nfloat-2))
    n2 = int(floats(nfloat-1))
    n3 = int(floats(nfloat))
!
!  Three flags
!
    nfloat = nfloat - 3
    if (lfit.and.n1.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 1
    endif
    if (lfit.and.n2.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 2
    endif
    if (lfit.and.n3.eq.1) then
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 4
      nfpot(nfit) = nfor
      nfvar(nfit) = 3
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol4(iline,llibrary,nvar1,itype1,symbol4(1,nfor),nvar2,itype2,symbol4(2,nfor), &
                     nvar3,itype3,symbol4(3,nfor),nvar4,itype4,symbol4(4,nfor),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nfor = nfor - 1
    nfit = nfit0
    goto 215
  endif
!
!  Assign coefficients and cutoffs
!
  mmfexc(nfor) = mmfexc(npot1)
  lfdreiding(nfor) = lfdreiding(npot1)
  n4botype(1:2,nfor) = n4botype(1:2,npot1)
  if (mmfexc(nfor).eq.1) then
    if (nfloat.ge.3) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
      forpoly(2,nfor) = floats(3+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif
  else
    if (nfloat.ge.6) then
      fork(nfor) = floats(1+nbeg)*units
      forpoly(1,nfor) = floats(2+nbeg)
      forpoly(2,nfor) = floats(3+nbeg)
      for1(nfor) = floats(4+nbeg)
      for2(nfor) = floats(5+nbeg)
      for3(nfor) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword4')
    endif  
  endif
  for4(nfor) = 0.0_dp
  nfspec1(nfor) = nvar1
  nfspec2(nfor) = nvar2
  nfspec3(nfor) = nvar3
  nfspec4(nfor) = nvar4
  nfptyp1(nfor) = itype1
  nfptyp2(nfor) = itype2
  nfptyp3(nfor) = itype3
  nfptyp4(nfor) = itype4
  goto 215
218 if (.not.l55) l1000 = .true.
  do nt = npot1,nfor
!
!  Copy intra/inter flags
!
    lfintra(nt) = lfintra(npot1)
    lfinter(nt) = lfinter(npot1)
  enddo
  lwordok = .true.
  return
!
  end
