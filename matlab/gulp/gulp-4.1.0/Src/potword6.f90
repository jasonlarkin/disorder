  subroutine potword6(iin,word,lwordok,iline,line,l55,l1000,linr,lint,llibrary,lfflags)
!
!  Processes potential input for six body potentials
!
!  iin = input fortran channel
!
!  llibrary = if .true. then this is a library input call
!
!  nsixty = 1 => cross - out of plane potential
!
!  Molecular Mechanics:
!  --------------------
!
!  mmsexc specifies molecular mechanics type for six-body potential types:
!
!    0 => bonded and nonbonded potential (default)
!    1 => bonded only potential
!
!  option word "molmec" after potential name selects default MM type
!  for marvin compatibility
!
!  11/04 Created based on potword4
!   8/06 iin passed to linepro
!   8/06 Separate flag added for fitting flags
!   8/06 Bonding type sub-option added
!   9/06 Literal symbols returned from getpotsymbol6
!   1/07 Amide bond type added
!  12/07 Unused variables removed
!  12/08 Module input renamed to gulpinput
!   5/13 Custom bond type added
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
  use constants
  use control
  use element, only : maxele
  use fitting
  use general, only : nwarn
  use gulpinput
  use iochannels
  use molecule
  use parallel
  use six
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
  integer(i4)                  :: itype5
  integer(i4)                  :: itype6
  integer(i4)                  :: n1
  integer(i4)                  :: nbeg
  integer(i4)                  :: nfit0
  integer(i4)                  :: npot1
  integer(i4)                  :: nt
  integer(i4)                  :: nvar1
  integer(i4)                  :: nvar2
  integer(i4)                  :: nvar3
  integer(i4)                  :: nvar4
  integer(i4)                  :: nvar5
  integer(i4)                  :: nvar6
  logical                      :: lflag1
  logical                      :: lfound
  logical                      :: lsymbol
  logical                      :: lvalidpot
  logical                      :: lwarnp
  real(dp)                     :: d1
  real(dp)                     :: units
!
!  Initialise local variables
!
  lwarnp = .not.lmol
  if (index(word,'xout').eq.1) goto 100
  return
!***********************************
!  Cross - out of plane potential  *
!***********************************
100 if (nsix.eq.maxsix) then
    maxsix = nsix + 10
    call changemaxsix
  endif
!
!  Set intra / inter / both flags
!
  lfound = .false.
  npot1 = nsix + 1
  units = 1.0_dp
  if (nword.gt.1) then
    i = 2
    do while (i.le.nword)
      if (index(words(i),'inte').eq.1) then
        lsintra(nsix+1) = .false.
        lsinter(nsix+1) = .true.
        lfound = .true.
      elseif (index(words(i),'intr').eq.1) then
        lsintra(nsix+1) = .true.
        lsinter(nsix+1) = .false.
        lfound = .true.
      elseif (index(words(i),'bot').eq.1) then
        lsintra(nsix+1) = .true.
        lsinter(nsix+1) = .true.
        lfound = .true.
      elseif (index(words(i),'bon').eq.1) then
        mmsexc(nsix+1) = 1
        lsintra(nsix+1) = .true.
        lsinter(nsix+1) = .false.
        lfound = .true.
      elseif (index(words(i),'mol').eq.1) then
        mmsexc(nsix+1) = 1
        lsintra(nsix+1) = .true.
        lsinter(nsix+1) = .false.
        lfound = .true.
      elseif (index(words(i),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(i),'kj').eq.1) then
        units = kjmtoev
      elseif (index(words(i),'sin').eq.1) then
        n6botype(1,nsix+1) = 1
      elseif (index(words(i),'dou').eq.1) then
        n6botype(1,nsix+1) = 2
      elseif (index(words(i),'tri').eq.1) then
        n6botype(1,nsix+1) = 3
      elseif (index(words(i),'qua').eq.1) then
        n6botype(1,nsix+1) = 4
      elseif (index(words(i),'res').eq.1) then
        n6botype(1,nsix+1) = 5
      elseif (index(words(i),'ami').eq.1) then
        n6botype(1,nsix+1) = 6
      elseif (index(words(i),'cus').eq.1) then
        n6botype(1,nsix+1) = 7
      elseif (index(words(i),'cyc').eq.1) then
        n6botype(2,nsix+1) = 2
      elseif (index(words(i),'exo').eq.1) then
        n6botype(2,nsix+1) = 3
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
    lsintra(nsix+1) = lint
    lsinter(nsix+1) = linr
  endif
105 line = '  '
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
  nsix = nsix + 1
!
!  Set flags to disable free energy minimisation and M-L method
!
  lnoanald3 = .true.
  lnomottlittleton = .true.
  if (nsix.gt.maxsix) then
    maxsix = nsix + 10
    call changemaxsix
  endif
  nsixty(nsix) = 1
!
!  Fitting flags
!
  nfit0 = nfit
  if (lfflags.and.nfloat.gt.1) then
!
!  Are there one or two fitting flags?
!
    d1 = floats(nfloat)
    n1 = nint(d1)
    lflag1 = (abs(d1-1.0_dp).lt.1.0d-10.or.abs(d1).lt.1.0d-10)
    if (lflag1) then
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
        nftyp(nfit) = 7
        nfpot(nfit) = nsix
        nfvar(nfit) = 1
      endif
    endif
  endif
!
!  Process symbol input
!
  call getpotsymbol6(iline,llibrary,nvar1,itype1,symbol6(1,nsix),nvar2,itype2,symbol6(2,nsix),nvar3,itype3,symbol6(3,nsix), &
                     nvar4,itype4,symbol6(4,nsix),nvar5,itype5,symbol6(5,nsix),nvar6,itype6,symbol6(6,nsix),nbeg,lvalidpot)
  if (.not.lvalidpot) then
    nsix = nsix - 1
    nfit = nfit0
    goto 105
  endif
!
!  Assign coefficients and cutoffs
!
  mmsexc(nsix) = mmsexc(npot1)
  n6botype(1:2,nsix) = n6botype(1:2,npot1)
  if (mmsexc(nsix).eq.1) then
    if (nfloat.ge.1) then
      sixk(nsix) = floats(1+nbeg)*units
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword6')
    endif
  else
    if (nfloat.ge.6) then
      sixk(nsix) = floats(1+nbeg)*units
      six1(nsix) = floats(2+nbeg)
      six2(nsix) = floats(3+nbeg)
      six3(nsix) = floats(4+nbeg)
      six4(nsix) = floats(5+nbeg)
      six5(nsix) = floats(6+nbeg)
    else
      call outerror('Incorrect potential coefficient input',iline)
      call stopnow('potword6')
    endif  
  endif
  nsspec1(nsix) = nvar1
  nsspec2(nsix) = nvar2
  nsspec3(nsix) = nvar3
  nsspec4(nsix) = nvar4
  nsspec5(nsix) = nvar5
  nsspec6(nsix) = nvar6
  nsptyp1(nsix) = itype1
  nsptyp2(nsix) = itype2
  nsptyp3(nsix) = itype3
  nsptyp4(nsix) = itype4
  nsptyp5(nsix) = itype5
  nsptyp6(nsix) = itype6
  goto 105
108 if (.not.l55) l1000 = .true.
  do nt = npot1,nsix
!
!  Copy intra/inter flags
!
    lsintra(nt) = lsintra(npot1)
    lsinter(nt) = lsinter(npot1)
  enddo
  lwordok = .true.
  return
!
  end
