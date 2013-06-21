  subroutine inword(iline)
!
!  Processes input after keyword line
!
!   6/95 Minimiser switch option added
!   7/95 Indefinite number of title lines allowed
!   7/97 Potential tapering added
!  12/97 QEq options added
!  12/97 Multiple rspeed values added
!   4/98 Ignore option added
!   4/98 Modified to allow for previous first pass of input -\
!        data now read from channel 4.
!   5/98 Stress tensor specification added
!   7/98 History files added
!   8/98 FDF files added
!   3/99 DRV files added for interfacing to QMPOT
!   5/00 Options largely placed into genword subroutine so
!        that they are accessible from setlib as well.
!   7/00 lflags is now in control module
!   1/01 call to mcword added for Monte Carlo options
!   6/01 Initialisation of line added for benefit of some compilers
!   2/03 Made implicit none
!   7/03 Call for boword added
!   6/05 llibrary flag added as input to genword
!   7/06 Six-body potentials added
!   8/06 nru passed to linepro
!   8/06 Separate flag added for fitting flags
!  11/06 NEB modifications added
!  11/06 Stop now calls gulpfinish for clean exit
!  11/06 Call to check defect shell species added
!   1/07 nlibseps & nlibatab added
!   3/07 sort renamed to GULP_sort
!   5/07 nlibbondQ added
!   7/07 nlibreaxFFspec added and call to setreaxFF
!   4/08 ncurr removed from boword argument list
!  12/08 Module input renamed to gulpinput
!   6/09 Module name changed from three to m_three
!   6/09 PDFword call added
!   9/09 Value of nru now replaced with iotmp from iomod
!   9/09 close(5) statement removed 
!   1/10 One-body potentials added
!   8/11 nreaxFFfixQspec added
!   5/13 Removal of duplicate library names added
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, August 2011
!
  use bondcharge,     only : nbondQ
  use bondorderdata
  use constants
  use control
  use defects,        only : ndef
  use four
  use gulpinput
  use iochannels,     only : ioin, iotmp
  use library
  use m_pdfneutron,   only : pdfword
  use m_three
  use one,            only : none
  use parallel
  use reaxFFdata
  use six
  use species
  use two
  use uffdata,        only : nUFFspec
  implicit none
!
!  Passed variables
!
  integer(i4),   intent(inout) :: iline
!
!  Local variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: i
  integer(i4)                  :: ilib
  integer(i4)                  :: ind1
  integer(i4)                  :: ind2
  integer(i4)                  :: ncurr
  logical                      :: l55
  logical                      :: l1000
  logical                      :: lbflag
  logical                      :: lduplicate
  logical                      :: lfflags
  logical                      :: lignore
  logical                      :: linr = .true.
  logical                      :: lint = .true.
  logical                      :: lspace
  logical                      :: lwordok
!
!  Initialise local variables
!
  lbflag = lflags
  lignore = .false.
  if (lbulknoopt) lbflag = .false.
  ncurr = 1
  lfflags = .true.
!
!  Read option and data lines
!
50 line = '  '
  read(iotmp,'(a)',end=1000) line
  iline = iline + 1
  call linepro(iotmp,line,iline)
  if (index(words(1),'help').ne.0) then
    call help
    goto 50
  endif
  if (nword.eq.0) goto 50
55 word = words(1)(1:20)
  call stolc(word,20_i4)
  lwordok = .false.
!
!  Ignore/erongi construct
!
  if (lignore) then
    if (index(word,'eron').eq.1) then
      lignore = .false.
    endif
    goto 50
  endif
  if (index(word,'igno').eq.1) then
    lignore = .true.
    goto 50
  endif
!
!  Start run option
!
  if (index(word,'star').eq.1) goto 1000
!
!  Skip keyword option on second pass
!
  if (index(word,'key').eq.1) goto 50
!
!  Options
!
  if (index(word,'stop').eq.1) then
    call gulpfinish
  endif
  if (index(word,'lib').eq.1)  goto 100
  l55 = .false.
  l1000 = .false.
  call genword(iotmp,word,lwordok,iline,line,l55,l1000,.false.,lfflags,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call strword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr,lbflag)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call surfword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr,lbflag)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call potword21(iotmp,word,lwordok,iline,line,l55,l1000,linr,lint,.false.,lfflags,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call potword22(iotmp,word,lwordok,iline,line,l55,l1000,linr,lint,.false.,lfflags,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call potword3(iotmp,word,lwordok,iline,line,l55,l1000,linr,lint,.false.,lfflags)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call potword4(iotmp,word,lwordok,iline,line,l55,l1000,linr,lint,.false.,lfflags)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call potword6(iotmp,word,lwordok,iline,line,l55,l1000,linr,lint,.false.,lfflags)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call potwordm(iotmp,word,lwordok,iline,line,l55,l1000,linr,lint,.false.,lfflags,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call boword(iotmp,word,lwordok,iline,line,l55,l1000,linr,lint,.false.,lfflags)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call specword(iotmp,word,lwordok,iline,line,l55,l1000)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call fitword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call phonword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call gaword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call defword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call mdword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call mcword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call nebword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call pdfword(iotmp,word,lwordok,iline,line,l55,l1000,ncurr)
  if (l55) goto 55
  if (l1000) goto 1000
  if (lwordok) goto 50
  call outerror('Unrecognised option in input = '//word,iline)
  call stopnow('inword')
!**************************
!  Library specification  *
!**************************
100 if (nword.eq.1) then
    call outerror('Library name missing from specification',0_i4)
    call stopnow('inword')
  endif
  do i = 2,nword
    if (index(words(i),'nodu').eq.1) then
      llibdump = .false.
    else
      nlib = nlib + 1
      if (nlib.gt.maxlib) then
        maxlib = nlib + 1
        call changemaxlib
      endif
      ind1 = index(line,words(i)(1:4))
      lspace = .false.
      ind2 = ind1
      do while (.not.lspace.and.ind2.lt.80)
        ind2 = ind2 + 1
        lspace = (line(ind2:ind2).eq.' ')
      enddo
      if (ind2-ind1.gt.59) ind2 = ind1 + 59
      libname(nlib) = line(ind1:ind2)
      if (nlib.gt.1) then
!
!  Check for duplicate library names
!
        lduplicate = .false.
        ilib = 1
        do while (ilib.lt.nlib.and..not.lduplicate) 
          lduplicate = (libname(ilib).eq.libname(nlib))
          ilib = ilib + 1
        enddo
!
!  If duplicate found then remove
!
        if (lduplicate) then
          nlib = nlib - 1
        endif
      endif
    endif
  enddo
  goto 50
!*****************************
!  End of input for options  *
!*****************************
!
!  Check that input is sufficient
!
1000 close(ioin)
!
!  Setup species
!
  call setspec
!
!  nlib1s,nlib2s,nlib3s,nlib4s etc are pointers to the end of the
!  non-library potentials for restart file purposes
!
  nlib1s = none
  nlib2s = npote
  nlib3s = nthb
  nlib4s = nfor
  nlib6s = nsix
  nlibatab = natab
  nlibbondQ = nbondQ
  nlibsp = nspec
  nlibseps = nseps
  nlibnboA = nboA
  nlibnboR = nboR
  nlibnboQ = nboQ
  nlibnboQ0 = nboQ0
  nlibnbopot = nbopot
  nlibUFFsp = nUFFspec
  nlibreaxFFspec = nreaxFFspec
  nlibreaxFFfixQspec = nreaxFFfixQspec
!****************************
!  Potential library input  *
!****************************
  if (nlib.gt.0) call setlib(ncurr)
!
!  Remove automatically added defect shells if not present in library
!
  if (ndef.gt.0) call checkdefshspec
!
!  Potential setup tasks
!
  if (nUFFspec.gt.0) call setuff
  if (npote.gt.0) call setpote
  if (nthb.gt.0) call setthree
  if (nfor.gt.0) call setfour
  if (nsix.gt.0) call setsix
  if (nbopot.gt.0) call setbondorder
  if (nreaxFFspec.gt.0) call setreaxFF
!****************************
!  Sort growth slice atoms  *
!****************************
  call GULP_sort(3_i4)
!*************************************************************************
!  Sort cores and shells in order to speed up phonon and property calcs  *
!*************************************************************************
  call GULP_sort(1_i4)
!************************************
!  Sort QM atoms from non-QM atoms  *
!************************************
  call GULP_sort(2_i4)
  return
!
  end
