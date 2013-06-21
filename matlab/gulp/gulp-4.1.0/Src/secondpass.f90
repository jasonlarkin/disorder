  subroutine secondpass
!
!  Second pass over input file to obtain estimate of key dimensions. It is
!  not critical that estimate is accurate but it minimises the extent of 
!  allocating and deallocating (array growth). This is important because
!  some machines do not clean up the stack while the code is running, leading
!  to excessive memory use.
!
!  Assumes that input has been previous processed by firstpass and placed on
!  unit 4.
!
!   5/03 Created
!   9/03 Handling of file end modified
!   8/06 I/O channel passed to linepro
!  11/06 NEB modifications added
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
  use configurations, only : maxatot, maxcfg
  use control
  use current,        only : maxat
  use gulpinput
  use iochannels
  use neb,            only : maxnebreplicatot
  use parallel
  implicit none
!
!  Local variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: i
  integer(i4)                  :: iline
  integer(i4)                  :: estmaxat
  integer(i4)                  :: estmaxatot
  integer(i4)                  :: estmaxcfg
  integer(i4)                  :: estmaxnebreplicatot
  integer(i4)                  :: natoms
  logical                      :: latomsfound
  logical                      :: ldittofound
  logical                      :: lreplicafound
  logical                      :: lsymbol
!
!  Initialise estimated values
!
  estmaxat = 0
  estmaxatot = 0
  estmaxcfg = 0
  estmaxnebreplicatot = 0
  natoms = 0
!**********************************
!  Loop over lines of input file  *
!**********************************
  iline = 0
10 line = ' '
!
!  Fetch next line
!
  read(4,'(a)',end=100) line
  iline = iline + 1
!
!  Process line
!
  call linepronc(line,iline)
!
!  Put the words into lower case
!
  do i = 1,nword
    call stolc(words(i),maxword)
  enddo
!****************************************************************
!  Check for options that have implications for key dimensions  *
!****************************************************************
  latomsfound = .false.
  ldittofound = .false.
  lreplicafound = .false.
  if (index(words(1),'cart').eq.1) latomsfound = .true.
  if (index(words(1),'frac').eq.1) latomsfound = .true.
  if (index(words(1),'sfra').eq.1) latomsfound = .true.
  if (index(words(1),'pfra').eq.1) latomsfound = .true.
  if (index(words(1),'ditt').eq.1) ldittofound = .true.
  if (index(words(1),'rcar').eq.1) lreplicafound = .true.
  if (index(words(1),'rfra').eq.1) lreplicafound = .true.
!**************************************************************************************
!  If set of atoms was found then increment number of configurations and count atoms  *
!**************************************************************************************
  if (latomsfound) then
    estmaxcfg = estmaxcfg + 1
    natoms = 0
!
!  Start of input loop within atoms found section
!
20   line = '  '
    read(4,'(a)',end=22) line
    iline = iline + 1
    call linepro(4_i4,line,iline)
!
!  Check whether input is symbol or option
!
    if ((nword+nfloat).eq.0) goto 20
!
!  Check for old fashion specification of number of atoms
!
    if (nword.eq.0.and.nfloat.eq.1) goto 20
    if (nword.gt.0) then
      word = words(1)(1:20)
      call stolc(word,20_i4)
      call worsy(word,lsymbol,.false.)
      if (.not.lsymbol) then
        goto 25
      endif
    endif
    natoms = natoms + 1
    goto 20
!
!  End of file
!
22   estmaxat = max(estmaxat,natoms)
    estmaxatot = estmaxatot + natoms
    goto 100
!
!  End of input loop for atoms found section
!
25   continue
!
!  Update counters
!
    estmaxat = max(estmaxat,natoms)
    estmaxatot = estmaxatot + natoms
  endif
!***********************
!  Ditto option found  *
!***********************
  if (ldittofound) then
    estmaxcfg = estmaxcfg + 1
    estmaxatot = estmaxatot + natoms
  endif
!**********************
!  NEB replica found  *
!**********************
  if (lreplicafound) then
    estmaxnebreplicatot = estmaxnebreplicatot + 1
  endif
!     
!  End of processing of this line -> go back to read 
!
  goto 10
!
!  End of reading
!
100 continue
!********************************************
!  Set and update values of key dimensions  *
!********************************************
  if (estmaxcfg.gt.maxcfg) then
    maxcfg = estmaxcfg
    call changemaxcfg
  endif
  if (estmaxat.gt.maxat) then
    maxat = estmaxat
    call changemaxat
  endif
  if (estmaxatot.gt.maxatot) then
    maxatot = estmaxatot
    call changemaxatot
  endif
  if (estmaxnebreplicatot.gt.maxnebreplicatot) then
    maxnebreplicatot = estmaxnebreplicatot
    call changemaxnebreplicatot
  endif
!
!  For debugging output estimates
!
!      if (ioproc) then
!        write(ioout,'(/,''  Estimated dimensions from second pass :'',/)')
!        write(ioout,'(''  maxat            = '',i8)') maxat
!        write(ioout,'(''  maxatot          = '',i8)') maxatot
!        write(ioout,'(''  maxcfg           = '',i8)') maxcfg
!        write(ioout,'(''  maxnebreplicatot = '',i8)') maxnebreplicatot
!        write(ioout,'(/)')
!      endif
  return
  end
