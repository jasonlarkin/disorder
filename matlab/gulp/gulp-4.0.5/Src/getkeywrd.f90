  subroutine getkeyword(iline)
!
!  Reads in keywords line and allows for multiple lines
!  Also sets general flags
!
!   5/02 lfbfgs added
!   6/03 llbfgs added
!   2/04 Processing of keywords moved to a separate routine
!   3/07 goto removed
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
  use control
  use gulpinput
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4), intent(inout)  :: iline
!
!  Local variables
!
  character(len=maxlinelength) :: line
  character(len=20)            :: word
  integer(i4)                  :: i
  integer(i4)                  :: iblank
  integer(i4)                  :: nptr
  logical                      :: lfound
  logical                      :: lHelp
!
!  Get the keywords as words
!
  lHelp = .false.
  do while (.not.lHelp)
    line = '  '
    read(4,'(a)',end=100,err=110) line
    iline = iline + 1
    if (index(line,'#').ne.1) then
      call linepro(4_i4,line,iline)
      lHelp = (index(words(1),'help').ne.0) 
      if (lHelp) then
        call help
      else
        exit
      endif
    endif
  enddo
!
!  Find the first location to put a keyword on the keyword line
!
  i = 400
  lfound = .false.
  do while (.not.lfound.and.i.gt.0)
    if (keyword(i:i).ne.' ') then
      nptr = i + 2
      lfound = .true.
    endif
    i = i - 1
  enddo
  if (.not.lfound) nptr = 1
!
!  Put the words on to the keyword line in lower case
!
  do i = 1,nword
    word = words(i)(1:20)
    call stolc(word,20_i4)
    iblank = index(word,' ')
    if (iblank.eq.0) iblank = 21
    keyword(nptr:nptr+iblank-2) = word(1:iblank-1)
    nptr = nptr + iblank
    if (nptr.gt.400) then
      call outerror('keyword line has exceeded maximum length',0_i4)
      call stopnow('getkeyword')
    endif
  enddo
!
!  If one of the keywords is stop, then stop!
!
  if (index(keyword,'stop').ne.0) then
    if (ioproc) then
      write(ioout,'(/,''Terminating GULP as requested'',/)')
    endif
    call stopnow('getkeyword')
  endif
!
  return
!
!  Error handling
!
100 call outerror('input file is empty',0_i4)
  call stopnow('getkeyword')
110 call outerror('error reading keyword line',0_i4)
  call stopnow('getkeyword')
!
  end
