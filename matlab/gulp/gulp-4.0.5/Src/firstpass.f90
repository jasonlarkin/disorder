  subroutine firstpass
!
!  Performs first pass of input file before proper reading in and processing
!  begins. The tasks performed during this first pass are as follows:
!
!    (1) Check for any "keyword" option lines as it is important that all
!        keywords present are known about before other options are read.
!    (2) Write out input to channel 4 for second pass.
!
!  10/02 Start option allowed for
!  11/06 Stop command now calls gulpfinish for clean exit
!  12/08 Module input renamed to gulpinput
!   6/09 Name of getline changed to gulp_getline
!  12/09 Check added to prevent adding duplicate keywords to the keyword line
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, December 2009
!
  use control
  use gulpinput
  use parallel
  implicit none
!
!  Local variables
!
  character(len=20)            :: word
  character(len=maxlinelength) :: line
  integer(i4)                  :: i
  integer(i4)                  :: iblank
  integer(i4)                  :: iline
  integer(i4)                  :: nptr
  logical                      :: lend
!
  nptr = 1
  iline = 0
!**********************************
!  Loop over lines of input file  *
!**********************************
!
10 continue
!
!  Fetch next line
!
  call gulp_getline(line,lend)
!
!  Check for end of input file
!
  if (lend) goto 100
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
  if (index(words(1),'help').ne.0) then
!
!  Check for help
!
    call help
    goto 10
  else
!
!  Write line
!
    write(4,'(a)') line
    iline = iline + 1
  endif
!
!  Check for comment line
!
  if (index(words(1),'#').eq.1) goto 10
!
!  Check for blank line
!
  if ((nfloat+nword).eq.0) goto 10
!
!  Check for stop
!
  if (index(words(1),'stop').eq.1) call gulpfinish
!
!  Check for run
!
  if (index(words(1),'run').eq.1) goto 100
!
!  Check for start
!
  if (index(words(1),'star').eq.1) goto 100
!
!  Is this a keyword line?
!
  if (index(words(1),'key').eq.1) goto 200
!
!  End of processing of this line -> go back to read
!
  goto 10
!
!  End of reading
!
100 continue
  return
!*********************
!  Get the keywords  *
!*********************
!
!  Keyword option line - add to keyword line
!
200 do i = 2,nword
    word = words(i)(1:20)
!
!  Check whether word is already in keyword line - if so, don't duplicate
!
    iblank = index(word,' ')
    if (iblank.eq.0) iblank = 21
    if (index(keyword,word(1:iblank-1)).eq.0) then
      if (nptr+iblank.gt.400) then
        call outerror('keyword line has exceeded maximum length',0_i4)
        stop
      endif
      keyword(nptr:nptr+iblank-2) = word(1:iblank-1)
      nptr = nptr + iblank
    endif
  enddo
  goto 10
!
  end
