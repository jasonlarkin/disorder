  subroutine help
!
!  Help for GULP
!  Reads info from help.txt
!
!  Note : several integer numbers exceed range for double
!  precision and therefore have to be quad words.
!
!   7/96 Ability to handle numbers added
!  12/96 All numbers treated as being the same - reduces index
!        range so that i8 can be avoided as this is not OK on
!        lots of machines.
!   8/98 Temporary file on channel 4 now removed when stop
!        command is given.
!   7/00 maxhelp is now a local variable
!   7/06 f90 advance I/O option replaces dollar sign
!   1/08 Unused variables removed
!   5/08 Modified to use string comparison rather than indexing
!  12/08 Module input renamed to gulpinput
!   6/09 Bug trapped for case where number of help words found
!        exceeds dimension of iptr
!   8/11 Line length increased for help text to 132
!  10/12 Position of deallocate for iptr moved to exit point
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
!  Julian Gale, NRI, Curtin University, October 2012
!
  use general
  use gulpinput
  use iochannels
  use parallel
  implicit none
!
!  Local variables
!
  character(len=20),           dimension(:), allocatable       :: hword
  character(len=132)                                           :: line
  character(len=6)                                             :: word
  integer(i4)                                                  :: i
  integer(i4)                                                  :: iline
  integer(i4),                 dimension(:), allocatable       :: iptr
  integer(i4)                                                  :: j
  integer(i4)                                                  :: maxhelp = 600
  integer(i4)                                                  :: n
  integer(i4)                                                  :: ndiff
  integer(i4)                                                  :: nend
  integer(i4),                 dimension(:), allocatable       :: nfinish
  integer(i4)                                                  :: nhw
  integer(i4)                                                  :: nptr
  integer(i4),                 dimension(:), allocatable       :: nstart
  integer(i4)                                                  :: nstr
  integer(i4)                                                  :: status
  logical                                                      :: leof
!
!  Initialise data
!
10 nhw = 0
!
!  Allocate local memory
!
  allocate(hword(maxhelp),stat=status)
  if (status/=0) call outofmemory('help','hword')
  allocate(nstart(maxhelp),stat=status)
  if (status/=0) call outofmemory('help','nstart')
  allocate(nfinish(maxhelp),stat=status)
  if (status/=0) call outofmemory('help','nfinish')
!
!  Open help file
!
  open(7,file='help.txt',status='old',err=30)
  goto 40
30 open(7,file=helpfile,status='old',err=50)
40 continue
  leof = .false.
  iline = 0
!
!  Read through help file and locate help keywords
!
  do while (.not.leof)
    read(7,'(a)',end=50) line
    iline = iline + 1
    if (index(line,'@@').eq.1) then
      nhw = nhw + 1
      if (nhw.le.maxhelp) then
        hword(nhw) = line(3:22)
        nstart(nhw) = iline + 1
        if (nhw.gt.1) nfinish(nhw-1) = iline - 1
      endif
    endif
  enddo
  if (nhw.gt.maxhelp) then
!
!  Increase maxhelp and do again
!
    maxhelp = nhw
    deallocate(nfinish,stat=status)
    if (status/=0) call deallocate_error('help','nfinish')
    deallocate(nstart,stat=status)
    if (status/=0) call deallocate_error('help','nstart')
    deallocate(hword,stat=status)
    if (status/=0) call deallocate_error('help','hword')
    close(7)
    goto 10
  endif
50 if (nhw.eq.0) then
    if (ioproc) then
      write(ioout,'(/,''  **** No Help information could be found ****'',/)')
    endif
    goto 999
  endif
  allocate(iptr(nhw),stat=status)
  if (status/=0) call outofmemory('help','iptr')
  nfinish(nhw) = iline
  rewind(7)
!
!  Help banner
!
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''********************************************************************************'')')
    write(ioout,'(''               Welcome to the GULP help system'')')
    write(ioout,'(''********************************************************************************'')')
    write(ioout,'(''  For help on a keyword, type the keyword'')')
    write(ioout,'(''  Only the first four letters are significant'')')
    write(ioout,'(''  To obtain a list of topics, type "topics"'')')
    write(ioout,'(''  To leave the help system:'')')
    write(ioout,'(''    stop => terminate program'')')
    write(ioout,'(''    quit => to continue with a run'')')
  endif
!
!  Read user input and perform basic interpretation
!
100 if (ioproc) write(ioout,'(/,''  GULP help on ? '')',advance='no')
  read(ioin,'(a)') word
  call stolc(word,6_i4)
  if (index(word,'stop').ne.0) then
    close(4,status='delete')
    stop
  endif
  if (index(word,'quit').ne.0) then
    close(7)
    goto 999
  endif
  if (index(word,'topi').ne.0) then
    if (ioproc) then
      write(ioout,'(/,''  *****************'')')
      write(ioout,'(''  *  Help topics  *'')')
      write(ioout,'(''  *****************'',/)')
      write(ioout,'(4a20)')(hword(i),i=1,nhw)
    endif
    goto 100
  endif
  nend = 0
  j = 2
  do while (j.le.6) 
    if (word(j:j).eq.' ') then
      nend = j - 1
      j = j + 6
    endif
    j = j + 1
  enddo
  if (nend.eq.0) nend = 6
!
!  Look for match at start of string
!
  i = 1
  nptr = 0
  do while (i.le.nhw)
    ndiff = index(hword(i),word(1:nend))
    if (ndiff.eq.1) then
      nptr = nptr + 1
      iptr(nptr) = i
    endif
    i = i + 1
  enddo
  if (nptr.eq.0) then
!
!  Find best match anywhere in string 
!
    i = 1
    nptr = 0
    do while (i.le.nhw)
      ndiff = index(hword(i),word(1:nend))
      if (ndiff.gt.0) then
        nptr = nptr + 1
        iptr(nptr) = i
      endif
      i = i + 1
    enddo
  endif
!
!  Output help
!
  if (nptr.ge.1.and.ioproc) then
    write(ioout,'(/)')
    if (nptr.gt.1) then
      write(ioout,'(''  No. of valid topics found = '',i3,/)') nptr
    endif
    do n = 1,nptr
      nstr = nstart(iptr(n))
      do i = 1,nfinish(iptr(n))
        read(7,'(a)') line
        if (i.ge.nstr) then
          write(ioout,'(a)') line
        endif
      enddo
      write(ioout,'(/)')
      rewind(7)
    enddo
  elseif (ioproc) then
    write(ioout,'(/,''  **** No valid help topic could be found for this subject  ****'')')
  endif
  goto 100
!***************
!  Exit point  *
!***************
999 continue
  deallocate(iptr,stat=status)
  if (status/=0) call deallocate_error('help','iptr')
  deallocate(nfinish,stat=status)
  if (status/=0) call deallocate_error('help','nfinish')
  deallocate(nstart,stat=status)
  if (status/=0) call deallocate_error('help','nstart')
  deallocate(hword,stat=status)
  if (status/=0) call deallocate_error('help','hword')
!
  return
  end
