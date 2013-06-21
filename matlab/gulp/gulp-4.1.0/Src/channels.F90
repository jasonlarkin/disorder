  subroutine channels(nunit,lformatted)
!
!  Opens scratch file channels with a unique name
!  to avoid compilcations when two jobs are running
!  in the same directory. Has 9999 tries at finding
!  a different name - crude but simple! Anyone who
!  has more than 10000 scratch files clearly has
!  too big a disk quota and has never listed the
!  contents of their directroy!
!
!  nunit      = unit number to be opened as a temporary file
!  lformatted = if .true. then file will be opened for 
!               formatted i/o
!
!   3/97 Created
!  10/97 Modified to handle input general channel number
!   9/03 Node number included in file name
!   5/06 Handling of negative unit numbers removed
!  11/06 Error in error format statement fixed
!  11/06 Call to ftow replaced with itow
!  11/07 Unused variables cleaned up
!   6/12 File handling changed to allow for spaces in names
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
!  Julian Gale, NRI, Curtin University, June 2012
!
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: nunit
  logical,     intent(in)  :: lformatted
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: ii
  integer(i4)              :: ind
  integer(i4)              :: indp
  integer(i4)              :: inq
  integer(i4)              :: n
  logical                  :: lexist
  character(len=20)        :: filename
  character(len=5)         :: cnum
  character(len=5)         :: cproc
  character(len=5)         :: cunit
!
#ifdef MPI
  include 'mpif.h'
  integer           :: ierr
#endif
!********************
!  Build file name  *
!********************
  filename = ' '
  filename(1:8) = 'gulptmp_'
!
!  For parallel runs include node number
!
  if (nprocs.gt.1) then
    if (procid.gt.999) then
      write(cproc,'(i4)') procid
      filename(9:12) = cproc(1:4)
      indp = 12
    elseif (procid.gt.99) then
      write(cproc,'(i3)') procid
      filename(9:11) = cproc(1:3)
      indp = 11
    elseif (procid.gt.9) then
      write(cproc,'(i2)') procid
      filename(9:10) = cproc(1:2)
      indp = 10
    else
      write(cproc,'(i1)') procid
      filename(9:9) = cproc(1:1)
      indp = 9
    endif
    indp = indp + 1
    filename(indp:indp) = '_'
  else
    indp = 8
  endif
!
  if (nunit.gt.9999) then
    write(cunit,'(i5)') nunit
  elseif (nunit.gt.999) then
    write(cunit,'(i4)') nunit
  elseif (nunit.gt.99) then
    write(cunit,'(i3)') nunit
  elseif (nunit.gt.9) then
    write(cunit,'(i2)') nunit
  else
    write(cunit,'(i1)') nunit
  endif
  filename(indp+1:indp+5) = cunit
  call endstring(filename,len(filename),ind)
  filename(ind:ind) = '_'
  do n = 1,9999
    call itow(cnum,n,5_i4)
    ii = index(cnum,'.')
    if (ii.gt.0) then
      do i = ii,5
        cnum(i:i) = ' '
      enddo
    endif
    filename(ind+1:ind+5) = cnum
    inquire(file=filename,exist=lexist,iostat=inq)
    if (inq.ne.0) goto 10
    if (.not. lexist) goto 20
  enddo
!
!  If execution reaches this point no suitable filename has been found
!
  write(ioout,'(a)') ' Could not find unique filename'
  goto 999

10 write(ioout,'(a)') ' Inquire failed'
  goto 999

20 if (lformatted) then
    open(nunit,file=filename,form='formatted',status='new',err=998)
  else
    open(nunit,file=filename,form='unformatted',status='new',err=998)
  endif
  rewind(nunit)
  return

998 if (ioproc) write(ioout,'(a)') 'error opening file '//filename

999 if (ioproc) then
    write(ioout,'(a,i5)') ' Program terminated in subroutine channels attempting to open unit ',nunit
  endif
  call stopnow('channels')
  end
