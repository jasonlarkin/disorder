  subroutine finish(lclose)
!
!  Performs tidying up and general output tasks at the
!  end of a run.
!
!   6/03 XML modifications added
!  11/04 Intent added
!  11/06 Unit 4 scratch file now deleted at finish
!   3/07 Chemshell modifications added
!   2/09 Old xml calls removed
!
!  lclose = if .true. then close and delete scratch files
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, June 2007
!
  use control
  use general,     only : nwarn
  use gulpchemsh
  use iochannels
  use parallel
  use reallocate
  implicit none
!
!  Passed variables
!
  logical, intent(in) :: lclose
!
  if (ioproc) then
!*************************
!  Remove scratch files  *
!*************************
    if (lclose) then
      close(41,status='delete')
      close(48,status='delete')
    endif
    close(4,status='delete')
!***********************
!  Output peak memory  *
!***********************
    call printmemory
!***************************
!  Output timing analysis  *
!***************************
    call outtime
!*****************
!  Output files  *
!*****************
    call outfile
    if (lclose) then
      close(42,status='delete')
    endif
    if (nwarn.gt.1) then
      write(ioout,'(/,''  **** GULP has completed with '',i3,'' warnings - beware! ****'',/)')nwarn
    elseif (nwarn.eq.1) then
      write(ioout,'(/,''  **** GULP has completed with 1 warning - beware! ****'',/)')
    endif
  endif
  call datetime(3_i4)
!*********************************************************
!  Close down message-passing (if appropriate) and stop  *
!*********************************************************
!         
! Chemshell - no mpfinish call
!       
  if (ichemsh_qm < 0) call mpfinish
!
  return
  end
