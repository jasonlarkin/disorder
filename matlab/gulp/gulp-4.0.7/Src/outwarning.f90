  subroutine outwarning(warningstring,iline)
!
!  Outputs warning message in standard form
!
!  11/01 Created from outerror 
!   6/05 Intent added
!   8/11 Format adjusted
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
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  character(len=*), intent(in) :: warningstring
  integer(i4),      intent(in) :: iline
!******************
!  Output header  *
!******************
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
    write(ioout,'(''!! WARNING : '',a)') warningstring
    if (iline.gt.0.and.iline.lt.1000000) then
      write(ioout,'(''!!         : Warning is apparently on line '',i6)') iline
    endif
    write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
  endif
!
  return
  end
