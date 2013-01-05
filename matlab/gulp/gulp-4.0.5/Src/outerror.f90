  subroutine outerror(errorstring,iline)
!
!  Outputs error message in standard form
!
!   4/01 Created 
!   5/01 iline optional print out added
!   6/05 Intent added
!   7/06 Call to gflush added
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, July 2006
!
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  character(len=*), intent(in) :: errorstring
  integer(i4),      intent(in) :: iline
!******************
!  Output header  *
!******************
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
    write(ioout,'(''!! ERROR : '',a)') errorstring
    if (iline.gt.0.and.iline.lt.1000000) then
      write(ioout,'(''!!       : Error is apparently on line '',i6)') iline
    endif
    write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
    call gflush(ioout)
  endif
!
  return
  end
