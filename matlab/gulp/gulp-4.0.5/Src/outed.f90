  subroutine outed
!
!  Output dumpfile if requested due to error having occurred
!
!  10/08 Length of dfile that can be output extended to 56
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
!  Julian Gale, NRI, Curtin University, October2008
!
  use dump
  use iochannels
  implicit none
!
!  Write out restart file
!
  write(ioout,'(/)')
  if (idump.gt.0) then
    call dumpdur(idump,0_i4)
    if (dfile(1:1).ne.' ') then
      write(ioout,'(''  Dump file written as '',a56)') dfile(1:56)
    else
      write(ioout,'(''  Dump file written on channel '',i3)') idump
    endif
  endif
  write(ioout,'(/)')
!
  return
  end
