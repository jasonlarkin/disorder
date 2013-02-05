  subroutine initarc(iout,filename)
!
!  Initialises an arc file for writing subsequent frames with
!  the subroutine addframe2arc.
!
!   7/08 Created from setmd
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
!  Julian Gale, NRI, Curtin University, July 2008
!
  use datatypes
  use current,  only : ndim
  use parallel, only : ioproc
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)  :: iout     ! Output channel for file
  character(len=80), intent(in)  :: filename ! Filename for arc file
!
!  Local variables
!
!  None for now
!
  if (ioproc) then
    if (filename.ne.' ') then
      open(iout,file=filename,status='unknown',form='formatted')
    else
      open(iout)
    endif
    write(iout,'(''!BIOSYM archive 3'')')
    if (ndim.eq.3) then
      write(iout,'(''PBC=ON'')')
    elseif (ndim.eq.2) then
      write(iout,'(''PBC=2D'')')
    elseif (ndim.eq.1) then
      write(iout,'(''PBC=1D'')')
    else
      write(iout,'(''PBC=OFF'')')
    endif
  endif
!
  return
  end
