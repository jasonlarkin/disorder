  subroutine outofmemory(routine,arrayname)
!
!  Handles an error due to memory exceeded
!
!  Julian Gale, Curtin University, July 2005
!
  use iochannels
  use parallel
!
  implicit none
  character(len=*) :: routine, arrayname
!
!  Write out error message
!
  call outerror('memory allocation failed',0_i4)
  write(ioout,'(''  Calling routine = '',a)') routine
  write(ioout,'(''  Array name      = '',a,/)') arrayname
!
!  Perform final tasks before stopping
!
  call finish(.true.)
!
  end

  subroutine deallocate_error(routine,arrayname)
!
!  Handles an error at deallocation time
!
!  Victor Milman, Accelrys, September 2006
!
  use iochannels
  use parallel
!
  implicit none
  character(len=*) :: routine, arrayname
!
!  Write out error message
!
  call outerror('memory deallocation failed',0_i4)
  write(ioout,'(''  Calling routine = '',a)') routine
  write(ioout,'(''  Array name      = '',a,/)') arrayname
!
!  Perform final tasks before stopping
!
  call finish(.true.)
!
  end
