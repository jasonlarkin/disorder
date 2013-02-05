subroutine trap_sigint
!
!  Trap control C during execution
!
  use interupt,   only : controlC
  use iochannels, only : ioout
  use parallel,   only : ioproc
  implicit none
!
  if (ioproc) then 
    write(ioout,'(''CTRL-C: Will exit current iterative process at end of the cycle'')')
    controlC = .true.
  endif
  return

end subroutine trap_sigint
