subroutine trap_sigint_fit
!
!  Trap control C during execution of fitting
!
  use datatypes
  use interupt,   only : controlC_fit
  use iochannels, only : ioout
  use parallel,   only : ioproc
  implicit none
!
  if (.not.controlC_fit) then
!
!  First call is to exit iterative process
!
    if (ioproc) then
      write(ioout,'(''CTRL-C: Will exit current iterative process at end of the cycle'')')
    endif
    controlC_fit = .true.
  else
!
!  Second call is to exit full stop
!
    call outerror('CTRL-C: Second call to control-C and so stopping now',0_i4)
    call stopnow('trap_sigint_fit')
  endif
  return

end subroutine trap_sigint_fit

subroutine trap_sigint_opt
!
!  Trap control C during execution of optimisation
!
  use datatypes
  use interupt,   only : controlC_opt
  use iochannels, only : ioout
  use parallel,   only : ioproc
  implicit none
!
  if (.not.controlC_opt) then
!
!  First call is to exit iterative process
!
    if (ioproc) then 
      write(ioout,'(''CTRL-C: Will exit current iterative process at end of the cycle'')')
    endif
    controlC_opt = .true.
  else
!
!  Second call is to exit full stop
!
    call outerror('CTRL-C: Second call to control-C and so stopping now',0_i4)
    call stopnow('trap_sigint_opt')
  endif
  return

end subroutine trap_sigint_opt
