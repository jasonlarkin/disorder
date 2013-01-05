  subroutine mpbarrier
!
!  Creates an MPI barrier point for synchronisation of nodes
!
#ifdef MPI
  use parallel, only : MPI_comm_GULP
#endif
  implicit none
!
#ifdef MPI
  include 'mpif.h'
  integer ierr

  call MPI_barrier(MPI_comm_GULP,ierr)
#endif
  end
