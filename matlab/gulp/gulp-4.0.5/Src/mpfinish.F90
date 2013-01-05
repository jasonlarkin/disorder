  subroutine mpfinish
!
!  Closes down MPI and stops program
!

#ifdef MPI
  integer ierr

  call MPI_Finalize(ierr)
#endif
  stop
  end
