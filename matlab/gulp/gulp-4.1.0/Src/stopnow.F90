  subroutine stopnow(routine)
!
!  Stops program after flushing and closing output files
!
  use iochannels
  use parallel
  implicit none
!
  character(len=*) :: routine
#ifdef MPI
  include 'mpif.h'
  integer ierr
#endif

  if (ioproc) then
    write (ioout,'(/,a,i5,a,a15,/)') ' Program terminated by processor ',procid,' in ',routine
    call gflush(ioout)
#ifdef ACCELRYS
     call create_killfile()
#endif
    close(4,status='delete')
  endif
#ifdef MPI
!******************************
!  Close down all processors  *
!******************************
  call MPI_finalize(ierr)
  stop
#else
  stop 'GULP terminated with an error'
#endif
  end

#ifdef ACCELRYS
  subroutine create_killfile ()
!=========================================================================C
! This creates an empty file called killfile in the current directory     C
!-------------------------------------------------------------------------C
! Arguments:                  none                                        C
!-------------------------------------------------------------------------C
!-------------------------------------------------------------------------C
! Written by Victor Milman, version 0.01, 01/05/02                        C
!=========================================================================C

  implicit none

  integer :: ios, lunit

  lunit = 60
  open(unit=lunit,form='FORMATTED',status='UNKNOWN', &
       access='SEQUENTIAL',file="killfile",iostat=ios)

  write (lunit,*) "Job terminated"
  close (lunit)

  return
  end subroutine create_killfile
#endif
