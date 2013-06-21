  subroutine sumall(partial,sum,count,caller,desc)
!
!  Uses MPI_allreduce to perform a global sum for "count" real*8 items
!

!
!  Modules
!
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)      :: count
  real(dp)         :: sum(count)
  real(dp)         :: partial(count)
  character(len=*) :: caller, desc
!
!  Other variables
!
#ifdef MPI
  include 'mpif.h'
  integer*4           :: ierror,icode,lenstring,ic,i
  real*8, allocatable :: partial_mpi(:)
  real*8, allocatable :: sum_mpi(:)
  character error_string(MPI_max_error_string)
!
  if (nprocs.gt.1) then
!
!  Copy values to MPI compatible variables
!
    allocate(partial_mpi(count))
    allocate(sum_mpi(count))
    do i = 1,count
      partial_mpi(i) = partial(i)
    enddo
!
    call MPI_allreduce(partial_mpi,sum_mpi,count,MPI_double_precision,MPI_sum,MPI_comm_GULP,ierror)
!
!  Copy values back from MPI compatible variables
!
    do i = 1,count
      sum(i) = sum_mpi(i)
    enddo
    deallocate(sum_mpi)
    deallocate(partial_mpi)

!  Check for errors
    if (ierror.ne.MPI_success) then
      write(*,"(5a)")  &
       "Call to MPI_allreduce failed in subroutine sumall,", &
       " called from subroutine ", caller, " for ", desc
      icode = ierror
      call MPI_error_string(icode,error_string,lenstring,ierror)
      write(*,"(a)") error_string 
      call MPI_abort(MPI_comm_GULP,1,ierror)
    endif
  elseif(nprocs.eq.1) then
    do ic = 1,count
      sum(ic) = partial(ic)
    enddo
  else 
    write(*,"(/a)") error_string
    call MPI_abort(MPI_comm_GULP,1,ierror)
  endif
#else
  integer ic
  do ic = 1,count
    sum(ic) = partial(ic)
  enddo
#endif
  return
  end

  subroutine isumall(ipartial,isum,count,caller,desc)
!
!  Uses MPI_allreduce to perform a global sum for "count" integer*4 items
!

!
!  Modules
!
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)      :: count
  integer(i4)      :: isum(count)
  integer(i4)      :: ipartial(count)
  character(len=*) :: caller, desc
!
!  Other variables
!
#ifdef MPI
  include 'mpif.h'
  integer*4              :: ierror,icode,lenstring,ic,i
  integer*4, allocatable :: ipartial_mpi(:)
  integer*4, allocatable :: isum_mpi(:)
  character error_string(MPI_max_error_string)
!
  if (nprocs.gt.1) then
!
!  Copy values to MPI compatible variables
!
    allocate(ipartial_mpi(count))
    allocate(isum_mpi(count))
    do i = 1,count
      ipartial_mpi(i) = ipartial(i)
    enddo
!
    call MPI_allreduce(ipartial_mpi,isum_mpi,count,MPI_integer,MPI_sum,MPI_comm_GULP,ierror)
!
!  Copy values back from MPI compatible variables
!
    do i = 1,count
      isum(i) = isum_mpi(i)
    enddo
    deallocate(isum_mpi)
    deallocate(ipartial_mpi)

!  Check for errors
    if (ierror.ne.MPI_success) then
      write(*,"(5a)")  &
       "Call to MPI_allreduce failed in subroutine isumall,", &
       " called from subroutine ", caller, " for ", desc
      icode = ierror
      call MPI_error_string(icode,error_string,lenstring,ierror)
      write(*,"(a)") error_string 
      call MPI_abort(MPI_comm_GULP,1,ierror)
    endif
  elseif(nprocs.eq.1) then
    do ic = 1,count
      isum(ic) = ipartial(ic)
    enddo
  else 
    write(*,"(/a)") error_string
    call MPI_abort(MPI_comm_GULP,1,ierror)
  endif
#else
  integer ic
  do ic = 1,count
    isum(ic) = ipartial(ic)
  enddo
#endif
  return
  end

  subroutine landall(lpartial,land,count,caller,desc)
!
!  Uses MPI_allreduce to perform a global logical "and" for "count" logical items
!

!
!  Modules
!
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)      :: count
  logical          :: land(count)
  logical          :: lpartial(count)
  character(len=*) :: caller, desc
!
!  Other variables
!
#ifdef MPI
  include 'mpif.h'
  integer*4              :: ierror,icode,lenstring,ic,i
  character error_string(MPI_max_error_string)
!
  if (nprocs.gt.1) then
!
    call MPI_allreduce(lpartial,land,count,MPI_logical,MPI_land,MPI_comm_GULP,ierror)

!  Check for errors
    if (ierror.ne.MPI_success) then
      write(*,"(5a)")  &
       "Call to MPI_allreduce failed in subroutine landall,", &
       " called from subroutine ", caller, " for ", desc
      icode = ierror
      call MPI_error_string(icode,error_string,lenstring,ierror)
      write(*,"(a)") error_string 
      call MPI_abort(MPI_comm_GULP,1,ierror)
    endif
  elseif(nprocs.eq.1) then
    do ic = 1,count
      land(ic) = lpartial(ic)
    enddo
  else 
    write(*,"(/a)") error_string
    call MPI_abort(MPI_comm_GULP,1,ierror)
  endif
#else
  integer ic
  do ic = 1,count
    land(ic) = lpartial(ic)
  enddo
#endif
  return
  end
