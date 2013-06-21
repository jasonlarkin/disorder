  subroutine chemshellenergy(echemsh)
!
!  Subroutine for calculating the energy due to Chemshell
!
!   3/07 Created by JDG based on modifications to energy from
!        chemshell version of GULP.
!   1/09 Modified to allow for different levels of integer precision
!        between GULP and MPI
!   3/09 MPI communicator changed to MPI_Comm_GULP 
!   6/09 MPI call modified to correct array name
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use configurations
  use current
  use element,        only : maxele
  use iochannels
  use parallel
  use gulpchemsh
  implicit none
#ifdef MPI
  include 'mpif.h'
#endif
!
!  Passed variables
!
  real(dp), intent(out) :: echemsh
!
!  Local variables
!
  integer(i4)           :: i
  integer(i4)           :: ishell 
  integer(i4)           :: iret
  integer(i4)           :: nsh  
#ifdef MPI
  integer*4             :: ier
  integer*4             :: nsh_mpi
  real*8,   allocatable :: shell_force_mpi(:,:)
#endif

!********************************************************************************
!  Calculate contribution to energy from electric field of QM region on shells  *
!********************************************************************************

  if (ichemsh_qm == 1) then

    if (ioproc)then
      call GetShellForces(shell_force,iret,nsh)
      if (iret.ne.0) then
        write(ioout,'(''Error in GULP run. get_shell_forces_ returned error code.'')')
        stop 1
      endif

    endif

#ifdef MPI
!
!  Copy values to MPI precision variables
!
    nsh_mpi = int(nsh)
    allocate(shell_force_mpi(3,size(shell_force)))
    if (ioproc) then
      do i = 1,nsh
        shell_force_mpi(1:3,i) = dble(shell_force(1:3,i))
      enddo
    endif
!
!  Call MPI
!
    call MPI_bcast(nsh_mpi,1,MPI_integer,0,MPI_comm_GULP,ier)

    call MPI_bcast(shell_force_mpi,3*nsh_mpi,MPI_double_precision,0,MPI_comm_GULP,ier)

!
!  Copy values back from MPI precision variables
!
    if (.not.ioproc) then
      nsh = int(nsh_mpi)
      do i = 1,nsh
        shell_force(1:3,i) = dble(shell_force_mpi(1:3,i))
      enddo
    endif
    deallocate(shell_force_mpi)
#endif

    ishell = 0
    echemsh = 0.0_dp

    if (loprt .and. ioproc) write(ioout,'(''Shell Forces:     nsh='',i6)') nsh

    do i = 1,numat
      if (iatn(i).gt.maxele) then
        ishell = ishell + 1
        echemsh = echemsh + 51.422606_dp* &
                ((xclat(i)-xstore(i))*shell_force(1,ishell) + &
                 (yclat(i)-ystore(i))*shell_force(2,ishell) + &
                 (zclat(i)-zstore(i))*shell_force(3,ishell))

        if (loprt .and. ioproc) write(ioout,'(i4, 2x, 3(g10.4))') ishell, &
                shell_force(1,ishell)*51.422606_dp, &
                shell_force(2,ishell)*51.422606_dp, &
                shell_force(3,ishell)*51.422606_dp

        loprt = .false.
      endif
    enddo
  else
    echemsh = 0.0_dp
  endif
!
  return
  end
