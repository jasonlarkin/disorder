  subroutine initdervs(lgrad1,lgrad2)
!
!  Subroutine for initialising the derivatives
!  Called from energy.f and fefunct.f
!
!   8/97 Created from part of energy.f
!  10/97 Zeroing of derv2 modified to allow for frozen atoms
!  11/97 Bug in zeroing of derv2 for frozen atoms fixed
!   5/02 Correction to array dimensions made for BSM case
!   8/02 External force added
!  11/02 On diagonal derv2 array added
!   5/03 Region derivatives initialised
!   8/04 External force moved to force.f
!   4/06 Inconsistency in the setting of maxd1 fixed
!   3/07 Chemshell modifications added
!   1/09 Modified to allow for different levels of integer precision
!        between GULP and MPI
!   3/09 MPI communicator changed to MPI_Comm_GULP 
!   3/09 Arrays for non-radial terms added
!   6/09 Virials added
!   6/09 Derivatives divided by inverse of number of processors in
!        the case of a chemshell call
!   6/09 Array passed to MPI call changed to shell_force_mpi
!  11/09 Initialisation of region derivatives added
!   4/12 xvir, yvir and zvir removed
!   5/12 Atomic stress array added
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations, only : maxregion
  use control,        only : latomicstress
  use current
  use derivatives
  use eam,            only : lMEAM
  use gulpchemsh
  use iochannels
  use optimisation
#ifdef MPI
  use parallel,       only : ioproc, nprocs, MPI_comm_GULP
#endif
  use symmetry
  implicit none
#ifdef MPI
  include 'mpif.h'
#endif
!
!  Passed variables
!
  logical, intent(in) :: lgrad1
  logical, intent(in) :: lgrad2
!
!  Local variables
!
  integer(i4)         :: i
  integer(i4)         :: j
  integer(i4)         :: maxlim
  integer(i4)         :: nff
!
!  ChemShell variables
!
  integer(i4)         :: ishell
  integer(i4)         :: iret
  integer(i4)         :: nsh
  integer(i4)         :: nodeid
  real(dp)            :: pfac
  external nodeid
#ifdef MPI
  integer*4             :: ier
  integer*4             :: nsh_mpi
  real*8,   allocatable :: shell_force_mpi(:,:)
#endif
!
!  Zero derivative arrays
!
!  In the case of the Cartesian derivatives, we set them equal to 
!  the externally applied force, which if not specified is zero.
!
  if (lgrad1) then
    if (maxat.gt.maxd1) then
      maxd1 = maxat
      call changemaxd1
    endif
!********************
!  CHEMSHELL_START  *
!********************
!
!  For shell optimisation in electric field of QM region
!
    if (ichemsh_qm == 1) then
      
      if (nodeid().eq.0) then
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

      pfac = nprocs
      pfac = 1.0_dp / pfac
#else
      pfac = 1.0_dp
#endif

      ishell = 0
      do i = 1,numat
        if (iatn(i).le.106) then
          xdrv(i) = 0.0_dp
          ydrv(i) = 0.0_dp
          zdrv(i) = 0.0_dp
        else
          ishell = ishell + 1
          xdrv(i) = shell_force(1,ishell)*51.422606_dp*pfac
          ydrv(i) = shell_force(2,ishell)*51.422606_dp*pfac
          zdrv(i) = shell_force(3,ishell)*51.422606_dp*pfac
        endif
      enddo
      if (ishell .ne. nsh) then
        write(6,*)'Shell Count error',nsh,ishell,nodeid()
        stop 1
      endif
!******************
!  CHEMSHELL_END  *
!******************
    else
      do i = 1,numat
        xdrv(i) = 0.0_dp
        ydrv(i) = 0.0_dp
        zdrv(i) = 0.0_dp
      enddo
    endif
    if (lMEAM) then
      do i = 1,numat
        xdrvnr(i) = 0.0_dp
        ydrvnr(i) = 0.0_dp
        zdrvnr(i) = 0.0_dp
      enddo
    endif
!
!  Region derivatives
!
    xregdrv(1:maxregion) = 0.0_dp
    yregdrv(1:maxregion) = 0.0_dp
    zregdrv(1:maxregion) = 0.0_dp
!
!  Initialise breathing shell forces if needed
!
    if (nbsmat.gt.0) then
      raderv(1:numat) = 0.0_dp
    endif
  endif
!
!  Find total number of variable atoms to set maximum
!  size of derv2 that will be used in the calculation
!
  if (lfreeze) then
    nff = 0
    do i = 1,numat
      if (lopf(nrelat(i))) then
        nff = nff + 3
        if (nbsmat.gt.0) nff = nff + 1
      endif
    enddo
  else
    if (nbsmat.gt.0) then
      nff = 4*numat
    else
      nff = 3*numat
    endif
  endif
!
!  Add 3 to nff to allow for region 2
!
  nff = nff + 3
!
  if (lgrad2) then
    if (lsymderv2) then
      maxlim = 3*nasym
      if (nbsmat.gt.0) maxlim = maxlim + nasym
      if (maxlim.gt.maxd2u) then
        maxd2u = maxlim
        call changemaxd2
      endif
      if (nff.gt.maxd2) then
        maxd2 = nff
        call changemaxd2
      endif
      do i = 1,maxlim
        do j = 1,nff
          derv2(j,i) = 0.0_dp
        enddo
        derv2d(i) = 0.0_dp
      enddo
    else
      maxlim = nff
      if (maxlim.gt.maxd2u) then
        maxd2u = maxlim
        call changemaxd2
      endif
      if (maxlim.gt.maxd2) then
        maxd2 = maxlim
        call changemaxd2
      endif
      do i = 1,maxlim
        do j = 1,maxlim
          derv2(j,i) = 0.0_dp
        enddo
        derv2d(i) = 0.0_dp
      enddo
    endif
    if (lstr) then
      do i = 1,nstrains
        do j = 1,6
          sderv2(j,i) = 0.0_dp
        enddo
        do j = 1,maxlim
          derv3(j,i) = 0.0_dp
        enddo
      enddo
    endif
  endif
  if (lstr) then
    if (latomicstress) then
      do i = 1,numat
        atomicstress(1:nstrains,i) = 0.0_dp
      enddo
    endif
    do i = 1,nstrains
      strderv(i) = 0.0_dp
      rstrd(i) = 0.0_dp
    enddo
    if (lMEAM) then
      do i = 1,nstrains
        rstrdnr(i) = 0.0_dp
      enddo
    endif
  endif
!
  return
  end
