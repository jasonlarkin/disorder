  subroutine GULP_initcomms(MPI_comm_in)
!
!  Initialises MPI if necessary, finds own taskid and number of tasks
!
!  Modified to handle possible precision issues - pass local scalars
!  to get procid/nprocs for benefit of Cray
!
!  12/03 Silent option added in which ioproc is set to false
!   3/07 Chemshell modifications added and renamed 
!   3/09 MPI communicator changed to MPI_comm_GULP based on passed argument
!   6/09 MPI barrier changed to MPI_comm_world instead of MPI_comm_GULP
!
!  Julian Gale, NRI, Curtin University, June 2009
!

!
!  Modules
!
  use iochannels
  use gulpchemsh
  use parallel

  implicit none
!
!  Passed variables
!
  integer*4,   intent(in)   :: MPI_comm_in
!
#ifdef MPI
  include 'mpif.h'
  integer ierr,lprocid,lnprocs

#ifdef ACCELRYS
#include "LS_LicenseHPMPI.h"
  INTEGER :: hpmpi_key, hpmpi_ierr
  hpmpi_key = LS_LICENSE_HPMPI_KEY            ! HP-MPI Licensing key
  call MPI_Initialized(hpmpi_key, hpmpi_ierr) ! HP-MPI Licensing
#endif


  if (ichemsh_qm .lt. 0) then
!
!  Non-ChemShell case - initialise MPI
!
    call MPI_init(ierr)
!
!  Set communicator for MPI based on MPI_comm_world
!
    MPI_comm_GULP = MPI_comm_world
  else
!
!  ChemShell case - MPI is assumed to be already running and communicator set using argument
!
    MPI_comm_GULP = MPI_comm_in
  endif

  call MPI_comm_rank(MPI_comm_GULP,lprocid,ierr)
  call MPI_comm_size(MPI_comm_GULP,lnprocs,ierr)

  procid  = lprocid
  nprocs  = lnprocs
#else
  procid  = 0
  nprocs  = 1
#endif
  if (lsilent) then
    ioproc = .false.
  else
    ioproc = (procid.eq.0)
  endif
#ifdef MPI
  call MPI_barrier(MPI_comm_GULP,ierr)
#endif

  return
  end
