  subroutine gulpmain(iret, ichemsh_qm_loc, MPI_comm_in )
!
!  General Utility Lattice Program including
!
!     (1) Electrostatic potential calculations
!     (2) Electronegativity equalisation method
!     (3) Interatomic potential fitting
!     (4) Structure optimisation
!     (5) Phonon calculations
!     (6) Defect calculations
!     (7) Molecular dynamics
!     (8) Free energy minimisation
!     (9) Calculations on 2-D and 3-D systems
!    (10) Monte Carlo calculations
!    (11) Continuum solvation models
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
!  Copyright Curtin University 2011
!
!  11/96 Dynamic memory for large arrays added to f77 version
!  11/96 Scan moved into main routine for memory handling
!   1/97 Dynamic memory introduced by direct call of malloc
!        or via a C subroutine call
!  12/97 Reduction of memory added when only EEM/QEq uses derv2
!        and passing of derv2 to MD routines added so that 
!        variable charges can be used in MD.
!   4/98 Input is now passed through twice and stored temporarily
!        on channel 4.
!   5/98 Dynamic memory allocation for Linux phonons moved to
!        top level as this is the only way to make this OS work.
!   5/98 Structure of defect calls rearranged as this avoids 
!        problems under Linux - avoid any call to free as this
!        causes Linux to core dump
!   3/99 MPI parallelisation of MD introduced into standard version
!   6/00 Genetic Algorithm and Simulated Annealing Routine added (SMW)
!   7/00 Moved call to predict to within loop over configurations (SMW)
!  11/99 Modifications for neutron scattering added
!  10/00 Program converted to f90
!  12/00 2-D systems added
!   1/01 Monte Carlo calculations added
!   4/01 Method/derivative checking routine added
!   4/01 Option calls moved to separate subroutine
!   6/01 COSMO solvation model added
!   6/03 XML modifications added
!   9/03 Parallel I/O modified
!  11/04 Manu changes - see HISTORY
!   9/06 Setup & finish separated for QM/MM benefit
!   3/07 Modified to fit in with Chemshell
!   5/07 GULPfinish call removed and placed in top level routine
!   5/07 Option to explicitly open inputfile added (VM)
!   6/07 Handling of inputfile removed from this routine
!   7/07 Handling of inputfile added back
!   3/09 Communicator for MPI now passed in as argument
!   6/09 Call to options renamed to gulp_options
!
!  Julian Gale, NRI, Curtin University, July 2011
!
!  With contributions by :
!
!  Ivan Walton,         HPCI, Southampton,     March  1999
!  Scott Woodley,       Royal Institution,     June   2000
!
  use control
  use gulpchemsh
  use iochannels
  use parallel
#ifdef ACCELRYS
  use license
#endif
  implicit none
  integer(i4), intent(out)  :: iret
  integer(i4), intent(in)   :: ichemsh_qm_loc
  integer*4,   intent(in)   :: MPI_comm_in
!
!  Local variables
!
#ifdef ACCELRYS
  integer(i4)               :: ierror
#endif
!
!  ChemShell additions
!
  character(len=132)        :: infile
  character(len=132)        :: outfile
  logical                   :: linquire
  logical                   :: opc_root
  external opc_root

!*****************************
!  ChemShell initialisation  *
!*****************************
  ichemsh_qm = ichemsh_qm_loc
  loprt = .true.
  if (opc_root() .and. ichemsh_qm .ge. 0) then
    ioin = 15
    inquire(unit=ioin,opened=linquire)
    if (linquire) close(ioin)
    ioout = 16
    inquire(unit=ioout,opened=linquire)
    if (linquire) close(ioout)
     
    call GetGulpFileNames(infile, outfile, 132 )
     
    if (outfile .ne. "stdout")then
      open(unit=ioout,file=outfile,form='formatted')
    else
      ioout = 6
    endif
    open(unit=ioin,file=infile,form='formatted')
  endif
  call GULP_initcomms(MPI_comm_in)
  if (ichemsh_qm .lt. 0) then
    call setupinputoutput
  endif
!***************
!  Setup GULP  *
!***************
  call gulp_setup
!*****************
!  Call options  *
!*****************
  if (lfit) then
    call fit
#ifdef ACCELRYS
    call sendHeartbeat(ierror)
    if (ierror /= 0) call gulpfinish
#endif
  endif
      
  if (ichemsh_qm.ne.99) call gulp_options

#ifdef ACCELRYS
!
!  Perform final tasks - cannot close scratch files under Linux as this causes a crash sometime!
!
  if (ioproc) then
    call license_checkin(ierror)
  endif
#endif

!
!  Final Chemshell tasks
!
  iret = 0
  if (ichemsh_qm.ge.0) then
    if (ioout.ne.6) close (ioout)
    close (ioin)
  endif

  end subroutine gulpmain

!    
! 03/Jul/2007 aperlov:
! This routine opens input/output files  if seedname is an argument of the command line
! (output file is open only for the ioproc)
! All this stuff is actually needed for parallel execution under vista where hp-mpi
! is not capable of dealing with redirection properly, but is harmless and can be used for any OS.
!
  subroutine setupinputoutput
  use parallel,   only : ioproc
  use iochannels, only : ioin,ioout
  implicit none
  integer            :: num_args
  character(len=132) :: seedname

  num_args = command_argument_count()
  seedname = ""
  if (num_args > 0) then
    call get_command_argument(1,seedname)
  endif

!  
! Open input file explicitly
! 
    
  if (seedname .ne. "") then
    open(unit=ioin,file=trim(seedname)//".gin",form='formatted')
    if(ioproc)then
       open(unit=ioout,file=trim(seedname)//".gout",form='formatted')
    endif
  endif

  end subroutine setupinputoutput
