  program gulp
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
!    (11) COSMO/COSMIC solvation calculations
!    (12) PDF calculations
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
!   3/07 Main routine now called gulpmain as a subroutine called
!        from here. 
!   5/07 Option to use seed argument added (VM)
!   7/07 Modified to remove argument checking (VM)
!  10/08 COSMIC model merged into this branch
!   3/09 Communicator passed to gulpmain to aid task farming
!   6/09 PDF calculation by Elizabeth Cope merged in
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use datatypes,  only : i4
  implicit none

  integer(i4)        :: iret
  integer(i4)        :: ichemsh_qm
  integer*4          :: MPI_comm_dummy
  
  iret = 0
  ichemsh_qm = -1
!
!  This is a dummy communicator here - for task farming gulpmain can be called
!  with different values of MPI_comm_dummy and in this case the ichemsh_qm
!  flag should be used to ensure that MPI_init is not called again later. 
!
  MPI_comm_dummy = 1

  call gulpmain(iret, ichemsh_qm, MPI_comm_dummy)

!********************
!  Finish GULP run  *
!********************
  call gulpfinish

  end
