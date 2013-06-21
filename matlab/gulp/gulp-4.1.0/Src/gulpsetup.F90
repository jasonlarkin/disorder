  subroutine gulp_setup
!
!  Performs setup tasks for GULP
!
!   9/06 Created from gulp.F
!   3/07 initcomms renamed to GULP_initcomms
!   8/07 Call to GULP_initcomms removed to avoid MPI error
!   1/08 Declaration of ierror wrapped with ifdef
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   6/09 Call to banner changed to gulp_banner
!   6/09 Renamed to gulp_setup for consistency with other names
!   6/09 Output of extra species info added for PDF/CML
!   9/09 Number for buffer channel now accessed from module
!   7/11 Version incremented to 4.0
!   8/11 Output of hostname added to setup info
!  12/12 Duplicate use of iochannels removed
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
!  Julian Gale, NRI, Curtin University, December 2012
!
  use configurations
  use control
  use current
  use general
  use gulp_cml,        only : lcml, gulp_cml_init, gulp_cml_outkey
  use gulp_cml_phonon, only : gulp_cml_outspec
  use iochannels,      only : iotmp, ioout
  use m_pdfneutron,    only : lpdfout, outpdfin
  use parallel
  use gulpchemsh
#ifdef ACCELRYS
  use license
#endif
  implicit none
!
  character(len=40) :: hostname
  integer(i4)       :: iline
#ifdef ACCELRYS
  integer(i4)       :: ierror
#endif
  integer(i4)       :: hlength
  integer(i4)       :: status
  logical           :: lopened
  ! ChemShell additions
  logical           :: opc_root
  external opc_root
#ifdef MPI
  include 'mpif.h'
#endif

!*************************
!  Nullify all pointers  *
!*************************
  call nullpointer
!*************************
!  Initialise memory     *
!*************************
  call initmemory
!*************************
!  Initialise variables  *
!*************************
  iline = 0
  call initial
!******************
!  Output header  *
!******************
  version = 4.0_dp
#ifdef ACCELRYS
  call license_checkout(nprocs,ierror)
  if (ierror.ne.0) call gulpfinish
! Set traps for signals
  
  call setup_traps()
#endif
  call gulp_banner
!**************************
!  Get local information  *
!**************************
  call local
!****************************
!  Set element information  *
!****************************
  call setele(lopened)
!*******************************************************
!  Pre-processor passes before reading input properly  *
!*******************************************************
  call channels(iotmp,.true.)
  call firstpass
  rewind(iotmp)
  call secondpass
  rewind(iotmp)
!*********************
!  Read in keywords  *
!*********************
  call getkeyword(iline)
  call setkeyword
!***********************
!  Process main input  *
!***********************
  if (ldefect) then
    call channels(41_i4,.false.)
    call channels(42_i4,.false.)
    call channels(48_i4,.false.)
  endif
  call inword(iline)
!*******************************************************
!  Set keywords again in case any were in the library  *
!*******************************************************
  call setkeyword
  close(iotmp,status='delete')
!***************************
!  Output keyword details  *
!***************************
  call outkey
!**************
!  Site name  *
!**************
  if (ioproc) then
    if (site.ne.' ') then
      write(ioout,'(''* '',a76,'' *'')') site(1:76)
      write(ioout,'(''********************************************************************************'')')
    endif
    call datetime(1_i4)
    write(ioout,'(''  Number of CPUs = '',i5,/)') nprocs
    hostname = ' '
    call get_environment_variable('HOSTNAME',hostname,hlength,status)
    if (status.eq.0) then
      write(ioout,'(''  Host name      = '',a40,/)') hostname
    elseif (status.eq.1) then
      call get_environment_variable('HOST',hostname,hlength,status)
      if (status.eq.0) then
        write(ioout,'(''  Host name      = '',a40,/)') hostname
      endif
    endif
  endif
!***********************
!  CML initialisation  *
!***********************
  if (lcml) then
    call gulp_cml_init
    call gulp_cml_outkey
  endif
!**************************
!  One off initial set up *
!**************************
  call setcfg
!*******************
!  Species output  *
!*******************
  if (ioproc) then
    call outspec
    if (lcml) call gulp_cml_outspec
    if (lpdfout) call outspec_pdf
  endif
!***********************************************************************
!  Check PDF related settings, convert frequencies and output PDF info *
!***********************************************************************
  if (lpdfout) call outpdfin
!************************
!  Output general info  *
!************************
  if (ioproc) call outgen
!*******************************
!  Output polarisability info  *
!*******************************
  if (ioproc) call outpolar
!*********************
!  Check potentials  *
!*********************
  call checkpot
!**********************
!  Output potentials  *
!**********************
  call outpot
!*****************************
!  Check options for method  *
!*****************************
  call methodok
!*****************************
!  ChemShell charge-only run *
!*****************************
  if (ichemsh_qm .eq. 99 .and. opc_root()) then
    call ExportGulpCharges(numat, qlcfg)
  endif
!
  return
  end
