! -*- mode: F90 ; mode: font-lock ; column-number-mode: true ; vc-back-end: RCS -*-
!=============================================================================!
!                          L I C E N S E                                          !
!=============================================================================!
!                                                                             !
! $Id: license.F90,v 1.12 2003/09/03 15:57:59 mds21 Exp $
!                                                                             !
!-----------------------------------------------------------------------------!
! Module for checking licenses: based on CASTEP module                        !
!                                                                             !
!-----------------------------------------------------------------------------!
! Written by Victor Milman, v. 1.0, 16 Apr 2004                               !
!-----------------------------------------------------------------------------!
!                                                                             !
!=============================================================================!

module license
  use parallel
  use iochannels
  implicit none      
#ifdef ACCELRYS
#define LIC_BAD_EXIT_STATUS 131
#define LICENSE_F_INTERFACE
#include "LS_LicensePolicy.h"
#endif


  private       
  !---------------------------------------------------------------------------!
  !                       P u b l i c   R o u t i n e s                       !
  !---------------------------------------------------------------------------!
  public :: license_checkout                 
  public :: license_checkin
  public :: sendHeartbeat
  public :: setup_traps
  interface
     function signal_handler(sig)
        integer :: signal_handler
        integer, intent(in) :: sig
     end function signal_handler
  end interface

  !---------------------------------------------------------------------------!
  !                      P r i v a t e   V a r i a b l e s                    !
  !---------------------------------------------------------------------------!
  integer, save :: lic_handle
  integer, save :: VA_ID
!  integer, parameter :: stdout=6

contains
  subroutine license_checkout(num_licenses,status)
    !=========================================================================!
    ! Checks out required number of licenses                                  !
    !-------------------------------------------------------------------------!
    ! Arguments:                                                              !
    !  num_licenses, in, required number of licenses                          !
    ! status, out                                                             !
    !-------------------------------------------------------------------------!
    ! Parent module variables used:                                           !
    !  lic_handle: stored to be used at the end for checking in licenses      !
    !-------------------------------------------------------------------------!
    ! Modules used:                                                           !
    !  io                                                                     !
    !  comms                                                                  !
    !-------------------------------------------------------------------------!
    ! Necessary conditions:                                                   !
    ! positive number of licenses                                             !
    !-------------------------------------------------------------------------!
    ! Written by Victor Milman, v1.0, 25.01.2002                              !
    !=========================================================================!
    implicit none

    integer, intent(in)  :: num_licenses
    integer, intent(out) :: status

    integer :: mystatus

#ifdef debug
#ifdef ACCELRYS
    if (num_licenses<=0 .and. ioproc) then 
       write(ioout,*) 'Error in license_checkout: should request positive number of licenses'
       call stopnow('license_checkout')
    endif
#endif 
#endif     


    status = 0
!#ifdef ACCELRYS
    ! create a list of PIDs - needed to kill from Perl script later
    ! call reportpid()
!#endif

    ! True if we are on the root node
    if (.not.ioproc) return

    ! Only check licences and do io when on root node

    ! Write banner at start of output
    write(ioout,*)                                                                             &
         &     '*******************************************************************************'
    write(ioout,*)                                                                             &
         &     '*                       GENERAL UTILITY LATTICE PROGRAM                       *'
    write(ioout,*)                                                                             &
         &     '*                     Julian Gale, NRI, Curtin University                     *'
    write(ioout,*)                                                                             &
         &     '*                                Version 3.1                                  *'
    write(ioout,*)                                                                             &
         &     '*                                                                             *'
    write(ioout,*)                                                                             &
         &     '*                      Copyright (c) 2000-2007                                *'
    write(ioout,*)                                                                             &
         &     '*                                                                             *'
    write(ioout,*)                                                                             &
         &     '*         Please cite                                                         *'
    write(ioout,*)                                                                             &
         &     '*                                                                             *'
    write(ioout,*)                                                                             &
         &     '*    "The General Utility Lattice Program (GULP)"                             *'
    write(ioout,*)                                                                             &
         &     '*     Molecular Simulation, 29(5) pp.291-341 (2003)                           *'
    write(ioout,*)                                                                             &
         &     '*     J. D. Gale, A. L. Rohl                                                  *'
    write(ioout,*)                                                                             &
         &     '*                                                                             *'
    write(ioout,*)                                                                             &
         &     '*         in all publications arising from your use of GULP                   *'
    write(ioout,*)                                                                             &
         &     '*******************************************************************************'
    write(ioout,*) 

#ifdef ACCELRYS

#ifdef __DATE__
#ifndef PLATFORM
#define PLATFORM "MS Windows"
#endif
#ifdef debug
#define DEBUG " DEBUG "
#else
#define DEBUG " "
#endif
    write(ioout,*) 
    write(ioout,*) "This",DEBUG, "version was compiled for ",PLATFORM, &
             & " on ", __DATE__
    write(ioout,*) 
#endif

#ifndef PAPERLICENSE
    if (num_licenses<=0 .and. ioproc) then 
       write(ioout,*) 'Error in license_checkout: should request positive number of licenses'
       status = -1
    endif

    ! switch from automatic to proper manual heartbeats
    VA_ID = GET_VA_ID_F()
    mystatus = SET_VA_F(VA_ID,"System.Heartbeats", "LS_LICENSE_MANUAL")
    if (mystatus /= 1) then
       status = -1
       write(ioout,'(a)') 'Licensing Error !'
       write(ioout,'(a)')                                         &
            &       'Error: Manual heartbeat set failed'
       mystatus = LIC_BAD_EXIT_STATUS 
       go to 999
    end if

    lic_handle = VALID_LICENSE_MP_VA_F(K_gulp,num_licenses,VA_ID)
    if ( lic_handle == 0 ) then
       write(ioout,'(a)') 'Licensing Error !'
       write(ioout,'(a)')                                         &
            &       'Error: Failed to checkout license'
       mystatus = LIC_BAD_EXIT_STATUS 
       status = -1
       go to 999
    else
       write(ioout,'(a)')                                         &
            &       'License checkout successful'
       write(ioout,*)
    end if

#endif
#endif
 999 continue
    return

  end subroutine license_checkout



  subroutine license_checkin(status)
    !=========================================================================!
    ! Checks in licenses                                                      !
    !-------------------------------------------------------------------------!
    ! Arguments:                                                              !
    ! status, out                                                             !
    !-------------------------------------------------------------------------!
    ! Written by Victor Milman, v1.0, 25.01.2002                              !
    !=========================================================================!

    implicit none


    integer, intent(out) :: status

    integer :: mystatus

    status = 0

    ! True if we are on the root node
    if (.not.ioproc) return

#ifdef ACCELRYS
#ifndef PAPERLICENSE
    mystatus = RELEASE_VALID_LICENSE_F(lic_handle)
    if (mystatus /= 0) status = LIC_BAD_EXIT_STATUS
    ! moved the attribute release to the checkin stage
    mystatus = RELEASE_VA_ID_F(VA_ID)
    if (mystatus /= 1) status = LIC_BAD_EXIT_STATUS
#endif
#endif

    return

  end subroutine license_checkin

  ! routine for sending manual heartbeats
  subroutine sendHeartbeat(status)
    
    implicit none
    integer,intent(out) :: status
    integer :: idum

    status = 0
    ! True if we are on the root node
    if (.not.ioproc) return
#ifdef ACCELRYS
#ifndef PAPERLICENSE
    idum = SEND_HEARTBEAT_F( lic_handle )
    if (idum /= 0) then
        write(ioout,'(a)') 'Licensing Error !'
        write(ioout,'(a)') 'Send heartbeat for MS_nanotech failed'
        status = LIC_BAD_EXIT_STATUS 
    endif
#endif
#endif

    
    return
    end subroutine sendHeartbeat


  subroutine setup_traps()
    !=========================================================================!
    ! Sets up traps for signals (SIGTERM, etc.)                               !
    ! Needed to exit gracefully when killed                                   !
    ! Required to cope with SGI: manual kill of a process also kills the      !
    ! parent, and so takes down Materials Studio Gateway                      !
    !-------------------------------------------------------------------------!
    ! System function SIGNAL is used; On WIN32 (CVF compiler) it comes from   !
    ! DFPORT library, on UNIX it is declared as external.                     !
    !-------------------------------------------------------------------------!
    ! Arguments: none                                                         !
    !-------------------------------------------------------------------------!
    ! Written by Victor Milman, v1.0, 11.09.2003                              !
    !=========================================================================!

#ifdef WIN32
    use DFPORT, only : signal
#endif

    implicit none

#ifndef WIN32
    integer,external :: signal
#endif

    integer :: handler

    ! Ideally we would like access to the values of SIGINT and SIGTERM,
    ! however there does not seem to be OS-independent Fortran access to
    ! to those values.
    handler = signal(2,signal_handler,-1)   ! SIGINT
    handler = signal(15,signal_handler,-1)  ! SIGTERM

    return
  end subroutine setup_traps


end module license

integer function signal_handler(sig)

    !=========================================================================!
    ! Sets up traps for signals (SIGTERM, etc.)                               !
    ! Needed to exit gracefully when killed                                   !
    ! Required to cope with SGI: manual kill of a process also kills the      !
    ! parent, and so takes down Materials Studio Gateway                      !
    !-------------------------------------------------------------------------!
    ! Arguments:                                                              !
    ! sig - dummy argument                                                    !
    !-------------------------------------------------------------------------!
    ! Written by Victor Milman, v1.0, 11.09.2003                              !
    !=========================================================================!
    use license, only : license_checkin
    use parallel
    implicit none

    integer,intent(in) :: sig
    integer :: status

    if (ioproc) then
       write (6,*) 'Trapped SIGINT or SIGTERM. Exiting...'
    endif

    call license_checkin(status)
    call stopnow('signal_handler')

    signal_handler = 1

    return
end function signal_handler
