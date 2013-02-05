  subroutine gulp_banner
!
!  Outputs banner for GULP
!
!   4/01 Created from GULP main routine for tidiness
!  10/02 Version incremented to 1.4.1 to signify SE changes
!  10/02 Date of modifcation added to banner
!   9/06 Number incremented due to major torsion modification
!   3/07 Number incremented due to switch to f90 format
!   6/09 Renamed to gulp_banner for benefit of Chemshell
!   8/11 Updated to MS studio 6.0 and GULP 4.0.
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
!  Julian Gale, NRI, Curtin University, October 2012
!
  use iochannels
  use parallel
  implicit none
!
!******************
!  Output header  *
!******************
  if (ioproc) then
#ifdef ACCELRYS
    write(ioout,'(''********************************************************************************'')')
    write(ioout,'(''*                       GENERAL UTILITY LATTICE PROGRAM  v4.0                  *'')')
    write(ioout,'(''*                                 Julian Gale                                  *'')')
    write(ioout,'(''*                      Nanochemistry Research Institute                        *'')')
    write(ioout,'(''*                           Department of Chemistry                            *'')')
    write(ioout,'(''*                    Curtin University, Western Australia                      *'')')
    write(ioout,'(''********************************************************************************'')')
    write(ioout,'(''*                    Accelrys Materials Studio 6.0 Release                     *'')')
    write(ioout,'(''********************************************************************************'')')
#else
    write(ioout,'(''********************************************************************************'')')
    write(ioout,'(''*                       GENERAL UTILITY LATTICE PROGRAM                        *'')')
    write(ioout,'(''*                                 Julian Gale                                  *'')')
    write(ioout,'(''*                      Nanochemistry Research Institute                        *'')')
    write(ioout,'(''*                           Department of Chemistry                            *'')')
    write(ioout,'(''*                    Curtin University, Western Australia                      *'')')
    write(ioout,'(''********************************************************************************'')')
    write(ioout,'(''* Version = 4.0.7 * Last modified =  14th December 2012                        *'')')
    write(ioout,'(''********************************************************************************'')')
#endif
  endif
!
  return
  end
