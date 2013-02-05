  subroutine outgen
!
!  Subroutine for outputing general parameter information
!
!  11/97 Created from part of gulp.F
!   5/03 Setting of lperiodic now tests for dimension to be
!        greater than or equal to 1 - not just 3.
!  10/04 Target Ewald radius added to output
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   8/09 Output of lattice sum name modified to include options
!        other than Ewald in name.
!  11/12 Modified Wolf sum of Fennell and Gezelter added
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
!  Julian Gale, NRI, Curtin University, November 2012
!
  use configurations
  use control
  use general
  use iochannels
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  logical     :: lperiodic
!********************
!  Accuracy factor  *
!********************
!
!  Are any of the structures periodic?
!
  lperiodic = .false.
  i = 0
  do while (i.lt.ncfg.and..not.lperiodic)
    i = i + 1
    lperiodic = (ndimen(i) .ge. 1)
  enddo
  if (lperiodic) then
    if (lwolf) then
      write(ioout,'(''  Lattice summation method = Wolf et al'',/)')
      if (lwolforiginal) then
        write(ioout,'(''  Original method - smooth cut-off / non-consistent forces'',/)')
      elseif (lwolffennell) then
        write(ioout,'(''  Modified method - smooth cut-off / consistent forces (Fennell-Gezelter)'',/)')
      else
        write(ioout,'(''  Modified method - smoothed energy only / correct forces'',/)')
      endif
      write(ioout,'(''  Eta    = '',f12.6,'' Angstrom**-1'')') etaw
      write(ioout,'(''  Cutoff = '',f12.6,'' Angstrom'',/)') cutw
    else
      write(ioout,'(''  Lattice summation method               =    Ewald          (3-D)'')')
      write(ioout,'(''                                         =    Parry          (2-D)'')')
      write(ioout,'(''                                         =    Saunders et al (1-D)'')')
      write(ioout,'(''  Accuracy factor for lattice sums       = '',f8.3)') accuracy
      if (targetrradmax.gt.0.0_dp) then
        write(ioout,'(''  Target real space radius for Ewald sum = '',f8.3)') targetrradmax
      endif
      write(ioout,'(/)')
    endif
  elseif (index(keyword,'nore').eq.0) then
    write(ioout,'(''  Accuracy factor for short range sums = '',f6.3,/)') accuracy
  endif
!***********************
!  Maximum time limit  *
!***********************
  if (timmax.le.0.0_dp) then
    write(ioout,'(''  Time limit = Infinity'')')
  else
    write(ioout,'(''  Time limit = '',f10.2,'' seconds'')')timmax
  endif
  return
  end
