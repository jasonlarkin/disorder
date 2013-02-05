  subroutine outpolar
!
!  Output polarisability species information
!
!   5/00 created from outspec
!   4/04 Quadupolar polarisability added
!  11/06 Format statement cleaned up
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006
!
  use iochannels
  use polarise
  implicit none
!
!  Local variables
!
  character(len=5)            :: lab
  integer(i4)                 :: i
  integer(i4)                 :: na
  integer(i4)                 :: ntyp
!
!  If no data then skip everything else
!
  if (.not.lpolar) return
!**********************************
!  Polarisability species output  *
!**********************************
  write(ioout,'(/,''  Polarisability species : '',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Species    Type    Dipolar polarisability     Quadrupolar polarisability      '')')
  write(ioout,'(''                           (Angs**3)                     (Angs**5)              '')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  do i = 1,npolspec
    na = natpolspec(i)
    ntyp = ntyppolspec(i)
    call label(na,ntyp,lab)
    write(ioout,'(4x,a5,4x,''core  '',1x,f18.6,13x,f18.6)') lab,dpolspec(i),qpolspec(i)
  enddo
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(/)')
!
  return
  end
