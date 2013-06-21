!************************
!  I/O Module for GULP  *
!************************
!
!  I/O Channels specified here so that they can be changed for the benefit
!  of external programs calling GULP.
!
!   3/02 Created
!  12/03 lsilent flag added - if true then output is suppressed
!   9/09 iotmp added
!   2/11 Integer types changed to i4
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
!  Julian Gale, NRI, Curtin University, February 2011
!

!
!  I/O channels
!
  module iochannels
    use datatypes
    integer(i4),                    save :: ioin  = 5_i4  ! Input channel
    integer(i4),                    save :: ioout = 6_i4  ! Output channel
    integer(i4),                    save :: iotmp = 4_i4  ! Temporary input buffer channel
    logical,                        save :: lsilent = .false.
  end module iochannels
