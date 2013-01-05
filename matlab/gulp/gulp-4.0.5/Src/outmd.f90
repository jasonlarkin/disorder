  subroutine outmd
!
!  Output from MD run - close files
!
!  Channels :
!
!    31 = trajectory file
!    32 = archive file
!    33 = xyz file
!    34 = history file
!    35 = pressure file
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
!  Copyright Curtin University 2005
!
  use files
  implicit none
!
  if (ltrj) then
    close(31)
  endif
  if (lhis) then
    close(34)
  endif
  if (lpre) then
    close(35)
  endif
!
  return
  end
