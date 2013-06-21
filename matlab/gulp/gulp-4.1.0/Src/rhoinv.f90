  subroutine rhoinv
!
!  Return inerted rho values in array rho
!
!   7/00 rho array now stored in two.inc so rhoinv only
!        need be called when the potentials change
!  11/08 Name of array for 1/bpot for Buckingham potential changed from rho to rhopot
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
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
!  Julian Gale, NRI, Curtin University, January 2009
!
  use two
  implicit none
!
  integer :: i
!
  do i = 1,npote
    if (nptype(i).eq.1.or.nptype(i).eq.7.or.nptype(i).eq.10) then
      if (abs(twopot(2,i)).gt.1.0d-8) then
        rhopot(i) = 1.0_dp/twopot(2,i)
      else
        rhopot(i) = 0.0_dp
      endif
    endif
  enddo
!
  return
  end
