  subroutine changemaxfreqat(nfqat)
!
!  Alters the size of the arrays associated with maxfqat
!
!   4/11 Created from changemaxat
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
!  Julian Gale, NRI, Curtin University, April 2011
!
  use datatypes
  use frequencies,    only : freq, maxfkpt, maxfqat
  use reallocate
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: nfqat
!
!  Local variables
!
  integer(i4)             :: ierror
!
  if (nfqat.gt.maxfqat) then
    maxfqat = nfqat
    call realloc(freq,3_i4*maxfqat,maxfkpt,ierror)
    if (ierror.ne.0) call outofmemory('changemaxfreqat','freq')
  endif
!
  return
  end
