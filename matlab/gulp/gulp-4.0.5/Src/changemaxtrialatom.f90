  subroutine changemaxtrialatom
!
!  Alters the size of the arrays associated with maxtrialatom
!
!   1/08 Created from changemaxcreated
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use montecarlo
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
  call realloc(nptrtrialatom,maxtrialatom,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtrialatom','nptrtrialatom')
!
  return
  end
