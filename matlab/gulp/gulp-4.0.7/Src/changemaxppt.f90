  subroutine changemaxppt
!
!  Alters the size of the arrays associated with maxppt
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use potentialpoints
  use reallocate
  implicit none
!
  integer(i4) :: ierror
!
  call realloc(xpotpt,maxppt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxppt','xpotpt')
  call realloc(ypotpt,maxppt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxppt','ypotpt')
  call realloc(zpotpt,maxppt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxppt','zpotpt')
  call realloc(vpotpt,maxppt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxppt','vpotpt')
!
  return
  end
