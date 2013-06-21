  subroutine changemaxword
!
!  Alters the size of the arrays associated with maxword
!
!  12/08 Module input renamed to gulpinput
!   8/10 floatwords array added
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, August 2010
!
  use gulpinput
  use reallocate
  implicit none
!
  integer(i4) :: ierror
!
  call realloc_ch80(floatwords,maxword,ierror)
  if (ierror.ne.0) call outofmemory('changemaxword','floatwords')
  call realloc_ch80(words,maxword,ierror)
  if (ierror.ne.0) call outofmemory('changemaxword','words')
  call realloc(floats,maxword,ierror)
  if (ierror.ne.0) call outofmemory('changemaxword','floats')
  call realloc(nlorder,2_i4*maxword,ierror)
  if (ierror.ne.0) call outofmemory('changemaxword','nlorder')
!
  return
  end
