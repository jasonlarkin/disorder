  subroutine changemaxd1
!
!  Alters the size of the arrays associated with maxd1 or maxd1u
!
!   3/09 Non-radial arrays added
!   6/09 Virial arrays added
!   4/12 xvir, yvir and zvir removed
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
!  Julian Gale, NRI, Curtin University, April 2012
!
  use derivatives
  use reallocate
  implicit none
!
  integer(i4) :: ierror
!
  call realloc(raderv,maxd1,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','raderv')
  call realloc(xdrv,maxd1,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','xdrv')
  call realloc(ydrv,maxd1,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','ydrv')
  call realloc(zdrv,maxd1,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','zdrv')
  call realloc(xdrvnr,maxd1,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','xdrvnr')
  call realloc(ydrvnr,maxd1,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','ydrvnr')
  call realloc(zdrvnr,maxd1,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','zdrvnr')
!
  return
  end
