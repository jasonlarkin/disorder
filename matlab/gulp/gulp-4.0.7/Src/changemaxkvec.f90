  subroutine changemaxkvec
!
!  Alters the size of the arrays associated with maxkvec
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
  use kspace
  use reallocate
  implicit none
!
  integer(i4) :: ierror
!
  call realloc(indk,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','indk')
  call realloc(argc,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','argc')
  call realloc(sine,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','sine')
  call realloc(csin,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','csin')
  call realloc(kmod,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','kmod')
  call realloc(ktrm,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','ktrm')
  call realloc(ktrms,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','ktrms')
  call realloc(xrk,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','xrk')
  call realloc(yrk,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','yrk')
  call realloc(zrk,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','zrk')
  call realloc(xrk0,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','xrk0')
  call realloc(yrk0,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','yrk0')
  call realloc(zrk0,maxkvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkvec','zrk0')
!
  return
  end
