  subroutine changemaxfcon
!
!  Alters the size of the arrays associated with maxfcon
!
!   9/05 fconpower added
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
!  Julian Gale, NRI, Curtin University, September 2005
!
  use fitting
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
!  Configuration data
!
  call realloc(fconadd,maxfcon,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfcon','fconadd')
  call realloc(fconco,maxfcon,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfcon','fconco')
  call realloc(fconpower,maxfcon,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfcon','fconpower')
  call realloc(nfcotyp,maxfcon,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfcon','nfcotyp')
  call realloc(nfcfix,maxfcon,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfcon','nfcfix')
  call realloc(nfcvar,maxfcon,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfcon','nfcvar')
!
  return
  end
