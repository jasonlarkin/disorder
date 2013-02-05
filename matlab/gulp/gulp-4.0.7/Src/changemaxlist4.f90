  subroutine changemaxlist4
!
!  Alters the size of the arrays associated with maxlist4
!
!   9/06 ilnum added
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, September 2006
!
  use four
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
!  Four-body lists
!
  call realloc(nforptr,maxlist4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist4','nforptr')
  call realloc(icell41,maxlist4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist4','icell41')
  call realloc(icell42,maxlist4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist4','icell42')
  call realloc(icell43,maxlist4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist4','icell43')
  call realloc(ilind,maxlist4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist4','ilind')
  call realloc(ilnum,maxlist4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist4','ilnum')
  call realloc(jkind,maxlist4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist4','jkind')
!
  return
  end
