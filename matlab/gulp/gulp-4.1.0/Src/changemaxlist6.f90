  subroutine changemaxlist6
!
!  Alters the size of the arrays associated with maxlist6
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
!  Julian Gale, NRI, Curtin University, July 2006
!
  use six
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
!  Six-body lists
!
  call realloc(nsixptr,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','nforptr')
  call realloc(icell61,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','icell61')
  call realloc(icell62,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','icell62')
  call realloc(icell63,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','icell63')
  call realloc(icell64,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','icell64')
  call realloc(icell65,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','icell65')
  call realloc(ijind,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','ijind')
  call realloc(klind,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','klind')
  call realloc(mnind,maxlist6,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist6','mnind')
!
  return
  end
