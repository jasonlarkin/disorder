  subroutine changemaxmany
!
!  Alters the size of the arrays associated with maxmany
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
  use feworkspace
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
  call realloc(nptrfork,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','nptrfork')
  call realloc(nptrforl,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','nptrforl')
  call realloc(nptrmanyk,maxmany,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','nptrmanyk')
  call realloc(d33,54_i4,maxmany,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d33')
  call realloc(d33r,54_i4,maxmany,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d33r')
  call realloc(d33i,54_i4,maxmany,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d33i')
  call realloc(d33s,108_i4,maxmany,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d33s')
  call realloc(d33rs,108_i4,maxmany,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d33rs')
  call realloc(d33is,108_i4,maxmany,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d33is')
  call realloc(d34,27_i4,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d34')
  call realloc(d34r,27_i4,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d34r')
  call realloc(d34i,27_i4,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d34i')
  call realloc(d34s,54_i4,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d34s')
  call realloc(d34rs,54_i4,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d34rs')
  call realloc(d34is,54_i4,maxmany2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmany','d34is')
!
  return
  end
