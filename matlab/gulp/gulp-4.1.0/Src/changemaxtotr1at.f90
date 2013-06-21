  subroutine changemaxtotr1at
!
!  Alters the size of the arrays associated with maxtotr1at
!
!   3/13 Created from changemaxcfg
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use configurations
  use defects
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror
!
!  Configuration data
!
  call realloc(natdefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','natdefecfg')
  call realloc(ndefindcfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','ndefindcfg')
  call realloc(ndefmolcfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','ndefmolcfg')
  call realloc(ntypdefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','ntypdefecfg')
  call realloc(nreldefcfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','nreldefcfg')
  call realloc(ldefbsmatcfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','ldefbsmatcfg')
  call realloc(ldqmatomcfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','ldqmatomcfg')
  call realloc(xdefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','xdefecfg')
  call realloc(ydefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','ydefecfg')
  call realloc(zdefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','zdefecfg')
  call realloc(qdefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','qdefecfg')
  call realloc(occdefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','occdefecfg')
  call realloc(radefecfg,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','radefecfg')
  call realloc(idoptcfg,3_i4,maxtotr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtotr1at','idoptcfg')
!
  return
  end
