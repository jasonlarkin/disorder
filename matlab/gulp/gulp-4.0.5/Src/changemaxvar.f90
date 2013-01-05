  subroutine changemaxvar
!
!  Alters the size of the arrays associated with maxvar
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
  use configurations
  use optimisation
  use reallocate
  use xcgc
  implicit none
!
  integer(i4)       :: ierror
!
  call realloc(gc,maxvar,ierror)
  if (ierror.ne.0) call outofmemory('changemaxvar','gc')
  call realloc(ioptcfg,maxvar,ierror)
  if (ierror.ne.0) call outofmemory('changemaxvar','ioptcfg')
  call realloc(rmode,maxvar,ierror)
  if (ierror.ne.0) call outofmemory('changemaxvar','rmode')
  call realloc(xc,maxvar,ierror)
  if (ierror.ne.0) call outofmemory('changemaxvar','xc')
  call realloc(xcother,maxvar,ierror)
  if (ierror.ne.0) call outofmemory('changemaxvar','xcother')
!
  return
  end
