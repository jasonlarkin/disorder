  subroutine changemaxobs
!
!  Alters the size of the arrays associated with maxobs
!
!   4/08 nobptr3 added
!   4/08 freaction added
!   7/10 fparameter added
!   9/10 Initialisations now performed in a subroutine
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
!  Julian Gale, NRI, Curtin University, September 2010
!
  use configurations,   only : maxcfg
  use observables
  use reallocate
  implicit none
!
  integer(i4), save :: oldmaxobs = 0
  integer(i4)       :: ierror, i
!
!  Configuration data
!
  call realloc(fcalc,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','fcalc')
  call realloc(fcalcoriginal,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','fcalcoriginal')
  call realloc(finfo,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','finfo')
  call realloc(fobs,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','fobs')
  call realloc(fparameter,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','fparameter')
  call realloc(freaction,maxcfg,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','freaction')
  call realloc(nobcfg,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','nobcfg')
  call realloc(nobptr,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','nobptr')
  call realloc(nobptr2,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','nobptr2')
  call realloc(nobptr3,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','nobptr3')
  call realloc(nobtyp,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','nobtyp')
  call realloc(weight,maxobs,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobs','weight')
!
!  Initialise new parts of data arrays
!
  if (maxobs.gt.oldmaxobs) then
    do i = oldmaxobs+1,maxobs
      call initmaxobsdefaults(i)
    enddo
  endif
!
!  Save current value of maxobs for next call
!
  oldmaxobs = maxobs
!
  return
  end
