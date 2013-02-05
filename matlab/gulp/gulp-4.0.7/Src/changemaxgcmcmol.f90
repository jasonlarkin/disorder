  subroutine changemaxgcmcmol
!
!  Alters the size of the arrays associated with maxgcmcmol
!  and maxgcmcmolat
!
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
  use montecarlo
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxgcmcmol   = 0
!
  call realloc(ngcmcmolat,maxgcmcmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxgcmcmol','ngcmcmolat')
  call realloc(ngcmcmolnat,maxgcmcmolat,maxgcmcmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxgcmcmol','ngcmcmolnat')
  call realloc(ngcmcmoltype,maxgcmcmolat,maxgcmcmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxgcmcmol','ngcmcmoltype')
  call realloc(xgcmcmol,maxgcmcmolat,maxgcmcmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxgcmcmol','xgcmcmol')
  call realloc(ygcmcmol,maxgcmcmolat,maxgcmcmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxgcmcmol','ygcmcmol')
  call realloc(zgcmcmol,maxgcmcmolat,maxgcmcmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxgcmcmol','zgcmcmol')
!
!  Initialise new parts of data arrays
!
  if (maxgcmcmol.gt.oldmaxgcmcmol) then
    do i = oldmaxgcmcmol+1,maxgcmcmol
      call initmaxgcmcmoldefaults(i)
    enddo
  endif
!
!  Save current value of maxgcmcmol for next call
!
  oldmaxgcmcmol = maxgcmcmol
!
  return
  end
