  subroutine changemaxmol
!
!  Alters the size of the arrays associated with maxmol
!
!   7/07 GCMC molecule flag added
!   6/09 New molecule indexing arrays added
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
  use molecule
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxmol = 0
!
  call realloc(moldim,maxmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmol','moldim')
  call realloc(moldimi,maxmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmol','moldimi')
  call realloc(molgcmc,maxmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmol','molgcmc')
  call realloc(nmolatom,maxmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmol','nmolatom')
  call realloc(nmolptr,maxmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmol','nmolptr')
  call realloc(lgcmcmol,maxmol,ierror)
  if (ierror.ne.0) call outofmemory('changemaxmol','lgcmcmol')
!
!  Initialise new parts of data arrays
!
  if (maxmol.gt.oldmaxmol) then
    do i = oldmaxmol+1,maxmol
      call initmaxmoldefaults(i)
    enddo
  endif
!
!  Save current value of maxmol for next call
!
  oldmaxmol = maxmol
!
  return
  end
