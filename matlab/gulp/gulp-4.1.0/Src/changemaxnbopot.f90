  subroutine changemaxnbopot
!
!  Alters the size of the arrays associated with maxnbopot
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
  use bondorderdata
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxnbopot = 0
!
!  Bond order potential data
!
  call realloc(BOacoeff,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','BOacoeff')
  call realloc(BObcoeff,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','BObcoeff')
  call realloc(BOchiA,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','BOchiA')
  call realloc(BOchiR,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','BOchiR')
  call realloc(BOzacoeff,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','BOzacoeff')
  call realloc(BOzbcoeff,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','BOzbcoeff')
  call realloc(BOcombi,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','BOcombi')
  call realloc(rBOmax,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','rBOmax')
  call realloc(rBOmin,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','rBOmin')
  call realloc(nBOspec1,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','nBOspec1')
  call realloc(nBOspec2,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','nBOspec2')
  call realloc(nBOtyp1,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','nBOtyp1')
  call realloc(nBOtyp2,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','nBOtyp2')
  call realloc(nBOtypeT,maxnbopot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnbopot','nBOtypeT')
!
!  Initialise defaults for new part of array
!
  if (maxnbopot.gt.oldmaxnbopot) then
    do i = oldmaxnbopot+1,maxnbopot
      call initmaxnbopotdefaults(i)
    enddo
  endif
!
!  Save current value of maxnbopot for next call
!
  oldmaxnbopot = maxnbopot
!
  return
  end
