  subroutine changemaxnboQ
!
!  Alters the size of the arrays associated with maxnboQ
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
  integer(i4), save :: oldmaxnboQ = 0
!
!  Bond order potential data
!
  call realloc(BOq0,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','BOq0')
  call realloc(rBOmaxQ,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','rBOmaxQ')
  call realloc(rBOminQ,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','rBOminQ')
  call realloc(nBOspecQ1,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','nBOspecQ1')
  call realloc(nBOspecQ2,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','nBOspecQ2')
  call realloc(nBOtypQ1,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','nBOtypQ1')
  call realloc(nBOtypQ2,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','nBOtypQ2')
  call realloc(nBOtaperQ,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','nBOtaperQ')
  call realloc(nBOtypeQ,maxnboQ,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ','nBOtypeQ')
!
!  Initialise defaults for new part of array
!
  if (maxnboQ.gt.oldmaxnboQ) then
    do i = oldmaxnboQ+1,maxnboQ
      call initmaxnboqdefaults(i)
    enddo
  endif
!
!  Save current value of maxnboQ for next call
!
  oldmaxnboQ = maxnboQ
!
  return
  end
