  subroutine changemaxnboQ0
!
!  Alters the size of the arrays associated with maxnboQ0
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
  integer(i4), save :: oldmaxnboQ0 = 0
!
!  Bond order potential data
!
  call realloc(BOq0pot,maxnboQ0,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ0','BOq0pot')
  call realloc(BOq0ref,maxnboQ0,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ0','BOq0ref')
  call realloc(BOq0rho,maxnboQ0,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ0','BOq0rho')
  call realloc(nBOspecQ0,maxnboQ0,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ0','nBOspecQ0')
  call realloc(nBOtypQ0,maxnboQ0,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ0','nBOtypQ0')
  call realloc(nBOtypeQ0,maxnboQ0,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboQ0','nBOtypeQ0')
!
!  Initialise defaults for new part of array
!
  if (maxnboQ0.gt.oldmaxnboQ0) then
    do i = oldmaxnboQ0+1,maxnboQ0
      call initmaxnboq0defaults(i)
    enddo
  endif
!
!  Save current value of maxnboQ for next call
!
  oldmaxnboQ0 = maxnboQ0
!
  return
  end
