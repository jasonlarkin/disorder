  subroutine changemaxone
!
!  Alters the size of the arrays associated with maxone
!
!   1/10 Created from changemaxpot
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
!  Julian Gale, NRI, Curtin University, January 2010
!
  use reallocate
  use one
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxone = 0
!
!  Potential data
!
  call realloc(onepot,maxone,ierror)
  if (ierror.ne.0) call outofmemory('changemaxone','onepot')
  call realloc(nspec11,maxone,ierror)
  if (ierror.ne.0) call outofmemory('changemaxone','nspec11')
  call realloc(nptyp11,maxone,ierror)
  if (ierror.ne.0) call outofmemory('changemaxone','nptyp11')
  call realloc_ch5(symbol1,maxone,ierror)
  if (ierror.ne.0) call outofmemory('changemaxone','symbol1')
!
!  Initialise defaults for new part of array
!
  if (maxone.gt.oldmaxone) then
    do i = oldmaxone+1,maxone
      call init1bodydefaults(i)
    enddo
  endif
!
!  Save current value of maxone for next call
!
  oldmaxone = maxone
!
  return
  end
