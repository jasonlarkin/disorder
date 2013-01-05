  subroutine changemaxlib
!
!  Alters the size of the arrays associated with maxlib
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
  use library
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxlib = 0
!
!  Library data
!
  call realloc_ch80(libname,maxlib,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlib','libname')
!
!  Initialise defaults for new part of array
!
  if (maxlib.gt.oldmaxlib) then
    do i = oldmaxlib+1,maxlib
      call initmaxlibdefaults(i)
    enddo
  endif
!
!  Save current value of maxlib for next call
!
  oldmaxlib = maxlib
!
  return
  end
