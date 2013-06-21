  subroutine changemaxfstress
!
!  Alters the size of the arrays associated with maxfstress
!
!   4/07 Weight array added
!   4/07 Dimension of fstress reduced by factor of 3
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
  use configurations
  use observables
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxfstress = 0
!
!  Configuration data
!
  call realloc(nfstrcfg,maxfstress,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfstress','nfstrcfg')
  call realloc(nfstrt,maxfstress,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfstress','nfstrt')
  call realloc(fstress,maxfstress,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfstress','fstress')
  call realloc(fstressweight,maxfstress,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfstress','fstressweight')
!
!  Initialise new parts of data arrays
!
  if (maxfstress.gt.oldmaxfstress) then
    do i = oldmaxfstress+1,maxfstress
      call initmaxfstressdefaults(i)
    enddo
  endif
!
!  Save current value of maxfstress for next call
!
  oldmaxfstress = maxfstress
!
  return
  end
