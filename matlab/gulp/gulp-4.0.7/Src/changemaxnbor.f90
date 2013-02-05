  subroutine changemaxnboR
!
!  Alters the size of the arrays associated with maxnboR
! 
!   6/10 Option to for second atom specifier added to 
!        repulsive terms.
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
  integer(i4), save :: oldmaxnboR = 0
!
!  Bond order potential data
!
  call realloc(BOccoeffR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','BOccoeffR')
  call realloc(BOdcoeffR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','BOdcoeffR')
  call realloc(BOecoeffR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','BOecoeffR')
  call realloc(BOhcoeffR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','BOhcoeffR')
  call realloc(BOlcoeffR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','BOlcoeffR')
  call realloc(BOmcoeffR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','BOmcoeffR')
  call realloc(BOncoeffR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','BOncoeffR')
  call realloc(nBOspecR1,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','nBOspecR1')
  call realloc(nBOspecR2,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','nBOspecR2')
  call realloc(nBOtypR1,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','nBOtypR1')
  call realloc(nBOtypR2,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','nBOtypR2')
  call realloc(nBOtypeR,maxnboR,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnboR','nBOtypeR')
!
!  Initialise defaults for new part of array
!
  if (maxnboR.gt.oldmaxnboR) then
    do i = oldmaxnboR+1,maxnboR
      call initmaxnbordefaults(i)
    enddo
  endif
!
!  Save current value of maxnboR for next call
!
  oldmaxnboR = maxnboR
!
  return
  end
