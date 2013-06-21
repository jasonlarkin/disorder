  subroutine changemaxdef
!
!  Alters the size of the arrays associated with maxdef
!
!   9/05 Explicit initialisation of defect quantities added
!   9/10 Initialisation performed in subroutine
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
  use defects
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxdef = 0
!
  call realloc(inddeffix,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','inddeffix')
  call realloc(ldeffix,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','ldeffix')
  call realloc(ndefcfg,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','ndefcfg')
  call realloc(ndefnat,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','ndefnat')
  call realloc(ndeftyp,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','ndeftyp')
  call realloc(ndeftp,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','ndeftp')
  call realloc(xdef,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','xdef')
  call realloc(ydef,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','ydef')
  call realloc(zdef,maxdef,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdef','zdef')
!
!  Initialise defaults for new part of array
!
  if (maxdef.gt.oldmaxdef) then
    do i = oldmaxdef+1,maxdef
      call initmaxdefdefaults(i)
    enddo
  endif
!
!  Save current value of maxdef for next call
!
  oldmaxdef = maxdef
!
  return
  end
