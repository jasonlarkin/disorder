  subroutine changemaxtdfield
!
!  Alters the size of the arrays associated with maxtdfield
!
!  12/12 Created from changemaxcfg
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, December 2012
!
  use configurations
  use field
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxtdfield = 0
!
!  Configuration data
!
  call realloc(td_fieldcfg,3_i4,maxtdfield,maxcfg,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtdfield','td_fieldcfg')
  call realloc(td_fielddirectioncfg,3_i4,maxtdfield,maxcfg,ierror)
  if (ierror.ne.0) call outofmemory('changemaxtdfield','td_fielddirectioncfg')
!
!  Initialise defaults for new part of array
!
  if (maxtdfield.gt.oldmaxtdfield) then
    do i = oldmaxtdfield+1,maxtdfield
      td_fieldcfg(1,i,1:maxcfg) = 0.0_dp
      td_fieldcfg(2,i,1:maxcfg) = 0.0_dp
      td_fieldcfg(3,i,1:maxcfg) = 0.0_dp
      td_fielddirectioncfg(1,i,1:maxcfg) = 0.0_dp
      td_fielddirectioncfg(2,i,1:maxcfg) = 0.0_dp
      td_fielddirectioncfg(3,i,1:maxcfg) = 1.0_dp
    enddo
  endif
!
!  Save current value of maxtdfield for next call
!
  oldmaxtdfield = maxtdfield
!
  return
  end
