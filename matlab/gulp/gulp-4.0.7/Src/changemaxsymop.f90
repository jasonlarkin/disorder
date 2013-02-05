  subroutine changemaxsymop
!
!  Alters the size of the arrays associated with maxsymop
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use configurations
  use reallocate
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror
!
!  Check that maxsymop does not exceed 48 since symmetry can't handle this
!
  if (maxsymop.gt.48) then
    call outerror('number of symmetry operators exceeds maximum possible (48)',0_i4)
    call stopnow('changemaxsymop')
  endif
!
!  Configuration data
!
  call realloc(ropcfg,3_i4,3_i4,maxsymop,maxcfg,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsymop','ropcfg')
  call realloc(vitcfg,3_i4,maxsymop,maxcfg,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsymop','vitcfg')
!
  return
  end
