  subroutine changemaxnppa
!
!  Alters the size of the arrays associated with maxnppa
!
!  12/08 Migrated to version 3.5 and converted to f90 format
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use cosmo,       only : maxnppa, cosmowt, sphere2
  use cosmopwtloc, only : npwtloc, npwtptrloc, maxnpwtloc
  use current,     only : maxat
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxnppa = 0
!
  call realloc(npwtloc,maxnppa,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnppa','npwtloc')
  call realloc(npwtptrloc,maxnpwtloc,maxnppa,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnppa','npwtptrloc')
!
  call realloc(cosmowt,maxnppa,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnppa','cosmowt')
  call realloc(sphere2,3_i4,maxnppa,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnppa','sphere2')
!
  if (maxnppa.gt.oldmaxnppa) then
    do i = oldmaxnppa+1,maxnppa
      call initmaxnppadefaults(i)
    enddo
  endif
!
!  Save current value of maxnppa for next call
!
  oldmaxnppa = maxnppa
!
  return
  end
