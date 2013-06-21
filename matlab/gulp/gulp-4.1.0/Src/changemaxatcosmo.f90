  subroutine changemaxatcosmo
!
!  Alters the size of the arrays in COSMO associated with maxat
!
!   1/05 nallnearsegrptr added
!  12/08 Migrated to version 3.5 and converted to f90 format
!   9/10 npwt arrays added
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
  use cosmo
  use cosmopwtloc,    only : npwtloc, npwtptrloc, maxnpwtloc
  use current
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror
!
  call realloc(atsrad,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','atsrad')
  call realloc(cosmotm,3_i4,3_i4,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','cosmotm')
  call realloc(cosmowt,maxnppa,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','cosmowt')
  call realloc(nallnearsegrptr,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','nallnearsegrptr')
  call realloc(npwtloc,maxnppa,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','npwtloc')
  call realloc(npwtptrloc,maxnpwtloc,maxnppa,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxat','npwtptrloc')
!
  return
  end
