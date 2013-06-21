  subroutine changemaxndistancetotal
!
!  Alters the size of the arrays associated with maxndistancetotal
!
!   1/05 Created
!   3/07 Bond type arrays added
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, March 2007
!
  use distances
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror
!
  call realloc(ndistancecell,3_i4,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','ndistancecell')
  call realloc(ndistanceptr,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','ndistanceptr')
  call realloc(ndistbotype,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','ndistbotype')
  call realloc(ndistbotype2,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','ndistbotype2')
  call realloc(distl1bond,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','distl1bond')
  call realloc(distl2bond,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','distl2bond')
  call realloc(distl3bond,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','distl3bond')
  call realloc(distlptrmol,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','distlptrmol')
  call realloc(distance,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','distance')
  call realloc(distance2,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','distance2')
  call realloc(distancexyz,3_i4,maxndistancetotal,ierror)
  if (ierror.ne.0) call outofmemory('changemaxndistancetotal','distancexyz')
!
  return
  end
