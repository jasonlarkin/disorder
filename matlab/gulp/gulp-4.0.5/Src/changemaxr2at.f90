  subroutine changemaxr2at
!
!  Alters the size of the arrays associated with maxr2at
!
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
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
  use eam,         only : maxmeamcomponent
  use reallocate
  use region2a
  implicit none
!
  integer(i4) :: ierror
!
  call realloc(dscrhor2d,maxmeamcomponent,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','dscrhor2d')
  call realloc(dscrhor2p,maxmeamcomponent,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','dscrhor2p')
  call realloc(ldbr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','ldbr2a')
  call realloc(nr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','nr2a')
  call realloc(ntr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','ntr2a')
  call realloc(nmr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','nmr2a')
  call realloc(nmir2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','nmir2a')
  call realloc(nps,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','nps')
  call realloc(ndrel2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','ndrel2a')
  call realloc(ndrelop2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','ndrelop2a')
  call realloc(ndeqv2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','ndeqv2a')
  call realloc(ndsptr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','ndsptr2a')
  call realloc(xr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','xr2a')
  call realloc(yr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','yr2a')
  call realloc(zr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','zr2a')
  call realloc(xdis,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','xdis')
  call realloc(ydis,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','ydis')
  call realloc(zdis,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','zdis')
  call realloc(qr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','qr2a')
  call realloc(or2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','or2a')
  call realloc(rr2a,maxr2at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxr2at','rr2a')
!
  return
  end
