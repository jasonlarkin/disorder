  subroutine changemaxkpt
!
!  Alters the size of the arrays associated with maxkpt
!
!   3/09 lkptdispersion added
!   9/10 Neutron scattering modifications added
!   9/10 Allocation of freq moved to subroutine.
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
  use dispersion
  use frequencies, only : maxfkpt
  use ksample
  use reallocate
  implicit none
!
  integer(i4) :: ierror
!
!  K point sampling
!
  call realloc(lkptdispersion,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','lkptdispersion')
  call realloc(xkpt,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','xkpt')
  call realloc(ykpt,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','ykpt')
  call realloc(zkpt,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','zkpt')
  call realloc(wkpt,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','wkpt')
  call realloc(nkptcfg,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','nkptcfg')
!
!  Dispersion curves
!
  call realloc(xdisp,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','xdisp')
  call realloc(ydisp,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','ydisp')
  call realloc(zdisp,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','zdisp')
  call realloc(ndispcfg,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','ndispcfg')
  call realloc(ndstart,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','ndstart')
  call realloc(ndend,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','ndend')
  call realloc(ndde,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','ndde')
  call realloc(ndds,maxkpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxkpt','ndds')
!
!  Frequencies
!
  if (maxkpt.gt.maxfkpt) then
    maxfkpt = maxkpt
    call changemaxfkpt
  endif
!
  return
  end
