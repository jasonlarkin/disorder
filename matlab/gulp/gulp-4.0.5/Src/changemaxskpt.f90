  subroutine changemaxskpt
!
!  Alters the size of the arrays associated with maxskpt
!
!   9/10 Created from changemaxkpt
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
  use current,         only : maxat
  use frequencies,     only : maxfkpt
  use ksample_scatter
  use reallocate
  use scatterdata,     only : sofomega, Qvector, tauvector
  implicit none
!
  integer(i4) :: ierror
!
!  K point sampling for scattering
!
  call realloc(xskpt,maxskpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxskpt','xskpt')
  call realloc(yskpt,maxskpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxskpt','yskpt')
  call realloc(zskpt,maxskpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxskpt','zskpt')
  call realloc(wskpt,maxskpt,ierror)
  if (ierror.ne.0) call outofmemory('changemaxskpt','wskpt')
!
!  Neutron scattering
!
  call realloc(Qvector,3_i4,maxskpt,ierror)
  if (ierror.gt.0) call outofmemory('changemaxat','Qvector')
  call realloc(tauvector,3_i4,maxskpt,ierror)
  if (ierror.gt.0) call outofmemory('changemaxat','tauvector')
  call realloc(sofomega,3_i4*maxat,maxskpt,ierror)
  if (ierror.gt.0) call outofmemory('changemaxat','sofomega')
!
!  Frequencies
!
  if (maxskpt.gt.maxfkpt) then
    maxfkpt = maxskpt
    call changemaxfkpt
  endif
!
  return
  end
