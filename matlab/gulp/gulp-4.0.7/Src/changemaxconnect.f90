  subroutine changemaxconnect
!
!  Alters the size of the arrays associated with maxconnect
!
!   8/06 nconnecttype added
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
  use molecule
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxconnect = 0
!
  call realloc(n1connect,maxconnect,ierror)
  if (ierror.ne.0) call outofmemory('changemaxconnect','n1connect')
  call realloc(n2connect,maxconnect,ierror)
  if (ierror.ne.0) call outofmemory('changemaxconnect','n2connect')
  call realloc(nconnectcfg,maxconnect,ierror)
  if (ierror.ne.0) call outofmemory('changemaxconnect','nconnectcfg')
  call realloc(nconnectind,maxconnect,ierror)
  if (ierror.ne.0) call outofmemory('changemaxconnect','nconnectind')
  call realloc(nconnecttype,2_i4,maxconnect,ierror)
  if (ierror.ne.0) call outofmemory('changemaxconnect','nconnecttype')
!
!  Initialise new parts of data arrays
!
  if (maxconnect.gt.oldmaxconnect) then
    do i = oldmaxconnect+1,maxconnect
      call initmaxconnectdefaults(i)
    enddo
  endif
!
!  Save current value of maxconnect for next call
!
  oldmaxconnect = maxconnect
!
  return
  end
