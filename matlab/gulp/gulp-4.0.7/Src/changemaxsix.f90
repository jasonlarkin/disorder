  subroutine changemaxsix
!
!  Alters the size of the arrays associated with maxsix
!
!   8/06 n6botype added
!   9/06 Array for literal symbols added
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
  use reallocate
  use six
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxsix = 0
!
!  Six-body data
!
  call realloc_ch5(symbol6,6_i4,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','symbol6')
  call realloc(sixk,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','sixk')
  call realloc(six1,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','six1')
  call realloc(six2,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','six2')
  call realloc(six3,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','six3')
  call realloc(six4,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','six4')
  call realloc(six5,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','six5')
  call realloc(n6botype,2_i4,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','n6botype')
  call realloc(npsix,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','npsix')
  call realloc(nsspec1,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsspec1')
  call realloc(nsspec2,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsspec2')
  call realloc(nsspec3,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsspec3')
  call realloc(nsspec4,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsspec4')
  call realloc(nsspec5,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsspec5')
  call realloc(nsspec6,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsspec6')
  call realloc(nsptyp1,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsptyp1')
  call realloc(nsptyp2,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsptyp2')
  call realloc(nsptyp3,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsptyp3')
  call realloc(nsptyp4,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsptyp4')
  call realloc(nsptyp5,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsptyp5')
  call realloc(nsptyp6,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsptyp6')
  call realloc(nsixty,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','nsixty')
  call realloc(mmsexc,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','mmsexc')
  call realloc(lsintra,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','lsintra')
  call realloc(lsinter,maxsix,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsix','lsinter')
!
!  Initialise defaults for new part of array
!
  if (maxsix.gt.oldmaxsix) then
    do i = oldmaxsix+1,maxsix
      call initmaxsixdefaults(i)
    enddo
  endif
!
!  Save current value of maxsix for next call
!
  oldmaxsix = maxsix
!
  return
  end
