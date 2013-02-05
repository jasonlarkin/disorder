  subroutine changemaxnebreplicatot
!
!  Alters the size of the arrays associated with maxnebreplicatot
!
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
  use current,        only : maxat
  use neb
  use reallocate
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxnebreplicatot = 0
!
  call realloc(nnebreplicano,maxnebreplicatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnebreplicatot','nnebreplicano')
  call realloc(nebreplicacfgptr,maxnebreplicatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnebreplicatot','nebreplicacfgptr')
  call realloc(nebreplicacell,6_i4,maxnebreplicatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnebreplicatot','nebreplicacell')
  call realloc(nebreplicaradius,maxat,maxnebreplicatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnebreplicatot','nebreplicaradius')
  call realloc(nebreplicaxyz,3_i4,maxat,maxnebreplicatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnebreplicatot','nebreplicaxyz')
!
!  Initialise new parts of data arrays
!
  if (maxnebreplicatot.gt.oldmaxnebreplicatot) then
    do i = oldmaxnebreplicatot+1,maxnebreplicatot
      call initmaxnebreplicatotdefaults(i)
    enddo
  endif
!
!  Save current value of maxnebreplicatot for next call
!
  oldmaxnebreplicatot = maxnebreplicatot
!
  return
  end
