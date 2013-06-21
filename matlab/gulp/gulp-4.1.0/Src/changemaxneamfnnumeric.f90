  subroutine changemaxneamfnnumeric
!
!  Alters the size of the arrays associated with maxneamfnnumeric
!
!  10/05 Created from changemaxneamfnnumeric
!   5/06 Modified due to separation of neamspec and neamfnspec
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, May 2006
!
  use eam
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
!  EAM data
!
  call realloc(eamfnnumeric,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric')
  call realloc(eamfnnumeric1,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric1')
  call realloc(eamfnnumeric2,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric2')
  call realloc(eamfnnumeric3,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric3')
  call realloc(eamfnnumeric4,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric4')
  call realloc(eamfnnumeric5,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric5')
  call realloc(eamfnnumeric6,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric6')
  call realloc(eamfnnumeric7,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric7')
  call realloc(eamfnnumeric8,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxneamfnnumeric','eamfnnumeric8')
!
  return
  end
