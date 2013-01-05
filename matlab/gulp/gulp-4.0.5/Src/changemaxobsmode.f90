  subroutine changemaxobsmode
!
!  Alters the size of the arrays associated with maxobsmode
!
!   1/12 Created
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, January 2012
!
  use current,      only : maxat
  use observables
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
!  Configuration data
!
  call realloc(nobsmodeat,maxobsmode,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobsmode','nobsmodeat')
  call realloc(fobsmode,3_i4,maxat,maxobsmode,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobsmode','fobsmode')
  call realloc(fobsmodefreq,maxobsmode,ierror)
  if (ierror.ne.0) call outofmemory('changemaxobsmode','fobsmodefreq')
!
  return
  end
