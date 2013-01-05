  subroutine changemaxlist3
!
!  Alters the size of the arrays associated with maxlist3
!
!   6/09 Module name changed from three to m_three
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use reallocate
  use m_three
  implicit none
!
  integer(i4)       :: ierror
!
!  Three-body lists
!
  call realloc(nthbptr,maxlist3,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist3','nthbptr')
  call realloc(icell31,maxlist3,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist3','icell31')
  call realloc(icell32,maxlist3,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist3','icell32')
  call realloc(i3ind,maxlist3,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist3','i3ind')
  call realloc(j3ind,maxlist3,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist3','j3ind')
  call realloc(k3ind,maxlist3,ierror)
  if (ierror.ne.0) call outofmemory('changemaxlist3','k3ind')
!
  return
  end
