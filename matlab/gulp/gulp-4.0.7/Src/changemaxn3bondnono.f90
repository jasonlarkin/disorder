  subroutine changemaxn3bondnono
!
!  Alters the size of the arrays associated with maxn3bondnono
!
!   6/08 Created from changemaxthb
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
!  Three-body data
!
  call realloc(n3bondno,maxn3bondnono,2_i4,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxn3bondnono','n3bondno')
!
  return
  end
