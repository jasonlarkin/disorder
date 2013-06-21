  subroutine changemaxcellpernodebo
!
!  Alters the size of the arrays associated with maxcellpernodebo
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, September 2004
!
  use spatialbo
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
!  Spatial decomposition data
!
  call realloc(lbuffercellbo,maxcellpernodebo,ierror)
  if (ierror.ne.0) call outofmemory('changemaxcellpernodebo','lbuffercellbo')
  call realloc(ncellnodeptrbo,maxcellpernodebo,ierror)
  if (ierror.ne.0) call outofmemory('changemaxcellpernodebo','ncellnodeptrbo')
!
  return
  end
