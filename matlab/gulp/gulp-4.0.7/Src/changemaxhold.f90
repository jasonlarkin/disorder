  subroutine changemaxhold
!
!  Alters the size of the arrays associated with maxHold1-4.
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
!  created for scatter.f90 by 
!
!  Daniel L. Roach, University of Salford 2010
!
!
  use reallocate
  use scatterdata
  implicit none
!
  integer(i4) :: ierror

!  Neutron Scattering Arrays
!
  call realloc(Hold_Q,maxHold1,maxHold2,maxHold3,maxHold4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxhold','Hold_Q')
  call realloc(Hold_smq,maxHold1,maxHold2,maxHold3,maxHold4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxhold','Hold_smq')
  call realloc(Hold_tau,maxHold1,maxHold2,maxHold3,maxHold4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxhold','Hold_tau')
!
  return
  end
