  subroutine changemaxsa(vec,maxdim)
!
!  Alters the size of the arrays within the type screening_atoms
!
!   4/09 Created
!   6/09 Distances added
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
  use eam,          only : screening_atoms
  use reallocate
  implicit none
!
!  Passed variables
!
  type(screening_atoms), intent(inout) :: vec
  integer(i4),           intent(in)    :: maxdim
!
!  Local variables
!
  integer(i4)       :: ierror
!
  vec%sa_maxdim = maxdim
!
  call realloc(vec%sa_atom,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_atom')
  call realloc(vec%sa_kxc,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_kmc')
  call realloc(vec%sa_rij,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_rij')
  call realloc(vec%sa_rik,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_rik')
  call realloc(vec%sa_rjk,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_rjk')
  call realloc(vec%sa_xik,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_xik')
  call realloc(vec%sa_yik,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_yik')
  call realloc(vec%sa_zik,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_zik')
  call realloc(vec%sa_xjk,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_xjk')
  call realloc(vec%sa_yjk,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_yjk')
  call realloc(vec%sa_zjk,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_zjk')
  call realloc(vec%sa_Sikj,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_Sikj')
  call realloc(vec%sa_dSikjdr,3_i4,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_dSikjdr')
  call realloc(vec%sa_drhototij,3_i4,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_drhototij')
  call realloc(vec%sa_drhototik,3_i4,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_drhototik')
  call realloc(vec%sa_drhototjk,3_i4,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_drhototjk')
  call realloc(vec%sa_drhotots,6_i4,maxdim,ierror)
  if (ierror.ne.0) call outofmemory('changemaxsa','sa_drhotots')
!
  return
  end
