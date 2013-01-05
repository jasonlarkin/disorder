  subroutine changemaxdis
!
!  Alters the size of the arrays associated with maxdis
!
!   1/05 cellindex array added
!   3/07 nbotype/nbotype2 added
!   2/09 Argument removed from changemaxdis call
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
  use current,      only : maxdis
  use reallocate
  use realvectors
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror
!
  call realloc(cellindex,3_i4,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','cellindex')
  call realloc(nbotype,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','nbotype')
  call realloc(nbotype2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','nbotype2')
  call realloc(lbonded,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','lbonded')
  call realloc(l2bonds,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','l2bonds')
  call realloc(l3bonds,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','l3bonds')
  call realloc(lptrmol,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','lptrmol')
  call realloc(deriv,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','deriv')
  call realloc(deriv2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','deriv2')
  call realloc(deriv3,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','deriv3')
  call realloc(derive0,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','derive0')
  call realloc(derive,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','derive')
  call realloc(derive2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','derive2')
  call realloc(derive3,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','derive3')
  call realloc(dist,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','dist')
  call realloc(dist2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','dist2')
  call realloc(dist3,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','dist3')
  call realloc(d0i,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','d0i')
  call realloc(d0j,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','d0j')
  call realloc(d1i,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','d1i')
  call realloc(d1j,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','d1j')
  call realloc(d2i2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','d2i2')
  call realloc(d2ij,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','d2ij')
  call realloc(d2j2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','d2j2')
  call realloc(rderiv,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','rderiv')
  call realloc(rpd,maxdis,6_i4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','rpd')
  call realloc(rtrm2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','rtrm2')
  call realloc(rtrm3,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','rtrm3')
  call realloc(rtrm32,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','rtrm32')
  call realloc(xtmp,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','xtmp')
  call realloc(ytmp,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','ytmp')
  call realloc(ztmp,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','ztmp')
  call realloc(xtmp2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','xtmp2')
  call realloc(ytmp2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','ytmp2')
  call realloc(ztmp2,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','ztmp2')
  call realloc(xtmp3,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','xtmp3')
  call realloc(ytmp3,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','ytmp3')
  call realloc(ztmp3,maxdis,ierror)
  if (ierror.ne.0) call outofmemory('changemaxdis','ztmp3')
!
  return
  end
