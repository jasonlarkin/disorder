  subroutine changemaxbondvec
!
!  Alters the size of the arrays associated with maxbondvec
!
!   2/07 nbtype(2)vec added
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, February 2007
!
  use bondvectors
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
  call realloc(nbondvecind,maxbondvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbondvec','nbondvecind')
  call realloc(nbtypevec,maxbondvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbondvec','nbtypevec')
  call realloc(nbtype2vec,maxbondvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbondvec','nbtype2vec')
  call realloc(lbondedvec,maxbondvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbondvec','lbondedvec')
  call realloc(l2bondsvec,maxbondvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbondvec','l2bondsvec')
  call realloc(l3bondsvec,maxbondvec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbondvec','l3bondsvec')
!
  return
  end
