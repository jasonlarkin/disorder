  subroutine changemaxbond
!
!  Alters the size of the arrays associated with maxbond
!
!   2/07 nbondedtype added
!   5/08 bonding arrays for defects changed to match structure for perfect system
!   9/10 Initialisations now performed in a subroutin
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
  use current
  use defects
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxbond = 0
!
!  Bulk bonding
!
  call realloc(nbonded,maxbond,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbond','nbonded')
  call realloc(nbondedtype,2_i4,maxbond,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbond','nbondedtype')
  call realloc(nbondind,maxbond,maxat,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbond','nbondind')
!
!  Defect bonding
!
  call realloc(nbondeddef,maxbond,maxr1at,ierror)
  if (ierror.ne.0) call outofmemory('changemaxbond','nbondeddef')
!
!  Initialise new part of data array
!
  if (maxbond.gt.oldmaxbond) then
    do i = oldmaxbond+1,maxbond
      call initmaxbonddefaults(i)
    enddo
  endif
!
!  Save current value of maxbond for next call
!
  oldmaxbond = maxbond
!
  return
  end
