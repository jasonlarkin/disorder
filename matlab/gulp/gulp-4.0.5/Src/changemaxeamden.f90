  subroutine changemaxeamden
!
!  Alters the size of the arrays associated with maxeamden
!
!   3/06 Created from changemaxeamspec.f
!   5/06 Bug in right hand dimension of arrays corrected on 
!        call to realloc
!   2/07 Size of denpar increased in first dimension to 15
!  10/08 MEAM modifications added - denpar increased in dimension
!  12/08 eammeamcoeff array changed from density to function related
!   9/10 Initialisations now performed in a subroutine
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
  use eam
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxeamden = 0
!
!  EAM data
!
  call realloc(denpar,16_i4,maxmeamorder,maxeamden,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamden','denpar')
  call realloc(ndenfn,maxeamden,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamden','ndenfn')
  call realloc(neammeamorder,maxeamden,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamden','neammeamorder')
!
!  Initialise defaults for new part of array
!
  if (maxeamden.gt.oldmaxeamden) then
    do i = oldmaxeamden+1,maxeamden
      call initmaxeamdendefaults(i)
    enddo
  endif
!
!  Save current value of maxeamden for next call
!
  oldmaxeamden = maxeamden
!
  return
  end
