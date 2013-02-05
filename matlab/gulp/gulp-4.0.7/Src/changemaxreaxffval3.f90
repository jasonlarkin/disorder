  subroutine changemaxreaxFFval3
!
!  Alters the size of the arrays associated with maxreaxFFval3
!
!   4/08 Created from changemaxreaxffspec
!   7/08 pval6 added to reaxFFval3 array
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
  use library
  use reallocate
  use reaxFFdata
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4)       :: maxreaxFFspec2
  integer(i4), save :: oldmaxreaxFFval3 = 0
!
!  Some things depend on pairs of species
!
  maxreaxFFspec2 = maxreaxFFspec*(maxreaxFFspec + 1)/2
!
!  Species data
!
  call realloc(reaxFFval3,6_i4,maxreaxFFval3,maxreaxFFspec2,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFval3')
!
!  Initialise new parts of data arrays
!
  do i = oldmaxreaxFFval3+1,maxreaxFFval3
    call initmaxreaxffval3defaults(i)
  enddo
!
!  Save current value of maxreaxFFspec for next call
!
  oldmaxreaxFFval3 = maxreaxFFval3
!
  return
  end
