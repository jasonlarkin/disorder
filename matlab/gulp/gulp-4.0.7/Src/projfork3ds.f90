  subroutine projfork3ds(nforkl,d34s,d34rs,dt11,dt21,dt31,dt22,dt32,dt33,dfeds)
!
!  Calculates the free energy derivative contribution from the
!  third atom k to fourth atom l distance for the i-i dynamical 
!  matrix block (on diagonal) to the strain derivative.
!
!   8/98 Created from projthbk3ds
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use current
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)        :: nforkl
  real(dp)           :: d34rs(54,*)
  real(dp)           :: d34s(54,*)
  real(dp)           :: dfeds(*)
  real(dp)           :: dt11
  real(dp)           :: dt21
  real(dp)           :: dt31
  real(dp)           :: dt22
  real(dp)           :: dt32
  real(dp)           :: dt33
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: k
  integer(i4)        :: kk
  integer(i4)        :: kkk
  real(dp)           :: dfed1r
!
!  Loop over four body k-l pairs
!
  do ii = 1,nforkl
!
!  Loop over strain indices
!
    do kkk = 1,nstrains
      k = nstrptr(kkk)
      kk = 9*(k - 1)
!******************************************************************
!  Calculate k-l component - no assumption that d34 is symmetric  *
!******************************************************************
!
!  Calculate difference between phased on-diagonal elements and pure real
!
      dfed1r = dt11*(d34rs(1+kk,ii) - d34s(1+kk,ii)) + &
               dt21*(d34rs(2+kk,ii) - d34s(2+kk,ii)) + &
               dt31*(d34rs(3+kk,ii) - d34s(3+kk,ii)) + &
               dt21*(d34rs(4+kk,ii) - d34s(4+kk,ii)) + &
               dt22*(d34rs(5+kk,ii) - d34s(5+kk,ii)) + &
               dt32*(d34rs(6+kk,ii) - d34s(6+kk,ii)) + &
               dt31*(d34rs(7+kk,ii) - d34s(7+kk,ii)) + &
               dt32*(d34rs(8+kk,ii) - d34s(8+kk,ii)) + &
               dt33*(d34rs(9+kk,ii) - d34s(9+kk,ii))
!
!  Add on derivative contributions
!
      dfeds(kkk) = dfeds(kkk) + dfed1r
    enddo
  enddo
!
  return
  end
