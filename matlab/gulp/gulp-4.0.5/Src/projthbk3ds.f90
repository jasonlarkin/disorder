  subroutine projthbk3ds(nthbk,d33s,d33rs,dt11,dt21,dt31,dt22,dt32,dt33,dfeds)
!
!  Calculates the free energy derivative contribution from the
!  third atom k for the i-i dynamical matrix block (on diagonal)
!  to the strain derivative
!
!   6/98 Created from projthbk3s
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
!  Julian Gale, NRI, Curtin University, June 2005
!
  use current
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)        :: nthbk
  real(dp)           :: d33s(108,*)
  real(dp)           :: d33rs(108,*)
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
  real(dp)           :: dfed2r
!
!  Loop over three body k atoms
!
  do ii = 1,nthbk
!
!  Loop over strain indices
!
    do kkk = 1,nstrains
      k = nstrptr(kkk)
      kk = 9*(k - 1)
!******************************************************************
!  Calculate i-k component - no assumption that d33 is symmetric  *
!******************************************************************
!
!  Calculate difference between phased on-diagonal elements and pure real
!
      dfed1r = dt11*(d33rs(1+kk,ii) - d33s(1+kk,ii)) + &
               dt21*(d33rs(2+kk,ii) - d33s(2+kk,ii)) + &
               dt31*(d33rs(3+kk,ii) - d33s(3+kk,ii)) + &
               dt21*(d33rs(4+kk,ii) - d33s(4+kk,ii)) + &
               dt22*(d33rs(5+kk,ii) - d33s(5+kk,ii)) + &
               dt32*(d33rs(6+kk,ii) - d33s(6+kk,ii)) + &
               dt31*(d33rs(7+kk,ii) - d33s(7+kk,ii)) + &
               dt32*(d33rs(8+kk,ii) - d33s(8+kk,ii)) + &
               dt33*(d33rs(9+kk,ii) - d33s(9+kk,ii))
!******************************************************************
!  Calculate j - k component  -  no assumption that d33 is symmetric  *
!******************************************************************
!
!  Real  -  real
!
      dfed2r = dt11*(d33rs(55+kk,ii) - d33s(55+kk,ii)) + &
               dt21*(d33rs(56+kk,ii) - d33s(56+kk,ii)) + &
               dt31*(d33rs(57+kk,ii) - d33s(57+kk,ii)) + &
               dt21*(d33rs(58+kk,ii) - d33s(58+kk,ii)) + &
               dt22*(d33rs(59+kk,ii) - d33s(59+kk,ii)) + &
               dt32*(d33rs(60+kk,ii) - d33s(60+kk,ii)) + &
               dt31*(d33rs(61+kk,ii) - d33s(61+kk,ii)) + &
               dt32*(d33rs(62+kk,ii) - d33s(62+kk,ii)) + &
               dt33*(d33rs(63+kk,ii) - d33s(63+kk,ii))
!
!  Add on derivative contributions
!
      dfeds(kkk) = dfeds(kkk) + dfed1r + dfed2r
    enddo
  enddo
!
  return
  end
