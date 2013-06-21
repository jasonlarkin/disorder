  subroutine projthbk3s(nthbk,d33s,d33rs,d33is,dt11r, &
    dt21r,dt31r,dt12r,dt22r,dt32r,dt13r,dt23r,dt33r,dt11i, &
    dt21i,dt31i,dt12i,dt22i,dt32i,dt13i,dt23i,dt33i,dt11, &
    dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33,dfeds)
!
!  Calculates the free energy derivative contribution from the
!  third atom k for the i-j dynamical matrix block. Three
!  dimensional version for strain derivatives.
!
!   5/98 Created from projthbk3
!   7/05 Style updated
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
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)        :: nthbk
  real(dp)           :: d33is(108,*)
  real(dp)           :: d33s(108,*)
  real(dp)           :: d33rs(108,*)
  real(dp)           :: dfeds(*)
  real(dp)           :: dt11
  real(dp)           :: dt21
  real(dp)           :: dt31
  real(dp)           :: dt12
  real(dp)           :: dt22
  real(dp)           :: dt32
  real(dp)           :: dt13
  real(dp)           :: dt23
  real(dp)           :: dt33
  real(dp)           :: dt11i
  real(dp)           :: dt21i
  real(dp)           :: dt31i
  real(dp)           :: dt12i
  real(dp)           :: dt22i
  real(dp)           :: dt32i
  real(dp)           :: dt13i
  real(dp)           :: dt23i
  real(dp)           :: dt33i
  real(dp)           :: dt11r
  real(dp)           :: dt21r
  real(dp)           :: dt31r
  real(dp)           :: dt12r
  real(dp)           :: dt22r
  real(dp)           :: dt32r
  real(dp)           :: dt13r
  real(dp)           :: dt23r
  real(dp)           :: dt33r
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: k
  integer(i4)        :: kk
  integer(i4)        :: kkk
  real(dp)           :: dfed1
  real(dp)           :: dfed1i
  real(dp)           :: dfed1r
  real(dp)           :: dfed2
  real(dp)           :: dfed2i
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
      kk = 9*(k-1)
!******************************************************************
!  Calculate i-k component - no assumption that d33 is symmetric  *
!******************************************************************
!
!  Real - real
!
      dfed1r = dt11r*d33rs(1+kk,ii) + dt21r*d33rs(2+kk,ii) + &
               dt31r*d33rs(3+kk,ii) + dt12r*d33rs(4+kk,ii) + &
               dt22r*d33rs(5+kk,ii) + dt32r*d33rs(6+kk,ii) + &
               dt13r*d33rs(7+kk,ii) + dt23r*d33rs(8+kk,ii) + &
               dt33r*d33rs(9+kk,ii)
!
!  Complex - complex
!
      dfed1i = dt11i*d33is(1+kk,ii) + dt21i*d33is(2+kk,ii) + &
               dt31i*d33is(3+kk,ii) + dt12i*d33is(4+kk,ii) + &
               dt22i*d33is(5+kk,ii) + dt32i*d33is(6+kk,ii) + &
               dt13i*d33is(7+kk,ii) + dt23i*d33is(8+kk,ii) + &
               dt33i*d33is(9+kk,ii)
!
!  Real - real - on-diagonal
!
      dfed1 = dt11*d33s(1+kk,ii) + dt21*d33s(2+kk,ii) + &
              dt31*d33s(3+kk,ii) + dt12*d33s(4+kk,ii) + &
              dt22*d33s(5+kk,ii) + dt32*d33s(6+kk,ii) + &
              dt13*d33s(7+kk,ii) + dt23*d33s(8+kk,ii) + &
              dt33*d33s(9+kk,ii)
!******************************************************************
!  Calculate j-k component - no assumption that d33 is symmetric  *
!******************************************************************
!
!  Real - real
!
      dfed2r = dt11r*d33rs(55+kk,ii) + dt21r*d33rs(56+kk,ii) + &
               dt31r*d33rs(57+kk,ii) + dt12r*d33rs(58+kk,ii) + &
               dt22r*d33rs(59+kk,ii) + dt32r*d33rs(60+kk,ii) + &
               dt13r*d33rs(61+kk,ii) + dt23r*d33rs(62+kk,ii) + &
               dt33r*d33rs(63+kk,ii)
!
!  Complex - complex
!
      dfed2i = dt11i*d33is(55+kk,ii) + dt21i*d33is(56+kk,ii) + &
               dt31i*d33is(57+kk,ii) + dt12i*d33is(58+kk,ii) + &
               dt22i*d33is(59+kk,ii) + dt32i*d33is(60+kk,ii) + &
               dt13i*d33is(61+kk,ii) + dt23i*d33is(62+kk,ii) + &
               dt33i*d33is(63+kk,ii)
!
!  Real - real - off-diagonal
!
      dfed2 = dt11*d33s(55+kk,ii) + dt21*d33s(56+kk,ii) + &
              dt31*d33s(57+kk,ii) + dt12*d33s(58+kk,ii) + &
              dt22*d33s(59+kk,ii) + dt32*d33s(60+kk,ii) + &
              dt13*d33s(61+kk,ii) + dt23*d33s(62+kk,ii) + &
              dt33*d33s(63+kk,ii)
!
!  Add on derivative contributions
!
      dfeds(kkk) = dfeds(kkk) + dfed1r + dfed1i + dfed1
      dfeds(kkk) = dfeds(kkk) + dfed2r + dfed2i + dfed2
    enddo
  enddo
!
  return
  end
