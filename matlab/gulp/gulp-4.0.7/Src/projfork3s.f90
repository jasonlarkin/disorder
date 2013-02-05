  subroutine projfork3s(nforkl,d34s,d34rs,d34is,dt11r, &
    dt21r,dt31r,dt12r,dt22r,dt32r,dt13r,dt23r,dt33r,dt11i, &
    dt21i,dt31i,dt12i,dt22i,dt32i,dt13i,dt23i,dt33i,dt11, &
    dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33,dfeds)
!
!  Calculates the free energy derivative contribution from the
!  third atom k to fourth atom l distance for the i-j dynamical 
!  matrix block. Three dimensional version for strain derivatives.
!
!   8/98 Created from projthbk3s
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
  integer(i4)        :: nforkl
  real(dp)           :: d34s(54,*)
  real(dp)           :: d34rs(54,*)
  real(dp)           :: d34is(54,*)
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
!  Real - real
!
      dfed1r = dt11r*d34rs(1+kk,ii) + dt21r*d34rs(2+kk,ii) + &
               dt31r*d34rs(3+kk,ii) + dt12r*d34rs(4+kk,ii) + &
               dt22r*d34rs(5+kk,ii) + dt32r*d34rs(6+kk,ii) + &
               dt13r*d34rs(7+kk,ii) + dt23r*d34rs(8+kk,ii) + &
               dt33r*d34rs(9+kk,ii)
!
!  Complex - complex
!
      dfed1i = dt11i*d34is(1+kk,ii) + dt21i*d34is(2+kk,ii) + &
               dt31i*d34is(3+kk,ii) + dt12i*d34is(4+kk,ii) + &
               dt22i*d34is(5+kk,ii) + dt32i*d34is(6+kk,ii) + &
               dt13i*d34is(7+kk,ii) + dt23i*d34is(8+kk,ii) + &
               dt33i*d34is(9+kk,ii)
!
!  Real - real - on-diagonal
!
      dfed1 = dt11*d34s(1+kk,ii) + dt21*d34s(2+kk,ii) + &
              dt31*d34s(3+kk,ii) + dt12*d34s(4+kk,ii) + &
              dt22*d34s(5+kk,ii) + dt32*d34s(6+kk,ii) + &
              dt13*d34s(7+kk,ii) + dt23*d34s(8+kk,ii) + &
              dt33*d34s(9+kk,ii)
!
!  Add on derivative contributions
!
      dfeds(kkk) = dfeds(kkk) + dfed1r + dfed1i + dfed1
    enddo
  enddo
!
  return
  end
