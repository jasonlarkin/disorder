  subroutine meamsymdensity(scrho,scrhofull)
!
!  Converts the MEAM densities for the asymmetric unit into
!  the full densities for the whole unit cell through using
!  the symmetry operators.
!
!   4/09 Created
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
!  Julian Gale, Curtin University, April 2009
!
  use current,     only : nasym, numat, nrel2, nrelat, nrotop
  use eam,         only : maxmeamcomponent
  use symmetry
  implicit none
!
!  Passed variables
!
  real(dp),   intent(in)  :: scrho(maxmeamcomponent,nasym)
  real(dp),   intent(out) :: scrhofull(maxmeamcomponent,numat)
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: ia
  integer(i4)      :: j
  integer(i4)      :: k
  integer(i4)      :: l
  integer(i4)      :: m
  integer(i4)      :: mv
  real(dp)         :: x(3)
  real(dp)         :: xf(3)
  real(dp)         :: x2(3,3)
  real(dp)         :: xf2(3,3)
  real(dp)         :: x3(3,3,3)
  real(dp)         :: xf3(3,3,3)
!
!  Loop over full cell atoms
!
  do i = 1,numat
!
!  Find atom in the asymmetric unit
!
    ia = nrelat(i)
!
    if (nrel2(ia).eq.i) then
!
!  If i is the atom in the asymmetric unit then just copy terms
!
      scrhofull(1:maxmeamcomponent,i) = scrho(1:maxmeamcomponent,ia)
    else
!
!  Apply symmetry operators to the density components of the asymmetric unit equivalent atom according to order
!
!  Find rotational symmetry operator
!
      mv = nrotop(i)
!
!  Spherical terms
!
      scrhofull(1,i)  = scrho(1,ia)
      scrhofull(11,i) = scrho(11,ia)
!
!  Order = 2 : x, y, z times spherical term
!
      x(1) = scrho(2,ia)
      x(2) = scrho(3,ia)
      x(3) = scrho(4,ia)
      do j = 1,3
        xf(j) = 0.0_dp
        do k = 1,3
          xf(j) = xf(j) + rop(j,k,mv)*x(k)
        enddo
      enddo
      scrhofull(2,i) = xf(1)
      scrhofull(3,i) = xf(2)
      scrhofull(4,i) = xf(3)
!
!  Order = 3 : alpha*beta times spherical term, where alpha/beta = x, y, z
!
      x2(1,1) = scrho(5,ia)
      x2(2,1) = scrho(6,ia)
      x2(3,1) = scrho(7,ia)
      x2(1,2) = scrho(6,ia)
      x2(2,2) = scrho(8,ia)
      x2(3,2) = scrho(9,ia)
      x2(1,3) = scrho(7,ia)
      x2(2,3) = scrho(9,ia)
      x2(3,3) = scrho(10,ia)
      do j = 1,3
        do k = 1,3
          xf2(k,j) = 0.0_dp
          do l = 1,3
            xf2(k,j) = xf2(k,j) + rop(k,l,mv)*x2(l,j)
          enddo
        enddo
      enddo
      do j = 1,3
        do k = 1,3
          x2(k,j) = 0.0_dp
          do l = 1,3
            x2(k,j) = x2(k,j) + rop(k,l,mv)*xf2(j,l)
          enddo
        enddo
      enddo
      scrhofull(5,i)  = x2(1,1)
      scrhofull(6,i)  = x2(2,1)
      scrhofull(7,i)  = x2(3,1)
      scrhofull(8,i)  = x2(2,2)
      scrhofull(9,i)  = x2(3,2)
      scrhofull(10,i) = x2(3,3)
!
!  Order = 4 : alpha*beta*gamma times spherical term, where alpha/beta/gamma = x, y, z
!
      x3(1,1,1) = scrho(12,ia)
      x3(2,1,1) = scrho(13,ia)
      x3(3,1,1) = scrho(14,ia)
      x3(1,2,1) = scrho(13,ia)
      x3(2,2,1) = scrho(15,ia)
      x3(3,2,1) = scrho(16,ia)
      x3(1,3,1) = scrho(14,ia)
      x3(2,3,1) = scrho(16,ia)
      x3(3,3,1) = scrho(17,ia)
      x3(1,1,2) = scrho(13,ia)
      x3(2,1,2) = scrho(15,ia)
      x3(3,1,2) = scrho(16,ia)
      x3(1,2,2) = scrho(15,ia)
      x3(2,2,2) = scrho(18,ia)
      x3(3,2,2) = scrho(19,ia)
      x3(1,3,2) = scrho(16,ia)
      x3(2,3,2) = scrho(19,ia)
      x3(3,3,2) = scrho(20,ia)
      x3(1,1,3) = scrho(14,ia)
      x3(2,1,3) = scrho(16,ia)
      x3(3,1,3) = scrho(17,ia)
      x3(1,2,3) = scrho(16,ia)
      x3(2,2,3) = scrho(19,ia)
      x3(3,2,3) = scrho(20,ia)
      x3(1,3,3) = scrho(17,ia)
      x3(2,3,3) = scrho(20,ia)
      x3(3,3,3) = scrho(21,ia)
!
      do j = 1,3
        do k = 1,3
          do l = 1,3
            xf3(l,k,j) = 0.0_dp
            do m = 1,3
              xf3(l,k,j) = xf3(l,k,j) + rop(l,m,mv)*x3(m,k,j)
            enddo
          enddo
        enddo
      enddo
      do j = 1,3
        do k = 1,3
          do l = 1,3
            x3(l,k,j) = 0.0_dp
            do m = 1,3
              x3(l,k,j) = x3(l,k,j) + rop(l,m,mv)*xf3(k,m,j)
            enddo
          enddo
        enddo
      enddo
      do j = 1,3
        do k = 1,3
          do l = 1,3
            xf3(l,k,j) = 0.0_dp
            do m = 1,3
              xf3(l,k,j) = xf3(l,k,j) + rop(l,m,mv)*x3(k,j,m)
            enddo
          enddo
        enddo
      enddo
!
      scrhofull(12,i) = xf3(1,1,1)
      scrhofull(13,i) = xf3(2,1,1)
      scrhofull(14,i) = xf3(3,1,1)
      scrhofull(13,i) = xf3(1,2,1)
      scrhofull(15,i) = xf3(2,2,1)
      scrhofull(16,i) = xf3(3,2,1)
      scrhofull(14,i) = xf3(1,3,1)
      scrhofull(16,i) = xf3(2,3,1)
      scrhofull(17,i) = xf3(3,3,1)
      scrhofull(13,i) = xf3(1,1,2)
      scrhofull(15,i) = xf3(2,1,2)
      scrhofull(16,i) = xf3(3,1,2)
      scrhofull(15,i) = xf3(1,2,2)
      scrhofull(18,i) = xf3(2,2,2)
      scrhofull(19,i) = xf3(3,2,2)
      scrhofull(16,i) = xf3(1,3,2)
      scrhofull(19,i) = xf3(2,3,2)
      scrhofull(20,i) = xf3(3,3,2)
      scrhofull(14,i) = xf3(1,1,3)
      scrhofull(16,i) = xf3(2,1,3)
      scrhofull(17,i) = xf3(3,1,3)
      scrhofull(16,i) = xf3(1,2,3)
      scrhofull(19,i) = xf3(2,2,3)
      scrhofull(20,i) = xf3(3,2,3)
      scrhofull(17,i) = xf3(1,3,3)
      scrhofull(20,i) = xf3(2,3,3)
      scrhofull(21,i) = xf3(3,3,3)
    endif
  enddo
!
  return
  end
