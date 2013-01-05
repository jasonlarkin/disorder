  subroutine sumderv2f(n,m,lsymderv2)
!
!  Completes second derivative matrix : diagonal blocks = sum of
!  the off diagonal blocks.
!  Called by both energy.
!  Special version called when lfreeze=.true. as second derivatives
!  are stored in compressed form.
!
!  11/02 Modified to add on on-diagonal elements
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
  use optimisation
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: m
  integer(i4), intent(in) :: n
  logical,     intent(in) :: lsymderv2
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: ii
  integer(i4)             :: ij
  integer(i4)             :: ix
  integer(i4)             :: iy
  integer(i4)             :: iz
  integer(i4)             :: ixi
  integer(i4)             :: iyi
  integer(i4)             :: izi
  integer(i4)             :: ixio
  integer(i4)             :: iyio
  integer(i4)             :: izio
  integer(i4)             :: j
  integer(i4)             :: jx
  integer(i4)             :: jy
  integer(i4)             :: jz
  logical                 :: lopi
  logical                 :: lopj
!*************************
!  Sum over coordinates  *
!*************************
  ix = - 2
  iy = - 1
  iz =   0
  ixi = 1
  iyi = 2
  izi = 3
  ixio = 1
  iyio = 2
  izio = 3
  do i = 1,m
    if (lsymderv2) then
      lopi = lopf(i)
      ij = nrel2(i)
    else
      ii = nrelat(i)
      lopi = lopf(ii)
      ij = i
    endif
    if (lopi) then
      ix = ix + 3
      iy = iy + 3
      iz = iz + 3
      if (lsymderv2) then
        ixi = ixio
        iyi = iyio
        izi = izio
        ixio = ixio + 3*neqv(i)
        iyio = iyio + 3*neqv(i)
        izio = izio + 3*neqv(i)
      else
        ixi = ix
        iyi = iy
        izi = iz
      endif
!
!  Set on-diagonal block to be negative of itself
!  as this is where contributions from all frozen
!  atoms are stored + saved on diagonal contributions.
!
      derv2(ixi,ix) = - derv2(ixi,ix) + derv2d(ix)
      derv2(ixi,iy) = - derv2(ixi,iy)
      derv2(ixi,iz) = - derv2(ixi,iz)
      derv2(iyi,ix) = - derv2(iyi,ix)
      derv2(iyi,iy) = - derv2(iyi,iy) + derv2d(iy)
      derv2(iyi,iz) = - derv2(iyi,iz)
      derv2(izi,ix) = - derv2(izi,ix)
      derv2(izi,iy) = - derv2(izi,iy)
      derv2(izi,iz) = - derv2(izi,iz) + derv2d(iz)
      jx = - 2
      jy = - 1
      jz =   0
      do j = 1,n
        lopj = lopf(nrelat(j))
        if (lopj) then
          jx = jx + 3
          jy = jy + 3
          jz = jz + 3
          if (ij.ne.j) then
            derv2(ixi,ix) = derv2(ixi,ix) - derv2(jx,ix)
            derv2(iyi,ix) = derv2(iyi,ix) - derv2(jy,ix)
            derv2(izi,ix) = derv2(izi,ix) - derv2(jz,ix)
            derv2(ixi,iy) = derv2(ixi,iy) - derv2(jx,iy)
            derv2(iyi,iy) = derv2(iyi,iy) - derv2(jy,iy)
            derv2(izi,iy) = derv2(izi,iy) - derv2(jz,iy)
            derv2(ixi,iz) = derv2(ixi,iz) - derv2(jx,iz)
            derv2(iyi,iz) = derv2(iyi,iz) - derv2(jy,iz)
            derv2(izi,iz) = derv2(izi,iz) - derv2(jz,iz)
          endif
        endif
      enddo
    endif
  enddo
!
  return
  end
