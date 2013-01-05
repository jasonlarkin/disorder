  subroutine sumderv2(n,ldbsm)
!
!  Completes second derivative matrix : diagonal blocks = sum of the off diagonal blocks.
!  Called by both energy and defener.
!
!  ldbsm = flag for whether breathing shells are present in defect calc
!
!  11/02 Modified to add on on-diagonal elements
!   5/03 Modified for region 3
!   7/07 Diagonal blocks for fixed sites added
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
!  Julian Gale, NRI, Curtin University, July 2007
!
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: n
  logical,     intent(in) :: ldbsm
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: indi
  integer(i4)             :: indj
  integer(i4)             :: ix
  integer(i4)             :: iy
  integer(i4)             :: iz
  integer(i4)             :: j
  integer(i4)             :: jx
  integer(i4)             :: jy
  integer(i4)             :: jz
  integer(i4)             :: nn
!*************************
!  Sum over coordinates  *
!*************************
  if (ldbsm) then
    nn = n - 1
  else
    nn = n
  endif
  do i = 1,nn
    indi = 3*(i-1)
    ix = indi + 1
    iy = indi + 2
    iz = indi + 3
    derv2(ix,ix) = derv2d(ix)
    derv2(ix,iy) = 0.0_dp
    derv2(ix,iz) = 0.0_dp
    derv2(iy,ix) = 0.0_dp
    derv2(iy,iy) = derv2d(iy)
    derv2(iy,iz) = 0.0_dp
    derv2(iz,ix) = 0.0_dp
    derv2(iz,iy) = 0.0_dp
    derv2(iz,iz) = derv2d(iz)
    do j = 1,n
      if (i.ne.j) then
        if (ldbsm.and.j.eq.n) then
          indj = 4*(n-1)
        else
          indj = 3*(j-1)
        endif
        jx = indj + 1
        jy = indj + 2
        jz = indj + 3
        derv2(ix,ix) = derv2(ix,ix) - derv2(jx,ix)
        derv2(ix,iy) = derv2(ix,iy) - derv2(jy,ix)
        derv2(ix,iz) = derv2(ix,iz) - derv2(jz,ix)
        derv2(iy,ix) = derv2(iy,ix) - derv2(jx,iy)
        derv2(iy,iy) = derv2(iy,iy) - derv2(jy,iy)
        derv2(iy,iz) = derv2(iy,iz) - derv2(jz,iy)
        derv2(iz,ix) = derv2(iz,ix) - derv2(jx,iz)
        derv2(iz,iy) = derv2(iz,iy) - derv2(jy,iz)
        derv2(iz,iz) = derv2(iz,iz) - derv2(jz,iz)
      endif
    enddo
  enddo
!
  return
  end
