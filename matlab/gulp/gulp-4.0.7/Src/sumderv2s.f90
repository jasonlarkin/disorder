  subroutine sumderv2s(n,m,ldefbsm,lbulk)
!
!  Completes second derivative matrix : diagonal blocks = sum of
!  the off diagonal blocks.
!  Called by both energy and defener.
!
!  lbulk = if .true. then call is from energy, else call from defener
!  ldefbsm = flag for whether breathing shells are present in defect calc
!
!   5/95 Modification to perform sums for bulk as well as defect calcs
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
  use defects
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: m
  integer(i4), intent(in) :: n
  logical,     intent(in) :: lbulk
  logical,     intent(in) :: ldefbsm
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: ii
  integer(i4)             :: indi
  integer(i4)             :: indii
  integer(i4)             :: indj
  integer(i4)             :: ix
  integer(i4)             :: iy
  integer(i4)             :: iz
  integer(i4)             :: ixi
  integer(i4)             :: iyi
  integer(i4)             :: izi
  integer(i4)             :: j
  integer(i4)             :: jx
  integer(i4)             :: jy
  integer(i4)             :: jz
  integer(i4)             :: nn
!
  if (ldefbsm) then
    nn = n - 1
  else
    nn = n
  endif
  do i = 1,m
    if (lbulk) then
      ii = nrel2(i)
    else
      ii = ndsptr(i)
    endif
    indi = 3*(i-1)
    ix = indi + 1
    iy = indi + 2
    iz = indi + 3
    indii = 3*(ii-1)
    ixi = indii + 1
    iyi = indii + 2
    izi = indii + 3
    derv2(ixi,ix) = derv2d(ix)
    derv2(ixi,iy) = 0.0_dp
    derv2(ixi,iz) = 0.0_dp
    derv2(iyi,ix) = 0.0_dp
    derv2(iyi,iy) = derv2d(iy)
    derv2(iyi,iz) = 0.0_dp
    derv2(izi,ix) = 0.0_dp
    derv2(izi,iy) = 0.0_dp
    derv2(izi,iz) = derv2d(iz)
    do j = 1,nn
      if (ii.ne.j) then
        if (ldefbsm.and.j.eq.n) then
          indj = 4*(n-1)
        else
          indj = 3*(j-1)
        endif
        jx = indj + 1
        jy = indj + 2
        jz = indj + 3
        derv2(ixi,ix) = derv2(ixi,ix) - derv2(jx,ix)
        derv2(ixi,iy) = derv2(ixi,iy) - derv2(jy,ix)
        derv2(ixi,iz) = derv2(ixi,iz) - derv2(jz,ix)
        derv2(iyi,ix) = derv2(iyi,ix) - derv2(jx,iy)
        derv2(iyi,iy) = derv2(iyi,iy) - derv2(jy,iy)
        derv2(iyi,iz) = derv2(iyi,iz) - derv2(jz,iy)
        derv2(izi,ix) = derv2(izi,ix) - derv2(jx,iz)
        derv2(izi,iy) = derv2(izi,iy) - derv2(jy,iz)
        derv2(izi,iz) = derv2(izi,iz) - derv2(jz,iz)
      endif
    enddo
  enddo
!
  return
  end
