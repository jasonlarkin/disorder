  subroutine setfour
!
!  Assign dummy cutoffs for four-body terms based on covalent
!  radii for bonded potentials. This is need for efficient
!  working in the location of region 2a atoms for defect
!  calculations.
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
  use element
  use four
  use molecule
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ni
  integer(i4) :: nj
  integer(i4) :: nk
  integer(i4) :: nl
  real(dp)    :: ri
  real(dp)    :: rj
  real(dp)    :: rk
  real(dp)    :: rl
  real(dp)    :: rtol2
!
  rtol2 = 1.6_dp*rtol
  do i = 1,nfor
    if (mmfexc(i).eq.1) then
      ni = nfspec1(i)
      nj = nfspec2(i)
      nk = nfspec3(i)
      nl = nfspec4(i)
      if (ni.gt.maxele) ni = ni - maxele
      if (nj.gt.maxele) nj = nj - maxele
      if (nk.gt.maxele) nk = nk - maxele
      if (nl.gt.maxele) nl = nl - maxele
      ri = rcov(ni)
      rj = rcov(nj)
      rk = rcov(nk)
      rl = rcov(nl)
      for1(i) = (ri+rj)*rtol2
      for2(i) = (rj+rk)*rtol2
      for3(i) = (rk+rl)*rtol2
      for4(i) = 0.0_dp
    endif
  enddo
!
  return
  end
