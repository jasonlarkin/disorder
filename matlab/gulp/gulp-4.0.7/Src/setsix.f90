  subroutine setsix
!
!  Assign dummy cutoffs for six-body terms based on covalent
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
!  Copyright Curtin University 2004
!
!  Julian Gale, Curtin University, November 2004
!
  use element
  use six
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
  integer(i4) :: nm
  integer(i4) :: nn
  real(dp)    :: ri
  real(dp)    :: rj
  real(dp)    :: rk
  real(dp)    :: rl
  real(dp)    :: rm
  real(dp)    :: rn
  real(dp)    :: rtol2
!
  rtol2 = 1.6_dp*rtol
  do i = 1,nsix
    if (mmsexc(i).eq.1) then
      ni = nsspec1(i)
      nj = nsspec2(i)
      nk = nsspec3(i)
      nl = nsspec4(i)
      nm = nsspec5(i)
      nn = nsspec6(i)
      if (ni.gt.maxele) ni = ni - maxele
      if (nj.gt.maxele) nj = nj - maxele
      if (nk.gt.maxele) nk = nk - maxele
      if (nl.gt.maxele) nl = nl - maxele
      if (nm.gt.maxele) nm = nm - maxele
      if (nn.gt.maxele) nn = nn - maxele
      ri = rcov(ni)
      rj = rcov(nj)
      rk = rcov(nk)
      rl = rcov(nl)
      rm = rcov(nm)
      rn = rcov(nn)
      six1(i) = (ri+rj)*rtol2
      six2(i) = (ri+rk)*rtol2
      six3(i) = (ri+rl)*rtol2
      six4(i) = (rj+rm)*rtol2
      six5(i) = (rj+rn)*rtol2
    endif
  enddo
  return
  end
