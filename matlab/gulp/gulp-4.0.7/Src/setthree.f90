  subroutine setthree
!
!  Assign dummy cutoffs for three-body terms based on covalent
!  radii for bonded potentials. This is need for efficient
!  working in the location of region 2a atoms for defect
!  calculations.
!
!   5/01 SW3 potential excluded since cut-offs are also
!        parameters and must be input
!   6/09 Module name changed from three to m_three
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use element
  use m_three
  use molecule
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ni
  integer(i4) :: nj
  integer(i4) :: nk
  real(dp)    :: ri
  real(dp)    :: rj
  real(dp)    :: rk
  real(dp)    :: rtol2
!
  rtol2 = 1.6_dp*rtol
  do i = 1,nthb
    if (nthrty(i).ne.5) then
      if (mmtexc(i).eq.1) then
        ni = ntspec1(i)
        nj = ntspec2(i)
        nk = ntspec3(i)
        if (ni.gt.maxele) ni = ni - maxele
        if (nj.gt.maxele) nj = nj - maxele
        if (nk.gt.maxele) nk = nk - maxele
        ri = rcov(ni)
        rj = rcov(nj)
        rk = rcov(nk)
        thr1(i) = (ri+rj)*rtol2
        thr2(i) = (ri+rk)*rtol2
        if (nthrty(i).eq.3.or.nthrty(i).eq.4) then
!
!  Symmetric potentials
!
          thr3(i) = (rj+rk)*rtol2
        else
!
!  Asymmetric potentials
!
          thr3(i) = thr1(i)+thr2(i)
        endif
      endif
    endif
  enddo
!
  return
  end
