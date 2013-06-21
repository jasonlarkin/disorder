  subroutine initqmatrix
!
!  Routine sets up terms needed by qmatrixelement.
!
!   8/01 Created from genpot
!   5/02 1-D case set by call to setmaxcell1D
!   1/03 Modifications made for Wolf sum
!  12/05 Trapping of lewald = .false. added
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   4/08 reaxFFcutoffQ removed
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, April 2008
!
  use control,    only : lwolf
  use current
  use general,    only : cutw
  use kspace
  use qmedata
  use shell
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  real(dp)    :: rv2
!
  if (ndim.gt.0) then
!
!  Estimate upper limits to looping
!
    if (ndim.eq.3) then
      if (lwolf) then
        radmax = cutw
      elseif (lewald) then
        radmax = accf/seta
      else
        radmax = 0.0_dp
      endif
      do i = 1,3
        rv2 = rv(1,i)**2 + rv(2,i)**2 + rv(3,i)**2
        rv2 = sqrt(rv2)
        maxloop(i) = (radmax/rv2) + 2
      enddo
    elseif (ndim.eq.2) then
      if (lwolf) then
        radmax = cutw
      elseif (lewald) then
        radmax = accf/seta
      else
        radmax = 0.0_dp
      endif
      do i = 1,2
        rv2 = rv(1,i)**2 + rv(2,i)**2
        rv2 = sqrt(rv2)
        maxloop(i) = (radmax/rv2) + 2
      enddo
      maxloop(3) = 0
    elseif (ndim.eq.1) then
      if (lwolf) then
        radmax = cutw
        maxloop(1) = (radmax/rv(1,1)) + 1
      else
        call setmaxcell1D(maxloop(1))
        radmax = (maxloop(1) + 1)*rv(1,1)
      endif
      maxloop(2) = 0
      maxloop(3) = 0
    endif
    rmax2 = radmax*radmax
  endif
!
  return
  end
