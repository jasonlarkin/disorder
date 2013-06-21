  subroutine qmatrix1D(xji,yji,zji,lgrad1,lgrad2,qme,dqme,d2qme)
!
!  This subroutine calculates the electrostatic potential
!  contribution due to the additional terms in the 1-D
!  electrostatic summations.
!
!   5/02 Created from real1D
!   5/02 hfunc call modified for third derivatives
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
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use control,      only : lnoreal
  use current
  use general,      only : nemorder
  use qmedata,      only : maxloop
  implicit none
!
!  Passed arguments
!
  real(dp), intent(in)    :: xji
  real(dp), intent(in)    :: yji
  real(dp), intent(in)    :: zji
  real(dp), intent(inout) :: qme
  real(dp), intent(inout) :: dqme(3)
  real(dp), intent(inout) :: d2qme(6)
  logical,  intent(in)    :: lgrad1
  logical,  intent(in)    :: lgrad2
!
!  Local variables
!
  real(dp)                :: d1
  real(dp)                :: dh1(3)
  real(dp)                :: dh2(3)
  real(dp)                :: dh1s
  real(dp)                :: dh2s
  real(dp)                :: d2h1(6)
  real(dp)                :: d2h2(6)
  real(dp)                :: d2h1m(3)
  real(dp)                :: d2h2m(3)
  real(dp)                :: d3h1(10)
  real(dp)                :: d3h1m(6)
  real(dp)                :: d2h1s
  real(dp)                :: d2h2s
  real(dp)                :: e1
  real(dp)                :: e2
  real(dp)                :: h1
  real(dp)                :: h2
  real(dp)                :: lna
  real(dp)                :: u
!
!  
!
!  If noreal specified, return
!
  if (lnoreal) then
    return
  endif
  u = (dble(maxloop(1))+0.5_dp)*a
  lna = log(a)
  call hfunc(u,+xji,1.0_dp,yji,zji,h1,dh1,d2h1,d3h1,lgrad1,lgrad2,.false.)
  call hfunc(u,-xji,-1.0_dp,yji,zji,h2,dh2,d2h2,d3h1,lgrad1,lgrad2,.false.)
  d1 = 1.0_dp/a
  qme = qme - (h1 + h2 - 2.0_dp*lna)*d1
  if (lgrad1) then
    dqme(1) = dqme(1) + d1*(dh1(1) + dh2(1))
    dqme(2) = dqme(2) + d1*(dh1(2) + dh2(2))
    dqme(3) = dqme(3) + d1*(dh1(3) + dh2(3))
  endif
  if (lgrad2) then
    d2qme(1) = d2qme(1) - d1*(d2h1(1)+d2h2(1))
    d2qme(2) = d2qme(2) - d1*(d2h1(6)+d2h2(6))
    d2qme(3) = d2qme(3) - d1*(d2h1(2)+d2h2(2))
    d2qme(4) = d2qme(4) - d1*(d2h1(5)+d2h2(5))
    d2qme(5) = d2qme(5) - d1*(d2h1(4)+d2h2(4))
    d2qme(6) = d2qme(6) - d1*(d2h1(3)+d2h2(3))
  endif
  call emfunc(nemorder,u,+xji,1.0_dp,yji,zji,a,e1,dh1,d2h1,dh1s, &
              d2h1s,d2h1m,d3h1,d3h1m,lgrad1,lgrad2,.false.)
  call emfunc(nemorder,u,-xji,-1.0_dp,yji,zji,a,e2,dh2,d2h2,dh2s, &
              d2h2s,d2h2m,d3h1,d3h1m,lgrad1,lgrad2,.false.)
  qme = qme + (e1 + e2)
  if (lgrad1) then
    dqme(1) = dqme(1) - (dh1(1) + dh2(1))
    dqme(2) = dqme(2) - (dh1(2) + dh2(2))
    dqme(3) = dqme(3) - (dh1(3) + dh2(3))
  endif
  if (lgrad2) then
    d2qme(1) = d2qme(1) + (d2h1(1)+d2h2(1))
    d2qme(2) = d2qme(2) + (d2h1(6)+d2h2(6))
    d2qme(3) = d2qme(3) + (d2h1(2)+d2h2(2))
    d2qme(4) = d2qme(4) + (d2h1(5)+d2h2(5))
    d2qme(5) = d2qme(5) + (d2h1(4)+d2h2(4))
    d2qme(6) = d2qme(6) + (d2h1(3)+d2h2(3))
  endif
!
  return
  end
