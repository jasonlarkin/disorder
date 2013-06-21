  subroutine mdpredict(lprod)
!
!  Performs the predictor step of the Gear 5th order algorithm
!  or of the velocity Verlet algorithm.
!
!   2/97 velocity Verlet added (JRH)
!   7/97 Gear and Verlet made consistent for shells
!  10/02 Single MD bond constraint added for velocity Verlet
!   3/08 Paolo's new integrator added
!   1/09 Name of new integrator changed to stochastic
!   7/11 Linear and angular momentum removed for stocastic integrator
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, July 2011.
!
  use control
  use current
  use derivatives
  use mdlogic,      only : ladiabatic
  use moldyn
  use m_pr,         only : lpr_thermo
  use optimisation
  use shell
  use velocities
  implicit none
!
!  Passed variables
!
  logical,     intent(in) :: lprod
!
!  Local variables
!
  integer(i4)  :: i
  integer(i4)  :: icp
  integer(i4)  :: isp
  integer(i4)  :: n1
  integer(i4)  :: n2
  real(dp)     :: cforce
  real(dp)     :: d21
  real(dp)     :: m1
  real(dp)     :: m2
  real(dp)     :: ra
  real(dp)     :: rb
  real(dp)     :: rc
  real(dp)     :: rnew(3)
  real(dp)     :: rold(3)
  real(dp)     :: rnewdotnew
  real(dp)     :: rnewdotold
  real(dp)     :: rolddotold
!
!  Predict positions using time derivatives
!
  if (nmdintegrator.eq.1) then
!*******************
!  Gear 5th order  *
!*******************
    if (nshell.ne.0.and.ladiabatic) then
!
!  If the shell positions are to be optimised move them with the
!  cores here to reduce number of iterations in optimisation
!
      do i = 1,nshell
        isp = nshptr(i)
        icp = ncsptr(isp)
        if (.not.lfix(icp)) then
          xalat(isp) = xalat(isp) + velx(icp) + x2(icp) + x3(icp) + x4(icp) + x5(icp)
          yalat(isp) = yalat(isp) + vely(icp) + y2(icp) + y3(icp) + y4(icp) + y5(icp)
          zalat(isp) = zalat(isp) + velz(icp) + z2(icp) + z3(icp) + z4(icp) + z5(icp)
        endif
      enddo
    endif
    do i = 1,numat
      if (lopf(i).and..not.lfix(i)) then
        xalat(i) = xalat(i) + velx(i) + x2(i) + x3(i) + x4(i) + x5(i)
        yalat(i) = yalat(i) + vely(i) + y2(i) + y3(i) + y4(i) + y5(i)
        zalat(i) = zalat(i) + velz(i) + z2(i) + z3(i) + z4(i) + z5(i)
!
        velx(i) = velx(i) + 2.0_dp*x2(i) + 3.0_dp*x3(i) + 4.0_dp*x4(i) + 5.0_dp*x5(i)
        vely(i) = vely(i) + 2.0_dp*y2(i) + 3.0_dp*y3(i) + 4.0_dp*y4(i) + 5.0_dp*y5(i)
        velz(i) = velz(i) + 2.0_dp*z2(i) + 3.0_dp*z3(i) + 4.0_dp*z4(i) + 5.0_dp*z5(i)
!
        x2(i) = x2(i) + 3.0_dp*x3(i) + 6.0_dp*x4(i) + 10.0_dp*x5(i)
        y2(i) = y2(i) + 3.0_dp*y3(i) + 6.0_dp*y4(i) + 10.0_dp*y5(i)
        z2(i) = z2(i) + 3.0_dp*z3(i) + 6.0_dp*z4(i) + 10.0_dp*z5(i)
!
        x3(i) = x3(i) + 4.0_dp*x4(i) + 10.0_dp*x5(i)
        y3(i) = y3(i) + 4.0_dp*y4(i) + 10.0_dp*y5(i)
        z3(i) = z3(i) + 4.0_dp*z4(i) + 10.0_dp*z5(i)
!
        x4(i) = x4(i) + 5.0_dp*x5(i)
        y4(i) = y4(i) + 5.0_dp*y5(i)
        z4(i) = z4(i) + 5.0_dp*z5(i)
      endif
    enddo
  elseif (nmdintegrator.eq.2) then
!********************
!  Velocity Verlet  *
!********************
    if (lmdconstrain(ncf)) then
!
!  Initialise variables for constrained dynamics
!
      n1 = nmdconstrainatom(1,ncf)
      n2 = nmdconstrainatom(2,ncf)
      m1 = mass(n1)
      m2 = mass(n2)
      d21 = nmdconstraindist(ncf)
!
!  If distance is constrained, then find old vector before move
!
      rold(1) = xalat(n1) - xalat(n2)
      rold(2) = yalat(n1) - yalat(n2)
      rold(3) = zalat(n1) - zalat(n2)
    endif
    if (nshell.ne.0.and.ladiabatic) then
!
!  If the shell positions are to be optimised move them with the
!  cores here to reduce number of iterations in optimisation
!
      do i = 1,nshell
        isp = nshptr(i)
        icp = ncsptr(isp)
        if (.not.lfix(icp)) then
          xalat(isp) = xalat(isp) + velx(icp) + x2(icp)
          yalat(isp) = yalat(isp) + vely(icp) + y2(icp)
          zalat(isp) = zalat(isp) + velz(icp) + z2(icp)
        endif
      enddo
    endif
    do i = 1,numat
      if (lopf(i).and..not.lfix(i)) then
        xalat(i) = xalat(i) + velx(i) + x2(i)
        yalat(i) = yalat(i) + vely(i) + y2(i)
        zalat(i) = zalat(i) + velz(i) + z2(i)
      endif
    enddo
!
!  Bond constraint
!
    if (lmdconstrain(ncf)) then
!
!  Calculate new vector between atoms
!
      rnew(1) = xalat(n1) - xalat(n2)
      rnew(2) = yalat(n1) - yalat(n2)
      rnew(3) = zalat(n1) - zalat(n2)
      rnewdotnew = rnew(1)*rnew(1) + rnew(2)*rnew(2) + rnew(3)*rnew(3)
      rnewdotold = rnew(1)*rold(1) + rnew(2)*rold(2) + rnew(3)*rold(3)
      rolddotold = rold(1)*rold(1) + rold(2)*rold(2) + rold(3)*rold(3)
!
!  Solve quadratic for constraint force x timestep**2 taking smallest root
!
      ra = rolddotold
      rb = 2.0_dp*rnewdotold
      rc = rnewdotnew - d21*d21
      lambdaR = - rb + sqrt(rb*rb - 4.0_dp*ra*rc)
      lambdaR = 0.5_dp*lambdaR/ra
      lambdaR = m1*m2*lambdaR/(m1 + m2)
!
!  Apply constraint force to atom positions
!
      cforce = lambdaR/m1
      xalat(n1) = xalat(n1) + cforce*rold(1)
      yalat(n1) = yalat(n1) + cforce*rold(2)
      zalat(n1) = zalat(n1) + cforce*rold(3)
      cforce = lambdaR/m2
      xalat(n2) = xalat(n2) - cforce*rold(1)
      yalat(n2) = yalat(n2) - cforce*rold(2)
      zalat(n2) = zalat(n2) - cforce*rold(3)
!
!  Apply distance constraint force to velocities
!
      cforce = lambdaR/m1
      velx(n1) = velx(n1) + cforce*rold(1)
      vely(n1) = vely(n1) + cforce*rold(2)
      velz(n1) = velz(n1) + cforce*rold(3)
      cforce = lambdaR/m2
      velx(n2) = velx(n2) - cforce*rold(1)
      vely(n2) = vely(n2) - cforce*rold(2)
      velz(n2) = velz(n2) - cforce*rold(3)
    endif
  elseif (nmdintegrator.eq.4) then
!**************************
!  Stochastic integrator  *
!**************************
!
!  Time reversible integrator obtained by a Trotter expansion:
!
!  exp(iL3 dt/2) * exp(iL2 dt/2) * exp(iL1) * exp(iL2 dt/2) * exp(iL3 dt/2)
!
!  iL3 -> thermostat
!  iL2 -> velocities
!  iL1 -> positions and cell
!
    if (lpr_thermo) call pr_thermostat
!
!  Update velocities -- dt/2
!
    call pr_update_velocities
!
!  Update positions -- dt
!
    call pr_update_positions
!
    if (.not.lprod) then
!
! Remove centre of mass motion
!
      call pr_remove_com
!
! Remove angular momentum
!
      if (ndim.eq.0) call pr_remove_torque
    endif

  endif
  return
  end
