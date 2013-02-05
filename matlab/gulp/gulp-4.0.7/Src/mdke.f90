  subroutine mdke(velrsq,lfirststep)
!
!  Calculates the kinetic energy - easiest to have
!  this in a subroutine so that shells can be
!  handled easily.
!
!  For Gear, KE is calculated at current timestep
!  whereas for Verlet velocity is +/- half a time
!  step and therefore current velocities are
!  estimated by averaging old and new values.
!
!  ekin   = kinetic energy
!  velsq  = sum of squares of velocities 
!  velrsq = sum of squares of velocities for shells
!
!   7/97 Created 
!  10/04 Intent added & style updated
!   6/05 lfirststep flag added to handle the velocity calculation
!        at the first point of a verlet algorithm
!   7/05 ratiom made species specific
!  12/07 Unused variables removed
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
!  Julian Gale, NRI, Curtin University, December 2007
!
  use current
  use element, only : maxele
  use mdlogic, only : ladiabatic
  use moldyn
  use optimisation
  use shell
  use velocities
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out) :: velrsq
  logical,     intent(in)  :: lfirststep
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: isp
  real(dp)                 :: rm
  real(dp)                 :: velsqc
  real(dp)                 :: velsqi
  real(dp)                 :: velsqs
  real(dp)                 :: vrx
  real(dp)                 :: vry
  real(dp)                 :: vrz
  real(dp)                 :: vx
  real(dp)                 :: vy
  real(dp)                 :: vz
  real(dp)                 :: vxc
  real(dp)                 :: vyc
  real(dp)                 :: vzc
  real(dp)                 :: vxs
  real(dp)                 :: vys
  real(dp)                 :: vzs
!
!  Calculate Kinetic Energy factor, if we are doing shell model
!  MD calculate it from the movement of the centres of mass and,
!  additionally, calculate the kinetic energy factor for the
!  relative movement of cores and corresponding shells
!
  velsq = 0.0_dp
  velrsq = 0.0_dp
  if (nmdintegrator.ne.3.or.lfirststep) then
!****************************
!  Gear or Velocity Verlet  *
!****************************
    do i = 1,numat
      if (lopf(i).and..not.lfix(i).and.nat(i).le.maxele) then
!
!  Core/shell pair
!
        isp = ncsptr(i)
        if (isp.ne.0.and..not.ladiabatic) then
          vxc = velx(i)
          vyc = vely(i)
          vzc = velz(i)
          vxs = velx(isp)
          vys = vely(isp)
          vzs = velz(isp)
          rm = mass(i) + mass(isp)
          vrx = velx(i) - velx(isp)
          vry = vely(i) - vely(isp)
          vrz = velz(i) - velz(isp)
          velrsq = velrsq + (vrx*vrx + vry*vry + vrz*vrz)*ratiom(i)*ratiom(isp)*rm
          velsqc = vxc*vxc + vyc*vyc + vzc*vzc
          velsqs = vxs*vxs + vys*vys + vzs*vzs
          velsq = velsq + mass(i)*velsqc + mass(isp)*velsqs
        else
!
!  Cores only
!
          vx = abs(velx(i))
          vy = abs(vely(i))
          vz = abs(velz(i))
          velsqi = vx*vx + vy*vy + vz*vz
          rm = mass(i)
          velsq = velsq + velsqi*rm
        endif
      endif
    enddo
  else
!***********
!  Verlet  *
!***********
    do i = 1,numat
      if (lopf(i).and..not.lfix(i).and.nat(i).le.maxele) then
!
!  Core/shell pair
!
        isp = ncsptr(i)
        if (isp.ne.0.and..not.ladiabatic) then
          vxc = 0.5_dp*(velx(i)+x3(i))
          vyc = 0.5_dp*(vely(i)+y3(i))
          vzc = 0.5_dp*(velz(i)+z3(i))
          vxs = 0.5_dp*(velx(isp)+x3(isp))
          vys = 0.5_dp*(vely(isp)+y3(isp))
          vzs = 0.5_dp*(velz(isp)+z3(isp))
          rm = mass(i) + mass(isp)
          vrx = vxc - vxs
          vry = vyc - vys
          vrz = vzc - vzs
          velrsq = velrsq + (vrx*vrx+vry*vry+vrz*vrz)*ratiom(i)*ratiom(isp)*rm
          velsqc = vxc*vxc + vyc*vyc + vzc*vzc
          velsqs = vxs*vxs + vys*vys + vzs*vzs
          velsq = velsq + velsqc*mass(i) + velsqs*mass(isp)
        else
!
!  Cores only
!
          vx = 0.5_dp*(velx(i)+x3(i))
          vy = 0.5_dp*(vely(i)+y3(i))
          vz = 0.5_dp*(velz(i)+z3(i))
          rm = mass(i)
          velsqi = vx*vx + vy*vy + vz*vz
          velsq = velsq + velsqi*rm
        endif
      endif
    enddo
  endif
!
  ekin = 0.5_dp*velsq*refct
!
  return
  end
