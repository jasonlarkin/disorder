  subroutine mdkestrain(cdrv)
!
!  Calculates the kinetic energy contribution to the pressure
!  
!  cdrv   = contains the lower half triangular components
!           of KE contribution to pressure. Note this array
!           must be initialised prior to call.
!
!   7/97 Created 
!   2/01 Modified for 1-D and 2-D
!   4/04 cdrv made an argument to routine
!   7/05 ratiom replaced
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
!  Julian Gale, NRI, Curtin University, July 2005.
!
  use current
  use element, only : maxele
  use mdlogic, only : ladiabatic
  use moldyn,  only : refct, lfix
  use optimisation
  use shell
  use velocities
  implicit none
!
!  Passed variables
!
  real(dp)          :: cdrv(6)
!
!  Local variables
!
  integer(i4)       :: i
  integer(i4)       :: isp
  real(dp)          :: rm
  real(dp)          :: rmc
  real(dp)          :: rms
  real(dp)          :: vx
  real(dp)          :: vy
  real(dp)          :: vz
  real(dp)          :: vxc
  real(dp)          :: vyc
  real(dp)          :: vzc
  real(dp)          :: vxs
  real(dp)          :: vys
  real(dp)          :: vzs
!
!  Calculate Kinetic Energy pressure
!
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
        rmc = mass(i)*refct
        rms = mass(isp)*refct
        if (ndim.eq.3) then
          cdrv(1) = cdrv(1) + rmc*vxc*vxc + rms*vxs*vxs
          cdrv(2) = cdrv(2) + rmc*vyc*vyc + rms*vys*vys
          cdrv(3) = cdrv(3) + rmc*vzc*vzc + rms*vzs*vzs
          cdrv(4) = cdrv(4) + rmc*vyc*vzc + rms*vys*vzs
          cdrv(5) = cdrv(5) + rmc*vxc*vzc + rms*vxs*vzs
          cdrv(6) = cdrv(6) + rmc*vxc*vyc + rms*vxs*vys
        elseif (ndim.eq.2) then
          cdrv(1) = cdrv(1) + rmc*vxc*vxc + rms*vxs*vxs
          cdrv(2) = cdrv(2) + rmc*vyc*vyc + rms*vys*vys
          cdrv(3) = cdrv(3) + rmc*vxc*vyc + rms*vxs*vys
        elseif (ndim.eq.1) then
          cdrv(1) = cdrv(1) + rmc*vxc*vxc + rms*vxs*vxs
        endif
      else
!
!  Cores only
!
        vx = velx(i)
        vy = vely(i)
        vz = velz(i)
        rm = mass(i)*refct
        if (ndim.eq.3) then
          cdrv(1) = cdrv(1) + rm*vx*vx
          cdrv(2) = cdrv(2) + rm*vy*vy
          cdrv(3) = cdrv(3) + rm*vz*vz
          cdrv(4) = cdrv(4) + rm*vy*vz
          cdrv(5) = cdrv(5) + rm*vx*vz
          cdrv(6) = cdrv(6) + rm*vx*vy
        elseif (ndim.eq.2) then
          cdrv(1) = cdrv(1) + rm*vx*vx
          cdrv(2) = cdrv(2) + rm*vy*vy
          cdrv(3) = cdrv(3) + rm*vx*vy
        elseif (ndim.eq.1) then
          cdrv(1) = cdrv(1) + rm*vx*vx
        endif
      endif
    endif
  enddo
!
  return
  end
