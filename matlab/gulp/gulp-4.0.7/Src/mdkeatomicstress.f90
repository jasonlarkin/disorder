  subroutine mdkeatomicstress
!
!  Calculates the kinetic energy contribution to the atomic stress
!  
!   5/12 Created from mdkestrain
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012.
!
  use current
  use derivatives, only : atomicstress
  use element,     only : maxele
  use mdlogic,     only : ladiabatic
  use moldyn,      only : refct, lfix
  use optimisation
  use shell
  use velocities
  implicit none
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
          atomicstress(1,i) = atomicstress(1,i) + rmc*vxc*vxc + rms*vxs*vxs
          atomicstress(2,i) = atomicstress(2,i) + rmc*vyc*vyc + rms*vys*vys
          atomicstress(3,i) = atomicstress(3,i) + rmc*vzc*vzc + rms*vzs*vzs
          atomicstress(4,i) = atomicstress(4,i) + rmc*vyc*vzc + rms*vys*vzs
          atomicstress(5,i) = atomicstress(5,i) + rmc*vxc*vzc + rms*vxs*vzs
          atomicstress(6,i) = atomicstress(6,i) + rmc*vxc*vyc + rms*vxs*vys
        elseif (ndim.eq.2) then
          atomicstress(1,i) = atomicstress(1,i) + rmc*vxc*vxc + rms*vxs*vxs
          atomicstress(2,i) = atomicstress(2,i) + rmc*vyc*vyc + rms*vys*vys
          atomicstress(3,i) = atomicstress(3,i) + rmc*vxc*vyc + rms*vxs*vys
        elseif (ndim.eq.1) then
          atomicstress(1,i) = atomicstress(1,i) + rmc*vxc*vxc + rms*vxs*vxs
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
          atomicstress(1,i) = atomicstress(1,i) + rm*vx*vx
          atomicstress(2,i) = atomicstress(2,i) + rm*vy*vy
          atomicstress(3,i) = atomicstress(3,i) + rm*vz*vz
          atomicstress(4,i) = atomicstress(4,i) + rm*vy*vz
          atomicstress(5,i) = atomicstress(5,i) + rm*vx*vz
          atomicstress(6,i) = atomicstress(6,i) + rm*vx*vy
        elseif (ndim.eq.2) then
          atomicstress(1,i) = atomicstress(1,i) + rm*vx*vx
          atomicstress(2,i) = atomicstress(2,i) + rm*vy*vy
          atomicstress(3,i) = atomicstress(3,i) + rm*vx*vy
        elseif (ndim.eq.1) then
          atomicstress(1,i) = atomicstress(1,i) + rm*vx*vx
        endif
      endif
    endif
  enddo
!
  return
  end
