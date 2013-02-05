  subroutine mdscale(lprod)
!
!  Scale velocities for MD
!
!   2/97 Modifications from JRH added
!   8/97 Shell scaling separated as a subroutine
!   7/99 Kinetic energy corrected for scaling of velocities
!   7/05 Intent added
!   7/05 ratiom made species specific
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
  use control
  use current
  use element, only : maxele
  use iochannels
  use mdlogic, only : ladiabatic
  use moldyn
  use optimisation
  use shell
  use velocities
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lprod
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: isp
  integer(i4)                                  :: j
  integer(i4)                                  :: ni
  integer(i4), dimension(:), allocatable       :: nsum
  integer(i4)                                  :: status
  real(dp)                                     :: distto
  real(dp)                                     :: factor
  real(dp)                                     :: pcorrx
  real(dp)                                     :: pcorry
  real(dp)                                     :: pcorrz
  real(dp)                                     :: pnewx
  real(dp)                                     :: pnewy
  real(dp)                                     :: pnewz
  real(dp)                                     :: px
  real(dp)                                     :: py
  real(dp)                                     :: pz
  real(dp)                                     :: rm
  real(dp)                                     :: velrsq
  real(dp)                                     :: vold
  real(dp)                                     :: vsq
  real(dp)                                     :: vsqi
  real(dp),    dimension(:), allocatable       :: vsum
  real(dp)                                     :: vx
  real(dp)                                     :: vy
  real(dp)                                     :: vz
!
!  If equilibrating then ensure that no atom gets too hot
!  Excessive velocities can lead to disaster!
!
  if (.not.lprod) then
    allocate(nsum(maxele),stat=status)
    if (status/=0) call outofmemory('mdscale','nsum')
    allocate(vsum(maxele),stat=status)
    if (status/=0) call outofmemory('mdscale','vsum')
    do i = 1,maxele
      vsum(i) = 0.0_dp
      nsum(i) = 0
    enddo
    px = 0.0_dp
    py = 0.0_dp
    pz = 0.0_dp
    do i = 1,numat
      ni = nat(i)
      if (lopf(i).and..not.lfix(i).and.ni.le.maxele) then
        isp = ncsptr(i)
        if (isp.ne.0.and..not.ladiabatic) then
          vx = ratiom(i)*velx(i) + ratiom(isp)*velx(isp)
          vy = ratiom(i)*vely(i) + ratiom(isp)*vely(isp)
          vz = ratiom(i)*velz(i) + ratiom(isp)*velz(isp)
          rm = mass(i) + mass(isp)
        else
          vx = velx(i)
          vy = vely(i)
          vz = velz(i)
          rm = mass(i)
        endif
        px = px + rm*vx
        py = py + rm*vy
        pz = pz + rm*vz
        vsqi = vx*vx + vy*vy + vz*vz
        vsum(ni) = vsum(ni) + vsqi
        nsum(ni) = nsum(ni) + 1
      endif
    enddo
!
    do i = 1,numat
      ni = nat(i)
      if (lopf(i).and..not.lfix(i).and.ni.le.maxele) then
        if (nsum(ni).gt.1) then
          isp = ncsptr(i)
          if (isp.ne.0.and..not.ladiabatic) then
            vx = ratiom(i)*velx(i) + ratiom(isp)*velx(isp)
            vy = ratiom(i)*vely(i) + ratiom(isp)*vely(isp)
            vz = ratiom(i)*velz(i) + ratiom(isp)*velz(isp)
          else
            vx = velx(i)
            vy = vely(i)
            vz = velz(i)
          endif
          vsqi = vx*vx + vy*vy + vz*vz
          vsq = (vsum(ni) - vsqi)/dble(nsum(ni)-1)
          if (vsq.ge.0.0_dp.and.vsqi.gt.velmax*vsq) then
!
!  Velocity is 100 times greater than average so reduce
!
            factor = sqrt(vsq/vsqi)
            if (isp.ne.0.and..not.ladiabatic) then
              rm = mass(i) + mass(isp)
              vold = velx(i)
              velx(i) = factor*(ratiom(i)*velx(i)+ratiom(isp)*velx(isp)) + ratiom(isp)*(velx(i)-velx(isp))
              velx(isp) = velx(isp) + velx(i) - vold
              pnewx = (velx(i)-vold)*rm
              vold = vely(i)
              vely(i) = factor*(ratiom(i)*vely(i)+ratiom(isp)*vely(isp)) + ratiom(isp)*(vely(i)-vely(isp))
              vely(isp) = vely(isp) + vely(i) - vold
              pnewy = (velx(i)-vold)*rm
              vold = velz(i)
              velz(i) = factor*(ratiom(i)*velz(i)+ratiom(isp)*velz(isp)) + ratiom(isp)*(velz(i)-velz(isp))
              velz(isp) = velz(isp) + velz(i) - vold
              pnewz = (velz(i)-vold)*rm
            else
              if (factor.eq.0.0_dp) then
                pnewx = velx(i)*mass(i)
                pnewy = vely(i)*mass(i)
                pnewz = velz(i)*mass(i)
              endif
              velx(i) = velx(i)*factor
              vely(i) = vely(i)*factor
              velz(i) = velz(i)*factor
              if (factor.ne.0.0_dp) then
                factor = (1.0_dp-1.0_dp/factor)
                pnewx = velx(i)*factor*mass(i)
                pnewy = vely(i)*factor*mass(i)
                pnewz = velz(i)*factor*mass(i)
              endif
            endif
!
!  Correct total momentum by distributing an additional momentum equal to
!  the lost momentum of the scaled atom equally over all other atoms
!
            distto = 1.0_dp/dble(numat-nshell-1)
            pcorrx = (px - pnewx)*distto
            pcorry = (py - pnewy)*distto
            pcorrz = (pz - pnewz)*distto
            do j = 1,numat
              if (j.ne.i.and.lopf(j).and..not.lfix(j).and.nat(j).le.maxele) then
                isp = ncsptr(j)
                if (isp.ne.0.and..not.ladiabatic) then
                  rm = mass(j) + mass(isp)
                  factor = 1.0_dp + pcorrx/(velx(j)*rm)
                  vold = velx(j)
                  velx(j) = factor*(ratiom(i)*velx(j)+ratiom(isp)*velx(isp)) + ratiom(isp)*(velx(j)-velx(isp))
                  velx(isp) = velx(isp) + velx(j) - vold
                  factor = 1.0_dp + pcorry/(vely(j)*rm)
                  vold = vely(j)
                  vely(j) = factor*(ratiom(i)*vely(j)+ratiom(isp)*vely(isp)) + ratiom(isp)*(vely(j)-vely(isp))
                  vely(isp) = vely(isp) + vely(j) - vold
                  vold = velz(j)
                  factor = 1.0_dp + pcorrz/(velz(j)*rm)
                  velz(j) = factor*(ratiom(i)*velz(j)+ratiom(isp)*velz(isp)) + ratiom(isp)*(velz(j)-velz(isp))
                  velz(isp) = velz(isp) + velz(j) - vold
                else
                  velx(j) = velx(j) + pcorrx*rmass(j)
                  vely(j) = vely(j) + pcorry*rmass(j)
                  velz(j) = velz(j) + pcorrz*rmass(j)
                endif
              endif
            enddo
          endif
        endif
      endif
    enddo
!
!  Recalculate Kinetic Energy factor
!
    call mdke(velrsq,.true.)
!
    deallocate(vsum,stat=status)
    if (status/=0) call deallocate_error('mdscale','vsum')
    deallocate(nsum,stat=status)
    if (status/=0) call deallocate_error('mdscale','nsum')
  endif
!
!  Calculate temperature correction factor, scaling velocities
!  for shell model MD is a little bit more complicated. We
!  scale so that the centre-of-mass velocity is scaled by the
!  appropriate factor while the relative core-shell velocity
!  remains constant.
!
  factor = sqrt(velfctt/velsq)
!
!  First scale all velocities to correct overall KE
!
  do i = 1,numat
    velx(i) = velx(i)*factor
    vely(i) = vely(i)*factor
    velz(i) = velz(i)*factor
  enddo
!
!  Correct KE for scaling
!
  ekin = ekin*factor*factor
!
!  Now moderate internal core-shell temperature
!
  if (nshell.ne.0.and..not.ladiabatic) then
    call mdshscale
  endif
!
!  If NVT ensemble then re-initialise thermostat variables
!
  if (nensemble(ncf).ge.2) then
    sfac = 0.0_dp
    sumsfac = 0.0_dp
    if (nensemble(ncf).eq.3) then
!
!  If NPT ensemble then re-initialise pressure variables
!
      do i = 1,nstrains
        velc(i) = 0.0_dp
      enddo
    endif
  endif
!
  return
  end
