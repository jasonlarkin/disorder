  subroutine mdlfvcorrect(lequilibration,lproduction)
!
!  Performs the corrector step of the Leap Frog Verlet algorithm
!
!   7/97 Created from mdcorrect
!   7/97 NPT ensemble added from Melchionna et al (Mol. Phys.
!        78, 533 (1993)
!   9/04 Flags now passed through for call to mdvelcor
!   7/05 lfirststep argument added to mdke call
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
  use derivatives
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
  logical,     intent(in)  :: lequilibration
  logical,     intent(in)  :: lproduction
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: icp
  integer(i4)              :: ii
  integer(i4)              :: isp
  real(dp)                 :: area
  real(dp),           save :: cfactor = 6.241460893d-3
  real(dp)                 :: cx
  real(dp)                 :: cy
  real(dp)                 :: cz
  real(dp)                 :: rcellmass
  real(dp)                 :: rm
  real(dp)                 :: rrmi
  real(dp)                 :: sfac2
  real(dp)                 :: velrsq
  real(dp)                 :: vol
  real(dp)                 :: volume
  real(dp)                 :: vx
  real(dp)                 :: vy
  real(dp)                 :: vz
  real(dp)                 :: xcom
  real(dp)                 :: ycom
  real(dp)                 :: zcom
!
  if (nensemble(ncf).eq.2.or.nensemble(ncf).eq.3) then
!**************
!  NVT / NPT  *
!**************
    sfac0 = sfac
!
!  Save initial coordinates and velocities using Gear acceleration storage
!
    do i = 1,numat
      x2(i) = xalat(i)
      y2(i) = yalat(i)
      z2(i) = zalat(i)
      x3(i) = velx(i)
      y3(i) = vely(i)
      z3(i) = velz(i)
    enddo
!
!  For NPT calculate centre of mass and store barostat variables
!
    if (nensemble(ncf).eq.3) then
      xcom = 0.0_dp
      ycom = 0.0_dp
      zcom = 0.0_dp
      do i = 1,numat
        xcom = xcom + mass(i)*xalat(i)
        ycom = ycom + mass(i)*yalat(i)
        zcom = zcom + mass(i)*zalat(i)
      enddo
      rcellmass = 1.0_dp/cellmass
      xcom = rcellmass*xcom
      ycom = rcellmass*ycom
      zcom = rcellmass*zcom
      do i = 1,nstrains
        c2(i) = velc(i)
      enddo
    endif
!
!  Estimate quantities for this timestep
!
!  KE is calculated at half timestep
!
    do i = 1,numat
      if (lopf(i).and..not.lfix(i)) then
        rrmi = - rmass(i)*stpsqh
        velx(i) = velx(i) + rrmi*xdrv(i)
        vely(i) = vely(i) + rrmi*ydrv(i)
        velz(i) = velz(i) + rrmi*zdrv(i)
      endif
    enddo
    call mdke(velrsq,.false.)
    if (nensemble(ncf).eq.3) then
!
!  Calculate kinetic energy contribution to pressure for NPT
!  and sum contributions together from other strains. Negative
!  sign comes from difference between force and gradient
!
      do i = 1,nstrains
        cdrv(i) = - strderv(i)
      enddo
      call mdkestrain(cdrv)
    endif
    do i = 1,numat
      if (lopf(i).and..not.lfix(i)) then
        rrmi = - rmass(i)*stpsqh
        velx(i) = velx(i) + rrmi*xdrv(i)
        vely(i) = vely(i) + rrmi*ydrv(i)
        velz(i) = velz(i) + rrmi*zdrv(i)
      endif
    enddo
!
!  Estimate change in barostat variable
!
    if (nensemble(ncf).eq.3) then
      if (ndim.eq.3) then
        vol = volume(rv)*cfactor
        velc(1) = c2(1) + psfctt*(cdrv(1) - press*vol)
        velc(2) = c2(2) + psfctt*(cdrv(2) - press*vol)
        velc(3) = c2(3) + psfctt*(cdrv(3) - press*vol)
        velc(4) = c2(4) + psfctt*cdrv(4)
        velc(5) = c2(5) + psfctt*cdrv(5)
        velc(6) = c2(6) + psfctt*cdrv(6)
      elseif (ndim.eq.2) then
        vol = area(rv)*cfactor
        velc(1) = c2(1) + psfctt*(cdrv(1) - press*vol)
        velc(2) = c2(2) + psfctt*(cdrv(2) - press*vol)
        velc(3) = c2(3) + psfctt*cdrv(3)
      elseif (ndim.eq.1) then
        vol = rv(1,1)*cfactor
        velc(1) = c2(1) + psfctt*(cdrv(1) - press*vol)
      endif
!
!  Store average barostat variables
!
      do i = 1,nstrains
        c3(i) = 0.25_dp*(velc(i) + c2(i))
      enddo
    endif
!
!  Estimate change in thermostat variable
!
    svel = qtemp(ncf)*((ekin/smdfctt) - 1.0_dp)
    sfac = sfac0 + svel
    sfac2 = 0.25_dp*(sfac0 + sfac)
!************************
!  Start of iterations  *
!************************
    do ii = 1,nmditer
      velsq = 0.0_dp
!
!  Advance with thermostat only
!
      if (nensemble(ncf).eq.2) then
        do i = 1,numat
          if (lopf(i).and..not.lfix(i)) then
            if (nat(i).le.maxele.or..not.ladiabatic) then
              rrmi = - 2.0_dp*rmass(i)*stpsqh
              velx(i) = x3(i) + rrmi*xdrv(i) - sfac2*(velx(i)+x3(i))
              vely(i) = y3(i) + rrmi*ydrv(i) - sfac2*(vely(i)+y3(i))
              velz(i) = z3(i) + rrmi*zdrv(i) - sfac2*(velz(i)+z3(i))
              xalat(i) = x2(i) + velx(i)
              yalat(i) = y2(i) + vely(i)
              zalat(i) = z2(i) + velz(i)
            endif
            if (nat(i).le.maxele) then
              isp = ncsptr(i)
              if (isp.ne.0.and..not.ladiabatic) then
!
!  Core/shell pair
!
                vx = ratiom(i)*(velx(i)+x3(i))+ratiom(isp)*(velx(isp)+x3(isp))
                vy = ratiom(i)*(vely(i)+y3(i))+ratiom(isp)*(vely(isp)+y3(isp))
                vz = ratiom(i)*(velz(i)+z3(i))+ratiom(isp)*(velz(isp)+z3(isp))
              else
!
!  Core only
!
                vx = (velx(i) + x3(i))
                vy = (vely(i) + y3(i))
                vz = (velz(i) + z3(i))
              endif
              velsq = velsq + 0.25_dp*mass(i)*(vx*vx + vy*vy + vz*vz)
            endif
          endif
        enddo
      else
!
!  Advance with thermostat and barostat
!
!  First re-initialise pressure tensor ready for accumulation
!
        do i = 1,nstrains
          cdrv(i) = - strderv(i)
        enddo
        do i = 1,numat
          if (lopf(i).and..not.lfix(i)) then
            if (nat(i).le.maxele.or..not.ladiabatic) then
!
!  Average velocities - half taken care of outside loop
!
              vx = (velx(i) + x3(i))
              vy = (vely(i) + y3(i))
              vz = (velz(i) + z3(i))
!
!  Advance velocities
!
              rrmi = - 2.0_dp*rmass(i)*stpsqh
              velx(i) = x3(i) + rrmi*xdrv(i) - (c3(1)+sfac2)*vx - c3(5)*vz - c3(6)*vy
              vely(i) = y3(i) + rrmi*ydrv(i) - (c3(2)+sfac2)*vy - c3(4)*vz - c3(6)*vx
              velz(i) = z3(i) + rrmi*zdrv(i) - (c3(3)+sfac2)*vz - c3(4)*vy - c3(5)*vx
!
!  Average shift of centre of mass - half taken care of outside of loop
!
              cx = (xalat(i) + x2(i)) - 2.0_dp*xcom
              cy = (yalat(i) + y2(i)) - 2.0_dp*ycom
              cz = (zalat(i) + z2(i)) - 2.0_dp*zcom
!
!  Advance coordinates
!
              xalat(i) = x2(i) + velx(i) + c3(1)*cx + c3(5)*cz + c3(6)*cy
              yalat(i) = y2(i) + vely(i) + c3(2)*cy + c3(4)*cz + c3(6)*cx
              zalat(i) = z2(i) + velz(i) + c3(3)*cz + c3(4)*cy + c3(5)*cx
            endif
!
!  Re-calculate KE and KE contribution to pressure
!
            if (nat(i).le.maxele) then
              isp = ncsptr(i)
              if (isp.ne.0.and..not.ladiabatic) then
!
!  Core/shell pair
!
                vx = ratiom(i)*(velx(i) + x3(i)) + ratiom(isp)*(velx(isp) + x3(isp))
                vy = ratiom(i)*(vely(i) + y3(i)) + ratiom(isp)*(vely(isp) + y3(isp))
                vz = ratiom(i)*(velz(i) + z3(i)) + ratiom(isp)*(velz(isp) + z3(isp))
                rm = mass(i) + mass(isp)
              else
!
!  Core only
!
                vx = (velx(i) + x3(i))
                vy = (vely(i) + y3(i))
                vz = (velz(i) + z3(i))
                rm = mass(i)
              endif
              rm = 0.25_dp*rm
              velsq = velsq + rm*(vx*vx + vy*vy + vz*vz)
              rm = rm*refct
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
      endif
      ekin = 0.5_dp*velsq*refct
!
!  New prediction of barostat parameters
!
      if (nensemble(ncf).eq.3) then
        if (ndim.eq.3) then
          velc(1) = c2(1) + psfctt*(cdrv(1) - press*vol)
          velc(2) = c2(2) + psfctt*(cdrv(2) - press*vol)
          velc(3) = c2(3) + psfctt*(cdrv(3) - press*vol)
          velc(4) = c2(4) + psfctt*cdrv(4)
          velc(5) = c2(5) + psfctt*cdrv(5)
          velc(6) = c2(6) + psfctt*cdrv(6)
        elseif (ndim.eq.2) then
          velc(1) = c2(1) + psfctt*(cdrv(1) - press*vol)
          velc(2) = c2(2) + psfctt*(cdrv(2) - press*vol)
          velc(3) = c2(3) + psfctt*cdrv(3)
        elseif (ndim.eq.1) then
          velc(1) = c2(1) + psfctt*(cdrv(1) - press*vol)
        endif
!
!  Store average barostat variables
!
        do i = 1,nstrains
          c3(i) = 0.25_dp*(velc(i) + c2(i))
        enddo
      endif
!
!  New prediction of thermostat parameter
!
      svel = qtemp(ncf)*((ekin/smdfctt) - 1.0_dp)
      sfac = sfac0 + svel
      sfac2 = 0.25_dp*(sfac0 + sfac)
    enddo
!**********************
!  End of iterations  *
!**********************
    if (nensemble(ncf).eq.3) then
!
!  Update cell parameters for change in strain
!
      if (ndim.eq.3) then
        call strain3D(velc,xcell)
      elseif (ndim.eq.2) then
        call strain2D(velc,xcell)
      elseif (ndim.eq.1) then
        call strain1D(velc,xcell)
      endif
!
!  Correct for any drift in total linear momentum
!
      call mdvelcor(.false.,lequilibration,lproduction)
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
          xalat(isp) = xalat(isp) + xalat(icp) - x2(icp)
          yalat(isp) = yalat(isp) + yalat(icp) - y2(icp)
          zalat(isp) = zalat(isp) + zalat(icp) - z2(icp)
        endif
      enddo
    endif
  else
!********
!  NVE  *
!********
    do i = 1,numat
      x3(i) = velx(i)
      y3(i) = vely(i)
      z3(i) = velz(i)
    enddo
    do i=1,numat
      if (lopf(i).and..not.lfix(i)) then
        if (nat(i).le.maxele.or..not.ladiabatic) then
          rrmi = - 2.0_dp*rmass(i)*stpsqh
          velx(i) = velx(i) + rrmi*xdrv(i)
          vely(i) = vely(i) + rrmi*ydrv(i)
          velz(i) = velz(i) + rrmi*zdrv(i)
          xalat(i) = xalat(i) + velx(i)
          yalat(i) = yalat(i) + vely(i)
          zalat(i) = zalat(i) + velz(i)
        endif
      endif
    enddo
    if (nshell.ne.0.and.ladiabatic) then
!
!  If the shell positions are to be optimised move them with the
!  cores here to reduce number of iterations in optimisation
!
      do i = 1,nshell
        isp = nshptr(i)
        icp = ncsptr(isp)
        if (.not.lfix(icp)) then
          xalat(isp) = xalat(isp) + velx(icp)
          yalat(isp) = yalat(isp) + vely(icp)
          zalat(isp) = zalat(isp) + velz(icp)
        endif
      enddo
    endif
  endif
!
  return
  end
