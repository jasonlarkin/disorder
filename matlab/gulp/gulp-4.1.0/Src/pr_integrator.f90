  subroutine pr_update_velocities
!
!  Update the velocities
!
  use current,     only : numat, mass, rmass, rv
  use derivatives, only : xdrv, ydrv, zdrv, virial
  use m_pr
  use moldyn,      only : refct, lfix
  use velocities,  only : velx, vely, velz

  implicit none
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: j
  integer(i4)      :: k
  real(dp),   save :: cfactor = 6.241460893d-3
  real(dp)         :: pp
  real(dp)         :: pf
  real(dp)         :: ff
  real(dp)         :: pevol
  real(dp)         :: pfm(3,3)
  real(dp)         :: ppm(3,3)
  real(dp)         :: ffm(3,3)
  real(dp)         :: fpm(3,3)
  real(dp)         :: wmtx(3,3)
  real(dp)         :: vol
  real(dp)         :: volume
!
!  Get volume of system
!
  vol = volume(rv)
!
!  Hoover implementation of the isotropic barostat
!
  if (lpr_baro_iso) then
    pp = 0.0_dp
    pf = 0.0_dp
    ff = 0.0_dp
    do i = 1,numat
      if (.not.lfix(i)) then
        pp = pp + mass(i)*(velx(i)**2 + vely(i)**2 + velz(i)**2)
        pf = pf - xdrv(i)*velx(i) - ydrv(i)*vely(i) - zdrv(i)*velz(i)
        ff = ff + (xdrv(i)**2 + ydrv(i)**2 + zdrv(i)**2)*rmass(i)
      endif
    enddo
!
!  Do unit conversions so that pevol ends up in eV.ps
!
    pp = refct*pp
    pf = pf/dt
    ff = ff*dt/refct
!
    pevol = - virial*dth - 3.0_dp*cfactor*vol*pr_target_press*dth + pp*dth + pf*dth**2 + ff*dth**3/3.0_dp
    p_iso = p_iso + pevol
!
!  Hoover implementation of the flexible cell barostat
!
  elseif (lpr_baro_flx) then
    ppm = 0.0_dp
    pfm = 0.0_dp
    ffm = 0.0_dp
    do k = 1,numat
      if (.not.lfix(k)) then
        ppm(1,1) = ppm(1,1) + velx(k)*velx(k)*mass(k)
        ppm(2,1) = ppm(2,1) + vely(k)*velx(k)*mass(k)
        ppm(3,1) = ppm(3,1) + velz(k)*velx(k)*mass(k)
        ppm(1,2) = ppm(1,2) + velx(k)*vely(k)*mass(k)
        ppm(2,2) = ppm(2,2) + vely(k)*vely(k)*mass(k)
        ppm(3,2) = ppm(3,2) + velz(k)*vely(k)*mass(k)
        ppm(1,3) = ppm(1,3) + velx(k)*velz(k)*mass(k)
        ppm(2,3) = ppm(2,3) + vely(k)*velz(k)*mass(k)
        ppm(3,3) = ppm(3,3) + velz(k)*velz(k)*mass(k)
!
        pfm(1,1) = pfm(1,1) - velx(k)*xdrv(k)
        pfm(2,1) = pfm(2,1) - vely(k)*xdrv(k)
        pfm(3,1) = pfm(3,1) - velz(k)*xdrv(k)
        pfm(1,2) = pfm(1,2) - velx(k)*ydrv(k)
        pfm(2,2) = pfm(2,2) - vely(k)*ydrv(k)
        pfm(3,2) = pfm(3,2) - velz(k)*ydrv(k)
        pfm(1,3) = pfm(1,3) - velx(k)*zdrv(k)
        pfm(2,3) = pfm(2,3) - vely(k)*zdrv(k)
        pfm(3,3) = pfm(3,3) - velz(k)*zdrv(k)
!
        ffm(1,1) = ffm(1,1) + xdrv(k)*xdrv(k)*rmass(k)
        ffm(2,1) = ffm(2,1) + ydrv(k)*xdrv(k)*rmass(k)
        ffm(3,1) = ffm(3,1) + zdrv(k)*xdrv(k)*rmass(k)
        ffm(1,2) = ffm(1,2) + xdrv(k)*ydrv(k)*rmass(k)
        ffm(2,2) = ffm(2,2) + ydrv(k)*ydrv(k)*rmass(k)
        ffm(3,2) = ffm(3,2) + zdrv(k)*ydrv(k)*rmass(k)
        ffm(1,3) = ffm(1,3) + xdrv(k)*zdrv(k)*rmass(k)
        ffm(2,3) = ffm(2,3) + ydrv(k)*zdrv(k)*rmass(k)
        ffm(3,3) = ffm(3,3) + zdrv(k)*zdrv(k)*rmass(k)
      endif
    enddo
    fpm = transpose(pfm)
!
!  Do unit conversions so that wmtx & p_flx end up in eV.ps
!
    ppm = refct*ppm
    pfm = pfm/dt
    ffm = ffm*dt/refct
!
    wmtx = ppm*dth + 0.5_dp*(fpm + pfm)*dth**2 + ffm*dth**3/3.0_dp
    p_flx = p_flx - virial_m*dth - cfactor*vol*pr_target_press_tens*dth + wmtx
!
!  Keep the angles fixed
!
    if (lpr_baro_ort) then
      do i = 1,2
        do j = i+1,3
          p_flx(j,i) = 0.0_dp
          p_flx(i,j) = 0.0_dp
        enddo
      enddo
    endif
  endif

  do i = 1,numat
    if (.not.lfix(i)) then
      velx(i) = velx(i) - 0.5_dp*xdrv(i)*rmass(i)/refct
      vely(i) = vely(i) - 0.5_dp*ydrv(i)*rmass(i)/refct
      velz(i) = velz(i) - 0.5_dp*zdrv(i)*rmass(i)/refct
    endif
  enddo
  return

  end subroutine pr_update_velocities

  subroutine pr_update_positions
!
!  Update the positions
!
  use current,    only : numat, xalat, yalat, zalat, rv, ndim
  use current,    only : a, b, c, alpha, beta, gamma
  use m_pr
  use moldyn,     only : xcell, lfix
  use velocities, only : velx, vely, velz

  implicit none

  integer(i4)      :: i
  real(dp)         :: fact
  real(dp)         :: m1(3,3)
  real(dp)         :: m2(3,3)
  real(dp)         :: m2_inv(3,3)
  real(dp)         :: p_flx_inv(3,3)
  real(dp)         :: scale1_m(3,3)
  real(dp)         :: scale2_m(3,3)
  real(dp)         :: wmtx1(3,3), wmtx2(3,3)
  real(dp), dimension (3,3) :: idm=reshape((/1.d0,0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,1.d0/),shape=(/3,3/))
  real(dp)         :: rtmp
  real(dp)         :: scale1
  real(dp)         :: scale2
  real(dp)         :: vol
!
!  Hoover implementation of the isotropic barostat
!
  if (lpr_baro_iso) then
    scale1 = exp(p_iso*dt/bmass_iso)
    scale2 = exp(-p_iso*dt/bmass_iso)
    rv = rv*scale1
    call get_hinv(rv,hinv,vol)
! NB Division by dt is to correct the units of fact to be Ang.timestep
    fact = bmass_iso/p_iso*0.5_dp*(scale1-scale2)/dt
    do i = 1,numat
      if (.not.lfix(i)) then
        xalat(i) = xalat(i)*scale1 + velx(i)*fact
        yalat(i) = yalat(i)*scale1 + vely(i)*fact
        zalat(i) = zalat(i)*scale1 + velz(i)*fact
        velx(i)  = velx(i)*scale2
        vely(i)  = vely(i)*scale2
        velz(i)  = velz(i)*scale2
      endif
    enddo
!
!  Hoover implementation of the flexible cell barostat
!
  elseif (lpr_baro_flx) then
!
!  Crank-Nicholson 5th order approximation
!
    wmtx1 = 0.5_dp*dt*p_flx/bmass_flx
    m1 = idm + wmtx1
    m2 = idm - wmtx1
    wmtx2 = wmtx1
    fact = 1.0_dp
    do i = 2,5
      fact = fact*dble(i)
      wmtx2 = matmul(wmtx1,wmtx2)
      m1 = m1 +         wmtx2/fact
      m2 = m2 + (-1)**i*wmtx2/fact
    enddo
    call get_hinv(m2,m2_inv,rtmp)
    scale1_m = matmul(m2_inv,m1)
    call get_hinv(scale1_m,scale2_m,rtmp)
!
!  Cell evolution
!
    wmtx1 = matmul(scale1_m,rv)
    rv = wmtx1
    call get_hinv(rv,hinv,vol)
!
!  Positions evolution
!
    call get_hinv(p_flx,p_flx_inv,rtmp)
! NB The division by dt is to correct the units to timestep
    wmtx1 = 0.5_dp*bmass_flx*matmul(p_flx_inv,scale1_m-scale2_m)/dt
    do i = 1,numat
      if (.not.lfix(i)) then
        xalat(i)=   wmtx1(1,1)*velx(i)  +    wmtx1(1,2)*vely(i)  +    wmtx1(1,3)*velz(i) + &
                 scale1_m(1,1)*xalat(i) + scale1_m(1,2)*yalat(i) + scale1_m(1,3)*zalat(i)
        yalat(i)=   wmtx1(2,1)*velx(i)  +    wmtx1(2,2)*vely(i)  +    wmtx1(2,3)*velz(i) + &
                 scale1_m(2,1)*xalat(i) + scale1_m(2,2)*yalat(i) + scale1_m(2,3)*zalat(i)
        zalat(i)=   wmtx1(3,1)*velx(i)  +    wmtx1(3,2)*vely(i)  +    wmtx1(3,3)*velz(i) + &
                 scale1_m(3,1)*xalat(i) + scale1_m(3,2)*yalat(i) + scale1_m(3,3)*zalat(i)
      endif
    enddo
!
!  Velocities evolution
!
    do i = 1,numat
      if (.not.lfix(i)) then
        velx(i) = scale2_m(1,1)*velx(i) + scale2_m(1,2)*vely(i) + scale2_m(1,3)*velz(i)
        vely(i) = scale2_m(2,1)*velx(i) + scale2_m(2,2)*vely(i) + scale2_m(2,3)*velz(i)
        velz(i) = scale2_m(3,1)*velx(i) + scale2_m(3,2)*vely(i) + scale2_m(3,3)*velz(i)
      endif
    enddo
  else
!
!  No barostat
!
    do i = 1,numat
      if (.not.lfix(i)) then
        xalat(i) = xalat(i) + velx(i)
        yalat(i) = yalat(i) + vely(i)
        zalat(i) = zalat(i) + velz(i)
      endif
    enddo
  endif
  if (ndim.eq.3) then
!
!  Return rv to xcell for MD
!
    xcell(1) = rv(1,1)
    xcell(2) = rv(2,1)
    xcell(3) = rv(3,1)
    xcell(4) = rv(1,2)
    xcell(5) = rv(2,2)
    xcell(6) = rv(3,2)
    xcell(7) = rv(1,3)
    xcell(8) = rv(2,3)
    xcell(9) = rv(3,3)
    call uncell3D(rv,a,b,c,alpha,beta,gamma)
  endif

  return

  end subroutine pr_update_positions

  subroutine pr_thermostat
!
!  Thermostat
!
  use current,          only : numat, nspecptr
  use m_pr
  use moldyn,           only : lfix
  use pr_random_module, only : gauss_rand
  use species,          only : nspec, numofspec
  use velocities,       only : velx, vely, velz

  implicit none

  integer(i4)        :: i
  integer(i4)        :: ik
  integer(i4)        :: nn
  real(dp)           :: c1
  real(dp)           :: c2
  real(dp)           :: ekin0
  real(dp)           :: fscale
  real(dp)           :: fscale1(20)
  real(dp)           :: fscale2
  real(dp)           :: r1
  real(dp)           :: r2
  real(dp)           :: wmtx(3,3)
  real(dp), external :: sumnoises
!
!  Kinetic energy update
!
  call compute_ekin()

  if (taut>1.d-6) then
    c1 = exp(-dth/taut)
  else
    c1 = 0.0_dp
  endif
!
!  Thermostat on the atoms
!
  if (lpr_globalt) then
    if (lpr_baro_iso) then
      pr_ekinbaro = 0.5_dp*p_iso**2/bmass_iso
    elseif (lpr_baro_flx) then
      pr_ekinbaro = 0.0_dp
      wmtx = matmul(transpose(p_flx),p_flx)
      do i = 1,3
        pr_ekinbaro = pr_ekinbaro + wmtx(i,i)/bmass_flx
      enddo
      pr_ekinbaro = 0.5_dp*pr_ekinbaro
    else
      pr_ekinbaro = 0.0_dp
    endif

    ekin0 = pr_ekin + pr_ekinbaro
    c2 = (1.0_dp - c1)*pr_ekintarget_atm/ekin0
    r1 = gauss_rand()
    r2 = sumnoises(ndof_atm+ndof_baro-1_i4)
    fscale2 = c1 + c2*(r1**2 + r2) + 2.0_dp*r1*sqrt(c1*c2)
!
!  Rescale the velocities
!
    fscale = sqrt(fscale2)
    do i = 1,numat
      if (.not.lfix(i)) then
        velx(i) = velx(i)*fscale
        vely(i) = vely(i)*fscale
        velz(i) = velz(i)*fscale
      endif
    enddo
    if (lpr_baro_iso) then
      p_iso = p_iso*fscale
    elseif (lpr_baro_flx) then
      p_flx = p_flx*fscale
    endif
    pr_cons = pr_cons + (1.0_dp - fscale2)*ekin0
    pr_ekin = pr_ekin*fscale2
    pr_ekinbaro = pr_ekinbaro*fscale2
  else
!
!  Apply a different thermostat for each species
!
    do ik = 1,nspec
      ekin0 = pekin(ik)
      nn = 3*numofspec(ik)
      c2 = (1.0_dp - c1)*pr_ekintarget_atm/ekin0
      r1 = gauss_rand()
      r2 = sumnoises(nn-1_i4)
      fscale2 = c1 + c2*(r1**2 + r2) + 2.0_dp*r1*sqrt(c1*c2)
      fscale1(ik) = sqrt(fscale2)
!
!  Integral of the kinetic energy for the conserved quantity
!
      pekin(ik) = pekin(ik)*fscale2
      pr_cons = pr_cons + (1.0_dp - fscale2)*ekin0
    enddo
    pr_ekin = sum(pekin)
!
!  Rescale the velocities
!
    do i = 1,numat
      if (.not.lfix(i)) then
        ik = nspecptr(i)
        velx(i) = velx(i)*fscale1(ik)
        vely(i) = vely(i)*fscale1(ik)
        velz(i) = velz(i)*fscale1(ik)
      endif
    enddo
!
!  Kinetic energy update and barostato momentum rescale
!
    if (lpr_baro_iso) then
      pr_ekinbaro = 0.5_dp*p_iso**2/bmass_iso
      ekin0 = pr_ekinbaro
      c2 = (1.0_dp - c1)*pr_ekintarget_baro/ekin0
      r1 = gauss_rand()
      fscale2 = c1 + c2*r1**2 + 2.0_dp*r1*sqrt(c1*c2)
      fscale = sqrt(fscale2)
!
!  Scale the barostat momentum
!
      p_iso = p_iso*fscale
!
!  Integral of the kinetic energy for the conserved quantity
!
      pr_cons = pr_cons + (1.0_dp - fscale2)*ekin0
! 
!  Kinetic energy update
!
      pr_ekinbaro = ekin0*fscale2
  
    elseif (lpr_baro_flx) then
      pr_ekinbaro = 0.0_dp
      wmtx = matmul(transpose(p_flx),p_flx)
      do i = 1,3
        pr_ekinbaro = pr_ekinbaro + wmtx(i,i)/bmass_flx
      enddo
      pr_ekinbaro = 0.5_dp*pr_ekinbaro
      ekin0 = pr_ekinbaro
      c2 = (1.0_dp - c1)*pr_ekintarget_baro/ekin0
      r1 = gauss_rand()
      r2 = sumnoises(ndof_baro-1_i4)
      fscale2 = c1 + c2*(r1**2 + r2) + 2.0_dp*r1*sqrt(c1*c2)
      fscale = sqrt(fscale2)
!
!  Scale the barostat momentum
!
      p_flx = p_flx*fscale
!
!  Integral of the kinetic energy for the conserved quantity
!
      pr_cons = pr_cons + (1.0_dp - fscale2)*ekin0
! 
!  Kinetic energy update
!
      pr_ekinbaro = ekin0*fscale2

    else
      pr_ekinbaro = 0.0_dp
    endif
  endif
 
  return

  end subroutine pr_thermostat

  function sumnoises(nn)
!
!  Returns the sum of n independent gaussian noises squared
!  (i.e. equivalent to summing the square of the return values of nn calls to gauss_rand)
!
  use datatypes
  use pr_random_module, only : gauss_rand

  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: nn
  real(dp),    external   :: gamdev
  real(dp)                :: sumnoises

  if (modulo(nn,2_i4)==0) then
    sumnoises = 2.0_dp*gamdev(nn/2_i4)
  else
    sumnoises = 2.0_dp*gamdev((nn-1_i4)/2_i4) + gauss_rand()**2
  endif

  return
  end function sumnoises

  function gamdev(ia)
!
!  Implemented as described in numerical recipes
!
  use datatypes
  use pr_random_module

  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: ia
  real(dp)                :: gamdev
!
!  Local variables
!
  integer(i4)             :: j
  real(dp)                :: am
  real(dp)                :: e
  real(dp)                :: s
  real(dp)                :: v1
  real(dp)                :: v2
  real(dp)                :: x
  real(dp)                :: y
!
  if (ia.lt.1) then
    call outerror('Bad argument in gamdev',0_i4)
    call stopnow('gamdev')
  endif
  if (ia.lt.6) then
    x = 1.0_dp
    do j = 1,ia
      x = x*std_rand()
    enddo
    x = - log(x)
  else
1   v1 = 2.0_dp*std_rand() - 1.0_dp
    v2 = 2.0_dp*std_rand() - 1.0_dp
    if (v1**2+v2**2.gt.1.0_dp) goto 1
    y = v2/v1
    am = ia - 1
    s = sqrt(2.0_dp*am+1.0_dp)
    x = s*y + am
    if (x.le.0.0_dp) goto 1
    e = (1.0_dp+y**2)*exp(am*log(x/am)-s*y)
    if (std_rand().gt.e) goto 1
  endif
  gamdev = x
  return

  end function gamdev

  subroutine get_hinv(hmat,hmati,deth)
 
  use datatypes

  implicit none
!
!  Passed variables
!
  real(dp), dimension(3,3) :: hmat, hmati
  real(dp)                 :: deth
!
!  Local variables
!
  real(dp)                 :: odet

  deth = &
         hmat(1,1) * ( hmat(2,2)*hmat(3,3)-hmat(2,3)*hmat(3,2) ) + &
         hmat(1,2) * ( hmat(2,3)*hmat(3,1)-hmat(2,1)*hmat(3,3) ) + &
         hmat(1,3) * ( hmat(2,1)*hmat(3,2)-hmat(2,2)*hmat(3,1) )
  !if (abs(deth) < 1.0d-10) then
  !  call outerror('Determinant too small in get_hinv',0_i4)
  !  call stopnow('get_hinv')
  !endif
  odet = 1.0_dp/deth
  hmati(1,1) = (hmat(2,2)*hmat(3,3)-hmat(2,3)*hmat(3,2))*odet
  hmati(2,2) = (hmat(1,1)*hmat(3,3)-hmat(1,3)*hmat(3,1))*odet
  hmati(3,3) = (hmat(1,1)*hmat(2,2)-hmat(1,2)*hmat(2,1))*odet
  hmati(1,2) = (hmat(1,3)*hmat(3,2)-hmat(1,2)*hmat(3,3))*odet
  hmati(2,1) = (hmat(3,1)*hmat(2,3)-hmat(2,1)*hmat(3,3))*odet
  hmati(1,3) = (hmat(1,2)*hmat(2,3)-hmat(1,3)*hmat(2,2))*odet
  hmati(3,1) = (hmat(2,1)*hmat(3,2)-hmat(3,1)*hmat(2,2))*odet
  hmati(2,3) = (hmat(1,3)*hmat(2,1)-hmat(2,3)*hmat(1,1))*odet
  hmati(3,2) = (hmat(3,1)*hmat(1,2)-hmat(3,2)*hmat(1,1))*odet

  return

  end subroutine get_hinv

  subroutine compute_ekin

  use current,    only : mass, numat, nspecptr
  use m_pr
  use moldyn,     only : refct, lfix
  use velocities, only : velx, vely, velz

  implicit none

  integer(i4) :: i
  integer(i4) :: ik

  pekin = 0.0_dp
  do i = 1,numat
    if (.not.lfix(i)) then
      ik = nspecptr(i)
      pekin(ik) = pekin(ik) + mass(i)*(velx(i)**2+vely(i)**2+velz(i)**2)
    endif
  enddo
! NB: refct converts to eV from mass*velocity**2
  pekin = 0.5_dp*refct*pekin
  pr_ekin = sum(pekin)

  return

  end subroutine compute_ekin

  subroutine compute_partial_ekin(ist,ind,partekin)

  use datatypes
  use current,    only : mass
  use moldyn,     only : refct
  use velocities, only : velx, vely, velz

  implicit none 

  integer(i4) :: i, ist, ind
  real(dp)    :: partekin

  partekin = 0.0_dp
  do i = ist,ind
    partekin = partekin + mass(i)*(velx(i)**2+vely(i)**2+velz(i)**2)
  enddo

! NB: refct converts to eV from mass*velocity**2
  partekin = 0.5_dp*refct*partekin

  return

  end subroutine compute_partial_ekin
  
  subroutine compute_kinstress

  use current,    only : numat, mass
  use m_pr
  use moldyn,     only : refct, lfix
  use velocities, only : velx, vely, velz

  implicit none 

  integer(i4) :: i

  pr_kinstress = 0.0_dp
  do i = 1,numat
    if (.not.lfix(i)) then
      pr_kinstress(1,1) = pr_kinstress(1,1) + mass(i)*velx(i)*velx(i)
      pr_kinstress(2,1) = pr_kinstress(2,1) + mass(i)*vely(i)*velx(i)
      pr_kinstress(3,1) = pr_kinstress(3,1) + mass(i)*velz(i)*velx(i)
      pr_kinstress(1,2) = pr_kinstress(1,2) + mass(i)*velx(i)*vely(i)
      pr_kinstress(2,2) = pr_kinstress(2,2) + mass(i)*vely(i)*vely(i)
      pr_kinstress(3,2) = pr_kinstress(3,2) + mass(i)*velz(i)*vely(i)
      pr_kinstress(1,3) = pr_kinstress(1,3) + mass(i)*velx(i)*velz(i)
      pr_kinstress(2,3) = pr_kinstress(2,3) + mass(i)*vely(i)*velz(i)
      pr_kinstress(3,3) = pr_kinstress(3,3) + mass(i)*velz(i)*velz(i)
    endif
  enddo

! NB: refct converts to eV from mass*velocity**2
  pr_kinstress = 0.5_dp*refct*pr_kinstress

  return

  end subroutine compute_kinstress

  subroutine pr_remove_com
!
! Author          :: Paolo Raiteri
! First committed :: 01-Jan-2010
!
! Modifications   ::
!
  use datatypes
  use current,     only : numat, mass
  use moldyn,      only : ltethered
  use velocities,  only : velx, vely, velz
  implicit none
!
! Local variables
!
  integer(i4)  :: i
  real(dp)     :: total_mass
  real(dp)     :: v(3)
!
!  If atoms are fixed then the centre of mass should already be constrained
!
  if (ltethered) return
!
  v(1:3) = 0.0_dp
  total_mass = 0.0_dp
  do i = 1,numat
    total_mass = total_mass + mass(i)
    v(1) = v(1) + velx(i)*mass(i)
    v(2) = v(2) + vely(i)*mass(i)
    v(3) = v(3) + velz(i)*mass(i)
  enddo
  v(1:3) = v(1:3)/total_mass
  do i = 1,numat
    velx(i) = velx(i) - v(1)
    vely(i) = vely(i) - v(2)
    velz(i) = velz(i) - v(3)
  enddo

  return

  end subroutine pr_remove_com

  subroutine pr_remove_torque
!
! Author          :: Paolo Raiteri
! First committed :: 01-Jan-2010
!
! Modifications   ::
!
  use datatypes
  use current,     only : numat, mass, xalat, yalat, zalat
  use moldyn,      only : ltethered
  use velocities,  only : velx, vely, velz
  implicit none
!
! Local variables
!
  integer(i4)  :: i
  real(dp)     :: Iner(3,3), L(3), omega(3), pcm(3), rij(3), Iner_inv(3,3), rtmp, vij(3)
  real(dp)     :: total_mass
!
!  If atoms are fixed then the torque should already be constrained
!
  if (ltethered) return
!
! Remove angular momentum
!
  pcm = 0.0_dp
  total_mass = 0.0_dp
  do i = 1,numat
    total_mass = total_mass + mass(i)
    pcm(1) = pcm(1) + mass(i)*xalat(i)
    pcm(2) = pcm(2) + mass(i)*yalat(i)
    pcm(3) = pcm(3) + mass(i)*zalat(i)
  enddo
  pcm = pcm/total_mass

  L(1:3) = 0.0_dp
  do i = 1,numat
    rij(1) = xalat(i) - pcm(1)
    rij(2) = yalat(i) - pcm(2)
    rij(3) = zalat(i) - pcm(3)
    L(1) = L(1) + mass(i)*( rij(2)*velz(i)-rij(3)*vely(i))
    L(2) = L(2) + mass(i)*(-rij(1)*velz(i)+rij(3)*velx(i))
    L(3) = L(3) + mass(i)*( rij(1)*vely(i)-rij(2)*velx(i))
  enddo

  Iner = 0.0_dp
  do i = 1,numat
    rij(1) = xalat(i) - pcm(1)
    rij(2) = yalat(i) - pcm(2)
    rij(3) = zalat(i) - pcm(3)
    Iner(1,1) = Iner(1,1) + mass(i)*(rij(2)**2+rij(3)**2)
    Iner(2,2) = Iner(2,2) + mass(i)*(rij(1)**2+rij(3)**2)
    Iner(3,3) = Iner(3,3) + mass(i)*(rij(1)**2+rij(2)**2)
    Iner(1,2) = Iner(1,2) + mass(i)*rij(1)*rij(2)
    Iner(2,1) = Iner(2,1) + mass(i)*rij(1)*rij(2)
    Iner(1,3) = Iner(1,3) + mass(i)*rij(1)*rij(3)
    Iner(3,1) = Iner(3,1) + mass(i)*rij(1)*rij(3)
    Iner(2,3) = Iner(2,3) + mass(i)*rij(2)*rij(3)
    Iner(3,2) = Iner(3,2) + mass(i)*rij(2)*rij(3)
  enddo
  call get_hinv(Iner,Iner_inv,rtmp)
  omega = matmul(Iner_inv,L)

  do i = 1,numat
    rij(1) = xalat(i) - pcm(1)
    rij(2) = yalat(i) - pcm(2)
    rij(3) = zalat(i) - pcm(3)
    vij(1) = ( omega(2)*rij(3)-omega(3)*rij(2))
    vij(2) = (-omega(1)*rij(3)+omega(3)*rij(1))
    vij(3) = ( omega(1)*rij(2)-omega(2)*rij(1))
    velx(i) = velx(i) - vij(1)
    vely(i) = vely(i) - vij(2)
    velz(i) = velz(i) - vij(3)
  enddo

  return

  end subroutine pr_remove_torque
