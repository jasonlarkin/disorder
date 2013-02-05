  subroutine mdvelcor(langular,lequilibration,lproduction)
!
!  Eliminates translational and rotational components from velocities
!
!   2/97 Modifications from JRH added
!   6/04 Rewritten to better handle angular momentum and to
!        better allow for fixed atoms.
!   7/04 Error in linear momentum correction fixed.
!   9/04 Flag for equilibration/production modes added to arguments
!   9/04 Option to restrict corrections to a subset of atoms added
!   7/11 Corrected so that linear momentum is removed, rather than velocity
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
  use current
  use element, only : maxele
  use moldyn
  use optimisation
  use shell,   only : ncore
  use velocities
  implicit none
!
!  Passed variables
!
  logical,      intent(in)                     :: langular
  logical,      intent(in)                     :: lequilibration
  logical,      intent(in)                     :: lproduction
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifail
  integer(i4)                                  :: j
  integer(i4)                                  :: napply
  integer(i4)                                  :: ni
  logical                                      :: lDoRotate
  logical                                      :: lDoTranslate
  real(dp)                                     :: inertia(3,3)
  real(dp)                                     :: lmatrix(3)
  real(dp)                                     :: omega(3)
  real(dp)                                     :: r2i
  real(dp)                                     :: rmi
  real(dp)                                     :: rmsum
  real(dp)                                     :: rmx
  real(dp)                                     :: rmy
  real(dp)                                     :: rmz
  real(dp)                                     :: rtmp(6)
  real(dp)                                     :: sumx
  real(dp)                                     :: sumy
  real(dp)                                     :: sumz
  real(dp)                                     :: total_rmi
  real(dp)                                     :: velsqi
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
!
!  Check whether there are sufficient fixed atoms to ensure that
!  momentum is naturally conserved without further work.
!  If one atom is fixed then there can be no translation.
!  If three atoms are fixed and they are non-colinear then there
!  can be no rotation.
!
  lDoTranslate = (ncore.eq.nmoving)
  lDoRotate = (langular.and.(ncore-nmoving).lt.3)
!
!  Need to add a check on non-colinearity of fixed atoms here!
!
  if (.not.lDoRotate.and..not.lDoTranslate) return
!
!  Set number of atoms to be corrected
!
  if (lequilibration) then
    if (nmdvelmode(ncf).gt.-1) then
      napply = nmdvelmode(ncf)
    else
      napply = numat
    endif
  elseif (lproduction) then
    if (nmdvelmodp(ncf).gt.-1) then
      napply = nmdvelmodp(ncf)
    else
      napply = numat
    endif
  else
    napply = numat
  endif
!*************************************************
!  Correct velocities to remove linear momentum  *
!*************************************************
  if (lDoTranslate) then
    sumx = 0.0_dp
    sumy = 0.0_dp
    sumz = 0.0_dp
    ni = 0
    total_rmi = 0.0_dp
    do i = 1,napply
      rmi = mass(i)
      if (lopf(i).and..not.lfix(i).and.abs(rmi).gt.1.0d-15) then
        ni = ni + 1
        total_rmi = total_rmi + rmi
        sumx = sumx + velx(i)*rmi
        sumy = sumy + vely(i)*rmi
        sumz = sumz + velz(i)*rmi
      endif
    enddo
    sumx = sumx/dble(total_rmi)
    sumy = sumy/dble(total_rmi)
    sumz = sumz/dble(total_rmi)
    velsq = 0.0_dp
    do i = 1,napply
      rmi = rmass(i)
      if (lopf(i).and..not.lfix(i).and.abs(rmi).gt.1.0d-15) then
        velx(i) = velx(i) - sumx
        vely(i) = vely(i) - sumy
        velz(i) = velz(i) - sumz
        velsqi = velx(i)*velx(i) + vely(i)*vely(i) + velz(i)*velz(i)
        velsq = velsq + velsqi*mass(i)
      endif
    enddo
  endif
!**************************************************
!  Correct velocities to remove angular momentum  *
!**************************************************
  if (lDoRotate) then
!
!  Find centre of mass
!
    rmsum = 0.0_dp
    rmx = 0.0_dp
    rmy = 0.0_dp
    rmz = 0.0_dp
    do i = 1,napply
      if (lopf(i).and..not.lfix(i)) then
        rmi = mass(i)
        rmx = rmx + rmi*xalat(i)
        rmy = rmy + rmi*yalat(i)
        rmz = rmz + rmi*zalat(i)
        rmsum = rmsum + rmi
      endif
    enddo
    rmx = rmx/rmsum
    rmy = rmy/rmsum
    rmz = rmz/rmsum
!
!  Calculate inert and L matrices
!
    inertia(1:3,1:3) = 0.0_dp
    lmatrix(1:3) = 0.0_dp
    do i = 1,napply
      if (lopf(i).and..not.lfix(i)) then
        rmi = mass(i)
        xi = xalat(i) - rmx
        yi = yalat(i) - rmy
        zi = zalat(i) - rmz
        r2i = xi*xi + yi*yi + zi*zi
        inertia(1,1) = inertia(1,1) + rmi*(r2i - xi*xi)
        inertia(2,1) = inertia(2,1) - rmi*yi*xi
        inertia(3,1) = inertia(3,1) - rmi*zi*xi
        inertia(1,2) = inertia(1,2) - rmi*xi*yi
        inertia(2,2) = inertia(2,2) + rmi*(r2i - yi*yi)
        inertia(3,2) = inertia(3,2) - rmi*zi*yi
        inertia(1,3) = inertia(1,3) - rmi*xi*zi
        inertia(2,3) = inertia(2,3) - rmi*yi*zi
        inertia(3,3) = inertia(3,3) + rmi*(r2i - zi*zi)
!
        lmatrix(1) = lmatrix(1) + rmi*(yi*velz(i) - zi*vely(i))
        lmatrix(2) = lmatrix(2) + rmi*(zi*velx(i) - xi*velz(i))
        lmatrix(3) = lmatrix(3) + rmi*(xi*vely(i) - yi*velx(i))
      endif
    enddo
!
!  Invert inertia matrix
!
    call matinv(inertia,3_i4,3_i4,rtmp,ifail)
    if (ifail.eq.0) then
!
!  Find omega matrix
!
      do i = 1,3
        omega(i) = 0.0_dp
        do j = 1,3
          omega(i) = omega(i) + inertia(i,j)*lmatrix(j)
        enddo
      enddo
!
!  Correct velocities
!
      lmatrix(1:3) = 0.0_dp
      do i = 1,napply
        if (lopf(i).and..not.lfix(i)) then
          rmi = mass(i)
          xi = xalat(i) - rmx
          yi = yalat(i) - rmy
          zi = zalat(i) - rmz
          velx(i) = velx(i) - (omega(2)*zi - omega(3)*yi)
          vely(i) = vely(i) - (omega(3)*xi - omega(1)*zi)
          velz(i) = velz(i) - (omega(1)*yi - omega(2)*xi)
          lmatrix(1) = lmatrix(1) + rmi*(yi*velz(i) - zi*vely(i))
          lmatrix(2) = lmatrix(2) + rmi*(zi*velx(i) - xi*velz(i))
          lmatrix(3) = lmatrix(3) + rmi*(xi*vely(i) - yi*velx(i))
        endif
      enddo
    endif
  endif
!
  return
  end
