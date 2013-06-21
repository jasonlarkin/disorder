  subroutine mdshscale
!
!  Scale shell velocities for MD to maintain C-S temperature
!
!   8/97 Created from mdscale
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
  use current
  use element, only : maxele
  use moldyn
  use optimisation
  use shell
  use velocities
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: isp
  real(dp)           :: csfactor
  real(dp)           :: csfct
  real(dp)           :: prodmas
  real(dp)           :: pxcs
  real(dp)           :: pycs
  real(dp)           :: pzcs
  real(dp)           :: rkecs
  real(dp)           :: rm
  real(dp)           :: rvx
  real(dp)           :: rvy
  real(dp)           :: rvz
!
!  Target KE per C-S pair
!
  csfactor = velfctt/(dble(nmoving)*temperature)
  do i = 1,numat
    if (lopf(i).and..not.lfix(i).and.nat(i).le.maxele) then
      isp = ncsptr(i)
      if (isp.ne.0) then
!
!  Calculate relative velocities of C and S
!
        rvx = velx(i) - velx(isp)
        rvy = vely(i) - vely(isp)
        rvz = velz(i) - velz(isp)
        prodmas = ratiom(i)*ratiom(isp)
        rm = mass(i) + mass(isp)
        rkecs = prodmas*rm*(rvx*rvx+rvy*rvy+rvz*rvz)
        if (rkecs.gt.1.0d-12) then
!
!  If there is a relative velocity then correct velocities
!
          csfct = prodmas*sqrt(csfactor/rkecs)
          pxcs = ratiom(i)*velx(i) + ratiom(isp)*velx(isp)
          pycs = ratiom(i)*vely(i) + ratiom(isp)*vely(isp)
          pzcs = ratiom(i)*velz(i) + ratiom(isp)*velz(isp)
          velx(i) = pxcs - csfct*rvx/ratiom(i)
          vely(i) = pycs - csfct*rvy/ratiom(i)
          velz(i) = pzcs - csfct*rvz/ratiom(i)
          velx(isp) = pxcs + csfct*rvx/ratiom(isp)
          vely(isp) = pycs + csfct*rvy/ratiom(isp)
          velz(isp) = pzcs + csfct*rvz/ratiom(isp)
        endif
      endif
    endif
  enddo
!
  return
  end
