  subroutine radialforce(eradial,lgrad1,lgrad2)
!
!  Subroutine for calculating the energy and derivatives
!  due to a radial force.
!
!   3/07 Created
!   6/09 Site energy and virials added
!  11/09 Region derivatives added
!   4/12 xvir, yvir and zvir removed
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
!  Julian Gale, NRI, Curtin University, April 2012
!
  use configurations, only : nregionno
  use current
  use derivatives,    only : xdrv, ydrv, zdrv
  use derivatives,    only : xregdrv, yregdrv, zregdrv
  use energies,       only : siteenergy
  use parallel,       only : nprocs, procid
  use radial
  implicit none
!
!  Passed variables
!
  logical,  intent(in)  :: lgrad1
  logical,  intent(in)  :: lgrad2
  real(dp), intent(out) :: eradial
!
!  Local variables
!
  integer(i4)           :: i
  integer(i4)           :: nregioni
  real(dp)              :: k
  real(dp)              :: x
  real(dp)              :: y
  real(dp)              :: z
!
!  Initialise energy
!
  eradial = 0.0_dp
!
!  If number of atoms is zero then return as there is nothing to do
!
  if (numat.eq.0) return
!
!  Loop over atoms calculating radial force
!
  k = radialKcfg(ncf)
  x = radialXYZcfg(1,ncf)
  y = radialXYZcfg(2,ncf)
  z = radialXYZcfg(3,ncf)
  do i = 1+procid,numat,nprocs
    eradial = eradial + k*(xclat(i) - x)**2
    eradial = eradial + k*(yclat(i) - y)**2
    eradial = eradial + k*(zclat(i) - z)**2
    siteenergy(i) = siteenergy(i) + 0.5_dp*k*((xclat(i) - x)**2 + (yclat(i) - y)**2 + (zclat(i) - z)**2)
  enddo
  if (lgrad1) then
    do i = 1+procid,nasym,nprocs
      xdrv(i) = xdrv(i) + k*(xclat(i) - x)
      ydrv(i) = ydrv(i) + k*(yclat(i) - y)
      zdrv(i) = zdrv(i) + k*(zclat(i) - z)
!
      nregioni = nregionno(nsft+i)
      xregdrv(nregioni) = xregdrv(nregioni) + k*(xclat(i) - x)
      yregdrv(nregioni) = yregdrv(nregioni) + k*(yclat(i) - y)
      zregdrv(nregioni) = zregdrv(nregioni) + k*(zclat(i) - z)
    enddo
  endif
  if (lgrad2) then
!
!  Note that second derivatives aren't done in parallel and so this is not catered for
!
  endif
!
!  Multiply by factor of a half
!
  eradial = 0.5_dp*eradial
!
  return
  end
