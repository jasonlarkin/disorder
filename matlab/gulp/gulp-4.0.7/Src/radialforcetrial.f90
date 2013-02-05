  subroutine radialforcetrial(eradial,ntrialatom,nptrtrialatom)
!
!  Subroutine for calculating the energy and derivatives
!  due to a radial force. Subset of atoms version.
!
!   1/08 Created from radialforce
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use current
  use parallel,       only : nprocs, procid
  use radial
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: ntrialatom
  integer(i4), intent(in)    :: nptrtrialatom(ntrialatom)
  real(dp),    intent(out)   :: eradial
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: nt
  real(dp)                   :: k
  real(dp)                   :: x
  real(dp)                   :: y
  real(dp)                   :: z
!
!  Initialise energy
!
  eradial = 0.0_dp
!
!  If number of atoms is zero then return as there is nothing to do
!
  if (ntrialatom.eq.0) return
!
!  Loop over atoms calculating radial force
!
  k = radialKcfg(ncf)
  x = radialXYZcfg(1,ncf)
  y = radialXYZcfg(2,ncf)
  z = radialXYZcfg(3,ncf)
  do nt = 1+procid,ntrialatom,nprocs
    i = nptrtrialatom(nt)
    eradial = eradial + k*(xclat(i) - x)**2
    eradial = eradial + k*(yclat(i) - y)**2
    eradial = eradial + k*(zclat(i) - z)**2
  enddo
!
!  Multiply by factor of a half
!
  eradial = 0.5_dp*eradial
!
  return
  end
