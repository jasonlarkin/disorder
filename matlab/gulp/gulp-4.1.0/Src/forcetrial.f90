  subroutine forcetrial(eforce,ntrialatom,nptrtrialatom)
!
!  Subroutine for calculating the correction to the energy
!  due to an external force. Subset of atoms version.
!
!   1/08 Created from force
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
  use configurations, only : forcecfg
  use parallel,       only : nprocs, procid
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: ntrialatom
  integer(i4), intent(in)    :: nptrtrialatom(ntrialatom)
  real(dp), intent(out)      :: eforce
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: nt
!
!  Initialise integral of force x distance
!
  eforce = 0.0_dp
!
!  If number of atoms is zero then return as there is nothing to do
!
  if (ntrialatom.eq.0) return
!
!  Loop over asymmetric unit performing integral
!
  do nt = 1+procid,ntrialatom,nprocs
    i = nptrtrialatom(nt)
    eforce = eforce - forcecfg(1,nsft+i)*(xalat(i) - xinitial(i))
    eforce = eforce - forcecfg(2,nsft+i)*(yalat(i) - yinitial(i))
    eforce = eforce - forcecfg(3,nsft+i)*(zalat(i) - zinitial(i))
  enddo
!
  return
  end
