  subroutine electricfieldtrial(efield,ntrialatom,nptrtrialatom)
!
!  Subroutine for calculating the energy / forces due to an external
!  electric field. Note that this can only be done in non-periodic
!  directions and that the energy is defined as the integral of the
!  field relative to the initial geometry. Subset of atoms version.
!
!   1/08 Created from electricfield.f
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
  use field
  use parallel,       only : nprocs, procid
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: ntrialatom
  integer(i4), intent(in)    :: nptrtrialatom(ntrialatom)
  real(dp),    intent(out)   :: efield
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: nt
  real(dp)                   :: fieldx
  real(dp)                   :: fieldy
  real(dp)                   :: fieldz
  real(dp)                   :: fnorm
!
!  Initialise integral of force x distance
!
  efield = 0.0_dp
!
!  If number of atoms is zero then return as there is nothing to do
!
  if (ntrialatom.eq.0) return
!
!  Find norm of field direction and scale components
!
  fnorm = fielddirectioncfg(1,ncf)**2 + fielddirectioncfg(2,ncf)**2 + fielddirectioncfg(3,ncf)**2
  fnorm = 1.0_dp/sqrt(fnorm)
  fieldx = fieldcfg(ncf)*fielddirectioncfg(1,ncf)*fnorm
  fieldy = fieldcfg(ncf)*fielddirectioncfg(2,ncf)*fnorm
  fieldz = fieldcfg(ncf)*fielddirectioncfg(3,ncf)*fnorm
!
!  Loop over asymmetric unit performing integral
!
  do nt = 1+procid,ntrialatom,nprocs
    i = nptrtrialatom(nt)
    efield = efield - dble(neqv(i))*fieldx*(xalat(i) - xinitial(i))*qa(i)
    efield = efield - dble(neqv(i))*fieldy*(yalat(i) - yinitial(i))*qa(i)
    efield = efield - dble(neqv(i))*fieldz*(zalat(i) - zinitial(i))*qa(i)
  enddo
!
  return
  end
