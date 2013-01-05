  subroutine electricfieldpotl(Vfield)
!
!  Subroutine for calculating the potential due to an external
!  electric field. Note that this can should only be done in non-periodic
!  directions and that the potential is defined as the integral of the
!  field/per charge relative to the initial geometry.
!
!   8/11 Created from electricfield.f90
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
!  Julian Gale, NRI, Curtin University, August 2011
!
  use current
  use field
  use parallel,       only : nprocs, procid
  implicit none
!
!  Passed variables
!
  real(dp), intent(inout) :: Vfield(*)
!
!  Local variables
!
  integer(i4)           :: i
  real(dp)              :: fieldx
  real(dp)              :: fieldy
  real(dp)              :: fieldz
  real(dp)              :: fnorm
!
!  If number of atoms is zero then return as there is nothing to do
!
  if (numat.eq.0) return
!
!  For 3-D convert the field direction from cell vector to Cartesian
!
  if (ndim.eq.3) then
    fieldx = fielddirectioncfg(1,ncf)*rv(1,1) + fielddirectioncfg(2,ncf)*rv(1,2) + fielddirectioncfg(3,ncf)*rv(1,3)
    fieldy = fielddirectioncfg(1,ncf)*rv(2,1) + fielddirectioncfg(2,ncf)*rv(2,2) + fielddirectioncfg(3,ncf)*rv(2,3)
    fieldz = fielddirectioncfg(1,ncf)*rv(3,1) + fielddirectioncfg(2,ncf)*rv(3,2) + fielddirectioncfg(3,ncf)*rv(3,3)
    fnorm = fieldx**2 + fieldy**2 + fieldz**2
    fnorm = 1.0_dp/sqrt(fnorm)
    fieldx = fieldcfg(ncf)*fieldx*fnorm
    fieldy = fieldcfg(ncf)*fieldy*fnorm
    fieldz = fieldcfg(ncf)*fieldz*fnorm
  else
!
!  Find norm of field direction and scale components
!
    fnorm = fielddirectioncfg(1,ncf)**2 + fielddirectioncfg(2,ncf)**2 + fielddirectioncfg(3,ncf)**2
    fnorm = 1.0_dp/sqrt(fnorm)
    fieldx = fieldcfg(ncf)*fielddirectioncfg(1,ncf)*fnorm
    fieldy = fieldcfg(ncf)*fielddirectioncfg(2,ncf)*fnorm
    fieldz = fieldcfg(ncf)*fielddirectioncfg(3,ncf)*fnorm
  endif
!
!  Loop over asymmetric unit performing integral
!
  do i = 1+procid,numat,nprocs
    Vfield(i) = Vfield(i) + (fieldx*xalat(i) + fieldy*yalat(i) + fieldz*zalat(i))
  enddo
!
  return
  end
