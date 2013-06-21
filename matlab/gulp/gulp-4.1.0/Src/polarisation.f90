  subroutine polarisation(epolar,esregion12,esregion2,eattach,lgrad1)
!
!  Main subroutine for calculating the point-ion polarisability
!  contribution to the lattice energy.
!
!   5/00 Created from energy.f
!   2/01 Logic for call of derivatives routines corrected
!   4/01 Region 2 self energy calculation for surfaces added
!  11/01 Attachment energy added
!   8/02 Surface energy calculation algorithm changed
!  11/02 Parallel modifications made
!   6/09 Site energies added
!
!  Note : Needs correction for polarisation energy between
!         surface region 1 and surface region 2.
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use configurations, only : nregions, nregionno
  use control,        only : lseok
  use constants
  use current
  use energies,       only : siteenergy
  use parallel
  use polarise
  use potentialxyz
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,     intent(in)    :: lgrad1
  real(dp),    intent(inout) :: epolar
  real(dp),    intent(inout) :: esregion12
  real(dp),    intent(inout) :: esregion2
  real(dp),    intent(inout) :: eattach
!
!  Local variables
!
  integer(i4)                :: i
  real(dp)                   :: eattachl
  real(dp)                   :: epolar2
!*********************
!  Calculate energy  *
!*********************
  do i = procid+1,nasym,nprocs
    epolar = epolar - dpolar(i)*(vx(i)*vx(i)+vy(i)*vy(i)+vz(i)*vz(i))*occua(i)*neqv(i)
    siteenergy(i) = siteenergy(i) - 0.5_dp*dpolar(i)*(vx(i)*vx(i)+vy(i)*vy(i)+vz(i)*vz(i))*occua(i)*neqv(i)/angstoev
  enddo
  epolar = 0.5_dp*epolar/angstoev
  if (lseok.and.nregions(ncf).gt.1) then
    epolar2 = 0.0_dp
    do i = procid+1,nasym,nprocs
      if (nregionno(nsft+nrelat(i)).gt.1) then
        epolar2 = epolar2 - dpolar(i)*(vx(i)*vx(i)+vy(i)*vy(i)+vz(i)*vz(i))*occua(i)*neqv(i)
      endif
    enddo
    esregion2 = esregion2 + 0.5_dp*epolar2/angstoev
!
!  In new algorithm region2 self contribution must be excluded from epolar
!
    epolar = epolar - 0.5_dp*epolar2/angstoev
  endif
  eattachl = 0.0_dp
  do i = procid+1,nasym,nprocs
    eattachl = eattachl - dpolar(i)*(vx12(i)*vx12(i)+vy12(i)*vy12(i)+vz12(i)*vz12(i))*occua(i)*neqv(i)
  enddo
  eattachl = 0.5_dp*eattachl/angstoev
  eattach = eattach + eattachl
!
!  If derivatives are required and polarisation is significant
!  then continue further, else return
!
  if (abs(epolar).lt.1.0d-12.or..not.lgrad1) return
!*******************************
!  Electrostatic contribution  *
!*******************************
  if (ndim.gt.0) then
    if (lsymopt.and.lsymderv) then
      call pirealrecips
    else
      call pirealrecip
    endif
  else
    call pireal0d
  endif
!
  return
  end
