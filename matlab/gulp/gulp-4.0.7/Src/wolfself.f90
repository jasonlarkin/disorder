  subroutine wolfself(ewolfself,esregion2)
!
!  Subroutine for calculating the energy due to the self term in the Wolf sum
!
!   1/03 Created 
!   3/09 Parallelised
!   4/09 Modified for shell model case so that sum of core and shell charge is used
!        rather than the individual values. This ensures consistency of energy.
!   4/09 Self term involving tweatpi now computed here rather than in selfterm 
!   6/09 Site energy added
!   7/11 Modified to correctly handle surface energy calculations
!   8/11 Site multiplicity factor added in for symmetry adapted case
!  11/11 Region-region energy contributions stored
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
!  Julian Gale, NRI, Curtin University, November 2011
!
  use configurations, only : nregions, nregionno, lregionrigid
  use constants,      only : angstoev
  use control,        only : lwolf
  use current
  use element,        only : maxele
  use energies,       only : siteenergy, eregion2region
  use general,        only : cutw, selfwolf
  use kspace,         only : tweatpi
  use parallel,       only : procid, nprocs
  use shell,          only : ncsptr
  implicit none
!
!  Passed variables
!
  real(dp), intent(out)   :: ewolfself
  real(dp), intent(inout) :: esregion2
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: ii
  integer(i4)             :: is
  integer(i4)             :: nregioni
  logical                 :: lreg2pair
  real(dp)                :: etrm
  real(dp)                :: q2sum
  real(dp)                :: q2sum2
  real(dp)                :: qi
!
!  Initialise energy
!
  ewolfself = 0.0_dp
!
!  If this is not a Wolf sum return
!
  if (.not.lwolf.or.cutw.lt.1.0d-8) return
!
!  Loop over asymmetric unit summing the square of the charges
!
  q2sum  = 0.0_dp
  q2sum2 = 0.0_dp
  etrm = 0.5_dp*angstoev*(selfwolf+tweatpi)
  do i = procid+1,nasym,nprocs
    if (iatn(i).le.maxele) then
!
!  Set region 2 pair flag
!
      lreg2pair = .false.
      if (nregions(ncf).ge.2) then
        lreg2pair = (lregionrigid(nregionno(nsft+i),ncf))
      endif
      qi = qa(i)*occua(i)
!
!  This is a core - check for a shell
!
      ii = nrel2(i)
      is = ncsptr(ii)
      if (is.gt.0) then
        qi = qi + qf(is)*occuf(is)
      endif
      if (lreg2pair) then
        q2sum2 = q2sum2 + qi*qi*dble(neqv(i))
      else
        q2sum  = q2sum  + qi*qi*dble(neqv(i))
      endif
!
      nregioni = nregionno(nsft+i)
      eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) - qi*qi*etrm
!
      siteenergy(i) = siteenergy(i) - qi*qi*etrm
    endif
  enddo
!
!  Calculate energy
!
  ewolfself = - q2sum*etrm
  esregion2 = esregion2 - q2sum2*etrm
!
  return
  end
