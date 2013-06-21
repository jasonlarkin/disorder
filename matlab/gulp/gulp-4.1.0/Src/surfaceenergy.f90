  subroutine surfaceenergy(etot)
!
!  Subroutine for calculating the surface energy
!
!   9/01 Created from energy as a separate routine so that it
!        can be reused in the surface free energy calculation.
!   8/02 Algorithm for calculation of surface energy changed.
!        Now assumes that region2 self term has been excluded
!        from etot along with region 1-2 interaction energy.
!   9/02 Modified to include polymer energy per length
!   9/02 Change in total energy definition handled with respect
!        to region 1-2 interaction
!  10/02 Energy expression finally corrected!
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
!  Julian Gale, NRI, Curtin University, June 2005
!
  use configurations
  use constants
  use control
  use current
  use energies
  use parallel
  implicit none
!
!  Passed variables
!
  real(dp),  intent(in) :: etot
!
!  Local variables
!
  real(dp)              :: ara
  real(dp)              :: area
  real(dp)              :: etotloc
!
  esurface = 0.0_dp
  etotloc = etot
!
!  Surface energy calculation / relative energy of polymer
!
  if (lseok) then
    if (nregions(ncf).gt.1) then
      esurface = -(sbulkecfg(ncf) - etotloc + 0.5_dp*esregion12 + esregion2)  
    else
      esurface = -(sbulkecfg(ncf) - etotloc)
    endif
    if (ndim.eq.2) then
      ara = area(rv)
      esurface = esurface*evtoj*1.0d20/ara
    elseif (ndim.eq.1) then
      ara = a
      esurface = esurface/ara
    endif
  endif
!
  return
  end
