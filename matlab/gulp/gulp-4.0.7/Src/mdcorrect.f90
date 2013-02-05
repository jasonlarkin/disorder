  subroutine mdcorrect(lequilibration,lproduction)
!
!  Performs the corrector step of the Gear 5th order algorithm
!  or of the Verlet algorithm.
!
!  nmdintegrator = 1 => Gear 5th order
!                  2 => velocity Verlet
!                  3 => leapfrog Verlet
!                  4 => Paolo's stochastic integrator
!
!   2/97 Velocity verlet added (JRH)
!   3/97 NVT ensemble added
!   7/97 Leapfrog verlet added
!   7/97 Individual integrators placed in subroutines
!   9/04 Flags passed through for leap frog verlet algorithm
!   3/08 Paolo's new integrator added
!   1/09 Name of new integrator changed to stochastic
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
!  Julian Gale, NRI, Curtin University, January 2009
!
  use current,     only : rv
  use derivatives, only : virial
  use moldyn
  use m_pr
  implicit none
!
!  Passed variables
!
  logical, intent(in)  :: lequilibration
  logical, intent(in)  :: lproduction
!
!  Local variables
!
  integer              :: i
  real(dp)             :: wmtx(3,3)
  real(dp)             :: vol
  real(dp)             :: volume
!
  if (nmdintegrator.eq.1) then
!******************************
!  Gear 5th order - NVE only  *
!******************************
    call mdgcorrect
  elseif (nmdintegrator.eq.2) then
!***********************************
!  Velocity Verlet - NVE / NVT(?)  *
!***********************************
    call mdvvcorrect
  elseif (nmdintegrator.eq.3) then
!***************************************
!  Leap Frog Verlet - NVE / NVT / NPT  *
!***************************************
    call mdlfvcorrect(lequilibration,lproduction)
  elseif (nmdintegrator.eq.4) then
!**************************
!  Stochastic integrator  *
!**************************
!
!  Update velocities -- dt/2
!
    call pr_update_velocities
!
!  Call thermostat-- dt/2
!
    if (lpr_thermo) then
      call pr_thermostat
    else
      call compute_ekin
      if (lpr_baro_iso) then
        pr_ekinbaro = 0.50_dp*p_iso**2/bmass_iso
      elseif (lpr_baro_flx) then
        pr_ekinbaro = 0.0_dp
        wmtx = matmul(transpose(p_flx),p_flx)
        do i = 1,3
          pr_ekinbaro = pr_ekinbaro + wmtx(i,i)/bmass_flx
        enddo
        pr_ekinbaro = 0.5_dp*pr_ekinbaro
      else
        pr_ekinbaro = 0.0_dp
      endif
    endif
!
!  Get volume of system
!
    vol = volume(rv)
!
!  Compute the pressure from the virial
!
    pr_press = pfct*(2.0_dp*pr_ekin - virial)/(3.0_dp*vol)
    if (lpr_baro_flx.or.lpr_stress) then
      call compute_kinstress
      pr_stress = pfct*(2.0_dp*pr_kinstress - virial_m)/vol
    endif
!
!  Temperature
!
    pr_temp = 2.0_dp*pr_ekin/pr_boltz/dble(ndof_atm)

  endif
!
  return
  end
