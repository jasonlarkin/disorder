  subroutine force(eforce,lgrad1)
!
!  Subroutine for calculating the correction to the energy
!  due to an external force.
!
!   8/02 Created
!  10/02 Weighting by neqv added
!   2/04 lgrad1 flag added
!   2/04 Force delay added for MD
!   6/04 End of force time added for MD
!   7/04 Sign of the force corrected for the TD part
!   8/04 Bug in parallel use corrected - forces only applied
!        once over all nodes.
!   8/04 Trap added for zero atom case
!   8/04 Addition of external force to force moved here from 
!        initdervs.f
!  11/04 Pi accessed from module
!   6/09 Site energy added
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
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
  use configurations, only : forcecfg, tdforcecfg, ltdforcecfg, nregionno
  use constants,      only : pi
  use current
  use derivatives,    only : xdrv, ydrv, zdrv, xregdrv, yregdrv, zregdrv
  use energies,       only : siteenergy, eregion2region
  use general,        only : timesofar
  use mdlogic,        only : lmd
  use moldyn,         only : tmdforcestart,tmdforcestop
  use parallel,       only : nprocs, procid
  implicit none
!
!  Passed variables
!
  logical,  intent(in)  :: lgrad1
  real(dp), intent(out) :: eforce
!
!  Local variables
!
  integer(i4)           :: i
  integer(i4)           :: nregioni
  real(dp)              :: esum
  real(dp)              :: tdf
  real(dp)              :: tsf
  real(dp)              :: twopi
!
!  Initialise integral of force x distance
!
  eforce = 0.0_dp
!
!  Force delay for MD
!
  if (lmd) then
    if (timesofar.lt.tmdforcestart(ncf)) return
    if (tmdforcestop(ncf).gt.0.0_dp) then
      if (timesofar.gt.tmdforcestop(ncf)) return
    endif
  endif
!
!  If number of atoms is zero then return as there is nothing to do
!
  if (nasym.eq.0) return
!
!  Loop over asymmetric unit performing integral
!
  do i = 1+procid,nasym,nprocs
    esum = dble(neqv(i))*(forcecfg(1,nsft+i)*(xalat(i) - xinitial(i)) + &
                          forcecfg(2,nsft+i)*(yalat(i) - yinitial(i)) + &
                          forcecfg(3,nsft+i)*(zalat(i) - zinitial(i)))
    eforce = eforce - esum
    nregioni = nregionno(nsft+i)
    eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) - esum
    siteenergy(i) = siteenergy(i) - esum
  enddo
  if (lgrad1) then
    do i = 1+procid,nasym,nprocs
      xdrv(i) = xdrv(i) - forcecfg(1,nsft+i)
      ydrv(i) = ydrv(i) - forcecfg(2,nsft+i)
      zdrv(i) = zdrv(i) - forcecfg(3,nsft+i)
!
      nregioni = nregionno(nsft+i)
      xregdrv(nregioni) = xregdrv(nregioni) - forcecfg(1,nsft+i)
      yregdrv(nregioni) = yregdrv(nregioni) - forcecfg(2,nsft+i)
      zregdrv(nregioni) = zregdrv(nregioni) - forcecfg(3,nsft+i)
    enddo
  endif
!
!  Time-dependent forces
!
  if (lmd.and.lgrad1) then
    twopi = 2.0_dp*pi
    tsf = timesofar - tmdforcestart(ncf)
    do i = 1+procid,nasym,nprocs
      nregioni = nregionno(nsft+i)
      if (ltdforcecfg(1,nsft+i)) then
        tdf = - tdforcecfg(1,1,nsft+i)*cos(twopi*(tsf*tdforcecfg(2,1,nsft+i) + tdforcecfg(3,1,nsft+i)))
        xdrv(i) = xdrv(i) + tdf
        xregdrv(nregioni) = xregdrv(nregioni) + tdf
      endif
      if (ltdforcecfg(2,nsft+i)) then
        tdf = - tdforcecfg(1,2,nsft+i)*cos(twopi*(tsf*tdforcecfg(2,2,nsft+i) + tdforcecfg(3,2,nsft+i)))
        ydrv(i) = ydrv(i) + tdf
        yregdrv(nregioni) = yregdrv(nregioni) + tdf
      endif
      if (ltdforcecfg(3,nsft+i)) then
        tdf = - tdforcecfg(1,3,nsft+i)*cos(twopi*(tsf*tdforcecfg(2,3,nsft+i) + tdforcecfg(3,3,nsft+i)))
        zdrv(i) = zdrv(i) + tdf
        zregdrv(nregioni) = zregdrv(nregioni) + tdf
      endif
    enddo
  endif
!
  return
  end
