  subroutine mctrialfunction(etrial,ecurrent,betamc,cpfactor,noperation,ftrial)
!
!  This routine computes the appropriate trial function to be sampled during
!  Monte Carlo. 
!
!  On entry :
!
!  etrial      = trial internal energy
!  ecurrent    = internal energy from previous step
!  betamc      = 1/kT
!  cpfactor    = chemical potential factor for GCMC
!  noperation  = pointer to type of trial move
!
!  On exit : 
!
!  ftrial      = trial function
!
!   1/01 Created
!   6/04 Sum of mass made cumulative
!  11/04 sum of masses now correctly calculated for creation as well as destruction
!  11/04 sum of masses is raised to power of 3/2 instead of individual masses
!  11/04 mcdecision renamed mctrialfunction as decision is no longer contained
!  11/04 calculation of mass for destruction corrected
!  12/04 ndestroyable changed to ndestroyablemol so that atoms in molecules are
!        counted as a single translating entity
!   5/06 Mass now uses species values
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, May 2006
!
  use datatypes
  use current,    only : numat, nspecptr
  use montecarlo, only : nptrtrialatom, ntrialatom, ndestroyablemol
  use species,    only : massspec
  implicit none
!
!  Passed variables
!
  integer(i4),     intent(in)              :: noperation
  real(dp),        intent(in)              :: betamc
  real(dp),        intent(in)              :: cpfactor
  real(dp),        intent(in)              :: ecurrent
  real(dp),        intent(in)              :: etrial
  real(dp),        intent(out)             :: ftrial
!
!  Local variables
!
  integer(i4)                              :: i
  integer(i4)                              :: ii
  integer(i4)                              :: ni
  real(dp)                                 :: sumofmass
  real(dp)                                 :: totgcfactor
!
!  Initialise Ftrial as internal energy change
!
  ftrial = (etrial - ecurrent)*betamc
!
!  If this is a move or rotation then just internal energy difference is needed
!
  if (noperation.ge.3) return
!
!  For GCMC compute the extra term that comes from the chemical potential and de Broglie wavelength.
!
  sumofmass = 0.0_dp
  if (noperation.eq.1) then
!
!  Determine mass by summing over atoms created
!
    do i = 1,ntrialatom
      ii = nptrtrialatom(i)
      ni = nspecptr(ii)
      sumofmass = sumofmass + massspec(ni)
    enddo
  elseif (noperation.eq.2) then
!
!  Determine mass by summing over atoms destroyed. Note that the destroyed atoms
!  are moved to the end of the atom arrays until the destruction is accepted and
!  therefore we address the data even though it goes beyond numat.
!
    do i = 1,ntrialatom
      ni = nspecptr(numat+i)
      sumofmass = sumofmass + massspec(ni)
    enddo
  endif
  sumofmass = sumofmass**1.5_dp
  totgcfactor = sumofmass*cpfactor
!
!  Correct trial function for chemical potential and partition function of an ideal gas
!
  if (noperation.eq.1) then
    ftrial = ftrial - log(totgcfactor/dble(ndestroyablemol+1))
  elseif (noperation.eq.2) then
    ftrial = ftrial - log(dble(ndestroyablemol)/totgcfactor)
  endif
!
  return
  end
