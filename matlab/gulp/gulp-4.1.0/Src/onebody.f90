  subroutine onebody(eone,esregion2,eattach)
!
!  Subroutine for calculating the one-body energy shift.
!  No need for any derivatives!
!
!   1/10 Created 
!  11/11 Region-region energy contributions stored and siteenergy
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
  use configurations, only : nregions, nregionno, lsliceatom
  use control
  use current
  use energies,       only : siteenergy, eregion2region
  use one
  use parallel
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out)                     :: eone
  real(dp),    intent(inout)                   :: esregion2
  real(dp),    intent(inout)                   :: eattach
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: nati
  integer(i4)                                  :: no
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nmin
  integer(i4)                                  :: nstep
  integer(i4)                                  :: ntypi
  logical                                      :: lmatch
  logical                                      :: lreg2ok
  logical                                      :: lslicei
  real(dp)                                     :: etrm
  real(dp)                                     :: oci      
!
!  Initialise the energy
!
  eone = 0.0_dp
!
  lreg2ok = (lseok.and.nregions(ncf).gt.1)
! 
!  Setup loop variables 
!
  if (nprocs.gt.1) then
    nmin  = procid + 1
    nstep = nprocs      
  else
    nmin  = 1           
    nstep = 1           
  endif
! 
!  Loop over asymmetric unit performing integral
! 
  do i = nmin,numat,nstep
!
!  Set attributes of i
!
    nati = nat(i)
    ntypi = nftype(i)
    oci = occuf(i)
    nregioni = nregionno(nsft+i)
    lslicei = lsliceatom(nsft + nrelat(i))
!
!  Loop over one-body potentials
!
    etrm = 0.0_dp
    do no = 1,none
!
!  Does this potential apply to this atom
!
      if (lmatch(nati,ntypi,nspec11(no),nptyp11(no),.true.)) then
!
!  Ashift
!
        etrm = etrm + onepot(no)
      endif
    enddo
    etrm = oci*etrm
!
!  Region handling
!
    if (lreg2ok.and.nregioni.gt.1) then
      esregion2 = esregion2 + etrm
    else
      eone = eone + etrm
    endif
    if (lslicei) eattach = eattach + etrm
!
    eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + etrm
!
    siteenergy(i) = siteenergy(i) + etrm
  enddo
!
  return
  end
