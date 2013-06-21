  subroutine planepot2D(eplane,esregion2,eattach,lgrad1,lgrad2)
!
!  Subroutine for calculating the energy from the plane potentials
!
!   7/07 Created 
!  12/07 Unused variables removed
!   6/09 Site energy and virial added
!   4/12 zvir removed
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
  use configurations, only : nregions, nregionno, lsliceatom
  use control
  use current
  use derivatives
  use energies,       only : siteenergy
  use mdlogic
  use optimisation
  use plane
  implicit none
!
!  Passed variables
!
  real(dp),    intent(inout)                   :: eplane
  real(dp),    intent(inout)                   :: esregion2
  real(dp),    intent(inout)                   :: eattach
  logical,     intent(in)                      :: lgrad1
  logical,     intent(in)                      :: lgrad2
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: iz
  integer(i4)                                  :: m
  integer(i4)                                  :: n  
  integer(i4)                                  :: nati
  integer(i4)                                  :: npp
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: ntypi
  logical                                      :: lmatch
  logical                                      :: lopi
  logical                                      :: lreg2ok
  logical                                      :: lslicei
  real(dp)                                     :: Atrm
  real(dp)                                     :: Btrm
  real(dp)                                     :: d1trm
  real(dp)                                     :: d2trm
  real(dp)                                     :: deltaz
  real(dp)                                     :: etrm
  real(dp)                                     :: oci      
  real(dp)                                     :: rdeltaz
  real(dp)                                     :: zp
!
!  Initialise the energy
!
  eplane = 0.0_dp
!
  lreg2ok = (lseok.and.nregions(ncf).gt.1)
  iz =   0
  do i = 1,numat
!
!  Set attributes of i
!
    nati = nat(i)
    ntypi = nftype(i)
    oci = occuf(i)
    nregioni = nregionno(nsft+i)
    lopi = (.not.lfreeze.or.lopf(nrelat(i)))
    lslicei = lsliceatom(nsft + nrelat(i))
    if (lopi.or..not.lfreeze) then
      iz = iz + 3 
    endif
!
!  Loop over plane potentials
!
    etrm = 0.0_dp
    do npp = 1,nplanepot
!
!  Does this potential apply to this atom
!
      if (lmatch(nati,ntypi,natplanepot(npp),ntypplanepot(npp),.true.)) then
!
!  Select potential type
!
        if (nplanepottype(npp).eq.1) then
!
!  Lennard-Jones
!
          zp = planepot(1,npp)
          deltaz = zclat(i) - zp
!
!  Check distance against cutoffs
!
          if (abs(deltaz).gt.planepotrmin(npp).and.abs(deltaz).le.planepotrmax(npp)) then
            m = nplanepotpower(1,npp)
            n = nplanepotpower(2,npp)
            rdeltaz = 1.0_dp/deltaz
            Atrm = planepot(2,npp)*rdeltaz**m
            Btrm = planepot(3,npp)*rdeltaz**n
            etrm = etrm + oci*(Atrm - Btrm)
            siteenergy(i) = siteenergy(i) + oci*(Atrm - Btrm)
            if (lgrad1) then
              d1trm = - oci*rdeltaz*(dble(m)*Atrm - dble(n)*Btrm)
              zdrv(i) = zdrv(i) + d1trm
              if (lgrad2) then
                d2trm = oci*(rdeltaz**2)*(dble(m*(m+1))*Atrm - dble(n*(n+1))*Btrm)
                derv2d(iz) = derv2d(iz) + d2trm
              endif
            endif
          endif
        endif
      endif
    enddo
!
!  Region handling
!
    if (lreg2ok.and.nregioni.gt.1) then
      esregion2 = esregion2 + etrm
    else
      eplane = eplane + etrm
    endif
    if (lslicei) eattach = eattach + etrm
  enddo
!
  return
  end
