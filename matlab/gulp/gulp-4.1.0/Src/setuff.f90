  subroutine setuff
!
!  Uses combination rules to create the UFF potentials
!
!   4/07 Created
!   7/07 Search for higher bond orders modified to save work
!   7/07 Three body terms involving higher bond orders added
!  12/07 Unused variables removed
!   4/08 Option to generate harmonic or Morse potentials added
!        and the default changed to harmonic
!   4/08 Conversion from kcal to eV removed where UFFtor or UFFd
!        are used since this duplicates the conversion on input
!   4/08 Higher bond order case also corrected to have harmonic
!        as the default bonded interaction
!   5/08 VDW potential corrected
!   5/08 isign corrected for lin3 potentials
!   5/08 Out of plane potentials added
!   5/08 Correction made for non-bonded potentials by setting scale14 = 1
!   5/08 OOP terms modified to be set from input
!   5/08 Theta0 ne 0 case modifed for c0, c1 & c2
!   5/08 Qualifier added so that out of plane potentials only apply to 
!        atoms with 3 bonds. 
!   5/08 New UFFchi array so that electronegativity values here can be
!        decoupled from values used for QEq that are built in.
!   5/08 Logic for checking whether higher bond orders should be included
!        corrected since it was only allowing one higher bond order.
!   5/08 Setting of bond types corrected for three-body terms with mixed
!        higher bond orders
!   6/08 Signs for special case angles set to match Forcite behaviour
!   6/08 Bug in calculation of force constant for higher bond order - single
!        three body terms corrected
!   6/08 Signs of linear 0 & 180 special three-body term reversed
!   6/08 Bond number controls on 3-body potentials added
!   6/08 Bond number controls now set to only apply to Bi and Po
!   6/08 Special handling of group 6 torsions added
!   7/08 Error in checking of atomic numbers for lbondnocheck fixed
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   6/09 Module name changed from three to m_three
!   3/10 loutofplane flag now set for UFF generated potentials
!   3/10 Arrays that flag rule generated potentials added
!   5/13 Bond order mapping to bond types now comes from array to allow
!        customisation.
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, May 2013
!
  use constants,       only : kcaltoev, degtorad
  use control,         only : keyword
  use current,         only : nat, nftype, ncf
  use element,         only : maxele
  use four
  use library,         only : libspec
  use m_three
  use molecule,        only : nconnect, n1connect, n2connect, nconnecttype
  use molecule,        only : nconnectcfg, rtol
  use species
  use two
  use uffdata
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: it1
  integer(i4)                                  :: it2
  integer(i4)                                  :: j
  integer(i4)                                  :: jt1
  integer(i4)                                  :: jt2
  integer(i4)                                  :: k
  integer(i4)                                  :: l
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: natk
  integer(i4)                                  :: newpots
  integer(i4)                                  :: nhigherbo
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl
  integer(i4)                                  :: nsp2
  integer(i4)                                  :: nsp3
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntodo
  integer(i4)                                  :: ntodouff1
  integer(i4)                                  :: ntodouff2
  integer(i4), dimension(:),   allocatable     :: nptr
  integer(i4), dimension(:),   allocatable     :: nptri
  integer(i4), dimension(:),   allocatable     :: nhigherbo1
  integer(i4), dimension(:),   allocatable     :: nhigherbo2
  integer(i4), dimension(:,:), allocatable     :: nhigherbotype
  integer(i4)                                  :: nuff
  integer(i4)                                  :: nuff1
  integer(i4)                                  :: nuff2
  integer(i4)                                  :: nv
  integer(i4)                                  :: nvalid
  integer(i4)                                  :: nvalidijk(3,4)
  integer(i4)                                  :: status
  logical                                      :: l0
  logical                                      :: l90
  logical                                      :: l120
  logical                                      :: l180
  logical                                      :: lbondnocheck
  logical                                      :: lfound
  logical                                      :: lfound1
  logical                                      :: lfound2
  logical                                      :: lgroup6j
  logical                                      :: lgroup6k
  logical                                      :: lmorse
  logical                                      :: lsym1
  logical                                      :: lsym2
  real(dp),                               save :: lambda = 0.1332_dp
  real(dp)                                     :: alphaij
  real(dp)                                     :: bo
  real(dp)                                     :: bo1
  real(dp)                                     :: bo2
  real(dp)                                     :: chii
  real(dp)                                     :: chij
  real(dp)                                     :: chik
  real(dp)                                     :: costheta0
  real(dp)                                     :: cos2theta0
  real(dp)                                     :: Di
  real(dp)                                     :: Dij
  real(dp)                                     :: Dj
  real(dp)                                     :: drbo
  real(dp)                                     :: dren
  real(dp)                                     :: Kij
  real(dp)                                     :: Kijk
  real(dp)                                     :: ri
  real(dp)                                     :: rij
  real(dp)                                     :: rik
  real(dp)                                     :: rj
  real(dp)                                     :: rjk
  real(dp)                                     :: rk
  real(dp)                                     :: Utorj
  real(dp)                                     :: Utork
  real(dp)                                     :: var1
  real(dp)                                     :: xi
  real(dp)                                     :: xij
  real(dp)                                     :: xij6
  real(dp)                                     :: xj
!
!  Allocate workspace arrays
!
  allocate(nptr(nspec),stat=status)
  if (status/=0) call outofmemory('setuff','nptr')
  allocate(nptri(nspec),stat=status)
  if (status/=0) call outofmemory('setuff','nptri')
  if (nconnect.gt.0) then
    allocate(nhigherbo1(nconnect),stat=status)
    if (status/=0) call outofmemory('setuff','nhigherbo1')
    allocate(nhigherbo2(nconnect),stat=status)
    if (status/=0) call outofmemory('setuff','nhigherbo2')
    allocate(nhigherbotype(2,nconnect),stat=status)
    if (status/=0) call outofmemory('setuff','nhigherbotype')
  endif
!
!  Set flag as to whether harmonic or Morse form is to be used
!
  lmorse = (index(keyword,'unmo').eq.1.or.index(keyword,' unmo').ne.0)
!
!  Zero counter for number of species to do
!
  ntodo = 0
!
!  Loop over species in structure and find which ones map to a UFF force field type
!
  do i = 1,nspec
    nati  = natspec(i)
    ntypi = ntypspec(i)
    nuff = 0
    lfound = .false.
    do while (.not.lfound.and.nuff.lt.nUFFspec) 
      nuff = nuff + 1
      lfound = (nati.eq.natUFFspec(nuff).and.(ntypi.eq.ntypUFFspec(nuff).or.ntypUFFspec(nuff).eq.0))
    enddo
    if (lfound) then
      ntodo = ntodo + 1
      nptr(ntodo) = nuff
      nptri(ntodo) = i
    endif
  enddo
!
!  Find if there are any higher bond order terms to be done - must be specified via explicit connectivity
!
  nhigherbo = 0
  if (nconnect.gt.0) then
    do ni = 1,nconnect
      if (nconnecttype(1,ni).gt.1.or.nconnecttype(2,ni).gt.1) then
!
!  Set up configuration
!
        ncf = nconnectcfg(ni)
        call setup(.false.)
!
!  Find atomic numbers & types
!
        nat1 = nat(n1connect(ni))
        ntyp1 = nftype(n1connect(ni))
        nat2 = nat(n2connect(ni))
        ntyp2 = nftype(n2connect(ni))
!
!  Find UFF species for these atoms
!
        ntodouff1 = 0
        lfound1 = .false.
        do while (.not.lfound1.and.ntodouff1.lt.ntodo) 
          ntodouff1 = ntodouff1 + 1
          nuff1 = nptr(ntodouff1)
          lfound1 = (nat1.eq.natUFFspec(nuff1).and.(ntyp1.eq.ntypUFFspec(nuff1).or.ntypUFFspec(nuff1).eq.0))
        enddo
        ntodouff2 = 0
        lfound2 = .false.
        do while (.not.lfound2.and.ntodouff2.lt.ntodo) 
          ntodouff2 = ntodouff2 + 1
          nuff2 = nptr(ntodouff2)
          lfound2 = (nat2.eq.natUFFspec(nuff2).and.(ntyp2.eq.ntypUFFspec(nuff2).or.ntypUFFspec(nuff2).eq.0))
        enddo
        if (lfound1.and.lfound2) then
          nuff1 = nptr(ntodouff1)
          nuff2 = nptr(ntodouff2)
!
!  Special bond type found - see if this has already been included and if not add to list
!
          nj = 0
          lfound = .false.
          do while (.not.lfound.and.nj.lt.nhigherbo) 
            nj = nj + 1
            lfound = ((nuff1.eq.nhigherbo1(nj).and.nuff2.eq.nhigherbo2(nj).or. &
                       nuff2.eq.nhigherbo1(nj).and.nuff1.eq.nhigherbo2(nj)).and. &
                       nhigherbotype(1,nj).eq.nconnecttype(1,ni).and. &
                       nhigherbotype(2,nj).eq.nconnecttype(2,ni))
          enddo
          if (.not.lfound) then
            nhigherbo = nhigherbo + 1
            nhigherbo1(nhigherbo)      = nuff1
            nhigherbo2(nhigherbo)      = nuff2
            nhigherbotype(1,nhigherbo) = nconnecttype(1,ni)
            nhigherbotype(2,nhigherbo) = nconnecttype(2,ni)
          endif
        endif
      endif
    enddo
  endif
!
!  Check that there is space in the arrays for the new two-body potentials
!
  newpots = ntodo*(ntodo + 1)/2 + nhigherbo
  if (npote+2*newpots.gt.maxpot) then
    maxpot = npote + 2*newpots
    call changemaxpot
  endif
!******************************************************************************
!  Loop over combinations of species to generate twobody potentials - bonded  *
!******************************************************************************
  do ni = 1,ntodo
    i = nptr(ni)
    do nj = ni,ntodo
      j = nptr(nj)
!
!  Compute sum of radii
!
      ri = UFFr(i)
      rj = UFFr(j)
      rij = ri + rj
!
!  Compute electronegativity correction
!
      chii = UFFchi(i)
      chij = UFFchi(j)
      dren = ri*rj*(sqrt(chii) - sqrt(chij))**2/(chii*ri + chij*rj)
      rij = rij - dren
!
!  Compute bond order correction
!
      bo = 1.0_dp
      drbo = - lambda*(ri + rj)*log(bo)
      rij = rij + drbo
!
!  Compute dissociation energy
!
      Dij = 70.0_dp
!
!  Compute harmonic force constant
!
      Kij = 664.12_dp*UFFZeff(i)*UFFZeff(j)/rij**3
!
!  Compute alpha for Morse potential 
!
      alphaij = sqrt(0.5_dp*Kij/Dij)
!
!  Add potential to arrays
!
      npote = npote + 1
      if (natUFFspec(i).eq.natUFFspec(j)) then
        nspec1(npote) = natUFFspec(i)
        nspec2(npote) = natUFFspec(j)
        if (ntypUFFspec(i).lt.ntypUFFspec(j)) then
          nptyp1(npote) = ntypUFFspec(i)
          nptyp2(npote) = ntypUFFspec(j)
        else
          nptyp1(npote) = ntypUFFspec(j)
          nptyp2(npote) = ntypUFFspec(i)
        endif
      elseif (natUFFspec(i).lt.natUFFspec(j)) then
        nspec1(npote) = natUFFspec(i) 
        nspec2(npote) = natUFFspec(j)
        nptyp1(npote) = ntypUFFspec(i)
        nptyp2(npote) = ntypUFFspec(j)
      else
        nspec1(npote) = natUFFspec(j)
        nspec2(npote) = natUFFspec(i)
        nptyp1(npote) = ntypUFFspec(j)
        nptyp2(npote) = ntypUFFspec(i)
      endif
      eshift(npote) = 0.0_dp
      gshift(npote) = 0.0_dp
      tpot(5,npote) = 0.0_dp
      mmexc(npote) = 1
      lgenerated2(npote) = .true.
      lintra(npote) = .true.
      linter(npote) = .false.
      n2botype(1,npote) = 1
      n2botype(2,npote) = 1
      scale14(npote) = 1.0_dp
      if (lmorse) then
!
!  Morse potential
!
        nptype(npote) = 3
        twopot(1,npote) = Dij*kcaltoev
        twopot(2,npote) = alphaij
        twopot(3,npote) = rij
      else
!
!  Harmonic potential
!
        nptype(npote) = 5
        twopot(1,npote) = Kij*kcaltoev
        twopot(2,npote) = rij
        twopot(3,npote) = 0.0_dp
      endif
      twopot(4,npote) = 0.0_dp
!
    enddo
  enddo
!
!  Special bond order combinations
!
  do ni = 1,nhigherbo
    i = nhigherbo1(ni)
    j = nhigherbo2(ni)
!
!  Compute sum of radii
!
    ri = UFFr(i)
    rj = UFFr(j)
    rij = ri + rj
!
!  Compute electronegativity correction
!
    chii = UFFchi(i)
    chij = UFFchi(j)
    dren = ri*rj*(sqrt(chii) - sqrt(chij))**2/(chii*ri + chij*rj)
    rij = rij - dren
!
!  Compute bond order correction
!
    bo = UFFbondorder(nhigherbotype(1,ni))
    drbo = - lambda*(ri + rj)*log(bo)
    rij = rij + drbo
!
!  Compute dissociation energy
!
    Dij = 70.0_dp
!
!  Compute harmonic force constant
!
    Kij = 664.12_dp*UFFZeff(i)*UFFZeff(j)/rij**3
!
!  Compute alpha for Morse potential 
!
    alphaij = sqrt(0.5_dp*Kij/Dij)
!
!  Add potential to arrays
!
    npote = npote + 1
    if (natUFFspec(i).eq.natUFFspec(j)) then
      nspec1(npote) = natUFFspec(i)
      nspec2(npote) = natUFFspec(j)
      if (ntypUFFspec(i).lt.ntypUFFspec(j)) then
        nptyp1(npote) = ntypUFFspec(i)
        nptyp2(npote) = ntypUFFspec(j)
      else
        nptyp1(npote) = ntypUFFspec(j)
        nptyp2(npote) = ntypUFFspec(i)
      endif
    elseif (natUFFspec(i).lt.natUFFspec(j)) then
      nspec1(npote) = natUFFspec(i) 
      nspec2(npote) = natUFFspec(j)
      nptyp1(npote) = ntypUFFspec(i)
      nptyp2(npote) = ntypUFFspec(j)
    else
      nspec1(npote) = natUFFspec(j)
      nspec2(npote) = natUFFspec(i)
      nptyp1(npote) = ntypUFFspec(j)
      nptyp2(npote) = ntypUFFspec(i)
    endif
    eshift(npote) = 0.0_dp
    gshift(npote) = 0.0_dp
    tpot(5,npote) = 0.0_dp
    mmexc(npote) = 1
    lgenerated2(npote) = .true.
    lintra(npote) = .true.
    linter(npote) = .false.
    n2botype(1,npote) = nhigherbotype(1,ni)
    n2botype(2,npote) = nhigherbotype(2,ni)
    scale14(npote) = 1.0_dp
    if (lmorse) then
!
!  Morse potential
!
      nptype(npote) = 3
      twopot(1,npote) = Dij*kcaltoev
      twopot(2,npote) = alphaij
      twopot(3,npote) = rij
    else
!
!  Harmonic potential
!
      nptype(npote) = 5
      twopot(1,npote) = Kij*kcaltoev
      twopot(2,npote) = rij
      twopot(3,npote) = 0.0_dp
    endif
    twopot(4,npote) = 0.0_dp
  enddo
!**********************************************************************************
!  Loop over combinations of species to generate twobody potentials - non-bonded  *
!**********************************************************************************
  do ni = 1,ntodo
    i = nptr(ni)
    do nj = ni,ntodo
      j = nptr(nj)
      xi = UFFx(i)
      xj = UFFx(j)
      Di = UFFd(i)
      Dj = UFFd(j)
!
!  Apply combination rules to atomic parameters
!
      Dij = sqrt(Di*Dj)
      xij = sqrt(xi*xj)
!
!  Add potential to arrays
!
      npote = npote + 1
      if (natUFFspec(i).eq.natUFFspec(j)) then
        nspec1(npote) = natUFFspec(i)
        nspec2(npote) = natUFFspec(j)
        if (ntypUFFspec(i).lt.ntypUFFspec(j)) then
          nptyp1(npote) = ntypUFFspec(i)
          nptyp2(npote) = ntypUFFspec(j)
        else
          nptyp1(npote) = ntypUFFspec(j)
          nptyp2(npote) = ntypUFFspec(i)
        endif
      elseif (natUFFspec(i).lt.natUFFspec(j)) then
        nspec1(npote) = natUFFspec(i) 
        nspec2(npote) = natUFFspec(j)
        nptyp1(npote) = ntypUFFspec(i)
        nptyp2(npote) = ntypUFFspec(j)
      else
        nspec1(npote) = natUFFspec(j)
        nspec2(npote) = natUFFspec(i)
        nptyp1(npote) = ntypUFFspec(j)
        nptyp2(npote) = ntypUFFspec(i)
      endif
      tpot(1,npote) = 12.0_dp
      tpot(2,npote) = 6.0_dp
      eshift(npote) = 0.0_dp
      gshift(npote) = 0.0_dp
      tpot(5,npote) = 0.0_dp
      mmexc(npote) = 3
      lgenerated2(npote) = .true.
      lintra(npote) = .true.
      linter(npote) = .true.
      n2botype(1,npote) = 0
      n2botype(2,npote) = 0
      scale14(npote) = 1.0_dp
      nptype(npote) = 2
!
      xij6 = xij**6
      twopot(1,npote) = Dij*xij6*xij6
      twopot(2,npote) = 2.0_dp*Dij*xij6
!
      rpot(npote) = 10.0_dp
      rpot2(npote) = 0.0_dp
!
    enddo
  enddo
!
!  Check that there is space in the arrays for the new threebody potentials
!
  newpots = ntodo*(ntodo*(ntodo+1)/2)
  if (nthb+2*newpots.gt.maxthb) then
    maxthb = nthb + 2*newpots
    call changemaxthb
  endif
!***********************************************************************
!  Loop over combinations of species to generate threebody potentials  *
!***********************************************************************
!
!  Loop over pivot atoms
!
  do ni = 1,ntodo
    i = nptr(ni)
    ri = UFFr(i)
    chii = UFFchi(i)
!
!  Compute constants based on equilibrium angle
!
    costheta0 = cos(UFFtheta(i)*degtorad)
    cos2theta0 = costheta0**2
!
!  Look for special case angle
!
    l0 = .false.
    l90 = .false.
    l120 = .false.
    l180 = .false.
    if (abs(UFFtheta(i)-0.0_dp).lt.1.0d-6) then
      l0 = .true.
    elseif (abs(UFFtheta(i)-90.0_dp).lt.1.0d-6) then
      l90 = .true.
    elseif (abs(UFFtheta(i)-120.0_dp).lt.1.0d-6) then
      l120 = .true.
    elseif (abs(UFFtheta(i)-180.0_dp).lt.1.0d-6) then
      l180 = .true.
    endif
!
!  Is bond check needed - only Bi and Po
!
    lbondnocheck = (natUFFspec(i).eq.83.or.natUFFspec(i).eq.84)
!
!  Loop over pairs of end atoms
!
    do nj = 1,ntodo
      j = nptr(nj)
      rj = UFFr(j)
      chij = UFFchi(j)
!
!  Compute sum of radii for i-j
!
      rij = ri + rj
!
!  Compute electronegativity correction for i-j
!
      dren = ri*rj*(sqrt(chii) - sqrt(chij))**2/(chii*ri + chij*rj)
      rij = rij - dren
!
      do nk = nj,ntodo
        k = nptr(nk)
        rk = UFFr(k)
        chik = UFFchi(k)
!
!  Compute sum of radii for i-k
!
        rik = ri + rk
!
!  Compute electronegativity correction for i-k
!
        dren = ri*rk*(sqrt(chii) - sqrt(chik))**2/(chii*ri + chik*rk)
        rik = rik - dren
!
!  Compute distance for j-k
!
        rjk = rij**2 + rik**2 - 2.0_dp*rij*rik*costheta0
        rjk = sqrt(rjk)
!
!  Compute force constant according to expression from paper
!
        Kijk = 664.12_dp*UFFZeff(j)*UFFZeff(k)/rjk**5
        Kijk = Kijk*(3.0_dp*rij*rik*(1.0_dp-cos2theta0) - costheta0*rjk**2)
!
!  Convert units from kcal to eV
!
        Kijk = Kijk*kcaltoev
!
!  Add new potential
!
        nthb = nthb + 1
!
        ntspec1(nthb) = natUFFspec(i)
        ntptyp1(nthb) = ntypUFFspec(i)
        if (natUFFspec(j).eq.natUFFspec(k)) then
          ntspec2(nthb) = natUFFspec(j)
          ntspec3(nthb) = natUFFspec(k)
          if (ntypUFFspec(j).lt.ntypUFFspec(k)) then
            ntptyp2(nthb) = ntypUFFspec(j)
            ntptyp3(nthb) = ntypUFFspec(k)
            thr1(nthb) = rij*1.6_dp*rtol
            thr2(nthb) = rik*1.6_dp*rtol
            thr3(nthb) = thr1(nthb) + thr2(nthb)
          else
            ntptyp2(nthb) = ntypUFFspec(k)
            ntptyp3(nthb) = ntypUFFspec(j)
            thr1(nthb) = rik*1.6_dp*rtol
            thr2(nthb) = rij*1.6_dp*rtol
            thr3(nthb) = thr1(nthb) + thr2(nthb)
            if (mmtexc(nthb).eq.0) then
              var1 = thr1(nthb)
              thr1(nthb) = thr2(nthb)
              thr2(nthb) = var1
            endif
          endif
        elseif (natUFFspec(j).lt.natUFFspec(k)) then
          ntspec2(nthb) = natUFFspec(j)
          ntspec3(nthb) = natUFFspec(k)
          ntptyp2(nthb) = ntypUFFspec(j)
          ntptyp3(nthb) = ntypUFFspec(k)
          thr1(nthb) = rij*1.6_dp*rtol
          thr2(nthb) = rik*1.6_dp*rtol
          thr3(nthb) = thr1(nthb) + thr2(nthb)
        else
          ntspec2(nthb) = natUFFspec(k)
          ntspec3(nthb) = natUFFspec(j)
          ntptyp2(nthb) = ntypUFFspec(k)
          ntptyp3(nthb) = ntypUFFspec(j)
          thr1(nthb) = rik*1.6_dp*rtol
          thr2(nthb) = rij*1.6_dp*rtol
          thr3(nthb) = thr1(nthb) + thr2(nthb)
          if (mmtexc(nthb).eq.0) then
            var1 = thr1(nthb)
            thr1(nthb) = thr2(nthb)
            thr2(nthb) = var1
          endif
        endif
!
        thrho1(nthb) = 0.0_dp
        thrho2(nthb) = 0.0_dp
        lgenerated3(nthb) = .true.
        lthetataper(nthb) = .false.
        mmtexc(nthb) = 1
        ltintra(nthb) = .true.
        ltinter(nthb) = .false.
        n3botype(1,1:2,nthb) = 1
        n3botype(2,1:2,nthb) = 1
!
!  Choose between special case and general forms based on angle
!
!  For special cases add check on bond number with general potential for case where this is not satisfied
!
        if (l90) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk/16.0_dp
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 4.0_dp
          if (lbondnocheck) then
            n3bondnono(1,nthb) = 2
            n3bondno(1,1,nthb) = 4
            n3bondno(2,1,nthb) = 6
!
            nthb = nthb + 1
            ntspec1(nthb) = ntspec1(nthb-1)
            ntspec2(nthb) = ntspec2(nthb-1)
            ntspec3(nthb) = ntspec3(nthb-1)
            ntptyp1(nthb) = ntptyp1(nthb-1)
            ntptyp2(nthb) = ntptyp2(nthb-1)
            ntptyp3(nthb) = ntptyp3(nthb-1)
            thr1(nthb) = thr1(nthb-1)
            thr2(nthb) = thr2(nthb-1)
            thr3(nthb) = thr3(nthb-1)
            thrho1(nthb) = 0.0_dp
            thrho2(nthb) = 0.0_dp
            lgenerated3(nthb) = .true.
            lthetataper(nthb) = .false.
            mmtexc(nthb) = 1
            ltintra(nthb) = .true.
            ltinter(nthb) = .false.
            n3botype(1:2,1:2,nthb) = 1
            nthrty(nthb) = 17
            thbk(nthb) = Kijk
            theta(nthb) = UFFtheta(i)
            n3bondnono(2,nthb) = 2
            n3bondno(1,2,nthb) = 4
            n3bondno(2,2,nthb) = 6
          endif
        elseif (l120) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk/9.0_dp
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 3.0_dp
        elseif (l180) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk
          theta(nthb) = 1.0_dp
          thrho1(nthb) = 1.0_dp
        elseif (l0) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 1.0_dp
        else
!
!  General
!
          nthrty(nthb) = 17
          thbk(nthb) = Kijk
          theta(nthb) = UFFtheta(i)
        endif
      enddo
    enddo
  enddo
!
!  Check that there is space in the arrays for the new threebody potentials
!
  newpots = (4*nhigherbo*(nhigherbo+1)/2)
  if (nthb+newpots.gt.maxthb) then
    maxthb = nthb + newpots
    call changemaxthb
  endif
!************************************************************************************************
!  Loop over combinations of species to generate threebody potentials - higher bond order case  *
!************************************************************************************************
!
!  Loop over first higher order bond
!
  do ni = 1,nhigherbo
!
!  Loop over second higher order bond
!
    do nj = 1,ni
!
!  Do the bonds have a common atom? If not there is nothing to do
!
      it1 = nhigherbo1(ni)
      jt1 = nhigherbo2(ni)
      it2 = nhigherbo1(nj)
      jt2 = nhigherbo2(nj)
      lsym1 = (it1.eq.jt1)
      lsym2 = (it2.eq.jt2)
!
!  Find valid permuations
!
      nvalid = 0
      if (it1.eq.it2) then
        nvalid = nvalid + 1
        nvalidijk(1,nvalid) = it1
        nvalidijk(2,nvalid) = jt1
        nvalidijk(3,nvalid) = jt2
      endif
      if (lsym1.and..not.lsym2) then
        if (it1.eq.jt2) then
          nvalid = nvalid + 1
          nvalidijk(1,nvalid) = it1
          nvalidijk(2,nvalid) = jt1
          nvalidijk(3,nvalid) = it2
        endif
      elseif (lsym2.and..not.lsym1) then
        if (it2.eq.jt1) then
          nvalid = nvalid + 1
          nvalidijk(1,nvalid) = it2
          nvalidijk(2,nvalid) = it1
          nvalidijk(3,nvalid) = jt2
        endif
      elseif (.not.lsym1.and..not.lsym2) then
        if (it1.eq.jt2) then
          nvalid = nvalid + 1
          nvalidijk(1,nvalid) = it1
          nvalidijk(2,nvalid) = jt1
          nvalidijk(3,nvalid) = it2
        endif
        if (jt1.eq.it2) then
          nvalid = nvalid + 1
          nvalidijk(1,nvalid) = jt1
          nvalidijk(2,nvalid) = it1
          nvalidijk(3,nvalid) = jt2
        endif
        if (jt1.eq.jt2) then
          nvalid = nvalid + 1
          nvalidijk(1,nvalid) = jt1
          nvalidijk(2,nvalid) = it1
          nvalidijk(3,nvalid) = it2
        endif
      endif
!
!  If there are valid terms then set bond order correction
!
      if (nvalid.gt.0) then
        bo1 = UFFbondorder(nhigherbotype(1,ni))
        bo2 = UFFbondorder(nhigherbotype(1,nj))
      endif
!
!  Loop over valid permuations of bond connections
!
      do nv = 1,nvalid
        i = nvalidijk(1,nv)
        j = nvalidijk(2,nv)
        k = nvalidijk(3,nv)
!
        ri = UFFr(i)
        rj = UFFr(j)
        rk = UFFr(k)
!
        chii = UFFchi(i)
        chij = UFFchi(j)
        chik = UFFchi(k)
!
!  Compute constants based on equilibrium angle
!
        costheta0 = cos(UFFtheta(i)*degtorad)
        cos2theta0 = costheta0**2
!
!  Look for special case angle
!
        l0 = .false.
        l90 = .false.
        l120 = .false.
        l180 = .false.
        if (abs(UFFtheta(i)-0.0_dp).lt.1.0d-6) then
          l0 = .true.
        elseif (abs(UFFtheta(i)-90.0_dp).lt.1.0d-6) then
          l90 = .true.
        elseif (abs(UFFtheta(i)-120.0_dp).lt.1.0d-6) then
          l120 = .true.
        elseif (abs(UFFtheta(i)-180.0_dp).lt.1.0d-6) then
          l180 = .true.
        endif
!
!  Is bond check needed - only Bi and Po
!
        lbondnocheck = (nat(i).eq.83.or.nat(i).eq.84)
!
!  Compute sum of radii for i-j
!
        rij = ri + rj
!
!  Compute electronegativity correction for i-j
!
        dren = ri*rj*(sqrt(chii) - sqrt(chij))**2/(chii*ri + chij*rj)
        rij = rij - dren
!         
!  Compute bond order correction for first bond
!     
        drbo = - lambda*(ri + rj)*log(bo1)
        rij = rij + drbo
!
!  Compute sum of radii for i-k
!
        rik = ri + rk
!
!  Compute electronegativity correction for i-k
!
        dren = ri*rk*(sqrt(chii) - sqrt(chik))**2/(chii*ri + chik*rk)
        rik = rik - dren
!         
!  Compute bond order correction for second bond
!     
        drbo = - lambda*(ri + rk)*log(bo2)
        rik = rik + drbo
!
!  Compute distance for j-k
!
        rjk = rij**2 + rik**2 - 2.0_dp*rij*rik*costheta0
        rjk = sqrt(rjk)
!
!  Compute force constant according to expression from paper
!
        Kijk = 664.12*UFFZeff(j)*UFFZeff(k)/rjk**5
        Kijk = Kijk*(3.0_dp*rij*rik*(1.0_dp-cos2theta0) - costheta0*rjk**2)
!
!  Convert units from kcal to eV
!
        Kijk = Kijk*kcaltoev
!
!  Add new potential
!
        nthb = nthb + 1
!
        ntspec1(nthb) = natUFFspec(i)
        ntptyp1(nthb) = ntypUFFspec(i)
        if (natUFFspec(j).eq.natUFFspec(k)) then
          ntspec2(nthb) = natUFFspec(j)
          ntspec3(nthb) = natUFFspec(k)
          if (ntypUFFspec(j).lt.ntypUFFspec(k)) then
            ntptyp2(nthb) = ntypUFFspec(j)
            ntptyp3(nthb) = ntypUFFspec(k)
            thr1(nthb) = rij*1.6_dp*rtol
            thr2(nthb) = rik*1.6_dp*rtol
            thr3(nthb) = thr1(nthb) + thr2(nthb)
            n3botype(1,1,nthb) = nhigherbotype(1,ni)
            n3botype(2,1,nthb) = nhigherbotype(2,ni)
            n3botype(1,2,nthb) = nhigherbotype(1,nj)
            n3botype(2,2,nthb) = nhigherbotype(2,nj)
          else
            ntptyp2(nthb) = ntypUFFspec(k)
            ntptyp3(nthb) = ntypUFFspec(j)
            thr1(nthb) = rik*1.6_dp*rtol
            thr2(nthb) = rij*1.6_dp*rtol
            thr3(nthb) = thr1(nthb) + thr2(nthb)
            if (mmtexc(nthb).eq.0) then
              var1 = thr1(nthb)
              thr1(nthb) = thr2(nthb)
              thr2(nthb) = var1
            endif
            n3botype(1,1,nthb) = nhigherbotype(1,nj)
            n3botype(2,1,nthb) = nhigherbotype(2,nj)
            n3botype(1,2,nthb) = nhigherbotype(1,ni)
            n3botype(2,2,nthb) = nhigherbotype(2,ni)
          endif
        elseif (natUFFspec(j).lt.natUFFspec(k)) then
          ntspec2(nthb) = natUFFspec(j)
          ntspec3(nthb) = natUFFspec(k)
          ntptyp2(nthb) = ntypUFFspec(j)
          ntptyp3(nthb) = ntypUFFspec(k)
          thr1(nthb) = rij*1.6_dp*rtol
          thr2(nthb) = rik*1.6_dp*rtol
          thr3(nthb) = thr1(nthb) + thr2(nthb)
          n3botype(1,1,nthb) = nhigherbotype(1,ni)
          n3botype(2,1,nthb) = nhigherbotype(2,ni)
          n3botype(1,2,nthb) = nhigherbotype(1,nj)
          n3botype(2,2,nthb) = nhigherbotype(2,nj)
        else
          ntspec2(nthb) = natUFFspec(k)
          ntspec3(nthb) = natUFFspec(j)
          ntptyp2(nthb) = ntypUFFspec(k)
          ntptyp3(nthb) = ntypUFFspec(j)
          thr1(nthb) = rik*1.6_dp*rtol
          thr2(nthb) = rij*1.6_dp*rtol
          thr3(nthb) = thr1(nthb) + thr2(nthb)
          if (mmtexc(nthb).eq.0) then
            var1 = thr1(nthb)
            thr1(nthb) = thr2(nthb)
            thr2(nthb) = var1
          endif
          n3botype(1,1,nthb) = nhigherbotype(1,nj)
          n3botype(2,1,nthb) = nhigherbotype(2,nj)
          n3botype(1,2,nthb) = nhigherbotype(1,ni)
          n3botype(2,2,nthb) = nhigherbotype(2,ni)
        endif
!
        thrho1(nthb) = 0.0_dp
        thrho2(nthb) = 0.0_dp
        lgenerated3(nthb) = .true.
        lthetataper(nthb) = .false.
        mmtexc(nthb) = 1
        ltintra(nthb) = .true.
        ltinter(nthb) = .false.
!
!  Chose between special case and general forms based on angle
!
        if (l90) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk/16.0_dp
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 4.0_dp
          if (lbondnocheck) then
            n3bondnono(1,nthb) = 2
            n3bondno(1,1,nthb) = 4
            n3bondno(2,1,nthb) = 6
!         
            nthb = nthb + 1
            ntspec1(nthb) = ntspec1(nthb-1)
            ntspec2(nthb) = ntspec2(nthb-1)
            ntspec3(nthb) = ntspec3(nthb-1)
            ntptyp1(nthb) = ntptyp1(nthb-1)
            ntptyp2(nthb) = ntptyp2(nthb-1)
            ntptyp3(nthb) = ntptyp3(nthb-1)
            thr1(nthb) = thr1(nthb-1)
            thr2(nthb) = thr2(nthb-1)
            thr3(nthb) = thr3(nthb-1)
            thrho1(nthb) = 0.0_dp
            thrho2(nthb) = 0.0_dp
            lgenerated3(nthb) = .true.
            lthetataper(nthb) = .false.
            mmtexc(nthb) = 1
            ltintra(nthb) = .true.
            ltinter(nthb) = .false.
            n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,nthb-1)
            nthrty(nthb) = 17
            thbk(nthb) = Kijk
            theta(nthb) = UFFtheta(i)
            n3bondnono(2,nthb) = 2
            n3bondno(1,2,nthb) = 4
            n3bondno(2,2,nthb) = 6
          endif
        elseif (l120) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk/9.0_dp
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 3.0_dp
        elseif (l180) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk
          theta(nthb) =  1.0_dp
          thrho1(nthb) = 1.0_dp
        elseif (l0) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 1.0_dp
        else
!
!  General
!
          nthrty(nthb) = 17
          thbk(nthb) = Kijk
          theta(nthb) = UFFtheta(i)
        endif
      enddo
    enddo
  enddo
!
!  Check that there is space in the arrays for the new threebody potentials
!
  newpots = (2*nhigherbo*ntodo)
  if (nthb+newpots.gt.maxthb) then
    maxthb = nthb + newpots
    call changemaxthb
  endif
!***********************************************************************************************************
!  Loop over combinations of species to generate threebody potentials - higher bond order / regular mixed  *
!***********************************************************************************************************
!
!  Loop over higher order bond
!
  do ni = 1,nhigherbo
!
!  Set properties of i-j bond
!
    it1 = nhigherbo1(ni)
    jt1 = nhigherbo2(ni)
    lsym1 = (it1.eq.jt1)
    bo1 = UFFbondorder(nhigherbotype(1,ni))
!
!  Find valid permuations
!
    nvalid = 1
    nvalidijk(1,nvalid) = it1
    nvalidijk(2,nvalid) = jt1
    if (.not.lsym1) then
      nvalid = nvalid + 1
      nvalidijk(1,nvalid) = jt1
      nvalidijk(2,nvalid) = it1
    endif
!
    ri = UFFr(it1)
    rj = UFFr(jt1)
!
    chii = UFFchi(it1)
    chij = UFFchi(jt1)
!
!  Compute sum of radii for i-j
!
    rij = ri + rj
!
!  Compute electronegativity correction for i-j
!
    dren = ri*rj*(sqrt(chii) - sqrt(chij))**2/(chii*ri + chij*rj)
    rij = rij - dren
!         
!  Compute bond order correction for first bond
!     
    drbo = - lambda*(ri + rj)*log(bo1)
    rij = rij + drbo
!
!  Loop over further end species
!
    do nj = 1,ntodo
      k = nptr(nj)
      rk = UFFr(k)
      chik = UFFchi(k)
!
!  Loop over valid permuations of bond connections
!
      do nv = 1,nvalid
        i = nvalidijk(1,nv)
        j = nvalidijk(2,nv)
!
        ri = UFFr(i)
!
        chii = UFFchi(i)
!
!  Compute constants based on equilibrium angle
!
        costheta0 = cos(UFFtheta(i)*degtorad)
        cos2theta0 = costheta0**2
!
!  Look for special case angle
!
        l0 = .false.
        l90 = .false.
        l120 = .false.
        l180 = .false.
        if (abs(UFFtheta(i)-0.0_dp).lt.1.0d-6) then
          l0 = .true.
        elseif (abs(UFFtheta(i)-90.0_dp).lt.1.0d-6) then
          l90 = .true.
        elseif (abs(UFFtheta(i)-120.0_dp).lt.1.0d-6) then
          l120 = .true.
        elseif (abs(UFFtheta(i)-180.0_dp).lt.1.0d-6) then
          l180 = .true.
        endif
!
!  Is bond check needed - only Bi and Po
!
        lbondnocheck = (nat(i).eq.83.or.nat(i).eq.84)
!
!  Compute sum of radii for i-k
!
        rik = ri + rk
!
!  Compute electronegativity correction for i-k
!
        dren = ri*rk*(sqrt(chii) - sqrt(chik))**2/(chii*ri + chik*rk)
        rik = rik - dren
!
!  Compute distance for j-k
!
        rjk = rij**2 + rik**2 - 2.0_dp*rij*rik*costheta0
        rjk = sqrt(rjk)
!
!  Compute force constant according to expression from paper
!
        Kijk = 664.12_dp*UFFZeff(j)*UFFZeff(k)/rjk**5
        Kijk = Kijk*(3.0_dp*rij*rik*(1.0_dp-cos2theta0) - costheta0*rjk**2)
!
!  Convert units from kcal to eV
!
        Kijk = Kijk*kcaltoev
!
!  Add new potential
!
        nthb = nthb + 1
!
        thr1(nthb) = rij*1.6_dp*rtol
        thr2(nthb) = rik*1.6_dp*rtol
        thr3(nthb) = thr1(nthb) + thr2(nthb)
!
        ntspec1(nthb) = natUFFspec(i)
        ntptyp1(nthb) = ntypUFFspec(i)
        if (natUFFspec(j).eq.natUFFspec(k)) then
          ntspec2(nthb) = natUFFspec(j)
          ntspec3(nthb) = natUFFspec(k)
          if (ntypUFFspec(j).lt.ntypUFFspec(k)) then
            ntptyp2(nthb) = ntypUFFspec(j)
            ntptyp3(nthb) = ntypUFFspec(k)
            n3botype(1,1,nthb) = nhigherbotype(1,ni)
            n3botype(2,1,nthb) = nhigherbotype(2,ni)
            n3botype(1,2,nthb) = 1
            n3botype(2,2,nthb) = 1
          else
            ntptyp2(nthb) = ntypUFFspec(k)
            ntptyp3(nthb) = ntypUFFspec(j)
            n3botype(1,2,nthb) = nhigherbotype(1,ni)
            n3botype(2,2,nthb) = nhigherbotype(2,ni)
            n3botype(1,1,nthb) = 1
            n3botype(2,1,nthb) = 1
            if (mmtexc(nthb).eq.0) then
              var1 = thr1(nthb)
              thr1(nthb) = thr2(nthb)
              thr2(nthb) = var1
            endif
          endif
        elseif (natUFFspec(j).lt.natUFFspec(k)) then
          ntspec2(nthb) = natUFFspec(j)
          ntspec3(nthb) = natUFFspec(k)
          ntptyp2(nthb) = ntypUFFspec(j)
          ntptyp3(nthb) = ntypUFFspec(k)
          n3botype(1,1,nthb) = nhigherbotype(1,ni)
          n3botype(2,1,nthb) = nhigherbotype(2,ni)
          n3botype(1,2,nthb) = 1
          n3botype(2,2,nthb) = 1
        else
          ntspec2(nthb) = natUFFspec(k)
          ntspec3(nthb) = natUFFspec(j)
          ntptyp2(nthb) = ntypUFFspec(k)
          ntptyp3(nthb) = ntypUFFspec(j)
          n3botype(1,2,nthb) = nhigherbotype(1,ni)
          n3botype(2,2,nthb) = nhigherbotype(2,ni)
          n3botype(1,1,nthb) = 1
          n3botype(2,1,nthb) = 1
          if (mmtexc(nthb).eq.0) then
            var1 = thr1(nthb)
            thr1(nthb) = thr2(nthb)
            thr2(nthb) = var1
          endif
        endif
!
        thrho1(nthb) = 0.0_dp
        thrho2(nthb) = 0.0_dp
        lgenerated3(nthb) = .true.
        lthetataper(nthb) = .false.
        mmtexc(nthb) = 1
        ltintra(nthb) = .true.
        ltinter(nthb) = .false.
!
!  Chose between special case and general forms based on angle
!
        if (l90) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk/16.0_dp
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 4.0_dp
          if (lbondnocheck) then
            n3bondnono(1,nthb) = 2
            n3bondno(1,1,nthb) = 4
            n3bondno(2,1,nthb) = 6
!         
            nthb = nthb + 1
            ntspec1(nthb) = ntspec1(nthb-1)
            ntspec2(nthb) = ntspec2(nthb-1)
            ntspec3(nthb) = ntspec3(nthb-1)
            ntptyp1(nthb) = ntptyp1(nthb-1)
            ntptyp2(nthb) = ntptyp2(nthb-1)
            ntptyp3(nthb) = ntptyp3(nthb-1)
            thr1(nthb) = thr1(nthb-1)
            thr2(nthb) = thr2(nthb-1)
            thr3(nthb) = thr3(nthb-1)
            thrho1(nthb) = 0.0_dp
            thrho2(nthb) = 0.0_dp
            lgenerated3(nthb) = .true.
            lthetataper(nthb) = .false.
            mmtexc(nthb) = 1
            ltintra(nthb) = .true.
            ltinter(nthb) = .false.
            n3botype(1:2,1:2,nthb) = n3botype(1:2,1:2,nthb-1)
            nthrty(nthb) = 17
            thbk(nthb) = Kijk
            theta(nthb) = UFFtheta(i)
            n3bondnono(2,nthb) = 2
            n3bondno(1,2,nthb) = 4
            n3bondno(2,2,nthb) = 6
          endif
        elseif (l120) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk/9.0_dp
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 3.0_dp
        elseif (l180) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk
          theta(nthb) = 1.0_dp
          thrho1(nthb) = 1.0_dp
        elseif (l0) then
          nthrty(nthb) = 12
          thbk(nthb) = Kijk
          theta(nthb) = -1.0_dp
          thrho1(nthb) = 1.0_dp
        else
!
!  General
!
          nthrty(nthb) = 17
          thbk(nthb) = Kijk
          theta(nthb) = UFFtheta(i)
        endif
      enddo
    enddo
  enddo
!
!  Count number of sp2 and sp3 centres to use in estimate of number of potentials
!
  nsp2 = 0
  nsp3 = 0
  do ni = 1,ntodo
    i = nptr(ni)
    if (nUFFtype(i).eq.2) then
      nsp2 = nsp2 + 1
    elseif (nUFFtype(i).eq.3) then
      nsp3 = nsp3 + 1
    endif
  enddo
!
!  Check that there is space in the arrays for the new fourbody potentials
!
  newpots = ntodo*(ntodo+1)/2 + nsp2*nsp3*(ntodo-1)
  if (nfor+newpots.gt.maxfor) then
    maxfor = nfor + newpots
    call changemaxfor
  endif
!**********************************************************************
!  Loop over combinations of species to generate fourbody potentials  *
!**********************************************************************
!
!  Loop over pairs of central atoms : j & k
!
  do nj = 1,ntodo
    j = nptr(nj)
    natj = natUFFspec(j)
    lgroup6j = (natj.eq.8.or.natj.eq.16.or.natj.eq.34.or.natj.eq.52.or.natj.eq.84)
    if (natj.le.10) then
      Utorj = 2.0_dp
    elseif (natj.le.18) then
      Utorj = 1.25_dp
    elseif (natj.le.36) then
      Utorj = 0.7_dp
    elseif (natj.le.54) then
      Utorj = 0.2_dp
    else
      Utorj = 0.1_dp
    endif
!
!  Check whether j is sp2 or sp3
!
    if (nUFFtype(j).eq.2.or.nUFFtype(j).eq.3) then
      do nk = 1,nj
        k = nptr(nk)
        natk = natUFFspec(k)
        lgroup6k = (natk.eq.8.or.natk.eq.16.or.natk.eq.34.or.natk.eq.52.or.natk.eq.84)
        if (natk.le.10) then
          Utork = 2.0_dp
        elseif (natk.le.18) then
          Utork = 1.25_dp
        elseif (natk.le.36) then
          Utork = 0.7_dp
        elseif (natk.le.54) then
          Utork = 0.2_dp
        else
          Utork = 0.1_dp
        endif
!
!  Check whether k is sp2 or sp3
!
        if (nUFFtype(k).eq.2.or.nUFFtype(k).eq.3) then
          if (nUFFtype(j).eq.3.and.nUFFtype(k).eq.3) then
!
!  j = sp3 / k = sp3 => i = X & k = X
!
            nfor = nfor + 1
            if (lgroup6j.and.lgroup6k) then
              if (natj.eq.8.and.natk.eq.8) then
                fork(nfor) = 2.0_dp*kcaltoev
              elseif (natj.eq.8.and.natk.ne.8) then
                fork(nfor) = sqrt(2.0_dp*6.8_dp)*kcaltoev
              elseif (natj.ne.8.and.natk.eq.8) then
                fork(nfor) = sqrt(2.0_dp*6.8_dp)*kcaltoev
              else
                fork(nfor) = 6.8_dp*kcaltoev
              endif
              npfor(nfor) = 2
              forpoly(1,nfor) = 90.0_dp
            else
              fork(nfor) = sqrt(UFFtor(j)*UFFtor(k))
              npfor(nfor) = 3
              forpoly(1,nfor) = 180.0_dp
            endif
            nforty(nfor) = 13
            mmfexc(nfor) = 1
            lgenerated4(nfor) = .true.
            lfintra(nfor) = .true.
            lfinter(nfor) = .false.
            lfdreiding(nfor) = .true.
            n4botype(1,nfor) = 1
            n4botype(2,nfor) = 1
            nfspec1(nfor) = maxele
            nfptyp1(nfor) = 0
            nfspec2(nfor) = natUFFspec(j)
            nfptyp2(nfor) = ntypUFFspec(j)
            nfspec3(nfor) = natUFFspec(k)
            nfptyp3(nfor) = ntypUFFspec(k)
            nfspec4(nfor) = maxele
            nfptyp4(nfor) = 0
          elseif (nUFFtype(j).eq.2.and.nUFFtype(k).eq.3) then
!
!  j = sp2 / k = sp3 => i = X (for group 6), species for other cases & k = X
!
            if (lgroup6k) then
!
!  Bond order correction to line below not needed since bo = 1 => log(bo) = 0
!
              nfor = nfor + 1
              fork(nfor) = 5.0_dp*sqrt(Utorj*Utork)*kcaltoev
              npfor(nfor) = 2
              forpoly(1,nfor) = 180.0_dp
              nforty(nfor) = 13
              mmfexc(nfor) = 1
              lgenerated4(nfor) = .true.
              lfintra(nfor) = .true.
              lfinter(nfor) = .false.
              lfdreiding(nfor) = .true.
              n4botype(1,nfor) = 1
              n4botype(2,nfor) = 1
              nfspec1(nfor) = maxele
              nfptyp1(nfor) = 0
              nfspec2(nfor) = natUFFspec(j)
              nfptyp2(nfor) = ntypUFFspec(j)
              nfspec3(nfor) = natUFFspec(k)
              nfptyp3(nfor) = ntypUFFspec(k)
              nfspec4(nfor) = maxele
              nfptyp4(nfor) = 0
            else
              do ni = 1,ntodo
                i = nptr(ni)
                nfor = nfor + 1
                if (nUFFtype(i).eq.2) then
                  fork(nfor) = 1.0_dp*kcaltoev
                  npfor(nfor) = 6
                  forpoly(1,nfor) = 0.0_dp
                else
                  fork(nfor) = 2.0_dp*kcaltoev
                  npfor(nfor) = 3
                  forpoly(1,nfor) = 180.0_dp
                endif
                nforty(nfor) = 13
                mmfexc(nfor) = 1
                lgenerated4(nfor) = .true.
                lfintra(nfor) = .true.
                lfinter(nfor) = .false.
                lfdreiding(nfor) = .true.
                n4botype(1,nfor) = 1
                n4botype(2,nfor) = 1
                nfspec1(nfor) = natUFFspec(i)
                nfptyp1(nfor) = ntypUFFspec(i)
                nfspec2(nfor) = natUFFspec(j)
                nfptyp2(nfor) = ntypUFFspec(j)
                nfspec3(nfor) = natUFFspec(k)
                nfptyp3(nfor) = ntypUFFspec(k)
                nfspec4(nfor) = maxele
                nfptyp4(nfor) = 0
              enddo
            endif
          elseif (nUFFtype(j).eq.3.and.nUFFtype(k).eq.2) then
!
!  j = sp3 / k = sp2 => i = X & k = X (for group 6), species for other cases
!
            if (lgroup6j) then
!
!  Bond order correction to line below not needed since bo = 1 => log(bo) = 0
!
              nfor = nfor + 1
              fork(nfor) = 5.0_dp*sqrt(Utorj*Utork)*kcaltoev
              npfor(nfor) = 2
              forpoly(1,nfor) = 180.0_dp
              nforty(nfor) = 13
              mmfexc(nfor) = 1
              lgenerated4(nfor) = .true.
              lfintra(nfor) = .true.
              lfinter(nfor) = .false.
              lfdreiding(nfor) = .true.
              n4botype(1,nfor) = 1
              n4botype(2,nfor) = 1
              nfspec1(nfor) = maxele
              nfptyp1(nfor) = 0
              nfspec2(nfor) = natUFFspec(j)
              nfptyp2(nfor) = ntypUFFspec(j)
              nfspec3(nfor) = natUFFspec(k)
              nfptyp3(nfor) = ntypUFFspec(k)
              nfspec4(nfor) = maxele
              nfptyp4(nfor) = 0
            else
              do nl = 1,ntodo
                l = nptr(nl)
                nfor = nfor + 1
                if (nUFFtype(l).eq.2) then
                  fork(nfor) = 1.0_dp*kcaltoev
                  npfor(nfor) = 6
                  forpoly(1,nfor) = 0.0_dp
                else
                  fork(nfor) = 2.0_dp*kcaltoev
                  npfor(nfor) = 3
                  forpoly(1,nfor) = 180.0_dp
                endif
                nforty(nfor) = 13
                lgenerated4(nfor) = .true.
                mmfexc(nfor) = 1
                lfintra(nfor) = .true.
                lfinter(nfor) = .false.
                lfdreiding(nfor) = .true.
                n4botype(1,nfor) = 1
                n4botype(2,nfor) = 1
                nfspec1(nfor) = maxele
                nfptyp1(nfor) = 0
                nfspec2(nfor) = natUFFspec(j)
                nfptyp2(nfor) = ntypUFFspec(j)
                nfspec3(nfor) = natUFFspec(k)
                nfptyp3(nfor) = ntypUFFspec(k)
                nfspec4(nfor) = natUFFspec(l)
                nfptyp4(nfor) = ntypUFFspec(l)
              enddo
            endif
          elseif (nUFFtype(j).eq.2.and.nUFFtype(k).eq.2) then
!
!  j = sp2 / k = sp2 => i = X & k = X
!
            nfor = nfor + 1
            nforty(nfor) = 13
            mmfexc(nfor) = 1
! Single bond order
            bo = 1.0_dp
            lgenerated4(nfor) = .true.
            lfintra(nfor) = .true.
            lfinter(nfor) = .false.
            lfdreiding(nfor) = .true.
            n4botype(1,nfor) = 1
            n4botype(2,nfor) = 1
            fork(nfor) = 5.0_dp*sqrt(UFFtor(j)*UFFtor(k))*(1.0_dp+4.18_dp*log(bo))
            npfor(nfor) = 2
            forpoly(1,nfor) = 180.0_dp
            nfspec1(nfor) = maxele
            nfptyp1(nfor) = 0
            nfspec2(nfor) = natUFFspec(j)
            nfptyp2(nfor) = ntypUFFspec(j)
            nfspec3(nfor) = natUFFspec(k)
            nfptyp3(nfor) = ntypUFFspec(k)
            nfspec4(nfor) = maxele
            nfptyp4(nfor) = 0
          endif
        endif
      enddo
    endif
  enddo
!
!  Check that there is space in the arrays for the new fourbody potentials
!
  newpots = nhigherbo
  if (nfor+newpots.gt.maxfor) then
    maxfor = nfor + newpots
    call changemaxfor
  endif
!**************************************************************************************************
!  Loop over combinations of species to generate fourbody potentials involving higher bondorders  *
!**************************************************************************************************
!
!  Loop over pairs of central atoms : j & k given higher bond orders
!
  do ni = 1,nhigherbo
    j = nhigherbo1(ni)
    k = nhigherbo2(ni)
!
!  Check whether j and k are sp2 or sp3
!
    if ((nUFFtype(j).eq.2.or.nUFFtype(j).eq.3).and.(nUFFtype(k).eq.2.or.nUFFtype(k).eq.3)) then
      natj = natUFFspec(j)
      lgroup6j = (natj.eq.8.or.natj.eq.16.or.natj.eq.34.or.natj.eq.52.or.natj.eq.84)
      if (natj.le.10) then
        Utorj = 2.0_dp
      elseif (natj.le.18) then
        Utorj = 1.25_dp
      elseif (natj.le.36) then
        Utorj = 0.7_dp
      elseif (natj.le.54) then
        Utorj = 0.2_dp
      else
        Utorj = 0.1_dp
      endif
      natk = natUFFspec(k)
      lgroup6k = (natk.eq.8.or.natk.eq.16.or.natk.eq.34.or.natk.eq.52.or.natk.eq.84)
      if (natk.le.10) then
        Utork = 2.0_dp
      elseif (natk.le.18) then
        Utork = 1.25_dp
      elseif (natk.le.36) then
        Utork = 0.7_dp
      elseif (natk.le.54) then
        Utork = 0.2_dp
      else
        Utork = 0.1_dp
      endif
      bo = UFFbondorder(nhigherbotype(1,ni))
!
      if (nUFFtype(j).eq.3.and.nUFFtype(k).eq.3) then
!
!  j = sp3 / k = sp3 => i = X & k = X
!
        nfor = nfor + 1
        if (lgroup6j.and.lgroup6k) then
          if (natj.eq.8.and.natk.eq.8) then
            fork(nfor) = 2.0_dp*kcaltoev
          elseif (natj.eq.8.and.natk.ne.8) then
            fork(nfor) = sqrt(2.0_dp*6.8_dp)*kcaltoev
          elseif (natj.ne.8.and.natk.eq.8) then
            fork(nfor) = sqrt(2.0_dp*6.8_dp)*kcaltoev
          else
            fork(nfor) = 6.8_dp*kcaltoev
          endif
          npfor(nfor) = 2
          forpoly(1,nfor) = 90.0_dp
        else
          fork(nfor) = sqrt(UFFtor(j)*UFFtor(k))
          npfor(nfor) = 3
          forpoly(1,nfor) = 180.0_dp
        endif
        nforty(nfor) = 13
        mmfexc(nfor) = 1
        lgenerated4(nfor) = .true.
        lfintra(nfor) = .true.
        lfinter(nfor) = .false.
        lfdreiding(nfor) = .true.
        n4botype(1,nfor) = nhigherbotype(1,ni)
        n4botype(2,nfor) = nhigherbotype(2,ni)
        nfspec1(nfor) = maxele
        nfptyp1(nfor) = 0
        nfspec2(nfor) = natUFFspec(j)
        nfptyp2(nfor) = ntypUFFspec(j)
        nfspec3(nfor) = natUFFspec(k)
        nfptyp3(nfor) = ntypUFFspec(k)
        nfspec4(nfor) = maxele
        nfptyp4(nfor) = 0
      elseif (nUFFtype(j).eq.2.and.nUFFtype(k).eq.3) then
!
!  j = sp2 / k = sp3 => i = X & k = X
!
        nfor = nfor + 1
        if (lgroup6k) then
          fork(nfor) = 5.0_dp*sqrt(Utorj*Utork)*kcaltoev*(1.0_dp+4.18_dp*log(bo))
          npfor(nfor) = 2
          forpoly(1,nfor) = 0.0_dp
        else
          fork(nfor) = 1.0_dp*kcaltoev
          npfor(nfor) = 6
          forpoly(1,nfor) = 0.0_dp
        endif
        nforty(nfor) = 13
        mmfexc(nfor) = 1
        lgenerated4(nfor) = .true.
        lfintra(nfor) = .true.
        lfinter(nfor) = .false.
        lfdreiding(nfor) = .true.
        n4botype(1,nfor) = nhigherbotype(1,ni)
        n4botype(2,nfor) = nhigherbotype(2,ni)
        nfspec1(nfor) = maxele
        nfptyp1(nfor) = 0
        nfspec2(nfor) = natUFFspec(j)
        nfptyp2(nfor) = ntypUFFspec(j)
        nfspec3(nfor) = natUFFspec(k)
        nfptyp3(nfor) = ntypUFFspec(k)
        nfspec4(nfor) = maxele
      elseif (nUFFtype(j).eq.3.and.nUFFtype(k).eq.2) then
!
!  j = sp3 / k = sp2 => i = X & k = X
!
        nfor = nfor + 1
        if (lgroup6j) then
          fork(nfor) = 5.0_dp*sqrt(Utorj*Utork)*kcaltoev*(1.0_dp+4.18_dp*log(bo))
          npfor(nfor) = 2
          forpoly(1,nfor) = 0.0_dp
        else
          fork(nfor) = 1.0_dp*kcaltoev
          npfor(nfor) = 6
          forpoly(1,nfor) = 0.0_dp
        endif
        nforty(nfor) = 13
        mmfexc(nfor) = 1
        lgenerated4(nfor) = .true.
        lfintra(nfor) = .true.
        lfinter(nfor) = .false.
        lfdreiding(nfor) = .true.
        n4botype(1,nfor) = nhigherbotype(1,ni)
        n4botype(2,nfor) = nhigherbotype(2,ni)
        nfspec1(nfor) = maxele
        nfptyp1(nfor) = 0
        nfspec2(nfor) = natUFFspec(j)
        nfptyp2(nfor) = ntypUFFspec(j)
        nfspec3(nfor) = natUFFspec(k)
        nfptyp3(nfor) = ntypUFFspec(k)
        nfspec4(nfor) = maxele
      elseif (nUFFtype(j).eq.2.and.nUFFtype(k).eq.2) then
!
!  j = sp2 / k = sp2 => i = X & k = X
!
        nfor = nfor + 1
        nforty(nfor) = 13
        mmfexc(nfor) = 1
        lgenerated4(nfor) = .true.
        lfintra(nfor) = .true.
        lfinter(nfor) = .false.
        lfdreiding(nfor) = .true.
        n4botype(1,nfor) = nhigherbotype(1,ni)
        n4botype(2,nfor) = nhigherbotype(2,ni)
        fork(nfor) = 5.0_dp*sqrt(UFFtor(j)*UFFtor(k))*(1.0_dp+4.18_dp*log(bo))
        npfor(nfor) = 2
        forpoly(1,nfor) = 180.0_dp
        nfspec1(nfor) = maxele
        nfptyp1(nfor) = 0
        nfspec2(nfor) = natUFFspec(j)
        nfptyp2(nfor) = ntypUFFspec(j)
        nfspec3(nfor) = natUFFspec(k)
        nfptyp3(nfor) = ntypUFFspec(k)
        nfspec4(nfor) = maxele
      endif
    endif
  enddo
!
!  Check that there is space in the arrays for the new out of plane potentials
!
  newpots = 4
  if (nfor+newpots.gt.maxfor) then
    maxfor = nfor + newpots
    call changemaxfor
  endif
!**************************************************************************
!  Loop over combinations of species to generate out of plane potentials  *
!**************************************************************************
  do ni = 1,ntodo
    i = nptr(ni)
!
!  Only do inversions for special cases and only if these atoms are present
!
    if (abs(UFFKoop(i)).gt.1.0d-12) then
      nfor = nfor + 1
      if (nfor.gt.maxfor) then
        maxfor = nfor + 4
        call changemaxfor
      endif
!
!  Assign coefficients and cutoffs
!
      nforty(nfor) = 15
      mmfexc(nfor) = 1
      n4botype(1,nfor) = 0
      n4botype(2,nfor) = 0
      lgenerated4(nfor) = .true.
      lfintra(nfor) = .true.
      lfinter(nfor) = .false.
      lfdreiding(nfor) = .true.
      lonly3oop(nfor) = .true.
      loutofplane(nfor) = .true.
!
      if (abs(UFFthetaoop(i)).gt.1.0d-12) then
        costheta0 = cos(UFFthetaoop(i)*degtorad)
        fork(nfor) = UFFKoop(i)
        forpoly(3,nfor) = 0.5_dp/(1.0_dp - 2.0_dp*costheta0 + costheta0**2)
        forpoly(2,nfor) = - 4.0_dp*forpoly(3,nfor)*costheta0
        forpoly(1,nfor) = forpoly(3,nfor)*(2.0_dp*costheta0**2 + 1.0_dp)
      else
        fork(nfor) = 0.5_dp*UFFKoop(i)
        forpoly(1,nfor) = 1.0_dp
        forpoly(2,nfor) = - 1.0_dp
        forpoly(3,nfor) = 0.0_dp
      endif
!
      nfspec1(nfor) = natUFFspec(i)
      nfspec2(nfor) = maxele
      nfspec3(nfor) = maxele
      nfspec4(nfor) = maxele
      nfptyp1(nfor) = ntypUFFspec(i)
      nfptyp2(nfor) = 0
      nfptyp3(nfor) = 0
      nfptyp4(nfor) = 0
      if (libspec(nptri(ni))(1:3).eq.'C_R'.or.libspec(nptri(ni))(1:3).eq.'C_2') then
!
!  Add supplementary potential for C_R/C_2 - O2 case, if needed - adds to other potential
!
        do nj = 1,ntodo
          if (libspec(nptri(nj))(1:3).eq.'O_2') then
            j = nptr(nj)
            nfor = nfor + 1
            if (nfor.gt.maxfor) then
              maxfor = nfor + 4 
              call changemaxfor
            endif
!     
!  Assign coefficients and cutoffs
!  
            nforty(nfor) = 15
            mmfexc(nfor) = 1
            n4botype(1,nfor) = 0
            n4botype(2,nfor) = 0
            lgenerated4(nfor) = .true.
            lfintra(nfor) = .true.
            lfinter(nfor) = .false.
            lfdreiding(nfor) = .true.
            lonly3oop(nfor) = .true.
            loutofplane(nfor) = .true.
!       
            fork(nfor) = 44.0_dp*kcaltoev
            forpoly(1,nfor) = 1.0_dp
            forpoly(2,nfor) = - 1.0_dp
            forpoly(3,nfor) = 0.0_dp
!
            nfspec1(nfor) = natUFFspec(i)
            nfspec2(nfor) = natUFFspec(j)
            nfspec3(nfor) = maxele
            nfspec4(nfor) = maxele
            nfptyp1(nfor) = ntypUFFspec(i)
            nfptyp2(nfor) = ntypUFFspec(j)
            nfptyp3(nfor) = 0
            nfptyp4(nfor) = 0
          endif
        enddo
      endif
    endif
  enddo
!
!  Free local workspace arrays
!
  if (nconnect.gt.0) then
    deallocate(nhigherbotype,stat=status)
    if (status/=0) call deallocate_error('setuff','nhigherbotype')
    deallocate(nhigherbo2,stat=status)
    if (status/=0) call deallocate_error('setuff','nhigherbo2')
    deallocate(nhigherbo1,stat=status)
    if (status/=0) call deallocate_error('setuff','nhigherbo1')
  endif
  deallocate(nptri,stat=status)
  if (status/=0) call deallocate_error('setuff','nptri')
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('setuff','nptr')
!
  return
  end
