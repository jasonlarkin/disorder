  subroutine create(newspecies,natom)
!
!  Creates new atoms in GCMC
!
!  On entry :
!
!  newspecies = species index to create
!
!  On exit :
!
!  natom = atom in asymmetric unit that was created
!
!   8/04 Initialisation of force quantities added
!   9/04 Cartesian coordinates properly set up
!   5/06 Species number now also stored in pointer
!   1/08 random -> GULP_random
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use configurations
  use control
  use current
  use element,       only : maxele
  use genetic,       only : iseed
  use moldyn,        only : lfix
  use optimisation,  only : lopf
  use polarise
  use potentialxyz
  use scan,          only : ltranat
  use species
  use symmetry,      only : nspcg, ngocfg
  use velocities 
  implicit none
!
!  Passed variables
!
  integer(i4)                            :: natom
  integer(i4)                            :: newspecies
!
  integer(i4)                            :: i
  integer(i4)                            :: ind
  integer(i4)                            :: nr
  integer(i4)                            :: numatold
  logical                                :: lfound
  real(dp)                               :: x(3)
  real(dp)                               :: GULP_random
!
!  Correct number of atoms in asymmetric unit
!
  nasym = nasym + 1
  natom = nasym
!
!  Check dimensions
!
  if (nasym.gt.maxat) then
    maxat = nasym + 10
    call changemaxat
  endif
  if (nasym.gt.maxatot) then
    maxatot = nasym + 10
    call changemaxatot
  endif
!
!  Fill in details in asymmetric unit
!
  nspecptr(natom) = newspecies
  iatn(natom) = natspec(newspecies)
  natype(natom) = ntypspec(newspecies)
  c6a(natom) = c6spec(newspecies)
  qa(natom) = qlspec(newspecies)
  rada(natom) = radspec(newspecies)
  occua(natom) = 1.0_dp
  lopf(natom) = .true.
!
  if (lpolar.and.iatn(natom).le.maxele) then
!
!  Polarisability
!
    lfound = .false.
    i = 0
    do while (.not.lfound.and.i.lt.npolspec)
      i = i + 1
      if (iatn(natom).eq.natpolspec(i)) then
        if (natype(natom).eq.ntyppolspec(i).or.ntyppolspec(i).eq.0) then
          lfound = .true.
          dpolar(natom) = dpolspec(i)
          qpolar(natom) = qpolspec(i)
        endif
      endif
    enddo
    if (.not.lfound) then
      dpolar(natom) = 0.0_dp
      qpolar(natom) = 0.0_dp
    endif
  endif
!
!  These are just dummy values for now as they
!  shouldn't be required during GCMC
!
  lbsmat(natom) = .false.
  lfix(natom) = .false.
  lqmatom(natom) = .false.
  ltdforcecfg(1,natom) = .false.
  ltdforcecfg(2,natom) = .false.
  ltdforcecfg(3,natom) = .false.
  ltranat(natom) = .false.
  oxa(natom) = 0.0_dp
  cna(natom) = 0.0_dp
  forcecfg(1,natom) = 0.0_dp
  forcecfg(2,natom) = 0.0_dp
  forcecfg(3,natom) = 0.0_dp
  xstore(natom) = 0.0_dp
  ystore(natom) = 0.0_dp
  zstore(natom) = 0.0_dp
  rstore(natom) = 0.0_dp
  v2xyz(1:6,natom) = 0.0_dp
  vx(natom) = 0.0_dp
  vy(natom) = 0.0_dp
  vz(natom) = 0.0_dp
  v2xyz12(1:6,natom) = 0.0_dp
  vx12(natom) = 0.0_dp
  vy12(natom) = 0.0_dp
  vz12(natom) = 0.0_dp
!
!  Create coordinates
!
  x(1) = GULP_random(iseed,2_i4)
  x(2) = GULP_random(iseed,2_i4)
  x(3) = GULP_random(iseed,2_i4)
  xafrac(natom) = x(1)
  yafrac(natom) = x(2)
  zafrac(natom) = x(3)
!
!  If symmetry is turned on, build images
!
  numatold = numat
  if (nspcg(ncf).gt.1.or.ngocfg(ncf).gt.1) then
    call symgenatom(natom,numat,x,.false.)
  else
    numat = numat + 1
    nrel2(numat) = numat
    nat(numat) = iatn(numat)
    nftype(numat) = natype(numat)
    nrelat(numat) = numat
    nrotop(numat) = 1
    c6f(numat) = c6a(numat)
    qf(numat) = qa(numat)
    occuf(numat) = occua(numat)
    radf(numat) = rada(numat)
    oxf(numat) = oxa(numat)
    cnf(numat) = cna(numat)
    neqv(numat) = 1
    xfrac(numat) = x(1)
    yfrac(numat) = x(2)
    zfrac(numat) = x(3)
  endif
!
!  Generate cartesian coordinates
!
  if (ndim.eq.3) then
    do i = numatold+1,numat
      xclat(i) = xfrac(i)*r1x + yfrac(i)*r2x + zfrac(i)*r3x
      yclat(i) = xfrac(i)*r1y + yfrac(i)*r2y + zfrac(i)*r3y
      zclat(i) = xfrac(i)*r1z + yfrac(i)*r2z + zfrac(i)*r3z
    enddo
  elseif (ndim.eq.2) then
    do i = numatold+1,numat
      xclat(i) = xfrac(i)*r1x + yfrac(i)*r2x
      yclat(i) = xfrac(i)*r1y + yfrac(i)*r2y
      zclat(i) = zfrac(i)
    enddo
  elseif (ndim.eq.1) then
    do i = numatold+1,numat
      xclat(i) = xfrac(i)*r1x
      yclat(i) = yfrac(i)
      zclat(i) = zfrac(i)
    enddo
  else
    do i = numatold+1,numat
      xclat(i) = xfrac(i)
      yclat(i) = yfrac(i)
      zclat(i) = zfrac(i)
    enddo
  endif
!
!  Fill remaining nasym arrays
!
  nr = nrel2(natom)
  xalat(natom) = xclat(nr)
  yalat(natom) = yclat(nr)
  zalat(natom) = zclat(nr)
  xinitial(natom) = xclat(nr)
  yinitial(natom) = yclat(nr)
  zinitial(natom) = zclat(nr)
!
!  Initialise dummy values for full cell quantities
!
  ind = nrel2(natom) - 1
  do i = 1,neqv(natom)
    icosx(ind+i) = 0
    icosy(ind+i) = 0
    icosz(ind+i) = 0
    mass(ind+i) = 0.0_dp
    rmass(ind+i) = 0.0_dp
    velx(ind+i) = 0.0_dp
    vely(ind+i) = 0.0_dp
    velz(ind+i) = 0.0_dp
    x2(ind+i) = 0.0_dp
    y2(ind+i) = 0.0_dp
    z2(ind+i) = 0.0_dp
    x3(ind+i) = 0.0_dp
    y3(ind+i) = 0.0_dp
    z3(ind+i) = 0.0_dp
    x4(ind+i) = 0.0_dp
    y4(ind+i) = 0.0_dp
    z4(ind+i) = 0.0_dp
    x5(ind+i) = 0.0_dp
    y5(ind+i) = 0.0_dp
    z5(ind+i) = 0.0_dp
  enddo
!
!  Set up optimisation variables and arrays
!
  lopfi(3*natom-2) = .true.
  lopfi(3*natom-1) = .true.
  lopfi(3*natom) = .true.
  iopt(nvar+1) = 1_i4
  iopt(nvar+2) = 1_i4
  iopt(nvar+3) = 1_i4
  nvar = nvar + 3
  x0(3*natom-2+nstrains) = x(1)
  x0(3*natom-1+nstrains) = x(2)
  x0(3*natom+nstrains) = x(3)
!
  return
  end
