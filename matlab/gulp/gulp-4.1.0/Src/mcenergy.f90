  subroutine mcenergy(epart,ntrialatom,nptrtrialatom,ltrialatom)
!
!  Subroutine for calculating energy associated with a subgroup of atoms for MC
!
!  NB: There are restrictions on when this routine can be called in MC. Important
!      ones are that the strains must be fixed and charge equilibration schemes
!      can't be used. 
!
!   1/08 Created from energy
!   1/08 lreaxFFqreal removed
!   6/09 Module name changed from three to m_three
!   1/10 One-body potentials added
!   9/10 EDIP energy added
!   9/11 Metadynamics internal code replaced with Plumed
!  11/12 Call to threemc & fourmc & sixmc modified as nptrtrialatom is no longer passed
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
!  Julian Gale, NRI, Curtin University, November 2012
!
  use bondorderdata, only : nboQ, nboQ0
  use cellmultipole
  use configurations
  use constants
  use control
  use current
  use energies
  use field,       only : lfieldcfg
  use four
  use gulpchemsh,  only : ichemsh_qm
  use iochannels
  use kspace
  use m_three
  use molecule
  use one,         only : none
  use optimisation
  use plane,       only : nplanepot
  use parallel
  use shifts
  use six
  use sutton
  use symmetry
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: ntrialatom
  integer(i4), intent(in)    :: nptrtrialatom(ntrialatom)
  logical,     intent(inout) :: ltrialatom(numat)
  real(dp),    intent(out)   :: epart
!
!  Local variables
!
  integer(i4)                :: i
!
!  Set up reverse pointer for trial atoms - assumes that ltrialatom is always previously initialised to false
!
  do i = 1,ntrialatom
    ltrialatom(nptrtrialatom(i)) = .true.
  enddo
!
  epart = 0.0_dp
  evib = 0.0_dp
  ereal = 0.0_dp
  erecip = 0.0_dp
  eatom = 0.0_dp
  eedip = 0.0_dp
  ethb = 0.0_dp
  eeinstein = 0.0_dp
  efield = 0.0_dp
  efor = 0.0_dp
  eforce = 0.0_dp
  eoop = 0.0_dp
  emany = 0.0_dp
  epolar = 0.0_dp
  epv = 0.0_dp
  ebrenner = 0.0_dp
  ecmm = 0.0_dp
  ec6 = 0.0_dp
  edipole = 0.0_dp
  ebgd = 0.0_dp
  eself = 0.0_dp
  esix = 0.0_dp
  eone = 0.0_dp
  eqeq = 0.0_dp
  eattach = 0.0_dp
  esurface = 0.0_dp
  esregion12 = 0.0_dp
  esregion2 = 0.0_dp
  ewolfself = 0.0_dp
  ebondorder = 0.0_dp
  eboQself = 0.0_dp
  echargecoupled = 0.0_dp
  eradial = 0.0_dp
  echemsh = 0.0_dp
  ereaxFF = 0.0_dp
  eplane  = 0.0_dp
  ecosmo  = 0.0_dp
!************************************************************
!  Convert linear structure array to main structure arrays  *
!************************************************************
  call x0tostrtrial(ntrialatom,nptrtrialatom)
!
!  Calculate parallel division of work :
!  
!  Non-spatial - divide atoms over processors
!   
  call setatomnodes(numat,nprocs,procid,.false.)
  call setatomnodesbo(numat,nprocs,procid,.false.)
!****************************************************
!  Calculate charges according to bond order model  *
!****************************************************
  if (nboQ.gt.0) then
    call getBOchargetrial(ntrialatom,nptrtrialatom)
  endif
!
!  Redetermine cell indices for molecule atoms
!  incase one has moved across cell boundary.
!
  if (nmol.gt.0.and.ndim.gt.0) then
    if (lmolfix) then
      call molindfixtrial(ntrialatom,nptrtrialatom,ltrialatom)
    else
      call molindtrial(ntrialatom,nptrtrialatom,ltrialatom)
!
!  Recalculate bond increment charges since they depend on the connectivity
!
      if (lqbond) call bondqtrial(ntrialatom,nptrtrialatom)
    endif
  endif
!*******************************
!  Electrostatic contribution  *
!*******************************
  if (lewald.and.ndim.gt.1) then
!
!  Reciprocal space component for 2-D and 3-D cases  
!
    if (ndim.eq.3) then
      call recip3Dtrial(erecip,ec6,ntrialatom,nptrtrialatom,ltrialatom)
    elseif (ndim.eq.2) then
      call recip2Dtrial(erecip,ntrialatom,nptrtrialatom,ltrialatom)
    endif
  elseif (lewald.and.ndim.eq.0) then
    rmx2 = 1.0d10
  else
    rmx2 = 0.0_dp
  endif
!**********************
!  Real space energy  *
!**********************
  if (lewald.or.lwolf.or.npote.ne.0) then
    if (ndim.gt.0) then
      if (lminimage) then
        call realmimc3(eatom,ereal,erecip,ec6,ntrialatom,nptrtrialatom,ltrialatom)
      else
        call realmc3(eatom,ereal,erecip,ec6,ntrialatom,nptrtrialatom,ltrialatom)
      endif
    else
      call realmc0(eatom,ereal,ntrialatom,nptrtrialatom,ltrialatom)
    endif
    if (ndim.eq.1.and..not.lwolf) then
!
!  Real space Coulomb contribution from beyond potential cut-off in 1-D
!
      call real1Dmc(ereal,ntrialatom,nptrtrialatom,ltrialatom)
    endif
  endif
!**********************************
!  Bond order charge self-energy  *
!**********************************
  if (nboQ0.gt.0) then
    call BOselftrial(eboQself,ntrialatom,nptrtrialatom)
  endif
!**********************
!  Three-body energy  *
!**********************
  if (nthb.gt.0) then
! DEBUG
!  The following routine needs optimisation!
    call threemc(ethb,ntrialatom,ltrialatom)
  endif
!*********************
!  Four-body energy  *
!*********************
  if (nfor.gt.0) then
! DEBUG
!  The following routine needs optimisation!
    call fourmc(efor,eoop,ntrialatom,ltrialatom)
  endif
!*********************
!  Six-body energy  *
!*********************
  if (nsix.gt.0) then
! DEBUG
!  The following routine needs optimisation!
    call sixmc(esix,ntrialatom,ltrialatom)
  endif
!*******************
!  External force  *
!*******************
  call forcetrial(eforce,ntrialatom,nptrtrialatom)
!*****************
!  Radial force  *
!*****************
  call radialforcetrial(eradial,ntrialatom,nptrtrialatom)
!*******************
!  Electric field  *
!*******************
  if (lfieldcfg(ncf)) then
    call electricfieldtrial(efield,ntrialatom,nptrtrialatom)
  endif
!*******************
!  Einstein model  *
!*******************
  if (leinstein) then
    call einsteintrial(eeinstein,ntrialatom,nptrtrialatom)
  endif
!************************
!  One-body potentials  *
!************************
  if (none.gt.0) then
    call onebody(eone,esregion2,eattach)
  endif
!*********************
!  Plane potentials  *
!*********************
  if ((ndim.eq.0.or.ndim.eq.2).and.nplanepot.gt.0) then
    call planepotmc(eplane,ntrialatom,nptrtrialatom)
  endif
!***********************
!  Wolf sum self term  *
!***********************
  if (lwolf) then
    call wolfselftrial(ewolfself,ntrialatom,nptrtrialatom)
  endif
!*****************************
!  Dipole correction energy  *
!*****************************
  if (ldipole.and.(lewald.or.lwolf).and.ndim.gt.0) then
    call dipole3Dtrial(edipole,ntrialatom,nptrtrialatom)
  endif
!**********************************
!  Chemshell energy contribution  *
!**********************************
  if (ichemsh_qm.eq.1) then
    call chemshellenergy(echemsh)
  endif
!********************************
!  Parallel sum of derivatives  *
!********************************
  if (nprocs.gt.1) then
    call psumall(eatom,ereal,erecip,ec6,eqeq,eattach,esregion12,esregion2,ethb,efor, &
      eoop,emany,ecmm,ebrenner,epolar,eeinstein,ewolfself,ebondorder,eforce,esix, &
      efield,eradial,ereaxFF,eplane,ecosmo,eone,eedip,.false.,.false.)
  endif
!
!  Sum components of total energy
!
  epart = erecip + eatom + ethb + ereal + efor + ec6 + eoop + emany + &
          edipole + ebgd + eself + eqeq + ebrenner + eforce + &
          esregion12 + esregion2 + eeinstein + ewolfself + ebondorder + eboQself + esix + &
          echargecoupled + efield + eradial + ereaxFF + eplane + ecosmo + eone + eedip
!
!  Add Chemshell energy
!
  epart = epart + echemsh
!
!  Reinitialise ltrialatom
!
  do i = 1,ntrialatom
    ltrialatom(nptrtrialatom(i)) = .false.
  enddo
!
  return
  end
