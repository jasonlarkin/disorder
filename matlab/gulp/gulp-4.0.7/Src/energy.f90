  subroutine energy(etot,lgrad1,lgrad2)
!
!  Subroutine for calculating lattice energy
!
!   5/95 Modifications added for symmetrisation of second derivatives
!   6/95 Calls of real space routines stopped if there are no charges
!        or potentials
!   4/97 Sutton-Chen many-body potentials added
!   7/97 Neutralising background added
!  10/97 Modification added to allow for frozen atoms during zeroing 
!        of derv2
!  11/97 Bug in zeroing of derv2 for frozen atoms fixed
!  12/97 Storage of energy components added
!  12/97 Calls to kindex modified for variable rspeed
!  12/97 Self energy term added to total for EEM/QEq
!  12/97 Zeroing of first derivatives moved to before call to eem
!        so that contributions can be set in eem if needed
!   8/98 Parts made into subroutines
!   8/98 Strfin call moved outside energy for benefit of FEM
!   3/99 Parallel modifications added
!   7/99 Option to use minimum image for large calculations added
!   5/00 Dipolar polarisation energy from point ion added
!   6/00 recipsd removed to reduce size of code
!   3/01 Calculation of surface energy added
!   9/01 Modified for 1-D systems
!  11/01 Attachment energy added
!   5/02 Scaling of shift added
!   5/02 Brenner potential added
!   8/02 Surface energy calculation removed since this is now done
!        in a separate subroutine
!   8/02 External force added
!  10/02 Interaction energy between region 1/2 corrected
!  11/02 Einstein energy added
!  11/02 Call to psumall added
!   1/03 Wolf sum modifications added
!   5/03 Spatial decomposition option introduced
!   6/03 XML modifications added
!  11/03 Bond order potentials added
!   9/04 Dual spatial decomposition introduced
!  11/04 Six-body potentials added
!   5/06 Call to real1Dmd added
!   7/06 Six-body potentials added
!   8/06 Vibrational energy initialised though not used
!  11/06 Celltype called to ensure that ictype is correct
!   3/07 Electric field option added
!   3/07 Radial force added
!   3/07 Chemshell changes added
!   5/07 MC call option added for x0tostr
!   5/07 Call to setspatial(bo) modified
!   7/07 emeta added as dummy
!   7/07 ReaxFF calls added
!   7/07 Plane potential added for 2-D case
!   8/07 Plane potential enabled for O-D case
!  11/07 Unused variables removed
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!  10/08 COSMO/COSMIC changes merged
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   2/09 Old xml calls removed
!   3/09 Call to sumderv1 added to handle non-radial derivative arrays
!   4/09 Minimum image option added for many in non-symmetry case
!   4/09 Separate call to generate MEAM densities added
!   6/09 Initialisation of site energies added
!   6/09 Module name changed from three to m_three
!   7/09 Experimental call to EVB added
!   7/09 cutoffmax/cutoffmaxbo moved to general module
!   7/09 Call to setcutoffmax(bo) removed
!   1/10 One-body potentials added
!   6/10 Symmetrised calculation of EAM/MEAM density turned off when
!        lsymderv is false
!   9/10 EDIP energy added
!  10/10 EDIP enabled for spatial decomposition
!  11/10 Anisotropic pressure added
!   7/11 esregion2 added to call for wolfself
!   9/11 lgrad1 argument added to eem call
!   9/11 Metadynamics internal code replaced with Plumed
!  11/11 Initialisation of eregion2region added
!  11/11 Summing of off-diagonal region-region energies added
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
  use bondorderdata, only : nbopot, nboQ, nboQ0
  use cellmultipole
  use configurations
  use constants
  use control
  use current
  use eam,           only : lMEAM, lMEAMden, maxmeamcomponent
  use energies
  use field,         only : lfieldcfg
  use four
  use gulpchemsh,    only : ichemsh_qm
  use iochannels
  use kspace
  use m_three
  use molecule
  use one,           only : none
  use optimisation
  use plane,         only : nplanepot
  use parallel
  use polarise
  use potentialxyz
  use reaxFFdata,    only : nreaxFFspec
  use shifts
  use six
  use spatial,       only : lspatialok
  use spatialbo,     only : lspatialBOok
  use sutton
  use symmetry
  use two
  implicit none
!
!  Passed variables
!
  real(dp), intent(out) :: etot
  logical,  intent(in)  :: lgrad1
  logical,  intent(in)  :: lgrad2
!
!  Local variables
!
  integer(i4)           :: i
  integer(i4)           :: j
  integer(i4),     save :: ncflast = 0
  logical               :: lcmm
  logical               :: lpress
  logical               :: lsymoffloc
  real(dp)              :: esum
  real(dp)              :: sft
!
  lcmm = (icmm.gt.0.and.ndim.eq.0)
  lpress = (abs(press).gt.0.0_dp.and.ndim.gt.0)
  lsymoffloc = lsymoff
!
!  Initialise energy components
!
  etot = 0.0_dp
  evib = 0.0_dp
  ereal = 0.0_dp
  erecip = 0.0_dp
  eatom = 0.0_dp
  ethb = 0.0_dp
  eedip = 0.0_dp
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
  emad = 0.0_dp
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
!
!  Initialise region-region interaction energy components and site energies
!
  eregion2region(1:nregions(ncf),1:nregions(ncf)) = 0.0_dp
  siteenergy(1:numat) = 0.0_dp
!
!  Zero many-body terms
!
  if (lsuttonc) then
    if (lMEAM) then
      do i = 1,numat
        scrho(1:maxmeamcomponent,i) = 0.0_dp
        scrho12(1:maxmeamcomponent,i) = 0.0_dp
      enddo
    else
      do i = 1,numat
        scrho(1,i) = 0.0_dp
        scrho12(1,i) = 0.0_dp
      enddo
    endif
  endif
!*******************************************
!  Set up spatial decomposition if needed  *
!*******************************************
  lspatialok = lspatial
  lspatialBOok = (lspatial.and.(lbrenner.or.lEDIP.or.(nbopot+nboQ).gt.0.or.nreaxFFspec.gt.0))
  if (lspatialok) then
    call setspatial(.true.)
  endif
  if (lspatialBOok) then
    call setspatialbo(.true.)
  endif
!
!  Calculate parallel division of work :
!  
!  Spatial - divide cells over processors     
!  Non-spatial - divide atoms over processors
!   
  call setatomnodes(numat,nprocs,procid,lspatialok)
  call setatomnodesbo(numat,nprocs,procid,lspatialBOok)
!
!  Set up local variables
!
  if (ndim.eq.3) then
!
!  Symmetry can only be used if the cell parameters conform
!  to the correct space group - perform continuous check here
!
    if (lsymopt.and..not.lsymoff) then
      call celltype(ictype,icfhr)
      lsymoffloc = (nccs.gt.ictype)
      if (index(keyword,'verb').ne.0.and.lsymoffloc) then
        if (ioproc) then
          write(ioout,'(''  ** Symmetry turned off due to cell **'')')
        endif
      endif
    endif
  endif
!**************************************
!  Call EEM/QEq to calculate charges  *
!**************************************
  if (leem) then
    call eem(.false.,lgrad1,lgrad2)
  endif
!****************************************************
!  Calculate charges according to bond order model  *
!****************************************************
  if (nboQ.gt.0) then
    call getBOcharge(lgrad1,lgrad2)
  endif
!*********************
!  Solvation set up  *
!*********************
  if (lcosmo) then
    if (ncf.ne.ncflast.or.index(keyword,'nosa').eq.0) then
      call setsas
    else
      call updatesas
    endif
  endif
!*************************************************
!  Zero derivatives                              *
!  In ChemShell case the QM force is added here  *
!*************************************************
  call initdervs(lgrad1,lgrad2)
!**************************************
!  Initialise electric field to zero  *
!**************************************
  if (lpolar) then
    do i = 1,nasym
      vx(i) = 0.0_dp
      vy(i) = 0.0_dp
      vz(i) = 0.0_dp
      vx12(i) = 0.0_dp
      vy12(i) = 0.0_dp
      vz12(i) = 0.0_dp
    enddo
    if (lqpolar) then
      do i = 1,nasym
        v2xyz(1:6,i) = 0.0_dp
        v2xyz12(1:6,i) = 0.0_dp
      enddo
    endif
  endif
!***************************************
!  Cell Multipole Method for clusters  *
!***************************************
  if (lcmm) call setcmm
!
!  Redetermine cell indices for molecule atoms
!  incase one has moved across cell boundary.
!
  if (nmol.gt.0.and.ndim.gt.0) then
    if (lmolfix) then
      call molindfix
    else
      call molind
!
!  Recalculate bond increment charges since they depend on the connectivity
!
      if (lqbond) call bondq(.false.)
    endif
  endif
!*******************************
!  Electrostatic contribution  *
!*******************************
  if (lewald.and.ndim.gt.1) then
!
!  Reciprocal space component for 2-D and 3-D cases  
!
    call kindex
    if (lsymopt.and.lsymderv) then
      if ((lgrad2.and..not.lsymderv2).or.lsymoffloc) then
        call recip3D(erecip,ec6,lgrad1,lgrad2)
      elseif (lgrad2) then
        call recipsd2(erecip,ec6,lgrad1,lgrad2)
      else
        call recipsd(erecip,ec6,lgrad1)
      endif
    else
      if (ndim.eq.3) then
        call recip3D(erecip,ec6,lgrad1,lgrad2)
      elseif (ndim.eq.2) then
        call recip2D(erecip,esregion12,esregion2,eattach,lgrad1,lgrad2)
      endif
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
    if (lsymopt) then
      if (lsymderv.and.(lgrad1.or.lgrad2)) then
        if ((lgrad2.and..not.lsymderv2).or.lsymoffloc) then
          if (.not.lgrad2) then
            if (lminimage) then
              call realmi3(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
            else
              if (lspatialok) then
                call realmd3s(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
              else
                call realmd3(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
              endif
            endif
          else
            call reale(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1,lgrad2)
          endif
        elseif (lgrad2) then
          call realsd2(eatom,ereal,erecip,ec6,eqeq,lgrad1,lgrad2)
        else
          call realsd(eatom,ereal,erecip,ec6,eqeq,lgrad1)
        endif
      else
        if (lgrad1.or.lgrad2.or.lsymoffloc) then
          if (.not.lgrad2) then
            if (lminimage) then
              call realmi3(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
            else
              if (lspatialok) then
                call realmd3s(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
              else
                call realmd3(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
              endif
            endif
          else
            call reale(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1,lgrad2)
          endif
        else
          call realsd(eatom,ereal,erecip,ec6,eqeq,lgrad1)
        endif
      endif
    else
      if (ndim.gt.0) then
        if (.not.lgrad2) then
          if (lminimage) then
            call realmi3(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
          else
            if (lspatialok) then
              call realmd3s(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
            else
              call realmd3(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
            endif
          endif
        else
          call reale(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1,lgrad2)
        endif
      else
        if (lcmm) then
          call realcmm(eatom,ereal,ecmm,eqeq,lgrad1)
        else
          if (.not.lgrad2) then
            call realmd0(eatom,ereal,eqeq,lgrad1)
          else
            call real0d(eatom,ereal,eqeq,lgrad1,lgrad2)
          endif
        endif
      endif
    endif
    if (ndim.eq.1.and..not.lwolf) then
!
!  Real space Coulomb contribution from beyond potential cut-off in 1-D
!
      if (.not.lgrad2) then
        call real1Dmd(ereal,esregion12,esregion2,lgrad1)
      else
        call real1D(ereal,esregion12,esregion2,lgrad1,lgrad2)
      endif
    endif
  endif
!*****************************
!  Point-ion polarisability  *
!*****************************
  if (lpolar) then
    call polarisation(epolar,esregion12,esregion2,eattach,lgrad1)
  endif
!**********************************
!  Bond order charge self-energy  *
!**********************************
  if (nboQ0.gt.0) then
    call BOself(eboQself,lgrad1,lgrad2,.false.)
  endif
!*********************
!  Solvation energy  *
!*********************
  if (lcosmo) then
    call solvation(ecosmo,lgrad1,lgrad2)
  endif
!**********************
!  Three-body energy  *
!**********************
  if (nthb.gt.0) then
    if (lsymderv2) then
      if (lgrad2) then
        call threesd2(ethb,lgrad1,lgrad2)
      else
        call threesd(ethb,lgrad1)
      endif
    else
      if (lgrad2) then
        call threenos(ethb,esregion12,esregion2,eattach,lgrad1,lgrad2)
      else
        if (lspatialok) then
          call threemds(ethb,esregion12,esregion2,eattach,lgrad1)
        else
          call threemd(ethb,esregion12,esregion2,eattach,lgrad1)
        endif
      endif
    endif
  endif
!*********************
!  Four-body energy  *
!*********************
  if (nfor.gt.0) then
    if (lsymderv2) then
      if (lgrad2) then
        call foursd2(efor,eoop,lgrad1,lgrad2)
      else
        call foursd(efor,eoop,lgrad1)
      endif
    else
      if (lgrad2) then
        call fournos(efor,eoop,esregion12,esregion2,eattach,lgrad1,lgrad2)
      else
        if (lspatialok) then
          call fourmds(efor,eoop,esregion12,esregion2,eattach,lgrad1)
        else
          call fourmd(efor,eoop,esregion12,esregion2,eattach,lgrad1)
        endif
      endif
    endif
  endif
!*********************
!  Six-body energy  *
!*********************
  if (nsix.gt.0) then
    if (lsymderv2) then
      if (lgrad2) then
        call sixsd2(esix,lgrad1,lgrad2)
      else
        call sixsd(esix,lgrad1)
      endif
    else
      if (lgrad2) then
        call sixnos(esix,esregion12,esregion2,eattach,lgrad1,lgrad2)
      else
        if (lspatialok) then
          call sixmds(esix,esregion12,esregion2,eattach,lgrad1)
        else
          call sixmd(esix,esregion12,esregion2,eattach,lgrad1)
        endif
      endif
    endif
  endif
!*********************
!  Many-body energy  *
!*********************
  if (lsuttonc) then
!---------------------------
!  Compute density : MEAM  !
!---------------------------
    if (lMEAMden) then
      if (lsymopt.and.lsymderv) then
        call densitysd
      elseif (ndim.eq.0) then
        call density0
      else
        if (lspatialok) then
          call density3s
        else
          call density3
        endif
      endif
    endif
!--------------------------------
!  Compute energy : EAM & MEAM  !
!--------------------------------
    if (lsymopt) then
      if (lsymderv.and.(lgrad1.or.lgrad2)) then
        if ((lgrad2.and..not.lsymderv2).or.lsymoffloc) then
          if (lgrad2) then
            call many(emany,esregion12,esregion2,eattach,lgrad1,lgrad2)
          else
            if (lminimage) then
              call manymi3(emany,esregion12,esregion2,eattach,lgrad1)
            else
              call manymd3(emany,esregion12,esregion2,eattach,lgrad1)
            endif
          endif
        else
          if (lgrad2) then
            call manysd2(emany,lgrad1,lgrad2)
          else
            call manysd(emany,lgrad1)
          endif
        endif
      else
        if (lgrad1.or.lgrad2.or.lsymoffloc) then
          if (lgrad2) then
            call many(emany,esregion12,esregion2,eattach,lgrad1,lgrad2)
          else
            if (lminimage) then
              call manymi3(emany,esregion12,esregion2,eattach,lgrad1)
            else
              if (lspatialok) then
                call manymd3s(emany,esregion12,esregion2,eattach,lgrad1)
              else
                call manymd3(emany,esregion12,esregion2,eattach,lgrad1)
              endif
            endif
          endif
        else
          if (lgrad2) then
            call manysd2(emany,lgrad1,lgrad2)
          else
            call manysd(emany,lgrad1)
          endif
        endif
      endif
    else
      if (ndim.gt.0) then
        if (lgrad2) then
          call many(emany,esregion12,esregion2,eattach,lgrad1,lgrad2)
        else
          if (lspatialok) then
            call manymd3s(emany,esregion12,esregion2,eattach,lgrad1)
          elseif (lminimage) then
            call manymi3(emany,esregion12,esregion2,eattach,lgrad1)
          else
            call manymd3(emany,esregion12,esregion2,eattach,lgrad1)
          endif
        endif
      else
        if (lgrad2) then
          call many0d(emany,lgrad1,lgrad2)
        else
          call manymd0(emany,lgrad1)
        endif
      endif
    endif
  endif
!**********************
!  Brenner potential  *
!**********************
  if (lbrenner) then
    if (lsymderv2) then
      call brennersd2(ebrenner,lgrad1,lgrad2)
    else
      if (lgrad2) then
        call brenner(ebrenner,0.0_dp,0.0_dp,0.0_dp,lgrad1,lgrad2,.false.)
      else
        call brennermd(ebrenner,lgrad1)
      endif
    endif
  endif
!**************************
!  Bond order potentials  *
!**************************
  if (nbopot.gt.0) then
    if (lsymderv2) then
      call bondordersd2(ebondorder,lgrad1,lgrad2)
    else
      if (lgrad2) then
        call bondorder(ebondorder,0.0_dp,0.0_dp,0.0_dp,lgrad1,lgrad2,.false.)
      else
        call bondordermd(ebondorder,lgrad1)
      endif
    endif
  endif
!**********************
!  ReaxFF forcefield  *
!**********************
  if (lreaxFF) then
!    if (lsymderv2) then
!      call reaxFFsd2(ereaxFF,lgrad1,lgrad2)
!    else
!      if (lgrad2) then
!        call reaxFF(ereaxFF,0.0_dp,0.0_dp,0.0_dp,lgrad1,lgrad2,.false.)
!      else
        call reaxFFmd(ereaxFF,lgrad1)
!      endif
!    endif
  endif
!********************
!  EDIP forcefield  *
!********************
  if (lEDIP) then
    call EDIPmd(eEDIP,lgrad1)
  endif
!*************************
!  Pressure-volume term  *
!*************************
  if (lpress.or.lanisotropicpress) call pressure(epv,lgrad1,lgrad2)
!*******************
!  External force  *
!*******************
  call force(eforce,lgrad1)
!*****************
!  Radial force  *
!*****************
  call radialforce(eradial,lgrad1,lgrad2)
!*******************
!  Electric field  *
!*******************
  if (lfieldcfg(ncf)) then
    call electricfield(efield,lgrad1)
  endif
!*******************
!  Einstein model  *
!*******************
  if (leinstein) then
    if (lsymderv2) then
      call einsteinsd2(eeinstein,lgrad1,lgrad2)
    else
      call einstein(eeinstein,lgrad1,lgrad2)
    endif
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
    if (lgrad2) then
      call planepot2D(eplane,esregion2,eattach,lgrad1,lgrad2)
    else
      call planepotmd(eplane,esregion2,eattach,lgrad1)
    endif
  endif
!***********************
!  Wolf sum self term  *
!***********************
  if (lwolf) then
    call wolfself(ewolfself,esregion2)
  endif
!****************************
!  Neutralising background  *
!****************************
  if (ndim.gt.0.and.abs(totalcharge).gt.1.0d-4) call background(ebgd,emad,lgrad1,lgrad2)
!*****************************
!  Dipole correction energy  *
!*****************************
  if (ldipole.and.(lewald.or.lwolf).and.ndim.gt.0) then
    if (lsymopt.and.lsymderv) then
      if ((lgrad2.and..not.lsymderv2).or.lsymoffloc) then
        call dipole3D(edipole,lgrad1,lgrad2)
      else
        call dipolesd(edipole,lgrad1,lgrad2)
      endif
    else
      call dipole3D(edipole,lgrad1,lgrad2)
    endif
  endif
!**********************************
!  Chemshell energy contribution  *
!**********************************
  if (ichemsh_qm.eq.1) then
    call chemshellenergy(echemsh)
  endif
!******************************************
!  Symmetrise second derivative matrices  *
!******************************************
  if (lgrad2.and..not.lsymderv2) call symderv2(lgrad2)
!********************************
!  Parallel sum of derivatives  *
!********************************
  if (nprocs.gt.1) then
    call psumall(eatom,ereal,erecip,ec6,eqeq,eattach,esregion12,esregion2,ethb,efor, &
      eoop,emany,ecmm,ebrenner,epolar,eeinstein,ewolfself,ebondorder,eforce,esix, &
      efield,eradial,ereaxFF,eplane,ecosmo,eone,eedip,lgrad1,lsymderv)
  endif
!
!  Complete derivatives
!
  if (lgrad1) then
    call sumderv1(nasym)
  endif
  if (lgrad2) then
    if (lfreeze) then
      if (lsymderv2) then
        call sumderv2f(numat,nasym,lsymderv2)
      else
        call sumderv2f(numat,numat,lsymderv2)
      endif
    else
      if (lsymderv2) then
        call sumderv2s(numat,nasym,.false.,.true.)
      else
        call sumderv2(numat,.false.)
      endif
    endif
  endif
!
!  Sum components of total energy
!
  sft = shift(nshcfg(ncf))*shscalecfg(ncf)
  etot = erecip + eatom + sft + ethb + ereal + efor + epv + ecmm + ec6 + eoop + emany + &
         edipole + ebgd + eself + eqeq + epolar + ebrenner + eforce + &
         esregion12 + esregion2 + eeinstein + ewolfself + ebondorder + eboQself + esix + &
         echargecoupled + efield + eradial + ereaxFF + eplane + ecosmo + eone + eedip + emad
!
!  Sum off diagonal region-region energies
!
  do i = 2,nregions(ncf)
    do j = 1,i-1
      esum = eregion2region(j,i) + eregion2region(i,j)
      eregion2region(j,i) = esum
      eregion2region(i,j) = esum
    enddo
  enddo
!
!  Add Chemshell energy
!
  etot = etot + echemsh
!
  fcstore = etot
  ncflast = ncf
!
  return
  end
