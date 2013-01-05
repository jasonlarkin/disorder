!****************************************************************************!
!                                                                            !
!       ******                 MODULE GULP_CML               ******          !
!                              ===============                               !
!                                                                            !
! This module writes all the new CML output according the the eMinerals      !
! CML convention (subset). Output is via the FoX libary (use is made of the  !
! wcml, xwml and common interfaces). All calls to the libary are via this    !
! module, this makes it easy to remove FoX in an enviroment where it can not !
! be compiled.                                                               !
!                                                                            !
! Currently FoX version 3.0 is used. see:                                    !
!                http://www.uszla.me.uk/FoX                                  !
! For more information regarding using, extending and developing the CML     !
! output format in GULP and its use see:                                     !
!                http://www.eminerals.org/~andreww/escience/gulpcml.html     !
!                                                                            !
! If you beleve you have found a bug in this module contact the author.      !
!                                                                            !
!                                             Andrew Walker (c) 2006, 2007   !
!                                                     awal05@esc.cam.ac.uk   !
!                                                                            !
!****************************************************************************!

module gulp_cml

#ifndef NOFOX
  ! Interfaces from the FoX Libary   
  use FoX_wxml
  use FoX_wcml
  use FoX_common, only : str, operator(//)
#endif

  ! Interfaces from gulp modules
  use datatypes

  private
  public :: cmlfile, lcml, lvcml, cmlfilename, gulp_cml_init, &
            gulp_cml_exit, gulp_cml_structure, gulp_cml_outkey, & 
            gulp_cml_print_born_charges, gulp_cml_add_minimise_step, &
            gulp_cml_output_derivs, gulp_cml_endmodule, gulp_cml_startmodule, &
            gulp_cml_startstep, gulp_cml_endstep, gulp_cml_print_epot_lattice, &
            gulp_cml_outener, gulp_cml_PDFstats, gulp_cml_outneutron, &
            gulp_cml_outpdf, gulp_cml_output_dielectric, &
            checkstatus

  logical, save :: lcml = .false.             ! Is cml output selected?
  logical, save :: lvcml = .false.            ! is verbose cml output selected?
  character(len=80), save  :: cmlfilename     ! CML file name 
                                              ! same restrictions as that in 
                                              ! the rest of the code (80 chars) 
#ifndef NOFOX
  type(xmlf_t), save :: cmlfile               ! file handle for the cml output file
                                              ! adds comments at the start of each output
                                              ! routine with a public interface
#endif
  logical, parameter :: cmldebugging = .true. ! Adds comments to cml output and checks lcml is
                                              ! set for each public sub.
contains 

  
!****************************************************************************!
!                                                                            !
!       ******               PUBLIC  SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!


subroutine gulp_cml_init()

!============================================================================!
! Open the CML document, XML output file and add the start time. Also add    !
!     namespaces, metadata and stylesheets                                   !
!                                                                            !
! First full version: 23-VIII-2006 AMW                                       !
! Modified (minor clean up): 3-V-2007 AMW                                    !
!============================================================================!

  use parallel, only : nprocs

  implicit none
#ifndef NOFOX
  
  ! Internal vars 
  character(len=24) :: cdatetime

  ! Check CML is set up
  call checkstatus("gulp_cml_init")

  ! Get the date string
  call get_iso_time(cdatetime)

  ! Check to see if cmlfilename is set - if not set it to 'gulp_cml.xml'
  if (index(cmlfilename,'.xml').eq.0) cmlfilename = 'gulp_cml.xml'

  ! Open the cml file handle and write the namespaces and stlylesheet
  call cmlBeginFile(unit=-1, filename=trim(cmlfilename),xf=cmlfile)
  call cmlAddNamespace(cmlfile,'gulp','http://www.ivec.org/GULP/') 
  call cmlAddNamespace(cmlfile,'gulp-units','http://www.ivec.org/GULP/units')
  call cmlAddNamespace(cmlfile,'eMinerals','http://www.eminerals.org/namespace')
  call xml_AddXMLStylesheet(cmlfile,href='http://www.eminerals.org/XSLT/display.xsl',type='text/xml')
  call cmlStartCml(cmlfile)

! Metadata about the xml document
  call cmlAddMetadata(cmlfile, name='eMinerals:cmlSubsetVersion', content='0.9')
     
! First metadata list (about the calculation part of the eMinerals CML subset)
  call cmlStartMetadataList(cmlfile)
    call cmlAddMetadata(cmlfile, name='eMinerals:Program', content='GULP')
    call cmlAddMetadata(cmlfile, name='eMinerals:Version', content='3.0.2-beta')
    call cmlAddMetadata(cmlfile, name='eMinerals:Nodes', content=str(nprocs))
    call cmlAddMetadata(cmlfile, name='eMinerals:StartTime', content=trim(cdatetime))
  call cmlEndMetadataList(cmlfile)    
#endif

end subroutine gulp_cml_init

  
subroutine gulp_cml_exit()

!============================================================================!
! Close the CML document, XML output file and add the end time               !
!                                                                            !
! First full version: 23-VIII-2006 AMW                                       !
! Modified (minor clean up): 3-V-2007 AMW                                    !
!============================================================================!

  implicit none
#ifndef NOFOX
  
  ! Internal vars 
  character(len=24) :: cdatetime

  ! Check CML is set up
  call checkstatus("gulp_cml_exit")

  ! Get the date string and add it as metadata
  call get_iso_time(cdatetime)
  call cmlAddMetadata(cmlfile, name='eMinerals:EndTime', content=trim(cdatetime))

  ! Close the document and file
  call cmlEndCml(cmlfile)
  call xml_Close(cmlfile)
#endif

end subroutine gulp_cml_exit


subroutine gulp_cml_structure(cfgnumber)

!============================================================================!
! Outputs information to xml file about a configuration                      !
! based on outopt.f and setcfg.f and modified.                               !
!                                                                            !
! Arguments: modulename - a string used to name the main module to hold the  !
!                         data.                                              !
!            cfgnumber  - configuration number to output.                    !
!                                                                            !
! First full version: 2-VIII-2006 AMW                                        !
! Modified (minor clean up): 3-V-2007 AMW                                    !
!============================================================================!

  use configurations, only : names, ndimen, ncfg, nsuper, tempcfg, presscfg
  use current, only : nasym, occuf, numat, qf
  use ksample, only : nxks, nyks, nzks

  implicit none

  ! Arguments 
  integer, intent(in) :: cfgnumber
#ifndef NOFOX

  ! Internal vars
  integer, dimension(3) :: is
  integer :: ind
  character(len=60) :: outform
  character(len=60) :: calc_formula

  ! Check CML is set up
  call checkstatus("gulp_cml_structure")

  call cmlStartParameterList(cmlfile, title='Configuration information', &
        & dictRef='gulp:configinfo')

    ! Output the system name and size
    if (names(cfgnumber)(1:1).eq.' ') then
      call cmlAddParameter(cmlfile, name='Configuration name', value='No name')
      call cmlAddParameter(cmlfile, name='Configuration number', value=cfgnumber, units='units:countable')
      call cmlAddParameter(cmlfile, name='Total number of configurations', value=ncfg, units='units:countable')
    else
      call cmlAddParameter(cmlfile, name='Configuration name', value=trim(names(cfgnumber)))
      call cmlAddParameter(cmlfile, name='Configuration number', value=cfgnumber, units='units:countable')
      call cmlAddParameter(cmlfile, name='Total number of configurations', value=ncfg, units='units:countable')
    endif
   
    outform =  calc_formula(60_i4)
    call cmlAddParameter(cmlfile, name='Chemical formula of config', &
       & value=trim(outform), units='units:dimensionless', &
       & dictRef='gulp:formula')

    call cmlAddParameter(cmlfile, name='Number of irreducible atoms and Shells', value=nasym, units='units:countable')
    call cmlAddParameter(cmlfile, name='Total number of atoms and shells', value=numat, units='units:countable')
    call cmlAddParameter(cmlfile, name='Dimensionality', value=ndimen(cfgnumber), units='units:countable')

    call cmlAddParameter(cmlfile, name='Charge', value=sum(qf(1:numat)*occuf(1:numat)), &
          &  units='gulp-units:electronicCharge')

    ! NB Supercell dimensions are packed in an 'intresting' way in
    ! the module variable nsuper in configurations. Unpack here and dump
    ! out - also have to care about the dimensionality and give an
    ! array of 1s for the no-supercell case(s).
    if (nsuper(cfgnumber).gt.1) then
        ind = nsuper(cfgnumber)
        is(1) = ind/10000
        ind = ind - 10000*is(1)
        is(2) = ind/100
        is(3) = ind - 100*is(2)
    else
        is(1:3) = 1
    endif
    call cmlAddParameter(cmlfile, name='Supercell dimensions',  &
       &  value=is(1:ndimen(cfgnumber)), units='units:countable', &
       &  dictRef = 'gulp:supercell')
    ! shrink factors...
    if ((nxks(cfgnumber)*nyks(cfgnumber)*nzks(cfgnumber)).gt.0) then
        is(1) = nxks(cfgnumber)
        is(2) = nyks(cfgnumber)
        is(3) = nzks(cfgnumber)
    else
        is(1:3) = 1 
    endif
    call cmlAddParameter(cmlfile, name='Shrink factors',  &
       &  value=is(1:ndimen(cfgnumber)), units='units:countable', &
       &  dictRef = 'gulp:shrink')
    call cmlAddParameter(cmlfile, name='Temperature of configuration', &
       & value=tempcfg(cfgnumber), units='gulp-units:K', &
       & dictRef='gulp:tempconfig')
    if (ndimen(cfgnumber).eq.3) then
       call cmlAddParameter(cmlfile, name='Pressure of configuration', &
          & value=presscfg(cfgnumber), units='gulp-units:GPa', &
          & dictRef='gulp:pressconfig')
     else
       call cmlAddParameter(cmlfile, name='Pressure of configuration', &
          & value='undefined', units='gulp-units:GPa', &
          & dictRef='gulp:pressconfig')
     endif

  call cmlEndParameterList(cmlfile)

  call dump_current_config
  call cml_symout
#endif

end subroutine gulp_cml_structure


subroutine gulp_cml_outkey()

!============================================================================!
! Outputs information to xml file about a configuration                      !
! based on outopt.f and setcfg.f and modified.                               !
!                                                                            !
! Arguments: modulename - a string used to name the main module to hold the  !
!                         data.                                              !
!            cfgnumber  - configuration number to output.                    !
!                                                                            !
! First full version: 2-VIII-2006                                            !
! Modified (minor clean up): 3-V-2007 AMW                                    !
!============================================================================!
    
  use control
  use element
  use general
  use mdlogic
  use molecule
  use m_pdfneutron !ers29 keywords
  use parallel
  use symmetry

  implicit none
#ifndef NOFOX

  ! Check CML is set up
  call checkstatus("gulp_cml_outkey")
    
  ! Open paramiter list
  call cmlStartParameterList(cmlfile, title='Output keywords',   &
      & dictRef='gulp:outkey')

    ! output parametes for each gulp keyword...
  call cmlAddParameter(cmlfile, name='perform fitting run',          &
      & value=lfit,dictRef='gulp:fitkey')

  call cmlAddParameter &
      & (cmlfile, name='transition state search by rfo method',         & 
      & value=ltran,          &
      & dictRef='gulp:trankey')

  call cmlAddParameter(cmlfile, name='perform optimisation run',                 &
      & value=(lopt.or.lrfo),      &
      & dictRef='gulp:optkey')

  call cmlAddParameter(cmlfile, name='perform gradient run',&
      & value=lgrad, dictRef='gulp:gradkey')

  call cmlAddParameter(cmlfile, name='do not calculate the energy',&
      & value=lnoenergy,dictRef='gulp:noenergykey')

  call cmlAddParameter(cmlfile, name='perform single point run',    &
      & value=(.not.lnoenergy.or.lgrad.or.lopt.or.lrfo.or.ltran.or.lfit),dictRef='gulp:key')

  call cmlAddParameter(cmlfile, name='Perform Monte Carlo run', value=lmc, dictRef='gulp:montecarlokey') 

  call cmlAddParameter(cmlfile, name='Perform Molecular Dynamics run', value=lmd, dictRef='gulp:moleculardynamicskey') 

  call cmlAddParameter(cmlfile, name='Use minimum image algorithm', value=lminimage, dictRef='gulp:minimumiamgekey')

  call cmlAddParameter(cmlfile, name ='perform free energy run', value=lfree, dictRef='gulp:freekey')

  call cmlAddParameter(cmlfile, name='use ZSISA approach to free energy derivatives', value=lzsisa,dictRef='gulp:zsisakey')

  call cmlAddParameter(cmlfile, name='optimise static energy before free energy',value=(index(keyword,'stat').ne.0), &
      &  dictRef='gulp:statkey')

  call cmlAddParameter(cmlfile, name='do not optimise during bulk calculation', &
      & value=(index(keyword,'bulk').ne.0), dictRef='gulp:key')

  call cmlAddParameter(cmlfile, name='constant pressure calculation',        &
      & value=lconp,dictRef='gulp:conpkey')

  call cmlAddParameter(cmlfile, name='constant volume calculation',        &
      & value=lconv,dictRef='gulp:convkey')

  call cmlAddParameter(cmlfile, name='optimise unit cell only',      &
      & value=lcello,dictRef='gulp:cellokey')

  call cmlAddParameter(cmlfile, name='no flags to be read in - assumed to be zero',  &
      & value=(index(keyword,'nofl').ne.0), &
      & dictRef='gulp:noflkey')

  call cmlAddParameter(cmlfile, name='only shells and radii to be optimised', value=lshello, dictRef='gulp:shellokey')

  call cmlAddParameter(cmlfile, name='only radii to be optimised', &
      &  value=(index(keyword,' brea').ne.0.or.index(keyword,'brea').eq.1), dictRef='gulp:breakey')

  call cmlAddParameter(cmlfile, name='radii not to be optimised',  &
      & value=(index(keyword,'nobr').ne.0), dictRef='gulp:nobrkey')

  call cmlAddParameter(cmlfile, name='only isotropic cell expansion allowed', &
      &  value=(index(keyword,'iso').ne.0), dictRef='gulp:isokey')

  call cmlAddParameter(cmlfile, name='electrostatic energy to be excluded', &
      &  value=(index(keyword,'noel').ne.0), dictRef='gulp:noelkey')

  call cmlAddParameter(cmlfile, name='non-charge neutral unit cell allowed', &
      &  value=(index(keyword,'qok').ne.0), dictRef='gulp:qokkey')

  call cmlAddParameter(cmlfile, name='include dipolar correction energy', value=ldipole, dictRef='gulp:dipolekey')

  call cmlAddParameter(cmlfile, name='calculate properties for final geometry', value=lprop, dictRef='gulp:propkey')

  call cmlAddParameter(cmlfile, name='use old units where appropriate', &
      &  value=(index(keyword,'oldu').ne.0), dictRef='gulp:oldukey')

  call cmlAddParameter(cmlfile, name='calculate phonons for final geometry', value=lphon, dictRef='gulp:phonkey')

  call cmlAddParameter(cmlfile, name='exclude non-analytic correction to gamma point phonons', & 
      & value=(index(keyword,'nono').ne.0), dictRef='gulp:nonokey')

  call cmlAddParameter(cmlfile, name='generate K points for the full centred cell', &
      & value=lkfull, dictRef='gulp:kfullkey')

  call cmlAddParameter(cmlfile, name='perform defect calculation after bulk run', &
      & value=ldefect, dictRef='gulp:defectkey')

  call cmlAddParameter(cmlfile, name='calculate defect frequencies', &
      & value=(ldefect.and.lfreq), dictRef='gulp:deffreeqkey')

  call cmlAddParameter(cmlfile, name='output region 1 at start of defect run', & 
      & value=(ldefect.and.(index(keyword,'regi').ne.0)), dictRef='gulp:regikey')

  call cmlAddParameter(cmlfile, name='use region 2a displacements in 3 and 4 body energy', & 
      & value=(ldefect.and.(index(keyword,'r234').ne.0)), dictRef='gulp:r1234key')

  call cmlAddParameter(cmlfile, name='do not treat region 2b anisotropically',                  & 
      & value=(ldefect.and.(index(keyword,'noan').ne.0)),               & 
      & dictRef='gulp:noanisokey')

  call cmlAddParameter(cmlfile, name='generate phonon eigenvectors',     & 
      & value=leigen,      & 
      & dictRef='gulp:eigenkey')

  call cmlAddParameter(cmlfile, name='calculate mean kinetic energy per atom during phonon calculation',     & 
      & value=lmeanke,      & 
      & dictRef='gulp:meankekey')

  call cmlAddParameter(cmlfile, name='compute bond increment charges',     & 
      & value=lqbond,      & 
      & dictRef='gulp:qbondkey')

  call cmlAddParameter(cmlfile, name='use genetic genetic',        & 
      & value=lga,dictRef='gulp:gakey')

  call cmlAddParameter(cmlfile, name='use simulated annealling',  & 
      & value=lanneal,dictRef='gulp:annealkey')

  call cmlAddParameter(cmlfile, name='estimate IR intensities',    & 
      & value=linten, dictRef='gulp:intenkey')

  call cmlAddParameter(cmlfile, name='reduce symmetry according to imaginary modes',      & 
      & value=(index(keyword,'lowe').ne.0),         & 
      & dictRef='gulp:lowekey')

  call cmlAddParameter(cmlfile, name='calculate electrostatic site potentials',    & 
      & value=(index(keyword,'pot').ne.0),              & 
      & dictRef='gulp:potkey')

  call cmlAddParameter(cmlfile, name='set the average site potential to zero',      & 
      & value=(index(keyword,'zer').ne.0),               & 
      & dictRef='gulp:zerkey')

  call cmlAddParameter(cmlfile, name='calculate electrostatic site potentials for all of region 1',        & 
      & value=(index(keyword,'nodp').ne.0),dictRef='gulp:nodpkey')

  call cmlAddParameter(cmlfile, name='calculate electrostatic electric field gradient',   & 
      & value=(index(keyword,'efg').ne.0), &
      & dictRef='gulp:efgkey')

  ! Symmetry stuff
  call cmlAddParameter(cmlfile, name='turn off symmetry after structure generation',  & 
          & value=(.not.lsym), &
          & dictRef='gulp:nosymkey')

  call cmlAddParameter(cmlfile, name='generate full centred unit cell when symmetry is removed', &
          & value=((.not.lsym).and.(index(keyword,' full').ne.0.or.index(keyword,'full').eq.1)),dictRef='gulp:fullkey')

  call cmlAddParameter(cmlfile, name='turn off use of symmetry after structure generation', & 
          & value=lsymoff,dictRef='gulp:symoffkey')

  call cmlAddParameter(cmlfile, name='relax shell positions and radii during fitting',&
          & value=(index(keyword,'simu').ne.0),dictRef='gulp:simukey')

  call cmlAddParameter(cmlfile, name='relax structure during fitting',&
          & value=lrelax,dictRef='gulp:relaxkey')

  call cmlAddParameter(cmlfile, name='symmetry not to be used for first derivatives',&
          & value=(.not.lsymdok),dictRef='gulp:nosdervkey')

  call cmlAddParameter(cmlfile, name='dump constraints to restart file',&
          & value=(index(keyword,'outc').ne.0),dictRef='gulp:outckey')

  call cmlAddParameter(cmlfile, name='symmetry not to be used for second derivatives',&
          & value=(index(keyword,'nod2').ne.0),&
          & dictRef='gulp:nod2symkey')

  call cmlAddParameter(cmlfile, name='symmetry not to be used in defect calculation', &
          & value=(index(keyword,'nods').ne.0), &
          & dictRef='gulp:nodsynkey')

  call cmlAddParameter(cmlfile, name='use spatial decomposition algorithm',&
          & value=(lspatial),dictRef='gulp:spatialkey')

  call cmlAddParameter(cmlfile, name='use QEq electronegativity equalisation method',&
          & value=(leem.and.lqeq),&
          & dictRef='gulp:QEqkey')

  call cmlAddParameter(cmlfile, name='use S-M electronegativity equalisation method',&
          & value=(leem.and.lSandM),&
          & dictRef='gulp:SandMkey')

  call cmlAddParameter(cmlfile, name='use EEM electronegativity equalisation method - old parameters',&
          & value=(leem.and.(index(keyword,'oldeem').ne.0)),dictRef='gulp:oldEEMkey')

  call cmlAddParameter(cmlfile, name='use EEM electronegativity equalisation method - new parameters',&
          & value=(leem.and.(.not.((index(keyword,'oldeem').ne.0).or.lSandM.or.lqeq))),dictRef='gulp:EEMkey')

  call cmlAddParameter(cmlfile, name='calculate bond lengths',&
          & value=lbond,dictRef='gulp:bondkey')

  call cmlAddParameter(cmlfile, name='calculate average bond lengths',&
          & value=laver,dictRef='gulp:avarkey')

  call cmlAddParameter(cmlfile, name='calculate distance',&
          & value=ldist,dictRef='gulp:distkey')

  call cmlAddParameter(cmlfile, name='calculate angles for valid three body terms',&
          & value=langle,dictRef='gulp:anglekey')

  call cmlAddParameter(cmlfile, name='calculate torsion angles for valid four body terms',&
          & value=ltors,&
          & dictRef='gulp:torskey')

  ! Molecule settings
  call cmlAddParameter(cmlfile, name='find molecules and keep Coulomb terms',&
          &  value=(lmol.and.lmolq), dictRef='gulp:molqkey')

  call cmlAddParameter(cmlfile, name='find molecules and Coulomb subtract 1-2/1-3 terms',&
          & value=(lmol.and.lmolmec), dictRef='gulp:molmeckey')

  call cmlAddParameter(cmlfile, name='find molecules and Coulomb subtract within molecule',&
          & value=(lmol.and..not.(lmolmec.or.lmolq)), dictRef='gulp:molkey')

  call cmlAddParameter(cmlfile, name='fix connectivity based on initial geometry',&
          & value=(lmol.and.lmolfix),  dictRef='gulp:molfixkey')

  call cmlAddParameter(cmlfile, name='compare initial and final structures',&
          & value=(lcomp.and.lopt) ,dictRef='gulp:comparekey')

  call cmlAddParameter(cmlfile, name='approximate initial Hessian by unit diagonal matrix',&
          & value=lunit,&
          & dictRef='gulp:unitkey')

  call cmlAddParameter(cmlfile, name='use full numerical Hessian in BFGS for fitting',&
          & value=lfbfgs,&
          & dictRef='gulp:fBFGSkey')

  call cmlAddParameter(cmlfile, name='use conjugate gradients minimiser',&
          & value=lconj,dictRef='gulp:conjkey')

  call cmlAddParameter(cmlfile, name='ensure Hessian acts as positive-definite in Newton-Raphson',&
          & value=(index(keyword,'posi').ne.0),dictRef='gulp:posikey')

  call cmlAddParameter(cmlfile, name='optimisation step to be determined by RFO method',&
          & value=lrfo,&
          & dictRef='gulp:rfokey')

  call cmlAddParameter(cmlfile, name='output eigenvalues and vectors of diagonalised Hessian',&
          & value=(lrfo.and.(index(keyword,'hess').ne.0)),&
          & dictRef='gulp:outhesseigenkey')

  call cmlAddParameter(cmlfile, name='output inverse Hessian matrix when calculated exactly',&
          & value=(.not.lrfo.and.(index(keyword,'hess').ne.0)),&
          & dictRef='gulp:outhessinvkey')

  call cmlAddParameter(cmlfile, &
          & name='use Davidon-Fletcher-Powell Hessian update',&
          & value=(index(keyword,'dfp').ne.0),&
          & dictRef='gulp:dfpkey')

  call cmlAddParameter(cmlfile, &
          & name='suppress frequency output after phonon calculation',&
          & value=(index(keyword,'nofr').ne.0),&
          & dictRef='gulp:nofreqkey')

  call cmlAddParameter(cmlfile, name='broaden density of states curves',&
          & value=lbroad,&
          & dictRef='gulp:broadkey')

  call cmlAddParameter(cmlfile,&
          &  name='do not use Brillouin zone symmetry when generating k points',&
          & value=((index(keyword,'noks').ne.0).or.lnoksym),dictRef='gulp:nokskey') ! ers29 modified

  call cmlAddParameter(cmlfile,&
          & name='output bulk symmetry operators',&
          & value=(index(keyword,'opera').ne.0),dictRef='gulp:operakey')

  call cmlAddParameter(cmlfile,&
          & name='do not freeze out atoms with no derivatives',&
          & value=(index(keyword,'noex').ne.0), &
          & dictRef='gulp:noexkey')

  call cmlAddParameter(cmlfile,&
          & name='do not use list based methods for 3 & 4-body terms in MD',&
          & value=(index(keyword,'noli').ne.0),dictRef='gulp:nolist_mdkey')

  call cmlAddParameter(cmlfile,&
          & name='do not print out list of k points',&
          & value=(index(keyword,'nokp').ne.0),dictRef='gulp:nokpkey')

  call cmlAddParameter(cmlfile, name='save defect matrices for restart',&
          & value=lsave,&
          & dictRef='gulp:savekey')

  call cmlAddParameter(cmlfile, name='restore defect matrices from disk for restart',&
          & value=lrest,&
          & dictRef='gulp:restkey')

  call cmlAddParameter(cmlfile,name='exclude the zero point energy from phonon/free energy calcns',&
          & value=(index(keyword,'noze').ne.0),dictRef='gulp:nozekey')

  call cmlAddParameter(cmlfile, name='surface energy to be calculated as per MARVIN',&
          & value=lmarvreg2,&
          & dictRef='gulp:marvreg2key')

  call cmlAddParameter(cmlfile,&
          & name='no automatic cutoff to be used for repulsion',&
          & value=(index(keyword,'norep').ne.0),&
          & dictRef='gulp:norepkey')

  call cmlAddParameter(cmlfile,&
          & name='no molecular internal KE in initial MD velocity', &
          & value=(index(keyword,'nomol').ne.0),&
          & dictRef='gulp:nomolkey')

  ! Neutron settings
  call cmlAddParameter(cmlfile,&
          & name='store all eigenvectors and frequencies after calculation', &
          & value=(lmakeeigarray),&
          & dictRef='gulp:makeeigarraykey')

  call cmlAddParameter(cmlfile,&
          & name='output atomic information for "cores" used', &
          & value=(lcoreinfo),&
          & dictRef='gulp:coreinfokey')

  call cmlAddParameter(cmlfile,&
          & name='calculate Pair Distribution Functions',&
          & value=(lpdf),&
          & dictRef='gulp:pdfkey')

  call cmlAddParameter(cmlfile,&
          & name='suppress output of peak widths for Pair Distribution Functions',&
          & value=(lnowidth),&
          & dictRef='gulp:nowidthkey')

  call cmlAddParameter(cmlfile,&
         & name='suppress output of partial Pair Distribution Functions',&
         & value=(.not.(lpartial)),&
         & dictRef='gulp:nopartialkey')

  call cmlAddParameter(cmlfile,&
          & name='use frequency cut-offs in PDF',&
          & value=(lfreqcut),&
          & dictRef='gulp:PDFcutkey')

  call cmlAddParameter(cmlfile,&
          & name='set rejected frequencies to limits in PDFS',&
          & value=(lkeepcut),&
          & dictRef='gulp:PDFkeepkey')

  ! Close the parameter list
  call cmlEndParameterList(cmlfile)
#endif
end subroutine gulp_cml_outkey

subroutine gulp_cml_print_epot_lattice(v)
  use element, only : maxele
  use current, only : nasym,  nrel2, &
                  & nat, nftype

  real(kind=dp), dimension(:), intent(in)  :: v

#ifndef NOFOX
  integer :: ii,  n, ion, inat, itype
  character(len=4)   :: lab

  call checkstatus("gulp_cml_print_epot_lattice")

  call cmlStartPropertyList(cmlfile, title="Electrostatic potential at atomic positions", &
       dictref="gulp:epot_lattice")
  n = 0
  do ion=1, nasym
     ii = nrel2(ion)
     inat = nat(ii)
     if (inat.le.maxele) then
          n = n + 1
          itype = nftype(ii)
          call label(inat,itype,lab)
          call cmlAddProperty(cmlfile, title=trim(lab), role="Electrostatic potential", &
             value=v(n), units="gulp-units:eV", ref=str(n))
             ! Note v(n) was v(i) which was wrong...
     endif
  enddo
 
  call cmlEndPropertyList(cmlfile)
#endif

end subroutine gulp_cml_print_epot_lattice


subroutine gulp_cml_print_born_charges(born_charge)
!============================================================================!
! Outputs information to xml file about Born charges.                        !
!                                                                            !
! First full version: lost in time AMW                                       !
!============================================================================!

    use element, only : maxele
    use current, only : nasym,  nrel2, &
                  & nat, nftype

    implicit none

    real(kind=dp), dimension(:,:,:), intent(in)  :: born_charge

    real(kind=dp), dimension(:,:), allocatable :: bec
    integer :: i, ii, j, n, ion, inat, itype
    character(len=4)   :: lab
#ifndef NOFOX

    call checkstatus("gulp_cml_print_born_charges")
    call cmlStartPropertyList(cmlfile, title="Born Effective Charges", &
         dictref="gulp:born_effective_charge")

    n = 0
    do ion=1, nasym
       ii = nrel2(ion)
       inat = nat(ii) 
       if (inat.le.maxele) then
            n = n + 1
            itype = nftype(ii)
            call label(inat,itype,lab)
            allocate(bec(3,3))
            do i=1,3
               do j=1,3
                  bec(i,j) = born_charge(i,j, ii)
               end do
            end do
        
            call cmlAddProperty(cmlfile, title="Born Effective Charge of atom "//trim(lab), &
               value=bec, units="gulp-units:electron", ref=str(n))
            deallocate(bec)
        end if
   end do
   call cmlEndPropertyList(cmlfile)
#endif

end subroutine gulp_cml_print_born_charges


subroutine gulp_cml_add_minimise_step(cycle, energy, gnorm, cputime)
!============================================================================!
! Outputs information to xml file about an optimisation step.                !
!                                                                            !
! First full version: lost in time AMW                                       !
!============================================================================!

   implicit none

   integer, intent(in) :: cycle
   real(kind=dp), intent(in), optional :: energy
   real(kind=dp), intent(in), optional :: gnorm
   real(kind=dp), intent(in), optional :: cputime
#ifndef NOFOX

   call checkstatus("gulp_cml_add_minimise_step")
   Call cmlStartStep(cmlfile, index=cycle)
       Call cmlAddProperty(cmlfile, title='Cycle', value=cycle, units="units:countable")
       if (present(energy)) Call cmlAddProperty(cmlfile, title='Energy', value=energy, units="gulp-units:eV")
       if (present(gnorm)) Call cmlAddProperty(cmlfile, title='Gnorm', value=gnorm, units="units:dimensionless")
       if (present(cputime)) Call cmlAddProperty(cmlfile, title='CPU time', value=cputime, units="gulp-units:seconds")
       if (lvcml) then
          Call dump_current_config
       endif 
   Call cmlEndStep(cmlfile)
#endif
end subroutine gulp_cml_add_minimise_step


subroutine gulp_cml_output_dielectric(sdct, srind, hfct, hrind, piezo, piezs)

!============================================================================!
! Outputs information to xml file about static dielectric constants and      !
! refractive indicies.                                                       !
! First full version: 30-X-2007 AMW                                          !
!============================================================================!

    implicit none

    real(kind=dp), intent(in), optional  :: sdct(3,3)
    real(kind=dp), intent(in), optional  :: srind(3)
    real(kind=dp), intent(in), optional  :: hfct(3,3)
    real(kind=dp), intent(in), optional  :: hrind(3)
    real(kind=dp), intent(in), optional  :: piezo(6,3)
    real(kind=dp), intent(in), optional  :: piezs(6,3)
#ifndef NOFOX

    call checkstatus("gulp_cml_output_dielectric")
    call cmlStartPropertyList(cmlfile)
        if ( present(sdct) ) call cmlAddProperty(cmlfile, value=sdct, &
            & title='Relative static permittivity tensor', &
            & units='units:dimensionless', dictRef='gulp:sdct')
        if ( present(srind) ) call cmlAddProperty(cmlfile, value=srind, &
            &  title='Static refractive indices', &
            & units='gulp-units:10-11CN-1', dictRef='gulp:srind')
        if ( present(hfct) ) call cmlAddProperty(cmlfile, value=hfct, &
            & title='Relative permittivity tensor at high frequency', &
            & units='units:dimensionless', dictRef='gulp:hfct')
        if ( present(hrind) ) call cmlAddProperty(cmlfile, value=hrind, &
            &  title='High frequency refractive indices', &
            & units='gulp-units:10-11CN-1', dictRef='gulp:hrind')
        if ( present(piezo) ) call cmlAddProperty(cmlfile, value=piezo, &
            & title='Piezoelectric Strain Matrix', &
            & units='gulp-units:Cm.2', dictRef='gulp:piezo')
        if ( present(piezs) ) call cmlAddProperty(cmlfile, value=piezs, &
            & title='Piezoelectric Stress Matrix', &
            & units='gulp-units:10-11CN-1', dictRef='gulp:piezs')
    call cmlEndPropertyList(cmlfile)
#endif

end subroutine gulp_cml_output_dielectric


subroutine gulp_cml_output_derivs(elcon, compliances, bulkmod_reuss, bulkmod_voigt, bulkmod_hill, &
    & shearmod_reuss, shearmod_voigt, shearmod_hill, vs_reuss, vs_voigt, vs_hill, & 
    & vp_reuss, vp_voigt, vp_hill)  

!============================================================================!
! Outputs information to xml file about a elastic constants and derived      !
! quantities. Output is as a cml propery list builf using FoX_wcml.          !
!                                                                            !
! First full version: 23-VIII-2006 AMW                                       !
!============================================================================!

    implicit none

    real(kind=dp), intent(in), optional :: elcon(6,6)       ! Elastic constants matrix - reduced notation
    real(kind=dp), intent(in), optional :: compliances(6,6) ! Elastic compliances matrix - reduced notation
    real(kind=dp), intent(in), optional :: bulkmod_reuss    ! Reuss bound on poly x-tal bulk mod
    real(kind=dp), intent(in), optional :: bulkmod_voigt    ! Voigt bound on poly x-tal bulk mod
    real(kind=dp), intent(in), optional :: bulkmod_hill     ! Hill avarage of poly x-tal bulk mod
    real(kind=dp), intent(in), optional :: shearmod_reuss   ! Reuss bound on poly x-tal shear mod
    real(kind=dp), intent(in), optional :: shearmod_voigt   ! Voigt bound on poly x-tal shear mod
    real(kind=dp), intent(in), optional :: shearmod_hill    ! Hill avarage of poly x-tal shear mod
    real(kind=dp), intent(in), optional :: vs_reuss         ! Shear velocity from Reuss bound
    real(kind=dp), intent(in), optional :: vs_voigt         ! Shear velocity from Voigt bound
    real(kind=dp), intent(in), optional :: vs_hill          ! Shear velocity from Hill avarage
    real(kind=dp), intent(in), optional :: vp_reuss         ! Compressional velocity from Reuss bound
    real(kind=dp), intent(in), optional :: vp_voigt         ! Compressional velocity from Voigt bound
    real(kind=dp), intent(in), optional :: vp_hill          ! Compressional velocity from Hill avarage
#ifndef NOFOX

    call checkstatus("gulp_cml_output_derivs")
    call cmlStartPropertyList(cmlfile)
        if (present(elcon)) call cmlAddProperty(cmlfile, &
            & value=elcon,title='Elastic Constant Matrix',units='units:GPa')
        if (present(compliances)) call cmlAddProperty(cmlfile,& 
            & value=compliances,title='Elastic Commpliance Matrix',units='units:GPa-1')
        if (present(bulkmod_reuss)) call cmlAddProperty(cmlfile, &
            & value=bulkmod_reuss,title='Bulk modulus (Reuss)', dictRef='gulp:bulkmodreuss',units='units:GPa')
        if (present(bulkmod_voigt)) call cmlAddProperty(cmlfile, &
            & value=bulkmod_voigt,title='Bulk modulus (Voigt)', dictRef='gulp:bulkmodvoigt',units='units:GPa')
        if (present(bulkmod_hill)) call cmlAddProperty(cmlfile, &
            & value=bulkmod_hill,title='Bulk modulus (Hill)',  dictRef='gulp:bulkmodhill',units='units:GPa')
        if (present(shearmod_reuss)) call cmlAddProperty(cmlfile, &
            & value=shearmod_reuss,title='Shear modulus (Reuss)', dictRef='gulp:shearmodreuss', &
            & units='units:GPa')
        if (present(shearmod_voigt)) call cmlAddProperty(cmlfile, &
            & value=shearmod_voigt,title='Shear modulus (Voigt)', dictRef='gulp:shearmodvoigt', &
            & units='units:GPa')
        if (present(shearmod_hill)) call cmlAddProperty(cmlfile, &
            & value=shearmod_hill,title='Shear modulus (Hill)', dictRef='gulp:shearmodhill',units='units:GPa')
        if (present(vs_reuss)) call cmlAddProperty(cmlfile, &
            & value=vs_reuss,title='Velocity S-wave (Reuss)', dictRef='gulp:velswavereuss',units='units:km.s-1')
        if (present(vs_voigt)) call cmlAddProperty(cmlfile, &
            & value=vs_voigt,title='Velocity S-wave (Voigt)', dictRef='gulp:velswavevoigt',units='units:km.s-1')
        if (present(vs_hill)) call cmlAddProperty(cmlfile, &
            & value=vs_hill,title='Velocity S-wave (Hill)', dictRef='gulp:velpwavehill',units='units:km.s-1')
        if (present(vp_reuss)) call cmlAddProperty(cmlfile, &
            & value=vp_reuss,title='Velocity P-wave (Reuss)', dictRef='gulp:velpwavereuss',units='units:km.s-1')
        if (present(vp_voigt)) call cmlAddProperty(cmlfile, &
            & value=vp_voigt,title='Velocity P-wave (Voigt)', dictRef='gulp:velpwavevoigt',units='units:km.s-1')
        if (present(vp_hill)) call cmlAddProperty(cmlfile, &
            & value=vp_hill,title='Velocity P-wave (Hill)', dictRef='gulp:velpwavehill',units='units:km.s-1')
    call cmlEndPropertyList(cmlfile)
#endif

end subroutine gulp_cml_output_derivs


subroutine gulp_cml_endmodule
!============================================================================!
! Simple wrapper for the FoX_wcml cmlEndModule interface. No arguments       ! 
!                                                                            !
! First full version: 23-VIII-2006 AMW                                       !
!============================================================================!

#ifndef NOFOX
  call checkstatus("gulp_cml_endmodule")
  call cmlEndModule(cmlfile)
#endif

end subroutine gulp_cml_endmodule


subroutine gulp_cml_startmodule(serial, title, id, convention, dictRef, role)
!============================================================================!
! Simple wrapper for the FoX_wcml cmlStartModule interface.                  ! 
! 
! First full version: 23-VIII-2006 AMW                                       !
!============================================================================!

  implicit none

  character(len=*), intent(in), optional :: serial
  character(len=*), intent(in), optional :: title
  character(len=*), intent(in), optional :: id
  character(len=*), intent(in), optional :: convention
  character(len=*), intent(in), optional :: dictRef
  character(len=*), intent(in), optional :: role

#ifndef NOFOX
  call checkstatus("gulp_cml_startmodule")
  call cmlStartmodule(cmlfile, serial, title, id, convention, dictRef, role)
#endif

end subroutine gulp_cml_startmodule


subroutine gulp_cml_startstep(cycle, type)
!============================================================================!
! Simple wrapper for the FoX_wcml cmlStartStep interface.                    ! 
!                                                                            ! 
! First full version: 27-IX-2006 AMW                                         !
!============================================================================!
   implicit none
   integer, intent(in), optional  :: cycle
   character(len=*), intent(in), optional :: type
#ifndef NOFOX
   call checkstatus("gulp_cml_startstep")
   Call cmlStartStep(cmlfile, index=cycle, type=type)
#endif
end subroutine gulp_cml_startstep

   
subroutine gulp_cml_endstep
!============================================================================!
! Simple wrapper for the FoX_wcml cmlEndStep interface.                      ! 
!                                                                            ! 
! First full version: 27-IX-2006 AMW                                         !
!============================================================================!
#ifndef NOFOX
   call checkstatus("gulp_cml_endstep")
   Call cmlEndStep(cmlfile)
#endif
end subroutine gulp_cml_endstep

subroutine gulp_cml_outener
!
! Based on outener.f and reduced for XML only output
! This seems easer then trying to cleanly mark out
! outener.f itself...
!
  use cellmultipole
  use configurations
  use control
  use current
  use element,        only : lSandM
  use energies
  use four
  use iochannels
  use shifts
  use six
  use sutton
  use symmetry
  use m_three
  implicit none
#ifndef NOFOX
!
!  Local variables
!
  character(len=10) :: cmmword(4)
  integer(i4)       :: icf
  logical           :: lpress
  real(dp)          :: etot
  real(dp)          :: sft

  data cmmword/'monopole  ','dipole    ','quadrupole','octopole  '/


  call checkstatus("gulp_cml_outener")
  lpress = (abs(press).gt.0.0d0.and.ndim.eq.3)
  sft = shift(nshcfg(ncf))*shscalecfg(ncf)
  etot = erecip + eatom + sft + ethb + ereal + efor + epv + ecmm + ec6 + eoop + emany + &
 &       edipole + ebgd + eself + eqeq + evib + epolar + ebrenner + eforce + esix + &
 &       esregion12 + esregion2 + eeinstein+ ewolfself + ebondorder + eboQself + &
 &       echargecoupled

! What sort of energy are we dumpint out (Module header)
  if (lfree) then
    call cmlStartModule(cmlfile, title='Components of the free energy', dictRef='gulp:outputFreeEner')
  elseif (lpress) then
    call cmlStartModule(cmlfile, title='Components of the enthalpy', dictRef='gulp:outputEnthalpy')
  else
    call cmlStartModule(cmlfile, title='Components of the energy', dictRef='gulp:outputEner')
    call cmlStartParameterList(cmlfile, title='CMM method', dictRef='gulp:CMMmethod')
    if (icmm.gt.0) then
      call cmlAddParameter(cmlfile, name='CMMorder', value=trim(cmmword(icmm)))
    else
      call cmlAddParameter(cmlfile, name='CMMorder', value='No CMM')
    endif
    call cmlEndParameterList(cmlfile)
  endif

! Components
  call cmlStartPropertyList(cmlfile, title='Components', dictRef='gulp:componetsofEnergy')
    call cmlAddProperty(cmlfile, title='Interatomic potentials', value=eatom, & 
         & units='gulp-units:eV', dictRef='gulp:interatompot')
    call cmlAddProperty(cmlfile, title='Three-body potentials', value=ethb,& 
         &  units='gulp-units:eV', dictRef='gulp:threebodypot')
    call cmlAddProperty(cmlfile, title='Four-body potentials', value=efor,& 
         &  units='gulp-units:eV', dictRef='gulp:fourbodypot')
    call cmlAddProperty(cmlfile, title='Out of plane potentials', value=eoop,& 
         &  units='gulp-units:eV', dictRef='gulp:ooppot')
    call cmlAddProperty(cmlfile, title='Six-body potentials', value=esix,& 
         &  units='gulp-units:eV', dictRef='gulp:sixbodypot')
    call cmlAddProperty(cmlfile, title='Many-body potentials', value=emany,& 
         &  units='gulp-units:eV', dictRef='gulp:manybodypot')
    call cmlAddProperty(cmlfile, title='Brenner potentials', value=ebrenner,& 
         &  units='gulp-units:eV', dictRef='gulp:brennerbodypot')
    call cmlAddProperty(cmlfile, title='Bond-order potentials', value=ebondorder,& 
         &  units='gulp-units:eV', dictRef='gulp:bondorderpot')
    call cmlAddProperty(cmlfile, title='Bond-order self energy', value=eboQself,& 
         &  units='gulp-units:eV', dictRef='gulp:bondorderself')
    call cmlAddProperty(cmlfile, title='Charge-coupled potentials', value=echargecoupled,& 
         &  units='gulp-units:eV', dictRef='gulp:chargecoupledpot')
  ! Coulomb terms
    call cmlAddProperty(cmlfile, title='Monopole - monopole (real)', value=ereal,& 
         &  units='gulp-units:eV', dictRef='gulp:ereal')
    call cmlAddProperty(cmlfile, title='Monopole - multipole', value=ecmm,& 
         &  units='gulp-units:eV', dictRef='gulp:ecmm')
    call cmlAddProperty(cmlfile, title='Monopole - monopole (recip)', value=erecip,& 
         &  units='gulp-units:eV', dictRef='gulp:erecip')
    call cmlAddProperty(cmlfile, title='Monopole - monopole (total)', value=(ereal+erecip),& 
         &  units='gulp-units:eV', dictRef='gulp:total')
    call cmlAddProperty(cmlfile, title='Dispersion (real+recip)', value=ec6,& 
         &  units='gulp-units:eV', dictRef='gulp:ec6')
    call cmlAddProperty(cmlfile, title='Polarisation energy', value=epolar,& 
         &  units='gulp-units:eV', dictRef='gulp:epolar')
    call cmlAddProperty(cmlfile, title='Einestein energy', value=eeinstein, & 
         & units='gulp-units:eV', dictRef='gulp:eeinstin')
    call cmlAddProperty(cmlfile, title='Energy shift', value=sft,& 
         &  units='gulp-units:eV', dictRef='gulp:sft')
    call cmlAddProperty(cmlfile, title='Dipole correction energy', value=edipole,& 
         &  units='gulp-units:eV', dictRef='gulp:edipole')
    call cmlAddProperty(cmlfile, title='Neutralising energy', value=ebgd,& 
         &  units='gulp-units:eV', dictRef='gulp:echargebgd')
    call cmlAddProperty(cmlfile, title='Wolf sum self energy', value=ewolfself,& 
         &  units='gulp-units:eV', dictRef='gulp:ewolfself')
    call cmlAddProperty(cmlfile, title='Self energy (EEM/QEq/SM)', value=eself,& 
         &  units='gulp-units:eV', dictRef='gulp:eself')
    if (lSandM) then
      call cmlAddProperty(cmlfile, title='SM Coulomb correction', value=eqeq,& 
         &  units='gulp-units:eV', dictRef='gulp:eqeqSM')
    else
      call cmlAddProperty(cmlfile, title='QEq Coulomb correction', value=eqeq,& 
         &  units='gulp-units:eV', dictRef='gulp:eqeqQEq')
    endif
    ! ML stuff
    call cmlAddProperty(cmlfile, title='Region 1-2 interaction energy', value= esregion12,& 
         &  units='gulp-units:eV', dictRef='gulp:esregion12')
    call cmlAddProperty(cmlfile, title='Region 2-2 interaction energy', value= esregion2,& 
         &  units='gulp-units:eV', dictRef='gulp:esregion2')
    ! Free  energy + external contributions
    call cmlAddProperty(cmlfile, title='Pressure*volume', value=epv,& 
         &  units='gulp-units:eV', dictRef='gulp:epv')
    call cmlAddProperty(cmlfile, title='External_force*distance', value=eforce,& 
         &  units='gulp-units:eV', dictRef='gulp:eforce')
    call cmlAddProperty(cmlfile, title='Vibrational contribution', value=evib,& 
         &  units='gulp-units:eV', dictRef='gulp:evib')
  call cmlEndPropertyList(cmlfile)

! Totals...
  icf = icentfct(ncbl)
  call cmlStartPropertyList(cmlfile, title='Energy Totals', dictRef='gulp:totalEnergy')
  if (lsymopt.and.icf.gt.1) then
! Totals are per primitive and non-primitive unit cell...
    if (lfree) then
      call cmlAddProperty(cmlfile, title='Total free energy per primitive cell', value=etot, & 
         & units='gulp-units:eV', dictRef='gulp:ePrimFreeEnergy')
      call cmlAddProperty(cmlfile, title='Total free energy per non-primitive cell', value=(etot*dble(icf)),& 
         & units='gulp-units:eV', dictRef='gulp:eFullFreeEnergy')
    elseif (lpress) then
      call cmlAddProperty(cmlfile, title='Total enthalpy per primitive cell', value=etot, & 
         & units='gulp-units:eV', dictRef='gulp:ePrimEnthalpy')
      call cmlAddProperty(cmlfile, title='Total enthalpy per non-primitive cell', value=(etot*dble(icf)),& 
         & units='gulp-units:eV', dictRef='gulp:eFullEnthalpy')
    else
      call cmlAddProperty(cmlfile, title='Total energy per primitive cell', value=etot, & 
         & units='gulp-units:eV', dictRef='gulp:ePrimEnergy')
      call cmlAddProperty(cmlfile, title='Total energy per non-primitive cell', value=(etot*dble(icf)),& 
         & units='gulp-units:eV', dictRef='gulp:eFullEnergy')
    endif
  else
! Energy is per full simulation box
    if (lfree) then
      call cmlAddProperty(cmlfile, title='Total free energy', value=etot, & 
         & units='gulp-units:eV', dictRef='gulp:eFreeEnergy')
    elseif (lpress) then
      call cmlAddProperty(cmlfile, title='Total enthalpy', value=etot, & 
         & units='gulp-units:eV', dictRef='gulp:eEnthalpy')
    else
      call cmlAddProperty(cmlfile, title='Total energy', value=etot,& 
         & units='gulp-units:eV', dictRef='gulp:eEnergy')
    endif
  endif
  call cmlEndPropertyList(cmlfile)

! Surface energy calculation

  if (lseok) then
    if (ndim.eq.2) then
      call cmlStartPropertyList(cmlfile, title='2D slab energy', dictRef='gulp:2DEnergy')
        call cmlAddProperty(cmlfile, title='Bulk energy', value=sbulkecfg(ncf),& 
           & units='gulp-units:eV', dictRef='gulp:slabBulkEnergy')
        if (nregions(ncf).gt.1) then
          call cmlAddProperty(cmlfile, title='Surface energy (region 1)', value=esurface,& 
             & units='gulp-units:J.m-2', dictRef='gulp:esurface')
        else
          call cmlAddProperty(cmlfile, title='Surface energy (fullslab)', value=esurface,& 
             & units='gulp-units:J.m-2', dictRef='gulp:eslabsurfacefull')
          call cmlAddProperty(cmlfile, title='Surface energy (per side)', &
             & value=(esurface*0.5_dp),& 
             & units='gulp-units:J.m-2', dictRef='gulp:eslabsurfaceside')
        endif
      call cmlEndPropertyList(cmlfile)
    else
      if (nregions(ncf).gt.1) then
        call cmlStartPropertyList(cmlfile, title='Polymer energy', dictRef='gulp:1DEnergy')
          call cmlAddProperty(cmlfile, title='Polymer energy (region 1)', value=esurface,& 
             & units='gulp-units:eV.Ang', dictRef='gulp:epolymer')
        call cmlEndPropertyList(cmlfile)
      endif
    endif
  endif
!
!! Attachment energy
!
  if (ndim.eq.2.and.abs(eattach).gt.1.0d-8) then
    call cmlStartPropertyList(cmlfile, title='Attachment energy', dictRef='gulp:attatchmentEnergy')
      call cmlAddProperty(cmlfile, title='Attachment energy', value=eattach,& 
         & units='gulp-units:eV', dictRef='gulp:eattach')
      call cmlAddProperty(cmlfile, title='Attachment energy per unit', &
         & value=(eattach/dble(nzmol)),& 
         & units='gulp-units:eV', dictRef='gulp:eattachperunit')
    call cmlEndPropertyList(cmlfile)
  endif
!
  call cmlEndModule(cmlfile)
#endif
  return
end subroutine gulp_cml_outener

subroutine gulp_cml_outneutron
  
!============================================================================!
! Outputs information to xml file about neutron-related (input) settings     !
! Output is as a cml propery list build using FoX_wcml.                      !
!                                                                            !
! Currently gives: PDF information, wbins and qbins                      !
!                                                                            !
! First version: 23-II-2007 ERS29                                            !
! modified: 29-IV-2007 ERS29 to convert to multiple configruation            !
!============================================================================!
  
  use datatypes
  use m_pdfneutron
  use configurations, only : ncfg !number of configurations
  
  implicit none
  integer(i4) :: i !current configuration
#ifndef NOFOX
  ! interal vars
  character(len=16):: cunitstring
  character(len=10):: istring
  
  call checkstatus("gulp_cml_outneutron")
  ! loop over all configurations, with a new parameter list for each
  do i = 1, ncfg
     write(cunitstring,*)'gulp-units:',trim(unitsnamecfgs(i)) !store the units of frequencies
     
     write(istring,*) i
     call cmlstartParameterlist(cmlfile,title='Neutron input data for configuration ' // istring,&
          &dictRef='gulp:neutroninput')

     if (lpdf) then   !one of the PDF keywords has been set.
        call cmlstartParameterlist(cmlfile,title='rbins (radius) for configuration ' // istring,&
             &dictRef='gulp:rbins')
        call cmlAddParameter(cmlfile, &
             & value=rmaxcfgs(i),title='maximum radius for PDF', &
             & dictref='gulp:PDFrmax', units='gulp-units:Angstrom')
        call cmlAddParameter(cmlfile, &
             & value=nrbinscfgs(i),title='number of rbins for PDF', &
             & dictref='gulp:PDFnrbins', units='gulp-units:countable')
        call cmlEndParameterList(cmlfile)
        call cmlAddParameter(cmlfile, &
             & value=wmincfgs(i),title='minimum frequency cutoff for PDF', &
             & dictref='gulp:PDFnrbins', units=cunitstring)
        call cmlAddParameter(cmlfile, &
             & value=wmaxcfgs(i),title='maximum frequency cutoff for PDF', &
             & dictref='gulp:PDFnrbins', units=cunitstring)
     endif
     call cmlEndParameterList(cmlfile)
  enddo
#endif
end subroutine gulp_cml_outneutron

subroutine gulp_cml_PDFstats 
!============================================================================!
! Outputs information to xml file about PDFs  statistics                     !
!                                                                            !
! First full version: 22-II-2007 ERS29                                       !
!============================================================================!

  use m_pdfneutron
  use m_pdfvariables

  implicit none

#ifndef NOFOX
  ! interal vars
  character(len=16):: cunitstring
  character(len=10):: istring
  character(len=10):: jstring
  integer(i4) :: i_atom,j_atom

  call checkstatus("gulp_cml_PDFstats")
  call cmlStartPropertyList(cmlfile, title='PDF statistics', dictref='gulp:PDFstatistics')
  write(cunitstring,*)'gulp-units:',trim(iounits)
  call cmlAddProperty(cmlfile, &
        & value=numdensity,title='number density', &
        & dictref='gulp:numberdensity', units='gulp-units:1/Angstrom^3')
  call cmlAddProperty(cmlfile, &
        & value=sum_cbbar_sq,title='[sum_i{c_i bbar_i}]^2', &
        & dictref='gulp:sum_cbbar_sq', units='gulp-units:Angstrom^2')
  call cmlStartPropertyList(cmlfile, title="Weightings of atom-pair contribution to overall PDF", &
        dictref="gulp:w_ij")
  do i_atom = 1, natomp
    write(istring,*) istring
    do j_atom = 1, natomp
      write(jstring,*) jstring
      call cmlAddProperty(cmlfile, title= &
              & str("weight of pair "//trim(nameslist(i_atom))//"(atom#"//istring//") to "&
              & //trim(nameslist(j_atom))//"(atom#"//jstring//")"),&
              & value = (bbar_cores_A(i_atom)*(bbar_cores_A(j_atom))/sum_cbbar_sq),&
              & units = "gulp-units:dimensionless", ref=(str(i_atom)//":"//str(j_atom)))
    enddo
  enddo
  call cmlEndPropertyList(cmlfile)

  call cmlAddProperty(cmlfile, &
        & value=widmax,title='PDF maximum peak width squared', &
        & dictref='gulp:PDFmaxwidthsq', units='gulp-units:Angstrom^2')
  call cmlAddProperty(cmlfile, &
        & value=np,title='number of pairs use in PDF',  &
        & dictref='gulp:PDFnpairs',units='gulp-units:countable')    
  call cmlAddProperty(cmlfile, &
        & value=wmin_inunits,title='minimum frequency used in PDF calculations', &
        & dictref='gulp:PDFwminused',units=cunitstring)
  call cmlAddProperty(cmlfile, &
        & value=wmax_inunits,title='maximum frequency used in PDF calculations', &
        & dictref='gulp:PDFwmaxused',units=cunitstring)
  call cmlEndPropertyList(cmlfile)
#endif
 end subroutine gulp_cml_PDFstats


 subroutine gulp_cml_outPDF
!============================================================================!
! Outputs information to xml file about PDFs from lattice dynamics           !
! Output is as a cml propery list built using FoX_wcml.                      !
! This is called from phonon, where ncf in current hold the currrent config  !
! Correct units to be gulp-units not gulpunits 24.X11.08
! Changed partial output, now only giving (unweighted) rho_ij and g_ir     !
! First full version: 26-II-2007 ERS29                                       !
! Current version   : 13-V-2009 ERS29                                       !
!============================================================================!
  use m_pdfneutron
  implicit none
#ifndef NOFOX

  integer(i4)                    :: ipar
  character(len=5), dimension(2) :: pname
  real(dp), dimension(2)         :: pmass

  call checkstatus("gulp_cml_outPDF")
  ! total PDFs first (ipar = 1)
  ipar = 1
  call cmlStartPropertyList(cmlfile, &
        & title='Total Pair Distribution Functions from lattice dynamics', &
        & dictRef='gulp:totalPDF_ld')
  call cmlAddProperty(cmlfile, title='radius', value=rbins,& 
        & units='gulp-units:Angstrom', dictRef='gulp:rbins')
  call cmlAddProperty(cmlfile, title='Chung and Thorpe Rho Function, rho(r)',&
        &  value=pdf(:,ipar),& 
        & units='gulp-units:Angstrom^-3', dictRef='gulp:rho')
  call cmlAddProperty(cmlfile, title='G^{PDF}(r)', value=G_PDF_r(:),& 
        & units='gulp-units:Angstrom^-2', dictRef='gulp:G_PDF_r')
  call cmlAddProperty(cmlfile, title='Keen D(r)', value=D_r(:),& 
        & units='gulp-units:Angstrom^-1', dictRef='gulp:D_r')
  call cmlAddProperty(cmlfile, title='Keen G(r)', value=G_r(:,ipar),& 
        & units='gulp-units:Angstrom^2', dictRef='gulp:G_r')
  call cmlAddProperty(cmlfile, title='Keen T(r)', value=T_r(:),& 
        & units='gulp-units:Angstrom^-1', dictRef='gulp:T_r')
  call cmlEndPropertyList(cmlfile)
  if (partialno >1) then      
    call cmlAddProperty(cmlfile, &
           & value=partialno,title='number of partial PDFs generated', &
           & dictref='gulp:partialno',units='gulp-units:countable')
    do ipar = 2, partialno+1
      pname = (/partialnames(ipar-1,1),partialnames(ipar-1,2)/)
      pmass = (/partialmasses(ipar-1,1),partialmasses(ipar-1,2)/)
      call cmlStartPropertyList(cmlfile, &
              & title='Partial Pair Distribution Functions from lattice dynamics for partial #'//str(ipar-1),&
              & dictRef='gulp:partialPDF_ld')
      call cmlAddProperty(cmlfile, &
              & title='Partial #'//str(ipar-1)//' Components (by name)', &
              & value=pname, & 
              & dictRef='gulp:partialnames')
      call cmlAddProperty(cmlfile, &
              & title='Partial #'//str(ipar-1)//' Components (by mass)', &
              & value=pmass, & 
              & units='gulp-units:amu', dictRef='gulp:partialmasses')
      call cmlAddProperty(cmlfile, title='radius', value=rbins,& 
              & units='gulp-units:Angstrom', dictRef='gulp:rbins')

      call cmlAddProperty(cmlfile, title='Chung and Thorpe partial rho function, rho_ij(r) for partial #'//str(ipar-1),&
              & value=pdf(:,ipar),& 
              & units='gulp-units:Angstrom^-3', dictRef='gulp:rho')
      call cmlAddProperty(cmlfile, title='Keen g_ij(r) for partial #'//str(ipar-1) , value=G_r(:,ipar),& 
              & units='gulp-units:Angstrom^2', dictRef='gulp:g_r')
      call cmlEndPropertyList(cmlfile)
    enddo
  endif
#endif
end subroutine gulp_cml_outPDF

#ifndef NOFOX
!****************************************************************************!
!                                                                            !
!       ******               PRIVATE SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!


subroutine dump_current_config

!============================================================================!
! This dumps the current crystal structure. Currently used only for vcml     !
!      output in each optimisation step - will eventually be used internaly  !
!      for all output of the crystal structure. This is private to the       !
!      module.                                                               !
!                                                                            !
! First full version: 18-IX-2006 AMW                                         !
!============================================================================!

use current, only : nasym, iatn, natype, nrel2, &
                  & nrelat, xafrac, yafrac, zafrac, occua, qa, rv, ndim
use element, only : maxele
use shell, only : ncsptr

implicit none

! functions from volume.f90 - should be a module I guess
external area
real(dp) :: area

! Internal variables

character(len=5) :: lab
character(len=5) :: labs

integer :: i, n
integer :: inat
integer :: itype
integer :: ishell
integer :: itypes 
integer :: inats

real(dp) :: volume  ! Function from volume.f90 - call as volume(rv) 
                    ! where rv is a matrix of lattice vectors.... 
                    ! possibly sensible to create a module of 
                    ! interfaces of gulp stuff useded in this module.

! Write out the cell paramenters (but which ones? The ones related to the
! atoms I assume? NB - 2D and 1D is rather experimental...
if (ndim.eq.3) then
    Call cmlAddLattice(cmlfile, title='Cell', dictRef='gulp:cell', & 
         &  cell=rv, spaceType='real')

    !TODO - add volume and density. I have this on a branch somewhere.
    ! and some assoceated data
    ! -- volume 
    !
    ! -- and density
    !!  density = (10.0_dp*totmass)/(vol*rmolfct)
    !--
    !End TODO
elseif (ndim.eq.2) then
    ! TODO - this really needs a CML call for 2D cells... 
    call cmlStartParameterList(cmlfile, title='2D cell', &
          & dictRef='gulp:2Dcell')
        call cmlAddParameter(cmlfile, name='2D lattice vectors', &
          &  dictRef='gulp:2Dlatvec', value=rv, units='gulp-units:Ang')
        call cmlAddParameter(cmlfile, name='2D cell area', &
          &  dictRef='gulp:surfacearea', value=area(rv), &
          &  units='gulp-units:Ang2')
    call cmlEndParameterList(cmlfile)
elseif (ndim.eq.1) then
    ! TODO - this really needs a CML call for 1D cells... 
    call cmlStartParameterList(cmlfile, title='1D cell', &
          & dictRef='gulp:1Dcell')
        call cmlAddParameter(cmlfile, name='1D lattice vectors', &
          &  dictRef='gulp:1Dlatvec', value=rv, units='gulp-units:Ang')
    call cmlEndParameterList(cmlfile)
endif 

Call cmlAddLattice(cmlfile, title='Cell', dictRef='gulp:cell', cell=rv, spaceType='real')

Call cmlStartPropertyList(cmlfile, title='Cell Volume', dictRef='gulp:cellVolume')
        call cmlAddProperty(cmlfile, title='3D Cell Volume', value=volume(rv), &
              & units='gulp-units:Ang3', dictRef='gulp:3Dcellvolume')
Call cmlEndPropertyList(cmlfile)

!
! Loop over all atoms and output the atomic positions as a molecule
! This is a bit of a mess and needs wxml as the wcml setup is not 
! ready for charges, shells and so on
!

Call xml_NewElement(cmlfile, name='molecule')
Call xml_NewElement(cmlfile, name='atomArray')
n = 0
do i = 1,nasym
  inat = iatn(i)
  itype = natype(i)
  call label(inat,itype,lab)

! New cml
! This is a bit crap because
! I cannot use FoX's wcml call
! to add a molecule as this does
! not yet support opccupancy
! charge, type of particles.

  if (inat.le.maxele) then
  n = n + 1
! lxml is true if xml output selected
! inat is less than maxele if this is 
! a core (so skip shells).
    Call xml_NewElement(cmlfile, name='atom')
      Call xml_AddAttribute(cmlfile, name='elementType', value=trim(lab))
      if (ndim.eq.3) then
         Call xml_AddAttribute(cmlfile, name='xFract', value=xafrac(i))
         Call xml_AddAttribute(cmlfile, name='yFract', value=yafrac(i))
         Call xml_AddAttribute(cmlfile, name='zFract', value=zafrac(i))
     
      elseif (ndim.eq.2) then
         Call xml_AddAttribute(cmlfile, name='x3', value=(xafrac(i)*rv(1,1) + yafrac(i)*rv(1,2)))
         Call xml_AddAttribute(cmlfile, name='y3', value=(xafrac(i)*rv(2,1) + yafrac(i)*rv(2,2)))
         Call xml_AddAttribute(cmlfile, name='z3', value=zafrac(i))

      elseif (ndim.eq.1) then
            stop
      elseif (ndim.eq.0) then
            stop
      endif 
      Call xml_AddAttribute(cmlfile, name='occupancy', value=occua(i))
      Call xml_AddAttribute(cmlfile, name='id', value=str(n))
                    
! charge and type
      Call xml_NewElement(cmlfile, name='scalar')
        Call xml_AddAttribute(cmlfile, name='title', value='Charge')
        Call xml_AddAttribute(cmlfile, name='units', value='gulp-units:electronicunits')
        Call xml_AddAttribute(cmlfile, name='dataType', value='fpx:real')
        Call xml_AddCharacters(cmlfile, chars=qa(i))
      Call xml_EndElement(cmlfile, name='scalar')

      Call xml_NewElement(cmlfile, name='scalar')
        Call xml_AddAttribute(cmlfile, name='title', value='Type')
        Call xml_AddAttribute(cmlfile, name='dataType', value='sxd:string')
        Call xml_AddCharacters(cmlfile, chars='core')
      Call xml_EndElement(cmlfile, name='scalar')
                     
! Shell?
    if (ncsptr(nrel2(i)).gt.0) then
! We have to add a shell for this atom...
! Get the shell number, type and label
      ishell = nrelat(ncsptr(nrel2(i)))
      inats = iatn(ishell)
      itypes = natype(ishell)
      call label(inats,itypes,labs)

      Call xml_NewElement(cmlfile, name='particle')
! atom attributes
        Call xml_AddAttribute(cmlfile, name='elementType', value=trim(labs))
        if (ndim.eq.3) then
           Call xml_AddAttribute(cmlfile, name='xFract', value=xafrac(ishell))
           Call xml_AddAttribute(cmlfile, name='yFract', value=yafrac(ishell))
           Call xml_AddAttribute(cmlfile, name='zFract', value=zafrac(ishell))
        elseif (ndim.eq.2) then
           Call xml_AddAttribute(cmlfile, name='x3', value=(xafrac(ishell)*rv(1,1) + yafrac(ishell)*rv(1,2)))
           Call xml_AddAttribute(cmlfile, name='y3', value=(xafrac(ishell)*rv(2,1) + yafrac(ishell)*rv(2,2)))
           Call xml_AddAttribute(cmlfile, name='z3', value=zafrac(i))

        elseif (ndim.eq.1) then
            stop
        elseif (ndim.eq.0) then
            stop
        endif

        Call xml_AddAttribute(cmlfile, name='occupancy', value=occua(i))

! charge and type
        Call xml_NewElement(cmlfile, name='scalar')
          Call xml_AddAttribute(cmlfile, name='title', value='Charge')
        Call xml_AddAttribute(cmlfile, name='units', value='gulp-units:electronicunits')
        Call xml_AddAttribute(cmlfile, name='dataType', value='fpx:real')
          Call xml_AddCharacters(cmlfile, chars=qa(ishell))
        Call xml_EndElement(cmlfile, name='scalar')

        Call xml_NewElement(cmlfile, name='scalar')
          Call xml_AddAttribute(cmlfile, name='title', value='Type')
          Call xml_AddAttribute(cmlfile, name='dataType', value='sxd:string')
          Call xml_AddCharacters(cmlfile, chars='shell')
        Call xml_EndElement(cmlfile, name='scalar')

        Call xml_EndElement(cmlfile, name='particle')
      endif
      Call xml_EndElement(cmlfile, name='atom')
    endif
enddo

Call xml_EndElement(cmlfile, name='atomArray')
Call xml_EndElement(cmlfile, name='molecule')



end subroutine dump_current_config


subroutine get_iso_time(time) 

!============================================================================!
! Gets the current date, time and time zone and returns in the readable      !
!      ISO 8601 format (YYYY-MM-DDTHH:MM:SS+ZZZZ). This is private to the    !
!      module.                                                               !
!                                                                            !
! First full version: 23-VIII-2006 AMW                                       !
!============================================================================!

    character(len=24), intent(out) :: time

    character(len=10)  :: cdate
    character(len=10)  :: ctime
    character(len=5) :: zone
 
    time  = 'YYYY-MM-DDTHH:MM:SSsZZZZ'
    call date_and_time(cdate,ctime, zone)

! To do - check for negatve times... this indicates the 
! processor lacks a clock or lacks a timezone or clock.

    time(12:13) = ctime(1:2)
    time(15:16) = ctime(3:4)
    time(18:19) = ctime(5:6)
    time(9:10) = cdate(7:8)
    time(6:7) = cdate(5:6)
    time(1:4) = cdate(1:4)
    time(20:24) = zone(1:5)

end subroutine get_iso_time


subroutine add_pot_arg(value, units, dictref)

!============================================================================!
! Outputs 1 argument for a potential.                                        !
!                                                                            !
! First full version: 30-IX-2006 AMW                                         !
!============================================================================!


   implicit none

   real(dp)  :: value
   character(len=*) :: units
   character(len=*) :: dictref
   call xml_NewElement(cmlfile, name="arg")
        call xml_AddAttribute(cmlfile, name="ref", value=trim(dictref))
        call xml_NewElement(cmlfile, name="scalar")
             call xml_AddAttribute(cmlfile, name="units", value=trim(units)) 
             call xml_AddCharacters(cmlfile, chars=value) 
        call xml_EndElement(cmlfile, name="scalar")
   call xml_EndElement(cmlfile, name="arg")

end subroutine add_pot_arg

subroutine cml_symout

!============================================================================!
! Outputs symetry ops as CML.                                                !
!                                                                            !
! Based on symout.f and modified                                             !
!
!     hmssg  = Hermann-Mauguin Symbol of Space Group
!     nspcg  = International Tables Number for Space Group
!     iflags = Integer Flag for Group Symbol
!     ifhr   = Integer Flag for Hexagonal or Rhombohedral Cell
!     ifso   = Integer Flag for Shift of the Origin
!     ivso   = Integer Value for Shift of the Origin
!     ishorg = Integer Shift of the Origin
!     nccs   = Numeric Code for Crystal System
!     ncpg   = Numeric Code for Point Group
!
! Note: this version is just intended to ouput what is in the input file     !
!       once the semantics of symetry in cmlChem have been settled this      !
!       should be modified to output these as well. This is why much of the  !
!       data is encoded as metadata - I anticipate that at some point the    !
!       space group symbol (for example) will just be intresting information !
!       about a set of machine readable 3*4 matrices that encode all the     !
!       symmetry.                                                            !
!                                                                            !
! First full version:               3-X-2006 AMW                             !
!============================================================================!
      
    use current
    use iochannels
    use symmetry
    implicit none

!  Local variables

    integer(i4)        :: i
    integer(i4)        :: nspg
    real(dp)           :: symop(3,4)
    character(len=16)  :: spacegroupname
    call cmlStartParameterList(cmlfile, title='Symmetry', &
          & dictRef='gulp:symmetrydata')

    if (.not.lsymopt) then
!
! No symmetry applied to optimisation. 
!
      call cmlAddParameter(cmlfile, name='symmetry method', dictRef='gulp:symmethod',&
            &  value="no symmetry applied to calculation")

    else if  (ngocfg(ncf).gt.1) then
!
!  Explicit symmetry operators given by the user
!
      call cmlAddParameter(cmlfile, name='symmetry method', dictRef='gulp:symmethod',&
            &  value="Explicit list of operators")

      if (ifhr(ncf).eq.1) then
          call cmlAddParameter(cmlfile, name='cell type', dictRef='gulp:cellType', value="rhombohedral")
      else
          call cmlAddParameter(cmlfile, name='cell type', dictRef='gulp:cellType', value=trim(texc(nccscfg(ncf))))
      endif
          call cmlStartParameterList(cmlfile, title='List of Explicit symmetry operators', &
                & dictRef='gulp:explicitesymm')
              do i = 1,ngocfg(ncf)
                  symop(1:3,1:3) = ropcfg(1:3,1:3,i,ncf)
                  symop(1:3,4) = vitcfg(1:3,i,ncf)
                  call cmlAddParameter( cmlfile, name='symetry oporator', &
                        & units='units:dimensionless', value=symop, dictRef='gulp:symop')
              enddo
          call cmlEndParameterList(cmlfile)

    else
!
! Space group data given by the user (and possibly interpreted from number to symbol)
!
      call cmlAddParameter(cmlfile, name='symmetry method', dictRef='gulp:symmethod',&
            &  value="From space group")
      nspg = nspcg(ncf)
      call cmlAddParameter(cmlfile, name='crystal family',&
            &dictRef='gulp:crystalFamily', value=trim(texc(nccs)))
      call cmlAddParameter(cmlfile, name='crystal class',&
            & dictRef='gulp:CrystalClass', value=trim(cl(ncpg)))
      if (ncs.eq.0) call cmlAddParameter(cmlfile, name='space group type',&
            & dictRef='gulp:SpaceGroupType', value='centrosymmetric')
      if (ncs.eq.1) call cmlAddParameter(cmlfile, name='space group type',&
            & dictRef='gulp:SpaceGroupType', value='noncentrosymmetric')
      if (lalter) call cmlAddParameter(cmlfile, name='space group type',&
            & dictRef='gulp:SpaceGroupType', value='Non-standard setting of group')
      do i = 1,16
        spacegroupname(i:i) = hmssg(i,ncf) 
      enddo
      if (.not.lalter) call cmlAddParameter(cmlfile, name='space group name',&
            & dictRef='gulp:SpaceGroupName', value=trim(spacegroupname))
      if (lalter) call cmlAddParameter(cmlfile, name='space group name',&
            & dictRef='gulp:SpaceGroupName', value=trim(gronam(nspg)))
      call cmlAddParameter(cmlfile, name='Patterson group',&
            & dictRef='gulp:PattersonGroup', value=trim(patter(ipatgrp(nspg))))
      ! Shif of the origin - cannot trim if I want an array (as this can give different length
      ! elements. Also these should probably be represented as fpx:real (for useful processing)
      ! rather then xsd:string (with fractions like 1/3) - how to convert I wonder...
      !call cmlAddParameter( cmlfile, name='Origin shift', dictRef='gulp:symOriginShift',&
      !      & value=(/adjustl(trax(ivso(1,ncf)+1)), &
      !      & adjustl(trax(ivso(2,ncf)+1)), &
      !      & adjustl(trax(ivso(3,ncf)+1))/) )
  endif
  call cmlEndParameterList(cmlfile)

end subroutine cml_symout

subroutine checkstatus(caller)
    implicit none
    character(len=*) :: caller
    if (.not.lcml) then
      call outerror(caller // ' called but lcml is false. This is a bug in the calling subroutine.',0_i4)
      call stopnow(caller)
    endif
    
    if (cmldebugging) then
      if (caller .ne. "gulp_cml_init")  then
        call xml_AddComment(cmlfile, comment=("In subroutine " // caller))
      endif
    endif 

end subroutine checkstatus
#endif

End Module gulp_cml
