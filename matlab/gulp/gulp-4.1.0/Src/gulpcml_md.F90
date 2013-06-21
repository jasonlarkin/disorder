!****************************************************************************!
!                                                                            !
!       ******                 MODULE GULP_CML_MD            ******          !
!                              ==================                            !
!                                                                            !
!                                                                            !
! This module handles the ouptut of molecular dynmaics data to the cml file  !
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

module gulp_cml_md

#ifndef NOFOX
  ! Interfaces from the FoX Libary   
  use FoX_wxml
  use FoX_wcml
  use FoX_common, only : str, operator(//)
#endif

  use gulp_cml, only : lvcml, cmlfile, gulp_cml_startstep, &
                       gulp_cml_endstep, checkstatus

  ! Interfaces from gulp modules
  use datatypes

  private
  public :: lcml, gulp_cml_startstep, gulp_cml_endstep, &
            gulp_cml_md_startstep, gulp_cml_md_endstep, &
            gulp_cml_md_ensambledata, gulp_cml_md_celldata

contains 

  
!****************************************************************************!
!                                                                            !
!       ******               PUBLIC  SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!

subroutine  gulp_cml_md_startstep(step, time, ke, pe, te, ake, ape, ate, &
               & steptype, temp, atemp, acstemp, cstemp)

  implicit none

  integer, intent(in) :: step ! The MD step number
  real(dp), intent(in) :: time ! MD simulation time (ps)
  ! instant kinetic, potential and total energy (eV), temperature (K)
  real(dp), intent(in) :: ke, pe, te, temp
  ! running avarage  kinetic, potential, total energy (eV) and temperature (K)
  real(dp), intent(in) :: ake, ape, ate, atemp
  ! C/S temperature (optional) (K)
  real(dp), intent(in), optional :: cstemp, acstemp
  logical, intent(in) :: steptype ! true = prod false = equil

#ifndef NOFOX

  call checkstatus("gulp_cml_md_startstep")


  if (steptype) then
    ! This is a production step.
    call gulp_cml_startstep(step, 'gulp:MDStep')
  else
    ! this is a initialisation step.
    call gulp_cml_startstep(step, 'gulp:MDInitialisationStep')
  endif 
  
  call cmlStartPropertyList(cmlfile, title='MD step data', &
     & dictRef='gulp:MDdata')

    ! Current properties...
    call cmlAddProperty(cmlfile, title='MD simulation time', value=time, &
       & units='gulp-units:ps', dictRef='gulp:MDtime')
    call cmlAddProperty(cmlfile, title='Kinetic energy', value=ke, &
       & units='gulp-units:eV', dictRef='gulp:MDKinEen')
    call cmlAddProperty(cmlfile, title='Potential energy', value=pe, &
       & units='gulp-units:eV', dictRef='gulp:MDEn')
    call cmlAddProperty(cmlfile, title='Total energy', value=te, &
       & units='gulp-units:eV', dictRef='gulp:MDEn')
    call cmlAddProperty(cmlfile, title='Temperature', value=temp, &
       & units='gulp-units:K', dictRef='gulp:MDtemp')
    if (present(cstemp)) call cmlAddProperty(cmlfile, &
       & title='Core-shell Temperature',&
       & value=cstemp, units='gulp-units:K', dictRef='gulp:AvCStemp')

    ! Av properties...
    call cmlAddProperty(cmlfile, title='Av Kinetic energy', value=ake, &
       & units='gulp-units:eV', dictRef='gulp:MDAvKEen')
    call cmlAddProperty(cmlfile, title='Av Potential energy', value=ape, &
       & units='gulp-units:eV', dictRef='gulp:MDAvEn')
    call cmlAddProperty(cmlfile, title='Av Total energy', value=ate, &
       & units='gulp-units:eV', dictRef='gulp:MDAvEn')
    call cmlAddProperty(cmlfile, title='Av Temperature', value=atemp, &
       & units='gulp-units:K', dictRef='gulp:MDAvtemp')
    if (present(acstemp)) call cmlAddProperty(cmlfile, &
       & title='Av core-shell Temperature',&
       & value=acstemp, units='gulp-units:K', dictRef='gulp:MDAvCStemp')

  call cmlEndPropertyList(cmlfile)

#endif

end subroutine gulp_cml_md_startstep

subroutine gulp_cml_md_ensambledata(cq, themKE, thermInt, barostat, acq)

  implicit none

  real(dp), intent(in) :: cq, acq, themKE, thermInt
  real(dp), intent(in), optional :: barostat

#ifndef NOFOX
  call checkstatus("gulp_cml_md_ensambledata")


  call cmlStartPropertyList(cmlfile, title='MD ensable data', &
     & dictRef='gulp:MDensabledata')
    call cmlAddProperty(cmlfile, title='MD Conserved quantity', value=cq, &
       & units='gulp-units:eV', dictRef='gulp:MDcq')
    call cmlAddProperty(cmlfile, title='MD Ave Conserved quantity', value=acq,&
       & units='gulp-units:eV', dictRef='gulp:MDavcq')
    call cmlAddProperty(cmlfile, title='MD Thermostat KE', value=themKE, &
       & units='gulp-units:eV', dictRef='gulp:MDthemKE')
    call cmlAddProperty(cmlfile, title='MD Thermostat Integrl', & 
       & value=thermInt, units='gulp-units:eV', dictRef='gulp:MDthermInt')
    if (present(barostat)) call cmlAddProperty(cmlfile, &
       & title='MD Barostat term', value=barostat, &
       & units='gulp-units:eV', dictRef='gulp:MDbarostat')
  call cmlEndPropertyList(cmlfile)
#endif

end subroutine gulp_cml_md_ensambledata

subroutine gulp_cml_md_celldata(a, ava, b, avb, c, avc, &
                & alpha, avalpha, beta, avbeta, gamma, avgamma, &
                & pressure, avpressure )

  implicit none

  real(dp), intent(in), optional :: a, ava ! A and avarage A parameter
  real(dp), intent(in), optional :: b, avb, c, avc ! b and c params
  real(dp), intent(in), optional :: alpha, avalpha, beta, avbeta, gamma, avgamma
  real(dp), intent(in), optional :: pressure, avpressure ! GPa

  real(dp), dimension(3,3) :: cell ! Working array for the cell params
  
  cell(:,:) = 0.0_dp

#ifndef NOFOX
  call checkstatus("gulp_cml_md_celldata")

  if ( present(a).and.present(ava).and.present(b).and.present(avb).and.&
     & present(alpha).and.present(avalpha).and. &
     & present(c).and.present(avc).and.present(beta).and.present(avbeta).and. &
     & present(gamma).and.present(avgamma) ) then
     ! This is a valid 3D cell call

     call cell3D(cell,a,b,c,alpha,beta,gamma)
     Call cmlAddLattice(cmlfile, title='Cell', dictRef='gulp:MDInstCell', &
        &  cell=cell, spaceType='real')
     call cell3D(cell,ava,avb,avc,avalpha,avbeta,avgamma)
     Call cmlAddLattice(cmlfile, title='Avarage Cell', dictRef='gulp:MDAvCell', &
        & cell=cell, spaceType='real')

  elseif ( present(a).and.present(ava).and.present(b).and.present(avb).and. &
     & present(alpha).and.present(avalpha).and. &
     & (.not.(present(c).and.present(avc).and.present(beta).and.present(avbeta) &
     & .and.present(gamma).and.present(avgamma))) ) then
     ! This is a valid 2D cell call

     call cell2D(cell,a,b,alpha)
     Call cmlAddLattice(cmlfile, title='Cell', dictRef='gulp:MDInstCell', &
        & cell=cell, spaceType='real')
     call cell2D(cell,ava,avb,avalpha)
     Call cmlAddLattice(cmlfile, title='Avarage Cell', dictRef='gulp:MDAvCell', &
        & cell=cell, spaceType='real')

  elseif ( present(a).and.present(ava).and..not.(present(b).and.present(avb).and.&
     & present(alpha).and. &
     & present(avalpha).and.present(c).and.present(avc).and.present(beta).and. &
     & present(avbeta).and.present(gamma).and.present(avgamma)) ) then
     ! This is a valid 1D cell call

     call cell1D(cell,a)
     Call cmlAddLattice(cmlfile, title='Cell', dictRef='gulp:MDInstCell', &
        & cell=cell, spaceType='real')
     call cell1D(cell,ava)
     Call cmlAddLattice(cmlfile, title='Avarage Cell', dictRef='gulp:MDAvCell', &
        & cell=cell, spaceType='real')

  elseif ( present(a).or.present(ava).or.present(b).or.present(avb).or.present(alpha).or. &
     & present(avalpha).or. &
     & present(c).or.present(avc).or.present(beta).or.present(avbeta).or. &
     & present(gamma).or.present(avgamma) ) then
     ! This is not a valid call... 
      call outerror('Argument error in gulp_cml_md_celldata (bug).',0_i4)
      call stopnow('gulp_cml_md_celldata')

  endif

  if (present(pressure).and.present(avpressure)) then
    call cmlStartPropertyList(cmlfile, title='MD Pressure', &
       & dictRef='gulp:MDpressuredata')
      call cmlAddProperty(cmlfile, title='MD Inst Pressure', value=pressure, &
       & units='gulp-units:GPa', dictRef='gulp:MDInstPressure')
      call cmlAddProperty(cmlfile, title='MD Av Pressure', value=avpressure, &
       & units='gulp-units:GPa', dictRef='gulp:MDAvPressure')
    call cmlEndPropertyList(cmlfile)
  endif
#endif

end subroutine gulp_cml_md_celldata

subroutine gulp_cml_md_endstep(constR, avconstR, constV, avconstV)

  implicit none
 
  ! Optioanlly add constraints on step end (ev/Ang) 
  real(dp), intent(in), optional :: constR, avconstR, constV, avconstV

#ifndef NOFOX
  call checkstatus("gulp_cml_md_endstep")

  if (present(constR).and.present(avconstR).and.present(constV).and.present(avconstV)) then
    call cmlStartPropertyList(cmlfile, title='MD Constraints', &
       & dictRef='gulp:MDconstraintdata')
      call cmlAddProperty(cmlfile, title='MD Inst Constraint R', value=constR, &
       & units='gulp-units:eV.Ang-1', dictRef='gulp:MDInstConstR')
      call cmlAddProperty(cmlfile, title='MD Av Constraint R', value=avconstR, &
       & units='gulp-units:eV.Ang-1', dictRef='gulp:MDAvConstR')
      call cmlAddProperty(cmlfile, title='MD Inst Constraint V', value=constV, &
       & units='gulp-units:eV.Ang-1', dictRef='gulp:MDInstConstV')
      call cmlAddProperty(cmlfile, title='MD Av Constraint V', value=avconstV, &
       & units='gulp-units:eV.Ang-1', dictRef='gulp:MDAvConstV')
    call cmlEndPropertyList(cmlfile)

  elseif (present(constR).or.present(avconstR).or.present(constV).or.present(avconstV)) then 
     ! This is not a valid call... 
      call outerror('Argument error in gulp_cml_md_celldata (bug).',0_i4)
      call stopnow('gulp_cml_md_endstep')

  endif

  ! If we are being (very) verbose then dump the atomic positions... 
  if (lvcml) call dump_current_md_config
  ! FIXME - but what about the velocities and accel?

  call gulp_cml_endstep()
#endif
end subroutine gulp_cml_md_endstep

#ifndef NOFOX

!****************************************************************************!
!                                                                            !
!       ******               PRIVATE SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!

subroutine dump_current_md_config

!============================================================================!
! This dumps the current crystal structure. Currently used only for vcml     !
!      output in each optimisation step - will eventually be used internaly  !
!      for all output of the crystal structure. This is private to the       !
!      module.                                                               !
!                                                                            !
! First full version: 18-IX-2006 AMW                                         !
!============================================================================!

use current, only : nasym, iatn, natype, nrel2, &
                  & nrelat, xalat, yalat, zalat, occua, qa, ndim
use element, only : maxele
use shell, only : ncsptr

implicit none

! Internal variables
character(len=5) :: lab
character(len=5) :: labs

integer :: i, n
integer :: inat
integer :: itype
integer :: ishell
integer :: itypes 
integer :: inats


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
         Call xml_AddAttribute(cmlfile, name='x3', value=xalat(i))
         Call xml_AddAttribute(cmlfile, name='y3', value=yalat(i))
         Call xml_AddAttribute(cmlfile, name='z3', value=zalat(i))
     
      elseif (ndim.eq.2) then
         Call xml_AddAttribute(cmlfile, name='x3', value=xalat(i))
         Call xml_AddAttribute(cmlfile, name='y3', value=yalat(i))
         Call xml_AddAttribute(cmlfile, name='z3', value=zalat(i))

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
           Call xml_AddAttribute(cmlfile, name='x3', value=xalat(ishell))
           Call xml_AddAttribute(cmlfile, name='y3', value=yalat(ishell))
           Call xml_AddAttribute(cmlfile, name='z3', value=zalat(ishell))
        elseif (ndim.eq.2) then
           Call xml_AddAttribute(cmlfile, name='x3', value=xalat(ishell))
           Call xml_AddAttribute(cmlfile, name='y3', value=xalat(ishell))
           Call xml_AddAttribute(cmlfile, name='z3', value=zalat(ishell))

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
end subroutine dump_current_md_config
#endif

End Module gulp_cml_md
