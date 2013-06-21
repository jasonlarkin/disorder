!****************************************************************************!
!                                                                            !
!       ******             MODULE GULP_CML_PROPS             ******          !
!                          =====================                             !
!                                                                            !
! This module writes new CML output according the the eMinerals CML          !
! convention (subset). This outputs some standard calculated properties.     !
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

module gulp_cml_props

#ifndef NOFOX
  ! Interfaces from the FoX Libary   
  use FoX_wxml
  use FoX_wcml
  use FoX_common, only : str, operator(//)
#endif

  ! Interfaces from gulp modules
  use datatypes
  use gulp_cml, only : cmlfile, checkstatus

  private
  public :: lcml, gulp_cml_print_born_charges, gulp_cml_output_derivs, &
            gulp_cml_print_epot_lattice, gulp_cml_output_dielectric, &
            gulp_cml_output_inertia

contains 

  
!****************************************************************************!
!                                                                            !
!       ******               PUBLIC  SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!

subroutine gulp_cml_print_epot_lattice(v)

  use element, only : maxele
  use current, only : nasym,  nrel2, nat, nftype

  real(kind=dp), dimension(:), intent(in)  :: v

#ifndef NOFOX
  integer :: ii,  n, ion, inat, itype
  character(len=4)   :: lab

  call checkstatus("gulp_cml_print_epot_lattice")

  call cmlStartPropertyList(cmlfile, title="Electrostatic potential at atomic positions", &
         dictref="gulp:epot_lattice")
  n = 0
  do ion = 1,nasym
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

#ifndef NOFOX
    real(kind=dp), dimension(:,:), allocatable :: bec
    integer :: i, ii, j, n, ion, inat, itype
    character(len=4)   :: lab

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
            & value=elcon,title='Elastic Constant Matrix',units='gulp-units:GPa')
        if (present(compliances)) call cmlAddProperty(cmlfile,& 
            & value=compliances,title='Elastic Commpliance Matrix',units='gulp-units:GPa-1')
        if (present(bulkmod_reuss)) call cmlAddProperty(cmlfile, &
            & value=bulkmod_reuss,title='Bulk modulus (Reuss)', dictRef='gulp:bulkmodreuss',units='gulp-units:GPa')
        if (present(bulkmod_voigt)) call cmlAddProperty(cmlfile, &
            & value=bulkmod_voigt,title='Bulk modulus (Voigt)', dictRef='gulp:bulkmodvoigt',units='gulp-units:GPa')
        if (present(bulkmod_hill)) call cmlAddProperty(cmlfile, &
            & value=bulkmod_hill,title='Bulk modulus (Hill)',  dictRef='gulp:bulkmodhill',units='gulp-units:GPa')
        if (present(shearmod_reuss)) call cmlAddProperty(cmlfile, &
            & value=shearmod_reuss,title='Shear modulus (Reuss)', dictRef='gulp:shearmodreuss', &
            & units='gulp-units:GPa')
        if (present(shearmod_voigt)) call cmlAddProperty(cmlfile, &
            & value=shearmod_voigt,title='Shear modulus (Voigt)', dictRef='gulp:shearmodvoigt', &
            & units='gulp-units:GPa')
        if (present(shearmod_hill)) call cmlAddProperty(cmlfile, &
            & value=shearmod_hill,title='Shear modulus (Hill)', dictRef='gulp:shearmodhill',units='gulp-units:GPa')
        if (present(vs_reuss)) call cmlAddProperty(cmlfile, &
            & value=vs_reuss,title='Velocity S-wave (Reuss)', dictRef='gulp:velswavereuss',units='gulp-units:km.s-1')
        if (present(vs_voigt)) call cmlAddProperty(cmlfile, &
            & value=vs_voigt,title='Velocity S-wave (Voigt)', dictRef='gulp:velswavevoigt',units='gulp-units:km.s-1')
        if (present(vs_hill)) call cmlAddProperty(cmlfile, &
            & value=vs_hill,title='Velocity S-wave (Hill)', dictRef='gulp:velpwavehill',units='gulp-units:km.s-1')
        if (present(vp_reuss)) call cmlAddProperty(cmlfile, &
            & value=vp_reuss,title='Velocity P-wave (Reuss)', dictRef='gulp:velpwavereuss',units='gulp-units:km.s-1')
        if (present(vp_voigt)) call cmlAddProperty(cmlfile, &
            & value=vp_voigt,title='Velocity P-wave (Voigt)', dictRef='gulp:velpwavevoigt',units='gulp-units:km.s-1')
        if (present(vp_hill)) call cmlAddProperty(cmlfile, &
            & value=vp_hill,title='Velocity P-wave (Hill)', dictRef='gulp:velpwavehill',units='gulp-units:km.s-1')
    call cmlEndPropertyList(cmlfile)
#endif

end subroutine gulp_cml_output_derivs

subroutine gulp_cml_output_inertia(inertia)

    real(kind=dp), dimension(3,3), intent(in) :: inertia

#ifndef NOFOX
    call checkstatus("gulp_cml_output_inertia")
    call cmlStartPropertyList(cmlfile)
        call cmlAddProperty(cmlfile, dictRef='gulp:momentOfInertia', &
            & value=inertia, title='Moment of inertia tensor', &
            & units='gulp-units:10-46.kg.m2')
    call cmlEndPropertyList(cmlfile)
#endif

end subroutine gulp_cml_output_inertia

!****************************************************************************!
!                                                                            !
!       ******               PRIVATE SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!

! Empty section...

End Module gulp_cml_props
