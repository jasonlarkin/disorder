!****************************************************************************!
!                                                                            !
!       ******             MODULE GULP_CML_PHONON            ******          !
!                          ======================                            !
!                                                                            !
! This module writes all the new CML output to do with phonon calculations.  !
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

module gulp_cml_phonon

#ifndef NOFOX
  ! Interfaces from the FoX Libary   
  use FoX_wxml
  use FoX_wcml
  use FoX_common, only : str, operator(//)
#endif

  use gulp_cml, only : lvcml, cmlfile, checkstatus

  ! Interfaces from gulp modules
  use datatypes

  private
  public :: lcml, gulp_cml_PDFstats, gulp_cml_outneutron, &
            gulp_cml_outpdf, gulp_cml_outspec, gulp_cml_startPhonons, &
            gulp_cml_endPhonons, gulp_cml_addKPoint


contains 

  
!****************************************************************************!
!                                                                            !
!       ******               PUBLIC  SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!

subroutine gulp_cml_outspec

!============================================================================!
! Outputs information to xml file about species (input)                      !
! Output is as a cml prameter list build using FoX_wcml.                     !
!                                                                            !
! First full version: 12-III-2007 ERS29                                      !
!============================================================================!
  use constants
  use element
  use general, only : elefile
  use iochannels
  use library
  use m_pdfneutron
  use shell
  use species
  implicit none

#ifndef NOFOX
  
  !  Local variables
  
  character(len=6) :: stype(4)
  character(len=5) :: lab
  character(len=6) :: istring
  integer(i4)      :: i
  integer(i4)      :: ind
  integer(i4)      :: na
  integer(i4)      :: nrms
  integer(i4)      :: ntyp
  logical          :: lfound
  real(dp)         :: atm
  real(dp)         :: convertA
  real(dp)         :: convertA2
  
  data stype/'Core  ','Shell ','BCore ','BShell'/
  convertA2 = 1D-8
  convertA = 1D-4
  
  call checkstatus("gulp_cml_outspec")
  call cmlstartParameterlist(cmlfile,title='Species parameters (for all configurations)', dictRef='gulp:outspec')
    call cmlAddParameter(cmlfile,& ! 'elefile' found in current directory? 
         & value=lelecurrent,title='Elemental Data Library ("elefile") found in current directory', &
         & dictref='gulp:elefile')
    if (.not.(lelecurrent)) then
       call cmlAddParameter(cmlfile, & ! library searched for
            & value=trim(adjustl(elefile)),title='Elemental Data Library (as specified in local.F90)', &
            & dictref='gulp:elefile')
       call cmlAddParameter(cmlfile,& ! 'elefile' found as specified in local?
            & value=lelefile,title='Elemental Data Library  found at '//trim(adjustl(elefile)), &
            & dictref='gulp:elefile')
    endif
    call cmlAddParameter(cmlfile, &
         & value=lneutronele,title='neutron scattering data read from a Library file?', &
         & dictref='gulp:lneutronele')
    do i = 1,nspec
       na = natspec(i)
       ntyp = ntypspec(i)
       call label(na,ntyp,lab)
       ind = 1
       if (na.gt.maxele) then
          na = na - maxele
          ind = 2
       endif
       !  Find shellmass ratio type
       lfound = .false.
       atm = massspec(i)
       nrms = 0
       do while (nrms.lt.nratiomspec.and..not.lfound) 
          nrms = nrms + 1
          if (na.eq.natratiomspec(nrms).and.(ntyp.eq.ntypratiomspec(nrms).or.ntypratiomspec(nrms).eq.0)) then
             lfound = .true.
             atm = massspec(i)*ratiomspec(nrms)
          endif
       enddo
       if (lbrspec(i)) ind = ind + 2
       ! output parameter details
       write(istring,'(i6)') i
       call cmlstartParameterList(cmlfile, title='Parameters for atom '//istring, dictRef='gulp:speciesparameters')
       call cmlAddParameter(cmlfile, value=lab,title='name', &
            & dictref='gulp:specieslabel')
       call cmlAddParameter(cmlfile, value=stype(ind),title='type', &
            & dictref='gulp:speciestype')
       call cmlAddParameter(cmlfile, value=na,title='atomic number', &
            & dictref='gulp:speciesatomicnumber', units='units:dimensionless')
       call cmlAddParameter(cmlfile, &
            & value=atm,title='atomic mass', &
            & dictref='gulp:speciesatomicmass', units = 'gulp-units:amu')
       call cmlAddParameter(cmlfile, &
            & value=qlspec(i),title='charge', &
            & dictref='gulp:speciescharge', units = 'gulp-units:e')
       call cmlAddParameter(cmlfile, &
            & value=rcov(na),title='covalent radii', &
            & dictref='gulp:speciescovalentradii', units = 'gulp-units:Ang')
       call cmlAddParameter(cmlfile, &
            & value=radspec(i),title='ionic radii', &
            & dictref='gulp:speciesionicradii', units = 'gulp-units:Ang')
       call cmlAddParameter(cmlfile, &
            & value=rvdw(na),title='Van der Waals radii', &
            & dictref='gulp:speciesvanderwaslsradii', units = 'gulp-units:Ang')
       call cmlAddParameter(cmlfile, &
            & value=bbar(na)*convertA,title='bbar: coherent bound neutron scattering length', &
            & dictref='gulp:speciesbbar', units = 'gulp-units:Ang')
       call cmlAddParameter(cmlfile, &
            & value=4.0_dp*pi*bbar(na)*bbar(na)*convertA2,title='sigma_coh: coherent neutron scattering cross-section', &
            & dictref='gulp:speciessigcoh', units = 'gulp-units:Ang2')
       call cmlAddParameter(cmlfile, &
            & value= siginc(na)*convertA2,title='sigma_inc: incoherent bound neutron scattering cross-section', &
            & dictref='gulp:speciessiginc', units = 'gulp-units:Ang2')
       call cmlAddParameter(cmlfile, &
            & value=libspec(i)(1:9),title='library symbol', &
            & dictref='gulp:specieslibrarysymbol')
       call cmlEndParameterList(cmlfile)
   enddo
  call cmlEndParameterList(cmlfile)
#endif
end subroutine gulp_cml_outspec


subroutine gulp_cml_outneutron
  
  !============================================================================!
  ! Outputs information to xml file about neutron-related (input) settings     !
  ! Output is as a cml propery list build using FoX_wcml.                      !
  !                                                                            !
  ! Currently gives: PDF information
  !                                                                            !
  ! First version: 23-II-2007 ERS29                                            !
  ! modified: 29-IV-2007 ERS29 to convert to multiple configruation            !
  !           31-VII-07  AMW convert units to rad.s-1 and correct namespace    !
  !============================================================================!
  
  use datatypes
  use m_pdfneutron
  use configurations, only : ncfg !number of configurations
  use constants,   only : cmtorads
  
  implicit none
#ifndef NOFOX
  integer(i4) :: i !current configuration
  ! interal vars
  character(len=16):: unitstr
  character(len=6) :: istring
  real(dp) :: wmin, wmax
  
  call checkstatus("gulp_cml_outneutron")
  ! loop over all configurations, with a new parameter list for each
  do i = 1, ncfg

      write(istring,*) i

      ! Convert wmincfgs and wmaxcfgs to cm^-1 for cml output
      wmin = wmincfgs(i)
      wmax = wmaxcfgs(i)
      call changethisfreq(.true., wmin, unitstr)
      call changethisfreq(.true., wmax, unitstr)
      wmin = wmin * (1/cmtorads)
      wmax = wmax * (1/cmtorads)
      ! We don't actually need unitstr - which is not an XML name so not useful for 
      ! a unit name attribute
     
     call cmlstartParameterlist(cmlfile,title='Neutron input data for configuration ' // istring,&
          &dictRef='gulp:neutroninput')
     
     if (lpdf) then   !one of the PDF keywords has been set.
        call cmlstartParameterlist(cmlfile,title='rbins (radius) for configuration ' // istring,&
             &dictRef='gulp:rbins')
        call cmlAddParameter(cmlfile, &
             & value=rmaxcfgs(i),title='maximum radius for PDF', &
             & dictref='gulp:PDFrmax', units='gulp-units:Ang')
        call cmlAddParameter(cmlfile, &
             & value=nrbinscfgs(i),title='number of rbins for PDF', &
             & dictref='gulp:PDFnrbins', units='units:countable')
        call cmlEndParameterList(cmlfile)
        call cmlAddParameter(cmlfile, &
             & value=wmin, title='minimum frequency cutoff for PDF', &
             & dictref='gulp:PDFnrbins', units='gulp-units:cm-1')
        call cmlAddParameter(cmlfile, &
             & value=wmax, title='maximum frequency cutoff for PDF', &
             & dictref='gulp:PDFnrbins', units='gulp-units:cm-1')
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
  use constants,      only : cmtorads

  implicit none
#ifndef NOFOX
!
!  Local variables
!
  character(len=16):: cunitstring
  character(len=6) :: istring
  character(len=6) :: jstring
  integer(i4) :: i_atom,j_atom
  real(dp) :: wmin, wmax
!
!   Get wmin and wmax in rad.s-1
!
  wmin = wmin_inunits
  wmax = wmax_inunits
  call changethisfreq(.true., wmin, cunitstring)
  call changethisfreq(.true., wmax, cunitstring)
  wmin = wmin * (1/cmtorads)
  wmax = wmax * (1/cmtorads)

  call checkstatus("gulp_cml_PDFstats")
  call cmlStartPropertyList(cmlfile, title='PDF statistics', dictref='gulp:PDFstatistics')
  call cmlAddProperty(cmlfile, &
        & value=numdensity,title='number density', &
        & dictref='gulp:numberdensity', units='gulp-units:Ang-3')
  call cmlAddProperty(cmlfile, &
        & value=sum_cbbar_sq,title='[sum_i{c_i bbar_i}]^2', &
        & dictref='gulp:sum_cbbar_sq', units='gulp-units:Ang-2')
  call cmlStartPropertyList(cmlfile, title="Weightings of atom-pair contribution to overall PDF", &
        dictref="gulp:w_ij")
  do i_atom = 1,natomp
    write(istring,'(i6)') i_atom
    do j_atom = 1,natomp
      write(jstring,'(i6)') j_atom
      call cmlAddProperty(cmlfile, title= &
              & str("weight of pair "//trim(nameslist(i_atom))//"(atom#"//istring//") to "&
              & //trim(nameslist(j_atom))//"(atom#"//jstring//")"),&
              & value = (bbar_cores_A(i_atom)*(bbar_cores_A(j_atom))/sum_cbbar_sq),&
              & units = "units:dimensionless", ref=(str(i_atom)//":"//str(j_atom)))
    enddo
  enddo 
  call cmlEndPropertyList(cmlfile)

  call cmlAddProperty(cmlfile, &
        & value=widmax,title='PDF maximum peak width squared', &
        & dictref='gulp:PDFmaxwidthsq', units='gulp-units:Ang-2')
  call cmlAddProperty(cmlfile, &
        & value=np,title='number of pairs use in PDF',  &
        & dictref='gulp:PDFnpairs',units='units:countable')    
  call cmlAddProperty(cmlfile, &
        & value=wmin, title='minimum frequency used in PDF calculations', &
        & dictref='gulp:PDFwminused',units='gulp-units:cm-1')
  call cmlAddProperty(cmlfile, &
        & value=wmax, title='maximum frequency used in PDF calculations', &
        & dictref='gulp:PDFwmaxused',units='gulp-units:cm-1')
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

subroutine gulp_cml_startPhonons
!============================================================================!
! Simple subroutine to start the (or a) list of phonons.                     !
!                                                                            !
! First full version: 20-III-2007 AMW                                        !
!============================================================================!

  implicit none
#ifndef NOFOX
  call checkstatus("gulp_cml_startPhonons")
 
  call cmlStartKpointList(cmlfile, title='Phonons', dictRef='gulp:phonons')
#endif

end subroutine gulp_cml_startPhonons


subroutine gulp_cml_endPhonons
!============================================================================!
! Simple subroutine to start the (or a) list of phonons.                     !
!                                                                            !
! First full version: 20-III-2007 AMW                                        !
!============================================================================!

  implicit none
#ifndef NOFOX
  call checkstatus("gulp_cml_endPhonons")

  call cmlEndKpointList(cmlfile)
#endif

end subroutine gulp_cml_endPhonons


subroutine gulp_cml_addKPoint(kpt, wk, mcv, freq, eigenr, eigeni, nonanal, nonanalpts, bornk)
!============================================================================!
! Output phonons at a K Point                                                !
!                                                                            !
! First full version: 20-III-2007 AMW                                        !
! Modified to use new K-Point output options. See                            !
!     http://www.uszla.me.uk/FoX/WCML_tutorial.html: 2-V-2008 AMW            !
!============================================================================!

  implicit none
  
  ! Dummy args
  real(dp), dimension(3) :: kpt
  real(dp), intent(in) :: wk
  integer(i4), intent(in) :: mcv   ! Number of modes...
  real(dp), intent(in), dimension(:) :: freq
  real(dp), intent(in), dimension(:,:), optional :: eigenr
  real(dp), intent(in), dimension(:,:), optional :: eigeni
  logical, intent(in), optional :: nonanal     ! Is there a correction
  integer(i4), intent(in), optional :: nonanalpts ! number of points OR
  real(dp), intent(in), dimension(3), optional :: bornk ! direction
#ifndef NOFOX

  ! Internal vars
  integer :: branch

  call checkstatus("gulp_cml_addKPoint")

  call cmlStartKpoint(cmlfile, coords=kpt, weight=wk, dictRef='gulp:qpoint')

  ! Does this kpoint have a nonanalitical correction of some form?
  if (present(nonanal)) then
     if (nonanal) then 
       call cmlStartPropertyList(cmlfile, title='Nonanalitical gamma point correction', &
           & dictRef='gulp:nonanalprop')
       if (present(nonanalpts)) then
          if (nonanalpts.gt.0) then
            ! polycrystaline avarage 
            call cmlAddProperty(cmlfile, title='Number of polycrystaline nonanalitical correction points',&
            & value=nonanalpts, units='units:countable', dictRef='gulp:qpoint_polycrystal')
          elseif (present(bornk)) then
            ! Correction from a particular direction
            call cmlAddProperty(cmlfile, title='Nonanalitical correction direction', & 
             & value=bornk, units='units:dimensionless', dictRef='gulp:qpoint_dir')
          endif
       endif
       call cmlEndPropertyList(cmlfile)
     endif
  endif

  ! Loop over branches
  do branch = 1, mcv
    call cmlStartBand(cmlfile, label=str(branch))

    ! If we don't have vectors...
    if (.not.lvcml.or.(.not.present(eigenr))) then
      call cmlAddEigenValue(cmlfile, freq(branch), &
       & units='gulp-units:cm-1', dictRef='gulp:eigenvalue')
    endif

    ! Eigenvectors are not compusary but we must have both parts.
    ! and only write them in verbose mode to save space (lots of 
    ! data for large systems).
    if (lvcml) then 
        if (present(eigenr)) then
           if(present(eigeni)) then
             ! We have a complex eigenvector - I don't understand why
             ! the kind casting is needed here (the ,dp arg to cmplx)
             call cmlAddEigenValueVector(cmlfile, eigval=freq(branch),   & 
               & eigvec=cmplx(reshape(eigenr(:, branch), (/3,(mcv/3)/)),  &
               &              reshape(eigeni(:, branch), (/3,(mcv/3)/)),dp), &
               & units='gulp-units:cm-1', &
               & dictRef='gulp:eigenvaluevector')

           else 
             ! No complex bits - probably gamma point.
             call cmlAddEigenValueVector(cmlfile, eigval=freq(branch),   & 
               & eigvec=reshape(eigenr(:, branch), (/3,(mcv/3)/)),  &
               & units='gulp-units:cm-1', &
               & dictRef='gulp:eigenvaluevector')

           endif
        endif
    endif
    call cmlEndBand(cmlfile) 
  enddo

  call cmlEndKpoint(cmlfile)
#endif

end subroutine gulp_cml_addKPoint


!****************************************************************************!
!                                                                            !
!       ******               PRIVATE SUBROUTINES          ******             !
!                            ===================                             !
!                                                                            !
!****************************************************************************!

! Nothing private as yet...

End Module gulp_cml_phonon
