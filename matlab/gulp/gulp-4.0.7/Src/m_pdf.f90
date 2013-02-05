!****************************************************************************!
!                                                                            !
!       ******                 MODULE pdfmodule               ******         !
!                              ================                              !
!                                                                            !
! This module controls the calculation of PDF and related neutron issues     !
! Further details published in                                               !
!     Cope, E. R. & Dove, M. T. (2007). J. Appl. Cryst. 40, 589-594.         !
! Subroutines are listed in alphabetical order at the end of the file        !
!                                                                            !
! If you beleve you have found a bug in this module contact the author.      !
!                                                                            !
! 04/07 Made .f90                                                            !
! 05/07 All subroutines moved into this file, and use neutron added to module!
! 11/07 Additional comments added, and reference to concentrations removed   !
! 11/09 Major upgrade: change to using equations of Chung and Thorpe as      !
!        much more effiencent and less subject to rounding error             !
!                                                                            !
!                                               Beth Cope [ers29] (c) 2009   !
!                                                          ers29@cam.ac.uk   !
!                                                                            !
!****************************************************************************!
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, 2010
!  Elizabeth Cope, Cambridge University, 2010
!
!****************************************************************************!
!  General Notes:
!     GULP allows multiple configurations to be run from the same input file
!     The number of configurations is stored as nfc. The PDF arrays are 
!     reallocated to the approriate size for each configuration. 
!     Each configuration will have n rbins (radius axis) as set by the user.
!
!     Both the total distribution functions and the partial distribution functions
!     are calculated. The PDF arrays have a second dimension(p) which can be used to ref
!     erence the partials.p=1 for the total, and then the partials are stored in p>1.
!
module m_pdf
  use datatypes
  use m_pdfneutron
  implicit none
  save

  private
  public :: pdfsetup, closepdfphonon

contains 
!****************************************************************************!
!*  subroutine allocatepdf:
!****************************************************************************!
!     allocates pdf arrays ready for use.
!
!  written by Elizabeth Cope (ers29) November 2006
!  4/07 modified for multple configurations
!  4/07 now calls outofmemory, and made .f90
!
!  variables set on input:
!      ncf:         current configuration number
!      nrbinscfgs:  pointer of size ncf that contains the number of r bins 
!                   for this each config
!      partialno:  number of partial pair distributions for this set up
!                   set in partialpdfsetup
!      n:           variable for allocating "partial" dimension of arrays!             
!                   n = 1:npartialno as the first element is the total
!  variable allocated in routine:
!      rbins(*):    radius-array allocated to have the number of bins 
!                   defined in nrbinscfgs(ncf). units: Ang
!      pdfpair(*): array allocated to have correct number of bins 
!                    used to store the contribution of current pair only
!      pdf_ij(*): internal array used for each pair whlie calculating pdfs
!  2D arrays  allocated to have correct number of bins 
!                   2nd dimension stores the total, and then partials      
!      pdf(*,*):   Chung & Thorpe's PDF (rho) [A-3]
!      T_r(*):    Keen T(r) [A-1]
!      G_r(*,*):    Keen G(r) [A2] 
!      G_PDF_r(*): Keen G^{PDF}(r) (as PDFFIT) [A-2]            
!      G_RMC_R(*,*): Keen G(r) in output units from RMC routines (not used in output)
!      D_R(*):    Keen D(r) [A-1]
!****************************************************************************!
  subroutine allocatepdf

    use current, only :ncf !CURRENT config number
    implicit none
    integer(i4) :: i_err
    integer(i4) :: n
    i_err = 1 ! to catch errors. i_err = 0 if all OK
    n = partialno +1

    call deallocatepdf()
    !     allocate bins
    allocate(rbins(nrbinscfgs(ncf)), stat=i_err)
    if (i_err/=0) call outofmemory('allocatepdf','rbins')
!
!  Allocate arrays for output
!
    allocate(pdf(nrbinscfgs(ncf),n), stat=i_err)
    if (i_err/=0) call outofmemory('allocatepdf','pdf')

    allocate(pdfpair(nrbinscfgs(ncf)), stat=i_err)
    if (i_err/=0) call outofmemory('allocatepdf','pdfpair')
    allocate(T_r(nrbinscfgs(ncf)), stat=i_err)
    if (i_err/=0) call outofmemory('allocatepdf','T_r')
    allocate(G_r(nrbinscfgs(ncf),n), stat=i_err)
    if (i_err/=0) call outofmemory('allocatepdf','G_r')
    allocate(G_PDF_r(nrbinscfgs(ncf)), stat=i_err)
    if (i_err/=0)call outofmemory('allocatepdf','G_PDF_R')
    allocate(G_RMC_r(nrbinscfgs(ncf),n), stat=i_err)
    if (i_err/=0)call outofmemory('allocatepdf','G_RMC_r')
    allocate(D_r(nrbinscfgs(ncf)), stat=i_err)
    if (i_err/=0)call outofmemory('allocatepdf','D_r')
    allocate(pdf_ij(nrbinscfgs(ncf)), stat=i_err)
    if (i_err/=0)call outofmemory('allocatepdf','pdf_ij')
!
!  Initialise to zero
!
    rbins = 0.0_dp
    pdf = 0.0_dp
    pdfpair = 0.0_dp
    T_r = 0.0_dp
    G_r = 0.0_dp
    G_RMC_r = 0.0_dp
    G_PDF_r = 0.0_dp
    D_r = 0.0_dp
    pdf_ij = 0.0_dp

  end subroutine allocatepdf

!****************************************************************************!
!* subroutine closepdfphonon
!****************************************************************************!
! Deallocate PDF & other neutron arrays  (ers29)
! (must be kept until after for CML calls)
!****************************************************************************!
  subroutine closepdfphonon
    implicit none
    call deallocatepdf
    call deallocatepartialpdf
    call deallocateneutronarrays
  end subroutine closepdfphonon

!****************************************************************************!
!* subroutine deallocatepdf
!****************************************************************************!
!
!  deallocate pdf arrays. see allocatepdf for details of arrays
!
!  written by Elizabeth Cope (ers29) November 2006
!
!  4/07 made .f90 and added call to deallocate_error
!
!****************************************************************************!
  subroutine deallocatepdf
    implicit none
    integer(i4) :: ierr

    if (allocated(rbins)) then
      deallocate(rbins, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','rbins')
    endif
    if (allocated(pdf)) then
      deallocate(pdf, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','pdf')
    endif
    if (allocated(pdfpair)) then
      deallocate(pdfpair, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','pdfpair')
    endif
    if (allocated(T_r)) then
      deallocate(T_r, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','T_r')
    endif
    if (allocated(G_r)) then
      deallocate(G_r, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','G_r')
    endif
    if (allocated(G_PDF_r)) then
      deallocate(G_PDF_r, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','G_PDF_r')
    endif
    if (allocated(G_RMC_r))then
      deallocate(G_RMC_r, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','G_RMC_r')
    endif
    if (allocated(D_r)) then
      deallocate(D_r, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','D_r')
    endif
    if (allocated(pdf_ij)) then
      deallocate(pdf_ij, stat=ierr)
      if (ierr/=0) call deallocate_error('deallocatepdf','pdf_ij')
    endif
  end subroutine deallocatepdf

!****************************************************************************!
!* subroutine deallocatepartialpdf
!****************************************************************************!
!  deallocate partial pdf arrays
!
!  written by Elizabeth Cope (ers29) November 2006
!
!  4/07 partials pdfs deallocated here (instead of in pdfsetup)
!  4/07 made .f90 and added call to deallocate_error
!
!****************************************************************************!
  subroutine deallocatepartialpdf
    implicit none
    integer(i4) :: ierr
    if (lpartial) then
      if (allocated(partialmasses)) then
        deallocate(partialmasses, stat=ierr)
        if (ierr/=0) call deallocate_error('deallocateparitalpdf','partialmasses')
      endif
      if (allocated(partialfactor)) then
        deallocate(partialfactor, stat=ierr)
        if (ierr/=0) call deallocate_error('deallocateparitalpdf','partialfactor')
      endif

      if (allocated(partialnames)) then
        deallocate(partialnames, stat=ierr)
        if (ierr/=0) call deallocate_error('deallocateparitalpdf','partialnames')
      endif
    endif
  end subroutine deallocatepartialpdf

!****************************************************************************!
!* subroutine outpdf()
!****************************************************************************!
!  a subroutine that outputs the PDFS (Chung&Thorpe'97)
!  and the (Keen '00) D(r), G(r) and T(R) (4/7/05 ers29) to specified file
!
!  variables set on input:
!      ncf:         current configuration number
!      nrbinscfgs:  pointer of size ncf that contains the number of r bins 
!                   for this each config
!      nkpoints:    number of k points used
!      temperature: temperature of this config (used in bose calcs)
!      partialno:  number of partial pair distributions for this set up
!                   set in partialpdfsetup
!      rbins(*):    radius-array allocated to have the number of bins [A]
!  2D arrays  allocated to have correct number of bins 
!                   2nd dimension stores the total, and then partials      
!      pdf(*,*):   Chung & Thorpe's PDF (rho) [A-3]
!      T_r(*):    Keen T(r) [A-1]
!      G_r(*,*):    Keen G(r) [A2]           
!      G_PDF_r(*,*): Keen G^{PDF}(r) (as PDFFIT) [A-2]            
!      G_RMC_R(*,*): Keen G(r) in output units from RMC routines (not used in output)
!      G_R(*,*):    Keen D(r) [A-1]
!
!  11/01/07 RMC output removed 11th Jan
!  05/03/07 if (sqrt((pdf(ii, ipar))**2) < 1E-40) pdf(ii,ipar) = 0.0
!  written by Elizabeth Cope 4th May 2005
!  4/07 updated for configs and added datestamp for output title
!
!****************************************************************************!

  subroutine outpdf()
    use current,        only : temperature, ncf !temperature of current config, and config no
    use configurations, only : names ! titles of configurations
    implicit none

    integer(i4)        :: iout !ouput channel
    integer(i4)        :: ir !r counter
    integer(i4)        :: iend ! file ending starts at this position in string
    integer(i4)        :: ipar ! partial counter
    character(len=6)   :: thisunitsname ! name of units in current state
    character(len=80)  :: parstring ! string of partial details
    character(len=5)   :: parname ! partial name
    character(len=80)  :: root ! file root
    iout = 9
    !     set up local variables to do with file names, partial names etc
    iend = index(pdffilecfg,'.wid')-1
    root = pdffilecfg(1:iend)
    do ipar = 1, partialno+1
      if (ipar == 1) then
        pdffilecfg = trim(adjustl(root))//'.pdfs'
        write(ioout,'(2x,"PDF output being written to ",a)') trim(pdffilecfg)
      else
        write(parname,'(I5)')ipar-1
        write(parstring,*)"_",trim(adjustl(parname)),&
             "_",trim(adjustl(partialnames(ipar-1,1))),&
             "_",adjustl(trim(partialnames(ipar-1,2)))
        parstring = trim(adjustl(parstring))
        pdffilecfg = trim(adjustl(root))//trim(adjustl(parstring))//'.pdfs'
        write(ioout,'(2x,"partial PDF output ",i3," being written to ",a)')&
             ipar-1,trim(pdffilecfg)
      endif

      if (pdffilecfg .ne. ' ') then !open the output file
        open (iout,file=pdffilecfg,status='unknown')
      endif

      !     output data
      !     title
      write(iout,'("#################################################")')
      if (ipar>1) then
        write(iout,'("           #### Partial PDF output ####")')         
        write(iout,'("weighted rho_ij and weighted g_ij can be summed")')
        write(iout,'("directly to give the total PDF. ")')
        write(iout,'("The unweighed Keen g_ij gives G(r) as")')
        write(iout,'("G(r) = SUM(ci cj bbari bbarj [g_ij(r) -1])")')
      else
        write(iout,'("            #### PDF output ####")')
      endif
      write(iout,'("Output for configuration ",I2,": ",A)')ncf, names(ncf)
      write(iout,'("Number of rbins:   ",I4)') nrbinscfgs(ncf)
      write(iout,'("Number of kpoints: ",I8)') nkpoints
      write(iout,'("Temperature:       ",F7.2,"K")') temperature
      if (ipar>1) then
        write(iout,'("partial PDFs for pair: ",A," - ",A)')&
              partialnames(ipar-1,1), partialnames(ipar-1,2)
        write(iout,'("  with atomic masses:",F6.2," -",F6.2," amu")')&
              partialmasses(ipar-1,1),partialmasses(ipar-1,2)
      endif
      call datetimechannel(2_i4,iout) !current time
      write(iout,'("#################################################")')

      if (lfreqcut) then            
        write(iout,'(/, " Using cutoffs:")')
        if (wmincfgs(ncf).ne.0.0_dp) then 
           call changethisfreq(.false.,wmincfgs(ncf),thisunitsname)
           write(iout,*) "minimum w cutoff", wmincfgs(ncf), thisunitsname
           call changethisfreq(.true.,wmincfgs(ncf),thisunitsname)
           if (lkeepcut) then
              write(iout,*) "Seting all w<wmin to be wmin"
           else
              write(iout,*) "Excluding all w<wmin"
           endif
        endif
        if (wmaxcfgs(ncf).ne.0.0_dp) then
          call changethisfreq(.false.,wmaxcfgs(ncf),thisunitsname)
          write(iout,*) "maximum w cutoff", wmaxcfgs(ncf), thisunitsname
          call changethisfreq(.true.,wmaxcfgs(ncf),thisunitsname)
          if (lkeepcut) then
            write(iout,*) "seting all w>wmax to be wmax"
          else
            write(iout,*) "excluding all w>wmax"
          endif
        endif
      else
        write(iout,*) "Using full frequency-range"
      endif
      write(iout,'("#################################################")')
      write(iout,*)""
      if (ipar>1) then
        write(iout,*) "  r[A]  ,  weighted rho_ij(r)[A-3],  weighted g_ij(r)[A2],      g_ij(r)" 
        do ir = 1,nrbinscfgs(ncf)
          write(iout,'(f8.5,3(" ,      ",e14.5e3))' ) &
                rbins(ir),pdf(ir,ipar),G_r(ir,ipar), G_RMC_r(ir,ipar)
        enddo
      else
        write(iout,*) "  r[A]  , rho(r)[A-3] ,GPDF(r)[A-2] ,   D(r)[A-1] ,",&
                      "  G(r)[A2]   ,   T(r)[A-1] " 
        do ir = 1,nrbinscfgs(ncf)
          write(iout,'(f8.5,5(",",e14.5e3))' ) &
                rbins(ir),pdf(ir,1),G_PDF_r(ir),D_r(ir),G_r(ir,1),T_r(ir)
        enddo
      endif
!
!  Close file
!
      close(iout)
    enddo
!
!  Close off output
!
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  End of PDF calculation'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(/,''--------------------------------------------------------------------------------'')')
    return

  end subroutine outpdf

!****************************************************************************!
!* subroutine partialpdfsetup
!****************************************************************************!
!  uses atomic masses to indenfiy the number of unique partial pairs 
!  (partialno) and generates arrays holding the mass-pairs and name-pairs 
!  to identify these. (This is especially useful when working with mean
!  field atoms, as the pair component is then named by the concatenation of
!  the contributing atoms and their weighted masses)     
!
!  MUST BE CALLED BEFORE PDF ARRAYS CAN BE ALLOCATED as number of partials 
!  needs to be known
!
!  Makes use of element information arrays set up in subroutine masses
!  to link atom number
!
!  Main Output:
!      partialno : stored in neutron module. Contains number of partials
!      partialmasses(partialno,2): stored in neutron module.for each
!                  partial (1st dimension) 2nd dimension holds the two
!                  contributing atomic masses
!      partialnames(partialno,2): stored in neutron module.for each
!                  partial (1st dimension) 2nd dimension holds the two
!                  contributing atomic names (concatonated names for
!                  mean field atoms)
!      partialfactor(partialno) : cicj:stored in neutron module for each partial
!                  holds {sumcibbari}^2
!
!
!  written by Elizabeth Cope (ers29) April 2006
!  04/07 made .f90 and deallocate errors handled by subroutine
!  neutron module variables:
!     nmodes is the number of modes 
!     natomp is the number of (fully occupied) cores
!****************************************************************************!
  subroutine partialpdfsetup
    implicit none
    real(dp)                                      :: mass1
    real(dp)                                      :: mass2
    real(dp)                                      :: massb
    real(dp)                                      :: massu
    character(len=5)                              :: names1
    character(len=5)                              :: names2
    character(len=5)                              :: namesb
    character(len=5)                              :: namesu
    real(dp),         dimension(:,:), allocatable :: list
    character(len=5), dimension(:,:), allocatable :: listnames
    integer(i4)                                   :: i,ierr
    integer(i4)                                   :: j
    integer(i4)                                   :: ii
!     
!  Make an array that lists masses of 'found' types
!     
    allocate(list(natomp**2,2), stat = ierr)
    if (ierr/=0) call outofmemory('partialpdfsetup','list')
    allocate(listnames(natomp**2,2), stat = ierr)
    if (ierr/=0) call outofmemory('partialpdfsetup','listnames')
    mass1 = 0.0_dp
    mass2 = 0.0_dp
    list = 0.0_dp
    listnames = ' '
    names1 = ' '
    names2 = ' '
!
!  First count number of partials
!  Use atomic mass to indentity atom (to allow for mean-field atoms from partial occupancies)
!
    partialno = 1
    do i = 1, natomp
      jloop : do j = 1, natomp
        mass1 = arraymasses_amu(i)
        names1 = nameslist(i)(1:5)
        mass2 = arraymasses_amu(j)
        names2 = nameslist(j)(1:5)
        if (mass1 <= mass2) then
          massb = mass1
          namesb = names1
          massu = mass2
          namesu = names2
        else
          massb = mass2
          namesb = names2
          massu = mass1
          namesu = names1
        endif
        if (partialno == 1) then
          list(partialno,1) = massb
          list(partialno,2) = massu
          listnames(partialno,1) = namesb
          listnames(partialno,2) = namesu
          partialno = partialno + 1
        else
          do ii = 1,partialno
            if ((massb == list(ii,1)).and.(massu == list(ii,2))) then
              cycle jloop
            endif
          enddo
          list(partialno,1) = massb
          list(partialno,2) = massu
          listnames(partialno,1) = namesb 
          listnames(partialno,2) = namesu
          partialno = partialno + 1
        endif
      enddo jloop
    enddo
    partialno = partialno - 1
!
!  Allocate partial name lists stored in pdf
!  Later, these are deallocated from phonon using deallocatepartialpdf
!
    call deallocatepartialpdf()
    allocate (partialmasses(partialno,2), stat = ierr)
    if (ierr/=0) call outofmemory('partialpdfsetup','paritalmasses')
    allocate (partialfactor(partialno), stat = ierr)
    if (ierr/=0) call outofmemory('partialpdfsetup','paritalfactor')
    allocate (partialnames(partialno,2), stat = ierr)
    if (ierr/=0) call outofmemory('partialpdfsetup','partialnames')

    do i = 1,partialno
      partialmasses(i,1:2) = list(i,1:2)
      partialnames(i,1:2) = listnames(i,1:2)
    enddo

    deallocate(list, stat = ierr)
    if (ierr/=0) call deallocate_error('partialpdfsetup','list')
    deallocate(listnames, stat = ierr)
    if (ierr/=0) call deallocate_error('partialpdfsetup','listnames')

  end subroutine partialpdfsetup

!****************************************************************************!
!* subroutine pdfsetup
!****************************************************************************!
!  MAIN PDF CALLING ROUTINE:
!  pdfsetup setups up necessary array and calls the necessary functions to 
!  calculate peak widths and correlation functions
!
!  Calls the following principle routines:
!      partialpdfsetup: to count number of partials and make naming arrays
!      allocatepdf:     to allocate all approriate arrays and reinitialise
!      pdfwidthsq:      to calculated the gaussian width sq of each 
!                       pair contribution 
!
!  makes use of various variables stored in neutron module, especially the
!  elemental information arrays by atom number produced using the neutron
!  subroutine masses
!     nmodes is the number of modes 
!     natomp is the number of (fully occupied) cores
!     epolar_array(3, nu, atom, kpoint): 
!               polarisation vector for each mode(nu), atom and kpoint 
!     omega_array(nu,kpoints): frequency for each mode(nu) and kpoint
!               units rad/s
!
!  variables set on input:
!      ncf:         current configuration number
!      nrbinscfgs:  pointer of size ncf that contains the number of r bins 
!                   for this each config
!      nkpoints:    number of k points used
!      temperature: temperature of this config (used in bose calcs)
!      partialno:  number of partial pair distributions for this set up
!                   set in partialpdfsetup
!      rbins(*):    radius-array allocated to have the number of bins [A]
!  2D arrays  allocated to have correct number of bins 
!                   2nd dimension stores the total, and then partials      
!      pdf(*,*):   Chung & Thorpe's PDF (rho) [A-3]
!      T_r(*):    Keen T(r) [A-1]
!      G_r(*,*):    Keen G(r) [A2]           
!      G_PDF_r(*): Keen G^{PDF}(r) (as PDFFIT) [A-2]            
!      G_RMC_R(*): Keen G(r) in output units from RMC routines (not used in output)
!      D_R(*):    Keen D(r) [A-1]
!
!  written by Elizabeth Cope (ers29) April 2005
!  4/07 CML moved to phonon 
!  4/07 modified for multiple configurations and 
!       added datetime stamp to output and made .f90
! 11/07 additional comments added and reference to concentration 
!       replaced by 1/natomp
!****************************************************************************!
  subroutine pdfsetup(lstore)
    use parallel,       only : ioproc !parallel output flag
    use m_pdfvariables                !stored for CML output
    use current,        only : rv,temperature, ncf, a, b, c
    ! rv converts from fractional to cartiesan position. nb need rvT for matmul
    use constants,      only : pi
    use configurations, only : names

    implicit none

    logical, intent(in)      :: lstore !store statisticsfor PDF CML ouput?
!
!  Local variables
!
    integer(i4), dimension(3)                     :: lvec 
    real(dp), dimension(3)                        :: R_ij           !interpair spacing vector     
    real(dp)                                      :: widthsq_A2_max !widthsq_A2_max to monitor max width
    real(dp)                                      :: widthsq_A2     !widths_A2 to monitor max width
    real(dp)                                      :: widthsq        !widthsq to monitor max widths
    real(dp)                                      :: drbeth         ! r step size
    real(dp)                                      :: factor         ! keen's (sum(cb))^2
    real(dp)                                      :: w_ij           !Peterson's pair weighting (b_i b_j)/factor
    real(dp)                                      :: prefactor      ! 1/sqrt(2 pi widthsq)
    real(dp)                                      :: deltar_sq      !(|R_ij|-r)^2
    real(dp), parameter                           :: test = 1d-5
    real(dp)                                      :: mass1
    real(dp)                                      :: mass2
    real(dp)                                      :: massb
    real(dp)                                      :: massu
    real(dp)                                      :: wminsofar, wmaxsofar !tracking
    real(dp)                                      :: rho0 !number density
    integer(i4)                                   :: i,ir,i_cfg, ip
    integer(i4)                                   :: n !number of cells needed for Rmax
    integer(i4)                                   :: i_atom,j_atom
    integer(i4)                                   :: j_x,j_y,j_z
    integer(i4)                                   :: ii,j
    integer(i4)                                   :: i_ci, i_cj      ! concentration counters
    integer(i4)                                   :: iend
    integer(i4)                                   :: iout= 9
    integer(i4)                                   :: number_of_pairs ! counter
    integer(i4)                                   :: npart           !link to 2nd dimenstion of pdf arrays for current partial
    character(len=6)                              :: theseunits      !current freq units
    real(dp), dimension(:,:), allocatable ::paircount
!
!  Functions: 
!
    real(dp)                                      :: volume !in volume.f
!
!  lpartial flag means calculated partials as well as total pdf:
!
    if (lpartial) call partialpdfsetup ! calculated no different partials by atomic masses, and returns partialno.

    number_of_pairs = 0
    if (.not.lpartial) partialno = 0

    call allocatepdf() !needs to know number of partials first
    if (allocated(paircount)) deallocate(paircount)
    allocate(paircount(partialno+1,4))
    paircount = 0
!
    wmaxsofar = 0.0_dp
    wminsofar = 0.0_dp
    widthsq_A2_max = 0.0_dp 
    widthsq_A2 = 0.0_dp
    widthsq = 0.0_dp
    rho0 = natomp/volume(rv)!     set up number density
!
!  Fill rbins (stored in m_pdfneutron module)
!
    drbeth = rmaxcfgs(ncf) / real(nrbinscfgs(ncf))
    do i = 1,nrbinscfgs(ncf)
      rbins(i) = real(i) * drbeth
    enddo
!
!  Set up output file
!
    pdffilecfg = pdffiles(ncf)
    if (pdffilecfg == ' ') then
      write(pdffilecfg,*) ncf
      pdffilecfg = "pdf_config"//trim(adjustl(pdffilecfg))//'.wid'
      if (ioproc) write(ioout,'(/, "Add [output pdf <filename>]to input to specifiy file")')
    else
      do i_cfg = 1,ncf-1
        if (pdffilecfg == pdffiles(i_cfg)) then
          write(pdffilecfg,*) "config_", ncf, "_",pdffiles(ncf)
          iend = index(pdffilecfg,'.wid')
          pdffilecfg =  pdffilecfg(i:iend)//"_config"//trim(adjustl(pdffilecfg))//'.wid'
        endif
      enddo
    endif
    if (.not.(lnowidth)) then
      if (ioproc) write(ioout,'(/,"  PDF peak widths being written to ",a)') pdffilecfg
      if (pdffilecfg.ne.' ') then !open the output file
        open(iout,file=pdffilecfg,status='unknown')
      endif
    endif
!
!  Set up local output variables
!
    if (ioproc) then
      write(ioout,'(/,"  PDF information:",/)')
      write(ioout,'("  Maximum radius        = ",f6.3," Ang")') rmaxcfgs(ncf)
      write(ioout,'("  Number density        = ",f6.3," Ang(^-3)")') rho0
    endif
    factor = 0.0_dp
    do i = 1,natomp            ! from Keen, {sum(cb)}^2 
      factor = factor + bbar_cores_A(i)/natomp ! conc removed 11/07
    enddo
    factor = factor**2
    if (ioproc) write(ioout,'("  (Sum{c_i bbar_i} )^2  = ",e11.4,/)') factor
    if (lpartial) then
      partialfactor = 0.0_dp
      do ip = 1,partialno ! count through all partials
        i_ci = 0
        i_cj = 0
        do i = 1,natomp !cycle through atoms in the primitive cell
          if (arraymasses_amu(i) == partialmasses(ip,1)) then !partials are identified by mass, to match 1st element in partial
            i_ci = i_ci + 1
          endif
          if (arraymasses_amu(i) == partialmasses(ip,2)) then !partials are identified by mass, to match 2nd element in partial
            i_cj = i_cj + 1
          endif
        enddo
        partialfactor(ip) = (dble(i_ci)/natomp)*(dble(i_cj)/natomp)
        if (partialmasses(ip,1) .ne. partialmasses(ip,2) ) partialfactor(ip) = partialfactor(ip) * 2 !! to count AB and BA pairs
             
        if (ioproc) write(ioout,'(2x,a,"_",a," partial c_ic_j   = ",f9.4)') &
                trim(adjustl(partialnames(ip,1))),adjustl(trim(partialnames(ip,2))),partialfactor(ip) 
      enddo
    endif
!
!  Output data to file
!
    if (ioproc.and.(.not.(lnowidth))) then
      ! title
      write(iout,'("#################################################")')
      write(iout,'("             #### PDF peak widths output ####")')
      write(iout,'("Output for configuration "I2,": ",A)')ncf, names(ncf)
      write(iout,'("Number of kpoints: ",I8)') nkpoints
      write(iout,'("Temperature:       ",F7.2,"K")') temperature
      write(iout,'("Rmax = ",F7.3, " A")') rmaxcfgs(ncf)
      call datetimechannel(2_i4,iout) !current time
      write(iout,'("#################################################")')
      if (lfreqcut) then     !updated 18th october 2006 to change units
        write(iout,'(/, " Using cutoffs:")')
        if (wmincfgs(ncf).ne.0.0_dp) then 
           call changethisfreq(.false.,wmincfgs(ncf),theseunits)
           write(iout,*) "minimum w cutoff", wmincfgs(ncf), theseunits
           call changethisfreq(.true.,wmincfgs(ncf),theseunits)
           if (lkeepcut) then
              write(iout,*) "Seting all w<wmin to be wmin"
           else
              write(iout,*) "Excluding all w<wmin"
           endif
        endif
        if (wmaxcfgs(ncf).ne.0.0_dp) then
          call changethisfreq(.false.,wmaxcfgs(ncf),theseunits)
          write(iout,*) "maximum w cutoff", wmaxcfgs(ncf), theseunits
          call changethisfreq(.true.,wmaxcfgs(ncf),theseunits)
          if (lkeepcut) then
            write(iout,*) "seting all w>wmax to be wmax"
          else
            write(iout,*) "excluding all w>wmax"
          endif
        endif
        write(iout,'("#################################################")')
      else
        write(iout,*) "Using full frequency-range"
      endif
      write(iout,'(/,"#################################################")')
    endif
    pdf = 0.0_dp
    pdf_ij = 0.0_dp
!
!  n = number of cells needed to cover radius Rmax
!
    if (ioproc) write(ioout,'(2x,a,3(f8.4,1x))') "Using primitive cell parameters ",a, b, c

    n = 1 + int ((rmaxcfgs(ncf)/(min(a,b,c))) +0.5) ! to ensure 'it rounds up'

    if (ioproc) then
      write(ioout,'(2x,"Testing primitive vectors with x,y,z=-(n+1) to n, n= ",i4)') n
    endif
    if (ioproc.and.(.not.(lnowidth))) write(iout,*)&
          "|R_ij|(A),  width(A), atomi, #, atomj, #,",&
          "     Rij_x  ,      Rij_y  ,     Rij_z"

    pdf = 0.0_dp
    if (lnowidth.and.ioproc) write(ioout,'("[width output suppressed by keyword nowidth]")')
    do i_atom = 1,natomp
      do j_atom = 1,natomp
        !     w_ij as in Peterson et al
        w_ij = (bbar_cores_A(i_atom))*(bbar_cores_A(j_atom)) /factor

        if (lpartial) then
          !     test which partial pair this is (by mass)
          mass1 = arraymasses_amu(i_atom)
          mass2 = arraymasses_amu(j_atom)
          if (mass1 <= mass2) then
            massb = mass1
            massu = mass2
          else
            massb = mass2
            massu = mass1
          endif
          do ii = 1, partialno
            if ((massb == partialmasses(ii,1)).and.(massu == partialmasses(ii,2))) then
              npart = ii+1 !stored as the ii+1th dimension of arrays
            endif
          enddo
        else
          npart = 1        !stored as the 1st dimension of arrays
        endif
        paircount(npart,1) = paircount(npart,1) + 1
        paircount(npart,3) = (bbar_cores_A(i_atom))*(bbar_cores_A(j_atom))
        if (npart>1) then
          if ((paircount(npart,2).ne.0).and.(paircount(npart,2).ne.w_ij)) then
            write(ioout,*) w_ij, paircount(npart,2), "w_ij mismatch"
          endif
          paircount(npart,2) = w_ij
          paircount(npart,4) = paircount(npart,3)*partialfactor(npart-1)
        else 
          paircount(npart,4) = factor
        endif
        !     

        pdfpair = 0.0_dp
        do j_x = -(n+1),n
          do j_y = -(n+1),n
            do j_z = -(n+1),n

              !     set up vector to origin of "second" cell
              lvec = (/j_x, j_y, j_z/)
              Rlbeth = matmul(lvec, rvT)
              ! rvT converts from fractional to cartiesan position
              R_ij = Rlbeth + (cartvec(j_atom,1:3)) - cartvec(i_atom,1:3)

              if (mod_vector(R_ij)<= test) cycle !check not itself
              if (mod_vector(R_ij)>(rmaxcfgs(ncf)+test)) cycle !check <rmaxcfgs(ncf)

              number_of_pairs = number_of_pairs + 1 !count
              !     find width
              widthsq = pdfwidthSQ(i_atom,j_atom,lvec,R_ij,wminsofar,wmaxsofar)
              widthsq_A2 = widthsq*1d20 !convert from m2 to A2
              widthsq_A2_max = max(widthsq_A2_max, widthsq_A2)

              if (widthsq /=0.0_dp) then
                if (ioproc .and. (.not.(lnowidth))) &
                  write(iout,'(f10.4,", ",e11.4,", ",a,", ",i3,", ",a,", ",i3,3(", ",f10.6))') & !changed 18th october 06
                        mod_vector(R_ij),sqrt(widthsq_A2),trim(nameslist(i_atom)),i_atom,trim(nameslist(j_atom)),j_atom,R_ij

                do ir = 1,nrbinscfgs(ncf)
                  prefactor = 1.0_dp / (sqrt( 2*pi*widthsq_A2))

                  deltar_sq = (mod_vector(R_ij)-rbins(ir))**2
                  pdf_ij(ir) = pdf_ij(ir) + prefactor * exp (-deltar_sq / (2*widthsq_A2) )
                enddo
                pdfpair = pdfpair + pdf_ij  ! weighted removed (to do unweighted partials below)

                !this is only the sum compenent of rho(r) = 1/N SUM{w_ij rho_ij(r)}
                pdf_ij = 0.0_dp
              else 
                pdf_ij = 0.0_dp
              endif

            enddo
          enddo
        enddo
          
        pdf(1:nrbinscfgs(ncf),1) = pdf(1:nrbinscfgs(ncf),1) + pdfpair*w_ij ! weighting put here ONLY
        if (npart>1) then ! also store weighted version in partial
          pdf(1:nrbinscfgs(ncf),npart) = pdf(1:nrbinscfgs(ncf),npart) + pdfpair*w_ij
        endif
      enddo !end j loop
    enddo ! end i loop

    !     now we have all contibrutions
    pdf = pdf/natomp   !average over all rmaxcfgs(ncf) spheres
    if (ioproc .and. lpartial) then
      write(ioout,'(/,2x,"Partial weightings:",/)')
      do i = 2,partialno+1
        write(ioout,'(2x,"partial ",i4," (",a5,a5,"): w_ij = ",f8.4,",n = ",i4,",cicjbibj = ",e14.6)') &
              i-1,trim(adjustl(partialnames(i-1,1))), &
              adjustl(trim(partialnames(i-1,2))),paircount(i,2),int(paircount(i,1)),paircount(i,4)
      enddo
    endif
!
!  Close file
!
    if (.not.(lnowidth).and.ioproc) then 
      close(iout)
      write(ioout,'(/,2x,a,/)') "Width output to file complete"
    endif
!
!  Make standard distributions 
!
    do i = 1,nrbinscfgs(ncf)
      !having finished summing,
      !now perform volume averaging:divide by 4pir^2, as in peterson
      pdf(i,1:partialno+1) = pdf(i,1:partialno+1)/(4.0_dp*pi*(rbins(i))**2) !this is rho(r) and weighted partials
!
!  Convert partials to weighted g_ir(r) 
!
      do j = 2,(partialno+1)
        G_r(i,j)= ((factor/rho0)*(pdf(i,j) - rho0)) + factor - paircount(j,4)
          
        !! and unweighted
        G_RMC_r(i,j) = (G_r(i,j)/(paircount(j,4))) + 1
      enddo
!
!  Now convert full partials
!
      G_PDF_r(i) = 4.0_dp*pi*rbins(i)*(pdf(i,1)-rho0) !this is G_PDF(r)
      ! convert to D(r) using equation 44 of Keen
      D_r(i) = G_PDF_r(i)*factor
      ! convert to Keen G_r using equation 29 of Keen
      G_r(i,1) = D_r(i)/(4.0_dp*pi*rbins(i)*rho0)
      ! convert to T_r using equation 30 of Keen
      T_r(i) = D_r(i) + 4.0_dp*pi*rbins(i)*rho0*factor
    enddo
!
!  Output statistics
!
    if (ioproc) then
      write(ioout,'(2x,"PDF statistics:",/)')
      write(ioout,'(2x,"Maximum width^2  was ",e11.4," Ang^2")') widthsq_A2_max
      write(ioout,'(2x,"Number of pairs",i10)') number_of_pairs
      write(ioout,'(2x,"Angular frequency range used:")')
      write(ioout,'(2x,e13.6," to ",e13.6," ",a)') wminsofar,wmaxsofar, unitsnamecfgs(ncf)
!
!  Change wminsofar into I/O units, write out, then put back to rad/s
!
      call changethisfreq(.false., wminsofar, theseunits)
      call changethisfreq(.false., wmaxsofar, theseunits)
      write(ioout,'(2x,e13.6," to ",e13.6," ",a,/)') wminsofar,wmaxsofar, theseunits
!
!  Store statistics in pdfstats module for cml output
!
      if (lstore) then
        widmax = widthsq_A2_max
        np = number_of_pairs
        wmin_inunits = wminsofar
        wmax_inunits = wmaxsofar
        iounits = theseunits
        numdensity = rho0
        sum_cbbar_sq = factor
      endif
      call changethisfreq(.true., wminsofar, theseunits)
      call changethisfreq(.true., wminsofar, theseunits)
    endif
    if (ioproc) call outpdf()

  end subroutine pdfsetup

!****************************************************************************!
!* function pdfwidthsq
!****************************************************************************!
! PDFwidthSQ calculates the square of the width of a pair distribution function
! and bose_eins (in neutron module)
!
! equations from Reichardt and Pintschovious Phys Rev B 63 174302 (2001)
!
! written by Elizabeth Cope (ers29) 2005
!    04/07 functions moved into neutron module
!    04/07 intents added and made .f90
!    04/07 updates for multiple configurations 
!    11/09 Rewritten to use Chung and Thorpe Equations: fewer rounding errors
!
! i_atom and j are the atom numbers sites concerned
!     and link to elemental information arrays created in neutron
!     subroutine masses
! l is the lth cell(lx, ly, lz) with position vector lvec 
!                         (carestian Rlbeth in neutron)
! R_ij is the (Cartesian) interpair vector
! wmin- and wmaxsofar keep count of accepted frequencies
!
! natomp is the number of (fully occupied) cores
!****************************************************************************!
  function pdfwidthsq(i_atom,j_atom,lvec,R_ij,wminsofar,wmaxsofar)
    use current,   only : ncf     ! config number
    use constants, only : hbar_js !planks constant in Js
    use general,   only : nwarn   ! GULP warning counter
    implicit none
!
!  Passed variables
!
    real(dp)                                 :: pdfwidthsq !function type
    integer(i4),               intent(in)    :: i_atom, j_atom
    real(dp),    dimension(3), intent(in)    :: R_ij
    integer(i4), dimension(3), intent(in)    :: lvec
    real(dp),                  intent(inout) :: wmaxsofar
    real(dp),                  intent(inout) :: wminsofar
!
!  Local variables
!
    real(dp)                                 :: running, omega_rs,bose
    real(dp)                                 :: copmodsq, thisone
    integer(i4)                              :: k, i_nu
    logical                                  :: lwrite
    character(len=100)                       :: warningstring
!
! variables for Chung version of equations
!
    real(dp)                                 :: absi ! |e(k).r_i|
    real(dp)                                 :: absj ! |e(k).r_j|
    real(dp)                                 :: absj_negk ! |e*(k).r_j| = |e(-k).r_j| when symmetric about Gamma
    real(dp), dimension(3)                   :: unitvector ! unit vector of direction r_ij
!
    lwrite = .false. !debug switch
    if (lwrite) then
      write(*,'("Pair:",i2,",",a," to", i2,",",a," in p. cell",3(i3))') &
            i_atom,trim(nameslist(i_atom)),&
            j_atom,trim(nameslist(j_atom)),lvec
      write(*,*) "       |R_0i|:", mod_vector(cartvec(i_atom, 1:3)) 
      write(*,*) "       |R_0j|:", mod_vector(cartvec(j_atom,1:3))
      write(*,*) "Rij", R_ij
      write(*,*) "       |Rlbeth|  : ", mod_vector(Rlbeth)
      write(*,*) "       |R_ij|: ", mod_vector(R_ij)
    endif

    running = 0.0_dp ! running total for this loop

    kloop : do k = 1,nkpoints
      thisone = 0.0_dp 
      call setcurk(k, .false.)   !veck, veckcart & icurrentk in neutron
      if (lwrite) then
        write(*,*) "#k, |R|",k,mod_vector(R_ij)
      endif

      nuloop : do i_nu = 1,nmodes 
        omega_rs = omega_array(i_nu,k) !frequency in rad/s
!
!  Use frequency cut off?
!
        if (lfreqcut) then 
          if (lkeepcut) then !replace w with w_cut
            if ((wmincfgs(ncf).ne.0.0_dp).and.(omega_rs<wmincfgs(ncf))) omega_rs = wmincfgs(ncf)
            if ((wmaxcfgs(ncf).ne.0.0_dp).and.(omega_rs>wmaxcfgs(ncf))) omega_rs = wmaxcfgs(ncf)
          else
            if ((wmincfgs(ncf).ne.0.0_dp).and.(omega_rs<wmincfgs(ncf))) cycle
            if ((wmaxcfgs(ncf).ne.0.0_dp).and.(omega_rs>wmaxcfgs(ncf))) cycle
          endif
        endif

        if (abs(omega_rs) <1.0d7) then ! ignoring anything effectively zero
          cycle
        endif

        if (omega_Rs<-1.0d7) then
          write(warningstring,'("-ve w at k # ",i7," mode # ",i4,": ",E12.4," rad/s")')&
                k,i_nu,omega_rs
          call outwarning(trim(adjustl(warningstring)), 0_i4)
          nwarn = nwarn + 1
        endif

        if (wminsofar == 0.0_dp) then
          wminsofar = omega_rs
        else
          wminsofar = min(omega_rs, wminsofar)
        endif
        wmaxsofar = max(omega_rs, wmaxsofar)

        if (lwrite) then
          write(*,*) "______", k,i_nu
          write(*,*) "k",veckcart
          write(*,*) "Rlm",(Rlbeth + cartvec(j_atom, 1:3))
          write(*,*) "R0i", cartvec(i_atom, 1:3)
          write(*,*) "e_j",epolar_array(1:3,i_nu,j_atom,k)
          write(*,*) "e_i",epolar_array(1:3,i_nu,i_atom,k)
          write(*,*) "w",omega_rs
          write(*,*) "_____"
        endif
        unitvector = mod_vector(R_ij)
        unitvector = R_ij / unitvector

        absi = abs(dot_product(epolar_array(1:3,i_nu,i_atom,k),unitvector))
        absj = abs(dot_product(epolar_array(1:3,i_nu,j_atom,k),unitvector))
        absj_negk = abs(dot_product(conjg(epolar_array(1:3,i_nu,j_atom,k)),unitvector))

        copmodsq = (absi**2 /(2.0_dp*(arraymasses_kg(i_atom)))) +&
             &(absj**2/(2.0_dp*(arraymasses_kg(j_atom)))) -&
             &((absi*absj_negk*euler(dot_product(veckcart,R_ij)))/&
             &(sqrt((arraymasses_kg(i_atom)) * (arraymasses_kg(j_atom)))))

        bose = bose_eins(omega_rs)

        thisone = ((bose+0.5_dp)*copmodsq)/omega_rs

        if (thisone<-1.0d6) then
           call changethisfreq(.false., omega_rs)
           write(warningstring,*)"frequency ", omega_rs," leads to negative term inside sum:", thisone
           call outwarning(trim(adjustl(warningstring)), 0_i4)
           call changethisfreq(.true., omega_rs)
          nwarn = nwarn + 1
        endif
        running = running + thisone

        if (lwrite) then
          write(*,'(3(F10.4, 1x))')R_ij
          write(*,*) "|R|",mod_vector(R_ij)
          write(*,*) "|comp|sq",copmodsq
          write(*,*) thisone
        endif
      enddo nuloop
    enddo kloop
    if (lwrite) then
      write(*,*) "__"   
      write(*,*) "final running", running
    endif
    PDFwidthSQ = (2.0*hbar_js*running)/(dble(nkpoints))

  end function Pdfwidthsq
!****************************************************************************!

end module m_pdf
