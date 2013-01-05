  subroutine outspec_pdf
!
!  Output species information: neutron scattering table
!
!  ratiom = controls split of mass between cores and shells
!
!   3/07 Made from outspec for neutrons, Elizabeth Cope (Ers29)
!   4/07 .f90
!   6/09 Style updated for integration
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009.
!  
  use constants
  use element
  use iochannels
  use library
  use m_pdfneutron
  use shell
  use species
  implicit none
!
!  Local variables
!
  character(len=6) :: stype(4)
  character(len=5) :: lab
  integer(i4)      :: i
  integer(i4)      :: ind
  integer(i4)      :: na
  integer(i4)      :: nrms
  integer(i4)      :: ntyp
  logical          :: lfound
  real(dp)         :: convertA
  real(dp)         :: convertA2
!
  data stype/'Core  ','Shell ','BCore ','BShell'/
  convertA2 = 1d-8
  convertA = 1d-4
!*******************
!  Species output  *
!*******************
  write(ioout,'(''  Species output for PDF data: '',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Species    Type       bbar     sigma_inc  sigma_coh     '')')
  write(ioout,'(''                       (Angs)     (Angs^2)   (Angs^2)     '')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  do i = 1,nspec
    na = natspec(i)
    ntyp = ntypspec(i)
    call label(na,ntyp,lab)
    ind = 1
    if (na.gt.maxele) then
      na = na - maxele
      ind = 2
    endif
!
!  Find shellmass ratio type
!
    lfound = .false.
    nrms = 0
    do while (nrms.lt.nratiomspec.and..not.lfound) 
      nrms = nrms + 1
      if (na.eq.natratiomspec(nrms).and.(ntyp.eq.ntypratiomspec(nrms).or.ntypratiomspec(nrms).eq.0)) then
        lfound = .true.
      endif
    enddo
    if (lbrspec(i)) ind = ind + 2
    write(ioout,'(4x,a5,4x,a6,3x,3(E10.4, 1x))') &
          lab,stype(ind), bbar(na)*convertA, siginc(na)*convertA2,4.0_dp*pi*bbar(na)*bbar(na)*convertA2
  enddo
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(/)')
!
  return
  end subroutine outspec_pdf
