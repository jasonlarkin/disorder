  subroutine outspec
!
!  Output species information
!
!  ratiom = controls split of mass between cores and shells
!
!   7/05 ratiom made species specific
!   5/06 Species specific mass introduced
!   4/07 Output of UFF data added
!   7/09 Bug in mass output corrected for shell model MD case
!   6/10 No of decimal points for charge increased by C.Fisher
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, June 2010.
!
  use element
  use iochannels
  use library
  use shell
  use species
  use uffdata
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
  real(dp)         :: atm
!
  data stype/'Core  ','Shell ','BCore ','BShell'/
!*******************
!  Species output  *
!*******************
  write(ioout,'(/,''********************************************************************************'')')
  write(ioout,'(''*  General input information                                                   *'')')
  write(ioout,'(''********************************************************************************'')')
  write(ioout,'(/,''  Species output for all configurations : '',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Species    Type    Atomic    Atomic    Charge       Radii (Angs)     Library'')')
  write(ioout,'(''                     Number     Mass       (e)     Cova   Ionic  VDW   Symbol'')')
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
    atm = massspec(i)
    nrms = 0
    do while (nrms.lt.nratiomspec.and..not.lfound) 
      nrms = nrms + 1
      if (na.eq.natratiomspec(nrms).and.(ntyp.eq.ntypratiomspec(nrms).or.ntypratiomspec(nrms).eq.0)) then
        lfound = .true.
        if (ind.eq.2) then
          atm = atmass(na)*ratiomspec(nrms)
        else
          atm = massspec(i)*(1.0_dp - ratiomspec(nrms))
        endif
      endif
    enddo
    if (lbrspec(i)) ind = ind + 2
    write(ioout,'(4x,a5,4x,a6,3x,i4,5x,f6.2,1x,f10.6,2x,f6.3,1x,f6.3,1x,f6.3,1x,a9)') &
      lab,stype(ind),na,atm,qlspec(i),rcov(na),radspec(i),rvdw(na),libspec(i)(1:9)
  enddo
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(/)')
!
!  Out of UFF species data
!
  if (nUFFspec.gt.0) then
    write(ioout,'(''  Universal Force Field (UFF) species output: '',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Species    Type    Radius    Theta    Distance    Energy     Zeta     Zeff    '')')
    write(ioout,'(''                     (Ang)   (degrees)   (Ang)       (eV)               (e)     '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nUFFspec
      na = natUFFspec(i)
      ntyp = ntypUFFspec(i)
      call label(na,ntyp,lab)
      ind = 1
      if (na.gt.maxele) then
        na = na - maxele
        ind = 2
      endif
      write(ioout,'(4x,a5,4x,a6,1x,f7.4,2x,f8.3,2x,f9.4,1x,f9.4,1x,f9.4,1x,f8.4)') &
        lab,stype(ind),UFFr(i),UFFtheta(i),UFFx(i),UFFd(i),UFFzeta(i),UFFZeff(i)
    enddo   
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(/)')
  endif
!
  return
  end
