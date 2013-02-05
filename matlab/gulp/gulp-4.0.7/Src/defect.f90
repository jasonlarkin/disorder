  subroutine defect(ldefectOK)
!
!  Master routine to set up defect calculations
!
!   5/98 Calls to defopt/defpot/move2a1 moved to gulp.F
!        as this prevents problems in Linux case
!   4/10 Return flag added to indicate that no error occured
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
!  Julian Gale, NRI, Curtin University, April 2010
!
  use control
  use current
  use defects
  use iochannels
  use molecule
  use parallel
  implicit none
!
!  Passed variables
!
  logical,          intent(out) :: ldefectOK
!
!  Local variables
!
  character(len=80)             :: string
  integer(i4)                   :: i
  integer(i4)                   :: ndefst
  integer(i4)                   :: ndf
  integer(i4)                   :: nds
  integer(i4)                   :: nldef
  logical                       :: lrestore
  real(dp)                      :: rdcmax
!
  lrestore = .false.
  ldefectOK = .true.
!
!  Write out general banner
!
  if (ioproc) then
    write(ioout,'(''********************************************************************************'')')
    write(ioout,'(''*  Defect calculation for configuration '',i3,'' :                                  *'')')ncf
    write(ioout,'(''********************************************************************************'',/)')
  endif
!
!  Find defects for current configuration
!
  nds = 0
  if (ndef.gt.0) then
    do i = 1,ndef
      if (ndefcfg(i).eq.ncf) then
        if (nds.eq.0) nds = i
        ndf = i
        if (ndeftyp(i).eq.0) lrestore = .true.
      endif
    enddo
    ndefst = nds - 1
    nldef = ndf - nds + 1
  else
    nldef = 0
  endif
  if (nldef.eq.0) then
    if (ioproc) then
      write(ioout,'(/,''  **** No defects for configuration number '',i4,'' ****'',/)') ncf
    endif
    ldefectOK = .false.
    return
  endif
!
!  Defects not allowed with background neutralising charge
!
  if (abs(totalcharge).gt.1.0d-4) then
    if (ioproc) then
      string = 'defects not allowed with non-charge neutral cells'
      call outerror(string,0_i4)
    endif
    call stopnow('defect')
  endif
!
!  Defects not allowed with dipole correction
!
  if (index(keyword,'dipo').ne.0) then
    if (ioproc) then
      string = 'defects not allowed with dipole correction energy'
      call outerror(string,0_i4)
    endif
    call stopnow('defect')
  endif
!
!  Set up region 1 coordinates and defect parameters
!
  call setdef(nldef,ndefst,lrestore)
!
!  Set up region 2a
!
  call set2a
  if (lmolmec) call setmoldef12a
!
!  Symmetrisation of region 1
!
  call defsym
!
!  Build list of vacancies and interstitials for 2a calc
!  and find maximum distance from defect to defect centre
!     
  if (ldeflin(ncf)) then
    call findrdcmax(rdcmax)
  elseif (mode2a.ge.3) then
    call finddef(rdcmax)
  else
    rdcmax = 0.0_dp
  endif
!
!  Output nature of defects and related info
!
  if (ioproc) call outdefin(nldef,ndefst,rdcmax)
!
  return
  end
