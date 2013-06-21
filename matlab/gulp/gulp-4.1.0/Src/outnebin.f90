  subroutine outnebin
!
!  Output initial state of NEB run
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006
!
  use control
  use current
  use iochannels
  use neb
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: j
  integer(i4) :: nrfirst
  logical     :: lfound
!
!  If not the I/O node return
!
  if (.not.ioproc) return
!
  write(ioout,'(/)')
  if (lnebclimbingimage) then
    write(ioout,'(''  Climbing Image Nudged Elastic Band :'',/)')
  else
    write(ioout,'(''  Nudged Elastic Band :'',/)')
  endif
  write(ioout,'(''  Number of replicas = '',i6,/)') nnebreplica(ncf)
  write(ioout,'(''  Maximum number of iterations    = '',i15)') nnebiter
  write(ioout,'(''  Maximum force tolerance         = '',f15.9)') nebtol
!      write(ioout,'(''  Maximum displacement per step   = '',f15.9,'' Angstroms'')') nebmaxdisp
  if (lnebvaryspring(ncf)) then
    write(ioout,'(''  Max force cnst between replicas = '',f15.9,'' eV/Angs**2'')') nebspring(ncf)
    write(ioout,'(''  Min force cnst between replicas = '',f15.9,'' eV/Angs**2'')') nebspringmin(ncf)
  else
    write(ioout,'(''  Force constant between replicas = '',f15.9,'' eV/Angs**2'')') nebspring(ncf)
  endif
  write(ioout,'(''  Initial random displacement     = '',f15.9,'' Angstroms'')') nebrandom
  write(ioout,'(''  Tangent type number to MEP      = '',i15,/)') nebtangent
!
!  Output replicas
!
  lfound = .false.
  do ii = 1,nnebreplicatot
    if (nebreplicacfgptr(ii).eq.ncf) then
      if (.not.lfound) then
        nrfirst = ii
        lfound = .true.
      endif
      write(ioout,'(''********************************************************************************'')')
      write(ioout,'('' Replica number = '',i4)') ii - nrfirst + 1
      write(ioout,'(''********************************************************************************'')')
      if (ndim.eq.3) then
        write(ioout,'('' Cell : '',3(f10.5,1x),3(f8.4,1x))') (nebreplicacell(j,ii-nrfirst+1),j=1,6)
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      elseif (ndim.eq.2) then
        write(ioout,'('' Cell : '',2(f10.5,1x),f8.4)') (nebreplicacell(j,ii-nrfirst+1),j=1,3)
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      elseif (ndim.eq.1) then
        write(ioout,'('' Cell : '',f10.5)') nebreplicacell(1,ii-nrfirst+1)
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      do i = 1,nasym
        write(ioout,'(i6,1x,3(f12.6,1x),f10.6)')i,(nebreplicaxyz(j,i,ii-nrfirst+1),j=1,3), &
          nebreplicaradius(i,ii-nrfirst+1)
      enddo
    endif
  enddo
  if (lfound) then
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Final state
!
  write(ioout,'(''********************************************************************************'')')
  write(ioout,'(''  Final configuration : '')')
  write(ioout,'(''********************************************************************************'')')
  if (ndim.eq.3) then
    write(ioout,'('' Cell : '',3(f10.5,1x),3(f8.4,1x))') (nebfinalcell(j,ncf),j=1,6)
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  elseif (ndim.eq.2) then
    write(ioout,'('' Cell : '',2(f10.5,1x),f8.4)') (nebfinalcell(j,ncf),j=1,3)
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  elseif (ndim.eq.1) then
    write(ioout,'('' Cell : '',f10.5)') nebfinalcell(1,ncf)
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  do i = 1,nasym
    write(ioout,'(i6,1x,3(f12.6,1x),f10.5)')i,(nebfinalxyz(j,i,ncf),j=1,3),nebfinalradius(i,ncf)
  enddo
  write(ioout,'(''--------------------------------------------------------------------------------'',/)')
!
  return
  end
