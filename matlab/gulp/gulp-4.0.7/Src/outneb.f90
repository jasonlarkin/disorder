  subroutine outneb(nrep,fcrep,gcrep)
!
!  Output final state of NEB run
!
!   7/03 Output of gradients added
!   9/03 Output of cell stresses added
!  10/06 Gradrep zeroed to correct output
!  11/06 GULP format modified so that headers are comments
!  11/06 Modified to include radii
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
  use element,      only : maxele
  use iochannels
  use neb
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4),                     intent(in)    :: nrep
  real(dp),                        intent(in)    :: fcrep(nrep)
  real(dp),                        intent(in)    :: gcrep(nvar,nrep)
!
!  Local variables
!
  character(len=4)                               :: cstype
  character(len=5)                               :: lab
  integer(i4)                                    :: i
  integer(i4)                                    :: inat
  integer(i4)                                    :: ii
  integer(i4)                                    :: itype
  integer(i4)                                    :: ix
  integer(i4)                                    :: j
  integer(i4)                                    :: mvar
  integer(i4)                                    :: n
  integer(i4)                                    :: nn
  integer(i4)                                    :: nrfirst
  integer(i4)                                    :: nrp
  integer(i4)                                    :: status
  logical                                        :: lfound
  real(dp)                                       :: cgradrep(6)
  real(dp),    dimension(:,:), allocatable       :: gradrep
!
!  If not the I/O node return
!
  if (.not.ioproc) return
!
!  Allocate local memory
!
  allocate(gradrep(4,numat),stat=status)
  if (status/=0) call outofmemory('outneb','gradrep')
!
!  Output energies of configurations
!
  write(ioout,'(/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Energy of NEB configurations :'')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Initial configuration '',f20.8,'' eV'')') fcrep(1)
  do nrp = 2,nrep-1
    write(ioout,'(''  Replica '',i13,1x,f20.8,'' eV'')') nrp-1,fcrep(nrp)
  enddo
  write(ioout,'(''  Final   configuration '',f20.8,'' eV'')') fcrep(nrep)
  write(ioout,'(''--------------------------------------------------------------------------------'',/)')
!
!  Output replicas
!
  lfound = .false.
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''                      Coordinates                        Gradients'')')
  mvar = 3*nasym + nstrains
  nrp = 0
  do ii = 1,nnebreplicatot
    if (nebreplicacfgptr(ii).eq.ncf) then
      if (.not.lfound) then
        nrfirst = ii
        lfound = .true.
      endif
!
!  Copy gradients to temporary array for output
!
      nrp = nrp + 1    
      cgradrep(1:6) = 0.0_dp
      gradrep(1:4,1:numat) = 0.0_dp
      do n = 1,nvar  
        if (iopt(n).gt.mvar) then
          i = iopt(n) - mvar
          gradrep(4,i) = gcrep(n,nrp+1)
        elseif (iopt(n).gt.nstrains) then
          nn = iopt(n) - nstrains
          i = (nn - 1)/3 + 1
          ix = nn - 3*(i - 1)
          gradrep(ix,i) = gcrep(n,nrp+1)
        else
          cgradrep(iopt(n)) = gcrep(n,nrp+1)
        endif
      enddo
!
      write(ioout,'(''********************************************************************************'')')
      write(ioout,'('' Replica number = '',i4)') ii - nrfirst + 1
      write(ioout,'(''********************************************************************************'')')
      if (ndim.eq.3) then
        write(ioout,'('' Cell : '',3(f10.5,1x),3(f8.4,1x))') (nebreplicacell(j,ii-nrfirst+1),j=1,6)
        write(ioout,'(''      : '',3(f10.5,1x),3(f8.4,1x))') (cgradrep(j),j=1,6)
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      elseif (ndim.eq.2) then
        write(ioout,'('' Cell : '',2(f10.5,1x),f8.4)') (nebreplicacell(j,ii-nrfirst+1),j=1,3)
        write(ioout,'(''      : '',2(f10.5,1x),f8.4)') (cgradrep(j),j=1,3)
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      elseif (ndim.eq.1) then
        write(ioout,'('' Cell : '',f10.5)') nebreplicacell(1,ii-nrfirst+1)
        write(ioout,'(''      : '',f10.5)') cgradrep(1)
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      write(ioout,'('' Coordinates : '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,nasym
        write(ioout,'(i6,1x,3(f12.6,1x),3(f10.5))')i,(nebreplicaxyz(j,i,ii-nrfirst+1),j=1,3),(gradrep(j,i),j=1,3)
      enddo
      if (nbsm.gt.0) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'('' Radii : '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        do i = 1,nasym
          write(ioout,'(i6,1x,f12.6,1x,f12.6)')i,nebreplicaradius(i,ii-nrfirst+1),gradrep(4,i)
        enddo
      endif
    endif
  enddo
  if (lfound) then
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Output replicas as coordinates suitable for GULP to read easily if edited out
!
  lfound = .false.
  write(ioout,'(''  Replica coordinates in GULP input format : '',/)')
  nrp = 0
  do ii = 1,nnebreplicatot
    if (nebreplicacfgptr(ii).eq.ncf) then
      if (.not.lfound) then
        nrfirst = ii
        lfound = .true.
      endif
      nrp = nrp + 1    
!
      write(ioout,'(''#-------------------------------------------------------------------------------'')')
      write(ioout,'(''#  Replica number = '',i4)') ii - nrfirst + 1
      write(ioout,'(''#-------------------------------------------------------------------------------'')')
      if (ndim.eq.3) then
        write(ioout,'(''cell'')')
        write(ioout,'(3(f10.5,1x),3(f8.4,1x))') (nebreplicacell(j,ii-nrfirst+1),j=1,6)
        write(ioout,'(''fractional'')')
      elseif (ndim.eq.2) then
        write(ioout,'(''scell'')')
        write(ioout,'(2(f10.5,1x),f8.4)') (nebreplicacell(j,ii-nrfirst+1),j=1,3)
        write(ioout,'(''sfractional'')')
      elseif (ndim.eq.1) then
        write(ioout,'(''pcell'')')
        write(ioout,'(f10.5)') nebreplicacell(1,ii-nrfirst+1)
        write(ioout,'(''pfractional'')')
      else
        write(ioout,'(''cartesian'')')
      endif
      do i = 1,nasym
        inat = iatn(i)
        if (inat.gt.maxele) then
          cstype = 'shel'
        else
          cstype = 'core'
        endif
        itype = natype(i)           
        call label(inat,itype,lab)
        write(ioout,'(a5,1x,a4,1x,3(f12.6,1x),1x,f8.5,1x,f6.4,1x,f8.6)')  &
          lab,cstype,(nebreplicaxyz(j,i,ii-nrfirst+1),j=1,3), &
          qa(i),occua(i),nebreplicaradius(i,ii-nrfirst+1)
      enddo
    endif
  enddo
  if (lfound) then
    write(ioout,'(''#-------------------------------------------------------------------------------'',/)')
  endif
!
!  Free local memory
!
  deallocate(gradrep,stat=status)
  if (status/=0) call deallocate_error('outneb','gradrep')
!
  return
  end
