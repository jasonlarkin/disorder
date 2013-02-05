  subroutine outkpt
!
!  Outputs kpoints for a structure
!
!  nkpt = total number of k points across all structures
!  xkpt = fractional x component of k point
!  ykpt = fractional y component of k point
!  zkpt = fractional z component of k point
!  wkpt = weight of each k point
!
!   2/02 Normalisation of K points corrected for multiple configurations
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use configurations, only : ncfg
  use control
  use current
  use ksample
  use iochannels
  implicit none
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: nk
  integer(i4)                               :: status
  logical                                   :: lfout
  logical                                   :: lnongamma
  real(dp), dimension(:), allocatable       :: sumpercfg
!
  lfout = .true.
  nk = 0
  allocate(sumpercfg(ncfg),stat=status)
  if (status/=0) call outofmemory('outkpt','sumpercfg')
  sumpercfg(1:ncfg) = 0.0_dp
  do i = 1,nkpt
    sumpercfg(nkptcfg(i)) = sumpercfg(nkptcfg(i)) + wkpt(i)
  enddo
  do i = 1,ncfg
    if (sumpercfg(i).gt.0.0_dp) then
      sumpercfg(i) = 1.0_dp/sumpercfg(i)
    else
      sumpercfg(i) = 1.0_dp
    endif
  enddo
!
!  Normalise weights
!
  do i = 1,nkpt
    wkpt(i) = wkpt(i)*sumpercfg(nkptcfg(i))
  enddo
  deallocate(sumpercfg,stat=status)
  if (status/=0) call deallocate_error('outkpt','sumpercfg')
!
!  Loop over k points and output those relating to this configuration
!
  lnongamma = .false.
  do i = 1,nkpt
    if (nkptcfg(i).eq.ncf) then
      if (lfout) then
        lfout = .false.
        write(ioout,'(''  Brillouin zone sampling points :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''  Point number          x          y          z            Weight'')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      nk = nk + 1
      write(ioout,'(4x,i5,10x,3(f9.6,2x),4x,f9.4)') nk,xkpt(i),ykpt(i),zkpt(i),wkpt(i)
      if (abs(xkpt(i)).gt.1.0d-4) lnongamma = .true.
      if (abs(ykpt(i)).gt.1.0d-4) lnongamma = .true.
      if (abs(zkpt(i)).gt.1.0d-4) lnongamma = .true.
    endif
  enddo
  if (.not.lfout) then
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  If EEM/QEq then non-gamma point phonons are not allowed yet
!
  if (leem.and.lnongamma) then
    call outerror('Only gamma point phonons allowed for EEM/QEq',0_i4)
    call stopnow('outkpt')
  endif
!
  return
  end
