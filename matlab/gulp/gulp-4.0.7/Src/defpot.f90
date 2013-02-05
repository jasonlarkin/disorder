  subroutine defpot
!
!  Subroutine for calculating site potential for region 1
!
!  11/08 Array references for defect atomic numbers and types updated
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, October 2008
!
  use control
  use current
  use defects
  use derivatives
  use element, only : maxele
  use iochannels
  use parallel
  implicit none
!
!  Local variables
!
  character(len=2)                    :: cstype
  character(len=5)                    :: lab
  integer(i4)                         :: i
  integer(i4)                         :: ii
  integer(i4)                         :: inat
  integer(i4)                         :: itype
  integer(i4)                         :: nloop
  integer(i4)                         :: status
  logical                             :: ldpotsym
  real(dp), dimension(:), allocatable :: v
!
  ldpotsym = (ldsym.and.ld1sym.and.index(keyword,'nodp').eq.0)
  allocate(v(ndasym),stat=status)
  if (status/=0) call outofmemory('defpot','v')
!
!  Zero arrays
!
  do i = 1,ndasym
    v(i) = 0.0_dp
    xdrv(i) = 0.0_dp
    ydrv(i) = 0.0_dp
    zdrv(i) = 0.0_dp
  enddo
!
!  Reciprocal space component of region 1 - region 2a potential
!
  call recip12apot(v,xdrv,ydrv,zdrv,ldpotsym)
!
!  Real space component of region 1 - region 2a potential
!
  call real12apot(v,xdrv,ydrv,zdrv,ldpotsym)
!
!  Correct for defective region 1 - perfect region 1 potential
!
  call real11pot(v,xdrv,ydrv,zdrv,3_i4,ldpotsym)
!
!  Region 1 - region 1 potential
!
  call real11pot(v,xdrv,ydrv,zdrv,1_i4,ldpotsym)
!
!  Calculate potential change due to region 2a
!
!      call real2apot(v,xdrv,ydrv,zdrv)
!
!  Output electrostatic potential and electric field
!
  if (ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''  Electrostatic site potentials for region 1 :'',/)')
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    write(ioout,'('' Site  Atomic      Potential                Derivatives (V/Angs)'')')
    write(ioout,'('' No.   Label          (V)                 x           y           z'')')
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    if (ldpotsym) then 
      nloop = ndasym
    else
      nloop = nreg1
    endif
    do i = 1,nloop
      if (ldpotsym) then
        ii = ndsptr(i)
      else
        ii = i
      endif
      inat = natdefe(ii)
      itype = ntypdefe(ii)
      call label(inat,itype,lab)
      if (ldefbsmat(ii)) then
        cstype = 'bc'
        if (inat.gt.maxele) cstype = 'bs'
      else
        cstype = 'c '
        if (inat.gt.maxele) cstype = 's '
      endif
      write(ioout,'(1x,i4,2x,a5,1x,a2,f13.6,7x,3f12.6)') i,lab,cstype,v(i),xdrv(i),ydrv(i),zdrv(i)
    enddo
    write(ioout,'(''-------------------------------------------------------------------------------'',/)')
  endif
!
  deallocate(v,stat=status)
  if (status/=0) call deallocate_error('defpot','v')
!
  return
  end
