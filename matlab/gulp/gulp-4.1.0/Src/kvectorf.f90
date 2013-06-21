  subroutine kvector3Df(kvf)
!
!  Calculate the reciprocal lattice vectors multiplied by 2 pi
!  for a 3-D system. Based on the full centred cell.
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
!  Copyright Curtin University 2004
!
!  Julian Gale, Curtin University, December 2004
!
  use constants
  use current
  use iochannels
  use kspace
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  real(dp)    :: kvf(3,3)
!
!  Local variables
!
  real(dp)    :: rvf(3,3)
  real(dp)    :: kvx,kvy,kvz
  real(dp)    :: rvx,rvy,rvz
  real(dp)    :: volfull
  integer(i4) :: i,j
!
!  Copy primitive cell
!
  do i = 1,3
    do j = 1,3
      rvf(j,i) = rv(j,i)
    enddo
  enddo
!
!  Convert cell to full one
!
  call uncentre(rvf)
!
!  Generate reciprocal lattice vectors
!
  if (lra) then
    rvx = rvf(1,1)
    rvy = rvf(2,2)
    rvz = rvf(3,3)
    kvx = rvy*rvz
    kvy = rvx*rvz
    kvz = rvx*rvy
    volfull = rvx*kvx
    kvf(1,2) = 0.0_dp
    kvf(2,3) = 0.0_dp
    kvf(3,1) = 0.0_dp
    kvf(2,1) = 0.0_dp
    kvf(3,2) = 0.0_dp
    kvf(1,3) = 0.0_dp
  else
    kvf(1,1) = rvf(2,2)*rvf(3,3) - rvf(2,3)*rvf(3,2)
    kvf(2,1) = rvf(3,2)*rvf(1,3) - rvf(3,3)*rvf(1,2)
    kvf(3,1) = rvf(1,2)*rvf(2,3) - rvf(1,3)*rvf(2,2)
    kvf(1,2) = rvf(2,3)*rvf(3,1) - rvf(2,1)*rvf(3,3)
    kvf(2,2) = rvf(3,3)*rvf(1,1) - rvf(3,1)*rvf(1,3)
    kvf(3,2) = rvf(1,3)*rvf(2,1) - rvf(1,1)*rvf(2,3)
    kvf(1,3) = rvf(2,1)*rvf(3,2) - rvf(2,2)*rvf(3,1)
    kvf(2,3) = rvf(3,1)*rvf(1,2) - rvf(3,2)*rvf(1,1)
    kvf(3,3) = rvf(1,1)*rvf(2,2) - rvf(1,2)*rvf(2,1)
    volfull = kvf(1,1)*rvf(1,1) + kvf(2,1)*rvf(2,1) + kvf(3,1)*rvf(3,1)
  endif
  volfull = abs(volfull)
  if (volfull.lt.1.0d-8) then
    call outerror('unit cell volume is very close to zero',0_i4)
    if (ioproc) then
      write(ioout,'(''  Current lattice vectors : '')')
      do i = 1,3
        write(ioout,'(4x,3f12.6)')(rvf(j,i),j=1,3)
      enddo
    endif
    call stopnow('kvector3Df')
  endif
  volfull = 2.0_dp*pi/volfull
  if (lra) then
    kvf(1,1) = kvx*volfull
    kvf(2,2) = kvy*volfull
    kvf(3,3) = kvz*volfull
  else
    do i = 1,3
      kvf(1,i) = kvf(1,i)*volfull
      kvf(2,i) = kvf(2,i)*volfull
      kvf(3,i) = kvf(3,i)*volfull
    enddo
  endif
!
  return
  end
