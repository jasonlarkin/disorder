!********
!  3-D  *
!********
  subroutine kvector3D
!
!  Calculate the reciprocal lattice vectors multiplied by 2 pi
!  for a 3-D system.
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
!  Julian Gale, Curtin University, February 2005
!
  use constants
  use current
  use iochannels
  use kspace
  use parallel
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4)     :: i
  integer(i4)     :: j
  real(dp)        :: kvx
  real(dp)        :: kvy
  real(dp)        :: kvz
  real(dp)        :: rvx
  real(dp)        :: rvy
  real(dp)        :: rvz
!
!  Generate reciprocal lattice vectors
!
  if (lra) then
    rvx = rv(1,1)
    rvy = rv(2,2)
    rvz = rv(3,3)
    kvx = rvy*rvz
    kvy = rvx*rvz
    kvz = rvx*rvy
    vol4pi = rvx*kvx
    kv(1,2) = 0.0_dp
    kv(2,3) = 0.0_dp
    kv(3,1) = 0.0_dp
    kv(2,1) = 0.0_dp
    kv(3,2) = 0.0_dp
    kv(1,3) = 0.0_dp
  else
    kv(1,1) = rv(2,2)*rv(3,3) - rv(2,3)*rv(3,2)
    kv(2,1) = rv(3,2)*rv(1,3) - rv(3,3)*rv(1,2)
    kv(3,1) = rv(1,2)*rv(2,3) - rv(1,3)*rv(2,2)
    kv(1,2) = rv(2,3)*rv(3,1) - rv(2,1)*rv(3,3)
    kv(2,2) = rv(3,3)*rv(1,1) - rv(3,1)*rv(1,3)
    kv(3,2) = rv(1,3)*rv(2,1) - rv(1,1)*rv(2,3)
    kv(1,3) = rv(2,1)*rv(3,2) - rv(2,2)*rv(3,1)
    kv(2,3) = rv(3,1)*rv(1,2) - rv(3,2)*rv(1,1)
    kv(3,3) = rv(1,1)*rv(2,2) - rv(1,2)*rv(2,1)
    vol4pi = kv(1,1)*rv(1,1) + kv(2,1)*rv(2,1) + kv(3,1)*rv(3,1)
  endif
  vol4pi = abs(vol4pi)
  if (vol4pi.lt.1.0d-8) then
    call outerror('unit cell volume is very close to zero',0_i4)
    if (ioproc) then
      write(ioout,'(''  Current lattice vectors : '')')
      do i = 1,3
        write(ioout,'(4x,3f12.6)') (rv(j,i),j=1,3)
      enddo
    endif
    call stopnow('kvector')
  endif
  vol4pi = 2.0_dp*pi/vol4pi
  if (lra) then
    kv(1,1) = kvx*vol4pi
    kv(2,2) = kvy*vol4pi
    kv(3,3) = kvz*vol4pi
  else
    do i = 1,3
      kv(1,i) = kv(1,i)*vol4pi
      kv(2,i) = kv(2,i)*vol4pi
      kv(3,i) = kv(3,i)*vol4pi
    enddo
  endif
  vol4pi = 2.0_dp*vol4pi
!
  return
  end
!********
!  2-D  *
!********
  subroutine kvector2D
!
!  Calculate the reciprocal surface vectors multiplied by 2 pi
!  for a 2-D system.
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
!  Julian Gale, Curtin University, January 2005
!
  use constants
  use current
  use iochannels
  use kspace
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4)     :: i
  integer(i4)     :: j
!
!  Create dummy unit vector for surface normal
!
  rv(3,1) = 0.0_dp
  rv(3,2) = 0.0_dp
  rv(1,3) = 0.0_dp
  rv(2,3) = 0.0_dp
  rv(3,3) = 1.0_dp
!
!  Generate reciprocal surface vectors
!
  kv(1,1) = rv(2,2)*rv(3,3) - rv(2,3)*rv(3,2)
  kv(2,1) = rv(3,2)*rv(1,3) - rv(3,3)*rv(1,2)
  kv(3,1) = rv(1,2)*rv(2,3) - rv(1,3)*rv(2,2)
  kv(1,2) = rv(2,3)*rv(3,1) - rv(2,1)*rv(3,3)
  kv(2,2) = rv(3,3)*rv(1,1) - rv(3,1)*rv(1,3)
  kv(3,2) = rv(1,3)*rv(2,1) - rv(1,1)*rv(2,3)
  kv(1,3) = rv(2,1)*rv(3,2) - rv(2,2)*rv(3,1)
  kv(2,3) = rv(3,1)*rv(1,2) - rv(3,2)*rv(1,1)
  kv(3,3) = rv(1,1)*rv(2,2) - rv(1,2)*rv(2,1)
  vol4pi  = kv(1,1)*rv(1,1) + kv(2,1)*rv(2,1) + kv(3,1)*rv(3,1)
  vol4pi  = abs(vol4pi)
  if (vol4pi.lt.1.0d-8) then
    call outerror('surface area is very close to zero',0_i4)
    if (ioproc) then
      write(ioout,'(''  Current surface vectors : '')')
      do i = 1,2
        write(ioout,'(4x,3f12.6)')(rv(j,i),j=1,2)
      enddo
    endif
    call stopnow('kvector')
  endif
  vol4pi = 2.0_dp*pi/vol4pi
  do i=1,2
    kv(1,i) = kv(1,i)*vol4pi
    kv(2,i) = kv(2,i)*vol4pi
  enddo
  kv(3,3) = 1.0_dp
!
  return
  end
!********
!  1-D  *
!********
  subroutine kvector1D
!
!  Calculate the reciprocal polymer vector multiplied by 2 pi
!  for a 1-D system.
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
!  Julian Gale, Curtin University, January 2005
!
  use constants
  use current
  use kspace
  use parallel
  implicit none
!
!  Generate reciprocal polymer vector
!
  kv(1,1) = 2.0_dp*pi/rv(1,1)
  vol4pi = 0.0_dp
!
  return
  end
