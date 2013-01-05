  subroutine potential
!
!  Subroutine for evaluating electrostatic potential at a
!  series of general points. Used in electrostatic potential
!  surface fitting.
!
!   7/00 referencing of xsite,ysite,zsite shift back by npotpt0
!   7/00 arrays filled for 0-D case as well
!   8/01 Call to epot0/3 replaced with generic call to epot
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
  use control
  use current
  use potentialpoints
  use times
  implicit none
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: nsite
  integer(i4)                               :: status
  real(dp)                                  :: cputime
  real(dp)                                  :: efg
  real(dp)                                  :: time1
  real(dp)                                  :: time2
  real(dp)                                  :: vx
  real(dp)                                  :: vy
  real(dp)                                  :: vz
  real(dp), dimension(:), allocatable       :: xsite
  real(dp), dimension(:), allocatable       :: ysite
  real(dp), dimension(:), allocatable       :: zsite
!
  time1 = cputime()
!
!  Set pointer to points
!
  npotpt0 = 0
  do i = 1,ncf - 1
    npotpt0 = npotpt0 + npotptcfg(i)
  enddo
  nsite = npotptcfg(ncf)
!
!  Allocate local memory
!
  allocate(xsite(nsite),stat=status)
  if (status/=0) call outofmemory('potential','xsite')
  allocate(ysite(nsite),stat=status)
  if (status/=0) call outofmemory('potential','ysite')
  allocate(zsite(nsite),stat=status)
  if (status/=0) call outofmemory('potential','zsite')
!
!  Generate cartesian coordinates
!
  if (ndim.eq.3) then
!**************
!  3D system  *
!**************
    do i = npotpt0 + 1,npotpt0 + nsite
      xsite(i-npotpt0) = xpotpt(i)*r1x + ypotpt(i)*r2x + zpotpt(i)*r3x
      ysite(i-npotpt0) = xpotpt(i)*r1y + ypotpt(i)*r2y + zpotpt(i)*r3y
      zsite(i-npotpt0) = xpotpt(i)*r1z + ypotpt(i)*r2z + zpotpt(i)*r3z
    enddo
  elseif (ndim.eq.2) then
!**************
!  2D system  *
!**************
    do i = npotpt0 + 1,npotpt0 + nsite
      xsite(i-npotpt0) = xpotpt(i)*r1x + ypotpt(i)*r2x
      ysite(i-npotpt0) = xpotpt(i)*r1y + ypotpt(i)*r2y
      zsite(i-npotpt0) = zpotpt(i)
    enddo
  elseif (ndim.eq.1) then
!**************
!  1D system  *
!**************
    do i = npotpt0 + 1,npotpt0 + nsite
      xsite(i-npotpt0) = xpotpt(i)*r1x
      ysite(i-npotpt0) = ypotpt(i)
      zsite(i-npotpt0) = zpotpt(i)
    enddo
  elseif (ndim.eq.0) then
!**************
!  0D system  *
!**************
    do i = npotpt0 + 1,npotpt0 + nsite
      xsite(i-npotpt0) = xpotpt(i)
      ysite(i-npotpt0) = ypotpt(i)
      zsite(i-npotpt0) = zpotpt(i)
    enddo
  endif
!************************
!  Calculate potential  *
!************************
  call epot(.true.,nsite,vpotpt(npotpt0+1),xsite,ysite,zsite,.false.,vx,vy,vz,.false.,efg,.false.)
!
!  Free local memory
!
  deallocate(zsite,stat=status)
  if (status/=0) call deallocate_error('potential','zsite')
  deallocate(ysite,stat=status)
  if (status/=0) call deallocate_error('potential','ysite')
  deallocate(xsite,stat=status)
  if (status/=0) call deallocate_error('potential','xsite')
!
!  Timing
!
  time2 = cputime()
  tion = tion + time2 - time1
!
  return
  end
