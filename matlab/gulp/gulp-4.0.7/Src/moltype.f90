  subroutine moltype(linear)
!
!  Subroutine to work out whether a molecule is linear or not
!
!   8/97 Created 
!  10/02 Style updated
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
  use current
  use shell
  implicit none
!
!  Passed variables
!
  logical, intent(out) :: linear
!
!  Local variables
!
  integer(i4)          :: i
  real(dp)             :: dot
  real(dp)             :: rvmod
  real(dp)             :: vmod
  real(dp)             :: vx
  real(dp)             :: vy
  real(dp)             :: vz
  real(dp)             :: vx0
  real(dp)             :: vy0
  real(dp)             :: vz0
!
!  Initialise linear as true until proved otherwise
!
  linear = .true.
!
!  If there are only two atoms then must be linear
!
  if (ncore.le.2) return
!
!  Compare interatomic vectors until we find a pair which are not
!  parallel to each other
!
  vx0 = xclat(2) - xclat(1)
  vy0 = yclat(2) - yclat(1)
  vz0 = zclat(2) - zclat(1)
  vmod = vx0*vx0 + vy0*vy0 + vz0*vz0
  rvmod = 1.0_dp/sqrt(vmod)
  vx0 = rvmod*vx0
  vy0 = rvmod*vy0
  vz0 = rvmod*vz0
  do i = 3,numat
    vx = xclat(i) - xclat(1)
    vy = yclat(i) - yclat(1)
    vz = zclat(i) - zclat(1)
    vmod = vx*vx + vy*vy + vz*vz
    rvmod = 1.0_dp/sqrt(vmod)
    dot = vx0*vx + vy0*vy + vz0*vz
    dot = dot*rvmod
    if (abs(dot).lt.0.9999_dp) then
      linear = .false.
      return
    endif
  enddo
!
  return
  end
