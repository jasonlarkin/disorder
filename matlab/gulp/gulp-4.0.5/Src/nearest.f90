  subroutine nearesti(xdc,ydc,zdc,xv,yv,zv,r1x,r1y,r1z,r2x,r2y,r2z,r3x,r3y,r3z,ix,iy,iz)
!
!  Find nearest image of xv,yv,zv to xdc,ydc,zdc
!
!   6/98 Name changed from nearest to nearesti to prevent
!        warning message about conflict under Linux
!   7/03 Style updated
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
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4) :: ix
  integer(i4) :: iy
  integer(i4) :: iz
  real(dp)    :: r1x
  real(dp)    :: r1y
  real(dp)    :: r1z
  real(dp)    :: r2x
  real(dp)    :: r2y
  real(dp)    :: r2z
  real(dp)    :: r3x
  real(dp)    :: r3y
  real(dp)    :: r3z
  real(dp)    :: xdc
  real(dp)    :: ydc
  real(dp)    :: zdc
  real(dp)    :: xv
  real(dp)    :: yv
  real(dp)    :: zv
!
!  Local variables
!
  integer(i4) :: ii
  integer(i4) :: jj
  integer(i4) :: kk
  real(dp)    :: r
  real(dp)    :: rmin
  real(dp)    :: xcdi
  real(dp)    :: ycdi
  real(dp)    :: zcdi
  real(dp)    :: xcdj
  real(dp)    :: ycdj
  real(dp)    :: zcdj
  real(dp)    :: xcrd
  real(dp)    :: ycrd
  real(dp)    :: zcrd
!
  rmin = 10000.0_dp
  xcdi = xv - xdc - 2.0_dp*r1x
  ycdi = yv - ydc - 2.0_dp*r1y
  zcdi = zv - zdc - 2.0_dp*r1z
!
!  Loop over unit cells
!
  do ii = - 1,1
    xcdi = xcdi + r1x
    ycdi = ycdi + r1y
    zcdi = zcdi + r1z
    xcdj = xcdi - 2.0_dp*r2x
    ycdj = ycdi - 2.0_dp*r2y
    zcdj = zcdi - 2.0_dp*r2z
    do jj = - 1,1
      xcdj = xcdj + r2x
      ycdj = ycdj + r2y
      zcdj = zcdj + r2z
      xcrd = xcdj - 2.0_dp*r3x
      ycrd = ycdj - 2.0_dp*r3y
      zcrd = zcdj - 2.0_dp*r3z
      do kk = - 1,1
        xcrd = xcrd + r3x
        ycrd = ycrd + r3y
        zcrd = zcrd + r3z
        r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
        if (r.le.rmin) then
          rmin = r
          xv = xcrd + xdc
          yv = ycrd + ydc
          zv = zcrd + zdc
          ix = ii
          iy = jj
          iz = kk
        endif
      enddo
    enddo
  enddo
!
  return
  end
!
  subroutine nearestr(ndim,xdc,ydc,zdc,rv,rmin)
!
!  Find distance to nearest image of vector xdc,ydc,zdc
!
!   4/08 Created from nearesti
!   9/11 ndim added to the argument list
!   9/11 Modified to handle different dimensionalities
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, September 2011
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: ndim
  real(dp),    intent(in)    :: rv(3,3)
  real(dp),    intent(inout) :: xdc
  real(dp),    intent(inout) :: ydc
  real(dp),    intent(inout) :: zdc
  real(dp),    intent(out)   :: rmin
!
!  Local variables
!
  integer(i4)                :: ii
  integer(i4)                :: jj
  integer(i4)                :: kk
  real(dp)                   :: r
  real(dp)                   :: xcdi
  real(dp)                   :: ycdi
  real(dp)                   :: zcdi
  real(dp)                   :: xcdj
  real(dp)                   :: ycdj
  real(dp)                   :: zcdj
  real(dp)                   :: xcrd
  real(dp)                   :: ycrd
  real(dp)                   :: zcrd
!
  if (ndim.eq.3) then
    rmin = 10000.0_dp
    xcdi = xdc - 2.0_dp*rv(1,1)
    ycdi = ydc - 2.0_dp*rv(2,1)
    zcdi = zdc - 2.0_dp*rv(3,1)
!
!  Loop over unit cells
!
    do ii = - 1,1
      xcdi = xcdi + rv(1,1)
      ycdi = ycdi + rv(2,1)
      zcdi = zcdi + rv(3,1)
      xcdj = xcdi - 2.0_dp*rv(1,2)
      ycdj = ycdi - 2.0_dp*rv(2,2)
      zcdj = zcdi - 2.0_dp*rv(3,2)
      do jj = - 1,1
        xcdj = xcdj + rv(1,2)
        ycdj = ycdj + rv(2,2)
        zcdj = zcdj + rv(3,2)
        xcrd = xcdj - 2.0_dp*rv(1,3)
        ycrd = ycdj - 2.0_dp*rv(2,3)
        zcrd = zcdj - 2.0_dp*rv(3,3)
        do kk = - 1,1
          xcrd = xcrd + rv(1,3)
          ycrd = ycrd + rv(2,3)
          zcrd = zcrd + rv(3,3)
          r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
          if (r.le.rmin) then
            rmin = r
            xdc = xcrd
            ydc = ycrd
            zdc = zcrd
          endif
        enddo
      enddo
    enddo
  elseif (ndim.eq.2) then
    rmin = 10000.0_dp
    xcdi = xdc - 2.0_dp*rv(1,1)
    ycdi = ydc - 2.0_dp*rv(2,1)
    zcrd = zdc 
!
!  Loop over unit cells
!
    do ii = - 1,1
      xcdi = xcdi + rv(1,1)
      ycdi = ycdi + rv(2,1)
      xcrd = xcdi - 2.0_dp*rv(1,2)
      ycrd = ycdi - 2.0_dp*rv(2,2)
      do jj = - 1,1
        xcrd = xcrd + rv(1,2)
        ycrd = ycrd + rv(2,2)
        r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
        if (r.le.rmin) then
          rmin = r
          xdc = xcrd
          ydc = ycrd
        endif
      enddo
    enddo
  elseif (ndim.eq.1) then
    rmin = 10000.0_dp
    xcrd = xdc - 2.0_dp*rv(1,1)
    ycrd = ydc
    zcrd = zdc
!
!  Loop over unit cells
!
    do ii = - 1,1
      xcrd = xcrd + rv(1,1)
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      if (r.le.rmin) then
        rmin = r
        xdc = xcrd
      endif
    enddo
  elseif (ndim.eq.0) then
    rmin = 10000.0_dp
    rmin = xdc*xdc + ydc*ydc + zdc*zdc
  endif
!
  rmin = sqrt(rmin)
!
  return
  end
