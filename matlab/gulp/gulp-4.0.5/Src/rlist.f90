  subroutine rlist
!
!  Store linear array of lattice vectors for ii/jj/kk=-1,1 
!  => 27 lattice vectors. This tidies up and speeds up the
!  loops over these indices.
!  Used in - bond, angle, three, four, etc
!
!   7/00 Now extended to take care of 0-D case as well
!   4/08 Setup of extended 5 x 5 x 5 cell case added
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
!  Julian Gale, NRI, Curtin University, June 2005
!
  use current
  implicit none
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: jj
  integer(i4)        :: kk
  real(dp)           :: xcdi
  real(dp)           :: ycdi
  real(dp)           :: zcdi
  real(dp)           :: xcdj
  real(dp)           :: ycdj
  real(dp)           :: zcdj
  real(dp)           :: xcrd
  real(dp)           :: ycrd
  real(dp)           :: zcrd
!
  if (ndim.eq.3) then
!*************
!  3-D case  *
!*************
    imaxl = 1_i4
    jmaxl = 1_i4
    kmaxl = 1_i4
    xcdi = -2.0_dp*r1x
    ycdi = -2.0_dp*r1y
    zcdi = -2.0_dp*r1z
    iimax = 0
!
!  Loop over unit cells
!
    do ii = -1,1
      xcdi = xcdi + r1x
      ycdi = ycdi + r1y
      zcdi = zcdi + r1z
      xcdj = xcdi - 2.0_dp*r2x
      ycdj = ycdi - 2.0_dp*r2y
      zcdj = zcdi - 2.0_dp*r2z
      do jj = -1,1
        xcdj = xcdj + r2x
        ycdj = ycdj + r2y
        zcdj = zcdj + r2z
        xcrd = xcdj - 2.0_dp*r3x
        ycrd = ycdj - 2.0_dp*r3y
        zcrd = zcdj - 2.0_dp*r3z
        do kk = -1,1
          iimax = iimax + 1
          xcrd = xcrd + r3x
          ycrd = ycrd + r3y
          zcrd = zcrd + r3z
          xvec1cell(iimax) = xcrd
          yvec1cell(iimax) = ycrd
          zvec1cell(iimax) = zcrd
        enddo
      enddo
    enddo
    iimid = 14
  elseif (ndim.eq.2) then
!*************
!  2-D case  *
!*************
    imaxl = 1_i4
    jmaxl = 1_i4
    kmaxl = 0_i4
    xcdi = -2.0_dp*r1x
    ycdi = -2.0_dp*r1y
    iimax = 0
!
!  Loop over unit cells
!
    do ii = -1,1
      xcdi = xcdi + r1x
      ycdi = ycdi + r1y
      xcrd = xcdi - 2.0_dp*r2x
      ycrd = ycdi - 2.0_dp*r2y
      do jj = -1,1
        xcrd = xcrd + r2x
        ycrd = ycrd + r2y
        iimax = iimax + 1
        xvec1cell(iimax) = xcrd
        yvec1cell(iimax) = ycrd
        zvec1cell(iimax) = 0.0_dp
      enddo
    enddo
    iimid = 5
  elseif (ndim.eq.1) then
!*************
!  1-D case  *
!*************
    imaxl = 1_i4
    jmaxl = 0_i4
    kmaxl = 0_i4
    xcrd = -2.0_dp*r1x
    iimax = 0
!
!  Loop over unit cells
!
    do ii = -1,1
      xcrd = xcrd + r1x
      iimax = iimax + 1
      xvec1cell(iimax) = xcrd
      yvec1cell(iimax) = 0.0_dp
      zvec1cell(iimax) = 0.0_dp
    enddo
    iimid = 2
  else
!*************
!  0-D case  *
!*************
    imaxl = 0_i4
    jmaxl = 0_i4
    kmaxl = 0_i4
    iimax = 1
    iimid = 1
    xvec1cell(1) = 0.0_dp
    yvec1cell(1) = 0.0_dp
    zvec1cell(1) = 0.0_dp
  endif
!
!  Setup extended cell list
!
  call rlist2
!
  return
  end
!
  subroutine rlist2
!
!  Store linear array of lattice vectors for ii/jj/kk=-2,2 
!  => 125 lattice vectors. This tidies up and speeds up the
!  loops over these indices.
!
!   4/08 Created from rlist
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
!  Julian Gale, NRI, Curtin University, April 2008
!
  use current
  implicit none
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: jj
  integer(i4)        :: kk
  real(dp)           :: xcdi
  real(dp)           :: ycdi
  real(dp)           :: zcdi
  real(dp)           :: xcdj
  real(dp)           :: ycdj
  real(dp)           :: zcdj
  real(dp)           :: xcrd
  real(dp)           :: ycrd
  real(dp)           :: zcrd
!
  if (ndim.eq.3) then
!*************
!  3-D case  *
!*************
    imaxl2 = 2_i4
    jmaxl2 = 2_i4
    kmaxl2 = 2_i4
    xcdi = -3.0_dp*r1x
    ycdi = -3.0_dp*r1y
    zcdi = -3.0_dp*r1z
    iimax2 = 0
!
!  Loop over unit cells
!
    do ii = -2,2
      xcdi = xcdi + r1x
      ycdi = ycdi + r1y
      zcdi = zcdi + r1z
      xcdj = xcdi - 3.0_dp*r2x
      ycdj = ycdi - 3.0_dp*r2y
      zcdj = zcdi - 3.0_dp*r2z
      do jj = -2,2
        xcdj = xcdj + r2x
        ycdj = ycdj + r2y
        zcdj = zcdj + r2z
        xcrd = xcdj - 3.0_dp*r3x
        ycrd = ycdj - 3.0_dp*r3y
        zcrd = zcdj - 3.0_dp*r3z
        do kk = -2,2
          iimax2 = iimax2 + 1
          xcrd = xcrd + r3x
          ycrd = ycrd + r3y
          zcrd = zcrd + r3z
          xvec2cell(iimax2) = xcrd
          yvec2cell(iimax2) = ycrd
          zvec2cell(iimax2) = zcrd
        enddo
      enddo
    enddo
    iimid2 = 63
  elseif (ndim.eq.2) then
!*************
!  2-D case  *
!*************
    imaxl2 = 2_i4
    jmaxl2 = 2_i4
    kmaxl2 = 0_i4
    xcdi = -3.0_dp*r1x
    ycdi = -3.0_dp*r1y
    iimax2 = 0
!
!  Loop over unit cells
!
    do ii = -2,2
      xcdi = xcdi + r1x
      ycdi = ycdi + r1y
      xcrd = xcdi - 3.0_dp*r2x
      ycrd = ycdi - 3.0_dp*r2y
      do jj = -2,2
        xcrd = xcrd + r2x
        ycrd = ycrd + r2y
        iimax2 = iimax2 + 1
        xvec2cell(iimax2) = xcrd
        yvec2cell(iimax2) = ycrd
        zvec2cell(iimax2) = 0.0_dp
      enddo
    enddo
    iimid2 = 13
  elseif (ndim.eq.1) then
!*************
!  1-D case  *
!*************
    imaxl2 = 2_i4
    jmaxl2 = 0_i4
    kmaxl2 = 0_i4
    xcrd = -3.0_dp*r1x
    iimax2 = 0
!
!  Loop over unit cells
!
    do ii = -2,2
      xcrd = xcrd + r1x
      iimax2 = iimax2 + 1
      xvec2cell(iimax2) = xcrd
      yvec2cell(iimax2) = 0.0_dp
      zvec2cell(iimax2) = 0.0_dp
    enddo
    iimid2 = 3
  else
!*************
!  0-D case  *
!*************
    imaxl2 = 0_i4
    jmaxl2 = 0_i4
    kmaxl2 = 0_i4
    iimax2 = 1
    iimid2 = 1
    xvec2cell(1) = 0.0_dp
    yvec2cell(1) = 0.0_dp
    zvec2cell(1) = 0.0_dp
  endif
!
  return
  end
