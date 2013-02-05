  subroutine unfreeze(ltmp)
!
!  Unfreeze atoms within a spherical region.
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
!  Julian Gale, Curtin University, April 2005
!
  use current
  use freeze
  use symmetry
  implicit none
!
!  Passed variables
!
  logical, intent(out) :: ltmp(*)
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: indx
  integer(i4)      :: indy
  integer(i4)      :: indz
  integer(i4)      :: ii
  integer(i4)      :: jj
  integer(i4)      :: kk
  integer(i4)      :: max1
  integer(i4)      :: max1l
  integer(i4)      :: max2
  integer(i4)      :: max2l
  integer(i4)      :: max3
  integer(i4)      :: max3l
  integer(i4)      :: nri
  real(dp)         :: cut2
  real(dp)         :: r
  real(dp)         :: ruf
  real(dp)         :: xc
  real(dp)         :: yc
  real(dp)         :: zc
  real(dp)         :: xcd
  real(dp)         :: ycd
  real(dp)         :: zcd
  real(dp)         :: xcdi
  real(dp)         :: ycdi
  real(dp)         :: zcdi
  real(dp)         :: xcdj
  real(dp)         :: ycdj
  real(dp)         :: zcdj
  real(dp)         :: xcrd
  real(dp)         :: ycrd
  real(dp)         :: zcrd
  real(dp)         :: xf
  real(dp)         :: yf
  real(dp)         :: zf
!
!  Set up local variables
!
  ruf = rufree(ncf)
  cut2 = ruf*ruf
!
!  Generate maximum looping indices
!
  if (ndim.eq.3) then
    max1 = ruf/a + 1
    max1l = max1 + 1
    max2 = ruf/b + 1
    max2l = max2 + 1
    max3 = ruf/c + 1
    max3l = max3 + 1
  elseif (ndim.eq.2) then
    max1 = ruf/a + 1
    max1l = max1 + 1
    max2 = ruf/b + 1
    max2l = max2 + 1
    max3 = 0
    max3l = 0
  elseif (ndim.eq.1) then
    max1 = ruf/a + 1
    max1l = max1 + 1
    max2 = 0
    max2l = 0
    max3 = 0
    max3l = 0
  else
    max1 = 0
    max1l = 0
    max2 = 0
    max2l = 0
    max3 = 0
    max3l = 0
  endif
  if (iufree(ncf).eq.0) then
    if (ndim.eq.3) then
      xf = xufree(1,ncf)
      yf = xufree(2,ncf)
      zf = xufree(3,ncf)
      xc = xf*r1x+yf*r2x+zf*r3x
      yc = xf*r1y+yf*r2y+zf*r3y
      zc = xf*r1z+yf*r2z+zf*r3z
    elseif (ndim.eq.2) then
      xf = xufree(1,ncf)
      yf = xufree(2,ncf)
      zf = xufree(3,ncf)
      xc = xf*r1x+yf*r2x
      yc = xf*r1y+yf*r2y
      zc = zf
    elseif (ndim.eq.1) then
      xf = xufree(1,ncf)
      yf = xufree(2,ncf)
      zf = xufree(3,ncf)
      xc = xf*r1x
      yc = yf
      zc = zf
    else
      xc = xufree(1,ncf)
      yc = xufree(2,ncf)
      zc = xufree(3,ncf)
    endif
  else
    ii = iufree(ncf)
    xc = xalat(ii)
    yc = yalat(ii)
    zc = zalat(ii)
  endif
!
!  Loop over sites
!
  do i = 1,numat
    xcd = xclat(i)
    ycd = yclat(i)
    zcd = zclat(i)
    xcdi = xcd - xc - max1l*r1x
    ycdi = ycd - yc - max1l*r1y
    zcdi = zcd - zc - max1l*r1z
!
!  Loop over unit cells
!
    do ii = -max1,max1
      xcdi = xcdi + r1x
      ycdi = ycdi + r1y
      zcdi = zcdi + r1z
      xcdj = xcdi - max2l*r2x
      ycdj = ycdi - max2l*r2y
      zcdj = zcdi - max2l*r2z
      do jj = -max2,max2
        xcdj = xcdj + r2x
        ycdj = ycdj + r2y
        zcdj = zcdj + r2z
        xcrd = xcdj - max3l*r3x
        ycrd = ycdj - max3l*r3y
        zcrd = zcdj - max3l*r3z
        do kk = -max3,max3
          xcrd = xcrd + r3x
          ycrd = ycrd + r3y
          zcrd = zcrd + r3z
          r = xcrd**2 + ycrd**2 + zcrd**2
          if (r.le.cut2) then
!
!  Valid distance - set flags to optimise
!
            if (lsymopt) then
              nri = nrelat(i)
              indx = 3*(nri - 1) + nstrains + 1
            else
              indx = 3*(i - 1) + nstrains + 1
            endif
            indy = indx + 1
            indz = indy + 1
            ltmp(indx) = .true.
            ltmp(indy) = .true.
            ltmp(indz) = .true.
          endif
!
!  End of loops over cell vectors
!
        enddo
      enddo
    enddo
!
!  End of asymmetric atom loop
!
  enddo
!
  return
  end
