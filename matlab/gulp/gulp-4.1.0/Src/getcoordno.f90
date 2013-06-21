  subroutine getcoordno(iat,cutoff,coordno)
!
!  Subroutine for calculating the coordination number of an atom
!
!  On entry:
!
!  iat      = atom number whose coordination number is to be found
!  cutoff   = cut-off distance for neighbour to be in coordination sphere
!
!  On exit:
!
!  coordno  = coordination number
!
!   7/10 Created from distance
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, July 2010
!
  use current
  use element, only : maxele
  use general
  use iochannels
  use species
  implicit none
!
!  Passed variables
!
  integer(i4),                 intent(in)     :: iat
  real(dp),                    intent(in)     :: cutoff
  real(dp),                    intent(out)    :: coordno
!
!  Local variables
!
  integer(i4)                                  :: ii
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: max1
  integer(i4)                                  :: max1l
  integer(i4)                                  :: max2
  integer(i4)                                  :: max2l
  integer(i4)                                  :: max3
  integer(i4)                                  :: max3l
  real(dp)                                     :: cut2
  real(dp)                                     :: r
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcdj
  real(dp)                                     :: ycdj
  real(dp)                                     :: zcdj
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
!  Zero return quantity
!
  coordno = 0.0_dp
!
!  Set up local variables
!
!  Generate maximum looping indices
!
  if (ndim.eq.3) then
    max1 = cutoff/a + 2
    max2 = cutoff/b + 2
    max3 = cutoff/c + 2
    max1l = max1 + 1
    max2l = max2 + 1
    max3l = max3 + 1
  elseif (ndim.eq.2) then
    max1 = cutoff/a + 2
    max2 = cutoff/b + 2
    max1l = max1 + 1
    max2l = max2 + 1
    max3 = 0
    max3l = 0
  elseif (ndim.eq.1) then
    max1 = cutoff/a + 2
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
  cut2 = cutoff**2
!
!  Set quantities associated with atom iat
!
  xcd = xalat(iat)
  ycd = yalat(iat)
  zcd = zalat(iat)
  do k = 1,numat
    xcdi = xclat(k) - xcd - max1l*r1x
    ycdi = yclat(k) - ycd - max1l*r1y
    zcdi = zclat(k) - zcd - max1l*r1z
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
          if (r.le.cut2.and.r.gt.0.0001_dp) then
!
!  Increment coordination number
!
            coordno = coordno + 1.0_dp
          endif
!
!  End of loops over cell vectors
!
        enddo
      enddo
    enddo
  enddo
!
  return
  end
