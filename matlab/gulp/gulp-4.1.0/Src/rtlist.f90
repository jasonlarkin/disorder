  subroutine rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmiddle,maxvector)
!
!  Store linear array of lattice vectors 
!  Used in - angle, three, cosmo routines
!
!  cut       = cutoff distance for radial search
!  nvector   = no. of valid vectors found, on return
!  xvec      = x components of vectors
!  yvec      = y components of vectors
!  zvec      = z components of vectors
!  nmiddle   = pointer to vector when components are all zero
!  maxvector = dimension of xvec,yvec,zvec
!
!   7/00 maxvector added to argument list to trap too many vectors
!  12/00 modifications for 1-D and 2-D cases added
!   8/01 nmiddle added
!   3/03 Style updated and nadd increased for small angles
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
  use general
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)     :: maxvector
  integer(i4), intent(out)    :: imax
  integer(i4), intent(out)    :: jmax
  integer(i4), intent(out)    :: kmax
  integer(i4), intent(out)    :: nmiddle
  integer(i4), intent(out)    :: nvector
  real(dp),    intent(in)     :: cut
  real(dp),    intent(out)    :: xvec(maxvector)
  real(dp),    intent(out)    :: yvec(maxvector)
  real(dp),    intent(out)    :: zvec(maxvector)
!
!  Local variables
!
  integer(i4) :: ii
  integer(i4) :: imin
  integer(i4) :: jj
  integer(i4) :: jmin
  integer(i4) :: kk
  integer(i4) :: kmin
  logical     :: lnadd
  real(dp)    :: ra
  real(dp)    :: rb
  real(dp)    :: rc
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
  lnadd = .true.
  if (ndim.eq.3) then
!
!  3-D
!
    if (nadd.eq.0) then
      lnadd = .false.
      if (lra) then
        nadd = 1
      else
        if (alpha.lt.30.0d0.or.beta.lt.30.0d0.or.gamma.lt.30.0d0) then
          nadd = 5
        elseif (alpha.gt.150.0d0.or.beta.gt.150.0d0.or.gamma.gt.150.0d0) then
          nadd = 5
        elseif (alpha.lt.50.0d0.or.beta.lt.50.0d0.or.gamma.lt.50.0d0) then
          nadd = 4
        elseif (alpha.gt.130.0d0.or.beta.gt.130.0d0.or.gamma.gt.130.0d0) then
          nadd = 4
        elseif (alpha.lt.70.0d0.or.beta.lt.70.0d0.or.gamma.lt.70.0d0) then
          nadd = 3
        elseif (alpha.gt.110.0d0.or.beta.gt.110.0d0.or.gamma.gt.110.0d0) then
          nadd = 3
        else
          nadd = 2
        endif
      endif
    endif
    ra = 1.0_dp/a
    rb = 1.0_dp/b
    rc = 1.0_dp/c
    imax = ra*cut + nadd
    jmax = rb*cut + nadd
    kmax = rc*cut + nadd
  elseif (ndim.eq.2) then
!
!  2-D
!
    if (nadd.eq.0) then
      lnadd = .false.
      if (lra) then
        nadd = 1
      else
        if (alpha.lt.30.0d0.or.alpha.gt.150.0d0) then
          nadd = 4   
        elseif (alpha.lt.50.0d0.or.alpha.gt.130.0d0) then
          nadd = 3
        elseif (alpha.lt.70.0d0.or.alpha.gt.110.0d0) then
          nadd = 2
        else
          nadd = 1
        endif
      endif
    endif
    ra = 1.0_dp/a
    rb = 1.0_dp/b
    imax = ra*cut + nadd
    jmax = rb*cut + nadd
    kmax = 0
  elseif (ndim.eq.1) then
!
!  1-D
!
    if (nadd.eq.0) then
      lnadd = .false.
      nadd = 1
    endif
    ra = 1.0_dp/a
    imax = ra*cut + nadd
    jmax = 0
    kmax = 0
  elseif (ndim.eq.0) then
!
!  0-D
!
    imax = 0
    jmax = 0
    kmax = 0
  endif
  imin = - imax
  jmin = - jmax
  kmin = - kmax
  xcdi = - r1x*(imax + 1)
  ycdi = - r1y*(imax + 1)
  zcdi = - r1z*(imax + 1)
  nvector = 0
!
!  Loop over unit cells
!
  do ii = imin,imax
    xcdi = xcdi + r1x
    ycdi = ycdi + r1y
    zcdi = zcdi + r1z
    xcdj = xcdi - r2x*(jmax + 1)
    ycdj = ycdi - r2y*(jmax + 1)
    zcdj = zcdi - r2z*(jmax + 1)
    do jj = jmin,jmax
      xcdj = xcdj+r2x
      ycdj = ycdj+r2y
      zcdj = zcdj+r2z
      xcrd = xcdj - r3x*(kmax + 1)
      ycrd = ycdj - r3y*(kmax + 1)
      zcrd = zcdj - r3z*(kmax + 1)
      do kk = kmin,kmax
        nvector = nvector + 1
        xcrd = xcrd + r3x
        ycrd = ycrd + r3y
        zcrd = zcrd + r3z
        if (ii.eq.0.and.jj.eq.0.and.kk.eq.0) nmiddle = nvector
        if (nvector.le.maxvector) then
          xvec(nvector) = xcrd
          yvec(nvector) = ycrd
          zvec(nvector) = zcrd
        endif
      enddo
    enddo
  enddo
  if (.not.lnadd) nadd = 0
!
  return
  end
