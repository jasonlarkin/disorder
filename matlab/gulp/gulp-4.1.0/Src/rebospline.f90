  subroutine BicubicSpline(GridData,Coeff,lCoeffDone,nxmax,nymax,x,y,Values,lgrad1,lgrad2)
!
!  Subroutine for calculating the bicubic spline evaluation of a function
!  and optionally derivatives. Note that it is assumed that the knots fall
!  at the integer values, as is the case for the REBO model.
!
!  On entry:
!
!  GridData(0:3,0:nxmax,0:nymax) = data on grid of points
!                                  1 = function, F
!                                  2 = dF/dx
!                                  3 = dF/dy
!                                  4 = d2F/dxdy
!  nxmax                         = maximum number of points in the x direction
!  nymax                         = maximum number of points in the y direction
!  x                             = x position at which function is to be evaluated
!  y                             = y position at which function is to be evaluated
!  lgrad1                        = if .true. then first derivatives are to be evaluated
!  lgrad2                        = if .true. then second derivatives are to be evaluated
!
!  On exit:
!
!  Values(6)                     = return values
!                                  1 = function
!                                  2 = dF/dx
!                                  3 = dF/dy
!                                  4 = d2F/dx2
!                                  5 = d2F/dxdy
!                                  6 = d2F/dy2
!
!  Both:
!
!  lCoeffDone(0:nxmax,0:nymax)   = logical indicating whether spline coefficients are
!                                  already precomputed
!  Coeff(4,4,0:nxmax,0:nymax))   = spline coefficients - potentially precomputed
!
!  11/04 Storage of pre-computed coefficients for re-use added
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4),   intent(in)     :: nxmax
  integer(i4),   intent(in)     :: nymax
  logical,       intent(inout)  :: lCoeffDone(0:nxmax,0:nymax)
  logical,       intent(in)     :: lgrad1
  logical,       intent(in)     :: lgrad2
  real(dp),      intent(inout)  :: Coeff(4,4,0:nxmax,0:nymax)
  real(dp),      intent(in)     :: GridData(0:3,0:nxmax,0:nymax)
  real(dp),      intent(in)     :: x
  real(dp),      intent(in)     :: y
  real(dp),      intent(out)    :: Values(6)
!
!  Local variables
!
  integer(i4)                   :: i
  integer(i4)                   :: ix
  integer(i4)                   :: iy
  integer(i4)                   :: j
  real(dp)                      :: dx
  real(dp)                      :: dxp
  real(dp)                      :: dxpm1
  real(dp)                      :: dxpm2
  real(dp)                      :: dy
  real(dp)                      :: dyp
  real(dp)                      :: dypm1
  real(dp)                      :: dypm2
  real(dp)                      :: xloc
  real(dp)                      :: yloc
!
!  Ensure that current point lies within grid
!
  xloc = min(x,dble(nxmax))
  xloc = max(x,0.0_dp)
  yloc = min(y,dble(nymax))
  yloc = max(y,0.0_dp)
!
!  Find grid points on low side of current point
!
  ix = xloc
  iy = yloc
!
!  Compute distance to lower grid point
!
  dx = xloc - dble(ix)
  dy = yloc - dble(iy)
!
!  If ix & iy are at the maximum or minimum values set values
!
  if (dx.lt.1.0d-12.and.dy.lt.1.0d-12.and.(ix.eq.0.or.ix.eq.nxmax).and.(iy.eq.0.or.iy.eq.nymax)) then
    Values(1) = GridData(0,ix,iy)
    if (lgrad1) then
      Values(2) = GridData(1,ix,iy)
      Values(3) = GridData(2,ix,iy)
      if (lgrad2) then
        Values(4) = 0.0_dp
        Values(5) = GridData(3,ix,iy)
        Values(6) = 0.0_dp
      endif
    endif
  else
    if (.not.lCoeffDone(ix,iy)) then
!
!  Set up spline coefficients
!
      lCoeffDone(ix,iy) = .true.
      call BicubicCoeff(GridData,nxmax,nymax,ix,iy,Coeff(1,1,ix,iy))
    endif
!
!  Compute function and derivatives for current point
!
    Values(1) = 0.0_dp
    if (lgrad1) then
      Values(2:3) = 0.0_dp
      if (lgrad2) then
        Values(4:6) = 0.0_dp
      endif
    endif
    dxp = 1.0_dp
    dxpm1 = 1.0_dp
    dxpm2 = 1.0_dp
    do i = 1,4
      dyp = 1.0_dp
      dypm1 = 1.0_dp
      dypm2 = 1.0_dp
      do j = 1,4
        Values(1) = Values(1) + Coeff(j,i,ix,iy)*dxp*dyp
        if (lgrad1) then
          Values(2) = Values(2) + dble(i-1)*Coeff(j,i,ix,iy)*dxpm1*dyp
          Values(3) = Values(3) + dble(j-1)*Coeff(j,i,ix,iy)*dxp*dypm1
          if (lgrad2) then
            Values(4) = Values(4) + dble((i-1)*(i-2))*Coeff(j,i,ix,iy)*dxpm2*dyp
            Values(5) = Values(5) + dble((i-1)*(j-1))*Coeff(j,i,ix,iy)*dxpm1*dypm1
            Values(6) = Values(6) + dble((j-1)*(j-2))*Coeff(j,i,ix,iy)*dxp*dypm2
          endif
        endif
        dyp = dyp*dy
        if (j.gt.1) dypm1 = dypm1*dy
        if (j.gt.2) dypm2 = dypm2*dy
      enddo
      dxp = dxp*dx
      if (i.gt.1) dxpm1 = dxpm1*dx
      if (i.gt.2) dxpm2 = dxpm2*dx
    enddo
  endif
!
  return
  end
!
  subroutine BicubicCoeff(GridData,nxmax,nymax,ix,iy,Coeff)
!
!  Subroutine for calculating the coefficients for a bicubic spline.
!
!   5/07 Modified to avoid out of bounds access
!
!  On entry:
!
!  GridData(0:3,0:nxmax,0:nymax) = data on grid of points
!                                  1 = function, F
!                                  2 = dF/dx
!                                  3 = dF/dy
!                                  4 = d2F/dxdy
!  nxmax                         = maximum number of points in the x direction
!  nymax                         = maximum number of points in the y direction
!  ix                            = position of lower knot in x direction
!  iy                            = position of lower knot in y direction
!
!  On exit:
!
!  Coeff(4,4)                    = Spline coefficients
!
  use datatypes
  use splineweights
  implicit none
!
!  Passed variables
!
  integer(i4),   intent(in)  :: ix
  integer(i4),   intent(in)  :: iy
  integer(i4),   intent(in)  :: nxmax
  integer(i4),   intent(in)  :: nymax
  real(dp),      intent(in)  :: GridData(0:3,0:nxmax,0:nymax)
  real(dp),      intent(out) :: Coeff(4,4)
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: ind
  integer(i4)                :: j
  integer(i4)                :: k
  real(dp)                   :: Coeff1(16)
  real(dp)                   :: sum
  real(dp)                   :: wrk(16)
!
!  Fill workspace array with values
!
  do i = 1,4
    ind = 4*(i-1)
    wrk(ind+1) = GridData(i-1,ix,iy)
    if (ix.lt.nxmax) then
      wrk(ind+2) = GridData(i-1,ix+1,iy)
      if (iy.lt.nymax) then
        wrk(ind+3) = GridData(i-1,ix+1,iy+1)
      else
        wrk(ind+3) = 0.0_dp
      endif
    else
      wrk(ind+2) = 0.0_dp
      wrk(ind+3) = 0.0_dp
    endif
    if (iy.lt.nymax) then
      wrk(ind+4) = GridData(i-1,ix,iy+1)
    else
      wrk(ind+4) = 0.0_dp
    endif
  enddo
!
!  Compute coefficients
!
  do i = 1,16
    sum = 0.0_dp
    do j = 1,16
      sum = sum + dble(bicubicweights(i,j))*wrk(j)
    enddo
    Coeff1(i) = sum
  enddo
!
  k = 0
  do i = 1,4
    do j = 1,4
      k = k + 1
      Coeff(j,i) = Coeff1(k)
    enddo
  enddo
!
  return
  end
