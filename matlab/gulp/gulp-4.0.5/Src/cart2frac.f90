  subroutine cart2frac(ndim,xc,yc,zc,rv,xf,yf,zf,icx,icy,icz)
!
!  Converts Cartesian coordinates to fractional coordinates
!
!  On entry : 
!
!  ndim    = number of dimensions (0-3)
!  xc      = Cartesian X coordinate
!  yc      = Cartesian Y coordinate
!  zc      = Cartesian Z coordinate
!  rv(3,3) = cell vectors
!
!  On exit : 
!
!  xf      = fractional X coordinate
!  yf      = fractional Y coordinate
!  zf      = fractional Z coordinate
!
!   5/04 lmodco option introduced
!   3/07 Gauss renamed to GULP_gauss
!  10/11 Indices added to handle use of mod on coordinations
!
!  Julian Gale, NRI, Curtin University, October 2011
!
  use datatypes
  use control,  only : lmodco
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: ndim
  integer(i4), intent(out) :: icx
  integer(i4), intent(out) :: icy
  integer(i4), intent(out) :: icz
  real(dp),    intent(in)  :: xc
  real(dp),    intent(in)  :: yc
  real(dp),    intent(in)  :: zc
  real(dp),    intent(out) :: xf
  real(dp),    intent(out) :: yf
  real(dp),    intent(out) :: zf
  real(dp),    intent(in)  :: rv(3,3)
!
!  Local variables
!
  integer(i4)   :: j
  real(dp)      :: rmat(3,4)
  real(dp)      :: xo
  real(dp)      :: yo
  real(dp)      :: zo
!
  icx = 0
  icy = 0
  icz = 0
!
  if (ndim.eq.3) then
!
!  Convert cartesian coordinates to fractional for 3D case
!
    rmat(1,4) = xc
    rmat(2,4) = yc
    rmat(3,4) = zc
    do j = 1,3
      rmat(1,j) = rv(1,j)
      rmat(2,j) = rv(2,j)
      rmat(3,j) = rv(3,j)
    enddo
    call GULP_gauss(3_i4,3_i4,1_i4,rmat)
    if (lmodco) then
      xo = rmat(1,4)
      yo = rmat(2,4)
      zo = rmat(3,4)
      xf = dmod(rmat(1,4)+10.0_dp,1.0_dp)
      yf = dmod(rmat(2,4)+10.0_dp,1.0_dp)
      zf = dmod(rmat(3,4)+10.0_dp,1.0_dp)
      icx = nint(xf-xo)
      icy = nint(yf-yo)
      icz = nint(zf-zo)
    else
      xf = rmat(1,4)
      yf = rmat(2,4)
      zf = rmat(3,4)
    endif
  elseif (ndim.eq.2) then
!
!  Convert cartesian coordinates to fractional for 2D case
!
    rmat(1,3) = xc
    rmat(2,3) = yc
    do j = 1,2
      rmat(1,j) = rv(1,j)
      rmat(2,j) = rv(2,j)
    enddo
    call GULP_gauss(2_i4,3_i4,1_i4,rmat)
    if (lmodco) then
      xo = rmat(1,4)
      yo = rmat(2,4)
      xf = dmod(rmat(1,3)+10.0_dp,1.0_dp)
      yf = dmod(rmat(2,3)+10.0_dp,1.0_dp)
      icx = nint(xf-xo)
      icy = nint(yf-yo)
    else
      xf = rmat(1,3)
      yf = rmat(2,3)
    endif
    zf = zc
  elseif (ndim.eq.1) then
!  
!  Convert cartesian coordinates to fractional for 1D case
!       
    rmat(1,1) = xc/rv(1,1)
    if (lmodco) then
      xo = rmat(1,4)
      xf = dmod(rmat(1,1)+10.0_dp,1.0_dp)
      icx = nint(xf-xo)
    else
      xf = rmat(1,1)
    endif
    yf = yc
    zf = zc
  else
!
!  Cluster case
!
    xf = xc
    yf = yc
    zf = zc
  endif
!
  return
  end
