  subroutine switch(r,w,lgrad1,dwdr,lgrad2,d2wdr2)
!
!  Subroutine for calculating switching function.
!  Used for inclusion of point switch in COSMO.
!
!   9/01 Second derivatives added to call
!  11/04 Intent added
!  12/08 Migrated to version 3.5 and converted to f90 format
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use cosmo
  implicit none
!
!  Arguments and function
!
  real(dp), intent(in)  :: r
  real(dp), intent(out) :: w
  real(dp), intent(out) :: dwdr
  real(dp), intent(out) :: d2wdr2
  logical,  intent(in)  :: lgrad1
  logical,  intent(in)  :: lgrad2
!
!  Local variables
!
  logical,        save  :: lfirst = .true.
  real(dp)              :: cosmorange2
  real(dp)              :: rd5
  real(dp),        save :: pts0, pts3
  real(dp),        save :: pts4, pts5
!
  if (lfirst) then
!
!  Set up taper constants
!
    cosmorange2 = cosmorange*cosmorange
    rd5  = 1.0_dp/(cosmorange**5.0_dp)
    pts0 = 1.0_dp
    pts3 = -10.0_dp*cosmorange2*rd5
    pts4 = 15.0_dp*cosmorange*rd5
    pts5 = -6.0_dp*rd5
    lfirst = .false.
  endif
!
!  Calculate taper function
!
  w = ((pts5*r+pts4)*r+pts3)*r*r*r+pts0
  if (lgrad1) then
    dwdr = (((5.0_dp*pts5*r+4.0_dp*pts4)*r+3.0_dp*pts3)*r)*r
    if (lgrad2) then
      d2wdr2 = ((20.0_dp*pts5*r+12.0_dp*pts4)*r+6.0_dp*pts3)*r
!          if (lgrad3) then
!            d3wdr3 = (60.0_dp*pts5*r+24.0_dp*pts4)*r+6.0_dp*pts3
!          endif
    endif
  endif
  return
  end
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  subroutine switch2(r,w,lgrad1,lgrad2,dwdr,d2wdr2)
!
!  Subroutine for calculating switching function.
!  Used for point-segment switch in COSMO.
!
!  Julian Gale, NRI, Curtin University, November 2004
!
  use cosmo
  implicit none
!
!  Arguments and function
!
  real(dp), intent(in)  :: r
  real(dp), intent(out) :: w
  real(dp), intent(out) :: dwdr
  real(dp), intent(out) :: d2wdr2
  logical,  intent(in)  :: lgrad1
  logical,  intent(in)  :: lgrad2
!
!  Local variables
!
  logical,        save  :: lfirst = .true.
  real(dp)              :: cosmormaxs2
  real(dp)              :: rd5
  real(dp),        save :: pts0, pts3
  real(dp),        save :: pts4, pts5
!
  if (lfirst) then
!
!  Set up taper constants
!
    cosmormaxs2 = cosmormaxs*cosmormaxs
    rd5  = 1.0_dp/(cosmormaxs**5.0_dp)
    pts0 = 1.0_dp
    pts3 = -10.0_dp*cosmormaxs2*rd5
    pts4 = 15.0_dp*cosmormaxs*rd5
    pts5 = -6.0_dp*rd5
    lfirst = .false.
  endif
!
!  Calculate taper function
!
  w = ((pts5*r+pts4)*r+pts3)*r*r*r+pts0
  if (lgrad1) then
    dwdr = (((5.0_dp*pts5*r+4.0_dp*pts4)*r+3.0_dp*pts3)*r)*r
    if (lgrad2) then
      d2wdr2 = ((20.0_dp*pts5*r+12.0_dp*pts4)*r+6.0_dp*pts3)*r
!          if (lgrad3) then
!            d3wdr3 = (60.0_dp*pts5*r+24.0_dp*pts4)*r+6.0_dp*pts3
!          endif
    endif
  endif
  return
  end
