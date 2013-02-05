  subroutine ratfn(n,ya,y,tmp1,tmp2)
!
!  Rational function extrapolation for shell coordinates - the
!  x coordinate is assumed to be in consecutive time steps.
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4),   intent(in)  :: n
  real(dp),      intent(in)  :: ya(n)
  real(dp),      intent(out) :: y
  real(dp),      intent(out) :: tmp1(n)
  real(dp),      intent(out) :: tmp2(n)
!
!  Local variables
!
  integer(i4)                 :: i
  integer(i4)                 :: m
  integer(i4)                 :: ns
  real(dp)                    :: dd
  real(dp)                    :: dy
  real(dp)                    :: h
  real(dp)                    :: t
  real(dp)                    :: w
  real(dp),              save :: tiny = 1.0d-20
!
  do i = 1,n
    tmp1(i) = ya(i)
    tmp2(i) = ya(i) + tiny
  enddo
  y = ya(n)
  ns = n - 1
  do m = 1,n-1
    do i = 1,n-m
      w = tmp1(i+1) - tmp2(i)
      h = dble(i+m - n - 1)
      t = (dble(i-n-1))*tmp2(i)/h
      dd = t - tmp1(i+1)
      if (dd.eq.0.0d0) then
        y = ya(n)
        return
      endif
      dd = w/dd
      tmp2(i) = tmp1(i+1)*dd
      tmp1(i) = t*dd
    enddo
    if (2*ns.lt.n-m) then
      dy = tmp1(ns+1)
    else
      dy = tmp2(ns)
      ns = ns - 1
    endif
    y = y + dy
  enddo
!
  end subroutine ratfn
