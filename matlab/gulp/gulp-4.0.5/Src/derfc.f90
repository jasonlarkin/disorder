  function derfc(x)
!
!  Calculates complementary error function
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp)              :: derfc
  real(dp), intent(in)  :: x
!
!  Local variables
!
  integer(i4)           :: j
  real(dp)              :: erfc
  real(dp)              :: expn
  real(dp)              :: factor
  real(dp),        save :: a(10)
  real(dp),        save :: p(8)
  real(dp),        save :: q(8)
  real(dp)              :: sd
  real(dp)              :: sn
  real(dp)              :: xsq
  real(dp)              :: z
!
  data factor/1.128379167095512d+0/, &
   p/883.47894260850d+0,1549.6793124037d+0,1347.1941340976d+0, &
   723.04000277753d+0,255.50049469496d+0,59.240010112914d+0, &
   8.3765310814197d+0,0.56418955944261d+0/,q/883.47894260850d+0, &
   2546.5785458098d+0,3337.2213699893d+0,2606.7120152651d+0, &
   1333.5699756800d+0,460.28512369160d+0,105.50025439769d+0, &
   14.847012237523d+0/
  data a/ &
   0.10000000000000d+01,-0.33333333333333d+00, 0.10000000000000d+00, &
  -0.23809523809524d-01, 0.46296296296296d-02,-0.75757575757576d-03, &
   0.10683760683761d-03,-0.13227513227513d-04, 0.14589169000934d-05, &
  -0.14503852223151d-06/
!
  z = abs(x)
  xsq = z*z
  if (z.gt.8.0)then
    erfc = 1.0_dp - sign(1.0_dp,x)
  else if (z.gt.0.47) then
    expn = exp(-xsq)
    sn = p(8)
    sd = z + q(8)
    do j = 2,8
      sn = sn*z + p(9-j)
      sd = sd*z + q(9-j)
    enddo
    erfc = 1.0_dp - sign((1.0_dp-sn*expn/sd),x)
  elseif (z.gt.1.0d-15) then
    erfc = a(10)
    do j = 1,9
      erfc = erfc*xsq + a(10-j)
    enddo
    erfc = 1.0_dp - sign(factor*z*erfc,x)
  else
    erfc = 1.0_dp
  endif
  derfc = erfc
  return
  end
!
  function derf(x)
!
!  Calculates the error function
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp)              :: derf
  real(dp),  intent(in) :: x
!
!  Local variables
!
  real(dp)              :: derfc
  real(dp)              :: erfcx
!
  erfcx = derfc(x)
  derf = 1.0_dp - erfcx
!
  return
  end

  function dserfc(s,x,rx)
!
!  Calculates complementary error function for case of shells
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp)              :: dserfc
  real(dp), intent(in)  :: s
  real(dp), intent(in)  :: x
  real(dp), intent(in)  :: rx
!
!  Local variables
!
  integer(i4)           :: j
  real(dp)              :: erfc
  real(dp)              :: expn
  real(dp)              :: factor
  real(dp),        save :: a(10)
  real(dp),        save :: p(8)
  real(dp),        save :: q(8)
  real(dp)              :: sd
  real(dp)              :: sn
  real(dp)              :: xsq
  real(dp)              :: z
!
  data factor/1.128379167095512d+0/, &
   p/883.47894260850d+0,1549.6793124037d+0,1347.1941340976d+0, &
   723.04000277753d+0,255.50049469496d+0,59.240010112914d+0, &
   8.3765310814197d+0,0.56418955944261d+0/,q/883.47894260850d+0, &
   2546.5785458098d+0,3337.2213699893d+0,2606.7120152651d+0, &
   1333.5699756800d+0,460.28512369160d+0,105.50025439769d+0, &
   14.847012237523d+0/
  data a/ &
   0.10000000000000d+01,-0.33333333333333d+00, 0.10000000000000d+00, &
  -0.23809523809524d-01, 0.46296296296296d-02,-0.75757575757576d-03, &
   0.10683760683761d-03,-0.13227513227513d-04, 0.14589169000934d-05, &
  -0.14503852223151d-06/
!
  z = abs(s*x)
  if (z.gt.8.0)then
    erfc = - sign(1.0_dp,x)*rx
  else if (z.gt.0.47) then
    xsq = z*z
    expn = exp(-xsq)
    sn = p(8)
    sd = z + q(8)
    do j = 2,8
      sn = sn*z + p(9-j)
      sd = sd*z + q(9-j)
    enddo
    erfc = - sign((1.0_dp-sn*expn/sd),x)*rx
  elseif (z.gt.1.0d-15) then
    xsq = z*z
    erfc = a(10)
    do j = 1,9
      erfc = erfc*xsq + a(10-j)
    enddo
    erfc = - sign(factor*s*erfc,x)
  else
    erfc = 0.0_dp
  endif
  dserfc = erfc
  return
  end
