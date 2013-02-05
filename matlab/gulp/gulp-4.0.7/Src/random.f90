  function GULP_random(iseed,imode)
!
!  Random number generator from numerical recipes
!
!  imode = 1 => random number between 0 and 1
!        = 2 => random number between -1 and 1
!
  use datatypes
  use randomnumbers,      only : nrandomcalls
  implicit none
!
!  Passed variables
!
  integer(i4)       :: iseed
  integer(i4)       :: imode
  real(dp)          :: GULP_random
!
!  Local variables
!
  integer(i4)       :: i
  integer(i4), save :: ia1 = 625
  integer(i4), save :: ia2 = 1741
  integer(i4), save :: ia3 = 1541
  integer(i4), save :: ic1 = 6571
  integer(i4), save :: ic2 = 2731
  integer(i4), save :: ic3 = 2957
  integer(i4), save :: ix1 
  integer(i4), save :: ix2 
  integer(i4), save :: ix3
  integer(i4)       :: j
  integer(i4), save :: m1  = 31104
  integer(i4), save :: m2  = 12960
  integer(i4), save :: m3  = 14000
  real(dp)          :: rm1
  real(dp)          :: rm2
  real(dp),    save :: rr(97)
!
  rm1 = 1.0_dp/m1
  rm2 = 1.0_dp/m2
  if (iseed.lt.0) then
    ix1 = mod(ic1 - iseed,m1)
    ix1 = mod(ia1*ix1 + ic1,m1)
    ix2 = mod(ix1,m2)
    ix1 = mod(ia1*ix1 + ic1,m1)
    ix3 = mod(ix1,m3)
    do i = 1,97
      ix1 = mod(ia1*ix1 + ic1,m1)
      ix2 = mod(ia2*ix2 + ic2,m2)
      rr(i) = (float(ix1) + float(ix2)*rm2)*rm1
    enddo
    iseed = abs(iseed)
  endif
  ix3 = mod(ia3*ix3 + ic3,m3)
  j = 1 + (97*ix3)/m3
  if (j.gt.97.or.j.lt.1) then
    call outerror('array bounds exceeded in GULP_random',0_i4)
    call stopnow('GULP_random')
  endif
  if (imode.eq.1) then
    GULP_random = rr(j)
  else
    GULP_random = 2.0_dp*rr(j) - 1.0_dp
  endif
  ix1 = mod(ia1*ix1 + ic1,m1)
  ix2 = mod(ia2*ix2 + ic2,m2)
  rr(j) = (float(ix1) + float(ix2)*rm2)*rm1
!
!  Increment counter for number of calls
!
  nrandomcalls = nrandomcalls + 1
!
  return
  end
!
  function GULP_grandom(iseed,stdev)
!
!  Uses calls to random to generate gaussian random numbers
!  with mean zero and standard deviation of stdev.
!
!  Generates numbers in pairs so store second number
!
  use constants
  implicit none
!
!  Passed variables
!
  integer(i4)       :: iseed
  real(dp)          :: GULP_grandom
  real(dp)          :: stdev
!
!  Local variables
!
  logical,     save :: lsecond  =  .false.
  real(dp)          :: fct
  real(dp)          :: r1
  real(dp)          :: r2
  real(dp)          :: GULP_random
  real(dp),    save :: rn1
  real(dp),    save :: rn2
  real(dp)          :: twopi
!
  twopi = 2.0_dp*pi
  if (lsecond) then
!
!  Use previously generated second number
!
    GULP_grandom = rn2
    lsecond = .false.
  else
    r1 = GULP_random(iseed,1_i4)
    r2 = GULP_random(iseed,1_i4)
    r2 = r2*twopi
    fct = sqrt(-2.0_dp*log(r1))
    rn1 = fct*cos(r2)
    rn2 = fct*sin(r2)
    GULP_grandom = rn1
    lsecond = .true.
  endif
  return
  end
