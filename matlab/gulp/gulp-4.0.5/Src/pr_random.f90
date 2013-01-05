module pr_random_module

  use datatypes
  use randomnumbers,      only : npr_randomcalls, npr_grandomcalls, lGaussianLast

  implicit none

! Period parameters
  integer(i4), parameter :: n = 624
  integer(i4), parameter :: n1 = 625
  integer(i4), parameter :: m = 397
  integer(i4), parameter :: mata = -1727483681
!                                    constant vector a
  integer(i4), parameter :: umask = -2147483646
!                                    most significant w-r bits
  integer(i4), parameter :: lmask =  2147483647
!                                    least significant r bits
! Tempering parameters
  integer(i4), parameter :: tmaskb= -1658038656
  integer(i4), parameter :: tmaskc= -272236544

!                     the array for the state vector
  integer(i4), save :: mt(0:n-1), mti = n1
!                     mti==N+1 means mt[N] is not initialized

  PRIVATE
  PUBLIC :: sgrnd, std_rand, init_genrand, gauss_rand, adv_rand

CONTAINS

  function gauss_rand()

  real(dp)          :: r1, r2, rr, fac, gauss_rand
  real(dp),    save :: gset
  integer(i4), save :: iset=0
  logical           :: lvalid

  lGaussianLast = .true.
  if (iset==0) then
    lvalid = .false.
    do while (.not.lvalid)
      r1 = 2.0_dp*grnd() - 1.0_dp
      r2 = 2.0_dp*grnd() - 1.0_dp
      rr = r1*r1 + r2*r2
      lvalid = (rr.lt.1.0_dp.and.rr.gt.0.0_dp) 
    enddo
    fac = (sqrt(-2.0_dp*log(rr)/rr))
    gauss_rand = r1*fac
    gset       = r2*fac
    iset = 1
  else
    gauss_rand = gset
    iset = 0
  endif
!
!  Keep track of how many calls have been made
!
  npr_grandomcalls = npr_grandomcalls + 1
!
  return

  end function gauss_rand

  function std_rand()

  real(dp)          :: std_rand

  lGaussianLast = .false.

  std_rand = grnd()
!
!  Keep track of how many calls have been made
!
  npr_randomcalls = npr_randomcalls + 1
!
  return

  end function std_rand

  subroutine adv_rand(nadv,ngadv)

  integer(i4), intent(in) :: nadv
  integer(i4), intent(in) :: ngadv
  integer(i4)             :: i
  real(dp)                :: r1
  
  if (lGaussianLast) then
    do i = 1,nadv
      r1 = std_rand()
    enddo
    do i = 1,ngadv
      r1 = gauss_rand()
    enddo
  else
    do i = 1,ngadv
      r1 = gauss_rand()
    enddo
    do i = 1,nadv
      r1 = std_rand()
    enddo
  endif

  return

  end subroutine adv_rand

  subroutine sgrnd(seed)
!
! This is the original version of the seeding routine.
! It was replaced in the Japanese version in C on 26 January 2002
! It is recommended that routine init_genrand is used instead.
!
  integer(i4), intent(in) :: seed
  integer(i4)             :: iand
!
!    setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
!    [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]
!
  mt(0)= iand(seed, -1_i4)
  do mti = 1,n-1
    mt(mti) = iand(69069_i4 * mt(mti-1_i4), -1_i4)
  enddo

  return

  end subroutine sgrnd
!***********************************************************************

  subroutine init_genrand(seed)
!
! This initialization is based upon the multiplier given on p.106 of the
! 3rd edition of Knuth, The Art of Computer Programming Vol. 2.

! This version assumes that integer overflow does NOT cause a crash.

  integer(i4), intent(in) :: seed

  integer(i4)             :: latest

  mt(0) = seed
  latest = seed
  do mti = 1, n-1
    latest = IEOR( latest, ISHFT( latest, -30_i4 ) )
    latest = latest * 1812433253_i4 + mti
    mt(mti) = latest
  enddo

  return

  end subroutine init_genrand
!***********************************************************************

  function grnd() result(fn_val)

  real(dp) :: fn_val

  integer(i4), SAVE :: mag01(0:1) = (/ 0_i4, mata /)
!                      mag01(x) = x * MATA for x=0,1
  integer(i4)       :: kk, y
  integer(i4)       :: iand, ieor, ishft

! These statement functions have been replaced with separate functions
! tshftu(y) = ISHFT(y,-11_i4)
! tshfts(y) = ISHFT(y,7_i4)
! tshftt(y) = ISHFT(y,15_i4)
! tshftl(y) = ISHFT(y,-18_i4)

  if (mti >= n) then
!                       generate N words at one time
    if (mti == n+1) then
!                            if sgrnd() has not been called,
      call sgrnd(4357_i4)
!                              a default initial seed is used
    endif
  
    do kk = 0,n-m-1
      y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
      mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
    enddo
    do kk = n-m,n-2
      y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
      mt(kk) = IEOR(IEOR(mt(kk+(m-n)), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
    enddo
    y = IOR(IAND(mt(n-1),umask), IAND(mt(0),lmask))
    mt(n-1) = IEOR(IEOR(mt(m-1), ISHFT(y,-1_i4)),mag01(IAND(y,1_i4)))
    mti = 0
  endif

  y = mt(mti)
  mti = mti + 1
  y = IEOR(y, tshftu(y))
  y = IEOR(y, IAND(tshfts(y),tmaskb))
  y = IEOR(y, IAND(tshftt(y),tmaskc))
  y = IEOR(y, tshftl(y))

  if (y < 0) then
    fn_val = (dble(y) + 2.0_dp**32) / (2.0_dp**32 - 1.0_dp)
  else
    fn_val = dble(y) / (2.0_dp**32 - 1.0_dp)
  endif

  return

  end function grnd

  function tshftu(y) result(fn_val)
  integer(i4), intent(IN) :: y
  integer(i4)             :: fn_val
  integer(i4)             :: ishft

  fn_val = ISHFT(y,-11_i4)
  return
  end function tshftu


  function tshfts(y) result(fn_val)
  integer(i4), intent(IN) :: y
  integer(i4)             :: fn_val
  integer(i4)             :: ishft

  fn_val = ISHFT(y,7_i4)
  return
  end function tshfts


  function tshftt(y) result(fn_val)
  integer(i4), intent(IN) :: y
  integer(i4)             :: fn_val
  integer(i4)             :: ishft

  fn_val = ISHFT(y,15_i4)
  return
  end function tshftt


  function tshftl(y) result(fn_val)
  integer(i4), intent(IN) :: y
  integer(i4)             :: fn_val
  integer(i4)             :: ishft

  fn_val = ISHFT(y,-18_i4)
  return
  end function tshftl

end module pr_random_module
