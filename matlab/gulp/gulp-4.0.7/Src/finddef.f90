  subroutine finddef(rdcmax)
!
!  Find interstitials and vacancies
!
!  rdcmax = maximum distance of defect from centre
!
!   3/98 Error in spotting interstitials where there was a
!        partially occupied ion on the same site previously
!        has been fixed.
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
  use defects
  use parallel
  implicit none
!
!  Passed variables
!
  real(dp)           :: rdcmax
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: j
  integer(i4)        :: nati
  integer(i4)        :: natj
  integer(i4)        :: ntypi
  integer(i4)        :: ntypj
  logical            :: lfound
  real(dp)           :: oci
  real(dp)           :: ocj
  real(dp)           :: r
  real(dp)           :: rdc
  real(dp)           :: small
  real(dp)           :: xcd
  real(dp)           :: ycd
  real(dp)           :: zcd
  real(dp)           :: xd
  real(dp)           :: yd
  real(dp)           :: zd
  real(dp)           :: xi
  real(dp)           :: yi
  real(dp)           :: zi
!
  nvaca = 0
  ninte = 0
  small = 1.0d-8
  rdcmax = 0.0_dp
!
!  Find vacancies
!
  do i = 1,nreg1old
    xi = xperf(i)
    yi = yperf(i)
    zi = zperf(i)
    nati = natp(i)
    ntypi = ntypep(i)
    lfound = .false.
    j = 0
    do while (j.lt.nreg1.and..not.lfound)
      j = j + 1
      natj = natdefe(j)
      ntypj = ntypdefe(j)
      if (nati.eq.natj.and.ntypi.eq.ntypj) then
        xcd = xdefe(j) - xi
        ycd = ydefe(j) - yi
        zcd = zdefe(j) - zi
        r = xcd*xcd + ycd*ycd + zcd*zcd
        lfound = (r.lt.small)
      endif
    enddo
    if (.not.lfound) then
      nvaca = nvaca + 1
      if (nvaca.gt.maxvacint) then
        maxvacint = nvaca + 5
        call changemaxvacint
      endif
      ndptr(nvaca) = i
      xd = xi - xdc
      yd = yi - ydc
      zd = zi - zdc
      rdc = xd*xd + yd*yd + zd*zd
      rdcmax = max(rdcmax,rdc)
    endif
  enddo
!
!  Find interstitials
!
  do i = 1,nreg1
    xi = xdefe(i)
    yi = ydefe(i)
    zi = zdefe(i)
    nati = natdefe(i)
    ntypi = ntypdefe(i)
    oci = occdefe(i)
    lfound = .false.
    j = 0
    do while (j.lt.nreg1old.and..not.lfound)
      j = j + 1
      natj = natp(j)
      ntypj = ntypep(j)
      ocj = occp(j)
      if (nati.eq.natj.and.ntypi.eq.ntypj) then
        xcd = xperf(j) - xi
        ycd = yperf(j) - yi
        zcd = zperf(j) - zi
        r = xcd*xcd + ycd*ycd + zcd*zcd
        lfound = (r.lt.small.and.abs(oci-ocj).lt.1.0d-5)
      endif
    enddo
    if (.not.lfound) then
      ninte=ninte+1
      if (ninte+nvaca.gt.maxvacint) then
        maxvacint = nvaca + ninte + 5
        call changemaxvacint
      endif
      ndptr(ninte+nvaca) = i
      xd = xi - xdc
      yd = yi - ydc
      zd = zi - zdc
      rdc = xd*xd + yd*yd + zd*zd
      rdcmax = max(rdcmax,rdc)
    endif
  enddo
  rdcmax = sqrt(rdcmax)
!
  return
  end
