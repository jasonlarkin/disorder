  subroutine finddef(rdcmax)
!
!  Find interstitials and vacancies
!
!  rdcmax = maximum distance of defect from centre
!
!   3/98 Error in spotting interstitials where there was a
!        partially occupied ion on the same site previously
!        has been fixed.
!   3/13 ndptr replaced with ndintptr and ndvacptr
!   3/13 Modified for lindvacptr/lindintptr
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
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
  integer(i4)        :: niind
  integer(i4)        :: nvind
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
!  Find start of vacancies for this configuration in the list
!
  niind = 0
  nvind = 0
  do i = 1,ncf-1
    niind = niind + nintecfg(i)
    nvind = nvind + nvacacfg(i)
  enddo
  if (ldeflin(ncf)) then
!
!  Set vacancy info for restart - old method
!
    do j = 1,nvacacfg(ncf)
      nvaca = nvaca + 1
      i = ndvacptrcfg(nvind+nvaca)
      ndvacptr(nvaca) = i
      natdvacptrcfg(nvind+nvaca) = natp(i)
      ntypdvacptrcfg(nvind+nvaca) = ntypep(i)
      xyzdvacptrcfg(1,nvind+nvaca) = xperf(i)
      xyzdvacptrcfg(2,nvind+nvaca) = yperf(i)
      xyzdvacptrcfg(3,nvind+nvaca) = zperf(i)
      ndvacptrcfg(nvind+nvaca) = ndvacptr(nvaca)
    enddo
  elseif (lindvacptr(ncf)) then
!
!  Find vacancies for restart - new method
!
    do i = 1,nreg1old
      xi = xperf(i)
      yi = yperf(i)
      zi = zperf(i)
      nati = natp(i)
      ntypi = ntypep(i)
      lfound = .false.
      j = 0
      do while (j.lt.nvacacfg(ncf).and..not.lfound)
        j = j + 1
        natj = natdvacptrcfg(nvind+j)
        ntypj = ntypdvacptrcfg(nvind+j)
        if (nati.eq.natj.and.ntypi.eq.ntypj) then
          xcd = xyzdvacptrcfg(1,nvind+j) - xi
          ycd = xyzdvacptrcfg(2,nvind+j) - yi
          zcd = xyzdvacptrcfg(3,nvind+j) - zi
          r = xcd*xcd + ycd*ycd + zcd*zcd
          lfound = (r.lt.small)
        endif
      enddo
      if (lfound) then
        nvaca = nvaca + 1
        if (nvaca.gt.maxvac) then
          maxvac = nvaca + 5
          call changemaxvac
        endif
        ndvacptr(nvaca) = i
        xd = xi - xdc
        yd = yi - ydc
        zd = zi - zdc
        rdc = xd*xd + yd*yd + zd*zd
        rdcmax = max(rdcmax,rdc)
      endif
    enddo
  else
!
!  Find vacancies in first run
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
        if (nvaca.gt.maxvac) then
          maxvac = nvaca + 5
          call changemaxvac
        endif
        ndvacptr(nvaca) = i
        xd = xi - xdc
        yd = yi - ydc
        zd = zi - zdc
        rdc = xd*xd + yd*yd + zd*zd
        rdcmax = max(rdcmax,rdc)
!
!  Place information in configuration arrays
!
        if (ntotvaca+nvaca.gt.maxtotvac) then
          maxtotvac = ntotvaca + nvaca + 5
          call changemaxtotvac
        endif
        natdvacptrcfg(nvind+nvaca) = nati
        ntypdvacptrcfg(nvind+nvaca) = ntypi
        xyzdvacptrcfg(1,nvind+nvaca) = xi
        xyzdvacptrcfg(2,nvind+nvaca) = yi
        xyzdvacptrcfg(3,nvind+nvaca) = zi
        ndvacptrcfg(nvind+nvaca) = ndvacptr(nvaca)
      endif
    enddo
!
!  Add to total vacancy count
!
    ntotvaca = ntotvaca + nvaca
    nvacacfg(ncf) = nvaca
  endif
  if (.not.lindvacptr(ncf)) then
!
!  Find interstitials for first run
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
        ninte = ninte + 1
        if (ninte.gt.maxint) then
          maxint = ninte + 5
          call changemaxint
        endif
        ndintptr(ninte) = i
        xd = xi - xdc
        yd = yi - ydc
        zd = zi - zdc
        rdc = xd*xd + yd*yd + zd*zd
        rdcmax = max(rdcmax,rdc)
!
!  Place information in configuration arrays
!
        if (ntotinte+ninte.gt.maxtotint) then
          maxtotint = ntotinte + ninte + 5
          call changemaxtotint
        endif
        ndintptrcfg(niind+ninte) = ndintptr(ninte)
      endif
    enddo
  endif
  rdcmax = sqrt(rdcmax)
!
  return
  end
