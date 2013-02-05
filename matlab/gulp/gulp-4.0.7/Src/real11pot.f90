  subroutine real11pot(v,xdrv,ydrv,zdrv,imode,lsymalg)
!
!  Subroutine for calculating the region 1 - region 1 potential
!
!  imode = 1 => defective region 1 - defective region 1 
!  imode = 3 => defective region 1 - perfect region 1
!
!  11/07 Unused variables cleaned up
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, November 2007
!
  use constants
  use control
  use current
  use defects
  use shell
  use times
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: imode
  logical,     intent(in) :: lsymalg
  real(dp)                :: v(*)
  real(dp)                :: xdrv(*)
  real(dp)                :: ydrv(*)
  real(dp)                :: zdrv(*)
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: ii
  integer(i4)             :: j
  integer(i4)             :: nloop1
  integer(i4)             :: nloop2
  real(dp)                :: cputime
  real(dp)                :: cut2s
  real(dp)                :: factor
  real(dp)                :: ocj
  real(dp)                :: qlj
  real(dp)                :: r
  real(dp)                :: r2
  real(dp)                :: rk
  real(dp)                :: time1
  real(dp)                :: time2
  real(dp)                :: trm1
  real(dp)                :: xal
  real(dp)                :: yal
  real(dp)                :: zal
  real(dp)                :: xcrd
  real(dp)                :: ycrd
  real(dp)                :: zcrd
!
  if (lnoreal) return
!
  time1 = cputime()
  cut2s = cuts*cuts
  if (imode.eq.3) then
    nloop2 = nreg1old
  else
    nloop2 = nreg1
  endif
!
!  Outer loop over sites
!
  if (lsymalg) then
    nloop1 = ndasym
  else
    nloop1 = nreg1
  endif
  do i = 1,nloop1
    if (lsymalg) then
      ii = ndsptr(i)
    else
      ii = i
    endif
    xal = xdefe(ii)
    yal = ydefe(ii)
    zal = zdefe(ii)
!
!  Inner loop over second site
!
    do j = 1,nloop2
      if (imode.eq.1) then
        qlj = qdefe(j)
        ocj = occdefe(j)
        xcrd = xdefe(j) - xal
        ycrd = ydefe(j) - yal
        zcrd = zdefe(j) - zal
      else
        qlj = qp(j)
        ocj = occp(j)
        xcrd = xperf(j) - xal
        ycrd = yperf(j) - yal
        zcrd = zperf(j) - zal
      endif
      factor = qlj*ocj*angstoev
      r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      if (r2.gt.cut2s) then
        r = sqrt(r2)
        rk = 1.0/r
        trm1 = factor*rk
        if (imode.eq.1) then
          v(i) = v(i) + trm1
          trm1 = trm1*rk*rk
          xdrv(i) = xdrv(i) + xcrd*trm1
          ydrv(i) = ydrv(i) + ycrd*trm1
          zdrv(i) = zdrv(i) + zcrd*trm1
        else
          v(i) = v(i) - trm1
          trm1 = trm1*rk*rk
          xdrv(i) = xdrv(i) - xcrd*trm1
          ydrv(i) = ydrv(i) - ycrd*trm1
          zdrv(i) = zdrv(i) - zcrd*trm1
        endif
      endif
    enddo
  enddo
!
!  End of real space part  -  perform general tasks
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
!
  return
  end
