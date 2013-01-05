  subroutine density11s2(imode)
!
!  Subroutine for calculating the region 1 - region 1 MEAM density
!
!  imode = 1 => defective region 1 - defective region 1 
!  imode = 3 => defective region 1 - perfect region 1
!
!   4/09 Created from real11s2
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, May 2009
!
  use control
  use current
  use defects
  use eam,            only : lMEAM, maxmeamcomponent
  use element,        only : maxele
  use general,        only : smallself
  use sutton
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: imode
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: j
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nloop2
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lmatch
  logical                                      :: lorder12
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: dist
  real(dp)                                     :: ocj
  real(dp)                                     :: r2
  real(dp)                                     :: rp
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  time1 = cputime()
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('real11s2','npotl')
!
  if (imode.eq.3) then
    nloop2 = nreg1old
  else
    nloop2 = nreg1
  endif
!
!  Outer loop over sites
!
  do i = 1,ndasym
    ii = ndsptr(i)
    xal = xdefe(ii)
    yal = ydefe(ii)
    zal = zdefe(ii)
    nati = natdefe(ii)
    ntypi = ntypdefe(ii)
!
!  Inner loop over second site
!
    do j = 1,nloop2
      if (imode.eq.1) then
        natj = natdefe(j)
        ntypj = ntypdefe(j)
        ocj = occdefe(j)
        xcrd = xdefe(j) - xal
        ycrd = ydefe(j) - yal
        zcrd = zdefe(j) - zal
      else
        natj = natp(j)
        ntypj = ntypep(j)
        ocj = occp(j)
        xcrd = xperf(j) - xal
        ycrd = yperf(j) - yal
        zcrd = zperf(j) - zal
      endif
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
      if (nati.eq.natj) then
        nat1 = nati
        nat2 = natj
        if (ntypi.lt.ntypj) then
          lorder12 = .true.
          ntyp1 = ntypi
          ntyp2 = ntypj
        else
          lorder12 = .false.
          ntyp1 = ntypj
          ntyp2 = ntypi
        endif
      elseif (nati.lt.natj) then
        lorder12 = .true.
        nat1 = nati
        nat2 = natj
        ntyp1 = ntypi
        ntyp2 = ntypj
      else
        lorder12 = .false.
        nat1 = natj
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      do n = 1,npote
        if (nptype(n).eq.19) then
          if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
            if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
              npots = npots + 1
              npotl(npots) = n
              if (rpot(n).gt.rp) rp = rpot(n)
            endif
          endif
        endif
      enddo
!
!  Generate looping indices
!
      cut2 = rp*rp
      r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
      if (r2.le.cut2.and.r2.gt.smallself) then
!
!  Store vector
!
        nor = 1
        dist = sqrt(r2)
!*********************************
!  Calculate unscreened density  *
!*********************************
        call twoden1(nor,1_i4,dist,xcrd,ycrd,zcrd,npots,npotl,sctrm1,sctrm2,lorder12)
!
!  For mode 3 the terms are to be subtracted  = > change sign
!
        if (imode.eq.3) then
          if (lMEAM) then
            sctrm1(1:maxmeamcomponent) = - sctrm1(1:maxmeamcomponent)
            sctrm2(1:maxmeamcomponent) = - sctrm2(1:maxmeamcomponent)
          else
            sctrm1(1) = - sctrm1(1)
            sctrm2(1) = - sctrm2(1)
          endif
        endif
        if (lsuttonc) then
          if (lMEAM) then
            if (lorder12) then
              dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i) + sctrm1(1:maxmeamcomponent)*ocj
            else
              dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i) + sctrm2(1:maxmeamcomponent)*ocj
            endif
          else
            if (lorder12) then
              dscrho(1,i) = dscrho(1,i) + sctrm1(1)*ocj
            else
              dscrho(1,i) = dscrho(1,i) + sctrm2(1)*ocj
            endif
          endif
        endif
      endif
    enddo
  enddo
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real11s2','npotl')
!
!  Timing
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
!
  return
  end
