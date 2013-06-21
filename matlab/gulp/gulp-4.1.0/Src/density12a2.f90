  subroutine density12a2(imode)
!
!  Subroutine for calculating defect MEAM density
!
!  xdefe = region 1 coordinates
!  xclat = region 2 coordinates per unit cell
!
!  imode = 1 => defective region 1 - region 2
!  imode = 2 => perfect region 1 - region 2
!
!   4/09 Created from real12a2
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
  use general,        only : smallself
  use region2a
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
  integer(i4)                                  :: j
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nloop
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lmatch
  logical                                      :: lorder12
  real(dp)                                     :: cmax
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: dist
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: r2
  real(dp)                                     :: rcheck
  real(dp)                                     :: rcheck2
  real(dp)                                     :: rdiffc
  real(dp)                                     :: rmiddle2
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
  real(dp)                                     :: xdiffc
  real(dp)                                     :: ydiffc
  real(dp)                                     :: zdiffc
!
  time1 = cputime()
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('real12a2','npotl')
!
!  Decide whether all interactions lie within unit cell and first
!  neighbours - saves time for large systems
!
  cmax = rpmax
!
  if (imode.eq.1) then
    nloop = nreg1
  elseif (imode.eq.2) then
    nloop = nreg1old
  endif
!**********************************
!  Region 1 - region 1/2a energy  *
!**********************************
!
!  Outer loop over sites
!
  do i = 1,nloop
!
!  Set i attributes according to whether it is defective or perfect region 1
!
    if (imode.eq.1) then
      xal = xdefe(i)
      yal = ydefe(i)
      zal = zdefe(i)
      nati = natdefe(i)
      ntypi = ntypdefe(i)
      oci = occdefe(i)
    elseif (imode.eq.2) then
      xal = xperf(i)
      yal = yperf(i)
      zal = zperf(i)
      nati = natp(i)
      ntypi = ntypep(i)
      oci = occp(i)
    endif
!
!  Find distance from the centre
!
    xdiffc = xal - xdc
    ydiffc = yal - ydc
    zdiffc = zal - zdc
    rdiffc = xdiffc*xdiffc + ydiffc*ydiffc + zdiffc*zdiffc
    rdiffc = sqrt(rdiffc)
    rcheck = rdiffc + cmax + 0.5_dp
    rcheck2 = rcheck*rcheck
!***************************
!  Loop over old region 1  *
!***************************
    do j = 1,nreg1old
      natj = natp(j)
      ntypj = ntypep(j)
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
      xcrd = xperf(j) - xal
      ycrd = yperf(j) - yal
      zcrd = zperf(j) - zal
      ocj = occp(j)
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
              npotl(npots)= n
              if (rpot(n).gt.rp) rp = rpot(n)
            endif
          endif
        endif
      enddo
!
!  Generate looping indices
!
      cut2 = rp*rp
!
!  Generate distance squared
!
      r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      if (r2.le.cut2.and.r2.gt.smallself) then
!
!  Store vector
!
        dist = sqrt(r2)
!
!  Compute unscreened density
!
        call twoden1(1_i4,1_i4,dist,xcrd,ycrd,zcrd,npots,npotl,sctrm1,sctrm2,lorder12)
!
!  For mode 2 the terms are to be subtracted  = > change sign
!
        if (imode.eq.2) then
          if (lMEAM) then
            sctrm1(1:maxmeamcomponent) = - sctrm1(1:maxmeamcomponent)
            sctrm2(1:maxmeamcomponent) = - sctrm2(1:maxmeamcomponent)
          else
            sctrm1(1) = - sctrm1(1)
            sctrm2(1) = - sctrm2(1)
          endif
        endif
        if (lsuttonc.and.imode.eq.1) then
!
!  Don't need to add terms for perfect region as rho  =  bulk rho
!
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
!************************
!  Loop over region 2a  *
!************************
    rmiddle2 = 0.0_dp
    j = 0
    do while (j.lt.npreg2.and.rmiddle2.lt.rcheck2)
      j = j + 1
      xcrd = xr2a(j) - xal
      ycrd = yr2a(j) - yal
      zcrd = zr2a(j) - zal
!
!  If not a molecular calc and component exceeds maximum
!  cut - off then there is nothing to evaluate
!
      if (abs(xcrd).gt.cmax) goto 10
      if (abs(ycrd).gt.cmax) goto 10
      if (abs(zcrd).gt.cmax) goto 10
!
      natj = nr2a(j)
      ntypj = ntr2a(j)
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
      if (nati.eq.natj) then
        lorder12 = .true.
        nat1 = nati
        nat2 = natj
        if (ntypi.lt.ntypj) then
          ntyp1 = ntypi
          ntyp2 = ntypj
        else
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
      ocj = or2a(j)
!
!  Distance check relative to centre of region 1
!
      xdiffc = xr2a(j) - xdc
      ydiffc = yr2a(j) - ydc
      zdiffc = zr2a(j) - zdc
      rmiddle2 = xdiffc*xdiffc + ydiffc*ydiffc + zdiffc*zdiffc
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
!
!  Generate distance squared
!
      r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      if (r2.le.cut2) then
!
!  Store vector
!
        dist = sqrt(r2)
!
!  Compute unscreened density
!
        call twoden1(1_i4,1_i4,dist,xcrd,ycrd,zcrd,npots,npotl,sctrm1,sctrm2,lorder12)
!
        if (lsuttonc) then
!
!  Don't need to add terms for perfect region as rho = bulk rho
!
          if (lMEAM) then
            if (lorder12) then
              if (imode.eq.1) then
                dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i) + sctrm1(1:maxmeamcomponent)*ocj
                dscrhor2d(1:maxmeamcomponent,j) = dscrhor2d(1:maxmeamcomponent,j) + sctrm2(1:maxmeamcomponent)*oci
              else
                dscrhor2p(1:maxmeamcomponent,j) = dscrhor2p(1:maxmeamcomponent,j) - sctrm2(1:maxmeamcomponent)*oci
              endif
            else
              if (imode.eq.1) then
                dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i) + sctrm2(1:maxmeamcomponent)*ocj
                dscrhor2d(1:maxmeamcomponent,j) = dscrhor2d(1:maxmeamcomponent,j) + sctrm1(1:maxmeamcomponent)*oci
              else
                dscrhor2p(1:maxmeamcomponent,j) = dscrhor2p(1:maxmeamcomponent,j) - sctrm1(1:maxmeamcomponent)*oci
              endif
            endif
          else
            if (lorder12) then
              if (imode.eq.1) then
                dscrho(1,i) = dscrho(1,i) + sctrm1(1)*ocj
                dscrhor2d(1,j) = dscrhor2d(1,j) + sctrm2(1)*oci
              else
                dscrhor2p(1,j) = dscrhor2p(1,j) - sctrm2(1)*oci
              endif
            else
              if (imode.eq.1) then
                dscrho(1,i) = dscrho(1,i) + sctrm2(1)*ocj
                dscrhor2d(1,j) = dscrhor2d(1,j) + sctrm1(1)*oci
              else
                dscrhor2p(1,j) = dscrhor2p(1,j) - sctrm1(1)*oci
              endif
            endif
          endif
        endif
      endif
10    continue
    enddo
  enddo
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real12a2','npotl')
!
!  Timing
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
!
  return
  end
