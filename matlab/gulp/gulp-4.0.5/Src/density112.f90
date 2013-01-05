  subroutine density112(imode)
!
!  Subroutine for calculating the region 1 - region 1 MEAM density
!
!  imode = 1 => defective region 1 - defective region 1 
!  imode = 3 => defective region 1 - perfect region 1
!
!  If called with modes 2 & 4 there is nothing to do.
!
!   4/09 Created from real112
!   6/09 Call to twobody1 corrected to be twoden
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use control
  use current
  use defects
  use eam,            only : lMEAM, maxmeamcomponent
  use element,        only : maxele
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
  integer(i4)                                  :: ione
  integer(i4)                                  :: j
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nloop1
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
  real(dp)                                     :: oci
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
  if (imode.ne.1.and.imode.ne.3) return
!
  time1 = cputime()
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('real112','npotl')
!
  if (imode.eq.1) then
    ione = 2
    nloop1 = nreg1
  elseif (imode.eq.2) then
    ione = 2
    nloop1 = nreg1old
  elseif (imode.eq.3) then
    ione = 1
    nloop1 = nreg1
  elseif (imode.eq.4) then
    ione = 1
    nloop1 = nreg1old
  endif
!
!  Outer loop over sites
!
  do i = ione,nloop1
    xal = xdefe(i)
    yal = ydefe(i)
    zal = zdefe(i)
    nati = natdefe(i)
    ntypi = ntypdefe(i)
    oci = occdefe(i)
!
    if (imode.eq.3) then
      nloop2 = nreg1old
    else
      nloop2 = i - 1
    endif
!
!  Inner loop over second site
!
    jloop: do j = 1,nloop2
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
      if (r2.lt.cut2) then
!
!  Store vector
!
        nor = 1
        dist = sqrt(r2)
      else
        cycle jloop
      endif
!*********************************
!  Calculate unscreened density  *
!*********************************
      call twoden(nor,1_i4,dist,xcrd,ycrd,zcrd,npots,npotl,sctrm1,sctrm2,lorder12)
!
!  For mode 3 the terms are to be subtracted  = > change sign
!
      if (imode.eq.3) then
        if (lMEAM) then
          sctrm1(1:maxmeamcomponent) = - sctrm1(1:maxmeamcomponent)
          sctrm2(1:maxmeamcomponent) = - sctrm2(1:maxmeamcomponent)
        else
          sctrm1(1) =  - sctrm1(1)
          sctrm2(1) =  - sctrm2(1)
        endif
      endif
      if (lsuttonc) then
!
!  Don't need to add terms for perfect region as rho  =  bulk rho
!
        if (lMEAM) then
          if (lorder12) then
            dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i) + sctrm1(1:maxmeamcomponent)*ocj
            if (imode.eq.1) dscrho(1:maxmeamcomponent,j) = dscrho(1:maxmeamcomponent,j) + sctrm2(1:maxmeamcomponent)*oci
          else
            dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i) + sctrm2(1:maxmeamcomponent)*ocj
            if (imode.eq.1) dscrho(1:maxmeamcomponent,j) = dscrho(1:maxmeamcomponent,j) + sctrm1(1:maxmeamcomponent)*oci
          endif
        else
          if (lorder12) then
            dscrho(1,i) = dscrho(1,i) + sctrm1(1)*ocj
            if (imode.eq.1) dscrho(1,j) = dscrho(1,j) + sctrm2(1)*oci
          else
            dscrho(1,i) = dscrho(1,i) + sctrm2(1)*ocj
            if (imode.eq.1) dscrho(1,j) = dscrho(1,j) + sctrm1(1)*oci
          endif
        endif
      endif
    enddo jloop
  enddo
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real112','npotl')
!
!  Timing
!
  time2 = cputime()
  treg1 = treg1 + time2 - time1
!
  return
  end
