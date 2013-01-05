  subroutine real2amany
!
!  Subroutine for calculating the contribution of the defective
!  region 1 to the region 2a asymmetric unit densities in the
!  Embedded Atom Model.
!
!  This routine is only needed when the symmetry adapted
!  algorithm is being used for the defect calculation.
!
!   7/97 Created from real12a
!  11/02 Wildcard atoms added
!   5/07 Argument list for twobody call modified
!  11/07 Unused variables cleaned up
!  12/07 Arguments to twobody1 corrected
!  11/08 x/y/z components passed to twobody1
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
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
  use eam,           only : lMEAMden
  use region2a
  use times
  use two
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: n
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lmatch
  logical                                      :: lorder12
  real(dp)                                     :: eatom
  real(dp)                                     :: ec6
  real(dp)                                     :: ereal
  real(dp)                                     :: cmax
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2r
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1i
  real(dp)                                     :: d1j
  real(dp)                                     :: d2i2
  real(dp)                                     :: d2ij
  real(dp)                                     :: d2j2
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r
  real(dp)                                     :: rcheck
  real(dp)                                     :: rcheck2
  real(dp)                                     :: rderiv
  real(dp)                                     :: rdiffc
  real(dp)                                     :: rmiddle2
  real(dp)                                     :: rp
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
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
  if (status/=0) call outofmemory('real2amany','npotl')
!
!  Set dummy energies to zero for numerical safety
!
  eatom = 0.0_dp
  ereal = 0.0_dp
  ec6 = 0.0_dp
!
  cmax = rpmax
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  if (lnoreal) goto 999
!**********************************
!  Region 1 - region 1/2a energy  *
!**********************************
!
!  Outer loop over sites
!
  do i = 1,nreg1
    xal = xdefe(i)
    yal = ydefe(i)
    zal = zdefe(i)
    nati = natdefe(i)
    ntypi = ntypdefe(i)
    oci = occdefe(i)
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
!************************
!  Loop over region 2a  *
!************************
    rmiddle2 = 0.0_dp
    j = 0
    do while (j.lt.ndpasym2a.and.rmiddle2.lt.rcheck2)
      j = j + 1
      jj = ndsptr2a(j)
      xcrd = xr2a(jj) - xal
      ycrd = yr2a(jj) - yal
      zcrd = zr2a(jj) - zal
!
!  If not a molecular calc and component exceeds maximum
!  cut-off then there is nothing to evaluate
!
      if (abs(xcrd).gt.cmax) cycle
      if (abs(ycrd).gt.cmax) cycle
      if (abs(zcrd).gt.cmax) cycle
      natj = nr2a(jj)
      ntypj = ntr2a(jj)
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
      ocj = or2a(jj)
      ofct = oci*ocj
!
!  Distance check relative to centre of region 1
!
      xdiffc = xr2a(jj) - xdc
      ydiffc = yr2a(jj) - ydc
      zdiffc = zr2a(jj) - zdc
      rmiddle2 = xdiffc*xdiffc + ydiffc*ydiffc + zdiffc*zdiffc
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      do n = 1,npote
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
            npots = npots + 1
            npotl(npots) = n
            if (rpot(n).gt.rp) rp = rpot(n)
          endif
        endif
      enddo
!
!  Generate looping indices
!
      cut2r = rp*rp
      if (cut2r.gt.cut2p) cut2r = cut2p
      cut2 = cut2r
      rp = sqrt(cut2)
!
!  Generate distance squared
!
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      if (r.le.cut2) then
!
!  Store vector
!
        dist = sqrt(r)
        call twobody1(eatom,ereal,ec6,.false.,.false.,.false.,1_i4,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                      deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,cut2r, &
                      0.0_dp,0.0_dp,.false.,0_i4,0.0_dp,ofct,0.0_dp,rtrm1,rtrm2,rtrm3,rtrm32, &
                      sctrm1,sctrm2,qli,qlj,.false.,.true.,.false.,.false.,.false.,.false.,0_i4,1_i4, &
                      .false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
        if (.not.lMEAMden) then
          if (lorder12) then
            dscrhor2d(1,j) = dscrhor2d(1,j) + sctrm2*oci
          else
            dscrhor2d(1,j) = dscrhor2d(1,j) + sctrm1*oci
          endif
        endif
      endif
    enddo
  enddo
!
!  End of atom loops
!
999 continue
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('real2amany','npotl')
!
!  Timing
!
  time2 = cputime()
  tregm = tregm + time2 - time1
!
  return
  end
