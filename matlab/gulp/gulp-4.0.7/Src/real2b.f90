  subroutine real2b(e2b)
!
!  Calculates region 2b energy from interaction energy with
!  monopole moment only.
!
!  Use scratch as set previously in real2a for region 2 not in core
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
  use constants
  use control
  use current
  use defects
  use derivatives
  use element, only : maxele
  use properties
  use region2a
  use times
  implicit none
!
!  Passed variables
!
  real(dp)           :: e2b
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ind
  integer(i4)        :: j
  integer(i4)        :: k
  integer(i4)        :: nip
  logical            :: liso
  real(dp)           :: const
  real(dp)           :: cputime
  real(dp)           :: csum4
  real(dp)           :: d2inv(3,3)
  real(dp)           :: diff
  real(dp)           :: prod
  real(dp)           :: qmomdx
  real(dp)           :: qmomdy
  real(dp)           :: qmomdz
  real(dp)           :: qmompx
  real(dp)           :: qmompy
  real(dp)           :: qmompz
  real(dp)           :: r2
  real(dp)           :: rqdef
  real(dp)           :: rr6
  real(dp)           :: rx
  real(dp)           :: ry
  real(dp)           :: rz
  real(dp)           :: sum26(3,3)
  real(dp)           :: sum4
  real(dp)           :: t1
  real(dp)           :: t2
  real(dp)           :: xcl
  real(dp)           :: ycl
  real(dp)           :: zcl
  real(dp),     save :: xmono = 0.0_dp
  real(dp),     save :: ymono = 0.0_dp
  real(dp),     save :: zmono = 0.0_dp
  real(dp)           :: xmonold
  real(dp)           :: ymonold
  real(dp)           :: zmonold
!
  t1 = cputime()
  liso = (index(keyword,'noan').ne.0)
!
!  If there is no monopole then there is no e2b energy
!  e2b has already been zero in setdef so return
!
  if (abs(qdef).lt.1.0d-8) return
!******************************************
!  Calculate location of monopole moment  *
!******************************************
  xmonold = xmono
  ymonold = ymono
  zmonold = zmono
!
!  Defective lattice moments
!
  qmomdx = 0.0_dp
  qmomdy = 0.0_dp
  qmomdz = 0.0_dp
  do j = 1,nreg1
    qmomdx = qmomdx + qdefe(j)*occdefe(j)*(xdefe(j) - xdc)
    qmomdy = qmomdy + qdefe(j)*occdefe(j)*(ydefe(j) - ydc)
    qmomdz = qmomdz + qdefe(j)*occdefe(j)*(zdefe(j) - zdc)
  enddo
!
!  Perfect lattice moments
!
  qmompx = 0.0_dp
  qmompy = 0.0_dp
  qmompz = 0.0_dp
  do j = 1,nreg1old
    qmompx = qmompx + qp(j)*occp(j)*(xperf(j) - xdc)
    qmompy = qmompy + qp(j)*occp(j)*(yperf(j) - ydc)
    qmompz = qmompz + qp(j)*occp(j)*(zperf(j) - zdc)
  enddo
  rqdef = 1.0_dp/qdef
  xmono = xdc + rqdef*(qmomdx - qmompx)
  ymono = ydc + rqdef*(qmomdy - qmompy)
  zmono = zdc + rqdef*(qmomdz - qmompz)
!
!  If monopole location is the same as before then no need
!  to recalculate the e2b energy.
!
  diff = (xmono - xmonold)**2 + (ymono - ymonold)**2 + (zmono - zmonold)**2
  if (diff.lt.1.0d-8.and.abs(e2b).gt.1.0d-12) return
!
!  Set up general reciprocal space terms
!
  csum4 = 0.0_dp
  call setktrm4(csum4)
  e2b = 0.0_dp
  const = 0.5_dp*qdef*qdef*angstoev*angstoev
!**************************
!  Loop over sublattices  *
!**************************
  do i = 1,numat
    if (abs(qf(i)).gt.0.0_dp.and.(.not.lshello.or.nat(i).gt.maxele)) then
!
!  Zero terms
!
      sum4 = csum4
      do j = 1,3
        do k = 1,3
          sum26(k,j) = 0.0_dp
        enddo
      enddo
!
!  Calculate infinite sums
!
      call recip4(i,sum4,sum26,xmono,ymono,zmono)
      call real4(i,sum4,sum26,xmono,ymono,zmono)
!
!  Construct final ra*rb/r*6 matrix
!
      if (.not.liso) then
        do j = 1,3
          do k = 1,3
            sum26(k,j) = 0.125_dp*sum26(k,j)
          enddo
        enddo
        sum26(1,1) = sum26(1,1) + 0.25_dp*sum4
        sum26(2,2) = sum26(2,2) + 0.25_dp*sum4
        sum26(3,3) = sum26(3,3) + 0.25_dp*sum4
      endif
!****************************************************
!  Subtract explicit terms due to regions 1 and 2a  *
!****************************************************
      do j = 1,nreg1old
        if (npsite(j).eq.i) then
!
!  Valid ion on sublattice
!
          rx = xperf(j) - xmono
          ry = yperf(j) - ymono
          rz = zperf(j) - zmono
          r2 = rx*rx + ry*ry + rz*rz
          if (r2.ge.0.01_dp) then
            if (liso) then
              sum4 = sum4 - 1.0_dp/(r2*r2)
            else
              rr6 = 1.0_dp/(r2*r2*r2)
              sum26(1,1) = sum26(1,1) - rx*rx*rr6
              sum26(2,1) = sum26(2,1) - ry*rx*rr6
              sum26(3,1) = sum26(3,1) - rz*rx*rr6
              sum26(2,2) = sum26(2,2) - ry*ry*rr6
              sum26(3,2) = sum26(3,2) - rz*ry*rr6
              sum26(3,3) = sum26(3,3) - rz*rz*rr6
            endif
          endif
        endif
      enddo
      do j = 1,nreg2
        xcl = xr2a(j)
        ycl = yr2a(j)
        zcl = zr2a(j)
        nip = nps(j)
        if (nip.eq.i) then
!
!  Valid ion on sublattice
!
          rx = xcl - xmono
          ry = ycl - ymono
          rz = zcl - zmono
          r2 = rx*rx + ry*ry + rz*rz
          if (r2.ge.0.01_dp) then
            if (liso) then
              sum4 = sum4 - 1.0_dp/(r2*r2)
            else
              rr6 = 1.0_dp/(r2*r2*r2)
              sum26(1,1) = sum26(1,1) - rx*rx*rr6
              sum26(2,1) = sum26(2,1) - ry*rx*rr6
              sum26(3,1) = sum26(3,1) - rz*rx*rr6
              sum26(2,2) = sum26(2,2) - ry*ry*rr6
              sum26(3,2) = sum26(3,2) - rz*ry*rr6
              sum26(3,3) = sum26(3,3) - rz*rz*rr6
            endif
          endif
        endif
      enddo
      ind = 3*(i - 1)
      do j = 1,3
        do k = 1,3
          d2inv(k,j) = derv3(ind + k,j)
        enddo
      enddo
      if (.not.liso) then
        sum26(1,2) = sum26(2,1)
        sum26(1,3) = sum26(3,1)
        sum26(2,3) = sum26(3,2)
      endif
!**********************************
!  Calculate energy contribution  *
!**********************************
      if (liso) then
        prod = sum4*(d2inv(1,1) + d2inv(2,2) + d2inv(3,3))/3.0_dp
      else
        prod = 0.0_dp
        do j = 1,3
          do k = 1,3
            prod = prod + d2inv(k,j)*sum26(k,j)
          enddo
        enddo
      endif
!
!  Charge is already included once in d2inv matrix
!
      e2b = e2b - const*qf(i)*qf(i)*occuf(i)*occuf(i)*prod
    endif
  enddo
!
  t2 = cputime()
  treg2b = treg2b + t2 - t1
!
  return
  end
