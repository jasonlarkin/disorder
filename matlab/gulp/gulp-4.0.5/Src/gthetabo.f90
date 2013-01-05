  subroutine GthetaBO(xij,yij,zij,xik,yik,zik,c,d,h,Gijk,dGijkdr, &
    d2Gijkdr2,d3Gijkdr3,lgrad1,lgrad2,lgrad3)
!
!  Subroutine to calculate the G(theta) function for Bond Order potentials
!
!  On entry : 
!
!  xij             = x component of i->j vector
!  yij             = y component of i->j vector
!  zij             = z component of i->j vector
!  xik             = x component of i->k vector
!  yik             = y component of i->k vector
!  zik             = z component of i->k vector
!  c               = c coefficient
!  d               = d coefficient
!  h               = h coefficient
!  lgrad1          = if .true. calculate the first derivative
!  lgrad2          = if .true. calculate the second derivative
!  lgrad3          = if .true. calculate the third derivative
!
!  On exit :
!
!  Gijk            = the value of the function G(theta)
!  dGijkdr(3)      = the first derivatives of G(theta) w.r.t. the
!                    three different interatomic vectors
!  d2Gijkdr2(6)    = the second derivatives of G(theta) w.r.t. the
!                    six combinations of 2 different vectors
!  d3Gijkdr3(10)   = the third derivatives of G(theta) w.r.t. the
!                    ten combinations of 3 different vectors
!
!  11/03 Created from gtheta
!  11/04 Pi accessed from module
!  12/07 Unused variables removed
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
!  Julian Gale, NRI, Curtin University, December 2007
!
  use bondorderdata
  use iochannels
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)             :: xij
  real(dp),    intent(in)             :: yij
  real(dp),    intent(in)             :: zij
  real(dp),    intent(in)             :: xik
  real(dp),    intent(in)             :: yik
  real(dp),    intent(in)             :: zik
  real(dp),    intent(in)             :: c
  real(dp),    intent(in)             :: d
  real(dp),    intent(in)             :: h
  real(dp),    intent(out)            :: Gijk
  real(dp),    intent(out)            :: dGijkdr(3)
  real(dp),    intent(out)            :: d2Gijkdr2(6)
  real(dp),    intent(out)            :: d3Gijkdr3(10)
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
  logical,     intent(in)             :: lgrad3
!
!  Local variables
!
  real(dp)                            :: c2
  real(dp)                            :: costheta
  real(dp)                            :: cos1d(3)
  real(dp)                            :: cos2d(6)
  real(dp)                            :: cos3d(10)
  real(dp)                            :: d2
  real(dp)                            :: dGijkdcostheta
  real(dp)                            :: d2Gijkdcostheta2
  real(dp)                            :: d3Gijkdcostheta3
  real(dp)                            :: rij
  real(dp)                            :: rik
  real(dp)                            :: rrij
  real(dp)                            :: rrik
  real(dp)                            :: rrij2
  real(dp)                            :: rrik2
  real(dp)                            :: rrij4
  real(dp)                            :: rrik4
  real(dp)                            :: r2ij
  real(dp)                            :: r2ik
  real(dp)                            :: r2jk
  real(dp)                            :: rtrm
  real(dp)                            :: trm
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
!
!  Calculate j->k vector
!
  xjk = xik - xij
  yjk = yik - yij
  zjk = zik - zij
!
!  Calculate interatomic distances
!
  r2ij = xij*xij + yij*yij + zij*zij
  r2ik = xik*xik + yik*yik + zik*zik
  r2jk = xjk*xjk + yjk*yjk + zjk*zjk
  rij = sqrt(r2ij)
  rik = sqrt(r2ik)
!
!  Calculate reciprocal distances
!
  rrij = 1.0_dp/rij
  rrik = 1.0_dp/rik
!
!  Calculate cos(theta) using cosine rule
!
  costheta = 0.5_dp*(r2ij + r2ik - r2jk)*rrij*rrik
!
!  Set remaining terms for convenience
!
  c2 = c*c
  d2 = d*d
  trm = d2 + (h - costheta)**2
  rtrm = 1.0_dp/trm
!***********************
!  Calculate G(theta)  *
!***********************
!
!  Calculate G for all other ranges and pivots
!
  Gijk = 1.0_dp + c2/d2 - c2*rtrm
  if (lgrad1) then
    dGijkdcostheta = - 2.0_dp*c2*rtrm*rtrm*(h - costheta)
    if (lgrad2) then
      d2Gijkdcostheta2 = - 8.0_dp*c2*rtrm*rtrm*rtrm*(h - costheta)*(h - costheta) + &
        2.0_dp*c2*rtrm*rtrm
      if (lgrad3) then
        d3Gijkdcostheta3 = - 24.0_dp*c2*rtrm*rtrm*rtrm*rtrm*(h - costheta)**3 + &
          16.0_dp*c2*rtrm*rtrm*rtrm*(h - costheta) + 4.0_dp*c2*rtrm*rtrm*rtrm*(h - costheta)
      endif
    endif
  endif
!**************************
!  Calculate derivatives  *
!**************************
  if (lgrad1) then
    rrij2 = rrij*rrij
    rrik2 = rrik*rrik
    rrij4 = rrij2*rrij2
    rrik4 = rrik2*rrik2
!
!  First derivatives of cos(theta)
!
!  1 = ij
!  2 = ik
!  3 = jk
!
    cos1d(1) = rrij*rrik - costheta*rrij2
    cos1d(2) = rrij*rrik - costheta*rrik2
    cos1d(3) = -rrij*rrik
!
!  First derivatives of Gijk
!
    dGijkdr(1:3) = dGijkdcostheta*cos1d(1:3)
!
    if (lgrad2) then
!
!  Second derivatives of cos(theta)
!
!  1 = ij/ij
!  2 = ik/ij
!  3 = jk/ij
!  4 = ik/ik
!  5 = jk/ik
!  6 = jk/jk
!
      cos2d(1) = - 2.0_dp*rrij2*rrij*rrik + 3.0_dp*costheta*rrij4
      cos2d(2) = costheta*rrij2*rrik2 - rrij*rrik*(rrij2 + rrik2)
      cos2d(3) = rrij2*rrij*rrik
      cos2d(4) = - 2.0_dp*rrik2*rrik*rrij + 3.0_dp*costheta*rrik4
      cos2d(5) = rrik2*rrij*rrik
      cos2d(6) = 0.0_dp
!
!  Second derivatives of Gijk
!
      d2Gijkdr2(1) = dGijkdcostheta*cos2d(1) + d2Gijkdcostheta2*cos1d(1)*cos1d(1)
      d2Gijkdr2(2) = dGijkdcostheta*cos2d(2) + d2Gijkdcostheta2*cos1d(2)*cos1d(1)
      d2Gijkdr2(3) = dGijkdcostheta*cos2d(3) + d2Gijkdcostheta2*cos1d(3)*cos1d(1)
      d2Gijkdr2(4) = dGijkdcostheta*cos2d(4) + d2Gijkdcostheta2*cos1d(2)*cos1d(2)
      d2Gijkdr2(5) = dGijkdcostheta*cos2d(5) + d2Gijkdcostheta2*cos1d(3)*cos1d(2)
      d2Gijkdr2(6) = dGijkdcostheta*cos2d(6) + d2Gijkdcostheta2*cos1d(3)*cos1d(3)
      if (lgrad3) then
!
!  Third derivatives of cos(theta)
!
!  1 = 111
!  2 = 211
!  3 = 311
!  4 = 221
!  5 = 321
!  6 = 331
!  7 = 222
!  8 = 322
!  9 = 332
! 10 = 333
!
        cos3d(1) = rrij4*(9.0_dp*rrij*rrik - 15.0_dp*costheta*rrij2)
        cos3d(2) = rrij2*(2.0_dp*rrij*rrik2*rrik + 3.0_dp*rrij2*rrij*rrik -  &
                   3.0_dp*costheta*rrij2*rrik2)
        cos3d(3) = - 3.0_dp*rrij4*rrij*rrik
        cos3d(4) = rrik2*(2.0_dp*rrik*rrij2*rrij + 3.0_dp*rrik2*rrik*rrij -  &
                   3.0_dp*costheta*rrik2*rrij2)
        cos3d(5) = - rrij2*rrij*rrik2*rrik
        cos3d(6) = 0.0_dp
        cos3d(7) = rrik4*(9.0_dp*rrik*rrij - 15.0_dp*costheta*rrik2)
        cos3d(8) = - 3.0_dp*rrik4*rrik*rrij
        cos3d(9) = 0.0_dp
        cos3d(10) = 0.0_dp
!
!  Third derivatives of Gijk
!
        d3Gijkdr3(1) = d3Gijkdcostheta3*cos1d(1)*cos1d(1)*cos1d(1) &
          + d2Gijkdcostheta2*(3.0_dp*cos2d(1)*cos1d(1)) &
          + dGijkdcostheta*cos3d(1)
        d3Gijkdr3(2) = d3Gijkdcostheta3*cos1d(2)*cos1d(1)*cos1d(1) &
          + d2Gijkdcostheta2*(2.0_dp*cos2d(2)*cos1d(1) &
          + cos2d(1)*cos1d(2)) + dGijkdcostheta*cos3d(2)
        d3Gijkdr3(3) = d3Gijkdcostheta3*cos1d(3)*cos1d(1)*cos1d(1) &
          + d2Gijkdcostheta2*(2.0_dp*cos2d(3)*cos1d(1) &
          + cos2d(1)*cos1d(3)) + dGijkdcostheta*cos3d(3)
        d3Gijkdr3(4) = d3Gijkdcostheta3*cos1d(2)*cos1d(2)*cos1d(1) &
          + d2Gijkdcostheta2*(2.0_dp*cos2d(2)*cos1d(2) &
          + cos2d(4)*cos1d(1)) + dGijkdcostheta*cos3d(4)
        d3Gijkdr3(5) = d3Gijkdcostheta3*cos1d(3)*cos1d(2)*cos1d(1) &
          + d2Gijkdcostheta2*(cos2d(5)*cos1d(1) + cos2d(3)*cos1d(2) &
          + cos2d(2)*cos1d(3)) + dGijkdcostheta*cos3d(5)
        d3Gijkdr3(6) = d3Gijkdcostheta3*cos1d(3)*cos1d(3)*cos1d(1) &
          + d2Gijkdcostheta2*(2.0_dp*cos2d(3)*cos1d(3) &
          + cos2d(6)*cos1d(1)) + dGijkdcostheta*cos3d(6)
        d3Gijkdr3(7) = d3Gijkdcostheta3*cos1d(2)*cos1d(2)*cos1d(2) &
          + d2Gijkdcostheta2*(3.0_dp*cos2d(4)*cos1d(2)) &
          + dGijkdcostheta*cos3d(7)
        d3Gijkdr3(8) = d3Gijkdcostheta3*cos1d(3)*cos1d(2)*cos1d(2) &
          + d2Gijkdcostheta2*(2.0_dp*cos2d(5)*cos1d(2) &
          + cos2d(4)*cos1d(3)) + dGijkdcostheta*cos3d(8)
        d3Gijkdr3(9) = d3Gijkdcostheta3*cos1d(3)*cos1d(3)*cos1d(2) &
          + d2Gijkdcostheta2*(2.0_dp*cos2d(5)*cos1d(3) &
          + cos2d(6)*cos1d(2)) + dGijkdcostheta*cos3d(9)
        d3Gijkdr3(10) = d3Gijkdcostheta3*cos1d(3)*cos1d(3)*cos1d(3) &
          + d2Gijkdcostheta2*(3.0_dp*cos2d(6)*cos1d(3)) &
          + dGijkdcostheta*cos3d(10)
      endif
    endif
  endif
!
  return
  end
