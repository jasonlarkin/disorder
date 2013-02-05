!**************************************
!  Gamma - Streitz and Mintmire form  *
!**************************************
  subroutine gammasm(za,zb,r,gamfafb,dgamfafb,d2gamfafb,gamafb,gambfa,dgamafb,dgambfa,d2gamafb,d2gambfa)
!
!  Subroutine calculates the integral over two 1s orbtials for Slater functions as
!  well as the integral between a point charge and each 1s orbital. The integrals
!  are shift by -1/r.
!
!  On input :
!
!  za   =  orbital exponent of A
!  zb   =  orbital exponent of B
!  r    =  distance between A and B
!
!  On exit :
!
!  gamfafb   =  integral of 1s on A with 1s on B
!  gamafb    =  integral of 1s on B with Znuc on A x Znuc of A
!  gambfa    =  integral of 1s on A with Znuc on B x Znuc of B
!  dgamfafb  =  first derivative of gamfafb with respect to r
!  dgamafb   =  first derivative of gamafb  with respect to r
!  dgambfa   =  first derivative of gambfa  with respect to r
!  d2gamfafb =  second derivative of gamfafb with respect to r
!  d2gamafb  =  second derivative of gamafb  with respect to r
!  d2gambfa  =  second derivative of gambfa  with respect to r
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp), intent(out)    :: dgamfafb
  real(dp), intent(out)    :: dgamafb
  real(dp), intent(out)    :: dgambfa
  real(dp), intent(out)    :: d2gamfafb
  real(dp), intent(out)    :: d2gamafb
  real(dp), intent(out)    :: d2gambfa
  real(dp), intent(out)    :: gamafb
  real(dp), intent(out)    :: gambfa
  real(dp), intent(out)    :: gamfafb
  real(dp), intent(in)     :: r
  real(dp), intent(in)     :: za
  real(dp), intent(in)     :: zb
!
!  Local variables
!
  real(dp)                 :: dtrm1
  real(dp)                 :: dtrm2
  real(dp)                 :: expzetaA
  real(dp)                 :: expzetaB
  real(dp)                 :: rr
  real(dp)                 :: rr2
  real(dp)                 :: rr3
  real(dp)                 :: rzetadiff
  real(dp)                 :: rzetasum
  real(dp)                 :: trm1
  real(dp)                 :: zc1
  real(dp)                 :: zc2
  real(dp)                 :: zc3
  real(dp)                 :: zc4
  real(dp)                 :: zr
!
  gamfafb = 0.0_dp
  gamafb = 0.0_dp
  gambfa = 0.0_dp
  dgamfafb = 0.0_dp
  dgamafb = 0.0_dp
  dgambfa = 0.0_dp
  d2gamfafb = 0.0_dp
  d2gamafb = 0.0_dp
  d2gambfa = 0.0_dp
!
!  This routine only handles two centre integrals
!
  if (r.lt.1.0d-10) return
!
!  Create useful general terms
!
  expzetaA = exp(-2.0_dp*za*r)
  expzetaB = exp(-2.0_dp*zb*r)
  rr = 1.0_dp/r
  rr2 = rr*rr
  rr3 = rr2*rr
!*****************
!  Fa | Fb term  *
!*****************
  if (abs(za-zb).lt.1.0d-8) then
    zr = za*r
    trm1 = (1.0_dp + zr*(1.375_dp + zr*(0.75_dp + zr/6.0_dp)))
    dtrm1 = za*(1.375_dp + zr*(1.5_dp + 0.5_dp*zr))
    dtrm2 = za*za*(1.5_dp + zr)
!
    gamfafb = - rr*trm1*expzetaA
!
    dgamfafb = rr2*trm1*expzetaA + rr*(2.0_dp*za*trm1*expzetaA) - rr*(dtrm1*expzetaA)
!
    d2gamfafb = - 2.0_dp*za*dgamfafb - 2.0_dp*rr3*trm1*expzetaA + rr2*dtrm1*expzetaA
    d2gamfafb = d2gamfafb - rr2*expzetaA*(2.0_dp*za*trm1 - dtrm1) - rr*(dtrm2*expzetaA)
    d2gamfafb = d2gamfafb + rr*(2.0_dp*za*dtrm1*expzetaA)
  else
    rzetasum = 1.0_dp/(za + zb)
    rzetadiff = 1.0_dp/(za - zb)
    zc1 = (rzetasum**2)*(rzetadiff**2)*(zb**4)*za
    zc2 = (rzetasum**2)*(rzetadiff**2)*(za**4)*zb
    zc3 = (rzetasum**3)*(rzetadiff**3)*(zb**4)*(3.0_dp*za**2 - zb**2)
    zc4 = - (rzetasum**3)*(rzetadiff**3)*(za**4)*(3.0_dp*zb**2 - za**2)
!
    gamfafb = - (zc1 + zc3*rr)*expzetaA - (zc2 + zc4*rr)*expzetaB
!
    dgamfafb = 2.0_dp*za*(zc1 + zc3*rr)*expzetaA + 2.0_dp*zb*(zc2 + zc4*rr)*expzetaB
    dgamfafb = dgamfafb + rr2*(zc3*expzetaA + zc4*expzetaB)
!
    d2gamfafb = - 4.0_dp*za*za*(zc1 + zc3*rr)*expzetaA - 4.0_dp*zb*zb*(zc2 + zc4*rr)*expzetaB
    d2gamfafb = d2gamfafb - 2.0_dp*za*zc3*rr2*expzetaA - 2.0_dp*zb*zc4*rr2*expzetaB
    d2gamfafb = d2gamfafb - 2.0_dp*rr3*(zc3*expzetaA + zc4*expzetaB)
    d2gamfafb = d2gamfafb - 2.0_dp*rr2*(za*zc3*expzetaA + zb*zc4*expzetaB)
  endif
!****************
!  a | Fb term  *
!****************
  gamafb = - expzetaB*(zb + rr)
  dgamafb = - 2.0_dp*zb*gamafb + expzetaB*rr2
  d2gamafb = - 2.0_dp*zb*dgamafb - 2.0_dp*expzetaB*(zb*rr2 + rr3)
!****************
!  b | Fa term  *
!****************
  gambfa = - expzetaA*(za + rr)
  dgambfa = - 2.0_dp*za*gambfa + expzetaA*rr2
  d2gambfa = - 2.0_dp*za*dgambfa - 2.0_dp*expzetaA*(za*rr2 + rr3)
!
  return
  end
