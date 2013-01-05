  subroutine setbrenner3
!
!  Subroutine sets the parameters for the Brenner potential
!  according to the potential number.
!
!   5/02 Created
!  10/02 Storage of Brenner spline data altered for F/H
!  10/04 Routine generalised for multiple species
!  10/04 Terms in bGcos rearranged into bGcosCspecial
!  10/04 bH -> bP to match papers
!  10/04 Triad data updated & fixed
!  11/04 Order of bP_CH data reversed with respect to Nc/Nh
!        to match bP order
!  11/04 Logical to indicate whether spline coefficients have been
!        determined for bP yet has been added
!   7/05 Subroutine renamed from setbrenner to allow for introduction
!        of setbrenner1 for nbrennertype = 1
!   7/05 bdelta made a function of only one species
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
!  Julian Gale, NRI Curtin University, December 2007
!
  use datatypes
  use brennerdata
  use constants, only : pi
  implicit none
!
!  Local variables
!
  integer(i4) :: ifail
  integer(i4) :: i
  integer(i4) :: ix
  integer(i4) :: ixm
  integer(i4) :: ixp
  integer(i4) :: iy
  integer(i4) :: iym
  integer(i4) :: iyp
  integer(i4) :: iz
  integer(i4) :: izm
  integer(i4) :: izp
  integer(i4) :: j
  integer(i4) :: k
  integer(i4) :: n
  real(dp)    :: matrix(8,8)
  real(dp)    :: Rch
  real(dp)    :: Rcf
  real(dp)    :: Rfh
  real(dp)    :: Rff
  real(dp)    :: Rhh
  real(dp)    :: Roh
  real(dp)    :: rhs(8)
  real(dp)    :: wrk(16)
!
!  Setup simple constants for convenience
!
  nREBOspecies2 = nREBOspecies**2
!
!  Initialise arrays to zero / false
!
  Fneeded(nREBOspecies*(nREBOspecies+1)/2) = .false.
  Tneeded(nREBOspecies*(nREBOspecies+1)/2) = .false.
  bF(0:7,0:3,0:3,1:9,1:nREBOspecies*(nREBOspecies+1)/2) = 0.0_dp
  bP(0:3,0:3,0:3,1:nREBOspecies*(nREBOspecies+1)/2,1:nREBOspecies) = 0.0_dp
  lbPdone(0:3,0:3,1:nREBOspecies*(nREBOspecies+1)/2,1:nREBOspecies) = .false.
  bP_CH(0:3,0:3,0:3,3) = 0.0_dp
  lbP_CHdone(0:3,0:3,3) = .false.
  bGcos(1:8,1:nREBOspecies) = 0.0_dp
  bGcosCspecial(1:8,1:2) = 0.0_dp
  bcos(1:8,1:2) = 0.0_dp
  bLcos(1:8,1:4) = 0.0_dp
!****************
!  Potential 3  *
!****************
!
!  C-C two-body parameters
!
  brepA(1) = 10953.544162170_dp
  brepAlpha(1) = 4.7465390606595_dp
  brepQ(1) = 0.3134602960833_dp
  battB(1,1) = 12388.79197798_dp
  battB(2,1) = 17.56740646509_dp
  battB(3,1) = 30.71493208065_dp
  battBeta(1,1) = 4.7204523127_dp
  battBeta(2,1) = 1.4332132499_dp
  battBeta(3,1) = 1.3826912506_dp
  bR1(1) = 1.7_dp   
  bR2(1) = 2.0_dp
  bTR1(1) = 1.7_dp   
  bTR2(1) = 2.0_dp
!
!  C-H two-body parameters
!
  brepA(2) = 149.94098723_dp
  brepAlpha(2) = 4.10254983_dp
  brepQ(2) = 0.340775728_dp
  battB(1,2) = 32.3551866587_dp
  battB(2,2) = 0.0_dp
  battB(3,2) = 0.0_dp
  battBeta(1,2) = 1.43445805925_dp
  battBeta(2,2) = 0.0_dp
  battBeta(3,2) = 0.0_dp
  bR1(2) = 1.3_dp
  bR2(2) = 1.8_dp
! Note : different distance cut-offs are used for torsions for C-H!
  bTR1(2) = 1.3_dp
  bTR2(2) = 1.6_dp
!
!  H-H two-body parameters
!
  brepA(3) = 32.817355747_dp
  brepAlpha(3) = 3.536298648_dp
  brepQ(3) = 0.370471487045_dp
  battB(1,3) = 29.632593_dp
  battB(2,3) = 0.0_dp
  battB(3,3) = 0.0_dp
  battBeta(1,3) = 1.71589217_dp
  battBeta(2,3) = 0.0_dp
  battBeta(3,3) = 0.0_dp
  bR1(3) = 1.1_dp
  bR2(3) = 1.7_dp
  bTR1(3) = 1.1_dp
  bTR2(3) = 1.7_dp
!
!  O-C two-body parameters
!
  brepA(4) = 81.0576_dp
  brepAlpha(4) = 3.554_dp
  brepQ(4) = 9.132_dp
  battB(1,4) = 268.043_dp
  battB(2,4) = 0.0_dp
  battB(3,4) = 0.0_dp
  battBeta(1,4) = 2.344_dp
  battBeta(2,4) = 0.0_dp
  battBeta(3,4) = 0.0_dp
  bR1(4) = 1.60_dp
  bR2(4) = 1.90_dp
  bTR1(4) = 1.60_dp
  bTR2(4) = 1.90_dp
!
!  O-H two-body parameters
!
  brepA(5) = 717.1495_dp
  brepAlpha(5) = 1.628_dp
  brepQ(5) = 0.1240_dp
  battB(1,5) = 884.5045_dp
  battB(2,5) = 0.0_dp
  battB(3,5) = 0.0_dp
  battBeta(1,5) = 1.704_dp
  battBeta(2,5) = 0.0_dp
  battBeta(3,5) = 0.0_dp
  bR1(5) = 1.30_dp
  bR2(5) = 1.70_dp
  bTR1(5) = 1.30_dp
  bTR2(5) = 1.70_dp
!     
!  O-O two-body parameters
!     
  brepA(6) = 685.2555_dp
  brepAlpha(6) = 1.173_dp
  brepQ(6) = 0.4065_dp
  battB(1,6) = 1105.0_dp
  battB(2,6) = 0.0_dp
  battB(3,6) = 0.0_dp
  battBeta(1,6) = 1.325_dp
  battBeta(2,6) = 0.0_dp
  battBeta(3,6) = 0.0_dp
  bR1(6) = 1.55_dp
  bR2(6) = 1.70_dp
  bTR1(6) = 1.55_dp
  bTR2(6) = 1.70_dp
!
!  F-C two-body parameters
!
  brepA(7) = 909.2022_dp
  brepAlpha(7) = 3.7128_dp
  brepQ(7) = 0.0_dp
  battB(1,7) = 219.7799_dp
  battB(2,7) = 0.0_dp
  battB(3,7) = 0.0_dp
  battBeta(1,7) = 2.1763_dp
  battBeta(2,7) = 0.0_dp
  battBeta(3,7) = 0.0_dp
  bR1(7) = 1.70_dp
  bR2(7) = 2.00_dp
  bTR1(7) = 1.70_dp
  bTR2(7) = 2.00_dp
!
!  F-H two-body parameters
!
  brepA(8) = 887.0513_dp
  brepAlpha(8) = 3.7789_dp
  brepQ(8) = 0.0_dp
  battB(1,8) = 571.1737_dp
  battB(2,8) = 0.0_dp
  battB(3,8) = 0.0_dp
  battBeta(1,8) = 3.0920_dp
  battBeta(2,8) = 0.0_dp
  battBeta(3,8) = 0.0_dp
  bR1(8) = 1.30_dp
  bR2(8) = 1.80_dp
  bTR1(8) = 1.30_dp
  bTR2(8) = 1.80_dp
!
!  F-O two-body parameters
!
  brepA(9) = 0.0_dp
  brepAlpha(9) = 0.0_dp
  brepQ(9) = 0.0_dp
  battB(1,9) = 0.0_dp
  battB(2,9) = 0.0_dp
  battB(3,9) = 0.0_dp
  battBeta(1,9) = 0.0_dp
  battBeta(2,9) = 0.0_dp
  battBeta(3,9) = 0.0_dp
  bR1(9) = 0.0_dp
  bR2(9) = 0.0_dp
  bTR1(9) = 0.0_dp
  bTR2(9) = 0.0_dp
!
!  F-F two-body parameters
!
  brepA(10) = 16451.97_dp
  brepAlpha(10) = 6.8149_dp
  brepQ(10) = 0.0_dp
  battB(1,10) = 146.8149_dp
  battB(2,10) = 0.0_dp
  battB(3,10) = 0.0_dp
  battBeta(1,10) = 2.8568_dp
  battBeta(2,10) = 0.0_dp
  battBeta(3,10) = 0.0_dp
  bR1(10) = 1.70_dp
  bR2(10) = 2.00_dp
  bTR1(10) = 1.70_dp
  bTR2(10) = 2.00_dp
!
  bF(0,1,1,1,1) = 0.105_dp
  bF(0,1,1,2,1) = -0.0041775_dp
  bF(0,1,1,3:9,1) = -0.0160856_dp
  bF(0,2,2,1,1) = 0.09444957_dp
  bF(0,2,2,2,1) = 0.022_dp
  bF(0,2,2,3,1) = 0.03970587_dp
  bF(0,2,2,4,1) = 0.03308822_dp
  bF(0,2,2,5,1) = 0.02647058_dp
  bF(0,2,2,6,1) = 0.01985293_dp
  bF(0,2,2,7,1) = 0.01323529_dp
  bF(0,2,2,8,1) = 0.00661764_dp
  bF(0,2,2,9,1) = 0.0_dp
  bF(0,0,1,1,1) = 0.04338699_dp
  bF(0,0,2,1,1) = 0.0099172158_dp
  bF(0,0,2,2,1) = -0.011942669_dp
  bF(0,0,3,1:9,1) = -0.119798935_dp
  bF(0,1,2,1,1) = 0.0096495698_dp
  bF(0,1,2,2,1) = 0.030_dp
  bF(0,1,2,3,1) = -0.020_dp
  bF(0,1,2,4,1) = -0.0233778774_dp
  bF(0,1,2,5,1) = -0.0267557548_dp
  bF(0,1,2,6:9,1) = -0.030133632_dp
  bF(0,1,3,2:9,1) = -0.124836752_dp
  bF(0,2,3,1:9,1) = -0.044709383_dp
!
!  The following values were not given in the original paper, 
!  but are present in the spline data of the program.
!
  bF(0,0,0,3:9,1) = 0.00991722_dp
  bF(0,0,1,3:9,1) = 0.00991722_dp
  bF(0,0,2,3:9,1) = 0.00991722_dp
  bF(0,0,3,3:9,1) = 0.00991722_dp
!
  bF(0,0,2,5:9,2) = -0.0090477875161288110_dp
  bF(0,1,3,1:9,2) = -0.213_dp
  bF(0,1,2,1:9,2) = -0.25_dp
  bF(0,1,1,1:9,2) = -0.5_dp
!
  bF(0,1,1,1,3) = 0.249831916_dp
!
  bF(1,2,1,1,1) = -0.052500_dp
  bF(2,1,2,1,1) = -0.052500_dp
  bF(1,2,1,5:9,1) = -0.054376_dp
  bF(2,1,2,5:9,1) = -0.054376_dp
  bF(1,2,3,2:9,1) = 0.062418_dp
  bF(2,3,2,2:9,1) = 0.062418_dp
  bF(3,2,2,4:8,1) = -0.006618_dp
  bF(3,1,1,2,1) = -0.060543_dp
  bF(3,1,2,4,1) = -0.020044_dp
  bF(3,2,1,4,1) = -0.020044_dp
  bF(3,1,2,5,1) = -0.020044_dp
  bF(3,2,1,5,1) = -0.020044_dp
!
!  The following derivatives were not given in the paper,
!  but appear in the program.
!
  bF(1,1,3,2,1) = 0.03754478_dp
  bF(2,3,1,2,1) = 0.03754478_dp
!
!  Evaluate second derivatives of F by finite differences
!
  do n = 1,3
    Fneeded(n) = .true.
    do iz = 2,8
      izp = min(9,iz+1)
      izm = max(1,iz-1)
      do iy = 1,2
        iyp = min(3,iy+1)
        iym = max(0,iy-1)
        do ix = 1,2
          ixp = min(3,ix+1)
          ixm = max(0,ix-1)
!  
!  xy cross derivs
!     
          bF(4,ix,iy,iz,n) = &
            (bF(0,ixp,iyp,iz,n)-bF(0,ixm,iyp,iz,n)-bF(0,ixp,iym,iz,n)+bF(0,ixm,iym,iz,n))/ &
            (dble(ixp-ixm)*dble(iyp-iym))
!    
!  xz cross derivs 
!
          bF(5,ix,iy,iz,n) = &
            (bF(0,ixp,iy,izp,n)-bF(0,ixm,iy,izp,n)-bF(0,ixp,iy,izm,n)+bF(0,ixm,iy,izm,n))/ &
            (dble(ixp-ixm)*dble(izp-izm))
!       
!  yz cross derivs
!       
          bF(6,ix,iy,iz,n) = &
            (bF(0,ix,iyp,izp,n)-bF(0,ix,iym,izp,n)-bF(0,ix,iyp,izm,n)+bF(0,ix,iym,izm,n))/ &
            (dble(iyp-iym)*dble(izp-izm))
!           
!  xyz cross deriv
!  
          bF(7,ix,iy,iz,n) = &
            ((bF(0,ixp,iyp,izp,n)-bF(0,ixp,iym,izp,n)-bF(0,ixp,iyp,izm,n)+bF(0,ixp,iym,izm,n))- &
            (bF(0,ixm,iyp,izp,n)-bF(0,ixm,iym,izp,n)-bF(0,ixm,iyp,izm,n)+bF(0,ixm,iym,izm,n)))/ &
            (dble(ixp-ixm)*dble(iyp-iym)*dble(izp-izm))
        enddo  
      enddo  
    enddo
  enddo
!*****************************************
!  Carbon P values from REBO 2002 paper  *
!*****************************************
  bP_CH(0,1,1,1) = 0.003026697473481_dp
  bP_CH(0,2,1,1) = 0.003179530830731_dp
  bP_CH(0,0,2,1) = 0.007860700254745_dp
  bP_CH(0,1,2,1) = 0.006326248241119_dp
  bP_CH(0,0,3,1) = 0.016125364564267_dp
  bP_CH(0,1,0,2) = 0.01_dp
  bP_CH(0,2,0,2) = -0.1220421462782555_dp
  bP_CH(0,3,0,2) = -0.307584705066_dp
  bP_CH(0,0,1,2) = 0.2093367328250380_dp
  bP_CH(0,1,1,2) = -0.1251234006287090_dp
  bP_CH(0,2,1,2) = -0.3005291724067579_dp
  bP_CH(0,0,2,2) = -0.064449615432525_dp
  bP_CH(0,1,2,2) = -0.298905245783_dp
  bP_CH(0,0,3,2) = -0.303927546346162_dp
!***********************************
!  Values for P from Oxygen paper  *
!***********************************
!
!  NB: The values are the ones used in the REBO code, which are slightly different from the paper
!
!  C - O
!
  bP(0,0,0,4,1) = -0.390_dp
  bP(0,1,0,4,1) = 0.128_dp
  bP(0,2,0,4,1) = 0.066_dp
  bP(0,3,0,4,1) = 0.100_dp
  bP(0,0,1,4,1) = 0.109_dp
  bP(0,1,1,4,1) = 0.173_dp
  bP(0,2,1,4,1) = 0.135_dp
  bP(0,0,2,4,1) = 0.177_dp
  bP(0,1,2,4,1) = 9.689d-5
  bP(0,0,3,4,1) = 0.054_dp
  bP(0,1,3,4,1) = 12.5_dp
!
!  C - C
!
  bP(0,0,1,1,1) = 0.139_dp
  bP(0,1,1,1,1) = 0.350_dp
  bP(0,2,1,1,1) = -0.02_dp
  bP(0,0,2,1,1) = 0.231_dp
  bP(0,1,2,1,1) = -7.662d-5
  bP(0,0,3,1,1) = -6.253d-3
!
!  C - H
!
  bP(0,0,1,2,1) = 0.385_dp
  bP(0,1,1,2,1) = 0.137_dp
  bP(0,2,1,2,1) = -0.229_dp
  bP(0,3,1,2,1) = 1.0_dp
  bP(0,0,2,2,1) = -0.083_dp
  bP(0,1,2,2,1) = -0.238_dp
  bP(0,0,3,2,1) = -0.224_dp
!
!  O - C
!
  bP(0,0,0,4,3) = 0.0_dp
  bP(0,1,0,4,3) = 0.607_dp
  bP(0,2,0,4,3) = 19.057_dp
  bP(0,3,0,4,3) = 19.057_dp
  bP(0,0,1,4,3) = 1.026_dp
  bP(0,1,1,4,3) = 19.057_dp
  bP(0,2,1,4,3) = 19.057_dp
  bP(0,0,2,4,3) = 19.057_dp
  bP(0,1,2,4,3) = 19.057_dp
  bP(0,0,3,4,3) = 19.057_dp
!
!  O - H
!
  bP(0,0,0,5,3) = -0.022_dp
  bP(0,1,0,5,3) = -0.011_dp
  bP(0,2,0,5,3) = 0.075_dp
  bP(0,3,0,5,3) = 0.082_dp
  bP(0,0,1,5,3) = -0.00656_dp
  bP(0,1,1,5,3) = 0.075_dp
  bP(0,2,1,5,3) = 0.082_dp
  bP(0,0,2,5,3) = 0.075_dp
  bP(0,1,2,5,3) = 0.082_dp
  bP(0,0,3,5,3) = 0.082_dp
!
!  H - O
!
  bP(0,0,0,5,2) = -0.019_dp
!
!  O - O
!
  bP(0,0,0,6,3) = -0.036_dp
  bP(0,1,0,6,3) = 1.166d-3
  bP(0,2,0,6,3) = 0.062_dp
  bP(0,3,0,6,3) = 0.071_dp
  bP(0,0,1,6,3) = 0.028_dp
  bP(0,1,1,6,3) = 0.071_dp
  bP(0,2,1,6,3) = 0.071_dp
  bP(0,0,2,6,3) = 0.062_dp
  bP(0,1,2,6,3) = 0.071_dp
  bP(0,0,3,6,3) = 0.071_dp
!
!  First derivatives specified in parameterisation
!
  bP(1,2,2,4,1) = 0.013_dp
  bP(1,2,1,4,1) = 0.228_dp
  bP(1,3,1,4,1) = 0.0425_dp
  bP(2,2,2,4,1) = -0.06395_dp
  bP(2,1,2,4,1) = 0.2835_dp
  bP(2,1,3,4,1) = -0.0275_dp
!
  bP(1,2,2,3,1) = -0.307_dp
  bP(1,2,1,3,1) = 0.0_dp
  bP(1,3,2,3,1) = 0.0_dp
  bP(2,2,2,3,1) = -0.119_dp
  bP(2,1,2,3,1) = -0.415_dp
  bP(2,1,3,3,1) = -0.3045_dp
!
  bP(1,2,2,3,2) = -0.307_dp
  bP(1,2,1,3,2) = 0.0_dp
  bP(1,3,2,3,2) = 0.0_dp
  bP(2,2,2,3,2) = -0.119_dp
  bP(2,1,2,3,2) = -0.415_dp
  bP(2,1,3,3,2) = -0.3045_dp
!
  bP(1,2,2,4,3) = 9.0155_dp
  bP(1,2,1,4,3) = 9.7235_dp
  bP(1,3,2,4,3) = 9.27_dp
  bP(2,2,2,4,3) = 9.27_dp
  bP(2,1,2,4,3) = 9.7235_dp
  bP(2,1,3,4,3) = 9.0155_dp
!
  bP(1,2,2,6,3) = 0.0215_dp
  bP(1,2,1,6,3) = 0.049_dp
  bP(1,3,2,6,3) = 0.034917_dp
  bP(2,2,2,6,3) = 0.034917_dp
  bP(2,1,2,6,3) = 0.049_dp
  bP(2,1,3,6,3) = 0.0215_dp
!
  bP(1,2,2,1,1) = -0.047_dp
  bP(1,2,1,1,1) = 0.0_dp
  bP(1,3,2,1,1) = 0.0_dp
  bP(2,2,2,1,1) = -0.00003831_dp
  bP(2,1,2,1,1) = 0.1155_dp
  bP(2,1,3,1,1) = -0.0726265_dp
!
  bP(1,2,2,5,3) = 0.04428_dp
  bP(1,2,1,5,3) = 0.0485_dp
  bP(1,3,2,5,3) = 0.0465_dp
  bP(2,2,2,5,3) = 0.0465_dp
  bP(2,1,2,5,3) = 0.0485_dp
  bP(2,1,3,5,3) = 0.04428_dp
!
  bP(1,2,2,5,2) = 0.04428_dp
  bP(1,2,1,5,2) = 0.0485_dp
  bP(1,3,2,5,2) = 0.0465_dp
  bP(2,2,2,5,2) = 0.0465_dp
  bP(2,1,2,5,2) = 0.0485_dp
  bP(2,1,3,5,2) = 0.04428_dp
!****************
!  Triads data  *
!****************
  Rhh = 0.7415886997_dp
  Rch = 1.09_dp
  Roh = 0.96_dp
  Rcf = 1.2718_dp
  Rff = 1.4119_dp
  Rfh = 0.9378_dp
!
  balpha(1:nREBOspecies**3) = 0.0_dp
  bexpco(1:nREBOspecies**3) = 1.0_dp
  bdelta(1:nREBOspecies) = 0.5_dp
!
!  Data from REBO 2002 paper
!
  balpha(nREBOspecies2+1) = 4.0_dp
  balpha(nREBOspecies2+2) = 4.0_dp
  balpha(nREBOspecies2+nREBOspecies+1) = 4.0_dp
  balpha(nREBOspecies2+nREBOspecies+2) = 4.0_dp
!
  bexpco(nREBOspecies2+2) = exp(balpha(nREBOspecies2+2)*(Rhh - Rch))
  bexpco(nREBOspecies2+nREBOspecies+1) = exp(-balpha(nREBOspecies2+nREBOspecies+1)*(Rhh - Rch))
!
!  Data from REBO oxygen modifications
!
  balpha(nREBOspecies2+3) = 8.0_dp
  balpha(nREBOspecies2+nREBOspecies+3) = 8.0_dp
  balpha(nREBOspecies2+2*nREBOspecies+1) = 8.0_dp
  balpha(nREBOspecies2+2*nREBOspecies+2) = 8.0_dp
  balpha(nREBOspecies2+2*nREBOspecies+3) = 8.0_dp
!
  bexpco(nREBOspecies2+3) = exp(balpha(nREBOspecies2+3)*(Roh - Rch))
  bexpco(nREBOspecies2+nREBOspecies+3) = exp(balpha(nREBOspecies2+nREBOspecies+3)*(Roh - Rhh))
  bexpco(nREBOspecies2+2*nREBOspecies+1) = exp(balpha(nREBOspecies2+2*nREBOspecies+1)*(Rch - Roh))
  bexpco(nREBOspecies2+2*nREBOspecies+2) = exp(balpha(nREBOspecies2+2*nREBOspecies+2)*(Rhh - Roh))
!
!  Data from REBO fluorine paper
!
  balpha(nREBOspecies2+3*nREBOspecies+1) = 4.0_dp
  balpha(nREBOspecies2+  nREBOspecies+4) = 4.0_dp
  balpha(nREBOspecies2+3*nREBOspecies+4) = 4.0_dp
  balpha(nREBOspecies2+4) = 4.0_dp
  balpha(nREBOspecies2+3*nREBOspecies+1) = 4.0_dp
  balpha(3*nREBOspecies2+  nREBOspecies+2) = 4.0_dp
  balpha(3*nREBOspecies2+  nREBOspecies+1) = 4.0_dp
  balpha(3*nREBOspecies2+  2) = 4.0_dp
  balpha(3*nREBOspecies2+  nREBOspecies+4) = 4.0_dp
  balpha(3*nREBOspecies2+3*nREBOspecies+2) = 4.0_dp
  balpha(3*nREBOspecies2+               4) = 4.0_dp
  balpha(3*nREBOspecies2+3*nREBOspecies+1) = 4.0_dp
  balpha(3*nREBOspecies2+3*nREBOspecies+4) = 4.0_dp
!
  bexpco(nREBOspecies2+3*nREBOspecies+1) = exp(balpha(nREBOspecies2+3*nREBOspecies+1)*(Rhh - Rfh))
  bexpco(nREBOspecies2+nREBOspecies+4) = exp(balpha(nREBOspecies2+nREBOspecies+4)*(Rfh - Rhh))
  bexpco(nREBOspecies2+3*nREBOspecies+4) = exp(balpha(nREBOspecies2+3*nREBOspecies+4)*(Rff - Rff))
  bexpco(nREBOspecies2+4) = exp(balpha(nREBOspecies2+4)*(Rfh - Rch))
  bexpco(nREBOspecies2+3*nREBOspecies+1) = exp(balpha(nREBOspecies2+3*nREBOspecies+1)*(Rch - Rfh))
  bexpco(3*nREBOspecies2+nREBOspecies+2) = exp(balpha(3*nREBOspecies2+nREBOspecies+2)*(Rfh - Rfh))
  bexpco(3*nREBOspecies2+nREBOspecies+1) = exp(balpha(3*nREBOspecies2+nREBOspecies+1)*(Rcf - Rfh))
  bexpco(3*nREBOspecies2+2) = exp(balpha(3*nREBOspecies2+2)*(Rfh - Rcf))
  bexpco(3*nREBOspecies2+nREBOspecies+4) = exp(balpha(3*nREBOspecies2+nREBOspecies+4)*(Rff - Rfh))
  bexpco(3*nREBOspecies2+3*nREBOspecies+2) = exp(balpha(3*nREBOspecies2+3*nREBOspecies+2)*(Rfh - Rff))
  bexpco(3*nREBOspecies2+4) = exp(balpha(3*nREBOspecies2+4)*(Rff - Rcf))
  bexpco(3*nREBOspecies2+3*nREBOspecies+1) = exp(balpha(3*nREBOspecies2+3*nREBOspecies+1)*(Rcf - Rff))
  bexpco(3*nREBOspecies2+3*nREBOspecies+4) = exp(balpha(3*nREBOspecies2+3*nREBOspecies+4)*(Rff - Rff))
!*********************
!  C centred angles  *
!*********************
!
!  Construct sixth order polynomial splines for G and L w.r.t. cos(theta)
!
  bcos(1,1) = 1.0_dp
  bcos(2,1) = 0.5_dp
  bcos(3,1) = 0.0_dp
  bcos(4,1) = cos(0.6082_dp*pi)
  bcos(5,1) = -0.5_dp
  bcos(6,1) = -1.0_dp
!
!  Carbon : Region 1 : 0 -> 0.6082*pi
!
!  rhs 1 = G(cos(theta)) at 0
!  rhs 2 = G(cos(theta)) at pi/3
!  rhs 3 = G(cos(theta)) at pi/2
!  rhs 4 = G(cos(theta)) at 0.6082*pi
!  rhs 5 = dG(cos(theta))/d(cos(theta)) at 0.6082*pi
!  rhs 6 = d2G(cos(theta))/d(cos(theta))2 at 0.6082*pi
!
  rhs(1) = 8.0_dp
  rhs(2) = 2.0014_dp
  rhs(3) = 0.37545_dp
  rhs(4) = 0.09733_dp
  rhs(5) = 0.400_dp
  rhs(6) = 1.98_dp
!
  matrix(1:6,1:6) = 0.0_dp
  matrix(1:4,1) = 1.0_dp
  matrix(1:4,2) = bcos(1:4,1)
  matrix(5,2)   = 1.0_dp
  matrix(1:4,3) = bcos(1:4,1)**2
  matrix(5,3)   = 2.0_dp*bcos(4,1)
  matrix(6,3)   = 2.0_dp
  matrix(1:4,4) = bcos(1:4,1)**3
  matrix(5,4)   = 3.0_dp*bcos(4,1)**2
  matrix(6,4)   = 6.0_dp*bcos(4,1)
  matrix(1:4,5) = bcos(1:4,1)**4
  matrix(5,5)   = 4.0_dp*bcos(4,1)**3
  matrix(6,5)   = 12.0_dp*bcos(4,1)**2
  matrix(1:4,6) = bcos(1:4,1)**5
  matrix(5,6)   = 5.0_dp*bcos(4,1)**4
  matrix(6,6)   = 20.0_dp*bcos(4,1)**3
!
  call matinv(matrix,8_i4,6_i4,wrk,ifail)
  if (ifail.ne.0) then
    call outerror('initialisation of Brenner potentials has failed',0_i4)
    call stopnow('setbrenner')
  endif
!
  do i = 1,6
    do j = 1,6
      bGcosCspecial(i,1) = bGcosCspecial(i,1) + matrix(i,j)*rhs(j)
    enddo
  enddo
!         
!  Carbon : Region 2 : 0.6082*pi -> 2*pi/3
!  
!  rhs 1 = G(cos(theta)) at 0.6082*pi
!  rhs 2 = dG(cos(theta))/d(cos(theta)) at 0.6082*pi
!  rhs 3 = d2G(cos(theta))/d(cos(theta))2 at 0.6082*pi
!  rhs 4 = G(cos(theta)) at 2*pi/3
!  rhs 5 = dG(cos(theta))/d(cos(theta)) at 2*pi/3
!  rhs 6 = d2G(cos(theta))/d(cos(theta))2 at 2*pi/3
!     
  rhs(1) = 0.09733_dp  
  rhs(2) = 0.400_dp
  rhs(3) = 1.98_dp
  rhs(4) = 0.05280_dp
  rhs(5) = 0.170_dp
  rhs(6) = 0.37_dp
!
  matrix(1:6,1:6) = 0.0_dp
  matrix(1,1) = 1.0_dp
  matrix(4,1) = 1.0_dp
  matrix(1,2) = bcos(4,1)
  matrix(2,2) = 1.0_dp
  matrix(4,2) = bcos(5,1)
  matrix(5,2) = 1.0_dp
  matrix(1,3) = bcos(4,1)**2
  matrix(2,3) = 2.0_dp*bcos(4,1)
  matrix(3,3) = 2.0_dp
  matrix(4,3) = bcos(5,1)**2
  matrix(5,3) = 2.0_dp*bcos(5,1)
  matrix(6,3) = 2.0_dp
  matrix(1,4) = bcos(4,1)**3
  matrix(2,4) = 3.0_dp*bcos(4,1)**2
  matrix(3,4) = 6.0_dp*bcos(4,1)
  matrix(4,4) = bcos(5,1)**3
  matrix(5,4) = 3.0_dp*bcos(5,1)**2
  matrix(6,4) = 6.0_dp*bcos(5,1)
  matrix(1,5) = bcos(4,1)**4
  matrix(2,5) = 4.0_dp*bcos(4,1)**3
  matrix(3,5) = 12.0_dp*bcos(4,1)**2
  matrix(4,5) = bcos(5,1)**4
  matrix(5,5) = 4.0_dp*bcos(5,1)**3
  matrix(6,5) = 12.0_dp*bcos(5,1)**2
  matrix(1,6) = bcos(4,1)**5
  matrix(2,6) = 5.0_dp*bcos(4,1)**4
  matrix(3,6) = 20.0_dp*bcos(4,1)**3
  matrix(4,6) = bcos(5,1)**5
  matrix(5,6) = 5.0_dp*bcos(5,1)**4
  matrix(6,6) = 20.0_dp*bcos(5,1)**3
!
  call matinv(matrix,8_i4,6_i4,wrk,ifail)
  if (ifail.ne.0) then
    call outerror('initialisation of Brenner potentials has failed',0_i4)
    call stopnow('setbrenner')
  endif
!
  do i = 1,6
    do j = 1,6
      bGcosCspecial(i,2) = bGcosCspecial(i,2) + matrix(i,j)*rhs(j)
    enddo
  enddo
!         
!  Carbon : Region 3 : 2*pi/3 -> pi
!  
!  rhs 1 = G(cos(theta)) at 2*pi/3
!  rhs 2 = dG(cos(theta))/d(cos(theta)) at 2*pi/3
!  rhs 3 = d2G(cos(theta))/d(cos(theta))2 at 2*pi/3
!  rhs 4 = G(cos(theta)) at pi
!  rhs 5 = dG(cos(theta))/d(cos(theta)) at pi
!  rhs 6 = d2G(cos(theta))/d(cos(theta))2 at pi
!     
  rhs(1) = 0.05280_dp
  rhs(2) = 0.170_dp
  rhs(3) = 0.37_dp
  rhs(4) = -0.01_dp
  rhs(5) = 0.104_dp
  rhs(6) = 0.0_dp
!
  matrix(1:6,1:6) = 0.0_dp
  matrix(1,1) = 1.0_dp
  matrix(4,1) = 1.0_dp
  matrix(1,2) = bcos(5,1)
  matrix(2,2) = 1.0_dp
  matrix(4,2) = bcos(6,1)
  matrix(5,2) = 1.0_dp
  matrix(1,3) = bcos(5,1)**2
  matrix(2,3) = 2.0_dp*bcos(5,1)
  matrix(3,3) = 2.0_dp
  matrix(4,3) = bcos(6,1)**2
  matrix(5,3) = 2.0_dp*bcos(6,1)
  matrix(6,3) = 2.0_dp
  matrix(1,4) = bcos(5,1)**3
  matrix(2,4) = 3.0_dp*bcos(5,1)**2
  matrix(3,4) = 6.0_dp*bcos(5,1)
  matrix(4,4) = bcos(6,1)**3
  matrix(5,4) = 3.0_dp*bcos(6,1)**2
  matrix(6,4) = 6.0_dp*bcos(6,1)
  matrix(1,5) = bcos(5,1)**4
  matrix(2,5) = 4.0_dp*bcos(5,1)**3
  matrix(3,5) = 12.0_dp*bcos(5,1)**2
  matrix(4,5) = bcos(6,1)**4
  matrix(5,5) = 4.0_dp*bcos(6,1)**3
  matrix(6,5) = 12.0_dp*bcos(6,1)**2
  matrix(1,6) = bcos(5,1)**5
  matrix(2,6) = 5.0_dp*bcos(5,1)**4
  matrix(3,6) = 20.0_dp*bcos(5,1)**3
  matrix(4,6) = bcos(6,1)**5
  matrix(5,6) = 5.0_dp*bcos(6,1)**4
  matrix(6,6) = 20.0_dp*bcos(6,1)**3
!
  call matinv(matrix,8_i4,6_i4,wrk,ifail)
  if (ifail.ne.0) then
    call outerror('initialisation of Brenner potentials has failed',0_i4)
    call stopnow('setbrenner')
  endif
!
  do i = 1,6
    do j = 1,6
      bGcos(i,1) = bGcos(i,1) + matrix(i,j)*rhs(j)
    enddo
  enddo
!
!  C centred angles : Lamba term
!
!  Region : 0 -> 0.6082*pi
!
!  rhs 1 = L(cos(theta)) at 0
!  rhs 2 = L(cos(theta)) at pi/3
!  rhs 3 = L(cos(theta)) at pi/2
!  rhs 4 = G(cos(theta)) at 0.6082*pi
!  rhs 5 = dG(cos(theta))/d(cos(theta)) at 0.6082*pi
!  rhs 6 = d2G(cos(theta))/d(cos(theta))2 at 0.6082*pi
!
  rhs(1) = 1.0_dp
  rhs(2) = 0.416335_dp
  rhs(3) = 0.271856_dp
  rhs(4) = 0.09733_dp
  rhs(5) = 0.400_dp
  rhs(6) = 1.98_dp
!
  matrix(1:6,1:6) = 0.0_dp
  matrix(1:4,1) = 1.0_dp
  matrix(1:4,2) = bcos(1:4,1)
  matrix(5,2)   = 1.0_dp
  matrix(1:4,3) = bcos(1:4,1)**2
  matrix(5,3)   = 2.0_dp*bcos(4,1)
  matrix(6,3)   = 2.0_dp
  matrix(1:4,4) = bcos(1:4,1)**3
  matrix(5,4)   = 3.0_dp*bcos(4,1)**2
  matrix(6,4)   = 6.0_dp*bcos(4,1)
  matrix(1:4,5) = bcos(1:4,1)**4
  matrix(5,5)   = 4.0_dp*bcos(4,1)**3
  matrix(6,5)   = 12.0_dp*bcos(4,1)**2
  matrix(1:4,6) = bcos(1:4,1)**5
  matrix(5,6)   = 5.0_dp*bcos(4,1)**4
  matrix(6,6)   = 20.0_dp*bcos(4,1)**3
!
  call matinv(matrix,8_i4,6_i4,wrk,ifail)
  if (ifail.ne.0) then
    call outerror('initialisation of Brenner potentials has failed',0_i4)
    call stopnow('setbrenner')
  endif
!
  do i = 1,6
    do j = 1,6
      bLcos(i,1) = bLcos(i,1) + matrix(i,j)*rhs(j)
    enddo
  enddo
!*********************
!  H centred angles  *
!*********************
!
!  Construct sixth order polynomial splines for G w.r.t. cos(theta)
!
  bcos(1,2) = 1.0_dp
  bcos(2,2) = 0.5_dp
  bcos(3,2) = 0.0_dp
  bcos(4,2) = -0.5_dp
  bcos(5,2) = -0.5_dp*sqrt(3.0_dp)
  bcos(6,2) = -1.0_dp
!
!  Hydrogen : Region : 0 -> pi
!
!  rhs 1 = G(cos(theta)) at   0 degrees
!  rhs 2 = G(cos(theta)) at  60 degrees
!  rhs 3 = G(cos(theta)) at  90 degrees
!  rhs 4 = G(cos(theta)) at 120 degrees
!  rhs 5 = G(cos(theta)) at 150 degrees
!  rhs 6 = G(cos(theta)) at 180 degrees
!  rhs 7 = dG(cos(theta))/dcos(theta) at 0 degrees
!  rhs 8 = dG(cos(theta))/dcos(theta) at 180 degrees
!
!  NOTE : The constraint on derivatives has been added
!  relative to what has been given in the Brenner potl
!  paper.
!
  rhs(1) = 19.991787_dp
  rhs(2) = 19.704059_dp
  rhs(3) = 19.065124_dp
  rhs(4) = 16.811574_dp
  rhs(5) = 12.164186_dp
  rhs(6) = 11.235870_dp
  rhs(7) = 0.0_dp
  rhs(8) = 0.0_dp
!
  matrix(1:8,1:8) = 0.0_dp
  matrix(1:6,1) = 1.0_dp
  matrix(1:6,2) = bcos(1:6,2)
  matrix(7:8,2) = 1.0_dp
  matrix(1:6,3) = bcos(1:6,2)**2
  matrix(7,3)   = 2.0_dp*bcos(1,2)
  matrix(8,3)   = 2.0_dp*bcos(6,2)
  matrix(1:6,4) = bcos(1:6,2)**3
  matrix(7,4)   = 3.0_dp*bcos(1,2)**2
  matrix(8,4)   = 3.0_dp*bcos(6,2)**2
  matrix(1:6,5) = bcos(1:6,2)**4
  matrix(7,5)   = 4.0_dp*bcos(1,2)**3
  matrix(8,5)   = 4.0_dp*bcos(6,2)**3
  matrix(1:6,6) = bcos(1:6,2)**5
  matrix(7,6)   = 5.0_dp*bcos(1,2)**4
  matrix(8,6)   = 5.0_dp*bcos(6,2)**4
  matrix(1:6,7) = bcos(1:6,2)**6
  matrix(7,7)   = 6.0_dp*bcos(1,2)**5
  matrix(8,7)   = 6.0_dp*bcos(6,2)**5
  matrix(1:6,8) = bcos(1:6,2)**7
  matrix(7,8)   = 7.0_dp*bcos(1,2)**6
  matrix(8,8)   = 7.0_dp*bcos(6,2)**6
!
  call matinv(matrix,8_i4,8_i4,wrk,ifail)
  if (ifail.ne.0) then
    call outerror('initialisation of Brenner potentials has failed',0_i4)
    call stopnow('setbrenner')
  endif
!
  do i = 1,8
    do j = 1,8
      bGcos(i,2) = bGcos(i,2) + matrix(i,j)*rhs(j)
    enddo
  enddo
!*********************
!  O centred angles  *
!*********************
!
!  Parameters from paper : 
!
!  a0 = -0.014
!  a1 = 0.07
!  a2 = -0.478
!
!  convoluted to form polynomial expansion coefficients
!
  bGcos(1,3) = 0.00199388_dp
  bGcos(2,3) = 0.066920_dp
  bGcos(3,3) = 0.07_dp
!
!  Apply relationships to F
!
  do n = 1,3
    do i = 1,9
      do j = 1,3
        do k = 0,j-1
          bF(0,j,k,i,n) = bF(0,k,j,i,n)
        enddo
      enddo
    enddo
  enddo
!
!  Set up interpolations
!
  call setbicubic
  call settricubic
  call settorcubic
!
!  Set up terms dependent on parameters for convenience
!
  do n = 1,nREBOspecies*(nREBOspecies+1)/2
    bR22(n) = bR2(n)**2
  enddo
!
  return
  end
!*******************
!  Bicubic set up  *
!*******************
  subroutine setbicubic
!
!  Subroutine to initialise the bicubic spline coefficients for the
!  Brenner potential H term.
!
  use datatypes
  use brennerdata, only : cobicubic, pbicubic
  implicit none
  integer(i4) :: i
  integer(i4) :: ind
  integer(i4) :: j
!
!  Initialise array to zero
!
  cobicubic(1:16,0:9,0:9,1:2) = 0.0_dp
!
!  Set non-zero values
!
  cobicubic( 1,0,0,1) =    0.075667436837_dp
  cobicubic( 2,0,0,1) =   -0.18160184841_dp
  cobicubic( 3,0,0,1) =    0.13620138631_dp
  cobicubic( 4,0,0,1) =   -0.030266974735_dp
  cobicubic( 5,0,0,1) =   -0.18160184841_dp
  cobicubic( 6,0,0,1) =    0.43584443618_dp
  cobicubic( 7,0,0,1) =   -0.32688332714_dp
  cobicubic( 8,0,0,1) =    0.072640739364_dp
  cobicubic( 9,0,0,1) =    0.13620138631_dp
  cobicubic(10,0,0,1) =   -0.32688332714_dp
  cobicubic(11,0,0,1) =    0.24516249535_dp
  cobicubic(12,0,0,1) =   -0.054480554523_dp
  cobicubic(13,0,0,1) =   -0.030266974735_dp
  cobicubic(14,0,0,1) =    0.072640739364_dp
  cobicubic(15,0,0,1) =   -0.054480554523_dp
  cobicubic(16,0,0,1) =    0.012106789894_dp
  cobicubic( 1,0,1,1) =    0.036530157382_dp
  cobicubic( 2,0,1,1) =   -0.027510004305_dp
  cobicubic( 3,0,1,1) =    0.011462501794_dp
  cobicubic( 4,0,1,1) =   -0.0015283335725_dp
  cobicubic( 5,0,1,1) =   -0.087672377718_dp
  cobicubic( 6,0,1,1) =    0.066024010332_dp
  cobicubic( 7,0,1,1) =   -0.027510004305_dp
  cobicubic( 8,0,1,1) =    0.0036680005740_dp
  cobicubic( 9,0,1,1) =    0.065754283288_dp
  cobicubic(10,0,1,1) =   -0.049518007749_dp
  cobicubic(11,0,1,1) =    0.020632503229_dp
  cobicubic(12,0,1,1) =   -0.0027510004305_dp
  cobicubic(13,0,1,1) =   -0.014612062953_dp
  cobicubic(14,0,1,1) =    0.011004001722_dp
  cobicubic(15,0,1,1) =   -0.0045850007175_dp
  cobicubic(16,0,1,1) =    0.00061133342900_dp
  cobicubic( 1,0,2,1) =   -1.2718123323_dp
  cobicubic( 2,0,2,1) =    1.1446310991_dp
  cobicubic( 3,0,2,1) =   -0.33385073723_dp
  cobicubic( 4,0,2,1) =    0.031795308307_dp
  cobicubic( 5,0,2,1) =    3.0523495975_dp
  cobicubic( 6,0,2,1) =   -2.7471146378_dp
  cobicubic( 7,0,2,1) =    0.80124176934_dp
  cobicubic( 8,0,2,1) =   -0.076308739938_dp
  cobicubic( 9,0,2,1) =   -2.2892621981_dp
  cobicubic(10,0,2,1) =    2.0603359783_dp
  cobicubic(11,0,2,1) =   -0.60093132701_dp
  cobicubic(12,0,2,1) =    0.057231554953_dp
  cobicubic(13,0,2,1) =    0.50872493292_dp
  cobicubic(14,0,2,1) =   -0.45785243963_dp
  cobicubic(15,0,2,1) =    0.13354029489_dp
  cobicubic(16,0,2,1) =   -0.012718123323_dp
  cobicubic( 1,1,0,1) =   -0.40332783369_dp
  cobicubic( 2,1,0,1) =    1.4962258580_dp
  cobicubic( 3,1,0,1) =   -1.1221693935_dp
  cobicubic( 4,1,0,1) =    0.24937097633_dp
  cobicubic( 5,1,0,1) =    0.53802169851_dp
  cobicubic( 6,1,0,1) =   -1.9704165784_dp
  cobicubic( 7,1,0,1) =    1.4778124338_dp
  cobicubic( 8,1,0,1) =   -0.32840276307_dp
  cobicubic( 9,1,0,1) =   -0.22417570771_dp
  cobicubic(10,1,0,1) =    0.82100690768_dp
  cobicubic(11,1,0,1) =   -0.61575518076_dp
  cobicubic(12,1,0,1) =    0.13683448461_dp
  cobicubic(13,1,0,1) =    0.029890094362_dp
  cobicubic(14,1,0,1) =   -0.10946758769_dp
  cobicubic(15,1,0,1) =    0.082100690768_dp
  cobicubic(16,1,0,1) =   -0.018244597948_dp
  cobicubic( 1,1,1,1) =   -4.9799065202_dp
  cobicubic( 2,1,1,1) =    6.5254122503_dp
  cobicubic( 3,1,1,1) =   -2.7189217710_dp
  cobicubic( 4,1,1,1) =    0.36252290279_dp
  cobicubic( 5,1,1,1) =    6.4121304235_dp
  cobicubic( 6,1,1,1) =   -8.3968897515_dp
  cobicubic( 7,1,1,1) =    3.4987040631_dp
  cobicubic( 8,1,1,1) =   -0.46649387508_dp
  cobicubic( 9,1,1,1) =   -2.6717210098_dp
  cobicubic(10,1,1,1) =    3.4987040631_dp
  cobicubic(11,1,1,1) =   -1.4577933596_dp
  cobicubic(12,1,1,1) =    0.19437244795_dp
  cobicubic(13,1,1,1) =    0.35622946797_dp
  cobicubic(14,1,1,1) =   -0.46649387508_dp
  cobicubic(15,1,1,1) =    0.19437244795_dp
  cobicubic(16,1,1,1) =   -0.025916326393_dp
  cobicubic( 1,1,2,1) =    6.8677865944_dp
  cobicubic( 2,1,2,1) =   -6.1810079349_dp
  cobicubic( 3,1,2,1) =    1.8027939810_dp
  cobicubic( 4,1,2,1) =   -0.17169466486_dp
  cobicubic( 5,1,2,1) =   -9.1570487925_dp
  cobicubic( 6,1,2,1) =    8.2413439133_dp
  cobicubic( 7,1,2,1) =   -2.4037253080_dp
  cobicubic( 8,1,2,1) =    0.22892621981_dp
  cobicubic( 9,1,2,1) =    3.8154369969_dp
  cobicubic(10,1,2,1) =   -3.4338932972_dp
  cobicubic(11,1,2,1) =    1.0015522117_dp
  cobicubic(12,1,2,1) =   -0.095385924922_dp
  cobicubic(13,1,2,1) =   -0.50872493292_dp
  cobicubic(14,1,2,1) =    0.45785243963_dp
  cobicubic(15,1,2,1) =   -0.13354029489_dp
  cobicubic(16,1,2,1) =    0.012718123323_dp
  cobicubic( 1,2,0,1) =   -5.2396933338_dp
  cobicubic( 2,2,0,1) =    14.200780423_dp
  cobicubic( 3,2,0,1) =   -10.650585318_dp
  cobicubic( 4,2,0,1) =    2.3667967372_dp
  cobicubic( 5,2,0,1) =    4.6576726879_dp
  cobicubic( 6,2,0,1) =   -12.606548444_dp
  cobicubic( 7,2,0,1) =    9.4549113328_dp
  cobicubic( 8,2,0,1) =   -2.1010914073_dp
  cobicubic( 9,2,0,1) =   -1.3584878673_dp
  cobicubic(10,2,0,1) =    3.6769099628_dp
  cobicubic(11,2,0,1) =   -2.7576824721_dp
  cobicubic(12,2,0,1) =    0.61281832713_dp
  cobicubic(13,2,0,1) =    0.12937979689_dp
  cobicubic(14,2,0,1) =   -0.35018190122_dp
  cobicubic(15,2,0,1) =    0.26263642591_dp
  cobicubic(16,2,0,1) =   -0.058363650203_dp
  cobicubic( 1,2,1,1) =    13.664696201_dp
  cobicubic( 2,2,1,1) =   -18.219594934_dp
  cobicubic( 3,2,1,1) =    7.5914978893_dp
  cobicubic( 4,2,1,1) =   -1.0121997186_dp
  cobicubic( 5,2,1,1) =   -12.298226581_dp
  cobicubic( 6,2,1,1) =    16.397635441_dp
  cobicubic( 7,2,1,1) =   -6.8323481004_dp
  cobicubic( 8,2,1,1) =    0.91097974672_dp
  cobicubic( 9,2,1,1) =    3.5869827527_dp
  cobicubic(10,2,1,1) =   -4.7826436703_dp
  cobicubic(11,2,1,1) =    1.9927681960_dp
  cobicubic(12,2,1,1) =   -0.26570242613_dp
  cobicubic(13,2,1,1) =   -0.34161740502_dp
  cobicubic(14,2,1,1) =    0.45548987336_dp
  cobicubic(15,2,1,1) =   -0.18978744723_dp
  cobicubic(16,2,1,1) =    0.025304992964_dp
  cobicubic( 1,3,0,1) =    11.287755195_dp
  cobicubic( 2,3,0,1) =   -33.863265585_dp
  cobicubic( 3,3,0,1) =    25.397449189_dp
  cobicubic( 4,3,0,1) =   -5.6438775975_dp
  cobicubic( 5,3,0,1) =   -7.7401749908_dp
  cobicubic( 6,3,0,1) =    23.220524973_dp
  cobicubic( 7,3,0,1) =   -17.415393729_dp
  cobicubic( 8,3,0,1) =    3.8700874954_dp
  cobicubic( 9,3,0,1) =    1.7415393729_dp
  cobicubic(10,3,0,1) =   -5.2246181188_dp
  cobicubic(11,3,0,1) =    3.9184635891_dp
  cobicubic(12,3,0,1) =   -0.87076968647_dp
  cobicubic(13,3,0,1) =   -0.12900291651_dp
  cobicubic(14,3,0,1) =    0.38700874954_dp
  cobicubic(15,3,0,1) =   -0.29025656216_dp
  cobicubic(16,3,0,1) =    0.064501458257_dp
  cobicubic( 1,0,0,2) =   -3.4209639171_dp
  cobicubic( 2,0,0,2) =    10.467421242_dp
  cobicubic( 3,0,0,2) =   -7.5318997411_dp
  cobicubic( 4,0,0,2) =    1.5321260799_dp
  cobicubic( 5,0,0,2) =    8.1758607783_dp
  cobicubic( 6,0,0,2) =   -25.039124687_dp
  cobicubic( 7,0,0,2) =    18.014544657_dp
  cobicubic( 8,0,0,2) =   -3.6633215426_dp
  cobicubic( 9,0,0,2) =   -5.9388298051_dp
  cobicubic(10,0,0,2) =    18.315985647_dp
  cobicubic(11,0,0,2) =   -13.163390091_dp
  cobicubic(12,0,0,2) =    2.6702648455_dp
  cobicubic(13,0,0,2) =    1.2339329440_dp
  cobicubic(14,0,0,2) =   -3.8642822020_dp
  cobicubic(15,0,0,2) =    2.7707451752_dp
  cobicubic(16,0,0,2) =   -0.55906938280_dp
  cobicubic( 1,0,1,2) =   -3.4522134912_dp
  cobicubic( 2,0,1,2) =    7.0743092416_dp
  cobicubic( 3,0,1,2) =   -4.1153505597_dp
  cobicubic( 4,0,1,2) =    0.67603568617_dp
  cobicubic( 5,0,1,2) =    4.8433440656_dp
  cobicubic( 6,0,1,2) =   -12.779585964_dp
  cobicubic( 7,0,1,2) =    8.2543934685_dp
  cobicubic( 8,0,1,2) =   -1.4315660399_dp
  cobicubic( 9,0,1,2) =   -4.6750632537_dp
  cobicubic(10,0,1,2) =    10.974763079_dp
  cobicubic(11,0,1,2) =   -6.7699924372_dp
  cobicubic(12,0,1,2) =    1.1509008414_dp
  cobicubic(13,0,1,2) =    1.5022608139_dp
  cobicubic(14,0,1,2) =   -3.0566467313_dp
  cobicubic(15,0,1,2) =    1.7618638020_dp
  cobicubic(16,0,1,2) =   -0.29007854762_dp
  cobicubic( 1,0,2,2) =    150.32749491_dp
  cobicubic( 2,0,2,2) =   -136.21998259_dp
  cobicubic( 3,0,2,2) =    40.154274529_dp
  cobicubic( 4,0,2,2) =   -3.8544627842_dp
  cobicubic( 5,0,2,2) =   -378.85548335_dp
  cobicubic( 6,0,2,2) =    343.74564654_dp
  cobicubic( 7,0,2,2) =   -101.52948573_dp
  cobicubic( 8,0,2,2) =    9.7602133183_dp
  cobicubic( 9,0,2,2) =    284.14161251_dp
  cobicubic(10,0,2,2) =   -257.80923490_dp
  cobicubic(11,0,2,2) =    76.147114295_dp
  cobicubic(12,0,2,2) =   -7.3201599887_dp
  cobicubic(13,0,2,2) =   -63.142580558_dp
  cobicubic(14,0,2,2) =    57.290941089_dp
  cobicubic(15,0,2,2) =   -16.921580954_dp
  cobicubic(16,0,2,2) =    1.6267022197_dp
  cobicubic( 1,0,3,2) =   -215.30929355_dp
  cobicubic( 2,0,3,2) =    147.64065843_dp
  cobicubic( 3,0,3,2) =   -33.219148147_dp
  cobicubic( 4,0,3,2) =    2.4606776405_dp
  cobicubic( 5,0,3,2) =    645.92788064_dp
  cobicubic( 6,0,3,2) =   -442.92197530_dp
  cobicubic( 7,0,3,2) =    99.657444442_dp
  cobicubic( 8,0,3,2) =   -7.3820329216_dp
  cobicubic( 9,0,3,2) =   -484.44591048_dp
  cobicubic(10,0,3,2) =    332.19148147_dp
  cobicubic(11,0,3,2) =   -74.743083331_dp
  cobicubic(12,0,3,2) =    5.5365246912_dp
  cobicubic(13,0,3,2) =    107.65464677_dp
  cobicubic(14,0,3,2) =   -73.820329216_dp
  cobicubic(15,0,3,2) =    16.609574074_dp
  cobicubic(16,0,3,2) =   -1.2303388203_dp
  cobicubic( 1,1,0,2) =   -7.3123373323_dp
  cobicubic( 2,1,0,2) =    8.4212187176_dp
  cobicubic( 3,1,0,2) =   -8.0367114683_dp
  cobicubic( 4,1,0,2) =    2.5507347397_dp
  cobicubic( 5,1,0,2) =    10.417602097_dp
  cobicubic( 6,1,0,2) =   -13.037295962_dp
  cobicubic( 7,1,0,2) =    12.072368545_dp
  cobicubic( 8,1,0,2) =   -3.7024803760_dp
  cobicubic( 9,1,0,2) =   -5.2620410623_dp
  cobicubic(10,1,0,2) =    7.8488088152_dp
  cobicubic(11,1,0,2) =   -6.8426051837_dp
  cobicubic(12,1,0,2) =    1.9454671841_dp
  cobicubic(13,1,0,2) =    0.82152491983_dp
  cobicubic(14,1,0,2) =   -1.3753756520_dp
  cobicubic(15,1,0,2) =    1.1589982153_dp
  cobicubic(16,1,0,2) =   -0.31420692621_dp
  cobicubic( 1,1,1,2) =    162.99607140_dp
  cobicubic( 2,1,1,2) =   -227.15861415_dp
  cobicubic( 3,1,1,2) =    99.811814853_dp
  cobicubic( 4,1,1,2) =   -13.767121295_dp
  cobicubic( 5,1,1,2) =   -219.07816880_dp
  cobicubic( 6,1,1,2) =    305.21158368_dp
  cobicubic( 7,1,1,2) =   -134.05468292_dp
  cobicubic( 8,1,1,2) =    18.485796809_dp
  cobicubic( 9,1,1,2) =    94.410235947_dp
  cobicubic(10,1,1,2) =   -131.34171402_dp
  cobicubic(11,1,1,2) =    57.593709890_dp
  cobicubic(12,1,1,2) =   -7.9340942713_dp
  cobicubic(13,1,1,2) =   -12.866046181_dp
  cobicubic(14,1,1,2) =    17.882914830_dp
  cobicubic(15,1,1,2) =   -7.8336139416_dp
  cobicubic(16,1,1,2) =    1.0784729192_dp
  cobicubic( 1,1,2,2) =   -649.14301240_dp
  cobicubic( 2,1,2,2) =    584.22871116_dp
  cobicubic( 3,1,2,2) =   -170.40004075_dp
  cobicubic( 4,1,2,2) =    16.228575310_dp
  cobicubic( 5,1,2,2) =    865.52401653_dp
  cobicubic( 6,1,2,2) =   -778.97161488_dp
  cobicubic( 7,1,2,2) =    227.20005434_dp
  cobicubic( 8,1,2,2) =   -21.638100413_dp
  cobicubic( 9,1,2,2) =   -360.63500689_dp
  cobicubic(10,1,2,2) =    324.57150620_dp
  cobicubic(11,1,2,2) =   -94.666689308_dp
  cobicubic(12,1,2,2) =    9.0158751722_dp
  cobicubic(13,1,2,2) =    48.084667585_dp
  cobicubic(14,1,2,2) =   -43.276200827_dp
  cobicubic(15,1,2,2) =    12.622225241_dp
  cobicubic(16,1,2,2) =   -1.2021166896_dp
  cobicubic( 1,2,0,2) =    148.13737559_dp
  cobicubic( 2,2,0,2) =   -372.67486778_dp
  cobicubic( 3,2,0,2) =    279.50615084_dp
  cobicubic( 4,2,0,2) =   -62.112477964_dp
  cobicubic( 5,2,0,2) =   -135.51439025_dp
  cobicubic( 6,2,0,2) =    341.97963766_dp
  cobicubic( 7,2,0,2) =   -256.48472825_dp
  cobicubic( 8,2,0,2) =    56.996606277_dp
  cobicubic( 9,2,0,2) =    40.209382862_dp
  cobicubic(10,2,0,2) =   -101.79711810_dp
  cobicubic(11,2,0,2) =    76.347838577_dp
  cobicubic(12,2,0,2) =   -16.966186350_dp
  cobicubic(13,2,0,2) =   -3.8783473468_dp
  cobicubic(14,2,0,2) =    9.8416105657_dp
  cobicubic(15,2,0,2) =   -7.3812079243_dp
  cobicubic(16,2,0,2) =    1.6402684276_dp
  cobicubic( 1,2,1,2) =   -645.63533089_dp
  cobicubic( 2,2,1,2) =    860.84710785_dp
  cobicubic( 3,2,1,2) =   -358.68629494_dp
  cobicubic( 4,2,1,2) =    47.824839325_dp
  cobicubic( 5,2,1,2) =    581.07179780_dp
  cobicubic( 6,2,1,2) =   -774.76239707_dp
  cobicubic( 7,2,1,2) =    322.81766545_dp
  cobicubic( 8,2,1,2) =   -43.042355393_dp
  cobicubic( 9,2,1,2) =   -169.47927436_dp
  cobicubic(10,2,1,2) =    225.97236581_dp
  cobicubic(11,2,1,2) =   -94.155152422_dp
  cobicubic(12,2,1,2) =    12.554020323_dp
  cobicubic(13,2,1,2) =    16.140883272_dp
  cobicubic(14,2,1,2) =   -21.521177696_dp
  cobicubic(15,2,1,2) =    8.9671573735_dp
  cobicubic(16,2,1,2) =   -1.1956209831_dp
  cobicubic( 1,3,0,2) =   -212.74928244_dp
  cobicubic( 2,3,0,2) =    638.24784733_dp
  cobicubic( 3,3,0,2) =   -478.68588550_dp
  cobicubic( 4,3,0,2) =    106.37464122_dp
  cobicubic( 5,3,0,2) =    145.88522225_dp
  cobicubic( 6,3,0,2) =   -437.65566674_dp
  cobicubic( 7,3,0,2) =    328.24175005_dp
  cobicubic( 8,3,0,2) =   -72.942611123_dp
  cobicubic( 9,3,0,2) =   -32.824175005_dp
  cobicubic(10,3,0,2) =    98.472525016_dp
  cobicubic(11,3,0,2) =   -73.854393762_dp
  cobicubic(12,3,0,2) =    16.412087503_dp
  cobicubic(13,3,0,2) =    2.4314203708_dp
  cobicubic(14,3,0,2) =   -7.2942611123_dp
  cobicubic(15,3,0,2) =    5.4706958342_dp
  cobicubic(16,3,0,2) =   -1.2157101854_dp
!
!  Initialise exponents
!
  ind = 0
  do i = 1,4
    do j = 1,4
      ind = ind + 1 
      pbicubic(ind,1) = j - 1
      pbicubic(ind,2) = i - 1
    enddo
  enddo
!
  return
  end
