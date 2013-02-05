  subroutine setbrenner1
!
!  Subroutine sets the parameters for the Brenner potential
!  version 1 
!
!   7/05 Created based on setbrenner3
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
  use datatypes
  use brennerdata
  implicit none
!
!  Local variables
!
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
  bGcos(3,1:nREBOspecies) = 1.0_dp
  bGcosCspecial(1:8,1:2) = 0.0_dp
  bcos(1:8,1:2) = 0.0_dp
  bLcos(1:8,1:4) = 0.0_dp
!****************
!  Potential 1  *
!****************
!
!  C-C two-body parameters
!
  brepA(1) = 6.325_dp
  brepAlpha(1) = 1.5_dp
  brepQ(1) = 1.29_dp
  battB(1,1) = 1.315_dp
  bR1(1) = 1.7_dp   
  bR2(1) = 2.0_dp
  bTR1(1) = 1.7_dp   
  bTR2(1) = 2.0_dp
!
!  C-H two-body parameters
!
  brepA(2) = 3.6422_dp
  brepAlpha(2) = 1.9583_dp
  brepQ(2) = 1.7386_dp
  battB(1,2) = 1.1199_dp
  bR1(2) = 1.3_dp
  bR2(2) = 1.8_dp
  bTR1(2) = 1.3_dp
  bTR2(2) = 1.8_dp
!
!  H-H two-body parameters
!
  brepA(3) = 4.7509_dp
  brepAlpha(3) = 1.9436_dp
  brepQ(3) = 2.3432_dp
  battB(1,3) = 0.74144_dp
  bR1(3) = 1.1_dp
  bR2(3) = 1.7_dp
  bTR1(3) = 1.1_dp
  bTR2(3) = 1.7_dp
!
!  Si-C two-body parameters
!
  brepA(4) = 4.510_dp
  brepAlpha(4) = 1.698_dp
  brepQ(4) = 1.492_dp
  battB(1,4) = 1.7631_dp
  bR1(4) = 2.20_dp
  bR2(4) = 2.50_dp
  bTR1(4) = 2.20_dp
  bTR2(4) = 2.50_dp
!
!  Si-H two-body parameters
!
  brepA(5) = 3.140_dp
  brepAlpha(5) = 1.6897_dp
  brepQ(5) = 1.8177_dp
  battB(1,5) = 1.441_dp
  bR1(5) = 1.60_dp
  bR2(5) = 2.20_dp
  bTR1(5) = 1.60_dp
  bTR2(5) = 2.20_dp
!     
!  Si-Si two-body parameters
!     
  brepA(6) = 3.3870_dp
  brepAlpha(6) = 1.469_dp
  brepQ(6) = 1.41_dp
  battB(1,6) = 2.197_dp
  bR1(6) = 2.65_dp
  bR2(6) = 2.95_dp
  bTR1(6) = 2.65_dp
  bTR2(6) = 2.95_dp
!
  bF(0,2,3,1,1) = -0.0465_dp
  bF(0,2,3,2,1) = -0.0465_dp
  bF(0,1,2,2,1) = -0.0355_dp
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
!****************************
!  Carbon P values for C-H  *
!****************************
!      bP_CH(0,1,1,1) = 
!      bP_CH(0,0,2,1) = 
!      bP_CH(0,0,3,1) = 
!      bP_CH(0,2,1,1) = 
!      bP_CH(0,1,2,1) = 
!      bP_CH(0,0,3,1) = 
!      bP_CH(0,1,0,2) = 
!      bP_CH(0,2,0,2) = 
!      bP_CH(0,3,0,2) = 
!      bP_CH(0,0,1,2) = 
!      bP_CH(0,1,1,2) = 
!      bP_CH(0,2,1,2) = 
!      bP_CH(0,0,2,2) = 
!      bP_CH(0,1,2,2) = 
!      bP_CH(0,0,3,2) = 
!
!  Finite difference evaluation of second derivative for P_CH
!
  do n = 1,3
    do iy = 1,2
      iyp = min(3,iy+1)
      iym = max(0,iy-1)
      do ix = 1,2
        ixp = min(3,ix+1)
        ixm = max(0,ix-1)
!  
!  xy cross derivs
!     
        bP_CH(3,ix,iy,n) = &
          (bP_CH(0,ixp,iyp,n)-bP_CH(0,ixm,iyp,n)-bP_CH(0,ixp,iym,n)+bP_CH(0,ixm,iym,n))/(dble(ixp-ixm)*dble(iyp-iym))
      enddo
    enddo
  enddo
!****************
!  Triads data  *
!****************
  bdelta(1) = 0.80469_dp
  bdelta(2) = 0.80469_dp
  bdelta(3) = 0.78_dp
!
  balpha(1:nREBOspecies**3) = 0.0_dp
  bexpco(1:nREBOspecies**3) = 1.0_dp
!
!  H - H - X
!
  balpha(nREBOspecies2+nREBOspecies+1) = 3.0_dp
  balpha(nREBOspecies2+nREBOspecies+2) = 3.0_dp
  balpha(nREBOspecies2+nREBOspecies+3) = 3.0_dp
  bexpco(nREBOspecies2+nREBOspecies+1) = exp(-3.0_dp*(0.74144_dp - 1.1199_dp))
  bexpco(nREBOspecies2+nREBOspecies+2) = 1.0_dp
  bexpco(nREBOspecies2+nREBOspecies+3) = exp(-3.0_dp*(0.74144_dp - 1.441_dp))
!
!  C - H - X
!
  balpha(nREBOspecies+1) = 3.0_dp
  balpha(nREBOspecies+2) = 3.0_dp
  balpha(nREBOspecies+3) = 3.0_dp
  bexpco(nREBOspecies+1) = exp(-3.0_dp*(1.1199_dp - 1.315_dp))
  bexpco(nREBOspecies+2) = 1.0_dp
  bexpco(nREBOspecies+3) = exp(-3.0_dp*(1.1199_dp - 1.7631_dp))
!
!  H - C - X
!
  balpha(nREBOspecies2+1) = 3.0_dp
  balpha(nREBOspecies2+2) = 3.0_dp
  balpha(nREBOspecies2+3) = 3.0_dp
  bexpco(nREBOspecies2+1) = 1.0_dp
  bexpco(nREBOspecies2+2) = exp(-3.0_dp*(1.1199_dp - 0.74144_dp))
  bexpco(nREBOspecies2+3) = exp(-3.0_dp*(1.1199_dp - 1.441_dp))
!
!  H - Si - X
!
  balpha(nREBOspecies2+2*nREBOspecies+1) = 3.0_dp
  balpha(nREBOspecies2+2*nREBOspecies+2) = 3.0_dp
  balpha(nREBOspecies2+2*nREBOspecies+3) = 3.0_dp
  bexpco(nREBOspecies2+2*nREBOspecies+1) = exp(-3.0_dp*(1.441_dp - 1.315_dp))
  bexpco(nREBOspecies2+2*nREBOspecies+2) = exp(-3.0_dp*(1.441_dp - 0.74144_dp))
  bexpco(nREBOspecies2+2*nREBOspecies+3) = 1.0_dp
!
!  Si - H - X
!
  balpha(2*nREBOspecies2+nREBOspecies+1) = 3.0_dp
  balpha(2*nREBOspecies2+nREBOspecies+2) = 3.0_dp
  balpha(2*nREBOspecies2+nREBOspecies+3) = 3.0_dp
  bexpco(2*nREBOspecies2+nREBOspecies+1) = exp(-3.0_dp*(1.441_dp - 1.7631_dp))
  bexpco(2*nREBOspecies2+nREBOspecies+2) = 1.0_dp
  bexpco(2*nREBOspecies2+nREBOspecies+3) = exp(-3.0_dp*(1.441_dp - 2.1970_dp))
!*********************
!  C centred angles  *
!*********************
  bGcos(1,1) = 0.011304_dp
  bGcos(2,1) = 19.0_dp
  bGcos(3,1) = 2.5_dp
!*********************
!  H centred angles  *
!*********************
  bGcos(1,2) = 4.0_dp
  bGcos(2,2) = 0.0_dp
  bGcos(3,2) = 1.0_dp
!**********************
!  Si centred angles  *
!**********************
  bGcos(1,3) = 0.010_dp
  bGcos(2,3) = 14.0_dp
  bGcos(3,3) = 2.1_dp
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
!  Set up terms dependent on parameters for convenience
!
  do n = 1,nREBOspecies*(nREBOspecies+1)/2
    bR22(n) = bR2(n)**2
  enddo
!
  return
  end
