  subroutine meamtotalrhocrossderv(m,scrho,rhototal,drho1,drhot1,drho2,drhot2,drhos1,drhos2, &
                                   drhotot2,drhotot2s,drhotot2m,lstr)
!
!  Calculates the derivatives of the total density from the derivatives of the component 
!  densities within the MEAM method. This version calculates the cross terms in the second
!  derivatives that arise between pairs of different i-j / i-k vectors. This contribution
!  only arises in MEAM and not EAM.
!
!  On entry :
!
!  m         = EAM function species number 
!  scrho     = density components in MEAM
!  rhototal  = total density in MEAM
!  drho1     = 1st derivatives of the density components w.r.t. Cartesian directions for first vector
!  drhos1    = 1st derivatives of the density components w.r.t. strain for first vector
!  drho2     = 1st derivatives of the density components w.r.t. Cartesian directions for second vector
!  drhos2    = 1st derivatives of the density components w.r.t. strain for second vector
!  drho2     = 2nd derivatives of the density components w.r.t. Cartesian directions
!  drho2s    = 2nd derivatives of the density components w.r.t. strain
!  drho2m    = 2nd derivatives of the density components w.r.t. mixed Cartesian direction / strain
!
!  On exit :
!
!  drhotot2  = 2nd derivative of the total density w.r.t. Cartesian directions 
!  drhotot2s = 2nd derivative of the total density w.r.t. strain (if lstr = .true.)
!  drhotot2m = 2nd derivative of the total density w.r.t. mixed Cartesian direction / strain (if lstr = .true.)
!
!  The MEAM components in scrho are numbered according to the following:
!
!  1 => order 0 r (standard EAM)
!  2 => order 1 x
!  3 => order 1 y
!  4 => order 1 z
!  5 => order 2 xx
!  6 => order 2 xy
!  7 => order 2 xz
!  8 => order 2 yy
!  9 => order 2 yz
! 10 => order 2 zz
! 11 => order 2 r
! 12 => order 3 xxx
! 13 => order 3 xxy
! 14 => order 3 xxz
! 15 => order 3 xyy
! 16 => order 3 xyz
! 17 => order 3 xzz
! 18 => order 3 yyy
! 19 => order 3 yyz
! 20 => order 3 yzz
! 21 => order 3 zzz
! 22 => order 3 x
! 23 => order 3 y
! 24 => order 3 z
!
!   2/09 Created from meamtotalrhoderv
!   3/09 l argument changed to be m directly
!   3/09 Strain derivatives completed
!   4/09 Extended MEAM form with 24 density components added
!   4/09 Exponential combination rule for MEAM components added
!   6/10 Missing factor of 6 added for xyz term in 3rd order.
!  12/11 coeff(8) corrected by removing multiplier of 6
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, December 2011
!
  use eam
  use numbers,     only : third
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: m
  logical,     intent(in)    :: lstr
  real(dp),    intent(in)    :: rhototal
  real(dp),    intent(in)    :: scrho(maxmeamcomponent)
  real(dp),    intent(in)    :: drho1(3,maxmeamcomponent)
  real(dp),    intent(in)    :: drho2(3,maxmeamcomponent)
  real(dp),    intent(in)    :: drhot1(3)
  real(dp),    intent(in)    :: drhot2(3)
  real(dp),    intent(in)    :: drhos1(6,maxmeamcomponent)
  real(dp),    intent(in)    :: drhos2(6,maxmeamcomponent)
  real(dp),    intent(out)   :: drhotot2(3,3)
  real(dp),    intent(out)   :: drhotot2s(6,6)
  real(dp),    intent(out)   :: drhotot2m(6,3)
!
!  Local variables
!
  integer(i4)                :: i                   ! Looping index over nterm components
  integer(i4)                :: j                   ! Looping index over nterm components
  integer(i4)                :: nc1                 ! Looping index over Cartesian coordinate
  integer(i4)                :: nc2                 ! Looping index over Cartesian coordinate
  integer(i4)                :: ns1                 ! Looping index over strain
  integer(i4)                :: ns2                 ! Looping index over strain
  integer(i4)                :: nterm               ! Number of density components
  real(dp)                   :: coeff(24)           ! Coefficients of MEAM density components
  real(dp)                   :: dgofgammadgamma     ! First derivative of G w.r.t. gamma
  real(dp)                   :: d2gofgammadgamma2   ! Second derivative of G w.r.t. gamma
  real(dp)                   :: dgamma1(3)          ! First derivatives of G w.r.t. Cartesian coordinates, pair 1
  real(dp)                   :: dgamma2(3)          ! First derivatives of G w.r.t. Cartesian coordinates, pair 2
  real(dp)                   :: dgamma1s(6)         ! First derivatives of G w.r.t. strains, pair 1
  real(dp)                   :: dgamma2s(6)         ! First derivatives of G w.r.t. strains, pair 2
  real(dp)                   :: d2gamma(3,3)        ! Second derivatives of G w.r.t. two Cartesian coordinates
  real(dp)                   :: d2gamma2s(6,6)      ! Second derivatives of G w.r.t. two strains
  real(dp)                   :: d2gamma2m(6,3)      ! Second derivatives of G w.r.t. one strain and one Cartesian coordinate
  real(dp)                   :: dgammadrhoh         ! First derivative of gamma w.r.t. rho_h when multiplied by rho_h and t_h
  real(dp)                   :: exptrm              ! Exp(-gamma)
  real(dp)                   :: gofgamma            ! Function G(gamma)
  real(dp)                   :: gamma               ! Gamma
  real(dp)                   :: rho0                ! Spherical density component, rho_0
  real(dp)                   :: rrho                ! Inverse of total density for sum case
  real(dp)                   :: rrho0               ! Inverse of rho0
!
!  Initialise total density derivatives
!
  drhotot2(1:3,1:3) = 0.0_dp
  if (lstr) then
    drhotot2s(1:6,1:6) = 0.0_dp
    drhotot2m(1:6,1:3) = 0.0_dp
  endif
  if (m.gt.0) then
    if (rhototal.gt.1.0d-12) then
      rrho = 1.0_dp/rhototal
!
!  Set coefficients
!
      nterm = 1
      coeff(1) = eamfnmeamcoeff(1,m)
      if (neamfnmeamorder(m).gt.1) then
        nterm = 4
        coeff(2) = eamfnmeamcoeff(2,m)
        coeff(3) = eamfnmeamcoeff(2,m)
        coeff(4) = eamfnmeamcoeff(2,m)
        if (neamfnmeamorder(m).gt.2) then
          nterm = 11
          coeff(5)  = eamfnmeamcoeff(3,m)
          coeff(6)  = eamfnmeamcoeff(3,m)*2.0_dp
          coeff(7)  = eamfnmeamcoeff(3,m)*2.0_dp
          coeff(8)  = eamfnmeamcoeff(3,m)
          coeff(9)  = eamfnmeamcoeff(3,m)*2.0_dp
          coeff(10) = eamfnmeamcoeff(3,m)
          coeff(11) = - eamfnmeamcoeff(3,m)*third
          if (neamfnmeamorder(m).gt.3) then
            if (neamfnmeamtype(m).eq.1) then
              nterm = 21
            else
              nterm = 24
              coeff(22) = - eamfnmeamcoeff(4,m)*0.6_dp
              coeff(23) = - eamfnmeamcoeff(4,m)*0.6_dp
              coeff(24) = - eamfnmeamcoeff(4,m)*0.6_dp
            endif
            coeff(12) = eamfnmeamcoeff(4,m)
            coeff(13) = eamfnmeamcoeff(4,m)*3.0_dp
            coeff(14) = eamfnmeamcoeff(4,m)*3.0_dp
            coeff(15) = eamfnmeamcoeff(4,m)*3.0_dp
            coeff(16) = eamfnmeamcoeff(4,m)
            coeff(17) = eamfnmeamcoeff(4,m)*3.0_dp
            coeff(18) = eamfnmeamcoeff(4,m)
            coeff(19) = eamfnmeamcoeff(4,m)*3.0_dp
            coeff(20) = eamfnmeamcoeff(4,m)*3.0_dp
            coeff(21) = eamfnmeamcoeff(4,m)
          endif
        endif
      endif
      if (neamfnmeamcombotype(m).eq.1) then
!-----------------------------
!  MEAM 1NN combination rule |
!-----------------------------
!
!  Convert coefficients of expansion into premultiplier for derivative product
!
        do i = 1,nterm
          coeff(i) = rrho*coeff(i)
        enddo
!
!  Build total derivative components 
!
        do i = 1,nterm
          drhotot2(1,1) = drhotot2(1,1) + coeff(i)*drho1(1,i)*drho2(1,i)
          drhotot2(2,1) = drhotot2(2,1) + coeff(i)*drho1(2,i)*drho2(1,i)
          drhotot2(3,1) = drhotot2(3,1) + coeff(i)*drho1(3,i)*drho2(1,i)
          drhotot2(1,2) = drhotot2(1,2) + coeff(i)*drho1(1,i)*drho2(2,i)
          drhotot2(2,2) = drhotot2(2,2) + coeff(i)*drho1(2,i)*drho2(2,i)
          drhotot2(3,2) = drhotot2(3,2) + coeff(i)*drho1(3,i)*drho2(2,i)
          drhotot2(1,3) = drhotot2(1,3) + coeff(i)*drho1(1,i)*drho2(3,i)
          drhotot2(2,3) = drhotot2(2,3) + coeff(i)*drho1(2,i)*drho2(3,i)
          drhotot2(3,3) = drhotot2(3,3) + coeff(i)*drho1(3,i)*drho2(3,i)
        enddo
        do i = 1,nterm
          do j = 1,nterm
            drhotot2(1,1) = drhotot2(1,1) - rrho*coeff(i)*coeff(j)*drho1(1,i)*drho2(1,j)*scrho(i)*scrho(j)
            drhotot2(2,1) = drhotot2(2,1) - rrho*coeff(i)*coeff(j)*drho1(2,i)*drho2(1,j)*scrho(i)*scrho(j)
            drhotot2(3,1) = drhotot2(3,1) - rrho*coeff(i)*coeff(j)*drho1(3,i)*drho2(1,j)*scrho(i)*scrho(j)
            drhotot2(1,2) = drhotot2(1,2) - rrho*coeff(i)*coeff(j)*drho1(1,i)*drho2(2,j)*scrho(i)*scrho(j)
            drhotot2(2,2) = drhotot2(2,2) - rrho*coeff(i)*coeff(j)*drho1(2,i)*drho2(2,j)*scrho(i)*scrho(j)
            drhotot2(3,2) = drhotot2(3,2) - rrho*coeff(i)*coeff(j)*drho1(3,i)*drho2(2,j)*scrho(i)*scrho(j)
            drhotot2(1,3) = drhotot2(1,3) - rrho*coeff(i)*coeff(j)*drho1(1,i)*drho2(3,j)*scrho(i)*scrho(j)
            drhotot2(2,3) = drhotot2(2,3) - rrho*coeff(i)*coeff(j)*drho1(2,i)*drho2(3,j)*scrho(i)*scrho(j)
            drhotot2(3,3) = drhotot2(3,3) - rrho*coeff(i)*coeff(j)*drho1(3,i)*drho2(3,j)*scrho(i)*scrho(j)
          enddo
        enddo
        if (lstr) then
          do i = 1,nterm
            do ns1 = 1,6
              do ns2 = 1,6
                drhotot2s(ns2,ns1) = drhotot2s(ns2,ns1) + coeff(i)*drhos1(ns1,i)*drhos2(ns2,i)
              enddo
            enddo
            do nc1 = 1,3
              do ns1 = 1,6
                drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) + coeff(i)*drhos2(ns1,i)*drho1(nc1,i)
              enddo
            enddo
          enddo
          do i = 1,nterm
            do j = 1,nterm
              do ns1 = 1,6
                do ns2 = 1,6
                  drhotot2s(ns2,ns1) = drhotot2s(ns2,ns1) - rrho*coeff(i)*coeff(j)*drhos1(ns1,i)*drhos2(ns2,j)*scrho(i)*scrho(j)
                enddo
              enddo
              do nc1 = 1,3
                do ns1 = 1,6
                  drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) - rrho*coeff(i)*coeff(j)*drhos2(ns1,i)*drho1(nc1,j)*scrho(i)*scrho(j)
                enddo
              enddo
            enddo
          enddo
        endif
      else
!-----------------------------
!  MEAM 2NN combination rule |
!-----------------------------
!
!  Set rho0
!
        rho0 = scrho(1)
!
!  If rho0 is less than a threshold then the derivatives are left at zero
!
        if (rho0.ge.1.0d-12) then
!
!  Compute rho2
!
          gamma = 0.0_dp
          if (nterm.gt.1) then
            do i = 2,nterm
              gamma = gamma + coeff(i)*scrho(i)**2
            enddo
          endif
          rrho0    = 1.0_dp/rho0
          gamma    = gamma*rrho0**2
          exptrm   = exp(-gamma)
          gofgamma  = 2.0_dp/(1.0_dp + exptrm)
          dgofgammadgamma = 0.5_dp*exptrm*gofgamma**2
          dgammadrhoh = 2.0_dp*rrho0**2
!
!  Compute derivatives of G with respect to coordinates
!
          dgamma1(1:3) = 0.0_dp
          dgamma2(1:3) = 0.0_dp
          if (lstr) then
            dgamma1s(1:6) = 0.0_dp
            dgamma2s(1:6) = 0.0_dp
          endif
!
!  Add derivative of G(gamma) w.r.t. rho_h, h > 0, multiplied by rho0
!
          if (nterm.gt.1) then
            do i = 2,nterm
              dgamma1(1:3) = dgamma1(1:3) + dgammadrhoh*coeff(i)*scrho(i)*drho1(1:3,i)
              dgamma2(1:3) = dgamma2(1:3) + dgammadrhoh*coeff(i)*scrho(i)*drho2(1:3,i)
            enddo
            if (lstr) then
              do i = 2,nterm
                dgamma1s(1:6) = dgamma1s(1:6) + dgammadrhoh*coeff(i)*scrho(i)*drhos1(1:6,i)
                dgamma2s(1:6) = dgamma2s(1:6) + dgammadrhoh*coeff(i)*scrho(i)*drhos2(1:6,i)
              enddo
            endif
          endif
!
!  Add derivative of G(gamma) w.r.t. rho_0
!
          dgamma1(1:3) = dgamma1(1:3) - 2.0_dp*gamma*rrho0*drho1(1:3,1)
          dgamma2(1:3) = dgamma2(1:3) - 2.0_dp*gamma*rrho0*drho2(1:3,1)
          if (lstr) then
            dgamma1s(1:6) = dgamma1s(1:6) - 2.0_dp*gamma*rrho0*drhos1(1:6,1)
            dgamma2s(1:6) = dgamma2s(1:6) - 2.0_dp*gamma*rrho0*drhos2(1:6,1)
          endif
!
!  Second derivatives of gamma
!
          d2gofgammadgamma2 = dgofgammadgamma*gofgamma*exptrm - dgofgammadgamma
          d2gamma(1:3,1:3) = 0.0_dp
          if (nterm.gt.1) then
            do nc1 = 1,3
              do nc2 = 1,3
                do i = 2,nterm
                  d2gamma(nc2,nc1) = d2gamma(nc2,nc1) + coeff(i)*dgammadrhoh*drho2(nc1,i)*drho1(nc2,i)
                  d2gamma(nc2,nc1) = d2gamma(nc2,nc1) - 2.0_dp*coeff(i)*dgammadrhoh*rrho0*scrho(i)* &
                                                       (drho2(nc1,1)*drho1(nc2,i) + drho1(nc2,1)*drho2(nc1,i))
                enddo
              enddo
            enddo
          endif
          do nc1 = 1,3
            do nc2 = 1,3
              d2gamma(nc2,nc1) = d2gamma(nc2,nc1) + 3.0_dp*dgammadrhoh*gamma*drho2(nc1,1)*drho1(nc2,1)
            enddo
          enddo
!
          if (lstr) then
            d2gamma2s(1:6,1:6) = 0.0_dp
            d2gamma2m(1:6,1:3) = 0.0_dp
            if (nterm.gt.1) then
              do ns1 = 1,6
                do ns2 = 1,6
                  do i = 2,nterm
                    d2gamma2s(ns2,ns1) = d2gamma2s(ns2,ns1) + coeff(i)*dgammadrhoh*drhos1(ns1,i)*drhos2(ns2,i)
                    d2gamma2s(ns2,ns1) = d2gamma2s(ns2,ns1) - 2.0_dp*coeff(i)*dgammadrhoh*rrho0*scrho(i)* &
                                                             (drhos1(ns1,1)*drhos2(ns2,i) + drhos2(ns2,1)*drhos1(ns1,i))
                  enddo
                enddo
              enddo
              do nc1 = 1,3
                do ns1 = 1,6
                  do i = 2,nterm
                    d2gamma2m(ns1,nc1) = d2gamma2m(ns1,nc1) + coeff(i)*dgammadrhoh*drhos2(ns1,i)*drho1(nc1,i)
                    d2gamma2m(ns1,nc1) = d2gamma2m(ns1,nc1) - 2.0_dp*coeff(i)*dgammadrhoh*rrho0*scrho(i)* &
                                                              (drhos2(ns1,1)*drho1(nc1,i) + drho1(nc1,1)*drhos2(ns1,i))
                  enddo
                enddo
              enddo
            endif
            do ns1 = 1,6
              do ns2 = 1,6
                d2gamma2s(ns2,ns1) = d2gamma2s(ns2,ns1) + 3.0_dp*dgammadrhoh*gamma*drhos1(ns1,1)*drhos2(ns2,1) 
              enddo
            enddo
            do nc1 = 1,3
              do ns1 = 1,6
                d2gamma2m(ns1,nc1) = d2gamma2m(ns1,nc1) + 3.0_dp*dgammadrhoh*gamma*drhos2(ns1,1)*drho1(nc1,1) 
              enddo
            enddo
          endif
!
!  Build total derivatives
!
          drhotot2(1:3,1:3) = drhotot2(1:3,1:3) + rho0*dgofgammadgamma*d2gamma(1:3,1:3)
          do nc1 = 1,3
            do nc2 = 1,3
              drhotot2(nc2,nc1) = drhotot2(nc2,nc1) + rho0*d2gofgammadgamma2*dgamma2(nc1)*dgamma1(nc2)
              drhotot2(nc2,nc1) = drhotot2(nc2,nc1) + drho2(nc1,1)*dgofgammadgamma*dgamma1(nc2)
              drhotot2(nc2,nc1) = drhotot2(nc2,nc1) + drho1(nc2,1)*dgofgammadgamma*dgamma2(nc1)
            enddo
          enddo
          if (lstr) then
            drhotot2s(1:6,1:6) = drhotot2s(1:6,1:6) + rho0*dgofgammadgamma*d2gamma2s(1:6,1:6)
            drhotot2m(1:6,1:3) = drhotot2m(1:6,1:3) + rho0*dgofgammadgamma*d2gamma2m(1:6,1:3)
            do ns1 = 1,6
              do ns2 = 1,6
                drhotot2s(ns2,ns1) = drhotot2s(ns2,ns1) + rho0*d2gofgammadgamma2*dgamma1s(ns1)*dgamma2s(ns2)
                drhotot2s(ns2,ns1) = drhotot2s(ns2,ns1) + drhos1(ns1,1)*dgofgammadgamma*dgamma2s(ns2)
                drhotot2s(ns2,ns1) = drhotot2s(ns2,ns1) + drhos2(ns2,1)*dgofgammadgamma*dgamma1s(ns1)
              enddo
            enddo
            do nc1 = 1,3
              do ns1 = 1,6
                drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) + rho0*d2gofgammadgamma2*dgamma2s(ns1)*dgamma1(nc1)
                drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) + drhos2(ns1,1)*dgofgammadgamma*dgamma1(nc1)
                drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) + drho1(nc1,1)*dgofgammadgamma*dgamma2s(ns1)
              enddo
            enddo
          endif
!
        endif
      endif
    endif
  endif
!
  return
  end
