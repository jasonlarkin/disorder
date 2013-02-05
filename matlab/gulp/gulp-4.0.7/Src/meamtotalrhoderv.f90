  subroutine meamtotalrhoderv(m,scrho,rhototal,drho,drhotot,drhos,drhotots,drho2,drhotot2, &
                              drho2s,drhotot2s,drho2m,drhotot2m,lstr,lgrad2)
!
!  Calculates the derivatives of the total density from the derivatives of the component 
!  densities within the MEAM method.
!
!  On entry :
!
!  m         = EAM function species number 
!  scrho     = density components in MEAM
!  rhototal  = total density in MEAM
!  drho      = 1st derivatives of the density components w.r.t. Cartesian directions
!  drhos     = 1st derivatives of the density components w.r.t. strain
!  drho2     = 2nd derivatives of the density components w.r.t. Cartesian directions
!  drho2s    = 2nd derivatives of the density components w.r.t. strain
!  drho2m    = 2nd derivatives of the density components w.r.t. mixed Cartesian direction / strain
!
!  On exit :
!
!  drhotot   = 1st derivative of the total density w.r.t. Cartesian directions
!  drhotots  = 1st derivative of the total density w.r.t. strain (if lstr = .true.)
!  drhotot2  = 2nd derivative of the total density w.r.t. Cartesian directions (if lgrad2 = .true.)
!  drhotot2s = 2nd derivative of the total density w.r.t. strain (if lgrad2 & lstr = .true.)
!  drhotot2m = 2nd derivative of the total density w.r.t. mixed Cartesian direction / strain (if lgrad2 & lstr = .true.)
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
!   2/09 Created from meamtotalrho
!   2/09 Strain terms added
!   3/09 l argument changed directly for m
!   3/09 Strain derivatives completed
!   4/09 Extended MEAM form with 24 density components added
!   4/09 Exponential combination rule for MEAM components added
!   5/09 Third derivatives removed for now 
!   6/10 Missing factor of 6 added for xyz term in 3rd order.
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, June 2010
!
  use eam
  use numbers,     only : third
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: m
  logical,     intent(in)    :: lstr
  logical,     intent(in)    :: lgrad2
  real(dp),    intent(in)    :: rhototal
  real(dp),    intent(in)    :: scrho(maxmeamcomponent)
  real(dp),    intent(in)    :: drho(3,maxmeamcomponent)
  real(dp),    intent(in)    :: drhos(6,maxmeamcomponent)
  real(dp),    intent(in)    :: drho2(6,maxmeamcomponent)
  real(dp),    intent(in)    :: drho2s(21,maxmeamcomponent)
  real(dp),    intent(in)    :: drho2m(6,3,maxmeamcomponent)
  real(dp),    intent(out)   :: drhotot(3)
  real(dp),    intent(out)   :: drhotots(6)
  real(dp),    intent(out)   :: drhotot2(6)
  real(dp),    intent(out)   :: drhotot2s(21)
  real(dp),    intent(out)   :: drhotot2m(6,3)
!
!  Local variables
!
  integer(i4)                :: i                   ! Looping index over nterm components
  integer(i4)                :: ind                 ! Looping index over convolution of 2 coordinates
  integer(i4)                :: nc1                 ! Looping index over Cartesian coordinate
  integer(i4)                :: nc2                 ! Looping index over Cartesian coordinate
  integer(i4)                :: ns1                 ! Looping index over strain
  integer(i4)                :: ns2                 ! Looping index over strain
  integer(i4)                :: nterm               ! Number of density components
  real(dp)                   :: coeff(24)           ! Coefficients of MEAM density components
  real(dp)                   :: dgofgammadgamma     ! First derivative of G w.r.t. gamma
  real(dp)                   :: d2gofgammadgamma2   ! Second derivative of G w.r.t. gamma
  real(dp)                   :: dgamma(3)           ! First derivatives of G w.r.t. Cartesian coordinates
  real(dp)                   :: dgammas(6)          ! First derivatives of G w.r.t. strains
  real(dp)                   :: d2gamma(6)          ! Second derivatives of G w.r.t. two Cartesian coordinates
  real(dp)                   :: d2gamma2s(21)       ! Second derivatives of G w.r.t. two strains
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
  drhotot(1:3) = 0.0_dp
  if (lgrad2) then
    drhotot2(1:6) = 0.0_dp
  endif
  if (lstr) then
    drhotots(1:6) = 0.0_dp
    if (lgrad2) then
      drhotot2s(1:21) = 0.0_dp
      drhotot2m(1:6,1:3) = 0.0_dp
    endif
  endif
  if (m.gt.0) then
    if (rhototal.gt.1.0d-12) then
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
            coeff(16) = eamfnmeamcoeff(4,m)*6.0_dp
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
!  Build interim derivative components 
!
        do i = 1,nterm
          drhotot(1:3) = drhotot(1:3) + coeff(i)*scrho(i)*drho(1:3,i)
        enddo
        if (lstr) then
          do i = 1,nterm
            drhotots(1:6) = drhotots(1:6) + coeff(i)*scrho(i)*drhos(1:6,i)
          enddo
        endif
        if (lgrad2) then
          do i = 1,nterm
            drhotot2(1:6) = drhotot2(1:6) + coeff(i)*scrho(i)*drho2(1:6,i)
            drhotot2(1)   = drhotot2(1)   + coeff(i)*drho(1,i)*drho(1,i)
            drhotot2(2)   = drhotot2(2)   + coeff(i)*drho(1,i)*drho(2,i)
            drhotot2(3)   = drhotot2(3)   + coeff(i)*drho(1,i)*drho(3,i)
            drhotot2(4)   = drhotot2(4)   + coeff(i)*drho(2,i)*drho(2,i)
            drhotot2(5)   = drhotot2(5)   + coeff(i)*drho(2,i)*drho(3,i)
            drhotot2(6)   = drhotot2(6)   + coeff(i)*drho(3,i)*drho(3,i)
          enddo
          if (lstr) then
            do i = 1,nterm
              drhotot2s(1:21) = drhotot2s(1:21) + coeff(i)*scrho(i)*drho2s(1:21,i)
              drhotot2m(1:6,1:3) = drhotot2m(1:6,1:3) + coeff(i)*scrho(i)*drho2m(1:6,1:3,i)
              ind = 0
              do ns1 = 1,6
                do ns2 = 1,ns1
                  ind = ind + 1
                  drhotot2s(ind) = drhotot2s(ind) + coeff(i)*drhos(ns1,i)*drhos(ns2,i)
                enddo
              enddo
              do ns1 = 1,3
                do ns2 = 1,6
                  drhotot2m(ns2,ns1) = drhotot2m(ns2,ns1) + coeff(i)*drhos(ns2,i)*drho(ns1,i)
                enddo
              enddo
            enddo
          endif
        endif
!
!  Divide contributions by inverse of total density
!
        rrho = 1.0_dp/rhototal
        drhotot(1:3) = rrho*drhotot(1:3)
        if (lstr) then
          drhotots(1:6) = rrho*drhotots(1:6)
        endif
        if (lgrad2) then
          drhotot2(1:6) = rrho*drhotot2(1:6)
          if (lstr) then
            drhotot2s(1:21)    = rrho*drhotot2s(1:21)
            drhotot2m(1:6,1:3) = rrho*drhotot2m(1:6,1:3)
          endif
        endif
!
!  Now combine contributions from derivatives to build totals
!
        if (lgrad2) then
          drhotot2(1)  = drhotot2(1) - rrho*drhotot(1)*drhotot(1)
          drhotot2(2)  = drhotot2(2) - rrho*drhotot(1)*drhotot(2)
          drhotot2(3)  = drhotot2(3) - rrho*drhotot(1)*drhotot(3)
          drhotot2(4)  = drhotot2(4) - rrho*drhotot(2)*drhotot(2)
          drhotot2(5)  = drhotot2(5) - rrho*drhotot(2)*drhotot(3)
          drhotot2(6)  = drhotot2(6) - rrho*drhotot(3)*drhotot(3)
          if (lstr) then
            ind = 0
            do ns1 = 1,6
              do ns2 = 1,ns1
                ind = ind + 1
                drhotot2s(ind)  = drhotot2s(ind) - rrho*drhotots(ns1)*drhotots(ns2)
              enddo
            enddo
            do ns1 = 1,3
              do ns2 = 1,6
                drhotot2m(ns2,ns1)  = drhotot2m(ns2,ns1) - rrho*drhotots(ns2)*drhotot(ns1)
              enddo
            enddo
          endif
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
          dgamma(1:3) = 0.0_dp
          if (lstr) then
            dgammas(1:6) = 0.0_dp
          endif
!
!  Add derivative of G(gamma) w.r.t. rho_h, h > 0, multiplied by rho0
!
          if (nterm.gt.1) then
            do i = 2,nterm
              dgamma(1:3) = dgamma(1:3) + dgammadrhoh*coeff(i)*scrho(i)*drho(1:3,i)
            enddo
            if (lstr) then
              do i = 2,nterm
                dgammas(1:6) = dgammas(1:6) + dgammadrhoh*coeff(i)*scrho(i)*drhos(1:6,i)
              enddo
            endif
          endif
!
!  Add derivative of G(gamma) w.r.t. rho_0
!
          dgamma(1:3) = dgamma(1:3) - 2.0_dp*gamma*rrho0*drho(1:3,1)
          if (lstr) then
            dgammas(1:6) = dgammas(1:6) - 2.0_dp*gamma*rrho0*drhos(1:6,1)
          endif
          if (lgrad2) then
!
!  Second derivatives of gamma
!
            d2gofgammadgamma2 = dgofgammadgamma*gofgamma*exptrm - dgofgammadgamma
            d2gamma(1:6) = 0.0_dp
            if (nterm.gt.1) then
              ind = 0
              do nc1 = 1,3
                do nc2 = nc1,3
                  ind = ind + 1
                  do i = 2,nterm
                    d2gamma(ind) = d2gamma(ind) + coeff(i)*dgammadrhoh*(drho(nc1,i)*drho(nc2,i) + scrho(i)*drho2(ind,i))
                    d2gamma(ind) = d2gamma(ind) - 2.0_dp*coeff(i)*dgammadrhoh*rrho0*scrho(i)* &
                                                  (drho(nc1,1)*drho(nc2,i) + drho(nc2,1)*drho(nc1,i))
                  enddo
                enddo
              enddo
            endif
            ind = 0
            do nc1 = 1,3
              do nc2 = nc1,3
                ind = ind + 1
                d2gamma(ind) = d2gamma(ind) + 3.0_dp*dgammadrhoh*gamma*drho(nc1,1)*drho(nc2,1) - 2.0_dp*gamma*rrho0*drho2(ind,1)
              enddo
            enddo
!
            if (lstr) then
              d2gamma2s(1:21) = 0.0_dp
              d2gamma2m(1:6,1:3) = 0.0_dp
              if (nterm.gt.1) then
                ind = 0
                do ns1 = 1,6
                  do ns2 = 1,ns1
                    ind = ind + 1
                    do i = 2,nterm
                      d2gamma2s(ind) = d2gamma2s(ind) + coeff(i)*dgammadrhoh*(drhos(ns1,i)*drhos(ns2,i) + scrho(i)*drho2s(ind,i))
                      d2gamma2s(ind) = d2gamma2s(ind) - 2.0_dp*coeff(i)*dgammadrhoh*rrho0*scrho(i)* &
                                                        (drhos(ns1,1)*drhos(ns2,i) + drhos(ns2,1)*drhos(ns1,i))
                    enddo
                  enddo
                enddo
                ind = 0
                do nc1 = 1,3
                  do ns1 = 1,6
                    ind = ind + 1
                    do i = 2,nterm
                      d2gamma2m(ns1,nc1) = d2gamma2m(ns1,nc1) + coeff(i)*dgammadrhoh* &
                                                                (drhos(ns1,i)*drho(nc1,i) + scrho(i)*drho2m(ns1,nc1,i))
                      d2gamma2m(ns1,nc1) = d2gamma2m(ns1,nc1) - 2.0_dp*coeff(i)*dgammadrhoh*rrho0*scrho(i)* &
                                                                (drhos(ns1,1)*drho(nc1,i) + drho(nc1,1)*drhos(ns1,i))
                    enddo
                  enddo
                enddo
              endif
              ind = 0
              do ns1 = 1,6
                do ns2 = 1,ns1
                  ind = ind + 1
                  d2gamma2s(ind) = d2gamma2s(ind) + 3.0_dp*dgammadrhoh*gamma*drhos(ns1,1)*drhos(ns2,1) &
                                                  - 2.0_dp*gamma*rrho0*drho2s(ind,1)
                enddo
              enddo
              ind = 0
              do nc1 = 1,3
                do ns1 = 1,6
                  ind = ind + 1
                  d2gamma2m(ns1,nc1) = d2gamma2m(ns1,nc1) + 3.0_dp*dgammadrhoh*gamma*drhos(ns1,1)*drho(nc1,1) &
                                                          - 2.0_dp*gamma*rrho0*drho2m(ns1,nc1,1)
                enddo
              enddo
            endif
          endif
!
!  Build total derivatives
!
          drhotot(1:3) = drhotot(1:3) + drho(1:3,1)*gofgamma + rho0*dgofgammadgamma*dgamma(1:3)
          if (lstr) then
            drhotots(1:6) = drhotots(1:6) + drhos(1:6,1)*gofgamma + rho0*dgofgammadgamma*dgammas(1:6)
          endif
          if (lgrad2) then
            drhotot2(1:6) = drhotot2(1:6) + drho2(1:6,1)*gofgamma + rho0*dgofgammadgamma*d2gamma(1:6)
            ind = 0
            do nc1 = 1,3
              do nc2 = nc1,3
                ind = ind + 1
                drhotot2(ind) = drhotot2(ind) + rho0*d2gofgammadgamma2*dgamma(nc1)*dgamma(nc2) 
                drhotot2(ind) = drhotot2(ind) + drho(nc1,1)*dgofgammadgamma*dgamma(nc2)
                drhotot2(ind) = drhotot2(ind) + drho(nc2,1)*dgofgammadgamma*dgamma(nc1)
              enddo
            enddo
            if (lstr) then
              drhotot2s(1:21) = drhotot2s(1:21) + drho2s(1:21,1)*gofgamma + rho0*dgofgammadgamma*d2gamma2s(1:21)
              drhotot2m(1:6,1:3) = drhotot2m(1:6,1:3) + drho2m(1:6,1:3,1)*gofgamma + rho0*dgofgammadgamma*d2gamma2m(1:6,1:3)
              ind = 0
              do ns1 = 1,6
                do ns2 = 1,ns1
                  ind = ind + 1
                  drhotot2s(ind) = drhotot2s(ind) + rho0*d2gofgammadgamma2*dgammas(ns1)*dgammas(ns2) 
                  drhotot2s(ind) = drhotot2s(ind) + drhos(ns1,1)*dgofgammadgamma*dgammas(ns2)
                  drhotot2s(ind) = drhotot2s(ind) + drhos(ns2,1)*dgofgammadgamma*dgammas(ns1)
                enddo
              enddo
              ind = 0
              do nc1 = 1,3
                do ns1 = 1,6
                  ind = ind + 1
                  drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) + rho0*d2gofgammadgamma2*dgammas(ns1)*dgamma(nc1) 
                  drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) + drhos(ns1,1)*dgofgammadgamma*dgamma(nc1)
                  drhotot2m(ns1,nc1) = drhotot2m(ns1,nc1) + drho(nc1,1)*dgofgammadgamma*dgammas(ns1)
                enddo
              enddo
            endif
          endif
!
        endif
      endif
    endif
  endif
!
  return
  end
