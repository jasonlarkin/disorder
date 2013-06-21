  subroutine meamtotalrhoscreenderv(m,npartial,partial,xij,yij,zij,scrho,rhototal,rscrhofn,rho,Sij,lstr)
!
!  Calculates the derivatives of the total density from the derivatives of the component 
!  densities within the MEAM method.
!
!  On entry :
!
!  m         = EAM function species number 
!  scrho     = density components in MEAM
!  rscrhofn  = derivative of MEAM functional w.r.t. rhototal
!  rhototal  = total density in MEAM
!  rho(*)    = components of density for this pair
!  Sij       = total screening factor for i-j pair
!  npartial  = number of atoms contributing to partial screening of i-j pair
!  partial   = data type containing the information regarding the i-k-j trio for screening
!  xij       = x component of i-j vector
!  yij       = y component of i-j vector
!  zij       = z component of i-j vector
!
!  On exit :
!
!  Stored in partial:
!
!  drhototij = 1st derivative of the total density w.r.t. Cartesian directions for ij
!  drhototik = 1st derivative of the total density w.r.t. Cartesian directions for ik
!  drhototjk = 1st derivative of the total density w.r.t. Cartesian directions for jk
!  drhotots  = 1st derivative of the total density w.r.t. strain (if lstr = .true.)
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
!   4/09 Created from meamtotalrhoderv
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
  integer(i4),           intent(in)    :: m
  integer(i4),           intent(in)    :: npartial
  logical,               intent(in)    :: lstr
  real(dp),              intent(in)    :: rhototal
  real(dp),              intent(in)    :: rscrhofn
  real(dp),              intent(in)    :: Sij
  real(dp),              intent(in)    :: scrho(maxmeamcomponent)
  real(dp),              intent(in)    :: rho(maxmeamcomponent)
  real(dp),              intent(in)    :: xij
  real(dp),              intent(in)    :: yij
  real(dp),              intent(in)    :: zij
  type(screening_atoms), intent(inout) :: partial
!
!  Local variables
!
  integer(i4)                          :: i                   ! Looping index over nterm components
  integer(i4)                          :: np                  ! Looping index over partial screening atom
  integer(i4)                          :: nterm               ! Number of density components
  real(dp)                             :: coeff(24)           ! Coefficients of MEAM density components
  real(dp)                             :: drhotot             ! First derivative w.r.t. rho
  real(dp)                             :: dgofgammadgamma     ! First derivative of G w.r.t. gamma
  real(dp)                             :: dgamma              ! First derivatives of G w.r.t. Cartesian coordinates
  real(dp)                             :: dgammadrhoh         ! First derivative of gamma w.r.t. rho_h when multiplied by rho_h and t_h
  real(dp)                             :: exptrm              ! Exp(-gamma)
  real(dp)                             :: gofgamma            ! Function G(gamma)
  real(dp)                             :: gamma               ! Gamma
  real(dp)                             :: rho0                ! Spherical density component, rho_0
  real(dp)                             :: rrho                ! Inverse of total density for sum case
  real(dp)                             :: rrho0               ! Inverse of rho0
  real(dp)                             :: Sij_rest            ! Sij product, but without current Sikj contribution (and Silj for second derivatives)
  real(dp)                             :: xik                 ! Local copy of partial%sa_xik(np) for brevity
  real(dp)                             :: yik                 ! Local copy of partial%sa_yik(np) for brevity
  real(dp)                             :: zik                 ! Local copy of partial%sa_zik(np) for brevity
  real(dp)                             :: xjk                 ! Local copy of partial%sa_xjk(np) for brevity
  real(dp)                             :: yjk                 ! Local copy of partial%sa_yjk(np) for brevity
  real(dp)                             :: zjk                 ! Local copy of partial%sa_zjk(np) for brevity
!
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
        drhotot = 0.0_dp
        do i = 1,nterm
          drhotot = drhotot + coeff(i)*scrho(i)*rho(i)
        enddo
!
!  Divide contributions by inverse of total density
!
        rrho = 1.0_dp/rhototal
        drhotot = rrho*drhotot
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
          dgamma = 0.0_dp
!
!  Add derivative of G(gamma) w.r.t. rho_h, h > 0, multiplied by rho0
!
          if (nterm.gt.1) then
            do i = 2,nterm
              dgamma = dgamma + dgammadrhoh*coeff(i)*scrho(i)*rho(i)
            enddo
          endif
!
!  Add derivative of G(gamma) w.r.t. rho_0
!
          dgamma = dgamma - 2.0_dp*gamma*rrho0*rho(1)
!
!  Build total derivatives
!
          drhotot = rho(1)*gofgamma + rho0*dgofgammadgamma*dgamma
        endif
      endif
!
!  Now loop over partially screening atoms to construct full derivative
!
      do np = 1,npartial
!
!  Set local scalars to keep code compact
!
        xik = partial%sa_xik(np)
        yik = partial%sa_yik(np)
        zik = partial%sa_zik(np)
        xjk = partial%sa_xjk(np)
        yjk = partial%sa_yjk(np)
        zjk = partial%sa_zjk(np)
!
!  Compute Sij without the current Sikj : Since all Sikj must be > 0 then we can just divide
!
        Sij_rest = Sij/partial%sa_Sikj(np)
!
        partial%sa_drhototij(1,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*xij
        partial%sa_drhototij(2,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*yij
        partial%sa_drhototij(3,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*zij
        partial%sa_drhototik(1,np) = drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*xik
        partial%sa_drhototik(2,np) = drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*yik
        partial%sa_drhototik(3,np) = drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*zik
        partial%sa_drhototjk(1,np) = drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*xjk
        partial%sa_drhototjk(2,np) = drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*yjk
        partial%sa_drhototjk(3,np) = drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*zjk
        if (lstr) then
          partial%sa_drhotots(1,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*xij*xij + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*xik*xik + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*xjk*xjk
          partial%sa_drhotots(2,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*yij*yij + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*yik*yik + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*yjk*yjk
          partial%sa_drhotots(3,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*zij*zij + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*zik*zik + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*zjk*zjk
          partial%sa_drhotots(4,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*yij*zij + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*yik*zik + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*yjk*zjk
          partial%sa_drhotots(5,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*xij*zij + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*xik*zik + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*xjk*zjk
          partial%sa_drhotots(6,np) = drhotot*Sij_rest*partial%sa_dSikjdr(1,np)*xij*yij + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(2,np)*xik*yik + &
                                      drhotot*Sij_rest*partial%sa_dSikjdr(3,np)*xjk*yjk
        endif
      enddo
!
!  Having potentially finished using first derivative terms in second derivatives, multiply by derivative of functional w.r.t. total rho
!
      partial%sa_drhototij(1:3,1:npartial) = rscrhofn*partial%sa_drhototij(1:3,1:npartial)
      partial%sa_drhototik(1:3,1:npartial) = rscrhofn*partial%sa_drhototik(1:3,1:npartial)
      partial%sa_drhototjk(1:3,1:npartial) = rscrhofn*partial%sa_drhototjk(1:3,1:npartial)
      if (lstr) then
        partial%sa_drhotots(1:6,1:npartial) = rscrhofn*partial%sa_drhotots(1:6,1:npartial)
      endif
    endif
  endif
!
  return
  end
