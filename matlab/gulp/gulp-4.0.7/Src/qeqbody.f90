  subroutine qeqbody(eqeq,lgrad1,lgrad2,nor,nor0,factor,qli,qlj,nati,natj)
!
!  Calculates the correction to the energy and derivative terms
!  for the QEq scheme due to the use of integrals at short range.
!  Must follow call to twobody as this sets the derivative terms
!  to zero.
!
!   1/98 Created from dgenpot
!
!  eqeq      = correction to energy due to QEq
!  lgrad1    = .true. if first derivatives are needed
!  lgrad2    = .true. if second derivatives are needed
!  nor       = upper bound to distances in array
!  nor0      = lower bound to distances in array
!  dist      = array of distances
!  deriv     = first derivative on return
!  deriv2    = second derivative on return
!  factor    = product of occupancies / sym factor
!  qli       = charge on i
!  qlj       = charge on j
!  nati      = atomic number of i
!  natj      = atomic number of j
!  d1i       = derivative of first derivative with respect to qi
!  d1j       = derivative of first derivative with respect to qj
!  d2i2      = 2nd derivative of energy with respect to qi/qi
!  d2ij      = 2nd derivative of energy with respect to qi/qj
!  d2j2      = 2nd derivative of energy with respect to qj/qj
!
!  NOTE : this routine only adds the gamma related contribution
!         to the above 5 terms - the rest is calculated in 
!         twobody/calling routine
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
  use element
  use realvectors
  use shell
  implicit none
!  
!  Passed variables
!     
  integer(i4), intent(in)    :: nor
  integer(i4), intent(in)    :: nor0
  integer(i4), intent(in)    :: nati
  integer(i4), intent(in)    :: natj
  logical,     intent(in)    :: lgrad1
  logical,     intent(in)    :: lgrad2
  real(dp),    intent(inout) :: eqeq
  real(dp),    intent(in)    :: factor
  real(dp),    intent(in)    :: qli
  real(dp),    intent(in)    :: qlj
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: npqni
  integer(i4)                :: npqnj
  real(dp)                   :: dgam
  real(dp)                   :: d2gamr2
  real(dp)                   :: dtrm1
  real(dp)                   :: dtrm2
  real(dp)                   :: dzetai
  real(dp)                   :: dzetaj
  real(dp)                   :: d2zetaii
  real(dp)                   :: d2zetaij
  real(dp)                   :: d2zetajj
  real(dp)                   :: d2zetari
  real(dp)                   :: d2zetarj
  real(dp)                   :: gam
  real(dp)                   :: r
  real(dp)                   :: r5
  real(dp)                   :: rrl
  real(dp)                   :: rrl2
  real(dp)                   :: zetai
  real(dp)                   :: zetaj
!
!  If this isn't a QEq run then subroutine shouldn't have been called
!
  if (.not.lqeq) return
!***************************************
!  Work out principal quantum numbers  *
!***************************************
  if (nati.le.2) then
    npqni = 1
  elseif (nati.le.10) then
    npqni = 2
  elseif (nati.le.18) then
    npqni = 3
  elseif (nati.le.36) then
    npqni = 4
  elseif (nati.le.54) then
    npqni = 5
  elseif (nati.le.86) then
    npqni = 6
  else
    npqni = 7
  endif
  if (natj.le.2) then
    npqnj = 1
  elseif (natj.le.10) then
    npqnj = 2
  elseif (natj.le.18) then
    npqnj = 3
  elseif (natj.le.36) then
    npqnj = 4
  elseif (natj.le.54) then
    npqnj = 5
  elseif (natj.le.86) then
    npqnj = 6
  else
    npqnj = 7
  endif
!***************************************
!  Calculate exponents for s orbitals  *
!***************************************
  zetai = 0.5_dp*qeqlambda*(2*npqni+1)/qeqrad(nati)
  zetaj = 0.5_dp*qeqlambda*(2*npqnj+1)/qeqrad(natj)
  r5 = 1.0_dp/0.529177_dp
  if (nati.eq.1) then
!
!  Special case for hydrogen
!
    zetai = zetai + qli*r5
  endif
  if (natj.eq.1) then
!
!  Special case for hydrogen
!
    zetaj = zetaj + qlj*r5
  endif
!************************
!  Loop over distances  *
!************************
  do i = nor0,nor
    r = dist(i)
!
!  Exclude distances outside maximum cutoff and
!  core-shell contacts
!
    if (r.lt.rqeq.and.r.gt.cuts) then
!***************************
!  QEq scheme corrections  *
!***************************
      call gammas(npqni,npqnj,zetai,zetaj,r,gam,dgam,dzetai,dzetaj,d2zetaii, &
        d2zetaij,d2zetajj,d2zetari,d2zetarj,d2gamr2)
      rrl = 1.0_dp/r
      rrl2 = rrl*rrl
!
!  Energy
!
      eqeq = eqeq + qli*qlj*factor*(gam - rrl)
      if (lgrad1) then
!
!  First derivatives
!
        dtrm1 = qli*qlj*factor*(dgam + rrl2)*rrl
        if (lgrad2) then
!
!  Second derivatives
!
          dtrm2 = qli*qlj*factor*(d2gamr2 - 2.0_dp*rrl2*rrl)
          dtrm2 = rrl2*(dtrm2 - dtrm1)
          deriv2(i) = deriv2(i) + dtrm2
!
!  Charge derivative terms
!
!  Standard correction for gamma-1/r
!
          d1i(i) = d1i(i) + qlj*factor*(dgam + rrl2)*rrl
          d1j(i) = d1j(i) + qli*factor*(dgam + rrl2)*rrl
          d2ij(i) = d2ij(i) + factor*(gam - rrl)
!
!  Correction for gamma derivatives with respect to charge
!
          if (nati.eq.1) then
            d1i(i) = d1i(i) + qli*qlj*factor*rrl*d2zetari*r5
            d2ij(i) = d2ij(i) + factor*qli*dzetai*r5
            if (natj.eq.1) then
              d2ij(i) = d2ij(i) + factor*(qli*qlj*d2zetaij)*r5*r5
            endif
            d2i2(i) = d2i2(i) + 2.0_dp*factor*qlj*dzetai*r5
            d2i2(i) = d2i2(i) + factor*qli*qlj*d2zetaii*r5*r5
          endif
          if (natj.eq.1) then
            d1j(i) = d1j(i) + qli*qlj*factor*rrl*d2zetarj*r5
            d2ij(i) = d2ij(i) + factor*qlj*dzetaj*r5
            d2j2(i) = d2j2(i) + 2.0_dp*factor*qli*dzetaj*r5
            d2j2(i) = d2j2(i) + factor*qli*qlj*d2zetajj*r5*r5
          endif
        endif
        deriv(i) = deriv(i) + dtrm1
      endif
    endif
!*******************************
!  End of loop over distances  *
!*******************************
  enddo
!
  return
  end
!*********************************************************************
!  Wrapper for qeqbody
!*********************************************************************
  subroutine qeqbody1(eqeq,lgrad1,lgrad2,nor,nor0,dist1,deriv1, &
    deriv21,factor,qli,qlj,nati,natj,d1i1,d1j1,d2i21,d2ij1,d2j21)
!
!  Wrapper for call to qeqbody when number of distances is one.
!
!   2/01 Created qeqbody
!
!  eqeq      = correction to energy due to QEq
!  lgrad1    = .true. if first derivatives are needed
!  lgrad2    = .true. if second derivatives are needed
!  nor       = upper bound to distances in array
!  nor0      = lower bound to distances in array
!  dist      = array of distances
!  deriv     = first derivative on return
!  deriv2    = second derivative on return
!  factor    = product of occupancies / sym factor
!  qli       = charge on i
!  qlj       = charge on j
!  nati      = atomic number of i
!  natj      = atomic number of j
!  d1i       = derivative of first derivative with respect to qi
!  d1j       = derivative of first derivative with respect to qj
!  d2i2      = 2nd derivative of energy with respect to qi/qi
!  d2ij      = 2nd derivative of energy with respect to qi/qj
!  d2j2      = 2nd derivative of energy with respect to qj/qj
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
  use realvectors
  implicit none
!     
!  Passed variables
!     
  integer(i4), intent(in)    :: nor
  integer(i4), intent(in)    :: nor0
  integer(i4), intent(in)    :: nati
  integer(i4), intent(in)    :: natj
  logical,     intent(in)    :: lgrad1
  logical,     intent(in)    :: lgrad2
  real(dp),    intent(inout) :: d1i1
  real(dp),    intent(inout) :: d1j1
  real(dp),    intent(inout) :: d2i21
  real(dp),    intent(inout) :: d2ij1
  real(dp),    intent(inout) :: d2j21
  real(dp),    intent(inout) :: deriv1
  real(dp),    intent(inout) :: deriv21
  real(dp),    intent(inout) :: dist1
  real(dp),    intent(inout) :: eqeq
  real(dp),    intent(in)    :: factor
  real(dp),    intent(in)    :: qli
  real(dp),    intent(in)    :: qlj
!
!  Set variables that should be in realvectors module
!
  dist(1) = dist1
  deriv(1) = deriv1
  deriv2(1) = deriv21
  d1i(1) = d1i1
  d1j(1) = d1j1
  d2i2(1) = d2i21
  d2ij(1) = d2ij1
  d2j2(1) = d2j21
!
!  Call qeqbody
!
  call qeqbody(eqeq,lgrad1,lgrad2,nor,nor0,factor,qli,qlj,nati,natj)
!
!  Set return variables from realvectors module
!
  dist1 = dist(1)
  deriv1 = deriv(1)
  deriv21 = deriv2(1)
  d1i1 = d1i(1)
  d1j1 = d1j(1)
  d2i21 = d2i2(1)
  d2ij1 = d2ij(1)
  d2j21 = d2j2(1)
!
  return
  end
