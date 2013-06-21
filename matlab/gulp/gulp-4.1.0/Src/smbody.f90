  subroutine smbody(eqeq,lgrad1,lgrad2,nor,nor0,factor,qli,qlj,nati,natj)
!
!  Calculates the correction to the energy and derivative terms for the
!  Streitz-Mintmire scheme due to the use of integrals at short range.
!  Must follow call to twobody as this sets the derivative terms to zero.
!  Note that gam in this routine differs from that in qeqbody in that 1/r
!  is pre-subtracted in gammasm.
!
!   7/05 Created from qeqbody
!
!  eqeq      = correction to energy due to SM
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
  real(dp)                   :: dgam
  real(dp)                   :: dgamifj
  real(dp)                   :: dgamjfi
  real(dp)                   :: d2gamr2
  real(dp)                   :: d2gamifj
  real(dp)                   :: d2gamjfi
  real(dp)                   :: dtrm1
  real(dp)                   :: dtrm2
  real(dp)                   :: gam
  real(dp)                   :: gamifj
  real(dp)                   :: gamjfi
  real(dp)                   :: r
  real(dp)                   :: rrl
  real(dp)                   :: rrl2
  real(dp)                   :: zetai
  real(dp)                   :: zetaj
  real(dp)                   :: znuci
  real(dp)                   :: znucj
!
!  If this isn't a SM run then subroutine shouldn't have been called
!
  if (.not.lSandM) return
!***************************************
!  Calculate exponents for s orbitals  *
!***************************************
  zetai = smzeta(nati)
  zetaj = smzeta(natj)
  znuci = smZnuc(nati)
  znucj = smZnuc(natj)
!************************
!  Loop over distances  *
!************************
  do i = nor0,nor
    r = dist(i)
!
!  Exclude distances outside maximum cutoff and core-shell contacts
!
    if (r.lt.rqeq.and.r.gt.cuts) then
!***************************
!  QEq scheme corrections  *
!***************************
      call gammasm(zetai,zetaj,r,gam,dgam,d2gamr2,gamifj,gamjfi,dgamifj,dgamjfi,d2gamifj,d2gamjfi)
      rrl = 1.0_dp/r
      rrl2 = rrl*rrl
!
!  Energy
!
      eqeq = eqeq + qli*qlj*factor*gam
      eqeq = eqeq + factor*(qli*znucj*(gamjfi - gam) + qlj*znuci*(gamifj - gam))
      if (.not.lSandMnoZZ) then
        eqeq = eqeq + factor*znuci*znucj*(gam - gamifj - gamjfi)
      endif
!
      if (lgrad1) then
!
!  First derivatives
!
        dtrm1 = qli*qlj*factor*dgam*rrl
        dtrm1 = dtrm1 + factor*(qli*znucj*(dgamjfi - dgam) + qlj*znuci*(dgamifj - dgam))*rrl
        if (.not.lSandMnoZZ) then
          dtrm1 = dtrm1 + factor*znuci*znucj*(dgam - dgamifj - dgamjfi)*rrl
        endif
        if (lgrad2) then
!
!  Second derivatives
!
          dtrm2 = qli*qlj*factor*d2gamr2
          dtrm2 = dtrm2 + factor*(qli*znucj*(d2gamjfi - d2gamr2) + qlj*znuci*(d2gamifj - d2gamr2))
          if (.not.lSandMnoZZ) then
            dtrm2 = dtrm2 + factor*znuci*znucj*(d2gamr2 - d2gamifj - d2gamjfi)
          endif
          dtrm2 = rrl2*(dtrm2 - dtrm1)
          deriv2(i) = deriv2(i) + dtrm2
!
!  Charge derivative terms
!
!  Standard correction for gamma-1/r
!
          d1i(i) = d1i(i) + qlj*factor*dgam*rrl
          d1i(i) = d1i(i) + factor*znucj*(dgamjfi - dgam)*rrl
          d1j(i) = d1j(i) + qli*factor*dgam*rrl
          d1j(i) = d1j(i) + factor*znuci*(dgamifj - dgam)*rrl
          d2ij(i) = d2ij(i) + factor*gam
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
!  Wrapper for smbody
!*********************************************************************
  subroutine smbody1(eqeq,lgrad1,lgrad2,nor,nor0,dist1,deriv1, &
    deriv21,factor,qli,qlj,nati,natj,d1i1,d1j1,d2i21,d2ij1,d2j21)
!
!  Wrapper for call to smbody when number of distances is one.
!
!   7/05 Created from qeqbody1
!
!  eqeq      = correction to energy due to SM
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
  call smbody(eqeq,lgrad1,lgrad2,nor,nor0,factor,qli,qlj,nati,natj)
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
