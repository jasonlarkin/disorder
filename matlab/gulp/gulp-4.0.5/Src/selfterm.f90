  subroutine selfterm(ereal,erecip,ec6,derive0self,factor,fct,ofct,dfct,npotl,npots, &
                      c6tot,d2self,lgrad1,lgrad2,i,j,ix,jx,escale,c6scale,lewaldtype, &
                      qli,qlj)
!
!  Calculates self term contributions to energy and second derivatives.
!
!   7/00 Created from reale/realsd/realsd2 code
!   1/01 c6scale added to fix problems with C6 self term
!        in realmd3/reale
!   5/02 New flag passed to indicate whether this is an Ewald-type
!        summation or not.
!   1/03 Wolf modifications made
!   9/04 derive0self term added - Coulomb contribution without charges
!   4/05 Modifications for cosh-spring made
!   5/07 Check added that i and j are matching core-shell pair
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   6/09 Charges now passed in as arguments
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use control
  use current
  use derivatives, only : derv2
  use general,     only : etaw
  use kspace
  use numbers,     only : third
  use shell,       only : ncsptr
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: i            ! Atom number for first atom i
  integer(i4), intent(in)    :: ix           ! Coordinate index for i / x
  integer(i4), intent(in)    :: j            ! Atom number for second atom j
  integer(i4), intent(in)    :: jx           ! Coordinate index for j / x
  integer(i4), intent(in)    :: npotl(*)     ! Array containing a list of valid potentials
  integer(i4), intent(in)    :: npots        ! Number of valid potentials
  logical,     intent(in)    :: lewaldtype   ! Flag to indicate whether Ewald sum is being performed
  logical,     intent(in)    :: lgrad1       ! If .true. then calculate first derivatives
  logical,     intent(in)    :: lgrad2       ! If .true. then calculate second derivatives
  real(dp),    intent(in)    :: c6scale
  real(dp),    intent(in)    :: c6tot
  real(dp),    intent(inout) :: d2self
  real(dp),    intent(inout) :: derive0self
  real(dp),    intent(in)    :: dfct
  real(dp),    intent(inout) :: ec6
  real(dp),    intent(inout) :: ereal
  real(dp),    intent(inout) :: erecip
  real(dp),    intent(in)    :: escale
  real(dp),    intent(in)    :: factor       ! Charges of i and j times fct
  real(dp),    intent(in)    :: fct          ! ofct times 1/r -> eV conversion factor
  real(dp),    intent(in)    :: ofct         ! Product of occupancies of i and j
  real(dp),    intent(in)    :: qli          ! Charge of i
  real(dp),    intent(in)    :: qlj          ! Charge of j
!
!  Local variables
!
  integer(i4)                :: iy
  integer(i4)                :: iz
  integer(i4)                :: jy
  integer(i4)                :: jz
  integer(i4)                :: k
  integer(i4)                :: npot
  integer(i4)                :: npt
  logical                    :: lc6loc
  real(dp)                   :: apt
  real(dp)                   :: c6prod
  real(dp)                   :: c6self1
  real(dp)                   :: c6self1d2
  real(dp)                   :: eta3
  real(dp)                   :: setrm
  real(dp)                   :: twoeta3
!
  lc6loc = (lc6.and.ndim.eq.3)
!
!  Set up second derivative position pointers
!
  if (lgrad2.and.i.ne.j) then
    iy = ix + 1
    iz = ix + 2
    jy = jx + 1
    jz = jx + 2
  endif
!
!  Core-shell spring constant at zero distant correct second derivative matrix
!
  if (lgrad2.and.i.ne.j.and.ncsptr(i).eq.j) then
    do k = 1,npots
      npot = npotl(k)
      npt = nptype(npot)
      if (npt.eq.5.or.npt.eq.8.or.npt.eq.33) then
        apt = twopot(1,npot)*ofct*dfct
        derv2(jx,ix) = derv2(jx,ix) - apt
        derv2(jy,iy) = derv2(jy,iy) - apt
        derv2(jz,iz) = derv2(jz,iz) - apt
      endif
    enddo
  endif
  if (lewaldtype) then
!
!  Remove self energy from Ewald/Wolf sums (charge and C6)
!
    if (lwolf) then
      twoeta3 = 2.0_dp*etaw*etaw*third
      setrm = factor*tweatpi
      d2self = d2self - fct*tweatpi
      if (lgrad1) then
        derive0self = derive0self - tweatpi*fct*escale
      endif
    else
      twoeta3 = 2.0_dp*eta*third
      setrm = factor*tweatpi
      erecip = erecip - setrm*escale
      d2self = d2self - fct*tweatpi
      if (lgrad1) then
        derive0self = derive0self - tweatpi*fct*escale
      endif
    endif
    if (lc6loc) then
      eta3 = eta*eta*eta
      c6self1 = eta3*third*c6scale
      if (lc6loc.and.lc6one) then
        c6prod = ofct*c6f(i)*c6f(j)
      else
        c6prod = ofct*c6tot
      endif
      ec6 = ec6 + c6self1*c6prod*escale
    endif
    if (lgrad2.and.i.ne.j) then
      setrm = setrm*twoeta3
      if (lc6loc) then
        c6self1d2 = 0.25_dp*eta3*eta
        setrm = setrm - c6prod*c6self1d2
      endif
      setrm = dfct*setrm
      derv2(jx,ix) = derv2(jx,ix) - setrm
      derv2(jy,iy) = derv2(jy,iy) - setrm
      derv2(jz,iz) = derv2(jz,iz) - setrm
    endif
  endif
!
  return
  end
