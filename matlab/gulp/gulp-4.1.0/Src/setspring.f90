  subroutine setspring(spring,bspring)
!
!  Sets up spring constants used to minimise shell variables
!  during a molecular dynamics run.
!
!   3/97 Created from part of runmd due to JRH
!   4/05 Modified to handle cosh-spring potential
!   5/07 nbsmptr replaced by nbsptr
!  10/08 Factor of a half removed from scaling of spring constant
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
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
!  Julian Gale, NRI, Curtin University, January 2009
!
  use current
  use element, only : maxele
  use moldyn
  use partial, only : nbsptr
  use shell
  use two
  implicit none
!
!  Passed variables
!
  real(dp), intent(out) :: bspring(*)
  real(dp), intent(out) :: spring(*)
!
!  Local variables
!
  integer(i4)           :: i
  integer(i4)           :: ibp
  integer(i4)           :: icp
  integer(i4)           :: isp
  integer(i4)           :: j
  integer(i4)           :: ntb
  integer(i4)           :: ntc
!
!  Create list of core-shell spring constants for each pair
!  to use in Newton-Raphson optimisation if shell positions
!  are to be optimised
!
  if (nshell.ne.0) then
    do i = 1,nshell
      isp = nshptr(i)
      icp = ncsptr(isp)
      isp = nat(isp)
      ntc = nftype(icp)
      icp = nat(icp)
      spring(i) = 0.0_dp
      do j = 1,npote
        if (nptype(j).eq.5.or.nptype(j).eq.8) then
          if ((nspec1(j).eq.icp.and.nspec2(j).eq.isp).and.(ntc.eq.nptyp1(j).or.nptyp1(j).eq.0)) then
            spring(i) = spring(i) + 0.5_dp/twopot(1,j)
          endif
        elseif (nptype(j).eq.33) then
          if ((nspec1(j).eq.icp.and.nspec2(j).eq.isp).and.(ntc.eq.nptyp1(j).or.nptyp1(j).eq.0)) then
!
!  The following is an approximation since the force constant for this potential is r dependent
!
            spring(i) = spring(i) + 0.5_dp/twopot(1,j)
          endif
        endif
      enddo
    enddo
  endif
!
!  Create a list of breathing shell spring constants for
!  use in Newton-Raphson optimisation if radii are to be
!  optimised
!
  if (nbsmat.ne.0) then
    do i = 1,nbsmat
      ibp = nbsptr(i)
      ntb = nftype(ibp)
      ibp = nat(ibp)
      if (ibp.gt.maxele) ibp = ibp - maxele
      bspring(i) = 0.0_dp
      do j = 1,npote
        if (nptype(j).eq.14.or.nptype(j).eq.17) then
          if (nspec1(j).eq.ibp.and.(nptyp1(j).eq.ntb.or.nptyp1(j).eq.0)) then
            if (nptype(j).eq.14) then
              bspring(i) = bspring(i) + 0.5_dp/twopot(1,j)
            else
!
!  This is only approximate as second derivative is not
!  a constant for this functional form, but should work
!  OK and avoid recalculation at every step. 
!
              bspring(i) = bspring(i) + 0.25_dp/(twopot(1,j)*twopot(2,j)**2)
            endif
          endif
        elseif (nptype(j).eq.31) then
          call outerror('bsm single_exponential unsuited to this form of MD',0_i4)
        endif
      enddo
    enddo
  endif
!
  return
  end
