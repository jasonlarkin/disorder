  subroutine einsteinsd2(eeinstein,lgrad1,lgrad2)
!
!  Subroutine for calculating the energy and derivatives due to the Einstein model
!  Symmetry adapted version
!
!  11/02 Created 
!  11/07 Unused variables removed
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
!  Julian Gale, NRI, Curtin University, November 2007
!
  use current
  use configurations
  use derivatives
  use optimisation,   only : lopf, lfreeze
  implicit none
!
!  Passed variables
!
  logical,  intent(in)  :: lgrad1
  logical,  intent(in)  :: lgrad2
  real(dp), intent(out) :: eeinstein
!
!  Local variables
!
  integer(i4)           :: i
  integer(i4)           :: ix
  integer(i4)           :: iy
  integer(i4)           :: iz
  integer(i4)           :: ixfo
  integer(i4)           :: iyfo
  integer(i4)           :: izfo
  real(dp)              :: ki
  real(dp)              :: xf
  real(dp)              :: yf
  real(dp)              :: zf
  real(dp)              :: xi
  real(dp)              :: yi
  real(dp)              :: zi
!
!  Initialise integral of force x distance
!
  eeinstein = 0.0_dp
!
!  If there are no Einstein model atoms return
!
  if (.not.leinstein) return
!
!  Initialise second derivative pointers
!
  if (lgrad2) then
    ix = -2
    iy = -1
    iz = 0
    ixfo = 1
    iyfo = 2
    izfo = 3
  endif
!
!  Loop over asymmetric unit performing integral
!
  do i = 1,nasym
    if (lopf(i).or..not.lfreeze) then
      if (lgrad2) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        ixfo = ixfo + 3*neqv(i)
        iyfo = iyfo + 3*neqv(i)
        izfo = izfo + 3*neqv(i)
      endif
      if (leinsteinat(nsft+i)) then
!
!  Find nearest image in fraction coordinates
!
        xf = xafrac(i) - xeinsteinat(nsft + i)
        yf = yafrac(i) - yeinsteinat(nsft + i)
        zf = zafrac(i) - zeinsteinat(nsft + i)
        xf = mod(xf+10.0_dp,1.0_dp)
        yf = mod(yf+10.0_dp,1.0_dp)
        zf = mod(zf+10.0_dp,1.0_dp)
        if (xf.gt.0.5_dp) xf = xf - 1.0_dp
        if (yf.gt.0.5_dp) yf = yf - 1.0_dp
        if (zf.gt.0.5_dp) zf = zf - 1.0_dp
!
!  Convert coordinate difference into Cartesian frame
!     
        xi = xf*rv(1,1) + yf*rv(1,2) + zf*rv(1,3)
        yi = xf*rv(2,1) + yf*rv(2,2) + zf*rv(2,3)
        zi = xf*rv(3,1) + yf*rv(3,2) + zf*rv(3,3)
!
!  Evaluate energy and derivatives
!
        ki = keinsteinat(nsft + i)*dble(neqv(i))
        eeinstein = eeinstein + 0.5_dp*ki*(xi*xi + yi*yi + zi*zi)
        if (lgrad1) then
          xdrv(i) = xdrv(i) + ki*xi
          ydrv(i) = ydrv(i) + ki*yi
          zdrv(i) = zdrv(i) + ki*zi
          if (lgrad2) then
            derv2d(ix) = derv2d(ix) + ki
            derv2d(iy) = derv2d(iy) + ki
            derv2d(iz) = derv2d(iz) + ki
          endif
        endif
      endif
    endif
  enddo
!
  return
  end
