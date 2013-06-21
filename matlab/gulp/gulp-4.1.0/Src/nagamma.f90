  subroutine nagamma(ncfoc,nphonatc,iocptr,rmass,qfrac,maxd2,derv2)
!
!  Calculates the non-analytic correction to the second
!  derivatives based on the Born effective charges.
!
!   3/02 Created
!   4/02 Gamma point approach direction now passed as argument
!        and converted to Cartesian space from fractional
!   7/02 Modified to allow for the possible handling of 2-D case later
!  11/04 Pi accessed from module
!
!  On entry :
!
!  ncfoc    = no. of reduced core sites 
!  nphonatc = no. of cores included in phonon calculation
!  iocptr   = pointer to core sites from reduced list
!  rmass    = inverse square root of atomic masses
!  qfrac    = fractional approach direction to gamma point
!  maxd2    = left-hand dimension of derv2
!  derv2    = uncorrected dynamical matrix
!
!  On exit :
!
!  derv2  = corrected dynamical matrix
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, November 2004
!
  use constants
  use control,        only : lkfull
  use current,        only : bornq, kv, rv
  use properties
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4)                                   :: iocptr(*)
  integer(i4)                                   :: maxd2
  integer(i4)                                   :: ncfoc
  integer(i4)                                   :: nphonatc
  real(dp)                                      :: qfrac(3)
  real(dp)                                      :: rmass(*)
  real(dp)                                      :: derv2(maxd2,*)
!
!  Local variables
!
  integer(i4)                                   :: i
  integer(i4)                                   :: ii
  integer(i4)                                   :: ix
  integer(i4)                                   :: iy
  integer(i4)                                   :: iz
  integer(i4)                                   :: j
  integer(i4)                                   :: jx
  integer(i4)                                   :: jy
  integer(i4)                                   :: jz
  integer(i4)                                   :: status
  real(dp),   dimension(:,:), allocatable       :: qz
  real(dp)                                      :: factor
  real(dp)                                      :: inveps
  real(dp)                                      :: kvf(3,3)
  real(dp)                                      :: mfactor
  real(dp)                                      :: qcart(3)
  real(dp)                                      :: vol
  real(dp)                                      :: volume
!
!  Calculate constant = 4*pi/V
!
  vol = volume(rv)
  factor = 4.0_dp*pi/vol
!
!  Select appropriate K vectors
!
  if (lkfull) then
    call kvector3Df(kvf) 
  else
    kvf(1:3,1:3) = kv(1:3,1:3)
  endif
!     
!  Calculate Cartesian space direction
!       
  qcart(1) = qfrac(1)*kvf(1,1) + qfrac(2)*kvf(1,2) + qfrac(3)*kvf(1,3)
  qcart(2) = qfrac(1)*kvf(2,1) + qfrac(2)*kvf(2,2) + qfrac(3)*kvf(2,3)
  qcart(3) = qfrac(1)*kvf(3,1) + qfrac(2)*kvf(3,2) + qfrac(3)*kvf(3,3)
!
!  Build arrays of Q.Z
!
  allocate(qz(3,ncfoc),stat=status)
  if (status/=0) call outofmemory('nagamma','qz')
  qz(1:3,1:ncfoc) = 0.0_dp
  do i = 1,nphonatc
    ii = iocptr(i)
    qz(1,ii) = qz(1,ii) + qcart(1)*bornq(1,1,i) + qcart(2)*bornq(2,1,i) + qcart(3)*bornq(3,1,i)
    qz(2,ii) = qz(2,ii) + qcart(1)*bornq(1,2,i) + qcart(2)*bornq(2,2,i) + qcart(3)*bornq(3,2,i)
    qz(3,ii) = qz(3,ii) + qcart(1)*bornq(1,3,i) + qcart(2)*bornq(2,3,i) + qcart(3)*bornq(3,3,i)
  enddo
!
!  Calculate Q.epsilon(static).Q and invert
!
  inveps = qcart(1)*diconh(1,1)*qcart(1) + qcart(2)*diconh(2,1)*qcart(1) + qcart(3)*diconh(3,1)*qcart(1) + &
           qcart(1)*diconh(1,2)*qcart(2) + qcart(2)*diconh(2,2)*qcart(2) + qcart(3)*diconh(3,2)*qcart(2) + &
           qcart(1)*diconh(1,3)*qcart(3) + qcart(2)*diconh(2,3)*qcart(3) + qcart(3)*diconh(3,3)*qcart(3)
  inveps = 1.0_dp/inveps
!
!  Loop over second derivative matrix elements adding terms
!
  factor = factor*inveps*angstoev
  ix = - 2
  iy = - 1
  iz =   0
  do i = 1,ncfoc
    ix = ix + 3
    iy = iy + 3
    iz = iz + 3
    jx = - 2
    jy = - 1
    jz =   0
    do j = 1,ncfoc
      jx = jx + 3
      jy = jy + 3
      jz = jz + 3
      mfactor = factor*rmass(i)*rmass(j)
      derv2(jx,ix) = derv2(jx,ix) + mfactor*qz(1,j)*qz(1,i)
      derv2(jy,ix) = derv2(jy,ix) + mfactor*qz(2,j)*qz(1,i)
      derv2(jz,ix) = derv2(jz,ix) + mfactor*qz(3,j)*qz(1,i)
      derv2(jx,iy) = derv2(jx,iy) + mfactor*qz(1,j)*qz(2,i)
      derv2(jy,iy) = derv2(jy,iy) + mfactor*qz(2,j)*qz(2,i)
      derv2(jz,iy) = derv2(jz,iy) + mfactor*qz(3,j)*qz(2,i)
      derv2(jx,iz) = derv2(jx,iz) + mfactor*qz(1,j)*qz(3,i)
      derv2(jy,iz) = derv2(jy,iz) + mfactor*qz(2,j)*qz(3,i)
      derv2(jz,iz) = derv2(jz,iz) + mfactor*qz(3,j)*qz(3,i)
    enddo
  enddo
!
!  Free local memory
!
  deallocate(qz,stat=status)
  if (status/=0) call deallocate_error('nagamma','qz')
!
  return
  end
