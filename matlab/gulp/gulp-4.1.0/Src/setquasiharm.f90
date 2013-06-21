  subroutine setquasiharm(dsqh,mint)
!
!  Calculates the matrix which maps the atomic free energy
!  derivatives on to strain for the quasiharmonic approxn.
!
!   9/97 Created
!  12/00 Generalised to any number of dimensions
!   2/03 matinv replaced to accelerate speed
!
!  On entry :
!
!    derv2 = internal-internal second derivative matrix
!    dsqh  = external-internal second derivative matrix
!
!  On exit :
!
!    derv2 = inverse of internal-internal second derivatives
!    dsqh  = product of inverse 2nd derivs and external-
!            internal matrix
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
!  Julian Gale, NRI, Curtin University, June 2005
!
  use current,    only : nstrains
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: mint
  real(dp)                                     :: dsqh(maxd2,*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifail
  integer(i4), dimension(:), allocatable       :: ipivot
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: n
  integer(i4)                                  :: status
  real(dp),    dimension(:), allocatable       :: dpacked
  real(dp),    dimension(:), allocatable       :: wrk
!************************************
!  Invert second derivative matrix  *
!************************************
  ifail = 0
  n = mint - 3
  allocate(dpacked(n*(n+1)/2),stat=status)
  if (status/=0) call outofmemory('setquasiharm','dpacked')
  allocate(ipivot(n),stat=status)
  if (status/=0) call outofmemory('setquasiharm','ipivot')
  allocate(wrk(3*n),stat=status)
  if (status/=0) call outofmemory('setquasiharm','wrk')
!
!  Copy data and remove first atom to avoid singularity
!
  k = 0
  do i = 4,mint
    do j = 4,i
      k = k + 1
      dpacked(k) = derv2(j,i)
    enddo
  enddo
!
  call dsptrf('U',n,dpacked,ipivot,ifail)
  if (ifail.eq.0) then
    call dsptri('U',n,dpacked,ipivot,wrk,ifail)
!
    k = 0
    do i = 1,n
      do j = 1,i
        k = k + 1
        derv2(j,i) = dpacked(k)
        derv2(i,j) = dpacked(k)
      enddo
    enddo
  endif
!
!  Free local workspace arrays
!
  deallocate(wrk,stat=status)
  if (status/=0) call deallocate_error('setquasiharm','wrk')
  deallocate(ipivot,stat=status)
  if (status/=0) call deallocate_error('setquasiharm','ipivot')
  deallocate(dpacked,stat=status)
  if (status/=0) call deallocate_error('setquasiharm','dpacked')
!************************************************************************
!  Multiply inverse second derivatives by mixed strain internal matrix  *
!************************************************************************
  do i = 1,nstrains
    dsqh(1,i) = 0.0_dp
    dsqh(2,i) = 0.0_dp
    dsqh(3,i) = 0.0_dp
    do j = 4,mint
      dsqh(j,i) = 0.0_dp
      do k = 4,mint
        dsqh(j,i) = dsqh(j,i) + derv3(k,i)*derv2(k-3,j-3)
      enddo
    enddo
  enddo
!
  return
  end
