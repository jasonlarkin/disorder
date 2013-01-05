  subroutine reaxffctfunct(iflag,nfree,nvari,numpf,listpfptr,listpf,xc,fc,gc,maxD,D,numD,listD,fij,chivec,qref,externalpot)
!
!  Computes energy and first derivatives of charge transfer function for reaxFF
!
!   4/10 Created from ctfunct
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
!  Julian Gale, NRI, Curtin University, April 2010
!
  use iochannels
  use parallel

  implicit none
!
!  Passed variables
!
  integer(i4),          intent(in)            :: iflag
  integer(i4),          intent(in)            :: nfree
  integer(i4),          intent(in)            :: nvari
  integer(i4),          intent(in)            :: maxD
  integer(i4),          intent(in)            :: numD(nfree)
  integer(i4),          intent(in)            :: numpf(nfree)
  integer(i4),          intent(in)            :: listD(maxD,nfree)
  integer(i4),          intent(in)            :: listpf(nvari)
  integer(i4),          intent(in)            :: listpfptr(nfree)
  real(dp),             intent(out)           :: fc
  real(dp),             intent(out)           :: gc(nvari)
  real(dp),             intent(in)            :: xc(nvari)
  real(dp),             intent(in)            :: chivec(nfree)
  real(dp),             intent(in)            :: qref(nfree)
  real(dp),             intent(in)            :: externalpot(nfree)
  real(dp),             intent(in)            :: D(maxD,*)
  real(dp),             intent(in)            :: fij(nvari)
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: ind
  integer(i4)                                 :: j
  integer(i4)                                 :: jj
  integer(i4)                                 :: status
  real(dp)                                    :: Jkl
  real(dp)                                    :: term
  real(dp),   dimension(:), allocatable, save :: q
  real(dp),   dimension(:), allocatable, save :: dEdq
!
!  Allocate workspace
!
  allocate(q(nfree),stat=status)
  if (status/=0) call outofmemory('reaxffctfunct','q')
  allocate(dEdq(nfree),stat=status)
  if (status/=0) call outofmemory('reaxffctfunct','dEdq')
!
!  Initialise function
!
  fc = 0.0_dp
!
!  Compute charges
!
  q(1:nfree) = qref(1:nfree)
  ind = 0
  do i = 1,nfree
    do jj = 1,numpf(i)
      ind = ind + 1
      j = listpf(ind)
      q(i) = q(i) + xc(ind)
      q(j) = q(j) - xc(ind)
    enddo
  enddo
! OLD algorithm - pre sparsity
!  do i = 2,nfree
!    do j = 1,i-1
!      ind = ind + 1
!      q(i) = q(i) + xc(ind)
!      q(j) = q(j) - xc(ind)
!    enddo
!  enddo
!
!  Compute energy and charge derivatives
!
  ind = 0
  do i = 1,nfree
    do jj = 1,numpf(i)
      ind = ind + 1
      j = listpf(ind)
      term = (chivec(i) - chivec(j))*fij(ind)
      fc = fc + term*xc(ind)
      gc(ind) = term
    enddo
  enddo
! OLD algorithm - pre sparsity
!  do i = 2,nfree
!    do j = 1,i-1
!      ind = ind + 1
!! NEED to handle fij
!!      term = (chivec(i) - chivec(j))*fij(j,i)
!      term = (chivec(i) - chivec(j))
!      fc = fc + term*xc(ind)
!      gc(ind) = term
!    enddo
!  enddo
  do i = 1,nfree
    Jkl = 0.0_dp
    do jj = 1,numD(i)
      j = listD(jj,i)
      Jkl = Jkl + q(j)*D(jj,i)
    enddo
    fc = fc + 0.5_dp*q(i)*Jkl + q(i)*externalpot(i)
    dEdq(i) = Jkl + externalpot(i)
  enddo
!
!  Compute derivatives of charge-transfer coefficients
!
  ind = 0
  do i = 1,nfree
    do jj = 1,numpf(i)
      ind = ind + 1
      j = listpf(ind)
      gc(ind) = gc(ind) + dEdq(i) - dEdq(j)
    enddo
  enddo
!  do i = 2,nfree
!    do j = 1,i-1
!      ind = ind + 1
!      gc(ind) = gc(ind) + dEdq(i) - dEdq(j)
!    enddo
!  enddo
!
!  Deallocate workspace
!
  deallocate(dEdq,stat=status)
  if (status/=0) call deallocate_error('reaxffctfunct','dEdq')
  deallocate(q,stat=status)
  if (status/=0) call deallocate_error('reaxffctfunct','q')
!
  return
  end
