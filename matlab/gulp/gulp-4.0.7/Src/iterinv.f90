  subroutine iterinv(n,A,lda,B,C,info)
!
!  Subroutine to interface to itpack subroutines for iterative solution of Ax = b
!
!  The solution is returned in vector C. Since this is an iterative approach,
!  the initial value of C is used as the initial guess.
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(out)       :: info
  integer(i4),  intent(in)        :: lda
  integer(i4),  intent(in)        :: n
  real(dp),     intent(inout)     :: A(lda,n)
  real(dp),     intent(in)        :: B(n)
  real(dp),     intent(inout)     :: C(n)
!
!  Local variables
!
  integer(i4)                     :: i
  integer(i4)                     :: iparm(12)
  integer(i4),  allocatable       :: iwksp(:)
  integer(i4)                     :: j
  integer(i4),  allocatable       :: jcoef(:,:)
  integer(i4)                     :: maxnonzero
  integer(i4)                     :: nonzero
  integer(i4)                     :: nw
  integer(i4)                     :: status
  real(dp)                        :: rparm(12)
  real(dp),     allocatable       :: wksp(:)
!
!  Set sparsity pattern equal to dense form
!
  maxnonzero = n
  allocate(jcoef(n,maxnonzero),stat=status)
  if (status/=0) call outofmemory('iterinv','jcoef')
  jcoef = 0
  do i = 1,n
    nonzero = 0
    do j = 1,n
      nonzero = nonzero + 1
      jcoef(i,nonzero) = j
    enddo
  enddo
!
!  Set parameters for itpack
!
  call dfault(iparm,rparm)
  rparm(1) = 1.0d-8
  nw = 6*n + 400
!
!  Allocate local workspace
!
  allocate(iwksp(3*n),stat=status)
  if (status/=0) call outofmemory('iterinv','iwksp')
  allocate(wksp(nw),stat=status)
  if (status/=0) call outofmemory('iterinv','wksp')
!
!  Reverse signs on C
!
  do i = 1,n
    C(i) = - C(i)
  enddo
!
!  Solve using iterative route
!
  call jcg(n,n,maxnonzero,jcoef,A,B,C,iwksp,nw,wksp,iparm,rparm,info)
!
!  Reverse signs on C back again
!
  do i = 1,n
    C(i) = - C(i)
  enddo
!
!  Free local workspace
!
  deallocate(wksp,stat=status)
  if (status/=0) call deallocate_error('iterinv','wksp')
  deallocate(iwksp,stat=status)
  if (status/=0) call deallocate_error('iterinv','iwksp')
  deallocate(jcoef,stat=status)
  if (status/=0) call deallocate_error('iterinv','jcoef')
!
  return
  end
