  subroutine bcgsolve(n,nloc,nlocptr,A,maxnumA,numA,nAdiag,listA,b,x,bn1,tol,itmax,iter,err)

  use datatypes
  use iochannels
  use control,     only : keyword
  use parallel,    only : ioproc

  implicit none
!
! Passed variables
!
  integer(i4), intent(in)    :: n                  ! Total dimension of problem
  integer(i4), intent(in)    :: nloc               ! Number of columns of A local to this node
  integer(i4), intent(in)    :: nlocptr(nloc)      ! Pointer from local columns to global number
  integer(i4), intent(in)    :: itmax              ! Maximum number of iterations
  integer(i4), intent(out)   :: iter               ! Number of iterations taken
  integer(i4), intent(in)    :: maxnumA            ! LHS dimension of listA and A
  integer(i4), intent(in)    :: numA(n)            ! Number of non-zero elements of column of A
  integer(i4), intent(in)    :: nAdiag(n)          ! Pointer to diagonal element of A in column
  integer(i4), intent(in)    :: listA(maxnumA,n)   ! Pointer to non-zero elements of column of A
  real(dp),    intent(in)    :: A(maxnumA,n)       ! Values of non-zero elements of column of A
  real(dp),    intent(in)    :: tol                ! Tolerance
  real(dp),    intent(in)    :: b(n)               ! Left-hand vector
  real(dp),    intent(in)    :: bn1
  real(dp),    intent(inout) :: x(n)               ! Solution vector : guess on input, actual on output
  real(dp),    intent(out)   :: err                ! Error flag
!
! Local variables
!
  integer(i4)                :: j
  logical                    :: converged
  real(dp)                   :: ak
  real(dp)                   :: akden
  real(dp)                   :: bk
  real(dp)                   :: bkden
  real(dp)                   :: bknum
  real(dp)                   :: bnrm
  real(dp)                   :: ddot
  real(dp),    allocatable   :: p(:)
  real(dp),    allocatable   :: pp(:)
  real(dp),    allocatable   :: r(:)
  real(dp),    allocatable   :: rr(:)
  real(dp),    allocatable   :: z(:)
  real(dp),    allocatable   :: zz(:)
!
! Allocate workspace
!
  allocate(p(n))
  allocate(pp(n))
  allocate(r(n))
  allocate(rr(n))
  allocate(z(n))
  allocate(zz(n))

  call sparseAxV(n,nloc,nlocptr,A,maxnumA,numA,listA,x,r)

  do j = 1,n
    r(j)  = b(j) - r(j)
    rr(j) = r(j)
  enddo

  bnrm = ddot(n,b,1_i4,b,1_i4)
  call sparseAdiagprecon(n,nloc,nlocptr,A,maxnumA,numA,nAdiag,listA,r,z)
!
! Main loop
!
  iter = 0
  converged = .false.
  do while (iter.le.itmax.and..not.converged)
    iter = iter + 1
    call sparseAdiagprecon(n,nloc,nlocptr,A,maxnumA,numA,nAdiag,listA,rr,zz)
    bknum = ddot(n,z,1_i4,rr,1_i4)
    if (iter.eq.1) then
      do j = 1,n
        p(j) = z(j)
        pp(j) = zz(j)
      enddo
    else
      bk = bknum/bkden
      do j = 1,n
        p(j) = bk*p(j) + z(j)
        pp(j) = bk*pp(j) + zz(j)
      enddo
    endif
    bkden = bknum
    call sparseAxV(n,nloc,nlocptr,A,maxnumA,numA,listA,p,z)
    akden = ddot(n,z,1_i4,pp,1_i4)
    ak = bknum/akden
    call sparseAxV(n,nloc,nlocptr,A,maxnumA,numA,listA,pp,zz)
!
    call daxpy(n,ak,p,1_i4,x,1_i4)
    call daxpy(n,-ak,z,1_i4,r,1_i4)
    call daxpy(n,-ak,zz,1_i4,rr,1_i4)
!
    call sparseAdiagprecon(n,nloc,nlocptr,A,maxnumA,numA,nAdiag,listA,r,z)
!
    err = ddot(n,r,1_i4,r,1_i4)/bnrm
    err = sqrt(err)
    if (ioproc.and.index(keyword,'verb').ne.0) then
      write(ioout,'('' BCG iteration = '',i4,'' Error = '',f16.12)') iter,err
    endif
    converged = (err.lt.tol)
  enddo
!
! Free workspace
!
  deallocate(zz)
  deallocate(z)
  deallocate(rr)
  deallocate(r)
  deallocate(pp)
  deallocate(p)

  end subroutine bcgsolve

  subroutine sparseAdiagprecon(n,nloc,nlocptr,A,maxnumA,numA,nAdiag,listA,Vin,Vout)
!
! Perform the precondition of a vector by the diagonal of a sparse matrix
!
  use datatypes
  use parallel,  only : nprocs
  use times,     only : tsum
  implicit none
  integer(i4),  intent(in)    :: n
  integer(i4),  intent(in)    :: nloc
  integer(i4),  intent(in)    :: nlocptr(nloc)
  integer(i4),  intent(in)    :: numA(n)
  integer(i4),  intent(in)    :: nAdiag(n)
  integer(i4),  intent(in)    :: maxnumA
  integer(i4),  intent(in)    :: listA(maxnumA,n)
  real(dp),     intent(in)    :: A(maxnumA,n)
  real(dp),     intent(in)    :: Vin(n)
  real(dp),     intent(out)   :: Vout(n)
!
  integer(i4)                 :: i
  integer(i4)                 :: io
  integer(i4)                 :: j
  real(dp),     allocatable   :: Vloc(:)
  real(dp)                    :: cputime
  real(dp)                    :: tsuml
!
! This code is specific to the form of the matrix being used here!!
!
  Vout(1:n-1) = 0.0_dp
  do i = 1,nloc
    io = nlocptr(i)
    j = nAdiag(i)
    Vout(io) = Vin(io)/A(j,i)
  enddo
  if (nprocs.gt.1) then
!
!  Globalization of Vout
!
    tsuml = cputime()
    allocate(Vloc(n))
    call sumall(Vout,Vloc,n-1,"sparseAdiagprecon","Vout") 
    Vout(1:n-1) = Vloc(1:n-1)
    deallocate(Vloc)
    tsum = tsum + cputime() - tsuml
  endif
  Vout(n) = Vin(n)

  end subroutine sparseAdiagprecon

  subroutine sparseAxV(n,nloc,nlocptr,A,maxnumA,numA,listA,Vin,Vout)
!
! Perform the multiplication of a sparse matrix by a vector
!
! NB This version is specific to the purpose here in that 
!    the last row and column are assumed to be 1, except the
!    diagonal element which is zero. This avoids having to 
!    explicitly store the constraint terms.
! 
  use datatypes
  use parallel,  only : nprocs
  use times,     only : tsum
  implicit none
  integer(i4),  intent(in)    :: n
  integer(i4),  intent(in)    :: nloc
  integer(i4),  intent(in)    :: nlocptr(nloc)
  integer(i4),  intent(in)    :: numA(nloc)
  integer(i4),  intent(in)    :: maxnumA
  integer(i4),  intent(in)    :: listA(maxnumA,nloc)
  real(dp),     intent(in)    :: A(maxnumA,nloc)
  real(dp),     intent(in)    :: Vin(n)
  real(dp),     intent(out)   :: Vout(n)

  integer(i4)                 :: i
  integer(i4)                 :: io
  integer(i4)                 :: jj
  integer(i4)                 :: jo
  real(dp),     allocatable   :: Vloc(:)
!
  real(dp)                    :: cputime
  real(dp)                    :: ddot
  real(dp)                    :: tsuml
!
  allocate(Vloc(n))
  Vout(1:n) = 0.0_dp
  do i = 1,nloc
    io = nlocptr(i)
    do jj = 1,numA(i)
      jo = listA(jj,i)
      Vloc(jj) = Vin(jo)
    enddo
    Vout(io) = ddot(numA(i),A(1,i),1_i4,Vloc,1_i4)
    Vout(io) = Vout(io) + Vin(n)
  enddo
  if (nprocs.gt.1) then
!
!  Globalization of Vout
!
    tsuml = cputime()
    call sumall(Vout,Vloc,n-1,"sparseAxV","Vout") 
    Vout(1:n-1) = Vloc(1:n-1)
    tsum = tsum + cputime() - tsuml
  endif
  Vout(n) = 0.0_dp
  do jo = 1,n-1
    Vout(n) = Vout(n) + Vin(jo)
  enddo
  deallocate(Vloc)

  end subroutine sparseAxV
