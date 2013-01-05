  subroutine GULP_mxmb(a,mcola,mrowa,b,mcolb,mrowb,r,mcolr,mrowr,ncol,nlink,nrow)
!
!  Routine for A x B = R : 
!
!  R(ncol,nrow) = R(ncol,nrow) + A(ncol,nlink)*B(nlink,nrow) matrix multiply
!
!  Sparsity of B used
!
!  R must be pre-initialized
!
!   3/07 Name changed from mxmb to GULP_mxmb
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)   :: mcola
  integer(i4)   :: mcolb
  integer(i4)   :: mcolr
  integer(i4)   :: mrowa
  integer(i4)   :: mrowb
  integer(i4)   :: mrowr
  integer(i4)   :: ncol
  integer(i4)   :: nlink
  integer(i4)   :: nrow
  real(dp)      :: a(*)
  real(dp)      :: b(*)
  real(dp)      :: r(*)
!
!  Local variables
!
  integer(i4)   :: i
  integer(i4)   :: ia
  integer(i4)   :: iaa
  integer(i4)   :: ib
  integer(i4)   :: ibb
  integer(i4)   :: ir
  integer(i4)   :: irr
  integer(i4)   :: j
  integer(i4)   :: k
  real(dp)      :: fac
!
  ir = 1
  ib = 1
  do j = 1,nrow
    ibb = ib
    ia = 1
    do k = 1,nlink
      fac = b(ibb)
      if (fac.ne.0.0_dp) then
        irr = ir
        iaa = ia
        do i = 1,ncol
          r(irr) = fac*a(iaa) + r(irr)
          irr = irr + mcolr
          iaa = iaa + mcola
        enddo
      endif
      ibb = ibb + mcolb
      ia = ia + mrowa
    enddo
    ir = ir + mrowr
    ib = ib + mrowb
  enddo
!
  return
  end
