  subroutine phoncopy2e(derv2,dervi,cmat,maxd2,mci,mcj,maxd2c,msv)
!
!  Eispack version / triangular version
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)  :: maxd2
  integer(i4)  :: maxd2c
  integer(i4)  :: mci
  integer(i4)  :: mcj
  integer(i4)  :: msv
  real(dp)     :: derv2(maxd2,*)
  real(dp)     :: dervi(maxd2,*)
  complex(dpc) :: cmat(maxd2c,*)
!
!  Local variables
!
  integer(i4)  :: i
  integer(i4)  :: j
  complex(dpc) :: cmp
!
!  Return inverse complex matrix to separate real and imaginary matrices
!  and resymmetrise
!
  do i = 1,msv
    do j = 1,i
      cmp = cmat(j,i)
      derv2(mci+j,mcj+i) = cmp
      dervi(mci+j,mcj+i) = aimag(cmp)
      derv2(mci+i,mcj+j) = cmp
      dervi(mci+i,mcj+j) = - aimag(cmp)
    enddo
  enddo
  return
  end
!
  subroutine phoncopy2l(derv2,dervi,cmat,maxd2,mci,mcj,maxd2c,msv)
!
!  Lapack version
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)  :: maxd2
  integer(i4)  :: maxd2c
  integer(i4)  :: mci
  integer(i4)  :: mcj
  integer(i4)  :: msv
  real(dp)     :: derv2(maxd2,*)
  real(dp)     :: dervi(maxd2,*)
  complex(dpc) :: cmat(maxd2c,*)
!
!  Local variables
!
  integer(i4)  :: i
  integer(i4)  :: j
  complex(dpc) :: cmp
!
!  Return inverse complex matrix to separate real and imaginary matrices
!  and resymmetrise
!
  do i = 1,msv
    do j = 1,msv
      cmp = cmat(j,i)
      derv2(mci+j,mcj+i) = cmp
      dervi(mci+j,mcj+i) = aimag(cmp)
    enddo
  enddo
  return
  end
!
  subroutine phoncopy2r(derv2,dmat,maxd2,mcv,msv)
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4) :: maxd2
  integer(i4) :: mcv
  integer(i4) :: msv
  real(dp)    :: derv2(maxd2,*),dmat(msv,*)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: j
  real(dp)    :: dmp
!
!  Return inverse matrix to main matrix and resymmetrise
!
  do i = 1,msv
    do j = 1,i
      dmp = dmat(j,i)
      derv2(mcv+j,mcv+i) = dmp
      derv2(mcv+i,mcv+j) = dmp
    enddo
  enddo
  return
  end
