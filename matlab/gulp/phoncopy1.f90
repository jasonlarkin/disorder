  subroutine phoncopy1(derv2,dervi,cmat,maxd2,mci,mcj,maxd2c,msv)
  use datatypes
  implicit none
!
  integer(i4)  :: maxd2
  integer(i4)  :: maxd2c
  integer(i4)  :: mci
  integer(i4)  :: mcj
  integer(i4)  :: msv
  real(dp)     :: derv2(maxd2,*),dervi(maxd2,*)
  complex(dpc) :: cmat(maxd2c,*)
!
  integer(i4)  :: i
  integer(i4)  :: j
!
  do i = 1,msv
    do j = 1,msv
      cmat(j,i) = cmplx(derv2(mci+j,mcj+i),dervi(mci+j,mcj+i))
    enddo
  enddo
  return
  end
!
  subroutine phoncopy1r(derv2,dmat,maxd2,mcv,msv)
  use datatypes
  implicit none
!
  integer(i4) :: maxd2
  integer(i4) :: mcv
  integer(i4) :: msv
  real(dp)    :: derv2(maxd2,*),dmat(msv,*)
!
  integer(i4) :: i
  integer(i4) :: j
!
  do i = 1,msv
    do j = 1,msv
      dmat(j,i) = derv2(mcv+j,mcv+i)
    enddo
  enddo
  return
  end
