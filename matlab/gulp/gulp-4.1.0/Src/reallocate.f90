  module reallocate
!
!  Set of routines and interface for handling memory
!  allocation and reallocation while preserving the
!  contents.
!
!  12/01 Modified so that characters are handled with individual
!        interfaces according to the string length since some
!        compilers cannot handle the passing of the string length.
!   5/07 Bug in realloc_r8_4 fixed
!   6/09 Support for character arrays of length six added for ERS
!        code.
!   6/09 Misordering of k and l in arguments to _4 routines fixed.
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
!  Julian Gale, NRI, Curtin University, June 2009
!
    use datatypes
    use iochannels
!
!  Local variables for storing memory usage information
!
    integer(i4), save :: losize  = 4
    integer(i4), save :: i2size  = 4
    integer(i4), save :: i4size  = 4
    integer(i4), save :: r4size  = 4
    integer(i4), save :: r8size  = 8
    integer(i4), save :: chsize  = 4
    integer(i4), save :: c8size  = 8
    integer(i4), save :: c16size = 16
    integer(i4), save :: Wordslo  = 0
    integer(i4), save :: Wordsi2  = 0
    integer(i4), save :: Wordsi4  = 0
    integer(i4), save :: Wordsr4  = 0
    integer(i4), save :: Wordsr8  = 0
    integer(i4), save :: Wordsch  = 0
    integer(i4), save :: Wordsc8  = 0
    integer(i4), save :: Wordsc16 = 0

    integer(i4), save :: PeakMemory = 0

    interface realloc

      module procedure &
        realloc_r4_1, realloc_r8_1, &
        realloc_i2_1, realloc_i4_1, &
        realloc_c8_1, realloc_c16_1, &
        realloc_l_1, &
        realloc_r4_2, realloc_r8_2, &
        realloc_i2_2, realloc_i4_2, &
        realloc_c8_2, realloc_c16_2, &
        realloc_l_2, &
        realloc_r4_3, realloc_r8_3, &
        realloc_i2_3, realloc_i4_3, &
        realloc_c8_3, realloc_c16_3, &
        realloc_l_3, &
        realloc_r4_4, realloc_r8_4, &
        realloc_i2_4, realloc_i4_4, &
        realloc_c8_4, realloc_c16_4, &
        realloc_l_4

    end interface 

    interface realloc_ch1
      module procedure &
        realloc_ch1_1, realloc_ch1_2, &
        realloc_ch1_3, realloc_ch1_4
    end interface 

    interface realloc_ch5
      module procedure &
        realloc_ch5_1, realloc_ch5_2, &
        realloc_ch5_3, realloc_ch5_4
    end interface 

    interface realloc_ch6 
       module procedure &
            realloc_ch6_1, realloc_ch6_2, &
            realloc_ch6_3, realloc_ch6_4
    end interface

    interface realloc_ch16
      module procedure &
        realloc_ch16_1, realloc_ch16_2, &
        realloc_ch16_3, realloc_ch16_4
    end interface 

    interface realloc_ch80
      module procedure &
        realloc_ch80_1, realloc_ch80_2, &
        realloc_ch80_3, realloc_ch80_4
    end interface 

  contains

    subroutine realloc_r4_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(sp), dimension(:), pointer :: p
      integer(i4), intent(in)         :: n
      integer(i4), intent(out)        :: ierror
!
!  Local arguments
!
      real(sp), dimension(:), pointer :: plocal
      integer(i4)                     :: nold
      integer(i4)                     :: i
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsr4 = Wordsr4 + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_sp
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
        Wordsr4 = Wordsr4 - nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          plocal(i) = p(i)
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r4_1

    subroutine realloc_r8_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(dp), dimension(:), pointer :: p
      integer(i4), intent(in)         :: n
      integer(i4), intent(out)        :: ierror
!
!  Local arguments
!
      real(dp), dimension(:), pointer :: plocal
      integer(i4)                     :: nold
      integer(i4)                     :: i
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsr8 = Wordsr8 + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_dp
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
        Wordsr8 = Wordsr8 - nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          plocal(i) = p(i)
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r8_1

    subroutine realloc_i2_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i2), dimension(:), pointer :: p
      integer(i4), intent(in)            :: n
      integer(i4), intent(out)           :: ierror
!
!  Local arguments
!
      integer(i2), dimension(:), pointer :: plocal
      integer(i4)                        :: nold
      integer(i4)                        :: i
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsi2 = Wordsi2 + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i2
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
        Wordsi2 = Wordsi2 - nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          plocal(i) = p(i)
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_i2_1

    subroutine realloc_i4_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), dimension(:), pointer :: p
      integer(i4), intent(in)            :: n
      integer(i4), intent(out)           :: ierror
!
!  Local arguments
!
      integer(i4), dimension(:), pointer :: plocal
      integer(i4)                        :: nold
      integer(i4)                        :: i
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsi4 = Wordsi4 + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i4
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
        Wordsi4 = Wordsi4 - nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          plocal(i) = p(i)
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_i4_1

    subroutine realloc_c8_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(spc), dimension(:), pointer :: p
      integer(i4), intent(in)             :: n
      integer(i4), intent(out)            :: ierror
!
!  Local arguments
!
      complex(spc), dimension(:), pointer :: plocal
      integer(i4)                         :: nold
      integer(i4)                         :: i
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsc8 = Wordsc8 + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_sp,0.0_sp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
        Wordsc8 = Wordsc8 - nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          plocal(i) = p(i)
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c8_1

    subroutine realloc_c16_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(dpc), dimension(:), pointer :: p
      integer(i4), intent(in)             :: n
      integer(i4), intent(out)            :: ierror
!
!  Local arguments
!
      complex(dpc), dimension(:), pointer :: plocal
      integer(i4)                         :: nold
      integer(i4)                         :: i
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsc16 = Wordsc16 + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_dp,0.0_dp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
        Wordsc16 = Wordsc16 - nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          plocal(i) = p(i)
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c16_1

    subroutine realloc_l_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      logical, dimension(:), pointer :: p
      integer(i4), intent(in)        :: n
      integer(i4), intent(out)       :: ierror
!
!  Local arguments
!
      logical, dimension(:), pointer :: plocal
      integer(i4)                    :: nold
      integer(i4)                    :: i
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordslo = Wordslo + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = .false.
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
        Wordslo = Wordslo - nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          plocal(i) = p(i)
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_l_1

    subroutine realloc_r4_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(sp), dimension(:,:), pointer :: p
      integer(i4), intent(in)           :: m,n
      integer(i4), intent(out)          :: ierror
!
!  Local arguments
!
      real(sp), dimension(:,:), pointer :: plocal
      integer(i4)                       :: mold,nold
      integer(i4)                       :: i, j
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsr4 = Wordsr4 + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_sp
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsr4 = Wordsr4 - mold*nold
!
!  Preserve data
!
        plocal(1:min(mold,m),1:min(nold,n)) = &
          p(1:min(mold,m),1:min(nold,n))
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            plocal(j,i) = p(j,i)
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r4_2

    subroutine realloc_r8_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(dp), dimension(:,:), pointer :: p
      integer(i4), intent(in)           :: m,n
      integer(i4), intent(out)          :: ierror
!
!  Local arguments
!
      real(dp), dimension(:,:), pointer :: plocal
      integer(i4)                       :: mold,nold
      integer(i4)                       :: i, j
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsr8 = Wordsr8 + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_dp
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsr8 = Wordsr8 - mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            plocal(j,i) = p(j,i)
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r8_2

    subroutine realloc_i2_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i2), dimension(:,:), pointer :: p
      integer(i4), intent(in)              :: m,n
      integer(i4), intent(out)             :: ierror
!
!  Local arguments
!
      integer(i2), dimension(:,:), pointer :: plocal
      integer(i4)                          :: mold,nold
      integer(i4)                          :: i, j
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsi2 = Wordsi2 + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i2
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsi2 = Wordsi2 - mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            plocal(j,i) = p(j,i)
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_i2_2

    subroutine realloc_i4_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), dimension(:,:), pointer :: p
      integer(i4), intent(in)              :: m,n
      integer(i4), intent(out)             :: ierror
!
!  Local arguments
!
      integer(i4), dimension(:,:), pointer :: plocal
      integer(i4)                          :: mold,nold
      integer(i4)                          :: i, j
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsi4 = Wordsi4 + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i4
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsi4 = Wordsi4 - mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            plocal(j,i) = p(j,i)
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_i4_2

    subroutine realloc_c8_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(spc), dimension(:,:), pointer :: p
      integer(i4), intent(in)               :: m,n
      integer(i4), intent(out)              :: ierror
!
!  Local arguments
!
      complex(spc), dimension(:,:), pointer :: plocal
      integer(i4)                           :: mold,nold
      integer(i4)                           :: i, j
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsc8 = Wordsc8 + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_sp,0.0_sp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsc8 = Wordsc8 - mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            plocal(j,i) = p(j,i)
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c8_2

    subroutine realloc_c16_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(dpc), dimension(:,:), pointer :: p
      integer(i4), intent(in)               :: m,n
      integer(i4), intent(out)              :: ierror
!
!  Local arguments
!
      complex(dpc), dimension(:,:), pointer :: plocal
      integer(i4)                           :: mold,nold
      integer(i4)                           :: i, j
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsc16 = Wordsc16 + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_dp,0.0_dp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsc16 = Wordsc16 - mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            plocal(j,i) = p(j,i)
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c16_2

    subroutine realloc_l_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      logical, dimension(:,:), pointer :: p
      integer(i4), intent(in)          :: m,n
      integer(i4), intent(out)         :: ierror
!
!  Local arguments
!
      logical, dimension(:,:), pointer :: plocal
      integer(i4)                      :: mold,nold
      integer(i4)                      :: i, j
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordslo = Wordslo + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = .false.
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordslo = Wordslo - mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            plocal(j,i) = p(j,i)
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_l_2

    subroutine realloc_r4_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(sp), dimension(:,:,:), pointer :: p
      integer(i4), intent(in)             :: k,m,n
      integer(i4), intent(out)            :: ierror
!
!  Local arguments
!
      real(sp), dimension(:,:,:), pointer :: plocal
      integer(i4)                         :: kold,mold,nold
      integer(i4)                         :: i, j, l
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsr4 = Wordsr4 + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_sp
!

      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsr4 = Wordsr4 - kold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(mold,m),1:min(nold,n)) = &
          p(1:min(kold,k),1:min(mold,m),1:min(nold,n))
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            do l = 1,min(kold,k)
              plocal(l,j,i) = p(l,j,i)
            enddo
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r4_3

    subroutine realloc_r8_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(dp), dimension(:,:,:), pointer :: p
      integer(i4), intent(in)             :: k,m,n
      integer(i4), intent(out)            :: ierror
!
!  Local arguments
!
      real(dp), dimension(:,:,:), pointer :: plocal
      integer(i4)                         :: kold,mold,nold
      integer(i4)                         :: i, j, l
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsr8 = Wordsr8 + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_dp
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsr8 = Wordsr8 - kold*mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            do l = 1,min(kold,k)
              plocal(l,j,i) = p(l,j,i)
            enddo
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r8_3

    subroutine realloc_i2_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i2), dimension(:,:,:), pointer :: p
      integer(i4), intent(in)                :: k,m,n
      integer(i4), intent(out)               :: ierror
!
!  Local arguments
!
      integer(i2), dimension(:,:,:), pointer :: plocal
      integer(i4)                            :: kold,mold,nold
      integer(i4)                            :: i, j, l
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsi2 = Wordsi2 + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i2
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsi2 = Wordsi2 - kold*mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            do l = 1,min(kold,k)
              plocal(l,j,i) = p(l,j,i)
            enddo
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_i2_3

    subroutine realloc_i4_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), dimension(:,:,:), pointer :: p
      integer(i4), intent(in)                :: k,m,n
      integer(i4), intent(out)               :: ierror
!
!  Local arguments
!
      integer(i4), dimension(:,:,:), pointer :: plocal
      integer(i4)                            :: kold,mold,nold
      integer(i4)                            :: i, j, l
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsi4 = Wordsi4 + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i4
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsi4 = Wordsi4 - kold*mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            do l = 1,min(kold,k)
              plocal(l,j,i) = p(l,j,i)
            enddo
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal

    end subroutine realloc_i4_3

    subroutine realloc_c8_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(spc), dimension(:,:,:), pointer :: p
      integer(i4), intent(in)                 :: k,m,n
      integer(i4), intent(out)                :: ierror
!
!  Local arguments
!
      complex(spc), dimension(:,:,:), pointer :: plocal
      integer(i4)                             :: kold,mold,nold
      integer(i4)                             :: i, j, l
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsc8 = Wordsc8 + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_sp,0.0_sp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsc8 = Wordsc8 - kold*mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            do l = 1,min(kold,k)
              plocal(l,j,i) = p(l,j,i)
            enddo
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c8_3

    subroutine realloc_c16_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(dpc), dimension(:,:,:), pointer :: p
      integer(i4), intent(in)                 :: k,m,n
      integer(i4), intent(out)                :: ierror
!
!  Local arguments
!
      complex(dpc), dimension(:,:,:), pointer :: plocal
      integer(i4)                             :: kold,mold,nold
      integer(i4)                             :: i, j, l
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsc16 = Wordsc16 + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_dp,0.0_dp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsc16 = Wordsc16 - kold*mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            do l = 1,min(kold,k)
              plocal(l,j,i) = p(l,j,i)
            enddo
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c16_3

    subroutine realloc_l_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      logical, dimension(:,:,:), pointer :: p
      integer(i4), intent(in)            :: k,m,n
      integer(i4), intent(out)           :: ierror
!
!  Local arguments
!
      logical, dimension(:,:,:), pointer :: plocal
      integer(i4)                        :: kold,mold,nold
      integer(i4)                        :: i, j, l
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordslo = Wordslo + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = .false.
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordslo = Wordslo - kold*mold*nold
!
!  Preserve data
!
        do i = 1,min(nold,n)
          do j = 1,min(mold,m)
            do l = 1,min(kold,k)
              plocal(l,j,i) = p(l,j,i)
            enddo
          enddo
        enddo
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_l_3

    subroutine realloc_r4_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(sp), dimension(:,:,:,:), pointer :: p
      integer(i4), intent(in)               :: k,l,m,n
      integer(i4), intent(out)              :: ierror
!
!  Local arguments
!
      real(sp), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                           :: kold,lold
      integer(i4)                           :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsr4 = Wordsr4 + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_sp
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsr4 = Wordsr4 - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r4_4

    subroutine realloc_r8_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      real(dp), dimension(:,:,:,:), pointer :: p
      integer(i4), intent(in)               :: k,l,m,n
      integer(i4), intent(out)              :: ierror
!
!  Local arguments
!
      real(dp), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                           :: kold,lold
      integer(i4)                           :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsr8 = Wordsr8 + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0.0_dp
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsr8 = Wordsr8 - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_r8_4

    subroutine realloc_i2_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i2), dimension(:,:,:,:), pointer :: p
      integer(i4), intent(in)                  :: k,l,m,n
      integer(i4), intent(out)                 :: ierror
!
!  Local arguments
!
      integer(i2), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                              :: kold,lold
      integer(i4)                              :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsi2 = Wordsi2 + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i2
!

      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsi2 = Wordsi2 - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_i2_4

    subroutine realloc_i4_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), dimension(:,:,:,:), pointer :: p
      integer(i4), intent(in)                  :: k,l,m,n
      integer(i4), intent(out)                 :: ierror
!
!  Local arguments
!
      integer(i4), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                              :: kold,lold
      integer(i4)                              :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsi4 = Wordsi4 + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = 0_i4
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsi4 = Wordsi4 - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_i4_4

    subroutine realloc_c8_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(spc), dimension(:,:,:,:), pointer :: p
      integer(i4), intent(in)                   :: k,l,m,n
      integer(i4), intent(out)                  :: ierror
!
!  Local arguments
!
      complex(spc), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                               :: kold,lold
      integer(i4)                               :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsc8 = Wordsc8 + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_sp,0.0_sp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsc8 = Wordsc8 - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c8_4

    subroutine realloc_c16_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      complex(dpc), dimension(:,:,:,:), pointer :: p
      integer(i4), intent(in)                   :: k,l,m,n
      integer(i4), intent(out)                  :: ierror
!
!  Local arguments
!
      complex(dpc), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                               :: kold,lold
      integer(i4)                               :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsc16 = Wordsc16 + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = (0.0_dp,0.0_dp)
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsc16 = Wordsc16 - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_c16_4

    subroutine realloc_l_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      logical, dimension(:,:,:,:), pointer :: p
      integer(i4), intent(in)              :: k,l,m,n
      integer(i4), intent(out)             :: ierror
!
!  Local arguments
!
      logical, dimension(:,:,:,:), pointer :: plocal
      integer(i4)                          :: kold,lold
      integer(i4)                          :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordslo = Wordslo + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = .false.
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordslo = Wordslo - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_l_4

    subroutine realloc_ch80_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                     :: n
      integer(i4), intent(out)                    :: ierror
      character(len=80), dimension(:), pointer :: p
!
!  Local arguments
!
      character(len=80), dimension(:), pointer :: plocal
      integer(i4)                                 :: nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsch = Wordsch + 80*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
!
!  Preserve data
!
        plocal(1:min(nold,n)) = p(1:min(nold,n))
        Wordsch = Wordsch - 80*nold
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch80_1

    subroutine realloc_ch80_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                       :: m,n
      integer(i4), intent(out)                      :: ierror
      character(len=80), dimension(:,:), pointer :: p
!
!  Local arguments
!
      character(len=80), dimension(:,:), pointer :: plocal
      integer(i4)                                   :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsch = Wordsch + 80*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsch = Wordsch - 80*mold*nold
!
!  Preserve data
!
        plocal(1:min(mold,m),1:min(nold,n)) = &
          p(1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch80_2

    subroutine realloc_ch80_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                      :: k,m,n
      integer(i4), intent(out)                     :: ierror
      character(len=80), dimension(:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=80), dimension(:,:,:), pointer :: plocal
      integer(i4)                                  :: mold,nold
      integer(i4)                                  :: kold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsch = Wordsch + 80*k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsch = Wordsch - 80*kold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(mold,m),1:min(nold,n)) = &
          p(1:min(kold,k),1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch80_3

    subroutine realloc_ch80_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                     :: k,l,m,n
      integer(i4), intent(out)                    :: ierror
      character(len=80), dimension(:,:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=80), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                                 :: mold,nold
      integer(i4)                                 :: kold,lold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsch = Wordsch + 80*k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsch = Wordsch - 80*kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch80_4

    subroutine realloc_ch16_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                     :: n
      integer(i4), intent(out)                    :: ierror
      character(len=16), dimension(:), pointer :: p
!
!  Local arguments
!
      character(len=16), dimension(:), pointer :: plocal
      integer(i4)                                 :: nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsch = Wordsch + 16*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
!
!  Preserve data
!
        plocal(1:min(nold,n)) = p(1:min(nold,n))
        Wordsch = Wordsch - 16*nold
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch16_1

    subroutine realloc_ch16_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                       :: m,n
      integer(i4), intent(out)                      :: ierror
      character(len=16), dimension(:,:), pointer :: p
!
!  Local arguments
!
      character(len=16), dimension(:,:), pointer :: plocal
      integer(i4)                                   :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsch = Wordsch + 16*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsch = Wordsch - 16*mold*nold
!
!  Preserve data
!
        plocal(1:min(mold,m),1:min(nold,n)) = &
          p(1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch16_2

    subroutine realloc_ch16_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                      :: k,m,n
      integer(i4), intent(out)                     :: ierror
      character(len=16), dimension(:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=16), dimension(:,:,:), pointer :: plocal
      integer(i4)                                  :: mold,nold
      integer(i4)                                  :: kold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsch = Wordsch + 16*k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsch = Wordsch - 16*kold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(mold,m),1:min(nold,n)) = &
          p(1:min(kold,k),1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch16_3

    subroutine realloc_ch16_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                     :: k,l,m,n
      integer(i4), intent(out)                    :: ierror
      character(len=16), dimension(:,:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=16), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                                 :: mold,nold
      integer(i4)                                 :: kold,lold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsch = Wordsch + 16*k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsch = Wordsch - 16*kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch16_4

    subroutine realloc_ch1_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                 :: n
      integer(i4), intent(out)                :: ierror
      character(len=1), dimension(:), pointer :: p
!
!  Local arguments
!
      character(len=1), dimension(:), pointer :: plocal
      integer(i4)                             :: nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsch = Wordsch + n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
!
!  Preserve data
!
        plocal(1:min(nold,n)) = p(1:min(nold,n))
        Wordsch = Wordsch - nold
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch1_1

    subroutine realloc_ch1_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                       :: m,n
      integer(i4), intent(out)                      :: ierror
      character(len=1), dimension(:,:), pointer :: p
!
!  Local arguments
!
      character(len=1), dimension(:,:), pointer :: plocal
      integer(i4)                                   :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsch = Wordsch + m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsch = Wordsch - mold*nold
!
!  Preserve data
!
        plocal(1:min(mold,m),1:min(nold,n)) = &
          p(1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch1_2

    subroutine realloc_ch1_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                      :: k,m,n
      integer(i4), intent(out)                     :: ierror
      character(len=1), dimension(:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=1), dimension(:,:,:), pointer :: plocal
      integer(i4)                                  :: mold,nold
      integer(i4)                                  :: kold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsch = Wordsch + k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsch = Wordsch - kold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(mold,m),1:min(nold,n)) = &
          p(1:min(kold,k),1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch1_3

    subroutine realloc_ch1_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                     :: k,l,m,n
      integer(i4), intent(out)                    :: ierror
      character(len=1), dimension(:,:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=1), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                                 :: mold,nold
      integer(i4)                                 :: kold,lold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsch = Wordsch + k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsch = Wordsch - kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch1_4

    subroutine realloc_ch5_1(p,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                 :: n
      integer(i4), intent(out)                :: ierror
      character(len=5), dimension(:), pointer :: p
!
!  Local arguments
!
      character(len=5), dimension(:), pointer :: plocal
      integer(i4)                             :: nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsch = Wordsch + 5*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        nold = size(p)
!
!  Preserve data
!
        plocal(1:min(nold,n)) = p(1:min(nold,n))
        Wordsch = Wordsch - 5*nold
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch5_1

    subroutine realloc_ch5_2(p,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                       :: m,n
      integer(i4), intent(out)                      :: ierror
      character(len=5), dimension(:,:), pointer     :: p
!
!  Local arguments
!
      character(len=5), dimension(:,:), pointer     :: plocal
      integer(i4)                                   :: mold,nold
      integer(i4)                                   :: minm,minn
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsch = Wordsch + 5*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        mold = size(p,1)
        nold = size(p,2)
        Wordsch = Wordsch - 5*mold*nold
        minm = min(mold,m)
        minn = min(nold,n)
!
!  Preserve data
!
        plocal(1:minm,1:minn) = p(1:minm,1:minn)
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch5_2

    subroutine realloc_ch5_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                      :: k,m,n
      integer(i4), intent(out)                     :: ierror
      character(len=5), dimension(:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=5), dimension(:,:,:), pointer :: plocal
      integer(i4)                                  :: mold,nold
      integer(i4)                                  :: kold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsch = Wordsch + 5*k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        mold = size(p,2)
        nold = size(p,3)
        Wordsch = Wordsch - 5*kold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(mold,m),1:min(nold,n)) = &
          p(1:min(kold,k),1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch5_3

    subroutine realloc_ch5_4(p,k,l,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                     :: k,l,m,n
      integer(i4), intent(out)                    :: ierror
      character(len=5), dimension(:,:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=5), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                                 :: mold,nold
      integer(i4)                                 :: kold,lold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsch = Wordsch + 5*k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
        write(ioout,'(''!! Allocation error !!'')')
        return
      endif
      plocal = ' '
!
      if (associated(p)) then
!
!  Find size of old array
!
        kold = size(p,1)
        lold = size(p,2)
        mold = size(p,3)
        nold = size(p,4)
        Wordsch = Wordsch - 5*kold*lold*mold*nold
!
!  Preserve data
!
        plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m), &
          1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l), &
          1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
        deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch5_4

    subroutine realloc_ch6_1(p,n,ierror)
!
      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                 :: n
      integer(i4), intent(out)                :: ierror
      character(len=6), dimension(:), pointer :: p
!
!  Local arguments
!
      character(len=6), dimension(:), pointer :: plocal
      integer(i4)                             :: nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(n),stat=ierror)
      Wordsch = Wordsch + 6*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
         write(ioout,'(''!! Allocation error !!'')')
         return
      endif
      plocal = ''
!
      if (associated(p)) then
!
!  Find size of old array
!
         nold = size(p)
!
!  Preserve data
!
         plocal(1:min(nold,n))=p(1:min(nold,n))
         Wordsch = Wordsch - 6*nold
!
!  Free old memory
!
         deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory
      
    end subroutine realloc_ch6_1
    
    subroutine realloc_ch6_2(p,m,n,ierror)
      
      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                       :: m,n
      integer(i4), intent(out)                      :: ierror
      character(len=6), dimension(:,:), pointer :: p
!
!  Local arguments
!
      character(len=6), dimension(:,:), pointer :: plocal
      integer(i4)                                   :: mold,nold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(m,n),stat=ierror)
      Wordsch = Wordsch + 6*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
         write(ioout,'(''!! Allocation error !!'')')
         return
      endif
      plocal = ''
!
      if (associated(p)) then
!
!  Find size of old array
!
         mold = size(p,1)
         nold = size(p,2)
         Wordsch = Wordsch - 6*mold*nold
!
!  Preserve data
!
         plocal(1:min(mold,m),1:min(nold,n)) = &
              p(1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
         deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory
      
    end subroutine realloc_ch6_2

    subroutine realloc_ch6_3(p,k,m,n,ierror)

      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                      :: k,m,n
      integer(i4), intent(out)                     :: ierror
      character(len=6), dimension(:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=6), dimension(:,:,:), pointer :: plocal
      integer(i4)                                  :: mold,nold
      integer(i4)                                  :: kold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,m,n),stat=ierror)
      Wordsch = Wordsch + 6*k*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
         write(ioout,'(''!! Allocation error !!'')')
         return
      endif
      plocal = ''
!
      if (associated(p)) then
!
!  Find size of old array
!
         kold = size(p,1)
         mold = size(p,2)
         nold = size(p,3)
         Wordsch = Wordsch - 6*kold*mold*nold
!
!  Preserve data
!
         plocal(1:min(kold,k),1:min(mold,m),1:min(nold,n)) = &
              p(1:min(kold,k),1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
         deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory
      
    end subroutine realloc_ch6_3
    
    subroutine realloc_ch6_4(p,k,l,m,n,ierror)
      
      implicit none
!
!  Passed arguments
!
      integer(i4), intent(in)                     :: k,l,m,n
      integer(i4), intent(out)                    :: ierror
      character(len=6), dimension(:,:,:,:), pointer :: p
!
!  Local arguments
!
      character(len=6), dimension(:,:,:,:), pointer :: plocal
      integer(i4)                                 :: mold,nold
      integer(i4)                                 :: kold,lold
!
!  Allocate new size of memory
!
      nullify(plocal)
      allocate(plocal(k,l,m,n),stat=ierror)
      Wordsch = Wordsch + 6*k*l*m*n
!
!  If there is an error then return
!
      if (ierror.ne.0) then
         write(ioout,'(''!! Allocation error !!'')')
         return
      endif
      plocal = ''
!
      if (associated(p)) then
!
!  Find size of old array
!
         kold = size(p,1)
         lold = size(p,2)
         mold = size(p,3)
         nold = size(p,4)
         Wordsch = Wordsch - 6*kold*lold*mold*nold
!
!  Preserve data
!
         plocal(1:min(kold,k),1:min(lold,l),1:min(mold,m),&
              1:min(nold,n)) = p(1:min(kold,k),1:min(lold,l),&
              1:min(mold,m),1:min(nold,n))
!
!  Free old memory
!
         deallocate(p)
      endif
!
!  Re-assign pointers so that p is returned as the new array
!
      p=>plocal
!
!  Check total memory usage
!
      call totalmemory

    end subroutine realloc_ch6_4

    subroutine totalmemory

    implicit none
!
!  Calculate the total memory currently in use and check
!  against the peak memory.
!
    integer(i4) :: totmem

    totmem = losize*Wordslo + &
             i2size*Wordsi2 + &
             i4size*Wordsi4 + &
             r4size*Wordsr4 + &
             r8size*Wordsr8 + &
             chsize*Wordsch + &
             c8size*Wordsc8 + &
             c16size*Wordsc16

    if (totmem .gt. PeakMemory) PeakMemory = totmem

    end subroutine totalmemory

    subroutine printmemory

    implicit none

    real(dp) :: outmem
!
!  Print out the peak total memory usage for dynamic memory
!
    outmem = dble(PeakMemory) * 1.0d-6
    write(ioout,'(/,''  Peak dynamic memory used = '',f10.2,'' MB '',/)') outmem

    end subroutine printmemory

  end module reallocate
