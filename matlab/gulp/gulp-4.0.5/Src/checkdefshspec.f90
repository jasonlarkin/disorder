  subroutine checkdefshspec
!
!  Check that defect shell species that were automatically added should be
!  present by looking through potential list for spring constant.
!
!  11/06 Created
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006
!
  use current
  use element,    only : maxele
  use species
  use two,        only : npote, nspec1, nspec2, nptyp1, nptyp2, nptype
  implicit none
!
!  Local variables
!
  integer(i4)                            :: i
  integer(i4)                            :: j
  integer(i4)                            :: n
  integer(i4), dimension(:), allocatable :: nptr
  integer(i4)                            :: status
  logical                                :: lfound
  logical,     dimension(:), allocatable :: ldelete
!
!  Allocate scratch arrays
!
  allocate(ldelete(nspec),stat=status)
  if (status/=0) call outofmemory('checkdefshspec','ldelete')
  allocate(nptr(nspec),stat=status)
  if (status/=0) call outofmemory('checkdefshspec','nptr')
  ldelete(1:nspec) = .false.
!
!  Loop over species looking for ones to test
!
  do n = 1,nspec
    if (ldefshspec(n)) then
!
!  Check whether species has a charge - if so then it has been assigned
!
      if (abs(qlspec(n)).gt.1.0d-8) then
        lfound = .true.
      else
!
!  Loop over potentials looking for spring constant involving this species
!
        lfound = .false.
        j = 0
        do while (.not.lfound.and.j.lt.npote)
          j = j + 1
          if (nptype(j).eq.5.or.nptype(j).eq.8.or.nptype(j).eq.33) then
            lfound = (nspec1(j).eq.natspec(n).and.(nptyp1(j).eq.ntypspec(n).or.nptyp1(j).eq.0))
            if (.not.lfound) lfound = (nspec2(j).eq.natspec(n).and.(nptyp2(j).eq.ntypspec(n).or.nptyp2(j).eq.0))
          endif
        enddo
      endif
      if (.not.lfound) ldelete(n) = .true.
    endif
  enddo
  j = 0
  do n = 1,nspec
    if (.not.ldelete(n)) then
      j = j + 1
      nptr(j) = n
    endif
  enddo
  if (j.ne.nspec) then
!
!  Shift species data to remove unwanted species
!
    do i = 1,j
      natspec(i) = natspec(nptr(i))
      ntypspec(i) = ntypspec(nptr(i))
      qlspec(i) = qlspec(nptr(i))
      radspec(i) = radspec(nptr(i))
      lbrspec(i) = lbrspec(nptr(i))
      lqinspec(i) = lqinspec(nptr(i))
      massspec(i) = massspec(nptr(i))
    enddo
    nspec = j
  endif
!
!  Deallocate scratch array
!
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('checkdefshspec','nptr')
  deallocate(ldelete,stat=status)
  if (status/=0) call deallocate_error('checkdefshspec','ldelete')
!
  return
  end
