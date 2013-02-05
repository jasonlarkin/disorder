  subroutine compressd1(d1,ncfoc,nsfoc,numat,iocptr)
!
!  Compress a vector that spans the full set of partially
!  occupied atoms to one that spans the fully occupied sites.
!
!   3/03 Created from code in phonon
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: iocptr(*)
  integer(i4)                                  :: ncfoc
  integer(i4)                                  :: nsfoc
  integer(i4)                                  :: numat
  real(dp)                                     :: d1(*)
!
!  Local variables
!
  integer(i4)                                  :: j
  integer(i4)                                  :: l
  integer(i4)                                  :: ncsfoc
  logical                                      :: lfirst1
!
  ncsfoc = ncfoc + nsfoc
!
!  Reduce partially occupied sites down to full occupancy ones
!
  do j = 1,ncsfoc
    lfirst1 = .true.
    do l = 1,numat
      if (iocptr(l).eq.j) then
        if (lfirst1) then
!
!  If the first occurance overwrite block
!
          d1(j) = d1(l)
          lfirst1 = .false.
        else
!
!  Subsequent add to block
!
          d1(j) = d1(j) + d1(l)
        endif
      endif
    enddo
  enddo
!
  return
  end
