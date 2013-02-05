  subroutine compressd3(d3,ldd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
!
!  Compress a second derivative matrix that spans the full set of partially
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
  integer(i4)                                  :: ibocptr(*)
  integer(i4)                                  :: iocptr(*)
  integer(i4)                                  :: ldd2
  integer(i4)                                  :: nbfoc
  integer(i4)                                  :: ncfoc
  integer(i4)                                  :: nsfoc
  integer(i4)                                  :: numat
  real(dp)                                     :: d3(ldd2,6)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: indj
  integer(i4)                                  :: indl
  integer(i4)                                  :: j
  integer(i4)                                  :: l
  integer(i4)                                  :: mint
  integer(i4)                                  :: mjr
  integer(i4)                                  :: mjs
  integer(i4)                                  :: n3f
  integer(i4)                                  :: ncsfoc
  logical                                      :: lfirst1
!
  ncsfoc = ncfoc + nsfoc
  n3f = 3*numat
  mint = n3f - 3
!
!  Reduce partially occupied sites down to full occupancy ones
!
  do i = 1,6
    do j = 1,ncsfoc
      indj = 3*(j - 1)
      lfirst1 = .true.
      do l = 1,numat
        if (iocptr(l).eq.j) then
          indl = 3*(l - 1)
          if (lfirst1) then
!
!  If the first occurance overwrite block
!
            d3(indj+1,i) = d3(indl+1,i)
            d3(indj+2,i) = d3(indl+2,i)
            d3(indj+3,i) = d3(indl+3,i)
            lfirst1 = .false.
          else
!
!  Subsequent add to block
!
            d3(indj+1,i) = d3(indj+1,i) + d3(indl+1,i)
            d3(indj+2,i) = d3(indj+2,i) + d3(indl+2,i)
            d3(indj+3,i) = d3(indj+3,i) + d3(indl+3,i)
          endif
        endif
      enddo
    enddo
  enddo
!
!  Compress second derivative matrix w.r.t. breathing shells
!
  if (nbfoc.gt.0) then
!
!  Partial occupancy
!
!  Pass 1 : reduce numat to ncfoc+nsfoc & numat to nbfoc in 1D
!
    mjs = mint + 3
    mjr = 3*ncsfoc
    do i = 1,6
      do j = 1,ncsfoc
        indj = 3*(j-1)
        lfirst1 = .true.
        do l = 1,numat
          if (iocptr(l).eq.j) then
            indl = 3*(l-1)
            if (lfirst1) then
!
!  First occurance -> overwrite block
!
              d3(indj+1,i) = d3(indl+1,i)
              d3(indj+2,i) = d3(indl+2,i)
              d3(indj+3,i) = d3(indl+3,i)
              lfirst1 = .false.
            else
!
!  Second occurance -> add terms on
!
              d3(indj+1,i) = d3(indj+1,i) + d3(indl+1,i)
              d3(indj+2,i) = d3(indj+2,i) + d3(indl+2,i)
              d3(indj+3,i) = d3(indj+3,i) + d3(indl+3,i)
            endif
          endif
        enddo
      enddo
      do j = 1,nbfoc
        lfirst1 = .true.
        do l = 1,numat
          if (ibocptr(l).eq.j) then
            if (lfirst1) then
              d3(mjr+j,i) = d3(mjs+l,i)
              lfirst1 = .false.
            else
              d3(mjr+j,i) = d3(mjr+j,i) + d3(mjs+l,i)
            endif
          endif
        enddo
      enddo
    enddo
  endif
!
  return
  end
