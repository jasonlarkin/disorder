  subroutine compressd2(d2,ldd2,ncfoc,nsfoc,nbfoc,numat,iocptr,ibocptr)
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
  real(dp)                                     :: d2(ldd2,*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4)                                  :: indl
  integer(i4)                                  :: j
  integer(i4)                                  :: l
  integer(i4)                                  :: mint
  integer(i4)                                  :: mir
  integer(i4)                                  :: mis
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
!  Pass 1 : shift j direction
!
  do i = 1,numat
    indi = 3*(i - 1)
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
            d2(indj+1,indi+1) = d2(indl+1,indi+1)
            d2(indj+2,indi+1) = d2(indl+2,indi+1)
            d2(indj+3,indi+1) = d2(indl+3,indi+1)
            d2(indj+1,indi+2) = d2(indl+1,indi+2)
            d2(indj+2,indi+2) = d2(indl+2,indi+2)
            d2(indj+3,indi+2) = d2(indl+3,indi+2)
            d2(indj+1,indi+3) = d2(indl+1,indi+3)
            d2(indj+2,indi+3) = d2(indl+2,indi+3)
            d2(indj+3,indi+3) = d2(indl+3,indi+3)
            lfirst1 = .false.
          else
!
!  Subsequent add to block
!
            d2(indj+1,indi+1) = d2(indj+1,indi+1) + d2(indl+1,indi+1)
            d2(indj+2,indi+1) = d2(indj+2,indi+1) + d2(indl+2,indi+1)
            d2(indj+3,indi+1) = d2(indj+3,indi+1) + d2(indl+3,indi+1)
            d2(indj+1,indi+2) = d2(indj+1,indi+2) + d2(indl+1,indi+2)
            d2(indj+2,indi+2) = d2(indj+2,indi+2) + d2(indl+2,indi+2)
            d2(indj+3,indi+2) = d2(indj+3,indi+2) + d2(indl+3,indi+2)
            d2(indj+1,indi+3) = d2(indj+1,indi+3) + d2(indl+1,indi+3)
            d2(indj+2,indi+3) = d2(indj+2,indi+3) + d2(indl+2,indi+3)
            d2(indj+3,indi+3) = d2(indj+3,indi+3) + d2(indl+3,indi+3)
          endif
        endif
      enddo
    enddo
  enddo
!
!  Pass 2 : shift i direction
!
  do i = 1,ncsfoc
    indi = 3*(i-1)
    do j = 1,ncsfoc
      indj = 3*(j-1)
      lfirst1 = .true.
      do l = 1,numat
        if (iocptr(l).eq.j) then
          indl = 3*(l - 1)
          if (lfirst1) then
!
!  If the first occurance overwrite block
!
            d2(indi+1,indj+1) = d2(indi+1,indl+1)
            d2(indi+2,indj+1) = d2(indi+2,indl+1)
            d2(indi+3,indj+1) = d2(indi+3,indl+1)
            d2(indi+1,indj+2) = d2(indi+1,indl+2)
            d2(indi+2,indj+2) = d2(indi+2,indl+2)
            d2(indi+3,indj+2) = d2(indi+3,indl+2)
            d2(indi+1,indj+3) = d2(indi+1,indl+3)
            d2(indi+2,indj+3) = d2(indi+2,indl+3)
            d2(indi+3,indj+3) = d2(indi+3,indl+3)
            lfirst1 = .false.
          else
!
!  Subsequent add to block
!
            d2(indi+1,indj+1) = d2(indi+1,indj+1) + d2(indi+1,indl+1)
            d2(indi+2,indj+1) = d2(indi+2,indj+1) + d2(indi+2,indl+1)
            d2(indi+3,indj+1) = d2(indi+3,indj+1) + d2(indi+3,indl+1)
            d2(indi+1,indj+2) = d2(indi+1,indj+2) + d2(indi+1,indl+2)
            d2(indi+2,indj+2) = d2(indi+2,indj+2) + d2(indi+2,indl+2)
            d2(indi+3,indj+2) = d2(indi+3,indj+2) + d2(indi+3,indl+2)
            d2(indi+1,indj+3) = d2(indi+1,indj+3) + d2(indi+1,indl+3)
            d2(indi+2,indj+3) = d2(indi+2,indj+3) + d2(indi+2,indl+3)
            d2(indi+3,indj+3) = d2(indi+3,indj+3) + d2(indi+3,indl+3)
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
    mis = mint + 3
    mjs = mint + 3
    mir = 3*ncsfoc
    mjr = 3*ncsfoc
    do i = 1,numat
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
              d2(indj+1,mis+i) = d2(indl+1,mis+i)
              d2(indj+2,mis+i) = d2(indl+2,mis+i)
              d2(indj+3,mis+i) = d2(indl+3,mis+i)
              d2(mis+i,indj+1) = d2(mis+i,indl+1)
              d2(mis+i,indj+2) = d2(mis+i,indl+2)
              d2(mis+i,indj+3) = d2(mis+i,indl+3)
              lfirst1 = .false.
            else
!
!  Second occurance -> add terms on
!
              d2(indj+1,mis+i) = d2(indj+1,mis+i) + d2(indl+1,mis+i)
              d2(indj+2,mis+i) = d2(indj+2,mis+i) + d2(indl+2,mis+i)
              d2(indj+3,mis+i) = d2(indj+3,mis+i) + d2(indl+3,mis+i)
              d2(mis+i,indj+1) = d2(mis+i,indj+1) + d2(mis+i,indl+1)
              d2(mis+i,indj+2) = d2(mis+i,indj+2) + d2(mis+i,indl+2)
              d2(mis+i,indj+3) = d2(mis+i,indj+3) + d2(mis+i,indl+3)
            endif
          endif
        enddo
      enddo
      do j = 1,nbfoc
        lfirst1 = .true.
        do l = 1,numat
          if (ibocptr(l).eq.j) then
            if (lfirst1) then
              d2(mjr+j,mis+i) = d2(mjs+l,mis+i)
              lfirst1 = .false.
            else
              d2(mjr+j,mis+i) = d2(mjr+j,mis+i) + d2(mjs+l,mis+i)
            endif
          endif
        enddo
      enddo
    enddo
!
!  Pass 2 : reduce numat to nbfoc in remaining directions
!
    do i = 1,ncsfoc
      indi = 3*(i-1)
      do j = 1,nbfoc
        lfirst1 = .true.
        do l = 1,numat
          if (ibocptr(l).eq.j) then
            if (lfirst1) then
!
!  First occurance -> overwrite block
!
              d2(indi+1,mir+j) = d2(indi+1,mis+l)
              d2(indi+2,mir+j) = d2(indi+2,mis+l)
              d2(indi+3,mir+j) = d2(indi+3,mis+l)
              d2(mir+j,indi+1) = d2(mis+l,indi+1)
              d2(mir+j,indi+2) = d2(mis+l,indi+2)
              d2(mir+j,indi+3) = d2(mis+l,indi+3)
              lfirst1 = .false.
            else
!
!  Second occurance -> add terms on
!
              d2(indi+1,mir+j) = d2(indi+1,mir+j) + d2(indi+1,mis+l)
              d2(indi+2,mir+j) = d2(indi+2,mir+j) + d2(indi+2,mis+l)
              d2(indi+3,mir+j) = d2(indi+3,mir+j) + d2(indi+3,mis+l)
              d2(mir+j,indi+1) = d2(mir+j,indi+1) + d2(mis+l,indi+1)
              d2(mir+j,indi+2) = d2(mir+j,indi+2) + d2(mis+l,indi+2)
              d2(mir+j,indi+3) = d2(mir+j,indi+3) + d2(mis+l,indi+3)
            endif
          endif
        enddo
      enddo
    enddo
    do i = 1,nbfoc
      do j = 1,nbfoc
        lfirst1 = .true.
        do l = 1,numat
          if (ibocptr(l).eq.j) then
            if (lfirst1) then
              d2(mjr+i,mir+j) = d2(mjr+i,mis+l)
              lfirst1 = .false.
            else
              d2(mjr+i,mir+j) = d2(mjr+i,mir+j) + d2(mjr+i,mis+l)
            endif
          endif
        enddo
      enddo
    enddo
  endif
!
  return
  end
