  subroutine xctox0(n,xc,lgeometryOK)
!
!  Called by funct / fefunct to set linear structure array
!  from optimisation variables array xc.
!
!   8/97 Created from funct
!  12/00 Generalised for 0 to 3-D
!   6/01 Order of cell operations corrected and lra test added
!   5/02 Check for small cell added
!  10/03 Cell parameter variables option added
!   5/04 lmodco option introduced
!  11/04 Inverse cell parameters set
!   5/07 Application of cell strain moved to subroutine
!  12/07 Unused variables removed
!   5/08 Geometry check flag added as argument
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, May 2008
!
  use configurations
  use control,        only : lmodco
  use current
  use optimisation,   only : loptcellpar
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(in)                     :: n
  logical,      intent(out)                    :: lgeometryOK
  real(dp),     intent(in)                     :: xc(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: mvar
  integer(i4), dimension(:), allocatable       :: ncount
  integer(i4)                                  :: status
  logical                                      :: ldothis
  logical                                      :: lfound
  real(dp)                                     :: diff
!
  mvar = 3*nasym + nstrains
!
!  First substitute parameters into place
!
  if (ndim.gt.0) then
    do i = 1,3
      rv(1,i) = rvcfg(1,i,ncf)
      rv(2,i) = rvcfg(2,i,ncf)
      rv(3,i) = rvcfg(3,i,ncf)
    enddo
    if (.not.loptcellpar) then
      do i = 1,nstrains
        x0(i) = 1.0_dp
      enddo
    endif
  endif
  do i = 1,n
    x0(iopt(i)) = xc(i)
  enddo
!
!  Make sure all fractional coords are between 0 and 1
!
  if (lmodco) then
    if (ndim.eq.3) then
      do i = 1,3*nasym
        x0(i+nstrains) = mod(x0(i+nstrains)+10.0_dp,1.0_dp)
      enddo
    elseif (ndim.eq.2) then
      ind = nstrains + 1
      do i = 1,nasym
        x0(ind) = mod(x0(ind)+10.0_dp,1.0_dp)
        ind = ind + 1
        x0(ind) = mod(x0(ind)+10.0_dp,1.0_dp)
        ind = ind + 2
      enddo
    elseif (ndim.eq.1) then
      ind = nstrains + 1
      do i = 1,nasym
        x0(ind) = mod(x0(ind)+10.0_dp,1.0_dp)
        ind = ind + 3
      enddo
    endif
  endif
!**********************
!  Apply constraints  *
!**********************
  if (ncon.gt.0) then
    do i = 1,ncon
      x0(ncfix(i)) = 0.0_dp
    enddo
    do i = 1,ncon
      x0(ncfix(i)) = x0(ncvar(i))*conco(i) + conadd(i) + x0(ncfix(i))
    enddo
!
!  Handle additive constraints for fractional coordinates
!  - take nearest pair of images
!
    if (ndim.gt.0) then
      allocate(ncount(mvar),stat=status)
      if (status/=0) call outofmemory('xctox0','ncount')
      do i = 1,mvar
        ncount(i) = 0
      enddo
      do i = 1,ncon
        ii = ncfix(i)
        ncount(ii) = ncount(ii) + 1
      enddo
      do i = nstrains+1,mvar
!
!  Select only those coordinates which are fractional
!
        if (ndim.eq.3) then
          ldothis = .true.
        elseif (ndim.eq.2) then
          ldothis = (mod((i-nstrains),3_i4).ne.0)
        elseif (ndim.eq.1) then
          ldothis = (mod((i-nstrains),3_i4).eq.1)
        endif
        if (ncount(i).ge.2.and.ldothis) then
          lfound = .false.
          j = 0
          do while (.not.lfound.and.j.lt.ncon-1)
            j = j + 1
            if (ncfix(j).eq.i) then
              k = j
              do while (.not.lfound.and.k.lt.ncon) 
                k = k + 1
                lfound = (ncfix(k).eq.i)
              enddo
            endif
          enddo
          if (lfound) then
            diff = abs(x0(ncvar(j)) - x0(ncvar(k)))
            if (diff.gt.0.5_dp) then
              x0(i) = x0(i) + 0.5_dp
              x0(i) = mod(x0(i),1.0_dp)
            endif
          endif
        endif
      enddo
      deallocate(ncount,stat=status)
      if (status/=0) call deallocate_error('xctox0','ncount')
    endif
  endif
  lgeometryOK = .true.
  if (ndim.gt.0) then
!*************************
!  Apply strain to cell  *
!*************************
    call x0strain(lgeometryOK)
  endif
!
  return
  end
