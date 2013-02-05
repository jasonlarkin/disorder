  subroutine dtotcosmowt(ipts,totwt,dwt,d2wt,lgrad2)
!
!  Subroutine calculates the total derivatives of the weighting factors
!  for inclusion of points into SAS.
!
!   2/02 Created from cosmoderv
!   1/05 Style updated
!  12/08 Migrated to version 3.5 and converted to f90 format
!
!  Input :
!
!  ipts         = segment no. whose weighting derivatives are to be calculated
!  totwt       = sum of all weights for this segment
!  lgrad2      = if .true. calculate second derivatives
!
!  Output :
!
!  dwt        = derivative of weight w.r.t. position of i
!  d2wt       = second derivative of weight w.r.t. position of i
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
!  Julian Gale, NRI, Curtin University, December 2008
!
  use cosmo
  use current
  implicit none
!
!  Passed variables
!
  logical,     intent(in)  :: lgrad2
  integer(i4), intent(in)  :: ipts
  real(dp),    intent(in)  :: totwt
  real(dp),    intent(out) :: dwt(3,maxnpwt,*)
  real(dp),    intent(out) :: d2wt(3,3,maxnpwt*(maxnpwt+1)/2,*)
!
!  Local variables
!
  integer(i4)                                      :: i, j, k
  integer(i4)                                      :: ia, ib
  integer(i4)                                      :: m
  integer(i4)                                      :: n
  integer(i4)                                      :: nari
  integer(i4)                                      :: nn
  integer(i4)                                      :: npwt1
  integer(i4)                                      :: nsetfi
  integer(i4)                                      :: status
  real(dp)                                         :: pw1
  real(dp)                                         :: w1
  real(dp)                                         :: wt
  real(dp),    dimension(:,:),   allocatable, save :: dwtsum
  real(dp),    dimension(:,:,:), allocatable, save :: d2wtsum
!
!  Check whether there are any weight derivatives
!  If not, return
!
  npwt1 = npwt(ipts)
  if (npwt1.eq.0) return
!
!  Assign local constants
!
!  nar         = no. of points for a given segment
!  nsetf       = pointer to first point of a given segment
!
  i = cosmoatomptr(ipts)
  nari = nar(ipts)
  nsetfi = nsetf(ipts)
!
!  Find number of atoms with contributions to this point and build pointer
!
!  npwt        = no. of atoms that contribute to weight of current point
!  npwtptr     = pointer to atoms that contribute to weight of current point
!
!  Allocate array for sum of derivatives
!
  allocate(dwtsum(3_i4,npwt1),stat=status)
  if (status/=0) call outofmemory('dtotcosmowt','dwtsum')
  dwtsum(1:3,1:npwt1) = 0.0_dp
  if (lgrad2) then
    allocate(d2wtsum(3_i4,3_i4,npwt1*(npwt1+1)/2),stat=status)
    if (status/=0) call outofmemory('dtotcosmowt','d2wtsum')
    d2wtsum(1:3,1:3,1:(npwt1*(npwt1+1)/2)) = 0.0_dp
  endif
!
!  Calculate derivatives of all individual weighting factors
!
  do k = 1,nari
    j = nset(k+nsetfi-1)
    pw1 = cosmopwt(k+nsetfi-1)
    call dcosmowt(ipts,j,i,pw1,wt,dwt(1,1,k),d2wt(1,1,1,k),lgrad2)
    do n = 1,npwt1
      dwtsum(1:3,n) = dwtsum(1:3,n) + dwt(1:3,n,k)
    enddo
    if (lgrad2) then
      nn = 0
      do m = 1,npwt1
        do n = 1,m
          nn = nn + 1
          do ia = 1,3
            do ib = 1,3
              d2wtsum(ib,ia,nn) = d2wtsum(ib,ia,nn) + d2wt(ib,ia,nn,k)
            enddo
          enddo
        enddo
      enddo
    endif
  enddo
!
!  Calculate overall individual weight derivatives
!
  do k = 1,nari
    j = nset(k+nsetfi-1)
    pw1 = cosmopwt(k+nsetfi-1)
    w1 = cosmowt(j,i)*pw1
    do n = 1,npwt1
      dwt(1:3,n,k) = totwt*(dwt(1:3,n,k) - w1*totwt*dwtsum(1:3,n))
    enddo
    if (lgrad2) then
      nn = 0
      do m = 1,npwt1
        do n = 1,m
          nn = nn + 1
          do ia = 1,3
            do ib = 1,3
              d2wt(ib,ia,nn,k) = totwt*(d2wt(ib,ia,nn,k) - w1*totwt*d2wtsum(ib,ia,nn) &
                - dwt(ia,m,k)*dwtsum(ib,n) - dwt(ib,n,k)*dwtsum(ia,m))
            enddo
          enddo
        enddo
      enddo
    endif
  enddo
!
!  Free local memory
!
  if (lgrad2) then
    deallocate(d2wtsum,stat=status)
    if (status/=0) call deallocate_error('dtotcosmowt','d2wtsum')
  endif
  deallocate(dwtsum,stat=status)
  if (status/=0) call deallocate_error('dtotcosmowt','dwtsum')
!
  return
  end
