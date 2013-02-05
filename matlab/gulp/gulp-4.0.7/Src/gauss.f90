  subroutine GULP_gauss(n,ndim,m,a)
!
!  Invert matrix A by Gaussian elimination 
!
!   3/07 Renamed from gauss to GULP_gauss to avoid Chemshell
!        conflicts
!
  use datatypes
  use general,    only : nwarn
  use iochannels
  implicit none
!
!  Passed variables
!
  integer(i4)         :: m
  integer(i4)         :: n
  integer(i4)         :: ndim
  real(dp)            :: a(ndim,n+m)
!
!  Local variables
!
  integer(i4)         :: i
  integer(i4)         :: ie
  integer(i4)         :: in
  integer(i4)         :: ix
  integer(i4)         :: j
  integer(i4)         :: k
  integer(i4)         :: kk
  real(dp)            :: delt
  real(dp)            :: u
  real(dp)            :: x
!
  delt = 1.0d-10
  if (n.gt.1) then
    do k = 1,n-1
      u = abs(a(k,k))
      kk = k + 1
      in = k
!
!  Search for index in of maximum pivot value
!      
      do i = kk,n
        if (abs(a(i,k)).gt.u) then
          u = abs(a(i,k))
          in = i
        endif
      enddo
      if (k.ne.in) then
!
!  Interchange rows k and index in
!
        do j = k,m+n
          x = a(k,j)
          a(k,j) = a(in,j)
          a(in,j) = x
        enddo
      endif
!
!  Check to see if pivot value is too small
!
      if (u.lt.delt) then
        nwarn = nwarn + 1
        call outwarning('Matrix is singular - Gaussian elimination cannot be performed',0_i4)
        return
      endif
!
!  Forward elimination step
!
      do i = kk,n
        do j = kk,m+n
          a(i,j) = a(i,j) - a(i,k)*a(k,j)/a(k,k)
        enddo
      enddo
    enddo
    if (abs(a(n,n)).lt.delt) then
      nwarn = nwarn + 1
      call outwarning('Matrix is singular - Gaussian elimination cannot be performed',0_i4)
      return
    endif
!
!  Back substitution
!
    do k = 1,m
      a(n,k+n) = a(n,k+n)/a(n,n)
      do ie = 1,n-1
        i = n - ie
        ix = i + 1
        do j = ix,n
          a(i,k+n) = a(i,k+n) - a(j,k+n)*a(i,j)
        enddo
        a(i,k+n) = a(i,k+n)/a(i,i)
      enddo
    enddo
    return
  elseif (abs(a(1,1)).lt.delt) then
    nwarn = nwarn + 1
    call outwarning('Matrix is singular - Gaussian elimination cannot be performed',0_i4)
    return
  endif
  x = 1.0_dp/a(1,1)
  do j = 1,m
    a(1,n+j) = a(1,n+j)*x
  enddo
!
  return
  end
