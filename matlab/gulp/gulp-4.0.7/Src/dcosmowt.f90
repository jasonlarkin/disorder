  subroutine dcosmowt(ipts,j,i,pw1,wtc,dwt,d2wt,lgrad2)
!
!  Subroutine to calculate the weight and the derivative of the weight
!  for the contribution of the j point on the sphere to atom i.
!
!  On entry : 
!
!  ipts             = global point pointer
!  j               = point on surface of sphere of atom i
!  i               = atom whose points are being looped over
!  pw1             = point weight due to sharing between segments
!  lgrad2          = if .true. then calculate the second derivatives as well
!
!  On exit :
!
!  wtc             = wtc - weight of current point
!  dwt(npwtloc)    = (d(wtc)/d(alpha)) - Cartesian first derivatives 
!                    of the weight
!  d2wt()          = second derivative contribution between different
!                    interatomic vectors. The first two dimensions are
!                    the 3 x 3 matrix of Cartesian directions, while 
!                    the third is a triangular storage index of the
!                    pair of vectors (npwtloc*(npwtloc+1)/2).
!
!  Here npwtloc (npwt(j,i)) is the number of atoms that contribute to 
!  the weighting factor of point (j,i). The atoms are references by the
!  pointer npwtptr(n,j,i).
!
!  Note to simplify the calculation of the second derivatives, it is assumed
!  that each point is only influenced by 1 image of each atom. Given the
!  extremely short range for sensible smoothing this should be a completely
!  valid approximation except for all but the most bizarre periodic case!
!
!   9/01 Second derivatives added
!   2/02 Modified to incorporate denominator derivatives
!   3/02 Second derivative error corrected
!   4/03 Order of storage in d2wt for alpha vs beta switched
!   1/05 Order of deallocation improved
!   1/05 Handling of drsolv/atsrad returned to original scheme
!  12/08 Migrated to version 3.5 and converted to f90 format
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use cosmo
  use current
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: ipts
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: j
  real(dp),    intent(in)             :: pw1
  real(dp),    intent(out)            :: wtc
  real(dp),    intent(out)            :: dwt(3,*)
  real(dp),    intent(out)            :: d2wt(3,3,*)
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: ii
  integer(i4)                         :: ix
  integer(i4)                         :: k
  integer(i4)                         :: k2
  integer(i4)                         :: kk
  integer(i4)                         :: kl
  integer(i4)                         :: l
  integer(i4)                         :: m
  integer(i4)                         :: npwtloc
  integer(i4)                         :: status
  logical                             :: lzero
  real(dp)                            :: cosmorange2
  real(dp)                            :: rotatedtm(3)
  real(dp)                            :: dist
  real(dp)                            :: dist2
  real(dp)                            :: dwtdr
  real(dp)                            :: d2wtdr2
  real(dp)                            :: r
  real(dp)                            :: rdist
  real(dp)                            :: rdist2
  real(dp)                            :: wt
  real(dp)                            :: xc, yc, zc
  real(dp)                            :: xi, yi, zi
  real(dp)                            :: xj, yj, zj
  real(dp)                            :: xk, yk, zk
  real(dp)                            :: xkc, ykc, zkc
  real(dp), dimension(:), allocatable :: wother
  real(dp), dimension(:), allocatable :: wother2
!
  npwtloc = npwt(ipts)
  cosmorange2 = cosmorange
!
!  Transform position of point j
!
  xc = sphere2(1,j)
  yc = sphere2(2,j)
  zc = sphere2(3,j)
  do ix = 1,3
    rotatedtm(ix) = xc*cosmotm(1,ix,i) + yc*cosmotm(2,ix,i) + zc*cosmotm(3,ix,i)
  enddo
!
!  Get values associated with atom i
!
  r  = atsrad(i)
  xi = xclat(i)
  yi = yclat(i)
  zi = zclat(i)
!
!  Find coordinates of point j relative to atom i
!
  xj = xi + rotatedtm(1)*r
  yj = yi + rotatedtm(2)*r
  zj = zi + rotatedtm(3)*r
!
!  Initialise wother array
!
  allocate(wother(npwtloc),stat=status)
  if (status/=0) call outofmemory('dcosmowt','wother')
  do k = 1,npwtloc
    wother(k) = pw1
  enddo
  if (lgrad2) then
    allocate(wother2(npwtloc*(npwtloc+1)/2),stat=status)
    if (status/=0) call outofmemory('dcosmowt','wother2')
    do k = 1,npwtloc*(npwtloc+1)/2
      wother2(k) = pw1
    enddo
  endif
  wtc = pw1
!
!  Loop over all atoms
!
  k = 0
  lzero = .false.
  dwt(1:3,1:npwtloc) = 0.0_dp
  if (lgrad2) then
    kl = npwtloc*(npwtloc+1)/2
    d2wt(1:3,1:3,1:kl) = 0.0_dp
  endif
  do while (k .lt. npwtloc .and. .not.lzero)
    k = k + 1
    kk = npwtptr(k,ipts)
    xk = xclat(kk) - xj
    yk = yclat(kk) - yj
    zk = zclat(kk) - zj
    ii = 0
    do while (ii .lt. iimax .and. .not.lzero)
      ii = ii + 1
      if (ii.ne.iimid.or.kk.ne.i) then
        xkc = xk + xvec1cell(ii)
        ykc = yk + yvec1cell(ii)
        zkc = zk + zvec1cell(ii)
        dist = xkc*xkc + ykc*ykc + zkc*zkc
        dist = sqrt(dist)
        dist2 = dist - atsrad(kk) - drsolv - cosmorange2
        if (dist2 .lt. -cosmorange) then
          lzero = .true.
          wt = 0.0_dp
          dwt(1:3,k) = 0.0_dp
          if (lgrad2) then
            do l = 1,npwtloc
              if (l.gt.k) then
                kl = l*(l-1)/2 + k
              else
                kl = k*(k-1)/2 + l
              endif
              d2wt(1:3,1:3,kl) = 0.0_dp
            enddo
          endif
!
!  Put the cumulative products of weighting factors to zero
!
          do l = 1,k-1
            wother(l) = 0.0_dp
          enddo
          do l = k+1,npwtloc
            wother(l) = 0.0_dp
          enddo
          if (lgrad2) then
            k2 = k*(k-1)/2
            do l = 1,k2
              wother2(l) = 0.0_dp
            enddo
            do l = k+1,npwtloc
              k2 = l*(l-1)/2
              do m = 1,k-1
                wother2(k2+m) = 0.0_dp
              enddo
              do m = k+1,l
                wother2(k2+m) = 0.0_dp
              enddo
            enddo
          endif
        elseif (dist2 .lt. 0.0_dp) then
          call switch(abs(dist2),wt,.true.,dwtdr,lgrad2,d2wtdr2)
          rdist = dwtdr/dist
          dwt(1,k) = dwt(1,k) - rdist*xkc
          dwt(2,k) = dwt(2,k) - rdist*ykc
          dwt(3,k) = dwt(3,k) - rdist*zkc
          if (lgrad2) then
            k2 = k*(k+1)/2
            rdist2 = (d2wtdr2 + rdist)/(dist**2)
            d2wt(1,1,k2) = d2wt(1,1,k2) + rdist2*xkc*xkc - rdist
            d2wt(2,1,k2) = d2wt(2,1,k2) + rdist2*ykc*xkc
            d2wt(3,1,k2) = d2wt(3,1,k2) + rdist2*zkc*xkc
            d2wt(1,2,k2) = d2wt(1,2,k2) + rdist2*xkc*ykc
            d2wt(2,2,k2) = d2wt(2,2,k2) + rdist2*ykc*ykc - rdist
            d2wt(3,2,k2) = d2wt(3,2,k2) + rdist2*zkc*ykc
            d2wt(1,3,k2) = d2wt(1,3,k2) + rdist2*xkc*zkc
            d2wt(2,3,k2) = d2wt(2,3,k2) + rdist2*ykc*zkc
            d2wt(3,3,k2) = d2wt(3,3,k2) + rdist2*zkc*zkc - rdist
          endif
!
!  Multiply the cumulative weighting factors of all other 
!  atoms excluding the present one.
!
          do l = 1,k-1
            wother(l) = wother(l)*wt
          enddo
          do l = k+1,npwtloc
            wother(l) = wother(l)*wt
          enddo
          if (lgrad2) then
            k2 = k*(k-1)/2
            do l = 1,k2
              wother2(l) = wother2(l)*wt
            enddo
            do l = k+1,npwtloc
              k2 = l*(l-1)/2
              do m = 1,k-1
                wother2(k2+m) = wother2(k2+m)*wt
              enddo
              do m = k+1,l
                wother2(k2+m) = wother2(k2+m)*wt
              enddo
            enddo
          endif
        else
          wt = 1.0_dp
        endif
        wtc = wtc*wt
      endif
    enddo
  enddo
!
!  Multiply derivatives by wother
!
  if (lzero) then
    do k = 1,npwtloc
      dwt(1:3,k) = 0.0_dp
    enddo
    if (lgrad2) then
      do k = 1,npwtloc*(npwtloc+1)/2
        d2wt(1:3,1:3,k) = 0.0_dp
      enddo
    endif
  else
    if (lgrad2) then
      kl = 0
      do k = 1,npwtloc
        do l = 1,k
          kl = kl + 1
          wt = pw1
          if (k .ne. l) then
            d2wt(1,1,kl) = wother2(kl)*dwt(1,l)*dwt(1,k)
            d2wt(2,1,kl) = wother2(kl)*dwt(2,l)*dwt(1,k)
            d2wt(3,1,kl) = wother2(kl)*dwt(3,l)*dwt(1,k)
            d2wt(1,2,kl) = wother2(kl)*dwt(1,l)*dwt(2,k)
            d2wt(2,2,kl) = wother2(kl)*dwt(2,l)*dwt(2,k)
            d2wt(3,2,kl) = wother2(kl)*dwt(3,l)*dwt(2,k)
            d2wt(1,3,kl) = wother2(kl)*dwt(1,l)*dwt(3,k)
            d2wt(2,3,kl) = wother2(kl)*dwt(2,l)*dwt(3,k)
            d2wt(3,3,kl) = wother2(kl)*dwt(3,l)*dwt(3,k)
          else
            d2wt(1,1,kl) = wother(k)*d2wt(1,1,kl)
            d2wt(2,1,kl) = wother(k)*d2wt(2,1,kl)
            d2wt(3,1,kl) = wother(k)*d2wt(3,1,kl)
            d2wt(1,2,kl) = wother(k)*d2wt(1,2,kl)
            d2wt(2,2,kl) = wother(k)*d2wt(2,2,kl)
            d2wt(3,2,kl) = wother(k)*d2wt(3,2,kl)
            d2wt(1,3,kl) = wother(k)*d2wt(1,3,kl)
            d2wt(2,3,kl) = wother(k)*d2wt(2,3,kl)
            d2wt(3,3,kl) = wother(k)*d2wt(3,3,kl)
          endif
        enddo
      enddo
    endif
    do k = 1,npwtloc
      do ix = 1,3
        dwt(ix,k) = dwt(ix,k)*wother(k)
      enddo
    enddo
  endif
!
!  Free wother array(s)
!
  if (lgrad2) then
    deallocate(wother2,stat=status)
    if (status/=0) call deallocate_error('dcosmowt','wother2')
  endif
  deallocate(wother,stat=status)
  if (status/=0) call deallocate_error('dcosmowt','wother')
!
  return
  end
