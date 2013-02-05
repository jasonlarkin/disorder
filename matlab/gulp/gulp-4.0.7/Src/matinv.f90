  subroutine matinv(a,ia,n,wrk,ifail)
!
!  Matrix inverter
!
!  On entry :
!
!  a     = matrix to be inverted
!  ia    = lower dimension of a
!  n     = actual size of matrix to be inverted
!  wrk   = workspace array of length 2*n
!
!  On exit :
!
!  a     = inverse matrix
!  ifail = 0, if OK
!
  use times
  implicit none
!
!  Passed variables
!
  integer(i4)    :: ia
  integer(i4)    :: ifail
  integer(i4)    :: n
  real(dp)       :: a(ia,*)
  real(dp)       :: wrk(*)
!
!  Local variables
!
  integer(i4)    :: i
  integer(i4)    :: i1
  integer(i4)    :: ins
  integer(i4)    :: j
  integer(i4)    :: jj1
  integer(i4)    :: k
  integer(i4)    :: kk
  integer(i4)    :: l
  integer(i4)    :: l1
  integer(i4)    :: m
  integer(i4)    :: nupper
  real(dp)       :: acc
  real(dp)       :: aloc
  real(dp)       :: cputime
  real(dp)       :: t
  real(dp)       :: t1
  real(dp)       :: t2
!
  t1 = cputime()
  acc = 1.0d-8
  ifail = 0
  do j = 1,n
    if (j.ne.1) then
      call mxm1(a(j,1),ia,a(1,j),ia,wrk,ia,n-j+1_i4,j-1_i4)
      nupper = n - j + 1
      do l = 1,nupper
        a(j+l-1,j) = a(j+l-1,j) + wrk(l)
      enddo
    endif
    t = abs(a(j,j))
    k = j
    if (j.ne.n) then
      do i = j+1,n
        if (abs(a(i,j)).gt.t) then
          t = abs(a(i,j))
          k = i
        endif
      enddo
    endif
    wrk(j+n) = dble(k)
    if (t.le.acc) then
      ifail = 1
      t2 = cputime()
      tmati = tmati + t2 - t1
      return
    endif
    if (k.ne.j) then
      do m = 1,n
        t = a(j,m)
        a(j,m) = a(k,m)
        a(k,m) = t
      enddo
    endif
    a(j,j) = 1.0_dp/a(j,j)
    if (j.ne.n) then
      if (j.ne.1) then
        call mxm2(a(1,j+1),ia,a(j,1),ia,wrk,n-j,j-1_i4)
        nupper = n - j
        do l1 = 1,nupper
          a(j,j+l1) = a(j,j+l1) + wrk(l1)
        enddo
      endif
      t = - a(j,j)
      nupper = n - (j+1)
      do i1 = j+1,n
        a(j,i1) = t*a(j,i1)
      enddo
    endif
  enddo
!
!  Use cminv method to solve for a**-1
!
  do k = 2,n
    nupper = k - 1
    do m = 1,nupper
      wrk(m) = 0.0_dp
    enddo
    do j = 1,k-1
      aloc = a(k,j)
      do m = 1,j
        wrk(m) = wrk(m)-aloc*a(j,m)
      enddo
    enddo
    aloc = a(k,k)
    nupper = k - 1
    do m = 1,nupper
      a(k,m) = wrk(m)*aloc
    enddo
  enddo
!
!  Now back substitution
!
  k = n
  do kk = 2,n
    k = k - 1
    jj1 = kk - 1
    call mxm2(a(k+1,1),ia,a(k,k+1),ia,wrk,n,jj1)
    do l = 1,k
      wrk(l) = wrk(l)+a(k,l)
    enddo
    do j = 1,n
      a(k,j) = wrk(j)
    enddo
  enddo
!
!  Multiply solution by inverse of permutation matrix
!
  k = n + 1
  do i = 1,n
    k = k-1
    ins = int(wrk(k+n))
    if (ins.ne.k) then
      do j = 1,n
        wrk(j) = a(j,ins)
        a(j,ins) = a(j,k)
        a(j,k) = wrk(j)
      enddo
    endif
  enddo
  t2 = cputime()
  tmati = tmati + t2 - t1
  return
  end
!******************************
!  Ancillary matrix routines  *
!******************************
  subroutine mxm1(rarr,ir,sarr,jr,tarr,kr,id1,id2)
!
!  Matrix multiplier
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)    :: id1
  integer(i4)    :: id2
  integer(i4)    :: ir
  integer(i4)    :: jr
  integer(i4)    :: kr
  real(dp)       :: rarr(*)
  real(dp)       :: sarr(*)
  real(dp)       :: tarr(*)
!
!  Local variables
!
  integer(i4)    :: i
  integer(i4)    :: ia
  integer(i4)    :: j
  real(dp)       :: sum
!
  do i = 1,id1
    ia = i - ir
    sum = 0.0_dp
    do j = 1,id2
      ia = ia + ir
      sum = sum + rarr(ia)*sarr(j)
    enddo
    tarr(i) = sum
  enddo
  return
  end
  subroutine mxm2(rarr,ic,sarr,jc,tarr,id1,id2)
!
!  Matrix multiplier
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)    :: id1
  integer(i4)    :: id2
  integer(i4)    :: ic
  integer(i4)    :: jc
  real(dp)       :: rarr(*)
  real(dp)       :: sarr(*)
  real(dp)       :: tarr(*)
!
!  Local variables
!
  integer(i4)    :: i
  integer(i4)    :: ia
  integer(i4)    :: ira
  integer(i4)    :: j
  integer(i4)    :: ja
  real(dp)       :: sum
!
!
  ira = 1 - ic
  do i = 1,id1
    ira = ira + ic
    ia = ira - 1
    ja = 1 - jc
    sum = 0.0_dp
    do j = 1,id2
      ja = ja + jc
      sum = sum + rarr(ia+j)*sarr(ja)
    enddo
    tarr(i) = sum
  enddo
  return
  end
