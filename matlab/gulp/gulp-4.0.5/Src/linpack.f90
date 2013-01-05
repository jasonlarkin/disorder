  subroutine dsidi(a,lda,n,kpvt,det,inert,work,job)
  use datatypes
  implicit none
  integer(i4) :: lda,n,job
  real(dp)    :: a(lda,n),work(n)
  real(dp)    :: det(2)
  integer(i4) :: kpvt(n),inert(3)
!
!     dsidi computes the determinant, inertia and inverse
!     of a double precision symmetric matrix using the factors from
!     dsifa.
!
!     on entry
!
!        a       double precision(lda,n)
!                the output from dsifa.
!
!        lda     integer
!                the leading dimension of the array a.
!
!        n       integer
!                the order of the matrix a.
!
!        kpvt    integer(n)
!                the pivot vector from dsifa.
!
!        work    double precision(n)
!                work vector.  contents destroyed.
!
!        job     integer
!                job has the decimal expansion  abc  where
!                   if  c .ne. 0, the inverse is computed,
!                   if  b .ne. 0, the determinant is computed,
!                   if  a .ne. 0, the inertia is computed.
!
!                for example, job = 111  gives all three.
!
!     on return
!
!        variables not requested by job are not used.
!
!        a      contains the upper triangle of the inverse of
!               the original matrix.  the strict lower triangle
!               is never referenced.
!
!        det    double precision(2)
!               determinant of original matrix.
!               determinant = det(1) * 10.0**det(2)
!               with 1.0 .le. dabs(det(1)) .lt. 10.0
!               or det(1) = 0.0.
!
!        inert  integer(3)
!               the inertia of the original matrix.
!               inert(1)  =  number of positive eigenvalues.
!               inert(2)  =  number of negative eigenvalues.
!               inert(3)  =  number of zero eigenvalues.
!
!     error condition
!
!        a division by zero may occur if the inverse is requested
!        and  dsico  has set rcond .eq. 0.0
!        or  dsifa  has set  info .ne. 0 .
!
!     linpack. this version dated 08/14/78 .
!     james bunch, univ. calif. san diego, argonne nat. lab
!
!     subroutines and functions
!
!     blas daxpy,dcopy,ddot,dswap
!     fortran dabs,abs,mod
!
!     internal variables.
!
  real(dp)    :: akkp1,ddot,temp
  real(dp)    :: ten,d,t,ak,akp1
  integer(i4) :: j,jb,k,km1,ks,kstep
  logical     :: noinv,nodet,noert
!
  noinv = mod(job,10_i4) .eq. 0
  nodet = mod(job,100_i4)/10 .eq. 0
  noert = mod(job,1000_i4)/100 .eq. 0
!
  if (nodet .and. noert) go to 140
     if (noert) go to 10
        inert(1) = 0
        inert(2) = 0
        inert(3) = 0
10    continue
     if (nodet) go to 20
        det(1) = 1.0_dp
        det(2) = 0.0_dp
        ten = 10.0_dp
20    continue
     t = 0.0_dp
     do 130 k = 1, n
        d = a(k,k)
!
!           check if 1 by 1
!
        if (kpvt(k) .gt. 0) go to 50
!
!              2 by 2 block
!              use det (d  s)  =  (d/t * c - t) * t  ,  t = dabs(s)
!                      (s  c)
!              to avoid underflow/overflow troubles.
!              take two passes through scaling.  use  t  for flag.
!
           if (t .ne. 0.0_dp) go to 30
              t = dabs(a(k,k+1))
              d = (d/t)*a(k+1,k+1) - t
           go to 40
30          continue
              d = t
              t = 0.0_dp
40          continue
50       continue
!
        if (noert) go to 60
           if (d .gt. 0.0_dp) inert(1) = inert(1) + 1
           if (d .lt. 0.0_dp) inert(2) = inert(2) + 1
           if (d .eq. 0.0_dp) inert(3) = inert(3) + 1
60       continue
!
        if (nodet) go to 120
           det(1) = d*det(1)
           if (det(1) .eq. 0.0_dp) go to 110
70             if (dabs(det(1)) .ge. 1.0_dp) go to 80
                 det(1) = ten*det(1)
                 det(2) = det(2) - 1.0_dp
              go to 70
80             continue
90             if (dabs(det(1)) .lt. ten) go to 100
                 det(1) = det(1)/ten
                 det(2) = det(2) + 1.0_dp
              go to 90
100             continue
110          continue
120       continue
130    continue
140 continue
!
!     compute inverse(a)
!
  if (noinv) go to 270
     k = 1
150    if (k .gt. n) go to 260
        km1 = k - 1
        if (kpvt(k) .lt. 0) go to 180
!
!              1 by 1
!
           a(k,k) = 1.0_dp/a(k,k)
           if (km1 .lt. 1) go to 170
              call dcopy(km1,a(1,k),1_i4,work,1_i4)
              do 160 j = 1, km1
                 a(j,k) = ddot(j,a(1,j),1_i4,work,1_i4)
                 call daxpy(j-1_i4,work(j),a(1,j),1_i4,a(1,k),1_i4)
160             continue
              a(k,k) = a(k,k) + ddot(km1,work,1_i4,a(1,k),1_i4)
170          continue
           kstep = 1
        go to 220
180       continue
!
!              2 by 2
!
           t = dabs(a(k,k+1))
           ak = a(k,k)/t
           akp1 = a(k+1,k+1)/t
           akkp1 = a(k,k+1)/t
           d = t*(ak*akp1 - 1.0_dp)
           a(k,k) = akp1/d
           a(k+1,k+1) = ak/d
           a(k,k+1) = -akkp1/d
           if (km1 .lt. 1) go to 210
              call dcopy(km1,a(1,k+1),1_i4,work,1_i4)
              do 190 j = 1, km1
                 a(j,k+1) = ddot(j,a(1,j),1_i4,work,1_i4)
                 call daxpy(j-1_i4,work(j),a(1,j),1_i4,a(1,k+1), &
                   1_i4)
190             continue
              a(k+1,k+1) = a(k+1,k+1) + ddot(km1,work,1_i4, &
                a(1,k+1),1_i4)
              a(k,k+1) = a(k,k+1) + ddot(km1,a(1,k),1_i4,a(1,k+1), &
                1_i4)
              call dcopy(km1,a(1,k),1_i4,work,1_i4)
              do 200 j = 1, km1
                 a(j,k) = ddot(j,a(1,j),1_i4,work,1_i4)
                 call daxpy(j-1_i4,work(j),a(1,j),1_i4,a(1,k),1_i4)
200             continue
              a(k,k) = a(k,k) + ddot(km1,work,1_i4,a(1,k),1_i4)
210          continue
           kstep = 2
220       continue
!
!           swap
!
        ks = abs(kpvt(k))
        if (ks .eq. k) go to 250
           call dswap(ks,a(1,ks),1_i4,a(1,k),1_i4)
           do 230 jb = ks, k
              j = k + ks - jb
              temp = a(j,k)
              a(j,k) = a(ks,j)
              a(ks,j) = temp
230          continue
           if (kstep .eq. 1) go to 240
              temp = a(ks,k+1)
              a(ks,k+1) = a(k,k+1)
              a(k,k+1) = temp
240          continue
250       continue
        k = k + kstep
     go to 150
260    continue
270 continue
  return
  end
  subroutine dsifa(a,lda,n,kpvt,info)
  use datatypes
  implicit none
  integer(i4) :: lda,n,kpvt(n),info
  real(dp)    :: a(lda,n)
!
!     dsifa factors a double precision symmetric matrix by elimination
!     with symmetric pivoting.
!
!     to solve  a*x = b , follow dsifa by dsisl.
!     to compute  inverse(a)*c , follow dsifa by dsisl.
!     to compute  determinant(a) , follow dsifa by dsidi.
!     to compute  inertia(a) , follow dsifa by dsidi.
!     to compute  inverse(a) , follow dsifa by dsidi.
!
!     on entry
!
!        a       double precision(lda,n)
!                the symmetric matrix to be factored.
!                only the diagonal and upper triangle are used.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!     on return
!
!        a       a block diagonal matrix and the multipliers which
!                were used to obtain it.
!                the factorization can be written  a = u*d*trans(u)
!                where  u  is a product of permutation and unit
!                upper triangular matrices , trans(u) is the
!                transpose of  u , and  d  is block diagonal
!                with 1 by 1 and 2 by 2 blocks.
!
!        kpvt    integer(n)
!                an integer vector of pivot indices.
!
!        info    integer
!                = 0  normal value.
!                = k  if the k-th pivot block is singular. this is
!                     not an error condition for this subroutine,
!                     but it does indicate that dsisl or dsidi may
!                     divide by zero if called.
!
!     linpack. this version dated 08/14/78 .
!     james bunch, univ. calif. san diego, argonne nat. lab.
!
!     subroutines and functions
!
!     blas daxpy,dswap,idamax
!     fortran dabs,dmax1,sqrt
!
!     internal variables
!
  real(dp)    :: ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
  real(dp)    :: absakk,alpha,colmax,rowmax
  integer(i4) :: imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,idamax
  logical     :: swap
!
!
!     initialize
!
!     alpha is used in choosing pivot block size.
  alpha = (1.0_dp + sqrt(17.0_dp))/8.0_dp
!
  info = 0
!
!     main loop on k, which goes from n to 1.
!
  k = n
10 continue
!
!        leave the loop if k=0 or k=1.
!
!     ...exit
     if (k .eq. 0) go to 200
     if (k .gt. 1) go to 20
        kpvt(1) = 1
        if (a(1,1) .eq. 0.0_dp) info = 1
!     ......exit
        go to 200
20    continue
!
!        this section of code determines the kind of
!        elimination to be performed.  when it is completed,
!        kstep will be set to the size of the pivot block, and
!        swap will be set to .true. if an interchange is
!        required.
!
     km1 = k - 1
     absakk = dabs(a(k,k))
!
!        determine the largest off-diagonal element in
!        column k.
!
     imax = idamax(k-1_i4,a(1,k),1_i4)
     colmax = dabs(a(imax,k))
     if (absakk .lt. alpha*colmax) go to 30
        kstep = 1
        swap = .false.
     go to 90
30    continue
!
!           determine the largest off-diagonal element in
!           row imax.
!
        rowmax = 0.0_dp
        imaxp1 = imax + 1
        do 40 j = imaxp1, k
           rowmax = dmax1(rowmax,dabs(a(imax,j)))
40       continue
        if (imax .eq. 1) go to 50
           jmax = idamax(imax-1_i4,a(1,imax),1_i4)
           rowmax = dmax1(rowmax,dabs(a(jmax,imax)))
50       continue
        if (dabs(a(imax,imax)) .lt. alpha*rowmax) go to 60
           kstep = 1
           swap = .true.
        go to 80
60       continue
        if (absakk .lt. alpha*colmax*(colmax/rowmax)) go to 70
           kstep = 1
           swap = .false.
        go to 80
70       continue
           kstep = 2
           swap = imax .ne. km1
80       continue
90    continue
     if (dmax1(absakk,colmax) .ne. 0.0_dp) go to 100
!
!           column k is zero.  set info and iterate the loop.
!
        kpvt(k) = k
        info = k
     go to 190
100    continue
     if (kstep .eq. 2) go to 140
!
!           1 x 1 pivot block.
!
        if (.not.swap) go to 120
!
!              perform an interchange.
!
           call dswap(imax,a(1,imax),1_i4,a(1,k),1_i4)
           do 110 jj = imax, k
              j = k + imax - jj
              t = a(j,k)
              a(j,k) = a(imax,j)
              a(imax,j) = t
110          continue
120       continue
!
!           perform the elimination.
!
        do 130 jj = 1, km1
           j = k - jj
           mulk = -a(j,k)/a(k,k)
           t = mulk
           call daxpy(j,t,a(1,k),1_i4,a(1,j),1_i4)
           a(j,k) = mulk
130       continue
!
!           set the pivot array.
!
        kpvt(k) = k
        if (swap) kpvt(k) = imax
     go to 190
140    continue
!
!           2 x 2 pivot block.
!
        if (.not.swap) go to 160
!
!              perform an interchange.
!
           call dswap(imax,a(1,imax),1_i4,a(1,k-1),1_i4)
           do 150 jj = imax, km1
              j = km1 + imax - jj
              t = a(j,k-1)
              a(j,k-1) = a(imax,j)
              a(imax,j) = t
150          continue
           t = a(k-1,k)
           a(k-1,k) = a(imax,k)
           a(imax,k) = t
160       continue
!
!           perform the elimination.
!
        km2 = k - 2
        if (km2 .eq. 0) go to 180
           ak = a(k,k)/a(k-1,k)
           akm1 = a(k-1,k-1)/a(k-1,k)
           denom = 1.0_dp - ak*akm1
           do 170 jj = 1, km2
              j = km1 - jj
              bk = a(j,k)/a(k-1,k)
              bkm1 = a(j,k-1)/a(k-1,k)
              mulk = (akm1*bk - bkm1)/denom
              mulkm1 = (ak*bkm1 - bk)/denom
              t = mulk
              call daxpy(j,t,a(1,k),1_i4,a(1,j),1_i4)
              t = mulkm1
              call daxpy(j,t,a(1,k-1),1_i4,a(1,j),1_i4)
              a(j,k) = mulk
              a(j,k-1) = mulkm1
170          continue
180       continue
!
!           set the pivot array.
!
        kpvt(k) = 1 - k
        if (swap) kpvt(k) = -imax
        kpvt(k-1) = kpvt(k)
190    continue
     k = k - kstep
  go to 10
200 continue
  return
  end

  subroutine zhidi(a,lda,n,kpvt,det,inert,work,job)
  use datatypes
  implicit none
  integer(i4)  :: lda,n,job
  complex(dpc) :: a(lda,n),work(n)
  real(dp)     :: det(2)
  integer(i4)  :: kpvt(n),inert(3)
!
!     zhidi computes the determinant, inertia and inverse
!     of a complex*16 hermitian matrix using the factors from zhifa.
!
!     on entry
!
!        a       complex*16(lda,n)
!                the output from zhifa.
!
!        lda     integer
!                the leading dimension of the array a.
!
!        n       integer
!                the order of the matrix a.
!
!        kpvt    integer(n)
!                the pivot vector from zhifa.
!
!        work    complex*16(n)
!                work vector.  contents destroyed.
!
!        job     integer
!                job has the decimal expansion  abc  where
!                   if  c .ne. 0, the inverse is computed,
!                   if  b .ne. 0, the determinant is computed,
!                   if  a .ne. 0, the inertia is computed.
!
!                for example, job = 111  gives all three.
!
!     on return
!
!        variables not requested by job are not used.
!
!        a      contains the upper triangle of the inverse of
!               the original matrix.  the strict lower triangle
!               is never referenced.
!
!        det    double precision(2)
!               determinant of original matrix.
!               determinant = det(1) * 10.0**det(2)
!               with 1.0 .le. dabs(det(1)) .lt. 10.0
!               or det(1) = 0.0.
!
!        inert  integer(3)
!               the inertia of the original matrix.
!               inert(1)  =  number of positive eigenvalues.
!               inert(2)  =  number of negative eigenvalues.
!               inert(3)  =  number of zero eigenvalues.
!
!     error condition
!
!        a division by zero may occur if the inverse is requested
!        and  zhico  has set rcond .eq. 0.0
!        or  zhifa  has set  info .ne. 0 .
!
!     linpack. this version dated 08/14/78 .
!     james bunch, univ. calif. san diego, argonne nat. lab
!
!     subroutines and functions
!
!     blas zaxpy,zcopy,zdotc,zswap
!     fortran dabs,cdabs,cmplx,conjg,abs,mod
!
!     internal variables.
!
  complex(dpc) :: akkp1,zdotc,temp
  real(dp)     :: ten,d,t,ak,akp1,cdabs
  integer(i4)  :: j,jb,k,km1,ks,kstep
  logical      :: noinv,nodet,noert
!
  noinv = mod(job,10_i4) .eq. 0
  nodet = mod(job,100_i4)/10 .eq. 0
  noert = mod(job,1000_i4)/100 .eq. 0
!
  if (nodet .and. noert) go to 140
     if (noert) go to 10
        inert(1) = 0
        inert(2) = 0
        inert(3) = 0
10    continue
     if (nodet) go to 20
        det(1) = 1.0_dp
        det(2) = 0.0_dp
        ten = 10.0_dp
20    continue
     t = 0.0_dp
     do 130 k = 1, n
        d = dble(a(k,k))
!
!           check if 1 by 1
!
        if (kpvt(k) .gt. 0) go to 50
!
!              2 by 2 block
!              use det (d  s)  =  (d/t * c - t) * t  ,  t = cdabs(s)
!                      (s  c)
!              to avoid underflow/overflow troubles.
!              take two passes through scaling.  use  t  for flag.
!
           if (t .ne. 0.0_dp) go to 30
              t = cdabs(a(k,k+1))
              d = (d/t)*dble(a(k+1,k+1)) - t
           go to 40
30          continue
              d = t
              t = 0.0_dp
40          continue
50       continue
!
        if (noert) go to 60
           if (d .gt. 0.0_dp) inert(1) = inert(1) + 1
           if (d .lt. 0.0_dp) inert(2) = inert(2) + 1
           if (d .eq. 0.0_dp) inert(3) = inert(3) + 1
60       continue
!
        if (nodet) go to 120
           det(1) = d*det(1)
           if (det(1) .eq. 0.0_dp) go to 110
70             if (dabs(det(1)) .ge. 1.0_dp) go to 80
                 det(1) = ten*det(1)
                 det(2) = det(2) - 1.0_dp
              go to 70
80             continue
90             if (dabs(det(1)) .lt. ten) go to 100
                 det(1) = det(1)/ten
                 det(2) = det(2) + 1.0_dp
              go to 90
100             continue
110          continue
120       continue
130    continue
140 continue
!
!     compute inverse(a)
!
  if (noinv) go to 270
     k = 1
150    if (k .gt. n) go to 260
        km1 = k - 1
        if (kpvt(k) .lt. 0) go to 180
!
!              1 by 1
!
           a(k,k) = cmplx(1.0_dp/dble(a(k,k)),0.0_dp)
           if (km1 .lt. 1) go to 170
              call zcopy(km1,a(1,k),1_i4,work,1_i4)
              do 160 j = 1, km1
                 a(j,k) = zdotc(j,a(1,j),1_i4,work,1_i4)
                 call zaxpy(j-1_i4,work(j),a(1,j),1_i4,a(1,k),1_i4)
160             continue
              a(k,k) = a(k,k) &
                + cmplx(dble(zdotc(km1,work,1_i4,a(1,k),1_i4)), &
                    0.0_dp)
170          continue
           kstep = 1
        go to 220
180       continue
!
!              2 by 2
!
           t = cdabs(a(k,k+1))
           ak = dble(a(k,k))/t
           akp1 = dble(a(k+1,k+1))/t
           akkp1 = a(k,k+1)/t
           d = t*(ak*akp1 - 1.0_dp)
           a(k,k) = cmplx(akp1/d,0.0_dp)
           a(k+1,k+1) = cmplx(ak/d,0.0_dp)
           a(k,k+1) = -akkp1/d
           if (km1 .lt. 1) go to 210
              call zcopy(km1,a(1,k+1),1_i4,work,1_i4)
              do 190 j = 1, km1
                 a(j,k+1) = zdotc(j,a(1,j),1_i4,work,1_i4)
                 call zaxpy(j-1_i4,work(j),a(1,j),1_i4,a(1,k+1), &
                   1_i4)
190             continue
              a(k+1,k+1) = a(k+1,k+1) &
                + cmplx(dble(zdotc(km1,work,1_i4, &
                    a(1,k+1),1_i4)),0.0_dp)
              a(k,k+1) = a(k,k+1) + zdotc(km1,a(1,k),1_i4,a(1,k+1), &
                1_i4)
              call zcopy(km1,a(1,k),1_i4,work,1_i4)
              do 200 j = 1, km1
                 a(j,k) = zdotc(j,a(1,j),1_i4,work,1_i4)
                 call zaxpy(j-1_i4,work(j),a(1,j),1_i4,a(1,k),1_i4)
200             continue
              a(k,k) = a(k,k) &
                 + cmplx(dble(zdotc(km1,work,1_i4,a(1,k),1_i4)), &
                     0.0_dp)
210          continue
           kstep = 2
220       continue
!
!           swap
!
        ks = abs(kpvt(k))
        if (ks .eq. k) go to 250
           call zswap(ks,a(1,ks),1_i4,a(1,k),1_i4)
           do 230 jb = ks, k
              j = k + ks - jb
              temp = conjg(a(j,k))
              a(j,k) = conjg(a(ks,j))
              a(ks,j) = temp
230          continue
           if (kstep .eq. 1) go to 240
              temp = a(ks,k+1)
              a(ks,k+1) = a(k,k+1)
              a(k,k+1) = temp
240          continue
250       continue
        k = k + kstep
     go to 150
260    continue
270 continue
  return
  end
  subroutine zhifa(a,lda,n,kpvt,info)
  use datatypes
  implicit none
  integer(i4)  :: lda,n,kpvt(n),info
  complex(dpc) :: a(lda,n)
!
!     zhifa factors a complex*16 hermitian matrix by elimination
!     with symmetric pivoting.
!
!     to solve  a*x = b , follow zhifa by zhisl.
!     to compute  inverse(a)*c , follow zhifa by zhisl.
!     to compute  determinant(a) , follow zhifa by zhidi.
!     to compute  inertia(a) , follow zhifa by zhidi.
!     to compute  inverse(a) , follow zhifa by zhidi.
!
!     on entry
!
!        a       complex*16(lda,n)
!                the hermitian matrix to be factored.
!                only the diagonal and upper triangle are used.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!     on return
!
!        a       a block diagonal matrix and the multipliers which
!                were used to obtain it.
!                the factorization can be written  a = u*d*ctrans(u)
!                where  u  is a product of permutation and unit
!                upper triangular matrices , ctrans(u) is the
!                conjugate transpose of  u , and  d  is block diagonal
!                with 1 by 1 and 2 by 2 blocks.
!
!        kpvt    integer(n)
!                an integer vector of pivot indices.
!
!        info    integer
!                = 0  normal value.
!                = k  if the k-th pivot block is singular. this is
!                     not an error condition for this subroutine,
!                     but it does indicate that zhisl or zhidi may
!                     divide by zero if called.
!
!     linpack. this version dated 08/14/78 .
!     james bunch, univ. calif. san diego, argonne nat. lab.
!
!     subroutines and functions
!
!     blas zaxpy,zswap,izamax
!     fortran dabs,dmax1,cmplx,conjg,sqrt
!
!     internal variables
!
  complex(dpc) :: ak,akm1,bk,bkm1,denom,mulk,mulkm1,t
  real(dp)     :: absakk,alpha,colmax,rowmax
  integer(i4)  :: imax,imaxp1,j,jj,jmax,k,km1,km2,kstep,izamax
  logical      :: swap
!
  complex(dpc) :: zdum
  real(dp)     :: cabs1
  cabs1(zdum) = dabs(dble(zdum)) + dabs(aimag(zdum))
!
!     initialize
!
!     alpha is used in choosing pivot block size.
  alpha = (1.0_dp + sqrt(17.0_dp))/8.0_dp
!
  info = 0
!
!     main loop on k, which goes from n to 1.
!
  k = n
10 continue
!
!        leave the loop if k=0 or k=1.
!
!     ...exit
     if (k .eq. 0) go to 200
     if (k .gt. 1) go to 20
        kpvt(1) = 1
        if (cabs1(a(1,1)) .eq. 0.0_dp) info = 1
!     ......exit
        go to 200
20    continue
!
!        this section of code determines the kind of
!        elimination to be performed.  when it is completed,
!        kstep will be set to the size of the pivot block, and
!        swap will be set to .true. if an interchange is
!        required.
!
     km1 = k - 1
     absakk = cabs1(a(k,k))
!
!        determine the largest off-diagonal element in
!        column k.
!
     imax = izamax(k-1_i4,a(1,k),1_i4)
     colmax = cabs1(a(imax,k))
     if (absakk .lt. alpha*colmax) go to 30
        kstep = 1
        swap = .false.
     go to 90
30    continue
!
!           determine the largest off-diagonal element in
!           row imax.
!
        rowmax = 0.0_dp
        imaxp1 = imax + 1
        do 40 j = imaxp1, k
           rowmax = dmax1(rowmax,cabs1(a(imax,j)))
40       continue
        if (imax .eq. 1) go to 50
           jmax = izamax(imax-1_i4,a(1,imax),1_i4)
           rowmax = dmax1(rowmax,cabs1(a(jmax,imax)))
50       continue
        if (cabs1(a(imax,imax)) .lt. alpha*rowmax) go to 60
           kstep = 1
           swap = .true.
        go to 80
60       continue
        if (absakk .lt. alpha*colmax*(colmax/rowmax)) go to 70
           kstep = 1
           swap = .false.
        go to 80
70       continue
           kstep = 2
           swap = imax .ne. km1
80       continue
90    continue
     if (dmax1(absakk,colmax) .ne. 0.0_dp) go to 100
!
!           column k is zero.  set info and iterate the loop.
!
        kpvt(k) = k
        info = k
     go to 190
100    continue
     if (kstep .eq. 2) go to 140
!
!           1 x 1 pivot block.
!
        if (.not.swap) go to 120
!
!              perform an interchange.
!
           call zswap(imax,a(1,imax),1_i4,a(1,k),1_i4)
           do 110 jj = imax, k
              j = k + imax - jj
              t = conjg(a(j,k))
              a(j,k) = conjg(a(imax,j))
              a(imax,j) = t
110          continue
120       continue
!
!           perform the elimination.
!
        do 130 jj = 1, km1
           j = k - jj
           mulk = -a(j,k)/a(k,k)
           t = conjg(mulk)
           call zaxpy(j,t,a(1,k),1_i4,a(1,j),1_i4)
           a(j,j) = cmplx(dble(a(j,j)),0.0_dp)
           a(j,k) = mulk
130       continue
!
!           set the pivot array.
!
        kpvt(k) = k
        if (swap) kpvt(k) = imax
     go to 190
140    continue
!
!           2 x 2 pivot block.
!
        if (.not.swap) go to 160
!
!              perform an interchange.
!
           call zswap(imax,a(1,imax),1_i4,a(1,k-1),1_i4)
           do 150 jj = imax, km1
              j = km1 + imax - jj
              t = conjg(a(j,k-1))
              a(j,k-1) = conjg(a(imax,j))
              a(imax,j) = t
150          continue
           t = a(k-1,k)
           a(k-1,k) = a(imax,k)
           a(imax,k) = t
160       continue
!
!           perform the elimination.
!
        km2 = k - 2
        if (km2 .eq. 0) go to 180
           ak = a(k,k)/a(k-1,k)
           akm1 = a(k-1,k-1)/conjg(a(k-1,k))
           denom = 1.0_dp - ak*akm1
           do 170 jj = 1, km2
              j = km1 - jj
              bk = a(j,k)/a(k-1,k)
              bkm1 = a(j,k-1)/conjg(a(k-1,k))
              mulk = (akm1*bk - bkm1)/denom
              mulkm1 = (ak*bkm1 - bk)/denom
              t = conjg(mulk)
              call zaxpy(j,t,a(1,k),1_i4,a(1,j),1_i4)
              t = conjg(mulkm1)
              call zaxpy(j,t,a(1,k-1),1_i4,a(1,j),1_i4)
              a(j,k) = mulk
              a(j,k-1) = mulkm1
              a(j,j) = cmplx(dble(a(j,j)),0.0_dp)
170          continue
180       continue
!
!           set the pivot array.
!
        kpvt(k) = 1 - k
        if (swap) kpvt(k) = -imax
        kpvt(k-1) = kpvt(k)
190    continue
     k = k - kstep
  go to 10
200 continue
  return
  end
  subroutine dgefa(a,lda,n,ipvt,info)
  use datatypes
  implicit none
  integer(i4) :: lda,n,ipvt(n),info
  real(dp)    :: a(lda,*)
!
!     dgefa factors a double precision matrix by gaussian elimination.
!
!     dgefa is usually called by dgeco, but it can be called
!     directly with a saving in time if  rcond  is not needed.
!     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
!
!     on entry
!
!        a       double precision(lda, n)
!                the matrix to be factored.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!     on return
!
!        a       an upper triangular matrix and the multipliers
!                which were used to obtain it.
!                the factorization can be written  a = l*u  where
!                l  is a product of permutation and unit lower
!                triangular matrices and  u  is upper triangular.
!
!        ipvt    integer(n)
!                an integer vector of pivot indices.
!
!        info    integer
!                = 0  normal value.
!                = k  if  u(k,k) .eq. 0.0 .  this is not an error
!                     condition for this subroutine, but it does
!                     indicate that dgesl or dgedi will divide by zero
!                     if called.  use  rcond  in dgeco for a reliable
!                     indication of singularity.
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,dscal,idamax
!
!     internal variables
!
  real(dp)    :: t
  integer(i4) :: idamax,j,k,kp1,l,nm1
!
!
!     gaussian elimination with partial pivoting
!
  info = 0
  nm1 = n - 1
  if (nm1 .lt. 1) go to 70
  do 60 k = 1, nm1
     kp1 = k + 1
!
!        find l = pivot index
!
     l = idamax(n-k+1_i4,a(k,k),1_i4) + k - 1
     ipvt(k) = l
!
!        zero pivot implies this column already triangularized
!
     if (a(l,k) .eq. 0.0_dp) go to 40
!
!           interchange if necessary
!
        if (l .eq. k) go to 10
           t = a(l,k)
           a(l,k) = a(k,k)
           a(k,k) = t
10       continue
!
!           compute multipliers
!
        t = -1.0_dp/a(k,k)
        call dscal(n-k,t,a(k+1,k),1_i4)
!
!           row elimination with column indexing
!
        do 30 j = kp1, n
           t = a(l,j)
           if (l .eq. k) go to 20
              a(l,j) = a(k,j)
              a(k,j) = t
20          continue
           call daxpy(n-k,t,a(k+1,k),1_i4,a(k+1,j),1_i4)
30       continue
     go to 50
40    continue
        info = k
50    continue
60 continue
70 continue
  ipvt(n) = n
  if (a(n,n) .eq. 0.0_dp) info = n
  return
  end
  subroutine dgesl(a,lda,n,ipvt,b,job)
  use datatypes
  implicit none
  integer(i4) :: lda,n,ipvt(n),job
  real(dp)    :: a(lda,*),b(n)
!
!     dgesl solves the double precision system
!     a * x = b  or  trans(a) * x = b
!     using the factors computed by dgeco or dgefa.
!
!     on entry
!
!        a       double precision(lda, n)
!                the output from dgeco or dgefa.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!        ipvt    integer(n)
!                the pivot vector from dgeco or dgefa.
!
!        b       double precision(n)
!                the right hand side vector.
!
!        job     integer
!                = 0         to solve  a*x = b ,
!                = nonzero   to solve  trans(a)*x = b  where
!                            trans(a)  is the transpose.
!
!     on return
!
!        b       the solution vector  x .
!
!     error condition
!
!        a division by zero will occur if the input factor contains a
!        zero on the diagonal.  technically this indicates singularity
!        but it is often caused by improper arguments or improper
!        setting of lda .  it will not occur if the subroutines are
!        called correctly and if dgeco has set rcond .gt. 0.0
!        or dgefa has set info .eq. 0 .
!
!     to compute  inverse(a) * c  where  c  is a matrix
!     with  p  columns
!           call dgeco(a,lda,n,ipvt,rcond,z)
!           if (rcond is too small) go to ...
!           do 10 j = 1, p
!              call dgesl(a,lda,n,ipvt,c(1,j),0)
!        10 continue
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,ddot
!
!     internal variables
!
  real(dp)    :: ddot,t
  integer(i4) :: k,kb,l,nm1
!
  nm1 = n - 1
  if (job .ne. 0) go to 50
!
!        job = 0 , solve  a * x = b
!        first solve  l*y = b
!
     if (nm1 .lt. 1) go to 30
     do 20 k = 1, nm1
        l = ipvt(k)
        t = b(l)
        if (l .eq. k) go to 10
           b(l) = b(k)
           b(k) = t
10       continue
        call daxpy(n-k,t,a(k+1,k),1_i4,b(k+1),1_i4)
20    continue
30    continue
!
!        now solve  u*x = y
!
     do 40 kb = 1, n
        k = n + 1 - kb
        b(k) = b(k)/a(k,k)
        t = -b(k)
        call daxpy(k-1_i4,t,a(1,k),1_i4,b(1),1_i4)
40    continue
  go to 100
50 continue
!
!        job = nonzero, solve  trans(a) * x = b
!        first solve  trans(u)*y = b
!
     do 60 k = 1, n
        t = ddot(k-1_i4,a(1,k),1_i4,b(1),1_i4)
        b(k) = (b(k) - t)/a(k,k)
60    continue
!
!        now solve trans(l)*x = y
!
     if (nm1 .lt. 1) go to 90
     do 80 kb = 1, nm1
        k = n - kb
        b(k) = b(k) + ddot(n-k,a(k+1,k),1_i4,b(k+1),1_i4)
        l = ipvt(k)
        if (l .eq. k) go to 70
           t = b(l)
           b(l) = b(k)
           b(k) = t
70       continue
80    continue
90    continue
100 continue
  return
  end
  subroutine dgedi(a,lda,n,ipvt,det,work,job)
  use datatypes
  implicit none
  integer(i4) :: lda,n,ipvt(1),job
  real(dp)    :: a(lda,1),det(2),work(1)
!
!     dgedi computes the determinant and inverse of a matrix
!     using the factors computed by dgeco or dgefa.
!
!     on entry
!
!        a       double precision(lda, n)
!                the output from dgeco or dgefa.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!        ipvt    integer(n)
!                the pivot vector from dgeco or dgefa.
!
!        work    double precision(n)
!                work vector.  contents destroyed.
!
!        job     integer
!                = 11   both determinant and inverse.
!                = 01   inverse only.
!                = 10   determinant only.
!
!     on return
!
!        a       inverse of original matrix if requested.
!                otherwise unchanged.
!
!        det     double precision(2)
!                determinant of original matrix if requested.
!                otherwise not referenced.
!                determinant = det(1) * 10.0**det(2)
!                with  1.0 .le. dabs(det(1)) .lt. 10.0
!                or  det(1) .eq. 0.0 .
!
!     error condition
!
!        a division by zero will occur if the input factor contains
!        a zero on the diagonal and the inverse is requested.
!        it will not occur if the subroutines are called correctly
!        and if dgeco has set rcond .gt. 0.0 or dgefa has set
!        info .eq. 0 .
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,dscal,dswap
!     fortran dabs,mod
!
!     internal variables
!
  real(dp)    :: t
  real(dp)    :: ten
  integer(i4) :: i,j,k,kb,kp1,l,nm1
!
!
!     compute determinant
!
  if (job/10 .eq. 0) go to 70
     det(1) = 1.0_dp
     det(2) = 0.0_dp
     ten = 10.0_dp
     do 50 i = 1, n
        if (ipvt(i) .ne. i) det(1) = -det(1)
        det(1) = a(i,i)*det(1)
!        ...exit
        if (det(1) .eq. 0.0_dp) go to 60
10       if (dabs(det(1)) .ge. 1.0_dp) go to 20
           det(1) = ten*det(1)
           det(2) = det(2) - 1.0_dp
        go to 10
20       continue
30       if (dabs(det(1)) .lt. ten) go to 40
           det(1) = det(1)/ten
           det(2) = det(2) + 1.0_dp
        go to 30
40       continue
50    continue
60    continue
70 continue
!
!     compute inverse(u)
!
  if (mod(job,10_i4) .eq. 0) go to 150
     do 100 k = 1, n
        a(k,k) = 1.0_dp/a(k,k)
        t = -a(k,k)
        call dscal(k-1_i4,t,a(1,k),1_i4)
        kp1 = k + 1
        if (n .lt. kp1) go to 90
        do 80 j = kp1, n
           t = a(k,j)
           a(k,j) = 0.0_dp
           call daxpy(k,t,a(1,k),1_i4,a(1,j),1_i4)
80       continue
90       continue
100    continue
!
!        form inverse(u)*inverse(l)
!
     nm1 = n - 1
     if (nm1 .lt. 1) go to 140
     do 130 kb = 1, nm1
        k = n - kb
        kp1 = k + 1
        do 110 i = kp1, n
           work(i) = a(i,k)
           a(i,k) = 0.0_dp
110       continue
        do 120 j = kp1, n
           t = work(j)
           call daxpy(n,t,a(1,j),1_i4,a(1,k),1_i4)
120       continue
        l = ipvt(k)
        if (l .ne. k) call dswap(n,a(1,k),1_i4,a(1,l),1_i4)
130    continue
140    continue
150 continue
  return
  end
