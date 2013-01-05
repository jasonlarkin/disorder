  subroutine ch(nm,n,ar,ai,w,matz,zr,zi,fv1,fv2,fm1,ierr)
!
  use datatypes
  integer(i4) i,j,n,nm,ierr,matz
  real(dp) ar(nm,n),ai(nm,n),w(n),zr(nm,n),zi(nm,n), &
         fv1(n),fv2(n),fm1(2,n)
!
!     this subroutine calls the recommended sequence of
!     subroutines from the eigensystem subroutine package (eispack)
!     to find the eigenvalues and eigenvectors (if desired)
!     of a complex hermitian matrix.
!
!     on input
!
!        nm  must be set to the row dimension of the two-dimensional
!        array parameters as declared in the calling program
!        dimension statement.
!
!        n  is the order of the matrix  a=(ar,ai).
!
!        ar  and  ai  contain the real and imaginary parts,
!        respectively, of the complex hermitian matrix.
!
!        matz  is an integer variable set equal to zero if
!        only eigenvalues are desired.  otherwise it is set to
!        any non-zero integer for both eigenvalues and eigenvectors.
!
!     on output
!
!        w  contains the eigenvalues in ascending order.
!
!        zr  and  zi  contain the real and imaginary parts,
!        respectively, of the eigenvectors if matz is not zero.
!
!        ierr  is an integer output variable set equal to an error
!           completion code described in the documentation for tqlrat
!           and tql2.  the normal completion code is zero.
!
!        fv1, fv2, and  fm1  are temporary storage arrays.
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
  if (n .le. nm) go to 10
  ierr = 10 * n
  go to 50
!
10 call  htridi(nm,n,ar,ai,w,fv1,fv2,fm1)
  if (matz .ne. 0) go to 20
!     .......... find eigenvalues only ..........
  call  tqlrat(n,w,fv2,ierr)
  go to 50
!     .......... find both eigenvalues and eigenvectors ..........
20 do i = 1, n
     do j = 1, n
        zr(j,i) = 0.0_dp
     enddo
     zr(i,i) = 1.0_dp
  enddo
!
  call tql2(nm,n,w,fv1,zr,ierr)
  if (ierr .ne. 0) go to 50
  call  htribk(nm,n,ar,ai,fm1,n,zr,zi)
50 return
  end
  SUBROUTINE TQLRAT(N,D,E2,IERR)
!
  use datatypes
  INTEGER(i4) I,J,L,M,N,II,L1,MML,IERR
  real(dp) D(N),E2(N)
  real(dp) B,C,F,G,H,P,R,S,T,EPSLON,PYTHAG
!
!     This subroutine is a translation of the Algol procedure tqlrat,
!     Algorithm 464, Comm. ACM 16, 689(1973) by Reinsch.
!
!     This subroutine finds the eigenvalues of a symmetric
!     tridiagonal matrix by the rational QL method.
!
!     On input
!
!        N is the order of the matrix.
!
!        D contains the diagonal elements of the input matrix.
!
!        E2 contains the squares of the subdiagonal elements of the
!          input matrix in its last N-1 positions.  E2(1) is arbitrary.
!
!      On output
!
!        D contains the eigenvalues in ascending order.  If an
!          error exit is made, the eigenvalues are correct and
!          ordered for indices 1,2,...IERR-1, but may not be
!          the smallest eigenvalues.
!
!        E2 has been destroyed.
!
!        IERR is set to
!          zero       for normal return,
!          J          if the J-th eigenvalue has not been
!                     determined after 30 iterations.
!
!     Calls PYTHAG for  DSQRT(A*A + B*B) .
!
!     Questions and comments should be directed to Burton S. Garbow,
!     Mathematics and Computer Science Div, Argonne National Laboratory
!
!     This version dated August 1987.
!     Modified by C. Moler to fix underflow/overflow difficulties,
!     especially on the VAX and other machines where epslon(1.0_dp)**2
!     nearly underflows.  See the loop involving statement 102 and
!     the two statements just before statement 200.
!
!     ------------------------------------------------------------------
!
  IERR = 0
  IF (N .EQ. 1) GO TO 1001
!
  DO I = 2, N
    E2(I-1) = E2(I)
  enddo
!
  F = 0.0D0
  T = 0.0D0
  E2(N) = 0.0D0
!
  DO L = 1, N
     J = 0
     H = DABS(D(L)) + DSQRT(E2(L))
     IF (T .GT. H) GO TO 105
     T = H
     B = EPSLON(T)
     C = B * B
     if (c .ne. 0.0_dp) go to 105
!        Spliting tolerance underflowed.  Look for larger value.
     do i = l, n
        h = dabs(d(i)) + sqrt(e2(i))
        if (h .gt. t) t = h
     enddo
     b = epslon(t)
     c = b * b
!     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
105    DO M = L, N
        IF (E2(M) .LE. C) GO TO 120
!     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
!                THROUGH THE BOTTOM OF THE LOOP ..........
     enddo
!
120    IF (M .EQ. L) GO TO 210
130    IF (J .EQ. 30) GO TO 1000
     J = J + 1
!     .......... FORM SHIFT ..........
     L1 = L + 1
     S = DSQRT(E2(L))
     G = D(L)
     P = (D(L1) - G) / (2.0D0 * S)
     R = PYTHAG(P,1.0_dp)
     D(L) = S / (P + DSIGN(R,P))
     H = G - D(L)
!
     DO I = L1, N
       D(I) = D(I) - H
     enddo
!
     F = F + H
!     .......... RATIONAL QL TRANSFORMATION ..........
     G = D(M)
     IF (G .EQ. 0.0D0) G = B
     H = G
     S = 0.0D0
     MML = M - L
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
     DO II = 1, MML
        I = M - II
        P = G * H
        R = P + E2(I)
        E2(I+1) = S * R
        S = E2(I) / R
        D(I+1) = H + S * (H + D(I))
        G = D(I) - E2(I) / G
!           Avoid division by zero on next pass
        if (g .eq. 0.0_dp) g = epslon(d(i))
        h = g * (p / r)
     enddo
!
     E2(L) = S * G
     D(L) = H
!     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
     IF (H .EQ. 0.0D0) GO TO 210
     IF (DABS(E2(L)) .LE. DABS(C/H)) GO TO 210
     E2(L) = H * E2(L)
     IF (E2(L) .NE. 0.0D0) GO TO 130
210    P = D(L) + F
!     .......... ORDER EIGENVALUES ..........
     IF (L .EQ. 1) GO TO 250
!     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
     do II = 2, L
        I = L + 2 - II
        IF (P .GE. D(I-1)) GO TO 270
        D(I) = D(I-1)
     enddo
!
250    I = 1
270    D(I) = P
  enddo
!
  GO TO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
1000 IERR = L
1001 RETURN
  END
  subroutine tql2(nm,n,d,e,z,ierr)
!
  use datatypes
  integer(i4) i,j,k,l,m,n,ii,l1,l2,nm,mml,ierr
  real(dp) d(n),e(n),z(nm,n)
  real(dp) c,c2,c3,dl1,el1,f,g,h,p,r,s,s2,tst1,tst2,pythag
!
!     this subroutine is a translation of the algol procedure tql2,
!     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
!     wilkinson.
!     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
!
!     this subroutine finds the eigenvalues and eigenvectors
!     of a symmetric tridiagonal matrix by the ql method.
!     the eigenvectors of a full symmetric matrix can also
!     be found if  tred2  has been used to reduce this
!     full matrix to tridiagonal form.
!
!     on input
!
!        nm must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        n is the order of the matrix.
!
!        d contains the diagonal elements of the input matrix.
!
!        e contains the subdiagonal elements of the input matrix
!          in its last n-1 positions.  e(1) is arbitrary.
!
!        z contains the transformation matrix produced in the
!          reduction by  tred2, if performed.  if the eigenvectors
!          of the tridiagonal matrix are desired, z must contain
!          the identity matrix.
!
!      on output
!
!        d contains the eigenvalues in ascending order.  if an
!          error exit is made, the eigenvalues are correct but
!          unordered for indices 1,2,...,ierr-1.
!
!        e has been destroyed.
!
!        z contains orthonormal eigenvectors of the symmetric
!          tridiagonal (or full) matrix.  if an error exit is made,
!          z contains the eigenvectors associated with the stored
!          eigenvalues.
!
!        ierr is set to
!          zero       for normal return,
!          j          if the j-th eigenvalue has not been
!                     determined after 30 iterations.
!
!     calls pythag for  sqrt(a*a + b*b) .
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
  ierr = 0
  if (n .eq. 1) go to 1001
!
  do i = 2, n
    e(i-1) = e(i)
  enddo
!
  f = 0.0_dp
  tst1 = 0.0_dp
  e(n) = 0.0_dp
!
  do l = 1, n
     j = 0
     h = dabs(d(l)) + dabs(e(l))
     if (tst1 .lt. h) tst1 = h
!     .......... look for small sub-diagonal element ..........
     do m = l, n
        tst2 = tst1 + dabs(e(m))
        if (tst2 .eq. tst1) go to 120
!     .......... e(n) is always zero, so there is no exit
!                through the bottom of the loop ..........
     enddo
!
120    if (m .eq. l) go to 220
130    if (j .eq. 30) go to 1000
     j = j + 1
!     .......... form shift ..........
     l1 = l + 1
     l2 = l1 + 1
     g = d(l)
     p = (d(l1) - g) / (2.0_dp * e(l))
     r = pythag(p,1.0_dp)
     d(l) = e(l) / (p + dsign(r,p))
     d(l1) = e(l) * (p + dsign(r,p))
     dl1 = d(l1)
     h = g - d(l)
     if (l2 .gt. n) go to 145
!
     do i = l2, n
       d(i) = d(i) - h
     enddo
!
145    f = f + h
!     .......... ql transformation ..........
     p = d(m)
     c = 1.0_dp
     c2 = c
     el1 = e(l1)
     s = 0.0_dp
     mml = m - l
!     .......... for i=m-1 step -1 until l do -- ..........
     do ii = 1, mml
        c3 = c2
        c2 = c
        s2 = s
        i = m - ii
        g = c * e(i)
        h = c * p
        r = pythag(p,e(i))
        e(i+1) = s * r
        s = e(i) / r
        c = p / r
        p = c * d(i) - s * g
        d(i+1) = h + s * (c * g + s * d(i))
!     .......... form vector ..........
        do k = 1, n
           h = z(k,i+1)
           z(k,i+1) = s * z(k,i) + c * h
           z(k,i) = c * z(k,i) - s * h
        enddo
!
     enddo
!
     p = -s * s2 * c3 * el1 * e(l) / dl1
     e(l) = s * p
     d(l) = c * p
     tst2 = tst1 + dabs(e(l))
     if (tst2 .gt. tst1) go to 130
220    d(l) = d(l) + f
  enddo
!     .......... order eigenvalues and eigenvectors ..........
  do 300 ii = 2, n
     i = ii - 1
     k = i
     p = d(i)
!
     do 260 j = ii, n
        if (d(j) .ge. p) go to 260
        k = j
        p = d(j)
260    continue
!
     if (k .eq. i) go to 300
     d(k) = d(i)
     d(i) = p
!
     do j = 1, n
        p = z(j,i)
        z(j,i) = z(j,k)
        z(j,k) = p
     enddo
!
300 continue
!
  go to 1001
!     .......... set error -- no convergence to an
!                eigenvalue after 30 iterations ..........
1000 ierr = l
1001 return
  end
  subroutine htribk(nm,n,ar,ai,tau,m,zr,zi)
!
  use datatypes
  integer(i4) i,j,k,l,m,n,nm
  real(dp) ar(nm,n),ai(nm,n),tau(2,n),zr(nm,m),zi(nm,m)
  real(dp) h,s,si
!
!     this subroutine is a translation of a complex analogue of
!     the algol procedure trbak1, num. math. 11, 181-195(1968)
!     by martin, reinsch, and wilkinson.
!     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
!
!     this subroutine forms the eigenvectors of a complex hermitian
!     matrix by back transforming those of the corresponding
!     real symmetric tridiagonal matrix determined by  htridi.
!
!     on input
!
!        nm must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        n is the order of the matrix.
!
!        ar and ai contain information about the unitary trans-
!          formations used in the reduction by  htridi  in their
!          full lower triangles except for the diagonal of ar.
!
!        tau contains further information about the transformations.
!
!        m is the number of eigenvectors to be back transformed.
!
!        zr contains the eigenvectors to be back transformed
!          in its first m columns.
!
!     on output
!
!        zr and zi contain the real and imaginary parts,
!          respectively, of the transformed eigenvectors
!          in their first m columns.
!
!     note that the last component of each returned vector
!     is real and that vector euclidean norms are preserved.
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
  if (m .eq. 0) go to 200
!     .......... transform the eigenvectors of the real symmetric
!                tridiagonal matrix to those of the hermitian
!                tridiagonal matrix. ..........
  do k = 1, n
     do j = 1, m
        zi(k,j) = -zr(k,j) * tau(2,k)
        zr(k,j) = zr(k,j) * tau(1,k)
     enddo
  enddo
!
  if (n .eq. 1) go to 200
!     .......... recover and apply the householder matrices ..........
  do 140 i = 2, n
     l = i - 1
     h = ai(i,i)
     if (h .eq. 0.0_dp) go to 140
!
     do j = 1, m
        s = 0.0_dp
        si = 0.0_dp
!
        do k = 1, l
           s = s + ar(i,k) * zr(k,j) - ai(i,k) * zi(k,j)
           si = si + ar(i,k) * zi(k,j) + ai(i,k) * zr(k,j)
        enddo
!     .......... double divisions avoid possible underflow ..........
        s = (s / h) / h
        si = (si / h) / h
!
        do k = 1, l
           zr(k,j) = zr(k,j) - s * ar(i,k) - si * ai(i,k)
           zi(k,j) = zi(k,j) - si * ar(i,k) + s * ai(i,k)
        enddo
!
     enddo
!
140 continue
!
200 return
  end
  subroutine htridi(nm,n,ar,ai,d,e,e2,tau)
!
  use datatypes
  integer(i4) i,j,k,l,n,ii,nm,jp1
  real(dp) ar(nm,n),ai(nm,n),d(n),e(n),e2(n),tau(2,n)
  real(dp) f,g,h,fi,gi,hh,si,scale,pythag
!
!     this subroutine is a translation of a complex analogue of
!     the algol procedure tred1, num. math. 11, 181-195(1968)
!     by martin, reinsch, and wilkinson.
!     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
!
!     this subroutine reduces a complex hermitian matrix
!     to a real symmetric tridiagonal matrix using
!     unitary similarity transformations.
!
!     on input
!
!        nm must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        n is the order of the matrix.
!
!        ar and ai contain the real and imaginary parts,
!          respectively, of the complex hermitian input matrix.
!          only the lower triangle of the matrix need be supplied.
!
!     on output
!
!        ar and ai contain information about the unitary trans-
!          formations used in the reduction in their full lower
!          triangles.  their strict upper triangles and the
!          diagonal of ar are unaltered.
!
!        d contains the diagonal elements of the the tridiagonal matrix.
!
!        e contains the subdiagonal elements of the tridiagonal
!          matrix in its last n-1 positions.  e(1) is set to zero.
!
!        e2 contains the squares of the corresponding elements of e.
!          e2 may coincide with e if the squares are not needed.
!
!        tau contains further information about the transformations.
!
!     calls pythag for  sqrt(a*a + b*b) .
!
!     questions and comments should be directed to burton s. garbow,
!     mathematics and computer science div, argonne national laboratory
!
!     this version dated august 1983.
!
!     ------------------------------------------------------------------
!
  tau(1,n) = 1.0_dp
  tau(2,n) = 0.0_dp
!
  do i = 1, n
    d(i) = ar(i,i)
  enddo
!     .......... for i=n step -1 until 1 do -- ..........
  do ii = 1, n
     i = n + 1 - ii
     l = i - 1
     h = 0.0_dp
     scale = 0.0_dp
     if (l .lt. 1) go to 130
!     .......... scale row (algol tol then not needed) ..........
     do k = 1, l
       scale = scale + dabs(ar(i,k)) + dabs(ai(i,k))
     enddo
!
     if (scale .ne. 0.0_dp) go to 140
     tau(1,l) = 1.0_dp
     tau(2,l) = 0.0_dp
130    e(i) = 0.0_dp
     e2(i) = 0.0_dp
     go to 290
!
140    do k = 1, l
        ar(i,k) = ar(i,k) / scale
        ai(i,k) = ai(i,k) / scale
        h = h + ar(i,k) * ar(i,k) + ai(i,k) * ai(i,k)
     enddo
!
     e2(i) = scale * scale * h
     g = sqrt(h)
     e(i) = scale * g
     f = pythag(ar(i,l),ai(i,l))
!     .......... form next diagonal element of matrix t ..........
     if (f .eq. 0.0_dp) go to 160
     tau(1,l) = (ai(i,l) * tau(2,i) - ar(i,l) * tau(1,i)) / f
     si = (ar(i,l) * tau(2,i) + ai(i,l) * tau(1,i)) / f
     h = h + f * g
     g = 1.0_dp + g / f
     ar(i,l) = g * ar(i,l)
     ai(i,l) = g * ai(i,l)
     if (l .eq. 1) go to 270
     go to 170
160    tau(1,l) = -tau(1,i)
     si = tau(2,i)
     ar(i,l) = g
170    f = 0.0_dp
!
     do j = 1, l
        g = 0.0_dp
        gi = 0.0_dp
!     .......... form element of a*u ..........
        do k = 1, j
           g = g + ar(j,k) * ar(i,k) + ai(j,k) * ai(i,k)
           gi = gi - ar(j,k) * ai(i,k) + ai(j,k) * ar(i,k)
        enddo
!
        jp1 = j + 1
        if (l .lt. jp1) go to 220
!
        do k = jp1, l
           g = g + ar(k,j) * ar(i,k) - ai(k,j) * ai(i,k)
           gi = gi - ar(k,j) * ai(i,k) - ai(k,j) * ar(i,k)
        enddo
!     .......... form element of p ..........
220       e(j) = g / h
        tau(2,j) = gi / h
        f = f + e(j) * ar(i,j) - tau(2,j) * ai(i,j)
     enddo
!
     hh = f / (h + h)
!     .......... form reduced a ..........
     do j = 1, l
        f = ar(i,j)
        g = e(j) - hh * f
        e(j) = g
        fi = -ai(i,j)
        gi = tau(2,j) - hh * fi
        tau(2,j) = -gi
!
        do k = 1, j
           ar(j,k) = ar(j,k) - f * e(k) - g * ar(i,k) &
                             + fi * tau(2,k) + gi * ai(i,k)
           ai(j,k) = ai(j,k) - f * tau(2,k) - g * ai(i,k) &
                             - fi * e(k) - gi * ar(i,k)
        enddo
     enddo
!
270    do k = 1, l
        ar(i,k) = scale * ar(i,k)
        ai(i,k) = scale * ai(i,k)
     enddo
!
     tau(2,l) = -si
290    hh = d(i)
     d(i) = ar(i,i)
     ar(i,i) = hh
     ai(i,i) = scale * sqrt(h)
  enddo
!
  return
  end
