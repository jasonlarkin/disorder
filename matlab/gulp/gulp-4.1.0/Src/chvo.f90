  subroutine chvo(nm,n,ar,ai,w,fv1,fv2,fm1,ierr)
!
  use datatypes
  implicit none
  integer(i4) :: n,nm,ierr
  real(dp)    :: ar(nm,n),ai(nm,n),w(n), &
                 fv1(n),fv2(n),fm1(2,n)
!
!  This routine has been modified from "ch.f", part of the eispack suite
!  in order to avoid the need to store the eigenvectors due to space
!  restrictions. It calls the appropriate eispack routines to find the
!  eigenvalues of a complex Hermitian matrix.
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
!     on output
!
!        w  contains the eigenvalues in ascending order.
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
  return
!
10 call  htridi(nm,n,ar,ai,w,fv1,fv2,fm1)
!
!  Find eigenvalues only
!
  call  tqlrat(n,w,fv2,ierr)
  return
  end
