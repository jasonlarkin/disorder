  subroutine sixstrterms(ndim,rprod,svec)
!
!  Subroutine for calculating six-body strain terms
!
!  On entry :
!
!   ndim              = number of dimensions
!   svec(3,6,6)       = Cartesian components of interatomic vectors
!
!  On exit :
!
!   rprod             = strain terms
!  
!  11/04 Created from fourstrterms
!   6/06 Adaption finished
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, June 2006
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4) :: ndim
  real(dp)    :: rprod(6,15)
  real(dp)    :: svec(3,6,6)
!
!  Local variables
!
  integer(i4) :: indvec
  integer(i4) :: ivec
  integer(i4) :: jvec
!
  if (ndim.eq.3) then
!
!  Set up strain products for 3-D case
!
    indvec = 0
    do ivec = 1,5
      do jvec = ivec+1,6
        indvec = indvec + 1
        rprod(1,indvec) = svec(1,jvec,ivec)*svec(1,jvec,ivec)
        rprod(2,indvec) = svec(2,jvec,ivec)*svec(2,jvec,ivec)
        rprod(3,indvec) = svec(3,jvec,ivec)*svec(3,jvec,ivec)
        rprod(4,indvec) = svec(2,jvec,ivec)*svec(3,jvec,ivec)
        rprod(5,indvec) = svec(1,jvec,ivec)*svec(3,jvec,ivec)
        rprod(6,indvec) = svec(1,jvec,ivec)*svec(2,jvec,ivec)
      enddo
    enddo
  elseif (ndim.eq.2) then
!
!  Set up strain products for 2-D case
!
    indvec = 0
    do ivec = 1,5
      do jvec = ivec+1,6
        indvec = indvec + 1
        rprod(1,indvec) = svec(1,jvec,ivec)*svec(1,jvec,ivec)
        rprod(2,indvec) = svec(2,jvec,ivec)*svec(2,jvec,ivec)
        rprod(3,indvec) = svec(1,jvec,ivec)*svec(2,jvec,ivec)
      enddo
    enddo
  elseif (ndim.eq.1) then
!
!  Set up strain products for 1-D case
!
    indvec = 0
    do ivec = 1,5
      do jvec = ivec+1,6
        indvec = indvec + 1
        rprod(1,indvec) = svec(1,jvec,ivec)*svec(1,jvec,ivec)
      enddo
    enddo
  endif
!
  return
  end
