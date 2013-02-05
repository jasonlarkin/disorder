  subroutine initmaxreaxFFspecdefaults(i)
!
!  Initialises the arrays associated with maxreaxFFspec
!
!   9/10 Created from changemax routine
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, September 2010
!
  use library
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
  integer(i4)             :: j
  integer(i4)             :: maxreaxFFspec2
  integer(i4)             :: oldmaxreaxFFspec
  integer(i4)             :: oldmaxreaxFFspec2 
!
!  Some things depend on pairs of species
!
  maxreaxFFspec2 = maxreaxFFspec*(maxreaxFFspec + 1)/2
  oldmaxreaxFFspec = i - 1
  oldmaxreaxFFspec2 = oldmaxreaxFFspec*(oldmaxreaxFFspec + 1)/2
!
!  Initialise new parts of data arrays
!
  if (i.ge.1.and.i.le.maxreaxFFspec) then
    nreaxFFval3(1:maxreaxFFspec2,i) = 0
    reaxFFr(1:3,i) = 0.0_dp
    reaxFFalpha(i) = 0.0_dp
    reaxFFeps(i) = 0.0_dp
    reaxFFrvdw(i) = 0.0_dp
    reaxFFgammaw(i) = 0.0_dp
    reaxFFpover(i) = 0.0_dp
    reaxFFpunder(i) = 0.0_dp
    reaxFFhincrement(i) = 0.0_dp
    reaxFFlp(1:3,i) = 0.0_dp
    reaxFFoc1(i) = 0.0_dp
    reaxFFuc1(i) = 0.0_dp
    reaxFFpboc(1:3,i) = 0.0_dp
    reaxFFval(1:4,i) = 0.0_dp
    reaxFFval1(1:2,i) = 0.0_dp
    reaxFFval3(1:6,1:maxreaxFFval3,1:maxreaxFFspec2,i) = 0.0_dp
    reaxFFconj3(1:4,1:maxreaxFFspec2,i) = 0.0_dp
    reaxFFhb3(1:4,1:maxreaxFFspec,1:maxreaxFFspec,i) = 0.0_dp
    reaxFFpen3(1:maxreaxFFspec2,i) = 0.0_dp
!
    do j = 1,oldmaxreaxFFspec
      nreaxFFval3(oldmaxreaxFFspec2+1:maxreaxFFspec2,j) = 0
      reaxFFval3(1:6,1:maxreaxFFval3,oldmaxreaxFFspec2+1:maxreaxFFspec2,j) = 0.0_dp
      reaxFFconj3(1:4,oldmaxreaxFFspec2+1:maxreaxFFspec2,j) = 0.0_dp
      reaxFFhb3(1:4,oldmaxreaxFFspec+1:maxreaxFFspec,oldmaxreaxFFspec+1:maxreaxFFspec,j) = 0.0_dp
      reaxFFpen3(oldmaxreaxFFspec2+1:maxreaxFFspec2,j) = 0.0_dp
    enddo
    do j = oldmaxreaxFFspec2+1,maxreaxFFspec2
      lreaxFFbocorrect(1:2,j) = .false.
      lreaxFFmorseinput(j) = .false.
      lreaxFFpboOK(j) = .false.
      lreaxFFtorsinput(1:maxreaxFFspec2+1,j) = .false.
      reaxFFmorse(1:3,j) = 0.0_dp
      reaxFFmorse(4:6,j) = -1.0_dp
      reaxFFDe(1:3,j) = 0.0_dp
      reaxFFpbe(1:2,j) = 0.0_dp
      reaxFFpbo(1:6,j) = 0.0_dp
      reaxFFpen2(1:3,j) = 0.0_dp
      reaxFFoc2(j) = 0.0_dp
      reaxFFtor4(1:5,1:maxreaxFFspec2+1,j) = 0.0_dp
    enddo
    do j = 1,oldmaxreaxFFspec2
      lreaxFFtorsinput(oldmaxreaxFFspec2+2:maxreaxFFspec2+1,j) = .false.
      reaxFFtor4(1:5,oldmaxreaxFFspec2+2:maxreaxFFspec2+1,j) = 0.0_dp
    enddo
  endif
!
  return
  end
