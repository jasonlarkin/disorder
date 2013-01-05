  subroutine buck4(np)
!
!  Sets up the four range buckingham potential
!
!  np            = potential number
!  twopot(1,)    = buckingham a
!  twopot(2,)    = buckingham rho
!  twopot(3,)    = buckingham c
!  twopot(4,)    = position of minimum
!  tpot(1,)      = first boundary between potentials
!  tpot(2,)      = second boundary between potentials
!  tpot(3,)-(8,) = fifth order polynomial coefficients
!  tpot(9,)-(12,)= third order polynomial coefficients
!  rmat          = temporary for coefficients, stored as (coeff.no.,condition)
!
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, January 2009
!
  use control
  use iochannels
  use parallel
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: np
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: info
  integer(i4)             :: j
  integer(i4)             :: ipvt(10)
  real(dp)                :: a
  real(dp)                :: b
  real(dp)                :: c
  real(dp)                :: ra
  real(dp)                :: rb
  real(dp)                :: rm
  real(dp)                :: rmat(10,10)
  real(dp)                :: trm
  real(dp)                :: trm1
  real(dp)                :: v(10)
!
!  Local variables
!
  ra = tpot(1,np)
  rm = twopot(4,np)
  rb = tpot(2,np)
!
!  Check ranges are in correct order
!
  if (ra.gt.rm.or.rm.gt.rb) then
    call outerror('Ranges for 4-range Buckingham are inconsistent',0_i4)
    if (ioproc) then
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
      write(ioout,'(''!! Cutoff 1 = '',f6.3,'' :  Minimum = '',f6.3,'' : Cutoff 2 = '',f6.3)')ra,rm,rb
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
    endif
    call stopnow('buck4')
  endif
  a = twopot(1,np)
  b = 1.0_dp/twopot(2,np)
  c = twopot(3,np)
  do i = 1,10
    do j = 1,10
      rmat(j,i) = 0.0_dp
    enddo
  enddo
!********************************************************************
!  Set up coefficent matrix using 9 boundary conditions on the      *
!  function and its first and second derivatives + the requirement  *
!  that rm is the minimum.                                          *
!********************************************************************
!
!  Polynomial 1 at boundary 1 (ra) - 3 conditions
!
  trm = a*exp(-ra*b)
  v(1) = trm
  trm = b*trm
  v(2) = - trm
  v(3) = b*trm
  rmat(1,1) = 1.0_dp
  rmat(2,2) = 1.0_dp
  rmat(3,3) = 2.0_dp
  rmat(2,1) = ra
  rmat(3,2) = 2.0_dp*ra
  rmat(4,3) = 6.0_dp*ra
  trm = ra*ra
  rmat(3,1) = trm
  rmat(4,2) = 3.0_dp*trm
  rmat(5,3) = 12.0_dp*trm
  trm = trm*ra
  rmat(4,1) = trm
  rmat(5,2) = 4.0_dp*trm
  rmat(6,3) = 20.0_dp*trm
  trm = trm*ra
  rmat(5,1) = trm
  rmat(6,2) = 5.0_dp*trm
  trm = trm*ra
  rmat(6,1) = trm
!
!  Polynomial 1 at minimum and equal to polynomial 2 (rm) - 4 conditions
!
  v(4) = 0.0_dp
  v(5) = 0.0_dp
  v(6) = 0.0_dp
  v(7) = 0.0_dp
  rmat(1,4) = 1.0_dp
  rmat(2,5) = 1.0_dp
  rmat(3,6) = 2.0_dp
  rmat(7,4) = - 1.0_dp
  rmat(8,5) = - 1.0_dp
  rmat(9,6) = - 2.0_dp
  rmat(8,7) = 1.0_dp
  rmat(2,4) = rm
  rmat(3,5) = 2.0_dp*rm
  rmat(4,6) = 6.0_dp*rm
  rmat(8,4) = - rm
  rmat(9,5) = - 2.0_dp*rm
  rmat(10,6) = - 6.0_dp*rm
  rmat(9,7) = 2.0_dp*rm
  trm = rm*rm
  rmat(3,4) = trm
  rmat(4,5) = 3.0_dp*trm
  rmat(5,6) = 12.0_dp*trm
  rmat(9,4) = - trm
  rmat(10,5) = - 3.0_dp*trm
  rmat(10,7) = 3.0_dp*trm
  trm = trm*rm
  rmat(4,4) = trm
  rmat(5,5) = 4.0_dp*trm
  rmat(6,6) = 20.0_dp*trm
  rmat(10,4) = - trm
  trm = trm*rm
  rmat(5,4) = trm
  rmat(6,5) = 5.0_dp*trm
  trm = trm*rm
  rmat(6,4) = trm
!
!  Polynomial 2 at boundary 2 (rb) - 3 conditions
!
  trm1 = 1.0_dp/rb
  trm = trm1**6
  trm = c*trm
  v(8) = - trm
  trm = trm*trm1
  v(9) = 6.0_dp*trm
  trm = trm*trm1
  v(10) = - 42.0_dp*trm
  rmat(7,8) = 1.0_dp
  rmat(8,9) = 1.0_dp
  rmat(9,10) = 2.0_dp
  rmat(8,8) = rb
  rmat(9,9) = 2.0_dp*rb
  rmat(10,10) = 6.0_dp*rb
  trm = rb*rb
  rmat(9,8) = trm
  rmat(10,9) = 3.0_dp*trm
  trm = rb*trm
  rmat(10,8) = trm
!***********************
!  Solve eigenproblem  *
!***********************
  call dgefa(rmat,10_i4,10_i4,ipvt,info)
  if (info.gt.0) then
    call outerror('Polynomial solution for buck4 is singular',0_i4)
    call stopnow('buck4')
  endif
  call dgesl(rmat,10_i4,10_i4,ipvt,v,1_i4)
!*******************************************
!  Assign coefficients to relevant arrays  *
!*******************************************
  do i = 1,10
    tpot(2+i,np) = v(i)
  enddo
!
!  Option to output polynomial coefficients
!
  if (ioproc.and.index(keyword,'spli').ne.0) then
    write(ioout,'(/)')
    write(ioout,'(''  Potential '',i3,'' : Four Range Buckingham'')') np
    write(ioout,'(/,''  Fifth Order Polynomial : '',/)')
    write(ioout,'(6f10.4)') (tpot(2+i,np),i = 1,6)
    write(ioout,'(/,''  Third Order Polynomial : '',/)')
    write(ioout,'(6f10.4)') (tpot(8+i,np),i = 1,4)
  endif
!
  return
  end
