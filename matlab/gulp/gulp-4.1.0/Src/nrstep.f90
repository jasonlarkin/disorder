  subroutine nrstep(pvect,hessian,gc,nvar,nmin)
!
!  Generates the Newton-Raphson step / search direction from the
!  hessian in lower-half triangular form and the gradient vector.
!
!  10/04 Rewritten to use blas call
!   1/09 Integer datatypes all explicitly declared
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
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: nmin
  integer(i4), intent(in)  :: nvar
  real(dp),    intent(in)  :: gc(*)
  real(dp),    intent(in)  :: hessian(*)
  real(dp),    intent(out) :: pvect(*)
!
!  Local variables
!
  integer(i4)              :: nlvar
!
!  Set local number of variables
!
  nlvar = nvar - nmin + 1
!
!  Call blas routine for matrix-vector multiply
!
  call dspmv('U',nlvar,1.0_dp,hessian,gc(nmin),1_i4,0.0_dp,pvect(nmin),1_i4)
!
  return
  end
