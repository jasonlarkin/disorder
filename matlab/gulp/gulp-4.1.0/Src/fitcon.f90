  subroutine fitcon(xc)
!
!  Apply constraints to fitted parameters
!
!   6/03 Style updated 
!   9/05 Power added
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, September 2005
!
  use fitting
  implicit none
!
!  Passed variables
!
  real(dp)    :: xc(*)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: nv1
  integer(i4) :: nv2
  real(dp)    :: tmp
!
  if (nfcon.gt.0) then
    do i = 1,nfcon
      if (nfcotyp(i).eq.1) then
        xc(nfcfix(i)) = (xc(nfcvar(i))**fconpower(i))*fconco(i) + fconadd(i)
      else
        nv1 = nfcvar(i)
        nv2 = nint(fconadd(i))
        if (nv1.eq.nv2) then
          tmp = xc(nv1)
        else
          tmp = xc(nv1)*xc(nv2)
        endif
        xc(nfcfix(i)) = fconco(i)*sqrt(abs(tmp))
      endif
    enddo
  endif
!
  return
  end
