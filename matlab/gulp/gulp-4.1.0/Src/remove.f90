  subroutine remove(ni,qreg1,lcreated)
!
!  Remove an atom from region 1 and correct totals
!
!  11/04 ldfix added to list of arrays modified
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
!  Copyright Curtin University 2004
!
!  Julian Gale, Curtin University, November 2004
!
  use current
  use defects
  implicit none
!
!  Passed variables
!
  integer(i4)        :: ni
  logical            :: lcreated(*)
  real(dp)           :: qreg1
!
!  Local variables
!
  integer(i4)        :: i
!
!  Shift all atoms down one in arrays
!
  qreg1 = qreg1 - qdefe(ni)*occdefe(ni)
  do i = ni+1,nreg1
    xdefe(i-1) = xdefe(i)
    ydefe(i-1) = ydefe(i)
    zdefe(i-1) = zdefe(i)
    qdefe(i-1) = qdefe(i)
    radefe(i-1) = radefe(i)
    occdefe(i-1) = occdefe(i)
    inddfix(i-1) = inddfix(i)
    natdefe(i-1) = natdefe(i)
    ntypdefe(i-1) = ntypdefe(i)
    ndefmol(i-1) = ndefmol(i)
    ndefind(i-1) = ndefind(i)
    nreldef(i-1) = nreldef(i)
    lcreated(i-1) = lcreated(i)
    ldefbsmat(i-1) = ldefbsmat(i)
    ldfix(i-1) = ldfix(i)
    ldqmatom(i-1) = ldqmatom(i)
  enddo
  nreg1 = nreg1 - 1
!
  return
  end
