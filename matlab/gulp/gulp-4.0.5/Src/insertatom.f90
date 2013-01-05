  subroutine insertatom(ninsert)
!
!  Makes space in the configuration arrays so that an atom can be inserted
!
!  On entry :
!
!  ninsert = pointer to position for insertion
!
!  12/10 Created from reorder
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
!  Julian Gale, NRI, Curtin University, December 2010
!
  use configurations
  use current
  use moldyn,  only : lfix
  use scan,    only : ltranat
  implicit none
!
!  Passed variables
!
  integer(i4),        intent(in)  :: ninsert
!
!  Local variables
!
  integer(i4)                     :: i
!
!  If insertion position is after the end of the current atoms then there is nothing to do
!
  if (ninsert.gt.nasum) return
!
!  Check that arrays are large enough to allow move of data
!
  if (nasum+1.gt.maxatot) then
    maxatot = maxatot + 1
    call changemaxatot
  endif
!
!  Loop over atoms that are above the insertion position to move them by one place
!
  do i = nasum,ninsert,-1
    natcfg(i+1) = natcfg(i)
    nregionno(i+1) = nregionno(i)
    ntypcfg(i+1) = ntypcfg(i)
    nspecptrcfg(i+1) = nspecptrcfg(i)
    cncfg(i+1) = cncfg(i)
    xcfg(i+1) = xcfg(i)
    ycfg(i+1) = ycfg(i)
    zcfg(i+1) = zcfg(i)
    qlcfg(i+1) = qlcfg(i)
    occucfg(i+1) = occucfg(i)
    oxcfg(i+1) = oxcfg(i)
    radcfg(i+1) = radcfg(i)
    lbsmat(i+1) = lbsmat(i)
    lqmatom(i+1) = lqmatom(i)
    lsliceatom(i+1) = lsliceatom(i)
    ltranat(i+1) = ltranat(i)
    lfix(i+1) = lfix(i)
    forcecfg(1:3,i+1) = forcecfg(1:3,i)
    tdforcecfg(1:3,1:3,i+1) = tdforcecfg(1:3,1:3,i)
    ltdforcecfg(1:3,i+1) = ltdforcecfg(1:3,i)
  enddo
!
!  Loop over coordinates of atoms that are above the insertion position to move them by one place
!
  do i = 3*nasum,3*ninsert-2,-1
    lopfi(i+1) = lopfi(i)
  enddo
!
  return
  end
