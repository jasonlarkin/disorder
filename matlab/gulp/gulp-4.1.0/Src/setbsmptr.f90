  subroutine setbsmptr(nbs,nbss,nbsptr,lregion1only)
!
!  Sets up pointers to breathing shell model sites
!
!  nbs    = number of breathing core+shell model sites
!  nbss   = number of breathing shell model sites
!  nbsptr = pointer to breathing shell model sites
!  lregion1only = if true, only handle region 1 atoms
!
!   8/97 Created from part of phonon.F
!   7/02 Region 1 only handling added
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use configurations, only : lbsmat, nregionno
  use current
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  integer(i4) :: nbs
  integer(i4) :: nbss
  integer(i4) :: nbsptr(*)
  logical     :: lregion1only
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: nri
!*************************************
!  Generate breathing shell pointer  *
!*************************************
  nbs = 0
  nbss = 0
  ii = 0
  do i = 1,numat
    if (.not.lregion1only.or.nregionno(nsft+nrelat(i)).eq.1) then
      ii = ii + 1
      nri = nrelat(i)
      if (lbsmat(nsft+nri)) then
        nbs = nbs + 1
        if (iatn(nri).gt.maxele) nbss = nbss + 1
        nbsptr(nbs) = ii
      endif
    endif
  enddo
!
  return
  end
