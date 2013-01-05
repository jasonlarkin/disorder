  subroutine getdistances(nor,ind,i,cut2,lself,nmolonly)
!
!  Subroutine for finding all distances required for twobody
!  potential within cutoff distance.
!
!   1/05 Created 
!   3/07 Bondtype arrays added
!   2/09 Argument removed from changemaxdis call
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
!  Julian Gale, NRI, Curtin University, February 2009
!
  use current,      only : maxdis
  use distances
  use realvectors
  implicit none
!
!  Passed variables
!
  integer(i4), intent(inout)                   :: nor
  integer(i4), intent(in)                      :: i
  integer(i4), intent(in)                      :: ind
  integer(i4), intent(out)                     :: nmolonly
  logical,     intent(out)                     :: lself
  real(dp),    intent(in)                      :: cut2
!
!  Local variables
!
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: norin
!
!  Check memory
!
  if (nor.gt.maxdis) then
    maxdis = nor
    call changemaxdis
  endif
!
!  Set pointer to j
!
  j = ndistanceptr(ind+1)
!
!  Copy overall data values
!
  nmolonly = ndistancemolonly(j,i)
  lself = distlself(j,i)
!
!  Loop over all distances finding those within cutoff
!
  norin = nor
  nor = 0
  do k = 1,norin
    if (distance2(ind+k).le.cut2.or.k.eq.ndistancemolonly(j,i)) then
      nor = nor + 1
!
!  Copy data to workspace arrays
!
      dist(nor) = distance(ind + k)
      xtmp(nor) = distancexyz(1,ind + k)
      ytmp(nor) = distancexyz(2,ind + k)
      ztmp(nor) = distancexyz(3,ind + k)
      lbonded(nor)  = distl1bond(ind + k)
      l2bonds(nor)  = distl2bond(ind + k)
      l3bonds(nor)  = distl3bond(ind + k)
      lptrmol(nor)  = distlptrmol(ind + k)
      nbotype(nor)  = ndistbotype(ind + k)
      nbotype2(nor) = ndistbotype2(ind + k)
      if (k.eq.ndistancemolonly(j,i)) nmolonly = nor
    endif
  enddo
!
  return
  end
