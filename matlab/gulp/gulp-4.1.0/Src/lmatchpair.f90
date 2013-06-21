  function lmatchpair(nat1,ntype1,nat2,ntype2,nspecpair,nat1pair,ntype1pair,nat2pair,ntype2pair)
!
!  Finds whether a pair of species matches any pair that are in a list
!
!  nat1                  = atomic number of first species to be matched
!  ntype1                = type number of first species to be matched
!  nat2                  = atomic number of second species to be matched
!  ntype2                = type number of second species to be matched
!  nspecpair             = number of species pairs to compare against
!  nat1pair(nspecpair)   = array of atomic numbers to compare against
!  ntype1pair(nspecpair) = array of type numbers to compare against
!  nat2pair(nspecpair)   = array of atomic numbers to compare against
!  ntype2pair(nspecpair) = array of type numbers to compare against
!
!   4/07 Created from lmatchany
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
!  Copyright Curtin University 2007
!     
!  Julian Gale, NRI, Curtin University, April 2007 
!
  use datatypes
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: nat1
  integer(i4), intent(in)  :: nat2
  integer(i4), intent(in)  :: ntype1
  integer(i4), intent(in)  :: ntype2
  integer(i4), intent(in)  :: nspecpair
  integer(i4), intent(in)  :: nat1pair(nspecpair)
  integer(i4), intent(in)  :: nat2pair(nspecpair)
  integer(i4), intent(in)  :: ntype1pair(nspecpair)
  integer(i4), intent(in)  :: ntype2pair(nspecpair)
  logical                  :: lmatchpair
!
!  Local variables
!
  integer(i4)              :: npair
  logical                  :: lmatchpair1
  logical                  :: lmatchpair2
!
  lmatchpair = .false.
  npair = 0
!
!  Loop over species to compare against
!
  do while (.not.lmatchpair.and.npair.lt.nspecpair)
    npair = npair + 1
    lmatchpair1 = ((nat1.eq.nat1pair(npair).and.(ntype1.eq.ntype1pair(npair).or.ntype1pair(npair).eq.0)).and. &
                   (nat2.eq.nat2pair(npair).and.(ntype2.eq.ntype2pair(npair).or.ntype2pair(npair).eq.0)))
    lmatchpair2 = ((nat2.eq.nat1pair(npair).and.(ntype2.eq.ntype1pair(npair).or.ntype1pair(npair).eq.0)).and. &
                   (nat1.eq.nat2pair(npair).and.(ntype1.eq.ntype2pair(npair).or.ntype2pair(npair).eq.0)))
    lmatchpair = (lmatchpair1.or.lmatchpair2)
  enddo
!
  return
  end
