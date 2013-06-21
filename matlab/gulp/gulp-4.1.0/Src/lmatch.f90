  function lmatch(nat1,ntype1,nat2,ntype2,lwildcard)
!
!  Compares atomic numbers and types to see if the atoms are a
!  match for each other. Option of wildcard allowed. Note that
!  the order of 1 vs 2 is important!
!
!  nat1      = atomic number of atom
!  nat2      = atomic number of potential type
!  ntype1    = type number of atom
!  ntype2    = type number of potential type
!  lwildcard = if .true. then allow wildcards for potential
!              (mod(nat2,maxele).eq.0 => wildcard atomic no.)
!              (ntype2.eq.0 => wildcard type number)
!
!   1/07 lwildcard flag implemented within routine as opposed
!        to being passed in
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
!  Julian Gale, NRI, Curtin University, January 2007 
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
  logical,     intent(in)  :: lwildcard
  logical                  :: lmatch
!
!  Local variables
!
  if (nat1.gt.maxele) then
    lmatch = ((nat1.eq.nat2.or.(lwildcard.and.nat2.eq.2*maxele)).and.(ntype1.eq.ntype2.or.ntype2.eq.0))
  else
    lmatch = ((nat1.eq.nat2.or.(lwildcard.and.nat2.eq.maxele)).and.(ntype1.eq.ntype2.or.ntype2.eq.0))
  endif
!
  return
  end
!
  function lexactmatch(nat1,ntype1,nat2,ntype2)
!
!  Compares atomic numbers and types to see if the atoms are a
!  match for each other. 
!
!  nat1      = atomic number of atom
!  nat2      = atomic number of potential type
!  ntype1    = type number of atom
!  ntype2    = type number of potential type
!
!  11/08 Created from lmatch as a stricter version
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
!  Copyright Curtin University 2008
!     
!  Julian Gale, NRI, Curtin University, November 2008
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
  logical                  :: lexactmatch
!
!  Local variables
!
  lexactmatch = ((nat1.eq.nat2).and.(ntype1.eq.ntype2))
!
  return
  end
