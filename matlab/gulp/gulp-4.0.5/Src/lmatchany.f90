  function lmatchany(nat1,ntype1,nspecany,natany,ntypeany)
!
!  Finds whether a species matches any that are in a list
!
!  nat1               = atomic number of species to be matched
!  ntype1             = type number of species to be matched
!  nspecany           = number of species to compare against
!  natany(nspecany)   = array of atomic numbers to compare against
!  ntypeany(nspecany) = array of type numbers to compare against
!
!   4/07 Created from lmatch
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
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: nat1
  integer(i4), intent(in)  :: nspecany
  integer(i4), intent(in)  :: natany(nspecany)
  integer(i4), intent(in)  :: ntype1
  integer(i4), intent(in)  :: ntypeany(nspecany)
  logical                  :: lmatchany
!
!  Local variables
!
  integer(i4)              :: nany
!
  lmatchany = .false.
  nany = 0
!
!  Loop over species to compare against
!
  do while (.not.lmatchany.and.nany.lt.nspecany)
    nany = nany + 1
    lmatchany = (nat1.eq.natany(nany).and.(ntype1.eq.ntypeany(nany).or.ntypeany(nany).eq.0))
  enddo
!
  return
  end
!
  function lmatchanyof3(nat,ntype,ntp1,nat1,ntype1,tr1,tr1min,ntp2,nat2,ntype2,tr2,tr2min,ntp3,nat3,ntype3,tr3,tr3min)
!
!  Compares atomic numbers and types to see if the atoms are a
!  match for each other. Option of wildcard allowed. 
!  If a match is found then the matched species is returned as
!  no. 1, and then the remainder are returned as 2 & 3.
!
!  nat       = atomic number
!  ntype     = type number
!
!  Test species for comparison
!
!  ntp1      = pointer number to atom 1
!  ntp2      = pointer number to atom 2
!  ntp3      = pointer number to atom 3
!  nat1      = atomic number of atom 1
!  nat2      = atomic number of atom 2
!  nat3      = atomic number of atom 3
!  ntype1    = type number of atom 1
!  ntype2    = type number of atom 2
!  ntype3    = type number of atom 3
!  tr1       = maximum cutoff for atom 1
!  tr2       = maximum cutoff for atom 2
!  tr3       = maximum cutoff for atom 3
!  tr1min    = minimum cutoff for atom 1
!  tr2min    = minimum cutoff for atom 2
!  tr3min    = minimum cutoff for atom 3
!
!   7/08 Created from lmatch
!   7/08 Pointers to atom numbers added
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
!  Julian Gale, NRI, Curtin University, July 2008
!
  use datatypes
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)     :: nat
  integer(i4), intent(in)     :: ntype
  integer(i4), intent(inout)  :: nat1
  integer(i4), intent(inout)  :: nat2
  integer(i4), intent(inout)  :: nat3
  integer(i4), intent(inout)  :: ntp1
  integer(i4), intent(inout)  :: ntp2
  integer(i4), intent(inout)  :: ntp3
  integer(i4), intent(inout)  :: ntype1
  integer(i4), intent(inout)  :: ntype2
  integer(i4), intent(inout)  :: ntype3
  logical                     :: lmatchanyof3
  real(dp),    intent(inout)  :: tr1
  real(dp),    intent(inout)  :: tr2
  real(dp),    intent(inout)  :: tr3
  real(dp),    intent(inout)  :: tr1min
  real(dp),    intent(inout)  :: tr2min
  real(dp),    intent(inout)  :: tr3min
!
!  Local variables
!
  integer(i4)                 :: ntmp
  logical                     :: lmatch1
  logical                     :: lmatch1full
  logical                     :: lmatch1wild
  logical                     :: lmatch2
  logical                     :: lmatch2full
  logical                     :: lmatch2wild
  logical                     :: lmatch3
  logical                     :: lmatch3full
  logical                     :: lmatch3wild
  real(dp)                    :: rtmp
!
!  Find out which symbols match
!
  if (nat.gt.maxele) then
    lmatch1     = (nat.eq.nat1.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch1full = (nat.eq.nat1.and.ntype.eq.ntype1)
    lmatch1wild = (nat1.eq.2*maxele.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch2     = (nat.eq.nat2.and.(ntype.eq.ntype2.or.ntype2.eq.0))
    lmatch2full = (nat.eq.nat2.and.ntype.eq.ntype2)
    lmatch2wild = (nat2.eq.2*maxele.and.(ntype.eq.ntype2.or.ntype2.eq.0))
    lmatch3     = (nat.eq.nat3.and.(ntype.eq.ntype3.or.ntype3.eq.0))
    lmatch3full = (nat.eq.nat3.and.ntype.eq.ntype3)
    lmatch3wild = (nat3.eq.2*maxele.and.(ntype.eq.ntype3.or.ntype3.eq.0))
  else
    lmatch1     = (nat.eq.nat1.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch1full = (nat.eq.nat1.and.ntype.eq.ntype1)
    lmatch1wild = (nat1.eq.maxele.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch2     = (nat.eq.nat2.and.(ntype.eq.ntype2.or.ntype2.eq.0))
    lmatch2full = (nat.eq.nat2.and.ntype.eq.ntype2)
    lmatch2wild = (nat2.eq.maxele.and.(ntype.eq.ntype2.or.ntype2.eq.0))
    lmatch3     = (nat.eq.nat3.and.(ntype.eq.ntype3.or.ntype3.eq.0))
    lmatch3full = (nat.eq.nat3.and.ntype.eq.ntype3)
    lmatch3wild = (nat3.eq.maxele.and.(ntype.eq.ntype3.or.ntype3.eq.0))
  endif
!
!  Is there any match at all?
!
  lmatchanyof3 = (lmatch1.or.lmatch1wild.or.lmatch2.or.lmatch2wild.or.lmatch3.or.lmatch3wild)
!
!  If there is a match adjust the return values of the species according to the priority 
!  full match > match for type = 0 > wildcard match
!  NB: The matched species is always returned as species 1
!
  if (lmatchanyof3) then
    if (lmatch1full) then
      return
    elseif (lmatch2full) then
      ntmp = ntp2
      ntp2 = ntp1
      ntp1 = ntmp
      ntmp = nat2
      nat2 = nat1
      nat1 = ntmp
      ntmp = ntype2
      ntype2 = ntype1
      ntype1 = ntmp
      rtmp = tr2
      tr2 = tr1
      tr1 = rtmp
      rtmp   = tr2min
      tr2min = tr1min
      tr1min = rtmp
    elseif (lmatch3full) then
      ntmp = ntp3
      ntp3 = ntp1
      ntp1 = ntmp
      ntmp = nat3
      nat3 = nat1
      nat1 = ntmp
      ntmp = ntype3
      ntype3 = ntype1
      ntype1 = ntmp
      rtmp = tr3
      tr3 = tr1
      tr1 = rtmp
      rtmp   = tr3min
      tr3min = tr1min
      tr1min = rtmp
    elseif (lmatch1) then
      return
    elseif (lmatch2) then
      ntmp = ntp2
      ntp2 = ntp1
      ntp1 = ntmp
      ntmp = nat2
      nat2 = nat1
      nat1 = ntmp
      ntmp = ntype2
      ntype2 = ntype1
      ntype1 = ntmp
      rtmp = tr2
      tr2 = tr1
      tr1 = rtmp
      rtmp   = tr2min
      tr2min = tr1min
      tr1min = rtmp
    elseif (lmatch3) then
      ntmp = ntp3
      ntp3 = ntp1
      ntp1 = ntmp
      ntmp = nat3
      nat3 = nat1
      nat1 = ntmp
      ntmp = ntype3
      ntype3 = ntype1
      ntype1 = ntmp
      rtmp = tr3
      tr3 = tr1
      tr1 = rtmp
      rtmp   = tr3min
      tr3min = tr1min
      tr1min = rtmp
    elseif (lmatch1wild) then
      return
    elseif (lmatch2wild) then
      ntmp = ntp2
      ntp2 = ntp1
      ntp1 = ntmp
      ntmp = nat2
      nat2 = nat1
      nat1 = ntmp
      ntmp = ntype2
      ntype2 = ntype1
      ntype1 = ntmp
      rtmp = tr2
      tr2 = tr1
      tr1 = rtmp
      rtmp   = tr2min
      tr2min = tr1min
      tr1min = rtmp
    elseif (lmatch3wild) then
      ntmp = ntp3
      ntp3 = ntp1
      ntp1 = ntmp
      ntmp = nat3
      nat3 = nat1
      nat1 = ntmp
      ntmp = ntype3
      ntype3 = ntype1
      ntype1 = ntmp
      rtmp = tr3
      tr3 = tr1
      tr1 = rtmp
      rtmp   = tr3min
      tr3min = tr1min
      tr1min = rtmp
    endif
  endif
!
  return
  end
!
  function lmatchanyof2(nat,ntype,ntp1,nat1,ntype1,tr1,tr1min,ntp2,nat2,ntype2,tr2,tr2min)
!
!  Compares atomic numbers and types to see if the atoms are a
!  match for each other. Option of wildcard allowed. 
!  If a match is found then the matched species is returned as
!  no. 1, and then the remainder is returned as 2.
!
!  nat       = atomic number
!  ntype     = type number
!
!  Test species for comparison
!
!  ntp1      = pointer number to atom 1
!  ntp2      = pointer number to atom 2
!  nat1      = atomic number of atom 1
!  nat2      = atomic number of atom 2
!  ntype1    = type number of atom 1
!  ntype2    = type number of atom 2
!  tr1       = maximum cutoff for atom 1
!  tr2       = maximum cutoff for atom 2
!  tr1min    = minimum cutoff for atom 1
!  tr2min    = minimum cutoff for atom 2
!
!   7/08 Created from lmatch
!   7/08 Pointers to atom numbers added
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
!  Julian Gale, NRI, Curtin University, July 2008
!
  use datatypes
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)     :: nat
  integer(i4), intent(in)     :: ntype
  integer(i4), intent(inout)  :: nat1
  integer(i4), intent(inout)  :: nat2
  integer(i4), intent(inout)  :: ntp1
  integer(i4), intent(inout)  :: ntp2
  integer(i4), intent(inout)  :: ntype1
  integer(i4), intent(inout)  :: ntype2
  logical                     :: lmatchanyof2
  real(dp),    intent(inout)  :: tr1
  real(dp),    intent(inout)  :: tr2
  real(dp),    intent(inout)  :: tr1min
  real(dp),    intent(inout)  :: tr2min
!
!  Local variables
!
  integer(i4)                 :: ntmp
  logical                     :: lmatch1
  logical                     :: lmatch1full
  logical                     :: lmatch1wild
  logical                     :: lmatch2
  logical                     :: lmatch2full
  logical                     :: lmatch2wild
  real(dp)                    :: rtmp
!
!  Find out which symbols match
!
  if (nat.gt.maxele) then
    lmatch1     = (nat.eq.nat1.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch1full = (nat.eq.nat1.and.ntype.eq.ntype1)
    lmatch1wild = (nat1.eq.2*maxele.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch2     = (nat.eq.nat2.and.(ntype.eq.ntype2.or.ntype2.eq.0))
    lmatch2full = (nat.eq.nat2.and.ntype.eq.ntype2)
    lmatch2wild = (nat2.eq.2*maxele.and.(ntype.eq.ntype2.or.ntype2.eq.0))
  else
    lmatch1     = (nat.eq.nat1.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch1full = (nat.eq.nat1.and.ntype.eq.ntype1)
    lmatch1wild = (nat1.eq.maxele.and.(ntype.eq.ntype1.or.ntype1.eq.0))
    lmatch2     = (nat.eq.nat2.and.(ntype.eq.ntype2.or.ntype2.eq.0))
    lmatch2full = (nat.eq.nat2.and.ntype.eq.ntype2)
    lmatch2wild = (nat2.eq.maxele.and.(ntype.eq.ntype2.or.ntype2.eq.0))
  endif
!
!  Is there any match at all?
!
  lmatchanyof2 = (lmatch1.or.lmatch1wild.or.lmatch2.or.lmatch2wild)
!
!  If there is a match adjust the return values of the species according to the priority 
!  full match > match for type = 0 > wildcard match
!  NB: The matched species is always returned as species 1
!
  if (lmatchanyof2) then
    if (lmatch1full) then
      return
    elseif (lmatch2full) then
      ntmp = ntp2
      ntp2 = ntp1
      ntp1 = ntmp
      ntmp = nat2
      nat2 = nat1
      nat1 = ntmp
      ntmp = ntype2
      ntype2 = ntype1
      ntype1 = ntmp
      rtmp = tr2
      tr2 = tr1
      tr1 = rtmp
      rtmp   = tr2min
      tr2min = tr1min
      tr1min = rtmp
    elseif (lmatch1) then
      return
    elseif (lmatch2) then
      ntmp = ntp2
      ntp2 = ntp1
      ntp1 = ntmp
      ntmp = nat2
      nat2 = nat1
      nat1 = ntmp
      ntmp = ntype2
      ntype2 = ntype1
      ntype1 = ntmp
      rtmp = tr2
      tr2 = tr1
      tr1 = rtmp
      rtmp   = tr2min
      tr2min = tr1min
      tr1min = rtmp
    elseif (lmatch1wild) then
      return
    elseif (lmatch2wild) then
      ntmp = ntp2
      ntp2 = ntp1
      ntp1 = ntmp
      ntmp = nat2
      nat2 = nat1
      nat1 = ntmp
      ntmp = ntype2
      ntype2 = ntype1
      ntype1 = ntmp
      rtmp = tr2
      tr2 = tr1
      tr1 = rtmp
      rtmp   = tr2min
      tr2min = tr1min
      tr1min = rtmp
    endif
  endif
!
  return
  end
