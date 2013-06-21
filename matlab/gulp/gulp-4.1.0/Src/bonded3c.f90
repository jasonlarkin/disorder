  subroutine bonded3c(lbonded,l2bonds,l3bonds,nbtype,nbtype2,ii,jj,kk)
!
!  Determines whether two atoms are bonded or share a common
!  bonded atom. It assumes that both atoms are in the same
!  molecule. Uses prebuilt list to assign values.
!
!  lbonded = if .true. then atoms are directly connected
!  l2bonds = if .true. then atoms are connected via 2 bonds
!  l3bonds = if .true. then atoms are connected via 3 bonds
!  ii,jj,kk = current indices of cell vector shifts
!
!   2/07 nbondedtype added & nbtype/nbtype2 to output
!   1/08 Unused arguments removed
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
!  Julian Gale, NRI, Curtin University, January 2008
!
  use bondvectors
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: ii
  integer(i4), intent(in)  :: jj
  integer(i4), intent(in)  :: kk
  integer(i4), intent(out) :: nbtype
  integer(i4), intent(out) :: nbtype2
  logical,     intent(out) :: lbonded
  logical,     intent(out) :: l2bonds
  logical,     intent(out) :: l3bonds
!
!  Local variables
!
  integer(i4)              :: ind
  integer(i4)              :: n
  logical                  :: found
!
  lbonded = .false.
  l2bonds = .false.
  l3bonds = .false.
  nbtype  = 0
  nbtype2 = 0
!
!  Check on cell indices exceeding the maximum
!
  if (abs(ii).ge.5.or.abs(jj).ge.5.or.abs(kk).ge.5) return
!
!  Calculate index
!
  ind = (ii+5) + 10*(jj+5) + 100*(kk+5)
!
!  Search through list looking for a valid assignment
!
  found = .false.
  n = 0
  do while (.not.found.and.n.lt.nbondvec) 
    n = n + 1
    found = (ind.eq.nbondvecind(n))
  enddo
!
!  If found assign flags
!
  if (found) then
    lbonded = lbondedvec(n)
    l2bonds = l2bondsvec(n)
    l3bonds = l3bondsvec(n)
    nbtype  = nbtypevec(n)
    nbtype2 = nbtype2vec(n)
  endif
!
  return
  end
