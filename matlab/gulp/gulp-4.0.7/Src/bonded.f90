  subroutine bonded(lbonded,l2bonds,nbtype,nbtype2,i,j,ii,jj,kk)
!
!  Determines whether two atoms are bonded or share a common
!  bonded atom. It assumes that both atoms are in the same
!  molecule.
!
!  lbonded = if .true. then atoms are bonded
!  l2bonds = if .true. then atoms are bonded to common atom
!  ii,jj,kk = current indices of cell vector shifts
!
!  10/02 Style updated
!   2/07 nbondedtype added & nbtype/nbtype2 to output
!   4/07 Searching for bonded atoms now uses nbonds number rather
!        than checking for a non-zero type
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
  use current
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: i
  integer(i4), intent(in)  :: ii
  integer(i4), intent(in)  :: j
  integer(i4), intent(in)  :: jj
  integer(i4), intent(in)  :: kk
  integer(i4), intent(out) :: nbtype
  integer(i4), intent(out) :: nbtype2
  logical,     intent(out) :: l2bonds
  logical,     intent(out) :: lbonded
!
!  Local variables
!
  integer(i4)              :: icm
  integer(i4)              :: imm
  integer(i4)              :: ind
  integer(i4)              :: indsum
  integer(i4)              :: jcm
  integer(i4)              :: jmm
!
  lbonded = .false.
  l2bonds = .false.
  nbtype  = 0
  nbtype2 = 0
!
  imm = 1
  icm = 1
  ind = (ii + 5) + 10*(jj + 5) + 100*(kk + 5)
  do while (imm.gt.0.and.icm.le.nbonds(i).and..not.lbonded)
    imm = nbonded(icm,i)
    lbonded = (imm.eq.j.and.ind.eq.nbondind(icm,i))
!
!  If bonded then set bond type indicators
!
    if (lbonded) then
      nbtype  = nbondedtype(1,icm,i)
      nbtype2 = nbondedtype(2,icm,i)
    endif
!
    jmm = 1
    jcm = 1
    do while (imm.gt.0.and.jmm.gt.0.and.jcm.le.nbonds(j).and..not.l2bonds)
      jmm = nbonded(jcm,j)
      indsum = nbondind(icm,i) - nbondind(jcm,j) + 555
      l2bonds = (imm.eq.jmm.and.indsum.eq.ind)
      if (indsum.eq.555.and.i.eq.j) l2bonds = .false.
      jcm = jcm + 1
    enddo
    icm = icm + 1
  enddo
!
!  Ensure that only one option is true
!
  if (lbonded) then
    l2bonds = .false.
  endif
!
  return
  end
