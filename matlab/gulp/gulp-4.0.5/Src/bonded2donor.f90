  function bonded2donor(i)
!
!  Determines whether an atom is bonded to a Dreiding donor
!  atom. Specifically, these are N, O, F, S, Cl, Br, I
!
!   5/07 Created based on bonded
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
!  Julian Gale, NRI, Curtin University, May 2007
!
  use current
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: i
  logical                  :: bonded2donor
!
!  Local variables
!
  integer(i4)              :: icm
  integer(i4)              :: j
  integer(i4)              :: natj
!
  bonded2donor = .false.
!
  j = 1
  icm = 1
  do while (j.gt.0.and.icm.le.nbonds(i).and..not.bonded2donor)
    j = nbonded(icm,i)
    natj = nat(j)
!
!  Check whether atom is N, O, F, S, Cl, Br, or I
!
    if (natj.eq.7.or.natj.eq.8.or.natj.eq.9.or.natj.eq.16.or.natj.eq.17.or.natj.eq.35.or.natj.eq.53) &
      bonded2donor = .true.
    icm = icm + 1
  enddo
!
  return
  end

  function bonded2donorJK(i,j,k)
!
!  Determines whether an atom is bonded to a Dreiding donor
!  atom. Specifically, these are N, O, F, S, Cl, Br, I.
!  This version also checks that the atom to which it is 
!  bonded is also one of j & k.
!
!   5/07 Created based on bonded
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
!  Julian Gale, NRI, Curtin University, May 2007
!
  use current
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: i
  integer(i4), intent(in)  :: j
  integer(i4), intent(in)  :: k
  logical                  :: bonded2donorJK
!
!  Local variables
!
  integer(i4)              :: icm
  integer(i4)              :: l
  integer(i4)              :: natl
!
  bonded2donorJK = .false.
!
  l = 1
  icm = 1
  do while (l.gt.0.and.icm.le.nbonds(i).and..not.bonded2donorJK)
    l = nbonded(icm,i)
    if (l.eq.j.or.l.eq.k) then
      natl = nat(l)
!
!  Check whether atom is N, O, F, S, Cl, Br, or I
!
      if (natl.eq.7.or.natl.eq.8.or.natl.eq.9.or.natl.eq.16.or.natl.eq.17.or.natl.eq.35.or.natl.eq.53) &
        bonded2donorJK = .true.
    endif
    icm = icm + 1
  enddo
!
  return
  end
