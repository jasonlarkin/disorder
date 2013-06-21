  subroutine setbondorder
!
!  Set-up bond order potentials determined by combination rules
!
!  12/03 Created from dumpbo
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
!  Copyright Curtin University 2003
!
!  Julian Gale, Curtin University, December 2003
!
  use bondorderdata
  use constants
  use control
  use current
  use element
  implicit none
!
!  Local variables
!
  integer                                      :: i
  integer                                      :: j
  integer                                      :: n1
  integer                                      :: n2
  integer                                      :: nat1
  integer                                      :: nat2
  integer                                      :: ntype1
  integer                                      :: ntype2
  logical                                      :: lfound1
  logical                                      :: lfound2
!
  do i = 1,nbopot
    if (BOcombi(i)) then
      nat1 = nBOspec1(i)
      ntype1 = nBOtyp1(i)
      nat2 = nBOspec2(i)
      ntype2 = nBOtyp2(i)
!
!  Find self-self element potentials
!
      j = 0
      lfound1 = .false.
      lfound2 = .false.
      do while ((.not.lfound1.or..not.lfound2).and.j.lt.nbopot)
        j = j + 1
!
!  Exclude combination potentials
!
        if (.not.BOcombi(j)) then
          if (.not.lfound1) then
            lfound1 = (nat1.eq.nBOspec1(j).and.(ntype1.eq.nBOtyp1(j).or.nBOtyp1(j).eq.0))
            if (lfound1) n1 = j
          endif
          if (.not.lfound2) then
            lfound2 = (nat2.eq.nBOspec2(j).and.(ntype2.eq.nBOtyp2(j).or.nBOtyp2(j).eq.0))
            if (lfound2) n2 = j
          endif
        endif
      enddo
!
!  Check that two potentials have been found
!
      if (.not.lfound1.or..not.lfound2) then
        call outerror('element self potential missing for bond order combination rule',0_i4)
        call stopnow('setbondorder')
      endif
!
!  Use combinaton rules to generate parameters
!
      BOacoeff(i) = BOchiR(i)*sqrt(BOacoeff(n1)*BOacoeff(n2))
      BObcoeff(i) = BOchiA(i)*sqrt(BObcoeff(n1)*BObcoeff(n2))
      BOzacoeff(i) = 0.5_dp*(BOzacoeff(n1) + BOzacoeff(n2))
      BOzbcoeff(i) = 0.5_dp*(BOzbcoeff(n1) + BOzbcoeff(n2))
      rBOmin(i) = sqrt(rBOmin(n1)*rBOmin(n2))
      rBOmax(i) = sqrt(rBOmax(n1)*rBOmax(n2))
    endif
  enddo
!
  return
  end
