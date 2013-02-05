  subroutine seteam
!
!  Set up EAM species pointers for each atom in structure
!
!   7/05 created
!   3/09 Setting of neamfnspecptr added
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
!  Julian Gale, NRI, Curtin University, March 2009
!
  use current
  use eam
  use element, only : maxele
  implicit none
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: j
  integer(i4)      :: natp
  integer(i4)      :: ntypp
!
!  Initialise EAM species pointers to zero
!
  neamfnspecptr(1:numat) = 0
  neamspecptr(1:numat) = 0
!
!  Loop over EAM species assigning to atoms
!
  do i = 1,neamspec
    natp = neamnat(i)
    ntypp = neamtyp(i)
    do j = 1,numat
      if (nat(j).eq.natp) then
        if (ntypp.eq.0.or.ntypp.eq.nftype(j)) then
          neamspecptr(j) = i
        endif
      endif
    enddo
  enddo
!
  do i = 1,neamfnspec
    natp = neamfnnat(i)
    ntypp = neamfntyp(i)
    do j = 1,numat
      if (nat(j).eq.natp) then
        if (ntypp.eq.0.or.ntypp.eq.nftype(j)) then
          neamfnspecptr(j) = i
        endif
      endif
    enddo
  enddo
!
  return
  end
