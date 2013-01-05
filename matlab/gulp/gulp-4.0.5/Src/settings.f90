  subroutine settings(hmsg,gronam,hbr,nspcg)
!
!  Checks whether symbol is alternative setting of a
!  monoclinic or orthorhombic group.
!
!  nspcg   = space group number
!
!   1/11 More extensive list of alternative settings added
!        and alternative name arrays moved to symmetry module.
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, January 2011
!
  use datatypes
  use symmetry,  only : naltgnam, altgnam
  implicit none
!
!  Passed variables
!
  character(len=16), intent(in)  :: gronam(231)
  character(len=1),  intent(in)  :: hbr(8)
  character(len=1),  intent(in)  :: hmsg(16)
  integer(i4),       intent(out) :: nspcg
!
!  Local variables
!
  character(len=16)              :: grpsymb
  character(len=16)              :: symbol_in
  integer(i4)                    :: i
  integer(i4)                    :: j
  logical                        :: lfound
!
  do i = 1,16
    symbol_in(i:i) = hmsg(i)
  enddo
!
!  Now check against those for valid groups
!
  lfound = .false.
  i = 0
  nspcg = 0
!
!  Loop over space groups below number 74 which is where alternative settings are found
!
  do while (i.lt.74.and..not.lfound)
    i = i + 1
!
!  Loop over different settings of each space group
!
    j = 0
    do while (j.lt.naltgnam(i).and..not.lfound)
      j = j + 1
      grpsymb = altgnam(j,i)
      lfound = (symbol_in.eq.grpsymb)
    enddo
  enddo
  if (lfound) then
    nspcg = i
  endif
!
  return
  end
