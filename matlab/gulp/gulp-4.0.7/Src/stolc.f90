  subroutine stolc(word,nlengthword)
!
!  Convert upper to lower case
!
!  12/07 Argument giving string length added to subroutine
!  12/07 Unused variables removed
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
!  Copyright Julian Gale, NRI, Curtin University, December 2007
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4),                intent(in)    :: nlengthword
  character(len=nlengthword), intent(inout) :: word
!
!  Local variables
!
  character(len=1)  :: letter
  integer(i4)       :: i
  integer(i4)       :: ia
  integer(i4)       :: ica
  integer(i4)       :: icz
  integer(i4)       :: il
  integer(i4)       :: ishift
!
  ia = ichar('a')
  ica = ichar('A')
  icz = ichar('Z')
  ishift = ica - ia
!
!  Adjust case if necessary
!
  do i = 1,nlengthword
    letter = word(i:i)
    il = ichar(letter)
    if (il.ge.ica.and.il.le.icz) word(i:i) = char(il-ishift)
  enddo
!
  return
  end
