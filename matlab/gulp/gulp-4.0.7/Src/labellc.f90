  subroutine labellc(as,inat)
!
!  Convert atomic number to atomic symbol in lower case
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
!  Copyright Curtin University 2005
!
  use element
  implicit none
!
!  Passed variables
!
  character(len=2), intent(out) :: as
  integer(i4),      intent(in)  :: inat
!
!  Local variables
!
  character(len=1)              :: letter
  integer(i4)                   :: ia
  integer(i4)                   :: ica
  integer(i4)                   :: icz
  integer(i4)                   :: il
  integer(i4)                   :: ishift
!
  ia = ichar('a')
  ica = ichar('A')
  icz = ichar('Z')
  ishift = ica - ia
!
!  Adjust case of symbol if necessary
!
  as = atsym(inat)
  letter = as(1:1)
  il = ichar(letter)
  if (il.ge.ica.and.il.le.icz) as(1:1) = char(il - ishift)
!
  return
  end
