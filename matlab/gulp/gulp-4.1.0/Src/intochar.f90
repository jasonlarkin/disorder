  subroutine intochar(string,inum)
!
!  Convert an integer number to a character string
!
!  string = contains string on output
!  inum   = contains integer number on entry
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use datatypes
  implicit none
!
!  Passed variables
!
  character(len=5)  :: string
  integer(i4)       :: inum
!
!  Local variables
!
  character(len=1)  :: c(10)
  integer(i4)       :: i
  integer(i4)       :: ifct
  integer(i4)       :: in
  integer(i4)       :: ind
  logical           :: lnonzero
!
  data c/'0','1','2','3','4','5','6','7','8','9'/
!
  lnonzero = .false.
  do i = 1,5
    string(i:i) = ' '
  enddo
  ind = inum
  ifct = 10000
  do i = 1,5
    in = ind/ifct
    if (in.gt.0.or.lnonzero) then
      string(6-i:6-i) = c(in+1)
    endif
    ind = ind - ifct*in
    ifct = ifct/10
  enddo
!
  return
  end
