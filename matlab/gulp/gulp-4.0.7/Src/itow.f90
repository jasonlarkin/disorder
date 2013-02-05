  subroutine itow(string,num,nlen)
!
!  Converts an integer number to a string representation. 
!
!  nlen = length of string - must be at least 4 in
!         very general case
!  ndp  = position of decimal point
!
!  11/06 Created from ftow
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, November 2006
!
  use datatypes
  implicit none
!
!  Passed variables
!
  character(len=*), intent(out) :: string
  integer(i4),      intent(in)  :: nlen
  integer(i4),      intent(in)  :: num
!
!  Local variables
!
  character(len=1)           :: cn(10)
  integer(i4)                :: i
  integer(i4)                :: ii
  integer(i4)                :: minus
  integer(i4)                :: num2
  integer(i4)                :: npower
  integer(i4)                :: nstart
  integer(i4)                :: ten
  logical                    :: lfound
!
  data cn/'0','1','2','3','4','5','6','7','8','9'/
!
  nstart = 0
!
!  Blank string
!
  do i = 1,nlen
    string(i:i) = ' '
  enddo
!
!  If number is negative start with sign
!
  if (num.lt.0_i4) then
    nstart = 1
    string(1:1) = '-'
  endif
  num2 = abs(num)
!
!  Find maximum power of ten
!
  npower = 0
  lfound = .false.
  ten = 1
  do while (.not.lfound)
    npower = npower + 1
    ten = 10*ten
    lfound = (num2.lt.ten)
  enddo
  npower = npower - 1
  if (npower.ge.(nlen-nstart)) then
    call outerror('number too large in itow',0_i4)
    call stopnow('itow')
  endif
  ten = 10**npower
  do i = 1,npower + 1
    ii = (num2/ten)
    minus = ii*ten
    num2 = num2 - minus
    ten = ten/10
    string(nstart+1:nstart+1) = cn(ii+1)
    nstart = nstart + 1
  enddo
!
  return
  end
