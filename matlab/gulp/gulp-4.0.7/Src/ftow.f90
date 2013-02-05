  subroutine ftow(string,rnum,nlen)
!
!  Converts a floating point number to a string
!  representation. If number is a multiple of
!  1/6 then write out as a fraction.
!
!  NEED TO ADD HANDLING OF LARGE AND SMALL NUMBERS
!
!  nlen = length of string - must be at least 4 in
!         very general case
!  ndp  = position of decimal point
!
!   4/97 Created from wtof (the reverse!)
!   9/01 Handling of floating point numbers improved to allow
!        for rounding errors
!  10/01 Handling of numbers close to 1.0 improved
!   4/02 Tolerance for rounding altered to 1 x 10**-8
!   3/12 Keyword added to suppress printing of fractions
!  10/12 Handing of rounding errors changed to avoid
!        out of bounds error for ~9.9999999
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, October 2012
!
  use datatypes
  use control,      only : keyword
  implicit none
!
!  Passed variables
!
  character(len=*)           :: string
  integer(i4)                :: nlen
  real(dp)                   :: rnum
!
!  Local variables
!
  character(len=1)           :: cn(10)
  integer(i4)                :: i
  integer(i4)                :: ii
  integer(i4)                :: npower
  integer(i4)                :: nsixth
  integer(i4)                :: nstart
  logical                    :: lfraction
  logical                    :: lfound
  real(dp)                   :: diff
  real(dp)                   :: rminus
  real(dp)                   :: rnum2
  real(dp)                   :: rsixth
  real(dp)                   :: rten
  real(dp)                   :: sixth
!
  data cn/'0','1','2','3','4','5','6','7','8','9'/
!
  sixth = 1.0_dp/6.0_dp
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
  if (rnum.lt.0.0_dp) then
    nstart = 1
    string(1:1) = '-'
  endif
!
!  Is number a fraction?
!
  lfraction = .false.
  rnum2 = abs(rnum)
  if (rnum2.lt.1.0_dp.and.rnum2.ne.0.5_dp) then
    nsixth = 0
    rsixth = 0.0_dp
    do while (nsixth.lt.6.and..not.lfraction) 
      nsixth = nsixth + 1
      rsixth = rsixth + sixth
      diff = abs(rnum) - rsixth
      if (abs(diff).lt.1.0d-5) lfraction = .true.
    enddo
    if (nsixth.eq.3) lfraction = .false.
    if (nsixth.eq.6) lfraction = .false.
  endif
!
!  Look for keyword that suppresses fraction printing
!
  if (index(keyword,'deci').eq.1.or.index(keyword,' deci').ne.0) lfraction = .false.
!
  if (lfraction) then
!*************
!  Fraction  *
!*************
    if (mod(nsixth,2_i4).eq.0) then
!
!  Even no. of sixths => thirds
!
      nsixth = nsixth/2
      string(nstart+1:nstart+1) = cn(nsixth+1)
      string(nstart+2:nstart+3) = '/3'
    else
!
!  Odd no. of sixths
!
      string(nstart+1:nstart+1) = cn(nsixth+1)
      string(nstart+2:nstart+3) = '/6'
    endif
  else
!*******************
!  Decimal number  *
!*******************
!
!  Handle number that is just smaller than 1 by rounding error
!
    if (abs(1.0_dp-rnum2).le.1.0d-8) then
      rnum2 = 1.0_dp
    endif
!
!  Handle number that is just smaller than 1/2 by rounding error
!
    if (abs(0.5_dp-rnum2).le.1.0d-8) then
      rnum2 = 0.5_dp
    endif
!
!  In front of decimal place
!
    if (rnum2.lt.1.0_dp) then
      string(nstart+1:nstart+2) = '0.'
      nstart = nstart + 2
    else
!
!  Find maximum power of ten
!
      npower = 0
      lfound = .false.
      rten = 1.0_dp
      do while (.not.lfound)
        npower = npower + 1
        rten = rten*10.0_dp
        lfound = (rnum2.lt.rten)
!
!  Check that we are not the victim of rounding error
!
        if (abs((rnum2-rten)).lt.1.0d-6) then
          lfound = .false.
        endif
      enddo
      npower = npower - 1
      if (npower.ge.(nlen-nstart)) then
        call outerror('number too large in ftow',0_i4)
        call stopnow('ftow')
      endif
      rten = 10.0_dp**npower
      do i = 1,npower + 1
        ii = (rnum2/rten)
        rminus = dble(ii)*rten
        rnum2 = rnum2 - rminus
        rten = rten*0.1_dp
        if (nstart.ne.0.or.ii.ne.0) then
!
!  Trap printing of zero at the front of a number
!
          string(nstart+1:nstart+1) = cn(ii+1)
          nstart = nstart + 1
        endif
      enddo
!
!  Is there space for decimal place?
!
      if (nstart.lt.nlen) then
        string(nstart+1:nstart+1) = '.'
        nstart = nstart + 1
      endif
    endif
!
!  After decimal place - if room
!
    if (nstart.lt.nlen) then
      rten = 10.0_dp**(nlen-nstart)
      rnum2 = rnum2*rten
      do i = nstart+1,nlen
        rten = rten*0.1_dp
        ii = (rnum2/rten)
        rminus = dble(ii)*rten
        rnum2 = rnum2 - rminus
        string(i:i) = cn(ii+1)
      enddo
    endif
  endif
!
  return
  end
