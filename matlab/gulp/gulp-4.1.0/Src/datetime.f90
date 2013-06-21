  subroutine datetime(imode)
!
!  Outputs date/time
!
!  imode = 1 => label as start time
!  imode = 2 => label as current time
!  imode = 3 => label as finish time
!
!   8/01 Created
!   1/04 Modified to handle 11th/12th/13th special case
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
  use iochannels
  use parallel
  implicit none
!
!  Passed arguments
!
  integer(i4), intent(in) :: imode
!
!  Local variables
!
  integer(i4)             :: nmonth
  real(dp)                :: rmonth
  character(len=60)       :: cdatetime
  character(len=10)       :: ctime
  character(len=9)        :: months(12)
  character(len=8)        :: cdate
  character(len=3)        :: cmonth
  data months/'January','February','March','April','May', &
              'June','July','August','September','October','November','December'/
  if (ioproc) then
!
!  Date stamp run
!
    call date_and_time(cdate,ctime)
!  Initialise strings
    cdatetime = ' '
!  Hours
    cdatetime(1:2) = ctime(1:2)
    cdatetime(3:3) = ':'
!  Minutes
    cdatetime(4:5) = ctime(3:4)
    cdatetime(6:6) = '.'
!  Seconds
    cdatetime(7:8) = ctime(5:6)
!  Day
    cdatetime(10:11) = cdate(7:8)
    cmonth(1:2) = cdate(7:8)
    cmonth(3:3) = '.'
    call wtof(cmonth,rmonth,3_i4,3_i4)
    nmonth = nint(rmonth)
    if (nmonth.lt.10) cdatetime(10:10) = ' '
    nmonth = mod(nmonth,10_i4)
    if (nmonth.eq.1.and.rmonth.ne.11.0_dp) then
      cdatetime(12:13) = 'st'
    elseif (nmonth.eq.2.and.rmonth.ne.12.0_dp) then
      cdatetime(12:13) = 'nd'
    elseif (nmonth.eq.3.and.rmonth.ne.13.0_dp) then
      cdatetime(12:13) = 'rd'
    else
      cdatetime(12:13) = 'th'
    endif
!  Month
    cmonth(1:2) = cdate(5:6)
    cmonth(3:3) = '.'
    call wtof(cmonth,rmonth,3_i4,3_i4)
    nmonth = nint(rmonth)
    cdatetime(15:24) = months(nmonth)
!  Year
    cdatetime(26:29) = cdate(1:4)
!  Write out strings
    if (imode.le.1) then
      write(ioout,'(/,''  Job Started  at '',a60,/)') cdatetime
    elseif (imode.eq.2) then
      write(ioout,'(''  Current Time is '',a60,/)') cdatetime
    else
      write(ioout,'(''  Job Finished at '',a60,/)') cdatetime
    endif
  endif
!
  return
  end
