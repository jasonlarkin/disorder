  subroutine wcoord(iout,lab1,lab2,x,y,z,q,occ,rad,lslice,ifx,ify,ifz,tchar,lflags,rv,ndim)
!
!  Routine to write out coordinate line - called from dumpdur
!  Needed to handle the possibility of recurring decimals
!  which are better represented as fractions in either the
!  coordinates or the occupancies.
!
!  Calls only ftow
!
!   4/97 Created 
!  11/01 Slice indicator added
!   5/02 rv now passed and option to output in Cartesian added
!   4/04 Dimensionality now added as an argument
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
!  Copyright Curtin University 2004
!
!  Julian Gale, Curtin University, November 2004
!
  use datatypes
  use dump,     only : ldumpcart
  implicit none
!
!  Passed variables
!
  character(len=1)  :: tchar
  character(len=4)  :: lab2
  character(len=5)  :: lab1
  integer(i4)       :: ifx
  integer(i4)       :: ify
  integer(i4)       :: ifz
  integer(i4)       :: iout
  integer(i4)       :: ndim
  logical           :: lflags
  logical           :: lslice
  real(dp)          :: occ
  real(dp)          :: q
  real(dp)          :: rad
  real(dp)          :: rv(3,3)
  real(dp)          :: x
  real(dp)          :: y
  real(dp)          :: z
!
!  Local variables
!
  character(len=10) :: cnumber
  character(len=80) :: line
  integer(i4)       :: i
  real(dp)          :: xloc
  real(dp)          :: yloc
  real(dp)          :: zloc
!
!  Handle coordinate transformation to Cartesian
!
  if (ldumpcart.and.ndim.gt.0) then
    if (ndim.eq.3) then
      xloc = x*rv(1,1) + y*rv(1,2) + z*rv(1,3)
      yloc = x*rv(2,1) + y*rv(2,2) + z*rv(2,3)
      zloc = x*rv(3,1) + y*rv(3,2) + z*rv(3,3)
    elseif (ndim.eq.2) then
      xloc = x*rv(1,1) + y*rv(1,2)
      yloc = x*rv(2,1) + y*rv(2,2)
      zloc = z
    elseif (ndim.eq.1) then
      xloc = x*rv(1,1)
      yloc = y
      zloc = z
    endif
  else
    xloc = x
    yloc = y
    zloc = z
  endif
!
!  Blank string
!
  do i = 1,80
    line(i:i) = ' '
  enddo
!
!  Write easy bits to string
!
  line(1:5) = lab1
  line(7:10) = lab2
  if (lflags) then
    if (ifx.eq.1) then
      line(69:69) = '1'
    else
      line(69:69) = '0'
    endif
    if (ify.eq.1) then
      line(71:71) = '1'
    else
      line(71:71) = '0'
    endif
    if (ifz.eq.1) then
      line(73:73) = '1'
    else
      line(73:73) = '0'
    endif
    line(75:75) = tchar
  else
    line(69:69) = tchar
  endif
  if (lslice) then
    line(77:77) = '%'
  endif
!
!  Coordinates
!
!  x = 12-20
!  y = 22-30
!  z = 32-40
!
  call ftow(cnumber,xloc,9_i4)
  line(12:20) = cnumber(1:9)
  call ftow(cnumber,yloc,9_i4)
  line(22:30) = cnumber(1:9)
  call ftow(cnumber,zloc,9_i4)
  line(32:40) = cnumber(1:9)
!
!  Charge (42-51)
!
  call ftow(cnumber,q,10_i4)
  line(42:51) = cnumber(1:10)
!
!  Occupancy (53-59)
!
  call ftow(cnumber,occ,7_i4)
  line(53:59) = cnumber(1:7)
!
!  Radius (61-67)
!
  call ftow(cnumber,rad,7_i4)
  line(61:67) = cnumber(1:7)
!
!  Line is now complete so write it out!
!
  write(iout,'(a80)') line
!
  return
  end
