  subroutine ltont(string,inat,itype)
!
!  Convert Label TO atomic Number and Type
!
!  string = contains label
!  inat   = atomic number on exit
!  itype  = type number on exit
!
!  10/02 Style updated to new one
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, December 2007
!
  use datatypes
  implicit none
!
!  Passed variables
!
  character(len=*), intent(in)  :: string
  integer(i4),      intent(out) :: inat
  integer(i4),      intent(out) :: itype
!
!  Local variables
!
  character(len=1)              :: c
  character(len=1)              :: space
  character(len=2)              :: sym
  integer(i4)                   :: i
  integer(i4)                   :: i0
  integer(i4)                   :: i9
  integer(i4)                   :: ic
  integer(i4)                   :: istoan
  integer(i4)                   :: iten
  integer(i4)                   :: nbegin
  integer(i4)                   :: nend
  logical                       :: lnumber
!
  space = ' '
  i0 = ichar('0')
  i9 = ichar('9')
!**************************
!  Extract atomic symbol  *
!**************************
  sym(1:1) = string(1:1)
!
!  Check whether element symbol is one or two characters
!
  c = string(2:2)
  ic = ichar(c)
  itype = 0
  if (ic.ge.i0.and.ic.le.i9) then
    nbegin = 2
    sym(2:2) = space
  elseif (c.eq.space) then
    nbegin = 0
    sym(2:2) = space
  else
    nbegin = 3
    sym(2:2) = string(2:2)
  endif
!
!  Generate atomic number
!
  inat = istoan(sym)
!
!  If no type is given then return
!
  if (nbegin.eq.0) return
  if (nbegin.eq.3.and.string(3:3).eq.space) return
!**********************
!  Extract atom type  *
!**********************
!
!  Find beginning and end of numeric string
!
  i = nbegin
  lnumber = .true.
  do while (lnumber.and.i.lt.20)
    i = i + 1
    c = string(i:i)
    ic = ichar(c)
    if (ic.lt.i0.or.ic.gt.i9) lnumber = .false.
  enddo
  nend = i - 1
!
!  Convert string to integer number
!
  itype = 0
  iten = 1
  do i = nend,nbegin,-1
    c = string(i:i)
    ic = ichar(c)
    itype = itype + (ic-i0)*iten
    iten = 10*iten
  enddo
!
  return
  end
