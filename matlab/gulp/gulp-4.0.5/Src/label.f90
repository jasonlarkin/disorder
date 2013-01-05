  subroutine label(inatin,itype,string)
!
!  Generate atom label from atomic number and type number
!
!  inat   = atomic number
!  itype  = type number
!  string = contains label on exit
!
!  12/07 Local variable added so that itype is unchanged on return
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
  use element
  implicit none
!
!  Passed variables
!
  character(len=*), intent(out)   :: string
  integer(i4),      intent(in)    :: inatin
  integer(i4),      intent(inout) :: itype
!
!  Local variables
!
  character(len=1)                :: numstr(10)
  character(len=1)                :: space
  character(len=2)                :: sym
  integer(i4)                     :: ihundreds
  integer(i4)                     :: inat
  integer(i4)                     :: itens
  integer(i4)                     :: itypeloc
  integer(i4)                     :: iunits
  integer(i4)                     :: nptr
!
  data numstr/'0','1','2','3','4','5','6','7','8','9'/
  space = ' '
  string = ' '
  inat = inatin
  if (inat.gt.maxele) inat = inat - maxele
!
!  Check how many characters are in the symbol so that the number
!  is correctly placed into the string
!
  sym = atsym(inat)
  string(1:1) = sym(1:1)
  if (sym(2:2).eq.space) then
    nptr = 2
  else
    nptr = 3
    string(2:2) = sym(2:2)
  endif
  if (itype.eq.0) return
!
!  Insert number
!
  ihundreds = (itype/100)
  itypeloc = itype - ihundreds*100
  itens = (itypeloc/10)
  iunits = itypeloc - 10*itens
  if (ihundreds.gt.0) then
    string(nptr:nptr) = numstr(ihundreds+1)
    nptr = nptr + 1
  endif
  if (itens.gt.0.or.ihundreds.gt.0) then
    string(nptr:nptr) = numstr(itens+1)
    nptr = nptr + 1
  endif
  string(nptr:nptr) = numstr(iunits+1)
!
  return
  end
