  function istoan(as)
!
!  Convert atomic symbol to atomic number
!
!  10/02 Style updated 
!   7/06 Modified to handle non-characters in second letter 
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
!  Julian Gale, NRI, Curtin University, July 2006
!
  use element
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  character(len=2), intent(inout) :: as
  integer(i4)                     :: istoan
!
!  Local variables
!
  character(len=1)                :: letter
  integer(i4)                     :: ia
  integer(i4)                     :: iatno
  integer(i4)                     :: ica
  integer(i4)                     :: icz
  integer(i4)                     :: il
  integer(i4)                     :: ishift
  integer(i4)                     :: iz
!
  ia  = ichar('a')
  ica = ichar('A')
  iz  = ichar('z')
  icz = ichar('Z')
  ishift = ica - ia
!
!  Adjust case of symbol if necessary
!  First letter upper case
!  Second letter lower case
!
  letter = as(1:1)
  il = ichar(letter)
  if (il.ge.ia.and.il.le.iz) as(1:1) = char(il+ishift)
  letter = as(2:2)
  if (letter.ne.' ') then
    il = ichar(letter)
    if (il.ge.ica.and.il.le.icz) then
      as(2:2) = char(il-ishift)
    elseif (il.lt.ia.or.il.gt.iz) then
      as(2:2) = ' '
    endif
  endif
!
!  Locate symbol
!
  iatno = 1
  do while (as.ne.atsym(iatno).and.iatno.le.maxele)
    iatno = iatno + 1
  enddo
  if (iatno.gt.maxele) then
    if (ioproc) then
      write(ioout,'(/)')
      write(ioout,'(''  **** Unrecognised element symbol '',a2,'' ****'')') as
      write(ioout,'(/)')
    endif
    call stopnow('istoan')
  endif
  istoan = iatno
!
  return
  end
