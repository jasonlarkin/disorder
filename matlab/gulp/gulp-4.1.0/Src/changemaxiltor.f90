  subroutine changemaxiltor
!
!  Alters the size of the arrays associated with maxiltor
!
!   9/06 Created from changemaxtor
!   4/07 Extra variables added
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
!  Julian Gale, NRI, Curtin University, April 2007
!
  use four
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
!  I/L torsional data
!
  call realloc(iltor,2_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','iltor')
  call realloc(ilftor,2_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','ilftor')
  call realloc(ilxtor,2_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','ilxtor')
  call realloc(neqiltor,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','neqiltor')
  call realloc(nfortor,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','nfortor')
  call realloc(lopiltor,2_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','lopiltor')
  call realloc(liltorswitch,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','liltorswitch')
  call realloc(ljktorswitch,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','ljktorswitch')
  call realloc(lsurfiltor,3_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','lsurfiltor')
  call realloc(oiltor,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','oiltor')
  call realloc(riltor,5_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','riltor')
  call realloc(xiltor,5_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','xiltor')
  call realloc(yiltor,5_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','yiltor')
  call realloc(ziltor,5_i4,maxiltor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxiltor','ziltor')
!
  return
  end
