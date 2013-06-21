  subroutine changemaxeamfnspec
!
!  Alters the size of the arrays associated with maxeamfnspec
!
!   5/06 Created from changemaxeamspec
!   2/07 Size of eamfnpar increased in first dimension to 16
!  11/07 Unused variables cleaned up
!  12/08 eamfnmeamcoeff and neamfnmeamorder arrays added
!   4/09 neamfnmeamtype array added
!   4/09 neamfnmeamcombotype array added
!   7/09 EAM species label array added
!   9/09 Left-hand dimension of eamfnpar increased to 33
!   9/10 Initialisations now performed in a subroutine
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, September 2010
!
  use eam
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxeamfnspec = 0
!
!  EAM data
!
  call realloc_ch5(symboleamfnspec,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','symboleamfnspec')
  call realloc_ch80(eamfnfile,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnfile')
  call realloc(eamfnnumeric,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric')
  call realloc(eamfnnumeric1,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric1')
  call realloc(eamfnnumeric2,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric2')
  call realloc(eamfnnumeric3,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric3')
  call realloc(eamfnnumeric4,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric4')
  call realloc(eamfnnumeric5,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric5')
  call realloc(eamfnnumeric6,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric6')
  call realloc(eamfnnumeric7,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric7')
  call realloc(eamfnnumeric8,maxneamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumeric8')
  call realloc(eamfnnumericdrho,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnnumericdrho')
  call realloc(eamfnpar,33_i4,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnpar')
  call realloc(eamfnmeamcoeff,maxmeamorder,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','eamfnmeamcoeff')
  call realloc(neamfnmeamcombotype,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','neamfnmeamcombotype')
  call realloc(neamfnmeamtype,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','neamfnmeamtype')
  call realloc(neamfnnumeric,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','neamfnnumeric')
  call realloc(neamfnnat,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','neamfnnat')
  call realloc(neamfntyp,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','neamfntyp')
  call realloc(neamfnmeamorder,maxeamfnspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamfnspec','neamfnmeamorder')
!
!  Initialise defaults for new part of array
!
  if (maxeamfnspec.gt.oldmaxeamfnspec) then
    do i = oldmaxeamfnspec+1,maxeamfnspec
      call initmaxeamfnspecdefaults(i)
    enddo
  endif
!
!  Save current value of maxeamfnspec for next call
!
  oldmaxeamfnspec = maxeamfnspec
!
  return
  end
