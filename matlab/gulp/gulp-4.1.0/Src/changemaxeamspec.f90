  subroutine changemaxeamspec
!
!  Alters the size of the arrays associated with maxeamspec
!
!   7/05 Default value for eamfnpar(1,) changed to one
!  10/05 New arrays for numerical EAM functional added
!  11/05 Taper arrays added
!   3/06 Modified to allow for density component number
!   4/06 Species specific density added
!   5/06 Some arrays moved to separate changemaxeamfnspec routine
!   2/07 Size of denpar increased in first dimension to 15
!   2/08 Option to read EAM density from a file added
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 lmeamspec added
!   7/09 EAM species label array added
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
  integer(i4), save :: oldmaxeamspec = 0
!
!  EAM data
!
  call realloc_ch5(symboleamspec,2_i4,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','symboleamspec')
  call realloc_ch80(eamdenfile,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','eamdenfile')
  call realloc(denpar,16_i4,maxmeamorder,maxeamden,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','denpar')
  call realloc(ndenfn,maxeamden,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','ndenfn')
  call realloc(ndenfncomp,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','ndenfncomp')
  call realloc(neammeamorder,maxeamden,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','neammeamorder')
  call realloc(eamalloy,2_i4,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','eamalloy')
  call realloc(eamtaperdrho,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','eamtaperdrho')
  call realloc(eamtaperrho,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','eamtaperrho')
  call realloc(neamnat,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','neamnat')
  call realloc(neamnat2,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','neamnat2')
  call realloc(neamtyp,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','neamtyp')
  call realloc(neamtyp2,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','neamtyp2')
  call realloc(lmeamspec,maxeamspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxeamspec','lmeamspec')
!
!  Initialise defaults for new part of array
!
  if (maxeamspec.gt.oldmaxeamspec) then
    do i = oldmaxeamspec+1,maxeamspec
      call initmaxeamspecdefaults(i)
    enddo
  endif
!
!  Save current value of maxeamspec for next call
!
  oldmaxeamspec = maxeamspec
!
  return
  end
