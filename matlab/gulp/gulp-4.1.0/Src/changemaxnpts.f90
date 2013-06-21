  subroutine changemaxnpts
!
!  Alters the size of the arrays associated with maxnpts
!  for one-dimensional arrays
!
!  12/08 Migrated to version 3.5 and converted to f90 format
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
  use cosmo
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxnpts = 0
!
  call realloc(sphere1,3_i4,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','sphere1')
  call realloc(cosmoatomptr,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','cosmoatomptr')
  call realloc(cosmoBq,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','cosmoBq')
  call realloc(sas,3_i4,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','sas')
  call realloc(segweight,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','segweight')
  call realloc(nnearseg,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','nnearseg')
  call realloc(nnearsegptr,maxnearseg,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','nnearsegptr')
  call realloc(nnearsegptrcell,maxnearseg,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','nnearsegptrcell')
  call realloc(npwt,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','npwt')
  call realloc(npwtptr,maxnpwt,maxnpts,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts','npwtptr')
!     
!  Initialise new parts of data arrays
!     
  if (maxnpts.gt.oldmaxnpts) then
    do i = oldmaxnpts+1,maxnpts
      call initmaxnptsdefaults(i)
    enddo
  endif
!
!  Save current value of maxnpts for next call
!
  oldmaxnpts = maxnpts
!
  return
  end
!
  subroutine changemaxnpts2
!
!  Alters the size of the arrays associated with maxnpts2
!  for two-dimensional arrays
!
!   1/05 cosmoB removed
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
!  Julian Gale, Curtin University, January 2005
!
  use cosmo
  use reallocate
  implicit none
!
  integer(i4)       :: ierror
!
  call realloc(cosmoA,maxnpts2*(maxnpts2+1_i4)/2_i4,ierror)
  if (ierror.ne.0) call outofmemory('changemaxnpts2','cosmoA')
!
  return
  end
