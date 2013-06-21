  subroutine changemaxfit
!
!  Alters the size of the arrays associated with maxfit
!
!   3/06 nfvar2 added
!   4/08 nfpot2 & nfpot3 added
!  11/08 nfvar2 added
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
  use fitting
  use genetic
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxfit = 0
!
  call realloc(ndiscret,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','ndiscret')
  call realloc(nfatyp,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfatyp')
  call realloc(nfcfg,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfcfg')
  call realloc(nfitptr,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfitptr')
  call realloc(nfpot,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfpot')
  call realloc(nfpot2,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfpot2')
  call realloc(nfpot3,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfpot3')
  call realloc(nftyp,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nftyp')
  call realloc(nfvar,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfvar')
  call realloc(nfvar2,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfvar2')
  call realloc(nfvar3,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','nfvar3')
  call realloc(scale,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','scale')
  call realloc(xmax,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','xmax')
  call realloc(xmin,maxfit,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfit','xmin')
!
!  Initialise new parts of data arrays
!
  if (maxfit.gt.oldmaxfit) then
    do i = oldmaxfit+1,maxfit
      call initmaxfitdefaults(i)
    enddo
  endif
!
!  Save current value of maxfit for next call
!
  oldmaxfit = maxfit
!
  return
  end
