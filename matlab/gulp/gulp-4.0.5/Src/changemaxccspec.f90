  subroutine changemaxCCspec
!
!  Alters the size of the arrays associated with maxCCspec
!
!   7/05 Created from changemaxCCspec
!  12/07 CCparAE allocation added
!   9/10 Initialisations now performed in a subroutin
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
  use chargecoupled
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxCCspec = 0
!
!  Species data
!
  call realloc(natCCspec,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','natCCspec')
  call realloc(ntypCCspec,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','ntypCCspec')
  call realloc(nCCparNb,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','nCCparNb')
  call realloc(CCbeta,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCbeta')
  call realloc(CCeta,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCeta')
  call realloc(CClambda,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CClambda')
  call realloc(CCmu,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCmu')
  call realloc(CCparA,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparA')
  call realloc(CCparB,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparB')
  call realloc(CCparC,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparC')
  call realloc(CCparD,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparD')
  call realloc(CCparDL,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparDL')
  call realloc(CCparDU,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparDU')
  call realloc(CCparH,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparH')
  call realloc(CCparM,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparM')
  call realloc(CCparN,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparN')
  call realloc(CCparIE,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparIE')
  call realloc(CCparAE,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparAE')
  call realloc(CCparQL,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparQL')
  call realloc(CCparQU,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCparQU')
  call realloc(CCvdwC,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','CCvdwC')
  call realloc(rCCmaxL,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','rCCmaxL')
  call realloc(rCCmaxS,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','rCCmaxS')
  call realloc(rCCminL,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','rCCminL')
  call realloc(rCCminS,maxCCspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxCCspec','rCCminS')
!
!  Initialise defaults for new part of array
!
  if (maxCCspec.gt.oldmaxCCspec) then
    do i = oldmaxCCspec+1,maxCCspec
      call initmaxccspecdefaults(i)
    enddo
  endif
!
!  Save current value of maxCCspec for next call
!
  oldmaxCCspec = maxCCspec
!
  return
  end
