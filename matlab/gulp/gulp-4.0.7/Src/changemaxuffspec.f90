  subroutine changemaxUFFspec
!
!  Alters the size of the arrays associated with maxUFFspec
!
!   4/07 Created from changemaxspec
!   5/07 nUFFtype added
!   5/07 UFFtor added
!   7/07 symbolUFFspec added
!   5/08 New arrays for out of plane term added
!   5/08 UFFchi added
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, May 2008
!
  use library
  use reallocate
  use uffdata
  implicit none
!
  integer(i4)       :: ierror
!
!  Species data
!
  call realloc_ch5(symbolUFFspec,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','symbolUFFspec')
  call realloc(natUFFspec,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','natUFFspec')
  call realloc(ntypUFFspec,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','ntypUFFspec')
  call realloc(nUFFtype,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','nUFFtype')
  call realloc(UFFr,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFr')
  call realloc(UFFtheta,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFtheta')
  call realloc(UFFtor,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFtor')
  call realloc(UFFx,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFx')
  call realloc(UFFd,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFd')
  call realloc(UFFzeta,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFzeta')
  call realloc(UFFZeff,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFZeff')
  call realloc(UFFKoop,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFKoop')
  call realloc(UFFthetaoop,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFthetaoop')
  call realloc(UFFchi,maxUFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxuffspec','UFFchi')
!
  return
  end
