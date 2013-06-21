  subroutine changemaxspec
!
!  Alters the size of the arrays associated with maxspec
!
!  11/04 linspec added
!   7/05 Shell mass ratio species added
!   5/06 Mass for individual species added
!  11/06 ldefshspec added
!   4/07 UFF arrays added
!   3/08 pekin array added
!   3/08 numofspec array added
!   9/10 Initialisations now performed in a subroutine
!   9/12 NMR properties added to species module
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, September 2012
!
  use library
  use polarise
  use m_pr,         only : pekin
  use montecarlo
  use reallocate
  use shell
  use species
  use two
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxspec = 0
!
!  Species data
!
  call realloc(natspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','natspec')
  call realloc(numofspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','numofspec')
  call realloc(ntypspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ntypspec')
  call realloc(lbrspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','lbrspec')
  call realloc(ldefshspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ldefshspec')
  call realloc(linspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','linspec')
  call realloc(lmassinspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','lmassinspec')
  call realloc(lnmrinspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','lnmrinspec')
  call realloc(lqinspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','lqinspec')
  call realloc(lmask,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','lmask')
  call realloc(c6spec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','c6spec')
  call realloc(qlspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','qlspec')
  call realloc(massspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','massspec')
  call realloc(nmrspec,3_i4,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','nmrspec')
  call realloc(radspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','radspec')
!
!  Library data
!
  call realloc_ch16(libspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','libspec')
!
!  GCMC related data
!
  call realloc(ngcmcnat,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ngcmcnat')
  call realloc(ngcmctype,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ngcmctype')
!
!  Polarisability data
!
  call realloc(dpolspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','dpolspec')
  call realloc(qpolspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','qpolspec')
  call realloc(natpolspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','natpolspec')
  call realloc(ntyppolspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ntyppolspec')
!
!  Shell mass ratio data
!
  call realloc(ratiomspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ratiomspec')
  call realloc(natratiomspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','natratiomspec')
  call realloc(ntypratiomspec,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ntypratiomspec')
!
!  Two-body related data
!
  call realloc(epsilon,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','epsilon')
  call realloc(sigma,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','sigma')
  call realloc(natse,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','natse')
  call realloc(ntypse,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ntypse')
  call realloc(atoma,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','atoma')
  call realloc(atomb,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','atomb')
  call realloc(nattab,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','nattab')
  call realloc(ntypab,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','ntypab')
!
!  MD related data
!
  call realloc(pekin,maxspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxspec','pekin')
!
!  Initialise defaults for new part of array
!
  if (maxspec.gt.oldmaxspec) then
    do i = oldmaxspec+1,maxspec
      call initmaxspecdefaults(i)
    enddo
  endif
!
!  Save current value of maxspec for next call
!
  oldmaxspec = maxspec
!
  return
  end
