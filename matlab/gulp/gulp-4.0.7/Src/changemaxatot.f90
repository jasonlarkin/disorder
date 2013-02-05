  subroutine changemaxatot
!
!  Alters the size of the arrays associated with maxatot
!
!  11/02 Einstein model parameters added
!   1/08 Initialisation of lopfi added 
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
  use configurations
  use moldyn,        only : lfix
  use reallocate
  use scan,          only : ltranat
  implicit none
!
!  Local variables
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxatot = 0
!
  call realloc(lbsmat,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','lbsmat')
  call realloc(leinsteinat,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','leinsteinat')
  call realloc(lfix,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','lfix')
  call realloc(ltdforcecfg,3_i4,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','ltdforcecfg')
  call realloc(lopfi,3_i4*maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','lopfi')
  call realloc(lqmatom,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','lqmatom')
  call realloc(lsliceatom,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','lsliceatom')
  call realloc(ltranat,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','ltranat')
  call realloc(natcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','natcfg')
  call realloc(nregionno,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','nregionno')
  call realloc(nspecptrcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','nspecptrcfg')
  call realloc(ntypcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','ntypcfg')
  call realloc(cncfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','cncfg')
  call realloc(forcecfg,3_i4,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','forcecfg')
  call realloc(keinsteinat,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','keinsteinat')
  call realloc(occucfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','occucfg')
  call realloc(oxcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','oxcfg')
  call realloc(qlcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','qlcfg')
  call realloc(radcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','radcfg')
  call realloc(tdforcecfg,3_i4,3_i4,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','tdforcecfg')
  call realloc(xcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','xcfg')
  call realloc(ycfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','ycfg')
  call realloc(zcfg,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','zcfg')
  call realloc(xeinsteinat,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','xeinsteinat')
  call realloc(yeinsteinat,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','yeinsteinat')
  call realloc(zeinsteinat,maxatot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxatot','zeinsteinat')
!
!  Initialise new parts of data arrays
!
  if (maxatot.gt.oldmaxatot) then
    do i = oldmaxatot+1,maxatot
      call initmaxatotdefaults(i)
    enddo
  endif
!
!  Save current value of maxatot for next call
!
  oldmaxatot = maxatot
!
  return
  end
