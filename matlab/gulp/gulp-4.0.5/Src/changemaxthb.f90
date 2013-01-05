  subroutine changemaxthb
!
!  Alters the size of the arrays associated with maxthb
!
!   5/01 Minimum three-body distance arrays added
!   8/06 n3botype added
!   9/06 Threebody theta taper added
!   9/06 Array for literal symbols added
!   5/07 ltdreiding added
!   7/07 Extra dimension added to n3botype
!   6/08 n3bondno added
!   6/09 Module name changed from three to m_three
!   3/10 Arrays that flag rule generated potentials added
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
  use m_three
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxthb = 0
!
!  Three-body data
!
  call realloc_ch5(symbol3,3_i4,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','symbol3')
  call realloc(threepoly,11_i4,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','threepoly')
  call realloc(thbk,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thbk')
  call realloc(theta,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','theta')
  call realloc(thetatapermax,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thetatapermax')
  call realloc(thetatapermin,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thetatapermin')
  call realloc(thr1min,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thr1min')
  call realloc(thr2min,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thr2min')
  call realloc(thr3min,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thr3min')
  call realloc(thr1,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thr1')
  call realloc(thr2,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thr2')
  call realloc(thr3,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thr3')
  call realloc(thrho1,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thrho1')
  call realloc(thrho2,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thrho2')
  call realloc(thrho3,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','thrho3')
  call realloc(ntspec1,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ntspec1')
  call realloc(ntspec2,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ntspec2')
  call realloc(ntspec3,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ntspec3')
  call realloc(ntptyp1,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ntptyp1')
  call realloc(ntptyp2,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ntptyp2')
  call realloc(ntptyp3,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ntptyp3')
  call realloc(nthrty,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','nthrty')
  call realloc(mmtexc,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','mmtexc')
  call realloc(n3botype,2_i4,2_i4,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','n3botype')
  call realloc(n3bondno,maxn3bondnono,2_i4,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','n3bondno')
  call realloc(n3bondnono,2_i4,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','n3bondnono')
  call realloc(lgenerated3,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','lgenerated3')
  call realloc(ltdreiding,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ltdreiding')
  call realloc(ltintra,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ltintra')
  call realloc(ltinter,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','ltinter')
  call realloc(lthetataper,maxthb,ierror)
  if (ierror.ne.0) call outofmemory('changemaxthb','lthetataper')
!
!  Initialise defaults for new part of array
!
  if (maxthb.gt.oldmaxthb) then
    do i = oldmaxthb+1,maxthb
      call initmaxthbdefaults(i)
    enddo
  endif
!
!  Save current value of maxthb for next call
!
  oldmaxthb = maxthb
!
  return
  end
