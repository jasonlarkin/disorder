  subroutine changemaxfor
!
!  Alters the size of the arrays associated with maxfor
!
!   8/06 n4botype added
!   9/06 lfdreiding added
!   9/06 Array for literal symbols added
!   4/08 Arrays for minimum cutoff distance added
!   5/08 Option to check bond number logical added
!  11/08 Logical array to flag out of plane potentials added
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
  use four
  use reallocate
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxfor = 0
!
!  Four-body data
!
  call realloc_ch5(symbol4,4_i4,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','symbol4')
  call realloc(forpoly,5_i4,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','forpoly')
  call realloc(fork,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','fork')
  call realloc(for1,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for1')
  call realloc(for2,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for2')
  call realloc(for3,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for3')
  call realloc(for4,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for4')
  call realloc(for1min,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for1min')
  call realloc(for2min,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for2min')
  call realloc(for3min,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for3min')
  call realloc(for4min,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','for4min')
  call realloc(n4botype,2_i4,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','n4botype')
  call realloc(npfor,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','npfor')
  call realloc(nfspec1,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfspec1')
  call realloc(nfspec2,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfspec2')
  call realloc(nfspec3,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfspec3')
  call realloc(nfspec4,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfspec4')
  call realloc(nfptyp1,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfptyp1')
  call realloc(nfptyp2,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfptyp2')
  call realloc(nfptyp3,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfptyp3')
  call realloc(nfptyp4,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nfptyp4')
  call realloc(nforty,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','nforty')
  call realloc(mmfexc,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','mmfexc')
  call realloc(lfdreiding,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','lfdreiding')
  call realloc(lfintra,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','lfintra')
  call realloc(lfinter,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','lfinter')
  call realloc(lgenerated4,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','lgenerated4')
  call realloc(lonly3oop,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','lonly3oop')
  call realloc(loutofplane,maxfor,ierror)
  if (ierror.ne.0) call outofmemory('changemaxfor','loutofplane')
!
!  Initialise defaults for new part of array
!
  if (maxfor.gt.oldmaxfor) then
    do i = oldmaxfor+1,maxfor
      call initmaxfordefaults(i)
    enddo
  endif
!
!  Save current value of maxfor for next call
!
  oldmaxfor = maxfor
!
  return
  end
