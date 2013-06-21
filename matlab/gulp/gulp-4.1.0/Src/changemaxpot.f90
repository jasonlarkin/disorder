  subroutine changemaxpot
!
!  Alters the size of the arrays associated with maxpot
!
!  11/05 Taper energy and gradient arrays added
!   8/06 n2botype added
!   9/06 Array for literal symbols added
!  11/06 Call to init2bodydefaults used instead of explicit
!        initialisation here
!  11/07 Unused variables cleaned up
!  11/08 Name of array for 1/bpot for Buckingham potential changed from rho to rhopot
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   4/09 Size of first dimension of twopot array increased to 6
!   4/09 Size of twopot left-hand dimension incremented to 7
!   3/10 Array that flags rule generated potentials added
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
!  Julian Gale, NRI, Curtin University, November 2012
!
  use potchange
  use reallocate
  use splinedata
  use two
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxpot = 0
!
!  Spline data
!
  call realloc(d1f,maxpts,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','d1f')
  call realloc(d2f,maxpts,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','d2f')
  call realloc(splf,maxpts,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','splf')
  call realloc(splr,maxpts,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','splr')
  call realloc(nsplpt,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','nsplpt')
  call realloc(nsplty,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','nsplty')
!
!  Potential change data
!
  call realloc(npchng,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','npchng')
!
!  Potential data
!
  call realloc(twopot,7_i4,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','twopot')
  call realloc(rpot,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','rpot')
  call realloc(rpot2,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','rpot2')
  call realloc(tpot,12_i4,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','tpot')
  call realloc(tapergrad,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','tapergrad')
  call realloc(taperpot,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','taperpot')
  call realloc(repcut,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','repcut')
  call realloc(rhopot,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','rhopot')
  call realloc(scale14,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','scale14')
  call realloc(eshift,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','eshift')
  call realloc(gshift,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','gshift')
  call realloc(lcombine,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','lcombine')
  call realloc(lgenerated2,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','lgenerated2')
  call realloc(lintra,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','lintra')
  call realloc(linter,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','linter')
  call realloc(leshift,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','leshift')
  call realloc(lgshift,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','lgshift')
  call realloc(mmexc,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','mmexc')
  call realloc(n2botype,2_i4,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','n2botype')
  call realloc(ncombipower,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','ncombipower')
  call realloc(nspec1,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','nspec1')
  call realloc(nspec2,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','nspec2')
  call realloc(nptype,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','nptype')
  call realloc(nptyp1,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','nptyp1')
  call realloc(nptyp2,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','nptyp2')
  call realloc_ch5(symbol2,2_i4,maxpot,ierror)
  if (ierror.ne.0) call outofmemory('changemaxpot','symbol2')
!
!  Initialise defaults for new part of array
!
  if (maxpot.gt.oldmaxpot) then
    do i = oldmaxpot+1,maxpot
      call init2bodydefaults(i)
    enddo
  endif
!
!  Save current value of maxpot for next call
!
  oldmaxpot = maxpot
!
  return
  end
