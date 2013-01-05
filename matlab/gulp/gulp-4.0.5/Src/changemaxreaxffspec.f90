  subroutine changemaxreaxFFspec
!
!  Alters the size of the arrays associated with maxreaxFFspec
!
!   4/07 Created from changemaxUFFspec
!   8/07 Modified to handle new ReaxFF form
!  12/07 reaxFFconj3 added
!  12/07 Dimensions of lreaxFFtorsinput corrected
!  12/07 lreaxFFbocorrect added
!   1/08 Handling of oldmaxreaxFFspec2 corrected
!   4/08 Extra arrays for precomputing VDW pairwise terms added
!   4/08 Modifications for multiple angle potentials added
!   7/08 pval6 added to reaxFFval3 array
!   1/09 lreaxFFpboOK added 
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
  use library
  use reallocate
  use reaxFFdata
  implicit none
!
  integer(i4)       :: ierror, i
  integer(i4)       :: maxreaxFFspec2
  integer(i4), save :: oldmaxreaxFFspec = 0
!
!  Some things depend on pairs of species
!
  maxreaxFFspec2 = maxreaxFFspec*(maxreaxFFspec + 1)/2
!
!  Species data
!
  call realloc_ch5(symbolreaxFFspec,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','symbolreaxFFspec')
  call realloc(natreaxFFspec,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','natreaxFFspec')
  call realloc(ntypreaxFFspec,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','ntypreaxFFspec')
  call realloc(nreaxFFval3,maxreaxFFspec2,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','nreaxFFval3')
  call realloc(lreaxFFbocorrect,2_i4,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','lreaxFFbocorrect')
  call realloc(lreaxFFmorseinput,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','lreaxFFmorseinput')
  call realloc(lreaxFFpboOK,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','lreaxFFpboOK')
  call realloc(lreaxFFtorsinput,maxreaxFFspec2+1_i4,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','lreaxFFtorsinput')
  call realloc(reaxFFr,3_i4,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFr')
  call realloc(reaxFFalpha,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFalpha')
  call realloc(reaxFFeps,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFeps')
  call realloc(reaxFFrvdw,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFrvdw')
  call realloc(reaxFFgammaw,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFgammaw')
  call realloc(reaxFFpover,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFpover')
  call realloc(reaxFFpunder,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFpunder')
  call realloc(reaxFFhincrement,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFhincrement')
  call realloc(reaxFFlp,3_i4,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFlp')
  call realloc(reaxFFoc1,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFoc1')
  call realloc(reaxFFuc1,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFuc1')
  call realloc(reaxFFpboc,3_i4,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFpboc')
  call realloc(reaxFFval,4_i4,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFval')
  call realloc(reaxFFval1,2_i4,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFval1')
  call realloc(reaxFFval3,6_i4,maxreaxFFval3,maxreaxFFspec2,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFval3')
  call realloc(reaxFFconj3,4_i4,maxreaxFFspec2,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFconj3')
  call realloc(reaxFFhb3,4_i4,maxreaxFFspec,maxreaxFFspec,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFhb3')
  call realloc(reaxFFmorse,6_i4,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFmorse')
  call realloc(reaxFFpen2,3_i4,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFpen2')
  call realloc(reaxFFpen3,maxreaxFFspec2,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFpen3')
  call realloc(reaxFFtor4,5_i4,1_i4+maxreaxFFspec2,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFtor4')
  call realloc(reaxFFDe,3_i4,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFDe')
  call realloc(reaxFFpbe,2_i4,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFpbe')
  call realloc(reaxFFpbo,6_i4,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFpbo')
  call realloc(reaxFFoc2,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFoc2')
  call realloc(reaxFFrmax,maxreaxFFspec,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFrmax')
  call realloc(reaxFFrmaxpair,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFrmaxpair')
!
!  Precompute VDW & Q terms
!
  call realloc(reaxFFDeVDW,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFDeVDW')
  call realloc(reaxFFalphaVDW,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFalphaVDW')
  call realloc(reaxFFr0VDW,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFr0VDW')
  call realloc(reaxFFgammaVDW,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFgammaVDW')
  call realloc(reaxFFgammaQ,maxreaxFFspec2,ierror)
  if (ierror.ne.0) call outofmemory('changemaxreaxFFspec','reaxFFgammaQ')
!
!  Initialise new parts of data arrays
!
  if (maxreaxFFspec.gt.oldmaxreaxFFspec) then
    do i = oldmaxreaxFFspec+1,maxreaxFFspec
      call initmaxreaxffspecdefaults(i)
    enddo
  endif
!
!  Save current value of maxreaxFFspec for next call
!
  oldmaxreaxFFspec = maxreaxFFspec
!
  return
  end
