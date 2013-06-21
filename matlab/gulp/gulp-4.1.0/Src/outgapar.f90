  subroutine outgapar(imode)
!
!  Output genetic algorithm parameters
!
!  imode = controls whether ga procedure is for opt or fit
!          1 => fit
!          2 => opt
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
!  Julian Gale, November 1993
!  Scott Woodley, September 1997
!
  use control
  use costfunction
  use dump
  use fitting
  use genetic
  use iochannels
  use optimisation
  implicit none
!
!  Passed variables
!
  integer(i4) :: imode
!
!  Local variables
!
  real(dp)    :: pts
  real(dp)    :: pcross
  real(dp)    :: pmuta
!
  pts = prob(1)
  pcross = prob(4)
  pmuta = prob(7)
!
  write(ioout,'(/)')
  if (imode.eq.1) then
    write(ioout,'(''  Maximum number of cycles     = '',i13)') maxfcal
    write(ioout,'(''  Number of configurations     = '',i13)') ngacfg
    write(ioout,'(''  Tournament probability       = '',f13.7)') pts
  else
    write(ioout,'(''  Maximum number of cycles     = '',i13)') maxgacyc
    write(ioout,'(''  Initial #configurations      = '',i13)') ngacfg
    if (mgacfg.ne.ngacfg) then
      write(ioout,'(''  expanding by '',i11,'' per cycle'',i13)') nspar
    endif
    write(ioout,'(''  Maximum #configurations      = '',i13,/)') mgacfg
    if (ngabset.eq.0) then
      write(ioout,'(''  Will look for '',i3,'' best candidates once all cycles completed'')')ngabest
    elseif (lgabest) then
      write(ioout,'(''  Will look for '',i3,'' overall best candidates after every '',i4,'' cycles'')')ngabest,ngabset
    else
      write(ioout,'(''  Will look for '',i3,'' best candidates after every '',i4,'' cycles'')')ngabest,ngabset
      write(ioout,'(''  A total of '',i4,'' candidates'')') ngabest*maxgacyc/ngabset
    endif
    write(ioout,'(/,''  Difference between best candidates:'',f9.5)') udif
    if (lgaexpw) then
      write(ioout,'(''  Exponentially weight parental success'')')
    else
      write(ioout,'(''  Tournament probability       = '',f13.7)') pts
    endif
  endif
  if (nspar.ne.0) then
    write(ioout,'(''  #successful old parents      = '',i13,'' (trying again)'')') nspar
  endif
  if (l2pxo) then
    write(ioout,'(''  Implementing a two point crossover scheme'')')
  else
    write(ioout,'(''  Implementing a one point crossover scheme'')')
  endif
  write(ioout,'(''  Crossover probability        = '',f13.7)') pcross
  write(ioout,'(''  Mutation probability         = '',f13.7)') pmuta
  write(ioout,'(''  Random number seed           = '',i13,/)') iseed
  if (ncycd.ne.1000) then
    if (ncycd.eq.1) then
      write(ioout,'(''  Dumpfile to be written after every cycle'',/)')
    else
      write(ioout,'(''  Dumpfile to be written after every '',i4,'' cycles'',/)') ncycd
    endif
  endif
  if (ngacjg.gt.0) then
    write(ioout,'(''  Using a conjugate minimiser on'')')
    write(ioout,'(''  top odd 5 structures every '',i11,''th cycle'')') ngacjg
  endif
  if (lgacost) then
    write(ioout,'(/,'' COST FUNCTION'',/)')
    write(ioout,'(''  Will use the following weighted terms to assess the candidate structures'',/)')
    write(ioout,'(''   Weight                 Term'')')
    if (dabs(kbcf).gt.0.1d-10) then
      write(ioout,'('' '',f8.4,''    Bond Valence Discrepancy'')') kbcf
    endif
    if (dabs(kcccf).gt.0.1d-10) then
      write(ioout,'('' '',f8.4,''    Cation 1st Coodination Number Discrepancy'')') kcccf
    endif
    if (dabs(kcacf).gt.0.1d-10) then
      write(ioout,'('' '',f8.4,''    Anion  1st Coodination Number Discrepancy'')') kcacf
    endif
    if (dabs(kqccf).gt.0.1d-10) then
      write(ioout,'('' '',f8.4,''    Coulombic Energy between Cations'')') kqccf
    endif
    if (dabs(kqacf).gt.0.1d-10) then
      write(ioout,'('' '',f8.4,''    Coulombic Energy between Anions'')') kqacf
    endif
    if (dabs(kscf).gt.0.1d-10) then
      write(ioout,'('' '',f8.4,''    Bond Valence Discrepancy in 2nd coordination shell'')') kscf
    endif
    if (dabs(kacf-1.0d0).gt.0.1d-10) then
      write(ioout,'('' '',f8.4,''    Coodination Numbers weighted in favour of 1st defined atom'')') kacf
    endif
    write(ioout,'(/)')
  endif
  if (imode.eq.1) then
    write(ioout,'(''  Start of fitting :'',/)')
  else
    write(ioout,'(''  Start of optimisation :'',/)')
  endif
!
  return
  end
