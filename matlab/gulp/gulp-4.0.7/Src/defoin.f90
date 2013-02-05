  subroutine defoin
!
!  Prints out input parameters for defect calculations
!
!  11/11 Number of variables output format increased to i8
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, November 2011
!
  use control
  use current
  use dump
  use general
  use iochannels
  use optimisation
  use parallel
  implicit none
!
!  Local variables
!
!
  if (morder.gt.0.and.nupdate.eq.10) nupdate = 1
!
!  After here everything is outputing
!
  if (.not.ioproc) return
!
  write(ioout,'(/)')
  write(ioout,'(''  Number of variables = '',i8,/)') nvar
  write(ioout,'(''  Maximum number of calculations  = '',i11)') maxcal
  write(ioout,'(''  Maximum Hessian update interval = '',i11)') nupdate
  write(ioout,'(''  Maximum step size               = '',f11.7)') stepmax
  write(ioout,'(''  Maximum parameter tolerance     = '',f11.7)') xtol
  write(ioout,'(''  Maximum function  tolerance     = '',f11.7)') ftol
  write(ioout,'(''  Maximum gradient  tolerance     = '',f11.7,/)') gtol
  if (ldsym) then
    write(ioout,'(''  Symmetry adapted optimisation'',/)')
  else
    write(ioout,'(''  Symmetry not applied to optimisation'',/)')
  endif
  if (ld2sym) then
    write(ioout,'(''  Symmetry to be used for second derivatives'',/)')
  endif
  if (lrfo) then
    write(ioout,'(''  RFO method to be used'',/)')
    if (mode.gt.1.and.morder.eq.1) then
      write(ioout,'(''  Transition state search along mode '',i2,/)') mode
    elseif (morder.gt.0) then
      write(ioout,'(''  Transition state search of order '',i3,/)') morder
    endif
  else
    if (lconj) then
      write(ioout,'(''  Conjugate gradient optimiser to be used'',/)')
    else
      write(ioout,'(''  Newton-Raphson optimiser to be used'',/)')
    endif
  endif
  if (nupdate.gt.1) then
    if (lrfo.and.index(keyword,'bfgs').eq.0.or.index(keyword,'dfp').ne.0) then
      write(ioout,'(''  DFP hessian update to be used'',/)')
    else
      write(ioout,'(''  BFGS hessian update to be used'',/)')
    endif
  endif
  if (lshello) then
    write(ioout,'(''  Shell only optimisation'',/)')
  endif
  if (ncycd.ne.1000) then
    if (ncycd.eq.1) then
      write(ioout,'(''  Dumpfile to be written after every cycle'',/)')
    else
      write(ioout,'(''  Dumpfile to be written after every '',i4,'' cycles'',/)') ncycd
    endif
  endif
  write(ioout,'(''  Start of defect optimisation :'',/)')
  return
  end
