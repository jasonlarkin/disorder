  subroutine optin
!
!  Subroutine for optimisation of crystal structure
!  Prints out input parameters
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
  use iochannels
  use optimisation
  use parallel
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4) :: ichcrit
!
  if (morder.gt.0.and.nupdate.eq.10) nupdate = 1
!
!  From here on is just output
!
  if (.not.ioproc) return
!
  write(ioout,'(/)')
  write(ioout,'(''  Number of variables = '',i8,/)') nvar
  write(ioout,'(''  Maximum number of calculations  = '',i13)') maxcal
  write(ioout,'(''  Maximum Hessian update interval = '',i13)') nupdate
  write(ioout,'(''  Maximum step size               = '',f13.9)') stepmax
  write(ioout,'(''  Maximum parameter tolerance     = '',f13.9)') xtol
  write(ioout,'(''  Maximum function  tolerance     = '',f13.9)') ftol
  write(ioout,'(''  Maximum gradient  tolerance     = '',f13.9)') gtol
  write(ioout,'(''  Maximum gradient  component     = '',f13.9,/)') grmax
  if (llbfgs) then
    write(ioout,'(''  Maximum LM gradient tolerance   = '',f13.9)') lmgtol
  endif
  if (lsymopt) then
    write(ioout,'(''  Symmetry constrained optimisation'',/)')
    if (lsymderv2) then
      write(ioout,'(''  Symmetry used for second derivatives'',/)')
    elseif (lsymderv) then
      write(ioout,'(''  Symmetry used only for first derivatives'',/)')
    endif
  elseif (lsym) then
    write(ioout,'(''  Symmetry not applied to optimisation'',/)')
  endif
  if (lstr) then
    if (loptcellpar) then
      write(ioout,'(''  Cell parameters to be optimised directly'',/)')
    else
      write(ioout,'(''  Cell parameters to be optimised using strains'',/)')
    endif
  endif
  if (lrfo) then
    write(ioout,'(''  RFO method to be used'',/)')
    if (mode.gt.1.and.morder.eq.1) then
      write(ioout,'(''  Transition state search along mode '',i2,/)')mode
    elseif (morder.gt.0) then
      write(ioout,'(''  Transition state search of order '',i3,/)') morder
    endif
  else
    if (llbfgs) then
      write(ioout,'(''  Limited Memory BFGS to be used of order '',i4,/)') lmbfgsorder
    elseif (lconj) then
      write(ioout,'(''  Conjugate gradient optimiser to be used'',/)')
    else
      write(ioout,'(''  Newton-Raphson optimiser to be used'',/)')
    endif
  endif
  if (nupdate.gt.1.and..not.lconj) then
    if (lrfo.and.index(keyword,'bfgs').eq.0.or.index(keyword,'dfp').ne.0) then
      write(ioout,'(''  DFP hessian update to be used'',/)')
    else
      write(ioout,'(''  BFGS hessian update to be used'',/)')
    endif
  endif
  if (lminch) then
    if (mintype.eq.1) then
      write(ioout,'(''  Minimiser to switch to BFGS with exact hessian'')')
    elseif (mintype.eq.2) then
      write(ioout,'(''  Minimiser to switch to RFO'')')
    elseif (mintype.eq.3) then
      write(ioout,'(''  Minimiser to switch to BFGS with unit hessian'')')
    elseif (mintype.eq.4) then
      write(ioout,'(''  Minimiser to switch to BFGS with numerical diagonal hessian'')')
    elseif (mintype.eq.5) then
      write(ioout,'(''  Minimiser to switch to Conjugate Gradients'')')
    endif
    if (minchcrit.eq.1) then
      ichcrit=nint(chcrit)
      write(ioout,'(''  After '',i4,'' cycles of optimisation'',/)')ichcrit
    else
      write(ioout,'(''  When gradient norm is less than '',f12.6,/)')chcrit
    endif
  endif
  if (index(keyword,'shel').ne.0) then
    write(ioout,'(''  Shell only optimisation'',/)')
  endif
  if (ncycd.ne.1000) then
    if (ncycd.eq.1) then
      write(ioout,'(''  Dumpfile to be written after every cycle'',/)')
    else
      write(ioout,'(''  Dumpfile to be written after every '',i4,'' cycles'',/)')ncycd
    endif
  endif
  if (ndim.eq.3) then
    write(ioout,'(''  Start of bulk optimisation :'',/)')
  elseif (ndim.eq.2) then
    write(ioout,'(''  Start of surface optimisation :'',/)')
  elseif (ndim.eq.1) then
    write(ioout,'(''  Start of polymer optimisation :'',/)')
  else
    write(ioout,'(''  Start of cluster optimisation :'',/)')
  endif
!
  return
  end
