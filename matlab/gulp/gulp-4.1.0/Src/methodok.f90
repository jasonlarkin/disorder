  subroutine methodok
!
!  Check whether requested options are compatible with the
!  derivatives that are available for the present potential
!  model.
!
!   4/01 Created 
!   9/04 Check to make sure that EEM / nboQ are not used in
!        the same run
!   7/05 Streitz and Mintmire modifications added
!   5/06 Check for switch to second derivative optimisation
!        method in parallel and disable
!   7/06 Check for whether Mott-Littleton method is OK added
!   4/08 Phonon/frequency calculations allowed to proceed for
!        case when lnoanald2 is true if finite differences are
!        specified
!   5/08 Finite differences enabled when eigen keyword is specified
!   5/09 Free energy minimisation disabled for EAM/MEAM
!   6/09 Modified so that properties can be calculated with finite
!        differences where analytic second derivatives are unavailable
!   3/10 Incorrect flagging of error for lsuttonc case removed
!   6/10 Check for incompatibility between solvation model and other
!        options added
!   9/12 Pacha added
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
  use bondorderdata, only : nboQ
  use cellmultipole, only : icmm
  use control
  use element,       only : lqeq, lSandM, lpacha
  use general,       only : phondiff, findiffc, findiffs
  use iochannels
  use optimisation,  only : lminch, mintype
  use parallel
  implicit none
!
!  Local variables
!
  logical lerror
!
  lerror = .false. 
!
  if (lnoanald2) then
!**********************************************
!  Analytical second derivatives unavailable  *
!**********************************************
    if (ldefect) lerror = .true.
    if (linten) lerror = .true.
    if (leigen.and.phondiff.lt.1.0d-12) lerror = .true.
    if (lfreq.and.phondiff.lt.1.0d-12) lerror = .true.
    if (lphon.and.phondiff.lt.1.0d-12) lerror = .true.
    if (lprop.and.(findiffc.lt.1.0d-12.or.findiffs.lt.1.0d-12)) lerror = .true.
    if (lrfo) lerror = .true.
    if (lerror) then
      if (ioproc) then
        call outerror('analytical second derivatives unavailable',0_i4)
        write(ioout,'('' The current potential model is incompatible with keywords:'',/)')
        write(ioout,'('' defect eigenvectors free frequency intensity phonon property'')')
        write(ioout,'('' rfo'',/)')
      endif
      call stopnow('methodok')
    endif
  endif
  if (lnoanald3) then
!*********************************************
!  Analytical third derivatives unavailable  *
!*********************************************
    if (lfree) lerror = .true.
    if (lerror) then
      if (ioproc) then
        call outerror('analytical third derivatives unavailable',0_i4)
        write(ioout,'('' The current potential model is incompatible with keywords:'',/)')
        write(ioout,'('' free'',/)')
      endif
      call stopnow('methodok')
    endif
  endif
  if (lnomottlittleton.and.ldefect) then  
!**************************************
!  Mott-Littleton method unavailable  *
!**************************************
    if (ioproc) then
      call outerror('Mott-Littleton method unavailable',0_i4)
      write(ioout,'('' The current potential model is incompatible with keywords:'',/)')
      write(ioout,'('' defect'',/)')
    endif
    call stopnow('methodok')
  endif
!*************************
!  Parallel unavailable  *
!*************************
  if (nprocs.gt.1) then
!
!  Set flag so that analytical second derivatives are not available
!
    lnoanald2 = .true.
!
    if (leigen) lerror = .true.
    if (lfreq) lerror = .true.
    if (linten) lerror = .true.
    if (lphon) lerror = .true.
    if (lprop) lerror = .true.
    if (lrfo) lerror = .true.
    if (lfree) lerror = .true.
!  
!  Ensure that any minimizer change won't invalidate the run
!  since only conjugate gradients will work in parallel
!       
    if (lminch.and.mintype.ne.5) lerror = .true.
    if (lerror) then
      if (ioproc) then
        call outerror('second derivatives unavailable in parallel',0_i4)
      endif
      call stopnow('methodok')
    endif
    if (leem) lerror = .true.
    if (lqeq) lerror = .true.
    if (lpacha) lerror = .true.
    if (lSandM) lerror = .true.
    if (lerror) then
      if (ioproc) then
        call outerror('variable charges unavailable in parallel',0_i4)
      endif
      call stopnow('methodok')
    endif
  endif
!*****************************************
!  Mutually exclusive potential options  *
!*****************************************
  if (nboQ.gt.0.and.leem) then
!
!  EEM & Bond order charge setting
!
    if (ioproc) then
      call outerror('EEM and bond order charges are mutually exclusive',0_i4)
    endif
    call stopnow('methodok')
  endif
!
  if (nboQ.gt.0.and.icmm.gt.0) then
!
!  Bond order charges and cell multipole method
!
    if (ioproc) then
      call outerror('Bond order charges not available with cell multipoles',0_i4)
    endif
    call stopnow('methodok')
  endif
  if (nboQ.gt.0.and.ldefect) then
!
!  Bond order charges and defect calculations
!
    if (ioproc) then
      call outerror('Bond order charges not available with defect calculations',0_i4)
    endif
    call stopnow('methodok')
  endif
!
!  Solvation and variable charge models - no SCF contribution yet available
!
  if (lcosmo) then
    if (leem.or.lSandM.or.lqeq.or.lpacha) then
      if (ioproc) then
        call outerror('Solvation not yet compatible with variable charges',0_i4)
      endif
      call stopnow('methodok')
    elseif (lreaxFF) then
      if (ioproc) then
        call outerror('Solvation not yet compatible with ReaxFF variable charges',0_i4)
      endif
      call stopnow('methodok')
    endif
  endif
!
  return
  end
