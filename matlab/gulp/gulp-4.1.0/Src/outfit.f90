  subroutine outfit(w1,w2,gc,loutputgc)
!
!  Output final fitted parameters and residuals
!
!   7/97 EAM parameters added
!   6/98 Three-body polynomial coefficients added
!   8/98 Refractive indices added
!  10/98 Output of final parameters now uses call to outvar
!  10/04 Percentage error now output  except for derivatives
!   6/05 Intent added
!   5/06 Output of comparison of initial fit quality and final 
!        one added
!   4/08 Output of gradients now added
!   7/12 Output of overlap for mode observable added
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
!  Julian Gale, NRI, Curtin University, July 2012
!
  use fitting
  use iochannels
  use observables
  implicit none
!
!  Passed variables
!
  real(dp), intent(in) :: w1(*)
  real(dp), intent(in) :: w2(*)
  real(dp), intent(in) :: gc(*)
  logical,  intent(in) :: loutputgc
!
!  Local variables
!
  integer(i4)          :: i
  real(dp)             :: percent
!*****************************
!  Output fitted parameters  *
!*****************************
  call outvar(w1,w2,2_i4)
  if (loutputgc) then
!**************************************
!  Output fitted parameter gradients  *
!**************************************
    call outvarg(gc)
  endif
!**************
!  Residuals  *
!**************
  write(ioout,'(/,''  Final values of residuals :'',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''   Observable no.  Type            Observable   Calculated    Residual  Error(%)'')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  do i = 1,nobs
    if (nobtyp(i).ne.2) then
!
!  Calculate percentage error
!
      if (abs(fobs(i)).gt.1.0d-8) then
        percent = (fcalc(i) - fobs(i))*100.0_dp/abs(fobs(i))
      else
        percent = 0.0_dp
      endif
!
!  Output values
!
      write(ioout,'(7x,i4,8x,a13,1x,3(f12.5,1x),f8.3)') i,nameobs(nobtyp(i)),fobs(i),fcalc(i),fres(i),percent
    else
      write(ioout,'(7x,i4,8x,a13,1x,3(f12.5,1x))') i,nameobs(nobtyp(i)),fobs(i),fcalc(i),fres(i)
    endif
  enddo
  write(ioout,'(''--------------------------------------------------------------------------------'')')
!
  write(ioout,'(/,''  Comparison of initial and final observables :'',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''   Observable no.  Type            Observable   Initial       Final             '')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  do i = 1,nobs
    if (nobtyp(i).eq.29) then
      write(ioout,'(7x,i4,8x,a4,1x,f6.4,3x,3(f12.5,1x))') i,nameobs(nobtyp(i))(1:4),finfo(i),fobs(i),fcalcoriginal(i),fcalc(i)
    else
      write(ioout,'(7x,i4,8x,a13,1x,3(f12.5,1x))') i,nameobs(nobtyp(i)),fobs(i),fcalcoriginal(i),fcalc(i)
    endif
  enddo
  write(ioout,'(''--------------------------------------------------------------------------------'')')
!
  return
  end
