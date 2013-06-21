  subroutine numhess(nvar,nmin,xc,fc,gc,hesinv,xvar,gvar)
!
!  Numerical estimation of hessian diagonal elements
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
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4) :: nmin
  integer(i4) :: nvar
  real(dp)    :: gc(*)
  real(dp)    :: gvar(*)
  real(dp)    :: hesinv(*)
  real(dp)    :: xc(*)
  real(dp)    :: xvar(*)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: iflag
  real(dp)    :: del
  real(dp)    :: deltag
  real(dp)    :: deltax
  real(dp)    :: fc
  real(dp)    :: funct2
  real(dp)    :: ggd
  real(dp)    :: pmstep
  real(dp)    :: tdel
!
  del = 0.01_dp
  tdel = 0.06_dp
!
!  Generate new position for differencing
!
  do i = nmin,nvar
    xvar(i) = xc(i) - sign(del,gc(i))
  enddo
  iflag = 1
!
!  Calculate function and gradients
!
  call funct(iflag,nvar,xvar,funct2,gvar)
  ii = 0
!
!  Calculate diagonal elements
!
  do i = nmin,nvar
    ii = ii + i - nmin + 1
    deltag = gc(i) - gvar(i)
    deltax = xc(i) - xvar(i)
    if (abs(deltag).lt.1.d-12) goto 70
    ggd = abs(gc(i))
    if (funct2.lt.fc) ggd = abs(gvar(i))
    hesinv(ii) = deltax/deltag
    if (hesinv(ii).lt.0.0_dp.and.ggd.lt.1.d-12) goto 70
    if (hesinv(ii).lt.0.0_dp) hesinv(ii) = tdel/ggd
    goto 80
70   hesinv(ii) = 0.01_dp
80   continue
    if (ggd.lt.1.d-12) ggd = 1.d-12
    pmstep = abs(0.1_dp/ggd)
    if (hesinv(ii).gt.pmstep) hesinv(ii) = pmstep
  enddo
!
!  If energy has been lowered by move, accept new position as current one
!
  if (funct2.lt.fc) then
    fc = funct2
    do i = nmin,nvar
      xc(i) = xvar(i)
      gc(i) = gvar(i)
    enddo
  endif
  if (ioproc) then
    write(ioout,'(''  ** Hessian recalculated - numerical/diagonal only **'')')
  endif
!
  return
  end
