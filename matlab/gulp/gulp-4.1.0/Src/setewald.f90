  subroutine setewald
!
!  Calculates the Ewald sum cut-offs in real and reciprocal space
!
!   9/03 Created from kindex
!   8/12 Check on keyword "old" changed to "oldewald" to avoid conflict with other keywords
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
!  Julian Gale, Curtin University, August 2012
!
  use constants
  use control
  use current
  use general
  use iochannels
  use kspace
  use optimisation
  use parallel
  use shell
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: j
  logical                  :: lverb
  real(dp)                 :: fact
  real(dp)                 :: rrmx2
!
  lverb = (index(keyword,'verb').ne.0)
  if (ndim.eq.3) then
    call kvector3D
  elseif (ndim.eq.2) then
    call kvector2D
  elseif (ndim.eq.1) then
    call kvector1D
  endif
!******************************************************************
!  Set the accuracy factor so that it is greater in the 2-D case  *
!******************************************************************
  if (ndim.eq.3) then
    accf = 10.0_dp**accuracy
    accf = sqrt(log(accf))
  else
    accf = 100.0_dp*10.0_dp**accuracy
    accf = sqrt(log(accf))
  endif
!********************************
!  Calculate optimum eta value  *
!********************************
  if (targetrradmax.gt.0.0_dp) then
    seta = accf/targetrradmax
    eta = seta*seta
  else
    if (ndim.eq.3) then
!
!  Try changing number of species to increase reciprocal space weighting
!
      fact = 0.0_dp
      do i = 1,numat
        fact = fact + occuf(i)
      enddo
      if (lfreeze.or.index(keyword,'oldew').ne.0) then
!
!  Common formula/rspeed for all levels of evaluation
!
        eta = rspeed0*fact*pi*(0.25_dp*vol4pi)**2
      else
        eta = rspeed0*sqrt(fact)*pi*(0.25_dp*vol4pi)**2
      endif
      eta = (eta)**(1.0_dp/3.0_dp)
    elseif (ndim.eq.2) then
      eta = 0.5_dp*rspeed0*vol4pi
    elseif (ndim.eq.1) then
      eta = 0.5_dp*rspeed0*vol4pi
    endif
    seta = sqrt(eta)
  endif
!*********************************************************
!  Determine the real and reciprocal space sum cut-offs  *
!*********************************************************
  if (ndim.eq.3) then
    rradmax = 2.0_dp*accf*seta
    rrmx2 = rradmax**2
!
!  Limit reciprocal space cutoff so as to avoid very small exponential terms
!
    if (rrmx2.gt.140.0_dp*eta) then
      rrmx2 = 140.0_dp*eta
      rradmax = sqrt(rrmx2)
    endif
    radmax = accf/seta
    rmx2 = radmax**2
    if (rmx2.gt.(85.0_dp/eta)) rmx2 = (85.0_dp/eta) 
    tweatpi = 2.0_dp*sqrt(eta/pi)
  elseif (ndim.eq.2) then
    rradmax = 2.0_dp*accf*seta
    rrmx2 = rradmax**2
    radmax = accf/seta
    rmx2 = radmax**2
    if (rmx2.gt.(85.0_dp/eta)) rmx2 = (85.0_dp/eta) 
    tweatpi = 2.0_dp*sqrt(eta/pi)
  elseif (ndim.eq.1) then
    rradmax = 2.0_dp*accf*seta
    rrmx2 = rradmax**2
    radmax = accf/seta
    rmx2 = radmax**2
    if (rmx2.gt.(85.0_dp/eta)) rmx2 = (85.0_dp/eta) 
  endif
!
!  Option to print out Ewald parameters
!
  if (lverb.and.ioproc) then
    write(ioout,'(/,''  Ewald parameters :  Eta               = '',f12.6,'' Angs-2'')') eta
    write(ioout,'(''                      Real space radius = '',f12.6,'' Angs'')') radmax
    write(ioout,'(''                      Recip space radius= '',f12.6,'' Angs-1'')') rradmax
    write(ioout,'(/,''  Reciprocal lattice vectors :'',/)')
    do i = 1,ndim
      write(ioout,'(3(f10.5,2x))')(kv(j,i),j=1,ndim)
    enddo
  endif
!
  return
  end
