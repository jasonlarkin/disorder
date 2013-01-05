  subroutine outstresses(lprint)
!
!  Calculate the stresses and output them
!
!   5/12 Created from property
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations, only : lanisotropicpresscfg
  use constants
  use control,        only : lstressout, latomicstress
  use current
  use derivatives,    only : stresses
  use iochannels
  use parallel,       only : ioproc
  implicit none
!
!  Local variables
!
  logical                                        :: lprint
  real(dp)                                       :: rvol
  real(dp)                                       :: vol
  real(dp)                                       :: volume
!
  vol = volume(rv)
!**********************
!  Output properties  *
!**********************
  if (lprint.and.ioproc) then
!
!  Stress tensor output
!
    if (lstressout) then
!
!  Convert strain derivatives to stresses and adjust units to GPa
!
      rvol = 10.0_dp*evtoj*1.0d20/vol
      stresses(1:6) = rvol*stresses(1:6)
!
!  Correct stresses for any applied pressure
!
      stresses(1:3) = stresses(1:3) - press
      if (lanisotropicpresscfg(ncf)) then
        stresses(1:6) = stresses(1:6) - anisotropicpress(1:6)
      endif
!
      write(ioout,'(''  Final stress tensor components (GPa):'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''       xx '',1x,f14.6,''    yz '',1x,f14.6)') stresses(1),stresses(4)
      write(ioout,'(''       yy '',1x,f14.6,''    xz '',1x,f14.6)') stresses(2),stresses(5)
      write(ioout,'(''       zz '',1x,f14.6,''    xy '',1x,f14.6)') stresses(3),stresses(6)
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  Flush the output buffer
!
  call gflush(ioout)
!
  return
  end
