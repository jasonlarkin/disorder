  subroutine gulpfinish
!
!  Finalisation tasks for GULP
!
!   9/06 Created from gulp.F
!   2/09 Modified to accommodate new version of FoX and gulp_cml
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, February 2009
!
  use control
  use gulp_cml,  only : lcml, gulp_cml_exit

  implicit none

!****************
!  Final tasks  *
!****************
  if (lcml) call gulp_cml_exit
  call finish(ldefect)
!
  stop
  end
