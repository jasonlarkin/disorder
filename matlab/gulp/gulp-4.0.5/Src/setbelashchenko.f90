  subroutine setbelashchenko(neamfnspecloc)
!
!  Sets the dependent coefficients for the Belashchenko style of EAM
!  functional.
!
!  neamfnspecloc = pointer to eamfnpar species
!
!   9/09 Created 
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
!  Julian Gale, NRI, Curtin University, September 2009
!
  use eam
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(in)     :: neamfnspecloc
!
!  Local variables
!
  integer(i4)                  :: i
  real(dp)                     :: atmp
  real(dp)                     :: btmp
  real(dp)                     :: ctmp
  real(dp)                     :: fn
  real(dp)                     :: dfndr
  real(dp)                     :: rhoim1
  real(dp)                     :: rhoim2
  real(dp)                     :: rm
!*****************
!  Belashchenko  *
!*****************
!
!  eamfnpar  1- 7  => rho1 - rho7
!  eamfnpar  8-15  => a1 - a8
!  eamfnpar 16-23  => b1 - b8
!  eamfnpar 24-31  => c1 - c8
!  eamfnpar 32-33  => m / n
!
!  Set b1 = 0.0 for safety
!
  eamfnpar(16,neamfnspecloc) = 0.0_dp
!**************
!  Range 1-2  *
!**************
!
!  Set rho at boundary 1-2
!
  rhoim1 = eamfnpar(1,neamfnspecloc)
!
!  Compute function at end of previous range and its derivative : rho0 = 1
!
  atmp = eamfnpar(8,neamfnspecloc)
  ctmp = eamfnpar(24,neamfnspecloc)
  fn = atmp + ctmp*(rhoim1 - 1.0_dp)**2
  dfndr = 2.0_dp*ctmp*(rhoim1 - 1.0_dp)
  eamfnpar(9,neamfnspecloc) = fn
  eamfnpar(17,neamfnspecloc) = dfndr
!*****************************
!  Loop over ranges i = 3-5  *
!*****************************
  do i = 3,5
!
!  Set rho at boundary and one before that
!
    rhoim1 = eamfnpar(i-1,neamfnspecloc)
    rhoim2 = eamfnpar(i-2,neamfnspecloc)
!
!  Compute function at end of previous range and its derivative
!
    atmp = eamfnpar(6+i,neamfnspecloc)
    btmp = eamfnpar(14+i,neamfnspecloc)
    ctmp = eamfnpar(22+i,neamfnspecloc)
    fn = atmp + btmp*(rhoim1 - rhoim2) + ctmp*(rhoim1 - rhoim2)**2
    dfndr = btmp + 2.0_dp*ctmp*(rhoim1 - rhoim2)
    eamfnpar(7+i,neamfnspecloc) = fn
    eamfnpar(15+i,neamfnspecloc) = dfndr
  enddo
!********************
!  Ranges i =  < 5  *
!********************
!
!  Set rho at boundary and one before that
!
  rhoim1 = eamfnpar(5,neamfnspecloc)
  rhoim2 = eamfnpar(4,neamfnspecloc)
!
!  Compute function at end of previous range and its derivative
!
  atmp = eamfnpar(12,neamfnspecloc)
  btmp = eamfnpar(20,neamfnspecloc)
  ctmp = eamfnpar(28,neamfnspecloc)
  fn = atmp + btmp*(rhoim1 - rhoim2) + ctmp*(rhoim1 - rhoim2)**2
  dfndr = btmp + 2.0_dp*ctmp*(rhoim1 - rhoim2)
  eamfnpar(13,neamfnspecloc) = fn
  eamfnpar(21,neamfnspecloc) = dfndr
!*********************
!  Ranges i = 6 - 7  *
!*********************
!
!  Set rho at boundary and one before that
!
  rhoim1 = eamfnpar(6,neamfnspecloc)
  rhoim2 = 1.0_dp
!
!  Compute function at end of previous range and its derivative
!
  atmp = eamfnpar(8,neamfnspecloc)
  ctmp = eamfnpar(24,neamfnspecloc)
  fn = atmp + ctmp*(rhoim1 - rhoim2)**2
  dfndr = 2.0_dp*ctmp*(rhoim1 - rhoim2)
  eamfnpar(14,neamfnspecloc) = fn
  eamfnpar(22,neamfnspecloc) = dfndr
!*******************
!  Ranges i = > 7  *
!*******************
!
!  Set rho at boundary and one before that
!
  rhoim1 = eamfnpar(7,neamfnspecloc)
  rhoim2 = eamfnpar(6,neamfnspecloc)
!
!  Compute function at end of previous range and its derivative
!
  rm = eamfnpar(32,neamfnspecloc)
  atmp = eamfnpar(14,neamfnspecloc)
  btmp = eamfnpar(22,neamfnspecloc)
  ctmp = eamfnpar(30,neamfnspecloc)
  fn = atmp + btmp*(rhoim1 - rhoim2) + ctmp*(rhoim1 - rhoim2)**rm
  dfndr = btmp + rm*ctmp*(rhoim1 - rhoim2)**(rm-1.0_dp)
  eamfnpar(15,neamfnspecloc) = fn
  eamfnpar(23,neamfnspecloc) = dfndr
!
  end
