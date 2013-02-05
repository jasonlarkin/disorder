  subroutine outdcdheader(iout)
!
!  Writes out the header for a DCD file
!
!   3/1/12 Created from outarc
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
!  Paolo Raiteri and Julian Gale, NRI, Curtin University, January 2012
!
  use current
  use general
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: iout
!
!  Local variables - note that those written to DCD format have to be of fixed precision
!
  character(len=4)  :: car4
  integer(i4)       :: i
  integer*4         :: int4(4)
  integer*4         :: int9(9)
  integer*4         :: icell
  integer*4         :: namin
  integer*4         :: nframes
  integer*4         :: nsanc
  integer*4         :: nstart
  integer*4         :: nstep
  real*4            :: delta
!
  car4 = "CORD"
  nframes = 1                 !! frames in the output dcd (it'll give a worning if wrong)
  nstart = 1                  !! first timestep in output
  nsanc = 1                   !! dcd frequency
  nstep = 1                   !! timestep of the last dcd
  int4 = (/0,0,0,0/)          !! four useless integers :-)
  namin = 0                   !! endian type
  delta = 0.01                !! timestep size
  if (ndim.eq.3) then
    icell = 1                 !! flag to indicate whether the cell is written in the dcd
  else
    icell = 0
  endif
  int9 = (/0,0,0,0,0,0,0,0,24/) !! nine useless integers :-)
!
!  Actually write the header to the DCD file
!
  write(iout) car4, nframes, nstart, nsanc, nstep, int4, namin, delta, icell, int9
  write(iout) ntitle, (titleword(i),i=1,ntitle)
  write(iout) int(numat,4)
!
  return
  end
