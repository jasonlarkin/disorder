  subroutine outmc(fc,nmcstep,temperature)
!
!  Writes out MC dumpfile on channel 31 plus the normal dumpfile.
!
!  fc = trial function value at current step
!
!  Format of MC dumpfile for each configuration is:
!
!  numat
!  fc
!  all the atomic numbers
!  all the atomic types
!  all the absolute x coordinates
!  all the absolute y coordinates
!  all the absolute z coordinates
!
!   1/01 Created
!   5/07 GMC file changed to be formatted 
!   5/07 Step number and temperature now passed in and written out
!   5/07 Cell information output to GMC file
!   1/08 nmcstep now passed to dumpdur
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use current,    only : numat, nat, nftype, xclat, yclat, zclat, ndim, rv
  use dump
  use files
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: nmcstep
  real(dp),    intent(in) :: fc
  real(dp),    intent(in) :: temperature
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: j
!
!  Ordinary dumpfile
!
  if (idump.gt.0) call dumpdur(idump,nmcstep)
!
!  MC dumpfile on channel 31
!
  write(31,'(''Step: '',i12,'' Temp: '',f20.6)') nmcstep,temperature
  write(31,'(f32.8)') fc
  if (ndim.eq.3) then
    write(31,'(3f16.8)') (rv(j,1),j=1,3)
    write(31,'(3f16.8)') (rv(j,2),j=1,3)
    write(31,'(3f16.8)') (rv(j,3),j=1,3)
  elseif (ndim.eq.2) then
    write(31,'(2f16.8)') (rv(j,1),j=1,2)
    write(31,'(2f16.8)') (rv(j,2),j=1,2)
  elseif (ndim.eq.1) then
    write(31,'(f16.8)') rv(1,1)
  endif
  write(31,'(i8)') numat
  do i = 1,numat
    write(31,'(i4,1x,i5,3(1x,f12.6))') nat(i),nftype(i),xclat(i),yclat(i),zclat(i)
  enddo
!
  call gflush(31_i4)
!
  return
  end
