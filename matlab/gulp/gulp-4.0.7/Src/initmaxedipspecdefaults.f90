  subroutine initmaxEDIPspecdefaults(i)
!
!  Initialises the arrays associated with maxEDIPspec
!
!   9/10 Created from changemax routine
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, September 2010
!
  use library
  use EDIPdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
  integer(i4)             :: j
  integer(i4)             :: maxEDIPspec2
  integer(i4)             :: oldmaxEDIPspec
  integer(i4)             :: oldmaxEDIPspec2 
!
!  Some things depend on pairs of species
!
  maxEDIPspec2 = maxEDIPspec*(maxEDIPspec + 1)/2
  oldmaxEDIPspec = i - 1
  oldmaxEDIPspec2 = oldmaxEDIPspec*(oldmaxEDIPspec + 1)/2
!
!  Initialise new parts of data arrays
!
  if (i.ge.1.and.i.le.maxEDIPspec) then
    do j = oldmaxEDIPspec+1,maxEDIPspec
      lEDIPtriadOK(1:maxEDIPspec2,j) = .false.
    enddo
    do j = oldmaxEDIPspec2+1,maxEDIPspec2
      lEDIPpairOK(j) = .false.
      lEDIPtriadOK(j,1:maxEDIPspec) = .false.
    enddo
  endif
!
  return
  end
