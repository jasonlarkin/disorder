  subroutine setEDIP
!
!  Subroutine sets the parameters for the EDIP potential.
!
!   9/10 Created from setreaxFF
!  10/10 Cut-off due to exponential term in 2 & 3 body terms added
!  10/10 Cut-off corrected for accuracy factor tolerances and taper
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
!  Julian Gale, NRI, Curtin University, October 2010
!
  use datatypes
  use control,       only : keyword, lnoanald2
  use EDIPdata
  use element
  use iochannels,    only : ioout
  use parallel,      only : ioproc
  implicit none
!
!  Local variables
!
  integer(i4)            :: i
  integer(i4)            :: ind
  integer(i4)            :: j
  logical                :: lpairsOK
  logical,          save :: lprintwarning = .true.
  real(dp)               :: expcut
!
!  Turn off second derivatives
!
  lnoanald2 = .true.
!
!  Check that there are bond order parameters for all pairs of species
!
  ind = 0
  lpairsOK = .true.
  do i = 1,nEDIPspec
    do j = 1,i
      ind = ind + 1
      if (.not.lEDIPpairOK(ind)) lpairsOK = .false.
    enddo
  enddo
  if (.not.lpairsOK) then
!
!  Only print this warning once
!
    if (lprintwarning) then
      call outwarning('bond order parameters are not specified for all pairs in EDIP',0_i4)
      lprintwarning = .false.
    endif
  endif
!
!  Set cutoff radii 
!
  EDIPcutoff = 0.0_dp
  EDIPrmax(1:nEDIPspec) = 0.0_dp
  ind = 0
  do i = 1,nEDIPspec
    do j = 1,i
      ind = ind + 1
      expcut = EDIP2a(ind) + EDIP2aprime(ind)*EDIPmaxZcutoff + EDIP2sigma(ind)*EDIPaccuracy2drmax
      EDIPrmaxpair(ind) = max(EDIPphigh(ind),EDIPfhigh(ind),expcut)
      EDIPcutoff = max(EDIPcutoff,EDIPrmaxpair(ind))
      EDIPrmax(i) = max(EDIPrmax(i),EDIPrmaxpair(ind))
      EDIPrmax(j) = max(EDIPrmax(j),EDIPrmaxpair(ind))
    enddo
  enddo
  if (index(keyword,'verb').ne.0.and.ioproc) then
    write(ioout,'(/,''  EDIP: Accuracy tolerances: '',/)')
    write(ioout,'(''  Start of taper = '',f16.14)') EDIPaccuracy1
    write(ioout,'(''  End   of taper = '',f16.14)') EDIPaccuracy2
    write(ioout,'(/,''  EDIP: Bond order cutoffs: '',/)')
    do i = 1,nEDIPspec
      write(ioout,'(''  Species number = '',i4,f10.5)') i,EDIPrmax(i)
    enddo
    write(ioout,'(/)')
  endif
!
  return
  end
