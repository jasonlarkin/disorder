  subroutine getvibmode(nobsmodeptr,maxd2,eigr,nfreqptr)
!
!  Find the vibrational mode with maximal overlap with a 
!  given set of eigenvectors stored in fobsmode
!
!   1/12 Created
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
!  Julian Gale, NRI, Curtin University, January 2012
!
  use current
  use observables, only : fobsmode, nobsmodeat
  use partial,     only : ncfoc
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: nobsmodeptr    ! Pointer to mode in fobsmode
  integer(i4), intent(in)  :: maxd2          ! Left-hand dimension of eigr array
  integer(i4), intent(out) :: nfreqptr       ! Pointer to frequency with maximal overlap
  real(dp),    intent(in)  :: eigr(maxd2,*)  ! Eigenvector array
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: ind
  integer(i4)      :: mode
  real(dp)         :: overlap_max
  real(dp)         :: overlap
!
!  Loop over modes to find best overlap
!
  nfreqptr = 0
  overlap_max = 0.0_dp
  do mode = 1,3*ncfoc
    overlap = 0.0_dp
!
!  Check that number of atoms matches
!
    if (nobsmodeat(ncf).ne.ncfoc) then
      call outerror('mismatch between number of atoms for mode in fit',0_i4)
      call stopnow('getvibmode')
    endif
    ind = 0
    do i = 1,ncfoc
      overlap = overlap + fobsmode(1,i,nobsmodeptr)*eigr(ind+1,mode) + &
                          fobsmode(2,i,nobsmodeptr)*eigr(ind+2,mode) + &
                          fobsmode(3,i,nobsmodeptr)*eigr(ind+3,mode)
      ind = ind + 3
    enddo
    if (abs(overlap).gt.overlap_max) then
      overlap_max = abs(overlap)
      nfreqptr = mode
    endif
  enddo
!
!  Check that mode was found
!
  if (nfreqptr.eq.0) then
    call outerror('no mode with greater than zero overlap found',0_i4)
    call stopnow('getvibmode')
  endif
!
  return
  end
