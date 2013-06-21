  subroutine cutscheck
!
!  Subroutine for checking that each shell is still within cuts
!  of its core
!
!   5/11 Created from cscheck
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, May 2011
!
  use current
  use iochannels
  use parallel
  use shell
  implicit none
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: isp
  integer(i4)                                  :: j
  logical                                      :: lanynotfound
  logical                                      :: lfound
  real(dp)                                     :: cut2
  real(dp)                                     :: largestr2
  real(dp)                                     :: r2
  real(dp)                                     :: smallestr2
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
!  If there are no shells give up now!
!
  if (nshell.eq.0) return
!
  cut2 = cuts*cuts
  largestr2 = 0.0_dp    ! This is the largest distance squared between a core and shell beyond cuts**2
  lanynotfound = .false.
!
!  Loop over shells
!
  do i = 1,nshell
    isp = nshptr(i)
    xcd = xclat(isp)
    ycd = yclat(isp)
    zcd = zclat(isp)
    lfound = .false.
    j = ncsptr(isp)
    xcdi = xclat(j) - xcd
    ycdi = yclat(j) - ycd
    zcdi = zclat(j) - zcd
!
!  Loop over unit cells
!
    ii = 0
    smallestr2 = 100000.0_dp    ! This is the smallest distance squared beyond cut2
    do while (ii.lt.iimax.and..not.lfound)
      ii = ii + 1
      xcrd = xcdi + xvec1cell(ii)
      ycrd = ycdi + yvec1cell(ii)
      zcrd = zcdi + zvec1cell(ii)
      r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      if (r2.le.cut2) then
        lfound = .true.
      elseif (r2.lt.smallestr2) then
        smallestr2 = r2
      endif
!
!  End of loop over cell vectors
!
    enddo
    if (.not.lfound) then
      lanynotfound = .true.
      largestr2 = max(largestr2,smallestr2)
    endif
!
!  End loop over shells
!
  enddo
!
!  If core-shell distances have been found that are beyond the range of cuts then output a warning
!
  if (lanynotfound) then
    if (ioproc) then
      call outerror('Largest core-shell distance exceeds cutoff of cuts',0_i4)
      write(ioout,'(/,''  Largest core-shell distance = '',f8.4,'' Angstroms'',/)') sqrt(largestr2)
    endif
    call stopnow('cutscheck')
  endif
!
  return
  end
