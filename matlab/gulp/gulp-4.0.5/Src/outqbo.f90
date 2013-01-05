  subroutine outqbo(imode,nboat,q,maxneigh,nneigh,neighno,BO)
!
!  Subroutine for outputing charges and/or bond orders
!
!  10/10 Created
!  11/10 MD time added as a comment
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
!  Julian Gale, NRI, Curtin University, November 2010
!
  use current
  use files
  use general,   only : timesofar
  use gulpinput
  use mdlogic,   only : lmd
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in) :: imode
  integer(i4),      intent(in) :: maxneigh
  integer(i4),      intent(in) :: nboat
  integer(i4),      intent(in) :: nneigh(nboat)
  integer(i4),      intent(in) :: neighno(maxneigh,nboat)
  real(dp),         intent(in) :: q(nboat)
  real(dp),         intent(in) :: BO(maxneigh,nboat)
!
!  Local variables
!
  character(len=maxlinelength) :: line
  integer(i4)                  :: i
  integer(i4),            save :: iout = 10
  integer(i4)                  :: j
  logical                      :: eof
!
!  Set up local variables
!
  iout = 24
!
!  If name has been given then open file
!
  if (qbofile(1:1).ne.' ') then
    open(iout,file=qbofile,status='unknown')
  endif
!
!  If append mode then wind to the end
!
  if (lqboappend) then
    eof = .false.
    do while (.not.eof)
      read(iout,'(a)',end=10) line
    enddo
 10 continue
  endif
!***********
!  Header  *
!***********
  if (imode.eq.1) then
    if (lmd.and.timesofar.gt.1.0d-6) then
      write(iout,'(''ReaxFF: Configuration: '',i10,'' # File written after '',f10.4,'' ps of MD'')') ncf,timesofar
    else
      write(iout,'(''ReaxFF: Configuration: '',i10)') ncf
    endif
    write(iout,'(''  Atom    Charge     Bond orders'')')
  endif
!****************************
!  Charges and bond orders  *
!****************************
  do i = 1,nboat
!
! Number of atom, charge and number of bond orders
!
    write(iout,'(i6,1x,f11.8,1x,i6)') i,q(i),nneigh(i)
    write(iout,'(20x,12(1x,i6))') (neighno(j,i),j=1,nneigh(i))
    write(iout,'(20x,6(1x,f8.6))') (BO(j,i),j=1,nneigh(i))
  enddo
!
!  Close file
!
  close(iout)
!
  return
  end
