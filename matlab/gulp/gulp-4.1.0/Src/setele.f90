  subroutine setele(lopened)
!
!  Elemental data setup - read info from external file to allow
!  modification on site.
!
!   6/01 Initialisation of line added for benefit of some compilers
!  10/02 Style updated
!   8/06 I/O channel passed to linepro
!  12/08 Module input renamed to gulpinput
!   6/09 PDF data added
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use constants,    only : pi
  use element
  use general
  use gulpinput
  use m_pdfneutron
  implicit none
!
!  Passed variables
!
  logical, intent(out)         :: lopened
!
!  Local variables
!
  character(len=2)             :: lab
  character(len=maxlinelength) :: line
  integer(i4)                  :: i
  integer(i4)                  :: iline
  integer(i4)                  :: n
  real(dp)                     :: sigtot
  real(dp)                     :: sigcoh
!
!  Try opening eledata in current directory
!
  lopened = .false.
  lelecurrent = .false.
  lelefile = .false.
  open(7,file='eledata',status='old',err=30)
  lopened = .true.
  lelecurrent = .true.
  goto 40
!
!  Try opening eledata as pointed to by elefile
!
30 open(7,file=elefile,status='old',err=50)
  lopened = .true.
  lelefile = .false.
40 line = '  '
  read(7,'(a)',end=50) line
!
!  Read in data
!
  iline = 1
  do i = 1,maxele
    line = '  '
    read(7,'(a)',end=50) line
    iline = iline + 1
    call linepro(7_i4,line,iline)
    lab = words(1)(1:2)
    n = nint(floats(1))
    atsym(n)  = lab
    atmass(n) = floats(2)
    rcov(n)   = floats(3)
    rvdw(n)   = floats(4)
    rion(n)   = floats(5)
!
!  Read in PDF data (ers29)
!
    if (nfloat.ge.6) then                   ! floats(6) is bbar in units of fm
      bbar(n)   = floats(6) / 10.0_dp       ! units 1E-14 m (bn^1/2)
      sigcoh    = 4.0_dp*pi*bbar(n)*bbar(n) ! barn
      sigtot    = floats(7)                 ! floats(7) is sig_tot in units of barn = 100fm^2 = 1E-28
      siginc(n) = sigtot - sigcoh
      lneutronele = .true.
    endif
  enddo
50 continue
  if (lopened) close(7)
!
  return
  end
