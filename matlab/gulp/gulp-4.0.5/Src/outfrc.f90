  subroutine outfrc(etot,lgrad1,lgrad2out)
!
!  Subroutine for outputing force constants as a file for
!  interfacing to QMPOT.
!
!  lgrad2out = controls whether second derivatives should be
!              output - they will only be in the right form
!              during a phonon calculation though
!
!   3/99 Created
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use current
  use derivatives
  use files
  use shell
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,  intent(in) :: lgrad1
  logical,  intent(in) :: lgrad2out
  real(dp), intent(in) :: etot
!
!  Local variables
!
  integer(i4)          :: i
  integer(i4),    save :: iout = 10
  integer(i4)          :: j
  integer(i4)          :: n3core
  real(dp)             :: xdv
  real(dp)             :: ydv
  real(dp)             :: zdv
!
!  Set up local variables
!
  iout = 10
  n3core = 3*ncore
!
!  If name has been given then open file
!
  if (frcfile(1:1).ne.' ') then
    open(iout,file=frcfile,status='unknown')
  endif
!***********
!  Energy  *
!***********
  write(iout,'(''energy '',f30.10,'' eV'')') etot
!****************
!  Coordinates  *
!****************
  write(iout,'(''coordinates cartesian Angstroms '',i6)') numat
  do i = 1,numat
    write(iout,'(i6,1x,i3,3(1x,f15.8))') i,nat(i),xclat(i),yclat(i),zclat(i)
  enddo
!**********************
!  First derivatives  *
!**********************
  if (lgrad1) then
    write(iout,'(''gradients cartesian eV/Ang '',i6)') ncore
    do i = 1,ncore
      xdv = xdrv(i)
      ydv = ydrv(i)
      zdv = zdrv(i)
!
!  Correct for shell derivatives
!
      if (ncsptr(i).gt.0) then
        xdv = xdv + xdrv(ncsptr(i))
        ydv = ydv + ydrv(ncsptr(i))
        zdv = zdv + zdrv(ncsptr(i))
      endif
      write(iout,'(i6,3(1x,f15.8))') i,xdv,ydv,zdv
    enddo
    if (lstr) then
      write(iout,'(''gradients strain eV'')')
      write(iout,'(3(1x,f15.8))')(strderv(i),i=1,nstrains)
    endif
  endif
  if (lgrad2out) then
!***********************
!  Second derivatives  *
!***********************
    write(iout,'(''force_constants cart-cart eV/(Ang**2)'')')
    do i = 1,n3core
      write(iout,'(3(1x,f15.8))') (derv2(j,i),j=1,n3core)
    enddo
    if (lstr) then
      write(iout,'(''force_constants cart-strain eV/Ang'')')
      do i = 1,n3core
        write(iout,'(3(1x,f15.8))') (derv3(i,j),j=1,nstrains)
      enddo
      write(iout,'(''force_constants strain-strain eV'')')
      do i = 1,nstrains
        write(iout,'(3(1x,f15.8))') (sderv2(j,i),j=1,nstrains)
      enddo
    endif
  endif
!
!  Close file
!
  close(iout)
!
  return
  end
