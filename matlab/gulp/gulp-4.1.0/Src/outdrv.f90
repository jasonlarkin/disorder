  subroutine outdrv(etot,lgrad1,lgrad2)
!
!  Subroutine for outputing derivatives as a file for
!  reading by other programs.
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use current
  use derivatives
  use files
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,     intent(in) :: lgrad1
  logical,     intent(in) :: lgrad2
  real(dp),    intent(in) :: etot
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4),       save :: iout = 10
  integer(i4)             :: j
  integer(i4)             :: nloop1
  integer(i4)             :: nloop2
!
!  If name has been given then open file
!
  if (drvfile(1:1).ne.' ') then
    open(iout,file=drvfile,status='unknown')
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
  if (lgrad1) then
!**********************
!  First derivatives  *
!**********************
    if (.not.lsymderv.or.(lgrad2.and..not.lsymderv2)) then
      nloop1 = numat
    else
      nloop1 = nasym
    endif
    write(iout,'(''gradients cartesian eV/Ang '',i6)') nloop1
    do i = 1,nloop1
      write(iout,'(i6,3(1x,f15.8))') i,xdrv(i),ydrv(i),zdrv(i)
    enddo
    if (lstr) then
      write(iout,'(''gradients strain eV'')')
      write(iout,'(3(1x,f15.8))')(strderv(i),i=1,nstrains)
    endif
    if (lgrad2) then
!***********************
!  Second derivatives  *
!***********************
      if (lsymderv2) then
        nloop1 = 3*nasym
      else
        nloop1 = 3*numat
      endif
      nloop2 = 3*numat
      write(iout,'(''force_constants cart-cart eV/Ang**2'')')
      do i = 1,nloop1
        write(iout,'(3(1x,f15.8))')(derv2(j,i),j=1,nloop2)
      enddo
      if (lstr) then
        write(iout,'(''force_constants cart-strain eV/Ang'')')
        do i = 1,nloop1
          write(iout,'(3(1x,f15.8))')(derv3(i,j),j=1,nstrains)
        enddo
        write(iout,'(''force_constants strain-strain eV'')')
        do i = 1,nstrains
          write(iout,'(3(1x,f15.8))')(sderv2(j,i),j=1,nstrains)
        enddo
      endif
    endif
  endif
!
!  Close file
!
  close(iout)
!
  return
  end
