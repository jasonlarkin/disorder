  subroutine outcell
!
!  Output of cell parameters
!
!   5/07 Created from modifications to optout
!   1/08 Unused variables removed
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
!  Copyright Curtin University, 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use configurations
  use constants
  use control
  use current
  use element
  use iochannels
  use parallel
  use terse,         only : lterseoutcell
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
!
  if (.not.lconv) then
    if (ndim.eq.3) then
      call uncell3D(rv,a,b,c,alpha,beta,gamma)
      if (ioproc) then
        if (.not.lterseoutcell) then
          write(ioout,'(''  Final Cartesian lattice vectors (Angstroms) :'',/)')
          do i = 1,3
            write(ioout,'(4x,3f12.6)')(rv(j,i),j=1,3)
          enddo
          write(ioout,'(/)')
        endif
        write(ioout,'(''  Final cell parameters :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''       a      '',f14.6,'' Angstrom '')') a
        write(ioout,'(''       b      '',f14.6,'' Angstrom '')') b
        write(ioout,'(''       c      '',f14.6,'' Angstrom '')') c
        write(ioout,'(''       alpha  '',f14.6,'' Degrees  '')') alpha
        write(ioout,'(''       beta   '',f14.6,'' Degrees  '')') beta
        write(ioout,'(''       gamma  '',f14.6,'' Degrees  '')') gamma
        write(ioout,'(''--------------------------------------------------------------------------------'',/)')
      endif
    elseif (ndim.eq.2) then
      if (.not.lterseoutcell.and.ioproc) then
        write(ioout,'(''  Final Cartesian surface vectors (Angstroms) :'',/)')
        do i = 1,2
          write(ioout,'(4x,3f12.6)')(rv(j,i),j=1,3)
        enddo
        write(ioout,'(/)')
      endif
      call uncell2D(rv,a,b,alpha)
      if (ioproc) then
        write(ioout,'(''  Final surface cell parameters :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''       a      '',f14.6,'' Angstrom '')') a
        write(ioout,'(''       b      '',f14.6,'' Angstrom '')') b
        write(ioout,'(''       alpha  '',f14.6,'' Degrees  '')') alpha
        write(ioout,'(''--------------------------------------------------------------------------------'',/)')
      endif
    elseif (ndim.eq.1) then
      if (.not.lterseoutcell.and.ioproc) then
        write(ioout,'(''  Final Cartesian polymer vector (Angstroms) :'',/)')
        write(ioout,'(4x,3f12.6)')(rv(j,1),j=1,3)
        write(ioout,'(/)')
      endif
      call uncell1D(rv,a)
      if (ioproc) then
        write(ioout,'(''  Final polymer cell parameter :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''       a      '',f14.6,'' Angstrom '')') a
        write(ioout,'(''--------------------------------------------------------------------------------'',/)')
      endif
    endif
  endif
!
  return
  end
