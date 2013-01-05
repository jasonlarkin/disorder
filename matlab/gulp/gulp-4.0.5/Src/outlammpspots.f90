  subroutine outlammpspots(iout)
!
!  Subroutine for outputing potentials as tables for Lammps
!
!   3/11 Created
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
!  Julian Gale, NRI, Curtin University, March 2011
!
  use constants,      only : angstoev
  use current
  use files
  use potentialnames, only : namepot
  use two
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in) :: iout
!
!  Local variables
!
  character(len=20)            :: unique_label
  character(len=5)             :: label1
  character(len=5)             :: label2
  integer(i4)                  :: ind
  integer(i4)                  :: itype1
  integer(i4)                  :: itype2
  integer(i4)                  :: n
  integer(i4)                  :: nat1
  integer(i4)                  :: nat2
  integer(i4)                  :: nlen
  integer(i4)                  :: np
  integer(i4)                  :: npotl(1)
  real(dp)                     :: cut2r
  real(dp)                     :: d0i
  real(dp)                     :: d0j
  real(dp)                     :: d1i
  real(dp)                     :: d1j
  real(dp)                     :: d2i2
  real(dp)                     :: d2ij
  real(dp)                     :: d2j2
  real(dp)                     :: deriv
  real(dp)                     :: deriv2
  real(dp)                     :: deriv3
  real(dp)                     :: derive
  real(dp)                     :: derive0
  real(dp)                     :: derive2
  real(dp)                     :: derive3
  real(dp)                     :: dr
  real(dp)                     :: eatom
  real(dp)                     :: ereal
  real(dp)                     :: ec6
  real(dp)                     :: r
  real(dp)                     :: rderiv
  real(dp)                     :: rtrm
  real(dp)                     :: rtrm2
  real(dp)                     :: rtrm3
  real(dp)                     :: rtrm32
  real(dp)                     :: sctrm1
  real(dp)                     :: sctrm2
  real(dp)                     :: x1
  real(dp)                     :: y1
  real(dp)                     :: z1
!
!  If name has been given then open file
!
  if (lammpspotsfile(1:1).ne.' ') then
    open(iout,file=lammpspotsfile,status='unknown')
  endif
!
!  Compute dr
!
  dr = (lammps_rend - lammps_r0)/dble(nlammpspoints)
!***********
!  Header  *
!***********
  write(iout,'("# LAMMPS TABLE FILE")')
!*****************
!  Body of file  *
!*****************
!
!  Loop over twobody potentials
!
  do n = 1,npote
!
!  Create label for potential
!
    unique_label = ' '
    ind = index(namepot(nptype(n)),' ')
    nlen = ind - 1
    unique_label(1:nlen) = namepot(nptype(n))(1:nlen)
    nlen = nlen + 1
    unique_label(nlen:nlen) = '_'
!
    nat1 = nspec1(n)
    itype1 = nptyp1(n)
    call label(nat1,itype1,label1)
    ind = index(label1,' ')
    unique_label(nlen+1:nlen+ind) = label1(1:ind-1)
    nlen = nlen + ind 
    unique_label(nlen:nlen) = '_'
!
    nat2 = nspec2(n)
    itype2 = nptyp2(n)
    call label(nat2,itype2,label2)
    ind = index(label2,' ')
    unique_label(nlen+1:nlen+ind) = label2(1:ind-1)
    nlen = nlen + ind - 1
!
!  Write header for potential
!
    write(iout,*)
    write(iout,'(a20)') unique_label
    write(iout,'("N ",i10," R ",2f10.5)') nlammpspoints+1,lammps_r0,lammps_rend
    write(iout,*)
!
!  Set overall variables for twobody1 call
!
    y1    = 0.0_dp
    z1    = 0.0_dp
    npotl(1) = n
    cut2r = min(cutp,rpot(n))
    cut2r = cut2r**2
!
!  Loop over points 
!
    r = lammps_r0 - dr
    do np = 1,nlammpspoints
!
!  Increment distance
!
      r = r + dr
!
!  Initialise values for call
!
      eatom = 0.0_dp
      ereal = 0.0_dp
      ec6   = 0.0_dp
      x1    = r
!
!  Call twobodymd to get potential value
!
      call twobody1(eatom,ereal,ec6,.true.,.false.,.false.,1_i4,1_i4,r,x1,y1,z1,d0i,d0j,deriv, &
                    deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,1_i4,npotl,cut2r,0.0_dp, &
                    0.0_dp,.false.,0_i4,angstoev,1.0_dp,0.0_dp,rtrm,rtrm2,rtrm3,rtrm32,sctrm1, &
                    sctrm2,0.0_dp,0.0_dp,.false.,.false.,.false.,.false.,.false.,.false.,0_i4, &
                    0_i4,.false.,.true.,.true.,d1i,d1j,d2i2,d2ij,d2j2)
!
!  Write out result - note that table requires force, not derivative, and so sign of deriv is changed
!
      write(iout,'(i10,2x,3(g20.10,2x))') np,r,eatom,-deriv
!
!  End of loop over points
!
    enddo
!
!  Write out final line with zero energy / force
!
    np = nlammpspoints + 1
    r = r + dr
    eatom = 0.0_dp
    deriv = 0.0_dp
    write(iout,'(i10,2x,3(g20.10,2x))') np,r,eatom,deriv
  enddo
!
!  Close file
!
  close(iout)
!
  return
  end
