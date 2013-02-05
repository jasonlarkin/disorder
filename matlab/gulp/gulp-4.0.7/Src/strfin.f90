  subroutine strfin(lgrad2)
!
!  Completes strain derivatives
!
!   5/95 Modifications added to handle symmetrised second derivatives
!   6/95 Symmetry adaption of strain moved here from energy
!   6/97 Corrections for strain-strain second derivatives at non-zero
!        strain added.
!   8/98 Symmetrisation of strains moved into subroutine shared with
!        fefunct
!   4/02 Referencing of derv3 corrected to allow for frozen atoms
!   5/02 Printing for debugging moved to separate routine
!  10/04 Intent added
!  10/04 oldel option removed as no longer used
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/09 Non-radial contributions now subtracted from
!        derv3 corrections
!   1/10 Addition of rstrd to strderv corrected for case where lwolf = true
!   3/11 Copying of strderv to stresses added
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
  use control
  use current
  use derivatives
  use iochannels
  use numbers
  use optimisation, only : lopf, lfreeze
  use parallel
  use symmetry
  implicit none
!
!  Passed arguments
!
  logical, intent(in)     :: lgrad2
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: ix
  integer(i4)             :: iy
  integer(i4)             :: iz
  integer(i4)             :: j
  integer(i4)             :: nloop
  logical                 :: lopi
  real(dp)                :: xdrvi             ! Radial only component of x derivative
  real(dp)                :: ydrvi             ! Radial only component of y derivative
  real(dp)                :: zdrvi             ! Radial only component of z derivative
  real(dp)                :: rstrdr(6)         ! Radial only component of rstr
  real(dp)                :: strdervr(6)       ! Radial only component of strderv
!**************************************************
!  Symmetrisation of strain vectors and matrices  *
!**************************************************
  if (lstr.and.lsymderv) then
    call strsym
  endif
!****************************************************************
!  Add atomistic first strain derivatives to total derivatives  *
!****************************************************************
  if (lstr) then
    do i = 1,nstrains
      strderv(i) = strderv(i) + rstrd(i)
      stresses(i) = strderv(i)
    enddo
  endif
!***********************************************
!  Collect together second strain derivatives  *
!***********************************************
  if (lstr.and.lgrad2) then
!
!  Subtract non-radial components of stress
!
    do i = 1,nstrains
      rstrdr(i) = rstrd(i) - rstrdnr(i)
      strdervr(i) = strderv(i) - rstrdnr(i)
    enddo
    if (ndim.eq.3) then
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*rstrdr(1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*rstrdr(2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*rstrdr(3)
!
!  Non-zero strain corrections to elastic constants
!
      sderv2(5,1) = sderv2(5,1) + 0.5_dp*strdervr(5)
      sderv2(6,1) = sderv2(6,1) + 0.5_dp*strdervr(6)
      sderv2(4,2) = sderv2(4,2) + 0.5_dp*strdervr(4)
      sderv2(6,2) = sderv2(6,2) + 0.5_dp*strdervr(6)
      sderv2(4,3) = sderv2(4,3) + 0.5_dp*strdervr(4)
      sderv2(5,3) = sderv2(5,3) + 0.5_dp*strdervr(5)
      sderv2(4,4) = sderv2(4,4) + 0.25_dp*(strdervr(2) + strdervr(3))
      sderv2(5,5) = sderv2(5,5) + 0.25_dp*(strdervr(1) + strdervr(3))
      sderv2(6,6) = sderv2(6,6) + 0.25_dp*(strdervr(1) + strdervr(2))
      sderv2(5,4) = sderv2(5,4) + 0.25_dp*strdervr(6)
      sderv2(6,4) = sderv2(6,4) + 0.25_dp*strdervr(5)
      sderv2(6,5) = sderv2(6,5) + 0.25_dp*strdervr(4)
!
      sderv2(4,4) = sderv2(4,4) + 0.25_dp*(rstrdr(2) + rstrdr(3))
      sderv2(5,5) = sderv2(5,5) + 0.25_dp*(rstrdr(1) + rstrdr(3))
      sderv2(6,6) = sderv2(6,6) + 0.25_dp*(rstrdr(1) + rstrdr(2))
      sderv2(5,1) = sderv2(5,1) + 0.5_dp*rstrdr(5)
      sderv2(6,1) = sderv2(6,1) + 0.5_dp*rstrdr(6)
      sderv2(4,2) = sderv2(4,2) + 0.5_dp*rstrdr(4)
      sderv2(6,2) = sderv2(6,2) + 0.5_dp*rstrdr(6)
      sderv2(4,3) = sderv2(4,3) + 0.5_dp*rstrdr(4)
      sderv2(5,3) = sderv2(5,3) + 0.5_dp*rstrdr(5)
!
      sderv2(5,4) = sderv2(5,4) + 0.25_dp*rstrdr(6)
      sderv2(6,4) = sderv2(6,4) + 0.25_dp*rstrdr(5)
      sderv2(6,5) = sderv2(6,5) + 0.25_dp*rstrdr(4)
    elseif (ndim.eq.2) then
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*rstrdr(1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*rstrdr(2)
!
!  Non-zero strain corrections to second derivatives
!
      sderv2(3,1) = sderv2(3,1) + 0.5_dp*strdervr(3)
      sderv2(3,2) = sderv2(3,2) + 0.5_dp*strdervr(3)
      sderv2(3,3) = sderv2(3,3) + 0.25_dp*(strdervr(1)+strdervr(2))
!
      sderv2(3,3) = sderv2(3,3) + 0.25_dp*(rstrdr(1)+rstrdr(2))
      sderv2(3,1) = sderv2(3,1) + 0.5_dp*rstrdr(3)
      sderv2(3,2) = sderv2(3,2) + 0.5_dp*rstrdr(3)
    elseif (ndim.eq.1) then
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*rstrdr(1)
    endif
!
!  Symmetrise elastic constants
!
    do i = 1,nstrains
      do j = 1,i-1
        sderv2(j,i) = sderv2(i,j)
      enddo
    enddo
!
!  Equivalence sdrv2 and sderv2
!
    do i = 1,nstrains
      do j = 1,nstrains
        sdrv2(j,i) = sderv2(j,i)
      enddo
    enddo
!
!  Add first internal derivatives to mixed derivatives
!
    if (lsymderv2) then
      nloop = nasym
    else
      nloop = numat
    endif
    if (ndim.eq.3) then
      ix = -2
      iy = -1
      iz =  0
      do i = 1,nloop
        if (lsymderv2) then
          lopi = (.not.lfreeze.or.lopf(i))
        else
          lopi = (.not.lfreeze.or.lopf(nrelat(i)))
        endif
        if (lopi) then
          ix = ix + 3
          iy = iy + 3
          iz = iz + 3
          xdrvi = xdrv(i) - xdrvnr(i)
          ydrvi = ydrv(i) - ydrvnr(i)
          zdrvi = zdrv(i) - zdrvnr(i)
          derv3(ix,5) = derv3(ix,5) + zdrvi
          derv3(ix,6) = derv3(ix,6) + ydrvi
          derv3(iy,4) = derv3(iy,4) + zdrvi
          derv3(iy,6) = derv3(iy,6) + xdrvi
          derv3(iz,4) = derv3(iz,4) + ydrvi
          derv3(iz,5) = derv3(iz,5) + xdrvi
          derv3(ix,1) = derv3(ix,1) + 2.0_dp*xdrvi
          derv3(iy,2) = derv3(iy,2) + 2.0_dp*ydrvi
          derv3(iz,3) = derv3(iz,3) + 2.0_dp*zdrvi
        endif
      enddo
    elseif (ndim.eq.2) then
      ix = -2
      iy = -1
      do i = 1,nloop
        lopi = (.not.lfreeze.or.lopf(i))  
        if (lopi) then
          ix = ix + 3
          iy = iy + 3
          xdrvi = xdrv(i) - xdrvnr(i)
          ydrvi = ydrv(i) - ydrvnr(i)
          derv3(ix,1) = derv3(ix,1) + 2.0_dp*xdrvi
          derv3(iy,2) = derv3(iy,2) + 2.0_dp*ydrvi
          derv3(ix,3) = derv3(ix,3) + ydrvi
          derv3(iy,3) = derv3(iy,3) + xdrvi
        endif
      enddo
    elseif (ndim.eq.1) then
      ix = -2
      do i = 1,nloop
        lopi = (.not.lfreeze.or.lopf(i))  
        if (lopi) then
          ix = ix + 3
          xdrvi = xdrv(i) - xdrvnr(i)
          derv3(ix,1) = derv3(ix,1) + 2.0_dp*xdrvi
        endif
      enddo
    endif
  endif
!
  return
  end
