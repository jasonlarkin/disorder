  subroutine symderv2(lgrad2)
!
!  Subroutine for symmetrising derv2 matrix. Potential terms need only be calculated for
!  the upper triangle and here they are transposed to the lower triangle. This shouldn't
!  be done for a symmetry-adapted second derivative calculation.
!
!  10/04 Created from part of reale.f where this task was performed
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
  use control,      only : lnoreal
  use current
  use derivatives,  only : derv2, sderv2
  use optimisation, only : lopf, lfreeze
  use symmetry,     only : lstr, lsymderv2
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                        :: lgrad2
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ix
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jx
  integer(i4)                                    :: jy
  integer(i4)                                    :: jz
  integer(i4)                                    :: maxlim
  integer(i4)                                    :: nff
!
!  If not a second derivative calculation then return as there is nothing to do
!
  if (.not.lgrad2) return
!
!  If this is a symmetrised second derivative calculation then don't transpose
!
  if (lsymderv2) return
!
!  Find number of unfrozen atoms
!
  if (lfreeze) then
    nff = 0
    do i = 1,nasym
      if (lopf(i)) then
        nff = nff + neqv(i)
      endif
    enddo
  else
    nff = numat
  endif
!
!  If freezing is being used then we must not symmetrise the on diagonal block.
!
  if (lfreeze) then
    ix = 1
    iy = 2
    iz = 3
    do i = 2,nff
      ix = ix + 3
      iy = iy + 3
      iz = iz + 3
      jx = - 2
      jy = - 1
      jz =   0
      do j = 1,i-1
        jx = jx + 3
        jy = jy + 3
        jz = jz + 3
        derv2(ix,jx) = derv2(jx,ix)
        derv2(iy,jx) = derv2(jx,iy)
        derv2(iz,jx) = derv2(jx,iz)
        derv2(ix,jy) = derv2(jy,ix)
        derv2(iy,jy) = derv2(jy,iy)
        derv2(iz,jy) = derv2(jy,iz)
        derv2(ix,jz) = derv2(jz,ix)
        derv2(iy,jz) = derv2(jz,iy)
        derv2(iz,jz) = derv2(jz,iz)
      enddo
    enddo
    if (nbsm.gt.0) then
      ix = 3*nff
      do i = 1,nff
        ix = ix + 1
        jx = - 2
        jy = - 1
        jz =   0
        do j = 1,nff
          jx = jx + 3
          jy = jy + 3
          jz = jz + 3
          derv2(ix,jx) = derv2(jx,ix)
          derv2(ix,jy) = derv2(jy,ix)
          derv2(ix,jz) = derv2(jz,ix)
        enddo
      enddo
    endif
  else
    if (lnoreal) then
      maxlim = 3*numat
      if (nbsmat.gt.0) maxlim = maxlim + numat
    else
      maxlim = 3*nff
      if (nbsmat.gt.0) maxlim = maxlim + nff
    endif
    do i = 2,maxlim
      do j = 1,i-1
        derv2(i,j) = derv2(j,i)
      enddo
    enddo
  endif
!
!  Strain second derivatives
!
  if (lstr) then
    do i = 2,nstrains
      do j = 1,i-1
        sderv2(j,i) = sderv2(i,j)
      enddo
    enddo
  endif
!
  return
  end
