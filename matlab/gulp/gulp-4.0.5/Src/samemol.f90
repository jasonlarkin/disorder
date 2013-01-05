  subroutine samemol(lsamemol,nmi,ii,jj,kk,ix,iy,iz)
!
!  Determines whether two atoms belong to the same molecule
!  based on the cell indices and the dimensionality of the
!  molecule. It is assumed that the equivalence of nmi and
!  nmj has been check prior to the call of this routine.
!
!  lsamemol = if .true. then atoms are in same molecule
!  ii,jj,kk = current indices of cell vector shifts
!  ix,iy,iz = differences of nmolind component values
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, November 2004
!
  use molecule
!
!  Passed variables
!
  integer(i4), intent(in)  :: ii
  integer(i4), intent(in)  :: ix
  integer(i4), intent(in)  :: iy
  integer(i4), intent(in)  :: iz
  integer(i4), intent(in)  :: jj
  integer(i4), intent(in)  :: kk
  integer(i4), intent(in)  :: nmi
  logical,     intent(out) :: lsamemol
!
!  Local variables
!
  integer(i4)              :: mold
!
  mold = moldim(nmi)
  if (mold.eq.3) then
!
!  3D case
!
    lsamemol = .true.
  elseif (mold.eq.0) then
!
!  0D case - discrete molecules
!
    lsamemol = (ii.eq.ix.and.jj.eq.iy.and.kk.eq.iz)
  else
    moldi = moldimi(nmi)
    if (mold.eq.1) then
!
!  1D case
!
      if (moldi.eq.1) then
        lsamemol = (jj.eq.iy.and.kk.eq.iz)
      elseif (moldi.eq.2) then
        lsamemol = (ii.eq.ix.and.kk.eq.iz)
      else
        lsamemol = (ii.eq.ix.and.jj.eq.iy)
      endif
    else
!
!  2D case
!
      if (moldi.eq.1) then
        lsamemol = (kk.eq.iz)
      elseif (moldi.eq.2) then
        lsamemol = (jj.eq.iy)
      else
        lsamemol = (ii.eq.ix)
      endif
    endif
  endif
!
  return
  end
