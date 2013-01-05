  subroutine x0tostrcentroid
!
!  Subroutine to convert linear structure array to main structure arrays.
!  Monte Carlo version that allows strains to be applied based on the 
!  centroids of molecules, rather than individual coordinates.
!
!   5/07 Created from x0tostr
!   6/07 Updating of fractional coordinates added at end of routine
!  12/07 rv copied to rvsave which was uninitialised
!   6/09 Module name changed from three to m_three
!  10/11 Call to cart2frac modified by adding cell indices
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
!  Julian Gale, NRI, Curtin University, October 2011
!
  use configurations
  use current
  use four
  use m_three
  use molecule,       only : natmol, nmolind, nmol
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4)              :: i
  integer(i4)              :: icx
  integer(i4)              :: icy
  integer(i4)              :: icz
  integer(i4)              :: indm
  integer(i4)              :: j
  integer(i4)              :: jj
  integer(i4)              :: jx
  integer(i4)              :: jy
  integer(i4)              :: jz
  integer(i4)              :: mvar
  integer(i4)              :: ninc
  integer(i4), allocatable :: nmlist(:)
  integer(i4)              :: nr
  integer(i4)              :: status
  real(dp)                 :: deltax
  real(dp)                 :: deltay
  real(dp)                 :: deltaz
  real(dp)                 :: rvsave(3,3)
  real(dp)                 :: xccent
  real(dp)                 :: yccent
  real(dp)                 :: zccent
  real(dp)                 :: xccentnew
  real(dp)                 :: yccentnew
  real(dp)                 :: zccentnew
  real(dp)                 :: xcrd
  real(dp)                 :: ycrd
  real(dp)                 :: zcrd
  real(dp)                 :: xfcent
  real(dp)                 :: yfcent
  real(dp)                 :: zfcent
  real(dp)                 :: xstr(6)
  real(dp),    allocatable :: xsave(:)
  real(dp),    allocatable :: ysave(:)
  real(dp),    allocatable :: zsave(:)
!
!  Allocate local workspace
!
  allocate(nmlist(numat),stat=status)
  if (status/=0) call outofmemory('mc_x0tostr','nmlist')
  allocate(xsave(numat),stat=status)
  if (status/=0) call outofmemory('mc_x0tostr','xsave')
  allocate(ysave(numat),stat=status)
  if (status/=0) call outofmemory('mc_x0tostr','ysave')
  allocate(zsave(numat),stat=status)
  if (status/=0) call outofmemory('mc_x0tostr','zsave')
!
!  Store the old Cartesian coordinates for use in preserving molecular geometries
!
  xsave(1:numat) = xclat(1:numat)
  ysave(1:numat) = yclat(1:numat)
  zsave(1:numat) = zclat(1:numat)
!
!  Copy cell vectors to rvsave
!
  rvsave(1:3,1:3) = rv(1:3,1:3)
!
!  Find old cell by removing strains
!
  do i = 1,nstrains
    xstr(i) = x0(i) - 1.0_dp
    xstr(i) = 1.0_dp/(1.0_dp + xstr(i))
  enddo
  if (ndim.eq.3) then
    call strain3D(xstr,rvsave)
  elseif (ndim.eq.2) then
    call strain2D(xstr,rvsave)
  elseif (ndim.eq.1) then
    call strain1D(xstr,rvsave)
  endif
!
  if (ndim.eq.3) then
    call uncell3D(rv,a,b,c,alpha,beta,gamma)
    if (a.gt.1.0d-12) then
      recipa = 1.0_dp/a
    else
      recipa = 0.0_dp
    endif
    if (b.gt.1.0d-12) then
      recipb = 1.0_dp/b
    else
      recipb = 0.0_dp
    endif
    if (c.gt.1.0d-12) then
      recipc = 1.0_dp/c
    else
      recipc = 0.0_dp
    endif
    r1x = rv(1,1)
    r1y = rv(2,1)
    r1z = rv(3,1)
    r2x = rv(1,2)
    r2y = rv(2,2)
    r2z = rv(3,2)
    r3x = rv(1,3)
    r3y = rv(2,3)
    r3z = rv(3,3)
    call rlist
  elseif (ndim.eq.2) then
    call uncell2D(rv,a,b,alpha)
    if (a.gt.1.0d-12) then
      recipa = 1.0_dp/a
    else
      recipa = 0.0_dp
    endif
    if (b.gt.1.0d-12) then
      recipb = 1.0_dp/b
    else
      recipb = 0.0_dp
    endif
    r1x = rv(1,1)
    r1y = rv(2,1)
    r2x = rv(1,2)
    r2y = rv(2,2)
    call rlist
  elseif (ndim.eq.1) then
    call uncell1D(rv,a)
    if (a.gt.1.0d-12) then
      recipa = 1.0_dp/a
    else
      recipa = 0.0_dp
    endif
    r1x = rv(1,1)
    call rlist
  endif
  mvar = 3*nasym + nstrains
!*************************************
!  Substitute parameters into place  *
!*************************************
!
!  Asymmetric unit coordinates
!
  do i = 1,nasym
    xafrac(i) = x0(3*i+nstrains-2)
    yafrac(i) = x0(3*i+nstrains-1)
    zafrac(i) = x0(3*i+nstrains)
  enddo
!
!  Radii
!
  if (nbsmat.gt.0) then
    do i = 1,nasym
      rada(i) = x0(mvar+i)
    enddo
  endif
!
!  Generate full coordinate set
!
  if (lsymopt) then
    call equpos(.true.,.false.)
  else
    do i = 1,numat
      xfrac(i) = xafrac(i)
      yfrac(i) = yafrac(i)
      zfrac(i) = zafrac(i)
    enddo
    if (nbsmat.gt.0) then
      do i = 1,numat
        radf(i) = rada(i)
      enddo
    endif
  endif
!**********************
!  Molecule handling  *
!**********************
!
!  Loop over molecules
!
  do i = 1,nmol
!
!  Find atoms in molecule
!
    ninc = 0
    do j = 1,numat
      if (natmol(j).eq.i) then
        ninc = ninc + 1
        nmlist(ninc) = j
      endif
    enddo
!
!  Find centroid of molecule in fractional and Cartesian forms
!
    xccent = 0.0_dp
    yccent = 0.0_dp
    zccent = 0.0_dp
    xfcent = 0.0_dp
    yfcent = 0.0_dp
    zfcent = 0.0_dp
    do jj = 1,ninc
      j = nmlist(jj)
      indm = nmolind(j)
      call mindtoijk(indm,jx,jy,jz)
!
!  Fractional centroid of new molecule position
!
      xfcent = xfcent + xfrac(j) + jx
      yfcent = yfcent + yfrac(j) + jy
      zfcent = zfcent + zfrac(j) + jz
!
!  Cartesian centroid of old molecule position
!
      xcrd = xsave(j)
      ycrd = ysave(j)
      zcrd = zsave(j)
      if (jx.ne.0) then
        xcrd = xcrd + jx*rvsave(1,1)
        ycrd = ycrd + jx*rvsave(2,1)
        zcrd = zcrd + jx*rvsave(3,1)
      endif
      if (jy.ne.0) then
        xcrd = xcrd + jy*rvsave(1,2)
        ycrd = ycrd + jy*rvsave(2,2)
        zcrd = zcrd + jy*rvsave(3,2)
      endif
      if (jz.ne.0) then
        xcrd = xcrd + jz*rvsave(1,3)
        ycrd = ycrd + jz*rvsave(2,3)
        zcrd = zcrd + jz*rvsave(3,3)
      endif
      xccent = xccent + xcrd
      yccent = yccent + ycrd
      zccent = zccent + zcrd
    enddo
    xccent = xccent/dble(ninc)
    yccent = yccent/dble(ninc)
    zccent = zccent/dble(ninc)
    xfcent = xfcent/dble(ninc)
    yfcent = yfcent/dble(ninc)
    zfcent = zfcent/dble(ninc)
!
!  Compute new Cartesian position of centroid
!
    if (ndim.eq.3) then
      xccentnew = xfcent*r1x + yfcent*r2x + zfcent*r3x
      yccentnew = xfcent*r1y + yfcent*r2y + zfcent*r3y
      zccentnew = xfcent*r1z + yfcent*r2z + zfcent*r3z
    elseif (ndim.eq.2) then
      xccentnew = xfcent*r1x + yfcent*r2x
      yccentnew = xfcent*r1y + yfcent*r2y
      zccentnew = zfcent
    elseif (ndim.eq.1) then
      xccentnew = xfcent*r1x
      yccentnew = yfcent
      zccentnew = zfcent
    else
      xccentnew = xfcent
      yccentnew = yfcent
      zccentnew = zfcent
    endif
!
!  Create fractional coordinates for atoms based on Cartesian displacement relative to centroid
!
    do jj = 1,ninc
      j = nmlist(jj)
      indm = nmolind(j)
      call mindtoijk(indm,jx,jy,jz)
!
!  Generate old Cartesian coordinate
!
      xcrd = xsave(j)
      ycrd = ysave(j)
      zcrd = zsave(j)
      if (jx.ne.0) then
        xcrd = xcrd + jx*rvsave(1,1)
        ycrd = ycrd + jx*rvsave(2,1)
        zcrd = zcrd + jx*rvsave(3,1)
      endif
      if (jy.ne.0) then
        xcrd = xcrd + jy*rvsave(1,2)
        ycrd = ycrd + jy*rvsave(2,2)
        zcrd = zcrd + jy*rvsave(3,2)
      endif
      if (jz.ne.0) then
        xcrd = xcrd + jz*rvsave(1,3)
        ycrd = ycrd + jz*rvsave(2,3)
        zcrd = zcrd + jz*rvsave(3,3)
      endif
!
!  Compute Cartesian position of atom relative to old centroid
!
      deltax = xcrd - xccent
      deltay = ycrd - yccent
      deltaz = zcrd - zccent
!
!  Compute new Cartesian position of atom based on new centroid position
!
      xcrd = xccentnew + deltax
      ycrd = yccentnew + deltay
      zcrd = zccentnew + deltaz
!
!  Convert new Cartesian position back to fractional
!
      call cart2frac(ndim,xcrd,ycrd,zcrd,rv,xfrac(j),yfrac(j),zfrac(j),icx,icy,icz)
    enddo
  enddo
!
!  Having created new fractional coordinates for full cell, populate asymmetric unit values
!
  do i = 1,nasym
    xafrac(i) = xfrac(nrel2(i))
    yafrac(i) = yfrac(nrel2(i))
    zafrac(i) = zfrac(nrel2(i))
  enddo
!********************************************************************************
!  Convert cell parameters and internal coordinates into cartesian coordinates  *
!********************************************************************************
  if (ndim.eq.3) then
    do i = 1,numat
      xclat(i) = xfrac(i)*r1x + yfrac(i)*r2x + zfrac(i)*r3x
      yclat(i) = xfrac(i)*r1y + yfrac(i)*r2y + zfrac(i)*r3y
      zclat(i) = xfrac(i)*r1z + yfrac(i)*r2z + zfrac(i)*r3z
    enddo
  elseif (ndim.eq.2) then
    do i = 1,numat
      xclat(i) = xfrac(i)*r1x + yfrac(i)*r2x
      yclat(i) = xfrac(i)*r1y + yfrac(i)*r2y
      zclat(i) = zfrac(i)
    enddo
  elseif (ndim.eq.1) then
    do i = 1,numat
      xclat(i) = xfrac(i)*r1x
      yclat(i) = yfrac(i)
      zclat(i) = zfrac(i)
    enddo
  else
    do i = 1,numat
      xclat(i) = xfrac(i)
      yclat(i) = yfrac(i)
      zclat(i) = zfrac(i)
    enddo
  endif
  if (lsymopt) then
    do i = 1,nasym
      nr = nrel2(i)
      xalat(i) = xclat(nr)
      yalat(i) = yclat(nr)
      zalat(i) = zclat(nr)
    enddo
  else
    do i = 1,nasym
      xalat(i) = xclat(i)
      yalat(i) = yclat(i)
      zalat(i) = zclat(i)
    enddo
  endif
!
!  Return to modifed Cartesian coordinates to fractional arrays
!
  do i = 1,numat
    call cart2frac(ndim,xclat(i),yclat(i),zclat(i),rv,x0(3*i+nstrains-2),x0(3*i+nstrains-1),x0(3*i+nstrains),icx,icy,icz)
  enddo
!
!  Deallocate local workspace
!
  deallocate(zsave,stat=status)
  if (status/=0) call deallocate_error('mc_x0tostr','zsave')
  deallocate(ysave,stat=status)
  if (status/=0) call deallocate_error('mc_x0tostr','ysave')
  deallocate(xsave,stat=status)
  if (status/=0) call deallocate_error('mc_x0tostr','xsave')
  deallocate(nmlist,stat=status)
  if (status/=0) call deallocate_error('mc_x0tostr','nmlist')
!
  return
  end
