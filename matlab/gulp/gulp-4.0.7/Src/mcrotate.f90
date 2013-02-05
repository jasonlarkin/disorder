  subroutine mcrotate(mode)
!
!  MC routine for rotation of molecules. Applies 3 random
!  rotations - one about each axis.
!
!  mode = if mode = 1, choose atoms for rotation
!         if mode = 2, then create new trial rotation
!         if mode = 3, then undo previous rotation
!
!   1/01 Created
!  11/04 Pi accessed from module
!   5/07 lmodco and connectunwrap option introduced
!   6/07 Modified to allow for a choice of different types of
!        rotations
!   6/07 Modified to keep centre of molecule in central cell
!        if nomod option is being used.
!  12/07 Nullification of pointers added
!   1/08 Saving of cell indices added in case of move not being accepted
!        Call to connectunwrap removed since this is no longer needed
!   1/08 Extra mode added to pick atoms
!   1/08 Common array now used to store pointer to trial atoms
!   1/08 random -> GULP_random
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
  use constants, only : pi
  use control,   only : lmodco
  use current
  use general
  use genetic,   only : iseed
  use molecule
  use montecarlo
  use parallel
  use reallocate
!
!  Passed variables
!
  implicit none
  integer(i4)                              :: mode
!
!  Local variables
!
  integer(i4), dimension(:), pointer, save :: nmolindsave  => null()
  integer(i4),                        save :: ntyperotate = 1
  integer(i4),                        save :: ntyperotate1
  integer(i4),                        save :: ntyperotate2
  logical,                            save :: lfirstcall = .true.
  real(dp),  dimension(:,:), pointer, save :: saverotated => null()
  real(dp),                           save :: deltax = 0.0_dp
  real(dp),                           save :: deltay = 0.0_dp
  real(dp),                           save :: deltaz = 0.0_dp
!
  integer(i4)                              :: i, ii, ierror
  integer(i4)                              :: ix, iy, iz, indm
  integer(i4)                              :: icx, icy, icz
  integer(i4)                              :: nmolrotate
  integer(i4)                              :: nrotate
  integer(i4)                              :: status
  real(dp)                                 :: dx, dy, dz
  real(dp)                                 :: dxold, dyold, dzold
  real(dp)                                 :: randnum
  real(dp)                                 :: GULP_random
  real(dp)                                 :: sinp, cosp
  real(dp)                                 :: xcrd
  real(dp)                                 :: ycrd
  real(dp)                                 :: zcrd
  real(dp),                           save :: xcent
  real(dp),                           save :: ycent
  real(dp),                           save :: zcent
  real(dp)                                 :: xoff
  real(dp)                                 :: yoff
  real(dp)                                 :: zoff
!
!  Allocate local memory on first call only - needs to preserved
!  subsequently for undoing of trial steps.
!
  if (lfirstcall) then
    lfirstcall = .false.
    allocate(nmolindsave(maxtrialatom),stat=status)
    if (status/=0) call outofmemory('mcrotate','nmolindsave')
    allocate(saverotated(3,maxtrialatom),stat=status)
    if (status/=0) call outofmemory('mcrotate','saverotated')
  endif
  if (mode.eq.3) then
!***************************
!  Mode 3 : Undo rotation  *
!***************************
    do i = 1,ntrialatom
      ii = nptrtrialatom(i)
      nmolind(ii) = nmolindsave(i)
      x0(3*ii+nstrains-2) = saverotated(1,i)
      x0(3*ii+nstrains-1) = saverotated(2,i)
      x0(3*ii+nstrains)   = saverotated(3,i)
    enddo
  elseif (mode.eq.1) then
!**********************************
!  Mode 1 : Find atoms to rotate  *
!**********************************
!
!  Choose type of rotation from those allowed
!
!  ntyperotate points to the type of rotation from 1 to nrotationtype
!
    randnum = GULP_random(iseed,1_i4)
    ntyperotate = nrotationtype*randnum + 1_i4
    if (ntyperotate.gt.nrotationtype) ntyperotate = nrotationtype
!
!  Choose molecule to rotate
!
    randnum = GULP_random(iseed,1_i4)
    nrotate = nrotateable*randnum + 1_i4
    if (nrotate.gt.nrotateable) nrotate = nrotateable
    nmolrotate = nptrrotateable(nrotate)
!
!  Build a list of all atoms to rotate
!
!  Check molecule is 0-D otherwise this is an error
!
    if (moldim(nmolrotate).gt.0) then
      call outerror('periodic molecule chosen for rotation',0_i4)
      call stopnow('mcrotate')
    endif
    ntrialatom = 0
    xcent = 0.0_dp
    ycent = 0.0_dp
    zcent = 0.0_dp
    do i = 1,nmoveable
      if (natmol(nptrmoveable(i)).eq.nmolrotate) then
!
!  Add to pointers
!
        ntrialatom = ntrialatom + 1
        if (ntrialatom.gt.maxtrialatom) then
          maxtrialatom = ntrialatom + 10
          call realloc(nmolindsave,maxtrialatom,ierror)
          if (ierror.ne.0) call outofmemory('mcrotate','nmolindsave')
          call realloc(saverotated,3_i4,maxtrialatom,ierror)
          if (ierror.ne.0) call outofmemory('mcrotate','saverotated')
        endif
        nptrtrialatom(ntrialatom) = nptrmoveable(i)
        ii = nptrtrialatom(ntrialatom)
!
!  Save coordinates for undo
!
        nmolindsave(ntrialatom) = nmolind(ii)
        saverotated(1,ntrialatom) = x0(3*ii+nstrains-2)
        saverotated(2,ntrialatom) = x0(3*ii+nstrains-1)
        saverotated(3,ntrialatom) = x0(3*ii+nstrains)
!
!  Add to running centre of mass totals
!
        xcrd = xclat(ii)
        ycrd = yclat(ii)
        zcrd = zclat(ii)
        indm = nmolind(ii)
        call mindtoijk(indm,ix,iy,iz)
        if (ix.ne.0) then
          xcrd = xcrd + ix*rv(1,1)
          ycrd = ycrd + ix*rv(2,1)
          zcrd = zcrd + ix*rv(3,1)
        endif
        if (iy.ne.0) then
          xcrd = xcrd + iy*rv(1,2)
          ycrd = ycrd + iy*rv(2,2)
          zcrd = zcrd + iy*rv(3,2)
        endif
        if (iz.ne.0) then
          xcrd = xcrd + iz*rv(1,3)
          ycrd = ycrd + iz*rv(2,3)
          zcrd = zcrd + iz*rv(3,3)
        endif
        xcent = xcent + xcrd
        ycent = ycent + ycrd
        zcent = zcent + zcrd
      endif
    enddo
!
!  If number of atoms to rotate is zero then exit now
!
    if (ntrialatom.eq.0) return
!
!  Choose point of rotation according to type of rotation
!
    if (nptrrotationtype(ntyperotate).eq.1) then
!
!  Find centre of mass
!
      xcent = xcent / dble(ntrialatom)
      ycent = ycent / dble(ntrialatom)
      zcent = zcent / dble(ntrialatom)
    elseif (nptrrotationtype(ntyperotate).eq.2) then
!
!  Choose atom to rotate about and store in ntyperotate1
!
      randnum = GULP_random(iseed,1_i4)
      ntyperotate1 = ntrialatom*randnum + 1_i4
      if (ntyperotate1.gt.ntrialatom) ntyperotate1 = ntrialatom
!
!  Find coordinates of atom for rotation
!
      ii = nptrtrialatom(ntyperotate1)
      xcent = xclat(ii)
      ycent = yclat(ii)
      zcent = zclat(ii)
      indm = nmolind(ii)
      call mindtoijk(indm,ix,iy,iz)
      if (ix.ne.0) then
        xcent = xcent + ix*rv(1,1)
        ycent = ycent + ix*rv(2,1)
        zcent = zcent + ix*rv(3,1)
      endif
      if (iy.ne.0) then
        xcent = xcent + iy*rv(1,2)
        ycent = ycent + iy*rv(2,2)
        zcent = zcent + iy*rv(3,2)
      endif
      if (iz.ne.0) then
        xcent = xcent + iz*rv(1,3)
        ycent = ycent + iz*rv(2,3)
        zcent = zcent + iz*rv(3,3)
      endif
    elseif (nptrrotationtype(ntyperotate).eq.3) then
!
!  Choose a pair of atoms to define a line to rotate about 
!
      randnum = GULP_random(iseed,1_i4)
      ntyperotate1 = ntrialatom*randnum + 1_i4
      if (ntyperotate1.gt.ntrialatom) ntyperotate1 = ntrialatom
      randnum = GULP_random(iseed,1_i4)
      ntyperotate2 = ntrialatom*randnum + 1_i4
      if (ntyperotate2.gt.ntrialatom) ntyperotate2 = ntrialatom
      call outerror('line rotation not yet implemented',0_i4)
      call stopnow('mcrotate')
    endif
  elseif (mode.eq.2) then
!*****************************
!  Mode 2 : Apply rotations  *
!*****************************
!
!  Find rotations to apply
!
    deltax = pi*rmaxmc*GULP_random(iseed,2_i4)
    deltay = pi*rmaxmc*GULP_random(iseed,2_i4)
    deltaz = pi*rmaxmc*GULP_random(iseed,2_i4)
!
!  Apply rotations to atoms in Cartesian space
!
    do i = 1,ntrialatom
      ii = nptrtrialatom(i)
      xcrd = xclat(ii)
      ycrd = yclat(ii)
      zcrd = zclat(ii)
      xoff = 0.0_dp
      yoff = 0.0_dp
      zoff = 0.0_dp
      indm = nmolind(ii)
      call mindtoijk(indm,ix,iy,iz)
      if (ix.ne.0) then
        xoff = xoff + ix*rv(1,1)
        yoff = yoff + ix*rv(2,1)
        zoff = zoff + ix*rv(3,1)
      endif
      if (iy.ne.0) then
        xoff = xoff + iy*rv(1,2)
        yoff = yoff + iy*rv(2,2)
        zoff = zoff + iy*rv(3,2)
      endif
      if (iz.ne.0) then
        xoff = xoff + iz*rv(1,3)
        yoff = yoff + iz*rv(2,3)
        zoff = zoff + iz*rv(3,3)
      endif
      dx = xcrd + xoff - xcent
      dy = ycrd + yoff - ycent
      dz = zcrd + zoff - zcent
!
!  Rotate about X
!
      sinp = sin(deltax)
      cosp = cos(deltax)
      dyold = dy
      dzold = dz
      dy = dyold*cosp - dzold*sinp
      dz = dzold*cosp + dyold*sinp
!
!  Rotate about Y
!
      sinp = sin(deltay)
      cosp = cos(deltay)
      dxold = dx
      dzold = dz
      dx = dxold*cosp - dzold*sinp
      dz = dzold*cosp + dxold*sinp
!
!  Rotate about Z
!
      sinp = sin(deltaz)
      cosp = cos(deltaz)
      dxold = dx
      dyold = dy
      dx = dxold*cosp - dyold*sinp
      dy = dyold*cosp + dxold*sinp
!
!  Return to fractional space as appropriate
!
      call cart2frac(ndim,dx-xoff+xcent,dy-yoff+ycent, &
        dz-zoff+zcent,rv,x0(3*ii+nstrains-2), &
        x0(3*ii+nstrains-1),x0(3*ii+nstrains),icx,icy,icz)
    enddo
    if (.not.lmodco) then
!
!  Find new centre of mass and move back to central cell
!
      xcent = 0.0_dp
      ycent = 0.0_dp
      zcent = 0.0_dp
      do i = 1,ntrialatom
        ii = nptrtrialatom(i)
        xcent = xcent + x0(3*ii+nstrains-2)
        ycent = ycent + x0(3*ii+nstrains-1)
        zcent = zcent + x0(3*ii+nstrains)
      enddo
      xcent = xcent / dble(ntrialatom)
      ycent = ycent / dble(ntrialatom)
      zcent = zcent / dble(ntrialatom)
!
      xoff = dmod(xcent+10.0_dp,1.0_dp) - xcent
      yoff = dmod(ycent+10.0_dp,1.0_dp) - ycent
      zoff = dmod(zcent+10.0_dp,1.0_dp) - zcent
      do i = 1,ntrialatom
        ii = nptrtrialatom(i)
        x0(3*ii+nstrains-2) = x0(3*ii+nstrains-2) + xoff
        x0(3*ii+nstrains-1) = x0(3*ii+nstrains-1) + yoff
        x0(3*ii+nstrains)   = x0(3*ii+nstrains)   + zoff
      enddo
    endif
  endif
!
  return
  end
