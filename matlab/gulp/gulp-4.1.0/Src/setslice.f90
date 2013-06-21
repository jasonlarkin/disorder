  subroutine setslice(nc)
!
!  Sets the flags for the growth slice based on dhkl. It is
!  assumed that lsliceatom is initialised to false by default.
!  The fact that symmetry is not available for 2-D systems is
!  also used in the referencing of arrays to omit nrelat etc.
!
!  On entry :
!
!  nc   = configuration to reorder
!
!  11/01 Created
!  11/01 Algorithm changed to use molecule centroids and to
!        only take atoms within dhkl of highest possible
!        atom.
!   8/04 Search for slice now only performed for cores & 
!        shells set to match the associated core.
!   8/04 Extra checking loop added if growth slice is not
!        charge neutral.
!   8/04 Bug in initialisation of zmax for case where molecules
!        are present has been fixed
!   9/04 Cores only used to define the molecule centroids
!   7/05 Memory deallocation cleaned
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
  use control
  use configurations
  use current
  use molecule
  use shell,         only : ncore, ncoptr, ncsptr
  implicit none
!
!  Passed arguments
!
  integer(i4)                                  :: nc
!
!  Local arrays
!
  integer(i4), dimension(:), allocatable       :: ncentroid
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ni
  integer(i4)                                  :: nreg1
  integer(i4)                                  :: nreg2
  integer(i4)                                  :: ntry 
  integer(i4)                                  :: status 
  real(dp),    dimension(:), allocatable       :: zcentroid
  real(dp)                                     :: dhkl
  real(dp),                               save :: dhkltol = 1.0d-4
  real(dp)                                     :: dif
  real(dp)                                     :: qtot
  real(dp)                                     :: zi
  real(dp)                                     :: zmax
  real(dp)                                     :: zone
  real(dp)                                     :: zreg1
  real(dp)                                     :: zreg2
!
!  Get number of atoms for this configuration
!
  nasym = nascfg(nc)
!
!  Set local variables
!
  dhkl = dhklcfg(nc) - dhkltol
  ntry = 0
!
!  Allocate local memory
!
  if (nmol.gt.0) then
    allocate(ncentroid(nmol),stat=status)
    if (status/=0) call outofmemory('setslice','ncentroid')
    allocate(zcentroid(nmol),stat=status)
    if (status/=0) call outofmemory('setslice','zcentroid')
!
!  Find centroids of molecules. No need to worry about xy wrap around since
!  only the z coordinate is needed.
!
    do i = 1,nmol
      ncentroid(i) = 0
      zcentroid(i) = 0.0_dp
    enddo
    do i = 1,ncore
      ii = ncoptr(i)
      ni = natmol(ii)
      if (ni.gt.0) then
        ncentroid(ni) = ncentroid(ni) + 1
        zcentroid(ni) = zcentroid(ni) + zclat(ii)
      endif
    enddo
    do i = 1,nmol
      if (ncentroid(i).gt.0) then
        zcentroid(i) = zcentroid(i) / dble(ncentroid(i))
      endif
    enddo
  endif
!
!  Initialise flag for whether atoms are in slice or have been found already
!  Also find average z coordinate of regions to get sense in z direction
!
  nreg1 = 0
  nreg2 = 0
  zreg1 = 0.0_dp
  zreg2 = 0.0_dp
  do i = 1,nasym
    if (nregionno(nsft + i).eq.1) then
      nreg1 = nreg1 + 1
      zreg1 = zreg1 + zalat(i)
    else
      nreg2 = nreg2 + 1
      zreg2 = zreg2 + zalat(i)
    endif
  enddo
  zreg1 = zreg1/dble(nreg1)
  zreg2 = zreg2/dble(nreg2)
  zreg1 = zreg1 - zreg2
  if (abs(zreg1).gt.1.0d-6) then
    zone = zreg1/abs(zreg1)
  else
    zone = 1.0_dp
  endif
!
!  Find atom at highest point of surface
!
  zmax = - 1.0d12
  do i = 1,ncore
    ii = ncoptr(i)
    if (nregionno(nsft + ii).eq.1) then
      if (nmol.gt.0) then
        if (natmol(ii).gt.0) then
          zi = zcentroid(natmol(ii))
        else
          zi = zalat(ii)
        endif
      else
        zi = zalat(ii)
      endif
      dif = zi*zone
      if (dif.gt.zmax) then
        zmax = dif
      endif
    endif
  enddo
!
!  Search for cores that are lower than surface equivalent by more less dhkl
!  If atom is found above the present one, then it is not in the slice
!
10 continue
  do i = 1,ncore
    ii = ncoptr(i)
    if (nregionno(nsft + ii).eq.1) then
      if (nmol.gt.0) then
        if (natmol(ii).gt.0) then
          zi = zcentroid(natmol(ii))
        else
          zi = zalat(ii)
        endif
      else
        zi = zalat(ii)
      endif
      dif = abs(zi - zmax)
      if (dif.lt.dhkl) then
        lsliceatom(nsft + ii) = .true.
      else
        lsliceatom(nsft + ii) = .false.
      endif
      if (ncsptr(ii).gt.0) then
        lsliceatom(nsft + ncsptr(ii)) = lsliceatom(nsft + ii)
      endif
    endif
  enddo
!
!  Check charge neutrality of growth slice
!
  qtot = 0.0_dp
  do i = 1,nasym
    if (lsliceatom(nsft + i)) then
      qtot = qtot + qa(i)
    endif
  enddo
  if (abs(qtot).gt.1.0d-4) then
    if (ntry.lt.2) then
      ntry = ntry + 1
      dhkl = dhkl + dhkltol
      goto 10
    else
      call outwarning('Growth slice is not charge neutral',0_i4)
    endif
  endif
!
!  Free local memory
!
  if (nmol.gt.0) then
    deallocate(zcentroid,stat=status)
    if (status/=0) call deallocate_error('setslice','zcentroid')
    deallocate(ncentroid,stat=status)
    if (status/=0) call deallocate_error('setslice','ncentroid')
  endif
!
  return
  end
