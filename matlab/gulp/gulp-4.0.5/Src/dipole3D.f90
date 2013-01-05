  subroutine dipole3D(edipole,lgrad1,lgrad2)
!
!  Subroutine for dipole correction energy
!
!   6/97 Dipoles for molecular systems sorted
!   6/97 Derivatives added.
!   7/97 Calculation of virial added
!   2/01 Modifications for general dimensionality added
!  11/02 Parallel mods added
!   2/07 Warning for non-charge neutral added
!   2/07 Partial occupancy handling added
!   3/07 Name changed from dipole to dipole3D for Chemshell compatibility
!  11/09 Region derivatives added
!   4/12 Explicit virial calculation removed as no longer needed
!   5/12 Atomic stress added
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use configurations, only : nregionno
  use constants
  use control,        only : latomicstress
  use current
  use derivatives
  use mdlogic
  use molecule
  use optimisation
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,     intent(in)    :: lgrad1
  logical,     intent(in)    :: lgrad2
  real(dp),    intent(out)   :: edipole
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: indm
  integer(i4)                :: ix
  integer(i4)                :: iy
  integer(i4)                :: iz
  integer(i4)                :: j
  integer(i4)                :: jx
  integer(i4)                :: jy
  integer(i4)                :: jz
  integer(i4)                :: kl
  integer(i4)                :: nmi
  integer(i4)                :: nregioni
  logical                    :: lopi
  logical                    :: lopj
  real(dp)                   :: as_shift
  real(dp)                   :: const
  real(dp)                   :: dip2
  real(dp)                   :: dtrm1
  real(dp)                   :: dtrm2
  real(dp)                   :: dx
  real(dp)                   :: dy
  real(dp)                   :: dz
  real(dp)                   :: q
  real(dp)                   :: qi
  real(dp)                   :: rvol
  real(dp)                   :: vol
  real(dp)                   :: volume
  real(dp)                   :: xi
  real(dp)                   :: yi
  real(dp)                   :: zi
!
!  Check dimensionality
!
  if (ndim.eq.2) then
    call outerror('dipole correction not available for surface',0_i4)
    call stopnow('dipole')
  elseif (ndim.eq.1) then
    call outerror('dipole correction not available for polymer',0_i4)
    call stopnow('dipole')
  endif
!
  vol = volume(rv)
  rvol = 1.0_dp/vol
  const = 2.0_dp*pi*angstoev*rvol/3.0_dp
!
!  Calculate charge and dipole moment per unit cell
!
  q = 0.0_dp
  dx = 0.0_dp
  dy = 0.0_dp
  dz = 0.0_dp
  do i = 1,numat
    qi = qf(i)*occuf(i)
    q = q + qi
    nmi = natmol(i)
    if (lmol.and.nmi.ne.0) then
!
!  If molecules are present that make sure that images
!  belong to the same molecule are used to ensure a
!  consistent dipole moment
!
      indm = nmolind(i)
      call mindtoijk(indm,ix,iy,iz)
      xi = xclat(i) + ix*rv(1,1) + iy*rv(2,1) + iz*rv(3,1)
      yi = yclat(i) + ix*rv(1,2) + iy*rv(2,2) + iz*rv(3,2)
      zi = zclat(i) + ix*rv(1,3) + iy*rv(2,3) + iz*rv(3,3)
      dx = dx + qi*xi
      dy = dy + qi*yi
      dz = dz + qi*zi
    else
      dx = dx + qi*xclat(i)
      dy = dy + qi*yclat(i)
      dz = dz + qi*zclat(i)
    endif
  enddo
  if (abs(q).gt.1.0d-8) then
    call outwarning('Dipole correction is origin dependent as charge is not zero',0_i4)
  endif
  dip2 = dx*dx + dy*dy + dz*dz
!
!  If dipole is zero then we are finished
!
  if (dip2.eq.0.0_dp) then
    edipole = 0.0_dp
    return
  endif
!
!  Calculate energy
!
  edipole = const*dip2
!
!  If no derivatives are needed then we are finished
!
  if (.not.lgrad1) return
!
!  Internal derivatives
!
  dtrm1 = 2.0_dp*const
  if (lgrad2) then
    ix = - 2
    iy = - 1
    iz =   0
    do i = 1,numat
      qi = qf(i)*occuf(i)
      xdrv(i) = xdrv(i) + dtrm1*dx*qi
      ydrv(i) = ydrv(i) + dtrm1*dy*qi
      zdrv(i) = zdrv(i) + dtrm1*dz*qi
!
      nregioni = nregionno(nsft+nrelat(i))
      xregdrv(nregioni) = xregdrv(nregioni) + dtrm1*dx*qi
      yregdrv(nregioni) = yregdrv(nregioni) + dtrm1*dy*qi
      zregdrv(nregioni) = zregdrv(nregioni) + dtrm1*dz*qi
!
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      if (lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        jx = - 2
        jy = - 1
        jz =   0
        do j = 1,i-1
          lopj = (.not.lfreeze.or.lopf(nrelat(j)))
          if (lopj) then
            jx = jx + 3
            jy = jy + 3
            jz = jz + 3
            dtrm2 = dtrm1*qi*qf(j)
            derv2(jx,ix) = derv2(jx,ix) + dtrm2
            derv2(jy,iy) = derv2(jy,iy) + dtrm2
            derv2(jz,iz) = derv2(jz,iz) + dtrm2
            derv2(ix,jx) = derv2(ix,jx) + dtrm2
            derv2(iy,jy) = derv2(iy,jy) + dtrm2
            derv2(iz,jz) = derv2(iz,jz) + dtrm2
          endif
        enddo
!
!  Mixed internal derivatives
!
        if (lstr) then
!
!  Rest of terms are completed in strfin
!
          dtrm2 = dtrm1*qi
          derv3(ix,1) = derv3(ix,1) - dtrm2*dx
          derv3(iy,1) = derv3(iy,1) - dtrm2*dy
          derv3(iz,1) = derv3(iz,1) - dtrm2*dz
          if (ndim.ge.2) then
            derv3(ix,2) = derv3(ix,2) - dtrm2*dx
            derv3(iy,2) = derv3(iy,2) - dtrm2*dy
            derv3(iz,2) = derv3(iz,2) - dtrm2*dz
            if (ndim.eq.3) then
              derv3(ix,3) = derv3(ix,3) - dtrm2*dx
              derv3(iy,3) = derv3(iy,3) - dtrm2*dy
              derv3(iz,3) = derv3(iz,3) - dtrm2*dz
            endif
          endif
        endif
      endif
    enddo
  else
    do i = procid + 1,numat,nprocs
      qi = qf(i)*occuf(i)
      xdrv(i) = xdrv(i) + dtrm1*dx*qi
      ydrv(i) = ydrv(i) + dtrm1*dy*qi
      zdrv(i) = zdrv(i) + dtrm1*dz*qi
!
      nregioni = nregionno(nsft+nrelat(i))
      xregdrv(nregioni) = xregdrv(nregioni) + dtrm1*dx*qi
      yregdrv(nregioni) = yregdrv(nregioni) + dtrm1*dy*qi
      zregdrv(nregioni) = zregdrv(nregioni) + dtrm1*dz*qi
    enddo
  endif
!
!  Strain first derivatives
!
  if (lstr) then
    strderv(1) = strderv(1) - edipole
    if (ioproc) then
      rstrd(1) = rstrd(1) + dtrm1*dx*dx
    endif
    if (ndim.ge.2) then
      strderv(2) = strderv(2) - edipole
      if (ioproc) then
        rstrd(2) = rstrd(2) + dtrm1*dy*dy
      endif
      if (ndim.eq.3) then
        strderv(3) = strderv(3) - edipole
        if (ioproc) then
          rstrd(3) = rstrd(3) + dtrm1*dz*dz
          rstrd(4) = rstrd(4) + dtrm1*dy*dz
          rstrd(5) = rstrd(5) + dtrm1*dx*dz
          rstrd(6) = rstrd(6) + dtrm1*dx*dy
        endif
      else
        if (ioproc) then
          rstrd(3) = rstrd(3) + dtrm1*dx*dy
        endif
      endif
    endif
!
!  Atomic stresses
!
    if (latomicstress) then
      as_shift = edipole/dble(numat)
      do i = procid+1,numat,nprocs
        qi = qf(i)*occuf(i)
        nmi = natmol(i)
        if (lmol.and.nmi.ne.0) then
!
!  If molecules are present that make sure that images
!  belong to the same molecule are used to ensure a
!  consistent dipole moment
!
          indm = nmolind(i)
          call mindtoijk(indm,ix,iy,iz)
          xi = qi*(xclat(i) + ix*rv(1,1) + iy*rv(2,1) + iz*rv(3,1))
          yi = qi*(yclat(i) + ix*rv(1,2) + iy*rv(2,2) + iz*rv(3,2))
          zi = qi*(zclat(i) + ix*rv(1,3) + iy*rv(2,3) + iz*rv(3,3))
        else
          xi = qi*xclat(i)
          yi = qi*yclat(i)
          zi = qi*zclat(i)
        endif
        if (ndim.eq.3) then
          do kl = 1,nstrains
            atomicstress(1,i) = atomicstress(1,i) + dtrm1*dx*xi - as_shift
            atomicstress(2,i) = atomicstress(2,i) + dtrm1*dy*yi - as_shift
            atomicstress(3,i) = atomicstress(3,i) + dtrm1*dz*zi - as_shift
            atomicstress(4,i) = atomicstress(4,i) + 0.5_dp*dtrm1*(dy*zi + dz*yi)
            atomicstress(5,i) = atomicstress(5,i) + 0.5_dp*dtrm1*(dx*zi + dz*xi)
            atomicstress(6,i) = atomicstress(6,i) + 0.5_dp*dtrm1*(dx*yi + dy*xi)
          enddo
        elseif (ndim.eq.2) then
          do kl = 1,nstrains
            atomicstress(1,i) = atomicstress(1,i) + dtrm1*dx*xi - as_shift
            atomicstress(2,i) = atomicstress(2,i) + dtrm1*dy*yi - as_shift
            atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*dtrm1*(dx*yi + dy*xi)
          enddo
        elseif (ndim.eq.1) then
          do kl = 1,nstrains
            atomicstress(1,i) = atomicstress(1,i) + dtrm1*dx*xi - as_shift
          enddo
        endif
      enddo
    endif
    if (lgrad2) then
!
!  Strain second derivatives
!
!  Strain - strain  -  low half triangle
!
      sderv2(1,1) = sderv2(1,1) + edipole - 2.0_dp*dtrm1*dx*dx
      if (ndim.eq.3) then
        sderv2(2,1) = sderv2(2,1) + edipole - dtrm1*(dx*dx + dy*dy)
        sderv2(3,1) = sderv2(3,1) + edipole - dtrm1*(dx*dx + dz*dz)
        sderv2(2,2) = sderv2(2,2) + edipole - 2.0_dp*dtrm1*dy*dy
        sderv2(3,2) = sderv2(3,2) + edipole - dtrm1*(dy*dy + dz*dz)
        sderv2(3,3) = sderv2(3,3) + edipole - 2.0_dp*dtrm1*dz*dz
        sderv2(4,1) = sderv2(4,1) - dtrm1*dy*dz
        sderv2(5,1) = sderv2(5,1) - dtrm1*dx*dz
        sderv2(6,1) = sderv2(6,1) - dtrm1*dx*dy
        sderv2(4,2) = sderv2(4,2) - dtrm1*dy*dz
        sderv2(5,2) = sderv2(5,2) - dtrm1*dx*dz
        sderv2(6,2) = sderv2(6,2) - dtrm1*dx*dy
        sderv2(4,3) = sderv2(4,3) - dtrm1*dy*dz
        sderv2(5,3) = sderv2(5,3) - dtrm1*dx*dz
        sderv2(6,3) = sderv2(6,3) - dtrm1*dx*dy
        sderv2(4,4) = sderv2(4,4) + 0.5_dp*edipole
        sderv2(5,5) = sderv2(5,5) + 0.5_dp*edipole
        sderv2(6,6) = sderv2(6,6) + 0.5_dp*edipole
      elseif (ndim.eq.2) then
        sderv2(2,1) = sderv2(2,1) + edipole - dtrm1*(dx*dx + dy*dy)
        sderv2(2,2) = sderv2(2,2) + edipole - 2.0_dp*dtrm1*dy*dy
        sderv2(3,1) = sderv2(3,1) - dtrm1*dx*dy
        sderv2(3,2) = sderv2(3,2) - dtrm1*dx*dy
        sderv2(3,3) = sderv2(3,3) + 0.5_dp*edipole
      endif
    endif
  endif
!
  return
  end
