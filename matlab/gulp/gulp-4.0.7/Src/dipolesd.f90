  subroutine dipolesd(edipole,lgrad1,lgrad2)
!
!  Subroutine for dipole correction energy - symmetry adapted version
!
!   6/97 Dipoles for molecular systems sorted
!   6/97 Derivatives added.
!  11/02 Parallel changes added
!   2/07 Warning for non-charge neutral added
!   2/07 Partial occupancy handling added
!   5/12 Atomic stresses added
!   5/12 Atomic stresses removed for routines involving symmetry
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
  use constants
  use current
  use derivatives
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
  integer(i4)                :: ixf
  integer(i4)                :: iyf
  integer(i4)                :: izf
  integer(i4)                :: ixfo
  integer(i4)                :: iyfo
  integer(i4)                :: izfo
  integer(i4)                :: j
  integer(i4)                :: jx
  integer(i4)                :: jy
  integer(i4)                :: jz
  integer(i4)                :: jxc
  integer(i4)                :: jyc
  integer(i4)                :: jzc
  integer(i4)                :: nmi
  logical                    :: lopi
  logical                    :: lopj
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
  vol = volume(rv)
  rvol = 1.0_dp/vol
  const = 2.0_dp*pi*angstoev*rvol/3.0_dp
!
!  Calculate charge and dipole moment per unit cell
!
  q  = 0.0_dp
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
    ix =  - 2
    iy =  - 1
    iz = 0
    ixf = 1
    iyf = 2
    izf = 3
    ixfo = 1
    iyfo = 2
    izfo = 3
    do i = 1,nasym
      qi = qa(i)*dble(neqv(i))*occua(i)
      xdrv(i) = xdrv(i) + dtrm1*dx*qi
      ydrv(i) = ydrv(i) + dtrm1*dy*qi
      zdrv(i) = zdrv(i) + dtrm1*dz*qi
      lopi = (.not.lfreeze.or.lopf(i))
      if (lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        ixf = ixfo
        iyf = iyfo
        izf = izfo
        ixfo = ixfo + 3*neqv(i)
        iyfo = iyfo + 3*neqv(i)
        izfo = izfo + 3*neqv(i)
        jxc =  - 2
        jyc =  - 1
        jzc = 0
        do j = 1,numat
          lopj = (.not.lfreeze.or.lopf(nrelat(j)))
          if (lopj) then
            jxc = jxc + 3
            jyc = jyc + 3
            jzc = jzc + 3
            jx = jxc
            jy = jyc
            jz = jzc
          else
            jx = ixf
            jy = iyf
            jz = izf
          endif
          dtrm2 = dtrm1*qi*qf(j)
          derv2(jx,ix) = derv2(jx,ix) + dtrm2
          derv2(jy,iy) = derv2(jy,iy) + dtrm2
          derv2(jz,iz) = derv2(jz,iz) + dtrm2
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
          derv3(ix,2) = derv3(ix,2) - dtrm2*dx
          derv3(iy,2) = derv3(iy,2) - dtrm2*dy
          derv3(iz,2) = derv3(iz,2) - dtrm2*dz
          derv3(ix,3) = derv3(ix,3) - dtrm2*dx
          derv3(iy,3) = derv3(iy,3) - dtrm2*dy
          derv3(iz,3) = derv3(iz,3) - dtrm2*dz
        endif
      endif
    enddo
  else
    do i = procid + 1,nasym,nprocs
      qi = qa(i)*dble(neqv(i))*occua(i)
      xdrv(i) = xdrv(i) + dtrm1*dx*qi
      ydrv(i) = ydrv(i) + dtrm1*dy*qi
      zdrv(i) = zdrv(i) + dtrm1*dz*qi
    enddo
  endif
!
!  Strain first derivatives
!
  if (lstr) then
    strderv(1) = strderv(1) - edipole
    strderv(2) = strderv(2) - edipole
    strderv(3) = strderv(3) - edipole
    if (ioproc) then
      rstrd(1) = rstrd(1) + dtrm1*dx*dx
      rstrd(2) = rstrd(2) + dtrm1*dy*dy
      rstrd(3) = rstrd(3) + dtrm1*dz*dz
      rstrd(4) = rstrd(4) + dtrm1*dy*dz
      rstrd(5) = rstrd(5) + dtrm1*dx*dz
      rstrd(6) = rstrd(6) + dtrm1*dx*dy
    endif
!
!  Strain second derivatives
!
!  Strain - strain  -  low half triangle
!
    if (lgrad2) then
      sderv2(1,1) = sderv2(1,1) + edipole - 2.0_dp*dtrm1*dx*dx
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
!
      sderv2(4,4) = sderv2(4,4) + 0.5_dp*edipole
      sderv2(5,5) = sderv2(5,5) + 0.5_dp*edipole
      sderv2(6,6) = sderv2(6,6) + 0.5_dp*edipole
    endif
  endif
!
  return
  end
