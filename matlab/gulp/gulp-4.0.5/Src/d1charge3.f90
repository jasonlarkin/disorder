  subroutine d1charge3(i,j,k,lopi,lopj,lopk,nor,d0i,d0j,d0k)
!
!  Calculates the contribution to the first derivatives from the bond
!  order charge derivatives. Threebody version.
!
!   9/04 Created from d1charge
!  11/09 Region derivatives added
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
  use control,        only : latomicstress
  use current,        only : nstrains, nrelat, nsft
  use derivatives
  use optimisation,   only : lfreeze, lopf
  use symmetry,       only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
  integer(i4), intent(in) :: j
  integer(i4), intent(in) :: k
  integer(i4), intent(in) :: nor
  logical,     intent(in) :: lopi
  logical,     intent(in) :: lopj
  logical,     intent(in) :: lopk
  real(dp),    intent(in) :: d0i(*)
  real(dp),    intent(in) :: d0j(*)
  real(dp),    intent(in) :: d0k(*)
!
!  Local variables
!
  integer(i4)             :: ix
  integer(i4)             :: iy
  integer(i4)             :: iz
  integer(i4)             :: jx
  integer(i4)             :: jy
  integer(i4)             :: jz
  integer(i4)             :: kl
  integer(i4)             :: kx
  integer(i4)             :: ky
  integer(i4)             :: kz
  integer(i4)             :: m
  integer(i4)             :: mx
  integer(i4)             :: my
  integer(i4)             :: mz
  integer(i4)             :: n
  integer(i4)             :: nregioni
  integer(i4)             :: nregionj
  integer(i4)             :: nregionk
  integer(i4)             :: nregionm
  logical                 :: lopm
  real(dp)                :: qpotsumi
  real(dp)                :: qpotsumj
  real(dp)                :: qpotsumk
!
!  Loop over distances collecting total Coulomb potential
!
  qpotsumi = 0.0_dp
  qpotsumj = 0.0_dp
  qpotsumk = 0.0_dp
  do n = 1,nor
    qpotsumi = qpotsumi + d0i(n)
    qpotsumj = qpotsumj + d0j(n)
    qpotsumk = qpotsumk + d0k(n)
  enddo
!
!  Derivatives for atom i
!
  if (lopi) then
    ix = 3*(i - 1) + 1
    iy = ix + 1
    iz = iy + 1
    xdrv(i) = xdrv(i) + qpotsumi*dqdxyz(ix,i)
    ydrv(i) = ydrv(i) + qpotsumi*dqdxyz(iy,i)
    zdrv(i) = zdrv(i) + qpotsumi*dqdxyz(iz,i)
  endif
!
  nregioni = nregionno(nsft+nrelat(i))
  xregdrv(nregioni) = xregdrv(nregioni) + qpotsumi*dqdxyz(ix,i)
  yregdrv(nregioni) = yregdrv(nregioni) + qpotsumi*dqdxyz(iy,i)
  zregdrv(nregioni) = zregdrv(nregioni) + qpotsumi*dqdxyz(iz,i)
!
  do n = 1,nqatoms(i)
    m = nqatomptr(n,i)
    lopm = (.not.lfreeze.or.lopf(nrelat(m)))
    if (lopm) then
      mx = 3*(m - 1) + 1
      my = mx + 1
      mz = my + 1
      xdrv(m) = xdrv(m) + qpotsumi*dqdxyz(mx,i)
      ydrv(m) = ydrv(m) + qpotsumi*dqdxyz(my,i)
      zdrv(m) = zdrv(m) + qpotsumi*dqdxyz(mz,i)
    endif
    nregionm = nregionno(nsft+nrelat(m))
    xregdrv(nregionm) = xregdrv(nregionm) + qpotsumi*dqdxyz(mx,i)
    yregdrv(nregionm) = yregdrv(nregionm) + qpotsumi*dqdxyz(my,i)
    zregdrv(nregionm) = zregdrv(nregionm) + qpotsumi*dqdxyz(mz,i)
  enddo
!
!  Derivatives for atom j
!
  if (lopj) then
    jx = 3*(j - 1) + 1
    jy = jx + 1
    jz = jy + 1
    xdrv(j) = xdrv(j) + qpotsumj*dqdxyz(jx,j)
    ydrv(j) = ydrv(j) + qpotsumj*dqdxyz(jy,j)
    zdrv(j) = zdrv(j) + qpotsumj*dqdxyz(jz,j)
  endif
!
  nregionj = nregionno(nsft+nrelat(j))
  xregdrv(nregionj) = xregdrv(nregionj) + qpotsumj*dqdxyz(jx,j)
  yregdrv(nregionj) = yregdrv(nregionj) + qpotsumj*dqdxyz(jy,j)
  zregdrv(nregionj) = zregdrv(nregionj) + qpotsumj*dqdxyz(jz,j)
!
  do n = 1,nqatoms(j)
    m = nqatomptr(n,j)
    lopm = (.not.lfreeze.or.lopf(nrelat(m)))
    if (lopm) then
      mx = 3*(m - 1) + 1
      my = mx + 1
      mz = my + 1
      xdrv(m) = xdrv(m) + qpotsumj*dqdxyz(mx,j)
      ydrv(m) = ydrv(m) + qpotsumj*dqdxyz(my,j)
      zdrv(m) = zdrv(m) + qpotsumj*dqdxyz(mz,j)
    endif
    nregionm = nregionno(nsft+nrelat(m))
    xregdrv(nregionm) = xregdrv(nregionm) + qpotsumj*dqdxyz(mx,j)
    yregdrv(nregionm) = yregdrv(nregionm) + qpotsumj*dqdxyz(my,j)
    zregdrv(nregionm) = zregdrv(nregionm) + qpotsumj*dqdxyz(mz,j)
  enddo
!
!  Derivatives for atom k
!
  if (lopk) then
    kx = 3*(k - 1) + 1
    ky = kx + 1
    kz = ky + 1
    xdrv(k) = xdrv(k) + qpotsumk*dqdxyz(kx,k)
    ydrv(k) = ydrv(k) + qpotsumk*dqdxyz(ky,k)
    zdrv(k) = zdrv(k) + qpotsumk*dqdxyz(kz,k)
  endif
!
  nregionk = nregionno(nsft+nrelat(k))
  xregdrv(nregionk) = xregdrv(nregionk) + qpotsumk*dqdxyz(kx,k)
  yregdrv(nregionk) = yregdrv(nregionk) + qpotsumk*dqdxyz(ky,k)
  zregdrv(nregionk) = zregdrv(nregionk) + qpotsumk*dqdxyz(kz,k)
!
  do n = 1,nqatoms(k)
    m = nqatomptr(n,k)
    lopm = (.not.lfreeze.or.lopf(nrelat(m)))
    if (lopm) then
      mx = 3*(m - 1) + 1
      my = mx + 1
      mz = my + 1
      xdrv(m) = xdrv(m) + qpotsumk*dqdxyz(mx,k)
      ydrv(m) = ydrv(m) + qpotsumk*dqdxyz(my,k)
      zdrv(m) = zdrv(m) + qpotsumk*dqdxyz(mz,k)
    endif
    nregionm = nregionno(nsft+nrelat(m))
    xregdrv(nregionm) = xregdrv(nregionm) + qpotsumk*dqdxyz(mx,k)
    yregdrv(nregionm) = yregdrv(nregionm) + qpotsumk*dqdxyz(my,k)
    zregdrv(nregionm) = zregdrv(nregionm) + qpotsumk*dqdxyz(mz,k)
  enddo
!
!  Strain derivatives
!
  if (lstr) then
    do kl = 1,nstrains
      rstrd(kl) = rstrd(kl) + qpotsumi*dqds(kl,i)
      rstrd(kl) = rstrd(kl) + qpotsumj*dqds(kl,j)
      rstrd(kl) = rstrd(kl) + qpotsumk*dqds(kl,k)
    enddo
    if (latomicstress) then
      do kl = 1,nstrains
        atomicstress(kl,i) = atomicstress(kl,i) + qpotsumi*dqds(kl,i)
        atomicstress(kl,j) = atomicstress(kl,j) + qpotsumj*dqds(kl,j)
        atomicstress(kl,k) = atomicstress(kl,k) + qpotsumj*dqds(kl,k)
      enddo
    endif
  endif
!
  return
  end
