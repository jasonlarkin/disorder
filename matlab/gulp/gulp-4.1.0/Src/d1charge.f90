  subroutine d1charge(i,j,lopi,lopj,nor,d0i,d0j)
!
!  Calculates the contribution to the first derivatives from the bond
!  order charge derivatives. 
!
!   9/04 Created
!   9/04 Arguments changed so that charges are not referenced
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
  integer(i4), intent(in) :: nor
  logical,     intent(in) :: lopi
  logical,     intent(in) :: lopj
  real(dp),    intent(in) :: d0i(*)
  real(dp),    intent(in) :: d0j(*)
!
!  Local variables
!
  integer(i4)             :: ix
  integer(i4)             :: iy
  integer(i4)             :: iz
  integer(i4)             :: jx
  integer(i4)             :: jy
  integer(i4)             :: jz
  integer(i4)             :: k
  integer(i4)             :: kl
  integer(i4)             :: kx
  integer(i4)             :: ky
  integer(i4)             :: kz
  integer(i4)             :: n
  integer(i4)             :: nregioni
  integer(i4)             :: nregionj
  integer(i4)             :: nregionk
  logical                 :: lopk
  real(dp)                :: qpotsumi
  real(dp)                :: qpotsumj
!
!  Loop over distances collecting total Coulomb potential
!
  qpotsumi = 0.0_dp
  qpotsumj = 0.0_dp
  do n = 1,nor
    qpotsumi = qpotsumi + d0i(n)
    qpotsumj = qpotsumj + d0j(n)
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
    k = nqatomptr(n,i)
    kx = 3*(k - 1) + 1
    ky = kx + 1
    kz = ky + 1
    lopk = (.not.lfreeze.or.lopf(nrelat(k)))
    if (lopk) then
      xdrv(k) = xdrv(k) + qpotsumi*dqdxyz(kx,i)
      ydrv(k) = ydrv(k) + qpotsumi*dqdxyz(ky,i)
      zdrv(k) = zdrv(k) + qpotsumi*dqdxyz(kz,i)
    endif
    nregionk = nregionno(nsft+nrelat(k))
    xregdrv(nregionk) = xregdrv(nregionk) + qpotsumi*dqdxyz(kx,i)
    yregdrv(nregionk) = yregdrv(nregionk) + qpotsumi*dqdxyz(ky,i)
    zregdrv(nregionk) = zregdrv(nregionk) + qpotsumi*dqdxyz(kz,i)
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
  xregdrv(nregionj) = xregdrv(nregionj) + qpotsumi*dqdxyz(jx,j)
  yregdrv(nregionj) = yregdrv(nregionj) + qpotsumi*dqdxyz(jy,j)
  zregdrv(nregionj) = zregdrv(nregionj) + qpotsumi*dqdxyz(jz,j)
!
  do n = 1,nqatoms(j)
    k = nqatomptr(n,j)
    kx = 3*(k - 1) + 1
    ky = kx + 1
    kz = ky + 1
    lopk = (.not.lfreeze.or.lopf(nrelat(k)))
    if (lopk) then
      xdrv(k) = xdrv(k) + qpotsumj*dqdxyz(kx,j)
      ydrv(k) = ydrv(k) + qpotsumj*dqdxyz(ky,j)
      zdrv(k) = zdrv(k) + qpotsumj*dqdxyz(kz,j)
    endif
    nregionk = nregionno(nsft+nrelat(k))
    xregdrv(nregionk) = xregdrv(nregionk) + qpotsumj*dqdxyz(kx,j)
    yregdrv(nregionk) = yregdrv(nregionk) + qpotsumj*dqdxyz(ky,j)
    zregdrv(nregionk) = zregdrv(nregionk) + qpotsumj*dqdxyz(kz,j)
  enddo
!
!  Strain derivatives
!
  if (lstr) then
    do kl = 1,nstrains
      rstrd(kl) = rstrd(kl) + qpotsumi*dqds(kl,i)
      rstrd(kl) = rstrd(kl) + qpotsumj*dqds(kl,j)
    enddo
    if (latomicstress) then
      do kl = 1,nstrains
        atomicstress(kl,i) = atomicstress(kl,i) + qpotsumi*dqds(kl,i)
        atomicstress(kl,j) = atomicstress(kl,j) + qpotsumj*dqds(kl,j)
      enddo
    endif
  endif
!
  return
  end
