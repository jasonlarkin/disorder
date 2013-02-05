  subroutine d2charge3p(i,j,k,nor,ix,iy,iz,jx,jy,jz,kx,ky,kz,xkv,ykv,zkv,dei,dej,dek,d1q,d2q)
!
!  Calculates the contribution to the dynamical matrix due to charge derivatives from variable 
!  charge models as a result of three body potentials.
!
!  At present this is gamma point only.
!
!  10/04 Created from d2charge3
!  11/07 Unused variables removed
!
!  On entry:
!
!  d1q(3,3,3) = derivative of first derivative of i - j vector with respect to charge
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, November 2007
!
  use configurations, only : nregionno
  use control
  use current
  use derivatives
  use element
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: i
  integer(i4), intent(in)    :: ix
  integer(i4), intent(in)    :: iy
  integer(i4), intent(in)    :: iz
  integer(i4), intent(in)    :: j
  integer(i4), intent(in)    :: jx
  integer(i4), intent(in)    :: jy
  integer(i4), intent(in)    :: jz
  integer(i4), intent(in)    :: k
  integer(i4), intent(in)    :: kx
  integer(i4), intent(in)    :: ky
  integer(i4), intent(in)    :: kz
  integer(i4), intent(in)    :: nor
  real(dp),    intent(in)    :: d1q(3,3,3)
  real(dp),    intent(in)    :: d2q(6,*)
  real(dp),    intent(in)    :: dei(*)
  real(dp),    intent(in)    :: dej(*)
  real(dp),    intent(in)    :: dek(*)
  real(dp),    intent(in)    :: xkv
  real(dp),    intent(in)    :: ykv
  real(dp),    intent(in)    :: zkv
!
!  Local variables
!
  integer(i4)                :: iv
  integer(i4)                :: l
  integer(i4)                :: lx
  integer(i4)                :: ly
  integer(i4)                :: lz
  integer(i4)                :: m
  integer(i4)                :: mnxx
  integer(i4)                :: mnxy
  integer(i4)                :: mnxz
  integer(i4)                :: mnyx
  integer(i4)                :: mnyy
  integer(i4)                :: mnyz
  integer(i4)                :: mnzx
  integer(i4)                :: mnzy
  integer(i4)                :: mnzz
  integer(i4)                :: mx
  integer(i4)                :: my
  integer(i4)                :: mz
  integer(i4)                :: n
  integer(i4)                :: nx
  integer(i4)                :: p
  integer(i4)                :: px
  integer(i4)                :: py
  integer(i4)                :: pz
  logical                    :: lNonIJQDeriv
  real(dp)                   :: d1ql(3,3,3)
  real(dp)                   :: d2i2s
  real(dp)                   :: d2ijs
  real(dp)                   :: d2iks
  real(dp)                   :: d2j2s
  real(dp)                   :: d2jks
  real(dp)                   :: d2k2s
  real(dp)                   :: deisum
  real(dp)                   :: dejsum
  real(dp)                   :: deksum
  real(dp)                   :: dilx
  real(dp)                   :: dily
  real(dp)                   :: dilz
  real(dp)                   :: dimx
  real(dp)                   :: dimy
  real(dp)                   :: dimz
  real(dp)                   :: djlx
  real(dp)                   :: djly
  real(dp)                   :: djlz
  real(dp)                   :: djmx
  real(dp)                   :: djmy
  real(dp)                   :: djmz
  real(dp)                   :: dklx
  real(dp)                   :: dkly
  real(dp)                   :: dklz
  real(dp)                   :: dkmx
  real(dp)                   :: dkmy
  real(dp)                   :: dkmz
!
!  Check that charge derivatives are in use
!
  if (.not.lDoQDeriv2) return
!
!  Set pointers to i-j second derivative block for eself derivatives
!
  d2i2s = 0.0_dp
  d2ijs = 0.0_dp
  d2iks = 0.0_dp
  d2j2s = 0.0_dp
  d2jks = 0.0_dp
  d2k2s = 0.0_dp
  do iv = 1,nor
    d2i2s = d2i2s + d2q(1,iv)
    d2ijs = d2ijs + d2q(2,iv)
    d2iks = d2iks + d2q(3,iv)
    d2j2s = d2j2s + d2q(4,iv)
    d2jks = d2jks + d2q(5,iv)
    d2k2s = d2k2s + d2q(6,iv)
  enddo
  d1ql(1:3,1:3,1:3) = d1q(1:3,1:3,1:3)
  if ((leem.and..not.lelementOK(nat(i))).or.nregionno(nsft+nrelat(i)).ne.1) then
    d2i2s = 0.0_dp
    d2ijs = 0.0_dp
    d2iks = 0.0_dp
    d1ql(1:3,1:3,1) = 0.0_dp
  endif
  if ((leem.and..not.lelementOK(nat(j))).or.nregionno(nsft+nrelat(j)).ne.1) then
    d2ijs = 0.0_dp
    d2j2s = 0.0_dp
    d2jks = 0.0_dp
    d1ql(1:3,1:3,2) = 0.0_dp
  endif
  if ((leem.and..not.lelementOK(nat(k))).or.nregionno(nsft+nrelat(k)).ne.1) then
    d2iks = 0.0_dp
    d2jks = 0.0_dp
    d2k2s = 0.0_dp
    d1ql(1:3,1:3,3) = 0.0_dp
  endif
!
!  The following flag causes the second derivatives of the charge distribution 
!  with respect to two atoms neither of which are the i/j of the energy term
!  to be calculated. At the moment this contribution is not used in any method
!  and so the flag is set to false for efficiency.
!
  lNonIJQDeriv = .false.
!
!  Second derivative of charge with respect to coordinate contributions
!
!  This term is zero for Hellman-Feynman case
!
  if (.not.leem) then
    deisum = 0.0_dp
    dejsum = 0.0_dp
    deksum = 0.0_dp
    do iv = 1,nor
      deisum = deisum + dei(iv)
      dejsum = dejsum + dej(iv)
      deksum = deksum + dek(iv)
    enddo
!
!  Terms involving Q derivatives for i/j/k and one other atom
!
    do m = 1,nqatoms(i)
      p = nqatomptr(m,i)
      px = 3*(p - 1) + 1
      py = px + 1
      pz = py + 1
      mx = 3*(m - 1) + 1
      my = mx + 1
      mz = my + 1
!
      mnxx = mx*(mx + 1)/2 
      mnyy = my*(my + 1)/2 
      mnzz = mz*(mz + 1)/2 
      mnxy = mnyy - 1
      mnxz = mnzz - 2
      mnyz = mnzz - 1
!
      if (i.gt.p) then
        derv2(px,ix) = derv2(px,ix) + deisum*d2qdxyz2(mnxx,i)
        derv2(py,ix) = derv2(py,ix) + deisum*d2qdxyz2(mnxy,i)
        derv2(pz,ix) = derv2(pz,ix) + deisum*d2qdxyz2(mnxz,i)
        derv2(px,iy) = derv2(px,iy) + deisum*d2qdxyz2(mnxy,i)
        derv2(py,iy) = derv2(py,iy) + deisum*d2qdxyz2(mnyy,i)
        derv2(pz,iy) = derv2(pz,iy) + deisum*d2qdxyz2(mnyz,i)
        derv2(px,iz) = derv2(px,iz) + deisum*d2qdxyz2(mnxz,i)
        derv2(py,iz) = derv2(py,iz) + deisum*d2qdxyz2(mnyz,i)
        derv2(pz,iz) = derv2(pz,iz) + deisum*d2qdxyz2(mnzz,i)
      elseif (i.lt.p) then
        derv2(ix,px) = derv2(ix,px) + deisum*d2qdxyz2(mnxx,i)
        derv2(iy,px) = derv2(iy,px) + deisum*d2qdxyz2(mnxy,i)
        derv2(iz,px) = derv2(iz,px) + deisum*d2qdxyz2(mnxz,i)
        derv2(ix,py) = derv2(ix,py) + deisum*d2qdxyz2(mnxy,i)
        derv2(iy,py) = derv2(iy,py) + deisum*d2qdxyz2(mnyy,i)
        derv2(iz,py) = derv2(iz,py) + deisum*d2qdxyz2(mnyz,i)
        derv2(ix,pz) = derv2(ix,pz) + deisum*d2qdxyz2(mnxz,i)
        derv2(iy,pz) = derv2(iy,pz) + deisum*d2qdxyz2(mnyz,i)
        derv2(iz,pz) = derv2(iz,pz) + deisum*d2qdxyz2(mnzz,i)
      endif
    enddo
!
    do m = 1,nqatoms(j)
      p = nqatomptr(m,j)
      px = 3*(p - 1) + 1
      py = px + 1
      pz = py + 1
      mx = 3*(m - 1) + 1
      my = mx + 1
      mz = my + 1
!
      mnxx = mx*(mx + 1)/2 
      mnyy = my*(my + 1)/2 
      mnzz = mz*(mz + 1)/2 
      mnxy = mnyy - 1
      mnxz = mnzz - 2
      mnyz = mnzz - 1
!
      if (j.gt.p) then
        derv2(px,jx) = derv2(px,jx) + dejsum*d2qdxyz2(mnxx,j)
        derv2(py,jx) = derv2(py,jx) + dejsum*d2qdxyz2(mnxy,j)
        derv2(pz,jx) = derv2(pz,jx) + dejsum*d2qdxyz2(mnxz,j)
        derv2(px,jy) = derv2(px,jy) + dejsum*d2qdxyz2(mnxy,j)
        derv2(py,jy) = derv2(py,jy) + dejsum*d2qdxyz2(mnyy,j)
        derv2(pz,jy) = derv2(pz,jy) + dejsum*d2qdxyz2(mnyz,j)
        derv2(px,jz) = derv2(px,jz) + dejsum*d2qdxyz2(mnxz,j)
        derv2(py,jz) = derv2(py,jz) + dejsum*d2qdxyz2(mnyz,j)
        derv2(pz,jz) = derv2(pz,jz) + dejsum*d2qdxyz2(mnzz,j)
      elseif (j.lt.p) then
        derv2(jx,px) = derv2(jx,px) + dejsum*d2qdxyz2(mnxx,j)
        derv2(jy,px) = derv2(jy,px) + dejsum*d2qdxyz2(mnxy,j)
        derv2(jz,px) = derv2(jz,px) + dejsum*d2qdxyz2(mnxz,j)
        derv2(jx,py) = derv2(jx,py) + dejsum*d2qdxyz2(mnxy,j)
        derv2(jy,py) = derv2(jy,py) + dejsum*d2qdxyz2(mnyy,j)
        derv2(jz,py) = derv2(jz,py) + dejsum*d2qdxyz2(mnyz,j)
        derv2(jx,pz) = derv2(jx,pz) + dejsum*d2qdxyz2(mnxz,j)
        derv2(jy,pz) = derv2(jy,pz) + dejsum*d2qdxyz2(mnyz,j)
        derv2(jz,pz) = derv2(jz,pz) + dejsum*d2qdxyz2(mnzz,j)
      endif
    enddo
!
    do m = 1,nqatoms(k)
      p = nqatomptr(m,k)
      px = 3*(p - 1) + 1
      py = px + 1
      pz = py + 1
      mx = 3*(m - 1) + 1
      my = mx + 1
      mz = my + 1
!
      mnxx = mx*(mx + 1)/2 
      mnyy = my*(my + 1)/2 
      mnzz = mz*(mz + 1)/2 
      mnxy = mnyy - 1
      mnxz = mnzz - 2
      mnyz = mnzz - 1
!
      if (k.gt.p) then
        derv2(px,kx) = derv2(px,kx) + deksum*d2qdxyz2(mnxx,k)
        derv2(py,kx) = derv2(py,kx) + deksum*d2qdxyz2(mnxy,k)
        derv2(pz,kx) = derv2(pz,kx) + deksum*d2qdxyz2(mnxz,k)
        derv2(px,ky) = derv2(px,ky) + deksum*d2qdxyz2(mnxy,k)
        derv2(py,ky) = derv2(py,ky) + deksum*d2qdxyz2(mnyy,k)
        derv2(pz,ky) = derv2(pz,ky) + deksum*d2qdxyz2(mnyz,k)
        derv2(px,kz) = derv2(px,kz) + deksum*d2qdxyz2(mnxz,k)
        derv2(py,kz) = derv2(py,kz) + deksum*d2qdxyz2(mnyz,k)
        derv2(pz,kz) = derv2(pz,kz) + deksum*d2qdxyz2(mnzz,k)
      elseif (k.lt.p) then
        derv2(kx,px) = derv2(kx,px) + deksum*d2qdxyz2(mnxx,k)
        derv2(ky,px) = derv2(ky,px) + deksum*d2qdxyz2(mnxy,k)
        derv2(kz,px) = derv2(kz,px) + deksum*d2qdxyz2(mnxz,k)
        derv2(kx,py) = derv2(kx,py) + deksum*d2qdxyz2(mnxy,k)
        derv2(ky,py) = derv2(ky,py) + deksum*d2qdxyz2(mnyy,k)
        derv2(kz,py) = derv2(kz,py) + deksum*d2qdxyz2(mnyz,k)
        derv2(kx,pz) = derv2(kx,pz) + deksum*d2qdxyz2(mnxz,k)
        derv2(ky,pz) = derv2(ky,pz) + deksum*d2qdxyz2(mnyz,k)
        derv2(kz,pz) = derv2(kz,pz) + deksum*d2qdxyz2(mnzz,k)
      endif
    enddo
!
!  Terms involving Q derivatives for non-i/j case
!
    if (lNonIJQDeriv) then
      do m = 2,nqatoms(i)
        p = nqatomptr(m,i)
        px = 3*(p - 1) + 1
        py = px + 1
        pz = py + 1
        mx = 3*(m - 1) + 1
        my = mx + 1
        mz = my + 1
        do n = 1,m-1
          l = nqatomptr(n,i)
          lx = 3*(l - 1) + 1
          ly = lx + 1
          lz = ly + 1
          nx = 3*(n - 1) + 1
!
          mnxx = mx*(mx - 1)/2 + nx
          mnxy = mnxx + 1
          mnxz = mnxx + 2
          mnyx = my*(my - 1)/2 + nx
          mnyy = mnyx + 1
          mnyz = mnyx + 2
          mnzx = mz*(mz - 1)/2 + nx
          mnzy = mnzx + 1
          mnzz = mnzx + 2
!
          derv2(lx,px) = derv2(lx,px) + deisum*d2qdxyz2(mnxx,i)
          derv2(ly,px) = derv2(ly,px) + deisum*d2qdxyz2(mnxy,i)
          derv2(lz,px) = derv2(lz,px) + deisum*d2qdxyz2(mnxz,i)
          derv2(lx,py) = derv2(lx,py) + deisum*d2qdxyz2(mnyx,i)
          derv2(ly,py) = derv2(ly,py) + deisum*d2qdxyz2(mnyy,i)
          derv2(lz,py) = derv2(lz,py) + deisum*d2qdxyz2(mnyz,i)
          derv2(lx,pz) = derv2(lx,pz) + deisum*d2qdxyz2(mnzx,i)
          derv2(ly,pz) = derv2(ly,pz) + deisum*d2qdxyz2(mnzy,i)
          derv2(lz,pz) = derv2(lz,pz) + deisum*d2qdxyz2(mnzz,i)
        enddo
      enddo
!
      do m = 2,nqatoms(j)
        p = nqatomptr(m,j)
        px = 3*(p - 1) + 1
        py = px + 1
        pz = py + 1
        mx = 3*(m - 1) + 1
        my = mx + 1
        mz = my + 1
        do n = 1,m-1
          l = nqatomptr(n,j)
          lx = 3*(l - 1) + 1
          ly = lx + 1
          lz = ly + 1
          nx = 3*(n - 1) + 1
!
          mnxx = mx*(mx - 1)/2 + nx
          mnxy = mnxx + 1
          mnxz = mnxx + 2
          mnyx = my*(my - 1)/2 + nx
          mnyy = mnyx + 1
          mnyz = mnyx + 2
          mnzx = mz*(mz - 1)/2 + nx
          mnzy = mnzx + 1
          mnzz = mnzx + 2
!
          derv2(lx,px) = derv2(lx,px) + deisum*d2qdxyz2(mnxx,j)
          derv2(ly,px) = derv2(ly,px) + deisum*d2qdxyz2(mnxy,j)
          derv2(lz,px) = derv2(lz,px) + deisum*d2qdxyz2(mnxz,j)
          derv2(lx,py) = derv2(lx,py) + deisum*d2qdxyz2(mnyx,j)
          derv2(ly,py) = derv2(ly,py) + deisum*d2qdxyz2(mnyy,j)
          derv2(lz,py) = derv2(lz,py) + deisum*d2qdxyz2(mnyz,j)
          derv2(lx,pz) = derv2(lx,pz) + deisum*d2qdxyz2(mnzx,j)
          derv2(ly,pz) = derv2(ly,pz) + deisum*d2qdxyz2(mnzy,j)
          derv2(lz,pz) = derv2(lz,pz) + deisum*d2qdxyz2(mnzz,j)
        enddo
      enddo
!
      do m = 2,nqatoms(k)
        p = nqatomptr(m,k)
        px = 3*(p - 1) + 1
        py = px + 1
        pz = py + 1
        mx = 3*(m - 1) + 1
        my = mx + 1
        mz = my + 1
        do n = 1,m-1
          l = nqatomptr(n,k)
          lx = 3*(l - 1) + 1
          ly = lx + 1
          lz = ly + 1
          nx = 3*(n - 1) + 1
!
          mnxx = mx*(mx - 1)/2 + nx
          mnxy = mnxx + 1
          mnxz = mnxx + 2
          mnyx = my*(my - 1)/2 + nx
          mnyy = mnyx + 1
          mnyz = mnyx + 2
          mnzx = mz*(mz - 1)/2 + nx
          mnzy = mnzx + 1
          mnzz = mnzx + 2
!
          derv2(lx,px) = derv2(lx,px) + deksum*d2qdxyz2(mnxx,k)
          derv2(ly,px) = derv2(ly,px) + deksum*d2qdxyz2(mnxy,k)
          derv2(lz,px) = derv2(lz,px) + deksum*d2qdxyz2(mnxz,k)
          derv2(lx,py) = derv2(lx,py) + deksum*d2qdxyz2(mnyx,k)
          derv2(ly,py) = derv2(ly,py) + deksum*d2qdxyz2(mnyy,k)
          derv2(lz,py) = derv2(lz,py) + deksum*d2qdxyz2(mnyz,k)
          derv2(lx,pz) = derv2(lx,pz) + deksum*d2qdxyz2(mnzx,k)
          derv2(ly,pz) = derv2(ly,pz) + deksum*d2qdxyz2(mnzy,k)
          derv2(lz,pz) = derv2(lz,pz) + deksum*d2qdxyz2(mnzz,k)
        enddo
      enddo
    endif
  endif
!
!  Loop over atoms to add ij-m contributions
!
  mx = - 2
  my = - 1
  mz =   0
  do m = 1,numat
    mx = mx + 3
    my = my + 3
    mz = mz + 3
!
    dimx = dqdxyz(mx,i)
    dimy = dqdxyz(my,i)
    dimz = dqdxyz(mz,i)
    djmx = dqdxyz(mx,j)
    djmy = dqdxyz(my,j)
    djmz = dqdxyz(mz,j)
    dkmx = dqdxyz(mx,k)
    dkmy = dqdxyz(my,k)
    dkmz = dqdxyz(mz,k)
    if (i.ne.m) then
!
!  d2E/(d(alpha).dq) x dq/d(beta) : i-m
!
      if (m.le.i) then
        derv2(mx,ix) = derv2(mx,ix) - d1ql(1,1,1)*dimx - d1ql(1,2,1)*dimx
        derv2(my,ix) = derv2(my,ix) - d1ql(1,1,1)*dimy - d1ql(1,2,1)*dimy
        derv2(mz,ix) = derv2(mz,ix) - d1ql(1,1,1)*dimz - d1ql(1,2,1)*dimz
        derv2(mx,iy) = derv2(mx,iy) - d1ql(2,1,1)*dimx - d1ql(2,2,1)*dimx
        derv2(my,iy) = derv2(my,iy) - d1ql(2,1,1)*dimy - d1ql(2,2,1)*dimy
        derv2(mz,iy) = derv2(mz,iy) - d1ql(2,1,1)*dimz - d1ql(2,2,1)*dimz
        derv2(mx,iz) = derv2(mx,iz) - d1ql(3,1,1)*dimx - d1ql(3,2,1)*dimx
        derv2(my,iz) = derv2(my,iz) - d1ql(3,1,1)*dimy - d1ql(3,2,1)*dimy
        derv2(mz,iz) = derv2(mz,iz) - d1ql(3,1,1)*dimz - d1ql(3,2,1)*dimz
!
        derv2(mx,ix) = derv2(mx,ix) - d1ql(1,1,2)*djmx - d1ql(1,2,2)*djmx
        derv2(my,ix) = derv2(my,ix) - d1ql(1,1,2)*djmy - d1ql(1,2,2)*djmy
        derv2(mz,ix) = derv2(mz,ix) - d1ql(1,1,2)*djmz - d1ql(1,2,2)*djmz
        derv2(mx,iy) = derv2(mx,iy) - d1ql(2,1,2)*djmx - d1ql(2,2,2)*djmx
        derv2(my,iy) = derv2(my,iy) - d1ql(2,1,2)*djmy - d1ql(2,2,2)*djmy
        derv2(mz,iy) = derv2(mz,iy) - d1ql(2,1,2)*djmz - d1ql(2,2,2)*djmz
        derv2(mx,iz) = derv2(mx,iz) - d1ql(3,1,2)*djmx - d1ql(3,2,2)*djmx
        derv2(my,iz) = derv2(my,iz) - d1ql(3,1,2)*djmy - d1ql(3,2,2)*djmy
        derv2(mz,iz) = derv2(mz,iz) - d1ql(3,1,2)*djmz - d1ql(3,2,2)*djmz
!
        derv2(mx,ix) = derv2(mx,ix) - d1ql(1,1,3)*dkmx - d1ql(1,2,3)*dkmx
        derv2(my,ix) = derv2(my,ix) - d1ql(1,1,3)*dkmy - d1ql(1,2,3)*dkmy
        derv2(mz,ix) = derv2(mz,ix) - d1ql(1,1,3)*dkmz - d1ql(1,2,3)*dkmz
        derv2(mx,iy) = derv2(mx,iy) - d1ql(2,1,3)*dkmx - d1ql(2,2,3)*dkmx
        derv2(my,iy) = derv2(my,iy) - d1ql(2,1,3)*dkmy - d1ql(2,2,3)*dkmy
        derv2(mz,iy) = derv2(mz,iy) - d1ql(2,1,3)*dkmz - d1ql(2,2,3)*dkmz
        derv2(mx,iz) = derv2(mx,iz) - d1ql(3,1,3)*dkmx - d1ql(3,2,3)*dkmx
        derv2(my,iz) = derv2(my,iz) - d1ql(3,1,3)*dkmy - d1ql(3,2,3)*dkmy
        derv2(mz,iz) = derv2(mz,iz) - d1ql(3,1,3)*dkmz - d1ql(3,2,3)*dkmz
      else
        derv2(ix,mx) = derv2(ix,mx) - d1ql(1,1,1)*dimx - d1ql(1,2,1)*dimx
        derv2(iy,mx) = derv2(iy,mx) - d1ql(2,1,1)*dimx - d1ql(2,2,1)*dimx
        derv2(iz,mx) = derv2(iz,mx) - d1ql(3,1,1)*dimx - d1ql(3,2,1)*dimx
        derv2(ix,my) = derv2(ix,my) - d1ql(1,1,1)*dimy - d1ql(1,2,1)*dimy
        derv2(iy,my) = derv2(iy,my) - d1ql(2,1,1)*dimy - d1ql(2,2,1)*dimy
        derv2(iz,my) = derv2(iz,my) - d1ql(3,1,1)*dimy - d1ql(3,2,1)*dimy
        derv2(ix,mz) = derv2(ix,mz) - d1ql(1,1,1)*dimz - d1ql(1,2,1)*dimz
        derv2(iy,mz) = derv2(iy,mz) - d1ql(2,1,1)*dimz - d1ql(2,2,1)*dimz
        derv2(iz,mz) = derv2(iz,mz) - d1ql(3,1,1)*dimz - d1ql(3,2,1)*dimz
!
        derv2(ix,mx) = derv2(ix,mx) - d1ql(1,1,2)*djmx - d1ql(1,2,2)*djmx
        derv2(iy,mx) = derv2(iy,mx) - d1ql(2,1,2)*djmx - d1ql(2,2,2)*djmx
        derv2(iz,mx) = derv2(iz,mx) - d1ql(3,1,2)*djmx - d1ql(3,2,2)*djmx
        derv2(ix,my) = derv2(ix,my) - d1ql(1,1,2)*djmy - d1ql(1,2,2)*djmy
        derv2(iy,my) = derv2(iy,my) - d1ql(2,1,2)*djmy - d1ql(2,2,2)*djmy
        derv2(iz,my) = derv2(iz,my) - d1ql(3,1,2)*djmy - d1ql(3,2,2)*djmy
        derv2(ix,mz) = derv2(ix,mz) - d1ql(1,1,2)*djmz - d1ql(1,2,2)*djmz
        derv2(iy,mz) = derv2(iy,mz) - d1ql(2,1,2)*djmz - d1ql(2,2,2)*djmz
        derv2(iz,mz) = derv2(iz,mz) - d1ql(3,1,2)*djmz - d1ql(3,2,2)*djmz
!
        derv2(ix,mx) = derv2(ix,mx) - d1ql(1,1,3)*dkmx - d1ql(1,2,3)*dkmx
        derv2(iy,mx) = derv2(iy,mx) - d1ql(2,1,3)*dkmx - d1ql(2,2,3)*dkmx
        derv2(iz,mx) = derv2(iz,mx) - d1ql(3,1,3)*dkmx - d1ql(3,2,3)*dkmx
        derv2(ix,my) = derv2(ix,my) - d1ql(1,1,3)*dkmy - d1ql(1,2,3)*dkmy
        derv2(iy,my) = derv2(iy,my) - d1ql(2,1,3)*dkmy - d1ql(2,2,3)*dkmy
        derv2(iz,my) = derv2(iz,my) - d1ql(3,1,3)*dkmy - d1ql(3,2,3)*dkmy
        derv2(ix,mz) = derv2(ix,mz) - d1ql(1,1,3)*dkmz - d1ql(1,2,3)*dkmz
        derv2(iy,mz) = derv2(iy,mz) - d1ql(2,1,3)*dkmz - d1ql(2,2,3)*dkmz
        derv2(iz,mz) = derv2(iz,mz) - d1ql(3,1,3)*dkmz - d1ql(3,2,3)*dkmz
      endif
    endif
    if (j.ne.m) then
!
!  d2E/(d(alpha).dq) x dq/d(beta) : j-m
!
      if (m.le.j) then
        derv2(mx,jx) = derv2(mx,jx) + d1ql(1,1,1)*dimx - d1ql(1,3,1)*dimx
        derv2(my,jx) = derv2(my,jx) + d1ql(1,1,1)*dimy - d1ql(1,3,1)*dimy
        derv2(mz,jx) = derv2(mz,jx) + d1ql(1,1,1)*dimz - d1ql(1,3,1)*dimz
        derv2(mx,jy) = derv2(mx,jy) + d1ql(2,1,1)*dimx - d1ql(2,3,1)*dimx
        derv2(my,jy) = derv2(my,jy) + d1ql(2,1,1)*dimy - d1ql(2,3,1)*dimy
        derv2(mz,jy) = derv2(mz,jy) + d1ql(2,1,1)*dimz - d1ql(2,3,1)*dimz
        derv2(mx,jz) = derv2(mx,jz) + d1ql(3,1,1)*dimx - d1ql(3,3,1)*dimx
        derv2(my,jz) = derv2(my,jz) + d1ql(3,1,1)*dimy - d1ql(3,3,1)*dimy
        derv2(mz,jz) = derv2(mz,jz) + d1ql(3,1,1)*dimz - d1ql(3,3,1)*dimz
!
        derv2(mx,jx) = derv2(mx,jx) + d1ql(1,1,2)*djmx - d1ql(1,3,2)*djmx
        derv2(my,jx) = derv2(my,jx) + d1ql(1,1,2)*djmy - d1ql(1,3,2)*djmy
        derv2(mz,jx) = derv2(mz,jx) + d1ql(1,1,2)*djmz - d1ql(1,3,2)*djmz
        derv2(mx,jy) = derv2(mx,jy) + d1ql(2,1,2)*djmx - d1ql(2,3,2)*djmx
        derv2(my,jy) = derv2(my,jy) + d1ql(2,1,2)*djmy - d1ql(2,3,2)*djmy
        derv2(mz,jy) = derv2(mz,jy) + d1ql(2,1,2)*djmz - d1ql(2,3,2)*djmz
        derv2(mx,jz) = derv2(mx,jz) + d1ql(3,1,2)*djmx - d1ql(3,3,2)*djmx
        derv2(my,jz) = derv2(my,jz) + d1ql(3,1,2)*djmy - d1ql(3,3,2)*djmy
        derv2(mz,jz) = derv2(mz,jz) + d1ql(3,1,2)*djmz - d1ql(3,3,2)*djmz
!
        derv2(mx,jx) = derv2(mx,jx) + d1ql(1,1,3)*dkmx - d1ql(1,3,3)*dkmx
        derv2(my,jx) = derv2(my,jx) + d1ql(1,1,3)*dkmy - d1ql(1,3,3)*dkmy
        derv2(mz,jx) = derv2(mz,jx) + d1ql(1,1,3)*dkmz - d1ql(1,3,3)*dkmz
        derv2(mx,jy) = derv2(mx,jy) + d1ql(2,1,3)*dkmx - d1ql(2,3,3)*dkmx
        derv2(my,jy) = derv2(my,jy) + d1ql(2,1,3)*dkmy - d1ql(2,3,3)*dkmy
        derv2(mz,jy) = derv2(mz,jy) + d1ql(2,1,3)*dkmz - d1ql(2,3,3)*dkmz
        derv2(mx,jz) = derv2(mx,jz) + d1ql(3,1,3)*dkmx - d1ql(3,3,3)*dkmx
        derv2(my,jz) = derv2(my,jz) + d1ql(3,1,3)*dkmy - d1ql(3,3,3)*dkmy
        derv2(mz,jz) = derv2(mz,jz) + d1ql(3,1,3)*dkmz - d1ql(3,3,3)*dkmz
      else
        derv2(jx,mx) = derv2(jx,mx) + d1ql(1,1,1)*dimx - d1ql(1,3,1)*dimx
        derv2(jy,mx) = derv2(jy,mx) + d1ql(2,1,1)*dimx - d1ql(2,3,1)*dimx
        derv2(jz,mx) = derv2(jz,mx) + d1ql(3,1,1)*dimx - d1ql(3,3,1)*dimx
        derv2(jx,my) = derv2(jx,my) + d1ql(1,1,1)*dimy - d1ql(1,3,1)*dimy
        derv2(jy,my) = derv2(jy,my) + d1ql(2,1,1)*dimy - d1ql(2,3,1)*dimy
        derv2(jz,my) = derv2(jz,my) + d1ql(3,1,1)*dimy - d1ql(3,3,1)*dimy
        derv2(jx,mz) = derv2(jx,mz) + d1ql(1,1,1)*dimz - d1ql(1,3,1)*dimz
        derv2(jy,mz) = derv2(jy,mz) + d1ql(2,1,1)*dimz - d1ql(2,3,1)*dimz
        derv2(jz,mz) = derv2(jz,mz) + d1ql(3,1,1)*dimz - d1ql(3,3,1)*dimz
!
        derv2(jx,mx) = derv2(jx,mx) + d1ql(1,1,2)*djmx - d1ql(1,3,2)*djmx
        derv2(jy,mx) = derv2(jy,mx) + d1ql(2,1,2)*djmx - d1ql(2,3,2)*djmx
        derv2(jz,mx) = derv2(jz,mx) + d1ql(3,1,2)*djmx - d1ql(3,3,2)*djmx
        derv2(jx,my) = derv2(jx,my) + d1ql(1,1,2)*djmy - d1ql(1,3,2)*djmy
        derv2(jy,my) = derv2(jy,my) + d1ql(2,1,2)*djmy - d1ql(2,3,2)*djmy
        derv2(jz,my) = derv2(jz,my) + d1ql(3,1,2)*djmy - d1ql(3,3,2)*djmy
        derv2(jx,mz) = derv2(jx,mz) + d1ql(1,1,2)*djmz - d1ql(1,3,2)*djmz
        derv2(jy,mz) = derv2(jy,mz) + d1ql(2,1,2)*djmz - d1ql(2,3,2)*djmz
        derv2(jz,mz) = derv2(jz,mz) + d1ql(3,1,2)*djmz - d1ql(3,3,2)*djmz
!
        derv2(jx,mx) = derv2(jx,mx) + d1ql(1,1,3)*dkmx - d1ql(1,3,3)*dkmx
        derv2(jy,mx) = derv2(jy,mx) + d1ql(2,1,3)*dkmx - d1ql(2,3,3)*dkmx
        derv2(jz,mx) = derv2(jz,mx) + d1ql(3,1,3)*dkmx - d1ql(3,3,3)*dkmx
        derv2(jx,my) = derv2(jx,my) + d1ql(1,1,3)*dkmy - d1ql(1,3,3)*dkmy
        derv2(jy,my) = derv2(jy,my) + d1ql(2,1,3)*dkmy - d1ql(2,3,3)*dkmy
        derv2(jz,my) = derv2(jz,my) + d1ql(3,1,3)*dkmy - d1ql(3,3,3)*dkmy
        derv2(jx,mz) = derv2(jx,mz) + d1ql(1,1,3)*dkmz - d1ql(1,3,3)*dkmz
        derv2(jy,mz) = derv2(jy,mz) + d1ql(2,1,3)*dkmz - d1ql(2,3,3)*dkmz
        derv2(jz,mz) = derv2(jz,mz) + d1ql(3,1,3)*dkmz - d1ql(3,3,3)*dkmz
      endif
    endif
    if (k.ne.m) then
!
!  d2E/(d(alpha).dq) x dq/d(beta) : k-m
!
      if (m.le.k) then
        derv2(mx,kx) = derv2(mx,kx) + d1ql(1,2,1)*dimx + d1ql(1,3,1)*dimx
        derv2(my,kx) = derv2(my,kx) + d1ql(1,2,1)*dimy + d1ql(1,3,1)*dimy
        derv2(mz,kx) = derv2(mz,kx) + d1ql(1,2,1)*dimz + d1ql(1,3,1)*dimz
        derv2(mx,ky) = derv2(mx,ky) + d1ql(2,2,1)*dimx + d1ql(2,3,1)*dimx
        derv2(my,ky) = derv2(my,ky) + d1ql(2,2,1)*dimy + d1ql(2,3,1)*dimy
        derv2(mz,ky) = derv2(mz,ky) + d1ql(2,2,1)*dimz + d1ql(2,3,1)*dimz
        derv2(mx,kz) = derv2(mx,kz) + d1ql(3,2,1)*dimx + d1ql(3,3,1)*dimx
        derv2(my,kz) = derv2(my,kz) + d1ql(3,2,1)*dimy + d1ql(3,3,1)*dimy
        derv2(mz,kz) = derv2(mz,kz) + d1ql(3,2,1)*dimz + d1ql(3,3,1)*dimz
!
        derv2(mx,kx) = derv2(mx,kx) + d1ql(1,2,2)*djmx + d1ql(1,3,2)*djmx
        derv2(my,kx) = derv2(my,kx) + d1ql(1,2,2)*djmy + d1ql(1,3,2)*djmy
        derv2(mz,kx) = derv2(mz,kx) + d1ql(1,2,2)*djmz + d1ql(1,3,2)*djmz
        derv2(mx,ky) = derv2(mx,ky) + d1ql(2,2,2)*djmx + d1ql(2,3,2)*djmx
        derv2(my,ky) = derv2(my,ky) + d1ql(2,2,2)*djmy + d1ql(2,3,2)*djmy
        derv2(mz,ky) = derv2(mz,ky) + d1ql(2,2,2)*djmz + d1ql(2,3,2)*djmz
        derv2(mx,kz) = derv2(mx,kz) + d1ql(3,2,2)*djmx + d1ql(3,3,2)*djmx
        derv2(my,kz) = derv2(my,kz) + d1ql(3,2,2)*djmy + d1ql(3,3,2)*djmy
        derv2(mz,kz) = derv2(mz,kz) + d1ql(3,2,2)*djmz + d1ql(3,3,2)*djmz
!
        derv2(mx,kx) = derv2(mx,kx) + d1ql(1,2,3)*dkmx + d1ql(1,3,3)*dkmx
        derv2(my,kx) = derv2(my,kx) + d1ql(1,2,3)*dkmy + d1ql(1,3,3)*dkmy
        derv2(mz,kx) = derv2(mz,kx) + d1ql(1,2,3)*dkmz + d1ql(1,3,3)*dkmz
        derv2(mx,ky) = derv2(mx,ky) + d1ql(2,2,3)*dkmx + d1ql(2,3,3)*dkmx
        derv2(my,ky) = derv2(my,ky) + d1ql(2,2,3)*dkmy + d1ql(2,3,3)*dkmy
        derv2(mz,ky) = derv2(mz,ky) + d1ql(2,2,3)*dkmz + d1ql(2,3,3)*dkmz
        derv2(mx,kz) = derv2(mx,kz) + d1ql(3,2,3)*dkmx + d1ql(3,3,3)*dkmx
        derv2(my,kz) = derv2(my,kz) + d1ql(3,2,3)*dkmy + d1ql(3,3,3)*dkmy
        derv2(mz,kz) = derv2(mz,kz) + d1ql(3,2,3)*dkmz + d1ql(3,3,3)*dkmz
      else
        derv2(kx,mx) = derv2(kx,mx) + d1ql(1,2,1)*dimx + d1ql(1,3,1)*dimx
        derv2(ky,mx) = derv2(ky,mx) + d1ql(2,2,1)*dimx + d1ql(2,3,1)*dimx
        derv2(kz,mx) = derv2(kz,mx) + d1ql(3,2,1)*dimx + d1ql(3,3,1)*dimx
        derv2(kx,my) = derv2(kx,my) + d1ql(1,2,1)*dimy + d1ql(1,3,1)*dimy
        derv2(ky,my) = derv2(ky,my) + d1ql(2,2,1)*dimy + d1ql(2,3,1)*dimy
        derv2(kz,my) = derv2(kz,my) + d1ql(3,2,1)*dimy + d1ql(3,3,1)*dimy
        derv2(kx,mz) = derv2(kx,mz) + d1ql(1,2,1)*dimz + d1ql(1,3,1)*dimz
        derv2(ky,mz) = derv2(ky,mz) + d1ql(2,2,1)*dimz + d1ql(2,3,1)*dimz
        derv2(kz,mz) = derv2(kz,mz) + d1ql(3,2,1)*dimz + d1ql(3,3,1)*dimz
!
        derv2(kx,mx) = derv2(kx,mx) + d1ql(1,2,2)*djmx + d1ql(1,3,2)*djmx
        derv2(ky,mx) = derv2(ky,mx) + d1ql(2,2,2)*djmx + d1ql(2,3,2)*djmx
        derv2(kz,mx) = derv2(kz,mx) + d1ql(3,2,2)*djmx + d1ql(3,3,2)*djmx
        derv2(kx,my) = derv2(kx,my) + d1ql(1,2,2)*djmy + d1ql(1,3,2)*djmy
        derv2(ky,my) = derv2(ky,my) + d1ql(2,2,2)*djmy + d1ql(2,3,2)*djmy
        derv2(kz,my) = derv2(kz,my) + d1ql(3,2,2)*djmy + d1ql(3,3,2)*djmy
        derv2(kx,mz) = derv2(kx,mz) + d1ql(1,2,2)*djmz + d1ql(1,3,2)*djmz
        derv2(ky,mz) = derv2(ky,mz) + d1ql(2,2,2)*djmz + d1ql(2,3,2)*djmz
        derv2(kz,mz) = derv2(kz,mz) + d1ql(3,2,2)*djmz + d1ql(3,3,2)*djmz
!
        derv2(kx,mx) = derv2(kx,mx) + d1ql(1,2,3)*dkmx + d1ql(1,3,3)*dkmx
        derv2(ky,mx) = derv2(ky,mx) + d1ql(2,2,3)*dkmx + d1ql(2,3,3)*dkmx
        derv2(kz,mx) = derv2(kz,mx) + d1ql(3,2,3)*dkmx + d1ql(3,3,3)*dkmx
        derv2(kx,my) = derv2(kx,my) + d1ql(1,2,3)*dkmy + d1ql(1,3,3)*dkmy
        derv2(ky,my) = derv2(ky,my) + d1ql(2,2,3)*dkmy + d1ql(2,3,3)*dkmy
        derv2(kz,my) = derv2(kz,my) + d1ql(3,2,3)*dkmy + d1ql(3,3,3)*dkmy
        derv2(kx,mz) = derv2(kx,mz) + d1ql(1,2,3)*dkmz + d1ql(1,3,3)*dkmz
        derv2(ky,mz) = derv2(ky,mz) + d1ql(2,2,3)*dkmz + d1ql(2,3,3)*dkmz
        derv2(kz,mz) = derv2(kz,mz) + d1ql(3,2,3)*dkmz + d1ql(3,3,3)*dkmz
      endif
    endif
!
!  Loop over atoms to add ij-ml correction
!
    lx = - 2
    ly = - 1
    lz =   0
    do l = 1,m-1
      lx = lx + 3
      ly = ly + 3
      lz = lz + 3
!
!  d2E/(dq.dq) x dq/d(alpha) x dq/d(beta) 
!
      dilx = dqdxyz(lx,i)
      dily = dqdxyz(ly,i)
      dilz = dqdxyz(lz,i)
      djlx = dqdxyz(lx,j)
      djly = dqdxyz(ly,j)
      djlz = dqdxyz(lz,j)
      dklx = dqdxyz(lx,k)
      dkly = dqdxyz(ly,k)
      dklz = dqdxyz(lz,k)
!
      derv2(lx,mx) = derv2(lx,mx) + d2ijs*dilx*djmx + d2iks*dilx*dkmx + d2jks*djlx*dkmx
      derv2(ly,mx) = derv2(ly,mx) + d2ijs*dilx*djmy + d2iks*dilx*dkmy + d2jks*djlx*dkmy
      derv2(lz,mx) = derv2(lz,mx) + d2ijs*dilx*djmz + d2iks*dilx*dkmz + d2jks*djlx*dkmz
      derv2(lx,my) = derv2(lx,my) + d2ijs*dily*djmx + d2iks*dily*dkmx + d2jks*djly*dkmx
      derv2(ly,my) = derv2(ly,my) + d2ijs*dily*djmy + d2iks*dily*dkmy + d2jks*djly*dkmy
      derv2(lz,my) = derv2(lz,my) + d2ijs*dily*djmz + d2iks*dily*dkmz + d2jks*djly*dkmz
      derv2(lx,mz) = derv2(lx,mz) + d2ijs*dilz*djmx + d2iks*dilz*dkmx + d2jks*djlz*dkmx
      derv2(ly,mz) = derv2(ly,mz) + d2ijs*dilz*djmy + d2iks*dilz*dkmy + d2jks*djlz*dkmy
      derv2(lz,mz) = derv2(lz,mz) + d2ijs*dilz*djmz + d2iks*dilz*dkmz + d2jks*djlz*dkmz
!
      derv2(lx,mx) = derv2(lx,mx) + d2ijs*djlx*dimx + d2iks*dklx*dimx + d2jks*dklx*djmx
      derv2(ly,mx) = derv2(ly,mx) + d2ijs*djlx*dimy + d2iks*dklx*dimy + d2jks*dklx*djmy
      derv2(lz,mx) = derv2(lz,mx) + d2ijs*djlx*dimz + d2iks*dklx*dimz + d2jks*dklx*djmz
      derv2(lx,my) = derv2(lx,my) + d2ijs*djly*dimx + d2iks*dkly*dimx + d2jks*dkly*djmx
      derv2(ly,my) = derv2(ly,my) + d2ijs*djly*dimy + d2iks*dkly*dimy + d2jks*dkly*djmy
      derv2(lz,my) = derv2(lz,my) + d2ijs*djly*dimz + d2iks*dkly*dimz + d2jks*dkly*djmz
      derv2(lx,mz) = derv2(lx,mz) + d2ijs*djlz*dimx + d2iks*dklz*dimx + d2jks*dklz*djmx
      derv2(ly,mz) = derv2(ly,mz) + d2ijs*djlz*dimy + d2iks*dklz*dimy + d2jks*dklz*djmy
      derv2(lz,mz) = derv2(lz,mz) + d2ijs*djlz*dimz + d2iks*dklz*dimz + d2jks*dklz*djmz
!
!  d2Edi2/d2Edj2/d2Edk2
!
      if (abs(d2i2s).gt.1.0d-8) then
        derv2(lx,mx) = derv2(lx,mx) + d2i2s*dilx*dimx
        derv2(ly,mx) = derv2(ly,mx) + d2i2s*dily*dimx
        derv2(lz,mx) = derv2(lz,mx) + d2i2s*dilz*dimx
        derv2(lx,my) = derv2(lx,my) + d2i2s*dilx*dimy
        derv2(ly,my) = derv2(ly,my) + d2i2s*dily*dimy
        derv2(lz,my) = derv2(lz,my) + d2i2s*dilz*dimy
        derv2(lx,mz) = derv2(lx,mz) + d2i2s*dilx*dimz
        derv2(ly,mz) = derv2(ly,mz) + d2i2s*dily*dimz
        derv2(lz,mz) = derv2(lz,mz) + d2i2s*dilz*dimz
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        derv2(lx,mx) = derv2(lx,mx) + d2j2s*djlx*djmx
        derv2(ly,mx) = derv2(ly,mx) + d2j2s*djly*djmx
        derv2(lz,mx) = derv2(lz,mx) + d2j2s*djlz*djmx
        derv2(lx,my) = derv2(lx,my) + d2j2s*djlx*djmy
        derv2(ly,my) = derv2(ly,my) + d2j2s*djly*djmy
        derv2(lz,my) = derv2(lz,my) + d2j2s*djlz*djmy
        derv2(lx,mz) = derv2(lx,mz) + d2j2s*djlx*djmz
        derv2(ly,mz) = derv2(ly,mz) + d2j2s*djly*djmz
        derv2(lz,mz) = derv2(lz,mz) + d2j2s*djlz*djmz
      endif
      if (abs(d2k2s).gt.1.0d-8.and.i.ne.k.and.j.ne.k) then
        derv2(lx,mx) = derv2(lx,mx) + d2k2s*dklx*dkmx
        derv2(ly,mx) = derv2(ly,mx) + d2k2s*dkly*dkmx
        derv2(lz,mx) = derv2(lz,mx) + d2k2s*dklz*dkmx
        derv2(lx,my) = derv2(lx,my) + d2k2s*dklx*dkmy
        derv2(ly,my) = derv2(ly,my) + d2k2s*dkly*dkmy
        derv2(lz,my) = derv2(lz,my) + d2k2s*dklz*dkmy
        derv2(lx,mz) = derv2(lx,mz) + d2k2s*dklx*dkmz
        derv2(ly,mz) = derv2(ly,mz) + d2k2s*dkly*dkmz
        derv2(lz,mz) = derv2(lz,mz) + d2k2s*dklz*dkmz
      endif
!
!  End of loop over l
!
    enddo
!
!  End of loop over m
!
  enddo
!
  return
  end
