  subroutine d2charge3(i,j,k,nor,ix,iy,iz,jx,jy,jz,kx,ky,kz,lopi,lopj,lopk,dei,dej,dek, &
                       d1q,d1qs,d2q)
!
!  Calculates the contribution to the second derivative matrices
!  due to charge derivatives from variable charge models as a 
!  result of three body potentials.
!
!  10/04 Created from d2charge
!  10/04 Corrections for sderv2, as applied to d2charge applied here
!   7/05 lReverse set to true for k.le.(i.or.j)
!  11/07 Unused variables removed
!
!  On entry:
!
!  d1q(3,3,3) = derivative of first derivative of i - j vector with respect to charge
!  d1qs(6,3)  = derivative of first strain derivative with respect to charge
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
  use optimisation
  use symmetry
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
  logical,     intent(in)    :: lopi
  logical,     intent(in)    :: lopj
  logical,     intent(in)    :: lopk
  real(dp),    intent(in)    :: d1q(3,3,3)
  real(dp),    intent(in)    :: d1qs(6,3)
  real(dp),    intent(in)    :: d2q(6,*)
  real(dp),    intent(in)    :: dei(*)
  real(dp),    intent(in)    :: dej(*)
  real(dp),    intent(in)    :: dek(*)
!
!  Local variables
!
  integer(i4)                :: ind
  integer(i4)                :: indl
  integer(i4)                :: indm
  integer(i4)                :: iv
  integer(i4)                :: ixl
  integer(i4)                :: iyl
  integer(i4)                :: izl
  integer(i4)                :: jxl
  integer(i4)                :: jyl
  integer(i4)                :: jzl
  integer(i4)                :: kk
  integer(i4)                :: kl
  integer(i4)                :: kxl
  integer(i4)                :: kyl
  integer(i4)                :: kzl
  integer(i4)                :: l
  integer(i4)                :: lx
  integer(i4)                :: ly
  integer(i4)                :: lz
  integer(i4)                :: lxl
  integer(i4)                :: lyl
  integer(i4)                :: lzl
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
  integer(i4)                :: mxl
  integer(i4)                :: myl
  integer(i4)                :: mzl
  integer(i4)                :: n
  integer(i4)                :: nx
  integer(i4)                :: p
  integer(i4)                :: px
  integer(i4)                :: py
  integer(i4)                :: pz
  logical                    :: lopl
  logical                    :: lopm
  logical                    :: lNonIJQDeriv
  logical                    :: lReverse
  real(dp)                   :: d1ql(3,3,3)
  real(dp)                   :: d1qsl(6,3)
  real(dp)                   :: d1si
  real(dp)                   :: d2si
  real(dp)                   :: d3si
  real(dp)                   :: d4si
  real(dp)                   :: d5si
  real(dp)                   :: d6si
  real(dp)                   :: d1sj
  real(dp)                   :: d2sj
  real(dp)                   :: d3sj
  real(dp)                   :: d4sj
  real(dp)                   :: d5sj
  real(dp)                   :: d6sj
  real(dp)                   :: d1sk
  real(dp)                   :: d2sk
  real(dp)                   :: d3sk
  real(dp)                   :: d4sk
  real(dp)                   :: d5sk
  real(dp)                   :: d6sk
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
  real(dp)                   :: dis1
  real(dp)                   :: dis2
  real(dp)                   :: dis3
  real(dp)                   :: dis4
  real(dp)                   :: dis5
  real(dp)                   :: dis6
  real(dp)                   :: djlx
  real(dp)                   :: djly
  real(dp)                   :: djlz
  real(dp)                   :: djmx
  real(dp)                   :: djmy
  real(dp)                   :: djmz
  real(dp)                   :: djs1
  real(dp)                   :: djs2
  real(dp)                   :: djs3
  real(dp)                   :: djs4
  real(dp)                   :: djs5
  real(dp)                   :: djs6
  real(dp)                   :: dklx
  real(dp)                   :: dkly
  real(dp)                   :: dklz
  real(dp)                   :: dkmx
  real(dp)                   :: dkmy
  real(dp)                   :: dkmz
  real(dp)                   :: dks1
  real(dp)                   :: dks2
  real(dp)                   :: dks3
  real(dp)                   :: dks4
  real(dp)                   :: dks5
  real(dp)                   :: dks6
  real(dp)                   :: xdrvm
  real(dp)                   :: ydrvm
  real(dp)                   :: zdrvm
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
  if (lstr) d1qsl(1:nstrains,1:3) = d1qs(1:nstrains,1:3)
  if ((leem.and..not.lelementOK(nat(i))).or.nregionno(nsft+nrelat(i)).ne.1) then
    d2i2s = 0.0_dp
    d2ijs = 0.0_dp
    d2iks = 0.0_dp
    d1ql(1:3,1:3,1) = 0.0_dp
    if (lstr) d1qsl(1:nstrains,1) = 0.0_dp
  endif
  if ((leem.and..not.lelementOK(nat(j))).or.nregionno(nsft+nrelat(j)).ne.1) then
    d2ijs = 0.0_dp
    d2j2s = 0.0_dp
    d2jks = 0.0_dp
    d1ql(1:3,1:3,2) = 0.0_dp
    if (lstr) d1qsl(1:nstrains,2) = 0.0_dp
  endif
  if ((leem.and..not.lelementOK(nat(k))).or.nregionno(nsft+nrelat(k)).ne.1) then
    d2iks = 0.0_dp
    d2jks = 0.0_dp
    d2k2s = 0.0_dp
    d1ql(1:3,1:3,3) = 0.0_dp
    if (lstr) d1qsl(1:nstrains,3) = 0.0_dp
  endif
  if (lstr) then
    if (ndim.eq.3) then
      dis1 = dqds(1,i)
      dis2 = dqds(2,i)
      dis3 = dqds(3,i)
      dis4 = dqds(4,i)
      dis5 = dqds(5,i)
      dis6 = dqds(6,i)
      djs1 = dqds(1,j)
      djs2 = dqds(2,j)
      djs3 = dqds(3,j)
      djs4 = dqds(4,j)
      djs5 = dqds(5,j)
      djs6 = dqds(6,j)
      dks1 = dqds(1,k)
      dks2 = dqds(2,k)
      dks3 = dqds(3,k)
      dks4 = dqds(4,k)
      dks5 = dqds(5,k)
      dks6 = dqds(6,k)
    elseif (ndim.eq.2) then
      dis1 = dqds(1,i)
      dis2 = dqds(2,i)
      dis3 = dqds(3,i)
      djs1 = dqds(1,j)
      djs2 = dqds(2,j)
      djs3 = dqds(3,j)
      dks1 = dqds(1,k)
      dks2 = dqds(2,k)
      dks3 = dqds(3,k)
    elseif (ndim.eq.1) then
      dis1 = dqds(1,i)
      djs1 = dqds(1,j)
      dks1 = dqds(1,k)
    endif
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
!  Strain - strain contribution  
!     
    if (lstr.and.ndim.gt.0) then
      ind = 0                
      do kk = 1,nstrains     
        do kl = 1,kk-1       
          ind = ind + 1      
          sderv2(kl,kk) = sderv2(kl,kk) + deisum*d2qds2(ind,i) + dejsum*d2qds2(ind,j) + deksum*d2qds2(ind,k)
          sderv2(kk,kl) = sderv2(kk,kl) + deisum*d2qds2(ind,i) + dejsum*d2qds2(ind,j) + deksum*d2qds2(ind,k)
        enddo                
        ind = ind + 1        
        sderv2(kk,kk) = sderv2(kk,kk) + deisum*d2qds2(ind,i) + dejsum*d2qds2(ind,j) + deksum*d2qds2(ind,k)
      enddo                  
    endif
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
!
!  Mixed strain contribution
!
      if (lstr.and.ndim.gt.0) then
        do kk = 1,nstrains
          derv3(px,kk) = derv3(px,kk) + deisum*d2qdxyzs(kk,mx,i)
          derv3(py,kk) = derv3(py,kk) + deisum*d2qdxyzs(kk,my,i)
          derv3(pz,kk) = derv3(pz,kk) + deisum*d2qdxyzs(kk,mz,i)
          derv3(ix,kk) = derv3(ix,kk) - deisum*d2qdxyzs(kk,mx,i)
          derv3(iy,kk) = derv3(iy,kk) - deisum*d2qdxyzs(kk,my,i)
          derv3(iz,kk) = derv3(iz,kk) - deisum*d2qdxyzs(kk,mz,i)
        enddo
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
!
!  Mixed - strain contribution
!
      if (lstr.and.ndim.gt.0) then
        do kk = 1,nstrains
          derv3(px,kk) = derv3(px,kk) + dejsum*d2qdxyzs(kk,mx,j)
          derv3(py,kk) = derv3(py,kk) + dejsum*d2qdxyzs(kk,my,j)
          derv3(pz,kk) = derv3(pz,kk) + dejsum*d2qdxyzs(kk,mz,j)
          derv3(jx,kk) = derv3(jx,kk) - dejsum*d2qdxyzs(kk,mx,j)
          derv3(jy,kk) = derv3(jy,kk) - dejsum*d2qdxyzs(kk,my,j)
          derv3(jz,kk) = derv3(jz,kk) - dejsum*d2qdxyzs(kk,mz,j)
        enddo
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
!
!  Mixed - strain contribution
!
      if (lstr.and.ndim.gt.0) then
        do kk = 1,nstrains
          derv3(px,kk) = derv3(px,kk) + deksum*d2qdxyzs(kk,mx,k)
          derv3(py,kk) = derv3(py,kk) + deksum*d2qdxyzs(kk,my,k)
          derv3(pz,kk) = derv3(pz,kk) + deksum*d2qdxyzs(kk,mz,k)
          derv3(kx,kk) = derv3(kx,kk) - deksum*d2qdxyzs(kk,mx,k)
          derv3(ky,kk) = derv3(ky,kk) - deksum*d2qdxyzs(kk,my,k)
          derv3(kz,kk) = derv3(kz,kk) - deksum*d2qdxyzs(kk,mz,k)
        enddo
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
!  Note indices must be set to cope with the fact that only
!  half of derv2 is constructed in upper triangle and the
!  effects of lopi/lopj/lopk if freezing is being used.
!
  mx = - 2
  my = - 1
  mz =   0
  do m = 1,numat
    lopm = (.not.lfreeze.or.lopf(m))
    indm = 3*(m-1)
    if (lopm) then
      mx = mx + 3
      my = my + 3
      mz = mz + 3
    endif
    dimx = dqdxyz(indm+1,i)
    dimy = dqdxyz(indm+2,i)
    dimz = dqdxyz(indm+3,i)
    djmx = dqdxyz(indm+1,j)
    djmy = dqdxyz(indm+2,j)
    djmz = dqdxyz(indm+3,j)
    dkmx = dqdxyz(indm+1,k)
    dkmy = dqdxyz(indm+2,k)
    dkmz = dqdxyz(indm+3,k)
    if (i.ne.m.and.(lopi.or.lopm)) then
      if (lopi.and.lopm) then
        if (m.le.i) then
          ixl = ix
          iyl = iy
          izl = iz
          mxl = mx
          myl = my
          mzl = mz
          lReverse = .true.
        else
          ixl = mx
          iyl = my
          izl = mz
          mxl = ix
          myl = iy
          mzl = iz
          lReverse = .true.
        endif
      elseif (lopi) then
        ixl = ix
        iyl = iy
        izl = iz
        mxl = ix
        myl = iy
        mzl = iz
        lReverse = .false.
      elseif (lopm) then
        ixl = mx
        iyl = my
        izl = mz
        mxl = mx
        myl = my
        mzl = mz
        lReverse = .true.
      endif
!
!  d2E/(d(alpha).dq) x dq/d(beta) : i-m
!
      if (lReverse) then
        derv2(mxl,ixl) = derv2(mxl,ixl) - d1ql(1,1,1)*dimx - d1ql(1,2,1)*dimx
        derv2(myl,ixl) = derv2(myl,ixl) - d1ql(2,1,1)*dimx - d1ql(2,2,1)*dimx
        derv2(mzl,ixl) = derv2(mzl,ixl) - d1ql(3,1,1)*dimx - d1ql(3,2,1)*dimx
        derv2(mxl,iyl) = derv2(mxl,iyl) - d1ql(1,1,1)*dimy - d1ql(1,2,1)*dimy
        derv2(myl,iyl) = derv2(myl,iyl) - d1ql(2,1,1)*dimy - d1ql(2,2,1)*dimy
        derv2(mzl,iyl) = derv2(mzl,iyl) - d1ql(3,1,1)*dimy - d1ql(3,2,1)*dimy
        derv2(mxl,izl) = derv2(mxl,izl) - d1ql(1,1,1)*dimz - d1ql(1,2,1)*dimz
        derv2(myl,izl) = derv2(myl,izl) - d1ql(2,1,1)*dimz - d1ql(2,2,1)*dimz
        derv2(mzl,izl) = derv2(mzl,izl) - d1ql(3,1,1)*dimz - d1ql(3,2,1)*dimz
!
        derv2(mxl,ixl) = derv2(mxl,ixl) - d1ql(1,1,2)*djmx - d1ql(1,2,2)*djmx
        derv2(myl,ixl) = derv2(myl,ixl) - d1ql(2,1,2)*djmx - d1ql(2,2,2)*djmx
        derv2(mzl,ixl) = derv2(mzl,ixl) - d1ql(3,1,2)*djmx - d1ql(3,2,2)*djmx
        derv2(mxl,iyl) = derv2(mxl,iyl) - d1ql(1,1,2)*djmy - d1ql(1,2,2)*djmy
        derv2(myl,iyl) = derv2(myl,iyl) - d1ql(2,1,2)*djmy - d1ql(2,2,2)*djmy
        derv2(mzl,iyl) = derv2(mzl,iyl) - d1ql(3,1,2)*djmy - d1ql(3,2,2)*djmy
        derv2(mxl,izl) = derv2(mxl,izl) - d1ql(1,1,2)*djmz - d1ql(1,2,2)*djmz
        derv2(myl,izl) = derv2(myl,izl) - d1ql(2,1,2)*djmz - d1ql(2,2,2)*djmz
        derv2(mzl,izl) = derv2(mzl,izl) - d1ql(3,1,2)*djmz - d1ql(3,2,2)*djmz
!
        derv2(mxl,ixl) = derv2(mxl,ixl) - d1ql(1,1,3)*dkmx - d1ql(1,2,3)*dkmx
        derv2(myl,ixl) = derv2(myl,ixl) - d1ql(2,1,3)*dkmx - d1ql(2,2,3)*dkmx
        derv2(mzl,ixl) = derv2(mzl,ixl) - d1ql(3,1,3)*dkmx - d1ql(3,2,3)*dkmx
        derv2(mxl,iyl) = derv2(mxl,iyl) - d1ql(1,1,3)*dkmy - d1ql(1,2,3)*dkmy
        derv2(myl,iyl) = derv2(myl,iyl) - d1ql(2,1,3)*dkmy - d1ql(2,2,3)*dkmy
        derv2(mzl,iyl) = derv2(mzl,iyl) - d1ql(3,1,3)*dkmy - d1ql(3,2,3)*dkmy
        derv2(mxl,izl) = derv2(mxl,izl) - d1ql(1,1,3)*dkmz - d1ql(1,2,3)*dkmz
        derv2(myl,izl) = derv2(myl,izl) - d1ql(2,1,3)*dkmz - d1ql(2,2,3)*dkmz
        derv2(mzl,izl) = derv2(mzl,izl) - d1ql(3,1,3)*dkmz - d1ql(3,2,3)*dkmz
      else
        derv2(mxl,ixl) = derv2(mxl,ixl) - d1ql(1,1,1)*dimx - d1ql(1,2,1)*dimx
        derv2(myl,ixl) = derv2(myl,ixl) - d1ql(1,1,1)*dimy - d1ql(1,2,1)*dimy
        derv2(mzl,ixl) = derv2(mzl,ixl) - d1ql(1,1,1)*dimz - d1ql(1,2,1)*dimz
        derv2(mxl,iyl) = derv2(mxl,iyl) - d1ql(2,1,1)*dimx - d1ql(2,2,1)*dimx
        derv2(myl,iyl) = derv2(myl,iyl) - d1ql(2,1,1)*dimy - d1ql(2,2,1)*dimy
        derv2(mzl,iyl) = derv2(mzl,iyl) - d1ql(2,1,1)*dimz - d1ql(2,2,1)*dimz
        derv2(mxl,izl) = derv2(mxl,izl) - d1ql(3,1,1)*dimx - d1ql(3,2,1)*dimx
        derv2(myl,izl) = derv2(myl,izl) - d1ql(3,1,1)*dimy - d1ql(3,2,1)*dimy
        derv2(mzl,izl) = derv2(mzl,izl) - d1ql(3,1,1)*dimz - d1ql(3,2,1)*dimz
!
        derv2(mxl,ixl) = derv2(mxl,ixl) - d1ql(1,1,2)*djmx - d1ql(1,2,2)*djmx
        derv2(myl,ixl) = derv2(myl,ixl) - d1ql(1,1,2)*djmy - d1ql(1,2,2)*djmy
        derv2(mzl,ixl) = derv2(mzl,ixl) - d1ql(1,1,2)*djmz - d1ql(1,2,2)*djmz
        derv2(mxl,iyl) = derv2(mxl,iyl) - d1ql(2,1,2)*djmx - d1ql(2,2,2)*djmx
        derv2(myl,iyl) = derv2(myl,iyl) - d1ql(2,1,2)*djmy - d1ql(2,2,2)*djmy
        derv2(mzl,iyl) = derv2(mzl,iyl) - d1ql(2,1,2)*djmz - d1ql(2,2,2)*djmz
        derv2(mxl,izl) = derv2(mxl,izl) - d1ql(3,1,2)*djmx - d1ql(3,2,2)*djmx
        derv2(myl,izl) = derv2(myl,izl) - d1ql(3,1,2)*djmy - d1ql(3,2,2)*djmy
        derv2(mzl,izl) = derv2(mzl,izl) - d1ql(3,1,2)*djmz - d1ql(3,2,2)*djmz
!
        derv2(mxl,ixl) = derv2(mxl,ixl) - d1ql(1,1,3)*dkmx - d1ql(1,2,3)*dkmx
        derv2(myl,ixl) = derv2(myl,ixl) - d1ql(1,1,3)*dkmy - d1ql(1,2,3)*dkmy
        derv2(mzl,ixl) = derv2(mzl,ixl) - d1ql(1,1,3)*dkmz - d1ql(1,2,3)*dkmz
        derv2(mxl,iyl) = derv2(mxl,iyl) - d1ql(2,1,3)*dkmx - d1ql(2,2,3)*dkmx
        derv2(myl,iyl) = derv2(myl,iyl) - d1ql(2,1,3)*dkmy - d1ql(2,2,3)*dkmy
        derv2(mzl,iyl) = derv2(mzl,iyl) - d1ql(2,1,3)*dkmz - d1ql(2,2,3)*dkmz
        derv2(mxl,izl) = derv2(mxl,izl) - d1ql(3,1,3)*dkmx - d1ql(3,2,3)*dkmx
        derv2(myl,izl) = derv2(myl,izl) - d1ql(3,1,3)*dkmy - d1ql(3,2,3)*dkmy
        derv2(mzl,izl) = derv2(mzl,izl) - d1ql(3,1,3)*dkmz - d1ql(3,2,3)*dkmz
      endif
    endif
    if (j.ne.m.and.(lopj.or.lopm)) then
      if (lopj.and.lopm) then
        if (m.le.j) then
          jxl = jx
          jyl = jy
          jzl = jz
          mxl = mx
          myl = my
          mzl = mz
          lReverse = .true.
        else
          jxl = mx
          jyl = my
          jzl = mz
          mxl = jx
          myl = jy
          mzl = jz
          lReverse = .true.
        endif
      elseif (lopj) then
        jxl = jx
        jyl = jy
        jzl = jz
        mxl = jx
        myl = jy
        mzl = jz
        lReverse = .false.
      elseif (lopm) then
        jxl = mx
        jyl = my
        jzl = mz
        mxl = mx
        myl = my
        mzl = mz
        lReverse = .true.
      endif
!
!  d2E/(d(alpha).dq) x dq/d(beta) : j-m
!
      if (lReverse) then
        derv2(mxl,jxl) = derv2(mxl,jxl) + d1ql(1,1,1)*dimx - d1ql(1,3,1)*dimx
        derv2(myl,jxl) = derv2(myl,jxl) + d1ql(2,1,1)*dimx - d1ql(2,3,1)*dimx
        derv2(mzl,jxl) = derv2(mzl,jxl) + d1ql(3,1,1)*dimx - d1ql(3,3,1)*dimx
        derv2(mxl,jyl) = derv2(mxl,jyl) + d1ql(1,1,1)*dimy - d1ql(1,3,1)*dimy
        derv2(myl,jyl) = derv2(myl,jyl) + d1ql(2,1,1)*dimy - d1ql(2,3,1)*dimy
        derv2(mzl,jyl) = derv2(mzl,jyl) + d1ql(3,1,1)*dimy - d1ql(3,3,1)*dimy
        derv2(mxl,jzl) = derv2(mxl,jzl) + d1ql(1,1,1)*dimz - d1ql(1,3,1)*dimz
        derv2(myl,jzl) = derv2(myl,jzl) + d1ql(2,1,1)*dimz - d1ql(2,3,1)*dimz
        derv2(mzl,jzl) = derv2(mzl,jzl) + d1ql(3,1,1)*dimz - d1ql(3,3,1)*dimz
!
        derv2(mxl,jxl) = derv2(mxl,jxl) + d1ql(1,1,2)*djmx - d1ql(1,3,2)*djmx
        derv2(myl,jxl) = derv2(myl,jxl) + d1ql(2,1,2)*djmx - d1ql(2,3,2)*djmx
        derv2(mzl,jxl) = derv2(mzl,jxl) + d1ql(3,1,2)*djmx - d1ql(3,3,2)*djmx
        derv2(mxl,jyl) = derv2(mxl,jyl) + d1ql(1,1,2)*djmy - d1ql(1,3,2)*djmy
        derv2(myl,jyl) = derv2(myl,jyl) + d1ql(2,1,2)*djmy - d1ql(2,3,2)*djmy
        derv2(mzl,jyl) = derv2(mzl,jyl) + d1ql(3,1,2)*djmy - d1ql(3,3,2)*djmy
        derv2(mxl,jzl) = derv2(mxl,jzl) + d1ql(1,1,2)*djmz - d1ql(1,3,2)*djmz
        derv2(myl,jzl) = derv2(myl,jzl) + d1ql(2,1,2)*djmz - d1ql(2,3,2)*djmz
        derv2(mzl,jzl) = derv2(mzl,jzl) + d1ql(3,1,2)*djmz - d1ql(3,3,2)*djmz
!
        derv2(mxl,jxl) = derv2(mxl,jxl) + d1ql(1,1,3)*dkmx - d1ql(1,3,3)*dkmx
        derv2(myl,jxl) = derv2(myl,jxl) + d1ql(2,1,3)*dkmx - d1ql(2,3,3)*dkmx
        derv2(mzl,jxl) = derv2(mzl,jxl) + d1ql(3,1,3)*dkmx - d1ql(3,3,3)*dkmx
        derv2(mxl,jyl) = derv2(mxl,jyl) + d1ql(1,1,3)*dkmy - d1ql(1,3,3)*dkmy
        derv2(myl,jyl) = derv2(myl,jyl) + d1ql(2,1,3)*dkmy - d1ql(2,3,3)*dkmy
        derv2(mzl,jyl) = derv2(mzl,jyl) + d1ql(3,1,3)*dkmy - d1ql(3,3,3)*dkmy
        derv2(mxl,jzl) = derv2(mxl,jzl) + d1ql(1,1,3)*dkmz - d1ql(1,3,3)*dkmz
        derv2(myl,jzl) = derv2(myl,jzl) + d1ql(2,1,3)*dkmz - d1ql(2,3,3)*dkmz
        derv2(mzl,jzl) = derv2(mzl,jzl) + d1ql(3,1,3)*dkmz - d1ql(3,3,3)*dkmz
      else
        derv2(mxl,jxl) = derv2(mxl,jxl) + d1ql(1,1,1)*dimx - d1ql(1,3,1)*dimx
        derv2(myl,jxl) = derv2(myl,jxl) + d1ql(1,1,1)*dimy - d1ql(1,3,1)*dimy
        derv2(mzl,jxl) = derv2(mzl,jxl) + d1ql(1,1,1)*dimz - d1ql(1,3,1)*dimz
        derv2(mxl,jyl) = derv2(mxl,jyl) + d1ql(2,1,1)*dimx - d1ql(2,3,1)*dimx
        derv2(myl,jyl) = derv2(myl,jyl) + d1ql(2,1,1)*dimy - d1ql(2,3,1)*dimy
        derv2(mzl,jyl) = derv2(mzl,jyl) + d1ql(2,1,1)*dimz - d1ql(2,3,1)*dimz
        derv2(mxl,jzl) = derv2(mxl,jzl) + d1ql(3,1,1)*dimx - d1ql(3,3,1)*dimx
        derv2(myl,jzl) = derv2(myl,jzl) + d1ql(3,1,1)*dimy - d1ql(3,3,1)*dimy
        derv2(mzl,jzl) = derv2(mzl,jzl) + d1ql(3,1,1)*dimz - d1ql(3,3,1)*dimz
!
        derv2(mxl,jxl) = derv2(mxl,jxl) + d1ql(1,1,2)*djmx - d1ql(1,3,2)*djmx
        derv2(myl,jxl) = derv2(myl,jxl) + d1ql(1,1,2)*djmy - d1ql(1,3,2)*djmy
        derv2(mzl,jxl) = derv2(mzl,jxl) + d1ql(1,1,2)*djmz - d1ql(1,3,2)*djmz
        derv2(mxl,jyl) = derv2(mxl,jyl) + d1ql(2,1,2)*djmx - d1ql(2,3,2)*djmx
        derv2(myl,jyl) = derv2(myl,jyl) + d1ql(2,1,2)*djmy - d1ql(2,3,2)*djmy
        derv2(mzl,jyl) = derv2(mzl,jyl) + d1ql(2,1,2)*djmz - d1ql(2,3,2)*djmz
        derv2(mxl,jzl) = derv2(mxl,jzl) + d1ql(3,1,2)*djmx - d1ql(3,3,2)*djmx
        derv2(myl,jzl) = derv2(myl,jzl) + d1ql(3,1,2)*djmy - d1ql(3,3,2)*djmy
        derv2(mzl,jzl) = derv2(mzl,jzl) + d1ql(3,1,2)*djmz - d1ql(3,3,2)*djmz
!
        derv2(mxl,jxl) = derv2(mxl,jxl) + d1ql(1,1,3)*dkmx - d1ql(1,3,3)*dkmx
        derv2(myl,jxl) = derv2(myl,jxl) + d1ql(1,1,3)*dkmy - d1ql(1,3,3)*dkmy
        derv2(mzl,jxl) = derv2(mzl,jxl) + d1ql(1,1,3)*dkmz - d1ql(1,3,3)*dkmz
        derv2(mxl,jyl) = derv2(mxl,jyl) + d1ql(2,1,3)*dkmx - d1ql(2,3,3)*dkmx
        derv2(myl,jyl) = derv2(myl,jyl) + d1ql(2,1,3)*dkmy - d1ql(2,3,3)*dkmy
        derv2(mzl,jyl) = derv2(mzl,jyl) + d1ql(2,1,3)*dkmz - d1ql(2,3,3)*dkmz
        derv2(mxl,jzl) = derv2(mxl,jzl) + d1ql(3,1,3)*dkmx - d1ql(3,3,3)*dkmx
        derv2(myl,jzl) = derv2(myl,jzl) + d1ql(3,1,3)*dkmy - d1ql(3,3,3)*dkmy
        derv2(mzl,jzl) = derv2(mzl,jzl) + d1ql(3,1,3)*dkmz - d1ql(3,3,3)*dkmz
      endif
    endif
    if (k.ne.m.and.(lopk.or.lopm)) then
      if (lopk.and.lopm) then
        if (m.le.k) then
          kxl = kx
          kyl = ky
          kzl = kz
          mxl = mx
          myl = my
          mzl = mz
          lReverse = .false.
        else
          kxl = mx
          kyl = my
          kzl = mz
          mxl = kx
          myl = ky
          mzl = kz
          lReverse = .true.
        endif
      elseif (lopk) then
        kxl = kx
        kyl = ky
        kzl = kz
        mxl = kx
        myl = ky
        mzl = kz
        lReverse = .false.
      elseif (lopm) then
        kxl = mx
        kyl = my
        kzl = mz
        mxl = mx
        myl = my
        mzl = mz
        lReverse = .true.
      endif
!
!  d2E/(d(alpha).dq) x dq/d(beta) : k-m
!
      if (lReverse) then
        derv2(mxl,kxl) = derv2(mxl,kxl) + d1ql(1,2,1)*dimx + d1ql(1,3,1)*dimx
        derv2(myl,kxl) = derv2(myl,kxl) + d1ql(2,2,1)*dimx + d1ql(2,3,1)*dimx
        derv2(mzl,kxl) = derv2(mzl,kxl) + d1ql(3,2,1)*dimx + d1ql(3,3,1)*dimx
        derv2(mxl,kyl) = derv2(mxl,kyl) + d1ql(1,2,1)*dimy + d1ql(1,3,1)*dimy
        derv2(myl,kyl) = derv2(myl,kyl) + d1ql(2,2,1)*dimy + d1ql(2,3,1)*dimy
        derv2(mzl,kyl) = derv2(mzl,kyl) + d1ql(3,2,1)*dimy + d1ql(3,3,1)*dimy
        derv2(mxl,kzl) = derv2(mxl,kzl) + d1ql(1,2,1)*dimz + d1ql(1,3,1)*dimz
        derv2(myl,kzl) = derv2(myl,kzl) + d1ql(2,2,1)*dimz + d1ql(2,3,1)*dimz
        derv2(mzl,kzl) = derv2(mzl,kzl) + d1ql(3,2,1)*dimz + d1ql(3,3,1)*dimz
!
        derv2(mxl,kxl) = derv2(mxl,kxl) + d1ql(1,2,2)*djmx + d1ql(1,3,2)*djmx
        derv2(myl,kxl) = derv2(myl,kxl) + d1ql(2,2,2)*djmx + d1ql(2,3,2)*djmx
        derv2(mzl,kxl) = derv2(mzl,kxl) + d1ql(3,2,2)*djmx + d1ql(3,3,2)*djmx
        derv2(mxl,kyl) = derv2(mxl,kyl) + d1ql(1,2,2)*djmy + d1ql(1,3,2)*djmy
        derv2(myl,kyl) = derv2(myl,kyl) + d1ql(2,2,2)*djmy + d1ql(2,3,2)*djmy
        derv2(mzl,kyl) = derv2(mzl,kyl) + d1ql(3,2,2)*djmy + d1ql(3,3,2)*djmy
        derv2(mxl,kzl) = derv2(mxl,kzl) + d1ql(1,2,2)*djmz + d1ql(1,3,2)*djmz
        derv2(myl,kzl) = derv2(myl,kzl) + d1ql(2,2,2)*djmz + d1ql(2,3,2)*djmz
        derv2(mzl,kzl) = derv2(mzl,kzl) + d1ql(3,2,2)*djmz + d1ql(3,3,2)*djmz
!
        derv2(mxl,kxl) = derv2(mxl,kxl) + d1ql(1,2,3)*dkmx + d1ql(1,3,3)*dkmx
        derv2(myl,kxl) = derv2(myl,kxl) + d1ql(2,2,3)*dkmx + d1ql(2,3,3)*dkmx
        derv2(mzl,kxl) = derv2(mzl,kxl) + d1ql(3,2,3)*dkmx + d1ql(3,3,3)*dkmx
        derv2(mxl,kyl) = derv2(mxl,kyl) + d1ql(1,2,3)*dkmy + d1ql(1,3,3)*dkmy
        derv2(myl,kyl) = derv2(myl,kyl) + d1ql(2,2,3)*dkmy + d1ql(2,3,3)*dkmy
        derv2(mzl,kyl) = derv2(mzl,kyl) + d1ql(3,2,3)*dkmy + d1ql(3,3,3)*dkmy
        derv2(mxl,kzl) = derv2(mxl,kzl) + d1ql(1,2,3)*dkmz + d1ql(1,3,3)*dkmz
        derv2(myl,kzl) = derv2(myl,kzl) + d1ql(2,2,3)*dkmz + d1ql(2,3,3)*dkmz
        derv2(mzl,kzl) = derv2(mzl,kzl) + d1ql(3,2,3)*dkmz + d1ql(3,3,3)*dkmz
      else
        derv2(mxl,kxl) = derv2(mxl,kxl) + d1ql(1,2,1)*dimx + d1ql(1,3,1)*dimx
        derv2(myl,kxl) = derv2(myl,kxl) + d1ql(1,2,1)*dimy + d1ql(1,3,1)*dimy
        derv2(mzl,kxl) = derv2(mzl,kxl) + d1ql(1,2,1)*dimz + d1ql(1,3,1)*dimz
        derv2(mxl,kyl) = derv2(mxl,kyl) + d1ql(2,2,1)*dimx + d1ql(2,3,1)*dimx
        derv2(myl,kyl) = derv2(myl,kyl) + d1ql(2,2,1)*dimy + d1ql(2,3,1)*dimy
        derv2(mzl,kyl) = derv2(mzl,kyl) + d1ql(2,2,1)*dimz + d1ql(2,3,1)*dimz
        derv2(mxl,kzl) = derv2(mxl,kzl) + d1ql(3,2,1)*dimx + d1ql(3,3,1)*dimx
        derv2(myl,kzl) = derv2(myl,kzl) + d1ql(3,2,1)*dimy + d1ql(3,3,1)*dimy
        derv2(mzl,kzl) = derv2(mzl,kzl) + d1ql(3,2,1)*dimz + d1ql(3,3,1)*dimz
!
        derv2(mxl,kxl) = derv2(mxl,kxl) + d1ql(1,2,2)*djmx + d1ql(1,3,2)*djmx
        derv2(myl,kxl) = derv2(myl,kxl) + d1ql(1,2,2)*djmy + d1ql(1,3,2)*djmy
        derv2(mzl,kxl) = derv2(mzl,kxl) + d1ql(1,2,2)*djmz + d1ql(1,3,2)*djmz
        derv2(mxl,kyl) = derv2(mxl,kyl) + d1ql(2,2,2)*djmx + d1ql(2,3,2)*djmx
        derv2(myl,kyl) = derv2(myl,kyl) + d1ql(2,2,2)*djmy + d1ql(2,3,2)*djmy
        derv2(mzl,kyl) = derv2(mzl,kyl) + d1ql(2,2,2)*djmz + d1ql(2,3,2)*djmz
        derv2(mxl,kzl) = derv2(mxl,kzl) + d1ql(3,2,2)*djmx + d1ql(3,3,2)*djmx
        derv2(myl,kzl) = derv2(myl,kzl) + d1ql(3,2,2)*djmy + d1ql(3,3,2)*djmy
        derv2(mzl,kzl) = derv2(mzl,kzl) + d1ql(3,2,2)*djmz + d1ql(3,3,2)*djmz
!
        derv2(mxl,kxl) = derv2(mxl,kxl) + d1ql(1,2,3)*dkmx + d1ql(1,3,3)*dkmx
        derv2(myl,kxl) = derv2(myl,kxl) + d1ql(1,2,3)*dkmy + d1ql(1,3,3)*dkmy
        derv2(mzl,kxl) = derv2(mzl,kxl) + d1ql(1,2,3)*dkmz + d1ql(1,3,3)*dkmz
        derv2(mxl,kyl) = derv2(mxl,kyl) + d1ql(2,2,3)*dkmx + d1ql(2,3,3)*dkmx
        derv2(myl,kyl) = derv2(myl,kyl) + d1ql(2,2,3)*dkmy + d1ql(2,3,3)*dkmy
        derv2(mzl,kyl) = derv2(mzl,kyl) + d1ql(2,2,3)*dkmz + d1ql(2,3,3)*dkmz
        derv2(mxl,kzl) = derv2(mxl,kzl) + d1ql(3,2,3)*dkmx + d1ql(3,3,3)*dkmx
        derv2(myl,kzl) = derv2(myl,kzl) + d1ql(3,2,3)*dkmy + d1ql(3,3,3)*dkmy
        derv2(mzl,kzl) = derv2(mzl,kzl) + d1ql(3,2,3)*dkmz + d1ql(3,3,3)*dkmz
      endif
    endif
    if (lstr.and.lopm) then
!
!  Mixed strain-internal contribution
!
!  d2E/d(strain).dq x dq/d(alpha)
!
      do kl = 1,nstrains
        derv3(mx,kl) = derv3(mx,kl) + d1qsl(kl,1)*dimx + d1qsl(kl,2)*djmx + d1qsl(kl,3)*dkmx
        derv3(my,kl) = derv3(my,kl) + d1qsl(kl,1)*dimy + d1qsl(kl,2)*djmy + d1qsl(kl,3)*dkmy
        derv3(mz,kl) = derv3(mz,kl) + d1qsl(kl,1)*dimz + d1qsl(kl,2)*djmz + d1qsl(kl,3)*dkmz
      enddo
!
!  d2E/dq.dq x dq/d(alpha) x dq/d(strain)
!
      do kl = 1,nstrains
        derv3(mx,kl) = derv3(mx,kl) + d2ijs*(dqds(kl,i)*djmx + dqds(kl,j)*dimx)
        derv3(my,kl) = derv3(my,kl) + d2ijs*(dqds(kl,i)*djmy + dqds(kl,j)*dimy)
        derv3(mz,kl) = derv3(mz,kl) + d2ijs*(dqds(kl,i)*djmz + dqds(kl,j)*dimz)
        derv3(mx,kl) = derv3(mx,kl) + d2iks*(dqds(kl,i)*dkmx + dqds(kl,k)*dimx)
        derv3(my,kl) = derv3(my,kl) + d2iks*(dqds(kl,i)*dkmy + dqds(kl,k)*dimy)
        derv3(mz,kl) = derv3(mz,kl) + d2iks*(dqds(kl,i)*dkmz + dqds(kl,k)*dimz)
        derv3(mx,kl) = derv3(mx,kl) + d2jks*(dqds(kl,j)*dkmx + dqds(kl,k)*djmx)
        derv3(my,kl) = derv3(my,kl) + d2jks*(dqds(kl,j)*dkmy + dqds(kl,k)*djmy)
        derv3(mz,kl) = derv3(mz,kl) + d2jks*(dqds(kl,j)*dkmz + dqds(kl,k)*djmz)
      enddo
      if (abs(d2i2s).gt.1.0d-8) then
        do kl = 1,nstrains
          derv3(mx,kl) = derv3(mx,kl) + d2i2s*dqds(kl,i)*dimx
          derv3(my,kl) = derv3(my,kl) + d2i2s*dqds(kl,i)*dimy
          derv3(mz,kl) = derv3(mz,kl) + d2i2s*dqds(kl,i)*dimz
          derv3(ix,kl) = derv3(ix,kl) - d2i2s*dqds(kl,i)*dimx
          derv3(iy,kl) = derv3(iy,kl) - d2i2s*dqds(kl,i)*dimy
          derv3(iz,kl) = derv3(iz,kl) - d2i2s*dqds(kl,i)*dimz
        enddo
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        do kl = 1,nstrains
          derv3(mx,kl) = derv3(mx,kl) + d2j2s*dqds(kl,j)*djmx
          derv3(my,kl) = derv3(my,kl) + d2j2s*dqds(kl,j)*djmy
          derv3(mz,kl) = derv3(mz,kl) + d2j2s*dqds(kl,j)*djmz
          derv3(jx,kl) = derv3(jx,kl) - d2j2s*dqds(kl,j)*djmx
          derv3(jy,kl) = derv3(jy,kl) - d2j2s*dqds(kl,j)*djmy
          derv3(jz,kl) = derv3(jz,kl) - d2j2s*dqds(kl,j)*djmz
        enddo
      endif
      if (abs(d2k2s).gt.1.0d-8.and.i.ne.k.and.j.ne.k) then
        do kl = 1,nstrains
          derv3(mx,kl) = derv3(mx,kl) + d2k2s*dqds(kl,k)*dkmx
          derv3(my,kl) = derv3(my,kl) + d2k2s*dqds(kl,k)*dkmy
          derv3(mz,kl) = derv3(mz,kl) + d2k2s*dqds(kl,k)*dkmz
          derv3(kx,kl) = derv3(kx,kl) - d2k2s*dqds(kl,k)*dkmx
          derv3(ky,kl) = derv3(ky,kl) - d2k2s*dqds(kl,k)*dkmy
          derv3(kz,kl) = derv3(kz,kl) - d2k2s*dqds(kl,k)*dkmz
        enddo
      endif
    endif
!
!  Loop over atoms to add ij-ml correction
!
    lx = - 2
    ly = - 1
    lz =   0
    do l = 1,m-1
      lopl = (.not.lfreeze.or.lopf(l))
      indl = 3*(l-1)
      if (lopl) then
        lx = lx + 3
        ly = ly + 3
        lz = lz + 3
      endif
      if (lopm.or.lopl) then
        if (lopm.and.lopl) then
          mxl = mx
          myl = my
          mzl = mz
          lxl = lx
          lyl = ly
          lzl = lz
        elseif (lopm) then
          mxl = mx
          myl = my
          mzl = mz
          lxl = mx
          lyl = my
          lzl = mz
        elseif (lopl) then
          mxl = lx
          myl = ly
          mzl = lz
          lxl = lx
          lyl = ly
          lzl = lz
        endif
!
!  d2E/(dq.dq) x dq/d(alpha) x dq/d(beta) 
!
        dilx = dqdxyz(indl + 1,i)
        dily = dqdxyz(indl + 2,i)
        dilz = dqdxyz(indl + 3,i)
        djlx = dqdxyz(indl + 1,j)
        djly = dqdxyz(indl + 2,j)
        djlz = dqdxyz(indl + 3,j)
        dklx = dqdxyz(indl + 1,k)
        dkly = dqdxyz(indl + 2,k)
        dklz = dqdxyz(indl + 3,k)
!
        derv2(lxl,mxl) = derv2(lxl,mxl) + d2ijs*dilx*djmx + d2iks*dilx*dkmx + d2jks*djlx*dkmx
        derv2(lyl,mxl) = derv2(lyl,mxl) + d2ijs*dilx*djmy + d2iks*dilx*dkmy + d2jks*djlx*dkmy
        derv2(lzl,mxl) = derv2(lzl,mxl) + d2ijs*dilx*djmz + d2iks*dilx*dkmz + d2jks*djlx*dkmz
        derv2(lxl,myl) = derv2(lxl,myl) + d2ijs*dily*djmx + d2iks*dily*dkmx + d2jks*djly*dkmx
        derv2(lyl,myl) = derv2(lyl,myl) + d2ijs*dily*djmy + d2iks*dily*dkmy + d2jks*djly*dkmy
        derv2(lzl,myl) = derv2(lzl,myl) + d2ijs*dily*djmz + d2iks*dily*dkmz + d2jks*djly*dkmz
        derv2(lxl,mzl) = derv2(lxl,mzl) + d2ijs*dilz*djmx + d2iks*dilz*dkmx + d2jks*djlz*dkmx
        derv2(lyl,mzl) = derv2(lyl,mzl) + d2ijs*dilz*djmy + d2iks*dilz*dkmy + d2jks*djlz*dkmy
        derv2(lzl,mzl) = derv2(lzl,mzl) + d2ijs*dilz*djmz + d2iks*dilz*dkmz + d2jks*djlz*dkmz
!
        derv2(lxl,mxl) = derv2(lxl,mxl) + d2ijs*djlx*dimx + d2iks*dklx*dimx + d2jks*dklx*djmx
        derv2(lyl,mxl) = derv2(lyl,mxl) + d2ijs*djlx*dimy + d2iks*dklx*dimy + d2jks*dklx*djmy
        derv2(lzl,mxl) = derv2(lzl,mxl) + d2ijs*djlx*dimz + d2iks*dklx*dimz + d2jks*dklx*djmz
        derv2(lxl,myl) = derv2(lxl,myl) + d2ijs*djly*dimx + d2iks*dkly*dimx + d2jks*dkly*djmx
        derv2(lyl,myl) = derv2(lyl,myl) + d2ijs*djly*dimy + d2iks*dkly*dimy + d2jks*dkly*djmy
        derv2(lzl,myl) = derv2(lzl,myl) + d2ijs*djly*dimz + d2iks*dkly*dimz + d2jks*dkly*djmz
        derv2(lxl,mzl) = derv2(lxl,mzl) + d2ijs*djlz*dimx + d2iks*dklz*dimx + d2jks*dklz*djmx
        derv2(lyl,mzl) = derv2(lyl,mzl) + d2ijs*djlz*dimy + d2iks*dklz*dimy + d2jks*dklz*djmy
        derv2(lzl,mzl) = derv2(lzl,mzl) + d2ijs*djlz*dimz + d2iks*dklz*dimz + d2jks*dklz*djmz
!
!  d2Edi2/d2Edj2/d2Edk2
!
        if (abs(d2i2s).gt.1.0d-8) then
          derv2(lxl,mxl) = derv2(lxl,mxl) + d2i2s*dilx*dimx
          derv2(lyl,mxl) = derv2(lyl,mxl) + d2i2s*dily*dimx
          derv2(lzl,mxl) = derv2(lzl,mxl) + d2i2s*dilz*dimx
          derv2(lxl,myl) = derv2(lxl,myl) + d2i2s*dilx*dimy
          derv2(lyl,myl) = derv2(lyl,myl) + d2i2s*dily*dimy
          derv2(lzl,myl) = derv2(lzl,myl) + d2i2s*dilz*dimy
          derv2(lxl,mzl) = derv2(lxl,mzl) + d2i2s*dilx*dimz
          derv2(lyl,mzl) = derv2(lyl,mzl) + d2i2s*dily*dimz
          derv2(lzl,mzl) = derv2(lzl,mzl) + d2i2s*dilz*dimz
        endif
        if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
          derv2(lxl,mxl) = derv2(lxl,mxl) + d2j2s*djlx*djmx
          derv2(lyl,mxl) = derv2(lyl,mxl) + d2j2s*djly*djmx
          derv2(lzl,mxl) = derv2(lzl,mxl) + d2j2s*djlz*djmx
          derv2(lxl,myl) = derv2(lxl,myl) + d2j2s*djlx*djmy
          derv2(lyl,myl) = derv2(lyl,myl) + d2j2s*djly*djmy
          derv2(lzl,myl) = derv2(lzl,myl) + d2j2s*djlz*djmy
          derv2(lxl,mzl) = derv2(lxl,mzl) + d2j2s*djlx*djmz
          derv2(lyl,mzl) = derv2(lyl,mzl) + d2j2s*djly*djmz
          derv2(lzl,mzl) = derv2(lzl,mzl) + d2j2s*djlz*djmz
        endif
        if (abs(d2k2s).gt.1.0d-8.and.i.ne.k.and.j.ne.k) then
          derv2(lxl,mxl) = derv2(lxl,mxl) + d2k2s*dklx*dkmx
          derv2(lyl,mxl) = derv2(lyl,mxl) + d2k2s*dkly*dkmx
          derv2(lzl,mxl) = derv2(lzl,mxl) + d2k2s*dklz*dkmx
          derv2(lxl,myl) = derv2(lxl,myl) + d2k2s*dklx*dkmy
          derv2(lyl,myl) = derv2(lyl,myl) + d2k2s*dkly*dkmy
          derv2(lzl,myl) = derv2(lzl,myl) + d2k2s*dklz*dkmy
          derv2(lxl,mzl) = derv2(lxl,mzl) + d2k2s*dklx*dkmz
          derv2(lyl,mzl) = derv2(lyl,mzl) + d2k2s*dkly*dkmz
          derv2(lzl,mzl) = derv2(lzl,mzl) + d2k2s*dklz*dkmz
        endif
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
!  Strain - charge derivatives terms
!
  if (lstr) then
!
!  Mixed strain-internal term
!
!  d2E/d(alpha).dq x dq/d(strain)
!
    if (lopi) then
      do kl = 1,nstrains
        derv3(ix,kl) = derv3(ix,kl) - (d1ql(1,1,1) + d1ql(1,2,1))*dqds(kl,i)
        derv3(iy,kl) = derv3(iy,kl) - (d1ql(2,1,1) + d1ql(2,2,1))*dqds(kl,i)
        derv3(iz,kl) = derv3(iz,kl) - (d1ql(3,1,1) + d1ql(3,2,1))*dqds(kl,i)
        derv3(ix,kl) = derv3(ix,kl) - (d1ql(1,1,2) + d1ql(1,2,2))*dqds(kl,j)
        derv3(iy,kl) = derv3(iy,kl) - (d1ql(2,1,2) + d1ql(2,2,2))*dqds(kl,j)
        derv3(iz,kl) = derv3(iz,kl) - (d1ql(3,1,2) + d1ql(3,2,2))*dqds(kl,j)
        derv3(ix,kl) = derv3(ix,kl) - (d1ql(1,1,3) + d1ql(1,2,3))*dqds(kl,k)
        derv3(iy,kl) = derv3(iy,kl) - (d1ql(2,1,3) + d1ql(2,2,3))*dqds(kl,k)
        derv3(iz,kl) = derv3(iz,kl) - (d1ql(3,1,3) + d1ql(3,2,3))*dqds(kl,k)
      enddo
    endif
    if (lopj) then
      do kl = 1,nstrains
        derv3(jx,kl) = derv3(jx,kl) + (d1ql(1,1,1) - d1ql(1,3,1))*dqds(kl,i)
        derv3(jy,kl) = derv3(jy,kl) + (d1ql(2,1,1) - d1ql(2,3,1))*dqds(kl,i)
        derv3(jz,kl) = derv3(jz,kl) + (d1ql(3,1,1) - d1ql(3,3,1))*dqds(kl,i)
        derv3(jx,kl) = derv3(jx,kl) + (d1ql(1,1,2) - d1ql(1,3,2))*dqds(kl,j)
        derv3(jy,kl) = derv3(jy,kl) + (d1ql(2,1,2) - d1ql(2,3,2))*dqds(kl,j)
        derv3(jz,kl) = derv3(jz,kl) + (d1ql(3,1,2) - d1ql(3,3,2))*dqds(kl,j)
        derv3(jx,kl) = derv3(jx,kl) + (d1ql(1,1,3) - d1ql(1,3,3))*dqds(kl,k)
        derv3(jy,kl) = derv3(jy,kl) + (d1ql(2,1,3) - d1ql(2,3,3))*dqds(kl,k)
        derv3(jz,kl) = derv3(jz,kl) + (d1ql(3,1,3) - d1ql(3,3,3))*dqds(kl,k)
      enddo
    endif
    if (lopk) then
      do kl = 1,nstrains
        derv3(kx,kl) = derv3(kx,kl) + (d1ql(1,2,1) + d1ql(1,3,1))*dqds(kl,i)
        derv3(ky,kl) = derv3(ky,kl) + (d1ql(2,2,1) + d1ql(2,3,1))*dqds(kl,i)
        derv3(kz,kl) = derv3(kz,kl) + (d1ql(3,2,1) + d1ql(3,3,1))*dqds(kl,i)
        derv3(kx,kl) = derv3(kx,kl) + (d1ql(1,2,2) + d1ql(1,3,2))*dqds(kl,j)
        derv3(ky,kl) = derv3(ky,kl) + (d1ql(2,2,2) + d1ql(2,3,2))*dqds(kl,j)
        derv3(kz,kl) = derv3(kz,kl) + (d1ql(3,2,2) + d1ql(3,3,2))*dqds(kl,j)
        derv3(kx,kl) = derv3(kx,kl) + (d1ql(1,2,3) + d1ql(1,3,3))*dqds(kl,k)
        derv3(ky,kl) = derv3(ky,kl) + (d1ql(2,2,3) + d1ql(2,3,3))*dqds(kl,k)
        derv3(kz,kl) = derv3(kz,kl) + (d1ql(3,2,3) + d1ql(3,3,3))*dqds(kl,k)
      enddo
    endif
!
!  Strain-strain terms for charge derivatives
!
    if (ndim.eq.3) then
      d1si = d1qsl(1,1)
      d2si = d1qsl(2,1)
      d3si = d1qsl(3,1)
      d4si = d1qsl(4,1)
      d5si = d1qsl(5,1)
      d6si = d1qsl(6,1)
      d1sj = d1qsl(1,2)
      d2sj = d1qsl(2,2)
      d3sj = d1qsl(3,2)
      d4sj = d1qsl(4,2)
      d5sj = d1qsl(5,2)
      d6sj = d1qsl(6,2)
      d1sk = d1qsl(1,3)
      d2sk = d1qsl(2,3)
      d3sk = d1qsl(3,3)
      d4sk = d1qsl(4,3)
      d5sk = d1qsl(5,3)
      d6sk = d1qsl(6,3)
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*(d1si*dis1 + d1sj*djs1 + d1sk*dks1)
      sderv2(2,1) = sderv2(2,1) + d1si*dis2 + d1sj*djs2 + d1sk*dks2
      sderv2(2,1) = sderv2(2,1) + d2si*dis1 + d2sj*djs1 + d2sk*dks1
      sderv2(3,1) = sderv2(3,1) + d1si*dis3 + d1sj*djs3 + d1sk*dks3
      sderv2(3,1) = sderv2(3,1) + d3si*dis1 + d3sj*djs1 + d3sk*dks1
      sderv2(4,1) = sderv2(4,1) + d1si*dis4 + d1sj*djs4 + d1sk*dks4
      sderv2(4,1) = sderv2(4,1) + d4si*dis1 + d4sj*djs1 + d4sk*dks1
      sderv2(5,1) = sderv2(5,1) + d1si*dis5 + d1sj*djs5 + d1sk*dks5
      sderv2(5,1) = sderv2(5,1) + d5si*dis1 + d5sj*djs1 + d5sk*dks1
      sderv2(6,1) = sderv2(6,1) + d1si*dis6 + d1sj*djs6 + d1sk*dks6
      sderv2(6,1) = sderv2(6,1) + d6si*dis1 + d6sj*djs1 + d6sk*dks1
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*(d2si*dis2 + d2sj*djs2 + d2sk*dks2)
      sderv2(3,2) = sderv2(3,2) + d2si*dis3 + d2sj*djs3 + d2sk*dks3
      sderv2(3,2) = sderv2(3,2) + d3si*dis2 + d3sj*djs2 + d3sk*dks2
      sderv2(4,2) = sderv2(4,2) + d2si*dis4 + d2sj*djs4 + d2sk*dks4
      sderv2(4,2) = sderv2(4,2) + d4si*dis2 + d4sj*djs2 + d4sk*dks2
      sderv2(5,2) = sderv2(5,2) + d2si*dis5 + d2sj*djs5 + d2sk*dks5
      sderv2(5,2) = sderv2(5,2) + d5si*dis2 + d5sj*djs2 + d5sk*dks2
      sderv2(6,2) = sderv2(6,2) + d2si*dis6 + d2sj*djs6 + d2sk*dks6
      sderv2(6,2) = sderv2(6,2) + d6si*dis2 + d6sj*djs2 + d6sk*dks2
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*(d3si*dis3 + d3sj*djs3 + d3sk*dks3)
      sderv2(4,3) = sderv2(4,3) + d3si*dis4 + d3sj*djs4 + d3sk*dks4
      sderv2(4,3) = sderv2(4,3) + d4si*dis3 + d4sj*djs3 + d4sk*dks3
      sderv2(5,3) = sderv2(5,3) + d3si*dis5 + d3sj*djs5 + d3sk*dks5
      sderv2(5,3) = sderv2(5,3) + d5si*dis3 + d5sj*djs3 + d5sk*dks3
      sderv2(6,3) = sderv2(6,3) + d3si*dis6 + d3sj*djs6 + d3sk*dks6
      sderv2(6,3) = sderv2(6,3) + d6si*dis3 + d6sj*djs3 + d6sk*dks3
      sderv2(4,4) = sderv2(4,4) + 2.0_dp*(d4si*dis4 + d4sj*djs4 + d4sk*dks4)
      sderv2(5,4) = sderv2(5,4) + d4si*dis5 + d4sj*djs5 + d4sk*dks5
      sderv2(5,4) = sderv2(5,4) + d5si*dis4 + d5sj*djs4 + d5sk*dks4
      sderv2(6,4) = sderv2(6,4) + d4si*dis6 + d4sj*djs6 + d4sk*dks6
      sderv2(6,4) = sderv2(6,4) + d6si*dis4 + d6sj*djs4 + d6sk*dks4
      sderv2(5,5) = sderv2(5,5) + 2.0_dp*(d5si*dis5 + d5sj*djs5 + d5sk*dks5)
      sderv2(6,5) = sderv2(6,5) + d5si*dis6 + d5sj*djs6 + d5sk*dks6
      sderv2(6,5) = sderv2(6,5) + d6si*dis5 + d6sj*djs5 + d6sk*dks5
      sderv2(6,6) = sderv2(6,6) + 2.0_dp*(d6si*dis6 + d6sj*djs6 + d6sk*dks6)
!
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2ijs*dis1*djs1
      sderv2(2,1) = sderv2(2,1) + d2ijs*(dis1*djs2 + dis2*djs1)
      sderv2(3,1) = sderv2(3,1) + d2ijs*(dis1*djs3 + dis3*djs1)
      sderv2(4,1) = sderv2(4,1) + d2ijs*(dis1*djs4 + dis4*djs1)
      sderv2(5,1) = sderv2(5,1) + d2ijs*(dis1*djs5 + dis5*djs1)
      sderv2(6,1) = sderv2(6,1) + d2ijs*(dis1*djs6 + dis6*djs1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*d2ijs*dis2*djs2
      sderv2(3,2) = sderv2(3,2) + d2ijs*(dis2*djs3 + dis3*djs2)
      sderv2(4,2) = sderv2(4,2) + d2ijs*(dis2*djs4 + dis4*djs2)
      sderv2(5,2) = sderv2(5,2) + d2ijs*(dis2*djs5 + dis5*djs2)
      sderv2(6,2) = sderv2(6,2) + d2ijs*(dis2*djs6 + dis6*djs2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*d2ijs*dis3*djs3
      sderv2(4,3) = sderv2(4,3) + d2ijs*(dis3*djs4 + dis4*djs3)
      sderv2(5,3) = sderv2(5,3) + d2ijs*(dis3*djs5 + dis5*djs3)
      sderv2(6,3) = sderv2(6,3) + d2ijs*(dis3*djs6 + dis6*djs3)
      sderv2(4,4) = sderv2(4,4) + 2.0_dp*d2ijs*dis4*djs4
      sderv2(5,4) = sderv2(5,4) + d2ijs*(dis4*djs5 + dis5*djs4)
      sderv2(6,4) = sderv2(6,4) + d2ijs*(dis4*djs6 + dis6*djs4)
      sderv2(5,5) = sderv2(5,5) + 2.0_dp*d2ijs*dis5*djs5
      sderv2(6,5) = sderv2(6,5) + d2ijs*(dis5*djs6 + dis6*djs5)
      sderv2(6,6) = sderv2(6,6) + 2.0_dp*d2ijs*dis6*djs6
!
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2iks*dis1*dks1
      sderv2(2,1) = sderv2(2,1) + d2iks*(dis1*dks2 + dis2*dks1)
      sderv2(3,1) = sderv2(3,1) + d2iks*(dis1*dks3 + dis3*dks1)
      sderv2(4,1) = sderv2(4,1) + d2iks*(dis1*dks4 + dis4*dks1)
      sderv2(5,1) = sderv2(5,1) + d2iks*(dis1*dks5 + dis5*dks1)
      sderv2(6,1) = sderv2(6,1) + d2iks*(dis1*dks6 + dis6*dks1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*d2iks*dis2*dks2
      sderv2(3,2) = sderv2(3,2) + d2iks*(dis2*dks3 + dis3*dks2)
      sderv2(4,2) = sderv2(4,2) + d2iks*(dis2*dks4 + dis4*dks2)
      sderv2(5,2) = sderv2(5,2) + d2iks*(dis2*dks5 + dis5*dks2)
      sderv2(6,2) = sderv2(6,2) + d2iks*(dis2*dks6 + dis6*dks2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*d2iks*dis3*dks3
      sderv2(4,3) = sderv2(4,3) + d2iks*(dis3*dks4 + dis4*dks3)
      sderv2(5,3) = sderv2(5,3) + d2iks*(dis3*dks5 + dis5*dks3)
      sderv2(6,3) = sderv2(6,3) + d2iks*(dis3*dks6 + dis6*dks3)
      sderv2(4,4) = sderv2(4,4) + 2.0_dp*d2iks*dis4*dks4
      sderv2(5,4) = sderv2(5,4) + d2iks*(dis4*dks5 + dis5*dks4)
      sderv2(6,4) = sderv2(6,4) + d2iks*(dis4*dks6 + dis6*dks4)
      sderv2(5,5) = sderv2(5,5) + 2.0_dp*d2iks*dis5*dks5
      sderv2(6,5) = sderv2(6,5) + d2iks*(dis5*dks6 + dis6*dks5)
      sderv2(6,6) = sderv2(6,6) + 2.0_dp*d2iks*dis6*dks6
!
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2jks*djs1*dks1
      sderv2(2,1) = sderv2(2,1) + d2jks*(djs1*dks2 + djs2*dks1)
      sderv2(3,1) = sderv2(3,1) + d2jks*(djs1*dks3 + djs3*dks1)
      sderv2(4,1) = sderv2(4,1) + d2jks*(djs1*dks4 + djs4*dks1)
      sderv2(5,1) = sderv2(5,1) + d2jks*(djs1*dks5 + djs5*dks1)
      sderv2(6,1) = sderv2(6,1) + d2jks*(djs1*dks6 + djs6*dks1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*d2jks*djs2*dks2
      sderv2(3,2) = sderv2(3,2) + d2jks*(djs2*dks3 + djs3*dks2)
      sderv2(4,2) = sderv2(4,2) + d2jks*(djs2*dks4 + djs4*dks2)
      sderv2(5,2) = sderv2(5,2) + d2jks*(djs2*dks5 + djs5*dks2)
      sderv2(6,2) = sderv2(6,2) + d2jks*(djs2*dks6 + djs6*dks2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*d2jks*djs3*dks3
      sderv2(4,3) = sderv2(4,3) + d2jks*(djs3*dks4 + djs4*dks3)
      sderv2(5,3) = sderv2(5,3) + d2jks*(djs3*dks5 + djs5*dks3)
      sderv2(6,3) = sderv2(6,3) + d2jks*(djs3*dks6 + djs6*dks3)
      sderv2(4,4) = sderv2(4,4) + 2.0_dp*d2jks*djs4*dks4
      sderv2(5,4) = sderv2(5,4) + d2jks*(djs4*dks5 + djs5*dks4)
      sderv2(6,4) = sderv2(6,4) + d2jks*(djs4*dks6 + djs6*dks4)
      sderv2(5,5) = sderv2(5,5) + 2.0_dp*d2jks*djs5*dks5
      sderv2(6,5) = sderv2(6,5) + d2jks*(djs5*dks6 + djs6*dks5)
      sderv2(6,6) = sderv2(6,6) + 2.0_dp*d2jks*djs6*dks6
!
      if (abs(d2i2s).gt.1.0d-8) then
        sderv2(1,1) = sderv2(1,1) + d2i2s*dis1*dis1
        sderv2(2,1) = sderv2(2,1) + d2i2s*dis1*dis2
        sderv2(3,1) = sderv2(3,1) + d2i2s*dis1*dis3
        sderv2(4,1) = sderv2(4,1) + d2i2s*dis1*dis4
        sderv2(5,1) = sderv2(5,1) + d2i2s*dis1*dis5
        sderv2(6,1) = sderv2(6,1) + d2i2s*dis1*dis6
        sderv2(2,2) = sderv2(2,2) + d2i2s*dis2*dis2
        sderv2(3,2) = sderv2(3,2) + d2i2s*dis2*dis3
        sderv2(4,2) = sderv2(4,2) + d2i2s*dis2*dis4
        sderv2(5,2) = sderv2(5,2) + d2i2s*dis2*dis5
        sderv2(6,2) = sderv2(6,2) + d2i2s*dis2*dis6
        sderv2(3,3) = sderv2(3,3) + d2i2s*dis3*dis3
        sderv2(4,3) = sderv2(4,3) + d2i2s*dis3*dis4
        sderv2(5,3) = sderv2(5,3) + d2i2s*dis3*dis5
        sderv2(6,3) = sderv2(6,3) + d2i2s*dis3*dis6
        sderv2(4,4) = sderv2(4,4) + d2i2s*dis4*dis4
        sderv2(5,4) = sderv2(5,4) + d2i2s*dis4*dis5
        sderv2(6,4) = sderv2(6,4) + d2i2s*dis4*dis6
        sderv2(5,5) = sderv2(5,5) + d2i2s*dis5*dis5
        sderv2(6,5) = sderv2(6,5) + d2i2s*dis5*dis6
        sderv2(6,6) = sderv2(6,6) + d2i2s*dis6*dis6
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        sderv2(1,1) = sderv2(1,1) + d2j2s*djs1*djs1
        sderv2(2,1) = sderv2(2,1) + d2j2s*djs1*djs2
        sderv2(3,1) = sderv2(3,1) + d2j2s*djs1*djs3
        sderv2(4,1) = sderv2(4,1) + d2j2s*djs1*djs4
        sderv2(5,1) = sderv2(5,1) + d2j2s*djs1*djs5
        sderv2(6,1) = sderv2(6,1) + d2j2s*djs1*djs6
        sderv2(2,2) = sderv2(2,2) + d2j2s*djs2*djs2
        sderv2(3,2) = sderv2(3,2) + d2j2s*djs2*djs3
        sderv2(4,2) = sderv2(4,2) + d2j2s*djs2*djs4
        sderv2(5,2) = sderv2(5,2) + d2j2s*djs2*djs5
        sderv2(6,2) = sderv2(6,2) + d2j2s*djs2*djs6
        sderv2(3,3) = sderv2(3,3) + d2j2s*djs3*djs3
        sderv2(4,3) = sderv2(4,3) + d2j2s*djs3*djs4
        sderv2(5,3) = sderv2(5,3) + d2j2s*djs3*djs5
        sderv2(6,3) = sderv2(6,3) + d2j2s*djs3*djs6
        sderv2(4,4) = sderv2(4,4) + d2j2s*djs4*djs4
        sderv2(5,4) = sderv2(5,4) + d2j2s*djs4*djs5
        sderv2(6,4) = sderv2(6,4) + d2j2s*djs4*djs6
        sderv2(5,5) = sderv2(5,5) + d2j2s*djs5*djs5
        sderv2(6,5) = sderv2(6,5) + d2j2s*djs5*djs6
        sderv2(6,6) = sderv2(6,6) + d2j2s*djs6*djs6
      endif
      if (abs(d2k2s).gt.1.0d-8.and.i.ne.k.and.j.ne.k) then
        sderv2(1,1) = sderv2(1,1) + d2k2s*dks1*dks1
        sderv2(2,1) = sderv2(2,1) + d2k2s*dks1*dks2
        sderv2(3,1) = sderv2(3,1) + d2k2s*dks1*dks3
        sderv2(4,1) = sderv2(4,1) + d2k2s*dks1*dks4
        sderv2(5,1) = sderv2(5,1) + d2k2s*dks1*dks5
        sderv2(6,1) = sderv2(6,1) + d2k2s*dks1*dks6
        sderv2(2,2) = sderv2(2,2) + d2k2s*dks2*dks2
        sderv2(3,2) = sderv2(3,2) + d2k2s*dks2*dks3
        sderv2(4,2) = sderv2(4,2) + d2k2s*dks2*dks4
        sderv2(5,2) = sderv2(5,2) + d2k2s*dks2*dks5
        sderv2(6,2) = sderv2(6,2) + d2k2s*dks2*dks6
        sderv2(3,3) = sderv2(3,3) + d2k2s*dks3*dks3
        sderv2(4,3) = sderv2(4,3) + d2k2s*dks3*dks4
        sderv2(5,3) = sderv2(5,3) + d2k2s*dks3*dks5
        sderv2(6,3) = sderv2(6,3) + d2k2s*dks3*dks6
        sderv2(4,4) = sderv2(4,4) + d2k2s*dks4*dks4
        sderv2(5,4) = sderv2(5,4) + d2k2s*dks4*dks5
        sderv2(6,4) = sderv2(6,4) + d2k2s*dks4*dks6
        sderv2(5,5) = sderv2(5,5) + d2k2s*dks5*dks5
        sderv2(6,5) = sderv2(6,5) + d2k2s*dks5*dks6
        sderv2(6,6) = sderv2(6,6) + d2k2s*dks6*dks6
      endif
    elseif (ndim.eq.2) then
      d1si = d1qsl(1,1)
      d2si = d1qsl(2,1)
      d3si = d1qsl(3,1)
      d1sj = d1qsl(1,2)
      d2sj = d1qsl(2,2)
      d3sj = d1qsl(3,2)
      d1sk = d1qsl(1,3)
      d2sk = d1qsl(2,3)
      d3sk = d1qsl(3,3)
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*(d1si*dis1 + d1sj*djs1 + d1sk*dks1)
      sderv2(2,1) = sderv2(2,1) + d1si*dis2 + d1sj*djs2 + d1sk*dks2
      sderv2(2,1) = sderv2(2,1) + d2si*dis1 + d2sj*djs1 + d2sk*dks1
      sderv2(3,1) = sderv2(3,1) + d1si*dis3 + d1sj*djs3 + d1sk*dks3
      sderv2(3,1) = sderv2(3,1) + d3si*dis1 + d3sj*djs1 + d3sk*dks1
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*(d2si*dis2 + d2sj*djs2 + d2sk*dks2)
      sderv2(3,2) = sderv2(3,2) + d2si*dis3 + d2sj*djs3 + d2sk*dks3
      sderv2(3,2) = sderv2(3,2) + d3si*dis2 + d3sj*djs2 + d3sk*dks2
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*(d3si*dis3 + d3sj*djs3 + d3sk*dks3)
!
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2ijs*dis1*djs1
      sderv2(2,1) = sderv2(2,1) + d2ijs*(dis1*djs2 + dis2*djs1)
      sderv2(3,1) = sderv2(3,1) + d2ijs*(dis1*djs3 + dis3*djs1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*d2ijs*dis2*djs2
      sderv2(3,2) = sderv2(3,2) + d2ijs*(dis2*djs3 + dis3*djs2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*d2ijs*dis3*djs3
!
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2iks*dis1*dks1
      sderv2(2,1) = sderv2(2,1) + d2iks*(dis1*dks2 + dis2*dks1)
      sderv2(3,1) = sderv2(3,1) + d2iks*(dis1*dks3 + dis3*dks1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*d2iks*dis2*dks2
      sderv2(3,2) = sderv2(3,2) + d2iks*(dis2*dks3 + dis3*dks2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*d2iks*dis3*dks3
!
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2jks*djs1*dks1
      sderv2(2,1) = sderv2(2,1) + d2jks*(djs1*dks2 + djs2*dks1)
      sderv2(3,1) = sderv2(3,1) + d2jks*(djs1*dks3 + djs3*dks1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*d2jks*djs2*dks2
      sderv2(3,2) = sderv2(3,2) + d2jks*(djs2*dks3 + dis3*dks2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*d2jks*djs3*dks3
      if (abs(d2i2s).gt.1.0d-8) then
        sderv2(1,1) = sderv2(1,1) + d2i2s*dis1*dis1
        sderv2(2,1) = sderv2(2,1) + d2i2s*dis1*dis2
        sderv2(3,1) = sderv2(3,1) + d2i2s*dis1*dis3
        sderv2(2,2) = sderv2(2,2) + d2i2s*dis2*dis2
        sderv2(3,2) = sderv2(3,2) + d2i2s*dis2*dis3
        sderv2(3,3) = sderv2(3,3) + d2i2s*dis3*dis3
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        sderv2(1,1) = sderv2(1,1) + d2j2s*djs1*djs1
        sderv2(2,1) = sderv2(2,1) + d2j2s*djs1*djs2
        sderv2(3,1) = sderv2(3,1) + d2j2s*djs1*djs3
        sderv2(2,2) = sderv2(2,2) + d2j2s*djs2*djs2
        sderv2(3,2) = sderv2(3,2) + d2j2s*djs2*djs3
        sderv2(3,3) = sderv2(3,3) + d2j2s*djs3*djs3
      endif
      if (abs(d2k2s).gt.1.0d-8.and.i.ne.k.and.j.ne.k) then
        sderv2(1,1) = sderv2(1,1) + d2k2s*dks1*dks1
        sderv2(2,1) = sderv2(2,1) + d2k2s*dks1*dks2
        sderv2(3,1) = sderv2(3,1) + d2k2s*dks1*dks3
        sderv2(2,2) = sderv2(2,2) + d2k2s*dks2*dks2
        sderv2(3,2) = sderv2(3,2) + d2k2s*dks2*dks3
        sderv2(3,3) = sderv2(3,3) + d2k2s*dks3*dks3
      endif
    elseif (ndim.eq.1) then
      d1si = d1qsl(1,1)
      d1sj = d1qsl(1,2)
      d1sk = d1qsl(1,3)
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*(d1si*dis1 + d1sj*djs1 + d1sk*dks1)
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2ijs*dis1*djs1
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2iks*dis1*dks1
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2jks*djs1*dks1
      if (abs(d2i2s).gt.1.0d-8) then
        sderv2(1,1) = sderv2(1,1) + d2i2s*dis1*dis1
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        sderv2(1,1) = sderv2(1,1) + d2j2s*djs1*djs1
      endif
      if (abs(d2k2s).gt.1.0d-8.and.i.ne.k.and.j.ne.k) then
        sderv2(1,1) = sderv2(1,1) + d2k2s*dks1*dks1
      endif
    endif
  endif
!
!  Remove terms from derv3 that get added in in strfin but shouldn't be 
!  Only applies to non-Hellmann-Feynman methods, such as bond order charges
!
  if (lstr.and..not.leem) then
    if (ndim.eq.3) then
      mx = -2
      my = -1
      mz =  0
      do m = 1,numat
        lopm = (.not.lfreeze.or.lopf(nrelat(m)))
        if (lopm) then
          mx = mx + 3
          my = my + 3
          mz = mz + 3
          xdrvm = deisum*dqdxyz(mx,i) + dejsum*dqdxyz(mx,j) + deksum*dqdxyz(mx,k)
          ydrvm = deisum*dqdxyz(my,i) + dejsum*dqdxyz(my,j) + deksum*dqdxyz(my,k)
          zdrvm = deisum*dqdxyz(mz,i) + dejsum*dqdxyz(mz,j) + deksum*dqdxyz(mz,k)
          derv3(mx,5) = derv3(mx,5) - zdrvm
          derv3(mx,6) = derv3(mx,6) - ydrvm
          derv3(my,4) = derv3(my,4) - zdrvm
          derv3(my,6) = derv3(my,6) - xdrvm
          derv3(mz,4) = derv3(mz,4) - ydrvm
          derv3(mz,5) = derv3(mz,5) - xdrvm
          derv3(mx,1) = derv3(mx,1) - 2.0_dp*xdrvm
          derv3(my,2) = derv3(my,2) - 2.0_dp*ydrvm
          derv3(mz,3) = derv3(mz,3) - 2.0_dp*zdrvm
        endif
      enddo
    elseif (ndim.eq.2) then
      mx = -2
      my = -1
      do m = 1,numat
        lopm = (.not.lfreeze.or.lopf(m))  
        if (lopm) then
          mx = mx + 3
          my = my + 3
          xdrvm = deisum*dqdxyz(mx,i) + dejsum*dqdxyz(mx,j) + deksum*dqdxyz(mx,k)
          ydrvm = deisum*dqdxyz(my,i) + dejsum*dqdxyz(my,j) + deksum*dqdxyz(my,k)
          derv3(mx,1) = derv3(mx,1) - 2.0_dp*xdrvm
          derv3(my,2) = derv3(my,2) - 2.0_dp*ydrvm
          derv3(mx,3) = derv3(mx,3) - ydrvm
          derv3(my,3) = derv3(my,3) - xdrvm
        endif
      enddo
    elseif (ndim.eq.1) then
      mx = -2
      do m = 1,numat
        lopm = (.not.lfreeze.or.lopf(m))  
        if (lopm) then
          mx = mx + 3
          xdrvm = deisum*dqdxyz(mx,i) + dejsum*dqdxyz(mx,j) + deksum*dqdxyz(mx,k)
          derv3(mx,1) = derv3(mx,1) - 2.0_dp*xdrvm
        endif
      enddo
    endif
  endif
!
  return
  end
