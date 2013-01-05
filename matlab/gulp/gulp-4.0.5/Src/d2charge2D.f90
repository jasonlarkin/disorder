  subroutine d2charge2D(i,j,nor,xtmp,ytmp,ix,iy,iz,jx,jy,jz,lopi,lopj,dei,dej, &
                        d1i,d1zi,d1j,d1zj,ds1i,ds1j,d2i2,d2ij,d2j2,d2self,rpd, &
                        mdis,d2trm1dij,d2trm1diz,d2trm1djz,dtrm1di,dtrm1dj)
!
!  Calculates the contribution to the second derivative matrices
!  due to charge derivatives from EEM/QEq for the 2-D case. Only
!  needed for reciprocal space part.
!
!   3/01 Created from d2charge
!  11/02 Second strain derivatives corrected
!  11/02 Error in derv3 calculation for QEq(H) case corrected
!   9/04 Order of terms in dqds switched
!   9/04 Modified to generalise to charge derivatives other than from EEM
!   9/04 Contribution from d2q/dalpha.dbeta added
!   7/05 lReverse set to true for k.le.(i.or.j)
!  11/07 Unused variables removed
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
  integer(i4), intent(in)  :: i
  integer(i4), intent(in)  :: ix
  integer(i4), intent(in)  :: iy
  integer(i4), intent(in)  :: iz
  integer(i4), intent(in)  :: j
  integer(i4), intent(in)  :: jx
  integer(i4), intent(in)  :: jy
  integer(i4), intent(in)  :: jz
  integer(i4), intent(in)  :: mdis
  integer(i4), intent(in)  :: nor
  logical,     intent(in)  :: lopi
  logical,     intent(in)  :: lopj
  real(dp),    intent(in)  :: d1i(*)
  real(dp),    intent(in)  :: d1j(*)
  real(dp),    intent(in)  :: d1zi(*)
  real(dp),    intent(in)  :: d1zj(*)
  real(dp),    intent(in)  :: d2i2(*)
  real(dp),    intent(in)  :: d2ij(*)
  real(dp),    intent(in)  :: d2j2(*)
  real(dp),    intent(in)  :: d2self
  real(dp),    intent(in)  :: d2trm1dij
  real(dp),    intent(in)  :: d2trm1diz
  real(dp),    intent(in)  :: d2trm1djz
  real(dp),    intent(in)  :: dei(*)
  real(dp),    intent(in)  :: dej(*)
  real(dp),    intent(in)  :: ds1i(*)
  real(dp),    intent(in)  :: ds1j(*)
  real(dp),    intent(in)  :: dtrm1di
  real(dp),    intent(in)  :: dtrm1dj
  real(dp),    intent(in)  :: rpd(mdis,*)
  real(dp),    intent(in)  :: xtmp(*)
  real(dp),    intent(in)  :: ytmp(*)
!
!  Local variables
!
  integer(i4)              :: ind
  integer(i4)              :: indk
  integer(i4)              :: indl
  integer(i4)              :: iv
  integer(i4)              :: ixl
  integer(i4)              :: iyl
  integer(i4)              :: izl
  integer(i4)              :: jxl
  integer(i4)              :: jyl
  integer(i4)              :: jzl
  integer(i4)              :: k
  integer(i4)              :: kk
  integer(i4)              :: kl
  integer(i4)              :: kx
  integer(i4)              :: ky
  integer(i4)              :: kz
  integer(i4)              :: kxl
  integer(i4)              :: kyl
  integer(i4)              :: kzl
  integer(i4)              :: l
  integer(i4)              :: lx
  integer(i4)              :: ly
  integer(i4)              :: lz
  integer(i4)              :: lxl
  integer(i4)              :: lyl
  integer(i4)              :: lzl
  integer(i4)              :: m
  integer(i4)              :: mnxx
  integer(i4)              :: mnxy
  integer(i4)              :: mnxz
  integer(i4)              :: mnyx
  integer(i4)              :: mnyy
  integer(i4)              :: mnyz
  integer(i4)              :: mnzx
  integer(i4)              :: mnzy
  integer(i4)              :: mnzz
  integer(i4)              :: mx
  integer(i4)              :: my
  integer(i4)              :: mz
  integer(i4)              :: n
  integer(i4)              :: nx
  logical                  :: lNonIJQDeriv
  logical                  :: lopk
  logical                  :: lopl
  logical                  :: lReverse
  real(dp)                 :: d1ix
  real(dp)                 :: d1iy
  real(dp)                 :: d1iz
  real(dp)                 :: d1jx
  real(dp)                 :: d1jy
  real(dp)                 :: d1jz
  real(dp)                 :: d2i2s
  real(dp)                 :: d2ijs
  real(dp)                 :: d2j2s
  real(dp)                 :: d1si   
  real(dp)                 :: d2si   
  real(dp)                 :: d3si
  real(dp)                 :: d1sj   
  real(dp)                 :: d2sj   
  real(dp)                 :: d3sj
  real(dp)                 :: deisum
  real(dp)                 :: dejsum
  real(dp)                 :: dikx   
  real(dp)                 :: diky
  real(dp)                 :: dikz
  real(dp)                 :: dilx
  real(dp)                 :: dily
  real(dp)                 :: dilz 
  real(dp)                 :: dis1 
  real(dp)                 :: dis2 
  real(dp)                 :: dis3
  real(dp)                 :: djkx
  real(dp)                 :: djky
  real(dp)                 :: djkz
  real(dp)                 :: djlx   
  real(dp)                 :: djly   
  real(dp)                 :: djlz  
  real(dp)                 :: djs1  
  real(dp)                 :: djs2  
  real(dp)                 :: djs3
  real(dp)                 :: dsi(3)
  real(dp)                 :: dsi2(3)
  real(dp)                 :: dsj(3)
  real(dp)                 :: dsj2(3)
!
!  Check that charge derivatives are in use
!
  if (.not.lDoQDeriv2) return
!
!  Set pointers to i-j second derivative block for eself derivatives
!
  d1ix = 0.0_dp
  d1iy = 0.0_dp
  d1iz = 0.0_dp
  d1jx = 0.0_dp
  d1jy = 0.0_dp
  d1jz = 0.0_dp
  d2ijs = d2self
  d2i2s = 0.0_dp
  d2j2s = 0.0_dp
  do iv = 1,nor
    d1ix = d1ix + d1i(iv)*xtmp(iv)
    d1iy = d1iy + d1i(iv)*ytmp(iv)
    d1iz = d1iz + d1zi(iv)
    d1jx = d1jx + d1j(iv)*xtmp(iv)
    d1jy = d1jy + d1j(iv)*ytmp(iv)
    d1jz = d1jz + d1zj(iv)
    d2ijs = d2ijs + d2ij(iv)
    d2i2s = d2i2s + d2i2(iv)
    d2j2s = d2j2s + d2j2(iv)
  enddo
  d1iz = d1iz + d2trm1diz
  d1jz = d1jz + d2trm1djz
  d2ijs = d2ijs + d2trm1dij
  if ((leem.and..not.lelementOK(nat(i))).or.nregionno(nsft+nrelat(i)).ne.1) then
    d2ijs = 0.0_dp  
    d2i2s = 0.0_dp  
    d1ix = 0.0_dp
    d1iy = 0.0_dp
    d1iz = 0.0_dp
  endif
  if ((leem.and..not.lelementOK(nat(j))).or.nregionno(nsft+nrelat(j)).ne.1) then
    d2ijs = 0.0_dp  
    d2j2s = 0.0_dp  
    d1jx = 0.0_dp
    d1jy = 0.0_dp
    d1jz = 0.0_dp
  endif
  if (lstr) then
    dis1 = dqds(1,i)
    dis2 = dqds(2,i)
    dis3 = dqds(3,i)
    djs1 = dqds(1,j)
    djs2 = dqds(2,j)
    djs3 = dqds(3,j)
    do kk = 1,nstrains
      dsi(kk) = 0.0_dp
      dsj(kk) = 0.0_dp
    enddo
    do iv = 1,nor
      dsi(1) = dsi(1) + ds1i(iv)*rpd(iv,1)
      dsj(1) = dsj(1) + ds1j(iv)*rpd(iv,1)
      dsi(2) = dsi(2) + ds1i(iv)*rpd(iv,2)
      dsj(2) = dsj(2) + ds1j(iv)*rpd(iv,2)
      dsi(3) = dsi(3) + ds1i(iv)*rpd(iv,3)
      dsj(3) = dsj(3) + ds1j(iv)*rpd(iv,3) 
    enddo
    do kk = 1,nstrains
      dsi2(kk) = dsi(kk)
      dsj2(kk) = dsj(kk)
    enddo
    do iv = 1,nor
      dsi(1) = dsi(1) - dei(iv)
      dsj(1) = dsj(1) - dej(iv)
      dsi(2) = dsi(2) - dei(iv)
      dsj(2) = dsj(2) - dej(iv)
    enddo
    dsi(1) = dsi(1) - dtrm1di
    dsj(1) = dsj(1) - dtrm1dj
    dsi(2) = dsi(2) - dtrm1di
    dsj(2) = dsj(2) - dtrm1dj
    if (leem) then
      do kk = 1,nstrains
        dsi2(kk) = dsi(kk)
        dsj2(kk) = dsj(kk)
      enddo
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
    deisum = dtrm1di
    dejsum = dtrm1dj
    do iv = 1,nor
      deisum = deisum + dei(iv)
      dejsum = dejsum + dej(iv)
    enddo
!
!  Terms involving Q derivatives for i/j and one other atom
!
    do m = 1,nqatoms(i)
      k = nqatomptr(m,i)
      kx = 3*(k - 1) + 1
      ky = kx + 1
      kz = ky + 1
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
      if (i.gt.k) then
        derv2(kx,ix) = derv2(kx,ix) + deisum*d2qdxyz2(mnxx,i)
        derv2(ky,ix) = derv2(ky,ix) + deisum*d2qdxyz2(mnxy,i)
        derv2(kz,ix) = derv2(kz,ix) + deisum*d2qdxyz2(mnxz,i)
        derv2(kx,iy) = derv2(kx,iy) + deisum*d2qdxyz2(mnxy,i)
        derv2(ky,iy) = derv2(ky,iy) + deisum*d2qdxyz2(mnyy,i)
        derv2(kz,iy) = derv2(kz,iy) + deisum*d2qdxyz2(mnyz,i)
        derv2(kx,iz) = derv2(kx,iz) + deisum*d2qdxyz2(mnxz,i)
        derv2(ky,iz) = derv2(ky,iz) + deisum*d2qdxyz2(mnyz,i)
        derv2(kz,iz) = derv2(kz,iz) + deisum*d2qdxyz2(mnzz,i)
      elseif (i.lt.k) then
        derv2(ix,kx) = derv2(ix,kx) + deisum*d2qdxyz2(mnxx,i)
        derv2(iy,kx) = derv2(iy,kx) + deisum*d2qdxyz2(mnxy,i)
        derv2(iz,kx) = derv2(iz,kx) + deisum*d2qdxyz2(mnxz,i)
        derv2(ix,ky) = derv2(ix,ky) + deisum*d2qdxyz2(mnxy,i)
        derv2(iy,ky) = derv2(iy,ky) + deisum*d2qdxyz2(mnyy,i)
        derv2(iz,ky) = derv2(iz,ky) + deisum*d2qdxyz2(mnyz,i)
        derv2(ix,kz) = derv2(ix,kz) + deisum*d2qdxyz2(mnxz,i)
        derv2(iy,kz) = derv2(iy,kz) + deisum*d2qdxyz2(mnyz,i)
        derv2(iz,kz) = derv2(iz,kz) + deisum*d2qdxyz2(mnzz,i)
      endif
!
!  Strain - strain contribution
!
      if (lstr.and.ndim.gt.0) then
        ind = 0
        do kk = 1,nstrains
          do kl = 1,kk-1
            ind = ind + 1
            sderv2(kk,kl) = sderv2(kk,kl) + deisum*d2qds2(ind,i)
          enddo
          ind = ind + 1
          sderv2(kk,kk) = sderv2(kk,kk) + deisum*d2qds2(ind,i)
!
          derv3(kx,kk) = derv3(kx,kk) + deisum*d2qdxyzs(kk,mx,i)
          derv3(ky,kk) = derv3(ky,kk) + deisum*d2qdxyzs(kk,my,i)
          derv3(kz,kk) = derv3(kz,kk) + deisum*d2qdxyzs(kk,mz,i)
          derv3(ix,kk) = derv3(ix,kk) - deisum*d2qdxyzs(kk,mx,i)
          derv3(iy,kk) = derv3(iy,kk) - deisum*d2qdxyzs(kk,my,i)
          derv3(iz,kk) = derv3(iz,kk) - deisum*d2qdxyzs(kk,mz,i)
        enddo
      endif
    enddo
!
    do m = 1,nqatoms(j)
      k = nqatomptr(m,j)
      kx = 3*(k - 1) + 1
      ky = kx + 1
      kz = ky + 1
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
      if (j.gt.k) then
        derv2(kx,jx) = derv2(kx,jx) + dejsum*d2qdxyz2(mnxx,j)
        derv2(ky,jx) = derv2(ky,jx) + dejsum*d2qdxyz2(mnxy,j)
        derv2(kz,jx) = derv2(kz,jx) + dejsum*d2qdxyz2(mnxz,j)
        derv2(kx,jy) = derv2(kx,jy) + dejsum*d2qdxyz2(mnxy,j)
        derv2(ky,jy) = derv2(ky,jy) + dejsum*d2qdxyz2(mnyy,j)
        derv2(kz,jy) = derv2(kz,jy) + dejsum*d2qdxyz2(mnyz,j)
        derv2(kx,jz) = derv2(kx,jz) + dejsum*d2qdxyz2(mnxz,j)
        derv2(ky,jz) = derv2(ky,jz) + dejsum*d2qdxyz2(mnyz,j)
        derv2(kz,jz) = derv2(kz,jz) + dejsum*d2qdxyz2(mnzz,j)
      elseif (j.lt.k) then
        derv2(jx,kx) = derv2(jx,kx) + dejsum*d2qdxyz2(mnxx,j)
        derv2(jy,kx) = derv2(jy,kx) + dejsum*d2qdxyz2(mnxy,j)
        derv2(jz,kx) = derv2(jz,kx) + dejsum*d2qdxyz2(mnxz,j)
        derv2(jx,ky) = derv2(jx,ky) + dejsum*d2qdxyz2(mnxy,j)
        derv2(jy,ky) = derv2(jy,ky) + dejsum*d2qdxyz2(mnyy,j)
        derv2(jz,ky) = derv2(jz,ky) + dejsum*d2qdxyz2(mnyz,j)
        derv2(jx,kz) = derv2(jx,kz) + dejsum*d2qdxyz2(mnxz,j)
        derv2(jy,kz) = derv2(jy,kz) + dejsum*d2qdxyz2(mnyz,j)
        derv2(jz,kz) = derv2(jz,kz) + dejsum*d2qdxyz2(mnzz,j)
      endif
!
!  Strain - strain contribution
!
      if (lstr.and.ndim.gt.0) then
        ind = 0
        do kk = 1,nstrains
          do kl = 1,kk-1
            ind = ind + 1
            sderv2(kk,kl) = sderv2(kk,kl) + dejsum*d2qds2(ind,j)
          enddo
          ind = ind + 1 
          sderv2(kk,kk) = sderv2(kk,kk) + dejsum*d2qds2(ind,j)
!
          derv3(kx,kk) = derv3(kx,kk) + dejsum*d2qdxyzs(kk,mx,j)
          derv3(ky,kk) = derv3(ky,kk) + dejsum*d2qdxyzs(kk,my,j)
          derv3(kz,kk) = derv3(kz,kk) + dejsum*d2qdxyzs(kk,mz,j)
          derv3(jx,kk) = derv3(jx,kk) - dejsum*d2qdxyzs(kk,mx,j)
          derv3(jy,kk) = derv3(jy,kk) - dejsum*d2qdxyzs(kk,my,j)
          derv3(jz,kk) = derv3(jz,kk) - dejsum*d2qdxyzs(kk,mz,j)
        enddo
      endif
    enddo
!
!  Terms involving Q derivatives for non-i/j case
!
    if (lNonIJQDeriv) then
      do m = 2,nqatoms(i)
        k = nqatomptr(m,i)
        kx = 3*(k - 1) + 1
        ky = kx + 1
        kz = ky + 1
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
          derv2(lx,kx) = derv2(lx,kx) + deisum*d2qdxyz2(mnxx,i)
          derv2(ly,kx) = derv2(ly,kx) + deisum*d2qdxyz2(mnxy,i)
          derv2(lz,kx) = derv2(lz,kx) + deisum*d2qdxyz2(mnxz,i)
          derv2(lx,ky) = derv2(lx,ky) + deisum*d2qdxyz2(mnyx,i)
          derv2(ly,ky) = derv2(ly,ky) + deisum*d2qdxyz2(mnyy,i)
          derv2(lz,ky) = derv2(lz,ky) + deisum*d2qdxyz2(mnyz,i)
          derv2(lx,kz) = derv2(lx,kz) + deisum*d2qdxyz2(mnzx,i)
          derv2(ly,kz) = derv2(ly,kz) + deisum*d2qdxyz2(mnzy,i)
          derv2(lz,kz) = derv2(lz,kz) + deisum*d2qdxyz2(mnzz,i)
        enddo
      enddo
!
      do m = 2,nqatoms(j)
        k = nqatomptr(m,j)
        kx = 3*(k - 1) + 1
        ky = kx + 1
        kz = ky + 1
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
          derv2(lx,kx) = derv2(lx,kx) + deisum*d2qdxyz2(mnxx,j)
          derv2(ly,kx) = derv2(ly,kx) + deisum*d2qdxyz2(mnxy,j)
          derv2(lz,kx) = derv2(lz,kx) + deisum*d2qdxyz2(mnxz,j)
          derv2(lx,ky) = derv2(lx,ky) + deisum*d2qdxyz2(mnyx,j)
          derv2(ly,ky) = derv2(ly,ky) + deisum*d2qdxyz2(mnyy,j)
          derv2(lz,ky) = derv2(lz,ky) + deisum*d2qdxyz2(mnyz,j)
          derv2(lx,kz) = derv2(lx,kz) + deisum*d2qdxyz2(mnzx,j)
          derv2(ly,kz) = derv2(ly,kz) + deisum*d2qdxyz2(mnzy,j)
          derv2(lz,kz) = derv2(lz,kz) + deisum*d2qdxyz2(mnzz,j)
        enddo
      enddo
    endif
  endif
!
!  Loop over atoms to add ij-k contributions
!
!  Note indices must be set to cope with the fact that only
!  half of derv2 is constructed in upper triangle and the
!  effects of lopi/lopj/lopk if freezing is being used.
!
  kx = - 2
  ky = - 1
  kz =   0
  do k = 1,numat
    lopk = (.not.lfreeze.or.lopf(k))
    indk = 3*(k - 1)
    if (lopk) then
      kx = kx + 3
      ky = ky + 3
      kz = kz + 3
    endif
    dikx = dqdxyz(indk+1,i)
    diky = dqdxyz(indk+2,i)
    dikz = dqdxyz(indk+3,i)
    djkx = dqdxyz(indk+1,j)
    djky = dqdxyz(indk+2,j)
    djkz = dqdxyz(indk+3,j)
    if (i.ne.k.and.(lopi.or.lopk)) then
      if (lopi.and.lopk) then
        if (k.le.i) then
          ixl = ix
          iyl = iy
          izl = iz
          kxl = kx
          kyl = ky
          kzl = kz
          lReverse = .true.
        else
          ixl = kx
          iyl = ky
          izl = kz
          kxl = ix
          kyl = iy
          kzl = iz
          lReverse = .true.
        endif
      elseif (lopi) then
        ixl = ix
        iyl = iy
        izl = iz
        kxl = ix
        kyl = iy
        kzl = iz
        lReverse = .false.
      elseif (lopk) then
        ixl = kx
        iyl = ky
        izl = kz
        kxl = kx
        kyl = ky
        kzl = kz
        lReverse = .true.
      endif
!
!  d2E/(d(alpha).dq) x dq/d(beta) : i-k
!
      if (lReverse) then
        derv2(kxl,ixl) = derv2(kxl,ixl) - d1ix*dikx
        derv2(kyl,ixl) = derv2(kyl,ixl) - d1iy*dikx
        derv2(kzl,ixl) = derv2(kzl,ixl) - d1iz*dikx
        derv2(kxl,iyl) = derv2(kxl,iyl) - d1ix*diky
        derv2(kyl,iyl) = derv2(kyl,iyl) - d1iy*diky
        derv2(kzl,iyl) = derv2(kzl,iyl) - d1iz*diky
        derv2(kxl,izl) = derv2(kxl,izl) - d1ix*dikz
        derv2(kyl,izl) = derv2(kyl,izl) - d1iy*dikz
        derv2(kzl,izl) = derv2(kzl,izl) - d1iz*dikz
!
        derv2(kxl,ixl) = derv2(kxl,ixl) - d1jx*djkx
        derv2(kyl,ixl) = derv2(kyl,ixl) - d1jy*djkx
        derv2(kzl,ixl) = derv2(kzl,ixl) - d1jz*djkx
        derv2(kxl,iyl) = derv2(kxl,iyl) - d1jx*djky
        derv2(kyl,iyl) = derv2(kyl,iyl) - d1jy*djky
        derv2(kzl,iyl) = derv2(kzl,iyl) - d1jz*djky
        derv2(kxl,izl) = derv2(kxl,izl) - d1jx*djkz
        derv2(kyl,izl) = derv2(kyl,izl) - d1jy*djkz
        derv2(kzl,izl) = derv2(kzl,izl) - d1jz*djkz
      else
        derv2(kxl,ixl) = derv2(kxl,ixl) - d1ix*dikx
        derv2(kyl,ixl) = derv2(kyl,ixl) - d1ix*diky
        derv2(kzl,ixl) = derv2(kzl,ixl) - d1ix*dikz
        derv2(kxl,iyl) = derv2(kxl,iyl) - d1iy*dikx
        derv2(kyl,iyl) = derv2(kyl,iyl) - d1iy*diky
        derv2(kzl,iyl) = derv2(kzl,iyl) - d1iy*dikz
        derv2(kxl,izl) = derv2(kxl,izl) - d1iz*dikx
        derv2(kyl,izl) = derv2(kyl,izl) - d1iz*diky
        derv2(kzl,izl) = derv2(kzl,izl) - d1iz*dikz
!
        derv2(kxl,ixl) = derv2(kxl,ixl) - d1jx*djkx
        derv2(kyl,ixl) = derv2(kyl,ixl) - d1jx*djky
        derv2(kzl,ixl) = derv2(kzl,ixl) - d1jx*djkz
        derv2(kxl,iyl) = derv2(kxl,iyl) - d1jy*djkx
        derv2(kyl,iyl) = derv2(kyl,iyl) - d1jy*djky
        derv2(kzl,iyl) = derv2(kzl,iyl) - d1jy*djkz
        derv2(kxl,izl) = derv2(kxl,izl) - d1jz*djkx
        derv2(kyl,izl) = derv2(kyl,izl) - d1jz*djky
        derv2(kzl,izl) = derv2(kzl,izl) - d1jz*djkz
      endif
    endif
    if (j.ne.k.and.(lopj.or.lopk)) then
      if (lopj.and.lopk) then
        if (k.le.j) then
          jxl = jx
          jyl = jy
          jzl = jz
          kxl = kx
          kyl = ky
          kzl = kz
          lReverse = .true.
        else
          jxl = kx
          jyl = ky
          jzl = kz
          kxl = jx
          kyl = jy
          kzl = jz
          lReverse = .true.
        endif
      elseif (lopj) then
        jxl = jx
        jyl = jy
        jzl = jz
        kxl = jx
        kyl = jy
        kzl = jz
        lReverse = .false.
      elseif (lopk) then
        jxl = kx
        jyl = ky
        jzl = kz
        kxl = kx
        kyl = ky
        kzl = kz
        lReverse = .true.
      endif
!
!  d2E/(d(alpha).dq) x dq/d(beta) : j-k
!
      if (lReverse) then
        derv2(kxl,jxl) = derv2(kxl,jxl) + d1jx*djkx
        derv2(kyl,jxl) = derv2(kyl,jxl) + d1jy*djkx
        derv2(kzl,jxl) = derv2(kzl,jxl) + d1jz*djkx
        derv2(kxl,jyl) = derv2(kxl,jyl) + d1jx*djky
        derv2(kyl,jyl) = derv2(kyl,jyl) + d1jy*djky
        derv2(kzl,jyl) = derv2(kzl,jyl) + d1jz*djky
        derv2(kxl,jzl) = derv2(kxl,jzl) + d1jx*djkz
        derv2(kyl,jzl) = derv2(kyl,jzl) + d1jy*djkz
        derv2(kzl,jzl) = derv2(kzl,jzl) + d1jz*djkz
!
        derv2(kxl,jxl) = derv2(kxl,jxl) + d1ix*dikx
        derv2(kyl,jxl) = derv2(kyl,jxl) + d1iy*dikx
        derv2(kzl,jxl) = derv2(kzl,jxl) + d1iz*dikx
        derv2(kxl,jyl) = derv2(kxl,jyl) + d1ix*diky
        derv2(kyl,jyl) = derv2(kyl,jyl) + d1iy*diky
        derv2(kzl,jyl) = derv2(kzl,jyl) + d1iz*diky
        derv2(kxl,jzl) = derv2(kxl,jzl) + d1ix*dikz
        derv2(kyl,jzl) = derv2(kyl,jzl) + d1iy*dikz
        derv2(kzl,jzl) = derv2(kzl,jzl) + d1iz*dikz
      else
        derv2(kxl,jxl) = derv2(kxl,jxl) + d1jx*djkx
        derv2(kyl,jxl) = derv2(kyl,jxl) + d1jx*djky
        derv2(kzl,jxl) = derv2(kzl,jxl) + d1jx*djkz
        derv2(kxl,jyl) = derv2(kxl,jyl) + d1jy*djkx
        derv2(kyl,jyl) = derv2(kyl,jyl) + d1jy*djky
        derv2(kzl,jyl) = derv2(kzl,jyl) + d1jy*djkz
        derv2(kxl,jzl) = derv2(kxl,jzl) + d1jz*djkx
        derv2(kyl,jzl) = derv2(kyl,jzl) + d1jz*djky
        derv2(kzl,jzl) = derv2(kzl,jzl) + d1jz*djkz
!
        derv2(kxl,jxl) = derv2(kxl,jxl) + d1ix*dikx
        derv2(kyl,jxl) = derv2(kyl,jxl) + d1ix*diky
        derv2(kzl,jxl) = derv2(kzl,jxl) + d1ix*dikz
        derv2(kxl,jyl) = derv2(kxl,jyl) + d1iy*dikx
        derv2(kyl,jyl) = derv2(kyl,jyl) + d1iy*diky
        derv2(kzl,jyl) = derv2(kzl,jyl) + d1iy*dikz
        derv2(kxl,jzl) = derv2(kxl,jzl) + d1iz*dikx
        derv2(kyl,jzl) = derv2(kyl,jzl) + d1iz*diky
        derv2(kzl,jzl) = derv2(kzl,jzl) + d1iz*dikz
      endif
    endif
    if (lstr.and.lopk) then
!
!  Mix strain-internal contribution
!
!  d2E/d(strain).dq x dq/d(alpha)
!
      do kl  =  1,nstrains
        derv3(kx,kl) = derv3(kx,kl) + dsi2(kl)*dikx + dsj2(kl)*djkx
        derv3(ky,kl) = derv3(ky,kl) + dsi2(kl)*diky + dsj2(kl)*djky
        derv3(kz,kl) = derv3(kz,kl) + dsi2(kl)*dikz + dsj2(kl)*djkz
     enddo
!
!  d2E/dq.dq x dq/d(alpha) x dq/d(strain)
!
      do kl  =  1,nstrains
        derv3(kx,kl) = derv3(kx,kl) + d2ijs*(dqds(kl,i)*djkx + dqds(kl,j)*dikx)
        derv3(ky,kl) = derv3(ky,kl) + d2ijs*(dqds(kl,i)*djky + dqds(kl,j)*diky)
        derv3(kz,kl) = derv3(kz,kl) + d2ijs*(dqds(kl,i)*djkz + dqds(kl,j)*dikz)
      enddo
      if (abs(d2i2s).gt.1.0d-8) then
        do kl  =  1,nstrains
          derv3(kx,kl) = derv3(kx,kl) + d2i2s*dqds(kl,i)*dikx
          derv3(ky,kl) = derv3(ky,kl) + d2i2s*dqds(kl,i)*diky
          derv3(kz,kl) = derv3(kz,kl) + d2i2s*dqds(kl,i)*dikz
        enddo
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        do kl  =  1,nstrains
          derv3(kx,kl) = derv3(kx,kl) + d2j2s*dqds(kl,j)*djkx
          derv3(ky,kl) = derv3(ky,kl) + d2j2s*dqds(kl,j)*djky
          derv3(kz,kl) = derv3(kz,kl) + d2j2s*dqds(kl,j)*djkz
        enddo
      endif
    endif
!
!  Loop over atoms to add ij-kl correction
!
    lx = - 2
    ly = - 1
    lz =   0
    do l = 1,k-1
      lopl = (.not.lfreeze.or.lopf(l))
      indl = 3*(l-1)
      if (lopl) then
        lx = lx + 3
        ly = ly + 3
        lz = lz + 3
      endif
      if (lopk.or.lopl) then
        if (lopk.and.lopl) then
          kxl = kx
          kyl = ky
          kzl = kz
          lxl = lx
          lyl = ly
          lzl = lz
        elseif (lopk) then
          kxl = kx
          kyl = ky
          kzl = kz
          lxl = kx
          lyl = ky
          lzl = kz
        elseif (lopl) then
          kxl = lx
          kyl = ly
          kzl = lz
          lxl = lx
          lyl = ly
          lzl = lz
        endif
!
!  d2E/(dq.dq) x dq/d(alpha) x dq/d(beta) 
!
        dilx = dqdxyz(indl+1,i)
        dily = dqdxyz(indl+2,i)
        dilz = dqdxyz(indl+3,i)
        djlx = dqdxyz(indl+1,j)
        djly = dqdxyz(indl+2,j)
        djlz = dqdxyz(indl+3,j)
!
        derv2(lxl,kxl) = derv2(lxl,kxl) + d2ijs*dilx*djkx
        derv2(lyl,kxl) = derv2(lyl,kxl) + d2ijs*dilx*djky
        derv2(lzl,kxl) = derv2(lzl,kxl) + d2ijs*dilx*djkz
        derv2(lxl,kyl) = derv2(lxl,kyl) + d2ijs*dily*djkx
        derv2(lyl,kyl) = derv2(lyl,kyl) + d2ijs*dily*djky
        derv2(lzl,kyl) = derv2(lzl,kyl) + d2ijs*dily*djkz
        derv2(lxl,kzl) = derv2(lxl,kzl) + d2ijs*dilz*djkx
        derv2(lyl,kzl) = derv2(lyl,kzl) + d2ijs*dilz*djky
        derv2(lzl,kzl) = derv2(lzl,kzl) + d2ijs*dilz*djkz
!
        derv2(lxl,kxl) = derv2(lxl,kxl) + d2ijs*djlx*dikx
        derv2(lyl,kxl) = derv2(lyl,kxl) + d2ijs*djlx*diky
        derv2(lzl,kxl) = derv2(lzl,kxl) + d2ijs*djlx*dikz
        derv2(lxl,kyl) = derv2(lxl,kyl) + d2ijs*djly*dikx
        derv2(lyl,kyl) = derv2(lyl,kyl) + d2ijs*djly*diky
        derv2(lzl,kyl) = derv2(lzl,kyl) + d2ijs*djly*dikz
        derv2(lxl,kzl) = derv2(lxl,kzl) + d2ijs*djlz*dikx
        derv2(lyl,kzl) = derv2(lyl,kzl) + d2ijs*djlz*diky
        derv2(lzl,kzl) = derv2(lzl,kzl) + d2ijs*djlz*dikz
!
!  d2Edi2/d2Edj2 - only non-zero for QEq/H
!
        if (abs(d2i2s).gt.1.0d-8) then
          derv2(lxl,kxl) = derv2(lxl,kxl) + d2i2s*dilx*dikx
          derv2(lyl,kxl) = derv2(lyl,kxl) + d2i2s*dilx*diky
          derv2(lzl,kxl) = derv2(lzl,kxl) + d2i2s*dilx*dikz
          derv2(lxl,kyl) = derv2(lxl,kyl) + d2i2s*dily*dikx
          derv2(lyl,kyl) = derv2(lyl,kyl) + d2i2s*dily*diky
          derv2(lzl,kyl) = derv2(lzl,kyl) + d2i2s*dily*dikz
          derv2(lxl,kzl) = derv2(lxl,kzl) + d2i2s*dilz*dikx
          derv2(lyl,kzl) = derv2(lyl,kzl) + d2i2s*dilz*diky
          derv2(lzl,kzl) = derv2(lzl,kzl) + d2i2s*dilz*dikz
        endif
        if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
          derv2(lxl,kxl) = derv2(lxl,kxl) + d2j2s*djlx*djkx
          derv2(lyl,kxl) = derv2(lyl,kxl) + d2j2s*djlx*djky
          derv2(lzl,kxl) = derv2(lzl,kxl) + d2j2s*djlx*djkz
          derv2(lxl,kyl) = derv2(lxl,kyl) + d2j2s*djly*djkx
          derv2(lyl,kyl) = derv2(lyl,kyl) + d2j2s*djly*djky
          derv2(lzl,kyl) = derv2(lzl,kyl) + d2j2s*djly*djkz
          derv2(lxl,kzl) = derv2(lxl,kzl) + d2j2s*djlz*djkx
          derv2(lyl,kzl) = derv2(lyl,kzl) + d2j2s*djlz*djky
          derv2(lzl,kzl) = derv2(lzl,kzl) + d2j2s*djlz*djkz
        endif
      endif
!
!  End of loop over l
!
    enddo
!
!  End of loop over k
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
        derv3(ix,kl) = derv3(ix,kl) - d1ix*dqds(kl,i)
        derv3(iy,kl) = derv3(iy,kl) - d1iy*dqds(kl,i)
        derv3(iz,kl) = derv3(iz,kl) - d1iz*dqds(kl,i)
        derv3(ix,kl) = derv3(ix,kl) - d1jx*dqds(kl,j)
        derv3(iy,kl) = derv3(iy,kl) - d1jy*dqds(kl,j)
        derv3(iz,kl) = derv3(iz,kl) - d1jz*dqds(kl,j)
      enddo
    endif
    if (lopj) then
      do kl = 1,nstrains
        derv3(jx,kl) = derv3(jx,kl) + d1jx*dqds(kl,j)
        derv3(jy,kl) = derv3(jy,kl) + d1jy*dqds(kl,j)
        derv3(jz,kl) = derv3(jz,kl) + d1jz*dqds(kl,j)
        derv3(jx,kl) = derv3(jx,kl) + d1ix*dqds(kl,i)
        derv3(jy,kl) = derv3(jy,kl) + d1iy*dqds(kl,i)
        derv3(jz,kl) = derv3(jz,kl) + d1iz*dqds(kl,i)
      enddo
    endif
!
!  Strain-strain terms for charge derivatives
!
    d1si = dsi(1)
    d2si = dsi(2)
    d3si = dsi(3)
    d1sj = dsj(1)
    d2sj = dsj(2)
    d3sj = dsj(3)
    sderv2(1,1) = sderv2(1,1) + 2.0_dp*(d1si*dis1 + d1sj*djs1)
    sderv2(2,1) = sderv2(2,1) + d1si*dis2 + d1sj*djs2
    sderv2(2,1) = sderv2(2,1) + d2si*dis1 + d2sj*djs1
    sderv2(3,1) = sderv2(3,1) + d1si*dis3 + d1sj*djs3
    sderv2(3,1) = sderv2(3,1) + d3si*dis1 + d3sj*djs1
    sderv2(2,2) = sderv2(2,2) + 2.0_dp*(d2si*dis2 + d2sj*djs2)
    sderv2(3,2) = sderv2(3,2) + d2si*dis3 + d2sj*djs3
    sderv2(3,2) = sderv2(3,2) + d3si*dis2 + d3sj*djs2
    sderv2(3,3) = sderv2(3,3) + 2.0_dp*(d3si*dis3 + d3sj*djs3)
!
    sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2ijs*dis1*djs1
    sderv2(2,1) = sderv2(2,1) + d2ijs*(dis1*djs2 + dis2*djs1)
    sderv2(3,1) = sderv2(3,1) + d2ijs*(dis1*djs3 + dis3*djs1)
    sderv2(2,2) = sderv2(2,2) + 2.0_dp*d2ijs*dis2*djs2
    sderv2(3,2) = sderv2(3,2) + d2ijs*(dis2*djs3 + dis3*djs2)
    sderv2(3,3) = sderv2(3,3) + 2.0_dp*d2ijs*dis3*djs3
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
  endif
!
  return
  end
