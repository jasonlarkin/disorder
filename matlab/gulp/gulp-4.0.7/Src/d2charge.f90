  subroutine d2charge(i,j,nor,ix,iy,iz,jx,jy,jz,lopi,lopj,dei,dej,d1xi,d1yi,d1zi, &
                      d1xj,d1yj,d1zj,ds1i,ds1j,d2i2,d2ij,d2j2,d2self,dei0,dej0, &
                      lreal,lDoSelf)
!
!  Calculates the contribution to the second derivative matrices
!  due to charge derivatives from variable charge models
!
!  lreal = .true.  => call from real space routine
!        = .false. => call from reciprocal space routine
!
!   2/01 Structure of rpd changed to suit 2-D case
!  10/02 ReaxFF modifications added
!  11/02 Second strain derivatives corrected
!  11/02 Error in derv3 calculation for QEq(H) case corrected
!   9/04 Order of terms in dqds switched
!   9/04 Modified to handle general charge derivatives and not just EEM
!   9/04 Contribution from d2q/dalpha.dbeta added
!   9/04 dei0/dej0 arguments added
!   9/04 xtmp/ytmp/ztmp removed from arguments and replace by d1ix, d1jx
!        etc being precomputed prior to entry to routine and passed in
!        as d1xi, d1xj etc
!   9/04 rpd/mdis removed & ds1i/ds1j made into sum as per above
!   9/04 lDoSelf flag added to control addition of self term for EEM
!  10/04 Position of adding to d2qds2 to sderv2 corrected
!   7/05 Streitz and Mintmire modifications added
!   7/05 lReverse set to true for k.le.(i.or.j)
!  11/07 Unused variables removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!   9/12 Pacha added
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
!  Julian Gale, NRI, Curtin University, September 2012
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
  integer(i4), intent(in)    :: nor
  logical,     intent(in)    :: lopi
  logical,     intent(in)    :: lopj
  logical,     intent(in)    :: lreal
  logical,     intent(in)    :: lDoSelf
  real(dp),    intent(in)    :: d1xi
  real(dp),    intent(in)    :: d1yi
  real(dp),    intent(in)    :: d1zi
  real(dp),    intent(in)    :: d1xj
  real(dp),    intent(in)    :: d1yj
  real(dp),    intent(in)    :: d1zj
  real(dp),    intent(in)    :: d2i2(*)
  real(dp),    intent(in)    :: d2ij(*)
  real(dp),    intent(in)    :: d2j2(*)
  real(dp),    intent(in)    :: d2self
  real(dp),    intent(in)    :: dei(*)
  real(dp),    intent(in)    :: dej(*)
  real(dp),    intent(in)    :: dei0
  real(dp),    intent(in)    :: dej0
  real(dp),    intent(in)    :: ds1i(*)
  real(dp),    intent(in)    :: ds1j(*)
!
!  Local variables
!
  integer(i4)                :: iix
  integer(i4)                :: iiy
  integer(i4)                :: iiz
  integer(i4)                :: ind
  integer(i4)                :: indi
  integer(i4)                :: indj
  integer(i4)                :: indk
  integer(i4)                :: indl
  integer(i4)                :: iv
  integer(i4)                :: ixl
  integer(i4)                :: iyl
  integer(i4)                :: izl
  integer(i4)                :: jjx
  integer(i4)                :: jjy
  integer(i4)                :: jjz
  integer(i4)                :: jxl
  integer(i4)                :: jyl
  integer(i4)                :: jzl
  integer(i4)                :: k
  integer(i4)                :: kk
  integer(i4)                :: kl
  integer(i4)                :: kll
  integer(i4)                :: kx
  integer(i4)                :: ky
  integer(i4)                :: kz
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
  integer(i4)                :: n
  integer(i4)                :: nx
  integer(i4)                :: nk
  logical                    :: lopk
  logical                    :: lopl
  logical                    :: lNonIJQDeriv
  logical                    :: lReverse
  real(dp)                   :: d1ix
  real(dp)                   :: d1iy
  real(dp)                   :: d1iz
  real(dp)                   :: d1jx
  real(dp)                   :: d1jy
  real(dp)                   :: d1jz
  real(dp)                   :: d2i2s
  real(dp)                   :: d2ijs
  real(dp)                   :: d2j2s
  real(dp)                   :: d2qk
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
  real(dp)                   :: deisum
  real(dp)                   :: dejsum
  real(dp)                   :: dikx
  real(dp)                   :: diky
  real(dp)                   :: dikz
  real(dp)                   :: dilx
  real(dp)                   :: dily
  real(dp)                   :: dilz
  real(dp)                   :: dis1
  real(dp)                   :: dis2
  real(dp)                   :: dis3
  real(dp)                   :: dis4
  real(dp)                   :: dis5
  real(dp)                   :: dis6
  real(dp)                   :: djkx
  real(dp)                   :: djky
  real(dp)                   :: djkz
  real(dp)                   :: djlx
  real(dp)                   :: djly
  real(dp)                   :: djlz
  real(dp)                   :: djs1
  real(dp)                   :: djs2
  real(dp)                   :: djs3
  real(dp)                   :: djs4
  real(dp)                   :: djs5
  real(dp)                   :: djs6
  real(dp)                   :: dkix
  real(dp)                   :: dkiy
  real(dp)                   :: dkiz
  real(dp)                   :: dkjx
  real(dp)                   :: dkjy
  real(dp)                   :: dkjz
  real(dp)                   :: dk1
  real(dp)                   :: dk2
  real(dp)                   :: dk3
  real(dp)                   :: dk4
  real(dp)                   :: dk5
  real(dp)                   :: dk6
  real(dp)                   :: dsi(6)
  real(dp)                   :: dsi2(6)
  real(dp)                   :: dsj(6)
  real(dp)                   :: dsj2(6)
  real(dp)                   :: ock
  real(dp)                   :: qlk
  real(dp)                   :: xdrvk
  real(dp)                   :: ydrvk
  real(dp)                   :: zdrvk
  real(dp)                   :: zetah0
!
!  Check that charge derivatives are in use
!
  if (.not.lDoQDeriv2) return
!
!  Set pointers to i-j second derivative block for eself derivatives
!
  if (lopi.and.lopj) then
    iix = ix
    iiy = iy
    iiz = iz
    jjx = jx
    jjy = jy
    jjz = jz
  elseif (lopi) then
    iix = ix
    iiy = iy
    iiz = iz
    jjx = ix
    jjy = iy
    jjz = iz
  elseif (lopj) then
    iix = jx
    iiy = jy
    iiz = jz
    jjx = jx
    jjy = jy
    jjz = jz
  endif
  if (lqeq) then
    zetah0 = 0.529177_dp*0.75_dp/qeqrad(1)
  endif
  indi = 3*(i - 1)
  indj = 3*(j - 1)
  d2ijs = d2self
  d2i2s = 0.0_dp
  d2j2s = 0.0_dp
  do iv = 1,nor
    d2ijs = d2ijs + d2ij(iv)
    d2i2s = d2i2s + d2i2(iv)
    d2j2s = d2j2s + d2j2(iv)
  enddo
  d1ix = d1xi
  d1iy = d1yi
  d1iz = d1zi
  d1jx = d1xj
  d1jy = d1yj
  d1jz = d1zj
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
    elseif (ndim.eq.2) then
      dis1 = dqds(1,i)
      dis2 = dqds(2,i)
      dis3 = dqds(3,i)
      djs1 = dqds(1,j)
      djs2 = dqds(2,j)
      djs3 = dqds(3,j)
    elseif (ndim.eq.1) then
      dis1 = dqds(1,i)
      djs1 = dqds(1,j)
    endif
    if (lreal) then
!
!  Real space
!
      do kl = 1,nstrains
        kll = nstrptr(kl)
        dsi(kl) = ds1i(kll)
        dsj(kl) = ds1j(kll)
        dsi2(kl) = dsi(kl)
        dsj2(kl) = dsj(kl)
      enddo
    else
!
!  Reciprocal space
!
      do kl = 1,nstrains
        kll = nstrptr(kl)
        dsi(kl) = ds1i(kll)
        dsj(kl) = ds1j(kll)
        dsi2(kl) = dsi(kl)
        dsj2(kl) = dsj(kl)
      enddo
      if (ndim.eq.3) then
        do kl = 1,3
          do iv = 1,nor
            dsi(kl) = dsi(kl) - dei(iv)
            dsj(kl) = dsj(kl) - dej(iv)
          enddo
        enddo
      elseif (ndim.eq.2) then
        do kl = 1,2
          do iv = 1,nor
            dsi(kl) = dsi(kl) - dei(iv)
            dsj(kl) = dsj(kl) - dej(iv)
          enddo
        enddo
      elseif (ndim.eq.1) then
        do iv = 1,nor
          dsi(1) = dsi(1) - dei(iv)
          dsj(1) = dsj(1) - dej(iv)
        enddo
      endif
      if (leem) then
        do kl = 1,nstrains
          dsi2(kl) = dsi(kl)
          dsj2(kl) = dsj(kl)
        enddo
      endif
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
    deisum = dei0
    dejsum = dej0
    do iv = 1,nor
      deisum = deisum + dei(iv)
      dejsum = dejsum + dej(iv)
    enddo
!   
!  Strain - strain contribution
!   
    if (lstr.and.ndim.gt.0) then
      ind = 0
      do kk = 1,nstrains
        do kl = 1,kk-1
          ind = ind + 1
          sderv2(kl,kk) = sderv2(kl,kk) + deisum*d2qds2(ind,i) + dejsum*d2qds2(ind,j)
          sderv2(kk,kl) = sderv2(kk,kl) + deisum*d2qds2(ind,i) + dejsum*d2qds2(ind,j)
        enddo
        ind = ind + 1
        sderv2(kk,kk) = sderv2(kk,kk) + deisum*d2qds2(ind,i) + dejsum*d2qds2(ind,j)
      enddo
    endif
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
!  Mixed strain contribution
!
      if (lstr.and.ndim.gt.0) then
        do kk = 1,nstrains
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
!  Mixed strain contribution
!
      if (lstr.and.ndim.gt.0) then
        do kk = 1,nstrains
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
    indk = 3*(k-1)
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
    if ((lopi.or.lopj).and.lreal.and.lDoSelf) then
!**************************************************
!  Contributions from derivatives of self energy  *
!**************************************************
      nk = nat(k)
      ock = occuf(k)
      if (leem) then
        if (lqeq) then
          if (nk.ne.1) then
            d2qk = 2.0_dp*ock*qeqmu(nk)
          else
            qlk = qf(k)
            d2qk = 2.0_dp*ock*qeqmu(nk)*(1.0_dp + 2.0_dp*qlk/zetah0)
          endif
        elseif (lSandM) then
          d2qk = 2.0_dp*ock*smmu(nk)
        elseif (lpacha) then
          d2qk = 2.0_dp*ock*mu_pacha(nk)
        else
          d2qk = 2.0_dp*ock*rmu(nk)
        endif
      else
        d2qk = 0.0_dp
      endif
      dkix = dqdxyz(indi + 1,k)
      dkiy = dqdxyz(indi + 2,k)
      dkiz = dqdxyz(indi + 3,k)
      dkjx = dqdxyz(indj + 1,k)
      dkjy = dqdxyz(indj + 2,k)
      dkjz = dqdxyz(indj + 3,k)
      derv2(jjx,iix) = derv2(jjx,iix) + d2qk*dkjx*dkix
      derv2(jjy,iix) = derv2(jjy,iix) + d2qk*dkjx*dkiy
      derv2(jjz,iix) = derv2(jjz,iix) + d2qk*dkjx*dkiz
      derv2(jjx,iiy) = derv2(jjx,iiy) + d2qk*dkjy*dkix
      derv2(jjy,iiy) = derv2(jjy,iiy) + d2qk*dkjy*dkiy
      derv2(jjz,iiy) = derv2(jjz,iiy) + d2qk*dkjy*dkiz
      derv2(jjx,iiz) = derv2(jjx,iiz) + d2qk*dkjz*dkix
      derv2(jjy,iiz) = derv2(jjy,iiz) + d2qk*dkjz*dkiy
      derv2(jjz,iiz) = derv2(jjz,iiz) + d2qk*dkjz*dkiz
      if (lstr.and.i.eq.j) then
!
!  Mixed strain terms from self energy - only need to do once
!  for each atom - hence check on i = j
!
        if (lopi) then
          do kl = 1,nstrains
            derv3(iix,kl) = derv3(iix,kl) + d2qk*dqds(kl,k)*dkix
            derv3(iiy,kl) = derv3(iiy,kl) + d2qk*dqds(kl,k)*dkiy
            derv3(iiz,kl) = derv3(iiz,kl) + d2qk*dqds(kl,k)*dkiz
          enddo
        endif
!
!  Strain-strain derivatives - only need to do if i=j=k
!
        if (i.eq.k) then
          if (ndim.eq.3) then
            dk1 = dqds(1,k)
            dk2 = dqds(2,k)
            dk3 = dqds(3,k)
            dk4 = dqds(4,k)
            dk5 = dqds(5,k)
            dk6 = dqds(6,k)
            sderv2(1,1) = sderv2(1,1) + d2qk*dk1*dk1
            sderv2(2,1) = sderv2(2,1) + d2qk*dk2*dk1
            sderv2(3,1) = sderv2(3,1) + d2qk*dk3*dk1
            sderv2(4,1) = sderv2(4,1) + d2qk*dk4*dk1
            sderv2(5,1) = sderv2(5,1) + d2qk*dk5*dk1
            sderv2(6,1) = sderv2(6,1) + d2qk*dk6*dk1
            sderv2(2,2) = sderv2(2,2) + d2qk*dk2*dk2
            sderv2(3,2) = sderv2(3,2) + d2qk*dk3*dk2
            sderv2(4,2) = sderv2(4,2) + d2qk*dk4*dk2
            sderv2(5,2) = sderv2(5,2) + d2qk*dk5*dk2
            sderv2(6,2) = sderv2(6,2) + d2qk*dk6*dk2
            sderv2(3,3) = sderv2(3,3) + d2qk*dk3*dk3
            sderv2(4,3) = sderv2(4,3) + d2qk*dk4*dk3
            sderv2(5,3) = sderv2(5,3) + d2qk*dk5*dk3
            sderv2(6,3) = sderv2(6,3) + d2qk*dk6*dk3
            sderv2(4,4) = sderv2(4,4) + d2qk*dk4*dk4
            sderv2(5,4) = sderv2(5,4) + d2qk*dk5*dk4
            sderv2(6,4) = sderv2(6,4) + d2qk*dk6*dk4
            sderv2(5,5) = sderv2(5,5) + d2qk*dk5*dk5
            sderv2(6,5) = sderv2(6,5) + d2qk*dk6*dk5
            sderv2(6,6) = sderv2(6,6) + d2qk*dk6*dk6
          elseif (ndim.eq.2) then
            dk1 = dqds(1,k)
            dk2 = dqds(2,k)
            dk3 = dqds(3,k)
            sderv2(1,1) = sderv2(1,1) + d2qk*dk1*dk1
            sderv2(2,1) = sderv2(2,1) + d2qk*dk2*dk1
            sderv2(3,1) = sderv2(3,1) + d2qk*dk3*dk1
            sderv2(2,2) = sderv2(2,2) + d2qk*dk2*dk2
            sderv2(3,2) = sderv2(3,2) + d2qk*dk3*dk2
            sderv2(3,3) = sderv2(3,3) + d2qk*dk3*dk3
          elseif (ndim.eq.1) then
            dk1 = dqds(1,k)
            sderv2(1,1) = sderv2(1,1) + d2qk*dk1*dk1
          endif
        endif
      endif
    endif
    if (lstr.and.lopk) then
!
!  Mix strain-internal contribution
!
!  d2E/d(strain).dq x dq/d(alpha)
!
      do kl = 1,nstrains
        derv3(kx,kl) = derv3(kx,kl) + dsi2(kl)*dikx + dsj2(kl)*djkx
        derv3(ky,kl) = derv3(ky,kl) + dsi2(kl)*diky + dsj2(kl)*djky
        derv3(kz,kl) = derv3(kz,kl) + dsi2(kl)*dikz + dsj2(kl)*djkz
      enddo
!
!  d2E/dq.dq x dq/d(alpha) x dq/d(strain)
!
      do kl = 1,nstrains
        derv3(kx,kl) = derv3(kx,kl) + d2ijs*(dqds(kl,i)*djkx + dqds(kl,j)*dikx)
        derv3(ky,kl) = derv3(ky,kl) + d2ijs*(dqds(kl,i)*djky + dqds(kl,j)*diky)
        derv3(kz,kl) = derv3(kz,kl) + d2ijs*(dqds(kl,i)*djkz + dqds(kl,j)*dikz)
      enddo
      if (abs(d2i2s).gt.1.0d-8) then
        do kl = 1,nstrains
          derv3(kx,kl) = derv3(kx,kl) + d2i2s*dqds(kl,i)*dikx
          derv3(ky,kl) = derv3(ky,kl) + d2i2s*dqds(kl,i)*diky
          derv3(kz,kl) = derv3(kz,kl) + d2i2s*dqds(kl,i)*dikz
          derv3(ix,kl) = derv3(ix,kl) - d2i2s*dqds(kl,i)*dikx
          derv3(iy,kl) = derv3(iy,kl) - d2i2s*dqds(kl,i)*diky
          derv3(iz,kl) = derv3(iz,kl) - d2i2s*dqds(kl,i)*dikz
        enddo
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        do kl = 1,nstrains
          derv3(kx,kl) = derv3(kx,kl) + d2j2s*dqds(kl,j)*djkx
          derv3(ky,kl) = derv3(ky,kl) + d2j2s*dqds(kl,j)*djky
          derv3(kz,kl) = derv3(kz,kl) + d2j2s*dqds(kl,j)*djkz
          derv3(jx,kl) = derv3(jx,kl) - d2j2s*dqds(kl,j)*djkx
          derv3(jy,kl) = derv3(jy,kl) - d2j2s*dqds(kl,j)*djky
          derv3(jz,kl) = derv3(jz,kl) - d2j2s*dqds(kl,j)*djkz
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
        dilx = dqdxyz(indl + 1,i)
        dily = dqdxyz(indl + 2,i)
        dilz = dqdxyz(indl + 3,i)
        djlx = dqdxyz(indl + 1,j)
        djly = dqdxyz(indl + 2,j)
        djlz = dqdxyz(indl + 3,j)
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
!  d2Edi2/d2Edj2 
!
        if (abs(d2i2s).gt.1.0d-8) then
          derv2(lxl,kxl) = derv2(lxl,kxl) + d2i2s*dilx*dikx
          derv2(lyl,kxl) = derv2(lyl,kxl) + d2i2s*dily*dikx
          derv2(lzl,kxl) = derv2(lzl,kxl) + d2i2s*dilz*dikx
          derv2(lxl,kyl) = derv2(lxl,kyl) + d2i2s*dilx*diky
          derv2(lyl,kyl) = derv2(lyl,kyl) + d2i2s*dily*diky
          derv2(lzl,kyl) = derv2(lzl,kyl) + d2i2s*dilz*diky
          derv2(lxl,kzl) = derv2(lxl,kzl) + d2i2s*dilx*dikz
          derv2(lyl,kzl) = derv2(lyl,kzl) + d2i2s*dily*dikz
          derv2(lzl,kzl) = derv2(lzl,kzl) + d2i2s*dilz*dikz
        endif
        if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
          derv2(lxl,kxl) = derv2(lxl,kxl) + d2j2s*djlx*djkx
          derv2(lyl,kxl) = derv2(lyl,kxl) + d2j2s*djly*djkx
          derv2(lzl,kxl) = derv2(lzl,kxl) + d2j2s*djlz*djkx
          derv2(lxl,kyl) = derv2(lxl,kyl) + d2j2s*djlx*djky
          derv2(lyl,kyl) = derv2(lyl,kyl) + d2j2s*djly*djky
          derv2(lzl,kyl) = derv2(lzl,kyl) + d2j2s*djlz*djky
          derv2(lxl,kzl) = derv2(lxl,kzl) + d2j2s*djlx*djkz
          derv2(lyl,kzl) = derv2(lyl,kzl) + d2j2s*djly*djkz
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
    if (ndim.eq.3) then
      d1si = dsi(1)
      d2si = dsi(2)
      d3si = dsi(3)
      d4si = dsi(4)
      d5si = dsi(5)
      d6si = dsi(6)
      d1sj = dsj(1)
      d2sj = dsj(2)
      d3sj = dsj(3)
      d4sj = dsj(4)
      d5sj = dsj(5)
      d6sj = dsj(6)
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*(d1si*dis1 + d1sj*djs1)
      sderv2(2,1) = sderv2(2,1) + d1si*dis2 + d1sj*djs2
      sderv2(2,1) = sderv2(2,1) + d2si*dis1 + d2sj*djs1
      sderv2(3,1) = sderv2(3,1) + d1si*dis3 + d1sj*djs3
      sderv2(3,1) = sderv2(3,1) + d3si*dis1 + d3sj*djs1
      sderv2(4,1) = sderv2(4,1) + d1si*dis4 + d1sj*djs4
      sderv2(4,1) = sderv2(4,1) + d4si*dis1 + d4sj*djs1
      sderv2(5,1) = sderv2(5,1) + d1si*dis5 + d1sj*djs5
      sderv2(5,1) = sderv2(5,1) + d5si*dis1 + d5sj*djs1
      sderv2(6,1) = sderv2(6,1) + d1si*dis6 + d1sj*djs6
      sderv2(6,1) = sderv2(6,1) + d6si*dis1 + d6sj*djs1
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*(d2si*dis2+ d2sj*djs2)
      sderv2(3,2) = sderv2(3,2) + d2si*dis3 + d2sj*djs3
      sderv2(3,2) = sderv2(3,2) + d3si*dis2 + d3sj*djs2
      sderv2(4,2) = sderv2(4,2) + d2si*dis4 + d2sj*djs4
      sderv2(4,2) = sderv2(4,2) + d4si*dis2 + d4sj*djs2
      sderv2(5,2) = sderv2(5,2) + d2si*dis5 + d2sj*djs5
      sderv2(5,2) = sderv2(5,2) + d5si*dis2 + d5sj*djs2
      sderv2(6,2) = sderv2(6,2) + d2si*dis6 + d2sj*djs6
      sderv2(6,2) = sderv2(6,2) + d6si*dis2 + d6sj*djs2
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*(d3si*dis3 + d3sj*djs3)
      sderv2(4,3) = sderv2(4,3) + d3si*dis4 + d3sj*djs4
      sderv2(4,3) = sderv2(4,3) + d4si*dis3 + d4sj*djs3
      sderv2(5,3) = sderv2(5,3) + d3si*dis5 + d3sj*djs5
      sderv2(5,3) = sderv2(5,3) + d5si*dis3 + d5sj*djs3
      sderv2(6,3) = sderv2(6,3) + d3si*dis6 + d3sj*djs6
      sderv2(6,3) = sderv2(6,3) + d6si*dis3 + d6sj*djs3
      sderv2(4,4) = sderv2(4,4) + 2.0_dp*(d4si*dis4 + d4sj*djs4)
      sderv2(5,4) = sderv2(5,4) + d4si*dis5 + d4sj*djs5
      sderv2(5,4) = sderv2(5,4) + d5si*dis4 + d5sj*djs4
      sderv2(6,4) = sderv2(6,4) + d4si*dis6 + d4sj*djs6
      sderv2(6,4) = sderv2(6,4) + d6si*dis4 + d6sj*djs4
      sderv2(5,5) = sderv2(5,5) + 2.0_dp*(d5si*dis5 + d5sj*djs5)
      sderv2(6,5) = sderv2(6,5) + d5si*dis6 + d5sj*djs6
      sderv2(6,5) = sderv2(6,5) + d6si*dis5 + d6sj*djs5
      sderv2(6,6) = sderv2(6,6) + 2.0_dp*(d6si*dis6 + d6sj*djs6)
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
    elseif (ndim.eq.2) then
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
    elseif (ndim.eq.1) then
      d1si = dsi(1)
      d1sj = dsj(1)
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*(d1si*dis1 + d1sj*djs1)
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*d2ijs*dis1*djs1
      if (abs(d2i2s).gt.1.0d-8) then
        sderv2(1,1) = sderv2(1,1) + d2i2s*dis1*dis1
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        sderv2(1,1) = sderv2(1,1) + d2j2s*djs1*djs1
      endif
    endif
  endif
!
!  Remove terms from derv3 that get added in in strfin but shouldn't be 
!  Only applies to non-Hellmann-Feynman methods, such as bond order charges
!
  if (lstr.and.lreal.and..not.leem) then
    if (ndim.eq.3) then
      kx = -2
      ky = -1
      kz =  0
      do k = 1,numat
        lopk = (.not.lfreeze.or.lopf(nrelat(k)))
        if (lopk) then
          kx = kx + 3
          ky = ky + 3
          kz = kz + 3
          xdrvk = deisum*dqdxyz(kx,i) + dejsum*dqdxyz(kx,j)
          ydrvk = deisum*dqdxyz(ky,i) + dejsum*dqdxyz(ky,j)
          zdrvk = deisum*dqdxyz(kz,i) + dejsum*dqdxyz(kz,j)
          derv3(kx,5) = derv3(kx,5) - zdrvk
          derv3(kx,6) = derv3(kx,6) - ydrvk
          derv3(ky,4) = derv3(ky,4) - zdrvk
          derv3(ky,6) = derv3(ky,6) - xdrvk
          derv3(kz,4) = derv3(kz,4) - ydrvk
          derv3(kz,5) = derv3(kz,5) - xdrvk
          derv3(kx,1) = derv3(kx,1) - 2.0_dp*xdrvk
          derv3(ky,2) = derv3(ky,2) - 2.0_dp*ydrvk
          derv3(kz,3) = derv3(kz,3) - 2.0_dp*zdrvk
        endif
      enddo
    elseif (ndim.eq.2) then
      kx = -2
      ky = -1
      do k = 1,numat
        lopk = (.not.lfreeze.or.lopf(k))  
        if (lopk) then
          kx = kx + 3
          ky = ky + 3
          xdrvk = deisum*dqdxyz(kx,i) + dejsum*dqdxyz(kx,j)
          ydrvk = deisum*dqdxyz(ky,i) + dejsum*dqdxyz(ky,j)
          derv3(kx,1) = derv3(kx,1) - 2.0_dp*xdrvk
          derv3(ky,2) = derv3(ky,2) - 2.0_dp*ydrvk
          derv3(kx,3) = derv3(kx,3) - ydrvk
          derv3(ky,3) = derv3(ky,3) - xdrvk
        endif
      enddo
    elseif (ndim.eq.1) then
      kx = - 2
      do k = 1,numat
        lopk = (.not.lfreeze.or.lopf(k))  
        if (lopk) then
          kx = kx + 3
          xdrvk = deisum*dqdxyz(kx,i) + dejsum*dqdxyz(kx,j)
          derv3(kx,1) = derv3(kx,1) - 2.0_dp*xdrvk
        endif
      enddo
    endif
  endif
!
  return
  end
