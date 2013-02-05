  subroutine d2chargep(i,j,nor,ix,iy,iz,jx,jy,jz,xkv,ykv,zkv,dei,dej, &
                       d1ixr,d1iyr,d1izr,d1jxr,d1jyr,d1jzr,d2i2r,d2ijr, &
                       d2j2r,d2self,dei0,dej0,lreal)
!
!  Calculates the contribution to the dynamical matrix due to charge derivatives from EEM/QEq. 
!
!  At present this is gamma point only.
!
!  lreal = .true.  => call from real space routine
!        = .false. => call from reciprocal space routine
!
!   2/98 Created from d2charge
!  10/02 ReaxFF modifications added
!  11/02 K vector now passed in for phasing
!   3/03 Frozen charge modifications added
!   9/04 Modified to generalise to charge derivatives other than from EEM
!   9/04 Contribution from d2q/dalpha.dbeta added
!   9/04 dei0/dej0 arguments added
!   9/04 xtmp/ytmp/ztmp premultiplied before entry to routine
!   7/05 Streitz and Mintmire modifications added
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
  integer(i4), intent(in)  :: nor
  logical,     intent(in)  :: lreal
  real(dp),    intent(in)  :: dei(*)
  real(dp),    intent(in)  :: dej(*)
  real(dp),    intent(in)  :: dei0
  real(dp),    intent(in)  :: dej0
  real(dp),    intent(in)  :: d1ixr
  real(dp),    intent(in)  :: d1iyr
  real(dp),    intent(in)  :: d1izr
  real(dp),    intent(in)  :: d1jxr
  real(dp),    intent(in)  :: d1jyr
  real(dp),    intent(in)  :: d1jzr
  real(dp),    intent(in)  :: d2i2r(*)
  real(dp),    intent(in)  :: d2ijr(*)
  real(dp),    intent(in)  :: d2j2r(*)
  real(dp),    intent(in)  :: d2self
  real(dp),    intent(in)  :: xkv
  real(dp),    intent(in)  :: ykv
  real(dp),    intent(in)  :: zkv
!
!  Local variables     
!
  integer(i4)              :: iv    
  integer(i4)              :: k
  integer(i4)              :: kx
  integer(i4)              :: ky 
  integer(i4)              :: kz  
  integer(i4)              :: l      
  integer(i4)              :: lx     
  integer(i4)              :: ly    
  integer(i4)              :: lz    
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
  integer(i4)              :: nk
  logical                  :: lNonIJQDeriv
  real(dp)                 :: d1ix
  real(dp)                 :: d1iy
  real(dp)                 :: d1iz
  real(dp)                 :: d1jx
  real(dp)                 :: d1jy
  real(dp)                 :: d1jz
  real(dp)                 :: d2i2s
  real(dp)                 :: d2ijs
  real(dp)                 :: d2j2s
  real(dp)                 :: d2qk 
  real(dp)                 :: deisum
  real(dp)                 :: dejsum
  real(dp)                 :: dikx
  real(dp)                 :: diky
  real(dp)                 :: dikz
  real(dp)                 :: dilx
  real(dp)                 :: dily
  real(dp)                 :: dilz 
  real(dp)                 :: djkx  
  real(dp)                 :: djky  
  real(dp)                 :: djkz   
  real(dp)                 :: djlx   
  real(dp)                 :: djly
  real(dp)                 :: djlz
  real(dp)                 :: dkix
  real(dp)                 :: dkiy
  real(dp)                 :: dkiz 
  real(dp)                 :: dkjx 
  real(dp)                 :: dkjy 
  real(dp)                 :: dkjz 
  real(dp)                 :: ock 
  real(dp)                 :: qlk 
  real(dp)                 :: zetah0
!
!  Check that charge derivatives are in use
!
  if (.not.lDoQDeriv2) return
!
!  Set pointers to i-j second derivative block for eself derivatives
!
  if (lqeq) then
    zetah0 = 0.529177_dp*0.75_dp/qeqrad(1)
  endif
  d1ix = d1ixr
  d1iy = d1iyr
  d1iz = d1izr
  d1jx = d1jxr
  d1jy = d1jyr
  d1jz = d1jzr
  d2ijs = d2self
  d2i2s = 0.0_dp
  d2j2s = 0.0_dp
  do iv = 1,nor
    d2ijs = d2ijs + d2ijr(iv)
    d2i2s = d2i2s + d2i2r(iv)
    d2j2s = d2j2s + d2j2r(iv)
  enddo
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
  kx = - 2
  ky = - 1
  kz =   0
  do k = 1,numat
    kx = kx + 3
    ky = ky + 3
    kz = kz + 3
    dikx = dqdxyz(kx,i)
    diky = dqdxyz(ky,i)
    dikz = dqdxyz(kz,i)
    djkx = dqdxyz(kx,j)
    djky = dqdxyz(ky,j)
    djkz = dqdxyz(kz,j)
    if (i.ne.k) then
!
!  d2E/(d(alpha).dq) x dq/d(beta) : i-k
!
      if (k.le.i) then
        derv2(kx,ix) = derv2(kx,ix) - d1ix*dikx
        derv2(ky,ix) = derv2(ky,ix) - d1ix*diky
        derv2(kz,ix) = derv2(kz,ix) - d1ix*dikz
        derv2(kx,iy) = derv2(kx,iy) - d1iy*dikx
        derv2(ky,iy) = derv2(ky,iy) - d1iy*diky
        derv2(kz,iy) = derv2(kz,iy) - d1iy*dikz
        derv2(kx,iz) = derv2(kx,iz) - d1iz*dikx
        derv2(ky,iz) = derv2(ky,iz) - d1iz*diky
        derv2(kz,iz) = derv2(kz,iz) - d1iz*dikz
!
        derv2(kx,ix) = derv2(kx,ix) - d1jx*djkx
        derv2(ky,ix) = derv2(ky,ix) - d1jx*djky
        derv2(kz,ix) = derv2(kz,ix) - d1jx*djkz
        derv2(kx,iy) = derv2(kx,iy) - d1jy*djkx
        derv2(ky,iy) = derv2(ky,iy) - d1jy*djky
        derv2(kz,iy) = derv2(kz,iy) - d1jy*djkz
        derv2(kx,iz) = derv2(kx,iz) - d1jz*djkx
        derv2(ky,iz) = derv2(ky,iz) - d1jz*djky
        derv2(kz,iz) = derv2(kz,iz) - d1jz*djkz
      else
        derv2(ix,kx) = derv2(ix,kx) - d1ix*dikx
        derv2(iy,kx) = derv2(iy,kx) - d1iy*dikx
        derv2(iz,kx) = derv2(iz,kx) - d1iz*dikx
        derv2(ix,ky) = derv2(ix,ky) - d1ix*diky
        derv2(iy,ky) = derv2(iy,ky) - d1iy*diky
        derv2(iz,ky) = derv2(iz,ky) - d1iz*diky
        derv2(ix,kz) = derv2(ix,kz) - d1ix*dikz
        derv2(iy,kz) = derv2(iy,kz) - d1iy*dikz
        derv2(iz,kz) = derv2(iz,kz) - d1iz*dikz
!
        derv2(ix,kx) = derv2(ix,kx) - d1jx*djkx
        derv2(iy,kx) = derv2(iy,kx) - d1jy*djkx
        derv2(iz,kx) = derv2(iz,kx) - d1jz*djkx
        derv2(ix,ky) = derv2(ix,ky) - d1jx*djky
        derv2(iy,ky) = derv2(iy,ky) - d1jy*djky
        derv2(iz,ky) = derv2(iz,ky) - d1jz*djky
        derv2(ix,kz) = derv2(ix,kz) - d1jx*djkz
        derv2(iy,kz) = derv2(iy,kz) - d1jy*djkz
        derv2(iz,kz) = derv2(iz,kz) - d1jz*djkz
      endif
    endif
    if (j.ne.k) then
!
!  d2E/(d(alpha).dq) x dq/d(beta) : j-k
!
      if (k.le.j) then
        derv2(kx,jx) = derv2(kx,jx) + d1jx*djkx
        derv2(ky,jx) = derv2(ky,jx) + d1jx*djky
        derv2(kz,jx) = derv2(kz,jx) + d1jx*djkz
        derv2(kx,jy) = derv2(kx,jy) + d1jy*djkx
        derv2(ky,jy) = derv2(ky,jy) + d1jy*djky
        derv2(kz,jy) = derv2(kz,jy) + d1jy*djkz
        derv2(kx,jz) = derv2(kx,jz) + d1jz*djkx
        derv2(ky,jz) = derv2(ky,jz) + d1jz*djky
        derv2(kz,jz) = derv2(kz,jz) + d1jz*djkz
!
        derv2(kx,jx) = derv2(kx,jx) + d1ix*dikx
        derv2(ky,jx) = derv2(ky,jx) + d1ix*diky
        derv2(kz,jx) = derv2(kz,jx) + d1ix*dikz
        derv2(kx,jy) = derv2(kx,jy) + d1iy*dikx
        derv2(ky,jy) = derv2(ky,jy) + d1iy*diky
        derv2(kz,jy) = derv2(kz,jy) + d1iy*dikz
        derv2(kx,jz) = derv2(kx,jz) + d1iz*dikx
        derv2(ky,jz) = derv2(ky,jz) + d1iz*diky
        derv2(kz,jz) = derv2(kz,jz) + d1iz*dikz
      else
        derv2(jx,kx) = derv2(jx,kx) + d1jx*djkx
        derv2(jy,kx) = derv2(jy,kx) + d1jy*djkx
        derv2(jz,kx) = derv2(jz,kx) + d1jz*djkx
        derv2(jx,ky) = derv2(jx,ky) + d1jx*djky
        derv2(jy,ky) = derv2(jy,ky) + d1jy*djky
        derv2(jz,ky) = derv2(jz,ky) + d1jz*djky
        derv2(jx,kz) = derv2(jx,kz) + d1jx*djkz
        derv2(jy,kz) = derv2(jy,kz) + d1jy*djkz
        derv2(jz,kz) = derv2(jz,kz) + d1jz*djkz
!
        derv2(jx,kx) = derv2(jx,kx) + d1ix*dikx
        derv2(jy,kx) = derv2(jy,kx) + d1iy*dikx
        derv2(jz,kx) = derv2(jz,kx) + d1iz*dikx
        derv2(jx,ky) = derv2(jx,ky) + d1ix*diky
        derv2(jy,ky) = derv2(jy,ky) + d1iy*diky
        derv2(jz,ky) = derv2(jz,ky) + d1iz*diky
        derv2(jx,kz) = derv2(jx,kz) + d1ix*dikz
        derv2(jy,kz) = derv2(jy,kz) + d1iy*dikz
        derv2(jz,kz) = derv2(jz,kz) + d1iz*dikz
      endif
    endif
    if (lreal.and.i.ne.j) then
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
            d2qk = 2.0_dp*ock*qeqmu(nk)*(1.0_dp+2.0_dp*qlk/zetah0)
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
      dkix = dqdxyz(ix,k)
      dkiy = dqdxyz(iy,k)
      dkiz = dqdxyz(iz,k)
      dkjx = dqdxyz(jx,k)
      dkjy = dqdxyz(jy,k)
      dkjz = dqdxyz(jz,k)
      derv2(jx,ix) = derv2(jx,ix) + d2qk*dkjx*dkix
      derv2(jy,ix) = derv2(jy,ix) + d2qk*dkjx*dkiy
      derv2(jz,ix) = derv2(jz,ix) + d2qk*dkjx*dkiz
      derv2(jx,iy) = derv2(jx,iy) + d2qk*dkjy*dkix
      derv2(jy,iy) = derv2(jy,iy) + d2qk*dkjy*dkiy
      derv2(jz,iy) = derv2(jz,iy) + d2qk*dkjy*dkiz
      derv2(jx,iz) = derv2(jx,iz) + d2qk*dkjz*dkix
      derv2(jy,iz) = derv2(jy,iz) + d2qk*dkjz*dkiy
      derv2(jz,iz) = derv2(jz,iz) + d2qk*dkjz*dkiz
    endif
!
!  Loop over atoms to add ij-kl correction
!
    lx = - 2
    ly = - 1
    lz =   0
    do l = 1,k-1
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
!
      derv2(lx,kx) = derv2(lx,kx) + d2ijs*dilx*djkx
      derv2(ly,kx) = derv2(ly,kx) + d2ijs*dilx*djky
      derv2(lz,kx) = derv2(lz,kx) + d2ijs*dilx*djkz
      derv2(lx,ky) = derv2(lx,ky) + d2ijs*dily*djkx
      derv2(ly,ky) = derv2(ly,ky) + d2ijs*dily*djky
      derv2(lz,ky) = derv2(lz,ky) + d2ijs*dily*djkz
      derv2(lx,kz) = derv2(lx,kz) + d2ijs*dilz*djkx
      derv2(ly,kz) = derv2(ly,kz) + d2ijs*dilz*djky
      derv2(lz,kz) = derv2(lz,kz) + d2ijs*dilz*djkz
!
      derv2(lx,kx) = derv2(lx,kx) + d2ijs*djlx*dikx
      derv2(ly,kx) = derv2(ly,kx) + d2ijs*djlx*diky
      derv2(lz,kx) = derv2(lz,kx) + d2ijs*djlx*dikz
      derv2(lx,ky) = derv2(lx,ky) + d2ijs*djly*dikx
      derv2(ly,ky) = derv2(ly,ky) + d2ijs*djly*diky
      derv2(lz,ky) = derv2(lz,ky) + d2ijs*djly*dikz
      derv2(lx,kz) = derv2(lx,kz) + d2ijs*djlz*dikx
      derv2(ly,kz) = derv2(ly,kz) + d2ijs*djlz*diky
      derv2(lz,kz) = derv2(lz,kz) + d2ijs*djlz*dikz
!
!  d2Edi2/d2Edj2 - only non-zero for QEq/H
!
      if (abs(d2i2s).gt.1.0d-8) then
        derv2(lx,kx) = derv2(lx,kx) + d2i2s*dilx*dikx
        derv2(ly,kx) = derv2(ly,kx) + d2i2s*dilx*diky
        derv2(lz,kx) = derv2(lz,kx) + d2i2s*dilx*dikz
        derv2(lx,ky) = derv2(lx,ky) + d2i2s*dily*dikx
        derv2(ly,ky) = derv2(ly,ky) + d2i2s*dily*diky
        derv2(lz,ky) = derv2(lz,ky) + d2i2s*dily*dikz
        derv2(lx,kz) = derv2(lx,kz) + d2i2s*dilz*dikx
        derv2(ly,kz) = derv2(ly,kz) + d2i2s*dilz*diky
        derv2(lz,kz) = derv2(lz,kz) + d2i2s*dilz*dikz
      endif
      if (abs(d2j2s).gt.1.0d-8.and.i.ne.j) then
        derv2(lx,kx) = derv2(lx,kx) + d2j2s*djlx*djkx
        derv2(ly,kx) = derv2(ly,kx) + d2j2s*djlx*djky
        derv2(lz,kx) = derv2(lz,kx) + d2j2s*djlx*djkz
        derv2(lx,ky) = derv2(lx,ky) + d2j2s*djly*djkx
        derv2(ly,ky) = derv2(ly,ky) + d2j2s*djly*djky
        derv2(lz,ky) = derv2(lz,ky) + d2j2s*djly*djkz
        derv2(lx,kz) = derv2(lx,kz) + d2j2s*djlz*djkx
        derv2(ly,kz) = derv2(ly,kz) + d2j2s*djlz*djky
        derv2(lz,kz) = derv2(lz,kz) + d2j2s*djlz*djkz
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
  return
  end
