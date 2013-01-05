  subroutine four3d3(nkp,iin,jin,d3,d3r,d3i,d3s,d3rs,d3is,nmanyk,nforkl)
!
!  Subroutine for third derivatives of four-body energy for i-j
!  pair. Called from realrecip3d3. On entry we already have a possible
!  valid i-j pair. Currently uses fourbody list method to make
!  this job easier.
!
!  All d33 type arrays are stored as full matrix for each component :
!    1 = xxx 10 = xxy 19 = xxz |
!    2 = yxx 11 = yxy 20 = yxz |
!    3 = zxx 12 = zxy 21 = zxz |for 1-3
!    4 = xyx 13 = xyy 22 = xyz |repeat in 28-54 for 2-3
!    5 = yyx 14 = yyy 23 = yyz |
!    6 = zyx 15 = zyy 24 = zyz |
!    7 = xzx 16 = xzy 25 = xzz |
!    8 = yzx 17 = yzy 26 = yzz |
!    9 = zzx 18 = zzy 27 = zzz |
!  same format is used for d34
!
!  iin / jin   = atoms of second derivative matrix block
!  nmanyk      = number of third atoms with valid derivatives
!  nptrmanyk   = pointer to stored position of derivatives w.r.t. third atom
!  d34         = third derivatives between k and l for i-j block
!  nptrfork    = pointer to atom k for each d34 block
!  nptrforl    = pointer to atom l for each d34 block
!  nforkl      = number of k-l third derivative blocks in d34
!
!   7/98 Created based on four0d3.f and three3d3.f
!   8/98 Extra array for k-l third derivatives of i-j block added
!   8/98 Modified to handle out of plane potentials with same code
!   6/00 xc1,yc1,zc1,x21,y21,z21 removed from the argument list as
!        these values are not needed to be passed and were being
!        corrupted
!   7/02 K4 added for outofplane potential
!  10/02 Torharm potential added
!  11/02 Wildcard atom types added
!  12/02 ntypi/ntypj removed as arguments since they were being set 
!        internally
!   4/03 Exponentially decaying torsion added
!   4/03 Tapered torsion added
!  11/03 Workspace arrays now in module for resizing
!  11/04 Torangle potential added
!  10/05 Inversion potential added
!   6/06 Inversion squared potential added
!   9/06 Order of atom search changed to allow for Dreiding
!   9/06 Dreiding scheme for force constant added as an option
!   1/07 Wildcard handling in lmatch calls corrected
!   1/07 UFF4 added
!  10/07 Angle-angle cross potential added
!  11/07 Unused variables removed
!   5/08 UFFoop potential added
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 Type checking algorithm changed
!  11/08 Corrections for potential dependent swapping of terms according to atom 
!        assignments added.
!   2/10 Handling of i-j vector across cell boundary for out of
!        plane term corrected
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, February 2010
!
  use constants
  use current
  use feworkspace
  use four
  use ksample
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: iin
  integer(i4)                                  :: jin
  integer(i4)                                  :: nforkl
  integer(i4)                                  :: nkp
  integer(i4)                                  :: nmanyk
  real(dp)                                     :: d3(3,3,3)
  real(dp)                                     :: d3i(3,3,3)
  real(dp)                                     :: d3r(3,3,3)
  real(dp)                                     :: d3s(3,3,6)
  real(dp)                                     :: d3is(3,3,6)
  real(dp)                                     :: d3rs(3,3,6)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: id3
  integer(i4)                                  :: ii
  integer(i4)                                  :: ij
  integer(i4)                                  :: ijmap
  integer(i4)                                  :: ik
  integer(i4)                                  :: ikmap
  integer(i4)                                  :: il
  integer(i4)                                  :: ilmap
  integer(i4)                                  :: imap
  integer(i4)                                  :: ind
  integer(i4)                                  :: isgn
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: j1
  integer(i4)                                  :: j2
  integer(i4)                                  :: jimap
  integer(i4)                                  :: jk
  integer(i4)                                  :: jkmap
  integer(i4)                                  :: jl
  integer(i4)                                  :: jlmap
  integer(i4)                                  :: jmap
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: k
  integer(i4)                                  :: k1
  integer(i4)                                  :: k2
  integer(i4)                                  :: k3
  integer(i4)                                  :: kb2(6,6)
  integer(i4)                                  :: kb3(6,6,6)
  integer(i4)                                  :: kd41
  integer(i4)                                  :: kd42
  integer(i4)                                  :: kin
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: klmap
  integer(i4)                                  :: kmap
  integer(i4)                                  :: kx
  integer(i4)                                  :: ky
  integer(i4)                                  :: kz
  integer(i4)                                  :: l
  integer(i4)                                  :: lin
  integer(i4)                                  :: lmap
  integer(i4)                                  :: mm
  integer(i4)                                  :: n
  integer(i4)                                  :: n3v1
  integer(i4)                                  :: n3v2
  integer(i4)                                  :: n3v3
  integer(i4)                                  :: n3vec(3,4)
  integer(i4)                                  :: nfortype
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl
  integer(i4)                                  :: npha
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: ntmp
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: nvmap(4,4)
  logical                                      :: limatch1
  logical                                      :: limatch4
  logical                                      :: ljmatch2
  logical                                      :: ljmatch3
  logical                                      :: lkmatch2
  logical                                      :: lkmatch3
  logical                                      :: lkfound
  logical                                      :: llfound
  logical                                      :: llmatch1
  logical                                      :: llmatch4
  logical                                      :: lmatch
  real(dp)                                     :: cos12
  real(dp)                                     :: d3l(3,3,3)
  real(dp)                                     :: d33l1(54)
  real(dp)                                     :: d33l2(54)
  real(dp)                                     :: d34l(27)
  real(dp)                                     :: d3sl(3,3,6)
  real(dp)                                     :: e1d(6)
  real(dp)                                     :: e2d(21)
  real(dp)                                     :: e3d(56)
  real(dp)                                     :: eterm
  real(dp)                                     :: fpoly(5)
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ocl 
  real(dp)                                     :: ofct 
  real(dp)                                     :: phi0 
  real(dp)                                     :: phi0o
  real(dp)                                     :: r21
  real(dp)                                     :: r212
  real(dp)                                     :: r31
  real(dp)                                     :: r312
  real(dp)                                     :: r32
  real(dp)                                     :: r322
  real(dp)                                     :: r41
  real(dp)                                     :: r412
  real(dp)                                     :: r42
  real(dp)                                     :: r422
  real(dp)                                     :: r43
  real(dp)                                     :: r432
  real(dp)                                     :: rkfor
  real(dp)                                     :: rkfor4
  real(dp)                                     :: rko
  real(dp)                                     :: rn
  real(dp)                                     :: rtmp
  real(dp)                                     :: sin12
  real(dp)                                     :: vec(3,3,4)
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x32
  real(dp)                                     :: y32
  real(dp)                                     :: z32
  real(dp)                                     :: x42
  real(dp)                                     :: y42
  real(dp)                                     :: z42
  real(dp)                                     :: x43
  real(dp)                                     :: y43
  real(dp)                                     :: z43
  real(dp)                                     :: xc1
  real(dp)                                     :: yc1
  real(dp)                                     :: zc1
  real(dp)                                     :: xc2
  real(dp)                                     :: yc2
  real(dp)                                     :: zc2
  real(dp)                                     :: xc3
  real(dp)                                     :: yc3
  real(dp)                                     :: zc3
  real(dp)                                     :: xc4
  real(dp)                                     :: yc4
  real(dp)                                     :: zc4
  real(dp)                                     :: xk
  real(dp)                                     :: yk
  real(dp)                                     :: zk
  real(dp)                                     :: xkv
  real(dp)                                     :: ykv
  real(dp)                                     :: zkv
!
  data kb2/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17, &
          18,5,10,14,17,19,20,6,11,15,18,20,21/
  data kb3/1,2,3,4,5,6,2,7,8,9,10,11,3,8,12,13,14,15,4,9,13,16,17, &
          18,5,10,14,17,19,20,6,11,15,18,20,21, &
          2,7,8,9,10,11,7,22,23,24,25,26,8,23,27,28,29,30,9,24,28, &
          31,32,33,10,25,29,32,34,35,11,26,30,33,35,36, &
          3,8,12,13,14,15,8,23,27,28,29,30,12,27,37,38,39,40,13, &
          28,38,41,42,43,14,29,39,42,44,45,15,30,40,43,45,46, &
          4,9,13,16,17,18,9,24,28,31,32,33,13,28,38,41,42,43,16,31, &
          41,47,48,49,17,32,42,48,50,51,18,33,43,49,51,52, &
          5,10,14,17,19,20,10,25,29,32,34,35,14,29,39,42,44,45,17, &
          32,42,48,50,51,19,34,44,50,53,54,20,35,45,51,54,55, &
          6,11,15,18,20,21,11,26,30,33,35,36,15,30,40,43,45,46,18, &
          33,43,49,51,52,20,35,45,51,54,55,21,36,46,52,55,56/
  data n3vec/1,2,3,1,4,5,2,4,6,3,5,6/
  data nvmap/0,1,2,3,1,0,4,5,2,4,0,6,3,5,6,0/
!
!  Calculate cartesian K vector components
!
  xk = xkpt(nkp)
  yk = ykpt(nkp)
  zk = zkpt(nkp)
  xkv = xk*kv(1,1) + yk*kv(1,2) + zk*kv(1,3)
  ykv = xk*kv(2,1) + yk*kv(2,2) + zk*kv(2,3)
  zkv = xk*kv(3,1) + yk*kv(3,2) + zk*kv(3,3)
!*****************************
!  Loop over four-body list  *
!*****************************
  do 10 mm = 1,nlist4md
    n = nforptr(mm)
    ind = ilind(mm)
    l = ind/(numat+1)
    i = ind - l*(numat+1)
    ind = jkind(mm)
    k = ind/(numat+1)
    j = ind - k*(numat+1)
!
!  Decide which atoms i / j / k / l map to
!
    imap = 0
    jmap = 0
    kmap = 0
    lmap = 0
    if (iin.eq.i) imap = 1
    if (iin.eq.j) imap = 2
    if (iin.eq.k) imap = 3
    if (iin.eq.l) imap = 4
    if (jin.eq.i.and.imap.ne.1) jmap = 1
    if (jin.eq.j.and.imap.ne.2) jmap = 2
    if (jin.eq.k.and.imap.ne.3) jmap = 3
    if (jin.eq.l.and.imap.ne.4) jmap = 4
    do kk = 1,4
      if (imap.ne.kk.and.jmap.ne.kk) kmap = kk
    enddo
    do kk = 1,4
      if (imap.ne.kk.and.jmap.ne.kk.and.kmap.ne.kk) lmap = kk
    enddo
    if (kmap.eq.1) then
      kin = i
    elseif (kmap.eq.2) then
      kin = j
    elseif (kmap.eq.3) then
      kin = k
    elseif (kmap.eq.4) then
      kin = l
    endif
    if (lmap.eq.1) then
      lin = i
    elseif (lmap.eq.2) then
      lin = j
    elseif (lmap.eq.3) then
      lin = k
    elseif (lmap.eq.4) then
      lin = l
    endif
!
!  Does this potential contain target i and j atoms?
!  If not then we can skip this potential
!
    if (imap.eq.0.or.jmap.eq.0) goto 10
!
    ii = icell41(mm)
    if (ii.eq.0) then
      ix = 0
      iy = 0
      iz = 0
    else
      iz = (ii/100) - 5
      ii = ii - 100*(iz+5)
      iy = (ii/10) - 5
      ii = ii - 10*(iy+5)
      ix = ii - 5
    endif
    ii = icell42(mm)
    if (ii.eq.0) then
      jx = 0
      jy = 0
      jz = 0
    else
      jz = (ii/100) - 5
      ii = ii - 100*(jz+5)
      jy = (ii/10) - 5
      ii = ii - 10*(jy+5)
      jx = ii - 5
    endif
    ii = icell43(mm)
    if (ii.eq.0) then
      kx = 0
      ky = 0
      kz = 0
    else
      kz = (ii/100) - 5
      ii = ii - 100*(kz+5)
      ky = (ii/10) - 5
      ii = ii - 10*(ky+5)
      kx = ii - 5
    endif
    nt1 = nfspec1(n)
    nt2 = nfspec2(n)
    nt3 = nfspec3(n)
    nt4 = nfspec4(n)
    ntyp1 = nfptyp1(n)
    ntyp2 = nfptyp2(n)
    ntyp3 = nfptyp3(n)
    ntyp4 = nfptyp4(n)
    nfortype = nforty(n)
    rkfor = fork(n)
    if (lfdreiding(n).and.ilnum(mm).gt.0) then
      rkfor = rkfor/dble(ilnum(mm))
    endif
    npha = 0
    if (nfortype.eq.1) then
      npha = npfor(n)
      if (npha.gt.0) then
        isgn = 1
      else
        isgn = - 1
      endif
      npha = abs(npha)
      phi0 = forpoly(1,n)*degtorad
    elseif (nfortype.eq.4.or.nfortype.eq.6.or.nfortype.eq.7) then
      npha = npfor(n)
      if (npha.gt.0) then
        isgn = 1
      else
        isgn = - 1
      endif
      npha = abs(npha)
      phi0 = forpoly(1,n)
      if (nfortype.eq.6.or.nfortype.eq.7) then
        fpoly(2:4) = forpoly(2:4,n)
      endif
    elseif (nfortype.eq.8.or.nfortype.eq.9) then
      npha = npfor(n)
      if (npha.gt.0) then
        isgn = 1
      else
        isgn = - 1
      endif
      npha = abs(npha)
      if (nfortype.eq.8) then
        phi0 = forpoly(1,n)*degtorad
      else
        phi0 = forpoly(1,n)
      endif
      fpoly(2) = forpoly(2,n)
      fpoly(3) = for1(n)
      fpoly(4) = for2(n)
      fpoly(5) = for3(n)
    elseif (nfortype.eq.2) then
      npha = npfor(n)
    elseif (nfortype.eq.3) then
      rkfor4 = forpoly(1,n)
    elseif (nfortype.eq.5) then
      phi0 = forpoly(1,n)*degtorad
    elseif (nfortype.eq.10.or.nfortype.eq.17) then
      fpoly(1) = forpoly(1,n)*degtorad
      fpoly(2) = forpoly(2,n)*degtorad
    elseif (nfortype.eq.12) then
      fpoly(1) = forpoly(1,n)
    elseif (nfortype.eq.13) then
      npha = abs(npfor(n))
      phi0 = forpoly(1,n)*degtorad
    elseif (nfortype.eq.15) then
      fpoly(1) = forpoly(1,n)
      fpoly(2) = forpoly(2,n)
      fpoly(3) = forpoly(3,n)
    endif
    rn = dble(npha)
!
!  First site
!
    ni = nat(i)
    ntypi = nftype(i)
    oci = occuf(i)
    if (loutofplane(n)) then
      xc1 = xclat(i)
      yc1 = yclat(i)
      zc1 = zclat(i)
    else
      xc1 = xclat(i) + ix*r1x + iy*r2x + iz*r3x
      yc1 = yclat(i) + ix*r1y + iy*r2y - iz*r3y
      zc1 = zclat(i) + ix*r1z + iy*r2z + iz*r3z
    endif
    limatch1 = lmatch(ni,ntypi,nt1,ntyp1,.true.)
    limatch4 = lmatch(ni,ntypi,nt1,ntyp1,.true.)
!
!  Second site
!
    nj = nat(j)
    ntypj = nftype(j)
    ocj = occuf(j)
    if (loutofplane(n)) then
      xc2 = xclat(j) + ix*r1x + iy*r2x + iz*r3x
      yc2 = yclat(j) + ix*r1y + iy*r2y + iz*r3y
      zc2 = zclat(j) + ix*r1z + iy*r2z + iz*r3z
    else
      xc2 = xclat(j) 
      yc2 = yclat(j) 
      zc2 = zclat(j) 
    endif
    x21 = xc2 - xc1
    y21 = yc2 - yc1
    z21 = zc2 - zc1
    r212 = x21*x21 + y21*y21 + z21*z21
    r21 = sqrt(r212)
    ljmatch2 = lmatch(nj,ntypj,nt2,ntyp2,.true.)
    ljmatch3 = lmatch(nj,ntypj,nt3,ntyp3,.true.)
!
!  Third site
!
    nk = nat(k)
    ntypk = nftype(k)
    ock = occuf(k)
    xc3 = xclat(k) + jx*r1x + jy*r2x + jz*r3x
    yc3 = yclat(k) + jx*r1y + jy*r2y + jz*r3y
    zc3 = zclat(k) + jx*r1z + jy*r2z + jz*r3z
    x31 = xc3 - xc1
    y31 = yc3 - yc1
    z31 = zc3 - zc1
    r312 = x31*x31 + y31*y31 + z31*z31
    r31 = sqrt(r312)
    x32 = xc3 - xc2
    y32 = yc3 - yc2
    z32 = zc3 - zc2
    r322 = x32*x32 + y32*y32 + z32*z32
    r32 = sqrt(r322)
    lkmatch2 = lmatch(nk,ntypk,nt2,ntyp2,.true.)
    lkmatch3 = lmatch(nk,ntypk,nt3,ntyp3,.true.)
!
!  Fourth site
!
    nl = nat(l)
    ntypl = nftype(l)
    ocl = occuf(l)
    xc4 = xclat(l) + kx*r1x + ky*r2x + kz*r3x
    yc4 = yclat(l) + kx*r1y + ky*r2y + kz*r3y
    zc4 = zclat(l) + kx*r1z + ky*r2z + kz*r3z
    llmatch1 = lmatch(nl,ntypl,nt1,ntyp1,.true.)
    llmatch4 = lmatch(nl,ntypl,nt4,ntyp4,.true.)
!
!  Define remaining vectors between atoms
!
    x43 = xc4 - xc3
    y43 = yc4 - yc3
    z43 = zc4 - zc3
    r432 = x43*x43 + y43*y43 + z43*z43
    r43 = sqrt(r432)
    x42 = x32 + x43
    y42 = y32 + y43
    z42 = z32 + z43
    r422 = x42*x42 + y42*y42 + z42*z42
    r42 = sqrt(r422)
    x41 = x43 + x32 + x21
    y41 = y43 + y32 + y21
    z41 = z43 + z32 + z21
    r412 = x41*x41 + y41*y41 + z41*z41
    r41 = sqrt(r412)
!
    if (.not.loutofplane(n)) then
!
!  Decide whether potential order is 1-2-3-4 or 4-3-2-1
!
      if (.not.limatch1.or..not.ljmatch2.or..not.lkmatch3.or..not.llmatch4) then
!
!  Failed to match 1-2-3-4, so reverse order and check
!
        if (limatch4.and.ljmatch3.and.lkmatch2.and.llmatch1) then
!
!  Switch round order of torsional atoms
!
          ntmp = nt1
          nt1 = nt4
          nt4 = ntmp
          ntmp = ntyp1
          ntyp1 = ntyp4
          ntyp4 = ntmp
          ntmp = nt2
          nt2 = nt3
          nt3 = ntmp
          ntmp = ntyp2
          ntyp2 = ntyp3
          ntyp3 = ntmp
!
!  Switch round potential terms to match
!
          if (nfortype.eq.8.or.nfortype.eq.9) then
            rtmp = fpoly(3)
            fpoly(3) = fpoly(5)
            fpoly(5) = rtmp
          elseif (nfortype.eq.6.or.nfortype.eq.7) then
            rtmp = fpoly(2)
            fpoly(2) = fpoly(4)
            fpoly(4) = rtmp
          elseif (nfortype.eq.10.or.nfortype.eq.17) then
            rtmp = fpoly(2)
            fpoly(2) = fpoly(1)
            fpoly(1) = rtmp
          endif
        else
          call outerror('inconsistency between setlist4 and four3d3',0_i4)
          call stopnow('four3d3')
        endif
      endif
    endif
!**********************************************************
!  All four body terms including out of plane potentials  *
!**********************************************************
!
!  Weight terms by occupancy factors
!
    ofct = oci*ocj*ock*ocl
    rko = rkfor*ofct
    phi0o = phi0
    if (nfortype.eq.2) then
      do kk = 1,npha
        fpoly(kk) = forpoly(kk,n)*ofct
      enddo
    elseif (nfortype.eq.3) then
      fpoly(1) = rkfor4*ofct
    elseif (nfortype.eq.4.or.nfortype.eq.7.or.nfortype.eq.9) then
      phi0o = phi0*ofct
    endif
!***********************************************
!  Calculate four-body energy and derivatives  *
!***********************************************
    call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d,rko, &
      rn,phi0o,isgn,fpoly,.true.,.true.,.true.)
!********************************************************
!  Set pointers for two non-i/j atoms in fourbody term  *
!********************************************************
!
!  Are k and l already on the list?
!
    lkfound = .false.
    llfound = .false.
    ii = 0
    do while (ii.lt.nmanyk.and.(.not.lkfound.or..not.llfound))
      ii = ii + 1
      lkfound = (nptrmanyk(ii).eq.kin)
      llfound = (nptrmanyk(ii).eq.lin)
      if (lkfound) kd41 = ii
      if (llfound) kd42 = ii
    enddo
!
!  If k/l is not already on the list, initialise d33
!
    if (.not.llfound) then
      nmanyk = nmanyk + 1
      if (nmanyk.gt.maxmany) then
        maxmany = nmanyk + 40
        call changemaxmany
      endif
      nptrmanyk(nmanyk) = lin
      kd42 = nmanyk
      do kk = 1,54
        d33(kk,kd42)  = 0.0_dp
        d33r(kk,kd42) = 0.0_dp
        d33i(kk,kd42) = 0.0_dp
      enddo
      if (lstr) then
        do kk = 1,108
          d33s(kk,kd42)  = 0.0_dp
          d33rs(kk,kd42) = 0.0_dp
          d33is(kk,kd42) = 0.0_dp
        enddo
      endif
    endif
    if (.not.lkfound) then
      nmanyk = nmanyk + 1
      if (nmanyk.gt.maxmany) then
        maxmany = nmanyk + 40
        call changemaxmany
      endif
      nptrmanyk(nmanyk) = kin
      kd41 = nmanyk
      do kk = 1,54
        d33(kk,kd41)  = 0.0_dp
        d33r(kk,kd41) = 0.0_dp
        d33i(kk,kd41) = 0.0_dp
      enddo
      if (lstr) then
        do kk = 1,108
          d33s(kk,kd41)  = 0.0_dp
          d33rs(kk,kd41) = 0.0_dp
          d33is(kk,kd41) = 0.0_dp
        enddo
      endif
    endif
!
!  Set k-l third derivative pointers
!
    nforkl = nforkl + 1
    if (nforkl.gt.maxmany2) then
      maxmany2 = nforkl + 40
      call changemaxmany
    endif
    nptrfork(nforkl) = kin
    nptrforl(nforkl) = lin
!
!  Zero d34 elements
!
    do kk = 1,27
      d34(kk,nforkl) = 0.0_dp
      d34r(kk,nforkl) = 0.0_dp
      d34i(kk,nforkl) = 0.0_dp
    enddo
    if (lstr) then
      do kk = 1,54
        d34s(kk,nforkl) = 0.0_dp
        d34rs(kk,nforkl) = 0.0_dp
        d34is(kk,nforkl) = 0.0_dp
      enddo
    endif
!****************************
!  Initialise local arrays  *
!****************************
    ind = 0
    do k1 = 1,3
      do k2 = 1,3
        do k3 = 1,3
          ind = ind + 1
          d3l(k3,k2,k1) = 0.0_dp
          d34l(ind) = 0.0_dp
        enddo
      enddo
    enddo
    do ii = 1,54
      d33l1(ii) = 0.0_dp
      d33l2(ii) = 0.0_dp
    enddo
!**********************************************
!  Vector array between atoms to handle sign  *
!**********************************************
!
!  Atom 1
!
    vec(1,1,1) = -x21
    vec(2,1,1) = -y21
    vec(3,1,1) = -z21
    vec(1,2,1) = -x31
    vec(2,2,1) = -y31
    vec(3,2,1) = -z31
    vec(1,3,1) = -x41
    vec(2,3,1) = -y41
    vec(3,3,1) = -z41
!
!  Atom 2
!
    vec(1,1,2) = x21
    vec(2,1,2) = y21
    vec(3,1,2) = z21
    vec(1,2,2) = -x32
    vec(2,2,2) = -y32
    vec(3,2,2) = -z32
    vec(1,3,2) = -x42
    vec(2,3,2) = -y42
    vec(3,3,2) = -z42
!
!  Atom 3
!
    vec(1,1,3) = x31
    vec(2,1,3) = y31
    vec(3,1,3) = z31
    vec(1,2,3) = x32
    vec(2,2,3) = y32
    vec(3,2,3) = z32
    vec(1,3,3) = -x43
    vec(2,3,3) = -y43
    vec(3,3,3) = -z43
!
!  Atom 4
!
    vec(1,1,4) = x41
    vec(2,1,4) = y41
    vec(3,1,4) = z41
    vec(1,2,4) = x42
    vec(2,2,4) = y42
    vec(3,2,4) = z42
    vec(1,3,4) = x43
    vec(2,3,4) = y43
    vec(3,3,4) = z43
!********************************
!  Calculate third derivatives  *
!********************************
!
!  Find common vector, ijmap
!
    ijmap = 0
    jimap = 0
    ij = nvmap(imap,jmap)
    ii = 0
    do while (ijmap.eq.0.or.jimap.eq.0)
      ii = ii + 1
      if (n3vec(ii,jmap).eq.ij) ijmap = ii
      if (n3vec(ii,imap).eq.ij) jimap = ii
    enddo
!
!  Derivatives of i-j with respect to r(iin,jin)
!
!  General note : sign of contribution to d3/d33 is negative
!  if vec(*,*,imap) is involved to maintain directional
!  consistency for j->i vector.
!
!  Loops over i- and j- vectors : ijmap is common vector
!
    do j1 = 1,3
      do j2 = 1,3
        n3v1 = n3vec(ijmap,jmap)
        n3v2 = n3vec(j1,imap)
        n3v3 = n3vec(j2,jmap)
        id3 = kb3(n3v1,n3v2,n3v3)
!
!  Loops over Cartesian components
!
        do k1 = 1,3
          do k2 = 1,3
            do k3 = 1,3
!
!  General terms
!
              d3l(k3,k2,k1) = d3l(k3,k2,k1) - e3d(id3)*vec(k1,ijmap,jmap)*vec(k2,j2,jmap)*vec(k3,j1,imap)
!
!  Delta terms
!
              if (k2.eq.k3.and.n3v2.eq.n3v3) then
                d3l(k3,k2,k1) = d3l(k3,k2,k1) + e2d(kb2(n3v2,n3v1))*vec(k1,ijmap,jmap)
              endif
              if (k1.eq.k2.and.n3v1.eq.n3v3) then
                d3l(k3,k2,k1) = d3l(k3,k2,k1) - e2d(kb2(n3v2,n3v1))*vec(k3,j1,imap)
              endif
              if (k1.eq.k3.and.n3v1.eq.n3v2) then
                d3l(k3,k2,k1) = d3l(k3,k2,k1) + e2d(kb2(n3v3,n3v1))*vec(k2,j2,jmap)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
!
!  Find vectors for i->k and j->k
!
    ikmap = 0
    jkmap = 0
    ii = 0
    ik = nvmap(imap,kmap)
    jk = nvmap(jmap,kmap)
    do while (ikmap.eq.0.or.jkmap.eq.0)
      ii = ii + 1
      if (n3vec(ii,kmap).eq.ik) ikmap = ii
      if (n3vec(ii,kmap).eq.jk) jkmap = ii
    enddo
!
!  Derivatives of i-j with respect to r(iin,kin)
!
!  Loops over i- and j- vectors 
!
    do j1 = 1,3
      do j2 = 1,3
        n3v1 = n3vec(ikmap,kmap)
        n3v2 = n3vec(j1,imap)
        n3v3 = n3vec(j2,jmap)
        id3 = kb3(n3v1,n3v2,n3v3)
!
!  Loops over Cartesian components
!
        ind = 0
        do k1 = 1,3
          do k2 = 1,3
            do k3 = 1,3
              ind = ind + 1
!
!  General terms
!
              d33l1(ind) = d33l1(ind) - e3d(id3)*vec(k1,ikmap,kmap)*vec(k2,j2,jmap)*vec(k3,j1,imap)
!
!  Delta terms
!
              if (k2.eq.k3.and.n3v2.eq.n3v3) then
                d33l1(ind) = d33l1(ind) + e2d(kb2(n3v2,n3v1))*vec(k1,ikmap,kmap)
              endif
              if (k1.eq.k2.and.n3v1.eq.n3v3) then
                d33l1(ind) = d33l1(ind) - e2d(kb2(n3v2,n3v1))*vec(k3,j1,imap)
              endif
              if (k1.eq.k3.and.n3v1.eq.n3v2) then
                d33l1(ind) = d33l1(ind) + e2d(kb2(n3v3,n3v1))*vec(k2,j2,jmap)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
!
!  Derivatives of i-j with respect to r(jin,kin)
!
!  Loops over i- and j- vectors 
!
    do j1 = 1,3
      do j2 = 1,3
        n3v1 = n3vec(jkmap,kmap)
        n3v2 = n3vec(j1,imap)
        n3v3 = n3vec(j2,jmap)
        id3 = kb3(n3v1,n3v2,n3v3)
!
!  Loops over Cartesian components
!
        ind = 27
        do k1 = 1,3
          do k2 = 1,3
            do k3 = 1,3
              ind = ind + 1
!
!  General terms
!
              d33l1(ind) = d33l1(ind) - e3d(id3)*vec(k1,jkmap,kmap)*vec(k2,j2,jmap)*vec(k3,j1,imap)
!
!  Delta terms
!
              if (k2.eq.k3.and.n3v2.eq.n3v3) then
                d33l1(ind) = d33l1(ind) + e2d(kb2(n3v2,n3v1))*vec(k1,jkmap,kmap)
              endif
              if (k1.eq.k2.and.n3v1.eq.n3v3) then
                d33l1(ind) = d33l1(ind) + e2d(kb2(n3v2,n3v1))*vec(k3,j1,imap)
              endif
              if (k1.eq.k3.and.n3v1.eq.n3v2) then
                d33l1(ind) = d33l1(ind) - e2d(kb2(n3v3,n3v1))*vec(k2,j2,jmap)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
!
!  Find vectors for i->l and j->l
!
    ilmap = 0
    jlmap = 0
    il = nvmap(imap,lmap)
    jl = nvmap(jmap,lmap)
    ii = 0
    do while (ilmap.eq.0.or.jlmap.eq.0)
      ii = ii + 1
      if (n3vec(ii,lmap).eq.il) ilmap = ii
      if (n3vec(ii,lmap).eq.jl) jlmap = ii
    enddo
!
!  Derivatives of i-j with respect to r(iin,lin)
!
!  Loops over i- and j- vectors 
!
    do j1 = 1,3
      do j2 = 1,3
        n3v1 = n3vec(ilmap,lmap)
        n3v2 = n3vec(j1,imap)
        n3v3 = n3vec(j2,jmap)
        id3 = kb3(n3v1,n3v2,n3v3)
!
!  Loops over Cartesian components
!
        ind = 0
        do k1 = 1,3
          do k2 = 1,3
            do k3 = 1,3
              ind = ind + 1
!
!  General terms
!
              d33l2(ind) = d33l2(ind) - e3d(id3)*vec(k1,ilmap,lmap)*vec(k2,j2,jmap)*vec(k3,j1,imap)
!
!  Delta terms
!
              if (k2.eq.k3.and.n3v2.eq.n3v3) then
                d33l2(ind) = d33l2(ind) + e2d(kb2(n3v2,n3v1))*vec(k1,ilmap,lmap)
              endif
              if (k1.eq.k2.and.n3v1.eq.n3v3) then
                d33l2(ind) = d33l2(ind) - e2d(kb2(n3v2,n3v1))*vec(k3,j1,imap)
              endif
              if (k1.eq.k3.and.n3v1.eq.n3v2) then
                d33l2(ind) = d33l2(ind) + e2d(kb2(n3v3,n3v1))*vec(k2,j2,jmap)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
!
!  Derivatives of i-j with respect to r(jin,lin)
!
!  Loops over i- and j- vectors 
!
    do j1 = 1,3
      do j2 = 1,3
        n3v1 = n3vec(jlmap,lmap)
        n3v2 = n3vec(j1,imap)
        n3v3 = n3vec(j2,jmap)
        id3 = kb3(n3v1,n3v2,n3v3)
!
!  Loops over Cartesian components
!
        ind = 27
        do k1 = 1,3
          do k2 = 1,3
            do k3 = 1,3
              ind = ind + 1
!
!  General terms
!
              d33l2(ind) = d33l2(ind) - e3d(id3)*vec(k1,jlmap,lmap)*vec(k2,j2,jmap)*vec(k3,j1,imap)
!
!  Delta terms
!
              if (k2.eq.k3.and.n3v2.eq.n3v3) then
                d33l2(ind) = d33l2(ind) + e2d(kb2(n3v2,n3v1))*vec(k1,jlmap,lmap)
              endif
              if (k1.eq.k2.and.n3v1.eq.n3v3) then
                d33l2(ind) = d33l2(ind) + e2d(kb2(n3v2,n3v1))*vec(k3,j1,imap)
              endif
              if (k1.eq.k3.and.n3v1.eq.n3v2) then
                d33l2(ind) = d33l2(ind) - e2d(kb2(n3v3,n3v1))*vec(k2,j2,jmap)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
!
!  Find vector for k->l
!
    klmap = 0
    kl = nvmap(kmap,lmap)
    ii = 0
    do while (klmap.eq.0)
      ii = ii + 1
      if (n3vec(ii,lmap).eq.kl) klmap = ii
    enddo
!
!  Derivatives of i-j with respect to r(kin,lin)
!
!  Loops over i- and j- vectors
!
    do j1 = 1,3
      do j2 = 1,3
        n3v1 = n3vec(klmap,lmap)
        n3v2 = n3vec(j1,imap)
        n3v3 = n3vec(j2,jmap)
        id3 = kb3(n3v1,n3v2,n3v3)
!
!  Loops over Cartesian components
!
        ind = 0
        do k1 = 1,3
          do k2 = 1,3
            do k3 = 1,3
              ind = ind + 1
!
!  General terms
!
              d34l(ind) = d34l(ind) - e3d(id3)*vec(k1,klmap,lmap)*vec(k2,j2,jmap)*vec(k3,j1,imap)
!
!  Delta terms
!
              if (k2.eq.k3.and.n3v2.eq.n3v3) then
                d34l(ind) = d34l(ind) + e2d(kb2(n3v2,n3v1))*vec(k1,klmap,lmap)
              endif
              if (k1.eq.k2.and.n3v1.eq.n3v3) then
                d34l(ind) = d34l(ind) - e2d(kb2(n3v2,n3v1))*vec(k3,j1,imap)
              endif
              if (k1.eq.k3.and.n3v1.eq.n3v2) then
                d34l(ind) = d34l(ind) + e2d(kb2(n3v3,n3v1))*vec(k2,j2,jmap)
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
!****************************
!  Calculate phase factors  *
!****************************
    cos12 = xkv*vec(1,ijmap,jmap) + ykv*vec(2,ijmap,jmap) + zkv*vec(3,ijmap,jmap)
    sin12 = sin(cos12)
    cos12 = cos(cos12)
!*********************************************
!  Phase derivatives and add to main arrays  *
!*********************************************
    ind = 0
    do k1 = 1,3
      do k2 = 1,3
        do k3 = 1,3
          ind = ind + 1
          d3(k3,k2,k1) = d3(k3,k2,k1) + d3l(k3,k2,k1)
          d3r(k3,k2,k1) = d3r(k3,k2,k1) + d3l(k3,k2,k1)*cos12
          d3i(k3,k2,k1) = d3i(k3,k2,k1) + d3l(k3,k2,k1)*sin12
          d34(ind,nforkl) = d34(ind,nforkl) + d34l(ind)
          d34r(ind,nforkl) = d34r(ind,nforkl) + d34l(ind)*cos12
          d34i(ind,nforkl) = d34i(ind,nforkl) + d34l(ind)*sin12
        enddo
      enddo
    enddo
    do ii = 1,54
      d33(ii,kd41) = d33(ii,kd41) + d33l1(ii)
      d33r(ii,kd41) = d33r(ii,kd41) + d33l1(ii)*cos12
      d33i(ii,kd41) = d33i(ii,kd41) + d33l1(ii)*sin12
      d33(ii,kd42) = d33(ii,kd42) + d33l2(ii)
      d33r(ii,kd42) = d33r(ii,kd42) + d33l2(ii)*cos12
      d33i(ii,kd42) = d33i(ii,kd42) + d33l2(ii)*sin12
    enddo
!***********************
!  Strain derivatives  *
!***********************
    if (lstr) then
!
!  i-j strain
!
      do k2 = 1,3
        do k3 = 1,3
          d3sl(k3,k2,1) = d3l(k3,k2,1)*vec(1,ijmap,jmap)
          d3sl(k3,k2,2) = d3l(k3,k2,2)*vec(2,ijmap,jmap)
          d3sl(k3,k2,3) = d3l(k3,k2,3)*vec(3,ijmap,jmap)
          d3sl(k3,k2,4) = d3l(k3,k2,2)*vec(3,ijmap,jmap)
          d3sl(k3,k2,5) = d3l(k3,k2,1)*vec(3,ijmap,jmap)
          d3sl(k3,k2,6) = d3l(k3,k2,2)*vec(1,ijmap,jmap)
        enddo
      enddo
      do k1 = 1,6
        do k2 = 1,3
          do k3 = 1,3
            d3s(k3,k2,k1) = d3s(k3,k2,k1) + d3sl(k3,k2,k1)
            d3rs(k3,k2,k1) = d3rs(k3,k2,k1) + d3sl(k3,k2,k1)*cos12
            d3is(k3,k2,k1) = d3is(k3,k2,k1) + d3sl(k3,k2,k1)*sin12
          enddo
        enddo
      enddo
!
!  i-k strain
!
      ind = 0
      do k2 = 1,3
        do k3 = 1,3
          ind = ind + 1
          d3sl(k3,k2,1) = d33l1(ind)*vec(1,ikmap,kmap)
          d3sl(k3,k2,2) = d33l1(ind+9)*vec(2,ikmap,kmap)
          d3sl(k3,k2,3) = d33l1(ind+18)*vec(3,ikmap,kmap)
          d3sl(k3,k2,4) = d33l1(ind+9)*vec(3,ikmap,kmap)
          d3sl(k3,k2,5) = d33l1(ind)*vec(3,ikmap,kmap)
          d3sl(k3,k2,6) = d33l1(ind+9)*vec(1,ikmap,kmap)
        enddo
      enddo
      ind = 0
      do k1 = 1,6
        do k2 = 1,3
          do k3 = 1,3
            ind = ind + 1
            d33s(ind,kd41) = d33s(ind,kd41) + d3sl(k3,k2,k1)
            d33rs(ind,kd41) = d33rs(ind,kd41) + d3sl(k3,k2,k1)*cos12
            d33is(ind,kd41) = d33is(ind,kd41) + d3sl(k3,k2,k1)*sin12
          enddo
        enddo
      enddo
!
!  j-k strain
!
      ind = 27
      do k2 = 1,3
        do k3 = 1,3
          ind = ind + 1
          d3sl(k3,k2,1) = d33l1(ind)*vec(1,jkmap,kmap)
          d3sl(k3,k2,2) = d33l1(ind+9)*vec(2,jkmap,kmap)
          d3sl(k3,k2,3) = d33l1(ind+18)*vec(3,jkmap,kmap)
          d3sl(k3,k2,4) = d33l1(ind+9)*vec(3,jkmap,kmap)
          d3sl(k3,k2,5) = d33l1(ind)*vec(3,jkmap,kmap)
          d3sl(k3,k2,6) = d33l1(ind+9)*vec(1,jkmap,kmap)
        enddo
      enddo
      ind = 54
      do k1 = 1,6
        do k2 = 1,3
          do k3 = 1,3
            ind = ind + 1
            d33s(ind,kd41) = d33s(ind,kd41) + d3sl(k3,k2,k1)
            d33rs(ind,kd41) = d33rs(ind,kd41) + d3sl(k3,k2,k1)*cos12
            d33is(ind,kd41) = d33is(ind,kd41) + d3sl(k3,k2,k1)*sin12
          enddo
        enddo
      enddo
!
!  i-l strain
!
      ind = 0
      do k2 = 1,3
        do k3 = 1,3
          ind = ind + 1
          d3sl(k3,k2,1) = d33l2(ind)*vec(1,ilmap,lmap)
          d3sl(k3,k2,2) = d33l2(ind+9)*vec(2,ilmap,lmap)
          d3sl(k3,k2,3) = d33l2(ind+18)*vec(3,ilmap,lmap)
          d3sl(k3,k2,4) = d33l2(ind+9)*vec(3,ilmap,lmap)
          d3sl(k3,k2,5) = d33l2(ind)*vec(3,ilmap,lmap)
          d3sl(k3,k2,6) = d33l2(ind+9)*vec(1,ilmap,lmap)
        enddo
      enddo
      ind = 0
      do k1 = 1,6
        do k2 = 1,3
          do k3 = 1,3
            ind = ind + 1
            d33s(ind,kd42) = d33s(ind,kd42) + d3sl(k3,k2,k1)
            d33rs(ind,kd42) = d33rs(ind,kd42) + d3sl(k3,k2,k1)*cos12
            d33is(ind,kd42) = d33is(ind,kd42) + d3sl(k3,k2,k1)*sin12
          enddo
        enddo
      enddo
!
!  j-l strain
!
      ind = 27
      do k2 = 1,3
        do k3 = 1,3
          ind = ind + 1
          d3sl(k3,k2,1) = d33l2(ind)*vec(1,jlmap,lmap)
          d3sl(k3,k2,2) = d33l2(ind+9)*vec(2,jlmap,lmap)
          d3sl(k3,k2,3) = d33l2(ind+18)*vec(3,jlmap,lmap)
          d3sl(k3,k2,4) = d33l2(ind+9)*vec(3,jlmap,lmap)
          d3sl(k3,k2,5) = d33l2(ind)*vec(3,jlmap,lmap)
          d3sl(k3,k2,6) = d33l2(ind+9)*vec(1,jlmap,lmap)
        enddo
      enddo
      ind = 54
      do k1 = 1,6
        do k2 = 1,3
          do k3 = 1,3
            ind = ind + 1
            d33s(ind,kd42) = d33s(ind,kd42) + d3sl(k3,k2,k1)
            d33rs(ind,kd42) = d33rs(ind,kd42) + d3sl(k3,k2,k1)*cos12
            d33is(ind,kd42) = d33is(ind,kd42) + d3sl(k3,k2,k1)*sin12
          enddo
        enddo
      enddo
!
!  k-l strain
!
      ind = 0
      do k2 = 1,3
        do k3 = 1,3
          ind = ind + 1
          d3sl(k3,k2,1) = d34l(ind)*vec(1,klmap,lmap)
          d3sl(k3,k2,2) = d34l(ind+9)*vec(2,klmap,lmap)
          d3sl(k3,k2,3) = d34l(ind+18)*vec(3,klmap,lmap)
          d3sl(k3,k2,4) = d34l(ind+9)*vec(3,klmap,lmap)
          d3sl(k3,k2,5) = d34l(ind)*vec(3,klmap,lmap)
          d3sl(k3,k2,6) = d34l(ind+9)*vec(1,klmap,lmap)
        enddo
      enddo
      ind = 0
      do k1 = 1,6
        do k2 = 1,3
          do k3 = 1,3
            ind = ind + 1
            d34s(ind,nforkl) = d34s(ind,nforkl) + d3sl(k3,k2,k1)
            d34rs(ind,nforkl) = d34rs(ind,nforkl) + d3sl(k3,k2,k1)*cos12
            d34is(ind,nforkl) = d34is(ind,nforkl) + d3sl(k3,k2,k1)*sin12
          enddo
        enddo
      enddo
    endif
!***********************
!  End of derivatives  *
!***********************
10 continue
!
  return
  end
