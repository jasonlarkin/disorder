  subroutine fourlist(efor,eoop,lgrad1)
!
!  Subroutine for four-body energy using list method
!
!   2/95 Phi0 offset added for standard torsion potential
!   4/97 Modified to handle out of plane terms
!   7/98 Modified to use call to fourbody and labelling
!        of vectors changed to new convention
!   7/98 List storage moved out of secondc
!   8/98 Out of plane terms now calculated in fourbody.f
!   3/99 Parallel modifications added
!   7/02 K4 added for outofplane potential
!  10/02 Torharm potential added
!  11/02 Wildcard atom types added
!  11/02 Parallel changes made
!   4/04 Exponentially decaying torsion added
!   4/04 Tapered torsion added
!   6/04 Sign of virial corrected
!  11/04 Torangle potential added
!   6/06 Inversion squared potential added
!   9/06 Order of atom search changed to allow for Dreiding
!   9/06 Dreiding scheme for force constant added as an option
!   1/07 Wildcard handling in lmatch calls corrected
!   1/07 UFF4 added
!  10/07 Angle-angle cross potential added
!  12/07 Unused variables removed
!   1/08 Setting of rkfor4 made specific to nfortype = 3
!   5/08 UFFoop potential added
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!  11/08 Type checking algorithm changed
!  11/08 Corrections for potential dependent swapping of terms according to atom 
!        assignments added.
!  11/08 Option to output energy terms added
!   3/09 rkfor4 now set for nfortype = 3
!   6/09 Site energy and virials added
!   2/10 Handling of i-j vector across cell boundary for out of
!        plane term corrected
!   5/10 Typo in limatch4 setting corrected. 
!   6/10 rkfor4 set for nfortype = 12
!  10/11 Strain derivatives added
!  11/11 Region-region energy contributions stored
!  11/11 Out of plane site energy divided on a per bond contribution basis
!   2/12 Site energy corrected as the centre atom was not weighted properly
!   4/12 Explicit virial calculation removed as no longer needed
!   4/12 xvir, yvir and zvir removed
!   5/12 Atomic stresses added
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
  use energies,       only : siteenergy, eregion2region
  use four
  use iochannels,     only : ioout
  use parallel
  use symmetry,       only : lstr
  use times
  implicit none
!
!  Passed variables
!
  real(dp),    intent(inout)                   :: efor
  real(dp),    intent(inout)                   :: eoop
  logical,     intent(in)                      :: lgrad1
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: isgn
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: kx
  integer(i4)                                  :: ky
  integer(i4)                                  :: kz
  integer(i4)                                  :: l
  integer(i4)                                  :: mm
  integer(i4)                                  :: n
  integer(i4)                                  :: nfortype
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl
  integer(i4)                                  :: npha
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregionl
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
  logical                                      :: limatch1
  logical                                      :: limatch4
  logical                                      :: ljmatch2
  logical                                      :: ljmatch3
  logical                                      :: lkmatch2
  logical                                      :: lkmatch3
  logical                                      :: llmatch1
  logical                                      :: llmatch4
  logical                                      :: lmatch
  logical                                      :: lsg1
  real(dp)                                     :: cputime
  real(dp)                                     :: e1d(6)
  real(dp)                                     :: e2d(21)
  real(dp)                                     :: e3d(56)
  real(dp)                                     :: eterm
  real(dp)                                     :: eterm4th
  real(dp)                                     :: eterm6th
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
  real(dp)                                     :: rprod(6,6)
  real(dp)                                     :: rn
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: rtmp
  real(dp)                                     :: time1
  real(dp)                                     :: time2
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
!
  time1 = cputime()
!
!  Openning banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Four : Atom No. 1  Atom No. 2  Atom No. 3  Atom No. 4  Torsion/OOP energy (eV)'')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!
!  Initialisation
!
  efor = 0.0_dp
  eoop = 0.0_dp
  lsg1 = (lstr.and.lgrad1)
!*****************************
!  Loop over four-body list  *
!*****************************
  do mm = procid+1,nlist4md,nprocs
    n = nforptr(mm)
    ind = ilind(mm)
    l = ind/(numat+1)
    i = ind - l*(numat+1)
    ind = jkind(mm)
    k = ind/(numat+1)
    j = ind - k*(numat+1)
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
    elseif (nfortype.eq.3.or.nfortype.eq.12) then
      rkfor4 = forpoly(1,n)
    elseif (nfortype.eq.4.or.nfortype.eq.6.or.nfortype.eq.7) then
      npha = npfor(n)
      if (npha.gt.0) then
        isgn = 1
      else
        isgn = - 1
      endif
      npha = abs(npha)
      if (nfortype.eq.6) then
        phi0 = forpoly(1,n)*degtorad
      else
        phi0 = forpoly(1,n)
      endif
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
    elseif (nfortype.eq.5) then
      phi0 = forpoly(1,n)*degtorad
    elseif (nfortype.eq.10.or.nfortype.eq.17) then
      fpoly(1) = forpoly(1,n)*degtorad
      fpoly(2) = forpoly(2,n)*degtorad
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
    nregioni = nregionno(nsft+i)
    oci = occuf(i)
    if (loutofplane(n)) then
      xc1 = xclat(i)
      yc1 = yclat(i)
      zc1 = zclat(i)
    else
      xc1 = xclat(i) + ix*r1x + iy*r2x + iz*r3x
      yc1 = yclat(i) + ix*r1y + iy*r2y + iz*r3y
      zc1 = zclat(i) + ix*r1z + iy*r2z + iz*r3z
    endif
    limatch1 = lmatch(ni,ntypi,nt1,ntyp1,.true.)
    limatch4 = lmatch(ni,ntypi,nt4,ntyp4,.true.)
!
!  Second site
!
    nj = nat(j)
    ntypj = nftype(j)
    nregionj = nregionno(nsft+j)
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
    nregionk = nregionno(nsft+k)
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
    nregionl = nregionno(nsft+l)
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
          call outerror('inconsistency between setlist4 and fourlist',0_i4)
          call stopnow('fourlist')
        endif
      endif
    endif
!***********************************************
!  Calculate four-body energy and derivatives  *
!***********************************************
    if (.not.loutofplane(n)) then
!********************************
!  Conventional four body term  *
!********************************
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
      elseif (nfortype.eq.4.or.nfortype.eq.7.or.nfortype.eq.9) then
        phi0o = phi0*ofct
      endif
      call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d,rko,rn, &
        phi0o,isgn,fpoly,lgrad1,.false.,.false.)
      efor = efor + eterm
!
      eterm6th = eterm/6.0_dp
      eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eterm6th
      eregion2region(nregionk,nregioni) = eregion2region(nregionk,nregioni) + eterm6th
      eregion2region(nregionl,nregioni) = eregion2region(nregionl,nregioni) + eterm6th
      eregion2region(nregionk,nregionj) = eregion2region(nregionk,nregionj) + eterm6th
      eregion2region(nregionl,nregionj) = eregion2region(nregionl,nregionj) + eterm6th
      eregion2region(nregionl,nregionk) = eregion2region(nregionl,nregionk) + eterm6th
!
      eterm4th = 0.25_dp*eterm
      siteenergy(i) = siteenergy(i) + eterm4th
      siteenergy(j) = siteenergy(j) + eterm4th
      siteenergy(k) = siteenergy(k) + eterm4th
      siteenergy(l) = siteenergy(l) + eterm4th
!
!  Output energy contribution
!
      if (lPrintFour) then
        write(ioout,'(4x,4i12,1x,f22.10)') i,j,k,l,eterm
      endif
    else
!***************************
!  Out of plane potential  *
!***************************
!
!  Weight terms by occupancy factors
!
      ofct = oci*ocj*ock*ocl
      rko = rkfor*ofct
      if (nfortype.eq.3.or.nfortype.eq.12) then
        fpoly(1) = rkfor4*ofct
      endif
      call fourbody(n,nfortype,r21,r31,r41,r32,r42,r43,eterm,e1d,e2d,e3d,rko, &
        rn,phi0,isgn,fpoly,lgrad1,.false.,.false.)
      eoop = eoop + eterm
!
      eterm6th = eterm/3.0_dp
      eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eterm6th
      eregion2region(nregionk,nregioni) = eregion2region(nregionk,nregioni) + eterm6th
      eregion2region(nregionl,nregioni) = eregion2region(nregionl,nregioni) + eterm6th
!
      siteenergy(i) = siteenergy(i) + 1.5_dp*eterm6th
      siteenergy(j) = siteenergy(j) + 0.5_dp*eterm6th
      siteenergy(k) = siteenergy(k) + 0.5_dp*eterm6th
      siteenergy(l) = siteenergy(l) + 0.5_dp*eterm6th
!
!  Output energy contribution
!
      if (lPrintFour) then
        write(ioout,'(4x,4i12,1x,f22.10)') i,j,k,l,eterm
      endif
    endif
!***********************
!  Strain derivatives  *
!***********************
    if (lsg1) then
!
!  Set up strain products
!
      call fourstrterms(ndim,rprod,x21,y21,z21,x31,y31,z31,x41,y41,z41, &
                        x32,y32,z32,x42,y42,z42,x43,y43,z43)
!
!  Strain derivatives
!
      rstrdloc(1:nstrains) = 0.0_dp
      do kl = 1,nstrains
        rstrdloc(kl) = rstrdloc(kl) + e1d(1)*rprod(kl,1)
        rstrdloc(kl) = rstrdloc(kl) + e1d(2)*rprod(kl,2)
        rstrdloc(kl) = rstrdloc(kl) + e1d(3)*rprod(kl,3)
        rstrdloc(kl) = rstrdloc(kl) + e1d(4)*rprod(kl,4)
        rstrdloc(kl) = rstrdloc(kl) + e1d(5)*rprod(kl,5)
        rstrdloc(kl) = rstrdloc(kl) + e1d(6)*rprod(kl,6)
        rstrd(kl) = rstrd(kl) + rstrdloc(kl)
      enddo
      if (latomicstress) then
        do kl = 1,nstrains
          atomicstress(kl,i) = atomicstress(kl,i) + 0.25_dp*rstrdloc(kl)
          atomicstress(kl,j) = atomicstress(kl,j) + 0.25_dp*rstrdloc(kl)
          atomicstress(kl,k) = atomicstress(kl,k) + 0.25_dp*rstrdloc(kl)
          atomicstress(kl,l) = atomicstress(kl,l) + 0.25_dp*rstrdloc(kl)
        enddo
      endif
    endif
!*************************
!  Internal derivatives  *
!*************************
    if (lgrad1) then
      xdrv(i) = xdrv(i) - x21*e1d(1) - x31*e1d(2) - x41*e1d(3)
      ydrv(i) = ydrv(i) - y21*e1d(1) - y31*e1d(2) - y41*e1d(3)
      zdrv(i) = zdrv(i) - z21*e1d(1) - z31*e1d(2) - z41*e1d(3)
      xdrv(j) = xdrv(j) - x32*e1d(4) + x21*e1d(1) - x42*e1d(5)
      ydrv(j) = ydrv(j) - y32*e1d(4) + y21*e1d(1) - y42*e1d(5)
      zdrv(j) = zdrv(j) - z32*e1d(4) + z21*e1d(1) - z42*e1d(5)
      xdrv(k) = xdrv(k) + x32*e1d(4) - x43*e1d(6) + x31*e1d(2)
      ydrv(k) = ydrv(k) + y32*e1d(4) - y43*e1d(6) + y31*e1d(2)
      zdrv(k) = zdrv(k) + z32*e1d(4) - z43*e1d(6) + z31*e1d(2)
      xdrv(l) = xdrv(l) + x43*e1d(6) + x42*e1d(5) + x41*e1d(3)
      ydrv(l) = ydrv(l) + y43*e1d(6) + y42*e1d(5) + y41*e1d(3)
      zdrv(l) = zdrv(l) + z43*e1d(6) + z42*e1d(5) + z41*e1d(3)
    endif
  enddo
!
!  Closing banner for energy decomposition
!
  if (lPrintFour) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  Timing
!
  time2 = cputime()
  tfour = tfour + time2 - time1
!
  return
  end
