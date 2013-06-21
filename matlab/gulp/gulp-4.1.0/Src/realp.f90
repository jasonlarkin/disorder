  subroutine realp(xkv,ykv,zkv)
!
!  Routine for calculating the phased second derivatives for
!  use in a phonon calculation.
!
!   3/95 nptrmol replaced by logical vector lptrmol
!   3/95 changes for periodic molecules added to lbonded,l2bond
!   8/95 Ewald sum for dispersion terms added
!   2/97 BSM exponential potential added
!   1/98 QEq modifications added
!   2/98 Derv2/dervi storage switched to normal triangle and
!        call to d2chargep added for EEM/QEq
!   4/98 ESFF form of Lennard-Jones now allowed for
!   8/98 Free energy minimisation modifications added - exclude
!        gamma point terms from on diagonal blocks.
!   1/99 1-4 interaction scaling added
!   7/00 Search for valid vectors and selfterms placed in subroutines
!   2/01 Structure of rpd changed to suit 2-D case
!   3/01 Symmetrisation of each 3 x 3 block removed during pairwise
!        sum as this is wrong.
!   4/01 Passed argument changed from K point number to K point 
!        coordinate
!   9/01 lmolq calculations accelerated using lneedmol 
!   2/02 lneedmol algorithm corrected
!  10/02 ReaxFF modifications added
!  11/02 K vector now passed to d2chargep
!  11/02 Wildcard atoms added
!   1/03 Wolf modifications made
!   1/03 Call to selfterm altered
!   9/04 Call to selfterm changed to allow for charge first derivatives
!   9/04 Modifications for variable charge second derivatives added
!   9/04 Variable charge contribution to the second derivatives moved to
!        after the nor = 0 skip point since there is a contribution from
!        the self term
!   9/04 Call to d2chargep altered
!   9/04 d0i/d0j now assumed to have been initialised in twobody
!   1/05 rp no longer sqrt'd and passed to rsearch routines
!   4/05 Mods for cosh-spring added
!   7/05 Streitz and Mintmire modifications added
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 Dimensions of sctrm1/2 corrected to maxmeamcomponent
!   3/09 lorder12 added to twobody argument list
!   6/09 Modified for charge as a coordinate option
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use configurations, only : lbsmat
  use constants
  use control
  use current
  use datatypes
  use derivatives
  use eam,            only : maxmeamcomponent
  use element
  use general,        only : cutw
  use kspace
  use molecule
  use parallel
  use realvectors
  use shell
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  real(dp),    intent(in)                      :: xkv
  real(dp),    intent(in)                      :: ykv
  real(dp),    intent(in)                      :: zkv
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: iis
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indri
  integer(i4)                                  :: indrj
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: j
  integer(i4)                                  :: jjs
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: k
  integer(i4)                                  :: maxlim
  integer(i4)                                  :: mint
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lmatch
  logical                                      :: lc6loc
  logical                                      :: lcspair
  logical                                      :: lewaldtype
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lself
  real(dp)                                     :: c6tot
  real(dp)                                     :: cosk
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2e
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: cut2w
  real(dp)                                     :: d1ix
  real(dp)                                     :: d1iy
  real(dp)                                     :: d1iz
  real(dp)                                     :: d1jx
  real(dp)                                     :: d1jy
  real(dp)                                     :: d1jz
  real(dp)                                     :: d2k
  real(dp)                                     :: d2ks
  real(dp)                                     :: d2self
  real(dp)                                     :: derive0self
  real(dp)                                     :: derive0selfi
  real(dp)                                     :: derive0selfj
  real(dp)                                     :: dk
  real(dp)                                     :: dks
  real(dp)                                     :: eatom
  real(dp)                                     :: ec6
  real(dp)                                     :: eqeq  
  real(dp)                                     :: ereal
  real(dp)                                     :: erecip
  real(dp)                                     :: factor
  real(dp)                                     :: fct
  real(dp)                                     :: hfactor
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: oneij
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: ritrm
  real(dp)                                     :: rp
  real(dp)                                     :: rqeq2
  real(dp)                                     :: rrtrm
  real(dp)                                     :: rtrm1
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: sink
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  time1 = cputime()
!
!  Local variables
!
  lc6loc = (lc6.and.ndim.eq.3)
  rqeq2 = rqeq*rqeq
  mint = 3*numat
  maxlim = mint
  if (nbsmat.gt.0) maxlim = maxlim + numat
  eatom = 0.0_dp
  ereal = 0.0_dp
  erecip = 0.0_dp
  ec6 = 0.0_dp
  eqeq = 0.0_dp
!
!  Set up cutoffs
!
  cut2p = cutp*cutp
  cut2e = rmx2
  cut2s = cuts*cuts
  cut2w = cutw*cutw
  if (lwolf) then
    cut2q = cut2w
  else
    cut2q = cut2e
  endif
!
!  Set the Coulomb term type based on dimensionality :
!
!  1-D         => 1/r
!  2-D and 3-D => erfc(seta*r)/r
!
  lewaldtype = (ndim.ne.1.or.lwolf)
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realp','npotl')
!
  if (.not.lnoreal) then
!***************************************************************
!  Atomistic and real space electrostatic second derivatives   *
!***************************************************************
!
!  Outer loop over sites
!
    do i = 1,numat
!
!  Inner loop over second site
!
      xal = xclat(i)
      yal = yclat(i)
      zal = zclat(i)
      nati = nat(i)
      ntypi = nftype(i)
      qli = qf(i)
      oci = occuf(i)
      if (lbsmat(nsft+nrelat(i))) then
        radi = radf(i)
        indri = 3*numat + i
      else
        radi = 0.0_dp
      endif
      indi = 3*(i - 1)
      ix = indi + 1
      iy = indi + 2
      iz = indi + 3
!
!  Molecule handling
!
      if (lmol) then
        nmi = natmol(i)
        indm = nmolind(i)
        call mindtoijk(indm,ixi,iyi,izi)
      endif
!
!  Start of second atom loop
!
      do j = 1,i
        natj = nat(j)
        ntypj = nftype(j)
!
!  Arrange nat1 to be lower than nat2. If they are equal
!  then arrange that ntyp1 is lower than ntyp2
!
        if (nati.eq.natj) then
          nat1 = nati
          nat2 = natj
          if (ntypi.lt.ntypj) then
            lorder12 = .true.
            ntyp1 = ntypi
            ntyp2 = ntypj
          else
            lorder12 = .false.
            ntyp1 = ntypj
            ntyp2 = ntypi
          endif
        elseif (nati.lt.natj) then
          lorder12 = .true.
          nat1 = nati
          nat2 = nat(j)
          ntyp1 = ntypi
          ntyp2 = nftype(j)
        else
          lorder12 = .false.
          nat1 = nat(j)
          nat2 = nati
          ntyp1 = nftype(j)
          ntyp2 = ntypi
        endif
        xcrd = xclat(j) - xal
        ycrd = yclat(j) - yal
        zcrd = zclat(j) - zal
        qlj = qf(j)
        ocj = occuf(j)
        if (lbsmat(nsft+nrelat(j))) then
          radj = radf(j)
          indrj = 3*numat + j
        else
          radj = 0.0_dp
        endif
        radsum = radi + radj
        ofct = oci*ocj
        indj = 3*(j - 1)
        jx = indj + 1
        jy = indj + 2
        jz = indj + 3
        fct = ofct*angstoev
        factor = qli*qlj*fct
!
!  Possible core-shell flag
!
        lcspair = (abs(nat1-nat2).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
!
!  Molecule handling
!
        if (lmol) then
          nmj = natmol(j)
          indmj = nmolind(j)
          call mindtoijk(indmj,ixj,iyj,izj)
          ixj = ixj - ixi
          iyj = iyj - iyi
          izj = izj - izi
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
        else
          lmolok = .false.
        endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
        rp = 0.0_dp
        npots = 0
        c6tot = 0.0_dp
        lneedmol = (lmol.and..not.lmolq)
        do n = 1,npote
          if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
            if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
              npots = npots + 1
              npotl(npots) = n
              if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol = .true.
              if (nptype(n).eq.8.or.nptype(n).eq.33) then
                if (cuts.gt.rp) rp = cuts
              elseif (lc6loc) then
                if (nptype(n).eq.1.or.nptype(n).eq.7) then
                  c6tot = c6tot + twopot(3,n)
                  if (repcut(n).gt.rp) rp = repcut(n)
                elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
                  c6tot = c6tot + twopot(2,n)
                  if (repcut(n).gt.rp) rp = repcut(n)
                else
                  if (rpot(n).gt.rp) rp = rpot(n)
                endif
              else
                if (rpot(n).gt.rp) rp = rpot(n)
              endif
            endif
          endif
        enddo
!
!  If no valid potentials and charge product is zero
!  then no need to search for distances
!
        if (npots.eq.0.and.abs(factor).lt.1.0d-8) goto 1120
        cut2r = rp*rp
        if (cut2r.gt.cut2p) cut2r = cut2p
        cut2 = cut2r
        if (cut2e.gt.cut2.and.lewald) cut2 = cut2e
        if (cut2w.gt.cut2.and.lwolf) cut2 = cut2w
        if (lqeq.or.lSandM) cut2 = max(cut2,rqeq2)
!  
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
        if (.not.lneedmol) lmolok = .false.
!
        d2self = 0.0_dp
!***********************
!  Find valid vectors  *
!***********************
        if (ndim.eq.3) then
          call rsearch3D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.2) then
          call rsearch2D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.1) then
          call rsearch1D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        endif
!
        derive0self = 0.0_dp
        if (lself) call selfterm(ereal,erecip,ec6,derive0self,factor,fct,ofct,1.0_dp,npotl,npots,c6tot, &
                                 d2self,.true.,.true.,i,j,ix,jx,1.0_dp,0.5_dp,lewaldtype,qli,qlj)
!
        if (nor.eq.0) goto 1110
!
        do k = 1,nor
          deriv2(k) = 0.0_dp
        enddo
!
!  Sqrt distances
!
        do k = 1,nor
          dist(k) = sqrt(dist(k))
        enddo
!
        call twobody(eatom,ereal,ec6,.true.,.true.,.false.,nor,1_i4,npots,npotl,cut2r,cut2q,cut2s, &
                     nmolonly,factor,ofct,radsum,rtrm1,sctrm1,sctrm2,qli,qlj,lcspair,lewaldtype, &
                     .false.,.false.,.false.,lorder12)
        if (leem.or.lDoQDeriv2) then
          do k = 1,nor
            d0i(k) = d0i(k) + derive0(k)*qlj
            d0j(k) = d0j(k) + derive0(k)*qli
          enddo
        endif
        if (leem) then
          if (lqeq) then
            call qeqbody(eqeq,.true.,.true.,nor,1_i4,fct,qli,qlj,nati,natj)
          elseif (lSandM) then
            call smbody(eqeq,.true.,.true.,nor,1_i4,fct,qli,qlj,nati,natj)
          endif
        endif
!
!  Generate products for derivatives
!
        do k = 1,nor
          rpd(k,1) = xtmp(k)*xtmp(k)
          rpd(k,2) = ytmp(k)*ytmp(k)
          rpd(k,3) = ztmp(k)*ztmp(k)
          rpd(k,4) = ytmp(k)*ztmp(k)
          rpd(k,5) = xtmp(k)*ztmp(k)
          rpd(k,6) = xtmp(k)*ytmp(k)
        enddo
!***************************************
!  Generate phased second derivatives  *
!***************************************
!
!  Set diagonal blocks to be difference from gamma point
!
        if (i.eq.j) then
          oneij = 1.0_dp
        else
          oneij = 0.0_dp
        endif
        do k = 1,nor
          cosk = xkv*xtmp(k) + ykv*ytmp(k) + zkv*ztmp(k)
          sink = sin(cosk)
          cosk = cos(cosk) - oneij
          dk = deriv(k)*cosk
          d2k = deriv2(k)*cosk
          dks = deriv(k)*sink
          d2ks = deriv2(k)*sink
          derv2(jx,ix) = derv2(jx,ix) - d2k*rpd(k,1)
          derv2(jy,ix) = derv2(jy,ix) - d2k*rpd(k,6)
          derv2(jz,ix) = derv2(jz,ix) - d2k*rpd(k,5)
          derv2(jx,iy) = derv2(jx,iy) - d2k*rpd(k,6)
          derv2(jy,iy) = derv2(jy,iy) - d2k*rpd(k,2)
          derv2(jz,iy) = derv2(jz,iy) - d2k*rpd(k,4)
          derv2(jx,iz) = derv2(jx,iz) - d2k*rpd(k,5)
          derv2(jy,iz) = derv2(jy,iz) - d2k*rpd(k,4)
          derv2(jz,iz) = derv2(jz,iz) - d2k*rpd(k,3)
          derv2(jx,ix) = derv2(jx,ix) - dk
          derv2(jy,iy) = derv2(jy,iy) - dk
          derv2(jz,iz) = derv2(jz,iz) - dk
          dervi(jx,ix) = dervi(jx,ix) - d2ks*rpd(k,1)
          dervi(jy,ix) = dervi(jy,ix) - d2ks*rpd(k,6)
          dervi(jz,ix) = dervi(jz,ix) - d2ks*rpd(k,5)
          dervi(jx,iy) = dervi(jx,iy) - d2ks*rpd(k,6)
          dervi(jy,iy) = dervi(jy,iy) - d2ks*rpd(k,2)
          dervi(jz,iy) = dervi(jz,iy) - d2ks*rpd(k,4)
          dervi(jx,iz) = dervi(jx,iz) - d2ks*rpd(k,5)
          dervi(jy,iz) = dervi(jy,iz) - d2ks*rpd(k,4)
          dervi(jz,iz) = dervi(jz,iz) - d2ks*rpd(k,3)
          dervi(jx,ix) = dervi(jx,ix) - dks
          dervi(jy,iy) = dervi(jy,iy) - dks
          dervi(jz,iz) = dervi(jz,iz) - dks
!
!  rpd arrays no longer needed - use to save cos/sin for use in EEM/QEq
!
          rpd(k,1) = cosk
          rpd(k,2) = sink
        enddo
        if (radsum.gt.0.0_dp) then
!
!  Radial components
!
          do k = 1,nor
            cosk = xkv*xtmp(k) + ykv*ytmp(k) + zkv*ztmp(k)
            sink = sin(cosk)
            cosk = cos(cosk) - oneij
            rrtrm = rderiv(k)*cosk
            ritrm = rderiv(k)*sink
            if (radi.gt.0.0_dp) then
              iis = indri
              derv2(ix,iis) = derv2(ix,iis) - rrtrm*xtmp(k)
              derv2(iy,iis) = derv2(iy,iis) - rrtrm*ytmp(k)
              derv2(iz,iis) = derv2(iz,iis) - rrtrm*ztmp(k)
              derv2(jx,iis) = derv2(jx,iis) + rrtrm*xtmp(k)
              derv2(jy,iis) = derv2(jy,iis) + rrtrm*ytmp(k)
              derv2(jz,iis) = derv2(jz,iis) + rrtrm*ztmp(k)
              dervi(ix,iis) = dervi(ix,iis) - ritrm*xtmp(k)
              dervi(iy,iis) = dervi(iy,iis) - ritrm*ytmp(k)
              dervi(iz,iis) = dervi(iz,iis) - ritrm*ztmp(k)
              dervi(jx,iis) = dervi(jx,iis) + ritrm*xtmp(k)
              dervi(jy,iis) = dervi(jy,iis) + ritrm*ytmp(k)
              dervi(jz,iis) = dervi(jz,iis) + ritrm*ztmp(k)
              derv2(iis,iis) = derv2(iis,iis) + rtrm2(k)*cosk
              dervi(iis,iis) = dervi(iis,iis) + rtrm2(k)*sink
            endif
            if (radj.gt.0.0_dp) then
              jjs = indrj
              derv2(ix,jjs) = derv2(ix,jjs) - rrtrm*xtmp(k)
              derv2(iy,jjs) = derv2(iy,jjs) - rrtrm*ytmp(k)
              derv2(iz,jjs) = derv2(iz,jjs) - rrtrm*ztmp(k)
              derv2(jx,jjs) = derv2(jx,jjs) + rrtrm*xtmp(k)
              derv2(jy,jjs) = derv2(jy,jjs) + rrtrm*ytmp(k)
              derv2(jz,jjs) = derv2(jz,jjs) + rrtrm*ztmp(k)
              dervi(ix,jjs) = dervi(ix,jjs) - ritrm*xtmp(k)
              dervi(iy,jjs) = dervi(iy,jjs) - ritrm*ytmp(k)
              dervi(iz,jjs) = dervi(iz,jjs) - ritrm*ztmp(k)
              dervi(jx,jjs) = dervi(jx,jjs) + ritrm*xtmp(k)
              dervi(jy,jjs) = dervi(jy,jjs) + ritrm*ytmp(k)
              dervi(jz,jjs) = dervi(jz,jjs) + ritrm*ztmp(k)
              derv2(jjs,jjs) = derv2(jjs,jjs) + rtrm2(k)*cosk
              dervi(jjs,jjs) = dervi(jjs,jjs) + rtrm2(k)*sink
              if (radi.gt.0.0_dp) then
                derv2(jjs,jjs) = derv2(jjs,jjs) + rtrm2(k)*cosk
                dervi(jjs,jjs) = dervi(jjs,jjs) + rtrm2(k)*sink
              endif
            endif
          enddo
        endif
!
!  If nor = 0, then rejoin here since there can still be a contribution to 
!  the second derivatives from the variable charges and the self term.
!
1110    continue
!********************************************
!  Variable charge contribution to phonons  *
!********************************************
        if (lDoQDeriv2) then
!
!  Calculate phased terms
!
          hfactor = 1.0_dp
          if (i.eq.j) hfactor = 0.5_dp
          d2self = hfactor*d2self
          derive0selfi = hfactor*derive0self*qlj
          derive0selfj = hfactor*derive0self*qli
          d1ix = 0.0_dp
          d1iy = 0.0_dp
          d1iz = 0.0_dp
          d1jx = 0.0_dp
          d1jy = 0.0_dp
          d1jz = 0.0_dp
          do k = 1,nor
            d0i(k) = d0i(k)*hfactor
            d0j(k) = d0j(k)*hfactor
            d1i(k) = d1i(k)*hfactor
            d1j(k) = d1j(k)*hfactor
            d2i2(k) = d2i2(k)*hfactor
            d2ij(k) = d2ij(k)*hfactor
            d2j2(k) = d2j2(k)*hfactor
            d1ix = d1ix + d1i(k)*xtmp(k)
            d1iy = d1iy + d1i(k)*ytmp(k)
            d1iz = d1iz + d1i(k)*ztmp(k)
            d1jx = d1jx + d1j(k)*xtmp(k)
            d1jy = d1jy + d1j(k)*ytmp(k)
            d1jz = d1jz + d1j(k)*ztmp(k)
          enddo
!
!  Apply variable charge correction to second derivatives
!
          call d2chargep(i,j,nor,ix,iy,iz,jx,jy,jz,xkv,ykv,zkv,d0i,d0j,d1ix,d1iy,d1iz, &
                         d1jx,d1jy,d1jz,d2i2,d2ij,d2j2,d2self,derive0selfi,derive0selfj,.true.)
        endif
1120       continue
      enddo
    enddo
!
!  End of real space part - perform general tasks
!
  endif
!
!  Symmetrise second derivative matrix
!
  do i = 2,maxlim
    do j = 1,i-1
      derv2(i,j) = derv2(j,i)
      dervi(i,j) = - dervi(j,i)
    enddo
  enddo
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realp','npotl')
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1
!
  return
  end
