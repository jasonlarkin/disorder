  subroutine realmimc3(eatom,ereal,erecip,ec6,ntrialatom,nptrtrialatom,ltrialatom)
!
!  Subroutine for calculating real space energy and gradients
!  This version is specifically for the MC of large systems
!  where we can use the minimum image convention. This means
!  looping over cell vectors can be eliminated for greater
!  speed. Subset of atoms version.
!
!   1/08 Created from realmi3
!  11/08 x/y/z components passed to twobody1
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   5/10 lra algorithm corrected
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
!  Julian Gale, NRI, Curtin University, May 2010
!
  use configurations, only : lbsmat
  use constants
  use control
  use current
  use derivatives
  use eam,            only : maxmeamcomponent
  use element
  use general,        only : cutw
  use kspace
  use mdlogic
  use molecule
  use numbers,        only : third
  use parallel
  use polarise
  use potentialxyz
  use shell
  use sutton
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: ntrialatom
  integer(i4), intent(in)                      :: nptrtrialatom(ntrialatom)
  logical,     intent(in)                      :: ltrialatom(numat)
  real(dp),    intent(inout)                   :: eatom
  real(dp),    intent(inout)                   :: ec6
  real(dp),    intent(inout)                   :: ereal
  real(dp),    intent(inout)                   :: erecip
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: iim
  integer(i4)                                  :: iimn
  integer(i4)                                  :: iimx
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjm
  integer(i4)                                  :: jjmn
  integer(i4)                                  :: jjmx
  integer(i4)                                  :: kk
  integer(i4)                                  :: kkm
  integer(i4)                                  :: kkmn
  integer(i4)                                  :: kkmx
  integer(i4)                                  :: n
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: nt
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lc6loc
  logical                                      :: lcspair
  logical                                      :: lewaldtype
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lptrmol
  logical                                      :: lsamemol
  real(dp)                                     :: c6prod
  real(dp)                                     :: c6self1
  real(dp)                                     :: c6tot
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cut2e
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2q
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: cut2w
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1i
  real(dp)                                     :: d1j
  real(dp)                                     :: d2i2
  real(dp)                                     :: d2ij
  real(dp)                                     :: d2j2
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: eatom2
  real(dp)                                     :: ec62
  real(dp)                                     :: ereal2
  real(dp)                                     :: eta3
  real(dp)                                     :: factor
  real(dp)                                     :: fct
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r2
  real(dp)                                     :: r2min
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rderiv
  real(dp)                                     :: rp
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1(maxmeamcomponent)
  real(dp)                                     :: sctrm2(maxmeamcomponent)
  real(dp)                                     :: setrm
  real(dp)                                     :: small
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcd
  real(dp)                                     :: ycd
  real(dp)                                     :: zcd
  real(dp)                                     :: xcdi
  real(dp)                                     :: ycdi
  real(dp)                                     :: zcdi
  real(dp)                                     :: xcdj
  real(dp)                                     :: ycdj
  real(dp)                                     :: zcdj
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
  real(dp)                                     :: xmin
  real(dp)                                     :: ymin
  real(dp)                                     :: zmin
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realmi3','npotl')
!
!  Local variables
!
  lc6loc = (lc6.and.ndim.eq.3)
  small = 1.0d-12
  if (lc6loc) then
    eta3 = eta*eta*eta
    c6self1 = 0.5_dp*eta3*third
  endif
!
!  Set the Coulomb term type based on dimensionality :
!
!  1-D         => 1/r
!  2-D and 3-D => erfc(seta*r)/r
!
  lewaldtype = (ndim.ne.1.or.lwolf)
!
  time1 = cputime()
!
!  Set up looping extents
!
  if (ndim.eq.3) then
    iimn = -1
    iimx =  1
    jjmn = -1
    jjmx =  1
    kkmn = -1
    kkmx =  1
  elseif (ndim.eq.2) then
    iimn = -1
    iimx =  1
    jjmn = -1
    jjmx =  1
    kkmn =  0
    kkmx =  0
  elseif (ndim.eq.1) then
    iimn = -1
    iimx =  1
    jjmn =  0
    jjmx =  0
    kkmn =  0
    kkmx =  0
  endif
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
  if (lnoreal) goto 999
!***************************************************************
!  Atomistic and real space electrostatic component of energy  *
!***************************************************************
!
!  Outer loop over trial sites
!
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
!
    xal = xclat(i)
    yal = yclat(i)
    zal = zclat(i)
    nati = nat(i)
    ntypi = nftype(i)
    qli = qf(i)
    oci = occuf(i)
    if (lbsmat(nrelat(i)+nsft)) then
      radi = radf(i)
    else
      radi = 0.0_dp
    endif
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
    jloop: do j = procid+1,numat,nprocs
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
!
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      qlj = qf(j)
      ocj = occuf(j)
      if (lbsmat(nsft+nrelat(j))) then
        radj = radf(j)
      else
        radj = 0.0_dp
      endif
      radsum = radi + radj
!       
!  Set factor that depends on whether j is a trial atom or not
!       
      if (ltrialatom(j)) then
        ofct = 0.5_dp*oci*ocj
      else
        ofct = oci*ocj
      endif
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
!  Calculate sum of all dispersion terms for pair
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
      if (npots.eq.0.and.abs(factor).lt.1.0d-8) cycle jloop
      cut2r = rp*rp
      if (cut2r.gt.cut2p) cut2r = cut2p
      cut2 = cut2r
      if (cut2e.gt.cut2.and.lewald) cut2 = cut2e
      if (cut2w.gt.cut2.and.lwolf) cut2 = cut2w
      rp = sqrt(cut2)
!  
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok = .false.
!
      nor = 0
      nmolonly = 0
!***********************
!  Find minimum image  *
!***********************
      if (lra) then
        r2min = 1.0d10
        xcd = xcrd + (iimn-1)*r1x
        do ii = iimn,iimx
          xcd = xcd + r1x
          ycd = ycrd + (jjmn-1)*r2y
          do jj = jjmn,jjmx
            ycd = ycd + r2y
            zcd = zcrd + (kkmn-1)*r3z
            do kk = kkmn,kkmx
              zcd = zcd + r3z
              r2 = xcd*xcd + ycd*ycd + zcd*zcd
              if (r2.lt.r2min) then
                r2min = r2
                xmin = xcd
                ymin = ycd
                zmin = zcd
                iim = ii
                jjm = jj
                kkm = kk
              endif
            enddo
          enddo
        enddo
        r2 = r2min
        xcrd = xmin
        ycrd = ymin
        zcrd = zmin
      else
        r2min = 1.0d10
        xcdi = xcrd + (iimn-1)*r1x
        ycdi = ycrd + (iimn-1)*r1y
        zcdi = zcrd + (iimn-1)*r1z
        do ii = iimn,iimx
          xcdi = xcdi + r1x
          ycdi = ycdi + r1y
          zcdi = zcdi + r1z
          xcdj = xcdi + (jjmn-1)*r2x
          ycdj = ycdi + (jjmn-1)*r2y
          zcdj = zcdi + (jjmn-1)*r2z
          do jj = jjmn,jjmx
            xcdj = xcdj + r2x
            ycdj = ycdj + r2y
            zcdj = zcdj + r2z
            xcd = xcdj + (kkmn-1)*r3x
            ycd = ycdj + (kkmn-1)*r3y
            zcd = zcdj + (kkmn-1)*r3z
            do kk = kkmn,kkmx
              xcd = xcd + r3x
              ycd = ycd + r3y
              zcd = zcd + r3z
              r2 = xcd*xcd + ycd*ycd + zcd*zcd
              if (r2.lt.r2min) then
                r2min = r2
                xmin = xcd
                ymin = ycd
                zmin = zcd
                iim = ii
                jjm = jj
                kkm = kk
              endif
            enddo
          enddo
        enddo
        r2 = r2min
        xcrd = xmin
        ycrd = ymin
        zcrd = zmin
      endif
!***************************
!  Molecule - check index  *
!***************************
      if (lmolok.and.(r2.gt.cut2s.or..not.lcspair)) then
        call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,i,j,iim,jjm,kkm)
        lsamemol = (lbonded.or.l2bonds.or.l3bonds)
        if (.not.lsamemol) then
          call samemol(lsamemol,nmi,iim,jjm,kkm,ixj,iyj,izj)
        endif
        lptrmol = lsamemol
        if (lsamemol) then
          if (r2.gt.cut2q) nmolonly = 1
        else
          lbonded   = .false.
          l2bonds   = .false.
          l3bonds   = .false.
          nbtypeij  = 0
          nbtypeij2 = 0
        endif
      else
        lptrmol   = .false.
        lbonded   = .false.
        l2bonds   = .false.
        l3bonds   = .false.
        nbtypeij  = 0
        nbtypeij2 = 0
      endif
!***************************
!  Check cut-off distance  *
!***************************
      if (r2.lt.small) then
        if (lewaldtype) then
!
!  Remove self energy
!
          setrm = factor*tweatpi
          erecip = erecip - setrm
          if (lc6loc) then
            if (lc6one) then
              c6prod = ofct*c6f(i)*c6f(j)
            else
              c6prod = ofct*c6tot
            endif
            ec6 = ec6 + c6self1*c6prod
          endif
        endif
      elseif (r2.le.cut2.or.lptrmol) then
!
!  Store vector
!
        nor = 1
        dist = sqrt(r2)
      endif
!**********************************************************
!  If no valid distance then skip the energy calculation  *
!**********************************************************
      if (nor.eq.0) cycle jloop
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
      eatom2 = 0.0_dp
      ereal2 = 0.0_dp
      ec62 = 0.0_dp
      call twobody1(eatom2,ereal2,ec62,.false.,.false.,.false.,1_i4,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,cut2r,cut2q, &
                    cut2s,lptrmol,nmolonly,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,lewaldtype,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,.false.,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
      eatom = eatom + eatom2
      ereal = ereal + ereal2
      ec6 = ec6 + ec62
    enddo jloop
  enddo
!
!  Exit point
!
999 continue
!
!  Free local memory
!
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realmi3','npotl')
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1 
!
  return
  end
