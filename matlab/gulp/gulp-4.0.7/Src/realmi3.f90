  subroutine realmi3(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
!
!  Subroutine for calculating real space energy and gradients
!  This version is specifically for the MD of large systems
!  where we can use the minimum image convention. This means
!  looping over cell vectors can be eliminated for greater
!  speed.
!
!   7/99 Created from realmd3
!   6/00 Calculation of electric field added for polarisation energy
!   2/01 Generalised for any dimensionality
!   4/01 Calculation of surface region 2 energy added
!   6/01 Setting of lsamemol altered
!   9/01 Modifications for 1-D made
!   9/01 lmolq calculations accelerated using lneedmol 
!  11/01 Attachment energy added
!   1/02 Correction made for molecule case - cell image indices tracked
!   2/02 lneedmol algorithm corrected
!   8/02 Surface energy calculation algorithm changed
!  10/02 ReaxFF modifications added
!  11/02 Wildcard atoms added
!  11/02 Parallel changes made
!   1/03 Wolf modifications made
!   1/03 Check on Ewald made for nptrmol as well as cut2e
!   5/03 Region 3 modifications made
!   9/04 Charge first derivatives added
!   4/05 Mods for cosh-spring added
!   7/05 Streitz and Mintmire modifications added
!   2/07 Bonding types added
!   3/07 Printing of twobody energies added as an option
!   3/07 Bonding types modified
!   5/07 QM/MM scheme added
!   5/07 Argument list for twobody call modified
!  11/07 Unused variables cleaned up
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!  11/08 x/y/z components passed to twobody1
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   4/09 MEAM density modifications removed since these are now handled in 
!        a separate routine.
!   6/09 Site energy and virials added
!  11/09 Region derivatives added
!   5/10 lra algorithm corrected 
!  11/11 Region-region energy contributions stored
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
  use configurations, only : lbsmat,lsliceatom,nregions,nregionno,nregiontype,QMMMmode
  use constants
  use control
  use current
  use derivatives
  use eam,            only : lMEAMden
  use element
  use energies,       only : siteenergy, eregion2region
  use general,        only : cutw
  use iochannels,     only : ioout
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
  logical,     intent(in)                      :: lgrad1
  real(dp),    intent(inout)                   :: eatom
  real(dp),    intent(inout)                   :: eattach
  real(dp),    intent(inout)                   :: ec6
  real(dp),    intent(inout)                   :: eqeq
  real(dp),    intent(inout)                   :: ereal
  real(dp),    intent(inout)                   :: erecip
  real(dp),    intent(inout)                   :: esregion12
  real(dp),    intent(inout)                   :: esregion2
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
  integer(i4)                                  :: kl
  integer(i4)                                  :: m
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
  integer(i4)                                  :: noff
  integer(i4)                                  :: noffm1
  integer(i4)                                  :: noffset
  integer(i4)                                  :: nor
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: npots
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lattach
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lc6loc
  logical                                      :: lcspair
  logical                                      :: lewaldtype
  logical                                      :: lgrad1p
  logical                                      :: lmatch
  logical                                      :: lmdl
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lorder12
  logical                                      :: lptrmol
  logical                                      :: lQMMMelectro
  logical                                      :: lreg2one
  logical                                      :: lreg2pair
  logical                                      :: lsamemol
  logical                                      :: lsg1
  logical                                      :: lslicei
  logical                                      :: lslicej
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
  real(dp)                                     :: derive0self
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: eatom2
  real(dp)                                     :: ec62
  real(dp)                                     :: ec6trm
  real(dp)                                     :: eqeq2  
  real(dp)                                     :: ereal2
  real(dp)                                     :: esum 
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
  real(dp)                                     :: rpdl(6)
  real(dp)                                     :: rqeq2
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp)                                     :: setrm
  real(dp)                                     :: small
  real(dp),    dimension(:), allocatable       :: sum
  real(dp),    dimension(:), allocatable       :: sum2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tsum0
  real(dp)                                     :: tsuml
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
  real(dp)                                     :: xtmp
  real(dp)                                     :: ytmp
  real(dp)                                     :: ztmp
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realmi3','npotl')
  allocate(sum(numat),stat=status)
  if (status/=0) call outofmemory('realmi3','sum')
  allocate(sum2(numat),stat=status)
  if (status/=0) call outofmemory('realmi3','sum2')
!
!  Local variables
!
  lmdl = lmd
  if (.not.lgrad1) lmdl = .false.
  lgrad1p = (lgrad1.or.lpolar)
  lc6loc = (lc6.and.ndim.eq.3)
  small = 1.0d-12
  rqeq2 = rqeq*rqeq
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
  tsuml = 0.0_dp
  lsg1 = (lgrad1.and.lstr)
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
!
!  Openning banner for energy decomposition
!
  if (lPrintTwo) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Two : Atom No. 1  Atom No. 2    Short-range energy (eV)   Coulomb energy (eV) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!***************************************************************
!  Atomistic and real space electrostatic component of energy  *
!***************************************************************
!
!  Outer loop over sites
!
!  Use Brode-Ahlrichs Algorithm
  noff = numat/2
  noffset = noff
  if (mod(numat,2_i4).eq.0) then
    noffm1 = noff - 1
  else
    noffm1 = noff
  endif
!orig do i = 2,numat
  do i = procid+1,numat,nprocs
    if (i.gt.noff) then
      noffset = noffm1
    endif
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
    nregioni = nregionno(nsft + nrelat(i))
    nregiontypi = nregiontype(nregioni,ncf)
    lslicei = lsliceatom(nsft + nrelat(i))
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
!orig do j=1,i-1
    jloop: do m = 1,noffset
      j = mod(i+m-1_i4,numat)+1
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
      nregionj = nregionno(nsft+nrelat(j))
      nregiontypj = nregiontype(nregionj,ncf)
!
!  QM/MM handling : i & j are both QM atoms => exclude
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop
      endif
!           
!  QM/MM : Set electrostatic embedding flag : If either i or j are QM atoms => exclude electrostatics
!       
      lQMMMelectro = (QMMMmode(ncf).eq.2.and.(nregiontypi.eq.1.or.nregiontypj.eq.1))
!
!  Set region 2 pair flag
!
      lreg2one  = .false.
      lreg2pair = .false.
      if (lseok.and.nregions(ncf).ge.2) then
        lreg2pair = (nregioni.eq.2.and.nregionj.eq.2)
        if (.not.lreg2pair) lreg2one = (nregioni.eq.2.or.nregionj.eq.2)
      endif
      lslicej = lsliceatom(nsft + nrelat(j))
      lattach = (lslicei.and..not.lslicej.or.lslicej.and..not.lslicei)
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
      ofct = oci*ocj
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
      if (lqeq.or.lSandM) cut2 = max(cut2,rqeq2)
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
          if (lreg2one) then
            esregion12 = esregion12 - setrm
          elseif (lreg2pair) then
            esregion2 = esregion2 - setrm
          else
            erecip = erecip - setrm
          endif
          if (lgrad1.and.lDoQDeriv1) then
            derive0self = - fct*tweatpi
            call d1charge(i,j,.true.,.true.,1_i4,derive0self*qlj,derive0self*qli)
          endif
          if (lattach) eattach = eattach - setrm
!
          eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + setrm
!
          siteenergy(i) = siteenergy(i) - 0.5_dp*setrm
          siteenergy(j) = siteenergy(j) - 0.5_dp*setrm
!
          if (lc6loc) then
            if (lc6one) then
              c6prod = ofct*c6f(i)*c6f(j)
            else
              c6prod = ofct*c6tot
            endif
            if (lreg2one) then
              esregion12 = esregion12 + c6self1*c6prod
            elseif (lreg2pair) then
              esregion2 = esregion2 + c6self1*c6prod
            else
              ec6 = ec6 + c6self1*c6prod
            endif
            if (lattach) eattach = eattach + c6self1*c6prod
!
            eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + c6self1*c6prod
!
            siteenergy(i) = siteenergy(i) + 0.5_dp*c6self1*c6prod
            siteenergy(j) = siteenergy(j) + 0.5_dp*c6self1*c6prod
          endif
        endif
      elseif (r2.le.cut2.or.lptrmol) then
!
!  Store vector
!
        nor = 1
        dist = sqrt(r2)
        xtmp = xcrd
        ytmp = ycrd
        ztmp = zcrd
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
      call twobody1(eatom2,ereal2,ec62,lgrad1p,.false.,.false.,1_i4,1_i4,dist,xtmp,ytmp,ztmp,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl,cut2r,cut2q, &
                    cut2s,lptrmol,nmolonly,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,lewaldtype,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,lQMMMelectro,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
      esum = eatom2 + ereal2 + ec62
      if (lreg2one) then
        esregion12 = esregion12 + esum
      elseif (lreg2pair) then
        esregion2 = esregion2 + esum
      else
        eatom = eatom + eatom2
        ereal = ereal + ereal2
        ec6 = ec6 + ec62
      endif
      if (lattach) eattach = eattach + esum
!
      eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + esum
!
      siteenergy(i) = siteenergy(i) + 0.5_dp*esum
      siteenergy(j) = siteenergy(j) + 0.5_dp*esum
!
      if (lPrintTwo) then
        write(ioout,'(4x,i12,i12,f22.10,1x,f22.10)') i,j,eatom2+ec62,ereal2
      endif
!
      if (lDoQDeriv1.and.lgrad1) then
        d0i = d0i + derive0*qlj
        d0j = d0j + derive0*qli
      endif
      if (leem) then
        if (lqeq.or.lSandM) then
          eqeq2 = 0.0_dp
          if (lqeq) then
            call qeqbody1(eqeq2,lgrad1,.false.,nor,1_i4,dist,deriv,deriv2,fct,qli,qlj,nati,natj,d1i,d1j,d2i2,d2ij,d2j2)
          elseif (lSandM) then
            call smbody1(eqeq2,lgrad1,.false.,nor,1_i4,dist,deriv,deriv2,fct,qli,qlj,nati,natj,d1i,d1j,d2i2,d2ij,d2j2)
          endif
          if (lreg2one) then
            esregion12 = esregion12 + eqeq2
          elseif (lreg2pair) then
            esregion2 = esregion2 + eqeq2
          else
            eqeq = eqeq + eqeq2
          endif
          if (lattach) eattach = eattach + eqeq2
        endif
!
        eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eqeq2
!
        siteenergy(i) = siteenergy(i) + 0.5_dp*eqeq2
        siteenergy(j) = siteenergy(j) + 0.5_dp*eqeq2
      endif
      if (lsuttonc) then
        if (.not.lMEAMden) then
          if (lorder12) then
            scrho(1,i) = scrho(1,i) + sctrm1*ocj
            scrho(1,j) = scrho(1,j) + sctrm2*oci
            if (lattach) then
              scrho12(1,i) = scrho12(1,i) + sctrm1*ocj
              scrho12(1,j) = scrho12(1,j) + sctrm2*oci
            endif
          else
            scrho(1,i) = scrho(1,i) + sctrm2*ocj
            scrho(1,j) = scrho(1,j) + sctrm1*oci
            if (lattach) then
              scrho12(1,i) = scrho12(1,i) + sctrm2*ocj
              scrho12(1,j) = scrho12(1,j) + sctrm1*oci
            endif
          endif
        endif
      endif
!
!  Generate products for derivatives
!
      if (lmdl.or.lsg1) then
        if (ndim.eq.3) then
          rpdl(1) = xtmp*xtmp
          rpdl(2) = ytmp*ytmp
          rpdl(3) = ztmp*ztmp
          rpdl(4) = ytmp*ztmp
          rpdl(5) = xtmp*ztmp
          rpdl(6) = xtmp*ytmp
        elseif (ndim.eq.2) then
          rpdl(1) = xtmp*xtmp
          rpdl(2) = ytmp*ytmp
          rpdl(3) = xtmp*ytmp
        elseif (ndim.eq.1) then
          rpdl(1) = xtmp*xtmp
        endif
      endif
!*****************************
!  Charge first derivatives  *
!*****************************
      if (lgrad1.and.lDoQDeriv1) then
        call d1charge(i,j,.true.,.true.,1_i4,d0i,d0j)
      endif
!*************************************
!  Electrostatic potential on-sites  *
!*************************************
      if (lpolar) then
        vx(i) = vx(i) - qlj*derive*xtmp
        vy(i) = vy(i) - qlj*derive*ytmp
        vz(i) = vz(i) - qlj*derive*ztmp
        vx(j) = vx(j) + qli*derive*xtmp
        vy(j) = vy(j) + qli*derive*ytmp
        vz(j) = vz(j) + qli*derive*ztmp
        if (lattach) then
          vx12(i) = vx12(i) - qlj*derive*xtmp
          vy12(i) = vy12(i) - qlj*derive*ytmp
          vz12(i) = vz12(i) - qlj*derive*ztmp
          vx12(j) = vx12(j) + qli*derive*xtmp
          vy12(j) = vy12(j) + qli*derive*ytmp
          vz12(j) = vz12(j) + qli*derive*ztmp
        endif
      endif
!***********************
!  Radial derivatives  *
!***********************
      if (lgrad1) then
        if (radi.gt.0.0_dp) then
          raderv(i) = raderv(i) + rtrm1
        endif
        if (radj.gt.0.0_dp) then
          raderv(j) = raderv(j) + rtrm1
        endif
      endif
!************************
!  Internal Derivatives *
!************************
!
!  First derivatives
!
      if (lgrad1) then
        xdrv(i) = xdrv(i) - deriv*xtmp
        ydrv(i) = ydrv(i) - deriv*ytmp
        zdrv(i) = zdrv(i) - deriv*ztmp
        xdrv(j) = xdrv(j) + deriv*xtmp
        ydrv(j) = ydrv(j) + deriv*ytmp
        zdrv(j) = zdrv(j) + deriv*ztmp
!
        if (nregioni.ne.nregionj) then
          xregdrv(nregioni) = xregdrv(nregioni) - deriv*xtmp
          yregdrv(nregioni) = yregdrv(nregioni) - deriv*ytmp
          zregdrv(nregioni) = zregdrv(nregioni) - deriv*ztmp
          xregdrv(nregionj) = xregdrv(nregionj) + deriv*xtmp
          yregdrv(nregionj) = yregdrv(nregionj) + deriv*ytmp
          zregdrv(nregionj) = zregdrv(nregionj) + deriv*ztmp
        endif
      endif
!***********************
!  Strain derivatives  *
!***********************
!
!  Strain only terms 
!
!  First derivatives 
!
      if (lsg1) then
        rstrdloc(1:nstrains) = 0.0_dp
        do kl = 1,nstrains
          rstrdloc(kl) = rstrdloc(kl) + deriv*rpdl(kl)
          rstrd(kl) = rstrd(kl) + deriv*rpdl(kl)
        enddo
        if (latomicstress) then
          do kl = 1,nstrains
            atomicstress(kl,i) = atomicstress(kl,i) + 0.5_dp*rstrdloc(kl)
            atomicstress(kl,j) = atomicstress(kl,j) + 0.5_dp*rstrdloc(kl)
          enddo
        endif
      endif
    enddo jloop
  enddo
!*******************
!  Self-term loop  *
!*******************
!orig do i=1,numat
  iloop: do i = procid+1,numat,nprocs
    nati = nat(i)
    ntypi = nftype(i)
    qli = qf(i)
    oci = occuf(i)
!
!  Factor of half for self term
!
    ofct = 0.5_dp*oci*oci
    fct = ofct*angstoev
    factor = qli*qli*fct
!
!  Set region 2 pair flag
!
    lreg2pair = .false.
    if (lseok.and.nregions(ncf).ge.2) then
      lreg2pair = (nregionno(nsft+nrelat(i)).eq.2)
    endif
    nregioni = nregionno(nsft+nrelat(i))
    nregiontypi = nregiontype(nregioni,ncf)
!
!  QM/MM handling : i is a QM atom => exclude
!
    if (QMMMmode(ncf).gt.0) then
      if (nregiontypi.eq.1) cycle iloop
    endif
!           
!  QM/MM : Set electrostatic embedding flag : If either i or j are QM atoms => exclude electrostatics
!       
    lQMMMelectro = (QMMMmode(ncf).eq.2.and.nregiontypi.eq.1)
!
!  Calculate sum of dispersion terms
!
    c6tot = 0.0_dp
    do n = 1,npote
      if (lmatch(nati,ntypi,nspec1(n),nptyp1(n),.true.)) then
        if (lmatch(nati,ntypi,nspec2(n),nptyp2(n),.true.)) then
          if (nptype(n).eq.1.or.nptype(n).eq.7) then
            c6tot = c6tot + twopot(3,n)
          elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
            c6tot = c6tot + twopot(2,n)
          endif
        endif
      endif
    enddo
!
!  Remove self energy
!
    setrm = factor*tweatpi
    if (lreg2pair) then
      esregion2 = esregion2 - setrm
    else
      erecip = erecip - setrm
    endif
!
    eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) - setrm
!
    siteenergy(i) = siteenergy(i) - setrm
!
    if (lgrad1.and.lDoQDeriv1) then
      derive0self = - fct*tweatpi
      call d1charge(i,i,.true.,.true.,1_i4,derive0self*qli,derive0self*qli)
    endif
    if (lc6loc) then
      if (lc6one) then
        ec6trm = c6self1*ofct*c6f(i)*c6f(i)
        if (lreg2pair) then
          esregion2 = esregion2 + ec6trm
        else
          ec6 = ec6 + ec6trm
        endif
      else
        ec6trm = c6self1*ofct*c6tot
        if (lreg2pair) then
          esregion2 = esregion2 + ec6trm
        else
          ec6 = ec6 + ec6trm
        endif
      endif
!
      eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + ec6trm
!
      siteenergy(i) = siteenergy(i) + ec6trm
    endif
  enddo iloop
!****************
!  Global sums  *
!****************
  tsum0 = cputime()
  if (lsuttonc.and.nprocs.gt.1) then
    if (.not.lMEAMden) then
      do i = 1,numat
        sum2(i) = scrho(1,i)
      enddo
      call sumall(sum2,sum,numat,"realmi3","scrho")
      do i = 1,numat
        scrho(1,i) = sum(i)
      enddo
      do i = 1,numat
        sum2(i) = scrho12(1,i)
      enddo
      call sumall(sum2,sum,numat,"realmi3","scrho12")
      do i = 1,numat
        scrho12(1,i) = sum(i)
      enddo
    endif
  endif
  tsuml = cputime() - tsum0
  tsum = tsum + tsuml
!
!  Closing banner for energy decomposition
!
  if (lPrintTwo) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!
!  Exit point
!
999 continue
!
!  Free local memory
!
  deallocate(sum2,stat=status)
  if (status/=0) call deallocate_error('realmi3','sum2')
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('realmi3','sum')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realmi3','npotl')
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1 - tsuml
!
  return
  end
