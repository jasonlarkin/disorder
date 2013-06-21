  subroutine realmd3s(eatom,ereal,erecip,ec6,eqeq,esregion12,esregion2,eattach,lgrad1)
!
!  Subroutine for calculating real space energy and gradients using a spatial decomposition
!  algorithm
!
!  lfreeze = if .true. only do derivatives for atoms with a degree of freedom
!
!   5/03 Created from realmd3
!   5/03 Region 3 modifications added
!   9/03 Flag for periodicity / molecule handling corrected
!  10/03 Modified parallel algorithm introduced to remove poor scaling
!        of nprocs > 1 section in brennermd
!   9/04 Charge first derivatives added
!   1/05 r2 sqrt'd before passing to twobody1
!   4/05 Mods for cosh-spring added
!   7/05 Streitz and Mintmire modifications added
!   2/07 Bonding types added
!   3/07 Printing of twobody energies added as an option
!   3/07 Bonding types modified
!   5/07 QM/MM scheme added
!   5/07 Argument list for twobody call modified
!   6/07 Structure of arrays for storing spatial distribution changed to 1-D
!  11/07 Unused variables cleaned up
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!   4/08 Modified for variable domain size
!   4/08 xvec1cell replaced by xvec2cell etc for spatial algorithm
!   4/08 ind1toijk replaced by ind2toijk
!  11/08 x/y/z components passed to twobody1
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   3/09 Value of distance check for lself replaced by global value smallself from general module
!   3/09 Breathing shell self term added
!   4/09 Globalisation of scrho values now only done if nprocs=1
!   4/09 MEAM density modifications removed since these are now handled in 
!        a separate routine.
!   6/09 Site energy and virials added
!  11/09 Region derivatives added
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
  use configurations, only : lbsmat,lsliceatom,nregions,nregionno, nregiontype, QMMMmode
  use constants
  use control
  use current
  use datatypes
  use derivatives,    only : xdrv, ydrv, zdrv, rstrd, raderv
  use derivatives,    only : xregdrv, yregdrv, zregdrv, atomicstress
  use eam,            only : lMEAMden
  use element
  use energies,       only : siteenergy, eregion2region
  use general,        only : cutw, smallself
  use iochannels,     only : ioout
  use kspace
  use mdlogic
  use molecule
  use optimisation
  use parallel
  use polarise
  use potentialxyz
  use shell
  use spatial
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
  integer(i4), dimension(:), allocatable       :: npotl
  integer(i4)                                  :: i
  integer(i4)                                  :: ic
  integer(i4)                                  :: icx
  integer(i4)                                  :: icy
  integer(i4)                                  :: icz
  integer(i4)                                  :: ii
  integer(i4)                                  :: ijcx
  integer(i4)                                  :: ijcy
  integer(i4)                                  :: ijcz
  integer(i4)                                  :: imx
  integer(i4)                                  :: imy
  integer(i4)                                  :: imz
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ind
  integer(i4)                                  :: ind2
  integer(i4)                                  :: indn
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: ixyz
  integer(i4)                                  :: j
  integer(i4)                                  :: jc
  integer(i4)                                  :: jcx
  integer(i4)                                  :: jcy
  integer(i4)                                  :: jcz
  integer(i4)                                  :: jj
  integer(i4)                                  :: kl
  integer(i4)                                  :: ks
  integer(i4)                                  :: maxxy
  integer(i4)                                  :: maxx
  integer(i4)                                  :: n
  integer(i4)                                  :: n1i
  integer(i4)                                  :: n1j
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: npots
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nsplower(3)
  integer(i4)                                  :: nspupper(3)
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lattach
  logical                                      :: lbonded
  logical                                      :: lc6loc
  logical                                      :: lcspair
  logical                                      :: lewaldtype
  logical                                      :: lgrad1p
  logical                                      :: lmatch
  logical                                      :: lmdl
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lorder12
  logical                                      :: lptrmol
  logical                                      :: lQMMMelectro
  logical                                      :: lreg2one
  logical                                      :: lreg2pair
  logical                                      :: lsamemol
  logical                                      :: lself
  logical                                      :: lsg1  
  logical                                      :: lslicei
  logical                                      :: lslicej
  real(dp)                                     :: apt
  real(dp)                                     :: bpt
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
  real(dp)                                     :: d2self
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0self
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: eatom2
  real(dp)                                     :: ec62 
  real(dp)                                     :: eqeq2  
  real(dp)                                     :: ereal2
  real(dp)                                     :: erecip2
  real(dp)                                     :: esum 
  real(dp)                                     :: etrm
  real(dp)                                     :: etrm1
  real(dp)                                     :: etrm2
  real(dp)                                     :: factor
  real(dp)                                     :: fct
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: r2
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rderiv
  real(dp)                                     :: rdiff
  real(dp)                                     :: rp
  real(dp)                                     :: rpd(6)
  real(dp)                                     :: rqeq2
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp),    dimension(:), allocatable       :: sum
  real(dp),    dimension(:), allocatable       :: sum2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tsum0
  real(dp)                                     :: tsuml
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
  real(dp)                                     :: xji
  real(dp)                                     :: yji
  real(dp)                                     :: zji
!
  time1 = cputime()
!
!  Local variables
!
  lmdl = lmd
  if (.not.lgrad1) lmdl = .false.
  lgrad1p = (lgrad1.or.lpolar)
  lc6loc = (lc6.and.ndim.eq.3)
  rqeq2 = rqeq*rqeq
!
!  Set the Coulomb term type based on dimensionality :
!
!  1-D         => 1/r
!  2-D and 3-D => erfc(seta*r)/r
!
  lewaldtype = (ndim.ne.1.or.lwolf)
!
  tsuml = 0.0_dp
  lsg1 = (lgrad1.and.lstr)
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
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realmd3s','npotl')
  allocate(sum(numat),stat=status)
  if (status/=0) call outofmemory('realmd3s','sum')
  allocate(sum2(numat),stat=status)
  if (status/=0) call outofmemory('realmd3s','sum2')
!
  if (.not.lnoreal) then
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
    maxxy = nspcell(1)*nspcell(2)
    maxx  = nspcell(1)
!
!  Loop over all local spatial cells except buffer regions
!
    do ixyz = 1,ncellpernode
      ind = ncellnodeptr(ixyz)
      ind2 = ind - 1
      iz = ind2/maxxy
      ind2 = ind2 - maxxy*iz
      iy = ind2/maxx
      ix = ind2 - maxx*iy + 1
      iy = iy + 1
      iz = iz + 1
      if (.not.lbuffercell(ixyz)) then
!
!  Set cell search bounds
!  
        nspupper(1) = min(ix+ncellsearch(1),nspcell(1))
        nspupper(2) = min(iy+ncellsearch(2),nspcell(2))
        nspupper(3) = min(iz+ncellsearch(3),nspcell(3))
        nsplower(1) = max(ix-ncellsearch(1),1)
        nsplower(2) = max(iy-ncellsearch(2),1)
        nsplower(3) = max(iz-ncellsearch(3),1)
!
!  Get number of atoms in this cell
!
        ni = nspcellat(ind)
        n1i = nspcellat1ptr(ind)
!
!  Outer loop over atoms within this cell
!
        do ii = 1,ni
          i = nspcellatptr(n1i+ii)
          ic = nspcellatptrcell(n1i+ii)
          call ind2toijk(ic,icx,icy,icz)
!
!  Set coordinates of atom i
!
          xi = xinbox(i) + xvec2cell(ic)
          yi = yinbox(i) + yvec2cell(ic)
          zi = zinbox(i) + zvec2cell(ic)
!
!  Set other properties of atom i
!
          nati = nat(i)
          ntypi = nftype(i)
          qli = qf(i)
          oci = occuf(i)
          nregioni = nregionno(nsft+nrelat(i))
          nregiontypi = nregiontype(nregioni,ncf)
          lopi = (.not.lfreeze.or.lopf(nrelat(i)))
          lslicei = lsliceatom(nsft + nrelat(i))
          if (lbsmat(nrelat(i)+nsft)) then
            radi = radf(i)
          else
            radi = 0.0_dp
          endif
!
!  Molecule handling for atom i
!
          if (lmol) then
            nmi = natmol(i)
            indm = nmolind(i)
            call mindtoijk(indm,ixi,iyi,izi)
          endif
!
!  Loop over neighbouring cells
!
          do imz = nsplower(3),nspupper(3)
            do imy = nsplower(2),nspupper(2)
              do imx = nsplower(1),nspupper(1)
                indn = (imz-1)*maxxy + (imy-1)*maxx + imx
!
!  Loop over atoms within neighbouring cells
!
                nj = nspcellat(indn)
                n1j = nspcellat1ptr(indn)
                jloop: do jj = 1,nj
                  j = nspcellatptr(n1j+jj)
                  jc = nspcellatptrcell(n1j+jj)
                  call ind2toijk(jc,jcx,jcy,jcz)
!
!  Only calculate lower-half triangular interactions
!
                  if (j.le.i) then
!
!  Freezing flag
!
                    lopj = (.not.lfreeze.or.lopf(nrelat(j)))
                    if (.not.lopi.and..not.lopj) cycle jloop
!
!  Set coordinate differences and calculate square of distance
!  
                    xji = xvec2cell(jc) + xinbox(j) - xi
                    yji = yvec2cell(jc) + yinbox(j) - yi
                    zji = zvec2cell(jc) + zinbox(j) - zi
                    r2 = xji*xji + yji*yji + zji*zji
!
!  Set species type parameters for atom j
!
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
!  Set remaining properties for atom type j
!
                    qlj = qf(j)
                    ocj = occuf(j)
                    if (lbsmat(nsft+nrelat(j))) then
                      radj = radf(j)
                    else
                      radj = 0.0_dp
                    endif
                    radsum = radi + radj
                    if (i.eq.j) then
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
                          if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n)))  &
                            lneedmol = .true.
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
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
                    if (.not.lneedmol) lmolok = .false.
                    nmolonly = 0
!
!  Molecule - check index
!
                    if (lmolok.and.(r2.gt.cut2s.or..not.lcspair)) then
                      ijcx = jcx - icx
                      ijcy = jcy - icy
                      ijcz = jcz - icz
                      call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,i,j,ijcx,ijcy,ijcz)
                      lsamemol = (lbonded.or.l2bonds.or.l3bonds)
                      if (.not.lsamemol) then
                        call samemol(lsamemol,nmi,ijcx,ijcy,ijcz,ixj,iyj,izj)
                      endif
                      lptrmol = lsamemol
                      if (lsamemol) then
                        if (r2.gt.cut2e) nmolonly = 1
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
!
!  Check distance against potential cutoff
!
                    if (r2.gt.cut2.and..not.lptrmol) cycle jloop
!
!  Set self term flag
!
                    lself = (r2.lt.smallself)
!
                    if (lself) then
                      d2self = 0.0_dp
                      ereal2 = 0.0_dp
                      erecip2 = 0.0_dp
                      ec62 = 0.0_dp
                      derive0self = 0.0_dp
                      call selfterm(ereal2,erecip2,ec62,derive0self,factor,fct,ofct,1.0_dp,npotl,npots, &
                                    c6tot,d2self,lgrad1,.false.,i,j,0,0,1.0_dp,0.5_dp,lewaldtype,qli,qlj)
                      esum = ereal2 + erecip2 + ec62
                      if (lreg2one) then
                        esregion12 = esregion12 + esum
                      elseif (lreg2pair) then
                        esregion2 = esregion2 + esum
                      else
                        ereal = ereal + ereal2
                        erecip = erecip + erecip2
                        ec6 = ec6 + ec62
                      endif
                      if (lattach) eattach = eattach + esum
!
                      eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + esum
!
                      siteenergy(i) = siteenergy(i) + 0.5_dp*esum
                      siteenergy(j) = siteenergy(j) + 0.5_dp*esum
                    endif
!*******************************************
!  Charge first derivatives for self-term  *
!*******************************************
                    if (lself.and.lgrad1.and.lDoQDeriv1) then
                      call d1charge(i,j,lopi,lopj,1_i4,derive0self*qlj,derive0self*qli)
                    endif
!
!  If self term, skip rest of working
!
                    if (lself) cycle jloop
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
                    eatom2 = 0.0_dp
                    ereal2 = 0.0_dp
                    ec62 = 0.0_dp
                    dist = sqrt(r2)
                    call twobody1(eatom2,ereal2,ec62,lgrad1p,.false.,.false.,1_i4,1_i4,dist,xji,yji,zji, &
                                  d0i,d0j,deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots, &
                                  npotl,cut2r,cut2q,cut2s,lptrmol,nmolonly,factor,ofct,radsum,rtrm1, &
                                  rtrm2,rtrm3,rtrm32,sctrm1,sctrm2,qli,qlj,lcspair,lewaldtype,.false., &
                                  lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,.false.,lQMMMelectro, &
                                  lorder12,d1i,d1j,d2i2,d2ij,d2j2)
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
                          call qeqbody(eqeq2,lgrad1,.false.,1_i4,1_i4,fct,qli,qlj,nati,natj)
                        elseif (lSandM) then
                          call smbody(eqeq2,lgrad1,.false.,1_i4,1_i4,fct,qli,qlj,nati,natj)
                        endif
                        if (lreg2one) then
                          esregion12 = esregion12 + eqeq2
                        elseif (lreg2pair) then
                          esregion2 = esregion2 + eqeq2
                        else
                          eqeq = eqeq + eqeq2
                        endif
                        if (lattach) eattach = eattach + eqeq2
!
                        eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + esum
!
                        siteenergy(i) = siteenergy(i) + 0.5_dp*eqeq2
                        siteenergy(j) = siteenergy(j) + 0.5_dp*eqeq2
                      endif
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
                      rpd(1) = xji*xji
                      rpd(2) = yji*yji
                      rpd(3) = zji*zji
                      rpd(4) = yji*zji
                      rpd(5) = xji*zji
                      rpd(6) = xji*yji
                    endif
!*****************************
!  Charge first derivatives  *
!*****************************
                    if (lgrad1.and.lDoQDeriv1) then
                      call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
                    endif
!*************************************
!  Electrostatic potential on-sites  *
!*************************************
                    if (lpolar) then
                      vx(i) = vx(i) - qlj*derive*xji
                      vy(i) = vy(i) - qlj*derive*yji
                      vz(i) = vz(i) - qlj*derive*zji
                      vx(j) = vx(j) + qli*derive*xji
                      vy(j) = vy(j) + qli*derive*yji
                      vz(j) = vz(j) + qli*derive*zji
                      if (lattach) then
                        vx12(i) = vx12(i) - qlj*derive*xji
                        vy12(i) = vy12(i) - qlj*derive*yji
                        vz12(i) = vz12(i) - qlj*derive*zji
                        vx12(j) = vx12(j) + qli*derive*xji
                        vy12(j) = vy12(j) + qli*derive*yji
                        vz12(j) = vz12(j) + qli*derive*zji
                      endif
                    endif
!***********************
!  Radial derivatives  *
!***********************
                    if (lgrad1) then
                      if (radi.gt.0.0_dp.and.lopi) then
                        raderv(i) = raderv(i) + rtrm1
                      endif
                      if (radj.gt.0.0_dp.and.lopj) then
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
                      if (lopi) then
                        xdrv(i) = xdrv(i) - deriv*xji
                        ydrv(i) = ydrv(i) - deriv*yji
                        zdrv(i) = zdrv(i) - deriv*zji
                      endif
                      if (lopj) then
                        xdrv(j) = xdrv(j) + deriv*xji
                        ydrv(j) = ydrv(j) + deriv*yji
                        zdrv(j) = zdrv(j) + deriv*zji
                      endif
                      if (nregioni.ne.nregionj) then
                        xregdrv(nregioni) = xregdrv(nregioni) - deriv*xji
                        yregdrv(nregioni) = yregdrv(nregioni) - deriv*yji
                        zregdrv(nregioni) = zregdrv(nregioni) - deriv*zji
                        xregdrv(nregionj) = xregdrv(nregionj) + deriv*xji
                        yregdrv(nregionj) = yregdrv(nregionj) + deriv*yji
                        zregdrv(nregionj) = zregdrv(nregionj) + deriv*zji
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
                        ks = nstrptr(kl)
                        rstrdloc(kl) = rstrdloc(kl) + deriv*rpd(ks)
                        rstrd(kl) = rstrd(kl) + deriv*rpd(ks)
                      enddo
                      if (latomicstress) then
                        do kl = 1,nstrains
                          atomicstress(kl,i) = atomicstress(kl,i) + 0.5_dp*rstrdloc(kl)
                          atomicstress(kl,j) = atomicstress(kl,j) + 0.5_dp*rstrdloc(kl)
                        enddo
                      endif
                    endif
                  endif
!
!  End loop over atom j
!
                enddo jloop
!
!  End loops over neighbouring cells
!
              enddo
            enddo
          enddo
!*********************
!  Radial self term  *
!*********************
          if (lbsmat(nsft+nrelat(i))) then
!
!  Find self term
!
            eatom2 = 0.0_dp
            do n = 1,npote
              if (nptype(n).eq.14) then
                if (lmatch(nati,ntypi,nspec1(n),nptyp1(n),.true.)) then
                  rdiff = radi - twopot(2,n)
                  apt = twopot(1,n)*oci
                  eatom2 = eatom2 + 0.5_dp*apt*rdiff*rdiff
                  if (lgrad1) then
                    raderv(i) = raderv(i) + apt*rdiff
                  endif
                endif
              elseif (nptype(n).eq.17) then
                if (lmatch(nati,ntypi,nspec1(n),nptyp1(n),.true.)) then
                  rdiff = radi - twopot(3,n)
                  apt = twopot(1,n)*oci
                  bpt = twopot(2,n)
                  etrm1 = exp(bpt*rdiff)
                  etrm2 = 1.0_dp/etrm1
                  etrm = apt*(etrm1 + etrm2)
                  eatom2 = eatom2 + etrm
                  if (lgrad1) then
                    raderv(i) = raderv(i) + apt*bpt*(etrm1 - etrm2)
                  endif
                endif
              elseif (nptype(n).eq.31) then
                if (lmatch(nati,ntypi,nspec1(n),nptyp1(n),.true.)) then
                  rdiff = radi - twopot(3,n)
                  apt = twopot(1,n)*oci
                  bpt = twopot(2,n)
                  etrm1 = exp(bpt*rdiff)
                  etrm = apt*etrm1
                  eatom2 = eatom2 + etrm
                  if (lgrad1) then
                    raderv(i) = raderv(i) + apt*bpt*etrm1
                  endif
                endif
              endif
            enddo
!
            eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + eatom2
!
            siteenergy(i) = siteenergy(i) + eatom2
!
!  Set region 2 flag
!
            lreg2pair = .false.
            if (lseok.and.nregions(ncf).ge.2) then
              lreg2pair = (nregionno(nsft+nrelat(i)).eq.2)
            endif
            if (lreg2pair) then
              esregion2 = esregion2 + eatom2
            else
              eatom = eatom + eatom2
            endif
          endif
!
!  End loop over atom i
!
        enddo
!
!  End checks on whether cell is required
!
      endif
!
!  End loop over cells on node
!
    enddo
!****************
!  Global sums  *
!****************
    tsum0 = cputime()
    if (lsuttonc.and.nprocs.gt.1) then
      if (.not.lMEAMden) then
        do i = 1,numat
          sum2(i) = scrho(1,i) 
        enddo
        call sumall(sum2,sum,numat,"realmd3","scrho")
        do i = 1,numat
          scrho(1,i) = sum(i)
        enddo
        do i = 1,numat
          sum2(i) = scrho12(1,i) 
        enddo
        call sumall(sum2,sum,numat,"realmd3","scrho12")
        do i = 1,numat
          scrho12(1,i) = sum(i)
        enddo
      endif
    endif
    tsuml = cputime() - tsum0
    tsum = tsum + tsuml
  endif
!
!  Closing banner for energy decomposition
!
  if (lPrintTwo) then
    call mpbarrier
    if (ioproc) then
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!
!  Free local memory
!
  deallocate(sum2,stat=status)
  if (status/=0) call deallocate_error('realmd3s','sum2')
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('realmd3s','sum')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realmd3s','npotl')
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1 - tsuml
!
  return
  end
