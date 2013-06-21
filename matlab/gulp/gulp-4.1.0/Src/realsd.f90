  subroutine realsd(eatom,ereal,erecip,ec6,eqeq,lgrad1)
!
!  Subroutine for calculating real space energy and 1st derivatives
!
!  Freezing now included
!
!   3/95 nptrmol replace by logical vector lptrmol
!   3/95 changes for periodic molecules added to lbonded,l2bond
!   8/95 Ewald sum for dispersion added
!   2/97 BSM exponential potential added
!   4/97 Sutton-Chen modifications added
!   1/98 QEq modifications added
!   4/98 ESFF form of Lennard-Jones allowed for
!   1/99 1-4 interaction scaling added
!   3/99 Parallel modifications added
!   7/00 Searching for valid vectors and self-terms moved to subroutines
!   2/01 Structure of rpd changed to suit 2-D case
!   2/01 Calculation of electric field for polarisation added
!   9/01 lmolq calculations accelerated using lneedmol 
!   2/02 lneedmol algorithm corrected
!   5/02 BSM made core/shell specific
!  10/02 ReaxFF modifications added
!  11/02 Wildcard atoms added
!  11/02 Parallel changes made
!   1/03 Wolf modifications made
!   1/03 Call to selfterm altered
!   3/03 Handling of electric field calculation corrected
!   9/03 eqeq argument to qeqbody corrected to eqeql
!   1/04 Bug in breathing shell self-term energy/forces corrected
!   6/04 nreli.ne.j condition removed from lmolok 
!   9/04 Charge first derivatives added
!   1/05 rp no longer sqrt'd and passed to rsearch routines
!   4/05 Mods for cosh-spring added
!   7/05 Streitz and Mintmire modifications added
!   7/05 Error in size of sum when lsuttonc is true fixed
!   3/07 Printing of twobody energies added as an option
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody argument list
!   4/09 MEAM density modifications removed since these are now handled in 
!        a separate routine.
!   6/09 Modified for charge as a coordinate option
!  10/09 Modified to allow for situation where nouterloop is too large to compute
!   3/10 Goto tos replaced by cycle - first loop goto was incorrect
!  11/11 Region-region energy contributions stored
!   1/12 Missing addition of breathing shell self energy added back in
!   5/12 Atomic stresses added
!   5/12 Atomic stresses removed for routines involving symmetry
!   4/13 Bug in breathing shell self-term contribution fixed
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, April 2013
!
  use configurations, only : lbsmat, nregionno
  use constants
  use control
  use current
  use datatypes
  use derivatives
  use eam,            only : lMEAMden
  use element
  use energies,       only : eregion2region
  use general,        only : cutw
  use iochannels,     only : ioout
  use kspace
  use molecule
  use optimisation
  use parallel
  use polarise
  use potentialxyz
  use realvectors
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
  real(dp),    intent(inout)                   :: ec6
  real(dp),    intent(inout)                   :: eqeq
  real(dp),    intent(inout)                   :: ereal
  real(dp),    intent(inout)                   :: erecip
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifree
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ix
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: j
  integer(i4)                                  :: jx
  integer(i4)                                  :: k
  integer(i4)                                  :: kl
  integer(i4)                                  :: ks
  integer(i4)                                  :: m
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
  integer(i4)                                  :: nfree
  integer(i4)                                  :: nout
  integer(i4)                                  :: nouterloop
  integer(i4), dimension(:), allocatable       :: nptr
  integer(i4)                                  :: nreli
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical                                      :: lc6loc
  logical                                      :: lcspair
  logical                                      :: lewaldtype
  logical                                      :: lgrad1p
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lorder12
  logical                                      :: lself
  logical                                      :: lsg1
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
  real(dp)                                     :: d2self
  real(dp)                                     :: derive0self
  real(dp)                                     :: dfct
  real(dp)                                     :: eatoml
  real(dp)                                     :: ereall
  real(dp)                                     :: ec6l
  real(dp)                                     :: eqeql
  real(dp)                                     :: etrm1
  real(dp)                                     :: etrm2
  real(dp)                                     :: factor
  real(dp)                                     :: fct
  real(dp)                                     :: fcti
  real(dp)                                     :: ffct
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ofct
  real(dp)                                     :: ofct2
  real(dp)                                     :: qli
  real(dp)                                     :: qlj
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rdiff
  real(dp)                                     :: rneqi
  real(dp)                                     :: rp
  real(dp)                                     :: rqeq2
  real(dp)                                     :: rtrm1
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp),    dimension(:), allocatable       :: sum
  real(dp),    dimension(:), allocatable       :: sum2
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tsum0
  real(dp)                                     :: tsuml
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  time1 = cputime()
!
!  Initialise local variables
!
  rqeq2 = rqeq*rqeq
  lsg1 = (lgrad1.and.lstr)
  lc6loc = (lc6.and.ndim.eq.3)
  lgrad1p = (lgrad1.or.lpolar)
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realsd','npotl')
  allocate(nptr(nasym),stat=status)
  if (status/=0) call outofmemory('realsd','nptr')
  allocate(sum(max(nstrains,numat)),stat=status)
  if (status/=0) call outofmemory('realsd','sum')
  allocate(sum2(max(nstrains,numat)),stat=status)
  if (status/=0) call outofmemory('realsd','sum2')
!
!  Set the Coulomb term type based on dimensionality :
!
!  1-D         => 1/r
!  2-D and 3-D => erfc(seta*r)/r
!
  lewaldtype = (ndim.ne.1.or.lwolf)
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
!
!  Generate pointer to non-frozen atoms
!
  if (lfreeze) then
    nfree = 0
    do i = 1,nasym
      if (lopf(i)) then
        nfree = nfree + 1
        nptr(nfree) = i
      endif
    enddo
    ffct = 1.0_dp
  else
    nfree = nasym
    do i = 1,nasym
      nptr(i) = i
    enddo
    ffct = 0.5_dp
  endif
!***************************************************************
!  Atomistic and real space electrostatic component of energy  *
!***************************************************************
  if (numat.gt.i4_limit) then
!
!  If nsite*numat is greater than integer precision will allow change algorithm
!
    do ifree = 1,nfree
      i = nptr(ifree)
      lopi = lopf(i)
!
!  Inner loop over second site
!
      xal = xalat(i)
      yal = yalat(i)
      zal = zalat(i)
      nati = iatn(i)
      ntypi = natype(i)
      qli = qa(i)
      oci = occua(i)
      if (lbsmat(nsft+i)) then
        radi = rada(i)
      else
        radi = 0.0_dp
      endif
      fcti = oci*dble(neqv(i))
      if (abs(fcti).gt.1.0d-15) then
        rneqi = 1.0_dp/fcti
      else
        rneqi = 1.0_dp/dble(neqv(i))
      endif
      nreli = nrel2(i)
      nregioni = nregionno(nsft+i)
!
!  Molecule handling
!
      if (lmol) then
        nmi = natmol(nreli)
        indm = nmolind(nreli)
        call mindtoijk(indm,ixi,iyi,izi)
      endif
!
!  Start of second atom loop
!
      jloop: do j = procid+1,numat,nprocs
        natj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft + nrelat(j))
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
          ntyp2 = ntypj
        else
          lorder12 = .false.
          nat1 = nat(j)
          nat2 = nati
          ntyp1 = ntypj
          ntyp2 = ntypi
        endif
!
!  Freeze flag
!
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        ofct = fcti
        dfct = 1.0_dp
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
        ofct = ofct*ocj
        fct = ofct*angstoev
        factor = qli*qlj*fct
!
!  Possible core-shell pair flag
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
          call rsearch3D(xcrd,ycrd,zcrd,lmolok,lcspair,nreli,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.2) then
          call rsearch2D(xcrd,ycrd,zcrd,lmolok,lcspair,nreli,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.1) then
          call rsearch1D(xcrd,ycrd,zcrd,lmolok,lcspair,nreli,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        endif
!
        if (lself) then
!**************
!  Self-term  *
!**************
          ec6l = 0.0_dp
          derive0self = 0.0_dp
          if (lfreeze) then
            call selfterm(ereal,erecip,ec6l,derive0self,factor,fct,ofct,dfct,npotl,npots,c6tot,d2self, &
                          lgrad,.false.,nreli,j,ix,jx,1.0_dp,1.0_dp,.true.,qli,qlj)
          else
            call selfterm(ereal,erecip,ec6l,derive0self,factor,fct,ofct,dfct,npotl,npots,c6tot,d2self, &
                          lgrad,.false.,nreli,j,ix,jx,0.5_dp,1.0_dp,.true.,qli,qlj)
          endif
          ec6 = ec6 + ffct*ec6l
!
          eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + ffct*ec6l
!******************************************
!  Charge first derivatives of self-term  *
!******************************************
!            if (lgrad1.and.lDoQDeriv1) then
!              call d1charges(i,lopi,1_i4,derive0self*qlj*dble(neqv(i)))
!            endif
        endif
        if (nor.eq.0) cycle jloop
!**********************************
!  Evaluate twobody contribution  *
!**********************************
        if (lfreeze.and.lopj) then
          ofct2 = ofct*0.5_dp
          dfct = 2.0_dp
        else
          ofct2 = ofct
          dfct = 1.0_dp
        endif
        eatoml = 0.0_dp
        ereall = 0.0_dp
        ec6l = 0.0_dp
!
!  Sqrt distances
!
        do k = 1,nor
          dist(k) = sqrt(dist(k))
        enddo
!
        call twobody(eatoml,ereall,ec6l,lgrad1p,.false.,.false.,nor,1_i4,npots,npotl,cut2r,cut2q,cut2s, &
                     nmolonly,factor,ofct2,radsum,rtrm1,sctrm1,sctrm2,qli,qlj,lcspair,lewaldtype, &
                     .false.,.false.,.false.,lorder12)
        eatom = eatom + ffct*eatoml
        ereal = ereal + ffct*ereall
        ec6 = ec6 + ffct*ec6l
        eqeql = 0.0_dp
        if (leem) then
          if (lqeq) then
            call qeqbody(eqeql,lgrad1,.false.,nor,1_i4,fct,qli,qlj,nati,natj)
          elseif (lSandM) then
            call smbody(eqeql,lgrad1,.false.,nor,1_i4,fct,qli,qlj,nati,natj)
          endif
          eqeq = eqeq + ffct*eqeql
        endif
!
        eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + ffct*(eatoml+ereall+ec6l+eqeql)
!
        if (lPrintTwo) then
          write(ioout,'(4x,i12,i12,f22.10,1x,f22.10)') nreli,j,ffct*(eatoml+ec6l),ffct*ereall
        endif
!
        if (lsuttonc) then
          if (.not.lMEAMden) then
            if (lorder12) then
              scrho(1,i) = scrho(1,i) + sctrm1*ocj
            else
              scrho(1,i) = scrho(1,i) + sctrm2*ocj
            endif
          endif
        endif
        if (lgrad1) then
          do k = 1,nor
            deriv(k) = dfct*deriv(k)
          enddo
        endif
        if (lsg1) then
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
        endif
!*************************************
!  Electrostatic potential on-sites  *
!*************************************
        if (lpolar.and.j.ne.nreli) then
          do k = 1,nor
            vx(i) = vx(i) - qlj*derive(k)*xtmp(k)*rneqi
            vy(i) = vy(i) - qlj*derive(k)*ytmp(k)*rneqi
            vz(i) = vz(i) - qlj*derive(k)*ztmp(k)*rneqi
          enddo
        endif
!***********************
!  Radial derivatives  *
!***********************
        if (lbsmat(nsft+i).and.lgrad1) then
          raderv(i) = raderv(i) + dfct*rtrm1
        endif
!************************
!  Internal Derivatives *
!************************
!
!  First derivatives
!
        if (lgrad1.and.(lopi.or..not.lfreeze)) then
          do k = 1,nor
            xdrv(i) = xdrv(i) - deriv(k)*xtmp(k)
            ydrv(i) = ydrv(i) - deriv(k)*ytmp(k)
            zdrv(i) = zdrv(i) - deriv(k)*ztmp(k)
          enddo
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
          do kl = 1,nstrains
            ks = nstrptr(kl)
            do k = 1,nor
              rstrd(kl) = rstrd(kl) + ffct*deriv(k)*rpd(k,ks)
            enddo
          enddo
        endif
      enddo jloop
    enddo
  else
!
!  Combine i/j loops into one for improved parallel efficiency
!
    nouterloop = nfree*numat
    noutloop: do nout = procid+1,nouterloop,nprocs
      ifree = ((nout-1)/numat)+1
      i = nptr(ifree)
      lopi = lopf(i)
!
!  Inner loop over second site
!
      xal = xalat(i)
      yal = yalat(i)
      zal = zalat(i)
      nati = iatn(i)
      ntypi = natype(i)
      qli = qa(i)
      oci = occua(i)
      if (lbsmat(nsft+i)) then
        radi = rada(i)
      else
        radi = 0.0_dp
      endif
      fcti = oci*dble(neqv(i))
      if (abs(fcti).gt.1.0d-15) then
        rneqi = 1.0_dp/fcti
      else
        rneqi = 1.0_dp/dble(neqv(i))
      endif
      nreli = nrel2(i)
      nregioni = nregionno(nsft+i)
!
!  Molecule handling
!
      if (lmol) then
        nmi = natmol(nreli)
        indm = nmolind(nreli)
        call mindtoijk(indm,ixi,iyi,izi)
      endif
!
!  Start of second atom loop
!
      j = nout - (ifree-1)*numat
!orig   do j=1,numat
      natj = nat(j)
      ntypj = nftype(j)
      nregionj = nregionno(nsft+nrelat(j))
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
        ntyp2 = ntypj
      else
        lorder12 = .false.
        nat1 = nat(j)
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
!
!  Freeze flag
!
      lopj = (.not.lfreeze.or.lopf(nrelat(j)))
      ofct = fcti
      dfct = 1.0_dp
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
      ofct = ofct*ocj
      fct = ofct*angstoev
      factor = qli*qlj*fct
!
!  Possible core-shell pair flag
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
      if (npots.eq.0.and.abs(factor).lt.1.0d-8) cycle noutloop
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
        call rsearch3D(xcrd,ycrd,zcrd,lmolok,lcspair,nreli,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
      elseif (ndim.eq.2) then
        call rsearch2D(xcrd,ycrd,zcrd,lmolok,lcspair,nreli,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
      elseif (ndim.eq.1) then
        call rsearch1D(xcrd,ycrd,zcrd,lmolok,lcspair,nreli,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
      endif
!
      if (lself) then
!**************
!  Self-term  *
!**************
        ec6l = 0.0_dp
        derive0self = 0.0_dp
        if (lfreeze) then
          call selfterm(ereal,erecip,ec6l,derive0self,factor,fct,ofct,dfct,npotl,npots,c6tot,d2self, &
                        lgrad,.false.,nreli,j,ix,jx,1.0_dp,1.0_dp,.true.,qli,qlj)
        else
          call selfterm(ereal,erecip,ec6l,derive0self,factor,fct,ofct,dfct,npotl,npots,c6tot,d2self, &
                        lgrad,.false.,nreli,j,ix,jx,0.5_dp,1.0_dp,.true.,qli,qlj)
        endif
        ec6 = ec6 + ffct*ec6l
!
        eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + ffct*ec6l
!******************************************
!  Charge first derivatives of self-term  *
!******************************************
!            if (lgrad1.and.lDoQDeriv1) then
!              call d1charges(i,lopi,1_i4,derive0self*qlj*dble(neqv(i)))
!            endif
      endif
      if (nor.eq.0) cycle noutloop
!**********************************
!  Evaluate twobody contribution  *
!**********************************
      if (lfreeze.and.lopj) then
        ofct2 = ofct*0.5_dp
        dfct = 2.0_dp
      else
        ofct2 = ofct
        dfct = 1.0_dp
      endif
      eatoml = 0.0_dp
      ereall = 0.0_dp
      ec6l = 0.0_dp
!
!  Sqrt distances
!
      do k = 1,nor
        dist(k) = sqrt(dist(k))
      enddo
!
      call twobody(eatoml,ereall,ec6l,lgrad1p,.false.,.false.,nor,1_i4,npots,npotl,cut2r,cut2q,cut2s, &
                   nmolonly,factor,ofct2,radsum,rtrm1,sctrm1,sctrm2,qli,qlj,lcspair,lewaldtype, &
                   .false.,.false.,.false.,lorder12)
      eatom = eatom + ffct*eatoml
      ereal = ereal + ffct*ereall
      ec6 = ec6 + ffct*ec6l
      eqeql = 0.0_dp
      if (leem) then
        if (lqeq) then
          call qeqbody(eqeql,lgrad1,.false.,nor,1_i4,fct,qli,qlj,nati,natj)
        elseif (lSandM) then
          call smbody(eqeql,lgrad1,.false.,nor,1_i4,fct,qli,qlj,nati,natj)
        endif
        eqeq = eqeq + ffct*eqeql
      endif
!
      eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + ffct*(eatoml+ereall+ec6l+eqeql)
!
      if (lPrintTwo) then
        write(ioout,'(4x,i12,i12,f22.10,1x,f22.10)') nreli,j,ffct*(eatoml+ec6l),ffct*ereall
      endif
!
      if (lsuttonc) then
        if (.not.lMEAMden) then
          if (lorder12) then
            scrho(1,i) = scrho(1,i) + sctrm1*ocj
          else
            scrho(1,i) = scrho(1,i) + sctrm2*ocj
          endif
        endif
      endif
      if (lgrad1) then
        do k = 1,nor
          deriv(k) = dfct*deriv(k)
        enddo
      endif
      if (lsg1) then
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
      endif
!*************************************
!  Electrostatic potential on-sites  *
!*************************************
      if (lpolar.and.j.ne.nreli) then
        do k = 1,nor
          vx(i) = vx(i) - qlj*derive(k)*xtmp(k)*rneqi
          vy(i) = vy(i) - qlj*derive(k)*ytmp(k)*rneqi
          vz(i) = vz(i) - qlj*derive(k)*ztmp(k)*rneqi
        enddo
      endif
!***********************
!  Radial derivatives  *
!***********************
      if (lbsmat(nsft+i).and.lgrad1) then
        raderv(i) = raderv(i) + dfct*rtrm1
      endif
!************************
!  Internal Derivatives *
!************************
!
!  First derivatives
!
      if (lgrad1.and.(lopi.or..not.lfreeze)) then
        do k = 1,nor
          xdrv(i) = xdrv(i) - deriv(k)*xtmp(k)
          ydrv(i) = ydrv(i) - deriv(k)*ytmp(k)
          zdrv(i) = zdrv(i) - deriv(k)*ztmp(k)
        enddo
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
        do kl = 1,nstrains
          ks = nstrptr(kl)
          do k = 1,nor
            rstrd(kl) = rstrd(kl) + ffct*deriv(k)*rpd(k,ks)
          enddo
        enddo
      endif
    enddo noutloop
  endif
  do i = procid+1,nasym,nprocs
!
!  Breathing shell self term
!
    if (lbsmat(nsft+i)) then
      nati = iatn(i)
      ntypi = natype(i)
      fcti = occua(i)*dble(neqv(i))
      radi = rada(i)
      eatoml = 0.0_dp
      do m = 1,npote
        if (nptype(m).eq.14) then
          if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
            apt = twopot(1,m)*fcti
            rdiff = radi - twopot(2,m)
            if (lfreeze) then
              eatoml = eatoml + 0.5_dp*apt*rdiff*rdiff*ffct
            else
              eatoml = eatoml + apt*rdiff*rdiff*ffct
            endif
            if (lgrad1) raderv(i) = raderv(i) + apt*rdiff
          endif
        elseif (nptype(m).eq.17) then
          if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
            apt = twopot(1,m)*fcti
            bpt = twopot(2,m)
            rdiff = radi - twopot(3,m)
            etrm1 = exp(bpt*rdiff)
            etrm2 = 1.0_dp/etrm1
            if (lfreeze) then
              eatoml = eatoml + apt*ffct*(etrm1 + etrm2)
            else
              eatoml = eatoml + 2.0_dp*apt*ffct*(etrm1 + etrm2)
            endif
            if (lgrad1) raderv(i) = raderv(i) + apt*bpt*(etrm1 - etrm2)
          endif
        elseif (nptype(m).eq.31) then
          if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
            apt = twopot(1,m)*fcti
            bpt = twopot(2,m)
            rdiff = radi - twopot(3,m)
            etrm1 = exp(bpt*rdiff)
            if (lfreeze) then
              eatoml = eatoml + apt*ffct*etrm1
            else
              eatoml = eatoml + 2.0_dp*apt*ffct*etrm1
            endif
            if (lgrad1) raderv(i) = raderv(i) + apt*bpt*etrm1
          endif
        endif
      enddo
!
      eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + eatoml
      eatom = eatom + eatoml
    endif
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
      call sumall(sum2,sum,numat,"realsd","scrho")
      do i = 1,numat
        scrho(1,i) = sum(i)
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
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(sum2,stat=status)
  if (status/=0) call deallocate_error('realsd','sum2')
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('realsd','sum')
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('realsd','nptr')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realsd','npotl')
!
!  Timing
!
  time2 = cputime()
  trls = trls + time2 - time1 - tsuml
!
  return
  end
