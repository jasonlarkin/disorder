  subroutine realmd0(eatom,ereal,eqeq,lgrad1)
!
!  Subroutine for calculating the real space energy
!  for a finite cluster => 0 D system for MD
!
!   3/99 Created from real0d.f
!   3/99 Parallel modifications added
!   5/00 Calculation of electric field added for polarisation 
!   8/00 Error in parallelisation of derivative sum corrected
!   9/01 lmolq calculations accelerated using lneedmol 
!   2/02 lneedmol algorithm corrected
!  10/02 ReaxFF modifications added
!  11/02 Wildcard atoms added
!  11/02 Parallel changes made
!   4/04 Bug in handling of nati for shells in BSM corrected
!   4/05 Mods for cosh-spring added
!   7/05 Streitz and Mintmire modifications added
!   2/07 Bonding types added
!   3/07 Printing of twobody energies added as an option
!   3/07 Bonding types modified
!   5/07 QM/MM scheme added
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!  11/08 x/y/z components passed to twobody1
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!   4/09 Globalisation of scrho values now only done if nprocs=1
!   4/09 MEAM density modifications removed since these are now handled in 
!        a separate routine.
!   6/09 Site energy added
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
!   4/12 xvir, yvir and zvir removed
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
!  Julian Gale, NRI, Curtin University, April 2012
!
  use configurations, only : lbsmat, nregionno, nregiontype, QMMMmode
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
  use molecule
  use optimisation
  use parallel
  use polarise
  use potentialxyz
  use shell
  use sutton
  use times
  use two
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lgrad1
  real(dp),    intent(inout)                   :: eatom
  real(dp),    intent(inout)                   :: eqeq
  real(dp),    intent(inout)                   :: ereal
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: iar
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
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
  logical                                      :: l2bonds
  logical                                      :: l3bonds
  logical                                      :: lbonded
  logical                                      :: lbreathe
  logical                                      :: lcspair
  logical                                      :: lgrad1p
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lopi
  logical                                      :: lopj
  logical                                      :: lorder12
  logical                                      :: lptrmol
  logical                                      :: lQMMMelectro
  real(dp)                                     :: apt
  real(dp)                                     :: bpt
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2e
  real(dp)                                     :: cut2p
  real(dp)                                     :: cut2r
  real(dp)                                     :: cut2s
  real(dp)                                     :: d0i
  real(dp)                                     :: d0j
  real(dp)                                     :: d1i
  real(dp)                                     :: d1j
  real(dp)                                     :: d2i2
  real(dp)                                     :: d2ij
  real(dp)                                     :: d2j2
  real(dp)                                     :: deltae
  real(dp)                                     :: deriv
  real(dp)                                     :: deriv2
  real(dp)                                     :: deriv3
  real(dp)                                     :: derive0
  real(dp)                                     :: derive
  real(dp)                                     :: derive2
  real(dp)                                     :: derive3
  real(dp)                                     :: dist
  real(dp)                                     :: eatom_before
  real(dp)                                     :: ec6
  real(dp)                                     :: ec6_before
  real(dp)                                     :: eqeq_before
  real(dp)                                     :: ereal_before
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
  real(dp)                                     :: r
  real(dp)                                     :: radi
  real(dp)                                     :: radj
  real(dp)                                     :: radsum
  real(dp)                                     :: rderiv
  real(dp)                                     :: rdiff
  real(dp)                                     :: rp
  real(dp)                                     :: rtrm1
  real(dp)                                     :: rtrm2
  real(dp)                                     :: rtrm3
  real(dp)                                     :: rtrm32
  real(dp)                                     :: sctrm1
  real(dp)                                     :: sctrm2
  real(dp)                                     :: small
  real(dp)                                     :: small2
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
  tsuml = 0.0_dp
!
!  Initialise local variables
!
  lgrad1p = (lgrad1.or.lpolar)
  small = 1.0d-12
  small2 = 1.0d-2
  ec6 = 0.0_dp
!
!  Allocate local memory
!
  allocate(npotl(npote),stat=status)
  if (status/=0) call outofmemory('realmd0','npotl')
  allocate(sum(numat),stat=status)
  if (status/=0) call outofmemory('realmd0','sum')
  allocate(sum2(numat),stat=status)
  if (status/=0) call outofmemory('realmd0','sum2')
!
!  Set up cutoffs
!
  if (lwolf) then
    cut2e = cutw*cutw
  else
    cut2e = 1.0d10
  endif
  cut2p = cutp*cutp
  cut2s = cuts*cuts
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
!orig do i=2,numat
  do i = procid+1,numat,nprocs
    if (i.gt.noff) then
      noffset = noffm1
    endif
    xal = xclat(i)
    yal = yclat(i)
    zal = zclat(i)
    nati = nat(i)
    ntypi = nftype(i)
    nregioni = nregionno(nsft+i)
    nregiontypi = nregiontype(nregioni,ncf)
    qli = qf(i)
    oci = occuf(i)
    lopi = (.not.lfreeze.or.lopf(i))
    if (lbsmat(i+nsft)) then
      radi = radf(i)
    else
      radi = 0.0_dp
    endif
!
!  Molecule handling
!
    if (lmol) then
      nmi = natmol(i)
      indmi = nmolind(i)
    endif
!
!  Inner loop over second site
!
!orig do j=1,i-1
    jloop: do m = 1,noffset
      j = mod(i+m-1_i4,numat) + 1
      lopj = (.not.lfreeze.or.lopf(j))
      if (.not.lopi.and..not.lopj) cycle jloop
      nregionj = nregionno(nsft+j)
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
      natj = nat(j)
      ntypj = nftype(j)
      qlj = qf(j)
      ocj = occuf(j)
      if (lbsmat(nsft+j)) then
        radj = radf(j)
      else
        radj = 0.0_dp
      endif
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      radsum = radi + radj
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
        nat2 = natj
        ntyp1 = ntypi
        ntyp2 = ntypj
      else
        lorder12 = .false.
        nat1 = natj
        nat2 = nati
        ntyp1 = ntypj
        ntyp2 = ntypi
      endif
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
      endif
!
!  Locate potential number
!  Check whether potential requires specific types
!
      rp = 0.0_dp
      npots = 0
      lneedmol = (lmol.and..not.lmolq)
      do n = 1,npote
        if (lmatch(nat1,ntyp1,nspec1(n),nptyp1(n),.true.)) then
          if (lmatch(nat2,ntyp2,nspec2(n),nptyp2(n),.true.)) then
            npots = npots + 1
            npotl(npots) = n
            if (mmexc(n).gt.0.or.(lintra(n).and..not.linter(n).or..not.lintra(n).and.linter(n))) lneedmol = .true.
            if (nptype(n).eq.8.or.nptype(n).eq.33) then
              if (cuts.gt.rp) rp = cuts
            else
              if (rpot(n).gt.rp) rp = rpot(n)
            endif
          endif
        endif
      enddo
!
!  Generate looping indices
!
      cut2r = rp*rp
      if (cut2r.gt.cut2p) cut2r = cut2p
      r = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
!
!  Molecule and bonding checks
!
      if (lmol) then
        lmolok = (nmi.eq.nmj.and.nmi.ne.0)
      else
        lmolok = .false.
      endif
!  
!  If the Coulomb term and the potentials don't need molecule info for
!  for this pair of atoms, then turn off molecule checking to save time
!
      if (.not.lneedmol) lmolok = .false.
      if (lmolok.and.(r.gt.cut2s.or..not.lcspair)) then
        ind = indmj - indmi
        lptrmol = (ind.eq.0)
        if (.not.lptrmol) then
          call mindtoijk(indmj,jxx,jyy,jzz)
          call mindtoijk(indmi,ixx,iyy,izz)
          jxx = jxx - ixx
          jyy = jyy - iyy
          jzz = jzz - izz
          call samemol(lptrmol,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
        endif
        if (lptrmol) then
          call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
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
      if (abs(r-small2).lt.1.0d-12) r = small2
      if (r.lt.small) then
        cycle jloop
      else
!
!  Store vector
!
        nor = 1
        dist = sqrt(r)
      endif
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
      eatom_before = eatom
      ereal_before = ereal
      ec6_before   = ec6
      call twobody1(eatom,ereal,ec6,lgrad1p,.false.,.false.,nor,1_i4,dist,xcrd,ycrd,zcrd,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,npots,npotl, &
                    cut2r,cut2e,cut2s,lptrmol,0_i4,factor,ofct,radsum,rtrm1,rtrm2,rtrm3,rtrm32, &
                    sctrm1,sctrm2,qli,qlj,lcspair,.false.,.false.,lbonded,l2bonds,l3bonds, &
                    nbtypeij,nbtypeij2,.false.,lQMMMelectro,lorder12,d1i,d1j,d2i2,d2ij,d2j2)
!
      deltae = (eatom + ereal + ec6 - eatom_before - ec6_before - ereal_before)
!
      eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + deltae
!
      siteenergy(i) = siteenergy(i) + 0.5_dp*deltae
      siteenergy(j) = siteenergy(j) + 0.5_dp*deltae
      if (lPrintTwo) then
        write(ioout,'(4x,i12,i12,f22.10,1x,f22.10)') i,j,eatom+ec6-eatom_before-ec6_before,ereal-ereal_before
      endif
!
      if (lDoQDeriv1.and.lgrad1) then
        d0i = d0i + derive0*qlj
        d0j = d0j + derive0*qli
      endif
      if (leem) then
        eqeq_before = eqeq
        if (lqeq) then
          call qeqbody1(eqeq,lgrad1,.false.,nor,1_i4,dist,deriv, &
            deriv2,fct,qli,qlj,nati,natj,d1i,d1j,d2i2,d2ij,d2j2)
        elseif (lSandM) then
          call smbody1(eqeq,lgrad1,.false.,nor,1_i4,dist,deriv, &
            deriv2,fct,qli,qlj,nati,natj,d1i,d1j,d2i2,d2ij,d2j2)
        endif
        deltae = (eqeq - eqeq_before)
!
        eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + deltae
!
        siteenergy(i) = siteenergy(i) + 0.5_dp*deltae
        siteenergy(j) = siteenergy(j) + 0.5_dp*deltae
      endif
      if (lsuttonc) then
        if (.not.lMEAMden) then
          if (lorder12) then
            scrho(1,i) = scrho(1,i) + ocj*sctrm1
            scrho(1,j) = scrho(1,j) + oci*sctrm2
          else
            scrho(1,i) = scrho(1,i) + ocj*sctrm2
            scrho(1,j) = scrho(1,j) + oci*sctrm1
          endif
        endif
      endif
!*****************************
!  Charge first derivatives  * 
!*****************************
      if (lgrad1.and.lDoQDeriv1) then
        call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
      endif
!***********************
!  Radial derivatives  *
!***********************
      if (lgrad1) then
        if (lopi.and.radi.gt.0.0_dp) then
          raderv(i) = raderv(i) + rtrm1
        endif
        if (radj.gt.0.0_dp) then
          raderv(j) = raderv(j) + rtrm1
        endif
      endif
!*************************************
!  Electrostatic potential on-sites  *
!*************************************
      if (lpolar) then
        vx(i) = vx(i) - qlj*derive*xcrd
        vy(i) = vy(i) - qlj*derive*ycrd
        vz(i) = vz(i) - qlj*derive*zcrd
        vx(j) = vx(j) + qli*derive*xcrd
        vy(j) = vy(j) + qli*derive*ycrd
        vz(j) = vz(j) + qli*derive*zcrd
      endif
!**************************
!  Coordinate Derivatives *
!**************************
!
!  First derivatives
!
      if (lgrad1) then
        xdrv(i) = xdrv(i) - deriv*xcrd
        ydrv(i) = ydrv(i) - deriv*ycrd
        zdrv(i) = zdrv(i) - deriv*zcrd
        xdrv(j) = xdrv(j) + deriv*xcrd
        ydrv(j) = ydrv(j) + deriv*ycrd
        zdrv(j) = zdrv(j) + deriv*zcrd
!
        if (nregioni.ne.nregionj) then
          xregdrv(nregioni) = xregdrv(nregioni) - deriv*xcrd
          yregdrv(nregioni) = yregdrv(nregioni) - deriv*ycrd
          zregdrv(nregioni) = zregdrv(nregioni) - deriv*zcrd
          xregdrv(nregionj) = xregdrv(nregionj) + deriv*xcrd
          yregdrv(nregionj) = yregdrv(nregionj) + deriv*ycrd
          zregdrv(nregionj) = zregdrv(nregionj) + deriv*zcrd
        endif
      endif
!
!  Skip to here if frozen pair
!
    enddo jloop
  enddo
!
!  Breathing shell self terms
!
  iloop: do i = 1,numat
    lopi = (.not.lfreeze.or.lopf(i))
    nregioni = nregionno(nsft+i)
    nregiontypi = nregiontype(nregioni,ncf)
!  
!  QM/MM handling : i is a QM atom => exclude
!       
    if (QMMMmode(ncf).gt.0) then
      if (nregiontypi.eq.1) cycle iloop
    endif
!           
!  QM/MM : Set electrostatic embedding flag : If i is a QM atom => exclude electrostatics
!       
    lQMMMelectro = (QMMMmode(ncf).eq.2.and.nregiontypi.eq.1)
    if (lopi) then
      iar = nsft + nrelat(i)
      lbreathe = lbsmat(iar)
      if (lbreathe) then
        nati = nat(i)
        ntypi = nftype(i)
        oci = occuf(i)
        radi = radf(i)
!******************************
!  Breathing shell self term  *
!******************************
        eatom_before = eatom
        do m = 1,npote
          if (nptype(m).eq.14) then
            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
              apt = twopot(1,m)*oci
              rdiff = radi - twopot(2,m)
              eatom = eatom + 0.5_dp*apt*rdiff*rdiff
              if (lgrad1) then
                raderv(i) = raderv(i) + apt*rdiff
              endif
            endif
          elseif (nptype(m).eq.17) then
            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
              apt = twopot(1,m)*oci
              bpt = twopot(2,m)
              rdiff = radi - twopot(3,m)
              etrm1 = exp(bpt*rdiff)
              etrm2 = 1.0_dp/etrm1
              etrm = apt*(etrm1 + etrm2)
              eatom = eatom + etrm
              if (lgrad1) then
                raderv(i) = raderv(i) + apt*bpt*(etrm1 - etrm2)
              endif
            endif
          elseif (nptype(m).eq.31) then
            if (lmatch(nati,ntypi,nspec1(m),nptyp1(m),.true.)) then
              apt = twopot(1,m)*oci
              bpt = twopot(2,m)
              rdiff = radi - twopot(3,m)
              etrm1 = exp(bpt*rdiff)
              etrm = apt*etrm1
              eatom = eatom + etrm
              if (lgrad1) then
                raderv(i) = raderv(i) + apt*bpt*etrm1
              endif
            endif
          endif
        enddo
        deltae = (eatom - eatom_before)
!
        eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + deltae
!
        siteenergy(i) = siteenergy(i) + deltae
      endif
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
      call sumall(sum2,sum,numat,"realmd0","scrho")
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
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
  endif
!
!  End of real space part - perform general tasks
!
999 continue
!
!  Add any C6 terms to ereal
!
  ereal = ereal + ec6
!
!  Free local memory
!
  deallocate(sum2,stat=status)
  if (status/=0) call deallocate_error('realmd0','sum2')
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('realmd0','sum')
  deallocate(npotl,stat=status)
  if (status/=0) call deallocate_error('realmd0','npotl')
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1 - tsuml
!
  return
  end
