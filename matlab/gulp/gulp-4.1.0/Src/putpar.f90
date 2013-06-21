  subroutine putpar(xc,ncfold,xf,lput)
!
!  Updates parameters in main arrays from those in xc
!
!  Called by fit and fitfun.
!
!  xc     = array of current parameter multipliers
!  xf     = final parameter values
!  ncfold = last configuration for setup
!  lput   = logical flag, if .true. then final value of parameters are
!           returned in xf, otherwise xf is unchanged
!
!  Fitting variable codes :
!
!   nftyp = variable type
!           1 = general
!           2 = two-body potential
!           3 = three-body potential
!           4 = four-body potential
!           5 = many-body potential
!           6 = bond order potential
!           7 = six-body potential
!           8 = ReaxFF potential
!           9 = EDIP potential
!
!   nfpot = potential number / variable type within general catagory
!           1 = shell coordinate
!           2 = breathing shell radius
!           3 = shift
!           4 = charge
!           5 = dipolar polarisability
!           6 = split
!           7 = epsilon for combination rules
!           8 = sigma for combination rules
!           9 = A for combination rules
!          10 = B for combination rules
!          11 = EEM chi
!          12 = EEM mu
!          13 = QEq chi
!          14 = QEq mu
!          15 = SM chi
!          16 = SM mu
!          17 = SM zeta
!          18 = SM Znuc
!          19 = QEq radius
!          20 = UFF radius
!          21 = UFF theta
!          22 = UFF distance
!          23 = UFF energy
!          24 = UFF zeta
!          25 = UFF effective charge
!          26 = UFF torsion
!          27 = Bond charge increment added
!          28 = Plane potential A
!          29 = Plane potential B
!          30 = UFF Koop
!          31 = UFF theta oop
!          32 = UFF chi
!          38 = 1-body self energy
!
!   nfvar = variable number within potential type or species for general case
!           For two-body potentials, nfvar < 10 => reference to twopot(nfvar,)
!                                    nfvar >=10 => reference to tpot(nfvar-9,)
!
!   6/95 Constraints removed as they have been added to setup
!   7/95 Code numbers changed due to standardisation of twobody
!        potentials to same systems as other variables
!  11/96 if lc6one, setc6 must be called again otherwise c6spec
!        is not updated
!   7/97 EAM parameters added
!   6/98 fitting type 17 assigned to three-body parameter
!   6/98 threebody polynomial coefficients added
!  10/98 Coding of fitting variables simplified
!  11/03 Bond order potential data added
!  12/03 Assignment of bond order potential number fixed
!   5/04 lmodco option introduced
!   3/04 labs option extended to two-body potentials
!   6/05 Fitting of chi & mu for EEM/QEq added
!   7/05 Streitz and Mintmire parameters added
!  11/05 Call added to update taper parameters if Voter type
!   3/06 New nfvar2 array to allow more flexibility to pointers
!   3/06 Modified to allow for density component number
!   7/06 Sixbody potentials added
!   8/06 QEq radius added as fitted parameter
!   2/07 EAM function parameters 4 & 5 added
!   4/07 UFF parameters added
!   5/07 UFF torsion added
!   5/07 Bond charge increment added
!   7/07 Call to setreaxFF added
!   7/07 Plane potential variables added
!  11/07 New EAM parameters added for density and functional
!   3/08 Bug in assignment of smzeta & smZnuc fixed
!   3/08 ReaxFF parameters added
!   4/08 More ReaxFF parameters added including nfpot2 & nfpot3
!   4/08 Handling of reaxFFval3 modified to allow for multiple angle pots
!   5/08 New UFF generators for OOP added
!   5/08 UFF chi added
!   7/08 pval6 added to reaxFFval3 array
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 nfvar3 added for MEAM case
!  12/08 Addressing of MEAM coefficient array modified to allow for change
!        from density to functional addressing
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 Use of nfvar for two-body potentials modified
!   4/09 Modified to allow for increased size of twopot
!   6/09 Module name changed from three to m_three
!  10/09 ReaxFF qr12 term added
!   1/10 One-body potential added
!   4/10 Charge updating modified to handle case where there are
!        multiple configurations and some don't include the charge
!        species being fitted.
!   5/10 Charge updating further modified so that total charge is
!        summed across configurations
!  10/10 EDIP potentials added
!  10/10 EDIP linear threebody modifications added
!   3/11 Trap added to prevent calling of setup when ncf = 0 (only happens
!        when fitting reaction energies with no other variables)
!  11/12 Unused argument n removed
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
!  Julian Gale, NRI, Curtin University, November 2012
!
  use bondcharge
  use bondorderdata
  use configurations
  use control
  use current
  use eam
  use EDIPdata
  use element, only : maxele, chi, rmu, qeqchi, qeqmu, smchi, smmu, smzeta, smZnuc, qeqrad
  use element, only : reaxFFchi, reaxFFmu, reaxFFgamma, reaxFFshell, reaxFFqfix
  use fitting
  use four
  use m_three
  use one
  use parallel
  use plane
  use polarise
  use potchange
  use reaxFFdata
  use shifts
  use six
  use species
  use two
  use uffdata
  implicit none
!
!  Passed variables
!
  integer(i4),                   intent(inout) :: ncfold
  logical,                       intent(in)    :: lput
  real(dp),                      intent(inout) :: xc(*)
  real(dp),                      intent(out)   :: xf(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: na
  integer(i4)                                  :: nc
  integer(i4)                                  :: ncc
  integer(i4)                                  :: ncorl
  integer(i4)                                  :: nf
  integer(i4)                                  :: nf2
  integer(i4)                                  :: nf3
  integer(i4)                                  :: nio
  integer(i4)                                  :: nio2
  integer(i4)                                  :: noco
  integer(i4)                                  :: nosh
  integer(i4)                                  :: np
  integer(i4)                                  :: nshel
  integer(i4)                                  :: nsht
  integer(i4)                                  :: nt
  integer(i4)                                  :: ntc
  integer(i4)                                  :: ntp
  integer(i4)                                  :: ntyps
  integer(i4)                                  :: nv
  integer(i4)                                  :: nv2
  integer(i4)                                  :: nv3
  integer(i4), dimension(:), allocatable       :: nvcore
  integer(i4), dimension(:), allocatable       :: nvshel
  integer(i4)                                  :: status
  logical                                      :: labs
  logical                                      :: lqbondchange
  logical                                      :: leschange
  logical                                      :: lfound
  logical                                      :: lnoabs
  logical                                      :: lqchange
  logical                                      :: luffchange
  real(dp)                                     :: aa
  real(dp)                                     :: bb
  real(dp)                                     :: charg
  real(dp)                                     :: dum
  real(dp)                                     :: epsn
  real(dp)                                     :: occsum
  real(dp)                                     :: pol
  real(dp)                                     :: qcore
  real(dp)                                     :: qnew
  real(dp)                                     :: qshel
  real(dp)                                     :: qtot
  real(dp)                                     :: ratio
  real(dp)                                     :: sigm
  real(dp)                                     :: sum
  real(dp)                                     :: sumtotalcharge
!
!  Allocate local memory
!
  allocate(nvcore(nspec),stat=status)
  if (status/=0) call outofmemory('putpar','nvcore')
  allocate(nvshel(nspec),stat=status)
  if (status/=0) call outofmemory('putpar','nvshel')
!
  leschange = .false.
  lqbondchange = .false.
  lqchange = .false.
  luffchange = .false.
  labs = (index(keyword,'abs').ne.0)
  lnoabs = (index(keyword,'noabs').ne.0)
!
!  Initialise logical mask to protect different
!  charges on same species in qupdate
!
  do i = 1,nspec
    lmask(i) = .false.
  enddo
!*************************************
!  Substitute parameters into place  *
!*************************************
  nsht = 0
  do i = 1,nfit
    nt = nftyp(i)
    nf = nfpot(i)
    nf2 = nfpot2(i)
    nf3 = nfpot3(i)
    nv = nfvar(i)
    nv2 = nfvar2(i)
    nv3 = nfvar3(i)
    nc = nfcfg(i)
    if (nt.eq.1) then
!----------------------
!  General variables  -
!----------------------
      if (nf.eq.1) then
!
!  Shell displacement
!
        ncf = nc
        if (ncfold.ne.ncf) then
          ncfold = ncf
          call setup(.true.)
        endif
        nio = nv
        nio2 = (nio - (nstrains-2))/3
        nio = nio - nio2*3 - (nstrains - 3)
        dum = xc(i)*scale(i)
        if (nio.eq.1) then
          if (ndimen(ncf).ge.1.and.lmodco) dum = mod(dum+10.0_dp,1.0_dp)
          xcfg(nsft+nio2) = dum
        elseif (nio.eq.2) then
          if (ndimen(ncf).ge.2.and.lmodco) dum = mod(dum+10.0_dp,1.0_dp)
          ycfg(nsft+nio2) = dum
        else
          if (ndimen(ncf).eq.3.and.lmodco) dum = mod(dum+10.0_dp,1.0_dp)
          zcfg(nsft+nio2) = dum
        endif
        if (lput) xf(i) = dum
      elseif (nf.eq.2) then
!
!  Breathing species radius
!
        ncf = nc
        if (ncfold.ne.ncf) then
          ncfold = ncf
          call setup(.true.)
        endif
        dum = xc(i)*scale(i)
        radcfg(nsft+nv) = dum
        if (lput) xf(i) = dum
      elseif (nf.eq.3) then
!
!  Shift parameter
!
        nsht = nsht + 1
        shift(nsht) = xc(i)*scale(i)
        if (lput) xf(i) = shift(nsht)
      elseif (nf.eq.4) then
!
!  Overall charge distribution
!
        charg = xc(i)*scale(i)
        ntp = nfatyp(i)
        do k = 1,nspec
          ntyps = ntypspec(k)
          if (natspec(k).eq.nv.and.(ntyps.eq.ntp.or.ntp.eq.0)) then
            qlspec(k) = charg
            lmask(k) = .true.
          endif
        enddo
        if (lput) xf(i) = charg
        lqchange = .true.
      elseif (nf.eq.5) then
!
!  Polarisability 
!
        pol = xc(i)*scale(i)
        do k = 1,nasym
          if (iatn(k).eq.nv) then
            dpolar(k) = pol
          endif
        enddo
        if (lput) xf(i) = pol
      elseif (nf.eq.7) then
!
!  Epsilon
!
        epsn = xc(i)*scale(i)
        epsn = abs(epsn)
        epsilon(nv) = epsn
        if (lput) xf(i) = epsn
        leschange = .true.
      elseif (nf.eq.8) then
!
!  Sigma
!
        sigm = xc(i)*scale(i)
        sigm = abs(sigm)
        sigma(nv) = sigm
        if (lput) xf(i) = sigm
        leschange = .true.
      elseif (nf.eq.9) then
!
!  Atom A
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        atoma(nv) = aa
        if (lput) xf(i) = aa
        leschange = .true.
      elseif (nf.eq.10) then
!
!  Atom B
!
        bb = xc(i)*scale(i)
        bb = abs(bb)
        atomb(nv) = bb
        if (lput) xf(i) = bb
        leschange = .true.
      elseif (nf.eq.11) then
!
!  EEM chi
!
        if (nv.ge.1.and.nv.le.maxele) then
          chi(nv) = xc(i)*scale(i)
          if (lput) xf(i) = chi(nv)
        endif
      elseif (nf.eq.12) then
!
!  EEM mu
!
        if (nv.ge.1.and.nv.le.maxele) then
          rmu(nv) = xc(i)*scale(i)
          if (lput) xf(i) = rmu(nv)
        endif
      elseif (nf.eq.13) then
!
!  QEq chi
!
        if (nv.ge.1.and.nv.le.maxele) then
          qeqchi(nv) = xc(i)*scale(i)
          if (lput) xf(i) = qeqchi(nv)
        endif
      elseif (nf.eq.14) then
!
!  QEq mu
!
        if (nv.ge.1.and.nv.le.maxele) then
          qeqmu(nv) = xc(i)*scale(i)
          if (lput) xf(i) = qeqmu(nv)
        endif
      elseif (nf.eq.15) then
!
!  S and M chi
!
        if (nv.ge.1.and.nv.le.maxele) then
          smchi(nv) = xc(i)*scale(i)
          if (lput) xf(i) = smchi(nv)
        endif
      elseif (nf.eq.16) then
!
!  S and M mu
!
        if (nv.ge.1.and.nv.le.maxele) then
          smmu(nv) = xc(i)*scale(i)
          if (lput) xf(i) = smmu(nv)
        endif
      elseif (nf.eq.17) then
!
!  S and M zeta
!
        if (nv.ge.1.and.nv.le.maxele) then
          smzeta(nv) = xc(i)*scale(i)
          if (lput) xf(i) = smzeta(nv)
        endif
      elseif (nf.eq.18) then
!
!  S and M Znuc
!
        if (nv.ge.1.and.nv.le.maxele) then
          smZnuc(nv) = xc(i)*scale(i)
          if (lput) xf(i) = smZnuc(nv)
        endif
      elseif (nf.eq.19) then
!
!  QEq radius
!
        if (nv.ge.1.and.nv.le.maxele) then
          qeqrad(nv) = abs(xc(i)*scale(i))
          if (lput) xf(i) = qeqrad(nv)
        endif
      elseif (nf.eq.20) then
!
!  UFF radius
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFr(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.21) then
!
!  UFF theta
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFtheta(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.22) then
!
!  UFF distance
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFx(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.23) then
!
!  UFF energy
!
        aa = xc(i)*scale(i)
        UFFd(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.24) then
!
!  UFF zeta
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFzeta(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.25) then
!
!  UFF effective charge
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFZeff(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.26) then
!
!  UFF torsion
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFtor(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.27) then
!
!  Bond charge increment
!
        aa = xc(i)*scale(i)
        bondQincrement(nv) = aa
        if (lput) xf(i) = aa
        lqchange = .true.
        lqbondchange = .true.
      elseif (nf.eq.28) then
!
!  Plane potental A
!
        aa = xc(i)*scale(i)
        planepot(1,nv) = aa
        if (lput) xf(i) = aa
      elseif (nf.eq.29) then
!
!  Plane potental B
!
        aa = xc(i)*scale(i)
        planepot(2,nv) = aa
        if (lput) xf(i) = aa
      elseif (nf.eq.30) then
!
!  UFF Koop
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFKoop(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.31) then
!
!  UFF theta oop
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFthetaoop(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.32) then
!
!  UFF chi
!
        aa = xc(i)*scale(i)
        aa = abs(aa)
        UFFchi(nv) = aa
        if (lput) xf(i) = aa
        luffchange = .true.
      elseif (nf.eq.38) then
!
!  One-body energy shift
!
        aa = xc(i)*scale(i)
        onepot(nv) = aa
        if (lput) xf(i) = aa
      elseif (nf.eq.6) then
!
!  Core-shell charge split
!
        charg = xc(i)*scale(i)
        ntp = nfatyp(i)
        ntc = nv + maxele
        ncorl = 0
        nshel = 0
        do k = 1,nspec
          ntyps = ntypspec(k)
          if (natspec(k).eq.nv.and.(ntyps.eq.ntp.or.ntp.eq.0)) then
            qcore = qlspec(k)
            ncorl = ncorl + 1
            nvcore(ncorl) = k
            lmask(k) = .true.
          elseif (natspec(k).eq.ntc.and.(ntyps.eq.ntp.or.ntp.eq.0)) then
            qshel = qlspec(k)
            nshel = nshel + 1
            nvshel(nshel) = k
            lmask(k) = .true.
          endif
        enddo
!
!  Find total number of each type in structure
!
        noco = 0
        nosh = 0
        do ncc = 1,ncfg
          ncf = ncc
          ncfold = ncf
          call setup(.true.)
          do k = 1,numat
            ntyps = nftype(k)
            if (nat(k).eq.nv.and.(ntyps.eq.ntp.or.ntp.eq.0)) then
              noco = noco + 1
            elseif (nat(k).eq.ntc.and.(ntyps.eq.ntp.or.ntp.eq.0)) then
              nosh = nosh + 1
            endif
          enddo
        enddo
        if (noco.gt.0) then
          ratio = dble(nosh)/dble(noco)
          if (nosh.eq.0) then
            call outerror('split used for species with no shell',0_i4)
            call stopnow('putpar')
          endif
          qtot = qcore+qshel*ratio
          qshel = (qtot-charg)/ratio
          do k = 1,ncorl
            qlspec(nvcore(k)) = charg
          enddo
          do k = 1,nshel
            qlspec(nvshel(k)) = qshel
          enddo
          if (lput) xf(i) = charg
          lqchange = .true.
        endif
      endif
    elseif (nt.eq.2) then
!-----------------------
!  Two-body variables  -
!-----------------------
      np = nptype(nf)
      if (nv.eq.1) then
        twopot(1,nf) = xc(i)*scale(i)
        if (np.eq.1.or.np.eq.2.or.(np.ge.4.and.np.le.7).or.np.eq.8.or.np.eq.14.or.np.eq.17.or.np.eq.31) then
          if (.not.lnoabs) twopot(1,nf) = abs(twopot(1,nf))
        endif
        if (lput) xf(i) = twopot(1,nf)
      elseif (nv.eq.2) then
        twopot(2,nf) = xc(i)*scale(i)
        if (np.le.2.or.np.eq.5.or.np.eq.6.or.np.eq.14.or.np.eq.16.or.np.eq.28.or.np.eq.29) then
          twopot(2,nf) = abs(twopot(2,nf))
        endif
        if (lput) xf(i) = twopot(2,nf)
      elseif (nv.eq.3) then
        twopot(3,nf) = xc(i)*scale(i)
        if (np.eq.1.or.np.eq.7.or.np.eq.10.or.np.eq.16.or.np.eq.17.or.np.eq.31.or.np.eq.46) then
          twopot(3,nf) = abs(twopot(3,nf))
        endif
        if (lput) xf(i) = twopot(3,nf)
      elseif (nv.eq.4) then
        twopot(4,nf) = xc(i)*scale(i)
        if (np.eq.10) then
          twopot(4,nf) = abs(twopot(4,nf))
          if (twopot(4,nf).gt.tpot(2,nf)) then
            twopot(4,nf) = tpot(2,nf) - 0.01_dp
            xc(i) = twopot(4,i)/scale(i)
          elseif (twopot(4,nf).lt.tpot(1,nf)) then
            twopot(4,nf) = tpot(1,nf) + 0.01_dp
            xc(i) = twopot(4,i)/scale(i)
          endif
        elseif (np.eq.46) then
          twopot(4,nf) = abs(twopot(4,nf))
! For VBO_twobody potential the cutoff is also related to the delta parameter
          rpot(nf) = twopot(4,nf)
        endif
        if (lput) xf(i) = twopot(4,nf)
      elseif (nv.eq.5) then
        twopot(5,nf) = xc(i)*scale(i)
        if (lput) xf(i) = twopot(5,nf)
      elseif (nv.eq.6) then
        twopot(6,nf) = xc(i)*scale(i)
        if (lput) xf(i) = twopot(6,nf)
      elseif (nv.eq.7) then
        twopot(7,nf) = xc(i)*scale(i)
        if (lput) xf(i) = twopot(7,nf)
      elseif (nv.ge.10) then
        tpot(nv-9,nf) = xc(i)*scale(i)
        if (lput) xf(i) = tpot(nv-9,nf)
      endif
      if (np.eq.10) call buck4(nf)
      if (np.eq.46) call setvbon(nf)
    elseif (nt.eq.3) then
!-------------------------
!  Three-body variables  -
!-------------------------
      if (nv.eq.1) then
        thbk(nf) = xc(i)*scale(i)
        if (labs) thbk(nf) = abs(thbk(nf))
        if (lput) xf(i) = thbk(nf)
      elseif (nv.eq.2) then
        theta(nf) = xc(i)*scale(i)
        if (lput) xf(i) = theta(nf)
      elseif (nv.eq.3) then
        thrho1(nf) = xc(i)*scale(i)
        if (labs) thrho1(nf) = abs(thrho1(nf))
        if (lput) xf(i) = thrho1(nf)
      elseif (nv.eq.4) then
        thrho2(nf) = xc(i)*scale(i)
        if (labs) thrho2(nf) = abs(thrho2(nf))
        if (lput) xf(i) = thrho2(nf)
      elseif (nv.eq.5) then
        thrho3(nf) = xc(i)*scale(i)
        if (labs) thrho3(nf) = abs(thrho3(nf))
        if (lput) xf(i) = thrho3(nf)
      else
        threepoly(nv-5,nf) = xc(i)*scale(i)
        if (lput) xf(i) = threepoly(nv-5,nf)
      endif
    elseif (nt.eq.4) then
!------------------------
!  Four-body variables  -
!------------------------
      if (nv.eq.1) then
        fork(nf) = xc(i)*scale(i)
        if (labs) fork(nf) = abs(fork(nf))
        if (lput) xf(i) = fork(nf)
      else
        forpoly(nv-1,nf) = xc(i)*scale(i)
        if (lput) xf(i) = forpoly(nv-1,nf)
      endif
    elseif (nt.eq.5) then
!------------------------
!  Many-body variables  -
!------------------------
      if (nv.eq.1) then
!
!  EAM density parameter 1
!
        bb = xc(i)*scale(i)
        denpar(1,nv3,nv2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.2) then
!
!  EAM density parameter 2
!
        bb = xc(i)*scale(i)
        denpar(2,nv3,nv2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.3) then
!
!  EAM density parameter 3
!
        bb = xc(i)*scale(i)
        denpar(3,nv3,nv2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.4) then
!
!  EAM functional parameter 1
!
        bb = xc(i)*scale(i)
        eamfnpar(1,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.5) then
!
!  EAM functional parameter 2
!
        bb = xc(i)*scale(i)
        eamfnpar(2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.6) then
!
!  EAM functional parameter 3
!
        bb = xc(i)*scale(i)
        eamfnpar(3,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.7) then
!
!  EAM alloy scale parameter 
!
        bb = xc(i)*scale(i)
        bb = abs(bb)
        eamalloy(1,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.8) then
!
!  EAM alloy additive parameter 
!
        bb = xc(i)*scale(i)
        eamalloy(2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.9) then
!
!  EAM functional parameter 4
!
        bb = xc(i)*scale(i)
        eamfnpar(4,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.10) then
!
!  EAM functional parameter 5
!
        bb = xc(i)*scale(i)
        eamfnpar(5,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.11) then
!
!  EAM functional parameter 6
!
        bb = xc(i)*scale(i)
        eamfnpar(6,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.12) then
!
!  EAM functional parameter 7
!
        bb = xc(i)*scale(i)
        eamfnpar(7,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.13) then
!
!  EAM functional parameter 8
!
        bb = xc(i)*scale(i)
        eamfnpar(8,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.14) then
!
!  EAM functional parameter 9
!
        bb = xc(i)*scale(i)
        eamfnpar(9,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.15) then
!
!  EAM density parameter 4
!
        bb = xc(i)*scale(i)
        denpar(4,nv3,nv2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.16) then
!
!  EAM density parameter 5
!
        bb = xc(i)*scale(i)
        denpar(5,nv3,nv2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.17) then
!
!  EAM density parameter 6
!
        bb = xc(i)*scale(i)
        denpar(6,nv3,nv2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.18) then
!
!  EAM density parameter 7
!
        bb = xc(i)*scale(i)
        denpar(7,nv3,nv2,nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.19) then
!
!  MEAM coefficient t
!
        bb = xc(i)*scale(i)
        eamfnmeamcoeff(nv2,nf) = bb
        if (lput) xf(i) = bb
      endif
    elseif (nt.eq.6) then
!-------------------------
!  Bond-order variables  -
!-------------------------
      if (nv.eq.1) then
!
!  Twobody A
!
        bb = xc(i)*scale(i)
        BOacoeff(nf) = abs(bb)
        if (lput) xf(i) = bb
      elseif (nv.eq.2) then
!
!  Twobody B
!
        bb = xc(i)*scale(i)
        BObcoeff(nf) = abs(bb)
        if (lput) xf(i) = bb
      elseif (nv.eq.3) then
!
!  Twobody zeta A
!
        bb = xc(i)*scale(i)
        BOzacoeff(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.4) then
!
!  Twobody zeta B
!
        bb = xc(i)*scale(i)
        BOzbcoeff(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.5) then
!
!  Repulsive BO alpha
!
        bb = xc(i)*scale(i)
        BOecoeffR(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.6) then
!
!  Repulsive BO n
!
        bb = xc(i)*scale(i)
        BOncoeffR(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.7) then
!
!  Repulsive BO lambda
!
        bb = xc(i)*scale(i)
        BOlcoeffR(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.8) then
!
!  Repulsive BO theta c
!
        bb = xc(i)*scale(i)
        BOccoeffR(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.9) then
!
!  Repulsive BO theta d
!
        bb = xc(i)*scale(i)
        BOdcoeffR(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.10) then
!
!  Repulsive BO theta h
!
        bb = xc(i)*scale(i)
        BOhcoeffR(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.11) then
!
!  Attractive BO alpha
!
        bb = xc(i)*scale(i)
        BOecoeffA(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.12) then
!
!  Attractive BO n
!
        bb = xc(i)*scale(i)
        BOncoeffA(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.13) then
!
!  Attractive BO lambda
!
        bb = xc(i)*scale(i)
        BOlcoeffA(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.14) then
!
!  Attractive BO theta c
!
        bb = xc(i)*scale(i)
        BOccoeffA(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.15) then
!
!  Attractive BO theta d
!
        bb = xc(i)*scale(i)
        BOdcoeffA(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.16) then
!
!  Attractive BO theta h
!
        bb = xc(i)*scale(i)
        BOhcoeffA(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.17) then
!
!  Twobody chi R
!
        bb = xc(i)*scale(i)
        BOchiR(nf) = bb
        if (lput) xf(i) = bb
      elseif (nv.eq.18) then
!
!  Twobody chi A
!
        bb = xc(i)*scale(i)
        BOchiA(nf) = bb
        if (lput) xf(i) = bb
      endif
    elseif (nt.eq.7) then
!-----------------------
!  Six-body variables  -
!-----------------------
      if (nv.eq.1) then
        sixk(nf) = xc(i)*scale(i)
        if (labs) sixk(nf) = abs(sixk(nf))
        if (lput) xf(i) = sixk(nf)
      endif
    elseif (nt.eq.8) then
!---------------------
!  ReaxFF variables  -
!---------------------
      if (nv.eq.1) then
!
!  ReaxFF1 radii - sigma, pi & pi-pi
!
        if (nv2.eq.1) then
          reaxFFr(1,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFr(1,nf)
        elseif (nv2.eq.2) then
          reaxFFr(2,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFr(2,nf)
        elseif (nv2.eq.3) then
          reaxFFr(3,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFr(3,nf)
        endif
      elseif (nv.eq.2) then
!     
!  ReaxFF1 valence - normal, boc, lp & angle
!       
        if (nv2.eq.1) then
          reaxFFval(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval(1,nf)
        elseif (nv2.eq.2) then
          reaxFFval(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval(2,nf)
        elseif (nv2.eq.3) then
          reaxFFval(3,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval(3,nf)
        elseif (nv2.eq.4) then
          reaxFFval(4,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval(4,nf)
        endif
      elseif (nv.eq.3) then
!     
!  ReaxFF1 over - p_boc3, p_boc4, p_boc5, p_ovun2
!
        if (nv2.eq.1) then
          reaxFFpboc(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpboc(1,nf)
        elseif (nv2.eq.2) then
          reaxFFpboc(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpboc(2,nf)
        elseif (nv2.eq.3) then
          reaxFFpboc(3,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpboc(3,nf)
        elseif (nv2.eq.4) then
          reaxFFoc1(nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFoc1(nf)
        endif
      elseif (nv.eq.4) then
!     
!  ReaxFF1 under - p_ovun5
!
        if (nv2.eq.1) then
          reaxFFuc1(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFuc1(nf)
        endif
      elseif (nv.eq.5) then
!     
!  ReaxFF1 lone pair - n_lp_opt & p_lp2
!
        if (nv2.eq.1) then
          reaxFFlp(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlp(1,nf)
        elseif (nv2.eq.2) then
          reaxFFlp(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlp(2,nf)
        endif
      elseif (nv.eq.6) then
!     
!  ReaxFF1 angle - p_val3 & p_val5
!
        if (nv2.eq.1) then
          reaxFFval1(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval1(1,nf)
        elseif (nv2.eq.2) then
          reaxFFval1(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval1(2,nf)
        endif
      elseif (nv.eq.7) then
!     
!  ReaxFF1 morse - alpha, Dij, rvdw and gamma_w
!
        if (nv2.eq.1) then
          reaxFFalpha(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFalpha(nf)
        elseif (nv2.eq.2) then
          reaxFFeps(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFeps(nf)
        elseif (nv2.eq.3) then
          reaxFFrvdw(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFrvdw(nf)
        elseif (nv2.eq.4) then
          reaxFFgammaw(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFgammaw(nf)
        endif
      elseif (nv.eq.8) then
!     
!  ReaxFF1 charge parameters - chi, mu, gamma, qshell
!
        if (nv2.eq.1) then
          reaxFFchi(nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFchi(nf)
        elseif (nv2.eq.2) then
          reaxFFmu(nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFmu(nf)
        elseif (nv2.eq.3) then
          reaxFFgamma(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFgamma(nf)
        elseif (nv2.eq.4) then
          reaxFFshell(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFshell(1,nf)
        elseif (nv2.eq.5) then
          reaxFFshell(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFshell(2,nf)
        elseif (nv2.eq.6) then
          reaxFFshell(3,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFshell(3,nf)
        elseif (nv2.eq.7) then
          reaxFFqfix(nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFqfix(nf)
        endif
      elseif (nv.eq.9) then
!     
!  ReaxFF0 bond parameters - p_boc1 p_boc2
!
        if (nv2.eq.1) then
          reaxFFlam(1) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(1)
        elseif (nv2.eq.2) then
          reaxFFlam(2) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(2)
        endif
      elseif (nv.eq.10) then
!
!  ReaxFF0 over parameters - p_ovun3 p_ovun4 p_ovun6 p_ovun7 p_ovun8
!
        if (nv2.eq.1) then
          reaxFFlam(6) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(6)
        elseif (nv2.eq.2) then
          reaxFFlam(31) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(31)
        elseif (nv2.eq.3) then
          reaxFFlam(7) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(7)
        elseif (nv2.eq.4) then
          reaxFFlam(8) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(8)
        elseif (nv2.eq.5) then
          reaxFFlam(9) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(9)
        endif
      elseif (nv.eq.11) then
!
!  ReaxFF0 valence parameters - p_val6 p_val8 p_val9 p_val10
!
        if (nv2.eq.1) then
          reaxFFlam(15) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(15)
        elseif (nv2.eq.2) then
          reaxFFlam(16) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(16)
        elseif (nv2.eq.3) then
          reaxFFlam(17) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(17)
        elseif (nv2.eq.4) then
          reaxFFlam(18) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(18)
        endif
      elseif (nv.eq.12) then
!
!  ReaxFF0 penalty parameters - p_pen2 p_pen3 p_pen4
!
        if (nv2.eq.1) then
          reaxFFlam(20) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(20)
        elseif (nv2.eq.2) then
          reaxFFlam(21) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(21)
        elseif (nv2.eq.3) then
          reaxFFlam(22) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(22)
        endif
      elseif (nv.eq.13) then
!
!  ReaxFF0 torsion parameters - p_tor2 p_tor3 p_tor4 p_cot2
!
        if (nv2.eq.1) then
          reaxFFlam(24) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(24)
        elseif (nv2.eq.2) then
          reaxFFlam(25) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(25)
        elseif (nv2.eq.3) then
          reaxFFlam(26) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(26)
        elseif (nv2.eq.4) then
          reaxFFlam(27) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(27)
        endif
      elseif (nv.eq.14) then
!
!  ReaxFF0 VDW parameter - p_vdw1
!
        if (nv2.eq.1) then
          reaxFFlam(28) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(28)
        endif
      elseif (nv.eq.15) then
!
!  ReaxFF0 lone pair parameter - p_lp1
!
        if (nv2.eq.1) then
          reaxFFlam(29) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFlam(29)
        endif
      elseif (nv.eq.16) then
!
!  ReaxFF2 bond parameters - De_sigma, De_pi, De_pipi, p_be1 & p_be2
!
        if (nv2.eq.1) then
          reaxFFDe(1,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFDe(1,nf)
        elseif (nv2.eq.2) then
          reaxFFDe(2,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFDe(2,nf)
        elseif (nv2.eq.3) then
          reaxFFDe(3,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFDe(3,nf)
        elseif (nv2.eq.4) then
          reaxFFpbe(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbe(1,nf)
        elseif (nv2.eq.5) then
          reaxFFpbe(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbe(2,nf)
        endif
      elseif (nv.eq.17) then
!         
!  ReaxFF2 over parameters - p_ovun1
!     
        if (nv2.eq.1) then
          reaxFFoc2(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFoc2(nf)
        endif
      elseif (nv.eq.18) then
!         
!  ReaxFF2 bo parameters - p_bo1, p_bo2, p_bo3, p_bo4, p_bo5 & p_bo6
!     
        if (nv2.eq.1) then
          reaxFFpbo(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbo(1,nf)
        elseif (nv2.eq.2) then
          reaxFFpbo(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbo(2,nf)
        elseif (nv2.eq.3) then
          reaxFFpbo(3,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbo(3,nf)
        elseif (nv2.eq.4) then
          reaxFFpbo(4,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbo(4,nf)
        elseif (nv2.eq.5) then
          reaxFFpbo(5,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbo(5,nf)
        elseif (nv2.eq.6) then
          reaxFFpbo(6,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpbo(6,nf)
        endif
      elseif (nv.eq.19) then
!
!  ReaxFF2 morse parameters - De, alpha, r0, r_sigma, r_pi & r_pipi
!
        if (nv2.eq.1) then
          reaxFFmorse(1,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFmorse(1,nf)
        elseif (nv2.eq.2) then
          reaxFFmorse(2,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFmorse(2,nf)
        elseif (nv2.eq.3) then
          reaxFFmorse(3,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFmorse(3,nf)
        elseif (nv2.eq.4) then
          reaxFFmorse(4,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFmorse(4,nf)
        elseif (nv2.eq.5) then
          reaxFFmorse(5,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFmorse(5,nf)
        elseif (nv2.eq.6) then
          reaxFFmorse(6,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFmorse(6,nf)
        endif
      elseif (nv.eq.20) then
!
!  ReaxFF2 pen parameters - p_pen1, p_pen2 & p_pen3
!
        if (nv2.eq.1) then
          reaxFFpen2(1,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpen2(1,nf)
        elseif (nv2.eq.2) then
          reaxFFpen2(2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpen2(2,nf)
        elseif (nv2.eq.3) then
          reaxFFpen2(3,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpen2(3,nf)
        endif
      elseif (nv.eq.21) then
!
!  ReaxFF3 ang parameters - theta_00, p_val1, p_val2, p_val4, p_val7 & p_val6
!
        if (nv2.eq.1) then
          reaxFFval3(1,nf3,nf2,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFval3(1,nf3,nf2,nf)
        elseif (nv2.eq.2) then
          reaxFFval3(2,nf3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval3(2,nf3,nf2,nf)
        elseif (nv2.eq.3) then
          reaxFFval3(3,nf3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFval3(3,nf3,nf2,nf)
        elseif (nv2.eq.4) then
          reaxFFval3(4,nf3,nf2,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFval3(4,nf3,nf2,nf)
        elseif (nv2.eq.5) then
          reaxFFval3(5,nf3,nf2,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFval3(5,nf3,nf2,nf)
        elseif (nv2.eq.6) then
          reaxFFval3(6,nf3,nf2,nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = reaxFFval3(6,nf3,nf2,nf)
        endif
      elseif (nv.eq.22) then
!
!  ReaxFF3 pen parameter - p_pen1
!
        if (nv2.eq.1) then
          reaxFFpen3(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFpen3(nf2,nf)
        endif
      elseif (nv.eq.23) then
!
!  ReaxFF3 conj parameters - p_coa1, p_coa2, p_coa3 & p_coa4
!
        if (nv2.eq.1) then
          reaxFFconj3(1,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFconj3(1,nf2,nf)
        elseif (nv2.eq.2) then
          reaxFFconj3(2,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFconj3(2,nf2,nf)
        elseif (nv2.eq.3) then
          reaxFFconj3(3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFconj3(3,nf2,nf)
        elseif (nv2.eq.4) then
          reaxFFconj3(4,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFconj3(4,nf2,nf)
        endif
      elseif (nv.eq.24) then
!
!  ReaxFF3 hbond parameters - r0_hb, p_hb1, p_hb2 & p_hb3
!
        if (nv2.eq.1) then
          reaxFFhb3(1,nf3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFhb3(1,nf3,nf2,nf)
        elseif (nv2.eq.2) then
          reaxFFhb3(2,nf3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFhb3(2,nf3,nf2,nf)
        elseif (nv2.eq.3) then
          reaxFFhb3(3,nf3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFhb3(3,nf3,nf2,nf)
        elseif (nv2.eq.4) then
          reaxFFhb3(4,nf3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFhb3(4,nf3,nf2,nf)
        endif
      elseif (nv.eq.25) then
!
!  ReaxFF4 torsion parameters - V1, V2, V3, p_tor1 & p_cot1
!
        if (nv2.eq.1) then
          reaxFFtor4(1,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFtor4(1,nf2,nf)
        elseif (nv2.eq.2) then
          reaxFFtor4(2,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFtor4(2,nf2,nf)
        elseif (nv2.eq.3) then
          reaxFFtor4(3,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFtor4(3,nf2,nf)
        elseif (nv2.eq.4) then
          reaxFFtor4(4,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFtor4(4,nf2,nf)
        elseif (nv2.eq.5) then
          reaxFFtor4(5,nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = reaxFFtor4(5,nf2,nf)
        endif
      endif
    elseif (nt.eq.9) then
!-------------------
!  EDIP variables  -
!-------------------
      if (nv.eq.1) then
!
!  EDIP coordination number
!
        if (nv2.eq.1) then
          EDIPalpha(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = EDIPalpha(nf)
        elseif (nv2.eq.2) then
          EDIPZdih(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = EDIPZdih(nf)
        elseif (nv2.eq.2) then
          EDIPZrep(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = EDIPZrep(nf)
        elseif (nv2.eq.2) then
          EDIPc0(nf) = abs(xc(i)*scale(i))
          if (lput) xf(i) = EDIPc0(nf)
        endif
      elseif (nv.eq.2) then
!
!  EDIP twobody energy
!
        if (nv2.eq.1) then
          EDIP2epsilon(nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP2epsilon(nf)
        elseif (nv2.eq.2) then
          EDIP2B(nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP2B(nf)
        elseif (nv2.eq.3) then
          EDIP2beta(nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP2beta(nf)
        elseif (nv2.eq.4) then
          EDIP2sigma(nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP2sigma(nf)
        elseif (nv2.eq.5) then
          EDIP2a(nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP2a(nf)
        elseif (nv2.eq.6) then
          EDIP2aprime(nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP2aprime(nf)
        endif
      elseif (nv.eq.3) then
!
!  EDIP threebody energy
!
        if (nv2.eq.1) then
          EDIP3lambda0(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP3lambda0(nf2,nf)
        elseif (nv2.eq.2) then
          EDIP3lambdap(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP3lambdap(nf2,nf)
        elseif (nv2.eq.3) then
          EDIP3Z0(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP3Z0(nf2,nf)
        elseif (nv2.eq.4) then
          EDIP3gamma0(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP3gamma0(nf2,nf)
        elseif (nv2.eq.5) then
          EDIP3gammap(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP3gammap(nf2,nf)
        elseif (nv2.eq.6) then
          EDIP3q(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP3q(nf2,nf)
        elseif (nv2.eq.7) then
          EDIP3kq2(nf2,nf) = xc(i)*scale(i)
          if (lput) xf(i) = EDIP3kq2(nf2,nf)
        endif
      endif
    endif
  enddo
!*******************
!  Update charges  *
!*******************
  if (lastq.ne.0) then
    lqchange = .true.
    sum = 0.0_dp
    sumtotalcharge = 0.0_dp
    occsum = 0.0_dp
    do ncc = 1,ncfg
      ncf = ncc
      ncfold = ncf
      call setup(.true.)
      sumtotalcharge = sumtotalcharge + totalcharge
      do i = 1,numat
        na = nat(i)
        ntp = nftype(i)
        if (na.ne.lastq.or.(ntp.ne.lastt.and.lastt.ne.0)) then
          j = 1
          lfound = .false.
          do while (.not.lfound.and.j.le.nspec) 
            ntyps = ntypspec(j)
            if (na.eq.natspec(j).and.(ntp.eq.ntyps.or.ntyps.eq.0)) then
              if (lmask(j)) then
                sum = sum + qlspec(j)*occuf(i)
              else
                sum = sum + qf(i)*occuf(i)
              endif
              lfound = .true.
            endif
            j = j + 1
          enddo
        else
          occsum = occsum + occuf(i)
        endif
      enddo
    enddo
    qnew = (sumtotalcharge - sum)/(occsum)
    do i = 1,nspec
      ntyps = ntypspec(i)
      if (natspec(i).eq.lastq.and.(ntyps.eq.lastt.or.lastt.eq.0)) then
        qlspec(i) = qnew
        lmask(i) = .true.
      endif
    enddo
  endif
!
!  Free local memory
!
  deallocate(nvshel,stat=status)
  if (status/=0) call deallocate_error('putpar','nvshel')
  deallocate(nvcore,stat=status)
  if (status/=0) call deallocate_error('putpar','nvcore')
!*******************
!  Update charges  *
!*******************
  if (lqbondchange) call bondq(.false.)
  if (lqchange) call qupdate
  if (ncf.gt.0) call setup(.true.)
!**********************
!  Update potentials  *
!**********************
  if (leschange) call setlenn
  if (luffchange) call setuff
  if (lc6.and.lc6one) call setc6
  if (nbopot.gt.0) call setbondorder
  if (nreaxFFspec.gt.0) call setreaxFF
  if (nEDIPspec.gt.0) call setedip
  if (tapertype.eq.3) call settaper
  call rhoinv
!
!  Update potential boundary corrections
!
  do i = 1,nchng
    call potcut(npchng(i))
  enddo
!
  return
  end
