  subroutine twobodymd(eatom,ereal,ec6,nor,nor0,npots,npotl,cut2r,cut2e,cut2s,nmolonly, &
                       factor,sfct,radsum,rtrm1,sctrm1,sctrm2,qi,qj,lcspair,lperiodic, &
                       lskipsr,lskipq,lorder12)
!
!  Subroutine for calculating twobody energy for pair of atoms
!
!  eatom   = interatomic potential energy
!  ereal   = real space electrostatic energy
!  ec6     = real and recip space dispersion energy
!  nor     = pointer to final distance to calculate for
!  nor0    = pointer to first distance to calculate for
!  dist    = array of distances (on entry squared, on exit not squared) 
!            from nor0 to nor
!  deriv   = on return contains first derivative component
!  derive0 = on return contains electrostatic energy component with the charges
!  derive  = on return contains electrostatic first derivative component
!  rderiv  = on return contains terms for mixed radial second derivatives
!  npots   = no. of valid potentials for this pair of atoms
!  npotl   = array of pointers to valid potentials
!  cut2r   = interatomic potential cutoff squared
!  cut2e   = real space Ewald/Wolf cutoff squared
!  cut2s   = core-shell cutoff squared
!  lptrmol = logical intramolecular distance pointer
!  nmolonly= electrostatic molecule correction pointer
!  factor  = product of charges and unit conversion factor (includes sfct)
!  sfct    = symmetry equivalence factor
!  radsum  = sum of radii of ions for breathing shell model
!  rtrm1   = radial first derivative term
!  sctrm1/2= contribution to many-body rho value for EAM
!  qi      = charge of ion i in pair
!  qj      = charge of ion j in pair
!  lcspair = possible core-shell pair
!  lperiodic=flag deciding whether Ewald or normal 1/r is to be used
!  lbonded = flag indicating whether atoms are bonded for MM calc
!  l2bonds = flag indicating whether atoms are bonded to a common atom
!  l3bonds = flag indicating whether atoms are connected via 3 bonds
!  repcut  = cutoff for exponential repulsion terms
!  lskipsr = flag indicating whether short-range potentials should be
!            skipped - used in defect calcs
!  lorder12= indicates whether i-j order is as expected or not
!
!  The following are only needed for EEM/QEq/lDoQderiv2 :
!
!  d1i     = on return contains the derivative of E with respect to qi/r
!  d1j     = on return contains the derivative of E with respect to qj/r
!
!  On return deriv contains the complete first derivative terms, while derive 
!  contains the derivatives for the charge only terms.
!
!   1/05 Created from twobody.f
!   4/05 cosh-spring potential added
!   8/05 sctrms modified to ensure that density is positive for cubic case
!   9/05 Voter form of density for EAM added
!  11/05 General Voter taper option added
!  11/05 Tapering of densities added
!   2/06 Quadratic and quartic densities added
!   3/06 Power law EAM densities truncated after r0
!   3/06 Modified to allow for density component number
!   3/06 Poly harmonic potential added
!   4/06 Species specific density added
!   8/06 Trap added for tapermax = 0 in Voter taper
!  10/06 small2 changed to 1 x 10-13
!  11/06 qoverr2 potential added
!   1/07 Force constant potential added
!   3/07 Glue potential density added
!   3/07 SRGlue potential added
!   3/07 Checks for correct bond type added
!   4/07 Error in setting of rd4 fixed
!   5/07 lskipq flag added
!   5/07 Exponential taper added
!   5/07 Morse with etaper added
!   5/07 eVoter EAM density added
!   9/07 ptaper renamed to p5taper
!  11/07 Mei-Davenport potential added
!  11/07 MDF taper added
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!   3/08 erferfc, erf and reperfc potentials added
!   7/08 nmolonly check is now only used if lcoulsub is true
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 Name of array for 1/bpot for Buckingham potential changed from rho to rhopot
!  11/08 rhoij / rhoji now an array to handle MEAM orders
!  11/08 x, y, z now passed as arguments to rhoderv
!  11/08 rho arrays redimensioned as 2-D for benefit of MEAM
!  11/08 call to rhoderv replaced by calls to meamrho/eamrho according to MEAM vs EAM
!  12/08 rho switched back to 1-D array with condensed components
!  12/08 sctrm1/2 changed to be an array for benefit of MEAM
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   1/09 Baskes twobody potential added
!   1/09 VBO_twobody potential added
!   1/09 Inconsistencies between use of derivk vs deriv(k) in twobody vs twobodymd fixed
!   1/09 rhoij now added to sctrm1 and rhoji is added to sctrm2 instead of vice versa
!   1/09 xtmp, ytmp, ztmp added to call of eamrho
!   2/09 New derivative arguments added to calls of eamrho/meamrho
!   3/09 lorder12 added to argument list
!   3/09 lorder12 added to list of arguments for meamrho
!   4/09 Inconsistency in small vs small2 corrected for shell case
!   4/09 d term added to Baskes potental
!   4/09 Core-shell interaction during Wolf sum modified to improve handling of small distances
!   4/09 MEAM density calculation removed from this routine since it requires more than twobody
!        interactions due to screening term.
!   4/09 Terms associated with Wolf sum modified for core-shell model
!   6/09 Charge as a coordinate option added
!   7/09 Exppowers potential added
!   8/09 Grimme_C6 potential added
!   9/09 r0 added for standard polynomial potential
!   2/10 Central force model potentials added
!   5/10 gcoulomb potential added
!   7/10 Modified to allow for molecular Coulomb subtraction for 1-D case
!  12/11 Sign of Baskes functional reversed to be consistent with papers.
!   1/12 g12 mmexc=5 option added
!   5/12 sixth and third now used from numbers module
!   5/12 Becke_Johnson_C6 added
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
  use constants
  use control
  use current, only : lewald, ndim
  use eam
  use general, only : cutw, etaw, rkw, selfwolf
  use kspace
  use molecule
  use numbers, only : sixth, third
  use realvectors
  use splinedata
  use two
  implicit none
!
!  Passed variables
!
  integer(i4),    intent(in)      :: nmolonly
  integer(i4),    intent(in)      :: nor
  integer(i4),    intent(in)      :: nor0
  integer(i4),    intent(in)      :: npotl(*)
  integer(i4),    intent(in)      :: npots
  logical,        intent(in)      :: lcspair
  logical,        intent(in)      :: lorder12
  logical,        intent(in)      :: lperiodic
  logical,        intent(in)      :: lskipq
  logical,        intent(in)      :: lskipsr
  real(dp),       intent(in)      :: cut2e
  real(dp),       intent(in)      :: cut2r
  real(dp),       intent(in)      :: cut2s
  real(dp),       intent(inout)   :: eatom
  real(dp),       intent(inout)   :: ec6
  real(dp),       intent(inout)   :: ereal
  real(dp),       intent(in)      :: factor
  real(dp),       intent(in)      :: qi
  real(dp),       intent(in)      :: qj
  real(dp),       intent(in)      :: radsum
  real(dp),       intent(out)     :: rtrm1
  real(dp),       intent(inout)   :: sctrm1
  real(dp),       intent(inout)   :: sctrm2
  real(dp),       intent(in)      :: sfct
!
!  Local variables
!
  integer(i4)                     :: k
  integer(i4)                     :: l
  integer(i4)                     :: m
  integer(i4)                     :: mpt
  integer(i4)                     :: npot
  integer(i4)                     :: norder
  integer(i4)                     :: npt
  integer(i4)                     :: nptyp
  logical                         :: lc6loc
  logical                         :: lcoulsub
  logical,                   save :: lfirsttime = .true.
  logical                         :: lneedgrad1
  logical                         :: lvalid
  real(dp)                        :: apt
  real(dp)                        :: bpt
  real(dp)                        :: b6
  real(dp)                        :: b8
  real(dp)                        :: b10
  real(dp)                        :: br6
  real(dp)                        :: br8
  real(dp)                        :: br10
  real(dp)                        :: betaoverr0
  real(dp)                        :: c6t1
  real(dp)                        :: c6t2
  real(dp)                        :: c6tot
  real(dp)                        :: c6trm1
  real(dp)                        :: cpt
  real(dp)                        :: ctrm1
  real(dp)                        :: d
  real(dp)                        :: detpfn
  real(dp)                        :: d2etpfn
  real(dp)                        :: d3etpfn
  real(dp)                        :: dpt
  real(dp)                        :: dgofr
  real(dp)                        :: dfun(4)
  real(dp)                        :: dtpfn
  real(dp)                        :: d2tpfn
  real(dp)                        :: d3tpfn
  real(dp)                        :: derf
  real(dp)                        :: derfc
  real(dp)                        :: derivk
  real(dp)                        :: derivktrm
  real(dp)                        :: df6
  real(dp)                        :: df8
  real(dp)                        :: df10
  real(dp)                        :: dfdmp
  real(dp)                        :: dfrhodr
  real(dp)                        :: dpte
  real(dp)                        :: drhoij
  real(dp)                        :: drhoji
  real(dp)                        :: drhoijs
  real(dp)                        :: drhojis
  real(dp)                        :: drhoij2
  real(dp)                        :: drhoji2
  real(dp)                        :: drhoij2s
  real(dp)                        :: drhoji2s
  real(dp)                        :: drhoij2m
  real(dp)                        :: drhoji2m
  real(dp)                        :: drhoij3
  real(dp)                        :: drhoji3
  real(dp)                        :: drk6
  real(dp)                        :: drk8
  real(dp)                        :: drk10
  real(dp)                        :: dserfc
  real(dp)                        :: dtrm0dr
  real(dp)                        :: dtrm1
  real(dp)                        :: dxdr
  real(dp)                        :: e01
  real(dp)                        :: e02
  real(dp)                        :: e0t
  real(dp)                        :: e11
  real(dp)                        :: eatm
  real(dp)                        :: eatmtrm
  real(dp)                        :: ebr6
  real(dp)                        :: ebr8
  real(dp)                        :: ebr10
  real(dp)                        :: ecfm
  real(dp)                        :: decfm
  real(dp)                        :: EcZ
  real(dp)                        :: effc
  real(dp)                        :: ept
  real(dp)                        :: erffc
  real(dp)                        :: erftrm
  real(dp)                        :: erfctrm
  real(dp)                        :: erealin
  real(dp)                        :: etar2
  real(dp)                        :: etpfn
  real(dp)                        :: exptrm
  real(dp)                        :: exptrm1
  real(dp)                        :: exptrm2
  real(dp)                        :: etaloc
  real(dp)                        :: f6
  real(dp)                        :: f8
  real(dp)                        :: f10
  real(dp)                        :: fdmp
  real(dp)                        :: fpt
  real(dp)                        :: frho
  real(dp)                        :: gofr
  real(dp)                        :: logtrm
  real(dp)                        :: pscale
  real(dp)                        :: psfct
  real(dp)                        :: qtrm
  real(dp)                        :: r
  real(dp)                        :: r0
  real(dp)                        :: r06
  real(dp)                        :: r2
  real(dp)                        :: r3
  real(dp)                        :: r4
  real(dp)                        :: r6
  real(dp)                        :: r12
  real(dp),                  save :: r24
  real(dp)                        :: rd
  real(dp)                        :: rd2
  real(dp)                        :: rd3
  real(dp)                        :: rd4
  real(dp)                        :: rd5
  real(dp)                        :: rd6
  real(dp)                        :: rk
  real(dp)                        :: rk2
  real(dp)                        :: rapt
  real(dp)                        :: rapt2
  real(dp)                        :: rexptrm
  real(dp)                        :: rho
  real(dp)                        :: rho0
  real(dp)                        :: rhol
  real(dp)                        :: rhoij(maxmeamcomponent)
  real(dp)                        :: rhoji(maxmeamcomponent)
  real(dp)                        :: rk6
  real(dp)                        :: rk8
  real(dp)                        :: rk10
  real(dp)                        :: rkd
  real(dp)                        :: rmax
  real(dp)                        :: rmpt
  real(dp)                        :: rnpt
  real(dp),                  save :: rnineth
  real(dp)                        :: rqtrm
  real(dp)                        :: rr0
  real(dp)                        :: rrho
  real(dp)                        :: rsfct
  real(dp)                        :: rtrm
  real(dp)                        :: rtrm0
  real(dp)                        :: rtrm1d
  real(dp)                        :: rtrm1k
  real(dp)                        :: rxm1
  real(dp)                        :: setaloc
  real(dp),                  save :: seventh
  real(dp)                        :: sig
  real(dp),                  save :: small2
  real(dp)                        :: sumtapergrad
  real(dp)                        :: sumtaperpot
  real(dp)                        :: th
  real(dp)                        :: dthdr
  real(dp)                        :: d2thdr2
  real(dp)                        :: d3thdr3
  real(dp)                        :: tp
  real(dp)                        :: tpfn
  real(dp)                        :: tpt
  real(dp)                        :: tpte
  real(dp)                        :: trm
  real(dp)                        :: trm0
  real(dp)                        :: trm1
  real(dp)                        :: trm12
  real(dp)                        :: trm2
  real(dp)                        :: trm3
  real(dp)                        :: trm4
  real(dp)                        :: trme
  real(dp)                        :: ts0
  real(dp)                        :: ts1
  real(dp)                        :: ts2
  real(dp)                        :: ts3
  real(dp)                        :: ts4
  real(dp)                        :: ts5
  real(dp)                        :: x
  real(dp)                        :: zeta
  real(dp)                        :: zetar
!
!  Local variables
!
  if (lfirsttime) then
    r24 = 1.0_dp/24.0_dp
    small2 = 1.0d-13
    seventh = 1.0_dp/7.0_dp
    rnineth = 1.0_dp/9.0_dp
    lfirsttime = .false.
  endif
!
  sctrm1 = 0.0_dp
  sctrm2 = 0.0_dp
!
  if (lwolf) then
    etaloc = etaw*etaw
    setaloc = etaw
  else
    etaloc = eta
    setaloc = seta
  endif
! 
!  Save ereal on entry in case lskipq is true
!
  erealin = ereal
!
!  Coulomb subtraction flag
!
  lcoulsub = (lmol.and.(.not.lmolq.and..not.lmolmec))
  lc6loc = (lc6.and.ndim.eq.3)
!*********************
!  Zero derivatives  *
!*********************
  if (nor0.eq.nor) then
    d0i(nor) = 0.0_dp
    d0j(nor) = 0.0_dp
    deriv(nor) = 0.0_dp
    derive0(nor) = 0.0_dp
    derive(nor) = 0.0_dp
    rtrm1 = 0.0_dp
    d1i(nor) = 0.0_dp
    d1j(nor) = 0.0_dp
  else
    do k = nor0,nor
      d0i(k) = 0.0_dp
      d0j(k) = 0.0_dp
      deriv(k) = 0.0_dp
      derive0(k) = 0.0_dp
      derive(k) = 0.0_dp
      d1i(k) = 0.0_dp
      d1j(k) = 0.0_dp
    enddo
    rtrm1 = 0.0_dp
  endif
!*****************************
!  Main loop over distances  *
!*****************************
  do k = nor0,nor
    r = dist(k)
    r2 = r*r
    rk = 1.0_dp/r
    rk2 = rk*rk
    if (abs(r2-small2).lt.small2) r2 = small2
!
!  Skip interatomic potentials if this is a region 1 (defective-perfect)
!  calculation to avoid numerical problems
!
    if ((r2.le.cut2r.or.lbonded(k)).and.npots.gt.0.and..not.lskipsr) then
!
!  Store potential terms in local variables so that tapering
!  can be applied after loop over potentials
!
      eatm = 0.0_dp
      derivk = 0.0_dp
      rtrm1k = 0.0_dp
      sumtapergrad = 0.0_dp
      sumtaperpot = 0.0_dp
!************************************
!  Interatomic two body potentials  *
!************************************
      do m = 1,npots
        npot = npotl(m)
!
!  Inter/intramolecular checks for validity
!
        lvalid = ((lintra(npot).and.lptrmol(k)).or.(linter(npot).and..not.lptrmol(k))) 
!
!  Molecular mechanics checks for validity
!
        if (lvalid) then
          if (mmexc(npot).eq.1.and..not.lbonded(k)) lvalid = .false.
          if (mmexc(npot).ge.2.and.lbonded(k)) lvalid = .false.
          if (mmexc(npot).ge.3.and.l2bonds(k)) lvalid = .false.
          if (mmexc(npot).eq.4.and..not.l3bonds(k)) lvalid = .false.
          if (mmexc(npot).eq.5.and..not.l3bonds(k)) lvalid = .false.
!
!  Bond order checks for validity
!
          if (n2botype(1,npot).gt.0) then
            if (n2botype(1,npot).ne.nbotype(k)) lvalid = .false.
          endif
          if (n2botype(2,npot).gt.0) then
            if (n2botype(2,npot).ne.nbotype2(k)) lvalid = .false.
          endif
        endif
        if (lvalid) then
          if ((r.gt.rpot2(npot).and.r.le.rpot(npot)).or.mmexc(npot).eq.1.and.lbonded(k)) then
            nptyp = nptype(npot)
!
!  1-4 scaling option
!
            if (l3bonds(k)) then
              pscale = scale14(npot)
            else
              pscale = 1.0_dp
            endif
            psfct = sfct*pscale
!
!  Taper contributions
!
            sumtapergrad = sumtapergrad + psfct*tapergrad(npot)
            sumtaperpot = sumtaperpot + psfct*taperpot(npot)
!
            if (nptyp.eq.1) then
!*************************
!  Buckingham potential  *
!*************************
              cpt = twopot(3,npot)*psfct
              if (r.lt.repcut(npot)) then
                apt = twopot(1,npot)*psfct
                bpt = rhopot(npot)
                trm1 = apt*exp(-bpt*(r-radsum))
                eatm = eatm + trm1
                trm1 = trm1*bpt
                derivk = derivk - trm1*rk
                rtrm1k = rtrm1k + trm1
              endif
              if (cpt.gt.0.0_dp.and..not.lc6loc) then
                r6 = cpt*rk2**3
                eatm = eatm - r6
                r6 = 6.0_dp*r6*rk2
                derivk = r6 + derivk
              endif
              eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*psfct
            elseif (nptyp.eq.2.or.nptyp.eq.21) then
!****************************
!  Lennard-Jones potential  *
!****************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)*psfct
              rnpt = tpot(2,npot)
              if (r.lt.repcut(npot)) then
                rmpt = tpot(1,npot)
                if (abs(rmpt-12.0_dp).lt.1.0d-12) then
                  r6 = rk2*rk2*rk2
                  r12 = apt*r6*r6
                elseif (abs(rmpt-9.0_dp).lt.1.0d-12) then
                  r3 = rk*rk2
                  r6 = r3*r3
                  r12 = apt*r6*r3
                else
                  r12 = apt*rk**rmpt
                endif
                eatm = eatm + r12
                r12 = rmpt*r12
                derivk = derivk - r12*rk2
                if (bpt.gt.0.0_dp.and.(.not.lc6loc.or.abs(rnpt-6.0_dp).gt.1.0d-12)) then
                  if (abs(rmpt-12.0_dp)+abs(rnpt-6.0_dp).lt.1.0d-12) then
                    r6 = bpt*r6
                  elseif (abs(rmpt-9.0_dp)+abs(rnpt-6.0_dp).lt.1.0d-12) then
                    r6 = bpt*r3*r3
                  else
                    r6 = bpt*rk**rnpt
                  endif
                  eatm = eatm - r6
                  r6 = rnpt*r6
                  derivk = r6*rk2 + derivk
                endif
              else
                if (bpt.gt.0.0_dp.and.(.not.lc6loc.or.abs(rnpt-6.0_dp).gt.1.0d-12)) then
                  if (abs(rnpt-6.0_dp).lt.1.0d-12) then
                    r6 = bpt*rk2*rk2*rk2
                  else
                    r6 = bpt*rk**rnpt
                  endif
                  eatm = eatm - r6
                  r6 = rnpt*r6
                  derivk = r6*rk2 + derivk
                endif
              endif
              eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*psfct
            elseif (nptyp.eq.3) then
!*********************************************
!  Morse potential - no coulomb subtraction  *
!*********************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              trme = exp(-bpt*(r-cpt))
              trm1 = 1.0_dp - trme
              trm2 = 2.0_dp*apt*bpt*trme
              derivk = trm2*trm1*rk + derivk
              trm1 = trm1*trm1
              eatm = eatm + apt*(trm1 - 1.0_dp)
              eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*psfct
            elseif (nptyp.eq.4) then
!***********************************************
!  Morse potential - with coulomb subtraction  *
!***********************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              dpte = twopot(4,npot)*pscale
              dpt = dpte*factor
              dpte = dpte*sfct
              trme = exp(-bpt*(r-cpt))
              trm1 = 1.0_dp - trme
              trm2 = 2.0_dp*apt*bpt*trme
              derivk = trm2*trm1*rk + derivk
              derive(k) = dpte*rk2*rk + derive(k)
              trm1 = trm1*trm1
              eatm = eatm + apt*(trm1-1.0_dp)
              ereal = ereal - dpt*rk
              derive0(k) = derive0(k) - dpte*rk
            elseif (nptyp.eq.5) then
!*******************************************
!  Harmonic potential - no coulomb offset  *
!*******************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              dpt = twopot(4,npot)
              rd = r - bpt
              rd2 = rd*rd
              eatm = eatm + 0.5_dp*apt*rd2
              derivk = apt*rd*rk + derivk
              if (cpt.ne.0.0_dp) then
                cpt = cpt*sfct
                eatm = eatm + sixth*cpt*rd2*rd
                derivk = derivk + 0.5_dp*rd2*cpt*rk
              endif
              if (dpt.ne.0.0_dp) then
                dpt = dpt*sfct
                eatm = eatm + 0.25_dp*sixth*dpt*rd2*rd2
                derivk = derivk + sixth*rd2*rd*dpt*rk
              endif
              eatm = eatm + sfct*(eshift(npot) + gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*sfct
            elseif (nptyp.eq.6) then
!*********************************************
!  Harmonic potential - with coulomb offset  *
!*********************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              tpte = tpot(1,npot)*pscale
              tpt = tpte*factor
              tpte = tpte*sfct
              cpt = twopot(3,npot)
              dpt = twopot(4,npot)
              rd = r - bpt
              rd2 = rd*rd
              eatm = eatm + 0.5_dp*apt*rd2
              ereal = ereal - tpt*rk
              derive0(k) = derive0(k) - tpte*rk
              derivk = apt*rd*rk + derivk
              derive(k) = tpte*rk2*rk + derive(k)
              if (cpt.ne.0.0_dp) then
                cpt = cpt*sfct
                eatm = eatm + sixth*cpt*rd2*rd
                derivk = derivk + 0.5_dp*rd2*cpt*rk
              endif
              if (dpt.ne.0.0_dp) then
                dpt = dpt*sfct
                eatm = eatm + 0.25_dp*sixth*dpt*rd2*rd2
                derivk = derivk + sixth*rd2*dpt*rk
              endif
            elseif (nptyp.eq.7) then
!**********************
!  General potential  *
!**********************
              apt = twopot(1,npot)*psfct
              bpt = rhopot(npot)
              cpt = twopot(3,npot)*psfct
              mpt = int(tpot(1,npot))
              npt = int(tpot(2,npot))
              if (r.lt.repcut(npot)) then
                r12 = rk**mpt
                trm1 = apt*exp(-bpt*r)
                eatm = eatm + trm1*r12
                derivk = - trm1*bpt*rk*r12 + derivk
                derivk = derivk - trm1*mpt*rk2*r12
              endif
              if (cpt.gt.0.0_dp.and.(.not.lc6loc.or.npt.ne.6)) then
                r6 = cpt*rk**npt
                eatm = eatm - r6
                r6 = npt*r6*rk2
                derivk = r6 + derivk
              endif
              eatm = eatm + psfct*(eshift(npot)+gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*psfct
            elseif (nptyp.eq.8) then
!*****************************************
!  Spring potential - core - shell only  *
!*****************************************
              apt = twopot(1,npot)*sfct
              bpt = twopot(2,npot)*r24*sfct
              trm = bpt*r2
              eatm = eatm + 0.5_dp*apt*r2 + trm*r2
              derivk = apt + 4.0_dp*trm + derivk
            elseif (nptyp.eq.9) then
!**************************
!  Coulomb subtract only  *
!**************************
              dpte = twopot(4,npot)*pscale
              dpt = factor*dpte
              dpte = dpte*sfct
              ereal = ereal - dpt*rk
              derive0(k) = derive0(k) - dpte*rk
              derive(k) = derive(k) + dpte*rk2*rk
            elseif (nptyp.eq.10) then
!************************************
!  Four range buckingham potential  *
!************************************
              if (r.lt.tpot(1,npot)) then
                apt = twopot(1,npot)*psfct
                bpt = rhopot(npot)
                trm1 = apt*exp(-bpt*r)
                eatm = eatm + trm1
                trm1 = trm1*bpt
                derivk = derivk - trm1*rk
              elseif (r.lt.twopot(4,npot)) then
                norder = 5
                rtrm = psfct
                do l = 1,norder+1
                  tp = tpot(2+l,npot)
                  eatm = eatm + tp*rtrm
                  trm = tp*(l-1)*rtrm*rk2
                  derivk = derivk + trm
                  rtrm = rtrm*r
                enddo
              elseif (r.lt.tpot(2,npot)) then
                norder = 3
                rtrm = psfct
                do l = 1,norder+1
                  tp = tpot(8+l,npot)
                  eatm = eatm + tp*rtrm
                  trm =  tp*(l-1)*rtrm*rk2
                  derivk = derivk + trm
                  rtrm = rtrm*r
                enddo
              else
                cpt = twopot(3,npot)*psfct
                r6 = cpt*rk2**3
                eatm = eatm - r6
                r6 = 6.0_dp*r6*rk2
                derivk = r6 + derivk
              endif
            elseif (nptyp.eq.11) then
!*********************
!  Spline potential  *
!*********************
              apt = twopot(1,npot)
              call spline(npot,nsplpt(npot),splr(1,npot),splf(1,npot),r+apt,dfun,nsplty(npot),.true.,.false.)
              eatom = eatom + dfun(1)*psfct
              deriv(k) = deriv(k) + dfun(2)*rk*psfct
            elseif (nptyp.eq.12.or.nptyp.eq.13) then
!***************************************************
!  Lennard-Jones potential - epsilon/sigma format  *
!***************************************************
              apt = twopot(1,npot)*twopot(3,npot)*psfct
              bpt = twopot(1,npot)*twopot(4,npot)*psfct
              sig = twopot(2,npot)
              rmpt = tpot(1,npot)
              rnpt = tpot(2,npot)
              r6 = bpt*(sig*rk)**rnpt
              r12 = apt*(sig*rk)**rmpt
              eatm = eatm + (r12 - r6)
              r6 = rnpt*r6
              r12 = rmpt*r12
              derivk = (r6-r12)*rk2 + derivk
            elseif (nptyp.eq.15) then
!**************************************
!  Stillinger-Weber 2 body potential  *
!**************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              rmax = sqrt(cut2r)
              if (r.lt.rmax) then
                trm1 = 1.0_dp/(r-rmax)
                apt = apt*exp(bpt*trm1)
                trm2 = rk2*rk2*cpt
              else
                apt = 0.0_dp
                trm1 = 0.0_dp
                trm2 = 0.0_dp
              endif
              trm3 = trm2 - 1.0_dp
              eatom = eatom + apt*trm3
              trm4 = trm1*trm1
              deriv(k) = deriv(k) - apt*rk*(bpt*trm4*trm3+4.0_dp*trm2*rk)
            elseif (nptyp.eq.16) then
!*******************************
!  Inverse Gaussian potential  *
!*******************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              trm1 = bpt*(r-cpt)
              trme = apt*exp(-trm1*(r-cpt))
              eatm = eatm - trme
              derivk = derivk + 2.0_dp*trm1*trme*rk
              eatm = eatm + psfct*(eshift(npot)+gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*psfct
            elseif (nptyp.eq.18) then
!**********************
!  Damped dispersion  *
!**********************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)*psfct
              cpt = tpot(1,npot)*psfct
              b6 = twopot(3,npot)
              b8 = twopot(4,npot)
              b10 = tpot(2,npot)
              if (b6.eq.0.0.and.b8.eq.0.0.and.b10.eq.0.0) then
!
!  No damping
!
                rk6 = rk2*rk2*rk2
                rk8 = rk6*rk2
                rk10 = rk8*rk2
                eatm = eatm - apt*rk6 - bpt*rk8 - cpt*rk10
                rk6 = 6.0_dp*rk8
                rk8 = 8.0_dp*rk8*rk2
                rk10 = 10.0_dp*rk10*rk2
                derivk = derivk + apt*rk6 + bpt*rk8 + cpt*rk10
              else
!
!  Damping
!
                rk6 = rk2*rk2*rk2
                rk8 = rk6*rk2
                rk10 = rk8*rk2
                if (b6.ne.0.0_dp) then
                  br6 = b6*r
                  ebr6 = exp(-br6)
                  f6 = 1.0_dp+br6*(1.0_dp+0.5_dp*br6*(1.0_dp+third*br6*( &
                    1.0_dp+0.25_dp*br6*(1.0_dp+0.2_dp*br6*(1.0_dp+sixth*br6)))))
                else
                  ebr6 = 1.0_dp
                  f6 = 0.0_dp
                endif
                if (b8.ne.0.0_dp) then
                  br8 = b8*r
                  ebr8 = exp(-br8)
                  f8 = 1.0_dp+br8*(1.0_dp+0.5_dp*br8*(1.0_dp+third*br8*( &
                    1.0_dp+0.25_dp*br8*(1.0_dp+0.2_dp*br8*(1.0_dp+sixth &
                    *br8*(1.0_dp+seventh*br8*(1.0_dp+0.125_dp*br8)))))))
                else
                  ebr8 = 1.0_dp
                  f8 = 0.0_dp
                endif
                if (b10.ne.0.0_dp) then
                  br10 = b10*r
                  ebr10 = exp(-br10)
                  f10 = 1.0_dp+br10*(1.0_dp+0.5_dp*br10*(1.0_dp+third* &
                    br10*(1.0_dp+0.25_dp*br10*(1.0_dp+0.2_dp*br10*( &
                    1.0_dp+sixth*br10*(1.0_dp+seventh*br10*(1.0_dp+ &
                    0.125_dp*br10*(1.0_dp+rnineth*br10*(1.0_dp+0.1_dp &
                    *br10)))))))))
                else
                  ebr10 = 1.0_dp
                  f10 = 0.0_dp
                endif
                f6 = 1.0_dp - f6*ebr6
                f8 = 1.0_dp - f8*ebr8
                f10 = 1.0_dp - f10*ebr10
                eatm = eatm - apt*rk6*f6-bpt*rk8*f8-cpt*rk10*f10
                drk6 = - 6.0_dp*rk6*rk
                drk8 = - 8.0_dp*rk8*rk
                drk10 = - 10.0_dp*rk10*rk
                if (b6.ne.0.0_dp) then
                  df6 = ebr6*(b6*(br6**6))/720.0_dp
                else
                  df6 = 0.0_dp
                endif
                if (b8.ne.0.0_dp) then
                  df8 = ebr8*(b8*(br8**8))/40320.0_dp
                else
                  df8 = 0.0_dp
                endif
                if (b10.ne.0.0_dp) then
                  df10 = ebr10*(b10*(br10**10))/3628800.0_dp
                else
                  df10 = 0.0_dp
                endif
                derivk = derivk - apt*(rk6*df6+drk6*f6)*rk
                derivk = derivk - bpt*(rk8*df8+drk8*f8)*rk
                derivk = derivk - cpt*(rk10*df10+drk10*f10)*rk
              endif
            elseif (nptyp.eq.19) then
!**************************
!  Many-body EAM density  *
!**************************
              if (.not.lMEAMden) then
                rhoij(1) = 0.0_dp
                rhoji(1) = 0.0_dp
                call eamrho(nspec1(npot),nptyp1(npot),nspec2(npot),nptyp2(npot),r,rpot(npot),xtmp(k),ytmp(k),ztmp(k), &
                            rhoij,rhoji,drhoij,drhoji,drhoijs,drhojis,drhoij2,drhoji2,drhoij2s,drhoji2s,drhoij2m,drhoji2m, &
                            drhoij3,drhoji3,1.0_dp,1.0_dp,.false.,.false.,.false.,.false.)
                sctrm1 = sctrm1 + rhoij(1)
                sctrm2 = sctrm2 + rhoji(1)
              endif
            elseif (nptyp.eq.20) then
!*****************************************
!  Rose-Smith-Guinea-Ferrante potential  *
!*****************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              r0 = twopot(3,npot)
              rr0 = 1.0_dp/r0
              trm1 = bpt*(rr0*r - 1.0_dp)
              exptrm = apt*exp(-trm1)
              eatom = eatom - (1.0_dp + trm1)*exptrm
              exptrm = exptrm*rr0*bpt
              deriv(k) = deriv(k) + exptrm*trm1*rk
            elseif (nptyp.eq.22) then
!***********
!  Qtaper  *
!***********
              ts0 = tpot(5,npot)
              ts1 = tpot(6,npot)
              ts2 = tpot(7,npot)
              ts3 = tpot(8,npot)
              ts4 = tpot(9,npot)
              ts5 = tpot(10,npot)
              e01 = - factor*rk
              e02 = twopot(1,npot)*psfct
              e0t = e01 + e02
              tpfn = ((((ts5*r+ts4)*r+ts3)*r+ts2)*r+ts1)*r+ts0
              eatom = eatom + tpfn*e0t
              e11 = - e01*rk
              dtpfn = (((5.0_dp*ts5*r+4.0_dp*ts4)*r+3.0_dp*ts3)*r+2.0_dp*ts2)*r+ts1
              deriv(k) = deriv(k) + (tpfn*e11+dtpfn*e0t)*rk
            elseif (nptyp.eq.23) then
!************************
!  Polynomial potential *
!************************
              norder = nint(twopot(4,npot))
              r0 = twopot(1,npot)
              rtrm = psfct
              rtrm1d = 0.0_dp
              do l = 1,norder+1
                tp = tpot(l,npot)
                eatm = eatm + tp*rtrm
                trm = tp*(l-1)*rtrm1d*rk
                derivk = derivk + trm
                rtrm = rtrm*(r - r0)
                if (l.eq.1) then
                  rtrm1d = psfct
                else
                  rtrm1d = rtrm1d*(r - r0)
                endif
              enddo
            elseif (nptyp.eq.24) then
!**********
!  Qerfc  *
!**********
              apt = twopot(1,npot)
              rapt = 1.0_dp/apt
              erffc = derfc(r*rapt)
              trm0 = erffc*rk*factor
              eatom = eatom + trm0
              rapt2 = rapt*rapt
              trm1 = 2.0_dp*rapt*exp(-r2*rapt2)/sqrtpi
              trm2 = (trm0 + trm1*factor)*rk2
              deriv(k) = deriv(k) - trm2
              eatm = eatm + factor*(eshift(npot) + gshift(npot)*r)
              deriv(k) = deriv(k) + gshift(npot)*rk*factor
            elseif (nptyp.eq.25) then
!***********
!  CovExp  *
!***********
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)*0.5_dp
              cpt = twopot(3,npot)
              trme = apt*exp(-bpt*((r-cpt)**2)/r)
              eatm = eatm - trme
              trm1 = bpt*(1.0_dp-cpt*cpt*rk2)
              derivk = trme*trm1*rk + derivk
            elseif (nptyp.eq.26) then
!**************************
!  Fermi-Dirac potential  *
!**************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              trme = exp(bpt*(r-cpt))
              rtrm = 1.0_dp/(1.0_dp+trme)
              trm1 = apt*rtrm
              eatm = eatm + trm1
              trm1 = trm1*bpt*rtrm*trme
              derivk = derivk - trm1*rk
              eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*psfct
            elseif (nptyp.eq.27) then
!*************************************
!  Lennard-Jones buffered potential  *
!*************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)*psfct
              rkd = 1.0_dp/(r + twopot(3,npot))
              if (r.lt.repcut(npot)) then
                rmpt = tpot(1,npot)
                r12 = apt*rkd**rmpt
                eatm = eatm + r12
                r12 = rmpt*r12
                derivk = derivk - r12*rk*rkd
              endif
              if (bpt.gt.0.0_dp) then
              rnpt = tpot(2,npot)
                r6 = bpt*rkd**rnpt
                eatm = eatm - r6
                r6 = rnpt*r6
                derivk = r6*rkd*rk + derivk
              endif
            elseif (nptyp.eq.28) then
!***************************************************
!  Squared Harmonic potential - no coulomb offset  *
!***************************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              dpt = twopot(4,npot)
              rd = r2 - bpt*bpt
              rd2 = rd*rd
              eatm = eatm + 0.25_dp*apt*rd2
              derivk = apt*rd + derivk
              eatm = eatm + sfct*(eshift(npot) + gshift(npot)*r)
              derivk = derivk + gshift(npot)*rk*sfct
            elseif (nptyp.eq.29) then
!*****************************************************
!  Squared Harmonic potential - with coulomb offset  *
!*****************************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              tpte = tpot(1,npot)*pscale
              tpt = tpte*factor
              tpte = tpte*sfct
              cpt = twopot(3,npot)
              dpt = twopot(4,npot)
              rd = r2 - bpt*bpt
              rd2 = rd*rd
              eatm = eatm + 0.25_dp*apt*rd2
              ereal = ereal - tpt*rk
              derive0(k) = derive0(k) - tpte*rk
              derivk = apt*rd + derivk
              derive(k) = derive(k) + tpte*rk2*rk
            elseif (nptyp.eq.30) then
!**************
!  Tsuneyuki  *
!**************
              apt = (twopot(1,npot)*twopot(2,npot)*psfct*angstoev - pscale*factor)
              zeta = twopot(3,npot)
              mpt = nint(tpot(1,npot))
              zetar = zeta*r
              trm1 = exp(-2.0_dp*zetar)
              if (mpt.eq.1) then
                gofr = (1.0_dp + zetar)*trm1
              elseif (mpt.eq.2) then
                gofr = (1.0_dp + zetar*(1.375_dp + zetar*(0.75_dp + zetar/6.0_dp)))*trm1
              endif
              eatom = eatom + apt*gofr*rk
              if (mpt.eq.1) then
                dgofr = zeta*trm1
              elseif (mpt.eq.2) then
                dgofr = zeta*(1.375_dp + zetar*(1.5_dp + zetar/2.0_dp))*trm1
              endif
              dgofr = dgofr - 2.0_dp*zeta*gofr
              derivk = derivk - apt*rk2*(gofr*rk - dgofr)
            elseif (nptyp.eq.32) then
!*****************************************************************
!  Stillinger-Weber 2 body potential - Jiang-Brown modification  *
!*****************************************************************
!
!  Note only first and second derivatives are given since charge derivatives are not available to higher order
!
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              dpt = twopot(4,npot)
              rmax = sqrt(cut2r)
              if (r.lt.rmax) then
                trm1 = 1.0_dp/(r-rmax)
                apt = apt*exp(bpt*trm1)
                trm2 = rk2*rk2*cpt
                if (qi+qj.lt.dpt) then
                  rqtrm = 1.0_dp/(qi+qj-dpt)
                  qtrm = exp(1.0_dp/dpt)*exp(rqtrm)
                else
                  rqtrm = 0.0_dp
                  qtrm = 0.0_dp
                endif
              else
                apt = 0.0_dp
                trm1 = 0.0_dp
                trm2 = 0.0_dp
                qtrm = 0.0_dp
              endif
              apt = apt*qtrm
              trm3 = trm2 - 1.0_dp
              eatom = eatom + apt*trm3
              trm4 = trm1*trm1
              deriv(k) = deriv(k) - apt*rk*(bpt*trm4*trm3+4.0_dp*trm2*rk)
              d0i(k) = d0i(k) - apt*trm3*rqtrm*rqtrm
              d0j(k) = d0j(k) - apt*trm3*rqtrm*rqtrm
            elseif (nptyp.eq.33) then
!**********************************************
!  Cosh-spring potential - core - shell only  *
!**********************************************
              apt = twopot(1,npot)*sfct
              bpt = twopot(2,npot)
              cpt = cosh(r/bpt)
              eatm = eatm + apt*bpt*bpt*(cpt - 1.0d0)
              dpt = sinh(r/bpt)
              derivk = derivk + apt*bpt*rk*dpt
            elseif (nptyp.eq.34) then
!********************************** 
!  EAM potential shift potential  *
!**********************************
              apt = 2.0_dp*twopot(1,npot)*sfct*autoev
              bpt = twopot(2,npot)
              exptrm = exp(-bpt*r)
              cpt = 2.0_dp**9
              trm1 = apt*exptrm*(1.0_dp + cpt*exptrm)
              eatm = eatm + r2*r2*r2*trm1
              trm2 = apt*exptrm*bpt*(1.0_dp + 2.0_dp*cpt*exptrm)
              derivk = derivk + r2*r2*(6.0_dp*trm1 - r*trm2)
            elseif (nptyp.eq.35) then
!***************************
!  Poly harmonic potential *
!***************************
              apt = twopot(1,npot)
              if (r.lt.apt) then
                norder = nint(twopot(4,npot))
                rtrm = psfct
                trm1 = (r - apt)**2
                dtrm1 = 2.0_dp*(r - apt)
                do l = 1,norder+1
                  tp = tpot(l,npot)
                  eatm = eatm + tp*rtrm*trm1
                  trm = tp*(l-1)*rtrm*rk2
                  if (l.gt.1) then
                    derivk = derivk + trm*trm1 + tp*rtrm*dtrm1*rk
                  else
                    derivk = derivk + tp*rtrm*dtrm1*rk
                  endif
                  rtrm = rtrm*r
                enddo
              endif
            elseif (nptyp.eq.36) then
!***********
!  QoverR2 *
!***********
              trm0 = rk2*factor
              eatom = eatom + trm0
              trm1 = - 2.0_dp*trm0*rk2
              deriv(k) = deriv(k) + trm1
              eatm = eatm + factor*(eshift(npot) + gshift(npot)*r)
              deriv(k) = deriv(k) + gshift(npot)*rk*factor
            elseif (nptyp.eq.37) then
!*****************************
!  Force constant potential  *
!*****************************
              bpt = twopot(2,npot)*sfct
              derivk = derivk + bpt
            elseif (nptyp.eq.38) then
!***********
!  SRGlue  *
!***********
              rd  = r - twopot(1,npot)
              rd2 = rd*rd
              rd3 = rd2*rd
              rd4 = rd3*rd
              if (r.lt.twopot(1,npot)) then
                eatm = eatm + sfct*(tpot(1,npot)*rd4 + tpot(2,npot)*rd3 + tpot(3,npot)*rd2 + &
                                    tpot(4,npot)*rd  + tpot(5,npot))
                trm1 = sfct*(4.0_dp*tpot(1,npot)*rd3 + 3.0_dp*tpot(2,npot)*rd2 + &
                             2.0_dp*tpot(3,npot)*rd  +        tpot(4,npot))
                deriv(k) = deriv(k) + trm1*rk
              else
                rd5 = rd4*rd
                rd6 = rd5*rd
                eatm = eatm + sfct*(tpot(6,npot)*rd6 + tpot(7,npot)*rd5 + tpot(8,npot)*rd4 + &
                                    tpot(9,npot)*rd3 + tpot(10,npot)*rd2 + tpot(11,npot)*rd+ &
                                    tpot(12,npot))
                trm1 = sfct*(6.0_dp*tpot(6,npot)*rd5 + 5.0_dp*tpot(7,npot)*rd4 + &
                             4.0_dp*tpot(8,npot)*rd3 + 3.0_dp*tpot(9,npot)*rd2 + &
                             2.0_dp*tpot(10,npot)*rd +        tpot(11,npot))
                deriv(k) = deriv(k) + trm1*rk
              endif
            elseif (nptyp.eq.39) then
!*********************************************************
!  Morse potential with etaper - no coulomb subtraction  *
!*********************************************************
              call etaper(r,0.0_dp,rpot(npot),etpfn,detpfn,d2etpfn,d3etpfn,.true.,.false.,.false.)
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              trme = exp(-bpt*(r-cpt))
              trm1 = 1.0_dp - trme
              trm12 = trm1*trm1
              eatmtrm = apt*(trm12 - 1.0_dp)*etpfn
              eatm = eatm + eatmtrm*etpfn
              trm2 = 2.0_dp*apt*bpt*trme
              derivktrm = trm2*trm1*rk
              derivk = derivk + derivktrm*etpfn + eatmtrm*detpfn*rk
            elseif (nptyp.eq.40) then
!***********************************************************
!  Morse potential with etaper - with coulomb subtraction  *
!***********************************************************
              call etaper(r,0.0_dp,rpot(npot),etpfn,detpfn,d2etpfn,d3etpfn,.true.,.false.,.false.)
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              dpte = twopot(4,npot)*pscale
              dpt = dpte*factor
              dpte = dpte*sfct
              trme = exp(-bpt*(r-cpt))
              trm1 = 1.0_dp - trme
              trm12 = trm1*trm1
              eatmtrm = apt*(trm12 - 1.0_dp)*etpfn
              eatm = eatm + eatmtrm*etpfn
              ereal = ereal - dpt*rk
              trm2 = 2.0_dp*apt*bpt*trme
              derivktrm = trm2*trm1*rk
              derivk = derivk + derivktrm*etpfn + eatmtrm*detpfn*rk
              derive(k) = dpte*rk2*rk + derive(k)
              derive0(k) = derive0(k) - dpte*rk
            elseif (nptyp.eq.41) then
!************************************
!  Mei-Davenport twobody potential  *
!************************************
              apt = twopot(1,npot)*psfct
              bpt = twopot(2,npot)
              cpt = twopot(3,npot)
              dpt = 1.0_dp/twopot(4,npot)
              trme = exp(-cpt*(r*dpt - 1.0_dp))
              trm1 = 1.0_dp + bpt*(r*dpt - 1.0_dp)
              eatm = eatm - apt*trm1*trme
              derivk = derivk - apt*trme*dpt*(bpt - trm1*cpt)*rk
            elseif (nptyp.eq.42) then
!*************
!  Erf-erfc  *
!*************
              apt = twopot(1,npot)*psfct
              bpt = 1.0_dp/twopot(2,npot)
              cpt = 1.0_dp/twopot(3,npot)
              erftrm = derf(bpt*r)
              erfctrm = derfc(cpt*r)
              trm0 = apt*erftrm*erfctrm
              eatm = eatm + trm0*rk
              exptrm1 = exp(-(bpt*r)**2)
              exptrm2 = exp(-(cpt*r)**2)
              derivk = derivk + (2.0_dp*apt*rk2/sqrtpi)*(bpt*exptrm1*erfctrm - erftrm*cpt*exptrm2)
              derivk = derivk - trm0*rk2*rk
            elseif (nptyp.eq.43) then
!*************
!  Rep-erfc  *
!*************
              apt = twopot(1,npot)*psfct
              bpt = 1.0_dp/twopot(2,npot)
              erfctrm = derfc(bpt*r)
              trm0 = apt*erfctrm*rk
              eatm = eatm + trm0
              exptrm1 = exp(-(bpt*r)**2)
              derivk = derivk - apt*rk2*((2.0_dp/sqrtpi)*bpt*exptrm1 + erfctrm*rk)
            elseif (nptyp.eq.44) then
!************
!  Erf pot  *
!************
              apt = twopot(1,npot)*psfct
              bpt = 1.0_dp/twopot(2,npot)
              erftrm = derf(bpt*r)
              trm0 = apt*erftrm*rk
              eatm = eatm + trm0
              exptrm1 = exp(-(bpt*r)**2)
              derivk = derivk + apt*rk2*((2.0_dp/sqrtpi)*bpt*exptrm1 - erftrm*rk)
            elseif (nptyp.eq.45) then
!***************
!  Baskes pot  *
!***************
              EcZ  = - 2.0_dp*twopot(1,npot)*psfct/twopot(5,npot) ! - (2/Z)*Ec
              apt  = twopot(2,npot)  ! A
              bpt  = twopot(3,npot)  ! alpha
              r0   = twopot(4,npot)  ! r0
              rho0 = twopot(6,npot)  ! rho0
              d    = twopot(7,npot)  ! d
!
!  Compute the rho_ref value
!
              frho = 0.0_dp
              dfrhodr = 0.0_dp
              do l = 1,maxmeamorder
                betaoverr0 = 2.0_dp*tpot(8+l,npot)/r0
                rhol = tpot(l,npot)*tpot(4+l,npot)*exp(-betaoverr0*(r-r0))
                frho = frho + rhol
                dfrhodr = dfrhodr - rhol*betaoverr0
              enddo
              rho = sqrt(frho)
              x = (rho/rho0)
!
              dtrm0dr = bpt/r0
              trm0 = dtrm0dr*(r - r0)
              exptrm = exp(-trm0)
              logtrm = log(x)
              eatm = eatm + EcZ*((1.0_dp + trm0)*exptrm + apt*x*logtrm)
              rrho = 1.0_dp/rho
              dxdr = 0.5_dp*rrho*dfrhodr/rho0
              deriv(k) = deriv(k) - EcZ*(exptrm*trm0*dtrm0dr*(1.0_dp-d*trm0*(3.0_dp-trm0)) - apt*dxdr*(1.0_dp + logtrm))*rk
            elseif (nptyp.eq.46) then
!********************
!  VBO twobody pot  *
!********************
              apt  = twopot(1,npot)*twopot(5,npot)*psfct ! c.N
              bpt  = twopot(2,npot)  ! gamma
              cpt  = twopot(4,npot)  ! delta
!
              x = sqrt(r/cpt)
              rxm1 = 1.0_dp/(1.0_dp - x)
              exptrm = exp(-bpt*rxm1)
              eatm = eatm + apt*exptrm
              trm1 = - apt*exptrm*bpt*rxm1*rxm1
              dxdr = 0.5_dp/(cpt*x)
              deriv(k) = deriv(k) + trm1*dxdr*rk
            elseif (nptyp.eq.47) then
!***************************
!  Exp-powers twobody pot  *
!***************************
              apt  = twopot(1,npot)*psfct ! A
              bpt  = twopot(2,npot)       ! B0
              cpt  = twopot(3,npot)       ! B1
              ept  = twopot(4,npot)       ! B2
              fpt  = twopot(5,npot)       ! B3
!
              exptrm = exp(bpt+r*(cpt+r*(ept+fpt*r)))
              eatm = eatm + apt*exptrm
              trm1 = cpt + r*(2.0_dp*ept + 3.0_dp*fpt*r)
              deriv(k) = deriv(k) + apt*exptrm*trm1*rk
            elseif (nptyp.eq.48) then
!**************************
!  Grimme_C6 twobody pot  *
!**************************
              apt  = twopot(1,npot)*psfct ! C6
              bpt  = twopot(2,npot)       ! d
              cpt  = twopot(3,npot)       ! r0
!
              exptrm = exp(-bpt*(r/cpt - 1.0_dp))
              fdmp = 1.0_dp/(1.0_dp + exptrm)
              rk6 = rk2*rk2*rk2
              eatm = eatm - apt*rk6*fdmp
              trm1 = - 6.0_dp*rk6*rk
              dfdmp = fdmp*fdmp*bpt*exptrm/cpt
              deriv(k) = deriv(k) - apt*(trm1*fdmp + rk6*dfdmp)*rk
            elseif (nptyp.eq.49) then
!*********************
!  CFM harmonic pot  *
!*********************
              apt  = twopot(1,npot)*psfct ! k
              bpt  = twopot(2,npot)       ! r0
              ept  = twopot(3,npot)       ! R
              fpt  = twopot(4,npot)       ! w
!
!  Compute taper function, th, and derivatives
!
              call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,.true.,.false.,.false.)
!
              ecfm = 0.5_dp*apt*(r - bpt)**2
              eatm = eatm + ecfm*(1.0_dp - th)
              decfm = apt*(r - bpt)
              deriv(k) = deriv(k) + rk*(-ecfm*dthdr + decfm*(1.0_dp - th))
            elseif (nptyp.eq.50) then
!*********************
!  CFM gaussian pot  *
!*********************
              apt  = twopot(1,npot)*psfct ! k
              bpt  = twopot(2,npot)       ! zeta
              cpt  = twopot(3,npot)       ! r0
              ept  = twopot(4,npot)       ! R
              fpt  = twopot(5,npot)       ! w
!
!  Compute taper function, th, and derivatives
!
              call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,.true.,.false.,.false.)
!
              exptrm = exp(-bpt*(r-cpt)**2)
              ecfm = apt*exptrm
              eatm = eatm + ecfm*th       
              decfm = - 2.0_dp*bpt*(r - cpt)*ecfm
              deriv(k) = deriv(k) + rk*(ecfm*dthdr + decfm*th)
            elseif (nptyp.eq.51) then
!******************
!  CFM power pot  *
!******************
              apt  = twopot(1,npot)*psfct ! A
              bpt  = twopot(2,npot)       ! power
              ept  = twopot(3,npot)       ! R
              fpt  = twopot(4,npot)       ! w
!
!  Compute taper function, th, and derivatives
!
              call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,.true.,.false.,.false.)
!
              ecfm = apt*rk**bpt
              eatm = eatm + ecfm*th       
              decfm = - bpt*ecfm*rk
              deriv(k) = deriv(k) + rk*(ecfm*dthdr + decfm*th)
            elseif (nptyp.eq.52) then
!******************
!  CFM fermi pot  *
!******************
              apt  = twopot(1,npot)*psfct ! k
              bpt  = twopot(2,npot)       ! zeta
              cpt  = twopot(3,npot)       ! r0
              ept  = twopot(4,npot)       ! R
              fpt  = twopot(5,npot)       ! w
!
!  Compute taper function, th, and derivatives
!
              call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,.true.,.false.,.false.)
!
              exptrm = exp(bpt*(r - cpt))
              rexptrm = 1.0_dp/(1.0_dp + exptrm)
              ecfm = apt*rexptrm
              eatm = eatm + ecfm*th       
              decfm = - ecfm*bpt*exptrm*rexptrm
              deriv(k) = deriv(k) + rk*(ecfm*dthdr + decfm*th)
            elseif (nptyp.eq.53) then
!********************************
!  gCoulomb subtract potential  *
!********************************
              apt  = twopot(1,npot)*psfct*angstoev ! A
              bpt  = twopot(2,npot)                ! gamma
!
              trm0 = 1.0_dp/(r**3 + bpt)
              rtrm0 = apt*(trm0**third)
              eatm = eatm - rtrm0
              trm1 = rtrm0*trm0*r
              deriv(k) = deriv(k) + trm1
            elseif (nptyp.eq.54) then
!*********************************
!  Becke_Johnson_C6 twobody pot  *
!*********************************
              apt  = twopot(1,npot)*psfct ! C6
              bpt  = twopot(2,npot)       ! r0
!
              r06 = bpt**6
              r6 = r2*r2*r2
              fdmp = 1.0_dp/(r6 + r06)
              eatm = eatm - apt*fdmp
              r4 = r2*r2
              deriv(k) = deriv(k) + 6.0_dp*apt*fdmp*fdmp*r4
            endif
          endif
        endif
      enddo
!*********************
!  Taper potentials  *
!*********************
      if (r.gt.tapermin.and.r.lt.tapermax) then
        if (tapertype.eq.3) then
!
!  Voter taper
!
          eatom = eatom + eatm - sumtaperpot + (tapermax/taperm)*sumtapergrad*(1.0_dp -  &
            (r/tapermax)**taperm)
          deriv(k) = deriv(k) + derivk - sumtapergrad*rk*(r/tapermax)**(taperm-1.0_dp)
          rtrm1 = rtrm1 + rtrm1k
        else
!
!  Polynomial, cosine or exponential taper
!
          if (tapertype.eq.1) then
            call p5taper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,.true.,.false.,.false.)
          elseif (tapertype.eq.4) then
            call etaper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,.true.,.false.,.false.)
          elseif (tapertype.eq.5) then
            call mdftaper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,.true.,.false.,.false.)
          else
            call ctaper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,.true.,.false.,.false.)
          endif
          eatom = eatom + tpfn*eatm
          deriv(k) = deriv(k) + tpfn*derivk + dtpfn*eatm*rk
          rtrm1 = rtrm1 + tpfn*rtrm1k
        endif
      else
        eatom = eatom + eatm
        deriv(k) = deriv(k) + derivk
        rtrm1 = rtrm1 + rtrm1k
      endif
    endif
    if (lc6loc) then
!*****************************
!  Ewald sum for dispersion  *
!*****************************
      c6tot = 0.0_dp
      r6 = rk2*rk2*rk2
!
!  Loop over potentials to sum C6 term and to
!  locate any potentials that need to be
!  subtracted due to rmin not equal to zero
!  or because of intermolecularity
!
      do m = 1,npots
        npot = npotl(m)
!
!  Inter/intramolecular checks for validity
!
        lvalid = ((lintra(npot).and.lptrmol(k)).or.(linter(npot).and..not.lptrmol(k))) 
!
!  Molecular mechanics checks for validity
!
        if (lvalid) then
          if (mmexc(npot).eq.1.and..not.lbonded(k)) lvalid = .false.
          if (mmexc(npot).ge.2.and.lbonded(k)) lvalid = .false.
          if (mmexc(npot).ge.3.and.l2bonds(k)) lvalid = .false.
          if (mmexc(npot).eq.4.and..not.l3bonds(k)) lvalid = .false.
          if (mmexc(npot).eq.5.and..not.l3bonds(k)) lvalid = .false.
!
!  Bond order checks for validity
!
          if (n2botype(1,npot).gt.0) then
            if (n2botype(1,npot).ne.nbotype(k)) lvalid = .false.
          endif
          if (n2botype(2,npot).gt.0) then
            if (n2botype(2,npot).ne.nbotype2(k)) lvalid = .false.
          endif
        endif
        if (r.lt.rpot2(npot)) lvalid=.false.
        nptyp = nptype(npot)
        if (nptyp.eq.1) then
! 
!  Buckingham potential
!
          cpt = twopot(3,npot)*sfct
          c6tot = c6tot + cpt
        elseif (nptyp.eq.2.or.nptyp.eq.21) then
!
!  Lennard-Jones potential
!
          cpt = twopot(2,npot)*sfct
          c6tot = c6tot + cpt
        elseif (nptyp.eq.7) then
!
!  General potential
!
          cpt = twopot(3,npot)*sfct
          c6tot = c6tot + cpt
        else 
          cpt = 0.0_dp
        endif
!
!  If not valid for this distance then remove term
!
        if (.not.lvalid.and.abs(cpt).gt.0.0_dp) then
          ctrm1 = cpt*r6
          ec6 = ec6 + ctrm1
          ctrm1 = 6.0_dp*ctrm1*rk2
          deriv(k) = deriv(k) - ctrm1
        endif
      enddo
      if (lperiodic) then
        if (r2.le.cut2e.and.abs(c6tot).gt.0.0_dp) then
          etar2 = eta*r2
          exptrm = exp(-etar2)
          c6t1 = 1.0_dp + etar2*(1.0_dp + 0.5_dp*etar2)
          c6t2 = c6tot*r6*exptrm
          c6trm1 = c6t1*c6t2
          ec6 = ec6 - c6trm1
          deriv(k) = deriv(k) + c6trm1*(6.0_dp*rk2 + 2.0_dp*eta)
          deriv(k) = deriv(k) - 2.0_dp*c6t2*eta*(1.0_dp + etar2)
        endif
      else
!
!  Cluster case
!
        ctrm1 = c6tot*r6
        ec6 = ec6 - ctrm1
        ctrm1 = 6.0_dp*ctrm1*rk2
        deriv(k) = deriv(k) + ctrm1
      endif
    endif
    if (lDoElectrostatics) then
!***********************
!  Electrostatic part  *
!***********************
!
!  The core-shell interaction depends on the electrostatic method and dimensionality:
!
!    Pure Coulomb: - Periodic (Ewald sum) -> difference between the screened term and 1/r
!                  - Finite               -> zero
!    Wolf sum:     - Periodic             -> self term
!                  - Finite               -> zero
!
      if (lperiodic.or.lwolf) then
!
!  Bulk electrostatics
!
        if ((r2.le.cut2e.or.lptrmol(k)).and.(lewald.or.lwolf).and.abs(factor).gt.1.0d-12) then
          if (k.ne.nmolonly.or..not.lcoulsub) then
            lneedgrad1 = .true.
            if (r2.le.cut2s.and.r2.gt.small2.and.lcspair) then
              if (lperiodic) then
                if (lwolf) then
                  effc = 0.0_dp
                  lneedgrad1 = .false.
                else
                  effc = dserfc(setaloc,r,rk)
                endif
              else
                effc = 0.0_dp
                lneedgrad1 = .false.
              endif
            else
              effc = derfc(setaloc*r)
              effc = effc*rk
            endif
            ereal = ereal + effc*factor
            if (lneedgrad1) then
              trm1 = tweatpi*exp(-etaloc*r2)*sfct
              erffc = effc*sfct
              trm2 = rk2*(erffc + trm1)
              derive(k) = derive(k) - trm2
              derive0(k) = derive0(k) + effc*sfct
            endif
!
!  Variable charge terms
!
            if (leem.or.lDoQDeriv2) then
              d1i(k) = d1i(k) - trm2*angstoev*qj
              d1j(k) = d1j(k) - trm2*angstoev*qi
            endif
!
!  Wolf sum correction only applied if this is not a core-shell interaction
!
            if (lwolf.and.(r2.gt.cut2s.or..not.lcspair)) then
!
!  Subtract boundary image interaction
!
              effc = selfwolf
              ereal = ereal - effc*factor
              if (lwolforiginal) then
                trm1 = tweatpi*exp(-etaloc*cutw*cutw)*sfct
                erffc = effc*sfct
                trm2 = rkw*rkw*(erffc + trm1)
                derive(k) = derive(k) + trm2
                derive0(k) = derive0(k) - effc*sfct
!
!  Variable charge terms
!
                if (leem.or.lDoQDeriv2) then
                  d1i(k) = d1i(k) + trm2*angstoev*qj
                  d1j(k) = d1j(k) + trm2*angstoev*qi
                endif
              endif
            endif
          endif
!
!  Molecule - subtract electrostatic contribution
!
          if (lptrmol(k).and.(lcoulsub.or.(lmolmec.and.(lbonded(k).or.l2bonds(k))))) then
            trm1 = factor*rk
            ereal = ereal - trm1
            trm1 = sfct*rk*rk2
            derive(k) = derive(k) + trm1
            derive0(k) = derive0(k) - sfct*rk
!
!  Variable charge terms
!
            if (leem.or.lDoQDeriv2) then
              trm2 = trm1*angstoev
              d1i(k) = d1i(k) + trm2*qj
              d1j(k) = d1j(k) + trm2*qi
            endif
          endif
        endif
      elseif (ndim.eq.1) then
!
!  Polymer electrostatics:
!
!  These are handled elsewhere except for molecular Coulomb subtraction which is performed here
!
        if (lptrmol(k).and.(lcoulsub.or.(lmolmec.and.(lbonded(k).or.l2bonds(k))))) then
          ereal = ereal - factor*rk
          trm1 = rk*rk2*sfct
          derive(k) = derive(k) + trm1
          derive0(k) = derive0(k) - sfct*rk
!
!  Variable charge terms
!
          if (leem.or.lDoQDeriv2) then
            trm2 = trm1*angstoev
            d1i(k) = d1i(k) + trm2*qj
            d1j(k) = d1j(k) + trm2*qi
          endif
        endif
      else
!
!  Cluster electrostatics
!
        if (lptrmol(k).and.(lcoulsub.or.(lmolmec.and.(lbonded(k).or.l2bonds(k))))) then
          ereal = ereal
        else
          if ((r2.gt.cut2s.or..not.lcspair).and.r2.lt.cut2e) then
            ereal = ereal + factor*rk
            trm1 = rk*rk2*sfct
            derive(k) = derive(k) - trm1
            derive0(k) = derive0(k) + sfct*rk
!
!  Variable charge terms
!
            if (leem.or.lDoQDeriv2) then
              trm2 = trm1*angstoev
              d1i(k) = d1i(k) - trm2*qj
              d1j(k) = d1j(k) - trm2*qi
            endif
          endif
        endif
      endif
    endif
  enddo
!***************************
!  Final merging of terms  *
!***************************
!
!  First add on electrostatic contribution
!
  if (abs(sfct).gt.1.0d-15.and..not.lskipq) then
    rsfct = factor/sfct
    do k = nor0,nor
      deriv(k) = deriv(k) + rsfct*derive(k)
    enddo
  endif
!
!  Convert units of derive, derive2, derive3
!
  if (.not.lskipq) then
    do k = nor0,nor
      derive0(k) = angstoev*derive0(k)
      derive(k)  = angstoev*derive(k)
    enddo
  endif
!  
!  If electrostatics were to be skipped then reset ereal for return
!  (Although the computation of the terms could be avoided this is a
!   quick way to implement for the time being!)
!  
  if (lskipq) then
    ereal = erealin
  endif
!
  return
  end
