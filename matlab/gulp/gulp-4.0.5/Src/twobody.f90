  subroutine twobody(eatom,ereal,ec6,lgrad1,lgrad2,lgrad3,nor,nor0,npots,npotl, &
       cut2r,cut2e,cut2s,nmolonly,factor,sfct,radsum,rtrm1,sctrm1,sctrm2,qi,qj, &
       lcspair,lperiodic,lregion1,lskipsr,lskipq,lorder12)
!
!  Subroutine for calculating twobody energy for pair of atoms
!
!  eatom   = interatomic potential energy
!  ereal   = real space electrostatic energy
!  ec6     = real and recip space dispersion energy
!  lgrad1  = flag controlling whether first derivatives are calculated
!  lgrad2  = flag controlling whether second derivatives are calculated
!  nor     = pointer to final distance to calculate for
!  nor0    = pointer to first distance to calculate for
!  dist    = array of distances from nor0 to nor
!  deriv   = on return contains first derivative component
!  deriv2  = on return contains second derivative component
!  derive0 = on return contains electrostatic energy component with the charges
!  derive  = on return contains electrostatic first derivative component
!  derive2 = on return contains electrostatic second derivative component
!  derive3 = on return contains electrostatic third derivative component
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
!  rtrm2   = radial only second derivative term
!  rtrm3   = radial only third derivative term
!  rtrm32  = radial-radial only third derivative term
!  sctrm1/2= contribution to many-body rho value for EAM
!  qi      = charge of ion i in pair
!  qj      = charge of ion j in pair
!  lcspair = possible core-shell pair
!  lperiodic=flag deciding whether Ewald or normal 1/r is to be used
!  lregion1= flag relating to whether calc is for region 1 in defect calc
!  lbonded = flag indicating whether atoms are bonded for MM calc
!  l2bonds = flag indicating whether atoms are bonded to a common atom
!  l3bonds = flag indicating whether atoms are connected via 3 bonds
!  repcut  = cutoff for exponential repulsion terms
!  lskipsr = flag indicating whether short-range potentials should be
!            skipped - used in defect calcs
!  lskipq  = flag indicating whether Coulomb contribution should be excluded
!  lorder12= if .true. then order is i-j as expected, but if false then i & j are switched
!
!  The following are only needed for EEM/QEq/lDoQderiv2 and when lgrad2 = .true. :
!
!  d1i     = on return contains the derivative of E with respect to qi/r
!  d1j     = on return contains the derivative of E with respect to qj/r
!  d2i2    = on return contains the second derivative of E w.r.t. to qi/qi
!  d2ij    = on return contains the second derivative of E w.r.t. to qi/qj
!  d2j2    = on return contains the second derivative of E w.r.t. to qj/qj
!
!  On return deriv and deriv2 contain the complete first and
!  second derivative terms, while derive and derive2 contain 
!  the derivatives for the charge only terms.
!
!   2/95 K3 and K4 added for harmonic potential
!   3/95 SW2 potential added
!   7/95 Exponential repulsion cutoff added to save cputime
!   8/95 Correction applied to the general potential energy shift
!   8/95 Ewald sum for 1/r**6 added
!   9/96 Error in lvalid setting corrected (condition on lptrmol
!        has been removed before molmec checks).
!   2/97 BSM exponential and damped_dispersion potls added
!   4/97 Sutton-Chen potential added
!   5/97 Exponential form of Sutton-Chen added
!   6/97 If lskipsr is true then exclude all short range potentials -
!        this is to avoid numerical problems with differencing large
!        numbers in defect calculations.
!   7/97 Tapering of potentials to zero at cutp added
!   7/97 Sutton-Chen changed to be general density form
!   7/97 Rose-Smith-Guinea-Ferrante potential added
!   1/98 Separate array added for distances to preserve them
!   1/98 Derivatives of energy with respect to charge added for EEM/QEq
!   4/98 ESFF form of Lennard-Jones combination rules added
!  10/98 Polynomial potential assigned number 23 and order stored in dpot
!   1/99 1-4 interaction scaling added
!   4/99 Coulomb x erfc potential added
!   4/99 Keyword to turn off electrostatic energy calculation added
!   8/99 Keyword checking removed locally as this leads to a BIG drop
!        in performance!!!
!  10/99 Cubic density function added
!   5/00 C10 term added to damped dispersion
!   5/00 derive is now calculated without the charges so that it
!        can also be used in the polarisation calculation
!   6/00 derive2 and derive3 have been added for polarisation derivatives
!   6/00 missing lc6 third derivatives added
!   6/00 third derivatives with respect to radius added
!   7/00 CovExp potential added
!   2/01 lc6 now checked for 3-D only
!   2/01 Fermi-Dirac potential added 
!   2/01 Arguments that were from modules now accessed from modules
!        otherwise performance is destroyed.
!   4/01 Testing of distances changed so that interaction will
!        always be counted for bonded atoms.
!   5/02 Maximum distance introduced for non-periodic electrostatics
!        so that it can be used for 1-D case.
!   5/02 Tapering moved to subroutine
!   8/02 Lennard-Jones buffered potential added
!   1/03 Modified to include Wolf sum
!  11/03 ndennat/ndentyp replaced
!  11/03 Scaling of EAM densities added
!   5/04 Tsuneyuki potential added
!   9/04 Array for Coulomb term, without charges added -> derive0
!   9/04 Trigger for d1i/d2ij calc changed to include lDoQDeriv2
!   9/04 Jiang-Brown modification of SW2 added - qi / qj added to argument list
!   9/04 d1i/d1j now calculated locally & d2i2/d2j2
!   1/05 Sqrt loop removed from this routine
!   4/05 cosh-spring potential added
!   8/05 sctrms modified to ensure that density is positive for cubic case
!   9/05 Voter form of density for EAM added
!  11/05 General Voter taper option added
!  11/05 Tapering of densities added
!   2/06 Quadratic and quartic EAM densities added
!   3/06 Power law EAM densities truncated after r0
!   3/06 Modified to allow for density component number
!   3/06 Poly harmonic potential added
!   4/06 Species specific density added
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
!  10/07 references to npt in Lennard-Jones potentials corrected to rnpt
!  11/07 Mei-Davenport potential added
!  11/07 MDF taper added
!  11/07 Unused variables cleaned up
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 erferfc and reperfc potentials added
!   7/08 nmolonly check is now only used if lcoulsub is true
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 Name of array for 1/bpot for Buckingham potential changed from rho to rhopot
!  11/08 rhoij / rhoji now an array to handle MEAM orders
!  11/08 x, y, z now passed as arguments to rhoderv
!  11/08 rho arrays redimensioned to 2-D for benefit of MEAM
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
!   3/09 lorder12 flag added as argument for benefit of meamrho where order of atoms 
!        is important.
!   3/09 lorder12 added to list of arguments for meamrho
!   4/09 Inconsistency in small vs small2 corrected for shell case
!   4/09 d term added to Baskes potental
!   4/09 Core-shell interaction during Wolf sum modified to improve handling of small distances
!   4/09 MEAM density calculation removed from this routine since it requires more than twobody
!        interactions due to screening term.
!   4/09 Terms associated with Wolf sum modified for core-shell model
!   6/09 Charge as a coordinate option added
!   7/09 Shifting of Morse minimum to zero added as an option
!   7/09 Exppowers potential added
!   8/09 Grimme_C6 potential added
!   9/09 r0 added for standard polynomial potential
!   2/10 Central force model potentials added
!   3/10 Error in C6 part of LJ buffer potential fixed
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
  logical,        intent(in)      :: lgrad1
  logical,        intent(in)      :: lgrad2
  logical,        intent(in)      :: lgrad3
  logical,        intent(in)      :: lorder12
  logical,        intent(in)      :: lperiodic
  logical,        intent(in)      :: lregion1
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
  integer(i4)                     :: norder
  integer(i4)                     :: npot
  integer(i4)                     :: npt
  integer(i4)                     :: nptyp
  logical                         :: lc6loc
  logical                         :: lcoulsub
  logical                         :: lneedgrad1
  logical                         :: lvalid
  real(dp)                        :: apt
  real(dp)                        :: b6
  real(dp)                        :: b8
  real(dp)                        :: b10
  real(dp)                        :: bpt
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
  real(dp)                        :: dfrhodr
  real(dp)                        :: d2frhodr2
  real(dp)                        :: d3frhodr3
  real(dp)                        :: dgofr
  real(dp)                        :: d2gofr
  real(dp)                        :: d3gofr
  real(dp)                        :: dfun(4)
  real(dp)                        :: dtpfn
  real(dp)                        :: d2f6
  real(dp)                        :: d2f8
  real(dp)                        :: d2f10
  real(dp)                        :: d2tpfn
  real(dp)                        :: d3f6
  real(dp)                        :: d3f8
  real(dp)                        :: d3f10
  real(dp)                        :: d2rk6
  real(dp)                        :: d2rk8
  real(dp)                        :: d2rk10
  real(dp)                        :: d3rk6
  real(dp)                        :: d3rk8
  real(dp)                        :: d3rk10
  real(dp)                        :: d3tpfn
  real(dp)                        :: dxdr
  real(dp)                        :: d2xdr2
  real(dp)                        :: d3xdr3
  real(dp)                        :: derf
  real(dp)                        :: derfc
  real(dp)                        :: derivk
  real(dp)                        :: deriv2k
  real(dp)                        :: deriv3k
  real(dp)                        :: derivktrm
  real(dp)                        :: deriv2ktrm
  real(dp)                        :: deriv3ktrm
  real(dp)                        :: df6
  real(dp)                        :: df8
  real(dp)                        :: df10
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
  real(dp)                        :: dfdmp
  real(dp)                        :: d2fdmp
  real(dp)                        :: d3fdmp
  real(dp)                        :: e01
  real(dp)                        :: e02
  real(dp)                        :: e0t
  real(dp)                        :: e11
  real(dp)                        :: e21
  real(dp)                        :: e31
  real(dp)                        :: eatm
  real(dp)                        :: eatmtrm
  real(dp)                        :: ecfm
  real(dp)                        :: decfm
  real(dp)                        :: d2ecfm
  real(dp)                        :: d3ecfm
  real(dp)                        :: erffc
  real(dp)                        :: etaloc
  real(dp)                        :: etpfn
  real(dp)                        :: ebr6
  real(dp)                        :: ebr8
  real(dp)                        :: ebr10
  real(dp)                        :: EcZ
  real(dp)                        :: effc
  real(dp)                        :: ept
  real(dp)                        :: erftrm
  real(dp)                        :: erfctrm
  real(dp)                        :: erealin
  real(dp)                        :: eta3
  real(dp)                        :: etar2
  real(dp)                        :: etar22
  real(dp)                        :: etatrm
  real(dp)                        :: exptrm
  real(dp)                        :: exptrm1
  real(dp)                        :: exptrm2
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
  real(dp)                        :: r24
  real(dp)                        :: rapt
  real(dp)                        :: rapt2
  real(dp)                        :: rd
  real(dp)                        :: rd2
  real(dp)                        :: rd3
  real(dp)                        :: rd4
  real(dp)                        :: rd5
  real(dp)                        :: rd6
  real(dp)                        :: rderivk
  real(dp)                        :: rexptrm
  real(dp)                        :: rho
  real(dp)                        :: rho0
  real(dp)                        :: rhol
  real(dp)                        :: rhoij(maxmeamcomponent)
  real(dp)                        :: rhoji(maxmeamcomponent)
  real(dp)                        :: rk
  real(dp)                        :: rk2
  real(dp)                        :: rk6
  real(dp)                        :: rk8
  real(dp)                        :: rk10
  real(dp)                        :: rkd
  real(dp)                        :: rmax
  real(dp)                        :: rmpt
  real(dp)                        :: rnpt
  real(dp)                        :: rr0
  real(dp)                        :: rrho
  real(dp)                        :: rtrm
  real(dp)                        :: rtrm0
  real(dp)                        :: rtrm1d
  real(dp)                        :: rtrm2d
  real(dp)                        :: rtrm3d
  real(dp)                        :: rtrm1k
  real(dp)                        :: rtrm2k
  real(dp)                        :: rtrm3k
  real(dp)                        :: rtrm32k
  real(dp)                        :: rnineth
  real(dp)                        :: rqtrm
  real(dp)                        :: rxm1
  real(dp)                        :: setaloc
  real(dp)                        :: seventh
  real(dp)                        :: sig
  real(dp)                        :: small2
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
  real(dp)                        :: trm2
  real(dp)                        :: trm21
  real(dp)                        :: trm22
  real(dp)                        :: trm3
  real(dp)                        :: trm4
  real(dp)                        :: trm5
  real(dp)                        :: trme
  real(dp)                        :: ts0
  real(dp)                        :: ts1
  real(dp)                        :: ts2
  real(dp)                        :: ts3
  real(dp)                        :: ts4
  real(dp)                        :: ts5
  real(dp)                        :: trm1
  real(dp)                        :: trm12
  real(dp)                        :: ttrm1
  real(dp)                        :: x
  real(dp)                        :: zeta
  real(dp)                        :: zetar
!
!  Local variables
!
  r24 = 1.0_dp/24.0_dp
  small2 = 1.0d-13
  seventh = 1.0_dp/7.0_dp
  rnineth = 1.0_dp/9.0_dp
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
    if (lgrad1) then
      d0i(nor) = 0.0_dp
      d0j(nor) = 0.0_dp
      deriv(nor) = 0.0_dp
      derive0(nor) = 0.0_dp
      derive(nor) = 0.0_dp
      rtrm1 = 0.0_dp
      d1i(nor) = 0.0_dp
      d1j(nor) = 0.0_dp
      if (lgrad2) then
        deriv2(nor) = 0.0_dp
        derive2(nor) = 0.0_dp
        rderiv(nor) = 0.0_dp
        rtrm2(nor) = 0.0_dp
        d2ij(nor) = 0.0_dp
        d2i2(nor) = 0.0_dp
        d2j2(nor) = 0.0_dp
        if (lgrad3) then
          deriv3(nor) = 0.0_dp
          derive3(nor) = 0.0_dp
          rtrm3(nor) = 0.0_dp
          rtrm32(nor) = 0.0_dp
        endif
      endif
    endif
  else
    if (lgrad1) then
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
      if (lgrad2) then
        do k = nor0,nor
          deriv2(k) = 0.0_dp
          derive2(k) = 0.0_dp
          rderiv(k) = 0.0_dp
          rtrm2(k) = 0.0_dp
          d2ij(k) = 0.0_dp
          d2i2(k) = 0.0_dp
          d2j2(k) = 0.0_dp
        enddo
        if (lgrad3) then
          do k = nor0,nor
            deriv3(k) = 0.0_dp
            derive3(k) = 0.0_dp
            rtrm3(k) = 0.0_dp
            rtrm32(k) = 0.0_dp
          enddo
        endif
      endif
    endif
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
    if (lregion1) then
!
!  Coulomb term handling for small values of r in defect calcs
!  
      if (lDoElectrostatics) then
        trm1 = tweatpi*exp(-etaloc*r2)
        erffc = derfc(setaloc*r)
        erffc = rk*(erffc-1.0_dp)
        ereal = ereal + factor*erffc
        if (lgrad1) then
          dpt = factor*rk2
          trm2 = dpt*(erffc + trm1)
          deriv(k) = deriv(k) - trm2
          if (lgrad2) deriv2(k) = deriv2(k) + 2.0_dp*(trm2 + factor*trm1*etaloc)
        endif
      endif
!
!  C6 term handling for small values of r in defect calcs
!  
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
!
!  Distance checks for validity
!
          if (r.lt.rpot2(npot)) lvalid = .false.
!
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
          endif
        enddo
        if (abs(c6tot).gt.0.0_dp) then
          etar2 = eta*r2
          etar22 = etar2*etar2
!
!  When etar2 is small need to use power series
!  expansion to maintain precision.
!
          if (etar2.le.1.0d-4) then
            exptrm = 1.0_dp - etar2 + 0.5_dp*etar22
            eta3 = eta*eta*eta
            c6trm1 = - c6tot*eta3*(sixth - 0.125_dp*etar2)
            if (lgrad1) etatrm = eta3*exptrm*c6tot*rk2
          elseif (etar2.le.1.0d-2) then
            exptrm = 1.0_dp - etar2 + 0.5_dp*etar22 - sixth*etar22*(1.0_dp - 0.25_dp*etar2)
            eta3 = eta*eta*eta
            c6trm1 = - c6tot*eta3*(sixth - 0.125_dp*etar2 + 0.05_dp*etar22 - etar22*etar2/72.0_dp)
            if (lgrad1) etatrm = eta3*exptrm*c6tot*rk2
          else
            c6t2 = c6tot*r6
            exptrm = exp(-etar2)
            c6t1 = (1.0_dp+etar2*(1.0_dp+0.5_dp*etar2))*exptrm-1.0_dp
            if (lgrad1) etatrm=etar22*eta*exptrm*c6t2
            c6trm1 = c6t1*c6t2
          endif
          ec6 = ec6 - c6trm1
          if (lgrad1) then
            deriv(k) = deriv(k) + c6trm1*6.0_dp*rk2
            deriv(k) = deriv(k) + etatrm
            if (lgrad2) then
              deriv2(k) = deriv2(k) - 42.0_dp*c6trm1*rk2
              deriv2(k) = deriv2(k) - etatrm*(7.0_dp+2.0_dp*etar2)
            endif
          endif
        endif
      endif
    else
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
        deriv2k = 0.0_dp
        deriv3k = 0.0_dp
        rtrm1k = 0.0_dp
        rtrm2k = 0.0_dp
        rtrm3k = 0.0_dp
        rtrm32k = 0.0_dp
        rderivk = 0.0_dp
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
                apt = twopot(1,npot)*psfct
                bpt = rhopot(npot)
                cpt = twopot(3,npot)*psfct
                if (r.lt.repcut(npot)) then
                  trm1 = apt*exp(-bpt*(r-radsum))
                  eatm = eatm + trm1
                  if (lgrad1) then
                    trm1 = trm1*bpt
                    derivk = - trm1*rk + derivk
                    rtrm1k = rtrm1k + trm1
                    if (lgrad2) then
                      trm1 = trm1*bpt
                      deriv2k = trm1 + deriv2k
                      rtrm2k = rtrm2k + trm1
                      rderivk = rderivk - trm1*rk
                      if (lgrad3) then
                        trm1 = trm1*bpt
                        rtrm3k = rtrm3k + trm1*rk
                        rtrm32k = rtrm32k + trm1
                        deriv3k = - trm1 + deriv3k
                      endif
                    endif
                  endif
                endif
                if (cpt.gt.0.0_dp.and..not.lc6loc) then
                  r6 = cpt*rk2**3
                  eatm = eatm - r6
                  if (lgrad1) then
                    r6 = 6.0_dp*r6*rk2
                    derivk = r6 + derivk
                    if (lgrad2) then
                      r6 = 7.0_dp*r6
                      deriv2k = - r6 + deriv2k
                      if (lgrad3) then
                        deriv3k = 8.0_dp*r6*rk + deriv3k
                      endif
                    endif
                  endif
                endif
                eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*psfct
                endif
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
                  if (lgrad1) then
                    r12 = rmpt*r12
                    derivk = derivk - r12*rk2
                    if (lgrad2) then
                      r12 = (rmpt + 1.0_dp)*r12
                      deriv2k = r12*rk2 + deriv2k
                      if (lgrad3) then
                        deriv3k = - (rmpt + 2.0_dp)*r12*rk2*rk + deriv3k
                      endif
                    endif
                  endif
                  if (bpt.gt.0.0_dp.and.(.not.lc6loc.or.abs(rnpt-6.0_dp).gt.1.0d-12)) then
                    if (abs(rmpt-12.0_dp)+abs(rnpt-6.0_dp).lt.1.0d-12) then
                      r6 = bpt*r6
                    elseif (abs(rmpt-9.0_dp)+abs(rnpt-6.0_dp).lt.1.0d-12) then
                      r6 = bpt*r3*r3
                    else
                      r6 = bpt*rk**rnpt
                    endif
                    eatm = eatm - r6
                    if (lgrad1) then
                      r6 = rnpt*r6
                      derivk = r6*rk2 + derivk
                      if (lgrad2) then
                        r6 = (rnpt + 1.0_dp)*r6
                        deriv2k = - r6*rk2 + deriv2k
                        if (lgrad3) then
                          deriv3k = (rnpt + 2.0_dp)*r6*rk2*rk + deriv3k
                        endif
                      endif
                    endif
                  endif
                else
                  if (bpt.gt.0.0_dp.and.(.not.lc6loc.or.abs(rnpt-6.0_dp).gt.1.0d-12)) then
                    if (abs(rnpt-6.0_dp).lt.1.0d-12) then
                      r6 = bpt*rk2*rk2*rk2
                    else
                      r6 = bpt*rk**rnpt
                    endif
                    eatm = eatm - r6
                    if (lgrad1) then
                      r6 = rnpt*r6
                      derivk = r6*rk2 + derivk
                      if (lgrad2) then
                        r6 = (rnpt + 1.0_dp)*r6
                        deriv2k = - r6*rk2 + deriv2k
                        if (lgrad3) then
                          deriv3k = (rnpt + 2.0_dp)*r6*rk2*rk + deriv3k
                        endif
                      endif
                    endif
                  endif
                endif
                eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*psfct
                endif
              elseif (nptyp.eq.3) then
!*********************************************
!  Morse potential - no coulomb subtraction  *
!*********************************************
                apt = twopot(1,npot)*psfct
                bpt = twopot(2,npot)
                cpt = twopot(3,npot)
                ept = twopot(5,npot)
                trme = exp(-bpt*(r-cpt))
                trm1 = 1.0_dp - trme
                if (lgrad1) then
                  trm2 = 2.0_dp*apt*bpt*trme
                  derivk = trm2*trm1*rk + derivk
                  if (lgrad2) then
                    trm2 = trm2*bpt
                    deriv2k = trm2*(2.0_dp*trme-1.0_dp) + deriv2k
                    if (lgrad3) then
                      deriv3k = trm2*bpt*(1.0_dp-4.0_dp*trme) + deriv3k
                    endif
                  endif
                endif
                trm1 = trm1*trm1
                eatm = eatm + apt*(trm1 - ept)
                eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*psfct
                endif
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
                ept = twopot(5,npot)
                trme = exp(-bpt*(r-cpt))
                trm1 = 1.0_dp - trme
                if (lgrad1) then
                  trm2 = 2.0_dp*apt*bpt*trme
                  derivk = trm2*trm1*rk + derivk
                  derive(k) = dpte*rk2*rk + derive(k)
                  if (lgrad2) then
                    trm2 = trm2*bpt
                    deriv2k = trm2*(2.0_dp*trme-1.0_dp) + deriv2k
                    derive2(k) = derive2(k) - 2.0_dp*dpte*rk2*rk
                    if (lgrad3) then
                      deriv3k = trm2*bpt*(1.0_dp-4.0_dp*trme) + deriv3k
                      derive3(k) = derive3(k) + 6.0_dp*dpte*rk2*rk2
                    endif
                  endif
                endif
                trm1 = trm1*trm1
                eatm = eatm + apt*(trm1 - ept)
                ereal = ereal - dpt*rk
                if (lgrad1) then
                  derive0(k) = derive0(k) - dpte*rk
                endif
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
                if (lgrad1) derivk = apt*rd*rk + derivk
                if (lgrad2) deriv2k = apt + deriv2k
                if (cpt.ne.0.0_dp) then
                  cpt = cpt*sfct
                  eatm = eatm + sixth*cpt*rd2*rd
                  if (lgrad1) derivk = derivk + 0.5_dp*rd2*cpt*rk
                  if (lgrad2) deriv2k = deriv2k + cpt*rd
                  if (lgrad3) deriv3k = deriv3k + cpt
                endif
                if (dpt.ne.0.0_dp) then
                  dpt = dpt*sfct
                  eatm = eatm + 0.25_dp*sixth*dpt*rd2*rd2
                  if (lgrad1) derivk = derivk + sixth*rd2*rd*dpt*rk
                  if (lgrad2) deriv2k = deriv2k + 0.5_dp*dpt*rd2
                  if (lgrad3) deriv3k = deriv3k + dpt*rd
                endif
                eatm = eatm + sfct*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*sfct
                endif
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
                if (lgrad1) then
                  derive0(k) = derive0(k) - tpte*rk
                  derivk = apt*rd*rk + derivk
                  derive(k) = tpte*rk2*rk + derive(k)
                  if (lgrad2) then
                    deriv2k = deriv2k + apt
                    derive2(k) = derive2(k) - 2.0_dp*tpte*rk2*rk
                    if (lgrad3) then
                      derive3(k) = derive3(k) + 6.0_dp*tpte*rk2*rk2
                    endif
                  endif
                endif
                if (cpt.ne.0.0_dp) then
                  cpt = cpt*sfct
                  eatm = eatm + sixth*cpt*rd2*rd
                  if (lgrad1) derivk = derivk + 0.5_dp*rd2*cpt*rk
                  if (lgrad2) deriv2k = deriv2k + cpt*rd
                  if (lgrad3) deriv3k = deriv3k + cpt
                endif
                if (dpt.ne.0.0_dp) then
                  dpt = dpt*sfct
                  eatm = eatm + 0.25_dp*sixth*dpt*rd2*rd2
                  if (lgrad1) derivk = derivk + sixth*rd2*dpt*rk
                  if (lgrad2) deriv2k = deriv2k + 0.5_dp*dpt*rd2
                  if (lgrad3) deriv3k = deriv3k + dpt*rd
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
                  if (lgrad1) then
                    derivk = - trm1*bpt*rk*r12 + derivk
                    derivk = derivk - trm1*mpt*rk2*r12
                    if (lgrad2) then
                      trm2 = bpt*bpt
                      deriv2k = deriv2k + trm1*trm2*r12
                      deriv2k = deriv2k + 2.0_dp*mpt*trm1*bpt*r12*rk
                      deriv2k = deriv2k + trm1*mpt*(mpt+1)*r12*rk2
                      if (lgrad3) then
                        deriv3k = deriv3k - trm1*(bpt*trm2*r12)
                        trm1 = mpt*trm1
                        deriv3k = deriv3k - 3.0_dp*trm1*(trm2*r12*rk)
                        trm1 = (mpt+1)*trm1
                        deriv3k = deriv3k - 3.0_dp*trm1*(bpt*r12*rk2)
                        trm1 = (mpt+2)*trm1
                        deriv3k = deriv3k - trm1*(r12*rk2*rk)
                      endif
                    endif
                  endif
                endif
                if (cpt.gt.0.0_dp.and.(.not.lc6loc.or.npt.ne.6)) then
                  r6 = cpt*rk**npt
                  eatm = eatm - r6
                  if (lgrad1) then
                    r6 = npt*r6*rk2
                    derivk = r6 + derivk
                    if (lgrad2) then
                      r6 = (npt+1)*r6
                      deriv2k = deriv2k - r6
                      if (lgrad3) then
                        deriv3k = (npt+2)*r6*rk + deriv3k
                      endif
                    endif
                  endif
                endif
                eatm = eatm + psfct*(eshift(npot)+gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*psfct
                endif
              elseif (nptyp.eq.8) then
!*****************************************
!  Spring potential - core - shell only  *
!*****************************************
                apt = twopot(1,npot)*sfct
                bpt = twopot(2,npot)*r24*sfct
                trm = bpt*r2
                eatm = eatm + 0.5_dp*apt*r2 + trm*r2
                if (lgrad1) then
                  derivk = apt + 4.0_dp*trm + derivk
                  if (lgrad2) then
                    deriv2k = apt + 12.0_dp*trm + deriv2k
                    if (lgrad3) then
                      deriv3k = 12.0_dp*bpt*r + deriv3k
                    endif
                  endif
                endif
              elseif (nptyp.eq.9) then
!**************************
!  Coulomb subtract only  *
!**************************
                dpte = twopot(4,npot)*pscale
                dpt = factor*dpte
                dpte = dpte*sfct
                ereal = ereal - dpt*rk
                if (lgrad1) then
                  derive0(k) = derive0(k) - dpte*rk
                  derive(k) = derive(k) + dpte*rk2*rk
                  if (lgrad2) then
                    derive2(k) = derive2(k) - 2.0_dp*dpte*rk2*rk
                    if (lgrad3) then
                      derive3(k) = derive3(k) + 6.0_dp*dpte*rk2*rk2
                    endif
                  endif
                endif
              elseif (nptyp.eq.10) then
!************************************
!  Four range buckingham potential  *
!************************************
                if (r.lt.tpot(1,npot)) then
                  apt = twopot(1,npot)*psfct
                  bpt = rhopot(npot)
                  trm1 = apt*exp(-bpt*r)
                  eatm = eatm + trm1
                  if (lgrad1) then
                    trm1 = trm1*bpt
                    derivk = -trm1*rk + derivk
                    if (lgrad2) then
                      trm1 = trm1*bpt
                      deriv2k = trm1 + deriv2k
                      if (lgrad3) then
                        deriv3k = -trm1*bpt + deriv3k
                      endif
                    endif
                  endif
                elseif (r.lt.twopot(4,npot)) then
                  norder = 5
                  rtrm = psfct
                  do l = 1,norder + 1
                    tp = tpot(2+l,npot)
                    eatm = eatm + tp*rtrm
                    if (lgrad1) then
                      trm = tp*(l-1)*rtrm*rk2
                      derivk = derivk+trm
                      if (lgrad2.and.l.gt.2) then
                        deriv2k = deriv2k + trm*(l-2)
                        if (l.gt.3.and.lgrad3) then
                          deriv3k = deriv3k + trm*(l-2)*(l-3)*rk
                        endif
                      endif
                    endif
                    rtrm = rtrm*r
                  enddo
                elseif (r.lt.tpot(2,npot)) then
                  norder = 3
                  rtrm = psfct
                  do l = 1,norder+1
                    tp = tpot(8+l,npot)
                    eatm = eatm + tp*rtrm
                    if (lgrad1) then
                      trm = tp*(l-1)*rtrm*rk2
                      derivk = derivk + trm
                      if (lgrad2.and.l.gt.2) then
                        deriv2k = deriv2k + trm*(l-2)
                        if (l.gt.3.and.lgrad3) then
                          deriv3k = deriv3k + trm*(l-2)*(l-3)*rk
                        endif
                      endif
                    endif
                    rtrm = rtrm*r
                  enddo
                else
                  cpt = twopot(3,npot)*psfct
                  r6 = cpt*rk2**3
                  eatm = eatm - r6
                  if (lgrad1) then
                    r6 = 6.0_dp*r6*rk2
                    derivk = r6 + derivk
                    if (lgrad2) then
                      r6 = 7.0_dp*r6
                      deriv2k = -r6 + deriv2k
                      if (lgrad3) then
                        deriv3k = 8.0_dp*r6*rk + deriv3k
                      endif
                    endif
                  endif
                endif
              elseif (nptyp.eq.11) then
!*********************
!  Spline potential  *
!*********************
                apt = twopot(1,npot)
                call spline(npot,nsplpt(npot),splr(1,npot),splf(1,npot),r+apt,dfun,nsplty(npot),lgrad1,lgrad2)
                eatom = eatom + dfun(1)*psfct
                if (lgrad1) deriv(k) = deriv(k) + dfun(2)*rk*psfct
                if (lgrad2) deriv2(k) = deriv2(k) + dfun(3)*psfct
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
                if (lgrad1) then
                  r6 = rnpt*r6
                  r12 = rmpt*r12
                  derivk = (r6-r12)*rk2 + derivk
                  if (lgrad2) then
                    r6 = (rnpt+1.0_dp)*r6
                    r12 = (rmpt+1.0_dp)*r12
                    deriv2k = (r12-r6)*rk2+deriv2k
                    if (lgrad3) then
                      deriv3k = ((rnpt+2.0_dp)*r6-(rmpt+2.0_dp)*r12)*rk2*rk + deriv3k
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  trm4 = trm1*trm1
                  deriv(k) = deriv(k) - apt*rk*(bpt*trm4*trm3+4.0_dp*trm2*rk)
                  if (lgrad2) then
                    trm5 = trm4*trm4
                    deriv2(k) = deriv2(k) + apt*(bpt*bpt*trm5*trm3+8.0_dp*trm2*rk*bpt*trm4+20.0_dp*trm2*rk2+ &
                      2.0_dp*bpt*trm4*trm1*trm3)
                    if (lgrad3) then
                      deriv3(k) = deriv3(k) - apt*(6.0_dp*bpt*bpt*trm5*trm1*trm3+12.0_dp*bpt*bpt*trm5*trm2* &
                        rk+40.0_dp*bpt*trm4*trm2*rk2+24.0_dp*trm2*rk*bpt*trm4*trm1+6.0_dp*trm5*bpt*trm3+ &
                        bpt*bpt*bpt*trm4*trm5*trm3)
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  derivk = derivk + 2.0_dp*trm1*trme*rk
                  if (lgrad2) then
                    deriv2k = deriv2k + 2.0_dp*bpt*trme
                    deriv2k = deriv2k - 4.0_dp*trm1*trm1*trme
                    if (lgrad3) then
                      deriv3k = deriv3k - 12.0_dp*bpt*trm1*trme
                      deriv3k = deriv3k + 8.0_dp*trm1*trm1*trm1*trme
                    endif
                  endif
                endif
                eatm = eatm + psfct*(eshift(npot)+gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*psfct
                endif
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
                if (b6.eq.0.0_dp.and.b8.eq.0.0_dp.and.b10.eq.0.0_dp) then
!
!  No damping
!
                  rk6 = rk2*rk2*rk2
                  rk8 = rk6*rk2
                  rk10 = rk8*rk2
                  eatm = eatm - apt*rk6 - bpt*rk8 - cpt*rk10
                  if (lgrad1) then
                    rk6 = 6.0_dp*rk8
                    rk8 = 8.0_dp*rk8*rk2
                    rk10 = 10.0_dp*rk10*rk2
                    derivk = derivk + apt*rk6 + bpt*rk8 + cpt*rk10
                    if (lgrad2) then
                      rk6 = 7.0_dp*rk6
                      rk8 = 9.0_dp*rk8
                      rk10 = 11.0_dp*rk10
                      deriv2k = deriv2k - apt*rk6 - bpt*rk8 - cpt*rk10
                      if (lgrad3) then
                        rk6 = 8.0_dp*rk6*rk
                        rk8 = 10.0_dp*rk8*rk
                        rk10 = 12.0_dp*rk10*rk
                        deriv3k = deriv3k + apt*rk6 + bpt*rk8 + cpt*rk10
                      endif
                    endif
                  endif
                else
!
!  Damping
!
                  rk6 = rk2*rk2*rk2
                  rk8 = rk6*rk2
                  rk10 = rk8*rk2
                  if (b6.ne.0.0_dp) then
                    ebr6 = exp(-b6*r)
                    br6 = b6*r
                    f6 = 1.0_dp + br6*(1.0_dp + 0.5_dp*br6*(1.0_dp + third*br6*( &
                      1.0_dp + 0.25_dp*br6*(1.0_dp + 0.2_dp*br6*(1.0_dp + sixth*br6)))))
                  else
                    ebr6 = 1.0_dp
                    f6 = 0.0_dp
                  endif
                  if (b8.ne.0.0_dp) then
                    br8 = b8*r
                    ebr8 = exp(-b8*r)
                    f8 = 1.0_dp + br8*(1.0_dp + 0.5_dp*br8*(1.0_dp + third*br8*( &
                      1.0_dp + 0.25_dp*br8*(1.0_dp + 0.2_dp*br8*(1.0_dp + sixth &
                      *br8*(1.0_dp + seventh*br8*(1.0_dp + 0.125_dp*br8)))))))
                  else
                    ebr8 = 1.0_dp
                    f8 = 0.0_dp
                  endif
                  if (b10.ne.0.0_dp) then
                    br10 = b10*r
                    ebr10 = exp(-b10*r)
                    f10 = 1.0_dp + br10*(1.0_dp+0.5_dp*br10*(1.0_dp+third* &
                      br10*(1.0_dp+0.25_dp*br10*(1.0_dp+0.2_dp*br10*( &
                      1.0_dp+sixth*br10*(1.0_dp+seventh*br10*(1.0_dp+ &
                      0.125_dp*br10*(1.0_dp+rnineth*br10*(1.0_dp+0.1_dp*br10)))))))))
                  else
                    ebr10 = 1.0_dp
                    f10 = 0.0_dp
                  endif
                  f6 = 1.0_dp - f6*ebr6
                  f8 = 1.0_dp - f8*ebr8
                  f10 = 1.0_dp - f10*ebr10
                  eatm = eatm - apt*rk6*f6 - bpt*rk8*f8 - cpt*rk10*f10
                  if (lgrad1) then
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
                    if (lgrad2) then
                      d2rk6 = - 7.0_dp*drk6*rk
                      d2rk8 = - 9.0_dp*drk8*rk
                      d2rk10 = - 11.0_dp*drk10*rk
                      if (b6.ne.0.0_dp) then
                        d2f6 = ebr6*b6*b6*(br6**5)*(1.0_dp-sixth*br6)/120.0_dp
                      else
                        d2f6 = 0.0_dp
                      endif
                      if (b8.ne.0.0_dp) then
                        d2f8 = ebr8*b8*b8*(br8**7)*(1.0_dp-0.125_dp*br8)/5040.0_dp
                      else
                        d2f8 = 0.0_dp
                      endif
                      if (b10.ne.0.0_dp) then
                        d2f10 = ebr10*b10*b10*(br10**9)*(1.0_dp-0.1_dp*br10)/362880.0_dp
                      else
                        d2f10 = 0.0_dp
                      endif
                      deriv2k = deriv2k - apt*(rk6*d2f6+2.0_dp*drk6*df6+d2rk6*f6)
                      deriv2k = deriv2k - bpt*(rk8*d2f8+2.0_dp*drk8*df8+d2rk8*f8)
                      deriv2k = deriv2k - bpt*(rk10*d2f10+2.0_dp*drk10*df10+d2rk10*f10)
                      if (lgrad3) then
                        d3rk6 = - 8.0_dp*d2rk6*rk
                        d3rk8 = - 10.0_dp*d2rk8*rk
                        d3rk10 = - 12.0_dp*d2rk10*rk
                        if (b6.ne.0.0_dp) then
                          d3f6 = ebr6*b6*b6*b6*(br6**4)*(1.0_dp-(br6/5.0_dp)*(2.0_dp-sixth*br6))*third*0.125_dp
                        else
                          d3f6 = 0.0_dp
                        endif
                        if (b8.ne.0.0_dp) then
                          d3f8 = ebr8*b8*b8*b8*(br8**6)*(1.0_dp-(br8/7.0_dp)*(2.0_dp-0.125_dp*br8))/720.0_dp
                        else
                          d3f8 = 0.0_dp
                        endif
                        if (b10.ne.0.0_dp) then
                          d3f10 = ebr10*b10*b10*b10*(br10**8)*(1.0_dp-(br10/9.0_dp)*(2.0_dp-0.1_dp*br10))/40320.0_dp
                        else
                          d3f10 = 0.0_dp
                        endif
                        deriv3k = deriv3k - apt*(d3rk6*f6+3.0_dp*(d2rk6*df6+drk6*d2f6)+rk6*d3f6)
                        deriv3k = deriv3k - apt*(d3rk8*f8+3.0_dp*(d2rk8*df8+drk8*d2f8)+rk8*d3f8)
                        deriv3k = deriv3k - apt*(d3rk10*f10+3.0_dp*(d2rk10*df10+drk10*d2f10)+rk10*d3f10)
                      endif
                    endif
                  endif
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
                if (lgrad1) then
                  exptrm = exptrm*rr0*bpt
                  deriv(k) = deriv(k) + exptrm*trm1*rk
                  if (lgrad2) then
                    exptrm = exptrm*rr0*bpt
                    deriv2(k) = deriv2(k) + exptrm*(1.0_dp - trm1)
                    if (lgrad3) then
                      exptrm = exptrm*rr0*bpt
                      deriv3(k) = deriv3(k) - exptrm*(2.0_dp - trm1)
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  e11 = - e01*rk
                  dtpfn = (((5.0_dp*ts5*r+4.0_dp*ts4)*r+3.0_dp*ts3)*r+2.0_dp*ts2)*r+ts1
                  deriv(k) = deriv(k) + (tpfn*e11+dtpfn*e0t)*rk
                  if (lgrad2) then
                    e21 = - 2.0_dp*e11*rk
                    d2tpfn = ((20.0_dp*ts5*r+12.0_dp*ts4)*r+6.0_dp*ts3)*r + 2.0_dp*ts2
                    deriv2(k) = deriv2(k) + tpfn*e21 + 2.0_dp*dtpfn*e11 + d2tpfn*e0t
                    if (lgrad3) then
                      e31 = - 3.0_dp*e21*rk
                      d3tpfn = (60.0_dp*ts5*r+24.0_dp*ts4)*r + 6.0_dp*ts3
                      deriv3(k) = deriv3(k) + tpfn*e31 + 3.0_dp*dtpfn*e21 + 3.0_dp*d2tpfn*e11 + d3tpfn*e0t
                    endif
                  endif
                endif
              elseif (nptyp.eq.23) then
!************************
!  Polynomial potential *
!************************
                norder = nint(twopot(4,npot))
                r0 = twopot(1,npot)
                rtrm = psfct
                rtrm1d = 0.0_dp
                rtrm2d = 0.0_dp
                rtrm3d = 0.0_dp
                do l = 1,norder+1
                  tp = tpot(l,npot)
                  eatm = eatm + tp*rtrm
                  if (lgrad1) then
                    trm = tp*(l-1)*rtrm1d*rk
                    derivk = derivk + trm
                    if (lgrad2.and.l.gt.2) then
                      deriv2k = deriv2k + tp*rtrm2d*(l-2)*(l-1)
                      if (l.gt.3.and.lgrad3) then
                        deriv3k = deriv3k + tp*rtrm3d*(l-1)*(l-2)*(l-3)
                        rtrm3d = rtrm3d*(r - r0)
                      elseif (l.eq.3.and.lgrad3) then
                        rtrm3d = psfct
                      endif
                      rtrm2d = rtrm2d*(r - r0)
                    elseif (lgrad2.and.l.eq.2) then
                      rtrm2d = psfct
                    endif
                    if (l.eq.1) then
                      rtrm1d = psfct
                    else
                      rtrm1d = rtrm1d*(r - r0)
                    endif
                  endif
                  rtrm = rtrm*(r - r0)
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
                if (lgrad1) then
                  rapt2 = rapt*rapt
                  trm1 = 2.0_dp*rapt*exp(-r2*rapt2)/sqrtpi
                  trm2 = (trm0 + trm1*factor)*rk2
                  deriv(k) = deriv(k) - trm2
                  if (lgrad2) then
                    trm3 = 2.0_dp*trm1*factor*rapt2
                    deriv2(k) = deriv2(k) + 2.0_dp*trm2 + trm3
                    if (lgrad3) then
                      deriv3(k) = deriv3(k) - rk*(6.0_dp*trm2+2.0_dp*trm3) - 2.0_dp*trm3*r*rapt2
                    endif
                  endif
                endif
                eatm = eatm + factor*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  deriv(k) = deriv(k) + gshift(npot)*rk*factor
                endif
              elseif (nptyp.eq.25) then
!***********
!  CovExp  *
!***********
                apt = twopot(1,npot)*psfct
                bpt = twopot(2,npot)*0.5_dp
                cpt = twopot(3,npot)
                trme = apt*exp(-bpt*((r-cpt)**2)/r)
                eatm = eatm - trme
                if (lgrad1) then
                  trm1 = bpt*(1.0_dp-cpt*cpt*rk2)
                  derivk = trme*trm1*rk + derivk
                  if (lgrad2) then
                    trm2 = 2.0_dp*bpt*cpt*cpt*rk*rk2
                    deriv2k = trme*(trm2-trm1*trm1) + deriv2k
                    if (lgrad3) then
                      trm3 = 3.0_dp*trm2*rk
                      deriv3k = trme*(trm1*trm1*trm1-3.0_dp*trm1*trm2-trm3) + deriv3k
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  trm1 = trm1*bpt*rtrm*trme
                  derivk = derivk - trm1*rk
                  if (lgrad2) then
                    trm1 = trm1*bpt*rtrm
                    deriv2k = trm1*(trme-1.0_dp)+deriv2k
                    if (lgrad3) then
                      trm1 = trm1*bpt*rtrm
                      deriv3k = deriv3k - trm1*(1.0_dp-trme*(4.0_dp-trme))
                    endif
                  endif
                endif
                eatm = eatm + psfct*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*psfct
                endif
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
                  if (lgrad1) then
                    r12 = rmpt*r12
                    derivk = derivk - r12*rk*rkd
                    if (lgrad2) then
                      r12 = (rmpt+1.0_dp)*r12
                      deriv2k = r12*rkd*rkd + deriv2k
                      if (lgrad3) then
                        deriv3k = -(rmpt+2.0_dp)*r12*rkd*rkd*rkd + deriv3k
                      endif
                    endif
                  endif
                endif
                if (bpt.gt.0.0) then
                  rnpt = tpot(2,npot)
                  r6 = bpt*rkd**rnpt
                  eatm = eatm - r6
                  if (lgrad1) then
                    r6 = rnpt*r6
                    derivk = r6*rkd*rk + derivk
                    if (lgrad2) then
                      r6 = (rnpt+1.0_dp)*r6
                      deriv2k = - r6*rkd*rkd + deriv2k
                      if (lgrad3) then
                        deriv3k = (rnpt+2.0_dp)*r6*rkd*rkd*rkd + deriv3k
                      endif
                    endif
                  endif
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
                if (lgrad1) derivk = apt*rd + derivk
                if (lgrad2) deriv2k = apt*(3.0_dp*r2 - bpt*bpt) + deriv2k
                eatm = eatm + sfct*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  derivk = derivk + gshift(npot)*rk*sfct
                endif
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
                if (lgrad1) then
                  derive0(k) = derive0(k) - tpte*rk
                  derivk = apt*rd + derivk
                  derive(k) = derive(k) + tpte*rk2*rk
                  if (lgrad2) then
                    deriv2k = deriv2k + apt*(3.0_dp*r2 - bpt*bpt)
                    derive2(k) = derive2(k) - 2.0_dp*tpte*rk2*rk
                    if (lgrad3) then
                      derive3(k) = derive3(k) + 6.0_dp*tpte*rk2*rk2
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  if (mpt.eq.1) then
                    dgofr = zeta*trm1
                  elseif (mpt.eq.2) then
                    dgofr = zeta*(1.375_dp + zetar*(1.5_dp + zetar/2.0_dp))*trm1
                  endif
                  dgofr = dgofr - 2.0_dp*zeta*gofr
                  derivk = derivk - apt*rk2*(gofr*rk - dgofr)
                  if (lgrad2) then
                    if (mpt.eq.1) then
                      d2gofr = - 2.0_dp*zeta*zeta*trm1
                    elseif (mpt.eq.2) then
                      d2gofr = zeta*zeta*(zetar - 0.5_dp)*trm1
                    endif
                    d2gofr = d2gofr - 2.0_dp*zeta*dgofr
                    deriv2k = deriv2k + apt*rk*(d2gofr - 2.0_dp*dgofr*rk + 2.0_dp*gofr*rk2)
                    if (lgrad3) then
                      if (mpt.eq.1) then
                        d3gofr = 4.0_dp*zeta*zeta*zeta*trm1
                      elseif (mpt.eq.2) then
                        d3gofr = zeta*zeta*zeta*(2.0_dp + zetar*(3.0_dp + 2.0_dp*zetar))*trm1
                      endif
                      d3gofr = d3gofr - 2.0_dp*zeta*d2gofr
                      deriv3k = deriv3k + apt*rk*(d3gofr - 3.0_dp*rk*d2gofr + 6.0_dp*rk2*(dgofr - gofr*rk))
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  trm4 = trm1*trm1
                  deriv(k) = deriv(k) - apt*rk*(bpt*trm4*trm3+4.0_dp*trm2*rk)
                  d0i(k) = d0i(k) - apt*trm3*rqtrm*rqtrm
                  d0j(k) = d0j(k) - apt*trm3*rqtrm*rqtrm
                  if (lgrad2) then
                    trm5 = trm4*trm4
                    deriv2(k) = deriv2(k) + apt*(bpt*bpt*trm5*trm3+8.0_dp*trm2*rk*bpt*trm4+20.0_dp*trm2*rk2+ &
                      2.0_dp*bpt*trm4*trm1*trm3)
                    d1i(k) = d1i(k) + apt*rk*(bpt*trm4*trm3+4.0_dp*trm2*rk)*rqtrm*rqtrm
                    d1j(k) = d1j(k) + apt*rk*(bpt*trm4*trm3+4.0_dp*trm2*rk)*rqtrm*rqtrm
                    d2ij(k) = d2ij(k) + apt*trm3*rqtrm*rqtrm*rqtrm*(rqtrm + 2.0_dp)
                    d2i2(k) = d2i2(k) + apt*trm3*rqtrm*rqtrm*rqtrm*(rqtrm + 2.0_dp)
                    d2j2(k) = d2j2(k) + apt*trm3*rqtrm*rqtrm*rqtrm*(rqtrm + 2.0_dp)
                    if (lgrad3) then
                      deriv3(k) = deriv3(k) - apt*(6.0_dp*bpt*bpt*trm5*trm1*trm3+12.0_dp*bpt*bpt*trm5*trm2* &
                        rk+40.0_dp*bpt*trm4*trm2*rk2+24.0_dp*trm2*rk*bpt*trm4*trm1+6.0_dp*trm5*bpt*trm3+ &
                        bpt*bpt*bpt*trm4*trm5*trm3)
                    endif
                  endif
                endif
              elseif (nptyp.eq.33) then
!**********************************************
!  Cosh-spring potential - core - shell only  *
!**********************************************
                apt = twopot(1,npot)*sfct
                bpt = twopot(2,npot)
                cpt = cosh(r/bpt)
                eatm = eatm + apt*bpt*bpt*(cpt - 1.0d0)
                if (lgrad1) then
                  dpt = sinh(r/bpt)
                  derivk = derivk + apt*bpt*rk*dpt
                  if (lgrad2) then
                    deriv2k = deriv2k + apt*cpt
                    if (lgrad3) then
                      deriv3k = deriv3k + apt*dpt/bpt
                    endif
                  endif 
                endif
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
                if (lgrad1) then
                  trm2 = apt*exptrm*bpt*(1.0_dp + 2.0_dp*cpt*exptrm)
                  derivk = derivk + r2*r2*(6.0_dp*trm1 - r*trm2)
                  if (lgrad2) then
                    trm3 = apt*exptrm*bpt*bpt*(1.0_dp + 4.0_dp*cpt*exptrm)
                    deriv2k = deriv2k + r2*r2*(30.0_dp*trm1 - 12.0_dp*r*trm2 + r2*trm3)
                    if (lgrad3) then
                      trm4 = apt*exptrm*bpt*bpt*bpt*(1.0_dp + 8.0_dp*cpt*exptrm)
                      deriv3k = deriv3k + r2*r*(120.0_dp*trm1-90.0_dp*r*trm2+18.0_dp*r2*trm3-r2*r*trm4)
                    endif
                  endif  
                endif
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
                    if (lgrad1) then
                      trm = tp*(l-1)*rtrm*rk2
                      if (l.gt.1) then
                        derivk = derivk + trm*trm1 + tp*rtrm*dtrm1*rk
                      else
                        derivk = derivk + tp*rtrm*dtrm1*rk
                      endif
                      if (lgrad2) then
                        if (l.gt.2) then
                          deriv2k = deriv2k + trm*(l-2)*trm1 + 2.0_dp*trm*r*dtrm1 + 2.0_dp*tp*rtrm
                        elseif (l.gt.1) then
                          deriv2k = deriv2k + 2.0_dp*trm*r*dtrm1 + 2.0_dp*tp*rtrm
                        else
                          deriv2k = deriv2k + 2.0_dp*tp*rtrm
                        endif
                        if (lgrad3) then
                          if (l.gt.3) then
                            deriv3k = deriv3k + trm*(l-2)*(l-3)*rk*trm1 + 3.0_dp*trm*(l-2)*dtrm1 +  &
                              6.0_dp*trm*r
                          elseif (l.gt.2) then
                            deriv3k = deriv3k + 3.0_dp*trm*(l-2)*dtrm1 + 6.0_dp*trm*r
                          elseif (l.gt.1) then
                            deriv3k = deriv3k + 6.0_dp*trm*r
                          endif
                        endif
                      endif
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
                if (lgrad1) then
                  trm1 = - 2.0_dp*trm0*rk2
                  deriv(k) = deriv(k) + trm1
                  if (lgrad2) then
                    trm2 = - 3.0_dp*trm1
                    deriv2(k) = deriv2(k) + trm2
                    if (lgrad3) then
                      trm3 = - 4.0_dp*trm2*rk
                      deriv3(k) = deriv3(k) + trm3
                    endif
                  endif
                endif
                eatm = eatm + factor*(eshift(npot) + gshift(npot)*r)
                if (lgrad1) then
                  deriv(k) = deriv(k) + gshift(npot)*rk*factor
                endif
              elseif (nptyp.eq.37) then
!*****************************
!  Force constant potential  *
!*****************************
                apt = twopot(1,npot)*psfct
                bpt = twopot(2,npot)*psfct
                if (lgrad1) derivk = derivk + bpt
                if (lgrad2) deriv2k = deriv2k + apt
              elseif (nptyp.eq.38) then
!***********
!  SRGlue  *
!***********
                rd  = r - twopot(1,npot)
                rd2 = rd*rd
                rd3 = rd2*rd
                rd4 = rd3*rd
                if (r.lt.twopot(1,npot)) then
                  eatom = eatom + psfct*(tpot(1,npot)*rd4 + tpot(2,npot)*rd3 + tpot(3,npot)*rd2 + &
                                         tpot(4,npot)*rd  + tpot(5,npot))
                  if (lgrad1) then
                    trm1 = psfct*(4.0_dp*tpot(1,npot)*rd3 + 3.0_dp*tpot(2,npot)*rd2 + &
                                  2.0_dp*tpot(3,npot)*rd  +        tpot(4,npot))
                    deriv(k) = deriv(k) + trm1*rk
                    if (lgrad2) then
                      trm2 = psfct*(12.0_dp*tpot(1,npot)*rd2 + 6.0_dp*tpot(2,npot)*rd + &
                                     2.0_dp*tpot(3,npot))
                      deriv2(k) = deriv2(k) + trm2
                      if (lgrad3) then
                        trm3 = psfct*(24.0_dp*tpot(1,npot)*rd + 6.0_dp*tpot(2,npot))
                        deriv3(k) = deriv3(k) + trm3
                      endif
                    endif
                  endif
                else
                  rd5 = rd4*rd
                  rd6 = rd5*rd
                  eatom = eatom + psfct*(tpot(6,npot)*rd6 + tpot(7,npot)*rd5 + tpot(8,npot)*rd4 + &
                                         tpot(9,npot)*rd3 + tpot(10,npot)*rd2 + tpot(11,npot)*rd+ &
                                         tpot(12,npot))
                  if (lgrad1) then
                    trm1 = psfct*(6.0_dp*tpot(6,npot)*rd5 + 5.0_dp*tpot(7,npot)*rd4 + &
                                  4.0_dp*tpot(8,npot)*rd3 + 3.0_dp*tpot(9,npot)*rd2 + &
                                  2.0_dp*tpot(10,npot)*rd +        tpot(11,npot))
                    deriv(k) = deriv(k) + trm1*rk
                    if (lgrad2) then
                      trm2 = psfct*(30.0_dp*tpot(6,npot)*rd4 + 20.0_dp*tpot(7,npot)*rd3 + &
                                    12.0_dp*tpot(8,npot)*rd2 +  6.0_dp*tpot(9,npot)*rd  + &
                                     2.0_dp*tpot(10,npot))
                      deriv2(k) = deriv2(k) + trm2
                      if (lgrad3) then
                        trm3 = psfct*(120.0_dp*tpot(6,npot)*rd3 + 60.0_dp*tpot(7,npot)*rd2 + &
                                       24.0_dp*tpot(8,npot)*rd  +  6.0_dp*tpot(9,npot))
                        deriv3(k) = deriv3(k) + trm3
                      endif
                    endif
                  endif
                endif
              elseif (nptyp.eq.39) then
!*********************************************************
!  Morse potential with etaper - no coulomb subtraction  *
!*********************************************************
                call etaper(r,0.0_dp,rpot(npot),etpfn,detpfn,d2etpfn,d3etpfn,lgrad1,lgrad2,lgrad3)
                apt = twopot(1,npot)*psfct
                bpt = twopot(2,npot)
                cpt = twopot(3,npot)
                trme = exp(-bpt*(r-cpt))
                trm1 = 1.0_dp - trme
                trm12 = trm1*trm1
                eatmtrm = apt*(trm12 - 1.0_dp)*etpfn
                eatm = eatm + eatmtrm*etpfn
                if (lgrad1) then
                  trm2 = 2.0_dp*apt*bpt*trme
                  derivktrm = trm2*trm1*rk
                  derivk = derivk + derivktrm*etpfn + eatmtrm*detpfn*rk
                  if (lgrad2) then
                    trm2 = trm2*bpt
                    deriv2ktrm = trm2*(2.0_dp*trme-1.0_dp)
                    deriv2k = deriv2k + deriv2ktrm*etpfn + 2.0_dp*detpfn*derivktrm*r + d2etpfn*eatmtrm
                    if (lgrad3) then
                      deriv3ktrm = trm2*bpt*(1.0_dp-4.0_dp*trme)
                      deriv3k = deriv3k + deriv3ktrm*etpfn + 3.0_dp*(detpfn*deriv2ktrm + d2etpfn*derivktrm*r) + d3etpfn*eatmtrm
                    endif
                  endif
                endif
              elseif (nptyp.eq.40) then
!***********************************************************
!  Morse potential with etaper - with coulomb subtraction  *
!***********************************************************
                call etaper(r,0.0_dp,rpot(npot),etpfn,detpfn,d2etpfn,d3etpfn,lgrad1,lgrad2,lgrad3)
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
                if (lgrad1) then
                  trm2 = 2.0_dp*apt*bpt*trme
                  derivktrm = trm2*trm1*rk
                  derivk = derivk + derivktrm*etpfn + eatmtrm*detpfn*rk
                  derive(k) = dpte*rk2*rk + derive(k)
                  derive0(k) = derive0(k) - dpte*rk
                  if (lgrad2) then
                    trm2 = trm2*bpt
                    deriv2ktrm = trm2*(2.0_dp*trme-1.0_dp)
                    deriv2k = deriv2k + deriv2ktrm*etpfn + 2.0_dp*detpfn*derivktrm*r + d2etpfn*eatmtrm
                    derive2(k) = derive2(k) - 2.0_dp*dpte*rk2*rk
                    if (lgrad3) then
                      deriv3ktrm = trm2*bpt*(1.0_dp-4.0_dp*trme)
                      deriv3k = deriv3k + deriv3ktrm*etpfn + 3.0_dp*(detpfn*deriv2ktrm + d2etpfn*derivktrm*r) + d3etpfn*eatmtrm
                      derive3(k) = derive3(k) + 6.0_dp*dpte*rk2*rk2
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  derivk = derivk - apt*trme*dpt*(bpt - trm1*cpt)*rk
                  if (lgrad2) then
                    deriv2k = deriv2k + apt*trme*dpt*dpt*cpt*(2.0_dp*bpt - trm1*cpt)
                    if (lgrad3) then
                      deriv3k = deriv3k + apt*trme*dpt*dpt*dpt*cpt*cpt*(- 3.0_dp*bpt + trm1*cpt)
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  exptrm1 = exp(-(bpt*r)**2)
                  exptrm2 = exp(-(cpt*r)**2)
                  trm1 = (2.0_dp*apt/sqrtpi)*(bpt*exptrm1*erfctrm - erftrm*cpt*exptrm2)
                  derivk = derivk + trm1*rk2 - trm0*rk2*rk
                  if (lgrad2) then
                    trm21 = 2.0_dp*(cpt*cpt*cpt*exptrm2*erftrm - bpt*bpt*bpt*exptrm1*erfctrm)
                    trm22 = (4.0_dp*bpt*cpt/sqrtpi)*exptrm1*exptrm2
                    trm2 = (2.0_dp*apt/sqrtpi)*(trm21*r - trm22)
                    deriv2k = deriv2k + trm2*rk - 2.0_dp*trm1*rk2 + 2.0_dp*trm0*rk2*rk
                    if (lgrad3) then
                      trm3 = 2.0_dp*((bpt**5)*r*exptrm1*erfctrm - (cpt**5)*r*exptrm2*erftrm)
                      deriv3k = deriv3k + (2.0_dp*apt/sqrtpi)*(trm1 + r*trm3 + 3.0_dp*trm2*r*(bpt*bpt + cpt*cpt))*rk
                      deriv3k = deriv3k - 3.0_dp*trm2*rk2 + 6.0_dp*trm1*rk2*rk - 6.0_dp*trm0*rk2*rk2
                    endif
                  endif
                endif
              elseif (nptyp.eq.43) then
!*************
!  Rep-erfc  *
!*************
                apt = twopot(1,npot)*psfct
                bpt = 1.0_dp/twopot(2,npot)
                erfctrm = derfc(bpt*r)
                trm0 = apt*erfctrm*rk
                eatm = eatm + trm0
                if (lgrad1) then
                  exptrm1 = exp(-(bpt*r)**2)
                  derivk = derivk - apt*rk2*((2.0_dp/sqrtpi)*bpt*exptrm1 + erfctrm*rk)
                  if (lgrad2) then
                    trm1 = (4.0_dp/sqrtpi)*bpt*exptrm1*(r*bpt*bpt + rk)
                    deriv2k = deriv2k + apt*rk*(trm1 + 2.0_dp*rk2*erfctrm)
                    if (lgrad3) then
                      trm2 = (4.0_dp/sqrtpi)*bpt*exptrm1*(2.0_dp*r*bpt*bpt*(bpt*bpt + rk2) + 3.0_dp*rk2*rk)
                      deriv3k = deriv3k - apt*(trm2 + 6.0_dp*erfctrm*rk2*rk2)
                    endif
                  endif
                endif
              elseif (nptyp.eq.44) then
!************
!  Erf pot  *
!************
                apt = twopot(1,npot)*psfct
                bpt = 1.0_dp/twopot(2,npot)
                erftrm = derf(bpt*r)
                trm0 = apt*erftrm*rk
                eatm = eatm + trm0
                if (lgrad1) then
                  exptrm1 = exp(-(bpt*r)**2)
                  derivk = derivk + apt*rk2*((2.0_dp/sqrtpi)*bpt*exptrm1 - erftrm*rk)
                  if (lgrad2) then
                    trm1 = (4.0_dp/sqrtpi)*bpt*exptrm1*(r*bpt*bpt + rk)
                    deriv2k = deriv2k - apt*rk*(trm1 - 2.0_dp*rk2*erftrm)
                    if (lgrad3) then
                      trm2 = (4.0_dp/sqrtpi)*bpt*exptrm1*(2.0_dp*r*bpt*bpt*(bpt*bpt + rk2) + 3.0_dp*rk2*rk)
                      deriv3k = deriv3k + apt*(trm2 - 6.0_dp*erftrm*rk2*rk2)
                    endif
                  endif
                endif
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
                d2frhodr2 = 0.0_dp
                d3frhodr3 = 0.0_dp
                do l = 1,maxmeamorder
                  betaoverr0 = 2.0_dp*tpot(8+l,npot)/r0
                  rhol = tpot(l,npot)*tpot(4+l,npot)*exp(-betaoverr0*(r-r0))
                  frho = frho + rhol
                  if (lgrad1) then
                    dfrhodr = dfrhodr - rhol*betaoverr0
                    if (lgrad2) then
                      d2frhodr2 = d2frhodr2 + rhol*betaoverr0**2
                      if (lgrad3) then
                        d3frhodr3 = d3frhodr3 - rhol*betaoverr0**3
                      endif
                    endif
                  endif
                enddo
                rho = sqrt(frho)
                x = (rho/rho0)
!
                dtrm0dr = bpt/r0
                trm0 = dtrm0dr*(r - r0)
                exptrm = exp(-trm0)
                logtrm = log(x)
                if (abs(d).gt.1.0d-12) then
                  eatm = eatm + EcZ*((1.0_dp + trm0*(1.0_dp + d*trm0**2))*exptrm + apt*x*logtrm)
                  if (lgrad1) then
                    rrho = 1.0_dp/rho
                    dxdr = 0.5_dp*rrho*dfrhodr/rho0
                    deriv(k) = deriv(k) - EcZ*(exptrm*trm0*dtrm0dr*(1.0_dp-d*trm0*(3.0_dp-trm0)) - apt*dxdr*(1.0_dp + logtrm))*rk
                    if (lgrad2) then
                      d2xdr2 = 0.5_dp*rrho*(d2frhodr2 - 0.5_dp*rrho*rrho*dfrhodr*dfrhodr)/rho0
                      deriv2(k) = deriv2(k) - EcZ*(exptrm*(1.0_dp - trm0 - d*trm0*(6.0_dp*(1.0_dp -trm0)+trm0**2 ))*dtrm0dr**2 - &
                                  apt*(d2xdr2*(1.0_dp + logtrm) + dxdr*dxdr/x))
                      if (lgrad3) then
                        d3xdr3 = 0.5_dp*rrho*(d3frhodr3 - 1.5_dp*rrho*rrho*dfrhodr*(d2frhodr2 &
                                 - 0.5_dp*rrho*rrho*dfrhodr*dfrhodr))/rho0
                        deriv3(k) = deriv3(k) + EcZ*(exptrm*(2.0_dp - trm0 + d*(6.0_dp - trm0*(18.0_dp - trm0*(9.0_dp - &
                                                trm0))))*dtrm0dr**3 + apt*(d3xdr3*(1.0_dp + logtrm) &
                                              + (dxdr/x)*(3.0_dp*d2xdr2 - dxdr*dxdr/x)))
                      endif
                    endif
                  endif
                else
                  eatm = eatm + EcZ*((1.0_dp + trm0)*exptrm + apt*x*logtrm)
                  if (lgrad1) then
                    rrho = 1.0_dp/rho
                    dxdr = 0.5_dp*rrho*dfrhodr/rho0
                    deriv(k) = deriv(k) - EcZ*(exptrm*trm0*dtrm0dr - apt*dxdr*(1.0_dp + logtrm))*rk
                    if (lgrad2) then
                      d2xdr2 = 0.5_dp*rrho*(d2frhodr2 - 0.5_dp*rrho*rrho*dfrhodr*dfrhodr)/rho0
                      deriv2(k) = deriv2(k) - EcZ*(exptrm*(1.0_dp - trm0)*dtrm0dr**2 - &
                                  apt*(d2xdr2*(1.0_dp + logtrm) + dxdr*dxdr/x))
                      if (lgrad3) then
                        d3xdr3 = 0.5_dp*rrho*(d3frhodr3 - 1.5_dp*rrho*rrho*dfrhodr*(d2frhodr2 &
                                 - 0.5_dp*rrho*rrho*dfrhodr*dfrhodr))/rho0
                        deriv3(k) = deriv3(k) + EcZ*(exptrm*(2.0_dp - trm0)*dtrm0dr**3 + apt*(d3xdr3*(1.0_dp + logtrm) &
                                    + (dxdr/x)*(3.0_dp*d2xdr2 - dxdr*dxdr/x)))
                      endif
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  trm1 = - apt*exptrm*bpt*rxm1*rxm1
                  dxdr = 0.5_dp/(cpt*x)
                  deriv(k) = deriv(k) + trm1*dxdr*rk
                  if (lgrad2) then
                    trm2 = - trm1*rxm1*(bpt*rxm1 - 2.0_dp)
                    d2xdr2 = - dxdr*dxdr/x
                    deriv2(k) = deriv2(k) + trm2*dxdr*dxdr + trm1*d2xdr2
                    if (lgrad3) then
                      trm3 = - trm2*bpt*rxm1*rxm1 - trm1*(4.0_dp*bpt*rxm1 - 6.0_dp)*rxm1*rxm1
                      d3xdr3 = - 3.0_dp*d2xdr2*dxdr/x
                      deriv3(k) = deriv3(k) + trm3*dxdr*dxdr*dxdr + 3.0_dp*trm2*dxdr*d2xdr2 + trm1*d3xdr3
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  trm1 = cpt + r*(2.0_dp*ept + 3.0_dp*fpt*r)
                  deriv(k) = deriv(k) + apt*exptrm*trm1*rk
                  if (lgrad2) then
                    trm2 = 2.0_dp*ept + 6.0_dp*fpt*r
                    deriv2(k) = deriv2(k) + apt*exptrm*(trm1*trm1 + trm2)
                    if (lgrad3) then
                      trm3 = - trm2*bpt*rxm1*rxm1 - trm1*(4.0_dp*bpt*rxm1 - 6.0_dp)*rxm1*rxm1
                      deriv3(k) = deriv3(k) + apt*exptrm*(trm1*(trm1*trm1 + 3.0_dp*trm2) + 6.0_dp*fpt)
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  trm1 = - 6.0_dp*rk6*rk
                  dfdmp = fdmp*fdmp*bpt*exptrm/cpt
                  deriv(k) = deriv(k) - apt*(trm1*fdmp + rk6*dfdmp)*rk
                  if (lgrad2) then
                    trm2 = 42.0_dp*rk6*rk2
                    d2fdmp = 2.0_dp*dfdmp*fdmp*bpt*exptrm/cpt - dfdmp*bpt/cpt
                    deriv2(k) = deriv2(k) - apt*(trm2*fdmp + 2.0_dp*trm1*dfdmp + rk6*d2fdmp)
                    if (lgrad3) then
                      trm3 = - 336.0_dp*rk6*rk2*rk
                      d3fdmp = 4.0_dp*d2fdmp*dfdmp*bpt*exptrm/cpt - d2fdmp*bpt/cpt - 2.0_dp*dfdmp*dfdmp*bpt*bpt*exptrm/(cpt**2)
                      deriv3(k) = deriv3(k) - apt*(trm3*fdmp + 3.0_dp*(trm2*dfdmp + trm1*d2fdmp) + rk6*d3fdmp)
                    endif
                  endif
                endif
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
                call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,lgrad1,lgrad2,lgrad3)
!
                ecfm = 0.5_dp*apt*(r - bpt)**2
                eatm = eatm + ecfm*(1.0_dp - th)
                if (lgrad1) then
                  decfm = apt*(r - bpt)
                  deriv(k) = deriv(k) + rk*(-ecfm*dthdr + decfm*(1.0_dp - th))
                  if (lgrad2) then
                    d2ecfm = apt
                    deriv2(k) = deriv2(k) - ecfm*d2thdr2 - 2.0_dp*decfm*dthdr + d2ecfm*(1.0_dp - th)
                    if (lgrad3) then
                      deriv3(k) = deriv3(k) - ecfm*d3thdr3 - 3.0_dp*(d2ecfm*dthdr + decfm*d2thdr2)
                    endif
                  endif
                endif
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
                call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,lgrad1,lgrad2,lgrad3)
!
                exptrm = exp(-bpt*(r-cpt)**2)
                ecfm = apt*exptrm
                eatm = eatm + ecfm*th       
                if (lgrad1) then
                  decfm = - 2.0_dp*bpt*(r - cpt)*ecfm
                  deriv(k) = deriv(k) + rk*(ecfm*dthdr + decfm*th)
                  if (lgrad2) then
                    d2ecfm = bpt*ecfm*(4.0_dp*bpt*(r - cpt)**2 - 2.0_dp)
                    deriv2(k) = deriv2(k) + ecfm*d2thdr2 + 2.0_dp*decfm*dthdr + d2ecfm*th
                    if (lgrad3) then
                      d3ecfm = - bpt*bpt*ecfm*(4.0_dp*bpt*(r-cpt)**2 - 6.0_dp)
                      deriv3(k) = deriv3(k) + ecfm*d3thdr3 + 3.0_dp*(d2ecfm*dthdr + decfm*d2thdr2) + d3ecfm*th
                    endif
                  endif
                endif
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
                call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,lgrad1,lgrad2,lgrad3)
!
                ecfm = apt*rk**bpt
                eatm = eatm + ecfm*th       
                if (lgrad1) then
                  decfm = - bpt*ecfm*rk
                  deriv(k) = deriv(k) + rk*(ecfm*dthdr + decfm*th)
                  if (lgrad2) then
                    d2ecfm = - (bpt + 1.0_dp)*decfm*rk
                    deriv2(k) = deriv2(k) + ecfm*d2thdr2 + 2.0_dp*decfm*dthdr + d2ecfm*th
                    if (lgrad3) then
                      d3ecfm = - d2ecfm*(bpt + 2.0_dp)*rk
                      deriv3(k) = deriv3(k) + ecfm*d3thdr3 + 3.0_dp*(d2ecfm*dthdr + decfm*d2thdr2) + d3ecfm*th
                    endif
                  endif
                endif
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
                call tfunc(r,ept,fpt,th,dthdr,d2thdr2,d3thdr3,lgrad1,lgrad2,lgrad3)
!
                exptrm = exp(bpt*(r - cpt))
                rexptrm = 1.0_dp/(1.0_dp + exptrm)
                ecfm = apt*rexptrm
                eatm = eatm + ecfm*th       
                if (lgrad1) then
                  decfm = - ecfm*bpt*exptrm*rexptrm
                  deriv(k) = deriv(k) + rk*(ecfm*dthdr + decfm*th)
                  if (lgrad2) then
                    d2ecfm = - decfm*bpt*(2.0_dp*exptrm*rexptrm - 1.0_dp)
                    deriv2(k) = deriv2(k) + ecfm*d2thdr2 + 2.0_dp*decfm*dthdr + d2ecfm*th
                    if (lgrad3) then
                      d3ecfm = decfm*rexptrm*bpt*bpt*(6.0_dp*exptrm*rexptrm*(exptrm*rexptrm - 1.0_dp) + 1.0_dp)
                      deriv3(k) = deriv3(k) + ecfm*d3thdr3 + 3.0_dp*(d2ecfm*dthdr + decfm*d2thdr2) + d3ecfm*th
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  trm1 = rtrm0*trm0*r
                  deriv(k) = deriv(k) + trm1
                  if (lgrad2) then
                    trm2 = - 4.0_dp*trm1*trm0*r*r2
                    deriv2(k) = deriv2(k) + trm2 + 2.0_dp*trm1
                    if (lgrad3) then
                      trm3 = - 7.0_dp*trm1*trm0*r2
                      deriv3(k) = deriv3(k) + trm3 + 7.5_dp*trm2*rk + 2.0_dp*trm1*rk
                    endif
                  endif
                endif
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
                if (lgrad1) then
                  r4 = r2*r2
                  dfdmp = r4*r2*fdmp
                  trm1 = apt*fdmp
                  deriv(k) = deriv(k) + 6.0_dp*trm1*dfdmp*rk2
                  if (lgrad2) then
                    trm2 = trm1*fdmp
                    deriv2(k) = deriv2(k) + trm2*r4*(30.0_dp - 72.0_dp*dfdmp)
                    if (lgrad3) then
                      deriv3(k) = deriv3(k) + trm2*r*r2*(120.0_dp - 1080.0_dp*dfdmp + 1296.0*dfdmp**2)
                    endif
                  endif
                endif
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
            ttrm1 = (tapermax/taperm)*sumtapergrad
            eatom = eatom + eatm - sumtaperpot + ttrm1*(1.0_dp - (r/tapermax)**taperm)
            if (lgrad1) then
              deriv(k) = deriv(k) + derivk - sumtapergrad*rk*(r/tapermax)**(taperm-1.0_dp)
              rtrm1 = rtrm1 + rtrm1k
              if (lgrad2) then
                deriv2(k) = deriv2(k) + deriv2k - ((taperm - 1.0_dp)/tapermax)*sumtapergrad*(r/tapermax)**(taperm-2.0_dp)
                rtrm2(k) = rtrm2(k) + rtrm2k
                rderiv(k) = rderiv(k) + rderivk
                if (lgrad3) then
                  deriv3(k) = deriv3(k) + deriv3k - ((taperm - 1.0_dp)*(taperm - 2.0_dp)/tapermax**2)* &
                    sumtapergrad*(r/tapermax)**(taperm-3.0_dp)
                  rtrm3(k) = rtrm3(k) + rtrm3k
                  rtrm32(k) = rtrm32(k) + rtrm32k
                endif
              endif
            endif
          else
!
!  Polynomial, cosine, MDF or exponential taper
!
            if (tapertype.eq.1) then
              call p5taper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,lgrad1,lgrad2,lgrad3)
            elseif (tapertype.eq.4) then
              call etaper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,lgrad1,lgrad2,lgrad3)
            elseif (tapertype.eq.5) then
              call mdftaper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,lgrad1,lgrad2,lgrad3)
            else
              call ctaper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,lgrad1,lgrad2,lgrad3)
            endif
            eatom = eatom + tpfn*eatm
            if (lgrad1) then
              deriv(k) = deriv(k) + tpfn*derivk + dtpfn*eatm*rk
              rtrm1 = rtrm1 + tpfn*rtrm1k
              if (lgrad2) then
                deriv2(k) = deriv2(k) + tpfn*deriv2k + 2.0_dp*dtpfn*derivk*r + d2tpfn*eatm
                rtrm2(k) = rtrm2(k) + tpfn*rtrm2k
                rderiv(k) = rderiv(k) + tpfn*rderivk
                if (lgrad3) then
                  deriv3(k) = deriv3(k) + tpfn*deriv3k + 3.0_dp*dtpfn*deriv2k + 3.0_dp*d2tpfn*derivk*r + d3tpfn*eatm
                  rtrm3(k) = rtrm3(k) + tpfn*rtrm3k
                  rtrm32(k) = rtrm32(k) + tpfn*rtrm32k
                endif
              endif
            endif
          endif
        else
          eatom = eatom + eatm
          if (lgrad1) then
            deriv(k) = deriv(k) + derivk
            rtrm1 = rtrm1 + rtrm1k
            if (lgrad2) then
              deriv2(k) = deriv2(k) + deriv2k
              rtrm2(k) = rtrm2(k) + rtrm2k
              rderiv(k) = rderiv(k) + rderivk
              if (lgrad3) then
                deriv3(k) = deriv3(k) + deriv3k
                rtrm3(k) = rtrm3(k) + rtrm3k
                rtrm32(k) = rtrm32(k) + rtrm32k
              endif
            endif
          endif
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
            if (lgrad1) then
              ctrm1 = 6.0_dp*ctrm1*rk2
              deriv(k) = deriv(k) - ctrm1
              if (lgrad2) then
                ctrm1 = 7.0_dp*ctrm1
                deriv2(k) = deriv2(k) + ctrm1
                if (lgrad3) then
                  ctrm1 = 8.0_dp*ctrm1*rk
                  deriv3(k) = deriv3(k) - ctrm1
                endif
              endif
            endif
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
            if (lgrad1) then
              deriv(k) = deriv(k) + c6trm1*(6.0_dp*rk2 + 2.0_dp*eta)
              deriv(k) = deriv(k) - 2.0_dp*c6t2*eta*(1.0_dp + etar2)
              if (lgrad2) then
                deriv2(k) = deriv2(k) - 42.0_dp*c6trm1*rk2
                deriv2(k) = deriv2(k) - c6t2*etar2*etar2*eta*(7.0_dp + 2.0_dp*etar2)
                if (lgrad3) then
                  deriv3(k) = deriv3(k) + c6t2*rk2*rk*(336.0_dp + 336.0_dp*etar2 +  &
                    168.0_dp*etar2*etar2 + 56.0_dp*etar2*etar2*etar2 + 14.0_dp*etar2*etar2*etar2*etar2 +  &
                    4.0_dp*etar2*etar2*etar2*etar2*etar2)
                endif
              endif
            endif
          endif
        else
!
!  Cluster case
!
          ctrm1 = c6tot*r6
          ec6 = ec6 - ctrm1
          if (lgrad1) then
            ctrm1 = 6.0_dp*ctrm1*rk2
            deriv(k) = deriv(k) + ctrm1
            if (lgrad2) then
              ctrm1 = 7.0_dp*ctrm1
              deriv2(k) = deriv2(k) - ctrm1
              if (lgrad3) then
                ctrm1 = 8.0_dp*ctrm1*rk
                deriv3(k) = deriv3(k) + ctrm1
              endif
            endif
          endif
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
              lneedgrad1 = lgrad1
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
                if (lgrad2) then
                  derive2(k) = derive2(k) + 2.0_dp*(trm2 + trm1*etaloc)
                  if (lgrad3) then
                    derive3(k) = derive3(k) - 6.0_dp*trm2*rk
                    derive3(k) = derive3(k) - 4.0_dp*trm1*etaloc*(rk + etaloc*r)
                  endif
                endif
              endif
!
!  Variable charge terms
!
              if ((leem.or.lDoQDeriv2).and.lgrad1) then
                d1i(k) = d1i(k) - trm2*angstoev*qj
                d1j(k) = d1j(k) - trm2*angstoev*qi
                if (lgrad2) then
                  d2ij(k) = d2ij(k) + erffc*angstoev
                endif
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
                  if (lgrad1) then
                    trm1 = tweatpi*exp(-etaloc*cutw*cutw)*sfct
                    erffc = effc*sfct
                    trm2 = rkw*rkw*(erffc + trm1)
                    derive(k) = derive(k) + trm2
                    derive0(k) = derive0(k) - effc*sfct
                    if (lgrad2) then
                      derive2(k) = derive2(k) - 2.0_dp*(trm2 + trm1*etaloc)
                      if (lgrad3) then
                        derive3(k) = derive3(k) + 6.0_dp*trm2*rkw
                        derive3(k) = derive3(k) + 4.0_dp*trm1*etaloc*(rkw + etaloc*cutw)
                      endif
                    endif
                  endif
!
!  Variable charge terms
!
                  if ((leem.or.lDoQDeriv2).and.lgrad1) then
                    d1i(k) = d1i(k) + trm2*angstoev*qj
                    d1j(k) = d1j(k) + trm2*angstoev*qi
                    if (lgrad2) then
                      d2ij(k) = d2ij(k) - erffc*angstoev
                    endif
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
              if (lgrad1) then
                trm1 = sfct*rk*rk2
                derive(k) = derive(k) + trm1
                derive0(k) = derive0(k) - sfct*rk
                if (lgrad2) then
                  derive2(k) = derive2(k) - 2.0_dp*trm1
                  if (lgrad3) then
                    derive3(k) = derive3(k) + 6.0_dp*trm1*rk
                  endif
                endif
!
!  Variable charge terms
!
                if ((leem.or.lDoQDeriv2).and.lgrad1) then
                  trm2 = trm1*angstoev
                  d1i(k) = d1i(k) + trm2*qj
                  d1j(k) = d1j(k) + trm2*qi
                  if (lgrad2) then
                    d2ij(k) = d2ij(k) - rk*sfct*angstoev
                  endif
                endif
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
            if (lgrad1) then
              trm1 = rk*rk2*sfct
              derive(k) = derive(k) + trm1
              derive0(k) = derive0(k) - sfct*rk
              if (lgrad2) then
                derive2(k) = derive2(k) - 2.0_dp*trm1
                if (lgrad3) then
                  derive3(k) = derive3(k) + 6.0_dp*trm1*rk
                endif
              endif
            endif
!
!  Variable charge terms
!
            if ((leem.or.lDoQDeriv2).and.lgrad1) then
              trm2 = trm1*angstoev
              d1i(k) = d1i(k) + trm2*qj
              d1j(k) = d1j(k) + trm2*qi
              if (lgrad2) then
                d2ij(k) = d2ij(k) - rk*sfct*angstoev
              endif
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
              if (lgrad1) then
                trm1 = rk*rk2*sfct
                derive(k) = derive(k) - trm1
                derive0(k) = derive0(k) + sfct*rk
                if (lgrad2) then
                  derive2(k) = derive2(k) + 2.0_dp*trm1
                  if (lgrad3) then
                    derive3(k) = derive3(k) - 6.0_dp*trm1*rk
                  endif
                endif
              endif
!
!  Variable charge terms
!
              if ((leem.or.lDoQDeriv2).and.lgrad1) then
                trm2 = trm1*angstoev
                d1i(k) = d1i(k) - trm2*qj
                d1j(k) = d1j(k) - trm2*qi
                if (lgrad2) then
                  d2ij(k) = d2ij(k) + rk*sfct*angstoev
                endif
              endif
            endif
          endif
        endif
      endif
    endif
!***************************
!  Final merging of terms  *
!***************************
!
!  First add on electrostatic contribution
!
    if (abs(sfct).gt.1.0d-15.and..not.lskipq) then
      if (lgrad1) deriv(k) = deriv(k) + factor*derive(k)/sfct
      if (lgrad2) deriv2(k) = deriv2(k) + factor*derive2(k)/sfct
      if (lgrad3) deriv3(k) = deriv3(k) + factor*derive3(k)/sfct
    endif
!
!  Now combine terms
!
    if (lgrad2) then
      deriv2(k) = rk2*(deriv2(k) - deriv(k))
      derive2(k) = rk2*(derive2(k) - derive(k))
      if (lgrad3) then
        deriv3(k) = rk*(rk2*deriv3(k) - 3.0_dp*deriv2(k)*rk)
        derive3(k) = rk*(rk2*derive3(k) - 3.0_dp*derive2(k)*rk)
      endif
    endif
!
!  Convert units of derive, derive2, derive3
!
    if (.not.lskipq) then
      if (lgrad1) then
        derive0(k) = angstoev*derive0(k)
        derive(k)  = angstoev*derive(k)
        if (lgrad2) then
          derive2(k) = angstoev*derive2(k)
          if (lgrad3) then
            derive3(k) = angstoev*derive3(k)
          endif
        endif
      endif
    endif
  enddo
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
!*******************************************************************
!  Wrapper routine for twobody                                     *
!*******************************************************************
  subroutine twobody1(eatom,ereal,ec6,lgrad1,lgrad2,lgrad3,nor,nor0,dist1,x1,y1,z1,d0i1,d0j1,deriv1, &
                      deriv21,deriv31,derive10,derive1,derive21,derive31,rderiv1,npots,npotl,cut2r,cut2e, &
                      cut2s,lptrmol1,nmolonly,factor,sfct,radsum,rtrm1,rtrm21,rtrm31,rtrm321,sctrm1, &
                      sctrm2,qi,qj,lcspair,lperiodic,lregion1,lbonded1,l2bonds1,l3bonds1,nbotype_1, &
                      nbotype2_1,lskipsr,lskipq,lorder12,d1i1,d1j1,d2i21,d2ij1,d2j21)
!
!  Wrapper for calling twobody when nor = 1 and arguments
!  being passed are not those in the modules.
!
!   2/01 Created from twobody
!   9/04 Extra argument of derive10 added
!   9/04 d0i1/d0j1 added
!   9/04 d1j1 added to arguments & d2i21/d2j21
!   8/06 Modified so that arguments are set to zero on return
!        if not calculated
!   3/07 Bondtypes added
!   5/07 lskipq added
!  11/08 x1, y1, z1 (Cartesian components of dist1 vector) added to argument list
!   3/09 lorder12 now passed through too
!   6/09 Charge as a coordinate option added
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
  use realvectors
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)     :: nmolonly
  integer(i4), intent(in)     :: nor
  integer(i4), intent(in)     :: nor0
  integer(i4), intent(in)     :: npotl(*)
  integer(i4), intent(in)     :: npots
  integer(i4), intent(in)     :: nbotype_1
  integer(i4), intent(in)     :: nbotype2_1
  logical,     intent(in)     :: l2bonds1
  logical,     intent(in)     :: l3bonds1
  logical,     intent(in)     :: lbonded1
  logical,     intent(in)     :: lgrad1
  logical,     intent(in)     :: lgrad2
  logical,     intent(in)     :: lgrad3
  logical,     intent(in)     :: lcspair
  logical,     intent(in)     :: lorder12
  logical,     intent(in)     :: lperiodic
  logical,     intent(in)     :: lptrmol1
  logical,     intent(in)     :: lregion1
  logical,     intent(in)     :: lskipq
  logical,     intent(in)     :: lskipsr
  real(dp),    intent(in)     :: cut2e
  real(dp),    intent(in)     :: cut2r
  real(dp),    intent(in)     :: cut2s
  real(dp),    intent(out)    :: d0i1
  real(dp),    intent(out)    :: d0j1
  real(dp),    intent(out)    :: d1i1
  real(dp),    intent(out)    :: d1j1
  real(dp),    intent(out)    :: d2i21
  real(dp),    intent(out)    :: d2ij1
  real(dp),    intent(out)    :: d2j21
  real(dp),    intent(inout)  :: dist1
  real(dp),    intent(out)    :: deriv1
  real(dp),    intent(out)    :: deriv21
  real(dp),    intent(out)    :: deriv31
  real(dp),    intent(out)    :: derive10
  real(dp),    intent(out)    :: derive1
  real(dp),    intent(out)    :: derive21
  real(dp),    intent(out)    :: derive31
  real(dp),    intent(inout)  :: eatom
  real(dp),    intent(inout)  :: ec6
  real(dp),    intent(inout)  :: ereal
  real(dp),    intent(in)     :: factor
  real(dp),    intent(in)     :: qi 
  real(dp),    intent(in)     :: qj 
  real(dp),    intent(in)     :: radsum
  real(dp),    intent(out)    :: rderiv1
  real(dp),    intent(out)    :: rtrm1
  real(dp),    intent(out)    :: rtrm21
  real(dp),    intent(out)    :: rtrm31
  real(dp),    intent(out)    :: rtrm321
  real(dp),    intent(out)    :: sctrm1
  real(dp),    intent(out)    :: sctrm2
  real(dp),    intent(in)     :: sfct
  real(dp),    intent(in)     :: x1
  real(dp),    intent(in)     :: y1
  real(dp),    intent(in)     :: z1
!
!  Assign incoming variables to appropriate module values
!
  l2bonds(1)  = l2bonds1
  l3bonds(1)  = l3bonds1
  lbonded(1)  = lbonded1
  lptrmol(1)  = lptrmol1
  nbotype(1)  = nbotype_1
  nbotype2(1) = nbotype2_1
  dist(1)     = dist1
  xtmp(1)     = x1
  ytmp(1)     = y1
  ztmp(1)     = z1
!
!  Call real twobody routine
!
  call twobody(eatom,ereal,ec6,lgrad1,lgrad2,lgrad3,nor,nor0,npots,npotl,cut2r,cut2e,cut2s,nmolonly, &
               factor,sfct,radsum,rtrm1,sctrm1,sctrm2,qi,qj,lcspair,lperiodic,lregion1,lskipsr,lskipq,lorder12)
!
!  Assign outgoing variables from appropriate module values
!
  dist1    = dist(1)
  derive10 = derive0(1)
  if (lgrad1) then
    d0i1     = d0i(1)
    d0j1     = d0j(1)
    d1i1     = d1i(1)
    d1j1     = d1j(1)
    deriv1   = deriv(1)
    derive1  = derive(1)
  else
    d0i1     = 0.0_dp
    d0j1     = 0.0_dp
    d1i1     = 0.0_dp
    d1j1     = 0.0_dp
    deriv1   = 0.0_dp
    derive1  = 0.0_dp
  endif
  if (lgrad2) then
    d2i21     = d2i2(1)
    d2ij1     = d2ij(1)
    d2j21     = d2j2(1)
    deriv21   = deriv2(1)
    derive21  = derive2(1)
    rtrm21    = rtrm2(1)
    rderiv1   = rderiv(1)
  else
    d2i21     = 0.0_dp
    d2ij1     = 0.0_dp
    d2j21     = 0.0_dp
    deriv21   = 0.0_dp
    derive21  = 0.0_dp
    rtrm21    = 0.0_dp
    rderiv1   = 0.0_dp
  endif
  if (lgrad3) then
    deriv31  = deriv3(1)
    derive31 = derive3(1)
    rtrm31   = rtrm3(1)
    rtrm321  = rtrm32(1)
  else
    deriv31  = 0.0_dp
    derive31 = 0.0_dp
    rtrm31   = 0.0_dp
    rtrm321  = 0.0_dp
  endif
!
  return
  end
