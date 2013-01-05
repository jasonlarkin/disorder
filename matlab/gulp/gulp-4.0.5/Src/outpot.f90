  subroutine outpot
!
!  Outputs interatomic potentials 
!
!  Potential type for onebody:
!
!   Only type is ashift
!
!  Potential type is given by nptype for twobody:
!
!   1 = buckingham
!   2 = lennard-jones
!   3 = morse
!   4 = morse-coulomb subtracted
!   5 = harmonic
!   6 = harmonic-coulomb subtracted
!   7 = general potential
!   8 = spring (core-shell only)
!   9 = coulomb
!  10 = buckingham 4 range
!  11 = spline
!  12 = lennard-jones - epsilon/sigma form : sigma = r at E=0
!  13 = lennard-jones - epsilon/sigma form : sigma = r at minimum
!  14 = BSM - breathing shell model
!  15 = Stillinger-Weber 2 body potential
!  16 = Inverse gaussian potential
!  17 = BSM - exponential
!  18 = Damped dispersion
!  19 = Many-body potential for metals
!  20 = Rydberg / Rose-Smith-Guinea-Ferrante potential
!  21 = Lennard-Jones with epsilon/sigma input : ESFF combination rules
!  22 = qtaper (short range Coulomb taper)
!  23 = polynomial 
!  24 = qerfc (Coulomb with erfc)
!  25 = CovExp (Covalent-Expotential form)
!  26 = Fermi-Dirac form
!  27 = Lennard-Jones buffered
!  28 = Squared harmonic potential
!  29 = Squared harmonic with Coulomb correction
!  30 = Tsuneyuki Coulomb correction potential
!  31 = BSM - single exponential breathing shell model
!  32 = Stillinger-Weber - Jiang & Brown modification
!  33 = Cosh spring potential
!  34 = EAM potential shift
!  35 = Poly harmonic 
!  36 = qoverr2
!  37 = force constant
!  38 = sr_glue
!  39 = morse with etaper
!  40 = morse-coulomb subtracted with etaper
!  41 = Mei-Davenport twobody
!  42 = erferfc potential
!  43 = reperfc potential
!  44 = erf potential
!  45 = Baskes twobody potential
!  46 = VBO_twobody potential
!  47 = exppowers
!  48 = Grimme_C6
!  49 = cfm_harmonic
!  50 = cfm_gaussian
!  51 = cfm_power
!  52 = cfm_fermi
!  53 = gcoulomb potential
!  54 = Becke_Johnson_C6
!
!  Potential type is given by nthrty for three-body:
!
!   1 = conventional harmonic (k2/k3/k4)
!   2 = exponentially decaying harmonic
!   3 = Axilrod-Teller
!   4 = Exponential three-body form
!   5 = Stillinger-Weber form
!   6 = Bcross (bond-bond cross term)
!   7 = Urey-Bradley
!   8 = exponentially decaying - Vessal form
!   9 = cosine harmonic
!  10 = Murrell-Mottram
!  11 = BAcross - theta
!  12 = Linear-three
!  13 = Bcoscross
!  14 = Stillinger-Weber - Jiang & Brown modification
!  15 = Hydrogen-bond
!  16 = Equatorial
!  17 = UFF3
!  18 = BAcoscross
!  19 = 3Coulomb
!  20 = exp2
!  21 = g3Coulomb
!
!  Potential type is given by nforty for four-body:
!
!   1 = standard torsion
!   2 = Ryckaert-Bellemanns
!   3 = Out of plane
!   4 = ESFF torsion
!   5 = Harmonic
!   6 = Exponentially decaying standard
!   7 = Exponentially decaying ESFF
!   8 = Tapered standard
!   9 = Tapered ESFF
!  10 = Angle cross term
!  11 = Inversion
!  12 = Inversion squared
!  13 = UFF4
!  14 = Angle-angle cross potential
!  15 = UFFoop
!  16 = Cos angle - cos angle cross potential
!
!  Called from gulp and fit
!
!   1/95 Intra/intermolecular option added for 3 and 4 body terms
!   1/95 K3 added for three-body harmonic potential
!   2/95 K3 and K4 added for harmonic potential
!   2/95 phi0 added for standard torsional potential 
!   2/95 Exponential and SW 3-body forms added
!   2/95 Bonded specification for 3 and 4 body potentials added
!   3/95 Bcross potential added
!   3/95 SW2 potential added
!   4/95 AB combination rules added for Lennard-Jones potential
!   3/96 Urey-Bradley potential added
!   4/96 Exponentially decaying Vessal form added
!   5/96 Inverse gaussian potential added
!   6/96 General theta0 added to SW3
!   2/97 BSM exponential and damped dispersion added
!   3/97 Outofplane potential added
!   4/97 Many-body potential added
!   7/97 EAM densities added
!   7/97 EAM functionals added
!   7/97 Rose-SGF potential added
!   3/98 Cosine based harmonic three body potential added
!   4/98 ESFF form of Lennard-Jones combination rules added
!   5/98 qtaper potential added
!   6/98 Murrell-Mottram potential added
!   8/98 ESFF torsional potential added
!  10/98 Bond-Angle cross potential added
!   1/99 Modifications to allow for 1-4 only potentials added
!   8/99 Linear-threebody term added
!  10/99 Cubic EAM density added
!   7/00 CovExp added
!   2/01 Fermi-Dirac form added
!   5/01 Output of 3-body potentials modified to allow for
!        minimum cut-offs
!   5/02 Scaling of shifts added
!   5/02 Brenner potential added
!   7/02 Out of plane K4 added
!   8/02 L-J buffered added
!  10/02 Torharm potential added
!  10/02 ReaxFF forcefield added
!   4/03 Exponentially decaying torsion added
!   4/03 Tapered torsion potentials added
!   7/03 Checking removed to separate routine
!   7/03 Bond order potentials added
!  11/03 ndennat/ndentyp replaced
!  11/03 Output of EAM alloy factors added
!   3/04 Squared harmonic potential added
!   7/04 Output for bond order charge potential added
!   9/04 Output for bond order charge self energy added
!  11/04 Output for torangle potential added
!  11/04 Output for six-body potentials added
!   2/05 Rho added for boselfenergy
!   4/05 Cosh spring potential added
!   6/05 Error in output of coefficients for Ryckaert-Bellemanns potl fixed
!   7/05 Output of constant terms for EAM functionals sqrt & power added
!   7/05 Output for Brenner 1990 potential added
!   9/05 Voter form of density for EAM added
!  10/05 Modified to handle numerical EAM functional
!  10/05 Hydrogen-bond potential added
!  10/05 Modified to handle Voter style tapering of Morse
!  10/05 Inversion outofplane potential added
!  11/05 EAM potential shift added
!  12/05 Equatorial ESFF three-body term added
!   2/06 Quadratic and quartic densities added
!   3/06 Modified to allow for density component number
!   3/06 Poly harmonic potential added
!   4/06 Species specific density added
!   4/06 Error in output for taper option fixed
!   5/06 Modified to handle introduction of neamfnspec
!   6/06 Squared inversion added
!  11/06 qoverr2 potential added
!   1/07 UFF3 potential added
!   1/07 UFF4 potential added
!   1/07 force constant potential added
!   2/07 Output of bond type for 4 & 6 body potentials added
!   2/07 End of CML output moved inside conditional statement
!   2/07 Johnson and Glue EAM functionals added
!   2/07 Glue density added
!   3/07 sr_glue potential added
!   4/07 Cyclic/exocyclic label now output for torsions
!   4/07 Units of linear-threebody corrected to eV in output
!   5/07 Exponential taper added
!   5/07 Morse with etaper added
!   5/07 eVoter EAM density added
!   5/07 Foiles EAM functional added
!   7/07 Plane potential added
!   8/07 Format statement for maximum range of potentials changed
!  10/07 Angle-angle cross potential added
!  11/07 Mei-Davenport potential added
!  12/07 Error in type setting for output of sixbody potential labels fixed
!   3/08 erferfc, erfpot and reperfc potentials added
!   4/08 Minimum cutoffs added for out of plane
!   5/08 UFF oop potential added
!   5/08 Output of bond type controls added for twobody pots
!  10/08 Output of taper function name corrected for MDF case
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 bacoscross form added
!  11/08 xcosangleangle potential added
!  11/08 torcosangle potential added
!  11/08 Output format statement for bond-bond cross potential K & standard
!        torsion K modified from g10.5 to g10.4
!  11/08 Baskes form of exponential density added
!  11/08 Baskes form of functional added
!  11/08 Output modified to accommodate MEAM densities
!  12/08 eammeamcoeff array changed from density to function related
!  12/08 rho0 added to Baskes functional
!   1/09 Output for Murrell-Mottram potential now includes c0 - c10
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 leshift/lgshift logicals introduced to replace use of tpot(3/4,) for energy
!        and gradient shifts
!   1/09 Baskes twobody potential added
!   1/09 VBO_twobody potential added
!   1/09 VBO density added 
!   1/09 VBO functional added 
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   3/09 3coulomb potential added
!   4/09 MEAM type output added
!   4/09 Output of MEAM screening parameters added
!   6/09 Module name changed from three to m_three
!   7/09 exp2 potential added
!   7/09 exppowers potential added
!   8/09 Grimme_C6 potential added
!   9/09 Belashchenko EAM functional added
!   9/09 Maximum order of polynomial potential increased to 8
!   1/10 One-body potentials added
!   2/10 Central force model potentials added
!   3/10 Format of output statements adjusted to accommodate
!        uo2_tiwary library
!   5/10 gcoulomb potential added
!   6/10 Bond order header wrapped with if statements for theta
!        option by C. Fisher
!   6/10 Modified to handle optional second atom type for bond-order
!        attractive and repulsive terms.
!   9/10 EDIP force field added
!  10/10 Output of EDIP parameters added
!  10/10 EDIP linear threebody modifications added
!   3/11 namepot moved to module
!   9/11 Twobody potential output format changed for 4th term to g9.3
!  10/11 Fractional power density added
!   1/12 Output for mmexc = 4 case added for missing non-polynomial case
!   1/12 Output for mmexc = 5 case added
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
!  Julian Gale, NRI, Curtin University, January 2012
!
  use bondorderdata
  use brennerdata
  use configurations
  use constants
  use control
  use current
  use eam
  use EDIPdata
  use element,        only : maxele
  use four
  use general,        only : nwarn
  use iochannels
  use m_three
  use molecule
  use one
  use parallel
  use plane
  use potentialnames, only : namepot
  use reaxFFdata
  use shell
  use shifts
  use six
  use species
  use terse,          only : ltersepotentials
  use two
  implicit none
!
!  Local variables
!
  integer(i4), dimension(:), allocatable       :: iptyp
  integer(i4), dimension(:), allocatable       :: ntemp
  character(len=1)                             :: sgs(2)
  character(len=1)                             :: cstyp1
  character(len=1)                             :: cstyp2
  character(len=1)                             :: cstyp3
  character(len=1)                             :: cstyp4
  character(len=1)                             :: cstyp5
  character(len=1)                             :: cstyp6
  character(len=3)                             :: botyword2(3)
  character(len=5)                             :: lab1
  character(len=5)                             :: lab2
  character(len=5)                             :: lab3
  character(len=5)                             :: lab4
  character(len=5)                             :: lab5
  character(len=5)                             :: lab6
  character(len=9)                             :: botyword(7)
  character(len=12)                            :: denfntyp
  integer(i4)                                  :: i
  integer(i4)                                  :: ind
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: mpt
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nat3
  integer(i4)                                  :: nat4
  integer(i4)                                  :: nat5
  integer(i4)                                  :: nat6
  integer(i4)                                  :: nbotyptr
  integer(i4)                                  :: nmpt(3)
  integer(i4)                                  :: norder
  integer(i4)                                  :: np
  integer(i4)                                  :: npi
  integer(i4)                                  :: npt
  integer(i4)                                  :: ntmp
  integer(i4)                                  :: ntype1
  integer(i4)                                  :: ntype2
  integer(i4)                                  :: ntype3
  integer(i4)                                  :: ntype4
  integer(i4)                                  :: ntype5
  integer(i4)                                  :: ntype6
  integer(i4)                                  :: order
  integer(i4)                                  :: status
  logical                                      :: lwarnpn
  logical                                      :: lout
  real(dp)                                     :: cpoti
  real(dp)                                     :: dpoti
  real(dp)                                     :: dcutp
  real(dp)                                     :: phi0
  real(dp)                                     :: the
!
  data (sgs(j),j=1,2)/'+','-'/
  data botyword/'         ','single   ','double   ','triple   ', &
                'quadruple','resonant ','amide    '/
  data botyword2/'   ','cyc','exo'/
!
  dcutp = 50.0_dp
!
!  Beyond this point everything is I/O
!
  if (.not.ioproc.or.ltersepotentials) return
!
  if (none.gt.0.or.npote.gt.0.or.lbrenner.or.lreaxFF.or.lEDIP) then
!
!  Sort potentials for those which are inter- and intra- molecular
!
    nmpt(2) = 0
    nmpt(3) = 0
    allocate(iptyp(npote),stat=status)
    if (status/=0) call outofmemory('outpot','iptyp')
    if (lmol) then
      nmpt(1) = 0
      do i = 1,npote
        if (lintra(i).and..not.linter(i)) then
          nmpt(2) = nmpt(2) + 1
          iptyp(i) = 2
        elseif (linter(i).and..not.lintra(i)) then
          nmpt(3) = nmpt(3) + 1
          iptyp(i) = 3
        else
          nmpt(1) = nmpt(1) + 1
          iptyp(i) = 1
        endif
      enddo
    else
      nmpt(1) = npote
      do i = 1,npote
        iptyp(i) = 1
      enddo
    endif
!**********************************
!  Output interatomic potentials  *
!**********************************
    if (cutp.ne.dcutp) then
      write(ioout,'(/,''  Maximum range for interatomic potentials = '',f16.6,'' Angstroms'')') cutp
    endif
    if ((tapermax - tapermin).gt.0.01_dp) then
      if (tapertype.eq.1) then
        write(ioout,'(/,''  Taper potentials to zero over '',f8.4,'' Angstroms using polynomial'')') (tapermax - tapermin)
      elseif (tapertype.eq.3) then
        write(ioout,'(/,''  Taper potentials to zero over '',f8.4,'' Angstroms using Voter m = '',f8.4)') tapermax,taperm
      elseif (tapertype.eq.4) then
        write(ioout,'(/,''  Taper potentials to zero over '',f8.4,'' Angstroms using exponential'')') tapermax
      elseif (tapertype.eq.5) then
        write(ioout,'(/,''  Taper potentials to zero over '',f8.4,'' Angstroms using M-D-F'')') tapermax
      else
        write(ioout,'(/,''  Taper potentials to zero over '',f8.4,'' Angstroms using cosine'')') (tapermax - tapermin)
      endif
    endif
    if (lMEAMscreen) then
      write(ioout,'(/,''  MEAM densities to be screened using ellipse parameters: Cmin = '',f8.4)') meam_Cmin
      write(ioout,'(''                                                          Cmax = '',f8.4)') meam_Cmax
    endif
    if (lc6) then
      if (lc6one) then
        write(ioout,'(/,''  C6 terms to be calculated in real/reciprocal space by one-centre decomposition'')')
      else
        write(ioout,'(/,''  C6 terms to be calculated in real and reciprocal space '')')
      endif
    endif
    if (lbrenner) then
      if (nbrennertype.eq.3) then
        if (lbrennersplineh) then
          write(ioout,'(/,''  Brenner et al 2002 potential to be used with explicit splines for P'',/)')
        else
          write(ioout,'(/,''  Brenner et al 2002 potential to be used with pretabulated spline for P'',/)')
        endif
      elseif (nbrennertype.eq.1) then
        write(ioout,'(/,''  Brenner 1990 and Dyson-Smith 1996 potential to be used '',/)')
      endif
    endif
    if (lreaxFF) then
      write(ioout,'(/,''  ReaxFF forcefield to be used'',/)')
      write(ioout,'(''  ReaxFF Coulomb cutoff = '',f8.4,'' Ang'')') reaxFFcutoffQ
      write(ioout,'(''  ReaxFF VDW     cutoff = '',f8.4,'' Ang'')') reaxFFcutoffVDW
      write(ioout,'(''  ReaxFF H-bond  cutoff = '',f8.4,'' Ang'',/)') reaxFFrhtol
      write(ioout,'(''  ReaxFF pairwise bond order       threshold = '',f16.14)') reaxFFtol
      write(ioout,'(''  ReaxFF angle/torsion bond order  threshold = '',f16.14)') reaxFFatol
      write(ioout,'(''  ReaxFF bond order double product threshold = '',f16.14)') reaxFFatol2
      write(ioout,'(''  ReaxFF bond order triple product threshold = '',f16.14)') reaxFFatol3
      write(ioout,'(''  ReaxFF hydrogen-bond bond order  threshold = '',f16.14,/)') reaxFFhtol
    endif
    if (lEDIP) then
      write(ioout,'(/,''  EDIP forcefield to be used'')')
      if (nEDIPspec.gt.0) then
!
!  Coordination number
!
        write(ioout,'(/,''  EDIP Coordination Number :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Types            alpha      f_low      f_high     p_low      p_high       '')') 
        write(ioout,'(''                                  (Ang)      (Ang)      (Ang)      (Ang)        '')') 
        write(ioout,'(''  1     2              Zdih       Zrep       c0                                 '')')
        write(ioout,'(''                                             (Ang)                              '')') 
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        ind = 0
        do i = 1,nEDIPspec
          nat1 = natEDIPspec(i)
          ntype1 = ntypEDIPspec(i)
          call label(nat1,ntype1,lab1)
          cstyp1 = 'c'
          if (nat1.gt.maxele) cstyp1 = 's'
          do j = 1,i
            ind = ind + 1
            if (lEDIPpairOK(ind)) then
              nat2 = natEDIPspec(j)
              ntype2 = ntypEDIPspec(j)
              call label(nat2,ntype2,lab2)
              cstyp2 = 'c'
              if (nat2.gt.maxele) cstyp2 = 's'
              write(ioout,'(2(a5,a1,1x),6x,f10.5,4(1x,f10.5))') &
                    lab1,cstyp1,lab2,cstyp2,EDIPalpha(ind),EDIPflow(ind),EDIPfhigh(ind),EDIPplow(ind),EDIPphigh(ind)
              write(ioout,'(20x,f10.5,2(1x,f10.5))') &
                    EDIPZdih(ind),EDIPZrep(ind),EDIPc0(ind)
            endif
          enddo
        enddo
        write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Twobody energy
!
        write(ioout,'(/,''  EDIP Twobody Energy :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Types   Epsilon       B        Beta       Sigma        a        a_prime   '')')
        write(ioout,'(''  1     2      (eV)       (Ang)      (Ang)      (Ang)      (Ang)      (Ang)     '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        ind = 0
        do i = 1,nEDIPspec
          nat1 = natEDIPspec(i)
          ntype1 = ntypEDIPspec(i)
          call label(nat1,ntype1,lab1)
          cstyp1 = 'c'
          if (nat1.gt.maxele) cstyp1 = 's'
          do j = 1,i
            ind = ind + 1
            if (lEDIPpairOK(ind)) then
              nat2 = natEDIPspec(j)
              ntype2 = ntypEDIPspec(j)
              call label(nat2,ntype2,lab2)
              cstyp2 = 'c'
              if (nat2.gt.maxele) cstyp2 = 's'
              write(ioout,'(2(a5,a1,1x),f10.5,5(1x,f10.5))') &
                    lab1,cstyp1,lab2,cstyp2,EDIP2epsilon(ind),EDIP2B(ind),EDIP2beta(ind), &
                    EDIP2sigma(ind),EDIP2a(ind),EDIP2aprime(ind)
            endif
          enddo
        enddo
        write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Threebody energy
!
        write(ioout,'(/,''  EDIP Threebody Energy :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Types                Lambda0/          Lambda_prime/       Z0/            '')')
        write(ioout,'(''  1     2     3            Gamma             Gamma_prime         q              '')')
        write(ioout,'(''                                             K_q2                               '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        do i = 1,nEDIPspec
          nat1 = natEDIPspec(i)
          ntype1 = ntypEDIPspec(i)
          call label(nat1,ntype1,lab1)
          cstyp1 = 'c'
          if (nat1.gt.maxele) cstyp1 = 's'
          ind = 0
          do j = 1,nEDIPspec
            nat2 = natEDIPspec(j)
            ntype2 = ntypEDIPspec(j)
            call label(nat2,ntype2,lab2)
            cstyp2 = 'c'
            if (nat2.gt.maxele) cstyp2 = 's'
            do k = 1,j
              ind = ind + 1
              if (lEDIPtriadOK(ind,i)) then
                nat3 = natEDIPspec(k)
                ntype3 = ntypEDIPspec(k)
                call label(nat3,ntype3,lab3)
                cstyp3 = 'c'
                if (nat3.gt.maxele) cstyp3 = 's'
                write(ioout,'(3(a5,a1,1x),f15.6,2(2x,f15.6))') &
                      lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,EDIP3lambda0(ind,i),EDIP3lambdap(ind,i),EDIP3Z0(ind,i)
                write(ioout,'(21x,f15.6,2(2x,f15.6))') EDIP3gamma0(ind,i),EDIP3gammap(ind,i),EDIP3q(ind,i)
                write(ioout,'(36x,2x,f15.6)') EDIP3kq2(ind,i)
              endif
            enddo
          enddo
        enddo
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    endif
!
!  One-body
!
    if (none.gt.0) then
      write(ioout,'(/,''  One-body self-energy potentials :'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom Type    Potential         Energy (eV) '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,none
        nat1 = nspec11(i)
        ntype1 = nptyp11(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,6x,''Ashift'',4x,f20.8)') lab1,cstyp1,onepot(i)
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'')')
    endif
!
!  Two-body
!
    lwarnpn = .false.
    do k = 1,3
      if (nmpt(k).gt.0) then
        if (k.eq.1) then
          write(ioout,'(/,''  General interatomic potentials :'',/)')
        elseif (k.eq.2) then
          write(ioout,'(/,''  Intramolecular potentials :'',/)')
        else
          write(ioout,'(/,''  Intermolecular potentials :'',/)')
        endif
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Types   Potential         A         B         C         D     Cutoffs(Ang)'')')
        write(ioout,'(''  1     2                                                            Min    Max '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        do i = 1,npote
          if (iptyp(i).eq.k) then
            np = nptype(i)
            nat1 = nspec1(i)
            nat2 = nspec2(i)
            ntype1 = nptyp1(i)
            ntype2 = nptyp2(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            cstyp1 = 'c'
            cstyp2 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (np.ne.23.and.np.ne.18.and.np.ne.35.and.np.ne.38) then
!
!  Non-polynomial type potential
!
              if (np.eq.7.or.np.eq.2.or.np.eq.12.or.np.eq.13.or.np.eq.21) then
                mpt = int(tpot(1,i))
                npt = int(tpot(2,i))
                if (np.eq.12.or.np.eq.13) then
                  cpoti = 0.0_dp
                  dpoti = 0.0_dp
                else
                  cpoti = twopot(3,i)
                  dpoti = twopot(4,i)
                endif
                if (mmexc(i).eq.0) then
                  write(ioout,'(2(a5,a1,1x),a7,2i3,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,f5.3,1x,f6.3)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),mpt,npt,twopot(1,i),twopot(2,i),cpoti,dpoti,rpot2(i),rpot(i)
                elseif (mmexc(i).eq.1) then
                  write(ioout,'(2(a5,a1,1x),a7,2i3,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,f5.3,1x,''1 Bond'')') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),mpt,npt,twopot(1,i),twopot(2,i),cpoti,dpoti,rpot2(i)
                  if (k.eq.3) lwarnpn = .true.
                elseif (mmexc(i).eq.2) then
                  write(ioout,'(2(a5,a1,1x),a7,2i3,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''2Bond'',1x,f6.3)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),mpt,npt,twopot(1,i),twopot(2,i),cpoti,dpoti,rpot(i)
                elseif (mmexc(i).eq.3) then
                  write(ioout,'(2(a5,a1,1x),a7,2i3,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''3Bond'',1x,f6.3)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),mpt,npt,twopot(1,i),twopot(2,i),cpoti,dpoti,rpot(i)
                elseif (mmexc(i).eq.4) then
                  write(ioout,'(2(a5,a1,1x),a7,2i3,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''1-4only'',f5.2)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),mpt,npt,twopot(1,i),twopot(2,i),cpoti,dpoti,rpot(i)
                elseif (mmexc(i).eq.5) then
                  write(ioout,'(2(a5,a1,1x),a7,2i3,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''1-4gtr '',f5.2)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),mpt,npt,twopot(1,i),twopot(2,i),cpoti,dpoti,rpot(i)
                endif
              else
                if (mmexc(i).eq.0) then
                  write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,f5.3,1x,f6.3)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),twopot(3,i),twopot(4,i),rpot2(i),rpot(i)
                elseif (mmexc(i).eq.1) then
                  write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,f5.3,1x,''1 Bond'')') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),twopot(3,i),twopot(4,i),rpot2(i)
                  if (k.eq.3) lwarnpn = .true.
                elseif (mmexc(i).eq.2) then
                  write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''2Bond'',1x,f6.3)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),twopot(3,i),twopot(4,i),rpot(i)
                elseif (mmexc(i).eq.3) then
                  write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''3Bond'',1x,f6.3)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),twopot(3,i),twopot(4,i),rpot(i)
                elseif (mmexc(i).eq.4) then
                  write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''1-4only'',f5.2)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),twopot(3,i),twopot(4,i),rpot(i)
                elseif (mmexc(i).eq.5) then
                  write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),g9.3,1x,g9.3,1x,''1-4gtr '',f5.2)') &
                    lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),twopot(3,i),twopot(4,i),rpot(i)
                endif
              endif
            elseif (np.eq.18) then
!
!  Damped dispersion potential
!
              if (mmexc(i).eq.0) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.3,1x),1x,f6.3,1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),tpot(1,i),0.0_dp,rpot2(i),rpot(i)
                write(ioout,'(28x,2(g9.3,1x),g8.3)') twopot(3,i),twopot(4,i),tpot(2,i)
              elseif (mmexc(i).eq.1) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.3,1x),1x,f6.3,1x,''1 Bond'')') &
                  lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),tpot(1,i),0.0_dp,rpot2(i)
                write(ioout,'(28x,2(g9.3,1x),g8.3)') twopot(3,i),twopot(4,i),tpot(2,i)
                if (k.eq.3) lwarnpn = .true.
              elseif (mmexc(i).eq.2) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.3,1x),1x,''2 Bond'',1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),tpot(1,i),0.0_dp,rpot(i)
                write(ioout,'(28x,2(g9.3,1x),g8.3)') twopot(3,i),twopot(4,i),tpot(2,i)
              elseif (mmexc(i).eq.3) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.3,1x),1x,''3 Bond'',1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),tpot(1,i),0.0_dp,rpot(i)
                write(ioout,'(28x,2(g9.3,1x),g8.3)') twopot(3,i),twopot(4,i),tpot(2,i)
              elseif (mmexc(i).eq.4) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.3,1x),1x,''1-4 only'',f5.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),tpot(1,i),0.0_dp,rpot(i)
                write(ioout,'(28x,2(g9.3,1x),g8.3)') twopot(3,i),twopot(4,i),tpot(2,i)
              elseif (mmexc(i).eq.5) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.3,1x),1x,''1-4 gtr'',f5.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(np),twopot(1,i),twopot(2,i),tpot(1,i),0.0_dp,rpot(i)
                write(ioout,'(28x,2(g9.3,1x),g8.3)') twopot(3,i),twopot(4,i),tpot(2,i)
              endif
            elseif (np.eq.35) then
!                     
!  Polynomial harmonic potential
!                     
              norder = nint(twopot(4,i))
              if (mmexc(i).eq.0) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),2(1x,f6.3))') &
                  lab1,cstyp1,lab2,cstyp2,namepot(35),twopot(1,i),tpot(1,i),tpot(2,i),tpot(3,i),rpot2(i),rpot(i)
              elseif (mmexc(i).eq.1) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),1x,f6.3,1x,''1 Bond'')') &
                  lab1,cstyp1,lab2,cstyp2,namepot(35),twopot(1,i),tpot(1,i),tpot(2,i),tpot(3,i),rpot2(i)
                  if (k.eq.3) lwarnpn = .true.
              elseif (mmexc(i).eq.2) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),1x,''2 Bond'',1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(35),twopot(1,i),tpot(1,i),tpot(2,i),tpot(3,i),rpot(i)
              elseif (mmexc(i).eq.3) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),1x,''3 Bond'',1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(35),twopot(1,i),tpot(1,i),tpot(2,i),tpot(3,i),rpot(i)
              endif
              if (norder.gt.1) then
                write(ioout,'(26x,3f11.4)') tpot(4,i),tpot(5,i),tpot(6,i)
              endif
            elseif (np.eq.38) then
!                     
!  Short-range Glue
!                     
              if (mmexc(i).eq.0) then
                write(ioout,'(2(a5,a1,1x),a13,1x,f9.6,1x,28x,2(1x,f6.3))') &
                  lab1,cstyp1,lab2,cstyp2,namepot(38),twopot(1,i),rpot2(i),rpot(i)
              elseif (mmexc(i).eq.1) then
                write(ioout,'(2(a5,a1,1x),a13,1x,f9.6,37x,''1 Bond'')') &
                  lab1,cstyp1,lab2,cstyp2,namepot(38),twopot(1,i)
              elseif (mmexc(i).eq.2) then
                write(ioout,'(2(a5,a1,1x),a13,1x,f9.6,37x,''2 Bond'')') &
                  lab1,cstyp1,lab2,cstyp2,namepot(38),twopot(1,i)
              elseif (mmexc(i).eq.3) then
                write(ioout,'(2(a5,a1,1x),a13,1x,f9.6,37x,''3 Bond'')') &
                  lab1,cstyp1,lab2,cstyp2,namepot(38),twopot(1,i)
              endif
              write(ioout,'(4x,5(f10.5,1x))') (tpot(j,i),j=1,5)
              write(ioout,'(4x,7(f10.5,1x))') (tpot(j,i),j=6,12)
            elseif (np.eq.23) then
!
!  Polynomial potential
!
              norder = nint(twopot(4,i))
              if (mmexc(i).eq.0) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),1x,f6.3,1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(23),tpot(1,i),tpot(2,i),tpot(3,i),tpot(4,i),rpot2(i),rpot(i)
              elseif (mmexc(i).eq.1) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),1x,f6.3,1x,''1 Bond'')') &
                  lab1,cstyp1,lab2,cstyp2,namepot(23),tpot(1,i),tpot(2,i),tpot(3,i),tpot(4,i),rpot2(i)
                  if (k.eq.3) lwarnpn = .true.
              elseif (mmexc(i).eq.2) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),1x,''2 Bond'',1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(23),tpot(1,i),tpot(2,i),tpot(3,i),tpot(4,i),rpot(i)
              elseif (mmexc(i).eq.3) then
                write(ioout,'(2(a5,a1,1x),a13,1x,2(g9.3,1x),2(g8.2,1x),1x,''3 Bond'',1x,f6.3)') &
                  lab1,cstyp1,lab2,cstyp2,namepot(23),tpot(1,i),tpot(2,i),tpot(3,i),tpot(4,i),rpot(i)
              endif
              if (norder.gt.3) then
                write(ioout,'(28x,2(g9.3,1x),2(g8.2,1x))') tpot(5,i),tpot(6,i),tpot(7,i),tpot(8,i)
              endif
              if (norder.gt.7) then
                write(ioout,'(28x,g9.3)') tpot(9,i)
              endif
            endif
!
!  Bond types
!
            if (n2botype(1,i).ne.0.or.n2botype(2,i).ne.0) then
              write(ioout,'(68x,a9,a3)') botyword(n2botype(1,i)+1),botyword2(n2botype(2,i)+1)
            endif
          endif
        enddo
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    enddo
    deallocate(iptyp,stat=status)
    if (status/=0) call deallocate_error('outpot','iptyp')
    if (lwarnpn) then
!
!  Intermolecular potential has been specified as being bonded only
!
      nwarn = nwarn + 1
      call outwarning('Intermolecular potential is specified as bonded potential',0_i4)
    endif
  endif
!**************************************************
!  Output potentials with energy/gradient shifts  *
!**************************************************
  ntmp = 0
  allocate(ntemp(npote),stat=status)
  if (status/=0) call outofmemory('outpot','ntemp')
  do i = 1,npote
    if (leshift(i).and.(nptype(i).ne.10.and.nptype(i).ne.23).and.nptype(i).lt.100) then
      ntmp = ntmp + 1
      ntemp(ntmp) = i
    endif
  enddo
  if (ntmp.gt.0) then
    write(ioout,'(/,''  Potentials with energy / gradient boundary corrections : '',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''    Potential       Atom1   Atom2    Type of correction'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,ntmp
      npi = ntemp(i)
      np = nptype(npi)
      nat1 = nspec1(npi)
      nat2 = nspec2(npi)
      ntype1 = nptyp1(npi)
      ntype2 = nptyp2(npi)
      call label(nat1,ntype1,lab1)
      call label(nat2,ntype2,lab2)
      cstyp1 = 'c'
      cstyp2 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
      if (nat2.gt.maxele) cstyp2 = 's'
      if (np.eq.7) then
        mpt = int(tpot(1,npi))
        npt = int(tpot(2,npi))
      endif
      if (tpot(5,npi).ne.0.0_dp.and.np.ne.10) then
        if (np.eq.7) then
          write(ioout,'(4x,a7,2i3,3x,a5,1x,a1,1x,a5,1x,a1,2x,''Voter'')') &
            namepot(7),mpt,npt,lab1,cstyp1,lab2,cstyp2
        else
          write(ioout,'(4x,a13,3x,a5,1x,a1,1x,a5,1x,a1,2x,''Voter'')') &
            namepot(np),lab1,cstyp1,lab2,cstyp2
        endif
      elseif (leshift(npi).and.lgshift(npi).and.np.ne.10) then
        if (np.eq.7) then
          write(ioout,'(4x,a7,2i3,3x,a5,1x,a1,1x,a5,1x,a1,2x,''Gradient'')') &
            namepot(7),mpt,npt,lab1,cstyp1,lab2,cstyp2
        else
          write(ioout,'(4x,a13,3x,a5,1x,a1,1x,a5,1x,a1,2x,''Gradient'')') &
            namepot(np),lab1,cstyp1,lab2,cstyp2
        endif
      else
        if (np.eq.7) then
          write(ioout,'(4x,a7,2i3,3x,a5,1x,a1,1x,a5,1x,a1,2x,''Energy'')') &
            namepot(7),mpt,npt,lab1,cstyp1,lab2,cstyp2
        else
          write(ioout,'(4x,a13,3x,a5,1x,a1,1x,a5,1x,a1,2x,''Energy'')') &
            namepot(np),lab1,cstyp1,lab2,cstyp2
        endif
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  deallocate(ntemp,stat=status)
  if (status/=0) call deallocate_error('outpot','ntemp')
!******************
!  EAM densities  *
!******************
  if (neamspec.gt.0) then
    lout = .true.
    do i = 1,neamspec
      nat1 = neamnat(i)
      ntype1 = neamtyp(i)
      call label(nat1,ntype1,lab1)
      cstyp1 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
      nat2 = neamnat2(i)
      if (nat2.gt.0) then
        ntype2 = neamtyp2(i)
        call label(nat2,ntype2,lab2)
        cstyp2 = 'c'
        if (nat2.gt.maxele) cstyp1 = 's'
      endif
      do j = 1,ndenfncomp(i)
        if (neammeamorder(j,i).eq.1) then
          if (lout) then
            write(ioout,'(/,''  Embedded Atom Model Densities :'',/)')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
            write(ioout,'(''  Atom(s)       Functional Form          A          B          C         n      '')')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
            lout = .false.
          endif
          if (ndenfn(j,i).gt.0) then
            if (ndenfn(j,i).eq.1) then
              denfntyp = 'Power Law   '
            elseif (ndenfn(j,i).eq.2) then
              denfntyp = 'Exponential '
            elseif (ndenfn(j,i).eq.3) then
              denfntyp = 'Gaussian    '
            elseif (ndenfn(j,i).eq.4) then
              denfntyp = 'Cubic       '
            elseif (ndenfn(j,i).eq.5) then
              denfntyp = 'Voter-Chen  '
            elseif (ndenfn(j,i).eq.6) then
              denfntyp = 'Quadratic   '
            elseif (ndenfn(j,i).eq.7) then
              denfntyp = 'Quartic     '
            elseif (ndenfn(j,i).eq.8) then
              denfntyp = 'Glue        '
            elseif (ndenfn(j,i).eq.9) then
              denfntyp = 'eVoter-Chen '
            elseif (ndenfn(j,i).eq.10) then
              denfntyp = 'Mei-Davenprt'
            elseif (ndenfn(j,i).eq.12) then
              denfntyp = 'Baskes      '
            elseif (ndenfn(j,i).eq.13) then
              denfntyp = 'VBO         '
            elseif (ndenfn(j,i).eq.14) then
              denfntyp = 'Fractl power'
            endif
            if (ndenfn(j,i).eq.8) then
              if (nat2.gt.0) then
                write(ioout,'(2x,a4,1x,a1,1x,a4,1x,a1,1x,a12,4x,g14.4,2(1x,g10.4))') &
                  lab1(1:4),cstyp1,lab2(1:4),cstyp2,denfntyp,(denpar(k,1,j,i),k=1,3)
              else
                write(ioout,'(2x,a4,1x,a1,8x,a12,4x,g14.4,2(1x,g10.4))') &
                  lab1(1:4),cstyp1,denfntyp,(denpar(k,1,j,i),k=1,3)
              endif
              write(ioout,'(16x,4(f12.6,1x))') (denpar(k,1,j,i),k=4,7)
              write(ioout,'(16x,4(f12.6,1x))') (denpar(k,1,j,i),k=8,11)
              write(ioout,'(16x,4(f12.6,1x))') (denpar(k,1,j,i),k=12,15)
            elseif (ndenfn(j,i).eq.10) then
              if (nat2.gt.0) then
                write(ioout,'(2x,a4,1x,a1,1x,a4,1x,a1,1x,a12,6x,3(f11.6,1x))') &
                  lab1(1:4),cstyp1,lab2(1:4),cstyp2,denfntyp,(denpar(k,1,j,i),k=1,3)
              else
                write(ioout,'(2x,a4,1x,a1,8x,a12,6x,3(f11.6,1x))') &
                  lab1(1:4),cstyp1,denfntyp,(denpar(k,1,j,i),k=1,3)
              endif
              write(ioout,'(34x,3(f11.6,1x),f8.4)') (denpar(k,1,j,i),k=4,7)
            elseif (ndenfn(j,i).eq.13) then
              if (nat2.gt.0) then
                write(ioout,'(2x,a4,1x,a1,1x,a4,1x,a1,1x,a12,6x,3(f11.6,1x))') &
                  lab1(1:4),cstyp1,lab2(1:4),cstyp2,denfntyp,(denpar(k,1,j,i),k=1,3)
              else
                write(ioout,'(2x,a4,1x,a1,8x,a12,6x,3(f11.6,1x))') &
                  lab1(1:4),cstyp1,denfntyp,(denpar(k,1,j,i),k=1,3)
              endif
              write(ioout,'(34x,2(f11.6,1x))') (denpar(k,1,j,i),k=4,5)
            else
              if (nat2.gt.0) then
                write(ioout,'(2x,a4,1x,a1,1x,a4,1x,a1,1x,a12,4x,g14.4,2(1x,g10.4),4x,i2)') &
                  lab1(1:4),cstyp1,lab2(1:4),cstyp2,denfntyp,(denpar(k,1,j,i),k=1,3),nint(denpar(6,1,j,i))
              else
                write(ioout,'(2x,a4,1x,a1,8x,a12,4x,g14.4,2(1x,g10.4),4x,i2)') &
                  lab1(1:4),cstyp1,denfntyp,(denpar(k,1,j,i),k=1,3),nint(denpar(6,1,j,i))
              endif
            endif
          endif
        endif
      enddo
    enddo
    if (.not.lout) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
!*******************
!  MEAM densities  *
!*******************
    lout = .true.
    do i = 1,neamspec
      nat1 = neamnat(i)
      ntype1 = neamtyp(i)
      call label(nat1,ntype1,lab1)
      cstyp1 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
      nat2 = neamnat2(i)
      if (nat2.gt.0) then
        ntype2 = neamtyp2(i)
        call label(nat2,ntype2,lab2)
        cstyp2 = 'c'
        if (nat2.gt.maxele) cstyp1 = 's'
      endif
      do j = 1,ndenfncomp(i)
        if (neammeamorder(j,i).gt.1) then
          if (lout) then
            write(ioout,'(/,''  Modified Embedded Atom Model Densities :'',/)')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
            write(ioout,'(''  Atom(s)       Functional/Order        A          B          C         n      '')')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
            lout = .false.
          endif
          if (ndenfn(j,i).gt.0) then
            if (ndenfn(j,i).eq.1) then
              denfntyp = 'Power Law   '
            elseif (ndenfn(j,i).eq.2) then
              denfntyp = 'Exponential '
            elseif (ndenfn(j,i).eq.3) then
              denfntyp = 'Gaussian    '
            elseif (ndenfn(j,i).eq.4) then
              denfntyp = 'Cubic       '
            elseif (ndenfn(j,i).eq.5) then
              denfntyp = 'Voter-Chen  '
            elseif (ndenfn(j,i).eq.6) then
              denfntyp = 'Quadratic   '
            elseif (ndenfn(j,i).eq.7) then
              denfntyp = 'Quartic     '
            elseif (ndenfn(j,i).eq.8) then
              denfntyp = 'Glue        '
            elseif (ndenfn(j,i).eq.9) then
              denfntyp = 'eVoter-Chen '
            elseif (ndenfn(j,i).eq.10) then
              denfntyp = 'Mei-Davenprt'
            elseif (ndenfn(j,i).eq.12) then
              denfntyp = 'Baskes      '
            elseif (ndenfn(j,i).eq.14) then
              denfntyp = 'Fractl power'
            endif
            if (ndenfn(j,i).eq.8) then
              if (nat2.gt.0) then
                write(ioout,'(2x,a4,1x,a1,1x,a4,1x,a1,1x,a12,1x,''0'',2x,g14.4,2(1x,g10.4))') &
                  lab1(1:4),cstyp1,lab2(1:4),cstyp2,denfntyp,(denpar(k,1,j,i),k=1,3)
              else
                write(ioout,'(2x,a4,1x,a1,8x,a12,1x,''0'',2x,g14.4,2(1x,g10.4))') &
                  lab1(1:4),cstyp1,denfntyp,(denpar(k,1,j,i),k=1,3)
              endif
              write(ioout,'(16x,4(f12.6,1x))') (denpar(k,1,j,i),k=4,7)
              write(ioout,'(16x,4(f12.6,1x))') (denpar(k,1,j,i),k=8,11)
              write(ioout,'(16x,4(f12.6,1x))') (denpar(k,1,j,i),k=12,15)
            elseif (ndenfn(j,i).eq.10) then
              if (nat2.gt.0) then
                write(ioout,'(2x,a4,1x,a1,1x,a4,1x,a1,1x,a12,1x,''0'',4x,3(f11.6,1x))') &
                  lab1(1:4),cstyp1,lab2(1:4),cstyp2,denfntyp,(denpar(k,1,j,i),k=1,3)
              else
                write(ioout,'(2x,a4,1x,a1,8x,a12,1x,''0'',4x,3(f11.6,1x))') &
                  lab1(1:4),cstyp1,denfntyp,(denpar(k,1,j,i),k=1,3)
              endif
              write(ioout,'(34x,3(f11.6,1x),f8.4)') (denpar(k,1,j,i),k=4,7)
            else
              if (nat2.gt.0) then
                write(ioout,'(2x,a4,1x,a1,1x,a4,1x,a1,1x,a12,1x,''0'',2x,g14.4,2(1x,g10.4),4x,i2)') &
                  lab1(1:4),cstyp1,lab2(1:4),cstyp2,denfntyp,(denpar(k,1,j,i),k=1,3),nint(denpar(6,1,j,i))
              else
                write(ioout,'(2x,a4,1x,a1,8x,a12,1x,''0'',2x,g14.4,2(1x,g10.4),4x,i2)') &
                  lab1(1:4),cstyp1,denfntyp,(denpar(k,1,j,i),k=1,3),nint(denpar(6,1,j,i))
              endif
            endif
            do order = 2,neammeamorder(j,i)
              if (ndenfn(j,i).eq.8) then
                write(ioout,'(29x,i1,2x,g14.4,2(1x,g10.4))') order-1,(denpar(k,order,j,i),k=1,3)
                write(ioout,'(16x,4(f12.6,1x))') (denpar(k,order,j,i),k=4,7)
                write(ioout,'(16x,4(f12.6,1x))') (denpar(k,order,j,i),k=8,11)
                write(ioout,'(16x,4(f12.6,1x))') (denpar(k,order,j,i),k=12,15)
              elseif (ndenfn(j,i).eq.10) then
                write(ioout,'(29x,i1,4x,3(f11.6,1x))') order-1,(denpar(k,order,j,i),k=1,3)
                write(ioout,'(34x,3(f11.6,1x),f8.4)') (denpar(k,order,j,i),k=4,7)
              else
                write(ioout,'(29x,i1,2x,g14.4,2(1x,g10.4),4x,i2)') order-1,(denpar(k,order,j,i),k=1,3),nint(denpar(6,order,j,i))
              endif
            enddo
          endif
        endif
      enddo
    enddo
    if (.not.lout) then
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
!**********************
!  EAM alloy factors  *
!**********************
    write(ioout,'(/,''  Embedded Atom Model alloy parameters :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Atom(s)                          Scale factor '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,neamspec
      nat1 = neamnat(i)
      ntype1 = neamtyp(i)
      call label(nat1,ntype1,lab1)
      cstyp1 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'   
      nat2 = neamnat2(i)
      if (nat2.gt.0) then
        ntype2 = neamtyp2(i)
        call label(nat2,ntype2,lab2)
        cstyp2 = 'c'
        if (nat2.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,4x,a5,1x,a1,4x,f21.6)') lab1,cstyp1,lab2,cstyp2,eamalloy(1,i)
      else
        write(ioout,'(2x,a5,1x,a1,15x,f21.6)') lab1,cstyp1,eamalloy(1,i)
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
!********************
!  EAM functionals  *
!********************
    if (neamfn.eq.1) then
      write(ioout,'(''  Embedded Atom Model functional = Square Root'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                     A     '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,9x,f14.6)') lab1,cstyp1,eamfnpar(1,i)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.2) then
      write(ioout,'(''  Embedded Atom Model functional = Inverse Power Law of '',i2,/)') neampower
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                     A     '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,9x,f14.6)') lab1,cstyp1,eamfnpar(1,i)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.3) then
      write(ioout,'(''  Embedded Atom Model functional = Banerjea/Smith : Power = '',i2,/)') neampower
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                     F0         F1        rho0               '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,9x,g14.4,2(1x,g10.4))') lab1,cstyp1,(eamfnpar(j,i),j=1,3)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.4) then
      write(ioout,'(''  Embedded Atom Model functional = Numerical '',/)') 
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom    Filename '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,1x,a70)') lab1,cstyp1,eamfnfile(i)(1:70)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.5) then
      write(ioout,'(''  Embedded Atom Model functional = Johnson '',/)') 
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                  F0         F1        rho0       Alpha    Beta    Gamma  '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,9x,g14.4,2(1x,g10.4),3(1x,f7.5))') lab1,cstyp1,(eamfnpar(j,i),j=1,6)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.6) then
      write(ioout,'(''  Embedded Atom Model functional = Glue '',/)') 
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                     rho1         rho2        '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,12x,2(1x,f12.6))') lab1,cstyp1,(eamfnpar(j,i),j=1,2)
        write(ioout,'(2x,5(f12.8,1x))') (eamfnpar(j,i),j=3,7)
        write(ioout,'(2x,5(f12.8,1x))') (eamfnpar(j,i),j=8,12)
        write(ioout,'(2x,4(f12.8,1x))') (eamfnpar(j,i),j=13,16)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.7) then
      write(ioout,'(''  Embedded Atom Model functional = Foiles '',/)') 
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                  F0         F1        F2       F3                   '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,9x,g14.4,2(1x,g10.4),1x,f7.5)') lab1,cstyp1,(eamfnpar(j,i),j=1,4)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.8) then
      write(ioout,'(''  Embedded Atom Model functional = Mei-Davenport '',/)') 
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                 Ec          Alpha      Beta     Gamma     Delta       '')')
      write(ioout,'(''                      (eV)          phi0       s1       s2        s3         '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,6x,f14.6,2x,4(1x,f9.5))') lab1,cstyp1,(eamfnpar(j,i),j=1,5)
        write(ioout,'(31x,4(1x,f9.5))') (eamfnpar(j,i),j=6,9)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.9) then
      write(ioout,'(''  Embedded Atom Model functional = Baskes'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                     A         rho0'')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,9x,f14.6,1x,g14.6)') lab1,cstyp1,eamfnpar(1,i),eamfnpar(2,i)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.10) then
      write(ioout,'(''  Embedded Atom Model functional = VBO'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom                     A         rn'')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,9x,f14.6,1x,g10.4)') lab1,cstyp1,eamfnpar(1,i),eamfnpar(2,i)
        if (neamfnmeamorder(i).gt.1) then
          if (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = sum : 21 term'')')
          elseif (neamfnmeamcombotype(i).eq.1.and.neamfnmeamtype(i).eq.2) then
            write(ioout,'(10x,''MEAM type = sum : 24 term'')')
          elseif (neamfnmeamcombotype(i).eq.2.and.neamfnmeamtype(i).eq.1) then
            write(ioout,'(10x,''MEAM type = exponential : 21 term'')')
          else
            write(ioout,'(10x,''MEAM type = exponential : 24 term'')')
          endif
          write(ioout,'(10x,''MEAM order/coefficients = '',i2,4(1x,f8.4))') &
            neamfnmeamorder(i),(eamfnmeamcoeff(j,i),j=1,neamfnmeamorder(i))
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    elseif (neamfn.eq.11) then
      write(ioout,'(''  Embedded Atom Model functional = Belashchenko '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Atom      Rho1         Rho2          Rho3        '')')
      write(ioout,'(''            Rho4         Rho5          Rho6         Rho7 '')')
      write(ioout,'(''            a1           m             n    '')')
      write(ioout,'(''            c1           c2            c3           c4   '')')
      write(ioout,'(''            c5           c6            c7           c8   '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,neamfnspec
        nat1 = neamfnnat(i)
        ntype1 = neamfntyp(i)
        call label(nat1,ntype1,lab1)
        cstyp1 = 'c'
        if (nat1.gt.maxele) cstyp1 = 's'
        write(ioout,'(2x,a5,1x,a1,3(1x,f12.6))') lab1,cstyp1,(eamfnpar(j,i),j=1,3)
        write(ioout,'(9x,4(1x,f12.6))') (eamfnpar(j,i),j=4,7)
        write(ioout,'(10x,3(f12.8,1x))') eamfnpar(8,i),eamfnpar(32,i),eamfnpar(33,i)
        write(ioout,'(10x,4(f12.8,1x))') (eamfnpar(j,i),j=24,27)
        write(ioout,'(10x,4(f12.8,1x))') (eamfnpar(j,i),j=28,31)
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
  endif
!**************************
!  Shift operator output  *
!**************************
  if (nshift.gt.0) then
    write(ioout,'(/,''  Energy shifts for configurations :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''      Configuration           Energy (eV)        Scale Factor'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i=1,ncfg
      write(ioout,'(11x,i3,15x,g12.6,10x,f8.3)')i,shift(nshcfg(i)),shscalecfg(i)
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!***********************************
!  Output three-bodied potentials  *
!***********************************
  if (nthb.gt.0) then
!
!  Count number of general, intra- and inter-molecular potentials
!
    nmpt(1) = 0
    nmpt(2) = 0
    nmpt(3) = 0
    allocate(iptyp(nthb),stat=status)
    if (status/=0) call outofmemory('outpot','iptyp')
    if (lmol) then
      do i = 1,nthb
        if (ltintra(i).and..not.ltinter(i)) then
          nmpt(2) = nmpt(2) + 1
          iptyp(i) = 2
        elseif (ltinter(i).and..not.ltintra(i)) then
          nmpt(3) = nmpt(3) + 1
          iptyp(i) = 3
        else
          nmpt(1) = nmpt(1) + 1
          iptyp(i) = 1
        endif
      enddo
    else
      nmpt(1) = nthb
      do i = 1,nthb
        iptyp(i) = 1
      enddo
    endif
!
!  Standard three-body potentials
!
    do k = 1,3
      if (nmpt(k).gt.0) then
        if (k.eq.1) then
          write(ioout,'(/,''  General Three-body potentials :'')')
        elseif (k.eq.2) then
          write(ioout,'(/,''  Intramolecular Three-body potentials :'')')
        elseif (k.eq.3) then
          write(ioout,'(/,''  Intermolecular Three-body potentials :'')')
        endif
        lout = .true.
        do i = 1,nthb
          if (nthrty(i).eq.1.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Harmonic form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''Atom    Atom    Atom           Force Constants            Theta     Cutoffs  '')')
              write(ioout,'(''  1       2       3     (eVrad**-2/eVrad**-3/eVrad**-4)   (deg)  1-2  1-3  2-3'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            the = theta(i)
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,2(1x,a5,1x,a1),1x,g10.4,2(1x,f9.4),3x,f7.3,5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thrho2(i),thrho1(i),the
            else
              write(ioout,'(a5,1x,a1,2(1x,a5,1x,a1),1x,g10.4,2(1x,f9.4),3x,f7.3,3(1x,f4.2))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thrho2(i),thrho1(i),the,thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(64x,3(1x,f4.2))')thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Exponentially decaying three-body potentials
!
        do i = 1,nthb
          if (nthrty(i).eq.2.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Harmonic + exponential form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types         Force Cst   Theta   Rho1   Rho2         Cutoffs  '')')
              write(ioout,'(''   1       2       3     (eVrad**-2)  (deg)  (Angs) (Angs)   1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout=.false.
            endif
            the = theta(i)
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.5,2x,f7.3,2(1x,f6.4),9x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thrho2(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.5,2x,f7.3,2(1x,f6.4),3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thrho2(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))')thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Axilrod-Teller
!
        do i = 1,nthb
          if (nthrty(i).eq.3.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Axilrod-Teller form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''Atom Type  Atom Type  Atom Type  Force Const       Cutoff   Cutoff   Cutoff'')')
              write(ioout,'(''    1          2          3      (eV*Angs**9)       1-2      1-3      2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,4x,g11.6,5x,5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i)
            else
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,4x,g11.6,5x,3(2x,f7.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(49x,3(2x,f7.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Exponentially decaying three-body potential with no theta component
!
        do i = 1,nthb
          if (nthrty(i).eq.4.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Exponential three-body form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types         Force Cst   Rho1   Rho2   Rho3         Cutoffs  '')')
              write(ioout,'(''   1       2       3     (eVrad**-2)  (A-1)  (A-1)  (A-1)   1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.5,2x,5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),thrho1(i),thrho2(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.5,2x,3(1x,f6.4),3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),thrho1(i),thrho2(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Stillinger-Weber 3-body form
!
        do i = 1,nthb
          if (nthrty(i).eq.5.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Stillinger-Weber form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types        Force Cst    Theta      Rhos           Cutoffs  '')')
              write(ioout,'(''   1       2       3        (eV)                (Angs)     1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            the = theta(i)
            write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,g11.4,1x,f8.3,2(1x,f5.3),3(1x,f6.3))') &
              lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thrho2(i),thr1min(i),thr2min(i),thr3min(i)
            write(ioout,'(56x,3(1x,f6.3))') thr1(i),thr2(i),thr3(i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Bcross potential
!
        do i = 1,nthb
          if (nthrty(i).eq.6.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Bond-bond cross term potential:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types         Force Cst       Rho1       Rho2         Cutoffs  '')')
              write(ioout,'(''   1       2       3     (eVrad**-2)     (Angs)     (Angs)   1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.4,6x,f6.4,5x,f6.4,8x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thrho1(i),thrho2(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.5,6x,f6.4,5x,f6.4,3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thrho1(i),thrho2(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Urey-Bradley potential
!
        do i = 1,nthb
          if (nthrty(i).eq.7.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Urey-Bradley potential:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types         Force Constant          R0              Cutoffs  '')')
              write(ioout,'(''   1       2       3     (eVrad**-2)            (Angs)       1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g12.5,9x,f8.5,4x,8x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g12.5,9x,f8.5,4x,3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Exponentially decaying three-body potentials - Vessal form
!
        do i = 1,nthb
          if (nthrty(i).eq.8.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Vessal exponential form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types         Force Cst   Theta   Rho1   Rho2         Cutoffs  '')')
              write(ioout,'(''   1       2       3     (eVrad**-2)  (deg)  (Angs) (Angs)   1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            the = theta(i)
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.5,2x,f7.3,2(1x,f6.4),5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thrho2(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.5,2x,f7.3,2(1x,f6.4),3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thrho2(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Cosine-harmonic potential
!
        lout = .true.
        do i = 1,nthb
          if (nthrty(i).eq.9.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Cosine Harmonic form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''Atom    Atom    Atom           Force Constants            Theta     Cutoffs  '')')
              write(ioout,'(''  1       2       3               (eV)                    (deg)  1-2  1-3  2-3'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            the = theta(i)
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,2(1x,a5,1x,a1),1x,g30.8,3x,f7.3,5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the
            else
              write(ioout,'(a5,1x,a1,2(1x,a5,1x,a1),1x,g30.8,3x,f7.3,3(1x,f4.2))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(64x,3(1x,f4.2))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Murrell-Mottram 3-body form
!
        do i = 1,nthb
          if (nthrty(i).eq.10.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Murrell-Mottram form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types       Force Cst    Rho         R0           Cutoffs  '')')
              write(ioout,'(''   1       2       3   (eVrad**-2) (Angs-1)    (Angs)       1-2   1-3   2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            the = theta(i)
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,g11.4,1x,f6.4,1x,3(f4.2,1x),7x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thrho2(i),thrho3(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,g11.4,1x,f6.4,1x,3(f4.2,1x),3(1x,f5.2))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thrho2(i),thrho3(i), &
                thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f5.2))') thr1(i),thr2(i),thr3(i)
            endif
            write(ioout,'(24x,g11.4,1x,f6.4,1x,3(f4.2,1x))') (threepoly(j,i),j=1,5)
            write(ioout,'(24x,g11.4,1x,f6.4,1x,3(f4.2,1x))') (threepoly(j,i),j=6,10)
            write(ioout,'(24x,g11.4)') threepoly(11,i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Bond-angle cross term 3-body form
!
        do i = 1,nthb
          if ((nthrty(i).eq.11.or.nthrty(i).eq.18).and.iptyp(i).eq.k) then
            if (lout) then
              if (nthrty(i).eq.11) then
                write(ioout,'(/,''  Bond-Angle cross form:'',/)')
              else
                write(ioout,'(/,''  Bond-Angle cosine cross form:'',/)')
              endif
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types           Force Csts        R0     Theta      Cutoffs  '')')
              if (nthrty(i).eq.11) then
                write(ioout,'(''   1       2       3       (eV/rad)         (Angs)   (deg)   1-2   1-3   2-3  '')')
              else
                write(ioout,'(''   1       2       3       (eV)             (Angs)   (deg)   1-2   1-3   2-3  '')')
              endif
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            the = theta(i)
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,f8.4,1x,f8.4,2x,2(f4.2,1x),f6.2,5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thrho3(i),thrho1(i),thrho2(i),the
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,f8.4,1x,f8.4,2x,2(f4.2,1x),f6.2,3(1x,f5.2))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thrho3(i),thrho1(i),thrho2(i),the, &
                thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(59x,3(1x,f5.2))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Linear three-body term
!
        do i = 1,nthb
          if (nthrty(i).eq.12.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Linear-threebody form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''Atom Type  Atom Type  Atom Type  Force Const    n    Cutoff   Cutoff   Cutoff'')')
              write(ioout,'(''    1          2          3         (eV)              1-2      1-3      2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,3x,g14.6,1x,i2,5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),nint(thrho1(i))
            else
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,3x,g14.5,1x,i2,3(2x,f7.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),nint(thrho1(i)),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(49x,3(2x,f7.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Bcoscross potential
!
        do i = 1,nthb
          if (nthrty(i).eq.13.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Bond-bond cross + cosine term potential:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types        Force Cst     B  m/n  Rho1  Rho2       Cutoffs      '')')
              write(ioout,'(''   1       2       3    (eVrad**-2)              (Angs)     1-2   1-3   2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,g10.4,1x,f5.2,1x,2(i1,1x),f5.3,1x,f5.3,4x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),nint(threepoly(1,i)),nint(thrho3(i)),thrho1(i), &
                thrho2(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.4,1x,f5.2,1x,2(i1,1x),f5.3,1x,f5.3,1x,3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),nint(threepoly(1,i)),nint(thrho3(i)),thrho1(i), &
                thrho2(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))')thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Stillinger-Weber threebody - Jiang & Brown modification
!
        do i = 1,nthb
          if (nthrty(i).eq.14.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Stillinger-Weber / Jiang & Brown form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types        Force Cst    Theta      Rhos           Cutoffs         '')')
              write(ioout,'(''   1       2       3    (eVrad**-2)     Q       (Angs)     1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            the = theta(i)
            write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,g11.4,1x,f8.3,1x,f11.6,3(1x,f6.3))') &
              lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),the,thrho1(i),thr1min(i),thr2min(i),thr3min(i)
            write(ioout,'(36x,f8.4,1x,f11.6,3(1x,f6.3))') thrho3(i),thrho2(i),thr1(i),thr2(i),thr3(i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Hydrogen-bond three-body potential
!
        do i = 1,nthb
          if (nthrty(i).eq.15.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Hydrogen-bond potential:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types         m  n  p        A          B             Cutoffs  '')')
              write(ioout,'(''   1       2       3                (eV*Ang**m) (eV*Ang**n)  1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(3(a5,1x,a1,1x),3i3,1x,f12.4,2x,f10.4,6x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,nint(thrho1(i)),nint(thrho2(i)),nint(thrho3(i)), &
                thbk(i),theta(i)
            else
              write(ioout,'(3(a5,1x,a1,1x),3i3,1x,f12.4,2x,f10.4,3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,nint(thrho1(i)),nint(thrho2(i)),nint(thrho3(i)), &
                thbk(i),theta(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Equatorial ESFF 3-body potential
!
        do i = 1,nthb
          if (nthrty(i).eq.16.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Equatorial ESFF form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types       Force Cst      N     Beta      r0          Cutoffs  '')')
              write(ioout,'(''   1       2       3       (eV)           (Ang**-1)   (Ang)   1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            the = theta(i)
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.4,2x,i3,2(1x,f8.4),5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),nint(the),thrho1(i),thrho2(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g10.4,2x,i3,2(1x,f8.4),3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),nint(the),thrho1(i),thrho2(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))')thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Non-linear UFF three-body term
!
        do i = 1,nthb
          if (nthrty(i).eq.17.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  UFF non-linear threebody form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''Atom Type  Atom Type  Atom Type  Force Const Theta0  Cutoff   Cutoff   Cutoff'')')
              write(ioout,'(''    1          2          3         (eV)               1-2      1-3      2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,1x,g15.6,1x,f6.2,6x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i)
            else
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,1x,g15.5,1x,f6.2,3(2x,f7.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(49x,3(2x,f7.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  3Coulomb potential
!
        do i = 1,nthb
          if (nthrty(i).eq.19.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  3 Coulomb potential:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types        Coulomb subtract                         Cutoffs  '')')
              write(ioout,'(''   1       2       3          (frac)                         1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g16.5,25x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,2x,g16.5,17x,3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(58x,3(1x,f6.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  Exponentially decaying three-body potential with only 2 exponentials
!
        do i = 1,nthb
          if (nthrty(i).eq.20.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  Exponential 2 three-body form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''       Atom Types         Force Cst     Rho12     R12_0          Cutoffs  '')')
              write(ioout,'(''   1       2       3     (eVrad**-2)    (A-1)     (Ang)     1-2    1-3    2-3  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,f11.5,1x,2f10.4,5x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),thrho1(i)
              write(ioout,'(36x,2f10.4)') thrho2(i),thrho3(i)
            else
              write(ioout,'(a5,1x,a1,1x,a5,1x,a1,1x,a5,1x,a1,1x,f11.5,1x,2f10.4,1x,3(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),thrho1(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(36x,2f10.4,1x,3(1x,f6.3))') thrho2(i),thrho3(i),thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
!
!  g3Coulomb potential
!
        do i = 1,nthb
          if (nthrty(i).eq.21.and.iptyp(i).eq.k) then
            if (lout) then
              write(ioout,'(/,''  g3coulomb subtract form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''Atom Type  Atom Type  Atom Type  Force Const  Gamma  Cutoff   Cutoff   Cutoff'')')
              write(ioout,'(''    1          2          3        (a.u.)    (Ang**3)  1-2      1-3      2-3 '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              lout = .false.
            endif
            nat1 = ntspec1(i)
            nat2 = ntspec2(i)
            nat3 = ntspec3(i)
            ntype1 = ntptyp1(i)
            ntype2 = ntptyp2(i)
            ntype3 = ntptyp3(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (mmtexc(i).eq.1) then
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,1x,g15.6,1x,f6.2,6x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i)
            else
              write(ioout,'(a5,1x,a1,4x,a5,1x,a1,4x,a5,1x,a1,1x,g15.5,1x,f6.2,3(2x,f7.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,thbk(i),theta(i),thr1min(i),thr2min(i),thr3min(i)
              write(ioout,'(52x,3(2x,f7.3))') thr1(i),thr2(i),thr3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        lout = .true.
      endif
    enddo
    deallocate(iptyp,stat=status)
    if (status/=0) call deallocate_error('outpot','iptyp')
  endif
!**********************************
!  Output four-bodied potentials  *
!**********************************
  if (nfor.gt.0) then
!
!  Count number of general, intra- and inter-molecular potentials
!
    nmpt(1) = 0
    nmpt(2) = 0
    nmpt(3) = 0
    allocate(iptyp(nfor),stat=status)
    if (status/=0) call outofmemory('outpot','iptyp')
    if (lmol) then
      do i = 1,nfor
        if (lfintra(i).and..not.lfinter(i)) then
          nmpt(2) = nmpt(2) + 1
          iptyp(i) = 2
        elseif (lfinter(i).and..not.lfintra(i)) then
          nmpt(3) = nmpt(3) + 1
          iptyp(i) = 3
        else
          nmpt(1)= nmpt(1) + 1
          iptyp(i) = 1
        endif
      enddo
    else
      nmpt(1) = nfor
      do i = 1,nfor
        iptyp(i) = 1
      enddo
    endif
!
!  Standard four-body potentials
!
    do k = 1,3
      if (nmpt(k).gt.0) then
        if (k.eq.1) then
          write(ioout,'(/,''  General Four-body potentials :'')')
        elseif (k.eq.2) then
          write(ioout,'(/,''  Intramolecular Four-body potentials :'')')
        elseif (k.eq.3) then
          write(ioout,'(/,''  Intermolecular Four-body potentials :'')')
        endif
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.1.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Standard form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types             Force cst Sign Phase Phi0         Cutoffs'')')
              write(ioout,'(''   1       2       3       4       (eV)                     1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            phi0 = forpoly(1,i)
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.4,1x,a1,2x,i2,3x,f6.2,2x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)), &
                phi0,botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.4,1x,a1,2x,i2,3x,f6.2,1x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)), &
                phi0,for1(i),for2(i),for3(i),for4(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Ryckaert-Bellemanns four body potential
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.2.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Ryckaert-Bellemanns form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types              Zeroth-order Power            Cutoffs'')')
              write(ioout,'(''   1       2       3       4      term (eV)             1-2   2-3   3-4   4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            norder = npfor(i)
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g11.5,3x,i2,6x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),npfor(i),botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g11.5,3x,i2,5x,4f6.2)') lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4, &
                cstyp4,fork(i),npfor(i),for1(i),for2(i),for3(i),for4(i)
            endif
            if (norder.gt.1) then
              write(ioout,'(''Coefficients (eV) = '',5f12.6)') &
                (forpoly(j,i),j = 1,norder)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Out of plane potential - non-inversion
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.3.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Out of plane potentials:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types              Force constants              Cutoffs'')')
              write(ioout,'(''   1       2       3       4      (eV/Angs**k)          1-2      1-3      1-4   '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (mmfexc(i).eq.1) then
              write(ioout,'(4(a5,1x,a1,1x),1x,g9.4,1x,g9.4,10x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),forpoly(1,i)
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g9.4,1x,g9.4,1x,3f8.3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),forpoly(1,i),for1min(i),for2min(i),for3min(i)
              write(ioout,'(53x,3f8.3)') for1(i),for2(i),for3(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Out of plane potential - inversion
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.11.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Inversion out of plane potentials:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types              Force constant                Cutoffs'')')
              write(ioout,'(''   1       2       3       4          (eV)               1-2   1-3   2-3   1-4'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (mmfexc(i).eq.1) then
              write(ioout,'(4(a5,1x,a1,1x),2x,g16.6,14x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i)
            else
              write(ioout,'(4(a5,1x,a1,1x),2x,g16.6,5x,4f6.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),for1(i),for2(i),for3(i),for4(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Out of plane potential - inversion squared
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.12.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Inversion squared out of plane potentials:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types              Force const     K0            Cutoffs'')')
              write(ioout,'(''   1       2       3       4          (eV)               1-2   1-3   2-3   1-4'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (mmfexc(i).eq.1) then
              write(ioout,'(4(a5,1x,a1,1x),2x,g16.6,14x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i)
            else
              write(ioout,'(4(a5,1x,a1,1x),2x,g12.6,f9.4,4f6.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),forpoly(1,i),for1(i),for2(i),for3(i),for4(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Out of plane potential - UFF form
!  
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.15.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  UFF out of plane potentials:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types                      K         C0/C1/C2       Cutoffs'')')
              write(ioout,'(''   1       2       3       4            (eV)                  1-2   1-3   1-4'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (mmfexc(i).eq.1) then
              write(ioout,'(4(a5,1x,a1,1x),2x,g16.6,1x,f8.4,8x,''Bonded'')') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),forpoly(1,i)
            else
              write(ioout,'(4(a5,1x,a1,1x),2x,g16.6,1x,f8.4,1x,3f6.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),forpoly(1,i),for1(i),for2(i),for3(i)
            endif
            write(ioout,'(51x,f8.4)') forpoly(2,i)
            write(ioout,'(51x,f8.4)') forpoly(3,i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  ESFF torsional potential
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.4.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  ESFF form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types             Force cst1 Sign Phase Force cst2   Cutoffs'')')
              write(ioout,'(''   1       2       3       4       (eV)      1               1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            phi0 = forpoly(1,i)
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,1x,f8.4,2x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)),phi0, &
                botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,1x,f8.4,1x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)),phi0, &
                for1(i),for2(i),for3(i),for4(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Harmonic torsional potential
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.5.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Harmonic torsional form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types             Force cst1       Phi0              Cutoffs'')')
              write(ioout,'(''   1       2       3       4    (eV/rad**2)    (degrees)      1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            phi0 = forpoly(1,i)
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g14.5,1x,f8.4,5x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),phi0,botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g14.5,1x,f8.4,4x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),phi0,for1(i),for2(i),for3(i),for4(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Exponentially decaying torsional potential
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.6.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Exponentially decaying torsional form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types             Force cst Sign Phase Phi0         Cutoffs'')')
              write(ioout,'(''   1       2       3       4       (eV)                     1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            phi0 = forpoly(1,i)
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,2x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)),phi0, &
                botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,1x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)), &
                phi0,for1(i),for2(i),for3(i),for4(i)
            endif
            write(ioout,'(24x,''rho12/rho23/rho34(Angs) = '',3f10.6)') forpoly(2,i),forpoly(3,i),forpoly(4,i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Exponentially decaying ESFF torsional potential
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.7.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Exponentially decaying ESFF torsional form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types            Force cst1 Sign Phase Force cst2   Cutoffs'')')
              write(ioout,'(''   1       2       3       4       (eV)                     1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,2x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)),forpoly(1,i), &
                botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,1x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)), &
                forpoly(1,i),for1(i),for2(i),for3(i),for4(i)
            endif
            write(ioout,'(24x,''rho12/rho23/rho34(Angs) = '',3f10.6)') forpoly(2,i),forpoly(3,i),forpoly(4,i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Tapered torsional potential
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.8.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Tapered torsional form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types             Force cst Sign Phase Phi0         Cutoffs'')')
              write(ioout,'(''   1       2       3       4       (eV)                     1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            phi0 = forpoly(1,i)
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,2x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)),phi0, &
                botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,1x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)), &
                phi0,for1(i),for2(i),for3(i),for4(i)
            endif
            write(ioout,'(59x,''Taper range  = '',f5.2)') forpoly(2,i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Tapered ESFF torsional potential
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.9.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Tapered ESFF torsional form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types            Force cst1 Sign Phase Force cst2   Cutoffs'')')
              write(ioout,'(''   1       2       3       4       (eV)                     1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,2x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)),forpoly(1,i), &
                botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,a1,2x,i2,3x,f6.2,1x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),sgs(ind),abs(npfor(i)), &
                forpoly(1,i),for1(i),for2(i),for3(i),for4(i)
            endif
            write(ioout,'(59x,''Taper range  = '',f5.2)') forpoly(2,i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Torsional - angle cross term potential
!
        lout = .true.
        do i = 1,nfor
          if ((nforty(i).eq.10.or.nforty(i).eq.17).and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              if (nforty(i).eq.10) then
                write(ioout,'(/,''  Torsional-angle cross potential:'',/)')
              else
                write(ioout,'(/,''  Torsional-cosine angle cross potential:'',/)')
              endif
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types             Force cst1       Phi0              Cutoffs'')')
              if (nforty(i).eq.10) then
                write(ioout,'(''   1       2       3       4    (eV/rad**2)    (degrees)      1-2  2-3  3-4  4-1'')')
              else
                write(ioout,'(''   1       2       3       4       (eV)        (degrees)      1-2  2-3  3-4  4-1'')')
              endif
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g14.5,1x,f8.4,5x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),forpoly(1,i), &
                botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g14.5,1x,f8.4,4x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),forpoly(1,i), &
                for1(i),for2(i),for3(i),for4(i)
            endif
            write(ioout,'(48x,f8.4)') forpoly(2,i)
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  UFF4 torsional
!
        lout = .true.
        do i = 1,nfor
          if (nforty(i).eq.13.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  UFF4 form:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types             Force cst Sign Phase Phi0         Cutoffs'')')
              write(ioout,'(''   1       2       3       4       (eV)                     1-2  2-3  3-4  4-1'')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            phi0 = forpoly(1,i)
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,3x,i2,3x,f6.2,2x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),abs(npfor(i)),phi0, &
                botyword(nbotyptr),botyword2(n4botype(2,i))
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g10.5,1x,3x,i2,3x,f6.2,1x,4f5.2)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),abs(npfor(i)), &
                phi0,for1(i),for2(i),for3(i),for4(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
!
!  Angle-angle cross out of plane potential 
!
        lout = .true.
        do i = 1,nfor
          if ((nforty(i).eq.14.or.nforty(i).eq.16).and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              if (nforty(i).eq.14) then
                write(ioout,'(/,''  Angle-angle cross form:'',/)')
              else
                write(ioout,'(/,''  Cosine angle - cosine angle cross form:'',/)')
              endif
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''         Atom Types               Force csts       Theta0          Cutoffs'')')
              if (nforty(i).eq.14) then
                write(ioout,'(''   1       2       3       4      (eV/rad2)       (degrees)    1-2   1-3   1-4  '')')
              else
                write(ioout,'(''   1       2       3       4      (eV)            (degrees)    1-2   1-3   1-4  '')')
              endif
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nfspec1(i)
            nat2 = nfspec2(i)
            nat3 = nfspec3(i)
            nat4 = nfspec4(i)
            ntype1 = nfptyp1(i)
            ntype2 = nfptyp2(i)
            ntype3 = nfptyp3(i)
            ntype4 = nfptyp4(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (npfor(i).gt.0) then
              ind = 1
            else
              ind = 2
            endif
            phi0 = forpoly(3,i)*radtodeg
            if (mmfexc(i).eq.1) then
              nbotyptr = n4botype(1,i) + 1
              write(ioout,'(4(a5,1x,a1,1x),1x,g12.5,6x,f7.3,3x,''Bonded'',1x,a9,1x,a3)') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i),phi0, &
                botyword(nbotyptr),botyword2(n4botype(2,i))
              write(ioout,'(33x,g12.5,6x,f7.3)') forpoly(1,i),forpoly(4,i)*radtodeg
              write(ioout,'(33x,g12.5,6x,f7.3)') forpoly(2,i),forpoly(5,i)*radtodeg
            else
              write(ioout,'(4(a5,1x,a1,1x),1x,g12.5,6x,f7.3,2x,f6.3,2(1x,f6.3))') &
                lab1,cstyp1,lab2,cstyp2,lab3,cstyp3,lab4,cstyp4,fork(i), &
                phi0,for1(i),for2(i),for3(i)
              write(ioout,'(33x,g12.5,6x,f7.3)') forpoly(1,i),forpoly(4,i)*radtodeg
              write(ioout,'(33x,g12.5,6x,f7.3)') forpoly(2,i),forpoly(5,i)*radtodeg
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
      endif
    enddo
    deallocate(iptyp,stat=status)
    if (status/=0) call deallocate_error('outpot','iptyp')
  endif
!**********************************
!  Output six-bodied potentials  *
!**********************************
  if (nsix.gt.0) then
!
!  Count number of general, intra- and inter-molecular potentials
!
    nmpt(1) = 0
    nmpt(2) = 0
    nmpt(3) = 0
    allocate(iptyp(nsix),stat=status)
    if (status/=0) call outofmemory('outpot','iptyp')
    if (lmol) then
      do i = 1,nsix
        if (lsintra(i).and..not.lsinter(i)) then
          nmpt(2) = nmpt(2) + 1
          iptyp(i) = 2
        elseif (lsinter(i).and..not.lsintra(i)) then
          nmpt(3) = nmpt(3) + 1
          iptyp(i) = 3
        else
          nmpt(1) = nmpt(1) + 1
          iptyp(i) = 1
        endif
      enddo
    else
      nmpt(1) = nsix
      do i = 1,nsix
        iptyp(i) = 1
      enddo
    endif
!
!  Standard six-body potentials
!
    do k = 1,3
      if (nmpt(k).gt.0) then
        if (k.eq.1) then
          write(ioout,'(/,''  General Six-body potentials :'')')
        elseif (k.eq.2) then
          write(ioout,'(/,''  Intramolecular Six-body potentials :'')')
        elseif (k.eq.3) then
          write(ioout,'(/,''  Intermolecular Six-body potentials :'')')
        endif
        lout = .true.
        do i = 1,nsix
          if (nsixty(i).eq.1.and.iptyp(i).eq.k) then
            if (lout) then
              lout = .false.
              write(ioout,'(/,''  Cross - out of plane :'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''          Atom Types              Force cst                       Cutoffs       '')')
              write(ioout,'(''           1       2                 (eV)                           1-2         '')')
              write(ioout,'(''   3       4       5       6                                1-3  1-4  2-5  2-6  '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            nat1 = nsspec1(i)
            nat2 = nsspec2(i)
            nat3 = nsspec3(i)
            nat4 = nsspec4(i)
            nat5 = nsspec5(i)
            nat6 = nsspec6(i)
            ntype1 = nsptyp1(i)
            ntype2 = nsptyp2(i)
            ntype3 = nsptyp3(i)
            ntype4 = nsptyp4(i)
            ntype5 = nsptyp5(i)
            ntype6 = nsptyp6(i)
            call label(nat1,ntype1,lab1)
            call label(nat2,ntype2,lab2)
            call label(nat3,ntype3,lab3)
            call label(nat4,ntype4,lab4)
            call label(nat5,ntype5,lab5)
            call label(nat6,ntype6,lab6)
            cstyp1 = 'c'
            cstyp2 = 'c'
            cstyp3 = 'c'
            cstyp4 = 'c'
            cstyp5 = 'c'
            cstyp6 = 'c'
            if (nat1.gt.maxele) cstyp1 = 's'
            if (nat2.gt.maxele) cstyp2 = 's'
            if (nat3.gt.maxele) cstyp3 = 's'
            if (nat4.gt.maxele) cstyp4 = 's'
            if (nat5.gt.maxele) cstyp5 = 's'
            if (nat6.gt.maxele) cstyp6 = 's'
            if (mmsexc(i).eq.1) then
              nbotyptr = n6botype(1,i) + 1
              write(ioout,'(8x,2(a5,1x,a1,1x),9x,g12.5,15x,''Bonded'',1x,a9)') &
                lab1,cstyp1,lab2,cstyp2,sixk(i),botyword(nbotyptr)
              write(ioout,'(4(a5,1x,a1,1x))') &
                lab3,cstyp3,lab4,cstyp4,lab5,cstyp5,lab6,cstyp6
            else
              write(ioout,'(8x,2(a5,1x,a1,1x),9x,g12.5,21x,f5.2)') &
                lab1,cstyp1,lab2,cstyp2,sixk(i),six1(i)
              write(ioout,'(4(a5,1x,a1,1x),26x,4f5.2)') &
                lab3,cstyp3,lab4,cstyp4,lab5,cstyp5,lab6,cstyp6,six2(i),six3(i),six4(i),six5(i)
            endif
          endif
        enddo
        if (.not.lout) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
      endif
    enddo
    deallocate(iptyp,stat=status)
    if (status/=0) call deallocate_error('outpot','iptyp')
  endif
!***********************************
!  Bond-order two-body potentials  *
!***********************************
  if (nbopot.gt.0) then
    lout = .true.
    do i = 1,nbopot
      nat1 = nBOspec1(i)
      nat2 = nBOspec2(i)
      ntype1 = nBOtyp1(i)
      ntype2 = nBOtyp2(i)
      call label(nat1,ntype1,lab1)
      call label(nat2,ntype2,lab2)
      cstyp1 = 'c'
      cstyp2 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
      if (nat2.gt.maxele) cstyp2 = 's'
!
      if (lout) then
        lout = .false.
        write(ioout,'(/,''  Bond-order two-body potentials :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Types                 A                 B                  Cutoffs(Ang)   '')')
        write(ioout,'(''  1     2                 ZetaA             ZetaB                Taper    Max   '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      write(ioout,'(2(a5,a1,1x),4x,2(f20.10,1x),1x,2(1x,f8.4))') lab1,cstyp1,lab2,cstyp2,BOacoeff(i), &
        BObcoeff(i),rBOmin(i),rBOmax(i)
      write(ioout,'(21x,2(g20.10,1x))') BOzacoeff(i),BOzbcoeff(i)
      if (.not.lout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    enddo
  endif
!
  if (nboR.gt.0) then
    lout = .true.
    do i = 1,nboR
      nat1 = nBOspecR1(i)
      ntype1 = nBOtypR1(i)
      call label(nat1,ntype1,lab1)
      cstyp1 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
      nat2 = nBOspecR2(i)
      ntype2 = nBOtypR2(i)
      call label(nat2,ntype2,lab2)
      cstyp2 = 'c'
      if (nat2.gt.maxele) cstyp2 = 's'
!
      if (lout) then
        lout = .false.
        write(ioout,'(/,''  Bond-order repulsive :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom    Type         Alpha            N             Lambda          M           '')')
        if (nBOtypeR(i).ne.1) then
          write(ioout,'('' 1/2     1/2           C              D                H                        '')')
        else
          write(ioout,'('' 1/2     1/2                                                                    '')')
        endif
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      if (nBOtypeR(i).eq.1) then
        write(ioout,'(a5,a1,6x,1x,3(f16.8,1x),4x,i3)') lab1,cstyp1,BOecoeffR(i),BOncoeffR(i),BOlcoeffR(i), &
          nint(BOmcoeffR(i))
        write(ioout,'(a5,a1)') lab2,cstyp2
      else
        write(ioout,'(a5,a1,2x,''Theta'',1x,3(f16.8,1x),4x,i3)') lab1,cstyp1,BOecoeffR(i),BOncoeffR(i), &
          BOlcoeffR(i),nint(BOmcoeffR(i))
        write(ioout,'(a5,a1,8x,3(f16.8,1x))') lab2,cstyp2,BOccoeffR(i),BOdcoeffR(i),BOhcoeffR(i)
      endif
      if (.not.lout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    enddo
  endif
!
  if (nboA.gt.0) then
    lout = .true.
    do i = 1,nboA
      nat1 = nBOspecA1(i)      
      ntype1 = nBOtypA1(i)      
      call label(nat1,ntype1,lab1)      
      cstyp1 = 'c'      
      if (nat1.gt.maxele) cstyp1 = 's'      
      nat2 = nBOspecR2(i)
      ntype2 = nBOtypR2(i)
      call label(nat2,ntype2,lab2)
      cstyp2 = 'c'
      if (nat2.gt.maxele) cstyp2 = 's'
!
      if (lout) then
        lout = .false.
        write(ioout,'(/,''  Bond-order attractive :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom    Type         Alpha            N             Lambda          M           '')')
        if (nBOtypeA(i).ne.1) then
          write(ioout,'('' 1/2     1/2           C              D                H                        '')')
        else
          write(ioout,'('' 1/2     1/2                                                                    '')')
        endif
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      if (nBOtypeA(i).eq.1) then
        write(ioout,'(a5,a1,6x,1x,3(f16.8,1x),4x,i3)') lab1,cstyp1,BOecoeffA(i),BOncoeffA(i),BOlcoeffA(i), &
          nint(BOmcoeffA(i))
        write(ioout,'(a5,a1)') lab2,cstyp2
      else
        write(ioout,'(a5,a1,2x,''Theta'',1x,3(f16.8,1x),4x,i3)') lab1,cstyp1,BOecoeffA(i),BOncoeffA(i), &
          BOlcoeffA(i),nint(BOmcoeffA(i))
        write(ioout,'(a5,a1,8x,3(f16.8,1x))') lab2,cstyp2,BOccoeffA(i),BOdcoeffA(i),BOhcoeffA(i)
      endif
      if (.not.lout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    enddo
  endif
!*********************************
!  Bond-order charge potentials  *
!*********************************
  if (nboQ.gt.0) then
    lout = .true.
    do i = 1,nboQ
      nat1 = nBOspecQ1(i)
      nat2 = nBOspecQ2(i)
      ntype1 = nBOtypQ1(i)
      ntype2 = nBOtypQ2(i)
      call label(nat1,ntype1,lab1)
      call label(nat2,ntype2,lab2)
      cstyp1 = 'c'
      cstyp2 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
      if (nat2.gt.maxele) cstyp2 = 's'
!
      if (lout) then
        lout = .false.
        write(ioout,'(/,''  Bond-order charge potentials :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Types                         q0           Taper           Cutoffs(Ang)   '')')
        write(ioout,'(''  1     2                                        type            Taper    Max   '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      if (nBOtaperQ(i).eq.2) then
        write(ioout,'(2(a5,a1,1x),15x,f15.6,5x,''sine  '',6x,2(1x,f8.4))') lab1,cstyp1,lab2,cstyp2,BOq0(i), &
          rBOminQ(i),rBOmaxQ(i)
      else
        write(ioout,'(2(a5,a1,1x),15x,f15.6,5x,''cosine'',6x,2(1x,f8.4))') lab1,cstyp1,lab2,cstyp2,BOq0(i), &
          rBOminQ(i),rBOmaxQ(i)
      endif
      if (.not.lout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    enddo
  endif
!**********************************
!  Bond-order charge self-energy  *
!**********************************
  if (nboQ0.gt.0) then
    lout = .true.
    do i = 1,nboQ0
      nat1 = nBOspecQ0(i)
      ntype1 = nBOtypQ0(i)
      call label(nat1,ntype1,lab1)
      cstyp1 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
!
      if (lout) then
        lout = .false.
        write(ioout,'(/,''  Bond-order charge self energy :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Type           e0               rho              q0                       '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      write(ioout,'(a5,1x,a1,3(6x,f15.6))') lab1,cstyp1,BOq0pot(i),BOq0rho(i),BOq0ref(i)
      if (.not.lout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    enddo
  endif
!*********************
!  Plane potentials  *
!*********************
  if (nplanepot.gt.0) then
    lout = .true.
    do i = 1,nplanepot
      nat1 = natplanepot(i)
      ntype1 = ntypplanepot(i)
      call label(nat1,ntype1,lab1)
      cstyp1 = 'c'
      if (nat1.gt.maxele) cstyp1 = 's'
!
      if (lout) then
        lout = .false.
        write(ioout,'(/,''  Plane potentials (L-J):'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''Atom  Type    m  n           z              A              B         rmin  rmax '')')
        write(ioout,'(''                           (Ang)       (eV/Ang**m)     (eV/Ang**n)   (Ang) (Ang)'')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      write(ioout,'(a5,1x,a1,5x,2i3,3(1x,f15.6),2(1x,f6.3))') &
        lab1,cstyp1,nplanepotpower(1,i),nplanepotpower(2,i),(planepot(j,i),j=1,3),planepotrmin(i),planepotrmax(i)
      if (.not.lout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
    enddo
  endif
!
  return
  end
