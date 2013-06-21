  subroutine dumpgen(iout)
!
!  Dump out non-configuration specific parameters for restart file
!
!   2/97 BSM exponential and damped dispersion potls added
!   4/97 Many-body potential added
!   7/97 EAM densities added
!   7/97 EAM functionals added
!   5/98 Error in scaling of fitting constraints in restart file corrected
!   5/98 XYZ file format added
!   7/98 History file format added
!   8/98 FDF file format added
!  10/98 Codes for fitting variables simplified
!   1/99 General scale factor and 1-4 only options added to Coulomb pot
!   3/99 DRV and FRC files added 
!  10/99 Cubic density for EAM added
!   5/00 C10 term added to damped dispersion potential
!   5/00 Polarisability species output added
!   7/00 rmin for Morse added
!   7/00 CovExp potential added
!   1/01 MC options output added
!   2/01 Fermi-Dirac potential added
!   3/01 CIF files added
!   6/01 COSMO options added
!   6/01 Fitting flags for damp_dispersion potential corrected
!   6/01 Dumping of VDW radii added
!   7/01 Potentials moved to separate file
!   9/01 Moptit incremented to counter decrease on input
!  11/01 Check on element parameters now uses global save arrays
!   2/02 Dimensioning of ntemp corrected
!   5/02 Minimum cell parameter added
!   5/02 Only weights not otherwise output now outputed here
!   5/02 Brenner potential added
!   8/02 Modifications for DLV files made
!  10/02 mcvolume added
!  11/02 Handling of changed EEM/QEq parameters improved
!   1/03 Wolf sum parameters added
!   2/03 NEB data added
!  11/03 Labelling of genetic algorithm section corrected to "genetic"
!   4/04 Output of pressure file added
!   4/04 Terse option added
!  12/04 Default sasparticle option changed
!   1/05 Identification of elements in element info now uses symbol
!   1/05 output sas option added
!   2/05 Potential interpolation option added
!   4/05 cwolf option added
!   4/05 Modifications for shell extrapolation added
!   6/05 Flags for EEM/QEq chi & mu added
!   7/05 Outputting of shell mass ratios changed to be species-wise
!   7/05 Streitz and Mintmire modifications added
!   9/05 Powers added for fit constraints
!  11/05 Voter style taper added
!  11/05 Bug in output of ratiom corrected
!   3/06 Output of oscillator strengths added
!   5/06 Output of shellmass parameters corrected
!   5/06 Output of masses now handles species specific case
!   8/06 Radius added to output of QEq parameters
!  11/06 NEB modifications added
!   1/07 Gasteiger parameters added
!   1/07 Bug fixed in outputing of element properties where more than thing
!        has been changed in the input.
!   5/07 Lowest sub-option added to mcsample dump
!   5/07 Probability of straining cell added
!   5/07 Exponential taper added
!   6/07 Rotation types added for MC
!   6/07 Format of dump for mc options modified, kcaltoev
!  10/07 Cutp format and default value changed
!  11/07 MDF taper added
!  11/07 Output of a biograf format file for ReaxFF added
!  12/07 Option to control frequency of writing to arcfile added
!  12/07 mdmaxtemp option added
!   3/08 mdmaxvolume option added
!   3/08 New MD integrator added
!   4/08 rcspatial added
!   4/08 Output of phonon finite difference interval added
!   5/08 Maxfcal now checked against fixed value of 5000 for output
!   8/08 Option to output number of random number calls added
!  10/08 Option to not overwrite dumpfiles added
!  10/08 COSMO/COSMIC options added
!  11/08 Checking of element values modified to use difference relative to a 
!        tolerance to handle rounding error
!  12/08 synchronous transit specific parameters added
!   1/09 swap move added to Monte Carlo
!   1/09 Checking of element parameters improved and format for write extended
!   1/09 Output of mcelowest added
!   1/09 Name of new integrator changed to stochastic
!   2/09 Default value of accuracy changed to 12
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   3/09 Anisotropic rcspatial sub-option added
!   3/09 lfinitediff now used to decide whether finite difference option is output
!   4/09 Output of lmbfgs_order corrected to use the tag lbfgs_order
!   6/09 Output of element info for PDFs added
!   3/10 Output of default weights added
!   4/10 COSMO file type added
!   8/10 Output of random number info modified
!   9/10 Default MD integrator changed to stochastic
!  10/10 qbo file output added
!   3/11 Lammps potential files added
!   9/11 Output of plumed file added
!   3/12 Output of DCD files added
!   6/12 Option to select the maths library added
!   9/12 Unique sub-option added to mdarchive option
!   9/12 NMR properties added for species
!  10/12 Modifications for output of lammps files added
!  11/12 Modified Wolf sum of Fennell and Gezelter added
!  12/12 NMR f parameter added
!  12/12 q0 parameters added for electronegativity models
!   5/13 Dumping of UFF bond orders added
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
!  Julian Gale, NRI, Curtin University, May 2013
!
  use brennerdata
  use cellmultipole
  use configurations,     only : ncfg
  use constants,          only : angstoev, kcaltoev
  use control
  use cosmo
  use current
  use defects
  use distances,          only : extracutoff
  use dump
  use element
  use files
  use fitting
  use general
  use genetic
  use gulp_cml,           only : lcml, lvcml, cmlfilename
  use library
  use maths,              only : leispack_eigensolve, ldivide_and_conquer
  use moldyn
  use molecule
  use montecarlo
  use neb
  use observables
  use optimisation
  use plumed
  use potentialinterpolation
  use properties
  use randomnumbers,      only : nrandomcalls, npr_randomcalls, npr_grandomcalls, lGaussianLast
  use shell
  use shellextrapolation, only : lextrapolateshells, maxextrapol
  use spatial,            only : rcspatial, lrcspatial_anisotropic
  use spatial,            only : rcspatialx, rcspatialy, rcspatialz
  use spatialbo,          only : rcspatialbo, lrcspatialBO_anisotropic
  use spatialbo,          only : rcspatialbox, rcspatialboy, rcspatialboz
  use species
  use sutton
  use synchro,            only : maxsynciter, maxsyncstep, synctol
  use terse
  use two
  use uffdata,            only : UFFbondorder
  use wolfcosmo,          only : cutwc, etawc
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in)                :: iout
!
!  Local variables
!
  character(len=4), dimension(:), allocatable :: ctmp2
  character(len=5), dimension(:), allocatable :: ctmp
  character(len=5)                            :: lab1
  character(len=5)                            :: lab2
  character(len=5)                            :: lab3
  character(len=5)                            :: lab4
  character(len=5)                            :: labels(11)
  character(len=80)                           :: line
  character(len=5)                            :: minword(6)
  character(len=6)                            :: mcword(3)
  integer(i4)                                 :: i
  integer(i4)                                 :: ichcrit
  integer(i4),      dimension(:), allocatable :: itmp
  integer(i4)                                 :: itype1
  integer(i4)                                 :: itype2
  integer(i4)                                 :: j
  integer(i4)                                 :: k
  integer(i4)                                 :: m
  integer(i4)                                 :: n1
  integer(i4)                                 :: n2
  integer(i4)                                 :: n3
  integer(i4)                                 :: n4
  integer(i4)                                 :: na
  integer(i4)                                 :: nati
  integer(i4)                                 :: nbatch
  integer(i4)                                 :: nbatch1
  integer(i4)                                 :: nc
  integer(i4)                                 :: nct
  integer(i4)                                 :: nfcpf
  integer(i4)                                 :: nfcpv
  integer(i4)                                 :: nfcpv2
  integer(i4)                                 :: nff
  integer(i4)                                 :: nfv
  integer(i4)                                 :: nfv2
  integer(i4)                                 :: nga
  integer(i4)                                 :: ni
  integer(i4)                                 :: np
  integer(i4)                                 :: nptr
  integer(i4)                                 :: ns
  integer(i4)                                 :: nst
  integer(i4)                                 :: nsymbolout
  integer(i4)                                 :: nt
  integer(i4)                                 :: ntp
  integer(i4)                                 :: ntypi
  integer(i4),      dimension(:), allocatable :: ntemp
  integer(i4)                                 :: nv
  integer(i4)                                 :: nvar1
  integer(i4)                                 :: nvar2
  integer(i4)                                 :: nwe
  integer(i4)                                 :: status
  logical                                     :: ldiff
  logical                                     :: lnoerror
  logical                                     :: loutok
  real(dp)                                    :: fca
  real(dp)                                    :: fco
  real(dp)                                    :: fcp
  real(dp)                                    :: ft
  real(dp)                                    :: gt
  real(dp)                                    :: scalenff
  real(dp)                                    :: scalenfv
  real(dp)                                    :: sgt
  real(dp),         dimension(:), allocatable :: temp
  real(dp)                                    :: xt
!
  data minword/'bfgs ','rfo  ','unit ','numer','conj ','lbfgs'/
!
!  Allocate local memory
!
  allocate(ctmp(2*nspec),stat=status)
  if (status/=0) call outofmemory('dumpgen','ctmp')
  allocate(ctmp2(2*nspec),stat=status)
  if (status/=0) call outofmemory('dumpgen','ctmp2')
  allocate(itmp(nfit),stat=status)
  if (status/=0) call outofmemory('dumpgen','itmp')
  allocate(ntemp(max(numat,nobs,ncfg)),stat=status)
  if (status/=0) call outofmemory('dumpgen','ntemp')
  allocate(temp(max(maxele,numat,nobs,ncfg)),stat=status)
  if (status/=0) call outofmemory('dumpgen','temp')
!********************************
!  Default weights for fitting  *
!********************************
  if (delwht_angle.ne.1.0_dp) then
    write(iout,'(''default_weight angle       '',f20.8)') delwht_angle
  endif
  if (delwht_bond.ne.1.0_dp) then
    write(iout,'(''default_weight bond        '',f20.8)') delwht_bond
  endif
  if (delwht_cell_angle.ne.1000.0_dp) then
    write(iout,'(''default_weight cell_angle  '',f20.8)') delwht_cell_angle
  endif
  if (delwht_cell_length.ne.1000.0_dp) then
    write(iout,'(''default_weight cell_length '',f20.8)') delwht_cell_length
  endif
  if (delwht_coord.ne.1000.0_dp) then
    write(iout,'(''default_weight coord       '',f20.8)') delwht_coord
  endif
  if (delwht_dielectric.ne.1.0_dp) then
    write(iout,'(''default_weight dielectric  '',f20.8)') delwht_dielectric
  endif
  if (delwht_elastic.ne.0.01_dp) then
    write(iout,'(''default_weight elastic     '',f20.8)') delwht_elastic
  endif
  if (delwht_energy.ne.1.0_dp) then
    write(iout,'(''default_weight energy      '',f20.8)') delwht_energy
  endif
  if (delwht_frac.ne.10000.0_dp) then
    write(iout,'(''default_weight frac        '',f20.8)') delwht_frac
  endif
  if (delwht_freq.ne.1.0_dp) then
    write(iout,'(''default_weight freq        '',f20.8)') delwht_freq
  endif
  if (delwht_grad.ne.1.0_dp) then
    write(iout,'(''default_weight grad        '',f20.8)') delwht_grad
  endif
  if (delwht_modulus.ne.1.0_dp) then
    write(iout,'(''default_weight modulus     '',f20.8)') delwht_modulus
  endif
  if (delwht_stress.ne.1.0_dp) then
    write(iout,'(''default_weight stress      '',f20.8)') delwht_stress
  endif
!***************************
!  Observables for fitting *
!***************************
  if (nobs.gt.0) then
!
!  Weighting factors
!
    nwe = 0
    do i = 1,nobs
      nt = nobtyp(i)
      if (nt.eq.6) then
        nptr = nobptr(i)
        if (nptr.le.6) then
          if (weight(i).ne.1000.0.and.weight(i).ne.10000) then
            nwe = nwe + 1
            ntemp(nwe) = i
            temp(nwe) = weight(i)
          endif
        else
          if (weight(i).ne.10000.0) then
            nwe = nwe + 1
            ntemp(nwe) = i
            temp(nwe) = weight(i)
          endif
        endif
      elseif (nt.eq.2.or.nt.eq.17) then
        if (weight(i).ne.1.0) then
          nwe = nwe + 1
          ntemp(nwe) = i
          temp(nwe) = weight(i)
        endif
      endif
    enddo
    if (nwe.gt.0) then
      write(iout,'(''observables'')')
      write(iout,'(''weight '',i5)') nwe
      write(iout,'(8i4)')(ntemp(i),i=1,nwe)
      write(iout,'(7(f10.3,1x))')(temp(i),i=1,nwe)
      write(iout,'(''end'')')
    endif
!
!  Reaction energies
!
    lfirst = .true.
    do i = 1,nobs
      nt = nobtyp(i)
      if (nt.eq.24) then
        if (lfirst) then
          lfirst = .false.
          write(iout,'(''observables'')')
        endif
        nwe = 0
!
!  Loop over configurations to count number to be output
!
        do j = 1,ncfg
          if (abs(freaction(j,i)).gt.1.0d-3) then
            nwe = nwe + 1
            ntemp(nwe) = j
            temp(nwe)  = freaction(j,i)
          endif
        enddo
        if (nwe.gt.0) then
          write(iout,'(''reaction '')')
          if (nwe.le.4) then
            if (nwe.eq.4) then
              write(iout,'(i4,1x,f14.6,1x,f18.6,4(i4,1x,f7.3))') nwe,fobs(i),weight(i),ntemp(1),temp(1), &
                ntemp(2),temp(2),ntemp(3),temp(3),ntemp(4),temp(4)
            elseif (nwe.eq.3) then
              write(iout,'(i4,1x,f14.6,1x,f18.6,3(i4,1x,f7.3))') nwe,fobs(i),weight(i),ntemp(1),temp(1), &
                ntemp(2),temp(2),ntemp(3),temp(3)
            elseif (nwe.eq.2) then
              write(iout,'(i4,1x,f14.6,1x,f18.6,2(i4,1x,f7.3))') nwe,fobs(i),weight(i),ntemp(1),temp(1), &
                ntemp(2),temp(2)
            elseif (nwe.eq.1) then
              write(iout,'(i4,1x,f14.6,1x,f18.6,i4,1x,f7.3)') nwe,fobs(i),weight(i),ntemp(1),temp(1)
            endif
          else
            write(iout,'(i4,1x,f14.6,1x,f18.6,4(i4,1x,f7.3),'' &'')') nwe,fobs(i),weight(i),ntemp(1),temp(1), &
              ntemp(2),temp(2),ntemp(3),temp(3),ntemp(4),temp(4)
            n4 = (nwe-5)/4
            k = 0
            do j = 1,n4
              k = k + 4
              write(iout,'(34x,4(i4,1x,f7.3),'' &'')') ntemp(k+1),temp(k+1),ntemp(k+2),temp(k+2), &
                ntemp(k+3),temp(k+3),ntemp(k+4),temp(k+4)
            enddo
            k = k + 4
            n4 = nwe - k
            if (n4.eq.4) then
              write(iout,'(34x,4(i4,1x,f7.3))') ntemp(k+1),temp(k+1),ntemp(k+2),temp(k+2), &
                ntemp(k+3),temp(k+3),ntemp(k+4),temp(k+4)
            elseif (n4.eq.3) then
              write(iout,'(34x,3(i4,1x,f7.3))') ntemp(k+1),temp(k+1),ntemp(k+2),temp(k+2), &
                ntemp(k+3),temp(k+3)
            elseif (n4.eq.2) then
              write(iout,'(34x,2(i4,1x,f7.3))') ntemp(k+1),temp(k+1),ntemp(k+2),temp(k+2)
            elseif (n4.eq.1) then
              write(iout,'(34x,i4,1x,f7.3)') ntemp(k+1),temp(k+1)
            endif
          endif
        endif
      endif
    enddo
    if (.not.lfirst) then
      write(iout,'(''end'')')
    endif
  endif
!*************************
!  Variables for fitting *
!*************************
  if (nfit.gt.0) then
    ns = 0
    nst = 0
    nc = 0
    nct = 0
    do i = 1,nfit
      nt = nftyp(i)
      np = nfpot(i)
      if (nt.eq.1) then
        if (np.eq.4) then
!
!  Charge
!
          nc = nc + 1
          nct = nct + 1
          nv = nfvar(i)
          ntp = nfatyp(i)
          call label(nv,ntp,lab1)
          ctmp(nct) = lab1
          if (nv.gt.maxele) then
            nct = nct + 1
            ctmp(nct) = 'shel'
          endif
        elseif (np.eq.6) then
!
!  Core-shell charge split
!
          ns = ns + 1
          nst = nst + 1
          nv = nfvar(i)
          ntp = nfatyp(i)
          call label(nv,ntp,lab1)
          ctmp2(nst) = lab1(1:4)
          if (nv.gt.maxele) then
            nst = nst + 1
            ctmp2(nst) = 'shel'
          endif
        endif
      endif
    enddo
    if (ns+nc.gt.0) then
      write(iout,'(''variables'')')
      if (nc.gt.0) then
        nc = nc + 1
        nct = nct + 1
        call label(lastq,lastt,lab1)
        ctmp(nct) = lab1
        if (lastq.gt.maxele) then
          nct = nct + 1
          ctmp(nct) = 'shel'
        endif
        write(iout,'(''charge '',i3)') nc
        write(iout,'(12(a5,1x))')(ctmp(j),j = 1,nct)
      endif
      if (ns.gt.0) then
        write(iout,'(''split '',i3)') ns
        write(iout,'(12(a5,1x))')(ctmp2(j),j = 1,nst)
      endif
      write(iout,'(''end'')')
    endif
  endif
!*************************************
!  Constraints on fitted parameters  *
!*************************************
  if (nfcon.gt.0) then
    write(iout,'(''variables'')')
    write(iout,'(''constrain fit '',i4)')nfcon
    do i = 1,nfcon
!
!  Need to correct nfcvar and nfcfix for change in
!  potential order in restart file
!
      nfv = nfcvar(i)
      nff = nfcfix(i)
      nfcpv = nftyp(nfv)
      nfcpf = nftyp(nff)
      scalenff = scale(nff)
      scalenfv = scale(nfv)
      if (nfcpv.ge.3.and.nfcpv.le.4) then
        if (nfcpv.eq.4) then
          do j = nfv+1,nfit
            if (nftyp(j).eq.2) then
              nfv = nfv + 1
            elseif (nftyp(j).eq.3) then
              nfv = nfv + 1
            endif
          enddo
        else
          do j = 1,nfv-1
            if (nftyp(j).eq.4) then
              nfv = nfv - 1
            endif
          enddo
          do j = nfv+1,nfit
            if (nftyp(j).eq.2) then
              nfv = nfv + 1
            endif
          enddo
        endif
      elseif (nfcpv.eq.2) then
        do j = 1,nfv-1
          if (nftyp(j).gt.2.and.nftyp(j).le.4) then
            nfv = nfv - 1
          endif
        enddo
      endif
      if (nfcpf.ge.3.and.nfcpf.le.4) then
        if (nfcpf.eq.4) then
          do j = nff+1,nfit
            if (nftyp(j).eq.2) then
              nff = nfv + 1
            elseif (nftyp(j).eq.3.or.nftyp(j).eq.4) then
              nff = nff + 1
            endif
          enddo
        else
          do j = 1,nff-1
            if (nftyp(j).eq.4) then
              nff = nff - 1
            endif
          enddo
          do j = nff+1,nfit
            if (nftyp(j).eq.2) then
              nff = nff + 1
            endif
          enddo
        endif
      elseif (nfcpf.eq.2) then
        do j = 1,nff-1
          if (nftyp(j).ge.3.and.nftyp(j).le.4) then
            nff = nff - 1
          endif
        enddo
      endif
      if (nfcotyp(i).eq.1) then
!
!  Re-scale fconadd and fconco
!
        fca = fconadd(i)
        fco = fconco(i)
        fcp = fconpower(i)
        fca = fca*scalenff
        fco = fco*(scalenff/scalenfv**fcp)
        write(iout,'(2(i4,1x),3(g14.8,1x))') nfv,nff,fco,fca,fcp
      else
        nfcpv2 = nftyp(nint(fconadd(i)))
        nfv2 = nfcvar(i)
        if (nfcpv2.ge.3.and.nfcpv2.le.4) then
          if (nfcpv2.eq.4) then
            do j = nfv2+1,nfit
              if (nftyp(j).eq.2) then
                nfv2 = nfv2 + 1
              elseif (nftyp(j).eq.3) then
                nfv2 = nfv2 + 1
              endif
            enddo
          else
            do j = 1,nfv2-1
              if (nftyp(j).eq.4) then
                nfv2 = nfv2 - 1
              endif
            enddo
            do j = nfv2+1,nfit
              if (nftyp(j).eq.2) then
                nfv2 = nfv2 + 1
              endif
            enddo
          endif
        elseif (nfcpv2.eq.2) then
          do j = 1,nfv2-1
            if (nftyp(j).ge.3.and.nftyp(j).le.4) then
              nfv2 = nfv2 - 1
            endif
          enddo
        endif
        write(iout,'(2(i4,1x),''mean '',i4,1x,g14.8)') nfv,nfv2,nff,fconco(i)
      endif
    enddo
    write(iout,'(''end'')')
  endif
!**************************
!  Cell multipole method  *
!**************************
  if (icmm.gt.0) then
    if (rbox.ne.6.0_dp) then
      if (icmm.eq.1) then
        write(iout,'(''cmm monopole '',f6.3)') rbox
      elseif (icmm.eq.2) then
        write(iout,'(''cmm dipole '',f6.3)') rbox
      elseif (icmm.eq.3) then
        write(iout,'(''cmm quadpole '',f6.3)') rbox
      elseif (icmm.eq.4) then
        write(iout,'(''cmm octopole '',f6.3)') rbox
      endif
    else
      if (icmm.eq.1) then
        write(iout,'(''cmm monopole '')')
      elseif (icmm.eq.2) then
        write(iout,'(''cmm dipole '')')
      elseif (icmm.eq.3) then
        write(iout,'(''cmm quadpole '')')
      elseif (icmm.eq.4) then
        write(iout,'(''cmm octopole '')')
      endif
    endif
  endif
!*********************************
!  Genetic Algorithm parameters  *
!*********************************
  loutok = .true.
  if (prob(1).ne.0.8_dp) then
    if (loutok) then
      write(iout,'(''genetic '')')
      loutok = .false.
    endif
    write(iout,'(''tournament '',f8.6)') prob(1)
  endif
  if (prob(4).ne.0.4_dp) then
    if (loutok) then
      write(iout,'(''genetic '')')
      loutok = .false.
    endif
    write(iout,'(''crossover  '',f8.6)') prob(4)
  endif
  if (nfit.gt.0) then
    if (prob(7).ne.-1.0_dp) then
      if (loutok) then
        write(iout,'(''genetic '')')
        loutok = .false.
      endif
      write(iout,'(''mutation   '',f8.6)') prob(7)
    endif
  endif
  if (ngacfg.ne.10) then
    if (loutok) then
      write(iout,'(''genetic '')')
      loutok = .false.
    endif
    write(iout,'(''configurations '',i6)') ngacfg
  endif
  if (ngabest.ne.2) then
    if (loutok) then
      write(iout,'(''genetic '')')
      loutok = .false.
    endif
    write(iout,'(''best '',i6)') ngabest
  endif
  nga = 0
  do i = 1,nfit
    if (ndiscret(i).ne.6) then
      nga = nga + 1
      itmp(nga) = i
    endif
  enddo
  if (nga.gt.0) then
    if (loutok) then
      write(iout,'(''genetic '')')
      loutok = .false.
    endif
    write(iout,'(''discretisation '',i4)') nga
    write(iout,'(18i4)')(itmp(i),i=1,nga)
    write(iout,'(18i4)')(ndiscret(itmp(i)),i=1,nga)
  endif
  nga = 0
  do i = 1,nfit
    if (xmin(i).ne.0.0_dp) then
      nga = nga + 1
      itmp(nga) = i
    endif
  enddo
  if (nga.gt.0) then
    if (loutok) then
      write(iout,'(''genetic '')')
      loutok = .false.
    endif
    write(iout,'(''minimum '',i4)') nga
    write(iout,'(18i4)')(itmp(i),i=1,nga)
    write(iout,'(6(f12.6,1x))')(xmin(itmp(i)),i=1,nga)
  endif
  nga = 0
  do i = 1,nfit
    if (xmax(i).ne.0.0_dp) then
      nga = nga + 1
      itmp(nga) = i
    endif
  enddo
  if (nga.gt.0) then
    if (loutok) then
      write(iout,'(''genetic '')')
      loutok = .false.
    endif
    write(iout,'(''maximum '',i4)') nga
    write(iout,'(18i4)')(itmp(i),i=1,nga)
    write(iout,'(6(f12.6,1x))')(xmax(itmp(i)),i=1,nga)
  endif
  if (.not.loutok) write(iout,'(''end'')')
!*******************
!  NEB parameters  *
!*******************
  if (nnebiter.ne.1000) then
    write(iout,'(''nebiterations '',i12)') nnebiter
  endif
  if (nebtangent.ne.3) then
    write(iout,'(''nebtangent    '',i12)') nebtangent
  endif
  if (nebmaxdisp.ne.0.1_dp) then
    write(iout,'(''nebmaxdisp    '',f12.8)') nebmaxdisp
  endif
  if (nebrandom.ne.0.0_dp) then
    write(iout,'(''nebrandom     '',f12.8)') nebrandom
  endif
  if (nebtol.ne.1.0d-4) then
    write(iout,'(''nebtolerance  '',f12.8)') nebtol
  endif
!***********************************
!  Synchronous transit parameters  *
!***********************************
  if (maxsynciter.ne.1000) then
    write(iout,'(''synciterations '',i12)') maxsynciter
  endif
  if (maxsyncstep.ne.1000) then
    write(iout,'(''syncsteps '',i12)') maxsyncstep
  endif
  if (synctol.ne.1.0d-4) then
    write(iout,'(''synctolerance  '',f12.8)') synctol
  endif
!**********************
!  Element parameters *
!**********************
  ldiff = .false.
  do na = 1,maxele
    if (atsymin(na).ne.atsym(na)) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      write(iout,'(''symbol   '',i3,1x,a2)') na,atsym(na)
    endif
    if (abs(atmassin(na)-atmass(na)).gt.1.0d-8) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      write(iout,'(''mass     '',a2,1x,f10.5)') atsym(na),atmass(na)
    endif
    if (abs(rionin(na)-rion(na)).gt.1.0d-8) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      write(iout,'(''ionic    '',a2,1x,f10.5)') atsym(na),rion(na)
    endif
    if (abs(rcovin(na)-rcov(na)).gt.1.0d-8) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      write(iout,'(''covalent '',a2,1x,f10.5)') atsym(na),rcov(na)
    endif
    if (abs(rvdwin(na)-rvdw(na)).gt.1.0d-8) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      write(iout,'(''vdw      '',a2,1x,f10.5)') atsym(na),rvdw(na)
    endif
    if (bbarin(na).ne.bbar(na)) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      write(iout,'(''bbar     '',a2,1x,f10.5)') atsym(na),bbar(na)
    endif
    if (sigincin(na).ne.siginc(na)) then 
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      write(iout,'(''siginc   '',a2,1x,e10.4)') atsym(na),siginc(na)*1e-8
    endif
  enddo
!
!  Species specific masses
!
  do i = 1,nspec
    if (lmassinspec(i)) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      call label(natspec(i),ntypspec(i),lab1)
      write(iout,'(''mass '',a5,1x,f7.3)') lab1,massspec(i)
    endif
  enddo
!
!  Species specific NMR parameters
!
  do i = 1,nspec
    if (lnmrinspec(i)) then
      if (.not.ldiff) then
        write(iout,'(''element'')')
        ldiff = .true.
      endif
      call label(natspec(i),ntypspec(i),lab1)
      write(iout,'(''nmr '',a5,1x,f10.4,1x,f10.4,1x,f7.5)') lab1,(nmrspec(j,i),j=1,3)
    endif
  enddo
  if (ldiff) write(iout,'(''end'')')
!*********************************
!  Electronegativity parameters  *
!*********************************
  ldiff = .false.
  if (lqeq) then
    do i = 1,maxele
      if (leemparaltered(i)) then
        if (.not.ldiff) then
          write(iout,'(''qelectronegativity'')')
          ldiff = .true.
        endif
!
!  Set fitting flags if needed
!
        if (lfit) then
          n1 = 0
          n2 = 0
          n3 = 0
          do ni = 1,nfit      
            nt = nftyp(ni)
            if (nt.eq.1) then
              np = nfpot(ni)                                 
              if (np.eq.13.and.i.eq.nfvar(ni)) then
                n1 = 1
              elseif (np.eq.14.and.i.eq.nfvar(ni)) then
                n2 = 1 
              elseif (np.eq.19.and.i.eq.nfvar(ni)) then            
                n3 = 1 
              endif
            endif
          enddo
          if (abs(qeqq0(i)).gt.1.0d-12) then
            write(iout,'(a2,1x,f9.5,1x,f9.5,f8.5,1x,f8.5,3i2)') atsym(i),qeqchi(i),qeqmu(i),qeqrad(i),qeqq0(i),n1,n2,n3
          else
            write(iout,'(a2,1x,f9.5,1x,f9.5,f8.5,3i2)') atsym(i),qeqchi(i),qeqmu(i),qeqrad(i),n1,n2,n3
          endif
        else
          if (abs(qeqq0(i)).gt.1.0d-12) then
            write(iout,'(a2,1x,f9.5,1x,f9.5,f8.5,1x,f8.5)') atsym(i),qeqchi(i),qeqmu(i),qeqrad(i),qeqq0(i)
          else
            write(iout,'(a2,1x,f9.5,1x,f9.5,f8.5)') atsym(i),qeqchi(i),qeqmu(i),qeqrad(i)
          endif
        endif
      endif
    enddo
  elseif (lSandM) then
    do i = 1,maxele
      if (leemparaltered(i)) then
        if (.not.ldiff) then
          write(iout,'(''smelectronegativity'')')
          ldiff = .true.
        endif
!
!  Set fitting flags if needed
!
        if (lfit) then
          n1 = 0
          n2 = 0
          n3 = 0
          n4 = 0
          do ni = 1,nfit                                   
            nt = nftyp(ni)                                 
            if (nt.eq.1) then                 
              np = nfpot(ni)                                 
              if (np.eq.15.and.i.eq.nfvar(ni)) then                 
                n1 = 1
              elseif (np.eq.16.and.i.eq.nfvar(ni)) then                 
                n2 = 1
              elseif (np.eq.17.and.i.eq.nfvar(ni)) then                 
                n3 = 1
              elseif (np.eq.18.and.i.eq.nfvar(ni)) then                 
                n4 = 1
              endif
            endif
          enddo
          if (abs(smq0(i)).gt.1.0d-12) then
            write(iout,'(a2,1x,3(f9.5,1x),f9.5,1x,f8.5,4i2)') atsym(i),smchi(i),smmu(i),smzeta(i),smZnuc(i),smq0(i),n1,n2,n3,n4
          else
            write(iout,'(a2,1x,3(f9.5,1x),f9.5,4i2)') atsym(i),smchi(i),smmu(i),smzeta(i),smZnuc(i),n1,n2,n3,n4
          endif
        else
          if (abs(smq0(i)).gt.1.0d-12) then
            write(iout,'(a2,1x,3(f9.5,1x),f9.5,1x,f8.5)') atsym(i),smchi(i),smmu(i),smzeta(i),smZnuc(i),smq0(i)
          else
            write(iout,'(a2,1x,3(f9.5,1x),f9.5)') atsym(i),smchi(i),smmu(i),smzeta(i),smZnuc(i)
          endif
        endif
      endif
    enddo
  else
    do i = 1,maxele
      if (leemparaltered(i)) then
        if (.not.ldiff) then
          write(iout,'(''electronegativity'')')
          ldiff = .true.
        endif
!
!  Set fitting flags if needed
!
        if (lfit) then
          n1 = 0
          n2 = 0
          do ni = 1,nfit
            nt = nftyp(ni)
            if (nt.eq.1) then
              np = nfpot(ni)
              if (np.eq.11.and.i.eq.nfvar(ni)) then
                n1 = 1
              elseif (np.eq.12.and.i.eq.nfvar(ni)) then
                n2 = 1
              endif
            endif
          enddo
          if (abs(q0(i)).gt.1.0d-12) then
            write(iout,'(a2,1x,f9.5,1x,f9.5,1x,f8.5,2i2)') atsym(i),chi(i),rmu(i),q0(i),n1,n2
          else
            write(iout,'(a2,1x,f9.5,1x,f9.5,2i2)') atsym(i),chi(i),rmu(i),n1,n2
          endif
        else
          if (abs(q0(i)).gt.1.0d-12) then
            write(iout,'(a2,1x,f9.5,1x,f9.5,1x,f8.5)') atsym(i),chi(i),rmu(i),q0(i)
          else
            write(iout,'(a2,1x,f9.5,1x,f9.5)') atsym(i),chi(i),rmu(i)
          endif
        endif
      endif
    enddo
  endif
!******************************
!  Bond exclusion directives  *
!******************************
  if (nnobo.gt.0) then
    do i = 1,nnobo
      nvar1 = nobond(i)/1000
      nvar2 = nobond(i) - 1000*nvar1
      if (nvar1.gt.maxele) then
        lab1 = 'shel'
        nvar1 = nvar1 - maxele
      else
        lab1 = 'core'
      endif
      if (nvar2.gt.maxele) then
        lab2 = 'shel'
        nvar2 = nvar2 - maxele
      else
        lab2 = 'core'
      endif
      itype1 = nobotyp(i)/1000
      itype2 = nobotyp(i) - 1000*itype1
      call label(nvar1,itype1,lab3)
      call label(nvar2,itype2,lab4)
      write(iout,'(''nobond '',2(a5,1x,a4,1x))') lab3,lab1,lab4,lab2
    enddo
  endif
!**********
!  Units  *
!**********
  if (abs(kcaltoev-4.3364432032d-2).gt.1.0d-8) then
    write(iout,'(''kcaltoev '',f18.14)') kcaltoev
  endif
  if (abs(angstoev-14.3997584_dp).gt.1.0d-7) then
    write(iout,'(''angstoev '',f18.14)') angstoev
  endif
!***************************
!  RFO control parameters  *
!***************************
  if (mode.gt.0) then
    write(iout,'(''maximise mode  '',i4)') mode
  elseif (morder.gt.0) then
    write(iout,'(''maximise order '',i4)') morder
  endif
!***********************
!  LM-BFGS parameters  *
!***********************
  if (lmbfgsorder.ne.10) then
    write(iout,'(''lbfgs_order '',i4)') lmbfgsorder
  endif
!***************************
!  Defect mode parameters  *
!***************************
  if (lmodeset) then
    write(iout,'(''mode2a '',i4)') mode2a
  endif
!***************************
!  Monte Carlo parameters  *
!***************************
  if (dmaxmc.ne.0.05_dp) then
    if (ntargetfreq.gt.0) then
      write(iout,'(''mcmaxdisplacement '',f13.8,'' target '',f6.4,1x,i4)') dmaxmc,targetmove,ntargetfreq
    else
      write(iout,'(''mcmaxdisplacement '',f13.8)') dmaxmc
    endif
  endif
  if (rmaxmc.ne.1.0_dp) then
    if (ntargetfreqr.gt.0) then
      write(iout,'(''mcmaxrotation '',f13.8,'' target '',f6.4,1x,i4)') rmaxmc*180.0_dp,targetrota,ntargetfreqr
    else
      write(iout,'(''mcmaxrotation '',f13.8)') rmaxmc*180.0_dp
    endif
  endif
  if (smaxmc.ne.0.1_dp) then
    if (ntargetfreqs.gt.0) then
      write(iout,'(''mcmaxstrain '',f13.8,'' target '',f6.4,1x,i4)') smaxmc,targetstrain,ntargetfreqs
    else
      write(iout,'(''mcmaxstrain '',f13.8)') smaxmc
    endif
  endif
  if (pcreate.ne.0.0_dp) then
    write(iout,'(''mccreation        '',f13.8)') pcreate
  endif
  if (pdestroy.ne.0.0_dp) then
    write(iout,'(''mcdestruction     '',f13.8)') pdestroy
  endif
  if (pmove.ne.1.0_dp) then
    write(iout,'(''mcmove            '',f13.8)') pmove
  endif
  if (protate.ne.0.0_dp) then
    if (nrotationtype.gt.0) then
      do i = 1,nrotationtype
        if (nptrrotationtype(i).eq.1) then
          mcword(i) = 'centre'
        elseif (nptrrotationtype(i).eq.2) then
          mcword(i) = 'atom'
        elseif (nptrrotationtype(i).eq.3) then
          mcword(i) = 'line'
        endif
      enddo
      write(iout,'(''mcrotation '',f13.8,3(1x,a6))') protate,(mcword(i),i=1,nrotationtype)
    else
      write(iout,'(''mcrotation        '',f12.8)') protate
    endif
  endif
  if (pswap.ne.0.0_dp) then
    if (lmcswapany) then
      write(iout,'(''mcswap     any    '',f13.8)') pswap
    else
      nsymbolout = min(6_i4,nmcswapspec)
      do i = 1,nsymbolout
        call label(nmcswapnat(i),nmcswaptype(i),labels(i))
      enddo
      if (nmcswapspec.gt.6) then
        write(iout,'(''mcswap     only   '',f13.8,6(1x,a5),'' &'')') pswap,(labels(j),j=1,nsymbolout)
        nbatch = (nmcswapspec - 7)/6 + 1
        nbatch1 = 6
        do i = 1,nbatch
          nsymbolout = min(11_i4,nmcswapspec-nbatch1)
          do j = 1,nsymbolout
            call label(nmcswapnat(nbatch1+j),nmcswaptype(nbatch1+j),labels(j))
          enddo
          write(iout,'(7x,11(1x,a5))') (labels(j),j=1,nsymbolout)
          nbatch1 = nbatch1 + 6
        enddo
      else
        write(iout,'(''mcswap     only   '',f13.8,6(1x,a5))') pswap,(labels(j),j=1,nsymbolout)
      endif
    endif
  endif
  if (pstrain.ne.0.0_dp) then
    write(iout,'(''mcstrain          '',f12.8)') pstrain
  endif
  if (nmctrial.ne.0) then
    write(iout,'(''mctrials          '',i12)')nmctrial
  endif
  if (nmcstep.ne.1) then
    write(iout,'(''mcstep            '',i12,1x,i12)')nmcstep,nmcaccepted
  endif
  if (nmcoutfreq.ne.100) then
    write(iout,'(''mcoutfrequency    '',i12)') nmcoutfreq
  endif
  if (nmcstep.ne.1) then
    write(iout,'(''mcmeans  '',f15.6,1x,f15.6)') mcemean,mcnmean
    write(iout,'(''mclowest '',f15.6,1x,f15.6)') mcelowest
  endif
  if (nmcsample.ne.10.or.mcfile.ne.' ') then
    if (nmcsample.ne.10.and.mcfile.ne.' ') then
      if (lmclowestwrite) then
        write(iout,'(''mcsample '',i5,1x,a60,'' lowest'')') nmcsample,mcfile(1:60)
      else
        write(iout,'(''mcsample '',i5,1x,a60)') nmcsample,mcfile(1:60)
      endif
    elseif (nmcsample.ne.10) then
      if (lmclowestwrite) then
        write(iout,'(''mcsample '',i5,'' lowest'')') nmcsample
      else
        write(iout,'(''mcsample '',i5)') nmcsample
      endif
    else
      if (lmclowestwrite) then
        write(iout,'(''mcsample '',a60,'' lowest'')') mcfile(1:60)
      else
        write(iout,'(''mcsample '',a60)') mcfile(1:60)
      endif
    endif
  endif
  if (chempot.ne.0.0_dp) then
    write(iout,'(''mcchemicalpotential '',f24.8)') chempot
  endif
  if (mcvolume.ne.0.0_dp) then
    write(iout,'(''mcvolume            '',f24.8)') mcvolume
  endif
  if (ngcmcspec.ne.0) then
    write(iout,'(''gcmcspecies '',i6)') ngcmcspec
    do i = 1,ngcmcspec
      nati = ngcmcnat(i)
      ntypi = ngcmctype(i)
      call label(nati,ntypi,lab1)
      if (nati.gt.maxele) then
        write(iout,'(a5,2x,''shel'')') lab1
      else
        write(iout,'(a5,2x,''core'')') lab1
      endif
    enddo
  endif
  if (ngcmcmol.ne.0) then
    do m = 1,ngcmcmol
      write(iout,'(''gcmcmolecule '',i6)')ngcmcmolat(m)
      do i = 1,ngcmcmolat(m)
        nati = ngcmcmolnat(i,m)
        ntypi = ngcmcmoltype(i,m)
        call label(nati,ntypi,lab1)
        if (nati.gt.maxele) then
          write(iout,'(a5,2x,''shel'',3(1x,f12.5))')lab1,xgcmcmol(i,m),ygcmcmol(i,m),zgcmcmol(i,m)
        else
          write(iout,'(a5,2x,''core'',3(1x,f12.5))')lab1,xgcmcmol(i,m),ygcmcmol(i,m),zgcmcmol(i,m)
        endif
      enddo
    enddo
  endif
!******************************
!  General control parameters *
!******************************
  if (accuracy.ne.12.0.or.nemorder.ne.4.or.nmaxcells.ne.20) then
    write(iout,'(''accuracy '',f6.3,2(1x,i2))') accuracy,nemorder,nmaxcells
  endif
  if (timmax.gt.0.0) then
    write(iout,'(''time '',f12.1)') timmax
  endif
  if (lbrenner) then
    if (.not.lbrennersplinef.and..not.lbrennersplineh) then
      write(iout,'(''brenner '',i1,'' nospline fh'')') nbrennertype
    elseif (.not.lbrennersplinef) then
      write(iout,'(''brenner '',i1,'' nospline f'')') nbrennertype
    elseif (.not.lbrennersplineh) then
      write(iout,'(''brenner '',i1,'' nospline h'')') nbrennertype
    else
      write(iout,'(''brenner '',i1)') nbrennertype
    endif
  endif
  if (npote.gt.0.and.(cutp.ne.100000.0_dp.or.tapermax.ne.0.0_dp.or.tapertype.ne.1)) then
    if (tapertype.eq.1) then
      write(iout,'(''cutp '',f12.5,1x,f10.5)') cutp,(tapermax-tapermin)
    elseif (tapertype.eq.3) then
      write(iout,'(''cutp '',f12.5,'' voter '',f10.5)') cutp,taperm
    elseif (tapertype.eq.4) then
      write(iout,'(''cutp '',f12.5,'' exponential '')') cutp
    elseif (tapertype.eq.5) then
      write(iout,'(''cutp '',f12.5,'' mdf '',f10.5)') cutp,(tapermax-tapermin)
    else
      write(iout,'(''cutp '',f12.5,'' cosine '',f10.5)') cutp,(tapermax-tapermin)
    endif
  endif
  if (lrcspatial_anisotropic.or.lrcspatialBO_anisotropic) then
    if ((rcspatialx+rcspatialy+rcspatialz).ne.0.0_dp) then
      if ((rcspatialbox+rcspatialboy+rcspatialboz).ne.0.0_dp) then
        write(iout,'(''rcspatial aniso '',3(f12.5,1x),''&'')') rcspatialx,rcspatialy,rcspatialz
        write(iout,'(16x,3(f12.5,1x))') rcspatialbox,rcspatialboy,rcspatialboz
      else
        write(iout,'(''rcspatial aniso '',3(f12.5,1x))') rcspatialx,rcspatialy,rcspatialz
      endif
    endif
  elseif (rcspatial.ne.0.0_dp.or.rcspatialbo.ne.0.0_dp) then
    write(iout,'(''rcspatial '',f12.5,1x,f12.5)') rcspatial,rcspatialbo
  endif
  if (lpotlinterpolate) then
    write(iout,'(''potential_interpolate '',i12)') nptsinterpolate
  endif
  if (cutb.ne.2.0_dp) then
    write(iout,'(''cutd '',f8.4)') cutb
  endif
  if (cuts.ne.0.8_dp) then
    write(iout,'(''cuts '',f8.6)') cuts
  endif
  if (extracutoff.ne.0.0_dp) then
    write(iout,'(''extracutoff '',f10.6)') extracutoff
  endif
  if (cellmin.ne.0.5_dp) then
    write(iout,'(''mincell '',f12.6)') cellmin
  endif
  if (scmaxsearch.ne.2.0_dp) then
    write(iout,'(''scmaxsearch '',f8.6)') scmaxsearch
  endif
  if (nmdintegrator.ne.4) then
    if (nmdintegrator.eq.1) then
      write(iout,'(''integrator gear'')')
    elseif (nmdintegrator.eq.2) then
      write(iout,'(''integrator velocity verlet'')')
    elseif (nmdintegrator.eq.3) then
      if (nmditer.ne.3) then
        write(iout,'(''integrator leapfrog verlet'',1x,i3)') nmditer
      else
        write(iout,'(''integrator leapfrog verlet'')')
      endif
    endif
  endif
  if (nrandomcalls.ne.0.or.npr_randomcalls.ne.0) then
    if (lGaussianLast) then
      write(iout,'(''random '',3(i12,1x),''G'')') nrandomcalls,npr_randomcalls,npr_grandomcalls
    else
      write(iout,'(''random '',3(i12,1x),''S'')') nrandomcalls,npr_randomcalls,npr_grandomcalls
    endif
  endif
  if (rmdmaxtemp.ne.100.0_dp) then
    write(iout,'(''mdmaxtemp   '',f12.4)') rmdmaxtemp
  endif
  if (rmdmaxvol.ne.100.0_dp) then
    write(iout,'(''mdmaxvolume '',f12.4)') rmdmaxvol
  endif
  if (xtol.ne.0.00001_dp) then
    xtol = abs(xtol)
    xt = - log10(xtol)
    write(iout,'(''xtol opt '',f10.6)') xt
  endif
  if (grmax.ne.0.01_dp) then
    grmax = abs(grmax)
    gt = - log10(grmax)
    write(iout,'(''gmax opt '',f10.6)') gt
  endif
  if (gtol.ne.0.001_dp) then
    gtol = abs(gtol)
    gt = - log10(gtol)
    write(iout,'(''gtol opt '',f10.6)') gt
  endif
  if (ftol.ne.0.00001_dp) then
    ftol = abs(ftol)
    ft = - log10(ftol)
    write(iout,'(''ftol opt '',f10.6)') ft
  endif
  if (gdcrit.ne.4.0_dp) then
    write(iout,'(''gdcrit '',f10.6)') gdcrit
  endif
  if (ldefect) then
    if (stepmax.ne.0.3) then
      write(iout,'(''stepmx opt '',f12.6)') stepmax
    endif
  else
    if (stepmax.ne.1.0) then
      write(iout,'(''stepmx opt '',f12.6)') stepmax
    endif
  endif
  if (maxcal.ne.1000) then
    write(iout,'(''maxcyc opt '',i8)') maxcal
  endif
  if (lowerscale.ne.0.001_dp) then
    write(iout,'(''slower '',f12.8)') lowerscale
  endif
  if (moptit.ne.9) then
    if (sgtol.ne.1.0d-10) then
      sgtol = abs(sgtol)
      sgt = - log10(sgtol)
      if (lextrapolateshells) then
        write(iout,'(''iterations '',i3,1x,f10.6,i3)') moptit+1,sgt,maxextrapol
      else
        write(iout,'(''iterations '',i3,1x,f10.6,'' noextrapolate'')') moptit+1,sgt
      endif
    else
      if (lextrapolateshells) then
        write(iout,'(''iterations '',i3,i3)') moptit+1,maxextrapol
      else
        write(iout,'(''iterations '',i3,'' noextrapolate'')') moptit+1
      endif
    endif
  elseif (sgtol.ne.1.0d-10) then
    sgtol = abs(sgtol)
    sgt = - log10(sgtol)
    if (lextrapolateshells) then
      write(iout,'(''iterations '',i3,1x,f10.6,i3)') moptit+1,sgt,maxextrapol
    else
      write(iout,'(''iterations '',i3,1x,f10.6,'' noextrapolate'')') moptit+1,sgt
    endif
  elseif (.not.lextrapolateshells) then
    write(iout,'(''iterations '',i3,1x,'' noextrapolate'')') moptit+1
  elseif (maxextrapol.ne.1) then
    write(iout,'(''iterations '',i3,1x,i3)') maxextrapol
  endif
  if (nratiomspec.gt.0) then
    write(iout,'(''shellmass '')') 
    do i = 1,nratiomspec
      nati = natratiomspec(i)
      ntypi = ntypratiomspec(i)
      call label(nati,ntypi,lab1)
      write(iout,'(a5,1x,f10.6)') lab1,ratiomspec(i)
    enddo
  endif
  if (fxtol.ne.0.00001_dp) then
    fxtol = abs(fxtol)
    xt = - log10(fxtol)
    write(iout,'(''xtol fit '',f8.4)') xt
  endif
  if (fftol.ne.0.00001_dp) then
    fftol = abs(fftol)
    ft = - log10(fftol)
    write(iout,'(''ftol fit '',f8.4)') ft
  endif
  if (fgmax.ne.0.001_dp) then
    fgmax = abs(fgmax)
    gt = - log10(fgmax)
    write(iout,'(''gmax fit '',f8.4)') gt
  endif
  if (fgtol.ne.0.0001_dp) then
    fgtol = abs(fgtol)
    gt = - log10(fgtol)
    write(iout,'(''gtol fit '',f8.4)') gt
  endif
  if (fstepmx.ne.1000.0) then
    write(iout,'(''stepmx fit '',f12.6)') fstepmx
  endif
  if (maxfcal.ne.5000) then
    write(iout,'(''maxcyc fit '',i8)') maxfcal
  endif
  if (delta.ne.0.0001_dp) then
    write(iout,'(''delta fit '',f9.7)') delta
  endif
  if (ncycp.ne.1000) then
    write(iout,'(''print '',i4)') ncycp
  endif
  if (nlinmin.ne.8) then
    write(iout,'(''line '',i4)') nlinmin
  endif
  if (delfc.ne.10.0_dp) then
    write(iout,'(''delf '',f7.2)') delfc
  endif
  if (morder.gt.0) then
    if (nupdate.ne.1) then
      write(iout,'(''update opt '',i4)') nupdate
    endif
  elseif (index(keyword,'unit').ne.0) then
    if (nfupdate.ne.100.and.lfit) then
      write(iout,'(''update fit '',i4)') nfupdate
    endif
  else
    if (nupdate.ne.10) then
      write(iout,'(''update opt '',i4)') nupdate
    endif
    if (nfupdate.ne.20) then
      write(iout,'(''update fit '',i4)') nfupdate
    endif
  endif
  if (lminch) then
    if (minchcrit.eq.1) then
      ichcrit = nint(chcrit)
      write(iout,'(''switch_min '',a5,'' cycle '',i4)') minword(mintype),ichcrit
    else
      write(iout,'(''switch_min '',a5,'' gnorm '',f12.6)') minword(mintype),chcrit
    endif
  endif
  if (lfinitediff) then
    write(iout,'(''finite  '',g16.8)') findiff
  endif
  if (phondiff.ne.1.0d-5) then
    write(iout,'(''pfinite '',g16.8)') phondiff
  endif
  if (findiffc.ne.1.0d-5.or.findiffs.ne.1.0d-5) then
    write(iout,'(''sfinite '',g16.8,1x,g16.8)') findiffc,findiffs
  endif
  if (nadd.ne.0) then
    write(iout,'(''nadd '',i4)') nadd
  endif
  if (lwolf) then
    if (lwolforiginal) then
      write(iout,'(''qwolf original '',f10.6,1x,f10.6)') etaw,cutw
    elseif (lwolffennell) then
      write(iout,'(''qwolf fennell '',f10.6,1x,f10.6)') etaw,cutw
    else
      write(iout,'(''qwolf '',f10.6,1x,f10.6)') etaw,cutw
    endif
  endif
  if (rspeed0.ne.1.0_dp) then
    write(iout,'(''rspeed'',1x,g10.5)') rspeed0
  endif
  if (targetrradmax.gt.0.0_dp) then
    write(iout,'(''ewaldrealradius'',1x,f12.4)') targetrradmax
  endif
  if (rtol.ne.1.2_dp) then
    write(iout,'(''rtol '',f8.4)') rtol
  endif
  if (bfactor.ne.0.2_dp) then
    write(iout,'(''broaden '',f10.4)') bfactor
  endif
!
!  QEq parameters
!
  if (qeqscfcrit.ne.0.000001_dp) then
    write(iout,'(''qeqtol '',f10.8)') qeqscfcrit
  endif
  if (nqeqitermax.ne.20) then
    write(iout,'(''qeqiter '',i5)') nqeqitermax
  endif
  if (rqeq.ne.15.0_dp) then
    write(iout,'(''qeqradius '',f8.4)') rqeq
  endif
!
!  Gasteiger parameters
!
  if (gasttol.ne.0.001_dp) then
    write(iout,'(''gasttol '',f10.8)') gasttol
  endif
  if (ngastitermax.ne.20) then
    write(iout,'(''gastiter '',i5)') ngastitermax
  endif
!
!  COSMO parameters
!
  if (isasatomoption.ne.1) then
    if (isasatomoption.eq.2) then
      write(iout,'(''sasparticle both_cores_and_shells'')')
    endif
  endif
  if (ldodeca) then
    write(iout,'(''cosmoshape dodecahedron'')')
    if (nppa.ne.92) then
      write(iout,'(''pointsperatom  '',i10)') nppa
    endif
    if (nspa.ne.92.or.nspah.ne.nspa) then
      write(iout,'(''segmentsperatom '',2(1x,i10))') nspa,nspah
    endif
  else
    if (nppa.ne.110) then
      write(iout,'(''pointsperatom  '',i10)')nppa
    endif
    if (nspa.ne.110.or.nspah.ne.nspa) then
      write(iout,'(''segmentsperatom '',2(1x,i10))') nspa,nspah
    endif
  endif
  if (cosmormax.ne.10.0_dp.or.cosmormaxs.ne.1.0_dp) then
    write(iout,'(''solventrmax    '',f10.6,1x,f10.6)') cosmormax,cosmormaxs
  endif
  if (cosmorange.ne.0.0_dp) then
    write(iout,'(''rangeforsmooth '',f10.6)') cosmorange
  endif
  if (etawc.ne.0.05_dp.or.cutwc.ne.20.0_dp) then
    write(iout,'(''cwolf '',f10.6,1x,f10.6)') etawc,cutwc
  endif
!********************
!  UFF bond orders  *
!********************
  if (UFFbondorder(2).ne.2.0_dp) then
    write(iout,'(''uff_bondorder double    '',f12.6)') UFFbondorder(2)
  endif
  if (UFFbondorder(3).ne.3.0_dp) then
    write(iout,'(''uff_bondorder triple    '',f12.6)') UFFbondorder(3)
  endif
  if (UFFbondorder(4).ne.4.0_dp) then
    write(iout,'(''uff_bondorder quadruple '',f12.6)') UFFbondorder(4)
  endif
  if (UFFbondorder(5).ne.1.5_dp) then
    write(iout,'(''uff_bondorder resonant  '',f12.6)') UFFbondorder(5)
  endif
  if (UFFbondorder(6).ne.1.41_dp) then
    write(iout,'(''uff_bondorder amide     '',f12.6)') UFFbondorder(6)
  endif
  if (UFFbondorder(7).ne.1.0_dp) then
    write(iout,'(''uff_bondorder custom    '',f12.6)') UFFbondorder(7)
  endif
!******************
!  GULP dumpfile  *
!******************
  if (idump.eq.12) then
    if (dfile(1:1).ne.' ') then
      if (ncycd.ne.1000) then
        if (ldumpcart) then
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'',1x,''cart'',1x,a60)') ncycd,dfile(1:60)
          else
            write(iout,'(''dump every '',i3,1x,''cart'',1x,a60)') ncycd,dfile(1:60)
          endif
        else
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'',1x,a60)') ncycd,dfile(1:60)
          else
            write(iout,'(''dump every '',i6,1x,a60)') ncycd,dfile(1:60)
          endif
        endif
      else
        if (ldumpcart) then
          write(iout,'(''dump cart '',a60)') dfile(1:60)
        else
          write(iout,'(''dump '',a60)') dfile(1:60)
        endif
      endif
    else
      if (ncycd.ne.1000) then
        if (ldumpcart) then
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'','' cart'')') ncycd
          else
            write(iout,'(''dump every '',i3,'' cart'')') ncycd
          endif
        else
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'')') ncycd
          else
            write(iout,'(''dump every '',i3)') ncycd
          endif
        endif
      else
        if (ldumpcart) then
          write(iout,'(''dump cart'')')
        else
          write(iout,'(''dump'')')
        endif
      endif
    endif
  elseif (idump.gt.0) then
    if (dfile(1:1).ne.' ') then
      if (ncycd.ne.1000) then
        if (ldumpcart) then
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'',1x,i2,1x,''cart'',1x,a60)') ncycd,idump,dfile(1:60)
          else
            write(iout,'(''dump every '',i3,1x,i2,1x,''cart'',1x,a60)') ncycd,idump,dfile(1:60)
          endif
        else
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'',1x,i2,1x,a60)') ncycd,idump,dfile(1:60)
          else
            write(iout,'(''dump every '',i3,1x,i2,1x,a60)') ncycd,idump,dfile(1:60)
          endif
        endif
      else
        if (ldumpcart) then
          write(iout,'(''dump '',i2,1x,''cart'',1x,a60)') idump,dfile(1:60)
        else
          write(iout,'(''dump '',i2,1x,a60)') idump,dfile(1:60)
        endif
      endif
    else
      if (ncycd.ne.1000) then
        if (ldumpcart) then
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'',1x,i2,1x,''cart'')') ncycd,idump
          else
            write(iout,'(''dump every '',i3,1x,i2,1x,''cart'')') ncycd,idump
          endif
        else
          if (ldumpnooverwrite) then
            write(iout,'(''dump every '',i3,'' noover'',1x,i2)') ncycd,idump
          else
            write(iout,'(''dump every '',i3,1x,i2)') ncycd,idump
          endif
        endif
      else
        if (ldumpcart) then
          write(iout,'(''dump '',i2,'' cart'')') idump
        else
          write(iout,'(''dump '',i2)') idump
        endif
      endif
    endif
  endif
!******************************
!  Output other file formats  *
!******************************
  if (lmarv) then
    if (lmarv2) then
      if (marvfile(1:1).ne.' ') then
        write(iout,'(''output marvin2 '',a60)') marvfile(1:60)
      else
        write(iout,'(''output marvin2'')')
      endif
    else
      if (marvfile(1:1).ne.' ') then
        write(iout,'(''output marvin '',a60)') marvfile(1:60)
      else
        write(iout,'(''output marvin'')')
      endif
    endif
  endif
  if (lthb) then
    if (thbfile(1:1).ne.' ') then
      write(iout,'(''output thbrel '',a60)') thbfile(1:60)
    else
      write(iout,'(''output thbrel'')')
    endif
  endif
  if (lxtl) then
    if (xtlfile(1:1).ne.' ') then
      write(iout,'(''output xtl '',a60)') xtlfile(1:60)
    else
      write(iout,'(''output xtl'')')
    endif
  endif
  if (lxr) then
    if (xrfile(1:1).ne.' ') then
      write(iout,'(''output xr '',a60)') xrfile(1:60)
    else
      write(iout,'(''output xr'')')
    endif
  endif
  if (lcssr) then
    if (cssrfile(1:1).ne.' ') then
      write(iout,'(''output cssr '',a60)') cssrfile(1:60)
    else
      write(iout,'(''output cssr'')')
    endif
  endif
  if (ltrj) then
    if (trjfile(1:1).ne.' ') then
      if (ltrjascii) then
        if (ltrjequil) then
          write(iout,'(''output trajectory ascii equil '',a60)') trjfile(1:60)
        else
          write(iout,'(''output trajectory ascii '',a60)') trjfile(1:60)
        endif
      else
        if (ltrjequil) then
          write(iout,'(''output trajectory equil '',a60)') trjfile(1:60)
        else
          write(iout,'(''output trajectory '',a60)') trjfile(1:60)
        endif
      endif
    else
      if (ltrjascii) then
        if (ltrjequil) then
          write(iout,'(''output trajectory ascii equil'')')
        else
          write(iout,'(''output trajectory ascii'')')
        endif
      else
        if (ltrjequil) then
          write(iout,'(''output trajectory equil'')')
        else
          write(iout,'(''output trajectory'')')
        endif
      endif
    endif
  endif
  if (larc) then
    if (lmovie) then
      if (loutshell) then
        if (arcfile(1:1).ne.' ') then
          if (narcwrite.ne.1) then
            write(iout,'(''output movie '',i4,'' shell arc '',a60)') narcwrite,arcfile(1:60)
          else
            write(iout,'(''output movie shell arc '',a60)') arcfile(1:60)
          endif
        else
          if (narcwrite.ne.1) then
            write(iout,'(''output movie '',i4,'' shell arc '')') narcwrite
          else
            write(iout,'(''output movie shell arc '')')
          endif
        endif
      else
        if (arcfile(1:1).ne.' ') then
          if (narcwrite.ne.1) then
            write(iout,'(''output movie '',i4,'' arc '',a60)') narcwrite,arcfile(1:60)
          else
            write(iout,'(''output movie arc '',a60)') arcfile(1:60)
          endif
        else
          if (narcwrite.ne.1) then
            write(iout,'(''output movie '',i4,'' arc '')') narcwrite
          else
            write(iout,'(''output movie arc '')')
          endif
        endif
      endif
    else
      if (loutshell) then
        if (arcfile(1:1).ne.' ') then
          write(iout,'(''output shell arc '',a60)')arcfile(1:60)
        else
          write(iout,'(''output shell arc '')')
        endif
      else
        if (arcfile(1:1).ne.' ') then
          write(iout,'(''output arc '',a60)')arcfile(1:60)
        else
          write(iout,'(''output arc '')')
        endif
      endif
    endif
  endif
  if (lphono) then
    if (phonfile(1:1).ne.' ') then
      write(iout,'(''output phonon '',a60)')phonfile(1:60)
    else
      write(iout,'(''output phonon'')')
    endif
  endif
  if (lfrq) then
    if (freqfile(1:1).ne.' ') then
      if (lfrqbin) then
        write(iout,'(''output freq binary '',a60)')freqfile(1:60)
      else
        write(iout,'(''output freq text '',a60)')freqfile(1:60)
      endif
    else
      if (lfrqbin) then
        write(iout,'(''output freq binary'')')
      else
        write(iout,'(''output freq text'')')
      endif
    endif
  endif
  if (lxyz) then
    if (lxyzmovie) then
      if (xyzfile(1:1).ne.' ') then
        write(iout,'(''output movie xyz '',a60)')xyzfile(1:60)
      else
        write(iout,'(''output movie xyz '')')
      endif
    else
      if (xyzfile(1:1).ne.' ') then
        write(iout,'(''output xyz '',a60)')xyzfile(1:60)
      else
        write(iout,'(''output xyz '')')
      endif
    endif
  endif
  if (lhis) then
    if (hisfile(1:1).ne.' ') then
      write(iout,'(''output history '',a60)')hisfile(1:60)
    else
      write(iout,'(''output history'')')
    endif
  endif
  if (lfdf) then
    if (fdffile(1:1).ne.' ') then
      write(iout,'(''output fdf '',a60)')fdffile(1:60)
    else
      write(iout,'(''output fdf'')')
    endif
  endif
  if (lcif) then
    if (ciffile(1:1).ne.' ') then
      write(iout,'(''output cif '',a60)')ciffile(1:60)
    else
      write(iout,'(''output cif'')')
    endif
  endif
  if (ldlv) then
    if (dlvfile(1:1).ne.' ') then
      write(iout,'(''output str '',a60)')dlvfile(1:60)
    else
      write(iout,'(''output str'')')
    endif
  endif
  if (leig) then
    if (eigfile(1:1).ne.' ') then
      write(iout,'(''output eig '',a60)')eigfile(1:60)
    else
      write(iout,'(''output eig'')')
    endif
  endif
  if (ldrv) then
    if (drvfile(1:1).ne.' ') then
      write(iout,'(''output drv '',a60)')drvfile(1:60)
    else
      write(iout,'(''output drv'')')
    endif
  endif
  if (lfrc) then
    if (frcfile(1:1).ne.' ') then
      write(iout,'(''output frc '',a60)')frcfile(1:60)
    else
      write(iout,'(''output frc'')')
    endif
  endif
  if (lpre) then
    if (prefile(1:1).ne.' ') then
      write(iout,'(''output pressure '',a60)') prefile(1:60)
    else
      write(iout,'(''output pressure'')')
    endif
  endif
  if (lsas) then
    if (sasfile(1:1).ne.' ') then
      write(iout,'(''output sas '',a60)') sasfile(1:60)
    else
      write(iout,'(''output sas'')')
    endif
  endif
  if (losc) then
    if (oscfile(1:1).ne.' ') then
      write(iout,'(''output osc '',a60)') oscfile(1:60)
    else
      write(iout,'(''output osc'')')
    endif
  endif
  if (lcml) then
    if (lvcml) then
      write(iout,'(''output vcml '',a80)') cmlfilename(1:80)
    else
      write(iout,'(''output cml '',a80)') cmlfilename(1:80)
    endif
  endif
  if (lbio) then
    if (biofile(1:1).ne.' ') then
      write(iout,'(''output bio '',a60)') biofile(1:60)
    else
      write(iout,'(''output bio'')')
    endif
  endif
  if (lcosmofile) then
    if (cosmofile(1:1).ne.' ') then
      write(iout,'(''output cosmo '',a60)') cosmofile(1:60)
    else
      write(iout,'(''output cosmo'')')
    endif
  endif
  if (lqbo) then
    if (qbofile(1:1).ne.' ') then
      if (lqboappend) then
        write(iout,'(''output qbo append '',a60)') qbofile(1:60)
      else
        write(iout,'(''output qbo '',a60)') qbofile(1:60)
      endif
    else
      if (lqboappend) then
        write(iout,'(''output qbo append '')')
      else
        write(iout,'(''output qbo'')')
      endif
    endif
  endif
  if (llammps) then
    if (lammpsfile(1:1).ne.' ') then
      write(iout,'(''output lammps '',a60)') lammpsfile(1:60)
    else
      write(iout,'(''output lammps'')') 
    endif
  endif
  if (llammpspots) then
    if (lammpspotsfile(1:1).ne.' ') then
      write(iout,'(''output lammps_pots '',f8.4,1x,f8.4,1x,i8,a60)') lammps_r0,lammps_rend,nlammpspoints,lammpspotsfile(1:60)
    else
      write(iout,'(''output lammps_pots '',f8.4,1x,f8.4,1x,i8)') lammps_r0,lammps_rend,nlammpspoints
    endif
  endif
  if (ldcd) then
    if (dcdfile(1:1).ne.' ') then
      write(iout,'(''output dcd '',a60)') dcdfile(1:60)
    else
      write(iout,'(''output dcd'')')
    endif
  endif
!******************
!  Terse options  *
!******************
  if (ltersepotentials) then
    write(iout,'(''terse potentials'')')
  endif
  if (ltersepotentials) then
    write(iout,'(''terse derivatives'')')
  endif
  if (lterseincell.and.lterseoutcell.and.lterseincoords.and.lterseoutcoords) then
    write(iout,'(''terse inout structure'')')
  else
    if (lterseincell.and.lterseoutcell) then
      write(iout,'(''terse inout cell'')')
    elseif (lterseincell) then
      write(iout,'(''terse in cell'')')
    elseif (lterseoutcell) then
      write(iout,'(''terse out cell'')')
    endif
    if (lterseincoords.and.lterseoutcoords) then
      write(iout,'(''terse inout coordinates'')')
    elseif (lterseincoords) then
      write(iout,'(''terse in coordinates'')')
    elseif (lterseoutcoords) then
      write(iout,'(''terse out coordinates'')')
    endif
  endif
!*************************
!  Output Marvin insert  *
!*************************
  if (index(marvtemp,' ').gt.1) then
    write(iout,'(''marvin'')')
    open(iout+10,file=marvtemp,status='old')
    lnoerror = .false.
    do while (lnoerror)
      read(iout+10,'(a)',err=100,end=100) line
      write(iout,'(a)') line
    enddo
100 write(iout,'(''end'')')
    close(iout+10,status='delete')
  endif
!**********************
!  Output MD archive  *
!**********************
  if (mdafil(1:1).ne.' ') then
    if (lmdarcunique) then
      write(iout,'(''mdarchive unique '',a)') mdafil(1:index(mdafil,' ')-1)
    else
      write(iout,'(''mdarchive '',a)') mdafil(1:index(mdafil,' ')-1)
    endif
  endif
!**********************
!  Output Plumed info *
!**********************
  if (lplumed) then
    write(iout,'(''plumed '',a)') plumedfile(1:index(plumedfile,' ')-1)
  endif
!****************************
!  Maths library selection  *
!****************************
  if (leispack_eigensolve) then
    write(iout,'(''maths eispack'')')
  elseif (.not.ldivide_and_conquer) then
    write(iout,'(''maths lapack nodivide'')')
  endif
!
!  Free local memory
!
  deallocate(temp,stat=status)
  if (status/=0) call deallocate_error('dumpgen','temp')
  deallocate(ntemp,stat=status)
  if (status/=0) call deallocate_error('dumpgen','ntemp')
  deallocate(itmp,stat=status)
  if (status/=0) call deallocate_error('dumpgen','itmp')
  deallocate(ctmp2,stat=status)
  if (status/=0) call deallocate_error('dumpgen','ctmp2')
  deallocate(ctmp,stat=status)
  if (status/=0) call deallocate_error('dumpgen','ctmp')
!
  return
  end
