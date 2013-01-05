  subroutine setcfg
!
!  One off configuration set up
!
!  11/96 Make sure that maxd2 is no larger than 3*maxat+6 to
!        avoid wasting memory.
!   6/97 Define cutoff (costcut) for Pannetier Cost Function (SMW)
!   7/97 Don't print co-ords if called from predict{maxd2<>maxd2in}(SMW)
!   5/98 Comment out cutoff defn for costcut - common genconst (SMW)
!   5/98 Output of stress tensor added
!   8/99 Testing of angles for monoclinic case corrected -
!        only affects version 1.3
!  10/99 Bug in setting k points for multiple configurations fixed
!   7/00 lflags now in control module
!  11/00 maxd2 removed as handled elsewhere now
!  12/00 2-D modifications added
!  12/00 call to rlist added before geometry measurements for safety
!   6/01 Solvation model details now output
!   8/01 More solvation model details added to output
!   9/01 Default K point for FEM changed to a symmetric point.
!   9/01 Temperature converted to g format
!  11/01 Set up of growth slice using dhkl added and nzmol
!   8/02 Output of external forces added
!  11/02 Einstein model data output added
!  11/02 If Einstein model, then no atom is forced to be fixed.
!   5/03 Checking of memory for maxvar altered to minimise number of
!        allocations and deallocations
!   6/03 XML modifications added
!   9/03 Rigid region frozen direction flags now set
!  10/03 Rhombohedral coordinates for hexagonal system now output
!   2/04 Time-dependent force added
!   3/04 Spatial decomposition version of poccon added
!   4/04 Terse options added
!  10/04 Modifications for non-standard (>230) space groups made
!  12/04 Fixed atom choice for clusters and polymers improved
!   4/05 Shells excluded from fix atom search
!   8/05 Setting of fixed directions changed for nspg = 1, nccs > 0
!   2/06 Modified so that setting of a fixed direction discounts constrained
!        atoms
!   5/06 Fitting to stresses added
!   9/06 Hiccup with fitting flags fixed in MC where no atoms are yet present
!   9/06 Call to formula changed to ioout
!  11/06 NEB modifications added
!  11/06 Checking of user input flags for symmetry correctness
!  11/06 Approach to flags modified. Default values of ltmp set first based 
!        either on user input or GULP defaults. Symmetry correctness then 
!        enforced. 
!  11/06 Total occupancy is zero warning added
!   2/07 Electric field output added
!   3/07 Radial force added
!   3/07 Calls to mxmb renamed to GULP_mxmb
!   3/07 Call to bond changed to GULP_bond
!   5/07 Output of QM or MM region type added
!   5/07 Output of QM/MM mode added
!   6/07 Forcing of first atom to be fixed turned off for MC if lflags
!   7/07 Flag setting modified so that if a plane potential is present then 
!        there is no default fixing in the z direction. 
!  12/07 Unused variables removed
!   1/08 Code that frees up first atom for MD excluded from acting in MC case
!   4/08 Logic for setting of lphonfit modified to avoid referencing ndimen(0)
!        due to reaction energies
!   5/08 Orthorhombic cell relaxation option added
!   6/08 Fixing of a single atom turned off for NEB/Sync and unfix option modified
!   8/08 Bug in unfixing atoms for MD on clusters corrected
!  10/08 COSMO/COSMIC changes merged in
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   1/09 Integer datatypes all explicitly declared
!   2/09 XML calls removed
!   3/09 lkptdispersion added
!   6/09 Module name changed from three to m_three
!   3/10 Modification of default weights added 
!   6/10 Number of decimal places for charges increased to 5.
!   8/10 lfix1atom introduced to indicate whether unfix keyword is specified or not
!  12/10 Check for missing shells + automatic addition added
!  12/10 Output of anisotropic pressure tensor added
!  12/10 Hide shell option added
!   6/11 Electric field enabled for periodic systems
!   7/11 Keyword added to prevent automatic adding of shells
!   7/11 Check on cell / space group consistency added
!   7/11 Output of constraints modified to allow for larger systems
!   8/11 Call to poccons now controlled by lspatialok and not lspatial
!  10/11 Output of electric field modified
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, October 2011
!
  use configurations
  use control
  use cosmo
  use current
  use dispersion
  use element
  use field
  use four
  use freeze
  use general
  use iochannels
  use ksample
  use m_three
  use mdlogic
  use moldyn,        only : lfix
  use molecule
  use parallel
  use plane,         only : nplanepot
  use observables
  use optimisation,  only : lfix1atom
  use radial
  use shell
  use spatial,       only : lspatialok
  use symmetry
  use terse
  use two
  use wolfcosmo,     only : etawc, cutwc
  implicit none
!
!  Local variables
!
  character(len=1)                         :: crd(3)
  character(len=1)                         :: ocha(3)
  character(len=2)                         :: cstype
  character(len=3)                         :: fixed
  character(len=3)                         :: fixstring
  character(len=5)                         :: lab
  character(len=7)                         :: systype(4)
  integer(i4)                              :: i
  integer(i4)                              :: ic
  integer(i4)                              :: idj
  integer(i4)                              :: ii
  integer(i4)                              :: inat
  integer(i4)                              :: ind
  integer(i4)                              :: indi
  integer(i4)                              :: itype
  integer(i4)                              :: ix
  integer(i4)                              :: iy
  integer(i4)                              :: iz
  integer(i4)                              :: j
  integer(i4)                              :: k
  integer(i4)                              :: mvar
  integer(i4)                              :: mvar2
  integer(i4)                              :: na
  integer(i4)                              :: nati
  integer(i4)                              :: ncm
  integer(i4)                              :: ncrf
  integer(i4)                              :: ncrv
  integer(i4)                              :: ncvi
  integer(i4)                              :: ndir
  integer(i4)                              :: ndirp(3)
  integer(i4)                              :: nfixatom
  integer(i4)                              :: nfv
  integer(i4)                              :: nfv1
  integer(i4)                              :: nin
  integer(i4)                              :: nj
  integer(i4)                              :: nk
  integer(i4)                              :: nkp
  integer(i4)                              :: nlfgra
  integer(i4)                              :: nlfstr
  integer(i4)                              :: nlfgrad
  integer(i4)                              :: nlfstress
  integer(i4)                              :: nlkpt
  integer(i4)                              :: nobsold
  integer(i4)                              :: np
  integer(i4)                              :: npt
  integer(i4)                              :: nr
  integer(i4)                              :: nri
  integer(i4)                              :: nrj
  integer(i4)                              :: nspg
  integer(i4)                              :: nufgra
  integer(i4)                              :: nufstr
  integer(i4)                              :: nvarl
  integer(i4)                              :: nvarlold
  integer(i4)                              :: nvv
  integer(i4)                              :: nvv1
  integer(i4)                              :: status
  logical, dimension(:), allocatable       :: ltmp
  logical, dimension(:), allocatable       :: ltmploc
  logical                                  :: lalleinstein
  logical                                  :: lbreathe
  logical                                  :: lcore
  logical                                  :: lfirstout
  logical                                  :: lfixeddirection
  logical                                  :: lfound
  logical                                  :: lfound1
  logical                                  :: lfound2
  logical                                  :: liso
  logical                                  :: lnoflagsloc
  logical                                  :: lnxi
  logical                                  :: lnyi
  logical                                  :: lnzi
  logical                                  :: lodd
  logical                                  :: lortho
  logical                                  :: lphonfit
  logical                                  :: lrhombo
  logical                                  :: lshrink
  logical                                  :: lslice
  logical                                  :: lsuper
  real(dp)                                 :: afull
  real(dp)                                 :: alphafull
  real(dp)                                 :: alpprim
  real(dp)                                 :: aprim
  real(dp)                                 :: ara
  real(dp)                                 :: area
  real(dp)                                 :: betafull
  real(dp)                                 :: betprim
  real(dp)                                 :: bfull
  real(dp)                                 :: bprim
  real(dp)                                 :: cfull
  real(dp)                                 :: cprim
  real(dp)                                 :: dipolex
  real(dp)                                 :: dipoley
  real(dp)                                 :: dipolez
  real(dp)                                 :: fieldnorm
  real(dp)                                 :: forcenorm
  real(dp)                                 :: gamprim
  real(dp)                                 :: gammafull
  real(dp)                                 :: occtot
  real(dp)                                 :: r2
  real(dp)                                 :: r2best
  real(dp)                                 :: rvt(3,3)
  real(dp)                                 :: sum
  real(dp)                                 :: sumx
  real(dp)                                 :: sumy
  real(dp)                                 :: sumz
  real(dp)                                 :: vol
  real(dp)                                 :: volp
  real(dp)                                 :: volume
  real(dp)                                 :: x(3)
  real(dp)                                 :: xx(3)
  real(dp)                                 :: zbest
  real(dp)                                 :: xmid
  real(dp)                                 :: ymid
  real(dp)                                 :: zmid
!
  data crd/'x','y','z'/
  data systype/'Cluster','Polymer','Surface','Bulk   '/
!
  lnoflagsloc = lnoflags
  if (.not.lnoflagsloc) then
    lnoflagsloc = (.not.lconp.and..not.lconv.and..not.lcello.and..not.lshello)
  endif
  liso = (index(keyword,'iso').eq.1.or.index(keyword,' iso').ne.0) 
  lortho = (index(keyword,'ort').eq.1.or.index(keyword,' ort').ne.0) 
  lbreathe = (index(keyword,' brea').ne.0.or.index(keyword,'brea').eq.1)
!
!  Store number of constraints read in - if none have been
!  read in then there is no need to dump constraints as 
!  they were automatically generated by GULP
!
  nconin = ncontot
!******************************************
!  Output total number of configurations  *
!******************************************
  if (ioproc) then
    write(ioout,'(/,''  Total number of configurations input = '',i3)')ncfg
  endif
!***************************************************
!  Check for missing shells that need to be added  *
!***************************************************
  if (index(keyword,' noad').eq.0.and.index(keyword,'noad').ne.1) then
    do i = 1,ncfg
      call setup(.false.)
      call addshell
    enddo
  endif
!*********************************************************
!  Transform configurations input in rhombohedral form   *
!  hexagonal setting (ifhr=1)                            *
!*********************************************************
  do i = 1,ncfg
    if (ndimen(i).eq.3) then
      ncf = i
      call setup(.false.)
      nspg = nspcg(i)
      if (nspg.le.2) then
        ictype = 1
      elseif (nspg.ge.3.and.nspg.le.15) then
        ictype = 2
      elseif (nspg.ge.16.and.nspg.le.74) then
        ictype = 3
      elseif (nspg.ge.75.and.nspg.le.142) then
        ictype = 4
      elseif (nspg.ge.143.and.nspg.le.194) then
        ictype = 5
        call cellfhr(icfhr,rv)
        ifhr(i) = icfhr
      elseif (nspg.ge.195.and.nspg.le.230) then
        ictype = 6
      elseif (nspg.ge.231.and.nspg.le.232) then
        ictype = 2
      endif
      call setup(.false.)
      if (nccs.ne.5) ifhr(i) = 0
      if (ifhr(i).eq.1) then
!
!  Change cell to hexgonal form
!
        call rhtohex
!
!  Change fractional coordinates
!
        do na = 1,nasym
          x(1) = xcfg(nsft+na)
          x(2) = ycfg(nsft+na)
          x(3) = zcfg(nsft+na)
          xx(1) = 0.0_dp
          xx(2) = 0.0_dp
          xx(3) = 0.0_dp
          call GULP_mxmb(w(ncbl,1,1),7_i4,21_i4,x,1_i4,1_i4,xx,1_i4,1_i4,3_i4,3_i4,1_i4)
!
!  Place fractional coordinates in range 0-1
!
          xx(1) = xx(1) + 3.0_dp
          nin = xx(1)
          xx(1) = xx(1) - nin
          xx(2) = xx(2) + 3.0_dp
          nin = xx(2)
          xx(2) = xx(2) - nin
          xx(3) = xx(3) + 3.0_dp
          nin = xx(3)
          xx(3) = xx(3) - nin
          xcfg(nsft+na) = xx(1)
          ycfg(nsft+na) = xx(2)
          zcfg(nsft+na) = xx(3)
        enddo
      endif
    endif
  enddo
!**************************************************
!  One off initial set up for each configuration  *
!**************************************************
!
!  (1) Call setup to do symmetry
!  (2) Centre cell if necessary
!  (3) Create optimisation pointer array ioptcfg()
!
  nvarl = 0
  do ii = 1,ncfg
    ncf = ii
    nspg = max(nspcg(ii),1)
    lsuper = (nsuper(ii).gt.1)
    lsymopt = lsymset(ii)
    call setup(.false.)
    if (ndimen(ii).eq.3) then
      if (lsymopt) call centre
      do i = 1,3
        rv(1,i) = rvcfg(1,i,ii)
        rv(2,i) = rvcfg(2,i,ii)
        rv(3,i) = rvcfg(3,i,ii)
      enddo
      do i = 1,3
        rvt(1,i) = rv(1,i)
        rvt(2,i) = rv(2,i)
        rvt(3,i) = rv(3,i)
      enddo
      call uncentre(rvt)
      call uncell3D(rvt,afull,bfull,cfull,alphafull,betafull,gammafull)
    endif
!
!  If symmetry is to be switched off then expand structure
!  due to nosym option or supercell calcn
!
    if ((lsymopt.and..not.lsym).or.lsuper.or.lmc.or.lmd) then
!
!  Call setup to reinitialise qf()
!
      call symoff
      call setup(.false.)
      lsym = .false.
      lsymopt = .false.
      lsymset(ii) = .false.
      ictype = 1
      if (lsuper) call super
    endif
!
!  Generate coordinates here in case molecule calculation is to be performed
!
    if (ndimen(ii).eq.3) then
!
!  3-D
!
      do i = 1,numat
        xclat(i) = xfrac(i)*rv(1,1) + yfrac(i)*rv(1,2) + zfrac(i)*rv(1,3)
        yclat(i) = xfrac(i)*rv(2,1) + yfrac(i)*rv(2,2) + zfrac(i)*rv(2,3)
        zclat(i) = xfrac(i)*rv(3,1) + yfrac(i)*rv(3,2) + zfrac(i)*rv(3,3)
      enddo
    elseif (ndimen(ii).eq.2) then
!
!  2-D
!
      do i = 1,numat
        xclat(i) = xfrac(i)*rv(1,1) + yfrac(i)*rv(1,2)
        yclat(i) = xfrac(i)*rv(2,1) + yfrac(i)*rv(2,2)
        zclat(i) = zfrac(i)
      enddo
    endif
    call setup(.true.)
!
!  Set total number of possible variables
!
!  3 x number of atoms + number of strains 
!
    if (lsymopt) then
      mvar = 3*nasym + nstrains 
      mvar2 = mvar + nasym
    else
      mvar = 3*numat + nstrains
      mvar2 = mvar + numat
    endif
!
!  Count number of breathing shells and set default radii
!
    nbsm = 0
    do i = 1,nasym
      if (lbsmat(nsft+i)) then
        nbsm = nbsm + 1
        if (radcfg(nsft+i).eq.0.0_dp) then
!
!  Find potential and set radius equal to equilibrium value
!
          lfound = .false.
          np = 0
          do while (np.lt.npote.and..not.lfound)
            np = np + 1
            npt = nptype(np)
            lfound = ((npt.eq.14.or.npt.eq.17.or.npt.eq.31).and.iatn(i).eq.nspec1(np).and. &
              (natype(i).eq.nptyp1(np).or.nptyp1(np).eq.0))
          enddo
          if (lfound) then
            radcfg(nsft+i) = twopot(2,np)
          else
            nati = iatn(i)
            if (nati.gt.maxele) nati = nati - maxele
            radcfg(nsft+i) = rion(nati)
          endif
        endif
      endif
    enddo
!
!  Set flag according to whether all atoms are fixed to sites by Einstein model
!
    lalleinstein = .true.
    do i = 1,nasym
      if (.not.leinsteinat(nsft + i)) lalleinstein = .false.
    enddo
!
    if (nspg.le.2) then
      ictype = 1
    elseif (nspg.ge.3.and.nspg.le.15) then
      ictype = 2
    elseif (nspg.ge.16.and.nspg.le.74) then
      ictype = 3
    elseif (nspg.ge.75.and.nspg.le.142) then
      ictype = 4
    elseif (nspg.ge.143.and.nspg.le.194) then
      ictype = 5
    elseif (nspg.ge.195.and.nspg.le.230) then
      ictype = 6
    elseif (nspg.ge.231.and.nspg.le.232) then
      ictype = 2
    endif
    lrhombo = (ifhr(ncf).eq.1.and.(.not.lhex))
!
!  Set up growth slice if necessary
!
    if (ndimen(ii).eq.2) then
      if (dhklcfg(ii).gt.0.0_dp) then
        call setslice(ii)
      endif
      call setzmolslice
    endif
!
!  Allocate array for optimisation logicals
!
    allocate(ltmp(mvar2),stat=status)
    if (status/=0) call outofmemory('setcfg','ltmp')
!
    if ((lopt.or.lgrad.or.lfit.or.lmc.or.lmd.or.lneb).and..not.lbulknoopt) then
!*******************
!  Unit cell flags *
!*******************
      if (ndimen(ii).gt.0) then
!
!  Set initial flags prior to symmetry checking
!
        if (lflags) then
          ind = 6*(ii-1)
          do j = 1,nstrains
            ltmp(j) = lopfc(ind+j)
          enddo
        else
          if (lconp.or.lcello) then
            if (liso) then
!
!  Isotropic cell expansion only
!
              ltmp(1) = .true.
              do j = 2,nstrains
                ltmp(j) = .false.
              enddo
            elseif (lortho) then
!
!  Orthorhombic cell expansion only
!
              if (ndimen(ii).eq.3) then
                ltmp(1) = .true.
                ltmp(2) = .true.
                ltmp(3) = .true.
                do j = 4,nstrains
                  ltmp(j) = .false.
                enddo
              elseif (ndimen(ii).eq.2) then
                ltmp(1) = .true.
                ltmp(2) = .true.
                ltmp(3) = .false.
              elseif (ndimen(ii).eq.1) then
                ltmp(1) = .true.
              endif
            else
!
!  Anisotropic cell expansion
!
              do j = 1,nstrains
                ltmp(j) = .true.
              enddo
            endif
          else
            do j = 1,nstrains
              ltmp(j) = .false.
            enddo
          endif
        endif
        if (liso) then
!
!  Isotropic cell expansion only
!
          if (ndim.eq.3) then
            if (ncontot+2.ge.maxcontot) then
              maxcontot = ncontot + 50
              call changemaxcontot
            endif 
            if (ii.lt.ncfg) then
              do k = ncontot,n1con(ii+1),-1
                ncvarcfg(k+2) = ncvarcfg(k)
                ncfixcfg(k+2) = ncfixcfg(k)
                concocfg(k+2) = concocfg(k)
                nconcfg(k+2) = nconcfg(k)
                conaddcfg(k+2) = conaddcfg(k)
              enddo
              do k = ii+1,ncfg
                n1con(k) = n1con(k) + 2
              enddo
            endif
            ncontot = ncontot + 2
            ncon = ncon + 1
            ncvarcfg(ncfst+ncon) = 1
            ncfixcfg(ncfst+ncon) = 2
            concocfg(ncfst+ncon) = 1.0_dp
            conaddcfg(ncfst+ncon) = 0.0_dp
            nconcfg(ncfst+ncon) = ii
            ncon = ncon + 1
            ncvarcfg(ncfst+ncon) = 1
            ncfixcfg(ncfst+ncon) = 3
            concocfg(ncfst+ncon) = 1.0_dp
            conaddcfg(ncfst+ncon) = 0.0_dp
            nconcfg(ncfst+ncon) = ii
          elseif (ndim.eq.2) then
            if (ncontot+1.ge.maxcontot) then
              maxcontot = ncontot + 50
              call changemaxcontot
            endif 
            if (ii.lt.ncfg) then
              do k = ncontot,n1con(ii+1),-1
                ncvarcfg(k+1) = ncvarcfg(k)
                ncfixcfg(k+1) = ncfixcfg(k)
                concocfg(k+1) = concocfg(k)
                nconcfg(k+1) = nconcfg(k)
                conaddcfg(k+1) = conaddcfg(k)
              enddo
              do k = ii+1,ncfg
                n1con(k) = n1con(k) + 1
              enddo
            endif
            ncontot = ncontot + 1
            ncon = ncon + 1
            ncvarcfg(ncfst+ncon) = 1
            ncfixcfg(ncfst+ncon) = 2
            concocfg(ncfst+ncon) = 1.0_dp
            conaddcfg(ncfst+ncon) = 0.0_dp
            nconcfg(ncfst+ncon) = ii
          endif
        else
!
!  Anisotropic cell expansion
!
          if (lsymopt.and.nspg.gt.1) then
!
!  For certain cell types remove redundant cell strains
!
            if (ictype.gt.2) then
!
!  Orthorhombic, tetragonal, hexagonal, trigonal and cubic
!
              ltmp(4) = .false.
              ltmp(5) = .false.
              ltmp(6) = .false.
              if (.not.lvecin(ncf)) then
                if (ictype.eq.6) then
                  ltmp(2) = .false.
                  ltmp(3) = .false.
!
!  Check to see if constraint already exists
!
                  lfound1 = .false.
                  lfound2 = .false.
                  do k = 1,ncon
                    if (ncvarcfg(k+ncfst).eq.1.and.ncfixcfg(k+ncfst).eq.2) then 
                      lfound1 = .true.
                    elseif (ncvarcfg(k+ncfst).eq.1.and.ncfixcfg(k+ncfst).eq.3) then
                      lfound2 = .true.
                    endif
                  enddo
                  if (.not.lfound1.and..not.lfound2) then
                    if (ncontot+2.ge.maxcontot) then
                      maxcontot = ncontot + 50
                      call changemaxcontot
                    endif 
                    if (ii.lt.ncfg) then
                      do k = ncontot,n1con(ii+1),-1
                        ncvarcfg(k+2) = ncvarcfg(k)
                        ncfixcfg(k+2) = ncfixcfg(k)
                        concocfg(k+2) = concocfg(k)
                        conaddcfg(k+2) = conaddcfg(k)
                        nconcfg(k+2) = nconcfg(k)
                      enddo
                      do k = ii+1,ncfg
                        n1con(k) = n1con(k) + 2
                      enddo
                    endif
                    ncontot = ncontot + 2
                    ncon = ncon + 1
                    ncvarcfg(ncfst+ncon) = 1
                    ncfixcfg(ncfst+ncon) = 2
                    concocfg(ncfst+ncon) = 1.0_dp
                    conaddcfg(ncfst+ncon) = 0.0_dp
                    nconcfg(ncfst+ncon) = ii
                    ncon = ncon + 1
                    ncvarcfg(ncfst+ncon) = 1
                    ncfixcfg(ncfst+ncon) = 3
                    concocfg(ncfst+ncon) = 1.0_dp
                    conaddcfg(ncfst+ncon) = 0.0_dp
                    nconcfg(ncfst+ncon) = ii
                  elseif (lfound1.and..not.lfound2) then
                    if (ncontot.ge.maxcontot) then
                      maxcontot = ncontot + 50
                      call changemaxcontot
                    endif 
                    if (ii.lt.ncfg) then
                      do k = ncontot,n1con(ii+1),-1
                        ncvarcfg(k+1) = ncvarcfg(k)
                        ncfixcfg(k+1) = ncfixcfg(k)
                        concocfg(k+1) = concocfg(k)
                        conaddcfg(k+1) = conaddcfg(k)
                        nconcfg(k+1) = nconcfg(k)
                      enddo
                      do k = ii+1,ncfg
                        n1con(k) = n1con(k) + 1
                      enddo
                    endif
                    ncontot = ncontot + 1
                    ncon = ncon + 1
                    ncvarcfg(ncfst+ncon) = 1
                    ncfixcfg(ncfst+ncon) = 3
                    concocfg(ncfst+ncon) = 1.0_dp
                    conaddcfg(ncfst+ncon) = 0.0_dp
                    nconcfg(ncfst+ncon) = ii
                  elseif (.not.lfound1.and.lfound2) then
                    if (ncontot.ge.maxcontot) then
                      maxcontot = ncontot + 50
                      call changemaxcontot
                    endif 
                    if (ii.lt.ncfg) then
                      do k = ncontot,n1con(ii+1),-1
                        ncvarcfg(k+1) = ncvarcfg(k)
                        ncfixcfg(k+1) = ncfixcfg(k)
                        concocfg(k+1) = concocfg(k)
                        nconcfg(k+1) = nconcfg(k)
                        conaddcfg(k+1) = conaddcfg(k)
                      enddo
                      do k = ii+1,ncfg
                        n1con(k) = n1con(k) + 1
                      enddo
                    endif
                    ncontot = ncontot + 1
                    ncon = ncon + 1
                    ncvarcfg(ncfst+ncon) = 1
                    ncfixcfg(ncfst+ncon) = 2
                    concocfg(ncfst+ncon) = 1.0_dp
                    conaddcfg(ncfst+ncon) = 0.0_dp
                    nconcfg(ncfst+ncon) = ii
                  endif
                elseif (ictype.eq.4.or.ictype.eq.5) then
                  ltmp(2) = .false.
!
!  Check to see if constraint already exists
!
                  lfound1 = .false.
                  do k = 1,ncon
                    if (ncvarcfg(k+ncfst).eq.1.and.ncfixcfg(k+ncfst).eq.2) then 
                      lfound1 = .true.
                    endif
                  enddo
                  if (.not.lfound1) then
                    if (ncontot.ge.maxcontot) then
                      maxcontot = ncontot + 50
                      call changemaxcontot
                    endif 
                    if (ii.lt.ncfg) then
                      do k = ncontot,n1con(ii+1),-1
                        ncvarcfg(k+1) = ncvarcfg(k)
                        ncfixcfg(k+1) = ncfixcfg(k)
                        concocfg(k+1) = concocfg(k)
                        conaddcfg(k+1) = conaddcfg(k)
                        nconcfg(k+1) = nconcfg(k)
                      enddo
                      do k = ii+1,ncfg
                        n1con(k) = n1con(k) + 1
                      enddo
                    endif
                    ncontot = ncontot + 1
                    ncon = ncon + 1
                    ncvarcfg(ncfst+ncon) = 1
                    ncfixcfg(ncfst+ncon) = 2
                    concocfg(ncfst+ncon) = 1.0_dp
                    conaddcfg(ncfst+ncon) = 0.0_dp
                    nconcfg(ncfst+ncon) = ii
                  endif
                endif
              endif
            elseif (ictype.eq.2) then
!
!  Monoclinic
!
              if (abs(alphafull-90.0_dp).gt.1.0d-4) then
                ltmp(5) = .false.
                ltmp(6) = .false.
              elseif (abs(gammafull-90.0_dp).gt.1.0d-4) then
                ltmp(4) = .false.
                ltmp(5) = .false.
              else
                ltmp(4) = .false.
                ltmp(6) = .false.
              endif
            endif
          endif
        endif
      endif
!******************
!  Internal flags *
!******************
!
!  Set initial flags based on keywords or lack of them
!
      if (lflags) then
        do j = 1,nasym
          ltmp(3*j+nstrains-2) = lopfi(3*(j+nsft-1)+1)
          ltmp(3*j+nstrains-1) = lopfi(3*(j+nsft-1)+2)
          ltmp(3*j+nstrains)   = lopfi(3*(j+nsft-1)+3)
        enddo
      elseif (lcello.or.lnoflags.or.lbreathe) then
        do j = 1,3*nasym
          ltmp(nstrains+j) = .false.
        enddo
      elseif (lshello) then
        do j = 1,nasym
          if (iatn(j).lt.maxele) then
            ltmp(3*j+nstrains-2) = .false.
            ltmp(3*j+nstrains-1) = .false.
            ltmp(3*j+nstrains)   = .false.
          else
            ltmp(3*j+nstrains-2) = .true.
            ltmp(3*j+nstrains-1) = .true.
            ltmp(3*j+nstrains)   = .true.
          endif
        enddo
      else
        do j = 1,3*nasym
          ltmp(nstrains+j) = .true.
        enddo
      endif
!
!  Unfreezing option
!
      if (lufree(ncf)) call unfreeze(ltmp)
!
!  Fix rigid region atom directions
!
      if (nregions(ii).gt.1) then
        do j = 1,nasym
          nrj = nregionno(nsft+j)
          if (lregionrigid(nrj,ncf)) then
            if (.not.lopfreg(3*(nrj-1)+1,ncf)) then
              ltmp(nstrains+3*(j-1)+1) = .false.
            endif
            if (.not.lopfreg(3*(nrj-1)+2,ncf)) then
              ltmp(nstrains+3*(j-1)+2) = .false.
            endif
            if (.not.lopfreg(3*(nrj-1)+3,ncf)) then
              ltmp(nstrains+3*(j-1)+3) = .false.
            endif
          endif
        enddo
      endif
      if (lsymopt) then
!
!  Special positions
!
        call special(ltmp,.false.)
      endif
!
!  Partial occupancy constraints
!
      if (lspatialok) then
        call poccons(ltmp)
      else
        call poccon(ltmp)
      endif
!
!  Rigid region translation
!
      if (nregions(ii).gt.0) then
        call setregiontrans(ltmp)
      endif
      if (ndimen(ii).eq.3.and.(.not.lflags.or..not.lmc).and.lfix1atom) then
!
!  For certain space groups there is one arbitary coordinate 
!  in which case one variable in this direction must be removed.
!  This is true if there is no inversion symmetry in this 
!  direction.
!
        lodd = .false.
        if (nspg.eq.1.and.ngocfg(ii).gt.1) then
!
!  General - operators used instead of space group
!       
          sumx = 0.0_dp
          sumy = 0.0_dp
          sumz = 0.0_dp
          do ngo = 1,ngocfg(ii)
            sumx = sumx + ropcfg(1,1,ngo,ii)
            sumy = sumy + ropcfg(2,2,ngo,ii)
            sumz = sumz + ropcfg(3,3,ngo,ii)       
          enddo
          ndir = 0
          if (abs(sumx).gt.0.0_dp) then
            ndir = ndir + 1
            ndirp(ndir) = 1
          endif
          if (abs(sumy).gt.0.0_dp) then
            ndir = ndir + 1
            ndirp(ndir) = 2
          endif
          if (abs(sumz).gt.0.0_dp) then
            ndir = ndir + 1
            ndirp(ndir) = 3
          endif
          lodd = (ndir.gt.0)
        elseif (nspg.eq.1.and.nccs.eq.1) then
          if (.not.lalleinstein) then
            lodd = .true.
            ndir = 3
            ndirp(1) = 1
            ndirp(2) = 2
            ndirp(3) = 3
          endif
        elseif (nspg.ge.3.and.nspg.le.5) then
!
!  Monoclinic
!
          lodd = .true.
          ndir = 1
          if (abs(alphafull-90.0_dp).gt.1.0d-4) then
            ndirp(1) = 1
          elseif (abs(gammafull-90.0_dp).gt.1.0d-4) then
            ndirp(1) = 3
          else
            ndirp(1) = 2
          endif
        elseif (nspg.ge.6.and.nspg.le.9) then
!
!  Monoclinic
!
          lodd = .true.
          ndir = 2
          if (abs(alphafull-90.0_dp).gt.1.0d-4) then
            ndirp(1) = 2
            ndirp(2) = 3
          elseif (abs(gammafull-90.0_dp).gt.1.0d-4) then
            ndirp(1) = 1
            ndirp(2) = 2
          else
            ndirp(1) = 1
            ndirp(2) = 3
          endif
        elseif (nspg.eq.231) then
!
!  Monoclinic - C 1 
!
          lodd = .true.
          ndir = 3
          ndirp(1) = 1
          ndirp(2) = 2
          ndirp(3) = 3
        elseif (nspg.ge.25.and.nspg.le.46) then
!
!  Orthorhombic
!
          lodd = .true.
          ndir = 1
          ndirp(1) = 3
        elseif ((nspg.ge.75.and.nspg.le.80).or.(nspg.ge.99.and.nspg.le.110)) then
!
!  Tetragonal
!
          lodd = .true.
          ndir = 1
          ndirp(1) = 3
        elseif (nspg.ge.143.and.nspg.le.146) then
!
!  Trigonal
!
          lodd = .true.
          ndir = 1
          ndirp(1) = 3
        elseif ((nspg.ge.156.and.nspg.le.159).or.(nspg.ge.168.and.nspg.le.173)) then
!
!  Trigonal/Hexagonal
!
          lodd = .true.
          ndir = 1
          ndirp(1) = 3
        elseif (nspg.ge.183.and.nspg.le.186) then
!
!  Hexagonal
!
          lodd = .true.
          ndir = 1
          ndirp(1) = 3
        endif
!
!  Correct flags for lack of inversion symmetry
!
        if (lodd) then
          allocate(ltmploc(3*nasym+6),stat=status)
          if (status/=0) call outofmemory('setcfg','ltmploc')
          ltmploc(1:3*nasym+6) = ltmp(1:3*nasym+6)
          do j = 1,ncon
            ltmploc(ncfixcfg(ncfst+j)) = .true.
          enddo
          lnxi = .true.
          lnyi = .true.
          lnzi = .true.
          do j = 1,nasym
            if (.not.ltmploc(3*j+4)) lnxi = .false.
            if (.not.ltmploc(3*j+5)) lnyi = .false.
            if (.not.ltmploc(3*j+6)) lnzi = .false.
          enddo
          do j = 1,ndir
            if (ndirp(j).eq.1) then
              k = 0
              do while (lnxi.and.k.lt.nasym)
                k = k + 1
                if (ltmp(6+3*(k-1)+1)) then
                  ltmp(6+3*(k-1)+1) = .false.
                  lnxi = .false.
                endif
              enddo
            elseif (ndirp(j).eq.2) then
              k = 0
              do while (lnyi.and.k.lt.nasym)
                k = k + 1
                if (ltmp(6+3*(k-1)+2)) then
                  ltmp(6+3*(k-1)+2) = .false.
                  lnyi = .false.
                endif
              enddo
            else
              k = 0
              do while (lnzi.and.k.lt.nasym)
                k = k + 1
                if (ltmp(6+3*(k-1)+3)) then
                  ltmp(6+3*(k-1)+3) = .false.
                  lnzi = .false.
                endif
              enddo
            endif
          enddo
          deallocate(ltmploc,stat=status)
          if (status/=0) call deallocate_error('setcfg','ltmploc')
        endif
      endif
!
!  If not symmetry optimisation or all Einstein model set derivatives of one atom to zero
!
!  For a slab, find atom in the middle to fix
!
      nfixatom = 1
      if (.not.lsymopt.and..not.lshello.and..not.lflags.and..not.lalleinstein.and..not.lneb &
          .and.lfix1atom) then
        if (nregions(ii).eq.1) then
          if (ndim.eq.2) then
!
!  Find atom close to middle of slab to fix
!
            zmid = 0.0_dp
            do i = 1,ncore
              ic = ncoptr(i)
              zmid = zmid + zclat(ic)
            enddo
            zmid = zmid/dble(ncore)
            zbest = 1.0d10
            do i = 1,ncore
              ic = ncoptr(i)
              if (abs(zclat(ic)-zmid).lt.zbest) then
                nfixatom = ic
                zbest = abs(zclat(ic)-zmid)
              endif
            enddo
          elseif (ndim.eq.1) then
!
!  Find atom close to the middle of polymer to fix
!
            ymid = 0.0_dp
            zmid = 0.0_dp
            do i = 1,numat
              ic = ncoptr(i)
              ymid = ymid + yclat(ic)
              zmid = zmid + zclat(ic)
            enddo
            ymid = ymid/dble(ncore)
            zmid = zmid/dble(ncore)
            r2best = 1.0d20
            do i = 1,ncore
              ic = ncoptr(i)
              r2 = (yclat(ic)-ymid)**2 + (zclat(ic)-zmid)**2
              if (r2.lt.r2best) then
                nfixatom = ic
                r2best = r2
              endif
            enddo
          elseif (ndim.eq.0) then
!
!  Find atom close to the middle of cluster to fix
!
            xmid = 0.0_dp
            ymid = 0.0_dp
            zmid = 0.0_dp
            do i = 1,ncore
              ic = ncoptr(i)
              xmid = xmid + xclat(ic)
              ymid = ymid + yclat(ic)
              zmid = zmid + zclat(ic)
            enddo
            xmid = xmid/dble(ncore)
            ymid = ymid/dble(ncore)
            zmid = zmid/dble(ncore)
            r2best = 1.0d20
            do i = 1,ncore
              ic = ncoptr(i)
              r2 = (xclat(ic)-xmid)**2 + (yclat(ic)-ymid)**2 + (zclat(ic)-zmid)**2
              if (r2.lt.r2best) then
                nfixatom = ic
                r2best = r2
              endif
            enddo
          endif
        endif
        lfound = .false.
        ind = nstrains - 2
        do i = 1,nasym
          ind = ind + 3
          if (.not.ltmp(ind)) lfound = .true.
        enddo
        if (.not.lfound) ltmp(nstrains+3*(nfixatom-1)+1) = .false.
        lfound = .false.
        ind = nstrains - 1
        do i = 1,nasym
          ind = ind + 3
          if (.not.ltmp(ind)) lfound = .true.
        enddo
        if (.not.lfound) ltmp(nstrains+3*(nfixatom-1)+2) = .false.
        lfound = .false.
        ind = nstrains
        do i = 1,nasym
          ind = ind + 3
          if (.not.ltmp(ind)) lfound = .true.
        enddo
!
!  If there is a plane potential and this is 2-D then there is no need to fix
!  a coordinate in the z-direction since the plane prevents translation
!
        if ((ndim.ne.0.and.ndim.ne.2).or.nplanepot.eq.0) then
          if (.not.lfound) ltmp(nstrains+3*(nfixatom-1)+3) = .false.
        endif
      endif
!
!  For MD no need to fix any atoms
!
      if (lmd.and..not.lflags.and.numat.gt.0) then
        ltmp(nstrains+3*(nfixatom-1)+1) = .true.
        ltmp(nstrains+3*(nfixatom-1)+2) = .true.
        ltmp(nstrains+3*(nfixatom-1)+3) = .true.
      endif
!
!  Check constraints to remove non-allowed variables
!
      if (ncon.gt.0) then
        do i = 1,ncon
          if (ncfixcfg(i+ncfst).le.mvar) ltmp(ncfixcfg(i+ncfst)) = .false.
        enddo
      endif
    else
      do i = 1,3*nasym+nstrains
        ltmp(i) = .false.
      enddo
    endif
!**************************************
!  Initialise variable pointer array  *
!**************************************
    n1var(ii) = nvarl + 1
!
!  Count number of variables and check memory
!
    nvarlold = nvarl
    do j = 1,mvar
      if (ltmp(j)) then
        nvarl = nvarl + 1
      endif
    enddo
    if (nvarl.ge.maxvar) then
      maxvar = nvarl + 10
      call changemaxvar
    endif
    nvarl = nvarlold
    do j = 1,mvar
      if (ltmp(j)) then
        nvarl = nvarl + 1
        ioptcfg(nvarl) = j
      endif
    enddo
!
!  Radius
!
    if (.not.lbulknoopt.and.index(keyword,'nobr').eq.0) then
      do j = 1,nasym
        if (lbsmat(j+nsft)) then
          ltmp(mvar+j) = .true.
        else
          ltmp(mvar+j) = .false.
        endif
      enddo
      if (ncon.gt.0) then
        do i = 1,ncon
          if (ncfixcfg(i+ncfst).gt.mvar) ltmp(ncfixcfg(i+ncfst)) = .false.
        enddo
      endif
      nvarlold = nvarl
      do j = 1,nasym
        if (ltmp(j+mvar)) then
          nvarl = nvarl + 1
        endif
      enddo
      if (nvarl.ge.maxvar) then
        maxvar = nvarl + 10
        call changemaxvar
      endif
      nvarl = nvarlold
      do j = 1,nasym
        if (ltmp(j+mvar)) then
          nvarl = nvarl + 1
          ioptcfg(nvarl) = mvar + j
        endif
      enddo
    endif
    nvarcfg(ii) = nvarl - n1var(ii) + 1
!
!  If shrinking factors have been set then set flag
!
    lshrink = ((nxks(ii)*nyks(ii)*nzks(ii)).gt.0)
    if (ioproc) then
!*******************************
!  Configuration based output  *
!*******************************
      write(ioout,'(/,''********************************************************************************'')')
      if (names(ii)(1:1).eq.' ') then
        write(ioout,'(''*  Input for Configuration = '',i3,47x,''*'')')ii
      else
        write(ioout,'(''*  Input for Configuration = '',i3,'' : '',a44,''*'')')ii,names(ii)(1:44)
      endif
      write(ioout,'(''********************************************************************************'')')
      call formula(ioout)
!
!  Check that total occupancy isn't zero and output warning
!
      occtot = 0.0_dp
      do i = 1,nasym
        occtot = occtot + occua(i)
      enddo
      if (occtot.lt.1.0d-12.and.nasym.gt.0) then
        nwarn = nwarn + 1
        call outwarning('Total occupancy of all atoms is zero and so energy will be zero',0_i4)
      endif
!
      write(ioout,'(/,''  Number of irreducible atoms/shells = '',i7,/)') nasym
      write(ioout,'(/,''  Total number atoms/shells = '',i7,/)') numat
      write(ioout,'(''  Dimensionality = '',i1,15x,'':'',2x,7a)') ndimen(ii),systype(ndimen(ii)+1)
      write(ioout,'(/)')
    endif
    sum = 0.0_dp
    do j = 1,numat
      sum = sum + qf(j)*occuf(j)
    enddo
    totalchargecfg(ii) = sum
    if (ioproc) then
      if (ndimen(ii).eq.0) then
        write(ioout,'(''  Charge on cluster = '',f6.2,/)')sum
      elseif (abs(sum).ge.1.0d-4) then
        if (ndimen(ii).eq.3) then
          write(ioout,'(''  Charge on solid   = '',f6.2,'' =>neutralising background added'',/)')sum
        elseif (ndimen(ii).eq.2) then
          write(ioout,'(''  Charge on slab    = '',f6.2,'' =>energy corrected for self-interaction'',/)')sum
        else
          write(ioout,'(''  Charge on polymer = '',f6.2,'' =>energy corrected for self-interaction'',/)')sum
          nwarn = nwarn + 1
          call outwarning('Charged polymer calculations are not reliable',0_i4)
        endif
      endif
      if (lcosmo) then
        if (lcosmic) then
          write(ioout,'(''  Solvated with COSMIC model : '',/)')
        else
          write(ioout,'(''  Solvated with COSMO model : '',/)')
        endif
        write(ioout,'(''  Dielectric constant        = '',f12.6)') cosmoepsilon(ncf)
        write(ioout,'(''  Solvent radius             = '',f12.6,'' Angstroms'')') cosmorsolv(ncf)
        write(ioout,'(''  Delta solvent radius       = '',f12.6,'' Angstroms'')') cosmodrsolv(ncf)
        write(ioout,'(''  Cutoff : point to segment  = '',f12.6,'' Angstroms'')') cosmormax
        write(ioout,'(''  Smooth : point to segment  = '',f12.6,'' Angstroms'')') cosmormaxs
        write(ioout,'(''  Smooth : point inclusion   = '',f12.6,'' Angstroms'')') cosmorange
        write(ioout,'(''  Coulomb: Wolf sum eta      = '',f12.6,'' Angstroms-1'')') etawc
        write(ioout,'(''  Coulomb: Wolf sum cutoff   = '',f12.6,'' Angstroms'')') cutwc
        write(ioout,'(''  No. of points per sphere   = '',i12)') nppa
        write(ioout,'(''  No. of segments per sphere = '',i12)') nspa
        if (isasatomoption.eq.1) then
          write(ioout,'(''  For generation of SAS use  = cores only'')')
        elseif (isasatomoption.eq.2) then
          write(ioout,'(''  For generation of SAS use  = both cores and shells'')')
        endif
        if (ldodeca) then
          write(ioout,'(''  Shape of atomic mesh       = '',''Dodecahedron'',/)')
        else
          write(ioout,'(''  Shape of atomic mesh       = '',''Octahedron'',/)')
        endif
      endif
      if (QMMMmode(ii).eq.1) then
        write(ioout,'(''  QM/MM rules for mechanical embedding to be applied to this configuration '')')
      elseif (QMMMmode(ii).eq.2) then
        write(ioout,'(''  QM/MM rules for electrical embedding to be applied to this configuration '')')
      endif
      if (lsymopt) call symout
      if (lsuper) then
        ind = nsuper(ii)
        ix = ind/10000
        ind = ind - 10000*ix
        iy = ind/100
        iz = ind - 100*iy
        if (ndimen(ii).eq.3) then
          write(ioout,'(/,''  Supercell dimensions :  x = '',i3,''  y = '',i3,''  z = '',i3)') ix,iy,iz
        elseif (ndimen(ii).eq.2) then
          write(ioout,'(/,''  Supercell dimensions :  x = '',i3,''  y = '',i3)') ix,iy
        elseif (ndimen(ii).eq.1) then
          write(ioout,'(/,''  Supercell dimensions :  x = '',i3)') ix
        endif
      endif
      if (ndimen(ii).eq.3) then
        if (.not.lterseincell) then
          write(ioout,'(/,''  Cartesian lattice vectors (Angstroms) :'',/)')
          do i = 1,3
            write(ioout,'(4x,3f12.6)')(rv(j,i),j=1,3)
          enddo
        endif
        if (ncbl.gt.1.and.ifhr(ii).eq.0) then
          aprim = a
          bprim = b
          cprim = c
          alpprim = alpha
          betprim = beta
          gamprim = gamma
          do i = 1,3
            rvt(1,i) = rv(1,i)
            rvt(2,i) = rv(2,i)
            rvt(3,i) = rv(3,i)
          enddo
          call uncentre(rvt)
          call uncell3D(rvt,a,b,c,alpha,beta,gamma)
          if (.not.lterseincell) then
            write(ioout,'(/,''  Primitive cell parameters :'',10x,''  Full cell parameters :'',/)')
            write(ioout,'(''  a = '',f8.4,''    alpha = '',f8.4,4x,''   a = '',f8.4,''    alpha = '',f8.4)') &
              aprim,alpprim,a,alpha
            write(ioout,'(''  b = '',f8.4,''    beta  = '',f8.4,4x,''   b = '',f8.4,''    beta  = '',f8.4)') &
              bprim,betprim,b,beta
            write(ioout,'(''  c = '',f8.4,''    gamma = '',f8.4,4x,''   c = '',f8.4,''    gamma = '',f8.4)') &
              cprim,gamprim,c,gamma
          endif
          volp = volume(rvt)
          vol = volume(rv)
          if (.not.lterseincell) then
            write(ioout,'(/,''  Initial volumes (Angstroms**3):'')')
            write(ioout,'(/,''  Primitive cell = '',f18.6,2x,''  Full cell = '',f18.6)')vol,volp
          endif
        else
          vol = volume(rv)
          if (.not.lterseincell) then
            write(ioout,'(/,''  Cell parameters (Angstroms/Degrees):'',/)')
            write(ioout,'(''  a = '',f12.4,''    alpha = '',f8.4)') a,alpha
            write(ioout,'(''  b = '',f12.4,''    beta  = '',f8.4)') b,beta
            write(ioout,'(''  c = '',f12.4,''    gamma = '',f8.4)') c,gamma
            write(ioout,'(/,''  Initial cell volume = '',f18.6,'' Angs**3'')') vol
          endif
        endif
!
!  Check that cell parameters are consistent with space group
!
        if (lsym) then
          call cellcheck(nspg,a,b,c,alpha,beta,gamma)
        endif
        if (lfieldcfg(ii)) then
          write(ioout,'(/,''  Electric field applied   = '',f13.6,'' eV/Ang.e '')') fieldcfg(ii)
          write(ioout,'(''  Electric field direction = '',f13.6,'' a '')') fielddirectioncfg(1,ii)
          write(ioout,'(''                             '',f13.6,'' b '')') fielddirectioncfg(2,ii)
          write(ioout,'(''                             '',f13.6,'' c '')') fielddirectioncfg(3,ii)
        endif
      elseif (ndimen(ii).eq.2) then
        if (.not.lterseincell) then
          write(ioout,'(/,''  Surface Cartesian vectors (Angstroms) :'',/)')
          do i = 1,2
            write(ioout,'(4x,3f12.6)')(rv(j,i),j=1,3)
          enddo
        endif
        ara = area(rv)
        if (.not.lterseincell) then
          write(ioout,'(/,''  Surface cell parameters (Angstroms/Degrees):'',/)')
          write(ioout,'(''  a = '',f12.4,''    alpha = '',f8.4)') a,alpha
          write(ioout,'(''  b = '',f12.4)') b
          write(ioout,'(/,''  Initial surface area   = '',f13.6,'' Angs**2'')') ara
        endif
        call getdipole2D(dipolez)
        write(ioout,'(/,''  Initial surface dipole = '',f13.6,'' e.Angs'')') dipolez
        if (lfieldcfg(ii)) then
          write(ioout,'(/,''  Electric field applied   = '',f13.6,'' eV/Ang.e'')') fieldcfg(ii)
          write(ioout,'(''  Electric field direction = '',f13.6,'' a '')') fielddirectioncfg(1,ii)
          write(ioout,'(''                             '',f13.6,'' b '')') fielddirectioncfg(2,ii)
          write(ioout,'(''                             '',f13.6,'' z '')') fielddirectioncfg(3,ii)
        endif
      elseif (ndimen(ii).eq.1) then
        if (.not.lterseincell) then
          write(ioout,'(/,''  Polymer Cartesian vector (Angstroms) :'',/)')
          write(ioout,'(4x,3f12.6)') (rv(j,1),j=1,3)
        endif
        if (.not.lterseincell) then
          write(ioout,'(/,''  Polymer cell parameter (Angstrom):'',/)')
          write(ioout,'(''  a = '',f12.4)') a
        endif
        call getdipole1D(dipoley,dipolez)
        write(ioout,'(/,''  Initial polymer dipoles : y = '',f13.6,'' e.Angs'')') dipoley
        write(ioout,'(''                            z = '',f13.6,'' e.Angs'')') dipolez
        if (lfieldcfg(ii)) then
          write(ioout,'(/,''  Electric field applied   = '',f13.6,'' eV/Ang.e'')') fieldcfg(ii)
          write(ioout,'(''  Electric field direction = '',f13.6,'' a '')') fielddirectioncfg(1,ii)
          write(ioout,'(''                             '',f13.6,'' y '')') fielddirectioncfg(2,ii)
          write(ioout,'(''                             '',f13.6,'' z '')') fielddirectioncfg(3,ii)
        endif
      elseif (ndimen(ii).eq.0) then
        call getdipole0D(dipolex,dipoley,dipolez)
        write(ioout,'(/,''  Initial cluster dipoles : x = '',f13.6,'' e.Angs'')') dipolex
        write(ioout,'(''                            y = '',f13.6,'' e.Angs'')') dipoley
        write(ioout,'(''                            z = '',f13.6,'' e.Angs'')') dipolez
        if (lfieldcfg(ii)) then
          fieldnorm = fielddirectioncfg(1,ii)**2 + fielddirectioncfg(2,ii)**2 + fielddirectioncfg(3,ii)**2
          fieldnorm = fieldcfg(ii)/sqrt(fieldnorm)
          write(ioout,'(/,''  Electric field applied  : x = '',f13.6,'' eV/Ang.e'')') fielddirectioncfg(1,ii)*fieldnorm
          write(ioout,'(  ''                          : y = '',f13.6,'' eV/Ang.e'')') fielddirectioncfg(2,ii)*fieldnorm
          write(ioout,'(  ''                          : z = '',f13.6,'' eV/Ang.e'')') fielddirectioncfg(3,ii)*fieldnorm
        endif
        if (lradialcfg(ii)) then
          write(ioout,'(/,''  Radial force applied    : K = '',f13.6,'' eV/Ang**2'')') radialKcfg(ii)
          write(ioout,'(  ''                          : x = '',f13.6,'' Ang'')') radialXYZcfg(1,ii)
          write(ioout,'(  ''                          : y = '',f13.6,'' Ang'')') radialXYZcfg(2,ii)
          write(ioout,'(  ''                          : z = '',f13.6,'' Ang'')') radialXYZcfg(3,ii)
        endif
      endif
      if (lshrink) then
        if (ndimen(ii).eq.3) then
          write(ioout,'(/,''  Shrinking factors = '',3(4x,i2))') nxks(ii),nyks(ii),nzks(ii)
        elseif (ndimen(ii).eq.2) then
          write(ioout,'(/,''  Shrinking factors = '',2(4x,i2))') nxks(ii),nyks(ii)
        elseif (ndimen(ii).eq.1) then
          write(ioout,'(/,''  Shrinking factor = '',4x,i2)') nxks(ii) 
        endif
      endif
      write(ioout,'(/,''  Temperature of configuration = '',g10.4,'' K '')') tempcfg(ii)
      if (ndimen(ii).eq.3) then
        write(ioout,'(/,''  Pressure of configuration = '',f13.3,'' GPa '')') presscfg(ii)
        if (lanisotropicpresscfg(ii)) then
          if (ndimen(ii).eq.3) then
            write(ioout,'(/,''  Anisotropic Pressure Tensor (GPa) : xx = '',f13.3)') anisotropicpresscfg(1,ii)
            write(ioout,'(''                                    : yy = '',f13.3)') anisotropicpresscfg(2,ii)
            write(ioout,'(''                                    : zz = '',f13.3)') anisotropicpresscfg(3,ii)
            write(ioout,'(''                                    : yz = '',f13.3)') anisotropicpresscfg(4,ii)
            write(ioout,'(''                                    : xz = '',f13.3)') anisotropicpresscfg(5,ii)
            write(ioout,'(''                                    : xy = '',f13.3)') anisotropicpresscfg(6,ii)
          elseif (ndimen(ii).eq.2) then
            write(ioout,'(/,''  Anisotropic Pressure Tensor (GPa) : xx = '',f13.3)') anisotropicpresscfg(1,ii)
            write(ioout,'(''                                    : yy = '',f13.3)') anisotropicpresscfg(2,ii)
            write(ioout,'(''                                    : xy = '',f13.3)') anisotropicpresscfg(3,ii)
          elseif (ndimen(ii).eq.1) then
            write(ioout,'(/,''  Anisotropic Pressure Tensor (GPa) : xx = '',f13.3)') anisotropicpresscfg(1,ii)
          endif
        endif
      endif
      if (.not.lterseincoords) then
        if (ndimen(ii).eq.3) then
          if (.not.lpredict) write(ioout,'(/,''  Fractional coordinates of asymmetric unit :'',/)')
        elseif (ndimen(ii).eq.2) then
          write(ioout,'(/,''  Mixed fractional/Cartesian coordinates of surface :'',/)')
        elseif (ndimen(ii).eq.1) then
          write(ioout,'(/,''  Mixed fractional/Cartesian coordinates of polymer :'',/)')
        else
          write(ioout,'(/,''  Cartesian coordinates of cluster :'',/)')
        endif
      endif
      if (.not.lpredict) then
        if (.not.lterseincoords) then
          write(ioout,'(''--------------------------------------------------------------------------------'')')
          write(ioout,'(''   No.  Atomic       x           y          z         Charge      Occupancy'')')
          if (ndimen(ii).eq.3) then
            write(ioout,'(''        Label      (Frac)      (Frac)     (Frac)        (e)         (Frac)  '')')
          elseif (ndimen(ii).eq.2) then
            write(ioout,'(''        Label      (Frac)      (Frac)     (Angs)        (e)         (Frac)  '')')
          elseif (ndimen(ii).eq.1) then
            write(ioout,'(''        Label      (Frac)      (Angs)     (Angs)        (e)         (Frac)  '')')
          else
            write(ioout,'(''        Label      (Angs)      (Angs)     (Angs)        (e)         (Frac)  '')')
          endif
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        endif
        do nr = 1,nregions(ii)
          if (nregions(ii).gt.1) then
            if (lregionrigid(nr,ncf)) then
              fixstring = ' '
              lfixeddirection = .false.
              if (.not.lopfreg(3*(nr-1)+1,ii)) then
                fixstring(1:1) = 'x'
                lfixeddirection = .true.
              endif
              if (.not.lopfreg(3*(nr-1)+2,ii)) then
                fixstring(2:2) = 'y'
                lfixeddirection = .true.
              endif
              if (.not.lopfreg(3*(nr-1)+3,ii)) then
                fixstring(3:3) = 'z'
                lfixeddirection = .true.
              endif
              if (.not.lterseincoords) then
                if (lfixeddirection) then
                  if (nregiontype(nr,ii).eq.1) then
                    write(ioout,'(''  Region '',i1,'' :  QM : Rigid translation fixed in '',a3)') nr,fixstring
                  elseif (nregiontype(nr,ii).eq.2) then
                    write(ioout,'(''  Region '',i1,'' :  MM : Rigid translation fixed in '',a3)') nr,fixstring
                  else
                    write(ioout,'(''  Region '',i1,'' :  Rigid translation fixed in '',a3)') nr,fixstring
                  endif
                else
                  if (nregiontype(nr,ii).eq.1) then
                    write(ioout,'(''  Region '',i1,'' : QM : Rigid translation '')') nr
                  elseif (nregiontype(nr,ii).eq.2) then
                    write(ioout,'(''  Region '',i1,'' : MM : Rigid translation '')') nr
                  else
                    write(ioout,'(''  Region '',i1,'' : Rigid translation '')') nr
                  endif
                endif
              endif
            else
              if (.not.lterseincoords) then
                if (nregiontype(nr,ii).eq.1) then
                  write(ioout,'(''  Region '',i1,'' : QM '')') nr
                elseif (nregiontype(nr,ii).eq.2) then
                  write(ioout,'(''  Region '',i1,'' : MM '')') nr
                else
                  write(ioout,'(''  Region '',i1,'' : '')') nr
                endif
              endif
            endif
            write(ioout,'(''--------------------------------------------------------------------------------'')')
          endif
          do i = 1,nasym
            if (nregionno(nsft+i).eq.nr) then
              inat = iatn(i)
              itype = natype(i)
!
!  Hide shells?
!
              if (inat.le.maxele.or..not.lhideshells) then
                call label(inat,itype,lab)
                if (lbsmat(i+nsft)) then
                  cstype = 'bc'
                  if (inat.gt.maxele) cstype = 'bs'
                else
                  cstype = 'c '
                  if (inat.gt.maxele) cstype = 's '
                endif
                ind = nstrains - 2 + 3*i
                if (ltmp(ind)) then
                  ocha(1) = '*'
                else
                  ocha(1) = ' '
                endif
                if (ltmp(ind+1)) then
                  ocha(2) = '*'
                else
                  ocha(2) = ' '
                endif
                if (ltmp(ind+2)) then
                  ocha(3) = '*'
                else
                  ocha(3) = ' '
                endif
                if (lfix(i)) then
                  fixed = 'fix'
                else
                  fixed = '   '
                endif
                if (.not.lterseincoords) then
                  if (ndimen(ii).eq.3) then
                    if (lrhombo) then
                      nri = nrel2(i)
                      write(ioout,'(i7,1x,a5,1x,a2,2x,f9.6,2(1x,a1,1x,f9.6),1x,a1,1x,f9.5,f12.6,1x,a3)') &
                        i,lab,cstype,xfrac(nri),ocha(1),yfrac(nri),ocha(2),zfrac(nri),ocha(3),qa(i),occua(i),fixed
                    else
                      write(ioout,'(i7,1x,a5,1x,a2,2x,f9.6,2(1x,a1,1x,f9.6),1x,a1,1x,f9.5,f12.6,1x,a3)') &
                        i,lab,cstype,xafrac(i),ocha(1),yafrac(i),ocha(2),zafrac(i),ocha(3),qa(i),occua(i),fixed
                    endif
                  elseif (ndimen(ii).eq.2) then
                    write(ioout,'(i7,1x,a5,1x,a2,2x,2(f9.6,1x,a1,1x),f9.4,1x,a1,1x,f9.5,f12.6,1x,a3)') &
                      i,lab,cstype,xafrac(i),ocha(1),yafrac(i),ocha(2),zafrac(i),ocha(3),qa(i),occua(i),fixed
                  elseif (ndimen(ii).eq.1) then
                    write(ioout,'(i7,1x,a5,1x,a2,2x,f9.6,2(1x,a1,1x,f9.4),1x,a1,1x,f9.5,f12.6,1x,a3)') &
                      i,lab,cstype,xafrac(i),ocha(1),yafrac(i),ocha(2),zafrac(i),ocha(3),qa(i),occua(i),fixed
                  else
                    write(ioout,'(i7,1x,a5,1x,a2,2x,f9.4,2(1x,a1,1x,f9.4),1x,a1,1x,f9.5,f12.6,1x,a3)') &
                      i,lab,cstype,xafrac(i),ocha(1),yafrac(i),ocha(2),zafrac(i),ocha(3),qa(i),occua(i),fixed
                  endif
                endif
              endif
            endif
          enddo
          write(ioout,'(''--------------------------------------------------------------------------------'')')
        enddo
      endif
      write(ioout,'(/)')
      if (ndimen(ii).eq.2) then
!
!  Check for growth slice output
!
        lslice = .false.
        i = 0
        do while (i.lt.nasym.and..not.lslice)
          i = i + 1
          lslice = lsliceatom(nsft + i)
        enddo
        if (lslice) then
          if (.not.lterseincoords) then
            write(ioout,'(/,''  Growth Slice : '',/)')
            write(ioout,'(''  Number of formula units in slice = '',i4,/)') nzmol
            write(ioout,'(''  Mixed fractional/Cartesian coordinates'',/)')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
            write(ioout,'(''   No.  Atomic       x           y          z         Charge      Occupancy'')')
            write(ioout,'(''        Label      (Frac)      (Frac)     (Angs)        (e)         (Frac)  '')')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
          endif
          do i = 1,nasym
            if (lsliceatom(nsft+i)) then
              inat = iatn(i)
              itype = natype(i)
!
!  Hide shells?
!
              if (inat.le.maxele.or..not.lhideshells) then
                call label(inat,itype,lab)
                if (lbsmat(i+nsft)) then
                  cstype = 'bc'
                  if (inat.gt.maxele) cstype = 'bs'
                else
                  cstype = 'c '
                  if (inat.gt.maxele) cstype = 's '
                endif
                if (.not.lterseincoords) then
                  write(ioout,'(i7,1x,a5,1x,a2,2x,2(f9.6,3x),f9.4,3x,f9.4,f12.6,1x)') &
                    i,lab,cstype,xafrac(i),yafrac(i),zafrac(i),qa(i),occua(i)
                endif
              endif
            endif
          enddo
          write(ioout,'(''--------------------------------------------------------------------------------'')')
          write(ioout,'(/)')
        endif
      endif
      if (ndimen(ii).gt.0.and.index(keyword,'cart').ne.0) then
        write(ioout,'(''  Initial Cartesian coordinates :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''   No.  Atomic        x           y           z          Charge   Occupancy'')')
        write(ioout,'(''        Label       (Angs)      (Angs)      (Angs)        (e)       (Frac)  '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        do i = 1,numat
          inat = nat(i)
          itype = nftype(i)
!
!  Hide shells?
!
          if (inat.le.maxele.or..not.lhideshells) then
            call label(inat,itype,lab)
            if (lbsmat(nrelat(i)+nsft)) then
              cstype = 'bc'
              if (inat.gt.maxele) cstype = 'bs'
            else
              cstype = 'c '
              if (inat.gt.maxele) cstype = 's '
            endif
            write(ioout,'(i7,1x,a5,1x,a2,5f12.6)')i,lab,cstype,xclat(i),yclat(i),zclat(i),qf(i),occuf(i)
          endif
        enddo
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(/)')
      endif
!
!  External force output
!
      lfirstout = .true.
      do i = 1,nasym
        forcenorm = abs(forcecfg(1,nsft+i)) + abs(forcecfg(2,nsft+i)) + abs(forcecfg(3,nsft+i))
        if (forcenorm.gt.1.0d-6) then
          if (lfirstout) then
          lfirstout = .false.
          write(ioout,'(''  External Cartesian forces on asymmetric unit:'',/)')
          write(ioout,'(''--------------------------------------------------------------------------------'')')
          write(ioout,'(''   No.  Atomic         Fx            Fy            Fz         '')')
          write(ioout,'(''        Label       (eV/Angs)     (eV/Angs)    (eV/Angs)      '')')
          write(ioout,'(''--------------------------------------------------------------------------------'')')
          endif
          inat = iatn(i)
          itype = natype(i)
          call label(inat,itype,lab)
          if (lbsmat(nrelat(i)+nsft)) then
            cstype = 'bc'
            if (inat.gt.maxele) cstype = 'bs'
          else
            cstype = 'c '
            if (inat.gt.maxele) cstype = 's '
          endif
          write(ioout,'(i7,1x,a5,1x,a2,3f14.6)')  &
            i,lab,cstype,forcecfg(1,nsft+i),forcecfg(2,nsft+i),forcecfg(3,nsft+i)
        endif
      enddo
      if (.not.lfirstout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
!
!  Time-dependent external force output
!
      lfirstout = .true.
      do i = 1,nasym
        do j = 1,3
          if (ltdforcecfg(j,nsft+i)) then
            if (lfirstout) then
              lfirstout = .false.
              write(ioout,'(''  Time-dependent external Cartesian forces on asymmetric unit:'',/)')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
              write(ioout,'(''   No.  Atomic   Direction       FA            FB             FC         '')')
              write(ioout,'(''        Label                 (eV/Angs)      (1/ps)      (Fraction 2xpi)      '')')
              write(ioout,'(''--------------------------------------------------------------------------------'')')
            endif
            inat = iatn(i)
            itype = natype(i)
!
!  Hide shells?
!
            if (inat.le.maxele.or..not.lhideshells) then
              call label(inat,itype,lab)
              if (lbsmat(nrelat(i)+nsft)) then
                cstype = 'bc'
                if (inat.gt.maxele) cstype = 'bs'
              else
                cstype = 'c '
                if (inat.gt.maxele) cstype = 's '
              endif
              write(ioout,'(i7,1x,a5,1x,a2,6x,a1,3x,3f14.6)')  &
                i,lab,cstype,crd(j),tdforcecfg(1,j,nsft+i),tdforcecfg(2,j,nsft+i),tdforcecfg(3,j,nsft+i)
            endif
          endif
        enddo
      enddo
      if (.not.lfirstout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
!
!  Einstein model output
!
      lfirstout = .true.
      do i = 1,nasym
        if (leinsteinat(nsft+i)) then
          if (lfirstout) then
            lfirstout = .false.
            write(ioout,'(''  Einstein Model sites/force constants for asymmetric unit :'',/)')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
            write(ioout,'(''   No.  Atomic         x             y            z         Force constant'')')
            write(ioout,'(''        Label        (frac)        (frac)       (frac)       (eV/Angs**2)'')')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
          endif
          inat = iatn(i)
          itype = natype(i)
!
!  Hide shells?
!
          if (inat.le.maxele.or..not.lhideshells) then
            call label(inat,itype,lab)
            if (lbsmat(nrelat(i)+nsft)) then
              cstype = 'bc'
              if (inat.gt.maxele) cstype = 'bs'
            else
              cstype = 'c '
              if (inat.gt.maxele) cstype = 's '
            endif
            write(ioout,'(i7,1x,a5,1x,a2,4f14.6)') &
              i,lab,cstype,xeinsteinat(nsft+i),yeinsteinat(nsft+i),zeinsteinat(nsft+i),keinsteinat(nsft+i)
          endif
        endif
      enddo
      if (.not.lfirstout) then
        write(ioout,'(''--------------------------------------------------------------------------------'',/)')
      endif
    endif
!********************
!  Molecule output  *
!********************
    if (lmol.and.ioproc) call outmol
!**********************
!  Constraint output  *
!**********************
    if (ncon.gt.0.and.ioproc) then
      write(ioout,'(''  Constraints : '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''  Constraint no.      Unconstrained     Constrained    Coefficient    Offset'')')
      write(ioout,'(''                         Variable         Variable'')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      ncm = 3*nasym + nstrains
      do i = 1,ncon
        ncvi = ncvarcfg(i+ncfst)
        if (ncvi.le.nstrains) then
          nfv = ncfixcfg(i+ncfst)
          write(ioout,'(8x,i4,14x,''Strain '',i1,8x,''Strain '',i1,4x,f10.5,5x,f7.4)') &
            i,ncvi,nfv,concocfg(i+ncfst),conaddcfg(i+ncfst)
        elseif (ncvi.gt.ncm) then
          ncvi = ncvi - ncm
          nfv = ncfixcfg(i+ncfst) - ncm
          write(ioout,'(6x,i6,14x,''Radius '',i4,5x,''Radius '',i4,1x,f10.5,5x,f7.4)') &
            i,ncvi,nfv,concocfg(i+ncfst),conaddcfg(i+ncfst)
        else
          nfv = ncfixcfg(i+ncfst) - (nstrains+1)
          nvv = ncvi - (nstrains+1)
          nfv1 = (nfv/3) + 1
          nvv1 = (nvv/3) + 1
          ncrf = nfv - 3*(nfv1-1) + 1
          ncrv = nvv - 3*(nvv1-1) + 1
          write(ioout,'(6x,i6,12x,i6,1x,a1,8x,i6,1x,a1,6x,f10.5,5x,f7.4)') &
            i,nvv1,crd(ncrv),nfv1,crd(ncrf),concocfg(i+ncfst),conaddcfg(i+ncfst)
        endif
      enddo
      write(ioout,'(''--------------------------------------------------------------------------------'',/)')
    endif
    if (ldist.or.lbond) then
      if (ndimen(ii).eq.3) then
        do i = 1,numat
          xclat(i) = xfrac(i)*rv(1,1) + yfrac(i)*rv(1,2) + zfrac(i)*rv(1,3)
          yclat(i) = xfrac(i)*rv(2,1) + yfrac(i)*rv(2,2) + zfrac(i)*rv(2,3)
          zclat(i) = xfrac(i)*rv(3,1) + yfrac(i)*rv(3,2) + zfrac(i)*rv(3,3)
        enddo
      elseif (ndimen(ii).eq.2) then
        do i = 1,numat
          xclat(i) = xfrac(i)*rv(1,1) + yfrac(i)*rv(1,2)
          yclat(i) = xfrac(i)*rv(2,1) + yfrac(i)*rv(2,2)
          zclat(i) = zfrac(i)
        enddo
      elseif (ndimen(ii).eq.1) then
        do i = 1,numat
          xclat(i) = xfrac(i)*rv(1,1)
          yclat(i) = yfrac(i)
          zclat(i) = zfrac(i)
        enddo
      endif
      do i = 1,nasym
        nr = nrel2(i)
        xalat(i) = xclat(nr)
        yalat(i) = yclat(nr)
        zalat(i) = zclat(nr)
      enddo
      if (ioproc) then
        if (lbond) call GULP_bond
        if (ldist) call distance
      endif
    endif
!
!  Option to print out valid angles
!
    if (ioproc) then
      call rlist
      if (nthb.gt.0.and.langle) call angle
      if (nfor.gt.0.and.ltors) call torsion
    endif
!*********************************
!  Configuration k point setups  *
!*********************************
    if (ndline.gt.0) then
      call setdisp
    endif
    if (lshrink) then
      if (ndim.eq.3) then
        call setkpt3D
      elseif (ndim.eq.2) then
        call setkpt2D
      elseif (ndim.eq.1) then
        call setkpt1D
      endif
    endif
!
!  If there is more than one k point per configuration or there
!  are too many atoms then warn that no eigenvectors will be produced.
!
    if (leigen) then
      nlkpt = 0
      do i = 1,nkpt
        nk = nkptcfg(i)
        if (nlkpt.eq.0.and.nk.eq.ii) nlkpt = i
      enddo
    endif
    if (nkpt.gt.0.and.index(keyword,'nokp').eq.0.and.ioproc) call outkpt
!**********************************************************
!  Include optimisation variables in fitting observables  *
!**********************************************************
    if (lfit) then
      if (lrelax) then
        do i = 1,nasym
          x0(3*i+nstrains-2) = xafrac(i)
          x0(3*i+nstrains-1) = yafrac(i)
          x0(3*i+nstrains) = zafrac(i)
        enddo
        nfst = n1var(ii) - 1
        do i = 1,nvarcfg(ii)
          ind = ioptcfg(i+nfst)
!
!  For relax fitting shell positions must not be included in
!  observables list
!
          lcore = .true.
          if (ind.gt.nstrains.and.ind.le.mvar) then
!
!  Coordinate variable - exclude shells
!
            indi = ind - nstrains
            na = ((indi-1)/3) + 1
            if (iatn(na).gt.maxele) lcore = .false.
          elseif (ind.gt.mvar) then
!
!  Variable is a radius or charge => exclude
!
            lcore = .false.
          endif
          if (lcore) then
            nobs = nobs + 1
            if (nobs.gt.maxobs) then
              maxobs = nobs + 20
              call changemaxobs
            endif
            nobtyp(nobs) = 6
            nobcfg(nobs) = ii
            nobptr(nobs) = i
            if (ind.le.nstrains) then
              if (ind.eq.1) then
                fobs(nobs) = a
              elseif (ind.eq.2) then
                fobs(nobs) = b
              elseif (ind.eq.3) then
!
!  For rhombohedral space groups, input in the rhombohedral setting the
!  second cell observable must be alpha instead of c for relax fitting.
!  Also for 2-D systems then quantity is alpha as well.
!
                if (ifhr(ii).eq.1.or.ndimen(ii).eq.2) then
                  fobs(nobs) = alpha
                else
                  fobs(nobs) = c
                endif
              elseif (ind.eq.4) then
                fobs(nobs) = alpha
              elseif (ind.eq.5) then
                fobs(nobs) = beta
              else
                fobs(nobs) = gamma
              endif
              if (ind.le.3) then
                if (weight(nobs).eq.0.0) weight(nobs) = delwht_cell_length
              else
                if (weight(nobs).eq.0.0) weight(nobs) = delwht_cell_angle
              endif
            else
              fobs(nobs) = x0(ind)
              if (ndimen(ncf).eq.3) then
                if (weight(nobs).eq.0.0) weight(nobs) = delwht_frac
              else
                if (weight(nobs).eq.0.0) weight(nobs) = delwht_coord
              endif
            endif
          endif
        enddo
      else
        nfst = n1var(ii) - 1
        nobsold = nobs
        do i = 1,nvarcfg(ii)
          nobs = nobs + 1
          if (nobs.gt.maxobs) then
            maxobs = nobs + 20
            call changemaxobs
          endif
          fobs(nobs) = 0.0_dp
          nobtyp(nobs) = 2
          nobcfg(nobs) = ii
          nobptr(nobs) = i
          if (weight(nobs).eq.0.0) weight(nobs) = delwht_grad
        enddo
!
!  Look for user specified gradients to fit
!
        nlfgrad = 0
        if (nfgrad.gt.0) then
          nlfgra = 0
          nufgra = 0
          do k = 1,nfgrad
            if (nfgracfg(k).eq.ii) then
              nufgra = k
              if (nlfgra.eq.0) nlfgra = k
            endif
          enddo
          if (nlfgra.gt.0) nlfgrad = nufgra - nlfgra + 1
        endif
        if (nlfgrad.gt.0) then
          if (nobsold+nvarcfg(ii).gt.maxobs) then
            maxobs = nobsold+nvarcfg(ii) + 20
            call changemaxobs
          endif
          do i = 1,nvarcfg(ii)
            ind = ioptcfg(i+nfst)
            if (ind.gt.nstrains.and.ind.le.mvar) then
              nj = (ind - (nstrains-2))/3
              do k = nlfgra,nufgra
                if (nfgrat(k).eq.nj) then
                  idj = ind - (3*nj+ (nstrains-3))
                  fobs(nobsold+i) = fgrad(3*(k-1)+idj)
                  weight(nobsold+i) = fgradweight(k)
                endif
              enddo
            endif
          enddo
        endif
!   
!  Look for user specified stresses to fit
!   
        nlfstress = 0
        if (nfstress.gt.0) then
          nlfstr = 0
          nufstr = 0
          do k = 1,nfstress 
            if (nfstrcfg(k).eq.ii) then
              nufstr = k
              if (nlfstr.eq.0) nlfstr = k
            endif
          enddo 
          if (nlfstr.gt.0) nlfstress = nufstr - nlfstr + 1
        endif 
        if (nlfstress.gt.0) then
          if (nobsold+nvarcfg(ii).gt.maxobs) then 
            maxobs = nobsold + nvarcfg(ii) + 20
            call changemaxobs 
          endif
          do i = 1,nvarcfg(ii)
            ind = ioptcfg(i+nfst)
            if (ind.le.nstrains.and.ind.le.mvar) then
              do k = nlfstr,nufstr
                if (nfstrt(k).eq.ind) then
                  fobs(nobsold+i) = fstress(k)
                  weight(nobsold+i) = fstressweight(k)
                endif
              enddo 
            endif
          enddo 
        endif
      endif
    endif
!
!  Free array for optimisation logicals
!
    deallocate(ltmp,stat=status)
    if (status/=0) call deallocate_error('setcfg','ltmp')
!*****************************
!  End of configuration loop *
!*****************************
  enddo
!
!  Check if phonon fitting is required
!
  lphonfit = .false.
  if (lfit) then
    do i = 1,nobs
      if (nobtyp(i).eq.9.or.nobtyp(i).eq.13.or.nobtyp(i).eq.14) then
        if (ndimen(nobcfg(i)).gt.0) lphonfit = .true.
      endif
    enddo
  endif
!
!  Set default k points if phonon specified but no k points given
!  Modified so that each configuration is checked individually.
!
  do ii = 1,ncfg
    nlkpt = 0
    do j = 1,nkpt
      if (nkptcfg(j).eq.ii) nlkpt = nlkpt + 1
    enddo
!
!  Create space in k point arrays for any new points
!
    if (nkpt+1.gt.maxkpt) then
      maxkpt = nkpt + 1
      call changemaxkpt
    endif
    if (nlkpt.eq.0.and.(lphon.or.lphonfit).and.ndline.eq.0) then
      if (nkpt.gt.0) then
        k = 0
        nkp = 0
        do while (k.lt.nkpt.and.nkp.lt.ii)
          k = k + 1
          nkp = nkptcfg(k)
        enddo
        if (nkp.lt.ii) then
          k = nkpt + 1
        else
          do j = nkpt,k,-1
            xkpt(j+1) = xkpt(j)
            ykpt(j+1) = ykpt(j)
            zkpt(j+1) = zkpt(j)
            wkpt(j+1) = wkpt(j)
            nkptcfg(j+1) = nkptcfg(j)
            lkptdispersion(j+1) = lkptdispersion(j)
          enddo
        endif
      else
        k = 1
      endif
      xkpt(k) = 0.0_dp
      ykpt(k) = 0.0_dp
      zkpt(k) = 0.0_dp
      wkpt(k) = 1.0_dp
      nkptcfg(k) = ii
      lkptdispersion(k) = .false.
      nkpt = nkpt + 1
    elseif (nlkpt.eq.0.and.lfree) then
      if (nkpt.gt.0) then
        k = 0
        nkp = 0
        do while (k.lt.nkpt.and.nkp.lt.ii)
          k = k + 1
          nkp = nkptcfg(k)
        enddo
        if (nkp.lt.ii) then
          k = nkpt + 1
        else
          do j = nkpt,k,-1
            xkpt(j+1) = xkpt(j)
            ykpt(j+1) = ykpt(j)
            zkpt(j+1) = zkpt(j)
            wkpt(j+1) = wkpt(j)
            nkptcfg(j+1) = nkptcfg(j)
            lkptdispersion(j+1) = lkptdispersion(j)
          enddo
        endif
      else
        k = 1
      endif
      xkpt(k) = 0.5_dp
      if (ndimen(ii).ge.2) then
        ykpt(k) = 0.5_dp
      else
        ykpt(k) = 0.0_dp
      endif
      if (ndimen(ii).eq.3) then
        zkpt(k) = 0.5_dp
      else
        zkpt(k) = 0.0_dp
      endif
      wkpt(k) = 1.0_dp
      nkptcfg(k) = ii
      lkptdispersion(k) = .false.
      nkpt = nkpt + 1
    endif
  enddo
!
  return
  end
