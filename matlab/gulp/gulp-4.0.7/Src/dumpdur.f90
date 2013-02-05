  subroutine dumpdur(iout,ncycle)
!
!  Dump out modified copy of the input file for restarts
!  Version that can be called during fitting and optimisation
!  without disturbing the current setup
!
!  ncycle = cycle number of call
!
!   8/98 Refractive indices added
!   3/01 Surface related data added
!   4/01 Connectivity lists added
!   8/01 Piezoelectric constant format changed
!  11/01 Growth slice specifiers added
!  12/01 Output of fixed atom indicator added for noflag case
!   3/02 Born charge modifications added
!   3/02 Frequency-dependent properties added
!   5/02 Shift scaling added
!   5/02 Weights added to observable lines
!   5/02 K point added for frequency observable
!   5/02 Option to output in Cartesian coordinates added
!        including passing rvcfg to wcoord - note this is
!        potentially problematic and not an advertised
!        feature!
!   7/02 Blanks at end of keyword line removed
!   8/02 Output of external forces added
!  10/02 Output of MD constraint added
!  11/02 Output of Einstein model data added
!   2/03 Output of NEB data added
!   5/03 Region 3 modifications added
!   9/03 Rigid region flag introduced
!  12/03 Output of symmetry operators added
!   2/04 Time dependent forces added
!   4/04 Dimensionality passed to wcoord
!   4/04 Cell vectors now output for MD case to preserve orientation relative to coords
!  12/04 Format of temperature output changed
!   1/05 SAS exclusion options added
!   1/05 Dumping of resetvectors option added
!   6/05 Monopoleq observable added
!   8/05 Output of symmetry_celltype added
!   9/05 Handling of timestep output corrected
!   3/05 Omega damping factor added
!   5/06 Stress added to observables
!   7/06 f90 advance I/O option replaces dollar sign
!   8/06 Connection type option output added
!  11/06 NEB modifications added
!  11/06 Replica number added to output
!  11/06 Radii added to NEB output
!   1/07 Amide bond type added
!   2/07 Electric field added
!   3/07 Radial force added
!   4/07 Searching for bonded atoms now uses nbonds number rather
!        than checking for a non-zero type
!   5/07 Format for temperature altered
!   5/07 nregiontype added
!   5/07 QMMMmode added
!   7/07 MC restart info added
!   7/07 Output of metadynamics parameters added
!   7/07 Output of history of metadynamics parameters added
!  11/07 Unused variables removed
!   3/08 New MD integrator added
!   4/08 ReaxFF charge, bond and bond angle added as observables
!   7/08 Spelling of equilibrate corrected
!   8/08 Distance added as a metadynamics variable
!   9/08 Coordinates added as metadynamics variables
!  10/08 Option to not overwrite dumpfile added. Instead a number is added to 
!        the dumpfile name.
!  10/08 COSMO/COSMIC merge performed
!  10/08 Incorrect use of nasym replaced by nlasym
!  12/09 Region sub-option tag added to cartesian output
!   1/10 Young's moduli and Poisson's ratios added
!   1/10 Incorrect ordering of region & rigid for one case fixed
!   3/10 nlsft added to referencing of einstein coordinate arrays
!   6/10 Output of tether option added if this is an MD run.
!   7/10 Output of coordination number for fitting added
!   7/10 Format of MD dump changed to give more precision on timestep.
!   8/10 Extra molecule information added to output of existing GCMC molecules.
!   8/10 Output of p_iso and p_flx added for MD restarting
!   8/10 Format statement for atom numbers extended to i7 in MD restart
!   9/10 S(Q,omega) fitting added
!   2/11 Hiding of shells added
!   6/11 Electric field enabled for periodic systems
!   9/11 Metadynamics internal code replaced with Plumed
!   5/12 Current average atomic stress output added
!   6/12 Output of mode observable added
!   6/12 File handling changed to allow for spaces in names
!   8/12 Format for output of shift modified to allow for larger numbers
!  12/12 Time-dependent field added
!  12/12 delayforce and endforce renamed to delay_force and end_force
!  12/12 delay_field and end_field added
!  12/12 Modified to allow for multiple time-dependent fields
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
!  Julian Gale, NRI, Curtin University, December 2012
!
  use configurations
  use control
  use cosmo
  use current
  use defects
  use derivatives,        only : sumatomicstress
  use dispersion
  use distances,          only : ndistancereset
  use dump
  use element,            only : maxele
  use energies,           only : esurface
  use field
  use general
  use genetic,            only : xmaxcfg,xmincfg
  use ksample
  use mdlogic
  use moldyn
  use molecule
  use montecarlo
  use m_pr,               only : taubcfg, tautcfg, pr_conscfg, pr_cons, p_iso, p_flx
  use m_pr,               only : lpisooutput, lpflxoutput
  use neb
  use observables
  use parallel
  use potentialgrid
  use potentialpoints
  use potentialsites
  use projectdos
  use radial
  use scan
  use scatterdata,        only : sofomega_filename
  use shifts
  use symmetry
  use velocities
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in)                 :: iout
  integer(i4),      intent(in)                 :: ncycle
!
!  Local variables
!
  character(len=1)                             :: crd(3)
  character(len=2)                             :: crd2(6)
  character(len=2)                             :: qmmmstring
  character(len=3)                             :: fixstring
  character(len=1)                             :: hbr(7)
  character(len=1)                             :: tchar
  character(len=4)                             :: lab2
  character(len=5)                             :: lab1,lab3,word5
  character(len=7)                             :: fixword
  character(len=9)                             :: conword(2)
  character(len=20)                            :: cdumps
  character(len=20)                            :: cextension
  character(len=80)                            :: line
  character(len=80)                            :: line2
  character(len=90)                            :: dfilelocal
  integer(i4)                                  :: endpoint
  integer(i4)                                  :: i
  integer(i4)                                  :: iblank
  integer(i4)                                  :: ic
  integer(i4)                                  :: icm
  integer(i4)                                  :: id1
  integer(i4)                                  :: id2
  integer(i4)                                  :: id3
  integer(i4)                                  :: ifx
  integer(i4)                                  :: ify
  integer(i4)                                  :: ifz
  integer(i4)                                  :: ii
  integer(i4)                                  :: ii1
  integer(i4)                                  :: ii2
  integer(i4)                                  :: ii3
  integer(i4)                                  :: imatm
  integer(i4)                                  :: imatmend
  integer(i4)                                  :: imatmlast
  integer(i4)                                  :: imagex
  integer(i4)                                  :: imagey
  integer(i4)                                  :: imagez
  integer(i4)                                  :: imm
  integer(i4)                                  :: imol
  integer(i4)                                  :: imolgroup
  integer(i4)                                  :: inat
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: indii
  integer(i4)                                  :: insertpoint
  integer(i4)                                  :: inum
  integer(i4)                                  :: iptr
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4), dimension(:), allocatable       :: itmp2
  integer(i4)                                  :: itype
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: mlvar
  integer(i4)                                  :: nc
  integer(i4)                                  :: ncblold
  integer(i4)                                  :: ncc
  integer(i4)                                  :: ncm
  integer(i4)                                  :: nconword
  integer(i4)                                  :: ncrf
  integer(i4)                                  :: ncrv
  integer(i4)                                  :: ndefst
  integer(i4)                                  :: ndf
  integer(i4)                                  :: nds
  integer(i4)                                  :: ndt
  integer(i4)                                  :: nexistgcmcmol
  integer(i4)                                  :: nf
  integer(i4)                                  :: nfv
  integer(i4)                                  :: nfv1
  integer(i4)                                  :: nimp
  integer(i4)                                  :: ninst
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nlasym
  integer(i4)                                  :: nlcfst
  integer(i4)                                  :: nlcon
  integer(i4)                                  :: nldef
  integer(i4)                                  :: nlfgra
  integer(i4)                                  :: nlfstr
  integer(i4)                                  :: nlfgrad
  integer(i4)                                  :: nlfstress
  integer(i4)                                  :: nlfst
  integer(i4)                                  :: nlkpt
  integer(i4)                                  :: nllkpt
  integer(i4)                                  :: nlpdp
  integer(i4)                                  :: nlsft
  integer(i4)                                  :: nlshift
  integer(i4)                                  :: nlvar
  integer(i4)                                  :: nm
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmol1
  integer(i4)                                  :: nmol2
  integer(i4)                                  :: nmolgroup
  integer(i4)                                  :: nonline
  integer(i4)                                  :: np
  integer(i4)                                  :: npc
  integer(i4)                                  :: npi
  integer(i4)                                  :: npifirst
  integer(i4)                                  :: npilast
  integer(i4)                                  :: npfirst
  integer(i4)                                  :: nplast
  integer(i4)                                  :: npro
  integer(i4)                                  :: nproj
  integer(i4)                                  :: nprojd
  integer(i4)                                  :: nptr
  integer(i4)                                  :: nr
  integer(i4)                                  :: nr1
  integer(i4)                                  :: nri
  integer(i4)                                  :: nsc
  integer(i4)                                  :: nsearch1
  integer(i4)                                  :: nsearch2
  integer(i4)                                  :: nt
  integer(i4)                                  :: nufgra
  integer(i4)                                  :: nufstr
  integer(i4)                                  :: nukpt
  integer(i4)                                  :: nupdp
  integer(i4)                                  :: nv
  integer(i4)                                  :: nvv
  integer(i4)                                  :: nvv1
  integer(i4)                                  :: nvac
  integer(i4)                                  :: nwptr
  integer(i4)                                  :: nxk
  integer(i4)                                  :: nyk
  integer(i4)                                  :: nzk
  integer(i4)                                  :: status
  logical                                      :: ldiff
  logical                                      :: lend
  logical                                      :: ldqm
  logical                                      :: lbre
  logical                                      :: lextension
  logical                                      :: lfirstfind
  logical                                      :: lfirstout
  logical                                      :: lrhombo
  logical                                      :: llsym
  logical                                      :: lstillfixed
  real(dp)                                     :: acl
  real(dp)                                     :: bcl
  real(dp)                                     :: ccl
  real(dp)                                     :: alpcl
  real(dp)                                     :: betcl
  real(dp)                                     :: gamcl
  real(dp)                                     :: forcenorm
  real(dp)                                     :: norm
  real(dp)                                     :: oc
  real(dp)                                     :: q
  real(dp)                                     :: ri
  real(dp)                                     :: rtfct
  real(dp)                                     :: rtstp
  real(dp)                                     :: rvp(3,3)
  real(dp)                                     :: shc
  real(dp)                                     :: sum
  real(dp)                                     :: vsq
  real(dp)                                     :: vx
  real(dp)                                     :: vy
  real(dp)                                     :: vz
  real(dp)                                     :: x2i
  real(dp)                                     :: y2i
  real(dp)                                     :: z2i
  real(dp)                                     :: x3i
  real(dp)                                     :: y3i
  real(dp)                                     :: z3i
  real(dp)                                     :: x4i
  real(dp)                                     :: y4i
  real(dp)                                     :: z4i
  real(dp)                                     :: x5i
  real(dp)                                     :: y5i
  real(dp)                                     :: z5i
  real(dp)                                     :: xal
  real(dp)                                     :: yal
  real(dp)                                     :: zal
!
  data crd/'x','y','z'/
  data crd2/'xx','yy','zz','yz','xz','xy'/
  data hbr/'P','A','B','C','F','I','R'/
!
  if (.not.ioproc) return
  nlshift = 0
  if (ldefect) rewind(42)
!
!  If dumpfile name has been given then open file
!
  if (dfile(1:1).ne.' ') then
    if (ldumpnooverwrite) then
!
!  Increment write number by one
!
      ndumpstep = ndumpstep + 1
!
!  Copy root name for dumpfile 
!
      dfilelocal = ' ' 
      dfilelocal(1:80) = dfile(1:80)
!
!  Find the point to insert the number
!
      insertpoint = index(dfile,'.')
      if (insertpoint.eq.0) then
        call endstring(dfile,len(dfile),insertpoint)
        lextension = .false.
      else
        lextension = .true.
      endif
      call endstring(dfile,len(dfile),endpoint) 
      endpoint = endpoint - 1
      if (lextension) then
        if (endpoint-insertpoint+1.gt.20) then
          call outerror('need to increase length of cextension in dumpdur',0_i4)
          call stopnow('dumpdur')
        endif
        cextension = dfile(insertpoint:endpoint)
      endif
!
!  Write in number and if there was an extension move
!
      if (ndumpstep.gt.9999999) then
        call outerror('number of dumpfile copies exceeds limit in dumpdur',0_i4)
        call stopnow('dumpdur')
      elseif (ndumpstep.gt.999999) then
        if (lextension) then
          dfilelocal(insertpoint:endpoint) = ' '
          dfilelocal(insertpoint+8:endpoint+8) = cextension(1:endpoint-insertpoint+1)
        endif
        dfilelocal(insertpoint:insertpoint) = '_'
        write(cdumps,'(i4)') ndumpstep
        dfilelocal(insertpoint+1:insertpoint+7) = cdumps(1:7)
      elseif (ndumpstep.gt.99999) then
        if (lextension) then
          dfilelocal(insertpoint:endpoint) = ' '
          dfilelocal(insertpoint+7:endpoint+7) = cextension(1:endpoint-insertpoint+1)
        endif
        dfilelocal(insertpoint:insertpoint) = '_'
        write(cdumps,'(i4)') ndumpstep
        dfilelocal(insertpoint+1:insertpoint+6) = cdumps(1:6)
      elseif (ndumpstep.gt.9999) then
        if (lextension) then
          dfilelocal(insertpoint:endpoint) = ' '
          dfilelocal(insertpoint+6:endpoint+6) = cextension(1:endpoint-insertpoint+1)
        endif
        dfilelocal(insertpoint:insertpoint) = '_'
        write(cdumps,'(i4)') ndumpstep
        dfilelocal(insertpoint+1:insertpoint+5) = cdumps(1:5)
      elseif (ndumpstep.gt.999) then
        if (lextension) then
          dfilelocal(insertpoint:endpoint) = ' '
          dfilelocal(insertpoint+5:endpoint+5) = cextension(1:endpoint-insertpoint+1)
        endif
        dfilelocal(insertpoint:insertpoint) = '_'
        write(cdumps,'(i4)') ndumpstep
        dfilelocal(insertpoint+1:insertpoint+4) = cdumps(1:4)
      elseif (ndumpstep.gt.99) then
        if (lextension) then
          dfilelocal(insertpoint:endpoint) = ' '
          dfilelocal(insertpoint+4:endpoint+4) = cextension(1:endpoint-insertpoint+1)
        endif
        dfilelocal(insertpoint:insertpoint) = '_'
        write(cdumps,'(i3)') ndumpstep
        dfilelocal(insertpoint+1:insertpoint+3) = cdumps(1:3)
      elseif (ndumpstep.gt.9) then
        if (lextension) then
          dfilelocal(insertpoint:endpoint) = ' '
          dfilelocal(insertpoint+3:endpoint+3) = cextension(1:endpoint-insertpoint+1)
        endif
        dfilelocal(insertpoint:insertpoint) = '_'
        write(cdumps,'(i2)') ndumpstep
        dfilelocal(insertpoint+1:insertpoint+2) = cdumps(1:2)
      else
        if (lextension) then
          dfilelocal(insertpoint:endpoint) = ' '
          dfilelocal(insertpoint+2:endpoint+2) = cextension(1:endpoint-insertpoint+1)
        endif
        dfilelocal(insertpoint:insertpoint) = '_'
        write(cdumps,'(i1)') ndumpstep
        dfilelocal(insertpoint+1:insertpoint+1) = cdumps(1:1)
      endif
!
!  Open file
!
      open(iout,file=dfilelocal,status='unknown')
    else
      open(iout,file=dfile,status='unknown')
    endif
  endif
!***************************
!  Write out keyword line  *
!***************************
  write(iout,'(''# '')')
  write(iout,'(''# Keywords:'')')
  write(iout,'(''# '')')
  nptr = 0
  nonline = 0
  do i = 1,80
    line(i:i) = ' '
  enddo
  do while (nptr.lt.400)
    nptr = nptr + 1
    iblank = index(keyword(nptr:nptr),' ')
!
!  Find start of word
!
    if (iblank.eq.0) then
      iblank = 0
      ii = 0
!
!  Find end of word
!
      do while (iblank.eq.0)
        ii = ii + 1
        iblank = index(keyword(nptr+ii:nptr+ii),' ')
      enddo
!
!  Is there space on the current line?
!
      if ((76-nonline).gt.ii) then
!
!  Yes - so add word
!
        line(nonline+1:nonline+ii) = keyword(nptr:nptr+ii-1)
        nonline = nonline + ii + 1
      else
!
!  No - so start a new line
!
        line(nonline+2:nonline+2) = '&'
        do k = 1,nonline + 2
          write(iout,'(1a1)',advance='no') line(k:k)
        enddo
        write(iout,'('' '')')
        do i = 1,80
          line(i:i) = ' '
        enddo
        line(1:ii) = keyword(nptr:nptr+ii-1)
        nonline = ii + 1
      endif
      nptr = nptr + ii
    endif
  enddo
!
!  If a line is partial written then output
!
  if (nonline.ne.0) then
    do k = 1,nonline
      write(iout,'(1a1)',advance='no') line(k:k)
    enddo
    write(iout,'('' '')')
  endif
  write(iout,'(''# '')')
  write(iout,'(''# Options:'')')
  write(iout,'(''# '')')
!**********************************************************************
!  Write out title
!**********************************************************************
  if (ntitle.gt.0) then
    write(iout,'(''title'')')
    do i = 1,ntitle
      write(iout,'(a)')titleword(i)
    enddo
    write(iout,'(''end'')')
  endif
!**********************************************************************
!  Write out comment containing cycle number
!**********************************************************************
  if (ncycle.gt.0) then
    if (lmd.and.timesofar.gt.1.0d-6) then
      write(iout,'(''# File written after '',f10.4,'' ps of molecular dynamics run'')') timesofar
    else
      if (nreg1.gt.0) then
        write(iout,'(''# File written after '',i4,'' cycles of defect calculation'')') ncycle
      else
        write(iout,'(''# File written after '',i4,'' cycles'')')ncycle
      endif
    endif
  elseif (ncycle.eq.0) then
    write(iout,'(''# File written after error had occurred'')')
  endif
!**********************************************************************
!  Write out option and data lines
!**********************************************************************
!*****************************
!  Loop over configurations  *
!*****************************
  npotpt0 = 0
  do nc = 1,ncfg
!
!  Structure name
!
    if (names(nc)(1:1).ne.' ') then
      write(iout,'(''name '',a75)') names(nc)(1:75)
    endif
!
!  Comment of surface energy
!
    if (nc.eq.ncf.and.ndimen(ncf).eq.2) then
      if (abs(esurface).gt.1.0d-6) then
        write(iout,'(''# Surface energy = '',f12.6,'' J/m2'')') esurface
      endif
    endif
!
!  Initialise necessary local variables to avoid
!  writing to global equivalents
!
    nlasym = nascfg(nc)
    nlsft = 0
    do i = 1,nc-1
      nlsft = nlsft + nascfg(i)
    enddo
    llsym = lsymset(nc)
    nlvar = nvarcfg(nc)
    mlvar = 3*nlasym + nstrains
    nlfst = n1var(nc) - 1
    if (ndimen(nc).eq.3) then
!
!  Transform primitive cell back to original cell
!  
      if (nc.eq.ncf) then
        do i = 1,3
          rvp(1,i) = rv(1,i)
          rvp(2,i) = rv(2,i)
          rvp(3,i) = rv(3,i)
        enddo
      else
        do i = 1,3
          rvp(1,i) = rvcfg(1,i,nc)
          rvp(2,i) = rvcfg(2,i,nc)
          rvp(3,i) = rvcfg(3,i,nc)
        enddo
      endif
      if (nspcg(nc).gt.1) then
        if (hmssg(1,nc).ne.'P'.and.(ifhr(nc).eq.0.or.lhex)) then
!
!  Need to temporarily reset ncbl for current structure for uncentre
!
          ncblold = ncbl
          do i = 1,7
            if (hmssg(1,nc).eq.hbr(i)) ncbl = i
          enddo
          call uncentre(rvp)
          ncbl = ncblold
        endif
      endif
      call uncell3D(rvp,acl,bcl,ccl,alpcl,betcl,gamcl)
!
!  Crystal structure info first
!
      lrhombo = (ifhr(nc).eq.1.and.(.not.lhex))
    elseif (ndimen(nc).eq.2) then
      if (nc.eq.ncf) then
        do i = 1,2
          rvp(1,i) = rv(1,i)
          rvp(2,i) = rv(2,i)
        enddo
      else
        do i = 1,2
          rvp(1,i) = rvcfg(1,i,nc)
          rvp(2,i) =  rvcfg(2,i,nc)
        enddo
      endif
      call uncell2D(rvp,acl,bcl,alpcl)
    elseif (ndimen(nc).eq.1) then
      if (nc.eq.ncf) then
        rvp(1,1) = rv(1,1)
      else
        rvp(1,1) = rvcfg(1,1,nc)
      endif
      call uncell1D(rvp,acl)
    endif
    if (lflags) then
      allocate(itmp(mlvar),stat=status)
      if (status/=0) call outofmemory('dumpdur','itmp')
      do i = 1,mlvar
        itmp(i) = 0
      enddo
      do i = 1,nlvar
        ii = ioptcfg(i+nlfst)
        if (ii.le.mlvar) itmp(ii) = 1
      enddo
      if (lmd.and.nc.eq.ncf.and.timesofar.gt.1.0d-6) then
!
!  As this is an MD run use Cartesian coordinates to save time of conversion
!  and use cell vectors to preserve orientation relative to coordinates.
!
        if (ndimen(nc).eq.3) then
          write(iout,'(''vectors '')')
          write(iout,'(3f12.6)') (rv(j,1),j=1,3)
          write(iout,'(3f12.6)') (rv(j,2),j=1,3)
          write(iout,'(3f12.6)') (rv(j,3),j=1,3)
          write(iout,'(6i2)') (itmp(j),j=1,6)
        elseif (ndimen(nc).eq.2) then
          write(iout,'(''svectors '')')
          write(iout,'(2f12.6)') (rv(j,1),j=1,2)
          write(iout,'(2f12.6)') (rv(j,2),j=1,2)
          write(iout,'(3i2)') (itmp(j),j=1,3)
        elseif (ndimen(nc).eq.1) then
          write(iout,'(''pvectors '')')
          write(iout,'(f12.6)') rv(1,1)
          write(iout,'(1i2)') itmp(1)
        endif
        do nr = 1,nregions(nc)
          fixstring = ' '
          qmmmstring = ' '
          if (nregiontype(nr,nc).eq.1) then
            qmmmstring = 'qm'
          elseif (nregiontype(nr,nc).eq.2) then
            qmmmstring = 'mm'
          endif
          if (lregionrigid(nr,nc)) then
            if (lopfreg(3*(nr-1)+1,nc)) then
              fixstring(1:1) = 'x'
            endif
            if (lopfreg(3*(nr-1)+2,nc)) then
              fixstring(2:2) = 'y'
            endif     
            if (lopfreg(3*(nr-1)+3,nc)) then
              fixstring(3:3) = 'z'    
            endif
            write(iout,'(''cartesian region '',i2,1x,''rigid'',1x,a3,1x,a2)') nr,fixstring,qmmmstring
          else
            if (nregions(nc).gt.1) then
              write(iout,'(''cartesian region '',i2,1x,a2)') nr,qmmmstring
            else
              write(iout,'(''cartesian '')') 
            endif
          endif
          do i = 1,nlasym
            if (nregionno(i+nlsft).eq.nr) then
              inat = natcfg(i+nlsft)
              itype = ntypcfg(i+nlsft)
              nri = nrel2(i)
              call label(inat,itype,lab1)
              if (lqmatom(i+nlsft)) then
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'qbsh'
                  else
                    lab2 = 'qbco'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'qshe'
                  else
                    lab2 = 'qcor'
                  endif
                endif
              else
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'bshe'
                  else
                    lab2 = 'bcor'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'shel'
                  else
                    lab2 = 'core'
                  endif
                endif
              endif
              if (ltranat(i+nlsft)) then
                tchar = 'T'
              else
                tchar = ' '
              endif
              if (inat.le.maxele.or..not.lhideshells) then
                ifx = itmp(nstrains+3*(i-1)+1)
                ify = itmp(nstrains+3*(i-1)+2)
                ifz = itmp(nstrains+3*(i-1)+3)
                call wcoord(iout,lab1,lab2,xclat(i),yclat(i),zclat(i),qlcfg(i+nlsft),occucfg(i+nlsft), &
                  radcfg(i+nlsft),lsliceatom(i+nlsft),ifx,ify,ifz,tchar,.true.,rvcfg(1,1,nc),ndimen(nc))
              endif
            endif
          enddo
        enddo
      else
!
!  Normal structure output
!
        if (ndimen(nc).eq.3) then
          write(iout,'(''cell '')')
          write(iout,'(6f11.6,6i2)') acl,bcl,ccl,alpcl,betcl,gamcl,(itmp(j),j=1,6)
!              write(iout,'(''cell '',6f11.6,6i2)')acl,bcl,ccl,alpcl,betcl,gamcl,(itmp(j),j=1,6)
        elseif (ndimen(nc).eq.2) then
          write(iout,'(''scell '')')
          write(iout,'(3f11.6,3i2)') acl,bcl,alpcl,(itmp(j),j=1,3)
!              write(iout,'(''scell '',3f11.6,3i2)')acl,bcl,alpcl,(itmp(j),j=1,3)
        elseif (ndimen(nc).eq.1) then
          write(iout,'(''pcell '')')
          write(iout,'(f11.6,i2)') acl,itmp(1)
!              write(iout,'(''pcell '',f11.6,i2)') acl,itmp(1)
        endif
!
!  Loop over regions
!
!  For region 1 of surface, output growth slice with suboptions first
!
        do nr = 1,nregions(nc)
          fixstring = ' '
          qmmmstring = ' '
          if (nregiontype(nr,nc).eq.1) then
            qmmmstring = 'qm'
          elseif (nregiontype(nr,nc).eq.2) then
            qmmmstring = 'mm'
          endif
          if (lregionrigid(nr,nc)) then
            if (lopfreg(3*(nr-1)+1,nc)) then
              fixstring(1:1) = 'x'
            endif
            if (lopfreg(3*(nr-1)+2,nc)) then
              fixstring(2:2) = 'y'
            endif
            if (lopfreg(3*(nr-1)+3,nc)) then
              fixstring(3:3) = 'z'
            endif       
            if (ndimen(nc).eq.3.and..not.ldumpcart) then
              write(iout,'(''fractional rigid'')')
            elseif (ndimen(nc).eq.2.and..not.ldumpcart) then
              write(iout,'(''sfractional region '',i2,1x,''rigid'',1x,a3,1x,a2)') nr,fixstring,qmmmstring
            elseif (ndimen(nc).eq.1.and..not.ldumpcart) then
              write(iout,'(''pfractional region '',i2,1x,''rigid'',1x,a3,1x,a2)') nr,fixstring,qmmmstring
            else
              write(iout,'(''cartesian rigid '')')
            endif
          else
            if (ndimen(nc).eq.3.and..not.ldumpcart) then
              write(iout,'(''fractional '')')
            elseif (ndimen(nc).eq.2.and..not.ldumpcart) then
              write(iout,'(''sfractional region '',i2,1x,a2)') nr,qmmmstring
            elseif (ndimen(nc).eq.1.and..not.ldumpcart) then
              write(iout,'(''pfractional region '',i2,1x,a2)') nr,qmmmstring
            else
              write(iout,'(''cartesian '')')
            endif
          endif
          do i = 1,nlasym
            if (nregionno(i+nlsft).eq.nr) then
              inat = natcfg(i+nlsft)
              itype = ntypcfg(i+nlsft)
              nri = nrel2(i)
              call label(inat,itype,lab1)
              if (lqmatom(i+nlsft)) then
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'qbsh'
                  else
                    lab2 = 'qbco'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'qshe'
                  else
                    lab2 = 'qcor'
                  endif
                endif
              else
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'bshe'
                  else
                    lab2 = 'bcor'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'shel'
                  else
                    lab2 = 'core'
                  endif
                endif
              endif
              if (ltranat(i+nlsft)) then
                tchar = 'T'
              else
                tchar = ' '
              endif
              if (inat.le.maxele.or..not.lhideshells) then
                ifx = itmp(nstrains+3*(i-1)+1)
                ify = itmp(nstrains+3*(i-1)+2)
                ifz = itmp(nstrains+3*(i-1)+3)
                if (ndimen(nc).eq.3) then
                  if (nc.eq.ncf.and.nreg1.eq.0) then
                    if (.not.lsymopt) then
                      call wcoord(iout,lab1,lab2,xfrac(i),yfrac(i),zfrac(i),qlcfg(i+nlsft),occucfg(i+nlsft), &
                        rada(i),lsliceatom(i+nlsft),ifx,ify,ifz,tchar,.true.,rvcfg(1,1,nc),ndimen(nc))
                    elseif (lrhombo) then
                      call wcoord(iout,lab1,lab2,xfrac(nri),yfrac(nri),zfrac(nri),qlcfg(i+nlsft), &
                        occucfg(i+nlsft),rada(i),lsliceatom(i+nlsft),ifx,ify,ifz,tchar,.true., &
                        rvcfg(1,1,nc),ndimen(nc))
                    else
                      call wcoord(iout,lab1,lab2,xcfg(i+nlsft),ycfg(i+nlsft),zcfg(i+nlsft),qlcfg(i+nlsft), &
                        occucfg(i+nlsft),radcfg(i+nlsft),lsliceatom(i+nlsft),ifx,ify,ifz,tchar, &
                        .true.,rvcfg(1,1,nc),ndimen(nc))
                    endif
                  else
                    call wcoord(iout,lab1,lab2,xcfg(i+nlsft),ycfg(i+nlsft),zcfg(i+nlsft),qlcfg(i+nlsft), &
                      occucfg(i+nlsft),radcfg(i+nlsft),lsliceatom(i+nlsft),ifx,ify,ifz,tchar,.true., &
                      rvcfg(1,1,nc),ndimen(nc))
                  endif
                else
                  if (nc.eq.ncf.and.nreg1.eq.0) then
                    call wcoord(iout,lab1,lab2,xfrac(i),yfrac(i),zfrac(i),qlcfg(i+nlsft),occucfg(i+nlsft), &
                      rada(i),lsliceatom(i+nlsft),ifx,ify,ifz,tchar,.true.,rvcfg(1,1,nc),ndimen(nc))
                  else
                    call wcoord(iout,lab1,lab2,xcfg(i+nlsft),ycfg(i+nlsft),zcfg(i+nlsft),qlcfg(i+nlsft), &
                      occucfg(i+nlsft),radcfg(i+nlsft),lsliceatom(i+nlsft),ifx,ify,ifz,tchar,.true., &
                      rvcfg(1,1,nc),ndimen(nc))
                  endif
                endif
              endif
            endif
          enddo
        enddo
        if (llsym) then
          if (ngocfg(nc).gt.1) then
            if (nccscfg(nc).eq.1) then
              write(iout,'(''symmetry_cell triclinic'')')
            elseif (nccscfg(nc).eq.2) then
              write(iout,'(''symmetry_cell monoclinic'')')
            elseif (nccscfg(nc).eq.3) then
              write(iout,'(''symmetry_cell orthorhombic'')')
            elseif (nccscfg(nc).eq.4) then
              write(iout,'(''symmetry_cell tetragonal'')')
            elseif (nccscfg(nc).eq.5.and.ifhr(nc).eq.0) then
              write(iout,'(''symmetry_cell hexagonal'')')
            elseif (nccscfg(nc).eq.5.and.ifhr(nc).eq.1) then
              write(iout,'(''symmetry_cell rhombohedral'')')
            elseif (nccscfg(nc).eq.6) then
              write(iout,'(''symmetry_cell cubic'')')
            endif
            do i = 2,ngocfg(nc)
              write(iout,'(''symmetry_operator'')')
              write(iout,'(3f12.8,2x,f12.8)') (ropcfg(j,1,i,nc),j=1,3),vitcfg(1,i,nc)
              write(iout,'(3f12.8,2x,f12.8)') (ropcfg(j,2,i,nc),j=1,3),vitcfg(2,i,nc)
              write(iout,'(3f12.8,2x,f12.8)') (ropcfg(j,3,i,nc),j=1,3),vitcfg(3,i,nc)
            enddo
          else
            write(iout,'(''space'')')
            if (iflags(nc).eq.0) then
              write(iout,'(i3)') nspcg(nc)
            else
              write(iout,'(16a1)') (hmssg(i,nc),i=1,16)
            endif
            if (ifso(nc).eq.1) then
              write(iout,'(''origin 2'')')
            elseif (ifso(nc).gt.1) then
              write(iout,'(''origin '',3i3)') (ivso(j,nc),j=1,3)
            endif
          endif
        endif
      endif
      deallocate(itmp,stat=status)
      if (status/=0) call deallocate_error('dumpdur','itmp')
    else
      if (lmd.and.nc.eq.ncf.and.timesofar.gt.1.0d-6) then
!
!  As this is an MD run use cartesian coords to save time
!
        if (ndimen(nc).eq.3) then
          write(iout,'(''vectors '')')
          write(iout,'(3f12.6)') (rv(j,1),j=1,3)
          write(iout,'(3f12.6)') (rv(j,2),j=1,3)
          write(iout,'(3f12.6)') (rv(j,3),j=1,3)
        elseif (ndimen(nc).eq.2) then
          write(iout,'(''svectors '')')
          write(iout,'(2f12.6)') (rv(j,1),j=1,2)
          write(iout,'(2f12.6)') (rv(j,2),j=1,2)
        elseif (ndimen(nc).eq.1) then
          write(iout,'(''pvectors '')')
          write(iout,'(f12.6)') rv(1,1)
        endif
        do nr = 1,nregions(nc)
          fixstring = ' '
          qmmmstring = ' '
          if (nregiontype(nr,nc).eq.1) then
            qmmmstring = 'qm'
          elseif (nregiontype(nr,nc).eq.2) then
            qmmmstring = 'mm'
          endif
          if (lregionrigid(nr,nc)) then
            if (lopfreg(3*(nr-1)+1,nc)) then
              fixstring(1:1) = 'x'
            endif
            if (lopfreg(3*(nr-1)+2,nc)) then
              fixstring(2:2) = 'y'
            endif      
            if (lopfreg(3*(nr-1)+3,nc)) then
              fixstring(3:3) = 'z'
            endif
            if (nregions(nc).gt.1) then
              write(iout,'(''cartesian region '',i2,1x,''rigid'',1x,a3)') nr,fixstring,qmmmstring
            else
              write(iout,'(''cartesian rigid'')')
            endif
          else
            if (nregions(nc).gt.1) then
              write(iout,'(''cartesian region '',i2,1x,a2)') nr,qmmmstring
            else
              write(iout,'(''cartesian '')')
            endif
          endif
          do i = 1,nlasym
            if (nregionno(i+nlsft).eq.nr) then
              inat = natcfg(i+nlsft)
              itype = ntypcfg(i+nlsft)
              nri = nrel2(i)
              call label(inat,itype,lab1)
              if (lqmatom(i+nlsft)) then
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'qbsh'
                  else
                    lab2 = 'qbco'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'qshe'
                  else
                    lab2 = 'qcor'
                  endif
                endif
              else
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'bshe'
                  else
                    lab2 = 'bcor'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'shel'
                  else
                    lab2 = 'core'
                  endif
                endif
              endif
              if (inat.le.maxele.or..not.lhideshells) then
                tchar = ' '
                call wcoord(iout,lab1,lab2,xclat(i),yclat(i),zclat(i),qlcfg(i+nlsft),occucfg(i+nlsft), &
                  radcfg(i+nlsft),lsliceatom(i+nlsft),0_i4,0_i4,0_i4,tchar,.false.,rvcfg(1,1,nc),ndimen(nc))
              endif
            endif
          enddo
        enddo
      else
!
!  Normal structure output
!
        if (ndimen(nc).eq.3) then
          write(iout,'(''cell'')')
          write(iout,'(6f11.6)')acl,bcl,ccl,alpcl,betcl,gamcl
!              write(iout,'(''cell '',6f11.6)')acl,bcl,ccl,alpcl,betcl,gamcl
        elseif (ndimen(nc).eq.2) then
          write(iout,'(''scell'')')
          write(iout,'(3f11.6)')acl,bcl,alpcl
!              write(iout,'(''scell '',3f11.6)')acl,bcl,alpcl
        elseif (ndimen(nc).eq.1) then
          write(iout,'(''pcell'')')
          write(iout,'(f11.6)')acl
!              write(iout,'(''pcell '',f11.6)')acl
        endif
!
!  Loop over regions
!
        do nr = 1,nregions(nc)
          fixstring = ' '  
          qmmmstring = ' '
          if (nregiontype(nr,nc).eq.1) then
            qmmmstring = 'qm'
          elseif (nregiontype(nr,nc).eq.2) then
            qmmmstring = 'mm'
          endif
          if (lregionrigid(nr,nc)) then  
            if (lopfreg(3*(nr-1)+1,nc)) then  
              fixstring(1:1) = 'x'  
            endif  
            if (lopfreg(3*(nr-1)+2,nc)) then  
              fixstring(2:2) = 'y'  
            endif  
            if (lopfreg(3*(nr-1)+3,nc)) then  
              fixstring(3:3) = 'z'  
            endif  
            if (ndimen(nc).eq.3.and..not.ldumpcart) then
              write(iout,'(''fractional rigid'',i2,1x,a2)') nr,qmmmstring
            elseif (ndimen(nc).eq.2.and..not.ldumpcart) then
              write(iout,'(''sfractional region '',i2,1x,''rigid'',1x,a3,1x,a2)') nr,fixstring,qmmmstring
            elseif (ndimen(nc).eq.1.and..not.ldumpcart) then
              write(iout,'(''pfractional region '',i2,1x,''rigid'',1x,a3,1x,a2)') nr,fixstring,qmmmstring
            else
              write(iout,'(''cartesian region rigid '',i2,1x,a2)') nr,qmmmstring
            endif
          else
            if (nr.eq.2) then
!
!  For region 2 the default is to be rigid and so were need to indicate if this is not the case
!
              if (ndimen(nc).eq.3.and..not.ldumpcart) then
                write(iout,'(''fractional '',i2,'' nonrigid'',1x,a2)') nr,qmmmstring
              elseif (ndimen(nc).eq.2.and..not.ldumpcart) then
                write(iout,'(''sfractional region '',i2,'' nonrigid'',1x,a2)') nr,qmmmstring
              elseif (ndimen(nc).eq.1.and..not.ldumpcart) then
                write(iout,'(''pfractional region '',i2,'' nonrigid'',1x,a2)') nr,qmmmstring
              else
                write(iout,'(''cartesian region '',i2,'' nonrigid'',1x,a2)') nr,qmmmstring
              endif
            else
              if (ndimen(nc).eq.3.and..not.ldumpcart) then
                write(iout,'(''fractional '',i2,1x,a2)') nr,qmmmstring
              elseif (ndimen(nc).eq.2.and..not.ldumpcart) then
                write(iout,'(''sfractional region '',i2,1x,a2)') nr,qmmmstring
              elseif (ndimen(nc).eq.1.and..not.ldumpcart) then
                write(iout,'(''pfractional region '',i2,1x,a2)') nr,qmmmstring
              else
                write(iout,'(''cartesian region '',i2,1x,a2)') nr,qmmmstring
              endif
            endif
          endif
          do i = 1,nlasym
            if (nregionno(i+nlsft).eq.nr) then
              inat = natcfg(i+nlsft)
              itype = ntypcfg(i+nlsft)
              nri = nrel2(i)
              call label(inat,itype,lab1)
              if (lqmatom(i+nlsft)) then
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'qbsh'
                  else
                    lab2 = 'qbco'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'qshe'
                  else
                    lab2 = 'qcor'
                  endif
                endif
              else
                if (lbsmat(i+nlsft)) then
                  if (inat.gt.maxele) then
                    lab2 = 'bshe'
                  else
                    lab2 = 'bcor'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'shel'
                  else
                    lab2 = 'core'
                  endif
                endif
              endif
              if (ltranat(i+nlsft)) then
                tchar = 'T'
              else
                tchar = ' '
              endif
              if (inat.le.maxele.or..not.lhideshells) then
                if (ndimen(nc).eq.3) then
                  if (nc.eq.ncf.and.nreg1.eq.0) then
                    if (.not.lsymopt) then
                      call wcoord(iout,lab1,lab2,xfrac(i),yfrac(i),zfrac(i),qlcfg(i+nlsft), &
                        occucfg(i+nlsft),rada(i),lsliceatom(i+nlsft), &
                        0_i4,0_i4,0_i4,tchar,.false.,rvcfg(1,1,nc),ndimen(nc))
                    elseif (lrhombo) then
                      call wcoord(iout,lab1,lab2,xfrac(nri),yfrac(nri),zfrac(nri),qlcfg(i+nlsft), &
                        occucfg(i+nlsft),rada(i),lsliceatom(i+nlsft), &
                        0_i4,0_i4,0_i4,tchar,.false.,rvcfg(1,1,nc),ndimen(nc))
                    else
                      call wcoord(iout,lab1,lab2,xcfg(i+nlsft),ycfg(i+nlsft),zcfg(i+nlsft),qlcfg(i+nlsft), &
                        occucfg(i+nlsft),radcfg(i+nlsft),lsliceatom(i+nlsft),0_i4,0_i4,0_i4,tchar, &
                        .false.,rvcfg(1,1,nc),ndimen(nc))
                    endif
                  else
                    call wcoord(iout,lab1,lab2,xcfg(i+nlsft),ycfg(i+nlsft),zcfg(i+nlsft),qlcfg(i+nlsft), &
                      occucfg(i+nlsft),radcfg(i+nlsft),lsliceatom(i+nlsft),0_i4,0_i4,0_i4,tchar, &
                      .false.,rvcfg(1,1,nc),ndimen(nc))
                  endif
                else
                  if (nc.eq.ncf.and.nreg1.eq.0) then
                    call wcoord(iout,lab1,lab2,xfrac(i),yfrac(i),zfrac(i),qlcfg(i+nlsft),occucfg(i+nlsft), &
                      rada(i),lsliceatom(i+nlsft),0_i4,0_i4,0_i4,tchar,.false.,rvcfg(1,1,nc),ndimen(nc))
                  else
                    call wcoord(iout,lab1,lab2,xcfg(i+nlsft),ycfg(i+nlsft),zcfg(i+nlsft),qlcfg(i+nlsft), &
                      occucfg(i+nlsft),radcfg(i+nlsft),lsliceatom(i+nlsft),0_i4,0_i4,0_i4,tchar, &
                      .false.,rvcfg(1,1,nc),ndimen(nc))
                  endif
                endif
              endif
            endif
          enddo
        enddo
        if (llsym) then
          if (ngocfg(nc).gt.1) then
            do i = 2,ngocfg(nc)
              write(iout,'(''symmetry_operator'')')
              write(iout,'(3f12.8,2x,f12.8)') (ropcfg(j,1,i,nc),j=1,3),vitcfg(1,i,nc)
              write(iout,'(3f12.8,2x,f12.8)') (ropcfg(j,2,i,nc),j=1,3),vitcfg(2,i,nc)
              write(iout,'(3f12.8,2x,f12.8)') (ropcfg(j,3,i,nc),j=1,3),vitcfg(3,i,nc)
            enddo
          else
            write(iout,'(''space'')')
            if (iflags(nc).eq.0) then
              write(iout,'(i3)') nspcg(nc)
            else
              write(iout,'(16a1)') (hmssg(i,nc),i=1,16)
            endif
            if (ifso(nc).eq.1) then
              write(iout,'(''origin 2'')')
            elseif (ifso(nc).gt.1) then
              write(iout,'(''origin '',3i3)') (ivso(j,nc),j=1,3)
            endif
          endif
        endif
      endif
    endif
!
!  QM/MM mode
!
    if (QMMMmode(nc).eq.1) then
      write(iout,'(''qmmm mechanical'')')
    elseif (QMMMmode(nc).eq.2) then
      write(iout,'(''qmmm electronic'')')
    endif
!
!  Surface region 1 bulk energy
!
    if (sbulkecfg(nc).ne.0.0_dp) then
      write(iout,'(''sbulkenergy '',f18.8)') sbulkecfg(nc)
    endif
!
!  Total energy for bulk
!
    if (ndimen(nc).eq.3.and.abs(energycfg(nc)).gt.1.0d-8) then
      if (lrhombo) then
        write(iout,'(''totalenergy '',f24.10,'' eV'')') energycfg(nc)/3.0_dp
      else
        write(iout,'(''totalenergy '',f24.10,'' eV'')') energycfg(nc)
      endif
    endif
!
!  Electric field data
!
    if (lfieldcfg(nc)) then
      if (ndimen(nc).eq.2) then
        write(iout,'(''field '',f24.10,'' z'')') fieldcfg(nc)
      elseif (ndimen(nc).eq.1) then
        if (abs(fielddirectioncfg(2,nc)).gt.1.0d-2) then
          write(iout,'(''field '',f24.10,'' y'')') fieldcfg(nc)
        else
          write(iout,'(''field '',f24.10,'' z'')') fieldcfg(nc)
        endif
      elseif (ndimen(nc).eq.0) then
        norm = fielddirectioncfg(1,nc)**2 + fielddirectioncfg(2,nc)**2 + fielddirectioncfg(3,nc)**2
        norm = 1.0_dp/sqrt(norm)
        if (norm*fielddirectioncfg(3,nc).gt.0.99999999_dp) then
          write(iout,'(''field '',f24.10,'' z'')') fieldcfg(nc)
        elseif (norm*fielddirectioncfg(2,nc).gt.0.99999999_dp) then
          write(iout,'(''field '',f24.10,'' y'')') fieldcfg(nc)
        elseif (norm*fielddirectioncfg(1,nc).gt.0.99999999_dp) then
          write(iout,'(''field '',f24.10,'' x'')') fieldcfg(nc)
        else
          write(iout,'(''field '',f24.10,1x,3f8.5)') fieldcfg(nc),(fielddirectioncfg(j,nc),j=1,3)
        endif
      elseif (ndimen(nc).eq.3) then
        if (abs(fielddirectioncfg(2,nc)).gt.1.0d-2) then
          write(iout,'(''field '',f24.10,'' b'')') fieldcfg(nc)
        elseif (abs(fielddirectioncfg(3,nc)).gt.1.0d-2) then
          write(iout,'(''field '',f24.10,'' c'')') fieldcfg(nc)
        else
          write(iout,'(''field '',f24.10,'' a'')') fieldcfg(nc)
        endif
      endif
    endif
!
!  Time-dependent electric field
!
    if (ntdfieldcfg(nc).gt.0) then
      do k = 1,ntdfieldcfg(nc)
        if (ndimen(nc).eq.2) then
          write(iout,'(''td_field '',f24.10,'' z '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
        elseif (ndimen(nc).eq.1) then
          if (abs(td_fielddirectioncfg(2,k,nc)).gt.1.0d-2) then
            write(iout,'(''td_field '',f24.10,'' y '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          else
            write(iout,'(''td_field '',f24.10,'' z '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          endif
        elseif (ndimen(nc).eq.0) then
          norm = td_fielddirectioncfg(1,k,nc)**2 + td_fielddirectioncfg(2,k,nc)**2 + td_fielddirectioncfg(3,k,nc)**2
          norm = 1.0_dp/sqrt(norm)
          if (norm*td_fielddirectioncfg(3,k,nc).gt.0.99999999_dp) then
            write(iout,'(''td_field '',f24.10,'' z '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          elseif (norm*td_fielddirectioncfg(2,k,nc).gt.0.99999999_dp) then
            write(iout,'(''td_field '',f24.10,'' y '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          elseif (norm*td_fielddirectioncfg(1,k,nc).gt.0.99999999_dp) then
            write(iout,'(''td_field '',f24.10,'' x '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          else
            write(iout,'(''td_field '',f24.10,1x,3f8.5,f12.6,1x,f8.5)') td_fieldcfg(1,k,nc), &
                  (td_fielddirectioncfg(j,k,nc),j=1,3),td_fieldcfg(2,k,nc),td_fieldcfg(3,k,nc)
          endif
        elseif (ndimen(nc).eq.3) then
          if (abs(td_fielddirectioncfg(2,k,nc)).gt.1.0d-2) then
            write(iout,'(''td_field '',f24.10,'' b '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          elseif (abs(td_fielddirectioncfg(3,k,nc)).gt.1.0d-2) then
            write(iout,'(''td_field '',f24.10,'' c '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          else
            write(iout,'(''td_field '',f24.10,'' a '',f12.6,1x,f8.5)') (td_fieldcfg(j,k,nc),j=1,3)
          endif
        endif
      enddo
    endif
!
!  Radial force
!
    if (lradialcfg(nc)) then
      write(iout,'(''radial_force '',f20.8,3(1x,f12.6))') radialKcfg(nc),(radialXYZcfg(j,nc),j=1,3)
    endif
!
!  Nudged elastic band data
!
    if (nnebreplica(nc).gt.0) then
      write(iout,'(''#'')')
      write(iout,'(''#  Nudged elastic band data'')')
      write(iout,'(''#'')')
      if (lnebvaryspring(nc)) then
        write(iout,'(''nebspring vary '',f12.6,1x,f12.6)') nebspring(nc),nebspringmin(nc)
      else
        write(iout,'(''nebspring  '',f12.6)') nebspring(nc)
      endif
      do ii = 1,nnebreplicatot
        if (nebreplicacfgptr(ii).eq.nc) then
          if (ndimen(nc).eq.3) then
            write(iout,'(''rcell'',1x,i6)') nnebreplicano(ii) 
            write(iout,'(3(f10.5,1x),3(f8.4,1x))') (nebreplicacell(j,ii),j=1,6)
          elseif (ndimen(nc).eq.2) then
            write(iout,'(''rcell'',1x,i6)') nnebreplicano(ii) 
            write(iout,'(2(f10.5,1x),f8.4)') (nebreplicacell(j,ii),j=1,3)
          elseif (ndimen(nc).eq.1) then
            write(iout,'(''rcell'',1x,i6)') nnebreplicano(ii) 
            write(iout,'(f10.5)') nebreplicacell(1,ii)
          endif
          if (ndimen(nc).gt.0) then
            write(iout,'(''rfractional'',1x,i6)') nnebreplicano(ii) 
          else
            write(iout,'(''rcartesian'',1x,i6)') nnebreplicano(ii) 
          endif
          do i = 1,nascfg(nc)
            write(iout,'(3(f12.6,1x,f10.6))') (nebreplicaxyz(j,i,ii),j=1,3),nebreplicaradius(i,ii)
          enddo
        endif
      enddo
      if (ndimen(nc).eq.3) then
        write(iout,'(''fcell'')')
        write(iout,'(3(f10.5,1x),3(f8.4,1x))') (nebfinalcell(j,nc),j=1,6)
      elseif (ndimen(nc).eq.2) then
        write(iout,'(''fcell'')')
        write(iout,'(2(f10.5,1x),f8.4)') (nebfinalcell(j,nc),j=1,3)
      elseif (ndimen(nc).eq.1) then
        write(iout,'(''fcell'')')
        write(iout,'(f10.5)') nebfinalcell(1,nc)
      endif
      if (ndimen(nc).gt.0) then
        write(iout,'(''ffractional'')')
      else
        write(iout,'(''fcartesian'')')
      endif
      do i = 1,nascfg(nc)
        write(iout,'(3(f12.6,1x),f10.5)') (nebfinalxyz(j,i,nc),j=1,3),nebfinalradius(i,nc)
      enddo
      write(iout,'(''nebreplica '',i6)') nnebreplica(nc)
      write(iout,'(''#'')')
    endif
!
!  Configuration related potentials and properties
!
    nsc = nshcfg(nc)
    shc = shift(nsc)
    if (nsc.ne.nlshift.and.shc.ne.0.0_dp) then
      write(iout,'(''shift'',1x,f18.8)') shc
      nlshift = nsc
    endif
    if (shscalecfg(nc).ne.1.0_dp) then
      write(iout,'(''sshift'',1x,f18.8)') shscalecfg(nc)
    endif
    lfirstout = .true.
    do i = 1,nlasym
      if (leinsteinat(nlsft+i)) then
        if (lfirstout) then
          lfirstout = .false.
          write(iout,'(''einstein'')')
        endif
        write(iout,'(i5,3(1x,f10.6),1x,f12.6)') i,xeinsteinat(nlsft+i), &
          yeinsteinat(nlsft+i),zeinsteinat(nlsft+i),keinsteinat(nlsft+i)
      endif
    enddo
!
!  Dump of full connectivity list for current structure only
!
    if (ldumpconnectivity.and.nc.eq.ncf) then
      do i = 1,numat
        icm = 1
        imm = 1
        do while (imm.gt.0.and.icm.le.nbonds(i))
          imm = nbonded(icm,i)
          if (imm.gt.0.and.imm.le.i) then
!
!  Set any bonding types
!
            nconword = 0
            if (nbondedtype(1,icm,i).gt.1) then
              nconword = nconword + 1
              if (nbondedtype(1,icm,i).eq.2) then
                conword(nconword) = 'double'
              elseif (nbondedtype(1,icm,i).eq.3) then
                conword(nconword) = 'triple'
              elseif (nbondedtype(1,icm,i).eq.4) then
                conword(nconword) = 'quadruple'
              elseif (nbondedtype(1,icm,i).eq.5) then
                conword(nconword) = 'resonant'
              elseif (nbondedtype(1,icm,i).eq.6) then
                conword(nconword) = 'amide'
              endif
            endif
            if (nbondedtype(2,icm,i).gt.1) then
              nconword = nconword + 1
              if (nbondedtype(2,icm,i).eq.2) then
                conword(nconword) = 'cyclic'
              elseif (nbondedtype(2,icm,i).eq.3) then
                conword(nconword) = 'exocyclic'
              endif
            endif
!
!  Valid bond - output details
!
            call mindtoijk(nbondind(icm,i),imagex,imagey,imagez)
            if (ndimen(nc).eq.3) then
              write(iout,'(''connect '',2i6,3(1x,i2),2(1x,a9))')  &
                i,imm,imagex,imagey,imagez,(conword(j),j=1,nconword)
            elseif (ndimen(nc).eq.2) then
              write(iout,'(''connect '',2i6,2(1x,i2),2(1x,a9))')  &
                i,imm,imagex,imagey,(conword(j),j=1,nconword)
            elseif (ndimen(nc).eq.1) then
              write(iout,'(''connect '',2i6,1x,i2,2(1x,a9))')  &
                i,imm,imagex,(conword(j),j=1,nconword)
            else
              write(iout,'(''connect '',2i6,2(1x,a9))')  &
                i,imm,(conword(j),j=1,nconword)
            endif
          endif
          icm = icm + 1
        enddo
      enddo
    else
!
!  User input connectivity lists
!
      do ic = 1,nconnect
        if (nconnectcfg(ic) .eq. nc) then
          nconword = 0
          if (nconnecttype(1,ic).gt.1) then
            nconword = nconword + 1
            if (nconnecttype(1,ic).eq.2) then
              conword(nconword) = 'double'
            elseif (nconnecttype(1,ic).eq.3) then
              conword(nconword) = 'triple'
            elseif (nconnecttype(1,ic).eq.4) then
              conword(nconword) = 'quadruple'
            elseif (nconnecttype(1,ic).eq.5) then
              conword(nconword) = 'resonant'
            elseif (nconnecttype(1,ic).eq.6) then
              conword(nconword) = 'amide'
            endif
          endif
          if (nconnecttype(2,ic).gt.1) then
            nconword = nconword + 1
            if (nconnecttype(2,ic).eq.2) then
              conword(nconword) = 'cyclic'
            elseif (nconnecttype(2,ic).eq.3) then
              conword(nconword) = 'exocyclic'
            endif
          endif
          if (nconnectind(ic).gt.0) then
            call mindtoijk(nconnectind(ic),imagex,imagey,imagez)
            if (ndimen(nc).eq.3) then
              write(iout,'(''connect '',i6,1x,i6,3(1x,i2),2(1x,a9))')  &
                n1connect(ic),n2connect(ic),imagex,imagey,imagez,(conword(i),i=1,nconword)
            elseif (ndimen(nc).eq.2) then
              write(iout,'(''connect '',i6,1x,i6,2(1x,i2),2(1x,a9))')  &
                n1connect(ic),n2connect(ic),imagex,imagey,(conword(i),i=1,nconword)
            elseif (ndimen(nc).eq.1) then
              write(iout,'(''connect '',i6,1x,i6,1x,i2,2(1x,a9))')  &
                n1connect(ic),n2connect(ic),imagex,(conword(i),i=1,nconword)
            else
              write(iout,'(''connect '',i6,1x,i6,2(1x,a9))')  &
                n1connect(ic),n2connect(ic),(conword(i),i=1,nconword)
            endif
          else
            write(iout,'(''connect '',i6,1x,i6,2(1x,a9))')  &
              n1connect(ic),n2connect(ic),(conword(i),i=1,nconword)
          endif
        endif
      enddo
    endif
!
!  Observables for fitting that relate to configuration
!
    if (nobs.gt.0) then
      ldiff = .true.
      do i = 1,nobs
        nt = nobtyp(i)
        if (nc.eq.nobcfg(i).and.(nt.ne.2.and.nt.ne.6)) then
          if (ldiff) write(iout,'(''observables'')')
          ldiff = .false.
          if (nt.eq.1) then
            write(iout,'(''energy'')')
            write(iout,'(f16.8,1x,f16.6)') fobs(i),weight(i)
          elseif (nt.eq.3) then
            write(iout,'(''elastic'')')
            ni = nobptr(i)/7
            nj = nobptr(i) - 7*ni
            write(iout,'(2i2,f12.5,1x,f16.6)') ni,nj,fobs(i),weight(i)
          elseif (nt.eq.4) then
            write(iout,'(''hfdlc'')')
            ni = nobptr(i)/4
            nj = nobptr(i) - 4*ni
            write(iout,'(2i2,f12.5,1x,f16.6)') ni,nj,fobs(i),weight(i)
          elseif (nt.eq.5) then
            write(iout,'(''sdlc'')')
            ni = nobptr(i)/4
            nj = nobptr(i)-4*ni
            write(iout,'(2i2,f12.5,1x,f16.6)') ni,nj,fobs(i),weight(i)
          elseif (nt.eq.7) then
            write(iout,'(''piezoelectric stress '')')
            ni = nobptr(i)/7
            nj = nobptr(i) - 7*ni
            write(iout,'(i2,1x,a1,1x,f12.5,1x,f16.6)') ni,crd(nj),fobs(i),weight(i)
          elseif (nt.eq.8) then
            write(iout,'(''piezoelectric strain '')')
            ni = nobptr(i)/7
            nj = nobptr(i) - 7*ni
            write(iout,'(i2,1x,a1,1x,f12.5,1x,f16.6)') ni,crd(nj),fobs(i),weight(i)
          elseif (nt.eq.9) then
            write(iout,'(''frequency '')')
            ni = nobptr(i)
            write(iout,'(i5,1x,f12.4,1x,i4,1x,f16.6)') ni,fobs(i),nobptr2(i),weight(i)
          elseif (nt.eq.10) then
            write(iout,'(''potential'')')
            ni = npotpt0 + nobptr(i)
            write(iout,'(4(f10.6,1x,1x,f12.6))') xpotpt(ni),ypotpt(ni),zpotpt(ni),fobs(i),weight(i)
          elseif (nt.eq.11) then
            write(iout,'(''bulk_modulus'')')
            write(iout,'(f12.5,1x,f16.6)') fobs(i),weight(i)
          elseif (nt.eq.12) then
            write(iout,'(''shear_modulus'')')
            write(iout,'(f12.5,1x,f16.6)') fobs(i),weight(i)
          elseif (nt.eq.13) then
            write(iout,'(''Cv'')')
            write(iout,'(f12.5,1x,f16.6)') fobs(i),weight(i)
          elseif (nt.eq.14) then
            write(iout,'(''entropy'')')
            write(iout,'(f12.5,1x,f16.6)') fobs(i),weight(i)
          elseif (nt.eq.15) then
            write(iout,'(''hfrefractive_index'')')
            ni = nobptr(i)
            write(iout,'(i2,1x,f10.5,1x,f16.6)') ni,fobs(i),weight(i)
          elseif (nt.eq.16) then
            write(iout,'(''srefractive_index'')')
            ni = nobptr(i)
            write(iout,'(i2,1x,f10.5,1x,f16.6)') ni,fobs(i),weight(i)
          elseif (nt.eq.17) then
            write(iout,'(''sqomega '',a40,1x,f16.6)') sofomega_filename(1:40),weight(i)
          elseif (nt.eq.18) then
            write(iout,'(''bornq'')')
            ni = (nobptr(i)-1)/9 + 1
            nj = nobptr(i) - 9*(ni-1)
            nk = (nj-1)/3 + 1
            nj = nj - 3*(nk-1)
            write(iout,'(i2,1x,2a1,1x,f10.5,1x,f16.6)') ni,crd2(nk),crd2(nj),fobs(i),weight(i)
          elseif (nt.eq.19) then
            write(iout,'(''monopoleq'')')
            ni = nobptr(i)
            write(iout,'(i4,1x,f10.6,1x,f16.6)') ni,fobs(i),weight(i)
          elseif (nt.eq.21) then
            write(iout,'(''qreaxff'')')
            ni = nobptr(i)
            write(iout,'(i4,1x,f10.6,1x,f16.6)') ni,fobs(i),weight(i)
          elseif (nt.eq.22) then
            write(iout,'(''fbond'')')
            ni = nobptr(i)
            nj = nobptr2(i)
            write(iout,'(i4,1x,i4,1x,f10.6,1x,f16.6)') ni,nj,fobs(i),weight(i)
          elseif (nt.eq.23) then
            write(iout,'(''fangle'')')
            ni = nobptr(i)
            nj = nobptr2(i)
            nk = nobptr3(i)
            write(iout,'(i4,1x,2(i4,1x),f10.6,1x,f16.6)') ni,nj,nk,fobs(i),weight(i)
          elseif (nt.eq.25) then
            write(iout,'(''youngs_modulus'')')
            ni = nobptr(i)
            if (ni.eq.1) then
              write(iout,'(2x,''x'',1x,f12.5,1x,f16.6)') fobs(i),weight(i)
            elseif (ni.eq.2) then
              write(iout,'(2x,''y'',1x,f12.5,1x,f16.6)') fobs(i),weight(i)
            elseif (ni.eq.3) then
              write(iout,'(2x,''z'',1x,f12.5,1x,f16.6)') fobs(i),weight(i)
            endif
          elseif (nt.eq.26) then
            write(iout,'(''poisson_ratio'')')
            ni = nobptr(i)
            if (ni.eq.1) then
              write(iout,'(2x,''xy'',1x,f12.8,1x,f16.6)') fobs(i),weight(i)
            elseif (ni.eq.2) then
              write(iout,'(2x,''xz'',1x,f12.8,1x,f16.6)') fobs(i),weight(i)
            elseif (ni.eq.3) then
              write(iout,'(2x,''yz'',1x,f12.8,1x,f16.6)') fobs(i),weight(i)
            endif
          elseif (nt.eq.27) then
            write(iout,'(''coordno'')')
            ni = nobptr(i)
            write(iout,'(i4,1x,f10.6,1x,f10.6,1x,f16.6)') ni,fparameter(i),fobs(i),weight(i)
          elseif (nt.eq.29) then
            write(iout,'(''mode '')')
            ni = nobptr(i)
            write(iout,'(f12.4,1x,i4,1x,f16.6)') fobs(i),nobptr2(i),weight(i)
            do j = 1,nobsmodeat(ni)
              write(iout,'(3(1x,f12.8))') (fobsmode(k,j,ni),k=1,3)
            enddo
          endif
        endif
      enddo
      if (.not.ldiff) write(iout,'(''end'')')
    endif
!
!  User specified gradients for fitting
!
    nlfgrad = 0
    if (nfgrad.gt.0) then
      nlfgra = 0
      nufgra = 0
      do k = 1,nfgrad
        if (nfgracfg(k).eq.nc) then
          nufgra = k
          if (nlfgra.eq.0) nlfgra = k
        endif
      enddo
      if (nlfgra.gt.0) nlfgrad = nufgra - nlfgra + 1
    endif
    if (nlfgrad.gt.0) then
      write(iout,'(''observables'')')
      write(iout,'(''gradients'')')
      do k = nlfgra,nufgra
        ind = 3*(k-1)
        write(iout,'(i4,3(1x,f14.6))') nfgrat(k),(fgrad(j),j=ind+1,ind+3)
      enddo
      write(iout,'(''end'')')
    endif
!
!  User specified stresses for fitting
!
    nlfstress = 0
    if (nfstress.gt.0) then
      nlfstr = 0
      nufstr = 0
      do k = 1,nfstress
        if (nfstrcfg(k).eq.nc) then
          nufstr = k
          if (nlfstr.eq.0) nlfstr = k
        endif
      enddo
      if (nlfstr.gt.0) nlfstress = nufstr - nlfstr + 1
    endif
    if (nlfstress.gt.0) then
      write(iout,'(''observables'')')
      write(iout,'(''stress'')')
      do k = nlfstr,nufstr
        write(iout,'(i4,1x,f14.6)') nfstrt(k),fstress(k)
      enddo
      write(iout,'(''end'')')
    endif
!
!  Constraints output
!
    if (nc.eq.ncfg) then
      if (ncontot.gt.0) then
        nlcon = ncontot + 1 - n1con(nc)
      else
        nlcon = 0
      endif
    else
      nlcon = n1con(nc+1) - n1con(nc)
    endif
    if (nlcon.gt.0.and.(nconin.gt.0.or.index(keyword,'outc').ne.0)) then
      nlcfst = n1con(nc) - 1
      write(iout,'(''variables'')')
      if (nlcon.eq.1) then
        write(iout,'(''constrain'')')
      else
        write(iout,'(''constrain '',i4)') nlcon
      endif
      ncm = 3*nlasym + nstrains
      do i = 1,nlcon
        nfv = ncfixcfg(i+nlcfst)
        nvv = ncvarcfg(i+nlcfst)
        if (nvv.gt.ncm) then
          nfv = nfv - ncm
          nvv = nvv - ncm
          write(iout,'(i4,1x,''r'',1x,i4,1x,''r'',1x,f12.6,1x,f12.6)')nvv,nfv,concocfg(i+nlcfst),conaddcfg(i+nlcfst)
        elseif (nvv.gt.nstrains) then
          nfv = nfv - (nstrains + 1)
          nvv = nvv - (nstrains + 1)
          nfv1 = (nfv/3) + 1
          nvv1 = (nvv/3) + 1
          ncrf = nfv - 3*(nfv1-1) + 1
          ncrv = nvv - 3*(nvv1-1) + 1
          write(iout,'(i4,1x,a1,i4,1x,a1,1x,f12.6,1x,f12.6)') &
            nvv1,crd(ncrv),nfv1,crd(ncrf),concocfg(i+nlcfst),conaddcfg(i+nlcfst)
        else
          write(iout,'(i4,1x,i4,2x,f12.6,1x,f12.6)') nvv,nfv,concocfg(i+nlcfst),conaddcfg(i+nlcfst)
        endif
      enddo
      write(iout,'(''end'')')
    endif
!**************************
!  External force option  *
!**************************
    lfirstout = .true.
    do i = 1,nlasym
      forcenorm = abs(forcecfg(1,nlsft+i)) + abs(forcecfg(2,nlsft+i)) + abs(forcecfg(3,nlsft+i))
      if (forcenorm.gt.1.0d-6) then
        if (lfirstout) then
          lfirstout = .false.
          write(iout,'(''external_force'')')
        endif
        write(iout,'(i6,3(1x,f14.6))') i,(forcecfg(j,nlsft+i),j=1,3)
      endif
    enddo
!*****************************************
!  Time-dependent External force option  *
!*****************************************
    lfirstout = .true.
    do i = 1,nlasym
      do j = 1,3
        if (ltdforcecfg(j,nlsft+i)) then
          if (lfirstout) then
            lfirstout = .false.
            write(iout,'(''external_force'')')
          endif
          write(iout,'(i6,1x,a1,3(1x,f14.6))') i,crd(j),(tdforcecfg(k,j,nlsft+i),k=1,3)
        endif
      enddo
    enddo
!******************************
!  Translational scan option  *
!******************************
    if (ntran(nc).ne.0) then
      write(iout,'(''translate '',3(f12.6,1x),i5)') xtran(nc),ytran(nc),ztran(nc),ntran(nc)
    endif
!******************
!  COSMO options  *
!******************
    if (cosmoepsilon(nc).ne.1.0_dp) then
      write(iout,'(''solventepsilon '',f10.6)') cosmoepsilon(nc)
    endif
    if (cosmorsolv(nc).ne.1.0_dp.or.cosmodrsolv(nc).ne.0.1_dp) then
      write(iout,'(''solventradius  '',2f10.6)') cosmorsolv(nc),cosmodrsolv(nc)
    endif
    if (lcosmoeigin(nc)) then
      write(iout,'(''cosmoframe '')')
      do i = 1,3
        write(iout,'(3(2x,f10.7))')(cosmoeigen(j,i,nc),j=1,3)
      enddo
    endif
    if (nsasexcludemin(nc).ne.-1) then
      if (nsasexcludemax(nc).ne.-1) then
        write(iout,'(''sasexclude '',i7,1x,i7)') nsasexcludemin(nc),nsasexcludemax(nc)
      else
        write(iout,'(''sasexclude '',i7)') nsasexcludemin(nc)
      endif
    endif
!***************************************
!  Lowest phonon mode for free energy  *
!***************************************
    if (minmodecfg(nc).ne.1) then
      if (maxmodecfg(nc).ne.0) then
        write(iout,'(''lowest_mode '',i4,1x,i4)') minmodecfg(nc),maxmodecfg(nc)
      else
        write(iout,'(''lowest_mode '',i4)') minmodecfg(nc)
      endif
    endif
!********************
!  Phonon k points  *
!********************
    if (nkpt.gt.0) then
      nllkpt = norigkpt(nc)
      if (nllkpt.gt.0) then
        nlkpt = 0
        do i = 1,nkpt
          nk = nkptcfg(i)
          if (nlkpt.eq.0.and.nk.eq.nc) nlkpt = i
        enddo
        nukpt = nlkpt + nllkpt - 1
        write(iout,'(''kpoints '',i4)') nllkpt
        do i = nlkpt,nukpt
          write(iout,'(3f7.4,f8.4)') xkpt(i),ykpt(i),zkpt(i),wkpt(i)
        enddo
      endif
    endif
!**********************************************
!  Born charge correction approach direction  *
!**********************************************
    if (ndimen(nc).gt.0) then
      if (bornk(1,nc).ne.1.0_dp.or.bornk(2,nc).ne.1.0_dp.or.bornk(3,nc).ne.1.0_dp) then
        write(iout,'(''gamma_direction_of_approach'',3(1x,f8.5))') bornk(1,nc),bornk(2,nc),bornk(3,nc)
      endif
      if (nbornstep(nc).ne.0) then
        write(iout,'(''gamma_angular_steps'',1x,i6)') nbornstep(nc)
      endif
    endif
!*************************************
!  Frequency for properties - omega  *
!*************************************
    if (omega(nc).ne.0.0_dp.or.omegastep(nc).ne.0.0_dp.or.nomegastep(nc).ne.0) then
      write(iout,'(''omega '',2(1x,f12.4),1x,i6)') omega(nc),omegastep(nc),nomegastep(nc)
    endif
    if (omegadamping(nc).ne.5.0_dp) then
      write(iout,'(''omega_damping '',f12.6)') omegadamping(nc)
    endif
    sum = 0.0_dp
    do i = 1,6
      sum = sum + abs(omegadir(i,nc))
    enddo
    if (sum.gt.0.0_dp) then
      if (omegadirtype(nc).eq.2) then
        write(iout,'(''odirection frac in'',3(1x,f7.4),'' out'',3(1x,f7.4))') (omegadir(i,nc),i=1,6)
      else
        write(iout,'(''odirection in'',3(1x,f7.4),'' out'',3(1x,f7.4))') (omegadir(i,nc),i=1,6)
      endif
    endif
!****************************
!  Phonon dispersion lines  *
!****************************
    if (ndline.gt.0) then
!
!  Find first and last points for this configuration
!
      nlpdp = 0
      do i = 1,ndpoint
        if (ndispcfg(i).eq.nc) then
          if (nlpdp.eq.0) nlpdp = i
          nupdp = i
        endif
      enddo
      if (nlpdp.gt.0) then
        write(iout,'(''dispersion '',i4,1x,i2)') nupdp-nlpdp+1,ndispres
!
!  Loop over dispersion lines
!
        do i = nlpdp,nupdp
          do j = ndstart(i),ndend(i)-1
            write(iout,'(3f6.3,'' to '')',advance='no') xdisp(j),ydisp(j),zdisp(j)
          enddo
          write(iout,'(3f6.3)') xdisp(ndend(i)),ydisp(ndend(i)),zdisp(ndend(i))
        enddo
      endif
    endif
!**********************
!  Shrinking factors  *
!**********************
    if (ndimen(nc).eq.3) then
      if (nxks(nc)*nyks(nc)*nzks(nc).gt.0) then
        nxk = nxks(nc)
        nyk = nyks(nc)
        nzk = nzks(nc)
        if (nxk.eq.nyk.and.nyk.eq.nzk) then
          write(iout,'(''shrink '',3i3)') nxk
        else
          write(iout,'(''shrink '',3i3)') nxk,nyk,nzk
        endif
      endif
    elseif (ndimen(nc).eq.2) then
      if (nxks(nc)*nyks(nc).gt.0) then
        nxk = nxks(nc)
        nyk = nyks(nc)
        if (nxk.eq.nyk) then
          write(iout,'(''shrink '',3i3)') nxk
        else
          write(iout,'(''shrink '',3i3)') nxk,nyk
        endif
      endif
    elseif (ndimen(nc).eq.1) then
      if (nxks(nc).gt.0) then
        nxk = nxks(nc)
        write(iout,'(''shrink '',3i3)') nxk
      endif
    endif
!***************************
!  Phonon DOS projections  *
!***************************
    nproj = nprojcfg(ncf)
    nprojd = nprojdef(ncf)
    if (nproj.gt.0) then
      npfirst = 1
      npifirst = 1
      ii = 0
      do i = 1,ncf-1
        npc = nprojcfg(i)
        npfirst = npfirst+npc
        do j = 1,npc
          npifirst = npifirst+nprojit(ii+j)
        enddo
        ii = ii+npc
      enddo
      nplast = npfirst + nproj - 1
      npilast = npifirst
      do i = 1,nproj
        npilast = npilast + nprojit(ii+i)
      enddo
      npilast = npilast - 1
      if (nprojd.gt.0) then
        write(iout,'(''project_dos defect '',i4)') nprojd
        do np = npfirst,nplast
          if (nprojdb(np).eq.2) then
            do k = 1,80
              line(k:k) = ' '
            enddo
            npro = 0
            do npi = npifirst,npilast
              if (nprojptr(npi).eq.np) then
                if (nprojtyp(npi).gt.99) then
                  inum = nprojnat(npi)
                  call intochar(word5,inum)
                  line(npro+1:npro+5) = word5
                else
                  inat = nprojnat(npi)
                  itype = nprojtyp(npi)
                  call label(inat,itype,lab1)
                  line(npro+1:npro+5) = lab1
                endif
              endif
              npro = npro + 6
            enddo
            write(iout,'(a80)') line
          endif
        enddo
      endif
      if ((nproj-nprojd).gt.0) then
        write(iout,'(''project_dos bulk '',i4)') nproj-nprojd
        do np = npfirst,nplast
          if (nprojdb(np).eq.1) then
            do k = 1,80
              line(k:k) = ' '
            enddo
            npro = 0
            do npi = npifirst,npilast
              if (nprojptr(npi).eq.np) then
                if (nprojtyp(npi).gt.99) then
                  inum = nprojnat(npi)
                  call intochar(word5,inum)
                  line(npro+1:npro+5) = word5
                else
                  inat = nprojnat(npi)
                  itype = nprojtyp(npi)
                  call label(inat,itype,lab1)
                  line(npro+1:npro+5) = lab1
                endif
              endif
              npro = npro+6
            enddo
          endif
        enddo
      endif
    endif
!**********************
!  Eigenvector range  *
!**********************
    if (neiglow(nc).ne.0) then
      if (neighigh(nc).ne.0) then
        write(iout,'(''eigenvectors '',i5,'' to '',i5)')neiglow(nc),neighigh(nc)
      else
        write(iout,'(''eigenvectors '',i5)')neiglow(nc)
      endif
    endif
!****************
!  Temperature  *
!****************
    if (tempcfg(nc).ne.0.0_dp) then
      if (ntempstp(nc).ne.0) then
        if (ntempstpstart(nc).ne.0) then
          write(iout,'(''temperature '',f10.3,1x,f15.9,1x,2(i10,1x))') tempcfg(nc),tempstp(nc),ntempstp(nc), &
            ntempstpstart(nc)
        else
          write(iout,'(''temperature '',f10.3,1x,f15.9,1x,i10)') tempcfg(nc),tempstp(nc),ntempstp(nc)
        endif
      else
        write(iout,'(''temperature '',f10.3)') tempcfg(nc)
      endif
    endif
!*************
!  Pressure  *
!*************
    if (presscfg(nc).ne.0.0_dp) then
      write(iout,'(''pressure '',f13.3)') presscfg(nc)
    endif
    if (lanisotropicpresscfg(nc)) then
      write(iout,'(''anisotropic_pressure'',3(1x,f10.4),'' &'')') (anisotropicpresscfg(j,nc),j=1,3)
      write(iout,'(20x,3(1x,f10.4))') (anisotropicpresscfg(j,nc),j=4,6)
    endif
!******************
!  Stress tensor  *
!******************
    sum = 0.0_dp
    do j = 1,nstrains
      sum = sum+abs(stresscfg(j,nc))
    enddo
!        if (sum.gt.1.0d-12) then
!          write(iout,'(''stress '')')
!          write(iout,'(6(g12.6,1x))')(stresscfg(j,nc),j=1,nstrains)
!        endif
!************
!  Potgrid  *
!************
    if (nxpg(nc).ne.0) then
      write(iout,'(''potgrid '',6f8.4,3(1x,i3))') xminpg(nc),xmaxpg(nc),yminpg(nc),ymaxpg(nc),zminpg(nc), &
        zmaxpg(nc),nxpg(nc),nypg(nc),nzpg(nc)
    endif
!********************
!  Potential sites  *
!********************
    lfirstout = .true.
    do j = 1,npotsites
      if (npotsitecfg(j).eq.nc) then
        if (lfirstout) then
          lfirstout = .false.
          write(iout,'(''potsites'')')
        endif
        write(iout,'(3(1x,f15.6))') xpotsite(j),ypotsite(j),zpotsite(j)
      endif
    enddo
!***********************
!  Molecular dynamics  *
!***********************
    if (nensemble(nc).eq.2) then
      if (nmdintegrator.eq.4) then
        write(iout,'(''ensemble nvt '')') 
      else
        write(iout,'(''ensemble nvt '',f12.6)') qtemp(nc)
      endif
    elseif (nensemble(nc).eq.3) then
      if (nmdintegrator.eq.4) then
        write(iout,'(''ensemble npt '')') 
      else
        write(iout,'(''ensemble npt '',f12.6,1x,f12.6)') qtemp(nc),qpres(nc)
      endif
    elseif (nensemble(nc).eq.4) then
      write(iout,'(''ensemble nph '')') 
    endif
    if (tstep(nc).gt.1.0d-12) then
      write(iout,'(''timestep '',8x,f10.6,'' ps'')') tstep(nc)
    endif
    if (tmdscale(nc).gt.1.0d-12) then
      write(iout,'(''tscale '',7x,f14.6,1x,f8.4,'' ps'')') tmdscale(nc),tmdscint(nc)
    endif
    if (tmdeq(nc).gt.1.0d-12) then
      write(iout,'(''equilibration '',f14.6,'' ps'')') tmdeq(nc)
    elseif (nmdeq(nc).ne.0) then
      write(iout,'(''equilibration '',i6)') nmdeq(nc)
    endif
    if (tmdprod(nc).gt.1.0d-12) then
      write(iout,'(''production   '',f14.6,'' ps'')') tmdprod(nc)
    elseif (nmdprod(nc).ne.0) then
      write(iout,'(''production   '',i6)') nmdprod(nc)
    endif
    if (tmdsamp(nc).gt.1.0d-12) then
      write(iout,'(''sample '',8x,f12.6,'' ps'')') tmdsamp(nc)
    elseif (nmdsamp(nc).ne.0) then
      write(iout,'(''sample '',8x,i6)') nmdsamp(nc)
    endif
    if (taubcfg(nc).ne.1.0_dp) then
      write(iout,'(''tau_barostat   '',8x,f8.4,'' ps'')') taubcfg(nc)
    endif
    if (tautcfg(nc).ne.1.0_dp) then
      write(iout,'(''tau_thermostat '',8x,f8.4,'' ps'')') tautcfg(nc)
    endif
    if (lpflxoutput) then
      write(iout,'(''p_flexible '')') 
      write(iout,'(3(2x,f20.12))') (p_flx(j,1),j=1,3)
      write(iout,'(3(2x,f20.12))') (p_flx(j,2),j=1,3)
      write(iout,'(3(2x,f20.12))') (p_flx(j,3),j=1,3)
    endif
    if (lpisooutput) then
      write(iout,'(''p_isotropic '',f20.12)') p_iso
    endif
    if (nc.eq.ncf) then
      if (pr_cons.ne.0.0_dp) then
        write(iout,'(''intconserved'',1x,f16.8)') pr_cons
      endif
    else
      if (pr_conscfg(nc).ne.0.0_dp) then
        write(iout,'(''intconserved'',1x,f16.8)') pr_conscfg(nc)
      endif
    endif
    if (tmdwrite(nc).gt.1.0d-12) then
      write(iout,'(''write_MD '',6x,f10.4,'' ps'')') tmdwrite(nc)
    elseif (nmdwrite(nc).ne.0) then
      write(iout,'(''write_MD '',6x,i6)') nmdwrite(nc)
    endif
    if (tmdforcestart(nc).gt.1.0d-12) then
      write(iout,'(''delay_force '',f12.4,'' ps'')') tmdforcestart(nc)
    endif
    if (tmdforcestop(nc).gt.1.0d-12) then
      write(iout,'(''end_force   '',f12.4,'' ps'')') tmdforcestop(nc)
    endif
    if (tmdfieldstart(nc).gt.1.0d-12) then
      write(iout,'(''delay_field '',f12.4,'' ps'')') tmdfieldstart(nc)
    endif
    if (tmdfieldstop(nc).gt.1.0d-12) then
      write(iout,'(''end_field   '',f12.4,'' ps'')') tmdfieldstop(nc)
    endif
    if ((nmdvelmode(nc)+nmdvelmodp(nc)).gt.-2) then
      write(iout,'(''momentum_correct '',i10,1x,i10)') nmdvelmode(nc),nmdvelmodp(nc)
    endif
    if (lmdconstrain(nc)) then
      write(iout,'(''mdconstraint '',i6,1x,i6,1x,f10.6)')nmdconstrainatom(1,nc), &
        nmdconstrainatom(2,nc),nmdconstraindist(nc)
    endif
    if (ndistancereset(nc).ne.1) then
      write(iout,'(''resetvectors     '',i10)') ndistancereset(nc)
    endif
    if (lmd) then
!
!  Tether option
!
      lfirstfind = .true.
      nsearch1 = 0
      line2 = ' '
      nwptr = 0
      do while (nsearch1.lt.numat) 
        nsearch1 = nsearch1 + 1
        if (lfix(nsearch1)) then
          if (lfirstfind) then
            write(line2,'(''tether '')')
            nwptr = 7
          endif
          nsearch2 = nsearch1
          lstillfixed = .true.
          do while (nsearch2.lt.numat.and.lstillfixed)
            nsearch2 = nsearch2 + 1
            lstillfixed = lfix(nsearch2)
          enddo
!
!  At this point we will have gone one past the end of the fixed atoms and 
!  so we need to subtract 1 from the end atom number
!
          nsearch2 = nsearch2 - 1
          if (nsearch2.gt.nsearch1) then
!
!  Will writing exceed line length?
!
            if (nwptr+16.gt.80) then
              write(iout,'(a)') line2
              line2 = ' '
              write(line2,'(''tether '')')
              nwptr = 7
              lfirstfind = .true.
            endif
!
!  Write out a range of atoms
!
            if (lfirstfind) then
              write(line2(nwptr+1:nwptr+15),'(i7,''-'',i7)') nsearch1,nsearch2
              nwptr = nwptr + 15
            else
              write(line2(nwptr+1:nwptr+16),'('','',i7,''-'',i7)') nsearch1,nsearch2
              nwptr = nwptr + 16
            endif
          else
!
!  Will writing exceed line length?
!
            if (nwptr+8.gt.80) then
              write(iout,'(a)') line2
              line2 = ' '
              write(line2,'(''tether '')')
              nwptr = 7
              lfirstfind = .true.
            endif
!
!  Write out a single atom
!
            if (lfirstfind) then
              write(line2(nwptr+1:nwptr+7),'(i7)') nsearch1
              nwptr = nwptr + 7
            else
              write(line2(nwptr+1:nwptr+8),'('','',i7)') nsearch1
              nwptr = nwptr + 8
            endif
          endif
          lfirstfind = .false.
!
!  Having searched for a block of fixed atoms, set first atom search
!  number to one past the point of end of last search
!
          nsearch1 = nsearch2 + 1
        endif
      enddo
      if (.not.lfirstfind) then
!
!  Write final line
!
        write(iout,'(a)') line2
      endif
    endif
!
!  Latest position info for MD restarts
!
    if (nc.eq.ncf.and.timesofar.gt.0.0_dp) then
      write(iout,'(''# '')')
      write(iout,'(''# Start of MD restart information:'')')
      write(iout,'(''# '')')
      write(iout,'(''current_time '',f12.6,'' ps'')') timesofar
      write(iout,'(''aver '',5(g14.6,1x))') sumvsq,sumener,sumvir,sumtem,sumcons
      write(iout,'(f18.9,1x,i8)') sumcst,naverpt
      if (lmdconstrain(ncf)) then
        write(iout,'(''cfaver '',2(g30.10,1x))') 4.0_dp*sumlambdaR/stpsqh,4.0_dp*sumlambdaV/stpsqh
      endif
      if (nensemble(ncf).eq.3) then
        write(iout,'(''caver '',3(g14.6,1x))') sumacell,sumbcell,sumccell
        write(iout,'(6x,4(g14.6,1x))') sumalpcell,sumbetcell,sumgamcell,sumvol
      endif
      write(iout,'(''absolute_coordinates'')')
      do i = 1,numat
        write(iout,'(i7,3x,3(f15.9,1x))') i,xalat(i),yalat(i),zalat(i)
      enddo
      if (nensemble(ncf).eq.3) then
        write(iout,'(''cvec 1 '',3(f15.9,1x))') xcell(1),xcell(2),xcell(3)
        write(iout,'(''cvec 2 '',3(f15.9,1x))') xcell(4),xcell(5),xcell(6)
        write(iout,'(''cvec 3 '',3(f15.9,1x))') xcell(7),xcell(8),xcell(9)
      endif
      write(iout,'(''velocities angs/ps'')')
      rtstp = 1.0_dp/tstep(nc)
      do i = 1,numat
        vx = velx(i)*rtstp
        vy = vely(i)*rtstp
        vz = velz(i)*rtstp
        vsq = vx*vx + vy*vy + vz*vz
        if (vsq.gt.1.0d-10) then
          write(iout,'(i7,3x,3(g15.9,1x))') i,vx,vy,vz
        endif
      enddo
      if (nensemble(ncf).ge.2) then
        write(iout,'(''thermostat '',2(g15.9,1x))') sfac*rtstp,sumsfac*rtstp
      endif
      if (nensemble(ncf).eq.3) then
        write(iout,'(''cvec 1 '',3(g15.9,1x))') velc(1)*rtstp,velc(2)*rtstp,velc(3)*rtstp
        write(iout,'(''cvec 2 '',3(g15.9,1x))') velc(4)*rtstp,velc(5)*rtstp,velc(6)*rtstp
      endif
!
      if (latomicstress) then
        write(iout,'(''catomic_stress eV'')')
        if (ndim.eq.3) then
          do i = 1,numat
            write(iout,'(i7,3x,3(g15.9,1x),'' &'')') i,(sumatomicstress(j,i),j=1,3)
            write(iout,'(10x,3(g15.9,1x))') (sumatomicstress(j,i),j=4,6)
          enddo
        elseif (ndim.eq.2) then
          do i = 1,numat
            write(iout,'(i7,3x,3(g15.9,1x))') i,(sumatomicstress(j,i),j=1,3)
          enddo
        elseif (ndim.eq.1) then
          do i = 1,numat
            write(iout,'(i7,3x,g15.9)') i,sumatomicstress(1,i)
          enddo
        endif
      endif
!
      if (nmdintegrator.eq.1) then
!
!  Gear fifth order
!
        write(iout,'(''accelerations 4'')')
        do i = 1,numat
          if ((abs(x2(i))+abs(y2(i))+abs(z2(i))).gt.1.0d-6) then
            rtfct = rtstp*rtstp*0.001_dp
            x2i = x2(i)*rtfct
            y2i = y2(i)*rtfct
            z2i = z2(i)*rtfct
            rtfct = rtfct*rtstp*0.01_dp
            x3i = x3(i)*rtfct
            y3i = y3(i)*rtfct
            z3i = z3(i)*rtfct
            rtfct = rtfct*rtstp*0.01_dp
            x4i = x4(i)*rtfct
            y4i = y4(i)*rtfct
            z4i = z4(i)*rtfct
            rtfct = rtfct*rtstp*0.01_dp
            x5i = x5(i)*rtfct
            y5i = y5(i)*rtfct
            z5i = z5(i)*rtfct
            write(iout,'(i6,1x,6(1x,g11.5))') i,x2i,y2i,z2i,x3i,y3i,z3i
            write(iout,'(7x,6(1x,g11.5))') x4i,y4i,z4i,x5i,y5i,z5i
          endif
        enddo
      elseif (nmdintegrator.eq.2) then
!
!  Velocity Verlet
!
        write(iout,'(''accelerations 1'')')
        do i = 1,numat
          if ((abs(x2(i))+abs(y2(i))+abs(z2(i))).gt.1.0d-6) then
            rtfct = rtstp*rtstp*0.001_dp
            x2i = x2(i)*rtfct
            y2i = y2(i)*rtfct
            z2i = z2(i)*rtfct
            write(iout,'(i7,3x,3(1x,g11.5))') i,x2i,y2i,z2i
          endif
        enddo
      endif
      write(iout,'(''# '')')
      write(iout,'(''# End of MD restart information'')')
      write(iout,'(''# '')')
    endif
    if (lmc.and.ngcmcmol.gt.0) then
!
!  Info for MC restarts
!
      nexistgcmcmol = 0
      do i = 1,nmol
        if (lgcmcmol(i)) nexistgcmcmol = nexistgcmcmol + 1
      enddo 
      if (nexistgcmcmol.gt.0) then
        write(iout,'(''# '')')
        write(iout,'(''# Start of MC restart information:'')')
        write(iout,'(''# '')')
        nmol1 = 0
        do while (nmol1.lt.nmol)
          nmol1 = nmol1 + 1
          if (lgcmcmol(nmol1)) then
            nmol2 = nmol1
            lend = .false.
            do while (nmol2.lt.nmol.and..not.lend)
              nmol2 = nmol2 + 1
              lend = (.not.lgcmcmol(nmol2))
            enddo
            if (nmol1.eq.nmol2) then
              write(iout,'(''gcmcexistingmolecules '',i8)') nmol1
            else
              write(iout,'(''gcmcexistingmolecules '',i8,'' to '',i8)') nmol1,nmol2
            endif
!
!  Dump list of atoms in molecules
!
            do imol = nmol1,nmol2
              nmolgroup = (nmolatom(imol)-1)/6 + 1
              imatm = nmolptr(imol)
              imatmlast = nmolptr(imol) + nmolatom(imol)
              do imolgroup = 1,nmolgroup
                imatmend = min(imatm+6,imatmlast)
                if (imolgroup.eq.1.and.nmolgroup.eq.1) then
                  write(iout,'(i4,6(1x,i7))') nmolatom(imol),(nmollist(j),j=imatm+1,imatmend)
                elseif (imolgroup.eq.1) then
                  write(iout,'(i4,6(1x,i7),'' &'')') nmolatom(imol),(nmollist(j),j=imatm+1,imatmend)
                elseif (imolgroup.eq.nmolgroup) then
                  write(iout,'(4x,6(1x,i7))') (nmollist(j),j=imatm+1,imatmend)
                else
                  write(iout,'(4x,6(1x,i7))') (nmollist(j),j=imatm+1,imatmend)
                endif
                imatm = imatm + 6
              enddo
            enddo
            nmol1 = nmol2 + 1
          endif
        enddo
        write(iout,'(''# '')')
        write(iout,'(''# End of MC restart information'')')
        write(iout,'(''# '')')
      endif
    endif
!****************************
!  Defect calcn parameters  *
!****************************
!
!  Find number of defects, if any for current structure
!
    nds = 0
    ndf = - 1
    do i = 1,ndef
      if (ndefcfg(i).eq.nc) then
        if (nds.eq.0) nds = i
        ndf = i
      endif
    enddo
    ndefst = nds - 1
    nldef = ndf - nds + 1
!
!  Defect centre
!
    if (nldef.gt.0) then
      if (nc.eq.ncf.and.ldefect) then
        write(iout,'(''centre cart '',3f9.4)') xdc,ydc,zdc
      else
        if (ndcentyp(nc).eq.1) then
          iptr = nint(xdcent(nc))
          write(iout,'(''centre '',i4)') iptr
        elseif (ndcentyp(nc).eq.2) then
          iptr = nint(xdcent(nc))
          inat = natcfg(iptr)
          itype = ntypcfg(iptr)
          call label(inat,itype,lab1)
          write(iout,'(''centre '',a5)') lab1
        elseif (ndcentyp(nc).eq.3) then
          write(iout,'(''centre '',3f7.4)') xdcent(nc),ydcent(nc),zdcent(nc)
        elseif (ndcentyp(nc).eq.4) then
          write(iout,'(''centre cart '',3f9.4)') xdcent(nc),ydcent(nc),zdcent(nc)
        elseif (ndcentyp(nc).eq.5) then
          write(iout,'(''centre molecule '',i3)') nint(xdcent(nc))
        endif
      endif
    endif
!
!  Region sizes
!
    if (reg1(nc).gt.0.0_dp.or.reg2(nc).gt.0.0_dp) then
      if (reg1last(nc).gt.0.0_dp) then
        if (ldcellr) then
          write(iout,'(''size neutral '',3(f8.4,1x))') reg1(nc),reg2(nc),reg1last(nc)
        else
          write(iout,'(''size '',3(f8.4,1x))') reg1(nc),reg2(nc),reg1last(nc)
        endif
      else
        if (ldcellr) then
          write(iout,'(''size neutral '',f8.4,1x,f8.4)') reg1(nc),reg2(nc)
        else
          write(iout,'(''size '',f8.4,1x,f8.4)') reg1(nc),reg2(nc)
        endif
      endif
    endif
!
!  Move 2a ions to region 1
!
    if (reg2a1(nc).gt.0.0_dp) then
      if (reg2a1(nc).gt.reg2(nc)) then
        write(iout,'(''move_2a_to_1'')')
      else
        write(iout,'(''move_2a_to_1 '',f8.4)') reg2a1(nc)
      endif
    endif
!************
!  Defects  *
!************
    if (nldef.gt.0) then
      if (nc.lt.ncf) then
!
!  Output saved final region 1
!
        read(42) ncc,nr1
        write(iout,'(''region_1 '',i4)') nr1
        do i = 1,nr1
          read(42) inat,itype,xal,yal,zal,q,oc,ri,nm,nmi,lbre,ldqm
          call label(inat,itype,lab1)
          if (ldqm) then
            if (lbre) then
              if (inat.gt.maxele) then
                lab2 = 'qbs '
              else
                lab2 = 'qbc '
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'qsh '
              else
                lab2 = 'qco '
              endif
            endif
          else
            if (lbre) then
              if (inat.gt.maxele) then
                lab2 = 'bsh '
              else
                lab2 = 'bco '
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'she '
              else
                lab2 = 'cor '
              endif
            endif
          endif
          read(42) id1,id2,id3
          if (lflags) then
            write(iout,'(a5,1x,a4,3(f10.5,1x),f9.5,1x,f5.3,1x,f6.4,1x,i2,1x,i3,3i2)') &
              lab1,lab2,xal,yal,zal,q,oc,ri,nm,nmi,id1,id2,id3
          else
            if (id1.eq.1.and.id2.eq.1.and.id3.eq.1) then
              fixword = ' '
            elseif (id1.eq.0.and.id2.eq.1.and.id3.eq.1) then
              fixword = 'fix x'
            elseif (id1.eq.1.and.id2.eq.0.and.id3.eq.1) then
              fixword = 'fix y'
            elseif (id1.eq.1.and.id2.eq.1.and.id3.eq.0) then
              fixword = 'fix z'
            elseif (id1.eq.0.and.id2.eq.0.and.id3.eq.1) then
              fixword = 'fix xy'
            elseif (id1.eq.0.and.id2.eq.1.and.id3.eq.0) then
              fixword = 'fix xz'
            elseif (id1.eq.1.and.id2.eq.0.and.id3.eq.0) then
              fixword = 'fix yz'
            else
              fixword = 'fix xyz'
            endif
            write(iout,'(a5,1x,a4,3(f10.5,1x),f9.5,1x,f6.4,1x,f6.4,1x,i2,1x,i3,1x,a7)') &
              lab1,lab2,xal,yal,zal,q,oc,ri,nm,nmi,fixword
          endif
        enddo
        if (mode2a.ge.3) then
          write(iout,'(''deflist '',i4,1x,i4)') nvaca,ninte
          write(iout,'(15(i4,1x))')(ndptr(i),i=1,nvaca+ninte)
        endif
        if (lmol) then
          allocate(itmp(nr1),stat=status)
          if (status/=0) call outofmemory('dumpdur','itmp')
          read(42)(itmp(i),i=1,nr1)
          write(iout,'(''reldef '')')
          write(iout,'(15(i4,1x))')(itmp(i),i=1,nr1)
          deallocate(itmp,stat=status)
          if (status/=0) call deallocate_error('dumpdur','itmp')
        endif
      elseif (nc.eq.ncf.and.nreg1.gt.0) then
!
!  Output current region 1
!
        allocate(itmp(3*nreg1),stat=status)
        if (status/=0) call outofmemory('dumpdur','itmp')
        if (ldsym) then
          allocate(itmp2(3*ndasym),stat=status)
          if (status/=0) call outofmemory('dumpdur','itmp2')
          do i = 1,3*ndasym
            itmp2(i) = 0
          enddo
          do i = 1,nvar
            itmp2(idopt(i)) = 1
          enddo
!
!  As defect constraints are not output we need
!  to set flags for symmetry constrained coordinates
!  to 1 as well.
!
          do i = 1,ndcon
            nf = ncdfix(i)
            nv = ncdvar(i)
            itmp2(nf) = itmp2(nv)
          enddo
          do i = 1,nreg1
            indii = 3*(ndrel(i)-1)
            ii1 = itmp2(indii+1)
            ii2 = itmp2(indii+2)
            ii3 = itmp2(indii+3)
            k = ndrelop(i)
            indi = 3*(i-1)
            itmp(indi+1) = abs(nint(ii1*dsymop(1,1,k)+ii2*dsymop(1,2,k)+ii3*dsymop(1,3,k)))
            itmp(indi+2) = abs(nint(ii1*dsymop(2,1,k)+ii2*dsymop(2,2,k)+ii3*dsymop(2,3,k)))
            itmp(indi+3) = abs(nint(ii1*dsymop(3,1,k)+ii2*dsymop(3,2,k)+ii3*dsymop(3,3,k)))
          enddo
          deallocate(itmp2,stat=status)
          if (status/=0) call deallocate_error('dumpdur','itmp2')
        else
          do i = 1,3*nreg1
            itmp(i) = 0
          enddo
          do i = 1,nvar
            itmp(idopt(i)) = 1
          enddo
        endif
        write(iout,'(''region_1 '',i4)') nreg1
        do i = 1,nreg1
          inat = natdefe(i)
          itype = ntypdefe(i)
          call label(inat,itype,lab1)
          if (ldqmatom(i)) then
            if (ldefbsmat(i)) then
              if (inat.gt.maxele) then
                lab2 = 'qbs '
              else
                lab2 = 'qbc '
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'qsh '
              else
                lab2 = 'qco '
              endif
            endif
          else
            if (ldefbsmat(i)) then
              if (inat.gt.maxele) then
                lab2 = 'bsh '
              else
                lab2 = 'bco '
              endif
            else
              if (inat.gt.maxele) then
                lab2 = 'she '
              else
                lab2 = 'cor '
              endif
            endif
          endif
          if (lflags) then
            write(iout,'(a5,1x,a4,3(f10.5,1x),f9.5,1x,f5.3,1x,f6.4,1x,i2,1x,i3,1x,3i2)') &
              lab1,lab2,xdefe(i),ydefe(i),zdefe(i),qdefe(i),occdefe(i),radefe(i),ndefmol(i), &
              ndefind(i),itmp(3*(i-1)+1),itmp(3*(i-1)+2),itmp(3*(i-1)+3)
          else
            id1 = itmp(3*(i-1)+1)
            id2 = itmp(3*(i-1)+2)
            id3 = itmp(3*(i-1)+3)
            if (id1.eq.1.and.id2.eq.1.and.id3.eq.1) then
              fixword = ' '
            elseif (id1.eq.0.and.id2.eq.1.and.id3.eq.1) then
              fixword = 'fix x'
            elseif (id1.eq.1.and.id2.eq.0.and.id3.eq.1) then
              fixword = 'fix y'
            elseif (id1.eq.1.and.id2.eq.1.and.id3.eq.0) then
              fixword = 'fix z'
            elseif (id1.eq.0.and.id2.eq.0.and.id3.eq.1) then
              fixword = 'fix xy'
            elseif (id1.eq.0.and.id2.eq.1.and.id3.eq.0) then
              fixword = 'fix xz'
            elseif (id1.eq.1.and.id2.eq.0.and.id3.eq.0) then
              fixword = 'fix yz'
            else
              fixword = 'fix xyz'
            endif
            write(iout,'(a5,1x,a4,3(f10.5,1x),f9.5,1x,f6.4,1x,f6.4,1x,i2,1x,i3,1x,a7)') &
              lab1,lab2,xdefe(i),ydefe(i),zdefe(i),qdefe(i),occdefe(i),radefe(i),ndefmol(i), &
              ndefind(i),fixword
          endif
        enddo
        if (mode2a.ge.3) then
          write(iout,'(''deflist '',i4,1x,i4)') nvaca,ninte
          write(iout,'(15(i4,1x))') (ndptr(i),i=1,nvaca+ninte)
        endif
        if (lmol) then
          write(iout,'(''reldef '')')
          write(iout,'(15(i4,1x))') (nreldef(i),i=1,nreg1)
        endif
        deallocate(itmp,stat=status)
        if (status/=0) call deallocate_error('dumpdur','itmp')
      else
!
!  Output defect commands
!
!  Vacancies
!
        nvac = 0
        do i = 1,nldef
          if (ndeftyp(ndefst+i).lt.10.and.ndeftyp(ndefst+i).gt.0) then
            nvac = nvac + 1
          endif
        enddo
        if (nvac.gt.0) then
          do i = 1,nldef
            if (ndeftyp(ndefst+i).lt.10) then
              ndt = ndeftyp(ndefst+i)
              if (ndt.eq.1) then
                write(iout,'(''vacancy '',i4)') nint(xdef(ndefst+i))
              elseif (ndt.eq.2) then
                inat = ndefnat(ndefst+i)
                itype = ndeftp(ndefst+i)
                if (inat.gt.2*maxele) then
                  call label(inat-2_i4*maxele,itype,lab1)
                  write(iout,'(''vacancy '',a4)') lab1
                else
                  if (inat.gt.maxele) then
                    lab2 = 'shel'
                  else
                    lab2 = 'core'
                  endif
                  call label(inat,itype,lab1)
                  write(iout,'(''vacancy '',a5,1x,a4)') lab1,lab2
                endif
              elseif (ndt.eq.3) then
                write(iout,'(''vacancy '',3f9.6)') xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
              elseif (ndt.eq.4) then
                write(iout,'(''vacancy cart '',3f9.5)') xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i)
              elseif (ndt.eq.5) then
                write(iout,'(''vacancy molecule '',i4)') nint(xdef(ndefst+i))
              endif
            endif
          enddo
        endif
!
!  List of interstitials
!
        ninst = 0
        do i = 1,nldef
          if (ndeftyp(ndefst+i).ge.20) then
            ninst = ninst + 1
          endif
        enddo
        if (ninst.gt.0) then
          do i = 1,nldef
            ndt = ndeftyp(i+ndefst)
            if (ldeffix(ndefst+i)) then
              if (inddeffix(ndefst+i).eq.0) then
                fixword = 'fix xyz'
              elseif (inddeffix(ndefst+i).eq.1) then
                fixword = 'fix x  '
              elseif (inddeffix(ndefst+i).eq.2) then
                fixword = 'fix y  '
              elseif (inddeffix(ndefst+i).eq.3) then
                fixword = 'fix z  '
              elseif (inddeffix(ndefst+i).eq.4) then
                fixword = 'fix xy '
              elseif (inddeffix(ndefst+i).eq.5) then
                fixword = 'fix xz '
              elseif (inddeffix(ndefst+i).eq.6) then
                fixword = 'fix yz '
              endif
            else
              fixword = '       '
            endif
            if (ndt.ge.20) then
              if (ndt.eq.21) then
                inat = ndefnat(ndefst+i)
                itype = ndeftp(ndefst+i)
                lbre = .false.
                if (inat.gt.3*maxele) then
                  lbre = .true.
                  inat = inat - 3*maxele
                endif
                if (inat.gt.2*maxele) then
                  call label(inat-2_i4*maxele,itype,lab1)
                  write(iout,'(''interstitial '',a5,2x,3f9.6,1x,a7)') &
                    lab1,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                else
                  if (lbre) then
                    if (inat.gt.maxele) then
                      lab2 = 'bshe'
                    else
                      lab2 = 'bcor'
                    endif
                  else
                    if (inat.gt.maxele) then
                      lab2 = 'shel'
                    else
                      lab2 = 'core'
                    endif
                  endif
                  call label(inat,itype,lab1)
                  write(iout,'(''interstitial '',a5,1x,a4,2x,3f9.6,1x,a7)') &
                    lab1,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                endif
              elseif (ndt.eq.22) then
                inat = ndefnat(ndefst+i)
                itype = ndeftp(ndefst+i)
                lbre = .false.
                if (inat.gt.3*maxele) then
                  lbre = .true.
                  inat = inat - 3*maxele
                endif
                if (inat.gt.2*maxele) then
                  call label(inat-2_i4*maxele,itype,lab1)
                  write(iout,'(''interstitial cart '',a5,2x,3f9.5,1x,a7)') &
                    lab1,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                else
                  if (lbre) then
                    if (inat.gt.maxele) then
                      lab2 = 'bshe'
                    else
                      lab2 = 'bcor'
                    endif
                  else
                    if (inat.gt.maxele) then
                      lab2 = 'shel'
                    else
                      lab2 = 'core'
                    endif
                  endif
                  call label(inat,itype,lab1)
                  write(iout,'(''interstitial cart '',a5,1x,a4,2x,3f9.5,1x,a7)') &
                    lab1,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                endif
              elseif (ndt.eq.23) then
                inat = ndefnat(ndefst+i)
                itype = ndeftp(ndefst+i)
                lbre = .false.
                if (inat.gt.3*maxele) then
                  lbre = .true.
                  inat = inat - 3*maxele
                endif
                if (inat.gt.2*maxele) then
                  call label(inat-2_i4*maxele,itype,lab1)
                  inat = nint(xdef(ndefst+i))
                  itype = nint(ydef(ndefst+i))
                  call label(inat,itype,lab3)
                  write(iout,'(''interstitial bond '',a5,2x,a5,1x,a7)') lab1,lab3,fixword
                else
                  if (lbre) then
                    if (inat.gt.maxele) then
                      lab2 = 'bshe'
                    else
                      lab2 = 'bcor'
                    endif
                  else
                    if (inat.gt.maxele) then
                      lab2 = 'shel'
                    else
                      lab2 = 'core'
                    endif
                  endif
                  call label(inat,itype,lab1)
                  inat = nint(xdef(ndefst+i))
                  itype = nint(ydef(ndefst+i))
                  call label(inat,itype,lab3)
                  write(iout,'(''interstitial bond '',a5,1x,a4,2x,a5,1x,a7)') lab1,lab2,lab3,fixword
                endif
              elseif (ndt.eq.24) then
                inat = ndefnat(ndefst+i)
                itype = ndeftp(ndefst+i)
                lbre = .false.
                if (inat.gt.3*maxele) then
                  lbre = .true.
                  inat = inat - 3*maxele
                endif
                if (inat.gt.2*maxele) then
                  call label(inat-2_i4*maxele,itype,lab1)
                  write(iout,'(''interstitial bond '',a5,2x,3f9.6,1x,a7)') &
                    lab1,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                else
                  if (lbre) then
                    if (inat.gt.maxele) then
                      lab2 = 'bshe'
                    else
                      lab2 = 'bcor'
                    endif
                  else
                    if (inat.gt.maxele) then
                      lab2 = 'shel'
                    else
                      lab2 = 'core'
                    endif
                  endif
                  call label(inat,itype,lab1)
                  write(iout,'(''interstitial bond '',a5,1x,a4,2x,3f9.6,1x,a7)') &
                    lab1,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                endif
              elseif (ndt.eq.25) then
                inat = ndefnat(ndefst+i)
                itype = ndeftp(ndefst+i)
                lbre = .false.
                if (inat.gt.3*maxele) then
                  lbre = .true.
                  inat = inat - 3*maxele
                endif
                if (inat.gt.2*maxele) then
                  call label(inat-2_i4*maxele,itype,lab1)
                  write(iout,'(''interstitial bondc '',a5,2x,3f9.5,1x,a7)') &
                    lab1,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                else
                  if (lbre) then
                    if (inat.gt.maxele) then
                      lab2 = 'bshe'
                    else
                      lab2 = 'bcor'
                    endif
                  else
                    if (inat.gt.maxele) then
                      lab2 = 'shel'
                    else
                      lab2 = 'core'
                    endif
                  endif
                  call label(inat,itype,lab1)
                  write(iout,'(''interstitial bond '',a5,1x,a4,2x,3f9.5,1x,a7)') &
                    lab1,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                endif
              endif
            endif
          enddo
        endif
!
!  List of impurities
!
        nimp = 0
        do i = 1,nldef
          if (ndeftyp(ndefst+i).ge.10.and.ndeftyp(ndefst+i).lt.20) then
            nimp = nimp + 1
          endif
        enddo
        if (nimp.gt.0) then
          do i = 1,nldef
            ndt = ndeftyp(ndefst+i)
            if (ldeffix(ndefst+i)) then
              if (inddeffix(ndefst+i).eq.0) then
                fixword = 'fix xyz'
              elseif (inddeffix(ndefst+i).eq.1) then
                fixword = 'fix x  '
              elseif (inddeffix(ndefst+i).eq.2) then
                fixword = 'fix y  '
              elseif (inddeffix(ndefst+i).eq.3) then
                fixword = 'fix z  '
              elseif (inddeffix(ndefst+i).eq.4) then
                fixword = 'fix xy '
              elseif (inddeffix(ndefst+i).eq.5) then
                fixword = 'fix xz '
              elseif (inddeffix(ndefst+i).eq.6) then
                fixword = 'fix yz '
              endif
            else
              fixword = '       '
            endif
            if (ndt.ge.10.and.ndt.lt.20) then
              inat = ndefnat(ndefst+i)
              itype = ndeftp(ndefst+i)
              lbre = .false.
              if (inat.gt.3*maxele) then
                lbre = .true.
                inat = inat - 3*maxele
              endif
              if (inat.gt.2*maxele) then
                call label(inat-2_i4*maxele,itype,lab1)
                if (ndt.eq.11) then
                  write(iout,'(''impurity '',i4,2x,a5,1x,a7)') nint(xdef(ndefst+i)),lab1,fixword
                elseif (ndt.eq.12) then
                  iptr = nint(xdef(ndefst+i))
                  inat = nat(iptr)
                  itype = nftype(iptr)
                  call label(inat,itype,lab3)
                  write(iout,'(''impurity '',a5,2x,a5,1x,a7)') lab1,lab3,fixword
                elseif (ndt.eq.13) then
                  write(iout,'(''impurity '',a5,2x,3f9.6,1x,a7)') lab1,xdef(ndefst+i),ydef(ndefst+i), &
                    zdef(ndefst+i),fixword
                elseif (ndt.eq.14) then
                  write(iout,'(''impurity cart '',a5,2x,3f9.5,1x,a7)') lab1,xdef(ndefst+i),ydef(ndefst+i), &
                    zdef(ndefst+i),fixword
                endif
              else
                if (lbre) then
                  if (inat.gt.maxele) then
                    lab2 = 'bshe'
                  else
                    lab2 = 'bcor'
                  endif
                else
                  if (inat.gt.maxele) then
                    lab2 = 'shel'
                  else
                    lab2 = 'core'
                  endif
                endif
                call label(inat,itype,lab1)
                if (ndt.eq.11) then
                  write(iout,'(''impurity '',i4,2x,a5,1x,a4,1x,a7)') nint(xdef(ndefst+i)),lab1,lab2,fixword
                elseif (ndt.eq.12) then
                  iptr = nint(xdef(ndefst+i))
                  inat = nat(iptr)
                  itype = nftype(iptr)
                  call label(inat,itype,lab3)
                  write(iout,'(''impurity '',a5,1x,a4,2x,a5,1x,a7)') lab1,lab2,lab3,fixword
                elseif (ndt.eq.13) then
                  write(iout,'(''impurity '',a5,1x,a4,2x,3f9.6,1x,a7)') &
                    lab1,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                elseif (ndt.eq.14) then
                  write(iout,'(''impurity cart '',a5,1x,a4,2x,3f9.5,1x,a7)') &
                    lab1,lab2,xdef(ndefst+i),ydef(ndefst+i),zdef(ndefst+i),fixword
                endif
              endif
            endif
          enddo
        endif
      endif
    endif
    npotpt0 = npotpt0 + npotptcfg(nc)
!
!  Genetic algorithm boundary values
!
    if (ndimen(nc).eq.2) then
      if (xmaxcfg(3,nc).ne.1.0_dp.or.xmincfg(3,nc).ne.0.0_dp) then
        write(iout,'(''genetic'')')
        if (xmaxcfg(3,nc).ne.1.0_dp) then
          write(iout,'(''dmaximum '',f12.6)') xmaxcfg(3,nc)
        endif
        if (xmincfg(3,nc).ne.0.0_dp) then
          write(iout,'(''dminimum '',f12.6)') xmincfg(3,nc)
        endif
        write(iout,'(''end'')')
      endif
    elseif (ndimen(nc).eq.1) then
      if (xmaxcfg(2,nc).ne.1.0_dp.or.xmaxcfg(3,nc).ne.1.0_dp.or. &
          xmincfg(2,nc).ne.0.0_dp.or.xmincfg(3,nc).ne.0.0_dp) then
        write(iout,'(''genetic'')')
        if (xmaxcfg(2,nc).ne.1.0_dp.or.xmaxcfg(3,nc).ne.1.0_dp) then
          write(iout,'(''dmaximum '',f12.6,1x,f12.6)') xmaxcfg(2,nc),xmaxcfg(3,nc)
        endif
        if (xmincfg(2,nc).ne.0.0_dp.or.xmincfg(3,nc).ne.0.0_dp) then
          write(iout,'(''dminimum '',f12.6,1x,f12.6)') xmincfg(2,nc),xmincfg(3,nc)
        endif
        write(iout,'(''end'')')
      endif
    elseif (ndimen(nc).eq.0) then
      if (xmaxcfg(1,nc).ne.1.0_dp.or.xmaxcfg(2,nc).ne.1.0_dp.or.xmaxcfg(3,nc).ne.1.0_dp.or. &
          xmincfg(1,nc).ne.0.0_dp.or.xmincfg(2,nc).ne.0.0_dp.or.xmincfg(3,nc).ne.0.0_dp) then
        write(iout,'(''genetic'')')
        if (xmaxcfg(1,nc).ne.1.0_dp.or.xmaxcfg(2,nc).ne.1.0_dp.or.xmaxcfg(3,nc).ne.1.0_dp) then
          write(iout,'(''dmaximum '',3(f12.6,1x))') (xmaxcfg(j,nc),j=1,3)
        endif
        if (xmincfg(1,nc).ne.0.0_dp.or.xmincfg(2,nc).ne.0.0_dp.or.xmincfg(3,nc).ne.0.0_dp) then
          write(iout,'(''dminimum '',3(f12.6,1x))') (xmincfg(j,nc),j=1,3)
        endif
        write(iout,'(''end'')')
      endif
    endif
!******************************
!  End of configuration loop  *
!******************************
  enddo
!***********************************************
!  Dump non-configuration specific parameters  *
!***********************************************
  call dumppot(iout)
  call dumpgen(iout)
  close(iout)
!
  return
  end
