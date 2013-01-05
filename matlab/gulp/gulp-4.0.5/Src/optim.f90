  subroutine optim(lmain,lscan,nscanpoint)
!
!  Subroutine for optimisation of crystal structure
!  also functions in single point calculation mode
!  Second derivative option now included
!
!  lmain = is this a main call of optim?
!  lscan = is this one point of a scan?
!  nscanpoint = no. of scanpoint
!
!   6/95 Use of symmetrised second derivatives in proeprty calc added
!   6/95 Modified for use of additive constraints
!   7/96 Correction added for effect of constraints on lopf
!  11/96 Change added to allow transmat to be called when lfreezeok
!        and number of variables is =< maxd2
!  12/96 Dynamical hessian allocation added
!   1/97 Dynamical hessian generalised for non-SG cases
!   2/98 Freezing turned off if symmetry is being used with EEM as
!        this algorithm combination doesn't work.
!   4/98 Freezing turned off altogether for EEM otherwise changes in
!        self energy get missed
!   6/98 Remove call to gaopt (SMW)
!   8/98 Analytical free energy derivative modifications added
!   2/99 Numerical gradients including hessian (SMW)
!   3/99 Option to minimise static energy before free energy added
!   5/99 Check on lfcphon to decide whether transmat should be called
!        added as fitting with phonons can overwrite tmat
!   8/99 Static call to funct added so that properties are correct
!        after a free energy minimisation
!  12/00 Modified to handle 1-D and 2-D cases
!   3/01 Freezing turned off for symmetry adapted optimisation since
!        it is hard to get the scaling of interactions correct for 
!        all cases.
!   8/01 lopf is now set even for non-freezing case since this can
!        still be used to save work
!  10/01 lfreeloc removed since free energy minimisation can be required
!        even when T = 0 due to ZPE
!  10/01 energycfg now scaled for full cell
!   2/02 Extra call to funct added to recalculate the second derivatives
!        when phonon/property is going to be called after an optimisation
!        otherwise acoustic branches can be wrong and xc contents updated
!   5/02 Array hess set to ensure that pointer is valid on calling functn
!   5/02 Order of calls to borncharge and property swapped to preserve
!        contents of dervi for defect calculations
!   6/02 Analytical calculation of second derivatives removed as precursor
!        to numerical first derivatives
!   9/02 Setting of lsymderv2 altered so that a single point calc can use
!        symmetry when no second derivatives are required
!  10/02 lopf initialised for all cases
!  11/02 scan input variables added to control outarc calls
!   6/03 XML modifications added
!   6/03 LM-BFGS modifications added
!   5/04 lmodco option introduced
!   9/04 Freezing turned off if variable charges are being used
!  10/04 Handling of finite difference cleaned up
!  11/04 Setting of inverse cell parameters added
!  12/04 Freezing turned off when bond order models are used 
!   3/06 Numerical free energy derivatives more widely enabled
!   5/06 Calls to outarc and outxyz suppressed during relaxed
!        fitting
!   5/06 Property output for clusters added
!  11/06 lfirst argument added to equpos call
!   3/07 Chemshell changes added
!   3/07 Flag for funct now passed as variable to avoid error
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!   4/08 Misleading comment corrected about writing only shells
!   4/08 Chemshell output now requires that lgradloc be true
!   4/08 lphonloc now not disabled for finite difference case
!   4/08 ChemShell file handling for the lgradloc conditional
!   6/08 Initialisation of numnonsh corrected.
!   8/08 Format of chemshell write adjusted
!   9/08 Logic of output modified for ChemShell to handle loptiloc case
!   2/09 Hessian dimension passed to minimize for checking
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   3/09 Use of lfinitediff replaces testing of finite difference value
!   5/09 Option to generate second derivatives for properties by finite
!        differences added 
!   6/09 Error format updated to standard form
!   6/10 loptsuccess flag added to indicate a successful optimisation
!   7/11 Call to transmat suppressed if not needed as this can cause
!        a large memory overhead.
!  10/11 Correction to allocated memory for case where switch occurs
!        from lmbfgs to a Hessian based method. 
!   4/12 Comments relating to calling cml directly removed
!   5/12 Stress output moved here from property.
!   6/12 Dummy variables added to phonon and deffreq call
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
!  Julian Gale, NRI, Curtin University, June 2012
!  Scott Woodley, R.I.G.B., June 1997
!
  use bondorderdata, only : nbopot, nboQ
  use configurations
  use control
  use current
  use derivatives,   only : xdrv, ydrv, zdrv
  use element,       only : maxele
  use energies,      only : fcsave, fcstore
  use files
  use fitting
  use general
  use genetic,       only : lgacost
  use gulpchemsh
  use gulp_cml,      only : lcml, gulp_cml_StartModule, gulp_cml_EndModule, gulp_cml_structure
  use iochannels
  use optimisation
  use parallel
  use reallocate
  use scan,          only : ntran
  use sutton,        only : lsuttonc
  use symmetry
  use times
  use xcgc

  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: nscanpoint
  logical,     intent(in)                      :: lmain
  logical,     intent(in)                      :: lscan
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ierror
  integer(i4)                                  :: ifail
  integer(i4)                                  :: iflag
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indc
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: mvar
  integer(i4),                            save :: ncflast = 0
  integer(i4), dimension(:), allocatable       :: ncount
  integer(i4)                                  :: nfwords
  integer(i4),                            save :: nhwords = 1
  integer(i4)                                  :: ipunch      !ChemShell
  integer(i4)                                  :: numnonsh    !ChemShell
  integer(i4)                                  :: status
  logical                                      :: lappend
  logical                                      :: ldothis
  logical,                                save :: lfirsttime = .true.
  logical                                      :: lfound
  logical                                      :: lfreezeok
  logical                                      :: lgradloc
  logical                                      :: lgrad2loc
  logical                                      :: llower
  logical                                      :: loptiloc
  logical                                      :: lphonloc
  logical                                      :: lprintsave
  logical                                      :: lproploc
  logical                                      :: lstrold
  logical                                      :: lusenumericd2
  real(dp)                                     :: cost
  real(dp)                                     :: cputime
  real(dp)                                     :: diff
  real(dp)                                     :: fc
  real(dp),    dimension(:), pointer,     save :: hess => null()
  real(dp)                                     :: oldcell(6)
  real(dp)                                     :: pv
  real(dp)                                     :: strloc(6)
  real(dp)                                     :: time1
  real(dp)                                     :: face        !ChemShell
  real(dp)                                     :: facg        !ChemShell
  real(dp)                                     :: xtt         !ChemShell
  real(dp)                                     :: ytt         !ChemShell
  real(dp)                                     :: ztt         !ChemShell
  real(dp)                                     :: rvi(3,3)    !ChemShell
  real(dp)                                     :: rr(6)       !ChemShell
!
!  Nullify Hessian pointer and initialise with basic size
!
  if (lfirsttime) then
    lfirsttime = .false.
    call realloc(hess,nhwords,ierror)
    if (ierror.ne.0) call outofmemory('optim','hess')
  endif
!
  lopprt = lmain
  lfreezeok = (index(keyword,'noex').eq.0.and.ncell.eq.0.and..not.lfree.and. &
    .not.lsymopt.and..not.lsuttonc.and..not.lDoQDeriv1.and..not.lDoQDeriv2 &
    .and..not.lbrenner.and.(nbopot+nboQ.eq.0).and..not.lcosmo.and..not.lreaxFF)
  lgacost = (index(keyword,'cost').ne.0.and.lopt)
  llower = (index(keyword,'lowe').ne.0)
  loptiloc = lopt
  lgradloc = lgrad
  lphonloc = lphon
  lproploc = ((lprop.or.lphon).and.ndim.eq.3)
  loptsuccess = .false.
!
!  Set flag to indicate whether numerical second derivatives should be used for properties
!
  lusenumericd2 = (lproploc.and.lnoanald2.or.index(keyword,'nume').ne.0)
!
!  Set flag to indicate whether second derivatives and therefore tmat will ever be needed
!
  lgrad2loc = (.not.lconj.and..not.llbfgs.and..not.lunit)
  if (lminch.and.mintype.le.2) lgrad2loc = .true.
!
  fc = 0.0_dp
  pv = 0.0_dp
!
!  If relax mode then restore original cell coordinates
!
  if (lrelax) then
    do i = 1,nasym
      xstore(i) = xcfg(nsft+i)
      ystore(i) = ycfg(nsft+i)
      zstore(i) = zcfg(nsft+i)
    enddo
    if (nbsmat.gt.0) then
      do i = 1,nasym
        rstore(i) = radcfg(nsft+i)
      enddo
    endif
  elseif (lcomp) then
!
!  Store structure for comparison afterwards
!
    if (ndim.eq.3) then
      oldcell(1) = a
      oldcell(2) = b
      oldcell(3) = c
      oldcell(4) = alpha
      oldcell(5) = beta
      oldcell(6) = gamma
    elseif (ndim.eq.2) then
      oldcell(1) = a
      oldcell(2) = b
      oldcell(3) = alpha
    elseif (ndim.eq.1) then
      oldcell(1) = a
    endif
    do i = 1,nasym
      xstore(i) = xcfg(nsft+i)
      ystore(i) = ycfg(nsft+i)
      zstore(i) = zcfg(nsft+i)
    enddo
    if (nbsmat.gt.0) then
      do i = 1,nasym
        rstore(i) = radcfg(nsft+i)
      enddo
    endif
  endif
!
!  Transfer cell to x0
!
  if (loptcellpar) then
    if (ndim.eq.3) then
      x0(1) = a
      x0(2) = b
      x0(3) = c
      x0(4) = alpha
      x0(5) = beta
      x0(6) = gamma
    elseif (ndim.eq.2) then
      x0(1) = a
      x0(2) = b
      x0(3) = alpha
    elseif (ndim.eq.1) then
      x0(1) = a
    endif
  endif
!
!  Transfer coordinates to x0
!
  do i = 1,nasym
    x0(3*i+nstrains-2) = xafrac(i)
    x0(3*i+nstrains-1) = yafrac(i)
    x0(3*i+nstrains)   = zafrac(i)
  enddo
!
!  Transfer breathing shell radii
!
  mvar = 3*nasym + nstrains
  if (nbsmat.gt.0) then
    do i = 1,nasym
      x0(mvar+i) = radcfg(nsft+i)
    enddo
  endif
!***********************
!  Set freezing flags  *
!***********************
!
!  Initialise to fixed
!
  do i = 1,nasym
    lopf(i) = .false.
  enddo
  if (lbulknoopt) then
    loptiloc = .false.
  elseif (nvar.gt.0) then
    do i = 1,nvar
      if (iopt(i).gt.nstrains.or.loptcellpar) then
        xc(i) = x0(iopt(i))
      else
        xc(i) = 1.0_dp
      endif
    enddo
    do i = 1,nvar
      ind = iopt(i)
      if (ind.gt.mvar) then
        ind = ind - mvar
        lopf(ind) = .true.
      elseif (ind.gt.nstrains) then
        ind = ind - (nstrains+1)
        ind = (ind/3) + 1
        lopf(ind) = .true.
      endif
!
!  Check for constrained atoms
!
      if (ncon.gt.0) then
        do j = 1,ncon
          if (iopt(i).eq.ncvar(j)) then
            indc = ncfix(j)
            if (indc.gt.mvar) then
              indc = indc - mvar
              lopf(indc) = .true.
            elseif (indc.gt.nstrains) then
              indc = indc - (nstrains+1)
              indc = (indc/3) + 1
              lopf(indc) = .true.
            endif
          endif
        enddo
      endif
    enddo
  elseif (nvar.eq.0.and.(lopt.or.lgrad)) then
    nwarn = nwarn + 1
    if (lopt) then
      call outwarning('No variables to optimise - single point performed',0_i4)
    else
      call outwarning('No variables for gradients - only energy calculated',0_i4)
    endif
    loptiloc = .false.
    lgradloc = .false.
  endif
  if ((loptiloc.or.lrelax).and.(.not.lconj.or.lminch)) then
!*********************
!  Allocate hessian  *
!*********************
    if (llbfgs) then
      nhwords = nvar*(2*lmbfgsorder + 1) + 2*lmbfgsorder
!
!  Check if minimiser might change and need more memory
!
      if (lminch.and.mintype.le.4) then
        nhwords = max(nhwords,nvar*(nvar+1)/2)
      endif
    else
      nhwords = nvar*(nvar+1)/2
!
!  For rfo need storage space to avoid using disk
!
      if (lfree) then
        nfwords = 3*numat*(3*numat+1)/2
        nhwords = max(nhwords,nfwords)
      endif
    endif
    call realloc(hess,nhwords,ierror)
    if (ierror.ne.0) call outofmemory('optim','hess')
  endif
  if (lmain) then
    lfirst = .true.
    lfreeze = .false.
    iflag = 0
    if (.not.loptiloc) then
      if (lgradloc.or.lfit) then
        iflag = 1
      endif
      if (lborn.or.lproploc.or.lphon) iflag = 2
      lstr = (ndim.gt.0)
      ifail = 4
    endif
    if (index(keyword,'sing').ne.0.or.nvar.eq.0) then
      ifail = 5
    endif
!********************************
!   Single point calculation    *
!********************************
    if (lfree) then
      lsymderv2 = .false.
      if (iflag.ge.2) then
        nhwords = nvar*(nvar+1)/2
        call realloc(hess,nhwords,ierror)
        if (ierror.ne.0) call outofmemory('optim','hess')
      endif
      if (lfinitediff.and..not.ldefect) then
        call fefunctn(iflag,nvar,xc,fc,gc,hess)
      else
        call fefunct(iflag,nvar,xc,fc,gc,hess)
      endif
    else
!
!  If not doing optimisation turn off symmetrisation of second derivatives
!  for property/phonon evaluation
!
      if (.not.loptiloc.and.iflag.ge.2) lsymderv2 = .false.
      if (lfinitediff.and..not.ldefect) then
        if (iflag.ge.2) then
          nhwords = nvar*(nvar+1)/2
          call realloc(hess,nhwords,ierror)
          if (ierror.ne.0) call outofmemory('optim','hess')
        endif
        call functn(iflag,nvar,xc,fc,gc,hess,1_i4)
      else
        if (lusenumericd2.and.iflag.eq.2) then
          call functnf(iflag,nvar,xc,fc,gc,.true.)
        else
          call funct(iflag,nvar,xc,fc,gc)
        endif
      endif
    endif
    if (ioproc) call outener
    if (.not.lfree.and..not.loptiloc) then
      if ((lprop.or.lborn.or.lphon).and.(lewald.or.lwolf)) then
        call borncharge(.true.)
      endif
      if (lproploc) then
        call property(.true.)
      endif
      if (lprop.and.ndim.eq.0) then
        call property0(.true.)
      endif
      if (lphonloc) then
        if (ndim.gt.0) then
          call phonon(.true.,fc,0_i4,0_i4)
        else
          call deffreq(.true.,fc,2_i4,0_i4,0_i4)
        endif
        if (llower) call setup(.true.)
      endif
      if (lproploc.and.ldefect.and.(lphonloc)) then
!
!  If a defect calculation is to be performed do silent property calculation
!  to restore matrices after phonon calculation
!
        lsymderv2 = .false.
        iflag = 2
        if (lusenumericd2) then
          call functnf(iflag,nvar,xc,fc,gc,.true.)
        else
          call funct(iflag,nvar,xc,fc,gc)
        endif
        call property(.false.)
      endif
    endif
  endif
!**********************************************
!  Initialise arcfile for movie if requested  *
!**********************************************
  fcsave = fc
  if (ioproc.and..not.lrelax) then
    lappend = (lscan.and.nscanpoint.gt.0)
    if (larc.and.lmovie) then
      call outarc(16_i4,lappend,.false.)
    endif
    if (lxyz.and.lxyzmovie) then
      call outxyz(18_i4,lappend,.false.)
    endif
  endif
!********************************
!       Optimisation            *
!********************************
  if (loptiloc.or.lrelax) then
    ifail = 1
    lfreeze = lfreezeok
    if (ioproc) call gflush(ioout)
!
!  Setup transformation matrix if needed
!
    if (ncf.ne.ncflast.and.lgrad2loc) call transmat
!
!  Output optimisation parameters, if main call
!
    if (lmain) call optin
!
!  Start minimisation
!
    if (lstaticfirst.and.lfree) then
!
!  Minimise static energy prior to free energy minimisation
!
      lprintsave = lopprt
      lopprt = .false.
      call minimise(nvar,1_i4,xc,fc,gc,hess,nhwords,ifail,1_i4,.false.)
      lopprt = lprintsave
      loptsuccess = (ifail.eq.0)
!
!  Reset number of frequencies to avoid false warning messages
!  at start of free energy minimisation
!
      nummode = 0
    endif
!
!  Minimise static/free energy
!
    call minimise(nvar,1_i4,xc,fc,gc,hess,nhwords,ifail,1_i4,lfree)
    loptsuccess = (ifail.eq.0)
!
!  Calculate the elastic constants at end of optimisation
!  Need to switch off lopt to enable second deriv calcn.
!
    lfreeze = .false.
    if ((lborn.or.lproploc.or.lphonloc).and.loptiloc) then
      lopt = .false.
      lstrold = lstr
      lstr = (ndim.eq.3)
      iflag = 2
      lsymderv2 = .false.
      if (lfree) then
        if (lfinitediff.and..not.ldefect) then
          call fefunctn(iflag,nvar,xc,fc,gc,hess)
        else
          call fefunct(iflag,nvar,xc,fc,gc,hess)
        endif
      else
        if (lusenumericd2) then
          call functnf(iflag,nvar,xc,fc,gc,.true.)
        else
          call funct(iflag,nvar,xc,fc,gc)
        endif
      endif
      lopt = .true.
      lstr = lstrold
    elseif (lfreezeok) then
      iflag = 1
      if (lfree) then
        if (lfinitediff.and..not.ldefect) then
          call fefunctn(iflag,nvar,xc,fc,gc,hess)
        else
          call fefunct(iflag,nvar,xc,fc,gc,hess)
        endif
      else
        call funct(iflag,nvar,xc,fc,gc)
      endif
    endif
!
!  Substitute parameters into place
!
    if (.not.loptcellpar) then
      do i = 1,nstrains
        x0(i) = 1.0_dp
      enddo
    endif
    do i = 1,nvar
      x0(iopt(i)) = xc(i)
    enddo
!**********************
!  Apply constraints  *
!**********************
    if (ncon.gt.0) then
      do i = 1,ncon
        x0(ncfix(i)) = 0.0_dp
      enddo
      do i = 1,ncon
        x0(ncfix(i)) = x0(ncvar(i))*conco(i) + conadd(i) + x0(ncfix(i))
      enddo
!
!  Handle additive constraints for fractional coordinates
!  - take nearest pair of images
!
      if (ndim.gt.0) then
        allocate(ncount(mvar),stat=status)
        if (status/=0) call outofmemory('optim','ncount')
        do i = 1,mvar
          ncount(i) = 0
        enddo
        do i = 1,ncon
          ii = ncfix(i)
          ncount(ii) = ncount(ii) + 1
        enddo
        do i = nstrains+1,mvar
!
!  Select only those coordinates which are fractional
!
          if (ndim.eq.3) then
            ldothis = .true.
          elseif (ndim.eq.2) then
            ldothis = (mod((i-nstrains),3_i4).ne.0)
          elseif (ndim.eq.1) then
            ldothis = (mod((i-nstrains),3_i4).eq.1)
          endif
          if (ncount(i).ge.2.and.ldothis) then
            lfound = .false.
            j = 0
            do while (.not.lfound.and.j.lt.ncon-1)
              j = j + 1
              if (ncfix(j).eq.i) then
                k = j
                do while (.not.lfound.and.k.lt.ncon) 
                  k = k + 1
                  lfound = (ncfix(k).eq.i)
                enddo
              endif
            enddo
            if (lfound) then
              diff = abs(x0(ncvar(j)) - x0(ncvar(k)))
              if (diff.gt.0.5_dp) then
                x0(i) = x0(i) + 0.5_dp
                x0(i) = mod(x0(i),1.0_dp)
              endif
            endif
          endif
        enddo
        deallocate(ncount,stat=status)
        if (status/=0) call deallocate_error('optim','ncount')
      endif
    endif
!****************************************
!  Return data to configuration arrays  *
!****************************************
    if (ncell.gt.0) then
      if (loptcellpar) then
!
!  Cell parameters
!
        if (ndim.eq.3) then
          a = x0(1)
          b = x0(2)
          c = x0(3)
          alpha = x0(4)
          beta  = x0(5)
          gamma = x0(6)
        elseif (ndim.eq.2) then
          a = x0(1)
          b = x0(2)
          alpha = x0(3)
        elseif (ndim.eq.1) then
          a = x0(1)
        endif
      else
!
!  Strains
!
        do i = 1,3
          rv(1,i) = rvcfg(1,i,ncf)
          rv(2,i) = rvcfg(2,i,ncf)
          rv(3,i) = rvcfg(3,i,ncf)
        enddo
        do i = 1,nstrains
          strloc(i) = x0(i) - 1.0_dp
        enddo
        if (ndim.eq.3) then
          call strain3D(strloc,rv)
          call uncell3D(rv,a,b,c,alpha,beta,gamma)
          if (a.gt.1.0d-12) then
            recipa = 1.0_dp/a
          else
            recipa = 0.0_dp
          endif
          if (b.gt.1.0d-12) then
            recipb = 1.0_dp/b
          else
            recipb = 0.0_dp
          endif
          if (c.gt.1.0d-12) then
            recipc = 1.0_dp/c
          else
            recipc = 0.0_dp
          endif
          if (.not.lrelax) then
            do i = 1,3
              rvcfg(1,i,ncf) = rv(1,i)
              rvcfg(2,i,ncf) = rv(2,i)
              rvcfg(3,i,ncf) = rv(3,i)
            enddo
          endif
        elseif (ndim.eq.2) then
          call strain2D(strloc,rv)
          call uncell2D(rv,a,b,alpha)
          if (a.gt.1.0d-12) then
            recipa = 1.0_dp/a
          else
            recipa = 0.0_dp
          endif
          if (b.gt.1.0d-12) then
            recipb = 1.0_dp/b
          else
            recipb = 0.0_dp
          endif
          if (.not.lrelax) then
            do i = 1,2
              rvcfg(1,i,ncf) = rv(1,i)
              rvcfg(2,i,ncf) = rv(2,i)
              rvcfg(3,i,ncf) = rv(3,i)
            enddo
          endif
        elseif (ndim.eq.1) then
          call strain1D(strloc,rv)
          call uncell1D(rv,a)
          if (a.gt.1.0d-12) then
            recipa = 1.0_dp/a
          else
            recipa = 0.0_dp
          endif
          if (.not.lrelax) then
            rvcfg(1,1,ncf) = rv(1,1)
            rvcfg(2,1,ncf) = rv(2,1)
            rvcfg(3,1,ncf) = rv(3,1)
          endif
        endif
      endif
    endif
!
!  Atomic positions
!
    if (.not.lrelax) then
      if (ndim.ge.1.and.lmodco) then
        do i = 1,nasym
          xcfg(i+nsft) = mod(x0(3*i+(nstrains-2))+1.0_dp,1.0_dp)
        enddo
      else
        do i = 1,nasym
          xcfg(i+nsft) = x0(3*i+(nstrains-2))
        enddo
      endif
      if (ndim.ge.2.and.lmodco) then
        do i = 1,nasym
          ycfg(i+nsft) = mod(x0(3*i+(nstrains-1))+1.0_dp,1.0_dp)
        enddo
      else
        do i = 1,nasym
          ycfg(i+nsft) = x0(3*i+(nstrains-1))
        enddo
      endif
      if (ndim.eq.3.and.lmodco) then
        do i = 1,nasym
          zcfg(i+nsft) = mod(x0(3*i+nstrains)+1.0_dp,1.0_dp)
        enddo
      else
        do i = 1,nasym
          zcfg(i+nsft) = x0(3*i+nstrains)
        enddo
      endif
!
!  Radii
!
      if (nbsmat.gt.0) then
        do i = 1,nasym
          radcfg(i+nsft) = x0(mvar+i)
          rada(i) = x0(mvar+i)
        enddo
      endif
!
!  Copy configuration coordinates back to current arrays
!
      do i = 1,nasym
        xafrac(i) = xcfg(nsft+i)
        yafrac(i) = ycfg(nsft+i)
        zafrac(i) = zcfg(nsft+i)
      enddo
      if (lsymopt) then
        call equpos(.true.,.false.)
      else
        do i = 1,numat
          xfrac(i) = xafrac(i)
          yfrac(i) = yafrac(i)
          zfrac(i) = zafrac(i)
        enddo
      endif
    endif
  endif
!**********************************
!  Output arc file or stop movie  *
!**********************************
  if (ioproc.and..not.lrelax) then
    if (larc) then
      if (lmovie) then
        if (nscanpoint.eq.ntran(ncf)) close(16)
      else
        call outarc(16_i4,.false.,.false.)
        close(16)
      endif
    endif
    if (lxyz) then
      if (lxyzmovie) then
        if (nscanpoint.eq.ntran(ncf)) close(18)
      else
        call outxyz(18_i4,.false.,.false.)
        close(18)
      endif
    endif
  endif
!************************
!  End of optimisation  *
!************************
  ncflast = ncf
  if (lmain) then
! 
! New CML - open finalization module and dump final structure
!
    if (lcml) then
      call gulp_cml_StartModule(title='Finalization') 
      call gulp_cml_structure(ncf)
    endif 
!
    energycfg(ncf) = fc*dble(icentfct(ncbl))
    if (loptiloc.or.lgradloc) then
      call optout(ifail,ncf,fc,pv,gc,oldcell)
      if (lstressout) call outstresses(.true.)
      if (latomicstress) call outatomicstress
    endif
    if ((loptiloc.or.lfree).and.(lborn.or.lproploc.or.lphonloc)) then
!
!  If this was a free energy minimisation then perform a static
!  calculation in order to get the correct properties at the end.
!  It is also important to reset the values of xc so that the
!  correct structure is used for the properties as x0 has been
!  changed to reflect the final structure.
!
      iflag = 2
      do i = 1,nvar
        if (iopt(i).gt.nstrains.or.loptcellpar) then
          xc(i) = x0(iopt(i))
        else
          xc(i) = 1.0_dp
        endif
      enddo
      if (lusenumericd2) then
        call functnf(iflag,nvar,xc,fc,gc,.true.)
      else
        call funct(iflag,nvar,xc,fc,gc)
      endif
    endif
    if (loptiloc.or.lfree) then
      if ((lprop.or.lborn.or.lphon).and.(lewald.or.lwolf)) then
        call borncharge(.true.)
      endif
      if (lproploc) then
!
! New CML - in  this case then the call to property will dump the elastic constants and so on.          
!
        call property(.true.)
      endif
      if (lprop.and.ndim.eq.0) then
        call property0(.true.)
      endif
      if (lphonloc) then
        if (ndim.gt.0) then
          call phonon(.true.,fc,0_i4,0_i4)
        else
          call deffreq(.true.,fc,2_i4,0_i4,0_i4)
        endif
        if (llower) call setup(.true.)
      endif
      if (lproploc.and.ldefect.and.lphonloc) then
!
!  If a defect calculation is to be performed do silent property calculation
!  to restore matrices after phonon calculation
!
        iflag = 2
        if (lusenumericd2) then
          call functnf(iflag,nvar,xc,fc,gc,.true.)
        else
          call funct(iflag,nvar,xc,fc,gc)
        endif
        call property(.false.)
      endif
    endif
    if (lcml) call gulp_cml_EndModule
  endif
!
!  Print out value of cost function if requested keyword 'cost'
!
  if (ioproc.and.index(keyword,'cost').ne.0.and..not.lopt) then
    if (.not.lga.and..not.lpredict) then
      lgacost = .true.
      iflag = 0
      call funct(iflag,nvar,xc,cost,gc)
      write(ioout,'(/,''  The value of the cost function for this initial structure is '',f14.7,/)')cost
      write(ioout,'(/,''--------------------------------------------------------------------------------''/)')
      lgacost = .false.
    endif
  endif
!
!  Timing
!
  time1 = cputime()
  time1 = time1 - time0
  if (ioproc) then
    if (lmain.and.loptiloc) then
      write(ioout,'(/,''  Time to end of optimisation = '',f12.4,'' seconds'',/)')time1
    elseif (lmain.and.(lproploc.or.lphonloc)) then
      write(ioout,'(/,''  Time to end of properties = '',f12.4,'' seconds'',/)')time1
    elseif (lmain.and.lgradloc) then
      write(ioout,'(/,''  Time to end of gradients = '',f12.4,'' seconds'',/)')time1
    endif
  endif
!********************
!  CHEMSHELL_START  *
!********************
  if (ioproc .and. ichemsh_qm.ge.0) then

    ipunch = 7

    open(ipunch,file='gulp.energy',form='formatted')

    face = 0.03674896_dp
    facg = 0.0194467_dp

    write(ipunch,*) "block=matrix records=0"
    write(ipunch,*) "block=matrix_title records=1"
    write(ipunch,*) "Gulp energy"
    write(ipunch,*) "block=dense_real_matrix records=1 dimensions=1 1"

    write(ipunch,100) fcstore*face

    close(unit=ipunch)

    numnonsh = 0
    do i = 1,numat
      if (iatn(i).le.maxele) then
        numnonsh = numnonsh + 1
      endif
    enddo

    if (lgradloc.or.loptiloc) then
      open(ipunch,file='gulp.gradient',form='formatted')

      write(ipunch,*) "block=matrix records=0"
      write(ipunch,*) "block=matrix_title records=1"
      write(ipunch,*) "Gulp gradient"

      write(ipunch,101) 3*numnonsh, 3, numnonsh
!
!  Check that derivatives have been generated; if not, set them to
!  zeroes as the old code would have (GULP 1.3)
!
      if (.not. (associated(xdrv) .and. associated(ydrv) .and. associated(zdrv))) then
        call realloc(xdrv,numat,status)
        if (status.ne.0) call outofmemory('optim','xdrv')
        call realloc(ydrv,numat,status)
        if (status.ne.0) call outofmemory('optim','ydrv')
        call realloc(zdrv,numat,status)
        if (status.ne.0) call outofmemory('optim','zdrv')
        xdrv = 0.0_dp
        ydrv = 0.0_dp
        zdrv = 0.0_dp
      endif
!
!  Write only atoms, not shells
!
      if (ndimen(ncf).eq.3) then
        if (ncbl.gt.1) then
          call outerror('No centred cells',0_i4)
          call stopnow('optim')
        endif
      
        do i = 1,3
          rvi(1,i) = rv(1,i)
          rvi(2,i) = rv(2,i)
          rvi(3,i) = rv(3,i)
        enddo
        ifail = 0
        call matinv(rvi,3_i4,3_i4,rr,ifail)
      
        do i = 1,numat
          if (nat(i).le.maxele) then
            xtt = xdrv(i)*rvi(1,1) + ydrv(i)*rvi(2,1) + zdrv(i)*rvi(3,1)
            ytt = xdrv(i)*rvi(1,2) + ydrv(i)*rvi(2,2) + zdrv(i)*rvi(3,2)
            ztt = xdrv(i)*rvi(1,3) + ydrv(i)*rvi(2,3) + zdrv(i)*rvi(3,3)
            write(ipunch,100) xtt*facg
            write(ipunch,100) ytt*facg
            write(ipunch,100) ztt*facg
          endif
        enddo
      else
        do i = 1,numat
          if (nat(i).le.maxele) then
            write(ipunch,100) xdrv(i)*facg
            write(ipunch,100) ydrv(i)*facg
            write(ipunch,100) zdrv(i)*facg
          endif
        enddo
      endif

      close(unit=ipunch)

    endif

    open(ipunch,file='gulp.relshel',form='formatted')

    write(ipunch,*) "block=matrix records=0"
    write(ipunch,*) "block=matrix_title records=1"
    write(ipunch,*) "Gulp relaxed shell coordinates"

    write(ipunch,101) 3*(nasym-numnonsh), 3, nasym-numnonsh

    do i = 1,numat
      if (iatn(i).gt.maxele) then
        write(ipunch,100) xclat(i)/0.52917706_dp
        write(ipunch,100) yclat(i)/0.52917706_dp
        write(ipunch,100) zclat(i)/0.52917706_dp
      endif
    enddo

    close(unit=ipunch)

  endif ! ChemShell output

100  format(f28.14)
101  format('block=dense_real_matrix records=',i6,' dimensions=',2i6)
!******************
!  CHEMSHELL_END  *
!******************
!
  return
  end
