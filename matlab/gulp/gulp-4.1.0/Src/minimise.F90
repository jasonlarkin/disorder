  subroutine minimise(nvari,nmin,xc,fc,gc,hessian,maxhess,ifail,imode,lfreeloc)
!
!  General optimisation routine by either:
!
!  (1) Newton-Raphson / BFGS
!  (2) RFO method
!  (3) Conjugate gradients
!  (4) LM-BFGS
!
!  Freeze option : If lfreeze=.true. then fc on entry represents full
!                  cell energy, while only partial energy is calcd
!                  subsequently. Therefore efreeze is the additive
!                  correction that must be applied to the energy.
!
!  efreeze = energy due to atoms with no derivatives
!  hessian = hessian matrix for RFO or inverse hessian for NR
!  lconj   = if .true. then conjugate gradients to be used
!  lfreeze = if .true. atoms with no derivatives will be excluded
!            from funct evaluations
!  lopprt  = logical controlling whether to print cycle info
!  lts     = if .true. then calculation is a transition state search
!  nmin    = first variable to include in minimisation
!  nvari   = number of variables
!  pvect   = displacement vector
!
!  imode   = 1 => bulk calculation
!  imode   = 2 => defect calculation
!
!  When in defect mode, endgame convergence must use gradient
!  only to achieve force balance and no line search must be
!  performed. In early stages line minimisation should avoid
!  local stationary points.
!
!   6/95 Switching of minimiser after a given number of cycles or
!        when the gradient norm drops below a certain threshold
!        has been added.
!  10/96 Freezing modifcations added to allow smaller second
!        derivative matrix.
!   8/98 Free energy minimisation modifications added
!   3/99 Option to use numerical derivatives added
!   4/01 Trap for unavailability of analytical derivatives added
!  12/01 Local scaling of gdcrit based on number of variables added
!   3/03 Checks on all criteria now enforced, not just one
!   6/03 XML modifications added
!   6/03 LM-BFGS added
!   9/03 Maximum gradient now check at initial entry as well as Gnorm
!  10/03 ltry option modified to help in case where rfo fails
!  11/03 xtol criterion dropped from convergence tests
!   3/06 Numerical free energy derivatives enabled
!   3/07 linmin renamed to olinmin
!  12/07 Option to control arcfile output frequency added
!  12/07 Unused variables removed
!   3/08 Handling of lunit case modified for ReaxFF
!   4/08 Behaviour modified for lm-bfgs. If the line search fails then
!        reset process and start again.
!   4/08 Fast return added for case where there are no variables
!   5/08 Local minimisation variables introducted to ensure switch
!        acts the same with multiple calls.
!   2/09 maxhess added as an argument
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!   2/09 Old xml calls removed
!   3/09 Use of lfinitediff replaces testing of finite difference value
!  12/09 Call to mcsrch3 modified to pass defect flag
!   1/11 Print out for Cycle line modified for force minimisation case
!   9/11 Modified to allow for larger time values
!  11/11 Threshold for changing the format of time printing corrected
!  10/12 Steepest descents now used when Hessian is reset rather than 
!        diagonal Hessian.
!  10/12 Control C handling added
!  10/12 Control C handling modified so that loop does not exit during
!        relax fitting.
!  11/12 Unused argument removed from call to lmbfgssub
!   3/13 Call to csignal wrapped with ifdef to make call optional
!   5/13 Control C trap for optimisation and fitting separated
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
!  Julian Gale, NRI, Curtin University, May 2013
!
  use control
  use current
  use defects
  use dump
  use energies,    only : efreeze, fcsave
  use files
  use general
  use gulp_cml,    only : lcml, gulp_cml_add_minimise_step
  use interupt,    only : controlC_opt, sigint
  use iochannels
  use optimisation
  use parallel
  use xcgc,        only : lnudgegc
#ifdef ACCELRYS
  use license
#endif

  implicit none
!
!  Passed variables
!
  integer(i4),          intent(out)           :: ifail
  integer(i4),          intent(in)            :: imode
  integer(i4),          intent(in)            :: nmin
  integer(i4),          intent(in)            :: nvari
  integer(i4),          intent(in)            :: maxhess
  logical,              intent(in)            :: lfreeloc
  real(dp),             intent(inout)         :: fc
  real(dp),             intent(inout)         :: gc(nvari)
  real(dp),             intent(inout)         :: hessian(maxhess)
  real(dp),             intent(inout)         :: xc(nvari)
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: i80
  integer(i4)                                 :: ichcrit
  integer(i4)                                 :: ii
  integer(i4)                                 :: iflag
  integer(i4)                                 :: ihdim
  integer(i4)                                 :: info
  integer(i4)                                 :: irepet
  integer(i4)                                 :: itry1
  integer(i4)                                 :: j
  integer(i4)                                 :: jcyc
  integer(i4)                                 :: jexact
  integer(i4)                                 :: jnrst
  integer(i4)                                 :: k
  integer(i4)                                 :: lnstop
  integer(i4)                                 :: maxnvari
  integer(i4)                                 :: ncount
  integer(i4)                                 :: nlvar
  integer(i4)                                 :: ntest
  integer(i4)                                 :: nupdatesave
  integer(i4)                                 :: status
  logical                                     :: dfp
  logical                                     :: lbfgs
  logical                                     :: lconjloc
  logical                                     :: ldefectloc
  logical                                     :: ldiag
  logical                                     :: lfrst
  logical                                     :: lfok
  logical                                     :: lfollow
  logical                                     :: lgmaxok
  logical                                     :: lgok
  logical                                     :: llbfgsloc
  logical                                     :: loffridge
  logical                                     :: loriginal
  logical                                     :: lrfoloc
  logical                                     :: lsaveconj
  logical                                     :: lsavelbfgs
  logical                                     :: lsavediag
  logical                                     :: lsaveorig
  logical                                     :: lsaverfo
  logical                                     :: lsaveunit
  logical                                     :: lswitch
  logical                                     :: lts
  logical                                     :: ltry
  logical                                     :: lverb
  logical                                     :: lxok
  logical                                     :: okf
  real(dp)                                    :: absmin
  real(dp)                                    :: alp
  real(dp)                                    :: area
  real(dp)                                    :: bet
  real(dp)                                    :: bsmvf
  real(dp)                                    :: cncadd
  real(dp)                                    :: cosm
  real(dp)                                    :: cputime
  real(dp)                                    :: ddot
  real(dp)                                    :: dggg
  real(dp)                                    :: dott
  real(dp)                                    :: drop
  real(dp)                                    :: fcin
  real(dp)                                    :: frepf
  real(dp)                                    :: funct1
  real(dp)                                    :: gam
  real(dp)                                    :: gdcritloc
  real(dp), dimension(:),   allocatable       :: gg
  real(dp)                                    :: ggg
  real(dp)                                    :: ggi
  real(dp), dimension(:),   allocatable       :: glast
  real(dp)                                    :: gsca
  real(dp), dimension(:),   allocatable       :: gvar
  real(dp)                                    :: gvari
  real(dp), dimension(:),   allocatable       :: hdiag
  real(dp)                                    :: hh
  real(dp)                                    :: pnlast
  real(dp)                                    :: pnorm
  real(dp), dimension(:),   allocatable       :: pvect
  real(dp)                                    :: rcellaver
  real(dp)                                    :: rhh
  real(dp)                                    :: rootv
  real(dp)                                    :: rst
  real(dp)                                    :: rst2
  real(dp)                                    :: rsy
  real(dp)                                    :: rvh
  real(dp)                                    :: ryhy
  real(dp)                                    :: smval
  real(dp)                                    :: step
  real(dp)                                    :: sy
  real(dp)                                    :: tf
  real(dp)                                    :: time1
  real(dp)                                    :: tolerf
  real(dp)                                    :: tx
  real(dp)                                    :: vh
  real(dp)                                    :: vhi
  real(dp)                                    :: vol
  real(dp)                                    :: volume
  real(dp), dimension(:),   allocatable       :: xlast
  real(dp)                                    :: xn
  real(dp), dimension(:),   allocatable       :: xvar
  real(dp)                                    :: xvari
  real(dp)                                    :: yhy
#ifdef CTRLC
  external                                    :: trap_sigint_opt
!
!  Call sig handler if this not during a relaxed fitting run
!
  if (.not.lrelax) then
    call csignal(sigint,trap_sigint_opt)
  endif
#endif
!
!  Set local number of variables
!
  nlvar = nvari - nmin + 1
  maxnvari = max(nvari,1_i4)
!
!  Allocate local memory
!
  allocate(gg(maxnvari),stat=status)
  if (status/=0) call outofmemory('minimise','gg')
  allocate(glast(maxnvari),stat=status)
  if (status/=0) call outofmemory('minimise','glast')
  allocate(gvar(maxnvari),stat=status)
  if (status/=0) call outofmemory('minimise','gvar')
  allocate(hdiag(maxnvari),stat=status)
  if (status/=0) call outofmemory('minimise','hdiag')
  allocate(pvect(maxnvari),stat=status)
  if (status/=0) call outofmemory('minimise','pvect')
  allocate(xlast(maxnvari),stat=status)
  if (status/=0) call outofmemory('minimise','xlast')
  allocate(xvar(maxnvari),stat=status)
  if (status/=0) call outofmemory('minimise','xvar')
!
!  Initialisation
!
  gsca = 0.001_dp
  rst  = 0.01_dp
  rst2 = 0.06_dp
  ldefectloc = (imode.eq.2)
  lconjloc = lconj
  llbfgsloc = llbfgs
  lrfoloc = lrfo
  lswitch = .false.
  ichcrit = nint(chcrit)
  if (index(keyword,'pmin').ne.0) lopprt = .true.
  if (.not.ioproc) lopprt = .false.
  if (maxline.eq.0) maxline = 3
!
!  Option for DFP optimiser - not default
!
  dfp = (index(keyword,'dfp').ne.0)
  loriginal = (index(keyword,'orig').ne.0)
  ldiag = (loriginal.or.lunit)
  lts = (morder.gt.0)
  lverb = (index(keyword,'verb').ne.0.and.ioproc)
  lbfgs = (index(keyword,' bfgs').ne.0.or.index(keyword,'bfgs').eq.1)
  if (lrfoloc.and.lts.and..not.lbfgs) dfp = .true.
!
!  If no analytical second derivatives, then select unit hessian
!
  if (lnoanald2) lunit = .true.
!
  lfollow = .true.
  loffridge = .true.
!
!  Remaining tolerances
!
  rootv = sqrt(nlvar+1.0d-5)
  tolerf = ftol
  gdcritloc = gdcrit/max(nlvar,1)
!
!  Set to large arbitary value
!
  drop = 1.0d15
  frepf = 1.0d15
!
!  Some final constants
!
  ihdim = (nlvar*(nlvar+1))/2
  cncadd = 1.0_dp/rootv
  if (cncadd.gt.0.15_dp) cncadd = 0.15_dp
!
!  End of initialisation part 1
!
!  Initialise variables
!
  absmin = 1.0d6
  jcyc = 0
  lnstop = 1
  irepet = 1
  alp = 1.0_dp
!
!  Set pnorm initially to 1/(vol**1/3)
!
  if (ndim.eq.3) then
    vol = volume(rv)
    rcellaver = 1.0_dp/vol**(1.0_dp/3.0_dp)
    pnorm = rcellaver
  elseif (ndim.eq.2) then
    vol = area(rv)
    rcellaver = 1.0_dp/sqrt(vol)
    pnorm = rcellaver
  elseif (ndim.eq.1) then
    rcellaver = 1.0_dp/rv(1,1)
    pnorm = rcellaver
  else
    pnorm = nlvar*stepmax
  endif
  pnlast = pnorm
!
!  If transition state search then set stepmax to approx 0.3 Angs
!
  if (imode.eq.1) then
    if (ndim.ge.1) then
      if (lts.and.stepmax.eq.1.0_dp) stepmax = 0.05_dp*rcellaver
    else
      if (lts.and.stepmax.eq.1.0_dp) stepmax = 0.3_dp
    endif
  else
    if (stepmax.eq.1.0_dp) then
      stepmax = 0.3_dp
    endif
  endif
  jexact = 0
  jnrst = 0
  cosm = 0.0_dp
  ncount = 1
!
!  Calculate initial function, gradients and necessary second derivatives
!
  ltry = .false.
  lfrst = .true.
  if (lunit.or.loriginal.or.lconjloc.or.llbfgsloc) then
    iflag = 1
  else
    iflag = 2
  endif
  fcin = fc
  if (imode.eq.1) then
    if (lfreeloc) then
      if (lfinitediff) then
        call fefunctn(iflag,nvari,xc,fc,gc,hessian)
      else
        call fefunct(iflag,nvari,xc,fc,gc,hessian)
      endif
    else
      call funct(iflag,nvari,xc,fc,gc)
    endif
  else
    call deffun(iflag,nvari,xc,fc,gc)
  endif
  gnorm = sqrt(ddot(nlvar,gc(nmin),1_i4,gc(nmin),1_i4))/nlvar
  fcsave = fc
  if (lfreeze) then
    efreeze = fcin - fc
    fc = fc + efreeze
  else
    efreeze = 0.0_dp
  endif
!
!  Write out dump/arcfile before run for checking
!
  if (.not.lrelax.and.ioproc) then
    if (ncycd.eq.1.and.idump.gt.0) call dumpdur(idump,-1_i4)
    if (larc.and.lmovie) then
      call outarc(16_i4,.true.,ldefectloc)
    endif
    if (lxyz.and.lxyzmovie) then
      call outxyz(18_i4,.true.,ldefectloc)
    endif
  endif
!
!  Output initial position
!
  time1 = cputime()
  time1 = time1 - time0
  if (lopprt) then
    if (imode.eq.1) then
      if (lforcemin) then
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.1)') jcyc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.3)') jcyc,gnorm,time1
        endif
      else
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Energy:'',f17.6,''  Gnorm:'',f14.6,''  CPU:'',f9.1)') jcyc,fc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Energy:'',f17.6,''  Gnorm:'',f14.6,''  CPU:'',f9.3)') jcyc,fc,gnorm,time1
        endif
      endif
      if (lcml) then
        call gulp_cml_add_minimise_step(cycle=jcyc, energy=fc, gnorm=gnorm, cputime=time1)
      endif
    else
      if (lforcemin) then
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.1)') jcyc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.3)') jcyc,gnorm,time1
        endif
      else
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Defect Energy: '',f10.6,''  Gnorm:'',f13.6,''  CPU:'',f9.1)') jcyc,fc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Defect Energy: '',f10.6,''  Gnorm:'',f13.6,''  CPU:'',f9.3)') jcyc,fc,gnorm,time1
        endif
      endif
      if (lcml) then
        call gulp_cml_add_minimise_step(cycle=jcyc, energy=fc, gnorm=gnorm, cputime=time1)
      endif
    endif
    call gflush(ioout)
  endif
!
  do i = nmin,nvari
    glast(i) = gc(i)
  enddo
  lgmaxok = .true.
  do i = nmin,nvari
    if (abs(gc(i)).gt.grmax) lgmaxok = .false.
  enddo
  if (nvari.eq.0.or.(gnorm.lt.gtol.and.lgmaxok)) then
!
!  Gradient is already low enough or there are no variables
!
    lgok = .true.
    lfok = .false.
    lxok = .false.
    ifail = 0
    goto 999
  endif
!
!  Initialise vector of hessian diagonal elements just in case
!
  do i = nmin,nvari
    hdiag(i) = 1.0_dp
  enddo
!****************************
!  Start of iteration cycle *
!****************************
10 continue

#ifdef ACCELRYS
  call sendHeartbeat(status)
  if (status /= 0) call gulpfinish
#endif
  jcyc = jcyc + 1
  jnrst = jnrst + 1
  jexact = jexact + 1
  i80 = 0
!
!  Check for switch of optimiser
!
  if (lminch.and..not.lswitch) then
    if (minchcrit.eq.1) then
      if (jcyc.gt.ichcrit) lswitch = .true.
    else
      if (gnorm.le.chcrit) lswitch = .true.
    endif
    if (lswitch) then
      if (lunit) nupdate = 500
      lconjloc = .false.
      ldiag = .false.
      llbfgsloc = .false.
      loriginal = .false.
      lrfoloc = .false.
      lunit = .false.
      if (mintype.eq.2) then
        lrfoloc = .true.
        ldiag = .true.
        lconjloc = .false.
      elseif (mintype.eq.3) then
        lunit = .true.
        ldiag = .true.
        nupdate = 100
      elseif (mintype.eq.4) then
        loriginal = .true.
        ldiag = .true.
      elseif (mintype.eq.5) then
        lconjloc = .true.
      elseif (mintype.eq.6) then
        llbfgsloc = .true.
      endif
    endif
  endif
40 continue
  if (llbfgsloc) then
!************************
!  Limited memory BFGS  *
!************************
    do i = nmin,nvari 
      xlast(i) = xc(i)
      glast(i) = gsca*gc(i)
    enddo
    if (jnrst.eq.1) then
      do i = nmin,nvari
        hdiag(i) = 1.0_dp
      enddo
    endif
    call lmbfgssub(jnrst,nvari,lmbfgsorder,alp,gc,hdiag,hessian,maxhess,pvect)
    pnorm = sqrt(ddot(nlvar,pvect(nmin),1_i4,pvect(nmin),1_i4))
  elseif (lconjloc) then
!************************
!  Conjugate gradients  *
!************************
    if (lfrst.or.mod(jcyc,nupdate).eq.0) then
      do i = nmin,nvari
        xlast(i) = xc(i)
        glast(i) = - gsca*gc(i)
        pvect(i) = glast(i)
      enddo
      lfrst = .false.
    else
      ggg = 0.0_dp
      dggg = 0.0_dp
      do i = nmin,nvari
        ggg = ggg + glast(i)*glast(i)
        dggg = dggg + (glast(i) + gsca*gc(i))*gc(i)*gsca
      enddo
      gam = dggg/ggg
      do i = nmin,nvari
        xlast(i) = xc(i)
        glast(i) = - gsca*gc(i)
        pvect(i) = glast(i) + gam*pvect(i)
      enddo
    endif
    pnorm = sqrt(ddot(nlvar,pvect(nmin),1_i4,pvect(nmin),1_i4))
!
!  Trim pvect back if necessary
!
    if (pnorm.gt.1.5_dp*pnlast) then
      if (lverb) write(ioout,'(''  ** Trimming pvect back'')')
      do i = nmin,nvari
        pvect(i) = pvect(i)*1.5_dp*pnlast/pnorm
      enddo
      pnorm = 1.5_dp*pnlast
    endif
    pnlast = pnorm
  else
!**************************
!  Hessian based methods  *
!**************************
    if (i80.eq.1.or.lnstop.eq.1.or.cosm.le.rst.or.(jnrst.ge.nupdate)) then
!
!  If verbose say why Hessian has been reset
!
      if (lverb.and.jcyc.gt.1) then
        if (jnrst.ge.nupdate) then
          write(ioout,'(''  ** Hess reset due to nupdate'')')
        endif
        if (lnstop.eq.1) then
          write(ioout,'(''  ** Hess reset due to lnstop'')')
        endif
        if (cosm.le.rst) then
          write(ioout,'(''  ** Hess reset due to cos < rst'')')
          write(ioout,'(''  ** Cos = '',f10.6)')cosm
        endif
        if (cosm.le.cncadd.and.drop.gt.1.0_dp) then
          write(ioout,'(''  ** Hess reset due to cncadd and drop'')')
        endif
        if (lunit) then
          write(ioout,'(''  ** Unit Hessian being set'')')
        endif
      endif
!***************************
!  Generate exact hessian  *
!***************************
      if ((.not.lfrst.or.lswitch).and..not.lunit.and..not.loriginal) then
        iflag = 2
        if (imode.eq.1) then
          if (lfreeloc) then
            if (lfinitediff) then
              call fefunctn(iflag,nvari,xc,funct1,gc,hessian)
            else
              call fefunct(iflag,nvari,xc,funct1,gc,hessian)
            endif
          else
            call funct(iflag,nvari,xc,funct1,gc)
          endif
        else
          call deffun(iflag,nvari,xc,funct1,gc)
        endif
        gnorm = sqrt(ddot(nlvar,gc(nmin),1_i4,gc(nmin),1_i4))/nlvar
      endif
      lfrst = .false.
      do i = 1,ihdim
        hessian(i) = 0.0_dp
      enddo
!
!  Current minimisation approach has failed, so try an alternative
!
      if (ltry) then
        lsaveconj  = lconjloc
        lsavediag  = ldiag
        lsavelbfgs = llbfgsloc
        lsaveorig  = loriginal
        lsaverfo   = lrfoloc
        lsaveunit  = lunit
        nupdatesave = nupdate
        if (lrfoloc) then
          lrfoloc = .false.
        endif
      endif
!
!  Set Hessian
!
      if (lunit) then
        ii = 0
        do i = 1,nlvar
          ii = ii + i
          hessian(ii) = 0.001_dp
        enddo
        if (ltry) lunit = .false.
      elseif (loriginal) then
        if (lfreeze) fc = fc - efreeze
        call numhess(nvari,nmin,xc,fc,gc,hessian,xvar,gvar)
        if (lfreeze) fc = fc + efreeze
      else
        if (imode.eq.1) then
          if (ndim.ge.1) then
            if (lfreeze) then
              call sec3f
            else
              call sec3
            endif
          else
            if (lfreeze) then
              call sec0f
            else
              call sec0
            endif
          endif
        else
          call defsec
        endif
        if (lrfoloc) then
          call rfohess(hessian,nvari,nmin)
        else
          call nrhess(hessian,hdiag,nvari,nmin,ldiag)
          if (lopprt) write(ioout,'(''  ** Hessian calculated **'')')
        endif
        jexact = 0
      endif
      ncount = ncount + 1
      jnrst = 0
    else
!
!  Create vectors for change in position and gradients
!
      do i = nmin,nvari
        xvar(i) = xc(i) - xlast(i)
        gvar(i) = gc(i) - glast(i)
      enddo
!****************************
!  Update previous Hessian  *
!****************************
      if (lrfoloc) then
!
!  Update Hessian for RFO method
!
        call nrstep(gg,hessian,xvar,nvari,nmin)
        k = 0
        if (dfp) then
!
!  Update Hessian according to DFP
!
          do i = nmin,nvari
            gvar(i) = gvar(i) - gg(i)
          enddo
          hh = ddot(nlvar,xvar(nmin),1_i4,xvar(nmin),1_i4)
          hh = max(hh,1.0d-8)
          rhh = 1.0_dp/hh
          do i = nmin,nvari
            vhi = gvar(i)*xvar(i)
            do j = 1,i
              k = k + 1
              hessian(k) = hessian(k) + rhh*(vhi + gvar(j)*xvar(j) - rhh*vhi*gvar(j)*xvar(j))
            enddo
          enddo      
        else
!
!  Update Hessian according to BFGS
!
          vh = ddot(nlvar,xvar(nmin),1_i4,gvar(nmin),1_i4)
          hh = ddot(nlvar,xvar(nmin),1_i4,gg(nmin),1_i4)
          if (abs(vh).lt.1.0d-8) vh = 1.0d-8
          if (abs(hh).lt.1.0d-8) hh = 1.0d-8
          rvh = 1.0_dp/vh
          rhh = 1.0_dp/hh
          do i = nmin,nvari
            gvari = gvar(i)*rvh
            ggi = gg(i)*rhh
            do j = 1,i
              k = k + 1
              hessian(k) = hessian(k) + gvar(j)*gvari - gg(j)*ggi
            enddo
          enddo
        endif
      else
!
!  Update inverse of Hessian for Newton-Raphson
!
        call nrstep(gg,hessian,gvar,nvari,nmin)
        if (lposidef) then
!
!  Assign sign of steps to ensure that function is always minimised
!  by NR step, otherwise maximisation of some modes can occur.
!
          do i = nmin,nvari
            gg(i) = sign(gg(i),gc(i))
          enddo
        endif
        yhy = ddot(nlvar,gg(nmin),1_i4,gvar(nmin),1_i4)
        sy = ddot(nlvar,xvar(nmin),1_i4,gvar(nmin),1_i4)
        if (abs(sy).lt.1.0d-8) sy = 1.0d-8
        k = 0
!
!  Update inverse Hessian according to Davidon-Fletcher-Powell
!
        if (dfp) then
          if (abs(yhy).lt.1.0d-8) yhy=1.0d-8
          rsy = 1.0_dp/sy
          ryhy = 1.0_dp/yhy
          do i = nmin,nvari
            xvari = xvar(i)*rsy
            ggi = gg(i)*ryhy
            do j = nmin,i
              k = k + 1
              hessian(k) = hessian(k) + xvar(j)*xvari - gg(j)*ggi
            enddo
          enddo
!
!  Update inverse Hessian according to BFGS
!
        else
          rsy = 1.0_dp/sy
          yhy = 1.0_dp + yhy*rsy
          do i = nmin,nvari
            xvari = xvar(i)*rsy
            ggi = gg(i)*rsy
            do j = nmin,i
              k = k + 1
              hessian(k) = hessian(k) - gg(j)*xvari - xvar(j)*ggi + yhy*xvar(j)*xvari
            enddo
          enddo
        endif
      endif
    endif
!
!  Establish new search direction
!
    if (lnudgegc) then
      call nudge(nvari,xc,pvect)
    endif
    pnlast = pnorm
    if (lrfoloc) then
      call rfostep(pvect,hessian,xc,fc,gc,gg,cosm,pnlast,nvari,nmin,jnrst,lfollow,loffridge,imode)
      dott = ddot(nlvar,pvect(nmin),1_i4,gc(nmin),1_i4)
      pnorm = sqrt(ddot(nlvar,pvect(nmin),1_i4,pvect(nmin),1_i4))
    else
      call nrstep(pvect,hessian,gc,nvari,nmin)
      dott = - ddot(nlvar,pvect(nmin),1_i4,gc(nmin),1_i4)
      do i = nmin,nvari
        pvect(i) = - pvect(i)
      enddo
      pnorm = sqrt(ddot(nlvar,pvect(nmin),1_i4,pvect(nmin),1_i4))
      if (pnorm.gt.1.0d-12.and.gnorm.gt.1.0d-12) then
        cosm = - dott/(pnorm*nlvar*gnorm)
      else
        cosm = 0.0_dp
      endif
      if (cosm.lt.rst2.and.jnrst.eq.0.and..not.ldiag) then
!
!  Angle between pvect and steepest descent path is too great
!  substitute diagonal hessian elements
!
        do i = 1,nlvar
          if (abs(hdiag(i)).lt.1.0d-8) hdiag(i) = 1.0d-8
          hdiag(i) = 1.0_dp/hdiag(i)
        enddo
        do i = 1,ihdim
          hessian(i) = 0.0_dp
        enddo
        ii = 0
        do i = 1,nlvar
          ii = ii + i
          hessian(ii) = hdiag(i)
        enddo
!
!  Initial set to steepest descents since this is more robust
!
        do i = nmin,nvari
          pvect(i) = gc(i)
        enddo
        pnorm = sqrt(ddot(nlvar,pvect(nmin),1_i4,pvect(nmin),1_i4))
        dott = - ddot(nlvar,pvect(nmin),1_i4,gc(nmin),1_i4)
        do i = nmin,nvari
          pvect(i) = - pvect(i)
        enddo
        cosm = - dott/(pnorm*nlvar*gnorm)
      endif
    endif
!
!  Trim pvect back if necessary
!
    if (pnorm.gt.1.5_dp*pnlast) then
      if (lverb) write(ioout,'(''  ** Trimming pvect back'')')
      do i = nmin,nvari
        pvect(i) = pvect(i)*1.5_dp*pnlast/pnorm
      enddo
      pnorm = 1.5_dp*pnlast
    endif
    if (lverb) then
      write(ioout,'(''  ** Cosine = '',f10.6)') cosm
    endif
    if (jnrst.eq.0) goto 190
    if (cosm.le.cncadd.and.drop.gt.1.0_dp) goto 170
    if (cosm.le.rst) goto 170
    goto 190
170   continue
    pnorm = pnlast
    i80 = 1
    goto 40
190 continue
    lnstop = 0
    alp = alp*pnlast/pnorm
    call dcopy(nlvar,gc(nmin),1_i4,glast(nmin),1_i4)
    call dcopy(nlvar,xc(nmin),1_i4,xlast(nmin),1_i4)
    if (jnrst.eq.0) alp = 1.0_dp
    drop = abs(alp*dott)
  endif
  bet = alp
  smval = fc
  okf = .false.
  if (lts.or.lfreeloc.or.(imode.eq.2.and.gnorm.lt.gdcritloc)) then
!
!  Transition state / free energy or nearly converged defect calc
!
    if (gnorm.lt.gdcritloc) then
      if (lfreeloc) then
        step = 1.0_dp
      else
        step = 0.8_dp
      endif
    elseif (nupdate.eq.1) then
      step = 0.5_dp
    elseif (gnorm.lt.5.0*gdcritloc) then
      step = 0.5_dp
    else
      step = 0.2_dp
    endif
!
!  Check for stepmax
!
    step = min(step,stepmax)
!        smax=1.0d-15
!        do i=nmin,nvari
!          if (abs(pvect(i)).gt.smax) smax=abs(pvect(i))
!        enddo
!        if (smax*step.gt.stepmax) then
!          step=step*stepmax/smax
!        endif
    call daxpy(nlvar,step,pvect(nmin),1_i4,xc(nmin),1_i4)
    okf = .true.
    alp = 1.0_dp
    loffridge = .true.
  else
!**********************
!  Line minimisation  *
!**********************
    if (lfreeze) fc = fc - efreeze
    if (llbfgsloc) then
      info = 0
      call mcsrch3(nvari,xc,fc,gc,pvect,alp,info,hdiag,ldefectloc)
      okf = (info.eq.1)
!
!  If mcsrch3 has thrown up an error then reset process and start again
!
      if (.not.okf.and.info.gt.1) then
        jnrst = 0
        okf = .true.
      endif
    else
      call olinmin(xc,alp,pvect,nvari,nmin,fc,okf,gg,imode)
    endif
    if (okf) then
      loffridge = .true.
    else
      fc = smval
      alp = bet
      call dcopy(nlvar,glast(nmin),1_i4,gc(nmin),1_i4)
      call dcopy(nlvar,xlast(nmin),1_i4,xc(nmin),1_i4)
    endif
  endif
  iflag = 1
  if (imode.eq.1) then
    if (lfreeloc) then
      if (lfinitediff) then
        call fefunctn(iflag,nvari,xc,fc,gc,hessian)
      else
        call fefunct(iflag,nvari,xc,fc,gc,hessian)
      endif
    else
      call funct(iflag,nvari,xc,fc,gc)
    endif
  else
    call deffun(iflag,nvari,xc,fc,gc)
  endif
  gnorm = sqrt(ddot(nlvar,gc(nmin),1_i4,gc(nmin),1_i4))/nlvar
  if (lfreeze) fc = fc + efreeze
  ncount = ncount + 1
  if (.not.lconjloc.and..not.llbfgsloc) then
    if (.not.okf) then
      lnstop = 1
      if (jnrst.eq.0.and.ltry) then
        ifail = 3
        goto 999
      else
        if (jexact.eq.0) ltry = .true.
        if (lverb) then
          write(ioout,'(''  ** Trying steepest descent method'')')
        endif
      endif
      cosm = 0.0_dp
      goto 470
    else
      if (ltry) then
!
!  Reset minimisation settings
!
        lconjloc  = lsaveconj
        ldiag     = lsavediag
        llbfgsloc = lsavelbfgs
        loriginal = lsaveorig
        lrfoloc   = lsaverfo
        lunit     = lsaveunit
        nupdate   = nupdatesave
      endif
      ltry = .false.
    endif
  endif
  xn = sqrt(ddot(nlvar,xc(nmin),1_i4,xc(nmin),1_i4))
  tx = abs(alp*pnorm)
  if (xn.ne.0.0_dp) tx = tx/xn
  tf = abs(smval-fc)
  if (abs(absmin-smval).lt.1.d-7) then
    itry1 = itry1 + 1
    if (itry1.gt.10) then
      goto 460
    endif
  else
    itry1 = 0
    absmin = smval
  endif
!
!  Set flags that indicate which criteria have been satisfied
!
  lfok = (tf.le.tolerf)
  lgok = (gnorm.le.gtol)
  lxok = (tx.le.xtol)
  lgmaxok = .true.
  do i = nmin,nvari
    if (abs(gc(i)).gt.grmax) lgmaxok = .false.
  enddo
!
!  Verbose print out of criteria that are satisfied
!
  if (lverb) then
    if (lfok) then
      write(ioout,'(''  **** Ftol satisfied in optimiser ****'')')
    endif
    if (lgmaxok) then
      write(ioout,'(''  **** Gmax satisfied in optimiser ****'')')
    endif
    if (lgok) then
      write(ioout,'(''  **** Gtol satisfied in optimiser ****'')')
    endif
    if (lxok) then
      write(ioout,'(''  **** Xtol satisfied in optimiser ****'')')
    endif
  endif
!
!  Overall check on whether all criteria are satisfied
!
!  Check on xtol removed since this can cause problems if the line
!  minimiser is efficient because it is basis on a change for the
!  last cycle rather than a prediction for the next one. 
!
  if (lfok.and.lgok.and.lgmaxok) then
    ifail = 0
    goto 400
  endif
  goto 470
400 do i = nmin,nvari
    if (abs(gc(i)).gt.grmax) then
      irepet = irepet + 1
      if (irepet.gt.1) goto 410
      frepf = fc
      if (jexact.eq.0.and..not.lrfoloc) ltry = .true.
410     ifail = 3
      if (abs(fc-frepf).gt.ftol) irepet = 0
      if (irepet.gt.maxline) then
        ifail = 3
        goto 999
      else
        goto 470
      endif
    endif
  enddo
  ifail = 0
460 continue
  goto 999
!
!  All tests have failed, we need to do another cycle.
!
470 continue
  bsmvf = abs(smval-fc)
  if (bsmvf.gt.delfc) then
    cosm = 0.0_dp
    if (lverb) write(ioout,'(''  ** Cos = 0.0 due to delfc test'')')
  endif
!
!  End of iteration loop, everything is still O.K. so go to
!  next iteration, if there is enough time left.
!
  time1 = cputime()
  time1 = time1 - time0
  fcsave = fc
  if (lopprt) then
    if (imode.eq.1) then
      if (lforcemin) then
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.1)') jcyc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.3)') jcyc,gnorm,time1
        endif
      else
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Energy:'',f17.6,''  Gnorm:'',f14.6,''  CPU:'',f9.1)') jcyc,fc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Energy:'',f17.6,''  Gnorm:'',f14.6,''  CPU:'',f9.3)') jcyc,fc,gnorm,time1
        endif
      endif
      if (lcml) then
        call gulp_cml_add_minimise_step(cycle=jcyc, energy=fc, gnorm=gnorm, cputime=time1)
      endif
    else
      if (lforcemin) then
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.1)') jcyc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Gradient norm: '',f17.6,''  CPU:'',f9.3)') jcyc,gnorm,time1
        endif
      else
        if (time1.ge.1.0d5) then
          write(ioout,'(''  Cycle: '',i6,'' Defect Energy: '',f10.6,''  Gnorm:'',f13.6,''  CPU:'',f9.1)') jcyc,fc,gnorm,time1
        else
          write(ioout,'(''  Cycle: '',i6,'' Defect Energy: '',f10.6,''  Gnorm:'',f13.6,''  CPU:'',f9.3)') jcyc,fc,gnorm,time1
        endif
      endif
      if (lcml) then
        call gulp_cml_add_minimise_step(cycle=jcyc, energy=fc, gnorm=gnorm, cputime=time1)
      endif
    endif
    call gflush(ioout)
  endif
!
!  Write out intermediate dumpfile if necessary
!
  if (.not.lrelax.and.ioproc) then
    ntest = jcyc/ncycd
    ntest = jcyc - ntest*ncycd
    if (ntest.eq.0.and.idump.gt.0) then
      call dumpdur(idump,jcyc)
    endif
    if (larc.and.lmovie) then
      if (mod(jcyc,narcwrite).eq.0) then
        call outarc(16_i4,.true.,ldefectloc)
      endif
    endif
    if (lxyz.and.lxyzmovie) then
      call outxyz(18_i4,.true.,ldefectloc)
    endif
  endif
!
!  Check time
!
  if (iflag.ge.0.and.jcyc.lt.maxcal.and.(.not.controlC_opt.or.lrelax)) goto 10
  if (iflag.lt.0) then
    ifail = - 1
  else
    ifail = 2
  endif
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(xvar,stat=status)
  if (status/=0) call deallocate_error('minimise','xvar')
  deallocate(xlast,stat=status)
  if (status/=0) call deallocate_error('minimise','xlast')
  deallocate(pvect,stat=status)
  if (status/=0) call deallocate_error('minimise','pvect')
  deallocate(hdiag,stat=status)
  if (status/=0) call deallocate_error('minimise','hdiag')
  deallocate(gvar,stat=status)
  if (status/=0) call deallocate_error('minimise','gvar')
  deallocate(glast,stat=status)
  if (status/=0) call deallocate_error('minimise','glast')
  deallocate(gg,stat=status)
  if (status/=0) call deallocate_error('minimise','gg')
!
  return
  end
