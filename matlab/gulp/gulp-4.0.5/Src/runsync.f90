  subroutine runsync
!
!  This subroutine performs a synchronous transit calculation 
!  in order to locate a transition state. Uses LM-BFGS to perform
!  minimisations.
!
!  11/06 Created from runneb
!   3/07 Flag for funct now passed as variable to avoid error
!  12/07 Initialisation of fnormlast added
!  12/07 Declaration of rvloc corrected to be (3,3)
!  12/07 Unused variables removed
!   5/08 lgeometryOK added as argument to xctox0
!   7/08 Generation of arc file movie added
!  12/08 Parameters that control optimisation moved to synchro module
!   2/09 Hessian dimension passed to lmbfgssub
!   4/12 Check that fixed atom coordinates match between initial and
!        final structures added
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
  use current
  use files,        only : arcfile, larc, lmovie
  use iochannels
  use neb
  use synchro,      only : maxsynciter, maxsyncstep, synctol, lfixtangent
  use parallel
  implicit none
!
!  Local variables
!
  character(len=87)                               :: arcfilel
  integer(i4)                                     :: i
  integer(i4)                                     :: iflag
  integer(i4)                                     :: inddot
  integer(i4)                                     :: indend
  integer(i4)                                     :: info
  integer(i4)                                     :: ii
  integer(i4)                                     :: ix
  integer(i4)                                     :: iy
  integer(i4)                                     :: iz
  integer(i4)                                     :: maxhess
  integer(i4)                                     :: mvar
  integer(i4)                                     :: n
  integer(i4)                                     :: nn
  integer(i4)                                     :: nstart
  integer(i4)                                     :: nstep
  integer(i4)                                     :: nhigher
  integer(i4)                                     :: nlower
  integer(i4)                                     :: ninsertstart
  integer(i4)                                     :: niter
  integer(i4)                                     :: niterlocal
  integer(i4)                                     :: nreadin
  integer(i4)                                     :: nrep
  integer(i4)                                     :: nrp
  integer(i4), dimension(:),    allocatable       :: nrepptr
  integer(i4)                                     :: nsince
  integer(i4)                                     :: status
  logical                                         :: lconverged
  logical                                         :: lgeometryOK
  logical,     dimension(:),    allocatable       :: lopttmp
  logical                                         :: lstepconverged
  logical                                         :: lfound
  real(dp),   dimension(:),     allocatable       :: fcrep
  real(dp),   dimension(:,:),   allocatable       :: gcrep
  real(dp),   dimension(:),     allocatable       :: pcrep
  real(dp),   dimension(:),     allocatable       :: diag
  real(dp),   dimension(:),     allocatable       :: hessian
  real(dp),   dimension(:),     allocatable       :: wa
  real(dp),   dimension(:),     allocatable       :: tangent
  real(dp),   dimension(:,:),   allocatable       :: xcrep
  real(dp),   dimension(:),     allocatable       :: xcoldrep
  real(dp)                                        :: diff
  real(dp)                                        :: fctot
  real(dp)                                        :: fnorm
  real(dp)                                        :: fnormlast
  real(dp)                                        :: rmsd
  real(dp)                                        :: stepmaxdisp
  real(dp)                                        :: steprmsd
  real(dp)                                        :: rvloc(3,3)
  real(dp)                                        :: stepsize
  real(dp)                                        :: stp
  real(dp)                                        :: tnorm
  real(dp)                                        :: xstr(6)
  real(dp)                                        :: xdiff
  real(dp)                                        :: xd
  real(dp)                                        :: yd
  real(dp)                                        :: zd
!***********************
!  Set up of replicas  *
!***********************
!
!  Check to see if replicas have been read in from input deck
!
  nreadin = 0
  do i = 1,nnebreplicatot
    if (nebreplicacfgptr(i).eq.ncf) then
      nreadin = nreadin + 1
    endif
  enddo
!
!  Does this match the number of replicas for this configuration?
!
  if (nnebreplica(ncf).eq.0) nnebreplica(ncf) = nreadin
!
!  Check that number of replicas is 2 since this is all that is allowed for this method
!
  if (nnebreplica(ncf).ne.2) then
    call outerror('number of replicas must be 2 for synchronous transit',0_i4)
    call stopnow('runsync')
  endif
!
!  Check that number of replicas readin is either 2 or 0
!
  if (nreadin.ne.2.and.nreadin.ne.0) then
    call outerror('number of replicas input must be 0 or 2 for synchronous transit',0_i4)
    call stopnow('runsync')
  endif
  if (nreadin.eq.0) then
!
!  Check fractional coordinate rounding is handled
!
    if (ndim.eq.3) then
      do i = 1,nasym
        xd = nebfinalxyz(1,i,ncf) - xafrac(i)
        yd = nebfinalxyz(2,i,ncf) - yafrac(i)
        zd = nebfinalxyz(3,i,ncf) - zafrac(i)
        if (xd.lt.-0.5_dp) xd = xd + 1.0_dp
        if (xd.gt.0.5_dp) xd = xd - 1.0_dp
        if (yd.lt.-0.5_dp) yd = yd + 1.0_dp
        if (yd.gt.0.5_dp) yd = yd - 1.0_dp
        if (zd.lt.-0.5_dp) zd = zd + 1.0_dp
        if (zd.gt.0.5_dp) zd = zd - 1.0_dp
        nebfinalxyz(1,i,ncf) = xd + xafrac(i)
        nebfinalxyz(2,i,ncf) = yd + yafrac(i)
        nebfinalxyz(3,i,ncf) = zd + zafrac(i)
      enddo
    elseif (ndim.eq.2) then
      do i = 1,nasym
        xd = nebfinalxyz(1,i,ncf) - xafrac(i)
        yd = nebfinalxyz(2,i,ncf) - yafrac(i)
        if (xd.lt.-0.5_dp) xd = xd + 1.0_dp
        if (xd.gt.0.5_dp) xd = xd - 1.0_dp
        if (yd.lt.-0.5_dp) yd = yd + 1.0_dp
        if (yd.gt.0.5_dp) yd = yd - 1.0_dp
        nebfinalxyz(1,i,ncf) = xd + xafrac(i)
        nebfinalxyz(2,i,ncf) = yd + yafrac(i)
      enddo
    elseif (ndim.eq.1) then
      do i = 1,nasym
        xd = nebfinalxyz(1,i,ncf) - xafrac(i)
        if (xd.lt.-0.5_dp) xd = xd + 1.0_dp
        if (xd.gt.0.5_dp) xd = xd - 1.0_dp
        nebfinalxyz(1,i,ncf) = xd + xafrac(i)
      enddo
    endif
  endif
!  
!  Create space in arrays for new replicas 
!  
  if (nnebreplicatot+nnebreplica(ncf)+2-nreadin.gt.maxnebreplicatot) then
    maxnebreplicatot = nnebreplicatot + nnebreplica(ncf) + 2 - nreadin
    call changemaxnebreplicatot
  endif
!
!  Find position for replicas
!
  ninsertstart = 0
  nstart = 0
  lfound = .false.
  do while (.not.lfound.and.ninsertstart.lt.nnebreplicatot)
    ninsertstart = ninsertstart + 1
    lfound = (nebreplicacfgptr(ninsertstart).gt.ncf)
    if (nebreplicacfgptr(ninsertstart).lt.ncf) nstart = nstart + 1
  enddo
  if (lfound) then
!
!  Move data to make space for replicas
!
    do i = nnebreplicatot,ninsertstart,-1
      nebreplicacfgptr(nnebreplica(ncf)+i) = nebreplicacfgptr(i)
    enddo
  endif
!
!  Add replica information 
!
  nnebreplicatot = nnebreplicatot + nnebreplica(ncf) - nreadin
  do i = 1,nnebreplica(ncf)-nreadin
    nebreplicacfgptr(ninsertstart+i) = ncf
  enddo
!
  if (nreadin.eq.0) then
!
!  Copy data to replica arrays as initial guess
!
    ii = nstart + 1
    if (ndim.eq.3) then
      nebreplicacell(1,ii) = a
      nebreplicacell(2,ii) = b
      nebreplicacell(3,ii) = c
      nebreplicacell(4,ii) = alpha
      nebreplicacell(5,ii) = beta
      nebreplicacell(6,ii) = gamma
    elseif (ndim.eq.2) then
      nebreplicacell(1,ii) = a
      nebreplicacell(2,ii) = b
      nebreplicacell(3,ii) = alpha
    elseif (ndim.eq.1) then
      nebreplicacell(1,ii) = a
    endif
!
!  Coordinates
!
    if (ndim.gt.0) then
      do i = 1,nasym
        nebreplicaxyz(1,i,ii) = xafrac(i)
        nebreplicaxyz(2,i,ii) = yafrac(i)
        nebreplicaxyz(3,i,ii) = zafrac(i)
      enddo
    else
      do i = 1,nasym
        nebreplicaxyz(1,i,ii) = xalat(i)
        nebreplicaxyz(2,i,ii) = yalat(i)
        nebreplicaxyz(3,i,ii) = zalat(i)
      enddo
    endif
!
!  Radii
!
    do i = 1,nasym
      nebreplicaradius(i,ii) = rada(i)
    enddo
!
    ii = nstart + 2
    if (ndim.eq.3) then
      nebreplicacell(1,ii) = nebfinalcell(1,ncf)
      nebreplicacell(2,ii) = nebfinalcell(2,ncf)
      nebreplicacell(3,ii) = nebfinalcell(3,ncf)
      nebreplicacell(4,ii) = nebfinalcell(4,ncf)
      nebreplicacell(5,ii) = nebfinalcell(5,ncf)
      nebreplicacell(6,ii) = nebfinalcell(6,ncf)
    elseif (ndim.eq.2) then
      nebreplicacell(1,ii) = nebfinalcell(1,ncf)
      nebreplicacell(2,ii) = nebfinalcell(2,ncf)
      nebreplicacell(3,ii) = nebfinalcell(3,ncf)
    elseif (ndim.eq.1) then
      nebreplicacell(1,ii) = nebfinalcell(1,ncf)
    endif
!     
!  Coordinates
!       
    do i = 1,nasym
      nebreplicaxyz(1,i,ii) = nebfinalxyz(1,i,ncf)
      nebreplicaxyz(2,i,ii) = nebfinalxyz(2,i,ncf)
      nebreplicaxyz(3,i,ii) = nebfinalxyz(3,i,ncf)
    enddo
!         
!  Radii    
!             
    do i = 1,nasym
      nebreplicaradius(i,ii) = nebfinalradius(i,ncf)
    enddo
  endif
!
  if (larc.and.lmovie) then
!
!  Create a name for the file
!
    arcfilel(1:80) = arcfile(1:80)
    inddot = index(arcfilel,'.')
    indend = index(arcfilel,' ')
    if (indend.eq.0) then
      arcfilel(72:80) = '_sync.arc'
    else
      if (inddot.eq.0) then
        arcfilel(indend:indend+8) = '_sync.arc'
      else
        indend = inddot
        arcfilel(indend:indend+8) = '_sync.arc'
      endif
    endif
!
!  Perform initialisation of arcfile
!
    call initarc(32_i4,arcfilel)
  endif
!**************************
!  Allocate local memory  *
!**************************
  nrep = nnebreplica(ncf) 
  allocate(nrepptr(nrep),stat=status)
  if (status/=0) call outofmemory('runsync','nrepptr')
  allocate(fcrep(nrep),stat=status)
  if (status/=0) call outofmemory('runsync','fcrep')
  allocate(gcrep(nvar,nrep),stat=status)
  if (status/=0) call outofmemory('runsync','gcrep')
  allocate(pcrep(nvar),stat=status)
  if (status/=0) call outofmemory('runsync','pcrep')
  allocate(xcrep(nvar,nrep),stat=status)
  if (status/=0) call outofmemory('runsync','xcrep')
  allocate(xcoldrep(nvar),stat=status)
  if (status/=0) call outofmemory('runsync','xcoldrep')
  allocate(tangent(nvar),stat=status)
  if (status/=0) call outofmemory('runsync','tangent')
  maxhess = 11*nvar + 10
  allocate(hessian(maxhess),stat=status)
  if (status/=0) call outofmemory('runsync','hessian')
  allocate(diag(nvar),stat=status)
  if (status/=0) call outofmemory('runsync','diag')
  allocate(wa(nvar),stat=status)
  if (status/=0) call outofmemory('runsync','wa')
!*************************************
!  Set up of minimisation variables  *
!*************************************
!   
!  Transfer coordinates to x0
!        
  do i = 1,nstrains
    x0(i) = 1.0_dp
  enddo
  do i = 1,nasym 
    x0(3*i+nstrains-2) = xafrac(i)
    x0(3*i+nstrains-1) = yafrac(i)
    x0(3*i+nstrains)   = zafrac(i)
  enddo
!
!  Check that fixed variables are aligned between initial and final structures
!
  allocate(lopttmp(3*nasym),stat=status)
  if (status/=0) call outofmemory('runsync','lopttmp')
  lopttmp(1:3*nasym) = .false.
  do i = 1,nvar
    lopttmp(iopt(i)-nstrains) = .true.
  enddo
  do i = 1,nasym
    iz = 3*i
    iy = iz - 1
    ix = iz - 2
    if (.not.lopttmp(ix)) then
      diff = abs(x0(nstrains+ix) - nebfinalxyz(1,i,ncf))
      if (diff.gt.1.0d-4) then
        call outerror('fixed variables must be the same for initial and final structures',0_i4)
        call stopnow('runsync')
      endif
    endif
    if (.not.lopttmp(iy)) then
      diff = abs(x0(nstrains+iy) - nebfinalxyz(2,i,ncf))
      if (diff.gt.1.0d-4) then
        call outerror('fixed variables must be the same for initial and final structures',0_i4)
        call stopnow('runsync')
      endif
    endif
    if (.not.lopttmp(iz)) then
      diff = abs(x0(nstrains+iz) - nebfinalxyz(3,i,ncf))
      if (diff.gt.1.0d-4) then
        call outerror('fixed variables must be the same for initial and final structures',0_i4)
        call stopnow('runsync')
      endif
    endif
  enddo
  deallocate(lopttmp,stat=status)
  if (status/=0) call deallocate_error('runsync','lopttmp')
!
!  Set up xc for all configurations
!
  mvar = nstrains + 3*nasym
  nrp = 0
  do ii = 1,nnebreplicatot
    if (nebreplicacfgptr(ii).eq.ncf) then
      nrp = nrp + 1
      nrepptr(nrp) = ii
!
!  Replica
!
      do n = 1,nvar
        if (iopt(n).gt.mvar) then
          i = iopt(n) - mvar
          xcrep(n,nrp) = nebreplicaradius(i,ii) 
        elseif (iopt(n).gt.nstrains) then
          nn = iopt(n) - nstrains
          i = (nn - 1)/3 + 1
          ix = nn - 3*(i - 1)
          xcrep(n,nrp) = nebreplicaxyz(ix,i,ii) 
          nebreplicaxyz(ix,i,ii) = xcrep(n,nrp)
        else
          xcrep(n,nrp) = 1.0_dp 
        endif
      enddo
    endif
  enddo
!**************************************
!  Output initial configuration data  *
!**************************************
  if (ioproc) call outsyncin
  niter = 0
!
!  Calculate initial forces on replicas
!
  nlower = 1
  nhigher = 2
  iflag = 1
  call functsync(iflag,niter,nrep,nrepptr,nvar,xcrep,fcrep,gcrep,tangent,fctot,nlower,nhigher)
  nlower = 2
  nhigher = 1
  iflag = 1
  call functsync(iflag,niter,nrep,nrepptr,nvar,xcrep,fcrep,gcrep,tangent,fctot,nlower,nhigher)
!
!  Calculate initial force norm
!
  if (fcrep(nlower).lt.fcrep(nhigher)) then
    fnormlast = 0.0_dp
    do n = 1,nvar
      fnormlast = fnormlast + gcrep(n,nlower)*gcrep(n,nlower)
    enddo
  else
    fnormlast = 0.0_dp
    do n = 1,nvar
      fnormlast = fnormlast + gcrep(n,nhigher)*gcrep(n,nhigher)
    enddo
  endif
!
!  Compute initial difference in structures
!
  steprmsd = 0.0_dp
  stepmaxdisp = 0.0_dp
  do n = 1,nvar
    diff = abs(xcrep(n,nlower) - xcrep(n,nhigher))
    steprmsd = steprmsd + diff**2
    stepmaxdisp = max(stepmaxdisp,diff)
  enddo
  steprmsd = steprmsd/dble(nvar)
!*****************************************************
!  Iterative loop to search for Minimum Energy Path  *
!*****************************************************
  nstep = 0
  lstepconverged = .false.
  do while (.not.lstepconverged.and.nstep.lt.maxsyncstep)
    nstep = nstep + 1
!
!  Choose configuration to move
!
    if (fcrep(1).lt.fcrep(2)) then
      nlower = 1
      nhigher = 2
    else
      nlower = 2
      nhigher = 1
    endif
    if (ioproc) then
      write(ioout,'(/,''  Step: '',i6,'' : Elow = '',f11.5,'' Ehigh = '',f11.5,'' MaxD = '',f11.5,/)')  &
            nstep,fcrep(nlower),fcrep(nhigher),stepmaxdisp
    endif
!
!  Move lower configuration in direction of higher one
!
    if (stepmaxdisp.lt.1.0d-5) then
      stepsize = 0.4_dp
    elseif (stepmaxdisp.lt.1.0d-3) then
      stepsize = 0.2_dp
    elseif (stepmaxdisp.lt.1.0d-1) then
      stepsize = 0.1_dp
    else
      stepsize = 0.05_dp
    endif
    do n = 1,nvar
      xcrep(n,nlower) = (1.0_dp - stepsize)*xcrep(n,nlower) + stepsize*xcrep(n,nhigher)
    enddo
!***************************************************************
!  Iterative loop to minimise current lowest energy structure  *
!***************************************************************
    nsince = 0
    niter  = 0
    niterlocal = 0
    lconverged = .false.
    stp = 1.0_dp
!
!  Calculate force norm
!
    fnorm = 0.0_dp
    do n = 1,nvar
      fnorm = fnorm + gcrep(n,nlower)*gcrep(n,nlower)
    enddo
    fnorm = sqrt(fnorm)/nvar
    rmsd = 0.0_dp
!
!  Output initial state
!
    if (ioproc) then
      write(ioout,'(''  Iteration = '',i6,'' Force norm = '',f20.6,'' RMSD = '',f12.8)') niter,fnorm,rmsd
    endif
!
!  If the fixed tangent option is being used then compute the tangent for this step
!
    if (lfixtangent) then
!
!  Calculate tangent
!
      tnorm = 0.0_dp
      do i = 1,nvar
        xdiff = xcrep(i,nhigher) - xcrep(i,nlower)
        tangent(i) = xdiff
        tnorm = tnorm + xdiff*xdiff
      enddo
!
!  Normalise tangent
!
      if (tnorm.gt.1.0d-12) then
        tnorm = 1.0_dp/sqrt(tnorm)
      else
        tnorm = 0.0_dp
      endif
      do i = 1,nvar
        tangent(i) = tnorm*tangent(i)
      enddo
    endif
!
    do n = 1,nvar
      diag(n) = 1.0_dp
    enddo
    do while (.not.lconverged.and.niter.lt.maxsynciter)
      niter = niter + 1
      niterlocal = niterlocal + 1
!
!  Copy xcrep to xcoldrep
!
      do n = 1,nvar
        xcoldrep(n) = xcrep(n,nlower)
      enddo
!
!  Call LM-BFGS main routine
!
      call lmbfgssub(niterlocal,nvar,5_i4,stp,xcrep(1,nlower),gcrep(1,nlower),diag,hessian,maxhess,pcrep)
!
!  Call line search routine
!
      call mcsrch3sync(nvar,xcrep,fctot,gcrep,pcrep,stp,info,wa,niter,nrep,nrepptr,nvar,tangent,fcrep,nlower,nhigher)
      if (info.eq.4.or.info.eq.6) then
        niterlocal = 0
        do n = 1,nvar
          diag(n) = 1.0_dp
        enddo
      endif
!
!  Calculate forces on replicas
!
      iflag = 1
      call functsync(iflag,niter,nrep,nrepptr,nvar,xcrep,fcrep,gcrep,tangent,fctot,nlower,nhigher)
!
!  Calculate force norm and rmsd
!
      fnorm = 0.0_dp
      rmsd   = 0.0_dp
      do n = 1,nvar
        fnorm = fnorm + gcrep(n,nlower)*gcrep(n,nlower)
        rmsd  = rmsd  + (xcrep(n,nlower) - xcoldrep(n))**2
      enddo
      fnorm = sqrt(fnorm)/dble(nvar)
      rmsd  = sqrt(rmsd)/dble(nvar)
      if (abs(fnorm - fnormlast).lt.1.d-6.and.niterlocal.gt.1.and.info.ne.1) then
        niterlocal = 0
        do n = 1,nvar
          diag(n) = 1.0_dp
        enddo
      endif
!
!  Output current state
!
      if (ioproc) then
        write(ioout,'(''  Iteration = '',i6,'' Force norm = '',f20.6,'' RMSD = '',f12.8)') niter,fnorm,rmsd
      endif
!
!  Check whether fnorm is the same as the last step
!
      if (abs(fnormlast-fnorm).lt.1.0d-6) then
        nsince = nsince + 1
      else
        nsince = 0
      endif
      fnormlast = fnorm
!
!  Convergence test
!
      lconverged = (fnorm.le.synctol.or.nsince.gt.3)
!*********************************************
!  End of loop for minimising configuration  *
!*********************************************
    enddo
!
!  Output whether run converged or not
!
    if (ioproc) then
      if (lconverged) then
        if (nsince.gt.3) then
          write(ioout,'(/,''  **** Optimisation stopped as force is not dropping ****'')')
        else
          write(ioout,'(/,''  **** Optimisation achieved ****'')')
        endif
      else
        if (niter.ge.maxsynciter) then
          write(ioout,'(/,''  **** Optimisation failed to converge - too many iterations ****'')')
        else
          write(ioout,'(/,''  **** Optimisation failed to converge ****'')')
        endif
      endif
    endif
!************************
!  End of pathway loop  *
!************************
!
!  Output archive frame if required
!
    if (larc.and.lmovie) call addframe2arc(32_i4,fcrep(nlower),.false.,nstep)
!
!  Perform convergence check on step by looking at difference between bounds
!
    steprmsd = 0.0_dp
    stepmaxdisp = 0.0_dp
    do n = 1,nvar
      diff = abs(xcrep(n,nlower) - xcrep(n,nhigher))
      steprmsd = steprmsd + diff**2
      stepmaxdisp = max(stepmaxdisp,diff)
    enddo
    steprmsd = steprmsd/dble(nvar)
    lstepconverged = (steprmsd.lt.1.0d-4.and.stepmaxdisp.lt.1.0d-4)
  enddo
!*************************************
!  Return final data to main arrays  *
!*************************************
  nrp = 0
  do ii = 1,nnebreplicatot
    if (nebreplicacfgptr(ii).eq.ncf) then
      nrp = nrp + 1
      xstr(1:6) = 0.0_dp
      do n = 1,nvar
        if (iopt(n).gt.mvar) then
          i = iopt(n) - mvar
          nebreplicaradius(i,ii) = xcrep(n,nrp)
        elseif (iopt(n).gt.nstrains) then
          nn = iopt(n) - nstrains
          i = (nn - 1)/3 + 1
          ix = nn - 3*(i - 1)
          nebreplicaxyz(ix,i,ii) = xcrep(n,nrp)
        else
          xstr(iopt(n)) = xcrep(n,nrp) - 1.0_dp
        endif
      enddo
      rvloc = 0.0_dp
      if (ndim.eq.3) then
        call cell3D(rvloc,nebreplicacell(1,ii),nebreplicacell(2,ii),nebreplicacell(3,ii), &
          nebreplicacell(4,ii),nebreplicacell(5,ii),nebreplicacell(6,ii))
        call strain3D(xstr,rvloc)
        call uncell3D(rvloc,nebreplicacell(1,ii),nebreplicacell(2,ii),nebreplicacell(3,ii), &
          nebreplicacell(4,ii),nebreplicacell(5,ii),nebreplicacell(6,ii))
      elseif (ndim.eq.2) then
        call cell2D(rvloc,nebreplicacell(1,ii),nebreplicacell(2,ii),nebreplicacell(3,ii))
        call strain2D(xstr,rvloc)
        call uncell2D(rvloc,nebreplicacell(1,ii),nebreplicacell(2,ii),nebreplicacell(3,ii))
      elseif (ndim.eq.1) then
        call cell1D(rvloc,nebreplicacell(1,ii))
        call strain1D(xstr,rvloc)
        call uncell1D(rvloc,nebreplicacell(1,ii))
      endif
    endif
  enddo
!
!  Place original structure back in main arrays for restart file
!
! NB - THIS NEEDS MODIFYING SINCE WE DO NOT HAVE ORIGINAL STRUCTURE IN XCREP
  call xctox0(nvar,xcrep(1,1),lgeometryOK)
  call x0tostr
!**********************
!  Output of results  *
!**********************
  nrep = nnebreplica(ncf) 
  if (ioproc) call outsync(nrep,fcrep,gcrep)
!****************************
!  Deallocate local memory  *
!****************************
  deallocate(wa,stat=status)
  if (status/=0) call deallocate_error('runsync','wa')
  deallocate(diag,stat=status)
  if (status/=0) call deallocate_error('runsync','diag')
  deallocate(hessian,stat=status)
  if (status/=0) call deallocate_error('runsync','hessian')
  deallocate(tangent,stat=status)
  if (status/=0) call deallocate_error('runsync','tangent')
  deallocate(xcoldrep,stat=status)
  if (status/=0) call deallocate_error('runsync','xcoldrep')
  deallocate(xcrep,stat=status)
  if (status/=0) call deallocate_error('runsync','xcrep')
  deallocate(pcrep,stat=status)
  if (status/=0) call deallocate_error('runsync','pcrep')
  deallocate(gcrep,stat=status)
  if (status/=0) call deallocate_error('runsync','gcrep')
  deallocate(fcrep,stat=status)
  if (status/=0) call deallocate_error('runsync','fcrep')
  deallocate(nrepptr,stat=status)
  if (status/=0) call deallocate_error('runsync','nrepptr')
!
  return
  end
