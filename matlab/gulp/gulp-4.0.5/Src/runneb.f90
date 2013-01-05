  subroutine runneb
!
!  This subroutine performs a Nudged Elastic Band calculation 
!  in order to locate a transition state. Uses LM-BFGS to find
!  solution.
!
!  10/06 Created in LM-BFGS form
!   3/07 Flag for funct now passed as variable to avoid error
!   4/07 Checking for nearest image moved inside of general 
!        interpolation in order to allow for intermediate 
!        configurations.
!   4/07 Declaration of rvloc corrected to be an array
!  12/07 Initialisation of fnormlast added
!  12/07 Unused variables removed
!   1/08 random -> GULP_random
!   5/08 lgeometryOK added as argument to xctox0
!   2/09 Hessian dimension passed to lmbfgs
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
  use genetic,        only : iseed
  use iochannels
  use neb
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4)                                     :: i
  integer(i4)                                     :: iabove
  integer(i4)                                     :: ibelow
  integer(i4)                                     :: iflag
  integer(i4)                                     :: info
  integer(i4)                                     :: itarget
  integer(i4)                                     :: ii
  integer(i4)                                     :: ix
  integer(i4)                                     :: iy
  integer(i4)                                     :: iz
  integer(i4)                                     :: maxhess
  integer(i4)                                     :: mvar
  integer(i4)                                     :: n
  integer(i4)                                     :: nn
  integer(i4)                                     :: nstart
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
  logical                                         :: lfound
  logical                                         :: lgeometryOK
  logical,    dimension(:),     allocatable       :: lopttmp
  logical,    dimension(:),     allocatable       :: lreplicaread
  real(dp),   dimension(:),     allocatable       :: aboveradius
  real(dp),   dimension(:,:),   allocatable       :: abovexyz
  real(dp),   dimension(:),     allocatable       :: belowradius
  real(dp),   dimension(:,:),   allocatable       :: belowxyz
  real(dp),   dimension(:),     allocatable       :: fcrep
  real(dp),   dimension(:,:),   allocatable       :: gcrep
  real(dp),   dimension(:,:),   allocatable       :: pcrep
  real(dp),   dimension(:,:),   allocatable       :: diag
  real(dp),   dimension(:),     allocatable       :: hessian
  real(dp),   dimension(:,:),   allocatable       :: nebreplicacellin
  real(dp),   dimension(:,:),   allocatable       :: nebreplicaradiusin
  real(dp),   dimension(:,:,:), allocatable       :: nebreplicaxyzin
  real(dp),   dimension(:),     allocatable       :: wa
  real(dp),   dimension(:),     allocatable       :: tangent
  real(dp),   dimension(:,:),   allocatable       :: xcrep
  real(dp),   dimension(:,:),   allocatable       :: xcoldrep
  real(dp)                                        :: aabove
  real(dp)                                        :: abelow
  real(dp)                                        :: alphaabove
  real(dp)                                        :: alphabelow
  real(dp)                                        :: babove
  real(dp)                                        :: bbelow
  real(dp)                                        :: betaabove
  real(dp)                                        :: betabelow
  real(dp)                                        :: cabove
  real(dp)                                        :: cbelow
  real(dp)                                        :: diff
  real(dp)                                        :: fctot
  real(dp)                                        :: fnorm
  real(dp)                                        :: fnorm2
  real(dp)                                        :: fnormlast
  real(dp)                                        :: gammaabove
  real(dp)                                        :: gammabelow
  real(dp)                                        :: GULP_random
  real(dp)                                        :: rcell(3)
  real(dp)                                        :: rmsd
  real(dp)                                        :: rvloc(3,3)
  real(dp)                                        :: stp
  real(dp)                                        :: xrand
  real(dp)                                        :: xrep
  real(dp)                                        :: xstr(6)
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
  allocate(lreplicaread(nnebreplica(ncf)),stat=status)
  if (status/=0) call outofmemory('runneb','lreplicaread')
  lreplicaread(1:nnebreplica(ncf)) = .false.
  if (nreadin.gt.0) then
!
!  Copy read in data to workspace arrays to avoid overwriting
!
    allocate(nebreplicacellin(6,nreadin),stat=status)
    if (status/=0) call outofmemory('runneb','nebreplicacellin')
    allocate(nebreplicaradiusin(nasym,nreadin),stat=status)
    if (status/=0) call outofmemory('runneb','nebreplicaradiusin')
    allocate(nebreplicaxyzin(3,nasym,nreadin),stat=status)
    if (status/=0) call outofmemory('runneb','nebreplicaxyzin')
    do i = 1,nreadin
      nebreplicacellin(1:6,i) = nebreplicacell(1:6,nstart+i)
      nebreplicaradiusin(1:nasym,i) = nebreplicaradius(1:nasym,nstart+i)
      nebreplicaxyzin(1:3,1:nasym,i) = nebreplicaxyz(1:3,1:nasym,nstart+i)
    enddo
!
!  Move replica data to appropriate place and set logical whether data has been read in or needs to be generated
!
    do i = 1,nreadin
      lreplicaread(nnebreplicano(nstart+i)) = .true.
      if (nnebreplicano(nstart+i).ne.i) then
        itarget = nstart + nnebreplicano(nstart+i)
        nebreplicacell(1:6,itarget) = nebreplicacellin(1:6,i)
        nebreplicaradius(1:nasym,itarget) = nebreplicaradiusin(1:nasym,i)
        nebreplicaxyz(1:3,1:nasym,itarget) = nebreplicaxyzin(1:3,1:nasym,i)
      endif
    enddo
    deallocate(nebreplicaxyzin,stat=status)
    if (status/=0) call deallocate_error('runneb','nebreplicaxyzin')
    deallocate(nebreplicaradiusin,stat=status)
    if (status/=0) call deallocate_error('runneb','nebreplicaradiusin')
    deallocate(nebreplicacellin,stat=status)
    if (status/=0) call deallocate_error('runneb','nebreplicacellin')
  endif
  if (nreadin.ne.nnebreplica(ncf)) then
!
!  Allocate interpolation arrays
!
    allocate(abovexyz(3,nasym),stat=status)
    if (status/=0) call outofmemory('runneb','abovexyz')
    allocate(aboveradius(nasym),stat=status)
    if (status/=0) call outofmemory('runneb','aboveradius')
    allocate(belowxyz(3,nasym),stat=status)
    if (status/=0) call outofmemory('runneb','belowxyz')
    allocate(belowradius(nasym),stat=status)
    if (status/=0) call outofmemory('runneb','belowradius')
!
!  Set up initial replicas by linear interpolation between nearest predefined states
!
    do nrep = 1,nnebreplica(ncf)
      ii = nrep + nstart
      nnebreplicano(ii) = nrep
      if (.not.lreplicaread(nrep)) then
!
!  Find nearest replica below
!
        lfound = .false.
        ibelow = ii - 1
        do while (.not.lfound.and.ibelow.gt.nstart)
          lfound = (lreplicaread(ibelow-nstart))
          ibelow = ibelow - 1
        enddo
        if (.not.lfound) then
          ibelow = nstart 
        else
          ibelow = ibelow + 1
        endif
!
!  If not found then use the initial structure...
!
        if (.not.lfound) then
          if (ndim.eq.3) then
            abelow = a
            bbelow = b
            cbelow = c
            alphabelow = alpha
            betabelow = beta
            gammabelow = gamma
          elseif (ndim.eq.2) then
            abelow = a
            bbelow = b
            alphabelow = alpha
          elseif (ndim.eq.1) then
            abelow = a
          endif
          if (ndim.gt.0) then
            do i = 1,nasym
              belowxyz(1,i) = xafrac(i)
              belowxyz(2,i) = yafrac(i)
              belowxyz(3,i) = zafrac(i)
            enddo
          else
            do i = 1,nasym
              belowxyz(1,i) = xalat(i)
              belowxyz(2,i) = yalat(i)
              belowxyz(3,i) = zalat(i)
            enddo
          endif
          do i = 1,nasym
            belowradius(i) = rada(i)
          enddo
        else
!
!  ...else use the nearest image below
!
          if (ndim.eq.3) then
            abelow = nebreplicacell(1,ibelow)
            bbelow = nebreplicacell(2,ibelow)
            cbelow = nebreplicacell(3,ibelow)
            alphabelow = nebreplicacell(4,ibelow)
            betabelow = nebreplicacell(5,ibelow)
            gammabelow = nebreplicacell(6,ibelow)
          elseif (ndim.eq.2) then
            abelow = nebreplicacell(1,ibelow)
            bbelow = nebreplicacell(2,ibelow)
            alphabelow = nebreplicacell(3,ibelow)
          elseif (ndim.eq.1) then
            abelow = nebreplicacell(1,ibelow)
          endif
          do i = 1,nasym
            belowxyz(1,i) = nebreplicaxyz(1,i,ibelow)
            belowxyz(2,i) = nebreplicaxyz(2,i,ibelow)
            belowxyz(3,i) = nebreplicaxyz(3,i,ibelow)
          enddo
          do i = 1,nasym
            belowradius(i) = nebreplicaradius(i,ibelow)
          enddo
        endif
!
!  Find nearest replica above
!
        lfound = .false.
        iabove = ii + 1
        do while (.not.lfound.and.iabove.lt.nstart+nnebreplica(ncf))
          lfound = (lreplicaread(iabove-nstart))
          iabove = iabove + 1
        enddo
        if (.not.lfound) then
          iabove = nstart + nnebreplica(ncf) + 1
        else
          iabove = iabove - 1
        endif
!
!  If not found then use the final structure...
!
        if (.not.lfound) then
          if (ndim.eq.3) then
            aabove = nebfinalcell(1,ncf)
            babove = nebfinalcell(2,ncf)
            cabove = nebfinalcell(3,ncf)
            alphaabove = nebfinalcell(4,ncf)
            betaabove = nebfinalcell(5,ncf)
            gammaabove = nebfinalcell(6,ncf)
          elseif (ndim.eq.2) then
            aabove = nebfinalcell(1,ncf)
            babove = nebfinalcell(2,ncf)
            alphaabove = nebfinalcell(3,ncf)
          elseif (ndim.eq.1) then
            aabove = nebfinalcell(1,ncf)
          endif
          do i = 1,nasym
            abovexyz(1,i) = nebfinalxyz(1,i,ncf)
            abovexyz(2,i) = nebfinalxyz(2,i,ncf)
            abovexyz(3,i) = nebfinalxyz(3,i,ncf)
          enddo
          do i = 1,nasym
            aboveradius(i) = nebfinalradius(i,ncf)
          enddo
        else
!
!  ...else use the nearest image above
!
          if (ndim.eq.3) then
            aabove = nebreplicacell(1,iabove)
            babove = nebreplicacell(2,iabove)
            cabove = nebreplicacell(3,iabove)
            alphaabove = nebreplicacell(4,iabove)
            betaabove = nebreplicacell(5,iabove)
            gammaabove = nebreplicacell(6,iabove)
          elseif (ndim.eq.2) then
            aabove = nebreplicacell(1,iabove)
            babove = nebreplicacell(2,iabove)
            alphaabove = nebreplicacell(3,iabove)
          elseif (ndim.eq.1) then
            aabove = nebreplicacell(1,iabove)
          endif
          do i = 1,nasym
            abovexyz(1,i) = nebreplicaxyz(1,i,iabove)
            abovexyz(2,i) = nebreplicaxyz(2,i,iabove)
            abovexyz(3,i) = nebreplicaxyz(3,i,iabove)
          enddo
          do i = 1,nasym
            aboveradius(i) = nebreplicaradius(i,iabove)
          enddo
        endif
!
!  Before interpolating make sure that the images are the nearest ones
!
        if (ndim.eq.3) then
          do i = 1,nasym
            xd = abovexyz(1,i) - belowxyz(1,i)
            yd = abovexyz(2,i) - belowxyz(2,i)
            zd = abovexyz(3,i) - belowxyz(3,i)
            if (xd.lt.-0.5_dp) xd = xd + 1.0_dp
            if (xd.gt.0.5_dp) xd = xd - 1.0_dp
            if (yd.lt.-0.5_dp) yd = yd + 1.0_dp
            if (yd.gt.0.5_dp) yd = yd - 1.0_dp
            if (zd.lt.-0.5_dp) zd = zd + 1.0_dp
            if (zd.gt.0.5_dp) zd = zd - 1.0_dp
            abovexyz(1,i) = xd + belowxyz(1,i)
            abovexyz(2,i) = yd + belowxyz(2,i)
            abovexyz(3,i) = zd + belowxyz(3,i)
          enddo
        elseif (ndim.eq.2) then
          do i = 1,nasym
            xd = abovexyz(1,i) - belowxyz(1,i)
            yd = abovexyz(2,i) - belowxyz(2,i)
            if (xd.lt.-0.5_dp) xd = xd + 1.0_dp
            if (xd.gt.0.5_dp) xd = xd - 1.0_dp
            if (yd.lt.-0.5_dp) yd = yd + 1.0_dp
            if (yd.gt.0.5_dp) yd = yd - 1.0_dp
            abovexyz(1,i) = xd + belowxyz(1,i)
            abovexyz(2,i) = yd + belowxyz(2,i)
          enddo
        elseif (ndim.eq.1) then
          do i = 1,nasym
            xd = abovexyz(1,i) - belowxyz(1,i)
            if (xd.lt.-0.5_dp) xd = xd + 1.0_dp
            if (xd.gt.0.5_dp) xd = xd - 1.0_dp
            abovexyz(1,i) = xd + belowxyz(1,i)
          enddo
        endif
!
!  Compute the mixing fraction
!
        xrep = dble(ii - ibelow)/dble(iabove - ibelow)
!
!  Cell
!
        if (ndim.eq.3) then
          nebreplicacell(1,ii) = (1.0_dp - xrep)*abelow + xrep*aabove
          nebreplicacell(2,ii) = (1.0_dp - xrep)*bbelow + xrep*babove
          nebreplicacell(3,ii) = (1.0_dp - xrep)*cbelow + xrep*cabove
          nebreplicacell(4,ii) = (1.0_dp - xrep)*alphabelow + xrep*alphaabove
          nebreplicacell(5,ii) = (1.0_dp - xrep)*betabelow  + xrep*betaabove
          nebreplicacell(6,ii) = (1.0_dp - xrep)*gammabelow + xrep*gammaabove
        elseif (ndim.eq.2) then
          nebreplicacell(1,ii) = (1.0_dp - xrep)*abelow + xrep*aabove
          nebreplicacell(2,ii) = (1.0_dp - xrep)*bbelow + xrep*babove
          nebreplicacell(3,ii) = (1.0_dp - xrep)*alphabelow + xrep*alphaabove
        elseif (ndim.eq.1) then
          nebreplicacell(1,ii) = (1.0_dp - xrep)*abelow + xrep*aabove
        endif
!
!  Coordinates
!
        do i = 1,nasym
          nebreplicaxyz(1,i,ii) = (1.0_dp - xrep)*belowxyz(1,i) + xrep*abovexyz(1,i) 
          nebreplicaxyz(2,i,ii) = (1.0_dp - xrep)*belowxyz(2,i) + xrep*abovexyz(2,i) 
          nebreplicaxyz(3,i,ii) = (1.0_dp - xrep)*belowxyz(3,i) + xrep*abovexyz(3,i)
        enddo
!
!  Radii
!
        do i = 1,nasym
          nebreplicaradius(i,ii) = (1.0_dp - xrep)*belowradius(i) + xrep*aboveradius(i)
        enddo
      endif
    enddo
!
!  End of interpolation section
!
    deallocate(belowradius,stat=status)
    if (status/=0) call deallocate_error('runneb','belowradius')
    deallocate(belowxyz,stat=status)
    if (status/=0) call deallocate_error('runneb','belowxyz')
    deallocate(aboveradius,stat=status)
    if (status/=0) call deallocate_error('runneb','aboveradius')
    deallocate(abovexyz,stat=status)
    if (status/=0) call deallocate_error('runneb','abovexyz')
  endif
!***********************************************
!  Deallocate memory that is no longer needed  *
!***********************************************
  deallocate(lreplicaread,stat=status)
  if (status/=0) call deallocate_error('runneb','lreplicaread')
!**************************
!  Allocate local memory  *
!**************************
  nrep = nnebreplica(ncf) + 2
  allocate(nrepptr(nrep),stat=status)
  if (status/=0) call outofmemory('runneb','nrepptr')
  allocate(fcrep(nrep),stat=status)
  if (status/=0) call outofmemory('runneb','fcrep')
  allocate(gcrep(nvar,nrep),stat=status)
  if (status/=0) call outofmemory('runneb','gcrep')
  allocate(pcrep(nvar,nrep),stat=status)
  if (status/=0) call outofmemory('runneb','pcrep')
  allocate(xcrep(nvar,nrep),stat=status)
  if (status/=0) call outofmemory('runneb','xcrep')
  allocate(xcoldrep(nvar,nrep),stat=status)
  if (status/=0) call outofmemory('runneb','xcoldrep')
  allocate(tangent(nvar),stat=status)
  if (status/=0) call outofmemory('runneb','tangent')
  maxhess = 11*nrep*nvar+10
  allocate(hessian(maxhess),stat=status)
  if (status/=0) call outofmemory('runneb','hessian')
  allocate(diag(nvar,nrep),stat=status)
  if (status/=0) call outofmemory('runneb','diag')
  allocate(wa(nrep*nvar),stat=status)
  if (status/=0) call outofmemory('runneb','wa')
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
  if (status/=0) call outofmemory('runneb','lopttmp')
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
        call stopnow('runneb')
      endif
    endif
    if (.not.lopttmp(iy)) then
      diff = abs(x0(nstrains+iy) - nebfinalxyz(2,i,ncf))
      if (diff.gt.1.0d-4) then
        call outerror('fixed variables must be the same for initial and final structures',0_i4)
        call stopnow('runneb')
      endif
    endif
    if (.not.lopttmp(iz)) then
      diff = abs(x0(nstrains+iz) - nebfinalxyz(3,i,ncf))
      if (diff.gt.1.0d-4) then
        call outerror('fixed variables must be the same for initial and final structures',0_i4)
        call stopnow('runneb')
      endif
    endif
  enddo
  deallocate(lopttmp,stat=status)
  if (status/=0) call deallocate_error('runneb','lopttmp')
!
!  Set up conversion factors to multiply random displacements
!  and maximum coordinate velocities in Angs/ps
!
  if (ndim.eq.3) then
    rcell(1) = nebrandom/a
    rcell(2) = nebrandom/b
    rcell(3) = nebrandom/c
  elseif (ndim.eq.2) then
    rcell(1) = nebrandom/a
    rcell(2) = nebrandom/b
    rcell(3) = nebrandom
  elseif (ndim.eq.1) then
    rcell(1) = nebrandom/a
    rcell(2) = nebrandom
    rcell(3) = nebrandom
  else
    rcell(1) = nebrandom
    rcell(2) = nebrandom
    rcell(3) = nebrandom
  endif
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
          xcrep(n,nrp+1) = nebreplicaradius(i,ii) 
        elseif (iopt(n).gt.nstrains) then
          nn = iopt(n) - nstrains
          i = (nn - 1)/3 + 1
          ix = nn - 3*(i - 1)
          xrand = rcell(ix)*GULP_random(iseed,2_i4)
          xcrep(n,nrp+1) = nebreplicaxyz(ix,i,ii) + xrand
          nebreplicaxyz(ix,i,ii) = xcrep(n,nrp+1)
        else
          if (iopt(n).le.ndim) then
            xcrep(n,nrp+1) = 1.0_dp + rcell(n)*GULP_random(iseed,2_i4)
          else
            xcrep(n,nrp+1) = 1.0_dp 
          endif
        endif
      enddo
    endif
  enddo
!
!  Initial structure
!
  do n = 1,nvar
    if (iopt(n).gt.mvar) then
      i = iopt(n) - mvar
      xcrep(n,1) = rada(i)
    elseif (iopt(n).gt.nstrains) then
      nn = iopt(n) - nstrains
      i = (nn - 1)/3 + 1
      ix = nn - 3*(i - 1)
      if (ix.eq.1) then
        xcrep(n,1) = xafrac(i)
      elseif (ix.eq.2) then
        xcrep(n,1) = yafrac(i)
      else
        xcrep(n,1) = zafrac(i)
      endif
    else
      xcrep(n,1) = 1.0_dp
    endif
  enddo
!
!  Final structure
!
  do n = 1,nvar
    if (iopt(n).gt.mvar) then
      i = iopt(n) - mvar
      xcrep(n,nrep) = nebfinalradius(i,ncf)
    elseif (iopt(n).gt.nstrains) then
      nn = iopt(n) - nstrains
      i = (nn - 1)/3 + 1
      ix = nn - 3*(i - 1)
      xcrep(n,nrep) = nebfinalxyz(ix,i,ncf)
    else
      xcrep(n,nrep) = 1.0_dp
    endif
  enddo
!**************************************
!  Output initial configuration data  *
!**************************************
  if (ioproc) call outnebin
!*****************************************************
!  Iterative loop to search for Minimum Energy Path  *
!*****************************************************
  niter = 0
  niterlocal = 0
  nsince = 0
  lconverged = .false.
  stp = 1.0_dp
!
!  Calculate initial forces on replicas
!
  iflag = 1
  call functneb(iflag,niter,nrep,nrepptr,nvar,xcrep,fcrep,gcrep,tangent,fctot)
!
!  Calculate force norm
!
  fnorm2 = 0.0_dp
  do nrp = 2,nrep-1
    do n = 1,nvar
      fnorm2 = fnorm2 + gcrep(n,nrp)*gcrep(n,nrp)
    enddo
  enddo
  fnorm = sqrt(fnorm2)/dble((nrep - 2)*nvar)
  fnormlast = fnorm
  rmsd = 0.0_dp
!
!  Output initial state
!
  if (ioproc) then
    write(ioout,'(/,''  Start of LM-BFGS optimisation: '',/)')
    write(ioout,'(''  Iteration = '',i6,'' Force norm = '',f20.6,'' RMSD = '',f12.8)') niter,fnorm,rmsd
  endif
  do nrp = 1,nrep
    do n = 1,nvar
      diag(n,nrp) = 1.0_dp
    enddo
  enddo
  do while (.not.lconverged.and.niter.lt.nnebiter)
    niter = niter + 1
    niterlocal = niterlocal + 1
!
!  Copy xcrep to xcoldrep
!
    do nrp = 1,nrep
      do n = 1,nvar
        xcoldrep(n,nrp) = xcrep(n,nrp)
      enddo
    enddo
!
!  Call LM-BFGS main routine
!
    call lmbfgssub(niterlocal,nvar*nrep,5_i4,stp,xcrep,gcrep,diag,hessian,maxhess,pcrep)
!
!  Call line search routine
!
    call mcsrch3neb(nvar*nrep,xcrep,fctot,gcrep,pcrep,stp,info,wa,niter,nrep,nrepptr,nvar,tangent,fcrep)
    if (info.eq.4.or.info.eq.6) then
      niterlocal = 0
      do nrp = 1,nrep
        do n = 1,nvar
          diag(n,nrp) = 1.0d0
        enddo
      enddo
    endif
!
!  Calculate forces on replicas
!
    iflag = 1
    call functneb(iflag,niter,nrep,nrepptr,nvar,xcrep,fcrep,gcrep,tangent,fctot)
!
!  Calculate force norm and rmsd
!
    fnorm2 = 0.0_dp
    rmsd   = 0.0_dp
    do nrp = 2,nrep-1
      do n = 1,nvar
        fnorm2 = fnorm2 + gcrep(n,nrp)*gcrep(n,nrp)
        rmsd   = rmsd   + (xcrep(n,nrp) - xcoldrep(n,nrp))**2
      enddo
    enddo
    fnorm = sqrt(fnorm2)/dble((nrep - 2)*nvar)
    rmsd  = sqrt(rmsd)/dble((nrep - 2)*nvar)
    if (abs(fnorm - fnormlast).lt.1.d-6.and.niterlocal.gt.1.and.info.ne.1) then
      niterlocal = 0
      do nrp = 1,nrep
        do n = 1,nvar
          diag(n,nrp) = 1.0d0
        enddo
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
    lconverged = (fnorm.le.nebtol.or.nsince.gt.3)
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
      if (niter.ge.nnebiter) then
        write(ioout,'(/,''  **** Optimisation failed to converge - too many iterations ****'')')
      else
        write(ioout,'(/,''  **** Optimisation failed to converge ****'')')
      endif
    endif
  endif
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
          nebreplicaradius(i,ii) = xcrep(n,nrp+1)
        elseif (iopt(n).gt.nstrains) then
          nn = iopt(n) - nstrains
          i = (nn - 1)/3 + 1
          ix = nn - 3*(i - 1)
          nebreplicaxyz(ix,i,ii) = xcrep(n,nrp+1)
        else
          xstr(iopt(n)) = xcrep(n,nrp+1) - 1.0_dp
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
  call xctox0(nvar,xcrep(1,1),lgeometryOK)
  call x0tostr
!**********************
!  Output of results  *
!**********************
  nrep = nnebreplica(ncf) + 2
  if (ioproc) call outneb(nrep,fcrep,gcrep)
!****************************
!  Deallocate local memory  *
!****************************
  deallocate(wa,stat=status)
  if (status/=0) call deallocate_error('runneb','wa')
  deallocate(diag,stat=status)
  if (status/=0) call deallocate_error('runneb','diag')
  deallocate(hessian,stat=status)
  if (status/=0) call deallocate_error('runneb','hessian')
  deallocate(tangent,stat=status)
  if (status/=0) call deallocate_error('runneb','tangent')
  deallocate(xcoldrep,stat=status)
  if (status/=0) call deallocate_error('runneb','xcoldrep')
  deallocate(xcrep,stat=status)
  if (status/=0) call deallocate_error('runneb','xcrep')
  deallocate(pcrep,stat=status)
  if (status/=0) call deallocate_error('runneb','pcrep')
  deallocate(gcrep,stat=status)
  if (status/=0) call deallocate_error('runneb','gcrep')
  deallocate(fcrep,stat=status)
  if (status/=0) call deallocate_error('runneb','fcrep')
  deallocate(nrepptr,stat=status)
  if (status/=0) call deallocate_error('runneb','nrepptr')
!
  return
  end
