  subroutine cosmoderv(lgrad2)
!
!  Subroutine calculates the derivatives of the COSMO solvation model
!
!   3/02 Optimisation flags corrected to remove errors in derivatives
!        for non-frozen calculation when weights are present.
!   4/03 Second derivatives completed for partial weighting
!   4/03 Calculation of second derivatives accelerated and made more
!        compact in memory
!   4/03 COSMIC derivative modifications made
!   4/03 Use of intermediate array containing derivatives with respect
!        to SAS charges introduced. This reduces the scaling bottleneck
!        in the second derivative cross-term calculation.
!   5/03 COSMIC second derivatives modifications added and debugged
!   5/03 Segment weighting added
!   5/03 Near segment pointer used to ensure that atoms are in nearsas list
!   7/03 Contribution to second derivatives from dcosmoA2 added
!  11/04 Addition of dcosmoAA terms corrected
!  11/04 Use of sasparticles data structures added
!  12/04 cosmoBq matrix introduced instead of Bq
!   1/05 Calculation of deltaq removed since this is now done once in
!        setqonsas
!   1/05 Additional terms for modified COSMIC scheme added based on
!        segment weights and their derivatives, including modifying 
!        the call to setdsegweight to include total segweight derivs
!   1/05 Bug in addition of dcosmoAA x dqsas product corrected
!   1/05 Sums accelerated
!  12/08 Migrated to version 3.5 and converted to f90 format
!  11/09 Region derivatives added
!   6/10 Referencing of nregionno now relates to asymmetric unit.
!   6/10 Dummy allocation of d2segweight/d2totsegweight added as this is
!        passed in a subroutine call, regardless of lgrad2 value.
!   7/10 dcosmoAA also allocated as it is passed to a subroutine.
!
!  It is assumed that lgrad1 = .true. on entry otherwise routine
!  would not have been called. If lgrad = .true. then second
!  derivatives will be calculated as well.
!
!  This is a new version that allows for the weighting factors for
!  points close to the intersection of atoms.
!
!  If second derivatives are required, then the Cartesian derivatives
!  of cosmoA and cosmoB must be saved in matrix form to avoid multiple
!  recalculations, whereas for first derivative only runs the terms
!  can be used on the fly to save memory.
!
!  Note : strain derivatives not yet available.
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, July 2010
!
  use configurations, only : nregionno
  use cosmo
  use constants
  use control
  use current
  use derivatives
  use iochannels
  use optimisation
  use reallocate
  implicit none
!
!  Passed variables
!
  logical                                       :: lgrad2
!
!  Local variables
!
  integer(i4)                                   :: i, j, k, l
  integer(i4)                                   :: ind
  integer(i4)                                   :: jl, kl, ll
  integer(i4)                                   :: ierror
  integer(i4)                                   :: ix
  integer(i4)                                   :: iy
  integer(i4)                                   :: iz
  integer(i4)                                   :: ipts
  integer(i4)                                   :: jpts
  integer(i4)                                   :: jj
  integer(i4)                                   :: jj2
  integer(i4)                                   :: jx
  integer(i4)                                   :: jy
  integer(i4)                                   :: jz
  integer(i4)                                   :: imax
  integer(i4)                                   :: jmax
  integer(i4)                                   :: kmax
  integer(i4)                                   :: kk
  integer(i4)                                   :: kn
  integer(i4)                                   :: kpts
  integer(i4)                                   :: kx
  integer(i4)                                   :: ky
  integer(i4)                                   :: kz
  integer(i4)                                   :: lx
  integer(i4),                             save :: maxvec = 1000
  integer(i4)                                   :: n
  integer(i4)                                   :: n3
  integer(i4)                                   :: nari
  integer(i4)                                   :: nearsas
  integer(i4),  dimension(:), allocatable, save :: nearsasptr
  integer(i4),  dimension(:), allocatable, save :: nearsasrptr
  integer(i4)                                   :: nmid
  integer(i4)                                   :: nregioni
  integer(i4)                                   :: nregionj
  integer(i4)                                   :: nsetfi
  integer(i4)                                   :: numatfork
  integer(i4)                                   :: nvec
  integer(i4)                                   :: status
  logical,      dimension(:), allocatable, save :: lanynonunit
  logical                                       :: lcosmicd2
  logical                                       :: ldqneeded
  logical,      dimension(:), allocatable, save :: lnearsas
  logical                                       :: lperiodic
  real(dp)                                      :: Bqf
  real(dp)                                      :: Bqfs
  real(dp)                                      :: Bqsum
  real(dp)                                      :: d2qfct
  real(dp)                                      :: d2xx
  real(dp)                                      :: d2yx
  real(dp)                                      :: d2zx
  real(dp)                                      :: d2xy
  real(dp)                                      :: d2yy
  real(dp)                                      :: d2zy
  real(dp)                                      :: d2xz
  real(dp)                                      :: d2yz
  real(dp)                                      :: d2zz
  real(dp)                                      :: dqijx
  real(dp)                                      :: dqijy
  real(dp)                                      :: dqijz
  real(dp)                                      :: dABjkx
  real(dp)                                      :: dABjky
  real(dp)                                      :: dABjkz
  real(dp)                                      :: fact
  real(dp)                                      :: fct
  real(dp)                                      :: rnpts
  real(dp)                                      :: swi
  real(dp), dimension(:,:,:), pointer,     save :: dcosmoA
  real(dp), dimension(:,:,:), pointer,     save :: dcosmoA2
  real(dp), dimension(:,:,:), pointer,     save :: dcosmoAA
  real(dp), dimension(:,:,:), pointer,     save :: dcosmoB
  real(dp), dimension(:,:,:), allocatable, save :: dqsas
  real(dp), dimension(:,:),   allocatable, save :: dqsassum
  real(dp), dimension(:,:,:), allocatable, save :: dsegweight
  real(dp), dimension(:,:),   allocatable, save :: dtotsegweight
  real(dp), dimension(:,:,:,:,:), allocatable, save :: d2segweight
  real(dp), dimension(:,:,:), allocatable, save :: d2totsegweight
  real(dp), dimension(:,:),   pointer,     save :: fullA
  real(dp), dimension(:),     allocatable, save :: xvec
  real(dp), dimension(:),     allocatable, save :: yvec
  real(dp), dimension(:),     allocatable, save :: zvec
!
!  Check that strain derivatives are not required
!
  if (ncell.gt.0) then
    call outerror('Strain derivatives not available for COSMO',0_i4)
    call stopnow('cosmoderv')
  endif
!
!  Set flag as to whether derivatives with respect to SAS charges needed to be stored
!
  ldqneeded = (lgrad2.or.lcosmic) 
!
!  Allocate local memory
!
  allocate(lanynonunit(numat),stat=status)
  if (status/=0) call outofmemory('cosmoderv','lanynonunit')
  allocate(lnearsas(numat),stat=status)
  if (status/=0) call outofmemory('cosmoderv','lnearsas')
  allocate(nearsasptr(numat),stat=status)
  if (status/=0) call outofmemory('cosmoderv','nearsasptr')
  allocate(nearsasrptr(numat),stat=status)
  if (status/=0) call outofmemory('cosmoderv','nearsasrptr')
!
!  Set local constants
!
  fact  = 0.5_dp*autoev*autoangs*cosmofneps
  lperiodic = (ndim.gt.0)
!
!  Set up list of cell vectors that are required
!
  if (lperiodic) then
10   nvec = 0
    allocate(xvec(maxvec),stat=status)
    if (status/=0) call outofmemory('cosmoderv','xvec')
    allocate(yvec(maxvec),stat=status)
    if (status/=0) call outofmemory('cosmoderv','yvec')
    allocate(zvec(maxvec),stat=status)
    if (status/=0) call outofmemory('cosmoderv','zvec')
    call rtlist(nvec,cosmormax,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvec)
    if (nvec.gt.maxvec) then
      maxvec = nvec + 10
      deallocate(xvec,stat=status)
      if (status/=0) call deallocate_error('cosmoderv','xvec')
      deallocate(yvec,stat=status)
      if (status/=0) call deallocate_error('cosmoderv','yvec')
      deallocate(zvec,stat=status)
      if (status/=0) call deallocate_error('cosmoderv','zvec')
      goto 10
    endif
  else
    nvec = 1
    nmid = 1
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('cosmoderv','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('cosmoderv','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('cosmoderv','zvec')
    xvec(1) = 0.0_dp
    yvec(1) = 0.0_dp
    zvec(1) = 0.0_dp
  endif
!*******************************************
!  Weighting of points and segments setup  *
!*******************************************
!
!  Find atoms with only unit weights and that might interact with the SAS
!
!  npts        = no. of segments
!  nar         = no. of points for a given segment
!  nsetf       = pointer to first point of a given segment
!  npwt        = no. of atoms that contribute to weight of current point
!  npwtptr     = pointer to atoms that contribute to weight of current point
!  lanynonunit = if .true. then weighting factors must be handled for this atom
!  lnearsas    = if .true. then this atom contributes to the SAS, either directly
!                or via a weighting factor of another atom
!
  lanynonunit(1:numat) = .false.
  lnearsas(1:numat) = .false.
  do ipts = 1,npts
!
!  Point weighting
!
    i = cosmoatomptr(ipts)
    nari = nar(ipts)
    nsetfi = nsetf(ipts)
    lnearsas(i) = .true.
    do k = nsetfi,nsetfi+nari-1
      if (npwt(ipts).gt.0) then
        lanynonunit(i) = .true.
        do l = 1,npwt(ipts)                                                                                  
          lnearsas(npwtptr(l,ipts)) = .true.                                                                 
        enddo                                                                                                
      endif
    enddo
!
!  Segment weighting
!
    do j = 1,nnearseg(ipts)
      k = nnearsegptr(j,ipts)
      lnearsas(k) = .true.
    enddo
  enddo
!
!  Build pointers to go directly to atoms that contribute to the SAS
!
!  nearsas     = no. of atoms that contribute to SAS
!  nearsasptr  = pointer from SAS sequence no. to full atom set
!  nearsasrptr = pointer from full atom no. to SAS sequence
!
  nearsas = 0
  nearsasrptr(1:numat) = 0
  do i = 1,numat
    if (lnearsas(i)) then
      nearsas = nearsas + 1
      nearsasptr(nearsas) = i
      nearsasrptr(i) = nearsas
    endif
  enddo
!
!  Calculate segment weighting derivatives
!
  allocate(dsegweight(3,maxnearseg,npts),stat=status)
  if (status/=0) call outofmemory('cosmoderv','dsegweight')
  if (lgrad2) then
    allocate(d2segweight(3,3,maxnearseg,maxnearseg,npts),stat=status)
    if (status/=0) call outofmemory('cosmoderv','d2segweight')
  else
    allocate(d2segweight(3,3,1,1,1),stat=status)
    if (status/=0) call outofmemory('cosmoderv','d2segweight')
  endif
  if (lcosmic) then
    allocate(dtotsegweight(3,maxallnearseg),stat=status)
    if (status/=0) call outofmemory('cosmoderv','dtotsegweight')
    if (lgrad2) then
      allocate(d2totsegweight(3,3,maxallnearseg*(maxallnearseg+1)/2),stat=status)
      if (status/=0) call outofmemory('cosmoderv','d2totsegweight')
    else
      allocate(d2totsegweight(3,3,1),stat=status)
      if (status/=0) call outofmemory('cosmoderv','d2totsegweight')
    endif
  else
    allocate(dtotsegweight(3,1),stat=status)
    if (status/=0) call outofmemory('cosmoderv','dtotsegweight')
    allocate(d2totsegweight(3,3,1),stat=status)
    if (status/=0) call outofmemory('cosmoderv','d2totsegweight')
  endif
  call setdsegweight(.true.,lgrad2,dsegweight,d2segweight,dtotsegweight,d2totsegweight)
!*******************************************************
!  Allocation and initialisation of derivative arrays  *
!*******************************************************
  if (ldqneeded) then
    call realloc(dcosmoA,3_i4,npts,nearsas,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','dcosmoA')
    call realloc(dcosmoA2,3_i4,npts,nearsas,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','dcosmoA2')
    call realloc(dcosmoB,3_i4,numat,npts,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','dcosmoB')
    dcosmoA(1:3,1:npts,1:nearsas) = 0.0_dp
    dcosmoA2(1:3,1:npts,1:nearsas) = 0.0_dp
    dcosmoB(1:3,1:numat,1:npts) = 0.0_dp
  endif
!
!  For COSMIC second derivatives allocate and initialise matrices
!
  lcosmicd2 = (lgrad2.and.lcosmic)
  if (lcosmicd2) then
    call realloc(dcosmoAA,3_i4,nearsas,npts,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','dcosmoAA')
    dcosmoAA(1:3,1:nearsas,1:npts) = 0.0_dp
  else
    call realloc(dcosmoAA,1_i4,1_i4,1_i4,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','dcosmoAA')
  endif
  if (ldqneeded) then
!
!  Make full copy of inverse of A for simplicity and scale by fact 
!
    call realloc(fullA,npts,npts,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','fullA')
    kk = 0
    do ipts = 1,npts
      do jpts = 1,ipts
        kk = kk + 1
        fullA(ipts,jpts) = - 2.0_dp*cosmoA(kk)*fact
        fullA(jpts,ipts) = - 2.0_dp*cosmoA(kk)*fact
      enddo
    enddo
!
!  Form sum of Bq matrix multiplied by point weights
!
    Bqsum = 0.0_dp
    do ipts = 1,npts
      Bqsum = Bqsum + segweight(ipts)*cosmoBq(ipts)
    enddo
!
!  Set conversion factor from d2qsassum to derv2
!
    if (totsegweight.gt.1.0d-12) then
      rnpts = 0.5_dp/totsegweight
    else
      rnpts = 0.0_dp
    endif
    d2qfct = - rnpts*Bqsum
  else
    Bqsum = 0.0_dp
    d2qfct = 0.0_dp
  endif
!****************************
!  Derivatives of A matrix  *
!****************************
  call setdcosmoamat(ldqneeded,lcosmicd2,lgrad2,dcosmoA,dcosmoAA,fullA,d2qfct,dsegweight,d2segweight, &
    nvec,nmid,nearsas,nearsasptr,nearsasrptr,lnearsas,lanynonunit,xvec,yvec,zvec,dcosmoA2)
!****************************
!  Derivatives of B matrix  *
!****************************
  call setdcosmobmat(ldqneeded,lcosmicd2,lgrad2,dcosmoB,fullA,d2qfct,dsegweight,d2segweight)
  if (ldqneeded) then
!*********************************************************
!  COSMIC correction due to derivatives of charge shift  *
!*********************************************************
!
!  Allocate memory for charge derivatives
!
    allocate(dqsassum(3,numat),stat=status)
    if (status/=0) call outofmemory('cosmoderv','dqsassum')
    allocate(dqsas(3,numat,npts),stat=status)
    if (status/=0) call outofmemory('cosmoderv','dqsas')
!
!  Calculate SAS charge derivatives in dqsas scaled by fact
!
!  Initialise arrays to zero
!
    dqsassum(1:3,1:numat) = 0.0_dp
    dqsas(1:3,1:numat,1:npts) = 0.0_dp
!
!  Calculate contributions from A matrix derivatives
!
    do n = 1,nearsas
      i = nearsasptr(n)
      do jpts = 1,npts 
        j = cosmoatomptr(jpts)
        do kpts = 1,npts
          dqsas(1,i,kpts) = dqsas(1,i,kpts) - (dcosmoA(1,jpts,n) + dcosmoA2(1,jpts,n))*fullA(kpts,jpts)
          dqsas(2,i,kpts) = dqsas(2,i,kpts) - (dcosmoA(2,jpts,n) + dcosmoA2(2,jpts,n))*fullA(kpts,jpts)
          dqsas(3,i,kpts) = dqsas(3,i,kpts) - (dcosmoA(3,jpts,n) + dcosmoA2(3,jpts,n))*fullA(kpts,jpts)
          dqsas(1,j,kpts) = dqsas(1,j,kpts) + dcosmoA(1,jpts,n)*fullA(kpts,jpts)
          dqsas(2,j,kpts) = dqsas(2,j,kpts) + dcosmoA(2,jpts,n)*fullA(kpts,jpts)
          dqsas(3,j,kpts) = dqsas(3,j,kpts) + dcosmoA(3,jpts,n)*fullA(kpts,jpts)
        enddo
      enddo
    enddo
!
!  Calculate contributions from B matrix derivatives
!
    do jpts = 1,npts 
      do kpts = 1,npts
        do i = 1,numat
          dqsas(1,i,kpts) = dqsas(1,i,kpts) + dcosmoB(1,i,jpts)*fullA(kpts,jpts)
          dqsas(2,i,kpts) = dqsas(2,i,kpts) + dcosmoB(2,i,jpts)*fullA(kpts,jpts)
          dqsas(3,i,kpts) = dqsas(3,i,kpts) + dcosmoB(3,i,jpts)*fullA(kpts,jpts)
        enddo
      enddo
    enddo
    if (lcosmic) then
!
!  Sum derviatives over SAS points
!
      do kpts = 1,npts
        do i = 1,numat
          dqsassum(1,i) = dqsassum(1,i) + dqsas(1,i,kpts)
          dqsassum(2,i) = dqsassum(2,i) + dqsas(2,i,kpts)
          dqsassum(3,i) = dqsassum(3,i) + dqsas(3,i,kpts)
        enddo
      enddo
!
!  Scale by 1/2 x inverse number of SAS points
!
      do i = 1,numat
        dqsassum(1,i) = rnpts*dqsassum(1,i)
        dqsassum(2,i) = rnpts*dqsassum(2,i)
        dqsassum(3,i) = rnpts*dqsassum(3,i)
      enddo
!
!  Add COSMIC corrections to first derivatives due to derivative of sum of charges
!
      do i = 1,numat
        xdrv(i) = xdrv(i) - Bqsum*dqsassum(1,i)
        ydrv(i) = ydrv(i) - Bqsum*dqsassum(2,i)
        zdrv(i) = zdrv(i) - Bqsum*dqsassum(3,i)
        nregioni = nregionno(nsft+nrelat(i))
        xregdrv(nregioni) = xregdrv(nregioni) - Bqsum*dqsassum(1,i)
        yregdrv(nregioni) = yregdrv(nregioni) - Bqsum*dqsassum(2,i)
        zregdrv(nregioni) = zregdrv(nregioni) - Bqsum*dqsassum(3,i)
      enddo
!
!  Add COSMIC corrections to first derivatives due to derivative of segment weighting factors
!
      Bqfs = 0.0d0
      do ipts = 1,npts
        i = cosmoatomptr(ipts)
        nregioni = nregionno(nsft+nrelat(i))
        Bqf = fact*deltaq*cosmoBq(ipts)
        Bqfs = Bqfs + Bqf*segweight(ipts)
        do jj = 1,nnearseg(ipts)
          j = nnearsegptr(jj,ipts)
          xdrv(i) = xdrv(i) + Bqf*dsegweight(1,jj,ipts)
          ydrv(i) = ydrv(i) + Bqf*dsegweight(2,jj,ipts)
          zdrv(i) = zdrv(i) + Bqf*dsegweight(3,jj,ipts)
          xdrv(j) = xdrv(j) - Bqf*dsegweight(1,jj,ipts)
          ydrv(j) = ydrv(j) - Bqf*dsegweight(2,jj,ipts)
          zdrv(j) = zdrv(j) - Bqf*dsegweight(3,jj,ipts)
          nregionj = nregionno(nsft+nrelat(j))
          if (nregioni.ne.nregionj) then
            xregdrv(nregioni) = xregdrv(nregioni) + Bqf*dsegweight(1,jj,ipts)
            yregdrv(nregioni) = yregdrv(nregioni) + Bqf*dsegweight(2,jj,ipts)
            zregdrv(nregioni) = zregdrv(nregioni) + Bqf*dsegweight(3,jj,ipts)
            xregdrv(nregionj) = xregdrv(nregionj) - Bqf*dsegweight(1,jj,ipts)
            yregdrv(nregionj) = yregdrv(nregionj) - Bqf*dsegweight(2,jj,ipts)
            zregdrv(nregionj) = zregdrv(nregionj) - Bqf*dsegweight(3,jj,ipts)
          endif
        enddo
      enddo
!
!  Add COSMIC corrections to the first derivatives due to the total segment weight derivatives
!
      Bqfs = Bqfs/totsegweight
      do jj = 1,nallnearseg
        j = nallnearsegptr(jj)
        xdrv(j) = xdrv(j) + Bqfs*dtotsegweight(1,jj)
        ydrv(j) = ydrv(j) + Bqfs*dtotsegweight(2,jj)
        zdrv(j) = zdrv(j) + Bqfs*dtotsegweight(3,jj)
        nregionj = nregionno(nsft+nrelat(j))
        xregdrv(nregionj) = xregdrv(nregionj) + Bqfs*dtotsegweight(1,jj)
        yregdrv(nregionj) = yregdrv(nregionj) + Bqfs*dtotsegweight(2,jj)
        zregdrv(nregionj) = zregdrv(nregionj) + Bqfs*dtotsegweight(3,jj)
      enddo
    endif
  endif
!************************************************************
!  Combination of first derivatives for second derivatives  *
!************************************************************
  if (lgrad2) then
!
!  (q).(dB/da).(A-1).(dA/db).(Qs)
!
!  and
!
!  (q).(dB/da).(A-1).(dB/db).(q)
!
    ix = -2
    iy = -1
    iz =  0
    do i = 1,numat
      ix = ix + 3
      iy = iy + 3
      iz = iz + 3
      do jpts = 1,npts
        j = cosmoatomptr(jpts)
        jx = 3*(j - 1) + 1
        jy = jx + 1
        jz = jx + 2
        dqijx = dqsas(1,i,jpts)
        dqijy = dqsas(2,i,jpts)
        dqijz = dqsas(3,i,jpts)
!
!  Set looping index for K to avoid redundant looping
!
        if (j.lt.i) then
          numatfork = numat
        else
          numatfork = i - 1
        endif
!
        do k = 1,numatfork
          kx = 3*(k - 1) + 1
          ky = kx + 1
          kz = kx + 2
          kn = nearsasrptr(k)
!
!  Contribution from A
!
          if (kn.gt.0) then
            dABjkx = dcosmoA(1,jpts,kn)
            dABjky = dcosmoA(2,jpts,kn)
            dABjkz = dcosmoA(3,jpts,kn)
!
            d2xx = dABjkx*dqijx 
            d2yx = dABjky*dqijx 
            d2zx = dABjkz*dqijx 
            d2xy = dABjkx*dqijy 
            d2yy = dABjky*dqijy 
            d2zy = dABjkz*dqijy 
            d2xz = dABjkx*dqijz 
            d2yz = dABjky*dqijz 
            d2zz = dABjkz*dqijz 
!  j - i
            if (j.lt.i) then
              derv2(jx,ix) = derv2(jx,ix) + d2xx
              derv2(jy,ix) = derv2(jy,ix) + d2yx
              derv2(jz,ix) = derv2(jz,ix) + d2zx
              derv2(jx,iy) = derv2(jx,iy) + d2xy
              derv2(jy,iy) = derv2(jy,iy) + d2yy
              derv2(jz,iy) = derv2(jz,iy) + d2zy
              derv2(jx,iz) = derv2(jx,iz) + d2xz
              derv2(jy,iz) = derv2(jy,iz) + d2yz
              derv2(jz,iz) = derv2(jz,iz) + d2zz
            endif
!  k - i
            if (k.lt.i) then
              derv2(kx,ix) = derv2(kx,ix) - d2xx
              derv2(ky,ix) = derv2(ky,ix) - d2yx
              derv2(kz,ix) = derv2(kz,ix) - d2zx
              derv2(kx,iy) = derv2(kx,iy) - d2xy
              derv2(ky,iy) = derv2(ky,iy) - d2yy
              derv2(kz,iy) = derv2(kz,iy) - d2zy
              derv2(kx,iz) = derv2(kx,iz) - d2xz
              derv2(ky,iz) = derv2(ky,iz) - d2yz
              derv2(kz,iz) = derv2(kz,iz) - d2zz
            endif
!
!  Contribution from dcosmoA2
!
            dABjkx = dcosmoA2(1,jpts,kn)
            dABjky = dcosmoA2(2,jpts,kn)
            dABjkz = dcosmoA2(3,jpts,kn)
!
            d2xx = dABjkx*dqijx
            d2yx = dABjky*dqijx
            d2zx = dABjkz*dqijx
            d2xy = dABjkx*dqijy
            d2yy = dABjky*dqijy
            d2zy = dABjkz*dqijy
            d2xz = dABjkx*dqijz
            d2yz = dABjky*dqijz
            d2zz = dABjkz*dqijz
!  k - i
            if (k.lt.i) then
              derv2(kx,ix) = derv2(kx,ix) - d2xx
              derv2(ky,ix) = derv2(ky,ix) - d2yx
              derv2(kz,ix) = derv2(kz,ix) - d2zx
              derv2(kx,iy) = derv2(kx,iy) - d2xy
              derv2(ky,iy) = derv2(ky,iy) - d2yy
              derv2(kz,iy) = derv2(kz,iy) - d2zy
              derv2(kx,iz) = derv2(kx,iz) - d2xz
              derv2(ky,iz) = derv2(ky,iz) - d2yz
              derv2(kz,iz) = derv2(kz,iz) - d2zz
            endif
          endif
!
!  Contribution from B
!
          dABjkx = dcosmoB(1,k,jpts)
          dABjky = dcosmoB(2,k,jpts)
          dABjkz = dcosmoB(3,k,jpts)
!
          d2xx = dABjkx*dqijx 
          d2yx = dABjky*dqijx 
          d2zx = dABjkz*dqijx 
          d2xy = dABjkx*dqijy 
          d2yy = dABjky*dqijy 
          d2zy = dABjkz*dqijy 
          d2xz = dABjkx*dqijz 
          d2yz = dABjky*dqijz 
          d2zz = dABjkz*dqijz 
!  k - i
          if (k.lt.i) then
            derv2(kx,ix) = derv2(kx,ix) + d2xx
            derv2(ky,ix) = derv2(ky,ix) + d2yx
            derv2(kz,ix) = derv2(kz,ix) + d2zx
            derv2(kx,iy) = derv2(kx,iy) + d2xy
            derv2(ky,iy) = derv2(ky,iy) + d2yy
            derv2(kz,iy) = derv2(kz,iy) + d2zy
            derv2(kx,iz) = derv2(kx,iz) + d2xz
            derv2(ky,iz) = derv2(ky,iz) + d2yz
            derv2(kz,iz) = derv2(kz,iz) + d2zz
          endif
        enddo
      enddo
    enddo
  endif
!*********************************************
!  COSMIC corrections to second derivatives  *
!*********************************************
  if (lcosmicd2) then
!
!  Add COSMIC corrections from products of charge derivatives and B matrix derivatives
!
    do ipts = 1,npts     
      swi = segweight(ipts)
      do j = 1,numat
        jl = 3*(j - 1)
        do jx = 1,3
          do k = 1,j
            kl = 3*(k - 1)
            do kx = 1,3      
              derv2(kl+kx,jl+jx) = derv2(kl+kx,jl+jx) - swi*dqsassum(jx,j)*dcosmoB(kx,k,ipts) &
                                                      - swi*dqsassum(kx,k)*dcosmoB(jx,j,ipts)
            enddo
          enddo
        enddo
      enddo
    enddo
!
!  Add products to second derivative of the total charge correction
!
    fct = 0.5_dp*d2qfct/fact
    do ipts = 1,npts
      do kn = 1,nearsas
        k = nearsasptr(kn)
        kl = 3*(k - 1)
        do kx = 1,3
          do l = 1,numat
            ll = 3*(l - 1)
            do lx = 1,3
              if (k.ge.l) then
                derv2(ll+lx,kl+kx) = derv2(ll+lx,kl+kx) - fct*dcosmoAA(kx,kn,ipts)*dqsas(lx,l,ipts) 
              else
                derv2(kl+kx,ll+lx) = derv2(kl+kx,ll+lx) - fct*dcosmoAA(kx,kn,ipts)*dqsas(lx,l,ipts) 
              endif
            enddo       
          enddo
        enddo
      enddo
    enddo
    if (lsegsmooth) then
!
!  Add COSMIC corrections to second derivatives due to derivative of segment weighting factors
!
!  Partial weight : dV/da x dw/db
!
      Bqf = fact*deltaq
      do ipts = 1,npts
        i = cosmoatomptr(ipts)
        ix = 3*(i - 1) + 1
        iy = ix + 1
        iz = iy + 1
        do jj = 1,nnearseg(ipts)
          j = nnearsegptr(jj,ipts)
          jx = 3*(j - 1) + 1
          jy = jx + 1
          jz = jy + 1
          do k = 1,numat
            kx = 3*(k - 1) + 1
            ky = kx + 1
            kz = kx + 2
            if (i.ge.k) then
              derv2(kx,ix) = derv2(kx,ix) + Bqf*dcosmoB(1,k,ipts)*dsegweight(1,jj,ipts)
              derv2(ky,ix) = derv2(ky,ix) + Bqf*dcosmoB(2,k,ipts)*dsegweight(1,jj,ipts)
              derv2(kz,ix) = derv2(kz,ix) + Bqf*dcosmoB(3,k,ipts)*dsegweight(1,jj,ipts)
              derv2(kx,iy) = derv2(kx,iy) + Bqf*dcosmoB(1,k,ipts)*dsegweight(2,jj,ipts)
              derv2(ky,iy) = derv2(ky,iy) + Bqf*dcosmoB(2,k,ipts)*dsegweight(2,jj,ipts)
              derv2(kz,iy) = derv2(kz,iy) + Bqf*dcosmoB(3,k,ipts)*dsegweight(2,jj,ipts)
              derv2(kx,iz) = derv2(kx,iz) + Bqf*dcosmoB(1,k,ipts)*dsegweight(3,jj,ipts)
              derv2(ky,iz) = derv2(ky,iz) + Bqf*dcosmoB(2,k,ipts)*dsegweight(3,jj,ipts)
              derv2(kz,iz) = derv2(kz,iz) + Bqf*dcosmoB(3,k,ipts)*dsegweight(3,jj,ipts)
            else
              derv2(ix,kx) = derv2(ix,kx) + Bqf*dcosmoB(1,k,ipts)*dsegweight(1,jj,ipts)
              derv2(iy,kx) = derv2(iy,kx) + Bqf*dcosmoB(1,k,ipts)*dsegweight(2,jj,ipts)
              derv2(iz,kx) = derv2(iz,kx) + Bqf*dcosmoB(1,k,ipts)*dsegweight(3,jj,ipts)
              derv2(ix,ky) = derv2(ix,ky) + Bqf*dcosmoB(2,k,ipts)*dsegweight(1,jj,ipts)
              derv2(iy,ky) = derv2(iy,ky) + Bqf*dcosmoB(2,k,ipts)*dsegweight(2,jj,ipts)
              derv2(iz,ky) = derv2(iz,ky) + Bqf*dcosmoB(2,k,ipts)*dsegweight(3,jj,ipts)
              derv2(ix,kz) = derv2(ix,kz) + Bqf*dcosmoB(3,k,ipts)*dsegweight(1,jj,ipts)
              derv2(iy,kz) = derv2(iy,kz) + Bqf*dcosmoB(3,k,ipts)*dsegweight(2,jj,ipts)
              derv2(iz,kz) = derv2(iz,kz) + Bqf*dcosmoB(3,k,ipts)*dsegweight(3,jj,ipts)
            endif
            if (j.ge.k) then
              derv2(kx,jx) = derv2(kx,jx) - Bqf*dcosmoB(1,k,ipts)*dsegweight(1,jj,ipts)
              derv2(ky,jx) = derv2(ky,jx) - Bqf*dcosmoB(2,k,ipts)*dsegweight(1,jj,ipts)
              derv2(kz,jx) = derv2(kz,jx) - Bqf*dcosmoB(3,k,ipts)*dsegweight(1,jj,ipts)
              derv2(kx,jy) = derv2(kx,jy) - Bqf*dcosmoB(1,k,ipts)*dsegweight(2,jj,ipts)
              derv2(ky,jy) = derv2(ky,jy) - Bqf*dcosmoB(2,k,ipts)*dsegweight(2,jj,ipts)
              derv2(kz,jy) = derv2(kz,jy) - Bqf*dcosmoB(3,k,ipts)*dsegweight(2,jj,ipts)
              derv2(kx,jz) = derv2(kx,jz) - Bqf*dcosmoB(1,k,ipts)*dsegweight(3,jj,ipts)
              derv2(ky,jz) = derv2(ky,jz) - Bqf*dcosmoB(2,k,ipts)*dsegweight(3,jj,ipts)
              derv2(kz,jz) = derv2(kz,jz) - Bqf*dcosmoB(3,k,ipts)*dsegweight(3,jj,ipts)
            else
              derv2(jx,kx) = derv2(jx,kx) - Bqf*dcosmoB(1,k,ipts)*dsegweight(1,jj,ipts)
              derv2(jy,kx) = derv2(jy,kx) - Bqf*dcosmoB(1,k,ipts)*dsegweight(2,jj,ipts)
              derv2(jz,kx) = derv2(jz,kx) - Bqf*dcosmoB(1,k,ipts)*dsegweight(3,jj,ipts)
              derv2(jx,ky) = derv2(jx,ky) - Bqf*dcosmoB(2,k,ipts)*dsegweight(1,jj,ipts)
              derv2(jy,ky) = derv2(jy,ky) - Bqf*dcosmoB(2,k,ipts)*dsegweight(2,jj,ipts)
              derv2(jz,ky) = derv2(jz,ky) - Bqf*dcosmoB(2,k,ipts)*dsegweight(3,jj,ipts)
              derv2(jx,kz) = derv2(jx,kz) - Bqf*dcosmoB(3,k,ipts)*dsegweight(1,jj,ipts)
              derv2(jy,kz) = derv2(jy,kz) - Bqf*dcosmoB(3,k,ipts)*dsegweight(2,jj,ipts)
              derv2(jz,kz) = derv2(jz,kz) - Bqf*dcosmoB(3,k,ipts)*dsegweight(3,jj,ipts)
            endif
          enddo
        enddo
      enddo
!
!  Total weight: dV/da x dw/db
!
      Bqf = - fact*deltaq/totsegweight
      do ipts = 1,npts
        i = cosmoatomptr(ipts)
        ix = 3*(i - 1) + 1
        iy = ix + 1
        iz = iy + 1
        Bqfs = Bqf*segweight(ipts)
        do jj = 1,nallnearseg
          j = nallnearsegptr(jj)
          jx = 3*(j - 1) + 1
          jy = jx + 1
          jz = jy + 1
          do k = 1,numat
            kx = 3*(k - 1) + 1
            ky = kx + 1
            kz = kx + 2
            if (j.ge.k) then
              derv2(kx,jx) = derv2(kx,jx) - Bqfs*dcosmoB(1,k,ipts)*dtotsegweight(1,jj)
              derv2(ky,jx) = derv2(ky,jx) - Bqfs*dcosmoB(2,k,ipts)*dtotsegweight(1,jj)
              derv2(kz,jx) = derv2(kz,jx) - Bqfs*dcosmoB(3,k,ipts)*dtotsegweight(1,jj)
              derv2(kx,jy) = derv2(kx,jy) - Bqfs*dcosmoB(1,k,ipts)*dtotsegweight(2,jj)
              derv2(ky,jy) = derv2(ky,jy) - Bqfs*dcosmoB(2,k,ipts)*dtotsegweight(2,jj)
              derv2(kz,jy) = derv2(kz,jy) - Bqfs*dcosmoB(3,k,ipts)*dtotsegweight(2,jj)
              derv2(kx,jz) = derv2(kx,jz) - Bqfs*dcosmoB(1,k,ipts)*dtotsegweight(3,jj)
              derv2(ky,jz) = derv2(ky,jz) - Bqfs*dcosmoB(2,k,ipts)*dtotsegweight(3,jj)
              derv2(kz,jz) = derv2(kz,jz) - Bqfs*dcosmoB(3,k,ipts)*dtotsegweight(3,jj)
            else
              derv2(jx,kx) = derv2(jx,kx) - Bqfs*dcosmoB(1,k,ipts)*dtotsegweight(1,jj)
              derv2(jy,kx) = derv2(jy,kx) - Bqfs*dcosmoB(1,k,ipts)*dtotsegweight(2,jj)
              derv2(jz,kx) = derv2(jz,kx) - Bqfs*dcosmoB(1,k,ipts)*dtotsegweight(3,jj)
              derv2(jx,ky) = derv2(jx,ky) - Bqfs*dcosmoB(2,k,ipts)*dtotsegweight(1,jj)
              derv2(jy,ky) = derv2(jy,ky) - Bqfs*dcosmoB(2,k,ipts)*dtotsegweight(2,jj)
              derv2(jz,ky) = derv2(jz,ky) - Bqfs*dcosmoB(2,k,ipts)*dtotsegweight(3,jj)
              derv2(jx,kz) = derv2(jx,kz) - Bqfs*dcosmoB(3,k,ipts)*dtotsegweight(1,jj)
              derv2(jy,kz) = derv2(jy,kz) - Bqfs*dcosmoB(3,k,ipts)*dtotsegweight(2,jj)
              derv2(jz,kz) = derv2(jz,kz) - Bqfs*dcosmoB(3,k,ipts)*dtotsegweight(3,jj)
            endif
          enddo 
        enddo 
      enddo
!
!  Partial weight : d(sum q)/da x dw/db
!
      do ipts = 1,npts
        i = cosmoatomptr(ipts)
        ix = 3*(i - 1) + 1
        iy = ix + 1
        iz = iy + 1
        Bqf = cosmoBq(ipts)
        do jj = 1,nnearseg(ipts)
          j = nnearsegptr(jj,ipts)
          jx = 3*(j - 1) + 1
          jy = jx + 1
          jz = jy + 1
          do k = 1,numat
            kx = 3*(k - 1) + 1
            ky = kx + 1
            kz = kx + 2
            if (i.ge.k) then
              derv2(kx,ix) = derv2(kx,ix) + Bqf*dqsassum(1,k)*dsegweight(1,jj,ipts)
              derv2(ky,ix) = derv2(ky,ix) + Bqf*dqsassum(2,k)*dsegweight(1,jj,ipts)
              derv2(kz,ix) = derv2(kz,ix) + Bqf*dqsassum(3,k)*dsegweight(1,jj,ipts)
              derv2(kx,iy) = derv2(kx,iy) + Bqf*dqsassum(1,k)*dsegweight(2,jj,ipts)
              derv2(ky,iy) = derv2(ky,iy) + Bqf*dqsassum(2,k)*dsegweight(2,jj,ipts)
              derv2(kz,iy) = derv2(kz,iy) + Bqf*dqsassum(3,k)*dsegweight(2,jj,ipts)
              derv2(kx,iz) = derv2(kx,iz) + Bqf*dqsassum(1,k)*dsegweight(3,jj,ipts)
              derv2(ky,iz) = derv2(ky,iz) + Bqf*dqsassum(2,k)*dsegweight(3,jj,ipts)
              derv2(kz,iz) = derv2(kz,iz) + Bqf*dqsassum(3,k)*dsegweight(3,jj,ipts)
            else
              derv2(ix,kx) = derv2(ix,kx) + Bqf*dqsassum(1,k)*dsegweight(1,jj,ipts)
              derv2(iy,kx) = derv2(iy,kx) + Bqf*dqsassum(1,k)*dsegweight(2,jj,ipts)
              derv2(iz,kx) = derv2(iz,kx) + Bqf*dqsassum(1,k)*dsegweight(3,jj,ipts)
              derv2(ix,ky) = derv2(ix,ky) + Bqf*dqsassum(2,k)*dsegweight(1,jj,ipts)
              derv2(iy,ky) = derv2(iy,ky) + Bqf*dqsassum(2,k)*dsegweight(2,jj,ipts)
              derv2(iz,ky) = derv2(iz,ky) + Bqf*dqsassum(2,k)*dsegweight(3,jj,ipts)
              derv2(ix,kz) = derv2(ix,kz) + Bqf*dqsassum(3,k)*dsegweight(1,jj,ipts)
              derv2(iy,kz) = derv2(iy,kz) + Bqf*dqsassum(3,k)*dsegweight(2,jj,ipts)
              derv2(iz,kz) = derv2(iz,kz) + Bqf*dqsassum(3,k)*dsegweight(3,jj,ipts)
            endif
            if (j.ge.k) then
              derv2(kx,jx) = derv2(kx,jx) - Bqf*dqsassum(1,k)*dsegweight(1,jj,ipts)
              derv2(ky,jx) = derv2(ky,jx) - Bqf*dqsassum(2,k)*dsegweight(1,jj,ipts)
              derv2(kz,jx) = derv2(kz,jx) - Bqf*dqsassum(3,k)*dsegweight(1,jj,ipts)
              derv2(kx,jy) = derv2(kx,jy) - Bqf*dqsassum(1,k)*dsegweight(2,jj,ipts)
              derv2(ky,jy) = derv2(ky,jy) - Bqf*dqsassum(2,k)*dsegweight(2,jj,ipts)
              derv2(kz,jy) = derv2(kz,jy) - Bqf*dqsassum(3,k)*dsegweight(2,jj,ipts)
              derv2(kx,jz) = derv2(kx,jz) - Bqf*dqsassum(1,k)*dsegweight(3,jj,ipts)
              derv2(ky,jz) = derv2(ky,jz) - Bqf*dqsassum(2,k)*dsegweight(3,jj,ipts)
              derv2(kz,jz) = derv2(kz,jz) - Bqf*dqsassum(3,k)*dsegweight(3,jj,ipts)
            else
              derv2(jx,kx) = derv2(jx,kx) - Bqf*dqsassum(1,k)*dsegweight(1,jj,ipts)
              derv2(jy,kx) = derv2(jy,kx) - Bqf*dqsassum(1,k)*dsegweight(2,jj,ipts)
              derv2(jz,kx) = derv2(jz,kx) - Bqf*dqsassum(1,k)*dsegweight(3,jj,ipts)
              derv2(jx,ky) = derv2(jx,ky) - Bqf*dqsassum(2,k)*dsegweight(1,jj,ipts)
              derv2(jy,ky) = derv2(jy,ky) - Bqf*dqsassum(2,k)*dsegweight(2,jj,ipts)
              derv2(jz,ky) = derv2(jz,ky) - Bqf*dqsassum(2,k)*dsegweight(3,jj,ipts)
              derv2(jx,kz) = derv2(jx,kz) - Bqf*dqsassum(3,k)*dsegweight(1,jj,ipts)
              derv2(jy,kz) = derv2(jy,kz) - Bqf*dqsassum(3,k)*dsegweight(2,jj,ipts)
              derv2(jz,kz) = derv2(jz,kz) - Bqf*dqsassum(3,k)*dsegweight(3,jj,ipts)
            endif
          enddo
        enddo
      enddo
!         
!  Total weight : d(sum q)/da x dw/db
!
      Bqf = Bqsum/totsegweight
      do jj = 1,nallnearseg
        j = nallnearsegptr(jj)
        jx = 3*(j - 1) + 1
        jy = jx + 1
        jz = jy + 1
        do k = 1,numat
          kx = 3*(k - 1) + 1
          ky = kx + 1
          kz = kx + 2 
          if (j.ge.k) then
            derv2(kx,jx) = derv2(kx,jx) + Bqf*dqsassum(1,k)*dtotsegweight(1,jj)
            derv2(ky,jx) = derv2(ky,jx) + Bqf*dqsassum(2,k)*dtotsegweight(1,jj)
            derv2(kz,jx) = derv2(kz,jx) + Bqf*dqsassum(3,k)*dtotsegweight(1,jj)
            derv2(kx,jy) = derv2(kx,jy) + Bqf*dqsassum(1,k)*dtotsegweight(2,jj)
            derv2(ky,jy) = derv2(ky,jy) + Bqf*dqsassum(2,k)*dtotsegweight(2,jj)
            derv2(kz,jy) = derv2(kz,jy) + Bqf*dqsassum(3,k)*dtotsegweight(2,jj)
            derv2(kx,jz) = derv2(kx,jz) + Bqf*dqsassum(1,k)*dtotsegweight(3,jj)
            derv2(ky,jz) = derv2(ky,jz) + Bqf*dqsassum(2,k)*dtotsegweight(3,jj)
            derv2(kz,jz) = derv2(kz,jz) + Bqf*dqsassum(3,k)*dtotsegweight(3,jj)
          else
            derv2(jx,kx) = derv2(jx,kx) + Bqf*dqsassum(1,k)*dtotsegweight(1,jj)
            derv2(jy,kx) = derv2(jy,kx) + Bqf*dqsassum(1,k)*dtotsegweight(2,jj)
            derv2(jz,kx) = derv2(jz,kx) + Bqf*dqsassum(1,k)*dtotsegweight(3,jj)
            derv2(jx,ky) = derv2(jx,ky) + Bqf*dqsassum(2,k)*dtotsegweight(1,jj)
            derv2(jy,ky) = derv2(jy,ky) + Bqf*dqsassum(2,k)*dtotsegweight(2,jj)
            derv2(jz,ky) = derv2(jz,ky) + Bqf*dqsassum(2,k)*dtotsegweight(3,jj)
            derv2(jx,kz) = derv2(jx,kz) + Bqf*dqsassum(3,k)*dtotsegweight(1,jj)
            derv2(jy,kz) = derv2(jy,kz) + Bqf*dqsassum(3,k)*dtotsegweight(2,jj)
            derv2(jz,kz) = derv2(jz,kz) + Bqf*dqsassum(3,k)*dtotsegweight(3,jj)
          endif
        enddo
      enddo
!
!  Partial weight : d2w/da.db
!
      do ipts = 1,npts
        i = cosmoatomptr(ipts)
        ix = 3*(i - 1) + 1
        iy = ix + 1
        iz = iy + 1
        Bqf = fact*deltaq*cosmoBq(ipts)
        do jj = 1,nnearseg(ipts)
          j = nnearsegptr(jj,ipts)
          jx = 3*(j - 1) + 1
          jy = jx + 1
          jz = jx + 2
          if (i.ge.j) then
            derv2(jx,ix) = derv2(jx,ix) - Bqf*d2segweight(1,1,jj,jj,ipts)
            derv2(jy,ix) = derv2(jy,ix) - Bqf*d2segweight(2,1,jj,jj,ipts)
            derv2(jz,ix) = derv2(jz,ix) - Bqf*d2segweight(3,1,jj,jj,ipts)
            derv2(jx,iy) = derv2(jx,iy) - Bqf*d2segweight(1,2,jj,jj,ipts)
            derv2(jy,iy) = derv2(jy,iy) - Bqf*d2segweight(2,2,jj,jj,ipts)
            derv2(jz,iy) = derv2(jz,iy) - Bqf*d2segweight(3,2,jj,jj,ipts)
            derv2(jx,iz) = derv2(jx,iz) - Bqf*d2segweight(1,3,jj,jj,ipts)
            derv2(jy,iz) = derv2(jy,iz) - Bqf*d2segweight(2,3,jj,jj,ipts)
            derv2(jz,iz) = derv2(jz,iz) - Bqf*d2segweight(3,3,jj,jj,ipts)
          else
            derv2(ix,jx) = derv2(ix,jx) - Bqf*d2segweight(1,1,jj,jj,ipts)
            derv2(iy,jx) = derv2(iy,jx) - Bqf*d2segweight(1,2,jj,jj,ipts)
            derv2(iz,jx) = derv2(iz,jx) - Bqf*d2segweight(1,3,jj,jj,ipts)
            derv2(ix,jy) = derv2(ix,jy) - Bqf*d2segweight(2,1,jj,jj,ipts)
            derv2(iy,jy) = derv2(iy,jy) - Bqf*d2segweight(2,2,jj,jj,ipts)
            derv2(iz,jy) = derv2(iz,jy) - Bqf*d2segweight(2,3,jj,jj,ipts)
            derv2(ix,jz) = derv2(ix,jz) - Bqf*d2segweight(3,1,jj,jj,ipts)
            derv2(iy,jz) = derv2(iy,jz) - Bqf*d2segweight(3,2,jj,jj,ipts)
            derv2(iz,jz) = derv2(iz,jz) - Bqf*d2segweight(3,3,jj,jj,ipts)
          endif
        enddo
        do jj = 2,nnearseg(ipts)
          j = nnearsegptr(jj,ipts)
          jx = 3*(j - 1) + 1
          jy = jx + 1
          jz = jx + 2
          do kk = 1,jj-1
            k = nnearsegptr(kk,ipts)
            kx = 3*(k - 1) + 1
            ky = kx + 1
            kz = kx + 2
            if (j.ge.k) then
              derv2(kx,jx) = derv2(kx,jx) - Bqf*d2segweight(1,1,kk,jj,ipts)
              derv2(ky,jx) = derv2(ky,jx) - Bqf*d2segweight(2,1,kk,jj,ipts)
              derv2(kz,jx) = derv2(kz,jx) - Bqf*d2segweight(3,1,kk,jj,ipts)
              derv2(kx,jy) = derv2(kx,jy) - Bqf*d2segweight(1,2,kk,jj,ipts)
              derv2(ky,jy) = derv2(ky,jy) - Bqf*d2segweight(2,2,kk,jj,ipts)
              derv2(kz,jy) = derv2(kz,jy) - Bqf*d2segweight(3,2,kk,jj,ipts)
              derv2(kx,jz) = derv2(kx,jz) - Bqf*d2segweight(1,3,kk,jj,ipts)
              derv2(ky,jz) = derv2(ky,jz) - Bqf*d2segweight(2,3,kk,jj,ipts)
              derv2(kz,jz) = derv2(kz,jz) - Bqf*d2segweight(3,3,kk,jj,ipts)
            else
              derv2(jx,kx) = derv2(jx,kx) - Bqf*d2segweight(1,1,kk,jj,ipts)
              derv2(jy,kx) = derv2(jy,kx) - Bqf*d2segweight(1,2,kk,jj,ipts)
              derv2(jz,kx) = derv2(jz,kx) - Bqf*d2segweight(1,3,kk,jj,ipts)
              derv2(jx,ky) = derv2(jx,ky) - Bqf*d2segweight(2,1,kk,jj,ipts)
              derv2(jy,ky) = derv2(jy,ky) - Bqf*d2segweight(2,2,kk,jj,ipts)
              derv2(jz,ky) = derv2(jz,ky) - Bqf*d2segweight(2,3,kk,jj,ipts)
              derv2(jx,kz) = derv2(jx,kz) - Bqf*d2segweight(3,1,kk,jj,ipts)
              derv2(jy,kz) = derv2(jy,kz) - Bqf*d2segweight(3,2,kk,jj,ipts)
              derv2(jz,kz) = derv2(jz,kz) - Bqf*d2segweight(3,3,kk,jj,ipts)
            endif
!
            if (i.ge.j) then
              derv2(jx,ix) = derv2(jx,ix) + Bqf*d2segweight(1,1,kk,jj,ipts)
              derv2(jy,ix) = derv2(jy,ix) + Bqf*d2segweight(1,2,kk,jj,ipts)
              derv2(jz,ix) = derv2(jz,ix) + Bqf*d2segweight(1,3,kk,jj,ipts)
              derv2(jx,iy) = derv2(jx,iy) + Bqf*d2segweight(2,1,kk,jj,ipts)
              derv2(jy,iy) = derv2(jy,iy) + Bqf*d2segweight(2,2,kk,jj,ipts)
              derv2(jz,iy) = derv2(jz,iy) + Bqf*d2segweight(2,3,kk,jj,ipts)
              derv2(jx,iz) = derv2(jx,iz) + Bqf*d2segweight(3,1,kk,jj,ipts)
              derv2(jy,iz) = derv2(jy,iz) + Bqf*d2segweight(3,2,kk,jj,ipts)
              derv2(jz,iz) = derv2(jz,iz) + Bqf*d2segweight(3,3,kk,jj,ipts)
            else
              derv2(ix,jx) = derv2(ix,jx) + Bqf*d2segweight(1,1,kk,jj,ipts)
              derv2(iy,jx) = derv2(iy,jx) + Bqf*d2segweight(2,1,kk,jj,ipts)
              derv2(iz,jx) = derv2(iz,jx) + Bqf*d2segweight(3,1,kk,jj,ipts)
              derv2(ix,jy) = derv2(ix,jy) + Bqf*d2segweight(1,2,kk,jj,ipts)
              derv2(iy,jy) = derv2(iy,jy) + Bqf*d2segweight(2,2,kk,jj,ipts)
              derv2(iz,jy) = derv2(iz,jy) + Bqf*d2segweight(3,2,kk,jj,ipts)
              derv2(ix,jz) = derv2(ix,jz) + Bqf*d2segweight(1,3,kk,jj,ipts)
              derv2(iy,jz) = derv2(iy,jz) + Bqf*d2segweight(2,3,kk,jj,ipts)
              derv2(iz,jz) = derv2(iz,jz) + Bqf*d2segweight(3,3,kk,jj,ipts)
            endif
!
            if (i.ge.k) then
              derv2(kx,ix) = derv2(kx,ix) + Bqf*d2segweight(1,1,kk,jj,ipts)
              derv2(ky,ix) = derv2(ky,ix) + Bqf*d2segweight(2,1,kk,jj,ipts)
              derv2(kz,ix) = derv2(kz,ix) + Bqf*d2segweight(3,1,kk,jj,ipts)
              derv2(kx,iy) = derv2(kx,iy) + Bqf*d2segweight(1,2,kk,jj,ipts)
              derv2(ky,iy) = derv2(ky,iy) + Bqf*d2segweight(2,2,kk,jj,ipts)
              derv2(kz,iy) = derv2(kz,iy) + Bqf*d2segweight(3,2,kk,jj,ipts)
              derv2(kx,iz) = derv2(kx,iz) + Bqf*d2segweight(1,3,kk,jj,ipts)
              derv2(ky,iz) = derv2(ky,iz) + Bqf*d2segweight(2,3,kk,jj,ipts)
              derv2(kz,iz) = derv2(kz,iz) + Bqf*d2segweight(3,3,kk,jj,ipts)
            else
              derv2(ix,kx) = derv2(ix,kx) + Bqf*d2segweight(1,1,kk,jj,ipts)
              derv2(iy,kx) = derv2(iy,kx) + Bqf*d2segweight(1,2,kk,jj,ipts)
              derv2(iz,kx) = derv2(iz,kx) + Bqf*d2segweight(1,3,kk,jj,ipts)
              derv2(ix,ky) = derv2(ix,ky) + Bqf*d2segweight(2,1,kk,jj,ipts)
              derv2(iy,ky) = derv2(iy,ky) + Bqf*d2segweight(2,2,kk,jj,ipts)
              derv2(iz,ky) = derv2(iz,ky) + Bqf*d2segweight(2,3,kk,jj,ipts)
              derv2(ix,kz) = derv2(ix,kz) + Bqf*d2segweight(3,1,kk,jj,ipts)
              derv2(iy,kz) = derv2(iy,kz) + Bqf*d2segweight(3,2,kk,jj,ipts)
              derv2(iz,kz) = derv2(iz,kz) + Bqf*d2segweight(3,3,kk,jj,ipts)
            endif
          enddo
        enddo
      enddo
!
!  Total weight : d2wt/da.db & dwt/da x dwt/db
!
      Bqf = fact*deltaq*Bqsum/totsegweight
      Bqfs = 2.0_dp*fact*deltaq*Bqsum/(totsegweight**2)
      do jj = 2,nallnearseg
        j = nallnearsegptr(jj)
        jj2 = jj*(jj - 1)/2
        jx = 3*(j - 1) + 1
        jy = jx + 1
        jz = jx + 2
        do kk = 1,jj-1
          ind = jj2 + kk
          k = nallnearsegptr(kk)
          kx = 3*(k - 1) + 1
          ky = kx + 1
          kz = kx + 2
          if (j.ge.k) then
            derv2(kx,jx) = derv2(kx,jx) - Bqf*d2totsegweight(1,1,ind) - Bqfs*dtotsegweight(1,kk)*dtotsegweight(1,jj)
            derv2(ky,jx) = derv2(ky,jx) - Bqf*d2totsegweight(2,1,ind) - Bqfs*dtotsegweight(2,kk)*dtotsegweight(1,jj)
            derv2(kz,jx) = derv2(kz,jx) - Bqf*d2totsegweight(3,1,ind) - Bqfs*dtotsegweight(3,kk)*dtotsegweight(1,jj)
            derv2(kx,jy) = derv2(kx,jy) - Bqf*d2totsegweight(1,2,ind) - Bqfs*dtotsegweight(1,kk)*dtotsegweight(2,jj)
            derv2(ky,jy) = derv2(ky,jy) - Bqf*d2totsegweight(2,2,ind) - Bqfs*dtotsegweight(2,kk)*dtotsegweight(2,jj)
            derv2(kz,jy) = derv2(kz,jy) - Bqf*d2totsegweight(3,2,ind) - Bqfs*dtotsegweight(3,kk)*dtotsegweight(2,jj)
            derv2(kx,jz) = derv2(kx,jz) - Bqf*d2totsegweight(1,3,ind) - Bqfs*dtotsegweight(1,kk)*dtotsegweight(3,jj)
            derv2(ky,jz) = derv2(ky,jz) - Bqf*d2totsegweight(2,3,ind) - Bqfs*dtotsegweight(2,kk)*dtotsegweight(3,jj)
            derv2(kz,jz) = derv2(kz,jz) - Bqf*d2totsegweight(3,3,ind) - Bqfs*dtotsegweight(3,kk)*dtotsegweight(3,jj)
          else
            derv2(jx,kx) = derv2(jx,kx) - Bqf*d2totsegweight(1,1,ind) - Bqfs*dtotsegweight(1,kk)*dtotsegweight(1,jj)
            derv2(jy,kx) = derv2(jy,kx) - Bqf*d2totsegweight(1,2,ind) - Bqfs*dtotsegweight(1,kk)*dtotsegweight(2,jj)
            derv2(jz,kx) = derv2(jz,kx) - Bqf*d2totsegweight(1,3,ind) - Bqfs*dtotsegweight(1,kk)*dtotsegweight(3,jj)
            derv2(jx,ky) = derv2(jx,ky) - Bqf*d2totsegweight(2,1,ind) - Bqfs*dtotsegweight(2,kk)*dtotsegweight(1,jj)
            derv2(jy,ky) = derv2(jy,ky) - Bqf*d2totsegweight(2,2,ind) - Bqfs*dtotsegweight(2,kk)*dtotsegweight(2,jj)
            derv2(jz,ky) = derv2(jz,ky) - Bqf*d2totsegweight(2,3,ind) - Bqfs*dtotsegweight(2,kk)*dtotsegweight(3,jj)
            derv2(jx,kz) = derv2(jx,kz) - Bqf*d2totsegweight(3,1,ind) - Bqfs*dtotsegweight(3,kk)*dtotsegweight(1,jj)
            derv2(jy,kz) = derv2(jy,kz) - Bqf*d2totsegweight(3,2,ind) - Bqfs*dtotsegweight(3,kk)*dtotsegweight(2,jj)
            derv2(jz,kz) = derv2(jz,kz) - Bqf*d2totsegweight(3,3,ind) - Bqfs*dtotsegweight(3,kk)*dtotsegweight(3,jj)
          endif
        enddo
      enddo
!
!  Mixed partial/total weight : dw/da x dwt/db
!
      do ipts = 1,npts
        i = cosmoatomptr(ipts)
        ix = 3*(i - 1) + 1
        iy = ix + 1
        iz = iy + 1
        Bqf = - fact*deltaq*cosmoBq(ipts)/totsegweight
        do jj = 1,nnearseg(ipts)
          j = nnearsegptr(jj,ipts)
          jx = 3*(j - 1) + 1
          jy = jx + 1
          jz = jy + 1
          do kk = 1,nallnearseg
            k = nallnearsegptr(kk)
            kx = 3*(k - 1) + 1
            ky = kx + 1
            kz = kx + 2
            if (i.ge.k) then
              derv2(kx,ix) = derv2(kx,ix) + Bqf*dtotsegweight(1,kk)*dsegweight(1,jj,ipts)
              derv2(ky,ix) = derv2(ky,ix) + Bqf*dtotsegweight(2,kk)*dsegweight(1,jj,ipts)
              derv2(kz,ix) = derv2(kz,ix) + Bqf*dtotsegweight(3,kk)*dsegweight(1,jj,ipts)
              derv2(kx,iy) = derv2(kx,iy) + Bqf*dtotsegweight(1,kk)*dsegweight(2,jj,ipts)
              derv2(ky,iy) = derv2(ky,iy) + Bqf*dtotsegweight(2,kk)*dsegweight(2,jj,ipts)
              derv2(kz,iy) = derv2(kz,iy) + Bqf*dtotsegweight(3,kk)*dsegweight(2,jj,ipts)
              derv2(kx,iz) = derv2(kx,iz) + Bqf*dtotsegweight(1,kk)*dsegweight(3,jj,ipts)
              derv2(ky,iz) = derv2(ky,iz) + Bqf*dtotsegweight(2,kk)*dsegweight(3,jj,ipts)
              derv2(kz,iz) = derv2(kz,iz) + Bqf*dtotsegweight(3,kk)*dsegweight(3,jj,ipts)
            else
              derv2(ix,kx) = derv2(ix,kx) + Bqf*dtotsegweight(1,kk)*dsegweight(1,jj,ipts)
              derv2(iy,kx) = derv2(iy,kx) + Bqf*dtotsegweight(1,kk)*dsegweight(2,jj,ipts)
              derv2(iz,kx) = derv2(iz,kx) + Bqf*dtotsegweight(1,kk)*dsegweight(3,jj,ipts)
              derv2(ix,ky) = derv2(ix,ky) + Bqf*dtotsegweight(2,kk)*dsegweight(1,jj,ipts)
              derv2(iy,ky) = derv2(iy,ky) + Bqf*dtotsegweight(2,kk)*dsegweight(2,jj,ipts)
              derv2(iz,ky) = derv2(iz,ky) + Bqf*dtotsegweight(2,kk)*dsegweight(3,jj,ipts)
              derv2(ix,kz) = derv2(ix,kz) + Bqf*dtotsegweight(3,kk)*dsegweight(1,jj,ipts)
              derv2(iy,kz) = derv2(iy,kz) + Bqf*dtotsegweight(3,kk)*dsegweight(2,jj,ipts)
              derv2(iz,kz) = derv2(iz,kz) + Bqf*dtotsegweight(3,kk)*dsegweight(3,jj,ipts)
            endif
            if (j.ge.k) then
              derv2(kx,jx) = derv2(kx,jx) - Bqf*dtotsegweight(1,kk)*dsegweight(1,jj,ipts)
              derv2(ky,jx) = derv2(ky,jx) - Bqf*dtotsegweight(2,kk)*dsegweight(1,jj,ipts)
              derv2(kz,jx) = derv2(kz,jx) - Bqf*dtotsegweight(3,kk)*dsegweight(1,jj,ipts)
              derv2(kx,jy) = derv2(kx,jy) - Bqf*dtotsegweight(1,kk)*dsegweight(2,jj,ipts)
              derv2(ky,jy) = derv2(ky,jy) - Bqf*dtotsegweight(2,kk)*dsegweight(2,jj,ipts)
              derv2(kz,jy) = derv2(kz,jy) - Bqf*dtotsegweight(3,kk)*dsegweight(2,jj,ipts)
              derv2(kx,jz) = derv2(kx,jz) - Bqf*dtotsegweight(1,kk)*dsegweight(3,jj,ipts)
              derv2(ky,jz) = derv2(ky,jz) - Bqf*dtotsegweight(2,kk)*dsegweight(3,jj,ipts)
              derv2(kz,jz) = derv2(kz,jz) - Bqf*dtotsegweight(3,kk)*dsegweight(3,jj,ipts)
            else
              derv2(jx,kx) = derv2(jx,kx) - Bqf*dtotsegweight(1,kk)*dsegweight(1,jj,ipts)
              derv2(jy,kx) = derv2(jy,kx) - Bqf*dtotsegweight(1,kk)*dsegweight(2,jj,ipts)
              derv2(jz,kx) = derv2(jz,kx) - Bqf*dtotsegweight(1,kk)*dsegweight(3,jj,ipts)
              derv2(jx,ky) = derv2(jx,ky) - Bqf*dtotsegweight(2,kk)*dsegweight(1,jj,ipts)
              derv2(jy,ky) = derv2(jy,ky) - Bqf*dtotsegweight(2,kk)*dsegweight(2,jj,ipts)
              derv2(jz,ky) = derv2(jz,ky) - Bqf*dtotsegweight(2,kk)*dsegweight(3,jj,ipts)
              derv2(jx,kz) = derv2(jx,kz) - Bqf*dtotsegweight(3,kk)*dsegweight(1,jj,ipts)
              derv2(jy,kz) = derv2(jy,kz) - Bqf*dtotsegweight(3,kk)*dsegweight(2,jj,ipts)
              derv2(jz,kz) = derv2(jz,kz) - Bqf*dtotsegweight(3,kk)*dsegweight(3,jj,ipts)
            endif
          enddo
        enddo
      enddo
    endif
  endif
  if (lgrad2) then
!
!  Symmetrise second derivative matrix
!
    n3 = 3*numat
    do i = 1,n3
      do j = 1,i
        derv2(i,j) = derv2(j,i)
      enddo
    enddo
  endif
!
!  Free local memory
!
  if (ldqneeded) then
    call realloc(fullA,0_i4,0_i4,ierror)
  endif
  call realloc(dcosmoAA,0_i4,0_i4,0_i4,ierror)
  if (ldqneeded) then
    deallocate(dqsassum,stat=status)
    if (status/=0) call deallocate_error('cosmoderv','dqsassum')
    deallocate(dqsas,stat=status)
    if (status/=0) call deallocate_error('cosmoderv','dqsas')
    call realloc(dcosmoB,0_i4,0_i4,0_i4,ierror)
    call realloc(dcosmoA2,0_i4,0_i4,0_i4,ierror)
    call realloc(dcosmoA,0_i4,0_i4,0_i4,ierror)
  endif
  deallocate(d2totsegweight,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','d2totsegweight')
  deallocate(dtotsegweight,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','dtotsegweight')
  deallocate(d2segweight,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','d2segweight')
  deallocate(dsegweight,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','dsegweight')
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','xvec')
  deallocate(nearsasrptr,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','nearsasrptr')
  deallocate(nearsasptr,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','nearsasptr')
  deallocate(lnearsas,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','lnearsas')
  deallocate(lanynonunit,stat=status)
  if (status/=0) call deallocate_error('cosmoderv','lanynonunit')
!
  return
  end
