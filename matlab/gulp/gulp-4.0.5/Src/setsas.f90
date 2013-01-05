  subroutine setsas
!
!  This routine constructs or updates the solvent-accessible
!  surface (SAS). Original 0-D version was based on the algorithm
!  of A. Klamt from MOPAC. Now modified to handle periodicity,
!  rotation of frame, and smoothing by partial weights.
!
!   4/02 Exclusion of shells from SAS added
!   4/03 Shells put back to prevent possible instabilities
!  10/03 Eispack call replaced with lapack
!  11/04 Re-sizing of arrays for maxnpts modified
!  11/04 Set up of sasparticles added and nloop removed
!   1/05 Exclusion of specific sasparticles added
!   1/05 Dummy atoms specifically excluded from being able to 
!        create SAS points
!   1/05 Handling of drsolv/atsrad corrected to be like original
!        algorithm - ie so that the points near where atoms touch
!        are excluded
!   2/05 Handling of drsolv corrected again since last fix was not
!        quite right! spxyzouter added
!   3/05 For sasparticle = both case, the points are the SAS are
!        now only determined by the cores, but the potential is
!        set by both.
!  12/08 Migrated to version 3.5 and converted to f90 format
!   9/10 Handling of maxnpwtloc modified
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
!  Julian Gale, NRI, Curtin University, September 2011
!
  use configurations, only : nregionno
  use constants,      only : pi
  use control,        only : ldebug, keyword
  use cosmo
  use cosmopwtloc
  use current
  use element,        only : maxele
  use iochannels
  use parallel,       only : ioproc
  use shell,          only : ncore, ncoptr, ncsptr
  use times,          only : tcosmo
  implicit none
!
!  Local variables
!
  integer(i4)                                    :: i, j, k
  integer(i4)                                    :: ierror
  integer(i4)                                    :: ii
  integer(i4)                                    :: ind
  integer(i4)                                    :: inset
  integer(i4)                                    :: ipm
  integer(i4)                                    :: ipts
  integer(i4)                                    :: ix
  integer(i4)                                    :: ivec1
  integer(i4)                                    :: ivec2
  integer(i4)                                    :: jpts
  integer(i4)                                    :: kk
  integer(i4)                                    :: n
  integer(i4)                                    :: nn
  integer(i4)                                    :: nara
  integer(i4)                                    :: nari
  integer(i4)                                    :: nati
  integer(i4)                                    :: nc
  integer(i4),                              save :: ncflast = 0
  integer(i4), dimension(:),   allocatable, save :: nfoundptr
  integer(i4)                                    :: nfound
  integer(i4)                                    :: npts0
  integer(i4)                                    :: nptsl
  integer(i4)                                    :: nptsu
  integer(i4)                                    :: nptsin
  integer(i4)                                    :: nptshin
  integer(i4)                                    :: nptstot
  integer(i4)                                    :: nsetfi
  integer(i4)                                    :: status
  logical,     dimension(:),   allocatable, save :: delete
  logical,     dimension(:),   allocatable, save :: ldone
  logical,     dimension(:),   allocatable, save :: lInSAS
  logical                                        :: lequalsi
  logical                                        :: lfirstcall
  logical                                        :: liniteverytime
  logical                                        :: lnotransform
  logical                                        :: lpartial
  real(dp),    dimension(:,:), allocatable, save :: rotatedtm
  real(dp),    dimension(:),   allocatable, save :: oldcosmoatomptr
  real(dp),    dimension(:,:), allocatable, save :: oldsas
  real(dp),    dimension(:),   allocatable, save :: pwt
  real(dp)                                       :: areafactor
  real(dp)                                       :: atomarea
  real(dp)                                       :: cos2ds
  real(dp)                                       :: dotprod(3)
  real(dp)                                       :: dpmax1
  real(dp)                                       :: dpmax2
  real(dp)                                       :: ds
  real(dp)                                       :: dwdr
  real(dp)                                       :: d2wdr2
  real(dp)                                       :: eig(3)
  real(dp)                                       :: fv1(9)
  real(dp)                                       :: moments(3,3)
  real(dp)                                       :: originx
  real(dp)                                       :: originy
  real(dp)                                       :: originz
  real(dp)                                       :: r
  real(dp)                                       :: r2
  real(dp)                                       :: ri
  real(dp)                                       :: ridr
  real(dp)                                       :: rarea
  real(dp)                                       :: rr
  real(dp)                                       :: rsign
  real(dp)                                       :: sdis
  real(dp)                                       :: sdis0
  real(dp)                                       :: sp_max
  real(dp)                                       :: ssp
  real(dp)                                       :: switchfn
  real(dp)                                       :: t1
  real(dp)                                       :: t2
  real(dp)                                       :: toler
  real(dp)                                       :: totalarea
  real(dp)                                       :: xc, yc, zc
  real(dp)                                       :: xi, yi, zi
  real(dp)                                       :: xj, yj, zj
  real(dp)                                       :: xjk, yjk, zjk
  real(dp)                                       :: xo, yo, zo
  real(dp)                                       :: xt, yt, zt
  real(dp)                                       :: xyz(3)
!
!  Functions
!
  real(dp)                                       :: cputime
!
!  Initialise timer for this routine
!
  t1 = cputime()
  toler = 1.0d-5
!
!  Set initialisation flag
!
  liniteverytime = (index(keyword,'nosa').eq.0)
  lnotransform   = (index(keyword,'notr').ne.0)
!
!  In case number of atoms is less than original maxat call changemaxat
!
  if (ncf.ne.ncflast) call changemaxat
!
!  Set first call flag
!
  lfirstcall = (ncf.ne.ncflast.or.liniteverytime)
  ncflast = ncf
!
!  Handle local memory that must be saved between calls
!
  if (lfirstcall) then
    if (allocated(rotatedtm)) then
      deallocate(rotatedtm,stat=status)
      if (status/=0) call deallocate_error('setsas','rotatedtm')
    endif
    allocate(rotatedtm(3,nppa),stat=status)
    if (status/=0) call outofmemory('setsas','rotatedtm')
  endif
!
!  Allocate local scratch memory
!
  nptstot = max(maxnpts,maxnptsh)
  nptstot = max(npts,nptstot)
  nptstot = max(nptsh,nptstot)
  if (nptstot.gt.maxnptstot) then
    maxnptstot = nptstot
    call changemaxnptstot
  endif
  if (maxnppa*numat/2.gt.maxnset) then
    maxnset = maxnppa*numat/2
    call changemaxnset
  endif
  allocate(nfoundptr(nppa),stat=status)
  if (status/=0) call outofmemory('setsas','nfoundptr')
  allocate(pwt(nppa),stat=status)
  if (status/=0) call outofmemory('setsas','pwt')
  allocate(lInSAS(nppa),stat=status)
  if (status/=0) call outofmemory('setsas','lInSAS')
  if (.not.lfirstcall) then
    allocate(oldcosmoatomptr(npts),stat=status)
    if (status/=0) call outofmemory('setsas','oldcosmoatomptr')
    allocate(oldsas(3,npts),stat=status)
    if (status/=0) call outofmemory('setsas','oldsas')
  endif
!
!  Set up cell vector lists
!
  call rlist
!
!  Save number of points per atomic sphere
!
  if (lfirstcall) then
    nptshin = nspah
    nptsin = nspa
  else
    nptshin = nptsh
    nptsin = npts
  endif
!
!  Save details of old SAS
!
  if (.not.lfirstcall) then
    do i = 1,nptsin
      oldcosmoatomptr(i) = cosmoatomptr(i)
      oldsas(1:3,i) = sas(1:3,i)
    enddo
  endif
  if (lfirstcall.and.ndim.eq.0) then
!
!  Find centre of system
!
    originx = 0.0_dp
    originy = 0.0_dp
    originz = 0.0_dp
    do nc = 1,ncore
      i = ncoptr(nc)
      originx = originx + xclat(i)
      originy = originy + yclat(i)
      originz = originz + zclat(i)
    enddo
    originx = originx/dble(ncore)
    originy = originy/dble(ncore)
    originz = originz/dble(ncore)
    if (.not.lcosmoeigin(ncf)) then
!
!  Calculate moments about centre
!
      moments(1:3,1:3) = 0.0_dp
      do nc = 1,ncore
        i = ncoptr(nc)
        xc = xclat(i) - originx
        yc = yclat(i) - originy
        zc = zclat(i) - originz
        moments(1,1) = moments(1,1) + xc*xc
        moments(2,1) = moments(2,1) + yc*xc
        moments(3,1) = moments(3,1) + zc*xc
        moments(2,2) = moments(2,2) + yc*yc
        moments(3,2) = moments(3,2) + zc*yc
        moments(3,3) = moments(3,3) + zc*zc
      enddo
      moments(1,2) = moments(2,1)
      moments(1,3) = moments(3,1)
      moments(2,3) = moments(3,2)
!
!  Find eigensystem of moments
!
      cosmoeigen(1:3,1:3,ncf) = moments(1:3,1:3)
      call dsyev('V','U',3_i4,cosmoeigen(1,1,ncf),3_i4,eig,fv1,9_i4,ierror)
      lcosmoeigin(ncf) = .true.
!
!  Debugging output
!
      if (ldebug.and.ioproc) then
        write(ioout,'(/)')
        write(ioout,'(''  COSMO transformation matrix and eigenvalues :'',/)')
        do i = 1,3
          write(ioout,'(''  No. '',i1,'' E = '',f12.6,'' TM = '',3(f12.6,1x))') i,eig(i),(cosmoeigen(j,i,ncf),j=1,3)
        enddo
        write(ioout,'(/)')
      endif
    endif
  endif
!
!  Build SAS
!
  sdis = 0.0_dp
  totalarea = 0.0_dp
  areafactor = 4.0_dp*pi/nppa
  inset = 1
  npts = 0
  nptsu = 1
!
!  Build list of atoms that interact with and control the position of the SAS
!
!  There are two lists:
!
!  nsasparticles     - particles that interact with the SAS
!  nsasparticlespart - particles that can give rise to points on the SAS
!
!  with nsasparticlespart being a subset of nsasparticles
!
  if (isasatomoption.eq.1) then
    if (ncore.gt.maxsasparticles) then
      maxsasparticles = ncore
      call changemaxsasparticles
    endif
    nsasparticles = 0
    do nc = 1,ncore
      i = ncoptr(nc)
      nsasparticles = nsasparticles + 1
      nsasparticleptr(nsasparticles) = i
      if (ncsptr(i).gt.0) then
        qsasparticles(nsasparticles) = qf(i) + qf(ncsptr(i))
      else
        qsasparticles(nsasparticles) = qf(i) 
      endif
    enddo
    if (ncore.gt.maxsasparticlespart) then
      maxsasparticlespart = ncore
      call changemaxsasparticlespart
    endif
    nsasparticlespart = 0
    do nc = 1,ncore
      i = ncoptr(nc)
      if ((i.lt.nsasexcludemin(ncf).or.i.gt.nsasexcludemax(ncf)).and.nat(i).ne.maxele) then
        nsasparticlespart = nsasparticlespart + 1
        nsasparticlepartptr(nsasparticlespart) = i
      endif
    enddo
  elseif (isasatomoption.eq.2) then
    if (numat.gt.maxsasparticles) then
      maxsasparticles = numat
      call changemaxsasparticles
    endif
    nsasparticles = 0
    do i = 1,numat
      nsasparticles = nsasparticles + 1
      nsasparticleptr(nsasparticles) = i
      qsasparticles(nsasparticles) = qf(i)
    enddo
    if (numat.gt.maxsasparticlespart) then
      maxsasparticlespart = numat
      call changemaxsasparticlespart
    endif
    nsasparticlespart = 0
    do i = 1,numat
      if ((i.lt.nsasexcludemin(ncf).or.i.gt.nsasexcludemax(ncf)).and.nat(i).lt.maxele) then
        nsasparticlespart = nsasparticlespart + 1
        nsasparticlepartptr(nsasparticlespart) = i
      endif
    enddo
  endif
!
!  Build points
!
  buildloop: do nc = 1,nsasparticlespart
    i = nsasparticlepartptr(nc)
    nati = nat(i)
    if (nati.gt.maxele) nati = nati - maxele
    if (nregionno(nsft+nrelat(i)).gt.1) cycle buildloop
    ds = sqrt(4.0_dp/nspa)
    if (nati.eq.1) ds = 2.0_dp*ds
    cos2ds = cos(2.0_dp*ds)
    atomarea = 0.0_dp
    ri = atsrad(i) 
    ridr = ri + drsolv
    xi = xclat(i)
    yi = yclat(i)
    zi = zclat(i)
    npts0 = npts + 1
    if (.not.lfirstcall) then
!
!  Find range of old SAS points that relate to atom i
!
!  nptsl = lower bound
!  nptsu = upper bound + 1
!
      nptsl = nptsu
      lequalsi = .true.
      ipts = nptsl - 1
      do while (lequalsi.and.ipts.lt.nptsin)
        ipts = ipts + 1
        lequalsi = (oldcosmoatomptr(ipts).eq.i)
      enddo
      if (lequalsi) then
        nptsu = ipts + 1
      else
        nptsu = ipts
      endif
!
!  Transform solvent accessible surface according to cosmotm(inv)
!
      do j = nptsl,nptsu-1
        xc = oldsas(1,j)
        yc = oldsas(2,j)
        zc = oldsas(3,j)
        oldsas(1,j) = xc*cosmotm(1,1,i) + yc*cosmotm(1,2,i) + zc*cosmotm(1,3,i)
        oldsas(2,j) = xc*cosmotm(2,1,i) + yc*cosmotm(2,2,i) + zc*cosmotm(2,3,i)
        oldsas(3,j) = xc*cosmotm(3,1,i) + yc*cosmotm(3,2,i) + zc*cosmotm(3,3,i)
      enddo
    endif
    if (lfirstcall) then
      if (ndim.eq.0) then
!
!  Use the system moment eigenvectors to generate transformation matrix
!  for each atom. Basically the same as the eigenvectors, but with the
!  signs adjusted so that the frame is rotated towards the centre of the
!  system.
!
        xo = xi - originx
        yo = yi - originy
        zo = zi - originz
!
!  Calculate dot product of vector to the origin with moment eigenvectors
!
        do ix = 1,3
          dotprod(ix) = xo*cosmoeigen(1,ix,ncf) + yo*cosmoeigen(2,ix,ncf) + zo*cosmoeigen(3,ix,ncf)
        enddo
!
!  Select eigenvectors that have the maximum overlap with the origin vector
!
        ivec1 = 3
        ivec2 = 2
        dpmax1 = - 1.0_dp
        dpmax2 = - 1.0_dp
        do ix = 3,1,-1
          if (abs(dotprod(ix)).gt.dpmax1) then
            dpmax1 = abs(dotprod(ix))
            ivec1 = ix
          elseif (abs(dotprod(ix)).gt.dpmax2) then
            dpmax2 = abs(dotprod(ix))
            ivec2 = ix
          endif
        enddo
!
!  Construct transformation matrix
!
        rsign = sign(1.0_dp,dotprod(ivec1))
        cosmotm(1,1,i) = cosmoeigen(1,ivec1,ncf)*rsign
        cosmotm(1,2,i) = cosmoeigen(2,ivec1,ncf)*rsign
        cosmotm(1,3,i) = cosmoeigen(3,ivec1,ncf)*rsign
!
        rsign = sign(1.0_dp,dotprod(ivec2))
        cosmotm(2,1,i) = cosmoeigen(1,ivec2,ncf)*rsign
        cosmotm(2,2,i) = cosmoeigen(2,ivec2,ncf)*rsign
        cosmotm(2,3,i) = cosmoeigen(3,ivec2,ncf)*rsign
!
        cosmotm(3,1,i) = cosmotm(1,2,i)*cosmotm(2,3,i) - cosmotm(2,2,i)*cosmotm(1,3,i)
        cosmotm(3,2,i) = cosmotm(1,3,i)*cosmotm(2,1,i) - cosmotm(2,3,i)*cosmotm(1,1,i)
        cosmotm(3,3,i) = cosmotm(1,1,i)*cosmotm(2,2,i) - cosmotm(2,1,i)*cosmotm(1,2,i)
        if (lnotransform) then
          cosmotm(1:3,1:3,i) = 0.0_dp
          cosmotm(1,1,i) = 1.0_dp
          cosmotm(2,2,i) = 1.0_dp
          cosmotm(3,3,i) = 1.0_dp
        endif
      else
!
!  For surfaces and solids there is no transformation matrix at the moment.
!
        cosmotm(1:3,1:3,i) = 0.0_dp
        cosmotm(1,1,i) = 1.0_dp
        cosmotm(2,2,i) = 1.0_dp
        cosmotm(3,3,i) = 1.0_dp
      endif
    endif
!
!  Transform sphere2 according to tm
!
    do j = 1,nppa
      xc = sphere2(1,j)
      yc = sphere2(2,j)
      zc = sphere2(3,j)
      do ix = 1,3
        rotatedtm(ix,j) = xc*cosmotm(1,ix,i) + yc*cosmotm(2,ix,i) + zc*cosmotm(3,ix,i)
      enddo
    enddo
    if (.not.lfirstcall) then
!
!  Add old SAS points on to new list in transformed form
!
      do j = nptsl,nptsu-1
        npts = npts + 1
        if (npts .gt. maxnpts) then
          maxnpts = npts + 50
          call changemaxnpts
        endif
        cosmoatomptr(npts) = i
        xc = oldsas(1,j)
        yc = oldsas(2,j)
        zc = oldsas(3,j)
        sas(1,npts) = xc*cosmotm(1,1,i) + yc*cosmotm(2,1,i) + zc*cosmotm(3,1,i)
        sas(2,npts) = xc*cosmotm(1,2,i) + yc*cosmotm(2,2,i) + zc*cosmotm(3,2,i)
        sas(3,npts) = xc*cosmotm(1,3,i) + yc*cosmotm(2,3,i) + zc*cosmotm(3,3,i)
      enddo
    else
!
!  Add SAS points on to new list in transformed form
!
      if (nati.eq.1) then
!
!  Hydrogen atom
!
        if (npts+nptshin.ge.maxnpts) then
          maxnpts = npts + 5*nptshin
          call changemaxnpts
        endif
        do j = 1,nptshin
          npts = npts + 1
          cosmoatomptr(npts) = i
          xc = sphere1h(1,j)
          yc = sphere1h(2,j)
          zc = sphere1h(3,j)
          sas(1,npts) = xc*cosmotm(1,1,i) + yc*cosmotm(2,1,i) + zc*cosmotm(3,1,i)
          sas(2,npts) = xc*cosmotm(1,2,i) + yc*cosmotm(2,2,i) + zc*cosmotm(3,2,i)
          sas(3,npts) = xc*cosmotm(1,3,i) + yc*cosmotm(2,3,i) + zc*cosmotm(3,3,i)
        enddo
      else
!
!  Non-hydrogen atom
!
        if (npts+nptsin .ge. maxnpts) then
          maxnpts = npts + 5*nptsin
          call changemaxnpts
        endif
        do j = 1,nptsin
          npts = npts + 1
          cosmoatomptr(npts) = i
          xc = sphere1(1,j)
          yc = sphere1(2,j)
          zc = sphere1(3,j)
          sas(1,npts) = xc*cosmotm(1,1,i) + yc*cosmotm(2,1,i) + zc*cosmotm(3,1,i)
          sas(2,npts) = xc*cosmotm(1,2,i) + yc*cosmotm(2,2,i) + zc*cosmotm(3,2,i)
          sas(3,npts) = xc*cosmotm(1,3,i) + yc*cosmotm(2,3,i) + zc*cosmotm(3,3,i)
        enddo
      endif
    endif
!
!  Initialise partial weighted point to atom pointer
!
    npwtloc(1:nppa,i) = 0_i4
!
!  Construct the SAS grid
!
!  lInSAS  - indicates whether point is part of SAS
!  cosmowt - stores the weight of point in SAS according to the switching function
!
    rarea = 0.0_dp
    do j = 1,nppa
      xj = xi + rotatedtm(1,j)*ridr
      yj = yi + rotatedtm(2,j)*ridr
      zj = zi + rotatedtm(3,j)*ridr
      kk = 0
      lInSAS(j) = .true.
      cosmowt(j,i) = 1.0_dp
      pwt(j) = 1.0_dp
      do while (lInSAS(j).and.kk.lt.nsasparticlespart)
        kk = kk + 1
        k = nsasparticlepartptr(kk)
        xjk = xclat(k) - xj
        yjk = yclat(k) - yj
        zjk = zclat(k) - zj
        ii = 0
        lpartial = .false.
        do while (lInSAS(j).and.ii.lt.iimax)
          ii = ii + 1
          if (k.ne.i.or.ii.ne.iimid) then
            r2 = (xjk + xvec1cell(ii))**2 + (yjk + yvec1cell(ii))**2 + (zjk + zvec1cell(ii))**2
            r = sqrt(r2) - atsrad(k) - drsolv - cosmorange
            if (r .lt. -cosmorange) then
              lInSAS(j) = .false.
              cosmowt(j,i) = 0.0_dp
            elseif (r .lt. 0.0_dp) then
              lpartial = .true.
              call switch(abs(r),switchfn,.false.,dwdr,.false.,d2wdr2)
              cosmowt(j,i) = cosmowt(j,i)*switchfn
            endif
          endif
        enddo
        if (lpartial) then
          npwtloc(j,i) = npwtloc(j,i) + 1
          if (npwtloc(j,i).gt.maxnpwtloc) then
            maxnpwtloc = npwtloc(j,i) + 2
            call changemaxnpwtloc
          endif
          npwtptrloc(npwtloc(j,i),j,i) = k
        endif
      enddo
!
!  Compute area as sum of weights
!
      if (lInSAS(j)) rarea = rarea + cosmowt(j,i)
    enddo
    atomarea = rarea*ridr*ridr
    totalarea = totalarea + atomarea
100 continue
!
!  Check for duplicate segments
!
    allocate(delete(npts-npts0+1),stat=status)
    if (status/=0) call outofmemory('setsas','delete')
    delete(1:npts-npts0+1) = .false.
    do ipts = npts0,npts-1
      if (.not.delete(ipts-npts0+1)) then
        do jpts = ipts+1,npts
          if (.not.delete(jpts-npts0+1)) then
            r2 = ((sas(1,ipts)-sas(1,jpts))**2 + &
                  (sas(2,ipts)-sas(2,jpts))**2 + &
                  (sas(3,ipts)-sas(3,jpts))**2)
            if (r2.lt.toler) delete(jpts-npts0+1) = .true.
          endif
        enddo
      endif
    enddo
    jpts = npts0 - 1
    do ipts = npts0,npts
      if (.not.delete(ipts-npts0+1)) then
        jpts = jpts + 1
        sas(1,jpts) = sas(1,ipts)
        sas(2,jpts) = sas(2,ipts)
        sas(3,jpts) = sas(3,ipts)
      endif
    enddo
    npts = jpts
    deallocate(delete,stat=status)
    if (status/=0) call deallocate_error('setsas','delete')
!
!  Group nppa points together into segments
!
    sdis0 = sdis
    if (npts .gt. maxnptstot) then
      maxnptstot = npts
      call changemaxnptstot
    endif
    nar(npts0:npts) = 0
    spxyz(1:3,npts0:npts) = 0.0_dp
    do j = 1,nppa
      if (lInSAS(j)) then
        xt = rotatedtm(1,j)
        yt = rotatedtm(2,j)
        zt = rotatedtm(3,j)
        nfound = 0
        sp_max = - 1.0_dp
        do ipts = npts0,npts
          ssp = xt*sas(1,ipts) + yt*sas(2,ipts) + zt*sas(3,ipts)
          if (ssp .ge. (sp_max+toler)) then
            sp_max = ssp
            nfound = 1
            nfoundptr(nfound) = ipts
          elseif (ssp .ge. (sp_max-toler)) then
            nfound = nfound + 1
            nfoundptr(nfound) = ipts
          endif
        enddo
        if (sp_max .lt. cos2ds) then
!
!  No existing segment point close enough to present sub-point
!  => add current subpoint as new segment
!
          npts = npts + 1
          if (npts .gt. maxnpts) then
            maxnpts = npts + 50
            call changemaxnpts
          endif
          sas(1,npts) = rotatedtm(1,j)
          sas(2,npts) = rotatedtm(2,j)
          sas(3,npts) = rotatedtm(3,j)
          cosmoatomptr(npts) = i
          goto 100
        endif
        pwt(j) = 1.0_dp/dble(nfound)
        do ipts = 1,nfound
          ipm = nfoundptr(ipts)
          nar(ipm) = nar(ipm) + 1
          spxyz(1,ipm) = spxyz(1,ipm) + rotatedtm(1,j)*pwt(j)
          spxyz(2,ipm) = spxyz(2,ipm) + rotatedtm(2,j)*pwt(j)
          spxyz(3,ipm) = spxyz(3,ipm) + rotatedtm(3,j)*pwt(j)
        enddo
      endif
    enddo
    sdis = 0.0_dp
    ipts = npts0 - 1
!
!  No points for this atom so skip the remainder of the working
!
    if (npts.eq.ipts) cycle buildloop
    if (npts.lt.ipts) goto 100
!
!  Eliminate segment points with no sub-points
!
    do while (ipts.lt.npts) 
      ipts = ipts + 1
      do while (nar(ipts).eq.0)
        npts = npts - 1
        if (npts.lt.ipts) goto 100
        do jpts = ipts,npts
          nar(jpts)   = nar(jpts+1)
          spxyz(1,jpts) = spxyz(1,jpts+1)
          spxyz(2,jpts) = spxyz(2,jpts+1)
          spxyz(3,jpts) = spxyz(3,jpts+1)
        enddo
      enddo
      r2 = spxyz(1,ipts)*spxyz(1,ipts) + spxyz(2,ipts)*spxyz(2,ipts) + spxyz(3,ipts)*spxyz(3,ipts)
      sdis = sdis + r2
      rr = 1.0_dp/sqrt(r2)
      sas(1,ipts) = spxyz(1,ipts)*rr
      sas(2,ipts) = spxyz(2,ipts)*rr
      sas(3,ipts) = spxyz(3,ipts)*rr
    enddo
!
    if (abs(sdis-sdis0) .gt. 1.d-5) go to 100
    do ipts = npts0,npts
      nsetf(ipts) = inset
      inset = inset + nar(ipts)
      nar(ipts) = 0
      spxyz(1,ipts) = xi + sas(1,ipts)*ri
      spxyz(2,ipts) = yi + sas(2,ipts)*ri
      spxyz(3,ipts) = zi + sas(3,ipts)*ri
      spxyzouter(1,ipts) = xi + sas(1,ipts)*ridr
      spxyzouter(2,ipts) = yi + sas(2,ipts)*ridr
      spxyzouter(3,ipts) = zi + sas(3,ipts)*ridr
    enddo
    do j = 1,nppa
      if (lInSAS(j)) then
        xt = rotatedtm(1,j)
        yt = rotatedtm(2,j)
        zt = rotatedtm(3,j)
        nfound = 0
        sp_max = -1.0_dp
        do ipts = npts0,npts
          ssp = xt*sas(1,ipts) + yt*sas(2,ipts) + zt*sas(3,ipts)
          if (ssp.ge.(sp_max+toler)) then
            sp_max = ssp
            nfound = 1
            nfoundptr(nfound) = ipts
          elseif (ssp.ge.(sp_max-toler)) then
            nfound = nfound + 1
            nfoundptr(nfound) = ipts
          endif
        enddo
        if (sp_max.ge.cos2ds) then
          pwt(j) = 1.0_dp/dble(nfound)
          do ipts = 1,nfound
            ipm = nfoundptr(ipts)
            nara = nar(ipm)
            ind = nsetf(ipm) + nara
            if (ind.gt.maxnset) then
              maxnset = ind + 50
              call changemaxnset
            endif
            nset(ind) = j
            cosmopwt(ind) = pwt(j)
            nar(ipm) = nara + 1
          enddo
        endif
      endif
    enddo
!
!  Debuging information for atoms
!
    if (ldebug.and.ioproc) then
      write(ioout,'(''  Atom = '',i5,'' : Area = '',f12.6,'' Angs**2'')') i,atomarea*areafactor
      write(ioout,'(''  Transformation matrix : '')')
      write(ioout,'(3(2x,f9.6))')(cosmotm(j,1,i),j=1,3)
      write(ioout,'(3(2x,f9.6))')(cosmotm(j,2,i),j=1,3)
      write(ioout,'(3(2x,f9.6))')(cosmotm(j,3,i),j=1,3)
    endif
!
!  End of loop over atoms
!
  enddo buildloop
!
!  Transform weight pointer
!
  allocate(ldone(numat),stat=status)
  if (status/=0) call outofmemory('setsas','ldone')
  do ipts = 1,npts
    i = cosmoatomptr(ipts)
    nari = nar(ipts)
    nsetfi = nsetf(ipts)
    npwt(ipts) = 0
    ldone(1:numat) = .false.
    do k = 1,nari
      j = nset(k+nsetfi-1)
      do n = 1,npwtloc(j,i)
        nn = npwtptrloc(n,j,i)
        if (.not.ldone(nn)) then
          npwt(ipts) = npwt(ipts) + 1  
          if (npwt(ipts).gt.maxnpwt) then
            maxnpwt = npwt(ipts) + 2
            call changemaxnpwt
          endif
          npwtptr(npwt(ipts),ipts) = nn
          ldone(nn) = .true.
        endif
      enddo
    enddo
  enddo
  deallocate(ldone,stat=status)
  if (status/=0) call deallocate_error('setsas','ldone')
!
  totalarea = totalarea*areafactor
  if (ldebug.and.ioproc) then
    write(ioout,'(''  Total  '',5x,''        = '',f12.6,'' Angs**2'',/)') totalarea
    write(ioout,'(''  Segments and Points per atom : '',/)')
    do i = 1,numat
      nptsl = 0
      nptsu = 0
      do ipts = 1,npts
        if (cosmoatomptr(ipts).eq.i) then
          nptsl = nptsl + 1
          nptsu = nptsu + nar(ipts)
        endif
      enddo
      write(ioout,'(i6,4x,i8,4x,i8)') i,nptsl,nptsu
    enddo
    write(ioout,'(/,''  Total number of segments on surface = '',i8,/)') npts
    write(ioout,'('' Segments on Solvent Accessible Surface : '',/)')
    write(ioout,'('' Segment    '',9x,''X'',12x,''Y'',12x,''Z'')')
    write(ioout,'('' -------    '',9x,''-'',12x,''-'',12x,''-'')')
    do ipts = 1,npts
      write(ioout,'(2x,i5,5x,1x,3(1x,f12.6))')ipts,spxyz(1,ipts),spxyz(2,ipts),spxyz(3,ipts)
    enddo
    write(ioout,'(/)')
    write(ioout,'('' Points on Solvent Accessible Surface : '',/)')
    write(ioout,'('' Segment    Point '',9x,''X'',12x,''Y'',12x,''Z'',12x,''W'')')
    write(ioout,'('' -------    ----- '',9x,''-'',12x,''-'',12x,''-'',12x,''-'')')
    do ipts = 1,npts
      i = cosmoatomptr(ipts)
      ri = atsrad(i)
      do k = nsetf(ipts),nsetf(ipts)+nar(ipts)-1
        xc = sphere2(1,nset(k))*ri
        yc = sphere2(2,nset(k))*ri
        zc = sphere2(3,nset(k))*ri
        do ix = 1,3
          xyz(ix) = xc*cosmotm(1,ix,i) + yc*cosmotm(2,ix,i) + zc*cosmotm(3,ix,i)
        enddo
        xyz(1) = xyz(1) + xclat(i)
        xyz(2) = xyz(2) + yclat(i)
        xyz(3) = xyz(3) + zclat(i)
        write(ioout,'(2x,i5,5x,i5,2x,4(1x,f12.6))') &
          ipts,k,xyz(1),xyz(2),xyz(3),cosmowt(nset(k),i)*cosmopwt(k)
      enddo
    enddo
    write(ioout,'(/)')
  endif
!
!  Free local memory no longer needed
!
  if (.not.lfirstcall) then
    deallocate(oldsas,stat=status)
    if (status/=0) call deallocate_error('setsas','oldsas')
    deallocate(oldcosmoatomptr,stat=status)
    if (status/=0) call deallocate_error('setsas','oldcosmoatomptr')
  endif
  deallocate(lInSAS,stat=status)
  if (status/=0) call deallocate_error('setsas','lInSAS')
  deallocate(pwt,stat=status)
  if (status/=0) call deallocate_error('setsas','pwt')
  deallocate(nfoundptr,stat=status)
  if (status/=0) call deallocate_error('setsas','nfoundptr')
!
!  Finalise timer for this routine
!
  t2 = cputime()
  tcosmo = tcosmo + t2 - t1
!
  return
  end
