  subroutine updatesas
!
!  This routine updates the solvent-accessible surface (SAS). 
!  Original 0-D version was based on the algorithm of A. Klamt 
!  from MOPAC. Now modified to handle periodicity,
!  rotation of frame, and smoothing by partial weights.
!
!   4/03 Option to use cores or shells or both to create SAS added
!  11/04 nloop replaced with sasparticle arrays
!   1/05 Handling of drsolv return to original algorithm
!   2/05 Correction made to handling of drsolv
!   2/05 spxyzouter added
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
!  Julian Gale, NRI, Curtin University, September 2010
!
  use configurations, only : nregionno
  use constants,      only : pi
  use control,        only : ldebug
  use cosmo
  use cosmopwtloc
  use current
  use element,        only : maxele
  use iochannels
  use parallel,       only : ioproc
  use times,          only : tcosmo
  implicit none
!
!  Local variables
!
  integer(i4)                                    :: i, j, k
  integer(i4)                                    :: ii
  integer(i4)                                    :: ind
  integer(i4)                                    :: inset
  integer(i4)                                    :: ipm
  integer(i4)                                    :: ipts
  integer(i4)                                    :: ix
  integer(i4)                                    :: jpts
  integer(i4)                                    :: kk
  integer(i4)                                    :: n
  integer(i4)                                    :: nati
  integer(i4)                                    :: nc
  integer(i4)                                    :: nn
  integer(i4)                                    :: nara
  integer(i4)                                    :: nari
  integer(i4)                                    :: nsetfi
  integer(i4)                                    :: nfound
  integer(i4), dimension(:),   allocatable, save :: nfoundptr
  integer(i4)                                    :: npts0
  integer(i4)                                    :: nptsl
  integer(i4)                                    :: nptsu
  integer(i4)                                    :: nptsin
  integer(i4)                                    :: nptshin
  integer(i4)                                    :: nptstot
  integer(i4)                                    :: status
  logical,     dimension(:),   allocatable, save :: delete
  logical,     dimension(:),   allocatable, save :: ldone
  logical,     dimension(:),   allocatable, save :: lInSAS
  logical                                        :: lpartial
  real(dp),    dimension(:,:), allocatable, save :: rotatedtm
  real(dp)                                       :: areafactor
  real(dp)                                       :: atomarea
  real(dp)                                       :: cos2ds
  real(dp)                                       :: cosmorange2
  real(dp)                                       :: ds
  real(dp)                                       :: dwdr
  real(dp)                                       :: d2wdr2
  real(dp)                                       :: r
  real(dp)                                       :: r2
  real(dp)                                       :: rr
  real(dp)                                       :: ri
  real(dp)                                       :: ridr
  real(dp)                                       :: rarea
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
  real(dp)                                       :: xt, yt, zt
  real(dp),    dimension(:),   allocatable, save :: pwt
!
!  Functions
!
  real(dp)                                       :: cputime
!
!  Initialise timer for this routine
!
  t1 = cputime()
  toler = 1.0d-4
!
!  Allocate local scratch memory
!
  nptstot = max(maxnpts,maxnptsh)
  if (nptstot.gt.maxnptstot) then
    maxnptstot = nptstot
    call changemaxnptstot
  endif
  if (maxnppa*numat/2.gt.maxnset) then
    maxnset = maxnppa*numat/2
    call changemaxnset
  endif
  allocate(nfoundptr(nppa),stat=status)
  if (status/=0) call outofmemory('updatesas','nfoundptr')
  allocate(lInSAS(nppa),stat=status)
  if (status/=0) call outofmemory('updatesas','lInSAS')
  allocate(pwt(nppa),stat=status)
  if (status/=0) call outofmemory('updatesas','pwt')
  allocate(rotatedtm(3,nppa),stat=status)
  if (status/=0) call outofmemory('updatesas','rotatedtm')
!
!  Set up cell vector lists
!
  call rlist
!
!  Save number of points per atomic sphere
!
  nptshin = nptsh
  nptsin = npts
!
!  Build SAS
!
  sdis = 0.0_dp
  totalarea = 0.0_dp
  areafactor = 4.0_dp*pi/nppa
  cosmorange2 = cosmorange
  inset = 1
  npts = 0
  nptsu = 1
!
  buildloop: do nc = 1,nsasparticles
    i = nsasparticleptr(nc)
    nati = nat(i)
    if (nati.gt.maxele) nati = nati - maxele
    if (nregionno(nsft+nrelat(i)).gt.1) cycle buildloop
    ds = sqrt(4.0_dp/nspa)
    if (nati .eq. 1) ds = 2.0_dp*ds
    cos2ds = cos(2.0_dp*ds)
    atomarea = 0.0_dp
    ri = atsrad(i)
    ridr = ri + drsolv
    xi = xclat(i)
    yi = yclat(i)
    zi = zclat(i)
    npts0 = npts + 1
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
!
!  Add SAS points on to new list in transformed form
!
    if (nat(i).eq.1) then
!
!  Hydrogen atom
!
      if (npts+nptshin .ge. maxnpts) then
        maxnpts = npts + nptshin
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
        maxnpts = npts + nptsin
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
!
!  Initialise partial weighted point to atom pointer
!
    npwtloc(1:nppa,i) = 0_i4
!
!  Find the points of the basic grid on the SAS
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
      do while (lInSAS(j).and.kk.lt.nsasparticles)
        kk = kk + 1
        k = nsasparticleptr(kk)
        xjk = xclat(k) - xj
        yjk = yclat(k) - yj
        zjk = zclat(k) - zj
        ii = 0
        lpartial = .false.
        do while (lInSAS(j).and.ii.lt.iimax)
          ii = ii + 1
          if (k.ne.i.or.ii.ne.iimid) then
            r2 = (xjk + xvec1cell(ii))**2 + (yjk + yvec1cell(ii))**2 + (zjk + zvec1cell(ii))**2
            r = sqrt(r2) - atsrad(k) - drsolv - cosmorange2
            if (r .lt. -cosmorange) then                                                         
              lInSAS(j) = .false.                                                                   
              cosmowt(j,i) = 0.0_dp                                                                 
            elseif (r .lt. 0.0_dp) then                                                          
              lpartial = .true.
              call switch(abs(r),switchfn,.false.,dwdr,.false.,d2wdr2)                                          
              cosmowt(j,i) = cosmowt(j,i) * switchfn                                                
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
    if (status/=0) call outofmemory('updatesas','delete')
    delete(1:npts-npts0+1) = .false.
    do ipts = npts0,npts-1
      if (.not.delete(ipts-npts0+1)) then
        do jpts = ipts+1,npts
          if (.not.delete(jpts-npts0+1)) then
            r2 = ((sas(1,ipts)-sas(1,jpts))**2 + (sas(2,ipts)-sas(2,jpts))**2 + (sas(3,ipts)-sas(3,jpts))**2)
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
    if (status/=0) call deallocate_error('updatesas','delete')
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
        sp_max = -1.0_dp
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
          npts = npts+1
          if (npts .gt. maxnpts) then
            maxnpts = npts + 20
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
          nar(jpts) = nar(jpts+1)
          spxyz(1,jpts) = spxyz(1,jpts+1)
          spxyz(2,jpts) = spxyz(2,jpts+1)
          spxyz(3,jpts) = spxyz(3,jpts+1)
        enddo
      enddo
!
      r2 = spxyz(1,ipts)**2 + spxyz(2,ipts)**2 + spxyz(3,ipts)**2
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
          if (ssp .ge. (sp_max+toler)) then
            sp_max = ssp
            nfound = 1
            nfoundptr(nfound) = ipts
          elseif (ssp .ge. (sp_max-toler)) then
            nfound = nfound + 1                                                                     
            nfoundptr(nfound) = ipts                                                                 
          endif
        enddo
        if (sp_max .ge. cos2ds) then
          pwt(j) = 1.0_dp/dble(nfound)
          do ipts = 1,nfound
            ipm = nfoundptr(ipts)
            nara = nar(ipm)
            ind = nsetf(ipm) + nara
            if (ind.gt.maxnset) then
              maxnset = ind + 10
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
  if (status/=0) call outofmemory('updatesas','ldone')
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
  if (status/=0) call deallocate_error('updatesas','ldone')
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
  endif
!
!  Free local memory no longer needed
!
  deallocate(rotatedtm,stat=status)
  if (status/=0) call deallocate_error('updatesas','rotatedtm')
  deallocate(pwt,stat=status)
  if (status/=0) call deallocate_error('updatesas','pwt')
  deallocate(lInSAS,stat=status)
  if (status/=0) call deallocate_error('updatesas','lInSAS')
  deallocate(nfoundptr,stat=status)
  if (status/=0) call deallocate_error('updatesas','nfoundptr')
!
!  Finalise timer for this routine
!
  t2 = cputime()
  tcosmo = tcosmo + t2 - t1
!
  return
  end
