  subroutine setcosmoamat
!
!  Constructs the COSMO A matrix between points on the SAS
!
!   5/03 Segment weighting factors introduced
!   5/03 Inversion moved out of this routine
!  11/04 Commented out weighting of self term removed
!  11/04 Order of deallocations reversed
!   1/05 Self term now weighted by smoothing terms
!   4/05 Wolf sum introduced for long range contribution
!  10/08 Converted to f90 format
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, October 2008
!
  use control,   only : ldebug, keyword
  use cosmo
  use current
  use iochannels
  use parallel,  only : ioproc
  use reallocate
  use wolfcosmo, only : lPureCoulomb0D
  implicit none
!
!  Local variables
!
  integer(i4)                                    :: i, j, k, l
  integer(i4)                                    :: ii
  integer(i4)                                    :: ipts
  integer(i4)                                    :: imax
  integer(i4)                                    :: jmax
  integer(i4)                                    :: kmax
  integer(i4)                                    :: j1, j2
  integer(i4)                                    :: jpts
  integer(i4)                                    :: kk
  integer(i4)                                    :: kkk
  integer(i4),                              save :: maxvec = 1000
  integer(i4)                                    :: nari
  integer(i4)                                    :: narj
  integer(i4)                                    :: nmid
  integer(i4)                                    :: nsetfi
  integer(i4)                                    :: nsetfj
  integer(i4)                                    :: nvec
  integer(i4)                                    :: status
  logical                                        :: lperiodic
  real(dp)                                       :: aa
  real(dp)                                       :: aij
  real(dp)                                       :: aij2
  real(dp)                                       :: aijm
  real(dp)                                       :: dist
  real(dp)                                       :: dists
  real(dp)                                       :: dists2
  real(dp)                                       :: dqme(3)
  real(dp)                                       :: d2qme(6)
  real(dp)                                       :: d2wdr2
  real(dp)                                       :: dwdr
  real(dp)                                       :: dwt
  real(dp)                                       :: fdiag
  real(dp)                                       :: qme
  real(dp)                                       :: qmeself
  real(dp)                                       :: rdist
  real(dp)                                       :: ri
  real(dp)                                       :: rj
  real(dp)                                       :: rnari
  real(dp)                                       :: rnarj
  real(dp)                                       :: swi
  real(dp)                                       :: swj
  real(dp)                                       :: w1
  real(dp)                                       :: w2
  real(dp)                                       :: xa, ya, za
  real(dp)                                       :: xc, yc, zc
  real(dp)                                       :: xi, yi, zi
  real(dp)                                       :: xij, yij, zij
  real(dp)                                       :: xj, yj, zj
  real(dp)                                       :: xji, yji, zji
  real(dp)                                       :: x1, x2, x3
  real(dp)                                       :: y1, y2, y3
  real(dp),    dimension(:),   allocatable, save :: xvec
  real(dp),    dimension(:),   allocatable, save :: yvec
  real(dp),    dimension(:),   allocatable, save :: zvec
!
!  Set local constants
!
  fdiag = 1.05_dp*sqrt(nppa+0.0_dp)
  lperiodic = (ndim.gt.0)
!
!  Calculate self term 
!
  if (lperiodic) then
    call qmatrixelementc(0.0_dp,0.0_dp,0.0_dp,0.0_dp,.true.,.false.,.false.,qmeself,dqme,d2qme)
  endif
!
!  Set up list of cell vectors that are required
!
  if (lperiodic) then
10  nvec = 0
    allocate(xvec(maxvec),stat=status)
    if (status/=0) call outofmemory('setcosmoamat','xvec')
    allocate(yvec(maxvec),stat=status)
    if (status/=0) call outofmemory('setcosmoamat','yvec')
    allocate(zvec(maxvec),stat=status)
    if (status/=0) call outofmemory('setcosmoamat','zvec')
    call rtlist(nvec,cosmormax,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvec)
    if (nvec.gt.maxvec) then
      maxvec = nvec + 10
      deallocate(zvec,stat=status)
      if (status/=0) call deallocate_error('setcosmoamat','zvec')
      deallocate(yvec,stat=status)
      if (status/=0) call deallocate_error('setcosmoamat','yvec')
      deallocate(xvec,stat=status)
      if (status/=0) call deallocate_error('setcosmoamat','xvec')
      goto 10
    endif
  else
    nvec = 1
    nmid = 1
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('setcosmoamat','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('setcosmoamat','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('setcosmoamat','zvec')
    xvec(1) = 0.0_dp
    yvec(1) = 0.0_dp
    zvec(1) = 0.0_dp
  endif
!
!  Zero cosmoA
!
  cosmoA(1:(npts*(npts+1)/2)) = 0.0_dp
!
!  Filling matrix between SAS points - cosmoA
!
  kk = 0
  kkk = 0
  do ipts = 1,npts
    i = cosmoatomptr(ipts)
    ri = atsrad(i)
    swi = segweight(ipts)
!
!  Self-term - no weighting for on-site interaction otherwise A becomes singular
!
    nari = nar(ipts)
    nsetfi = nsetf(ipts)
    aa = 0.0_dp
    rnari = 0.0_dp
    do k = nsetfi,nsetfi+nari-1
      j1 = nset(k)
      w1 = cosmowt(j1,i)*cosmopwt(k)
      aa = aa + fdiag*w1*w1
      x1 = sphere2(1,j1)
      x2 = sphere2(2,j1)
      x3 = sphere2(3,j1)
      rnari = rnari + w1
      do l = nsetfi,k-1
        j2 = nset(l)
        w2 = cosmowt(j2,i)*cosmopwt(l)
        dist = 2.0_dp/sqrt((x1-sphere2(1,j2))**2+(x2-sphere2(2,j2))**2+(x3-sphere2(3,j2))**2)
        aa = aa + w1*w2*dist
      enddo
    enddo
    aa = aa/(ri*rnari**2)
    kk = kk + ipts
    cosmoA(kk) = aa
!
!  Coulomb correction - with weighting
!
    if (lperiodic) then
      cosmoA(kk) = cosmoA(kk) + qmeself*swi*swi
    endif
!
!  Set constants for first SAS point
!
    xi = xclat(i)
    yi = yclat(i)
    zi = zclat(i)
    xa = spxyz(1,ipts) 
    ya = spxyz(2,ipts) 
    za = spxyz(3,ipts) 
!
!  Calculation terms for A-matrix between SAS points
!
    do jpts = 1,ipts
      kkk = kkk + 1
      narj = nar(jpts)
      nsetfj = nsetf(jpts)
      swj = segweight(jpts)
      j = cosmoatomptr(jpts)
      xji = spxyz(1,jpts) - xa
      yji = spxyz(2,jpts) - ya
      zji = spxyz(3,jpts) - za
      do ii = 1,nvec
        if (ii.ne.nmid.or.jpts.ne.ipts) then
          xij = xji + xvec(ii)
          yij = yji + yvec(ii)
          zij = zji + zvec(ii)
          dists2 = xij*xij + yij*yij + zij*zij
          aij = 0.0_dp
          if (dists2 .lt. cosmormax2) then
            if (dists2 .gt. cosmormax2s) then
              dists = sqrt(dists2)
              if (lPureCoulomb0D) then
                aij2 = 1.0_dp/dists
              else
                call qmatrixelementc(xij,yij,zij,0.0_dp,.false.,.false.,.false.,aij2,dqme,d2qme)
              endif
              call switch2(cosmormax-dists,dwt,.false.,.false.,dwdr,d2wdr2)
              aij2 = dwt*aij2
            else
              dwt = 0.0_dp
              aij2 = 0.0_dp
            endif
            xj = xclat(j) - xi + xvec(ii)
            yj = yclat(j) - yi + yvec(ii)
            zj = zclat(j) - zi + zvec(ii)
            rj = atsrad(j)
            do k = nsetfi,nsetfi+nari-1
              j1 = nset(k)
              xc = sphere2(1,j1)*ri
              yc = sphere2(2,j1)*ri
              zc = sphere2(3,j1)*ri
              w1 = cosmowt(j1,i)*cosmopwt(k)
              if (i.ne.j.or.ii.ne.nmid) then
                x1 = xc*cosmotm(1,1,i) + yc*cosmotm(2,1,i) + zc*cosmotm(3,1,i) - xj
                x2 = xc*cosmotm(1,2,i) + yc*cosmotm(2,2,i) + zc*cosmotm(3,2,i) - yj
                x3 = xc*cosmotm(1,3,i) + yc*cosmotm(2,3,i) + zc*cosmotm(3,3,i) - zj
                rnarj = 0.0_dp
                do l = nsetfj,nsetfj+narj-1
                  j2 = nset(l)
                  w2 = cosmowt(j2,j)*cosmopwt(l)
                  rnarj = rnarj + w2
                  xc = sphere2(1,j2)*rj
                  yc = sphere2(2,j2)*rj
                  zc = sphere2(3,j2)*rj
                  y1 = xc*cosmotm(1,1,j) + yc*cosmotm(2,1,j) + zc*cosmotm(3,1,j) - x1
                  y2 = xc*cosmotm(1,2,j) + yc*cosmotm(2,2,j) + zc*cosmotm(3,2,j) - x2
                  y3 = xc*cosmotm(1,3,j) + yc*cosmotm(2,3,j) + zc*cosmotm(3,3,j) - x3
                  dist = y1*y1 + y2*y2 + y3*y3
                  rdist = 1.0_dp/sqrt(dist)
                  aij = aij + w1*w2*rdist
                enddo
              else
                rnarj = 0.0_dp
                do l = nsetfj,nsetfj+narj-1
                  j2 = nset(l)
                  w2 = cosmowt(j2,j)*cosmopwt(l)
                  rnarj = rnarj + w2
                  dist = ((sphere2(1,j2)*rj-xc)**2 + (sphere2(2,j2)*rj-yc)**2 + (sphere2(3,j2)*rj-zc)**2)
                  if (dist.gt.1.0d-8) then
                    aij = aij + w1*w2/sqrt(dist)
                  endif
                enddo
              endif
            enddo
            aij = (1.0_dp - dwt)*aij/(rnari*rnarj) + aij2
!
!  For periodic case, subtract 1/r term to avoid double counting with the term from qmatrixelementc.
!
            if (lperiodic.and.dists2.gt.1.0d-15) then
              call qmatrixelementc(xij,yij,zij,0.0_dp,.false.,.false.,.false.,aijm,dqme,d2qme)
              aij = aij - aijm
            endif
          else
!
!  Coulomb element only needs adding for a cluster since this is handled for periodic systems via qmatrixelementc.
!
            if (.not.lperiodic.and.dists2.gt.1.0d-8) then
              if (lPureCoulomb0D) then
                aij = 1.0_dp/sqrt(dists2)
              else
                call qmatrixelementc(xji,yji,zji,0.0_dp,.false.,.false.,.false.,aij,dqme,d2qme)
              endif
            else
              aij = 0.0_dp
            endif
          endif
          cosmoA(kkk) = cosmoA(kkk) + aij*swi*swj
        endif
      enddo
      if (lperiodic.and.(ipts.ne.jpts)) then
        call qmatrixelementc(xji,yji,zji,0.0_dp,.true.,.false.,.false.,qme,dqme,d2qme)
        cosmoA(kkk) = cosmoA(kkk) + qme*swi*swj
      endif
    enddo
  enddo
!
!  Debugging output
!
  if (index(keyword,'amat').ne.0.and.ldebug.and.ioproc) then
    write(ioout,'(/,'' COSMO A matrix : '',/)')
    kk = 0
    do i = 1,npts
      write(ioout,'(10f12.6)')(cosmoA(kk+j),j=1,i)
      kk = kk + i
    enddo
  endif
!
!  Free local memory
!
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('setcosmoamat','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('setcosmoamat','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('setcosmoamat','xvec')
!
  return
  end
