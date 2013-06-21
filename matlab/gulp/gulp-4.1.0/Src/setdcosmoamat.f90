  subroutine setdcosmoamat(ldqneeded,lcosmicd2,lgrad2,dcosmoA,dcosmoAA,fullA,d2qfct,dsegweight,d2segweight, &
    nvec,nmid,nearsas,nearsasptr,nearsasrptr,lnearsas,lanynonunit,xvec,yvec,zvec,dcosmoA2)
!
!  Subroutine calculates the derivatives of the COSMO A matrix
!
!   5/03 Created from cosmoderv
!   5/03 Segment weighting added
!   7/03 Subroutine introduced for derivatives
!   7/03 Second derivatives with respect to segment smoothing completed
!  11/04 Intent added
!  11/04 Error in address adrvi/adrvj corrected (adrv*(*,m) -> adrv*(*,mm))
!  12/04 Local scalars introduced to reduce multiplication count
!  12/04 Logicals to indicate which elements of adrv/a2drv are set added
!   1/05 Handling of drsolv modified back to original COSMO scheme
!  10/08 Converted to f90 format
!  12/08 Cleaned up & ii/nmid no longer passed to cosmoaijderv
!  11/09 Region derivatives added
!   7/10 Referencing of nregionno corrected to refer to asymmetric unit
!   7/10 qsipj and sumAinvipts/jpts terms initialised for case where lcosmicd2 is false
!   7/10 Dummy allocation of a2drv added when lgrad2 is false so that pointer is 
!        defined before passing to subroutine.
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
  use wolfcosmo,      only : lPureCoulomb0D
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)                 :: nearsas
  integer(i4),       intent(in)                 :: nearsasptr(*)
  integer(i4),       intent(in)                 :: nearsasrptr(*)
  integer(i4),       intent(in)                 :: nmid
  integer(i4),       intent(in)                 :: nvec
  logical,           intent(in)                 :: lanynonunit(*)
  logical,           intent(in)                 :: lcosmicd2
  logical,           intent(in)                 :: ldqneeded
  logical,           intent(in)                 :: lgrad2
  logical,           intent(in)                 :: lnearsas(*)
  real(dp),          intent(in)                 :: d2qfct
  real(dp),          intent(inout)              :: dcosmoA(3,npts,*)
  real(dp),          intent(inout)              :: dcosmoA2(3,npts,*)
  real(dp),          intent(inout)              :: dcosmoAA(3,nearsas,*)
  real(dp),          intent(inout)              :: dsegweight(3,maxnearseg,*)
  real(dp),          intent(inout)              :: d2segweight(3,3,maxnearseg,maxnearseg,*)
  real(dp),          intent(in)                 :: fullA(npts,*)
  real(dp),          intent(in)                 :: xvec(*)
  real(dp),          intent(in)                 :: yvec(*)
  real(dp),          intent(in)                 :: zvec(*)
!
!  Local variables
!
  integer(i4)                                   :: i
  integer(i4)                                   :: ia
  integer(i4)                                   :: ierror
  integer(i4)                                   :: ii
  integer(i4)                                   :: in
  integer(i4)                                   :: ix
  integer(i4)                                   :: iy
  integer(i4)                                   :: iz
  integer(i4)                                   :: ipts
  integer(i4)                                   :: j
  integer(i4)                                   :: j1
  integer(i4)                                   :: j2
  integer(i4)                                   :: ja
  integer(i4)                                   :: jn
  integer(i4)                                   :: jpts
  integer(i4)                                   :: jx
  integer(i4)                                   :: jy
  integer(i4)                                   :: jz
  integer(i4)                                   :: k
  integer(i4)                                   :: kpts
  integer(i4)                                   :: l
  integer(i4)                                   :: maxnpwt2
  integer(i4)                                   :: maxPperS
  integer(i4)                                   :: m
  integer(i4)                                   :: mm
  integer(i4)                                   :: mn
  integer(i4)                                   :: mx
  integer(i4)                                   :: my
  integer(i4)                                   :: mz
  integer(i4)                                   :: n
  integer(i4)                                   :: nn
  integer(i4)                                   :: nari
  integer(i4)                                   :: narj
  integer(i4)                                   :: nearsas2
  integer(i4)                                   :: npwt1
  integer(i4)                                   :: npwt2
  integer(i4)                                   :: nregioni
  integer(i4)                                   :: nregionj
  integer(i4)                                   :: nregionn
  integer(i4)                                   :: nsetfi
  integer(i4)                                   :: nsetfj
  integer(i4)                                   :: nx
  integer(i4)                                   :: ny
  integer(i4)                                   :: nz
  integer(i4)                                   :: status
  logical,      dimension(:), allocatable, save :: ladrv
  logical                                       :: lopi
  logical                                       :: lopj
  logical                                       :: lperiodic
  logical                                       :: lrweight
  real(dp)                                      :: aij
  real(dp)                                      :: aij2
  real(dp)                                      :: adrvijx
  real(dp)                                      :: adrvijxx
  real(dp)                                      :: adrvijxy
  real(dp)                                      :: adrvijxz
  real(dp)                                      :: adrvijy
  real(dp)                                      :: adrvijyx
  real(dp)                                      :: adrvijyy
  real(dp)                                      :: adrvijyz
  real(dp)                                      :: adrvijz
  real(dp)                                      :: adrvijzx
  real(dp)                                      :: adrvijzy
  real(dp)                                      :: adrvijzz
  real(dp)                                      :: d1rdist
  real(dp)                                      :: ddist
  real(dp)                                      :: dist
  real(dp)                                      :: dists
  real(dp)                                      :: dists2
  real(dp)                                      :: dqme(3)
  real(dp)                                      :: d2qme(6)
  real(dp)                                      :: d2wdr2
  real(dp)                                      :: dwdr
  real(dp)                                      :: dwtm1
  real(dp)                                      :: dwtm1swiswj
  real(dp)                                      :: dwt
  real(dp)                                      :: f0
  real(dp)                                      :: f1(3)
  real(dp)                                      :: f2(6)
  real(dp)                                      :: fact
  real(dp)                                      :: fdiag
  real(dp)                                      :: ff
  real(dp)                                      :: ffa
  real(dp)                                      :: pw1
  real(dp)                                      :: pw2
  real(dp)                                      :: qme
  real(dp)                                      :: qmeself
  real(dp)                                      :: qsi
  real(dp)                                      :: qsi2
  real(dp)                                      :: qsij
  real(dp)                                      :: qsijr
  real(dp)                                      :: qsijri
  real(dp)                                      :: qsijrj
  real(dp)                                      :: qsijdwtm1swiswj
  real(dp)                                      :: qsipj
  real(dp)                                      :: qsipjr
  real(dp)                                      :: qsipjdwtm1swiswj
  real(dp)                                      :: qsj
  real(dp)                                      :: ri
  real(dp)                                      :: rj
  real(dp)                                      :: rdist
  real(dp)                                      :: rdists
  real(dp)                                      :: rfact
  real(dp)                                      :: rnari
  real(dp)                                      :: rtrm
  real(dp)                                      :: rtrm2
  real(dp)                                      :: sumAinvipts
  real(dp)                                      :: sumAinvjpts
  real(dp)                                      :: swi
  real(dp)                                      :: swj
  real(dp)                                      :: x1, x2, x3
  real(dp)                                      :: xa, ya, za
  real(dp)                                      :: xc, yc, zc
  real(dp)                                      :: xi, yi, zi
  real(dp)                                      :: xij, yij, zij
  real(dp)                                      :: xj, yj, zj
  real(dp)                                      :: xji, yji, zji
  real(dp)                                      :: y1, y2, y3
  real(dp)                                      :: w1
  real(dp)                                      :: w2
  real(dp), dimension(:,:),   pointer,     save :: adrvi => null()
  real(dp), dimension(:,:),   pointer,     save :: adrvj => null()
  real(dp), dimension(:,:,:), pointer,     save :: a2drv => null()
  real(dp), dimension(:,:,:), pointer,     save :: dwti => null()
  real(dp), dimension(:,:,:), pointer,     save :: dwtj => null()
  real(dp), dimension(:,:,:,:), pointer,   save :: d2wti => null()
  real(dp), dimension(:,:,:,:), pointer,   save :: d2wtj => null()
  real(dp), dimension(:),     allocatable, save :: totalwt
!
!  Allocate local memory
!
  allocate(totalwt(npts),stat=status)
  if (status/=0) call outofmemory('setdcosmoamat','totalwt')
!
!  Set local constants
!
  fdiag = 1.05_dp*sqrt(nppa+0.0_dp)
  fact  = 0.5_dp*autoev*autoangs*cosmofneps
  lperiodic = (ndim.gt.0)
  nearsas2 = nearsas*(nearsas + 1)/2
!
!  Calculate self term
!
  if (lperiodic.and.lsegsmooth) then
    call qmatrixelementc(0.0_dp,0.0_dp,0.0_dp,0.0_dp,.true.,.false.,.false.,qmeself,dqme,d2qme)
  endif
!
!  Find maximum number of points per segment, maxPperS
!
  maxPperS = 0
  do ipts = 1,npts
    nari = nar(ipts)
    maxPperS = max(maxPperS,nari)
  enddo
!
!  Allocate arrays to store derivatives between i and other atoms
!
!  dwti   = derivative of weight w.r.t. position of i
!  dwtj   = derivative of weight w.r.t. position of j
!  d2wti  = second derivative of weight w.r.t. position of i
!  d2wtj  = second derivative of weight w.r.t. position of j
!  adrvi  = derivative of matrix element Aij w.r.t. i
!  adrvj  = derivative of matrix element Aij w.r.t. j
!  a2drv  = second derivative of matrix element Aij 
!
  call realloc(dwti,3_i4,maxnpwt,maxPperS,ierror)
  if (ierror.ne.0) call outofmemory('cosmoderv','dwti')
  call realloc(dwtj,3_i4,maxnpwt,maxPperS,ierror)
  if (ierror.ne.0) call outofmemory('cosmoderv','dwtj')
  call realloc(adrvi,3_i4,nearsas,ierror)
  if (ierror.ne.0) call outofmemory('cosmoderv','adrvi')
  call realloc(adrvj,3_i4,nearsas,ierror)
  if (ierror.ne.0) call outofmemory('cosmoderv','adrvj')
  allocate(ladrv(nearsas),stat=status)
  if (status/=0) call outofmemory('setdcosmoamat','ladrv')
  if (lgrad2) then
    maxnpwt2 = maxnpwt*(maxnpwt + 1)/2
    call realloc(a2drv,3_i4,3_i4,nearsas2,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','a2drv')
  else
    maxnpwt2 = 1
    call realloc(a2drv,1_i4,1_i4,1_i4,ierror)
    if (ierror.ne.0) call outofmemory('cosmoderv','a2drv')
  endif
  call realloc(d2wti,3_i4,3_i4,maxnpwt2,maxPperS,ierror)
  if (ierror.ne.0) call outofmemory('cosmoderv','d2wti')
  call realloc(d2wtj,3_i4,3_i4,maxnpwt2,maxPperS,ierror)
  if (ierror.ne.0) call outofmemory('cosmoderv','d2wtj')
!
!  Calculate the sum of the weighting factors for each segment
!
  do ipts = 1,npts
    i = cosmoatomptr(ipts)
    nari = nar(ipts)
    nsetfi = nsetf(ipts)
    rnari = 0.0_dp
    do k = 1,nari
      j1 = nset(k+nsetfi-1)
      w1 = cosmowt(j1,i)*cosmopwt(k+nsetfi-1)
      rnari = rnari + w1
    enddo
    totalwt(ipts) = 1.0_dp/rnari
  enddo
!
!  First initialisation of variables
!
  do n = 1,nearsas
    adrvi(1:3,n) = 0.0_dp
    adrvj(1:3,n) = 0.0_dp
  enddo
  ladrv(1:nearsas) = .false.
  if (lgrad2) then
    do n = 1,nearsas2
      a2drv(1:3,1:3,n) = 0.0_dp
    enddo
  endif
!********************************
!  Derivatives of the A matrix  *
!********************************
  do ipts = 1,npts
    i = cosmoatomptr(ipts)
    ia = nrelat(i)
    in = nearsasrptr(i)
    npwt1 = npwt(ipts)
    nregioni = nregionno(nsft+ia)
    lopi = (.not.lfreeze.or.lopf(i).or.lnearsas(i))
    ix = 3*(i-1) + 1
    iy = ix + 1
    iz = ix + 2
    ri = atsrad(i) 
    swi = segweight(ipts)
    nari = nar(ipts)
    nsetfi = nsetf(ipts)
    qsi = qonsas(ipts)
    qsi2 = qsi*qsi*fact
    if (lcosmicd2) then
      sumAinvipts = 0.0_dp
      do kpts = 1,npts
        sumAinvipts = sumAinvipts + fullA(kpts,ipts)
      enddo
      qsipj = qsi*sumAinvipts
    else
      sumAinvipts = 0.0_dp
      qsipj = 0.0_dp
    endif
!
!  Get derivatives of weighting factors
!
    call dtotcosmowt(ipts,totalwt(ipts),dwti,d2wti,lgrad2)
    if (lanynonunit(i)) then
!
!  Self-term - derivatives come from the weighting factor
!
      do k = 1,nari
        j1 = nset(k+nsetfi-1)
!
!  Calculate derivatives of all weighting factors
!
        w1 = cosmowt(j1,i)
        pw1 = cosmopwt(k+nsetfi-1)
        w1 = w1*pw1*totalwt(ipts)
        call cosmoaiiderv(ipts,in,k,k,nearsasrptr,npwt1,maxnpwt2,fdiag,w1,w1,dwti,d2wti,adrvi,a2drv,ladrv,lgrad2)
!
        x1 = sphere2(1,j1)
        x2 = sphere2(2,j1)
        x3 = sphere2(3,j1)
        do l = 1,k-1
          j2 = nset(l+nsetfi-1)
          w2 = cosmowt(j2,i)
          pw2 = cosmopwt(l+nsetfi-1)
          w2 = w2*pw2*totalwt(ipts)
          dist = (x1-sphere2(1,j2))**2 + (x2-sphere2(2,j2))**2 + (x3-sphere2(3,j2))**2
          dist = 2.0_dp/sqrt(dist)
          call cosmoaiiderv(ipts,in,k,l,nearsasrptr,npwt1,maxnpwt2,dist,w1,w2,dwti,d2wti,adrvi,a2drv,ladrv,lgrad2)
        enddo
      enddo
      rfact = 1.0_dp/ri
      do n = 1,nearsas
        adrvi(1:3,n) = rfact*adrvi(1:3,n)
      enddo
      if (lgrad2) then
        do n = 1,nearsas2
          a2drv(1:3,1:3,n) = rfact*a2drv(1:3,1:3,n)
        enddo
      endif
!
!  Add derivatives connected with self-term on to main arrays
!
      do nn = 1,nearsas
        if (ladrv(nn)) then
          n = nearsasptr(nn)
          if (n .ne. i) then
            xdrv(i) = xdrv(i) - qsi2*adrvi(1,nn)
            ydrv(i) = ydrv(i) - qsi2*adrvi(2,nn)
            zdrv(i) = zdrv(i) - qsi2*adrvi(3,nn)
            xdrv(n) = xdrv(n) + qsi2*adrvi(1,nn)
            ydrv(n) = ydrv(n) + qsi2*adrvi(2,nn)
            zdrv(n) = zdrv(n) + qsi2*adrvi(3,nn)
            nregionn = nregionno(nsft+nrelat(n))
            if (nregioni.ne.nregionn) then
              xregdrv(nregioni) = xregdrv(nregioni) - qsi2*adrvi(1,nn)
              yregdrv(nregioni) = yregdrv(nregioni) - qsi2*adrvi(2,nn)
              zregdrv(nregioni) = zregdrv(nregioni) - qsi2*adrvi(3,nn)
              xregdrv(nregionn) = xregdrv(nregionn) + qsi2*adrvi(1,nn)
              yregdrv(nregionn) = yregdrv(nregionn) + qsi2*adrvi(2,nn)
              zregdrv(nregionn) = zregdrv(nregionn) + qsi2*adrvi(3,nn)
            endif
          endif
          if (ldqneeded) then
!  
!  Save first derivatives of A matrix term
!     
            dcosmoA(1,ipts,nn) = dcosmoA(1,ipts,nn) - qsi*adrvi(1,nn)
            dcosmoA(2,ipts,nn) = dcosmoA(2,ipts,nn) - qsi*adrvi(2,nn)
            dcosmoA(3,ipts,nn) = dcosmoA(3,ipts,nn) - qsi*adrvi(3,nn)
            if (lcosmicd2) then
              dcosmoAA(1,nn,ipts) = dcosmoAA(1,nn,ipts) - sumAinvipts*adrvi(1,nn)
              dcosmoAA(2,nn,ipts) = dcosmoAA(2,nn,ipts) - sumAinvipts*adrvi(2,nn)
              dcosmoAA(3,nn,ipts) = dcosmoAA(3,nn,ipts) - sumAinvipts*adrvi(3,nn)
            endif
          endif
        endif
      enddo
      if (lgrad2) then
        do mm = 1,nearsas
          if (ladrv(mm)) then
            m = nearsasptr(mm)
            mx = 3*(m-1) + 1
            my = mx + 1
            mz = my + 1
            do nn = 1,mm
              if (ladrv(nn)) then
                mn = mm*(mm - 1)/2 + nn
                n = nearsasptr(nn)
                nx = 3*(n-1) + 1
                ny = nx + 1
                nz = ny + 1
                derv2(nx,mx) = derv2(nx,mx) + qsi2*a2drv(1,1,mn)
                derv2(ny,mx) = derv2(ny,mx) + qsi2*a2drv(2,1,mn)
                derv2(nz,mx) = derv2(nz,mx) + qsi2*a2drv(3,1,mn)
                derv2(nx,my) = derv2(nx,my) + qsi2*a2drv(1,2,mn)
                derv2(ny,my) = derv2(ny,my) + qsi2*a2drv(2,2,mn)
                derv2(nz,my) = derv2(nz,my) + qsi2*a2drv(3,2,mn)
                derv2(nx,mz) = derv2(nx,mz) + qsi2*a2drv(1,3,mn)
                derv2(ny,mz) = derv2(ny,mz) + qsi2*a2drv(2,3,mn)
                derv2(nz,mz) = derv2(nz,mz) + qsi2*a2drv(3,3,mn)
                if (lcosmicd2) then
                  derv2(nx,mx) = derv2(nx,mx) + d2qfct*qsi*sumAinvipts*a2drv(1,1,mn)
                  derv2(ny,mx) = derv2(ny,mx) + d2qfct*qsi*sumAinvipts*a2drv(2,1,mn)
                  derv2(nz,mx) = derv2(nz,mx) + d2qfct*qsi*sumAinvipts*a2drv(3,1,mn)
                  derv2(nx,my) = derv2(nx,my) + d2qfct*qsi*sumAinvipts*a2drv(1,2,mn)
                  derv2(ny,my) = derv2(ny,my) + d2qfct*qsi*sumAinvipts*a2drv(2,2,mn)
                  derv2(nz,my) = derv2(nz,my) + d2qfct*qsi*sumAinvipts*a2drv(3,2,mn)
                  derv2(nx,mz) = derv2(nx,mz) + d2qfct*qsi*sumAinvipts*a2drv(1,3,mn)
                  derv2(ny,mz) = derv2(ny,mz) + d2qfct*qsi*sumAinvipts*a2drv(2,3,mn)
                  derv2(nz,mz) = derv2(nz,mz) + d2qfct*qsi*sumAinvipts*a2drv(3,3,mn)
                endif
              endif
            enddo
          endif
        enddo
      endif
!
!  Rezero derivatives
!
      call cosmoaiizero(ipts,in,nearsasrptr,npwt1,adrvi,a2drv,ladrv,lgrad2)
    endif
!
!  Self-term derivatives
!
    if (lperiodic.and.lsegsmooth) then
      qsi2 = 2.0_dp*qsi2
      qsipj = 2.0_dp*qsipj
      f1(1:3) = 0.0_dp
      f2(1:6) = 0.0_dp
      call cosmoamatdadd(ipts,ipts,qmeself,f1,f2,qsi2,qsipj,sumAinvipts,sumAinvipts,ldqneeded,lcosmicd2, &
        lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr,dcosmoA2)
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
    do jpts = ipts,npts
      j = cosmoatomptr(jpts)
      ja = nrelat(j)
      npwt2 = npwt(jpts)
      jn = nearsasrptr(j)
      nregionj = nregionno(nsft+ja)
      lopj = (.not.lfreeze.or.lopf(j).or.lnearsas(j))
      if (lopi.or.lopj) then
        jx = 3*(j-1) + 1
        jy = jx + 1
        jz = jx + 2
        swj = segweight(jpts)
        narj = nar(jpts)
        nsetfj = nsetf(jpts)
        qsj = qonsas(jpts)
        qsij = 2.0_dp*qsi*qsj*fact
        if (lcosmicd2) then
          sumAinvjpts = 0.0_dp
          do kpts = 1,npts
            sumAinvjpts = sumAinvjpts + fullA(kpts,jpts)
          enddo
          qsipj = (qsi*sumAinvjpts + qsj*sumAinvipts)
        else
          sumAinvjpts = 0.0_dp
          qsipj = 0.0_dp
        endif
        xji = spxyz(1,jpts) - xa
        yji = spxyz(2,jpts) - ya
        zji = spxyz(3,jpts) - za
!
!  Get derivatives of weighting factors
!
        call dtotcosmowt(jpts,totalwt(jpts),dwtj,d2wtj,lgrad2)
!
!  Loop over cell images
!
        do ii = 1,nvec
          if (ii.ne.nmid.or.jpts.ne.ipts) then
            xij = xji + xvec(ii)
            yij = yji + yvec(ii)
            zij = zji + zvec(ii)
            dists2 = xij*xij + yij*yij + zij*zij
!
            if (dists2 .lt. cosmormax2) then
              if (dists2 .gt. cosmormax2s) then
                lrweight = .true.
                dists = sqrt(dists2)
                call switch2(cosmormax-dists,dwt,.true.,lgrad2,dwdr,d2wdr2)
!
!  Switch sign of dwdr since the negative of the distance was passed into switch
!
                dwdr = - dwdr
!
                rdists = 1.0_dp/dists
                dwtm1 = 1.0_dp - dwt
                dwdr  = rdists*dwdr
                d2wdr2 = rdists*rdists*(d2wdr2 - dwdr)
!
                if (lPureCoulomb0D) then
                  f0    = dwt*rdists
                  rtrm = rdists*(dwdr - dwt*rdists*rdists)
                  f1(1) = rtrm*xij
                  f1(2) = rtrm*yij
                  f1(3) = rtrm*zij
                  rtrm2 = rdists*(d2wdr2 + rdists*rdists*(3.0_dp*dwt*rdists*rdists - 2.0_dp*dwdr))
                  f2(1) = rtrm2*xij*xij + rtrm
                  f2(2) = rtrm2*xij*yij
                  f2(3) = rtrm2*yij*yij + rtrm
                  f2(4) = rtrm2*xij*zij
                  f2(5) = rtrm2*yij*zij
                  f2(6) = rtrm2*zij*zij + rtrm
                else
                  call qmatrixelementc(xij,yij,zij,0.0_dp,.false.,.true.,lgrad2,aij2,dqme,d2qme)
                  f0 = dwt*aij2
                  f1(1) = - dwt*dqme(1) + dwdr*aij2*xij
                  f1(2) = - dwt*dqme(2) + dwdr*aij2*yij
                  f1(3) = - dwt*dqme(3) + dwdr*aij2*zij
                  f2(1) = - dwt*d2qme(1) + d2wdr2*aij2*xij*xij - 2.0_dp*dwdr*xij*dqme(1) + dwdr*aij2
                  f2(2) = - dwt*d2qme(2) + d2wdr2*aij2*xij*yij - dwdr*(xij*dqme(2) + yij*dqme(1))
                  f2(3) = - dwt*d2qme(3) + d2wdr2*aij2*yij*yij - 2.0_dp*dwdr*yij*dqme(2) + dwdr*aij2
                  f2(4) = - dwt*d2qme(4) + d2wdr2*aij2*xij*zij - dwdr*(xij*dqme(3) + zij*dqme(1))
                  f2(5) = - dwt*d2qme(5) + d2wdr2*aij2*yij*zij - dwdr*(zij*dqme(2) + yij*dqme(3))
                  f2(6) = - dwt*d2qme(6) + d2wdr2*aij2*zij*zij - 2.0_dp*dwdr*zij*dqme(3) + dwdr*aij2
                endif
                call cosmoamatdadd(ipts,jpts,f0,f1,f2,qsij,qsipj,sumAinvipts,sumAinvjpts,ldqneeded,lcosmicd2, &
                  lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr,dcosmoA2)
              else
                lrweight = .false.
                dwdr = 0.0_dp
                d2wdr2 = 0.0_dp
                dwt = 0.0_dp
                dwtm1 = 1.0_dp
              endif
              xj = xclat(j) - xi + xvec(ii)
              yj = yclat(j) - yi + yvec(ii)
              zj = zclat(j) - zi + zvec(ii)
              rj = atsrad(j)
              do k = 1,nari
                j1 = nset(k+nsetfi-1)
                xc = sphere2(1,j1)*ri
                yc = sphere2(2,j1)*ri
                zc = sphere2(3,j1)*ri
                w1 = cosmowt(j1,i)
                pw1 = cosmopwt(k+nsetfi-1)
                w1 = w1*pw1*totalwt(ipts)
                if (i .ne. j .or. ii.ne.nmid) then
                  x1 = xc*cosmotm(1,1,i) + yc*cosmotm(2,1,i) + zc*cosmotm(3,1,i) - xj
                  x2 = xc*cosmotm(1,2,i) + yc*cosmotm(2,2,i) + zc*cosmotm(3,2,i) - yj
                  x3 = xc*cosmotm(1,3,i) + yc*cosmotm(2,3,i) + zc*cosmotm(3,3,i) - zj
                  do l = 1,narj
                    j2 = nset(l+nsetfj-1)
                    w2 = cosmowt(j2,j)
                    pw2 = cosmopwt(l+nsetfj-1)
                    w2 = w2*pw2*totalwt(jpts)
                    xc = sphere2(1,j2)*rj
                    yc = sphere2(2,j2)*rj
                    zc = sphere2(3,j2)*rj
                    y1 = xc*cosmotm(1,1,j) + yc*cosmotm(2,1,j) + zc*cosmotm(3,1,j) - x1
                    y2 = xc*cosmotm(1,2,j) + yc*cosmotm(2,2,j) + zc*cosmotm(3,2,j) - x2
                    y3 = xc*cosmotm(1,3,j) + yc*cosmotm(2,3,j) + zc*cosmotm(3,3,j) - x3
                    dist = y1*y1 + y2*y2 + y3*y3
                    rdist = 1.0_dp/sqrt(dist)
                    d1rdist = - rdist*rdist*rdist
                    call cosmoaijderv(ipts,in,k,jpts,jn,l,nearsasrptr,npwt1,npwt2,maxnpwt2, &
                      y1,y2,y3,rdist,d1rdist,w1,w2,dwti,dwtj,d2wti,d2wtj,adrvi,adrvj, &
                      a2drv,ladrv,lgrad2)
!
!  Derivatives with respect to smoothing
!
                    ddist = w1*w2*d1rdist
                    ff = dwtm1*ddist      
                    f0    = dwtm1*w1*w2*rdist
                    f1(1) = ff*y1 
                    f1(2) = ff*y2
                    f1(3) = ff*y3
                    if (lrweight) then
                      ffa = - w1*w2*rdist*dwdr
                      f1(1) = f1(1) + ffa*xij
                      f1(2) = f1(2) + ffa*yij
                      f1(3) = f1(3) + ffa*zij
                    endif
                    rtrm  = - ddist*dwtm1
                    rtrm2 = - 3.0_dp*rdist*rdist*ff
                    f2(1) = rtrm2*y1*y1 - rtrm
                    f2(2) = rtrm2*y1*y2  
                    f2(3) = rtrm2*y2*y2 - rtrm
                    f2(4) = rtrm2*y1*y3       
                    f2(5) = rtrm2*y2*y3     
                    f2(6) = rtrm2*y3*y3 - rtrm
                    if (lrweight) then
                      rtrm2 = - ddist*dwdr
                      f2(1) = f2(1) + rtrm2*(y1*xij + xij*y1)
                      f2(2) = f2(2) + rtrm2*(y2*xij + yij*y1)
                      f2(3) = f2(3) + rtrm2*(y2*yij + yij*y2)
                      f2(4) = f2(4) + rtrm2*(y3*xij + zij*y1)
                      f2(5) = f2(5) + rtrm2*(y3*yij + zij*y2)
                      f2(6) = f2(6) + rtrm2*(y3*zij + zij*y3)
!
                      rtrm2 = - w1*w2*rdist*d2wdr2
                      rtrm  = - w1*w2*rdist*dwdr
                      f2(1) = f2(1) + rtrm2*xij*xij + rtrm
                      f2(2) = f2(2) + rtrm2*xij*yij
                      f2(3) = f2(3) + rtrm2*yij*yij + rtrm
                      f2(4) = f2(4) + rtrm2*xij*zij
                      f2(5) = f2(5) + rtrm2*yij*zij
                      f2(6) = f2(6) + rtrm2*zij*zij + rtrm
                    endif
                    call cosmoamatdadd(ipts,jpts,f0,f1,f2,qsij,qsipj,sumAinvipts,sumAinvjpts,ldqneeded,lcosmicd2, &
                      lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr,dcosmoA2)
                  enddo
                else
                  do l = 1,narj
                    j2 = nset(l+nsetfj-1)
                    w2 = cosmowt(j2,j)
                    pw2 = cosmopwt(l+nsetfj-1)
                    w2 = w2*pw2*totalwt(jpts)
                    y1 = sphere2(1,j2)*rj - xc
                    y2 = sphere2(2,j2)*rj - yc
                    y3 = sphere2(3,j2)*rj - zc
                    dist = y1*y1 + y2*y2 + y3*y3                                                                           
                    if (dist.gt.1.0d-8) then
                      rdist = 1.0_dp/sqrt(dist)
                      d1rdist = 0.0_dp
                      call cosmoaijderv(ipts,in,k,jpts,jn,l,nearsasrptr,npwt1,npwt2,maxnpwt2, &
                        y1,y2,y3,rdist,d1rdist,w1,w2,dwti,dwtj,d2wti,d2wtj,adrvi,adrvj, &
                        a2drv,ladrv,lgrad2)
!
!  Derivatives with respect to smoothing
!
                      f0 = dwtm1*w1*w2*rdist
                      f1(1:3) = 0.0_dp
                      if (lrweight) then
                        ffa = - w1*w2*rdist*dwdr
                        f1(1) = f1(1) + ffa*xij
                        f1(2) = f1(2) + ffa*yij
                        f1(3) = f1(3) + ffa*zij
                      endif
                      f2(1:6) = 0.0_dp
                      if (lrweight) then
                        rtrm2 = - w1*w2*rdist*d2wdr2
                        rtrm  = - w1*w2*rdist*dwdr
                        f2(1) = f2(1) + rtrm2*xij*xij + rtrm
                        f2(2) = f2(2) + rtrm2*xij*yij
                        f2(3) = f2(3) + rtrm2*yij*yij + rtrm
                        f2(4) = f2(4) + rtrm2*xij*zij
                        f2(5) = f2(5) + rtrm2*yij*zij
                        f2(6) = f2(6) + rtrm2*zij*zij + rtrm
                      endif
                      call cosmoamatdadd(ipts,jpts,f0,f1,f2,qsij,qsipj,sumAinvipts,sumAinvjpts,ldqneeded,lcosmicd2, &
                        lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr,dcosmoA2)
                    endif
                  enddo
                endif
              enddo
!
!  Add contributions of weighting factors to overall derivatives
!                     
              dwtm1swiswj = dwtm1*swi*swj
              qsijdwtm1swiswj = qsij*dwtm1swiswj
              qsipjdwtm1swiswj = qsipj*dwtm1swiswj
              do nn = 1,nearsas
                if (ladrv(nn)) then
                  n = nearsasptr(nn) 
                  nregionn = nregionno(nsft+nrelat(n))
                  if (n .ne. i) then
                    xdrv(i) = xdrv(i) - qsijdwtm1swiswj*adrvi(1,nn)
                    ydrv(i) = ydrv(i) - qsijdwtm1swiswj*adrvi(2,nn)
                    zdrv(i) = zdrv(i) - qsijdwtm1swiswj*adrvi(3,nn)
                    xdrv(n) = xdrv(n) + qsijdwtm1swiswj*adrvi(1,nn)
                    ydrv(n) = ydrv(n) + qsijdwtm1swiswj*adrvi(2,nn)
                    zdrv(n) = zdrv(n) + qsijdwtm1swiswj*adrvi(3,nn)
                    if (nregioni.ne.nregionn) then
                      xregdrv(nregioni) = xregdrv(nregioni) - qsijdwtm1swiswj*adrvi(1,nn)
                      yregdrv(nregioni) = yregdrv(nregioni) - qsijdwtm1swiswj*adrvi(2,nn)
                      zregdrv(nregioni) = zregdrv(nregioni) - qsijdwtm1swiswj*adrvi(3,nn)
                      xregdrv(nregionn) = xregdrv(nregionn) + qsijdwtm1swiswj*adrvi(1,nn)
                      yregdrv(nregionn) = yregdrv(nregionn) + qsijdwtm1swiswj*adrvi(2,nn)
                      zregdrv(nregionn) = zregdrv(nregionn) + qsijdwtm1swiswj*adrvi(3,nn)
                    endif
                  endif
!
!  Save first derivatives of A matrix term
!
                  if (ldqneeded) then
                    dcosmoA(1,jpts,in) = dcosmoA(1,jpts,in) + qsi*adrvi(1,nn)*dwtm1swiswj
                    dcosmoA(2,jpts,in) = dcosmoA(2,jpts,in) + qsi*adrvi(2,nn)*dwtm1swiswj
                    dcosmoA(3,jpts,in) = dcosmoA(3,jpts,in) + qsi*adrvi(3,nn)*dwtm1swiswj
                    dcosmoA(1,ipts,nn) = dcosmoA(1,ipts,nn) - qsj*adrvi(1,nn)*dwtm1swiswj
                    dcosmoA(2,ipts,nn) = dcosmoA(2,ipts,nn) - qsj*adrvi(2,nn)*dwtm1swiswj
                    dcosmoA(3,ipts,nn) = dcosmoA(3,ipts,nn) - qsj*adrvi(3,nn)*dwtm1swiswj
                    dcosmoA(1,jpts,nn) = dcosmoA(1,jpts,nn) - qsi*adrvi(1,nn)*dwtm1swiswj
                    dcosmoA(2,jpts,nn) = dcosmoA(2,jpts,nn) - qsi*adrvi(2,nn)*dwtm1swiswj
                    dcosmoA(3,jpts,nn) = dcosmoA(3,jpts,nn) - qsi*adrvi(3,nn)*dwtm1swiswj
!
                    if (lcosmicd2) then
                      dcosmoAA(1,in,jpts) = dcosmoAA(1,in,jpts) + sumAinvipts*adrvi(1,nn)*dwtm1swiswj
                      dcosmoAA(2,in,jpts) = dcosmoAA(2,in,jpts) + sumAinvipts*adrvi(2,nn)*dwtm1swiswj
                      dcosmoAA(3,in,jpts) = dcosmoAA(3,in,jpts) + sumAinvipts*adrvi(3,nn)*dwtm1swiswj
                      dcosmoAA(1,nn,ipts) = dcosmoAA(1,nn,ipts) - sumAinvjpts*adrvi(1,nn)*dwtm1swiswj
                      dcosmoAA(2,nn,ipts) = dcosmoAA(2,nn,ipts) - sumAinvjpts*adrvi(2,nn)*dwtm1swiswj
                      dcosmoAA(3,nn,ipts) = dcosmoAA(3,nn,ipts) - sumAinvjpts*adrvi(3,nn)*dwtm1swiswj
                      dcosmoAA(1,nn,jpts) = dcosmoAA(1,nn,jpts) - sumAinvipts*adrvi(1,nn)*dwtm1swiswj
                      dcosmoAA(2,nn,jpts) = dcosmoAA(2,nn,jpts) - sumAinvipts*adrvi(2,nn)*dwtm1swiswj
                      dcosmoAA(3,nn,jpts) = dcosmoAA(3,nn,jpts) - sumAinvipts*adrvi(3,nn)*dwtm1swiswj
!
                      dcosmoAA(1,in,ipts) = dcosmoAA(1,in,ipts) + sumAinvjpts*adrvi(1,nn)*dwtm1swiswj
                      dcosmoAA(2,in,ipts) = dcosmoAA(2,in,ipts) + sumAinvjpts*adrvi(2,nn)*dwtm1swiswj
                      dcosmoAA(3,in,ipts) = dcosmoAA(3,in,ipts) + sumAinvjpts*adrvi(3,nn)*dwtm1swiswj
                    endif
                  endif
                  if (n .ne. j) then
                    xdrv(j) = xdrv(j) - qsijdwtm1swiswj*adrvj(1,nn)
                    ydrv(j) = ydrv(j) - qsijdwtm1swiswj*adrvj(2,nn)
                    zdrv(j) = zdrv(j) - qsijdwtm1swiswj*adrvj(3,nn)
                    xdrv(n) = xdrv(n) + qsijdwtm1swiswj*adrvj(1,nn)
                    ydrv(n) = ydrv(n) + qsijdwtm1swiswj*adrvj(2,nn)
                    zdrv(n) = zdrv(n) + qsijdwtm1swiswj*adrvj(3,nn)
                    if (nregionj.ne.nregionn) then
                      xregdrv(nregionj) = xregdrv(nregionj) - qsijdwtm1swiswj*adrvj(1,nn)
                      yregdrv(nregionj) = yregdrv(nregionj) - qsijdwtm1swiswj*adrvj(2,nn)
                      zregdrv(nregionj) = zregdrv(nregionj) - qsijdwtm1swiswj*adrvj(3,nn)
                      xregdrv(nregionn) = xregdrv(nregionn) + qsijdwtm1swiswj*adrvj(1,nn)
                      yregdrv(nregionn) = yregdrv(nregionn) + qsijdwtm1swiswj*adrvj(2,nn)
                      zregdrv(nregionn) = zregdrv(nregionn) + qsijdwtm1swiswj*adrvj(3,nn)
                    endif
                  endif
!
!  Save first derivatives of A matrix term
!
                  if (ldqneeded) then
                    dcosmoA(1,ipts,jn) = dcosmoA(1,ipts,jn) + qsj*adrvj(1,nn)*dwtm1swiswj
                    dcosmoA(2,ipts,jn) = dcosmoA(2,ipts,jn) + qsj*adrvj(2,nn)*dwtm1swiswj
                    dcosmoA(3,ipts,jn) = dcosmoA(3,ipts,jn) + qsj*adrvj(3,nn)*dwtm1swiswj
                    dcosmoA(1,jpts,nn) = dcosmoA(1,jpts,nn) - qsi*adrvj(1,nn)*dwtm1swiswj
                    dcosmoA(2,jpts,nn) = dcosmoA(2,jpts,nn) - qsi*adrvj(2,nn)*dwtm1swiswj
                    dcosmoA(3,jpts,nn) = dcosmoA(3,jpts,nn) - qsi*adrvj(3,nn)*dwtm1swiswj
                    dcosmoA(1,ipts,nn) = dcosmoA(1,ipts,nn) - qsj*adrvj(1,nn)*dwtm1swiswj
                    dcosmoA(2,ipts,nn) = dcosmoA(2,ipts,nn) - qsj*adrvj(2,nn)*dwtm1swiswj
                    dcosmoA(3,ipts,nn) = dcosmoA(3,ipts,nn) - qsj*adrvj(3,nn)*dwtm1swiswj
                    if (lcosmicd2) then
                      dcosmoAA(1,jn,ipts) = dcosmoAA(1,jn,ipts) + sumAinvjpts*adrvj(1,nn)*dwtm1swiswj
                      dcosmoAA(2,jn,ipts) = dcosmoAA(2,jn,ipts) + sumAinvjpts*adrvj(2,nn)*dwtm1swiswj
                      dcosmoAA(3,jn,ipts) = dcosmoAA(3,jn,ipts) + sumAinvjpts*adrvj(3,nn)*dwtm1swiswj
                      dcosmoAA(1,nn,jpts) = dcosmoAA(1,nn,jpts) - sumAinvipts*adrvj(1,nn)*dwtm1swiswj
                      dcosmoAA(2,nn,jpts) = dcosmoAA(2,nn,jpts) - sumAinvipts*adrvj(2,nn)*dwtm1swiswj
                      dcosmoAA(3,nn,jpts) = dcosmoAA(3,nn,jpts) - sumAinvipts*adrvj(3,nn)*dwtm1swiswj
                      dcosmoAA(1,nn,ipts) = dcosmoAA(1,nn,ipts) - sumAinvjpts*adrvj(1,nn)*dwtm1swiswj
                      dcosmoAA(2,nn,ipts) = dcosmoAA(2,nn,ipts) - sumAinvjpts*adrvj(2,nn)*dwtm1swiswj
                      dcosmoAA(3,nn,ipts) = dcosmoAA(3,nn,ipts) - sumAinvjpts*adrvj(3,nn)*dwtm1swiswj
!
                      dcosmoAA(1,jn,jpts) = dcosmoAA(1,jn,jpts) + sumAinvipts*adrvj(1,nn)*dwtm1swiswj
                      dcosmoAA(2,jn,jpts) = dcosmoAA(2,jn,jpts) + sumAinvipts*adrvj(2,nn)*dwtm1swiswj
                      dcosmoAA(3,jn,jpts) = dcosmoAA(3,jn,jpts) + sumAinvipts*adrvj(3,nn)*dwtm1swiswj
                    endif
                  endif
                endif
              enddo
              if (lgrad2) then
                if (lrweight) then
                  rtrm = dwdr*swi*swj
                  qsijr = qsij*rtrm
                  qsipjr = qsipj*rtrm
                endif
!
                do mm = 1,nearsas
                  if (ladrv(mm)) then
                    m = nearsasptr(mm)
                    mx = 3*(m-1) + 1
                    my = mx + 1
                    mz = my + 1
                    do nn = 1,mm
                      if (ladrv(nn)) then
                        n = nearsasptr(nn)
                        if (m.ne.n) then
                          mn = mm*(mm - 1)/2 + nn
                          nx = 3*(n-1) + 1
                          ny = nx + 1
                          nz = ny + 1
                          derv2(nx,mx) = derv2(nx,mx) + qsijdwtm1swiswj*a2drv(1,1,mn)
                          derv2(ny,mx) = derv2(ny,mx) + qsijdwtm1swiswj*a2drv(2,1,mn)
                          derv2(nz,mx) = derv2(nz,mx) + qsijdwtm1swiswj*a2drv(3,1,mn)
                          derv2(nx,my) = derv2(nx,my) + qsijdwtm1swiswj*a2drv(1,2,mn)
                          derv2(ny,my) = derv2(ny,my) + qsijdwtm1swiswj*a2drv(2,2,mn)
                          derv2(nz,my) = derv2(nz,my) + qsijdwtm1swiswj*a2drv(3,2,mn)
                          derv2(nx,mz) = derv2(nx,mz) + qsijdwtm1swiswj*a2drv(1,3,mn)
                          derv2(ny,mz) = derv2(ny,mz) + qsijdwtm1swiswj*a2drv(2,3,mn)
                          derv2(nz,mz) = derv2(nz,mz) + qsijdwtm1swiswj*a2drv(3,3,mn)
                          if (lcosmicd2) then
                            derv2(nx,mx) = derv2(nx,mx) + d2qfct*qsipjdwtm1swiswj*a2drv(1,1,mn)
                            derv2(ny,mx) = derv2(ny,mx) + d2qfct*qsipjdwtm1swiswj*a2drv(2,1,mn)
                            derv2(nz,mx) = derv2(nz,mx) + d2qfct*qsipjdwtm1swiswj*a2drv(3,1,mn)
                            derv2(nx,my) = derv2(nx,my) + d2qfct*qsipjdwtm1swiswj*a2drv(1,2,mn)
                            derv2(ny,my) = derv2(ny,my) + d2qfct*qsipjdwtm1swiswj*a2drv(2,2,mn)
                            derv2(nz,my) = derv2(nz,my) + d2qfct*qsipjdwtm1swiswj*a2drv(3,2,mn)
                            derv2(nx,mz) = derv2(nx,mz) + d2qfct*qsipjdwtm1swiswj*a2drv(1,3,mn)
                            derv2(ny,mz) = derv2(ny,mz) + d2qfct*qsipjdwtm1swiswj*a2drv(2,3,mn)
                            derv2(nz,mz) = derv2(nz,mz) + d2qfct*qsipjdwtm1swiswj*a2drv(3,3,mn)
                          endif
                        endif
                      endif
                    enddo
!
!  Addition derivatives due to distance weighting of i-j
!
                    if (lrweight) then
                      adrvijx = adrvi(1,mm) + adrvj(1,mm)
                      adrvijy = adrvi(2,mm) + adrvj(2,mm)
                      adrvijz = adrvi(3,mm) + adrvj(3,mm)
                      adrvijxx = adrvijx*xij
                      adrvijyx = adrvijy*xij
                      adrvijzx = adrvijz*xij
                      adrvijxy = adrvijx*yij
                      adrvijyy = adrvijy*yij
                      adrvijzy = adrvijz*yij
                      adrvijxz = adrvijx*zij
                      adrvijyz = adrvijy*zij
                      adrvijzz = adrvijz*zij
!
!  M - I
!
                      if (i.gt.m) then
                        derv2(mx,ix) = derv2(mx,ix) + qsijr*adrvijxx
                        derv2(my,ix) = derv2(my,ix) + qsijr*adrvijyx
                        derv2(mz,ix) = derv2(mz,ix) + qsijr*adrvijzx
                        derv2(mx,iy) = derv2(mx,iy) + qsijr*adrvijxy
                        derv2(my,iy) = derv2(my,iy) + qsijr*adrvijyy
                        derv2(mz,iy) = derv2(mz,iy) + qsijr*adrvijzy
                        derv2(mx,iz) = derv2(mx,iz) + qsijr*adrvijxz
                        derv2(my,iz) = derv2(my,iz) + qsijr*adrvijyz
                        derv2(mz,iz) = derv2(mz,iz) + qsijr*adrvijzz
                        if (lcosmicd2) then
                          derv2(mx,ix) = derv2(mx,ix) + d2qfct*qsipjr*adrvijxx
                          derv2(my,ix) = derv2(my,ix) + d2qfct*qsipjr*adrvijyx
                          derv2(mz,ix) = derv2(mz,ix) + d2qfct*qsipjr*adrvijzx
                          derv2(mx,iy) = derv2(mx,iy) + d2qfct*qsipjr*adrvijxy
                          derv2(my,iy) = derv2(my,iy) + d2qfct*qsipjr*adrvijyy
                          derv2(mz,iy) = derv2(mz,iy) + d2qfct*qsipjr*adrvijzy
                          derv2(mx,iz) = derv2(mx,iz) + d2qfct*qsipjr*adrvijxz
                          derv2(my,iz) = derv2(my,iz) + d2qfct*qsipjr*adrvijyz
                          derv2(mz,iz) = derv2(mz,iz) + d2qfct*qsipjr*adrvijzz
                        endif
                      else
                        derv2(ix,mx) = derv2(ix,mx) + qsijr*adrvijxx
                        derv2(iy,mx) = derv2(iy,mx) + qsijr*adrvijxy
                        derv2(iz,mx) = derv2(iz,mx) + qsijr*adrvijxz
                        derv2(ix,my) = derv2(ix,my) + qsijr*adrvijyx
                        derv2(iy,my) = derv2(iy,my) + qsijr*adrvijyy
                        derv2(iz,my) = derv2(iz,my) + qsijr*adrvijyz
                        derv2(ix,mz) = derv2(ix,mz) + qsijr*adrvijzx
                        derv2(iy,mz) = derv2(iy,mz) + qsijr*adrvijzy
                        derv2(iz,mz) = derv2(iz,mz) + qsijr*adrvijzz
                        if (lcosmicd2) then
                          derv2(ix,mx) = derv2(ix,mx) + d2qfct*qsipjr*adrvijxx
                          derv2(iy,mx) = derv2(iy,mx) + d2qfct*qsipjr*adrvijxy
                          derv2(iz,mx) = derv2(iz,mx) + d2qfct*qsipjr*adrvijxz
                          derv2(ix,my) = derv2(ix,my) + d2qfct*qsipjr*adrvijyx
                          derv2(iy,my) = derv2(iy,my) + d2qfct*qsipjr*adrvijyy
                          derv2(iz,my) = derv2(iz,my) + d2qfct*qsipjr*adrvijyz
                          derv2(ix,mz) = derv2(ix,mz) + d2qfct*qsipjr*adrvijzx
                          derv2(iy,mz) = derv2(iy,mz) + d2qfct*qsipjr*adrvijzy
                          derv2(iz,mz) = derv2(iz,mz) + d2qfct*qsipjr*adrvijzz
                        endif
                      endif
!
!  M - J
!
                      if (j.gt.m) then
                        derv2(mx,jx) = derv2(mx,jx) - qsijr*adrvijxx
                        derv2(my,jx) = derv2(my,jx) - qsijr*adrvijyx
                        derv2(mz,jx) = derv2(mz,jx) - qsijr*adrvijzx
                        derv2(mx,jy) = derv2(mx,jy) - qsijr*adrvijxy
                        derv2(my,jy) = derv2(my,jy) - qsijr*adrvijyy
                        derv2(mz,jy) = derv2(mz,jy) - qsijr*adrvijzy
                        derv2(mx,jz) = derv2(mx,jz) - qsijr*adrvijxz
                        derv2(my,jz) = derv2(my,jz) - qsijr*adrvijyz
                        derv2(mz,jz) = derv2(mz,jz) - qsijr*adrvijzz
                        if (lcosmicd2) then
                          derv2(mx,jx) = derv2(mx,jx) - d2qfct*qsipjr*adrvijxx
                          derv2(my,jx) = derv2(my,jx) - d2qfct*qsipjr*adrvijyx
                          derv2(mz,jx) = derv2(mz,jx) - d2qfct*qsipjr*adrvijzx
                          derv2(mx,jy) = derv2(mx,jy) - d2qfct*qsipjr*adrvijxy
                          derv2(my,jy) = derv2(my,jy) - d2qfct*qsipjr*adrvijyy
                          derv2(mz,jy) = derv2(mz,jy) - d2qfct*qsipjr*adrvijzy
                          derv2(mx,jz) = derv2(mx,jz) - d2qfct*qsipjr*adrvijxz
                          derv2(my,jz) = derv2(my,jz) - d2qfct*qsipjr*adrvijyz
                          derv2(mz,jz) = derv2(mz,jz) - d2qfct*qsipjr*adrvijzz
                        endif
                      else
                        derv2(jx,mx) = derv2(jx,mx) - qsijr*adrvijxx
                        derv2(jy,mx) = derv2(jy,mx) - qsijr*adrvijxy
                        derv2(jz,mx) = derv2(jz,mx) - qsijr*adrvijxz
                        derv2(jx,my) = derv2(jx,my) - qsijr*adrvijyx
                        derv2(jy,my) = derv2(jy,my) - qsijr*adrvijyy
                        derv2(jz,my) = derv2(jz,my) - qsijr*adrvijyz
                        derv2(jx,mz) = derv2(jx,mz) - qsijr*adrvijzx
                        derv2(jy,mz) = derv2(jy,mz) - qsijr*adrvijzy
                        derv2(jz,mz) = derv2(jz,mz) - qsijr*adrvijzz
                        if (lcosmicd2) then
                          derv2(jx,mx) = derv2(jx,mx) - d2qfct*qsipjr*adrvijxx
                          derv2(jy,mx) = derv2(jy,mx) - d2qfct*qsipjr*adrvijxy
                          derv2(jz,mx) = derv2(jz,mx) - d2qfct*qsipjr*adrvijxz
                          derv2(jx,my) = derv2(jx,my) - d2qfct*qsipjr*adrvijyx
                          derv2(jy,my) = derv2(jy,my) - d2qfct*qsipjr*adrvijyy
                          derv2(jz,my) = derv2(jz,my) - d2qfct*qsipjr*adrvijyz
                          derv2(jx,mz) = derv2(jx,mz) - d2qfct*qsipjr*adrvijzx
                          derv2(jy,mz) = derv2(jy,mz) - d2qfct*qsipjr*adrvijzy
                          derv2(jz,mz) = derv2(jz,mz) - d2qfct*qsipjr*adrvijzz
                        endif
                      endif
!
!  I - J
!
                      derv2(ix,jx) = derv2(ix,jx) + qsijr*adrvi(1,mm)*xij
                      derv2(iy,jx) = derv2(iy,jx) + qsijr*adrvi(2,mm)*xij
                      derv2(iz,jx) = derv2(iz,jx) + qsijr*adrvi(3,mm)*xij
                      derv2(ix,jy) = derv2(ix,jy) + qsijr*adrvi(1,mm)*yij
                      derv2(iy,jy) = derv2(iy,jy) + qsijr*adrvi(2,mm)*yij
                      derv2(iz,jy) = derv2(iz,jy) + qsijr*adrvi(3,mm)*yij
                      derv2(ix,jz) = derv2(ix,jz) + qsijr*adrvi(1,mm)*zij
                      derv2(iy,jz) = derv2(iy,jz) + qsijr*adrvi(2,mm)*zij
                      derv2(iz,jz) = derv2(iz,jz) + qsijr*adrvi(3,mm)*zij
                      derv2(ix,jx) = derv2(ix,jx) - qsijr*adrvj(1,mm)*xij
                      derv2(iy,jx) = derv2(iy,jx) - qsijr*adrvj(1,mm)*yij
                      derv2(iz,jx) = derv2(iz,jx) - qsijr*adrvj(1,mm)*zij
                      derv2(ix,jy) = derv2(ix,jy) - qsijr*adrvj(2,mm)*xij
                      derv2(iy,jy) = derv2(iy,jy) - qsijr*adrvj(2,mm)*yij
                      derv2(iz,jy) = derv2(iz,jy) - qsijr*adrvj(2,mm)*zij
                      derv2(ix,jz) = derv2(ix,jz) - qsijr*adrvj(3,mm)*xij
                      derv2(iy,jz) = derv2(iy,jz) - qsijr*adrvj(3,mm)*yij
                      derv2(iz,jz) = derv2(iz,jz) - qsijr*adrvj(3,mm)*zij
                      if (lcosmicd2) then
                        derv2(ix,jx) = derv2(ix,jx) + d2qfct*qsipjr*adrvi(1,mm)*xij
                        derv2(iy,jx) = derv2(iy,jx) + d2qfct*qsipjr*adrvi(2,mm)*xij
                        derv2(iz,jx) = derv2(iz,jx) + d2qfct*qsipjr*adrvi(3,mm)*xij
                        derv2(ix,jy) = derv2(ix,jy) + d2qfct*qsipjr*adrvi(1,mm)*yij
                        derv2(iy,jy) = derv2(iy,jy) + d2qfct*qsipjr*adrvi(2,mm)*yij
                        derv2(iz,jy) = derv2(iz,jy) + d2qfct*qsipjr*adrvi(3,mm)*yij
                        derv2(ix,jz) = derv2(ix,jz) + d2qfct*qsipjr*adrvi(1,mm)*zij
                        derv2(iy,jz) = derv2(iy,jz) + d2qfct*qsipjr*adrvi(2,mm)*zij
                        derv2(iz,jz) = derv2(iz,jz) + d2qfct*qsipjr*adrvi(3,mm)*zij
                        derv2(ix,jx) = derv2(ix,jx) - d2qfct*qsipjr*adrvj(1,mm)*xij
                        derv2(iy,jx) = derv2(iy,jx) - d2qfct*qsipjr*adrvj(1,mm)*yij
                        derv2(iz,jx) = derv2(iz,jx) - d2qfct*qsipjr*adrvj(1,mm)*zij
                        derv2(ix,jy) = derv2(ix,jy) - d2qfct*qsipjr*adrvj(2,mm)*xij
                        derv2(iy,jy) = derv2(iy,jy) - d2qfct*qsipjr*adrvj(2,mm)*yij
                        derv2(iz,jy) = derv2(iz,jy) - d2qfct*qsipjr*adrvj(2,mm)*zij
                        derv2(ix,jz) = derv2(ix,jz) - d2qfct*qsipjr*adrvj(3,mm)*xij
                        derv2(iy,jz) = derv2(iy,jz) - d2qfct*qsipjr*adrvj(3,mm)*yij
                        derv2(iz,jz) = derv2(iz,jz) - d2qfct*qsipjr*adrvj(3,mm)*zij
                      endif
                    endif
                  endif
                enddo
                if (lsegsmooth) then
!
!  Cross terms between point and segment smoothing
!
                  do mm = 1,nearsas
                    if (ladrv(mm)) then
                      m = nearsasptr(mm)
                      mx = 3*(m - 1) + 1
                      my = mx + 1
                      mz = my + 1
!
                      rtrm = dwtm1
!
                      qsijrj = qsij*rtrm*swj
!
                      do nn = 1,nnearseg(ipts)
                        n = nnearsegptr(nn,ipts)
                        nx = 3*(n - 1) + 1
                        ny = nx + 1
                        nz = ny + 1
!
!  I - J
!
                        if (i.ne.j) then
                          derv2(ix,jx) = derv2(ix,jx) + qsijrj*adrvj(1,mm)*dsegweight(1,nn,ipts)
                          derv2(iy,jx) = derv2(iy,jx) + qsijrj*adrvj(1,mm)*dsegweight(2,nn,ipts)
                          derv2(iz,jx) = derv2(iz,jx) + qsijrj*adrvj(1,mm)*dsegweight(3,nn,ipts)
                          derv2(ix,jy) = derv2(ix,jy) + qsijrj*adrvj(2,mm)*dsegweight(1,nn,ipts)
                          derv2(iy,jy) = derv2(iy,jy) + qsijrj*adrvj(2,mm)*dsegweight(2,nn,ipts)
                          derv2(iz,jy) = derv2(iz,jy) + qsijrj*adrvj(2,mm)*dsegweight(3,nn,ipts)
                          derv2(ix,jz) = derv2(ix,jz) + qsijrj*adrvj(3,mm)*dsegweight(1,nn,ipts)
                          derv2(iy,jz) = derv2(iy,jz) + qsijrj*adrvj(3,mm)*dsegweight(2,nn,ipts)
                          derv2(iz,jz) = derv2(iz,jz) + qsijrj*adrvj(3,mm)*dsegweight(3,nn,ipts)
                        endif
!
!  M - I
!
                        if (i.gt.m) then
                          derv2(mx,ix) = derv2(mx,ix) - qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,ipts)
                          derv2(my,ix) = derv2(my,ix) - qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,ipts)
                          derv2(mz,ix) = derv2(mz,ix) - qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,ipts)
                          derv2(mx,iy) = derv2(mx,iy) - qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,ipts)
                          derv2(my,iy) = derv2(my,iy) - qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,ipts)
                          derv2(mz,iy) = derv2(mz,iy) - qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,ipts)
                          derv2(mx,iz) = derv2(mx,iz) - qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,ipts)
                          derv2(my,iz) = derv2(my,iz) - qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,ipts)
                          derv2(mz,iz) = derv2(mz,iz) - qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,ipts)
                        elseif (m.gt.i) then
                          derv2(ix,mx) = derv2(ix,mx) - qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,ipts)
                          derv2(iy,mx) = derv2(iy,mx) - qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,ipts)
                          derv2(iz,mx) = derv2(iz,mx) - qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,ipts)
                          derv2(ix,my) = derv2(ix,my) - qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,ipts)
                          derv2(iy,my) = derv2(iy,my) - qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,ipts)
                          derv2(iz,my) = derv2(iz,my) - qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,ipts)
                          derv2(ix,mz) = derv2(ix,mz) - qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,ipts)
                          derv2(iy,mz) = derv2(iy,mz) - qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,ipts)
                          derv2(iz,mz) = derv2(iz,mz) - qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,ipts)
                        endif
!
!  N - J
!
                        if (j.gt.n) then
                          derv2(nx,jx) = derv2(nx,jx) - qsijrj*adrvj(1,mm)*dsegweight(1,nn,ipts)
                          derv2(ny,jx) = derv2(ny,jx) - qsijrj*adrvj(1,mm)*dsegweight(2,nn,ipts)
                          derv2(nz,jx) = derv2(nz,jx) - qsijrj*adrvj(1,mm)*dsegweight(3,nn,ipts)
                          derv2(nx,jy) = derv2(nx,jy) - qsijrj*adrvj(2,mm)*dsegweight(1,nn,ipts)
                          derv2(ny,jy) = derv2(ny,jy) - qsijrj*adrvj(2,mm)*dsegweight(2,nn,ipts)
                          derv2(nz,jy) = derv2(nz,jy) - qsijrj*adrvj(2,mm)*dsegweight(3,nn,ipts)
                          derv2(nx,jz) = derv2(nx,jz) - qsijrj*adrvj(3,mm)*dsegweight(1,nn,ipts)
                          derv2(ny,jz) = derv2(ny,jz) - qsijrj*adrvj(3,mm)*dsegweight(2,nn,ipts)
                          derv2(nz,jz) = derv2(nz,jz) - qsijrj*adrvj(3,mm)*dsegweight(3,nn,ipts)
                        elseif (n.gt.j) then
                          derv2(jx,nx) = derv2(jx,nx) - qsijrj*adrvj(1,mm)*dsegweight(1,nn,ipts)
                          derv2(jy,nx) = derv2(jy,nx) - qsijrj*adrvj(2,mm)*dsegweight(1,nn,ipts)
                          derv2(jz,nx) = derv2(jz,nx) - qsijrj*adrvj(3,mm)*dsegweight(1,nn,ipts)
                          derv2(jx,ny) = derv2(jx,ny) - qsijrj*adrvj(1,mm)*dsegweight(2,nn,ipts)
                          derv2(jy,ny) = derv2(jy,ny) - qsijrj*adrvj(2,mm)*dsegweight(2,nn,ipts)
                          derv2(jz,ny) = derv2(jz,ny) - qsijrj*adrvj(3,mm)*dsegweight(2,nn,ipts)
                          derv2(jx,nz) = derv2(jx,nz) - qsijrj*adrvj(1,mm)*dsegweight(3,nn,ipts)
                          derv2(jy,nz) = derv2(jy,nz) - qsijrj*adrvj(2,mm)*dsegweight(3,nn,ipts)
                          derv2(jz,nz) = derv2(jz,nz) - qsijrj*adrvj(3,mm)*dsegweight(3,nn,ipts)
                        endif
!
!  I - N
!
                        if (i.gt.n) then
                          derv2(nx,ix) = derv2(nx,ix) - qsijrj*adrvi(1,mm)*dsegweight(1,nn,ipts)
                          derv2(ny,ix) = derv2(ny,ix) - qsijrj*adrvi(1,mm)*dsegweight(2,nn,ipts)
                          derv2(nz,ix) = derv2(nz,ix) - qsijrj*adrvi(1,mm)*dsegweight(3,nn,ipts)
                          derv2(nx,iy) = derv2(nx,iy) - qsijrj*adrvi(2,mm)*dsegweight(1,nn,ipts)
                          derv2(ny,iy) = derv2(ny,iy) - qsijrj*adrvi(2,mm)*dsegweight(2,nn,ipts)
                          derv2(nz,iy) = derv2(nz,iy) - qsijrj*adrvi(2,mm)*dsegweight(3,nn,ipts)
                          derv2(nx,iz) = derv2(nx,iz) - qsijrj*adrvi(3,mm)*dsegweight(1,nn,ipts)
                          derv2(ny,iz) = derv2(ny,iz) - qsijrj*adrvi(3,mm)*dsegweight(2,nn,ipts)
                          derv2(nz,iz) = derv2(nz,iz) - qsijrj*adrvi(3,mm)*dsegweight(3,nn,ipts)
                        elseif (n.gt.i) then
                          derv2(ix,nx) = derv2(ix,nx) - qsijrj*adrvi(1,mm)*dsegweight(1,nn,ipts)
                          derv2(iy,nx) = derv2(iy,nx) - qsijrj*adrvi(2,mm)*dsegweight(1,nn,ipts)
                          derv2(iz,nx) = derv2(iz,nx) - qsijrj*adrvi(3,mm)*dsegweight(1,nn,ipts)
                          derv2(ix,ny) = derv2(ix,ny) - qsijrj*adrvi(1,mm)*dsegweight(2,nn,ipts)
                          derv2(iy,ny) = derv2(iy,ny) - qsijrj*adrvi(2,mm)*dsegweight(2,nn,ipts)
                          derv2(iz,ny) = derv2(iz,ny) - qsijrj*adrvi(3,mm)*dsegweight(2,nn,ipts)
                          derv2(ix,nz) = derv2(ix,nz) - qsijrj*adrvi(1,mm)*dsegweight(3,nn,ipts)
                          derv2(iy,nz) = derv2(iy,nz) - qsijrj*adrvi(2,mm)*dsegweight(3,nn,ipts)
                          derv2(iz,nz) = derv2(iz,nz) - qsijrj*adrvi(3,mm)*dsegweight(3,nn,ipts)
                        endif
!
!  M - N
!
                        if (m.gt.n) then
                          derv2(nx,mx) = derv2(nx,mx) + qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,ipts)
                          derv2(ny,mx) = derv2(ny,mx) + qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,ipts)
                          derv2(nz,mx) = derv2(nz,mx) + qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,ipts)
                          derv2(nx,my) = derv2(nx,my) + qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,ipts)
                          derv2(ny,my) = derv2(ny,my) + qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,ipts)
                          derv2(nz,my) = derv2(nz,my) + qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,ipts)
                          derv2(nx,mz) = derv2(nx,mz) + qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,ipts)
                          derv2(ny,mz) = derv2(ny,mz) + qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,ipts)
                          derv2(nz,mz) = derv2(nz,mz) + qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,ipts)
                        elseif (n.gt.m) then
                          derv2(mx,nx) = derv2(mx,nx) + qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,ipts)
                          derv2(my,nx) = derv2(my,nx) + qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,ipts)
                          derv2(mz,nx) = derv2(mz,nx) + qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,ipts)
                          derv2(mx,ny) = derv2(mx,ny) + qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,ipts)
                          derv2(my,ny) = derv2(my,ny) + qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,ipts)
                          derv2(mz,ny) = derv2(mz,ny) + qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,ipts)
                          derv2(mx,nz) = derv2(mx,nz) + qsijrj*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,ipts)
                          derv2(my,nz) = derv2(my,nz) + qsijrj*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,ipts)
                          derv2(mz,nz) = derv2(mz,nz) + qsijrj*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,ipts)
                        endif
                      enddo
!
                      qsijri = qsij*rtrm*swi
!
                      do nn = 1,nnearseg(jpts)
                        n = nnearsegptr(nn,jpts)
                        nx = 3*(n - 1) + 1
                        ny = nx + 1
                        nz = ny + 1
!
!  I - J
!
                        if (i.ne.j) then
                          derv2(ix,jx) = derv2(ix,jx) + qsijri*adrvi(1,mm)*dsegweight(1,nn,jpts)
                          derv2(iy,jx) = derv2(iy,jx) + qsijri*adrvi(2,mm)*dsegweight(1,nn,jpts)
                          derv2(iz,jx) = derv2(iz,jx) + qsijri*adrvi(3,mm)*dsegweight(1,nn,jpts)
                          derv2(ix,jy) = derv2(ix,jy) + qsijri*adrvi(1,mm)*dsegweight(2,nn,jpts)
                          derv2(iy,jy) = derv2(iy,jy) + qsijri*adrvi(2,mm)*dsegweight(2,nn,jpts)
                          derv2(iz,jy) = derv2(iz,jy) + qsijri*adrvi(3,mm)*dsegweight(2,nn,jpts)
                          derv2(ix,jz) = derv2(ix,jz) + qsijri*adrvi(1,mm)*dsegweight(3,nn,jpts)
                          derv2(iy,jz) = derv2(iy,jz) + qsijri*adrvi(2,mm)*dsegweight(3,nn,jpts)
                          derv2(iz,jz) = derv2(iz,jz) + qsijri*adrvi(3,mm)*dsegweight(3,nn,jpts)
                        endif
!
!  M - J
!
                        if (j.gt.m) then
                          derv2(mx,jx) = derv2(mx,jx) - qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,jpts)
                          derv2(my,jx) = derv2(my,jx) - qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,jpts)
                          derv2(mz,jx) = derv2(mz,jx) - qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,jpts)
                          derv2(mx,jy) = derv2(mx,jy) - qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,jpts)
                          derv2(my,jy) = derv2(my,jy) - qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,jpts)
                          derv2(mz,jy) = derv2(mz,jy) - qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,jpts)
                          derv2(mx,jz) = derv2(mx,jz) - qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,jpts)
                          derv2(my,jz) = derv2(my,jz) - qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,jpts)
                          derv2(mz,jz) = derv2(mz,jz) - qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,jpts)
                        elseif (m.gt.j) then
                          derv2(jx,mx) = derv2(jx,mx) - qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,jpts)
                          derv2(jy,mx) = derv2(jy,mx) - qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,jpts)
                          derv2(jz,mx) = derv2(jz,mx) - qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,jpts)
                          derv2(jx,my) = derv2(jx,my) - qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,jpts)
                          derv2(jy,my) = derv2(jy,my) - qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,jpts)
                          derv2(jz,my) = derv2(jz,my) - qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,jpts)
                          derv2(jx,mz) = derv2(jx,mz) - qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,jpts)
                          derv2(jy,mz) = derv2(jy,mz) - qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,jpts)
                          derv2(jz,mz) = derv2(jz,mz) - qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,jpts)
                        endif
!
!  N - I
!
                        if (i.gt.n) then
                          derv2(nx,ix) = derv2(nx,ix) - qsijri*adrvi(1,mm)*dsegweight(1,nn,jpts)
                          derv2(ny,ix) = derv2(ny,ix) - qsijri*adrvi(1,mm)*dsegweight(2,nn,jpts)
                          derv2(nz,ix) = derv2(nz,ix) - qsijri*adrvi(1,mm)*dsegweight(3,nn,jpts)
                          derv2(nx,iy) = derv2(nx,iy) - qsijri*adrvi(2,mm)*dsegweight(1,nn,jpts)
                          derv2(ny,iy) = derv2(ny,iy) - qsijri*adrvi(2,mm)*dsegweight(2,nn,jpts)
                          derv2(nz,iy) = derv2(nz,iy) - qsijri*adrvi(2,mm)*dsegweight(3,nn,jpts)
                          derv2(nx,iz) = derv2(nx,iz) - qsijri*adrvi(3,mm)*dsegweight(1,nn,jpts)
                          derv2(ny,iz) = derv2(ny,iz) - qsijri*adrvi(3,mm)*dsegweight(2,nn,jpts)
                          derv2(nz,iz) = derv2(nz,iz) - qsijri*adrvi(3,mm)*dsegweight(3,nn,jpts)
                        elseif (n.gt.i) then
                          derv2(ix,nx) = derv2(ix,nx) - qsijri*adrvi(1,mm)*dsegweight(1,nn,jpts)
                          derv2(iy,nx) = derv2(iy,nx) - qsijri*adrvi(2,mm)*dsegweight(1,nn,jpts)
                          derv2(iz,nx) = derv2(iz,nx) - qsijri*adrvi(3,mm)*dsegweight(1,nn,jpts)
                          derv2(ix,ny) = derv2(ix,ny) - qsijri*adrvi(1,mm)*dsegweight(2,nn,jpts)
                          derv2(iy,ny) = derv2(iy,ny) - qsijri*adrvi(2,mm)*dsegweight(2,nn,jpts)
                          derv2(iz,ny) = derv2(iz,ny) - qsijri*adrvi(3,mm)*dsegweight(2,nn,jpts)
                          derv2(ix,nz) = derv2(ix,nz) - qsijri*adrvi(1,mm)*dsegweight(3,nn,jpts)
                          derv2(iy,nz) = derv2(iy,nz) - qsijri*adrvi(2,mm)*dsegweight(3,nn,jpts)
                          derv2(iz,nz) = derv2(iz,nz) - qsijri*adrvi(3,mm)*dsegweight(3,nn,jpts)
                        endif
!
!  J - N
!
                        if (j.gt.n) then
                          derv2(nx,jx) = derv2(nx,jx) - qsijri*adrvj(1,mm)*dsegweight(1,nn,jpts)
                          derv2(ny,jx) = derv2(ny,jx) - qsijri*adrvj(1,mm)*dsegweight(2,nn,jpts)
                          derv2(nz,jx) = derv2(nz,jx) - qsijri*adrvj(1,mm)*dsegweight(3,nn,jpts)
                          derv2(nx,jy) = derv2(nx,jy) - qsijri*adrvj(2,mm)*dsegweight(1,nn,jpts)
                          derv2(ny,jy) = derv2(ny,jy) - qsijri*adrvj(2,mm)*dsegweight(2,nn,jpts)
                          derv2(nz,jy) = derv2(nz,jy) - qsijri*adrvj(2,mm)*dsegweight(3,nn,jpts)
                          derv2(nx,jz) = derv2(nx,jz) - qsijri*adrvj(3,mm)*dsegweight(1,nn,jpts)
                          derv2(ny,jz) = derv2(ny,jz) - qsijri*adrvj(3,mm)*dsegweight(2,nn,jpts)
                          derv2(nz,jz) = derv2(nz,jz) - qsijri*adrvj(3,mm)*dsegweight(3,nn,jpts)
                        elseif (n.gt.j) then
                          derv2(jx,nx) = derv2(jx,nx) - qsijri*adrvj(1,mm)*dsegweight(1,nn,jpts)
                          derv2(jy,nx) = derv2(jy,nx) - qsijri*adrvj(2,mm)*dsegweight(1,nn,jpts)
                          derv2(jz,nx) = derv2(jz,nx) - qsijri*adrvj(3,mm)*dsegweight(1,nn,jpts)
                          derv2(jx,ny) = derv2(jx,ny) - qsijri*adrvj(1,mm)*dsegweight(2,nn,jpts)
                          derv2(jy,ny) = derv2(jy,ny) - qsijri*adrvj(2,mm)*dsegweight(2,nn,jpts)
                          derv2(jz,ny) = derv2(jz,ny) - qsijri*adrvj(3,mm)*dsegweight(2,nn,jpts)
                          derv2(jx,nz) = derv2(jx,nz) - qsijri*adrvj(1,mm)*dsegweight(3,nn,jpts)
                          derv2(jy,nz) = derv2(jy,nz) - qsijri*adrvj(2,mm)*dsegweight(3,nn,jpts)
                          derv2(jz,nz) = derv2(jz,nz) - qsijri*adrvj(3,mm)*dsegweight(3,nn,jpts)
                        endif
!
!  M - N
!
                        if (m.gt.n) then
                          derv2(nx,mx) = derv2(nx,mx) + qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,jpts)
                          derv2(ny,mx) = derv2(ny,mx) + qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,jpts)
                          derv2(nz,mx) = derv2(nz,mx) + qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,jpts)
                          derv2(nx,my) = derv2(nx,my) + qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,jpts)
                          derv2(ny,my) = derv2(ny,my) + qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,jpts)
                          derv2(nz,my) = derv2(nz,my) + qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,jpts)
                          derv2(nx,mz) = derv2(nx,mz) + qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,jpts)
                          derv2(ny,mz) = derv2(ny,mz) + qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,jpts)
                          derv2(nz,mz) = derv2(nz,mz) + qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,jpts)
                        elseif (n.gt.m) then
                          derv2(mx,nx) = derv2(mx,nx) + qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(1,nn,jpts)
                          derv2(my,nx) = derv2(my,nx) + qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(1,nn,jpts)
                          derv2(mz,nx) = derv2(mz,nx) + qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(1,nn,jpts)
                          derv2(mx,ny) = derv2(mx,ny) + qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(2,nn,jpts)
                          derv2(my,ny) = derv2(my,ny) + qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(2,nn,jpts)
                          derv2(mz,ny) = derv2(mz,ny) + qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(2,nn,jpts)
                          derv2(mx,nz) = derv2(mx,nz) + qsijri*(adrvi(1,mm) + adrvj(1,mm))*dsegweight(3,nn,jpts)
                          derv2(my,nz) = derv2(my,nz) + qsijri*(adrvi(2,mm) + adrvj(2,mm))*dsegweight(3,nn,jpts)
                          derv2(mz,nz) = derv2(mz,nz) + qsijri*(adrvi(3,mm) + adrvj(3,mm))*dsegweight(3,nn,jpts)
                        endif
                      enddo
!
                    endif
                  enddo
                endif
              endif
!
!  For periodic case, subtract 1/r term to avoid double counting with the term from the long range sum.
!
              if (lperiodic.and.dists2.gt.1.0d-15) then
                call qmatrixelementc(xij,yij,zij,0.0_dp,.false.,.true.,lgrad2,aij,dqme,d2qme)
                f0    = - aij
                f1(1:3) = dqme(1:3)
                f2(1:6) = d2qme(1:6)
                call cosmoamatdadd(ipts,jpts,f0,f1,f2,qsij,qsipj,sumAinvipts,sumAinvjpts,ldqneeded,lcosmicd2, &
                  lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr,dcosmoA2)
              endif
!
!  Rezero derivatives
!
              call cosmoaijzero(ipts,in,jpts,jn,nearsasrptr,npwt1,npwt2,adrvi,adrvj,a2drv,ladrv,lgrad2)
            else
!
!  Coulomb element only needs adding for a cluster since this is handled for periodic systems via qmatrixelementc.
!
              if (.not.lperiodic.and.dists2.gt.1.0d-8) then
                if (lPureCoulomb0D) then
                  aij = 1.0_dp/sqrt(dists2)
                  f0    = aij
                  rtrm  = aij**3
                  f1(1) = - rtrm*xij
                  f1(2) = - rtrm*yij
                  f1(3) = - rtrm*zij
                  f2(1) = rtrm*(3.0_dp*xij*xij*aij**2 - 1.0_dp)
                  f2(2) = rtrm*3.0_dp*xij*yij*aij**2
                  f2(3) = rtrm*(3.0_dp*yij*yij*aij**2 - 1.0_dp)
                  f2(4) = rtrm*3.0_dp*xij*zij*aij**2
                  f2(5) = rtrm*3.0_dp*yij*zij*aij**2
                  f2(6) = rtrm*(3.0_dp*zij*zij*aij**2 - 1.0_dp)
                else
                  call qmatrixelementc(xji,yji,zji,0.0_dp,.false.,.true.,lgrad2,qme,dqme,d2qme)
                  f0 = qme
                  f1(1:3) = - dqme(1:3)
                  f2(1:6) = - d2qme(1:6)
                endif
                call cosmoamatdadd(ipts,jpts,f0,f1,f2,qsij,qsipj,sumAinvipts,sumAinvjpts,ldqneeded,lcosmicd2, &
                  lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr,dcosmoA2)
              endif
            endif
          endif
!
!  End loop over cell images
!
        enddo
!
!  Long range contribution to derivatives for periodic systems
!
        if (lperiodic.and.(ipts.ne.jpts)) then
          call qmatrixelementc(xji,yji,zji,0.0_dp,.true.,.true.,lgrad2,qme,dqme,d2qme)
          f0 = qme
          f1(1:3) = - dqme(1:3)
          f2(1:6) = - d2qme(1:6)
          call cosmoamatdadd(ipts,jpts,f0,f1,f2,qsij,qsipj,sumAinvipts,sumAinvjpts,ldqneeded,lcosmicd2, &
            lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr,dcosmoA2)
        endif
!
!  End if for lopi/lopj
!
      endif
!
!  End loops over segment points
!
    enddo
  enddo
!
!  Deallocate array for local derivative storage
!
  call realloc(d2wtj,0_i4,0_i4,0_i4,0_i4,ierror)
  call realloc(d2wti,0_i4,0_i4,0_i4,0_i4,ierror)
  call realloc(a2drv,0_i4,0_i4,0_i4,ierror)
  deallocate(ladrv,stat=status)
  if (status/=0) call deallocate_error('setdcosmoamat','ladrv')
  call realloc(adrvj,0_i4,0_i4,ierror)
  call realloc(adrvi,0_i4,0_i4,ierror)
  call realloc(dwtj,0_i4,0_i4,0_i4,ierror)
  call realloc(dwti,0_i4,0_i4,0_i4,ierror)
!
!  Free local memory
!
  deallocate(totalwt,stat=status)
  if (status/=0) call deallocate_error('setdcosmoamat','totalwt')
!
  return
  end
