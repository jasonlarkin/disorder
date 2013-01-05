  subroutine cosmoamatdadd(ipts,jpts,f0,f1,f2,qsij,qsipj,sumAinvipts,sumAinvjpts,ldqneeded,lcosmicd2, &
                           lgrad2,dcosmoA,dcosmoAA,d2qfct,dsegweight,d2segweight,nearsas,nearsasrptr, &
                           dcosmoA2)
!
!  Subroutine adds contributions from a term in the cosmo A matrix to the first and second 
!  derivative matrices, as appropriate.
!
!  On input :
!
!  ipts     = first segment pointer, associated with atom i
!  jpts     = second segment pointer, associated with atom j
!  f0      = A matrix term
!  f1(3)   = Cartesian first derivatives of A matrix term with respect to rij
!  f2(6)   = Cartesian second derivatives of A matrix term with respect to rij
!
!   7/03 Created from setdcosmoamat
!  12/04 Style updated & local scalars added
!   1/05 Contributions to d2qsassum added for smoothing case
!   1/05 Contributions to dcosmoAA added for smoothing case
!   1/05 d2qsassum eliminated by direct addition to derv2
!   1/05 cosmicd2 contributions wrapped into normal second derivative additions
!  12/08 Migrated to version 3.5 and converted to f90 format
!  11/09 Region derivatives added
!   7/10 Referencing of nregionno corrected to refer to asymmetric unit
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
  use current,        only : nsft, nrelat
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4)                                   :: ipts
  integer(i4)                                   :: jpts
  integer(i4)                                   :: nearsas
  integer(i4)                                   :: nearsasrptr(*)
  logical                                       :: lcosmicd2
  logical                                       :: ldqneeded
  logical                                       :: lgrad2
  real(dp)                                      :: d2qfct
  real(dp)                                      :: dcosmoA(3,npts,*)
  real(dp)                                      :: dcosmoA2(3,npts,*)
  real(dp)                                      :: dcosmoAA(3,nearsas,*)
  real(dp)                                      :: dsegweight(3,maxnearseg,*)
  real(dp)                                      :: d2segweight(3,3,maxnearseg,maxnearseg,*)
  real(dp)                                      :: f0
  real(dp)                                      :: f1(3)
  real(dp)                                      :: f2(6)
  real(dp)                                      :: qsij
  real(dp)                                      :: qsipj
  real(dp)                                      :: sumAinvipts
  real(dp)                                      :: sumAinvjpts
!
!  Local variables
!
  integer(i4)                                   :: i
  integer(i4)                                   :: ia
  integer(i4)                                   :: in
  integer(i4)                                   :: ix
  integer(i4)                                   :: iy
  integer(i4)                                   :: iz
  integer(i4)                                   :: j
  integer(i4)                                   :: ja
  integer(i4)                                   :: jn
  integer(i4)                                   :: jx
  integer(i4)                                   :: jy
  integer(i4)                                   :: jz
  integer(i4)                                   :: k
  integer(i4)                                   :: kk
  integer(i4)                                   :: kn
  integer(i4)                                   :: kx
  integer(i4)                                   :: ky
  integer(i4)                                   :: kz
  integer(i4)                                   :: l
  integer(i4)                                   :: ll
  integer(i4)                                   :: ln
  integer(i4)                                   :: lx
  integer(i4)                                   :: ly
  integer(i4)                                   :: lz
  integer(i4)                                   :: m
  integer(i4)                                   :: mm
  integer(i4)                                   :: mn
  integer(i4)                                   :: nregioni
  integer(i4)                                   :: nregionj
  integer(i4)                                   :: nregionm
  real(dp)                                      :: f0trm
  real(dp)                                      :: qsi
  real(dp)                                      :: qsj
  real(dp)                                      :: qsijw
  real(dp)                                      :: qsipjw
  real(dp)                                      :: smtrm
  real(dp)                                      :: smtrmi
  real(dp)                                      :: smtrmj
  real(dp)                                      :: smtrmp
  real(dp)                                      :: smtrmpi
  real(dp)                                      :: smtrmpj
  real(dp)                                      :: swi
  real(dp)                                      :: swj
  real(dp)                                      :: trm
  real(dp)                                      :: trmi
  real(dp)                                      :: trmj
!
!  Set local constants
!
  i = cosmoatomptr(ipts)
  ia = nrelat(i)
  in = nearsasrptr(i)
  qsi = qonsas(ipts)
  swi = segweight(ipts)
  nregioni = nregionno(nsft+ia)
!
  j = cosmoatomptr(jpts)
  ja = nrelat(j)
  jn = nearsasrptr(j)
  qsj = qonsas(jpts)
  swj = segweight(jpts)
  nregionj = nregionno(nsft+ja)
!
  qsijw = qsij*swi*swj
  qsipjw = qsipj*swi*swj
!**************************************
!  First derivatives of the A matrix  *
!**************************************
  if (i.ne.j) then
    xdrv(i) = xdrv(i) - qsijw*f1(1)
    ydrv(i) = ydrv(i) - qsijw*f1(2)
    zdrv(i) = zdrv(i) - qsijw*f1(3)
    xdrv(j) = xdrv(j) + qsijw*f1(1)
    ydrv(j) = ydrv(j) + qsijw*f1(2)
    zdrv(j) = zdrv(j) + qsijw*f1(3)
    if (nregioni.ne.nregionj) then
      xregdrv(nregioni) = xregdrv(nregioni) - qsijw*f1(1)
      yregdrv(nregioni) = yregdrv(nregioni) - qsijw*f1(2)
      zregdrv(nregioni) = zregdrv(nregioni) - qsijw*f1(3)
      xregdrv(nregionj) = xregdrv(nregionj) + qsijw*f1(1)
      yregdrv(nregionj) = yregdrv(nregionj) + qsijw*f1(2)
      zregdrv(nregionj) = zregdrv(nregionj) + qsijw*f1(3)
    endif
  endif
!
!  Derivatives with respect to smoothing of segments
!
  if (lsegsmooth) then
    if (ipts.eq.jpts) then
      smtrm = 0.5_dp*qsij*f0
    else
      smtrm = qsij*f0
    endif
    smtrmi = smtrm*swi
    smtrmj = smtrm*swj
    do mm = 1,nnearseg(ipts)
      m = nnearsegptr(mm,ipts)
      xdrv(i) = xdrv(i) - smtrmj*dsegweight(1,mm,ipts)
      ydrv(i) = ydrv(i) - smtrmj*dsegweight(2,mm,ipts)
      zdrv(i) = zdrv(i) - smtrmj*dsegweight(3,mm,ipts)
      xdrv(m) = xdrv(m) + smtrmj*dsegweight(1,mm,ipts)
      ydrv(m) = ydrv(m) + smtrmj*dsegweight(2,mm,ipts)
      zdrv(m) = zdrv(m) + smtrmj*dsegweight(3,mm,ipts)
      nregionm = nregionno(nsft+nrelat(m))
      if (nregioni.ne.nregionm) then
        xregdrv(nregioni) = xregdrv(nregioni) - smtrmj*dsegweight(1,mm,ipts)
        yregdrv(nregioni) = yregdrv(nregioni) - smtrmj*dsegweight(2,mm,ipts)
        zregdrv(nregioni) = zregdrv(nregioni) - smtrmj*dsegweight(3,mm,ipts)
        xregdrv(nregionm) = xregdrv(nregionm) + smtrmj*dsegweight(1,mm,ipts)
        yregdrv(nregionm) = yregdrv(nregionm) + smtrmj*dsegweight(2,mm,ipts)
        zregdrv(nregionm) = zregdrv(nregionm) + smtrmj*dsegweight(3,mm,ipts)
      endif
    enddo
    do mm = 1,nnearseg(jpts)
      m = nnearsegptr(mm,jpts)
      xdrv(j) = xdrv(j) - smtrmi*dsegweight(1,mm,jpts)
      ydrv(j) = ydrv(j) - smtrmi*dsegweight(2,mm,jpts)
      zdrv(j) = zdrv(j) - smtrmi*dsegweight(3,mm,jpts)
      xdrv(m) = xdrv(m) + smtrmi*dsegweight(1,mm,jpts)
      ydrv(m) = ydrv(m) + smtrmi*dsegweight(2,mm,jpts)
      zdrv(m) = zdrv(m) + smtrmi*dsegweight(3,mm,jpts)
      nregionm = nregionno(nsft+nrelat(m))
      if (nregionj.ne.nregionm) then
        xregdrv(nregionj) = xregdrv(nregionj) - smtrmi*dsegweight(1,mm,jpts)
        yregdrv(nregionj) = yregdrv(nregionj) - smtrmi*dsegweight(2,mm,jpts)
        zregdrv(nregionj) = zregdrv(nregionj) - smtrmi*dsegweight(3,mm,jpts)
        xregdrv(nregionm) = xregdrv(nregionm) + smtrmi*dsegweight(1,mm,jpts)
        yregdrv(nregionm) = yregdrv(nregionm) + smtrmi*dsegweight(2,mm,jpts)
        zregdrv(nregionm) = zregdrv(nregionm) + smtrmi*dsegweight(3,mm,jpts)
      endif
    enddo
  endif
!
  if (ldqneeded) then
!
!  Save first derivatives of A matrix term
!
    if (i.ne.j) then
      dcosmoA(1,ipts,jn) = dcosmoA(1,ipts,jn) - f1(1)*qsj*swi*swj
      dcosmoA(2,ipts,jn) = dcosmoA(2,ipts,jn) - f1(2)*qsj*swi*swj
      dcosmoA(3,ipts,jn) = dcosmoA(3,ipts,jn) - f1(3)*qsj*swi*swj
      dcosmoA(1,jpts,in) = dcosmoA(1,jpts,in) + f1(1)*qsi*swi*swj
      dcosmoA(2,jpts,in) = dcosmoA(2,jpts,in) + f1(2)*qsi*swi*swj
      dcosmoA(3,jpts,in) = dcosmoA(3,jpts,in) + f1(3)*qsi*swi*swj
    endif
!
!  Smoothing contribution to dcosmoA
!
    if (lsegsmooth) then
      if (ipts.eq.jpts) then
        smtrm = 0.5_dp*f0
      else
        smtrm = f0
      endif
      smtrmi = smtrm*qsi*swj
      smtrmj = smtrm*qsj*swj
      do mm = 1,nnearseg(ipts)
        m = nnearsegptr(mm,ipts)
        mn = nearsasrptr(m)
        dcosmoA(1,ipts,mn) = dcosmoA(1,ipts,mn) - smtrmj*dsegweight(1,mm,ipts)
        dcosmoA(2,ipts,mn) = dcosmoA(2,ipts,mn) - smtrmj*dsegweight(2,mm,ipts)
        dcosmoA(3,ipts,mn) = dcosmoA(3,ipts,mn) - smtrmj*dsegweight(3,mm,ipts)
        dcosmoA2(1,jpts,mn) = dcosmoA2(1,jpts,mn) - smtrmi*dsegweight(1,mm,ipts)
        dcosmoA2(2,jpts,mn) = dcosmoA2(2,jpts,mn) - smtrmi*dsegweight(2,mm,ipts)
        dcosmoA2(3,jpts,mn) = dcosmoA2(3,jpts,mn) - smtrmi*dsegweight(3,mm,ipts)
        dcosmoA2(1,jpts,in) = dcosmoA2(1,jpts,in) + smtrmi*dsegweight(1,mm,ipts)
        dcosmoA2(2,jpts,in) = dcosmoA2(2,jpts,in) + smtrmi*dsegweight(2,mm,ipts)
        dcosmoA2(3,jpts,in) = dcosmoA2(3,jpts,in) + smtrmi*dsegweight(3,mm,ipts)
      enddo
      smtrmi = smtrm*qsi*swi
      smtrmj = smtrm*qsj*swi
      do mm = 1,nnearseg(jpts)
        m = nnearsegptr(mm,jpts)
        mn = nearsasrptr(m)
        dcosmoA(1,jpts,mn) = dcosmoA(1,jpts,mn) - smtrmi*dsegweight(1,mm,jpts)
        dcosmoA(2,jpts,mn) = dcosmoA(2,jpts,mn) - smtrmi*dsegweight(2,mm,jpts)
        dcosmoA(3,jpts,mn) = dcosmoA(3,jpts,mn) - smtrmi*dsegweight(3,mm,jpts)
        dcosmoA2(1,ipts,mn) = dcosmoA2(1,ipts,mn) - smtrmj*dsegweight(1,mm,jpts)
        dcosmoA2(2,ipts,mn) = dcosmoA2(2,ipts,mn) - smtrmj*dsegweight(2,mm,jpts)
        dcosmoA2(3,ipts,mn) = dcosmoA2(3,ipts,mn) - smtrmj*dsegweight(3,mm,jpts)
        dcosmoA2(1,ipts,jn) = dcosmoA2(1,ipts,jn) + smtrmj*dsegweight(1,mm,jpts)
        dcosmoA2(2,ipts,jn) = dcosmoA2(2,ipts,jn) + smtrmj*dsegweight(2,mm,jpts)
        dcosmoA2(3,ipts,jn) = dcosmoA2(3,ipts,jn) + smtrmj*dsegweight(3,mm,jpts)
      enddo
    endif
  endif
  if (lgrad2) then
!***********************************
!  Second derivatives of A matrix  *
!***********************************
    ix = 3*(i - 1) + 1
    iy = ix + 1
    iz = ix + 2
    jx = 3*(j - 1) + 1
    jy = jx + 1
    jz = jx + 2
!
    derv2(ix,jx) = derv2(ix,jx) - qsijw*f2(1)
    derv2(iy,jx) = derv2(iy,jx) - qsijw*f2(2)
    derv2(iz,jx) = derv2(iz,jx) - qsijw*f2(4)
    derv2(ix,jy) = derv2(ix,jy) - qsijw*f2(2)
    derv2(iy,jy) = derv2(iy,jy) - qsijw*f2(3)
    derv2(iz,jy) = derv2(iz,jy) - qsijw*f2(5)
    derv2(ix,jz) = derv2(ix,jz) - qsijw*f2(4)
    derv2(iy,jz) = derv2(iy,jz) - qsijw*f2(5)
    derv2(iz,jz) = derv2(iz,jz) - qsijw*f2(6)
    if (lcosmicd2) then
      derv2(ix,jx) = derv2(ix,jx) - d2qfct*qsipjw*f2(1)
      derv2(iy,jx) = derv2(iy,jx) - d2qfct*qsipjw*f2(2)
      derv2(iz,jx) = derv2(iz,jx) - d2qfct*qsipjw*f2(4)
      derv2(ix,jy) = derv2(ix,jy) - d2qfct*qsipjw*f2(2)
      derv2(iy,jy) = derv2(iy,jy) - d2qfct*qsipjw*f2(3)
      derv2(iz,jy) = derv2(iz,jy) - d2qfct*qsipjw*f2(5)
      derv2(ix,jz) = derv2(ix,jz) - d2qfct*qsipjw*f2(4)
      derv2(iy,jz) = derv2(iy,jz) - d2qfct*qsipjw*f2(5)
      derv2(iz,jz) = derv2(iz,jz) - d2qfct*qsipjw*f2(6)
!
      dcosmoAA(1,jn,ipts) = dcosmoAA(1,jn,ipts) - f1(1)*sumAinvjpts*swi*swj
      dcosmoAA(2,jn,ipts) = dcosmoAA(2,jn,ipts) - f1(2)*sumAinvjpts*swi*swj
      dcosmoAA(3,jn,ipts) = dcosmoAA(3,jn,ipts) - f1(3)*sumAinvjpts*swi*swj
      dcosmoAA(1,in,jpts) = dcosmoAA(1,in,jpts) + f1(1)*sumAinvipts*swi*swj
      dcosmoAA(2,in,jpts) = dcosmoAA(2,in,jpts) + f1(2)*sumAinvipts*swi*swj
      dcosmoAA(3,in,jpts) = dcosmoAA(3,in,jpts) + f1(3)*sumAinvipts*swi*swj
!
      dcosmoAA(1,jn,jpts) = dcosmoAA(1,jn,jpts) - f1(1)*sumAinvipts*swi*swj
      dcosmoAA(2,jn,jpts) = dcosmoAA(2,jn,jpts) - f1(2)*sumAinvipts*swi*swj
      dcosmoAA(3,jn,jpts) = dcosmoAA(3,jn,jpts) - f1(3)*sumAinvipts*swi*swj
      dcosmoAA(1,in,ipts) = dcosmoAA(1,in,ipts) + f1(1)*sumAinvjpts*swi*swj
      dcosmoAA(2,in,ipts) = dcosmoAA(2,in,ipts) + f1(2)*sumAinvjpts*swi*swj
      dcosmoAA(3,in,ipts) = dcosmoAA(3,in,ipts) + f1(3)*sumAinvjpts*swi*swj
    endif
    if (lsegsmooth) then
!
!  Derivatives due to segment smoothing
!
      if (ipts.eq.jpts) then
        smtrm = 0.5_dp*qsij
        smtrmp = 0.5_dp*qsipj
        f0trm = 0.5_dp*f0
      else
        smtrm = qsij
        smtrmp = qsipj
        f0trm = f0
      endif
      smtrmi = smtrm*swi
      smtrmj = smtrm*swj
      smtrmpi = smtrmp*swi*d2qfct
      smtrmpj = smtrmp*swj*d2qfct
      do kk = 1,nnearseg(ipts)
        k = nnearsegptr(kk,ipts)
        kn = nearsasrptr(k)
!
        if (lcosmicd2) then
          dcosmoAA(1,in,ipts) = dcosmoAA(1,in,ipts) + f0trm*sumAinvjpts*swj*dsegweight(1,kk,ipts)
          dcosmoAA(2,in,ipts) = dcosmoAA(2,in,ipts) + f0trm*sumAinvjpts*swj*dsegweight(2,kk,ipts)
          dcosmoAA(3,in,ipts) = dcosmoAA(3,in,ipts) + f0trm*sumAinvjpts*swj*dsegweight(3,kk,ipts)
          dcosmoAA(1,kn,ipts) = dcosmoAA(1,kn,ipts) - f0trm*sumAinvjpts*swj*dsegweight(1,kk,ipts)
          dcosmoAA(2,kn,ipts) = dcosmoAA(2,kn,ipts) - f0trm*sumAinvjpts*swj*dsegweight(2,kk,ipts)
          dcosmoAA(3,kn,ipts) = dcosmoAA(3,kn,ipts) - f0trm*sumAinvjpts*swj*dsegweight(3,kk,ipts)
          dcosmoAA(1,in,jpts) = dcosmoAA(1,in,jpts) + f0trm*sumAinvipts*swj*dsegweight(1,kk,ipts)
          dcosmoAA(2,in,jpts) = dcosmoAA(2,in,jpts) + f0trm*sumAinvipts*swj*dsegweight(2,kk,ipts)
          dcosmoAA(3,in,jpts) = dcosmoAA(3,in,jpts) + f0trm*sumAinvipts*swj*dsegweight(3,kk,ipts)
          dcosmoAA(1,kn,jpts) = dcosmoAA(1,kn,jpts) - f0trm*sumAinvipts*swj*dsegweight(1,kk,ipts)
          dcosmoAA(2,kn,jpts) = dcosmoAA(2,kn,jpts) - f0trm*sumAinvipts*swj*dsegweight(2,kk,ipts)
          dcosmoAA(3,kn,jpts) = dcosmoAA(3,kn,jpts) - f0trm*sumAinvipts*swj*dsegweight(3,kk,ipts)
        endif
!
        kx = 3*(k - 1) + 1
        ky = kx + 1
        kz = kx + 2
        if (i.ne.j) then
          derv2(ix,jx) = derv2(ix,jx) - smtrmj*f1(1)*dsegweight(1,kk,ipts)
          derv2(iy,jx) = derv2(iy,jx) - smtrmj*f1(1)*dsegweight(2,kk,ipts)
          derv2(iz,jx) = derv2(iz,jx) - smtrmj*f1(1)*dsegweight(3,kk,ipts)
          derv2(ix,jy) = derv2(ix,jy) - smtrmj*f1(2)*dsegweight(1,kk,ipts)
          derv2(iy,jy) = derv2(iy,jy) - smtrmj*f1(2)*dsegweight(2,kk,ipts)
          derv2(iz,jy) = derv2(iz,jy) - smtrmj*f1(2)*dsegweight(3,kk,ipts)
          derv2(ix,jz) = derv2(ix,jz) - smtrmj*f1(3)*dsegweight(1,kk,ipts)
          derv2(iy,jz) = derv2(iy,jz) - smtrmj*f1(3)*dsegweight(2,kk,ipts)
          derv2(iz,jz) = derv2(iz,jz) - smtrmj*f1(3)*dsegweight(3,kk,ipts)
          if (lcosmicd2) then
            derv2(ix,jx) = derv2(ix,jx) - smtrmpj*f1(1)*dsegweight(1,kk,ipts)
            derv2(iy,jx) = derv2(iy,jx) - smtrmpj*f1(1)*dsegweight(2,kk,ipts)
            derv2(iz,jx) = derv2(iz,jx) - smtrmpj*f1(1)*dsegweight(3,kk,ipts)
            derv2(ix,jy) = derv2(ix,jy) - smtrmpj*f1(2)*dsegweight(1,kk,ipts)
            derv2(iy,jy) = derv2(iy,jy) - smtrmpj*f1(2)*dsegweight(2,kk,ipts)
            derv2(iz,jy) = derv2(iz,jy) - smtrmpj*f1(2)*dsegweight(3,kk,ipts)
            derv2(ix,jz) = derv2(ix,jz) - smtrmpj*f1(3)*dsegweight(1,kk,ipts)
            derv2(iy,jz) = derv2(iy,jz) - smtrmpj*f1(3)*dsegweight(2,kk,ipts)
            derv2(iz,jz) = derv2(iz,jz) - smtrmpj*f1(3)*dsegweight(3,kk,ipts)
          endif
        endif
        if (i.gt.k) then
          derv2(kx,ix) = derv2(kx,ix) - smtrmj*(f1(1)*dsegweight(1,kk,ipts) - f0*d2segweight(1,1,kk,kk,ipts))
          derv2(ky,ix) = derv2(ky,ix) - smtrmj*(f1(1)*dsegweight(2,kk,ipts) - f0*d2segweight(2,1,kk,kk,ipts))
          derv2(kz,ix) = derv2(kz,ix) - smtrmj*(f1(1)*dsegweight(3,kk,ipts) - f0*d2segweight(3,1,kk,kk,ipts))
          derv2(kx,iy) = derv2(kx,iy) - smtrmj*(f1(2)*dsegweight(1,kk,ipts) - f0*d2segweight(1,2,kk,kk,ipts))
          derv2(ky,iy) = derv2(ky,iy) - smtrmj*(f1(2)*dsegweight(2,kk,ipts) - f0*d2segweight(2,2,kk,kk,ipts))
          derv2(kz,iy) = derv2(kz,iy) - smtrmj*(f1(2)*dsegweight(3,kk,ipts) - f0*d2segweight(3,2,kk,kk,ipts))
          derv2(kx,iz) = derv2(kx,iz) - smtrmj*(f1(3)*dsegweight(1,kk,ipts) - f0*d2segweight(1,3,kk,kk,ipts))
          derv2(ky,iz) = derv2(ky,iz) - smtrmj*(f1(3)*dsegweight(2,kk,ipts) - f0*d2segweight(2,3,kk,kk,ipts))
          derv2(kz,iz) = derv2(kz,iz) - smtrmj*(f1(3)*dsegweight(3,kk,ipts) - f0*d2segweight(3,3,kk,kk,ipts))
          if (lcosmicd2) then
            derv2(kx,ix) = derv2(kx,ix) + smtrmpj*f0*d2segweight(1,1,kk,kk,ipts)
            derv2(ky,ix) = derv2(ky,ix) + smtrmpj*f0*d2segweight(2,1,kk,kk,ipts)
            derv2(kz,ix) = derv2(kz,ix) + smtrmpj*f0*d2segweight(3,1,kk,kk,ipts)
            derv2(kx,iy) = derv2(kx,iy) + smtrmpj*f0*d2segweight(1,2,kk,kk,ipts)
            derv2(ky,iy) = derv2(ky,iy) + smtrmpj*f0*d2segweight(2,2,kk,kk,ipts)
            derv2(kz,iy) = derv2(kz,iy) + smtrmpj*f0*d2segweight(3,2,kk,kk,ipts)
            derv2(kx,iz) = derv2(kx,iz) + smtrmpj*f0*d2segweight(1,3,kk,kk,ipts)
            derv2(ky,iz) = derv2(ky,iz) + smtrmpj*f0*d2segweight(2,3,kk,kk,ipts)
            derv2(kz,iz) = derv2(kz,iz) + smtrmpj*f0*d2segweight(3,3,kk,kk,ipts)
!
            derv2(kx,ix) = derv2(kx,ix) - smtrmpj*f1(1)*dsegweight(1,kk,ipts)
            derv2(ky,ix) = derv2(ky,ix) - smtrmpj*f1(1)*dsegweight(2,kk,ipts)
            derv2(kz,ix) = derv2(kz,ix) - smtrmpj*f1(1)*dsegweight(3,kk,ipts)
            derv2(kx,iy) = derv2(kx,iy) - smtrmpj*f1(2)*dsegweight(1,kk,ipts)
            derv2(ky,iy) = derv2(ky,iy) - smtrmpj*f1(2)*dsegweight(2,kk,ipts)
            derv2(kz,iy) = derv2(kz,iy) - smtrmpj*f1(2)*dsegweight(3,kk,ipts)
            derv2(kx,iz) = derv2(kx,iz) - smtrmpj*f1(3)*dsegweight(1,kk,ipts)
            derv2(ky,iz) = derv2(ky,iz) - smtrmpj*f1(3)*dsegweight(2,kk,ipts)
            derv2(kz,iz) = derv2(kz,iz) - smtrmpj*f1(3)*dsegweight(3,kk,ipts)
          endif
        elseif (i.lt.k) then
          derv2(ix,kx) = derv2(ix,kx) - smtrmj*(f1(1)*dsegweight(1,kk,ipts) - f0*d2segweight(1,1,kk,kk,ipts))
          derv2(iy,kx) = derv2(iy,kx) - smtrmj*(f1(2)*dsegweight(1,kk,ipts) - f0*d2segweight(1,2,kk,kk,ipts))
          derv2(iz,kx) = derv2(iz,kx) - smtrmj*(f1(3)*dsegweight(1,kk,ipts) - f0*d2segweight(1,3,kk,kk,ipts))
          derv2(ix,ky) = derv2(ix,ky) - smtrmj*(f1(1)*dsegweight(2,kk,ipts) - f0*d2segweight(2,1,kk,kk,ipts))
          derv2(iy,ky) = derv2(iy,ky) - smtrmj*(f1(2)*dsegweight(2,kk,ipts) - f0*d2segweight(2,2,kk,kk,ipts))
          derv2(iz,ky) = derv2(iz,ky) - smtrmj*(f1(3)*dsegweight(2,kk,ipts) - f0*d2segweight(2,3,kk,kk,ipts))
          derv2(ix,kz) = derv2(ix,kz) - smtrmj*(f1(1)*dsegweight(3,kk,ipts) - f0*d2segweight(3,1,kk,kk,ipts))
          derv2(iy,kz) = derv2(iy,kz) - smtrmj*(f1(2)*dsegweight(3,kk,ipts) - f0*d2segweight(3,2,kk,kk,ipts))
          derv2(iz,kz) = derv2(iz,kz) - smtrmj*(f1(3)*dsegweight(3,kk,ipts) - f0*d2segweight(3,3,kk,kk,ipts))
          if (lcosmicd2) then
            derv2(ix,kx) = derv2(ix,kx) + smtrmpj*f0*d2segweight(1,1,kk,kk,ipts)
            derv2(iy,kx) = derv2(iy,kx) + smtrmpj*f0*d2segweight(1,2,kk,kk,ipts)
            derv2(iz,kx) = derv2(iz,kx) + smtrmpj*f0*d2segweight(1,3,kk,kk,ipts)
            derv2(ix,ky) = derv2(ix,ky) + smtrmpj*f0*d2segweight(2,1,kk,kk,ipts)
            derv2(iy,ky) = derv2(iy,ky) + smtrmpj*f0*d2segweight(2,2,kk,kk,ipts)
            derv2(iz,ky) = derv2(iz,ky) + smtrmpj*f0*d2segweight(2,3,kk,kk,ipts)
            derv2(ix,kz) = derv2(ix,kz) + smtrmpj*f0*d2segweight(3,1,kk,kk,ipts)
            derv2(iy,kz) = derv2(iy,kz) + smtrmpj*f0*d2segweight(3,2,kk,kk,ipts)
            derv2(iz,kz) = derv2(iz,kz) + smtrmpj*f0*d2segweight(3,3,kk,kk,ipts)
!
            derv2(ix,kx) = derv2(ix,kx) - smtrmpj*f1(1)*dsegweight(1,kk,ipts)
            derv2(iy,kx) = derv2(iy,kx) - smtrmpj*f1(2)*dsegweight(1,kk,ipts)
            derv2(iz,kx) = derv2(iz,kx) - smtrmpj*f1(3)*dsegweight(1,kk,ipts)
            derv2(ix,ky) = derv2(ix,ky) - smtrmpj*f1(1)*dsegweight(2,kk,ipts)
            derv2(iy,ky) = derv2(iy,ky) - smtrmpj*f1(2)*dsegweight(2,kk,ipts)
            derv2(iz,ky) = derv2(iz,ky) - smtrmpj*f1(3)*dsegweight(2,kk,ipts)
            derv2(ix,kz) = derv2(ix,kz) - smtrmpj*f1(1)*dsegweight(3,kk,ipts)
            derv2(iy,kz) = derv2(iy,kz) - smtrmpj*f1(2)*dsegweight(3,kk,ipts)
            derv2(iz,kz) = derv2(iz,kz) - smtrmpj*f1(3)*dsegweight(3,kk,ipts)
          endif
        endif
        if (j.gt.k) then
          derv2(kx,jx) = derv2(kx,jx) + smtrmj*f1(1)*dsegweight(1,kk,ipts)
          derv2(ky,jx) = derv2(ky,jx) + smtrmj*f1(1)*dsegweight(2,kk,ipts)
          derv2(kz,jx) = derv2(kz,jx) + smtrmj*f1(1)*dsegweight(3,kk,ipts)
          derv2(kx,jy) = derv2(kx,jy) + smtrmj*f1(2)*dsegweight(1,kk,ipts)
          derv2(ky,jy) = derv2(ky,jy) + smtrmj*f1(2)*dsegweight(2,kk,ipts)
          derv2(kz,jy) = derv2(kz,jy) + smtrmj*f1(2)*dsegweight(3,kk,ipts)
          derv2(kx,jz) = derv2(kx,jz) + smtrmj*f1(3)*dsegweight(1,kk,ipts)
          derv2(ky,jz) = derv2(ky,jz) + smtrmj*f1(3)*dsegweight(2,kk,ipts)
          derv2(kz,jz) = derv2(kz,jz) + smtrmj*f1(3)*dsegweight(3,kk,ipts)
          if (lcosmicd2) then
            derv2(kx,jx) = derv2(kx,jx) + smtrmpj*f1(1)*dsegweight(1,kk,ipts)
            derv2(ky,jx) = derv2(ky,jx) + smtrmpj*f1(1)*dsegweight(2,kk,ipts)
            derv2(kz,jx) = derv2(kz,jx) + smtrmpj*f1(1)*dsegweight(3,kk,ipts)
            derv2(kx,jy) = derv2(kx,jy) + smtrmpj*f1(2)*dsegweight(1,kk,ipts)
            derv2(ky,jy) = derv2(ky,jy) + smtrmpj*f1(2)*dsegweight(2,kk,ipts)
            derv2(kz,jy) = derv2(kz,jy) + smtrmpj*f1(2)*dsegweight(3,kk,ipts)
            derv2(kx,jz) = derv2(kx,jz) + smtrmpj*f1(3)*dsegweight(1,kk,ipts)
            derv2(ky,jz) = derv2(ky,jz) + smtrmpj*f1(3)*dsegweight(2,kk,ipts)
            derv2(kz,jz) = derv2(kz,jz) + smtrmpj*f1(3)*dsegweight(3,kk,ipts)
          endif
        elseif (j.lt.k) then
          derv2(jx,kx) = derv2(jx,kx) + smtrmj*f1(1)*dsegweight(1,kk,ipts)
          derv2(jy,kx) = derv2(jy,kx) + smtrmj*f1(2)*dsegweight(1,kk,ipts)
          derv2(jz,kx) = derv2(jz,kx) + smtrmj*f1(3)*dsegweight(1,kk,ipts)
          derv2(jx,ky) = derv2(jx,ky) + smtrmj*f1(1)*dsegweight(2,kk,ipts)
          derv2(jy,ky) = derv2(jy,ky) + smtrmj*f1(2)*dsegweight(2,kk,ipts)
          derv2(jz,ky) = derv2(jz,ky) + smtrmj*f1(3)*dsegweight(2,kk,ipts)
          derv2(jx,kz) = derv2(jx,kz) + smtrmj*f1(1)*dsegweight(3,kk,ipts)
          derv2(jy,kz) = derv2(jy,kz) + smtrmj*f1(2)*dsegweight(3,kk,ipts)
          derv2(jz,kz) = derv2(jz,kz) + smtrmj*f1(3)*dsegweight(3,kk,ipts)
          if (lcosmicd2) then
            derv2(jx,kx) = derv2(jx,kx) + smtrmpj*f1(1)*dsegweight(1,kk,ipts)
            derv2(jy,kx) = derv2(jy,kx) + smtrmpj*f1(2)*dsegweight(1,kk,ipts)
            derv2(jz,kx) = derv2(jz,kx) + smtrmpj*f1(3)*dsegweight(1,kk,ipts)
            derv2(jx,ky) = derv2(jx,ky) + smtrmpj*f1(1)*dsegweight(2,kk,ipts)
            derv2(jy,ky) = derv2(jy,ky) + smtrmpj*f1(2)*dsegweight(2,kk,ipts)
            derv2(jz,ky) = derv2(jz,ky) + smtrmpj*f1(3)*dsegweight(2,kk,ipts)
            derv2(jx,kz) = derv2(jx,kz) + smtrmpj*f1(1)*dsegweight(3,kk,ipts)
            derv2(jy,kz) = derv2(jy,kz) + smtrmpj*f1(2)*dsegweight(3,kk,ipts)
            derv2(jz,kz) = derv2(jz,kz) + smtrmpj*f1(3)*dsegweight(3,kk,ipts)
          endif
        endif
      enddo
      do ll = 1,nnearseg(jpts)
        l = nnearsegptr(ll,jpts)
        ln = nearsasrptr(l)
!
        if (lcosmicd2) then
          dcosmoAA(1,jn,jpts) = dcosmoAA(1,jn,jpts) + f0trm*sumAinvipts*swi*dsegweight(1,ll,jpts)
          dcosmoAA(2,jn,jpts) = dcosmoAA(2,jn,jpts) + f0trm*sumAinvipts*swi*dsegweight(2,ll,jpts)
          dcosmoAA(3,jn,jpts) = dcosmoAA(3,jn,jpts) + f0trm*sumAinvipts*swi*dsegweight(3,ll,jpts)
          dcosmoAA(1,ln,jpts) = dcosmoAA(1,ln,jpts) - f0trm*sumAinvipts*swi*dsegweight(1,ll,jpts)
          dcosmoAA(2,ln,jpts) = dcosmoAA(2,ln,jpts) - f0trm*sumAinvipts*swi*dsegweight(2,ll,jpts)
          dcosmoAA(3,ln,jpts) = dcosmoAA(3,ln,jpts) - f0trm*sumAinvipts*swi*dsegweight(3,ll,jpts)
          dcosmoAA(1,jn,ipts) = dcosmoAA(1,jn,ipts) + f0trm*sumAinvjpts*swi*dsegweight(1,ll,jpts)
          dcosmoAA(2,jn,ipts) = dcosmoAA(2,jn,ipts) + f0trm*sumAinvjpts*swi*dsegweight(2,ll,jpts)
          dcosmoAA(3,jn,ipts) = dcosmoAA(3,jn,ipts) + f0trm*sumAinvjpts*swi*dsegweight(3,ll,jpts)
          dcosmoAA(1,ln,ipts) = dcosmoAA(1,ln,ipts) - f0trm*sumAinvjpts*swi*dsegweight(1,ll,jpts)
          dcosmoAA(2,ln,ipts) = dcosmoAA(2,ln,ipts) - f0trm*sumAinvjpts*swi*dsegweight(2,ll,jpts)
          dcosmoAA(3,ln,ipts) = dcosmoAA(3,ln,ipts) - f0trm*sumAinvjpts*swi*dsegweight(3,ll,jpts)
        endif
!
        lx = 3*(l - 1) + 1
        ly = lx + 1
        lz = lx + 2
        if (i.ne.j) then
          derv2(ix,jx) = derv2(ix,jx) + smtrmi*f1(1)*dsegweight(1,ll,jpts)
          derv2(iy,jx) = derv2(iy,jx) + smtrmi*f1(2)*dsegweight(1,ll,jpts)
          derv2(iz,jx) = derv2(iz,jx) + smtrmi*f1(3)*dsegweight(1,ll,jpts)
          derv2(ix,jy) = derv2(ix,jy) + smtrmi*f1(1)*dsegweight(2,ll,jpts)
          derv2(iy,jy) = derv2(iy,jy) + smtrmi*f1(2)*dsegweight(2,ll,jpts)
          derv2(iz,jy) = derv2(iz,jy) + smtrmi*f1(3)*dsegweight(2,ll,jpts)
          derv2(ix,jz) = derv2(ix,jz) + smtrmi*f1(1)*dsegweight(3,ll,jpts)
          derv2(iy,jz) = derv2(iy,jz) + smtrmi*f1(2)*dsegweight(3,ll,jpts)
          derv2(iz,jz) = derv2(iz,jz) + smtrmi*f1(3)*dsegweight(3,ll,jpts)
          if (lcosmicd2) then
            derv2(ix,jx) = derv2(ix,jx) + smtrmpi*f1(1)*dsegweight(1,ll,jpts)
            derv2(iy,jx) = derv2(iy,jx) + smtrmpi*f1(2)*dsegweight(1,ll,jpts)
            derv2(iz,jx) = derv2(iz,jx) + smtrmpi*f1(3)*dsegweight(1,ll,jpts)
            derv2(ix,jy) = derv2(ix,jy) + smtrmpi*f1(1)*dsegweight(2,ll,jpts)
            derv2(iy,jy) = derv2(iy,jy) + smtrmpi*f1(2)*dsegweight(2,ll,jpts)
            derv2(iz,jy) = derv2(iz,jy) + smtrmpi*f1(3)*dsegweight(2,ll,jpts)
            derv2(ix,jz) = derv2(ix,jz) + smtrmpi*f1(1)*dsegweight(3,ll,jpts)
            derv2(iy,jz) = derv2(iy,jz) + smtrmpi*f1(2)*dsegweight(3,ll,jpts)
            derv2(iz,jz) = derv2(iz,jz) + smtrmpi*f1(3)*dsegweight(3,ll,jpts)
          endif
        endif
        if (i.gt.l) then
          derv2(lx,ix) = derv2(lx,ix) - smtrmi*f1(1)*dsegweight(1,ll,jpts)
          derv2(ly,ix) = derv2(ly,ix) - smtrmi*f1(1)*dsegweight(2,ll,jpts)
          derv2(lz,ix) = derv2(lz,ix) - smtrmi*f1(1)*dsegweight(3,ll,jpts)
          derv2(lx,iy) = derv2(lx,iy) - smtrmi*f1(2)*dsegweight(1,ll,jpts)
          derv2(ly,iy) = derv2(ly,iy) - smtrmi*f1(2)*dsegweight(2,ll,jpts)
          derv2(lz,iy) = derv2(lz,iy) - smtrmi*f1(2)*dsegweight(3,ll,jpts)
          derv2(lx,iz) = derv2(lx,iz) - smtrmi*f1(3)*dsegweight(1,ll,jpts)
          derv2(ly,iz) = derv2(ly,iz) - smtrmi*f1(3)*dsegweight(2,ll,jpts)
          derv2(lz,iz) = derv2(lz,iz) - smtrmi*f1(3)*dsegweight(3,ll,jpts)
          if (lcosmicd2) then
            derv2(lx,ix) = derv2(lx,ix) - smtrmpi*f1(1)*dsegweight(1,ll,jpts)
            derv2(ly,ix) = derv2(ly,ix) - smtrmpi*f1(1)*dsegweight(2,ll,jpts)
            derv2(lz,ix) = derv2(lz,ix) - smtrmpi*f1(1)*dsegweight(3,ll,jpts)
            derv2(lx,iy) = derv2(lx,iy) - smtrmpi*f1(2)*dsegweight(1,ll,jpts)
            derv2(ly,iy) = derv2(ly,iy) - smtrmpi*f1(2)*dsegweight(2,ll,jpts)
            derv2(lz,iy) = derv2(lz,iy) - smtrmpi*f1(2)*dsegweight(3,ll,jpts)
            derv2(lx,iz) = derv2(lx,iz) - smtrmpi*f1(3)*dsegweight(1,ll,jpts)
            derv2(ly,iz) = derv2(ly,iz) - smtrmpi*f1(3)*dsegweight(2,ll,jpts)
            derv2(lz,iz) = derv2(lz,iz) - smtrmpi*f1(3)*dsegweight(3,ll,jpts)
          endif
        elseif (i.lt.l) then
          derv2(ix,lx) = derv2(ix,lx) - smtrmi*f1(1)*dsegweight(1,ll,jpts)
          derv2(iy,lx) = derv2(iy,lx) - smtrmi*f1(2)*dsegweight(1,ll,jpts)
          derv2(iz,lx) = derv2(iz,lx) - smtrmi*f1(3)*dsegweight(1,ll,jpts)
          derv2(ix,ly) = derv2(ix,ly) - smtrmi*f1(1)*dsegweight(2,ll,jpts)
          derv2(iy,ly) = derv2(iy,ly) - smtrmi*f1(2)*dsegweight(2,ll,jpts)
          derv2(iz,ly) = derv2(iz,ly) - smtrmi*f1(3)*dsegweight(2,ll,jpts)
          derv2(ix,lz) = derv2(ix,lz) - smtrmi*f1(1)*dsegweight(3,ll,jpts)
          derv2(iy,lz) = derv2(iy,lz) - smtrmi*f1(2)*dsegweight(3,ll,jpts)
          derv2(iz,lz) = derv2(iz,lz) - smtrmi*f1(3)*dsegweight(3,ll,jpts)
          if (lcosmicd2) then
            derv2(ix,lx) = derv2(ix,lx) - smtrmpi*f1(1)*dsegweight(1,ll,jpts)
            derv2(iy,lx) = derv2(iy,lx) - smtrmpi*f1(2)*dsegweight(1,ll,jpts)
            derv2(iz,lx) = derv2(iz,lx) - smtrmpi*f1(3)*dsegweight(1,ll,jpts)
            derv2(ix,ly) = derv2(ix,ly) - smtrmpi*f1(1)*dsegweight(2,ll,jpts)
            derv2(iy,ly) = derv2(iy,ly) - smtrmpi*f1(2)*dsegweight(2,ll,jpts)
            derv2(iz,ly) = derv2(iz,ly) - smtrmpi*f1(3)*dsegweight(2,ll,jpts)
            derv2(ix,lz) = derv2(ix,lz) - smtrmpi*f1(1)*dsegweight(3,ll,jpts)
            derv2(iy,lz) = derv2(iy,lz) - smtrmpi*f1(2)*dsegweight(3,ll,jpts)
            derv2(iz,lz) = derv2(iz,lz) - smtrmpi*f1(3)*dsegweight(3,ll,jpts)
          endif
        endif
        if (j.gt.l) then
          derv2(lx,jx) = derv2(lx,jx) + smtrmi*(f1(1)*dsegweight(1,ll,jpts) + f0*d2segweight(1,1,ll,ll,jpts))
          derv2(ly,jx) = derv2(ly,jx) + smtrmi*(f1(1)*dsegweight(2,ll,jpts) + f0*d2segweight(2,1,ll,ll,jpts))
          derv2(lz,jx) = derv2(lz,jx) + smtrmi*(f1(1)*dsegweight(3,ll,jpts) + f0*d2segweight(3,1,ll,ll,jpts))
          derv2(lx,jy) = derv2(lx,jy) + smtrmi*(f1(2)*dsegweight(1,ll,jpts) + f0*d2segweight(1,2,ll,ll,jpts))
          derv2(ly,jy) = derv2(ly,jy) + smtrmi*(f1(2)*dsegweight(2,ll,jpts) + f0*d2segweight(2,2,ll,ll,jpts))
          derv2(lz,jy) = derv2(lz,jy) + smtrmi*(f1(2)*dsegweight(3,ll,jpts) + f0*d2segweight(3,2,ll,ll,jpts))
          derv2(lx,jz) = derv2(lx,jz) + smtrmi*(f1(3)*dsegweight(1,ll,jpts) + f0*d2segweight(1,3,ll,ll,jpts))
          derv2(ly,jz) = derv2(ly,jz) + smtrmi*(f1(3)*dsegweight(2,ll,jpts) + f0*d2segweight(2,3,ll,ll,jpts))
          derv2(lz,jz) = derv2(lz,jz) + smtrmi*(f1(3)*dsegweight(3,ll,jpts) + f0*d2segweight(3,3,ll,ll,jpts))
          if (lcosmicd2) then
            derv2(lx,jx) = derv2(lx,jx) + smtrmpi*f0*d2segweight(1,1,ll,ll,jpts)
            derv2(ly,jx) = derv2(ly,jx) + smtrmpi*f0*d2segweight(2,1,ll,ll,jpts)
            derv2(lz,jx) = derv2(lz,jx) + smtrmpi*f0*d2segweight(3,1,ll,ll,jpts)
            derv2(lx,jy) = derv2(lx,jy) + smtrmpi*f0*d2segweight(1,2,ll,ll,jpts)
            derv2(ly,jy) = derv2(ly,jy) + smtrmpi*f0*d2segweight(2,2,ll,ll,jpts)
            derv2(lz,jy) = derv2(lz,jy) + smtrmpi*f0*d2segweight(3,2,ll,ll,jpts)
            derv2(lx,jz) = derv2(lx,jz) + smtrmpi*f0*d2segweight(1,3,ll,ll,jpts)
            derv2(ly,jz) = derv2(ly,jz) + smtrmpi*f0*d2segweight(2,3,ll,ll,jpts)
            derv2(lz,jz) = derv2(lz,jz) + smtrmpi*f0*d2segweight(3,3,ll,ll,jpts)
!
            derv2(lx,jx) = derv2(lx,jx) + smtrmpi*f1(1)*dsegweight(1,ll,jpts)
            derv2(ly,jx) = derv2(ly,jx) + smtrmpi*f1(1)*dsegweight(2,ll,jpts)
            derv2(lz,jx) = derv2(lz,jx) + smtrmpi*f1(1)*dsegweight(3,ll,jpts)
            derv2(lx,jy) = derv2(lx,jy) + smtrmpi*f1(2)*dsegweight(1,ll,jpts)
            derv2(ly,jy) = derv2(ly,jy) + smtrmpi*f1(2)*dsegweight(2,ll,jpts)
            derv2(lz,jy) = derv2(lz,jy) + smtrmpi*f1(2)*dsegweight(3,ll,jpts)
            derv2(lx,jz) = derv2(lx,jz) + smtrmpi*f1(3)*dsegweight(1,ll,jpts)
            derv2(ly,jz) = derv2(ly,jz) + smtrmpi*f1(3)*dsegweight(2,ll,jpts)
            derv2(lz,jz) = derv2(lz,jz) + smtrmpi*f1(3)*dsegweight(3,ll,jpts)
          endif
        elseif (j.lt.l) then
          derv2(jx,lx) = derv2(jx,lx) + smtrmi*(f1(1)*dsegweight(1,ll,jpts) + f0*d2segweight(1,1,ll,ll,jpts))
          derv2(jy,lx) = derv2(jy,lx) + smtrmi*(f1(2)*dsegweight(1,ll,jpts) + f0*d2segweight(1,2,ll,ll,jpts))
          derv2(jz,lx) = derv2(jz,lx) + smtrmi*(f1(3)*dsegweight(1,ll,jpts) + f0*d2segweight(1,3,ll,ll,jpts))
          derv2(jx,ly) = derv2(jx,ly) + smtrmi*(f1(1)*dsegweight(2,ll,jpts) + f0*d2segweight(2,1,ll,ll,jpts))
          derv2(jy,ly) = derv2(jy,ly) + smtrmi*(f1(2)*dsegweight(2,ll,jpts) + f0*d2segweight(2,2,ll,ll,jpts))
          derv2(jz,ly) = derv2(jz,ly) + smtrmi*(f1(3)*dsegweight(2,ll,jpts) + f0*d2segweight(2,3,ll,ll,jpts))
          derv2(jx,lz) = derv2(jx,lz) + smtrmi*(f1(1)*dsegweight(3,ll,jpts) + f0*d2segweight(3,1,ll,ll,jpts))
          derv2(jy,lz) = derv2(jy,lz) + smtrmi*(f1(2)*dsegweight(3,ll,jpts) + f0*d2segweight(3,2,ll,ll,jpts))
          derv2(jz,lz) = derv2(jz,lz) + smtrmi*(f1(3)*dsegweight(3,ll,jpts) + f0*d2segweight(3,3,ll,ll,jpts))
          if (lcosmicd2) then
            derv2(jx,lx) = derv2(jx,lx) + smtrmpi*f0*d2segweight(1,1,ll,ll,jpts)
            derv2(jy,lx) = derv2(jy,lx) + smtrmpi*f0*d2segweight(1,2,ll,ll,jpts)
            derv2(jz,lx) = derv2(jz,lx) + smtrmpi*f0*d2segweight(1,3,ll,ll,jpts)
            derv2(jx,ly) = derv2(jx,ly) + smtrmpi*f0*d2segweight(2,1,ll,ll,jpts)
            derv2(jy,ly) = derv2(jy,ly) + smtrmpi*f0*d2segweight(2,2,ll,ll,jpts)
            derv2(jz,ly) = derv2(jz,ly) + smtrmpi*f0*d2segweight(2,3,ll,ll,jpts)
            derv2(jx,lz) = derv2(jx,lz) + smtrmpi*f0*d2segweight(3,1,ll,ll,jpts)
            derv2(jy,lz) = derv2(jy,lz) + smtrmpi*f0*d2segweight(3,2,ll,ll,jpts)
            derv2(jz,lz) = derv2(jz,lz) + smtrmpi*f0*d2segweight(3,3,ll,ll,jpts)
!
            derv2(jx,lx) = derv2(jx,lx) + smtrmpi*f1(1)*dsegweight(1,ll,jpts)
            derv2(jy,lx) = derv2(jy,lx) + smtrmpi*f1(2)*dsegweight(1,ll,jpts)
            derv2(jz,lx) = derv2(jz,lx) + smtrmpi*f1(3)*dsegweight(1,ll,jpts)
            derv2(jx,ly) = derv2(jx,ly) + smtrmpi*f1(1)*dsegweight(2,ll,jpts)
            derv2(jy,ly) = derv2(jy,ly) + smtrmpi*f1(2)*dsegweight(2,ll,jpts)
            derv2(jz,ly) = derv2(jz,ly) + smtrmpi*f1(3)*dsegweight(2,ll,jpts)
            derv2(jx,lz) = derv2(jx,lz) + smtrmpi*f1(1)*dsegweight(3,ll,jpts)
            derv2(jy,lz) = derv2(jy,lz) + smtrmpi*f1(2)*dsegweight(3,ll,jpts)
            derv2(jz,lz) = derv2(jz,lz) + smtrmpi*f1(3)*dsegweight(3,ll,jpts)
          endif
        endif
      enddo
!
!  Combined loops over weighting atoms - one for i and one for j
!
      if (ipts.eq.jpts) then
        smtrm = 0.5_dp*qsij*f0
        smtrmp = 0.5_dp*qsipj*f0*d2qfct
      else
        smtrm = qsij*f0
        smtrmp = qsipj*f0*d2qfct
      endif
      if (lcosmicd2) then
        trm = smtrm + smtrmp
      else
        trm = smtrm 
      endif
      do kk = 1,nnearseg(ipts)
        k = nnearsegptr(kk,ipts)
        kx = 3*(k - 1) + 1
        ky = kx + 1
        kz = kx + 2
        do ll = 1,nnearseg(jpts)
          l = nnearsegptr(ll,jpts)
          lx = 3*(l - 1) + 1
          ly = lx + 1
          lz = lx + 2
          if (i.ne.j) then
            derv2(ix,jx) = derv2(ix,jx) + trm*dsegweight(1,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(iy,jx) = derv2(iy,jx) + trm*dsegweight(1,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(iz,jx) = derv2(iz,jx) + trm*dsegweight(1,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(ix,jy) = derv2(ix,jy) + trm*dsegweight(2,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(iy,jy) = derv2(iy,jy) + trm*dsegweight(2,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(iz,jy) = derv2(iz,jy) + trm*dsegweight(2,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(ix,jz) = derv2(ix,jz) + trm*dsegweight(3,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(iy,jz) = derv2(iy,jz) + trm*dsegweight(3,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(iz,jz) = derv2(iz,jz) + trm*dsegweight(3,ll,jpts)*dsegweight(3,kk,ipts)
          endif
          if (i.gt.l) then
            derv2(lx,ix) = derv2(lx,ix) - trm*dsegweight(1,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ly,ix) = derv2(ly,ix) - trm*dsegweight(2,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(lz,ix) = derv2(lz,ix) - trm*dsegweight(3,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(lx,iy) = derv2(lx,iy) - trm*dsegweight(1,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(ly,iy) = derv2(ly,iy) - trm*dsegweight(2,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(lz,iy) = derv2(lz,iy) - trm*dsegweight(3,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(lx,iz) = derv2(lx,iz) - trm*dsegweight(1,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(ly,iz) = derv2(ly,iz) - trm*dsegweight(2,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(lz,iz) = derv2(lz,iz) - trm*dsegweight(3,ll,jpts)*dsegweight(3,kk,ipts)
          elseif (i.lt.l) then
            derv2(ix,lx) = derv2(ix,lx) - trm*dsegweight(1,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(iy,lx) = derv2(iy,lx) - trm*dsegweight(1,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(iz,lx) = derv2(iz,lx) - trm*dsegweight(1,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(ix,ly) = derv2(ix,ly) - trm*dsegweight(2,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(iy,ly) = derv2(iy,ly) - trm*dsegweight(2,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(iz,ly) = derv2(iz,ly) - trm*dsegweight(2,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(ix,lz) = derv2(ix,lz) - trm*dsegweight(3,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(iy,lz) = derv2(iy,lz) - trm*dsegweight(3,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(iz,lz) = derv2(iz,lz) - trm*dsegweight(3,ll,jpts)*dsegweight(3,kk,ipts)
          endif
          if (j.gt.k) then
            derv2(kx,jx) = derv2(kx,jx) - trm*dsegweight(1,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ky,jx) = derv2(ky,jx) - trm*dsegweight(1,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(kz,jx) = derv2(kz,jx) - trm*dsegweight(1,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(kx,jy) = derv2(kx,jy) - trm*dsegweight(2,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ky,jy) = derv2(ky,jy) - trm*dsegweight(2,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(kz,jy) = derv2(kz,jy) - trm*dsegweight(2,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(kx,jz) = derv2(kx,jz) - trm*dsegweight(3,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ky,jz) = derv2(ky,jz) - trm*dsegweight(3,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(kz,jz) = derv2(kz,jz) - trm*dsegweight(3,ll,jpts)*dsegweight(3,kk,ipts)
          elseif (j.lt.k) then
            derv2(jx,kx) = derv2(jx,kx) - trm*dsegweight(1,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(jy,kx) = derv2(jy,kx) - trm*dsegweight(2,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(jz,kx) = derv2(jz,kx) - trm*dsegweight(3,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(jx,ky) = derv2(jx,ky) - trm*dsegweight(1,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(jy,ky) = derv2(jy,ky) - trm*dsegweight(2,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(jz,ky) = derv2(jz,ky) - trm*dsegweight(3,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(jx,kz) = derv2(jx,kz) - trm*dsegweight(1,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(jy,kz) = derv2(jy,kz) - trm*dsegweight(2,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(jz,kz) = derv2(jz,kz) - trm*dsegweight(3,ll,jpts)*dsegweight(3,kk,ipts)
          endif
          if (l.gt.k) then
            derv2(kx,lx) = derv2(kx,lx) + trm*dsegweight(1,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ky,lx) = derv2(ky,lx) + trm*dsegweight(1,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(kz,lx) = derv2(kz,lx) + trm*dsegweight(1,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(kx,ly) = derv2(kx,ly) + trm*dsegweight(2,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ky,ly) = derv2(ky,ly) + trm*dsegweight(2,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(kz,ly) = derv2(kz,ly) + trm*dsegweight(2,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(kx,lz) = derv2(kx,lz) + trm*dsegweight(3,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ky,lz) = derv2(ky,lz) + trm*dsegweight(3,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(kz,lz) = derv2(kz,lz) + trm*dsegweight(3,ll,jpts)*dsegweight(3,kk,ipts)
          elseif (l.lt.k) then
            derv2(lx,kx) = derv2(lx,kx) + trm*dsegweight(1,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(ly,kx) = derv2(ly,kx) + trm*dsegweight(2,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(lz,kx) = derv2(lz,kx) + trm*dsegweight(3,ll,jpts)*dsegweight(1,kk,ipts)
            derv2(lx,ky) = derv2(lx,ky) + trm*dsegweight(1,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(ly,ky) = derv2(ly,ky) + trm*dsegweight(2,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(lz,ky) = derv2(lz,ky) + trm*dsegweight(3,ll,jpts)*dsegweight(2,kk,ipts)
            derv2(lx,kz) = derv2(lx,kz) + trm*dsegweight(1,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(ly,kz) = derv2(ly,kz) + trm*dsegweight(2,ll,jpts)*dsegweight(3,kk,ipts)
            derv2(lz,kz) = derv2(lz,kz) + trm*dsegweight(3,ll,jpts)*dsegweight(3,kk,ipts)
          endif
        enddo
      enddo
!
!  Combined loops over weighting atoms - both on the same centre
!
      smtrmi = smtrm*swi
      smtrmj = smtrm*swj
      smtrmpi = smtrmp*swi
      smtrmpj = smtrmp*swj
      if (lcosmicd2) then
        trmi = smtrmi + smtrmpi
        trmj = smtrmj + smtrmpj
      else
        trmi = smtrmi
        trmj = smtrmj
      endif
      do kk = 2,nnearseg(ipts)
        k = nnearsegptr(kk,ipts)
        kx = 3*(k - 1) + 1
        ky = kx + 1
        kz = kx + 2
        do ll = 1,kk-1
          l = nnearsegptr(ll,ipts)
          lx = 3*(l - 1) + 1
          ly = lx + 1
          lz = lx + 2
          if (i.gt.k) then
            derv2(kx,ix) = derv2(kx,ix) - trmj*d2segweight(1,1,ll,kk,ipts)
            derv2(ky,ix) = derv2(ky,ix) - trmj*d2segweight(1,2,ll,kk,ipts)
            derv2(kz,ix) = derv2(kz,ix) - trmj*d2segweight(1,3,ll,kk,ipts)
            derv2(kx,iy) = derv2(kx,iy) - trmj*d2segweight(2,1,ll,kk,ipts)
            derv2(ky,iy) = derv2(ky,iy) - trmj*d2segweight(2,2,ll,kk,ipts)
            derv2(kz,iy) = derv2(kz,iy) - trmj*d2segweight(2,3,ll,kk,ipts)
            derv2(kx,iz) = derv2(kx,iz) - trmj*d2segweight(3,1,ll,kk,ipts)
            derv2(ky,iz) = derv2(ky,iz) - trmj*d2segweight(3,2,ll,kk,ipts)
            derv2(kz,iz) = derv2(kz,iz) - trmj*d2segweight(3,3,ll,kk,ipts)
          elseif (i.lt.k) then
            derv2(ix,kx) = derv2(ix,kx) - trmj*d2segweight(1,1,ll,kk,ipts)
            derv2(iy,kx) = derv2(iy,kx) - trmj*d2segweight(2,1,ll,kk,ipts)
            derv2(iz,kx) = derv2(iz,kx) - trmj*d2segweight(3,1,ll,kk,ipts)
            derv2(ix,ky) = derv2(ix,ky) - trmj*d2segweight(1,2,ll,kk,ipts)
            derv2(iy,ky) = derv2(iy,ky) - trmj*d2segweight(2,2,ll,kk,ipts)
            derv2(iz,ky) = derv2(iz,ky) - trmj*d2segweight(3,2,ll,kk,ipts)
            derv2(ix,kz) = derv2(ix,kz) - trmj*d2segweight(1,3,ll,kk,ipts)
            derv2(iy,kz) = derv2(iy,kz) - trmj*d2segweight(2,3,ll,kk,ipts)
            derv2(iz,kz) = derv2(iz,kz) - trmj*d2segweight(3,3,ll,kk,ipts)
          endif
          if (i.gt.l) then
            derv2(lx,ix) = derv2(lx,ix) - trmj*d2segweight(1,1,ll,kk,ipts)
            derv2(ly,ix) = derv2(ly,ix) - trmj*d2segweight(2,1,ll,kk,ipts)
            derv2(lz,ix) = derv2(lz,ix) - trmj*d2segweight(3,1,ll,kk,ipts)
            derv2(lx,iy) = derv2(lx,iy) - trmj*d2segweight(1,2,ll,kk,ipts)
            derv2(ly,iy) = derv2(ly,iy) - trmj*d2segweight(2,2,ll,kk,ipts)
            derv2(lz,iy) = derv2(lz,iy) - trmj*d2segweight(3,2,ll,kk,ipts)
            derv2(lx,iz) = derv2(lx,iz) - trmj*d2segweight(1,3,ll,kk,ipts)
            derv2(ly,iz) = derv2(ly,iz) - trmj*d2segweight(2,3,ll,kk,ipts)
            derv2(lz,iz) = derv2(lz,iz) - trmj*d2segweight(3,3,ll,kk,ipts)
          elseif (i.lt.l) then
            derv2(ix,lx) = derv2(ix,lx) - trmj*d2segweight(1,1,ll,kk,ipts)
            derv2(iy,lx) = derv2(iy,lx) - trmj*d2segweight(1,2,ll,kk,ipts)
            derv2(iz,lx) = derv2(iz,lx) - trmj*d2segweight(1,3,ll,kk,ipts)
            derv2(ix,ly) = derv2(ix,ly) - trmj*d2segweight(2,1,ll,kk,ipts)
            derv2(iy,ly) = derv2(iy,ly) - trmj*d2segweight(2,2,ll,kk,ipts)
            derv2(iz,ly) = derv2(iz,ly) - trmj*d2segweight(2,3,ll,kk,ipts)
            derv2(ix,lz) = derv2(ix,lz) - trmj*d2segweight(3,1,ll,kk,ipts)
            derv2(iy,lz) = derv2(iy,lz) - trmj*d2segweight(3,2,ll,kk,ipts)
            derv2(iz,lz) = derv2(iz,lz) - trmj*d2segweight(3,3,ll,kk,ipts)
          endif
          if (k.gt.l) then
            derv2(lx,kx) = derv2(lx,kx) + trmj*d2segweight(1,1,ll,kk,ipts)
            derv2(ly,kx) = derv2(ly,kx) + trmj*d2segweight(2,1,ll,kk,ipts)
            derv2(lz,kx) = derv2(lz,kx) + trmj*d2segweight(3,1,ll,kk,ipts)
            derv2(lx,ky) = derv2(lx,ky) + trmj*d2segweight(1,2,ll,kk,ipts)
            derv2(ly,ky) = derv2(ly,ky) + trmj*d2segweight(2,2,ll,kk,ipts)
            derv2(lz,ky) = derv2(lz,ky) + trmj*d2segweight(3,2,ll,kk,ipts)
            derv2(lx,kz) = derv2(lx,kz) + trmj*d2segweight(1,3,ll,kk,ipts)
            derv2(ly,kz) = derv2(ly,kz) + trmj*d2segweight(2,3,ll,kk,ipts)
            derv2(lz,kz) = derv2(lz,kz) + trmj*d2segweight(3,3,ll,kk,ipts)
          elseif (k.lt.l) then
            derv2(kx,lx) = derv2(kx,lx) + trmj*d2segweight(1,1,ll,kk,ipts)
            derv2(ky,lx) = derv2(ky,lx) + trmj*d2segweight(1,2,ll,kk,ipts)
            derv2(kz,lx) = derv2(kz,lx) + trmj*d2segweight(1,3,ll,kk,ipts)
            derv2(kx,ly) = derv2(kx,ly) + trmj*d2segweight(2,1,ll,kk,ipts)
            derv2(ky,ly) = derv2(ky,ly) + trmj*d2segweight(2,2,ll,kk,ipts)
            derv2(kz,ly) = derv2(kz,ly) + trmj*d2segweight(2,3,ll,kk,ipts)
            derv2(kx,lz) = derv2(kx,lz) + trmj*d2segweight(3,1,ll,kk,ipts)
            derv2(ky,lz) = derv2(ky,lz) + trmj*d2segweight(3,2,ll,kk,ipts)
            derv2(kz,lz) = derv2(kz,lz) + trmj*d2segweight(3,3,ll,kk,ipts)
          endif
        enddo
      enddo
!
      do kk = 2,nnearseg(jpts)
        k = nnearsegptr(kk,jpts)
        kx = 3*(k - 1) + 1
        ky = kx + 1
        kz = kx + 2
        do ll = 1,kk-1
          l = nnearsegptr(ll,jpts)
          lx = 3*(l - 1) + 1
          ly = lx + 1
          lz = lx + 2
          if (j.gt.k) then
            derv2(kx,jx) = derv2(kx,jx) - trmi*d2segweight(1,1,ll,kk,jpts)
            derv2(ky,jx) = derv2(ky,jx) - trmi*d2segweight(1,2,ll,kk,jpts)
            derv2(kz,jx) = derv2(kz,jx) - trmi*d2segweight(1,3,ll,kk,jpts)
            derv2(kx,jy) = derv2(kx,jy) - trmi*d2segweight(2,1,ll,kk,jpts)
            derv2(ky,jy) = derv2(ky,jy) - trmi*d2segweight(2,2,ll,kk,jpts)
            derv2(kz,jy) = derv2(kz,jy) - trmi*d2segweight(2,3,ll,kk,jpts)
            derv2(kx,jz) = derv2(kx,jz) - trmi*d2segweight(3,1,ll,kk,jpts)
            derv2(ky,jz) = derv2(ky,jz) - trmi*d2segweight(3,2,ll,kk,jpts)
            derv2(kz,jz) = derv2(kz,jz) - trmi*d2segweight(3,3,ll,kk,jpts)
          elseif (j.lt.k) then
            derv2(jx,kx) = derv2(jx,kx) - trmi*d2segweight(1,1,ll,kk,jpts)
            derv2(jy,kx) = derv2(jy,kx) - trmi*d2segweight(2,1,ll,kk,jpts)
            derv2(jz,kx) = derv2(jz,kx) - trmi*d2segweight(3,1,ll,kk,jpts)
            derv2(jx,ky) = derv2(jx,ky) - trmi*d2segweight(1,2,ll,kk,jpts)
            derv2(jy,ky) = derv2(jy,ky) - trmi*d2segweight(2,2,ll,kk,jpts)
            derv2(jz,ky) = derv2(jz,ky) - trmi*d2segweight(3,2,ll,kk,jpts)
            derv2(jx,kz) = derv2(jx,kz) - trmi*d2segweight(1,3,ll,kk,jpts)
            derv2(jy,kz) = derv2(jy,kz) - trmi*d2segweight(2,3,ll,kk,jpts)
            derv2(jz,kz) = derv2(jz,kz) - trmi*d2segweight(3,3,ll,kk,jpts)
          endif
          if (j.gt.l) then
            derv2(lx,jx) = derv2(lx,jx) - trmi*d2segweight(1,1,ll,kk,jpts)
            derv2(ly,jx) = derv2(ly,jx) - trmi*d2segweight(2,1,ll,kk,jpts)
            derv2(lz,jx) = derv2(lz,jx) - trmi*d2segweight(3,1,ll,kk,jpts)
            derv2(lx,jy) = derv2(lx,jy) - trmi*d2segweight(1,2,ll,kk,jpts)
            derv2(ly,jy) = derv2(ly,jy) - trmi*d2segweight(2,2,ll,kk,jpts)
            derv2(lz,jy) = derv2(lz,jy) - trmi*d2segweight(3,2,ll,kk,jpts)
            derv2(lx,jz) = derv2(lx,jz) - trmi*d2segweight(1,3,ll,kk,jpts)
            derv2(ly,jz) = derv2(ly,jz) - trmi*d2segweight(2,3,ll,kk,jpts)
            derv2(lz,jz) = derv2(lz,jz) - trmi*d2segweight(3,3,ll,kk,jpts)
          elseif (j.lt.l) then
            derv2(jx,lx) = derv2(jx,lx) - trmi*d2segweight(1,1,ll,kk,jpts)
            derv2(jy,lx) = derv2(jy,lx) - trmi*d2segweight(1,2,ll,kk,jpts)
            derv2(jz,lx) = derv2(jz,lx) - trmi*d2segweight(1,3,ll,kk,jpts)
            derv2(jx,ly) = derv2(jx,ly) - trmi*d2segweight(2,1,ll,kk,jpts)
            derv2(jy,ly) = derv2(jy,ly) - trmi*d2segweight(2,2,ll,kk,jpts)
            derv2(jz,ly) = derv2(jz,ly) - trmi*d2segweight(2,3,ll,kk,jpts)
            derv2(jx,lz) = derv2(jx,lz) - trmi*d2segweight(3,1,ll,kk,jpts)
            derv2(jy,lz) = derv2(jy,lz) - trmi*d2segweight(3,2,ll,kk,jpts)
            derv2(jz,lz) = derv2(jz,lz) - trmi*d2segweight(3,3,ll,kk,jpts)
          endif
          if (k.gt.l) then
            derv2(lx,kx) = derv2(lx,kx) + trmi*d2segweight(1,1,ll,kk,jpts)
            derv2(ly,kx) = derv2(ly,kx) + trmi*d2segweight(2,1,ll,kk,jpts)
            derv2(lz,kx) = derv2(lz,kx) + trmi*d2segweight(3,1,ll,kk,jpts)
            derv2(lx,ky) = derv2(lx,ky) + trmi*d2segweight(1,2,ll,kk,jpts)
            derv2(ly,ky) = derv2(ly,ky) + trmi*d2segweight(2,2,ll,kk,jpts)
            derv2(lz,ky) = derv2(lz,ky) + trmi*d2segweight(3,2,ll,kk,jpts)
            derv2(lx,kz) = derv2(lx,kz) + trmi*d2segweight(1,3,ll,kk,jpts)
            derv2(ly,kz) = derv2(ly,kz) + trmi*d2segweight(2,3,ll,kk,jpts)
            derv2(lz,kz) = derv2(lz,kz) + trmi*d2segweight(3,3,ll,kk,jpts)
          elseif (k.lt.l) then
            derv2(kx,lx) = derv2(kx,lx) + trmi*d2segweight(1,1,ll,kk,jpts)
            derv2(ky,lx) = derv2(ky,lx) + trmi*d2segweight(1,2,ll,kk,jpts)
            derv2(kz,lx) = derv2(kz,lx) + trmi*d2segweight(1,3,ll,kk,jpts)
            derv2(kx,ly) = derv2(kx,ly) + trmi*d2segweight(2,1,ll,kk,jpts)
            derv2(ky,ly) = derv2(ky,ly) + trmi*d2segweight(2,2,ll,kk,jpts)
            derv2(kz,ly) = derv2(kz,ly) + trmi*d2segweight(2,3,ll,kk,jpts)
            derv2(kx,lz) = derv2(kx,lz) + trmi*d2segweight(3,1,ll,kk,jpts)
            derv2(ky,lz) = derv2(ky,lz) + trmi*d2segweight(3,2,ll,kk,jpts)
            derv2(kz,lz) = derv2(kz,lz) + trmi*d2segweight(3,3,ll,kk,jpts)
          endif
        enddo
      enddo
    endif
  endif
!
  return
  end
