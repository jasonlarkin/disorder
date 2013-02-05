  subroutine setdcosmobmat(ldqneeded,lcosmicd2,lgrad2,dcosmoB,fullA,d2qfct,dsegweight,d2segweight)
!
!  Subroutine calculates the derivatives of the COSMO B matrix
!
!   5/03 Created from cosmoderv
!   5/03 Segment weighting added
!   6/03 Segment weighting corrections for d2qsassum added
!  11/04 Intent added
!  11/04 Use of sasparticles data structures added
!   1/05 deltaq removed from argument list
!   1/05 deltaq scaled by segweight for modified COSMIC algorithm
!   1/05 d2qsassum removed by direct addition to derv2
!  10/08 Converted to f90 format
!   7/10 sumAinvipts term initialised for case where lcosmicd2 is false
!
!  It is assumed that lgrad1 = .true. on entry otherwise routine
!  would not have been called. If lgrad = .true. then second
!  derivatives will be calculated as well.
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
  use cosmo
  use constants
  use control
  use current
  use derivatives
  use optimisation
  use reallocate
  implicit none
!
!  Passed variables
!
  logical,        intent(in)                    :: lcosmicd2
  logical,        intent(in)                    :: ldqneeded
  logical,        intent(in)                    :: lgrad2
  real(dp),       intent(in)                    :: d2qfct
  real(dp),       intent(inout)                 :: dcosmoB(3,numat,*)
  real(dp),       intent(in)                    :: dsegweight(3,maxnearseg,*)
  real(dp),       intent(in)                    :: d2segweight(3,3,maxnearseg,maxnearseg,*)
  real(dp),       intent(in)                    :: fullA(npts,*)
!
!  Local variables
!
  integer(i4)                                   :: i
  integer(i4)                                   :: ix
  integer(i4)                                   :: iy
  integer(i4)                                   :: iz
  integer(i4)                                   :: ipts
  integer(i4)                                   :: j
  integer(i4)                                   :: jx
  integer(i4)                                   :: jy
  integer(i4)                                   :: jz
  integer(i4)                                   :: k
  integer(i4)                                   :: kk
  integer(i4)                                   :: kpts
  integer(i4)                                   :: kx
  integer(i4)                                   :: ky
  integer(i4)                                   :: kz
  integer(i4)                                   :: l
  integer(i4)                                   :: ll
  integer(i4)                                   :: lx
  integer(i4)                                   :: ly
  integer(i4)                                   :: lz
  integer(i4)                                   :: n
  real(dp)                                      :: dqme(3)
  real(dp)                                      :: d2qme(6)
  real(dp)                                      :: fact
  real(dp)                                      :: ff
  real(dp)                                      :: ffs
  real(dp)                                      :: qj
  real(dp)                                      :: qme
  real(dp)                                      :: qsk
  real(dp)                                      :: qtrm
  real(dp)                                      :: qtrmnow
  real(dp)                                      :: sumAinvipts
  real(dp)                                      :: swi
  real(dp)                                      :: xd, yd, zd
!
!  Set local constants
!
  fact  = 0.5_dp*autoev*autoangs*cosmofneps
!****************************
!  Derivatives of B matrix  *
!****************************
  do ipts = 1,npts
    i = cosmoatomptr(ipts)
    ix = 3*(i-1) + 1
    iy = ix + 1
    iz = ix + 2
    swi = segweight(ipts)
    qsk = 2.0_dp*qonsas(ipts) 
    sumAinvipts = 0.0_dp
    if (lcosmicd2) then
      do kpts = 1,npts
        sumAinvipts = sumAinvipts + fullA(kpts,ipts)
      enddo
    endif
    do n = 1,nsasparticles
      j = nsasparticleptr(n)
      qj = qsasparticles(n)
      jx = 3*(j - 1) + 1
      jy = jx + 1
      jz = jx + 2
      xd = spxyz(1,ipts) - xclat(j)                                                                         
      yd = spxyz(2,ipts) - yclat(j)                                                                   
      zd = spxyz(3,ipts) - zclat(j)
      call qmatrixelement(xd,yd,zd,0.0_dp,.true.,lgrad2,qme,dqme,d2qme)
      ff = (qsk - deltaq*swi)*fact*qj
      ffs = swi*ff
      qtrmnow = sumAinvipts*qj*d2qfct
      qtrm = qtrmnow*swi
      if (i.ne.j) then
!
!  First derivatives of B matrix due to B matrix element
!
        xdrv(i) = xdrv(i) - ffs*dqme(1)
        ydrv(i) = ydrv(i) - ffs*dqme(2)
        zdrv(i) = zdrv(i) - ffs*dqme(3)
        xdrv(j) = xdrv(j) + ffs*dqme(1)
        ydrv(j) = ydrv(j) + ffs*dqme(2)
        zdrv(j) = zdrv(j) + ffs*dqme(3)
      endif
      if (lsegsmooth) then
!
!  First derivatives of B matrix due to smoothing term
!
        do kk = 1,nnearseg(ipts)
          k = nnearsegptr(kk,ipts)
          xdrv(i) = xdrv(i) - ff*qme*dsegweight(1,kk,ipts)
          ydrv(i) = ydrv(i) - ff*qme*dsegweight(2,kk,ipts)
          zdrv(i) = zdrv(i) - ff*qme*dsegweight(3,kk,ipts)
          xdrv(k) = xdrv(k) + ff*qme*dsegweight(1,kk,ipts)
          ydrv(k) = ydrv(k) + ff*qme*dsegweight(2,kk,ipts)
          zdrv(k) = zdrv(k) + ff*qme*dsegweight(3,kk,ipts)
        enddo
!
!  Smoothing contribution to dcosmoB
!
        if (ldqneeded) then
          do kk = 1,nnearseg(ipts)
            k = nnearsegptr(kk,ipts)
            dcosmoB(1,k,ipts) = dcosmoB(1,k,ipts) + qme*qj*dsegweight(1,kk,ipts)
            dcosmoB(2,k,ipts) = dcosmoB(2,k,ipts) + qme*qj*dsegweight(2,kk,ipts)
            dcosmoB(3,k,ipts) = dcosmoB(3,k,ipts) + qme*qj*dsegweight(3,kk,ipts)
            dcosmoB(1,i,ipts) = dcosmoB(1,i,ipts) - qme*qj*dsegweight(1,kk,ipts)
            dcosmoB(2,i,ipts) = dcosmoB(2,i,ipts) - qme*qj*dsegweight(2,kk,ipts)
            dcosmoB(3,i,ipts) = dcosmoB(3,i,ipts) - qme*qj*dsegweight(3,kk,ipts)
          enddo
        endif
      endif
!
      if (i.ne.j) then
        if (ldqneeded) then
!
!  Save first derivative terms for use in second derivatives
!
          dcosmoB(1,j,ipts) = dcosmoB(1,j,ipts) + swi*dqme(1)*qj
          dcosmoB(2,j,ipts) = dcosmoB(2,j,ipts) + swi*dqme(2)*qj
          dcosmoB(3,j,ipts) = dcosmoB(3,j,ipts) + swi*dqme(3)*qj
          dcosmoB(1,i,ipts) = dcosmoB(1,i,ipts) - swi*dqme(1)*qj
          dcosmoB(2,i,ipts) = dcosmoB(2,i,ipts) - swi*dqme(2)*qj
          dcosmoB(3,i,ipts) = dcosmoB(3,i,ipts) - swi*dqme(3)*qj
        endif
      endif
      if (lgrad2) then
!
!  Second derivatives of B matrix
!
        if (j.lt.i) then
          derv2(jx,ix) = derv2(jx,ix) + ffs*d2qme(1)
          derv2(jy,ix) = derv2(jy,ix) + ffs*d2qme(2)
          derv2(jz,ix) = derv2(jz,ix) + ffs*d2qme(4)
          derv2(jx,iy) = derv2(jx,iy) + ffs*d2qme(2)
          derv2(jy,iy) = derv2(jy,iy) + ffs*d2qme(3)
          derv2(jz,iy) = derv2(jz,iy) + ffs*d2qme(5)
          derv2(jx,iz) = derv2(jx,iz) + ffs*d2qme(4)
          derv2(jy,iz) = derv2(jy,iz) + ffs*d2qme(5)
          derv2(jz,iz) = derv2(jz,iz) + ffs*d2qme(6)
          if (lcosmicd2) then
            derv2(jx,ix) = derv2(jx,ix) + qtrm*d2qme(1)
            derv2(jy,ix) = derv2(jy,ix) + qtrm*d2qme(2)
            derv2(jz,ix) = derv2(jz,ix) + qtrm*d2qme(4)
            derv2(jx,iy) = derv2(jx,iy) + qtrm*d2qme(2)
            derv2(jy,iy) = derv2(jy,iy) + qtrm*d2qme(3)
            derv2(jz,iy) = derv2(jz,iy) + qtrm*d2qme(5)
            derv2(jx,iz) = derv2(jx,iz) + qtrm*d2qme(4)
            derv2(jy,iz) = derv2(jy,iz) + qtrm*d2qme(5)
            derv2(jz,iz) = derv2(jz,iz) + qtrm*d2qme(6)
            if (lsegsmooth) then
              do kk = 1,nnearseg(ipts)
                k = nnearsegptr(kk,ipts)
                kx = 3*(k - 1) + 1
                ky = kx + 1
                kz = kx + 2
                derv2(jx,ix) = derv2(jx,ix) - qtrmnow*dqme(1)*dsegweight(1,kk,ipts)
                derv2(jy,ix) = derv2(jy,ix) - qtrmnow*dqme(2)*dsegweight(1,kk,ipts)
                derv2(jz,ix) = derv2(jz,ix) - qtrmnow*dqme(3)*dsegweight(1,kk,ipts)
                derv2(jx,iy) = derv2(jx,iy) - qtrmnow*dqme(1)*dsegweight(2,kk,ipts)
                derv2(jy,iy) = derv2(jy,iy) - qtrmnow*dqme(2)*dsegweight(2,kk,ipts)
                derv2(jz,iy) = derv2(jz,iy) - qtrmnow*dqme(3)*dsegweight(2,kk,ipts)
                derv2(jx,iz) = derv2(jx,iz) - qtrmnow*dqme(1)*dsegweight(3,kk,ipts)
                derv2(jy,iz) = derv2(jy,iz) - qtrmnow*dqme(2)*dsegweight(3,kk,ipts)
                derv2(jz,iz) = derv2(jz,iz) - qtrmnow*dqme(3)*dsegweight(3,kk,ipts)
!
                if (i.gt.k) then
                  derv2(kx,ix) = derv2(kx,ix) - qtrmnow*(dqme(1)*dsegweight(1,kk,ipts) - qme*d2segweight(1,1,kk,kk,ipts))
                  derv2(ky,ix) = derv2(ky,ix) - qtrmnow*(dqme(1)*dsegweight(2,kk,ipts) - qme*d2segweight(2,1,kk,kk,ipts))
                  derv2(kz,ix) = derv2(kz,ix) - qtrmnow*(dqme(1)*dsegweight(3,kk,ipts) - qme*d2segweight(3,1,kk,kk,ipts))
                  derv2(kx,iy) = derv2(kx,iy) - qtrmnow*(dqme(2)*dsegweight(1,kk,ipts) - qme*d2segweight(1,2,kk,kk,ipts))
                  derv2(ky,iy) = derv2(ky,iy) - qtrmnow*(dqme(2)*dsegweight(2,kk,ipts) - qme*d2segweight(2,2,kk,kk,ipts))
                  derv2(kz,iy) = derv2(kz,iy) - qtrmnow*(dqme(2)*dsegweight(3,kk,ipts) - qme*d2segweight(3,2,kk,kk,ipts))
                  derv2(kx,iz) = derv2(kx,iz) - qtrmnow*(dqme(3)*dsegweight(1,kk,ipts) - qme*d2segweight(1,3,kk,kk,ipts))
                  derv2(ky,iz) = derv2(ky,iz) - qtrmnow*(dqme(3)*dsegweight(2,kk,ipts) - qme*d2segweight(2,3,kk,kk,ipts))
                  derv2(kz,iz) = derv2(kz,iz) - qtrmnow*(dqme(3)*dsegweight(3,kk,ipts) - qme*d2segweight(3,3,kk,kk,ipts))
                else
                  derv2(ix,kx) = derv2(ix,kx) - qtrmnow*(dqme(1)*dsegweight(1,kk,ipts) - qme*d2segweight(1,1,kk,kk,ipts))
                  derv2(iy,kx) = derv2(iy,kx) - qtrmnow*(dqme(2)*dsegweight(1,kk,ipts) - qme*d2segweight(1,2,kk,kk,ipts))
                  derv2(iz,kx) = derv2(iz,kx) - qtrmnow*(dqme(3)*dsegweight(1,kk,ipts) - qme*d2segweight(1,3,kk,kk,ipts))
                  derv2(ix,ky) = derv2(ix,ky) - qtrmnow*(dqme(1)*dsegweight(2,kk,ipts) - qme*d2segweight(2,1,kk,kk,ipts))
                  derv2(iy,ky) = derv2(iy,ky) - qtrmnow*(dqme(2)*dsegweight(2,kk,ipts) - qme*d2segweight(2,2,kk,kk,ipts))
                  derv2(iz,ky) = derv2(iz,ky) - qtrmnow*(dqme(3)*dsegweight(2,kk,ipts) - qme*d2segweight(2,3,kk,kk,ipts))
                  derv2(ix,kz) = derv2(ix,kz) - qtrmnow*(dqme(1)*dsegweight(3,kk,ipts) - qme*d2segweight(3,1,kk,kk,ipts))
                  derv2(iy,kz) = derv2(iy,kz) - qtrmnow*(dqme(2)*dsegweight(3,kk,ipts) - qme*d2segweight(3,2,kk,kk,ipts))
                  derv2(iz,kz) = derv2(iz,kz) - qtrmnow*(dqme(3)*dsegweight(3,kk,ipts) - qme*d2segweight(3,3,kk,kk,ipts))
                endif
!
                if (j.gt.k) then
                  derv2(kx,jx) = derv2(kx,jx) + qtrmnow*dqme(1)*dsegweight(1,kk,ipts)
                  derv2(ky,jx) = derv2(ky,jx) + qtrmnow*dqme(1)*dsegweight(2,kk,ipts)
                  derv2(kz,jx) = derv2(kz,jx) + qtrmnow*dqme(1)*dsegweight(3,kk,ipts)
                  derv2(kx,jy) = derv2(kx,jy) + qtrmnow*dqme(2)*dsegweight(1,kk,ipts)
                  derv2(ky,jy) = derv2(ky,jy) + qtrmnow*dqme(2)*dsegweight(2,kk,ipts)
                  derv2(kz,jy) = derv2(kz,jy) + qtrmnow*dqme(2)*dsegweight(3,kk,ipts)
                  derv2(kx,jz) = derv2(kx,jz) + qtrmnow*dqme(3)*dsegweight(1,kk,ipts)
                  derv2(ky,jz) = derv2(ky,jz) + qtrmnow*dqme(3)*dsegweight(2,kk,ipts)
                  derv2(kz,jz) = derv2(kz,jz) + qtrmnow*dqme(3)*dsegweight(3,kk,ipts)
                else
                  derv2(jx,kx) = derv2(jx,kx) + qtrmnow*dqme(1)*dsegweight(1,kk,ipts)
                  derv2(jy,kx) = derv2(jy,kx) + qtrmnow*dqme(2)*dsegweight(1,kk,ipts)
                  derv2(jz,kx) = derv2(jz,kx) + qtrmnow*dqme(3)*dsegweight(1,kk,ipts)
                  derv2(jx,ky) = derv2(jx,ky) + qtrmnow*dqme(1)*dsegweight(2,kk,ipts)
                  derv2(jy,ky) = derv2(jy,ky) + qtrmnow*dqme(2)*dsegweight(2,kk,ipts)
                  derv2(jz,ky) = derv2(jz,ky) + qtrmnow*dqme(3)*dsegweight(2,kk,ipts)
                  derv2(jx,kz) = derv2(jx,kz) + qtrmnow*dqme(1)*dsegweight(3,kk,ipts)
                  derv2(jy,kz) = derv2(jy,kz) + qtrmnow*dqme(2)*dsegweight(3,kk,ipts)
                  derv2(jz,kz) = derv2(jz,kz) + qtrmnow*dqme(3)*dsegweight(3,kk,ipts)
                endif
              enddo
!
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
                  if (k.gt.l) then
                    derv2(lx,kx) = derv2(lx,kx) + qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(ly,kx) = derv2(ly,kx) + qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(lz,kx) = derv2(lz,kx) + qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(lx,ky) = derv2(lx,ky) + qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(ly,ky) = derv2(ly,ky) + qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(lz,ky) = derv2(lz,ky) + qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(lx,kz) = derv2(lx,kz) + qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(ly,kz) = derv2(ly,kz) + qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(lz,kz) = derv2(lz,kz) + qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  else
                    derv2(kx,lx) = derv2(kx,lx) + qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(ky,lx) = derv2(ky,lx) + qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(kz,lx) = derv2(kz,lx) + qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(kx,ly) = derv2(kx,ly) + qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(ky,ly) = derv2(ky,ly) + qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(kz,ly) = derv2(kz,ly) + qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(kx,lz) = derv2(kx,lz) + qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(ky,lz) = derv2(ky,lz) + qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(kz,lz) = derv2(kz,lz) + qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  endif
!
                  if (i.gt.k) then
                    derv2(kx,ix) = derv2(kx,ix) - qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(ky,ix) = derv2(ky,ix) - qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(kz,ix) = derv2(kz,ix) - qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(kx,iy) = derv2(kx,iy) - qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(ky,iy) = derv2(ky,iy) - qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(kz,iy) = derv2(kz,iy) - qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(kx,iz) = derv2(kx,iz) - qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(ky,iz) = derv2(ky,iz) - qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(kz,iz) = derv2(kz,iz) - qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  else
                    derv2(ix,kx) = derv2(ix,kx) - qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(iy,kx) = derv2(iy,kx) - qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(iz,kx) = derv2(iz,kx) - qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(ix,ky) = derv2(ix,ky) - qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(iy,ky) = derv2(iy,ky) - qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(iz,ky) = derv2(iz,ky) - qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(ix,kz) = derv2(ix,kz) - qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(iy,kz) = derv2(iy,kz) - qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(iz,kz) = derv2(iz,kz) - qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  endif
!
                  if (i.gt.l) then
                    derv2(lx,ix) = derv2(lx,ix) - qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(ly,ix) = derv2(ly,ix) - qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(lz,ix) = derv2(lz,ix) - qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(lx,iy) = derv2(lx,iy) - qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(ly,iy) = derv2(ly,iy) - qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(lz,iy) = derv2(lz,iy) - qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(lx,iz) = derv2(lx,iz) - qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(ly,iz) = derv2(ly,iz) - qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(lz,iz) = derv2(lz,iz) - qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  else
                    derv2(ix,lx) = derv2(ix,lx) - qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(iy,lx) = derv2(iy,lx) - qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(iz,lx) = derv2(iz,lx) - qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(ix,ly) = derv2(ix,ly) - qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(iy,ly) = derv2(iy,ly) - qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(iz,ly) = derv2(iz,ly) - qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(ix,lz) = derv2(ix,lz) - qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(iy,lz) = derv2(iy,lz) - qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(iz,lz) = derv2(iz,lz) - qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  endif
                enddo
              enddo
            endif
          endif
        else
          if (i.lt.j) then
            derv2(ix,jx) = derv2(ix,jx) + ffs*d2qme(1)
            derv2(iy,jx) = derv2(iy,jx) + ffs*d2qme(2)
            derv2(iz,jx) = derv2(iz,jx) + ffs*d2qme(4)
            derv2(ix,jy) = derv2(ix,jy) + ffs*d2qme(2)
            derv2(iy,jy) = derv2(iy,jy) + ffs*d2qme(3)
            derv2(iz,jy) = derv2(iz,jy) + ffs*d2qme(5)
            derv2(ix,jz) = derv2(ix,jz) + ffs*d2qme(4)
            derv2(iy,jz) = derv2(iy,jz) + ffs*d2qme(5)
            derv2(iz,jz) = derv2(iz,jz) + ffs*d2qme(6)
          endif
          if (lcosmicd2) then
            if (i.lt.j) then
              derv2(ix,jx) = derv2(ix,jx) + qtrm*d2qme(1)
              derv2(iy,jx) = derv2(iy,jx) + qtrm*d2qme(2)
              derv2(iz,jx) = derv2(iz,jx) + qtrm*d2qme(4)
              derv2(ix,jy) = derv2(ix,jy) + qtrm*d2qme(2)
              derv2(iy,jy) = derv2(iy,jy) + qtrm*d2qme(3)
              derv2(iz,jy) = derv2(iz,jy) + qtrm*d2qme(5)
              derv2(ix,jz) = derv2(ix,jz) + qtrm*d2qme(4)
              derv2(iy,jz) = derv2(iy,jz) + qtrm*d2qme(5)
              derv2(iz,jz) = derv2(iz,jz) + qtrm*d2qme(6)
            endif
            if (lsegsmooth) then
              do kk = 1,nnearseg(ipts)
                k = nnearsegptr(kk,ipts)
                kx = 3*(k - 1) + 1
                ky = kx + 1
                kz = kx + 2
                if (i.lt.j) then
                  derv2(ix,jx) = derv2(ix,jx) - qtrmnow*dqme(1)*dsegweight(1,kk,ipts)
                  derv2(iy,jx) = derv2(iy,jx) - qtrmnow*dqme(1)*dsegweight(2,kk,ipts)
                  derv2(iz,jx) = derv2(iz,jx) - qtrmnow*dqme(1)*dsegweight(3,kk,ipts)
                  derv2(ix,jy) = derv2(ix,jy) - qtrmnow*dqme(2)*dsegweight(1,kk,ipts)
                  derv2(iy,jy) = derv2(iy,jy) - qtrmnow*dqme(2)*dsegweight(2,kk,ipts)
                  derv2(iz,jy) = derv2(iz,jy) - qtrmnow*dqme(2)*dsegweight(3,kk,ipts)
                  derv2(ix,jz) = derv2(ix,jz) - qtrmnow*dqme(3)*dsegweight(1,kk,ipts)
                  derv2(iy,jz) = derv2(iy,jz) - qtrmnow*dqme(3)*dsegweight(2,kk,ipts)
                  derv2(iz,jz) = derv2(iz,jz) - qtrmnow*dqme(3)*dsegweight(3,kk,ipts)
                endif
!
                if (i.gt.k) then
                  derv2(kx,ix) = derv2(kx,ix) - qtrmnow*(dqme(1)*dsegweight(1,kk,ipts) - qme*d2segweight(1,1,kk,kk,ipts))
                  derv2(ky,ix) = derv2(ky,ix) - qtrmnow*(dqme(1)*dsegweight(2,kk,ipts) - qme*d2segweight(2,1,kk,kk,ipts))
                  derv2(kz,ix) = derv2(kz,ix) - qtrmnow*(dqme(1)*dsegweight(3,kk,ipts) - qme*d2segweight(3,1,kk,kk,ipts))
                  derv2(kx,iy) = derv2(kx,iy) - qtrmnow*(dqme(2)*dsegweight(1,kk,ipts) - qme*d2segweight(1,2,kk,kk,ipts))
                  derv2(ky,iy) = derv2(ky,iy) - qtrmnow*(dqme(2)*dsegweight(2,kk,ipts) - qme*d2segweight(2,2,kk,kk,ipts))
                  derv2(kz,iy) = derv2(kz,iy) - qtrmnow*(dqme(2)*dsegweight(3,kk,ipts) - qme*d2segweight(3,2,kk,kk,ipts))
                  derv2(kx,iz) = derv2(kx,iz) - qtrmnow*(dqme(3)*dsegweight(1,kk,ipts) - qme*d2segweight(1,3,kk,kk,ipts))
                  derv2(ky,iz) = derv2(ky,iz) - qtrmnow*(dqme(3)*dsegweight(2,kk,ipts) - qme*d2segweight(2,3,kk,kk,ipts))
                  derv2(kz,iz) = derv2(kz,iz) - qtrmnow*(dqme(3)*dsegweight(3,kk,ipts) - qme*d2segweight(3,3,kk,kk,ipts))
                else
                  derv2(ix,kx) = derv2(ix,kx) - qtrmnow*(dqme(1)*dsegweight(1,kk,ipts) - qme*d2segweight(1,1,kk,kk,ipts))
                  derv2(iy,kx) = derv2(iy,kx) - qtrmnow*(dqme(2)*dsegweight(1,kk,ipts) - qme*d2segweight(1,2,kk,kk,ipts))
                  derv2(iz,kx) = derv2(iz,kx) - qtrmnow*(dqme(3)*dsegweight(1,kk,ipts) - qme*d2segweight(1,3,kk,kk,ipts))
                  derv2(ix,ky) = derv2(ix,ky) - qtrmnow*(dqme(1)*dsegweight(2,kk,ipts) - qme*d2segweight(2,1,kk,kk,ipts))
                  derv2(iy,ky) = derv2(iy,ky) - qtrmnow*(dqme(2)*dsegweight(2,kk,ipts) - qme*d2segweight(2,2,kk,kk,ipts))
                  derv2(iz,ky) = derv2(iz,ky) - qtrmnow*(dqme(3)*dsegweight(2,kk,ipts) - qme*d2segweight(2,3,kk,kk,ipts))
                  derv2(ix,kz) = derv2(ix,kz) - qtrmnow*(dqme(1)*dsegweight(3,kk,ipts) - qme*d2segweight(3,1,kk,kk,ipts))
                  derv2(iy,kz) = derv2(iy,kz) - qtrmnow*(dqme(2)*dsegweight(3,kk,ipts) - qme*d2segweight(3,2,kk,kk,ipts))
                  derv2(iz,kz) = derv2(iz,kz) - qtrmnow*(dqme(3)*dsegweight(3,kk,ipts) - qme*d2segweight(3,3,kk,kk,ipts))
                endif
!
                if (j.gt.k) then
                  derv2(kx,jx) = derv2(kx,jx) + qtrmnow*dqme(1)*dsegweight(1,kk,ipts)
                  derv2(ky,jx) = derv2(ky,jx) + qtrmnow*dqme(1)*dsegweight(2,kk,ipts)
                  derv2(kz,jx) = derv2(kz,jx) + qtrmnow*dqme(1)*dsegweight(3,kk,ipts)
                  derv2(kx,jy) = derv2(kx,jy) + qtrmnow*dqme(2)*dsegweight(1,kk,ipts)
                  derv2(ky,jy) = derv2(ky,jy) + qtrmnow*dqme(2)*dsegweight(2,kk,ipts)
                  derv2(kz,jy) = derv2(kz,jy) + qtrmnow*dqme(2)*dsegweight(3,kk,ipts)
                  derv2(kx,jz) = derv2(kx,jz) + qtrmnow*dqme(3)*dsegweight(1,kk,ipts)
                  derv2(ky,jz) = derv2(ky,jz) + qtrmnow*dqme(3)*dsegweight(2,kk,ipts)
                  derv2(kz,jz) = derv2(kz,jz) + qtrmnow*dqme(3)*dsegweight(3,kk,ipts)
                else
                  derv2(jx,kx) = derv2(jx,kx) + qtrmnow*dqme(1)*dsegweight(1,kk,ipts)
                  derv2(jy,kx) = derv2(jy,kx) + qtrmnow*dqme(2)*dsegweight(1,kk,ipts)
                  derv2(jz,kx) = derv2(jz,kx) + qtrmnow*dqme(3)*dsegweight(1,kk,ipts)
                  derv2(jx,ky) = derv2(jx,ky) + qtrmnow*dqme(1)*dsegweight(2,kk,ipts)
                  derv2(jy,ky) = derv2(jy,ky) + qtrmnow*dqme(2)*dsegweight(2,kk,ipts)
                  derv2(jz,ky) = derv2(jz,ky) + qtrmnow*dqme(3)*dsegweight(2,kk,ipts)
                  derv2(jx,kz) = derv2(jx,kz) + qtrmnow*dqme(1)*dsegweight(3,kk,ipts)
                  derv2(jy,kz) = derv2(jy,kz) + qtrmnow*dqme(2)*dsegweight(3,kk,ipts)
                  derv2(jz,kz) = derv2(jz,kz) + qtrmnow*dqme(3)*dsegweight(3,kk,ipts)
                endif
              enddo
!
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
                  if (k.gt.l) then
                    derv2(lx,kx) = derv2(lx,kx) + qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(ly,kx) = derv2(ly,kx) + qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(lz,kx) = derv2(lz,kx) + qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(lx,ky) = derv2(lx,ky) + qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(ly,ky) = derv2(ly,ky) + qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(lz,ky) = derv2(lz,ky) + qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(lx,kz) = derv2(lx,kz) + qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(ly,kz) = derv2(ly,kz) + qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(lz,kz) = derv2(lz,kz) + qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  else
                    derv2(kx,lx) = derv2(kx,lx) + qtrmnow*qme*d2segweight(1,1,ll,kk,ipts)
                    derv2(ky,lx) = derv2(ky,lx) + qtrmnow*qme*d2segweight(1,2,ll,kk,ipts)
                    derv2(kz,lx) = derv2(kz,lx) + qtrmnow*qme*d2segweight(1,3,ll,kk,ipts)
                    derv2(kx,ly) = derv2(kx,ly) + qtrmnow*qme*d2segweight(2,1,ll,kk,ipts)
                    derv2(ky,ly) = derv2(ky,ly) + qtrmnow*qme*d2segweight(2,2,ll,kk,ipts)
                    derv2(kz,ly) = derv2(kz,ly) + qtrmnow*qme*d2segweight(2,3,ll,kk,ipts)
                    derv2(kx,lz) = derv2(kx,lz) + qtrmnow*qme*d2segweight(3,1,ll,kk,ipts)
                    derv2(ky,lz) = derv2(ky,lz) + qtrmnow*qme*d2segweight(3,2,ll,kk,ipts)
                    derv2(kz,lz) = derv2(kz,lz) + qtrmnow*qme*d2segweight(3,3,ll,kk,ipts)
                  endif
!
                  if (i.gt.k) then
                    derv2(kx,ix) = derv2(kx,ix) - qtrmnow*qme*d2segweight(1,1,kk,ll,ipts)
                    derv2(ky,ix) = derv2(ky,ix) - qtrmnow*qme*d2segweight(2,1,kk,ll,ipts)
                    derv2(kz,ix) = derv2(kz,ix) - qtrmnow*qme*d2segweight(3,1,kk,ll,ipts)
                    derv2(kx,iy) = derv2(kx,iy) - qtrmnow*qme*d2segweight(1,2,kk,ll,ipts)
                    derv2(ky,iy) = derv2(ky,iy) - qtrmnow*qme*d2segweight(2,2,kk,ll,ipts)
                    derv2(kz,iy) = derv2(kz,iy) - qtrmnow*qme*d2segweight(3,2,kk,ll,ipts)
                    derv2(kx,iz) = derv2(kx,iz) - qtrmnow*qme*d2segweight(1,3,kk,ll,ipts)
                    derv2(ky,iz) = derv2(ky,iz) - qtrmnow*qme*d2segweight(2,3,kk,ll,ipts)
                    derv2(kz,iz) = derv2(kz,iz) - qtrmnow*qme*d2segweight(3,3,kk,ll,ipts)
                  else
                    derv2(ix,kx) = derv2(ix,kx) - qtrmnow*qme*d2segweight(1,1,kk,ll,ipts)
                    derv2(iy,kx) = derv2(iy,kx) - qtrmnow*qme*d2segweight(1,2,kk,ll,ipts)
                    derv2(iz,kx) = derv2(iz,kx) - qtrmnow*qme*d2segweight(1,3,kk,ll,ipts)
                    derv2(ix,ky) = derv2(ix,ky) - qtrmnow*qme*d2segweight(2,1,kk,ll,ipts)
                    derv2(iy,ky) = derv2(iy,ky) - qtrmnow*qme*d2segweight(2,2,kk,ll,ipts)
                    derv2(iz,ky) = derv2(iz,ky) - qtrmnow*qme*d2segweight(2,3,kk,ll,ipts)
                    derv2(ix,kz) = derv2(ix,kz) - qtrmnow*qme*d2segweight(3,1,kk,ll,ipts)
                    derv2(iy,kz) = derv2(iy,kz) - qtrmnow*qme*d2segweight(3,2,kk,ll,ipts)
                    derv2(iz,kz) = derv2(iz,kz) - qtrmnow*qme*d2segweight(3,3,kk,ll,ipts)
                  endif
!
                  if (i.gt.l) then
                    derv2(lx,ix) = derv2(lx,ix) - qtrmnow*qme*d2segweight(1,1,kk,ll,ipts)
                    derv2(ly,ix) = derv2(ly,ix) - qtrmnow*qme*d2segweight(1,2,kk,ll,ipts)
                    derv2(lz,ix) = derv2(lz,ix) - qtrmnow*qme*d2segweight(1,3,kk,ll,ipts)
                    derv2(lx,iy) = derv2(lx,iy) - qtrmnow*qme*d2segweight(2,1,kk,ll,ipts)
                    derv2(ly,iy) = derv2(ly,iy) - qtrmnow*qme*d2segweight(2,2,kk,ll,ipts)
                    derv2(lz,iy) = derv2(lz,iy) - qtrmnow*qme*d2segweight(2,3,kk,ll,ipts)
                    derv2(lx,iz) = derv2(lx,iz) - qtrmnow*qme*d2segweight(3,1,kk,ll,ipts)
                    derv2(ly,iz) = derv2(ly,iz) - qtrmnow*qme*d2segweight(3,2,kk,ll,ipts)
                    derv2(lz,iz) = derv2(lz,iz) - qtrmnow*qme*d2segweight(3,3,kk,ll,ipts)
                  else
                    derv2(ix,lx) = derv2(ix,lx) - qtrmnow*qme*d2segweight(1,1,kk,ll,ipts)
                    derv2(iy,lx) = derv2(iy,lx) - qtrmnow*qme*d2segweight(2,1,kk,ll,ipts)
                    derv2(iz,lx) = derv2(iz,lx) - qtrmnow*qme*d2segweight(3,1,kk,ll,ipts)
                    derv2(ix,ly) = derv2(ix,ly) - qtrmnow*qme*d2segweight(1,2,kk,ll,ipts)
                    derv2(iy,ly) = derv2(iy,ly) - qtrmnow*qme*d2segweight(2,2,kk,ll,ipts)
                    derv2(iz,ly) = derv2(iz,ly) - qtrmnow*qme*d2segweight(3,2,kk,ll,ipts)
                    derv2(ix,lz) = derv2(ix,lz) - qtrmnow*qme*d2segweight(1,3,kk,ll,ipts)
                    derv2(iy,lz) = derv2(iy,lz) - qtrmnow*qme*d2segweight(2,3,kk,ll,ipts)
                    derv2(iz,lz) = derv2(iz,lz) - qtrmnow*qme*d2segweight(3,3,kk,ll,ipts)
                  endif
                enddo
              enddo
            endif
          endif
        endif
        if (lsegsmooth) then
!
!  Product of first derivatives due to smoothing term
!
          do kk = 1,nnearseg(ipts)
            k = nnearsegptr(kk,ipts)
            kx = 3*(k-1) + 1
            ky = kx + 1
            kz = kx + 2
            if (j.lt.i) then
              derv2(jx,ix) = derv2(jx,ix) - ff*dqme(1)*dsegweight(1,kk,ipts)
              derv2(jy,ix) = derv2(jy,ix) - ff*dqme(2)*dsegweight(1,kk,ipts)
              derv2(jz,ix) = derv2(jz,ix) - ff*dqme(3)*dsegweight(1,kk,ipts)
              derv2(jx,iy) = derv2(jx,iy) - ff*dqme(1)*dsegweight(2,kk,ipts)
              derv2(jy,iy) = derv2(jy,iy) - ff*dqme(2)*dsegweight(2,kk,ipts)
              derv2(jz,iy) = derv2(jz,iy) - ff*dqme(3)*dsegweight(2,kk,ipts)
              derv2(jx,iz) = derv2(jx,iz) - ff*dqme(1)*dsegweight(3,kk,ipts)
              derv2(jy,iz) = derv2(jy,iz) - ff*dqme(2)*dsegweight(3,kk,ipts)
              derv2(jz,iz) = derv2(jz,iz) - ff*dqme(3)*dsegweight(3,kk,ipts)
            elseif (i.lt.j) then
              derv2(ix,jx) = derv2(ix,jx) - ff*dqme(1)*dsegweight(1,kk,ipts)
              derv2(iy,jx) = derv2(iy,jx) - ff*dqme(1)*dsegweight(2,kk,ipts)
              derv2(iz,jx) = derv2(iz,jx) - ff*dqme(1)*dsegweight(3,kk,ipts)
              derv2(ix,jy) = derv2(ix,jy) - ff*dqme(2)*dsegweight(1,kk,ipts)
              derv2(iy,jy) = derv2(iy,jy) - ff*dqme(2)*dsegweight(2,kk,ipts)
              derv2(iz,jy) = derv2(iz,jy) - ff*dqme(2)*dsegweight(3,kk,ipts)
              derv2(ix,jz) = derv2(ix,jz) - ff*dqme(3)*dsegweight(1,kk,ipts)
              derv2(iy,jz) = derv2(iy,jz) - ff*dqme(3)*dsegweight(2,kk,ipts)
              derv2(iz,jz) = derv2(iz,jz) - ff*dqme(3)*dsegweight(3,kk,ipts)
            endif
            if (k.lt.i) then
              derv2(kx,ix) = derv2(kx,ix) - ff*dqme(1)*dsegweight(1,kk,ipts)
              derv2(ky,ix) = derv2(ky,ix) - ff*dqme(1)*dsegweight(2,kk,ipts)
              derv2(kz,ix) = derv2(kz,ix) - ff*dqme(1)*dsegweight(3,kk,ipts)
              derv2(kx,iy) = derv2(kx,iy) - ff*dqme(2)*dsegweight(1,kk,ipts)
              derv2(ky,iy) = derv2(ky,iy) - ff*dqme(2)*dsegweight(2,kk,ipts)
              derv2(kz,iy) = derv2(kz,iy) - ff*dqme(2)*dsegweight(3,kk,ipts)
              derv2(kx,iz) = derv2(kx,iz) - ff*dqme(3)*dsegweight(1,kk,ipts)
              derv2(ky,iz) = derv2(ky,iz) - ff*dqme(3)*dsegweight(2,kk,ipts)
              derv2(kz,iz) = derv2(kz,iz) - ff*dqme(3)*dsegweight(3,kk,ipts)
            else
              derv2(ix,kx) = derv2(ix,kx) - ff*dqme(1)*dsegweight(1,kk,ipts)
              derv2(iy,kx) = derv2(iy,kx) - ff*dqme(2)*dsegweight(1,kk,ipts)
              derv2(iz,kx) = derv2(iz,kx) - ff*dqme(3)*dsegweight(1,kk,ipts)
              derv2(ix,ky) = derv2(ix,ky) - ff*dqme(1)*dsegweight(2,kk,ipts)
              derv2(iy,ky) = derv2(iy,ky) - ff*dqme(2)*dsegweight(2,kk,ipts)
              derv2(iz,ky) = derv2(iz,ky) - ff*dqme(3)*dsegweight(2,kk,ipts)
              derv2(ix,kz) = derv2(ix,kz) - ff*dqme(1)*dsegweight(3,kk,ipts)
              derv2(iy,kz) = derv2(iy,kz) - ff*dqme(2)*dsegweight(3,kk,ipts)
              derv2(iz,kz) = derv2(iz,kz) - ff*dqme(3)*dsegweight(3,kk,ipts)
            endif
            if (k.lt.j) then
              derv2(kx,jx) = derv2(kx,jx) + ff*dqme(1)*dsegweight(1,kk,ipts)
              derv2(ky,jx) = derv2(ky,jx) + ff*dqme(1)*dsegweight(2,kk,ipts)
              derv2(kz,jx) = derv2(kz,jx) + ff*dqme(1)*dsegweight(3,kk,ipts)
              derv2(kx,jy) = derv2(kx,jy) + ff*dqme(2)*dsegweight(1,kk,ipts)
              derv2(ky,jy) = derv2(ky,jy) + ff*dqme(2)*dsegweight(2,kk,ipts)
              derv2(kz,jy) = derv2(kz,jy) + ff*dqme(2)*dsegweight(3,kk,ipts)
              derv2(kx,jz) = derv2(kx,jz) + ff*dqme(3)*dsegweight(1,kk,ipts)
              derv2(ky,jz) = derv2(ky,jz) + ff*dqme(3)*dsegweight(2,kk,ipts)
              derv2(kz,jz) = derv2(kz,jz) + ff*dqme(3)*dsegweight(3,kk,ipts)
            else
              derv2(jx,kx) = derv2(jx,kx) + ff*dqme(1)*dsegweight(1,kk,ipts)
              derv2(jy,kx) = derv2(jy,kx) + ff*dqme(2)*dsegweight(1,kk,ipts)
              derv2(jz,kx) = derv2(jz,kx) + ff*dqme(3)*dsegweight(1,kk,ipts)
              derv2(jx,ky) = derv2(jx,ky) + ff*dqme(1)*dsegweight(2,kk,ipts)
              derv2(jy,ky) = derv2(jy,ky) + ff*dqme(2)*dsegweight(2,kk,ipts)
              derv2(jz,ky) = derv2(jz,ky) + ff*dqme(3)*dsegweight(2,kk,ipts)
              derv2(jx,kz) = derv2(jx,kz) + ff*dqme(1)*dsegweight(3,kk,ipts)
              derv2(jy,kz) = derv2(jy,kz) + ff*dqme(2)*dsegweight(3,kk,ipts)
              derv2(jz,kz) = derv2(jz,kz) + ff*dqme(3)*dsegweight(3,kk,ipts)
            endif
          enddo
!
!  Second derivatives of B matrix due to smoothing term
!
          do kk = 1,nnearseg(ipts)
            k = nnearsegptr(kk,ipts)
            kx = 3*(k-1) + 1
            ky = kx + 1
            kz = kx + 2
            if (k.lt.i) then
              derv2(kx,ix) = derv2(kx,ix) + ff*qme*d2segweight(1,1,kk,kk,ipts)
              derv2(ky,ix) = derv2(ky,ix) + ff*qme*d2segweight(2,1,kk,kk,ipts)
              derv2(kz,ix) = derv2(kz,ix) + ff*qme*d2segweight(3,1,kk,kk,ipts)
              derv2(kx,iy) = derv2(kx,iy) + ff*qme*d2segweight(1,2,kk,kk,ipts)
              derv2(ky,iy) = derv2(ky,iy) + ff*qme*d2segweight(2,2,kk,kk,ipts)
              derv2(kz,iy) = derv2(kz,iy) + ff*qme*d2segweight(3,2,kk,kk,ipts)
              derv2(kx,iz) = derv2(kx,iz) + ff*qme*d2segweight(1,3,kk,kk,ipts)
              derv2(ky,iz) = derv2(ky,iz) + ff*qme*d2segweight(2,3,kk,kk,ipts)
              derv2(kz,iz) = derv2(kz,iz) + ff*qme*d2segweight(3,3,kk,kk,ipts)
            elseif (k.gt.i) then
              derv2(ix,kx) = derv2(ix,kx) + ff*qme*d2segweight(1,1,kk,kk,ipts)
              derv2(iy,kx) = derv2(iy,kx) + ff*qme*d2segweight(1,2,kk,kk,ipts)
              derv2(iz,kx) = derv2(iz,kx) + ff*qme*d2segweight(1,3,kk,kk,ipts)
              derv2(ix,ky) = derv2(ix,ky) + ff*qme*d2segweight(2,1,kk,kk,ipts)
              derv2(iy,ky) = derv2(iy,ky) + ff*qme*d2segweight(2,2,kk,kk,ipts)
              derv2(iz,ky) = derv2(iz,ky) + ff*qme*d2segweight(2,3,kk,kk,ipts)
              derv2(ix,kz) = derv2(ix,kz) + ff*qme*d2segweight(3,1,kk,kk,ipts)
              derv2(iy,kz) = derv2(iy,kz) + ff*qme*d2segweight(3,2,kk,kk,ipts)
              derv2(iz,kz) = derv2(iz,kz) + ff*qme*d2segweight(3,3,kk,kk,ipts)
            endif
            do ll = 1,kk-1
              l = nnearsegptr(ll,ipts)
              lx = 3*(l-1) + 1
              ly = lx + 1
              lz = lx + 2
              derv2(lx,kx) = derv2(lx,kx) + ff*qme*d2segweight(1,1,ll,kk,ipts)
              derv2(ly,kx) = derv2(ly,kx) + ff*qme*d2segweight(2,1,ll,kk,ipts)
              derv2(lz,kx) = derv2(lz,kx) + ff*qme*d2segweight(3,1,ll,kk,ipts)
              derv2(lx,ky) = derv2(lx,ky) + ff*qme*d2segweight(1,2,ll,kk,ipts)
              derv2(ly,ky) = derv2(ly,ky) + ff*qme*d2segweight(2,2,ll,kk,ipts)
              derv2(lz,ky) = derv2(lz,ky) + ff*qme*d2segweight(3,2,ll,kk,ipts)
              derv2(lx,kz) = derv2(lx,kz) + ff*qme*d2segweight(1,3,ll,kk,ipts)
              derv2(ly,kz) = derv2(ly,kz) + ff*qme*d2segweight(2,3,ll,kk,ipts)
              derv2(lz,kz) = derv2(lz,kz) + ff*qme*d2segweight(3,3,ll,kk,ipts)
              if (k.lt.i) then
                derv2(kx,ix) = derv2(kx,ix) - ff*qme*d2segweight(1,1,ll,kk,ipts)
                derv2(ky,ix) = derv2(ky,ix) - ff*qme*d2segweight(1,2,ll,kk,ipts)
                derv2(kz,ix) = derv2(kz,ix) - ff*qme*d2segweight(1,3,ll,kk,ipts)
                derv2(kx,iy) = derv2(kx,iy) - ff*qme*d2segweight(2,1,ll,kk,ipts)
                derv2(ky,iy) = derv2(ky,iy) - ff*qme*d2segweight(2,2,ll,kk,ipts)
                derv2(kz,iy) = derv2(kz,iy) - ff*qme*d2segweight(2,3,ll,kk,ipts)
                derv2(kx,iz) = derv2(kx,iz) - ff*qme*d2segweight(3,1,ll,kk,ipts)
                derv2(ky,iz) = derv2(ky,iz) - ff*qme*d2segweight(3,2,ll,kk,ipts)
                derv2(kz,iz) = derv2(kz,iz) - ff*qme*d2segweight(3,3,ll,kk,ipts)
              elseif (k.gt.i) then
                derv2(ix,kx) = derv2(ix,kx) - ff*qme*d2segweight(1,1,ll,kk,ipts)
                derv2(iy,kx) = derv2(iy,kx) - ff*qme*d2segweight(2,1,ll,kk,ipts)
                derv2(iz,kx) = derv2(iz,kx) - ff*qme*d2segweight(3,1,ll,kk,ipts)
                derv2(ix,ky) = derv2(ix,ky) - ff*qme*d2segweight(1,2,ll,kk,ipts)
                derv2(iy,ky) = derv2(iy,ky) - ff*qme*d2segweight(2,2,ll,kk,ipts)
                derv2(iz,ky) = derv2(iz,ky) - ff*qme*d2segweight(3,2,ll,kk,ipts)
                derv2(ix,kz) = derv2(ix,kz) - ff*qme*d2segweight(1,3,ll,kk,ipts)
                derv2(iy,kz) = derv2(iy,kz) - ff*qme*d2segweight(2,3,ll,kk,ipts)
                derv2(iz,kz) = derv2(iz,kz) - ff*qme*d2segweight(3,3,ll,kk,ipts)
              endif
              if (l.lt.i) then
                derv2(lx,ix) = derv2(lx,ix) - ff*qme*d2segweight(1,1,ll,kk,ipts)
                derv2(ly,ix) = derv2(ly,ix) - ff*qme*d2segweight(2,1,ll,kk,ipts)
                derv2(lz,ix) = derv2(lz,ix) - ff*qme*d2segweight(3,1,ll,kk,ipts)
                derv2(lx,iy) = derv2(lx,iy) - ff*qme*d2segweight(1,2,ll,kk,ipts)
                derv2(ly,iy) = derv2(ly,iy) - ff*qme*d2segweight(2,2,ll,kk,ipts)
                derv2(lz,iy) = derv2(lz,iy) - ff*qme*d2segweight(3,2,ll,kk,ipts)
                derv2(lx,iz) = derv2(lx,iz) - ff*qme*d2segweight(1,3,ll,kk,ipts)
                derv2(ly,iz) = derv2(ly,iz) - ff*qme*d2segweight(2,3,ll,kk,ipts)
                derv2(lz,iz) = derv2(lz,iz) - ff*qme*d2segweight(3,3,ll,kk,ipts)
              elseif (l.gt.i) then
                derv2(ix,lx) = derv2(ix,lx) - ff*qme*d2segweight(1,1,ll,kk,ipts)
                derv2(iy,lx) = derv2(iy,lx) - ff*qme*d2segweight(1,2,ll,kk,ipts)
                derv2(iz,lx) = derv2(iz,lx) - ff*qme*d2segweight(1,3,ll,kk,ipts)
                derv2(ix,ly) = derv2(ix,ly) - ff*qme*d2segweight(2,1,ll,kk,ipts)
                derv2(iy,ly) = derv2(iy,ly) - ff*qme*d2segweight(2,2,ll,kk,ipts)
                derv2(iz,ly) = derv2(iz,ly) - ff*qme*d2segweight(2,3,ll,kk,ipts)
                derv2(ix,lz) = derv2(ix,lz) - ff*qme*d2segweight(3,1,ll,kk,ipts)
                derv2(iy,lz) = derv2(iy,lz) - ff*qme*d2segweight(3,2,ll,kk,ipts)
                derv2(iz,lz) = derv2(iz,lz) - ff*qme*d2segweight(3,3,ll,kk,ipts)
              endif
            enddo
          enddo
        endif
      endif
    enddo
  enddo
!
  return
  end
