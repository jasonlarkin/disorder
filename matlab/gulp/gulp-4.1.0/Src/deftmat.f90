  subroutine deftmat
!
!  Create transformation matrix which maps full set of defect
!  cartesian variables onto constrained asymmetric unit set
!
!  Matrix is stored as : tmat(3*nreg1,3*ndasym)
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use control
  use current
  use defects
  use derivatives
  use iochannels
  use parallel
  use symmetry
  use times
  use transform
  implicit none
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: ii
  integer(i4)                               :: iio
  integer(i4)                               :: indi
  integer(i4)                               :: indj
  integer(i4)                               :: indjk
  integer(i4)                               :: io
  integer(i4)                               :: ix
  integer(i4)                               :: iy
  integer(i4)                               :: iz
  integer(i4)                               :: j
  integer(i4)                               :: jj
  integer(i4)                               :: jk
  integer(i4)                               :: jkx
  integer(i4)                               :: jky
  integer(i4)                               :: jkz
  integer(i4)                               :: jo
  integer(i4)                               :: jx
  integer(i4)                               :: jy
  integer(i4)                               :: jz
  integer(i4)                               :: k
  integer(i4)                               :: ki
  integer(i4)                               :: kj
  integer(i4)                               :: l
  integer(i4)                               :: lo
  integer(i4)                               :: mv
  integer(i4)                               :: n3a
  integer(i4)                               :: n3f
  integer(i4)                               :: neqi
  integer(i4)                               :: neqj
  integer(i4)                               :: nofa
  integer(i4)                               :: noff
  integer(i4)                               :: status
  logical                                   :: lfound
  logical                                   :: lint
  real(dp), dimension(:), allocatable       :: cmat
  real(dp)                                  :: diff
  real(dp)                                  :: difx
  real(dp)                                  :: dify
  real(dp)                                  :: difz
  real(dp)                                  :: rri
  real(dp)                                  :: xj
  real(dp)                                  :: yj
  real(dp)                                  :: zj
  real(dp)                                  :: xjj
  real(dp)                                  :: yjj
  real(dp)                                  :: zjj
!
  n3f = 3*nreg1
  n3a = 3*ndasym
  noff = n3f
  nofa = n3a
  if (ldbsm) then
    n3f = n3f + nreg1
    n3a = n3a + ndasym
  endif
  lint = (nvar.gt.0)
  if (ndcon.eq.0.and.nvar.eq.n3f) return
!
!  Check size of tmat
!
  if (n3a+nvar.gt.maxn3a) then
    maxn3a = n3a + nvar
    call changemaxn3a
  endif
  if (n3f.gt.maxn3f) then
    maxn3f = n3f
    call changemaxn3f
  endif
!
!  Allocate local memory
!
  allocate(cmat(n3a),stat=status)
  if (status/=0) call outofmemory('deftmat','cmat')
!
  if (lint.and..not.ld2sym) then
!
!  Check sizes for derv2
!
    if (n3a.gt.maxd2u) then
      maxd2u = n3a
      call changemaxd2
    endif
    if (n3f.gt.maxd2) then
      maxd2 = n3f
      call changemaxd2
    endif
!
!  Zero arrays
!
    do i = 1,n3a
      do j = 1,n3f
        derv2(j,i) = 0.0_dp
      enddo
    enddo
!************************************
!  Collect transformation elements  *
!************************************
!
!  Coordinates
!
    do i = 1,ndasym
      ix = 3*(i-1) + 1
      iy = ix + 1
      iz = ix + 2
      do j = 1,nreg1
        if (ndrel(j).eq.i) then
          jx = 3*(j-1)+1
          jy = jx+1
          jz = jx+2
          mv = ndrelop(j)
          derv2(jx,ix) = dsymop(1,1,mv)
          derv2(jy,ix) = dsymop(2,1,mv)
          derv2(jz,ix) = dsymop(3,1,mv)
          derv2(jx,iy) = dsymop(1,2,mv)
          derv2(jy,iy) = dsymop(2,2,mv)
          derv2(jz,iy) = dsymop(3,2,mv)
          derv2(jx,iz) = dsymop(1,3,mv)
          derv2(jy,iz) = dsymop(2,3,mv)
          derv2(jz,iz) = dsymop(3,3,mv)
        endif
      enddo
    enddo
!
!  Radii
!
    if (ldbsm) then
      do i = 1,ndasym
        ii = ndsptr(i)
        if (ldefbsmat(ii)) then
          do j = 1,nreg1
            if (ndrel(j).eq.i) then
              derv2(noff+j,nofa+i) = 1.0_dp
            endif
          enddo
        endif
      enddo
    endif
!*********************************
!  Loop over internal variables  *
!*********************************
    do i = 1,nvar
      do j = 1,n3a
        cmat(j) = 0.0_dp
      enddo
!
!  Variable mapping matrix
!
      io = idopt(i)
      cmat(io) = 1.0_dp
!
!  Apply constraints
!
      if (ndcon.gt.0) then
        do k = 1,ndcon
          ii = ncdvar(k)
          io = ncdfix(k)
          if (idopt(i).eq.ii) cmat(io) = dconco(k)
        enddo
      endif
!
!  Combine transformation matrices
!
      do j = 1,n3f
        tmat(j,i) = 0.0_dp
        do k = 1,n3a
          tmat(j,i) = tmat(j,i)+derv2(j,k)*cmat(k)
        enddo
      enddo
    enddo
    if (index(keyword,'tmat').ne.0.and.ioproc) then
      write(ioout,'(/,''  Transformation matrix :'',/)')
      do i = 1,n3f
        write(ioout,'(16f5.1)')(tmat(i,j),j = 1,nvar)
      enddo
      write(ioout,'(/)')
    endif
  elseif (lint) then
!********************
!  New test method  *
!********************
!
!  Zero tmat
!
    do i = 1,n3a
      do j = 1,n3f
        tmat(j,i) = 0.0_dp
      enddo
    enddo
    ki = 0
!
!  Loop over asymmetric unit site
!
    do i = 1,ndasym
      indi = 3*(i-1)
      ix = indi+1
      iy = indi+2
      iz = indi+3
      neqi = ndeqv(i)
      rri = 1.0_dp/neqi
      kj = 0
      do j = 1,ndasym
        indj = 3*(j-1)
        jx = indj+1
        jy = indj+2
        jz = indj+3
        neqj = ndeqv(j)
        ii = ki
        do k = 1,neqi
          ii = ii + 1
          io = ndrelop(ii)
          iio = inverse(io)
          jj = kj
          do l = 1,neqj
            jj = jj + 1
            jo = ndrelop(jj)
            xj = xdefe(jj) - xdc
            yj = ydefe(jj) - ydc
            zj = zdefe(jj) - zdc
!
!  Transformation element
!  Inverse of io * jo
!
            lo = iptab(iio,jo)
!
!  Find equivalent derv2 matrix element in asymmetric unit
!
            lfound = .false.
            jk = 0
            do while (jk.lt.neqj.and..not.lfound)
              jk = jk + 1
              lfound = (ndrelop(jk+kj).eq.lo)
            enddo
            if (.not.lfound) then
              jk = 0
              xjj = xj*dsymop(1,1,iio) + yj*dsymop(1,2,iio) + zj*dsymop(1,3,iio) + xdc
              yjj = xj*dsymop(2,1,iio) + yj*dsymop(2,2,iio) + zj*dsymop(2,3,iio) + ydc
              zjj = xj*dsymop(3,1,iio) + yj*dsymop(3,2,iio) + zj*dsymop(3,3,iio) + zdc
              do while (jk.lt.neqj.and..not.lfound)
                jk = jk + 1
                difx = xjj - xdefe(jk+kj)
                dify = yjj - ydefe(jk+kj)
                difz = zjj - zdefe(jk+kj)
                diff = difx*difx + dify*dify + difz*difz
                lfound = (diff.lt.1.0d-8)
              enddo
            endif
            if (.not.lfound) then
              call outerror('symmetry atom not found in deftmat',0_i4)
              call stopnow('deftmat')
            endif
            indjk = 3*(jk + kj - 1)
            jkx = indjk + 1
            jky = indjk + 2
            jkz = indjk + 3
!
!  Add term to tmat
!
            tmat(jkx,ix) = tmat(jkx,ix) + dsymop(1,1,lo)*rri
            tmat(jky,ix) = tmat(jky,ix) + dsymop(2,1,lo)*rri
            tmat(jkz,ix) = tmat(jkz,ix) + dsymop(3,1,lo)*rri
            tmat(jkx,iy) = tmat(jkx,iy) + dsymop(1,2,lo)*rri
            tmat(jky,iy) = tmat(jky,iy) + dsymop(2,2,lo)*rri
            tmat(jkz,iy) = tmat(jkz,iy) + dsymop(3,2,lo)*rri
            tmat(jkx,iz) = tmat(jkx,iz) + dsymop(1,3,lo)*rri
            tmat(jky,iz) = tmat(jky,iz) + dsymop(2,3,lo)*rri
            tmat(jkz,iz) = tmat(jkz,iz) + dsymop(3,3,lo)*rri
          enddo
        enddo
        kj = kj + neqj
      enddo
      ki = ki + neqi
    enddo
    do i = 1,nvar
      do j = 1,n3a
        tmat(j,i+n3a) = 0.0_dp
      enddo
    enddo
    if (index(keyword,'tmat').ne.0.and.ioproc) then
      write(ioout,'(/,''  Transformation matrix (to asymmetric unit) :'',/)')
      do i = 1,n3f
        write(ioout,'(16f5.1)')(tmat(i,j),j = 1,n3a)
      enddo
      write(ioout,'(/)')
    endif
!*********************************
!  Loop over internal variables  *
!*********************************
    do i = 1,nvar
      do j = 1,n3a
        cmat(j) = 0.0_dp
      enddo
!
!  Variable mapping matrix
!
      io = idopt(i)
      cmat(io) = 1.0_dp
!
!  Apply constraints
!
      if (ndcon.gt.0) then
        do k = 1,ndcon
          ii = ncdvar(k)
          io = ncdfix(k)
          if (idopt(i).eq.ii) cmat(io) = dconco(k)
        enddo
      endif
      do j = 1,n3a
        tmat(j,i+n3a) = cmat(j)
      enddo
    enddo
    if (index(keyword,'tmat').ne.0.and.ioproc) then
      write(ioout,'(/,''  Transformation matrix (to variables) :'',/)')
      do i = 1,n3a
        write(ioout,'(16f5.1)')(tmat(i,j+n3a),j = 1,nvar)
      enddo
      write(ioout,'(/)')
    endif
  endif
!
!  Free local memory
!
  deallocate(cmat,stat=status)
  if (status/=0) call deallocate_error('deftmat','cmat')
!
  return
  end
