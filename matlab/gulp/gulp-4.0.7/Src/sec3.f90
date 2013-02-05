  subroutine sec3
!
!  Generate symmetry adapted second derivative matrix in
!  fractional units and strain from cartesian matrix for
!  the full unit cell.
!
!   5/95 Modifications now added for symmetry adapted second
!        derivative matrix.
!   8/95 Modification added to avoid use of transformation
!        unless necessary as this slows jobs down a lot.
!  11/96 Freezing removed as sec3f is a separate routine now
!   2/01 Modifications for general dimensionality added
!  10/03 Modified to handle cell parameter variables
!   1/04 Typos in rotation of breathing shells corrected
!   1/04 Modification of nuclear-radius cross terms
!        corrected in derv2 & radius-radius terms
!   1/04 Rotation of coordinate - radius block introduced
!        for symmetry adapted case
!  11/06 Cell parameter optimisation added
!  12/07 Arguments to setcellderv1D corrected
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use configurations, only : lbsmat
  use control
  use current
  use derivatives
  use iochannels
  use optimisation,   only : loptcellpar
  use parallel
  use symmetry
  use times
  use transform
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ibs
  integer(i4)                                  :: id
  integer(i4)                                  :: ii
  integer(i4)                                  :: indi
  integer(i4)                                  :: indii
  integer(i4)                                  :: indj
  integer(i4)                                  :: io
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: jd
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjx
  integer(i4)                                  :: jk
  integer(i4)                                  :: jo
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: l
  integer(i4)                                  :: n
  integer(i4)                                  :: n3a
  integer(i4)                                  :: n3f
  integer(i4)                                  :: ncellfull
  integer(i4)                                  :: neqj
  integer(i4)                                  :: nint
  integer(i4)                                  :: nloop
  integer(i4)                                  :: nofa
  integer(i4)                                  :: noff
  integer(i4)                                  :: status
  logical                                      :: lfullra
  logical                                      :: lsdebug
  logical                                      :: ltmat
  real(dp)                                     :: celldrv2(6,6)
  real(dp)                                     :: cputime
  real(dp)                                     :: d2(3,3)
  real(dp)                                     :: d2x
  real(dp)                                     :: d2y
  real(dp)                                     :: d2z
  real(dp)                                     :: d3tmp(6)
  real(dp)                                     :: dum
  real(dp)                                     :: dx
  real(dp)                                     :: dy
  real(dp)                                     :: dz
  real(dp)                                     :: r11
  real(dp)                                     :: r12
  real(dp)                                     :: r13
  real(dp)                                     :: r22
  real(dp)                                     :: r23
  real(dp)                                     :: r33
  real(dp)                                     :: r1xl
  real(dp)                                     :: r2yl
  real(dp)                                     :: r3zl
  real(dp)                                     :: rmat(3,3)
  real(dp)                                     :: sum
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp)                                     :: tmp1(6,6)
  real(dp), dimension(:), allocatable          :: tmp2
!
  t1 = cputime()
  nint = nvar - ncell
  lsdebug = (index(keyword,'derv2').ne.0)
  n3f = 3*numat
  n3a = 3*nasym
  noff = n3f
  nofa = n3a
  if (nbsm.gt.0) then
    n3f = n3f + numat
    n3a = n3a + nasym
  endif
!
!  Work out whether full tmat multiplication is needed - use faster method to handle P1 fully relaxed case
!
  if (ncon.eq.0.and.(nspcg(ncf).le.1.and.ngocfg(ncf).eq.1)) then
    ltmat = .false.
  elseif (nint.eq.0) then
    ltmat = .false.
  else
    ltmat = .true.
  endif
  if (lsymderv2) then
    nloop = nasym
  else
    nloop = numat
  endif
!
!  Set up cell scaling factors
!
  if (ndim.eq.3) then
    do i = 1,3
      rmat(i,1) = rv(i,1)
      rmat(i,2) = rv(i,2)
      rmat(i,3) = rv(i,3)
    enddo
  elseif (ndim.eq.2) then
    do i = 1,2
      rmat(i,1) = rv(i,1)
      rmat(i,2) = rv(i,2)
      rmat(i,3) = 0.0_dp
      rmat(3,i) = 0.0_dp
    enddo
    rmat(3,3) = 1.0_dp
  elseif (ndim.eq.1) then
    do i = 1,3
      rmat(i,1) = 0.0_dp
      rmat(i,2) = 0.0_dp
      rmat(i,3) = 0.0_dp
    enddo
    rmat(1,1) = rv(1,1)
    rmat(2,2) = 1.0_dp
    rmat(3,3) = 1.0_dp
  endif
!
!  Generate full cell vectors. Also if full cell
!  is right angled then use speed up tricks for
!  this special case (lfullra=.true.)
!
  if (ncbl.gt.1) then
    call uncentre(rmat)
    sum = abs(rv(1,2)) + abs(rv(2,1)) + abs(rv(1,3)) + abs(rv(3,1)) + abs(rv(2,3)) + abs(rv(3,2))
    lfullra = (sum.lt.1.0d-6)
  else
    lfullra = lra
  endif
!
!  Print out full second derivatives
!
  if (lsdebug.and.ioproc) then
    write(ioout,'(/,'' Second Derivative Matrix  :'',/)')
    do i = 1,n3f
      write(ioout,'(2x,9(f9.4))')(derv2(i,j),j=1,(3*nloop+nbsmat))
    enddo
    write(ioout,'(/)')
  endif
!***********************************************************************
!  Transform internal cartesian second derivatives to internal system  *
!  Needed in all algorithms                                            *
!***********************************************************************
  if (lfullra) then
    r1xl = rmat(1,1)
    r2yl = rmat(2,2)
    r3zl = rmat(3,3)
    r11 = r1xl*r1xl
    r12 = r1xl*r2yl
    r13 = r1xl*r3zl
    r22 = r2yl*r2yl
    r23 = r2yl*r3zl
    r33 = r3zl*r3zl
    do i = 1,nloop
      indi = 3*(i-1)
!
!  Coordinate
!
      do j = 1,numat
        indj = 3*(j-1)
        derv2(indj+1,indi+1) = r11*derv2(indj+1,indi+1)
        derv2(indj+1,indi+2) = r12*derv2(indj+1,indi+2)
        derv2(indj+1,indi+3) = r13*derv2(indj+1,indi+3)
        derv2(indj+2,indi+1) = r12*derv2(indj+2,indi+1)
        derv2(indj+2,indi+2) = r22*derv2(indj+2,indi+2)
        derv2(indj+2,indi+3) = r23*derv2(indj+2,indi+3)
        derv2(indj+3,indi+1) = r13*derv2(indj+3,indi+1)
        derv2(indj+3,indi+2) = r23*derv2(indj+3,indi+2)
        derv2(indj+3,indi+3) = r33*derv2(indj+3,indi+3)
      enddo
!
!  Radial
!
      if (lsymderv2) then
        ii = i
      else
        ii = nrelat(i)
      endif
      if (lbsmat(nsft+ii)) then
        do j = 1,numat
          derv2(noff+j,indi+1) = r1xl*derv2(noff+j,indi+1)
          derv2(noff+j,indi+2) = r2yl*derv2(noff+j,indi+2)
          derv2(noff+j,indi+3) = r3zl*derv2(noff+j,indi+3)
        enddo
        indii = 3*nloop + i
        do j = 1,numat
          jj = 3*(j-1)
          jx = jj + 1
          jy = jj + 2
          jz = jj + 3
          derv2(jx,indii) = r1xl*derv2(jx,indii)
          derv2(jy,indii) = r2yl*derv2(jy,indii)
          derv2(jz,indii) = r3zl*derv2(jz,indii)
        enddo
      endif
    enddo
  else
    if (lsymderv2) then
      do i = 1,nasym
        indi = 3*(i - 1)
!
!  Coordinates
!
        do j = 1,numat
          indj = 3*(j - 1)
          do ii = 1,3
            do jj = 1,3
              tmp1(jj,ii) = derv2(indj+1,indi+ii)*rmat(1,jj) + &
                            derv2(indj+2,indi+ii)*rmat(2,jj) + &
                            derv2(indj+3,indi+ii)*rmat(3,jj)
            enddo
          enddo
          do ii = 1,3
            do jj = 1,3
              derv2(indj+jj,indi+ii) = rmat(1,ii)*tmp1(jj,1) + &
                                       rmat(2,ii)*tmp1(jj,2) + &
                                       rmat(3,ii)*tmp1(jj,3)
            enddo
          enddo
        enddo
!
!  Radial
!
        if (lbsmat(nsft+i)) then
          do j = 1,numat
            dx = derv2(noff+j,indi+1)
            dy = derv2(noff+j,indi+2)
            dz = derv2(noff+j,indi+3)
            derv2(noff+j,indi+1) = dx*rmat(1,1) + dy*rmat(2,1) + dz*rmat(3,1)
            derv2(noff+j,indi+2) = dx*rmat(1,2) + dy*rmat(2,2) + dz*rmat(3,2)
            derv2(noff+j,indi+3) = dx*rmat(1,3) + dy*rmat(2,3) + dz*rmat(3,3)
          enddo
          indii = 3*nasym + i
          do j = 1,numat
            jj = 3*(j - 1)
            jx = jj + 1
            jy = jj + 2
            jz = jj + 3
            dx = derv2(jx,indii)
            dy = derv2(jy,indii)
            dz = derv2(jz,indii)
            derv2(jx,indii) = dx*rmat(1,1) + dy*rmat(2,1) + dz*rmat(3,1)
            derv2(jy,indii) = dx*rmat(1,2) + dy*rmat(2,2) + dz*rmat(3,2)
            derv2(jz,indii) = dx*rmat(1,3) + dy*rmat(2,3) + dz*rmat(3,3)
          enddo
        endif
      enddo
    else
      do i = 1,numat
        indi = 3*(i-1)
!
!  Coordinates
!
        do j = 1,i
          indj = 3*(j-1)
          do ii = 1,3
            do jj = 1,3
              tmp1(ii,jj) = derv2(indi+ii,indj+1)*rmat(1,jj) + &
                            derv2(indi+ii,indj+2)*rmat(2,jj) + &
                            derv2(indi+ii,indj+3)*rmat(3,jj)
            enddo
          enddo
          do ii = 1,3
            do jj = 1,3
              derv2(indi+ii,indj+jj) = rmat(1,ii)*tmp1(1,jj) + &
                                       rmat(2,ii)*tmp1(2,jj) + &
                                       rmat(3,ii)*tmp1(3,jj)
              derv2(indj+jj,indi+ii) = derv2(indi+ii,indj+jj)
            enddo
          enddo
        enddo
!
!  Radial
!
        if (lbsmat(nsft+nrelat(i))) then
          do j = 1,numat
            dx = derv2(noff+j,indi+1)
            dy = derv2(noff+j,indi+2)
            dz = derv2(noff+j,indi+3)
            derv2(noff+j,indi+1) = dx*rmat(1,1) + dy*rmat(2,1) + dz*rmat(3,1)
            derv2(noff+j,indi+2) = dx*rmat(1,2) + dy*rmat(2,2) + dz*rmat(3,2)
            derv2(noff+j,indi+3) = dx*rmat(1,3) + dy*rmat(2,3) + dz*rmat(3,3)
          enddo
          indii = 3*numat + i
          do j = 1,numat
            jj = 3*(j - 1)
            jx = jj + 1
            jy = jj + 2
            jz = jj + 3
            dx = derv2(jx,indii)
            dy = derv2(jy,indii)
            dz = derv2(jz,indii)
            derv2(jx,indii) = dx*rmat(1,1) + dy*rmat(2,1) + dz*rmat(3,1)
            derv2(jy,indii) = dx*rmat(1,2) + dy*rmat(2,2) + dz*rmat(3,2)
            derv2(jz,indii) = dx*rmat(1,3) + dy*rmat(2,3) + dz*rmat(3,3)
          enddo
        endif
      enddo
    endif
  endif
!******************************************
!                                         *
!  Internal derivatives :                 *
!                                         *
!  Symmetry transform second derivatives  *
!******************************************
  if (ltmat) then
    if (.not.lsymderv2) then
!**************************
!  Numat-numat algorithm  *
!**************************
      allocate(tmp2(n3f),stat=status)
      if (status/=0) call outofmemory('sec3','tmp2')
      do i = 1,nint
        do j = 1,n3f
          tmp2(j) = 0.0_dp
          do k = 1,n3f
            tmp2(j) = tmp2(j) + tmat(k,i)*derv2(j,k)
          enddo
        enddo
        do j = 1,nint
          derv2(j,i) = 0.0_dp
          do k = 1,n3f
            derv2(j,i) = derv2(j,i) + tmp2(k)*tmat(k,j)
          enddo
        enddo
      enddo
      deallocate(tmp2,stat=status)
      if (status/=0) call deallocate_error('sec3','tmp2')
    else
!**************************
!  Nasym-numat algorithm  *
!**************************
!
!  Reduce matrix to symmetry reduced full second derivative form
!
      do i = 1,nasym
        ix = 3*(i-1) + 1
        iy = ix + 1
        iz = ix + 2
        do j = 1,i
          jj = nrel2(j)
          jx = 3*(j-1) + 1
          jy = jx + 1
          jz = jx + 2
          neqj = neqv(j)
          jjx = 3*(jj-1)
          d2(1,1) = 0.0_dp
          d2(2,1) = 0.0_dp
          d2(3,1) = 0.0_dp
          d2(1,2) = 0.0_dp
          d2(2,2) = 0.0_dp
          d2(3,2) = 0.0_dp
          d2(1,3) = 0.0_dp
          d2(2,3) = 0.0_dp
          d2(3,3) = 0.0_dp
          do jk = 1,3*neqj
            d2(1,1) = d2(1,1) + derv2(jjx+jk,ix)*tmat(jjx+jk,ix)
            d2(2,1) = d2(2,1) + derv2(jjx+jk,iy)*tmat(jjx+jk,ix)
            d2(3,1) = d2(3,1) + derv2(jjx+jk,iz)*tmat(jjx+jk,ix)
            d2(1,2) = d2(1,2) + derv2(jjx+jk,ix)*tmat(jjx+jk,iy)
            d2(2,2) = d2(2,2) + derv2(jjx+jk,iy)*tmat(jjx+jk,iy)
            d2(3,2) = d2(3,2) + derv2(jjx+jk,iz)*tmat(jjx+jk,iy)
            d2(1,3) = d2(1,3) + derv2(jjx+jk,ix)*tmat(jjx+jk,iz)
            d2(2,3) = d2(2,3) + derv2(jjx+jk,iy)*tmat(jjx+jk,iz)
            d2(3,3) = d2(3,3) + derv2(jjx+jk,iz)*tmat(jjx+jk,iz)
          enddo
          derv2(jx,ix) = d2(1,1)
          derv2(jy,ix) = d2(1,2)
          derv2(jz,ix) = d2(1,3)
          derv2(jx,iy) = d2(2,1)
          derv2(jy,iy) = d2(2,2)
          derv2(jz,iy) = d2(2,3)
          derv2(jx,iz) = d2(3,1)
          derv2(jy,iz) = d2(3,2)
          derv2(jz,iz) = d2(3,3)
        enddo
!
!  Breathing shells - coordinate-radius
!
        if (nbsm.gt.0.and.lbsmat(nsft+i)) then
          ibs = nofa + i
          do j = 1,nasym
            jj = nrel2(j)
            neqj = neqv(j)
            d2x = 0.0_dp
            d2y = 0.0_dp
            d2z = 0.0_dp
            kk = 3*(jj-1)
            do k = 1,neqj
              io = nrotop(jj+k-1)
              d2x = d2x + derv2(1+kk,ibs)*rop(1,1,io) + derv2(2+kk,ibs)*rop(2,1,io) + derv2(3+kk,ibs)*rop(3,1,io)
              d2y = d2y + derv2(1+kk,ibs)*rop(1,2,io) + derv2(2+kk,ibs)*rop(2,2,io) + derv2(3+kk,ibs)*rop(3,2,io)
              d2z = d2z + derv2(1+kk,ibs)*rop(1,3,io) + derv2(2+kk,ibs)*rop(2,3,io) + derv2(3+kk,ibs)*rop(3,3,io)
              kk = kk + 3
            enddo
            indj = 3*(j-1)
            derv2(indj+1,ibs) = d2x
            derv2(indj+2,ibs) = d2y
            derv2(indj+3,ibs) = d2z
          enddo
        endif
      enddo
!
!  Breathing shells - radius-radius
!
      if (nbsm.gt.0) then
        do i = 1,nasym
          do j = 1,numat
            jj = nrelat(j)
            if (lbsmat(nsft+i).and.lbsmat(nsft+jj)) then
              if (j.eq.nrel2(jj)) then
                derv2(nofa+jj,nofa+i) = derv2(noff+j,nofa+i)
              else
                derv2(nofa+jj,nofa+i) = derv2(nofa+jj,nofa+i) + derv2(noff+j,nofa+i)
              endif
            else
              derv2(nofa+jj,nofa+i) = 0.0_dp
            endif
          enddo
        enddo
      endif
!
!  Symmetrise matrix
!
      do i = 2,n3a
        do j = 1,i-1
          derv2(i,j) = derv2(j,i)
        enddo
      enddo
!
!  Reduce to nvar x nvar form
!
      if (ncon.eq.0) then
        do i = 1,nint
          id = iopt(ncell+i) - nstrains
          do j = 1,nint
            jd = iopt(ncell+j) - nstrains
            derv2(j,i) = derv2(jd,id)
          enddo
        enddo
      else
        allocate(tmp2(nint),stat=status)
        if (status/=0) call outofmemory('sec3','tmp2')
        do i = 1,n3a
          do j = 1,nint
            tmp2(j) = 0.0_dp
            do k = 1,n3a
              tmp2(j) = tmp2(j) + tmat(k,n3a+j)*derv2(k,i)
            enddo
          enddo
          do j = 1,nint
            derv2(j,i) = tmp2(j)
          enddo
        enddo
        do i = 1,nint
          do j = 1,nint
            tmp2(j) = 0.0_dp
            do k = 1,n3a
              tmp2(j) = tmp2(j) + tmat(k,n3a+j)*derv2(i,k)
            enddo
          enddo
          do j = 1,nint
            derv2(i,j) = tmp2(j)
          enddo
        enddo
        deallocate(tmp2,stat=status)
        if (status/=0) call deallocate_error('sec3','tmp2')
      endif
    endif
  elseif (nint.eq.(n3f-3).and.iopt(ncell+1).eq.10) then
    do i = 1,nint
      do j = 1,nint
        derv2(j,i) = derv2(j+3,i+3)
      enddo
    enddo
  else
    do i = 1,nint
      ii = iopt(ncell+i) - nstrains
      do j = 1,nint
        jj = iopt(ncell+j) - nstrains
        derv2(j,i) = derv2(jj,ii)
      enddo
    enddo
  endif
!
!  Check size of derv2
!
  if (nint+ncell.gt.maxd2u) then
    maxd2u = nint + ncell
    call changemaxd2
  endif
  if (nint+ncell.gt.maxd2) then
    maxd2 = nint + ncell
    call changemaxd2
  endif
!
!  Shift position of second derivatives 
!
  if (ncell.gt.0) then
    do i = nint,1,-1
      do j = nint,1,-1
        derv2(j+ncell,i+ncell) = derv2(j,i)
      enddo
    enddo
  endif
!**********************
!  Mixed derivatives  *
!**********************
  if (lstr) then
!
!  Transform to fractional derivatives
!
    if (lfullra) then
      do jj = 1,nstrains
        do i = 1,nloop
          indi = 3*(i-1)
          derv3(indi+1,jj) = derv3(indi+1,jj)*r1xl
          derv3(indi+2,jj) = derv3(indi+2,jj)*r2yl
          derv3(indi+3,jj) = derv3(indi+3,jj)*r3zl
        enddo
      enddo
    else
      do i = 1,nloop
        indi = 3*(i-1)
        do ii = 1,3
          do jj = 1,nstrains
            tmp1(ii,jj) = derv3(indi+1,jj)*rmat(1,ii) + &
                          derv3(indi+2,jj)*rmat(2,ii) + &
                          derv3(indi+3,jj)*rmat(3,ii)
          enddo
        enddo
        do ii = 1,3
          do jj = 1,nstrains
            derv3(indi+ii,jj) = tmp1(ii,jj)
          enddo
        enddo
      enddo
    endif
    if (loptcellpar) then
!
!  Transform to cell parameter derivatives
!
      if (ndim.eq.3) then
        call setcellderv3D(.true.,.false.,.true.)
      elseif (ndim.eq.2) then
        call setcellderv2D(.true.,.false.,.true.)
      elseif (ndim.eq.1) then
        call setcellderv1D(.true.,.true.)
      endif
      do n = 1,nint
        do i = 1,nstrains
          d3tmp(i) = derv3(n,i)
          derv3(n,i) = 0.0_dp
        enddo
        do i = 1,nstrains
          do j = 1,nstrains
            derv3(n,j) = derv3(n,j) + d3tmp(i)*cderv(j,i)
          enddo    
        enddo
      enddo
    endif
!*****************************************
!  Symmetry transform mixed derivatives  *
!*****************************************
    if (ltmat) then
      if (.not.lsymderv2) then
        allocate(tmp2(n3f),stat=status)
        if (status/=0) call outofmemory('sec3','tmp2')
        do i = 1,nstrains
          do j = 1,n3f
            tmp2(j) = derv3(j,i)
          enddo
          do j = 1,nint
            derv3(j,i) = 0.0_dp
            do k = 1,n3f
              derv3(j,i) = derv3(j,i) + tmat(k,j)*tmp2(k)
            enddo
          enddo
        enddo
        deallocate(tmp2,stat=status)
        if (status/=0) call deallocate_error('sec3','tmp2')
      else
        if (ncon.eq.0) then
          do i = 1,nstrains
            do j = 1,nint
              jd = iopt(ncell+j) - nstrains
              derv3(j,i) = derv3(jd,i)
            enddo
          enddo
        else
          allocate(tmp2(nint),stat=status)
          if (status/=0) call outofmemory('sec3','tmp2')
          do i = 1,nstrains
            do j = 1,nint
              tmp2(j) = 0.0_dp
              do k = 1,n3a
                tmp2(j) = tmp2(j) + tmat(k,n3a+j)*derv3(k,i)
              enddo
            enddo
            do j = 1,nint
              derv3(j,i) = tmp2(j)
            enddo
          enddo
          deallocate(tmp2,stat=status)
          if (status/=0) call deallocate_error('sec3','tmp2')
        endif
      endif
    elseif (nint.eq.(n3f-3).and.iopt(ncell+1).eq.10) then
      do i = 1,nstrains
        do j = 1,nint
          derv3(j,i) = derv3(j+3,i)
        enddo
      enddo
    else
      do i = 1,nstrains
        do j = 1,nint
          jd = iopt(ncell+j) - nstrains
          derv3(j,i) = derv3(jd,i)
        enddo
      enddo
    endif
!
!  Reduce derv3 by strain reduction matrix
!
    allocate(tmp2(nint),stat=status)
    if (status/=0) call outofmemory('sec3','tmp2')
    do i = 1,ncell
      do j = 1,nint
        tmp2(j) = 0.0_dp
        do k = 1,nstrains
          tmp2(j) = tmp2(j) + stmat(k,i)*derv3(j,k)
        enddo
      enddo
      do j = 1,nint
        derv3(j,i) = tmp2(j)
      enddo
    enddo
    deallocate(tmp2,stat=status)
    if (status/=0) call deallocate_error('sec3','tmp2')
!
!  Move into place
!
    do i = 1,ncell
      do j = 1,nint
        derv2(ncell+j,i) = derv3(j,i)
        derv2(i,j+ncell) = derv3(j,i)
      enddo
    enddo
!******************************
!  Strain second derivatives  *
!******************************
    if (loptcellpar) then
      if (ndim.eq.3) then
        ncellfull = 6
      elseif (ndim.eq.2) then
        ncellfull = 3
      else
        ncellfull = 1
      endif
!
!  Convert strain second derivatives to cell parameter second derivatives
!
     celldrv2(1:ncellfull,1:ncellfull) = 0.0_dp
      do i = 1,ncellfull
        do j = 1,ncellfull
          do k = 1,nstrains
            do l = 1,nstrains
              celldrv2(j,i) = celldrv2(j,i) + sdrv2(l,k)*cderv(j,l)*cderv(i,k)
            enddo
          enddo
        enddo
      enddo
      sdrv2(1:ncellfull,1:ncellfull) = celldrv2(1:ncellfull,1:ncellfull)
    endif
    if (ncon.eq.0) then
      do i = 1,ncell
        io = iopt(i)
        do j = 1,ncell
          jo = iopt(j)
          dum = sdrv2(jo,io)
          derv2(j,i) = dum
          derv2(i,j) = dum
        enddo
      enddo
    else
      do i = 1,ncell
        do j = 1,nstrains
          tmp1(j,i) = 0.0_dp
          do k = 1,nstrains
            tmp1(j,i) = tmp1(j,i) + sdrv2(j,k)*stmat(k,i)
          enddo
        enddo
      enddo
      do i = 1,ncell
        do j = 1,ncell
          derv2(j,i) = 0.0_dp
          do k = 1,nstrains
            derv2(j,i) = derv2(j,i) + tmp1(k,j)*stmat(k,i)
          enddo
        enddo
      enddo
    endif
  endif
  if (lsdebug.and.ioproc) then
    write(ioout,'(/,''  Symmetrised Second Derivative Matrix  :'',/)')
    do i = 1,nvar
      write(ioout,'(2x,10(f9.2))')(derv2(j,i),j=1,i)
    enddo
    write(ioout,'(/)')
  endif
!
  t2 = cputime()
  thes = thes + t2 - t1
!
  return
  end
