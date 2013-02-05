  subroutine sec3f
!
!  Generate symmetry adapted second derivative matrix in
!  fractional units and strain from cartesian matrix for
!  the non-frozen atoms. Assumes second derivatives have
!  been generated in a packed form. This method only
!  applies to constant volume calculations so this has
!  been assumed.
!
!  Freezing now added.
!
!   5/95 Modifications now added for symmetry adapted second
!        derivative matrix.
!   8/95 Modification added to avoid use of transformation
!        unless necessary as this slows jobs down a lot.
!  11/97 Bug in i loop upper bound for .not.lfullra / lsymderv2
!        case fixed
!   2/01 Modifications for general dimensionality added
!   1/04 Typos in rotation of breathing shells corrected
!   1/04 Modification of nuclear-radius cross terms
!        corrected in derv2 & radius-radius
!   1/04 Rotation of coordinate - radius block introduced
!        for symmetry adapted case
!   7/05 Stack cleaned up
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
  use optimisation
  use parallel
  use symmetry
  use times
  use transform
  implicit none
!
!  Local variables
!
  integer(i4), dimension(:), allocatable       :: ioptf
  integer(i4), dimension(:), allocatable       :: npa
  integer(i4), dimension(:), allocatable       :: npf
  integer(i4), dimension(:), allocatable       :: nptr
  integer(i4)                                  :: i
  integer(i4)                                  :: ibs
  integer(i4)                                  :: id
  integer(i4)                                  :: ii
  integer(i4)                                  :: iii
  integer(i4)                                  :: indi
  integer(i4)                                  :: indii
  integer(i4)                                  :: indj
  integer(i4)                                  :: io
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: ja
  integer(i4)                                  :: jd
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjlast
  integer(i4)                                  :: jjx
  integer(i4)                                  :: jk
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: n3a
  integer(i4)                                  :: n3f
  integer(i4)                                  :: neqj
  integer(i4)                                  :: nfa
  integer(i4)                                  :: nff
  integer(i4)                                  :: nloop
  integer(i4)                                  :: nofa
  integer(i4)                                  :: noff
  integer(i4)                                  :: status
  logical                                      :: lfullra
  logical                                      :: lsdebug
  logical                                      :: ltmat
  real(dp)                                     :: cputime
  real(dp)                                     :: d2(3,3)
  real(dp)                                     :: d2x
  real(dp)                                     :: d2y
  real(dp)                                     :: d2z
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
  real(dp),    dimension(:), allocatable, save :: tmp2
!
  t1 = cputime()
  lsdebug = (index(keyword,'derv2').ne.0)
!
!  Allocate local memory
!
  allocate(ioptf(nvar),stat=status)
  if (status/=0) call outofmemory('sec3f','ioptf')
  allocate(npa(nasym),stat=status)
  if (status/=0) call outofmemory('sec3f','npa')
  allocate(npf(numat),stat=status)
  if (status/=0) call outofmemory('sec3f','npf')
  allocate(nptr(nasym),stat=status)
  if (status/=0) call outofmemory('sec3f','nptr')
!
!  Find unfrozen atoms
!
  nfa = 0
  nff = 0
  do i = 1,nasym
    if (lopf(i)) then
      nfa = nfa + 1
      npa(nfa) = i
      do j = 1,neqv(i)
        nff = nff + 1
        npf(nff) = i
      enddo
    endif
  enddo
  ii = 0
  do i = 1,nasym
    if (lopf(i)) then
      ii = ii + 1
      nptr(i) = ii
    else
      nptr(i) = 0
    endif
  enddo
!
!  Convert iopt values to new reference system of atoms
!
  do i = 1,nvar
    ii = iopt(i) - (nstrains+1)
    if (ii+1.gt.3*nasym) then
!
!  Breathing shell
!
      iii = ii + 1 - 3*nasym
      ioptf(i) = 3*nfa + nptr(iii)
    else
!
!  Coordinate
!
      iii = ii/3
      ii = ii - 3*iii + 1
      iii = iii + 1
      ioptf(i) = 3*(nptr(iii)-1) + ii
    endif
  enddo
  n3a = 3*nfa
  n3f = 3*nff
  nofa = n3a
  noff = n3f
  if (nbsm.gt.0) then
    n3a = n3a + nfa
    n3f = n3f + nff
  endif
!
!  Work out whether full tmat multiplication is needed - use
!  faster method to handle P1 fully relaxed case
!
  if (ncon.eq.0.and.(nspcg(ncf).le.1.and.ngocfg(ncf).eq.1)) then
    ltmat = .false.
  else
    ltmat = .true.
  endif
  if (lsymderv2) then
    nloop = nfa
  else
    nloop = nff
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
!  Generate full cell vectors. Also if full cell is right angled then use speed up tricks for
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
      write(ioout,'(2x,10(f9.2))')(derv2(i,j),j=1,n3a)
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
    indi = - 3
    do i = 1,nloop
      indi = indi + 3
!
!  Coordinate
!
      indj = - 3
      do j = 1,nff
        indj = indj + 3
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
        if (lbsmat(nsft+npa(i))) then
          do j = 1,nff
            derv2(noff+j,indi+1) = r1xl*derv2(noff+j,indi+1)
            derv2(noff+j,indi+2) = r2yl*derv2(noff+j,indi+2)
            derv2(noff+j,indi+3) = r3zl*derv2(noff+j,indi+3)
          enddo
          indii = 3*nloop + i
          jj = - 3
          do j = 1,nff
            jj = jj + 3
            jx = jj + 1
            jy = jj + 2
            jz = jj + 3
            derv2(jx,indii) = r1xl*derv2(jx,indii)
            derv2(jy,indii) = r2yl*derv2(jy,indii)
            derv2(jz,indii) = r3zl*derv2(jz,indii)
          enddo
        endif
      else
        if (lbsmat(nsft+npf(i))) then
          do j = 1,nff
            derv2(noff+j,indi+1) = r1xl*derv2(noff+j,indi+1)
            derv2(noff+j,indi+2) = r2yl*derv2(noff+j,indi+2)
            derv2(noff+j,indi+3) = r3zl*derv2(noff+j,indi+3)
          enddo
          indii = 3*nloop + i
          jj = - 3
          do j = 1,nff
            jj = jj + 3
            jx = jj + 1
            jy = jj + 2
            jz = jj + 3
            derv2(jx,indii) = r1xl*derv2(jx,indii)
            derv2(jy,indii) = r2yl*derv2(jy,indii)
            derv2(jz,indii) = r3zl*derv2(jz,indii)
          enddo
        endif
      endif
    enddo
  else
    if (lsymderv2) then
      indi = - 3
      do i = 1,nfa
        indi = indi + 3
!
!  Coordinates
!
        indj = - 3
        do j = 1,nff
          indj = indj + 3
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
        if (lbsmat(nsft+npa(i))) then
          do j = 1,nff
            dx = derv2(noff+j,indi+1)
            dy = derv2(noff+j,indi+2)
            dz = derv2(noff+j,indi+3)
            derv2(noff+j,indi+1) = dx*rmat(1,1) + dy*rmat(2,1) + dz*rmat(3,1)
            derv2(noff+j,indi+2) = dx*rmat(1,2) + dy*rmat(2,2) + dz*rmat(3,2)
            derv2(noff+j,indi+3) = dx*rmat(1,3) + dy*rmat(2,3) + dz*rmat(3,3)
          enddo
          indii = 3*nfa + i
          jj = - 3
          do j = 1,nff
            jj = jj + 3
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
      indi = - 3
      do i = 1,nff
        indi = indi + 3
!
!  Coordinates
!
        indj = - 3
        do j = 1,i
          indj = indj + 3
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
        if (lbsmat(nsft+nrelat(npf(i)))) then
          do j = 1,nff
            dx = derv2(noff+j,indi+1)
            dy = derv2(noff+j,indi+2)
            dz = derv2(noff+j,indi+3)
            derv2(noff+j,indi+1) = dx*rmat(1,1) + dy*rmat(2,1) + dz*rmat(3,1)
            derv2(noff+j,indi+2) = dx*rmat(1,2) + dy*rmat(2,2) + dz*rmat(3,2)
            derv2(noff+j,indi+3) = dx*rmat(1,3) + dy*rmat(2,3) + dz*rmat(3,3)
          enddo
          indii = 3*nff + i
          jj = - 3
          do j = 1,nff
            jj = jj + 3
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
      if (status/=0) call outofmemory('sec3f','tmp2')
      do i = 1,nvar
        do j = 1,n3f
          tmp2(j) = 0.0_dp
          do k = 1,n3f
            tmp2(j) = tmp2(j) + tmat(k,i)*derv2(j,k)
          enddo
        enddo
        do j = 1,nvar
          derv2(j,i) = 0.0_dp
          do k = 1,n3f
            derv2(j,i) = derv2(j,i) + tmp2(k)*tmat(k,j)
          enddo
        enddo
      enddo
      deallocate(tmp2,stat=status)
      if (status/=0) call deallocate_error('sec3f','tmp2')
    else
!**************************
!  Nasym-numat algorithm  *
!**************************
!
!  Reduce matrix to symmetry reduced full second derivative form
!
      ix = - 2
      iy = - 1
      iz =   0
      do i = 1,nfa
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        jx = - 2
        jy = - 1
        jz =   0
        jj =   0
        do j = 1,i
          jx = jx + 3
          jy = jy + 3
          jz = jz + 3
          jjx = 3*jj
          neqj = neqv(npa(j))
          jj = jj + neqj
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
        if (lbsmat(nsft+npa(i))) then
          ibs = nofa + i
          jj = 0
          do j = 1,nfa
            ja = npa(j)
            kk = 3*jj
            neqj = neqv(ja)
            d2x = 0.0_dp
            d2y = 0.0_dp
            d2z = 0.0_dp
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
            jj = jj + neqj
          enddo
        endif
      enddo
!
!  Breathing shells - radius-radius
!
      if (nbsm.gt.0) then
        jjlast = 0
        do i = 1,nfa
          do j = 1,nff
            jj = npf(j)
            if (lbsmat(nsft+i).and.lbsmat(nsft+jj)) then
              if (jj.ne.jjlast) then
                derv2(nofa+jj,nofa+i) = derv2(noff+j,nofa+i)
              else      
                derv2(nofa+jj,nofa+i) = derv2(nofa+jj,nofa+i) + derv2(noff+j,nofa+i)
              endif     
            else
              derv2(nofa+jj,nofa+i) = 0.0_dp
            endif
            jjlast = jj
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
        do i = 1,nvar
          id = ioptf(i)
          do j = 1,nvar
            jd = ioptf(j)
            derv2(j,i) = derv2(jd,id)
          enddo
        enddo
      else
        allocate(tmp2(nvar),stat=status)
        if (status/=0) call outofmemory('sec3f','tmp2')
        do i = 1,n3a
          do j = 1,nvar
            tmp2(j) = 0.0_dp
            do k = 1,n3a
              tmp2(j) = tmp2(j) + tmat(k,n3a+j)*derv2(k,i)
            enddo
          enddo
          do j = 1,nvar
            derv2(j,i) = tmp2(j)
          enddo
        enddo
        do i = 1,nvar
          do j = 1,nvar
            tmp2(j) = 0.0_dp
            do k = 1,n3a
              tmp2(j) = tmp2(j) + tmat(k,n3a+j)*derv2(i,k)
            enddo
          enddo
          do j = 1,nvar
            derv2(i,j) = tmp2(j)
          enddo
        enddo
        deallocate(tmp2,stat=status)
        if (status/=0) call deallocate_error('sec3f','tmp2')
      endif
    endif
  elseif (nvar.ne.n3f) then
    do i = 1,nvar
      ii = ioptf(i)
      do j = 1,nvar
        jj = ioptf(j)
        derv2(j,i) = derv2(jj,ii)
      enddo
    enddo
  endif
  if (lsdebug.and.ioproc) then
    write(ioout,'(/,''  Symmetrised Second Derivative Matrix  :'',/)')
    do i = 1,nvar
      write(ioout,'(2x,10(f9.2))')(derv2(j,i),j=1,i)
    enddo
    write(ioout,'(/)')
  endif
!
!  Free local memory
!
  deallocate(nptr,stat=status)
  if (status/=0) call deallocate_error('sec3f','nptr')
  deallocate(npf,stat=status)
  if (status/=0) call deallocate_error('sec3f','npf')
  deallocate(npa,stat=status)
  if (status/=0) call deallocate_error('sec3f','npa')
  deallocate(ioptf,stat=status)
  if (status/=0) call deallocate_error('sec3f','ioptf')
!
!  Timing
!
  t2 = cputime()
  thes = thes + t2 - t1
  return
  end
