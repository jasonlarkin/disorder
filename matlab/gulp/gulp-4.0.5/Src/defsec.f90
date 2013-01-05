  subroutine defsec
!
!  Generates variable reduced second derivative matrix
!
!  Can work in one of two modes:
!
!  (a) Reduce symmetry generated derv2(3*nreg1,3*ndasym) 
!  (b) Full nosymmetry generated derv2(3*nreg1,3*nreg1)
!
!  Both reduced to derv2(nvar,nvar)
!
!  11/07 Unused variables removed
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, November 2007
!
  use control
  use current
  use defects
  use derivatives
  use general
  use iochannels
  use parallel
  use times
  use transform
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: id
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjx
  integer(i4)                                  :: jk
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: jd
  integer(i4)                                  :: k
  integer(i4)                                  :: maxlim
  integer(i4)                                  :: maxlims
  integer(i4)                                  :: neqj
  integer(i4)                                  :: status
  real(dp),    dimension(:), allocatable       :: tmpx
  real(dp)                                     :: cputime
  real(dp)                                     :: d2(3,3)
  real(dp)                                     :: t1
  real(dp)                                     :: t2
!
  t1 = cputime()
  maxlim = 3*nreg1
  if (ldbsm) maxlim = maxlim + nreg1
  if (index(keyword,'derv2').ne.0.and.ioproc) then
    write(ioout,'(/,''  Second derivative matrix before reduction : (eV/Angstrom**2)'',/)')
    if (ld2sym) then
      maxlims = 3*ndasym
      if (ldbsm) maxlims = maxlims + ndasym
      do i = 1,maxlim + 3
        write(ioout,'(9f10.4)')(derv2(i,j),j=1,maxlims)
      enddo
    else
      do i = 1,maxlim+3
        write(ioout,'(9f10.4)')(derv2(j,i),j=1,maxlim+3)
      enddo
    endif
    write(ioout,'(/)')
  endif
!
!  If no reduction is needed then return
!
  if (nvar.eq.maxlim) goto 10
!
!  Check size of second derivative arrays
!
  if (maxlim + nvar.gt.maxd2u) then
    maxd2u = maxlim + nvar
    call changemaxd2
  endif
  if (maxlim.gt.maxd2) then
    maxd2 = maxlim
    call changemaxd2
  endif
!
  if (.not.ldsym) then
!************************************
!  Reduce second derivative matrix  *
!************************************
    do i = 1,nvar
      id = idopt(i)
      do j = 1,nvar
        jd = idopt(j)
        derv2(j,i) = derv2(jd,id)
      enddo
    enddo
  else
    allocate(tmpx(maxlim),stat=status)
    if (status/=0) call outofmemory('defsec','tmpx')
    if (ld2sym) then
!****************************
!  Symmetry adapted method  *
!****************************
!
!  Reduce matrix to symmetry reduced full second derivative form
!
      do i = 1,ndasym
        ix = 3*(i - 1) + 1
        iy = ix + 1
        iz = ix + 2
        do j = 1,ndasym
          jj = ndsptr(j)
          jx = 3*(j - 1) + 1
          jy = jx + 1
          jz = jx + 2
          neqj = ndeqv(j)
          jjx = 3*(jj - 1)
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
            d2(1,1) = d2(1,1) + derv2(jjx + jk,ix)*tmat(jjx + jk,ix)
            d2(2,1) = d2(2,1) + derv2(jjx + jk,iy)*tmat(jjx + jk,ix)
            d2(3,1) = d2(3,1) + derv2(jjx + jk,iz)*tmat(jjx + jk,ix)
            d2(1,2) = d2(1,2) + derv2(jjx + jk,ix)*tmat(jjx + jk,iy)
            d2(2,2) = d2(2,2) + derv2(jjx + jk,iy)*tmat(jjx + jk,iy)
            d2(3,2) = d2(3,2) + derv2(jjx + jk,iz)*tmat(jjx + jk,iy)
            d2(1,3) = d2(1,3) + derv2(jjx + jk,ix)*tmat(jjx + jk,iz)
            d2(2,3) = d2(2,3) + derv2(jjx + jk,iy)*tmat(jjx + jk,iz)
            d2(3,3) = d2(3,3) + derv2(jjx + jk,iz)*tmat(jjx + jk,iz)
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
      enddo
!
!  Symmetrise matrix
!
      maxlim = 3*ndasym
!
!  Reduce to nvar x nvar form
!
      if (ndcon.eq.0) then
        do i = 1,nvar
          id = idopt(i)
          do j = 1,nvar
            jd = idopt(j)
            derv2(j,i) = derv2(jd,id)
          enddo
        enddo
      else
        if ((nvar + maxlim).le.maxd2) then
          do i = 1,nvar
            do j = 1,maxlim
              derv2(j,maxlim + i) = 0.0_dp
              do k = 1,maxlim
                derv2(j,maxlim + i) = derv2(j,maxlim + i) + tmat(k,maxlim + i)*derv2(k,j)
              enddo
            enddo
          enddo
          do i = 1,nvar
            do j = 1,nvar
              derv2(j,i) = 0.0_dp
              do k = 1,maxlim
                derv2(j,i) = derv2(j,i) + tmat(k,maxlim + j)*derv2(k,maxlim + i)
              enddo
            enddo
          enddo
        else
          do i = 1,nvar
            do j = 1,maxlim
              tmpx(j) = 0.0_dp
              do k = 1,maxlim
                tmpx(j) = tmpx(j) + tmat(k,maxlim + i)*derv2(j,k)
              enddo
            enddo
            do j = 1,nvar
              derv2(j,i) = 0.0_dp
              do k = 1,maxlim
                derv2(j,i) = derv2(j,i) + tmpx(k)*tmat(k,maxlim + j)
              enddo
            enddo
          enddo
        endif
      endif
    else
!***********************
!  No symmetry method  *
!***********************
      if ((2*nvar).le.maxd2) then
        do i = 1,nvar
          do j = 1,maxlim
            tmat(j,nvar + i) = 0.0_dp
            do k = 1,maxlim
              tmat(j,nvar + i) = tmat(j,nvar + i) + tmat(k,i)*derv2(k,j)
            enddo
          enddo
        enddo
        do i = 1,nvar
          do j = 1,nvar
            derv2(j,i) = 0.0_dp
            do k = 1,maxlim
              derv2(j,i) = derv2(j,i) + tmat(k,j)*tmat(k,nvar + i)
            enddo
          enddo
        enddo
      else
        do i = 1,nvar
          do j = 1,maxlim
            tmpx(j) = 0.0_dp
            do k = 1,maxlim
              tmpx(j) = tmpx(j) + tmat(k,i)*derv2(j,k)
            enddo
          enddo
          do j = 1,nvar
            derv2(j,i) = 0.0_dp
            do k = 1,maxlim
              derv2(j,i) = derv2(j,i) + tmpx(k)*tmat(k,j)
            enddo
          enddo
        enddo
      endif
    endif
    deallocate(tmpx,stat=status)
    if (status/=0) call deallocate_error('defsec','tmpx')
  endif
10 if (index(keyword,'derv2').ne.0.and.ioproc) then
    write(ioout,'(/,''  Reduced second derivative matrix : (eV/Angstrom**2)'',/)')
    do i = 1,nvar
      write(ioout,'(9f10.4)')(derv2(j,i),j = 1,nvar)
    enddo
    write(ioout,'(/)')
  endif
!
  t2 = cputime()
  tdel = t2 - t1
  thes = thes + tdel
!
  return
  end
