  subroutine sec0
!
!  Generate symmetry adapted cluster second derivative matrix.
!
!  Freezing now added.
!
!   8/95 Modifications added to avoid using transformation
!        matrix unless constraints are present as this is
!        much slower.
!  10/04 Style updated for new version
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
  use control
  use current
  use derivatives
  use iochannels
  use parallel
  use times
  use transform
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: n3f
  integer(i4)                                  :: nint
  integer(i4)                                  :: status
  logical                                      :: lsdebug
  logical                                      :: ltmat
  real(dp)                                     :: cputime
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp), dimension(:), allocatable          :: tmp2
  real(dp)                                     :: tr1
!
  t1 = cputime()
  nint = nvar
  lsdebug = (index(keyword,'derv2').ne.0)
  n3f = 3*numat
  if (nbsm.gt.0) then
    n3f = n3f + numat
  endif
!
!  Work out whether full tmat multiplication is needed - use faster method to handle P1 fully relaxed case
!
  if (ncon.eq.0) then
    ltmat = .false.
  else
    ltmat = .true.
  endif
!******************************************
!  Internal derivatives :                 *
!  Symmetry transform second derivatives  *
!******************************************
  if (ltmat) then
    if ((2*nint).le.maxd2) then
      do i = 1,nint
        do j = 1,n3f
          tr1 = 0.0_dp
          do k = 1,n3f
            tr1 = tr1 + tmat(k,i)*derv2(k,j)
          enddo
          tmat(j,nint+i) = tr1
        enddo
      enddo
      do i = 1,nint
        do j = 1,nint
          derv2(j,i) = 0.0_dp
          do k = 1,n3f
            derv2(j,i) = derv2(j,i) + tmat(k,j)*tmat(k,nint+i)
          enddo
        enddo
      enddo
    else
      allocate(tmp2(n3f),stat=status)
      if (status/=0) call outofmemory('sec0','tmp2')
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
      if (status/=0) call deallocate_error('sec0','tmp2')
    endif
  elseif (nint.eq.(n3f-3).and.iopt(1).eq.10) then
    do i = 1,nint
      do j = 1,nint
        derv2(j,i) = derv2(j+3,i+3)
      enddo
    enddo
  else
    do i = 1,nint
      ii = iopt(i) - nstrains
      do j = 1,nint
        jj = iopt(j) - nstrains
        derv2(j,i) = derv2(jj,ii)
      enddo
    enddo
  endif
  if (lsdebug.and.ioproc) then
    write(ioout,'(/,''  Symmetrised Second Derivative Matrix  :'',/)')
    do i = 1,nvar
      write(ioout,'(2x,10(f14.4))')(derv2(i,j),j=1,i)
    enddo
    write(ioout,'(/)')
  endif
!
  t2 = cputime()
  thes = thes + t2 - t1
!
  return
  end
