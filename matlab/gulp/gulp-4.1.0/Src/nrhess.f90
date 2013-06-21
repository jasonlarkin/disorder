  subroutine nrhess(hesinv,diag,nvar,nmin,ldiag)
!
!  Calculate hessian matrix by one of two methods:
!
!    (1) Exact inverse of second derivative matrix
!    (2) Diagonal elements only
!
!  The first corresponds to Newton-Raphson, while the second is only
!  employed if the Hessian is singular.
!
!  12/07 Unused variables removed
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
!  Julian Gale, NRI, Curtin University, December 2007
!
  use control
  use derivatives
  use general
  use iochannels
  use parallel
  use times
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: nmin
  integer(i4)                                  :: nvar
  logical                                      :: ldiag
  real(dp)                                     :: diag(*)
  real(dp)                                     :: hesinv(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ihdim
  integer(i4)                                  :: ind
  integer(i4)                                  :: info
  integer(i4)                                  :: j
  integer(i4), dimension(:), allocatable       :: kpvt
  integer(i4)                                  :: nlvar
  integer(i4)                                  :: status
  logical                                      :: lhdebug
  real(dp)                                     :: cputime
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp)                                     :: t1i
  real(dp)                                     :: t2i
  real(dp),    dimension(:), allocatable       :: temp
!
  t1 = cputime()
  lhdebug = (index(keyword,'hess').ne.0)
  ldiag = .false.
!
!  Set local number of variables
!
  nlvar = nvar - nmin + 1
!
!  Pack hessian and save diagonal elements
!
  ind = 0
  do i = nmin,nvar
    do j = nmin,i
      ind = ind + 1
      hesinv(ind) = derv2(j,i)
    enddo
    diag(i-nmin+1) = derv2(i,i)
  enddo
  if (lhdebug.and.ioproc) then
    write(ioout,'(/,'' Hessian Matrix :'',/)')
    do i = 1,nlvar
      ind = i*(i-1)/2
      write(ioout,'(2x,10(f12.4))')(hesinv(ind+j),j=1,i)
    enddo
  endif
!
!  Factorise matrix
!
  t1i = cputime()
  allocate(kpvt(nlvar),stat=status)
  if (status/=0) call outofmemory('nrhess','kpvt')
  call dsptrf('U',nlvar,hesinv,kpvt,info)
!
!  Check for singularities
!
  if (info.gt.0) then
    call outwarning('ill conditioned Hessian - using diagonal elements only',0_i4)
    ldiag = .true.
    ihdim = nlvar*(nlvar+1)/2
    do i = 1,ihdim
      hesinv(i) = 0.0_dp
    enddo
    do i = 1,nlvar
      ind = i*(i+1)/2
      if (abs(diag(i)).lt.1.0d-12) diag(i) = 1.0d-12
      hesinv(ind) = 1.0_dp/diag(i)
    enddo
  else
!
!  Complete inversion
!
    allocate(temp(3*nlvar),stat=status)
    if (status/=0) call outofmemory('nrhess','temp')
    call dsptri('U',nlvar,hesinv,kpvt,temp,info)
    deallocate(temp,stat=status)
    if (status/=0) call deallocate_error('nrhess','temp')
  endif
  deallocate(kpvt,stat=status)
  if (status/=0) call deallocate_error('nrhess','kpvt')
  t2i = cputime()
  tmati = tmati + t2i - t1i
  if (lhdebug.and.ioproc) then
    if (index(keyword,'hessd').ne.0) then
      write(ioout,'(/,'' Inverse Hessian Diagonal Elements :'',/)')
      do i = 1,nlvar
        ind = i*(i+1)/2
        write(ioout,'(2x,f12.8)')hesinv(ind)
      enddo
    else
      write(ioout,'(/,'' Inverse Hessian Matrix :'',/)')
      do i = 1,nlvar
        ind = i*(i-1)/2
        write(ioout,'(2x,10(f12.8))')(hesinv(ind+j),j=1,i)
      enddo
    endif
    write(ioout,'(/)')
  endif
!
  t2 = cputime()
  tdel = t2 - t1
  thes = thes + tdel - t2i + t1i
!
  return
  end
