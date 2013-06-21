  subroutine diagdyn(mcv,maxd2loc,mcvmin,freq,fscale,eigr,eigi,ifail)
!
!  Calculate eigenvalues and eigenvectors of the dynamical matrix
!
!   8/97 Created from peigen for use from fefunct
!   6/00 Trap for diagonalisation failure added
!   6/00 Workspace now locally allocated
!  10/04 Intent added
!   6/12 Modified so that either eispack or lapack can be used.
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, February 2012
!
  use current
  use derivatives
  use maths,        only : leispack_eigensolve
  use times
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(out)                   :: ifail
  integer(i4),  intent(in)                    :: maxd2loc
  integer(i4),  intent(in)                    :: mcv
  integer(i4),  intent(out)                   :: mcvmin
  real(dp),     intent(out)                   :: freq(*)
  real(dp),     intent(in)                    :: fscale
  real(dp),     intent(out)                   :: eigi(maxd2loc,*)
  real(dp),     intent(out)                   :: eigr(maxd2loc,*)
!
!  Local variables
!
  character(len=1)                            :: jobz
  integer(i4)                                 :: i
  integer(i4)                                 :: ilower
  integer(i4)                                 :: ilaenv
  integer(i4)                                 :: lcwork
  integer(i4)                                 :: nb
  integer(i4)                                 :: matz
  integer(i4)                                 :: status
  real(dp)                                    :: cputime
  real(dp)                                    :: rt
  real(dp)                                    :: t1
  real(dp)                                    :: t2
  real(dp), dimension(:),   allocatable       :: w1
  real(dp), dimension(:),   allocatable       :: w2
  real(dp), dimension(:,:), allocatable       :: w3
  complex(dpc), dimension(:), allocatable     :: cwork
!
  if (leispack_eigensolve) then
!
!  Eispack solver
!
    allocate(w1(mcv),stat=status)
    if (status/=0) call outofmemory('diagdyn','w1')
    allocate(w2(mcv),stat=status)
    if (status/=0) call outofmemory('diagdyn','w2')
    allocate(w3(2,mcv),stat=status)
    if (status/=0) call outofmemory('diagdyn','w3')
!
    matz = 1
!
!  Call Eispack eigen solver
!
    t1 = cputime()
    call ch(maxd2loc,mcv,derv2,dervi,freq,matz,eigr,eigi,w1,w2,w3,ifail)
    if (ifail.gt.0) then
      call outerror('diagonalisation of dynamical matrix failed',0_i4)
      call stopnow('diagdyn')
    endif
!
    deallocate(w3,stat=status)
    if (status/=0) call deallocate_error('diagdyn','w3')
    deallocate(w2,stat=status)
    if (status/=0) call deallocate_error('diagdyn','w2')
    deallocate(w1,stat=status)
    if (status/=0) call deallocate_error('diagdyn','w1')
  else
!
!  Lapack solver
!
!  Copy real and imaginary dynamical matrices to single complex matrix
!
    call phoncopy1(derv2,dervi,eigr,maxd2,0_i4,0_i4,maxd2,mcv)
    t1 = cputime()
    jobz = 'V'
    allocate(w1(3_i4*mcv),stat=status)
    if (status/=0) call outofmemory('diagdyn','w1')
    nb = ilaenv( 1_i4, 'ZHETRD', 'U', mcv, -1_i4, -1_i4, -1_i4 )
    lcwork = (nb + 1)*mcv
    allocate(cwork(lcwork),stat=status)
    if (status/=0) call outofmemory('diagdyn','cwork')
!
!  Call lapack eigen solver
!
    call zheev(jobz,'U',mcv,eigr,maxd2,freq,cwork,lcwork,w1,ifail)
!
    deallocate(cwork,stat=status)
    if (status/=0) call deallocate_error('diagdyn','cwork')
    deallocate(w1,stat=status)
    if (status/=0) call deallocate_error('diagdyn','w1')
!
    if (ifail.gt.0) then
      call outerror('diagonalisation of dynamical matrix failed',0_i4)
      call stopnow('diagdyn')
    endif
!
!  Transfer complex eigenvectors back to right arrays
!
    call phoncopy2l(derv2,dervi,eigr,maxd2,0_i4,0_i4,maxd2,mcv)
!
    eigr(1:mcv,1:mcv) = derv2(1:mcv,1:mcv)
    eigi(1:mcv,1:mcv) = dervi(1:mcv,1:mcv)
!
  endif
!
  t2 = cputime()
  tdiag = tdiag + t2 - t1
!**********************************************************************
!  Convert frequency units - imaginary freqs denoted by negative no.  *
!**********************************************************************
  do i = 1,mcv
    rt = freq(i)
    if (rt.ge.0.0_dp) then
      freq(i) = sqrt(rt)*fscale
    else
      rt = abs(rt)
      freq(i) = - sqrt(rt)*fscale
    endif
  enddo
  mcvmin = minmode
  ilower = mcvmin
  do i = ilower,mcv
    if (freq(i).lt.1.0_dp) mcvmin = mcvmin + 1
  enddo
!
  return
  end
