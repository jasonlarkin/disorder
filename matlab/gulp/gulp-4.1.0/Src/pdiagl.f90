  subroutine pdiagl(mcv,maxd2,derv2,dervi,eigr,eigi,freq,fscale,lvectors,lprint,ifail)
!
!  Calculate eigenvalues and eigenvectors of the dynamical matrix
!  and subsequent calculation of properties of eigenvectors.
!
!  Lapack version
!
!   2/12 Lapack version created from eispack version
!   3/12 Corrected for case where vectors are not being calculated
!
!  On entry :
!
!  mcv      = number of modes
!  maxd2    = left-hand dimension of derv2 and eigr
!  derv2    = mass-weighted dynamical matrix (real component)
!  dervi    = mass-weighted dynamical matrix (imaginary component)
!  fscale   = scale factor for frequencies to convert to wavenumbers
!  lprint   = if .true. print warnings
!  lvectors = if .true. then calculate eigenvectors
!
!  On exit :
!
!  eigr     = real eigenvectors of dynamical matrix (if lvectors is true)
!  eigi     = imaginary eigenvectors of dynamical matrix (if lvectors is true)
!  freq     = frequencies of vibration in wavenumbers
!  ifail    = flag indicating success or failure
!
!  NB  eigr and eigi must be of at least dimension mcv*mcv on entry
!      even if eigenvectors are not being calculated so that memory 
!      can be used as workspace to hold complex matrix.
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
!  Julian Gale, NRI, Curtin University, March 2012
!
  use constants
  use current
  use element
  use general,      only : nwarn
  use gulpinput,    only : maxlinelength
  use m_pdfneutron, only : icurrentk, lpdf
  use maths,        only : ldivide_and_conquer
  use parallel
  use species
  use times
  implicit none
!
!  Passed variables
!
  integer(i4),    intent(out)                  :: ifail
  integer(i4),    intent(in)                   :: maxd2
  integer(i4),    intent(in)                   :: mcv
  logical,        intent(in)                   :: lprint
  logical,        intent(in)                   :: lvectors
  real(dp),       intent(in)                   :: derv2(maxd2,*)
  real(dp),       intent(in)                   :: dervi(maxd2,*)
  real(dp),       intent(out)                  :: eigr(maxd2,*)
  real(dp),       intent(out)                  :: eigi(maxd2,*)
  real(dp),       intent(in)                   :: fscale
  real(dp),       intent(out)                  :: freq(*)
!
!  Local variables
!
  character(len=maxlinelength)                 :: warningstring
  character(len=1)                             :: jobz
  integer(i4)                                  :: i
  integer(i4)                                  :: ilaenv
  integer(i4)                                  :: lcwork
  integer(i4)                                  :: liwork
  integer(i4)                                  :: lwork
  integer(i4)                                  :: nb
  integer(i4)                                  :: status
  integer(i4),  dimension(:),   allocatable    :: iw
  real(dp)                                     :: cputime
  real(dp)                                     :: root
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp),     dimension(:),   allocatable    :: w1
  complex(dpc), dimension(:),   allocatable    :: cwork
!
!  Copy real and imaginary dynamical matrices to single complex matrix
!
  call phoncopy1(derv2,dervi,eigr,maxd2,0_i4,0_i4,maxd2,mcv)
!
!  Call lapack eigen solver
!
  t1 = cputime()
  if (lvectors) then
    jobz = 'V'
  else
    jobz = 'N'
  endif
!
!  Call lapack eigen solver
!
  if (ldivide_and_conquer) then
    if (lvectors) then
      lcwork = mcv*(2 + mcv)
      lwork  = 1 + mcv*(5 + 2*mcv)
    else
      lcwork = mcv + 1
      lwork  = mcv
    endif
    liwork = 5*mcv + 3
!
    allocate(iw(liwork),stat=status)
    if (status/=0) call outofmemory('pdiag','iw')
    allocate(w1(lwork),stat=status)
    if (status/=0) call outofmemory('pdiag','w1')
    allocate(cwork(lcwork),stat=status)
    if (status/=0) call outofmemory('pdiag','cwork')
!
    call zheevd(jobz,'U',mcv,eigr,maxd2,freq,cwork,lcwork,w1,lwork,iw,liwork,ifail)
  else
    allocate(w1(3_i4*mcv),stat=status)
    if (status/=0) call outofmemory('pdiag','w1')
    nb = ilaenv( 1_i4, 'ZHETRD', 'U', mcv, -1_i4, -1_i4, -1_i4 )
    lcwork = (nb + 1)*mcv
    allocate(cwork(lcwork),stat=status)
    if (status/=0) call outofmemory('pdiag','cwork')
!
    call zheev(jobz,'U',mcv,eigr,maxd2,freq,cwork,lcwork,w1,ifail)
  endif
!
  deallocate(cwork,stat=status)
  if (status/=0) call deallocate_error('pdiag','cwork')
  deallocate(w1,stat=status)
  if (status/=0) call deallocate_error('pdiag','w1')
!
  t2 = cputime()
  tdiag = tdiag + t2 - t1
!
!  Trap diagonalisation failure
!
  if (ifail.gt.0) then
    call outerror('diagonalisation of dynamical matrix failed',0_i4)
    call stopnow('pdiag')
  endif
!
  if (lvectors) then
!
!  Transfer complex eigenvectors back to right arrays
!
    call phoncopy2l(derv2,dervi,eigr,maxd2,0_i4,0_i4,maxd2,mcv)
!
    eigr(1:mcv,1:mcv) = derv2(1:mcv,1:mcv)
    eigi(1:mcv,1:mcv) = dervi(1:mcv,1:mcv)
  endif
!
!  Convert frequency units - imaginary freqs denoted by negative no.
!
  do i = 1,mcv
    root = freq(i)
    if (root.ge.0.0_dp) then
      freq(i) = sqrt(root)*fscale
    else
      root = abs(root)
      freq(i) = - sqrt(root)*fscale
      if (lpdf.and.lprint) then
!
!  For PDF calculations warn of significant imaginary modes
!
        if (freq(i).lt.(-1.0d-2)) then
          write(warningstring,*) "Imag freq, mode ",i,", K",icurrentk,": ",freq(i)," cm-1"
          call outwarning(trim(adjustl(warningstring)),0_i4 )
          nwarn = nwarn + 1
        endif
      endif
    endif
  enddo
!
  return
  end
