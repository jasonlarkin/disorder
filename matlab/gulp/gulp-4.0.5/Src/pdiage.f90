  subroutine pdiage(mcv,maxd2,derv2,dervi,eigr,eigi,freq,fscale,lvectors,lprint,ifail)
!
!  Calculate eigenvalues and eigenvectors of the dynamical matrix
!  and subsequent calculation of properties of eigenvectors.
!
!  Eispack version - original one used in GULP
!
!   3/02 Created from peigen/pdiagg
!  10/04 Style updated
!   6/09 Changes added for PDF output and lprint added to arguments
!   8/10 Warning added for PDF calcs if there are imaginary modes
!   8/10 Call to rephase removed for PDF calcs
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
  use constants
  use current
  use element
  use general,      only : nwarn
  use gulpinput,    only : maxlinelength
  use m_pdfneutron, only : icurrentk, lpdf
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
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: matz
  integer(i4)                                  :: status
  real(dp)                                     :: cputime
  real(dp)                                     :: root
  real(dp)                                     :: sum
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp),    dimension(:), allocatable       :: w1
  real(dp),    dimension(:), allocatable       :: w2
  real(dp),    dimension(:), allocatable       :: w3
!
!  Call Eispack eigen solver
!
  t1 = cputime()
  if (lvectors) then
    matz = 1
  else
    matz = 0
  endif
  allocate(w1(mcv),stat=status)
  if (status/=0) call outofmemory('pdiag','w1')
  allocate(w2(mcv),stat=status)
  if (status/=0) call outofmemory('pdiag','w2')
  allocate(w3(2*mcv),stat=status)
  if (status/=0) call outofmemory('pdiag','w3')
  call ch(maxd2,mcv,derv2,dervi,freq,matz,eigr,eigi,w1,w2,w3,ifail)
  deallocate(w3,stat=status)
  if (status/=0) call deallocate_error('pdiag','w3')
  deallocate(w2,stat=status)
  if (status/=0) call deallocate_error('pdiag','w2')
  deallocate(w1,stat=status)
  if (status/=0) call deallocate_error('pdiag','w1')
  t2 = cputime()
  tdiag = tdiag + t2 - t1
!
!  Trap diagonalisation failure
!
  if (ifail.gt.0) then
    call outerror('diagonalisation of dynamical matrix failed',0_i4)
    call stopnow('pdiag')
  endif
  if (lvectors) then
!
!  Normalise eigenvectors
!
    do i = 1,mcv
      sum = 0.0_dp
      do j = 1,mcv
        sum = sum + eigr(j,i)*eigr(j,i) + eigi(j,i)*eigi(j,i)
      enddo
      if (sum.gt.0.0_dp) then
        sum = 1.0_dp/sqrt(sum)
        do j = 1,mcv
          eigr(j,i) = sum*eigr(j,i)
          eigi(j,i) = sum*eigi(j,i)
        enddo
      endif
    enddo
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
