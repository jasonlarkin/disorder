  subroutine pdiagg(mcv,maxd2,derv2,eigr,freq,fscale,lvectors,lprint,ifail)
!
!  Calculate eigenvalues and eigenvectors of the dynamical matrix
!  and subsequent calculation of properties of eigenvectors.
!
!  Gamma point only version!!
!
!   3/02 Created from peigeng
!  10/04 Eispack call replaced by lapack
!   4/05 Modified so that eigr is used as workspace for dsyev so the
!        the dynamical matrix is preversed in derv2 for lvectors =
!        .true. case
!   6/09 Changes added for PDF output and lprint added to arguments
!
!  On entry :
!
!  mcv      = number of modes
!  maxd2    = left-hand dimension of derv2 and eigr
!  derv2    = mass-weighted dynamical matrix
!  fscale   = scale factor for frequencies to convert to wavenumbers
!  lvectors = if .true. then calculate eigenvectors
!  lprint   = if .true. print warnings
!
!  On exit :
!
!  eigr     = eigenvectors of dynamical matrix (if lvectors is true)
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, June 2009
!
  use constants
  use current
  use element
  use parallel
  use species
  use times
  implicit none
!
!  Passed variables
!
  integer(i4),  intent(out)                    :: ifail
  integer(i4),  intent(in)                     :: maxd2
  integer(i4),  intent(in)                     :: mcv
  logical,      intent(in)                     :: lprint
  logical,      intent(in)                     :: lvectors
  real(dp),     intent(in)                     :: derv2(maxd2,*)
  real(dp),     intent(out)                    :: eigr(maxd2,*)
  real(dp),     intent(in)                     :: fscale
  real(dp),     intent(out)                    :: freq(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ilaenv
  integer(i4)                                  :: j
  integer(i4)                                  :: nb
  integer(i4)                                  :: nsize
  integer(i4)                                  :: status
  real(dp)                                     :: cputime
  real(dp)                                     :: root
  real(dp)                                     :: sum
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp),    dimension(:), allocatable       :: w1
!
!  Call eigen solver
!
  t1 = cputime()
  if (lvectors) then
!
!  Copy dynamical matrix to eigenvector array to avoid overwriting
!
    eigr(1:mcv,1:mcv) = derv2(1:mcv,1:mcv)
  endif
!
!  Set parameters for eigensolver
!
  nb = ilaenv(1_i4,'DSYTRD','U',mcv,-1_i4,-1_i4,-1_i4)
  nsize = max(1,(nb+2)*mcv)
  allocate(w1(nsize+10),stat=status)
  if (status/=0) call outofmemory('pdiagg','w1')
  if (lvectors) then
    call dsyev('V','U',mcv,eigr,maxd2,freq,w1,nsize,ifail)
  else
    call dsyev('N','U',mcv,derv2,maxd2,freq,w1,nsize,ifail)
  endif
  deallocate(w1,stat=status)
  if (status/=0) call deallocate_error('pdiagg','w1')
  t2 = cputime()
  tdiag = tdiag + t2 - t1
!
!  Trap diagonalisation failure
!
  if (ifail.gt.0) then
    call outerror('diagonalisation of dynamical matrix failed',0_i4)
    call stopnow('pdiagg')
  endif
  if (lvectors) then
!
!  Normalise eigenvectors
!
    do i = 1,mcv
      sum = 0.0_dp
      do j = 1,mcv
        sum = sum + eigr(j,i)*eigr(j,i)
      enddo
      if (sum.gt.0.0_dp) then
        sum = 1.0_dp/sqrt(sum)
        do j = 1,mcv
          eigr(j,i) = sum*eigr(j,i)
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
    endif
  enddo
!
  return
  end
