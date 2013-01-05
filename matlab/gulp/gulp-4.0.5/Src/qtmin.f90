  subroutine qtmin(n,xc,fc,gc,maxj,jmat,fij,chivec,ifail)
!
!  Conjugate gradients optimisation routine for QTPIE charges
!
!   4/10 Created from minimise
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
!  Julian Gale, NRI, Curtin University, April 2010
!
  use control
  use iochannels
  use parallel
  use m_conjgr, only: conjgr

  implicit none
!
!  Passed variables
!
  integer(i4),          intent(out)         :: ifail
  integer(i4),          intent(in)          :: n
  integer(i4),          intent(in)          :: maxj
  real(dp),             intent(out)         :: fc
  real(dp),             intent(out)         :: gc(n)
  real(dp),             intent(inout)       :: xc(n)
  real(dp),             intent(in)          :: chivec(n)
  real(dp),             intent(in)          :: jmat(maxj,*)
  real(dp),             intent(in)          :: fij(maxj,*)
!
!  Local variables
!
  integer(i4)                               :: i
  integer(i4)                               :: niter
  integer(i4)                               :: status
  logical                                   :: okf
  logical                                   :: loutput
  real(dp)                                  :: dxmax
  real(dp)                                  :: ftol
  real(dp)                                  :: gnorm
  real(dp)                                  :: cgcntr(0:20)
  real(dp), dimension(:), allocatable, save :: aux
!
!  Initialisation
!
  niter = 0
  okf = .false.
  cgcntr(0:20) = 0.0_dp
  dxmax = 0.05_dp
  ftol = 1.0d-4
  loutput = .true.
!
!  Allocate workspace
!
  allocate(aux(2*n),stat=status)
  if (status/=0) call outofmemory('qtmin','aux')
!
!  Output heading
!
  if (loutput) then
    write(ioout,'(/,'' QTmin: minimisation of charge transfer matrix elements: '',/)')
  endif
!
!  Start loop over calls to conjugate gradients until converged
!
  do while (.not.okf)
    niter = niter + 1
!
!  Call function
!
    call ctfunct(1_i4,n,xc,fc,gc,maxj,jmat,fij,chivec)
!
!  Change gradient to force
!
    gnorm = 0.0_dp
    do i = 1,n
      gc(i) = - gc(i)
      gnorm = gnorm + gc(i)**2
    enddo
    gnorm = gnorm/dble(n)
    gnorm = sqrt(gnorm)
!
!  Print latest info
!
    if (loutput) then
      write(ioout,'('' QTmin: '',i5,'' Energy = '',f18.8,'' Gnorm = '',f18.8)') niter,fc,gnorm
    endif
!
!  Call minimiser
!
    call conjgr( n, xc, gc, dxmax, ftol, cgcntr, aux )
!
!  Test for convergence
!
    okf = (int(cgcntr(0)) .eq. 0)  
  enddo
!
!  Finalisation
!
  ifail = 0
!
!  Deallocate workspace
!
  deallocate(aux,stat=status)
  if (status/=0) call deallocate_error('qtmin','aux')
  return
  end
