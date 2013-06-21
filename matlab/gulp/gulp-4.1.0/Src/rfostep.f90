  subroutine rfostep(pvect,hessian,xc,fc,gc,eig,cosm,pnlast, &
    nvar,nmin,jnrst,lfrst,loffridge,imode)
!
!  Determine the optimum minimisation/maximisation step according
!  to the RFO approach of Simons et al.
!
!  (1) Diagonalise the hessian, storing the eigenvectors in derv2
!  (2) Find lambda values for two P-RFO problems
!  (3) Form pvect, containing optimum displacement vector.
!
!  cosm = cosm of angle between search direction and gradients
!  eig    = eigenvalue array
!  gc     = gradient vector
!  hessian= hessian matrix in lower-half triangular form
!  jnrst  = cycle counter since hessian was last reset
!  lfrst  = flag to indicate whether to set mode for following
!  nupdate= no. of cycles of updating before hessian recalculation
!  nvar   = number of variables
!  pvect  = search direction
!
!  10/04 Style updated & rsp replaced by lapack call
!   6/05 Deallocation order reversed
!   3/07 linmin renamed to olinmin
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
!  Julian Gale, NRI, Curtin University, March 2007
!
  use control
  use derivatives
  use general
  use iochannels
  use optimisation
  use parallel
  use times
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in)                 :: imode
  integer(i4),      intent(out)                :: jnrst
  integer(i4),      intent(in)                 :: nmin
  integer(i4),      intent(in)                 :: nvar
  logical,          intent(inout)              :: lfrst
  logical,          intent(inout)              :: loffridge
  real(dp),         intent(out)                :: cosm
  real(dp),         intent(out)                :: eig(*)
  real(dp)                                     :: fc
  real(dp)                                     :: gc(*)
  real(dp)                                     :: hessian(*)
  real(dp)                                     :: pnlast
  real(dp)                                     :: pvect(*)
  real(dp)                                     :: xc(*)
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ierr
  integer(i4)                                  :: igroup
  integer(i4)                                  :: ihdim
  integer(i4)                                  :: ii
  integer(i4)                                  :: indi
  integer(i4)                                  :: ip
  integer(i4)                                  :: iresid
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: nactual
  integer(i4)                                  :: nlower
  integer(i4)                                  :: nlvar
  integer(i4)                                  :: nmode
  integer(i4)                                  :: nneg
  integer(i4)                                  :: nupper
  integer(i4), dimension(:), allocatable       :: nvptr
  integer(i4)                                  :: status
  logical                                      :: lokf
  logical                                      :: lsavehess
  real(dp)                                     :: alpha
  real(dp)                                     :: cputime
  real(dp)                                     :: ddot
  real(dp)                                     :: fcold
  real(dp)                                     :: gl
  real(dp)                                     :: gnrm
  real(dp),    dimension(:), allocatable       :: hessave
  real(dp)                                     :: olap
  real(dp)                                     :: omax
  real(dp)                                     :: pnorm
  real(dp)                                     :: pscal
  real(dp)                                     :: rlam
  real(dp)                                     :: step
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp),    dimension(:), allocatable       :: tmp1
  real(dp),    dimension(:), allocatable       :: tmp2
  real(dp)                                     :: trm1
  real(dp)                                     :: trm2
!
  t1 = cputime()
  lsavehess = (nupdate.gt.1)
  nlvar = nvar - nmin + 1
  ihdim = nlvar*(nlvar+1)/2
  if (lsavehess) then
    allocate(hessave(ihdim),stat=status)
    if (status/=0) call outofmemory('rfostep','hessave')
  endif
!
!  Allocate local memory
!
  allocate(nvptr(nlvar),stat=status)
  if (status/=0) call outofmemory('rfostep','nvptr')
  allocate(tmp1(3*nlvar),stat=status)
  if (status/=0) call outofmemory('rfostep','tmp1')
  allocate(tmp2(nlvar),stat=status)
  if (status/=0) call outofmemory('rfostep','tmp2')
!
  cosm = 0.0_dp
  pnorm = 0.0_dp
  gnrm = 0.0_dp
!*************************************
!  Save hessian to avoid corruption  *
!*************************************
  if (lsavehess) then
    do i = 1,ihdim
      hessave(i) = hessian(i)
    enddo
  endif
!************************
!  Diagonalise hessian  *
!************************
  call dspev('V','U',nlvar,hessian,eig,derv2,maxd2,tmp1,ierr)
  if (ierr.gt.0) then
    call outerror('diagonalisation of Hessian failed in RFO',0_i4)
    call stopnow('rfostep')
  endif
!****************************
!  Check hessian structure  *
!****************************
  nneg = 0
  do i = 1,nlvar
    if (eig(i).lt.-1.0d-3) nneg = nneg + 1
  enddo
  if (lopprt.and.ioproc) then
    if (nneg.eq.morder) then
      write(ioout,'(''  ** Hessian has required structure'')')
    else
      write(ioout,'(''  ** Hessian has wrong structure'')')
      write(ioout,'(''  ** Imaginary eigenvectors = '',i3)') nneg
    endif
  endif
  if (nneg.gt.morder.and.loffridge) then
    loffridge = .false.
!
!  Too many imaginary modes - perform off ridge line minimisation
!
    alpha = 0.1_dp
    do i = nmin,nvar
      pvect(i) = derv2(i-nmin+1,nneg)
    enddo
    pnorm = sqrt(ddot(nlvar,pvect(nmin),1_i4,pvect(nmin),1_i4))
    if (pnorm.gt.1.5_dp*pnlast) then
      pscal = 1.5_dp*pnlast/pnorm
      do i = nmin,nvar
        pvect(i) = pvect(i)*pscal
      enddo
    endif
    fcold = fc
    call olinmin(xc,alpha,pvect,nvar,nmin,fc,lokf,gc,imode)
    if (lokf) then
      jnrst = nupdate + 1
    else
      fc = fcold
      call daxpy(nlvar,-alpha,pvect(nmin),1_i4,xc(nmin),1_i4)
    endif
  endif
!********************
!  Restore hessian  *
!********************
  if (lsavehess) then
    do i = 1,ihdim
      hessian(i) = hessave(i)
    enddo
  endif
!****************************************************
!  Transform gradient into the local hessian modes  *
!****************************************************
  do i = 1,nlvar
    tmp1(i) = 0.0_dp
    do j = 1,nlvar
      tmp1(i) = tmp1(i) + derv2(j,i)*gc(j+nmin-1)
    enddo
    tmp2(i) = tmp1(i)*tmp1(i)
  enddo
!*******************************
!  Output Hessian if required  *
!*******************************
  if (index(keyword,'hess').ne.0.and.ioproc) then
    write(ioout,'(/,''  Diagonalised Hessian analysis : '',/)')
    igroup = nvar/3
    iresid = nvar - igroup*3
    indi = 0
    if (igroup.gt.0) then
      do i = 1,igroup
        write(ioout,'(''  Eigenvalues'',3f20.6)') (eig(indi+j),j=1,3)
        write(ioout,'(''  Gradient   '',3f20.6)') (tmp1(indi+j),j=1,3)
        write(ioout,'(/,''  Eigenvectors:'')')
        do j = 1,nvar
          write(ioout,'(3x,i6,4x,3f20.6)') j,(derv2(j,indi+k),k=1,3)
        enddo
        indi = indi + 3
        write(ioout,'(/)')
      enddo
    endif
    if (iresid.gt.0) then
      write(ioout,'(''  Eigenvalues'',3f20.6)') (eig(indi+j),j=1,iresid)
      write(ioout,'(''  Gradient   '',3f20.6)') (tmp1(indi+j),j=1,iresid)
      write(ioout,'(/,''  Eigenvectors:'')')
      do j = 1,nvar
        write(ioout,'(3x,i6,4x,3f20.6)') j,(derv2(j,indi+k),k=1,iresid)
      enddo
    endif
    write(ioout,'(/)')
  endif
!
!  Analyse local modes to exclude those with zero gradients
!
  nactual = 0
  do i = 1,nlvar
    if (abs(tmp1(i)).gt.1.0d-16) then
      nactual = nactual + 1
      nvptr(nactual) = i
    endif
  enddo
!*******************
!  Mode following  *
!*******************
  if (mode.gt.0) then
    if (lfrst) then
!
!  First time vector is given by mode number
!
      ii = nvptr(mode)
      lfrst = .false.
    else
!
!  Subsequently find maximum overlap with previous mode
!
      omax = 0.0_dp
      ii = 1
      do i = 1,nactual
        ip = nvptr(i)
        olap = 0.0_dp
        do j = 1,nvar
          olap = olap + rmode(j)*derv2(j,ip)
        enddo
        olap = abs(olap)
        if (olap.gt.omax) then
          omax = olap
          ii = ip
        endif
      enddo
    endif
!
!  Swap mode to be followed to be mode 1
!
    trm1 = tmp1(ii)
    trm2 = tmp2(ii)
    do i = 1,nlvar
      rmode(i) = derv2(i,ii)
    enddo
    do i = 2,ii
      tmp1(ii) = tmp1(ii-1)
      tmp2(ii) = tmp2(ii-1)
      do j = 1,nlvar
        derv2(j,ii) = derv2(j,ii-1)
      enddo
    enddo
    tmp1(1) = trm1
    tmp2(1) = trm2
    do i = 1,nlvar
      derv2(i,1) = rmode(i)
    enddo
!
!  Reset pointer to valid modes
!
    nactual = 0
    do i = 1,nlvar
      if (abs(tmp1(i)).gt.1.0d-8) then
        nactual = nactual + 1
        nvptr(nactual) = i
      endif
    enddo
  endif
!***************
!  Zero pvect  *
!***************
  do i = nmin,nvar
    pvect(i) = 0.0_dp
  enddo
!*********************************
!  Generate displacement vector  *
!*********************************
!
!  Minimisation modes
!
  nmode = morder + 1
  nlower = morder + 1
  nupper = nactual
  if (nlower.gt.nupper+1) then
    call outerror('no. of actual local modes is less than order',0_i4)
    call stopnow('rfostep')
  endif
  if (nlower.le.nupper) then
    call lambda(nmode,nlower,nupper,nvptr,nactual,rlam,tmp2,eig)
    do i = nlower,nupper
      ii = nvptr(i)
      gl = tmp1(ii)
      step = gl/(rlam-eig(ii))
      cosm = cosm - step*gl
      pnorm = pnorm + step*step
      gnrm = gnrm + gl*gl
      do j = nmin,nvar
        pvect(j) = pvect(j) + step*derv2(j-nmin+1,ii)
      enddo
    enddo
    if (ldebug.and.ioproc) then
      write(ioout,'(''  ** Lambda for minimisation = '',f12.6)')rlam
    endif
  endif
!
!  Maximisation modes
!
  if (morder.gt.0) then
    nmode = morder + 1
    nlower = 1
    nupper = morder
    call lambda(nmode,nlower,nupper,nvptr,nactual,rlam,tmp2,eig)
    do i = nlower,nupper
      ii = nvptr(i)
      gl = tmp1(ii)
      step = gl/(rlam-eig(ii))
      cosm = cosm + step*gl
      pnorm = pnorm + step*step
      gnrm = gnrm + gl*gl
      do j = nmin,nvar
        pvect(j) = pvect(j) + step*derv2(j-nmin+1,ii)
      enddo
    enddo
    if (ldebug.and.ioproc) then
      write(ioout,'(''  ** Lambda for maximisation = '',f12.6)') rlam
    endif
  endif
  gnrm = sqrt(gnrm)
  pnorm = sqrt(pnorm)
  cosm = cosm/(pnorm*gnrm)
!
!  Free local memory
!
  deallocate(tmp2,stat=status)
  if (status/=0) call deallocate_error('rfostep','tmp2')
  deallocate(tmp1,stat=status)
  if (status/=0) call deallocate_error('rfostep','tmp1')
  deallocate(nvptr,stat=status)
  if (status/=0) call deallocate_error('rfostep','nvptr')
  if (lsavehess) then
    deallocate(hessave,stat=status)
    if (status/=0) call deallocate_error('rfostep','hessave')
  endif
!
!  Timing
!
  t2 = cputime()
  tdel = t2 - t1
  thes = thes + tdel
!
  return
  end
