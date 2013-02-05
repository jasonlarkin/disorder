  subroutine gasteiger(lmain)
!
!  Subroutine for calculating charges according to the scheme
!  of Gasteiger and Marsili.
!
!  lmain = .true.  => call from gulp, one off calc with output
!        = .false. => call from energy, needed for charges
!
!   1/07 Created based on eem.f
!   4/07 Searching for bonded atoms now uses nbonds number rather
!        than checking for a non-zero type
!   5/07 Partial occupancy data moved to module
!  12/07 References to nitergast replaced with niter
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
  use configurations
  use current
  use element
  use energies
  use iochannels
  use parallel
  use partial
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  logical, intent(in)                          :: lmain
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ia
  integer(i4)                                  :: imm
  integer(i4)                                  :: ii
  integer(i4)                                  :: j
  integer(i4)                                  :: nbond
  integer(i4)                                  :: ngast
  integer(i4)                                  :: ngastfoc
  integer(i4)                                  :: ni
  integer(i4)                                  :: niter
  integer(i4)                                  :: nri
  integer(i4), dimension(:), allocatable       :: ngastptr
  integer(i4), dimension(:), allocatable       :: ngastrptr
  integer(i4)                                  :: status
  logical,     dimension(:), allocatable       :: lgastfoc
  logical                                      :: lconverged
  real(dp)                                     :: damping
  real(dp)                                     :: cputime
  real(dp)                                     :: qdiff
  real(dp)                                     :: qd
  real(dp)                                     :: qsum
  real(dp)                                     :: qtot
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp),    dimension(:), allocatable       :: chiG
  real(dp),    dimension(:), allocatable       :: chiGplus
  real(dp),    dimension(:), allocatable       :: gastA
  real(dp),    dimension(:), allocatable       :: gastB
  real(dp),    dimension(:), allocatable       :: gastC
  real(dp),    dimension(:), allocatable       :: oldqa
!
  time1 = cputime()
!
!  Allocate local memory
!
  allocate(ngastptr(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','ngastptr')
  allocate(ngastrptr(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','ngastrptr')
  allocate(lgastfoc(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','lgastfoc')
  allocate(chiG(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','chiG')
  allocate(chiGplus(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','chiGplus')
  allocate(gastA(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','gastA')
  allocate(gastB(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','gastB')
  allocate(gastC(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','gastC')
  allocate(oldqa(numat),stat=status)
  if (status/=0) call outofmemory('gasteiger','oldqa')
!
!  Set up list of allowed elements
!
  do i = 1,maxele
    if (abs(gasteigerA(i)).gt.1.0d-6.or.abs(gasteigerB(i)).gt.1.0d-6) then
      lelementOK(i) = .true.
    else
      lelementOK(i) = .false.
    endif
  enddo
!
  qsum = 0.0_dp
  qtot = 0.0_dp
  ngast = 0
  ngastrptr(1:nasym) = 0
!
!  Check elements
!
  if (lsymopt) then
    do i = 1,nasym
      ia = iatn(i)
      if (lelementOK(ia).and.nregionno(nsft+i).eq.1) then
        ngast = ngast + 1
        ngastptr(ngast) = i
        ngastrptr(i) = ngast
        qsum = qsum - neqv(i)*qa(i)*occua(i)
      elseif (ia.gt.maxele) then
        call outerror('cannot use Gasteiger with shells present',0_i4)
        call stopnow('gasteiger')
      else
        qtot = qtot + neqv(i)*qa(i)*occua(i)
      endif
    enddo
  else
    do i = 1,numat
      ia = nat(i)
      if (lelementOK(ia).and.nregionno(nsft+nrelat(i)).eq.1) then
        ngast = ngast + 1
        ngastptr(ngast) = i
        ngastrptr(i) = ngast
        qsum = qsum - qf(i)*occuf(i)
      elseif (ia.gt.maxele) then
        call outerror('cannot use Gasteiger with shells present',0_i4)
        call stopnow('gasteiger')
      else
        qtot = qtot + qf(i)*occuf(i)
      endif
    enddo
  endif
!
!  Decide on total fragment charge - for cluster
!  there is no constraint so use sum of initial charges.
!  For periodic system, charge must be equal to the
!  negative sum of the non-Gasteiger ion charges.
!
  if (ndim.eq.0) then
    qtot = qsum
  endif
!
!  Now find the number of fully occupied sites for Gasteiger
!
  lgastfoc(1:numat) = .false.
  do i = 1,ngast
    ii = iocptr(ngastptr(i))
    lgastfoc(ii) = .true.
  enddo
  ngastfoc = 0
  do i = 1,ncfoc
    if (lgastfoc(i)) ngastfoc = ngastfoc + 1
  enddo
!
!  Initialise charges and store for convergence check
!
  do i = 1,nasym
    qa(i) = 0.0_dp
    oldqa(i) = qa(i)
  enddo
!
!  Assign parameters - take defaults from main arrays and then check hybridisation of C, N & O
!
  do i = 1,ngast
    ii = ngastptr(i)
    ni = iatn(ii)
    nri = nrel2(ii)
    gastA(i) = gasteigerA(ni)
    gastB(i) = gasteigerB(ni)
    gastC(i) = gasteigerC(ni)
!
!  Get number of bonds for C, N or O
!
    nbond = 0
    imm = 1
    do while (imm.gt.0.and.nbond.lt.nbonds(ii))
      nbond = nbond + 1
      imm = nbonded(nbond,nri)
    enddo
    if (imm.eq.0) nbond = nbond - 1
!
!  Modify parameters for C, N or O if not sp3 hybridised
!
    if (ni.eq.6.and.nbond.eq.3) then
      gastA(i) =  8.79_dp
      gastB(i) =  9.32_dp
      gastC(i) =  1.51_dp
    elseif (ni.eq.6.and.nbond.le.2) then
      gastA(i) = 10.39_dp
      gastB(i) =  9.45_dp
      gastC(i) =  0.73_dp
    elseif (ni.eq.7.and.nbond.eq.2) then
      gastA(i) = 12.87_dp
      gastB(i) = 11.15_dp
      gastC(i) =  0.85_dp
    elseif (ni.eq.7.and.nbond.eq.1) then
      gastA(i) = 15.68_dp
      gastB(i) = 11.70_dp
      gastC(i) = -0.27_dp
    elseif (ni.eq.8.and.nbond.eq.1) then
      gastA(i) = 17.07_dp
      gastB(i) = 13.79_dp
      gastC(i) =  0.47_dp
    endif
!
!  Set electronegativity of plus one ion - doesn't depend on charge!
!  Exception is hydrogen where a special value is used.
!
    if (ni.eq.1) then
      chiGplus(i) = 20.02_dp
    else
      chiGplus(i) = gastA(i) + gastB(i) + gastC(i)
    endif
  enddo
!****************************
!  Start of iterative loop  *
!****************************
  lconverged = .false.
  niter = 0
  if (lmain.and.ioproc) then
    write(ioout,'(''  Iterative solution of Gasteiger charges :'',/)')
  endif
  damping = 1.0_dp
  do while (niter.lt.ngastitermax.and..not.lconverged)
    niter = niter + 1
    damping = 0.5_dp*damping
!***********************************************
!  Find electronegativities at this iteration  *
!***********************************************
    do i = 1,ngast
      ii = ngastptr(i)
      chiG(i) = gastA(i) + qa(ii)*(gastB(i) + qa(ii)*gastC(i))
    enddo
!****************************************
!  Compute charges at latest iteration  *
!****************************************
    do i = 1,ngast
      ii = ngastptr(i)
      nri = nrel2(ii)
!
!  Loop over bonds and compute contributions to charge
!
      nbond = 0
      imm = 1
      do while (imm.gt.0.and.nbond.lt.maxbond)
        nbond = nbond + 1
        imm = nbonded(nbond,nri)
        if (imm.gt.0) then
          j = ngastrptr(nrelat(imm))
          if (j.gt.0) then
            if (chiG(j).gt.chiG(i)) then
              qa(ii) = qa(ii) + damping*(chiG(j)-chiG(i))/chiGplus(i)
            else
              qa(ii) = qa(ii) + damping*(chiG(j)-chiG(i))/chiGplus(j)
            endif
          endif
        endif
      enddo
    enddo
!**************************
!  Check for convergence  *
!**************************
    qdiff = 0.0_dp
    do i = 1,nasym
      qd = qa(i) - oldqa(i)
      qdiff = qdiff + abs(qd)*neqv(i)
      oldqa(i) = qa(i)
    enddo
    qdiff = qdiff/dble(numat)
    lconverged = (qdiff.lt.gasttol)
    if (lmain.and.ioproc) then
      write(ioout,'(''  ** Cycle : '',i4,'' Qdiff : '',f10.8)') niter,qdiff
    endif
!*****************************
!  End loop over iterations  *
!*****************************
  enddo
!
!  Store charges in configurational array
!
  do i = 1,nasym
    qlcfg(nsft+i) = qa(i)
  enddo
!*******************
!  Output results  *
!*******************
  if (lmain.and.ioproc) then
    write(ioout,'(//,''  Final Gasteiger charges:'',/)')
    if (lconverged) then
      write(ioout,'(''  Charges converged in '',i3,'' iterations'',/)') niter
    else
      write(ioout,'(''  Failed to converged after '',i3,'' iterations'',/)') niter
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''    Atom no.            Atomic No.             Charge'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nasym
      write(ioout,'(6x,i4,18x,i2,16x,f10.7)') i,iatn(i),qa(i)
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
!
!  Free local memory 
!
  deallocate(oldqa,stat=status)
  if (status/=0) call deallocate_error('gasteiger','oldqa')
  deallocate(gastC,stat=status)
  if (status/=0) call deallocate_error('gasteiger','gastC')
  deallocate(gastB,stat=status)
  if (status/=0) call deallocate_error('gasteiger','gastB')
  deallocate(gastA,stat=status)
  if (status/=0) call deallocate_error('gasteiger','gastA')
  deallocate(chiGplus,stat=status)
  if (status/=0) call deallocate_error('gasteiger','chiGplus')
  deallocate(chiG,stat=status)
  if (status/=0) call deallocate_error('gasteiger','chiG')
  deallocate(lgastfoc,stat=status)
  if (status/=0) call deallocate_error('gasteiger','lgastfoc')
  deallocate(ngastrptr,stat=status)
  if (status/=0) call deallocate_error('gasteiger','ngastrptr')
  deallocate(ngastptr,stat=status)
  if (status/=0) call deallocate_error('gasteiger','ngastptr')
!
!  Timing
!
  time2 = cputime()
  teem = teem + time2 - time1
!
  return
  end
