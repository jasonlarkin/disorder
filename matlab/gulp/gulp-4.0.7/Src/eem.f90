  subroutine eem(lmain,lgrad1,lgrad2)
!
!  Subroutine for performing electronegativity equilisation calcns
!  according to work of Mortier
!
!  Periodic boundary condition version
!  Now uses only the asymmetric unit
!
!  lmain = .true.  => call from gulp, one off calc with output
!        = .false. => call from energy, needed for charges
!
!  If second derivatives are being used then tmat must be stored
!  on channel 54 while charges are calculated.
!
!  12/97 Modified to included QEq scheme. Note that when hydrogen
!        is present solution is iterative as interaction depends
!        on the charge in a non-linear fashion.
!  12/97 Self energy calculated and stored for inclusion in total
!        energy.
!  12/97 Modified so that only one of the second derivative arrays
!        has to be passed to make it easier to incorporate into
!        energy call.
!  12/97 Gradient logicals added to call as there are now EEM/QEq
!        contributions to the derivatives
!   1/98 Correction of derivatives removed as this is now handled
!        during main derivative calculation
!   2/98 If lgrad2 and .not.lmain then force dcharge to be used
!        to generate dq/dalpha for the full set of atoms as this 
!        is needed in the second derivative calculation.
!   6/98 Matrix inversion failure trapped
!  12/99 Modified so that unparameterised elements are treated as
!        fixed point charges.
!   2/01 Modified to handle mean field models by reducing sites
!        down to the fully occupied set.
!   5/02 Charges fixed for region 2 in surface calculation
!  10/02 ReaxFF modifications added
!  11/02 Calculation of strain derivatives of charge turned on
!  11/02 Initial charges now set for the benefit of the 1-D case
!        where the convergence of the energy is tested in order
!        to find the range of the Coulomb interaction.
!   2/03 Use of matinv removed to increase speed
!   3/03 dgetrf/i used for lsymopt case since matrix is not symmetric
!   7/05 Streitz and Mintmire modifications added
!   7/05 Array that holds the negative electronegativity now passed
!        to genpot/sympot for correction in S and M scheme
!   5/07 Partial occupancy data moved to module
!   6/07 Size of z array corrected due to requirements of genpot
!   6/07 z array reduced after genpot call for S and M method to required
!        subset of elements
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!   6/09 Bug in dimensioning of linear arrays fixed
!   6/09 Site energy added
!   8/11 Electric field added to charge calculation
!   9/11 lgrad1 added to incoming argument list
!  11/11 Region-region energy contributions stored
!   9/12 Pacha added
!  12/12 Option for q0 added
!  12/12 Time-dependent field added
!  12/12 Modified to allow for multiple time-dependent fields
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
!  Julian Gale, NRI, Curtin University, December 2012
!
  use control
  use configurations
  use constants,     only : angstoev
  use current
  use derivatives
  use element
  use energies
  use field,         only : lfieldcfg, ntdfieldcfg
  use iochannels
  use parallel
  use partial
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  logical, intent(in)                          :: lgrad1
  logical, intent(in)                          :: lgrad2
  logical, intent(in)                          :: lmain
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ia
  integer(i4)                                  :: ifail
  integer(i4)                                  :: ii
  integer(i4)                                  :: ilaenv
  integer(i4), dimension(:), allocatable       :: ipivot
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: lwrk
  integer(i4)                                  :: n
  integer(i4),                            save :: ncfold = 0
  integer(i4)                                  :: neemfoc
  integer(i4)                                  :: ni
  integer(i4)                                  :: niter
  integer(i4)                                  :: nitereem
  integer(i4)                                  :: nr
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: status
  logical,     dimension(:), allocatable       :: leemfoc
  logical                                      :: lconverged
  logical                                      :: lDoChargeDeriv
  logical                                      :: literate
  real(dp)                                     :: chii
  real(dp)                                     :: cputime
  real(dp),    dimension(:), allocatable       :: dpacked
  real(dp)                                     :: enega
  real(dp)                                     :: eself_before
  real(dp)                                     :: q0i
  real(dp)                                     :: qdiff
  real(dp)                                     :: qd
  real(dp)                                     :: qguesstot
  real(dp)                                     :: qi
  real(dp),    dimension(:), allocatable       :: qnmr
  real(dp)                                     :: qsum
  real(dp)                                     :: qtot
  real(dp)                                     :: reqv
  real(dp)                                     :: rjfac
  real(dp)                                     :: rmui
  real(dp)                                     :: rnguess
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp),    dimension(:), allocatable       :: wrk
  real(dp)                                     :: zetah0
  real(dp),    dimension(:), allocatable       :: oldqa
  real(dp),    dimension(:), allocatable       :: vfield
  real(dp),    dimension(:), allocatable       :: z
  real(dp),    dimension(:), allocatable       :: z2
!
  time1 = cputime()
!
!  Allocate local memory
!
  allocate(leemfoc(numat),stat=status)
  if (status/=0) call outofmemory('eem','leemfoc')
!
  lDoChargeDeriv = (index(keyword,'dcha').ne.0)
!
!  Assign parameters
!  Note that there are parameter sets for hydrogen:
!     nat = 1 => H+
!     nat = 2 => H-
!
  if (.not.lqeq.and..not.lSandM.and.index(keyword,'oldeem').ne.0) then
    chi(14) = 3.478_dp
    rmu(14) = 6.408_dp
    rmu(8) = 9.466_dp
    chi(1) = 3.398_dp
    rmu(1) = 20.818_dp
    chi(2) = 4.706_dp
    rmu(2) = 8.899_dp
  endif
!
!  Set up chi/mu according to method
!
  if (lqeq) then
    do i = 1,maxele
      if (abs(qeqchi(i)).gt.1.0d-6.or.abs(qeqmu(i)).gt.1.0d-6) then
        lelementOK(i) = .true.
      else
        lelementOK(i) = .false.
      endif
    enddo
  elseif (lSandM) then
    do i = 1,maxele
      if (abs(smchi(i)).gt.1.0d-6.or.abs(smmu(i)).gt.1.0d-6) then
        lelementOK(i) = .true.
      else
        lelementOK(i) = .false.
      endif
    enddo
  elseif (lpacha) then
    do i = 1,maxele
      if (abs(chi_pacha(i)).gt.1.0d-6.or.abs(rad_pacha(i)).gt.1.0d-6) then
        lelementOK(i) = .true.
        mu_pacha(i) = 0.5_dp*angstoev/rad_pacha(i)
      else
        lelementOK(i) = .false.
      endif
    enddo
  else
    do i = 1,maxele
      if (abs(chi(i)).gt.1.0d-6.or.abs(rmu(i)).gt.1.0d-6) then
        lelementOK(i) = .true.
      else
        lelementOK(i) = .false.
      endif
    enddo
  endif
!
  qsum = 0.0_dp
  qtot = 0.0_dp
  neem = 0
!
!  Check elements
!
  if (lsymopt) then
    do i = 1,nasym
      ia = iatn(i)
      if (lelementOK(ia).and.nregionno(nsft+i).eq.1) then
        neem = neem + 1
        neemptr(neem) = i
        qsum = qsum - neqv(i)*qa(i)*occua(i)
      elseif (ia.gt.maxele) then
        call outerror('cannot use EEM with shells present',0_i4)
        call stopnow('eem')
      else
        qtot = qtot + neqv(i)*qa(i)*occua(i)
      endif
    enddo
  else
    do i = 1,numat
      ia = nat(i)
      if (lelementOK(ia).and.nregionno(nsft+nrelat(i)).eq.1) then
        neem = neem + 1
        neemptr(neem) = i
        qsum = qsum - qf(i)*occuf(i)
      elseif (ia.gt.maxele) then
        call outerror('cannot use EEM with shells present',0_i4)
        call stopnow('eem')
      else
        qtot = qtot + qf(i)*occuf(i)
      endif
    enddo
  endif
!
!  Now find the number of fully occupied sites for EEM/QEq
!
  leemfoc(1:numat) = .false.
  do i = 1,neem
    ii = iocptr(neemptr(i))
    leemfoc(ii) = .true.
  enddo
  neemfoc = 0
  do i = 1,ncfoc
    if (leemfoc(i)) neemfoc = neemfoc + 1
  enddo
!
!  Check the memory for the linear arrays
!
  if (numat+1.gt.maxat) then
    maxat = numat + 1
    call changemaxat
  endif
!
!  Check the memory for the square arrays
!
  if (lsymopt) then
    if (nasym+1.gt.maxd2u) then
      maxd2u = nasym + 1
      call changemaxd2
    endif
    if (numat+1.gt.maxd2) then
      maxd2 = numat + 1
      call changemaxd2
    endif
  else
    if (numat+1.gt.maxd2u) then
      maxd2u = numat + 1
      call changemaxd2
    endif
    if (numat+1.gt.maxd2) then
      maxd2 = numat + 1
      call changemaxd2
    endif
  endif
!
!  Set the pointer to where the electronegativity should be as well
!
  neemptr(neemfoc+1) = nasym + 1
!
!  Decide on total EEM fragment charge - for cluster
!  there is no constraint so use sum of initial charges.
!  For periodic system, charge must be equal to the
!  negative sum of the non-EEM ion charges.
!
  if (ndim.eq.0) then
    qtot = qsum
  endif
!*****************************************************************
!  Is hydrogen present in QEq? If so then solution is iterative  *
!*****************************************************************
  literate = .false.
  if (lqeq) then
    i = 0
    do while (i.lt.nasym.and..not.literate)
      i = i + 1
      literate = (iatn(i).eq.1)
    enddo
  endif
  if (literate) then
    nitereem = nqeqitermax
    zetah0 = 0.529177_dp*0.75_dp/qeqrad(1)
  else
    nitereem = 1
  endif
!
!  Allocate local memory that depends on neem
!
  allocate(z(max(neem+1,numat)),stat=status)
  if (status/=0) call outofmemory('eem','z')
  allocate(z2(numat),stat=status)
  if (status/=0) call outofmemory('eem','z2')
  if (literate) then
    allocate(oldqa(nasym),stat=status)
    if (status/=0) call outofmemory('eem','oldqa')
  endif
  if (lfieldcfg(ncf).or.(ntdfieldcfg(ncf).gt.0)) then
    allocate(vfield(numat),stat=status)
    if (status/=0) call outofmemory('eem','vfield')
  endif
!
!  Calculate reciprocal lattice vectors
!
  if (lewald.and.ndim.gt.1) call kindex
!
!  For 1-D case set guess at charges based EEM parameters
!  and scaled up by 1.5 to allow for increase in ionicity.
!
  if (ndim.eq.1.and.ncf.ne.ncfold) then
    ncfold = ncf
    qguesstot = 0.0_dp
    rnguess = 0.0_dp
    do i = 1,neem
      ii = neemptr(i)
      if (lqeq) then   
        chii = qeqchi(iatn(ii))
        rmui = qeqmu(iatn(ii))
        q0i  = qeqq0(iatn(ii))
      elseif (lSandM) then   
        chii = smchi(iatn(ii))
        rmui = smmu(iatn(ii))
        q0i  = smq0(iatn(ii))
      elseif (lpacha) then   
        chii = chi_pacha(iatn(ii))
        rmui = mu_pacha(iatn(ii))
        q0i  = q0_pacha(iatn(ii))
      else
        chii = chi(iatn(ii))
        rmui = rmu(iatn(ii))
        q0i  = q0(iatn(ii))
      endif
      qa(ii) = q0i - chii/rmui
      qguesstot = qguesstot + qa(ii)*dble(neqv(ii))*occua(ii)
      rnguess = rnguess + dble(neqv(ii))*occua(ii)
    enddo
    qguesstot = (qguesstot + qtot)/rnguess
    do i = 1,neem
      ii = neemptr(i)
      qa(ii) = qa(ii) - qguesstot
      if (abs(qtot).lt.1.0d-12) qa(ii) = 1.5_dp*qa(ii)
      do j = 1,numat
        if (nrelat(j).eq.ii) then
          qf(j) = qa(ii)
        endif
      enddo
    enddo
    if (index(keyword,'debu').ne.0.and.ioproc) then
      write(ioout,'(/,''  Initial guess for 1-D variable charges :'',/)')
      write(ioout,'('' Atom        Q'')')
      do i = 1,neem
        ii = neemptr(i)
        write(ioout,'(i5,1x,f12.6)') ii,qa(ii)
      enddo
      write(ioout,'(/)')
    endif
  endif
!
!  Setup coordinates
!
  if (lsymopt) then
    do i = 1,nasym
      nr = nrel2(i)
      xalat(i) = xclat(nr)
      yalat(i) = yclat(nr)
      zalat(i) = zclat(nr)
    enddo
  else
    do i = 1,numat
      xalat(i) = xclat(i)
      yalat(i) = yclat(i)
      zalat(i) = zclat(i)
    enddo
  endif
!
!  Store charges for convergence check
!
  if (literate) then
    do i = 1,nasym
      oldqa(i) = qa(i)
    enddo
  endif
!
!  Generate electric field potential
!
  if (lfieldcfg(ncf).or.(ntdfieldcfg(ncf).gt.0)) then
    vfield(1:numat) = 0.0_dp
    call electricfieldpotl(vfield)
  endif
!****************************
!  Start of iterative loop  *
!****************************
  lconverged = .false.
  niter = 0
  if (literate.and.lmain.and.ioproc) then
    write(ioout,'(''  Iterative solution of QEq :'',/)')
  endif
  do while (niter.lt.nitereem.and..not.lconverged)
    niter = niter + 1
!
!  Zero right hand vector
!
    z(1:neem) = 0.0_dp
!*******************************************
!  Generate electrostatic potential terms  *
!*******************************************
    if (lsymopt) then
      call sympot(derv2,maxd2,z,1_i4)
    else
      call genpot(derv2,maxd2,z,1_i4)
    endif
!
!  From S & M, where z has been set without reference to neem, reduce
!  elements to those that are needed
!
    if (lSandM) then
      do i = 1,neem
        z2(i) = z(neemptr(i))
      enddo
      z(1:neem) = z2(1:neem)
    endif
!
!  Reduce to nasym x nasym form
!
    do i = 1,neem
!
!  Zero storage vector for derv2 array
!
      do j = 1,numat
        z2(j) = 0.0_dp
      enddo
!
!  Place i-j potential terms into derv2
!
      do j = 1,numat
        k = nrelat(j)
        jj = 1
        kk = neemptr(jj)
        do while (jj.lt.neem.and.kk.ne.k)
          jj = jj + 1
          kk = neemptr(jj)
        enddo
!
!  Variable j charge case
!
        if (kk.eq.k) then
          z2(k) = z2(k) + derv2(j,neemptr(i))*occuf(j)
        else
          z(i) = z(i) - qf(j)*derv2(j,neemptr(i))*occuf(j)
        endif
      enddo
!
!  Copy temporary storage vector back into derv2 array
!
      do j = 1,numat
        derv2(j,i) = z2(j)
      enddo
    enddo
!********************************
!  Form matrix of coefficients  *
!********************************
    if (lqeq) then
      do i = 1,neem
        ii = neemptr(i)
        if (iatn(ii).ne.1) then
          derv2(i,i) = derv2(i,i) + 2.0_dp*qeqmu(iatn(ii))*occua(ii)
        else
!
!  For hydrogen charge dependant factor must be introduced
!
          rjfac = 1.0_dp+(qa(ii)/zetah0)
          derv2(i,i) = derv2(i,i) + 2.0_dp*qeqmu(1)*occua(ii)*rjfac
        endif
      enddo
    elseif (lSandM) then
      do i = 1,neem
        ii = neemptr(i)
        derv2(i,i) = derv2(i,i) + 2.0_dp*smmu(iatn(ii))*occua(ii)
      enddo
    elseif (lpacha) then
      do i = 1,neem
        ii = neemptr(i)
        derv2(i,i) = derv2(i,i) + 2.0_dp*mu_pacha(iatn(ii))*occua(ii)
      enddo
    else
      do i = 1,neem
        ii = neemptr(i)
        derv2(i,i) = derv2(i,i) + 2.0_dp*rmu(iatn(ii))*occua(ii)
      enddo
    endif
    derv2(nasym+1,nasym+1) = 0.0_dp
    do i = 1,neem
      ii = neemptr(i)
      derv2(i,neem+1) = dble(neqv(ii))*occua(ii)
      derv2(neem+1,i) = 1.0_dp
    enddo
    derv2(neem+1,neem+1) = 0.0_dp
    if (lqeq) then
      do i = 1,neem
        ii = neemptr(i)
        ni = iatn(ii)
        z(i) = z(i) - qeqchi(ni) + 2.0_dp*qeqmu(ni)*qeqq0(ni)
      enddo
    elseif (lSandM) then
      do i = 1,neem
        ii = neemptr(i)
        ni = iatn(ii)
        z(i) = z(i) - smchi(ni) + 2.0_dp*smmu(ni)*smq0(ni)
      enddo
    elseif (lpacha) then
      do i = 1,neem
        ii = neemptr(i)
        ni = iatn(ii)
        z(i) = z(i) - chi_pacha(ni) + 2.0_dp*mu_pacha(ni)*q0_pacha(ni)
      enddo
    else
      do i = 1,neem
        ii = neemptr(i)
        ni = iatn(ii)
        z(i) = z(i) - chi(ni) + 2.0_dp*rmu(ni)*q0(ni)
      enddo
    endif
    if (lfieldcfg(ncf).or.(ntdfieldcfg(ncf).gt.0)) then
      do i = 1,neem
        ii = neemptr(i)
        z(i) = z(i) - vfield(ii)
      enddo
    endif
    z(neem+1) = - qtot
!
!  Debugging output
!
    if (index(keyword,'debu').ne.0.and.ioproc) then
      write(ioout,'(/,''  EEM/QEq Matrix :'',/)')
      do i = 1,neem + 1
        write(ioout,'(10(1x,f9.5))')(derv2(j,i),j=1,neem+1),z(i)
      enddo
    endif
!******************
!  Invert matrix  *
!******************
    ifail = 0
    n = neem + 1
    if (lsymopt) then
!*************************
!  Asymmetric inversion  *
!*************************
!     
!  Allocate workspace for inversion
!     
      lwrk = n*ilaenv(1_i4,'DGETRI',' ',n,-1_i4,-1_i4,-1_i4)
      allocate(ipivot(n),stat=status)
      if (status/=0) call outofmemory('eem','ipivot')
      allocate(wrk(lwrk),stat=status)
      if (status/=0) call outofmemory('eem','wrk')
!     
!  Factorise matrix
!     
      call dgetrf(n,n,derv2,maxd2,ipivot,ifail)
      if (ifail.eq.0) then
!     
!  Form inverse
!     
        call dgetri(n,derv2,maxd2,ipivot,wrk,lwrk,ifail)
      endif
!     
!  Free workspace  
!     
      deallocate(wrk,stat=status)
      if (status/=0) call deallocate_error('eem','wrk')
      deallocate(ipivot,stat=status)  
      if (status/=0) call deallocate_error('eem','ipivot')
    else
!************************
!  Symmetric inversion  *
!************************
!     
!  Allocate workspace for inversion
!     
      allocate(dpacked(n*(n+1)/2),stat=status)
      if (status/=0) call outofmemory('eem','dpacked')
      allocate(ipivot(n),stat=status)
      if (status/=0) call outofmemory('eem','ipivot')
      allocate(wrk(3*n),stat=status)
      if (status/=0) call outofmemory('eem','wrk')
!
!  Transfer data to packed storage
!
      k = 0
      do i = 1,n
        do j = 1,i
          k = k + 1
          dpacked(k) = derv2(j,i)
        enddo
      enddo
!     
!  Factorise matrix
!     
      call dsptrf('U',n,dpacked,ipivot,ifail)
      if (ifail.eq.0) then
!     
!  Form inverse
!     
        call dsptri('U',n,dpacked,ipivot,wrk,ifail)
!
!  Transfer data back
!
        k = 0
        do i = 1,n
          do j = 1,i
            k = k + 1
            derv2(j,i) = dpacked(k)
            derv2(i,j) = dpacked(k)
          enddo
        enddo
      endif
!     
!  Free workspace  
!     
      deallocate(wrk,stat=status)
      if (status/=0) call deallocate_error('eem','wrk')
      deallocate(ipivot,stat=status)  
      if (status/=0) call deallocate_error('eem','ipivot')
      deallocate(dpacked,stat=status)
      if (status/=0) call deallocate_error('eem','dpacked')
    endif
!
!  Was inversion successful?
!
    if (ifail.ne.0) then
      call outerror('matrix inversion failed in EEM/QEq',0_i4)
      call stopnow('eem')
    endif
!  
!  Multiply inverse matrix and chi matrix to get charges
!
    do i = 1,neem + 1
      ii = neemptr(i)
      qf(ii) = 0.0_dp
      do j = 1,neem + 1
        qf(ii) = qf(ii) + z(j)*derv2(j,i)
      enddo
    enddo
    do i = 1,neem
      ii = neemptr(i)
      qa(ii) = qf(ii)
    enddo
    enega = - qf(nasym+1)
    if (literate) then
!
!  Check for convergence
!
      qdiff = 0.0_dp
      do i = 1,nasym
        qd = qa(i) - oldqa(i)
        qdiff = qdiff + abs(qd)
      enddo
      qdiff = qdiff/dble(nasym)
      lconverged = (qdiff.lt.qeqscfcrit)
      if (lmain.and.ioproc) then
        write(ioout,'(''  ** Cycle : '',i4,'' Qdiff : '',f10.8)') niter,qdiff
      endif
      if (.not.lconverged) then
!
!  Damp change to improve convergence
!
        do i = 1,neem
          ii = neemptr(i)
          qd = qa(ii) - oldqa(ii)
          qa(ii) = qa(ii) - 0.25_dp*qd
          oldqa(ii) = qa(ii)
        enddo
      endif
    endif
!
!  Transfer charges to qf
!
    do i = 1,numat
      nr = nrelat(i)
      qf(i) = qa(nr)
    enddo
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
!**************************
!  Calculate self energy  *
!**************************
  eself = 0.0_dp
  do i = 1,neem
    ii = neemptr(i)
    qi = qa(ii)
    ni = iatn(ii)
    reqv = dble(neqv(ii))*occua(ii)
    eself_before = eself
    if (lqeq) then
      q0i = qeqq0(ni)
      if (ni.ne.1) then
        eself = eself + (qi-q0i)*reqv*(qeqchi(ni)+(qi-q0i)*qeqmu(ni))
      else
        eself = eself + (qi-q0i)*reqv*(qeqchi(ni)+(qi-q0i)*qeqmu(ni)*(1.0_dp+(2.0_dp*(qi-q0i)/(3.0_dp*zetah0))))
      endif
    elseif (lSandM) then
      q0i = smq0(ni)
      eself = eself + (qi-q0i)*reqv*(smchi(ni)+(qi-q0i)*smmu(ni))
    elseif (lpacha) then
      q0i = q0_pacha(ni)
      eself = eself + (qi-q0i)*reqv*(chi_pacha(ni)+(qi-q0i)*mu_pacha(ni))
    else
      q0i = q0(ni)
      eself = eself + (qi-q0i)*reqv*(chi(ni)+(qi-q0i)*rmu(ni))
    endif
!
    nregioni = nregionno(nsft+ii)
    eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + eself - eself_before
!
    siteenergy(i) = siteenergy(i) + eself - eself_before 
  enddo
!*********************************
!  Calculate charge derivatives  *
!*********************************
  if (lgrad2.or.(lgrad1.and.(lfieldcfg(ncf).or.(ntdfieldcfg(ncf).gt.0))).or.lDoChargeDeriv) then
    if (lsymopt.and.lmain) then
      call dcharges(lmain)
    else
      call dcharge(lmain,lsymopt,.true.)
    endif
  endif
!
!  For Pacha we can also compute approximate NMR shifts for relevant nuclei
!
  if (lpacha) then
    allocate(qnmr(numat),stat=status)
    if (status/=0) call outofmemory('eem','qnmr')
    call getnmr(nasym,iatn,qa,qnmr)
  endif
!*******************
!  Output results  *
!*******************
  if ((lmain.or.index(keyword,'debu').ne.0).and.ioproc) then
    if (lqeq) then
      write(ioout,'(//,''  Final charges from QEq :'',/)')
      if (literate) then
        if (lconverged) then
          write(ioout,'(''  Charges converged in '',i3,'' iterations'',/)')niter
        else
          write(ioout,'(''  Failed to converged after '',i3,'' iterations'',/)')nitereem
        endif
      else
        write(ioout,'(''  No hydrogens present - no iteration needed'',/)')
      endif
    elseif (lpacha) then
      write(ioout,'(//,''  Final charges from PACHA-EEM :'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''    Atom no.            Atomic No.             Charge       Chemical shift'')')
      write(ioout,'(''                                                 (e)              (ppm)   '')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,nasym
        write(ioout,'(6x,i4,18x,i2,16x,f10.7,2x,f10.4)') i,iatn(i),qa(i),qnmr(i)
      enddo
    else
      write(ioout,'(//,''  Final charges from EEM :'',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      write(ioout,'(''    Atom no.            Atomic No.             Charge'')')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
      do i = 1,nasym
        write(ioout,'(6x,i4,18x,i2,16x,f10.7)') i,iatn(i),qa(i)
      enddo
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''  Electronegativity = '',f16.6,'' eV'')') enega
    write(ioout,'(''  Self energy       = '',f16.6,'' eV'')') eself
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
!
!  Free local memory 
!
  if (lpacha) then
    deallocate(qnmr,stat=status)
    if (status/=0) call deallocate_error('eem','qnmr')
  endif
  if (lfieldcfg(ncf).or.(ntdfieldcfg(ncf).gt.0)) then
    deallocate(vfield,stat=status)
    if (status/=0) call deallocate_error('eem','vfield')
  endif
  if (literate) then
    deallocate(oldqa,stat=status)
    if (status/=0) call deallocate_error('eem','oldqa')
  endif
  deallocate(z2,stat=status)
  if (status/=0) call deallocate_error('eem','z2')
  deallocate(z,stat=status)
  if (status/=0) call deallocate_error('eem','z')
  deallocate(leemfoc,stat=status)
  if (status/=0) call deallocate_error('eem','leemfoc')
!
!  Timing
!
  time2 = cputime()
  teem = teem + time2 - time1
!
  return
  end
