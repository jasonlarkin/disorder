  subroutine dcharge(lprint,lrecalcA,lstrain)
!
!  Calculates the first derivative of the charge with respect
!  to the coordinates of the atoms.
!
!  12/97 Created from dgenpot
!   1/98 Strain derivatives added - note that derv3 is used as
!        scratch storage in this algorithm
!   6/99 lstrain added to protect storage in derv3 for phonon
!        calculations
!   2/01 Modifications made for general dimensionality
!  10/02 ReaxFF modifications added
!  11/02 1-D case moved to cluster section
!   1/03 Wolf sum modifications made
!   3/03 Corrections made for frozen regions with fixed charges
!   9/04 Order of terms in dqds switched
!   9/04 Terms initialised before call to qmatrix1D
!   7/05 Extra argument to genpot added
!   7/05 Streitz and Mintmire modifications added
!  12/05 Handling of zero charge case added
!  11/07 Handling of noelectrostatic case added
!  11/07 Memory allocation moved to before initialisation
!  12/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!   9/11 Electric field modifications added
!
!  On entry:
!
!    derv2    = inverse matrix calculated during EEM/QEq
!    lprint   = logical indicating whether printed output is wanted
!    lrecalcA = if .true. this forces the recalculation of A
!    lstrain  = if .true. then strain derivatives are calculated
!               and derv3 is overwritten
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, September 2011
!
  use configurations, only : nregionno
  use constants
  use control
  use current
  use derivatives,     dqs => derv3 
  use element
  use field,          only : lfieldcfg
  use general,        only : cutw, etaw
  use iochannels
  use kspace
  use parallel
  use qmedata
  use shell
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lprint
  logical,     intent(in)                      :: lrecalcA
  logical,     intent(in)                      :: lstrain
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ieem
  integer(i4)                                  :: ifail
  integer(i4)                                  :: igroup
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4), dimension(:), allocatable       :: ipivot
  integer(i4)                                  :: iresid
  integer(i4)                                  :: iv
  integer(i4)                                  :: j
  integer(i4)                                  :: jeem
  integer(i4)                                  :: jj
  integer(i4)                                  :: jx
  integer(i4)                                  :: jy
  integer(i4)                                  :: jz
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: ml1
  integer(i4)                                  :: ml2
  integer(i4)                                  :: ml3
  integer(i4)                                  :: n
  integer(i4)                                  :: neemfull
  integer(i4), dimension(:), allocatable       :: neemfullptr
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: npqni
  integer(i4)                                  :: npqnj
  integer(i4)                                  :: status
  logical                                      :: lhi
  logical                                      :: lhj
  logical                                      :: lhpresent
  real(dp)                                     :: accf2
  real(dp)                                     :: arg
  real(dp)                                     :: argtest
  real(dp),    dimension(:), allocatable       :: chitmp
  real(dp)                                     :: cosa
  real(dp)                                     :: costrm
  real(dp)                                     :: cuts2
  real(dp)                                     :: d2zetaii
  real(dp)                                     :: d2zetaij
  real(dp)                                     :: d2zetajj
  real(dp)                                     :: d2zetari
  real(dp)                                     :: d2zetarj
  real(dp)                                     :: d2gamr2
  real(dp)                                     :: d2gamifj
  real(dp)                                     :: d2gamjfi
  real(dp)                                     :: darg1
  real(dp)                                     :: darg2
  real(dp)                                     :: dchis(6)
  real(dp),    dimension(:), allocatable       :: dchix
  real(dp),    dimension(:), allocatable       :: dchiy
  real(dp),    dimension(:), allocatable       :: dchiz
  real(dp)                                     :: derf
  real(dp)                                     :: derfc
  real(dp)                                     :: derfc1
  real(dp)                                     :: derfc2
  real(dp)                                     :: derfez
  real(dp)                                     :: dexp1
  real(dp)                                     :: dexp2
  real(dp)                                     :: dexp3
  real(dp)                                     :: dexp4
  real(dp)                                     :: dexpz
  real(dp)                                     :: dqme(3)
  real(dp)                                     :: d2qme(6)
  real(dp)                                     :: dgam
  real(dp)                                     :: dgamifj
  real(dp)                                     :: dgamjfi
  real(dp),    dimension(:), allocatable       :: dpacked
  real(dp),    dimension(:), allocatable       :: dqx
  real(dp),    dimension(:), allocatable       :: dqy
  real(dp),    dimension(:), allocatable       :: dqz
  real(dp)                                     :: dtrm1
  real(dp)                                     :: dtrm1i
  real(dp)                                     :: dtrm1zn
  real(dp)                                     :: dtrm1j
  real(dp)                                     :: dzetai
  real(dp)                                     :: dzetaj
  real(dp)                                     :: errfcn
  real(dp)                                     :: etaloc
  real(dp)                                     :: etaz
  real(dp)                                     :: etaz2
  real(dp)                                     :: etrm
  real(dp)                                     :: fieldx
  real(dp)                                     :: fieldy
  real(dp)                                     :: fieldz
  real(dp)                                     :: gam
  real(dp)                                     :: gamifj
  real(dp)                                     :: gamjfi
  real(dp)                                     :: Gmax
  real(dp)                                     :: kexperfc
  real(dp)                                     :: kvec
  real(dp)                                     :: qme
  real(dp)                                     :: qli
  real(dp)                                     :: qlii
  real(dp)                                     :: qlj
  real(dp)                                     :: qljj
  real(dp)                                     :: rconv
  real(dp)                                     :: rexp
  real(dp)                                     :: rjfac
  real(dp)                                     :: rkvec
  real(dp)                                     :: rl
  real(dp)                                     :: rmax
  real(dp)                                     :: rqeq2
  real(dp)                                     :: rr2
  real(dp)                                     :: rrr
  real(dp)                                     :: rv2
  real(dp)                                     :: rx
  real(dp)                                     :: rxi
  real(dp)                                     :: rxj
  real(dp)                                     :: rxk
  real(dp)                                     :: ry
  real(dp)                                     :: ry2
  real(dp)                                     :: ryi
  real(dp)                                     :: ryj
  real(dp)                                     :: ryk
  real(dp)                                     :: rz
  real(dp)                                     :: rz2
  real(dp)                                     :: rzi
  real(dp)                                     :: rzj
  real(dp)                                     :: rzk
  real(dp)                                     :: setaloc
  real(dp)                                     :: sina
  real(dp)                                     :: sineq
  real(dp)                                     :: smallestG
  real(dp)                                     :: strm
  real(dp)                                     :: strm1
  real(dp)                                     :: strm2
  real(dp)                                     :: sum
  real(dp)                                     :: sumx
  real(dp)                                     :: sumy
  real(dp)                                     :: sumz
  real(dp)                                     :: trmi
  real(dp),    dimension(:), allocatable       :: wrk
  real(dp)                                     :: xci
  real(dp)                                     :: yci
  real(dp)                                     :: zci
  real(dp)                                     :: xd
  real(dp)                                     :: yd
  real(dp)                                     :: zd
  real(dp)                                     :: zetah0
  real(dp)                                     :: zetai
  real(dp)                                     :: zetaj
  real(dp)                                     :: znucj
  real(dp)                                     :: ztrm1
!
!  Check the memory for the square arrays
!
  if (numat+1.gt.maxd2u) then
    maxd2u = numat + 1
    call changemaxd2
  endif
  if (numat+1.gt.maxd2) then
    maxd2 = numat + 1
    call changemaxd2
  endif
!
!  Check the memory for the charge derivatives
!
  if (numat.gt.maxd2qu) then
    maxd2qu = numat
    call changemaxd2q
  endif
  if (3*numat.gt.maxd2q) then
    maxd2q = 3*numat
    call changemaxd2q
  endif
!
!  Zero dq/dxyz and dq/ds
!
  do i = 1,numat
    do j = 1,3*numat
      dqdxyz(j,i) = 0.0_dp
    enddo
  enddo
  if (ndim.gt.0) then
    do i = 1,numat
      do j = 1,nstrains
        dqds(j,i) = 0.0_dp
      enddo
    enddo
  endif
!
!  If electrostatics have been turned off then there is no point in proceeding.
!
  if (.not.lDoElectrostatics) return
!
  cuts2 = cuts*cuts
  rconv = 1.0_dp/autoangs
!
!  Is this QEq with H present?
!
  lhpresent = .false.
  if (lqeq) then
    i = 0
    do while (.not.lhpresent.and.i.lt.numat)
      i = i + 1
      if (nat(i).eq.1) lhpresent = .true.
    enddo
  endif
!
!  Find number of EEM active atoms
!
  allocate(neemfullptr(numat),stat=status)
  if (status/=0) call outofmemory('dcharge','neemfullptr')
  neemfull = 0
  do i = 1,numat
    if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
      neemfull = neemfull + 1
      neemfullptr(neemfull) = i
    endif
  enddo
  if (lfieldcfg(ncf)) then
!
!  Get electric field if needed
!
    call electricfieldparts(fieldx,fieldy,fieldz)
  endif
  if ((lqeq.and.lhpresent).or.lrecalcA) then
!*************************************
!  Hydrogen/QEq case or Symopt case  *
!*************************************
    allocate(chitmp(numat),stat=status)
    if (status/=0) call outofmemory('dcharge','chitmp')
!
!  If hydrogen is present then we need to calculate A + (dA/dq).q
!  invert this matrix to use in place of A**-1 below.
!
!  First generate 2 centre terms
!
    chitmp(1:numat) = 0.0_dp
    if (lqeq.and.lhpresent) then
      call genpot(derv2,maxd2,chitmp,2_i4)
    else
      call genpot(derv2,maxd2,chitmp,1_i4)
    endif
!
!  Reduce potential contributions to neemfull x neemfull
!
    ieem = 0
    do i = 1,numat
      if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
        ieem = ieem + 1
        jeem = 0
        do j = 1,numat
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            jeem = jeem + 1
            derv2(jeem,ieem) = derv2(j,i)
          endif
        enddo
      endif
    enddo
!
!  Add one centre terms
!
    zetah0 = autoangs*0.75_dp/qeqrad(1)
    if (lqeq) then
      ieem = 0
      do i = 1,numat
        if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
          ieem = ieem + 1
          if (nat(i).ne.1) then
            derv2(ieem,ieem) = derv2(ieem,ieem) + 2.0_dp*qeqmu(nat(i))*occuf(i)
          else
!
!  For hydrogen charge dependant factor must be introduced
!  which includes both Aii and dAii/dqi
!
            rjfac = 1.0_dp + (2.0_dp*qf(i)/zetah0)
            derv2(ieem,ieem) = derv2(ieem,ieem) + 2.0_dp*qeqmu(1)*occuf(i)*rjfac
          endif 
        endif 
      enddo
    elseif (lSandM) then
      ieem = 0
      do i = 1,numat
        if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
          ieem = ieem + 1
          derv2(ieem,ieem) = derv2(ieem,ieem) + 2.0_dp*smmu(nat(i))*occuf(i)
        endif
      enddo
    else
      ieem = 0
      do i = 1,numat
        if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
          ieem = ieem + 1
          derv2(ieem,ieem) = derv2(ieem,ieem) + 2.0_dp*rmu(nat(i))*occuf(i)
        endif
      enddo
    endif
!
!  Complete constraint terms
!
    ieem = 0
    do i = 1,numat
      if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
        ieem = ieem + 1
        derv2(ieem,neemfull+1) = occuf(i)
        derv2(neemfull+1,ieem) = 1.0_dp
      endif
    enddo
    derv2(neemfull+1,neemfull+1) = 0.0_dp
!     
!  Allocate workspace for inversion
!     
    n = neemfull + 1
    allocate(dpacked(n*(n+1)/2),stat=status)
    if (status/=0) call outofmemory('dcharge','dpacked')
    allocate(ipivot(n),stat=status)
    if (status/=0) call outofmemory('dcharge','ipivot')
    allocate(wrk(3*n),stat=status)
    if (status/=0) call outofmemory('dcharge','wrk')
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
    if (status/=0) call deallocate_error('dcharge','wrk')
    deallocate(ipivot,stat=status)  
    if (status/=0) call deallocate_error('dcharge','ipivot')
    deallocate(dpacked,stat=status)
    if (status/=0) call deallocate_error('dcharge','dpacked')
!
!  Was inversion successful?
!
    if (ifail.ne.0) then
      call outerror('matrix inversion failed in dcharge',0_i4)
      call stopnow('eem')
    endif
!
    deallocate(chitmp,stat=status)
    if (status/=0) call deallocate_error('dcharge','chitmp')
!**************************
!  End hydrogen/QEq case  *
!**************************
  endif
  rqeq2 = rqeq*rqeq
!
!  Allocate local memory
!
  allocate(dqx(numat),stat=status)
  if (status/=0) call outofmemory('dcharge','dqx')
  allocate(dqy(numat),stat=status)
  if (status/=0) call outofmemory('dcharge','dqy')
  allocate(dqz(numat),stat=status)
  if (status/=0) call outofmemory('dcharge','dqz')
  if (lSandM) then
    allocate(dchix(numat),stat=status)
    if (status/=0) call outofmemory('dcharge','chix')
    allocate(dchiy(numat),stat=status)
    if (status/=0) call outofmemory('dcharge','chiy')
    allocate(dchiz(numat),stat=status)
    if (status/=0) call outofmemory('dcharge','chiz')
  endif
  if (lstrain) then
!
!  Zero temporary strain derivative arrays
!
    do i = 1,nstrains
      do j = 1,numat
        dqs(j,i) = 0.0_dp
      enddo
    enddo
  endif
  if (ndim.gt.1.or.lwolf) then
!******************
!  Periodic case  *
!******************
!
!  Define constants
!
    if (lwolf) then
      radmax = cutw
      etaloc = etaw*etaw
      setaloc = etaw
    elseif (lewald) then
      if (ndim.eq.2) then
        rpieta = 1.0_dp / sqrt(pi * eta)
        rhseta = 0.5_dp / seta
        accf2 = accf*accf
        argtest = sqrt(3.0+0.5*accf2) - sqrt(3.0)
        smallestG = min(kv(1,1),kv(2,2))
      endif
      radmax = accf/seta
      eta4 = 0.25_dp/eta
      etaloc = eta
      setaloc = seta
    else
      radmax = 0.0_dp
      eta4 = 0.25_dp
      etaloc = eta
      setaloc = seta
    endif
!
    if (lqeq.or.lSandM) then
      rmax = max(radmax,rqeq)
    else
      rmax = radmax
    endif
    rmax2 = rmax*rmax
!
!  Estimate upper limits for looping
!
    if (ndim.eq.3) then
      rv2 = rv(1,1)**2 + rv(2,1)**2 + rv(3,1)**2
      rv2 = sqrt(rv2)
      ml1 = rmax/rv2 + 1
      rv2 = rv(1,2)**2 + rv(2,2)**2 + rv(3,2)**2
      rv2 = sqrt(rv2)
      ml2 = rmax/rv2 + 1
      rv2 = rv(1,3)**2 + rv(2,3)**2 + rv(3,3)**2
      rv2 = sqrt(rv2)
      ml3 = rmax/rv2 + 1
    elseif (ndim.eq.2) then
      rv2 = rv(1,1)**2 + rv(2,1)**2
      rv2 = sqrt(rv2)
      ml1 = rmax/rv2 + 1
      rv2 = rv(1,2)**2 + rv(2,2)**2
      rv2 = sqrt(rv2)
      ml2 = rmax/rv2 + 1
      ml3 = 0
    elseif (ndim.eq.1) then
      rv2 = rv(1,1)
      ml1 = rmax/rv2 + 1
      ml2 = 0
      ml3 = 0
    elseif (ndim.eq.0) then
      ml1 = 0
      ml2 = 0
      ml3 = 0
    endif
!**********************************
!  Reciprocal space contribution  *
!**********************************
    if (lnorecip.or.lwolf) goto 5
!
!  Start loop over atoms for coordinate differentiation
!
    ieem = 0
    do i = 1,numat
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      qli = qf(i)
      ind = 3*(i - 1)
      if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) ieem = ieem + 1
!
!  Zero temporary derivative arrays
!
      do j = 1,numat
        dqx(j) = 0.0_dp
        dqy(j) = 0.0_dp
        dqz(j) = 0.0_dp
      enddo
      jeem = 0
      do j = 1,numat
        xd = xclat(j) - xci
        yd = yclat(j) - yci
        zd = zclat(j) - zci
        qlj = qf(j)
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) jeem = jeem + 1
!
!  Evaluate derivative of reciprocal space elements of A
!
        if (ndim.eq.3) then
          do iv = 1,nkvec
            argc(iv) = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
            sine(iv) = sin(argc(iv))*ktrm(iv)
            if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
              dqx(ieem) = dqx(ieem) + sine(iv)*xrk(iv)*qlj
              dqy(ieem) = dqy(ieem) + sine(iv)*yrk(iv)*qlj
              dqz(ieem) = dqz(ieem) + sine(iv)*zrk(iv)*qlj
            endif
            if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
              dqx(jeem) = dqx(jeem) + sine(iv)*xrk(iv)*qli
              dqy(jeem) = dqy(jeem) + sine(iv)*yrk(iv)*qli
              dqz(jeem) = dqz(jeem) + sine(iv)*zrk(iv)*qli
            endif
            if (lstrain) then
              if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                costrm = cos(argc(iv))*angstoev*qlj
                strm1 = costrm*ktrms(iv)
                strm2 = costrm*ktrm(iv)
                dqs(ieem,1) = dqs(ieem,1) - strm1*xrk(iv)*xrk(iv) - strm2
                dqs(ieem,2) = dqs(ieem,2) - strm1*yrk(iv)*yrk(iv) - strm2
                dqs(ieem,3) = dqs(ieem,3) - strm1*zrk(iv)*zrk(iv) - strm2
                dqs(ieem,4) = dqs(ieem,4) - strm1*yrk(iv)*zrk(iv)
                dqs(ieem,5) = dqs(ieem,5) - strm1*xrk(iv)*zrk(iv)
                dqs(ieem,6) = dqs(ieem,6) - strm1*xrk(iv)*yrk(iv)
              endif
            endif
          enddo
        elseif (ndim.eq.2) then
!
!  First term - K vector independent
!
          etaz = seta*zd
          etaz2 = etaz*etaz
          derfez = derf(etaz)
          dexpz  = exp(-etaz2)
          etrm   = - vol4pi*(zd*derfez + dexpz*rpieta)*angstoev
          dtrm1  = - vol4pi*derfez
          if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
            dqz(ieem) = dqz(ieem) - dtrm1*qlj
          endif
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            dqz(jeem) = dqz(jeem) - dtrm1*qli
          endif
          if (lstrain) then
            if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
              dqs(ieem,1) = dqs(ieem,1) - etrm*qlj
              dqs(ieem,2) = dqs(ieem,2) - etrm*qlj
            endif
          endif
!
!  Find local kvector cut-off
!
          if (abs(etaz).gt.argtest) then
            Gmax = abs(accf2/zd)
          else
            Gmax = sqrt(4.0*eta*(accf2-etaz2))
          endif
          if (Gmax.ge.smallestG) then
            do iv = 1,nkvec
              kvec = kmod(iv)
              if (kvec.le.Gmax) then
                arg = xrk(iv)*xd + yrk(iv)*yd
                sina = sin(arg)*ktrm(iv)
                cosa = cos(arg)*ktrm(iv)
                dexp1 = exp(kvec*zd)
                dexp2 = 1.0_dp/dexp1
                darg1 = kvec*rhseta + etaz
                darg2 = kvec*rhseta - etaz
                dexp3 = exp(-(darg1)**2)
                dexp4 = exp(-(darg2)**2)
                derfc1 = derfc(darg1)
                derfc2 = derfc(darg2)
                kexperfc = dexp1*derfc1 + dexp2*derfc2
                sineq = sina*kexperfc
                ztrm1 = (kvec*(dexp1*derfc1-dexp2*derfc2) - tweatpi*(dexp1*dexp3-dexp2*dexp4))
                if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                  dqx(ieem) = dqx(ieem) + sineq*xrk(iv)*qlj
                  dqy(ieem) = dqy(ieem) + sineq*yrk(iv)*qlj
                  dqz(ieem) = dqz(ieem) - cosa*ztrm1*qlj
                endif
                if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
                  dqx(jeem) = dqx(jeem) + sineq*xrk(iv)*qli
                  dqy(jeem) = dqy(jeem) + sineq*yrk(iv)*qli
                  dqz(jeem) = dqz(jeem) - cosa*ztrm1*qli
                endif
                if (lstrain) then
                  if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                    costrm = cosa*angstoev*qlj
                    rkvec = 1.0_dp/kvec
                    strm = rkvec*(-rkvec*kexperfc + zd*(dexp1*derfc1-dexp2*derfc2) - &
                           rpieta*(dexp1*dexp3+dexp2*dexp4))
                    strm1 = costrm*strm
                    strm2 = costrm*kexperfc
                    dqs(ieem,1) = dqs(ieem,1) - strm1*xrk(iv)*xrk(iv) - strm2
                    dqs(ieem,2) = dqs(ieem,2) - strm1*yrk(iv)*yrk(iv) - strm2
                    dqs(ieem,3) = dqs(ieem,3) - strm1*xrk(iv)*yrk(iv)                                                    
                  endif
                endif
              endif
            enddo
          endif
        endif
!
!  End inner atom loop
!
      enddo
!
!  Multiply dqx/dqy/dqz by inverse matrix
!
      jeem = 0
      do j = 1,numat
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
          jeem = jeem + 1
          sumx = 0.0_dp
          sumy = 0.0_dp
          sumz = 0.0_dp
          do k = 1,neemfull
            sumx = sumx + dqx(k)*derv2(k,jeem)
            sumy = sumy + dqy(k)*derv2(k,jeem)
            sumz = sumz + dqz(k)*derv2(k,jeem)
          enddo
          sumx = sumx*angstoev
          sumy = sumy*angstoev
          sumz = sumz*angstoev
          dqdxyz(ind+1,j) = dqdxyz(ind+1,j) - sumx
          dqdxyz(ind+2,j) = dqdxyz(ind+2,j) - sumy
          dqdxyz(ind+3,j) = dqdxyz(ind+3,j) - sumz
        endif
      enddo
    enddo
5   continue
!*************************
!  Real space summation  *
!*************************
    if (lnoreal.and.lstrain) then
!
!  Strain terms
!
      jeem = 0
      do j = 1,numat
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
          jeem = jeem + 1
          do kl = 1,nstrains
            sum = 0.0_dp
            do k = 1,neemfull
              sum = sum + dqs(k,kl)*derv2(k,jeem)
            enddo
            dqds(kl,j) = dqds(kl,j) - sum
          enddo
        endif
      enddo
      goto 135
    endif
!
!  Start loop over atoms for coordinate differentiation
!
    ieem = 0
    do i = 1,numat
      qlii = qf(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      ind = 3*(i-1)
      ni = nat(i)
      if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) ieem = ieem + 1
!
!  If QEq work out principle quantum number
!
      if (lqeq) then
        if (ni.le.2) then
          npqni = 1
        elseif (ni.le.10) then
          npqni = 2
        elseif (ni.le.18) then
          npqni = 3
        elseif (ni.le.36) then
          npqni = 4
        elseif (ni.le.54) then
          npqni = 5
        elseif (ni.le.86) then
          npqni = 6
        else
          npqni = 7
        endif
        zetai = 0.5_dp*qeqlambda*(2*npqni+1)/qeqrad(ni)
        lhi = (ni.eq.1)
        if (lhi) then
!
!  Special case for hydrogen
!
          zetai = zetai + qlii*rconv
        endif
      elseif (lSandM) then   
        zetai = smzeta(ni)
      endif
!
!  Zero temporary derivative arrays
!
      do j = 1,numat
        dqx(j) = 0.0_dp
        dqy(j) = 0.0_dp
        dqz(j) = 0.0_dp
      enddo
      if (lSandM) then
        dchix(1:numat) = 0.0_dp
        dchiy(1:numat) = 0.0_dp
        dchiz(1:numat) = 0.0_dp
        dchis(1:6) = 0.0_dp
      endif
      jeem = 0
      do j = 1,numat
        qljj = qf(j)
        qlj = qljj*occuf(j)
        rx = xclat(j) - xci
        ry = yclat(j) - yci
        rz = zclat(j) - zci
        nj = nat(j)
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) jeem = jeem + 1
!
!  If QEq work out principle quantum number
!
        if (lqeq) then
          if (nj.le.2) then
            npqnj = 1
          elseif (nj.le.10) then
            npqnj = 2
          elseif (nj.le.18) then
            npqnj = 3
          elseif (nj.le.36) then
            npqnj = 4
          elseif (nj.le.54) then
            npqnj = 5
          elseif (nj.le.86) then
            npqnj = 6
          else
            npqnj = 7
          endif
          zetaj = 0.5_dp*qeqlambda*(2*npqnj+1)/qeqrad(nj)
          lhj = (nj.eq.1)
          if (lhj) then
!
!  Special case for hydrogen
!
            zetaj = zetaj + qljj*rconv
          endif
        elseif (lSandM) then
          zetaj = smzeta(nj)
          znucj = smZnuc(nj)
        endif
!
!  Loop over cell vectors
!
        rxi = rx - (ml1 + 1)*r1x
        ryi = ry - (ml1 + 1)*r1y
        rzi = rz - (ml1 + 1)*r1z
        do ii = -ml1,ml1
          rxi = rxi + r1x
          ryi = ryi + r1y
          rzi = rzi + r1z
          rxj = rxi - (ml2+1)*r2x
          ryj = ryi - (ml2+1)*r2y
          rzj = rzi - (ml2+1)*r2z
          do jj = -ml2,ml2
            rxj = rxj + r2x
            ryj = ryj + r2y
            rzj = rzj + r2z
            rxk = rxj - (ml3+1)*r3x
            ryk = ryj - (ml3+1)*r3y
            rzk = rzj - (ml3+1)*r3z
            do 120 kk = -ml3,ml3
              rxk = rxk + r3x
              ryk = ryk + r3y
              rzk = rzk + r3z
!
!  Calculate distance squared
!
              rr2 = rxk*rxk + ryk*ryk + rzk*rzk
!
!  Exclude distances outside maximum cutoff
!
              if (rr2.gt.rmax2) goto 120
!
!  Trap self term
!
              if (rr2.lt.1.0d-15) goto 120
              rl = sqrt(rr2)
              rrr = 1.0_dp/rl
              if (rr2.lt.cuts2) then
!
!  Core-shell interaction
!
                dtrm1 = rrr*rrr*rrr
                dtrm1i = dtrm1
                dtrm1j = dtrm1
                dtrm1zn = 0.0_dp
              elseif (rr2.lt.rqeq2.and.lqeq) then
!
!  Calculate Coulomb interaction according to QEq scheme and
!  subtract 1/r term from Ewald sum.
!
                call gammas(npqni,npqnj,zetai,zetaj,rl,gam,dgam,dzetai,dzetaj, &
                            d2zetaii,d2zetaij,d2zetajj,d2zetari,d2zetarj,d2gamr2)
                dtrm1 = (rrr*rrr+dgam)*rrr
                dtrm1i = dtrm1
                dtrm1j = dtrm1
                if (lhi) then
                  dtrm1i = dtrm1i + qlii*d2zetari*rrr*rconv
                endif
                if (lhj) then
                  dtrm1j = dtrm1j + qljj*d2zetarj*rrr*rconv
                endif
              elseif (rr2.lt.rqeq2.and.lSandM) then
                call gammasm(zetai,zetaj,rl,gam,dgam,d2gamr2,gamifj,gamjfi,dgamifj,dgamjfi,d2gamifj,d2gamjfi)
                dtrm1 = dgam*rrr
                dtrm1i = dtrm1
                dtrm1j = dtrm1
                dtrm1zn = znucj*(dgamjfi - dgam)*rrr
              else
                dtrm1i = 0.0_dp
                dtrm1j = 0.0_dp
                dtrm1zn = 0.0_dp
              endif
!
!  Complementary error function
!
              errfcn = derfc(setaloc*rl)
              trmi = errfcn/rl
              rexp = tweatpi*exp(-etaloc*rr2)
              dtrm1 = (trmi+rexp)*rrr*rrr
              dtrm1i = dtrm1i - dtrm1
              dtrm1j = dtrm1j - dtrm1
!
!  First derivatives of matrix elements
!
              dtrm1i = dtrm1i*angstoev
              dtrm1j = dtrm1j*angstoev
              if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                dqx(ieem) = dqx(ieem) - qljj*dtrm1i*rxk
                dqy(ieem) = dqy(ieem) - qljj*dtrm1i*ryk
                dqz(ieem) = dqz(ieem) - qljj*dtrm1i*rzk
              endif
              if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
                dqx(jeem) = dqx(jeem) - qlii*dtrm1j*rxk
                dqy(jeem) = dqy(jeem) - qlii*dtrm1j*ryk
                dqz(jeem) = dqz(jeem) - qlii*dtrm1j*rzk
              endif
              if (lSandM) then
                dtrm1zn = dtrm1zn*angstoev
                if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                  dchix(ieem) = dchix(ieem) - dtrm1zn*rxk
                  dchiy(ieem) = dchiy(ieem) - dtrm1zn*ryk
                  dchiz(ieem) = dchiz(ieem) - dtrm1zn*rzk
                endif
                if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
                  dchix(jeem) = dchix(jeem) + dtrm1zn*rxk
                  dchiy(jeem) = dchiy(jeem) + dtrm1zn*ryk
                  dchiz(jeem) = dchiz(jeem) + dtrm1zn*rzk
                endif
              endif
              if (lstrain) then
                if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
!
!  Strain
!
                  if (ndim.eq.3) then
                    dqs(ieem,1) = dqs(ieem,1) + qljj*dtrm1i*rxk*rxk
                    dqs(ieem,2) = dqs(ieem,2) + qljj*dtrm1i*ryk*ryk
                    dqs(ieem,3) = dqs(ieem,3) + qljj*dtrm1i*rzk*rzk
                    dqs(ieem,4) = dqs(ieem,4) + qljj*dtrm1i*ryk*rzk
                    dqs(ieem,5) = dqs(ieem,5) + qljj*dtrm1i*rxk*rzk
                    dqs(ieem,6) = dqs(ieem,6) + qljj*dtrm1i*rxk*ryk
                  elseif (ndim.eq.2) then
                    dqs(ieem,1) = dqs(ieem,1) + qljj*dtrm1i*rxk*rxk
                    dqs(ieem,2) = dqs(ieem,2) + qljj*dtrm1i*ryk*ryk
                    dqs(ieem,3) = dqs(ieem,3) + qljj*dtrm1i*rxk*ryk
                  endif
                  if (lSandM) then
                    if (ndim.eq.3) then
                      dchis(1) = dchis(1) + dtrm1zn*rxk*rxk
                      dchis(2) = dchis(2) + dtrm1zn*ryk*ryk
                      dchis(3) = dchis(3) + dtrm1zn*rzk*rzk
                      dchis(4) = dchis(4) + dtrm1zn*ryk*rzk
                      dchis(5) = dchis(5) + dtrm1zn*rxk*rzk
                      dchis(6) = dchis(6) + dtrm1zn*rxk*ryk
                    elseif (ndim.eq.2) then
                      dchis(1) = dchis(1) + dtrm1zn*rxk*rxk
                      dchis(2) = dchis(2) + dtrm1zn*ryk*ryk
                      dchis(3) = dchis(3) + dtrm1zn*rxk*ryk
                    endif
                  endif
                endif
              endif
!
!  End of loops over lattice vectors
!
120           continue
          enddo
        enddo
!
!  End of loop over inner atom 
!
      enddo
!
!  Multiply dqx/dqy/dqz by inverse matrix
!
      jeem = 0
      do j = 1,numat
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
          jeem = jeem + 1
          sumx = 0.0_dp
          sumy = 0.0_dp
          sumz = 0.0_dp
          do k = 1,neemfull
            sumx = sumx + dqx(k)*derv2(k,jeem)
            sumy = sumy + dqy(k)*derv2(k,jeem)
            sumz = sumz + dqz(k)*derv2(k,jeem)
          enddo
          dqdxyz(ind+1,j) = dqdxyz(ind+1,j) - sumx
          dqdxyz(ind+2,j) = dqdxyz(ind+2,j) - sumy
          dqdxyz(ind+3,j) = dqdxyz(ind+3,j) - sumz
        endif
      enddo
      if (lSandM) then
!
!  For S & M, multiply dchix/dchiy/dchiz by inverse matrix
!
        jeem = 0
        do j = 1,numat
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            jeem = jeem + 1
            do k = 1,neemfull
              indk = 3*(neemfullptr(k) - 1)
              dqdxyz(indk+1,j) = dqdxyz(indk+1,j) - dchix(k)*derv2(jeem,ieem)
              dqdxyz(indk+2,j) = dqdxyz(indk+2,j) - dchiy(k)*derv2(jeem,ieem)
              dqdxyz(indk+3,j) = dqdxyz(indk+3,j) - dchiz(k)*derv2(jeem,ieem)
            enddo
          endif
        enddo
      endif
      if (lfieldcfg(ncf)) then
!
!  For electric field, multiply dchix/dchiy/dchiz by inverse matrix
!
        jeem = 0
        do j = 1,numat
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            jeem = jeem + 1
            indj = 3*(j - 1)
            dqdxyz(indj+1,i) = dqdxyz(indj+1,i) - fieldx*derv2(jeem,ieem)
            dqdxyz(indj+2,i) = dqdxyz(indj+2,i) - fieldy*derv2(jeem,ieem)
            dqdxyz(indj+3,i) = dqdxyz(indj+3,i) - fieldz*derv2(jeem,ieem)
          endif
        enddo
      endif
!
!  Strain terms
!
      if (lSandM.and.lstrain) then
!
!  For S & M, multiply dchis by inverse matrix
!                 
        jeem = 0
        do j = 1,numat
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            jeem = jeem + 1
            do kl = 1,nstrains
              dqds(kl,j) = dqds(kl,j) - dchis(kl)*derv2(jeem,ieem)
            enddo
          endif
        enddo
      endif
!
!  End loop over i
!
    enddo
    if (lstrain) then
!
!  Strain terms
!
      jeem = 0
      do j = 1,numat
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
          jeem = jeem + 1
          do kl = 1,nstrains
            sum = 0.0_dp
            do k = 1,neemfull
              sum = sum + dqs(k,kl)*derv2(k,jeem)
            enddo
            dqds(kl,j) = dqds(kl,j) - sum
          enddo
        endif
      enddo
    endif
!**********************
!  End periodic case  *
!**********************
  else
!***********************
!  Cluster case / 1-D  *
!***********************
    if (lnoreal) goto 135
    if (ndim.eq.1) then
      call setmaxcell1D(maxloop(1))
      ml1 = maxloop(1)
    else
      ml1 = 0
      r1x = 0.0_dp
    endif
!
!  Start loop over cluster atoms
!
    ieem = 0
    do i = 1,numat
      qlii = qf(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      ind = 3*(i-1)
      ni = nat(i)
      if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) ieem = ieem + 1
!
!  If QEq work out principle quantum number
!
      if (lqeq) then
        if (ni.le.2) then
          npqni = 1
        elseif (ni.le.10) then
          npqni = 2
        elseif (ni.le.18) then
          npqni = 3
        elseif (ni.le.36) then
          npqni = 4
        elseif (ni.le.54) then
          npqni = 5
        elseif (ni.le.86) then
          npqni = 6
        else
          npqni = 7
        endif
        zetai = 0.5_dp*qeqlambda*(2*npqni+1)/qeqrad(ni)
        lhi = (ni.eq.1)
        if (lhi) then
!
!  Special case for hydrogen
!
          zetai = zetai + qlii*rconv
        endif
      elseif (lSandM) then
        zetai = smzeta(ni)
      endif
!
!  Zero temporary derivative arrays
!
      dqx(1:numat) = 0.0_dp
      dqy(1:numat) = 0.0_dp
      dqz(1:numat) = 0.0_dp
      if (lSandM) then
        dchix(1:numat) = 0.0_dp
        dchiy(1:numat) = 0.0_dp
        dchiz(1:numat) = 0.0_dp
        dchis(1:6) = 0.0_dp
      endif
      jeem = 0
!
!  Loop over other atoms and build daij/d(alpha)
!
      do j = 1,numat
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) jeem = jeem + 1
        if (i.ne.j.or.lstrain) then
          qljj = qf(j)
          qlj = qljj*occuf(j)
          nj = nat(j)
!
!  If QEq work out principle quantum number
!
          if (lqeq) then
            if (nj.le.2) then
              npqnj = 1
            elseif (nj.le.10) then
              npqnj = 2
            elseif (nj.le.18) then
              npqnj = 3
            elseif (nj.le.36) then
              npqnj = 4
            elseif (nj.le.54) then
              npqnj = 5
            elseif (nj.le.86) then
              npqnj = 6
            else
              npqnj = 7
            endif
            zetaj = 0.5_dp*qeqlambda*(2*npqnj+1)/qeqrad(nj)
            lhj = (nj.eq.1)
            if (lhj) then
!
!  Special case for hydrogen
!
              zetaj = zetaj + qljj*rconv
            endif
          elseif (lSandM) then
            zetaj = smzeta(nj)
            znucj = smZnuc(nj)
          endif
!
!  Find relative vector between atoms
!
          rx = xclat(j) - xci
          ry = yclat(j) - yci
          rz = zclat(j) - zci
!
!  Calculate Euler-Maclaurin correction to 1-D sum
!
          if (ndim.eq.1) then
            qme = 0.0_dp
            dqme(1) = 0.0_dp
            dqme(2) = 0.0_dp
            dqme(3) = 0.0_dp
            call qmatrix1D(rx,ry,rz,.true.,.false.,qme,dqme,d2qme)
            dqme(1) = dqme(1)*angstoev
            dqme(2) = dqme(2)*angstoev
            dqme(3) = dqme(3)*angstoev
            if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
              dqx(ieem) = dqx(ieem) - qljj*dqme(1)
              dqy(ieem) = dqy(ieem) - qljj*dqme(2)
              dqz(ieem) = dqz(ieem) - qljj*dqme(3)
            endif
            if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
              dqx(jeem) = dqx(jeem) - qlii*dqme(1)
              dqy(jeem) = dqy(jeem) - qlii*dqme(2)
              dqz(jeem) = dqz(jeem) - qlii*dqme(3)
            endif
          endif
!
!  Calculate distances for search
!
          rx = rx - (ml1+1)*r1x
          ry2 = ry*ry
          rz2 = rz*rz
!
!  Loop over lattice vectors
!
          do ii = -ml1,ml1
            rx = rx + r1x
            rr2 = rx*rx + ry2 + rz2
            rl = sqrt(rr2)
            if (rr2.gt.cuts2) then
              rrr = 1.0_dp/rl
              if (lqeq.and.rl.lt.rqeq) then
!***************
!  QEq scheme  *
!***************
                call gammas(npqni,npqnj,zetai,zetaj,rl,gam,dgam,dzetai,dzetaj, &
                            d2zetaii,d2zetaij,d2zetajj,d2zetari,d2zetarj,d2gamr2)
                dtrm1 = dgam*rrr
                dtrm1i = dtrm1
                dtrm1j = dtrm1
                if (lhi) then
                  dtrm1i = dtrm1i + qlii*d2zetari*rrr*rconv
                endif
                if (lhj) then
                  dtrm1j = dtrm1j + qljj*d2zetarj*rrr*rconv
                endif
              elseif (lSandM.and.rl.lt.rqeq) then
!****************************
!  Streitz-Mintmire scheme  *
!****************************
                call gammasm(zetai,zetaj,rl,gam,dgam,d2gamr2,gamifj,gamjfi,dgamifj,dgamjfi,d2gamifj,d2gamjfi)
                dtrm1 = (dgam - rrr*rrr)*rrr
                dtrm1i = dtrm1
                dtrm1j = dtrm1
                dtrm1zn = znucj*(dgamjfi - dgam)*rrr
              else
!***************
!  EEM scheme  *
!***************
                dtrm1 = - rrr*rrr*rrr
                dtrm1i = dtrm1
                dtrm1j = dtrm1
                dtrm1zn = 0.0_dp
              endif
!
!  First derivatives of matrix elements
!
              dtrm1i = dtrm1i*angstoev
              dtrm1j = dtrm1j*angstoev
              if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                dqx(ieem) = dqx(ieem) - qljj*dtrm1i*rx
                dqy(ieem) = dqy(ieem) - qljj*dtrm1i*ry
                dqz(ieem) = dqz(ieem) - qljj*dtrm1i*rz
              endif
              if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
                dqx(jeem) = dqx(jeem) - qlii*dtrm1j*rx
                dqy(jeem) = dqy(jeem) - qlii*dtrm1j*ry
                dqz(jeem) = dqz(jeem) - qlii*dtrm1j*rz
              endif
              if (lSandM) then
                dtrm1zn = dtrm1zn*angstoev
                if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                  dchix(ieem) = dchix(ieem) - dtrm1zn*rx
                  dchiy(ieem) = dchiy(ieem) - dtrm1zn*ry
                  dchiz(ieem) = dchiz(ieem) - dtrm1zn*rz
                endif
                if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
                  dchix(jeem) = dchix(jeem) + dtrm1zn*rx
                  dchiy(jeem) = dchiy(jeem) + dtrm1zn*ry
                  dchiz(jeem) = dchiz(jeem) + dtrm1zn*rz
                endif
              endif
            endif
            if (lstrain) then
!             
!  Strain 
!             
              if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
                dqs(ieem,1) = dqs(ieem,1) + qljj*dtrm1i*rx*rx
                if (lSandM) then
                  dchis(1) = dchis(1) + dtrm1zn*rx*rx
                endif
              endif
            endif
          enddo
!       
!  End of loop over lattice vectors
!
        endif
      enddo
!
!  Multiply dqx/dqy/dqz by inverse matrix 
!
      jeem = 0
      do j = 1,numat
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
          jeem = jeem + 1
          sumx = 0.0_dp
          sumy = 0.0_dp
          sumz = 0.0_dp
          do k = 1,neemfull
            sumx = sumx + dqx(k)*derv2(k,jeem)
            sumy = sumy + dqy(k)*derv2(k,jeem)
            sumz = sumz + dqz(k)*derv2(k,jeem)
          enddo
          dqdxyz(ind+1,j) = dqdxyz(ind+1,j) - sumx
          dqdxyz(ind+2,j) = dqdxyz(ind+2,j) - sumy
          dqdxyz(ind+3,j) = dqdxyz(ind+3,j) - sumz
        endif
      enddo
      if (lSandM) then
!
!  For S & M, multiply dchix/dchiy/dchiz by inverse matrix 
!
        jeem = 0
        do j = 1,numat
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            jeem = jeem + 1
            do k = 1,neemfull
              indk = 3*(neemfullptr(k) - 1)
              dqdxyz(indk+1,j) = dqdxyz(indk+1,j) - dchix(k)*derv2(jeem,ieem)
              dqdxyz(indk+2,j) = dqdxyz(indk+2,j) - dchiy(k)*derv2(jeem,ieem)
              dqdxyz(indk+3,j) = dqdxyz(indk+3,j) - dchiz(k)*derv2(jeem,ieem)
            enddo
          endif
        enddo
      endif
      if (lfieldcfg(ncf)) then
!
!  For electric field, multiply dchix/dchiy/dchiz by inverse matrix
!
        jeem = 0
        do j = 1,numat
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            jeem = jeem + 1
            indj = 3*(j - 1)
            dqdxyz(indj+1,i) = dqdxyz(indj+1,i) - fieldx*derv2(jeem,ieem)
            dqdxyz(indj+2,i) = dqdxyz(indj+2,i) - fieldy*derv2(jeem,ieem)
            dqdxyz(indj+3,i) = dqdxyz(indj+3,i) - fieldz*derv2(jeem,ieem)
          endif
        enddo
      endif
    enddo
    if (lstrain) then
!         
!  Strain terms
!           
      jeem = 0
      do j = 1,numat
        if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
          jeem = jeem + 1
          do kl = 1,nstrains
            sum = 0.0_dp
            do k = 1,neemfull
              sum = sum + dqs(k,kl)*derv2(k,jeem)
            enddo
            dqds(kl,j) = dqds(kl,j) - sum
          enddo
        endif
      enddo
      if (lSandM) then
!
!  For S & M, multiply dchis by inverse matrix
!                 
        jeem = 0
        do j = 1,numat
          if (lelementOK(nat(j)).and.nregionno(nsft+nrelat(j)).eq.1) then
            jeem = jeem + 1
            do kl = 1,nstrains
              dqds(kl,j) = dqds(kl,j) - dchis(kl)*derv2(jeem,ieem)
            enddo
          endif
        enddo
      endif
    endif
!******************************
!  End of cluster / 1-D case  *
!******************************
  endif
135 continue
!***********************************************************************************
!  Enforce sum rules that total charge derivative equals zero for each coordinate  *
!***********************************************************************************
!
!  Sum elements and count number of active atoms
!
  do i = 1,numat
    dqx(i) = 0.0_dp
    dqy(i) = 0.0_dp
    dqz(i) = 0.0_dp
  enddo
  do i = 1,numat
    jx = - 2
    jy = - 1
    jz =   0
    do j = 1,numat
      jx = jx + 3
      jy = jy + 3
      jz = jz + 3
      dqx(j) = dqx(j) + dqdxyz(jx,i)
      dqy(j) = dqy(j) + dqdxyz(jy,i)
      dqz(j) = dqz(j) + dqdxyz(jz,i)
    enddo
  enddo
!
!  Average error and subtract from active elements
!
  if (neemfull.gt.0) then
    do i = 1,numat
      dqx(i) = - dqx(i)/dble(neemfull)
      dqy(i) = - dqy(i)/dble(neemfull)
      dqz(i) = - dqz(i)/dble(neemfull)
    enddo
    do i = 1,numat
      if (lelementOK(nat(i)).and.nregionno(nsft+nrelat(i)).eq.1) then
        jx = - 2
        jy = - 1
        jz =   0
        do j = 1,numat
          jx = jx + 3
          jy = jy + 3
          jz = jz + 3
          dqdxyz(jx,i) = dqdxyz(jx,i) + dqx(j)
          dqdxyz(jy,i) = dqdxyz(jy,i) + dqy(j)
          dqdxyz(jz,i) = dqdxyz(jz,i) + dqz(j)
        enddo
      endif
    enddo
  endif
!
!  Free local memory
!
  if (lSandM) then
    deallocate(dchiz,stat=status)
    if (status/=0) call deallocate_error('dcharge','dchiz')
    deallocate(dchiy,stat=status)
    if (status/=0) call deallocate_error('dcharge','dchiy')
    deallocate(dchix,stat=status)
    if (status/=0) call deallocate_error('dcharge','dchix')
  endif
  deallocate(dqz,stat=status)
  if (status/=0) call deallocate_error('dcharge','dqz')
  deallocate(dqy,stat=status)
  if (status/=0) call deallocate_error('dcharge','dqy')
  deallocate(dqx,stat=status)
  if (status/=0) call deallocate_error('dcharge','dqx')
  deallocate(neemfullptr,stat=status)
  if (status/=0) call deallocate_error('dcharge','neemfullptr')
!
  if (lprint.and.ioproc) then
    if (ndim.eq.0) then
      write(ioout,'(/,''  First derivatives of charge distribution : (Angstroms**-1)'',/)')
    else
      write(ioout,'(/,''  First derivatives of charge distribution : '',/)')
      write(ioout,'(''  Strain :'',/)')
      write(ioout,'(''  Atom   '',6(i10))') (j,j=1,nstrains)
      do i = 1,numat
        write(ioout,'(i6,4x,6f10.6)') i,(dqds(j,i),j=1,nstrains)
      enddo
      write(ioout,'(/,''  Coordinate (Angstroms**-1) :'',/)')
    endif
    igroup = numat/6
    iresid = numat - igroup*6
    indi = 0
    if (igroup.gt.0) then
      do i = 1,igroup
        write(ioout,'(''  Atom   '',6(i10))') (indi+j,j=1,6)
        indj = 0
        do j = 1,numat
          write(ioout,'(i6,'' x '',4x,6f10.6)') j,(dqdxyz(indj+1,indi+k),k=1,6)
          write(ioout,'(i6,'' y '',4x,6f10.6)') j,(dqdxyz(indj+2,indi+k),k=1,6)
          write(ioout,'(i6,'' z '',4x,6f10.6)') j,(dqdxyz(indj+3,indi+k),k=1,6)
          indj = indj + 3
        enddo
        indi = indi + 6
        write(ioout,'(/)')
      enddo
    endif
    if (iresid.gt.0) then
      write(ioout,'(''  Atom   '',6(i10))') (indi+j,j=1,iresid)
      indj = 0
      do j = 1,numat
        write(ioout,'(i6,'' x '',4x,6f10.6)') j,(dqdxyz(indj+1,indi+k),k=1,iresid)
        write(ioout,'(i6,'' y '',4x,6f10.6)') j,(dqdxyz(indj+2,indi+k),k=1,iresid)
        write(ioout,'(i6,'' z '',4x,6f10.6)') j,(dqdxyz(indj+3,indi+k),k=1,iresid)
        indj = indj + 3
      enddo
    endif
    write(ioout,'(/)')
  endif
!
  return
  end
