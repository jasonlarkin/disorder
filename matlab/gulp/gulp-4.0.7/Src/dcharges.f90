  subroutine dcharges(lprint)
!
!  Calculates the first derivative of the charge with respect
!  to the coordinates of the atoms. Symmetry adapted version 
!  of dcharge.f
!
!   1/98 Created from dcharge
!   1/98 Strain derivatives added - note that derv3 is used as
!        scratch storage in this algorithm
!  10/02 ReaxFF modifications added
!   3/03 Modified to handle frozen regions with fixed charges
!   9/04 Order of terms in dqds switched
!   9/04 z2 array made local
!   7/05 Extra argument to sympot added
!   7/05 Streitz and Mintmire modifications added
!  12/05 Return added for case where there are no charges
!   8/06 Bug due to memory being zeroed before being resized fixed
!  11/07 Unused variables removed
!  12/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   3/08 ReaxFF charge scheme removed as this is handled elsewhere
!   5/12 third not overwritten in this routine
!
!  On entry:
!
!    lprint= logical indicating whether printed output is wanted
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use constants
  use control
  use current
  use derivatives,    dqs => derv3
  use element
  use general,        only : cutw, etaw
  use iochannels
  use kspace
  use numbers,        only : third
  use parallel
  use shell
  use symmetry
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lprint
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ieem
  integer(i4)                                  :: ifail
  integer(i4)                                  :: igroup
  integer(i4)                                  :: ii
  integer(i4)                                  :: ilaenv
  integer(i4)                                  :: ind
  integer(i4)                                  :: indi
  integer(i4)                                  :: indj
  integer(i4), dimension(:), allocatable       :: ipivot
  integer(i4)                                  :: iresid
  integer(i4)                                  :: iv
  integer(i4)                                  :: j
  integer(i4)                                  :: jeem
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: kl
  integer(i4)                                  :: lwrk
  integer(i4)                                  :: ml1
  integer(i4)                                  :: ml2
  integer(i4)                                  :: ml3
  integer(i4)                                  :: n
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: npqni
  integer(i4)                                  :: npqnj
  integer(i4)                                  :: status
  logical                                      :: lhi
  logical                                      :: lhj
  logical                                      :: lhpresent
  real(dp),    dimension(:), allocatable       :: chitmp
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
  real(dp)                                     :: dchis(6)
  real(dp),    dimension(:), allocatable       :: dchix
  real(dp),    dimension(:), allocatable       :: dchiy
  real(dp),    dimension(:), allocatable       :: dchiz
  real(dp)                                     :: derfc
  real(dp)                                     :: dgam
  real(dp)                                     :: dgamifj
  real(dp)                                     :: dgamjfi
  real(dp),    dimension(:), allocatable       :: dqx
  real(dp),    dimension(:), allocatable       :: dqy
  real(dp),    dimension(:), allocatable       :: dqz
  real(dp)                                     :: dtrm1
  real(dp)                                     :: dtrm1zn
  real(dp)                                     :: dtrm1i
  real(dp)                                     :: dzetai
  real(dp)                                     :: dzetaj
  real(dp)                                     :: errfcn
  real(dp)                                     :: etaloc
  real(dp)                                     :: gam
  real(dp)                                     :: gamifj
  real(dp)                                     :: gamjfi
  real(dp)                                     :: qlii
  real(dp)                                     :: qlj
  real(dp)                                     :: qljj
  real(dp)                                     :: rconv
  real(dp)                                     :: rexp
  real(dp)                                     :: rjfac
  real(dp)                                     :: rl
  real(dp)                                     :: rmax
  real(dp)                                     :: rmax2
  real(dp)                                     :: rqeq2
  real(dp)                                     :: rr2
  real(dp)                                     :: rrr
  real(dp)                                     :: rv2
  real(dp)                                     :: rx
  real(dp)                                     :: rxi
  real(dp)                                     :: rxj
  real(dp)                                     :: rxk
  real(dp)                                     :: ry
  real(dp)                                     :: ryi
  real(dp)                                     :: ryj
  real(dp)                                     :: ryk
  real(dp)                                     :: rz
  real(dp)                                     :: rzi
  real(dp)                                     :: rzj
  real(dp)                                     :: rzk
  real(dp)                                     :: setaloc
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
  real(dp),    dimension(:), allocatable       :: z2
!******************
!  Memory checks  *
!******************
!
!  Check the memory for the square arrays
!
  if (nasym+1.gt.maxd2u) then
    maxd2u = nasym + 1
    call changemaxd2
  endif
  if (numat+1.gt.maxd2) then
    maxd2 = numat + 1
    call changemaxd2
  endif
!
!  Check the memory for the charge derivatives
!
  if (nasym.gt.maxd2qu) then
    maxd2qu = nasym
    call changemaxd2q
  endif
  if (3*numat.gt.maxd2q) then
    maxd2q = 3*numat
    call changemaxd2q
  endif
!*********************
!  Zero dq/d(alpha)  *
!*********************
  do i = 1,nasym
    do j = 1,3*numat
      dqdxyz(j,i) = 0.0_dp
    enddo
  enddo
  do i = 1,numat
    do j = 1,6
      dqds(j,i) = 0.0_dp
    enddo
  enddo
!
!  If this is not a charged system return
!
  if (.not.lewald) return
!
  cuts2 = cuts*cuts
  rconv = 1.0_dp/autoangs
!
!  Is this QEq with H present?
!
  lhpresent = .false.
  if (lqeq) then
    i = 0
    do while (.not.lhpresent.and.i.lt.nasym)
      i = i + 1
      if (iatn(i).eq.1) lhpresent = .true.
    enddo
  endif
  if (lqeq.and.lhpresent) then
!**********************
!  Hydrogen/QEq case  *
!**********************
    allocate(chitmp(numat),stat=status)
    if (status/=0) call outofmemory('dcharges','chitmp')
    allocate(z2(numat),stat=status)
    if (status/=0) call outofmemory('dcharges','z2')
!
!  If hydrogen is present then we need to calculate A + (dA/dq).q
!  invert this matrix to use in place of A**-1 below.
!
!  First generate 2 centre terms
!
    call sympot(derv2,maxd2,chitmp,2_i4)
!
!  Reduce to nasym x nasym form
!
    do i = 1,nasym
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
        z2(k) = z2(k) + derv2(j,i)*occuf(j)
      enddo
!
!  Copy temporary storage vector back into derv2 array
!
      do j = 1,numat
        derv2(j,i) = z2(j)
      enddo
    enddo
!
!  Reduce to neem x neem form for potential
!
    do i = 1,neem
      ieem = neemptr(i)
      do j = 1,neem
        jeem = neemptr(j)
        derv2(jeem,ieem) = derv2(j,i)
      enddo
    enddo
!
!  Add one centre terms
!
    zetah0 = autoangs*0.75_dp/qeqrad(1)
    do ieem = 1,neem
      i = neemptr(ieem)
      if (iatn(i).ne.1) then
        derv2(ieem,ieem) = derv2(ieem,ieem) + 2.0_dp*qeqmu(iatn(i))*occua(i)
      else
!
!  For hydrogen charge dependant factor must be introduced
!  which includes both Aii and dAii/dqi
!
        rjfac = 1.0_dp + (2.0_dp*qa(i))/zetah0
        derv2(ieem,ieem) = derv2(ieem,ieem) + 2.0_dp*qeqmu(1)*occua(i)*rjfac
      endif
    enddo
!
!  Complete constraint terms
!
    do ieem = 1,neem
      i = neemptr(ieem)
      derv2(ieem,neem+1) = dble(neqv(i))*occua(i)
      derv2(neem+1,ieem) = 1.0_dp
    enddo
    derv2(neem+1,neem+1) = 0.0_dp
!
!  Allocate workspace for inversion
!
    n = neem + 1
    lwrk = n*ilaenv(1_i4,'DGETRI',' ',n,-1_i4,-1_i4,-1_i4)
    allocate(ipivot(n),stat=status)
    if (status/=0) call outofmemory('dcharges','ipivot')
    allocate(wrk(lwrk),stat=status)
    if (status/=0) call outofmemory('dcharges','wrk')
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
    if (status/=0) call deallocate_error('dcharges','wrk')
    deallocate(ipivot,stat=status)
    if (status/=0) call deallocate_error('dcharges','ipivot')
    deallocate(z2,stat=status)
    if (status/=0) call deallocate_error('dcharges','z2')
    deallocate(chitmp,stat=status)
    if (status/=0) call deallocate_error('dcharges','chitmp')
!**************************
!  End hydrogen/QEq case  *
!**************************
  endif
  rqeq2 = rqeq*rqeq
!
!  Allocate local memory
!
  allocate(dqx(nasym),stat=status)
  if (status/=0) call outofmemory('dcharges','dqx')
  allocate(dqy(nasym),stat=status)
  if (status/=0) call outofmemory('dcharges','dqy')
  allocate(dqz(nasym),stat=status)
  if (status/=0) call outofmemory('dcharges','dqz')
  if (lSandM) then
    allocate(dchix(nasym),stat=status)
    if (status/=0) call outofmemory('dcharges','dchix')
    allocate(dchiy(nasym),stat=status)
    if (status/=0) call outofmemory('dcharges','dchiy')
    allocate(dchiz(nasym),stat=status)
    if (status/=0) call outofmemory('dcharges','dchiz')
  endif
!******************
!  Periodic case  *
!******************
  if (lwolf) then
    radmax = cutw
    etaloc = etaw*etaw
    setaloc = etaw
  else
    radmax = accf/seta
    eta4 = 0.25_dp/eta
    etaloc = eta
    setaloc = seta
  endif
  if (lqeq.or.lSandM) then
    rmax = max(radmax,rqeq)
  else
    rmax = radmax
  endif
  rmax2 = rmax*rmax
!
!  Estimate upper limits for looping
!
  rv2 = rv(1,1)**2 + rv(2,1)**2 + rv(3,1)**2
  rv2 = sqrt(rv2)
  ml1 = rmax/rv2 + 1
  rv2 = rv(1,2)**2 + rv(2,2)**2 + rv(3,2)**2
  rv2 = sqrt(rv2)
  ml2 = rmax/rv2 + 1
  rv2 = rv(1,3)**2 + rv(2,3)**2 + rv(3,3)**2
  rv2 = sqrt(rv2)
  ml3 = rmax/rv2 + 1
!
!  Zero temporary strain derivative arrays
!
  do i = 1,6
    do j = 1,nasym
      dqs(j,i) = 0.0_dp
    enddo
  enddo
!**********************************
!  Reciprocal space contribution  *
!**********************************
  if (lnorecip.or.lwolf) goto 5
!
!  Start loop over atoms for coordinate differentiation
!
  do ieem = 1,neem
    i = neemptr(ieem)
    xci = xalat(i)
    yci = yalat(i)
    zci = zalat(i)
    ind = 3*(i-1)
!
!  Zero temporary derivative arrays
!
    do j = 1,nasym
      dqx(j) = 0.0_dp
      dqy(j) = 0.0_dp
      dqz(j) = 0.0_dp
    enddo
    do j = 1,numat
      xd = xclat(j) - xci
      yd = yclat(j) - yci
      zd = zclat(j) - zci
      qlj = qf(j)
!
!  Evaluate derivative of reciprocal space elements of A
!
      do iv = 1,nkvec
        argc(iv) = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
        sine(iv) = sin(argc(iv))*ktrm(iv)
        dqx(ieem) = dqx(ieem) + sine(iv)*xrk(iv)*qlj
        dqy(ieem) = dqy(ieem) + sine(iv)*yrk(iv)*qlj
        dqz(ieem) = dqz(ieem) + sine(iv)*zrk(iv)*qlj
        costrm = cos(argc(iv))*angstoev*qlj
        strm1 = costrm*ktrms(iv)
        strm2 = costrm*ktrm(iv)
        dqs(ieem,1) = dqs(ieem,1) - strm1*xrk(iv)*xrk(iv) - strm2
        dqs(ieem,2) = dqs(ieem,2) - strm1*yrk(iv)*yrk(iv) - strm2
        dqs(ieem,3) = dqs(ieem,3) - strm1*zrk(iv)*zrk(iv) - strm2
        dqs(ieem,4) = dqs(ieem,4) - strm1*yrk(iv)*zrk(iv)
        dqs(ieem,5) = dqs(ieem,5) - strm1*xrk(iv)*zrk(iv)
        dqs(ieem,6) = dqs(ieem,6) - strm1*xrk(iv)*yrk(iv)
      enddo
!
!  End inner atom loop
!
    enddo
!
!  Multiply dqx/dqy/dqz by inverse matrix
!
    do jeem = 1,neem
      j = neemptr(jeem)
      sumx = 0.0_dp
      sumy = 0.0_dp
      sumz = 0.0_dp
      do k = 1,neem
        sumx = sumx + dqx(k)*derv2(k,jeem)
        sumy = sumy + dqy(k)*derv2(k,jeem)
        sumz = sumz + dqz(k)*derv2(k,jeem)
      enddo
      sumx = 2.0_dp*sumx*angstoev
      sumy = 2.0_dp*sumy*angstoev
      sumz = 2.0_dp*sumz*angstoev
      dqdxyz(ind+1,j) = dqdxyz(ind+1,j) - sumx
      dqdxyz(ind+2,j) = dqdxyz(ind+2,j) - sumy
      dqdxyz(ind+3,j) = dqdxyz(ind+3,j) - sumz
    enddo
  enddo
5 continue
!*************************
!  Real space summation  *
!*************************
  if (lnoreal) then
!
!  Strain terms
!
    do jeem = 1,neem
      j = neemptr(jeem)
      do kl = 1,nstrains
        sum = 0.0_dp
        do k = 1,neem
          sum = sum + dqs(k,kl)*derv2(k,jeem)
        enddo
        dqds(kl,j) = dqds(kl,j) - sum
      enddo
    enddo
    goto 135
  endif
!
!  Start loop over atoms for coordinate differentiation
!
  do ieem = 1,neem
    i = neemptr(ieem)
    qlii = qa(i)
    xci = xalat(i)
    yci = yalat(i)
    zci = zalat(i)
    ind = 3*(i-1)
    ni = iatn(i)
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
    dqx(1:nasym) = 0.0_dp
    dqy(1:nasym) = 0.0_dp
    dqz(1:nasym) = 0.0_dp
    if (lSandM) then
      dchix(1:nasym) = 0.0_dp
      dchiy(1:nasym) = 0.0_dp
      dchiz(1:nasym) = 0.0_dp
      dchis(1:6) = 0.0_dp
    endif
!
    do j = 1,numat
      qljj = qf(j)
      qlj = qljj*occuf(j)
      rx = xclat(j) - xci
      ry = yclat(j) - yci
      rz = zclat(j) - zci
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
!  Loop over cell vectors
!
      rxi = rx - (ml1+1)*r1x
      ryi = ry - (ml1+1)*r1y
      rzi = rz - (ml1+1)*r1z
      do ii = -ml1,ml1
        rxi = rxi + r1x
        ryi = ryi + r1y
        rzi = rzi + r1z
        rxj = rxi - (ml2+1)*r2x
        ryj = ryi - (ml2+1)*r2y
        rzj = rzi - (ml2+1)*r2z
        do jj = - ml2,ml2
          rxj = rxj + r2x
          ryj = ryj + r2y
          rzj = rzj + r2z
          rxk = rxj - (ml3+1)*r3x
          ryk = ryj - (ml3+1)*r3y
          rzk = rzj - (ml3+1)*r3z
          do 120 kk = - ml3,ml3
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
              dtrm1i = rrr*rrr*rrr
              dtrm1zn = 0.0_dp
            elseif (rr2.lt.rqeq2.and.lqeq) then
!
!  Calculate Coulomb interaction according to QEq scheme and
!  subtract 1/r term from Ewald sum.
!
              call gammas(npqni,npqnj,zetai,zetaj,rl,gam,dgam,dzetai,dzetaj, &
                          d2zetaii,d2zetaij,d2zetajj,d2zetari,d2zetarj,d2gamr2)
              dtrm1i = (rrr*rrr + dgam)*rrr
              if (lhi) then
                dtrm1i = dtrm1i + qlii*d2zetari*rrr*rconv
              endif
            elseif (rr2.lt.rqeq2.and.lSandM) then
              call gammasm(zetai,zetaj,rl,gam,dgam,d2gamr2,gamifj,gamjfi,dgamifj,dgamjfi,d2gamifj,d2gamjfi)
              dtrm1i = dgam*rrr
              dtrm1zn = znucj*(dgamjfi - dgam)*rrr
            else
              dtrm1i = 0.0_dp
              dtrm1zn = 0.0_dp
            endif
!
!  Complementary error function
!
            errfcn = derfc(setaloc*rl)
            trmi = errfcn/rl
            rexp = tweatpi*exp(-etaloc*rr2)
            dtrm1 = (trmi + rexp)*rrr*rrr
            dtrm1i = dtrm1i - dtrm1
!
!  First derivatives of matrix elements
!
            dtrm1i = dtrm1i*angstoev
            dqx(ieem) = dqx(ieem) - qljj*dtrm1i*rxk
            dqy(ieem) = dqy(ieem) - qljj*dtrm1i*ryk
            dqz(ieem) = dqz(ieem) - qljj*dtrm1i*rzk
            if (lSandM) then
              dtrm1zn = dtrm1zn*angstoev
              dchix(ieem) = dchix(ieem) - dtrm1zn*rxk
              dchiy(ieem) = dchiy(ieem) - dtrm1zn*ryk
              dchiz(ieem) = dchiz(ieem) - dtrm1zn*rzk
            endif
!
!  Strain
!
            dqs(ieem,1) = dqs(ieem,1) + qljj*dtrm1i*rxk*rxk
            dqs(ieem,2) = dqs(ieem,2) + qljj*dtrm1i*ryk*ryk
            dqs(ieem,3) = dqs(ieem,3) + qljj*dtrm1i*rzk*rzk
            dqs(ieem,4) = dqs(ieem,4) + qljj*dtrm1i*ryk*rzk
            dqs(ieem,5) = dqs(ieem,5) + qljj*dtrm1i*rxk*rzk
            dqs(ieem,6) = dqs(ieem,6) + qljj*dtrm1i*rxk*ryk
            if (lSandM) then
              dchis(1) = dchis(1) + dtrm1zn*rxk*rxk
              dchis(2) = dchis(2) + dtrm1zn*ryk*ryk
              dchis(3) = dchis(3) + dtrm1zn*rzk*rzk
              dchis(4) = dchis(4) + dtrm1zn*ryk*rzk
              dchis(5) = dchis(5) + dtrm1zn*rxk*rzk
              dchis(6) = dchis(6) + dtrm1zn*rxk*ryk
            endif
!
!  End of loops over lattice vectors
!
120         continue
        enddo
      enddo
!
!  End of loop over inner atom 
!
    enddo
!
!  Multiply dqx/dqy/dqz by inverse matrix
!
    do jeem = 1,neem
      j = neemptr(jeem)
      sumx = 0.0_dp
      sumy = 0.0_dp
      sumz = 0.0_dp
      do k = 1,neem
        sumx = sumx + dqx(k)*derv2(k,jeem)
        sumy = sumy + dqy(k)*derv2(k,jeem)
        sumz = sumz + dqz(k)*derv2(k,jeem)
      enddo
      sumx = 2.0_dp*sumx
      sumy = 2.0_dp*sumy
      sumz = 2.0_dp*sumz
      dqdxyz(ind+1,j) = dqdxyz(ind+1,j) - sumx
      dqdxyz(ind+2,j) = dqdxyz(ind+2,j) - sumy
      dqdxyz(ind+3,j) = dqdxyz(ind+3,j) - sumz
    enddo
    if (lSandM) then
!
!  For S & M, multiply dchix/dchiy/dchiz by inverse matrix
!
      do jeem = 1,neem
        j = neemptr(jeem)
        do k = 1,neem
          indk = 3*(neemptr(k) - 1)
          dqdxyz(indk+1,j) = dqdxyz(indk+1,j) - 2.0_dp*dchix(k)*derv2(jeem,ieem)
          dqdxyz(indk+2,j) = dqdxyz(indk+2,j) - 2.0_dp*dchiy(k)*derv2(jeem,ieem)
          dqdxyz(indk+3,j) = dqdxyz(indk+3,j) - 2.0_dp*dchiz(k)*derv2(jeem,ieem)
        enddo
      enddo
    endif
!
!  Strain terms
!
    if (lSandM) then
!
!  For S & M, multiply dchis by inverse matrix
!
      do jeem = 1,neem
        j = neemptr(jeem)
        do kl = 1,nstrains
          dqds(kl,j) = dqds(kl,j) - dchis(kl)*derv2(jeem,ieem)
        enddo
      enddo
    endif
!
!  End loop over i
!
  enddo
!
!  Strain terms
!
  do jeem = 1,neem
    j = neemptr(jeem)
    do kl = 1,nstrains
      sum = 0.0_dp
      do k = 1,neem
        sum = sum + dqs(k,kl)*derv2(k,jeem)
      enddo
      dqds(kl,j) = dqds(kl,j) - sum
    enddo
  enddo
!**********************
!  End periodic case  *
!**********************
135 continue
!******************************************
!  Apply symmetry corrections to strains  *
!******************************************
  call celltype(ictype,icfhr)
  if ((ictype.eq.5.and.icfhr.eq.1).or.ictype.eq.6) then
!
!  Cubic or rhombohedral
!
    do i = 1,nasym
      sum = dqds(1,i) + dqds(2,i) + dqds(3,i)
      sum = third*sum
      dqds(1,i) = sum
      dqds(2,i) = sum
      dqds(3,i) = sum
      dqds(4,i) = 0.0_dp
      dqds(5,i) = 0.0_dp
      dqds(6,i) = 0.0_dp
    enddo
  elseif (ictype.eq.4.or.ictype.eq.5) then
!
!  Tetragonal or hexagonal
!
    do i = 1,nasym
      sum = dqds(i,1) + dqds(i,2)
      sum = 0.5_dp*sum
      dqds(1,i) = sum
      dqds(2,i) = sum
      dqds(4,i) = 0.0_dp
      dqds(5,i) = 0.0_dp
      dqds(6,i) = 0.0_dp
    enddo
  elseif (ictype.eq.3) then
!
!  Orthorhombic
!
    do i = 1,nasym
      dqds(4,i) = 0.0_dp
      dqds(5,i) = 0.0_dp
      dqds(6,i) = 0.0_dp
    enddo
  endif
!
!  Free local memory
!
  if (lSandM) then
    deallocate(dchiz,stat=status)
    if (status/=0) call deallocate_error('dcharges','dchiz')
    deallocate(dchiy,stat=status)
    if (status/=0) call deallocate_error('dcharges','dchiy')
    deallocate(dchix,stat=status)
    if (status/=0) call deallocate_error('dcharges','dchix')
  endif
  deallocate(dqz,stat=status)
  if (status/=0) call deallocate_error('dcharges','dqz')
  deallocate(dqy,stat=status)
  if (status/=0) call deallocate_error('dcharges','dqy')
  deallocate(dqx,stat=status)
  if (status/=0) call deallocate_error('dcharges','dqx')
!***********
!  Output  *
!***********
  if (lprint.and.ioproc) then
    write(ioout,'(/,''  First derivatives of charge distribution (symmetry adapted) :'',/)')
    write(ioout,'(''  Strain :'',/)')
    write(ioout,'(''  Atom   '',6(i10))') (j,j=1,6)
    do i = 1,nasym
      write(ioout,'(i6,4x,6f10.6)') i,(dqds(j,i),j=1,6)
    enddo
    write(ioout,'(/,''  Coordinate (Angstroms**-1) :'',/)')
    igroup = nasym/6
    iresid = nasym - igroup*6
    indi = 0
    if (igroup.gt.0) then
      do i = 1,igroup
        write(ioout,'(''  Atom   '',6(i10))') (indi+j,j=1,6)
        indj = 0
        do j = 1,nasym
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
      do j = 1,nasym
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
