  subroutine three0d3(i,j,nati,ntypi,natj,ntypj,d3,xc1,yc1,zc1,x21,y21,z21,nthbk)
!
!  Subroutine for third derivatives of three-body energy for i-j
!  pair. Called from real0d3. On entry we already have a possible
!  valid i-j pair. Algorithm modified - less checking now done
!  while searching for triads and more when a possible triad has
!  been found. This makes the checking for valid combinations
!  simpler and more robust (in the case of multiple pivots per
!  triad) at the cost of more cpu time.
!
!   9/97 Created from three12
!   5/98 Debugged properly and d3/d33 expanded to full matrix storage
!   5/98 Third derivatives for all three body potentials added
!   5/98 Handling of cut-off permutation sorted
!   5/98 Potential dependent parts placed in separate subroutine
!  10/98 BAcross potential added
!  10/98 Conversion of theta to rad now done in here
!   6/99 Partial occupancies added
!   8/99 Linear-three potential added
!   6/00 Modified to handle multiple possible pivots
!   5/01 Modifications added for rhos in sw3
!   6/01 Passing of cut-offs to threebody fixed
!  10/02 Bcoscross potential added
!  11/02 Wildcard atom type added
!  12/02 Correction made for lin3 potential
!  11/03 Workspace arrays moved into module for resizing
!   9/04 New arguments added to threebody
!  10/05 Hydrogen-bond potential added
!  12/05 ESFF equatorial potential added
!   9/06 Theta tapering added
!   1/07 UFF3 added
!   5/07 Dreiding option added
!   6/07 Dreiding option bonded2donorJK check added
!  11/08 BAcoscross form added
!   3/08 3coulomb potential added
!   6/09 Module name changed from three to m_three
!   7/09 Modifications for exp2 potential added
!
!  d33 is stored as full matrix for each component :
!    1 = xxx 10 = xxy 19 = xxz |
!    2 = yxx 11 = yxy 20 = yxz |
!    3 = zxx 12 = zxy 21 = zxz |for 1-3 
!    4 = xyx 13 = xyy 22 = xyz |repeat in 28-54 for 2-3
!    5 = yyx 14 = yyy 23 = yyz |
!    6 = zyx 15 = zyy 24 = zyz |
!    7 = xzx 16 = xzy 25 = xzz |
!    8 = yzx 17 = yzy 26 = yzz |
!    9 = zzx 18 = zzy 27 = zzz |
!
!  ipivot = pointer to i,j,k according to which is the central atom
!           for an asymmetric potential
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
!  Julian Gale, NRI, Curtin University, July 2009
!
  use constants
  use current
  use feworkspace
  use m_three
  implicit none
!
!  Passed variables
!
  integer(i4)        :: i
  integer(i4)        :: j
  integer(i4)        :: nati
  integer(i4)        :: natj
  integer(i4)        :: nthbk
  integer(i4)        :: ntypi
  integer(i4)        :: ntypj
  real(dp)           :: d3(3,3,3)
  real(dp)           :: x21
  real(dp)           :: y21
  real(dp)           :: z21
  real(dp)           :: xc1
  real(dp)           :: yc1
  real(dp)           :: zc1
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: ii1
  integer(i4)        :: ii2
  integer(i4)        :: ii3
  integer(i4)        :: ind
  integer(i4)        :: ipivot
  integer(i4)        :: ipivottriad(3)
  integer(i4)        :: jkorder(3)
  integer(i4)        :: k
  integer(i4)        :: kd3
  integer(i4)        :: kk
  integer(i4)        :: n
  integer(i4)        :: n3
  integer(i4)        :: n3ty
  integer(i4)        :: natk
  integer(i4)        :: nt1
  integer(i4)        :: nt2
  integer(i4)        :: nt3
  integer(i4)        :: ntmp
  integer(i4)        :: ntriad
  integer(i4)        :: ntyp1
  integer(i4)        :: ntyp2
  integer(i4)        :: ntyp3
  integer(i4)        :: ntypk
  logical            :: bonded2donor
  logical            :: bonded2donorJK
  logical            :: lkfound
  logical            :: lmatch
  logical            :: lsymijk
  logical            :: lvalid
  logical            :: lvalidij
  real(dp)           :: ang
  real(dp)           :: d0i
  real(dp)           :: d0j
  real(dp)           :: d0k
  real(dp)           :: d1q(3,3)
  real(dp)           :: d2q(6)
  real(dp)           :: dot
  real(dp)           :: e1d1
  real(dp)           :: e1d2
  real(dp)           :: e1d3
  real(dp)           :: e2d(6)
  real(dp)           :: e3d(10)
  real(dp)           :: ethb
  real(dp)           :: ocij
  real(dp)           :: ock
  real(dp)           :: ofct
  real(dp)           :: qli
  real(dp)           :: qlj
  real(dp)           :: qlk
  real(dp)           :: qjk(3)
  real(dp)           :: r12
  real(dp)           :: r122
  real(dp)           :: r13
  real(dp)           :: r132
  real(dp)           :: r23
  real(dp)           :: r232
  real(dp)           :: rho1
  real(dp)           :: rho2
  real(dp)           :: rho3
  real(dp)           :: rho4
  real(dp)           :: rho5
  real(dp)           :: rk32
  real(dp)           :: rk33
  real(dp)           :: rk34
  real(dp)           :: rkthb
  real(dp)           :: rkthb3
  real(dp)           :: rkthb4
  real(dp)           :: ro1
  real(dp)           :: ro2
  real(dp)           :: ro3
  real(dp)           :: ro4
  real(dp)           :: ro5
  real(dp)           :: the0
  real(dp)           :: tr1
  real(dp)           :: tr2
  real(dp)           :: tr3
  real(dp)           :: tr11
  real(dp)           :: tr21
  real(dp)           :: tr31
  real(dp)           :: trmax
  real(dp)           :: ttr11
  real(dp)           :: ttr21
  real(dp)           :: ttr31
  real(dp)           :: v12(3)
  real(dp)           :: v13(3)
  real(dp)           :: v23(3)
  real(dp)           :: x31
  real(dp)           :: y31
  real(dp)           :: z31
  real(dp)           :: x32
  real(dp)           :: y32
  real(dp)           :: z32
!
!  Charges are just dummies here since third derivatives are not available for variable charges
!
  qli = 0.0_dp
  qlj = 0.0_dp
  qlk = 0.0_dp
!
  ocij = occuf(i)*occuf(j)
!*************************
!  Loop over potentials  *
!*************************
  potloop: do n = 1,nthb
    n3ty = nthrty(n)
    nt1 = ntspec1(n)
    nt2 = ntspec2(n)
    nt3 = ntspec3(n)
    ntyp1 = ntptyp1(n)
    ntyp2 = ntptyp2(n)
    ntyp3 = ntptyp3(n)
    tr11 = thr1(n)
    tr21 = thr2(n)
    tr31 = thr3(n)
    tr1 = tr11*tr11
    tr2 = tr21*tr21
    tr3 = tr31*tr31
    rkthb = thbk(n)
    rkthb3 = 0.0_dp
    rkthb4 = 0.0_dp
    ro1 = 0.0_dp
    ro2 = 0.0_dp
    ro3 = 0.0_dp
    ro4 = 0.0_dp
    ro5 = 0.0_dp
    if (n3ty.eq.2) then
      the0 = theta(n)*degtorad
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      if (ro1.ne.0.0_dp) ro1 = 1.0_dp/ro1
      if (ro2.ne.0.0_dp) ro2 = 1.0_dp/ro2
      lsymijk = .false.
    elseif (n3ty.eq.1) then
      the0 = theta(n)*degtorad
      rkthb3 = thrho2(n)/6.0_dp
      rkthb4 = thrho1(n)/24.0_dp
      lsymijk = .false.
    elseif (n3ty.eq.3) then
      the0 = 0.0_dp
      lsymijk = .true.
    elseif (n3ty.eq.4) then
      the0 = 0.0_dp
      ro1 = theta(n)
      ro2 = thrho1(n)
      ro3 = thrho2(n)
      lsymijk = .true.
    elseif (n3ty.eq.5) then
      the0 = cos(theta(n)*degtorad)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      ro3 = 0.0_dp
      lsymijk = .false.
    elseif (n3ty.eq.6) then
      the0 = 0.0_dp
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      lsymijk = .false.
    elseif (n3ty.eq.7) then
      the0 = theta(n)
      lsymijk = .false.
    elseif (n3ty.eq.8) then
      the0 = theta(n)*degtorad
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      if (ro1.ne.0.0_dp) ro1 = 1.0_dp/ro1
      if (ro2.ne.0.0_dp) ro2 = 1.0_dp/ro2
      the0 = the0-pi
      the0 = the0*the0
      rkthb = 0.25_dp*rkthb/the0
      lsymijk = .false.
    elseif (n3ty.eq.9) then
      the0 = cos(theta(n)*degtorad)
      lsymijk = .false.
    elseif (n3ty.eq.10) then
      the0 = theta(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      ro3 = thrho3(n)
      lsymijk = .true.
    elseif (n3ty.eq.11) then
      rkthb3 = thrho3(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      the0 = theta(n)*degtorad
      lsymijk = .false.
    elseif (n3ty.eq.12) then
      the0 = theta(n)
      rkthb3 = nint(thrho1(n))
      lsymijk = .false.
    elseif (n3ty.eq.13) then
      the0 = theta(n)
      rkthb3 = thrho3(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)  
      lsymijk = .false.
    elseif (n3ty.eq.14) then
      the0 = cos(theta(n)*degtorad)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      ro3 = thrho3(n)
      lsymijk = .false.
    elseif (n3ty.eq.15) then
      rkthb3 = theta(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      ro3 = thrho3(n)
      lsymijk = .false.
    elseif (n3ty.eq.16) then
      the0 = theta(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      lsymijk = .false.
    elseif (n3ty.eq.17) then
      the0 = theta(n)*degtorad
      rkthb4 = 1.0_dp/(2.0_dp*sin(the0))**2
      rkthb3 = - 4.0_dp*rkthb4*cos(the0)
      the0 = rkthb4*(2.0_dp*cos(the0)**2 + 1.0_dp)
      lsymijk = .false.
    elseif (n3ty.eq.18) then
      rkthb3 = thrho3(n)
      ro1 = thrho1(n)
      ro2 = thrho2(n)
      the0 = cos(theta(n)*degtorad)
      lsymijk = .false.
    elseif (n3ty.eq.19) then
      lsymijk = .false.
    elseif (n3ty.eq.20) then
      the0 = 0.0_dp
      ro1 = theta(n)
      ro2 = thrho2(n)
      ro4 = thrho1(n)
      ro5 = thrho3(n)
      lsymijk = .false.
    endif
!
!  Find maximum cut-off
!
    trmax = max(tr1,tr2,tr3)
!*****************************
!  Are i and j valid types?  *
!*****************************
    lvalidij = .false.
    if (lmatch(nati,ntypi,nt1,ntyp1,.false.)) then
      if (lmatch(natj,ntypj,nt2,ntyp2,.false.)) then
        lvalidij = .true.
      elseif (lmatch(natj,ntypj,nt3,ntyp3,.false.)) then
        lvalidij = .true.
        ntmp = nt3
        nt3 = nt2
        nt2 = ntmp
        ntmp = ntyp3
        ntyp3 = ntyp2
        ntyp2 = ntmp
      endif
    elseif (lmatch(nati,ntypi,nt2,ntyp2,.false.)) then
      if (lmatch(natj,ntypj,nt1,ntyp1,.false.)) then
        lvalidij = .true.
      elseif (lmatch(natj,ntypj,nt3,ntyp3,.false.)) then
        lvalidij = .true.
        ntmp = nt3
        nt3 = nt1
        nt1 = ntmp
        ntmp = ntyp3
        ntyp3 = ntyp1
        ntyp1 = ntmp
      endif
    elseif (lmatch(nati,ntypi,nt3,ntyp3,.true.)) then
      if (lmatch(natj,ntypj,nt2,ntyp2,.false.)) then
        lvalidij = .true.
        ntmp = nt3
        nt3 = nt1
        nt1 = ntmp
        ntmp = ntyp3
        ntyp3 = ntyp1
        ntyp1 = ntmp
      elseif (lmatch(natj,ntypj,nt1,ntyp1,.true.)) then
        lvalidij = .true.
        ntmp = nt2
        nt2 = nt3
        nt3 = ntmp
        ntmp = ntyp2
        ntyp2 = ntyp3
        ntyp3 = ntmp
      endif
    endif
!
!  Not a valid three body potential based on types  = > skip to next potential
!
    if (.not.lvalidij) cycle potloop
!   
!  Dreiding option handling
!   
    if (ltdreiding(n)) then
      if (.not.bonded2donor(i)) cycle potloop
    endif
!
!  Check r12 is OK
!
    r122 = x21*x21 + y21*y21 + z21*z21
    if (r122.lt.1.0d-10.or.r122.gt.trmax) cycle potloop
    r12 = sqrt(r122)
!*******************************
!  Inner loop over third site  *
!*******************************
    kloop: do k = 1,numat
!
!  Exclude i = k and j=k cases
!
      if (k.eq.i.or.k.eq.j) cycle kloop
      natk = nat(k)
      ntypk = nftype(k)
!
!  Check k is allowed for n
!
      if (.not.lmatch(natk,ntypk,nt3,ntyp3,.true.)) cycle kloop
      ock = occuf(k)
      x31 = xfrac(k) - xc1
      y31 = yfrac(k) - yc1
      z31 = zfrac(k) - zc1
!
!  Check r13 is OK
!
      r132 = x31*x31 + y31*y31 + z31*z31
      if (r132.lt.1.0d-10.or.r132.gt.trmax) cycle kloop
!
!  Check r23 is OK
!
      x32 = x31 - x21
      y32 = y31 - y21
      z32 = z31 - z21
      r232 = x32**2 + y32**2 + z32**2
      if (r232.gt.trmax.or.r232.lt.1d-10) cycle kloop
!
!  Dreiding option handling
!
      if (ltdreiding(n)) then
        if (.not.bonded2donorJK(i,j,k)) cycle kloop
      endif
!
      r13 = sqrt(r132)
      r23 = sqrt(r232)
!***********************************************
!  Possible valid triad  -  now do full checks *
!***********************************************
      ntriad  =  0
      if (lsymijk) then
!
!  Pivot  =  i
!
        call validtriad(i,j,k,nati,ntypi,natj,ntypj,natk,ntypk,r12,r13,r23,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
        if (lvalid) then
          ntriad = ntriad + 1
          ipivottriad(ntriad) = 1
          jkorder(ntriad) = 1
          qjk(ntriad) = qf(j)*qf(k)
        endif
        if (.not.lvalid) then
          call validtriad(i,k,j,nati,ntypi,natk,ntypk,natj,ntypj,r13,r12,r23,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 1
            jkorder(ntriad) = 2
            qjk(ntriad) = qf(j)*qf(k)
          endif
        endif
!
!  Pivot  =  j
!
        if (.not.lvalid) then
          call validtriad(j,i,k,natj,ntypj,nati,ntypi,natk,ntypk,r12,r23,r13,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 2
            jkorder(ntriad) = 1
            qjk(ntriad) = qf(i)*qf(k)
          endif
        endif
        if (.not.lvalid) then
          call validtriad(j,k,i,natj,ntypj,natk,ntypk,nati,ntypi,r23,r12,r13,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 2
            jkorder(ntriad) = 2
            qjk(ntriad) = qf(i)*qf(k)
          endif
        endif
!
!  Pivot  =  k
!
        if (.not.lvalid) then
          call validtriad(k,i,j,natk,ntypk,nati,ntypi,natj,ntypj,r13,r23,r12,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 3
            jkorder(ntriad) = 1
            qjk(ntriad) = qf(i)*qf(j)
          endif
        endif
        if (.not.lvalid) then
          call validtriad(k,j,i,natk,ntypk,natj,ntypj,nati,ntypi,r23,r13,r12,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 3
            jkorder(ntriad) = 2
            qjk(ntriad) = qf(i)*qf(j)
          endif
        endif
      else
!
!  Pivot  =  i
!
        call validtriad(i,j,k,nati,ntypi,natj,ntypj,natk,ntypk,r12,r13,r23,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
        if (lvalid) then
          ntriad = ntriad + 1
          ipivottriad(ntriad) = 1
          jkorder(ntriad) = 1
          qjk(ntriad) = qf(j)*qf(k)
        else
          call validtriad(i,k,j,nati,ntypi,natk,ntypk,natj,ntypj,r13,r12,r23,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 1
            jkorder(ntriad) = 2
            qjk(ntriad) = qf(j)*qf(k)
          endif
        endif
!
!  Pivot  =  j
!
        call validtriad(j,i,k,natj,ntypj,nati,ntypi,natk,ntypk,r12,r23,r13,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
        if (lvalid) then
          ntriad = ntriad + 1
          ipivottriad(ntriad) = 2
          jkorder(ntriad) = 1
          qjk(ntriad) = qf(i)*qf(k)
        else
          call validtriad(j,k,i,natj,ntypj,natk,ntypk,nati,ntypi,r23,r12,r13,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 2
            jkorder(ntriad) = 2
            qjk(ntriad) = qf(i)*qf(k)
          endif
        endif
!
!  Pivot  =  k
!
        call validtriad(k,i,j,natk,ntypk,nati,ntypi,natj,ntypj,r13,r23,r12,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
        if (lvalid) then
          ntriad = ntriad + 1
          ipivottriad(ntriad) = 3
          jkorder(ntriad) = 1
          qjk(ntriad) = qf(i)*qf(j)
        else
          call validtriad(k,j,i,natk,ntypk,natj,ntypj,nati,ntypi,r23,r13,r12,n,0_i4,0_i4,0_i4,0_i4,0_i4,0_i4,lvalid)
          if (lvalid) then
            ntriad = ntriad + 1
            ipivottriad(ntriad) = 3
            jkorder(ntriad) = 2
            qjk(ntriad) = qf(i)*qf(j)
          endif
        endif
      endif
!
      if (ntriad.gt.0) then
!***************************
!  Valid three-body terms  *
!***************************
!
!  Is k already on the list?
!
        lkfound = .false.
        ii = 0
        do while (ii.lt.nthbk.and..not.lkfound)
          ii = ii + 1
          lkfound = (nptrmanyk(ii).eq.k)
        enddo
!
!  If k is not already on the list, initialise d33
!
        if (.not.lkfound) then
          nthbk = nthbk + 1
          if (nthbk.gt.maxmany) then
            maxmany = nthbk + 40
            call changemaxmany
          endif
          nptrmanyk(nthbk) = k
          kd3 = nthbk
          do kk = 1,54
            d33(kk,kd3) = 0.0_dp
          enddo
        else
          kd3 = ii
        endif
!***************************
!  Loop over valid triads  *
!***************************
        do n3 = 1,ntriad
          ipivot = ipivottriad(n3)
!
!  Set potential parameters that depend on atom order
!
          if (ipivot.eq.1) then
            if (jkorder(n3).eq.1) then
              rho1 = ro1
              rho2 = ro2
              rho3 = ro3
              rho4 = ro4
              rho5 = ro5
              ttr11 = tr11
              ttr21 = tr21
              ttr31 = tr31
            else
              rho1 = ro2
              rho2 = ro1
              rho3 = ro3
              rho4 = ro5
              rho5 = ro4
              ttr11 = tr21
              ttr21 = tr11
              ttr31 = tr31
            endif
          elseif (ipivot.eq.2) then
            if (jkorder(n3).eq.1) then
              rho1 = ro1
              rho2 = ro3
              rho3 = ro2
              rho4 = ro4
              rho5 = ro5
              ttr11 = tr11
              ttr21 = tr31
              ttr31 = tr21
            else
              rho1 = ro2
              rho2 = ro3
              rho3 = ro1
              rho4 = ro5
              rho5 = ro4
              ttr11 = tr21
              ttr21 = tr31
              ttr31 = tr11
            endif
          else
            if (jkorder(n3).eq.1) then
              rho1 = ro3
              rho2 = ro1
              rho3 = ro2
              rho4 = ro4
              rho5 = ro5
              ttr11 = tr31
              ttr21 = tr11
              ttr31 = tr21
            else
              rho1 = ro3
              rho2 = ro2
              rho3 = ro1
              rho4 = ro5
              rho5 = ro4
              ttr11 = tr31
              ttr21 = tr21
              ttr31 = tr11
            endif
          endif
!
!  Calculate theta / cos(theta)
!
          if (n3ty.eq.1.or.n3ty.eq.2.or.n3ty.eq.3.or.n3ty.eq.5.or.n3ty.eq.8.or.n3ty.eq.9.or.n3ty.eq.11.or.n3ty.eq.12 &
              .or.n3ty.eq.13.or.n3ty.eq.14.or.n3ty.eq.15.or.n3ty.eq.17.or.n3ty.eq.18) then
            if (ipivot.eq.1) then
              dot = x21*x31 + y21*y31 + z21*z31
              dot = dot/(r12*r13)
            elseif (ipivot.eq.2) then
              dot = x21*x32 + y21*y32 + z21*z32
              dot = - dot/(r12*r23)
            elseif (ipivot.eq.3) then
              dot = x32*x31 + y32*y31 + z32*z31
              dot = dot/(r23*r13)
            endif
            if (abs(dot).gt.0.999999999999_dp) dot = sign(1.0_dp,dot)
            ang = acos(dot)
          else
            dot = 0.0_dp
          endif
          ofct = ocij*ock
          if (n3ty.eq.19) then
            rk32 = rkthb*ofct*qjk(n3)
          else
            rk32 = rkthb*ofct
          endif
          if (n3ty.eq.12.or.n3ty.eq.17) then
            rk33 = rkthb3
          else
            rk33 = rkthb3*ofct
          endif
          if (n3ty.eq.17) then
            rk34 = rkthb4
          else
            rk34 = rkthb4*ofct
          endif
          if (n3ty.eq.15) then
            rho1 = thrho1(n)
            rho2 = thrho2(n)
            rho3 = thrho3(n)
          elseif (n3ty.eq.16) then
            rho1 = thrho1(n)
            rho2 = thrho2(n)
          endif
!*****************************************************
!  Calculate derivatives with respect to potentials  *
!*****************************************************
          call threebody(ipivot,n3ty,r12,r13,r23,e1d1,e1d2,e1d3,ethb,e2d,e3d,ttr11,ttr21,ttr31, &
                         rho1,rho2,rho3,rho4,rho5,rk32,rk33,rk34,the0,ang,dot,.true.,.true.,.true., &
                         n,qli,qlj,qlk,d0i,d0j,d0k,d1q,d2q,lthetataper(n),thetatapermin(n), &
                         thetatapermax(n))
!********************************
!  Calculate third derivatives  *
!********************************
!
!  Set up vector arrays
!
          v12(1) = x21
          v12(2) = y21
          v12(3) = z21
          v13(1) = x31
          v13(2) = y31
          v13(3) = z31
          v23(1) = x32
          v23(2) = y32
          v23(3) = z32
!
!  Derivatives of i - j with respect to r12
!
!  First, non delta terms
!
          do ii1 = 1,3
            do ii2 = 1,3
              do ii3 = 1,3
                d3(ii3,ii2,ii1) = d3(ii3,ii2,ii1) + e3d(1)*v12(ii3)*v12(ii2)*v12(ii1)
                d3(ii3,ii2,ii1) = d3(ii3,ii2,ii1) + e3d(2)*v13(ii3)*v12(ii2)*v12(ii1)
                d3(ii3,ii2,ii1) = d3(ii3,ii2,ii1) - e3d(3)*v12(ii3)*v23(ii2)*v12(ii1)
                d3(ii3,ii2,ii1) = d3(ii3,ii2,ii1) - e3d(5)*v13(ii3)*v23(ii2)*v12(ii1)
              enddo
            enddo
          enddo
!
!  Now the delta terms
!
          d3(1,1,1) = d3(1,1,1) + 3.0_dp*e2d(1)*v12(1) + e2d(2)*v13(1) - e2d(3)*v23(1)
          d3(2,1,1) = d3(2,1,1) + e2d(1)*v12(2) + e2d(2)*v13(2)
          d3(3,1,1) = d3(3,1,1) + e2d(1)*v12(3) + e2d(2)*v13(3)
          d3(1,2,1) = d3(1,2,1) + e2d(1)*v12(2) - e2d(3)*v23(2)
          d3(2,2,1) = d3(2,2,1) + e2d(1)*v12(1)
          d3(1,3,1) = d3(1,3,1) + e2d(1)*v12(3) - e2d(3)*v23(3)
          d3(3,3,1) = d3(3,3,1) + e2d(1)*v12(1)
          d3(1,1,2) = d3(1,1,2) + e2d(1)*v12(2)
          d3(2,1,2) = d3(2,1,2) + e2d(1)*v12(1) - e2d(3)*v23(1)
          d3(1,2,2) = d3(1,2,2) + e2d(1)*v12(1) + e2d(2)*v13(1)
          d3(2,2,2) = d3(2,2,2) + 3.0_dp*e2d(1)*v12(2) + e2d(2)*v13(2) - e2d(3)*v23(2)
          d3(3,2,2) = d3(3,2,2) + e2d(1)*v12(3) + e2d(2)*v13(3)
          d3(2,3,2) = d3(2,3,2) + e2d(1)*v12(3) - e2d(3)*v23(3)
          d3(3,3,2) = d3(3,3,2) + e2d(1)*v12(2)
          d3(1,1,3) = d3(1,1,3) + e2d(1)*v12(3)
          d3(3,1,3) = d3(3,1,3) + e2d(1)*v12(1) - e2d(3)*v23(1)
          d3(2,2,3) = d3(2,2,3) + e2d(1)*v12(3)
          d3(3,2,3) = d3(3,2,3) + e2d(1)*v12(2) - e2d(3)*v23(2)
          d3(1,3,3) = d3(1,3,3) + e2d(1)*v12(1) + e2d(2)*v13(1)
          d3(2,3,3) = d3(2,3,3) + e2d(1)*v12(2) + e2d(2)*v13(2)
          d3(3,3,3) = d3(3,3,3) + 3.0_dp*e2d(1)*v12(3) + e2d(2)*v13(3) - e2d(3)*v23(3)
!
!  Derivatives of i - j with respect to r13
!
!  First, non delta terms
!
          ind = 0
          do ii1 = 1,3
            do ii2 = 1,3
              do ii3 = 1,3
                ind = ind + 1
                d33(ind,kd3) = d33(ind,kd3) + e3d(2)*v12(ii3)*v12(ii2)*v13(ii1)
                d33(ind,kd3) = d33(ind,kd3) + e3d(4)*v13(ii3)*v12(ii2)*v13(ii1)
                d33(ind,kd3) = d33(ind,kd3) - e3d(5)*v12(ii3)*v23(ii2)*v13(ii1)
                d33(ind,kd3) = d33(ind,kd3) - e3d(8)*v13(ii3)*v23(ii2)*v13(ii1)
              enddo
            enddo
          enddo
!
!  Now the delta terms
!
          d33(1,kd3) = d33(1,kd3) + e2d(2)*v13(1) + e2d(2)*v12(1) - e2d(5)*v23(1)
          d33(4,kd3) = d33(4,kd3) + e2d(2)*v12(2) - e2d(5)*v23(2)
          d33(5,kd3) = d33(5,kd3) + e2d(2)*v13(1)
          d33(7,kd3) = d33(7,kd3) + e2d(2)*v12(3) - e2d(5)*v23(3)
          d33(9,kd3) = d33(9,kd3) + e2d(2)*v13(1)
          d33(10,kd3) = d33(10,kd3) + e2d(2)*v13(2)
          d33(11,kd3) = d33(11,kd3) + e2d(2)*v12(1) - e2d(5)*v23(1)
          d33(14,kd3) = d33(14,kd3) + e2d(2)*v13(2) + e2d(2)*v12(2) - e2d(5)*v23(2)
          d33(17,kd3) = d33(17,kd3) + e2d(2)*v12(3) - e2d(5)*v23(3)
          d33(18,kd3) = d33(18,kd3) + e2d(2)*v13(2)
          d33(19,kd3) = d33(19,kd3) + e2d(2)*v13(3)
          d33(21,kd3) = d33(21,kd3) + e2d(2)*v12(1) - e2d(5)*v23(1)
          d33(23,kd3) = d33(23,kd3) + e2d(2)*v13(3)
          d33(24,kd3) = d33(24,kd3) + e2d(2)*v12(2) - e2d(5)*v23(2)
          d33(27,kd3) = d33(27,kd3) + e2d(2)*v13(3) + e2d(2)*v12(3) - e2d(5)*v23(3)
!
!  Derivatives of i - j with respect to r23
!
!  First, non delta terms
!
          ind = 27
          do ii1 = 1,3
            do ii2 = 1,3
              do ii3 = 1,3
                ind = ind + 1
                d33(ind,kd3) = d33(ind,kd3) + e3d(3)*v12(ii3)*v12(ii2)*v23(ii1)
                d33(ind,kd3) = d33(ind,kd3) + e3d(5)*v13(ii3)*v12(ii2)*v23(ii1)
                d33(ind,kd3) = d33(ind,kd3) - e3d(6)*v12(ii3)*v23(ii2)*v23(ii1)
                d33(ind,kd3) = d33(ind,kd3) - e3d(9)*v13(ii3)*v23(ii2)*v23(ii1)
              enddo
            enddo
          enddo
!
!  Now the delta terms
!
          d33(28,kd3) = d33(28,kd3) + e2d(3)*v23(1) - e2d(3)*v12(1) - e2d(5)*v13(1)
          d33(29,kd3) = d33(29,kd3) - e2d(3)*v12(2) - e2d(5)*v13(2)
          d33(30,kd3) = d33(30,kd3) - e2d(3)*v12(3) - e2d(5)*v13(3)
          d33(32,kd3) = d33(32,kd3) + e2d(3)*v23(1)
          d33(36,kd3) = d33(36,kd3) + e2d(3)*v23(1)
          d33(37,kd3) = d33(37,kd3) + e2d(3)*v23(2)
          d33(40,kd3) = d33(40,kd3) - e2d(3)*v12(1) - e2d(5)*v13(1)
          d33(41,kd3) = d33(41,kd3) + e2d(3)*v23(2) - e2d(3)*v12(2) - e2d(5)*v13(2)
          d33(42,kd3) = d33(42,kd3) - e2d(3)*v12(3) - e2d(5)*v13(3)
          d33(45,kd3) = d33(45,kd3) + e2d(3)*v23(2)
          d33(46,kd3) = d33(46,kd3) + e2d(3)*v23(3)
          d33(50,kd3) = d33(50,kd3) + e2d(3)*v23(3)
          d33(52,kd3) = d33(52,kd3) - e2d(3)*v12(1) - e2d(5)*v13(1)
          d33(53,kd3) = d33(53,kd3) - e2d(3)*v12(2) - e2d(5)*v13(2)
          d33(54,kd3) = d33(54,kd3) + e2d(3)*v23(3) - e2d(3)*v12(3) - e2d(5)*v13(3)
!
!  End of loop over valid triads
!
        enddo
      endif
!***********************
!  End of derivatives  *
!***********************
!
!  End of inner loop over third atoms
!
    enddo kloop
!
!  End of outer loops over potentials
!
  enddo potloop
!
  return
  end
