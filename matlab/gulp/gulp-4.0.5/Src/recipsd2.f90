  subroutine recipsd2(erecip,ec6,lgrad1,lgrad2)
!
!  Calculation of electrostatic potential and derivatives,
!  including strain derivatives. Reciprocal space part.
!  Vector version
!
!  Freezing now added.
!
!   4/95 Speed up added to avoid recalculation of cos and sine
!   5/95 Algorithm changed to aid the above - lveck is .false.
!        if either first or second derivatives are being calculated
!        as this appears to give large speed ups.
!   8/95 Ewald sum for 1/r**6 added
!   8/95 Derivatives for lveck removed as this is only used for the
!        energy - simplifies routine greatly!
!  11/96 Compression of second derivatives added when lfreeze=.true.
!        Because i(opt)-j(frozen) d2 blocks are stored in i(asym)-
!        i(full) block, it is necessary to exclude self-terms in the
!        the second derivatives.
!  12/97 Modification of energy/derivatives made purely local
!   4/98 ESFF Lennard-Jones form now allowed for
!   7/00 Dynamic memory allocation added
!   7/00 lfirst removed for safety
!   2/01 Electric field calculation added for polarisation
!   4/02 derv3 indexing for strain correction fixed for lfreeze case
!   9/04 Charge first derivatives added
!  10/04 oldel option removed as no longer used
!  11/04 Sqrt pi taken from module
!  12/07 Unused variables removed
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   5/09 Adding of contribution to sderv2 regardless of lgrad2 trapped
!   5/12 Atomic stresses added
!   5/12 Atomic stresses removed for routines involving symmetry
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
  use derivatives
  use kspace
  use optimisation
  use polarise
  use potentialxyz
  use shell
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                     :: lgrad1
  logical,     intent(in)                     :: lgrad2
  real(dp),    intent(out)                    :: ec6
  real(dp),    intent(out)                    :: erecip
!
!  Local variables
!
  integer(i4)                                 :: e
  integer(i4)                                 :: f
  integer(i4)                                 :: g
  integer(i4)                                 :: i
  integer(i4)                                 :: idk
  integer(i4)                                 :: iv
  integer(i4)                                 :: ix
  integer(i4)                                 :: iy
  integer(i4)                                 :: iz
  integer(i4)                                 :: ixf
  integer(i4)                                 :: iyf
  integer(i4)                                 :: izf
  integer(i4)                                 :: ixfo
  integer(i4)                                 :: iyfo
  integer(i4)                                 :: izfo
  integer(i4)                                 :: j
  integer(i4)                                 :: jx
  integer(i4)                                 :: jy
  integer(i4)                                 :: jz
  integer(i4)                                 :: jxc
  integer(i4)                                 :: jyc
  integer(i4)                                 :: jzc
  integer(i4)                                 :: kk
  integer(i4)                                 :: kl
  integer(i4)                                 :: ll
  integer(i4)                                 :: n
  integer(i4)                                 :: nati
  integer(i4)                                 :: natj
  integer(i4)                                 :: nreli
  integer(i4)                                 :: ntypj
  integer(i4)                                 :: ntypi
  integer(i4)                                 :: status
  logical                                     :: lc6loc
  logical                                     :: ldoc6
  logical                                     :: lopi
  logical                                     :: lopj
  logical                                     :: lsg1
  logical                                     :: lveck
  real(dp)                                    :: arg
  real(dp)                                    :: argck
  real(dp)                                    :: arge
  real(dp)                                    :: c6i
  real(dp)                                    :: c6j
  real(dp)                                    :: c6self2
  real(dp)                                    :: c6t1
  real(dp)                                    :: c6t2
  real(dp)                                    :: c6t3
  real(dp)                                    :: c6t4
  real(dp)                                    :: c6tot
  real(dp)                                    :: cos6
  real(dp)                                    :: cosa
  real(dp)                                    :: cosi
  real(dp)                                    :: cosq
  real(dp)                                    :: cputime
  real(dp)                                    :: csin6
  real(dp)                                    :: csink
  real(dp)                                    :: csink6
  real(dp)                                    :: csinq
  real(dp)                                    :: csprod
  real(dp)                                    :: d1trm
  real(dp)                                    :: d2trm
  real(dp)                                    :: d3trm
  real(dp)                                    :: d21q
  real(dp)                                    :: d22q
  real(dp)                                    :: d23q
  real(dp)                                    :: d24q
  real(dp)                                    :: d25q
  real(dp)                                    :: d26q
  real(dp)                                    :: derfc
  real(dp)                                    :: eltrm
  real(dp)                                    :: erecipr
  real(dp)                                    :: esum
  real(dp)                                    :: factor
  real(dp), dimension(:),   allocatable       :: ktrm3
  real(dp), dimension(:),   allocatable       :: ktrm6
  real(dp), dimension(:),   allocatable       :: ktrm62
  real(dp), dimension(:),   allocatable       :: ktrm63
  real(dp)                                    :: kvv(3)
  real(dp)                                    :: oci
  real(dp)                                    :: ocj
  real(dp), dimension(:),   allocatable       :: phsq
  real(dp)                                    :: phsqk
  real(dp)                                    :: phsqk6
  real(dp)                                    :: phsqksum
  real(dp)                                    :: qfct
  real(dp)                                    :: qli
  real(dp)                                    :: qlj
  real(dp)                                    :: rangstoev
  real(dp)                                    :: reta
  real(dp)                                    :: rk
  real(dp)                                    :: rk2
  real(dp)                                    :: rketa2
  real(dp)                                    :: rneqi
  real(dp)                                    :: rrk2
  real(dp)                                    :: sina
  real(dp)                                    :: sini
  real(dp)                                    :: sinek
  real(dp)                                    :: sinek6
  real(dp)                                    :: sineq
  real(dp)                                    :: sinqx
  real(dp)                                    :: sinqy
  real(dp)                                    :: sinqz
  real(dp)                                    :: strm1
  real(dp)                                    :: time0
  real(dp)                                    :: time1
  real(dp), dimension(:,:), allocatable       :: tmp
  real(dp)                                    :: tmps(6)
  real(dp)                                    :: trmk
  real(dp)                                    :: trmk2
  real(dp)                                    :: trmk3
  real(dp)                                    :: trmk6
  real(dp)                                    :: trmk62
  real(dp)                                    :: trmk63
  real(dp)                                    :: xci
  real(dp)                                    :: yci
  real(dp)                                    :: zci
  real(dp)                                    :: xd
  real(dp)                                    :: yd
  real(dp)                                    :: zd
  real(dp)                                    :: xrkk
  real(dp)                                    :: yrkk
  real(dp)                                    :: zrkk
  real(dp)                                    :: xpon
!
  time0 = cputime()
!
!  Zero energy
!
  erecip = 0.0_dp
  ec6 = 0.0_dp
  lveck = (nkvec.ge.numat)
  lsg1 = (lstr.and.lgrad1)
  lc6loc = (lc6.and.ndim.eq.3)
!
!  Modification added to increase speed - outer loop
!  over k vectors is faster for derivatives as
!  recalculation of cos and sin is avoided.
!
  if (lgrad1.or.lgrad2) lveck = .false.
!
!  If Ewald sum for dispersion then don't use lveck
!  as this requires more vector storage
!
  if (lc6loc) lveck = .false.
!
!  Allocate local memory
!
  allocate(phsq(nkvec),stat=status)
  if (status/=0) call outofmemory('recipsd2','phsq')
  allocate(ktrm3(nkvec),stat=status)
  if (status/=0) call outofmemory('recipsd2','ktrm3')
  allocate(ktrm6(nkvec),stat=status)
  if (status/=0) call outofmemory('recipsd2','ktrm6')
  allocate(ktrm62(nkvec),stat=status)
  if (status/=0) call outofmemory('recipsd2','ktrm62')
  allocate(ktrm63(nkvec),stat=status)
  if (status/=0) call outofmemory('recipsd2','ktrm63')
  allocate(tmp(nkvec,6),stat=status)
  if (status/=0) call outofmemory('recipsd2','tmp')
!
!  Setup
!
  eta4 = 0.25_dp/eta
  reta = eta4/eta
  rangstoev = 1.0_dp/angstoev
!*******************************
!  Sum 1/r**6 + coulomb terms  *
!*******************************
  if (lc6loc) then
    c6t1 = vol4pi*rangstoev*sqrtpi/48.0_dp
!
!  Reciprocal space self term
!
    c6self2 = 4.0_dp*c6t1*eta*seta
    if (lra) then
      kvv(1) = kv(1,1)
      kvv(2) = kv(2,2)
      kvv(3) = kv(3,3)
      do i = 1,nkvec
        idk = indk(i)
        e = (idk/6400) - 40
        if (e.eq.0) then
          factor = 1.0_dp
        else
          factor = 2.0_dp
        endif
        idk = idk - (e+40)*6400
        f = (idk/80)-40
        g = idk - (f+40)*80 - 40
        xrk(i) = e*kvv(1)
        yrk(i) = f*kvv(2)
        zrk(i) = g*kvv(3)
        rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i) + zrk(i)*zrk(i)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(i) = xpon*vol4pi*factor*rrk2
        rk = sqrt(rk2)
        rketa2 = 0.5_dp*rk/seta
        c6t2 = sqrtpi*derfc(rketa2)
        rketa2 = 1.0_dp/rketa2
        c6t3 = 0.5_dp*rketa2**3 - rketa2
        c6t3 = c6t3*xpon
        c6t4 = c6t1*rk2*rk*factor
        ktrm6(i) = c6t4*(c6t2 + c6t3)
        if (lgrad1) then
          ktrms(i) = - 2.0_dp*ktrm(i)*(4.0_dp*eta + rk2)*eta4*rrk2
          ktrm62(i) = 3.0_dp*ktrm6(i)*rrk2
          ktrm62(i) = ktrm62(i) - c6t4*xpon*(12.0_dp*eta*seta*rrk2*rrk2)/rk
          if (lgrad2) then
            ktrm3(i) = (ktrm(i)*reta-4.0_dp*ktrms(i)*rrk2)
            ktrm63(i) = 3.0_dp*c6t4*c6t2*rrk2*rrk2
          endif
        endif
      enddo
    else
      do i = 1,nkvec
        idk = indk(i)
        e = (idk/6400) - 40
        idk = idk - (e+40)*6400
        f = (idk/80) - 40
        g = idk - (f+40)*80 - 40
        factor = 2.0_dp
        if (e.eq.0.and.nkangle.eq.1) then
          factor = 1.0_dp
        elseif (f.eq.0.and.nkangle.eq.2) then
          factor = 1.0_dp
        elseif (g.eq.0.and.nkangle.eq.3) then
          factor = 1.0_dp
        elseif (nkangle.eq.0) then
          factor = 1.0_dp
        endif
        xrk(i) = e*kv(1,1) + f*kv(1,2) + g*kv(1,3)
        yrk(i) = e*kv(2,1) + f*kv(2,2) + g*kv(2,3)
        zrk(i) = e*kv(3,1) + f*kv(3,2) + g*kv(3,3)
        rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i) + zrk(i)*zrk(i)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(i) = xpon*vol4pi*factor*rrk2
        rk = sqrt(rk2)
        rketa2 = 0.5_dp*rk/seta
        c6t2 = sqrtpi*derfc(rketa2)
        rketa2 = 1.0_dp/rketa2
        c6t3 = 0.5_dp*rketa2**3 - rketa2
        c6t3 = c6t3*xpon
        c6t4 = c6t1*rk2*rk*factor
        ktrm6(i) = c6t4*(c6t2 + c6t3)
        if (lgrad1) then
          ktrms(i) = - 2.0_dp*ktrm(i)*(4.0_dp*eta + rk2)*eta4*rrk2
          ktrm62(i) = 3.0_dp*ktrm6(i)*rrk2
          ktrm62(i) = ktrm62(i) - c6t4*xpon*(12.0_dp*eta*seta*rrk2*rrk2)/rk
          if (lgrad2) then
            ktrm3(i) = (ktrm(i)*reta - 4.0_dp*ktrms(i)*rrk2)
            ktrm63(i) = 3.0_dp*c6t4*c6t2*rrk2*rrk2
          endif
        endif
      enddo
    endif
  else
!*****************
!  Coulomb only  *
!*****************
    if (lra) then
      kvv(1) = kv(1,1)
      kvv(2) = kv(2,2)
      kvv(3) = kv(3,3)
      do i = 1,nkvec
        idk = indk(i)
        e = (idk/6400) - 40
        if (e.eq.0) then
          factor = 1.0_dp
        else
          factor = 2.0_dp
        endif
        idk = idk - (e+40)*6400
        f = (idk/80) - 40
        g = idk - (f+40)*80 - 40
        xrk(i) = e*kvv(1)
        yrk(i) = f*kvv(2)
        zrk(i) = g*kvv(3)
        rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i) + zrk(i)*zrk(i)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(i) = xpon*vol4pi*factor*rrk2
        if (lgrad1) then
          ktrms(i) = - 2.0_dp*ktrm(i)*(4.0_dp*eta + rk2)*eta4*rrk2
          if (lgrad2) then
            ktrm3(i) = (ktrm(i)*reta - 4.0_dp*ktrms(i)*rrk2)
          endif
        endif
      enddo
    else
      do i = 1,nkvec
        idk = indk(i)
        e = (idk/6400) - 40
        idk = idk - (e+40)*6400
        f = (idk/80) - 40
        g = idk - (f+40)*80 - 40
        factor = 2.0_dp
        if (e.eq.0.and.nkangle.eq.1) then
          factor = 1.0_dp
        elseif (f.eq.0.and.nkangle.eq.2) then
          factor = 1.0_dp
        elseif (g.eq.0.and.nkangle.eq.3) then
          factor = 1.0_dp
        elseif (nkangle.eq.0) then
          factor = 1.0_dp
        endif
        xrk(i) = e*kv(1,1) + f*kv(1,2) + g*kv(1,3)
        yrk(i) = e*kv(2,1) + f*kv(2,2) + g*kv(2,3)
        zrk(i) = e*kv(3,1) + f*kv(3,2) + g*kv(3,3)
        rk2 = xrk(i)*xrk(i) + yrk(i)*yrk(i) + zrk(i)*zrk(i)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(i) = xpon*vol4pi*factor*rrk2
        if (lgrad1) then
          ktrms(i) = - 2.0_dp*ktrm(i)*(4.0_dp*eta+rk2)*eta4*rrk2
          if (lgrad2) then
            ktrm3(i) = (ktrm(i)*reta - 4.0_dp*ktrms(i)*rrk2)
          endif
        endif
      enddo
    endif
  endif
!
!  End of set-up section
!
  if (lnorecip) goto 999
!*****************************
!  Vectorise over k vectors  *
!*****************************
  if (lveck) then
!
!  Start loop over cluster atom - unit cell atom pairs
!
    do iv = 1,nkvec
      csin(iv) = 0.0_dp
      sine(iv) = 0.0_dp
    enddo
    do i = 1,numat
      qli = qf(i)*occuf(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      do iv = 1,nkvec
        argc(iv) = xrk(iv)*xci + yrk(iv)*yci+argc(iv) + zrk(iv)*zci+argc(iv)
      enddo
      do iv = 1,nkvec
        csin(iv) = csin(iv) + qli*cos(argc(iv))
        sine(iv) = sine(iv) + qli*sin(argc(iv))
      enddo
    enddo
    do iv = 1,nkvec
      phsq(iv) = (csin(iv)*csin(iv) + sine(iv)*sine(iv))
    enddo
!**********************
!  Lattice energy     *
!**********************
    erecipr = 0.0_dp
    do iv = 1,nkvec
      erecipr = erecipr + ktrm(iv)*phsq(iv)
    enddo
    erecip = erecip + 0.5_dp*angstoev*erecipr
  elseif ((lc6loc.and..not.lc6one).or.lDoQDeriv1) then
!********************************************************************
!  Algorithm for cases where dispersion cannot be factorised into   *
!  one centre terms or where charge derivatives are required        *
!********************************************************************
    if (lsg1.or.lgrad2) then
      do iv = 1,nkvec
        tmp(iv,1) = xrk(iv)*xrk(iv)
        tmp(iv,2) = yrk(iv)*yrk(iv)
        tmp(iv,3) = zrk(iv)*zrk(iv)
        tmp(iv,4) = yrk(iv)*zrk(iv)
        tmp(iv,5) = xrk(iv)*zrk(iv)
        tmp(iv,6) = xrk(iv)*yrk(iv)
      enddo
    endif
    ix = - 2
    iy = - 1
    iz =   0
    ixf = 1
    iyf = 2
    izf = 3
    ixfo = 1
    iyfo = 2
    izfo = 3
    do i = 1,nasym
      oci = occua(i)*dble(neqv(i))*angstoev
      qli = qa(i)*oci
      nati = iatn(i)
      ntypi = natype(i)
      xci = xalat(i)
      yci = yalat(i)
      zci = zalat(i)
      nreli = nrel2(i)
      lopi = lopf(i)
      if (.not.lfreeze.or.lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        ixf = ixfo
        iyf = iyfo
        izf = izfo
        ixfo = ixfo + 3*neqv(i)
        iyfo = iyfo + 3*neqv(i)
        izfo = izfo + 3*neqv(i)
      endif
      jx = - 2
      jy = - 1
      jz =   0
      do j = 1,numat
        ocj = occuf(j)
        qlj = qf(j)*ocj
        natj = nat(j)
        ntypj = nftype(j)
        lopj = lopf(nrelat(j))
        if (.not.lfreeze.or.lopj) then
          jx = jx + 3
          jy = jy + 3
          jz = jz + 3
        endif
        if (lc6loc) then
!
!  Find C6 term for pair
!
          c6tot = 0.0_dp
          do n = 1,npote
            if (nati.eq.nspec1(n).and.natj.eq.nspec2(n)) then
              if ((ntypi.eq.nptyp1(n).or.nptyp1(n).eq.0).and.(ntypj.eq.nptyp2(n).or.nptyp2(n).eq.0)) then
                if (nptype(n).eq.1.or.nptype(n).eq.7) then
                  c6tot = c6tot + twopot(3,n)
                elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
                  c6tot = c6tot + twopot(2,n)
                endif
              endif
            elseif (natj.eq.nspec1(n).and.nati.eq.nspec2(n)) then
              if ((ntypj.eq.nptyp1(n).or.nptyp1(n).eq.0).and.(ntypi.eq.nptyp2(n).or.nptyp2(n).eq.0)) then
                if (nptype(n).eq.1.or.nptype(n).eq.7) then
                  c6tot = c6tot + twopot(3,n)
                elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
                  c6tot = c6tot + twopot(2,n)
                endif
              endif
            endif
          enddo
        endif
        ldoc6 = (lc6loc.and.abs(c6tot).gt.1.0d-4)
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xci
        yd = yclat(j) - yci
        zd = zclat(j) - zci
        qfct = qli*qlj
        csinq = 0.0_dp
        if (lgrad1) then
          sinqx = 0.0_dp
          sinqy = 0.0_dp
          sinqz = 0.0_dp
          if (lgrad2) then
            d21q = 0.0_dp
            d22q = 0.0_dp
            d23q = 0.0_dp
            d24q = 0.0_dp
            d25q = 0.0_dp
            d26q = 0.0_dp
          endif
        endif
        if (ldoc6) then
          c6tot = c6tot*oci*ocj
          if (lgrad1) then
            csin6 = 0.0_dp
            do iv = 1,nkvec
              xrkk = xrk(iv)
              yrkk = yrk(iv)
              zrkk = zrk(iv)
              arg = xrkk*xd + yrkk*yd + zrkk*zd
              cosa = cos(arg)
              sina = sin(arg)
              cosq = cosa*ktrm(iv)
              cos6 = cosa*ktrm6(iv)*c6tot
              csinq = csinq + cosq
              cosq = cosq*qfct
              csin6 = csin6 + cos6
              d1trm = (ktrm(iv)*qfct-ktrm6(iv)*c6tot)*sina
              sinqx = sinqx + d1trm*xrkk
              sinqy = sinqy + d1trm*yrkk
              sinqz = sinqz + d1trm*zrkk
              if (lgrad2) then
                d2trm = cosq - cos6
                d21q = d21q + d2trm*tmp(iv,1)
                d22q = d22q + d2trm*tmp(iv,2)
                d23q = d23q + d2trm*tmp(iv,3)
                d24q = d24q + d2trm*tmp(iv,4)
                d25q = d25q + d2trm*tmp(iv,5)
                d26q = d26q + d2trm*tmp(iv,6)
              endif
              if (lsg1) then
                strm1 = (ktrms(iv)*qfct - ktrm62(iv)*c6tot)
                d3trm = strm1*sina
                strm1 = 0.5_dp*strm1*cosa
                strderv(1) = strderv(1) - strm1*tmp(iv,1)
                strderv(2) = strderv(2) - strm1*tmp(iv,2)
                strderv(3) = strderv(3) - strm1*tmp(iv,3)
                strderv(4) = strderv(4) - strm1*tmp(iv,4)
                strderv(5) = strderv(5) - strm1*tmp(iv,5)
                strderv(6) = strderv(6) - strm1*tmp(iv,6)
                if (lgrad2) then
                  eltrm = 0.5_dp*(ktrm3(iv)*qfct-ktrm63(iv)*c6tot)*cosa
                  do kk = 1,6
                    do ll = kk,6
                      sderv2(ll,kk) = sderv2(ll,kk) + eltrm*tmp(iv,kk)*tmp(iv,ll)
                    enddo
                  enddo
                  if (.not.lfreeze.or.lopi) then
                    do kk = 1,6
                      derv3(ix,kk) = derv3(ix,kk) - tmp(iv,kk)*xrkk*d3trm
                      derv3(iy,kk) = derv3(iy,kk) - tmp(iv,kk)*yrkk*d3trm
                      derv3(iz,kk) = derv3(iz,kk) - tmp(iv,kk)*zrkk*d3trm
                    enddo
                  endif
                endif
              endif
            enddo
          else
            csin6 = 0.0_dp
            do iv = 1,nkvec
              arg = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
              cosa = cos(arg)
              csinq = csinq + cosa*ktrm(iv)
              csin6 = csin6 + cosa*ktrm6(iv)
            enddo
            csin6 = csin6*c6tot
          endif
!*******************
!  Lattice energy  *
!*******************
          erecip = erecip + 0.5_dp*csinq*qfct
          ec6 = ec6 - 0.5_dp*(csin6 + c6self2*c6tot)
          esum = 0.5_dp*csinq*qfct - 0.5_dp*(csin6 + c6self2*c6tot)
!*****************************
!  Charge first derivatives  *
!*****************************
!              if (lgrad1.and.lDoQDeriv1) then
!                call d1charges(i,lopi,1_i4,0.5_dp*csinq*qlj)
!              endif
        else
          if (lgrad1) then
            do iv = 1,nkvec
              xrkk = xrk(iv)
              yrkk = yrk(iv)
              zrkk = zrk(iv)
              arg = xrkk*xd + yrkk*yd + zrkk*zd
              cosa = cos(arg)
              sina = sin(arg)*qfct
              cosq = cosa*ktrm(iv)
              cosa = cosa*qfct
              csinq = csinq + cosq
              cosq = cosq*qfct
              sineq = sina*ktrm(iv)
              sinqx = sinqx + sineq*xrkk
              sinqy = sinqy + sineq*yrkk
              sinqz = sinqz + sineq*zrkk
              if (lgrad2) then
                d21q = d21q + cosq*tmp(iv,1)
                d22q = d22q + cosq*tmp(iv,2)
                d23q = d23q + cosq*tmp(iv,3)
                d24q = d24q + cosq*tmp(iv,4)
                d25q = d25q + cosq*tmp(iv,5)
                d26q = d26q + cosq*tmp(iv,6)
              endif
              if (lsg1) then
                strm1 = 0.5_dp*ktrms(iv)*cosa
                d3trm = ktrms(iv)*sina
                strderv(1) = strderv(1) - strm1*tmp(iv,1)
                strderv(2) = strderv(2) - strm1*tmp(iv,2)
                strderv(3) = strderv(3) - strm1*tmp(iv,3)
                strderv(4) = strderv(4) - strm1*tmp(iv,4)
                strderv(5) = strderv(5) - strm1*tmp(iv,5)
                strderv(6) = strderv(6) - strm1*tmp(iv,6)
                if (lgrad2) then
                  eltrm = 0.5_dp*ktrm3(iv)*cosa
                  do kk = 1,6
                    do ll = kk,6
                      sderv2(ll,kk) = sderv2(ll,kk) + eltrm*tmp(iv,kk)*tmp(iv,ll)
                    enddo
                  enddo
                  if (.not.lfreeze.or.lopi) then
                    do kk = 1,6
                      derv3(ix,kk) = derv3(ix,kk) - tmp(iv,kk)*xrkk*d3trm
                      derv3(iy,kk) = derv3(iy,kk) - tmp(iv,kk)*yrkk*d3trm
                      derv3(iz,kk) = derv3(iz,kk) - tmp(iv,kk)*zrkk*d3trm
                    enddo
                  endif
                endif
              endif
            enddo
          else
            csinq = 0.0_dp
            do iv = 1,nkvec
              arg = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
              cosa = cos(arg)
              csinq = csinq + cosa*ktrm(iv)
            enddo
          endif
!*******************
!  Lattice energy  *
!*******************
          erecip = erecip + 0.5_dp*csinq*qfct
          esum = 0.5_dp*csinq*qfct
!*****************************
!  Charge first derivatives  *
!*****************************
!              if (lgrad1.and.lDoQDeriv1) then
!                call d1charges(i,lopi,1_i4,0.5_dp*csinq*qlj)
!              endif
        endif
!
!  Internal derivatives
!
        if (lgrad1.and.(.not.lfreeze.or.lopi)) then
          xdrv(i) = xdrv(i) + sinqx
          ydrv(i) = ydrv(i) + sinqy
          zdrv(i) = zdrv(i) + sinqz
          if (lgrad2.and.j.ne.nreli) then
            if (.not.lfreeze.or.lopj) then
              jxc = jx
              jyc = jy
              jzc = jz
            else
              jxc = ixf
              jyc = iyf
              jzc = izf
            endif
            derv2(jxc,ix) = derv2(jxc,ix) + d21q
            derv2(jyc,ix) = derv2(jyc,ix) + d26q
            derv2(jzc,ix) = derv2(jzc,ix) + d25q
            derv2(jxc,iy) = derv2(jxc,iy) + d26q
            derv2(jyc,iy) = derv2(jyc,iy) + d22q
            derv2(jzc,iy) = derv2(jzc,iy) + d24q
            derv2(jxc,iz) = derv2(jxc,iz) + d25q
            derv2(jyc,iz) = derv2(jyc,iz) + d24q
            derv2(jzc,iz) = derv2(jzc,iz) + d23q
          endif
        endif
      enddo
    enddo
  else
!***********************************
!  Vectorise over number of atoms  *
!***********************************
!
!  Start loop over k vectors
!
    do iv = 1,nkvec
      csink = 0.0_dp
      sinek = 0.0_dp
      xrkk = xrk(iv)
      yrkk = yrk(iv)
      zrkk = zrk(iv)
      trmk = ktrm(iv)*angstoev
      if (lgrad1) then
        trmk2 = ktrms(iv)*angstoev
        if (lgrad2) then
          trmk3 = ktrm3(iv)*angstoev
        endif
      endif
      if (lc6loc.and.lc6one) then
        trmk6 = ktrm6(iv)*angstoev
        if (lgrad1) then
          trmk62 = ktrm62(iv)*angstoev
          if (lgrad2) then
            trmk63 = ktrm63(iv)*angstoev
          endif
        endif
        csink6 = 0.0_dp
        sinek6 = 0.0_dp
        do i = 1,numat
          oci = occuf(i)
          qli = qf(i)*oci
          c6i = c6f(i)*oci
          xci = xclat(i)
          yci = yclat(i)
          zci = zclat(i)
          argc(i) = xrkk*xci + yrkk*yci + zrkk*zci
          csin(i) = cos(argc(i))
          sine(i) = sin(argc(i))
          csink6 = csink6 + csin(i)*c6i
          sinek6 = sinek6 + sine(i)*c6i
          csink = csink + csin(i)*qli
          sinek = sinek + sine(i)*qli
        enddo
        phsqk6 = (csink6*csink6 + sinek6*sinek6)
      else
        do i = 1,numat
          qli = qf(i)*occuf(i)
          xci = xclat(i)
          yci = yclat(i)
          zci = zclat(i)
          argc(i) = xrkk*xci + yrkk*yci + zrkk*zci
          csin(i) = cos(argc(i))*qli
          sine(i) = sin(argc(i))*qli
          csink = csink + csin(i)
          sinek = sinek + sine(i)
        enddo
      endif
      phsqk = (csink*csink + sinek*sinek)
!**********************
!  Lattice energy     *
!**********************
      erecip = erecip + 0.5_dp*trmk*phsqk
      if (lc6loc.and.lc6one) ec6 = ec6 - 0.5_dp*trmk6*phsqk6
!**********************
!  Strain derivatives *
!**********************
      if (lsg1.or.lgrad2) then
!
!  First derivatives - except erecip term which is added after
!  strderv has been used for second derivatives
!
        tmps(1) = xrkk*xrkk
        tmps(2) = yrkk*yrkk
        tmps(3) = zrkk*zrkk
        tmps(4) = yrkk*zrkk
        tmps(5) = xrkk*zrkk
        tmps(6) = xrkk*yrkk
      endif
      if (lsg1) then
        if (lc6loc.and.lc6one) then
          do i = 1,6
            strderv(i) = strderv(i) - 0.5_dp*trmk2*phsqk*tmps(i)
            strderv(i) = strderv(i) + 0.5_dp*trmk62*phsqk6*tmps(i)
          enddo
          if (lgrad2) eltrm = 0.5_dp*(trmk3*phsqk - trmk63*phsqk6)
        else
          do i = 1,6
            strderv(i) = strderv(i) - 0.5_dp*trmk2*phsqk*tmps(i)
          enddo
          if (lgrad2) eltrm = 0.5_dp*trmk3*phsqk
        endif
        if (lgrad2) then
!
!  Second derivatives
!
!  General terms
!
          do i = 1,6
            do j = i,6
              sderv2(j,i) = sderv2(j,i) + eltrm*tmps(i)*tmps(j)
            enddo
          enddo
        endif
      endif
!************************
!  Internal derivatives *
!************************
!
!  First and second derivatives
!
      if (lgrad1) then
        ix = - 2
        iy = - 1
        iz =   0
        ixf = 1
        iyf = 2
        izf = 3
        ixfo = 1
        iyfo = 2
        izfo = 3
        do i = 1,nasym
          lopi = lopf(i)
          if (.not.lfreeze.or.lopi) then
            rneqi = dble(neqv(i))
            oci = occua(i)*rneqi
            qli = qa(i)*oci
            nreli = nrel2(i)
            if (lc6loc.and.lc6one) c6i = c6a(i)*oci
!
!  Excursion into second derivatives
!
            if (lgrad2) then
              ix = ix + 3
              iy = iy + 3
              iz = iz + 3
              ixf = ixfo
              iyf = iyfo
              izf = izfo
              ixfo = ixfo + 3*neqv(i)
              iyfo = iyfo + 3*neqv(i)
              izfo = izfo + 3*neqv(i)
              jx = - 2
              jy = - 1
              jz =   0
              do j = 1,numat
                lopj = lopf(nrelat(j))
                if (.not.lfreeze.or.lopj) then
                  jx = jx + 3
                  jy = jy + 3
                  jz = jz + 3
                  jxc = jx
                  jyc = jy
                  jzc = jz
                else
                  jxc = ixf
                  jyc = iyf
                  jzc = izf
                endif
                if (j.ne.nreli) then
                  ocj = occuf(j)
                  qlj = qf(j)*ocj
                  if (lc6loc.and.lc6one) c6j = c6f(j)*ocj
                  csprod = (csin(nreli)*csin(j) + sine(nreli)*sine(j))
                  if (lc6loc.and.lc6one) then
                    argck = csprod*(trmk*qli*qlj - trmk6*c6i*c6j)
                  else
                    argck = rneqi*trmk*csprod
                  endif
                  derv2(jxc,ix) = derv2(jxc,ix) + argck*tmps(1)
                  derv2(jyc,ix) = derv2(jyc,ix) + argck*tmps(6)
                  derv2(jzc,ix) = derv2(jzc,ix) + argck*tmps(5)
                  derv2(jxc,iy) = derv2(jxc,iy) + argck*tmps(6)
                  derv2(jyc,iy) = derv2(jyc,iy) + argck*tmps(2)
                  derv2(jzc,iy) = derv2(jzc,iy) + argck*tmps(4)
                  derv2(jxc,iz) = derv2(jxc,iz) + argck*tmps(5)
                  derv2(jyc,iz) = derv2(jyc,iz) + argck*tmps(4)
                  derv2(jzc,iz) = derv2(jzc,iz) + argck*tmps(3)
                endif
              enddo
            endif
!
!  Return to first derivatives
!
            if (lc6loc.and.lc6one) then
              phsqk = qli*(csin(nreli)*sinek - sine(nreli)*csink)
              phsqk6 = c6i*(csin(nreli)*sinek6 - sine(nreli)*csink6)
              phsqksum = phsqk*trmk - phsqk6*trmk6
            else
              phsqk = rneqi*(csin(nreli)*sinek - sine(nreli)*csink)
              phsqksum = trmk*phsqk
            endif
            xdrv(i) = xdrv(i) + phsqksum*xrkk
            ydrv(i) = ydrv(i) + phsqksum*yrkk
            zdrv(i) = zdrv(i) + phsqksum*zrkk
!
!  Mixed strain - internal derivatives
!
            if (lgrad2.and.lsg1) then
              if (lc6loc.and.lc6one) then
                argck = phsqk*trmk2 - phsqk6*trmk62
              else
                argck = phsqk*trmk2
              endif
              do kk = 1,6
                derv3(ix,kk) = derv3(ix,kk) - tmps(kk)*xrkk*argck
                derv3(iy,kk) = derv3(iy,kk) - tmps(kk)*yrkk*argck
                derv3(iz,kk) = derv3(iz,kk) - tmps(kk)*zrkk*argck
              enddo
            endif
          endif
        enddo
      endif
!*******************************
!  End of loop over k vectors  *
!*******************************
    enddo
    if (lc6loc.and.lc6one) then
!
!  Self term
!
      c6tot = 0.0_dp
      do i = 1,numat
        c6tot = c6tot + c6f(i)*occuf(i)
      enddo
      ec6 = ec6 - 0.5_dp*c6self2*c6tot*c6tot*angstoev
    endif
  endif
!****************************************************
!  Complete strain derivatives in reciprocal space  *
!****************************************************
  if (lsg1) then
    if (lc6loc) then
      esum = erecip + ec6
    else
      esum = erecip
    endif
    if (lgrad2) then
      sderv2(1,1) = sderv2(1,1) + 2.0_dp*esum - 5.0_dp*strderv(1)
      sderv2(2,2) = sderv2(2,2) + 2.0_dp*esum - 5.0_dp*strderv(2)
      sderv2(3,3) = sderv2(3,3) + 2.0_dp*esum - 5.0_dp*strderv(3)
!
      sderv2(4,4) = sderv2(4,4) + 0.5_dp*esum - 0.75_dp*(strderv(2)+strderv(3))
      sderv2(5,5) = sderv2(5,5) + 0.5_dp*esum - 0.75_dp*(strderv(1)+strderv(3))
      sderv2(6,6) = sderv2(6,6) + 0.5_dp*esum - 0.75_dp*(strderv(1)+strderv(2))
!
      sderv2(4,1) = sderv2(4,1) - strderv(4)
      sderv2(5,2) = sderv2(5,2) - strderv(5)
      sderv2(6,3) = sderv2(6,3) - strderv(6)
!
      sderv2(5,4) = sderv2(5,4) - 0.75_dp*strderv(6)
      sderv2(6,4) = sderv2(6,4) - 0.75_dp*strderv(5)
      sderv2(6,5) = sderv2(6,5) - 0.75_dp*strderv(4)
!
      sderv2(2,1) = sderv2(2,1) + esum - strderv(1) - strderv(2)
      sderv2(3,1) = sderv2(3,1) + esum - strderv(1) - strderv(3)
      sderv2(3,2) = sderv2(3,2) + esum - strderv(2) - strderv(3)
!
      sderv2(5,1) = sderv2(5,1) - 2.5_dp*strderv(5)
      sderv2(6,1) = sderv2(6,1) - 2.5_dp*strderv(6)
      sderv2(4,2) = sderv2(4,2) - 2.5_dp*strderv(4)
      sderv2(6,2) = sderv2(6,2) - 2.5_dp*strderv(6)
      sderv2(4,3) = sderv2(4,3) - 2.5_dp*strderv(4)
      sderv2(5,3) = sderv2(5,3) - 2.5_dp*strderv(5)
    endif
    strderv(1) = - esum + strderv(1)
    strderv(2) = - esum + strderv(2)
    strderv(3) = - esum + strderv(3)
    if (lgrad2) then
!  
!  Correction from older versions / THBREL/CASCADE etc
!
      sderv2(1,1) = sderv2(1,1) + strderv(1)
      sderv2(2,2) = sderv2(2,2) + strderv(2)
      sderv2(3,3) = sderv2(3,3) + strderv(3)
    endif
  endif
!
!  Collect together reciprocal space terms
!
  if (lgrad2.and.lstr) then
    ix = -2
    iy = -1
    iz =  0
    do i = 1,nasym
      lopi = lopf(i)
      if (.not.lfreeze.or.lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        derv3(ix,1) = derv3(ix,1) - xdrv(i)
        derv3(iy,1) = derv3(iy,1) - ydrv(i)
        derv3(iz,1) = derv3(iz,1) - zdrv(i)
        derv3(ix,2) = derv3(ix,2) - xdrv(i)
        derv3(iy,2) = derv3(iy,2) - ydrv(i)
        derv3(iz,2) = derv3(iz,2) - zdrv(i)
        derv3(ix,3) = derv3(ix,3) - xdrv(i)
        derv3(iy,3) = derv3(iy,3) - ydrv(i)
        derv3(iz,3) = derv3(iz,3) - zdrv(i)
!
!  Subtract off derivative to counter their being added in strfin
!  as reciprocal space term should be excluded in strfin.
!
        derv3(ix,5) = derv3(ix,5) - zdrv(i)
        derv3(ix,6) = derv3(ix,6) - ydrv(i)
        derv3(iy,4) = derv3(iy,4) - zdrv(i)
        derv3(iy,6) = derv3(iy,6) - xdrv(i)
        derv3(iz,4) = derv3(iz,4) - ydrv(i)
        derv3(iz,5) = derv3(iz,5) - xdrv(i)
        derv3(ix,1) = derv3(ix,1) - 2.0_dp*xdrv(i)
        derv3(iy,2) = derv3(iy,2) - 2.0_dp*ydrv(i)
        derv3(iz,3) = derv3(iz,3) - 2.0_dp*zdrv(i)
      endif
    enddo
  endif
  if (lpolar) then
!*******************************
!  Electric field calculation  *
!*******************************
!
!  Start loop over k vectors
!
    do iv = 1,nkvec
      csink = 0.0_dp
      sinek = 0.0_dp
      xrkk = xrk(iv)
      yrkk = yrk(iv)
      zrkk = zrk(iv)
      trmk = ktrm(iv)*angstoev
      do i = 1,numat
        qli = qf(i)*occuf(i)
        xci = xclat(i)
        yci = yclat(i)
        zci = zclat(i)
        argc(i) = xrkk*xci + yrkk*yci + zrkk*zci
        cosi = cos(argc(i))
        sini = sin(argc(i))
        csin(i) = cosi
        sine(i) = sini
        csink = csink + cosi*qli
        sinek = sinek + sini*qli
      enddo
      do i = 1,nasym
!
!  Electric field
!
        nreli = nrel2(i)
        phsqk = (csin(nreli)*sinek - sine(nreli)*csink)
        phsqksum = trmk*phsqk
        vx(i) = vx(i) + phsqksum*xrkk
        vy(i) = vy(i) + phsqksum*yrkk
        vz(i) = vz(i) + phsqksum*zrkk
      enddo
!
!  End of loop over k vectors
!
    enddo
  endif
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('recipsd2','tmp')
  deallocate(ktrm63,stat=status)
  if (status/=0) call deallocate_error('recipsd2','ktrm63')
  deallocate(ktrm62,stat=status)
  if (status/=0) call deallocate_error('recipsd2','ktrm62')
  deallocate(ktrm6,stat=status)
  if (status/=0) call deallocate_error('recipsd2','ktrm6')
  deallocate(ktrm3,stat=status)
  if (status/=0) call deallocate_error('recipsd2','ktrm3')
  deallocate(phsq,stat=status)
  if (status/=0) call deallocate_error('recipsd2','phsq')
!
!  Timing
!
  time1 = cputime()
  tres = tres + time1 - time0
!
  return
  end
