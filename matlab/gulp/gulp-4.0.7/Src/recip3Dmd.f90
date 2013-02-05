  subroutine recip3Dmd(erecip,ec6)
!
!  Calculation of electrostatic potential and derivatives,
!  including strain derivatives. Reciprocal space part.
!  Vector version. First derivatives only for MD
!
!  Freezing now added.
!
!   6/09 Created from recip3D specifically for MD
!   6/09 Site energy added
!   6/09 Bug in handling of C6 terms corrected
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
!   4/12 xvir, yvir and zvir removed
!   5/12 Atomic stresses added
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
  use configurations, only : nregionno
  use constants
  use control
  use current
  use derivatives
  use energies,       only : siteenergy, eregion2region
  use kspace
  use optimisation
  use parallel
  use polarise
  use potentialxyz
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  real(dp), intent(out)                       :: ec6
  real(dp), intent(out)                       :: erecip
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: idk
  integer(i4)                                 :: ii
  integer(i4)                                 :: iv
  integer(i4)                                 :: j
  integer(i4)                                 :: jj
  integer(i4)                                 :: kk
  integer(i4)                                 :: kl
  integer(i4)                                 :: kvec0
  integer(i4)                                 :: n
  integer(i4)                                 :: nati
  integer(i4)                                 :: natj
  integer(i4)                                 :: nff
  integer(i4)                                 :: nlocalkvec
  integer(i4)                                 :: nprock
  integer(i4)                                 :: nregioni
  integer(i4)                                 :: nregionj
  integer(i4)                                 :: nremainder
  integer(i4)                                 :: ntypj
  integer(i4)                                 :: ntypi
  integer(i4)                                 :: status
  logical                                     :: lc6loc
  logical                                     :: ldoc6
  logical                                     :: lopi
  logical                                     :: lopj
  real(dp)                                    :: arg
  real(dp)                                    :: arge
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
  real(dp)                                    :: csinq
  real(dp)                                    :: d1trm
  real(dp)                                    :: derfc
  real(dp)                                    :: esum
  real(dp)                                    :: factor
  real(dp)                                    :: fct
  real(dp), dimension(:),   allocatable       :: ktrm3
  real(dp), dimension(:),   allocatable       :: ktrm4
  real(dp), dimension(:),   allocatable       :: ktrm6
  real(dp), dimension(:),   allocatable       :: ktrm62
  real(dp)                                    :: kvv(3)
  real(dp)                                    :: oci
  real(dp)                                    :: ocj
  real(dp), dimension(:),   allocatable       :: phsq
  real(dp)                                    :: phsqk
  real(dp)                                    :: phsqksum
  real(dp)                                    :: qfct
  real(dp)                                    :: qli
  real(dp)                                    :: qlj
  real(dp)                                    :: rangstoev
  real(dp)                                    :: rk
  real(dp)                                    :: rk2
  real(dp)                                    :: rketa2
  real(dp)                                    :: rrk2
  real(dp)                                    :: sina
  real(dp)                                    :: sini
  real(dp)                                    :: sinek
  real(dp)                                    :: sineq
  real(dp)                                    :: sinqx
  real(dp)                                    :: sinqy
  real(dp)                                    :: sinqz
  real(dp)                                    :: strdervloc(6)
  real(dp)                                    :: strm1
  real(dp), dimension(:),   allocatable       :: sum
  real(dp)                                    :: time0
  real(dp)                                    :: time1
  real(dp), dimension(:,:), allocatable       :: tmp
  real(dp)                                    :: trmk
  real(dp)                                    :: tsum0
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
!  Initialise energies
!
  ec6 = 0.0_dp
  erecip = 0.0_dp
!
  lc6loc = (lc6.and.ndim.eq.3)
!
!  Distribute kvec loops
!
  kvec0 = procid + 1
  nprock = nprocs
  nlocalkvec = (nkvec/nprocs)
  nremainder = nkvec - nlocalkvec*nprocs
  if (procid.lt.nremainder) nlocalkvec = nlocalkvec + 1
!
!  Allocate local memory
!
  allocate(phsq(nlocalkvec),stat=status)
  if (status/=0) call outofmemory('recip3D','phsq')
  allocate(ktrm3(nlocalkvec),stat=status)
  if (status/=0) call outofmemory('recip3D','ktrm3')
  allocate(ktrm4(nlocalkvec),stat=status)
  if (status/=0) call outofmemory('recip3D','ktrm4')
  allocate(ktrm6(nlocalkvec),stat=status)
  if (status/=0) call outofmemory('recip3D','ktrm6')
  allocate(ktrm62(nlocalkvec),stat=status)
  if (status/=0) call outofmemory('recip3D','ktrm62')
  allocate(tmp(nlocalkvec,6),stat=status)
  if (status/=0) call outofmemory('recip3D','tmp')
  allocate(sum(max(numat,nstrains)),stat=status)
  if (status/=0) call outofmemory('recip3D','sum')
!
!  Setup
!
  eta4 = 0.25_dp/eta
  rangstoev = 1.0_dp/angstoev
!*******************************
!  Sum 1/r**6 + coulomb terms  *
!*******************************
  if (lc6loc) then
    c6t1 = vol4pi*rangstoev*sqrtpi/48.0_dp
!
!  Reciprocal space self term
!
    c6self2 = 4.0_dp*c6t1*eta*seta/dble(nprock)
    iv = 0
    if (lra) then
      kvv(1) = kv(1,1)
      kvv(2) = kv(2,2)
      kvv(3) = kv(3,3)
      do i = kvec0,nkvec,nprock
        iv = iv + 1
        idk = indk(i)
        ii = (idk/6400) - 40
        if (ii.eq.0) then
          factor = 1.0_dp
        else
          factor =  2.0_dp
        endif
        idk = idk - (ii+40)*6400
        jj = (idk/80) - 40
        kk = idk - (jj+40)*80 - 40
        xrk(iv) = ii*kvv(1)
        yrk(iv) = jj*kvv(2)
        zrk(iv) = kk*kvv(3)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv) + zrk(iv)*zrk(iv)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(iv) = xpon*vol4pi*factor*rrk2
        rk = sqrt(rk2)
        rketa2 = 0.5_dp*rk/seta
        c6t2 = sqrtpi*derfc(rketa2)
        rketa2 = 1.0_dp/rketa2
        c6t3 = 0.5_dp*rketa2**3- rketa2
        c6t3 = c6t3*xpon
        c6t4 = c6t1*rk2*rk*factor
        ktrm6(iv) = c6t4*(c6t2+c6t3)
        if (lstr) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
          ktrm62(iv) = 3.0*ktrm6(iv)*rrk2
          ktrm62(iv) = ktrm62(iv) - c6t4*xpon*(12.0*eta*seta*rrk2*rrk2)/rk
        endif
      enddo
    else
      do i = kvec0,nkvec,nprock
        iv = iv + 1
        idk = indk(i)
        ii = (idk/6400) - 40
        idk = idk - (ii+40)*6400
        jj = (idk/80) - 40
        kk = idk - (jj+40)*80 - 40
        factor = 2.0_dp
        if (ii.eq.0.and.nkangle.eq.1) then
          factor = 1.0_dp
        elseif (jj.eq.0.and.nkangle.eq.2) then
          factor = 1.0_dp
        elseif (kk.eq.0.and.nkangle.eq.3) then
          factor = 1.0_dp
        elseif (nkangle.eq.0) then
          factor = 1.0_dp
        endif
        xrk(iv) = ii*kv(1,1) + jj*kv(1,2) + kk*kv(1,3)
        yrk(iv) = ii*kv(2,1) + jj*kv(2,2) + kk*kv(2,3)
        zrk(iv) = ii*kv(3,1) + jj*kv(3,2) + kk*kv(3,3)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv) + zrk(iv)*zrk(iv)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(iv) = xpon*vol4pi*factor*rrk2
        rk = sqrt(rk2)
        rketa2 = 0.5_dp*rk/seta
        c6t2 = sqrtpi*derfc(rketa2)
        rketa2 = 1.0_dp/rketa2
        c6t3 = 0.5_dp*rketa2**3 - rketa2
        c6t3 = c6t3*xpon
        c6t4 = c6t1*rk2*rk*factor
        ktrm6(iv) = c6t4*(c6t2 + c6t3)
        if (lstr) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
          ktrm62(iv) = 3.0_dp*ktrm6(iv)*rrk2
          ktrm62(iv) = ktrm62(iv) - c6t4*xpon*(12.0*eta*seta*rrk2*rrk2)/rk
        endif
      enddo
    endif
  else
!*****************
!  Coulomb only  *
!*****************
    iv = 0
    if (lra) then
      kvv(1) = kv(1,1)
      kvv(2) = kv(2,2)
      kvv(3) = kv(3,3)
      do i = kvec0,nkvec,nprock
        iv = iv + 1
        idk = indk(i)
        ii = (idk/6400) - 40
        if (ii.eq.0) then
          factor = 1.0_dp
        else
          factor = 2.0_dp
        endif
        idk = idk - (ii+40)*6400
        jj = (idk/80) - 40
        kk = idk - (jj+40)*80 - 40
        xrk(iv) = ii*kvv(1)
        yrk(iv) = jj*kvv(2)
        zrk(iv) = kk*kvv(3)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv) + zrk(iv)*zrk(iv)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(iv) = xpon*vol4pi*factor*rrk2
        if (lstr) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
        endif
      enddo
    else
      do i = kvec0,nkvec,nprock
        iv = iv + 1
        idk = indk(i)
        ii = (idk/6400) - 40
        idk = idk - (ii+40)*6400
        jj = (idk/80) - 40
        kk = idk - (jj+40)*80 - 40
        factor = 2.0_dp
        if (ii.eq.0.and.nkangle.eq.1) then
          factor = 1.0_dp
        elseif (jj.eq.0.and.nkangle.eq.2) then
          factor = 1.0_dp
        elseif (kk.eq.0.and.nkangle.eq.3) then
          factor = 1.0_dp
        elseif (nkangle.eq.0) then
          factor = 1.0_dp
        endif
        xrk(iv) = ii*kv(1,1) + jj*kv(1,2) + kk*kv(1,3)
        yrk(iv) = ii*kv(2,1) + jj*kv(2,2) + kk*kv(2,3)
        zrk(iv) = ii*kv(3,1) + jj*kv(3,2) + kk*kv(3,3)
        rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv) + zrk(iv)*zrk(iv)
        arge = - rk2*eta4
        xpon = exp(arge)
        rrk2 = 1.0_dp/rk2
        ktrm(iv) = xpon*vol4pi*factor*rrk2
        if (lstr) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta + rk2)*eta4*rrk2
        endif
      enddo
    endif
  endif
!
!  End of set-up section
!
  if (lnorecip) goto 999
!************************************************
!  Algorithm for cases where dispersion cannot  *
!  be factorised into one centre terms          *
!************************************************
  if (lstr) then
    do iv = 1,nlocalkvec
      tmp(iv,1) = xrk(iv)*xrk(iv)
      tmp(iv,2) = yrk(iv)*yrk(iv)
      tmp(iv,3) = zrk(iv)*zrk(iv)
      tmp(iv,4) = yrk(iv)*zrk(iv)
      tmp(iv,5) = xrk(iv)*zrk(iv)
      tmp(iv,6) = xrk(iv)*yrk(iv)
    enddo
  endif
  nff = 0
  do i = 1,numat
    oci = occuf(i)*angstoev
    qli = qf(i)*oci
    nati = nat(i)
    ntypi = nftype(i)
    nregioni = nregionno(nsft+i)
    xci = xclat(i)
    yci = yclat(i)
    zci = zclat(i)
    lopi = (.not.lfreeze.or.lopf(nrelat(i)))
    if (lopi) then
      nff = nff + 1
    endif
    do j = 1,i
      ocj = occuf(j)
      if (i.eq.j) then
        ocj = 0.5_dp*ocj
      endif
      qlj = qf(j)*ocj
      nregionj = nregionno(nsft+j)
      lopj = (.not.lfreeze.or.lopf(nrelat(j)))
      if (lc6loc) then
        natj = nat(j)
        ntypj = nftype(j)
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
        ldoc6 = (abs(c6tot).gt.1.0d-4)
      else
        ldoc6 = .false.
      endif
!
!  Find relative vector between atoms
!
      xd = xclat(j) - xci
      yd = yclat(j) - yci
      zd = zclat(j) - zci
      qfct = qli*qlj
      csinq = 0.0_dp
      sinqx = 0.0_dp
      sinqy = 0.0_dp
      sinqz = 0.0_dp
      if (lstr) then
        strdervloc(1:6) = 0.0_dp
      endif
      if (ldoc6) then
        c6tot = c6tot*oci*ocj
        csin6 = 0.0_dp
        do iv = 1,nlocalkvec
          xrkk = xrk(iv)
          yrkk = yrk(iv)
          zrkk = zrk(iv)
          arg = xrkk*xd + yrkk*yd + zrkk*zd
          cosa = cos(arg)
          sina = sin(arg)
          cosq = cosa*ktrm(iv)*qfct
          cos6 = cosa*ktrm6(iv)*c6tot
          csinq = csinq + cosq
          csin6 = csin6 + cos6
          d1trm = (ktrm(iv)*qfct - ktrm6(iv)*c6tot)*sina
          sinqx = sinqx + d1trm*xrkk
          sinqy = sinqy + d1trm*yrkk
          sinqz = sinqz + d1trm*zrkk
          if (lstr) then
            strm1 = (ktrms(iv)*qfct - ktrm62(iv)*c6tot)
            strm1 = strm1*cosa
            strdervloc(1) = strdervloc(1) - strm1*tmp(iv,1)
            strdervloc(2) = strdervloc(2) - strm1*tmp(iv,2)
            strdervloc(3) = strdervloc(3) - strm1*tmp(iv,3)
            strdervloc(4) = strdervloc(4) - strm1*tmp(iv,4)
            strdervloc(5) = strdervloc(5) - strm1*tmp(iv,5)
            strdervloc(6) = strdervloc(6) - strm1*tmp(iv,6)
          endif
        enddo
!
!  Lattice energy
!
        erecip  =  erecip + csinq
        ec6  =  ec6 - (csin6 + c6self2*c6tot)
!
        esum = (csinq - csin6 - c6self2*c6tot)
        eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + esum
!
        siteenergy(i) = siteenergy(i) + 0.5_dp*esum
        siteenergy(j) = siteenergy(j) + 0.5_dp*esum
      else
        do iv = 1,nlocalkvec
          xrkk = xrk(iv)
          yrkk = yrk(iv)
          zrkk = zrk(iv)
          arg = xrkk*xd + yrkk*yd + zrkk*zd
          cosa = cos(arg)*qfct
          sina = sin(arg)*qfct
          cosq = cosa*ktrm(iv)
          sineq = sina*ktrm(iv)
          csinq = csinq + cosq
          sinqx = sinqx + sineq*xrkk
          sinqy = sinqy + sineq*yrkk
          sinqz = sinqz + sineq*zrkk
          if (lstr) then
            strm1 = ktrms(iv)*cosa
            strdervloc(1) = strdervloc(1) - strm1*tmp(iv,1)
            strdervloc(2) = strdervloc(2) - strm1*tmp(iv,2)
            strdervloc(3) = strdervloc(3) - strm1*tmp(iv,3)
            strdervloc(4) = strdervloc(4) - strm1*tmp(iv,4)
            strdervloc(5) = strdervloc(5) - strm1*tmp(iv,5)
            strdervloc(6) = strdervloc(6) - strm1*tmp(iv,6)
          endif
        enddo
!
!  Lattice energy
!
        erecip = erecip + csinq
!
        eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + csinq
!
        esum = csinq
        siteenergy(i) = siteenergy(i) + 0.5_dp*esum
        siteenergy(j) = siteenergy(j) + 0.5_dp*esum
      endif
!
!  Strain terms
!
      if (lstr) then
        do kl = 1,6
          strderv(kl) = strderv(kl) + strdervloc(kl)
        enddo
        if (latomicstress) then
          do kl = 1,6
            atomicstress(kl,i) = atomicstress(kl,i) + 0.5_dp*strdervloc(kl)
            atomicstress(kl,j) = atomicstress(kl,j) + 0.5_dp*strdervloc(kl)
          enddo
          do kl = 1,3
            atomicstress(kl,i) = atomicstress(kl,i) - 0.5_dp*esum
            atomicstress(kl,j) = atomicstress(kl,j) - 0.5_dp*esum
          enddo
        endif
      endif
!
!  Internal derivatives
!
      if (i.ne.j) then
        xdrv(i) = xdrv(i) + sinqx
        ydrv(i) = ydrv(i) + sinqy
        zdrv(i) = zdrv(i) + sinqz
        xdrv(j) = xdrv(j) - sinqx
        ydrv(j) = ydrv(j) - sinqy
        zdrv(j) = zdrv(j) - sinqz
!
        if (nregioni.ne.nregionj) then
          xregdrv(nregioni) = xregdrv(nregioni) + sinqx
          yregdrv(nregioni) = yregdrv(nregioni) + sinqy
          zregdrv(nregioni) = zregdrv(nregioni) + sinqz
          xregdrv(nregionj) = xregdrv(nregionj) - sinqx
          yregdrv(nregionj) = yregdrv(nregionj) - sinqy
          zregdrv(nregionj) = zregdrv(nregionj) - sinqz
        endif
      endif
    enddo
  enddo
!**********************************
!  Bond order charge derivatives  *
!**********************************
  if (lDoQDeriv1) then
    do i = 1,numat
      oci = occuf(i)
      qli = qf(i)*oci
      fct = angstoev
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      do j = 1,i
        ocj = occuf(j)
        if (i.eq.j) then
          fct = 0.5_dp*fct
        endif
        qlj = qf(j)
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xci
        yd = yclat(j) - yci
        zd = zclat(j) - zci
        do iv = 1,nlocalkvec
          xrkk = xrk(iv)
          yrkk = yrk(iv)
          zrkk = zrk(iv)
          arg = xrkk*xd + yrkk*yd + zrkk*zd
          cosa = cos(arg)*fct
          argc(iv) = cosa*oci*ocj*ktrm(iv)*qlj
          phsq(iv) = cosa*oci*ocj*ktrm(iv)*qli
        enddo
        call d1charge(i,j,lopi,lopj,nlocalkvec,argc,phsq)
      enddo
    enddo
  endif
!****************
!  Global sums  *
!****************
  tsum0 = cputime()
  if (lc6loc) then
    call sumall(erecip+ec6,sum,1_i4,"recip","ec6")
    esum = sum(1)
  else
    call sumall(erecip,sum,1_i4,"recip","erecip")
    esum = sum(1)
  endif       
  if (lstr) then
    call sumall(strderv,sum,6_i4,"recip","strderv")
    do i = 1,6
      strderv(i) = sum(i)
    enddo
  endif
  tsum = tsum + cputime() - tsum0
!****************************************************
!  Complete strain derivatives in reciprocal space  *
!****************************************************
  if (lstr) then
    strderv(1) = strderv(1) - esum
    strderv(2) = strderv(2) - esum
    strderv(3) = strderv(3) - esum
  endif
  if (lpolar) then
!*******************************
!  Electric field calculation  *
!*******************************
!
!  Start loop over k vectors
!
    do iv = 1,nlocalkvec
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
      do i = 1,numat
!
!  Electric field
!
        phsqk = (csin(i)*sinek - sine(i)*csink)
        phsqksum = trmk*phsqk
        vx(i) = vx(i) + phsqksum*xrkk
        vy(i) = vy(i) + phsqksum*yrkk
        vz(i) = vz(i) + phsqksum*zrkk
      enddo
!
!  End of loop over k vectors
!
    enddo
    if (lnoreal) then
!
!  Perform global sums on this subroutines contribution to vx,vy,vz
!  if this won't be done after real space sum
!
      tsum0 = cputime()
      call sumall(vx,sum,numat,"recip","vx")
      do i = 1,numat
        vx(i) = sum(i)
      enddo
      call sumall(vy,sum,numat,"recip","vy")
      do i = 1,numat
        vy(i) = sum(i)
      enddo
      call sumall(vz,sum,numat,"recip","vz")
      do i = 1,numat
        vz(i) = sum(i)
      enddo
      tsum = tsum + cputime() - tsum0
    endif
  endif
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('recip3D','sum')
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('recip3D','tmp')
  deallocate(ktrm62,stat=status)
  if (status/=0) call deallocate_error('recip3D','ktrm62')
  deallocate(ktrm6,stat=status)
  if (status/=0) call deallocate_error('recip3D','ktrm6')
  deallocate(ktrm4,stat=status)
  if (status/=0) call deallocate_error('recip3D','ktrm4')
  deallocate(ktrm3,stat=status)
  if (status/=0) call deallocate_error('recip3D','ktrm3')
  deallocate(phsq,stat=status)
  if (status/=0) call deallocate_error('recip3D','phsq')
!
!  Timing
!
  time1 = cputime()
  tion = tion + time1 - time0
!
  return
  end
