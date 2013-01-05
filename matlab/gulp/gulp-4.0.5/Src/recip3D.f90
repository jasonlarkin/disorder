  subroutine recip3D(erecip,ec6,lgrad1,lgrad2)
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
!  12/97 Modification of energy/derivatives made purely local - multiplication
!        by efct and angstoev built into calculation rather than being
!        performed at the end.
!   1/98 Modified to include charge contribution to second derivatives
!   2/98 Charge contribution to second derivatives added
!   4/98 ESFF Lennard-Jones form now allowed for
!   3/99 Parallel modifications introduced (ICW/JDG)
!   6/00 Calculation of electric field added for polarisation
!   7/00 Dynamic memory allocation added
!   7/00 lfirst removed for safety
!   4/02 derv3 indexing for strain correction fixed for lfreeze case
!  11/02 Parallel changes made
!   9/04 Bond order charge derivative contributions added
!  10/04 oldel option removed as no longer used
!  11/04 Sqrt pi taken from module
!  11/04 Terms for non-lveck case rearranged to save multiplies
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   5/09 Adding of contribution to sderv2 regardless of lgrad2 trapped
!   6/09 Site energies added 
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
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
  logical,  intent(in)                        :: lgrad1
  logical,  intent(in)                        :: lgrad2
  real(dp), intent(out)                       :: ec6
  real(dp), intent(out)                       :: erecip
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: idk
  integer(i4)                                 :: ii
  integer(i4)                                 :: iv
  integer(i4)                                 :: ix
  integer(i4)                                 :: iy
  integer(i4)                                 :: iz
  integer(i4)                                 :: ixc
  integer(i4)                                 :: iyc
  integer(i4)                                 :: izc
  integer(i4)                                 :: j
  integer(i4)                                 :: jj
  integer(i4)                                 :: jx
  integer(i4)                                 :: jy
  integer(i4)                                 :: jz
  integer(i4)                                 :: jxc
  integer(i4)                                 :: jyc
  integer(i4)                                 :: jzc
  integer(i4)                                 :: kk
  integer(i4)                                 :: kl
  integer(i4)                                 :: ks
  integer(i4)                                 :: kvec0
  integer(i4)                                 :: ll
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
  real(dp)                                    :: d1is(6)
  real(dp)                                    :: d1ix
  real(dp)                                    :: d1iy
  real(dp)                                    :: d1iz
  real(dp)                                    :: d1js(6)
  real(dp)                                    :: d1jx
  real(dp)                                    :: d1jy
  real(dp)                                    :: d1jz
  real(dp)                                    :: d1trm
  real(dp)                                    :: d2trm
  real(dp)                                    :: d21q
  real(dp)                                    :: d22q
  real(dp)                                    :: d23q
  real(dp)                                    :: d24q
  real(dp)                                    :: d25q
  real(dp)                                    :: d26q
  real(dp)                                    :: d2self
  real(dp)                                    :: d3kk
  real(dp)                                    :: d3trm
  real(dp)                                    :: derfc
  real(dp)                                    :: eltrm
  real(dp)                                    :: esum
  real(dp)                                    :: factor
  real(dp)                                    :: fct
  real(dp), dimension(:),   allocatable       :: ktrm3
  real(dp), dimension(:),   allocatable       :: ktrm4
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
  real(dp)                                    :: rrk2
  real(dp)                                    :: sina
  real(dp)                                    :: sini
  real(dp)                                    :: sinek
  real(dp)                                    :: sinek6
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
  real(dp)                                    :: tmps(6)
  real(dp)                                    :: trmk
  real(dp)                                    :: trmks
  real(dp)                                    :: trmk2
  real(dp)                                    :: trmk3
  real(dp)                                    :: trmk6
  real(dp)                                    :: trmk62
  real(dp)                                    :: trmk63
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
  lveck = (nkvec.ge.numat)
  lsg1 = (lstr.and.lgrad1)
  lc6loc = (lc6.and.ndim.eq.3)
!
!  Modification added to increase speed - outer loop over k vectors is faster for derivatives as
!  recalculation of cos and sin is avoided.
!
  if (lgrad1.or.lgrad2) lveck = .false.
!
!  If Ewald sum for dispersion then don't use lveck as this requires more vector storage
!
  if (lc6loc) lveck = .false.
!
!  Distribute kvec loops
!
  if (lgrad2) then
    kvec0 = 1
    nprock = 1
    nlocalkvec = nkvec
  else
    kvec0 = procid + 1
    nprock = nprocs
    nlocalkvec = (nkvec/nprocs)
    nremainder = nkvec - nlocalkvec*nprocs
    if (procid.lt.nremainder) nlocalkvec = nlocalkvec + 1
  endif
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
  allocate(ktrm63(nlocalkvec),stat=status)
  if (status/=0) call outofmemory('recip3D','ktrm63')
  allocate(tmp(nlocalkvec,6),stat=status)
  if (status/=0) call outofmemory('recip3D','tmp')
  allocate(sum(max(numat,nstrains)),stat=status)
  if (status/=0) call outofmemory('recip3D','sum')
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
        if (lsg1) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
          ktrm62(iv) = 3.0*ktrm6(iv)*rrk2
          ktrm62(iv) = ktrm62(iv) - c6t4*xpon*(12.0*eta*seta*rrk2*rrk2)/rk
          if (lgrad2) then
            ktrm3(iv) = (ktrm(iv)*reta - 4.0_dp*ktrms(iv)*rrk2)
            ktrm63(iv) = 3.0_dp*c6t4*c6t2*rrk2*rrk2
          endif
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
        if (lsg1) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
          ktrm62(iv) = 3.0_dp*ktrm6(iv)*rrk2
          ktrm62(iv) = ktrm62(iv) - c6t4*xpon*(12.0*eta*seta*rrk2*rrk2)/rk
          if (lgrad2) then
            ktrm3(iv) = (ktrm(iv)*reta - 4.0_dp*ktrms(iv)*rrk2)
            ktrm63(iv) = 3.0_dp*c6t4*c6t2*rrk2*rrk2
          endif
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
        if (lsg1) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta+rk2)*eta4*rrk2
          if (lgrad2) then
            ktrm3(iv) = (ktrm(iv)*reta - 4.0_dp*ktrms(iv)*rrk2)
          endif
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
        if (lsg1) then
          ktrms(iv) = - 2.0_dp*ktrm(iv)*(4.0_dp*eta + rk2)*eta4*rrk2
          if (lgrad2) then
            ktrm3(iv) = (ktrm(iv)*reta - 4.0_dp*ktrms(iv)*rrk2)
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
    do iv = 1,nlocalkvec
      csin(iv) = 0.0_dp
      sine(iv) = 0.0_dp
    enddo
    do i = 1,numat
      qli = qf(i)*occuf(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      do iv = 1,nlocalkvec
        argc(iv) = xrk(iv)*xci + yrk(iv)*yci + zrk(iv)*zci
        csin(iv) = csin(iv) + qli*cos(argc(iv))
        sine(iv) = sine(iv) + qli*sin(argc(iv))
      enddo
    enddo
    do iv = 1,nlocalkvec
      phsq(iv) = (csin(iv)*csin(iv) + sine(iv)*sine(iv))
    enddo
!**********************
!  Lattice energy     *
!**********************
    erecip = 0.0_dp
    do iv = 1,nlocalkvec
      erecip = erecip + ktrm(iv)*phsq(iv)
    enddo
    erecip = 0.5_dp*erecip*angstoev
  elseif ((lc6loc.and..not.lc6one).or.(lstr.and.latomicstress)) then
!************************************************
!  Algorithm for cases where dispersion cannot  *
!  be factorised into one centre terms and/or   *
!  atomic stresses are to be calculated.        *
!************************************************
    if (lsg1.or.lgrad2) then
      do iv = 1,nlocalkvec
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
    nff = 0
    do i = 1,numat
      oci = occuf(i)*angstoev
      qli = qf(i)*oci
      nati = nat(i)
      ntypi = nftype(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      nregioni = nregionno(nsft+nrelat(i))
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      if (lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        nff = nff + 1
      endif
      jx = - 2
      jy = - 1
      jz =   0
      do j = 1,i
        ocj = occuf(j)
        if (i.eq.j) then
          ocj = 0.5_dp*ocj
        endif
        qlj = qf(j)*ocj
        natj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft+nrelat(j))
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        if (lopj) then
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
          if (lsg1) then
            strdervloc(1:6) = 0.0_dp
          endif
        endif
        if (ldoc6) then
          c6tot = c6tot*oci*ocj
          if (lgrad1) then
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
                strm1 = strm1*cosa
                strdervloc(1) = strdervloc(1) - strm1*tmp(iv,1)
                strdervloc(2) = strdervloc(2) - strm1*tmp(iv,2)
                strdervloc(3) = strdervloc(3) - strm1*tmp(iv,3)
                strdervloc(4) = strdervloc(4) - strm1*tmp(iv,4)
                strdervloc(5) = strdervloc(5) - strm1*tmp(iv,5)
                strdervloc(6) = strdervloc(6) - strm1*tmp(iv,6)
                if (lgrad2) then
                  eltrm = (ktrm3(iv)*qfct - ktrm63(iv)*c6tot)*cosa
                  do kk = 1,6
                    do ll = kk,6
                      sderv2(ll,kk) = sderv2(ll,kk) + eltrm*tmp(iv,kk)*tmp(iv,ll)
                    enddo
                    d3kk = tmp(iv,kk)*d3trm
                    derv3(ix,kk) = derv3(ix,kk) - xrkk*d3kk
                    derv3(iy,kk) = derv3(iy,kk) - yrkk*d3kk
                    derv3(iz,kk) = derv3(iz,kk) - zrkk*d3kk
                    if (i.ne.j) then
                      derv3(jx,kk) = derv3(jx,kk) + xrkk*d3kk
                      derv3(jy,kk) = derv3(jy,kk) + yrkk*d3kk
                      derv3(jz,kk) = derv3(jz,kk) + zrkk*d3kk
                    endif
                  enddo
                endif
              endif
            enddo
          else
            csin6 = 0.0_dp
            do iv = 1,nlocalkvec
              arg = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
              cosa = cos(arg)
              csinq = csinq + cosa*ktrm(iv)
              csin6 = csin6 + cosa*ktrm6(iv)
            enddo
            csinq = csinq*qfct
            csin6 = csin6*c6tot
          endif
!
!  Lattice energy
!
          erecip  =  erecip + csinq
          ec6  =  ec6 - (csin6 + c6self2*c6tot)
!
          esum = csinq - csin6 - c6self2*c6tot
          eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + esum
!
          siteenergy(i) = siteenergy(i) + 0.5_dp*esum
          siteenergy(j) = siteenergy(j) + 0.5_dp*esum
        else
          if (lgrad1) then
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
              if (lgrad2) then
                d21q = d21q + cosq*tmp(iv,1)
                d22q = d22q + cosq*tmp(iv,2)
                d23q = d23q + cosq*tmp(iv,3)
                d24q = d24q + cosq*tmp(iv,4)
                d25q = d25q + cosq*tmp(iv,5)
                d26q = d26q + cosq*tmp(iv,6)
              endif
              if (lsg1) then
                strm1 = ktrms(iv)*cosa
                d3trm = ktrms(iv)*sina
                strdervloc(1) = strdervloc(1) - strm1*tmp(iv,1)
                strdervloc(2) = strdervloc(2) - strm1*tmp(iv,2)
                strdervloc(3) = strdervloc(3) - strm1*tmp(iv,3)
                strdervloc(4) = strdervloc(4) - strm1*tmp(iv,4)
                strdervloc(5) = strdervloc(5) - strm1*tmp(iv,5)
                strdervloc(6) = strdervloc(6) - strm1*tmp(iv,6)
                if (lgrad2) then
                  eltrm = ktrm3(iv)*cosa
                  do kk = 1,6
                    do ll = kk,6
                      sderv2(ll,kk) = sderv2(ll,kk) + eltrm*tmp(iv,kk)*tmp(iv,ll)
                    enddo
                    d3kk = tmp(iv,kk)*d3trm
                    derv3(ix,kk) = derv3(ix,kk) - xrkk*d3kk
                    derv3(iy,kk) = derv3(iy,kk) - yrkk*d3kk
                    derv3(iz,kk) = derv3(iz,kk) - zrkk*d3kk
                    if (i.ne.j) then
                      derv3(jx,kk) = derv3(jx,kk) + xrkk*d3kk
                      derv3(jy,kk) = derv3(jy,kk) + yrkk*d3kk
                      derv3(jz,kk) = derv3(jz,kk) + zrkk*d3kk
                    endif
                  enddo
                endif
              endif
            enddo
          else
            csinq = 0.0_dp
            do iv = 1,nlocalkvec
              arg = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
              cosa = cos(arg)
              csinq = csinq + cosa*ktrm(iv)
            enddo
            csinq = csinq*qfct
          endif
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
        if (lsg1) then
!
!  Strain terms
!
          do kl = 1,nstrains
            strderv(kl) = strderv(kl) + strdervloc(kl)
          enddo
          if (latomicstress) then
            do kl = 1,nstrains
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
        if (lgrad1.and.i.ne.j) then
          xdrv(i) = xdrv(i) + sinqx
          ydrv(i) = ydrv(i) + sinqy
          zdrv(i) = zdrv(i) + sinqz
          xdrv(j) = xdrv(j) - sinqx
          ydrv(j) = ydrv(j) - sinqy
          zdrv(j) = zdrv(j) - sinqz
          if (nregioni.ne.nregionj) then
            xregdrv(nregioni) = xregdrv(nregioni) + sinqx
            yregdrv(nregioni) = yregdrv(nregioni) + sinqy
            zregdrv(nregioni) = zregdrv(nregioni) + sinqz
            xregdrv(nregionj) = xregdrv(nregionj) - sinqx
            yregdrv(nregionj) = yregdrv(nregionj) - sinqy
            zregdrv(nregionj) = zregdrv(nregionj) - sinqz
          endif
!
          if (lgrad2.and.(.not.lfreeze.or.lopi.or.lopj)) then
            if (.not.lfreeze.or.(lopi.and.lopj)) then
              ixc = ix
              iyc = iy
              izc = iz
              jxc = jx
              jyc = jy
              jzc = jz
            elseif (lopi) then
              ixc = ix
              iyc = iy
              izc = iz
              jxc = ix
              jyc = iy
              jzc = iz
            elseif (lopj) then
              ixc = jx
              iyc = jy
              izc = jz
              jxc = jx
              jyc = jy
              jzc = jz
            endif
            derv2(jxc,ixc) = derv2(jxc,ixc) + d21q
            derv2(jyc,ixc) = derv2(jyc,ixc) + d26q
            derv2(jzc,ixc) = derv2(jzc,ixc) + d25q
            derv2(jxc,iyc) = derv2(jxc,iyc) + d26q
            derv2(jyc,iyc) = derv2(jyc,iyc) + d22q
            derv2(jzc,iyc) = derv2(jzc,iyc) + d24q
            derv2(jxc,izc) = derv2(jxc,izc) + d25q
            derv2(jyc,izc) = derv2(jyc,izc) + d24q
            derv2(jzc,izc) = derv2(jzc,izc) + d23q
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
    do iv = 1,nlocalkvec
      csink = 0.0_dp
      sinek = 0.0_dp
      xrkk = xrk(iv)
      yrkk = yrk(iv)
      zrkk = zrk(iv)
      trmk = ktrm(iv)*angstoev
      if (lsg1) then
        trmk2 = ktrms(iv)*angstoev
        if (lgrad2) trmk3 = ktrm3(iv)*angstoev
      endif
      if (lc6loc.and.lc6one) then
        trmk6 = ktrm6(iv)*angstoev
        if (lsg1) trmk62 = ktrm62(iv)*angstoev
        if (lgrad2) trmk63 = ktrm63(iv)*angstoev
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
          cosi = cos(argc(i))
          sini = sin(argc(i))
          csin(i) = cosi*qli
          sine(i) = sini*qli
          csink = csink + csin(i)
          sinek = sinek + sine(i)
        enddo
      endif
      phsqk = (csink*csink + sinek*sinek)
!**********************
!  Lattice energy     *
!**********************
!  erecip has a factor of a half but this multiplied later
      erecip = erecip + trmk*phsqk
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
          trmks = 0.5_dp*(trmk2*phsqk - trmk62*phsqk6)
          if (lgrad2) eltrm = 0.5_dp*(trmk3*phsqk - trmk63*phsqk6)
        else
          trmks = 0.5_dp*trmk2*phsqk
          if (lgrad2) eltrm = 0.5_dp*trmk3*phsqk
        endif
        do i = 1,6
          strderv(i) = strderv(i) - trmks*tmps(i)
        enddo
        if (lgrad2) then
!
!  Second derivatives
!
!  General terms
!
          do i = 1,6
            do j = i,6
              sderv2(j,i) = sderv2(j,i) + eltrm*tmps(j)*tmps(i)
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
        do i = 1,numat
          if (lsymopt) then
            lopi = lopf(nrelat(i))
          else
            lopi = lopf(i)
          endif
          oci = occuf(i)
          qli = qf(i)*oci
          nregioni = nregionno(nsft+nrelat(i))
          if (lc6loc.and.lc6one) c6i = c6f(i)*oci
!
!  Excursion into second derivatives
!
          if (lgrad2) then
            if (.not.lfreeze.or.lopi) then
              ix = ix + 3
              iy = iy + 3
              iz = iz + 3
            endif
            jx = -2
            jy = -1
            jz = 0
            do j = 1,i-1
              if (lsymopt) then
                lopj = lopf(nrelat(j))
              else
                lopj = lopf(j)
              endif
              if (.not.lfreeze.or.lopj) then
                jx = jx + 3
                jy = jy + 3
                jz = jz + 3
              endif
              if (.not.lfreeze.or.lopi.or.lopj) then
                if (.not.lfreeze.or.(lopi.and.lopj)) then
                  ixc = ix
                  iyc = iy
                  izc = iz
                  jxc = jx
                  jyc = jy
                  jzc = jz
                elseif (lopi) then
                  ixc = ix
                  iyc = iy
                  izc = iz
                  jxc = ix
                  jyc = iy
                  jzc = iz
                elseif (lopj) then
                  ixc = jx
                  iyc = jy
                  izc = jz
                  jxc = jx
                  jyc = jy
                  jzc = jz
                endif
                ocj = occuf(j)
                qlj = qf(j)*ocj
                csprod = (csin(i)*csin(j) + sine(i)*sine(j))
                if (lc6loc.and.lc6one) then
                  c6j = c6f(j)*ocj
                  argck = csprod*(trmk*qli*qlj - trmk6*c6i*c6j)
                else
                  argck = trmk*csprod
                endif
                derv2(jxc,ix) = derv2(jxc,ix) + argck*tmps(1)
                derv2(jyc,iy) = derv2(jyc,iy) + argck*tmps(2)
                derv2(jzc,iz) = derv2(jzc,iz) + argck*tmps(3)
                derv2(jyc,ix) = derv2(jyc,ix) + argck*tmps(6)
                derv2(jzc,ix) = derv2(jzc,ix) + argck*tmps(5)
                derv2(jzc,iy) = derv2(jzc,iy) + argck*tmps(4)
              endif
            enddo
          endif
!
!  Return to first derivatives
!
          if (.not.lfreeze.or.lopi) then
            if (lc6loc.and.lc6one) then
              phsqk = qli*(csin(i)*sinek - sine(i)*csink)
              phsqk6 = c6i*(csin(i)*sinek6 - sine(i)*csink6)
              phsqksum = phsqk*trmk - phsqk6*trmk6
!
              esum = (trmk*qli*(csin(i)*csink + sine(i)*sinek) - &
                      trmk6*c6i*(csin(i)*csink6 + sine(i)*sinek6))
              eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + esum
!
              siteenergy(i) = siteenergy(i) + 0.5_dp*esum
            else
              phsqk = (csin(i)*sinek - sine(i)*csink)
              phsqksum = trmk*phsqk
!
              esum = trmk*(csin(i)*csink + sine(i)*sinek)
              eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + esum
!
              siteenergy(i) = siteenergy(i) + 0.5_dp*esum
            endif
            xdrv(i) = xdrv(i) + phsqksum*xrkk
            ydrv(i) = ydrv(i) + phsqksum*yrkk
            zdrv(i) = zdrv(i) + phsqksum*zrkk
!
            xregdrv(nregioni) = xregdrv(nregioni) + phsqksum*xrkk
            yregdrv(nregioni) = yregdrv(nregioni) + phsqksum*yrkk
            zregdrv(nregioni) = zregdrv(nregioni) + phsqksum*zrkk
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
    if (lgrad2) then
      ix = - 2
      iy = - 1
      iz =   0
      nff = 0
      do i = 1,numat
        if (.not.lfreeze.or.lopf(nrelat(i))) then
          nff = nff + 1
          ix = ix + 3
          iy = iy + 3
          iz = iz + 3
          jx = - 2
          jy = - 1
          jz =   0
          do j = 1,i
            if (.not.lfreeze.or.lopf(nrelat(j))) then
              jx = jx + 3
              jy = jy + 3
              jz = jz + 3
              derv2(jx,iy) = derv2(jy,ix)
              derv2(jx,iz) = derv2(jz,ix)
              derv2(jy,iz) = derv2(jz,iy)
            endif
          enddo
        endif
      enddo
    endif
!
!  Multiply factor of half into erecip
!
    erecip = 0.5_dp*erecip
  endif
!**********************************
!  Bond order charge derivatives  *
!**********************************
  if (lgrad1.and.lDoQDeriv1) then
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
  if (lDoQDeriv2.and.lgrad2) then
!***********************************************************************
!  Calculation of charge derivative contribution for variable charges  *
!***********************************************************************
!
!  To save space :
!  d1i  is stored in ktrm3
!  d1j  is stored in ktrm4
!  d2i2 is stored in csin
!  d2ij is stored in argc 
!  d2j2 is stored in sine
!
    do iv = 1,nlocalkvec
      tmp(iv,1) = xrk(iv)*xrk(iv)
      tmp(iv,2) = yrk(iv)*yrk(iv)
      tmp(iv,3) = zrk(iv)*zrk(iv)
      tmp(iv,4) = yrk(iv)*zrk(iv)
      tmp(iv,5) = xrk(iv)*zrk(iv)
      tmp(iv,6) = xrk(iv)*yrk(iv)
      csin(iv) = 0.0_dp
      sine(iv) = 0.0_dp
    enddo
    ix = - 2
    iy = - 1
    iz =   0
    nff = 0
    do i = 1,numat
      oci = occuf(i)
      qli = qf(i)*oci
      fct = angstoev
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      if (lopi) then
        ix = ix + 3
        iy = iy + 3
        iz = iz + 3
        nff = nff + 1
      endif
      jx = - 2
      jy = - 1
      jz =   0
      do j = 1,i
        ocj = occuf(j)
        if (i.eq.j) then
          fct = 0.5_dp*fct
        endif
        qlj = qf(j)*ocj
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        if (lopj) then
          jx = jx + 3
          jy = jy + 3
          jz = jz + 3
        endif
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
          sina = sin(arg)*fct
! d2E/dqi.dqj
          argc(iv) = cosa*oci*ocj*ktrm(iv)
! d2E/dq.dr
          ktrm3(iv) = - ktrm(iv)*sina
! d2E/dqi.de
          ktrm6(iv) = - ktrms(iv)*oci*qlj*cosa
! d2E/dqj.de
          ktrm62(iv) = - ktrms(iv)*ocj*qli*cosa
! dE/dqi
          phsq(iv) = cosa*oci*qlj*ktrm(iv)
! dE/dqj
          ktrm4(iv) = cosa*qli*ocj*ktrm(iv)
        enddo
!
!  Call d2charge
!
        d2self = 0.0_dp
        d1ix = 0.0_dp
        d1iy = 0.0_dp
        d1iz = 0.0_dp
        do iv = 1,nlocalkvec
          d1ix = d1ix + ktrm3(iv)*xrk(iv)
          d1iy = d1iy + ktrm3(iv)*yrk(iv)
          d1iz = d1iz + ktrm3(iv)*zrk(iv)
        enddo
        d1jx = d1ix*qli*ocj
        d1jy = d1iy*qli*ocj
        d1jz = d1iz*qli*ocj
        d1ix = d1ix*qlj*oci
        d1iy = d1iy*qlj*oci
        d1iz = d1iz*qlj*oci
        if (lstr) then
          do kl = 1,nstrains
            ks = nstrptr(kl)
            d1is(kl) = 0.0_dp
            d1js(kl) = 0.0_dp
            do iv = 1,nlocalkvec
              d1is(kl) = d1is(kl) + ktrm6(iv)*tmp(iv,ks)
              d1js(kl) = d1js(kl) + ktrm62(iv)*tmp(iv,ks)
            enddo
          enddo
        endif
        call d2charge(i,j,nlocalkvec,ix,iy,iz,jx,jy,jz,lopi,lopj,phsq,ktrm4,d1ix,d1iy,d1iz, &
                      d1jx,d1jy,d1jz,d1is,d1js,csin,argc,sine,d2self,0.0_dp,0.0_dp,.false., &
                      .false.)
      enddo
    enddo
  endif
!****************
!  Global sums  *
!****************
  if (.not.lgrad2) then
    tsum0 = cputime()
    if (lc6loc) then
      call sumall(erecip+ec6,sum,1_i4,"recip","ec6")
      esum = sum(1)
    else
      call sumall(erecip,sum,1_i4,"recip","erecip")
      esum = sum(1)
    endif       
    if (lsg1) then
      call sumall(strderv,sum,6_i4,"recip","strderv")
      do i = 1,6
        strderv(i) = sum(i)
      enddo
    endif
    tsum = tsum + cputime() - tsum0
  elseif (lsg1) then
    if (lc6loc.and.lc6loc) then
      esum = erecip + ec6
    else
      esum = erecip
    endif
  endif
!****************************************************
!  Complete strain derivatives in reciprocal space  *
!****************************************************
  if (lsg1) then
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
    strderv(1) = strderv(1) - esum
    strderv(2) = strderv(2) - esum
    strderv(3) = strderv(3) - esum
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
  if (lgrad2.and.lsg1) then
    ix = - 2
    iy = - 1
    iz =   0
    do i = 1,numat
      lopi = lopf(nrelat(i))
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
  deallocate(ktrm63,stat=status)
  if (status/=0) call deallocate_error('recip3D','ktrm63')
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
