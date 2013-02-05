  subroutine recip3Dtrial(erecip,ec6,ntrialatom,nptrtrialatom,ltrialatom)
!
!  Calculation of electrostatic potential and derivatives,
!  including strain derivatives. Reciprocal space part.
!  Subset of atoms version.
!
!   1/08 Created from recip3D
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
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
!  Julian Gale, NRI, Curtin University, January 2009
!
  use constants
  use control
  use current
  use kspace
  use parallel
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                     :: ntrialatom
  integer(i4), intent(in)                     :: nptrtrialatom(ntrialatom)
  logical,     intent(in)                     :: ltrialatom(numat)
  real(dp),    intent(out)                    :: ec6
  real(dp),    intent(out)                    :: erecip
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
  integer(i4)                                 :: kvec0
  integer(i4)                                 :: n
  integer(i4)                                 :: nati
  integer(i4)                                 :: natj
  integer(i4)                                 :: nlocalkvec
  integer(i4)                                 :: nprock
  integer(i4)                                 :: nremainder
  integer(i4)                                 :: nt
  integer(i4)                                 :: ntypj
  integer(i4)                                 :: ntypi
  integer(i4)                                 :: status
  logical                                     :: lc6loc
  logical                                     :: ldoc6
  real(dp)                                    :: arg
  real(dp)                                    :: arge
  real(dp)                                    :: c6self2
  real(dp)                                    :: c6t1
  real(dp)                                    :: c6t2
  real(dp)                                    :: c6t3
  real(dp)                                    :: c6t4
  real(dp)                                    :: c6tot
  real(dp)                                    :: cosa
  real(dp)                                    :: cputime
  real(dp)                                    :: csin6
  real(dp)                                    :: csinq
  real(dp)                                    :: derfc
  real(dp)                                    :: factor
  real(dp), dimension(:),   allocatable       :: ktrm6
  real(dp)                                    :: kvv(3)
  real(dp)                                    :: oci
  real(dp)                                    :: ocj
  real(dp)                                    :: qfct
  real(dp)                                    :: qli
  real(dp)                                    :: qlj
  real(dp)                                    :: rangstoev
  real(dp)                                    :: rk
  real(dp)                                    :: rk2
  real(dp)                                    :: rketa2
  real(dp)                                    :: rrk2
  real(dp)                                    :: sum
  real(dp)                                    :: time0
  real(dp)                                    :: time1
  real(dp)                                    :: tsum0
  real(dp)                                    :: xci
  real(dp)                                    :: yci
  real(dp)                                    :: zci
  real(dp)                                    :: xd
  real(dp)                                    :: yd
  real(dp)                                    :: zd
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
  allocate(ktrm6(nlocalkvec),stat=status)
  if (status/=0) call outofmemory('recip3D','ktrm6')
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
  if (lc6loc) then
!************************************************
!  Algorithm for cases where dispersion cannot  *
!  be factorised into one centre terms          *
!************************************************
    do nt = 1,ntrialatom
      i = nptrtrialatom(nt)
      oci = occuf(i)*angstoev
      qli = qf(i)*oci
      nati = nat(i)
      ntypi = nftype(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      do j = 1,numat
        ocj = occuf(j)
!       
!  Set factor that depends on whether j is a trial atom or not
!       
        if (ltrialatom(j)) then
          ocj = 0.5_dp*ocj
        endif
        qlj = qf(j)*ocj
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
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xci
        yd = yclat(j) - yci
        zd = zclat(j) - zci
        qfct = qli*qlj
        csinq = 0.0_dp
        if (ldoc6) then
          c6tot = c6tot*oci*ocj
          csin6 = 0.0_dp
          do iv = 1,nlocalkvec
            arg = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
            cosa = cos(arg)
            csinq = csinq + cosa*ktrm(iv)
            csin6 = csin6 + cosa*ktrm6(iv)
          enddo
          csinq = csinq*qfct
          csin6 = csin6*c6tot
!
!  Lattice energy
!
          erecip = erecip + csinq
          ec6    = ec6 - (csin6 + c6self2*c6tot)
        else
          csinq = 0.0_dp
          do iv = 1,nlocalkvec
            arg = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
            cosa = cos(arg)
            csinq = csinq + cosa*ktrm(iv)
          enddo
          csinq = csinq*qfct
!
!  Lattice energy
!
          erecip = erecip + csinq
        endif
      enddo
    enddo
  else
    do nt = 1,ntrialatom
      i = nptrtrialatom(nt)
      oci = occuf(i)*angstoev
      qli = qf(i)*oci
      nati = nat(i)
      ntypi = nftype(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      do j = 1,numat
        ocj = occuf(j)
!       
!  Set factor that depends on whether j is a trial atom or not
!       
        if (ltrialatom(j)) then
          ocj = 0.5_dp*ocj
        endif
        qlj = qf(j)*ocj
        natj = nat(j)
        ntypj = nftype(j)
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xci
        yd = yclat(j) - yci
        zd = zclat(j) - zci
        qfct = qli*qlj
        csinq = 0.0_dp
        csinq = 0.0_dp
        do iv = 1,nlocalkvec
          arg = xrk(iv)*xd + yrk(iv)*yd + zrk(iv)*zd
          cosa = cos(arg)
          csinq = csinq + cosa*ktrm(iv)
        enddo
        csinq = csinq*qfct
!
!  Lattice energy
!
        erecip = erecip + csinq
      enddo
    enddo
  endif
!****************
!  Global sums  *
!****************
  tsum0 = cputime()
  if (lc6loc) then
    call sumall(erecip+ec6,sum,1_i4,"recip","ec6")
  else
    call sumall(erecip,sum,1_i4,"recip","erecip")
  endif       
  tsum = tsum + cputime() - tsum0
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(ktrm6,stat=status)
  if (status/=0) call deallocate_error('recip3D','ktrm6')
!
!  Timing
!
  time1 = cputime()
  tion = tion + time1 - time0
!
  return
  end
