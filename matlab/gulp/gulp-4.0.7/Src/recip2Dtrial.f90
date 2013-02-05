  subroutine recip2Dtrial(erecip,ntrialatom,nptrtrialatom,ltrialatom)
!
!  Calculation of electrostatic potential and derivatives,
!  including strain derivatives. Reciprocal space part.
!  Two-dimensional version using the Parry sum.
!  Subset of atoms version.
!
!   1/08 Created from recip2D
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use constants
  use control
  use current
  use kspace
  use parallel
  use symmetry,    only : lra
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: ntrialatom
  integer(i4), intent(in)                        :: nptrtrialatom(ntrialatom)
  logical,     intent(in)                        :: ltrialatom(numat)
  real(dp),    intent(inout)                     :: erecip
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: idk
  integer(i4)                                    :: ii
  integer(i4)                                    :: imin
  integer(i4)                                    :: iv
  integer(i4)                                    :: j
  integer(i4)                                    :: jj
  integer(i4)                                    :: kvec0
  integer(i4)                                    :: nt
  integer(i4)                                    :: nlocalkvec
  integer(i4)                                    :: nprock
  integer(i4)                                    :: nremainder
  real(dp)                                       :: accf2
  real(dp)                                       :: arg
  real(dp)                                       :: argtest
  real(dp)                                       :: cosa
  real(dp)                                       :: cputime
  real(dp)                                       :: csinq
  real(dp)                                       :: derf
  real(dp)                                       :: derfc
  real(dp)                                       :: derfez
  real(dp)                                       :: dexp1
  real(dp)                                       :: dexp2
  real(dp)                                       :: dexpz
  real(dp)                                       :: etaz
  real(dp)                                       :: etaz2
  real(dp)                                       :: eztrm
  real(dp)                                       :: factor
  real(dp)                                       :: fct
  real(dp)                                       :: Gmax
  real(dp)                                       :: Gmin
  real(dp)                                       :: gseta
  real(dp)                                       :: kexperfc
  real(dp)                                       :: kvec
  real(dp)                                       :: kvv(3)
  real(dp)                                       :: oci
  real(dp)                                       :: ocj
  real(dp)                                       :: qfct
  real(dp)                                       :: qli
  real(dp)                                       :: qlj
  real(dp)                                       :: rk2
  real(dp)                                       :: rktemp
  real(dp)                                       :: smallestG
  real(dp)                                       :: sum
  real(dp)                                       :: time0
  real(dp)                                       :: time1
  real(dp)                                       :: tsum0
  real(dp)                                       :: twoqv
  real(dp)                                       :: xci
  real(dp)                                       :: yci
  real(dp)                                       :: zci
  real(dp)                                       :: xd
  real(dp)                                       :: yd
  real(dp)                                       :: zd
!
  time0 = cputime()
  accf2 = accf*accf
  argtest = sqrt(3.0d0+0.5d0*accf2) - sqrt(3.0d0)
  smallestG = min(abs(kv(1,1)),abs(kv(2,2)))
!
!  Distribute kvec loops
!
  kvec0 = procid + 1
  nprock = nprocs
  nlocalkvec = (nkvec/nprocs)
  nremainder = nkvec - nlocalkvec*nprocs
  if (procid.lt.nremainder) nlocalkvec = nlocalkvec + 1
!**********
!  Setup  *
!**********
  iv = 0
  if (lra) then
    kvv(1) = kv(1,1)
    kvv(2) = kv(2,2)
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
      xrk(iv) = ii*kvv(1)
      yrk(iv) = jj*kvv(2)
      rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv)
      kmod(iv) = sqrt(rk2)
      ktrm(iv) = 0.5_dp*vol4pi*factor/kmod(iv)
    enddo
  else
    do i = kvec0,nkvec,nprock
      iv = iv + 1
      idk = indk(i)
      ii = (idk/6400) - 40
      idk = idk - (ii+40)*6400
      jj = (idk/80) - 40
      factor = 2.0_dp
      if (ii.eq.0.and.nkangle.eq.1) then
        factor = 1.0_dp
      elseif (nkangle.eq.0) then
        factor = 1.0_dp
      endif
      xrk(iv) = ii*kv(1,1) + jj*kv(1,2)
      yrk(iv) = ii*kv(2,1) + jj*kv(2,2)
      rk2 = xrk(iv)*xrk(iv) + yrk(iv)*yrk(iv)
      kmod(iv) = sqrt(rk2)
      ktrm(iv) = 0.5_dp*vol4pi*factor/kmod(iv)
    enddo
  endif
!*******************************************
!  Sort K vectors by increasing magnitude  *
!*******************************************
  do i = 1,nlocalkvec
    Gmin = 2.0_dp*rradmax
    do j = i,nlocalkvec
      if (kmod(j).lt.Gmin) then
        imin = j
        Gmin = kmod(j)
      endif
    enddo
    rktemp = kmod(i)
    kmod(i) = kmod(imin)
    kmod(imin) = rktemp
    rktemp = ktrm(i)
    ktrm(i) = ktrm(imin)
    ktrm(imin) = rktemp
    rktemp = xrk(i)
    xrk(i) = xrk(imin)
    xrk(imin) = rktemp
    rktemp = yrk(i)
    yrk(i) = yrk(imin)
    yrk(imin) = rktemp
  enddo
!**************************
!  End of set-up section  *
!**************************
  if (lnorecip) goto 999
!
!  Define constants
!
  rpieta = 1.0_dp/sqrt(pi * eta)
  rhseta = 0.5_dp/seta
!
!  Build products of K vector components
!
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    oci = occuf(i)*angstoev
    qli = qf(i)*oci
    xci = xclat(i)
    yci = yclat(i)
    zci = zclat(i)
    do j = 1,numat
      ocj = occuf(j)
      if (ltrialatom(j)) then
        fct = 0.5_dp
      else 
        fct = 1.0_dp
      endif
      qlj = qf(j)*ocj*fct
!
      if (i.ne.j) then
!
!  Find relative vector between atoms
!
        xd = xclat(j) - xci
        yd = yclat(j) - yci
        zd = zclat(j) - zci
        qfct = qli*qlj
        etaz = seta*zd
        etaz2 = etaz * etaz
!
!  First term - K vector independent
!
        derfez = derf(etaz)
        dexpz  = exp(-etaz2)
        twoqv  = qfct*vol4pi
        eztrm  = twoqv*(zd*derfez + dexpz*rpieta)
        if (procid.eq.0) then
          erecip = erecip - eztrm
        endif
!
!  Find local kvector cut-off
!
        if (abs(etaz).gt.argtest) then
          Gmax = abs(accf2/zd)
        else
          Gmax = sqrt(4.0_dp*eta*(accf2-etaz2))
        endif
!
!  Second term - K vector dependent
!
        csinq = 0.0_dp
        if (Gmax.ge.smallestG) then
          iv = 1
          kvec = kmod(iv)
          do while (iv.le.nlocalkvec.and.kvec.le.Gmax)
            arg = xrk(iv)*xd + yrk(iv)*yd
            cosa = cos(arg)
            dexp1 = exp(kvec*zd)
            dexp2 = 1.0_dp/dexp1
            gseta = kvec*rhseta
            kexperfc = dexp1*derfc(gseta+etaz) + dexp2*derfc(gseta-etaz)
            csinq = csinq + cosa*ktrm(iv)*kexperfc
            iv = iv + 1
            kvec = kmod(iv)
          enddo
        endif
      else
        qfct = qli*qlj
!
!  First term - K vector independent
!
        twoqv = qfct*vol4pi
        if (procid.eq.0) then
          erecip = erecip - twoqv*rpieta
        endif
!
!  Second term - K vector dependent
!
        csinq = 0.0_dp
        do iv = 1,nlocalkvec
          kvec = kmod(iv)
          gseta = kvec*rhseta
          kexperfc = 2.0_dp*derfc(gseta)
          csinq = csinq + ktrm(iv)*kexperfc
        enddo
      endif
!
!  Lattice energy
!
      erecip = erecip + csinq*qfct
    enddo
  enddo
!****************
!  Global sums  *
!****************
  tsum0 = cputime()
  call sumall(erecip,sum,1_i4,"recip","erecip")
  tsum = tsum + cputime() - tsum0
!***************
!  Exit point  *
!***************
999 continue
!
!  Timing
!
  time1 = cputime()
  tion = tion + time1 - time0
!
  return
  end
