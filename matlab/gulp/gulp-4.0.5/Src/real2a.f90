  subroutine real2a(e2a,e12ap,e12ad,lgrad1,lgrad2)
!
!  Calculate harmonic relaxation energy of region 2a
!
!  mode2a => controls treatment of region 2a
!
!  11/07 Unused variables cleaned up
!  12/07 Unused variables removed further
!   6/09 Module name changed from three to m_three
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use control
  use current
  use defects
  use derivatives
  use element, only : maxele
  use four
  use iochannels
  use m_three
  use parallel
  use properties
  use region2a
  use species
  use times
  implicit none
!
!  Passed variables
!
  logical,     intent(in)     :: lgrad1
  logical,     intent(in)     :: lgrad2
  real(dp)                    :: e2a
  real(dp)                    :: e12ap
  real(dp)                    :: e12ad
!
!  Local variables
!
  integer(i4)                 :: i
  integer(i4)                 :: ifail
  integer(i4)                 :: ii
  integer(i4)                 :: ik
  integer(i4)                 :: ind
  integer(i4)                 :: indm
  integer(i4)                 :: inert(6)
  integer(i4)                 :: itmp(6)
  integer(i4)                 :: j
  integer(i4)                 :: jk
  integer(i4)                 :: job
  integer(i4)                 :: kk
  integer(i4)                 :: nati
  integer(i4)                 :: neqi
  integer(i4)                 :: ni
  integer(i4)                 :: nip
  integer(i4)                 :: nloop
  integer(i4)                 :: nmi
  integer(i4)                 :: nti
  integer(i4)                 :: ntypi
  logical                     :: lg1
  logical                     :: locg1
  logical                     :: locg2
  logical                     :: lr2asym
  logical,               save :: lr2pf
  real(dp)                    :: cputime
  real(dp)                    :: d1x
  real(dp)                    :: d1y
  real(dp)                    :: d1z
  real(dp)                    :: d1xe
  real(dp)                    :: d1ye
  real(dp)                    :: d1ze
  real(dp)                    :: d2(3,3)
  real(dp)                    :: d2inv(3,3)
  real(dp)                    :: det(3)
  real(dp)                    :: dieinv(3,3)
  real(dp)                    :: dx
  real(dp)                    :: dy
  real(dp)                    :: dz
  real(dp)                    :: e1
  real(dp)                    :: e12ad2
  real(dp)                    :: e12ap2
  real(dp),              save :: e1store
  real(dp)                    :: oci
  real(dp)                    :: qi
  real(dp)                    :: qia
  real(dp)                    :: qli
  real(dp)                    :: qtot
  real(dp)                    :: r2d
  real(dp)                    :: rai
  real(dp)                    :: rqi
  real(dp)                    :: rqtot
  real(dp)                    :: rtmp(6)
  real(dp)                    :: time1
  real(dp)                    :: time2
  real(dp)                    :: xcl
  real(dp)                    :: ycl
  real(dp)                    :: zcl
!
  time1 = cputime()
  e2a = 0.0_dp
  e12ad = 0.0_dp
  e12ap = 0.0_dp
  r2dmax = 0.0_dp
  if (lr2f) then
!*****************************************************************
!  Do once off inversion of D2 matrices and dielectric constant  *
!*****************************************************************
!
!  Dielectric constant inversion
!
    if (abs(qdef).gt.0.0_dp) then
      if (lshello) then
        do j = 1,3
          dieinv(j,1) = diconh(j,1)
          dieinv(j,2) = diconh(j,2)
          dieinv(j,3) = diconh(j,3)
        enddo
      else
        do j = 1,3
          dieinv(j,1) = dicons(j,1)
          dieinv(j,2) = dicons(j,2)
          dieinv(j,3) = dicons(j,3)
        enddo
      endif
      call dsifa(dieinv,3_i4,3_i4,itmp,ifail)
      if (ifail.ne.0) then
        call outerror('inversion of dielectric constants failed',0_i4)
        call stopnow('real2a')
      endif
      job = 1_i4
      call dsidi(dieinv,3_i4,3_i4,itmp,det,inert,rtmp,job)
    else
      dieinv(1,1) = 1.0_dp
      dieinv(2,2) = 1.0_dp
      dieinv(3,3) = 1.0_dp
      dieinv(1,2) = 0.0_dp
      dieinv(1,3) = 0.0_dp
      dieinv(2,3) = 0.0_dp
    endif
    dieinv(2,1) = dieinv(1,2)
    dieinv(3,1) = dieinv(1,3)
    dieinv(3,2) = dieinv(2,3)
!
!  Second derivative matrix inversion
!
    ind = - 3
    do i = 1,numat
      ind = ind + 3
      do ik = 1,3
        do jk = 1,3
          d2inv(jk,ik) = derv3(ind + jk,ik)
        enddo
      enddo
!
!  Multiply inverse dielectric and d2 matrices together
!
      do ik = 1,3
        do jk = 1,3
          d2(jk,ik) = 0.0_dp
          do kk = 1,3
            d2(jk,ik) = d2(jk,ik) + d2inv(jk,kk)*dieinv(kk,ik)
          enddo
        enddo
      enddo
!
!  Store matrices in derv3 :
!  1-3  = > (WK)-1
!
      do ik = 1,3
        do jk = 1,3
          derv3(ind + jk,ik) = d2(jk,ik)
        enddo
      enddo
    enddo
!********************************************
!  Correct (WK)-1 matrix - Kilner's approx  *
!********************************************
    do i = 1,3
      do j = 1,3
        d2(j,i) = 0.0_dp
      enddo
    enddo
    qtot = 0.0_dp
    ind = - 3
    do i = 1,numat
      ind = ind + 3
      qi = qf(i)
!
!  Set species charge sums
!
      if (nat(i).gt.maxele) then
        qia = 0.0_dp
      else
        qia = abs(qi)
        ni = nat(i) + maxele
        nti = nftype(i)
        do j = 1,nspec
          if (natspec(j).eq.ni.and.(ntypspec(j).eq.nti.or.ntypspec(j).eq.0)) then
            qia = abs(qi + qlspec(j))
          endif
        enddo
      endif
!
!  Sum (WK)-1 matrices and charges
!
      qia = qia*occuf(i)
      qtot = qtot + qia
      do ik = 1,3
        d2(1,ik) = d2(1,ik) + qia*derv3(ind + 1,ik)
        d2(2,ik) = d2(2,ik) + qia*derv3(ind + 2,ik)
        d2(3,ik) = d2(3,ik) + qia*derv3(ind + 3,ik)
      enddo
    enddo
    if (abs(qtot).gt.0.0_dp) then
      rqtot = 1.0_dp/qtot
    else
      rqtot = 0.0_dp
    endif
    do i = 1,3
      do j = 1,3
        d2(j,i) = rqtot*d2(j,i)
      enddo
    enddo
    ind = -3
    do i = 1,numat
      ind = ind + 3
      qi = qf(i)*occuf(i)
      if (abs(qi).gt.0.0_dp) then
        rqi = 1.0_dp/qi
      else
        rqi = 0.0_dp
      endif
!
!  Apply correction
!
      do ik = 1,3
        derv3(ind + 1,ik) = rqi*(derv3(ind + 1,ik) - d2(1,ik))
        derv3(ind + 2,ik) = rqi*(derv3(ind + 2,ik) - d2(2,ik))
        derv3(ind + 3,ik) = rqi*(derv3(ind + 3,ik) - d2(3,ik))
      enddo
    enddo
!
!  Debug printing
!
    if (index(keyword,'d2mats').ne.0.and.ioproc) then
      do i = 1,numat
        write(ioout,'(/,''  D2 matrix for atom '',i3,'' :'',/)')i
        ind = 3*(i-1)
        qi = 14.399_dp*qf(i)*occuf(i)
        write(ioout,'(3f12.6)')qi*derv3(ind + 1,1),qi*derv3(ind + 1,2),qi*derv3(ind + 1,3)
        write(ioout,'(3f12.6)')qi*derv3(ind + 2,1),qi*derv3(ind + 2,2),qi*derv3(ind + 2,3)
        write(ioout,'(3f12.6)')qi*derv3(ind + 3,1),qi*derv3(ind + 3,2),qi*derv3(ind + 3,3)
      enddo
    endif
    lr2f = .false.
    lr2pf = (mode2a.eq.3.or.mode2a.eq.4)
    e1store = 0.0_dp
  endif
!
!  Return if no region 2a once derivative matrix is inverted
!
  if (reg2(ncf).le.reg1(ncf).or.nreg2.eq.0) return
!*******************************************
!  Region 2a - region 1 relaxation energy  *
!*******************************************
  lr2asym = ldsym
  if (lr2asym) then
    nloop = ndasym2a
  else
    nloop = nreg2
  endif
  do i = 1,nloop
    if (lr2asym) then
      ii = ndsptr2a(i)
      neqi = ndeqv2a(i)
    else
      ii = i
      neqi = 1
    endif
    nati = nr2a(ii)
    ntypi = ntr2a(ii)
    xcl = xr2a(ii)
    ycl = yr2a(ii)
    zcl = zr2a(ii)
    qli = qr2a(ii)
    oci = or2a(ii)
    if (ldbr2a(ii)) then
      rai = rr2a(ii)
    else
      rai = 0.0_dp
    endif
    nmi = nmr2a(ii)
    indm = nmir2a(ii)
    nip = nps(ii)
!
!  If optical relaxation only calculate r2 relaxation energy for shells
!
    if (nati.gt.maxele.or..not.lshello) then
!
!  Initialise gradient and second derivative matrix
!
      d1x = 0.0_dp
      d1y = 0.0_dp
      d1z = 0.0_dp
      d1xe = 0.0_dp
      d1ye = 0.0_dp
      d1ze = 0.0_dp
      ind = 3*(nip-1)
!***********************************************************
!  Evaluate 12a energy correction and displacement forces  *
!***********************************************************
      if (mode2a.eq.3.or.mode2a.eq.4) then
        call real2aef(xcl,ycl,zcl,qli,oci,nmi,indm,nip,d1xe,d1ye,d1ze)
        locg1 = .false.
        locg2 = .false.
        lg1 = .false.
      else
        locg1 = lgrad1
        locg2 = lgrad2
        lg1 = .true.
      endif
      e1 = 0.0_dp
      if (mode2a.eq.5) then
!
!  Subtract region 2a - perfect region 1 interaction
!
        call real2adef(e1,nati,ntypi,xcl,ycl,zcl,qli,oci,rai, &
          nmi,indm,d1x,d1y,d1z,d1xe,d1ye,d1ze,dx,dy,dz,2_i4,0_i4,neqi,nip)
!
!  Add region 2a - defective region 1 interaction
!
        call real2adef(e12ap,nati,ntypi,xcl,ycl,zcl,qli,oci,rai, &
          nmi,indm,d1x,d1y,d1z,d1xe,d1ye,d1ze,dx,dy,dz,1_i4,2_i4,neqi,nip)
      else
        if (mode2a.lt.3.or.lr2pf) then
!
!  Subtract region 2a - perfect region 1 interaction
!
          call real2a1(e1,nati,ntypi,xcl,ycl,zcl,qli,oci,rai,nmi,indm,d1x,d1y,d1z, &
            d1xe,d1ye,d1ze,dx,dy,dz,locg1,locg2,2_i4,0_i4,neqi,nip,lg1)
          e1store = e1 + e1store
          if (mode2a.ge.3) e1 = 0.0_dp
        endif
!
!  Add region 2a - defective region 1 interaction
!
        if (.not.lr2asym.or.(mode2a.eq.2.or.mode2a.eq.4)) then
          call real2a1(e12ap,nati,ntypi,xcl,ycl,zcl,qli,oci,rai,nmi,indm,d1x,d1y,d1z, &
            d1xe,d1ye,d1ze,dx,dy,dz,locg1,locg2,1_i4,2_i4,neqi,nip,lg1)
        else
          e12ap2 = 0.0_dp
          call real2a1(e12ap2,nati,ntypi,xcl,ycl,zcl,qli,oci,rai,nmi,indm,d1x,d1y,d1z, &
            d1xe,d1ye,d1ze,dx,dy,dz,.false.,.false.,1_i4,2_i4,neqi,nip,lg1)
        endif
      endif
      e12ap = e12ap - e1
!
!  Substitute electrostatic only force 
!
      d1x = d1xe
      d1y = d1ye
      d1z = d1ze
      do ik = 1,3
        d2inv(1,ik) = derv3(ind + 1,ik)
        d2inv(2,ik) = derv3(ind + 2,ik)
        d2inv(3,ik) = derv3(ind + 3,ik)
      enddo
!**********************************************
!  Calculate displacement vector = -g*(WK)-1  *
!**********************************************
      dx = - (d1x*d2inv(1,1) + d1y*d2inv(1,2) + d1z*d2inv(1,3))
      dy = - (d1x*d2inv(2,1) + d1y*d2inv(2,2) + d1z*d2inv(2,3))
      dz = - (d1x*d2inv(3,1) + d1y*d2inv(3,2) + d1z*d2inv(3,3))
!
!  Store displacements
!
      xdis(ii) = dx
      ydis(ii) = dy
      zdis(ii) = dz
!
!  Locate largest displacement in region 2
!
      r2d = dx*dx + dy*dy + dz*dz
      if (r2d.gt.r2dmax) r2dmax = r2d
!*******************************************************
!  Calculate region 2a relaxed - region 1 interaction  *
!*******************************************************
      d1x = 0.0_dp
      d1y = 0.0_dp
      d1z = 0.0_dp
      xcl = xcl + dx
      ycl = ycl + dy
      zcl = zcl + dz
      e1 = 0.0_dp
      if (mode2a.eq.5) then
!
!  Region 1 perfect
!
        call real2adef(e1,nati,ntypi,xcl,ycl,zcl,qli,oci,rai, &
          nmi,indm,d1x,d1y,d1z,d1xe,d1ye,d1ze,dx,dy,dz,2_i4,0_i4,neqi,nip)
!
!  Region 1 defective
!
        call real2adef(e12ad,nati,ntypi,xcl,ycl,zcl,qli,oci,rai, &
          nmi,indm,d1x,d1y,d1z,d1xe,d1ye,d1ze,dx,dy,dz,1_i4,1_i4,neqi,nip)
      else
!
!  Region 1 perfect
!
        call real2a1(e1,nati,ntypi,xcl,ycl,zcl,qli,oci,rai,nmi,indm,d1x,d1y,d1z, &
          d1xe,d1ye,d1ze,dx,dy,dz,lgrad1,lgrad2,2_i4,0_i4,neqi,nip,.true.)
!
!  Region 1 defective
!
        if (.not.lr2asym.or.(mode2a.eq.2.or.mode2a.eq.4)) then
          call real2a1(e12ad,nati,ntypi,xcl,ycl,zcl,qli,oci,rai,nmi,indm,d1x,d1y,d1z, &
            d1xe,d1ye,d1ze,dx,dy,dz,lgrad1,lgrad2,1_i4,1_i4,neqi,nip,.true.)
        else
          e12ad2 = 0.0_dp
          call real2a1(e12ad2,nati,ntypi,xcl,ycl,zcl,qli,oci,rai,nmi,indm,d1x,d1y,d1z, &
            d1xe,d1ye,d1ze,dx,dy,dz,.false.,.false.,1_i4,1_i4,neqi,nip,.true.)
        endif
      endif
      e12ad = e12ad - e1
!
!  Energy  =  -1/2gx
!
      e2a = e2a - 0.5_dp*(dx*d1x + dy*d1y + dz*d1z)*neqi
    else
!
!  For cores in optical calcn set displacements to zero
!
      xdis(ii) = 0.0_dp
      ydis(ii) = 0.0_dp
      zdis(ii) = 0.0_dp
    endif
!
!  End of loop over region 2a ions
!
  enddo
  if (mode2a.eq.3.or.mode2a.eq.4) e12ap = e12ap - e1store
!
!  If symmetry is being used for region 2a and the full
!  set of displacements are needed then generate
!
  if ((nthb+nfor.gt.0.or.(mode2a.eq.1.or.mode2a.eq.3)).and.lr2asym) call defequ2a
!
!  Test method
!
  if (lr2asym.and.(mode2a.eq.1.or.mode2a.eq.3)) then
    call real2a1f(e12ap,e12ad,lgrad1,lgrad2)
  endif
  lr2pf = .false.
  r2dmax = sqrt(r2dmax)
  time2 = cputime()
  treg2a = treg2a + time2 - time1
!
  return
  end
