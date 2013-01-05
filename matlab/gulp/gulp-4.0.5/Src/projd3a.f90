  subroutine projd3a(mcvmin,mcvmax,d3r,d3i,d3,d3rs,d3is,d3s,lstr, &
    i,ix,iy,iz,j,jx,jy,jz,lcorei,lcorej,rmassi,rmassj,dfedx, &
    dfedy,dfedz,dfeds,derv2,dervi,eigr,eigi,d33,d33r,d33i, &
    d33s,d33rs,d33is,lmany,nmanyk,nptrmanyk,d34,d34r,d34i,d34s, &
    d34rs,d34is,nforkl,nptrfork,nptrforl,maxd2,dsqh,iocptr,lzsisa)
!
!  Projects the 3rd derivatives on to the phonon modes for internal and strain derivatives
!
!   9/97 Created from projd3/projd3s
!   9/97 All references to d3 matrices are made lower half triangular
!        to avoid the need to symmetrise the matrices
!   5/98 Three body derivatives added
!   5/98 Strain derivatives added
!   8/98 Four body modifications added
!   8/99 zsisa corrections for 3-/4- body potentials added
!   3/01 Generalised for any dimensionality
!   1/08 Unused passed variable removed
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
  use current
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)         :: i
  integer(i4)         :: ix
  integer(i4)         :: iy
  integer(i4)         :: iz
  integer(i4)         :: iocptr(*)
  integer(i4)         :: j
  integer(i4)         :: jx
  integer(i4)         :: jy
  integer(i4)         :: jz
  integer(i4)         :: maxd2
  integer(i4)         :: mcvmax
  integer(i4)         :: mcvmin
  integer(i4)         :: nmanyk
  integer(i4)         :: nforkl
  integer(i4)         :: nptrfork(*)
  integer(i4)         :: nptrforl(*)
  integer(i4)         :: nptrmanyk(*)
  logical             :: lcorei
  logical             :: lcorej
  logical             :: lmany
  logical             :: lstr
  logical             :: lzsisa
  real(dp)            :: d3(3,3,3)
  real(dp)            :: d3d(3,3)
  real(dp)            :: d3i(3,3,3)
  real(dp)            :: d3r(3,3,3)
  real(dp)            :: d3is(3,3,6)
  real(dp)            :: d3rs(3,3,6)
  real(dp)            :: d3s(3,3,6)
  real(dp)            :: d33(54,*)
  real(dp)            :: d33i(54,*)
  real(dp)            :: d33r(54,*)
  real(dp)            :: d33is(108,*)
  real(dp)            :: d33rs(108,*)
  real(dp)            :: d33s(108,*)
  real(dp)            :: d34(27,*)
  real(dp)            :: d34i(27,*)
  real(dp)            :: d34is(54,*)
  real(dp)            :: d34r(27,*)
  real(dp)            :: d34rs(54,*)
  real(dp)            :: d34s(54,*)
  real(dp)            :: derv2(maxd2,*)
  real(dp)            :: dervi(maxd2,*)
  real(dp)            :: dfeds(6)
  real(dp)            :: dfedx
  real(dp)            :: dfedy
  real(dp)            :: dfedz
  real(dp)            :: dsqh(maxd2,*)
  real(dp)            :: eigi(maxd2,*)
  real(dp)            :: eigr(maxd2,*)
  real(dp)            :: rmassi
  real(dp)            :: rmassj
!
!  Local variables
!
  integer(i4)         :: ii
  integer(i4)         :: k
  integer(i4)         :: kk
  integer(i4)         :: kkk
  real(dp)            :: d1ij11
  real(dp)            :: d1ij21
  real(dp)            :: d1ij31
  real(dp)            :: d1ij12
  real(dp)            :: d1ij22
  real(dp)            :: d1ij32
  real(dp)            :: d1ij13
  real(dp)            :: d1ij23
  real(dp)            :: d1ij33
  real(dp)            :: d2ij11
  real(dp)            :: d2ij21
  real(dp)            :: d2ij31
  real(dp)            :: d2ij12
  real(dp)            :: d2ij22
  real(dp)            :: d2ij32
  real(dp)            :: d2ij13
  real(dp)            :: d2ij23
  real(dp)            :: d2ij33
  real(dp)            :: dii11
  real(dp)            :: dii21
  real(dp)            :: dii31
  real(dp)            :: dii22
  real(dp)            :: dii32
  real(dp)            :: dii33
  real(dp)            :: diix
  real(dp)            :: diiy
  real(dp)            :: diiz
  real(dp)            :: dijx
  real(dp)            :: dijy
  real(dp)            :: dijz
  real(dp)            :: djj11
  real(dp)            :: djj21
  real(dp)            :: djj31
  real(dp)            :: djj22
  real(dp)            :: djj32
  real(dp)            :: djj33
  real(dp)            :: drix
  real(dp)            :: driy
  real(dp)            :: driz
  real(dp)            :: drjx
  real(dp)            :: drjy
  real(dp)            :: drjz
  real(dp)            :: dt11
  real(dp)            :: dt21
  real(dp)            :: dt31
  real(dp)            :: dt12
  real(dp)            :: dt22
  real(dp)            :: dt32
  real(dp)            :: dt13
  real(dp)            :: dt23
  real(dp)            :: dt33
  real(dp)            :: dt11i
  real(dp)            :: dt21i
  real(dp)            :: dt31i
  real(dp)            :: dt12i
  real(dp)            :: dt22i
  real(dp)            :: dt32i
  real(dp)            :: dt13i
  real(dp)            :: dt23i
  real(dp)            :: dt33i
  real(dp)            :: dt11r
  real(dp)            :: dt21r
  real(dp)            :: dt31r
  real(dp)            :: dt12r
  real(dp)            :: dt22r
  real(dp)            :: dt32r
  real(dp)            :: dt13r
  real(dp)            :: dt23r
  real(dp)            :: dt33r
  real(dp)            :: dw2ds12
  real(dp)            :: dw2ds12x
  real(dp)            :: dw2ds12y
  real(dp)            :: dw2ds12z
  real(dp)            :: dw2ds34
  real(dp)            :: dw2ds34x
  real(dp)            :: dw2ds34y
  real(dp)            :: dw2ds34z
  real(dp)            :: dw2dsii
  real(dp)            :: dw2dsiix
  real(dp)            :: dw2dsiiy
  real(dp)            :: dw2dsiiz
  real(dp)            :: dw2dsij
  real(dp)            :: dw2dsijx
  real(dp)            :: dw2dsijy
  real(dp)            :: dw2dsijz
  real(dp)            :: dw2dsjj
  real(dp)            :: dw2dsjjx
  real(dp)            :: dw2dsjjy
  real(dp)            :: dw2dsjjz
  real(dp)            :: eiix
  real(dp)            :: eiiy
  real(dp)            :: eiiz
  real(dp)            :: eijx
  real(dp)            :: eijy
  real(dp)            :: eijz
  real(dp)            :: erix
  real(dp)            :: eriy
  real(dp)            :: eriz
  real(dp)            :: erjx
  real(dp)            :: erjy
  real(dp)            :: erjz
  real(dp)            :: rmii
  real(dp)            :: rmij
  real(dp)            :: rmjj
!
!*********************************************
!  Project contributions on to phonon modes  *
!*********************************************
  rmij = rmassi*rmassj
  rmii = rmassi*rmassi
  rmjj = rmassj*rmassj
  dfedx = 0.0_dp
  dfedy = 0.0_dp
  dfedz = 0.0_dp
  if (lstr) then
    do kk = 1,nstrains
      dfeds(kk) = 0.0_dp
    enddo
  endif
  if (i.ne.j) then
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
!  i and j different case  $
!$$$$$$$$$$$$$$$$$$$$$$$$$$$
    d1ij11 = 0.0_dp
    d1ij21 = 0.0_dp
    d1ij31 = 0.0_dp
    d1ij12 = 0.0_dp
    d1ij22 = 0.0_dp
    d1ij32 = 0.0_dp
    d1ij13 = 0.0_dp
    d1ij23 = 0.0_dp
    d1ij33 = 0.0_dp
    d2ij11 = 0.0_dp
    d2ij21 = 0.0_dp
    d2ij31 = 0.0_dp
    d2ij12 = 0.0_dp
    d2ij22 = 0.0_dp
    d2ij32 = 0.0_dp
    d2ij13 = 0.0_dp
    d2ij23 = 0.0_dp
    d2ij33 = 0.0_dp
    dii11 = 0.0_dp
    dii21 = 0.0_dp
    dii31 = 0.0_dp
    dii22 = 0.0_dp
    dii32 = 0.0_dp
    dii33 = 0.0_dp
    djj11 = 0.0_dp
    djj21 = 0.0_dp
    djj31 = 0.0_dp
    djj22 = 0.0_dp
    djj32 = 0.0_dp
    djj33 = 0.0_dp
    if (lcorei.and.lcorej) then
!****************
!  Core - Core  *
!****************
      do k = mcvmin,mcvmax
        erix = eigr(ix,k)
        eriy = eigr(iy,k)
        eriz = eigr(iz,k)
        erjx = eigr(jx,k)
        erjy = eigr(jy,k)
        erjz = eigr(jz,k)
        eiix = eigi(ix,k)
        eiiy = eigi(iy,k)
        eiiz = eigi(iz,k)
        eijx = eigi(jx,k)
        eijy = eigi(jy,k)
        eijz = eigi(jz,k)
!%%%%%%%%%%%%%%%%%
!  Off diagonal  %
!%%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        d1ij11 = d1ij11 + erix*erjx + eiix*eijx
        d1ij21 = d1ij21 + eriy*erjx + eiiy*eijx
        d1ij31 = d1ij31 + eriz*erjx + eiiz*eijx
        d1ij12 = d1ij12 + erix*erjy + eiix*eijy
        d1ij22 = d1ij22 + eriy*erjy + eiiy*eijy
        d1ij32 = d1ij32 + eriz*erjy + eiiz*eijy
        d1ij13 = d1ij13 + erix*erjz + eiix*eijz
        d1ij23 = d1ij23 + eriy*erjz + eiiy*eijz
        d1ij33 = d1ij33 + eriz*erjz + eiiz*eijz
!
!  Imag - imag - real  -  real*imag*imag
!
        d2ij11 = d2ij11 + eiix*erjx - erix*eijx
        d2ij21 = d2ij21 + eiiy*erjx - eriy*eijx
        d2ij31 = d2ij31 + eiiz*erjx - eriz*eijx
        d2ij12 = d2ij12 + eiix*erjy - erix*eijy
        d2ij22 = d2ij22 + eiiy*erjy - eriy*eijy
        d2ij32 = d2ij32 + eiiz*erjy - eriz*eijy
        d2ij13 = d2ij13 + eiix*erjz - erix*eijz
        d2ij23 = d2ij23 + eiiy*erjz - eriy*eijz
        d2ij33 = d2ij33 + eiiz*erjz - eriz*eijz
!%%%%%%%%%%%%%%%%
!  On diagonal  %
!%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        dii11 = dii11 + erix*erix + eiix*eiix
        dii21 = dii21 + erix*eriy + eiix*eiiy
        dii31 = dii31 + erix*eriz + eiix*eiiz
        dii22 = dii22 + eriy*eriy + eiiy*eiiy
        dii32 = dii32 + eriy*eriz + eiiy*eiiz
        dii33 = dii33 + eriz*eriz + eiiz*eiiz
        djj11 = djj11 + erjx*erjx + eijx*eijx
        djj21 = djj21 + erjx*erjy + eijx*eijy
        djj31 = djj31 + erjx*erjz + eijx*eijz
        djj22 = djj22 + erjy*erjy + eijy*eijy
        djj32 = djj32 + erjy*erjz + eijy*eijz
        djj33 = djj33 + erjz*erjz + eijz*eijz
      enddo
!
!  Convert to free energy derivatives
!
!  Factor of 2 comes from the fact that each i - j block appears
!  in two places   -  on - diagonal blocks handled separately as
!  they are real when coming from i - j
!
      dw2ds12x = d3r(1,1,1)*d1ij11 + d3r(2,1,1)*d1ij21 +  &
                 d3r(3,1,1)*d1ij31 + d3r(1,2,1)*d1ij12 +  &
                 d3r(2,2,1)*d1ij22 + d3r(3,2,1)*d1ij32 +  &
                 d3r(1,3,1)*d1ij13 + d3r(2,3,1)*d1ij23 +  &
                 d3r(3,3,1)*d1ij33
      dw2ds12y = d3r(1,1,2)*d1ij11 + d3r(2,1,2)*d1ij21 +  &
                 d3r(3,1,2)*d1ij31 + d3r(1,2,2)*d1ij12 +  &
                 d3r(2,2,2)*d1ij22 + d3r(3,2,2)*d1ij32 +  &
                 d3r(1,3,2)*d1ij13 + d3r(2,3,2)*d1ij23 +  &
                 d3r(3,3,2)*d1ij33
      dw2ds12z = d3r(1,1,3)*d1ij11 + d3r(2,1,3)*d1ij21 +  &
                 d3r(3,1,3)*d1ij31 + d3r(1,2,3)*d1ij12 +  &
                 d3r(2,2,3)*d1ij22 + d3r(3,2,3)*d1ij32 +  &
                 d3r(1,3,3)*d1ij13 + d3r(2,3,3)*d1ij23 +  &
                 d3r(3,3,3)*d1ij33
      dw2ds34x = d3i(1,1,1)*d2ij11 + d3i(2,1,1)*d2ij21 +  &
                 d3i(3,1,1)*d2ij31 + d3i(1,2,1)*d2ij12 +  &
                 d3i(2,2,1)*d2ij22 + d3i(3,2,1)*d2ij32 +  &
                 d3i(1,3,1)*d2ij13 + d3i(2,3,1)*d2ij23 +  &
                 d3i(3,3,1)*d2ij33
      dw2ds34y = d3i(1,1,2)*d2ij11 + d3i(2,1,2)*d2ij21 +  &
                 d3i(3,1,2)*d2ij31 + d3i(1,2,2)*d2ij12 +  &
                 d3i(2,2,2)*d2ij22 + d3i(3,2,2)*d2ij32 +  &
                 d3i(1,3,2)*d2ij13 + d3i(2,3,2)*d2ij23 +  &
                 d3i(3,3,2)*d2ij33
      dw2ds34z = d3i(1,1,3)*d2ij11 + d3i(2,1,3)*d2ij21 +  &
                 d3i(3,1,3)*d2ij31 + d3i(1,2,3)*d2ij12 +  &
                 d3i(2,2,3)*d2ij22 + d3i(3,2,3)*d2ij32 +  &
                 d3i(1,3,3)*d2ij13 + d3i(2,3,3)*d2ij23 +  &
                 d3i(3,3,3)*d2ij33
      dw2dsijx = 2.0_dp*(dw2ds12x + dw2ds34x)*rmij
      dw2dsijy = 2.0_dp*(dw2ds12y + dw2ds34y)*rmij
      dw2dsijz = 2.0_dp*(dw2ds12z + dw2ds34z)*rmij
      dw2dsiix = d3(1,1,1)*dii11 + (d3(2,1,1) + d3(1,2,1))*dii21 +  &
                 (d3(3,1,1) + d3(1,3,1))*dii31 + d3(2,2,1)*dii22 +  &
                 (d3(3,2,1) + d3(2,3,1))*dii32 + d3(3,3,1)*dii33
      dw2dsiiy = d3(1,1,2)*dii11 + (d3(2,1,2) + d3(1,2,2))*dii21 +  &
                 (d3(3,1,2) + d3(1,3,2))*dii31 + d3(2,2,2)*dii22 +  &
                 (d3(3,2,2) + d3(2,3,2))*dii32 + d3(3,3,2)*dii33
      dw2dsiiz = d3(1,1,3)*dii11 + (d3(2,1,3) + d3(1,2,3))*dii21 +  &
                 (d3(3,1,3) + d3(1,3,3))*dii31 + d3(2,2,3)*dii22 +  &
                 (d3(3,2,3) + d3(2,3,3))*dii32 + d3(3,3,3)*dii33
      dw2dsjjx = d3(1,1,1)*djj11 + (d3(2,1,1) + d3(1,2,1))*djj21 +  &
                 (d3(3,1,1) + d3(1,3,1))*djj31 + d3(2,2,1)*djj22 +  &
                 (d3(3,2,1) + d3(2,3,1))*djj32 + d3(3,3,1)*djj33
      dw2dsjjy = d3(1,1,2)*djj11 + (d3(2,1,2) + d3(1,2,2))*djj21 +  &
                 (d3(3,1,2) + d3(1,3,2))*djj31 + d3(2,2,2)*djj22 +  &
                 (d3(3,2,2) + d3(2,3,2))*djj32 + d3(3,3,2)*djj33
      dw2dsjjz = d3(1,1,3)*djj11 + (d3(2,1,3) + d3(1,2,3))*djj21 +  &
                 (d3(3,1,3) + d3(1,3,3))*djj31 + d3(2,2,3)*djj22 +  &
                 (d3(3,2,3) + d3(2,3,3))*djj32 + d3(3,3,3)*djj33
      dfedx = (dw2dsijx - dw2dsiix*rmii - dw2dsjjx*rmjj)
      dfedy = (dw2dsijy - dw2dsiiy*rmii - dw2dsjjy*rmjj)
      dfedz = (dw2dsijz - dw2dsiiz*rmii - dw2dsjjz*rmjj)
      if (lstr) then
        do kkk = 1,nstrains
          kk  =  nstrptr(kkk)
          dw2ds12 = d3rs(1,1,kk)*d1ij11 + d3rs(2,1,kk)*d1ij21 +  &
                    d3rs(3,1,kk)*d1ij31 + d3rs(1,2,kk)*d1ij12 +  &
                    d3rs(2,2,kk)*d1ij22 + d3rs(3,2,kk)*d1ij32 +  &
                    d3rs(1,3,kk)*d1ij13 + d3rs(2,3,kk)*d1ij23 +  &
                    d3rs(3,3,kk)*d1ij33
          dw2ds34 = d3is(1,1,kk)*d2ij11 + d3is(2,1,kk)*d2ij21 +  &
                    d3is(3,1,kk)*d2ij31 + d3is(1,2,kk)*d2ij12 +  &
                    d3is(2,2,kk)*d2ij22 + d3is(3,2,kk)*d2ij32 +  &
                    d3is(1,3,kk)*d2ij13 + d3is(2,3,kk)*d2ij23 +  &
                    d3is(3,3,kk)*d2ij33
          dw2dsij = 2.0_dp*(dw2ds12 + dw2ds34)*rmij
          dw2dsii = d3s(1,1,kk)*dii11 + (d3s(2,1,kk) + d3s(1,2,kk))*dii21 +  &
                    (d3s(3,1,kk) + d3s(1,3,kk))*dii31 + d3s(2,2,kk)*dii22 +  &
                    (d3s(3,2,kk) + d3s(2,3,kk))*dii32 + d3s(3,3,kk)*dii33
          dw2dsjj = d3s(1,1,kk)*djj11 + (d3s(2,1,kk) + d3s(1,2,kk))*djj21 +  &
                    (d3s(3,1,kk) + d3s(1,3,kk))*djj31 + d3s(2,2,kk)*djj22 +  &
                    (d3s(3,2,kk) + d3s(2,3,kk))*djj32 + d3s(3,3,kk)*djj33
          dfeds(kkk) = (dw2dsij - dw2dsii*rmii - dw2dsjj*rmjj)
        enddo
      endif
      if (lmany.and.nmanyk.gt.0) then
!*****************
!  Off diagonal  *
!*****************
!
!  Real
!
        dt11r = 2.0_dp*d1ij11*rmij
        dt21r = 2.0_dp*d1ij21*rmij
        dt31r = 2.0_dp*d1ij31*rmij
        dt12r = 2.0_dp*d1ij12*rmij
        dt22r = 2.0_dp*d1ij22*rmij
        dt32r = 2.0_dp*d1ij32*rmij
        dt13r = 2.0_dp*d1ij13*rmij
        dt23r = 2.0_dp*d1ij23*rmij
        dt33r = 2.0_dp*d1ij33*rmij
!
!  Imaginary
!
        dt11i = 2.0_dp*d2ij11*rmij
        dt21i = 2.0_dp*d2ij21*rmij
        dt31i = 2.0_dp*d2ij31*rmij
        dt12i = 2.0_dp*d2ij12*rmij
        dt22i = 2.0_dp*d2ij22*rmij
        dt32i = 2.0_dp*d2ij32*rmij
        dt13i = 2.0_dp*d2ij13*rmij
        dt23i = 2.0_dp*d2ij23*rmij
        dt33i = 2.0_dp*d2ij33*rmij
!
!  Real  -  on - diagonal
!
        dt11 =  - (dii11*rmii + djj11*rmjj)
        dt21 =  - (dii21*rmii + djj21*rmjj)
        dt31 =  - (dii31*rmii + djj31*rmjj)
        dt12 =  - (dii21*rmii + djj21*rmjj)
        dt22 =  - (dii22*rmii + djj22*rmjj)
        dt32 =  - (dii32*rmii + djj32*rmjj)
        dt13 =  - (dii31*rmii + djj31*rmjj)
        dt23 =  - (dii32*rmii + djj32*rmjj)
        dt33 =  - (dii33*rmii + djj33*rmjj)
!
        call projthbk3(i,j,nmanyk,nptrmanyk,d33,d33r,d33i, &
          dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
          dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
          dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
          dt22,dt32,dt13,dt23,dt33,dsqh,iocptr,lzsisa)
        if (nforkl.gt.0) then
          call projfork3(nforkl,nptrfork,nptrforl,d34, &
            d34r,d34i,dt11r,dt21r,dt31r,dt12r,dt22r, &
            dt32r,dt13r,dt23r,dt33r,dt11i,dt21i,dt31i, &
            dt12i,dt22i,dt32i,dt13i,dt23i,dt33i,dt11, &
            dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33, &
            dsqh,iocptr,lzsisa)
        endif
        if (lstr) then
          call projthbk3s(nmanyk,d33s,d33rs,d33is, &
            dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
            dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
            dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
            dt22,dt32,dt13,dt23,dt33,dfeds)
          if (nforkl.gt.0) then
            call projfork3s(nforkl,d34s,d34rs,d34is, &
              dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
              dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
              dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
              dt22,dt32,dt13,dt23,dt33,dfeds)
          endif
        endif
      endif
    elseif (.not.lcorei.and..not.lcorej) then
!******************
!  Shell - Shell  *
!******************
      do k = mcvmin,mcvmax
        drix = derv2(k,ix)
        driy = derv2(k,iy)
        driz = derv2(k,iz)
        drjx = derv2(k,jx)
        drjy = derv2(k,jy)
        drjz = derv2(k,jz)
        diix = dervi(k,ix)
        diiy = dervi(k,iy)
        diiz = dervi(k,iz)
        dijx = dervi(k,jx)
        dijy = dervi(k,jy)
        dijz = dervi(k,jz)
!%%%%%%%%%%%%%%%%%
!  Off - diagonal  %
!%%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        d1ij11 = d1ij11 + drix*drjx + diix*dijx
        d1ij21 = d1ij21 + driy*drjx + diiy*dijx
        d1ij31 = d1ij31 + driz*drjx + diiz*dijx
        d1ij12 = d1ij12 + drix*drjy + diix*dijy
        d1ij22 = d1ij22 + driy*drjy + diiy*dijy
        d1ij32 = d1ij32 + driz*drjy + diiz*dijy
        d1ij13 = d1ij13 + drix*drjz + diix*dijz
        d1ij23 = d1ij23 + driy*drjz + diiy*dijz
        d1ij33 = d1ij33 + driz*drjz + diiz*dijz
!
!  Imag - imag - real  -  real*imag*imag
!
        d2ij11 = d2ij11 + diix*drjx - drix*dijx
        d2ij21 = d2ij21 + diiy*drjx - driy*dijx
        d2ij31 = d2ij31 + diiz*drjx - driz*dijx
        d2ij12 = d2ij12 + diix*drjy - drix*dijy
        d2ij22 = d2ij22 + diiy*drjy - driy*dijy
        d2ij32 = d2ij32 + diiz*drjy - driz*dijy
        d2ij13 = d2ij13 + diix*drjz - drix*dijz
        d2ij23 = d2ij23 + diiy*drjz - driy*dijz
        d2ij33 = d2ij33 + diiz*drjz - driz*dijz
!%%%%%%%%%%%%%%%%
!  On diagonal  %
!%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        dii11 = dii11 + drix*drix + diix*diix
        dii21 = dii21 + drix*driy + diix*diiy
        dii31 = dii31 + drix*driz + diix*diiz
        dii22 = dii22 + driy*driy + diiy*diiy
        dii32 = dii32 + driy*driz + diiy*diiz
        dii33 = dii33 + driz*driz + diiz*diiz
        djj11 = djj11 + drjx*drjx + dijx*dijx
        djj21 = djj21 + drjx*drjy + dijx*dijy
        djj31 = djj31 + drjx*drjz + dijx*dijz
        djj22 = djj22 + drjy*drjy + dijy*dijy
        djj32 = djj32 + drjy*drjz + dijy*dijz
        djj33 = djj33 + drjz*drjz + dijz*dijz
      enddo
!
!  Convert to free energy derivatives
!
      dw2ds12x = d3r(1,1,1)*d1ij11 + d3r(2,1,1)*d1ij21 +  &
                 d3r(3,1,1)*d1ij31 + d3r(1,2,1)*d1ij12 +  &
                 d3r(2,2,1)*d1ij22 + d3r(3,2,1)*d1ij32 +  &
                 d3r(1,3,1)*d1ij13 + d3r(2,3,1)*d1ij23 +  &
                 d3r(3,3,1)*d1ij33
      dw2ds12y = d3r(1,1,2)*d1ij11 + d3r(2,1,2)*d1ij21 +  &
                 d3r(3,1,2)*d1ij31 + d3r(1,2,2)*d1ij12 +  &
                 d3r(2,2,2)*d1ij22 + d3r(3,2,2)*d1ij32 +  &
                 d3r(1,3,2)*d1ij13 + d3r(2,3,2)*d1ij23 +  &
                 d3r(3,3,2)*d1ij33
      dw2ds12z = d3r(1,1,3)*d1ij11 + d3r(2,1,3)*d1ij21 +  &
                 d3r(3,1,3)*d1ij31 + d3r(1,2,3)*d1ij12 +  &
                 d3r(2,2,3)*d1ij22 + d3r(3,2,3)*d1ij32 +  &
                 d3r(1,3,3)*d1ij13 + d3r(2,3,3)*d1ij23 +  &
                 d3r(3,3,3)*d1ij33
      dw2ds34x = d3i(1,1,1)*d2ij11 + d3i(2,1,1)*d2ij21 +  &
                 d3i(3,1,1)*d2ij31 + d3i(1,2,1)*d2ij12 +  &
                 d3i(2,2,1)*d2ij22 + d3i(3,2,1)*d2ij32 +  &
                 d3i(1,3,1)*d2ij13 + d3i(2,3,1)*d2ij23 +  &
                 d3i(3,3,1)*d2ij33
      dw2ds34y = d3i(1,1,2)*d2ij11 + d3i(2,1,2)*d2ij21 +  &
                 d3i(3,1,2)*d2ij31 + d3i(1,2,2)*d2ij12 +  &
                 d3i(2,2,2)*d2ij22 + d3i(3,2,2)*d2ij32 +  &
                 d3i(1,3,2)*d2ij13 + d3i(2,3,2)*d2ij23 +  &
                 d3i(3,3,2)*d2ij33
      dw2ds34z = d3i(1,1,3)*d2ij11 + d3i(2,1,3)*d2ij21 +  &
                 d3i(3,1,3)*d2ij31 + d3i(1,2,3)*d2ij12 +  &
                 d3i(2,2,3)*d2ij22 + d3i(3,2,3)*d2ij32 +  &
                 d3i(1,3,3)*d2ij13 + d3i(2,3,3)*d2ij23 +  &
                 d3i(3,3,3)*d2ij33
      dw2dsijx = 2.0_dp*(dw2ds12x + dw2ds34x)
      dw2dsijy = 2.0_dp*(dw2ds12y + dw2ds34y)
      dw2dsijz = 2.0_dp*(dw2ds12z + dw2ds34z)
      dw2dsiix = d3(1,1,1)*dii11 + (d3(2,1,1) + d3(1,2,1))*dii21 +  &
                 (d3(3,1,1) + d3(1,3,1))*dii31 + d3(2,2,1)*dii22 +  &
                 (d3(3,2,1) + d3(2,3,1))*dii32 + d3(3,3,1)*dii33
      dw2dsiiy = d3(1,1,2)*dii11 + (d3(2,1,2) + d3(1,2,2))*dii21 +  &
                 (d3(3,1,2) + d3(1,3,2))*dii31 + d3(2,2,2)*dii22 +  &
                 (d3(3,2,2) + d3(2,3,2))*dii32 + d3(3,3,2)*dii33
      dw2dsiiz = d3(1,1,3)*dii11 + (d3(2,1,3) + d3(1,2,3))*dii21 +  &
                 (d3(3,1,3) + d3(1,3,3))*dii31 + d3(2,2,3)*dii22 +  &
                 (d3(3,2,3) + d3(2,3,3))*dii32 + d3(3,3,3)*dii33
      dw2dsjjx = d3(1,1,1)*djj11 + (d3(2,1,1) + d3(1,2,1))*djj21 +  &
                 (d3(3,1,1) + d3(1,3,1))*djj31 + d3(2,2,1)*djj22 +  &
                 (d3(3,2,1) + d3(2,3,1))*djj32 + d3(3,3,1)*djj33
      dw2dsjjy = d3(1,1,2)*djj11 + (d3(2,1,2) + d3(1,2,2))*djj21 +  &
                 (d3(3,1,2) + d3(1,3,2))*djj31 + d3(2,2,2)*djj22 +  &
                 (d3(3,2,2) + d3(2,3,2))*djj32 + d3(3,3,2)*djj33
      dw2dsjjz = d3(1,1,3)*djj11 + (d3(2,1,3) + d3(1,2,3))*djj21 +  &
                 (d3(3,1,3) + d3(1,3,3))*djj31 + d3(2,2,3)*djj22 +  &
                 (d3(3,2,3) + d3(2,3,3))*djj32 + d3(3,3,3)*djj33
      dfedx = (dw2dsijx - dw2dsiix - dw2dsjjx)
      dfedy = (dw2dsijy - dw2dsiiy - dw2dsjjy)
      dfedz = (dw2dsijz - dw2dsiiz - dw2dsjjz)
      if (lstr) then
        do kkk = 1,nstrains
          kk  =  nstrptr(kkk)
          dw2ds12 = d3rs(1,1,kk)*d1ij11 + d3rs(2,1,kk)*d1ij21 +  &
                    d3rs(3,1,kk)*d1ij31 + d3rs(1,2,kk)*d1ij12 +  &
                    d3rs(2,2,kk)*d1ij22 + d3rs(3,2,kk)*d1ij32 +  &
                    d3rs(1,3,kk)*d1ij13 + d3rs(2,3,kk)*d1ij23 +  &
                    d3rs(3,3,kk)*d1ij33
          dw2ds34 = d3is(1,1,kk)*d2ij11 + d3is(2,1,kk)*d2ij21 +  &
                    d3is(3,1,kk)*d2ij31 + d3is(1,2,kk)*d2ij12 +  &
                    d3is(2,2,kk)*d2ij22 + d3is(3,2,kk)*d2ij32 +  &
                    d3is(1,3,kk)*d2ij13 + d3is(2,3,kk)*d2ij23 +  &
                    d3is(3,3,kk)*d2ij33
          dw2dsij = 2.0_dp*(dw2ds12 + dw2ds34)
          dw2dsii = d3s(1,1,kk)*dii11 + (d3s(2,1,kk) + d3s(1,2,kk))*dii21 +  &
                    (d3s(3,1,kk) + d3s(1,3,kk))*dii31 + d3s(2,2,kk)*dii22 +  &
                    (d3s(3,2,kk) + d3s(2,3,kk))*dii32 + d3s(3,3,kk)*dii33
          dw2dsjj = d3s(1,1,kk)*djj11 + (d3s(2,1,kk) + d3s(1,2,kk))*djj21 +  &
                    (d3s(3,1,kk) + d3s(1,3,kk))*djj31 + d3s(2,2,kk)*djj22 +  &
                    (d3s(3,2,kk) + d3s(2,3,kk))*djj32 + d3s(3,3,kk)*djj33
          dfeds(kkk) = (dw2dsij - dw2dsii - dw2dsjj)
        enddo
      endif
      if (lmany.and.nmanyk.gt.0) then
!
!  Real  -  off - diagonal
!
        dt11r = 2.0_dp*d1ij11
        dt21r = 2.0_dp*d1ij21
        dt31r = 2.0_dp*d1ij31
        dt12r = 2.0_dp*d1ij12
        dt22r = 2.0_dp*d1ij22
        dt32r = 2.0_dp*d1ij32
        dt13r = 2.0_dp*d1ij13
        dt23r = 2.0_dp*d1ij23
        dt33r = 2.0_dp*d1ij33
!
!  Imaginary
!
        dt11i = 2.0_dp*d2ij11
        dt21i = 2.0_dp*d2ij21
        dt31i = 2.0_dp*d2ij31
        dt12i = 2.0_dp*d2ij12
        dt22i = 2.0_dp*d2ij22
        dt32i = 2.0_dp*d2ij32
        dt13i = 2.0_dp*d2ij13
        dt23i = 2.0_dp*d2ij23
        dt33i = 2.0_dp*d2ij33
!
!  Real  -  on - diagonal
!
        dt11 = - dii11 - djj11
        dt21 = - dii21 - djj21
        dt31 = - dii31 - djj31
        dt12 = - dii21 - djj21
        dt22 = - dii22 - djj22
        dt32 = - dii32 - djj32
        dt13 = - dii31 - djj31
        dt23 = - dii32 - djj32
        dt33 = - dii33 - djj33
!
        call projthbk3(i,j,nmanyk,nptrmanyk,d33,d33r,d33i, &
          dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
          dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
          dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
          dt22,dt32,dt13,dt23,dt33,dsqh,iocptr,lzsisa)
        if (nforkl.gt.0) then
          call projfork3(nforkl,nptrfork,nptrforl,d34, &
            d34r,d34i,dt11r,dt21r,dt31r,dt12r,dt22r, &
            dt32r,dt13r,dt23r,dt33r,dt11i,dt21i,dt31i, &
            dt12i,dt22i,dt32i,dt13i,dt23i,dt33i,dt11, &
            dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33, &
            dsqh,iocptr,lzsisa)
        endif
        if (lstr) then
          call projthbk3s(nmanyk,d33s,d33rs,d33is, &
            dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
            dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
            dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
            dt22,dt32,dt13,dt23,dt33,dfeds)
          if (nforkl.gt.0) then
            call projfork3s(nforkl,d34s,d34rs,d34is, &
              dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
              dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
              dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
              dt22,dt32,dt13,dt23,dt33,dfeds)
          endif
        endif
      endif
    elseif (lcorei.and..not.lcorej) then
!***************
!  Core - Shell  *
!***************
!
!  As shells are sorted to after cores, this combination
!  should never happen!
!
      print *,' This combination should never be reached!'
      call stopnow('projd3a')
    elseif (.not.lcorei.and.lcorej) then
!***************
!  Shell - Core  *
!***************
      do k = mcvmin,mcvmax
        erjx = eigr(jx,k)
        erjy = eigr(jy,k)
        erjz = eigr(jz,k)
        eijx = eigi(jx,k)
        eijy = eigi(jy,k)
        eijz = eigi(jz,k)
        drix = derv2(k,ix)
        driy = derv2(k,iy)
        driz = derv2(k,iz)
        diix = dervi(k,ix)
        diiy = dervi(k,iy)
        diiz = dervi(k,iz)
!%%%%%%%%%%%%%%%%%%%%%%%%%
!  e.(D3).Psn component  %
!%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        d1ij11 = d1ij11 + drix*erjx + diix*eijx
        d1ij21 = d1ij21 + driy*erjx + diiy*eijx
        d1ij31 = d1ij31 + driz*erjx + diiz*eijx
        d1ij12 = d1ij12 + drix*erjy + diix*eijy
        d1ij22 = d1ij22 + driy*erjy + diiy*eijy
        d1ij32 = d1ij32 + driz*erjy + diiz*eijy
        d1ij13 = d1ij13 + drix*erjz + diix*eijz
        d1ij23 = d1ij23 + driy*erjz + diiy*eijz
        d1ij33 = d1ij33 + driz*erjz + diiz*eijz
!
!  Imag - imag - real  -  real*imag*imag
!
        d2ij11 = d2ij11 + diix*erjx - drix*eijx
        d2ij21 = d2ij21 + diiy*erjx - driy*eijx
        d2ij31 = d2ij31 + diiz*erjx - driz*eijx
        d2ij12 = d2ij12 + diix*erjy - drix*eijy
        d2ij22 = d2ij22 + diiy*erjy - driy*eijy
        d2ij32 = d2ij32 + diiz*erjy - driz*eijy
        d2ij13 = d2ij13 + diix*erjz - drix*eijz
        d2ij23 = d2ij23 + diiy*erjz - driy*eijz
        d2ij33 = d2ij33 + diiz*erjz - driz*eijz
!%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  Pns.(D3).Psn component  %
!%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        dii11 = dii11 + drix*drix + diix*diix
        dii21 = dii21 + drix*driy + diix*diiy
        dii31 = dii31 + drix*driz + diix*diiz
        dii22 = dii22 + driy*driy + diiy*diiy
        dii32 = dii32 + driy*driz + diiy*diiz
        dii33 = dii33 + driz*driz + diiz*diiz
        djj11 = djj11 + erjx*erjx + eijx*eijx
        djj21 = djj21 + erjx*erjy + eijx*eijy
        djj31 = djj31 + erjx*erjz + eijx*eijz
        djj22 = djj22 + erjy*erjy + eijy*eijy
        djj32 = djj32 + erjy*erjz + eijy*eijz
        djj33 = djj33 + erjz*erjz + eijz*eijz
      enddo
!
!  Convert to free energy derivatives
!
      dw2ds12x = d3r(1,1,1)*d1ij11 + d3r(2,1,1)*d1ij21 +  &
                 d3r(3,1,1)*d1ij31 + d3r(1,2,1)*d1ij12 +  &
                 d3r(2,2,1)*d1ij22 + d3r(3,2,1)*d1ij32 +  &
                 d3r(1,3,1)*d1ij13 + d3r(2,3,1)*d1ij23 +  &
                 d3r(3,3,1)*d1ij33
      dw2ds12y = d3r(1,1,2)*d1ij11 + d3r(2,1,2)*d1ij21 +  &
                 d3r(3,1,2)*d1ij31 + d3r(1,2,2)*d1ij12 +  &
                 d3r(2,2,2)*d1ij22 + d3r(3,2,2)*d1ij32 +  &
                 d3r(1,3,2)*d1ij13 + d3r(2,3,2)*d1ij23 +  &
                 d3r(3,3,2)*d1ij33
      dw2ds12z = d3r(1,1,3)*d1ij11 + d3r(2,1,3)*d1ij21 +  &
                 d3r(3,1,3)*d1ij31 + d3r(1,2,3)*d1ij12 +  &
                 d3r(2,2,3)*d1ij22 + d3r(3,2,3)*d1ij32 +  &
                 d3r(1,3,3)*d1ij13 + d3r(2,3,3)*d1ij23 +  &
                 d3r(3,3,3)*d1ij33
      dw2ds34x = d3i(1,1,1)*d2ij11 + d3i(2,1,1)*d2ij21 +  &
                 d3i(3,1,1)*d2ij31 + d3i(1,2,1)*d2ij12 +  &
                 d3i(2,2,1)*d2ij22 + d3i(3,2,1)*d2ij32 +  &
                 d3i(1,3,1)*d2ij13 + d3i(2,3,1)*d2ij23 +  &
                 d3i(3,3,1)*d2ij33
      dw2ds34y = d3i(1,1,2)*d2ij11 + d3i(2,1,2)*d2ij21 +  &
                 d3i(3,1,2)*d2ij31 + d3i(1,2,2)*d2ij12 +  &
                 d3i(2,2,2)*d2ij22 + d3i(3,2,2)*d2ij32 +  &
                 d3i(1,3,2)*d2ij13 + d3i(2,3,2)*d2ij23 +  &
                 d3i(3,3,2)*d2ij33
      dw2ds34z = d3i(1,1,3)*d2ij11 + d3i(2,1,3)*d2ij21 +  &
                 d3i(3,1,3)*d2ij31 + d3i(1,2,3)*d2ij12 +  &
                 d3i(2,2,3)*d2ij22 + d3i(3,2,3)*d2ij32 +  &
                 d3i(1,3,3)*d2ij13 + d3i(2,3,3)*d2ij23 +  &
                 d3i(3,3,3)*d2ij33
      dw2dsijx = 2.0_dp*(dw2ds12x + dw2ds34x)*rmassj
      dw2dsijy = 2.0_dp*(dw2ds12y + dw2ds34y)*rmassj
      dw2dsijz = 2.0_dp*(dw2ds12z + dw2ds34z)*rmassj
      dw2dsiix = d3(1,1,1)*dii11 + (d3(2,1,1) + d3(1,2,1))*dii21 +  &
                 (d3(3,1,1) + d3(1,3,1))*dii31 + d3(2,2,1)*dii22 +  &
                 (d3(3,2,1) + d3(2,3,1))*dii32 + d3(3,3,1)*dii33
      dw2dsiiy = d3(1,1,2)*dii11 + (d3(2,1,2) + d3(1,2,2))*dii21 +  &
                 (d3(3,1,2) + d3(1,3,2))*dii31 + d3(2,2,2)*dii22 +  &
                 (d3(3,2,2) + d3(2,3,2))*dii32 + d3(3,3,2)*dii33
      dw2dsiiz = d3(1,1,3)*dii11 + (d3(2,1,3) + d3(1,2,3))*dii21 +  &
                 (d3(3,1,3) + d3(1,3,3))*dii31 + d3(2,2,3)*dii22 +  &
                 (d3(3,2,3) + d3(2,3,3))*dii32 + d3(3,3,3)*dii33
      dw2dsjjx = d3(1,1,1)*djj11 + (d3(2,1,1) + d3(1,2,1))*djj21 +  &
                 (d3(3,1,1) + d3(1,3,1))*djj31 + d3(2,2,1)*djj22 +  &
                 (d3(3,2,1) + d3(2,3,1))*djj32 + d3(3,3,1)*djj33
      dw2dsjjy = d3(1,1,2)*djj11 + (d3(2,1,2) + d3(1,2,2))*djj21 +  &
                 (d3(3,1,2) + d3(1,3,2))*djj31 + d3(2,2,2)*djj22 +  &
                 (d3(3,2,2) + d3(2,3,2))*djj32 + d3(3,3,2)*djj33
      dw2dsjjz = d3(1,1,3)*djj11 + (d3(2,1,3) + d3(1,2,3))*djj21 +  &
                 (d3(3,1,3) + d3(1,3,3))*djj31 + d3(2,2,3)*djj22 +  &
                 (d3(3,2,3) + d3(2,3,3))*djj32 + d3(3,3,3)*djj33
      dfedx = - (dw2dsijx + dw2dsiix + dw2dsjjx*rmjj)
      dfedy = - (dw2dsijy + dw2dsiiy + dw2dsjjy*rmjj)
      dfedz = - (dw2dsijz + dw2dsiiz + dw2dsjjz*rmjj)
      if (lstr) then
        do kkk = 1,nstrains
          kk = nstrptr(kkk)
          dw2ds12 = d3rs(1,1,kk)*d1ij11 + d3rs(2,1,kk)*d1ij21 +  &
                    d3rs(3,1,kk)*d1ij31 + d3rs(1,2,kk)*d1ij12 +  &
                    d3rs(2,2,kk)*d1ij22 + d3rs(3,2,kk)*d1ij32 +  &
                    d3rs(1,3,kk)*d1ij13 + d3rs(2,3,kk)*d1ij23 +  &
                    d3rs(3,3,kk)*d1ij33
          dw2ds34 = d3is(1,1,kk)*d2ij11 + d3is(2,1,kk)*d2ij21 +  &
                    d3is(3,1,kk)*d2ij31 + d3is(1,2,kk)*d2ij12 +  &
                    d3is(2,2,kk)*d2ij22 + d3is(3,2,kk)*d2ij32 +  &
                    d3is(1,3,kk)*d2ij13 + d3is(2,3,kk)*d2ij23 +  &
                    d3is(3,3,kk)*d2ij33
          dw2dsij = 2.0_dp*(dw2ds12 + dw2ds34)*rmassj
          dw2dsii = d3s(1,1,kk)*dii11 + (d3s(2,1,kk) + d3s(1,2,kk))*dii21 +  &
                    (d3s(3,1,kk) + d3s(1,3,kk))*dii31 + d3s(2,2,kk)*dii22 +  &
                    (d3s(3,2,kk) + d3s(2,3,kk))*dii32 + d3s(3,3,kk)*dii33
          dw2dsjj = d3s(1,1,kk)*djj11 + (d3s(2,1,kk) + d3s(1,2,kk))*djj21 +  &
                    (d3s(3,1,kk) + d3s(1,3,kk))*djj31 + d3s(2,2,kk)*djj22 +  &
                    (d3s(3,2,kk) + d3s(2,3,kk))*djj32 + d3s(3,3,kk)*djj33
          dfeds(kkk) = - (dw2dsij + dw2dsii + dw2dsjj*rmjj)
        enddo
      endif
      if (lmany.and.nmanyk.gt.0) then
!
!  Real  -  off - diagonal
!
        dt11r = - 2.0_dp*d1ij11*rmassj
        dt21r = - 2.0_dp*d1ij21*rmassj
        dt31r = - 2.0_dp*d1ij31*rmassj
        dt12r = - 2.0_dp*d1ij12*rmassj
        dt22r = - 2.0_dp*d1ij22*rmassj
        dt32r = - 2.0_dp*d1ij32*rmassj
        dt13r = - 2.0_dp*d1ij13*rmassj
        dt23r = - 2.0_dp*d1ij23*rmassj
        dt33r = - 2.0_dp*d1ij33*rmassj
!
!  Imaginary
!
        dt11i = - 2.0_dp*d2ij11*rmassj
        dt21i = - 2.0_dp*d2ij21*rmassj
        dt31i = - 2.0_dp*d2ij31*rmassj
        dt12i = - 2.0_dp*d2ij12*rmassj
        dt22i = - 2.0_dp*d2ij22*rmassj
        dt32i = - 2.0_dp*d2ij32*rmassj
        dt13i = - 2.0_dp*d2ij13*rmassj
        dt23i = - 2.0_dp*d2ij23*rmassj
        dt33i = - 2.0_dp*d2ij33*rmassj
!
!  Real  -  on - diagonal
!
        dt11 = - (dii11 + djj11*rmjj)
        dt21 = - (dii21 + djj21*rmjj)
        dt31 = - (dii31 + djj31*rmjj)
        dt12 = - (dii21 + djj21*rmjj)
        dt22 = - (dii22 + djj22*rmjj)
        dt32 = - (dii32 + djj32*rmjj)
        dt13 = - (dii31 + djj31*rmjj)
        dt23 = - (dii32 + djj32*rmjj)
        dt33 = - (dii33 + djj33*rmjj)
!
        call projthbk3(i,j,nmanyk,nptrmanyk,d33,d33r,d33i, &
          dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
          dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
          dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
          dt22,dt32,dt13,dt23,dt33,dsqh,iocptr,lzsisa)
        if (nforkl.gt.0) then
          call projfork3(nforkl,nptrfork,nptrforl,d34, &
            d34r,d34i,dt11r,dt21r,dt31r,dt12r,dt22r, &
            dt32r,dt13r,dt23r,dt33r,dt11i,dt21i,dt31i, &
            dt12i,dt22i,dt32i,dt13i,dt23i,dt33i,dt11, &
            dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33, &
            dsqh,iocptr,lzsisa)
        endif
        if (lstr) then
          call projthbk3s(nmanyk,d33s,d33rs,d33is, &
            dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
            dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
            dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
            dt22,dt32,dt13,dt23,dt33,dfeds)
          if (nforkl.gt.0) then
            call projfork3s(nforkl,d34s,d34rs,d34is, &
              dt11r,dt21r,dt31r,dt12r,dt22r,dt32r,dt13r, &
              dt23r,dt33r,dt11i,dt21i,dt31i,dt12i,dt22i, &
              dt32i,dt13i,dt23i,dt33i,dt11,dt21,dt31,dt12, &
              dt22,dt32,dt13,dt23,dt33,dfeds)
          endif
        endif
      endif
    endif
  elseif (i.eq.j.and.(lstr.or.lmany)) then
!$$$$$$$$$$$$$$$
!  i  =  j case  $
!$$$$$$$$$$$$$$$
    dii11 = 0.0_dp
    dii21 = 0.0_dp
    dii31 = 0.0_dp
    dii22 = 0.0_dp
    dii32 = 0.0_dp
    dii33 = 0.0_dp
    if (lcorei.and.lcorej) then
!**************
!  Core - Core  *
!**************
      do k = mcvmin,mcvmax
        erix = eigr(ix,k)
        eriy = eigr(iy,k)
        eriz = eigr(iz,k)
        eiix = eigi(ix,k)
        eiiy = eigi(iy,k)
        eiiz = eigi(iz,k)
!%%%%%%%%%%%%%%%%
!  On diagonal  %
!%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        dii11 = dii11 + erix*erix + eiix*eiix
        dii21 = dii21 + erix*eriy + eiix*eiiy
        dii31 = dii31 + erix*eriz + eiix*eiiz
        dii22 = dii22 + eriy*eriy + eiiy*eiiy
        dii32 = dii32 + eriy*eriz + eiiy*eiiz
        dii33 = dii33 + eriz*eriz + eiiz*eiiz
      enddo
!
!  Multiply by mass factor
!
      dii11 = dii11*rmii
      dii21 = dii21*rmii
      dii31 = dii31*rmii
      dii22 = dii22*rmii
      dii32 = dii32*rmii
      dii33 = dii33*rmii
!
!  Convert to free energy derivatives
!
      if (lstr) then
        do kkk = 1,nstrains
          kk  =  nstrptr(kkk)
!
!  Take difference of phased on - diagonal element and pure real term
!
          do ii = 1,3
            d3d(1,ii) = d3rs(1,ii,kk) - d3s(1,ii,kk)
            d3d(2,ii) = d3rs(2,ii,kk) - d3s(2,ii,kk)
            d3d(3,ii) = d3rs(3,ii,kk) - d3s(3,ii,kk)
          enddo
          dw2dsii = d3d(1,1)*dii11 + 2.0_dp*d3d(2,1)*dii21 +  &
                    2.0_dp*d3d(3,1)*dii31 + d3d(2,2)*dii22 +  &
                    2.0_dp*d3d(3,2)*dii32 + d3d(3,3)*dii33
          dfeds(kkk) = dw2dsii
        enddo
      endif
      if (lmany.and.nmanyk.gt.0) then
!
!  Three - body free energy derivatives of diagonal elements
!
        call projthbk3d(i,nmanyk,nptrmanyk,d33,d33r,dii11,dii21,dii31,dii22,dii32,dii33,iocptr,lzsisa)
        if (nforkl.gt.0) then
          call projfork3d(nforkl,nptrfork,nptrforl,d34,d34r,dii11,dii21,dii31,dii22,dii32,dii33, &
            dsqh,iocptr,lzsisa)
        endif
        if (lstr) then
          call projthbk3ds(nmanyk,d33s,d33rs,dii11,dii21,dii31,dii22,dii32,dii33,dfeds)
          if (nforkl.gt.0) then
            call projfork3ds(nforkl,d34s,d34rs,dii11,dii21,dii31,dii22,dii32,dii33,dfeds)
          endif
        endif
      endif
    elseif (.not.lcorei.and..not.lcorej) then
!****************
!  Shell - Shell  *
!****************
      do k = mcvmin,mcvmax
        drix = derv2(k,ix)
        driy = derv2(k,iy)
        driz = derv2(k,iz)
        diix = dervi(k,ix)
        diiy = dervi(k,iy)
        diiz = dervi(k,iz)
!%%%%%%%%%%%%%%%%
!  On diagonal  %
!%%%%%%%%%%%%%%%%
!
!  Real - real - real  +  Imag - real - imag
!
        dii11 = dii11 + drix*drix + diix*diix
        dii21 = dii21 + drix*driy + diix*diiy
        dii31 = dii31 + drix*driz + diix*diiz
        dii22 = dii22 + driy*driy + diiy*diiy
        dii32 = dii32 + driy*driz + diiy*diiz
        dii33 = dii33 + driz*driz + diiz*diiz
      enddo
!
!  Convert to free energy derivatives
!
      if (lstr) then
        do kkk = 1,nstrains
          kk  =  nstrptr(kkk)
!
!  Take difference of phased on - diagonal element and pure real term
!
          do ii = 1,3
            d3d(1,ii) = d3rs(1,ii,kk) - d3s(1,ii,kk)
            d3d(2,ii) = d3rs(2,ii,kk) - d3s(2,ii,kk)
            d3d(3,ii) = d3rs(3,ii,kk) - d3s(3,ii,kk)
          enddo
          dw2dsii = d3d(1,1)*dii11 + 2.0_dp*d3d(2,1)*dii21 +  &
                    2.0_dp*d3d(3,1)*dii31 + d3d(2,2)*dii22 +  &
                    2.0_dp*d3d(3,2)*dii32 + d3d(3,3)*dii33
          dfeds(kkk) = dw2dsii
        enddo
      endif
      if (lmany.and.nmanyk.gt.0) then
!
!  Three - body free energy derivatives of diagonal elements
!
        call projthbk3d(i,nmanyk,nptrmanyk,d33,d33r,dii11,dii21,dii31,dii22,dii32,dii33,iocptr,lzsisa)
        if (nforkl.gt.0) then
          call projfork3d(nforkl,nptrfork,nptrforl,d34,d34r,dii11,dii21,dii31,dii22,dii32,dii33, &
            dsqh,iocptr,lzsisa)
        endif
        if (lstr) then
          call projthbk3ds(nmanyk,d33s,d33rs,dii11,dii21,dii31,dii22,dii32,dii33,dfeds)
          if (nforkl.gt.0) then
            call projfork3ds(nforkl,d34s,d34rs,dii11,dii21,dii31,dii22,dii32,dii33,dfeds)
          endif
        endif
      endif
    endif
  endif
!
  return
  end
