  subroutine projd0(mcvmin,mcvmax,d3,i,ix,iy,iz,j,jx,jy,jz,lcorei,lcorej,rmassi,rmassj, &
    dfedx,dfedy,dfedz,derv2,eigr,maxd2,lmany,nmanyk,nptrmanyk,d33,nforkl,nptrfork, &
    nptrforl,d34)
!
!  Projects the 3rd derivatives on to the phonon modes for clusters
!
!   9/97 Created from projd3a
!   5/98 Modified to handle the fact that d3 is not symmetric
!        once we go beyond two-body terms
!   8/98 Extra array for k-l third derivatives of i-j block added
!        plus call to projfork0 to handle the projection of d34
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4)       :: i
  integer(i4)       :: ix
  integer(i4)       :: iy
  integer(i4)       :: iz
  integer(i4)       :: j
  integer(i4)       :: jx
  integer(i4)       :: jy
  integer(i4)       :: jz
  integer(i4)       :: maxd2
  integer(i4)       :: mcvmax
  integer(i4)       :: mcvmin
  integer(i4)       :: nforkl
  integer(i4)       :: nmanyk
  integer(i4)       :: nptrfork(*)
  integer(i4)       :: nptrforl(*)
  integer(i4)       :: nptrmanyk(*)
  logical           :: lcorei
  logical           :: lcorej
  logical           :: lmany
  real(dp)          :: d3(3,3,3)
  real(dp)          :: d33(54,*)
  real(dp)          :: d34(27,*)
  real(dp)          :: derv2(maxd2,*)
  real(dp)          :: dfedx
  real(dp)          :: dfedy
  real(dp)          :: dfedz
  real(dp)          :: eigr(maxd2,*)
  real(dp)          :: rmassi
  real(dp)          :: rmassj
!
!  Local variables
!
  integer(i4)       :: k
  real(dp)          :: d1ij11
  real(dp)          :: d1ij21
  real(dp)          :: d1ij31
  real(dp)          :: d1ij12
  real(dp)          :: d1ij22
  real(dp)          :: d1ij32
  real(dp)          :: d1ij13
  real(dp)          :: d1ij23
  real(dp)          :: d1ij33
  real(dp)          :: dii11
  real(dp)          :: dii21
  real(dp)          :: dii31
  real(dp)          :: dii22
  real(dp)          :: dii32
  real(dp)          :: dii33
  real(dp)          :: djj11
  real(dp)          :: djj21
  real(dp)          :: djj31
  real(dp)          :: djj22
  real(dp)          :: djj32
  real(dp)          :: djj33
  real(dp)          :: drix
  real(dp)          :: driy
  real(dp)          :: driz
  real(dp)          :: drjx
  real(dp)          :: drjy
  real(dp)          :: drjz
  real(dp)          :: dt11
  real(dp)          :: dt21
  real(dp)          :: dt31
  real(dp)          :: dt12
  real(dp)          :: dt22
  real(dp)          :: dt32
  real(dp)          :: dt13
  real(dp)          :: dt23
  real(dp)          :: dt33
  real(dp)          :: dw2ds12x
  real(dp)          :: dw2ds12y
  real(dp)          :: dw2ds12z
  real(dp)          :: dw2dsiix
  real(dp)          :: dw2dsiiy
  real(dp)          :: dw2dsiiz
  real(dp)          :: dw2dsijx
  real(dp)          :: dw2dsijy
  real(dp)          :: dw2dsijz
  real(dp)          :: dw2dsjjx
  real(dp)          :: dw2dsjjy
  real(dp)          :: dw2dsjjz
  real(dp)          :: erix
  real(dp)          :: eriy
  real(dp)          :: eriz
  real(dp)          :: erjx
  real(dp)          :: erjy
  real(dp)          :: erjz
  real(dp)          :: rmii
  real(dp)          :: rmij
  real(dp)          :: rmjj
!*********************************************
!  Project contributions on to phonon modes  *
!*********************************************
  rmij = rmassi*rmassj
  rmii = rmassi*rmassi
  rmjj = rmassj*rmassj
  dfedx = 0.0_dp
  dfedy = 0.0_dp
  dfedz = 0.0_dp
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
!%%%%%%%%%%%%%%%%%
!  Off diagonal  %
!%%%%%%%%%%%%%%%%%
!
!  Real - real - real 
!
        d1ij11 = d1ij11 + erix*erjx
        d1ij21 = d1ij21 + eriy*erjx
        d1ij31 = d1ij31 + eriz*erjx
        d1ij12 = d1ij12 + erix*erjy
        d1ij22 = d1ij22 + eriy*erjy
        d1ij32 = d1ij32 + eriz*erjy
        d1ij13 = d1ij13 + erix*erjz
        d1ij23 = d1ij23 + eriy*erjz
        d1ij33 = d1ij33 + eriz*erjz
!%%%%%%%%%%%%%%%%
!  On diagonal  %
!%%%%%%%%%%%%%%%%
!
!  Real - real - real 
!
        dii11 = dii11 + erix*erix
        dii21 = dii21 + erix*eriy
        dii31 = dii31 + erix*eriz
        dii22 = dii22 + eriy*eriy
        dii32 = dii32 + eriy*eriz
        dii33 = dii33 + eriz*eriz
        djj11 = djj11 + erjx*erjx
        djj21 = djj21 + erjx*erjy
        djj31 = djj31 + erjx*erjz
        djj22 = djj22 + erjy*erjy
        djj32 = djj32 + erjy*erjz
        djj33 = djj33 + erjz*erjz
      enddo
!
!  Convert to free energy derivatives
!
!  Factor of 2 comes from the fact that each i - j block appears
!  in two places   -  on - diagonal blocks handled separately as
!  they are real when coming from i - j
!
      dw2ds12x = d3(1,1,1)*d1ij11 + d3(2,1,1)*d1ij21 +  &
                 d3(3,1,1)*d1ij31 + d3(1,2,1)*d1ij12 +  &
                 d3(2,2,1)*d1ij22 + d3(3,2,1)*d1ij32 +  &
                 d3(1,3,1)*d1ij13 + d3(2,3,1)*d1ij23 +  &
                 d3(3,3,1)*d1ij33
      dw2ds12y = d3(1,1,2)*d1ij11 + d3(2,1,2)*d1ij21 +  &
                 d3(3,1,2)*d1ij31 + d3(1,2,2)*d1ij12 +  &
                 d3(2,2,2)*d1ij22 + d3(3,2,2)*d1ij32 +  &
                 d3(1,3,2)*d1ij13 + d3(2,3,2)*d1ij23 +  &
                 d3(3,3,2)*d1ij33
      dw2ds12z = d3(1,1,3)*d1ij11 + d3(2,1,3)*d1ij21 +  &
                 d3(3,1,3)*d1ij31 + d3(1,2,3)*d1ij12 +  &
                 d3(2,2,3)*d1ij22 + d3(3,2,3)*d1ij32 +  &
                 d3(1,3,3)*d1ij13 + d3(2,3,3)*d1ij23 +  &
                 d3(3,3,3)*d1ij33
      dw2dsijx = 2.0_dp*dw2ds12x*rmij
      dw2dsijy = 2.0_dp*dw2ds12y*rmij
      dw2dsijz = 2.0_dp*dw2ds12z*rmij
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
      if (lmany.and.nmanyk.gt.0) then
        dt11 = 2.0_dp*d1ij11*rmij - dii11*rmii - djj11*rmjj
        dt21 = 2.0_dp*d1ij21*rmij - (dii21*rmii + djj21*rmjj)
        dt31 = 2.0_dp*d1ij31*rmij - (dii31*rmii + djj31*rmjj)
        dt12 = 2.0_dp*d1ij12*rmij - (dii21*rmii + djj21*rmjj)
        dt22 = 2.0_dp*d1ij22*rmij - dii22*rmii - djj22*rmjj
        dt32 = 2.0_dp*d1ij32*rmij - (dii32*rmii + djj32*rmjj)
        dt13 = 2.0_dp*d1ij13*rmij - (dii31*rmii + djj31*rmjj)
        dt23 = 2.0_dp*d1ij23*rmij - (dii32*rmii + djj32*rmjj)
        dt33 = 2.0_dp*d1ij33*rmij - dii33*rmii - djj33*rmjj
        call projthbk0(i,j,nmanyk,nptrmanyk,d33,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
        if (nforkl.gt.0) then
          call projfork0(nforkl,nptrfork,nptrforl,d34,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
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
!%%%%%%%%%%%%%%%%%
!  Off - diagonal  %
!%%%%%%%%%%%%%%%%%
!
!  Real - real - real 
!
        d1ij11 = d1ij11 + drix*drjx
        d1ij21 = d1ij21 + driy*drjx
        d1ij31 = d1ij31 + driz*drjx
        d1ij12 = d1ij12 + drix*drjy
        d1ij22 = d1ij22 + driy*drjy
        d1ij32 = d1ij32 + driz*drjy
        d1ij13 = d1ij13 + drix*drjz
        d1ij23 = d1ij23 + driy*drjz
        d1ij33 = d1ij33 + driz*drjz
!%%%%%%%%%%%%%%%%
!  On diagonal  %
!%%%%%%%%%%%%%%%%
!
!  Real - real - real 
!
        dii11 = dii11 + drix*drix
        dii21 = dii21 + drix*driy
        dii31 = dii31 + drix*driz
        dii22 = dii22 + driy*driy
        dii32 = dii32 + driy*driz
        dii33 = dii33 + driz*driz
        djj11 = djj11 + drjx*drjx
        djj21 = djj21 + drjx*drjy
        djj31 = djj31 + drjx*drjz
        djj22 = djj22 + drjy*drjy
        djj32 = djj32 + drjy*drjz
        djj33 = djj33 + drjz*drjz
      enddo
!
!  Convert to free energy derivatives
!
      dw2ds12x = d3(1,1,1)*d1ij11 + d3(2,1,1)*d1ij21 +  &
                 d3(3,1,1)*d1ij31 + d3(1,2,1)*d1ij12 +  &
                 d3(2,2,1)*d1ij22 + d3(3,2,1)*d1ij32 +  &
                 d3(1,3,1)*d1ij13 + d3(2,3,1)*d1ij23 +  &
                 d3(3,3,1)*d1ij33
      dw2ds12y = d3(1,1,2)*d1ij11 + d3(2,1,2)*d1ij21 +  &
                 d3(3,1,2)*d1ij31 + d3(1,2,2)*d1ij12 +  &
                 d3(2,2,2)*d1ij22 + d3(3,2,2)*d1ij32 +  &
                 d3(1,3,2)*d1ij13 + d3(2,3,2)*d1ij23 +  &
                 d3(3,3,2)*d1ij33
      dw2ds12z = d3(1,1,3)*d1ij11 + d3(2,1,3)*d1ij21 +  &
                 d3(3,1,3)*d1ij31 + d3(1,2,3)*d1ij12 +  &
                 d3(2,2,3)*d1ij22 + d3(3,2,3)*d1ij32 +  &
                 d3(1,3,3)*d1ij13 + d3(2,3,3)*d1ij23 +  &
                 d3(3,3,3)*d1ij33
      dw2dsijx = 2.0_dp*dw2ds12x
      dw2dsijy = 2.0_dp*dw2ds12y
      dw2dsijz = 2.0_dp*dw2ds12z
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
      if (lmany.and.nmanyk.gt.0) then
        dt11 = 2.0_dp*d1ij11 - dii11 - djj11
        dt21 = 2.0_dp*d1ij21 - dii21 - djj21
        dt31 = 2.0_dp*d1ij31 - dii31 - djj31
        dt12 = 2.0_dp*d1ij12 - dii21 - djj21
        dt22 = 2.0_dp*d1ij22 - dii22 - djj22
        dt32 = 2.0_dp*d1ij32 - dii32 - djj32
        dt13 = 2.0_dp*d1ij13 - dii31 - djj31
        dt23 = 2.0_dp*d1ij23 - dii32 - djj32
        dt33 = 2.0_dp*d1ij33 - dii33 - djj33
        call projthbk0(i,j,nmanyk,nptrmanyk,d33,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
        if (nforkl.gt.0) then
          call projfork0(nforkl,nptrfork,nptrforl,d34,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
        endif
      endif
    elseif (lcorei.and..not.lcorej) then
!*****************
!  Core - Shell  *
!*****************
!
!  As shells are sorted to after cores, this combination
!  should never happen!
!
      call outerror('This free energy combination should be never reached',0_i4)
      call stopnow('projd0')
    elseif (.not.lcorei.and.lcorej) then
!***************
!  Shell - Core  *
!***************
      do k = mcvmin,mcvmax
        erjx = eigr(jx,k)
        erjy = eigr(jy,k)
        erjz = eigr(jz,k)
        drix = derv2(k,ix)
        driy = derv2(k,iy)
        driz = derv2(k,iz)
!%%%%%%%%%%%%%%%%%%%%%%%%%
!  e.(D3).Psn component  %
!%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  Real - real - real 
!
        d1ij11 = d1ij11 + drix*erjx
        d1ij21 = d1ij21 + driy*erjx
        d1ij31 = d1ij31 + driz*erjx
        d1ij12 = d1ij12 + drix*erjy
        d1ij22 = d1ij22 + driy*erjy
        d1ij32 = d1ij32 + driz*erjz
        d1ij13 = d1ij13 + drix*erjz
        d1ij23 = d1ij23 + driy*erjz
        d1ij33 = d1ij33 + driz*erjz
!%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  Pns.(D3).Psn component  %
!%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!  Real - real - real 
!
        dii11 = dii11 + drix*drix
        dii21 = dii21 + drix*driy
        dii31 = dii31 + drix*driz
        dii22 = dii22 + driy*driy
        dii32 = dii32 + driy*driz
        dii33 = dii33 + driz*driz
        djj11 = djj11 + erjx*erjx
        djj21 = djj21 + erjx*erjy
        djj31 = djj31 + erjx*erjz
        djj22 = djj22 + erjy*erjy
        djj32 = djj32 + erjy*erjz
        djj33 = djj33 + erjz*erjz
      enddo
!
!  Convert to free energy derivatives
!
      dw2ds12x = d3(1,1,1)*d1ij11 + d3(2,1,1)*d1ij21 +  &
                 d3(3,1,1)*d1ij31 + d3(1,2,1)*d1ij12 +  &
                 d3(2,2,1)*d1ij22 + d3(3,2,1)*d1ij32 +  &
                 d3(1,3,1)*d1ij13 + d3(2,3,1)*d1ij23 +  &
                 d3(3,3,1)*d1ij33
      dw2ds12y = d3(1,1,2)*d1ij11 + d3(2,1,2)*d1ij21 +  &
                 d3(3,1,2)*d1ij31 + d3(1,2,2)*d1ij12 +  &
                 d3(2,2,2)*d1ij22 + d3(3,2,2)*d1ij32 +  &
                 d3(1,3,2)*d1ij13 + d3(2,3,2)*d1ij23 +  &
                 d3(3,3,2)*d1ij33
      dw2ds12z = d3(1,1,3)*d1ij11 + d3(2,1,3)*d1ij21 +  &
                 d3(3,1,3)*d1ij31 + d3(1,2,3)*d1ij12 +  &
                 d3(2,2,3)*d1ij22 + d3(3,2,3)*d1ij32 +  &
                 d3(1,3,3)*d1ij13 + d3(2,3,3)*d1ij23 +  &
                 d3(3,3,3)*d1ij33
      dw2dsijx = 2.0_dp*dw2ds12x*rmassj
      dw2dsijy = 2.0_dp*dw2ds12y*rmassj
      dw2dsijz = 2.0_dp*dw2ds12z*rmassj
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
      if (lmany.and.nmanyk.gt.0) then
        dt11 = - 2.0_dp*d1ij11*rmassj - dii11 - djj11*rmjj
        dt21 = - 2.0_dp*d1ij21*rmassj - (dii21 + djj21*rmjj)
        dt31 = - 2.0_dp*d1ij31*rmassj - (dii31 + djj31*rmjj)
        dt12 = - 2.0_dp*d1ij12*rmassj - dii21 - djj21*rmjj
        dt22 = - 2.0_dp*d1ij22*rmassj - dii22 - djj22*rmjj
        dt32 = - 2.0_dp*d1ij32*rmassj - (dii32 + djj32*rmjj)
        dt13 = - 2.0_dp*d1ij13*rmassj - (dii31 + djj31*rmjj)
        dt23 = - 2.0_dp*d1ij23*rmassj - (dii32 + djj32*rmjj)
        dt33 = - 2.0_dp*d1ij33*rmassj - dii33 - djj33*rmjj
        call projthbk0(i,j,nmanyk,nptrmanyk,d33,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
        if (nforkl.gt.0) then
          call projfork0(nforkl,nptrfork,nptrforl,d34,dt11,dt21,dt31,dt12,dt22,dt32,dt13,dt23,dt33)
        endif
      endif
    endif
  endif
!
  return
  end
