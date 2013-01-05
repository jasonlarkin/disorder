  subroutine calcP(nREBOsi,rnREBOs,nREBObo,P,dPdrn,d2Pdrn2,lgrad1,lgrad2)
!
!  Subroutine to calculate the P function of the REBO potential.
!  New generalised version for any species as much as possible. 
!  However, since some of the rules are rather specific this is not
!  as general as it might be!
!
!  On entry : 
!
!  nREBOsi         = REBO species number of i
!  rnREBOs()       = no. of each type of REBO species bonded to i
!  nREBObo         = bond-type indicator for i-j
!
!  On exit :
!
!  P               = P function
!  dPdrn()         = first derivative of P w.r.t. rnh if lgrad1
!  d2Pdrnh2        = second derivative of P w.r.t. rnh if lgrad2
!
!   5/02 Created
!   6/02 Interpolation included
!   6/02 Second derivatives added
!  10/02 Explicit splines introduced
!   9/04 Trap for uncoordinate case added
!  10/04 REBO species number now added as an input variable
!  10/04 C/H/O case added
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, October  2004
!
  use brennerdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nREBOsi
  real(dp),    intent(in)             :: rnREBOs(*)
  integer(i4), intent(in)             :: nREBObo
  real(dp),    intent(out)            :: P
  real(dp),    intent(out)            :: dPdrn(*)
  real(dp),    intent(out)            :: d2Pdrn2(*)
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: m
  integer(i4)                         :: n
  integer(i4)                         :: nc
  integer(i4)                         :: nh
  logical                             :: lCHonly
  real(dp)                            :: Ptrm
  real(dp)                            :: Pval(6)
  real(dp)                            :: rc
  real(dp)                            :: rch
  real(dp)                            :: rh
  real(dp)                            :: ro
!
!  Calculate value and derivatives according to spline
!
  P = 0.0_dp
  if (lgrad1) then
    dPdrn(1:nREBOspecies) = 0.0_dp
    if (lgrad2) then
      d2Pdrn2(1:nREBOspecies*(nREBOspecies+1)/2) = 0.0_dp
    endif
  endif
!
!  Find whether this is a C/H only case or general
!
  lCHonly = (nREBObo.le.3)
  if (lCHonly) then
    n = 2
    do while (lCHonly.and.n.lt.nREBOspecies)
      n = n + 1
      lCHonly = (rnREBOs(n).eq.0.0d0)
    enddo
  endif
  if (lCHonly) then
!*********************************************
!  C/H case : Bicubic spline based on Nc/Nh  *
!*********************************************
!
!  If i is not carbon then there is no contribution
!
    if (nREBOsi.ne.1) return
!
!  Find integers of rnh/rnc
!
    nc = int(rnREBOs(1)+1.0d-12)
    nh = int(rnREBOs(2)+1.0d-12)
    rc = rnREBOs(1) + 1.0_dp
    rh = rnREBOs(2) + 1.0_dp
!     
!  If greater than maxima lower as appropriate
!     
    nc = min(nc,3_i4)                   
    nh = min(nh,3_i4) 
    rc = min(rc,4.0_dp)                 
    rh = min(rh,4.0_dp)
!
!  If there are no neighbours then skip calculation
!
    if ((nc+nh).eq.0) return
!
    if (lbrennersplineh) then
      rc = rc - 1.0_dp
      rh = rh - 1.0_dp
      call BicubicSpline(bP_CH(0,0,0,nREBObo),bP_CHcoeff(1,1,0,0,nREBObo),lbP_CHdone(0,0,nREBObo),3_i4,3_i4,rc,rh, &
                         Pval,lgrad1,lgrad2)
      P = Pval(1)
      if (lgrad1) then
        dPdrn(1) = Pval(2)
        dPdrn(2) = Pval(3)
        if (lgrad2) then
          d2Pdrn2(1) = Pval(4)
          d2Pdrn2(2) = Pval(5)
          d2Pdrn2(3) = Pval(6)
        endif
      endif
    else
      do m = 1,16
        Ptrm = cobicubic(m,nh,nc,nREBObo)*(rc**pbicubic(m,1))*(rh**pbicubic(m,2))
        P = P + Ptrm
        if (lgrad1) then
          dPdrn(1) = dPdrn(1) + Ptrm*pbicubic(m,1)/rc
          dPdrn(2) = dPdrn(2) + Ptrm*pbicubic(m,2)/rh
          if (lgrad2) then
            d2Pdrn2(1) = d2Pdrn2(1) + Ptrm*pbicubic(m,1)*(pbicubic(m,1) - 1.0_dp)/(rc*rc)
            d2Pdrn2(2) = d2Pdrn2(2) + Ptrm*pbicubic(m,1)*pbicubic(m,2)/(rc*rh)
            d2Pdrn2(3) = d2Pdrn2(3) + Ptrm*pbicubic(m,2)*(pbicubic(m,2) - 1.0_dp)/(rh*rh)
          endif
        endif
      enddo
    endif
  else
!***************************************************
!  C/H/O case : Bicubic spline based on Nc+Nh & No *
!***************************************************
    ro = min(rnREBOs(3),3.0_dp)
    rch = min((rnREBOs(1)+rnREBOs(2)),3.0_dp)
    call BicubicSpline(bP(0,0,0,nREBObo,nREBOsi),bPcoeff(1,1,0,0,nREBObo,nREBOsi),lbPdone(0,0,nREBObo,nREBOsi),3_i4,3_i4,rch,ro, &
                       Pval,lgrad1,lgrad2)
    P = Pval(1)
    if (lgrad1) then
      dPdrn(1) = Pval(2)
      dPdrn(2) = Pval(2)
      dPdrn(3) = Pval(3)
      if (lgrad2) then
        d2Pdrn2(1) = Pval(4)
        d2Pdrn2(2) = Pval(4)
        d2Pdrn2(3) = Pval(4)
        d2Pdrn2(4) = Pval(5)
        d2Pdrn2(5) = Pval(5)
        d2Pdrn2(6) = Pval(6)
      endif
    endif
  endif
!
  return
  end
!
  subroutine calcF(nREBObo,i,j,nij,nji,rni,rnj,rnc,rnci,rncj,maxn, &
    nneigh,d1i,d1j,d2i,d2j,dNdr,d2Ndr2,drncidr,drncjdr,d2rncidr2, &
    d2rncjdr2,F,lgrad1,lgrad2)
!
!  Subroutine to calculate the F function of the Brenner potential
!
!  On entry : 
!
!  nREBObo          = pointer to bond type
!  i                = atom number of i
!  j                = atom number of j
!  nij              = no. of atom j in i's neighbour list
!  nji              = no. of atom i in j's neighbour list
!  rni              = no. of C+H atoms bonded to i
!  rnj              = no. of C+H atoms bonded to j
!  rnc              = conjugation factor for i-j
!  rnci             = conjugation factor for i-j - contribution from i
!  rncj             = conjugation factor for i-j - contribution from j
!  maxn             = maximum number of neighbours
!  nneigh           = array of no. of neighbours of each atom
!  d1i              = array of first derivatives of atom distances to i
!  d1j              = array of first derivatives of atom distances to j
!  d2i              = array of second derivatives of atom distances to i
!  d2j              = array of second derivatives of atom distances to j
!  dNdr             = first derivatives of neighbour numbers
!  d2Ndr2           = second derivatives of neighbour numbers
!  drncidr          = first derivative of rni with respect to distances
!  drncjdr          = first derivative of rnj with respect to distances
!  d2rncidr2        = second derivative of rni with respect to distances
!  d2rncjdr2        = second derivative of rnj with respect to distances
!  lgrad1           = if .true. then calculate first derivatives
!  lgrad2           = if .true. then calculate second derivatives
!
!  On exit :
!
!  F                = F function
!  d1i              = array of first derivatives of atom distances to i
!  d1j              = array of first derivatives of atom distances to j
!  d2i              = array of second derivatives of atom distances to i
!  d2j              = array of second derivatives of atom distances to j
!
!   5/02 Created
!   6/02 Splines and first derivatives added
!   6/02 Second derivatives added
!   8/02 Derivatives made internal to calcF instead of brenner.f
!  10/02 Use of explicit splines added
!   7/03 Bug in order of first derivative assignments from splines corrected
!  10/04 Explicit spline removed since the answers differ and the derivatives
!        to second order were not available for all terms.
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
!  Copyright Curtin University 2004
!
!  Julian Gale, NRI, Curtin University, October 2004
!
  use brennerdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: nREBObo
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: j
  integer(i4), intent(in)             :: nij
  integer(i4), intent(in)             :: nji
  real(dp),    intent(in)             :: rni
  real(dp),    intent(in)             :: rnj
  real(dp),    intent(in)             :: rnc
  real(dp),    intent(in)             :: rnci
  real(dp),    intent(in)             :: rncj
  integer(i4), intent(in)             :: maxn
  integer(i4), intent(in)             :: nneigh(*)
  real(dp),    intent(inout)          :: d1i(*)
  real(dp),    intent(inout)          :: d1j(*)
  real(dp),    intent(inout)          :: d2i(*)
  real(dp),    intent(inout)          :: d2j(*)
  real(dp),    intent(in)             :: dNdr(maxn,*)
  real(dp),    intent(in)             :: d2Ndr2(maxn,*)
  real(dp),    intent(in)             :: drncidr(*)
  real(dp),    intent(in)             :: drncjdr(*)
  real(dp),    intent(in)             :: d2rncidr2(*)
  real(dp),    intent(in)             :: d2rncjdr2(*)
  real(dp),    intent(out)            :: F
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: m
  integer(i4)                         :: n
  integer(i4)                         :: nc
  integer(i4)                         :: ni
  integer(i4)                         :: nj
  integer(i4)                         :: nn
  integer(i4)                         :: nn1
  real(dp)                            :: d1trm
  real(dp)                            :: d2trm
  real(dp)                            :: dFdrni
  real(dp)                            :: dFdrnj
  real(dp)                            :: dFdrnc
  real(dp)                            :: d2Fdrni2
  real(dp)                            :: d2Fdrnidrnj
  real(dp)                            :: d2Fdrnidrnc
  real(dp)                            :: d2Fdrnj2
  real(dp)                            :: d2Fdrnjdrnc
  real(dp)                            :: d2Fdrnc2
  real(dp)                            :: Ftrm
  real(dp)                            :: rc
  real(dp)                            :: ri
  real(dp)                            :: rj
!     
!  Initialise values
!     
  F = 0.0_dp   
  if (lgrad1) then
    dFdrni = 0.0_dp
    dFdrnj = 0.0_dp
    dFdrnc = 0.0_dp
    if (lgrad2) then
      d2Fdrni2 = 0.0_dp
      d2Fdrnj2 = 0.0_dp
      d2Fdrnidrnj = 0.0_dp
      d2Fdrnc2    = 0.0_dp
      d2Fdrnidrnc = 0.0_dp
      d2Fdrnjdrnc = 0.0_dp
    endif
  endif
!
!  If F is not needed for this bond type return
!
  if (.not.Fneeded(nREBObo)) return
!
!  Find integers of rnh/rnc
!
  ni = int(rni + 1.0d-12)
  nj = int(rnj + 1.0d-12)
  nc = int(rnc + 1.0d-12) - 1
  ri = rni + 1.0_dp
  rj = rnj + 1.0_dp
  rc = rnc
!
!  If greater than maxima lower as appropriate
!
  ni = min(ni,3_i4)
  nj = min(nj,3_i4)
  nc = min(nc,8_i4)
  ri = min(ri,4.0_dp)
  rj = min(rj,4.0_dp)
  rc = min(rc,9.0_dp)
!     
!  Calculate value and derivatives according to spline
!     
  do m = 1,64  
    Ftrm = cotricubic(m,nj,ni,nc,nREBObo)*(ri**ptricubic(m,1))*(rj**ptricubic(m,2))*(rc**ptricubic(m,3))
    F = F + Ftrm
    if (lgrad1) then
      dFdrni = dFdrni + Ftrm*ptricubic(m,1)/ri
      dFdrnj = dFdrnj + Ftrm*ptricubic(m,2)/rj
      dFdrnc = dFdrnc + Ftrm*ptricubic(m,3)/rc
      if (lgrad2) then
        d2Fdrni2 = d2Fdrni2 + Ftrm*ptricubic(m,1)*(ptricubic(m,1) - 1.0_dp)/(ri*ri)
        d2Fdrnj2 = d2Fdrnj2 + Ftrm*ptricubic(m,2)*(ptricubic(m,2) - 1.0_dp)/(rj*rj)
        d2Fdrnidrnj = d2Fdrnidrnj + Ftrm*ptricubic(m,1)*ptricubic(m,2)/(ri*rj)
        d2Fdrnc2 = d2Fdrnc2 + Ftrm*ptricubic(m,3)*(ptricubic(m,3) - 1.0_dp)/(rc*rc)
        d2Fdrnidrnc = d2Fdrnidrnc + Ftrm*ptricubic(m,1)*ptricubic(m,3)/(ri*rc)
        d2Fdrnjdrnc = d2Fdrnjdrnc + Ftrm*ptricubic(m,2)*ptricubic(m,3)/(rj*rc)
      endif
    endif
  enddo
!
!  Scale F by 1/2 : Not documented in Brenner paper but needed to
!  agree with published results.
!
  F = 0.5_dp*F
!
!  Derivatives
!
  if (lgrad1) then
!
!  First derivatives of F w.r.t. Ni and Nj
!
    d1trm = 0.5_dp*dFdrni
    do m = 1,nneigh(i) 
      if (m.ne.nij) then
        d1i(m) = d1i(m) + d1trm*dNdr(m,i)
      endif
    enddo
    d1trm = 0.5_dp*dFdrnj
    do m = 1,nneigh(j) 
      if (m.ne.nji) then
        d1j(m) = d1j(m) + d1trm*dNdr(m,j)
      endif
    enddo
!
!  First derivatives of F w.r.t. Nconj
!
    do m = 1,nneigh(i)
      d1i(m) = d1i(m) + dFdrnc*rnci*drncidr(m)
    enddo
    do m = 1,nneigh(j)
      d1j(m) = d1j(m) + dFdrnc*rncj*drncjdr(m)
    enddo
    if (lgrad2) then
!
!  Second derivatives of F w.r.t. Ni and Nj
!
      do m = 1,nneigh(i) 
        if (m.ne.nij) then
          nn = m*(m + 1)/2
          d2i(nn) = d2i(nn) + 0.5_dp*dFdrni*d2Ndr2(m,i)
          do n = 1,m
            if (n.ne.nij) then
              nn = m*(m - 1)/2 + n
              d2i(nn) = d2i(nn) + 0.5_dp*d2Fdrni2*dNdr(m,i)*dNdr(n,i)
            endif
          enddo
        endif
      enddo
      do m = 1,nneigh(j) 
        if (m.ne.nji) then
          nn = m*(m + 1)/2
          d2j(nn) = d2j(nn) + 0.5_dp*dFdrnj*d2Ndr2(m,j)
          do n = 1,m
            if (n.ne.nji) then
              nn = m*(m - 1)/2 + n
              d2j(nn) = d2j(nn) + 0.5_dp*d2Fdrnj2*dNdr(m,j)*dNdr(n,j)
            endif
          enddo
        endif
      enddo
!
      d2trm = 0.5_dp*d2Fdrnidrnj
      do m = 1,nneigh(i)
        if (m.ne.nij) then
          do n = 1,nneigh(j)
            if (n.ne.nji) then
              nn1 = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nij - 1)*(maxn+1)*maxn + nij*maxn + n
              nn = nn1*(nn1 - 1)/2 + m
              d2i(nn) = d2i(nn) + d2trm*dNdr(m,i)*dNdr(n,j)
            endif
          enddo
        endif
      enddo
!                     
!  Second derivatives of F w.r.t. Nconj
!               
      d2trm = dFdrnc + 2.0_dp*rnci*rnci*d2Fdrnc2
      nn = 0
      do m = 1,nneigh(i)
        do n = 1,m
          nn = nn + 1
          d2i(nn) = d2i(nn) + dFdrnc*rnci*d2rncidr2(nn)
          d2i(nn) = d2i(nn) + d2trm*drncidr(m)*drncidr(n)
        enddo
      enddo
!
      d2trm = dFdrnc + 2.0_dp*rncj*rncj*d2Fdrnc2
      nn = 0
      do m = 1,nneigh(j) 
        do n = 1,m
          nn = nn + 1
          d2j(nn) = d2j(nn) + dFdrnc*rncj*d2rncjdr2(nn)
          d2j(nn) = d2j(nn) + d2trm*drncjdr(m)*drncjdr(n)
        enddo
      enddo
!
      d2trm = 2.0_dp*d2Fdrnc2*rnci*rncj
      do m = 1,nneigh(i)
        do n = 1,nneigh(j)
          nn1 = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nij - 1)*(maxn+1)*maxn + nij*maxn + n
          nn = nn1*(nn1 - 1)/2 + m
          d2i(nn) = d2i(nn) + d2trm*drncidr(m)*drncjdr(n)
        enddo
      enddo
!
!  Second derivatives with respect to mix of Ni/Nj/Nconj
!
      d2trm = 2.0_dp*d2Fdrnidrnc*rnci
      do m = 1,nneigh(i)
        if (m.ne.nij) then
          do n = 1,m
            nn = m*(m - 1)/2 + n
            d2i(nn) = d2i(nn) + d2trm*drncidr(n)*dNdr(m,i)
          enddo
        endif
      enddo
      d2trm = 2.0_dp*d2Fdrnjdrnc*rncj
      do m = 1,nneigh(j)
        if (m.ne.nji) then
          do n = 1,m
            nn = m*(m - 1)/2 + n
            d2j(nn) = d2j(nn) + d2trm*drncjdr(n)*dNdr(m,j)
          enddo
        endif
      enddo
!
      d2trm = d2Fdrnidrnc*rncj
      do m = 1,nneigh(i)
        if (m.ne.nij) then
          do n = 1,nneigh(j)
            nn1 = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nij - 1)*(maxn+1)*maxn + nij*maxn + n
            nn = nn1*(nn1 - 1)/2 + m
            d2i(nn) = d2i(nn) + d2trm*drncjdr(n)*dNdr(m,i)
          enddo
        endif
      enddo
      d2trm = d2Fdrnjdrnc*rnci
      do m = 1,nneigh(j)
        if (m.ne.nji) then
          do n = 1,nneigh(i)
            nn1 = nneigh(j) + nneigh(j)*(nneigh(j) + 1)/2 + (nji - 1)*(maxn+1)*maxn + nji*maxn + n
            nn = nn1*(nn1 - 1)/2 + m
            d2j(nn) = d2j(nn) + d2trm*drncidr(n)*dNdr(m,j)
          enddo
        endif
      enddo
    endif
  endif
!
  return
  end
!
  subroutine calcT(i,j,nij,nji,rni,rnj,rnc,rnci,rncj,rij,xji,yji, &
    zji,maxn,nneigh,rneigh,xneigh,yneigh,zneigh,nREBObond,T,d1i,d1j, &
    d2i,d2j,dNdr,d2Ndr2,drncidr,drncjdr,d2rncidr2,d2rncjdr2,lgrad1, &
    lgrad2)
!
!  Subroutine to calculate the T function of the Brenner potential
!
!  On entry : 
!
!  i               = atom number of i
!  j               = atom number of j
!  nij             = no. of atom j in i's neighbour list
!  nji             = no. of atom i in j's neighbour list
!  rni             = no. of C+H atoms bonded to i
!  rnj             = no. of C+H atoms bonded to j
!  rnc             = conjugation factor for i-j
!  rnci            = conjugation factor for i-j - contribution from i
!  rncj            = conjugation factor for i-j - contribution from j
!  rij             = distance from i to j
!  xji             = x component of vector from i to j
!  yji             = y component of vector from i to j
!  zji             = z component of vector from i to j
!  maxn            = maximum number of neighbours
!  nneigh          = array of no. of neighbours of each atom
!  rneigh          = distance of neighbours
!  xneigh          = x component of vector to neighbours
!  yneigh          = y component of vector to neighbours
!  zneigh          = z component of vector to neighbours
!  nREBObond       = i-j bond type indicator
!  d1i             = array of first derivatives of atom distances to i
!  d1j             = array of first derivatives of atom distances to j
!  d2i             = array of second derivatives of atom distances to i
!  d2j             = array of second derivatives of atom distances to j
!  dNdr            = first derivatives of neighbour numbers
!  d2Ndr2          = second derivatives of neighbour numbers
!  drncidr         = first derivative of rni with respect to distances
!  drncjdr         = first derivative of rnj with respect to distances
!  d2rncidr2       = second derivative of rni with respect to distances
!  d2rncjdr2       = second derivative of rnj with respect to distances
!  lgrad1          = if .true. then calculate first derivatives
!  lgrad2          = if .true. then calculate second derivatives
!
!  On exit :
!
!  T               = T function
!  d1i             = array of first derivatives of atom distances to i
!  d1j             = array of first derivatives of atom distances to j
!  d2i             = array of second derivatives of atom distances to i
!  d2j             = array of second derivatives of atom distances to j
!
!   5/02 Created
!   6/02 Derivatives added
!   7/02 Second derivatives added
!   8/07 lgrad1 corrected to lgrad2 in one place
!  11/07 Unused variables cleaned up
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, November 2007
!
  use brennerdata
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: j
  integer(i4), intent(in)             :: nij
  integer(i4), intent(in)             :: nji
  real(dp),    intent(in)             :: rni
  real(dp),    intent(in)             :: rnj
  real(dp),    intent(in)             :: rnc
  real(dp),    intent(in)             :: rnci
  real(dp),    intent(in)             :: rncj
  real(dp),    intent(in)             :: rij
  real(dp),    intent(in)             :: xji
  real(dp),    intent(in)             :: yji
  real(dp),    intent(in)             :: zji
  integer(i4), intent(in)             :: maxn
  integer(i4), intent(in)             :: nneigh(*)
  real(dp),    intent(in)             :: rneigh(maxn,*)
  real(dp),    intent(in)             :: xneigh(maxn,*)
  real(dp),    intent(in)             :: yneigh(maxn,*)
  real(dp),    intent(in)             :: zneigh(maxn,*)
  integer(i4), intent(in)             :: nREBObond(maxn,*)
  real(dp),    intent(out)            :: T
  real(dp),    intent(inout)          :: d1i(*)
  real(dp),    intent(inout)          :: d1j(*)
  real(dp),    intent(inout)          :: d2i(*)
  real(dp),    intent(inout)          :: d2j(*)
  real(dp),    intent(in)             :: dNdr(maxn,*)
  real(dp),    intent(in)             :: d2Ndr2(maxn,*)
  real(dp),    intent(in)             :: drncidr(*)
  real(dp),    intent(in)             :: drncjdr(*)
  real(dp),    intent(in)             :: d2rncidr2(*)
  real(dp),    intent(in)             :: d2rncjdr2(*)
  logical,     intent(in)             :: lgrad1
  logical,     intent(in)             :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: indi(6)
  integer(i4)                         :: indj(6)
  integer(i4)                         :: ii
  integer(i4)                         :: k
  integer(i4)                         :: l
  integer(i4)                         :: m
  integer(i4)                         :: n
  integer(i4)                         :: ni
  integer(i4)                         :: nj
  integer(i4)                         :: nc
  integer(i4)                         :: nn
  integer(i4)                         :: nn1
  real(dp)                            :: d1trm
  real(dp)                            :: d2trm
  real(dp)                            :: dfikdr
  real(dp)                            :: d2fikdr2
  real(dp)                            :: d3fikdr3
  real(dp)                            :: dfjldr
  real(dp)                            :: d2fjldr2
  real(dp)                            :: d3fjldr3
  real(dp)                            :: fct
  real(dp)                            :: fik   
  real(dp)                            :: fjl
  real(dp)                            :: rc
  real(dp)                            :: ri
  real(dp)                            :: rj
  real(dp)                            :: rik
  real(dp)                            :: ril
  real(dp)                            :: rjk
  real(dp)                            :: rjl
  real(dp)                            :: rkl
  real(dp)                            :: rrik
  real(dp)                            :: rrjl
  real(dp)                            :: Tspline
  real(dp)                            :: Ttrm
  real(dp)                            :: T1d(6)
  real(dp)                            :: T2d(21)
  real(dp)                            :: dTdrni
  real(dp)                            :: dTdrnj
  real(dp)                            :: dTdrnc 
  real(dp)                            :: d2Tdrni2
  real(dp)                            :: d2Tdrnidrnj
  real(dp)                            :: d2Tdrnidrnc 
  real(dp)                            :: d2Tdrnj2
  real(dp)                            :: d2Tdrnjdrnc
  real(dp)                            :: d2Tdrnc2
  real(dp)                            :: xki
  real(dp)                            :: yki
  real(dp)                            :: zki
  real(dp)                            :: xkj
  real(dp)                            :: ykj
  real(dp)                            :: zkj
  real(dp)                            :: xli
  real(dp)                            :: yli
  real(dp)                            :: zli
  real(dp)                            :: xlk
  real(dp)                            :: ylk
  real(dp)                            :: zlk
!
!  Find integers of rnh/rnc
!
  ni = int(rni + 1.0d-12)
  nj = int(rnj + 1.0d-12)
  nc = int(rnc + 1.0d-12) - 1
  ri = rni + 1.0_dp
  rj = rnj + 1.0_dp
  rc = rnc
!
!  If greater than maxima lower as appropriate
!
  ni = min(ni,3_i4)
  nj = min(nj,3_i4)
  nc = min(nc,2_i4)
  ri = min(ri,4.0_dp)
  rj = min(rj,4.0_dp)
  rc = min(rc,9.0_dp)
!     
!  Calculate value and derivatives according to spline
!     
  T = 0.0_dp   
  if (lgrad1) then
    dTdrni = 0.0_dp
    dTdrnj = 0.0_dp
    dTdrnc = 0.0_dp
    if (lgrad2) then
      d2Tdrni2 = 0.0_dp
      d2Tdrnj2 = 0.0_dp
      d2Tdrnc2 = 0.0_dp
      d2Tdrnidrnj = 0.0_dp
      d2Tdrnidrnc = 0.0_dp
      d2Tdrnjdrnc = 0.0_dp
    endif
  endif
!
!  Determine spline contribution and derivatives
!
  Tspline = 0.0_dp   
  do m = 1,64  
    Ttrm = cotorcubic(m,nj,ni,nc)*(ri**ptricubic(m,1))*(rj**ptricubic(m,2))*(rc**ptricubic(m,3))
    Tspline = Tspline + Ttrm
    if (lgrad1) then
      dTdrni = dTdrni + Ttrm*ptricubic(m,1)/ri
      dTdrnj = dTdrnj + Ttrm*ptricubic(m,2)/rj
      dTdrnc = dTdrnc + Ttrm*ptricubic(m,3)/rc
      if (lgrad2) then
        d2Tdrni2 = d2Tdrni2 + Ttrm*ptricubic(m,1)*(ptricubic(m,1) - 1.0_dp)/(ri*ri)
        d2Tdrnj2 = d2Tdrnj2 + Ttrm*ptricubic(m,2)*(ptricubic(m,2) - 1.0_dp)/(rj*rj)
        d2Tdrnc2 = d2Tdrnc2 + Ttrm*ptricubic(m,3)*(ptricubic(m,3) - 1.0_dp)/(rc*rc)
        d2Tdrnidrnj = d2Tdrnidrnj + Ttrm*ptricubic(m,1)*ptricubic(m,2)/(ri*rj)
        d2Tdrnidrnc = d2Tdrnidrnc + Ttrm*ptricubic(m,1)*ptricubic(m,3)/(ri*rc)
        d2Tdrnjdrnc = d2Tdrnjdrnc + Ttrm*ptricubic(m,2)*ptricubic(m,3)/(rj*rc)
      endif
    endif
  enddo
  if (abs(Tspline).gt.1.0d-10) then
!
!  Loop over neighbours of i .ne. j 
!
    do k = 1,nneigh(i)
      if (k.ne.nij) then
        rik = rneigh(k,i)
        xki = xneigh(k,i)
        yki = yneigh(k,i)
        zki = zneigh(k,i)
!
!  Calculate fik
!
        call ctaper(rik,bTR1(nREBObond(k,i)),bTR2(nREBObond(k,i)),fik,dfikdr,d2fikdr2,d3fikdr3,lgrad1,lgrad2,.false.)
        if (lgrad1) then
          rrik = 1.0_dp/rik
          dfikdr = rrik*dfikdr
          if (lgrad2) then
            d2fikdr2 = rrik*rrik*(d2fikdr2 - dfikdr)
          endif
        endif
!
!  Calculate rjk
!
        xkj = xneigh(k,i) - xji
        ykj = yneigh(k,i) - yji
        zkj = zneigh(k,i) - zji
        rjk = xkj*xkj + ykj*ykj + zkj*zkj
        rjk = sqrt(rjk)
!
!  Loop over neighbours of j .ne. i 
!
        do l = 1,nneigh(j)
          if (l.ne.nji) then
            rjl = rneigh(l,j)
!
!  Calculate fik
!
            call ctaper(rjl,bTR1(nREBObond(l,j)),bTR2(nREBObond(l,j)), &
              fjl,dfjldr,d2fjldr2,d3fjldr3,lgrad1,lgrad2,.false.)
            if (lgrad1) then
              rrjl = 1.0_dp/rjl
              dfjldr = rrjl*dfjldr
              if (lgrad2) then
                d2fjldr2 = rrjl*rrjl*(d2fjldr2 - dfjldr)
              endif
            endif
!
!  Calculate ril
!
            xli = xneigh(l,j) + xji
            yli = yneigh(l,j) + yji
            zli = zneigh(l,j) + zji
            ril = xli*xli + yli*yli + zli*zli
            ril = sqrt(ril)
!
!  Calculate rkl
!
            xlk = xli - xki
            ylk = yli - yki
            zlk = zli - zki
            rkl = xlk*xlk + ylk*ylk + zlk*zlk
            rkl = sqrt(rkl)
!
!  Calculate cos(phi) term and derivatives
!
            call btorsion(rik,rjk,rkl,rij,ril,rjl,Ttrm,T1d,T2d,lgrad1,lgrad2)
!
!  Add term to T
!
            T = T + 0.5_dp*Tspline*Ttrm*fik*fjl
!
!  Calculate derivatives
!
            if (lgrad1) then
!
!  Work out indices of distances
!
!  From perspective of i
!
              indi(1) = k
              if (nij.ge.k) then
                indi(2) = nneigh(i) + nij*(nij-1)/2 + k
              else
                indi(2) = nneigh(i) + k*(k-1)/2 + nij
              endif
              indi(3) = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nij - 1)*(maxn+1)*maxn + k*maxn + l
              indi(4) = nij
              indi(5) = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nij - 1)*(maxn+1)*maxn + l
              indi(6) = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nij - 1)*(maxn+1)*maxn + nij*maxn + l
!
!  From perspective of j
!
              indj(1) = nneigh(j) + nneigh(j)*(nneigh(j) + 1)/2 + (nji - 1)*(maxn+1)*maxn + nji*maxn + k
              indj(2) = nneigh(j) + nneigh(j)*(nneigh(j) + 1)/2 + (nji - 1)*(maxn+1)*maxn + k
              indj(3) = nneigh(j) + nneigh(j)*(nneigh(j) + 1)/2 + (nji - 1)*(maxn+1)*maxn + l*maxn + k
              indj(4) = nji
              if (nji.ge.l) then
                indj(5) = nneigh(j) + nji*(nji-1)/2 + l
              else
                indj(5) = nneigh(j) + l*(l-1)/2 + nji
              endif
              indj(6) = l

!
!  Derivatives of fik/fjl
!
              d1trm = 0.5_dp*Tspline*Ttrm
              d1i(indi(1)) = d1i(indi(1)) + d1trm*dfikdr*fjl
              d1i(indi(6)) = d1i(indi(6)) + d1trm*fik*dfjldr
!
!  Derivatives of Ttrm
!
              d1trm = 0.5_dp*Tspline*fik*fjl
!
              do m = 1,6
                d1i(indi(m)) = d1i(indi(m)) + d1trm*T1d(m)
              enddo
!
!  Derivatives of Tspline
!
              d1trm = 0.5_dp*Ttrm*fik*fjl
              do m = 1,nneigh(i)
                if (m.ne.nij) then
                  d1i(m) = d1i(m) + d1trm*dTdrni*dNdr(m,i)
                endif
              enddo
              do m = 1,nneigh(j)
                if (m.ne.nji) then
                  d1j(m) = d1j(m) + d1trm*dTdrnj*dNdr(m,j)
                endif
              enddo
              d1trm = Ttrm*fik*fjl
              do m = 1,nneigh(i)
                d1i(m) = d1i(m) + d1trm*dTdrnc*rnci*drncidr(m)
              enddo
              do m = 1,nneigh(j)
                d1j(m) = d1j(m) + d1trm*dTdrnc*rncj*drncjdr(m)
              enddo
!
!  Second derivatives
!
              if (lgrad2) then
                d2trm = 0.5_dp*Tspline*Ttrm
                nn = indi(1)*(indi(1) + 1)/2
                d2i(nn) = d2i(nn) + d2trm*d2fikdr2*fjl
                nn = indi(6)*(indi(6) + 1)/2
                d2i(nn) = d2i(nn) + d2trm*fik*d2fjldr2
!
                d2trm = 0.5_dp*Tspline*Ttrm
                if (indi(1).ge.indi(6)) then
                  nn = indi(1)*(indi(1) - 1)/2 + indi(6)
                else
                  nn = indi(6)*(indi(6) - 1)/2 + indi(1)
                endif
                d2i(nn) = d2i(nn) + d2trm*dfikdr*dfjldr
!
                d2trm = 0.5_dp*Tspline*fik*fjl
                ii = 0
                do m = 1,6
                  do n = m,6
                    ii = ii + 1
                    if (indi(m).ge.indi(n)) then
                      nn = indi(m)*(indi(m) - 1)/2 + indi(n)
                    else
                      nn = indi(n)*(indi(n) - 1)/2 + indi(m)
                    endif
                    d2i(nn) = d2i(nn) + d2trm*T2d(ii)
                  enddo
                enddo
!
                do m = 1,6
                  if (indi(m).eq.indi(1)) then
                    nn = indi(m)*(indi(m) + 1)/2
                    d2trm = Tspline
                  elseif (indi(m).gt.indi(1)) then
                    nn = indi(m)*(indi(m) - 1)/2 + indi(1)
                    d2trm = 0.5_dp*Tspline
                  else
                    nn = indi(1)*(indi(1) - 1)/2 + indi(m)
                    d2trm = 0.5_dp*Tspline
                  endif
                  d2i(nn) = d2i(nn) + d2trm*T1d(m)*dfikdr*fjl
                  if (indi(m).eq.indi(6)) then
                    nn = indi(m)*(indi(m) + 1)/2
                    d2trm = Tspline
                  elseif (indi(m).gt.indi(6)) then
                    nn = indi(m)*(indi(m) - 1)/2 + indi(6)
                    d2trm = 0.5_dp*Tspline
                  else
                    nn = indi(6)*(indi(6) - 1)/2 + indi(m)
                    d2trm = 0.5_dp*Tspline
                  endif
                  d2i(nn) = d2i(nn) + d2trm*T1d(m)*fik*dfjldr
                enddo
!
                do m = 1,nneigh(i)
                  if (m.ne.nij) then
                    if (m.eq.indi(1)) then
                      nn = m*(m + 1)/2
                      d2trm = 1.0_dp
                    elseif (m.gt.indi(1)) then
                      nn = m*(m - 1)/2 + indi(1)
                      d2trm = 0.5_dp
                    else
                      nn = indi(1)*(indi(1) - 1)/2 + m
                      d2trm = 0.5_dp
                    endif
                    d2i(nn) = d2i(nn) + d2trm*dTdrni*dNdr(m,i)*Ttrm*dfikdr*fjl
                    if (m.eq.indi(6)) then
                      nn = m*(m + 1)/2
                      d2trm = 1.0_dp
                    elseif (m.gt.indi(6)) then
                      nn = m*(m - 1)/2 + indi(6)
                      d2trm = 0.5_dp
                    else
                      nn = indi(6)*(indi(6) - 1)/2 + m
                      d2trm = 0.5_dp
                    endif
                    d2i(nn) = d2i(nn) + d2trm*dTdrni*dNdr(m,i)*Ttrm*fik*dfjldr
                  endif
                enddo
                do m = 1,nneigh(j)
                  if (m.ne.nji) then
                    if (m.eq.indj(1)) then
                      nn = m*(m + 1)/2
                      d2trm = 1.0_dp
                    elseif (m.gt.indj(1)) then
                      nn = m*(m - 1)/2 + indj(1)
                      d2trm = 0.5_dp
                    else
                      nn = indj(1)*(indj(1) - 1)/2 + m
                      d2trm = 0.5_dp
                    endif
                    d2j(nn) = d2j(nn) + d2trm*dTdrnj*dNdr(m,j)*Ttrm*dfikdr*fjl
                    if (m.eq.indj(6)) then
                      nn = m*(m + 1)/2
                      d2trm = 1.0_dp
                    elseif (m.gt.indj(6)) then
                      nn = m*(m - 1)/2 + indj(6)
                      d2trm = 0.5_dp
                    else
                      nn = indj(6)*(indj(6) - 1)/2 + m
                      d2trm = 0.5_dp
                    endif
                    d2j(nn) = d2j(nn) + d2trm*dTdrnj*dNdr(m,j)*Ttrm*fik*dfjldr
                  endif
                enddo
!
                do m = 1,nneigh(i)
                  if (m.ne.nij) then
                    do n = 1,6
                      if (m.eq.indi(n)) then
                        nn = m*(m + 1)/2
                        d2trm = 1.0_dp
                      elseif (m.gt.indi(n)) then
                        nn = m*(m - 1)/2 + indi(n)
                        d2trm = 0.5_dp
                      else
                        nn = indi(n)*(indi(n) - 1)/2 + m
                        d2trm = 0.5_dp
                      endif
                      d2i(nn) = d2i(nn) + d2trm*dTdrni*dNdr(m,i)*T1d(n)*fik*fjl
                    enddo
                  endif
                enddo
!
                do m = 1,nneigh(j)
                  if (m.ne.nji) then
                    do n = 1,6
                      if (m.eq.indj(n)) then
                        nn = m*(m + 1)/2
                        d2trm = 1.0_dp
                      elseif (m.gt.indj(n)) then
                        nn = m*(m - 1)/2 + indj(n)
                        d2trm = 0.5_dp
                      else
                        nn = indj(n)*(indj(n) - 1)/2 + m
                        d2trm = 0.5_dp
                      endif
                      d2j(nn) = d2j(nn) + d2trm*dTdrnj*dNdr(m,j)*T1d(n)*fik*fjl
                    enddo
                  endif
                enddo
!
                d2trm = Ttrm*dTdrnc
                do m = 1,nneigh(i)
                  if (m.eq.indi(1)) then
                    nn = m*(m + 1)/2
                    fct = 2.0_dp
                  elseif (m.gt.indi(1)) then
                    nn = m*(m - 1)/2 + indi(1)
                    fct = 1.0_dp
                  else
                    nn = indi(1)*(indi(1) - 1)/2 + m
                    fct = 1.0_dp
                  endif
                  d2i(nn) = d2i(nn) + d2trm*rnci*drncidr(m)*dfikdr*fjl*fct
                  if (m.eq.indi(6)) then
                    nn = m*(m + 1)/2
                    fct = 2.0_dp
                  elseif (m.gt.indi(6)) then
                    nn = m*(m - 1)/2 + indi(6)
                    fct = 1.0_dp
                  else
                    nn = indi(6)*(indi(6) - 1)/2 + m
                    fct = 1.0_dp
                  endif
                  d2i(nn) = d2i(nn) + d2trm*rnci*drncidr(m)*fik*dfjldr*fct
                enddo
                do m = 1,nneigh(j)
                  if (m.eq.indj(1)) then
                    nn = m*(m + 1)/2
                    fct = 2.0_dp
                  elseif (m.gt.indj(1)) then
                    nn = m*(m - 1)/2 + indj(1)
                    fct = 1.0_dp
                  else
                    nn = indj(1)*(indj(1) - 1)/2 + m
                    fct = 1.0_dp
                  endif
                  d2j(nn) = d2j(nn) + d2trm*rncj*drncjdr(m)*dfikdr*fjl*fct
                  if (m.eq.indj(6)) then
                    nn = m*(m + 1)/2
                    fct = 2.0_dp
                  elseif (m.gt.indj(6)) then
                    nn = m*(m - 1)/2 + indj(6)
                    fct = 1.0_dp
                  else
                    nn = indj(6)*(indj(6) - 1)/2 + m
                    fct = 1.0_dp
                  endif
                  d2j(nn) = d2j(nn) + d2trm*rncj*drncjdr(m)*fik*dfjldr*fct
                enddo
!
                d2trm = dTdrnc*fik*fjl
                do m = 1,nneigh(i)
                  do n = 1,6
                    if (m.eq.indi(n)) then
                      nn = m*(m + 1)/2
                      fct = 2.0_dp
                    elseif (m.gt.indi(n)) then
                      nn = m*(m - 1)/2 + indi(n)
                      fct = 1.0_dp
                    else   
                      nn = indi(n)*(indi(n) - 1)/2 + m
                      fct = 1.0_dp
                    endif
                    d2i(nn) = d2i(nn) + d2trm*rnci*drncidr(m)*T1d(n)*fct
                  enddo
                enddo
                do m = 1,nneigh(j)
                  do n = 1,6
                    if (m.eq.indj(n)) then
                      nn = m*(m + 1)/2
                      fct = 2.0_dp
                    elseif (m.gt.indj(n)) then
                      nn = m*(m - 1)/2 + indj(n)
                      fct = 1.0_dp
                    else   
                      nn = indj(n)*(indj(n) - 1)/2 + m
                      fct = 1.0_dp
                    endif
                    d2j(nn) = d2j(nn) + d2trm*rncj*drncjdr(m)*T1d(n)*fct
                  enddo
                enddo
!
                d2trm = 0.5_dp*Ttrm*fik*fjl
                do m = 1,nneigh(i)
                  if (m.ne.nij) then
                    nn = m*(m + 1)/2
                    d2i(nn) = d2i(nn) + d2trm*dTdrni*d2Ndr2(m,i)
                    do n = 1,m
                      if (n.ne.nij) then
                        nn = m*(m - 1)/2 + n
                        d2i(nn) = d2i(nn) + d2trm*d2Tdrni2*dNdr(m,i)*dNdr(n,i)
                      endif
                    enddo
                  endif
                enddo
                do m = 1,nneigh(j)
                  if (m.ne.nji) then
                    nn = m*(m + 1)/2
                    d2j(nn) = d2j(nn) + d2trm*dTdrnj*d2Ndr2(m,j)
                    do n = 1,m
                      if (n.ne.nji) then
                        nn = m*(m - 1)/2 + n
                        d2j(nn) = d2j(nn) + d2trm*d2Tdrnj2*dNdr(m,j)*dNdr(n,j)
                      endif
                    enddo
                  endif
                enddo
!
                d2trm = (dTdrnc + 2.0_dp*rnci*rnci*d2Tdrnc2)*Ttrm*fik*fjl
                do m = 1,nneigh(i)
                  do n = 1,m
                    nn = m*(m - 1)/2 + n
                    d2i(nn) = d2i(nn) + d2trm*drncidr(n)*drncidr(m)
                  enddo
                enddo
                d2trm = (dTdrnc + 2.0_dp*rncj*rncj*d2Tdrnc2)*Ttrm*fik*fjl
                do m = 1,nneigh(j)
                  do n = 1,m
                    nn = m*(m - 1)/2 + n
                    d2j(nn) = d2j(nn) + d2trm*drncjdr(n)*drncjdr(m)
                  enddo
                enddo
!
                d2trm = dTdrnc*Ttrm*fik*fjl
                nn = 0
                do m = 1,nneigh(i)
                  do n = 1,m
                    nn = nn + 1
                    d2i(nn) = d2i(nn) + d2trm*rnci*d2rncidr2(nn)
                  enddo
                enddo
                nn = 0
                do m = 1,nneigh(j)
                  do n = 1,m
                    nn = nn + 1
                    d2j(nn) = d2j(nn) + d2trm*rncj*d2rncjdr2(nn)
                  enddo
                enddo
!
                d2trm = 0.5_dp*d2Tdrnidrnj*Ttrm*fik*fjl
                do m = 1,nneigh(i)
                  if (m.ne.nij) then
                    do n = 1,nneigh(j)
                      if (n.ne.nji) then
                        nn1 = nneigh(i) + nneigh(i)*(nneigh(i)+1)/2 &
                                        + (nij - 1)*(maxn+1)*maxn &
                                        + nij*maxn + n
                        nn = nn1*(nn1 - 1)/2 + m
                        d2i(nn) = d2i(nn) + d2trm*dNdr(m,i)*dNdr(n,j)
                      endif
                    enddo
                  endif
                enddo
!
                d2trm = 2.0_dp*d2Tdrnc2*rnci*rncj*Ttrm*fik*fjl
                do m = 1,nneigh(i)
                  do n = 1,nneigh(j)
                    nn1 = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 &
                                    + (nij - 1)*(maxn+1)*maxn &
                                    + nij*maxn + n
                    nn = nn1*(nn1 - 1)/2 + m
                    d2i(nn) = d2i(nn) + d2trm*drncidr(m)*drncjdr(n)
                  enddo
                enddo
!
                d2trm = 2.0_dp*d2Tdrnidrnc*rnci*Ttrm*fik*fjl
                do m = 1,nneigh(i)
                  if (m.ne.nij) then
                    do n = 1,m
                      nn = m*(m - 1)/2 + n
                      d2i(nn) = d2i(nn) + d2trm*dNdr(m,i)*drncidr(n)
                    enddo
                  endif
                enddo
                d2trm = 2.0_dp*d2Tdrnjdrnc*rncj*Ttrm*fik*fjl
                do m = 1,nneigh(j)
                  if (m.ne.nji) then
                    do n = 1,m
                      nn = m*(m - 1)/2 + n
                      d2j(nn) = d2j(nn) + d2trm*dNdr(m,j)*drncjdr(n)
                    enddo
                  endif
                enddo
!
                d2trm = d2Tdrnidrnc*rncj*Ttrm*fik*fjl
                do m = 1,nneigh(i)
                  if (m.ne.nij) then
                    do n = 1,nneigh(j)
                      nn1 = nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 &
                                      + (nij - 1)*(maxn+1)*maxn &
                                      + nij*maxn + n
                      nn = nn1*(nn1 - 1)/2 + m
                      d2i(nn) = d2i(nn) + d2trm*dNdr(m,i)*drncjdr(n)
                    enddo
                  endif
                enddo
                d2trm = d2Tdrnjdrnc*rnci*Ttrm*fik*fjl
                do m = 1,nneigh(j)
                  if (m.ne.nji) then
                    do n = 1,nneigh(i)
                      nn1 = nneigh(j) + nneigh(j)*(nneigh(j) + 1)/2 &
                                      + (nji - 1)*(maxn+1)*maxn &
                                      + nji*maxn + n
                      nn = nn1*(nn1 - 1)/2 + m
                      d2j(nn) = d2j(nn) + d2trm*dNdr(m,j)*drncidr(n)
                    enddo
                  endif
                enddo
              endif
            endif
!
          endif
        enddo
      endif
    enddo
  endif
!
  return
  end
!
  subroutine btorsion(r12,r13,r14,r23,r24,r34,T,T1d,T2d,lgrad1,lgrad2)
!
!  Calculates the cos(phi) term for the Brenner potential
!
!  r12     = distance between atoms 1 and 2
!  r13     = distance between atoms 1 and 3
!  r14     = distance between atoms 1 and 4
!  r23     = distance between atoms 2 and 3
!  r24     = distance between atoms 2 and 4
!  r34     = distance between atoms 3 and 4
!  T       = function of cosphi
!  T1d     = array of first derivative terms
!  T2d     = array of second derivative terms
!  lgrad1  = if .true. calculate the first derivatives
!  lgrad2  = if .true. calculate the second derivatives
!
!   6/02 Created from fourbody
!  12/07 Unused variables cleaned up
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
!  Copyright Curtin University 2007
!     
!  Julian Gale, NRI, Curtin University, December 2007
!
  use datatypes
  implicit none
!
!  Passed variables
!
  real(dp), intent(in)                :: r12
  real(dp), intent(in)                :: r13
  real(dp), intent(in)                :: r14
  real(dp), intent(in)                :: r23
  real(dp), intent(in)                :: r24
  real(dp), intent(in)                :: r34
  real(dp), intent(out)               :: T
  real(dp), intent(out)               :: T1d(6)
  real(dp), intent(out)               :: T2d(21)
  logical,  intent(in)                :: lgrad1
  logical,  intent(in)                :: lgrad2
!
!  Local variables
!
  integer(i4)                         :: i
  integer(i4)                         :: ii
  integer(i4)                         :: j
  real(dp)                            :: bot0
  real(dp)                            :: bot1(6)
  real(dp)                            :: bot2(21)
  real(dp)                            :: cos1
  real(dp)                            :: cos2
  real(dp)                            :: cos3
  real(dp)                            :: cosphi
  real(dp)                            :: cos11d(6)
  real(dp)                            :: cos31d(6)
  real(dp)                            :: cos12d(21)
  real(dp)                            :: cos32d(21)
  real(dp)                            :: cosp1d(6)
  real(dp)                            :: cosp2d(21)
  real(dp)                            :: d1
  real(dp)                            :: d2
  real(dp)                            :: r122
  real(dp)                            :: r132
  real(dp)                            :: r142
  real(dp)                            :: r232
  real(dp)                            :: r242
  real(dp)                            :: r342
  real(dp)                            :: rr12
  real(dp)                            :: rr23
  real(dp)                            :: rr24
  real(dp)                            :: rr122
  real(dp)                            :: rr232
  real(dp)                            :: rr242
  real(dp)                            :: rr124
  real(dp)                            :: rr234
  real(dp)                            :: rr244
  real(dp)                            :: rsin1
  real(dp)                            :: rsin3
  real(dp)                            :: rtan1
  real(dp)                            :: rtan3
  real(dp)                            :: sin1
  real(dp)                            :: sin3
  real(dp)                            :: sin11d(6)
  real(dp)                            :: sin31d(6)
  real(dp)                            :: sin12d(21)
  real(dp)                            :: sin32d(21)
  real(dp)                            :: top0
  real(dp)                            :: top1(6)
  real(dp)                            :: top2(21)
!
!  Zero terms
!
  T = 0.0_dp
  if (lgrad1) then
    do i = 1,6
      T1d(i) = 0.0_dp
      top1(i) = 0.0_dp
      bot1(i) = 0.0_dp
      cos11d(i) = 0.0_dp
      cos31d(i) = 0.0_dp
      cosp1d(i) = 0.0_dp
      sin11d(i) = 0.0_dp
      sin31d(i) = 0.0_dp
    enddo
    if (lgrad2) then
      do i = 1,21
        T2d(i) = 0.0_dp
        top2(i) = 0.0_dp
        bot2(i) = 0.0_dp
        cos12d(i) = 0.0_dp
        cos32d(i) = 0.0_dp
        cosp2d(i) = 0.0_dp
        sin12d(i) = 0.0_dp
        sin32d(i) = 0.0_dp
      enddo
    endif
  endif
!
!  Set up local constants
!
  r122 = r12*r12
  r132 = r13*r13
  r142 = r14*r14
  r232 = r23*r23
  r242 = r24*r24
  r342 = r34*r34
  rr12 = 1.0_dp/r12
  rr23 = 1.0_dp/r23
  rr24 = 1.0_dp/r24
  rr122 = rr12*rr12
  rr232 = rr23*rr23
  rr242 = rr24*rr24
  if (lgrad2) then
    rr124 = rr122*rr122
    rr234 = rr232*rr232
    rr244 = rr242*rr242
  endif
!$$$$$$$$$$$$$$$$$
!  Cosine terms  $
!$$$$$$$$$$$$$$$$$
!
!  Cosine theta 1 = 1-2-3, 2 = 2-3-4 and 3 = 3-2-4
!
  cos1 = 0.5_dp*(r232 + r122 - r132)/(r12*r23)
  cos2 = 0.5_dp*(r232 + r342 - r242)/(r23*r34)
  cos3 = 0.5_dp*(r232 + r242 - r342)/(r23*r24)
!
!  Check for angles which are 0 or 180 degrees. If there are
!  any present return leaving function as zero.
!
  if (abs(cos1).ge.0.99999999_dp) return
  if (abs(cos2).ge.0.99999999_dp) return
  if (lgrad1) then
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r14
!  4 = r23
!  5 = r24
!  6 = r34
!
    cos11d(1) = rr12*rr23 - cos1*rr122
    cos11d(2) = -rr12*rr23
    cos11d(4) = rr12*rr23 - cos1*rr232
!
    cos31d(4) = rr23*rr24 - cos3*rr232
    cos31d(5) = rr23*rr24 - cos3*rr242
    cos31d(6) = -rr23*rr24
    if (lgrad2) then
!
!  Second
!
!  1 = 11  7 = 22 12 = 33 16 = 44 19 = 55 21 = 66
!  2 = 21  8 = 32 13 = 43 17 = 54 20 = 65
!  3 = 31  9 = 42 14 = 53 18 = 64
!  4 = 41 10 = 52 15 = 63
!  5 = 51 11 = 62
!  6 = 61 
!
      cos12d(1) = -2.0_dp*rr122*rr12*rr23+3.0_dp*cos1*rr124
      cos12d(2) = rr122*rr12*rr23
      cos12d(4) = rr12*rr23*(cos1*rr12*rr23-rr122-rr232)
      cos12d(9) = rr232*rr23*rr12
      cos12d(16) = -2.0_dp*rr232*rr23*rr12+3.0_dp*cos1*rr234
!
      cos32d(16) = -2.0_dp*rr232*rr23*rr24+3.0_dp*cos3*rr234
      cos32d(17) = rr23*rr24*(cos3*rr23*rr24-rr232-rr242)
      cos32d(18) = rr232*rr23*rr24
      cos32d(19) = -2.0_dp*rr242*rr24*rr23+3.0_dp*cos3*rr244
      cos32d(20) = rr242*rr24*rr23
    endif
  endif
!$$$$$$$$$$$$$$$
!  Sine terms  $
!$$$$$$$$$$$$$$$
  sin1 = sqrt(1.0_dp - cos1*cos1)
  sin3 = sqrt(1.0_dp - cos3*cos3)
  rsin1 = 1.0_dp/sin1
  rsin3 = 1.0_dp/sin3
  if (lgrad1) then
!
!  First derivatives
!
    rtan1 = cos1*rsin1
    rtan3 = cos3*rsin3
    do i = 1,6
      sin11d(i) = - rtan1*cos11d(i)
      sin31d(i) = - rtan3*cos31d(i)
    enddo
    if (lgrad2) then
!
!  Second derivatives
!
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          sin12d(ii) = - rtan1*cos12d(ii) - rsin1*cos11d(i)*cos11d(j)*(1.0_dp+rtan1*rtan1)
          sin32d(ii) = - rtan3*cos32d(ii) - rsin3*cos31d(i)*cos31d(j)*(1.0_dp+rtan3*rtan3)
        enddo
      enddo
    endif
  endif
!$$$$$$$$$$$$$$
!  Phi terms  $
!$$$$$$$$$$$$$$
  top0 = r122 + r242 - r142 - 2.0_dp*r12*r24*cos1*cos3
  bot0 = rr12*rr24*rsin1*rsin3
  cosphi = 0.5_dp*top0*bot0
  if (abs(cosphi).gt.1.0_dp) cosphi = sign(1.0_dp,cosphi)
  if (lgrad1) then
!
!  First
!
!  1 = r12
!  2 = r13
!  3 = r14
!  4 = r23
!  5 = r24
!  6 = r34
!
    top1(1) = 2.0_dp - 2.0_dp*r24*cos1*cos3*rr12
    top1(3) = - 2.0_dp
    top1(5) = 2.0_dp - 2.0_dp*r12*cos1*cos3*rr24
    do i = 1,6
      top1(i) = top1(i) - 2.0_dp*r12*r24*(cos11d(i)*cos3 + cos1*cos31d(i))
    enddo
    bot1(1) = r24*sin1*sin3*rr12
    bot1(5) = r12*sin1*sin3*rr24
    do i = 1,6
      bot1(i) = bot1(i) + r12*r24*(sin11d(i)*sin3+sin1*sin31d(i))
    enddo
!
!  Combine derivatives
!
    do i = 1,6
      cosp1d(i) = 0.5_dp*bot0*(top1(i) - bot0*top0*bot1(i))
    enddo
    if (lgrad2) then
!
!  Second
!
!  1 = 11  7 = 22 12 = 33 16 = 44 19 = 55 21 = 66
!  2 = 21  8 = 32 13 = 43 17 = 54 20 = 65
!  3 = 31  9 = 42 14 = 53 18 = 64
!  4 = 41 10 = 52 15 = 63
!  5 = 51 11 = 62
!  6 = 61 
!
!
!  Top / bottom part of cosphi derivatives 
!
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          top2(ii) = top2(ii) - 2.0_dp*r12*r24*(cos12d(ii)*cos3+cos1*cos32d(ii))
          top2(ii) = top2(ii) - 2.0_dp*r12*r24*(cos11d(i)*cos31d(j)+cos11d(j)*cos31d(i))
          if (i.eq.1) then
            top2(ii) = top2(ii) - 2.0_dp*r24*rr12*(cos1*cos31d(j)+cos3*cos11d(j))
            if (j.eq.1) then
              top2(ii) = top2(ii) + 2.0_dp*cos1*cos3*r24*rr122*rr12
            endif
          elseif (i.eq.5) then
            top2(ii) = top2(ii) - 2.0_dp*r12*rr24*(cos1*cos31d(j)+cos3*cos11d(j))
            if (j.eq.5) then
              top2(ii) = top2(ii) + 2.0_dp*cos1*cos3*r12*rr242*rr24
            elseif (j.eq.1) then
              top2(ii) = top2(ii) - 2.0_dp*cos1*cos3*rr24*rr12
            endif
          endif
          if (j.eq.1) then
            top2(ii) = top2(ii) - 2.0_dp*r24*rr12*(cos1*cos31d(i)+cos3*cos11d(i))
          elseif (j.eq.5) then
            top2(ii) = top2(ii) - 2.0_dp*r12*rr24*(cos1*cos31d(i)+cos3*cos11d(i))
            if (i.eq.1) then
              top2(ii) = top2(ii) - 2.0_dp*cos1*cos3*rr24*rr12
            endif
          endif
        enddo
      enddo
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii+1
          bot2(ii) = bot2(ii) + r12*r24*(sin12d(ii)*sin3+sin1*sin32d(ii))
          bot2(ii) = bot2(ii) + r12*r24*(sin11d(i)*sin31d(j)+sin11d(j)*sin31d(i))
          if (i.eq.1) then
            bot2(ii) = bot2(ii) + r24*rr12*(sin1*sin31d(j)+sin3*sin11d(j))
            if (j.eq.1) then
              bot2(ii) = bot2(ii) - sin1*sin3*r24*rr122*rr12
            endif
          elseif (i.eq.5) then
            bot2(ii) = bot2(ii) + r12*rr24*(sin1*sin31d(j)+sin3*sin11d(j))
            if (j.eq.5) then
              bot2(ii) = bot2(ii) - sin1*sin3*r12*rr242*rr24
            elseif (j.eq.1) then
              bot2(ii) = bot2(ii) + sin1*sin3*rr24*rr12
            endif
          endif
          if (j.eq.1) then
            bot2(ii) = bot2(ii) + r24*rr12*(sin1*sin31d(i) + sin3*sin11d(i))
          elseif (j.eq.5) then
            bot2(ii) = bot2(ii) + r12*rr24*(sin1*sin31d(i) + sin3*sin11d(i))
            if (i.eq.1) then
              bot2(ii) = bot2(ii) + sin1*sin3*rr24*rr12
            endif
          endif
        enddo
      enddo
!
!  Combine derivatives
!
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          cosp2d(ii) = 0.5_dp*bot0*bot0*(2.0_dp*top0*bot0*bot1(j)*bot1(i)-top1(j)*bot1(i)-top1(i)*bot1(j))
        enddo
      enddo
      do i = 1,21
        cosp2d(i) = cosp2d(i) + 0.5_dp*bot0*(top2(i)-bot0*top0*bot2(i))
      enddo
    endif
  endif
!******************
!  Combine terms  *
!******************
  T = (1.0_dp - cosphi**2)
  if (lgrad1) then
!
!  First derivatives of energy
!
    d1 = - 2.0_dp*cosphi
    do i = 1,6
      T1d(i) = d1*cosp1d(i)
    enddo
    if (lgrad2) then
!
!  Second derivatives of energy
!
      d2 = - 2.0_dp
      ii = 0
      do i = 1,6
        do j = i,6
          ii = ii + 1
          T2d(ii) = d2*cosp1d(i)*cosp1d(j) + d1*cosp2d(ii)
        enddo
      enddo
    endif
  endif
  return
  end
