  subroutine mcmolinsert(newmol)
!
!  MC routine for insertion of molecules. Assumes that
!  atoms have been already created in data structures
!  as given by the pointer nptrcreated.
!
!  ntrialatom    = number of new atoms in molecule - assumed to
!                  be the same as ngcmcmolat(newmol)
!  nptrtrialatom = pointer to where these atoms are in main arrays
!  newmol        = number of new molecule in gcmc molecule arrays
!
!   2/01 Created
!  11/04 Arguments ntrialatom and nptrtrialatom now in module
!  11/04 Pi accessed from module
!   1/08 random -> GULP_random
!  10/11 Call to cart2frac modified by adding cell indices
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, October 2011
!
  use constants, only : pi
  use current
  use element
  use general
  use genetic,   only : iseed
  use molecule
  use montecarlo
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4),   intent(in)                :: newmol
!
!  Local variables
!
  real(dp),                           save :: deltax = 0.0_dp
  real(dp),                           save :: deltay = 0.0_dp
  real(dp),                           save :: deltaz = 0.0_dp
!
  integer(i4)                              :: i
  integer(i4)                              :: icx
  integer(i4)                              :: icy
  integer(i4)                              :: icz
  integer(i4)                              :: ii
  real(dp)                                 :: cosp
  real(dp)                                 :: dx
  real(dp)                                 :: dy
  real(dp)                                 :: dz
  real(dp)                                 :: dxold
  real(dp)                                 :: dyold
  real(dp)                                 :: dzold
  real(dp)                                 :: GULP_random
  real(dp)                                 :: sinp
  real(dp)                                 :: xcrd
  real(dp)                                 :: ycrd
  real(dp)                                 :: zcrd
  real(dp)                                 :: xcent
  real(dp)                                 :: ycent
  real(dp)                                 :: zcent
  real(dp)                                 :: xmid
  real(dp)                                 :: ymid
  real(dp)                                 :: zmid
  real(dp)                                 :: xmidf
  real(dp)                                 :: ymidf
  real(dp)                                 :: zmidf
!
!  Check that ntrialatom = ngcmcmolat(newmol)
!
  if (ntrialatom.ne.ngcmcmolat(newmol)) then
    call outerror('inconsistency in mcmolinsert for ntrialatom',0_i4)
    call stopnow('mcmolinsert')
  endif
!
!  Check that newmol is valid
!
  if (newmol.lt.1.or.newmol.gt.maxgcmcmol) then
    call outerror('invalid value for newmol in mcmolinsert',0_i4)
    call stopnow('mcmolinsert')
  endif
!***********************************
!  Choose centre of mass position  *
!***********************************
!
!  First choose in fractional coordinates
!
  xmidf = GULP_random(iseed,1_i4)
  ymidf = GULP_random(iseed,1_i4)
  zmidf = GULP_random(iseed,1_i4)
!
!  Now convert to Cartesian
!
  xmid = xmidf*rv(1,1) + ymidf*rv(1,2) + zmidf*rv(1,3)
  ymid = xmidf*rv(2,1) + ymidf*rv(2,2) + zmidf*rv(2,3)
  zmid = xmidf*rv(3,1) + ymidf*rv(3,2) + zmidf*rv(3,3)
!***********************
!  Choose orientation  *
!***********************
  deltax = pi*GULP_random(iseed,2_i4)
  deltay = pi*GULP_random(iseed,2_i4)
  deltaz = pi*GULP_random(iseed,2_i4)
!
  xcent = 0.0_dp
  ycent = 0.0_dp
  zcent = 0.0_dp
  do i = 1,ngcmcmolat(newmol)
!
!  Add to running centre of mass totals
!
    xcrd = xgcmcmol(i,newmol)
    ycrd = ygcmcmol(i,newmol)
    zcrd = zgcmcmol(i,newmol)
    xcent = xcent + xcrd
    ycent = ycent + ycrd
    zcent = zcent + zcrd
  enddo
!
!  Find centre of mass
!
  xcent = xcent / dble(ngcmcmolat(newmol))
  ycent = ycent / dble(ngcmcmolat(newmol))
  zcent = zcent / dble(ngcmcmolat(newmol))
!
!  Apply rotations to atoms in Cartesian space
!
  do i = 1,ngcmcmolat(newmol)
    dx = xgcmcmol(i,newmol) - xcent
    dy = ygcmcmol(i,newmol) - ycent
    dz = zgcmcmol(i,newmol) - zcent
!
!  Rotate about X
!
    sinp = sin(deltax)
    cosp = cos(deltax)
    dyold = dy
    dzold = dz
    dy = dyold*cosp - dzold*sinp
    dz = dzold*cosp + dyold*sinp
!
!  Rotate about Y
!
    sinp = sin(deltay)
    cosp = cos(deltay)
    dxold = dx
    dzold = dz
    dx = dxold*cosp - dzold*sinp
    dz = dzold*cosp + dxold*sinp
!
!  Rotate about Z
!
    sinp = sin(deltaz)
    cosp = cos(deltaz)
    dxold = dx
    dyold = dy
    dx = dxold*cosp - dyold*sinp
    dy = dyold*cosp + dxold*sinp
!
!  Return to fractional space as appropriate
!
    ii = nptrtrialatom(i)
    call cart2frac(ndim,dx+xmid,dy+ymid,dz+zmid,rv,x0(3*ii+nstrains-2),x0(3*ii+nstrains-1),x0(3*ii+nstrains),icx,icy,icz)
  enddo
!*********************
!  Set up molecules  *
!*********************
  call mcmolconnect(ntrialatom,nptrtrialatom,newmol,.true.)
!
  return
  end
!
  subroutine mcmolconnect(ncreatedloc,nptrcreatedloc,newmol,lSetnmolind)
!
!  MC routine for handling of molecular connectivity after atoms have been created.
!
!  ncreated    = number of new atoms in molecule - assumed to
!                be the same as ngcmcmolat(newmol)
!  nptrcreated = pointer to where these atoms are in main arrays
!  newmol      = number of new molecule in gcmc molecule arrays
!  lSetnmolind = if .true. then initialise the value of nmolind, otherwise
!                leave untouched
!
!  10/02 Created from mcmolinsert
!   7/07 lgcmcmol added
!   1/08 Logical to control what happens to nmolind added
!   8/10 Number of atoms in molecule now set
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, August 2010
!
  use current
  use element
  use general
  use molecule
  use montecarlo
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4)                              :: ncreatedloc
  integer(i4)                              :: nptrcreatedloc(*)
  integer(i4)                              :: newmol
  logical,       intent(in)                :: lSetnmolind
!
!  Local variables
!
  integer(i4)                              :: i, ii, j
  integer(i4)                              :: iptr, jptr
  integer(i4)                              :: ni, ni1, nj, nj1
  integer(i4)                              :: nti, ntj, nti1, ntj1
  integer(i4)                              :: nb1, nb2, indb
  logical                                  :: lbondok
  real(dp)                                 :: ri, rj, rij, rcut
  real(dp)                                 :: xal, yal, zal
  real(dp)                                 :: xcrd, ycrd, zcrd
  real(dp)                                 :: xcd, ycd, zcd
!
!  Check that newmol is valid
!
  if (newmol.lt.1.or.newmol.gt.maxgcmcmol) then
    call outerror('invalid value for newmol in mcmolconnect',0_i4)
    call stopnow('mcmolinsert')
  endif
!
!  Set molecule information
!
  nmol = nmol + 1
  if (nmol.gt.maxmol) then
    maxmol = nmol
    call changemaxmol
  endif
  nmolatom(nmol) = ngcmcmolat(newmol)
  if (nmol.eq.1) then
    nmolptr(nmol) = 0
  else
    nmolptr(nmol) = nmolptr(nmol-1) + nmolatom(nmol-1)
  endif
  do i = 1,ncreatedloc
    ii = nptrcreatedloc(i)
    natmol(ii) = nmol
    nmollist(nmolptr(nmol)+i) = ii
    if (lSetnmolind) nmolind(ii) = 555
  enddo
  moldim(nmol) = 0
  moldimi(nmol) = 0
  molgcmc(nmol) = newmol
  lgcmcmol(nmol) = .true.
!***************************
!  Calculate connectivity  *
!***************************
  lbondok = .true.
!
!  Initialise array
!
  do i = 1,ncreatedloc
    ii = nptrcreatedloc(i)
    nbonds(ii) = 0
    do j = 1,maxbond
      nbonded(j,ii) = 0
    enddo
  enddo
!
!  Find bonds
!
  do i = 1,ncreatedloc
    iptr = nptrcreatedloc(i)
    xal = xgcmcmol(i,newmol)
    yal = ygcmcmol(i,newmol)
    zal = zgcmcmol(i,newmol)
    ni1 = ngcmcmolnat(i,newmol)
    nti = ngcmcmoltype(i,newmol)
    ni = ni1
    if (ni.gt.maxele) ni = ni - maxele
    ri = rcov(ni)
!
!  Find all atoms bonded to atom i
!
    if (ri.ne.0.0d0) then
      do j = 1,ncreatedloc
        jptr = nptrcreatedloc(j)
        xcd = xgcmcmol(j,newmol)
        ycd = ygcmcmol(j,newmol)
        zcd = zgcmcmol(j,newmol)
        nj1 = ngcmcmolnat(j,newmol)
        ntj = ngcmcmoltype(j,newmol)
        nj = nj1
        if (nj.gt.maxele) nj = nj - maxele
        rj = rcov(nj)
!
!  Check whether bond type is excluded
!
        if (nnobo.gt.0) then
          if (ni1.eq.nj1) then
            indb = nj1 + 1000*ni1
            if (nti.lt.ntj) then
              nti1 = nti
              ntj1 = ntj
            else
              nti1 = ntj
              ntj1 = nti
            endif
          elseif (ni1.lt.nj1) then
            indb = nj1 + 1000*ni1
            nti1 = nti
            ntj1 = ntj
          else
            indb = ni1 + 1000*nj1
            nti1 = ntj
            ntj1 = nti
          endif
          lbondok = .true.
          ii = 1
          do while (lbondok.and.(ii.le.nnobo))
            if (indb.eq.nobond(ii)) then
              nb1 = nobotyp(ii)/1000
              nb2 = nobotyp(ii) - 1000*nb1
              if ((nb1.eq.nti1.or.nb1.eq.0).and.(nb2.eq.ntj1.or.nb2.eq.0)) lbondok = .false.
              if (ni1.eq.nj1.and.lbondok) then
                if ((nb1.eq.ntj1.or.nb1.eq.0).and.(nb2.eq.nti1.or.nb2.eq.0)) lbondok = .false.
              endif
            endif
            ii = ii + 1
          enddo
        endif
!
!  Distance check
!
        if (rj.ne.0.0d0.and.lbondok) then
          rcut = rtol*(ri+rj)
          rcut = rcut*rcut
          if (rj.ne.0.0d0) then
            xcrd = xcd - xal
            ycrd = ycd - yal
            zcrd = zcd - zal
            rij = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
            if (rij.le.rcut.and.rij.gt.0.0d0.and.(i.ne.j)) then
!
!  Valid bond
!
              nbonds(iptr) = nbonds(iptr) + 1
              if (nbonds(iptr).gt.maxbond) then
                maxbond = nbonds(iptr) + 2
                call changemaxbond
              endif
              nbonded(nbonds(iptr),iptr) = jptr
              nbondind(nbonds(iptr),iptr) = 555
            endif
          endif
        endif
      enddo
    endif
  enddo
!
  return
  end
!
  subroutine mcmolinitial
!
!  MC routine for handling of molecule related initialisation
!
!  10/02 Created from mcmolinsert
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
!  Julian Gale, Curtin University, October 2004
!
  use current
  use element
  use general
  use molecule
  use montecarlo
  implicit none
!
!  Local variables
!
  integer(i4)                              :: i
!
!  Set molecule information - assume everything is of one type
!
  do i = 1,nmol
    molgcmc(i) = 1
  enddo
!
  return
  end
