  subroutine setmoldef12a
!
!  Determines number of molecules and their constituent atoms
!
!  10/96 Correction added for molmec when link atom is in region 2a
!   9/06 Modified to reflect new storage arrays for defect quantities
!   5/08 Defect bonding array structure changed
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
!  Julian Gale, NRI, Curtin University, May 2008
!
  use current
  use defects
  use element
  use molecule
  use parallel
  use region2a
  implicit none
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: indb
  integer(i4)        :: j
  integer(i4)        :: nb1
  integer(i4)        :: nb2
  integer(i4)        :: ni
  integer(i4)        :: ni1
  integer(i4)        :: nj
  integer(i4)        :: nj1
  integer(i4)        :: nti
  integer(i4)        :: nti1
  integer(i4)        :: ntj
  integer(i4)        :: ntj1
  logical            :: lbondok
  real(dp)           :: rcut
  real(dp)           :: ri
  real(dp)           :: rij
  real(dp)           :: rj
  real(dp)           :: xal
  real(dp)           :: yal
  real(dp)           :: zal
  real(dp)           :: xcd
  real(dp)           :: ycd
  real(dp)           :: zcd
  real(dp)           :: xcrd
  real(dp)           :: ycrd
  real(dp)           :: zcrd
!
  lbondok = .true.
!********************************************************
!  Check if interstitials belong to existing molecules  *
!********************************************************
  do i = 1,nreg1
    xal = xdefe(i)
    yal = ydefe(i)
    zal = zdefe(i)
    ni1 = natdefe(i)
    nti = ntypdefe(i)
    ni = ni1
    if (ni.gt.maxele) ni = ni - maxele
    ri = rcov(ni)
!
!  Find all atoms bonded to atom i
!
    if (ri.ne.0.0_dp) then
!******************************
!  Region 2a link atom check  *
!******************************
      do j = 1,ntreg2
        xcd = xr2a(j)
        ycd = yr2a(j)
        zcd = zr2a(j)
        nj1 = nr2a(j)
        ntj = ntr2a(j)
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
        if (rj.ne.0.0_dp.and.lbondok) then
          rcut = rtol*(ri+rj)
          rcut = rcut*rcut
          if (rj.ne.0.0_dp) then
            xcrd = xcd - xal
            ycrd = ycd - yal
            zcrd = zcd - zal
            rij = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
            if (rij.le.rcut.and.rij.gt.0.0_dp) then
!
!  Valid bond
!
              if (ndefmol(i).eq.0) then
                ndefmol(i) = nmr2a(j)
                ndefind(i) = nmir2a(j)
              endif
              nbondsdef(i) = nbondsdef(i) + 1
              if (nbondsdef(i).gt.maxbond) then
                maxbond = nbondsdef(i) + 2
                call changemaxbond
              endif
!
!  Add nreg1 to number to distinguish from region 1 atoms
!
              nbondeddef(nbondsdef(i),i) = j + nreg1
            endif
          endif
        endif
      enddo
    endif
  enddo
!
  return
  end
