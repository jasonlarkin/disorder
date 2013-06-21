  subroutine real2aef(xcl,ycl,zcl,qli,oci,nmi,indmi,npsi,d1xe,d1ye,d1ze)
!
!  Calculate electrostatic force on region 2a ion i due to
!  defects in region 1.
!
!  i   = ion in region 2a
!  zcl = z coordinate of i
!  qli = charge of i
!  oci = site occupancy of i
!  nmi = molecule number of i
!  indmi= molecule cell index number of i
!  npsi = perfect lattice site of i
!
!  d1xe,d1ye,d1ze = electrostatic only gradients on i
!
!   3/95 Corrections added for periodic molecules
!   1/99 1-4 interaction scaling added
!   2/07 Bonding types added
!  11/07 Unused variables cleaned up
!  11/07 Modified to handle case where nmj is uninitialised
!   3/13 Meaning of nreldef value changed
!   3/13 ndptr replaced with ndintptr and ndvacptr
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, March 2013
!
  use constants
  use current
  use defects
  use molecule
  use region2a
  implicit none
!
!  Passed variables
!
  integer(i4)        :: ind
  integer(i4)        :: indmi
  integer(i4)        :: indmj
  integer(i4)        :: indmmi
  integer(i4)        :: indmmj
  integer(i4)        :: ixx
  integer(i4)        :: iyy
  integer(i4)        :: izz
  integer(i4)        :: ixx2
  integer(i4)        :: iyy2
  integer(i4)        :: izz2
  integer(i4)        :: jxx
  integer(i4)        :: jyy
  integer(i4)        :: jzz
  integer(i4)        :: jxx2
  integer(i4)        :: jyy2
  integer(i4)        :: jzz2
  integer(i4)        :: nmi
  integer(i4)        :: nmj
  integer(i4)        :: npsi
  integer(i4)        :: npsj
  real(dp)           :: d1xe
  real(dp)           :: d1ye
  real(dp)           :: d1ze
  real(dp)           :: derive
  real(dp)           :: factor
  real(dp)           :: oci
  real(dp)           :: qli
  real(dp)           :: r
  real(dp)           :: r2
  real(dp)           :: xcl
  real(dp)           :: ycl
  real(dp)           :: zcl
  real(dp)           :: xcrd
  real(dp)           :: ycrd
  real(dp)           :: zcrd
!
!  Local variables
!
  integer(i4)        :: nloop1
  integer(i4)        :: j
  integer(i4)        :: jj
  integer(i4)        :: nbtypeij
  integer(i4)        :: nbtypeij2
  logical            :: l2bonds
  logical            :: l3bonds
  logical            :: lbonded
  logical            :: ldoit
  logical            :: lvalid
  real(dp)           :: ocj
  real(dp)           :: qlj
  real(dp)           :: rsgn
  real(dp)           :: xal
  real(dp)           :: yal
  real(dp)           :: zal
!
  nloop1 = nvaca + ninte
  do j = 1,nloop1
    if (j.gt.nvaca) then
      jj = ndintptr(j-nvaca)
      xal = xdefe(jj)
      yal = ydefe(jj)
      zal = zdefe(jj)
      qlj = qdefe(jj)
      ocj = occdefe(jj)
      if (nreldef(jj).gt.0) then
        npsj = nreldef(jj)
      else
        npsj = 0
      endif
      rsgn = 1.0_dp
    else
      jj = ndvacptr(j)
      xal = xperf(jj)
      yal = yperf(jj)
      zal = zperf(jj)
      qlj = qp(jj)
      ocj = occp(jj)
      npsj = npsite(jj)
      rsgn =  - 1.0_dp
    endif
!
!  Molecule handling
!
    if (lmol) then
      if (j.gt.nvaca) then
        nmj = ndefmol(jj)
        indmj = ndefind(jj)
      else
        nmj = ndefmolp(jj)
        indmj = ndefindp(jj)
      endif
    endif
!
!  Molecule and bonding checks
!
    if (lmol) then
      if (nmi.ne.0.and.nmi.eq.nmj) then
        ind = indmj - indmi
        lvalid = (ind.eq.0)
        if (.not.lvalid) then
          call mindtoijk(indmj,jxx,jyy,jzz)
          call mindtoijk(indmi,ixx,iyy,izz)
          jxx = jxx - ixx
          jyy = jyy - iyy
          jzz = jzz - izz
          call samemol(lvalid,nmi,jxx,jyy,jzz,0_i4,0_i4,0_i4)
        endif
        if (lvalid) then
          if (npsj.gt.0) then
            call mindtoijk(indmj,jxx,jyy,jzz)
            call mindtoijk(indmi,ixx,iyy,izz)
            jxx = jxx - ixx
            jyy = jyy - iyy
            jzz = jzz - izz
            indmmj = nmolind(npsj)
            indmmi = nmolind(npsi)
            call mindtoijk(indmmj,jxx2,jyy2,jzz2)
            call mindtoijk(indmmi,ixx2,iyy2,izz2)
            jxx = jxx + jxx2 - ixx2
            jyy = jyy + jyy2 - iyy2
            jzz = jzz + jzz2 - izz2
            call bonded3(lbonded,l2bonds,l3bonds,nbtypeij,nbtypeij2,npsi,npsj,jxx,jyy,jzz)
          else
            lbonded   = .false.
            l2bonds   = .false.
            l3bonds   = .false.
            nbtypeij  = 0
            nbtypeij2 = 0
          endif
        else
          lbonded   = .false.
          l2bonds   = .false.
          l3bonds   = .false.
          nbtypeij  = 0
          nbtypeij2 = 0
        endif
      else
        lvalid    = .false.
        lbonded   = .false.
        l2bonds   = .false.
        l3bonds   = .false.
        nbtypeij  = 0
        nbtypeij2 = 0
      endif
    else
      lvalid    = .false.
      lbonded   = .false.
      l2bonds   = .false.
      l3bonds   = .false.
      nbtypeij  = 0
      nbtypeij2 = 0
    endif
    if (lmolq) then
      ldoit = (.not.lvalid)
    elseif (lmolmec) then
      ldoit = (.not.lvalid.or.(.not.lbonded.and..not.l2bonds))
    else
      ldoit = .true.
    endif
    if (ldoit) then
!
!  First derivatives
!
      xcrd = xal - xcl
      ycrd = yal - ycl
      zcrd = zal - zcl
      r2 = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
      r = sqrt(r2)*r2
      derive =  - rsgn*qlj*ocj/r
      d1xe = d1xe - derive*xcrd
      d1ye = d1ye - derive*ycrd
      d1ze = d1ze - derive*zcrd
    endif
  enddo
!
!  Scale by factor
!
  factor = qli*oci*angstoev
  d1xe = d1xe*factor
  d1ye = d1ye*factor
  d1ze = d1ze*factor
!
  return
  end
