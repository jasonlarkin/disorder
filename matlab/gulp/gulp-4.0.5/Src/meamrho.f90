  subroutine meamrho(nati,ntypi,natj,ntypj,r,rmax,x,y,z,rhoij,rhoji,drhoij,drhoji, &
                     drhoijs,drhojis,drhoij2,drhoji2,drhoij2s,drhoji2s,drhoij2m,drhoji2m, &
                     oci,ocj,lorder12,lstr,lgrad1,lgrad2)
!
!  Calculates the density function and its derivatives in the Embedded Atom Method. 
!  Assumes that derivatives have been pre-initialised by the calling routine.
!
!  On entry :
!
!  nati    = atomic number of i
!  ntypi   = type number of i
!  natj    = atomic number of j
!  ntypj   = type number of j
!  r       = distance from i to j (Angstroms)
!  rmax    = cutoff distance from i to j (Angstroms)
!  x       = Cartesian component of r in x direction from i->j (Angstroms)
!  y       = Cartesian component of r in y direction from i->j (Angstroms)
!  z       = Cartesian component of r in z direction from i->j (Angstroms)
!  oci     = occupancy of site i
!  ocj     = occupancy of site j
!  lorder12= if .true. then order of atoms is really i-j; if false then this is j-i
!  lstr    = if .true. then compute strain derivatives as well as Cartesian
!  lgrad2  = if .true. calculate first derivatives 
!  lgrad2  = if .true. and lgrad1, calculate second derivatives 
!
!  On exit :
!
!  rhoij    = density of j at i (if MEAM then this is the contributions for each order)
!  rhoji    = density of i at j (if MEAM then this is the contributions for each order)
!  drhoij   = Cartesian first derivatives of j density at i (if lgrad1 = true)
!  drhoji   = Cartesian first derivatives of i density at j (if lgrad1 = true)
!  drhoijs  = Strain first derivatives of j density at i (if lgrad1 = true)
!  drhojis  = Strain first derivatives of i density at j (if lgrad1 = true)
!  drhoij2  = Cartesian second derivatives of j density at i (if lgrad2 = true)
!  drhoji2  = Cartesian second derivatives of i density at j (if lgrad2 = true)
!  drhoij2s = Strain second derivatives of j density at i (if lgrad2 = true)
!  drhoji2s = Strain second derivatives of i density at j (if lgrad2 = true)
!  drhoij2m = Strain-Cartesian mixed second derivatives of j density at i (if lgrad2 = true)
!  drhoji2m = Strain-Cartesian mixed second derivatives of i density at j (if lgrad2 = true)
!
!  The MEAM components in rhoij/rhoji are numbered according to the following:
!
!  1 => order 0 r (standard EAM)
!  2 => order 1 x
!  3 => order 1 y
!  4 => order 1 z
!  5 => order 2 xx
!  6 => order 2 xy
!  7 => order 2 xz
!  8 => order 2 yy
!  9 => order 2 yz
! 10 => order 2 zz
! 11 => order 2 r
! 12 => order 3 xxx
! 13 => order 3 xxy
! 14 => order 3 xxz
! 15 => order 3 xyy
! 16 => order 3 xyz
! 17 => order 3 xzz
! 18 => order 3 yyy
! 19 => order 3 yyz
! 20 => order 3 yzz
! 21 => order 3 zzz
! 22 => order 3 x
! 23 => order 3 y
! 24 => order 3 z
!
!  10/99 Cubic density function added
!  11/03 ndennat/ndentyp replaced
!   7/05 Style updated
!   9/05 Voter form of density added
!  11/05 Tapering of density added
!   2/06 Quadratic and quartic densities added
!   3/06 Power law EAM densities truncated after r0
!   3/06 Modified to allow for density component number
!   4/06 Species specific density added
!   3/07 Glue density added
!   5/07 eVoter EAM density added
!  11/07 Mei-Davenport density added
!  11/07 MDF tapering of density added
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 Calculation of density added to this routine so that all 
!        terms are collected in a single place
!  11/08 Baskes form of exponential density added
!  11/08 x, y, z Cartesian components of r passed in for benefit of MEAM
!  11/08 Modified so that rho arguments are arrays rather than scalars to 
!        handle the needs of MEAM
!  11/08 rho arrays made 2-D to accommodate MEAM
!  11/08 rho*sum variables removed since terms for each order have to be kept separate
!  11/08 Created from rhoderv
!   1/09 x y z now divided by r
!   2/09 drho / drho2 / drho3 arrays now 2-D with right-hand dimenion being maxmeamcomponent
!   3/09 lorder12 added to list of arguments
!   4/09 Extended MEAM form with 24 density components added
!   5/09 Partial hooks for third derivatives cleaned up for now
!  10/11 Fractional power density added
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
  use eam
  use two, only : tapertype, tapermax, taperm, tapermin
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nati
  integer(i4), intent(in)    :: natj
  integer(i4), intent(in)    :: ntypi
  integer(i4), intent(in)    :: ntypj
  logical,     intent(in)    :: lgrad1
  logical,     intent(in)    :: lgrad2
  logical,     intent(in)    :: lorder12
  logical,     intent(in)    :: lstr
  real(dp),    intent(inout) :: rhoij(maxmeamcomponent)
  real(dp),    intent(inout) :: rhoji(maxmeamcomponent)
  real(dp),    intent(inout) :: drhoij(3,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoijs(6,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoij2(6,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoij2s(21,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoij2m(6,3,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoji(3,maxmeamcomponent)
  real(dp),    intent(inout) :: drhojis(6,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoji2(6,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoji2s(21,maxmeamcomponent)
  real(dp),    intent(inout) :: drhoji2m(6,3,maxmeamcomponent)
  real(dp),    intent(in)    :: oci
  real(dp),    intent(in)    :: ocj
  real(dp),    intent(in)    :: r
  real(dp),    intent(in)    :: rmax
  real(dp),    intent(in)    :: x
  real(dp),    intent(in)    :: y
  real(dp),    intent(in)    :: z
!
!  Local variables
!
  integer(i4)                :: ind
  integer(i4)                :: j
  integer(i4)                :: l
  integer(i4)                :: mm
  integer(i4)                :: nc1
  integer(i4)                :: nc2
  integer(i4)                :: nord
  integer(i4)                :: npt
  integer(i4)                :: ns1
  integer(i4)                :: ns2
  integer(i4)                :: order
  logical                    :: lt24             ! If true then use 24 term MEAM expression
  real(dp)                   :: apt
  real(dp)                   :: bpt
  real(dp)                   :: cpt
  real(dp)                   :: d1t
  real(dp)                   :: d2t
  real(dp)                   :: dr
  real(dp)                   :: dr2
  real(dp)                   :: dr3
  real(dp)                   :: dt
  real(dp)                   :: etrm
  real(dp)                   :: r2
  real(dp)                   :: rd
  real(dp)                   :: rd2
  real(dp)                   :: drhoijloc
  real(dp)                   :: drhoij2loc
  real(dp)                   :: drhojiloc
  real(dp)                   :: drhoji2loc
  real(dp)                   :: dxyzdc(3,3)      ! First derivatives of Cartesian components over r w.r.t. Cartesian components
  real(dp)                   :: dxyzds(6,3)      ! First derivatives of Cartesian components over r w.r.t. strains
  real(dp)                   :: dxyznords(6,3)   ! First derivatives of Cartesian components w.r.t. strains
  real(dp)                   :: d2xyzds2(21,3)   ! Second derivatives of Cartesian components over r w.r.t. two strains
  real(dp)                   :: d2xyzdsdc(6,3,3) ! Second derivatives of Cartesian components over r w.r.t. one strain and one Cartesian component
  real(dp)                   :: d2xyzdc2(6,3)    ! Second derivatives of Cartesian components over r w.r.t. two Cartesian components
  real(dp)                   :: d2sds2(21)       ! Derivatives of strain Cartesian components w.r.t. another strain / distance squared
  real(dp)                   :: detpfn
  real(dp)                   :: d2etpfn
  real(dp)                   :: d3etpfn
  real(dp)                   :: dtdc(3)          ! First derivatives of spherical density w.r.t. Cartesian components
  real(dp)                   :: dtds(6)          ! First derivatives of spherical density w.r.t. strains
  real(dp)                   :: d2tdc2(6)        ! Second derivatives of spherical density w.r.t. two Cartesian components
  real(dp)                   :: d2tds2(21)       ! Second derivatives of spherical density w.r.t. two strains
  real(dp)                   :: d2tdsdc(6,3)     ! Second derivatives of spherical density w.r.t. one Cartesian component and one strain
  real(dp)                   :: etpfn
  real(dp)                   :: ordtrm(24)
  real(dp)                   :: rhoijloc
  real(dp)                   :: rhojiloc
  real(dp)                   :: rk
  real(dp)                   :: rk2
  real(dp)                   :: rk3
  real(dp)                   :: rn1
  real(dp)                   :: rpd(6)
  real(dp)                   :: rpt
  real(dp)                   :: rr0
  real(dp)                   :: rr12
  real(dp)                   :: tpfn
  real(dp)                   :: dtpfn
  real(dp)                   :: d2tpfn
  real(dp)                   :: d3tpfn
  real(dp)                   :: term0
  real(dp)                   :: term1
  real(dp)                   :: term2
  real(dp)                   :: trm1
  real(dp)                   :: trm2
  real(dp)                   :: trm3
  real(dp)                   :: xl         !  Local copy of x or -x according to lorder12
  real(dp)                   :: yl         !  Local copy of y or -y according to lorder12
  real(dp)                   :: zl         !  Local copy of z or -z according to lorder12
  real(dp)                   :: xr         !  x/r
  real(dp)                   :: yr         !  y/r
  real(dp)                   :: zr         !  z/r
!
!  Calculate local variables
!
  r2 = r*r
  rk = 1.0_dp/r
  rk2 = rk*rk
  rk3 = rk2*rk
!
!  Create local copy of Cartesian coordinates with sign set according to lorder12
!
  if (lorder12) then
    xl = x
    yl = y
    zl = z
  else
    xl = - x
    yl = - y
    zl = - z
  endif
  xr = xl*rk
  yr = yl*rk
  zr = zl*rk
!
!  Set up array of prefactors for each order
!
  ordtrm(1)  = 1.0_dp
  ordtrm(2)  = xr
  ordtrm(3)  = yr
  ordtrm(4)  = zr
  ordtrm(5)  = xr*xr
  ordtrm(6)  = xr*yr
  ordtrm(7)  = xr*zr
  ordtrm(8)  = yr*yr
  ordtrm(9)  = yr*zr
  ordtrm(10) = zr*zr
  ordtrm(11) = 1.0_dp
  ordtrm(12) = xr*xr*xr
  ordtrm(13) = xr*xr*yr
  ordtrm(14) = xr*xr*zr
  ordtrm(15) = xr*yr*yr
  ordtrm(16) = xr*yr*zr
  ordtrm(17) = xr*zr*zr
  ordtrm(18) = yr*yr*yr
  ordtrm(19) = yr*yr*zr
  ordtrm(20) = yr*zr*zr
  ordtrm(21) = zr*zr*zr
  ordtrm(22) = xr
  ordtrm(23) = yr
  ordtrm(24) = zr
!
!  First derivatives of xr, yr & zr with respect to Cartesian components
!
  dxyzdc(1,1) = - xr*xr*rk + rk
  dxyzdc(2,1) = - xr*yr*rk
  dxyzdc(3,1) = - xr*zr*rk
  dxyzdc(1,2) = - yr*xr*rk
  dxyzdc(2,2) = - yr*yr*rk + rk
  dxyzdc(3,2) = - yr*zr*rk
  dxyzdc(1,3) = - zr*xr*rk
  dxyzdc(2,3) = - zr*yr*rk
  dxyzdc(3,3) = - zr*zr*rk + rk
  if (lgrad2) then
!
!  Second derivatives of xr, yr & zr with respect to two Cartesian components
!
    d2xyzdc2(1,1) = rk2*(3.0_dp*xr*xr*xr - 3.0_dp*xr)
    d2xyzdc2(2,1) = rk2*(3.0_dp*xr*xr*yr - yr)
    d2xyzdc2(3,1) = rk2*(3.0_dp*xr*xr*zr - zr)
    d2xyzdc2(4,1) = rk2*(3.0_dp*xr*yr*yr - xr)
    d2xyzdc2(5,1) = rk2*(3.0_dp*xr*yr*zr)
    d2xyzdc2(6,1) = rk2*(3.0_dp*xr*zr*zr - xr)
    d2xyzdc2(1,2) = rk2*(3.0_dp*yr*xr*xr - yr)
    d2xyzdc2(2,2) = rk2*(3.0_dp*yr*xr*yr - xr)
    d2xyzdc2(3,2) = rk2*(3.0_dp*yr*xr*zr)
    d2xyzdc2(4,2) = rk2*(3.0_dp*yr*yr*yr - 3.0_dp*yr)
    d2xyzdc2(5,2) = rk2*(3.0_dp*yr*yr*zr - zr)
    d2xyzdc2(6,2) = rk2*(3.0_dp*yr*zr*zr - yr)
    d2xyzdc2(1,3) = rk2*(3.0_dp*zr*xr*xr - zr)
    d2xyzdc2(2,3) = rk2*(3.0_dp*zr*xr*yr)
    d2xyzdc2(3,3) = rk2*(3.0_dp*zr*xr*zr - xr)
    d2xyzdc2(4,3) = rk2*(3.0_dp*zr*yr*yr - zr)
    d2xyzdc2(5,3) = rk2*(3.0_dp*zr*yr*zr - yr)
    d2xyzdc2(6,3) = rk2*(3.0_dp*zr*zr*zr - 3.0_dp*zr)
  endif
!
!  Strain terms if required
!
  if (lstr) then
    rpd(1) = xl*xl
    rpd(2) = yl*yl
    rpd(3) = zl*zl
    rpd(4) = yl*zl
    rpd(5) = xl*zl
    rpd(6) = xl*yl
!
!  First derivatives of xr, yr & zr with respect to strain
!
    dxyzds(1:6,1:3) = 0.0_dp
    do ns1 = 1,6
      dxyzds(ns1,1) = - rpd(ns1)*xr*rk2
      dxyzds(ns1,2) = - rpd(ns1)*yr*rk2
      dxyzds(ns1,3) = - rpd(ns1)*zr*rk2
    enddo
    dxyzds(1,1) = dxyzds(1,1) + xr
    dxyzds(5,1) = dxyzds(5,1) + 0.5_dp*zr
    dxyzds(6,1) = dxyzds(6,1) + 0.5_dp*yr
    dxyzds(2,2) = dxyzds(2,2) + yr
    dxyzds(4,2) = dxyzds(4,2) + 0.5_dp*zr
    dxyzds(6,2) = dxyzds(6,2) + 0.5_dp*xr
    dxyzds(3,3) = dxyzds(3,3) + zr
    dxyzds(4,3) = dxyzds(4,3) + 0.5_dp*yr
    dxyzds(5,3) = dxyzds(5,3) + 0.5_dp*xr
    if (lgrad2) then
!
!  Setup derivatives with respect to strain of just x, y and z
!
      dxyznords(1:6,1:3) = 0.0_dp
      dxyznords(1,1) = xl
      dxyznords(5,1) = 0.5_dp*zl
      dxyznords(6,1) = 0.5_dp*yl
      dxyznords(2,2) = yl
      dxyznords(4,2) = 0.5_dp*zl
      dxyznords(6,2) = 0.5_dp*xl
      dxyznords(3,3) = zl
      dxyznords(4,3) = 0.5_dp*yl
      dxyznords(5,3) = 0.5_dp*xl
!
      d2sds2(1:21) = 0.0_dp
      d2sds2(1)  = 2.0_dp*xr*xr
      d2sds2(3)  = 2.0_dp*yr*yr
      d2sds2(6)  = 2.0_dp*zr*zr
      d2sds2(8)  = yr*zr
      d2sds2(9)  = yr*zr
      d2sds2(10) = 0.5_dp*(yr*yr + zr*zr)
      d2sds2(11) = xr*zr
      d2sds2(13) = xr*zr
      d2sds2(14) = 0.5_dp*xr*yr
      d2sds2(15) = 0.5_dp*(xr*xr + zr*zr)
      d2sds2(16) = xr*yr
      d2sds2(17) = xr*yr
      d2sds2(19) = 0.5_dp*xr*zr
      d2sds2(20) = 0.5_dp*yr*zr
      d2sds2(21) = 0.5_dp*(xr*xr + yr*yr)
!
!  Second derivatives of xr, yr & zr with respect to two strains
!
      ind = 0
      do ns1 = 1,6
        do ns2 = 1,ns1
          ind = ind + 1
          d2xyzds2(ind,1) = 3.0_dp*xr*rpd(ns1)*rpd(ns2)*rk2*rk2 - rk3*(dxyznords(ns1,1)*rpd(ns2) + dxyznords(ns2,1)*rpd(ns1)) - &
                            xr*d2sds2(ind)
          d2xyzds2(ind,2) = 3.0_dp*yr*rpd(ns1)*rpd(ns2)*rk2*rk2 - rk3*(dxyznords(ns1,2)*rpd(ns2) + dxyznords(ns2,2)*rpd(ns1)) - &
                            yr*d2sds2(ind)
          d2xyzds2(ind,3) = 3.0_dp*zr*rpd(ns1)*rpd(ns2)*rk2*rk2 - rk3*(dxyznords(ns1,3)*rpd(ns2) + dxyznords(ns2,3)*rpd(ns1)) - &
                            zr*d2sds2(ind)
        enddo
      enddo
!
!  Terms from second derivatives of Cartesian coordinate with respect to two strains
!
      d2xyzds2(1,1)  = d2xyzds2(1,1)  + xr
      d2xyzds2(11,1) = d2xyzds2(11,1) + 0.5_dp*zr
      d2xyzds2(15,1) = d2xyzds2(15,1) + 0.25_dp*xr
      d2xyzds2(16,1) = d2xyzds2(16,1) + 0.5_dp*yr
      d2xyzds2(21,1) = d2xyzds2(21,1) + 0.25_dp*xr
!
      d2xyzds2(3,2)  = d2xyzds2(3,2)  + yr
      d2xyzds2(8,2)  = d2xyzds2(8,2)  + 0.5_dp*zr
      d2xyzds2(10,2) = d2xyzds2(10,2) + 0.25_dp*yr
      d2xyzds2(14,2) = d2xyzds2(14,2) + 0.25_dp*xr
      d2xyzds2(17,2) = d2xyzds2(17,2) + 0.5_dp*xr
      d2xyzds2(21,2) = d2xyzds2(21,2) + 0.25_dp*yr
!
      d2xyzds2(6,3)  = d2xyzds2(6,3)  + zr
      d2xyzds2(9,3)  = d2xyzds2(9,3)  + 0.5_dp*yr
      d2xyzds2(10,3) = d2xyzds2(10,3) + 0.25_dp*zr
      d2xyzds2(13,3) = d2xyzds2(13,3) + 0.5_dp*xr
      d2xyzds2(15,3) = d2xyzds2(15,3) + 0.25_dp*zr
      d2xyzds2(19,3) = d2xyzds2(19,3) + 0.25_dp*xr
      d2xyzds2(20,3) = d2xyzds2(20,3) + 0.25_dp*yr
!
!  Second derivatives of xr, yr & zr with respect to one Cartesian component and one strain
!
      do ns1 = 1,6
        d2xyzdsdc(ns1,1,1) = (3.0_dp*xr*xr - 1.0_dp)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,2,1) = (3.0_dp*xr*yr)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,3,1) = (3.0_dp*xr*zr)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,1,2) = (3.0_dp*yr*xr)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,2,2) = (3.0_dp*yr*yr - 1.0_dp)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,3,2) = (3.0_dp*yr*zr)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,1,3) = (3.0_dp*zr*xr)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,2,3) = (3.0_dp*zr*yr)*rk3*rpd(ns1)
        d2xyzdsdc(ns1,3,3) = (3.0_dp*zr*zr - 1.0_dp)*rk3*rpd(ns1)
      enddo
!
      d2xyzdsdc(1,1,1) = d2xyzdsdc(1,1,1) - 3.0_dp*xr*xr*rk + rk
      d2xyzdsdc(5,1,1) = d2xyzdsdc(5,1,1) - 1.5_dp*xr*zr*rk
      d2xyzdsdc(6,1,1) = d2xyzdsdc(6,1,1) - 1.5_dp*xr*yr*rk
      d2xyzdsdc(1,2,1) = d2xyzdsdc(1,2,1) - yr*xr*rk
      d2xyzdsdc(2,2,1) = d2xyzdsdc(2,2,1) - 2.0_dp*xr*yr*rk
      d2xyzdsdc(4,2,1) = d2xyzdsdc(4,2,1) - xr*zr*rk
      d2xyzdsdc(5,2,1) = d2xyzdsdc(5,2,1) - 0.5_dp*yr*zr*rk
      d2xyzdsdc(6,2,1) = d2xyzdsdc(6,2,1) - xr*xr*rk - 0.5_dp*yr*yr*rk + 0.5_dp*rk
      d2xyzdsdc(1,3,1) = d2xyzdsdc(1,3,1) - zr*xr*rk
      d2xyzdsdc(3,3,1) = d2xyzdsdc(3,3,1) - 2.0_dp*xr*zr*rk
      d2xyzdsdc(4,3,1) = d2xyzdsdc(4,3,1) - xr*yr*rk
      d2xyzdsdc(5,3,1) = d2xyzdsdc(5,3,1) - xr*xr*rk - 0.5_dp*zr*zr*rk + 0.5_dp*rk
      d2xyzdsdc(6,3,1) = d2xyzdsdc(6,3,1) - 0.5_dp*zr*yr*rk
!
      d2xyzdsdc(1,1,2) = d2xyzdsdc(1,1,2) - 2.0_dp*yr*xr*rk
      d2xyzdsdc(2,1,2) = d2xyzdsdc(2,1,2) - xr*yr*rk
      d2xyzdsdc(4,1,2) = d2xyzdsdc(4,1,2) - 0.5_dp*xr*zr*rk
      d2xyzdsdc(5,1,2) = d2xyzdsdc(5,1,2) - yr*zr*rk
      d2xyzdsdc(6,1,2) = d2xyzdsdc(6,1,2) - yr*yr*rk - 0.5_dp*xr*xr*rk + 0.5_dp*rk
      d2xyzdsdc(2,2,2) = d2xyzdsdc(2,2,2) - 3.0_dp*yr*yr*rk + rk
      d2xyzdsdc(4,2,2) = d2xyzdsdc(4,2,2) - 1.5_dp*yr*zr*rk
      d2xyzdsdc(6,2,2) = d2xyzdsdc(6,2,2) - 1.5_dp*yr*xr*rk
      d2xyzdsdc(2,3,2) = d2xyzdsdc(2,3,2) - zr*yr*rk
      d2xyzdsdc(3,3,2) = d2xyzdsdc(3,3,2) - 2.0_dp*yr*zr*rk
      d2xyzdsdc(4,3,2) = d2xyzdsdc(4,3,2) - yr*yr*rk - 0.5_dp*zr*zr*rk + 0.5_dp*rk
      d2xyzdsdc(5,3,2) = d2xyzdsdc(5,3,2) - yr*xr*rk
      d2xyzdsdc(6,3,2) = d2xyzdsdc(6,3,2) - 0.5_dp*zr*xr*rk
!
      d2xyzdsdc(1,1,3) = d2xyzdsdc(1,1,3) - 2.0_dp*zr*xr*rk
      d2xyzdsdc(3,1,3) = d2xyzdsdc(3,1,3) - xr*zr*rk
      d2xyzdsdc(4,1,3) = d2xyzdsdc(4,1,3) - 0.5_dp*xr*yr*rk
      d2xyzdsdc(5,1,3) = d2xyzdsdc(5,1,3) - zr*zr*rk - 0.5_dp*xr*xr*rk + 0.5_dp*rk
      d2xyzdsdc(6,1,3) = d2xyzdsdc(6,1,3) - zr*yr*rk
      d2xyzdsdc(2,2,3) = d2xyzdsdc(2,2,3) - 2.0_dp*zr*yr*rk
      d2xyzdsdc(3,2,3) = d2xyzdsdc(3,2,3) - yr*zr*rk
      d2xyzdsdc(4,2,3) = d2xyzdsdc(4,2,3) - zr*zr*rk - 0.5_dp*yr*yr*rk + 0.5_dp*rk
      d2xyzdsdc(5,2,3) = d2xyzdsdc(5,2,3) - 0.5_dp*yr*xr*rk
      d2xyzdsdc(6,2,3) = d2xyzdsdc(6,2,3) - zr*xr*rk
      d2xyzdsdc(3,3,3) = d2xyzdsdc(3,3,3) - 3.0_dp*zr*zr*rk + rk
      d2xyzdsdc(4,3,3) = d2xyzdsdc(4,3,3) - 1.5_dp*zr*yr*rk
      d2xyzdsdc(5,3,3) = d2xyzdsdc(5,3,3) - 1.5_dp*zr*xr*rk
    endif
  endif
!
!  Set up general taper values
!
  if (tapertype.eq.5.and.tapermax.gt.1.0d-12) then
!
!  MDF taper
!
    call mdftaper(r,tapermin,tapermax,tpfn,dtpfn,d2tpfn,d3tpfn,lgrad1,lgrad2,.false.)
!
!  Convert taper terms to allow for 1/r factors
!
    if (lgrad1) then
      dtpfn = rk*dtpfn
      if (lgrad2) then
        d2tpfn = rk2*d2tpfn
        d2tpfn = d2tpfn - rk2*dtpfn
      endif
    endif
  endif
!
  do mm = 1,neamspec
!
!  Set flag as to whether this species uses the 21 or 24 term MEAM expression
!
    lt24 = (neamfnmeamtype(mm).ne.1)
!
!  Terms for i->j
!
    if (nati.eq.neamnat(mm).and.(ntypi.eq.neamtyp(mm).or.neamtyp(mm).eq.0)) then
      if (neamnat2(mm).eq.0.or.(neamnat2(mm).eq.natj.and.(ntypj.eq.neamtyp2(mm).or.neamtyp2(mm).eq.0))) then
!
        do j = 1,ndenfncomp(mm)
!
!  Loop over MEAM order
!
          do order = 1,neammeamorder(j,mm)
            rhojiloc = 0.0_dp
            drhojiloc = 0.0_dp
            drhoji2loc = 0.0_dp
!
!  Density functional forms
!
            npt = nint(denpar(6,order,j,mm))
            if (ndenfn(j,mm).eq.1) then
!
!  Power law
!
              trm1 = oci*denpar(1,order,j,mm)*(rk**npt)
              rhojiloc = trm1
              if (lgrad1) then
                trm1 = - dble(npt)*trm1*rk2
                drhojiloc = trm1
                if (lgrad2) then
                  trm2 = - trm1*dble(npt+2)*rk2
                  drhoji2loc = trm2
                endif
              endif
            elseif (ndenfn(j,mm).eq.2) then
!
!  Exponential
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              cpt = denpar(3,order,j,mm)
              trm1 = apt*oci*exp(-bpt*(r-cpt))
              if (npt.ne.0) then
                rn1 = r**(npt-1)
                rhojiloc = trm1*rn1*r
                if (lgrad1) then
                  drhojiloc = trm1*rn1*(npt*rk-bpt)
                  if (lgrad2) then
                    drhoji2loc = trm1*rn1*rk*(npt*(npt-1)*rk2 - bpt*dble(2*npt-1)*rk + bpt*bpt)
                  endif
                endif
              else
                rhojiloc = trm1
                if (lgrad1) then
                  drhojiloc = - bpt*trm1*rk
                  if (lgrad2) then
                    drhoji2loc = bpt*trm1*rk2*(bpt + rk)
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.3) then
!
!  Gaussian
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              cpt = denpar(3,order,j,mm)
              rd = (r - cpt)
              rd2 = rd*rd
              trm1 = apt*oci*exp(-bpt*rd2)
              if (npt.ne.0) then
                rn1 = r**(npt-1)
                rhojiloc = trm1*rn1*r
                if (lgrad1) then
                  drhojiloc = trm1*rn1*(npt*rk - 2.0_dp*bpt*rd)
                  if (lgrad2) then
                    drhoji2loc = trm1*rn1*rk*(npt*(npt-2)*rk2- &
                      2.0_dp*bpt*rd*dble(2*npt-1)*rk+2.0_dp*bpt*(2.0_dp*bpt*rd2-1._dp))
                  endif
                endif
              else
                rhojiloc = trm1
                if (lgrad1) then
                  drhojiloc = - 2.0_dp*bpt*rd*trm1*rk
                  if (lgrad2) then
                    drhoji2loc = 2.0_dp*trm1*rk2*bpt*(rd*rk-1.0_dp+4.0_dp*bpt*rd2)
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.4) then
!
!  Cubic
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              if (r.lt.bpt) then
                rd = (r - bpt)
                trm1 = 3.0_dp*apt*rk
                rhojiloc = apt*rd*rd*rd
                if (lgrad1) then
                  drhojiloc = trm1*rd*rd
                  if (lgrad2) then
                    trm1 = trm1*rk
                    drhoji2loc = trm1*rd*(2.0_dp - rd*rk)
                  endif
                endif
              else
                rhojiloc = 0.0_dp
                if (lgrad1) then
                  drhojiloc = 0.0_dp
                  if (lgrad2) then
                    drhoji2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.5) then
!
!  Voter-Chen
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              etrm = exp(-bpt*r)
              cpt = 2.0_dp**9
              trm1 = apt*etrm*(1.0_dp + cpt*etrm)
              trm2 = apt*etrm*(1.0_dp + 2.0_dp*cpt*etrm)
              rhojiloc = r2*r2*r2*trm1
              if (lgrad1) then
                drhojiloc = r2*r2*(6.0_dp*trm1 - bpt*r*trm2)
                if (lgrad2) then
                  trm3 = apt*etrm*(1.0_dp + 4.0_dp*cpt*etrm)
                  drhoji2loc = r2*(24.0_dp*trm1 - 11.0_dp*bpt*r*trm2 + bpt*bpt*r2*trm3)
                endif
              endif
            elseif (ndenfn(j,mm).eq.6) then
!
!  Quadratic
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              if (r.lt.bpt) then
                rd = (r - bpt)
                trm1 = 2.0_dp*apt*rk
                rhojiloc = apt*rd*rd
                if (lgrad1) then
                  drhojiloc = trm1*rd
                  if (lgrad2) then
                    trm1 = trm1*rk
                    drhoji2loc = trm1*(1.0_dp - rd*rk)
                  endif
                endif
              else
                rhojiloc = 0.0_dp
                if (lgrad1) then
                  drhojiloc = 0.0_dp
                  if (lgrad2) then
                    drhoji2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.7) then
!
!  Quartic
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              if (r.lt.bpt) then
                rd = (r - bpt)
                trm1 = 4.0_dp*apt*rk*rd
                rhojiloc = apt*rd*rd*rd*rd
                if (lgrad1) then
                  drhojiloc = trm1*rd*rd
                  if (lgrad2) then
                    trm1 = trm1*rk
                    drhoji2loc = trm1*rd*(3.0_dp - rd*rk)
                  endif
                endif
              else
                rhojiloc = 0.0_dp
                if (lgrad1) then
                  drhojiloc = 0.0_dp
                  if (lgrad2) then
                    drhoji2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.8) then
!
!  Glue
!
              if (r.lt.denpar(1,order,j,mm)) then
                dr = (r - denpar(1,order,j,mm))
                dr2 = dr*dr
                dr3 = dr2*dr
                trm1 = 3.0_dp*denpar(4,order,j,mm)*dr2 + 2.0_dp*denpar(5,order,j,mm)*dr + denpar(6,order,j,mm)
                rhojiloc = denpar(4,order,j,mm)*dr3 + denpar(5,order,j,mm)*dr2 + denpar(6,order,j,mm)*dr + denpar(7,order,j,mm)
                if (lgrad1) then
                  drhojiloc = trm1*rk
                  if (lgrad2) then
                    trm2 = 6.0_dp*denpar(4,order,j,mm)*dr + 2.0_dp*denpar(5,order,j,mm)
                    drhoji2loc = rk2*(trm2 - rk*trm1)
                  endif
                endif
              elseif (r.lt.denpar(2,order,j,mm)) then
                dr = (r - denpar(1,order,j,mm))
                dr2 = dr*dr
                dr3 = dr2*dr
                trm1 = 3.0_dp*denpar(8,order,j,mm)*dr2 + 2.0_dp*denpar(9,order,j,mm)*dr + denpar(10,order,j,mm)
                rhojiloc = denpar(8,order,j,mm)*dr3 + denpar(9,order,j,mm)*dr2 + denpar(10,order,j,mm)*dr + denpar(11,order,j,mm)
                if (lgrad1) then
                  drhojiloc = trm1*rk
                  if (lgrad2) then
                    trm2 = 6.0_dp*denpar(8,order,j,mm)*dr + 2.0_dp*denpar(9,order,j,mm)
                    drhoji2loc = rk2*(trm2 - rk*trm1)
                  endif
                endif
              elseif (r.lt.denpar(3,order,j,mm)) then
                dr = (r - denpar(3,order,j,mm))
                dr2 = dr*dr
                dr3 = dr2*dr
                trm1 = 3.0_dp*denpar(12,order,j,mm)*dr2 + 2.0_dp*denpar(13,order,j,mm)*dr + denpar(14,order,j,mm)
                rhojiloc = denpar(12,order,j,mm)*dr3 + denpar(13,order,j,mm)*dr2 + denpar(14,order,j,mm)*dr + denpar(15,order,j,mm)
                if (lgrad1) then
                  drhojiloc = trm1*rk
                  if (lgrad2) then
                    trm2 = 6.0_dp*denpar(12,order,j,mm)*dr + 2.0_dp*denpar(13,order,j,mm)
                    drhoji2loc = rk2*(trm2 - rk*trm1)
                  endif
                endif
              else
                rhojiloc = 0.0_dp
                if (lgrad1) then
                  drhojiloc = 0.0_dp
                  if (lgrad2) then
                    drhoji2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.9) then
!
!  eVoter-Chen
!    
              call etaper(r,0.0_dp,rmax,etpfn,detpfn,d2etpfn,d3etpfn,lgrad1,lgrad2,.false.)
              detpfn = rk*detpfn
              d2etpfn = rk2*d2etpfn
              d3etpfn = rk2*(rk*d3etpfn - 3.0_dp*(d2etpfn - rk2*detpfn))
              d2etpfn = d2etpfn - rk2*detpfn
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              etrm = exp(-bpt*r)
              cpt = 2.0_dp**9
              trm1 = apt*etrm*(1.0_dp + cpt*etrm)
              trm2 = apt*etrm*(1.0_dp + 2.0_dp*cpt*etrm)
              rhojiloc = r2*r2*r2*trm1
              if (lgrad1) then
                drhojiloc = r2*r2*(6.0_dp*trm1 - bpt*r*trm2)
                if (lgrad2) then
                  trm3 = apt*etrm*(1.0_dp + 4.0_dp*cpt*etrm)
                  drhoji2loc = r2*(24.0_dp*trm1 - 11.0_dp*bpt*r*trm2 + bpt*bpt*r2*trm3)
                  drhoji2loc = drhoji2loc*etpfn + 2.0_dp*drhojiloc*detpfn + rhojiloc*d2etpfn
                endif
                drhojiloc = drhojiloc*etpfn + rhojiloc*detpfn
              endif
            elseif (ndenfn(j,mm).eq.10) then
!
!  Mei-Davenport
!   
              rr0 = denpar(7,order,j,mm)
              rr12 = 1.0_dp/12.0_dp
              trm1 = rk*rr0
              rr0 = 1.0_dp/denpar(7,order,j,mm)
              rhojiloc = 0.0_dp
              if (lgrad1) then
                drhojiloc = 0.0_dp
                if (lgrad2) then
                  drhoji2loc = 0.0_dp
                endif
              endif
              do l = 0,5
                trm2 = rr0*rr0
                rhojiloc = rhojiloc + rr12*denpar(l+1,order,j,mm)*(trm1)**(l)
                if (lgrad1) then
                  drhojiloc = drhojiloc - rr12*denpar(l+1,order,j,mm)*trm2*dble(l)*(trm1)**(l+2)
                  if (lgrad2) then
                    trm2 = trm2*trm2
                    drhoji2loc = drhoji2loc + rr12*denpar(l+1,order,j,mm)*trm2*dble(l*(l+2))*(trm1)**(l+4)
                  endif
                endif
              enddo
            elseif (ndenfn(j,mm).eq.12) then
!
!  Baskes
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)/denpar(3,order,j,mm)
              cpt = denpar(3,order,j,mm)
              trm1 = apt*oci*exp(-bpt*(r-cpt))
              rhojiloc = trm1
              if (lgrad1) then
                drhojiloc = - bpt*trm1*rk
                if (lgrad2) then
                  drhoji2loc = bpt*trm1*rk2*(bpt + rk)
                endif
              endif
            elseif (ndenfn(j,mm).eq.14) then
!
!  Fractional power law
!
              rpt = denpar(2,order,j,mm)
              trm1 = oci*denpar(1,order,j,mm)*(rk**npt)
              rhojiloc = trm1
              if (lgrad1) then
                trm1 = - rpt*trm1*rk2
                drhojiloc = trm1
                if (lgrad2) then
                  trm2 = - trm1*(rpt+2.0_dp)*rk2
                  drhoji2loc = trm2
                endif
              endif
            endif
!
!  Add contributions 
!
            if (tapertype.eq.3.and.tapermax.gt.1.0d-12) then
              dt = eamtaperrho(mm) - (tapermax/taperm)*eamtaperdrho(mm)*(1.0_dp - (r/tapermax)**taperm)
              term0 = rhojiloc - dt
              if (lgrad1) then
                d1t = eamtaperdrho(mm)*rk*(r/tapermax)**(taperm-1.0_dp)
                term1 = drhojiloc - d1t
                if (lgrad2) then
                  d2t = ((taperm - 1.0_dp)/tapermax)*eamtaperdrho(mm)*(r/tapermax)**(taperm-2.0_dp)
                  term2 = drhoji2loc - rk2*(d2t - d1t)
                endif
              endif
            elseif (tapertype.eq.5.and.tapermax.gt.1.0d-12) then
!
!  MDF taper
!
              term0 = rhojiloc*tpfn
              if (lgrad1) then
                term1 = drhojiloc*tpfn + rhojiloc*dtpfn
                if (lgrad2) then
                  term2 = drhoji2loc*tpfn + 2.0_dp*drhojiloc*dtpfn + rhojiloc*d2tpfn
                endif
              endif
            else
              term0 = rhojiloc
              if (lgrad1) then
                term1 = drhojiloc
                if (lgrad2) then
                  term2 = drhoji2loc
                endif
              endif
            endif
!
!  Combine radial derivatives with angular contributions
!
            if (order.eq.1) then
              rhoji(1)  = rhoji(1)  + term0
            elseif (order.eq.2) then
              rhoji(2)  = rhoji(2)  - xr*term0
              rhoji(3)  = rhoji(3)  - yr*term0
              rhoji(4)  = rhoji(4)  - zr*term0
            elseif (order.eq.3) then
              rhoji(5)  = rhoji(5)  + xr*xr*term0
              rhoji(6)  = rhoji(6)  + xr*yr*term0
              rhoji(7)  = rhoji(7)  + xr*zr*term0
              rhoji(8)  = rhoji(8)  + yr*yr*term0
              rhoji(9)  = rhoji(9)  + yr*zr*term0
              rhoji(10) = rhoji(10) + zr*zr*term0
              rhoji(11) = rhoji(11) + term0
            elseif (order.eq.4) then
              rhoji(12) = rhoji(12) - xr*xr*xr*term0
              rhoji(13) = rhoji(13) - xr*xr*yr*term0
              rhoji(14) = rhoji(14) - xr*xr*zr*term0
              rhoji(15) = rhoji(15) - xr*yr*yr*term0
              rhoji(16) = rhoji(16) - xr*yr*zr*term0
              rhoji(17) = rhoji(17) - xr*zr*zr*term0
              rhoji(18) = rhoji(18) - yr*yr*yr*term0
              rhoji(19) = rhoji(19) - yr*yr*zr*term0
              rhoji(20) = rhoji(20) - yr*zr*zr*term0
              rhoji(21) = rhoji(21) - zr*zr*zr*term0
              if (lt24) then
                rhoji(22) = rhoji(22) - xr*term0
                rhoji(23) = rhoji(23) - yr*term0
                rhoji(24) = rhoji(24) - zr*term0
              endif
            endif
            if (lgrad1) then
              dtdc(1) = term1*xl
              dtdc(2) = term1*yl
              dtdc(3) = term1*zl
              if (lstr) then
                do ns1 = 1,6
                  dtds(ns1) = term1*rpd(ns1)
                enddo
              endif
              if (order.eq.1) then
                drhoji(1,1) = drhoji(1,1) + dtdc(1)
                drhoji(2,1) = drhoji(2,1) + dtdc(2)
                drhoji(3,1) = drhoji(3,1) + dtdc(3)
                if (lstr) then
                  do ns1 = 1,6
                    drhojis(ns1,1) = drhojis(ns1,1) + dtds(ns1)
                  enddo
                endif
              elseif (order.eq.2) then
                do nc1 = 1,3
                  drhoji(nc1,2) = drhoji(nc1,2) - xr*dtdc(nc1) - dxyzdc(nc1,1)*term0
                  drhoji(nc1,3) = drhoji(nc1,3) - yr*dtdc(nc1) - dxyzdc(nc1,2)*term0
                  drhoji(nc1,4) = drhoji(nc1,4) - zr*dtdc(nc1) - dxyzdc(nc1,3)*term0
                enddo
                if (lstr) then
                  do ns1 = 1,6
                    drhojis(ns1,2) = drhojis(ns1,2) - xr*dtds(ns1) - dxyzds(ns1,1)*term0
                    drhojis(ns1,3) = drhojis(ns1,3) - yr*dtds(ns1) - dxyzds(ns1,2)*term0
                    drhojis(ns1,4) = drhojis(ns1,4) - zr*dtds(ns1) - dxyzds(ns1,3)*term0
                  enddo
                endif
              elseif (order.eq.3) then
                do nc1 = 1,3
                  drhoji(nc1,5)  = drhoji(nc1,5)  + xr*xr*dtdc(nc1) + 2.0_dp*xr*dxyzdc(nc1,1)*term0
                  drhoji(nc1,6)  = drhoji(nc1,6)  + xr*yr*dtdc(nc1) + (xr*dxyzdc(nc1,2) + yr*dxyzdc(nc1,1))*term0
                  drhoji(nc1,7)  = drhoji(nc1,7)  + xr*zr*dtdc(nc1) + (xr*dxyzdc(nc1,3) + zr*dxyzdc(nc1,1))*term0
                  drhoji(nc1,8)  = drhoji(nc1,8)  + yr*yr*dtdc(nc1) + 2.0_dp*yr*dxyzdc(nc1,2)*term0
                  drhoji(nc1,9)  = drhoji(nc1,9)  + yr*zr*dtdc(nc1) + (yr*dxyzdc(nc1,3) + zr*dxyzdc(nc1,2))*term0
                  drhoji(nc1,10) = drhoji(nc1,10) + zr*zr*dtdc(nc1) + 2.0_dp*zr*dxyzdc(nc1,3)*term0
                  drhoji(nc1,11) = drhoji(nc1,11) + dtdc(nc1)
                enddo
                if (lstr) then
                  do ns1 = 1,6
                    drhojis(ns1,5)  = drhojis(ns1,5)  + xr*xr*dtds(ns1) + 2.0_dp*xr*dxyzds(ns1,1)*term0
                    drhojis(ns1,6)  = drhojis(ns1,6)  + xr*yr*dtds(ns1) + (xr*dxyzds(ns1,2) + yr*dxyzds(ns1,1))*term0
                    drhojis(ns1,7)  = drhojis(ns1,7)  + xr*zr*dtds(ns1) + (xr*dxyzds(ns1,3) + zr*dxyzds(ns1,1))*term0
                    drhojis(ns1,8)  = drhojis(ns1,8)  + yr*yr*dtds(ns1) + 2.0_dp*yr*dxyzds(ns1,2)*term0
                    drhojis(ns1,9)  = drhojis(ns1,9)  + yr*zr*dtds(ns1) + (yr*dxyzds(ns1,3) + zr*dxyzds(ns1,2))*term0
                    drhojis(ns1,10) = drhojis(ns1,10) + zr*zr*dtds(ns1) + 2.0_dp*zr*dxyzds(ns1,3)*term0
                    drhojis(ns1,11) = drhojis(ns1,11) + dtds(ns1)
                  enddo
                endif
              elseif (order.eq.4) then
                do nc1 = 1,3
                  drhoji(nc1,12) = drhoji(nc1,12) - xr*xr*xr*dtdc(nc1) - 3.0_dp*xr*xr*dxyzdc(nc1,1)*term0
                  drhoji(nc1,13) = drhoji(nc1,13) - xr*xr*yr*dtdc(nc1) - (2.0_dp*xr*yr*dxyzdc(nc1,1) + xr*xr*dxyzdc(nc1,2))*term0
                  drhoji(nc1,14) = drhoji(nc1,14) - xr*xr*zr*dtdc(nc1) - (2.0_dp*xr*zr*dxyzdc(nc1,1) + xr*xr*dxyzdc(nc1,3))*term0
                  drhoji(nc1,15) = drhoji(nc1,15) - xr*yr*yr*dtdc(nc1) - (2.0_dp*xr*yr*dxyzdc(nc1,2) + yr*yr*dxyzdc(nc1,1))*term0
                  drhoji(nc1,16) = drhoji(nc1,16) - xr*yr*zr*dtdc(nc1) - (xr*yr*dxyzdc(nc1,3) + xr*zr*dxyzdc(nc1,2) + &
                                                                          yr*zr*dxyzdc(nc1,1))*term0
                  drhoji(nc1,17) = drhoji(nc1,17) - xr*zr*zr*dtdc(nc1) - (2.0_dp*xr*zr*dxyzdc(nc1,3) + zr*zr*dxyzdc(nc1,1))*term0
                  drhoji(nc1,18) = drhoji(nc1,18) - yr*yr*yr*dtdc(nc1) - 3.0_dp*yr*yr*dxyzdc(nc1,2)*term0
                  drhoji(nc1,19) = drhoji(nc1,19) - yr*yr*zr*dtdc(nc1) - (2.0_dp*yr*zr*dxyzdc(nc1,2) + yr*yr*dxyzdc(nc1,3))*term0
                  drhoji(nc1,20) = drhoji(nc1,20) - yr*zr*zr*dtdc(nc1) - (2.0_dp*yr*zr*dxyzdc(nc1,3) + zr*zr*dxyzdc(nc1,2))*term0
                  drhoji(nc1,21) = drhoji(nc1,21) - zr*zr*zr*dtdc(nc1) - 3.0_dp*zr*zr*dxyzdc(nc1,3)*term0
                enddo
                if (lt24) then
                  do nc1 = 1,3
                    drhoji(nc1,22) = drhoji(nc1,22) - xr*dtdc(nc1) - dxyzdc(nc1,1)*term0
                    drhoji(nc1,23) = drhoji(nc1,23) - yr*dtdc(nc1) - dxyzdc(nc1,2)*term0
                    drhoji(nc1,24) = drhoji(nc1,24) - zr*dtdc(nc1) - dxyzdc(nc1,3)*term0
                  enddo
                endif
!
                if (lstr) then
!
!  Derivatives of rk3 and term0
!
                  do ns1 = 1,6
                    drhojis(ns1,12) = drhojis(ns1,12) - xr*xr*xr*dtds(ns1) - 3.0_dp*xr*xr*dxyzds(ns1,1)*term0
                    drhojis(ns1,13) = drhojis(ns1,13) - xr*xr*yr*dtds(ns1) - (2.0_dp*xr*yr*dxyzds(ns1,1) + &
                                                                              xr*xr*dxyzds(ns1,2))*term0
                    drhojis(ns1,14) = drhojis(ns1,14) - xr*xr*zr*dtds(ns1) - (2.0_dp*xr*zr*dxyzds(ns1,1) + &
                                                                              xr*xr*dxyzds(ns1,3))*term0
                    drhojis(ns1,15) = drhojis(ns1,15) - xr*yr*yr*dtds(ns1) - (2.0_dp*xr*yr*dxyzds(ns1,2) + &
                                                                              yr*yr*dxyzds(ns1,1))*term0
                    drhojis(ns1,16) = drhojis(ns1,16) - xr*yr*zr*dtds(ns1) - (xr*yr*dxyzds(ns1,3) + &
                                                                              xr*zr*dxyzds(ns1,2) + &
                                                                              yr*zr*dxyzds(ns1,1))*term0
                    drhojis(ns1,17) = drhojis(ns1,17) - xr*zr*zr*dtds(ns1) - (2.0_dp*xr*zr*dxyzds(ns1,3) + &
                                                                              zr*zr*dxyzds(ns1,1))*term0
                    drhojis(ns1,18) = drhojis(ns1,18) - yr*yr*yr*dtds(ns1) - 3.0_dp*yr*yr*dxyzds(ns1,2)*term0
                    drhojis(ns1,19) = drhojis(ns1,19) - yr*yr*zr*dtds(ns1) - (2.0_dp*yr*zr*dxyzds(ns1,2) + &
                                                                              yr*yr*dxyzds(ns1,3))*term0
                    drhojis(ns1,20) = drhojis(ns1,20) - yr*zr*zr*dtds(ns1) - (2.0_dp*yr*zr*dxyzds(ns1,3) + &
                                                                              zr*zr*dxyzds(ns1,2))*term0
                    drhojis(ns1,21) = drhojis(ns1,21) - zr*zr*zr*dtds(ns1) - 3.0_dp*zr*zr*dxyzds(ns1,3)*term0
                  enddo
                  if (lt24) then
                    do ns1 = 1,6
                      drhojis(ns1,22) = drhojis(ns1,22) - xr*dtds(ns1) - dxyzds(ns1,1)*term0
                      drhojis(ns1,23) = drhojis(ns1,23) - yr*dtds(ns1) - dxyzds(ns1,2)*term0
                      drhojis(ns1,24) = drhojis(ns1,24) - zr*dtds(ns1) - dxyzds(ns1,3)*term0
                    enddo
                  endif
                endif
              endif
              if (lgrad2) then
                d2tdc2(1) = term2*xl*xl + term1
                d2tdc2(2) = term2*xl*yl
                d2tdc2(3) = term2*xl*zl
                d2tdc2(4) = term2*yl*yl + term1
                d2tdc2(5) = term2*yl*zl
                d2tdc2(6) = term2*zl*zl + term1
                if (order.eq.1) then
                  do nc1 = 1,6
                    drhoji2(nc1,1) = drhoji2(nc1,1) + d2tdc2(nc1)
                  enddo
                elseif (order.eq.2) then
                  ind = 0
                  do nc1 = 1,3
                    do nc2 = nc1,3
                      ind = ind + 1
                      drhoji2(ind,2) = drhoji2(ind,2) - xr*d2tdc2(ind) - term0*d2xyzdc2(ind,1) - &
                                                        dxyzdc(nc1,1)*dtdc(nc2) - dxyzdc(nc2,1)*dtdc(nc1)
                      drhoji2(ind,3) = drhoji2(ind,3) - yr*d2tdc2(ind) - term0*d2xyzdc2(ind,2) - &
                                                        dxyzdc(nc1,2)*dtdc(nc2) - dxyzdc(nc2,2)*dtdc(nc1)
                      drhoji2(ind,4) = drhoji2(ind,4) - zr*d2tdc2(ind) - term0*d2xyzdc2(ind,3) - &
                                                        dxyzdc(nc1,3)*dtdc(nc2) - dxyzdc(nc2,3)*dtdc(nc1)
                    enddo
                  enddo
                elseif (order.eq.3) then
                  ind = 0
                  do nc1 = 1,3
                    do nc2 = nc1,3
                      ind = ind + 1
                      drhoji2(ind,5)  = drhoji2(ind,5)  + xr*xr*d2tdc2(ind) + term0*(2.0_dp*d2xyzdc2(ind,1)*xr) + &
                                                          term0*(2.0_dp*dxyzdc(nc1,1)*dxyzdc(nc2,1)) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,1)*xr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,1)*xr)
                      drhoji2(ind,6)  = drhoji2(ind,6)  + xr*yr*d2tdc2(ind) + term0*(d2xyzdc2(ind,1)*yr + d2xyzdc2(ind,2)*xr) + &
                                                          term0*(dxyzdc(nc1,1)*dxyzdc(nc2,2) + dxyzdc(nc1,2)*dxyzdc(nc2,1)) + &
                                                          dtdc(nc1)*(dxyzdc(nc2,1)*yr + dxyzdc(nc2,2)*xr) + &
                                                          dtdc(nc2)*(dxyzdc(nc1,1)*yr + dxyzdc(nc1,2)*xr)
                      drhoji2(ind,7)  = drhoji2(ind,7)  + xr*zr*d2tdc2(ind) + term0*(d2xyzdc2(ind,1)*zr + d2xyzdc2(ind,3)*xr) + &
                                                          term0*(dxyzdc(nc1,1)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,1)) + &
                                                          dtdc(nc1)*(dxyzdc(nc2,1)*zr + dxyzdc(nc2,3)*xr) + &
                                                          dtdc(nc2)*(dxyzdc(nc1,1)*zr + dxyzdc(nc1,3)*xr)
                      drhoji2(ind,8)  = drhoji2(ind,8)  + yr*yr*d2tdc2(ind) + term0*(2.0_dp*d2xyzdc2(ind,2)*yr) + &
                                                          term0*(2.0_dp*dxyzdc(nc1,2)*dxyzdc(nc2,2)) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,2)*yr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,2)*yr)
                      drhoji2(ind,9)  = drhoji2(ind,9)  + yr*zr*d2tdc2(ind) + term0*(d2xyzdc2(ind,2)*zr + d2xyzdc2(ind,3)*yr) + &
                                                          term0*(dxyzdc(nc1,2)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,2)) + &
                                                          dtdc(nc1)*(dxyzdc(nc2,2)*zr + dxyzdc(nc2,3)*yr) + &
                                                          dtdc(nc2)*(dxyzdc(nc1,2)*zr + dxyzdc(nc1,3)*yr)
                      drhoji2(ind,10) = drhoji2(ind,10) + zr*zr*d2tdc2(ind) + term0*(2.0_dp*d2xyzdc2(ind,3)*zr) + &
                                                          term0*(2.0_dp*dxyzdc(nc1,3)*dxyzdc(nc2,3)) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,3)*zr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,3)*zr)
                      drhoji2(ind,11) = drhoji2(ind,11) + d2tdc2(ind)
                    enddo
                  enddo
                elseif (order.eq.4) then
                  ind = 0
                  do nc1 = 1,3
                    do nc2 = nc1,3
                      ind = ind + 1
                      drhoji2(ind,12) = drhoji2(ind,12) - xr*xr*xr*d2tdc2(ind) - term0*(3.0_dp*d2xyzdc2(ind,1)*xr*xr) - &
                                                          term0*(6.0_dp*dxyzdc(nc1,1)*dxyzdc(nc2,1)*xr) - &
                                                          dtdc(nc1)*(3.0_dp*dxyzdc(nc2,1)*xr*xr) - &
                                                          dtdc(nc2)*(3.0_dp*dxyzdc(nc1,1)*xr*xr)
                      drhoji2(ind,13) = drhoji2(ind,13) - xr*xr*yr*d2tdc2(ind) - &
                                                          term0*(2.0_dp*d2xyzdc2(ind,1)*xr*yr + d2xyzdc2(ind,2)*xr*xr) - &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,1)*dxyzdc(nc2,2) - &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,2)*dxyzdc(nc2,1) - &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,1)*dxyzdc(nc2,1) - &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,1)*xr*yr + dxyzdc(nc2,2)*xr*xr) - &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,1)*xr*yr + dxyzdc(nc1,2)*xr*xr)
                      drhoji2(ind,14) = drhoji2(ind,14) - xr*xr*zr*d2tdc2(ind) - &
                                                          term0*(2.0_dp*d2xyzdc2(ind,1)*xr*zr + d2xyzdc2(ind,3)*xr*xr) - &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,1)*dxyzdc(nc2,3) - &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,3)*dxyzdc(nc2,1) - &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,1)*dxyzdc(nc2,1) - &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,1)*xr*zr + dxyzdc(nc2,3)*xr*xr) - &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,1)*xr*zr + dxyzdc(nc1,3)*xr*xr)
                      drhoji2(ind,15) = drhoji2(ind,15) - xr*yr*yr*d2tdc2(ind) - &
                                                          term0*(2.0_dp*d2xyzdc2(ind,2)*xr*yr + d2xyzdc2(ind,1)*yr*yr) - &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,1)*dxyzdc(nc2,2) - &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,2)*dxyzdc(nc2,1) - &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,2)*dxyzdc(nc2,2) - &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,2)*xr*yr + dxyzdc(nc2,1)*yr*yr) - &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,2)*xr*yr + dxyzdc(nc1,1)*yr*yr)
                      drhoji2(ind,16) = drhoji2(ind,16) - xr*yr*zr*d2tdc2(ind) - &
                                                          term0*(d2xyzdc2(ind,1)*yr*zr + &
                                                                 d2xyzdc2(ind,2)*xr*zr + &
                                                                 d2xyzdc2(ind,3)*xr*yr) - &
                                                          term0*xr*(dxyzdc(nc1,2)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,2)) - &
                                                          term0*yr*(dxyzdc(nc1,1)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,1)) - &
                                                          term0*zr*(dxyzdc(nc1,1)*dxyzdc(nc2,2) + dxyzdc(nc1,2)*dxyzdc(nc2,1)) - &
                                                          dtdc(nc1)*(dxyzdc(nc2,1)*yr*zr + &
                                                                     dxyzdc(nc2,2)*xr*zr + &
                                                                     dxyzdc(nc2,3)*xr*yr) - &
                                                          dtdc(nc2)*(dxyzdc(nc1,1)*yr*zr + &
                                                                     dxyzdc(nc1,2)*xr*zr + &
                                                                     dxyzdc(nc1,3)*xr*yr)
                      drhoji2(ind,17) = drhoji2(ind,17) - xr*zr*zr*d2tdc2(ind) - &
                                                          term0*(2.0_dp*d2xyzdc2(ind,3)*xr*zr + d2xyzdc2(ind,1)*zr*zr) - &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,1)*dxyzdc(nc2,3) - &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,3)*dxyzdc(nc2,1) - &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,3)*dxyzdc(nc2,3) - &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,3)*xr*zr + dxyzdc(nc2,1)*zr*zr) - &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,3)*xr*zr + dxyzdc(nc1,1)*zr*zr)
                      drhoji2(ind,18) = drhoji2(ind,18) - yr*yr*yr*d2tdc2(ind) - term0*(3.0_dp*d2xyzdc2(ind,2)*yr*yr) - &
                                                          term0*(6.0_dp*dxyzdc(nc1,2)*dxyzdc(nc2,2)*yr) - &
                                                          dtdc(nc1)*(3.0_dp*dxyzdc(nc2,2)*yr*yr) - &
                                                          dtdc(nc2)*(3.0_dp*dxyzdc(nc1,2)*yr*yr)
                      drhoji2(ind,19) = drhoji2(ind,19) - yr*yr*zr*d2tdc2(ind) - &
                                                          term0*(2.0_dp*d2xyzdc2(ind,2)*yr*zr + d2xyzdc2(ind,3)*yr*yr) - &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,2)*dxyzdc(nc2,3) - &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,3)*dxyzdc(nc2,2) - &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,2)*dxyzdc(nc2,2) - &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,2)*yr*zr + dxyzdc(nc2,3)*yr*yr) - &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,2)*yr*zr + dxyzdc(nc1,3)*yr*yr)
                      drhoji2(ind,20) = drhoji2(ind,20) - yr*zr*zr*d2tdc2(ind) - &
                                                          term0*(2.0_dp*d2xyzdc2(ind,3)*yr*zr + d2xyzdc2(ind,2)*zr*zr) - &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,3)*dxyzdc(nc2,2) - &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,2)*dxyzdc(nc2,3) - &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,3)*dxyzdc(nc2,3) - &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,3)*yr*zr + dxyzdc(nc2,2)*zr*zr) - &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,3)*yr*zr + dxyzdc(nc1,2)*zr*zr)
                      drhoji2(ind,21) = drhoji2(ind,21) - zr*zr*zr*d2tdc2(ind) - term0*(3.0_dp*d2xyzdc2(ind,3)*zr*zr) - &
                                                          term0*(6.0_dp*dxyzdc(nc1,3)*dxyzdc(nc2,3)*zr) - &
                                                          dtdc(nc1)*(3.0_dp*dxyzdc(nc2,3)*zr*zr) - &
                                                          dtdc(nc2)*(3.0_dp*dxyzdc(nc1,3)*zr*zr)
                    enddo
                  enddo
                  if (lt24) then
                    ind = 0
                    do nc1 = 1,3
                      do nc2 = nc1,3
                        ind = ind + 1
                        drhoji2(ind,22) = drhoji2(ind,22) - xr*d2tdc2(ind) - term0*d2xyzdc2(ind,1) - &
                                                            dxyzdc(nc1,1)*dtdc(nc2) - dxyzdc(nc2,1)*dtdc(nc1)
                        drhoji2(ind,23) = drhoji2(ind,23) - yr*d2tdc2(ind) - term0*d2xyzdc2(ind,2) - &
                                                            dxyzdc(nc1,2)*dtdc(nc2) - dxyzdc(nc2,2)*dtdc(nc1)
                        drhoji2(ind,24) = drhoji2(ind,24) - zr*d2tdc2(ind) - term0*d2xyzdc2(ind,3) - &
                                                            dxyzdc(nc1,3)*dtdc(nc2) - dxyzdc(nc2,3)*dtdc(nc1)
                      enddo
                    enddo
                  endif
                endif
                if (lstr) then
                  ind = 0
                  do ns1 = 1,6
                    do ns2 = 1,ns1
                      ind = ind + 1
                      d2tds2(ind) = term2*rpd(ns1)*rpd(ns2)
                    enddo
                    d2tdsdc(ns1,1) = term2*rpd(ns1)*xl
                    d2tdsdc(ns1,2) = term2*rpd(ns1)*yl
                    d2tdsdc(ns1,3) = term2*rpd(ns1)*zl
                  enddo
                  if (order.eq.1) then
                    do ns1 = 1,21
                      drhoji2s(ns1,1) = drhoji2s(ns1,1) + d2tds2(ns1)
                    enddo
!
                    drhoji2s(1,1)  = drhoji2s(1,1)  + 2.0_dp*dtds(1)
                    drhoji2s(3,1)  = drhoji2s(3,1)  + 2.0_dp*dtds(2)
                    drhoji2s(6,1)  = drhoji2s(6,1)  + 2.0_dp*dtds(3)
                    drhoji2s(8,1)  = drhoji2s(8,1)  + dtds(4)
                    drhoji2s(9,1)  = drhoji2s(9,1)  + dtds(4)
                    drhoji2s(10,1) = drhoji2s(10,1) + 0.5_dp*(dtds(2)+dtds(3))
                    drhoji2s(11,1) = drhoji2s(11,1) + dtds(5)
                    drhoji2s(13,1) = drhoji2s(13,1) + dtds(5)
                    drhoji2s(14,1) = drhoji2s(14,1) + 0.5_dp*dtds(6)
                    drhoji2s(15,1) = drhoji2s(15,1) + 0.5_dp*(dtds(1)+dtds(3))
                    drhoji2s(16,1) = drhoji2s(16,1) + dtds(6)
                    drhoji2s(17,1) = drhoji2s(17,1) + dtds(6)
                    drhoji2s(19,1) = drhoji2s(19,1) + 0.5_dp*dtds(5)
                    drhoji2s(20,1) = drhoji2s(20,1) + 0.5_dp*dtds(4)
                    drhoji2s(21,1) = drhoji2s(21,1) + 0.5_dp*(dtds(1)+dtds(2))
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoji2m(ns1,nc1,1) = drhoji2m(ns1,nc1,1) + d2tdsdc(ns1,nc1) + 2.0_dp*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                  elseif (order.eq.2) then
                    ind = 0
                    do ns1 = 1,6
                      do ns2 = 1,ns1
                        ind = ind + 1
                        drhoji2s(ind,2) = drhoji2s(ind,2) - xr*d2tds2(ind) - term0*d2xyzds2(ind,1) - &
                                                            dxyzds(ns1,1)*dtds(ns2) - dxyzds(ns2,1)*dtds(ns1)
                        drhoji2s(ind,3) = drhoji2s(ind,3) - yr*d2tds2(ind) - term0*d2xyzds2(ind,2) - &
                                                            dxyzds(ns1,2)*dtds(ns2) - dxyzds(ns2,2)*dtds(ns1)
                        drhoji2s(ind,4) = drhoji2s(ind,4) - zr*d2tds2(ind) - term0*d2xyzds2(ind,3) - &
                                                            dxyzds(ns1,3)*dtds(ns2) - dxyzds(ns2,3)*dtds(ns1)
                      enddo
                    enddo
!
                    do nord = 2,4
                      drhoji2s(1,nord)  = drhoji2s(1,nord)  - 2.0_dp*ordtrm(nord)*dtds(1)
                      drhoji2s(3,nord)  = drhoji2s(3,nord)  - 2.0_dp*ordtrm(nord)*dtds(2)
                      drhoji2s(6,nord)  = drhoji2s(6,nord)  - 2.0_dp*ordtrm(nord)*dtds(3)
                      drhoji2s(8,nord)  = drhoji2s(8,nord)  - ordtrm(nord)*dtds(4)
                      drhoji2s(9,nord)  = drhoji2s(9,nord)  - ordtrm(nord)*dtds(4)
                      drhoji2s(10,nord) = drhoji2s(10,nord) - 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                      drhoji2s(11,nord) = drhoji2s(11,nord) - ordtrm(nord)*dtds(5)
                      drhoji2s(13,nord) = drhoji2s(13,nord) - ordtrm(nord)*dtds(5)
                      drhoji2s(14,nord) = drhoji2s(14,nord) - 0.5_dp*ordtrm(nord)*dtds(6)
                      drhoji2s(15,nord) = drhoji2s(15,nord) - 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                      drhoji2s(16,nord) = drhoji2s(16,nord) - ordtrm(nord)*dtds(6)
                      drhoji2s(17,nord) = drhoji2s(17,nord) - ordtrm(nord)*dtds(6)
                      drhoji2s(19,nord) = drhoji2s(19,nord) - 0.5_dp*ordtrm(nord)*dtds(5)
                      drhoji2s(20,nord) = drhoji2s(20,nord) - 0.5_dp*ordtrm(nord)*dtds(4)
                      drhoji2s(21,nord) = drhoji2s(21,nord) - 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                    enddo
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoji2m(ns1,nc1,2) = drhoji2m(ns1,nc1,2) - xr*d2tdsdc(ns1,nc1) - term0*d2xyzdsdc(ns1,nc1,1) - &
                                                                    dxyzds(ns1,1)*dtdc(nc1) - dxyzdc(nc1,1)*dtds(ns1) - &
                                                                    2.0_dp*xr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,3) = drhoji2m(ns1,nc1,3) - yr*d2tdsdc(ns1,nc1) - term0*d2xyzdsdc(ns1,nc1,2) - &
                                                                    dxyzds(ns1,2)*dtdc(nc1) - dxyzdc(nc1,2)*dtds(ns1) - &
                                                                    2.0_dp*yr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,4) = drhoji2m(ns1,nc1,4) - zr*d2tdsdc(ns1,nc1) - term0*d2xyzdsdc(ns1,nc1,3) - &
                                                                    dxyzds(ns1,3)*dtdc(nc1) - dxyzdc(nc1,3)*dtds(ns1) - &
                                                                    2.0_dp*zr*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                  elseif (order.eq.3) then
                    ind = 0
                    do ns1 = 1,6
                      do ns2 = 1,ns1
                        ind = ind + 1
                        drhoji2s(ind,5)  = drhoji2s(ind,5)  + xr*xr*d2tds2(ind) + term0*(2.0_dp*d2xyzds2(ind,1)*xr) + &
                                                              term0*(2.0_dp*dxyzds(ns1,1)*dxyzds(ns2,1)) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,1)*xr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,1)*xr)
                        drhoji2s(ind,6)  = drhoji2s(ind,6)  + xr*yr*d2tds2(ind) + term0*(d2xyzds2(ind,1)*yr + d2xyzds2(ind,2)*xr)+&
                                                              term0*(dxyzds(ns1,1)*dxyzds(ns2,2) + dxyzds(ns1,2)*dxyzds(ns2,1)) + &
                                                              dtds(ns1)*(dxyzds(ns2,1)*yr + dxyzds(ns2,2)*xr) + &
                                                              dtds(ns2)*(dxyzds(ns1,1)*yr + dxyzds(ns1,2)*xr)
                        drhoji2s(ind,7)  = drhoji2s(ind,7)  + xr*zr*d2tds2(ind) + term0*(d2xyzds2(ind,1)*zr + d2xyzds2(ind,3)*xr)+&
                                                              term0*(dxyzds(ns1,1)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,1)) + &
                                                              dtds(ns1)*(dxyzds(ns2,1)*zr + dxyzds(ns2,3)*xr) + &
                                                              dtds(ns2)*(dxyzds(ns1,1)*zr + dxyzds(ns1,3)*xr)
                        drhoji2s(ind,8)  = drhoji2s(ind,8)  + yr*yr*d2tds2(ind) + term0*(2.0_dp*d2xyzds2(ind,2)*yr) + &
                                                              term0*(2.0_dp*dxyzds(ns1,2)*dxyzds(ns2,2)) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,2)*yr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,2)*yr)
                        drhoji2s(ind,9)  = drhoji2s(ind,9)  + yr*zr*d2tds2(ind) + term0*(d2xyzds2(ind,2)*zr + d2xyzds2(ind,3)*yr)+&
                                                              term0*(dxyzds(ns1,2)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,2)) + &
                                                              dtds(ns1)*(dxyzds(ns2,2)*zr + dxyzds(ns2,3)*yr) + &
                                                              dtds(ns2)*(dxyzds(ns1,2)*zr + dxyzds(ns1,3)*yr)
                        drhoji2s(ind,10) = drhoji2s(ind,10) + zr*zr*d2tds2(ind) + term0*(2.0_dp*d2xyzds2(ind,3)*zr) + &
                                                              term0*(2.0_dp*dxyzds(ns1,3)*dxyzds(ns2,3)) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,3)*zr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,3)*zr)
                        drhoji2s(ind,11) = drhoji2s(ind,11) + d2tds2(ind)
                      enddo
                    enddo
!
                    do nord = 5,11
                      drhoji2s(1,nord)  = drhoji2s(1,nord)  + 2.0_dp*ordtrm(nord)*dtds(1)
                      drhoji2s(3,nord)  = drhoji2s(3,nord)  + 2.0_dp*ordtrm(nord)*dtds(2)
                      drhoji2s(6,nord)  = drhoji2s(6,nord)  + 2.0_dp*ordtrm(nord)*dtds(3)
                      drhoji2s(8,nord)  = drhoji2s(8,nord)  + ordtrm(nord)*dtds(4)
                      drhoji2s(9,nord)  = drhoji2s(9,nord)  + ordtrm(nord)*dtds(4)
                      drhoji2s(10,nord) = drhoji2s(10,nord) + 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                      drhoji2s(11,nord) = drhoji2s(11,nord) + ordtrm(nord)*dtds(5)
                      drhoji2s(13,nord) = drhoji2s(13,nord) + ordtrm(nord)*dtds(5)
                      drhoji2s(14,nord) = drhoji2s(14,nord) + 0.5_dp*ordtrm(nord)*dtds(6)
                      drhoji2s(15,nord) = drhoji2s(15,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                      drhoji2s(16,nord) = drhoji2s(16,nord) + ordtrm(nord)*dtds(6)
                      drhoji2s(17,nord) = drhoji2s(17,nord) + ordtrm(nord)*dtds(6)
                      drhoji2s(19,nord) = drhoji2s(19,nord) + 0.5_dp*ordtrm(nord)*dtds(5)
                      drhoji2s(20,nord) = drhoji2s(20,nord) + 0.5_dp*ordtrm(nord)*dtds(4)
                      drhoji2s(21,nord) = drhoji2s(21,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                    enddo
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoji2m(ns1,nc1,5)  = drhoji2m(ns1,nc1,5)  + xr*xr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,1)*xr) + &
                                                                      term0*(2.0_dp*dxyzds(ns1,1)*dxyzdc(nc1,1)) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,1)*xr) + &
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,1)*xr) + &
                                                                      2.0_dp*xr*xr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,6)  = drhoji2m(ns1,nc1,6)  + xr*yr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(d2xyzdsdc(ns1,nc1,1)*yr + d2xyzdsdc(ns1,nc1,2)*xr) + &
                                                                      term0*(dxyzds(ns1,1)*dxyzdc(nc1,2) + &
                                                                             dxyzds(ns1,2)*dxyzdc(nc1,1)) + &
                                                                      dtds(ns1)*(dxyzdc(nc1,1)*yr + dxyzdc(nc1,2)*xr) + &
                                                                      dtdc(nc1)*(dxyzds(ns1,1)*yr + dxyzds(ns1,2)*xr) + &
                                                                      2.0_dp*xr*yr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,7)  = drhoji2m(ns1,nc1,7)  + xr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(d2xyzdsdc(ns1,nc1,1)*zr + d2xyzdsdc(ns1,nc1,3)*xr) + &
                                                                      term0*(dxyzds(ns1,1)*dxyzdc(nc1,3) + &
                                                                             dxyzds(ns1,3)*dxyzdc(nc1,1)) + &
                                                                      dtds(ns1)*(dxyzdc(nc1,1)*zr + dxyzdc(nc1,3)*xr) + &
                                                                      dtdc(nc1)*(dxyzds(ns1,1)*zr + dxyzds(ns1,3)*xr) + &
                                                                      2.0_dp*xr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,8)  = drhoji2m(ns1,nc1,8)  + yr*yr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,2)*yr) + &
                                                                      term0*(2.0_dp*dxyzds(ns1,2)*dxyzdc(nc1,2)) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,2)*yr) + &
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,2)*yr) + &
                                                                      2.0_dp*yr*yr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,9)  = drhoji2m(ns1,nc1,9)  + yr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(d2xyzdsdc(ns1,nc1,2)*zr + d2xyzdsdc(ns1,nc1,3)*yr) + &
                                                                      term0*(dxyzds(ns1,2)*dxyzdc(nc1,3) + &
                                                                             dxyzds(ns1,3)*dxyzdc(nc1,2)) + &
                                                                      dtds(ns1)*(dxyzdc(nc1,2)*zr + dxyzdc(nc1,3)*yr) + &
                                                                      dtdc(nc1)*(dxyzds(ns1,2)*zr + dxyzds(ns1,3)*yr) + &
                                                                      2.0_dp*yr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,10) = drhoji2m(ns1,nc1,10) + zr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,3)*zr) + &
                                                                      term0*(2.0_dp*dxyzds(ns1,3)*dxyzdc(nc1,3)) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,3)*zr) + &
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,3)*zr) + &
                                                                      2.0_dp*zr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,11) = drhoji2m(ns1,nc1,11) + d2tdsdc(ns1,nc1) + 2.0_dp*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                  elseif (order.eq.4) then
                    ind = 0
                    do ns1 = 1,6
                      do ns2 = 1,ns1
                        ind = ind + 1
                        drhoji2s(ind,12) = drhoji2s(ind,12) - xr*xr*xr*d2tds2(ind) - term0*(3.0_dp*d2xyzds2(ind,1)*xr*xr) - &
                                                              term0*(6.0_dp*dxyzds(ns1,1)*dxyzds(ns2,1)*xr) - &
                                                              dtds(ns1)*(3.0_dp*dxyzds(ns2,1)*xr*xr) - &
                                                              dtds(ns2)*(3.0_dp*dxyzds(ns1,1)*xr*xr)
                        drhoji2s(ind,13) = drhoji2s(ind,13) - xr*xr*yr*d2tds2(ind) - &
                                                              term0*(2.0_dp*d2xyzds2(ind,1)*xr*yr + d2xyzds2(ind,2)*xr*xr) - &
                                                              2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzds(ns2,2) - &
                                                              2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzds(ns2,1) - &
                                                              2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzds(ns2,1) - &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,1)*xr*yr + dxyzds(ns2,2)*xr*xr) - &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,1)*xr*yr + dxyzds(ns1,2)*xr*xr)
                        drhoji2s(ind,14) = drhoji2s(ind,14) - xr*xr*zr*d2tds2(ind) - &
                                                              term0*(2.0_dp*d2xyzds2(ind,1)*xr*zr + d2xyzds2(ind,3)*xr*xr) - &
                                                              2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzds(ns2,3) - &
                                                              2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzds(ns2,1) - &
                                                              2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzds(ns2,1) - &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,1)*xr*zr + dxyzds(ns2,3)*xr*xr) - &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,1)*xr*zr + dxyzds(ns1,3)*xr*xr)
                        drhoji2s(ind,15) = drhoji2s(ind,15) - xr*yr*yr*d2tds2(ind) - &
                                                              term0*(2.0_dp*d2xyzds2(ind,2)*xr*yr + d2xyzds2(ind,1)*yr*yr) - &
                                                              2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzds(ns2,2) - &
                                                              2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzds(ns2,1) - &
                                                              2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzds(ns2,2) - &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,2)*xr*yr + dxyzds(ns2,1)*yr*yr) - &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,2)*xr*yr + dxyzds(ns1,1)*yr*yr)
                        drhoji2s(ind,16) = drhoji2s(ind,16) - xr*yr*zr*d2tds2(ind) - &
                                                              term0*(d2xyzds2(ind,1)*yr*zr + &
                                                                     d2xyzds2(ind,2)*xr*zr + &
                                                                     d2xyzds2(ind,3)*xr*yr) - &
                                                              term0*xr*(dxyzds(ns1,2)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,2))-&
                                                              term0*yr*(dxyzds(ns1,1)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,1))-&
                                                              term0*zr*(dxyzds(ns1,1)*dxyzds(ns2,2) + dxyzds(ns1,2)*dxyzds(ns2,1))-&
                                                              dtds(ns1)*(dxyzds(ns2,1)*yr*zr + &
                                                                         dxyzds(ns2,2)*xr*zr + &
                                                                         dxyzds(ns2,3)*xr*yr) - &
                                                              dtds(ns2)*(dxyzds(ns1,1)*yr*zr + &
                                                                         dxyzds(ns1,2)*xr*zr + &
                                                                         dxyzds(ns1,3)*xr*yr)
                        drhoji2s(ind,17) = drhoji2s(ind,17) - xr*zr*zr*d2tds2(ind) - &
                                                              term0*(2.0_dp*d2xyzds2(ind,3)*xr*zr + d2xyzds2(ind,1)*zr*zr) - &
                                                              2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzds(ns2,3) - &
                                                              2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzds(ns2,1) - &
                                                              2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzds(ns2,3) - &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,3)*xr*zr + dxyzds(ns2,1)*zr*zr) - &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,3)*xr*zr + dxyzds(ns1,1)*zr*zr)
                        drhoji2s(ind,18) = drhoji2s(ind,18) - yr*yr*yr*d2tds2(ind) - term0*(3.0_dp*d2xyzds2(ind,2)*yr*yr) - &
                                                              term0*(6.0_dp*dxyzds(ns1,2)*dxyzds(ns2,2)*yr) - &
                                                              dtds(ns1)*(3.0_dp*dxyzds(ns2,2)*yr*yr) - &
                                                              dtds(ns2)*(3.0_dp*dxyzds(ns1,2)*yr*yr)
                        drhoji2s(ind,19) = drhoji2s(ind,19) - yr*yr*zr*d2tds2(ind) - &
                                                              term0*(2.0_dp*d2xyzds2(ind,2)*yr*zr + d2xyzds2(ind,3)*yr*yr) - &
                                                              2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzds(ns2,3) - &
                                                              2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzds(ns2,2) - &
                                                              2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzds(ns2,2) - &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,2)*yr*zr + dxyzds(ns2,3)*yr*yr) - &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,2)*yr*zr + dxyzds(ns1,3)*yr*yr)
                        drhoji2s(ind,20) = drhoji2s(ind,20) - yr*zr*zr*d2tds2(ind) - &
                                                              term0*(2.0_dp*d2xyzds2(ind,3)*yr*zr + d2xyzds2(ind,2)*zr*zr) - &
                                                              2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzds(ns2,2) - &
                                                              2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzds(ns2,3) - &
                                                              2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzds(ns2,3) - &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,3)*yr*zr + dxyzds(ns2,2)*zr*zr) - &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,3)*yr*zr + dxyzds(ns1,2)*zr*zr)
                        drhoji2s(ind,21) = drhoji2s(ind,21) - zr*zr*zr*d2tds2(ind) - term0*(3.0_dp*d2xyzds2(ind,3)*zr*zr) - &
                                                              term0*(6.0_dp*dxyzds(ns1,3)*dxyzds(ns2,3)*zr) - &
                                                              dtds(ns1)*(3.0_dp*dxyzds(ns2,3)*zr*zr) - &
                                                              dtds(ns2)*(3.0_dp*dxyzds(ns1,3)*zr*zr)
                      enddo
                    enddo
!
                    do nord = 12,21
                      drhoji2s(1,nord)  = drhoji2s(1,nord)  - 2.0_dp*ordtrm(nord)*dtds(1)
                      drhoji2s(3,nord)  = drhoji2s(3,nord)  - 2.0_dp*ordtrm(nord)*dtds(2)
                      drhoji2s(6,nord)  = drhoji2s(6,nord)  - 2.0_dp*ordtrm(nord)*dtds(3)
                      drhoji2s(8,nord)  = drhoji2s(8,nord)  - ordtrm(nord)*dtds(4)
                      drhoji2s(9,nord)  = drhoji2s(9,nord)  - ordtrm(nord)*dtds(4)
                      drhoji2s(10,nord) = drhoji2s(10,nord) - 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                      drhoji2s(11,nord) = drhoji2s(11,nord) - ordtrm(nord)*dtds(5)
                      drhoji2s(13,nord) = drhoji2s(13,nord) - ordtrm(nord)*dtds(5)
                      drhoji2s(14,nord) = drhoji2s(14,nord) - 0.5_dp*ordtrm(nord)*dtds(6)
                      drhoji2s(15,nord) = drhoji2s(15,nord) - 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                      drhoji2s(16,nord) = drhoji2s(16,nord) - ordtrm(nord)*dtds(6)
                      drhoji2s(17,nord) = drhoji2s(17,nord) - ordtrm(nord)*dtds(6)
                      drhoji2s(19,nord) = drhoji2s(19,nord) - 0.5_dp*ordtrm(nord)*dtds(5)
                      drhoji2s(20,nord) = drhoji2s(20,nord) - 0.5_dp*ordtrm(nord)*dtds(4)
                      drhoji2s(21,nord) = drhoji2s(21,nord) - 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                    enddo
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoji2m(ns1,nc1,12) = drhoji2m(ns1,nc1,12) - xr*xr*xr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(3.0_dp*d2xyzdsdc(ns1,nc1,1)*xr*xr) - &
                                                                      term0*(6.0_dp*dxyzds(ns1,1)*dxyzdc(nc1,1)*xr) - &
                                                                      dtds(ns1)*(3.0_dp*dxyzdc(nc1,1)*xr*xr) - &
                                                                      dtdc(nc1)*(3.0_dp*dxyzds(ns1,1)*xr*xr) - &
                                                                      2.0_dp*xr*xr*xr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,13) = drhoji2m(ns1,nc1,13) - xr*xr*yr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,1)*xr*yr + &
                                                                             d2xyzdsdc(ns1,nc1,2)*xr*xr) - &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzdc(nc1,2) - &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzdc(nc1,1) - &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzdc(nc1,1) - &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,1)*xr*yr + dxyzdc(nc1,2)*xr*xr)-&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,1)*xr*yr + dxyzds(ns1,2)*xr*xr)-&
                                                                      2.0_dp*xr*xr*yr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,14) = drhoji2m(ns1,nc1,14) - xr*xr*zr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,1)*xr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,3)*xr*xr) - &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzdc(nc1,3) - &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzdc(nc1,1) - &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzdc(nc1,1) - &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,1)*xr*zr + dxyzdc(nc1,3)*xr*xr)-&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,1)*xr*zr + dxyzds(ns1,3)*xr*xr)-&
                                                                      2.0_dp*xr*xr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,15) = drhoji2m(ns1,nc1,15) - xr*yr*yr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,2)*xr*yr + &
                                                                             d2xyzdsdc(ns1,nc1,1)*yr*yr) - &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzdc(nc1,2) - &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzdc(nc1,1) - &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzdc(nc1,2) - &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,2)*xr*yr + dxyzdc(nc1,1)*yr*yr)-&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,2)*xr*yr + dxyzds(ns1,1)*yr*yr)-&
                                                                      2.0_dp*xr*yr*yr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,16) = drhoji2m(ns1,nc1,16) - xr*yr*zr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(d2xyzdsdc(ns1,nc1,1)*yr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,2)*xr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,3)*xr*yr) - &
                                                                      term0*xr*(dxyzds(ns1,2)*dxyzdc(nc1,3) + &
                                                                                dxyzds(ns1,3)*dxyzdc(nc1,2)) - &
                                                                      term0*yr*(dxyzds(ns1,1)*dxyzdc(nc1,3) + &
                                                                                dxyzds(ns1,3)*dxyzdc(nc1,1)) - &
                                                                      term0*zr*(dxyzds(ns1,1)*dxyzdc(nc1,2) + &
                                                                                dxyzds(ns1,2)*dxyzdc(nc1,1)) - &
                                                                      dtds(ns1)*(dxyzdc(nc1,1)*yr*zr + &
                                                                                 dxyzdc(nc1,2)*xr*zr + &
                                                                                 dxyzdc(nc1,3)*xr*yr) - &
                                                                      dtdc(nc1)*(dxyzds(ns1,1)*yr*zr + &
                                                                                 dxyzds(ns1,2)*xr*zr + &
                                                                                 dxyzds(ns1,3)*xr*yr) - &
                                                                      2.0_dp*xr*yr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,17) = drhoji2m(ns1,nc1,17) - xr*zr*zr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,3)*xr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,1)*zr*zr) - &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzdc(nc1,3) - &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzdc(nc1,1) - &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzdc(nc1,3) - &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,3)*xr*zr + dxyzdc(nc1,1)*zr*zr)-&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,3)*xr*zr + dxyzds(ns1,1)*zr*zr)-&
                                                                      2.0_dp*xr*zr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,18) = drhoji2m(ns1,nc1,18) - yr*yr*yr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(3.0_dp*d2xyzdsdc(ns1,nc1,2)*yr*yr) - &
                                                                      term0*(6.0_dp*dxyzds(ns1,2)*dxyzdc(nc1,2)*yr) - &
                                                                      dtds(ns1)*(3.0_dp*dxyzdc(nc1,2)*yr*yr) - &
                                                                      dtdc(nc1)*(3.0_dp*dxyzds(ns1,2)*yr*yr) - &
                                                                      2.0_dp*yr*yr*yr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,19) = drhoji2m(ns1,nc1,19) - yr*yr*zr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,2)*yr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,3)*yr*yr) - &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzdc(nc1,3) - &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzdc(nc1,2) - &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzdc(nc1,2) - &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,2)*yr*zr + dxyzdc(nc1,3)*yr*yr)-&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,2)*yr*zr + dxyzds(ns1,3)*yr*yr)-&
                                                                      2.0_dp*yr*yr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,20) = drhoji2m(ns1,nc1,20) - yr*zr*zr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,3)*yr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,2)*zr*zr) - &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzdc(nc1,2) - &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzdc(nc1,3) - &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzdc(nc1,3) - &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,3)*yr*zr + dxyzdc(nc1,2)*zr*zr)-&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,3)*yr*zr + dxyzds(ns1,2)*zr*zr)-&
                                                                      2.0_dp*yr*zr*zr*term1*dxyznords(ns1,nc1)
                        drhoji2m(ns1,nc1,21) = drhoji2m(ns1,nc1,21) - zr*zr*zr*d2tdsdc(ns1,nc1) - &
                                                                      term0*(3.0_dp*d2xyzdsdc(ns1,nc1,3)*zr*zr) - &
                                                                      term0*(6.0_dp*dxyzds(ns1,3)*dxyzdc(nc1,3)*zr) - &
                                                                      dtds(ns1)*(3.0_dp*dxyzdc(nc1,3)*zr*zr) - &
                                                                      dtdc(nc1)*(3.0_dp*dxyzds(ns1,3)*zr*zr) - &
                                                                      2.0_dp*zr*zr*zr*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                    if (lt24) then
                      ind = 0
                      do ns1 = 1,6
                        do ns2 = 1,ns1
                          ind = ind + 1
                          drhoji2s(ind,22) = drhoji2s(ind,22) - xr*d2tds2(ind) - term0*d2xyzds2(ind,1) - &
                                                                dxyzds(ns1,1)*dtds(ns2) - dxyzds(ns2,1)*dtds(ns1)
                          drhoji2s(ind,23) = drhoji2s(ind,23) - yr*d2tds2(ind) - term0*d2xyzds2(ind,2) - &
                                                                dxyzds(ns1,2)*dtds(ns2) - dxyzds(ns2,2)*dtds(ns1)
                          drhoji2s(ind,24) = drhoji2s(ind,24) - zr*d2tds2(ind) - term0*d2xyzds2(ind,3) - &
                                                                dxyzds(ns1,3)*dtds(ns2) - dxyzds(ns2,3)*dtds(ns1)
                        enddo
                      enddo
!
                      do nord = 22,24
                        drhoji2s(1,nord)  = drhoji2s(1,nord)  - 2.0_dp*ordtrm(nord)*dtds(1)
                        drhoji2s(3,nord)  = drhoji2s(3,nord)  - 2.0_dp*ordtrm(nord)*dtds(2)
                        drhoji2s(6,nord)  = drhoji2s(6,nord)  - 2.0_dp*ordtrm(nord)*dtds(3)
                        drhoji2s(8,nord)  = drhoji2s(8,nord)  - ordtrm(nord)*dtds(4)
                        drhoji2s(9,nord)  = drhoji2s(9,nord)  - ordtrm(nord)*dtds(4)
                        drhoji2s(10,nord) = drhoji2s(10,nord) - 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                        drhoji2s(11,nord) = drhoji2s(11,nord) - ordtrm(nord)*dtds(5)
                        drhoji2s(13,nord) = drhoji2s(13,nord) - ordtrm(nord)*dtds(5)
                        drhoji2s(14,nord) = drhoji2s(14,nord) - 0.5_dp*ordtrm(nord)*dtds(6)
                        drhoji2s(15,nord) = drhoji2s(15,nord) - 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                        drhoji2s(16,nord) = drhoji2s(16,nord) - ordtrm(nord)*dtds(6)
                        drhoji2s(17,nord) = drhoji2s(17,nord) - ordtrm(nord)*dtds(6)
                        drhoji2s(19,nord) = drhoji2s(19,nord) - 0.5_dp*ordtrm(nord)*dtds(5)
                        drhoji2s(20,nord) = drhoji2s(20,nord) - 0.5_dp*ordtrm(nord)*dtds(4)
                        drhoji2s(21,nord) = drhoji2s(21,nord) - 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                      enddo
!
                      do nc1 = 1,3
                        do ns1 = 1,6
                          drhoji2m(ns1,nc1,22) = drhoji2m(ns1,nc1,22) - xr*d2tdsdc(ns1,nc1) - term0*d2xyzdsdc(ns1,nc1,1) - &
                                                                        dxyzds(ns1,1)*dtdc(nc1) - dxyzdc(nc1,1)*dtds(ns1) - &
                                                                        2.0_dp*xr*term1*dxyznords(ns1,nc1)
                          drhoji2m(ns1,nc1,23) = drhoji2m(ns1,nc1,23) - yr*d2tdsdc(ns1,nc1) - term0*d2xyzdsdc(ns1,nc1,2) - &
                                                                        dxyzds(ns1,2)*dtdc(nc1) - dxyzdc(nc1,2)*dtds(ns1) - &
                                                                        2.0_dp*yr*term1*dxyznords(ns1,nc1)
                          drhoji2m(ns1,nc1,24) = drhoji2m(ns1,nc1,24) - zr*d2tdsdc(ns1,nc1) - term0*d2xyzdsdc(ns1,nc1,3) - &
                                                                        dxyzds(ns1,3)*dtdc(nc1) - dxyzdc(nc1,3)*dtds(ns1) - &
                                                                        2.0_dp*zr*term1*dxyznords(ns1,nc1)
                        enddo
                      enddo
                    endif
                  endif
                endif
              endif
            endif
!
!  End loop over MEAM order
!
          enddo
        enddo
      endif
    endif
!
!  Terms for j->i
!
    if (natj.eq.neamnat(mm).and.(ntypj.eq.neamtyp(mm).or.neamtyp(mm).eq.0)) then
      if (neamnat2(mm).eq.0.or.(neamnat2(mm).eq.nati.and.(ntypi.eq.neamtyp2(mm).or.neamtyp2(mm).eq.0))) then
        do j = 1,ndenfncomp(mm)
!
!  Loop over MEAM order
!
          do order = 1,neammeamorder(j,mm)
            rhoijloc = 0.0_dp
            drhoijloc = 0.0_dp
            drhoij2loc = 0.0_dp
!
!  Density functional forms
!
            npt = nint(denpar(6,order,j,mm))
            if (ndenfn(j,mm).eq.1) then
!
!  Power law
!
              trm1 = oci*denpar(1,order,j,mm)*(rk**npt)
              rhoijloc = trm1
              if (lgrad1) then
                trm1 = - dble(npt)*trm1*rk2
                drhoijloc = trm1
                if (lgrad2) then
                  trm2 = - trm1*dble(npt+2)*rk2
                  drhoij2loc = trm2
                endif
              endif
            elseif (ndenfn(j,mm).eq.2) then
!
!  Exponential
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              cpt = denpar(3,order,j,mm)
              trm1 = apt*ocj*exp(-bpt*(r-cpt))
              if (npt.ne.0) then
                rn1 = r**(npt-1)
                rhoijloc = trm1*rn1*r
                if (lgrad1) then
                  drhoijloc = trm1*rn1*(npt*rk-bpt)
                  if (lgrad2) then
                    drhoij2loc = trm1*rn1*rk*(npt*(npt-1)*rk2-bpt*dble(2*npt-1)*rk+bpt*bpt)
                  endif
                endif
              else
                rhoijloc = trm1
                if (lgrad1) then
                  drhoijloc = - bpt*trm1*rk
                  if (lgrad2) then
                    drhoij2loc = bpt*trm1*rk2*(bpt+rk)
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.3) then
!
!  Gaussian
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              cpt = denpar(3,order,j,mm)
              rd = (r - cpt)
              rd2 = rd*rd
              trm1 = apt*ocj*exp(-bpt*rd2)
              if (npt.ne.0) then
                rn1 = r**(npt-1)
                rhoijloc = trm1*rn1*r
                if (lgrad1) then
                  drhoijloc = trm1*rn1*(npt*rk-2.0_dp*bpt*rd)
                  if (lgrad2) then
                    drhoij2loc = trm1*rn1*rk*(npt*(npt-2)*rk2-2.0_dp*bpt*rd*dble(2*npt-1)*rk+2.0_dp*bpt*( &
                      2.0_dp*bpt*rd2-1.0_dp))
                  endif
                endif
              else
                rhoijloc = trm1
                if (lgrad1) then
                  drhoijloc = - 2.0_dp*bpt*rd*trm1*rk
                  if (lgrad2) then
                    drhoij2loc = 2.0_dp*trm1*rk2*bpt*(rd*rk-1.0_dp+4.0_dp*bpt*rd2)
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.4) then
!
!  Cubic
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              if (r.lt.bpt) then
                rd = (r - bpt)
                trm1 = 3.0_dp*apt*rk
                rhoijloc = apt*rd*rd*rd
                if (lgrad1) then
                  drhoijloc = trm1*rd*rd
                  if (lgrad2) then
                    trm1 = trm1*rk
                    drhoij2loc = trm1*rd*(2.0_dp - rd*rk)
                  endif
                endif
              else
                rhoijloc = 0.0_dp
                if (lgrad1) then
                  drhoijloc = 0.0_dp
                  if (lgrad2) then
                    drhoij2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.5) then
!
!  Voter-Chen
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              etrm = exp(-bpt*r)
              cpt = 2.0_dp**9
              trm1 = apt*etrm*(1.0_dp + cpt*etrm)
              trm2 = apt*etrm*(1.0_dp + 2.0_dp*cpt*etrm)
              rhoijloc = r2*r2*r2*trm1
              if (lgrad1) then
                drhoijloc = r2*r2*(6.0_dp*trm1 - bpt*r*trm2)
                if (lgrad2) then
                  trm3 = apt*etrm*(1.0_dp + 4.0_dp*cpt*etrm)
                  drhoij2loc = r2*(24.0_dp*trm1 - 11.0_dp*bpt*r*trm2 + bpt*bpt*r2*trm3)
                endif
              endif
            elseif (ndenfn(j,mm).eq.6) then
!
!  Quadratic
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              if (r.lt.bpt) then
                rd = (r - bpt)
                trm1 = 2.0_dp*apt*rk
                rhoijloc = apt*rd*rd
                if (lgrad1) then
                  drhoijloc = trm1*rd
                  if (lgrad2) then
                    trm1 = trm1*rk
                    drhoij2loc = trm1*(1.0_dp - rd*rk)
                  endif
                endif
              else
                rhoijloc = 0.0_dp
                if (lgrad1) then
                  drhoijloc = 0.0_dp
                  if (lgrad2) then
                    drhoij2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.7) then
!
!  Quartic
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              if (r.lt.bpt) then
                rd = (r - bpt)
                trm1 = 4.0_dp*apt*rk*rd
                rhoijloc = apt*rd*rd*rd*rd
                if (lgrad1) then
                  drhoijloc = trm1*rd*rd
                  if (lgrad2) then
                    trm1 = trm1*rk
                    drhoij2loc = trm1*rd*(3.0_dp - rd*rk)
                  endif
                endif
              else
                rhoijloc = 0.0_dp
                if (lgrad1) then
                  drhoijloc = 0.0_dp
                  if (lgrad2) then
                    drhoij2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.8) then
!
!  Glue
!
              if (r.lt.denpar(1,order,j,mm)) then
                dr = (r - denpar(1,order,j,mm))
                dr2 = dr*dr
                dr3 = dr2*dr
                trm1 = 3.0_dp*denpar(4,order,j,mm)*dr2 + 2.0_dp*denpar(5,order,j,mm)*dr + denpar(6,order,j,mm)
                rhoijloc = denpar(4,order,j,mm)*dr3 + denpar(5,order,j,mm)*dr2 + denpar(6,order,j,mm)*dr + denpar(7,order,j,mm)
                if (lgrad1) then
                  drhoijloc = trm1*rk
                  if (lgrad2) then
                    trm2 = 6.0_dp*denpar(4,order,j,mm)*dr + 2.0_dp*denpar(5,order,j,mm)
                    drhoij2loc = rk2*(trm2 - rk*trm1)
                  endif
                endif
              elseif (r.lt.denpar(2,order,j,mm)) then
                dr = (r - denpar(1,order,j,mm))
                dr2 = dr*dr
                dr3 = dr2*dr
                trm1 = 3.0_dp*denpar(8,order,j,mm)*dr2 + 2.0_dp*denpar(9,order,j,mm)*dr + denpar(10,order,j,mm)
                rhoijloc = denpar(8,order,j,mm)*dr3 + denpar(9,order,j,mm)*dr2 + denpar(10,order,j,mm)*dr + denpar(11,order,j,mm)
                if (lgrad1) then
                  drhoijloc = trm1*rk
                  if (lgrad2) then
                    trm2 = 6.0_dp*denpar(8,order,j,mm)*dr + 2.0_dp*denpar(9,order,j,mm)
                    drhoij2loc = rk2*(trm2 - rk*trm1)
                  endif
                endif
              elseif (r.lt.denpar(3,order,j,mm)) then
                dr = (r - denpar(3,order,j,mm))
                dr2 = dr*dr
                dr3 = dr2*dr
                trm1 = 3.0_dp*denpar(12,order,j,mm)*dr2 + 2.0_dp*denpar(13,order,j,mm)*dr + denpar(14,order,j,mm)
                rhoijloc = denpar(12,order,j,mm)*dr3 + denpar(13,order,j,mm)*dr2 + denpar(14,order,j,mm)*dr + denpar(15,order,j,mm)
                if (lgrad1) then
                  drhoijloc = trm1*rk
                  if (lgrad2) then
                    trm2 = 6.0_dp*denpar(12,order,j,mm)*dr + 2.0_dp*denpar(13,order,j,mm)
                    drhoij2loc = rk2*(trm2 - rk*trm1)
                  endif
                endif
              else
                rhoijloc = 0.0_dp
                if (lgrad1) then
                  drhoijloc = 0.0_dp
                  if (lgrad2) then
                    drhoij2loc = 0.0_dp
                  endif
                endif
              endif
            elseif (ndenfn(j,mm).eq.9) then
!
!  eVoter-Chen
!
              call etaper(r,0.0_dp,rmax,etpfn,detpfn,d2etpfn,d3etpfn,.true.,lgrad2,.false.)
              detpfn = rk*detpfn
              d2etpfn = rk2*d2etpfn
              d3etpfn = rk2*(rk*d3etpfn - 3.0_dp*(d2etpfn - rk2*detpfn))
              d2etpfn = d2etpfn - rk2*detpfn
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)
              etrm = exp(-bpt*r)
              cpt = 2.0_dp**9
              trm1 = apt*etrm*(1.0_dp + cpt*etrm)
              trm2 = apt*etrm*(1.0_dp + 2.0_dp*cpt*etrm)
              rhoijloc = r2*r2*r2*trm1
              if (lgrad1) then
                drhoijloc = r2*r2*(6.0_dp*trm1 - bpt*r*trm2)
                if (lgrad2) then
                  trm3 = apt*etrm*(1.0_dp + 4.0_dp*cpt*etrm)
                  drhoij2loc = r2*(24.0_dp*trm1 - 11.0_dp*bpt*r*trm2 + bpt*bpt*r2*trm3)
                  drhoij2loc = drhoij2loc*etpfn + 2.0_dp*drhoijloc*detpfn + rhoijloc*d2etpfn
                endif
                drhoijloc = drhoijloc*etpfn + rhoijloc*detpfn
              endif
            elseif (ndenfn(j,mm).eq.10) then
!
!  Mei-Davenport
!   
              rr0 = denpar(7,order,j,mm)
              rr12 = 1.0_dp/12.0_dp
              trm1 = rk*rr0
              rr0 = 1.0_dp/denpar(7,order,j,mm)
              rhoijloc = 0.0_dp
              if (lgrad1) then
                drhoijloc = 0.0_dp
                if (lgrad2) then
                  drhoij2loc = 0.0_dp
                endif
              endif
              do l = 0,5
                trm2 = rr0*rr0
                rhoijloc = rhoijloc + rr12*denpar(l+1,order,j,mm)*(trm1)**(l)
                if (lgrad1) then
                  drhoijloc = drhoijloc - rr12*denpar(l+1,order,j,mm)*trm2*dble(l)*(trm1)**(l+2)
                  if (lgrad2) then
                    trm2 = trm2*trm2
                    drhoij2loc = drhoij2loc + rr12*denpar(l+1,order,j,mm)*trm2*dble(l*(l+2))*(trm1)**(l+4)
                  endif
                endif
              enddo
            elseif (ndenfn(j,mm).eq.12) then
!
!  Baskes
!
              apt = denpar(1,order,j,mm)
              bpt = denpar(2,order,j,mm)/denpar(3,order,j,mm)
              cpt = denpar(3,order,j,mm)
              trm1 = apt*oci*exp(-bpt*(r-cpt))
              rhoijloc = trm1
              if (lgrad1) then
                drhoijloc = - bpt*trm1*rk
                if (lgrad2) then
                  drhoij2loc = bpt*trm1*rk2*(bpt + rk)
                endif
              endif
            elseif (ndenfn(j,mm).eq.14) then
!
!  Fractional power law
!
              rpt = denpar(2,order,j,mm)
              trm1 = oci*denpar(1,order,j,mm)*(rk**npt)
              rhoijloc = trm1
              if (lgrad1) then
                trm1 = - rpt*trm1*rk2
                drhoijloc = trm1
                if (lgrad2) then
                  trm2 = - trm1*(rpt+2.0_dp)*rk2
                  drhoij2loc = trm2
                endif
              endif
            endif
!               
!  Add contributions 
!    
            if (tapertype.eq.3.and.tapermax.gt.1.0d-12) then
              dt = eamtaperrho(mm) - (tapermax/taperm)*eamtaperdrho(mm)*(1.0_dp - (r/tapermax)**taperm)
              term0 = rhoijloc - dt
              if (lgrad1) then
                d1t = eamtaperdrho(mm)*rk*(r/tapermax)**(taperm-1.0_dp)
                term1 = drhoijloc - d1t
                if (lgrad2) then
                  d2t = ((taperm - 1.0_dp)/tapermax)*eamtaperdrho(mm)*(r/tapermax)**(taperm-2.0_dp)
                  term2 = drhoij2loc - rk2*(d2t - d1t)
                endif
              endif
            elseif (tapertype.eq.5.and.tapermax.gt.1.0d-12) then
!
!  MDF taper
!
              term0 = rhoijloc*tpfn
              if (lgrad1) then
                term1 = drhoijloc*tpfn + rhoijloc*dtpfn
                if (lgrad2) then
                  term2 = drhoij2loc*tpfn + 2.0_dp*drhoijloc*dtpfn + rhoijloc*d2tpfn
                endif
              endif
            else
              term0 = rhoijloc
              if (lgrad1) then
                term1 = drhoijloc
                if (lgrad2) then
                  term2 = drhoij2loc
                endif
              endif
            endif
!
!  Combine radial derivatives with angular contributions
!
            if (order.eq.1) then
              rhoij(1)  = rhoij(1)  + term0
            elseif (order.eq.2) then
              rhoij(2)  = rhoij(2)  + xr*term0
              rhoij(3)  = rhoij(3)  + yr*term0
              rhoij(4)  = rhoij(4)  + zr*term0
            elseif (order.eq.3) then
              rhoij(5)  = rhoij(5)  + xr*xr*term0
              rhoij(6)  = rhoij(6)  + xr*yr*term0
              rhoij(7)  = rhoij(7)  + xr*zr*term0
              rhoij(8)  = rhoij(8)  + yr*yr*term0
              rhoij(9)  = rhoij(9)  + yr*zr*term0
              rhoij(10) = rhoij(10) + zr*zr*term0
              rhoij(11) = rhoij(11) + term0
            elseif (order.eq.4) then
              rhoij(12) = rhoij(12) + xr*xr*xr*term0
              rhoij(13) = rhoij(13) + xr*xr*yr*term0
              rhoij(14) = rhoij(14) + xr*xr*zr*term0
              rhoij(15) = rhoij(15) + xr*yr*yr*term0
              rhoij(16) = rhoij(16) + xr*yr*zr*term0
              rhoij(17) = rhoij(17) + xr*zr*zr*term0
              rhoij(18) = rhoij(18) + yr*yr*yr*term0
              rhoij(19) = rhoij(19) + yr*yr*zr*term0
              rhoij(20) = rhoij(20) + yr*zr*zr*term0
              rhoij(21) = rhoij(21) + zr*zr*zr*term0
              if (lt24) then
                rhoij(22) = rhoij(22) + xr*term0
                rhoij(23) = rhoij(23) + yr*term0
                rhoij(24) = rhoij(24) + zr*term0
              endif
            endif
            if (lgrad1) then
              dtdc(1) = term1*xl
              dtdc(2) = term1*yl
              dtdc(3) = term1*zl
              if (lstr) then
                do ns1 = 1,6
                  dtds(ns1) = term1*rpd(ns1)
                enddo
              endif
              if (order.eq.1) then
                drhoij(1,1) = drhoij(1,1) + dtdc(1)
                drhoij(2,1) = drhoij(2,1) + dtdc(2)
                drhoij(3,1) = drhoij(3,1) + dtdc(3)
                if (lstr) then
                  do ns1 = 1,6
                    drhoijs(ns1,1) = drhoijs(ns1,1) + dtds(ns1)
                  enddo
                endif
              elseif (order.eq.2) then
                do nc1 = 1,3
                  drhoij(nc1,2) = drhoij(nc1,2) + xr*dtdc(nc1) + dxyzdc(nc1,1)*term0
                  drhoij(nc1,3) = drhoij(nc1,3) + yr*dtdc(nc1) + dxyzdc(nc1,2)*term0
                  drhoij(nc1,4) = drhoij(nc1,4) + zr*dtdc(nc1) + dxyzdc(nc1,3)*term0
                enddo
                if (lstr) then
                  do ns1 = 1,6
                    drhoijs(ns1,2) = drhoijs(ns1,2) + xr*dtds(ns1) + dxyzds(ns1,1)*term0
                    drhoijs(ns1,3) = drhoijs(ns1,3) + yr*dtds(ns1) + dxyzds(ns1,2)*term0
                    drhoijs(ns1,4) = drhoijs(ns1,4) + zr*dtds(ns1) + dxyzds(ns1,3)*term0
                  enddo
                endif
              elseif (order.eq.3) then
                do nc1 = 1,3
                  drhoij(nc1,5)  = drhoij(nc1,5)  + xr*xr*dtdc(nc1) + 2.0_dp*xr*dxyzdc(nc1,1)*term0
                  drhoij(nc1,6)  = drhoij(nc1,6)  + xr*yr*dtdc(nc1) + (xr*dxyzdc(nc1,2) + yr*dxyzdc(nc1,1))*term0
                  drhoij(nc1,7)  = drhoij(nc1,7)  + xr*zr*dtdc(nc1) + (xr*dxyzdc(nc1,3) + zr*dxyzdc(nc1,1))*term0
                  drhoij(nc1,8)  = drhoij(nc1,8)  + yr*yr*dtdc(nc1) + 2.0_dp*yr*dxyzdc(nc1,2)*term0
                  drhoij(nc1,9)  = drhoij(nc1,9)  + yr*zr*dtdc(nc1) + (yr*dxyzdc(nc1,3) + zr*dxyzdc(nc1,2))*term0
                  drhoij(nc1,10) = drhoij(nc1,10) + zr*zr*dtdc(nc1) + 2.0_dp*zr*dxyzdc(nc1,3)*term0
                  drhoij(nc1,11) = drhoij(nc1,11) + dtdc(nc1)
                enddo
                if (lstr) then
                  do ns1 = 1,6
                    drhoijs(ns1,5)  = drhoijs(ns1,5)  + xr*xr*dtds(ns1) + 2.0_dp*xr*dxyzds(ns1,1)*term0
                    drhoijs(ns1,6)  = drhoijs(ns1,6)  + xr*yr*dtds(ns1) + (xr*dxyzds(ns1,2) + yr*dxyzds(ns1,1))*term0
                    drhoijs(ns1,7)  = drhoijs(ns1,7)  + xr*zr*dtds(ns1) + (xr*dxyzds(ns1,3) + zr*dxyzds(ns1,1))*term0
                    drhoijs(ns1,8)  = drhoijs(ns1,8)  + yr*yr*dtds(ns1) + 2.0_dp*yr*dxyzds(ns1,2)*term0
                    drhoijs(ns1,9)  = drhoijs(ns1,9)  + yr*zr*dtds(ns1) + (yr*dxyzds(ns1,3) + zr*dxyzds(ns1,2))*term0
                    drhoijs(ns1,10) = drhoijs(ns1,10) + zr*zr*dtds(ns1) + 2.0_dp*zr*dxyzds(ns1,3)*term0
                    drhoijs(ns1,11) = drhoijs(ns1,11) + dtds(ns1)
                  enddo
                endif
              elseif (order.eq.4) then
                do nc1 = 1,3
                  drhoij(nc1,12) = drhoij(nc1,12) + xr*xr*xr*dtdc(nc1) + 3.0_dp*xr*xr*dxyzdc(nc1,1)*term0
                  drhoij(nc1,13) = drhoij(nc1,13) + xr*xr*yr*dtdc(nc1) + (2.0_dp*xr*yr*dxyzdc(nc1,1) + xr*xr*dxyzdc(nc1,2))*term0
                  drhoij(nc1,14) = drhoij(nc1,14) + xr*xr*zr*dtdc(nc1) + (2.0_dp*xr*zr*dxyzdc(nc1,1) + xr*xr*dxyzdc(nc1,3))*term0
                  drhoij(nc1,15) = drhoij(nc1,15) + xr*yr*yr*dtdc(nc1) + (2.0_dp*xr*yr*dxyzdc(nc1,2) + yr*yr*dxyzdc(nc1,1))*term0
                  drhoij(nc1,16) = drhoij(nc1,16) + xr*yr*zr*dtdc(nc1) + (xr*yr*dxyzdc(nc1,3) + xr*zr*dxyzdc(nc1,2) + &
                                                                          yr*zr*dxyzdc(nc1,1))*term0
                  drhoij(nc1,17) = drhoij(nc1,17) + xr*zr*zr*dtdc(nc1) + (2.0_dp*xr*zr*dxyzdc(nc1,3) + zr*zr*dxyzdc(nc1,1))*term0
                  drhoij(nc1,18) = drhoij(nc1,18) + yr*yr*yr*dtdc(nc1) + 3.0_dp*yr*yr*dxyzdc(nc1,2)*term0
                  drhoij(nc1,19) = drhoij(nc1,19) + yr*yr*zr*dtdc(nc1) + (2.0_dp*yr*zr*dxyzdc(nc1,2) + yr*yr*dxyzdc(nc1,3))*term0
                  drhoij(nc1,20) = drhoij(nc1,20) + yr*zr*zr*dtdc(nc1) + (2.0_dp*yr*zr*dxyzdc(nc1,3) + zr*zr*dxyzdc(nc1,2))*term0
                  drhoij(nc1,21) = drhoij(nc1,21) + zr*zr*zr*dtdc(nc1) + 3.0_dp*zr*zr*dxyzdc(nc1,3)*term0
                enddo
                if (lt24) then
                  do nc1 = 1,3
                    drhoij(nc1,22) = drhoij(nc1,22) + xr*dtdc(nc1) + dxyzdc(nc1,1)*term0
                    drhoij(nc1,23) = drhoij(nc1,23) + yr*dtdc(nc1) + dxyzdc(nc1,2)*term0
                    drhoij(nc1,24) = drhoij(nc1,24) + zr*dtdc(nc1) + dxyzdc(nc1,3)*term0
                  enddo
                endif
!
                if (lstr) then
!
!  Derivatives of rk3 and term0
!
                  do ns1 = 1,6
                    drhoijs(ns1,12) = drhoijs(ns1,12) + xr*xr*xr*dtds(ns1) + 3.0_dp*xr*xr*dxyzds(ns1,1)*term0
                    drhoijs(ns1,13) = drhoijs(ns1,13) + xr*xr*yr*dtds(ns1) + (2.0_dp*xr*yr*dxyzds(ns1,1) + &
                                                                              xr*xr*dxyzds(ns1,2))*term0
                    drhoijs(ns1,14) = drhoijs(ns1,14) + xr*xr*zr*dtds(ns1) + (2.0_dp*xr*zr*dxyzds(ns1,1) + &
                                                                              xr*xr*dxyzds(ns1,3))*term0
                    drhoijs(ns1,15) = drhoijs(ns1,15) + xr*yr*yr*dtds(ns1) + (2.0_dp*xr*yr*dxyzds(ns1,2) + &
                                                                              yr*yr*dxyzds(ns1,1))*term0
                    drhoijs(ns1,16) = drhoijs(ns1,16) + xr*yr*zr*dtds(ns1) + (xr*yr*dxyzds(ns1,3) + &
                                                                              xr*zr*dxyzds(ns1,2) + &
                                                                              yr*zr*dxyzds(ns1,1))*term0
                    drhoijs(ns1,17) = drhoijs(ns1,17) + xr*zr*zr*dtds(ns1) + (2.0_dp*xr*zr*dxyzds(ns1,3) + &
                                                                              zr*zr*dxyzds(ns1,1))*term0
                    drhoijs(ns1,18) = drhoijs(ns1,18) + yr*yr*yr*dtds(ns1) + 3.0_dp*yr*yr*dxyzds(ns1,2)*term0
                    drhoijs(ns1,19) = drhoijs(ns1,19) + yr*yr*zr*dtds(ns1) + (2.0_dp*yr*zr*dxyzds(ns1,2) + &
                                                                              yr*yr*dxyzds(ns1,3))*term0
                    drhoijs(ns1,20) = drhoijs(ns1,20) + yr*zr*zr*dtds(ns1) + (2.0_dp*yr*zr*dxyzds(ns1,3) + &
                                                                              zr*zr*dxyzds(ns1,2))*term0
                    drhoijs(ns1,21) = drhoijs(ns1,21) + zr*zr*zr*dtds(ns1) + 3.0_dp*zr*zr*dxyzds(ns1,3)*term0
                  enddo
                  if (lt24) then
                    do ns1 = 1,6
                      drhoijs(ns1,22) = drhoijs(ns1,22) + xr*dtds(ns1) + dxyzds(ns1,1)*term0
                      drhoijs(ns1,23) = drhoijs(ns1,23) + yr*dtds(ns1) + dxyzds(ns1,2)*term0
                      drhoijs(ns1,24) = drhoijs(ns1,24) + zr*dtds(ns1) + dxyzds(ns1,3)*term0
                    enddo
                  endif
                endif
              endif
              if (lgrad2) then
                d2tdc2(1) = term2*x*x + term1
                d2tdc2(2) = term2*x*y
                d2tdc2(3) = term2*x*z
                d2tdc2(4) = term2*y*y + term1
                d2tdc2(5) = term2*y*z
                d2tdc2(6) = term2*z*z + term1
                if (order.eq.1) then
                  do nc1 = 1,6
                    drhoij2(nc1,1) = drhoij2(nc1,1) + d2tdc2(nc1)
                  enddo
                elseif (order.eq.2) then
                  ind = 0
                  do nc1 = 1,3
                    do nc2 = nc1,3
                      ind = ind + 1
                      drhoij2(ind,2) = drhoij2(ind,2) + xr*d2tdc2(ind) + term0*d2xyzdc2(ind,1) + &
                                                        dxyzdc(nc1,1)*dtdc(nc2) + dxyzdc(nc2,1)*dtdc(nc1)
                      drhoij2(ind,3) = drhoij2(ind,3) + yr*d2tdc2(ind) + term0*d2xyzdc2(ind,2) + &
                                                        dxyzdc(nc1,2)*dtdc(nc2) + dxyzdc(nc2,2)*dtdc(nc1)
                      drhoij2(ind,4) = drhoij2(ind,4) + zr*d2tdc2(ind) + term0*d2xyzdc2(ind,3) + &
                                                        dxyzdc(nc1,3)*dtdc(nc2) + dxyzdc(nc2,3)*dtdc(nc1)
                    enddo
                  enddo
                elseif (order.eq.3) then
                  ind = 0
                  do nc1 = 1,3
                    do nc2 = nc1,3
                      ind = ind + 1
                      drhoij2(ind,5)  = drhoij2(ind,5)  + xr*xr*d2tdc2(ind) + term0*(2.0_dp*d2xyzdc2(ind,1)*xr) + &
                                                          term0*(2.0_dp*dxyzdc(nc1,1)*dxyzdc(nc2,1)) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,1)*xr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,1)*xr)
                      drhoij2(ind,6)  = drhoij2(ind,6)  + xr*yr*d2tdc2(ind) + term0*(d2xyzdc2(ind,1)*yr + d2xyzdc2(ind,2)*xr) + &
                                                          term0*(dxyzdc(nc1,1)*dxyzdc(nc2,2) + dxyzdc(nc1,2)*dxyzdc(nc2,1)) + &
                                                          dtdc(nc1)*(dxyzdc(nc2,1)*yr + dxyzdc(nc2,2)*xr) + &
                                                          dtdc(nc2)*(dxyzdc(nc1,1)*yr + dxyzdc(nc1,2)*xr)
                      drhoij2(ind,7)  = drhoij2(ind,7)  + xr*zr*d2tdc2(ind) + term0*(d2xyzdc2(ind,1)*zr + d2xyzdc2(ind,3)*xr) + &
                                                          term0*(dxyzdc(nc1,1)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,1)) + &
                                                          dtdc(nc1)*(dxyzdc(nc2,1)*zr + dxyzdc(nc2,3)*xr) + &
                                                          dtdc(nc2)*(dxyzdc(nc1,1)*zr + dxyzdc(nc1,3)*xr)
                      drhoij2(ind,8)  = drhoij2(ind,8)  + yr*yr*d2tdc2(ind) + term0*(2.0_dp*d2xyzdc2(ind,2)*yr) + &
                                                          term0*(2.0_dp*dxyzdc(nc1,2)*dxyzdc(nc2,2)) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,2)*yr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,2)*yr)
                      drhoij2(ind,9)  = drhoij2(ind,9)  + yr*zr*d2tdc2(ind) + term0*(d2xyzdc2(ind,2)*zr + d2xyzdc2(ind,3)*yr) + &
                                                          term0*(dxyzdc(nc1,2)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,2)) + &
                                                          dtdc(nc1)*(dxyzdc(nc2,2)*zr + dxyzdc(nc2,3)*yr) + &
                                                          dtdc(nc2)*(dxyzdc(nc1,2)*zr + dxyzdc(nc1,3)*yr)
                      drhoij2(ind,10) = drhoij2(ind,10) + zr*zr*d2tdc2(ind) + term0*(2.0_dp*d2xyzdc2(ind,3)*zr) + &
                                                          term0*(2.0_dp*dxyzdc(nc1,3)*dxyzdc(nc2,3)) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,3)*zr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,3)*zr)
                      drhoij2(ind,11) = drhoij2(ind,11) + d2tdc2(ind)
                    enddo
                  enddo
                elseif (order.eq.4) then
                  ind = 0
                  do nc1 = 1,3
                    do nc2 = nc1,3
                      ind = ind + 1
                      drhoij2(ind,12) = drhoij2(ind,12) + xr*xr*xr*d2tdc2(ind) + term0*(3.0_dp*d2xyzdc2(ind,1)*xr*xr) + &
                                                          term0*(6.0_dp*dxyzdc(nc1,1)*dxyzdc(nc2,1)*xr) + &
                                                          dtdc(nc1)*(3.0_dp*dxyzdc(nc2,1)*xr*xr) + &
                                                          dtdc(nc2)*(3.0_dp*dxyzdc(nc1,1)*xr*xr)
                      drhoij2(ind,13) = drhoij2(ind,13) + xr*xr*yr*d2tdc2(ind) + &
                                                          term0*(2.0_dp*d2xyzdc2(ind,1)*xr*yr + d2xyzdc2(ind,2)*xr*xr) + &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,1)*dxyzdc(nc2,2) + &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,2)*dxyzdc(nc2,1) + &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,1)*dxyzdc(nc2,1) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,1)*xr*yr + dxyzdc(nc2,2)*xr*xr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,1)*xr*yr + dxyzdc(nc1,2)*xr*xr)
                      drhoij2(ind,14) = drhoij2(ind,14) + xr*xr*zr*d2tdc2(ind) + &
                                                          term0*(2.0_dp*d2xyzdc2(ind,1)*xr*zr + d2xyzdc2(ind,3)*xr*xr) + &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,1)*dxyzdc(nc2,3) + &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,3)*dxyzdc(nc2,1) + &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,1)*dxyzdc(nc2,1) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,1)*xr*zr + dxyzdc(nc2,3)*xr*xr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,1)*xr*zr + dxyzdc(nc1,3)*xr*xr)
                      drhoij2(ind,15) = drhoij2(ind,15) + xr*yr*yr*d2tdc2(ind) + &
                                                          term0*(2.0_dp*d2xyzdc2(ind,2)*xr*yr + d2xyzdc2(ind,1)*yr*yr) + &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,1)*dxyzdc(nc2,2) + &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,2)*dxyzdc(nc2,1) + &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,2)*dxyzdc(nc2,2) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,2)*xr*yr + dxyzdc(nc2,1)*yr*yr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,2)*xr*yr + dxyzdc(nc1,1)*yr*yr)
                      drhoij2(ind,16) = drhoij2(ind,16) + xr*yr*zr*d2tdc2(ind) + &
                                                          term0*(d2xyzdc2(ind,1)*yr*zr + &
                                                                 d2xyzdc2(ind,2)*xr*zr + &
                                                                 d2xyzdc2(ind,3)*xr*yr) + &
                                                          term0*xr*(dxyzdc(nc1,2)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,2)) + &
                                                          term0*yr*(dxyzdc(nc1,1)*dxyzdc(nc2,3) + dxyzdc(nc1,3)*dxyzdc(nc2,1)) + &
                                                          term0*zr*(dxyzdc(nc1,1)*dxyzdc(nc2,2) + dxyzdc(nc1,2)*dxyzdc(nc2,1)) + &
                                                          dtdc(nc1)*(dxyzdc(nc2,1)*yr*zr + &
                                                                     dxyzdc(nc2,2)*xr*zr + &
                                                                     dxyzdc(nc2,3)*xr*yr) + &
                                                          dtdc(nc2)*(dxyzdc(nc1,1)*yr*zr + &
                                                                     dxyzdc(nc1,2)*xr*zr + &
                                                                     dxyzdc(nc1,3)*xr*yr)
                      drhoij2(ind,17) = drhoij2(ind,17) + xr*zr*zr*d2tdc2(ind) + &
                                                          term0*(2.0_dp*d2xyzdc2(ind,3)*xr*zr + d2xyzdc2(ind,1)*zr*zr) + &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,1)*dxyzdc(nc2,3) + &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,3)*dxyzdc(nc2,1) + &
                                                          2.0_dp*term0*xr*dxyzdc(nc1,3)*dxyzdc(nc2,3) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,3)*xr*zr + dxyzdc(nc2,1)*zr*zr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,3)*xr*zr + dxyzdc(nc1,1)*zr*zr)
                      drhoij2(ind,18) = drhoij2(ind,18) + yr*yr*yr*d2tdc2(ind) + term0*(3.0_dp*d2xyzdc2(ind,2)*yr*yr) + &
                                                          term0*(6.0_dp*dxyzdc(nc1,2)*dxyzdc(nc2,2)*yr) + &
                                                          dtdc(nc1)*(3.0_dp*dxyzdc(nc2,2)*yr*yr) + &
                                                          dtdc(nc2)*(3.0_dp*dxyzdc(nc1,2)*yr*yr)
                      drhoij2(ind,19) = drhoij2(ind,19) + yr*yr*zr*d2tdc2(ind) + &
                                                          term0*(2.0_dp*d2xyzdc2(ind,2)*yr*zr + d2xyzdc2(ind,3)*yr*yr) + &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,2)*dxyzdc(nc2,3) + &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,3)*dxyzdc(nc2,2) + &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,2)*dxyzdc(nc2,2) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,2)*yr*zr + dxyzdc(nc2,3)*yr*yr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,2)*yr*zr + dxyzdc(nc1,3)*yr*yr)
                      drhoij2(ind,20) = drhoij2(ind,20) + yr*zr*zr*d2tdc2(ind) + &
                                                          term0*(2.0_dp*d2xyzdc2(ind,3)*yr*zr + d2xyzdc2(ind,2)*zr*zr) + &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,3)*dxyzdc(nc2,2) + &
                                                          2.0_dp*term0*zr*dxyzdc(nc1,2)*dxyzdc(nc2,3) + &
                                                          2.0_dp*term0*yr*dxyzdc(nc1,3)*dxyzdc(nc2,3) + &
                                                          dtdc(nc1)*(2.0_dp*dxyzdc(nc2,3)*yr*zr + dxyzdc(nc2,2)*zr*zr) + &
                                                          dtdc(nc2)*(2.0_dp*dxyzdc(nc1,3)*yr*zr + dxyzdc(nc1,2)*zr*zr)
                      drhoij2(ind,21) = drhoij2(ind,21) + zr*zr*zr*d2tdc2(ind) + term0*(3.0_dp*d2xyzdc2(ind,3)*zr*zr) + &
                                                          term0*(6.0_dp*dxyzdc(nc1,3)*dxyzdc(nc2,3)*zr) + &
                                                          dtdc(nc1)*(3.0_dp*dxyzdc(nc2,3)*zr*zr) + &
                                                          dtdc(nc2)*(3.0_dp*dxyzdc(nc1,3)*zr*zr)
                    enddo
                  enddo
                  if (lt24) then
                    ind = 0
                    do nc1 = 1,3
                      do nc2 = nc1,3
                        ind = ind + 1
                        drhoij2(ind,22) = drhoij2(ind,22) + xr*d2tdc2(ind) + term0*d2xyzdc2(ind,1) + &
                                                            dxyzdc(nc1,1)*dtdc(nc2) + dxyzdc(nc2,1)*dtdc(nc1)
                        drhoij2(ind,23) = drhoij2(ind,23) + yr*d2tdc2(ind) + term0*d2xyzdc2(ind,2) + &
                                                            dxyzdc(nc1,2)*dtdc(nc2) + dxyzdc(nc2,2)*dtdc(nc1)
                        drhoij2(ind,24) = drhoij2(ind,24) + zr*d2tdc2(ind) + term0*d2xyzdc2(ind,3) + &
                                                            dxyzdc(nc1,3)*dtdc(nc2) + dxyzdc(nc2,3)*dtdc(nc1)
                      enddo
                    enddo
                  endif
                endif
                if (lstr) then
                  ind = 0
                  do ns1 = 1,6
                    do ns2 = 1,ns1
                      ind = ind + 1
                      d2tds2(ind) = term2*rpd(ns1)*rpd(ns2)
                    enddo
                    d2tdsdc(ns1,1) = term2*rpd(ns1)*xl
                    d2tdsdc(ns1,2) = term2*rpd(ns1)*yl
                    d2tdsdc(ns1,3) = term2*rpd(ns1)*zl
                  enddo
                  if (order.eq.1) then
                    do ns1 = 1,21
                      drhoij2s(ns1,1) = drhoij2s(ns1,1) + d2tds2(ns1)
                    enddo
!
                    drhoij2s(1,1)  = drhoij2s(1,1)  + 2.0_dp*dtds(1)
                    drhoij2s(3,1)  = drhoij2s(3,1)  + 2.0_dp*dtds(2)
                    drhoij2s(6,1)  = drhoij2s(6,1)  + 2.0_dp*dtds(3)
                    drhoij2s(8,1)  = drhoij2s(8,1)  + dtds(4)
                    drhoij2s(9,1)  = drhoij2s(9,1)  + dtds(4)
                    drhoij2s(10,1) = drhoij2s(10,1) + 0.5_dp*(dtds(2)+dtds(3))
                    drhoij2s(11,1) = drhoij2s(11,1) + dtds(5)
                    drhoij2s(13,1) = drhoij2s(13,1) + dtds(5)
                    drhoij2s(14,1) = drhoij2s(14,1) + 0.5_dp*dtds(6)
                    drhoij2s(15,1) = drhoij2s(15,1) + 0.5_dp*(dtds(1)+dtds(3))
                    drhoij2s(16,1) = drhoij2s(16,1) + dtds(6)
                    drhoij2s(17,1) = drhoij2s(17,1) + dtds(6)
                    drhoij2s(19,1) = drhoij2s(19,1) + 0.5_dp*dtds(5)
                    drhoij2s(20,1) = drhoij2s(20,1) + 0.5_dp*dtds(4)
                    drhoij2s(21,1) = drhoij2s(21,1) + 0.5_dp*(dtds(1)+dtds(2))
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoij2m(ns1,nc1,1) = drhoij2m(ns1,nc1,1) + d2tdsdc(ns1,nc1) + 2.0_dp*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                  elseif (order.eq.2) then
                    ind = 0
                    do ns1 = 1,6
                      do ns2 = 1,ns1
                        ind = ind + 1
                        drhoij2s(ind,2) = drhoij2s(ind,2) + xr*d2tds2(ind) + term0*d2xyzds2(ind,1) + &
                                                            dxyzds(ns1,1)*dtds(ns2) + dxyzds(ns2,1)*dtds(ns1)
                        drhoij2s(ind,3) = drhoij2s(ind,3) + yr*d2tds2(ind) + term0*d2xyzds2(ind,2) + &
                                                            dxyzds(ns1,2)*dtds(ns2) + dxyzds(ns2,2)*dtds(ns1)
                        drhoij2s(ind,4) = drhoij2s(ind,4) + zr*d2tds2(ind) + term0*d2xyzds2(ind,3) + &
                                                            dxyzds(ns1,3)*dtds(ns2) + dxyzds(ns2,3)*dtds(ns1)
                      enddo
                    enddo
!
                    do nord = 2,4
                      drhoij2s(1,nord)  = drhoij2s(1,nord)  + 2.0_dp*ordtrm(nord)*dtds(1)
                      drhoij2s(3,nord)  = drhoij2s(3,nord)  + 2.0_dp*ordtrm(nord)*dtds(2)
                      drhoij2s(6,nord)  = drhoij2s(6,nord)  + 2.0_dp*ordtrm(nord)*dtds(3)
                      drhoij2s(8,nord)  = drhoij2s(8,nord)  + ordtrm(nord)*dtds(4)
                      drhoij2s(9,nord)  = drhoij2s(9,nord)  + ordtrm(nord)*dtds(4)
                      drhoij2s(10,nord) = drhoij2s(10,nord) + 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                      drhoij2s(11,nord) = drhoij2s(11,nord) + ordtrm(nord)*dtds(5)
                      drhoij2s(13,nord) = drhoij2s(13,nord) + ordtrm(nord)*dtds(5)
                      drhoij2s(14,nord) = drhoij2s(14,nord) + 0.5_dp*ordtrm(nord)*dtds(6)
                      drhoij2s(15,nord) = drhoij2s(15,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                      drhoij2s(16,nord) = drhoij2s(16,nord) + ordtrm(nord)*dtds(6)
                      drhoij2s(17,nord) = drhoij2s(17,nord) + ordtrm(nord)*dtds(6)
                      drhoij2s(19,nord) = drhoij2s(19,nord) + 0.5_dp*ordtrm(nord)*dtds(5)
                      drhoij2s(20,nord) = drhoij2s(20,nord) + 0.5_dp*ordtrm(nord)*dtds(4)
                      drhoij2s(21,nord) = drhoij2s(21,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                    enddo
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoij2m(ns1,nc1,2) = drhoij2m(ns1,nc1,2) + xr*d2tdsdc(ns1,nc1) + term0*d2xyzdsdc(ns1,nc1,1) + &
                                                                    dxyzds(ns1,1)*dtdc(nc1) + dxyzdc(nc1,1)*dtds(ns1) + &
                                                                    2.0_dp*xr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,3) = drhoij2m(ns1,nc1,3) + yr*d2tdsdc(ns1,nc1) + term0*d2xyzdsdc(ns1,nc1,2) + &
                                                                    dxyzds(ns1,2)*dtdc(nc1) + dxyzdc(nc1,2)*dtds(ns1) + &
                                                                    2.0_dp*yr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,4) = drhoij2m(ns1,nc1,4) + zr*d2tdsdc(ns1,nc1) + term0*d2xyzdsdc(ns1,nc1,3) + &
                                                                    dxyzds(ns1,3)*dtdc(nc1) + dxyzdc(nc1,3)*dtds(ns1) + &
                                                                    2.0_dp*zr*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                  elseif (order.eq.3) then
                    ind = 0
                    do ns1 = 1,6
                      do ns2 = 1,ns1
                        ind = ind + 1
                        drhoij2s(ind,5)  = drhoij2s(ind,5)  + xr*xr*d2tds2(ind) + term0*(2.0_dp*d2xyzds2(ind,1)*xr) + &
                                                              term0*(2.0_dp*dxyzds(ns1,1)*dxyzds(ns2,1)) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,1)*xr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,1)*xr)
                        drhoij2s(ind,6)  = drhoij2s(ind,6)  + xr*yr*d2tds2(ind) + term0*(d2xyzds2(ind,1)*yr + d2xyzds2(ind,2)*xr)+&
                                                              term0*(dxyzds(ns1,1)*dxyzds(ns2,2) + dxyzds(ns1,2)*dxyzds(ns2,1)) + &
                                                              dtds(ns1)*(dxyzds(ns2,1)*yr + dxyzds(ns2,2)*xr) + &
                                                              dtds(ns2)*(dxyzds(ns1,1)*yr + dxyzds(ns1,2)*xr)
                        drhoij2s(ind,7)  = drhoij2s(ind,7)  + xr*zr*d2tds2(ind) + term0*(d2xyzds2(ind,1)*zr + d2xyzds2(ind,3)*xr)+&
                                                              term0*(dxyzds(ns1,1)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,1)) + &
                                                              dtds(ns1)*(dxyzds(ns2,1)*zr + dxyzds(ns2,3)*xr) + &
                                                              dtds(ns2)*(dxyzds(ns1,1)*zr + dxyzds(ns1,3)*xr)
                        drhoij2s(ind,8)  = drhoij2s(ind,8)  + yr*yr*d2tds2(ind) + term0*(2.0_dp*d2xyzds2(ind,2)*yr) + &
                                                              term0*(2.0_dp*dxyzds(ns1,2)*dxyzds(ns2,2)) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,2)*yr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,2)*yr)
                        drhoij2s(ind,9)  = drhoij2s(ind,9)  + yr*zr*d2tds2(ind) + term0*(d2xyzds2(ind,2)*zr + d2xyzds2(ind,3)*yr)+&
                                                              term0*(dxyzds(ns1,2)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,2)) + &
                                                              dtds(ns1)*(dxyzds(ns2,2)*zr + dxyzds(ns2,3)*yr) + &
                                                              dtds(ns2)*(dxyzds(ns1,2)*zr + dxyzds(ns1,3)*yr)
                        drhoij2s(ind,10) = drhoij2s(ind,10) + zr*zr*d2tds2(ind) + term0*(2.0_dp*d2xyzds2(ind,3)*zr) + &
                                                              term0*(2.0_dp*dxyzds(ns1,3)*dxyzds(ns2,3)) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,3)*zr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,3)*zr)
                        drhoij2s(ind,11) = drhoij2s(ind,11) + d2tds2(ind)
                      enddo
                    enddo
!
                    do nord = 5,11
                      drhoij2s(1,nord)  = drhoij2s(1,nord)  + 2.0_dp*ordtrm(nord)*dtds(1)
                      drhoij2s(3,nord)  = drhoij2s(3,nord)  + 2.0_dp*ordtrm(nord)*dtds(2)
                      drhoij2s(6,nord)  = drhoij2s(6,nord)  + 2.0_dp*ordtrm(nord)*dtds(3)
                      drhoij2s(8,nord)  = drhoij2s(8,nord)  + ordtrm(nord)*dtds(4)
                      drhoij2s(9,nord)  = drhoij2s(9,nord)  + ordtrm(nord)*dtds(4)
                      drhoij2s(10,nord) = drhoij2s(10,nord) + 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                      drhoij2s(11,nord) = drhoij2s(11,nord) + ordtrm(nord)*dtds(5)
                      drhoij2s(13,nord) = drhoij2s(13,nord) + ordtrm(nord)*dtds(5)
                      drhoij2s(14,nord) = drhoij2s(14,nord) + 0.5_dp*ordtrm(nord)*dtds(6)
                      drhoij2s(15,nord) = drhoij2s(15,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                      drhoij2s(16,nord) = drhoij2s(16,nord) + ordtrm(nord)*dtds(6)
                      drhoij2s(17,nord) = drhoij2s(17,nord) + ordtrm(nord)*dtds(6)
                      drhoij2s(19,nord) = drhoij2s(19,nord) + 0.5_dp*ordtrm(nord)*dtds(5)
                      drhoij2s(20,nord) = drhoij2s(20,nord) + 0.5_dp*ordtrm(nord)*dtds(4)
                      drhoij2s(21,nord) = drhoij2s(21,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                    enddo
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoij2m(ns1,nc1,5)  = drhoij2m(ns1,nc1,5)  + xr*xr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,1)*xr) + &
                                                                      term0*(2.0_dp*dxyzds(ns1,1)*dxyzdc(nc1,1)) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,1)*xr) + &
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,1)*xr) + &
                                                                      2.0_dp*xr*xr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,6)  = drhoij2m(ns1,nc1,6)  + xr*yr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(d2xyzdsdc(ns1,nc1,1)*yr + d2xyzdsdc(ns1,nc1,2)*xr) + &
                                                                      term0*(dxyzds(ns1,1)*dxyzdc(nc1,2) + &
                                                                             dxyzds(ns1,2)*dxyzdc(nc1,1)) + &
                                                                      dtds(ns1)*(dxyzdc(nc1,1)*yr + dxyzdc(nc1,2)*xr) + &
                                                                      dtdc(nc1)*(dxyzds(ns1,1)*yr + dxyzds(ns1,2)*xr) + &
                                                                      2.0_dp*xr*yr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,7)  = drhoij2m(ns1,nc1,7)  + xr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(d2xyzdsdc(ns1,nc1,1)*zr + d2xyzdsdc(ns1,nc1,3)*xr) + &
                                                                      term0*(dxyzds(ns1,1)*dxyzdc(nc1,3) + &
                                                                             dxyzds(ns1,3)*dxyzdc(nc1,1)) + &
                                                                      dtds(ns1)*(dxyzdc(nc1,1)*zr + dxyzdc(nc1,3)*xr) + &
                                                                      dtdc(nc1)*(dxyzds(ns1,1)*zr + dxyzds(ns1,3)*xr) + &
                                                                      2.0_dp*xr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,8)  = drhoij2m(ns1,nc1,8)  + yr*yr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,2)*yr) + &
                                                                      term0*(2.0_dp*dxyzds(ns1,2)*dxyzdc(nc1,2)) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,2)*yr) + &
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,2)*yr) + &
                                                                      2.0_dp*yr*yr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,9)  = drhoij2m(ns1,nc1,9)  + yr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(d2xyzdsdc(ns1,nc1,2)*zr + d2xyzdsdc(ns1,nc1,3)*yr) + &
                                                                      term0*(dxyzds(ns1,2)*dxyzdc(nc1,3) + &
                                                                             dxyzds(ns1,3)*dxyzdc(nc1,2)) + &
                                                                      dtds(ns1)*(dxyzdc(nc1,2)*zr + dxyzdc(nc1,3)*yr) + &
                                                                      dtdc(nc1)*(dxyzds(ns1,2)*zr + dxyzds(ns1,3)*yr) + &
                                                                      2.0_dp*yr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,10) = drhoij2m(ns1,nc1,10) + zr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,3)*zr) + &
                                                                      term0*(2.0_dp*dxyzds(ns1,3)*dxyzdc(nc1,3)) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,3)*zr) + &
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,3)*zr) + &
                                                                      2.0_dp*zr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,11) = drhoij2m(ns1,nc1,11) + d2tdsdc(ns1,nc1) + 2.0_dp*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                  elseif (order.eq.4) then
                    ind = 0
                    do ns1 = 1,6
                      do ns2 = 1,ns1
                        ind = ind + 1
                        drhoij2s(ind,12) = drhoij2s(ind,12) + xr*xr*xr*d2tds2(ind) + term0*(3.0_dp*d2xyzds2(ind,1)*xr*xr) + &
                                                              term0*(6.0_dp*dxyzds(ns1,1)*dxyzds(ns2,1)*xr) + &
                                                              dtds(ns1)*(3.0_dp*dxyzds(ns2,1)*xr*xr) + &
                                                              dtds(ns2)*(3.0_dp*dxyzds(ns1,1)*xr*xr)
                        drhoij2s(ind,13) = drhoij2s(ind,13) + xr*xr*yr*d2tds2(ind) + &
                                                              term0*(2.0_dp*d2xyzds2(ind,1)*xr*yr + d2xyzds2(ind,2)*xr*xr) + &
                                                              2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzds(ns2,2) + &
                                                              2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzds(ns2,1) + &
                                                              2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzds(ns2,1) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,1)*xr*yr + dxyzds(ns2,2)*xr*xr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,1)*xr*yr + dxyzds(ns1,2)*xr*xr)
                        drhoij2s(ind,14) = drhoij2s(ind,14) + xr*xr*zr*d2tds2(ind) + &
                                                              term0*(2.0_dp*d2xyzds2(ind,1)*xr*zr + d2xyzds2(ind,3)*xr*xr) + &
                                                              2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzds(ns2,3) + &
                                                              2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzds(ns2,1) + &
                                                              2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzds(ns2,1) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,1)*xr*zr + dxyzds(ns2,3)*xr*xr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,1)*xr*zr + dxyzds(ns1,3)*xr*xr)
                        drhoij2s(ind,15) = drhoij2s(ind,15) + xr*yr*yr*d2tds2(ind) + &
                                                              term0*(2.0_dp*d2xyzds2(ind,2)*xr*yr + d2xyzds2(ind,1)*yr*yr) + &
                                                              2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzds(ns2,2) + &
                                                              2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzds(ns2,1) + &
                                                              2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzds(ns2,2) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,2)*xr*yr + dxyzds(ns2,1)*yr*yr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,2)*xr*yr + dxyzds(ns1,1)*yr*yr)
                        drhoij2s(ind,16) = drhoij2s(ind,16) + xr*yr*zr*d2tds2(ind) + &
                                                              term0*(d2xyzds2(ind,1)*yr*zr + &
                                                                     d2xyzds2(ind,2)*xr*zr + &
                                                                     d2xyzds2(ind,3)*xr*yr) + &
                                                              term0*xr*(dxyzds(ns1,2)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,2))+&
                                                              term0*yr*(dxyzds(ns1,1)*dxyzds(ns2,3) + dxyzds(ns1,3)*dxyzds(ns2,1))+&
                                                              term0*zr*(dxyzds(ns1,1)*dxyzds(ns2,2) + dxyzds(ns1,2)*dxyzds(ns2,1))+&
                                                              dtds(ns1)*(dxyzds(ns2,1)*yr*zr + &
                                                                         dxyzds(ns2,2)*xr*zr + &
                                                                         dxyzds(ns2,3)*xr*yr) + &
                                                              dtds(ns2)*(dxyzds(ns1,1)*yr*zr + &
                                                                         dxyzds(ns1,2)*xr*zr + &
                                                                         dxyzds(ns1,3)*xr*yr)
                        drhoij2s(ind,17) = drhoij2s(ind,17) + xr*zr*zr*d2tds2(ind) + &
                                                              term0*(2.0_dp*d2xyzds2(ind,3)*xr*zr + d2xyzds2(ind,1)*zr*zr) + &
                                                              2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzds(ns2,3) + &
                                                              2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzds(ns2,1) + &
                                                              2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzds(ns2,3) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,3)*xr*zr + dxyzds(ns2,1)*zr*zr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,3)*xr*zr + dxyzds(ns1,1)*zr*zr)
                        drhoij2s(ind,18) = drhoij2s(ind,18) + yr*yr*yr*d2tds2(ind) + term0*(3.0_dp*d2xyzds2(ind,2)*yr*yr) + &
                                                              term0*(6.0_dp*dxyzds(ns1,2)*dxyzds(ns2,2)*yr) + &
                                                              dtds(ns1)*(3.0_dp*dxyzds(ns2,2)*yr*yr) + &
                                                              dtds(ns2)*(3.0_dp*dxyzds(ns1,2)*yr*yr)
                        drhoij2s(ind,19) = drhoij2s(ind,19) + yr*yr*zr*d2tds2(ind) + &
                                                              term0*(2.0_dp*d2xyzds2(ind,2)*yr*zr + d2xyzds2(ind,3)*yr*yr) + &
                                                              2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzds(ns2,3) + &
                                                              2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzds(ns2,2) + &
                                                              2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzds(ns2,2) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,2)*yr*zr + dxyzds(ns2,3)*yr*yr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,2)*yr*zr + dxyzds(ns1,3)*yr*yr)
                        drhoij2s(ind,20) = drhoij2s(ind,20) + yr*zr*zr*d2tds2(ind) + &
                                                              term0*(2.0_dp*d2xyzds2(ind,3)*yr*zr + d2xyzds2(ind,2)*zr*zr) + &
                                                              2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzds(ns2,2) + &
                                                              2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzds(ns2,3) + &
                                                              2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzds(ns2,3) + &
                                                              dtds(ns1)*(2.0_dp*dxyzds(ns2,3)*yr*zr + dxyzds(ns2,2)*zr*zr) + &
                                                              dtds(ns2)*(2.0_dp*dxyzds(ns1,3)*yr*zr + dxyzds(ns1,2)*zr*zr)
                        drhoij2s(ind,21) = drhoij2s(ind,21) + zr*zr*zr*d2tds2(ind) + term0*(3.0_dp*d2xyzds2(ind,3)*zr*zr) + &
                                                              term0*(6.0_dp*dxyzds(ns1,3)*dxyzds(ns2,3)*zr) + &
                                                              dtds(ns1)*(3.0_dp*dxyzds(ns2,3)*zr*zr) + &
                                                              dtds(ns2)*(3.0_dp*dxyzds(ns1,3)*zr*zr)
                      enddo
                    enddo
!
                    do nord = 12,21
                      drhoij2s(1,nord)  = drhoij2s(1,nord)  + 2.0_dp*ordtrm(nord)*dtds(1)
                      drhoij2s(3,nord)  = drhoij2s(3,nord)  + 2.0_dp*ordtrm(nord)*dtds(2)
                      drhoij2s(6,nord)  = drhoij2s(6,nord)  + 2.0_dp*ordtrm(nord)*dtds(3)
                      drhoij2s(8,nord)  = drhoij2s(8,nord)  + ordtrm(nord)*dtds(4)
                      drhoij2s(9,nord)  = drhoij2s(9,nord)  + ordtrm(nord)*dtds(4)
                      drhoij2s(10,nord) = drhoij2s(10,nord) + 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                      drhoij2s(11,nord) = drhoij2s(11,nord) + ordtrm(nord)*dtds(5)
                      drhoij2s(13,nord) = drhoij2s(13,nord) + ordtrm(nord)*dtds(5)
                      drhoij2s(14,nord) = drhoij2s(14,nord) + 0.5_dp*ordtrm(nord)*dtds(6)
                      drhoij2s(15,nord) = drhoij2s(15,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                      drhoij2s(16,nord) = drhoij2s(16,nord) + ordtrm(nord)*dtds(6)
                      drhoij2s(17,nord) = drhoij2s(17,nord) + ordtrm(nord)*dtds(6)
                      drhoij2s(19,nord) = drhoij2s(19,nord) + 0.5_dp*ordtrm(nord)*dtds(5)
                      drhoij2s(20,nord) = drhoij2s(20,nord) + 0.5_dp*ordtrm(nord)*dtds(4)
                      drhoij2s(21,nord) = drhoij2s(21,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                    enddo
!
                    do nc1 = 1,3
                      do ns1 = 1,6
                        drhoij2m(ns1,nc1,12) = drhoij2m(ns1,nc1,12) + xr*xr*xr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(3.0_dp*d2xyzdsdc(ns1,nc1,1)*xr*xr) + &
                                                                      term0*(6.0_dp*dxyzds(ns1,1)*dxyzdc(nc1,1)*xr) + &
                                                                      dtds(ns1)*(3.0_dp*dxyzdc(nc1,1)*xr*xr) + &
                                                                      dtdc(nc1)*(3.0_dp*dxyzds(ns1,1)*xr*xr) + &
                                                                      2.0_dp*xr*xr*xr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,13) = drhoij2m(ns1,nc1,13) + xr*xr*yr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,1)*xr*yr + &
                                                                             d2xyzdsdc(ns1,nc1,2)*xr*xr) + &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzdc(nc1,2) + &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzdc(nc1,1) + &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzdc(nc1,1) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,1)*xr*yr + dxyzdc(nc1,2)*xr*xr)+&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,1)*xr*yr + dxyzds(ns1,2)*xr*xr)+&
                                                                      2.0_dp*xr*xr*yr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,14) = drhoij2m(ns1,nc1,14) + xr*xr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,1)*xr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,3)*xr*xr) + &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,1)*dxyzdc(nc1,3) + &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzdc(nc1,1) + &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzdc(nc1,1) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,1)*xr*zr + dxyzdc(nc1,3)*xr*xr)+&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,1)*xr*zr + dxyzds(ns1,3)*xr*xr)+&
                                                                      2.0_dp*xr*xr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,15) = drhoij2m(ns1,nc1,15) + xr*yr*yr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,2)*xr*yr + &
                                                                             d2xyzdsdc(ns1,nc1,1)*yr*yr) + &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,1)*dxyzdc(nc1,2) + &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzdc(nc1,1) + &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,2)*dxyzdc(nc1,2) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,2)*xr*yr + dxyzdc(nc1,1)*yr*yr)+&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,2)*xr*yr + dxyzds(ns1,1)*yr*yr)+&
                                                                      2.0_dp*xr*yr*yr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,16) = drhoij2m(ns1,nc1,16) + xr*yr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(d2xyzdsdc(ns1,nc1,1)*yr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,2)*xr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,3)*xr*yr) + &
                                                                      term0*xr*(dxyzds(ns1,2)*dxyzdc(nc1,3) + &
                                                                                dxyzds(ns1,3)*dxyzdc(nc1,2)) + &
                                                                      term0*yr*(dxyzds(ns1,1)*dxyzdc(nc1,3) + &
                                                                                dxyzds(ns1,3)*dxyzdc(nc1,1)) + &
                                                                      term0*zr*(dxyzds(ns1,1)*dxyzdc(nc1,2) + &
                                                                                dxyzds(ns1,2)*dxyzdc(nc1,1)) + &
                                                                      dtds(ns1)*(dxyzdc(nc1,1)*yr*zr + &
                                                                                 dxyzdc(nc1,2)*xr*zr + &
                                                                                 dxyzdc(nc1,3)*xr*yr) + &
                                                                      dtdc(nc1)*(dxyzds(ns1,1)*yr*zr + &
                                                                                 dxyzds(ns1,2)*xr*zr + &
                                                                                 dxyzds(ns1,3)*xr*yr) + &
                                                                      2.0_dp*xr*yr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,17) = drhoij2m(ns1,nc1,17) + xr*zr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,3)*xr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,1)*zr*zr) + &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,1)*dxyzdc(nc1,3) + &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzdc(nc1,1) + &
                                                                      2.0_dp*term0*xr*dxyzds(ns1,3)*dxyzdc(nc1,3) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,3)*xr*zr + dxyzdc(nc1,1)*zr*zr)+&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,3)*xr*zr + dxyzds(ns1,1)*zr*zr)+&
                                                                      2.0_dp*xr*zr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,18) = drhoij2m(ns1,nc1,18) + yr*yr*yr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(3.0_dp*d2xyzdsdc(ns1,nc1,2)*yr*yr) + &
                                                                      term0*(6.0_dp*dxyzds(ns1,2)*dxyzdc(nc1,2)*yr) + &
                                                                      dtds(ns1)*(3.0_dp*dxyzdc(nc1,2)*yr*yr) + &
                                                                      dtdc(nc1)*(3.0_dp*dxyzds(ns1,2)*yr*yr) + &
                                                                      2.0_dp*yr*yr*yr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,19) = drhoij2m(ns1,nc1,19) + yr*yr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,2)*yr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,3)*yr*yr) + &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,2)*dxyzdc(nc1,3) + &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzdc(nc1,2) + &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzdc(nc1,2) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,2)*yr*zr + dxyzdc(nc1,3)*yr*yr)+&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,2)*yr*zr + dxyzds(ns1,3)*yr*yr)+&
                                                                      2.0_dp*yr*yr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,20) = drhoij2m(ns1,nc1,20) + yr*zr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(2.0_dp*d2xyzdsdc(ns1,nc1,3)*yr*zr + &
                                                                             d2xyzdsdc(ns1,nc1,2)*zr*zr) + &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,3)*dxyzdc(nc1,2) + &
                                                                      2.0_dp*term0*zr*dxyzds(ns1,2)*dxyzdc(nc1,3) + &
                                                                      2.0_dp*term0*yr*dxyzds(ns1,3)*dxyzdc(nc1,3) + &
                                                                      dtds(ns1)*(2.0_dp*dxyzdc(nc1,3)*yr*zr + dxyzdc(nc1,2)*zr*zr)+&
                                                                      dtdc(nc1)*(2.0_dp*dxyzds(ns1,3)*yr*zr + dxyzds(ns1,2)*zr*zr)+&
                                                                      2.0_dp*yr*zr*zr*term1*dxyznords(ns1,nc1)
                        drhoij2m(ns1,nc1,21) = drhoij2m(ns1,nc1,21) + zr*zr*zr*d2tdsdc(ns1,nc1) + &
                                                                      term0*(3.0_dp*d2xyzdsdc(ns1,nc1,3)*zr*zr) + &
                                                                      term0*(6.0_dp*dxyzds(ns1,3)*dxyzdc(nc1,3)*zr) + &
                                                                      dtds(ns1)*(3.0_dp*dxyzdc(nc1,3)*zr*zr) + &
                                                                      dtdc(nc1)*(3.0_dp*dxyzds(ns1,3)*zr*zr) + &
                                                                      2.0_dp*zr*zr*zr*term1*dxyznords(ns1,nc1)
                      enddo
                    enddo
                    if (lt24) then
                      ind = 0
                      do ns1 = 1,6
                        do ns2 = 1,ns1
                          ind = ind + 1
                          drhoij2s(ind,22) = drhoij2s(ind,22) + xr*d2tds2(ind) + term0*d2xyzds2(ind,1) + &
                                                                dxyzds(ns1,1)*dtds(ns2) + dxyzds(ns2,1)*dtds(ns1)
                          drhoij2s(ind,23) = drhoij2s(ind,23) + yr*d2tds2(ind) + term0*d2xyzds2(ind,2) + &
                                                                dxyzds(ns1,2)*dtds(ns2) + dxyzds(ns2,2)*dtds(ns1)
                          drhoij2s(ind,24) = drhoij2s(ind,24) + zr*d2tds2(ind) + term0*d2xyzds2(ind,3) + &
                                                                dxyzds(ns1,3)*dtds(ns2) + dxyzds(ns2,3)*dtds(ns1)
                        enddo
                      enddo
!
                      do nord = 22,24
                        drhoij2s(1,nord)  = drhoij2s(1,nord)  + 2.0_dp*ordtrm(nord)*dtds(1)
                        drhoij2s(3,nord)  = drhoij2s(3,nord)  + 2.0_dp*ordtrm(nord)*dtds(2)
                        drhoij2s(6,nord)  = drhoij2s(6,nord)  + 2.0_dp*ordtrm(nord)*dtds(3)
                        drhoij2s(8,nord)  = drhoij2s(8,nord)  + ordtrm(nord)*dtds(4)
                        drhoij2s(9,nord)  = drhoij2s(9,nord)  + ordtrm(nord)*dtds(4)
                        drhoij2s(10,nord) = drhoij2s(10,nord) + 0.5_dp*ordtrm(nord)*(dtds(2)+dtds(3))
                        drhoij2s(11,nord) = drhoij2s(11,nord) + ordtrm(nord)*dtds(5)
                        drhoij2s(13,nord) = drhoij2s(13,nord) + ordtrm(nord)*dtds(5)
                        drhoij2s(14,nord) = drhoij2s(14,nord) + 0.5_dp*ordtrm(nord)*dtds(6)
                        drhoij2s(15,nord) = drhoij2s(15,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(3))
                        drhoij2s(16,nord) = drhoij2s(16,nord) + ordtrm(nord)*dtds(6)
                        drhoij2s(17,nord) = drhoij2s(17,nord) + ordtrm(nord)*dtds(6)
                        drhoij2s(19,nord) = drhoij2s(19,nord) + 0.5_dp*ordtrm(nord)*dtds(5)
                        drhoij2s(20,nord) = drhoij2s(20,nord) + 0.5_dp*ordtrm(nord)*dtds(4)
                        drhoij2s(21,nord) = drhoij2s(21,nord) + 0.5_dp*ordtrm(nord)*(dtds(1)+dtds(2))
                      enddo
!
                      do nc1 = 1,3
                        do ns1 = 1,6
                          drhoij2m(ns1,nc1,22) = drhoij2m(ns1,nc1,22) + xr*d2tdsdc(ns1,nc1) + term0*d2xyzdsdc(ns1,nc1,1) + &
                                                                        dxyzds(ns1,1)*dtdc(nc1) + dxyzdc(nc1,1)*dtds(ns1) + &
                                                                        2.0_dp*xr*term1*dxyznords(ns1,nc1)
                          drhoij2m(ns1,nc1,23) = drhoij2m(ns1,nc1,23) + yr*d2tdsdc(ns1,nc1) + term0*d2xyzdsdc(ns1,nc1,2) + &
                                                                        dxyzds(ns1,2)*dtdc(nc1) + dxyzdc(nc1,2)*dtds(ns1) + &
                                                                        2.0_dp*yr*term1*dxyznords(ns1,nc1)
                          drhoij2m(ns1,nc1,24) = drhoij2m(ns1,nc1,24) + zr*d2tdsdc(ns1,nc1) + term0*d2xyzdsdc(ns1,nc1,3) + &
                                                                        dxyzds(ns1,3)*dtdc(nc1) + dxyzdc(nc1,3)*dtds(ns1) + &
                                                                        2.0_dp*zr*term1*dxyznords(ns1,nc1)
                        enddo
                      enddo
                    endif
                  endif
                endif
              endif
            endif
!
!  End loop over MEAM order
!
          enddo
        enddo
      endif
    endif
  enddo
!
  return
  end
