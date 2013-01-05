  subroutine rsearch1D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
!
!  Subroutine that searches for valid distances - called from real
!  space subroutines.
!
!  On input :
!
!  xcrd    = x-coordinate of i-j vector for central unit cell
!  ycrd    = y-coordinate of i-j vector for central unit cell
!  zcrd    = z-coordinate of i-j vector for central unit cell
!  lmolok  = whether it is possible for atoms to be in molecule
!  lcspair = is this a possible core-shell pair
!  i       = number of atom i
!  j       = number of atom j
!  nmi     = molecule number for atom i
!  ixj     = molecule cell index in x direction
!  iyj     = molecule cell index in y direction
!  izj     = molecule cell index in z direction
!  cut2    = cut-off distance squared for this pair of atoms
!
!  On exit :
!
!  nor     = number of valid vectors between atoms
!  dist    = distance between atoms i and j for each vector
!  xtmp    = x component of valid i-j vector
!  ytmp    = y component of valid i-j vector
!  ztmp    = z component of valid i-j vector
!  lbonded = logical for bonding/connectivity
!  l2bonds = logical for bonding/connectivity
!  l3bonds = logical for bonding/connectivity
!  lptrmol = logical for bonding/connectivity
!  nmolonly= vector within molecule if non-zero
!  lself   = if .true. then a self term was found and must be handled
!
!
!  l111    = if .true. all interactions lie within unit cell and immediately
!            adjacent cells only => use short cut
!
!   2/01 Created from rsearch3D
!   6/01 Setting of lsamemol altered
!   1/02 Handling of negative cell parameters added
!   1/02 Minimum image option incorporated
!   1/03 Wolf modifications made
!   3/03 Call to rfind replaces general case
!   3/03 lra code removed since this will always be true for 1-D
!  11/04 sqrt cut2e removed
!   1/05 rp removed as an argument since it is no longer needed
!   1/05 Storage of cell indices added for valid vectors
!  10/06 Default value of small lowered
!   3/07 Bonding types added
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!   2/09 Argument removed from changemaxdis call
!   3/09 small replaced by global value smallself from general module
!   3/12 Call to getbonds added as it is needed for rfind1D
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
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, March 2012
!
  use control,     only : lminimage, lwolf
  use current
  use general
  use kspace
  use molecule
  use realvectors
  use shell
  use two
  implicit none
!
!  Passed variables
!
  integer(i4)        :: i
  integer(i4)        :: ixj
  integer(i4)        :: iyj
  integer(i4)        :: izj
  integer(i4)        :: j
  integer(i4)        :: nmi
  integer(i4)        :: nmolonly
  integer(i4)        :: nor
  logical            :: lcspair
  logical            :: lmolok
  logical            :: lself
  real(dp)           :: cut2
!
!  Local variables
!
  integer(i4)        :: ii
  integer(i4)        :: iim
  logical            :: l111
  logical            :: lsamemol
  real(dp)           :: cmax
  real(dp)           :: cut2e
  real(dp)           :: cut2s
  real(dp)           :: r2
  real(dp)           :: r2min
  real(dp)           :: xcd
  real(dp)           :: ycd
  real(dp)           :: zcd
  real(dp)           :: xcrd
  real(dp)           :: ycrd
  real(dp)           :: zcrd
  real(dp)           :: ycrd2
  real(dp)           :: zcrd2
  real(dp)           :: xmin
!
!  Initialise return variables
!
  nor = 0
  nmolonly = 0
  lself = .false.
!     
!  For minimum image we must exclude i=j case since there is no unique nearest image
!     
  if (lminimage) then
    if (i.eq.j) then
      lself = .true.
      return   
    endif 
  endif
!***************************
!  Set up local variables  *
!***************************
!
!  Set up cutoffs
!
  if (lwolf) then
    cut2e = cutw*cutw
  else
    cut2e = rmx2
  endif
  cut2s = cuts*cuts
!
!  Decide whether all interactions lie within unit cell and first
!  neighbours - saves time for large systems
!
  cmax = max(cut2,cut2e)
  l111 = .true.
  if (a*a.lt.cmax) l111 = .false.
!********************
!  Looping section  *
!********************
  ycrd2 = ycrd*ycrd
  zcrd2 = zcrd*zcrd
  if (lminimage) then
!***********************
!  Find minimum image  *
!***********************
    iim = dnint(xcrd/r1x)
    xmin = xcrd - r1x*iim
    r2min = xmin*xmin + ycrd2 + zcrd2
!***************************  
!  Molecule - check index  *
!***************************
    if (lmolok.and.(r2min.gt.cut2s.or..not.lcspair)) then
      call bonded3(lbonded(nor+1),l2bonds(nor+1),l3bonds(nor+1),nbotype(nor+1),nbotype2(nor+1),i,j,iim,0_i4,0_i4)
      lsamemol = (lbonded(nor+1).or.l2bonds(nor+1).or.l3bonds(nor+1))
      if (.not.lsamemol) then
        call samemol(lsamemol,nmi,iim,0_i4,0_i4,ixj,iyj,izj)
      endif 
      lptrmol(nor+1) = lsamemol
      if (lsamemol) then
        if (r2min.gt.cut2e) nmolonly = 1
      else
        lbonded(nor+1) = .false.
        l2bonds(nor+1) = .false.
        l3bonds(nor+1) = .false.
      endif
    else
      lptrmol(nor+1)  = .false.
      lbonded(nor+1)  = .false.
      l2bonds(nor+1)  = .false.
      nbotype(nor+1)  = 0
      nbotype2(nor+1) = 0
    endif
!***************************
!  Check cut-off distance  *
!***************************
    if (r2min.lt.smallself) then
      lself = .true.
    elseif (r2min.le.cut2.or.lptrmol(nor+1)) then
!           
!  Store vector
!         
      nor = 1
      dist(nor) = r2min
      xtmp(nor) = xmin
      ytmp(nor) = ycrd
      ztmp(nor) = zcrd
      cellindex(1,nor) = iim
      cellindex(2,nor) = 0
      cellindex(3,nor) = 0
    endif
!********************
!  Find all images  *
!********************
  else
!
!  General cell
!
    if (l111) then
      xcd = xcrd - 2.0_dp*r1x
      ycd = ycrd - 2.0_dp*r1y
      zcd = zcrd - 2.0_dp*r1z
!
!  Loop over unit cells to find interatomic distances
!
      do ii = -1,1
        xcd = xcd + r1x
        ycd = ycd + r1y
        zcd = zcd + r1z
!
        r2 = xcd*xcd + ycd*ycd + zcd*zcd
        if (nor.ge.maxdis) then
          maxdis = nor + 200
          call changemaxdis
        endif
!
!  Molecule - check index
!
        if (lmolok.and.(r2.gt.cut2s.or..not.lcspair)) then
          call bonded3(lbonded(nor+1),l2bonds(nor+1),l3bonds(nor+1),nbotype(nor+1),nbotype2(nor+1),i,j,ii,0_i4,0_i4)
          lsamemol = (lbonded(nor+1).or.l2bonds(nor+1).or.l3bonds(nor+1))
          if (.not.lsamemol) then
            call samemol(lsamemol,nmi,ii,0_i4,0_i4,ixj,iyj,izj)
          endif
          lptrmol(nor+1) = lsamemol
          if (lsamemol) then
            if (r2.gt.cut2e) nmolonly = nor + 1
          else
            lbonded(nor+1) = .false.
            l2bonds(nor+1) = .false.
            l3bonds(nor+1) = .false.
          endif
        else
          lptrmol(nor+1)  = .false.
          lbonded(nor+1)  = .false.
          l2bonds(nor+1)  = .false.
          l3bonds(nor+1)  = .false.
          nbotype(nor+1)  = 0
          nbotype2(nor+1) = 0
        endif
        if (r2.lt.smallself) then
!
!  Self term - set flag to indicate it was found
!
          lself = .true.
        elseif (r2.le.cut2.or.lptrmol(nor+1)) then
!
!  Store vector
!
          nor = nor + 1
          dist(nor) = r2
          xtmp(nor) = xcd
          ytmp(nor) = ycd
          ztmp(nor) = zcd
          cellindex(1,nor) = ii
          cellindex(2,nor) = 0
          cellindex(3,nor) = 0
        endif
      enddo
    else
!
!  Set up bonds
!
      call getbonds(i,j)
!
      call rfind1D(xcrd,ycrd,zcrd,cut2,cut2e,cut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lself,0_i4,nor)
    endif
  endif
!
  return
  end
