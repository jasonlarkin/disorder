  subroutine rsearch3D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
!
!  Subroutine that searches for valid distances - called from real space subroutines.
!
!  On input :
!
!  xcrd      = x-coordinate of i-j vector for central unit cell
!  ycrd      = y-coordinate of i-j vector for central unit cell
!  zcrd      = z-coordinate of i-j vector for central unit cell
!  lmolok    = whether it is possible for atoms to be in molecule
!  lcspair   = is this a possible core-shell pair
!  i         = number of atom i
!  j         = number of atom j
!  nmi       = molecule number for atom i
!  ixj       = molecule cell index in x direction
!  iyj       = molecule cell index in y direction
!  izj       = molecule cell index in z direction
!  cut2      = cut-off distance squared for this pair of atoms
!
!  On exit :
!
!  nor       = number of valid vectors between atoms
!  dist      = distance between atoms i and j for each vector
!  xtmp      = x component of valid i-j vector
!  ytmp      = y component of valid i-j vector
!  ztmp      = z component of valid i-j vector
!  cellindex = pointers to cell indices for each vector
!  lbonded   = logical for bonding/connectivity
!  l2bonds   = logical for bonding/connectivity
!  l3bonds   = logical for bonding/connectivity
!  lptrmol   = logical for bonding/connectivity
!  nmolonly  = vector within molecule if non-zero
!  lself     = if .true. then a self term was found and must be handled
!
!
!  l111    = if .true. all interactions lie within unit cell and immediately
!            adjacent cells only => use short cut
!
!   7/00 Created from reale/realsd/realsd2 code
!   2/01 Modified for general dimensionality
!   6/01 Setting of lsamemol changed
!   1/02 Handling of negative cell parameters added
!   1/02 Minimum image option incorporated
!  12/02 Nadd set to larger values for more extreme angles
!   1/03 Wolf modifications made
!   3/03 rfind called to improve general distance search
!   9/03 rfind used in lra case as well
!  11/04 intent added
!  11/04 Use of sqrt in l111 flag setting removed
!   1/05 rp removed as an argument since it is no longer needed
!   1/05 Storing of cell indices for valid vectors added in cellindex array
!  10/06 Default value of small decreased
!   3/07 Bonding types added
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!   1/08 Calls to bonded3c modified
!   2/09 Argument removed from changemaxdis call
!   3/09 small replaced by global value smallself from general module
!   3/12 Position of call to getbonds moved as it is needed for rfind3D
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
  use symmetry,    only : lra
  use two
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)  :: i
  integer(i4), intent(in)  :: ixj
  integer(i4), intent(in)  :: iyj
  integer(i4), intent(in)  :: izj
  integer(i4), intent(in)  :: j
  integer(i4), intent(in)  :: nmi
  integer(i4), intent(out) :: nmolonly
  integer(i4), intent(out) :: nor
  logical,     intent(in)  :: lcspair       
  logical,     intent(in)  :: lmolok
  logical,     intent(out) :: lself
  real(dp),    intent(in)  :: cut2
!
!  Local variables
!
  integer(i4)              :: ii
  integer(i4)              :: iim
  integer(i4)              :: iimn
  integer(i4)              :: iimx
  integer(i4)              :: jj
  integer(i4)              :: jjm
  integer(i4)              :: jjmn
  integer(i4)              :: jjmx
  integer(i4)              :: kk
  integer(i4)              :: kkm
  integer(i4)              :: kkmn
  integer(i4)              :: kkmx
  logical                  :: l111
  logical                  :: lsamemol
  real(dp)                 :: cmax          
  real(dp)                 :: cut2e 
  real(dp)                 :: cut2s
  real(dp)                 :: r2
  real(dp)                 :: r2min
  real(dp)                 :: xcd  
  real(dp)                 :: xcd2  
  real(dp)                 :: ycd  
  real(dp)                 :: ycd2
  real(dp)                 :: zcd  
  real(dp)                 :: xcdi
  real(dp)                 :: ycdi    
  real(dp)                 :: zcdi    
  real(dp)                 :: xcdj
  real(dp)                 :: ycdj    
  real(dp)                 :: zcdj    
  real(dp)                 :: xcrd    
  real(dp)                 :: ycrd          
  real(dp)                 :: zcrd  
  real(dp)                 :: xmin
  real(dp)                 :: ymin
  real(dp)                 :: zmin
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
  if (a*a.lt.cmax.or.b*b.lt.cmax.or.c*c.lt.cmax) l111 = .false.
  if (alpha.lt.80.0d0.or.beta.lt.80.0d0.or.gamma.lt.80.0d0) l111 = .false.
  if (alpha.gt.100.0d0.or.beta.gt.100.0d0.or.gamma.gt.100.0d0) l111 = .false.
!
!  Create unit vectors
!
!
!  Set looping limits
!
  if (l111) then
    iimn = 1
    iimx = 1
    jjmn = 1
    jjmx = 1
    kkmn = 1
    kkmx = 1
  endif
  if (lminimage) then
!***********************
!  Find minimum image  *
!***********************
    if (lra) then
      iim = dnint(xcrd/r1x)
      jjm = dnint(ycrd/r2y)
      kkm = dnint(zcrd/r3z)
      xmin = xcrd - r1x*iim
      ymin = ycrd - r2y*jjm
      zmin = zcrd - r3z*kkm
      r2min = xmin*xmin + ymin*ymin + zmin*zmin
    else
      r2min = 1.0d10
      xcdi = xcrd - 2.0_dp*r1x
      ycdi = ycrd - 2.0_dp*r1y
      zcdi = zcrd - 2.0_dp*r1z
      do ii = -1,1
        xcdi = xcdi + r1x
        ycdi = ycdi + r1y
        zcdi = zcdi + r1z
        xcdj = xcdi - 2.0_dp*r2x
        ycdj = ycdi - 2.0_dp*r2y
        zcdj = zcdi - 2.0_dp*r2z
        do jj = -1,1
          xcdj = xcdj + r2x
          ycdj = ycdj + r2y
          zcdj = zcdj + r2z
          xcd = xcdj - 2.0_dp*r3x
          ycd = ycdj - 2.0_dp*r3y
          zcd = zcdj - 2.0_dp*r3z
          do kk = -1,1
            xcd = xcd + r3x
            ycd = ycd + r3y
            zcd = zcd + r3z
            r2 = xcd*xcd + ycd*ycd + zcd*zcd
            if (r2.lt.r2min) then
              r2min = r2
              xmin = xcd
              ymin = ycd
              zmin = zcd
              iim = ii
              jjm = jj
              kkm = kk
            endif
          enddo
        enddo
      enddo
    endif
!***************************
!  Molecule - check index  *
!***************************
    if (lmolok.and.(r2min.gt.cut2s.or..not.lcspair)) then
      call bonded3(lbonded(nor+1),l2bonds(nor+1),l3bonds(nor+1),nbotype(nor+1),nbotype2(nor+1),i,j,iim,jjm,kkm)
      lsamemol = (lbonded(nor+1).or.l2bonds(nor+1).or.l3bonds(nor+1))
      if (.not.lsamemol) then
        call samemol(lsamemol,nmi,iim,jjm,kkm,ixj,iyj,izj)
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
      l3bonds(nor+1)  = .false.
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
      ytmp(nor) = ymin
      ztmp(nor) = zmin
      cellindex(1,nor) = iim
      cellindex(2,nor) = jjm
      cellindex(3,nor) = kkm
    endif
  elseif (lra) then
!********************
!  Find all images  *
!********************
!
!  Set up bonding
!
    if (lmolok) call getbonds(i,j)
!
!  Right angled cell
!
    if (l111) then
!
      xcd = xcrd - (iimn+1)*r1x
      do ii = - iimn,iimx
        xcd = xcd + r1x
        xcd2 = xcd*xcd
        ycd = ycrd - (jjmn+1)*r2y
        do jj = - jjmn,jjmx
          ycd = ycd + r2y
          ycd2 = ycd*ycd
          zcd = zcrd - (kkmn+1)*r3z
          do kk = - kkmn,kkmx
            zcd = zcd + r3z
            r2 = xcd2 + ycd2 + zcd*zcd
            if (nor.ge.maxdis) then
              maxdis = nor + 200
              call changemaxdis
            endif
!
!  Molecule - check index
!
            if (lmolok.and.(r2.gt.cut2s.or..not.lcspair)) then
              call bonded3c(lbonded(nor+1),l2bonds(nor+1),l3bonds(nor+1),nbotype(nor+1),nbotype2(nor+1),ii,jj,kk)
              lsamemol = (lbonded(nor+1).or.l2bonds(nor+1).or.l3bonds(nor+1))
              if (.not.lsamemol) then
                call samemol(lsamemol,nmi,ii,jj,kk,ixj,iyj,izj)
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
              cellindex(2,nor) = jj
              cellindex(3,nor) = kk
            endif
          enddo
        enddo
      enddo
    else
      call rfind3D(xcrd,ycrd,zcrd,cut2,cut2e,cut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lself,0_i4,nor)
    endif
  else
!
!  Set up bonding
!
    if (lmolok) call getbonds(i,j)
!
!  General cell
!
    if (l111) then
!
      xcdi = xcrd - (iimn + 1)*r1x
      ycdi = ycrd - (iimn + 1)*r1y
      zcdi = zcrd - (iimn + 1)*r1z
!
!  Loop over unit cells to find interatomic distances
!
      do ii = - iimn,iimx
        xcdi = xcdi + r1x
        ycdi = ycdi + r1y
        zcdi = zcdi + r1z
!
        xcdj = xcdi - (jjmn + 1)*r2x
        ycdj = ycdi - (jjmn + 1)*r2y
        zcdj = zcdi - (jjmn + 1)*r2z
        do jj = - jjmn,jjmx
          xcdj = xcdj + r2x
          ycdj = ycdj + r2y
          zcdj = zcdj + r2z
          xcd = xcdj - (kkmn+1)*r3x
          ycd = ycdj - (kkmn+1)*r3y
          zcd = zcdj - (kkmn+1)*r3z
          do kk = - kkmn,kkmx
            xcd = xcd + r3x
            ycd = ycd + r3y
            zcd = zcd + r3z
            r2 = xcd*xcd + ycd*ycd + zcd*zcd
            if (nor.ge.maxdis) then
              maxdis = nor + 200
              call changemaxdis
            endif
!
!  Molecule - check index
!
            if (lmolok.and.(r2.gt.cut2s.or..not.lcspair)) then
              call bonded3c(lbonded(nor+1),l2bonds(nor+1),l3bonds(nor+1),nbotype(nor+1),nbotype2(nor+1),ii,jj,kk)
              lsamemol = (lbonded(nor+1).or.l2bonds(nor+1).or.l3bonds(nor+1))
              if (.not.lsamemol) then
                call samemol(lsamemol,nmi,ii,jj,kk,ixj,iyj,izj)
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
              cellindex(2,nor) = jj
              cellindex(3,nor) = kk
            endif
          enddo
        enddo
      enddo
    else
      call rfind3D(xcrd,ycrd,zcrd,cut2,cut2e,cut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lself,0_i4,nor)
    endif
  endif
!
  return
  end
