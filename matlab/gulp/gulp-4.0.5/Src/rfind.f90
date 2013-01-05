!******************************************************
!  Interfaces to routines for correct dimensionality  *
!******************************************************
  subroutine rfind(xij0,yij0,zij0,rcut2,rcut2e,rcut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid distances for a general cell.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xij0         = x-coordinate of i-j vector for central unit cell
!  yij0         = y-coordinate of i-j vector for central unit cell
!  zij0         = z-coordinate of i-j vector for central unit cell
!  rcut2        = cut-off distance for this pair of atoms squared
!  rcut2e       = electrostatic cut-off distance for this pair of atoms squared
!  rcut2s       = core-shell cut-off distance for this pair of atoms squared
!  lcspair      = is this is a potential core-shell pair?
!  lmolok       = do we need to do molecule checks?
!  nmi          = molecule number for i/j
!  i            = atom number for i
!  j            = atom number for j
!  ixj          = difference in first cell index between i and j for molecule
!  iyj          = difference in second cell index between i and j for molecule
!  izj          = difference in third cell index between i and j for molecule
!  nvec0        = initial location for vectors - 1
!
!  On exit :
!
!  lincludeself = if .true. then include self-term
!  nmolonly     = vector within molecule if nonzero
!  nvec         = number of valid vectors between atoms
!  dist         = distance between atoms i and j for each vector
!  xtmp         = x component of valid i-j vector
!  ytmp         = y component of valid i-j vector
!  ztmp         = z component of valid i-j vector
!
!   3/03 Created
!   3/07 Bondtype arrays added
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
!  Julian Gale, NRI, Curtin University, March 2007
!
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: i
  integer(i4), intent(in)    :: ixj
  integer(i4), intent(in)    :: iyj
  integer(i4), intent(in)    :: izj
  integer(i4), intent(in)    :: j
  integer(i4), intent(in)    :: nmi
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nmolonly
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lcspair
  logical,     intent(out)   :: lincludeself
  logical,     intent(in)    :: lmolok
  real(dp),    intent(in)    :: rcut2
  real(dp),    intent(in)    :: rcut2e
  real(dp),    intent(in)    :: rcut2s
  real(dp),    intent(in)    :: xij0
  real(dp),    intent(in)    :: yij0
  real(dp),    intent(in)    :: zij0
!
  if (ndim.eq.3) then
    call rfind3D(xij0,yij0,zij0,rcut2,rcut2e,rcut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lincludeself,nvec0,nvec)
  elseif (ndim.eq.2) then
    call rfind2D(xij0,yij0,zij0,rcut2,rcut2e,rcut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lincludeself,nvec0,nvec)
  elseif (ndim.eq.1) then
    call rfind1D(xij0,yij0,zij0,rcut2,rcut2e,rcut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lincludeself,nvec0,nvec)
  endif
!
  return
  end
!
  subroutine rfindeither(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of either atom i or atom j.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xik0         = x-coordinate of i-k vector for central unit cell
!  yik0         = y-coordinate of i-k vector for central unit cell
!  zik0         = z-coordinate of i-k vector for central unit cell
!  xjk0         = x-coordinate of j-k vector for central unit cell
!  yjk0         = y-coordinate of j-k vector for central unit cell
!  zjk0         = z-coordinate of j-k vector for central unit cell
!  rcut2i       = cut-off distance for i to k squared
!  rcut2j       = cut-off distance for j to k squared
!  nvec0        = initial location of vectors - 1
!  lincludeself = if .true. then include self-term
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff 
!                 of i or j
!  dist         = distance between atoms i and k for each vector
!  dist2        = distance between atoms j and k for each vector
!  xtmp         = x component of valid i-k vector
!  ytmp         = y component of valid i-k vector
!  ztmp         = z component of valid i-k vector
!  xtmp2        = x component of valid j-k vector
!  ytmp2        = y component of valid j-k vector
!  ztmp2        = z component of valid j-k vector
!
!   3/03 Created
!  12/07 lincludeself intent changed to in
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
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: xik0
  real(dp),    intent(in)    :: yik0
  real(dp),    intent(in)    :: zik0
  real(dp),    intent(in)    :: xjk0
  real(dp),    intent(in)    :: yjk0
  real(dp),    intent(in)    :: zjk0
!
  if (ndim.eq.3) then
    call rfindeither3D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec)
  elseif (ndim.eq.2) then
    call rfindeither2D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec)
  elseif (ndim.eq.1) then
    call rfindeither1D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec)
  endif
!
  return
  end
!
  subroutine rfindeither3(xil0,yil0,zil0,xjl0,yjl0,zjl0,xkl0,ykl0,zkl0,rcut2i,rcut2j,rcut2k,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom l that are 
!  within a cutoff of either atom i or atom j or atom k.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xil0         = x-coordinate of i-l vector for central unit cell
!  yil0         = y-coordinate of i-l vector for central unit cell
!  zil0         = z-coordinate of i-l vector for central unit cell
!  xjl0         = x-coordinate of j-l vector for central unit cell
!  yjl0         = y-coordinate of j-l vector for central unit cell
!  zjl0         = z-coordinate of j-l vector for central unit cell
!  xkl0         = x-coordinate of k-l vector for central unit cell
!  ykl0         = y-coordinate of k-l vector for central unit cell
!  zkl0         = z-coordinate of k-l vector for central unit cell
!  rcut2i       = cut-off distance for i to l squared
!  rcut2j       = cut-off distance for j to l squared
!  rcut2k       = cut-off distance for k to l squared
!  nvec0        = initial location of vectors - 1
!  lincludeself = if .true. then include self-term
!
!  On exit :
!
!  nvec         = number of valid images of l that are within cutoff 
!                 of i or j or k
!  dist         = distance between atoms i and l for each vector
!  dist2        = distance between atoms j and l for each vector
!  dist3        = distance between atoms k and l for each vector
!  xtmp         = x component of valid i-l vector
!  ytmp         = y component of valid i-l vector
!  ztmp         = z component of valid i-l vector
!  xtmp2        = x component of valid j-l vector
!  ytmp2        = y component of valid j-l vector
!  ztmp2        = z component of valid j-l vector
!  xtmp3        = x component of valid j-l vector
!  ytmp3        = y component of valid j-l vector
!  ztmp3        = z component of valid j-l vector
!
!   3/03 Created
!  12/07 lincludeself intent changed to in
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
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: rcut2k
  real(dp),    intent(in)    :: xil0
  real(dp),    intent(in)    :: yil0
  real(dp),    intent(in)    :: zil0
  real(dp),    intent(in)    :: xjl0
  real(dp),    intent(in)    :: yjl0
  real(dp),    intent(in)    :: zjl0
  real(dp),    intent(in)    :: xkl0
  real(dp),    intent(in)    :: ykl0
  real(dp),    intent(in)    :: zkl0
!
  if (ndim.eq.3) then
    call rfindeither33D(xil0,yil0,zil0,xjl0,yjl0,zjl0,xkl0,ykl0,zkl0,rcut2i,rcut2j,rcut2k,lincludeself,nvec0,nvec)
  elseif (ndim.eq.2) then
    call rfindeither32D(xil0,yil0,zil0,xjl0,yjl0,zjl0,xkl0,ykl0,zkl0,rcut2i,rcut2j,rcut2k,lincludeself,nvec0,nvec)
  elseif (ndim.eq.1) then
    call rfindeither31D(xil0,yil0,zil0,xjl0,yjl0,zjl0,xkl0,ykl0,zkl0,rcut2i,rcut2j,rcut2k,lincludeself,nvec0,nvec)
  endif
!
  return
  end
!********************************************************
!  Rfind3D : Get any image relative to a single atom i  *
!********************************************************
  subroutine rfind3D(xij0,yij0,zij0,rcut2,rcut2e,rcut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid distances for a general cell.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xij0         = x-coordinate of i-j vector for central unit cell
!  yij0         = y-coordinate of i-j vector for central unit cell
!  zij0         = z-coordinate of i-j vector for central unit cell
!  rcut2        = cut-off distance for this pair of atoms squared
!  rcut2e       = electrostatic cut-off distance for this pair of atoms squared
!  rcut2s       = core-shell cut-off distance for this pair of atoms squared
!  lcspair      = is this is a potential core-shell pair?
!  lmolok       = do we need to do molecule checks?
!  i            = atom number for i
!  j            = atom number for j
!  nmi          = molecule number for i/j
!  ixj          = difference in first cell index between i and j for molecule
!  iyj          = difference in second cell index between i and j for molecule
!  izj          = difference in third cell index between i and j for molecule
!  nvec0        = initial location for vectors - 1
!
!  On exit :
!
!  lincludeself = if .true. then include self-term
!  nvec         = number of valid vectors between atoms
!  dist         = distance between atoms i and j for each vector
!  xtmp         = x component of valid i-j vector
!  ytmp         = y component of valid i-j vector
!  ztmp         = z component of valid i-j vector
!
!   3/03 Created
!   4/04 Bonding setup added through call to getbonds
!  11/04 Calculation of cell norms eliminated
!   1/05 Storage of valid cell indices added
!   2/07 Bonding types added
!   1/08 Calls to bonded3c modified
!   2/09 Argument removed from changemaxdis call
!  10/11 Safety margin added for angles that deviate from right angle
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
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: i
  integer(i4), intent(in)    :: ixj
  integer(i4), intent(in)    :: iyj
  integer(i4), intent(in)    :: izj
  integer(i4), intent(in)    :: j
  integer(i4), intent(in)    :: nmi
  integer(i4), intent(out)   :: nmolonly
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lcspair
  logical,     intent(out)   :: lincludeself
  logical,     intent(in)    :: lmolok
  real(dp),    intent(in)    :: rcut2
  real(dp),    intent(in)    :: rcut2e
  real(dp),    intent(in)    :: rcut2s
  real(dp),    intent(in)    :: xij0
  real(dp),    intent(in)    :: yij0
  real(dp),    intent(in)    :: zij0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: ijx
  integer(i4)                :: ijy
  integer(i4)                :: ijz
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: kdir
  integer(i4)                :: kk
  integer(i4)                :: kmid
  integer(i4)                :: nallfound1
  integer(i4)                :: nallfound2
  integer(i4)                :: nallfound3
  integer(i4)                :: nsafety
  integer(i4)                :: nvecj0
  integer(i4)                :: nveck0
  logical                    :: aOK
  logical                    :: bOK
  logical                    :: cOK
  logical                    :: lallfound1
  logical                    :: lallfound2
  logical                    :: lallfound3
  logical                    :: lsamemol
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: proj3
  real(dp)                   :: r2
  real(dp)                   :: r2i
  real(dp)                   :: r2j
  real(dp)                   :: r2k
  real(dp)                   :: rcx1
  real(dp)                   :: rcx2
  real(dp)                   :: rcx3
  real(dp)                   :: rcy1
  real(dp)                   :: rcy2
  real(dp)                   :: rcy3
  real(dp)                   :: rcz1
  real(dp)                   :: rcz2
  real(dp)                   :: rcz3
  real(dp)                   :: rnorm
  real(dp)                   :: rx
  real(dp)                   :: ry
  real(dp)                   :: rz
  real(dp)                   :: rxi
  real(dp)                   :: ryi
  real(dp)                   :: rzi
  real(dp)                   :: rxj
  real(dp)                   :: ryj
  real(dp)                   :: rzj
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
!
!  Initialise variables
!
  lincludeself = .false.
!
!  nsafety is the number of search loops allowed without finding a new distance
!
  aOK = (abs(alpha-90.0_dp).lt.5.0_dp)
  bOK = (abs(beta-90.0_dp) .lt.5.0_dp)
  cOK = (abs(gamma-90.0_dp).lt.5.0_dp)
  if (aOK.and.bOK.and.cOK) then
    nsafety = 1
  elseif (aOK.and.bOK.or.aOK.and.cOK.or.bOK.and.cOK) then
    nsafety = 2
  elseif (aOK.or.bOK.or.cOK) then
    nsafety = 3
  else
    nsafety = 4
  endif
!
!  Find image of j nearest to i 
!
  xij = xij0
  yij = yij0
  zij = zij0
!     
!  Find projection of cell vector 3 on to i - j vector
!     
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj3 = rnorm*recipc*(xij*r3x + yij*r3y + zij*r3z)
  kmid = nint(proj3)
  xij = xij - kmid*r3x
  yij = yij - kmid*r3y 
  zij = zij - kmid*r3z
!     
!  Find projection of cell vector 2 on to i - j vector
!     
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj2 = rnorm*recipb*(xij*r2x + yij*r2y + zij*r2z)
  jmid = nint(proj2) 
  xij = xij - jmid*r2x
  yij = yij - jmid*r2y
  zij = zij - jmid*r2z
!     
!  Find projection of cell vector 1 on to i - j vector
!     
  rnorm = xij*xij + yij*yij + zij*zij 
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
  xij = xij - imid*r1x
  yij = yij - imid*r1y
  zij = zij - imid*r1z
!
!  Adjust molecule indices for coordinate shift
!
  ijx = ixj + imid
  ijy = iyj + jmid
  ijz = izj + kmid
!
!  Set up bonding
!
  if (lmolok) call getbonds(i,j)
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!     
!  Reinitialise distance squared 
!     
    r2i = 10000.0_dp*rcut2
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    nallfound1 = 0
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vector
!
    rxi = xij + dble(ii)*r1x
    ryi = yij + dble(ii)*r1y
    rzi = zij + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Save number of vectors before search over second direction
!
      nvecj0 = nvec
!
!  Outer loop over second cell vector direction 
!
      do jdir = 1,-1,-2
!     
!  Reinitialise saved distance squared
!     
        r2j = 10000.0_dp*rcut2
!
!  Loop over second cell vector
!
        lallfound2 = .false.
        nallfound2 = 0
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vector
!
        rxj = rxi + dble(jj)*r2x
        ryj = ryi + dble(jj)*r2y
        rzj = rzi + dble(jj)*r2z
!
!  Set increment vector
!
        rcx2 = dble(jdir)*r2x
        rcy2 = dble(jdir)*r2y
        rcz2 = dble(jdir)*r2z
!
        do while (.not.lallfound2)
!
!  Save number of vectors before search over third direction
!
          nveck0 = nvec
!
!  Outer loop over third cell vector direction 
!
          do kdir = 1,-1,-2
!
!  Reinitialise saved distance squared
!           
            r2k = 10000.0_dp*rcut2
!
!  Loop over third cell vector
!
            lallfound3 = .false.
            nallfound3 = 0
            if (kdir.eq.1) then
              kk = 0
            else
              kk = - 1
            endif
!
!  Set initial coordinate vector
!
            rx = rxj + dble(kk)*r3x
            ry = ryj + dble(kk)*r3y
            rz = rzj + dble(kk)*r3z
!
!  Set increment vector
!
            rcx3 = dble(kdir)*r3x
            rcy3 = dble(kdir)*r3y
            rcz3 = dble(kdir)*r3z
!
            do while (.not.lallfound3)
!
!  Calculate square of distance
!
              r2 = rx*rx + ry*ry + rz*rz
!
!  Molecule - check index
!
              if (lmolok.and.(r2.gt.rcut2s.or..not.lcspair)) then
                call bonded3c(lbonded(nvec0+nvec+1),l2bonds(nvec0+nvec+1),l3bonds(nvec0+nvec+1),  &
                              nbotype(nvec0+nvec+1),nbotype2(nvec0+nvec+1),ii-imid,jj-jmid,kk-kmid)
                lsamemol = (lbonded(nvec0+nvec+1).or.l2bonds(nvec0+nvec+1).or.l3bonds(nvec0+nvec+1))
                if (.not.lsamemol) then
                  call samemol(lsamemol,nmi,ii,jj,kk,ijx,ijy,ijz)
                endif
                lptrmol(nvec0+nvec+1) = lsamemol
                if (lsamemol) then
                  if (r2.gt.rcut2e) nmolonly = nvec0 + nvec + 1
                else
                  lbonded(nvec0+nvec+1) = .false.
                  l2bonds(nvec0+nvec+1) = .false.
                  l3bonds(nvec0+nvec+1) = .false.
                endif
              else 
                lptrmol(nvec0+nvec+1)  = .false.
                lbonded(nvec0+nvec+1)  = .false.
                l2bonds(nvec0+nvec+1)  = .false.
                l3bonds(nvec0+nvec+1)  = .false.
                nbotype(nvec0+nvec+1)  = 0
                nbotype2(nvec0+nvec+1) = 0
              endif
!
!  Check distance squared against cutoff squared
!
              if (r2.le.rcut2) then
                if (r2.gt.1.0d-12) then
                  nvec = nvec + 1
                  if (nvec0+nvec.ge.maxdis) then
                    maxdis = nvec0 + nvec + 50
                    call changemaxdis
                  endif
                  dist(nvec0+nvec) = r2
                  xtmp(nvec0+nvec) = rx
                  ytmp(nvec0+nvec) = ry
                  ztmp(nvec0+nvec) = rz
                  cellindex(1,nvec0+nvec) = ii - imid
                  cellindex(2,nvec0+nvec) = jj - jmid
                  cellindex(3,nvec0+nvec) = kk - kmid
                else
                  lincludeself = .true.
                endif
              endif
!
!  Increment by third vector
!
              kk = kk + kdir
              rx = rx + rcx3
              ry = ry + rcy3
              rz = rz + rcz3
!                     
!  Check to see if this direction is complete
!                     
              if (r2.gt.r2k.and.r2.gt.rcut2) nallfound3 = nallfound3 + 1
              lallfound3 = (nallfound3.eq.nsafety)
              r2k = r2
            enddo
          enddo
!
!  Increment by second vector
!
          jj = jj + jdir
          rxj = rxj + rcx2
          ryj = ryj + rcy2
          rzj = rzj + rcz2
!
!  Check to see if this direction is complete
!
          if (r2.gt.r2j.and.r2.gt.rcut2.and.nvec.eq.nveck0) nallfound2 = nallfound2 + 1
          lallfound2 = (nallfound2.eq.nsafety)
          r2j = r2
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxi = rxi + rcx1
      ryi = ryi + rcy1
      rzi = rzi + rcz1
!
!  Check to see if this direction is complete
!
      if (r2.gt.r2i.and.r2.gt.rcut2.and.nvec.eq.nvecj0) nallfound1 = nallfound1 + 1
      lallfound1 = (nallfound1.eq.nsafety)
      r2i = r2
    enddo
  enddo
!
  return
  end
!******************************************************
!  Rfindeither3D : Get any image common to two atoms  *
!******************************************************
  subroutine rfindeither3D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of either atom i or atom j.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xik0         = x-coordinate of i-k vector for central unit cell
!  yik0         = y-coordinate of i-k vector for central unit cell
!  zik0         = z-coordinate of i-k vector for central unit cell
!  xjk0         = x-coordinate of j-k vector for central unit cell
!  yjk0         = y-coordinate of j-k vector for central unit cell
!  zjk0         = z-coordinate of j-k vector for central unit cell
!  rcut2i       = cut-off distance for i to k squared
!  rcut2j       = cut-off distance for j to k squared
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff 
!                 of i or j
!  dist         = distance between atoms i and k for each vector
!  dist2        = distance between atoms j and k for each vector
!  xtmp         = x component of valid i-k vector
!  ytmp         = y component of valid i-k vector
!  ztmp         = z component of valid i-k vector
!  xtmp2        = x component of valid j-k vector
!  ytmp2        = y component of valid j-k vector
!  ztmp2        = z component of valid j-k vector
!
!   3/03 Created
!  11/04 Calculation of cell norms eliminated
!   2/09 Argument removed from changemaxdis call
!  10/11 Safety margin added for angles that deviate from right angle
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
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: xik0
  real(dp),    intent(in)    :: yik0
  real(dp),    intent(in)    :: zik0
  real(dp),    intent(in)    :: xjk0
  real(dp),    intent(in)    :: yjk0
  real(dp),    intent(in)    :: zjk0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: kdir
  integer(i4)                :: kk
  integer(i4)                :: kmid
  integer(i4)                :: nallfound1
  integer(i4)                :: nallfound2
  integer(i4)                :: nallfound3
  integer(i4)                :: nsafety
  integer(i4)                :: nvecj0
  integer(i4)                :: nveck0
  logical                    :: aOK
  logical                    :: bOK
  logical                    :: cOK
  logical                    :: lallfound1
  logical                    :: lallfound2
  logical                    :: lallfound3
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: proj3
  real(dp)                   :: r2ik
  real(dp)                   :: r2jk
  real(dp)                   :: r2iki
  real(dp)                   :: r2jki
  real(dp)                   :: r2ikj
  real(dp)                   :: r2jkj
  real(dp)                   :: r2ikk
  real(dp)                   :: r2jkk
  real(dp)                   :: rcx1
  real(dp)                   :: rcx2
  real(dp)                   :: rcx3
  real(dp)                   :: rcy1
  real(dp)                   :: rcy2
  real(dp)                   :: rcy3
  real(dp)                   :: rcz1
  real(dp)                   :: rcz2
  real(dp)                   :: rcz3
  real(dp)                   :: rnorm
  real(dp)                   :: rxik
  real(dp)                   :: ryik
  real(dp)                   :: rzik
  real(dp)                   :: rxjk
  real(dp)                   :: ryjk
  real(dp)                   :: rzjk
  real(dp)                   :: rxiki
  real(dp)                   :: ryiki
  real(dp)                   :: rziki
  real(dp)                   :: rxikj
  real(dp)                   :: ryikj
  real(dp)                   :: rzikj
  real(dp)                   :: rxjki
  real(dp)                   :: ryjki
  real(dp)                   :: rzjki
  real(dp)                   :: rxjkj
  real(dp)                   :: ryjkj
  real(dp)                   :: rzjkj
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xik
  real(dp)                   :: yik
  real(dp)                   :: zik
  real(dp)                   :: xjk
  real(dp)                   :: yjk
  real(dp)                   :: zjk
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  nsafety is the number of search loops allowed without finding a new distance
!
  aOK = (abs(alpha-90.0_dp).lt.5.0_dp)
  bOK = (abs(beta-90.0_dp) .lt.5.0_dp)
  cOK = (abs(gamma-90.0_dp).lt.5.0_dp)
  if (aOK.and.bOK.and.cOK) then
    nsafety = 1
  elseif (aOK.and.bOK.or.aOK.and.cOK.or.bOK.and.cOK) then
    nsafety = 2
  elseif (aOK.or.bOK.or.cOK) then
    nsafety = 3
  else
    nsafety = 4
  endif
!
!  Copy input vectors
!
  xik = xik0
  yik = yik0
  zik = zik0
  xjk = xjk0
  yjk = yjk0
  zjk = zjk0
!
!  Find mid point of i - j vector to k
!
  xij = 0.5_dp*(xjk + xik)
  yij = 0.5_dp*(yjk + yik)
  zij = 0.5_dp*(zjk + zik)
!
!  Find projection of cell vector 3 on to mid point - k vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj3 = rnorm*recipc*(xij*r3x + yij*r3y + zij*r3z)
  kmid = nint(proj3)
  xij = xij - kmid*r3x
  yij = yij - kmid*r3y
  zij = zij - kmid*r3z
!
!  Find projection of cell vector 2 on to mid point - k vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj2 = rnorm*recipb*(xij*r2x + yij*r2y + zij*r2z)
  jmid = nint(proj2)
  xij = xij - jmid*r2x
  yij = yij - jmid*r2y
  zij = zij - jmid*r2z
!
!  Find projection of cell vector 1 on to mid point - k vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
!
!  Subtract cell vectors from i-k and j-k vectors 
!
  xik = xik0 - imid*r1x - jmid*r2x - kmid*r3x
  yik = yik0 - imid*r1y - jmid*r2y - kmid*r3y
  zik = zik0 - imid*r1z - jmid*r2z - kmid*r3z
  xjk = xjk0 - imid*r1x - jmid*r2x - kmid*r3x
  yjk = yjk0 - imid*r1y - jmid*r2y - kmid*r3y
  zjk = zjk0 - imid*r1z - jmid*r2z - kmid*r3z
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!
!  Reinitialise distances squared
!     
    r2iki = 10000.0_dp*rcut2i
    r2jki = 10000.0_dp*rcut2j
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    nallfound1 = 0
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vectors
!
    rxiki = xik + dble(ii)*r1x 
    ryiki = yik + dble(ii)*r1y
    rziki = zik + dble(ii)*r1z
    rxjki = xjk + dble(ii)*r1x
    ryjki = yjk + dble(ii)*r1y
    rzjki = zjk + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Save number of vectors before search over second direction
!
      nvecj0 = nvec
!
!  Outer loop over second cell vector direction 
!
      do jdir = 1,-1,-2
!
!  Reinitialise distances squared
!
        r2ikj = 10000.0_dp*rcut2i
        r2jkj = 10000.0_dp*rcut2j
!
!  Loop over second cell vector
!
        lallfound2 = .false.
        nallfound2 = 0
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vectors
!
        rxikj = rxiki + dble(jj)*r2x
        ryikj = ryiki + dble(jj)*r2y
        rzikj = rziki + dble(jj)*r2z
        rxjkj = rxjki + dble(jj)*r2x
        ryjkj = ryjki + dble(jj)*r2y
        rzjkj = rzjki + dble(jj)*r2z
!
!  Set increment vector
!
        rcx2 = dble(jdir)*r2x
        rcy2 = dble(jdir)*r2y
        rcz2 = dble(jdir)*r2z
!
        do while (.not.lallfound2)
!
!  Save number of vectors before search over third direction
!
          nveck0 = nvec
!
!  Outer loop over third cell vector direction 
!
          do kdir = 1,-1,-2
!
!  Reinitialise distances squared
!
            r2ikk = 10000.0_dp*rcut2i
            r2jkk = 10000.0_dp*rcut2j
!
!  Loop over third cell vector
!
            lallfound3 = .false.
            nallfound3 = 0
            if (kdir.eq.1) then
              kk = 0
            else
              kk = - 1
            endif
!
!  Set initial coordinate vectors
!
            rxik = rxikj + dble(kk)*r3x
            ryik = ryikj + dble(kk)*r3y
            rzik = rzikj + dble(kk)*r3z
            rxjk = rxjkj + dble(kk)*r3x
            ryjk = ryjkj + dble(kk)*r3y
            rzjk = rzjkj + dble(kk)*r3z
!
!  Set increment vector
!
            rcx3 = dble(kdir)*r3x
            rcy3 = dble(kdir)*r3y
            rcz3 = dble(kdir)*r3z
!
            do while (.not.lallfound3)
!
!  Calculate squares of distances
!
              r2ik = rxik*rxik + ryik*ryik + rzik*rzik
              r2jk = rxjk*rxjk + ryjk*ryjk + rzjk*rzjk
!
!  Check distances squared against cutoffs squared
!
              if (r2ik.le.rcut2i.or.r2jk.le.rcut2j) then
                if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
                  nvec = nvec + 1
                  if (nvec0+nvec.gt.maxdis) then
                    maxdis = nvec0 + nvec + 50
                    call changemaxdis
                  endif
                  dist(nvec0 + nvec) = r2ik
                  xtmp(nvec0 + nvec) = rxik
                  ytmp(nvec0 + nvec) = ryik
                  ztmp(nvec0 + nvec) = rzik
                  dist2(nvec0 + nvec) = r2jk
                  xtmp2(nvec0 + nvec) = rxjk
                  ytmp2(nvec0 + nvec) = ryjk
                  ztmp2(nvec0 + nvec) = rzjk
                endif
              endif
!
!  Increment by third vector
!
              kk = kk + kdir
              rxik = rxik + rcx3
              ryik = ryik + rcy3
              rzik = rzik + rcz3
              rxjk = rxjk + rcx3
              ryjk = ryjk + rcy3
              rzjk = rzjk + rcz3
!                     
!  Check to see if this direction is complete
!                       
              if ((r2ik.gt.r2ikk.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jkk.and.r2jk.gt.rcut2j)) nallfound3 = nallfound3 + 1
              lallfound3 = (nallfound3.eq.nsafety)
              r2ikk = r2ik
              r2jkk = r2jk
            enddo
          enddo
!
!  Increment by second vector
!
          jj = jj + jdir
          rxikj = rxikj + rcx2
          ryikj = ryikj + rcy2
          rzikj = rzikj + rcz2
          rxjkj = rxjkj + rcx2
          ryjkj = ryjkj + rcy2
          rzjkj = rzjkj + rcz2
!
!  Check to see if this direction is complete
!
          if ((r2ik.gt.r2ikj.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jkj.and.r2jk.gt.rcut2j).and.nvec.eq.nveck0) &
            nallfound2 = nallfound2 + 1
          lallfound2 = (nallfound2.eq.nsafety)
          r2ikj = r2ik
          r2jkj = r2jk
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxiki = rxiki + rcx1
      ryiki = ryiki + rcy1
      rziki = rziki + rcz1
      rxjki = rxjki + rcx1
      ryjki = ryjki + rcy1
      rzjki = rzjki + rcz1
!
!  Check to see if this direction is complete
!
      if ((r2ik.gt.r2iki.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jki.and.r2jk.gt.rcut2j).and.nvec.eq.nvecj0) nallfound1 = nallfound1 + 1
      lallfound1 = (nallfound1.eq.nsafety)
      r2iki = r2ik
      r2jki = r2jk
    enddo
  enddo
!
  return
  end
!*********************************************************
!  Rfindeither33D : Get any image common to three atoms  *
!*********************************************************
  subroutine rfindeither33D(xil0,yil0,zil0,xjl0,yjl0,zjl0,xkl0,ykl0,zkl0,rcut2i,rcut2j,rcut2k,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom l that are 
!  within a cutoff of either atom i or atom j or atom k.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xil0         = x-coordinate of i-l vector for central unit cell
!  yil0         = y-coordinate of i-l vector for central unit cell
!  zil0         = z-coordinate of i-l vector for central unit cell
!  xjl0         = x-coordinate of j-l vector for central unit cell
!  yjl0         = y-coordinate of j-l vector for central unit cell
!  zjl0         = z-coordinate of j-l vector for central unit cell
!  xkl0         = x-coordinate of k-l vector for central unit cell
!  ykl0         = y-coordinate of k-l vector for central unit cell
!  zkl0         = z-coordinate of k-l vector for central unit cell
!  rcut2i       = cut-off distance for i to l squared
!  rcut2j       = cut-off distance for j to l squared
!  rcut2k       = cut-off distance for k to l squared
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of l that are within cutoff 
!                 of i or j or k
!  dist         = distance between atoms i and l for each vector
!  dist2        = distance between atoms j and l for each vector
!  dist3        = distance between atoms k and l for each vector
!  xtmp         = x component of valid i-l vector
!  ytmp         = y component of valid i-l vector
!  ztmp         = z component of valid i-l vector
!  xtmp2        = x component of valid j-l vector
!  ytmp2        = y component of valid j-l vector
!  ztmp2        = z component of valid j-l vector
!  xtmp3        = x component of valid j-l vector
!  ytmp3        = y component of valid j-l vector
!  ztmp3        = z component of valid j-l vector
!
!   3/03 Created
!  11/04 Calculation of cell norms eliminated
!  12/07 Unused variables removed
!   2/09 Argument removed from changemaxdis call
!  10/11 Safety margin added for angles that deviate from right angle
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
  use current
  use general
  use numbers,     only : third
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: rcut2k
  real(dp),    intent(in)    :: xil0
  real(dp),    intent(in)    :: yil0
  real(dp),    intent(in)    :: zil0
  real(dp),    intent(in)    :: xjl0
  real(dp),    intent(in)    :: yjl0
  real(dp),    intent(in)    :: zjl0
  real(dp),    intent(in)    :: xkl0
  real(dp),    intent(in)    :: ykl0
  real(dp),    intent(in)    :: zkl0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: kdir
  integer(i4)                :: kk
  integer(i4)                :: kmid
  integer(i4)                :: nallfound1
  integer(i4)                :: nallfound2
  integer(i4)                :: nallfound3
  integer(i4)                :: nsafety
  integer(i4)                :: nveck0
  logical                    :: aOK
  logical                    :: bOK
  logical                    :: cOK
  logical                    :: lallfound1
  logical                    :: lallfound2
  logical                    :: lallfound3
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: proj3
  real(dp)                   :: r2il
  real(dp)                   :: r2jl
  real(dp)                   :: r2kl
  real(dp)                   :: r2ili
  real(dp)                   :: r2jli
  real(dp)                   :: r2kli
  real(dp)                   :: r2ilj
  real(dp)                   :: r2jlj
  real(dp)                   :: r2klj
  real(dp)                   :: r2ilk
  real(dp)                   :: r2jlk
  real(dp)                   :: r2klk
  real(dp)                   :: rcx1
  real(dp)                   :: rcx2
  real(dp)                   :: rcx3
  real(dp)                   :: rcy1
  real(dp)                   :: rcy2
  real(dp)                   :: rcy3
  real(dp)                   :: rcz1
  real(dp)                   :: rcz2
  real(dp)                   :: rcz3
  real(dp)                   :: rnorm
  real(dp)                   :: rxil
  real(dp)                   :: ryil
  real(dp)                   :: rzil
  real(dp)                   :: rxjl
  real(dp)                   :: ryjl
  real(dp)                   :: rzjl
  real(dp)                   :: rxkl
  real(dp)                   :: rykl
  real(dp)                   :: rzkl
  real(dp)                   :: rxili
  real(dp)                   :: ryili
  real(dp)                   :: rzili
  real(dp)                   :: rxilj
  real(dp)                   :: ryilj
  real(dp)                   :: rzilj
  real(dp)                   :: rxjli
  real(dp)                   :: ryjli
  real(dp)                   :: rzjli
  real(dp)                   :: rxjlj
  real(dp)                   :: ryjlj
  real(dp)                   :: rzjlj
  real(dp)                   :: rxkli
  real(dp)                   :: rykli
  real(dp)                   :: rzkli
  real(dp)                   :: rxklj
  real(dp)                   :: ryklj
  real(dp)                   :: rzklj
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xil
  real(dp)                   :: yil
  real(dp)                   :: zil
  real(dp)                   :: xjl
  real(dp)                   :: yjl
  real(dp)                   :: zjl
  real(dp)                   :: xkl
  real(dp)                   :: ykl
  real(dp)                   :: zkl
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  nsafety is the number of search loops allowed without finding a new distance
!
  aOK = (abs(alpha-90.0_dp).lt.5.0_dp)
  bOK = (abs(beta-90.0_dp) .lt.5.0_dp)
  cOK = (abs(gamma-90.0_dp).lt.5.0_dp)
  if (aOK.and.bOK.and.cOK) then
    nsafety = 1
  elseif (aOK.and.bOK.or.aOK.and.cOK.or.bOK.and.cOK) then
    nsafety = 2
  elseif (aOK.or.bOK.or.cOK) then
    nsafety = 3
  else
    nsafety = 4
  endif
!
!  Copy input vectors
!
  xil = xil0
  yil = yil0
  zil = zil0
  xjl = xjl0
  yjl = yjl0
  zjl = zjl0
  xkl = xjl0
  ykl = yjl0
  zkl = zjl0
!
!  Find mid point of i - j - k vector to l
!
  xij = third*(xjl + xil + xkl)
  yij = third*(yjl + yil + ykl)
  zij = third*(zjl + zil + zkl)
!
!  Find projection of cell vector 3 on to mid point - l vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj3 = rnorm*recipc*(xij*r3x + yij*r3y + zij*r3z)
  kmid = nint(proj3)
  xij = xij - kmid*r3x
  yij = yij - kmid*r3y
  zij = zij - kmid*r3z
!
!  Find projection of cell vector 2 on to mid point - l vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj2 = rnorm*recipb*(xij*r2x + yij*r2y + zij*r2z)
  jmid = nint(proj2)
  xij = xij - jmid*r2x
  yij = yij - jmid*r2y
  zij = zij - jmid*r2z
!
!  Find projection of cell vector 1 on to mid point - l vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
!
!  Subtract cell vectors from i-k and j-k vectors 
!
  xil = xil0 - imid*r1x - jmid*r2x - kmid*r3x
  yil = yil0 - imid*r1y - jmid*r2y - kmid*r3y
  zil = zil0 - imid*r1z - jmid*r2z - kmid*r3z
  xjl = xjl0 - imid*r1x - jmid*r2x - kmid*r3x
  yjl = yjl0 - imid*r1y - jmid*r2y - kmid*r3y
  zjl = zjl0 - imid*r1z - jmid*r2z - kmid*r3z
  xkl = xkl0 - imid*r1x - jmid*r2x - kmid*r3x
  ykl = ykl0 - imid*r1y - jmid*r2y - kmid*r3y
  zkl = zkl0 - imid*r1z - jmid*r2z - kmid*r3z
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!     
!  Reinitialise distances squared 
!     
    r2ili = 10000.0_dp*rcut2i 
    r2jli = 10000.0_dp*rcut2j
    r2kli = 10000.0_dp*rcut2k
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    nallfound1 = 0
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vectors
!
    rxili = xil + dble(ii)*r1x 
    ryili = yil + dble(ii)*r1y
    rzili = zil + dble(ii)*r1z
    rxjli = xjl + dble(ii)*r1x
    ryjli = yjl + dble(ii)*r1y
    rzjli = zjl + dble(ii)*r1z
    rxkli = xkl + dble(ii)*r1x
    rykli = ykl + dble(ii)*r1y
    rzkli = zkl + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Outer loop over second cell vector direction 
!
      do jdir = 1,-1,-2
!     
!  Reinitialise distances squared 
!     
        r2ilj = 10000.0_dp*rcut2i 
        r2jlj = 10000.0_dp*rcut2j
        r2klj = 10000.0_dp*rcut2k
!
!  Loop over second cell vector
!
        lallfound2 = .false.
        nallfound2 = 0
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vectors
!
        rxilj = rxili + dble(jj)*r2x
        ryilj = ryili + dble(jj)*r2y
        rzilj = rzili + dble(jj)*r2z
        rxjlj = rxjli + dble(jj)*r2x
        ryjlj = ryjli + dble(jj)*r2y
        rzjlj = rzjli + dble(jj)*r2z
        rxklj = rxkli + dble(jj)*r2x
        ryklj = rykli + dble(jj)*r2y
        rzklj = rzkli + dble(jj)*r2z
!
!  Set increment vector
!
        rcx2 = dble(jdir)*r2x
        rcy2 = dble(jdir)*r2y
        rcz2 = dble(jdir)*r2z
!
        do while (.not.lallfound2)
!
!  Save number of vectors before search over third direction
!
          nveck0 = nvec
!
!  Outer loop over third cell vector direction 
!
          do kdir = 1,-1,-2
!     
!  Reinitialise distances squared 
!     
            r2ilk = 10000.0_dp*rcut2i 
            r2jlk = 10000.0_dp*rcut2j
            r2klk = 10000.0_dp*rcut2k
!
!  Loop over third cell vector
!
            lallfound3 = .false.
            nallfound3 = 0
            if (kdir.eq.1) then
              kk = 0
            else
              kk = - 1
            endif
!
!  Set initial coordinate vectors
!
            rxil = rxilj + dble(kk)*r3x
            ryil = ryilj + dble(kk)*r3y
            rzil = rzilj + dble(kk)*r3z
            rxjl = rxjlj + dble(kk)*r3x
            ryjl = ryjlj + dble(kk)*r3y
            rzjl = rzjlj + dble(kk)*r3z
            rxkl = rxklj + dble(kk)*r3x
            rykl = ryklj + dble(kk)*r3y
            rzkl = rzklj + dble(kk)*r3z
!
!  Set increment vector
!
            rcx3 = dble(kdir)*r3x
            rcy3 = dble(kdir)*r3y
            rcz3 = dble(kdir)*r3z
!
            do while (.not.lallfound3)
!
!  Calculate squares of distances
!
              r2il = rxil*rxil + ryil*ryil + rzil*rzil
              r2jl = rxjl*rxjl + ryjl*ryjl + rzjl*rzjl
              r2kl = rxkl*rxkl + rykl*rykl + rzkl*rzkl
!
!  Check distances squared against cutoffs squared
!
              if (r2il.le.rcut2i.or.r2jl.le.rcut2j.or.r2kl.le.rcut2k) then
                if ((r2il.gt.1.0d-12.and.r2jl.gt.1.0d-12.and.r2kl.gt.1.0d-12).or.lincludeself) then
                  nvec = nvec + 1
                  if (nvec0+nvec.gt.maxdis) then
                    maxdis = nvec0 + nvec + 50
                    call changemaxdis
                  endif
                  dist(nvec0 + nvec) = r2il
                  xtmp(nvec0 + nvec) = rxil
                  ytmp(nvec0 + nvec) = ryil
                  ztmp(nvec0 + nvec) = rzil
                  dist2(nvec0 + nvec) = r2jl
                  xtmp2(nvec0 + nvec) = rxjl
                  ytmp2(nvec0 + nvec) = ryjl
                  ztmp2(nvec0 + nvec) = rzjl
                  dist3(nvec0 + nvec) = r2kl
                  xtmp3(nvec0 + nvec) = rxkl
                  ytmp3(nvec0 + nvec) = rykl
                endif
              endif
!
!  Increment by third vector
!
              kk = kk + kdir
              rxil = rxil + rcx3
              ryil = ryil + rcy3
              rzil = rzil + rcz3
              rxjl = rxjl + rcx3
              ryjl = ryjl + rcy3
              rzjl = rzjl + rcz3
              rxkl = rxkl + rcx3
              rykl = rykl + rcy3
              rzkl = rzkl + rcz3
!
!  Check to see if this direction is complete
!
              if ((r2il.gt.r2ilk.and.r2il.gt.rcut2i).and.(r2jl.gt.r2jlk.and.r2jl.gt.rcut2j) &
                .and.(r2kl.gt.r2klk.and.r2kl.gt.rcut2k)) nallfound3 = nallfound3 + 1
              lallfound3 = (nallfound3.eq.nsafety)
              r2ilk = r2il
              r2jlk = r2jl
              r2klk = r2kl
            enddo
          enddo
!
!  Increment by second vector
!
          jj = jj + jdir
          rxilj = rxilj + rcx2
          ryilj = ryilj + rcy2
          rzilj = rzilj + rcz2
          rxjlj = rxjlj + rcx2
          ryjlj = ryjlj + rcy2
          rzjlj = rzjlj + rcz2
          rxklj = rxklj + rcx2
          ryklj = ryklj + rcy2
          rzklj = rzklj + rcz2
!
!  Check to see if this direction is complete
!
          if ((r2il.gt.r2ilj.and.r2il.gt.rcut2i).and.(r2jl.gt.r2jlj.and.r2jl.gt.rcut2j) &
            .and.(r2kl.gt.r2klj.and.r2kl.gt.rcut2k).and.nvec.eq.nveck0) nallfound2 = nallfound2 + 1
          lallfound2 = (nallfound2.eq.nsafety)
          r2ilj = r2il
          r2jlj = r2jl
          r2klj = r2kl
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxili = rxili + rcx1
      ryili = ryili + rcy1
      rzili = rzili + rcz1
      rxjli = rxjli + rcx1
      ryjli = ryjli + rcy1
      rzjli = rzjli + rcz1
      rxkli = rxkli + rcx1
      rykli = rykli + rcy1
      rzkli = rzkli + rcz1
!
!  Check to see if this direction is complete
!
      if ((r2il.gt.r2ili.and.r2il.gt.rcut2i).and.(r2jl.gt.r2jli.and.r2jl.gt.rcut2j) &
        .and.(r2kl.gt.r2kli.and.r2kl.gt.rcut2k).and.nvec.eq.nveck0) nallfound1 = nallfound1 + 1
      lallfound1 = (nallfound1.eq.nsafety)
      r2ili = r2il
      r2jli = r2jl
      r2kli = r2kl
    enddo
  enddo
!
  return
  end
!********************************************************
!  Rfind2D : Get any image relative to a single atom i  *
!********************************************************
  subroutine rfind2D(xij0,yij0,zij0,rcut2,rcut2e,rcut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid distances for a general 2D cell.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xij0         = x-coordinate of i-j vector for central unit cell
!  yij0         = y-coordinate of i-j vector for central unit cell
!  zij0         = z-coordinate of i-j vector for central unit cell
!  rcut2        = cut-off distance for this pair of atoms squared
!  rcut2e       = electostatic cut-off distance for this pair of atoms squared
!  rcut2s       = core-shell cut-off distance for this pair of atoms squared
!  lcspair      = is this is a potential core-shell pair? 
!  lmolok       = do we need to do molecule checks?
!  i            = atom number for i
!  j            = atom number for j
!  nmi          = molecule number for i/j
!  ixj          = difference in first cell index between i and j for molecule
!  iyj          = difference in second cell index between i and j for molecule
!  izj          = difference in third cell index between i and j for molecule
!  nvec0        = initial location for vectors - 1
!
!  On exit :
!
!  lincludeself = if .true. then include self-term, if found
!  nvec         = number of valid vectors between atoms
!  dist         = distance between atoms i and j for each vector
!  xtmp         = x component of valid i-j vector
!  ytmp         = y component of valid i-j vector
!  ztmp         = z component of valid i-j vector
!
!   3/03 Created
!   4/04 Bonding setup added through call to getbonds
!  11/04 Calculation of cell norms eliminated
!   1/05 Storage of valid cell indices added
!   2/07 Bonding types added
!   1/08 Calls to bonded3c modified
!   2/09 Argument removed from changemaxdis call
!  10/11 Safety margin added for angles that deviate from right angle
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
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: i
  integer(i4), intent(in)    :: ixj
  integer(i4), intent(in)    :: iyj
  integer(i4), intent(in)    :: izj
  integer(i4), intent(in)    :: j
  integer(i4), intent(in)    :: nmi
  integer(i4), intent(out)   :: nmolonly
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lcspair
  logical,     intent(out)   :: lincludeself
  logical,     intent(in)    :: lmolok
  real(dp),    intent(in)    :: rcut2
  real(dp),    intent(in)    :: rcut2e
  real(dp),    intent(in)    :: rcut2s
  real(dp),    intent(in)    :: xij0
  real(dp),    intent(in)    :: yij0
  real(dp),    intent(in)    :: zij0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: ijx
  integer(i4)                :: ijy
  integer(i4)                :: ijz
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: nallfound1
  integer(i4)                :: nallfound2
  integer(i4)                :: nsafety
  integer(i4)                :: nvecj0
  logical                    :: aOK
  logical                    :: lallfound1
  logical                    :: lallfound2
  logical                    :: lsamemol
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: r2
  real(dp)                   :: r2i
  real(dp)                   :: r2j
  real(dp)                   :: rcx1
  real(dp)                   :: rcx2
  real(dp)                   :: rcy1
  real(dp)                   :: rcy2
  real(dp)                   :: rcz1
  real(dp)                   :: rcz2
  real(dp)                   :: rnorm
  real(dp)                   :: rx
  real(dp)                   :: ry
  real(dp)                   :: rz
  real(dp)                   :: rxi
  real(dp)                   :: ryi
  real(dp)                   :: rzi
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
!
!  Initialise variables
!
  lincludeself = .false.
!
!  nsafety is the number of search loops allowed without finding a new distance
!
  aOK = (abs(alpha-90.0_dp).lt.5.0_dp)
  if (aOK) then
    nsafety = 1
  else
    nsafety = 2
  endif
!
!  Find image of j nearest to i 
!
  xij = xij0
  yij = yij0
  zij = zij0
!     
!  Find projection of cell vector 2 on to i - j vector
!     
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj2 = rnorm*recipb*(xij*r2x + yij*r2y + zij*r2z)
  jmid = nint(proj2)
  xij = xij - jmid*r2x
  yij = yij - jmid*r2y 
  zij = zij - jmid*r2z
!     
!  Find projection of cell vector 1 on to i - j vector
!     
  rnorm = xij*xij + yij*yij + zij*zij 
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
  xij = xij - imid*r1x
  yij = yij - imid*r1y
  zij = zij - imid*r1z
!
!  Adjust indices for molecule
!
  ijx = ixj + imid
  ijy = iyj + jmid
  ijz = izj
!
!  Set up bonding
!
  if (lmolok) call getbonds(i,j)
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!
!  Reinitialise distance squared
!
    r2i = 10000.0_dp*rcut2
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    nallfound1 = 0
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vector
!
    rxi = xij + dble(ii)*r1x
    ryi = yij + dble(ii)*r1y
    rzi = zij + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Save number of vectors before search over second direction
!
      nvecj0 = nvec
!
!  Outer loop over second cell vector direction 
!
      do jdir = 1,-1,-2
!     
!  Reinitialise saved distance squared
!       
        r2j = 10000.0_dp*rcut2
!
!  Loop over second cell vector
!
        lallfound2 = .false.
        nallfound2 = 0
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vector
!
        rx = rxi + dble(jj)*r2x
        ry = ryi + dble(jj)*r2y
        rz = rzi + dble(jj)*r2z
!
!  Set increment vector
!
        rcx2 = dble(jdir)*r2x
        rcy2 = dble(jdir)*r2y
        rcz2 = dble(jdir)*r2z
!
        do while (.not.lallfound2)
!
!  Calculate square of distance
!
          r2 = rx*rx + ry*ry + rz*rz
!                 
!  Molecule - check index
!  
          if (lmolok.and.(r2.gt.rcut2s.or..not.lcspair)) then
            call bonded3c(lbonded(nvec0+nvec+1),l2bonds(nvec0+nvec+1),l3bonds(nvec0+nvec+1), &
                          nbotype(nvec0+nvec+1),nbotype2(nvec0+nvec+1),ii-imid,jj-jmid,0_i4)
            lsamemol = (lbonded(nvec0+nvec+1).or.l2bonds(nvec0+nvec+1).or.l3bonds(nvec0+nvec+1))
            if (.not.lsamemol) then
              call samemol(lsamemol,nmi,ii,jj,0_i4,ijx,ijy,ijz)
            endif
            lptrmol(nvec0+nvec+1) = lsamemol
            if (lsamemol) then
              if (r2.gt.rcut2e) nmolonly = nvec0 + nvec + 1
            else
              lbonded(nvec0+nvec+1) = .false.
              l2bonds(nvec0+nvec+1) = .false.
              l3bonds(nvec0+nvec+1) = .false.
            endif
          else
            lptrmol(nvec0+nvec+1)  = .false.
            lbonded(nvec0+nvec+1)  = .false.
            l2bonds(nvec0+nvec+1)  = .false.
            l3bonds(nvec0+nvec+1)  = .false.
            nbotype(nvec0+nvec+1)  = 0
            nbotype2(nvec0+nvec+1) = 0
          endif
!
!  Check distance squared against cutoff squared
!
          if (r2.le.rcut2) then
            if (r2.gt.1.0d-12) then
              nvec = nvec + 1
              if (nvec0+nvec.ge.maxdis) then
                maxdis = nvec0 + nvec + 50
                call changemaxdis
              endif
              dist(nvec0+nvec) = r2
              xtmp(nvec0+nvec) = rx
              ytmp(nvec0+nvec) = ry
              ztmp(nvec0+nvec) = rz
              cellindex(1,nvec0+nvec) = ii - imid
              cellindex(2,nvec0+nvec) = jj - jmid
              cellindex(3,nvec0+nvec) = 0
            else
              lincludeself = .true.
            endif
          endif
!
!  Increment by second vector
!
          jj = jj + jdir
          rx = rx + rcx2
          ry = ry + rcy2
          rz = rz + rcz2
!  
!  Check to see if this direction is complete
!                 
          if (r2.gt.r2j.and.r2.gt.rcut2) nallfound2 = nallfound2 + 1
          lallfound2 = (nallfound2.eq.nsafety)
          r2j = r2
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxi = rxi + rcx1
      ryi = ryi + rcy1
      rzi = rzi + rcz1
!
!  Check to see if this direction is complete
!
      if (r2.gt.r2i.and.r2.gt.rcut2.and.nvec.eq.nvecj0) nallfound1 = nallfound1 + 1
      lallfound1 = (nallfound1.eq.nsafety)
      r2i = r2
    enddo
  enddo
!
  return
  end
!******************************************************
!  Rfindeither2D : Get any image common to two atoms  *
!******************************************************
  subroutine rfindeither2D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of either atom i or atom j.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xik0         = x-coordinate of i-k vector for central unit cell
!  yik0         = y-coordinate of i-k vector for central unit cell
!  zik0         = z-coordinate of i-k vector for central unit cell
!  xjk0         = x-coordinate of j-k vector for central unit cell
!  yjk0         = y-coordinate of j-k vector for central unit cell
!  zjk0         = z-coordinate of j-k vector for central unit cell
!  rcut2i       = cut-off distance for i to k squared
!  rcut2j       = cut-off distance for j to k squared
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff 
!                 of i or j
!  dist         = distance between atoms i and k for each vector
!  dist2        = distance between atoms j and k for each vector
!  xtmp         = x component of valid i-k vector
!  ytmp         = y component of valid i-k vector
!  ztmp         = z component of valid i-k vector
!  xtmp2        = x component of valid j-k vector
!  ytmp2        = y component of valid j-k vector
!  ztmp2        = z component of valid j-k vector
!
!   3/03 Created
!  11/04 Calculation of cell norms eliminated
!   2/09 Argument removed from changemaxdis call
!  10/11 Safety margin added for angles that deviate from right angle
!  11/11 Error in setting of lallfound2 fixed
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
!  Julian Gale, NRI, Curtin University, November 2011
!
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: xik0
  real(dp),    intent(in)    :: yik0
  real(dp),    intent(in)    :: zik0
  real(dp),    intent(in)    :: xjk0
  real(dp),    intent(in)    :: yjk0
  real(dp),    intent(in)    :: zjk0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: nallfound1
  integer(i4)                :: nallfound2
  integer(i4)                :: nsafety
  integer(i4)                :: nvecj0
  logical                    :: aOK
  logical                    :: lallfound1
  logical                    :: lallfound2
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: r2ik
  real(dp)                   :: r2jk
  real(dp)                   :: r2iki
  real(dp)                   :: r2jki
  real(dp)                   :: r2ikj
  real(dp)                   :: r2jkj
  real(dp)                   :: rcx1
  real(dp)                   :: rcx2
  real(dp)                   :: rcy1
  real(dp)                   :: rcy2
  real(dp)                   :: rcz1
  real(dp)                   :: rcz2
  real(dp)                   :: rnorm
  real(dp)                   :: rxik
  real(dp)                   :: ryik
  real(dp)                   :: rzik
  real(dp)                   :: rxjk
  real(dp)                   :: ryjk
  real(dp)                   :: rzjk
  real(dp)                   :: rxiki
  real(dp)                   :: ryiki
  real(dp)                   :: rziki
  real(dp)                   :: rxjki
  real(dp)                   :: ryjki
  real(dp)                   :: rzjki
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xik
  real(dp)                   :: yik
  real(dp)                   :: zik
  real(dp)                   :: xjk
  real(dp)                   :: yjk
  real(dp)                   :: zjk
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  nsafety is the number of search loops allowed without finding a new distance
!
  aOK = (abs(alpha-90.0_dp).lt.5.0_dp)
  if (aOK) then
    nsafety = 1
  else
    nsafety = 2
  endif
!
!  Copy input vectors
!
  xik = xik0
  yik = yik0
  zik = zik0
  xjk = xjk0
  yjk = yjk0
  zjk = zjk0
!
!  Find mid point of i - j vector to k
!
  xij = 0.5_dp*(xjk + xik)
  yij = 0.5_dp*(yjk + yik)
  zij = 0.5_dp*(zjk + zik)
!
!  Find projection of cell vector 2 on to mid point - k vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj2 = rnorm*recipb*(xij*r2x + yij*r2y + zij*r2z)
  jmid = nint(proj2)
  xij = xij - jmid*r2x
  yij = yij - jmid*r2y
  zij = zij - jmid*r2z
!
!  Find projection of cell vector 1 on to mid point - k vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
!
!  Subtract cell vectors from i-k and j-k vectors 
!
  xik = xik0 - imid*r1x - jmid*r2x
  yik = yik0 - imid*r1y - jmid*r2y
  zik = zik0 - imid*r1z - jmid*r2z
  xjk = xjk0 - imid*r1x - jmid*r2x
  yjk = yjk0 - imid*r1y - jmid*r2y
  zjk = zjk0 - imid*r1z - jmid*r2z
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!       
!  Reinitialise distances squared 
!       
    r2iki = 10000.0_dp*rcut2i
    r2jki = 10000.0_dp*rcut2j
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    nallfound1 = 0
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vectors
!
    rxiki = xik + dble(ii)*r1x 
    ryiki = yik + dble(ii)*r1y
    rziki = zik + dble(ii)*r1z
    rxjki = xjk + dble(ii)*r1x
    ryjki = yjk + dble(ii)*r1y
    rzjki = zjk + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Save number of vectors before search over second direction
!
      nvecj0 = nvec
!
!  Outer loop over second cell vector direction 
!
      do jdir = 1,-1,-2
!       
!  Reinitialise distances squared 
!       
        r2ikj = 10000.0_dp*rcut2i
        r2jkj = 10000.0_dp*rcut2j
!
!  Loop over third cell vector
!
        lallfound2 = .false.
        nallfound2 = 0
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vectors
!
        rxik = rxiki + dble(jj)*r2x
        ryik = ryiki + dble(jj)*r2y
        rzik = rziki + dble(jj)*r2z
        rxjk = rxjki + dble(jj)*r2x
        ryjk = ryjki + dble(jj)*r2y
        rzjk = rzjki + dble(jj)*r2z
!
!  Set increment vector
!
        rcx2 = dble(jdir)*r2x
        rcy2 = dble(jdir)*r2y
        rcz2 = dble(jdir)*r2z
!
        do while (.not.lallfound2)
!
!  Calculate squares of distances
!
          r2ik = rxik*rxik + ryik*ryik + rzik*rzik
          r2jk = rxjk*rxjk + ryjk*ryjk + rzjk*rzjk
!
!  Check distances squared against cutoffs squared
!
          if (r2ik.le.rcut2i.or.r2jk.le.rcut2j) then
            if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
              nvec = nvec + 1
              if (nvec0+nvec.gt.maxdis) then
                maxdis = nvec0 + nvec + 50
                call changemaxdis
              endif
              dist(nvec0 + nvec) = r2ik
              xtmp(nvec0 + nvec) = rxik
              ytmp(nvec0 + nvec) = ryik
              ztmp(nvec0 + nvec) = rzik
              dist2(nvec0 + nvec) = r2jk
              xtmp2(nvec0 + nvec) = rxjk
              ytmp2(nvec0 + nvec) = ryjk
              ztmp2(nvec0 + nvec) = rzjk
            endif
          endif
!
!  Increment by second vector
!
          jj = jj + jdir
          rxik = rxik + rcx2
          ryik = ryik + rcy2
          rzik = rzik + rcz2
          rxjk = rxjk + rcx2
          ryjk = ryjk + rcy2
          rzjk = rzjk + rcz2
!                     
!  Check to see if this direction is complete  
!                     
          if ((r2ik.gt.r2ikj.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jkj.and.r2jk.gt.rcut2j)) nallfound2 = nallfound2 + 1
          lallfound2 = (nallfound2.eq.nsafety)
          r2ikj = r2ik
          r2jkj = r2jk
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxiki = rxiki + rcx1
      ryiki = ryiki + rcy1
      rziki = rziki + rcz1
      rxjki = rxjki + rcx1
      ryjki = ryjki + rcy1
      rzjki = rzjki + rcz1
!
!  Check to see if this direction is complete
!
      if ((r2ik.gt.r2iki.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jki.and.r2jk.gt.rcut2j).and.nvec.eq.nvecj0) nallfound1 = nallfound1 + 1
      lallfound1 = (nallfound1.eq.nsafety)
      r2iki = r2ik
      r2jki = r2jk
    enddo
  enddo
!
  return
  end
!*********************************************************
!  Rfindeither32D : Get any image common to three atoms  *
!*********************************************************
  subroutine rfindeither32D(xil0,yil0,zil0,xjl0,yjl0,zjl0,xkl0,ykl0,zkl0,rcut2i,rcut2j,rcut2k,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom l that are 
!  within a cutoff of either atom i or atom j or atom k.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xil0         = x-coordinate of i-l vector for central unit cell
!  yil0         = y-coordinate of i-l vector for central unit cell
!  zil0         = z-coordinate of i-l vector for central unit cell
!  xjl0         = x-coordinate of j-l vector for central unit cell
!  yjl0         = y-coordinate of j-l vector for central unit cell
!  zjl0         = z-coordinate of j-l vector for central unit cell
!  xkl0         = x-coordinate of k-l vector for central unit cell
!  ykl0         = y-coordinate of k-l vector for central unit cell
!  zkl0         = z-coordinate of k-l vector for central unit cell
!  rcut2i       = cut-off distance for i to l squared
!  rcut2j       = cut-off distance for j to l squared
!  rcut2k       = cut-off distance for k to l squared
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of l that are within cutoff 
!                 of i or j or k
!  dist         = distance between atoms i and l for each vector
!  dist2        = distance between atoms j and l for each vector
!  dist3        = distance between atoms k and l for each vector
!  xtmp         = x component of valid i-l vector
!  ytmp         = y component of valid i-l vector
!  ztmp         = z component of valid i-l vector
!  xtmp2        = x component of valid j-l vector
!  ytmp2        = y component of valid j-l vector
!  ztmp2        = z component of valid j-l vector
!  xtmp3        = x component of valid j-l vector
!  ytmp3        = y component of valid j-l vector
!  ztmp3        = z component of valid j-l vector
!
!   3/03 Created
!  11/04 Calculation of cell norms eliminated
!   2/09 Argument removed from changemaxdis call
!  10/11 Safety margin added for angles that deviate from right angle
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
  use current
  use general
  use numbers,     only : third
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: rcut2k
  real(dp),    intent(in)    :: xil0
  real(dp),    intent(in)    :: yil0
  real(dp),    intent(in)    :: zil0
  real(dp),    intent(in)    :: xjl0
  real(dp),    intent(in)    :: yjl0
  real(dp),    intent(in)    :: zjl0
  real(dp),    intent(in)    :: xkl0
  real(dp),    intent(in)    :: ykl0
  real(dp),    intent(in)    :: zkl0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: nallfound1
  integer(i4)                :: nallfound2
  integer(i4)                :: nsafety
  integer(i4)                :: nvecj0
  logical                    :: aOK
  logical                    :: lallfound1
  logical                    :: lallfound2
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: r2il
  real(dp)                   :: r2jl
  real(dp)                   :: r2kl
  real(dp)                   :: r2ili
  real(dp)                   :: r2jli
  real(dp)                   :: r2kli
  real(dp)                   :: r2ilj
  real(dp)                   :: r2jlj
  real(dp)                   :: r2klj
  real(dp)                   :: rcx1
  real(dp)                   :: rcx2
  real(dp)                   :: rcy1
  real(dp)                   :: rcy2
  real(dp)                   :: rcz1
  real(dp)                   :: rcz2
  real(dp)                   :: rnorm
  real(dp)                   :: rxil
  real(dp)                   :: ryil
  real(dp)                   :: rzil
  real(dp)                   :: rxjl
  real(dp)                   :: ryjl
  real(dp)                   :: rzjl
  real(dp)                   :: rxkl
  real(dp)                   :: rykl
  real(dp)                   :: rzkl
  real(dp)                   :: rxili
  real(dp)                   :: ryili
  real(dp)                   :: rzili
  real(dp)                   :: rxjli
  real(dp)                   :: ryjli
  real(dp)                   :: rzjli
  real(dp)                   :: rxkli
  real(dp)                   :: rykli
  real(dp)                   :: rzkli
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xil
  real(dp)                   :: yil
  real(dp)                   :: zil
  real(dp)                   :: xjl
  real(dp)                   :: yjl
  real(dp)                   :: zjl
  real(dp)                   :: xkl
  real(dp)                   :: ykl
  real(dp)                   :: zkl
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  nsafety is the number of search loops allowed without finding a new distance
!
  aOK = (abs(alpha-90.0_dp).lt.5.0_dp)
  if (aOK) then
    nsafety = 1
  else
    nsafety = 2
  endif
!
!  Copy input vectors
!
  xil = xil0
  yil = yil0
  zil = zil0
  xjl = xjl0
  yjl = yjl0
  zjl = zjl0
  xkl = xjl0
  ykl = yjl0
  zkl = zjl0
!
!  Find mid point of i - j - k vector to l
!
  xij = third*(xjl + xil + xkl)
  yij = third*(yjl + yil + ykl)
  zij = third*(zjl + zil + zkl)
!
!  Find projection of cell vector 2 on to mid point - l vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj2 = rnorm*recipb*(xij*r2x + yij*r2y + zij*r2z)
  jmid = nint(proj2)
  xij = xij - jmid*r2x
  yij = yij - jmid*r2y
  zij = zij - jmid*r2z
!
!  Find projection of cell vector 1 on to mid point - l vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
!
!  Subtract cell vectors from i-k and j-k vectors 
!
  xil = xil0 - imid*r1x - jmid*r2x 
  yil = yil0 - imid*r1y - jmid*r2y 
  zil = zil0 - imid*r1z - jmid*r2z 
  xjl = xjl0 - imid*r1x - jmid*r2x 
  yjl = yjl0 - imid*r1y - jmid*r2y 
  zjl = zjl0 - imid*r1z - jmid*r2z 
  xkl = xkl0 - imid*r1x - jmid*r2x 
  ykl = ykl0 - imid*r1y - jmid*r2y 
  zkl = zkl0 - imid*r1z - jmid*r2z 
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!       
!  Reinitialise distances squared 
!       
    r2ili = 10000.0_dp*rcut2i
    r2jli = 10000.0_dp*rcut2j
    r2kli = 10000.0_dp*rcut2k
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    nallfound1 = 0
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vectors
!
    rxili = xil + dble(ii)*r1x 
    ryili = yil + dble(ii)*r1y
    rzili = zil + dble(ii)*r1z
    rxjli = xjl + dble(ii)*r1x
    ryjli = yjl + dble(ii)*r1y
    rzjli = zjl + dble(ii)*r1z
    rxkli = xkl + dble(ii)*r1x
    rykli = ykl + dble(ii)*r1y
    rzkli = zkl + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Save number of vectors before search over second direction
!
      nvecj0 = nvec
!
!  Outer loop over second cell vector direction 
!
      do jdir = 1,-1,-2
!       
!  Reinitialise distances squared 
!       
        r2ilj = 10000.0_dp*rcut2i
        r2jlj = 10000.0_dp*rcut2j
        r2klj = 10000.0_dp*rcut2k
!
!  Loop over second cell vector
!
        lallfound2 = .false.
        nallfound2 = 0
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vectors
!
        rxil = rxili + dble(jj)*r2x
        ryil = ryili + dble(jj)*r2y
        rzil = rzili + dble(jj)*r2z
        rxjl = rxjli + dble(jj)*r2x
        ryjl = ryjli + dble(jj)*r2y
        rzjl = rzjli + dble(jj)*r2z
        rxkl = rxkli + dble(jj)*r2x
        rykl = rykli + dble(jj)*r2y
        rzkl = rzkli + dble(jj)*r2z
!
!  Set increment vector
!
        rcx2 = dble(jdir)*r2x
        rcy2 = dble(jdir)*r2y
        rcz2 = dble(jdir)*r2z
!
        do while (.not.lallfound2)
!
!  Calculate squares of distances
!
          r2il = rxil*rxil + ryil*ryil + rzil*rzil
          r2jl = rxjl*rxjl + ryjl*ryjl + rzjl*rzjl
          r2kl = rxkl*rxkl + rykl*rykl + rzkl*rzkl
!
!  Check distances squared against cutoffs squared
!
          if (r2il.le.rcut2i.or.r2jl.le.rcut2j.or.r2kl.le.rcut2k) then
            if ((r2il.gt.1.0d-12.and.r2jl.gt.1.0d-12.and.r2kl.gt.1.0d-12).or.lincludeself) then
              nvec = nvec + 1
              if (nvec0+nvec.gt.maxdis) then
                maxdis = nvec0 + nvec + 50
                call changemaxdis
              endif
              dist(nvec0 + nvec) = r2il
              xtmp(nvec0 + nvec) = rxil
              ytmp(nvec0 + nvec) = ryil
              ztmp(nvec0 + nvec) = rzil
              dist2(nvec0 + nvec) = r2jl
              xtmp2(nvec0 + nvec) = rxjl
              ytmp2(nvec0 + nvec) = ryjl
              ztmp2(nvec0 + nvec) = rzjl
              dist3(nvec0 + nvec) = r2kl
              xtmp3(nvec0 + nvec) = rxkl
              ytmp3(nvec0 + nvec) = rykl
            endif
          endif
!
!  Increment by second vector
!
          jj = jj + jdir
          rxil = rxil + rcx2
          ryil = ryil + rcy2
          rzil = rzil + rcz2
          rxjl = rxjl + rcx2
          ryjl = ryjl + rcy2
          rzjl = rzjl + rcz2
          rxkl = rxkl + rcx2
          rykl = rykl + rcy2
          rzkl = rzkl + rcz2
!                 
!  Check to see if this direction is complete
!                     
          if ((r2il.gt.r2ilj.and.r2il.gt.rcut2i).and.(r2jl.gt.r2jlj.and.r2jl.gt.rcut2j) &
            .and.(r2kl.gt.r2klj.and.r2kl.gt.rcut2k)) nallfound2 = nallfound2 + 1
          lallfound2 = (nallfound2.eq.nsafety)
          r2ilj = r2il
          r2jlj = r2jl
          r2klj = r2kl
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxili = rxili + rcx1
      ryili = ryili + rcy1
      rzili = rzili + rcz1
      rxjli = rxjli + rcx1
      ryjli = ryjli + rcy1
      rzjli = rzjli + rcz1
      rxkli = rxkli + rcx1
      rykli = rykli + rcy1
      rzkli = rzkli + rcz1
!
!  Check to see if this direction is complete
!
      if ((r2il.gt.r2ili.and.r2il.gt.rcut2i).and.(r2jl.gt.r2jli.and.r2jl.gt.rcut2j) &
        .and.(r2kl.gt.r2kli.and.r2kl.gt.rcut2k).and.nvec.eq.nvecj0) nallfound1 = nallfound1 + 1
      lallfound1 = (nallfound1.eq.nsafety)
      r2ili = r2il
      r2jli = r2jl
      r2kli = r2kl
    enddo
  enddo
!
  return
  end
!********************************************************
!  Rfind1D : Get any image relative to a single atom i  *
!********************************************************
  subroutine rfind1D(xij0,yij0,zij0,rcut2,rcut2e,rcut2s,lcspair,lmolok,i,j,nmi,ixj,iyj,izj,nmolonly,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid distances for a general 2D cell.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xij0         = x-coordinate of i-j vector for central unit cell
!  yij0         = y-coordinate of i-j vector for central unit cell
!  zij0         = z-coordinate of i-j vector for central unit cell
!  rcut2        = cut-off distance for this pair of atoms squared
!  rcut2e       = electrostatic cut-off distance for this pair of atoms squared
!  rcut2s       = core-shell cut-off distance for this pair of atoms squared
!  lcspair      = is this is a potential core-shell pair?
!  lmolok       = do we need to do molecule checks?
!  i            = atom number for i
!  j            = atom number for j
!  nmi          = molecule number for i/j
!  ixj          = difference in first cell index between i and j for molecule
!  iyj          = difference in second cell index between i and j for molecule
!  izj          = difference in third cell index between i and j for molecule
!  nvec0        = initial location for vectors - 1
!
!  On exit :
!
!  lincludeself = if .true. then include self-term, if found
!  nvec         = number of valid vectors between atoms
!  dist         = distance between atoms i and j for each vector
!  xtmp         = x component of valid i-j vector
!  ytmp         = y component of valid i-j vector
!  ztmp         = z component of valid i-j vector
!
!   3/03 Created
!   4/04 Bonding setup added through call to getbonds
!  11/04 Calculation of cell norms eliminated
!   1/05 Storage of valid cell indices added
!   2/07 Bonding types added
!   1/08 Calls to bonded3c modified
!   2/09 Argument removed from changemaxdis call
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
!  Julian Gale, NRI, Curtin University, February 2009
!
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: i
  integer(i4), intent(in)    :: ixj
  integer(i4), intent(in)    :: iyj
  integer(i4), intent(in)    :: izj
  integer(i4), intent(in)    :: j
  integer(i4), intent(in)    :: nmi
  integer(i4), intent(out)   :: nmolonly
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lcspair
  logical,     intent(out)   :: lincludeself
  logical,     intent(in)    :: lmolok
  real(dp),    intent(in)    :: rcut2
  real(dp),    intent(in)    :: rcut2e
  real(dp),    intent(in)    :: rcut2s
  real(dp),    intent(in)    :: xij0
  real(dp),    intent(in)    :: yij0
  real(dp),    intent(in)    :: zij0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: ijx
  integer(i4)                :: ijy
  integer(i4)                :: ijz
  integer(i4)                :: imid
  logical                    :: lallfound1
  logical                    :: lsamemol
  real(dp)                   :: proj1
  real(dp)                   :: r2
  real(dp)                   :: r2i
  real(dp)                   :: rcx1
  real(dp)                   :: rcy1
  real(dp)                   :: rcz1
  real(dp)                   :: rnorm
  real(dp)                   :: rx
  real(dp)                   :: ry
  real(dp)                   :: rz
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
!
!  Initialise lincludeself
!
  lincludeself = .false.
!
!  Find image of j nearest to i 
!
  xij = xij0
  yij = yij0
  zij = zij0
!     
!  Find projection of cell vector 1 on to i - j vector
!     
  rnorm = xij*xij + yij*yij + zij*zij 
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
  xij = xij - imid*r1x
  yij = yij - imid*r1y
  zij = zij - imid*r1z
!
!  Adjust indices for molecule
!
  ijx = ixj + imid
  ijy = iyj
  ijz = izj
!
!  Set up bonding
!
  if (lmolok) call getbonds(i,j)
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!     
!  Reinitialise distance squared
!     
    r2i = 10000.0_dp*rcut2
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vector
!
    rx = xij + dble(ii)*r1x
    ry = yij + dble(ii)*r1y
    rz = zij + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Calculate square of distance
!
      r2 = rx*rx + ry*ry + rz*rz
!
!  Molecule - check index
!
      if (lmolok.and.(r2.gt.rcut2s.or..not.lcspair)) then
        call bonded3c(lbonded(nvec0+nvec+1),l2bonds(nvec0+nvec+1),l3bonds(nvec0+nvec+1), &
                      nbotype(nvec0+nvec+1),nbotype2(nvec0+nvec+1),ii-imid,0_i4,0_i4)
        lsamemol = (lbonded(nvec0+nvec+1).or.l2bonds(nvec0+nvec+1).or.l3bonds(nvec0+nvec+1))
        if (.not.lsamemol) then
          call samemol(lsamemol,nmi,ii,0_i4,0_i4,ijx,ijy,ijz)
        endif
        lptrmol(nvec0+nvec+1) = lsamemol
        if (lsamemol) then
          if (r2.gt.rcut2e) nmolonly = nvec0 + nvec + 1
        else
          lbonded(nvec0+nvec+1) = .false.
          l2bonds(nvec0+nvec+1) = .false.
          l3bonds(nvec0+nvec+1) = .false.
        endif
      else
        lptrmol(nvec0+nvec+1)  = .false.
        lbonded(nvec0+nvec+1)  = .false.
        l2bonds(nvec0+nvec+1)  = .false.
        l3bonds(nvec0+nvec+1)  = .false.
        nbotype(nvec0+nvec+1)  = 0
        nbotype2(nvec0+nvec+1) = 0
      endif
!
!  Check distance squared against cutoff squared
!
      if (r2.le.rcut2) then
        if (r2.gt.1.0d-12) then
          nvec = nvec + 1
          if (nvec0+nvec.ge.maxdis) then
            maxdis = nvec0 + nvec + 50
            call changemaxdis
          endif
          dist(nvec0+nvec) = r2
          xtmp(nvec0+nvec) = rx
          ytmp(nvec0+nvec) = ry
          ztmp(nvec0+nvec) = rz
          cellindex(1,nvec0+nvec) = ii - imid
          cellindex(2,nvec0+nvec) = 0
          cellindex(3,nvec0+nvec) = 0
        else
          lincludeself = .true.
        endif
      endif
!
!  Increment by first vector
!
      ii = ii + idir
      rx = rx + rcx1
      ry = ry + rcy1
      rz = rz + rcz1
!
!  Check to see if this direction is complete
!
      lallfound1 = (r2.gt.r2i.and.r2.gt.rcut2)
      r2i = r2
    enddo
  enddo
!
  return
  end
!******************************************************
!  Rfindeither1D : Get any image common to two atoms  *
!******************************************************
  subroutine rfindeither1D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of either atom i or atom j.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xik0         = x-coordinate of i-k vector for central unit cell
!  yik0         = y-coordinate of i-k vector for central unit cell
!  zik0         = z-coordinate of i-k vector for central unit cell
!  xjk0         = x-coordinate of j-k vector for central unit cell
!  yjk0         = y-coordinate of j-k vector for central unit cell
!  zjk0         = z-coordinate of j-k vector for central unit cell
!  rcut2i       = cut-off distance for i to k squared
!  rcut2j       = cut-off distance for j to k squared
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff 
!                 of i or j
!  dist         = distance between atoms i and k for each vector
!  dist2        = distance between atoms j and k for each vector
!  xtmp         = x component of valid i-k vector
!  ytmp         = y component of valid i-k vector
!  ztmp         = z component of valid i-k vector
!  xtmp2        = x component of valid j-k vector
!  ytmp2        = y component of valid j-k vector
!  ztmp2        = z component of valid j-k vector
!
!   3/03 Created
!  11/04 Calculation of cell norms eliminated
!   2/09 Argument removed from changemaxdis call
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
!  Julian Gale, NRI, Curtin University, February 2009
!
  use current
  use general
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: xik0
  real(dp),    intent(in)    :: yik0
  real(dp),    intent(in)    :: zik0
  real(dp),    intent(in)    :: xjk0
  real(dp),    intent(in)    :: yjk0
  real(dp),    intent(in)    :: zjk0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  logical                    :: lallfound1
  real(dp)                   :: proj1
  real(dp)                   :: r2ik
  real(dp)                   :: r2jk
  real(dp)                   :: r2iki
  real(dp)                   :: r2jki
  real(dp)                   :: rcx1
  real(dp)                   :: rcy1
  real(dp)                   :: rcz1
  real(dp)                   :: rnorm
  real(dp)                   :: rxik
  real(dp)                   :: ryik
  real(dp)                   :: rzik
  real(dp)                   :: rxjk
  real(dp)                   :: ryjk
  real(dp)                   :: rzjk
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xik
  real(dp)                   :: yik
  real(dp)                   :: zik
  real(dp)                   :: xjk
  real(dp)                   :: yjk
  real(dp)                   :: zjk
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  Copy input vectors
!
  xik = xik0
  yik = yik0
  zik = zik0
  xjk = xjk0
  yjk = yjk0
  zjk = zjk0
!
!  Find mid point of i - j vector to k
!
  xij = 0.5_dp*(xjk + xik)
  yij = 0.5_dp*(yjk + yik)
  zij = 0.5_dp*(zjk + zik)
!
!  Find projection of cell vector 1 on to mid point - k vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
!
!  Subtract cell vectors from i-k and j-k vectors 
!
  xik = xik0 - imid*r1x
  yik = yik0 - imid*r1y
  zik = zik0 - imid*r1z
  xjk = xjk0 - imid*r1x
  yjk = yjk0 - imid*r1y
  zjk = zjk0 - imid*r1z
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!       
!  Reinitialise distances squared 
!       
    r2iki = 10000.0_dp*rcut2i
    r2jki = 10000.0_dp*rcut2j
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vectors
!
    rxik = xik + dble(ii)*r1x
    ryik = yik + dble(ii)*r1y
    rzik = zik + dble(ii)*r1z
    rxjk = xjk + dble(ii)*r1x
    ryjk = yjk + dble(ii)*r1y
    rzjk = zjk + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Calculate squares of distances
!
      r2ik = rxik*rxik + ryik*ryik + rzik*rzik
      r2jk = rxjk*rxjk + ryjk*ryjk + rzjk*rzjk
!
!  Check distances squared against cutoffs squared
!
      if (r2ik.le.rcut2i.or.r2jk.le.rcut2j) then
        if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
          nvec = nvec + 1
          if (nvec0+nvec.gt.maxdis) then
            maxdis = nvec0 + nvec + 50
            call changemaxdis
          endif
          dist(nvec0 + nvec) = r2ik
          xtmp(nvec0 + nvec) = rxik
          ytmp(nvec0 + nvec) = ryik
          ztmp(nvec0 + nvec) = rzik
          dist2(nvec0 + nvec) = r2jk
          xtmp2(nvec0 + nvec) = rxjk
          ytmp2(nvec0 + nvec) = ryjk
          ztmp2(nvec0 + nvec) = rzjk
        endif
      endif
!
!  Increment by first vector
!
      ii = ii + idir
      rxik = rxik + rcx1
      ryik = ryik + rcy1
      rzik = rzik + rcz1
      rxjk = rxjk + rcx1
      ryjk = ryjk + rcy1
      rzjk = rzjk + rcz1
!           
!  Check to see if this direction is complete
!                 
      lallfound1 = ((r2ik.gt.r2iki.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jki.and.r2jk.gt.rcut2j))
      r2iki = r2ik
      r2jki = r2jk
    enddo
  enddo
!
  return
  end
!*********************************************************
!  Rfindeither31D : Get any image common to three atoms  *
!*********************************************************
  subroutine rfindeither31D(xil0,yil0,zil0,xjl0,yjl0,zjl0,xkl0,ykl0,zkl0,rcut2i,rcut2j,rcut2k,lincludeself,nvec0,nvec)
!
!  Subroutine that searches for valid images of an atom l that are 
!  within a cutoff of either atom i or atom j or atom k.
!  New iterative algorithm that should work without use of nadd.
!
!  On input :
!
!  xil0         = x-coordinate of i-l vector for central unit cell
!  yil0         = y-coordinate of i-l vector for central unit cell
!  zil0         = z-coordinate of i-l vector for central unit cell
!  xjl0         = x-coordinate of j-l vector for central unit cell
!  yjl0         = y-coordinate of j-l vector for central unit cell
!  zjl0         = z-coordinate of j-l vector for central unit cell
!  xkl0         = x-coordinate of k-l vector for central unit cell
!  ykl0         = y-coordinate of k-l vector for central unit cell
!  zkl0         = z-coordinate of k-l vector for central unit cell
!  rcut2i       = cut-off distance for i to l squared
!  rcut2j       = cut-off distance for j to l squared
!  rcut2k       = cut-off distance for k to l squared
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of l that are within cutoff 
!                 of i or j or k
!  dist         = distance between atoms i and l for each vector
!  dist2        = distance between atoms j and l for each vector
!  dist3        = distance between atoms k and l for each vector
!  xtmp         = x component of valid i-l vector
!  ytmp         = y component of valid i-l vector
!  ztmp         = z component of valid i-l vector
!  xtmp2        = x component of valid j-l vector
!  ytmp2        = y component of valid j-l vector
!  ztmp2        = z component of valid j-l vector
!  xtmp3        = x component of valid j-l vector
!  ytmp3        = y component of valid j-l vector
!  ztmp3        = z component of valid j-l vector
!
!   3/03 Created
!  11/04 Calculation of cell norms eliminated
!   2/09 Argument removed from changemaxdis call
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
!  Julian Gale, NRI, Curtin University, February 2009
!
  use current
  use general
  use numbers,     only : third
  use realvectors
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: nvec0
  integer(i4), intent(out)   :: nvec
  logical,     intent(in)    :: lincludeself
  real(dp),    intent(in)    :: rcut2i
  real(dp),    intent(in)    :: rcut2j
  real(dp),    intent(in)    :: rcut2k
  real(dp),    intent(in)    :: xil0
  real(dp),    intent(in)    :: yil0
  real(dp),    intent(in)    :: zil0
  real(dp),    intent(in)    :: xjl0
  real(dp),    intent(in)    :: yjl0
  real(dp),    intent(in)    :: zjl0
  real(dp),    intent(in)    :: xkl0
  real(dp),    intent(in)    :: ykl0
  real(dp),    intent(in)    :: zkl0
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  logical                    :: lallfound1
  real(dp)                   :: proj1
  real(dp)                   :: r2il
  real(dp)                   :: r2jl
  real(dp)                   :: r2kl
  real(dp)                   :: r2ili
  real(dp)                   :: r2jli
  real(dp)                   :: r2kli
  real(dp)                   :: rcx1
  real(dp)                   :: rcy1
  real(dp)                   :: rcz1
  real(dp)                   :: rnorm
  real(dp)                   :: rxil
  real(dp)                   :: ryil
  real(dp)                   :: rzil
  real(dp)                   :: rxjl
  real(dp)                   :: ryjl
  real(dp)                   :: rzjl
  real(dp)                   :: rxkl
  real(dp)                   :: rykl
  real(dp)                   :: rzkl
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xil
  real(dp)                   :: yil
  real(dp)                   :: zil
  real(dp)                   :: xjl
  real(dp)                   :: yjl
  real(dp)                   :: zjl
  real(dp)                   :: xkl
  real(dp)                   :: ykl
  real(dp)                   :: zkl
!
!  Initialise number of distances to zero
!
  nvec = 0
!
!  Copy input vectors
!
  xil = xil0
  yil = yil0
  zil = zil0
  xjl = xjl0
  yjl = yjl0
  zjl = zjl0
  xkl = xjl0
  ykl = yjl0
  zkl = zjl0
!
!  Find mid point of i - j - k vector to l
!
  xij = third*(xjl + xil + xkl)
  yij = third*(yjl + yil + ykl)
  zij = third*(zjl + zil + zkl)
!
!  Find projection of cell vector 1 on to mid point - l vector
!
  rnorm = xij*xij + yij*yij + zij*zij
  if (rnorm.gt.1.0d-12) rnorm = 1.0_dp/sqrt(rnorm)
  proj1 = rnorm*recipa*(xij*r1x + yij*r1y + zij*r1z)
  imid = nint(proj1)
!
!  Subtract cell vectors from i-k and j-k vectors 
!
  xil = xil0 - imid*r1x
  yil = yil0 - imid*r1y
  zil = zil0 - imid*r1z
  xjl = xjl0 - imid*r1x
  yjl = yjl0 - imid*r1y
  zjl = zjl0 - imid*r1z
  xkl = xkl0 - imid*r1x
  ykl = ykl0 - imid*r1y
  zkl = zkl0 - imid*r1z
!
!  Outer loop over first cell vector direction 
!
  do idir = 1,-1,-2
!       
!  Reinitialise distances squared 
!       
    r2ili = 10000.0_dp*rcut2i
    r2jli = 10000.0_dp*rcut2j
    r2kli = 10000.0_dp*rcut2k
!
!  Loop over first cell vector
!
    lallfound1 = .false.
    if (idir.eq.1) then
      ii = 0
    else
      ii = - 1
    endif
!
!  Set initial coordinate vectors
!
    rxil = xil + dble(ii)*r1x
    ryil = yil + dble(ii)*r1y
    rzil = zil + dble(ii)*r1z
    rxjl = xjl + dble(ii)*r1x
    ryjl = yjl + dble(ii)*r1y
    rzjl = zjl + dble(ii)*r1z
    rxkl = xkl + dble(ii)*r1x
    rykl = ykl + dble(ii)*r1y
    rzkl = zkl + dble(ii)*r1z
!
!  Set increment vector
!
    rcx1 = dble(idir)*r1x
    rcy1 = dble(idir)*r1y
    rcz1 = dble(idir)*r1z
!
    do while (.not.lallfound1)
!
!  Calculate squares of distances
!
      r2il = rxil*rxil + ryil*ryil + rzil*rzil
      r2jl = rxjl*rxjl + ryjl*ryjl + rzjl*rzjl
      r2kl = rxkl*rxkl + rykl*rykl + rzkl*rzkl
!
!  Check distances squared against cutoffs squared
!
      if (r2il.le.rcut2i.or.r2jl.le.rcut2j.or.r2kl.le.rcut2k) then
        if ((r2il.gt.1.0d-12.and.r2jl.gt.1.0d-12.and.r2kl.gt.1.0d-12).or.lincludeself) then
          nvec = nvec + 1
          if (nvec0+nvec.gt.maxdis) then
            maxdis = nvec0 + nvec + 50
            call changemaxdis
          endif
          dist(nvec0 + nvec) = r2il
          xtmp(nvec0 + nvec) = rxil
          ytmp(nvec0 + nvec) = ryil
          ztmp(nvec0 + nvec) = rzil
          dist2(nvec0 + nvec) = r2jl
          xtmp2(nvec0 + nvec) = rxjl
          ytmp2(nvec0 + nvec) = ryjl
          ztmp2(nvec0 + nvec) = rzjl
          dist3(nvec0 + nvec) = r2kl
          xtmp3(nvec0 + nvec) = rxkl
          ytmp3(nvec0 + nvec) = rykl
        endif
      endif
!
!  Increment by first vector
!
      ii = ii + idir
      rxil = rxil + rcx1
      ryil = ryil + rcy1
      rzil = rzil + rcz1
      rxjl = rxjl + rcx1
      ryjl = ryjl + rcy1
      rzjl = rzjl + rcz1
      rxkl = rxkl + rcx1
      rykl = rykl + rcy1
      rzkl = rzkl + rcz1
!           
!  Check to see if this direction is complete
!                 
      lallfound1 = ((r2il.gt.r2ili.and.r2il.gt.rcut2i).and.(r2jl.gt.r2jli.and.r2jl.gt.rcut2j) &
        .and.(r2kl.gt.r2kli.and.r2kl.gt.rcut2k))
      r2ili = r2il
      r2jli = r2jl
      r2kli = r2kl
    enddo
  enddo
!
  return
  end
