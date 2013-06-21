!******************************************************
!  Interfaces to routines for correct dimensionality  *
!******************************************************
  subroutine rfindboth(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of both atom i or atom j.
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
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindeither
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2i
  real(dp),          intent(in)    :: rcut2j
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
!
  if (ndim.eq.3) then
    call rfindboth3D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec,vectorpair)
  elseif (ndim.eq.2) then
    call rfindboth2D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec,vectorpair)
  elseif (ndim.eq.1) then
    call rfindboth1D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec,vectorpair)
  endif
!
  return
  end
!****************************************************
!  Rfindboth : Get any image common to two atoms  *
!****************************************************
  subroutine rfindboth3D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of both atom i or atom j.
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
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindeither3D
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2i
  real(dp),          intent(in)    :: rcut2j
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
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
  integer(i4)                :: nvecj0
  integer(i4)                :: nveck0
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
              if (r2ik.le.rcut2i.and.r2jk.le.rcut2j) then
                if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
                  nvec = nvec + 1
                  if (nvec0+nvec.gt.vectorpair%maxdim_pair) then
                    call changemaxvectorpair(vectorpair,nvec0+nvec+50)
                  endif
                  vectorpair%distance_pair1(nvec0 + nvec) = r2ik
                  vectorpair%xvector_pair1(nvec0 + nvec) = rxik
                  vectorpair%yvector_pair1(nvec0 + nvec) = ryik
                  vectorpair%zvector_pair1(nvec0 + nvec) = rzik
                  vectorpair%distance_pair2(nvec0 + nvec) = r2jk
                  vectorpair%xvector_pair2(nvec0 + nvec) = rxjk
                  vectorpair%yvector_pair2(nvec0 + nvec) = ryjk
                  vectorpair%zvector_pair2(nvec0 + nvec) = rzjk
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
              lallfound3 = ((r2ik.gt.r2ikk.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jkk.and.r2jk.gt.rcut2j))
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
          lallfound2 = ((r2ik.gt.r2ikj.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jkj.and.r2jk.gt.rcut2j).and.nvec.eq.nveck0)
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
      lallfound1 = ((r2ik.gt.r2iki.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jki.and.r2jk.gt.rcut2j).and.nvec.eq.nvecj0)
      r2iki = r2ik
      r2jki = r2jk
    enddo
  enddo
!
  return
  end
!****************************************************
!  Rfindboth : Get any image common to two atoms  *
!****************************************************
  subroutine rfindboth2D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of both atom i or atom j.
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
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindeither2D
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2i
  real(dp),          intent(in)    :: rcut2j
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: nvecj0
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
          if (r2ik.le.rcut2i.and.r2jk.le.rcut2j) then
            if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
              nvec = nvec + 1
              if (nvec0+nvec.gt.vectorpair%maxdim_pair) then
                call changemaxvectorpair(vectorpair,nvec0+nvec+50)
              endif
              vectorpair%distance_pair1(nvec0 + nvec) = r2ik
              vectorpair%xvector_pair1(nvec0 + nvec) = rxik
              vectorpair%yvector_pair1(nvec0 + nvec) = ryik
              vectorpair%zvector_pair1(nvec0 + nvec) = rzik
              vectorpair%distance_pair2(nvec0 + nvec) = r2jk
              vectorpair%xvector_pair2(nvec0 + nvec) = rxjk
              vectorpair%yvector_pair2(nvec0 + nvec) = ryjk
              vectorpair%zvector_pair2(nvec0 + nvec) = rzjk
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
          lallfound2 = ((r2ik.gt.r2ikj.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jkj.and.r2jk.gt.rcut2j))
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
      lallfound1 = ((r2ik.gt.r2iki.and.r2ik.gt.rcut2i).and.(r2jk.gt.r2jki.and.r2jk.gt.rcut2j).and.nvec.eq.nvecj0)
      r2iki = r2ik
      r2jki = r2jk
    enddo
  enddo
!
  return
  end
!****************************************************
!  Rfindboth : Get any image common to two atoms  *
!****************************************************
  subroutine rfindboth1D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2i,rcut2j,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of both atom i or atom j.
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
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindeither1D
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2i
  real(dp),          intent(in)    :: rcut2j
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
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
      if (r2ik.le.rcut2i.and.r2jk.le.rcut2j) then
        if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
          nvec = nvec + 1
          if (nvec0+nvec.gt.vectorpair%maxdim_pair) then
            call changemaxvectorpair(vectorpair,nvec0+nvec+50)
          endif
          vectorpair%distance_pair1(nvec0 + nvec) = r2ik
          vectorpair%xvector_pair1(nvec0 + nvec) = rxik
          vectorpair%yvector_pair1(nvec0 + nvec) = ryik
          vectorpair%zvector_pair1(nvec0 + nvec) = rzik
          vectorpair%distance_pair2(nvec0 + nvec) = r2jk
          vectorpair%xvector_pair2(nvec0 + nvec) = rxjk
          vectorpair%yvector_pair2(nvec0 + nvec) = ryjk
          vectorpair%zvector_pair2(nvec0 + nvec) = rzjk
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
!******************************************************
!  Interfaces to routines for correct dimensionality  *
!******************************************************
  subroutine rfindmid(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of the mid point of atom i and atom j.
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
!  rcut2        = cut-off distance 
!  nvec0        = initial location of vectors - 1
!  lincludeself = if .true. then include self-term
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindboth
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
!
  if (ndim.eq.3) then
    call rfindmid3D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,lincludeself,nvec0,nvec,vectorpair)
  elseif (ndim.eq.2) then
    call rfindmid2D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,lincludeself,nvec0,nvec,vectorpair)
  elseif (ndim.eq.1) then
    call rfindmid1D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,lincludeself,nvec0,nvec,vectorpair)
  endif
!
  return
  end
!*************************************************
!  Rfindmid : Get any image common to two atoms  *
!*************************************************
  subroutine rfindmid3D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of the mid point of atom i and atom j.
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
!  rcut2        = cut-off distance 
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindboth3D
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
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
  integer(i4)                :: nvecj0
  integer(i4)                :: nveck0
  logical                    :: lallfound1
  logical                    :: lallfound2
  logical                    :: lallfound3
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: proj3
  real(dp)                   :: r2ij
  real(dp)                   :: r2ik
  real(dp)                   :: r2jk
  real(dp)                   :: r2iji
  real(dp)                   :: r2ijj
  real(dp)                   :: r2ijk
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
  real(dp)                   :: rxij
  real(dp)                   :: ryij
  real(dp)                   :: rzij
  real(dp)                   :: rxik
  real(dp)                   :: ryik
  real(dp)                   :: rzik
  real(dp)                   :: rxjk
  real(dp)                   :: ryjk
  real(dp)                   :: rzjk
  real(dp)                   :: rxiji
  real(dp)                   :: ryiji
  real(dp)                   :: rziji
  real(dp)                   :: rxiki
  real(dp)                   :: ryiki
  real(dp)                   :: rziki
  real(dp)                   :: rxijj
  real(dp)                   :: ryijj
  real(dp)                   :: rzijj
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
  real(dp)                   :: xij0
  real(dp)                   :: yij0
  real(dp)                   :: zij0
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
!  Save mid point vector
!
  xij0 = xij
  yij0 = yij
  zij0 = zij
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
!  Subtract cell vectors from i-j, i-k and j-k vectors 
!
  xij = xij0 - imid*r1x - jmid*r2x - kmid*r3x
  yij = yij0 - imid*r1y - jmid*r2y - kmid*r3y
  zij = zij0 - imid*r1z - jmid*r2z - kmid*r3z
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
    r2iji = 10000.0_dp*rcut2
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
    rxiji = xij + dble(ii)*r1x 
    ryiji = yij + dble(ii)*r1y
    rziji = zij + dble(ii)*r1z
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
        r2ijj = 10000.0_dp*rcut2
!
!  Loop over second cell vector
!
        lallfound2 = .false.
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vectors
!
        rxijj = rxiji + dble(jj)*r2x
        ryijj = ryiji + dble(jj)*r2y
        rzijj = rziji + dble(jj)*r2z
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
            r2ijk = 10000.0_dp*rcut2
!
!  Loop over third cell vector
!
            lallfound3 = .false.
            if (kdir.eq.1) then
              kk = 0
            else
              kk = - 1
            endif
!
!  Set initial coordinate vectors
!
            rxij = rxijj + dble(kk)*r3x
            ryij = ryijj + dble(kk)*r3y
            rzij = rzijj + dble(kk)*r3z
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
              r2ij = rxij*rxij + ryij*ryij + rzij*rzij
              r2ik = rxik*rxik + ryik*ryik + rzik*rzik
              r2jk = rxjk*rxjk + ryjk*ryjk + rzjk*rzjk
!
!  Check distance squared against cutoff squared
!
              if (r2ij.le.rcut2) then
                if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
                  nvec = nvec + 1
                  if (nvec0+nvec.gt.vectorpair%maxdim_pair) then
                    call changemaxvectorpair(vectorpair,nvec0+nvec+50)
                  endif
                  vectorpair%distance_pair1(nvec0 + nvec) = r2ik
                  vectorpair%xvector_pair1(nvec0 + nvec) = rxik
                  vectorpair%yvector_pair1(nvec0 + nvec) = ryik
                  vectorpair%zvector_pair1(nvec0 + nvec) = rzik
                  vectorpair%distance_pair2(nvec0 + nvec) = r2jk
                  vectorpair%xvector_pair2(nvec0 + nvec) = rxjk
                  vectorpair%yvector_pair2(nvec0 + nvec) = ryjk
                  vectorpair%zvector_pair2(nvec0 + nvec) = rzjk
                endif
              endif
!
!  Increment by third vector
!
              kk = kk + kdir
              rxij = rxij + rcx3
              ryij = ryij + rcy3
              rzij = rzij + rcz3
              rxik = rxik + rcx3
              ryik = ryik + rcy3
              rzik = rzik + rcz3
              rxjk = rxjk + rcx3
              ryjk = ryjk + rcy3
              rzjk = rzjk + rcz3
!                     
!  Check to see if this direction is complete
!                       
              lallfound3 = (r2ij.gt.r2ijk.and.r2ij.gt.rcut2)
              r2ijk = r2ij
            enddo
          enddo
!
!  Increment by second vector
!
          jj = jj + jdir
          rxijj = rxijj + rcx2
          ryijj = ryijj + rcy2
          rzijj = rzijj + rcz2
          rxikj = rxikj + rcx2
          ryikj = ryikj + rcy2
          rzikj = rzikj + rcz2
          rxjkj = rxjkj + rcx2
          ryjkj = ryjkj + rcy2
          rzjkj = rzjkj + rcz2
!
!  Check to see if this direction is complete
!
          lallfound2 = (r2ij.gt.r2ijj.and.r2ij.gt.rcut2.and.nvec.eq.nveck0)
          r2ijj = r2ij
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxiji = rxiji + rcx1
      ryiji = ryiji + rcy1
      rziji = rziji + rcz1
      rxiki = rxiki + rcx1
      ryiki = ryiki + rcy1
      rziki = rziki + rcz1
      rxjki = rxjki + rcx1
      ryjki = ryjki + rcy1
      rzjki = rzjki + rcz1
!
!  Check to see if this direction is complete
!
      lallfound1 = (r2ij.gt.r2iji.and.r2ij.gt.rcut2.and.nvec.eq.nvecj0)
      r2iji = r2ij
    enddo
  enddo
!
  return
  end
!*************************************************
!  Rfindmid : Get any image common to two atoms  *
!*************************************************
  subroutine rfindmid2D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of the mid point of atom i and atom j.
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
!  rcut2        = cut-off distance 
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindboth2D
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  integer(i4)                :: jdir
  integer(i4)                :: jj
  integer(i4)                :: jmid
  integer(i4)                :: nvecj0
  logical                    :: lallfound1
  logical                    :: lallfound2
  real(dp)                   :: proj1
  real(dp)                   :: proj2
  real(dp)                   :: r2ij
  real(dp)                   :: r2ik
  real(dp)                   :: r2jk
  real(dp)                   :: r2iji
  real(dp)                   :: r2ijj
  real(dp)                   :: rcx1
  real(dp)                   :: rcx2
  real(dp)                   :: rcy1
  real(dp)                   :: rcy2
  real(dp)                   :: rcz1
  real(dp)                   :: rcz2
  real(dp)                   :: rnorm
  real(dp)                   :: rxij
  real(dp)                   :: ryij
  real(dp)                   :: rzij
  real(dp)                   :: rxik
  real(dp)                   :: ryik
  real(dp)                   :: rzik
  real(dp)                   :: rxjk
  real(dp)                   :: ryjk
  real(dp)                   :: rzjk
  real(dp)                   :: rxiji
  real(dp)                   :: ryiji
  real(dp)                   :: rziji
  real(dp)                   :: rxiki
  real(dp)                   :: ryiki
  real(dp)                   :: rziki
  real(dp)                   :: rxjki
  real(dp)                   :: ryjki
  real(dp)                   :: rzjki
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xij0
  real(dp)                   :: yij0
  real(dp)                   :: zij0
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
!  Save mid point vector
!
  xij0 = xij
  yij0 = yij
  zij0 = zij
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
  xij = xij0 - imid*r1x - jmid*r2x
  yij = yij0 - imid*r1y - jmid*r2y
  zij = zij0 - imid*r1z - jmid*r2z
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
    r2iji = 10000.0_dp*rcut2
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
    rxiji = xij + dble(ii)*r1x 
    ryiji = yij + dble(ii)*r1y
    rziji = zij + dble(ii)*r1z
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
        r2ijj = 10000.0_dp*rcut2
!
!  Loop over third cell vector
!
        lallfound2 = .false.
        if (jdir.eq.1) then
          jj = 0
        else
          jj = - 1
        endif
!
!  Set initial coordinate vectors
!
        rxij = rxiji + dble(jj)*r2x
        ryij = ryiji + dble(jj)*r2y
        rzij = rziji + dble(jj)*r2z
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
          r2ij = rxij*rxij + ryij*ryij + rzij*rzij
          r2ik = rxik*rxik + ryik*ryik + rzik*rzik
          r2jk = rxjk*rxjk + ryjk*ryjk + rzjk*rzjk
!
!  Check distances squared against cutoffs squared
!
          if (r2ij.le.rcut2) then
            if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
              nvec = nvec + 1
              if (nvec0+nvec.gt.vectorpair%maxdim_pair) then
                call changemaxvectorpair(vectorpair,nvec0+nvec+50)
              endif
              vectorpair%distance_pair1(nvec0 + nvec) = r2ik
              vectorpair%xvector_pair1(nvec0 + nvec) = rxik
              vectorpair%yvector_pair1(nvec0 + nvec) = ryik
              vectorpair%zvector_pair1(nvec0 + nvec) = rzik
              vectorpair%distance_pair2(nvec0 + nvec) = r2jk
              vectorpair%xvector_pair2(nvec0 + nvec) = rxjk
              vectorpair%yvector_pair2(nvec0 + nvec) = ryjk
              vectorpair%zvector_pair2(nvec0 + nvec) = rzjk
            endif
          endif
!
!  Increment by second vector
!
          jj = jj + jdir
          rxij = rxij + rcx2
          ryij = ryij + rcy2
          rzij = rzij + rcz2
          rxik = rxik + rcx2
          ryik = ryik + rcy2
          rzik = rzik + rcz2
          rxjk = rxjk + rcx2
          ryjk = ryjk + rcy2
          rzjk = rzjk + rcz2
!                     
!  Check to see if this direction is complete  
!                     
          lallfound2 = (r2ij.gt.r2ijj.and.r2ij.gt.rcut2)
          r2ijj = r2ij
        enddo
      enddo
!
!  Increment by first vector
!
      ii = ii + idir
      rxiji = rxiji + rcx1
      ryiji = ryiji + rcy1
      rziji = rziji + rcz1
      rxiki = rxiki + rcx1
      ryiki = ryiki + rcy1
      rziki = rziki + rcz1
      rxjki = rxjki + rcx1
      ryjki = ryjki + rcy1
      rzjki = rzjki + rcz1
!
!  Check to see if this direction is complete
!
      lallfound1 = (r2ij.gt.r2iji.and.r2ij.gt.rcut2.and.nvec.eq.nvecj0)
      r2iji = r2ij
    enddo
  enddo
!
  return
  end
!*************************************************
!  Rfindmid : Get any image common to two atoms  *
!*************************************************
  subroutine rfindmid1D(xik0,yik0,zik0,xjk0,yjk0,zjk0,rcut2,lincludeself,nvec0,nvec,vectorpair)
!
!  Subroutine that searches for valid images of an atom k that are 
!  within a cutoff of mid point of atom i and atom j.
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
!  rcut2        = cut-off distance 
!  lincludeself = if .true. then include self-term, if found
!  nvec0        = initial location of vectors - 1
!
!  On exit :
!
!  nvec         = number of valid images of k that are within cutoff of i or j
!  vectorpair   = type containing:
!
!  distance_pair1 = distance between atoms i and k for each vector
!  distance_pair2 = distance between atoms j and k for each vector
!  xvector_pair1  = x component of valid i-k vector
!  yvector_pair1  = y component of valid i-k vector
!  zvector_pair1  = z component of valid i-k vector
!  xvector_pair2  = x component of valid j-k vector
!  yvector_pair2  = y component of valid j-k vector
!  zvector_pair2  = z component of valid j-k vector
!
!   4/09 Created based on rfindboth1D
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use current
  use general
  use shell
  use vectors,      only : vector_pair
  implicit none
!
!  Passed variables
!
  integer(i4),       intent(in)    :: nvec0
  integer(i4),       intent(out)   :: nvec
  logical,           intent(in)    :: lincludeself
  real(dp),          intent(in)    :: rcut2
  real(dp),          intent(in)    :: xik0
  real(dp),          intent(in)    :: yik0
  real(dp),          intent(in)    :: zik0
  real(dp),          intent(in)    :: xjk0
  real(dp),          intent(in)    :: yjk0
  real(dp),          intent(in)    :: zjk0
  type(vector_pair), intent(out)   :: vectorpair
!
!  Local variables
!
  integer(i4)                :: idir
  integer(i4)                :: ii
  integer(i4)                :: imid
  logical                    :: lallfound1
  real(dp)                   :: proj1
  real(dp)                   :: r2ij
  real(dp)                   :: r2ik
  real(dp)                   :: r2jk
  real(dp)                   :: r2iji
  real(dp)                   :: rcx1
  real(dp)                   :: rcy1
  real(dp)                   :: rcz1
  real(dp)                   :: rnorm
  real(dp)                   :: rxij
  real(dp)                   :: ryij
  real(dp)                   :: rzij
  real(dp)                   :: rxik
  real(dp)                   :: ryik
  real(dp)                   :: rzik
  real(dp)                   :: rxjk
  real(dp)                   :: ryjk
  real(dp)                   :: rzjk
  real(dp)                   :: xij
  real(dp)                   :: yij
  real(dp)                   :: zij
  real(dp)                   :: xij0
  real(dp)                   :: yij0
  real(dp)                   :: zij0
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
!  Save mid point vector
!
  xij0 = xij
  yij0 = yij
  zij0 = zij
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
  xij = xij0 - imid*r1x
  yij = yij0 - imid*r1y
  zij = zij0 - imid*r1z
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
    r2iji = 10000.0_dp*rcut2
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
    rxij = xij + dble(ii)*r1x
    ryij = yij + dble(ii)*r1y
    rzij = zij + dble(ii)*r1z
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
      r2ij = rxij*rxij + ryij*ryij + rzij*rzij
      r2ik = rxik*rxik + ryik*ryik + rzik*rzik
      r2jk = rxjk*rxjk + ryjk*ryjk + rzjk*rzjk
!
!  Check distances squared against cutoffs squared
!
      if (r2ij.le.rcut2) then
        if ((r2ik.gt.1.0d-12.and.r2jk.gt.1.0d-12).or.lincludeself) then
          nvec = nvec + 1
          if (nvec0+nvec.gt.vectorpair%maxdim_pair) then
            call changemaxvectorpair(vectorpair,nvec0+nvec+50)
          endif
          vectorpair%distance_pair1(nvec0 + nvec) = r2ik
          vectorpair%xvector_pair1(nvec0 + nvec) = rxik
          vectorpair%yvector_pair1(nvec0 + nvec) = ryik
          vectorpair%zvector_pair1(nvec0 + nvec) = rzik
          vectorpair%distance_pair2(nvec0 + nvec) = r2jk
          vectorpair%xvector_pair2(nvec0 + nvec) = rxjk
          vectorpair%yvector_pair2(nvec0 + nvec) = ryjk
          vectorpair%zvector_pair2(nvec0 + nvec) = rzjk
        endif
      endif
!
!  Increment by first vector
!
      ii = ii + idir
      rxij = rxij + rcx1
      ryij = ryij + rcy1
      rzij = rzij + rcz1
      rxik = rxik + rcx1
      ryik = ryik + rcy1
      rzik = rzik + rcz1
      rxjk = rxjk + rcx1
      ryjk = ryjk + rcy1
      rzjk = rzjk + rcz1
!           
!  Check to see if this direction is complete
!                 
      lallfound1 = (r2ij.gt.r2iji.and.r2ij.gt.rcut2) 
      r2iji = r2ij
    enddo
  enddo
!
  return
  end
