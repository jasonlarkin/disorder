  subroutine setdistance(lreset)
!
!  Subroutine for finding all distances required in real space
!
!  If lreset is true, then search for atoms is performed. If
!  false, then the distances are computed based on current list.
!
!   1/05 Created based on reale.f
!   3/07 Bondtype arrays added
!   1/09 Integer datatypes all explicitly declared
!   2/09 Expanding and contracting of maxdis arrays removed
!   7/09 cutoffmax now passed via general module
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
!  Julian Gale, NRI, Curtin University, July 2009
!
  use control
  use current
  use distances
  use element
  use general,      only : cutoffmax
  use molecule
  use parallel
  use realvectors
  use shell
  use times
  implicit none
!
!  Passed variables
!
  logical,     intent(in)                      :: lreset
!
!  Local variables
!
  integer(i4)                                  :: estmaxndistance
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indmi
  integer(i4)                                  :: indmj
  integer(i4)                                  :: ixd
  integer(i4)                                  :: iyd
  integer(i4)                                  :: izd
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: k
  integer(i4)                                  :: kk
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  logical                                      :: lcspair
  logical                                      :: lmolok
  logical                                      :: lself
  real(dp)                                     :: cputime
  real(dp)                                     :: cut2
  real(dp)                                     :: cutvol
  real(dp)                                     :: numberdensity
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: volume
  real(dp)                                     :: xci
  real(dp)                                     :: yci
  real(dp)                                     :: zci
  real(dp)                                     :: xcrd
  real(dp)                                     :: ycrd
  real(dp)                                     :: zcrd
!
  time1 = cputime()
  if (lreset) then
!**********************************************************************************
!  Reset - recompute all valid vectors based on maximum cutoff for any potential  *
!**********************************************************************************
!
!  Get maximum cutoff distance
!
    call setcutoffmax
!
!  Add tolerance for storage
!
    cutoffmax = cutoffmax + extracutoff
!
!  Estimate size of arrays required based on number density within maximum cutoff
!
    cut2 = cutoffmax*cutoffmax
    cutvol = cutoffmax*cut2
    numberdensity = dble(numat)/volume(rv)
    estmaxndistance = numat*(numat+1)*nint(numberdensity*cutvol)/2
!
!  Increase memory if required
!
    if (estmaxndistance.gt.maxndistancetotal) then
      maxndistancetotal = estmaxndistance
      call changemaxndistancetotal
    endif
!
!  Save cell indices of atoms at present position
!
    icosxs(1:numat) = icosx(1:numat)
    icosys(1:numat) = icosy(1:numat)
    icoszs(1:numat) = icosz(1:numat)
!
!  Zero total number of distances
!
    ndistancetotal = 0
!
!  Outer loop over sites
!
    do i = 1,numat
      nati = nat(i)
      oci = occuf(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
!
!  Molecule handling
!
      if (lmol) then
        nmi = natmol(i)
        indmi = nmolind(i)
        call mindtoijk(indmi,ixi,iyi,izi)
      endif
!
!  Set pointer to start of data for atom i in distance arrays - 1 & zero no. of distances for i
!
      ndistance(i) = 0
      ndistanceind(i) = ndistancetotal
!
!  Find other sites within maximum cutoff distance
!
      do j = 1,i
        natj = nat(j)
        ocj = occuf(j)
        xcrd = xclat(j) - xci
        ycrd = yclat(j) - yci
        zcrd = zclat(j) - zci
!
!  Possible core-shell flag
!
        lcspair = (abs(nati-natj).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
!
!  Molecule handling
!
        if (lmol) then
          nmj = natmol(j)
          indmj = nmolind(j)
          call mindtoijk(indmj,ixj,iyj,izj)
          ixj = ixj - ixi
          iyj = iyj - iyi
          izj = izj - izi
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
        else
          lmolok = .false.
        endif
!
!  Find valid vectors
!
        if (ndim.eq.3) then
          call rsearch3D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.2) then
          call rsearch2D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        elseif (ndim.eq.1) then
          call rsearch1D(xcrd,ycrd,zcrd,lmolok,lcspair,i,j,nmi,ixj,iyj,izj,nor,nmolonly,lself,cut2)
        endif
!
!  Check dimensions of arrays
!
        if (nor+ndistancetotal.gt.maxndistancetotal) then
          maxndistancetotal = nint(1.1_dp*dble(nor + ndistancetotal))
          call changemaxndistancetotal
        endif
!
!  Add valid vectors on to list for i
!
        call dcopy(nor,dist,1_i4,distance2(ndistancetotal+1),1_i4)
        call dcopy(nor,xtmp,1_i4,distancexyz(1,ndistancetotal+1),3_i4)
        call dcopy(nor,ytmp,1_i4,distancexyz(2,ndistancetotal+1),3_i4)
        call dcopy(nor,ztmp,1_i4,distancexyz(3,ndistancetotal+1),3_i4)
!
!  Add pointers to j for i vector
!
        do k = 1,nor
          ndistanceptr(ndistancetotal+k) = j
        enddo
!
!  Add integers to arrays
!
        do k = 1,nor
          ndistancecell(1,ndistancetotal+k) = cellindex(1,k)
          ndistancecell(2,ndistancetotal+k) = cellindex(2,k)
          ndistancecell(3,ndistancetotal+k) = cellindex(3,k)
          ndistbotype(ndistancetotal+k)  = nbotype(k)
          ndistbotype2(ndistancetotal+k) = nbotype2(k)
        enddo
        ndistanceij(j,i) = nor
        ndistancemolonly(j,i) = nmolonly
!
!  Add logicals to arrays
!
        do k = 1,nor
          distl1bond(ndistancetotal+k) = lbonded(k)
          distl2bond(ndistancetotal+k) = l2bonds(k)
          distl3bond(ndistancetotal+k) = l3bonds(k)
          distlptrmol(ndistancetotal+k) = lptrmol(k)
        enddo
        distlself(j,i) = lself
!
!  Increment partial and total distance counters
!
        ndistance(i) = ndistance(i) + nor
        ndistancetotal = ndistancetotal + nor
      enddo
    enddo
  else
!*****************************************************************
!  Update - recompute distances based on existing atom listings  *
!*****************************************************************
    ind = 0
    do i = 1,numat
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      ixd = icosxs(i) - icosx(i)
      iyd = icosys(i) - icosy(i)
      izd = icoszs(i) - icosz(i)
      do k = 1,ndistance(i)
        ind = ind + 1
        j = ndistanceptr(ind)
!
!  Compute central cell difference in coordinates
!
        xcrd = xclat(j) - xci
        ycrd = yclat(j) - yci
        zcrd = zclat(j) - zci
!
!  Add unit cell shift
!
        ii = ndistancecell(1,ind) - icosxs(j) + icosx(j) + ixd
        jj = ndistancecell(2,ind) - icosys(j) + icosy(j) + iyd
        kk = ndistancecell(3,ind) - icoszs(j) + icosz(j) + izd
        xcrd = xcrd + ii*r1x + jj*r2x + kk*r3x
        ycrd = ycrd + ii*r1y + jj*r2y + kk*r3y
        zcrd = zcrd + ii*r1z + jj*r2z + kk*r3z
!
!  Store square of distance
!
        distance2(ind) = xcrd*xcrd + ycrd*ycrd + zcrd*zcrd
        distancexyz(1,ind) = xcrd
        distancexyz(2,ind) = ycrd
        distancexyz(3,ind) = zcrd
      enddo
    enddo
  endif
!*************************************
!  Perform square root of distances  *
!*************************************
  do i = 1,ndistancetotal
    distance(i) = sqrt(distance2(i))
  enddo
!
!  Timing
!
  time2 = cputime()
  tsearch = tsearch + time2 - time1
!
  return
  end
