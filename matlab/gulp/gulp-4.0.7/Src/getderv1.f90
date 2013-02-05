  subroutine getderv1(n,xc,gc,lgrad2,lrotalg)
!
!  Collects the derivatives into a linear array, including
!  some symmetry and constraint handling
!
!   8/97 Created from funct
!  10/97 If lrotalg then calculate asymmetric unit derivs
!        by rotation and summation - this is because K
!        point sampling doesn't lead to all derivatives
!        being equal in a free energy gradient calculation
!   5/03 Region 3 modifications added
!   6/03 No of strains corrected to nstrains from 6 in constraint
!        section
!  11/06 Nudging of gradients option added
!   6/09 Charge as a coordinate option added
!   6/09 Starting value of j increased to mvar for radial constraint search
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use configurations, only : lbsmat
  use current
  use derivatives
  use optimisation,   only : loptcellpar
  use symmetry
  use xcgc,           only : lnudgegc
  implicit none
!
!  Passed variables
!
  integer(i4) :: n
  logical     :: lgrad2
  logical     :: lrotalg
  real(dp)    :: gc(*)
  real(dp)    :: xc(*)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ij
  integer(i4) :: ind
  integer(i4) :: indf
  integer(i4) :: indv
  integer(i4) :: iv
  integer(i4) :: k
  integer(i4) :: kk
  integer(i4) :: j
  integer(i4) :: mvar
  integer(i4) :: neq
  integer(i4) :: nj
  integer(i4) :: nr
  integer(i4) :: nv
  logical     :: lfound
  real(dp)    :: cellderv(6)
  real(dp)    :: g1(3)
  real(dp)    :: g2(3)
  real(dp)    :: rotinv(3,3)
  real(dp)    :: rvp(3,3)
  real(dp)    :: xdv
  real(dp)    :: ydv
  real(dp)    :: zdv
!
  mvar  = 3*nasym + nstrains
!
!  For symmetry optimisations correct internal first derivatives
!
  if (lsymopt.and.(.not.lsymderv.or.(lgrad2.and..not.lsymderv2))) then
!
!  Generate non-primitive cell
!
    do i = 1,3
      rvp(1,i) = rv(1,i)
      rvp(2,i) = rv(2,i)
      rvp(3,i) = rv(3,i)
    enddo
    if (ncbl.gt.1) call uncentre(rvp)
!
!  Transform cartesian derivatives to internal coords
!  before symmetry reduction so that rotation can be
!  performed
!
    do kk = 1,numat
      xdv = xdrv(kk)
      ydv = ydrv(kk)
      zdv = zdrv(kk)
      xdrv(kk) = xdv*rvp(1,1) + ydv*rvp(2,1) + zdv*rvp(3,1)
      ydrv(kk) = xdv*rvp(1,2) + ydv*rvp(2,2) + zdv*rvp(3,2)
      zdrv(kk) = xdv*rvp(1,3) + ydv*rvp(2,3) + zdv*rvp(3,3)
    enddo
    if (lrotalg) then
!*****************************
!  Rotate and sum algorithm  *
!*****************************
      do i = 1,nasym
        nr = nrel2(i)
        neq = neqv(i)
        xdrv(i) = xdrv(nr)
        ydrv(i) = ydrv(nr)
        zdrv(i) = zdrv(nr)
        if (neq.gt.1) then
          do j = 2,neq
            nr = nr + 1
            g1(1) = xdrv(nr)
            g1(2) = ydrv(nr)
            g1(3) = zdrv(nr)
            do k = 1,3
              rotinv(1,k) = rop(1,k,nrotop(nr))
              rotinv(2,k) = rop(2,k,nrotop(nr))
              rotinv(3,k) = rop(3,k,nrotop(nr))
            enddo
            g2(1) = rotinv(1,1)*g1(1) + rotinv(2,1)*g1(2) + rotinv(3,1)*g1(3)
            g2(2) = rotinv(1,2)*g1(1) + rotinv(2,2)*g1(2) + rotinv(3,2)*g1(3)
            g2(3) = rotinv(1,3)*g1(1) + rotinv(2,3)*g1(2) + rotinv(3,3)*g1(3)
            xdrv(i) = xdrv(i) + g2(1)
            ydrv(i) = ydrv(i) + g2(2)
            zdrv(i) = zdrv(i) + g2(3)
          enddo
        endif
      enddo
    else
!***************************
!  Conventional algorithm  *
!***************************
      do i = 1,nasym
        nr = nrel2(i)
        neq = neqv(i)
        xdrv(i) = neq*xdrv(nr)
        ydrv(i) = neq*ydrv(nr)
        zdrv(i) = neq*zdrv(nr)
        if (lbsmat(i+nsft)) raderv(i) = neq*raderv(nr)
      enddo
    endif
  elseif (ndim.eq.3) then
!
!  Generate non-primitive cell
!
    do i = 1,3
      rvp(1,i) = rv(1,i)
      rvp(2,i) = rv(2,i)
      rvp(3,i) = rv(3,i)
    enddo
    if (ncbl.gt.1) call uncentre(rvp)
!
!  Transform cartesian derivatives to internal coords
!
    do kk = 1,nasym
      xdv = xdrv(kk)
      ydv = ydrv(kk)
      zdv = zdrv(kk)
      xdrv(kk) = xdv*rvp(1,1) + ydv*rvp(2,1) + zdv*rvp(3,1)
      ydrv(kk) = xdv*rvp(1,2) + ydv*rvp(2,2) + zdv*rvp(3,2)
      zdrv(kk) = xdv*rvp(1,3) + ydv*rvp(2,3) + zdv*rvp(3,3)
    enddo
  elseif (ndim.eq.2) then
!
!  Transform cartesian derivatives to internal coords for x and y
!
    do kk = 1,nasym
      xdv = xdrv(kk)
      ydv = ydrv(kk)
      xdrv(kk) = xdv*rv(1,1) + ydv*rv(2,1)
      ydrv(kk) = xdv*rv(1,2) + ydv*rv(2,2)
    enddo
  elseif (ndim.eq.1) then
!
!  Transform cartesian derivatives to internal coords for x only
!
    do kk = 1,nasym
      xdv = xdrv(kk)
      xdrv(kk) = xdv*rv(1,1)
    enddo
  endif
!*********************************
!  Collect internal derivatives  *
!*********************************
  do i = ncell+1,(n-nbsm)
    ind = iopt(i)
    nj = (ind-(nstrains-2))/3
    ij = ind - (3*nj+(nstrains-3))
    if (ij.eq.1) then
      gc(i) = xdrv(nj)
    elseif (ij.eq.2) then
      gc(i) = ydrv(nj)
    else
      gc(i) = zdrv(nj)
    endif
  enddo
  if (nbsm.gt.0) then
    do i = (n-nbsm+1),n
      ind = iopt(i) - mvar
      gc(i) = raderv(ind)
    enddo
  endif
!****************************
!  Constrained derivatives  *
!****************************
  if (ncon.gt.0) then
    do i = 1,ncon
      indf = ncfix(i)
      indv = ncvar(i)
      if (indf.gt.mvar) then
!
!  Radial derivatives
!
        nv = indf - mvar
        lfound = .false.
        j = mvar
        do while (.not.lfound.and.j.le.n)
          j = j + 1
          if (indv.eq.iopt(j)) lfound = .true.
        enddo
        gc(j) = gc(j) + raderv(nv)*conco(i)
      elseif (indf.gt.nstrains) then
!
!  Internal derivatives
!
        nv = (indf - (nstrains-2))/3
        iv = indf - (3*nv+(nstrains-3))
        lfound = .false.
        j = ncell
        do while (.not.lfound.and.j.le.n)
          j = j + 1
          if (indv.eq.iopt(j)) lfound = .true.
        enddo
        if (iv.eq.1) then
          gc(j) = gc(j) + xdrv(nv)*conco(i)
        elseif (iv.eq.2) then
          gc(j) = gc(j) + ydrv(nv)*conco(i)
        else
          gc(j) = gc(j) + zdrv(nv)*conco(i)
        endif
      else
!
!  Strain derivatives
!
        strderv(indv) = strderv(indv) + strderv(indf)*conco(i)
      endif
    enddo
  endif
!*******************************
!  Collect strain derivatives  *
!*******************************
  if (ncell.gt.0) then
    if (loptcellpar) then
      call celldrv(strderv,cellderv) 
      do i = 1,ncell
        gc(i) = cellderv(iopt(i))
      enddo
    else
      do i = 1,ncell
        gc(i) = strderv(iopt(i))
      enddo
    endif
  endif
!*************************
!  Nudging of gradients  *
!*************************
  if (lnudgegc) then
    call nudge(n,xc,gc)
  endif
!
  return
  end
