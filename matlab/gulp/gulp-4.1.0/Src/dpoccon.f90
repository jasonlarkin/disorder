  subroutine dpoccon(iop)
!
!  This routine creates constraints that are needed to ensure
!  that species with partial occupancies on the same site
!  follow each other's motions.
!
!  ltmp = array of logical flags according to whether a
!         parameter can be varied or not. Passed from
!         setcfg.
!
!   6/95 Initially created.
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, June 2005
!
  use control
  use current
  use defects
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  integer(i4)        :: iop(*)
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: indjx
  integer(i4)        :: indjy
  integer(i4)        :: indjz
  integer(i4)        :: indx
  integer(i4)        :: indy
  integer(i4)        :: indz
  integer(i4)        :: j
  integer(i4)        :: jj
  integer(i4)        :: k
  integer(i4)        :: nati
  logical            :: lcore
  real(dp)           :: r
  real(dp)           :: xal
  real(dp)           :: yal
  real(dp)           :: zal
  real(dp)           :: xd
  real(dp)           :: yd
  real(dp)           :: zd
!
  if (index(keyword,'noco').ne.0) return
!*******************************************************************
!  Check for partial occupancy sites which need to be constrained  *
!*******************************************************************
  do i = 2,ndasym
    ii = ndsptr(i)
    nati = natdefe(ii)
    lcore = (nati.le.maxele)
    xal = xdefe(ii)
    yal = ydefe(ii)
    zal = zdefe(ii)
    indx = 3*(i-1) + 1
    indy = indx + 1
    indz = indy + 1
!
!  Loop over previous atoms to find any on the same site
!
    do j = 1,i-1
      jj = ndsptr(j)
!
!  Check that both species are of the same type
!
      if (natdefe(jj).le.maxele.and.lcore.or.natdefe(jj).gt.maxele.and..not.lcore) then
!
!  Check distance 
!
        xd = xdefe(jj) - xal
        yd = ydefe(jj) - yal
        zd = zdefe(jj) - zal
        r = xd*xd + yd*yd + zd*zd
        if (r.lt.1.0d-10) then
          indjx = 3*(j-1)+1
          indjy = indjx + 1
          indjz = indjy + 1
          if (iop(indjx).eq.1) then
!
!  Add new constraint putting x coordinate of i equal
!  to that of x
!
            ndcon = ndcon + 1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            iop(indx) = 0
            ncdvar(ndcon) = indjx
            ncdfix(ndcon) = indx
            dconco(ndcon) = 1.0_dp
!
!  Check to see whether any other coordinates of i are
!  dependent on the x coordinate of i
!
            do k = 1,ndcon-1
              if (ncdvar(k).eq.indx) then
                ncdvar(k) = indjx
              endif
            enddo
          endif
          if (iop(indjy).eq.1) then
!
!  Add new constraint putting y coordinate of i equal
!  to that of y
!
            ndcon = ndcon + 1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            iop(indy) = 0
            ncdvar(ndcon) = indjy
            ncdfix(ndcon) = indy
            dconco(ndcon) = 1.0_dp
!
!  Check to see whether any other coordinates of i are
!  dependent on the y coordinate of i
!
            do k = 1,ndcon-1
              if (ncdvar(k).eq.indy) then
                ncdvar(k) = indjy
              endif
            enddo
          endif
          if (iop(indjz).eq.1) then
!
!  Add new constraint putting z coordinate of i equal
!  to that of z
!
            ndcon = ndcon + 1
            if (ndcon.ge.maxdcon) then
              maxdcon  =  ndcon + 10
              call changemaxdcon
            endif
            iop(indz) = 0
            ncdvar(ndcon) = indjz
            ncdfix(ndcon) = indz
            dconco(ndcon) = 1.0_dp
!
!  Check to see whether any other coordinates of i are
!  dependent on the z coordinate of i
!
            do k = 1,ndcon-1
              if (ncdvar(k).eq.indz) then
                ncdvar(k) = indjz
              endif
            enddo
          endif
        endif
      endif
    enddo
  enddo
!
  return
  end
