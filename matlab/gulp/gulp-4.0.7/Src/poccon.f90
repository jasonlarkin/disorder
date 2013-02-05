  subroutine poccon(ltmp)
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
!   5/03 Style updated
!   4/05 Intent added
!   2/06 Bug in referencing of arrays for X case fixed
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
!  Copyright Curtin University 2006
!
!  Julian Gale, NRI, Curtin University, February 2006
!
  use configurations
  use control
  use current
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  logical,     intent(inout) :: ltmp(*)
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: indjx
  integer(i4)                :: indjy
  integer(i4)                :: indjz
  integer(i4)                :: indx
  integer(i4)                :: indy
  integer(i4)                :: indz
  integer(i4)                :: j
  integer(i4)                :: k
  integer(i4)                :: nati
  logical                    :: lcore
  real(dp)                   :: r
  real(dp)                   :: xal
  real(dp)                   :: yal
  real(dp)                   :: zal
  real(dp)                   :: xd
  real(dp)                   :: yd
  real(dp)                   :: zd
!
  if (index(keyword,'noco').ne.0) return
!*******************************************************************
!  Check for partial occupancy sites which need to be constrained  *
!*******************************************************************
  do i = 2,nasym
    nati = iatn(i)
    lcore = (nati.le.maxele)
    xal = xalat(i)
    yal = yalat(i)
    zal = zalat(i)
    indx = 3*(i-1) + nstrains + 1
    indy = indx + 1
    indz = indy + 1
!
!  Loop over previous atoms to find any on the same site
!
    do j = 1,i-1
!
!  Check that both species are of the same type
!
      if (iatn(j).le.maxele.and.lcore.or.iatn(j).gt.maxele.and..not.lcore) then
!
!  Check distance 
!
        xd = xalat(j) - xal
        yd = yalat(j) - yal
        zd = zalat(j) - zal
        r = xd*xd + yd*yd + zd*zd
        if (r.lt.1.0d-10) then
          indjx = 3*(j-1) + nstrains + 1
          indjy = indjx + 1
          indjz = indjy + 1
          if (ltmp(indjx)) then
            if (ncontot.ge.maxcontot) then
              maxcontot = ncontot + 10
              call changemaxcontot
            endif
!
!  Add new constraint putting x coordinate of i equal
!  to that of x
!
            if (ncf.lt.ncfg) then
              do k = ncontot,n1con(ncf+1),-1
                ncvarcfg(k+1) = ncvarcfg(k)
                ncfixcfg(k+1) = ncfixcfg(k)
                concocfg(k+1) = concocfg(k)
                nconcfg(k+1) = nconcfg(k)
                conaddcfg(k+1) = conaddcfg(k)
              enddo
              do k = ncf+1,ncfg
                n1con(k)=n1con(k) + 1
              enddo
            endif
            ncontot = ncontot + 1
            ltmp(indx) = .false.
            ncon = ncon + 1
            ncvarcfg(ncfst+ncon) = indjx
            ncfixcfg(ncfst+ncon) = indx
            concocfg(ncfst+ncon) = 1.0_dp
            conaddcfg(ncfst+ncon) = 0.0_dp
            nconcfg(ncfst+ncon) = ncf
!
!  Check to see whether any other coordinates of i are
!  dependent on the x coordinate of i
!
            do k = 1,ncon-1
              if (ncvarcfg(ncfst+k).eq.indx) then
                ncvarcfg(ncfst+k) = indjx
              endif
            enddo
          endif
          if (ltmp(indjy)) then
            if (ncontot.ge.maxcontot) then
              maxcontot = ncontot + 10
              call changemaxcontot
            endif
!
!  Add new constraint putting y coordinate of i equal
!  to that of y
!
            if (ncf.lt.ncfg) then
              do k = ncontot,n1con(ncf+1),-1
                ncvarcfg(k+1) = ncvarcfg(k)
                ncfixcfg(k+1) = ncfixcfg(k)
                concocfg(k+1) = concocfg(k)
                nconcfg(k+1) = nconcfg(k)
                conaddcfg(k+1) = conaddcfg(k)
              enddo
              do k = ncf+1,ncfg
                n1con(k) = n1con(k) + 1
              enddo
            endif
            ncontot = ncontot + 1
            ltmp(indy) = .false.
            ncon = ncon + 1
            ncvarcfg(ncfst+ncon) = indjy
            ncfixcfg(ncfst+ncon) = indy
            concocfg(ncfst+ncon) = 1.0_dp
            conaddcfg(ncfst+ncon) = 0.0_dp
            nconcfg(ncfst+ncon) = ncf
!
!  Check to see whether any other coordinates of i are
!  dependent on the y coordinate of i
!
            do k = 1,ncon-1
              if (ncvarcfg(ncfst+k).eq.indy) then
                ncvarcfg(ncfst+k) = indjy
              endif
            enddo
          endif
          if (ltmp(indjz)) then
            if (ncontot.ge.maxcontot) then
              maxcontot = ncontot + 10
              call changemaxcontot
            endif
!
!  Add new constraint putting z coordinate of i equal
!  to that of z
!
            if (ncf.lt.ncfg) then
              do k = ncontot,n1con(ncf+1),-1
                ncvarcfg(k+1) = ncvarcfg(k)
                ncfixcfg(k+1) = ncfixcfg(k)
                concocfg(k+1) = concocfg(k)
                nconcfg(k+1) = nconcfg(k)
                conaddcfg(k+1) = conaddcfg(k)
              enddo
              do k = ncf+1,ncfg
                n1con(k) = n1con(k) + 1
              enddo
            endif
            ncontot = ncontot + 1
            ltmp(indz) = .false.
            ncon = ncon + 1
            ncvarcfg(ncfst+ncon) = indjz
            ncfixcfg(ncfst+ncon) = indz
            concocfg(ncfst+ncon) = 1.0_dp
            conaddcfg(ncfst+ncon) = 0.0_dp
            nconcfg(ncfst+ncon) = ncf
!
!  Check to see whether any other coordinates of i are
!  dependent on the z coordinate of i
!
            do k = 1,ncon-1
              if (ncvarcfg(ncfst+k).eq.indz) then
                ncvarcfg(ncfst+k) = indjz
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
