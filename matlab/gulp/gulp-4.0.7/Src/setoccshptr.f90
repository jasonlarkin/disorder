  subroutine setoccshptr(nsfoc,nbsfoc,iocshptr,ibocshptr,lregion1only)
!
!  Sets up pointers to partial occupany sites for shells only
!
!  nsfoc     = number of fully occupied sites for shells
!  nbsfoc    = number of fully occupied sites for breathing shells
!  iocshptr  = points from total number of atoms to
!              location in reduced set for phonons
!  ibocshptr = pointer to reduced breathing sites
!  lregion1only = if true, only consider region 1 atoms
!
!   3/03 Created from setoccptr
!  12/07 Unused variables removed
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
  use configurations, only : lbsmat, nregionno
  use current
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  integer(i4) :: ibocshptr(*)
  integer(i4) :: iocshptr(*)
  integer(i4) :: nbsfoc
  integer(i4) :: nsfoc
  logical     :: lregion1only
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: j
  integer(i4) :: jj
  logical     :: lbsmi
  logical     :: lbsmj
  logical     :: lfound
  real(dp)    :: rsum
  real(dp)    :: xd
  real(dp)    :: yd
  real(dp)    :: zd
  real(dp)    :: xi
  real(dp)    :: yi
  real(dp)    :: zi
!************************************
!  Setup partial occupancy pointer  *
!************************************
  nsfoc = 0
  nbsfoc = 0
  ii = 0
  do i = 1,numat
    if (nat(i).gt.maxele) then
      if (.not.lregion1only.or.nregionno(nsft+nrelat(i)).eq.1) then
        ii = ii + 1
        ibocshptr(ii) = 0
        if (occuf(i).eq.1.0_dp) then
!
!  Fully occupied site
!
          nsfoc = nsfoc + 1
          iocshptr(ii) = nsfoc
          if (lbsmat(nsft+nrelat(i))) then
            nbsfoc = nbsfoc + 1
            ibocshptr(ii) = nbsfoc
          endif
        else
!
!  Partially occupied site
!  Check to see if there is a previous atom on this site
!
          xi = xclat(i)
          yi = yclat(i)
          zi = zclat(i)
          lbsmi = (lbsmat(nsft+nrelat(i)))
          lfound = .false.
          j = 1
          jj = 0
          do while (j.lt.i.and..not.lfound)
            if (nat(j).gt.maxele) then
              if (.not.lregion1only.or.nregionno(nsft+nrelat(j)).eq.1) then
                jj = jj + 1
                xd = xclat(j) - xi
                yd = yclat(j) - yi
                zd = zclat(j) - zi
                rsum = abs(xd)+abs(yd)+abs(zd)
                if (rsum.lt.1.0d-4) then
                  lfound = .true.
                  iocshptr(ii) = iocshptr(jj)
                  lbsmj = (lbsmat(nsft+nrelat(j)))
                  if (lbsmi) then
                    if (lbsmj) then
                      ibocshptr(ii) = ibocshptr(jj)
                    else
                      nbsfoc = nbsfoc + 1
                      ibocshptr(ii) = nbsfoc
                    endif
                  endif
                endif
              endif
            endif
            j = j + 1
          enddo
          if (.not.lfound) then
!
!  Must be new site
!
            nsfoc = nsfoc + 1
            iocshptr(ii) = nsfoc
            if (lbsmat(nsft+nrelat(i))) then
              nbsfoc = nbsfoc + 1
              ibocshptr(ii) = nbsfoc
            endif
          endif
        endif
      endif
    endif
  enddo
!
  return
  end
