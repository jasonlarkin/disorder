  subroutine setoccshptrs(nsfoc,nbsfoc,iocshptr,ibocshptr,lregion1only)
!
!  Sets up pointers to partial occupany sites for shells only. Spatial
!  decomposition version.
!
!  nsfoc     = number of fully occupied sites for shells
!  nbsfoc    = number of fully occupied sites for breathing shells
!  iocshptr  = points from total number of atoms to
!              location in reduced set for phonons
!  ibocshptr = pointer to reduced breathing sites
!  lregion1only = if true, only consider region 1 atoms
!
!   7/11 Created from setoccshptr and setoccptrs
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
!  Julian Gale, NRI, Curtin University, July 2011
!
  use configurations, only : lbsmat, nregionno
  use current
  use element, only : maxele
  use spatial 
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
  integer(i4), dimension(:), allocatable       :: iiptr
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: ixyz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: ni
  integer(i4)                                  :: status
  logical                                      :: lbsmi
  logical                                      :: lbsmj
  logical                                      :: lfound
  real(dp)                                     :: rsum
  real(dp)                                     :: xd
  real(dp)                                     :: yd
  real(dp)                                     :: zd
  real(dp)                                     :: xi
  real(dp)                                     :: yi
  real(dp)                                     :: zi
!
!  Allocate local memory
!
  allocate(iiptr(numat),stat=status)
  if (status/=0) call outofmemory('setoccshptrs','iiptr')
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
        iiptr(i) = ii
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
          xi = xinbox(i)
          yi = yinbox(i)
          zi = zinbox(i)
          lbsmi = (lbsmat(nsft+nrelat(i)))
          lfound = .false.
!
!  Translate atom to spatial decomposition cell
!
          ixyz = nspcell2atptr(i)
!
!  Loop over atoms in spatial cell
!
          ind = nspcellat1ptr(ixyz)
          ni = 0
          do while (ni.lt.nspcellat(ixyz).and..not.lfound)
            ni = ni + 1
            j = nspcellatptr(ind+ni)
            if (j.lt.i.and.nat(j).gt.maxele) then
              if (.not.lregion1only.or.nregionno(nsft+nrelat(j)).eq.1) then
                jj = iiptr(j)
                xd = xinbox(j) - xi
                yd = yinbox(j) - yi
                zd = zinbox(j) - zi
                rsum = abs(xd) + abs(yd) + abs(zd)
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
!  Free local memory
!
  deallocate(iiptr,stat=status)
  if (status/=0) call deallocate_error('setoccshptrs','iiptr')
!
  return
  end
