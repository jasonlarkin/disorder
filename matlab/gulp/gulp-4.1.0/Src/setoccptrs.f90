  subroutine setoccptrs(ncfoc,nsfoc,nbfoc,iocptr,ibocptr,lregion1only)
!
!  Sets up pointers to partial occupany sites. Spatial decomposition version.
!
!  ncfoc   = number of fully occupied sites for cores
!  nsfoc   = number of fully occupied sites for shells
!  nbfoc   = number of fully occupied sites for breathing
!  iocptr  = points from total number of atoms to
!            location in reduced set for phonons
!  ibocptr = pointer to reduced breathing sites
!  lregion1only = if true, only consider region 1 atoms
!
!   7/11 Created from setoccptr.
!  10/12 Option added to allow two atoms co-exist on a site with an
!        occupancy greater than one.
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
!  Julian Gale, NRI, Curtin University, October 2012
!
  use configurations, only : lbsmat, nregionno
  use control,        only : lallowgt1
  use current
  use element,        only : maxele
  use spatial 
  implicit none
!
!  Passed variables
!
  integer(i4),                    intent(out)  :: ibocptr(*)
  integer(i4),                    intent(out)  :: iocptr(*)
  integer(i4),                    intent(out)  :: nbfoc
  integer(i4),                    intent(out)  :: ncfoc
  integer(i4),                    intent(out)  :: nsfoc
  logical,                        intent(in)   :: lregion1only
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
  integer(i4)                                  :: nati
  integer(i4)                                  :: ni
  integer(i4)                                  :: status
  logical                                      :: lbsmi
  logical                                      :: lbsmj
  logical                                      :: lcorei
  logical                                      :: lcorej
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
  if (status/=0) call outofmemory('setoccptrs','iiptr')
!************************************
!  Setup partial occupancy pointer  *
!************************************
  ncfoc = 0
  nsfoc = 0
  nbfoc = 0
  ii = 0
  do i = 1,numat
    if (.not.lregion1only.or.nregionno(nsft+nrelat(i)).eq.1) then
      ii = ii + 1
      iiptr(i) = ii
      ibocptr(ii) = 0
      if (occuf(i).eq.1.0_dp.and..not.lallowgt1) then
!
!  Fully occupied site
!
        if (nat(i).gt.maxele) then
          nsfoc = nsfoc + 1
        else
          ncfoc = ncfoc + 1
        endif
        iocptr(ii) = ncfoc + nsfoc
        if (lbsmat(nsft+nrelat(i))) then
          nbfoc = nbfoc + 1
          ibocptr(ii) = nbfoc
        endif
      else
!
!  Partially occupied site
!  Check to see if there is a previous atom on this site
!
        xi = xinbox(i)
        yi = yinbox(i)
        zi = zinbox(i)
        nati = nat(i)
        lcorei = (nati.le.maxele)
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
          if (j.lt.i) then
            if (.not.lregion1only.or.nregionno(nsft+nrelat(j)).eq.1) then
              jj = iiptr(j)
              lcorej = (nat(j).le.maxele)
              if ((lcorei.and.lcorej).or.(.not.lcorei.and..not.lcorej)) then
                xd = xinbox(j) - xi
                yd = yinbox(j) - yi
                zd = zinbox(j) - zi
                rsum = abs(xd) + abs(yd) + abs(zd)
                if (rsum.lt.1.0d-4) then
                  lfound = .true.
                  iocptr(ii) = iocptr(jj)
                  lbsmj = (lbsmat(nsft+nrelat(j)))
                  if (lbsmi) then
                    if (lbsmj) then
                      ibocptr(ii) = ibocptr(jj)
                    else
                      nbfoc = nbfoc + 1
                      ibocptr(ii) = nbfoc
                    endif
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
          if (lcorei) then
            ncfoc = ncfoc + 1
          else
            nsfoc = nsfoc + 1
          endif
          iocptr(ii) = ncfoc + nsfoc
          if (lbsmat(nsft+nrelat(i))) then
            nbfoc = nbfoc + 1
            ibocptr(ii) = nbfoc
          endif
        endif
      endif
    endif
  enddo
!
!  Free local memory
!
  deallocate(iiptr,stat=status)
  if (status/=0) call deallocate_error('setoccptrs','iiptr')
!
  return
  end
