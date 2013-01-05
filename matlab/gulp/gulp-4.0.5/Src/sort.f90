  subroutine GULP_sort(imode)
!
!  Sort atoms into order specified by imode.
!
!  imode = 1 => collect cores first then shells
!  imode = 2 => collect qm atoms first then non-qm atoms
!  imode = 3 => collect slice atoms first 
!
!  11/96 Sorting of breathing shell constraints added
!  11/01 Actual reordering collected into common subroutine
!  11/01 Sorting of surface growth slice region 1 atoms added
!   6/05 Intent added
!   3/07 Name changed from sort to GULP_sort
!   3/07 Call to setup added to ensure that all data are set
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
  use control
  use configurations
  use current
  use defects
  use element, only : maxele
  use observables
  use projectdos
  implicit none
!
!  Passed arguments
!
  integer(i4), intent(in)                      :: imode
!
!  Local variables
!
  integer(i4), dimension(:), allocatable       :: iptr
  integer(i4)                                  :: i
  integer(i4)                                  :: nc
  integer(i4)                                  :: nco
  integer(i4)                                  :: ncore
  integer(i4)                                  :: nqm
  integer(i4)                                  :: ns
  integer(i4)                                  :: nslice
  integer(i4)                                  :: status
!
!  Loop over configurations
!
  nsft = 0
  do nc = 1,ncfg
    nasym = nascfg(nc)
!
!  Call setup for this configuration
!
    ncf = nc
    call setup(.false.)
!
!  Allocate local memory
!
    allocate(iptr(nasym),stat=status)
    if (status/=0) call outofmemory('GULP_sort','iptr')
!***********************************
!  Mode 1 : Sort cores and shells  *
!***********************************
    if (imode.eq.1) then
      ncore = 0
!
!  Find number of each species type
!
      do i = 1,nasym
        if (natcfg(nsft+i).le.maxele) then
          ncore = ncore + 1
        endif
      enddo
!
!  Create pointer to old positions
!
      nco = 0
      ns = ncore
      do i = 1,nasym
        if (natcfg(nsft+i).le.maxele) then
          nco = nco + 1
          iptr(nco) = i
        else
          ns = ns + 1
          iptr(ns) = i
        endif
      enddo
!
!  Do reordering
!
      call reorder(nc,iptr)
    elseif (imode.eq.2) then
!******************
!  Sort QM atoms  *
!******************
      nqm = 0
!
!  Find number of each species type
!
      nasym = nascfg(nc)
      do i = 1,nasym
        if (lqmatom(nsft+i)) then
          nqm = nqm + 1
        endif
      enddo
!
!  Create pointer to old positions
!
      nco = 0
      ns = nqm
      do i = 1,nasym
        if (lqmatom(nsft+i)) then
          nco = nco + 1
          iptr(nco) = i
        else
          ns = ns + 1
          iptr(ns) = i
        endif
      enddo
!
!  Do reordering
!
      call reorder(nc,iptr)
    elseif (imode.eq.3) then
!****************************
!  Sort growth slice atoms  *
!****************************
!
!  Check that this is a surface otherwise there is nothing to do
!
      if (ndimen(nc).eq.2) then
        nslice = 0
!
!  Find number of each species type
!
        nasym = nascfg(nc)
        do i = 1,nasym
          if (lsliceatom(nsft+i)) then
            nslice = nslice + 1
          endif
        enddo
!
!  Create pointer to old positions
!
        nco = 0
        ns = nslice
        do i = 1,nasym
          if (lsliceatom(nsft+i)) then
            nco = nco + 1
            iptr(nco) = i
          else
            ns = ns + 1
            iptr(ns) = i
          endif
        enddo
!
!  Do reordering
!
        call reorder(nc,iptr)
      endif
    endif
    nsft = nsft + nasym
!
!  Free local memory
!
    deallocate(iptr,stat=status)
    if (status/=0) call deallocate_error('GULP_sort','iptr')
  enddo
!
  return
  end
