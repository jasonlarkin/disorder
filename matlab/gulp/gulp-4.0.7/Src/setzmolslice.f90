  subroutine setzmolslice
!
!  Calculate number of molecules within the slice needed to divide the 
!  attachment energy.
!
!  11/01 Created
!   2/02 New algorithm introduced based on formula units
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
  use configurations
  use current
  use molecule
  implicit none
!
!  Local arrays
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ni
  integer(i4)                                  :: ninc
  integer(i4)                                  :: nmin
  integer(i4)                                  :: ntypes
  integer(i4), dimension(:), allocatable       :: inslice
  integer(i4), dimension(:), allocatable       :: nnat
  integer(i4), dimension(:), allocatable       :: numnat
  integer(i4)                                  :: status
  logical                                      :: lok
  logical                                      :: lsame
  logical,     dimension(:), allocatable       :: linslice
!
  if (nmol.gt.0) then
!***************************************
!  Algorithm based on slice molecules  *
!***************************************
!
!  Allocate local memory
!
    allocate(linslice(nmol),stat=status)
    if (status/=0) call outofmemory('setzmolslice','linslice')
!
!  Loop over slice atoms to see if molecule is in slice
!
    linslice(1:nmol) = .false.
    do i = 1,numat
      if (lsliceatom(nsft + i)) then
        if (natmol(i).gt.0) then
          ni = natmol(i)
          linslice(ni) = .true.
        endif
      endif
    enddo
!  
!  Count number of molecules 
!  
    nzmol = 0_i4
    do i = 1,nmol
      if (linslice(i)) then
        nzmol = nzmol + 1_i4
      endif
    enddo
    nzmol = max(nzmol,1_i4)
!
!  Free local memory
!
    deallocate(linslice,stat=status)
    if (status/=0) call deallocate_error('setzmolslice','linslice')
  else
!***************************************
!  Algorithm based on all slice atoms  *
!***************************************
!
!  Allocate local memory
!
    allocate(inslice(numat),stat=status)
    if (status/=0) call outofmemory('setzmolslice','inslice')
    ninc = 0
    do i = 1,numat
      if (lsliceatom(nsft + i)) then
        ninc = ninc + 1
        inslice(ninc) = i
      endif
    enddo
!
!  Allocate further local memory
!
    allocate(nnat(ninc),stat=status)
    if (status/=0) call outofmemory('setzmolslice','nnat')
    allocate(numnat(ninc),stat=status)
    if (status/=0) call outofmemory('setzmolslice','numnat')
!
!  Count number of formula units in group of atoms that count
!
    ntypes = 0_i4
    do i = 1,ninc
      ni = 0
      lsame = .false.
      do while (.not.lsame.and.ni.lt.ntypes)
        ni = ni + 1
        lsame = (nat(inslice(i)).eq.nnat(ni))
      enddo
      if (.not.lsame) then
        ntypes = ntypes + 1
        nnat(ntypes) = nat(inslice(i))
        numnat(ntypes) = 1
      else
        numnat(ni) = numnat(ni) + 1
      endif
    enddo
!
!  Find largest factor of atom numbers
!
    nmin = numat
    do i = 1,ntypes
      nmin = min(nmin,numnat(i))
    enddo
    nzmol = 1
    do i = 2,nmin
      lok = .true.
      ni = 0
      do while (lok.and.ni.lt.ntypes)
        ni = ni + 1
        lok = (mod(numnat(ni),i).eq.0)
      enddo
      if (lok) nzmol = i
    enddo
!
!  Free local memory
!
    deallocate(numnat,stat=status)
    if (status/=0) call deallocate_error('setzmolslice','numnat')
    deallocate(nnat,stat=status)
    if (status/=0) call deallocate_error('setzmolslice','nnat')
    deallocate(inslice,stat=status)
    if (status/=0) call deallocate_error('setzmolslice','inslice')
  endif
!
  nzmolcfg(ncf) = nzmol
!
  return
  end
