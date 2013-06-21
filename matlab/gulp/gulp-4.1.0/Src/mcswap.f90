  subroutine mcswap(mode)
!
!  MC routine for swapping of atoms. Approach taken is to swap
!  coordinates rather than attributes since this is simpler.
!
!  mode = if mode = 1, choose atoms to apply swap to
!         if mode = 2, then create new trial swap
!         if mode = 3, then undo previous swap
!
!   1/09 Created from mcmove
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
!  Julian Gale, NRI, Curtin University, January 2009
!
  use current
  use general
  use genetic, only : iseed
  use montecarlo
  use parallel
  use reallocate
!
!  Passed variables
!
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                  :: mode
!
!  Local variables
!
  integer(i4),                        save :: nptrswap1
  integer(i4),                        save :: nptrswap2
  integer(i4),                        save :: nswap1
  integer(i4),                        save :: nswap2
!
  integer(i4), dimension(:), pointer, save :: nptrswapable2 => null()
  integer(i4)                              :: i
  integer(i4)                              :: ii
  integer(i4)                              :: nat1
  integer(i4)                              :: nswapable2
  integer(i4)                              :: ntype1
  integer(i4)                              :: status
  logical                                  :: lmatch
  real(dp)                                 :: randnum
  real(dp)                                 :: GULP_random
  real(dp)                                 :: x1
  real(dp)                                 :: y1
  real(dp)                                 :: z1
  real(dp)                                 :: x2
  real(dp)                                 :: y2
  real(dp)                                 :: z2
!
  if (mode.eq.3) then
!****************************
!  Mode 3 : Undo last swap  *
!****************************
    x1 = x0(3*nptrswap1+nstrains-2)
    y1 = x0(3*nptrswap1+nstrains-1)
    z1 = x0(3*nptrswap1+nstrains)  
    x2 = x0(3*nptrswap2+nstrains-2)
    y2 = x0(3*nptrswap2+nstrains-1)
    z2 = x0(3*nptrswap2+nstrains)  
!
    x0(3*nptrswap1+nstrains-2) = x2
    x0(3*nptrswap1+nstrains-1) = y2
    x0(3*nptrswap1+nstrains)   = z2
    x0(3*nptrswap2+nstrains-2) = x1
    x0(3*nptrswap2+nstrains-1) = y1
    x0(3*nptrswap2+nstrains)   = z1
  elseif (mode.eq.1) then
!**********************
!  Mode 1 : New swap  *
!**********************
!
!  Allocate array to store details of second atom choice
!
    allocate(nptrswapable2(nswapable),stat=status)
    if (status/=0) call outofmemory('mcswap','nptrswapable2')
!
!  Choose first atom to swap
!
    randnum = GULP_random(iseed,1_i4)
    nswap1 = nswapable*randnum + 1_i4
    if (nswap1.gt.nswapable) nswap1 = nswapable
    nptrswap1 = nptrswapable(nswap1)
    nat1 = nat(nptrswap1)
    ntype1 = nftype(nptrswap1)
!
!  Build list of atoms to swap with that are different from atom 1
!
    nswapable2 = 0
    do i = 1,nswapable
      ii = nptrswapable(i)
      if (.not.lmatch(nat1,ntype1,nat(ii),nftype(ii),.false.)) then
        nswapable2 = nswapable2 + 1
        nptrswapable2(nswapable2) = i
      endif
    enddo
!
!  Check that there are some swaps possible
!
    if (nswapable2.eq.0) then
      call outerror('swap requested but no meaningful swap possible',0_i4)
      call stopnow('mcswap')
    endif
!
!  Choose second atom to swap
!
    randnum = GULP_random(iseed,1_i4)
    nswap2 = nswapable2*randnum + 1_i4
    if (nswap2.gt.nswapable2) nswap2 = nswapable2
!
!  Translate choice from second list to first list
!
    nswap2 = nptrswapable2(nswap2)
    nptrswap2 = nptrswapable(nswap2)
!
!  Deallocate array used to  store details of second atom choice
!
    deallocate(nptrswapable2,stat=status)
    if (status/=0) call deallocate_error('mcswap','nptrswapable2')
!
!  Copy pointers to main arrays
!
    ntrialatom = 2
    nptrtrialatom(1) = nptrswap1
    nptrtrialatom(2) = nptrswap2
  elseif (mode.eq.2) then
!************************
!  Mode 2 : Apply swap  *
!************************
!
!  Apply swap to configuration array
!
    x1 = x0(3*nptrswap1+nstrains-2)
    y1 = x0(3*nptrswap1+nstrains-1)
    z1 = x0(3*nptrswap1+nstrains)  
    x2 = x0(3*nptrswap2+nstrains-2)
    y2 = x0(3*nptrswap2+nstrains-1)
    z2 = x0(3*nptrswap2+nstrains)  
!
    x0(3*nptrswap1+nstrains-2) = x2
    x0(3*nptrswap1+nstrains-1) = y2
    x0(3*nptrswap1+nstrains)   = z2
    x0(3*nptrswap2+nstrains-2) = x1
    x0(3*nptrswap2+nstrains-1) = y1
    x0(3*nptrswap2+nstrains)   = z1
  endif
!
  return
  end
