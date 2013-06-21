  subroutine setc6
!
!  Setup tasks for C/r**6 sum
!
!  lc6    = if .true. then Ewald sum for 1/r**6 to be used
!  lc6one = if .true. then C terms are separable into one
!           centre parameters
!  c6     = one centre parameters for C terms
!
!   8/95 created
!   9/95 fix added as lc6one was being set to true when not valid
!   4/98 ESFF Lennard-Jones form now allowed for
!   6/06 Allocation of lokpot brought forward to avoid error
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
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
  use control
  use current
  use species
  use two
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: n
  integer(i4)                                  :: nati
  integer(i4)                                  :: natj
  integer(i4)                                  :: nc6
  integer(i4)                                  :: nlink6
  integer(i4)                                  :: nself6
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: status
  logical,     dimension(:), allocatable       :: lokpot
  real(dp)                                     :: c6pair
  real(dp)                                     :: c6trm1
  real(dp)                                     :: c6trm2
  real(dp)                                     :: diff
  real(dp)                                     :: pdiff
!*************************
!  Find r**6 potentials  *
!*************************
  allocate(lokpot(npote),stat=status)
  if (status/=0) call outofmemory('setc6','lokpot')
  nc6 = 0
  do i = 1,npote
    if (nptype(i).eq.1.or.nptype(i).eq.7) then
      if (abs(twopot(3,i)).gt.0.0_dp) then
        nc6 = nc6 + 1
      endif
    elseif ((nptype(i).eq.2.or.nptype(i).eq.21).and.nint(tpot(2,i)).eq.6) then
      if (abs(twopot(2,i)).gt.0.0_dp) then
        nc6 = nc6 + 1
      endif
    endif
  enddo
!
!  If no potentials then return
!
  if (nc6.eq.0) then
    lc6 = .false.
    lc6one = .false.
    goto 10
  endif
!*******************************************************
!  Attempt to reduce coefficients to one-centre terms  *
!*******************************************************
  do i = 1,nspec
    c6spec(i) = 0.0_dp
  enddo
  do i = 1,npote
    lokpot(i) = .false.
  enddo
!************************
!  Look for self terms  *
!************************
  nself6 = 0
  do i = 1,npote
    if (nspec1(i).eq.nspec2(i).and.nptyp1(i).eq.nptyp2(i)) then
      lokpot(i) = .true.
      nself6 = nself6 + 1
      do j = 1,nspec
        if (nspec1(i).eq.natspec(j).and.(nptyp1(i).eq.ntypspec(j).or.nptyp1(i).eq.0)) then
          if (nptype(i).eq.2.or.nptype(i).eq.21) then
            c6spec(j) = sqrt(twopot(2,i))
          else
            c6spec(j) = sqrt(twopot(3,i))
          endif
        endif
      enddo
    endif
  enddo
!
!  If nself6 = npote then all potentials have been found
!
  if (npote.eq.nself6) then
    lc6one = .true.
    goto 10
  endif
!********************************************
!  If self potentials have been found look  *
!  for potentials which are linked to them  *
!********************************************
  if (nself6.gt.0) then
    nlink6 = 0
    do i=1,npote
      if (.not.lokpot(i)) then
        c6trm1 = 0.0_dp
        c6trm2 = 0.0_dp
        do j = 1,nspec
          if (nspec1(i).eq.natspec(j).and.(nptyp1(i).eq.ntypspec(j).or.nptyp1(i).eq.0)) then
            c6trm1 = c6spec(j)
          endif
          if (nspec2(i).eq.natspec(j).and.(nptyp2(i).eq.ntypspec(j).or.nptyp2(i).eq.0)) then
            c6trm2 = c6spec(j)
          endif
        enddo
        if (c6trm1.eq.0.0.and.c6trm2.ne.0.0) then
          nlink6 = nlink6 + 1
          if (nptype(i).eq.2.or.nptype(i).eq.21) then
            c6trm1 = twopot(2,i)/c6trm2
          elseif (nptype(i).eq.1.or.nptype(i).eq.7) then
            c6trm1 = twopot(3,i)/c6trm2
          endif
          do j = 1,nspec
            if (nspec1(i).eq.natspec(j).and.(nptyp1(i).eq.ntypspec(j).or.nptyp1(i).eq.0)) then
              c6spec(j) = c6trm1
            endif
          enddo
        elseif (c6trm2.eq.0.0.and.c6trm1.ne.0.0) then
          nlink6 = nlink6 + 1
          if (nptype(i).eq.2.or.nptype(i).eq.21) then
            c6trm2 = twopot(2,i)/c6trm1
          elseif (nptype(i).eq.1.or.nptype(i).eq.7) then
            c6trm2 = twopot(3,i)/c6trm1
          endif
          do j = 1,nspec
            if (nspec2(i).eq.natspec(j).and.(nptyp2(i).eq.ntypspec(j).or.nptyp2(i).eq.0)) then
              c6spec(j) = c6trm2
            endif
          enddo
        elseif (c6trm1.ne.0.0.and.c6trm2.ne.0.0) then
          nlink6 = nlink6 + 1
          if (nptype(i).eq.2.or.nptype(i).eq.21) then
            pdiff = twopot(2,i) - c6trm1*c6trm2
          elseif (nptype(i).eq.1.or.nptype(i).eq.7) then
            pdiff = twopot(3,i) - c6trm1*c6trm2
          else
            pdiff = 0.0_dp
          endif
          if (abs(pdiff).gt.1.0d-4) then
            lc6one = .false.
            return
          endif
        endif
      endif
    enddo
!
!  If nself6 + nlink6 = nc6 then all potentials have been found
!
    if (npote.eq.(nself6+nlink6)) then
      lc6one = .true.
      goto 10
    endif
  endif
!
!  If no self terms look for loops of related potentials
!
!
!  For the moment just give up!
!
  lc6one = .false.
  deallocate(lokpot,stat=status)
  if (status/=0) call deallocate_error('setc6','lokpot')
  return
!
!  Final check to make sure correct algorithm is used
!
10 do i = 1,nspec
    nati = natspec(i)
    ntypi = ntypspec(i)
    do j = 1,nspec
      natj = natspec(j)
      ntypj = ntypspec(j)
!
!  Find potential between species to get C term - if
!  no potential then C term must be equal to zero.
!
      c6pair = 0.0_dp
      do n = 1,npote
        if (nspec1(n).eq.nati.and.nspec2(n).eq.natj.and. &
            (nptyp1(n).eq.ntypi.or.nptyp1(n).eq.0).and. &
            (nptyp2(n).eq.ntypj.or.nptyp2(n).eq.0)) then
          if (nptype(n).eq.1.or.nptype(n).eq.7) then
            c6pair = c6pair + twopot(3,n)
          elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
            c6pair = c6pair + twopot(2,n)
          endif
        elseif (nspec2(n).eq.nati.and.nspec1(n).eq.natj.and. &
            (nptyp2(n).eq.ntypi.or.nptyp2(n).eq.0).and. &
            (nptyp1(n).eq.ntypj.or.nptyp1(n).eq.0)) then
          if (nptype(n).eq.1.or.nptype(n).eq.7) then
            c6pair = c6pair + twopot(3,n)
          elseif (nptype(n).eq.2.or.nptype(n).eq.21) then
            c6pair = c6pair + twopot(2,n)
          endif
        endif
      enddo
      diff = c6pair - c6spec(i)*c6spec(j)
      if (abs(diff).gt.1.0d-4) then
        lc6one = .false.
        return
      endif
    enddo
  enddo
  deallocate(lokpot,stat=status)
  if (status/=0) call deallocate_error('setc6','lokpot')
!
  if (index(keyword,'offc61').ne.0) lc6one = .false.
!
  return
  end
