  subroutine dynamicn_defect
!
!  Subroutine for calculating the dynamical matrix by finite differences
!  for Mott-Littleton calculations.
!
!   7/12 Created from dynamicn.f90
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
!  Julian Gale, NRI, Curtin University, July 2012
!
  use configurations
  use constants
  use control
  use current
  use defects,       only : nreg1, xdefe, ydefe, zdefe, ldbsm
  use derivatives
  use general,       only : phondiff
  use optimisation
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ind
  integer(i4)                                    :: j
  integer(i4)                                    :: m
  integer(i4)                                    :: matom
  integer(i4)                                    :: maxlim
  integer(i4)                                    :: mcrd
  integer(i4)                                    :: mint
  integer(i4)                                    :: mm
  integer(i4)                                    :: status
  logical                                        :: lforward
  logical                                        :: lradial
  logical                                        :: ld1symsave
  logical                                        :: ld2symsave
  real(dp)                                       :: fc
  real(dp)                                       :: rstep
  real(dp)                                       :: step
  real(dp),    dimension(:),   allocatable, save :: rdrvin
  real(dp),    dimension(:),   allocatable, save :: xdrvin
  real(dp),    dimension(:),   allocatable, save :: ydrvin
  real(dp),    dimension(:),   allocatable, save :: zdrvin
!*****************************
!  Save initial derivatives  *
!*****************************
  allocate(rdrvin(nreg1),stat=status)
  if (status/=0) call outofmemory('dynamicn','rdrvin')
  allocate(xdrvin(nreg1),stat=status)
  if (status/=0) call outofmemory('dynamicn','xdrvin')
  allocate(ydrvin(nreg1),stat=status)
  if (status/=0) call outofmemory('dynamicn','ydrvin')
  allocate(zdrvin(nreg1),stat=status)
  if (status/=0) call outofmemory('dynamicn','zdrvin')
  rdrvin(1:nreg1) = raderv(1:nreg1)
  xdrvin(1:nreg1) = xdrv(1:nreg1)
  ydrvin(1:nreg1) = ydrv(1:nreg1)
  zdrvin(1:nreg1) = zdrv(1:nreg1)
!
!  Save symmetry flags
!
  ld1symsave = ld1sym
  ld2symsave = ld2sym
!
!  Turn off symmetry
!
  ld1sym = .false.
  ld2sym = .false.
!****************************
!  Zero second derivatives  *
!****************************
  mint = 3*nreg1
  maxlim = mint
  if (ldbsm) maxlim = maxlim + nreg1
!
  if (maxlim.gt.maxd2u) then
    maxd2u = maxlim
    call changemaxd2
  endif
  if (maxlim.gt.maxd2) then
    maxd2 = maxlim
    call changemaxd2
  endif
  do i = 1,maxlim
    do j = 1,maxlim
      derv2(j,i) = 0.0_dp
    enddo
  enddo
!
  rstep = 0.5_dp/phondiff
!************************************************
!  Loop over coordinates for finite difference  *
!************************************************
  do m = 1,2*maxlim
!
!  Find the degree of freedom and whether this is the forward or backward step
!
    if (m.gt.maxlim) then
      lforward = .true.
      mm = m - maxlim
    else
      lforward = .false.
      mm = m
    endif
    if (mm.gt.mint) then
      lradial = .true.
      matom = mm
    else
      lradial = .false.
      matom = (mm-1)/3 + 1
      mcrd  = mm - 3*(matom - 1)
    endif
!
!  Select step
!
    if (lforward) then
      step = phondiff
    else
      step = - phondiff
    endif
!
!  Alter coordinate
!
    if (lradial) then
      rada(matom) = rada(matom) + step
      radf(matom) = radf(matom) + step
    else
      if (mcrd.eq.1) then
        xdefe(matom) = xdefe(matom) + step
      elseif (mcrd.eq.2) then
        ydefe(matom) = ydefe(matom) + step
      elseif (mcrd.eq.3) then
        zdefe(matom) = zdefe(matom) + step
      endif
    endif
!
!  Evaluate function and first derivatives
!
    call defener(fc,.true.,.false.)
!
!  Add gradients to dynamical array
!
    if (lforward) then
      ind = 0
      do i = 1,nreg1
        derv2(ind+1,mm) = derv2(ind+1,mm) + rstep*xdrv(i)
        derv2(ind+2,mm) = derv2(ind+2,mm) + rstep*ydrv(i)
        derv2(ind+3,mm) = derv2(ind+3,mm) + rstep*zdrv(i)
        ind = ind + 3
      enddo
      if (ldbsm) then
        do i = 1,nreg1
          derv2(ind+i,mm) = derv2(ind+i,mm) + rstep*raderv(i)
        enddo
      endif
    else
      ind = 0
      do i = 1,nreg1
        derv2(ind+1,mm) = derv2(ind+1,mm) - rstep*xdrv(i)
        derv2(ind+2,mm) = derv2(ind+2,mm) - rstep*ydrv(i)
        derv2(ind+3,mm) = derv2(ind+3,mm) - rstep*zdrv(i)
        ind = ind + 3
      enddo
      if (ldbsm) then
        do i = 1,nreg1
          derv2(ind+i,mm) = derv2(ind+i,mm) - rstep*raderv(i)
        enddo
      endif
    endif
!  
!  Reverse step
!
    if (lradial) then
      rada(matom) = rada(matom) - step
      radf(matom) = radf(matom) - step
    else
      if (mcrd.eq.1) then
        xdefe(matom) = xdefe(matom) - step
      elseif (mcrd.eq.2) then
        ydefe(matom) = ydefe(matom) - step
      elseif (mcrd.eq.3) then
        zdefe(matom) = zdefe(matom) - step
      endif
    endif
!*************************************
!  End loop over finite differences  *
!*************************************
  enddo
!
!  Symmetrise second derivative matrix
!
  do i = 1,maxlim
    do j = 1,i-1
      derv2(j,i) = 0.5_dp*(derv2(j,i) + derv2(i,j))
      derv2(i,j) = derv2(j,i)
    enddo
  enddo
!
!  Reset symmetry flags
!
  ld1sym = ld1symsave
  ld2sym = ld2symsave
!**********************************
!  Copy back initial derivatives  *
!**********************************
  raderv(1:nreg1) = rdrvin(1:nreg1)
  xdrv(1:nreg1) = xdrvin(1:nreg1)
  ydrv(1:nreg1) = ydrvin(1:nreg1)
  zdrv(1:nreg1) = zdrvin(1:nreg1)
  deallocate(zdrvin,stat=status)
  if (status/=0) call deallocate_error('dynamicn','zdrvin')
  deallocate(ydrvin,stat=status)
  if (status/=0) call deallocate_error('dynamicn','ydrvin')
  deallocate(xdrvin,stat=status)
  if (status/=0) call deallocate_error('dynamicn','xdrvin')
  deallocate(rdrvin,stat=status)
  if (status/=0) call deallocate_error('dynamicn','rdrvin')
!
  return
  end
