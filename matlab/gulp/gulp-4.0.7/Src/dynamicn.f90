  subroutine dynamicn
!
!  Subroutine for calculating the dynamical matrix by finite differences
!
!   4/08 Created from energy.f90
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   4/09 Separate call to generate MEAM densities added
!   6/09 Module name changed from three to m_three
!  11/10 Modified to use call to energy or evb to generalise code
!  11/11 Second derivative matrix explicitly symmetrised and translational
!        invariance imposed
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
!  Julian Gale, NRI, Curtin University, November 2011
!
  use cellmultipole
  use configurations
  use constants
  use control
  use current
  use derivatives
  use four
  use general,       only : phondiff
  use iochannels
  use kspace
  use molecule
  use optimisation
  use parallel
  use polarise
  use potentialxyz
  use six
  use sutton
  use m_three
  use two
  implicit none
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ind
  integer(i4)                                    :: indi
  integer(i4)                                    :: indj
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
  allocate(rdrvin(numat),stat=status)
  if (status/=0) call outofmemory('dynamicn','rdrvin')
  allocate(xdrvin(numat),stat=status)
  if (status/=0) call outofmemory('dynamicn','xdrvin')
  allocate(ydrvin(numat),stat=status)
  if (status/=0) call outofmemory('dynamicn','ydrvin')
  allocate(zdrvin(numat),stat=status)
  if (status/=0) call outofmemory('dynamicn','zdrvin')
  rdrvin(1:numat) = raderv(1:numat)
  xdrvin(1:numat) = xdrv(1:numat)
  ydrvin(1:numat) = ydrv(1:numat)
  zdrvin(1:numat) = zdrv(1:numat)
!****************************
!  Zero second derivatives  *
!****************************
  mint  = 3*numat
  maxlim = mint
  if (nbsmat.gt.0) maxlim = maxlim + numat
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
  if (lewald.and.ndim.gt.1) then
!
!  Set up reciprocal space terms
!
    call kindex
  endif
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
        xalat(matom) = xalat(matom) + step
        xclat(matom) = xclat(matom) + step
      elseif (mcrd.eq.2) then
        yalat(matom) = yalat(matom) + step
        yclat(matom) = yclat(matom) + step
      elseif (mcrd.eq.3) then
        zalat(matom) = zalat(matom) + step
        zclat(matom) = zclat(matom) + step
      endif
    endif
!
!  Evaluate function and first derivatives
!
    call energy(fc,.true.,.false.)
!
!  Add gradients to dynamical array
!
    if (lforward) then
      ind = 0
      do i = 1,numat
        derv2(ind+1,mm) = derv2(ind+1,mm) + rstep*xdrv(i)
        derv2(ind+2,mm) = derv2(ind+2,mm) + rstep*ydrv(i)
        derv2(ind+3,mm) = derv2(ind+3,mm) + rstep*zdrv(i)
        ind = ind + 3
      enddo
      if (nbsmat.gt.0) then
        do i = 1,numat
          derv2(ind+i,mm) = derv2(ind+i,mm) + rstep*raderv(i)
        enddo
      endif
    else
      ind = 0
      do i = 1,numat
        derv2(ind+1,mm) = derv2(ind+1,mm) - rstep*xdrv(i)
        derv2(ind+2,mm) = derv2(ind+2,mm) - rstep*ydrv(i)
        derv2(ind+3,mm) = derv2(ind+3,mm) - rstep*zdrv(i)
        ind = ind + 3
      enddo
      if (nbsmat.gt.0) then
        do i = 1,numat
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
        xalat(matom) = xalat(matom) - step
        xclat(matom) = xclat(matom) - step
      elseif (mcrd.eq.2) then
        yalat(matom) = yalat(matom) - step
        yclat(matom) = yclat(matom) - step
      elseif (mcrd.eq.3) then
        zalat(matom) = zalat(matom) - step
        zclat(matom) = zclat(matom) - step
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
!  Impose translational invariance condition
!
  indi = 0
  do i = 1,numat
!
!  Set diagonal block to zero
!
    derv2(indi+1,indi+1) = 0.0_dp
    derv2(indi+2,indi+1) = 0.0_dp
    derv2(indi+3,indi+1) = 0.0_dp
    derv2(indi+1,indi+2) = 0.0_dp
    derv2(indi+2,indi+2) = 0.0_dp
    derv2(indi+3,indi+2) = 0.0_dp
    derv2(indi+1,indi+3) = 0.0_dp
    derv2(indi+2,indi+3) = 0.0_dp
    derv2(indi+3,indi+3) = 0.0_dp
    indj = 0
!
!  Negative sum of column blocks below atom i
!
    do j = 1,i-1
      derv2(indi+1,indi+1) = derv2(indi+1,indi+1) - derv2(indj+1,indi+1)
      derv2(indi+2,indi+1) = derv2(indi+2,indi+1) - derv2(indj+2,indi+1)
      derv2(indi+3,indi+1) = derv2(indi+3,indi+1) - derv2(indj+3,indi+1)
      derv2(indi+1,indi+2) = derv2(indi+1,indi+2) - derv2(indj+1,indi+2)
      derv2(indi+2,indi+2) = derv2(indi+2,indi+2) - derv2(indj+2,indi+2)
      derv2(indi+3,indi+2) = derv2(indi+3,indi+2) - derv2(indj+3,indi+2)
      derv2(indi+1,indi+3) = derv2(indi+1,indi+3) - derv2(indj+1,indi+3)
      derv2(indi+2,indi+3) = derv2(indi+2,indi+3) - derv2(indj+2,indi+3)
      derv2(indi+3,indi+3) = derv2(indi+3,indi+3) - derv2(indj+3,indi+3)
      indj = indj + 3
    enddo
    indj = indj + 3
!
!  Negative sum of column blocks above atom i
!
    do j = i+1,numat
      derv2(indi+1,indi+1) = derv2(indi+1,indi+1) - derv2(indj+1,indi+1)
      derv2(indi+2,indi+1) = derv2(indi+2,indi+1) - derv2(indj+2,indi+1)
      derv2(indi+3,indi+1) = derv2(indi+3,indi+1) - derv2(indj+3,indi+1)
      derv2(indi+1,indi+2) = derv2(indi+1,indi+2) - derv2(indj+1,indi+2)
      derv2(indi+2,indi+2) = derv2(indi+2,indi+2) - derv2(indj+2,indi+2)
      derv2(indi+3,indi+2) = derv2(indi+3,indi+2) - derv2(indj+3,indi+2)
      derv2(indi+1,indi+3) = derv2(indi+1,indi+3) - derv2(indj+1,indi+3)
      derv2(indi+2,indi+3) = derv2(indi+2,indi+3) - derv2(indj+2,indi+3)
      derv2(indi+3,indi+3) = derv2(indi+3,indi+3) - derv2(indj+3,indi+3)
      indj = indj + 3
    enddo
    indi = indi + 3
  enddo
!**********************************
!  Copy back initial derivatives  *
!**********************************
  raderv(1:numat) = rdrvin(1:numat)
  xdrv(1:numat) = xdrvin(1:numat)
  ydrv(1:numat) = ydrvin(1:numat)
  zdrv(1:numat) = zdrvin(1:numat)
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
