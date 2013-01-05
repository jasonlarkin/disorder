  subroutine defener(edefect,lgrad1,lgrad2)
!
!  Subroutine for calculating defect energies and derivatives
!
!  Calculate region 2a displacements first then use positions
!  in defect energy calculation.
!
!  ld2sym = if .true. then the second derivatives will be
!           generated using symmetry
!
!   9/95 Looping limit in zeroing of derivatives put equal to 
!        ndasym
!   4/97 Modifications for Sutton-Chen potentials added
!   7/97 Initialisation of region2 densities for EAM added
!   7/97 New algorithm added for defect calculations which
!        avoids more numerical instabilities associated with
!        adding and subtracting terms. However, it is often
!        slower and therefore the original algorithm is also
!        retained. Note that the new algorithm must be used
!        for EAM calcs though.
!   8/02 Brenner potential added
!  12/03 Initialisation of raderv now uses nloop instead of ndasym
!  10/06 Explicit subtraction of region 1 - 2 removed
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
!   6/09 Module name changed from three to m_three
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use bondorderdata, only : nbopot
  use control
  use current
  use defects
  use derivatives
  use eam,           only : lMEAMden, maxmeamcomponent
  use energies
  use four
  use parallel
  use region2a
  use sutton
  use m_three
  implicit none
!
!  Passed variables
!
  real(dp), intent(out) :: edefect
  logical,  intent(in)  :: lgrad1
  logical,  intent(in)  :: lgrad2
!
!  Local variables
!
  integer(i4)           :: i
  integer(i4)           :: j
  integer(i4)           :: maxlim
  integer(i4)           :: nloop
  logical               :: lmanysym
  logical               :: lnewdefalg
!
!  Set new defect algorithm flag - must use this algorithm if EAM
!  model is being used.
!
  lnewdefalg = (index(keyword,'newda').ne.0.or.lsuttonc.or.ldcellr)
!
!  Initialise energies to zero
!
  e12a = 0.0_dp
  e12ad = 0.0_dp
  e12ap = 0.0_dp
  e11 = 0.0_dp
  e2a = 0.0_dp
  e12b = 0.0_dp
  e12bo = 0.0_dp
  e12t = 0.0_dp
  e12f = 0.0_dp
  e12m = 0.0_dp
  maxlim = 3*(nreg1 + 1)
  if (ldbsm) maxlim = maxlim + nreg1
!
!  Zero arrays
!
  if (lgrad1) then
    if (ld1sym) then
      nloop = nreg1
    else
      nloop = ndasym
    endif
    if (nloop.gt.maxd1) then
      maxd1 = nloop
      call changemaxd1
    endif
    do i = 1,nloop
      xdrv(i) = 0.0_dp
      ydrv(i) = 0.0_dp
      zdrv(i) = 0.0_dp
    enddo
    if (ldbsm) then
      do i = 1,nloop
        raderv(i) = 0.0_dp
      enddo
    endif
  endif
  if (lgrad2) then
    if (ld2sym) then
      nloop = 3*ndasym
      if (ldbsm) nloop = nloop + ndasym
    else
      nloop = maxlim
    endif
    if (maxlim.gt.maxd2) then
      maxd2 = maxlim
      call changemaxd2
    endif
    if (nloop.gt.maxd2u) then
      maxd2u = nloop
      call changemaxd2
    endif
    do i = 1,nloop
      do j = 1,maxlim
        derv2(j,i) = 0.0_dp
      enddo
    enddo
  endif
!
!  Zero local energies
!
  erecip = 0.0_dp
  eatom = 0.0_dp
  ereal = 0.0_dp
  ec6 = 0.0_dp
  if (ld1sym.and.(.not.lgrad2.or.ld2sym)) then
!***********************************************
!  Symmetry adapted - including second derivs  *
!***********************************************
    if (lsuttonc) then
      lmanysym = .true.
!
!  Zero defective region 1 densities
!
      if (lMEAMden) then
        do i = 1,ndasym
          dscrho(1:maxmeamcomponent,i) = 0.0_dp
        enddo
      else
        do i = 1,ndasym
          dscrho(1,i) = 0.0_dp
        enddo
      endif
!
!  Initialise region 2 densities to bulk values - perfect region 1
!
      if (lMEAMden) then
        do i = 1,ndpasym2a
          dscrhor2d(1:maxmeamcomponent,i) = dscrhor2p(1:maxmeamcomponent,ndsptr2a(i))
        enddo
      else
        do i = 1,ndpasym2a
          dscrhor2d(1,i) = dscrhor2p(1,ndsptr2a(i))
        enddo
      endif
    endif
!
!  Reciprocal space component of region 1 - region 2a energy
!
    if (lewald) then
      call recip12as(erecip,ec6,lgrad1,lgrad2)
    endif
!
!  Real space component of region 1 - region 2a energy
!
    if (lnewdefalg) then
      call real12as2(eatom,ereal,erecip,ec6,lgrad1,lgrad2)
      if (lsuttonc.and.lMEAMden) call density12as2
!
!  Correct region 2a densities for defective region 1 in EAM
!
      if (lsuttonc) then
        if (lMEAMden) then
          call density2a
        else
          call real2amany
        endif
      endif
    else
      call real12as(eatom,ereal,erecip,ec6,lgrad1,lgrad2)
    endif
    e12a = erecip + eatom + ereal + ec6
!
!  Correct for defective region 1 - perfect region 1 energy
!
    ereal = 0.0_dp
    eatom = 0.0_dp
    if (lnewdefalg) then
      call real11s2(eatom,ereal,lgrad1,lgrad2,3_i4)
      if (lsuttonc.and.lMEAMden) call density11s2(3_i4)
    endif
    e12a = e12a - (eatom + ereal)
!
!  Region 1 - region 1 energy
!
    ereal = 0.0_dp
    eatom = 0.0_dp
    if (lnewdefalg) then
      call real11s2(eatom,ereal,lgrad1,lgrad2,1_i4)
      if (lsuttonc.and.lMEAMden) call density11s2(1_i4)
    else
      call real11s(eatom,ereal,lgrad1,lgrad2,1_i4)
    endif
    e11 = eatom + ereal
  else
!****************
!  No symmetry  *
!****************
    if (lsuttonc) then
      lmanysym = .false.
!
!  Zero defective region 1 densities
!
      if (lMEAMden) then
        do i = 1,nreg1
          dscrho(1:maxmeamcomponent,i) = 0.0_dp
        enddo
      else
        do i = 1,nreg1
          dscrho(1,i) = 0.0_dp
        enddo
      endif
!
!  Initialise region 2 densities to bulk values - perfect region 1
!
      if (lMEAMden) then
        do i = 1,npreg2
          dscrhor2d(1:maxmeamcomponent,i) = dscrhor2p(1:maxmeamcomponent,i)
        enddo
      else
        do i = 1,npreg2
          dscrhor2d(1,i) = dscrhor2p(1,i)
        enddo
      endif
    endif
!
!  Reciprocal space component of region 1 - region 2a energy
!
    if (lewald) then
      call recip12a(erecip,ec6,lgrad1,lgrad2,1_i4)
    endif
!
!  Real space component of region 1 - region 2a energy
!
    if (lnewdefalg) then
      call real12a2(eatom,ereal,erecip,ec6,lgrad1,lgrad2,1_i4)
      if (lsuttonc.and.lMEAMden) call density12a2(1_i4)
    else
      call real12a(eatom,ereal,erecip,ec6,lgrad1,lgrad2,1_i4)
    endif
    e12a = erecip + eatom + ereal + ec6
!
!  Correct for defective region 1 - perfect region 1 energy
!
    ereal = 0.0_dp
    eatom = 0.0_dp
    if (lnewdefalg) then
      call real112(eatom,ereal,lgrad1,lgrad2,3_i4)
      if (lsuttonc.and.lMEAMden) call density112(3_i4)
    endif
    e12a = e12a - (eatom + ereal)
!
!  Region 1 - region 1 energy
!
    ereal = 0.0_dp
    eatom = 0.0_dp
    if (lnewdefalg) then
      call real112(eatom,ereal,lgrad1,lgrad2,1_i4)
      if (lsuttonc.and.lMEAMden) call density112(1_i4)
    else
      call real11(eatom,ereal,lgrad1,lgrad2,1_i4)
    endif
    e11 = eatom + ereal
  endif
!
!  Region 2 relaxed coordinate energy
!  Calculates displacements and uses them in energy calc.
!
  call real2a(e2a,e12ap,e12ad,lgrad1,lgrad2)
  call real2b(e2b)
!
!  Three-body component
!
  if (nthb.gt.0) call three12(e12t,lgrad1,lgrad2,1_i4)
!
!  Four-body component
!
  if (nfor.gt.0) call four12(e12f,lgrad1,lgrad2,1_i4)
!
!  Many-body component
!
  if (lsuttonc) then
    call many12(e12m,lgrad1,lgrad2,1_i4,lmanysym)
  endif
!
!  Brenner potential
!
  if (lbrenner) call brenner12(e12b,1_i4,lgrad1,lgrad2)
!
!  Bond order potential
!
  if (nbopot.gt.0) call bondorder12(e12bo,1_i4,lgrad1,lgrad2)
!
!  Complete derivatives
!
  if (lgrad2) then
    if (ld2sym) then
      call sumderv2s(nreg1+1_i4,ndasym,ldbsm,.false.)
    else
      call sumderv2(nreg1+1_i4,ldbsm)
    endif
  endif
!
!  Sum components of defect energy
!  e2a is kept separate from edefect as it's gradients are not
!  included and thus would cause problems in minimisation
!
  edefect = e12a - e12aold + e11 - e11old + e12t - e12told + e12ad + e2a + &
    e2b - e12ap + e12f - e12fold + e12m - e12mold + e12b - e12bold + e12bo - e12boold
  fcstore = edefect
  if (ioproc.and.leprint) call outdener
!
  return
  end
