  subroutine setcutoffmax
!
!  Calculates the maximum cut-off distance over all potentials required
!  for a spatial decomposition of the atoms into boxes.
!
!   5/03 Created
!   8/03 Bond order cut-offs added
!   9/04 Cutoff for nboQ potentials added
!   7/05 Streitz and Mintmire modifications added
!   7/05 Coupled-charge potential modifications added
!  10/05 Inversion potential added
!   6/06 Inversion squared potential added
!   7/06 Six-body potentials included
!  10/07 Angle-angle cross potential added
!  11/07 Modifications for reaxFF Coulomb term added
!   1/08 lreaxFFqreal removed
!  11/08 xcosangleangle potential added
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
!   6/09 Module name changed from three to m_three
!   7/09 cutoffmax now passed via general module
!
!  On exit :
!
!  cutoffmax = maximum cut-off distance needed over all potential types
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
!  Julian Gale, NRI, Curtin University, July 2009
!
  use bondorderdata
  use brennerdata,   only : bR2, bTR2
  use chargecoupled, only : rCCmaxL, rCCmaxS, nCCspec
  use control,       only : lbrenner, lwolf
  use current,       only : lewald
  use element,       only : lqeq, rqeq, lSandM
  use four
  use general,       only : cutw, cutoffmax
  use kspace,        only : radmax
  use m_three
  use six
  use two
  implicit none
!
!  Local variables
!
  integer(i4)               :: n
  real(dp)                  :: cmax1
  real(dp)                  :: cmax2
!
!  Initialise the cut-off distance to be equal to the maximum two-body cut-off from setpote
!
  cutoffmax = rpmax
!
!  Compare with electrostatic cut-off
!
  if (lewald) cutoffmax = max(cutoffmax,radmax)
  if (lwolf) cutoffmax = max(cutoffmax,cutw)
  if (lqeq.or.lSandM) cutoffmax = max(cutoffmax,rqeq)
!
!  Compare with three-body cut-off
!
  do n = 1,nthb
    if (thr3(n).gt.0.0_dp) then
      cutoffmax = max(cutoffmax,thr1(n),thr2(n),thr3(n))
    else
      cutoffmax = max(cutoffmax,thr1(n)+thr2(n))
    endif
  enddo
!
!  Compare with four-body cut-off
!
  do n = 1,nfor
    if (loutofplane(n)) then
!
!  Out of plane
!
      cutoffmax = max(cutoffmax,for1(n),for2(n),for3(n))
    else
!
!  Standard torsion
!
      if (for4(n).gt.0.0_dp) then
        cutoffmax = max(cutoffmax,for1(n),for2(n),for3(n),for4(n))
      else
        cutoffmax = max(cutoffmax,for1(n)+for2(n)+for3(n))
      endif
    endif
  enddo
!
!  Compare with six-body cut-off
!
  do n = 1,nsix
    cmax1 = max(six2(n),six3(n))
    cmax2 = max(six4(n),six5(n))
    cutoffmax = max(cutoffmax,six1(n)+cmax1+cmax2)
  enddo
!
!  Compare with Brenner cut-off
!
  if (lbrenner) cutoffmax = max(cutoffmax,bR2(1),bR2(2),bR2(3),bTR2(1),bTR2(2),bTR2(3))
!
!  Compare with bond-order cut-off
!
  do n = 1,nbopot
    cutoffmax = max(cutoffmax,rBOmax(n))
  enddo
  do n = 1,nboQ
    cutoffmax = max(cutoffmax,rBOmaxQ(n))
  enddo
  do n = 1,nCCspec
    cutoffmax = max(cutoffmax,rCCmaxS(n),rCCmaxL(n))
  enddo
!
  return
  end
