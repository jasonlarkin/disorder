  subroutine qmatrixelementc(xji,yji,zji,cuts2,lallimages,lgrad1,lgrad2,qme,dqme,d2qme)
!
!  Calculates the Coulomb matrix element for COSMO/COSMIC solvation
!  using the summation technique of Wolf et al.
!
!   4/05 Created from qmatrixelement
!   4/05 lallimages flag added
!  12/08 Migrated to version 3.5 and converted to f90 format
!
!  On entry:
!
!    xji    = difference in X coordinates of two points
!    yji    = difference in Y coordinates of two points
!    zji    = difference in Z coordinates of two points
!    cuts2  = core-shell cutoff, if applicable
!    lgrad1 = if .true. then calculate the first derivatives
!    lgrad2 = if .true. then calculate the second derivatives
!    lAllImages = if .true. then the potential due to all periodic images
!                 is computed, rather than just the single image input
!
!  On exit:
!
!    qme    = Coulomb matrix element between points (in Angs**-1)
!    dqme   = first Cartesian derivatives of qme (dQ/d(alpha) in
!             Angs**-2) if lgrad1 is .true.
!    d2qme  = second Cartesian derivatives of qme (d2Q/d(alpha)d(beta)
!             in Angs**-3) if lgrad2 is .true.
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
!  Copyright Curtin Univerisity 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use datatypes
  use current,   only : r1x, r1y, r1z
  use current,   only : r2x, r2y, r2z
  use current,   only : r3x, r3y, r3z
  use wolfcosmo, only : etawc, selfwolfc, maxloopc, rmax2c, tweatpic
  implicit none
!
!  Passed variables
!
  real(dp), intent(in)    :: xji
  real(dp), intent(in)    :: yji
  real(dp), intent(in)    :: zji
  real(dp), intent(in)    :: cuts2
  real(dp), intent(out)   :: qme
  real(dp), intent(out)   :: dqme(3)
  real(dp), intent(out)   :: d2qme(6)
  logical,  intent(in)    :: lAllImages
  logical,  intent(in)    :: lgrad1
  logical,  intent(in)    :: lgrad2
!
!  Local variables
!
  integer(i4) :: ii
  integer(i4) :: jj
  integer(i4) :: kk
  integer(i4) :: ml1
  integer(i4) :: ml2
  integer(i4) :: ml3
  real(dp)    :: dtrm
  real(dp)    :: dtrm2
  real(dp)    :: etaloc
  real(dp)    :: rexp
  real(dp)    :: rl
  real(dp)    :: rrl
  real(dp)    :: rrl2
  real(dp)    :: rr2
  real(dp)    :: rxi, ryi, rzi
  real(dp)    :: rxj, ryj, rzj
  real(dp)    :: rxk, ryk, rzk
  real(dp)    :: trm
!
!  Functions
!
  real(dp)    :: derfc
!
!  Zero matrix element / derivatives
!
  qme = 0.0_dp
  if (lgrad1) then
    dqme(1:3) = 0.0_dp
    if (lgrad2) then
      d2qme(1:6) = 0.0_dp
    endif
  endif
!
!  Set up local variables
!
  etaloc = etawc*etawc
!*************************
!  Real space summation  *
!*************************
  if (lAllImages) then
    ml1 = maxloopc(1)
    ml2 = maxloopc(2)
    ml3 = maxloopc(3)
  else
    ml1 = 0
    ml2 = 0
    ml3 = 0
  endif
!
!  Loop over cell vectors
!
  rxi = xji - (ml1+1)*r1x
  ryi = yji - (ml1+1)*r1y
  rzi = zji - (ml1+1)*r1z
  do ii = -ml1,ml1
    rxi = rxi + r1x
    ryi = ryi + r1y
    rzi = rzi + r1z
    rxj = rxi - (ml2+1)*r2x
    ryj = ryi - (ml2+1)*r2y
    rzj = rzi - (ml2+1)*r2z
    do jj = -ml2,ml2
      rxj = rxj + r2x
      ryj = ryj + r2y
      rzj = rzj + r2z
      rxk = rxj - (ml3+1)*r3x
      ryk = ryj - (ml3+1)*r3y
      rzk = rzj - (ml3+1)*r3z
      do kk = -ml3,ml3
        rxk = rxk + r3x
        ryk = ryk + r3y
        rzk = rzk + r3z
!
!  Calculate distance squared
!
        rr2 = rxk*rxk + ryk*ryk + rzk*rzk
!
!  Exclude distances outside maximum cutoff
!
        if (rr2.le.rmax2c) then
!
!  Trap self term
!
          if (rr2.lt.1.0d-15) then
            qme = qme - tweatpic - selfwolfc
            if (lgrad2) then
              trm = 2.0_dp*tweatpic*etaloc/3.0_dp
              d2qme(1) = d2qme(1) - trm
              d2qme(3) = d2qme(3) - trm
              d2qme(6) = d2qme(6) - trm
            endif
          else
            rl = sqrt(rr2)
            rrl = 1.0_dp/rl
            if (rr2.lt.cuts2) then
!
!  Core-shell interaction
!
              qme = qme - rrl
              if (lgrad1) then
                dtrm = - rrl
                if (lgrad2) then
                  dtrm2 = - 3.0_dp*rrl*rrl*rrl
                endif
              endif
            else
              dtrm = 0.0_dp
              dtrm2 = 0.0_dp
            endif
            trm = derfc(etawc*rl)*rrl
            qme = qme + trm - selfwolfc
            if (lgrad1) then
              rexp = tweatpic*exp(-etaloc*rr2)
              rrl2 = rrl*rrl
              dtrm = (dtrm + trm + rexp)*rrl2
              dqme(1) = dqme(1) + dtrm*rxk
              dqme(2) = dqme(2) + dtrm*ryk
              dqme(3) = dqme(3) + dtrm*rzk
              if (lgrad2) then
                dtrm2 = dtrm2 + rexp*((3.0_dp*rrl2)+2.0_dp*etaloc) + 3.0_dp*trm*rrl2
                dtrm2 = dtrm2*rrl2
                d2qme(1) = d2qme(1) - dtrm2*rxk*rxk
                d2qme(2) = d2qme(2) - dtrm2*rxk*ryk
                d2qme(3) = d2qme(3) - dtrm2*ryk*ryk
                d2qme(4) = d2qme(4) - dtrm2*rxk*rzk
                d2qme(5) = d2qme(5) - dtrm2*ryk*rzk
                d2qme(6) = d2qme(6) - dtrm2*rzk*rzk
                d2qme(1) = d2qme(1) + dtrm
                d2qme(3) = d2qme(3) + dtrm
                d2qme(6) = d2qme(6) + dtrm
              endif
            endif
          endif
        endif
!
!  End of loops over lattice vectors
!
      enddo
    enddo
  enddo
!
  return
  end
