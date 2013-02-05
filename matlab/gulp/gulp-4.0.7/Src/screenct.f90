  subroutine screenct(fij,maxd2)
!
!  Routine calculates the screening terms for QTPIE.
!
!   4/10 Created based on genpot
!
!  On entry:
!
!    maxd2 = lower dimension of fij
!
!  On exit:
!
!    fij  = fij
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, April 2010
!
  use constants
  use control
  use current
  use element
  use kspace
  use shell
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: maxd2
  real(dp),    intent(inout) :: fij(maxd2,*)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ii
  integer(i4) :: j
  integer(i4) :: jj
  integer(i4) :: kk
  integer(i4) :: maxloop2(3)
  integer(i4) :: ni
  integer(i4) :: nj
  integer(i4) :: npqni
  integer(i4) :: npqnj
  real(dp)    :: cuts2
  real(dp)    :: dsdr
  real(dp)    :: d2sdr2
  real(dp)    :: qlii
  real(dp)    :: qljj
  real(dp)    :: rconv
  real(dp)    :: rl
  real(dp)    :: rqeq2
  real(dp)    :: rr2
  real(dp)    :: rv2
  real(dp)    :: rx
  real(dp)    :: ry
  real(dp)    :: rz
  real(dp)    :: rxi
  real(dp)    :: ryi
  real(dp)    :: rzi
  real(dp)    :: rxj
  real(dp)    :: ryj
  real(dp)    :: rzj
  real(dp)    :: rxk
  real(dp)    :: ryk
  real(dp)    :: rzk
  real(dp)    :: s
  real(dp)    :: xci
  real(dp)    :: yci
  real(dp)    :: zci
  real(dp)    :: zetai
  real(dp)    :: zetaj
  logical     :: lhi
  logical     :: lhj
!
!  Set to unit matrix unless QEq
!
  if (lqeq) then
    do i = 1,numat
      do j = 1,numat
        fij(j,i) = 0.0_dp
      enddo
    enddo
  else
    do i = 1,numat
      do j = 1,numat
        fij(j,i) = 1.0_dp
      enddo
    enddo
  endif
!
!  If electrostatics have been turned off then there is no point in proceeding.
!
  if (.not.lDoElectrostatics) return
!
!  Local variables
!
  rconv = 1.0_dp/autoangs
  cuts2 = cuts*cuts
  rqeq2 = rqeq*rqeq
  if (ndim.gt.0) then
!******************
!  Periodic case  *
!******************
!**************************************************
!  QEq/SandM/ReaxFF corrections to Coulomb terms  *
!**************************************************
!
!  Estimate extent of summations over lattice vectors
!
    if (ndim.eq.3) then
      do i = 1,3
        rv2 = rv(1,i)**2 + rv(2,i)**2 + rv(3,i)**2
        rv2 = sqrt(rv2)
        maxloop2(i) = (rqeq/rv2) + 2
      enddo
    elseif (ndim.eq.2) then
      do i = 1,2
        rv2 = rv(1,i)**2 + rv(2,i)**2
        rv2 = sqrt(rv2)
        maxloop2(i) = (rqeq/rv2) + 2
      enddo
      maxloop2(3) = 0
    elseif (ndim.eq.1) then
      rv2 = rv(1,1)
      maxloop2(1) = (rqeq/rv2) + 1
      maxloop2(2) = 0
      maxloop2(3) = 0
    endif
!
!  Start loop over lower half triangular atom pairs
!
    do i = 1,numat
      ni = nat(i)
      qlii = qf(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
!
!  If QEq work out principle quantum number
!
      if (lqeq) then
        if (ni.le.2) then
          npqni = 1
        elseif (ni.le.10) then
          npqni = 2
        elseif (ni.le.18) then
          npqni = 3
        elseif (ni.le.36) then
          npqni = 4
        elseif (ni.le.54) then
          npqni = 5
        elseif (ni.le.86) then
          npqni = 6
        else
          npqni = 7
        endif
        zetai = 0.5_dp*qeqlambda*(2*npqni+1)/qeqrad(ni)
        lhi = (ni.eq.1)
        if (lhi) then
!
!  Special case for hydrogen
!
          zetai = zetai + qlii*rconv
        endif
      endif
      do j = 1,i
        qljj = qf(j)
        rx = xclat(j) - xci
        ry = yclat(j) - yci
        rz = zclat(j) - zci
        nj = nat(j)
!
!  If QEq work out principle quantum number
!
        if (lqeq) then
          if (nj.le.2) then
            npqnj = 1
          elseif (nj.le.10) then
            npqnj = 2
          elseif (nj.le.18) then
            npqnj = 3
          elseif (nj.le.36) then
            npqnj = 4
          elseif (nj.le.54) then
            npqnj = 5
          elseif (nj.le.86) then
            npqnj = 6
          else
            npqnj = 7
          endif
          zetaj = 0.5_dp*qeqlambda*(2*npqnj+1)/qeqrad(nj)
          lhj = (nj.eq.1)
          if (lhj) then
!
!  Special case for hydrogen
!
            zetaj = zetaj + qljj*rconv
          endif
        endif
!
!  Loop over cell vectors
!
        rxi = rx - (maxloop2(1) + 1)*r1x
        ryi = ry - (maxloop2(1) + 1)*r1y
        rzi = rz - (maxloop2(1) + 1)*r1z
        do ii = - maxloop2(1),maxloop2(1)
          rxi = rxi + r1x
          ryi = ryi + r1y
          rzi = rzi + r1z
          rxj = rxi - (maxloop2(2) + 1)*r2x
          ryj = ryi - (maxloop2(2) + 1)*r2y
          rzj = rzi - (maxloop2(2) + 1)*r2z
          do jj = - maxloop2(2),maxloop2(2)
            rxj = rxj + r2x
            ryj = ryj + r2y
            rzj = rzj + r2z
            rxk = rxj - (maxloop2(3) + 1)*r3x
            ryk = ryj - (maxloop2(3) + 1)*r3y
            rzk = rzj - (maxloop2(3) + 1)*r3z
            do 120 kk = - maxloop2(3),maxloop2(3)
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
              if (rr2.gt.rqeq2) goto 120
!
!  Trap self term
!
              if (rr2.lt.cuts2) goto 120
              rl = sqrt(rr2)
              if (lqeq) then
!
!  Calculate screening using overlap integral of s orbitals
!
                call overlap(npqni,npqnj,zetai,zetaj,rl,s,dsdr,d2sdr2)
                fij(j,i) = fij(j,i) + s
                if (i.ne.j) then
                  fij(i,j) = fij(i,j) + s
                endif
              endif
!
!  End of loops over lattice vectors
!
120         continue
          enddo
        enddo
!
!  End of loops over atom pairs
!
      enddo
    enddo
  else
!*****************
!  Cluster case  *
!*****************
!
!  Start loop over cluster atom - unit cell atom pairs
!
    do i = 1,numat
      qlii = qf(i)
      xci = xclat(i)
      yci = yclat(i)
      zci = zclat(i)
      ni = nat(i)
!
!  If QEq work out principle quantum number
!
      if (lqeq) then
        if (ni.le.2) then
          npqni = 1
        elseif (ni.le.10) then
          npqni = 2
        elseif (ni.le.18) then
          npqni = 3
        elseif (ni.le.36) then
          npqni = 4
        elseif (ni.le.54) then
          npqni = 5
        elseif (ni.le.86) then
          npqni = 6
        else
          npqni = 7
        endif
        zetai = 0.5_dp*qeqlambda*(2*npqni+1)/qeqrad(ni)
        lhi = (ni.eq.1)
        if (lhi) then
!
!  Special case for hydrogen
!
          zetai = zetai + qlii*rconv
        endif
      endif
      do j = 1,i-1
        qljj = qf(j)
        nj = nat(j)
!
!  If QEq work out principle quantum number
!
        if (lqeq) then
          if (nj.le.2) then
            npqnj = 1
          elseif (nj.le.10) then
            npqnj = 2
          elseif (nj.le.18) then
            npqnj = 3
          elseif (nj.le.36) then
            npqnj = 4
          elseif (nj.le.54) then
            npqnj = 5
          elseif (nj.le.86) then
            npqnj = 6
          else
            npqnj = 7
          endif
          zetaj = 0.5_dp*qeqlambda*(2*npqnj+1)/qeqrad(nj)
          lhj = (nj.eq.1)
          if (lhj) then
!
!  Special case for hydrogen
!
            zetaj = zetaj + qljj*rconv
          endif
        endif
!
!  Find relative vector between atoms
!
        rx = xclat(j) - xci
        ry = yclat(j) - yci
        rz = zclat(j) - zci
        rr2 = rx*rx + ry*ry + rz*rz
!
!  Exclude core-shell interaction
!
        if (rr2.lt.cuts2) goto 125
        rl = sqrt(rr2)
        if (lqeq.and.rl.lt.rqeq) then
!***************
!  QEq scheme  *
!***************
          call overlap(npqni,npqnj,zetai,zetaj,rl,s,dsdr,d2sdr2)
          fij(j,i) = fij(j,i) + s
          fij(i,j) = fij(i,j) + s
        endif
125     continue
      enddo
    enddo
  endif
!
  return
  end
