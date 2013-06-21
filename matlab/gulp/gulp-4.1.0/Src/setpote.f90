  subroutine setpote
!
!  Setup tasks for twobody potentials
!
!   7/95 repcut added - limit range of exponential terms in
!        Buckingham potential to save expense
!   8/95 setup for 1/r**6 sum added
!   8/95 handling of repcut for lennard-jones added
!  12/00 apot now abs in recput calculation for exponential
!        forms to avoid negative log problems
!   4/05 Mods for cosh-spring potential added
!  11/05 Call to set taper parameters for Voter style added
!   8/07 Possibility of non-integer powers in L-J potentials added
!  11/07 Unused variables cleaned up
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
  use element
  use general
  use molecule
  use shell
  use two
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ni
  integer(i4) :: nj
  real(dp)    :: ri
  real(dp)    :: rj
  real(dp)    :: rmpt
  real(dp)    :: rtol2
!
!  Set potential cutoffs for core-shell spring potential
!  This must be done after input is complete in case
!  cuts value is changed.
!  Also set maximum potential cutoff
!
  rpmax = 0.0_dp
  do i = 1,npote
    if (nptype(i).eq.8.or.nptype(i).eq.33) then
      rpot(i) = cuts
      rpot2(i) = 0.0_dp
    endif
    if (rpot(i).gt.rpmax) rpmax = rpot(i)
  enddo
  if (rpmax.gt.cutp) rpmax = cutp
!
!  Setup Lennard-Jones potentials
!
  call setlenn
!
!  Set repulsive exponential cutoffs
!
  if (index(keyword,'norep').eq.0) then
    do i = 1,npote
      if (nptype(i).eq.1.or.nptype(i).eq.7) then
        if (abs(twopot(1,i)).lt.1.0d-6) then
          repcut(i) = rpot2(i)
        elseif (abs(twopot(2,i)).lt.1.0d-6) then
          repcut(i) = rpot(i)
        else
          repcut(i) = - twopot(2,i)*log((10.0**(-accuracy)/abs(twopot(1,i))))
          repcut(i) = min(repcut(i),rpot(i))
        endif
      elseif (nptype(i).eq.2) then
        if (abs(twopot(1,i)).lt.1.0d-6) then
          repcut(i) = rpot2(i)
        else
          rmpt = tpot(1,i)
          rmpt = 1.0_dp/rmpt
          repcut(i) = (abs(twopot(1,i)*10.0_dp**accuracy))**rmpt
          repcut(i) = min(repcut(i),rpot(i))
        endif
      else
        repcut(i) = rpot(i)
      endif
    enddo
  else
    do i = 1,npote
      repcut(i) = rpot(i)
    enddo
  endif
!
!  Setup C6 terms
!
  if (lc6) call setc6
!
!  Assign dummy cutoffs for two-body terms based on covalent
!  radii for bonded potentials. 
!
  rtol2 = 1.6_dp*rtol
  do i = 1,npote
    if (mmexc(i).eq.1) then
      ni = nspec1(i)
      nj = nspec2(i)
      if (ni.gt.maxele) ni = ni - maxele
      if (nj.gt.maxele) nj = nj - maxele
      ri = rcov(ni)
      rj = rcov(nj)
      rpot2(i) = 0.0_dp
      rpot(i) = rtol2*(ri + rj)
    endif
  enddo
!
!  Set up taper parameters for Voter style
!
  if (tapertype.eq.3) call settaper
!
!  Set up inverse rho values
!
  call rhoinv
!
  return
  end
