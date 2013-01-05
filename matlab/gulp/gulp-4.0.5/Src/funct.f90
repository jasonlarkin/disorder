  subroutine funct(iflag,n,xc,fc,gc)
!
!  Supplies the function and derivatives
!
!   5/95 Modified to handle symmetrised second derivatives
!   6/95 Modified to allow for additive constraints
!   8/97 Modified to allow evaluation of cost function - SMW RIGB
!   9/97 Modified to allow evaluation 1st dervs of cost function
!   3/99 Option to write out derivatives to a file added
!  11/06 xc now passed getderv1
!   3/07 Intent added
!   5/08 lgeometryOK added as argument to xctox0 & iflag set to -2
!        on return if geometry is not OK.
!   5/09 Calls to x0tostr routines moved from energy to calling routine
!   7/09 Modified for empirical valence bond theory
!   1/11 Force minimisation added
!   5/11 Call to cutscheck added
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
!  Julian Gale, NRI, Curtin University, May 2011
!
  use control
  use files
  use general
  use genetic,   only : lgacost
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(inout) :: iflag
  integer(i4), intent(in)    :: n
  real(dp),    intent(out)   :: fc
  real(dp),    intent(in)    :: xc(*)
  real(dp),    intent(out)   :: gc(*)
!
!  Local variables
!
  integer(i4)                :: i
  logical                    :: lgeometryOK
  logical                    :: lgrad1
  logical                    :: lgrad2
  real(dp)                   :: cputime
  real(dp),             save :: tdmax = 0.0_dp
  real(dp)                   :: t1
  real(dp)                   :: t2
!
  t1 = cputime()
  lgrad1 = (iflag.ge.1.or.lforcemin)
  lgrad2 = (iflag.ge.2)
!
!  If cost function required rather than energy
!
  if (lgacost) then
    call costfn(n,xc,fc,iflag)
    if (lgrad1) then
      call getderv1(n,xc,gc,lgrad2,.false.)
    endif
    return
  endif
!
!  Convert optimisation variables to linear structure array
!
  call xctox0(n,xc,lgeometryOK)
!
!  Convert linear structure array to main structure arrays
!
  if (lx0centroid) then
    call x0tostrcentroid
  else
    call x0tostr
  endif
!
!  Check core-shell distances
!
  call cutscheck
!
  lfirst = .true.
!
!  Evaluate function and first derivatives
!
  call energy(fc,lgrad1,lgrad2)
!
!  For surface, get surface energy
!
  if (lseok) call surfaceenergy(fc)
!
!  Complete strain derivatives
!
  if (lstr) call strfin(lgrad2)
!
!  Output second derivatives 
!
  if (lgrad2.and.ioproc) call outderv
!
!  Output energy and derivatives to a file
!
  if (ldrv.and.ioproc) call outdrv(fc,lgrad1,lgrad2)
!
!  Option to write out a .frc file for QMPOT
!
  if (lfrc.and.ioproc) call outfrc(fc,lgrad1,.false.)
!
!  First derivative handling
!
  if (lgrad1) then
    call getderv1(n,xc,gc,lgrad2,.false.)
  endif
!
!  Force minimisation option
!
  if (lforcemin) then
    fc = 0.0_dp
    if (n.gt.0) then
      do i = 1,n
        fc = fc + gc(i)**2
      enddo
      fc = sqrt(fc)/dble(n)
    endif
  endif
!
  t2 = cputime()
  if (t2-t1.gt.tdmax) tdmax = t2 - t1
  if (timmax.gt.0.0_dp) then
    if ((timmax-t2).lt.tdmax.and..not.lrelax) iflag = -1
  endif
  if (.not.lgeometryOK) iflag = -2
!
  return
  end
