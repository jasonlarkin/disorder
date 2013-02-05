  subroutine functsync(iflag,niter,nrep,nrepptr,n,xcrep,fcrep,gcrep,tangent, &
                       fctot,nlower)
!
!  Supplies the function and derivatives for a synchronous transit point. 
!  Note that although the information regarding both points is passed in,
!  only the energy and forces are computed for the lower image. 
!
!  On entry :
!
!  n             = number of variables per replica
!  nlower        = pointer to replica that is lower in energy
!  niter         = iteration counter
!  nrep          = number of replicas (including initial and final configs)
!  xcrep(n,nrep) = variables for replicas
!
!  On return :
!
!  fctot         = sum of energies for all replicas
!  fcrep(nrep)   = energies for each replica
!  gcrep(n,nrep) = gradients for each replica
!
!  11/06 Created from functneb
!  11/06 xc now passed getderv1
!  12/07 Intent for tangent argument corrected
!   5/08 lgeometryOK added as argument to xctox0
!   5/09 Calls to x0tostr routines moved from energy to calling routine
!  11/12 Unused argument removed including from call to syncforce
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
!  Julian Gale, NRI, Curtin University, November 2012
!
  use configurations, only : rvcfg
  use control
  use current,        only : ncf, ndim
  use general
  use neb,            only : nebreplicacell
  use parallel
  use symmetry,       only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(inout):: iflag
  integer(i4), intent(in)   :: n
  integer(i4), intent(in)   :: nlower
  integer(i4), intent(in)   :: niter
  integer(i4), intent(in)   :: nrep
  integer(i4), intent(in)   :: nrepptr(nrep)
  real(dp),    intent(out)  :: fcrep(n)
  real(dp),    intent(out)  :: fctot
  real(dp),    intent(out)  :: gcrep(n,nrep)
  real(dp),    intent(in)   :: xcrep(n,nrep)
  real(dp),    intent(out)  :: tangent(n)
!
!  Local variables
!
  integer(i4)               :: i
  integer(i4)               :: j
  integer(i4)               :: nrepf
  logical                   :: lgeometryOK
  real(dp)                  :: cputime
  real(dp)                  :: rvcfgsave(3,3)
  real(dp),            save :: tdmax = 0.0_dp
  real(dp)                  :: t1
  real(dp)                  :: t2
!
  t1 = cputime()
  iflag = 0
!***************************************
!  Compute energy and standard forces  *
!***************************************
  fctot = 0.0_dp
!
!  Swap current replica cell into reference cell for configuration
!
  do i = 1,3
    do j = 1,3
      rvcfgsave(j,i) = rvcfg(j,i,ncf)
    enddo
  enddo
!
!  Replica
!
  nrepf = nrepptr(nlower)
  if (ndim.eq.3) then
    call cell3D(rvcfg(1,1,ncf),nebreplicacell(1,nrepf),nebreplicacell(2,nrepf),nebreplicacell(3,nrepf), &
      nebreplicacell(4,nrepf),nebreplicacell(5,nrepf),nebreplicacell(6,nrepf))
  elseif (ndim.eq.2) then
    call cell2D(rvcfg(1,1,ncf),nebreplicacell(1,nrepf),nebreplicacell(2,nrepf),nebreplicacell(3,nrepf))
  elseif (ndim.eq.1) then
    call cell1D(rvcfg(1,1,ncf),nebreplicacell(1,nrepf))
  endif
!
!  Convert optimisation variables to linear structure array
!
  call xctox0(n,xcrep(1,nlower),lgeometryOK)
!
!  Check geometry
!
  if (.not.lgeometryOK) then
    call outerror('Cell parameter has fallen below allowed limit',0_i4)
    call stopnow('functsync')
  endif
!
!  Convert linear structure array to main structure arrays
!
  if (lx0centroid) then
    call x0tostrcentroid
  else
    call x0tostr
  endif
!
!  Evaluate function and first derivatives
!
  call energy(fcrep(nlower),.true.,.false.)
  fctot = fctot + fcrep(nlower)
!
!  Complete strain derivatives
!
  if (lstr) call strfin(.false.)
!
!  First derivative handling
!
  call getderv1(n,xcrep(1,nlower),gcrep(1,nlower),.false.,.false.)
!
!  Swap back reference cell for configuration
!
  do i = 1,3
    do j = 1,3
      rvcfg(j,i,ncf) = rvcfgsave(j,i)
    enddo
  enddo
!*******************
!  Project forces  *
!*******************
!
!  Calculate influence of forces between replicas
!
  call syncforce(nlower,n,xcrep,gcrep(1,nlower),tangent)
!
!  Check cputime
!
  t2 = cputime()
  if (t2-t1.gt.tdmax) tdmax = t2 - t1
  if (timmax.gt.0.0_dp) then
    if ((timmax-t2).lt.tdmax.and..not.lrelax) iflag = -1
  endif
!
  return
  end
