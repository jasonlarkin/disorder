  subroutine nebforce(nrp,nrep,n,niter,nrepwithmaxfc,xcrep,fcrep,gc,tangent)
!
!  Projects out the perpendicular component of the derivatives and adds
!  the spring constant term.
!
!  On entry :
!
!  n             = number of variables per replica
!  nrp           = number of current replica
!  nrep          = total number of replicas
!  niter         = iteration counter
!  nrepwithmaxfc = pointer to replica with highest energy
!  xcrep(n,*)    = variables for replicas
!  fcrep(*)      = energies for replicas
!  gc(n)         = uncorrected gradients for current replica
!
!  On return :
!
!  gc(n)         = corrected gradients for current replica
!
!  Workspace :
!
!  tangent(n)    = storage for tangent to path 
!
!   2/03 Created from functneb
!   7/03 Options for the tangent introduced
!   7/03 Energies now passed in for new tangent algorithm
!   7/03 Climbing image implemented
!   7/03 Variable springs added
!  11/06 Updated for GULP3.2
!   4/07 All dimensions of passed arrays explicitly declared
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, April 2007
!
  use control,  only : keyword
  use current,  only : ncf
  use neb
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4),                  intent(in)    :: n
  integer(i4),                  intent(in)    :: niter
  integer(i4),                  intent(in)    :: nrep
  integer(i4),                  intent(in)    :: nrp
  integer(i4),                  intent(in)    :: nrepwithmaxfc
  real(dp),                     intent(in)    :: fcrep(nrep)
  real(dp),                     intent(inout) :: gc(n)
  real(dp),                     intent(in)    :: xcrep(n,nrep)
  real(dp),                     intent(out)   :: tangent(n)
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: status
  logical                                     :: ldomaxfcimage
  real(dp)                                    :: dVmax
  real(dp)                                    :: dVmin
  real(dp)                                    :: Eimax1
  real(dp)                                    :: Eimax2
  real(dp)                                    :: Eref
  real(dp)                                    :: Emax
  real(dp)                                    :: fdot
  real(dp)                                    :: gdot
  real(dp)                                    :: gpnorm
  real(dp),   dimension(:), allocatable       :: fp
  real(dp),   dimension(:), allocatable       :: fs
  real(dp),   dimension(:), allocatable       :: gp
  real(dp)                                    :: kneb
  real(dp)                                    :: kneb1
  real(dp)                                    :: kneb2
  real(dp)                                    :: tnorm
  real(dp)                                    :: tnorm1
  real(dp)                                    :: tnorm2
  real(dp)                                    :: vd1
  real(dp)                                    :: vd2
  real(dp)                                    :: xdiff
!
!  Allocate workspace array
!
  allocate(fs(n),stat=status)
  if (status/=0) call outofmemory('nebforce','fs')
!
!  Decide whether we need to do the climbing image special case on this call
!
  ldomaxfcimage = (lnebclimbingimage.and.nrp.eq.nrepwithmaxfc.and.niter.gt.4)
!
!  Calculate tangent 
!
!  1 => central finite difference between replicas
!  2 => bisect vectors 
!  3 => new algorithm favouring higher energy replica neighbour
!
  tnorm = 0.0_dp
  if (nebtangent.eq.1) then
    do i = 1,n
      xdiff = xcrep(i,nrp+1) - xcrep(i,nrp-1)
      tangent(i) = xdiff
      tnorm = tnorm + xdiff*xdiff
    enddo
  elseif (nebtangent.eq.2) then
    tnorm1 = 0.0_dp
    tnorm2 = 0.0_dp
    do i = 1,n
      xdiff = xcrep(i,nrp+1) - xcrep(i,nrp)
      tangent(i) = xdiff
      tnorm1 = tnorm1 + xdiff*xdiff
      xdiff = xcrep(i,nrp) - xcrep(i,nrp-1)
      fs(i) = xdiff
      tnorm2 = tnorm2 + xdiff*xdiff
    enddo
    tnorm1 = 1.0_dp/sqrt(tnorm1)
    tnorm2 = 1.0_dp/sqrt(tnorm2)
    do i = 1,n
      xdiff = tnorm1*tangent(i) + tnorm2*fs(i)
      tangent(i) = xdiff
      tnorm = tnorm + xdiff*xdiff
    enddo
  elseif (nebtangent.eq.3) then
!
!  New algorithm from JCP 113 9978 (2003)
!
    if (fcrep(nrp).gt.fcrep(nrp-1).and.fcrep(nrp).lt.fcrep(nrp+1)) then
!
!  Replica on slope : case 1
!
      do i = 1,n
        xdiff = xcrep(i,nrp+1) - xcrep(i,nrp)
        tangent(i) = xdiff
        tnorm = tnorm + xdiff*xdiff
      enddo
    elseif (fcrep(nrp).lt.fcrep(nrp-1).and.fcrep(nrp).gt.fcrep(nrp+1)) then
!
!  Replica on slope : case 2
!
      do i = 1,n
        xdiff = xcrep(i,nrp) - xcrep(i,nrp-1)
        tangent(i) = xdiff
        tnorm = tnorm + xdiff*xdiff
      enddo
    else
!
!  Replica is either a maxima or minima
!
      dVmax = max(abs(fcrep(nrp+1)-fcrep(nrp)),abs(fcrep(nrp-1)-fcrep(nrp)))
      dVmin = min(abs(fcrep(nrp+1)-fcrep(nrp)),abs(fcrep(nrp-1)-fcrep(nrp)))
      if (fcrep(nrp+1).gt.fcrep(nrp-1)) then
        vd1 = dVmax
        vd2 = dVmin
      else
        vd1 = dVmin
        vd2 = dVmax
      endif
      do i = 1,n
        xdiff = vd1*(xcrep(i,nrp+1) - xcrep(i,nrp)) + vd2*(xcrep(i,nrp) - xcrep(i,nrp-1))
        tangent(i) = xdiff
        tnorm = tnorm + xdiff*xdiff
      enddo
    endif
  endif
!
!  Normalise tangent
!
  if (tnorm.gt.1.0d-12) then
    tnorm = 1.0_dp/sqrt(tnorm)
  else
    tnorm = 0.0_dp
  endif
  do i = 1,n
    tangent(i) = tnorm*tangent(i)
  enddo
!
!  Calculate spring force constant contribution
!
  kneb = nebspring(ncf)
  if (lnebvaryspring(ncf)) then
    Eref = max(fcrep(1),fcrep(nrep))
    Emax = fcrep(nrepwithmaxfc)
    Eimax1 = max(fcrep(nrp+1),fcrep(nrp))
    Eimax2 = max(fcrep(nrp-1),fcrep(nrp))
    if (Eimax1.gt.Eref) then
      kneb1 = kneb - (kneb - nebspringmin(ncf))*(Emax - Eimax1)/(Emax - Eref)
    else
      kneb1 = nebspringmin(ncf)
    endif
    if (Eimax2.gt.Eref) then
      kneb2 = kneb - (kneb - nebspringmin(ncf))*(Emax - Eimax2)/(Emax - Eref)
    else
      kneb2 = nebspringmin(ncf)
    endif
  else
    kneb1 = kneb
    kneb2 = kneb
  endif
  do i = 1,n
    fs(i) = kneb1*sqrt((xcrep(i,nrp+1) - xcrep(i,nrp))**2) - kneb2*sqrt((xcrep(i,nrp-1) - xcrep(i,nrp))**2)
  enddo
!
!  Calculate projection of derivative vectors on to tangent
!
  fdot = 0.0_dp
  do i = 1,n
    fdot = fdot + gc(i)*tangent(i)
  enddo
  if (ldomaxfcimage) then
    do i = 1,n
      gc(i) = gc(i) - 2.0_dp*fdot*tangent(i) 
    enddo
  else
    allocate(fp(n),stat=status)
    if (status/=0) call outofmemory('nebforce','fp')
    allocate(gp(n),stat=status)
    if (status/=0) call outofmemory('nebforce','gp')
    gpnorm = 0.0_dp
    do i = 1,n
      gp(i) = gc(i) - fdot*tangent(i)
      gpnorm = gpnorm + gp(i)**2
    enddo
    gpnorm = sqrt(gpnorm)
!
!  Remove projection according to nudging expressions
!
    do i = 1,n
      gc(i) = gp(i) - fs(i)*tangent(i)
      fp(i) = fs(i) - fs(i)*tangent(i)
    enddo
    if (index(keyword,'dneb').ne.0) then
!
!  Doubly nudged elastic band 
!
      gdot = 0.0_dp
      do i = 1,n
        gp(i) = gp(i)/gpnorm
        gdot = gdot + fp(i)*gp(i)
      enddo
      do i = 1,n
        gc(i) = gc(i) + fp(i) - gdot*gp(i)
      enddo
    endif
    deallocate(gp,stat=status)
    if (status/=0) call deallocate_error('nebforce','gp')
    deallocate(fp,stat=status)
    if (status/=0) call deallocate_error('nebforce','fp')
  endif
!
!  Free workspace array
!
  deallocate(fs,stat=status)
  if (status/=0) call deallocate_error('nebforce','fs')
!
  return
  end
