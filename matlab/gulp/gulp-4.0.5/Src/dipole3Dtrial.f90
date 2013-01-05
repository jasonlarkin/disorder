  subroutine dipole3Dtrial(edipole,ntrialatom,nptrtrialatom)
!
!  Subroutine for dipole correction energy for subset of atoms
!
!   1/08 Created from dipole3D
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, January 2008
!
  use constants
  use current
  use mdlogic
  use molecule
  use optimisation
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: ntrialatom
  integer(i4), intent(in)    :: nptrtrialatom(ntrialatom)
  real(dp),    intent(out)   :: edipole
!
!  Local variables
!
  integer(i4)                :: i
  integer(i4)                :: indm
  integer(i4)                :: ix
  integer(i4)                :: iy
  integer(i4)                :: iz
  integer(i4)                :: nmi
  integer(i4)                :: nt
  real(dp)                   :: const
  real(dp)                   :: dip2
  real(dp)                   :: dx
  real(dp)                   :: dy
  real(dp)                   :: dz
  real(dp)                   :: q
  real(dp)                   :: qi
  real(dp)                   :: rvol
  real(dp)                   :: vol
  real(dp)                   :: volume
  real(dp)                   :: xi
  real(dp)                   :: yi
  real(dp)                   :: zi
!
!  Check dimensionality
!
  if (ndim.eq.2) then
    call outerror('dipole correction not available for surface',0_i4)
    call stopnow('dipole')
  elseif (ndim.eq.1) then
    call outerror('dipole correction not available for polymer',0_i4)
    call stopnow('dipole')
  endif
!
  vol = volume(rv)
  rvol = 1.0_dp/vol
  const = 2.0_dp*pi*angstoev*rvol/3.0_dp
!
!  Calculate charge and dipole moment per unit cell
!
  q = 0.0_dp
  dx = 0.0_dp
  dy = 0.0_dp
  dz = 0.0_dp
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    qi = qf(i)*occuf(i)
    q = q + qi
    nmi = natmol(i)
    if (lmol.and.nmi.ne.0) then
!
!  If molecules are present that make sure that images
!  belong to the same molecule are used to ensure a
!  consistent dipole moment
!
      indm = nmolind(i)
      call mindtoijk(indm,ix,iy,iz)
      xi = xclat(i) + ix*rv(1,1) + iy*rv(2,1) + iz*rv(3,1)
      yi = yclat(i) + ix*rv(1,2) + iy*rv(2,2) + iz*rv(3,2)
      zi = zclat(i) + ix*rv(1,3) + iy*rv(2,3) + iz*rv(3,3)
      dx = dx + qi*xi
      dy = dy + qi*yi
      dz = dz + qi*zi
    else
      dx = dx + qi*xclat(i)
      dy = dy + qi*yclat(i)
      dz = dz + qi*zclat(i)
    endif
  enddo
  if (abs(q).gt.1.0d-8) then
    call outwarning('Dipole correction is origin dependent as charge is not zero',0_i4)
  endif
  dip2 = dx*dx + dy*dy + dz*dz
!
!  If dipole is zero then we are finished
!
  if (dip2.eq.0.0_dp) then
    edipole = 0.0_dp
    return
  endif
!
!  Calculate energy
!
  edipole = const*dip2
!
  return
  end
