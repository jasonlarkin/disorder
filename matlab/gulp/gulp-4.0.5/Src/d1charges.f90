  subroutine d1charges(i,j,lopi,nor,derive0)
!
!  Calculates the contribution to the first derivatives from the bond
!  order charge derivatives. Symmetry adapted version. Note, here only
!  i is specified and j is assumed to be handled prior to call. This
!  makes for general compatability with the reciprocal space form.
!
!  NOTE: This routine does not yet work and should not be called!!!!!!
!
!   9/04 Created from d1charge
!   5/12 Atomic stress added
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use control,        only : latomicstress
  use current,        only : nstrains, nrelat, nrel2, neqv, qa, qf
  use derivatives
  use symmetry,       only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
  integer(i4), intent(in) :: j
  integer(i4), intent(in) :: nor
  logical,     intent(in) :: lopi
  real(dp),    intent(in) :: derive0(*)
!
!  Local variables
!
  integer(i4)             :: ii
  integer(i4)             :: ix
  integer(i4)             :: iy
  integer(i4)             :: iz
  integer(i4)             :: k
  integer(i4)             :: ka
  integer(i4)             :: kl
  integer(i4)             :: kx
  integer(i4)             :: ky
  integer(i4)             :: kz
  integer(i4)             :: n
  real(dp)                :: qpotsum
!
!  Loop over distances collecting total Coulomb potential
!
  qpotsum = 0.0_dp
  do n = 1,nor
    qpotsum = qpotsum + derive0(n)
  enddo
!
!  Derivatives for atom i
!
  ii = nrel2(i)
  if (lopi) then
    ix = 3*(ii - 1) + 1
    iy = ix + 1
    iz = iy + 1
    xdrv(i) = xdrv(i) + qpotsum*dqdxyz(ix,ii)*qf(j)
    ydrv(i) = ydrv(i) + qpotsum*dqdxyz(iy,ii)*qf(j)
    zdrv(i) = zdrv(i) + qpotsum*dqdxyz(iz,ii)*qf(j)
    do n = 1,nqatoms(j)
      k = nqatomptr(n,j)
      ka = nrelat(k)
      if (nrel2(ka).eq.k.and.ka.eq.i) then
        kx = 3*(k - 1) + 1
        ky = kx + 1
        kz = ky + 1
        xdrv(i) = xdrv(i) + qpotsum*dqdxyz(kx,j)*qa(i)
        ydrv(i) = ydrv(i) + qpotsum*dqdxyz(ky,j)*qa(i)
        zdrv(i) = zdrv(i) + qpotsum*dqdxyz(kz,j)*qa(i)
      endif
    enddo
  endif
  do n = 1,nqatoms(ii)
    k = nqatomptr(n,ii)
    ka = nrelat(k)
    if (nrel2(ka).eq.k) then
      kx = 3*(k - 1) + 1
      ky = kx + 1
      kz = ky + 1
      xdrv(k) = xdrv(k) + qpotsum*dqdxyz(kx,ii)*qf(j)*dble(neqv(ka))
      ydrv(k) = ydrv(k) + qpotsum*dqdxyz(ky,ii)*qf(j)*dble(neqv(ka))
      zdrv(k) = zdrv(k) + qpotsum*dqdxyz(kz,ii)*qf(j)*dble(neqv(ka))
    endif
  enddo
!
!  Strain derivatives
!
  if (lstr) then
    do kl = 1,nstrains
      rstrd(kl) = rstrd(kl) + qpotsum*dqds(kl,ii)
    enddo
    if (latomicstress) then
      do kl = 1,nstrains
        atomicstress(kl,i) = atomicstress(kl,i) + qpotsum*dqds(kl,ii)
      enddo
    endif
  endif
!
  return
  end
