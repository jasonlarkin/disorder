  subroutine property0(lprint)
!
!  Calculate the properties of clusters
!
!  10/05 Created
!   5/06 Mass now set using species values
!   8/06 Missing occupancy factor in inertia calculation added
!  12/07 Unused variables removed
!   1/09 Integer datatypes all explicitly declared
!   2/09 Modified to accommodate new version of FoX and gulp_cml
!  12/10 Wrong spelling of principal corrected
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
!  Julian Gale, NRI, Curtin University, December 2010
!
  use constants,      only : avogadro
  use current
  use element
  use gulp_cml,       only : lcml
  use gulp_cml_props, only : gulp_cml_output_inertia
  use iochannels
  use parallel
  use shell,          only : ncore
  use species,        only : massspec
  use times
  implicit none
!
!  Passed variables
!
  logical                                        :: lprint
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ifail
  integer(i4)                                    :: j
  real(dp)                                       :: cputime
  real(dp)                                       :: dx
  real(dp)                                       :: dy
  real(dp)                                       :: dz
  real(dp)                                       :: inertia(3,3)
  real(dp)                                       :: mi
  real(dp)                                       :: r3(3)
  real(dp)                                       :: r33(3,3)
  real(dp)                                       :: t1p
  real(dp)                                       :: t2p
  real(dp)                                       :: totalmass
  real(dp)                                       :: w1l(9)
  real(dp)                                       :: xcom
  real(dp)                                       :: ycom
  real(dp)                                       :: zcom
!
  t1p = cputime()
!*****************************
!  Moment of inertia tensor  *
!*****************************
!
!  Initialise tensor
!
  inertia(1:3,1:3) = 0.0_dp
!
!  Find centre of mass
!
  xcom = 0.0_dp
  ycom = 0.0_dp
  zcom = 0.0_dp
  totalmass = 0.0_dp
  do i = 1,ncore
    mi = occuf(i)*massspec(nspecptr(i))
    xcom = xcom + xclat(i)*mi
    ycom = ycom + yclat(i)*mi
    zcom = zcom + zclat(i)*mi
    totalmass = totalmass + mi
  enddo
  xcom = xcom/totalmass
  ycom = ycom/totalmass
  zcom = zcom/totalmass
!
!  Calculate tensor
!
  do i = 1,numat
    mi = occuf(i)*massspec(nspecptr(i))
    dx = xclat(i) - xcom
    dy = yclat(i) - ycom
    dz = zclat(i) - zcom
    inertia(1,1) = inertia(1,1) + mi*dy*dy + mi*dz*dz
    inertia(2,1) = inertia(2,1) - mi*dx*dy
    inertia(3,1) = inertia(3,1) - mi*dx*dz
    inertia(1,2) = inertia(1,2) - mi*dy*dx
    inertia(2,2) = inertia(2,2) + mi*dx*dx + mi*dz*dz
    inertia(3,2) = inertia(3,2) - mi*dy*dz
    inertia(1,3) = inertia(1,3) - mi*dz*dx
    inertia(2,3) = inertia(2,3) - mi*dz*dy
    inertia(3,3) = inertia(3,3) + mi*dx*dx + mi*dy*dy
  enddo
!
!  Convert units to 10**-46 x kg.m**2
!
  inertia(1:3,1:3) = inertia(1:3,1:3)*1.0d23/avogadro
!
!  Calculate diagonalised tensor components
!
  do i = 1,3
    do j = 1,3
      r33(j,i) = inertia(j,i)
    enddo
  enddo
  call dsyev('N','U',3_i4,r33,3_i4,r3,w1l,9_i4,ifail)
!**********************
!  Output properties  *
!**********************
  if (lprint.and.ioproc) then
    write(ioout,'(/)')
    write(ioout,'(''  Moment of inertia tensor (10^-46 x kgm^2): '',/)')
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    write(ioout,'(10x,6x,''x'',11x,''y'',11x,''z'',11x,''Principal axis system'')')
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    write(ioout,'(7x,''x'',2x,3f12.2,2x,f18.6)')(inertia(1,i),i=1,3),r3(1)
    write(ioout,'(7x,''y'',2x,3f12.2,2x,f18.6)')(inertia(2,i),i=1,3),r3(2)
    write(ioout,'(7x,''z'',2x,3f12.2,2x,f18.6)')(inertia(3,i),i=1,3),r3(3)
    write(ioout,'(''-------------------------------------------------------------------------------'')')
    write(ioout,'(''  Centre of mass (Ang) = '',3(f12.6,1x))') xcom,ycom,zcom
    write(ioout,'(''-------------------------------------------------------------------------------'',/)')
    if (lcml) call gulp_cml_output_inertia(inertia)
    call gflush(ioout)
  endif
!***************
!  Exit tasks  *
!***************
!
!  Timings
!
  t2p = cputime()
  tprop = t2p - t1p + tprop
!
  return
  end
