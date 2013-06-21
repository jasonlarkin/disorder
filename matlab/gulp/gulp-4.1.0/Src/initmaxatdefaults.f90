  subroutine initmaxatdefaults(i)
!
!  Initialises the arrays associated with maxat
!
!   9/10 Created from changemaxat
!  10/11 icosx, icosy and icosz initialised here
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
!  Julian Gale, NRI, Curtin University, October 2011
!
  use configurations, only : maxcfg
  use control,        only : lcosmo
  use cosmo
  use cosmopwtloc,    only : npwtloc
  use current
  use derivatives,    only : nqatoms
  use eam,            only : maxmeamcomponent
  use montecarlo,     only : ltrialatom
  use neb
  use partial
  use potentialxyz
  use reaxFFdata,     only : qreaxFF
  use shell,          only : ratiom
  use shellextrapolation
  use velocities 
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: i
!
!  Local variables
!
  integer(i4)             :: j
!
!  Initialise data arrays
!
  if (i.ge.1.and.i.le.maxat) then
    icosx(i) = 0
    icosy(i) = 0
    icosz(i) = 0
    neamfnspecptr(i) = 0
    neamspecptr(i) = 0
    nqatoms(i) = 0
    do j = 1,maxbond
      nbonded(j,i) = 0
      nbondedtype(1:2,j,i) = 1
    enddo
    do j = 1,maxcfg
      nebfinalradius(i,j) = 0
    enddo
    do j = 1,maxnebreplicatot
      nebreplicaradius(i,j) = 0
    enddo
    if (lcosmo) then
      do j = 1,maxnppa
        npwtloc(j,i) = 0_i4
      enddo
    endif
    ltrialatom(i) = .false.
    bornq(1:3,1:3,i) = 0.0_dp
    ratiom(i) = 0.0_dp
    velx(i) = 0.0_dp
    vely(i) = 0.0_dp
    velz(i) = 0.0_dp
    qreaxFF(i) = 0.0_dp
  endif
!
  return
  end
