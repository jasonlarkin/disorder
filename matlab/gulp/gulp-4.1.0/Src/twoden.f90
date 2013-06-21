  subroutine twoden(nor,nor0,npots,npotl,sctrm1,sctrm2,lorder12)
!
!  Subroutine for calculating twobody density between a pair of atoms
!
!  nor     = pointer to final distance to calculate for
!  nor0    = pointer to first distance to calculate for
!  dist    = array of distances from nor0 to nor
!  npots   = no. of valid potentials for this pair of atoms
!  npotl   = array of pointers to valid potentials
!  sctrm1/2= contribution to many-body rho value for EAM
!  lorder12= if .true. then order is i-j as expected, but if false then i & j are switched
!
!   4/09 Created from twobody
!   5/09 MEAM third derivative arguments removed
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
!  Julian Gale, NRI, Curtin University, May 2009
!
  use constants
  use control
  use eam
  use realvectors
  use two
  implicit none
!
!  Passed variables
!
  integer(i4),    intent(in)      :: nor
  integer(i4),    intent(in)      :: nor0
  integer(i4),    intent(in)      :: npotl(*)
  integer(i4),    intent(in)      :: npots
  logical,        intent(in)      :: lorder12
  real(dp),       intent(inout)   :: sctrm1(maxmeamcomponent)
  real(dp),       intent(inout)   :: sctrm2(maxmeamcomponent)
!
!  Local variables
!
  integer(i4)                     :: k
  integer(i4)                     :: m
  integer(i4)                     :: npot
  integer(i4)                     :: nptyp
  real(dp)                        :: drhoij
  real(dp)                        :: drhoji
  real(dp)                        :: drhoijs
  real(dp)                        :: drhojis
  real(dp)                        :: drhoij2
  real(dp)                        :: drhoji2
  real(dp)                        :: drhoij2s
  real(dp)                        :: drhoji2s
  real(dp)                        :: drhoij2m
  real(dp)                        :: drhoji2m
  real(dp)                        :: drhoij3
  real(dp)                        :: drhoji3
  real(dp)                        :: r
  real(dp)                        :: rhoij(maxmeamcomponent)
  real(dp)                        :: rhoji(maxmeamcomponent)
!
  if (lMEAMden) then
    sctrm1(1:maxmeamcomponent) = 0.0_dp
    sctrm2(1:maxmeamcomponent) = 0.0_dp
  else
    sctrm1(1) = 0.0_dp
    sctrm2(1) = 0.0_dp
  endif
!*****************************
!  Main loop over distances  *
!*****************************
  do k = nor0,nor
    r = dist(k)
!**************************************************
!  Loop over potentials to find relevant density  *
!**************************************************
    do m = 1,npots
      npot = npotl(m)
      if (r.gt.rpot2(npot).and.r.le.rpot(npot)) then
        nptyp = nptype(npot)
        if (nptyp.eq.19) then
!************************
!  Many-body potential  *
!************************
          if (lMEAMden) then
            rhoij(1:maxmeamcomponent) = 0.0_dp
            rhoji(1:maxmeamcomponent) = 0.0_dp
            call meamrho(nspec1(npot),nptyp1(npot),nspec2(npot),nptyp2(npot),r,rpot(npot),xtmp(k),ytmp(k),ztmp(k), &
                         rhoij,rhoji,drhoij,drhoji,drhoijs,drhojis,drhoij2,drhoji2,drhoij2s,drhoji2s,drhoij2m,drhoji2m, &
                         1.0_dp,1.0_dp,lorder12,.false.,.false.,.false.)
            sctrm1(1:maxmeamcomponent) = sctrm1(1:maxmeamcomponent) + rhoij(1:maxmeamcomponent)
            sctrm2(1:maxmeamcomponent) = sctrm2(1:maxmeamcomponent) + rhoji(1:maxmeamcomponent)
          else
            rhoij(1) = 0.0_dp
            rhoji(1) = 0.0_dp
            call eamrho(nspec1(npot),nptyp1(npot),nspec2(npot),nptyp2(npot),r,rpot(npot),xtmp(k),ytmp(k),ztmp(k), &
                        rhoij,rhoji,drhoij,drhoji,drhoijs,drhojis,drhoij2,drhoji2,drhoij2s,drhoji2s,drhoij2m,drhoji2m, &
                        drhoij3,drhoji3,1.0_dp,1.0_dp,.false.,.false.,.false.,.false.)
            sctrm1(1) = sctrm1(1) + rhoij(1)
            sctrm2(1) = sctrm2(1) + rhoji(1)
          endif
        endif
      endif
    enddo
!*********************
!  Taper potentials  *
!*********************
!
!  Add tapering of density here from two-body part
!
  enddo
!
  return
  end
!*******************************************************************
!  Wrapper routine for twobody                                     *
!*******************************************************************
  subroutine twoden1(nor,nor0,dist1,x1,y1,z1,npots,npotl,sctrm1,sctrm2,lorder12)
!
!  Wrapper for calling twoden when nor = 1 and arguments
!  being passed are not those in the modules.
!
!   4/09 Created from twobody
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
!  Julian Gale, NRI, Curtin University, April 2009
!
  use realvectors
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)     :: nor
  integer(i4), intent(in)     :: nor0
  integer(i4), intent(in)     :: npotl(*)
  integer(i4), intent(in)     :: npots
  logical,     intent(in)     :: lorder12
  real(dp),    intent(inout)  :: dist1
  real(dp),    intent(out)    :: sctrm1(*)
  real(dp),    intent(out)    :: sctrm2(*)
  real(dp),    intent(in)     :: x1
  real(dp),    intent(in)     :: y1
  real(dp),    intent(in)     :: z1
!
!  Assign incoming variables to appropriate module values
!
  dist(1)     = dist1
  xtmp(1)     = x1
  ytmp(1)     = y1
  ztmp(1)     = z1
!
!  Call real twobody routine
!
  call twoden(nor,nor0,npots,npotl,sctrm1,sctrm2,lorder12)
!
!  Assign outgoing variables from appropriate module values
!
  dist1 = dist(1)
!
  return
  end
