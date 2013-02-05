  subroutine changemaxregion
!
!  Alters the size of the arrays associated with maxregion
!
!   9/03 lregionrigid flag added
!   5/07 nregiontype added
!  11/09 Arrays for derivatives on regions added
!   9/10 Initialisations now performed in a subroutine
!  11/11 eregion2region added
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
!  Julian Gale, NRI, Curtin University, November 2011
!
  use configurations, only : lopfreg, lregionrigid, maxcfg, maxregion, nregiontype
  use derivatives
  use energies,       only : eregion2region
  use reallocate
  implicit none
!
!  Local variables
!  
  integer(i4)       :: ierror, i
  integer(i4), save :: oldmaxregion = 0
!
  call realloc(lopfreg,3_i4*maxregion,maxcfg,ierror)
  if (ierror.ne.0) call outofmemory('changemaxregion','lopfreg')
  call realloc(lregionrigid,maxregion,maxcfg,ierror)
  if (ierror.ne.0) call outofmemory('changemaxregion','lregionrigid')
  call realloc(nregiontype,maxregion,maxcfg,ierror)
  if (ierror.ne.0) call outofmemory('changemaxregion','nregiontype')
!
!  Energies interaction of regions
!
  call realloc(eregion2region,maxregion,maxregion,ierror)
  if (ierror.ne.0) call outofmemory('changemaxregion','eregion2region')
!
!  Derivatives of regions
!
  call realloc(xregdrv,maxregion,ierror)
  if (ierror.ne.0) call outofmemory('changemaxregion','xregdrv')
  call realloc(yregdrv,maxregion,ierror)
  if (ierror.ne.0) call outofmemory('changemaxregion','yregdrv')
  call realloc(zregdrv,maxregion,ierror)
  if (ierror.ne.0) call outofmemory('changemaxregion','zregdrv')
!
!  Initialise new parts of data arrays
!  
  if (maxregion.gt.oldmaxregion) then
    do i = oldmaxregion+1,maxregion
      call initmaxregiondefaults(i)
    enddo
  endif
!
!  Save current value of maxregion for next call
!
  oldmaxregion = maxregion
!
  return
  end
