  subroutine eamscalescrho(imode)
!
!  Scales the bulk densities in the EAM method according to the
!  alloy variables. With imode >= 0 the scale factor is divided
!  while if imode < 0 the scale factor is multiplied.
!
!  11/03 Created
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
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
!  Julian Gale, NRI, Curtin University, December 2008
!
  use current
  use eam
  use sutton
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: imode
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: nati
  integer(i4)                                  :: ntypi
  logical                                      :: lfound
!
!  Loop over bulk atoms
!
  do i = 1,nasym
    nati = nat(i)
    ntypi = nftype(i)
!
!  Find valid EAM species for each atom
!
    j = 0
    lfound = .false.
    do while (j.lt.neamspec.and..not.lfound)
      j = j + 1
      if (nati.eq.neamnat(j).and.(ntypi.eq.neamtyp(j).or.neamtyp(j).eq.0)) then
        lfound = .true.
      endif
    enddo
!
!  If found, scale by inverse of scaling factor
!
    if (lfound) then
      if (lMEAM) then
        if (imode.ge.0) then
          scrho(1:maxmeamcomponent,i) = scrho(1:maxmeamcomponent,i)/eamalloy(1,j)**2
        else
          scrho(1:maxmeamcomponent,i) = scrho(1:maxmeamcomponent,i)*eamalloy(1,j)**2
        endif
      else
        if (imode.ge.0) then
          scrho(1,i) = scrho(1,i)/eamalloy(1,j)**2
        else
          scrho(1,i) = scrho(1,i)*eamalloy(1,j)**2
        endif
      endif
    endif
  enddo
!
  return
  end
!
  subroutine eamscaledscrho(imode)
!
!  Scales the defect region densities in the EAM method according to
!  the alloy variables. With imode >= 0 the scale factor is divided
!  while if imode < 0 the scale factor is multiplied.
!
!  11/03 Created
!  11/08 scrho changed to be square of density for benefit of MEAM
!  12/08 scrho, and defect versions, converted to 2-D array for MEAM
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
!  Julian Gale, NRI, Curtin University, December 2008
!
  use current
  use defects
  use eam
  use region2a
  use sutton
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: imode
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: nati
  integer(i4)                                  :: npsi
  integer(i4)                                  :: ntypi
  logical                                      :: lfound
!
!  Loop over region 1 atoms
!
  do i = 1,nreg1
    nati = natdefe(i)     
    ntypi = ntypdefe(i)
!
!  Find valid EAM species for each atom
!
    j = 0
    lfound = .false.
    do while (j.lt.neamspec.and..not.lfound)
      j = j + 1
      if (nati.eq.neamnat(j).and.(ntypi.eq.neamtyp(j).or.neamtyp(j).eq.0)) then
        lfound = .true.
      endif
    enddo
!
!  If found, scale by inverse of scaling factor
!
    if (lfound) then
      if (lMEAM) then
        if (imode.ge.0) then
          dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i)/eamalloy(1,j)
        else
          dscrho(1:maxmeamcomponent,i) = dscrho(1:maxmeamcomponent,i)*eamalloy(1,j)
        endif
      else
        if (imode.ge.0) then
          dscrho(1,i) = dscrho(1,i)/eamalloy(1,j)
        else
          dscrho(1,i) = dscrho(1,i)*eamalloy(1,j)
        endif
      endif
    endif
  enddo
!
!  Loop over region 2 atoms
!
  do i = 1,ndpasym2a     
    npsi = nps(ndsptr2a(i))
    nati = nat(npsi)
    ntypi = nftype(npsi)
!
!  Find valid EAM species for each atom
!
    j = 0
    lfound = .false.
    do while (j.lt.neamspec.and..not.lfound)
      j = j + 1
      if (nati.eq.neamnat(j).and.(ntypi.eq.neamtyp(j).or.neamtyp(j).eq.0)) then
        lfound = .true.
      endif
    enddo
!
!  If found, scale by inverse of scaling factor
!
    if (lfound) then
      if (lMEAM) then
        if (imode.ge.0) then
          dscrhor2d(1:maxmeamcomponent,i) = dscrhor2d(1:maxmeamcomponent,i)/eamalloy(1,j)
        else
          dscrhor2d(1:maxmeamcomponent,i) = dscrhor2d(1:maxmeamcomponent,i)*eamalloy(1,j)
        endif
      else
        if (imode.ge.0) then
          dscrhor2d(1,i) = dscrhor2d(1,i)/eamalloy(1,j)
        else
          dscrhor2d(1,i) = dscrhor2d(1,i)*eamalloy(1,j)
        endif
      endif
    endif
  enddo
!
  return
  end
