  subroutine celltype(itype,icfhr)
!
!  Determines what class of cell is applicable based on parameters
!
!   9/04 Rewritten to use the space group to determine the values
!        rather than just the cell parameters, since the latter can
!        be misleading
!  10/04 Correction for non-standard space groups (> 230) added
!   8/05 Modified to handle user specified cell types
!   7/12 C 1 & C -1 changed to triclinic from monoclinic
!
!  On return:
!
!  itype = 1 => triclinic
!        = 2 => monoclinic
!        = 3 => orthorhombic
!        = 4 => tetragonal
!        = 5 => icfhr = 0 => hexagonal
!               icfhr = 1 => trigonal
!        = 6 => cubic
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
!  Julian Gale, NRI, Curtin University, July 2012
!
  use datatypes
  use current,   only : ncf
  use symmetry,  only : nspcg, nccs, ifhr
  implicit none
!
!  Passed variables
! 
  integer(i4), intent(out) :: icfhr
  integer(i4), intent(out) :: itype
!
!  Local variables     
!
  integer(i4)              :: nspg
!
  nspg = nspcg(ncf)
  icfhr = ifhr(ncf)
  if (nspg.eq.1.and.nccs.gt.1) then
!
!  Use user specified cell symmetry to determine cell type
!
    itype = nccs
  else
!
!  Use space groups to determine the cell type
!
    if (nspg.le.2) then
      itype = 1
    elseif (nspg.ge.3.and.nspg.le.15) then
      itype = 2
    elseif (nspg.ge.16.and.nspg.le.74) then
      itype = 3
    elseif (nspg.ge.75.and.nspg.le.142) then
      itype = 4
    elseif (nspg.ge.143.and.nspg.le.194) then
      itype = 5
    elseif (nspg.ge.195.and.nspg.le.230) then
      itype = 6
    elseif (nspg.ge.231.and.nspg.le.232) then
      itype = 1
    endif
    if (nccs.ne.5) icfhr = 0
  endif
!
  return
  end
