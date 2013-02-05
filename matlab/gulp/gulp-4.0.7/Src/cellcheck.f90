  subroutine cellcheck(nspcg,a,b,c,alpha,beta,gamma)
!
!   Checks that cell parameters are consistent with the space group specified.
!
!   7/11 Created.
!   7/12 C 1 & C -1 changed to triclinic from monoclinic
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
  implicit none
!
!  Passed variables
!
  integer(i4),                         intent(in)   :: nspcg
  real(dp),                            intent(in)   :: a
  real(dp),                            intent(in)   :: b
  real(dp),                            intent(in)   :: c
  real(dp),                            intent(in)   :: alpha
  real(dp),                            intent(in)   :: beta
  real(dp),                            intent(in)   :: gamma
!
!  Local variables
!
  integer(i4)                                       :: ictype
  logical                                           :: la90
  logical                                           :: lb90
  logical                                           :: lc90
  logical                                           :: lc120
  logical                                           :: laeqb
  logical                                           :: laeqc
  logical                                           :: laleqbe
  logical                                           :: laleqga
  logical                                           :: lcellok
!
!  Set indicator to cell type based on space group number
!
  if (nspcg.le.2) then
    ictype = 1
  elseif (nspcg.ge.3.and.nspcg.le.15) then
    ictype = 2
  elseif (nspcg.ge.16.and.nspcg.le.74) then
    ictype = 3
  elseif (nspcg.ge.75.and.nspcg.le.142) then
    ictype = 4
  elseif (nspcg.ge.143.and.nspcg.le.194) then
    ictype = 5
  elseif (nspcg.ge.195.and.nspcg.le.230) then
    ictype = 6
  elseif (nspcg.ge.231.and.nspcg.le.232) then
    ictype = 1
  endif
!
!  Check cell parameters are consistent
!
  lcellok = .false.
  la90 = (abs(90.0_dp-alpha).lt.1.0d-6)
  lb90 = (abs(90.0_dp-beta).lt.1.0d-6)
  lc90 = (abs(90.0_dp-gamma).lt.1.0d-6)
  lc120 = (abs(120.0_dp-gamma).lt.1.0d-6)
  laeqb = (abs(a-b).lt.1.0d-6)
  laeqc = (abs(a-c).lt.1.0d-6)
  laleqbe = (abs(alpha-beta).lt.1.0d-6)
  laleqga = (abs(alpha-gamma).lt.1.0d-6)
  if (ictype.eq.1) then
!
!  Triclinic - all cell parameters must be OK
!
    lcellok = .true.
  elseif (ictype.eq.2) then
!
!  Monoclinic
!
    lcellok = ((la90.and.lb90).or.(la90.and.lc90).or.(lb90.and.lc90))
  elseif (ictype.eq.3) then
!
!  Orthorhombic
!
    lcellok = (la90.and.lb90.and.lc90)
  elseif (ictype.eq.4) then
!
!  Tetragonal
!
    lcellok = ((la90.and.lb90.and.lc90).and.laeqb)
  elseif (ictype.eq.5) then
!
!  Hexagonal / rhombohedral
!
    lcellok = (((la90.and.lb90.and.lc120).and.laeqb).or. &
               (laleqbe.and.laleqga.and.laeqb.and.laeqc))
  elseif (ictype.eq.6) then
!
!  Cubic
!
    lcellok = ((la90.and.lb90.and.lc90).and.laeqb.and.laeqc)
  endif
  if (.not.lcellok) then
!
!  If cell is not OK, stop with error message
!
    call outerror('input cell is inconsistent with the space group specified',0_i4)
    call stopnow('cellcheck')
  endif
!
  return
  end
