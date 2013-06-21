  subroutine outdener
!
!  Output energy components for defect calculation
!
!  11/03 Bond order energy added in
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, July 2005
!
  use defects
  use energies
  use iochannels
  implicit none
!
!  Local variables
!
  real(dp)         :: edefect
!
  edefect = e12a - e12aold + e11 - e11old + e12t - e12told + e2a + e2b + e12ad - &
            e12ap + e12f - e12fold + e12m - e12mold + e12b - e12bold + e12bo - e12boold
  write(ioout,'(/,''  Components of defect energy : '',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Region 1 - region 1                   = '',f15.8,'' eV'')') e11 - e11old
  write(ioout,'(''  Region 1 - region 2a (unrelaxed)      = '',f15.8,'' eV'')') e12a - e12aold
  if (e12t.gt.0.0_dp.or.e12told.gt.0.0_dp) then
  write(ioout,'(''  Three body defect energy              = '',f15.8,'' eV'')') e12t - e12told
  endif
  if (e12f.gt.0.0_dp.or.e12fold.gt.0.0_dp) then
  write(ioout,'(''  Four body defect energy               = '',f15.8,'' eV'')') e12f - e12fold
  endif
  if (e12m.ne.0.0_dp.or.e12mold.ne.0.0_dp) then
  write(ioout,'(''  Many body defect energy               = '',f15.8,'' eV'')') e12m - e12mold
  endif
  if (e12b.gt.0.0_dp.or.e12bold.gt.0.0_dp) then
  write(ioout,'(''  Brenner defect energy                 = '',f15.8,'' eV'')') e12b - e12bold
  endif
  if (e12bo.gt.0.0_dp.or.e12boold.gt.0.0_dp) then
  write(ioout,'(''  Bond order  defect energy             = '',f15.8,'' eV'')') e12bo - e12boold
  endif
  write(ioout,'(''  Region 1 - 2a (relaxed - correction)  = '',f15.8,'' eV'')') e12ad - e12ap
  write(ioout,'(''  Region 1 (Total)                      = '',f15.8,'' eV'')') &
    e12a - e12aold + e11 - e11old + e12t - e12told + e12ad - e12ap + e12f - e12fold + e12m - e12mold +  &
    e12b - e12bold + e12bo - e12boold
  write(ioout,'(''  Region 2a                             = '',f15.8,'' eV'')') e2a
  write(ioout,'(''  Region 2b                             = '',f15.8,'' eV'')') e2b
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Total defect energy                   = '',f15.8,'' eV'')') edefect
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  if (r2dmax.gt.0.0_dp) then
    write(ioout,'(/,''  Largest displacement in region 2 = '',f8.4,'' Angstroms'')') r2dmax
  endif
!
  return
  end
