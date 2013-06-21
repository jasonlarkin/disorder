  subroutine outtime
!
!  Output timing information
!
!   7/01 Alignment of output altered
!   9/02 time0 subtracted from ttot for benefit of Intel compiler
!   1/05 Timing for search phase added
!   7/06 Six-body potential timing added
!   9/07 ReaxFF timing added
!   1/08 MC timing added
!  10/08 COSMO timing added
!   5/10 Molecule timing added
!   9/10 Phonon timing added
!   9/10 EDIP timing added
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
!  Julian Gale, NRI, Curtin University, May 2010
!
  use iochannels
  use cosmo,     only : lcosmic
  use general,   only : time0
  use times
  implicit none
!
!  Local variables
!
  real(dp)       :: cputime
  real(dp)       :: ttot
!***************************
!  Output timing analysis  *
!***************************
  write(ioout,'(/,''  Timing analysis for GULP :'',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Task / Subroutine                                          Time (Seconds)'')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  if (tion.gt.1d-4) write(ioout,'(''  Calculation of reciprocal space energy and derivatives'',f16.4)')tion
  if (tres.gt.1d-4) write(ioout,'(''  Calculation of reciprocal space energy using symmetry '',f16.4)')tres
  if (tatom.gt.1d-4) write(ioout,'(''  Calculation of real space energy and derivatives      '',f16.4)')tatom
  if (trls.gt.1d-4) write(ioout,'(''  Calculation of real space energy using symmetry       '',f16.4)')trls
  if (tthree.gt.1d-4) then
    write(ioout,'(''  Calculation of three-body energy and derivatives      '',f16.4)')tthree
  endif
  if (tfour.gt.1d-4) then
    write(ioout,'(''  Calculation of four-body energy and derivatives       '',f16.4)')tfour
  endif
  if (tsix.gt.1d-4) then
    write(ioout,'(''  Calculation of six-body energy and derivatives        '',f16.4)')tsix
  endif
  if (tmany.gt.1d-4) then
    write(ioout,'(''  Calculation of many-body energy and derivatives       '',f16.4)')tmany
  endif
  if (tbondorder.gt.1d-4) then
    write(ioout,'(''  Calculation of Bond Order energy and derivatives      '',f16.4)')tbondorder
  endif
  if (tbrenner.gt.1d-4) then
    write(ioout,'(''  Calculation of Brenner energy and derivatives         '',f16.4)')tbrenner
  endif
  if (treaxff.gt.1d-4) then
    write(ioout,'(''  Calculation of reaxFF energy and derivatives          '',f16.4)')treaxFF
  endif
  if (tedip.gt.1d-4) then
    write(ioout,'(''  Calculation of EDIP energy and derivatives            '',f16.4)')tedip
  endif
  if (tmol.gt.1d-4) then
    write(ioout,'(''  Calculation of molecules and connectivity             '',f16.4)')tmol
  endif
  if (tsearch.gt.1d-4) then
    write(ioout,'(''  Search for valid interatomic vectors                  '',f16.4)')tsearch
  endif
  if (tfederiv.gt.1d-4) then
    write(ioout,'(''  Calculation of free energy and derivatives            '',f16.4)')tfederiv
  endif
  if (tphon.gt.1d-4) then
    write(ioout,'(''  Calculation of phonons                                '',f16.4)')tphon
  endif
  if (tphon.gt.1d-4) then
    write(ioout,'(''  Calculation of scattering                             '',f16.4)')tscatter
  endif
  if (treg1.gt.1d-4) then
    write(ioout,'(''  Calculation for region 1 energy and derivatives (2-b) '',f16.4)')treg1
  endif
  if (treg3.gt.1d-4) then
    write(ioout,'(''  Calculation for region 1 energy and derivatives (3-b) '',f16.4)')treg3
  endif
  if (treg4.gt.1d-4) then
    write(ioout,'(''  Calculation for region 1 energy and derivatives (4-b) '',f16.4)')treg4
  endif
  if (tregm.gt.1d-4) then
    write(ioout,'(''  Calculation for region 1 energy and derivatives (m-b) '',f16.4)')tregm
  endif
  if (treg2a.gt.1d-4) then
    write(ioout,'(''  Calculation for region 2a energy                      '',f16.4)')treg2a
  endif
  if (treg2b.gt.1d-4) then
    write(ioout,'(''  Calculation for region 2b energy                      '',f16.4)')treg2b
  endif
  if (tcosmo.gt.1d-4) then
    if (lcosmic) then
      write(ioout,'(''  Calculation of COSMIC solvation energy                '',f16.4)')tcosmo
    else
      write(ioout,'(''  Calculation of COSMO solvation energy                 '',f16.4)')tcosmo
    endif
  endif
  if (tcosmoderv.gt.1d-4) then
    if (lcosmic) then
      write(ioout,'(''  Calculation of COSMIC solvation derivatives           '',f16.4)')tcosmoderv
    else
      write(ioout,'(''  Calculation of COSMO solvation derivatives            '',f16.4)')tcosmoderv
    endif
  endif
  if (tmati.gt.1d-4) then
    write(ioout,'(''  Calculation of matrix inversion                       '',f16.4)')tmati
  endif
  if (tdisk.gt.1d-4) then
    write(ioout,'(''  Disk read/write operations to scratch files           '',f16.4)')tdisk
  endif
  if (tmc.gt.1d-4) then
    write(ioout,'(''  Monte Carlo operations excluding energy               '',f16.4)')tmc
  endif
  if (teem.gt.1d-4) then
    write(ioout,'(''  Electronegativity equalisation                        '',f16.4)')teem
  endif
  if (tfitf.gt.1d-4) then
    write(ioout,'(''  Sum of squares for fitting                            '',f16.4)')tfitf
  endif
  if (tsym.gt.1.0d-4) then
    write(ioout,'(''  Symmetry generation of equivalent positions           '',f16.4)')tsym
  endif
  if (tsum.gt.1d-4) then
    write(ioout,'(''  Global summation overhead                             '',f16.4)')tsum
  endif
!
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  ttot = cputime() - time0
  write(ioout,'(''  Total CPU time                                        '',f16.4)')ttot
  write(ioout,'(''--------------------------------------------------------------------------------'')')
!
  return
  end
