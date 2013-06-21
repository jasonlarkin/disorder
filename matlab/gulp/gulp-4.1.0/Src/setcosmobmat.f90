  subroutine setcosmobmat
!
!  Subroutine generates the SAS - atom Coulomb matrix for the COSMO model
!
!  Adapted from the routine from MOPAC7 by A. Klamt
!
!   4/03 Modified to ensure charge neutrality of the SAS when requested
!   4/03 cosmoD matrix eliminated
!   5/03 Segment weighting introduced
!  11/04 sasparticle arrays now used instead of numat
!  12/04 cosmoBq matrix created for general use
!  12/04 Computation of q on SAS removed into separate routine
!   1/05 cosmoB matrix eliminated
!  10/08 Converted to f90 format
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
!  Julian Gale, NRI, Curtin University, October 2008
!
  use constants, only : angstoev
  use control,   only : ldebug, keyword
  use cosmo
  use current
  use iochannels
  use parallel,  only : ioproc
  implicit none
!
!  Local variables
!
  integer(i4)                                 :: i
  integer(i4)                                 :: ipts
  integer(i4)                                 :: n
  real(dp)                                    :: qme
  real(dp)                                    :: dqme(3)
  real(dp)                                    :: d2qme(6)
  real(dp)                                    :: qi
  real(dp)                                    :: xd
  real(dp)                                    :: yd
  real(dp)                                    :: zd
  real(dp)                                    :: xi
  real(dp)                                    :: yi
  real(dp)                                    :: zi
!
!  Build potential due to particles on SAS in cosmoBq
!
  cosmoBq(1:npts) = 0.0_dp
  do n = 1,nsasparticles
    i = nsasparticleptr(n)
    qi = qsasparticles(n)
    xi = xclat(i)
    yi = yclat(i)
    zi = zclat(i)
    do ipts = 1,npts
      xd = spxyz(1,ipts) - xi
      yd = spxyz(2,ipts) - yi
      zd = spxyz(3,ipts) - zi
      call qmatrixelement(xd,yd,zd,0.0_dp,.false.,.false.,qme,dqme,d2qme)
      cosmoBq(ipts) = cosmoBq(ipts) + qi*qme*segweight(ipts)
    enddo
  enddo
  if (index(keyword,'bmat').ne.0.and.ldebug.and.ioproc) then
    write(ioout,'(/,'' COSMO Bq matrix (eV/q): '',/)')
    do n = 1,npts
      write(ioout,'(i6,1x,f8.5)') n,cosmoBq(n)*angstoev
    enddo
  endif
!
  return
  end
