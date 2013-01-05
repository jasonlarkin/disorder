  subroutine outcosmo(iout)
!
!  Write out Accelrys COSMO file (for input to Cerius)
!
!   4/10 Created
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
!  Julian Gale, NRI, Curtin University, April 2010
!
  use configurations
  use cosmo
  use current
  use element
  use files
  use iochannels
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: iout
!
!  Local variables
!
  character(len=80)  :: cosmofilel
  character(len=5)   :: lab
  integer(i4)        :: i
  integer(i4)        :: ii
  integer(i4)        :: inat
  integer(i4)        :: incf
  integer(i4)        :: ind
  integer(i4)        :: itype
  integer(i4)        :: j
  integer(i4)        :: jj
  integer(i4)        :: ndummy
!***************************
!  Initialisation of file  *
!***************************
  cosmofilel = cosmofile
!
!  If cosmo file name has been given then open file
!
  if (cosmofilel(1:1).eq.' ') then
    cosmofilel = 'gulp.cosmo'
  endif
  ind = index(cosmofilel,'.cosmo')
  if (ind.eq.0) then
    ind = index(cosmofilel,' ')
    cosmofilel(ind:ind+5) = '.cosmo'
  endif
  open(iout,file = cosmofilel,status='unknown')
  write(ioout,'(''  COSMO file written as '',a30)') cosmofilel(1:30)
!*************************
!  Output COSMO details  *
!*************************
  write(iout,''COSMO Results from GULP'',/)')
!
  ndummy = 1
!
!  General output
!
  write(iout,'(''Note: COSMO for ideal conductor limit (fepsi= 1.0000)'')')
  write(iout,'(''  Number of Segments on an Atom    = '',i6)') nspa
  write(iout,'(''  Number of Segments on Hydrogen   = '',i6)') nspah
  write(iout,'(''  Solvent Radius    [au]           = '',f6.2)') cosmorsolv(ncf)/angtoau
  write(iout,'(''  IBS-SAS alf                      = '',f6.2)') cosmormax
  write(iout,'(''  maximum segment or cutoff radius = '',f6.2)') cosmormaxs
  write(iout,'(''  total number of segments         = '',i6)') npts
  write(iout,'(''  number of charge corr segments   = '',i6)') ndummy
!
!  Atom specific information
!
  write(iout,'(''$coordinates xyz [au]                         and cosmo atom analysis:   radius   '', &
               ''charge    area  charge density  (srad)'')')
  write(iout,'(''$end'')')
!
!  Overall energy and results
!
  write(iout,'(''$cosmo_energy               [Hartree atomic units]       [eV]     [kcal/mol]'')')
  write(iout,'(''$screening_charge'')')
!
!  Segment information
!
  write(iout,'(''$segment information'')')
!
  return
  end
