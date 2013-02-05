  subroutine solvation(ecosmo,lgrad1,lgrad2)
!
!  Subroutine for calculating the solvation energy
!  according to the COSMO model.
!
!  11/04 Use of sasparticle data structures added
!  12/04 Matrix cosmoBq introduced in energy calc
!  12/04 Computation of charges on SAS moved to new routine
!   1/05 Setting of deltaq removed since this is now done
!        in setqonsas
!   1/05 New COSMIC energy implemented in which deltaq is
!        multiplied by segment weight
!  12/08 Migrated to version 3.5 and converted to f90 format
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
!  Julian Gale, NRI, Curtin University, December 2008
!
  use constants
  use cosmo
  use current
  use times,   only : tcosmo, tcosmoderv
  implicit none
!
!  Passed arguments
!
  real(dp),    intent(out) :: ecosmo
  logical,     intent(in)  :: lgrad1
  logical,     intent(in)  :: lgrad2
!
!  Local variables
!
  integer(i4)              :: ipts
  real(dp)                 :: fact
  real(dp)                 :: t1
  real(dp)                 :: t2
  real(dp)                 :: t3
!
!  Functions
!
  real(dp)                 :: cputime
!
!  Initialise timer for this routine
!     
  t1 = cputime()                                                                              
!
!  Set conversion factor x dielectric factor
!
  fact = 0.5_dp*autoev*autoangs*cosmofneps
!
!  Check that dimensions for two-dimensional arrays are up to date
!
  if (maxnpts.gt.maxnpts2) then
    maxnpts2 = maxnpts
    call changemaxnpts2
  endif
!
!  Initialise Coulomb matrix element terms
!
  call initqmatrix
  call initqmatrixc
  call initktrm
!
!  Generate segment weighting factors
!
  call setsegweight
!
!  Generate SAS - SAS matrix - cosmoA
!
  call setcosmoamat
!
!  Generate SAS - atom matrix - cosmoB
!
  call setcosmobmat
!
!  Calculate charges on SAS
!
  call setqonsas(lgrad2)
!
!  Calculate energy
!
  ecosmo = 0.0_dp
  do ipts = 1,npts
    ecosmo = ecosmo + (qonsas(ipts) - deltaq*segweight(ipts))*cosmoBq(ipts)
  enddo
  ecosmo = fact*ecosmo
!
  t2 = cputime()                                                                              
!
!  Calculate gradients
!
  if (lgrad1.or.lgrad2) call cosmoderv(lgrad2)
!
!  Finalise timer for this routine
!     
  t3 = cputime()                                                                              
  tcosmo = tcosmo + t2 - t1
  tcosmoderv = tcosmoderv + t3 - t2
!
  return
  end
