  subroutine oscillatorstrength(mcv,nphonatc,nphonatptr,ncfoc,iocptr,eigr,eigi,maxd2,oscstrength)
!
!  Calculates the oscillator strengths for the modes
!
!   3/02 Created from peigeng
!   7/02 Region 1 only modifications added
!   5/06 Mass now uses species values
!   8/12 Site occupancy introduced to scale oscillator strength
!   8/12 Created by adding imaginary eigenvectors to oscillatorstrengthg
!
!  On entry :
!
!  mcv         = no. of modes
!  nphonatc    = total number of cores in relevant regions
!  nphonatptr  = pointer from reduce to full atom set
!  ncfoc       = number of condensed core sites
!  iocptr      = pointer that connects sites to condensed sites
!  eigr        = eigenvectors of dynamical matrix - real part
!  eigi        = eigenvectors of dynamical matrix - imaginary part
!  maxd2       = left-hand dimension of eigr
!
!  On exit : 
!
!  oscstrength = oscillator strengths for each mode as a 3 x 3 tensor per mode
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
!  Julian Gale, NRI, Curtin University, August 2012
!
  use current
  use element
  use iochannels
  use species,    only : massspec
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)    :: iocptr(*)
  integer(i4), intent(in)    :: maxd2
  integer(i4), intent(in)    :: mcv
  integer(i4), intent(in)    :: nphonatc
  integer(i4), intent(in)    :: nphonatptr(*)
  integer(i4), intent(in)    :: ncfoc
  real(dp),    intent(in)    :: eigr(maxd2,mcv)
  real(dp),    intent(in)    :: eigi(maxd2,mcv)
  real(dp),    intent(out)   :: oscstrength(3,3,mcv)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: ix
  integer(i4) :: iy
  integer(i4) :: iz
  integer(i4) :: j
  integer(i4) :: m
  real(dp)    :: osxi
  real(dp)    :: osyi
  real(dp)    :: oszi
  real(dp)    :: osxr
  real(dp)    :: osyr
  real(dp)    :: oszr
  real(dp)    :: trmj
!**********************************
!  Oscillator strengths per mode  *
!**********************************
!
!  Loop over modes
!
  do m = 1,mcv
    osxr = 0.0_dp
    osyr = 0.0_dp
    oszr = 0.0_dp
    osxi = 0.0_dp
    osyi = 0.0_dp
    oszi = 0.0_dp
!
!  Loop over full sites
!
    do i = 1,ncfoc
      ix = 3*(i-1) + 1
      iy = ix + 1
      iz = ix + 2
!
!  Find all cores associated with full site
!
      do j = 1,nphonatc
        if (iocptr(j).eq.i) then
          trmj = occuf(j)/sqrt(massspec(nspecptr(nrelat(nphonatptr(j)))))
!
!  Multiple inverse mass weighted eigenvectors by Born charges
!
          osxr = osxr + (eigr(ix,m)*bornq(1,1,j) + &
                       eigr(iy,m)*bornq(2,1,j) + &
                       eigr(iz,m)*bornq(3,1,j))*trmj
          osyr = osyr + (eigr(ix,m)*bornq(1,2,j) + &
                       eigr(iy,m)*bornq(2,2,j) + &
                       eigr(iz,m)*bornq(3,2,j))*trmj
          oszr = oszr + (eigr(ix,m)*bornq(1,3,j) + &
                       eigr(iy,m)*bornq(2,3,j) + &
                       eigr(iz,m)*bornq(3,3,j))*trmj
!
          osxi = osxi + (eigi(ix,m)*bornq(1,1,j) + &
                       eigi(iy,m)*bornq(2,1,j) + &
                       eigi(iz,m)*bornq(3,1,j))*trmj
          osyi = osyi + (eigi(ix,m)*bornq(1,2,j) + &
                       eigi(iy,m)*bornq(2,2,j) + &
                       eigi(iz,m)*bornq(3,2,j))*trmj
          oszi = oszi + (eigi(ix,m)*bornq(1,3,j) + &
                       eigi(iy,m)*bornq(2,3,j) + &
                       eigi(iz,m)*bornq(3,3,j))*trmj
        endif
      enddo
    enddo
    oscstrength(1,1,m) = osxr*osxr + osxi*osxi
    oscstrength(2,1,m) = osyr*osxr + osyi*osxi
    oscstrength(3,1,m) = oszr*osxr + oszi*osxi
    oscstrength(1,2,m) = osxr*osyr + osxi*osyi
    oscstrength(2,2,m) = osyr*osyr + osyi*osyi
    oscstrength(3,2,m) = oszr*osyr + oszi*osyi
    oscstrength(1,3,m) = osxr*oszr + osxi*oszi
    oscstrength(2,3,m) = osyr*oszr + osyi*oszi
    oscstrength(3,3,m) = oszr*oszr + oszi*oszi
  enddo
!
  return
  end
