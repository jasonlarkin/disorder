  subroutine settaper
!
!  Setup tasks for tapering of potentials
!
!  11/05 Created
!   2/06 Quadratic and quartic densities added
!   3/06 Modified to allow for density component number
!   8/06 sctrm1/2 set to zero before call to twobody
!   3/07 Glue potential added
!   3/07 Bondtype arguments added to call to twobody1
!   5/07 Argument list for twobody call modified
!  11/07 Mei-Davenport potential added
!  10/08 MEAM modifications added - denpar increased in dimension
!  11/08 x/y/z components passed to twobody1
!  11/08 Baskes form of exponential density added
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody1 argument list
!  10/11 Fractional power density added
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
!  Julian Gale, NRI, Curtin University, October 2011
!
  use eam
  use two
  implicit none
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: j
  integer(i4) :: l
  integer(i4) :: npt
  real(dp)    :: deriv
  real(dp)    :: deriv2
  real(dp)    :: deriv3
  real(dp)    :: derive
  real(dp)    :: derive0
  real(dp)    :: derive2
  real(dp)    :: derive3
  real(dp)    :: d0i
  real(dp)    :: d0j
  real(dp)    :: d1i
  real(dp)    :: d1j
  real(dp)    :: d2i2
  real(dp)    :: d2ij
  real(dp)    :: d2j2
  real(dp)    :: dist
  real(dp)    :: dr
  real(dp)    :: dr2
  real(dp)    :: dr3
  real(dp)    :: eatom
  real(dp)    :: ec6
  real(dp)    :: ereal
  real(dp)    :: etrm
  real(dp)    :: rderiv
  real(dp)    :: rpt
  real(dp)    :: rr0
  real(dp)    :: rr12
  real(dp)    :: rtrm1
  real(dp)    :: rtrm2
  real(dp)    :: rtrm3
  real(dp)    :: rtrm32
  real(dp)    :: savetapermax
  real(dp)    :: sctrm1
  real(dp)    :: sctrm2
  real(dp)    :: trm
!
!  If upper bound of taper is zero return
!
  if (tapermax.eq.0.0_dp) return
!
!  Temporarily increase the potential cutoff so that we can calculate the value 
!
  cutp = cutp + 1.0_dp
!
!  Temporarily turn off the tapering to get the untapered values
!
  savetapermax = tapermax
  tapermax = 0.0_dp
!
!  Set distance at which to calculate the energy/gradient
!
  dist = savetapermax
  if (tapertype.eq.3) then
!
!  Loop over potentials to compute the energy and gradient values at the cutoff for the Voter taper fn
!
    do i = 1,npote
      eatom = 0.0_dp
      ereal = 0.0_dp
      ec6 = 0.0_dp
      sctrm1 = 0.0_dp
      sctrm2 = 0.0_dp
      call twobody1(eatom,ereal,ec6,.true.,.false.,.false.,1_i4,1_i4,dist,0.0_dp,0.0_dp,0.0_dp,d0i,d0j, &
                    deriv,deriv2,deriv3,derive0,derive,derive2,derive3,rderiv,1_i4,i,cutp*cutp,1.0d12,0.01d0,.false., &
                    0_i4,0.0_dp,1.0_dp,0.0_dp,rtrm1,rtrm2,rtrm3,rtrm32,sctrm1,sctrm2,0.0_dp,0.0_dp,.false., &
                    .false.,.false.,.false.,.false.,.false.,0_i4,0_i4,.false.,.false.,.true.,d1i,d1j,d2i2,d2ij,d2j2)
      taperpot(i) = eatom
      tapergrad(i) = deriv*savetapermax
    enddo
  endif
!
!  Reset cutp & tapermax
!
  cutp = cutp - 1.0_dp
  tapermax = savetapermax
!
!  Calculate taper correction for density in EAM
!
  if (neamspec.gt.0) then
    do i = 1,neamspec
      eamtaperrho(i) = 0.0_dp
      eamtaperdrho(i) = 0.0_dp
      do j = 1,ndenfncomp(i)
        npt = nint(denpar(6,1,j,i))
        if (ndenfn(j,i).eq.1) then
!
!  Power law
!
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)/(tapermax**npt)
          sctrm2 = - dble(npt)*sctrm1/tapermax
        elseif (ndenfn(j,i).eq.2) then
!
!  Exponential
!
          trm = denpar(2,1,j,i)*(tapermax - denpar(3,1,j,i))
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)*(tapermax**npt)*exp(-trm)
          sctrm2 = sctrm1*((dble(npt)/tapermax) - denpar(2,1,j,i))
        elseif (ndenfn(j,i).eq.3) then
!
!  Gaussian
!
          trm = denpar(2,1,j,i)*((tapermax - denpar(3,1,j,i))**2)
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)*(tapermax**npt)*exp(-trm)
          sctrm2 = sctrm1*((dble(npt)/tapermax) - 2.0_dp*denpar(2,1,j,i)*(tapermax - denpar(3,1,j,i)))
        elseif (ndenfn(j,i).eq.4) then
!
!  Cubic
!
          trm = (tapermax - denpar(2,1,j,i))
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)*trm**3
          sctrm2 = 3.0_dp*eamalloy(1,i)*denpar(1,1,j,i)*trm**2
        elseif (ndenfn(j,i).eq.5) then
!
!  Voter-Chen
!
          trm = 2.0_dp**9
          etrm = exp(-denpar(2,1,j,i)*tapermax)
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)*(tapermax**6)*etrm*(1.0_dp + trm*etrm)
          sctrm2 = eamalloy(1,i)*denpar(1,1,j,i)*etrm*(tapermax**5)* &
            (6.0_dp*(1.0_dp + trm*etrm) - denpar(2,1,j,i)*tapermax*(1.0_dp + 2.0_dp*trm*etrm))
        elseif (ndenfn(j,i).eq.6) then
!
!  Quadratic
!
          trm = (tapermax - denpar(2,1,j,i))
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)*trm**2
          sctrm2 = 2.0_dp*eamalloy(1,i)*denpar(1,1,j,i)*trm
        elseif (ndenfn(j,i).eq.7) then
!
!  Quartic
!
          trm = (tapermax - denpar(2,1,j,i))
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)*trm**4
          sctrm2 = 4.0_dp*eamalloy(1,i)*denpar(1,1,j,i)*trm**3
        elseif (ndenfn(j,i).eq.8) then
!
!  Glue
!
          if (tapermax.lt.denpar(1,1,j,i)) then
            dr = (tapermax - denpar(1,1,j,i))
            dr2 = dr*dr
            dr3 = dr2*dr
            sctrm1 = eamalloy(1,i)*(denpar(4,1,j,i)*dr3 + denpar(5,1,j,i)*dr2 + denpar(6,1,j,i)*dr + denpar(7,1,j,i))
            sctrm2 = eamalloy(1,i)*(3.0_dp*denpar(4,1,j,i)*dr2 + 2.0_dp*denpar(5,1,j,i)*dr + denpar(6,1,j,i))
          elseif (tapermax.lt.denpar(2,1,j,i)) then
            dr = (tapermax - denpar(1,1,j,i))
            dr2 = dr*dr
            dr3 = dr2*dr
            sctrm1 = eamalloy(1,i)*(denpar(8,1,j,i)*dr3 + denpar(9,1,j,i)*dr2 + denpar(10,1,j,i)*dr + denpar(11,1,j,i))
            sctrm2 = eamalloy(1,i)*(3.0_dp*denpar(8,1,j,i)*dr2 + 2.0_dp*denpar(9,1,j,i)*dr + denpar(10,1,j,i))
          elseif (tapermax.lt.denpar(3,1,j,i)) then
            dr = (tapermax - denpar(3,1,j,i))
            dr2 = dr*dr
            dr3 = dr2*dr
            sctrm1 = eamalloy(1,i)*(denpar(12,1,j,i)*dr3 + denpar(13,1,j,i)*dr2 + denpar(14,1,j,i)*dr + denpar(15,1,j,i))
            sctrm2 = eamalloy(1,i)*(3.0_dp*denpar(12,1,j,i)*dr2 + 2.0_dp*denpar(13,1,j,i)*dr + denpar(14,1,j,i))
          else
            sctrm1 = 0.0_dp
            sctrm2 = 0.0_dp
          endif
        elseif (ndenfn(j,i).eq.10) then
!                           
!  Mei-Davenport          
!                           
          rr0 = 1.0_dp/denpar(7,1,j,i)
          rr12 = 1.0_dp/12.0_dp 
          trm = 1.0_dp
          sctrm1 = 0.0_dp
          sctrm2 = 0.0_dp
          do l = 1,6
            sctrm1 = sctrm1 + eamalloy(1,i)*denpar(l,1,j,i)*rr12*trm
            sctrm2 = sctrm2 + eamalloy(1,i)*denpar(l,1,j,i)*rr12*trm*dble(l-1)/tapermax
            trm = trm*tapermax*rr0
          enddo
        elseif (ndenfn(j,i).eq.12) then
!
!  Baskes
!
          trm = denpar(2,1,j,i)*((tapermax/denpar(3,1,j,i))-1.0_dp)
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)*exp(-trm)
          sctrm2 = - sctrm1*denpar(2,1,j,i)/denpar(3,1,j,i)
        elseif (ndenfn(j,i).eq.14) then
!
!  Fractional power law
!
          rpt = denpar(2,1,j,i)
          sctrm1 = eamalloy(1,i)*denpar(1,1,j,i)/(tapermax**rpt)
          sctrm2 = - rpt*sctrm1/tapermax
        else
          sctrm1 = 0.0_dp
          sctrm2 = 0.0_dp
        endif
        eamtaperrho(i) = eamtaperrho(i) + sctrm1
        eamtaperdrho(i) = eamtaperdrho(i) + sctrm2
      enddo
    enddo
  endif
!
  return
  end
