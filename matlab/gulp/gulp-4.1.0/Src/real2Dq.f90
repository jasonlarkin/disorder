  subroutine real2Dq(ereal,erecip,qtot,lgrad1,lgrad2)
!
!  Calculates the real space contribution to the interaction of an ion with it's own 2-D images.
!
!   6/01 Created from reale
!   1/03 Wolf sum modifcations made
!   1/05 rp no longer sqrt'd and passed to rsearch routines
!   3/07 Printing of twobody energies added as an option
!   5/07 Argument list for twobody call modified
!  11/07 Modifications for reaxFF Coulomb term added
!  12/07 Unused variables removed
!   1/08 lreaxFFqreal removed
!   1/09 Integer datatypes all explicitly declared
!   2/09 Expanding and contracting of maxdis arrays removed
!   3/09 lorder12 added to twobody argument list
!   4/12 Explicit virial calculation removed as no longer needed
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
!  Julian Gale, NRI, Curtin University, April 2012
!
  use constants
  use control
  use current
  use derivatives
  use eam,            only : maxmeamcomponent
  use element
  use general,        only : cutw
  use kspace
  use mdlogic
  use molecule
  use optimisation
  use parallel
  use realvectors
  use shell
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  logical,     intent(in)    :: lgrad1
  logical,     intent(in)    :: lgrad2
  real(dp)                   :: ereal
  real(dp)                   :: erecip
  real(dp)                   :: qtot
!
!  Local variables
!
  integer(i4)                :: k
  integer(i4)                :: kk
  integer(i4)                :: kl
  integer(i4)                :: ks
  integer(i4)                :: kt
  integer(i4)                :: nmolonly
  integer(i4)                :: nor
  logical                    :: lmdl
  logical                    :: lself
  logical                    :: lsg1
  logical                    :: lsg2
  real(dp)                   :: cputime
  real(dp)                   :: cut2
  real(dp)                   :: cut2q
  real(dp)                   :: cut2s
  real(dp)                   :: eatom
  real(dp)                   :: ec6
  real(dp)                   :: factor
  real(dp)                   :: fct
  real(dp)                   :: ofct
  real(dp)                   :: rtrm1
  real(dp)                   :: sctrm1(maxmeamcomponent)
  real(dp)                   :: sctrm2(maxmeamcomponent)
  real(dp)                   :: time1
  real(dp)                   :: time2
!
  time1 = cputime()
!
!  Local variables
!
  lmdl = lmd
  if (.not.lgrad1) lmdl = .false.
!
  lsg1 = (lgrad1.and.lstr)
  lsg2 = (lgrad2.and.lstr)
!
!  Set up cutoffs
!
  cut2s = cuts*cuts
  if (lwolf) then        
    cut2q = cutw*cutw
  else
    cut2q = rmx2
  endif
  cut2 = cut2q
!
  if (lnoreal) return
!***************************************************************
!  Atomistic and real space electrostatic component of energy  *
!***************************************************************
  ofct = 0.5_dp
  fct = ofct*angstoev
  factor = qtot*qtot*fct
!***********************
!  Find valid vectors  *
!***********************
  call rsearch2D(0.0_dp,0.0_dp,0.0_dp,.false.,.false.,1_i4,1_i4, &
                 0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cut2)
!
!  Self term
!
  erecip = erecip + factor*tweatpi
!
  if (nor.eq.0) goto 1110
!
!  Sqrt distances
!
  do k = 1,nor
    dist(k) = sqrt(dist(k))
  enddo
!*************************************************
!  Calculate twobody contribution in real space  *
!*************************************************
  call twobody(eatom,ereal,ec6,lgrad1,lgrad2,.false.,nor,1_i4,0_i4,0_i4,0.0_dp,cut2q,cut2s, &
               0_i4,factor,ofct,0.0_dp,rtrm1,sctrm1,sctrm2,qtot,qtot,.false.,.true.,.false., &
               .false.,.false.,.true.)
!
!  Change sign of ereal
!
  ereal = - ereal
!
!  Generate products for derivatives
!
  if (lmdl.or.lsg1.or.lgrad2) then
    do k = 1,nor
      rpd(k,1) = xtmp(k)*xtmp(k)
      rpd(k,2) = ytmp(k)*ytmp(k)
      rpd(k,3) = ztmp(k)*ztmp(k)
      rpd(k,4) = ytmp(k)*ztmp(k)
      rpd(k,5) = xtmp(k)*ztmp(k)
      rpd(k,6) = xtmp(k)*ytmp(k)
    enddo
  endif
!***********************
!  Strain derivatives  *
!***********************
!
!  First derivatives 
!
  if (lsg1.or.lgrad2) then
    do kl = 1,nstrains
      ks = nstrptr(kl)
      do k = 1,nor
        rstrd(kl) = rstrd(kl) - deriv(k)*rpd(k,ks)
      enddo
    enddo
!
!  Second derivatives
!
    if (lsg2) then
      do kk = 1,nstrains
        ks = nstrptr(kk)
        do kl = 1,nstrains
          kt = nstrptr(kl)
          do k = 1,nor
            sderv2(kl,kk) = sderv2(kl,kk) - deriv2(k)*rpd(k,kt)*rpd(k,ks)
          enddo
        enddo
      enddo
    endif
  endif
1110 continue
!
!  End of real space part - perform general tasks
!
!  Timing
!
  time2 = cputime()
  tatom = tatom + time2 - time1
!
  return
  end
