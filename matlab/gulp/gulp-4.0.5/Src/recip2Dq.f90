  subroutine recip2Dq(erecip,qtot,lgrad1,lgrad2)
!
!  Calculates the energy of interaction of an ion with a
!  periodic array in 2-D of itself. Assumes that it is 
!  being called after recip2D so that setup phase is
!  unnecessary.
!
!   6/01 Created from recip2D
!  11/02 Parallel modifications made
!   6/07 Bug in parallel execution corrected
!  12/07 Unused variables removed
!   6/12 Handling of esum corrected 
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
!  Julian Gale, NRI, Curtin University, June 2012
!
  use constants
  use control
  use current
  use derivatives
  use kspace
  use parallel
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  logical,    intent(in)                        :: lgrad1
  logical,    intent(in)                        :: lgrad2
  real(dp),   intent(inout)                     :: erecip
  real(dp),   intent(in)                        :: qtot
!
!  Local variables
!
  integer(i4)                                   :: i
  integer(i4)                                   :: iv
  integer(i4)                                   :: kk
  integer(i4)                                   :: ll
  integer(i4)                                   :: nlocalkvec
  integer(i4)                                   :: nremainder
  integer(i4)                                   :: status
  logical                                       :: lsg1
  real(dp)                                      :: cosq
  real(dp)                                      :: cputime
  real(dp)                                      :: csinq
  real(dp)                                      :: d21q
  real(dp)                                      :: d22q
  real(dp)                                      :: d23q
  real(dp)                                      :: d26q
  real(dp)                                      :: darg1
  real(dp)                                      :: derfc
  real(dp)                                      :: derfc1
  real(dp)                                      :: dexp1
  real(dp)                                      :: eltrm
  real(dp)                                      :: eltrm1
  real(dp)                                      :: eltrm2
  real(dp)                                      :: erecipl
  real(dp)                                      :: esum
  real(dp)                                      :: gseta
  real(dp)                                      :: kexperfc
  real(dp)                                      :: kvec
  real(dp)                                      :: qfct
  real(dp)                                      :: rkvec
  real(dp)                                      :: strdrvl(6)
  real(dp)                                      :: strm1
  real(dp),   dimension(:),   allocatable       :: sum
  real(dp)                                      :: time0
  real(dp)                                      :: time1
  real(dp),   dimension(:,:), allocatable       :: tmp
  real(dp)                                      :: tsum0
  real(dp)                                      :: twoqv
!
  if (lnorecip) goto 999
!
  time0 = cputime()
  erecipl = 0.0_dp
  lsg1 = (lstr.and.lgrad1)
!
!  Distribute kvec loops
!
  if (lgrad2) then
    nlocalkvec = nkvec
  else
    nlocalkvec = (nkvec/nprocs)
    nremainder = nkvec - nlocalkvec*nprocs
    if (procid.lt.nremainder) nlocalkvec = nlocalkvec + 1
  endif
!
!  Allocate local memory
!
  allocate(tmp(nlocalkvec,3),stat=status)
  if (status/=0) call outofmemory('recip2Dq','tmp')
  allocate(sum(max(1_i4,nstrains)),stat=status)
  if (status/=0) call outofmemory('recip2Dq','sum')
!
!  Define constants
!
  rpieta = 1.0_dp / sqrt(pi * eta)
  rhseta = 0.5_dp / seta
!
!  Build products of K vector components
!
  if (lsg1.or.lgrad2) then
    do iv = 1,nlocalkvec
      tmp(iv,1) = xrk(iv)*xrk(iv)
      tmp(iv,2) = yrk(iv)*yrk(iv)
      tmp(iv,3) = xrk(iv)*yrk(iv)
    enddo
  endif
  qfct = 0.5_dp*qtot*qtot*angstoev
!
!  First term - K vector independent
!
  twoqv = qfct*vol4pi
  if (procid.eq.0) then
    erecipl = erecipl + twoqv*rpieta
  endif
!
!  Second term - K vector dependent
!
  csinq = 0.0_dp
  if (lgrad1) then
    if (lsg1) then
      strdrvl(1:6) = 0.0_dp
    endif
    if (lgrad2) then
      d21q = 0.0_dp
      d22q = 0.0_dp
      d23q = 0.0_dp
      d26q = 0.0_dp
    endif
    do iv = 1,nlocalkvec
      cosq = qfct*ktrm(iv)
      kvec = kmod(iv)
      darg1 = kvec*rhseta
      dexp1 = exp(-(darg1)**2)
      derfc1 = derfc(darg1)
      kexperfc = 2.0_dp*derfc1
!
!  Energy
!
      csinq = csinq + cosq*kexperfc
!
!  First derivatives with respect to atoms
!
      if (lgrad2) then
!
!  Second derivatives with respect to atoms
!
        d21q = d21q - cosq*tmp(iv,1)*kexperfc
        d22q = d22q - cosq*tmp(iv,2)*kexperfc
        d23q = d23q + cosq*(kvec*kvec*kexperfc - 4.0_dp*tweatpi*(kvec*dexp1 - seta*darg1*dexp1))
        d26q = d26q - cosq*tmp(iv,3)*kexperfc
      endif
      if (lsg1) then
!
!  Strain first derivatives
!
        rkvec = 1.0_dp/kvec
        strm1 = rkvec*(-rkvec*kexperfc - 2.0*rpieta*dexp1)
        do kk = 1,nstrains
          strdrvl(kk) = strdrvl(kk) + strm1*cosq*tmp(iv,kk)
        enddo
        if (lgrad2) then
          eltrm1 = - 2.0_dp*strm1*cosq*rkvec*rkvec
          eltrm2 = cosq*rkvec*rkvec*(2.0*derfc1*(rkvec*rkvec) + &
            rpieta*(2.0*dexp1*(rkvec+0.5_dp*kvec/eta)))
          eltrm = (eltrm1 + eltrm2)
          do kk = 1,nstrains
            do ll = 1,kk
              sderv2(kk,ll) = sderv2(kk,ll) - eltrm*tmp(iv,kk)*tmp(iv,ll)
            enddo
          enddo
        endif
      endif
    enddo
  else
    do iv = 1,nlocalkvec
      kvec = kmod(iv)
      gseta = kvec*rhseta
      kexperfc = 2.0_dp*derfc(gseta)
      csinq = csinq + ktrm(iv)*kexperfc
    enddo
    csinq = csinq*qfct
  endif
!
!  Lattice energy
!
  erecipl = erecipl - csinq
!****************
!  Global sums  *
!****************
  if (.not.lgrad2) then
    tsum0 = cputime()
    call sumall(erecipl,sum,1_i4,"recip","erecip")
    esum = sum(1)
    if (lsg1) then
      call sumall(strdrvl,sum,nstrains,"recip","strderv")
      do i = 1,nstrains
        strdrvl(i) = sum(i)
        strderv(i) = strderv(i) + sum(i)
      enddo
    endif
    tsum = tsum + cputime() - tsum0
  else
    esum = erecipl
  endif
  erecip = erecip + esum
!****************************************************
!  Complete strain derivatives in reciprocal space  *
!****************************************************
  if (lsg1) then
    if (lgrad2) then
      sderv2(1,1) = sderv2(1,1) + esum - 4.0_dp*strdrvl(1)
      sderv2(2,2) = sderv2(2,2) + esum - 4.0_dp*strdrvl(2)
!
      sderv2(3,3) = sderv2(3,3) + 0.5_dp*esum - 0.75_dp*(strdrvl(1)+strdrvl(2))
!
      sderv2(2,1) = sderv2(2,1) + esum - strdrvl(1) - strdrvl(2)
!
      sderv2(3,1) = sderv2(3,1) - 2.5_dp*strdrvl(3)
      sderv2(3,2) = sderv2(3,2) - 2.5_dp*strdrvl(3)
    endif
    strderv(1) = strderv(1) - esum
    strderv(2) = strderv(2) - esum
  endif
!***************
!  Exit point  *
!***************
999 continue
!
!  Free local memory
!
  deallocate(sum,stat=status)
  if (status/=0) call deallocate_error('recip2Dq','sum')
  deallocate(tmp,stat=status)
  if (status/=0) call deallocate_error('recip2Dq','tmp')
!
!  Timing
!
  time1 = cputime()
  tion = tion + time1 - time0
!
  return
  end
